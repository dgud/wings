%%
%%  wpc_wmf_paths.erl --
%%
%%     EMF and WMF path importer.
%%
%%  Copyright (c) 2022-2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%%  This plugin uses some of the wpc_ai functions by Howard Trickey
%%  and contributors

-module(wpc_wmf_paths).

-export([init/0,menu/2,command/2]).


-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-ifdef(DEBUG_1).
-define(DEBUG_FMT(A,B), io:format(A,B)).
-else.
-define(DEBUG_FMT(A,B), none).
-endif.

-define(SINT, little-signed-integer).
-define(UINT, little-unsigned-integer).
-define(FLT,  little-float).

init() ->
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{?__(1,"WMF/EMF Paths (.emf|.wmf)..."),wmf_paths,[option]}].


command({file,{import,{wmf_paths,Ask}}}, St) ->
    do_import(Ask, St);
command(_, _) ->
    next.

%%%
%%% Import.
%%%

more_info() ->
    [?__(1,"<b>Automatic center</b>\n"
    "Automatically center the imported shape.\n\n"
    "<b>Scale fraction</b>\n"
    "Set the scale ratio of 1 unit in wings to the units in the metafile. "
    "If set to 100pt, then 100pt is the same as 1 unit in wings. Available "
    "units include pt, pc, cm, mm, and in.\n\n"
    "<b>Scale fit within view</b>\n"
    "Rescale the shape to fit in view. "
    "This means the scaling based on document units is ignored.\n\n"
    "<b>Transforms in text file</b>\n"
    "When checked, a text file with the same file name as the .wmf,.emf file "
    "(but with a .txt file extension) in the same directory can contain "
    "transform information such as z=2 to move the shape in the z axis. Each "
    "line of the text file can contain a line of the form "
    "&lt;shape number&gt;:&lt;transforms&gt;, where shape number starts from "
    "1 and refers to the nth shape in the .wmf/.emf file. The specified "
    "transforms will be set to only that shape.\n\n"
    "Enhanced metafiles (.emf) can contain clipped images which will be used "
    "by the importer to make textures. Original windows metafiles (.wmf) can "
    "only have color fills on the shapes.\n\n")].

info_button() ->
    Title = ?__(1,"EMF/WMF Import Information"),
    TextFun = fun () -> more_info() end,
    {help,Title,TextFun}.

do_import(Ask, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(?MODULE, wmf_bisections, 3),
    AutoScale = wpa:pref_get(?MODULE, wmf_auto_scale, false),
    TransformsInTextFile = wpa:pref_get(?MODULE, wmf_transforms_in_txt_file, false),
    %% Force SetScale to be text
    case wpa:pref_get(?MODULE, wmf_set_scale, "100pt") of
        Number when is_float(Number); is_integer(Number) ->
            SetScale = lists:flatten(io_lib:format("~p", [Number]));
        Str when is_list(Str) ->
            SetScale = Str
    end,
    Hook_Auto_Scale = fun(_Key, Value, Store) ->
        wings_dialog:enable(set_scale, Value =:= false, Store)
    end,
    Dialog =
      [{hframe,[{label,?__(1,"Number of edge bisections")},
                {text,DefBisect,[{key,bisections}]}]},
       {hframe,[{label,?__(4,"Scale fraction") ++ ": 1 / "},
                {text,SetScale,[{key,set_scale}]}]},
       {?__(5,"Automatic center"),true,[{key,auto_center}]},
       {?__(3,"Scale fit within view"),AutoScale,
           [{key,auto_scale_fit}, {hook, Hook_Auto_Scale},
            {info, ?__(9,"Automatically rescale shapes to fit within the camera view.")}]},
       {?__(6,"Transforms in text file"),TransformsInTextFile,[
           {info, ?__(8,"Transforms in text file with same file name but .txt extension")},
           {key,transforms_in_txt_file}]},
       panel,
       {hframe,[info_button()]},
       {hframe,[{label,?__(7,"To import shapes with clipped images,\n"
                             "use enhanced metafiles (.emf).")}]}
       ],
    wpa:dialog(Ask, ?__(2,"EMF/WMF Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{wmf_paths,Res}}}
           end);
do_import(List, St) when is_list(List) ->
    Nsub_0 = proplists:get_value(bisections, List, 0),
    AutoScale = proplists:get_value(auto_scale_fit, List, false),
    SetScale_S = proplists:get_value(set_scale, List, "100pt"),
    AutoCenter = proplists:get_value(auto_center, List, true),
    TransformsInTextFile = proplists:get_value(transforms_in_txt_file, List, false),
    case Nsub_0 < 1 of
        true -> Nsub = 1;
        false -> Nsub = Nsub_0
    end,
    wpa:import(props(), import_fun(Nsub, AutoScale, SetScale_S, AutoCenter, TransformsInTextFile), St).
props() ->
    [{extensions,
      [{".emf", "Enhanced Metafile"},
       {".wmf",  "Windows Metafile"}]}].

%% The following record is used to communicate with wpc_ai's functions
%%
-record(cedge, % polyarea and cedge records must match definitions in wpc_ai.erl
        {vs,cp1=nil,cp2=nil,ve}).    %all are {x,y} pairs

-type color() :: {float(),float(),float()}.

-type texcoord() :: {
        {float(),float()},
        {float(),float()},
        {float(),float()},
        {float(),float()}}.

-type texinfo_image() :: {integer(), integer(), binary()}.

-type texinfo() :: {texcoord(), texinfo_image()}.

%%
%%
-record(rpaths, {
    path_list = [] :: [[#cedge{}]],
    color_list = [] :: [color()],
    mat_list = [] :: [texinfo()],
    current_color = {0.9,0.9,0.9} :: color(),
    current_tex = none :: texinfo() | none,
    world_transform = {1.0, 0.0, 0.0, 1.0} :: {float(),float(),float(),float()}
}).


import_fun(Nsub, AutoScale, SetScale_S, AutoCenter, TransformsInTextFile) ->
    fun(Filename) ->
        case parse_float_number_w_unit(SetScale_S, 0.0) of
            {ScaleVal, ScaleUnit} when ScaleVal > 0.001 ->
                wpa:pref_set(?MODULE, wmf_set_scale, SetScale_S),
                SetScale = {ScaleVal, unit_atom(ScaleUnit)};
            _Unk ->
                SetScale = {100.0, pt}
        end,
        try try_import(Filename, Nsub, AutoScale, SetScale, AutoCenter, TransformsInTextFile) of
            {ok, E3DFile} ->
                wpa:pref_set(?MODULE, wmf_bisections, Nsub),
                wpa:pref_set(?MODULE, wmf_auto_scale, AutoScale),
                wpa:pref_set(?MODULE, wmf_transforms_in_txt_file, TransformsInTextFile),
                {ok, E3DFile};
            {error, Reason} ->
                {error, ?__(1,"wmf/emf path import failed")++": " ++ Reason}
        catch EClass:E:ST ->
            io:format("File Import Error Report:\n ~p: ~p\nstack trace: ~p~n",
                [EClass, E, ST]),
            {error, ?__(2,"wmf/emf path import internal error")}
        end
    end.
    
try_import(Filename, Nsub, AutoScale, SetScale, AutoCenter, TransformsInTextFile) ->
    ShortFilename = filename:rootname(filename:basename(Filename)),
    
    FType = get_file_type(Filename), %% emf or wmf
    {ok, CommandsList_0, DPI} = read_file(FType, Filename, Nsub),
    {ok, TIn} = read_txt_transforms_if_any(Filename, TransformsInTextFile),
    {CommandsList_1, TexturesList} = get_textures_list(ShortFilename, CommandsList_0),
    CommandsList_2 = remove_double_path(exclusion(into_paths(FType, CommandsList_1))),
    CommandsList_3 = remove_invisible_paths(FType, CommandsList_2),
    {ViewPortScaleX,ViewPortScaleY} = viewport_to_window(FType, CommandsList_3),
    
    CommandsList_4 = remove_bad_paths(CommandsList_3),
    
    case AutoScale of
        true ->
            [CamDist] = wpa:camera_info([distance_to_aim]),
            Rescale_0 = calculate_rescale_amount(CamDist, CamDist, CommandsList_4),
            RescaleX = Rescale_0,
            RescaleY = Rescale_0;
        _ ->
            Rescale_Denom = conv_unit(SetScale, px, DPI),
            RescaleX = ViewPortScaleX / Rescale_Denom,
            RescaleY = ViewPortScaleY / Rescale_Denom
    end,
    {ok, CEdges, Colors, MatList} = cedge_realize(FType, rescale({RescaleX,RescaleY}, CommandsList_4)),
    
    ShapeTransforms = assign_transforms_to_shapes(CEdges, TIn),
    MatTrnList = lists:zip(MatList, ShapeTransforms),
    
    case CEdges of
        [] ->
            {error, "No paths found"};
            
        Cntrs when length(Cntrs) > 0 ->
            
            %% NOTE: Coordinate values in #cedge.vs and #cedge.ve have to
            %%       be of type floating point or the polyareas_to_faces
            %%       function doesn't work (change integers to floats).
            %%
            
            %% This is borrowed from the other path plugins
            %% (wpc_ps, wpc_svg) to maintain a similar behaviour
            %% for the polygon subdivision.
            %%
            Pas = [wpc_ai:findpolyareas(Cntr) || Cntr <- Cntrs],
            MatTrnList_1 = repeat_matlist_if_needed(MatTrnList, Pas),
            Pas1 = lists:append(Pas),
            List = process_islands(Pas1),
            {Vs0,Efs,Tx,HEs} = into_mesh_parts(List, MatTrnList_1),
            
            case AutoCenter of
                true ->
                    Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
                    Vec = e3d_vec:sub(e3d_vec:zero(),Center),
                    Vs = [ e3d_vec:add(V,Vec) || V <- Vs0];
                _ ->
                    Vs = Vs0
            end,
            
            Mesh = #e3d_mesh{
                type=polygon,
                vs=Vs,
                fs=Efs,
                he=HEs,
                vc=[C || C <- Colors],
                tx=Tx},
            Obj = #e3d_object{name=ShortFilename,obj=Mesh},
            
            {ok, #e3d_file{objs=[Obj],mat=TexturesList}}
    end.

get_textures_list(Filename, Cmds) when is_binary(Filename) ->
    {{OCmds, OMat}, _} = get_textures_list(binary_to_list(Filename), Cmds, [], [], []),
    {lists:reverse(OCmds), lists:reverse(OMat)};
get_textures_list(Filename, Cmds) when is_list(Filename) ->
    {{OCmds, OMat}, _} = get_textures_list(Filename, Cmds, [], [], []),
    {lists:reverse(OCmds), lists:reverse(OMat)}.
get_textures_list(_Filename, [], [], OCmds, OMat) ->
    {{OCmds, OMat}, []};
get_textures_list(Filename, [], OCmdsAfter, OCmds, OMat)
  when length(OCmdsAfter) > 0 ->
    get_textures_list(Filename, [], [], OCmdsAfter ++ OCmds, OMat);
get_textures_list(Filename, [A|Cmds], OCmdsAfter, OCmds, OMat) ->
    case A of
        {tex_bitmap, {Where, {BitmapWidth, BitmapHeight, Blob}}} ->
            Id = "_" ++ integer_to_list(abs(erlang:unique_integer())),
            MatName = list_to_atom("material_" ++ Filename ++ Id),
            TexName = "texture_" ++ Filename ++ Id ++ ".bmp",
            Maps = {maps, [{diffuse, #e3d_image{
                type = b8g8r8a8,
                bytes_pp = 4,
                alignment = 1,
                order = lower_left,
                width = BitmapWidth,
                height = BitmapHeight,
                image = Blob,
                filename=none,
                name=TexName,
                extra=[]
            }}]},
            OpenGL = {opengl,
                     [{ambient,{0.0,0.0,0.0,0.0}},
                      {specular, {0.2,0.2, 0.2,1.0}},
                      {shininess,0.2},
                      {diffuse, {0.8,0.8,0.7,1.0}},
                      {emission,{0.0,0.0,0.0,1.0}},
                      {metallic,0.1},
                      {roughness,0.8},
                      {vertex_colors, set}]},
            M = {MatName, [Maps, OpenGL]},
            A_1 = {tex_bitmap, {Where, MatName}},
            get_textures_list(Filename, Cmds, OCmdsAfter, [A_1 | OCmds], [M | OMat]);
            
        {save_dc, _} ->
            {{OCmds_1, OMat_1}, Cmds_1} = get_textures_list(Filename, Cmds, [], [], []),
            
            get_textures_list(Filename, Cmds_1, OCmdsAfter, OCmds_1 ++ [A | OCmds], OMat_1 ++ OMat);
        {restore_dc, _} ->
            {{[A|OCmdsAfter ++ OCmds], OMat}, Cmds};
        {selectclippath, _} ->
            get_textures_list(Filename, Cmds, OCmds ++ OCmdsAfter, [], OMat);
        
        _Other ->
            get_textures_list(Filename, Cmds, OCmdsAfter, [A|OCmds], OMat)
    end.

%% Remove paths that are impossible to use:
%% 2 coordinate paths, and 3 coordinate paths where
%% the first and last coordinate are the same.
%%
remove_bad_paths(Cmds) ->
    remove_bad_paths(Cmds, []).
remove_bad_paths([], OCmds) -> lists:reverse(OCmds);
remove_bad_paths([A|Cmds], OCmds) ->
    case A of
        {path, [List|_]} when length(List) < 3 ->
            remove_bad_paths(Cmds, OCmds);
        {path, [[C1|_]=List|_]} when length(List) < 4 ->
            case lists:reverse(List) of
                [C2|_] when C1 =:= C2 -> remove_bad_paths(Cmds, OCmds);
                _ -> remove_bad_paths(Cmds, [A|OCmds])
            end;
        {path, [List|AList]} ->
            AList_1 = remove_bad_paths_1(AList),
            remove_bad_paths(Cmds, [{path, [List|AList_1]} | OCmds]);
        _Other ->
            remove_bad_paths(Cmds, [A|OCmds])
    end.
remove_bad_paths_1(SubPaths) ->
    remove_bad_paths_1(SubPaths, []).
remove_bad_paths_1([], O) ->
    lists:reverse(O);
remove_bad_paths_1([List|R], O) when length(List) < 3 ->
    remove_bad_paths_1(R, O);
remove_bad_paths_1([[C1|_]=List|R], O) when length(List) < 4 ->
    case lists:reverse(List) of
        [C2|_] when C1 =:= C2 -> remove_bad_paths_1(R, O);
        _ -> remove_bad_paths_1(R, [List|O])
    end;
remove_bad_paths_1([List|R], O) ->
    remove_bad_paths_1(R, [List|O]).

%% Remove rectangles that are at the very beginning of the file before
%% there are color assignments and context commands. Affinity Designer
%% exports enhanced metafiles with an initial path before any color
%% information.
%%
remove_invisible_paths(emf, [{path,[
    [{X0,Y0},{X1,Y0},{X1,Y1},{X0,Y1},{X0,Y0}]]} | CommandsList_R]) ->
    remove_invisible_paths(emf, CommandsList_R);
remove_invisible_paths(emf, [{CmdAtom,_}=Cmd | CommandsList_R])
  when CmdAtom =:= viewport; CmdAtom =:= window ->
    [Cmd | remove_invisible_paths(emf, CommandsList_R)];
remove_invisible_paths(_, CommandsList) ->
    CommandsList.
    

-record(win_viewp, {
    w_origin = {0.0, 0.0} :: {float(),float()},
    w_extent = {1.0, 1.0} :: {float(),float()},
    v_origin = {0.0, 0.0} :: {float(),float()},
    v_extent = none       :: {float(),float()} | none
}).
    
    
viewport_to_window(wmf, CommandList) ->
    viewport_to_window_1(CommandList, #win_viewp{});
viewport_to_window(emf, CommandList) ->
    viewport_to_window_1(CommandList, #win_viewp{}).
viewport_to_window_1([], #win_viewp{
    w_origin={WX1,WY1},
    w_extent={WX2,WY2},
    v_origin=_,
    v_extent=none}=_) ->
    {num_sign(WX2 - WX1), num_sign(WY2 - WY1)};
viewport_to_window_1([], #win_viewp{
    w_origin={WX1,WY1},
    w_extent={WX2,WY2},
    v_origin={VX1,VY1},
    v_extent={VX2,VY2}}=_) ->
    {(VX2 - VX1) / (WX2 - WX1),
     (VY2 - VY1) / (WY2 - WY1)};
viewport_to_window_1([{window, Which}|L], WinViewP) ->
    case Which of
        {origin, XY} -> viewport_to_window_1(L, WinViewP#win_viewp{w_origin=XY});
        {extent, XY} -> viewport_to_window_1(L, WinViewP#win_viewp{w_extent=XY})
    end;
viewport_to_window_1([{viewport, Which}|L], WinViewP) ->
    case Which of
        {origin, XY} -> viewport_to_window_1(L, WinViewP#win_viewp{v_origin=XY});
        {extent, XY} -> viewport_to_window_1(L, WinViewP#win_viewp{v_extent=XY})
    end;
viewport_to_window_1([_|L], WinViewP) ->
    viewport_to_window_1(L, WinViewP).

num_sign(A) when A < 0.0 ->
    -1.0;
num_sign(_) ->
    1.0.


repeat_matlist_if_needed(MatList, Pas) ->
    repeat_matlist_if_needed(MatList, Pas, []).
repeat_matlist_if_needed([], [], O) ->
    lists:append(lists:reverse(O));
repeat_matlist_if_needed([M|MatList], [P|Pas], O) ->
    M_1 = [M || _I <- lists:seq(1, length(P))],
    repeat_matlist_if_needed(MatList, Pas, [M_1|O]).


into_mesh_parts(Objs, MatList) ->
    into_mesh_parts(Objs, MatList, 0, 0, 0, [], [], [], []).
into_mesh_parts([], _, _, _, _, Vs_L, Fs_L, Tx_L, He_L) ->
    {lists:append(lists:reverse(Vs_L)),
     lists:append(lists:reverse(Fs_L)),
     lists:append(lists:reverse(Tx_L)),
     lists:append(lists:reverse(He_L))};
into_mesh_parts([{Vs,Fs0,He0} | Objs], [{Mat1, T} | MatList], ColorIdx,
                VsOffset, TxOffset, Vs_L, Fs_L, Tx_L, He_L) ->
    case Mat1 of
        none ->
            Tx = [],
            TxOffset_1 = TxOffset;
        {BitmapCoords, _} ->
            Tx = [to_uv(BitmapCoords, X, Y) || {X,Y,_} <- Vs],
            TxOffset_1 = TxOffset + length(Vs)
    end,
    VsOffset_1 = VsOffset + length(Vs),
    EFs = [ #e3d_face{
        vs=[V+VsOffset || V <- F],
        vc=[ColorIdx   || _ <- F],
        tx=[V+TxOffset || V <- F],
        mat=
            case Mat1 of
                none -> [];
                {_, M} -> [M]
            end
    } || F <- Fs0],
    He = [{V1+VsOffset,V2+VsOffset} || {V1,V2} <- He0],
    
    CenterOffset = e3d_vec:average(e3d_vec:bounding_box(Vs)),
    Vs_1 = [transform_vs({X,Y,Z}, T, CenterOffset) || {X,Y,Z} <- Vs],
    
    into_mesh_parts(Objs, MatList, ColorIdx + 1, VsOffset_1, TxOffset_1,
                    [Vs_1 | Vs_L], [EFs | Fs_L], [Tx | Tx_L], [He | He_L]).

to_uv({{DestX1,DestY1}, {DestX2,DestY2}, {_SrcX1,_SrcY1}, {_SrcX2,_SrcY2}}, X, Y) ->
    UV = {(X-DestX1) / (DestX2-DestX1), (Y-DestY1) / -(DestY2-DestY1)},
    UV.

transform_vs({X, Y, Z}, TL, {XC,YC,ZC}) ->
    {X1,Y1,Z1} = transform_vs_1({X-XC,Y-YC,Z-ZC}, TL),
    {X1+XC,Y1+YC,Z1+ZC}.
transform_vs_1({X, Y, Z}=V, [T|TL]) ->
    case T of
        {x, Val} ->
            transform_vs_1({X+Val, Y, Z}, TL);
        {y, Val} ->
            transform_vs_1({X, Y+Val, Z}, TL);
        {z, Val} ->
            transform_vs_1({X, Y, Z+Val}, TL);
        {scalex, Val} ->
            transform_vs_1({X*Val, Y, Z}, TL);
        {scaley, Val} ->
            transform_vs_1({X, Y*Val, Z}, TL);
        {scalez, Val} ->
            transform_vs_1({X, Y, Z*Val}, TL);
        {rotx, Ang} ->
            Y1 = (Y * math:cos(Ang)) + (Z * math:sin(Ang)),
            Z1 = (Z * math:cos(Ang)) - (Y * math:sin(Ang)),
            transform_vs_1({X, Y1, Z1}, TL);
        {roty, Ang} ->
            X1 = (X * math:cos(Ang)) + (Z * math:sin(Ang)),
            Z1 = (Z * math:cos(Ang)) - (X * math:sin(Ang)),
            transform_vs_1({X1, Y, Z1}, TL);
        {rotz, Ang} ->
            X1 = (X * math:cos(Ang)) + (Y * math:sin(Ang)),
            Y1 = (Y * math:cos(Ang)) - (X * math:sin(Ang)),
            transform_vs_1({X1, Y1, Z}, TL);
        _ ->
            transform_vs_1(V, TL)
    end;
transform_vs_1({_X, _Y, _Z}=V, []) ->
    V.
    

get_file_type(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
        ".wmf" -> wmf;
        ".emf" -> emf
    end.

read_file(wmf, FileName, _NumDiv) ->
    read_wmf_file(FileName);
read_file(emf, FileName, NumDiv) ->
    read_emf_file(FileName, NumDiv).

into_paths(FType, CommandsList) ->
    remove_bad_paths(into_paths_1(FType, CommandsList)).
into_paths_1(wmf, CommandsList) ->
    wmf_into_paths(CommandsList);
into_paths_1(emf, CommandsList) ->
    emf_into_paths(CommandsList).


cedge_realize(wmf, Commands) ->
    wmf_cedge_realize(Commands);
cedge_realize(emf, Commands) ->
    emf_cedge_realize(Commands).


%%
%% EMF Specific
%%

-define(E_ANGLEARC, 41).
-define(E_ARC, 45).
-define(E_ARCTO, 55).
-define(E_CHORD, 46).
-define(E_ELLIPSE, 42).
-define(E_MOVETOEX, 27).
-define(E_LINETO, 54).
-define(E_PIE, 47).
-define(E_POLYBEZIER, 2).
-define(E_POLYBEZIER16, 85).
-define(E_POLYBEZIERTO, 5).
-define(E_POLYBEZIERTO16, 88).
-define(E_POLYDRAW, 56).
-define(E_POLYDRAW16, 92).
-define(E_POLYGON, 3).
-define(E_POLYGON16, 86).
-define(E_POLYLINE, 4).
-define(E_POLYLINE16, 87).
-define(E_POLYLINETO, 6).
-define(E_POLYLINETO16, 89).
-define(E_POLYPOLYGON, 8).
-define(E_POLYPOLYGON16, 91).
-define(E_POLYPOLYLINE, 7).
-define(E_POLYPOLYLINE16, 90).
-define(E_POLYTEXTOUTA, 96).
-define(E_POLYTEXTOUTW, 97).
-define(E_RECTANGLE, 43).
-define(E_ROUNDRECT, 44).

-define(E_EXTTEXTOUTA, 83).
-define(E_EXTTEXTOUTW, 84).

-define(E_BEGINPATH, 59).
-define(E_CLOSEFIGURE, 61).
-define(E_CREATEBRUSHINDIRECT, 39).
-define(E_CREATEPEN, 38).
-define(E_DELETEOBJECT, 40).
-define(E_ENDPATH, 60).
-define(E_EOF, 14).
-define(E_EXTCREATEFONTINDIRECTW, 82).
-define(E_EXTCREATEPEN, 95).
-define(E_FILLPATH, 62).
-define(E_GDICOMMENT, 70).
-define(E_HEADER, 1).
-define(E_INTERSECTCLIPRECT, 30).
-define(E_MODIFYWORLDTRANSFORM, 36).
-define(E_RESTOREDC, 34).
-define(E_SAVEDC, 33).
-define(E_SELECTOBJECT, 37).
-define(E_SETBKMODE, 18).
-define(E_SETMITERLIMIT, 58).
-define(E_SETPOLYFILLMODE, 19).
-define(E_SETROP2, 20).
-define(E_SETTEXTALIGN, 22).
-define(E_SETTEXTCOLOR, 24).
-define(E_SETVIEWPORTEXTEX, 11).
-define(E_SETVIEWPORTORGEX, 12).
-define(E_SETWINDOWEXTEX, 9).
-define(E_SETWINDOWORGEX, 10).
-define(E_STROKEANDFILLPATH, 63).
-define(E_STROKEPATH, 64).

%% Unimplemented
-define(E_ABORTPATH, 68).
-define(E_CREATEMONOBRUSH, 93).
-define(E_CREATEPALETTE, 49).
-define(E_EXCLUDECLIPRECT, 29).
-define(E_EXTFLOODFILL, 53).
-define(E_EXTSELECTCLIPRGN, 75).
-define(E_FILLRGN, 71).
-define(E_FLATTENPATH, 65).
-define(E_FRAMERGN, 72).
-define(E_INVERTRGN, 73).
-define(E_OFFSETCLIPRGN, 26).
-define(E_PAINTRGN, 74).
-define(E_REALIZEPALETTE, 52).
-define(E_RESIZEPALETTE, 51).
-define(E_SCALEVIEWPORTEXTEX, 31).
-define(E_SCALEWINDOWEXTEX, 32).
-define(E_SELECTCLIPPATH, 67).
-define(E_SELECTPALETTE, 48).
-define(E_SETARCDIRECTION, 57).
-define(E_SETBKCOLOR, 25).
-define(E_SETBRUSHORGEX, 13).
-define(E_SETCOLORADJUSTMENT, 23).
-define(E_SETMAPMODE, 17).
-define(E_SETMAPPERFLAGS, 16).
-define(E_SETMETARGN, 28).
-define(E_SETPALETTEENTRIES, 50).
-define(E_SETPIXELV, 15).
-define(E_SETWORLDTRANSFORM, 35).
-define(E_WIDENPATH, 66).

%% Unimplemented
-define(E_CREATEDIBPATTERNBRUSHPT, 94).
-define(E_BITBLT, 76).
-define(E_MASKBLT, 78).
-define(E_PLGBLT, 79).
-define(E_SETSTRETCHBLTMODE, 21).
-define(E_SETDIBITSTODEVICE, 80).
-define(E_STRETCHBLT, 77).
-define(E_STRETCHDIBITS, 81).


read_emf_file(FileName, NumDiv) ->
    
    {ok, Fp2} = file:open(FileName, [read, binary]),

    {ok, << RecordType:32/?UINT,       % Record type
            RecordSize:32/?UINT,       % Record size
            _BoundsLeft:32/?SINT,
            _BoundsRight:32/?SINT,
            _BoundsTop:32/?SINT,
            _BoundsBottom:32/?SINT,
            _FrameLeft:32/?SINT,
            _FrameRight:32/?SINT,
            _FrameTop:32/?SINT,
            _FrameBottom:32/?SINT,
            Signature:32/?UINT,        % Always 0x464D4520
            _Version:32/?UINT,          % Version of the metafile
            _Size:32/?UINT,
            NumObj:32/?UINT,           % Number of records in the metafile
            _NumOfHandles:16/?UINT,    % Number of handles in the handle table
            _Reserved:16/?UINT,
            _SizeOfDescrip:32/?UINT,
            _OffsOfDescrip:32/?UINT,
            _NumPalEntries:32/?UINT,
            WidthDevPixels:32/?SINT,
            HeightDevPixels:32/?SINT,
            WidthDevMM:32/?SINT,
            HeightDevMM:32/?SINT
            >>} = file:read(Fp2, 88),
    
    %% Get DPI information from four variables
    W_DPI = WidthDevPixels / conv_unit({WidthDevMM, mm}, in, 96),
    H_DPI = HeightDevPixels / conv_unit({HeightDevMM, mm}, in, 96),
    %% Average the two
    DPI = (W_DPI + H_DPI) / 2.0,
    
    {ok, _Reserved2} = file:read(Fp2, RecordSize - 88),
    case RecordType =:= 1 andalso Signature =:= 16#464D4520 of
        false -> ?DEBUG_FMT("RecordType = ~w", [RecordType]), error;
        true ->
            CommandList = emf_loop(Fp2, NumObj, NumDiv, []),
            {ok, CommandList, DPI}
    end.
    
emf_loop(Fp2, NumObj, NumDiv, Commands) ->
    {ok, <<Command:32/?SINT>>} = file:read(Fp2, 4),
    {ok, <<NumBytes0:32/?UINT>>} = file:read(Fp2, 4),
    NumBytes = NumBytes0 - 8,
    case Command of
        0 -> file:close(Fp2), done;
        ?E_EOF ->
            file:close(Fp2),
            lists:reverse(Commands);
        _ ->
            NewCommand = case Command of

                ?E_POLYBEZIER              -> emf_cmd_polybezier(Fp2, NumDiv);
                ?E_POLYBEZIER16            -> emf_cmd_polybezier16(Fp2, NumDiv);
                ?E_POLYBEZIERTO            -> emf_cmd_polybezierto(Fp2,
                                                last_linestart_coordinate(Commands), NumDiv);
                ?E_POLYBEZIERTO16          -> emf_cmd_polybezierto16(Fp2,
                                                last_linestart_coordinate(Commands), NumDiv);
                ?E_POLYDRAW                -> emf_cmd_polydraw(Fp2, NumDiv);
                ?E_POLYDRAW16              -> emf_cmd_polydraw16(Fp2, NumDiv);
                ?E_POLYGON                 -> emf_cmd_polygon(Fp2);
                ?E_POLYGON16               -> emf_cmd_polygon16(Fp2);
                ?E_POLYLINE                -> emf_cmd_polyline(Fp2);
                ?E_POLYLINE16              -> emf_cmd_polyline16(Fp2);
                ?E_POLYLINETO              -> emf_cmd_polylineto(Fp2);
                ?E_POLYLINETO16            -> emf_cmd_polylineto16(Fp2);
                ?E_POLYPOLYGON             -> emf_cmd_polypolygon(Fp2);
                ?E_POLYPOLYGON16           -> emf_cmd_polypolygon16(Fp2);
                ?E_POLYPOLYLINE            -> emf_cmd_polypolyline(Fp2);
                ?E_POLYPOLYLINE16          -> emf_cmd_polypolyline16(Fp2);
                ?E_MOVETOEX                -> emf_cmd_moveto(Fp2);
                ?E_LINETO                  -> emf_cmd_lineto(Fp2);
                ?E_CHORD                   -> emf_cmd_chord(Fp2);
                ?E_CLOSEFIGURE             -> emf_cmd_closefigure(Fp2,
                                                last_linecontinue_coordinate(Commands));
                
                ?E_ANGLEARC                -> emf_cmd_anglearc(Fp2);
                ?E_ARC                     -> emf_cmd_arc(Fp2);
                ?E_ARCTO                   -> emf_cmd_arcto(Fp2);
                ?E_ELLIPSE                 -> emf_cmd_ellipse(Fp2);
                ?E_PIE                     -> emf_cmd_pie(Fp2);
                ?E_RECTANGLE               -> emf_cmd_rectangle(Fp2);
                ?E_ROUNDRECT               -> emf_cmd_roundrect(Fp2);

                ?E_BEGINPATH               -> emf_cmd_beginpath(Fp2);
                ?E_STROKEANDFILLPATH       -> emf_cmd_strokeandfillpath(Fp2);
                ?E_FILLPATH                -> emf_cmd_fillpath(Fp2, NumBytes);
                ?E_ENDPATH                 -> emf_cmd_endpath(Fp2);
                ?E_SETPOLYFILLMODE         -> emf_cmd_setpolyfillmode(Fp2);
                
                ?E_SAVEDC                  -> emf_cmd_savedc(Fp2, NumBytes);
                ?E_RESTOREDC               -> emf_cmd_restoredc(Fp2, NumBytes);
                ?E_CREATEPEN               -> emf_cmd_createpen(Fp2, NumBytes);
                ?E_CREATEBRUSHINDIRECT     -> emf_cmd_createbrushindirect(Fp2, NumBytes);
                ?E_INTERSECTCLIPRECT       -> emf_cmd_skip(Fp2, NumBytes);
                ?E_SETVIEWPORTEXTEX        -> emf_cmd_setviewportextex(Fp2, NumBytes);
                ?E_SETWINDOWEXTEX          -> emf_cmd_setwindowextex(Fp2, NumBytes);
                ?E_SETVIEWPORTORGEX        -> emf_cmd_setviewportorgex(Fp2, NumBytes);
                ?E_SETWINDOWORGEX          -> emf_cmd_setwindoworgex(Fp2, NumBytes);
                ?E_SETWORLDTRANSFORM       -> emf_cmd_setworldtransform(Fp2, NumBytes);
                ?E_SETMAPMODE              -> emf_cmd_setmapmode(Fp2, NumBytes);
                ?E_MODIFYWORLDTRANSFORM    -> emf_cmd_modifyworldtransform(Fp2, NumBytes);
                ?E_SETBKMODE               -> emf_cmd_skip(Fp2, NumBytes);
                ?E_STROKEPATH              -> emf_cmd_skip(Fp2, NumBytes);
                ?E_SETMITERLIMIT           -> emf_cmd_skip(Fp2, NumBytes);
                ?E_EXTCREATEPEN            -> emf_cmd_extcreatepen(Fp2, NumBytes);
                ?E_SELECTOBJECT            -> emf_cmd_selectobject(Fp2, NumBytes);
                ?E_DELETEOBJECT            -> emf_cmd_skip(Fp2, NumBytes);
                
                ?E_SELECTCLIPPATH          -> emf_cmd_selectclippath(Fp2, NumBytes);
                
                %% Text related
                ?E_EXTCREATEFONTINDIRECTW  -> emf_cmd_extcreatepen(Fp2, NumBytes);
                ?E_SETTEXTALIGN            -> emf_cmd_skip(Fp2, NumBytes);
                ?E_SETTEXTCOLOR            -> emf_cmd_skip(Fp2, NumBytes);
                ?E_EXTTEXTOUTA             -> emf_cmd_skip(Fp2, NumBytes);
                ?E_EXTTEXTOUTW             -> emf_cmd_skip(Fp2, NumBytes);
                
                %% Ignored for vector paths
                ?E_SETROP2                 -> emf_cmd_skip(Fp2, NumBytes);
                
                %% Ignored by design
                ?E_GDICOMMENT              -> emf_cmd_skip(Fp2, NumBytes);
                
                %% For texture
                ?E_STRETCHDIBITS           -> emf_cmd_stretchdibits(Fp2, NumBytes);
                ?E_SETDIBITSTODEVICE       -> emf_cmd_setdibits(Fp2, NumBytes);
                
                %% Unimplemented bitmap related
                ?E_CREATEDIBPATTERNBRUSHPT -> emf_cmd_extcreatepen(Fp2, NumBytes);
                ?E_MASKBLT                 -> emf_cmd_skip(Fp2, NumBytes);
                ?E_SETSTRETCHBLTMODE       -> emf_cmd_skip(Fp2, NumBytes);
                ?E_BITBLT                  -> emf_cmd_skip(Fp2, NumBytes);
                ?E_PLGBLT                  -> emf_cmd_skip(Fp2, NumBytes);
                ?E_STRETCHBLT              -> emf_cmd_skip(Fp2, NumBytes);
                
                %% Unknown or unimplemented commands.
                _   ->
                    ?DEBUG_FMT("unk command: ~w size: ~w~n", [Command, NumBytes]),
                    emf_cmd_unknown(Fp2, NumBytes)
            end,
            case NewCommand of
                [] ->
                    Commands_1 = Commands;
                L when is_list(L) ->
                    Commands_1 = lists:reverse(NewCommand) ++ Commands;
                _ ->
                    Commands_1 = [NewCommand|Commands]
            end,
            emf_loop(Fp2, NumObj, NumDiv, Commands_1)
    end.


last_linestart_coordinate([]) -> {0,0};
last_linestart_coordinate([{linestart, Path} | _]) when length(Path) > 0 ->
    [Coord={_X,_Y} | _] = lists:reverse(Path),
    Coord;
last_linestart_coordinate([{linecontinue, Path} | _]) when length(Path) > 0 ->
    [Coord={_X,_Y} | _] = lists:reverse(Path),
    Coord;
last_linestart_coordinate([_ | R]) ->
    last_linestart_coordinate(R).
    

last_linecontinue_coordinate([]) -> {0,0};
last_linecontinue_coordinate([{linestart, Path} | _]) when length(Path) > 0 ->
    [Coord={_X,_Y} | _] = lists:reverse(Path),
    Coord;
last_linecontinue_coordinate([_ | R]) ->
    last_linecontinue_coordinate(R).
    

emf_cmd_unknown(_  , 0) -> unused;
emf_cmd_unknown(Fp2, NumBytes) ->
    {ok, <<_B:8/?SINT>>} = file:read(Fp2, 1),
    emf_cmd_unknown(Fp2, NumBytes-1).
    
emf_cmd_chord(Fp2) ->
    {ok, <<X1:32/?SINT, Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, <<X2:32/?SINT, Y2:32/?SINT>>} = file:read(Fp2, 8),
    {ok, <<XR1:32/?SINT, YR1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, <<XR2:32/?SINT, YR2:32/?SINT>>} = file:read(Fp2, 8),
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    [C | R] = paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, []),
    {set, [C] ++ R ++ [C]}.

emf_cmd_skip(_  , 0) -> unused;
emf_cmd_skip(Fp2, NumBytes) ->
    {ok, <<_:8/?SINT>>} = file:read(Fp2, 1),
    emf_cmd_skip(Fp2, NumBytes-1).
    
emf_cmd_savedc(Fp2, NumBytes) ->
    {ok, _} = file:read(Fp2, NumBytes),
    {save_dc, 0}.

emf_cmd_restoredc(Fp2, NumBytes) ->
    {ok, _} = file:read(Fp2, NumBytes),
    {restore_dc, 0}.

emf_cmd_setmapmode(Fp2, 4) ->
    {ok, <<_Mode:32/?SINT>>} = file:read(Fp2, 4),
    unused.

%% Viewports and window
emf_cmd_setviewportorgex(Fp2, 8) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {viewport, {origin, {float(X1),float(Y1)}}}.
emf_cmd_setwindoworgex(Fp2, 8) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {window, {origin, {float(X1),float(Y1)}}}.
emf_cmd_setviewportextex(Fp2, 8) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {viewport, {extent, {float(X1),float(Y1)}}}.
emf_cmd_setwindowextex(Fp2, 8) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {window, {extent, {float(X1),float(Y1)}}}.


emf_cmd_setworldtransform(Fp2, NumBytes) ->
    {ok, <<
        EM11:32/?FLT,
        EM12:32/?FLT,
        EM21:32/?FLT,
        EM22:32/?FLT,
        EDx:32/?FLT,
        EDy:32/?FLT,
        _/binary
        >>
        } = file:read(Fp2, NumBytes),
    {world_transform, {{EM11, EM12, EM21, EM22}, {EDx, EDy}}}.
    
emf_cmd_selectclippath(Fp2, 4) ->
    {ok, <<_ClipPath:32/?SINT>>} = file:read(Fp2, 4),
    {selectclippath, 0}.
    
emf_cmd_modifyworldtransform(Fp2, NumBytes) ->
    {ok, <<
        EM11:32/?FLT,
        EM12:32/?FLT,
        EM21:32/?FLT,
        EM22:32/?FLT,
        EDx:32/?FLT,
        EDy:32/?FLT,
        _Mode:32/?UINT,
        _/binary
        >>
        } = file:read(Fp2, NumBytes),
    {world_transform, {{EM11, EM12, EM21, EM22}, {EDx, EDy}}}.


emf_cmd_createpen(Fp2, NumBytes) ->
    {ok, << Index:32/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_skip(Fp2, NumBytes-4),
    {alloc_color, {Index, ignore}}.
emf_cmd_extcreatepen(Fp2, NumBytes) ->
    {ok, << Index:32/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_skip(Fp2, NumBytes-4),
    {alloc_color, {Index, ignore}}.

emf_cmd_createbrushindirect(Fp2, 16) ->
    {ok, << Index:32/?SINT,
            _Unk2:32/?SINT,
            Red:8/?UINT,
            Green:8/?UINT,
            Blue:8/?UINT,
            _Unk3:8/?SINT,
            _Unk4:32/?SINT>>} = file:read(Fp2, 16),
    {alloc_color, {Index, {Red, Green, Blue}}}.


emf_cmd_selectobject(Fp2, 4) ->
    {ok, << ObjNumber:16/?SINT,
            Unk:16/?SINT>>} = file:read(Fp2, 4),
    case Unk > 0 of
        true ->
            unused;
        false ->
            {select_object, ObjNumber}
    end;
emf_cmd_selectobject(Fp2, NumBytes) -> emf_cmd_skip(Fp2, NumBytes).
    

emf_cmd_moveto(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {linestart, [{X1, Y1}]}.

emf_cmd_lineto(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {linecontinue, [{X1, Y1}]}.
    
emf_cmd_closefigure(_Fp2, FirstCoord) ->
    {linecontinue, [FirstCoord]}.
    

-define(EMF_FILLMODE_WINDING, 2).
-define(EMF_FILLMODE_ALTERNATE, 1).
emf_cmd_setpolyfillmode(Fp2) ->
    {ok, << Mode:32/?SINT>>} = file:read(Fp2, 4),
    case Mode of
        ?EMF_FILLMODE_WINDING -> {fill_mode, winding};
        ?EMF_FILLMODE_ALTERNATE -> {fill_mode, alternate}
    end.

emf_cmd_beginpath(_Fp2) ->
    beginpath.

emf_cmd_strokeandfillpath(Fp2) ->
    {ok, << _Unused1:32/?SINT,
            _Unused2:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << _Unused3:32/?SINT,
            _Unused4:32/?SINT>>} = file:read(Fp2, 8),
    unused.

emf_cmd_fillpath(Fp2, NumBytes) ->
    emf_cmd_skip(Fp2, NumBytes).

emf_cmd_endpath(_Fp2) ->
    endpath.

read_emf_bbox(Fp2) ->
    {ok, << BoundRectLeft:32/?SINT, BoundRectTop:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << BoundRectRight:32/?SINT, BoundRectBottom:32/?SINT>>} = file:read(Fp2, 8),
    {BoundRectLeft, BoundRectTop, BoundRectRight, BoundRectBottom}.
    

emf_cmd_polyline(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, << NumSeg:32/?SINT>>} = file:read(Fp2, 4),
    emf_cmd_polyline_nextseg(Fp2, NumSeg, []).
emf_cmd_polyline_nextseg(_, 0, Coords) -> {set, emf_close_poly(lists:reverse(Coords))};
emf_cmd_polyline_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    emf_cmd_polyline_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).

emf_cmd_polyline16(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, << NumSeg:32/?SINT>>} = file:read(Fp2, 4),
    emf_cmd_polyline16_nextseg(Fp2, NumSeg, []).
emf_cmd_polyline16_nextseg(_, 0, Coords) -> {set, emf_close_poly(lists:reverse(Coords))};
emf_cmd_polyline16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT>>} = file:read(Fp2, 4),
    emf_cmd_polyline16_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).


emf_cmd_polylineto(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, << NumSeg:32/?SINT>>} = file:read(Fp2, 4),
    emf_cmd_polylineto_nextseg(Fp2, NumSeg, []).
emf_cmd_polylineto_nextseg(_, 0, Coords) -> {linecontinue, lists:reverse(Coords)};
emf_cmd_polylineto_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polylineto_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).

emf_cmd_polylineto16(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polylineto16_nextseg(Fp2, NumSeg, []).
emf_cmd_polylineto16_nextseg(_, 0, Coords) -> {linecontinue, lists:reverse(Coords)};
emf_cmd_polylineto16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polylineto16_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).


emf_cmd_polybezierto(Fp2, PrevCoord, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polybezierto_nextseg(Fp2, NumSeg, NumDiv, [PrevCoord]).
emf_cmd_polybezierto_nextseg(_, 0, NumDiv, Coords) ->
    {linecontinue, emf_paths_from_bezier(lists:reverse(Coords), NumDiv)};
emf_cmd_polybezierto_nextseg(Fp2, NumSeg, NumDiv, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polybezierto_nextseg(Fp2, NumSeg-1, NumDiv, [{X1,Y1}|Coords]).

emf_cmd_polybezierto16(Fp2, PrevCoord, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polybezierto16_nextseg(Fp2, NumSeg, NumDiv, [PrevCoord]).
emf_cmd_polybezierto16_nextseg(_, 0, NumDiv, Coords) ->
    {linecontinue, emf_paths_from_bezier(lists:reverse(Coords), NumDiv)};
emf_cmd_polybezierto16_nextseg(Fp2, NumSeg, NumDiv, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polybezierto16_nextseg(Fp2, NumSeg-1, NumDiv, [{X1,Y1}|Coords]).
    

emf_cmd_anglearc(Fp2) ->
    {ok, << XC:32/?SINT,
            YC:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << Radius:32/?SINT>>} = file:read(Fp2, 4),
    {ok, << StartAngle_0:32/?FLT,
            AngleDiff_0:32/?FLT>>} = file:read(Fp2, 8),
            
    NumPoints = round(abs(AngleDiff_0 / 180 * 10)),
    OPath = anglearc_points({XC, YC}, 0, NumPoints, StartAngle_0, AngleDiff_0, Radius, []),
    {linecontinue, OPath}.
anglearc_points({XC, YC}, I, NumPoints, StartAngle, AngleDiff, Radius, OPath)
  when I =:= NumPoints ->
    lists:reverse([anglearc_cos_sin({XC, YC}, StartAngle+AngleDiff, Radius)|OPath]);
anglearc_points({XC, YC}=Coord, I, NumPoints, StartAngle, AngleDiff, Radius, OPath)
  when I < NumPoints ->
    Ang = StartAngle + AngleDiff * (float(I) / NumPoints),
    OPath_1 = [anglearc_cos_sin({XC, YC}, Ang, Radius)|OPath],
    anglearc_points(Coord, I+1, NumPoints, StartAngle, AngleDiff, Radius, OPath_1).
anglearc_cos_sin({XC, YC}, Ang, Radius) ->
    X1 = XC + math:cos(Ang / 360.0 * math:pi() * 2.0) * Radius,
    Y1 = YC - math:sin(Ang / 360.0 * math:pi() * 2.0) * Radius,
    {X1, Y1}.

emf_cmd_arc(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << X2_0:32/?SINT, %% 1 lower than win32 argument
            Y2_0:32/?SINT>>} = file:read(Fp2, 8), %% 1 lower than win32 argument
    {ok, << XR1:32/?SINT,
            YR1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << XR2:32/?SINT,
            YR2:32/?SINT>>} = file:read(Fp2, 8),
    X2 = X2_0 + 1,
    Y2 = Y2_0 + 1,
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    %% Since all shapes need to be filled, this has to be filled too.
    [C | R] = paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, []),
    {set, [C] ++ R ++ [C]}.

emf_cmd_arcto(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << Right_0:32/?SINT, %% 1 lower than win32 argument
            Bottom_0:32/?SINT>>} = file:read(Fp2, 8), %% 1 lower than win32 argument
    {ok, << XR1:32/?SINT,
            YR1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << XR2:32/?SINT,
            YR2:32/?SINT>>} = file:read(Fp2, 8),
    X2 = Right_0 + 1,
    Y2 = Bottom_0 + 1,
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    {linecontinue, paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, [])}.

emf_cmd_ellipse(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << Right_0:32/?SINT, %% 1 lower than win32 argument
            Bottom_0:32/?SINT>>} = file:read(Fp2, 8), %% 1 lower than win32 argument
    X2 = Right_0 + 1,
    Y2 = Bottom_0 + 1,
    {set, paths_round_rect(X1,Y1,X2,Y2,(X2-X1)/2.0,(Y2-Y1)/2.0)}.
  
emf_cmd_pie(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << Right_0:32/?SINT, %% 1 lower than win32 argument
            Bottom_0:32/?SINT>>} = file:read(Fp2, 8), %% 1 lower than win32 argument
    {ok, << XR1:32/?SINT,
            YR1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << XR2:32/?SINT,
            YR2:32/?SINT>>} = file:read(Fp2, 8),
    X2 = Right_0 + 1,
    Y2 = Bottom_0 + 1,
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    X3 = XC,
    Y3 = YC,
    {set, [{X3,Y3} | paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, [{X3, Y3}])]}.

emf_cmd_rectangle(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << X2_0:32/?SINT,
            Y2_0:32/?SINT>>} = file:read(Fp2, 8),
    X2 = X2_0 + 1,
    Y2 = Y2_0 + 1,
    {set, [{X1,Y1}, {X1,Y2}, {X2,Y2}, {X2,Y1}, {X1,Y1}]}.
  
emf_cmd_roundrect(Fp2) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << X2_0:32/?SINT,
            Y2_0:32/?SINT>>} = file:read(Fp2, 8),
    {ok, << WR:32/?SINT,
            HR:32/?SINT>>} = file:read(Fp2, 8),
    X2 = X2_0 + 1,
    Y2 = Y2_0 + 1,
    {set, paths_round_rect(X1,Y1,X2,Y2,WR,HR)}.


emf_cmd_polybezier(Fp2, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polybezier_nextseg(Fp2, NumSeg, NumDiv, []).
emf_cmd_polybezier_nextseg(_, 0, NumDiv, Coords) ->
    {set, emf_close_poly(emf_paths_from_bezier(lists:reverse(Coords), NumDiv))};
emf_cmd_polybezier_nextseg(Fp2, NumSeg, NumDiv, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polybezier_nextseg(Fp2, NumSeg-1, NumDiv, [{X1, Y1}|Coords]).



emf_cmd_polybezier16(Fp2, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polybezier16_nextseg(Fp2, NumSeg, NumDiv, []).
emf_cmd_polybezier16_nextseg(_, 0, NumDiv, Coords) ->
    {set, emf_close_poly(emf_paths_from_bezier(lists:reverse(Coords), NumDiv))};
emf_cmd_polybezier16_nextseg(Fp2, NumSeg, NumDiv, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polybezier16_nextseg(Fp2, NumSeg-1, NumDiv, [{X1, Y1}|Coords]).


emf_cmd_polydraw(Fp2, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    Paths = emf_cmd_polydraw_nextseg(Fp2, NumSeg, []),
    PathTypes = emf_cmd_polydraw_nextptype(Fp2, NumSeg, []),
    %% Align to 4 byte boundary
    case NumSeg rem 4 > 0 of
        true -> emf_cmd_polydraw_padbytes(Fp2, 4 - (NumSeg rem 4));
        _    -> ok
    end,
    emf_paths_poly_draw(Paths, PathTypes, NumDiv).
emf_cmd_polydraw_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polydraw_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polydraw_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).
emf_cmd_polydraw_nextptype(_, 0, PointTypes) -> lists:reverse(PointTypes);
emf_cmd_polydraw_nextptype(Fp2, NumSeg, PointTypes) ->
    {ok, << PointType:8/?SINT >>} = file:read(Fp2, 1),
    emf_cmd_polydraw_nextptype(Fp2, NumSeg-1, [PointType|PointTypes]).
emf_cmd_polydraw_padbytes(_, 0) -> ok;
emf_cmd_polydraw_padbytes(Fp2, NumSeg) ->
    {ok, << _ >>} = file:read(Fp2, 1),
    emf_cmd_polydraw_padbytes(Fp2, NumSeg-1).



emf_cmd_polydraw16(Fp2, NumDiv) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    Paths = emf_cmd_polydraw16_nextseg(Fp2, NumSeg, []),
    PathTypes = emf_cmd_polydraw16_nextptype(Fp2, NumSeg, []),
    %% Align to 4 byte boundary
    case NumSeg rem 4 > 0 of
        true -> emf_cmd_polydraw16_padbytes(Fp2, 4 - (NumSeg rem 4));
        _    -> ok
    end,
    emf_paths_poly_draw(Paths, PathTypes, NumDiv).
emf_cmd_polydraw16_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polydraw16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polydraw16_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).
emf_cmd_polydraw16_nextptype(_, 0, PointTypes) -> lists:reverse(PointTypes);
emf_cmd_polydraw16_nextptype(Fp2, NumSeg, PointTypes) ->
    {ok, << PointType:8/?SINT >>} = file:read(Fp2, 1),
    emf_cmd_polydraw16_nextptype(Fp2, NumSeg-1, [PointType|PointTypes]).
emf_cmd_polydraw16_padbytes(_, 0) -> ok;
emf_cmd_polydraw16_padbytes(Fp2, NumSeg) ->
    {ok, << _ >>} = file:read(Fp2, 1),
    emf_cmd_polydraw16_padbytes(Fp2, NumSeg-1).


-define(POLY_DRAW_PT_MOVETO, 6).
-define(POLY_DRAW_PT_LINETO, 2).
-define(POLY_DRAW_PT_BEZIERTO, 4).
-define(POLY_DRAW_PT_CLOSEFIGURE, 1).
emf_paths_poly_draw(Paths, PathTypes, NumDiv) ->
    emf_paths_poly_draw(Paths, PathTypes, NumDiv, []).
emf_paths_poly_draw([], [], _NumDiv, OPaths) ->
    lists:reverse(OPaths);
emf_paths_poly_draw([{X1,Y1} | _Paths], [], NumDiv, OPaths) ->
    emf_paths_poly_draw([], [], NumDiv, [{linecontinue, [{X1,Y1}]} | OPaths]);
emf_paths_poly_draw(
  [{X2,Y2}, {X3,Y3}, {X4,Y4}| Paths],
  [?POLY_DRAW_PT_BEZIERTO, ?POLY_DRAW_PT_BEZIERTO, ?POLY_DRAW_PT_BEZIERTO | PathTypes], NumDiv,
  [{_,PrevPath}| _]=OPaths) ->
    [{X1,Y1} | _] = lists:reverse(PrevPath),
    emf_paths_poly_draw(
        [{X4,Y4} | Paths], PathTypes, NumDiv,
        [{linecontinue, lists:reverse(paths_bezier_4_points({X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}, NumDiv))} | OPaths]);
emf_paths_poly_draw([{X1,Y1} | Paths],
                    [?POLY_DRAW_PT_MOVETO | PathTypes], NumDiv, OPaths) ->
    emf_paths_poly_draw(
        Paths, PathTypes, NumDiv,
        [{linestart, [{X1,Y1}]} | OPaths]);
emf_paths_poly_draw([{X1,Y1} | Paths],
                    [?POLY_DRAW_PT_LINETO | PathTypes], NumDiv, OPaths) ->
    emf_paths_poly_draw(
        Paths, PathTypes, NumDiv,
        [{linecontinue, [{X1,Y1}]} | OPaths]);
emf_paths_poly_draw([{X1,Y1} | Paths],
                    [?POLY_DRAW_PT_CLOSEFIGURE | PathTypes], NumDiv, OPaths) ->
    emf_paths_poly_draw(
        Paths, PathTypes, NumDiv,
        [{linecontinue, [{X1,Y1}]} | OPaths]);
emf_paths_poly_draw([{X1,Y1} | Paths], [_ | PathTypes], NumDiv, OPaths) ->
    %% Just draw a line if we're not sure
    emf_paths_poly_draw(
        Paths, PathTypes, NumDiv,
        [{linecontinue, [{X1,Y1}]} | OPaths]).
    


emf_cmd_polygon(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, << NumSeg:32/?SINT>>} = file:read(Fp2, 4),
    emf_cmd_polygon_nextseg(Fp2, NumSeg, []).
emf_cmd_polygon_nextseg(_, 0, Coords) -> {set, emf_close_poly(lists:reverse(Coords))};
emf_cmd_polygon_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT>>} = file:read(Fp2, 8),
    emf_cmd_polygon_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).

emf_cmd_polygon16(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% Number of segments
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polygon16_nextseg(Fp2, NumSeg, []).
emf_cmd_polygon16_nextseg(_, 0, Coords) -> {set, emf_close_poly(lists:reverse(Coords))};
emf_cmd_polygon16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polygon16_nextseg(Fp2, NumSeg-1, [{X1,Y1}|Coords]).


emf_cmd_polypolygon(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% First: Number of polygons
    {ok, <<NumPolys:32/?UINT>>} = file:read(Fp2, 4),
    {ok, <<_NumSegTotal:32/?UINT>>} = file:read(Fp2, 4),
    %% Second: Number of points per polygon
    NumSegList = emf_cmd_polypolygon_lengths(Fp2, NumPolys, []),
    %% Third: Points for each polygon, starting with the first
    {path, lists:map(
        fun (NumSeg) ->
            emf_close_poly(emf_cmd_polypolygon_nextseg(Fp2, NumSeg, []))
        end, NumSegList)}.
emf_cmd_polypolygon_lengths(_, 0, Lengths) -> lists:reverse(Lengths);
emf_cmd_polypolygon_lengths(Fp2, NumPolys, Lengths) ->
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polypolygon_lengths(Fp2, NumPolys-1, [NumSeg|Lengths]).
emf_cmd_polypolygon_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polypolygon_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polypolygon_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).



emf_cmd_polypolygon16(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% First: Number of polygons
    {ok, <<NumPolys:32/?UINT>>} = file:read(Fp2, 4),
    {ok, <<_NumSegTotal:32/?UINT>>} = file:read(Fp2, 4),
    %% Second: Number of points per polygon
    NumSegList = emf_cmd_polypolygon16_lengths(Fp2, NumPolys, []),
    %% Third: Points for each polygon, starting with the first
    {path, lists:map(
        fun (NumSeg) ->
            emf_close_poly(emf_cmd_polypolygon16_nextseg(Fp2, NumSeg, []))
        end, NumSegList)}.
emf_cmd_polypolygon16_lengths(_, 0, Lengths) -> lists:reverse(Lengths);
emf_cmd_polypolygon16_lengths(Fp2, NumPolys, Lengths) ->
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polypolygon16_lengths(Fp2, NumPolys-1, [NumSeg|Lengths]).
emf_cmd_polypolygon16_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polypolygon16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polypolygon16_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).


emf_cmd_polypolyline(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% First: Number of polygons
    {ok, <<NumPolys:32/?UINT>>} = file:read(Fp2, 4),
    {ok, <<_NumSegTotal:32/?UINT>>} = file:read(Fp2, 4),
    %% Second: Number of points per polygon
    NumSegList = emf_cmd_polypolyline_lengths(Fp2, NumPolys, []),
    %% Third: Points for each polygon, starting with the first
    {path, lists:map(
        fun (NumSeg) ->
            emf_close_poly(emf_cmd_polypolyline_nextseg(Fp2, NumSeg, []))
        end, NumSegList)}.
emf_cmd_polypolyline_lengths(_, 0, Lengths) -> lists:reverse(Lengths);
emf_cmd_polypolyline_lengths(Fp2, NumPolys, Lengths) ->
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polypolyline_lengths(Fp2, NumPolys-1, [NumSeg|Lengths]).
emf_cmd_polypolyline_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polypolyline_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:32/?SINT,
            Y1:32/?SINT >>} = file:read(Fp2, 8),
    emf_cmd_polypolyline_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).


emf_cmd_polypolyline16(Fp2) ->
    %% Bounding rectangle (can ignore)
    read_emf_bbox(Fp2),
    %% First: Number of polygons
    {ok, <<NumPolys:32/?UINT>>} = file:read(Fp2, 4),
    {ok, <<_NumSegTotal:32/?UINT>>} = file:read(Fp2, 4),
    %% Second: Number of points per polygon
    NumSegList = emf_cmd_polypolyline16_lengths(Fp2, NumPolys, []),
    %% Third: Points for each polygon, starting with the first
    {path, lists:map(
        fun (NumSeg) ->
            emf_close_poly(emf_cmd_polypolyline16_nextseg(Fp2, NumSeg, []))
        end, NumSegList)}.
emf_cmd_polypolyline16_lengths(_, 0, Lengths) -> lists:reverse(Lengths);
emf_cmd_polypolyline16_lengths(Fp2, NumPolys, Lengths) ->
    {ok, <<NumSeg:32/?UINT>>} = file:read(Fp2, 4),
    emf_cmd_polypolyline16_lengths(Fp2, NumPolys-1, [NumSeg|Lengths]).
emf_cmd_polypolyline16_nextseg(_, 0, Coords) -> lists:reverse(Coords);
emf_cmd_polypolyline16_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    emf_cmd_polypolyline16_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).


%% emf_paths_from_bezier/1

emf_paths_from_bezier(Path, NumDiv) ->
    emf_paths_from_bezier(Path, NumDiv, []).
emf_paths_from_bezier([], _NumDiv, OPath) ->
    lists:reverse(OPath);
emf_paths_from_bezier([_], _NumDiv, OPath) ->
    lists:reverse(OPath);
emf_paths_from_bezier([XY1,XY4], _NumDiv, OPath) ->
    %% This isn't supposed to happen, but we'll draw a straight line
    lists:reverse([XY4,XY1] ++ OPath);
emf_paths_from_bezier([XY1,_XY2,XY4], _NumDiv, OPath) ->
    %% This isn't supposed to happen, but we'll draw a straight line
    lists:reverse([XY4,XY1] ++ OPath);
emf_paths_from_bezier([XY1,XY2,XY3,XY4|Path_0], NumDiv, OPath) ->
    OPath_2 = paths_bezier_4_points(XY1, XY2, XY3, XY4, NumDiv),
    emf_paths_from_bezier([XY4|Path_0], NumDiv, OPath_2 ++ OPath).

paths_bezier_4_points({X1,Y1}, {XC1,YC1}, {XC2,YC2}, {X2,Y2}, NumDiv) ->
    paths_bezier_4_points({X1,Y1}, {XC1,YC1},
                          {XC2,YC2}, {X2,Y2}, [], 0, round(math:pow(2, NumDiv))).
paths_bezier_4_points({X1,Y1}=XY1, {XC1,YC1}=XY2, {XC2,YC2}=XY3, {X2,Y2}=XY4, Path, I, End)
  when I >= 0 andalso I =< End ->
    W = I / float(End),
    WC3 = (1.0 - W) * (1.0 - W) * (1.0 - W),
    WC2 = (1.0 - W) * (1.0 - W),
    WC1 = (1.0 - W),
    ZX = (WC3 * X1) + (3.0 * WC2 * W * XC1) + (3.0 * WC1 * W * W * XC2) + (W * W * W * X2),
    ZY = (WC3 * Y1) + (3.0 * WC2 * W * YC1) + (3.0 * WC1 * W * W * YC2) + (W * W * W * Y2),
    paths_bezier_4_points(XY1, XY2, XY3, XY4, [{ZX,ZY}|Path], I+1, End);
paths_bezier_4_points(_, _, _, _, Path, _, _) -> Path.


emf_into_paths(Commands) when is_list(Commands) -> emf_into_paths(Commands, [], []).
emf_into_paths([], OCommands, []) -> lists:reverse(OCommands);
emf_into_paths([], OCommands, RelPaths) -> emf_into_paths([], [{path, [RelPaths]}|OCommands], []);
emf_into_paths([Ignored|Commands], OCommands, RelPaths) 
  when Ignored =:= todo; Ignored =:= unused; Ignored =:= endpath ->
        emf_into_paths(Commands, OCommands, RelPaths);
emf_into_paths([A|Commands], OCommands, []) ->
    case A of
        beginpath ->
            %% Begin an actual path
            {ok, Commands_1, OCommands2} = emf_into_paths_inpath(Commands),
            emf_into_paths(Commands_1, OCommands2 ++ OCommands, []);
        {linecontinue, List} ->
            emf_into_paths(Commands, OCommands, List);
        {linestart, List} ->
            emf_into_paths(Commands, OCommands, List);
        
        %% Other drawing commands that are self contained
        {path, _} ->
            emf_into_paths(Commands, [A | OCommands], []);
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
               Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath;
               Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            emf_into_paths(Commands, [A | OCommands], []);
        {set, Path} when length(Path) > 0 ->
            emf_into_paths(Commands, [{path, [emf_close_poly(Path)]} | OCommands], []);
        {set, []} ->
            emf_into_paths(Commands, OCommands, [])
    end;
emf_into_paths([A|Commands], OCommands, RelPaths) when length(RelPaths) > 0 ->
    case A of
        beginpath ->
            %% Begin an actual path
            {ok, Commands_1, OCommands2} = emf_into_paths_inpath(Commands),
            emf_into_paths(Commands_1, OCommands2 ++ OCommands, RelPaths);
        {linecontinue, List} ->
            emf_into_paths(Commands, OCommands, RelPaths ++ List);
        {linestart, List} ->
            emf_into_paths(Commands, [{path, [emf_close_poly(RelPaths)]}|OCommands], List);
        
        %% Other drawing commands that are self contained
        {path, _} ->
            emf_into_paths(Commands, [A | OCommands], RelPaths);
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
               Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath;
               Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            emf_into_paths(Commands, [A | OCommands], RelPaths);
        {set, Path} when length(Path) > 0 ->
            emf_into_paths(Commands, [{path, [emf_close_poly(Path)]},
                                      {path, [emf_close_poly(RelPaths)]}|OCommands], []);
        {set, []} ->
            emf_into_paths(Commands, OCommands, RelPaths)
    end.

emf_into_paths_inpath(Commands) ->
    emf_into_paths_inpath(Commands, [], [], []).

%% OCommands - commands that showed up inside beginpath ... endpath but are full
%%             commands that aren't part of the current path.
%% RelPaths  - current list of coordinates
%% PathList  - There can be more than one closing polygon in beginpath ... endpath.
emf_into_paths_inpath([Ignored|Commands], OCommands, RelPaths, PathList) 
  when Ignored =:= todo; Ignored =:= unused; Ignored =:= beginpath ->
        emf_into_paths_inpath(Commands, OCommands, RelPaths, PathList);
emf_into_paths_inpath([endpath|Commands], OCommands, [], []) ->
    {ok, Commands, OCommands};
emf_into_paths_inpath([A|Commands], OCommands, [], PathList) ->
    case A of
        endpath ->
            {ok, Commands, [{path, lists:reverse(PathList)} | OCommands]};
        
        {linecontinue, List} ->
            emf_into_paths_inpath(Commands, OCommands, List, PathList);
        {linestart, List} ->
            emf_into_paths_inpath(Commands, OCommands, List, PathList);
        
        %% Other drawing commands not part of path.
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
               Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath;
               Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            emf_into_paths_inpath(Commands, [A | OCommands], [], PathList);
        {path, _} ->
            emf_into_paths_inpath(Commands, [A | OCommands], [], PathList);
        {set, Path} when length(Path) > 0 ->
            emf_into_paths_inpath(Commands, [{path, [Path]} | OCommands], [], PathList);
        {set, []} ->
            emf_into_paths_inpath(Commands, OCommands, [], PathList)
    end;
emf_into_paths_inpath([A|Commands], OCommands, RelPaths, PathList)
  when length(RelPaths) > 0 ->
    case A of
        endpath ->
            {ok, Commands, [{path, lists:reverse([RelPaths | PathList])} | OCommands]};
        
        {linecontinue, List} ->
            emf_into_paths_inpath(Commands, OCommands, RelPaths ++ List, PathList);
        {linestart, List} ->
            emf_into_paths_inpath(Commands, OCommands, List, [RelPaths | PathList]);
        
        %% Other drawing commands not part of path.
        {path, _} ->
            emf_into_paths_inpath(Commands, [A | OCommands], RelPaths, PathList);
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
            Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath;
            Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            emf_into_paths_inpath(Commands, [A | OCommands], RelPaths, PathList);
        {set, Path} when length(Path) > 0 ->
            emf_into_paths_inpath(Commands, [{path, [Path]}|OCommands], RelPaths, PathList);
        {set, []} ->
            emf_into_paths_inpath(Commands, OCommands, RelPaths, PathList)
    end.


emf_cedge_realize(Commands) ->
    emf_cedge_realize(#rpaths{}, Commands, []).
emf_cedge_realize(#rpaths{path_list=PathList,color_list=ColorList,mat_list=MatL}=_OStt, [], _) ->
    {ok, lists:reverse(PathList),
        lists:reverse(ColorList),
        lists:reverse(MatL)};
emf_cedge_realize(#rpaths{current_color=CurColor,current_tex=CurTex,
    color_list=ColList,mat_list=MatList,path_list=PList,
    world_transform=WT}=OStt, [A|Commands], ColorList) ->
    case A of
        {path, List} when length(List) > 0 ->
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                color_list=[ CurColor | ColList ],
                mat_list=[ CurTex | MatList],
                path_list=[
                    [ paths_to_cedge_list(SubPath) || SubPath <- List]
                | PList]
            };
        {world_transform, {Mtx, _}} ->
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                world_transform = Mtx
            };
        {tex_bitmap, {Coords, TexBitmap}} ->
            {EM11, _,  _, EM22} = WT,
            {{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}} = Coords,
            
            Coords_1 = {
                {float(X1),-float(Y1)},
                {float(X1+(EM11*X2)),-float(Y1+(EM22*Y2))},
                {float(X3),-float(Y3)},
                {float(X4),-float(Y4)}},
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                current_tex = {Coords_1, TexBitmap}
            };
        {alloc_color, {Index, Color}} ->
            ColorList1 = [{Index, Color} | ColorList],
            OStt_1 = OStt;
        {select_object, ObjNumber} ->
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                current_color =
                    case proplists:get_value(ObjNumber, ColorList, ignore) of
                        ignore  -> CurColor;
                        {R,G,B} -> {R / 255.0, G / 255.0, B / 255.0}
                    end
            };
        _ ->
            ColorList1 = ColorList,
            OStt_1 = OStt
    end,
    emf_cedge_realize(OStt_1, Commands, ColorList1).

emf_close_poly(List) when length(List) < 2 -> List;
emf_close_poly([FCoord | _]=List) ->
    [LCoord | _] = lists:reverse(List),
    case LCoord =:= FCoord of
        true -> List;
        false -> [LCoord | List]
    end.
    
emf_cmd_setdibits(Fp2, NumBytes_0) ->
    %% Command
    BeforeBitmapLen = (18 + 2 + 7)*4,
    {ok, <<
        _BoundLeft:32/?UINT,
        _BoundTop:32/?UINT,
        _BoundRight:32/?UINT,
        _BoundBottom:32/?UINT,
        
        DestX:32/?SINT,
        DestY:32/?SINT,
        SrcX:32/?SINT,
        SrcY:32/?SINT,
        SrcWidth:32/?SINT,
        SrcHeight:32/?SINT,
        
        _OffsetBM:32/?UINT,
        _ByteCountBH:32/?UINT,
        _OffsetBits:32/?UINT,
        _ByteCountSize:32/?UINT,
        IUsage:32/?UINT, %% Should be 00 00 00 00
        
        _IStartScans:32/?UINT, %% Usually 20 00 CC 00
        _CountScans:32/?SINT,
        
        SizeOfHeader:32/?UINT, %% Size of header (=40)
        BitmapWidth:32/?UINT,
        BitmapHeight:32/?UINT,
        NumberOfPlanes:16/?UINT, % Always 1
        BitsPerPixel:16/?UINT, %% Must be 32
        CompressionMethod:32/?UINT,
        _SizeOfImage:32/?UINT,
        _HPixelPerMetre:32/?UINT,
        _VPixelPerMetre:32/?UINT,
        _NumberOfColors:32/?UINT,
        _BMPIgnore:32/?UINT
        >>} = file:read(Fp2, BeforeBitmapLen),
    NumBytes = NumBytes_0 - BeforeBitmapLen,
    case {IUsage, SizeOfHeader, NumberOfPlanes, BitsPerPixel, CompressionMethod} of
        {0, 40, 1, 32, 0} ->
            DestWidth = BitmapWidth,
            DestHeight = BitmapHeight,
            {ok, Blob} = file:read(Fp2, NumBytes),
            TxWhere = {{DestX, DestY},
                       {DestWidth, DestHeight},
                       {SrcX, SrcY},
                       {SrcWidth, SrcHeight}},
            {tex_bitmap, {TxWhere, {BitmapWidth, BitmapHeight, Blob}}};
        _ ->
            %% Skip
            {ok, _} = file:read(Fp2, NumBytes),
            unused
    end.
    
emf_cmd_stretchdibits(Fp2, NumBytes_0) ->
    %% Command
    BeforeBitmapLen = (19 + 2 + 7)*4,
    {ok, <<
        _BoundLeft:32/?UINT,
        _BoundTop:32/?UINT,
        _BoundRight:32/?UINT,
        _BoundBottom:32/?UINT,
        
        DestX:32/?SINT,
        DestY:32/?SINT,
        SrcX:32/?SINT,
        SrcY:32/?SINT,
        SrcWidth:32/?SINT,
        SrcHeight:32/?SINT,
        
        _OffsetBM:32/?UINT,
        _ByteCountBH:32/?UINT,
        _OffsetBits:32/?UINT,
        _ByteCountSize:32/?UINT,
        IUsage:32/?UINT, %% Should be 00 00 00 00
        _ROP:32/?UINT, %% Usually 20 00 CC 00
        DestWidth_0:32/?SINT,
        DestHeight_0:32/?SINT,
        
        SizeOfHeader:32/?UINT, %% Size of header (=40)
        BitmapWidth:32/?UINT,
        BitmapHeight:32/?UINT,
        NumberOfPlanes:16/?UINT, % Always 1
        BitsPerPixel:16/?UINT, %% Must be 32
        CompressionMethod:32/?UINT,
        _SizeOfImage:32/?UINT,
        _HPixelPerMetre:32/?UINT,
        _VPixelPerMetre:32/?UINT,
        _NumberOfColors:32/?UINT,
        _BMPIgnore:32/?UINT
        >>} = file:read(Fp2, BeforeBitmapLen),
    NumBytes = NumBytes_0 - BeforeBitmapLen,
    case {IUsage, SizeOfHeader, NumberOfPlanes, BitsPerPixel, CompressionMethod} of
        {0, 40, 1, 32, 0} ->
            DestWidth = DestWidth_0,
            DestHeight = DestHeight_0,
            {ok, Blob} = file:read(Fp2, NumBytes),
            TxWhere = {{DestX, DestY},
                       {DestWidth, DestHeight},
                       {SrcX, SrcY},
                       {SrcWidth, SrcHeight}},
            {tex_bitmap, {TxWhere, {BitmapWidth, BitmapHeight, Blob}}};
        _ ->
            %% Skip
            {ok, _} = file:read(Fp2, NumBytes),
            unused
    end.

%%
%% WMF Specific
%%

-define(W_ARC, 16#0817).
-define(W_ELLIPSE, 16#0418).
-define(W_LINETO, 16#0213).
-define(W_MOVETO, 16#0214).
-define(W_PIE, 16#081A).
-define(W_RECTANGLE, 16#041B).
-define(W_ROUNDRECT, 16#061C).
-define(W_POLYGON, 16#0324).
-define(W_POLYPOLYGON, 16#0538).
-define(W_POLYLINE, 16#0325).
-define(W_CHORD, 16#0830).

%% Not implemented
-define(W_DRAWTEXT, 16#062F).
-define(W_EXTTEXTOUT, 16#0A32).
-define(W_TEXTOUT, 16#0521).
-define(W_SETTEXTCHAREXTRA, 16#0108).
-define(W_SETTEXTCOLOR, 16#0209).
-define(W_SETTEXTJUSTIFICATION, 16#020A).

%% Sets brush and pen state
-define(W_INTERSECTCLIPRECT, 16#0416).
-define(W_RESTOREDC, 16#0127).
-define(W_SAVEDC, 16#001E).
-define(W_SELECTOBJECT, 16#012D).
-define(W_SETTEXTALIGN, 16#012E).
-define(W_SETBKCOLOR, 16#0201).
-define(W_SETBKMODE, 16#0102).
-define(W_SETMAPMODE, 16#0103).
-define(W_SETPOLYFILLMODE, 16#0106).
-define(W_SETROP2, 16#0104).
-define(W_SETWINDOWEXT, 16#020C).
-define(W_SETWINDOWORG, 16#020B).
-define(W_SETVIEWPORTEXT, 16#020E).
-define(W_SETVIEWPORTORG, 16#020D).
-define(W_CREATEBRUSHINDIRECT, 16#02FC).
-define(W_CREATEFONTINDIRECT, 16#02FB).
-define(W_CREATEPENINDIRECT, 16#02FA).
-define(W_DELETEOBJECT, 16#01F0).
-define(W_ESCAPE, 16#0626).
-define(W_SETRELABS, 16#0105).       %% Not implemented
-define(W_CREATEBRUSH, 16#00F8).

%% Palette related - Not implemented
-define(W_REALIZEPALETTE, 16#0035).
-define(W_SELECTPALETTE, 16#0234).
-define(W_CREATEPALETTE, 16#00F7).

%% Unimplemented
-define(W_ABORTDOC, 16#0052).
-define(W_ENDDOC, 16#005E).
-define(W_ENDPAGE, 16#0050).
-define(W_EXCLUDECLIPRECT, 16#0415).
-define(W_EXTFLOODFILL, 16#0548).
-define(W_FILLREGION, 16#0228).
-define(W_FLOODFILL, 16#0419).
-define(W_FRAMEREGION, 16#0429).
-define(W_INVERTREGION, 16#012A).
-define(W_OFFSETCLIPRGN, 16#0220).
-define(W_OFFSETVIEWPORTORG, 16#0211).
-define(W_OFFSETWINDOWORG, 16#020F).
-define(W_PAINTREGION, 16#012B).
-define(W_RESETDC, 16#014C).
-define(W_RESIZEPALETTE, 16#0139).
-define(W_SCALEVIEWPORTEXT, 16#0412).
-define(W_SCALEWINDOWEXT, 16#0410).
-define(W_SELECTCLIPREGION, 16#012C).
-define(W_SETMAPPERFLAGS, 16#0231).
-define(W_SETPALENTRIES, 16#0037).
-define(W_SETPIXEL, 16#041F).
-define(W_STARTDOC, 16#014D).
-define(W_STARTPAGE, 16#004F).
-define(W_ANIMATEPALETTE, 16#0436).
-define(W_CREATEREGION, 16#06FF).

%% Unimplemented because bitmap related
-define(W_CREATEPATTERNBRUSH, 16#01F9).
-define(W_SETSTRETCHBLTMODE, 16#0107).
-define(W_PATBLT, 16#061D).
-define(W_CREATEBITMAP, 16#06FE).
-define(W_CREATEBITMAPINDIRECT, 16#02FD).
-define(W_SETDIBTODEV, 16#0D33).
-define(W_DIBBITBLT, 16#0940).
-define(W_DIBCREATEPATTERNBRUSH, 16#0142).
-define(W_DIBSTRETCHBLT, 16#0B41).
-define(W_BITBLT, 16#0922).
-define(W_STRETCHBLT, 16#0B23).
-define(W_STRETCHDIBITS, 16#0F43).

read_wmf_file(FileName) ->
    
    {ok, Fp2} = file:open(FileName, [read, binary]),

    %% header
    {ok, << SI1:8/?UINT,
            _SI2_0:8/?UINT>>} = file:read(Fp2, 2),
    case SI1 of
        16#D7 -> %% followed by 16#CD
            %% header
            {ok, << _Key:16/?UINT,    %% Other 16 bits of Magic number
                    _Handle:16/?UINT,
                    _Left:16/?SINT,
                    _Top:16/?SINT,
                    _Right:16/?SINT,
                    _Bottom:16/?SINT,
                    Inch:16/?SINT,
                    _Reserved:32/?UINT,
                    _Checksum:16/?UINT>>} = file:read(Fp2, 22 - 2),
            
            {ok, << _SI1_1:8/?UINT,
                    _SI2_1:8/?UINT >>} = file:read(Fp2, 2),
            
            %% Number of twips per inch (same as DPI)
            DPI = float(Inch),
            ok;
        _ ->
            %% Default is 1440 twips per inch
            DPI = 1440.0,
            ok
    end,
    
    %% Type of metafile (0=memory, 1=disk) in SI2
    {ok, << HeaderSize:16/?UINT,      %% Size of header in WORDs (always 9)
            _Version:16/?UINT,
            _FileSize:32/?UINT,
            NumObj:16/?UINT,          %% Number of objects in the file
            MaxRecordSize:32/?UINT,   %% The size of largest record in WORDs, we
                                      %% can use this to detect unexpected sizes.
            _Unused:16/?UINT>>}
            = file:read(Fp2, 2+2+4+2+4+2),
    case HeaderSize of
        9 ->
            %% Likely a windows metafile
            CommandList = wmf_loop(Fp2, NumObj, MaxRecordSize + 4, []),
            {ok, CommandList, DPI};
        _ ->
            erlang:error(unexpected_header)
    end.
    
wmf_loop(Fp2, NumObj, MaxRecordSize, Commands) ->
    case file:read(Fp2, 4) of
        {ok, <<NumParams0:32/?UINT>>} when NumParams0 =< MaxRecordSize -> 
            NumParams = NumParams0 - 3,
            case file:read(Fp2, 2) of
                {ok, <<Command_0:16/?SINT>>} -> Command = Command_0;
                {ok, <<0>>} -> Command = 0;
                eof         -> Command = 0
            end;
        {ok, <<HugeSize:32/?UINT>>} ->
            io:format("~p: " ++
                ?__(1,"ERROR: HugeSize: found ~w MaxRecordSize=~w") ++ "\n",
                [?MODULE, HugeSize, MaxRecordSize]),
            erlang:error(huge_param), NumParams = 0, Command = 0;
        {ok, <<0,0,0>>} -> NumParams = 0, Command = 0;
        {ok, <<0,0>>}   -> NumParams = 0, Command = 0;
        {ok, <<0>>}     -> NumParams = 0, Command = 0;
        eof             -> NumParams = 0, Command = 0
    end,
    case Command of
        0 ->
            file:close(Fp2),
            lists:reverse(Commands);
        _ ->
            NewCommand = case Command of
                ?W_POLYGON               -> wmf_cmd_polyline(Fp2);
                ?W_POLYLINE              -> wmf_cmd_polyline(Fp2);
                ?W_POLYPOLYGON           -> wmf_cmd_polypolygon(Fp2);
                ?W_ARC                   -> wmf_cmd_arc(Fp2);
                ?W_ELLIPSE               -> wmf_cmd_ellipse(Fp2);
                ?W_RECTANGLE             -> wmf_cmd_rectangle(Fp2);
                ?W_ROUNDRECT             -> wmf_cmd_roundrect(Fp2);
                ?W_PIE                   -> wmf_cmd_pie(Fp2);
                ?W_MOVETO                -> wmf_cmd_moveto(Fp2);
                ?W_LINETO                -> wmf_cmd_lineto(Fp2);
                ?W_CHORD                 -> wmf_cmd_chord(Fp2);
                
                ?W_SETPOLYFILLMODE       -> wmf_cmd_setpolyfillmode(Fp2, NumParams);
                ?W_SAVEDC                -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETBKCOLOR            -> wmf_cmd_skip(Fp2, NumParams);
                ?W_INTERSECTCLIPRECT     -> wmf_cmd_skip(Fp2, NumParams);
                ?W_RESTOREDC             -> wmf_cmd_skip(Fp2, NumParams);
                ?W_CREATEBRUSHINDIRECT   -> wmf_cmd_createbrushindirect(Fp2, NumParams);
                ?W_SETBKMODE             -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETWINDOWEXT          -> wmf_cmd_setwindowext(Fp2, NumParams);
                ?W_SETWINDOWORG          -> wmf_cmd_setwindoworg(Fp2, NumParams);
                ?W_SETVIEWPORTEXT        -> wmf_cmd_setviewportext(Fp2, NumParams);
                ?W_SETVIEWPORTORG        -> wmf_cmd_setviewportorg(Fp2, NumParams);
                ?W_SETMAPMODE            -> wmf_cmd_setmapmode(Fp2, NumParams);
                ?W_CREATEPENINDIRECT     -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_SELECTOBJECT          -> wmf_cmd_selectobject(Fp2, NumParams);
                ?W_DELETEOBJECT          -> wmf_cmd_deleteobject(Fp2, NumParams);
                ?W_SETRELABS             -> wmf_cmd_skip(Fp2, NumParams);
                
                ?W_CREATEPALETTE         -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SELECTPALETTE         -> wmf_cmd_skip(Fp2, NumParams);
                ?W_REALIZEPALETTE        -> wmf_cmd_skip(Fp2, NumParams); %% 0 params
                
                %% Text related
                ?W_CREATEFONTINDIRECT    -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_TEXTOUT               -> wmf_cmd_skip(Fp2, NumParams);
                ?W_EXTTEXTOUT            -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETTEXTJUSTIFICATION  -> wmf_cmd_skip(Fp2, NumParams); %% 2 params
                ?W_SETTEXTCOLOR          -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETTEXTALIGN          -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETTEXTCHAREXTRA      -> wmf_cmd_skip(Fp2, NumParams);
                
                %% Ignored for vector paths
                ?W_EXCLUDECLIPRECT       -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETPIXEL              -> wmf_cmd_skip(Fp2, NumParams);
                ?W_SETROP2               -> wmf_cmd_skip(Fp2, NumParams);
                
                %% Ignored by design
                ?W_ESCAPE                -> wmf_cmd_skip(Fp2, NumParams);
                
                %% For texture
                ?W_STRETCHDIBITS         -> wmf_cmd_stretchdibits(Fp2, NumParams);

                %% Unimplemented bitmap related
                ?W_CREATEPATTERNBRUSH    -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_SETSTRETCHBLTMODE     -> wmf_cmd_skip(Fp2, NumParams);
                ?W_PATBLT                -> wmf_cmd_skip(Fp2, NumParams);
                ?W_CREATEBITMAP          -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_CREATEBITMAPINDIRECT  -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_SETDIBTODEV           -> wmf_cmd_skip(Fp2, NumParams);
                ?W_DIBBITBLT             -> wmf_cmd_skip(Fp2, NumParams);
                ?W_DIBCREATEPATTERNBRUSH -> wmf_cmd_createpen(Fp2, NumParams);
                ?W_DIBSTRETCHBLT         -> wmf_cmd_skip(Fp2, NumParams);
                ?W_BITBLT                -> wmf_cmd_skip(Fp2, NumParams);
                ?W_STRETCHBLT            -> wmf_cmd_skip(Fp2, NumParams);
                
                %% Unknown or unimplemented commands
                _ when Command >= 16#1000 -> %% The command number is too high
                    %% Reposition a bit and try again.
                    io:format("~p: " ++
                        ?__(2,"NOTE: repositioning and trying again") ++ "\n",
                        [?MODULE]),
                    file:position(Fp2, {cur, -5}),
                    unused;
                _ ->
                    ?DEBUG_FMT("unk command: ~w size: ~w~n", [Command, NumParams]),
                    wmf_cmd_unknown(Fp2, NumParams)
            end,
            wmf_loop(Fp2, NumObj - 1, MaxRecordSize, [NewCommand|Commands])
    end.
    

wmf_cmd_unknown(_  , 0) -> unused;
wmf_cmd_unknown(Fp2, NumParams) ->
    {ok, <<_Param:16/?SINT>>} = file:read(Fp2, 2),
    wmf_cmd_unknown(Fp2, NumParams-1).

wmf_cmd_skip(_  , 0) -> unused;
wmf_cmd_skip(Fp2, NumParams) ->
    {ok, <<_Param:16/?SINT>>} = file:read(Fp2, 2),
    wmf_cmd_skip(Fp2, NumParams-1).


wmf_cmd_setwindowext(Fp2, 2) ->
    {ok, <<Y1:16/?SINT>>} = file:read(Fp2, 2),
    {ok, <<X1:16/?SINT>>} = file:read(Fp2, 2),
    {window, {extent, {float(X1),float(Y1)}}}.

wmf_cmd_setwindoworg(Fp2, 2) ->
    {ok, <<Y1:16/?SINT>>} = file:read(Fp2, 2),
    {ok, <<X1:16/?SINT>>} = file:read(Fp2, 2),
    {window, {origin, {float(X1),float(Y1)}}}.

wmf_cmd_setviewportext(Fp2, 2) ->
    {ok, <<Y1:16/?SINT>>} = file:read(Fp2, 2),
    {ok, <<X1:16/?SINT>>} = file:read(Fp2, 2),
    {viewport, {extent, {float(X1),float(Y1)}}}.

wmf_cmd_setviewportorg(Fp2, 2) ->
    {ok, <<Y1:16/?SINT>>} = file:read(Fp2, 2),
    {ok, <<X1:16/?SINT>>} = file:read(Fp2, 2),
    {viewport, {origin, {float(X1),float(Y1)}}}.

wmf_cmd_setmapmode(Fp2, 1) ->
    {ok, <<_MapMode:16/?SINT>>} = file:read(Fp2, 2),
    unused.


wmf_cmd_selectobject(Fp2, 1) ->
    {ok, <<Param:16/?SINT>>} = file:read(Fp2, 2),
    {select_object, Param};
wmf_cmd_selectobject(Fp2, NumParams) ->
    wmf_cmd_skip(Fp2, NumParams).


wmf_cmd_deleteobject(Fp2, 1) ->
    {ok, <<Param:16/?SINT>>} = file:read(Fp2, 2),
    {delete_object, Param};
wmf_cmd_deleteobject(Fp2, NumParams) ->
    wmf_cmd_skip(Fp2, NumParams).


wmf_cmd_createbrushindirect(Fp2, 4) ->
    {ok, <<_Param1:16/?SINT,
        Red:8/?UINT,
        Green:8/?UINT,
        Blue:8/?UINT,
        _Unk:8/?UINT,
        _Param4:16/?SINT>>} = file:read(Fp2, 8),
    {alloc_color, {Red, Green, Blue}};
wmf_cmd_createbrushindirect(Fp2, NumParams) ->
    wmf_cmd_skip(Fp2, NumParams).

wmf_cmd_createpen(Fp2, NumParams) ->
    wmf_cmd_skip(Fp2, NumParams),
    {alloc_color, ignore}.


wmf_cmd_polypolygon(Fp2) ->
    %% Not in reverse order.
    %% First: Number of polygons
    {ok, <<NumPolys:16/?UINT>>} = file:read(Fp2, 2),
    %% Second: Number of points per polygon
    NumSegList = wmf_cmd_polypolygon_lengths(Fp2, NumPolys, []),
    %% Third: Points for each polygon, starting with the first
    {path, lists:map(
        fun (NumSeg) ->
            wmf_close_poly(wmf_cmd_polypolygon_nextseg(Fp2, NumSeg, []))
        end, NumSegList)}.
wmf_cmd_polypolygon_lengths(_, 0, Lengths) -> lists:reverse(Lengths);
wmf_cmd_polypolygon_lengths(Fp2, NumPolys, Lengths) ->
    {ok, <<NumSeg:16/?UINT>>} = file:read(Fp2, 2),
    wmf_cmd_polypolygon_lengths(Fp2, NumPolys-1, [NumSeg|Lengths]).
wmf_cmd_polypolygon_nextseg(_, 0, Coords) -> lists:reverse(Coords);
wmf_cmd_polypolygon_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    wmf_cmd_polypolygon_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).
    
wmf_cmd_polyline(Fp2) ->
    {ok, <<NumSeg:16/?UINT>>} = file:read(Fp2, 2),
    wmf_cmd_polyline_nextseg(Fp2, NumSeg, []).
wmf_cmd_polyline_nextseg(_, 0, Coords) ->
    {set, wmf_close_poly(lists:reverse(Coords))};
wmf_cmd_polyline_nextseg(Fp2, NumSeg, Coords) ->
    {ok, << X1:16/?SINT,
            Y1:16/?SINT >>} = file:read(Fp2, 4),
    wmf_cmd_polyline_nextseg(Fp2, NumSeg-1, [{X1, Y1}|Coords]).

wmf_cmd_moveto(Fp2) ->
    {ok, << Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 4),
    {linestart, [{X1, Y1}]}.

wmf_cmd_lineto(Fp2) ->
    {ok, << Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 4),
    {linecontinue, [{X1, Y1}]}.
    

-define(WMF_FILLMODE_WINDING, 2).
-define(WMF_FILLMODE_ALTERNATE, 1).
wmf_cmd_setpolyfillmode(Fp2, Size) ->
    case Size of
        2 ->
            {ok, << Mode:16/?SINT,
                    _Unused1:16/?SINT>>} = file:read(Fp2, 4);
        1 ->
            {ok, << Mode:16/?SINT >>} = file:read(Fp2, 2)
    end,
    case Mode of
        ?WMF_FILLMODE_WINDING -> {fill_mode, winding};
        ?WMF_FILLMODE_ALTERNATE -> {fill_mode, alternate}
    end.
    

wmf_cmd_arc(Fp2) ->
    {ok, << YR2:16/?SINT,
            XR2:16/?SINT,
            YR1:16/?SINT,
            XR1:16/?SINT,
            Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 16),
    wmf_cmd_arc(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2).
wmf_cmd_arc(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when Y2 < Y1 ->
    wmf_cmd_arc(X1,Y2,X2,Y1,XR1,YR1,XR2,YR2);
wmf_cmd_arc(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when X2 < X1 ->
    wmf_cmd_arc(X2,Y1,X1,Y2,XR1,YR1,XR2,YR2);
wmf_cmd_arc(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) ->
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    %% Since all shapes need to be filled, this has to be filled too.
    [C | R] = paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, []),
    {set, [C] ++ R ++ [C]}.

wmf_cmd_ellipse(Fp2) ->
    {ok, << Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 8),
    wmf_cmd_ellipse(X1,Y1,X2,Y2).
wmf_cmd_ellipse(X1,Y1,X2,Y2) when Y2 < Y1 ->
    wmf_cmd_ellipse(X1,Y2,X2,Y1);
wmf_cmd_ellipse(X1,Y1,X2,Y2) when X2 < X1 ->
    wmf_cmd_ellipse(X2,Y1,X1,Y2);
wmf_cmd_ellipse(X1,Y1,X2,Y2) ->
    {set, paths_round_rect(X1,Y1,X2,Y2,(X2-X1)/2.0,(Y2-Y1)/2.0)}.

wmf_cmd_rectangle(Fp2) ->
    {ok, << Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 8),
    wmf_cmd_rectangle(X1,Y1,X2,Y2).
wmf_cmd_rectangle(X1,Y1,X2,Y2) when Y2 < Y1 ->
    wmf_cmd_rectangle(X1,Y2,X2,Y1);
wmf_cmd_rectangle(X1,Y1,X2,Y2) when X2 < X1 ->
    wmf_cmd_rectangle(X2,Y1,X1,Y2);
wmf_cmd_rectangle(X1,Y1,X2,Y2) ->
    {set, [{X1, Y1}, {X2, Y1}, {X2, Y2}, {X1, Y2}, {X1, Y1}]}.

wmf_cmd_roundrect(Fp2) ->
    {ok, << RH:16/?SINT,
            RW:16/?SINT,
            Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 12),
    wmf_cmd_roundrect(X1,Y1,X2,Y2,RH,RW).
wmf_cmd_roundrect(X1,Y1,X2,Y2,RH,RW) when Y2 < Y1 ->
    wmf_cmd_roundrect(X1,Y2,X2,Y1,RH,RW);
wmf_cmd_roundrect(X1,Y1,X2,Y2,RH,RW) when X2 < X1 ->
    wmf_cmd_roundrect(X2,Y1,X1,Y2,RH,RW);
wmf_cmd_roundrect(X1,Y1,X2,Y2,RH,RW) ->
    {set, paths_round_rect(X1,Y1,X2,Y2,RW,RH)}.
    
wmf_cmd_pie(Fp2) ->
    {ok, << YR2:16/?SINT,
            XR2:16/?SINT,
            YR1:16/?SINT,
            XR1:16/?SINT,
            Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 16),
    wmf_cmd_pie(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2).
wmf_cmd_pie(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when Y2 < Y1 ->
    wmf_cmd_pie(X1,Y2,X2,Y1,XR1,YR1,XR2,YR2);
wmf_cmd_pie(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when X2 < X1 ->
    wmf_cmd_pie(X2,Y1,X1,Y2,XR1,YR1,XR2,YR2);
wmf_cmd_pie(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) ->
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    X3 = XC,
    Y3 = YC,
    {set, [{X3,Y3} | paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, [{X3,Y3}]) ]}.
    
wmf_cmd_chord(Fp2) ->
    {ok, << YR2:16/?SINT,
            XR2:16/?SINT,
            YR1:16/?SINT,
            XR1:16/?SINT,
            Y2:16/?SINT,
            X2:16/?SINT,
            Y1:16/?SINT,
            X1:16/?SINT>>} = file:read(Fp2, 16),
    wmf_cmd_chord(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2).
wmf_cmd_chord(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when Y2 < Y1 ->
    wmf_cmd_chord(X1,Y2,X2,Y1,XR1,YR1,XR2,YR2);
wmf_cmd_chord(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) when X2 < X1 ->
    wmf_cmd_chord(X2,Y1,X1,Y2,XR1,YR1,XR2,YR2);
wmf_cmd_chord(X1,Y1,X2,Y2,XR1,YR1,XR2,YR2) ->
    XC = X1 + (X2 - X1) / 2,
    YC = Y1 + (Y2 - Y1) / 2,
    Ang1 = radial_lines_to_angle(XC, YC, XR1, YR1),
    Ang2 = radial_lines_to_angle(XC, YC, XR2, YR2),
    [C | R] = paths_arcto_open(X1, Y1, X2, Y2, Ang1, Ang2, []),
    {set, [C] ++ R ++ [C]}.


wmf_into_paths(Commands) when is_list(Commands) ->
    wmf_into_paths(Commands, [], []).
wmf_into_paths([], OCommands, []) ->
    lists:reverse(OCommands);
wmf_into_paths([], OCommands, RelPaths) ->
    wmf_into_paths([], [{path, [RelPaths]}|OCommands], []);
wmf_into_paths([Ignored|Commands], OCommands, RelPaths)
  when Ignored =:= unused ->
    wmf_into_paths(Commands, OCommands, RelPaths);
wmf_into_paths([A|Commands], OCommands, []) ->
    case A of
        {linecontinue, List} ->
            wmf_into_paths(Commands, OCommands, List);
        {linestart, List} ->
            wmf_into_paths(Commands, OCommands, List);
        
        %% Other drawing commands that are self contained
        {path, _} ->
            wmf_into_paths(Commands, [A | OCommands], []);
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
               Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath; Atm =:= delete_object;
               Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            wmf_into_paths(Commands, [A | OCommands], []);
        {set, Path} when length(Path) > 0 ->
            wmf_into_paths(Commands, [{path, [wmf_close_poly(Path)]} | OCommands], []);
        {set, []} ->
            wmf_into_paths(Commands, OCommands, [])
    end;
wmf_into_paths([A|Commands], OCommands, RelPaths) when length(RelPaths) > 0 ->
    case A of
        {linecontinue, List} ->
            wmf_into_paths(Commands, OCommands, RelPaths ++ List);
        {linestart, List} ->
            wmf_into_paths(Commands, [{path, [wmf_close_poly(RelPaths)]}|OCommands], List);
        
        %% Other drawing commands that are self contained
        {path, _} ->
            wmf_into_paths(Commands, [A | OCommands], RelPaths);
        {Atm, _}
          when Atm =:= fill_mode; Atm =:= world_transform; Atm =:= window; Atm =:= viewport;
               Atm =:= save_dc; Atm =:= restore_dc; Atm =:= selectclippath; Atm =:= delete_object;
               Atm =:= tex_bitmap; Atm =:= alloc_color; Atm =:= select_object ->
            wmf_into_paths(Commands, [A | OCommands], []);
        {set, Path} when length(Path) > 0 ->
            wmf_into_paths(Commands, [{path, [wmf_close_poly(Path)]},
                                      {path, [wmf_close_poly(RelPaths)]}|OCommands], []);
        {set, []} ->
            wmf_into_paths(Commands, OCommands, RelPaths)
    end.

wmf_cedge_realize(Commands) ->
    wmf_cedge_realize(#rpaths{}, Commands, []).
wmf_cedge_realize(#rpaths{path_list=PathList,color_list=ColorList,mat_list=MatL}=_OStt, [], _) ->
    {ok, lists:reverse(PathList),
         lists:reverse(ColorList),
         lists:reverse(MatL)};
wmf_cedge_realize(#rpaths{current_color=CurColor,current_tex=CurTex,
    color_list=ColList,mat_list=MatList,path_list=PList}=OStt,
    [A|Commands], ColorList) ->
    case A of
        {path, List} when length(List) > 0 ->
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                color_list = [ CurColor | ColList ],
                mat_list = [ CurTex | MatList ],
                path_list = [
                    [ paths_to_cedge_list(SubPath) || SubPath <- List]
                | PList]
            };
        {tex_bitmap, {Coords, TexBitmap}} ->
            {{X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}} = Coords,
            Coords_1 = {
                {float(X1),-float(Y1)},
                {float(X2),-float(Y2)},
                {float(X3),-float(Y3)},
                {float(X4),-float(Y4)}},
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{
                current_tex = {Coords_1, TexBitmap}
            };
        {alloc_color, Color} ->
            case lists:any(fun ({_, empty}) -> true; (_) -> false end, ColorList) of
                false ->
                    ColorList1 = [{length(ColorList)+1, Color} | ColorList];
                _ ->
                    ObjNumber = wmf_first_index_available(ColorList),
                    ColorList1 = [ if
                        AC =:= ObjNumber+1 -> {AC, Color};
                        true -> {AC,ARes}
                    end || {AC,ARes} <- ColorList]
            end,
            OStt_1 = OStt;
        {select_object, ObjNumber} ->
            ColorList1 = ColorList,
            OStt_1 = OStt#rpaths{current_color =
                case proplists:get_value(ObjNumber+1, ColorList, ignore) of
                    empty ->
                        %% This shouldn't happen as a well formed wmf will have
                        %% allocated a resource.
                        CurColor;
                    ignore ->
                        CurColor;
                    {R,G,B} ->
                        {R / 255.0,G / 255.0, B / 255.0}
                end
            };
        {delete_object, ObjNumber} ->
            ColorList1 = [if
                AC =:= (ObjNumber+1) -> {AC, empty};
                true -> {AC,ARes}
            end || {AC, ARes} <- ColorList],
            OStt_1 = OStt;
        _ ->
            ColorList1 = ColorList,
            OStt_1 = OStt
    end,
    wmf_cedge_realize(OStt_1, Commands, ColorList1).

wmf_first_index_available(ColorList) ->
    wmf_first_index_available(ColorList, 1).
wmf_first_index_available(ColorList, I) when I =< length(ColorList) ->
    case proplists:get_value(I, ColorList, empty) of
        empty -> I-1;
        _ -> wmf_first_index_available(ColorList, I+1)
    end.

wmf_close_poly(List) when length(List) < 2 -> List;
wmf_close_poly([FCoord | _]=List) ->
    [LCoord | _] = lists:reverse(List),
    case LCoord =:= FCoord of
        true -> List;
        false -> [LCoord | List]
    end.

wmf_cmd_stretchdibits(Fp2, NumParams_0) ->
    BeforeBitmapLen = 14 + 6 + 11,
    {ok, <<
        _ROP:32/?UINT, %% Usually 20 00 CC 00
        IUsage:16/?UINT, %% Should be 00 00
        SrcHeight:16/?SINT,
        SrcWidth:16/?SINT,
        SrcY:16/?SINT,
        SrcX:16/?SINT,
        DestHeight:16/?SINT,
        DestWidth:16/?SINT,
        DestY:16/?SINT,
        DestX:16/?SINT,
        
        SizeOfHeader:32/?UINT, %% Size of header (=40)
        BitmapWidth:32/?UINT,
        BitmapHeight:32/?UINT,
        NumberOfPlanes:16/?UINT, %% Always 1
        BitsPerPixel:16/?UINT,
        CompressionMethod:32/?UINT,
        _UnusedSize:32/?UINT, %% =0
        _HPixelPerMetre:32/?UINT,
        _VPixelPerMetre:32/?UINT,
        _NumberOfColors:32/?UINT,
        _BMPIgnore:32/?UINT
        >>} = file:read(Fp2, BeforeBitmapLen*2),
    NumParams = NumParams_0 - BeforeBitmapLen,
    case {IUsage, SizeOfHeader, NumberOfPlanes, BitsPerPixel, CompressionMethod} of
        {0, 40, 1, 32, 0} ->
            {ok, Blob} = file:read(Fp2, NumParams*2),
            TxWhere = {{DestX, DestY},
                       {DestX+DestWidth, DestY+DestHeight},
                       {SrcX, SrcY},
                       {SrcX+SrcWidth, SrcY+SrcHeight}},
            {tex_bitmap, {TxWhere, {BitmapWidth, BitmapHeight, Blob}}};
        _ ->
            {ok, _} = file:read(Fp2, NumParams*2),
            unused
    end.

%%

paths_to_cedge_list([{X1,Y1}=_C|_]=Path) ->
    paths_to_cedge_list({X1,Y1}, Path).
paths_to_cedge_list({X2,Y2}, [{X1,Y1}])
  when not ((X2 =:= X1) and (Y2 =:= Y1)) ->
    [ #cedge{vs={float(X1),-float(Y1)},cp1=nil,cp2=nil,ve={float(X2),-float(Y2)}} ];
paths_to_cedge_list(_, [_]) -> [];
paths_to_cedge_list(C0, [{X1,Y1},{X2,Y2}=Second|R]) ->
    [ #cedge{vs={float(X1),-float(Y1)},cp1=nil,cp2=nil,ve={float(X2),-float(Y2)}} |
    paths_to_cedge_list(C0, [Second|R]) ].


%%
%% Rounding and Arcs
%%

aroundCos(I, RH, Max) ->
    RH * math:cos(((I / Max) * math:pi() * 0.5)).
aroundSin(I, RH, Max) ->
    RH * math:sin(((I / Max) * math:pi() * 0.5)).
    
stepped_curve(Start, End, Acc, F) ->
    stepped_curve(Start, End, Acc, F, Start).
stepped_curve(Start, End, Acc, F, I)
  when I >= Start andalso I < End ->
    Acc_2 = F(I, Acc),
    stepped_curve(Start, End, Acc_2, F, I+1);
stepped_curve(_, _, Acc, _, _) -> Acc.


%% When paths_round_rect is making a shape with a radius that
%% makes the shape a ellipse (the amount of space between round
%% corners is very close to zero), remove the last point.
%%
paths_round_rect_middle([_|P],X) when X < 0.001 ->
    P;
paths_round_rect_middle(P,_) ->
    P.


paths_round_rect(X1, Y1, X2, Y2, _RW, _RH)
  when X1 =:= X2; Y1 =:= Y2 ->
    [];
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when X1 > X2 ->
    paths_round_rect(X2, Y1, X1, Y2, RW, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when Y1 > Y2 ->
    paths_round_rect(X1, Y2, X2, Y1, RW, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when RW*2.0 > (X2-X1) ->
    paths_round_rect(X1, Y1, X2, Y2, X2-X1, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when RH*2.0 > (Y2-Y1) ->
    paths_round_rect(X1, Y1, X2, Y2, RW, Y2-Y1);
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when X1 < X2, Y1 < Y2 ->
    Max = 10,
    RSW = (X2 - X1) - RW,
    RSH = (Y2 - Y1) - RH,
    Path_0 = [{X1 + RW,Y1}],
    Path_1_1 = stepped_curve(0, Max+1, Path_0, fun(I, Paths) ->
        XA = X1 + RSW + aroundSin(I, RW, Max),
        YA = Y1 + (RH - aroundCos(I, RH, Max)),
        [{XA, YA}|Paths]
    end),
    Path_1 = paths_round_rect_middle(Path_1_1, RSH - RH),
    Path_2_1 = stepped_curve(0, Max+1, Path_1, fun(I, Paths) ->
        XA = X1 + RSW + aroundCos(I, RW, Max),
        YA = Y1 + RSH + aroundSin(I, RH, Max),
        [{XA, YA}|Paths]
    end),
    Path_2 = paths_round_rect_middle(Path_2_1, RSW - RW),
    Path_3_1 = stepped_curve(0, Max+1, Path_2, fun(I, Paths) ->
        XA = X1 + RW - aroundSin(I, RW, Max),
        YA = Y1 + RSH + aroundCos(I, RH, Max),
        [{XA, YA}|Paths]
    end),
    Path_3 = paths_round_rect_middle(Path_3_1, RSH - RH),
    Path_4_1 = stepped_curve(0, Max+1, Path_3, fun(I, Paths) ->
        XA = X1 + RW - aroundSin(Max - I, RW, Max),
        YA = Y1 + RH - aroundCos(Max - I, RH, Max),
        [{XA, YA}|Paths]
    end),
    Path_4 = paths_round_rect_middle(Path_4_1, RSW - RW),
    Path_4.


radial_lines_to_angle(X1, Y1, XR1, YR1) ->
    case float(XR1 - X1) == 0.0 of
        true ->
            case YR1 > Y1 of
                true  -> Ang2 = math:pi() * 1.5;
                false -> Ang2 = math:pi() * 0.5
            end;
        false ->
            RO1 = (YR1 - Y1) / (XR1 - X1),
            A1 = math:atan(RO1),
            case XR1 > X1 of
                true ->
                    case YR1 > Y1 of
                        true ->
                            Ang2 = -A1;
                        false ->
                            Ang2 = math:pi() * 2 - A1
                    end;
                false ->
                    Ang2 = math:pi() - A1
            end
    end,
    Ang2.
    
overAngle(Ang1, Ang2) when Ang2 < Ang1 -> overAngle(Ang1, Ang2 + (2.0 * math:pi()));
overAngle(_   , Ang2) -> Ang2.
paths_arcto_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0, StartList)
  when StartAngle =:= EndAngle_0 ->
    paths_arcto_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0 + math:pi() * 2.0, StartList);
paths_arcto_open(X1, Y1, X2, Y2, StartAngle, EndAngle_0, StartList) ->
    EndAngle = overAngle(StartAngle, EndAngle_0),
    WR = (X2 - X1) / 2.0,
    HR = (Y2 - Y1) / 2.0,
    XC = X1 + WR,
    YC = Y1 + HR,
    AngleDiff = EndAngle - StartAngle,
    NumPoints = round(abs(AngleDiff / math:pi() * 10)),
    paths_arcto_open({XC, YC}, 0, NumPoints, StartAngle, AngleDiff, WR, HR, StartList).
paths_arcto_open({XC, YC}, I, NumPoints, StartAngle, AngleDiff, WR, HR, OPath)
  when I =:= NumPoints ->
    lists:reverse([arcto_cos_sin({XC, YC}, StartAngle+AngleDiff, WR, HR) | OPath]);
paths_arcto_open({XC, YC}=Coord, I, NumPoints, StartAngle, AngleDiff, WR, HR, OPath)
  when I < NumPoints ->
    Ang = StartAngle + AngleDiff * (float(I) / NumPoints),
    OPath_1 = [arcto_cos_sin({XC, YC}, Ang, WR, HR) | OPath],
    paths_arcto_open(Coord, I+1, NumPoints, StartAngle, AngleDiff, WR, HR, OPath_1).
arcto_cos_sin({XC, YC}, Ang, WR, HR) ->
    X1 = XC + math:cos(Ang) * WR,
    Y1 = YC - math:sin(Ang) * HR,
    {X1, Y1}.

%%
%%
%%

    
%% Shape inner exclusion depends on fill mode, in winding mode inner edges are merged,
%% in alternate mode, we need to divide shapes on line intersections.
%%
%% If in alternate mode, line intersections creates distinct mutually exclusive paths,
%% as an example, a 5 point star with overlapping edges will turn into 5 paths.
%%
%% If in winding mode, line intersections are merged if possible. A 5 point star
%% with overlapping edges the inner edges are discarded. Overlapping paths that
%% create the holes are assumed to already be in the opposite direction to the 
%% container path.
%%

exclusion(Commands) ->
    exclusion(Commands, alternate, []).

exclusion([], _FillMode, OCommands) -> lists:reverse(OCommands);
exclusion([{fill_mode, alternate}| Commands], _, OCommands) ->
    ?DEBUG_FMT("now alternate~n",[]),
    exclusion(Commands, alternate, OCommands);
exclusion([{fill_mode, winding}| Commands], _, OCommands) ->
    ?DEBUG_FMT("now winding~n",[]),
    exclusion(Commands, winding, OCommands);
exclusion([{path, _}=Paths|Commands], FillMode, OCommands) ->
    NewPaths = fill_mode_path(Paths, FillMode),
    exclusion(Commands, FillMode, NewPaths ++ OCommands);
exclusion([Cmd|Commands], FillMode, OCommands) ->
    exclusion(Commands, FillMode, [Cmd|OCommands]).
    
coord_same({X1,Y1}, {X2, Y2}) ->
    (round(X1*1000) == round(X2*1000)) andalso
    (round(Y1*1000) == round(Y2*1000)).

fill_mode_path({path, [L|_]=Paths}, alternate) when length(L) > 1 ->
    %% Ignoring other subpaths in alternate if the first path breaks up into
    %% many paths, it gets very confusing.
    [FirstPath | SubPaths] = Paths,
    FirstPath_1 = fill_intersects_and_flip(remove_repeat_coords(FirstPath)),
    case uncouple_shape(remove_repeat_coords(FirstPath_1), alternate) of
        [FirstPath2] ->
            [{path, [FirstPath2 | SubPaths]}];
        ManyPaths ->
            [{path, [Path]} || Path <- ManyPaths]
    end;
fill_mode_path({path, Paths}, winding) ->
    fill_mode_path({path, Paths}, alternate).
    %% The winding code doesn't really work in some tests
    %% so using alternate for everything for now
    

trim_path_coords(Path) ->
    trim_path_coords(Path, []).
trim_path_coords([], OPath) ->
    lists:reverse(OPath);
trim_path_coords([{X1,Y1} | R], OPath) ->
    trim_path_coords(R,
        [{round(X1 * 1000) / 1000.0,
          round(Y1 * 1000) / 1000.0} | OPath]).

remove_repeat_coords(Path) ->
    remove_repeat_coords(trim_path_coords(Path), []).
remove_repeat_coords([Point1], OPath) ->
    lists:reverse([Point1 | OPath]);
remove_repeat_coords([Point1, Point2 | R], OPath) ->
    case coord_same(Point1, Point2) of
    true  ->
        remove_repeat_coords([Point2 | R], OPath);
    false ->
        remove_repeat_coords([Point2 | R], [Point1 | OPath])
    end.


%% Use smallest to get the behaviour of fill mode alternate
uncouple_shape(R, alternate) ->
    case uncouple_shape_1(R, []) of
        [] ->
            %% Simple shape
            [R];
        PossiblePaths ->
            %% A few possible walks, use the smallest total in angles.
            case uncouple_shape_take_min(PossiblePaths, 99999999, false) of
                false ->
                    [];
                {List1, List2} ->
                    [List1 | uncouple_shape(List2, alternate)]
            end
    end.
    
uncouple_shape_take_min([], _, Current) ->
    Current;
uncouple_shape_take_min([{possib, AngleTotal, L1, L2} | R], Min, Current) ->
    case AngleTotal < Min of
    true ->
        uncouple_shape_take_min(R, AngleTotal, {L1, L2});
    false ->
        uncouple_shape_take_min(R, Min, Current)
    end.
    

uncouple_shape_1([_], _) -> [];
uncouple_shape_1([Point1, Point2 | R], PrevPath) ->
    case find_other_point(Point2, R, [Point2]) of
        false ->
            uncouple_shape_1([Point2 | R], [Point1 | PrevPath]);
        {List, [FPointL2|_]=List2} ->
            PrevPath1 = lists:reverse(PrevPath),
            PossibList = PrevPath1 ++ [Point1 | List],
            [{possib, path_angle_total(PossibList),PossibList, List2 ++ [FPointL2]}
            | uncouple_shape_1([Point2 | R], [Point1 | PrevPath])]
    end.
find_other_point(_, [], _) -> false;
find_other_point(Point2, [Point3 | R], Other) ->
    case Point3 =:= Point2 of
        true -> {[Point3 | R], lists:reverse(Other)};
        false -> find_other_point(Point2, R, [Point3 | Other])
    end.

path_angle_total([_]) -> 0.0;
path_angle_total([_, _]) -> 0.0;
path_angle_total([Coord1, Coord2, Coord3 | List]) ->
    Amt = inward_angle_of_line(angle_of_line(Coord1, Coord2), angle_of_line(Coord2, Coord3)),
    Amt + path_angle_total([Coord2, Coord3 | List]).

-spec angle_of_line({float(),float()},{float(),float()}) -> float().
angle_of_line({A1_X,A1_Y}, {A2_X,A2_Y}) ->
    case A1_X == A2_X of
    true ->
        case A2_Y > A1_Y of
        true ->
            Ang2 = 0.5 * math:pi();
        false ->
            Ang2 = 1.5 * math:pi()
        end;
    false ->
        R0 = (A2_Y - A1_Y) / (A2_X - A1_X),
        case abs(R0) == 0.0 of
        true ->
            case A2_X < A1_X of
            true ->
                Ang2 = math:pi();
            false ->
                Ang2 = 0
            end;
        false ->
            Ang = math:atan(R0),
            case A2_X > A1_X of
            true ->
                Ang2 = Ang;
            false ->
                Ang2 = Ang + math:pi()
            end
        end
    end,
    Ang2.

%% How big the inner angle from the right hand side, our path traversal is counter
%% clockwise (grid system where Y is going up)
inward_angle_of_line(OAng, XAng) ->
    RAng = inward_angle_of_line_2(-(XAng - OAng + math:pi())),
    RAng.
inward_angle_of_line_2(RAng) when RAng < 0 ->
    inward_angle_of_line_2(RAng  + 2.0 * math:pi());
inward_angle_of_line_2(RAng) -> RAng.

    
fill_intersects_and_flip(Path) ->
    fill_intersects_and_flip(Path, []).
fill_intersects_and_flip([Point], OPath) ->
    lists:reverse([Point | OPath]);
fill_intersects_and_flip([Point1, Point2 | R], OPath) ->
    %% When we find the intersection, we add a new point in the path, and we have to go further in
    %% the existing path to find the intersection from the other side to add a point there as well.
    %% After finding the two parts of the path that intersects at that pont, we reverse the inner
    %% path between XY3 and XY3
    case intersects_in_path(Point1, Point2, [Point2 | R], []) of
        {P1, P2} ->
            fill_intersects_and_flip(P1 ++ P2, OPath);
        false ->
            fill_intersects_and_flip([Point2 | R], [Point1 | OPath])
    end.


intersects_in_path(_, _, [_Point4], _) -> false;
intersects_in_path(Point1, Point2, [Point3, Point4 | R], InbetweenPath) ->
    InbetweenPath2 = [Point3 | InbetweenPath],
    case line_intersect(Point1, Point2, Point3, Point4) of
        {X1, Y1} ->
            {[Point1, {X1, Y1}], (InbetweenPath2) ++ [{X1, Y1}, Point4 | R]};
        false -> intersects_in_path(Point1, Point2, [Point4 | R], InbetweenPath2)
    end.

%% If any of the lines have points in common they don't intersect
line_intersect(_, L2A1_L1A2_Same, L2A1_L1A2_Same, _) -> false;
line_intersect(L2A1_L1A1_Same, _, L2A1_L1A1_Same, _) -> false;
line_intersect(L2A2_L1A1_Same, _, _, L2A2_L1A1_Same) -> false;
line_intersect(_, L2A2_L1A2_Same, _, L2A2_L1A2_Same) -> false;
%% Check if two lines intersect at a point.
line_intersect(L1A1={L1A1_X,L1A1_Y}, L1A2={L1A2_X,L1A2_Y}, L2A1={L2A1_X,L2A1_Y}, L2A2={L2A2_X,L2A2_Y}) ->
    case (L1A1_Y < L1A2_Y) of
    true ->
        LeastY1 = L1A1_Y,
        MostY1 = L1A2_Y;
    false ->
        LeastY1 = L1A2_Y,
        MostY1 = L1A1_Y
    end,
    case (L2A1_Y < L2A2_Y) of
    true ->
        LeastY2 = L2A1_Y,
        MostY2 = L2A2_Y;
    false ->
        LeastY2 = L2A2_Y,
        MostY2 = L2A1_Y
    end,
    
    case (MostY1 < LeastY2) orelse (MostY2 < LeastY1) of
    true ->
        false;
    false ->
        case (L1A1_X < L1A2_X) of
        true ->
            L1B1 = L1A1,
            L1B2 = L1A2;
        false ->
            L1B1 = L1A2,
            L1B2 = L1A1
        end,
        case (L2A1_X < L2A2_X) of
        true ->
            L2B1 = L2A1,
            L2B2 = L2A2;
        false ->
            L2B1 = L2A2,
            L2B2 = L2A1
        end,
        
        {L1B1_X,L1B1_Y} = L1B1,
        {L1B2_X,L1B2_Y} = L1B2,
        {L2B1_X,L2B1_Y} = L2B1,
        {L2B2_X,L2B2_Y} = L2B2,
        
        case ((L1B2_Y - L1B1_Y) == (L2B2_Y - L2B1_Y)) andalso
             ((L1B2_X - L1B1_X) == (L2B2_X - L2B1_X)) of
        true ->
            false;
        false when abs(L1B2_X - L1B1_X) == 0.0 ->
            case abs(L2B2_X - L2B1_X) == 0.0 of
            true ->
                false;
            false ->
                case (L2B1_X < L1B1_X) andalso (L2B2_X > L1B1_X) of
                true ->
                    B = (L2B2_Y - L2B1_Y) / (L2B2_X - L2B1_X),
                    D = L2B1_Y - (L2B1_X * B),
                    MX = L1B1_X,
                    MY = B * MX + D,
                    line_intersect_y_bound(
                        {MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
                false ->
                    false
                end
            end;
        false when abs(L2B2_X - L2B1_X) == 0.0 ->
            case (L1B1_X < L2B1_X) andalso (L1B2_X > L2B1_X) of
            true ->
                A = (L1B2_Y - L1B1_Y) / (L1B2_X - L1B1_X),
                C = L1B1_Y - (L1B1_X * A),
                MX = L2B1_X,
                MY = A * MX + C,
                line_intersect_y_bound(
                    {MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
            false ->
                false
            end;
        false ->
            A = (L1B2_Y - L1B1_Y) / (L1B2_X - L1B1_X),
            B = (L2B2_Y - L2B1_Y) / (L2B2_X - L2B1_X),
            C = L1B1_Y - (L1B1_X * A),
            D = L2B1_Y - (L2B1_X * B),

            case abs(A - B) == 0.0 of
            true ->
                false;
            false ->
                MX = (D - C) / (A - B),
                MY = A * MX + C,
                
                case MX > L1B1_X andalso MX < L1B2_X andalso
                     MX > L2B1_X andalso MX < L2B2_X of
                true ->
                    line_intersect_y_bound(
                        {MX, MY}, LeastY1, MostY1, LeastY2, MostY2);
                false ->
                    false
                end
            end
        end
    end.
line_intersect_y_bound({_, MY}=Point, LeastY1, MostY1, LeastY2, MostY2)
  when MY >= LeastY2 andalso MY =< MostY2 andalso
       MY >= LeastY1 andalso MY =< MostY1 ->
    Point;
line_intersect_y_bound(_, _, _, _, _) ->
    false.


%%%
%%% Unit conversion
%%%

unit_atom(Unit) when is_atom(Unit) -> Unit;
unit_atom(Unit) when is_list(Unit) ->
    case string:to_lower(Unit) of
        "px" -> px;
        "pt" -> pt;
        "pc" -> pc;
        "mm" -> mm;
        "cm" -> cm;
        "in" -> in;
        "em" -> em;
        "ex" -> ex;
        
        %% Convenience units for larger models
        "dm" -> dm;
        "m" -> meter;
        "ft" -> ft;
        "yd" -> yd;

        _ -> px  %% The default for user unit is pixels (twips?).
    end.

unit_scaled_pt(pt) -> 1.0;
unit_scaled_pt(pc) -> (1.0 / 6.0) * unit_scaled_pt(in);
unit_scaled_pt(mm) -> 0.03937008 * unit_scaled_pt(in);
unit_scaled_pt(cm) -> 10.0   * unit_scaled_pt(mm);
unit_scaled_pt(dm) -> 100.0  * unit_scaled_pt(mm);
unit_scaled_pt(meter)  -> 1000.0 * unit_scaled_pt(mm);
unit_scaled_pt(em) -> 12.0;
unit_scaled_pt(ex) -> 8.0;
unit_scaled_pt(in) -> 72.0;
unit_scaled_pt(ft) -> 12.0   * unit_scaled_pt(in);
unit_scaled_pt(yd) -> 3.0    * unit_scaled_pt(ft).

%% 1.0 if it is the same unit.
unit_ratio(Unit1, Unit2, _DPI)
  when Unit1 =:= Unit2 ->
    1.0;

%% Use pt for the physical units on the document unit side.
unit_ratio(Unit1, Unit2, DPI)
  when Unit2 =/= px, Unit2 =/= pt ->
    unit_ratio(Unit1, pt, DPI) / unit_scaled_pt(Unit2);

%% Physical units on both side
unit_ratio(Unit1, pt, _DPI)
  when Unit1 =/= px ->
    unit_scaled_pt(Unit1);

%% Physical unit document, pixel unit shape
unit_ratio(px, pt, DPI) ->
    unit_scaled_pt(in) / DPI;

%% Pixel unit document, physical unit shape
unit_ratio(Unit1, px, DPI) ->
    (DPI / unit_scaled_pt(in)) * unit_scaled_pt(Unit1).

%% User units
conv_unit({Num, user}, _, _DPI)
  when is_float(Num); is_integer(Num) ->
    Num * 1.0;


conv_unit({Num, Unit}, DocUnit, DPI)
  when is_float(Num), is_atom(Unit), is_atom(DocUnit) ->
    Num * unit_ratio(Unit, DocUnit, DPI);
conv_unit({Num, Unit}, DocUnit, DPI)
  when is_integer(Num) ->
    conv_unit({float(Num), Unit}, DocUnit, DPI).

number_val_unit(Val, "") ->
    Val;
number_val_unit(Val, Unit) ->
    {Val, Unit}.
parse_float_number_w_unit(Num_S, DVal) ->
    case parse_float_number_w_unit_1(Num_S) of
        NotNum when is_list(NotNum) -> {DVal, user};
        {Num_I, Unit} when is_integer(Num_I) -> {Num_I * 1.0, Unit};
        {Num_F, Unit} when is_float(Num_F) -> {Num_F, Unit};
        {Num_I, ""} when is_integer(Num_I) -> {Num_I * 1.0, user};
        {Num_F, ""} when is_float(Num_F) -> {Num_F, user};
        Num_I when is_integer(Num_I) -> {Num_I * 1.0, user};
        Num_F when is_float(Num_F) -> {Num_F, user};
        _ -> {DVal, user}
    end.
parse_float_number_w_unit_1([$.|R]) ->
    parse_float_number_w_unit_1([$0,$.|R]);
parse_float_number_w_unit_1([A0 | _] = Num_S)
  when (A0 >= $0 andalso A0 =< $9) orelse A0 =:= $- ->
    case string:split(Num_S, "e") of
        [LExpNum_S, RExpNum_S] ->
            LExpNum = case string:to_float(LExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(LExpNum_S) of
                        {error, _} -> none;
                        {Num_1, ""} -> Num_1;
                        {_, _} -> none
                    end;
                {Num_1, _} -> Num_1
            end,
            {RExpNum, Unit} = case string:to_float(RExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(RExpNum_S) of
                        {error, _}      -> {none, none};
                        {Num_2, Unit_S} -> {Num_2, Unit_S}
                    end;
                {Num_2, Unit_S} -> {Num_2, Unit_S}
            end,
            case LExpNum =:= none orelse RExpNum =:= none of
                true -> Num_S;
                _ ->
                    Val = LExpNum * math:pow(10, RExpNum),
                    number_val_unit(Val, Unit)
            end;
        [_NoExponent] ->
            case string:to_float(Num_S) of
                {error, no_float} -> 
                    case string:to_integer(Num_S) of
                        {error, _} -> Num_S;
                        {Num_1, Unit_S} ->
                            number_val_unit(Num_1, Unit_S)
                    end;
                {Num_1, Unit_S} ->
                    number_val_unit(Num_1, Unit_S)
            end
    end;
parse_float_number_w_unit_1(NotNumber) ->
    NotNumber.


%%%
%%% User text file transforms parse
%%%

layerdir_3(Name_S, Num) ->
    case string:to_lower(Name_S) of
        "x" -> {x, Num};
        "y" -> {y, Num};
        "z" -> {z, Num};
        "scalex" -> {scalex, Num};
        "scaley" -> {scaley, Num};
        "scalez" -> {scalez, Num};
        "rotx" -> {rotx, Num/180.0*math:pi()};
        "roty" -> {roty, Num/180.0*math:pi()};
        "rotz" -> {rotz, Num/180.0*math:pi()};
        _ ->
            %% Provide the advanced user a way to know that the directive he entered
            %% into the layer name might have a typo.
            io:format("~p: " ++
                ?__(1,"NOTE: Transforms list contains a directive but not known: ") ++
                "~w=~w~n",
                [?MODULE,Name_S,Num]),
            none
    end.
layerdir_2(Name_S, Val_S) ->
    case string:to_float(Val_S) of
        {error, no_float} -> 
            case string:to_integer(Val_S) of
                {error, _} -> none;
                {Num_1, ""} -> layerdir_3(Name_S, float(Num_1));
                {_, _} -> none
            end;
        {Num_1, ""} -> layerdir_3(Name_S, Num_1);
        {_, _} -> none
    end.

layerdir(T) ->
    Toks = string:tokens(T, " "),
    layerdir_1(Toks, [], []).
layerdir_1([A | R], O, OW) ->
    case string:split(A, "=") of
        [Name_S, Val_S] ->
            case layerdir_2(Name_S, Val_S) of
                none -> layerdir_1(R, O, [A | OW]);
                {_Name, _Val}=Tup ->
                    layerdir_1(R, [Tup|O], OW)
            end;
        [_] ->
            layerdir_1(R, O, [A | OW])
    end;
layerdir_1([], O, OW) ->
    %% Provide both the list of transforms and the layer name.
    { lists:reverse(O),
      string:join(lists:reverse(OW), " ") }.


read_txt_transforms_if_any(_File, false) ->
    {ok, []};
read_txt_transforms_if_any(File, true) ->
    case read_txt_transforms_if_any_1(File) of
        none -> {ok, []};
        {ok, List} -> {ok, List}
    end.
read_txt_transforms_if_any_1(File) ->
    TxtFile = filename:rootname(File) ++ ".txt",
    case file:read_file_info(TxtFile) of
        {ok, _} ->
            read_txt_transforms(TxtFile);
        _ ->
            none
    end.
read_txt_transforms(File) ->
    case file:open(File, [read, binary]) of
        {ok, Fp} ->
            Transforms = read_txt_transforms_1(Fp, []),
            file:close(Fp),
            {ok, Transforms};
        _ ->
            none
    end.
read_txt_transforms_1(Fp, O) ->
    case file:read_line(Fp) of
        {ok, Line} ->
            case read_txt_transforms_2(Line) of
                {Spec, Transforms_S} ->
                    {Transforms, _} = layerdir(binary_to_list(Transforms_S)),
                    T = {Spec, Transforms},
                    read_txt_transforms_1(Fp, [T|O]);
                _ ->
                    read_txt_transforms_1(Fp, O)
            end;
        _ ->
            lists:reverse(O)
    end.
read_txt_transforms_2(Line) ->
    case string:split(Line, ":") of
        [Spec_S0, Transforms_S0] ->
            Spec_S = string:trim(Spec_S0),
            Transforms_S = string:trim(Transforms_S0),
            case read_txt_transforms_spec(Spec_S) of
                {ok, Spec} ->
                    {Spec, Transforms_S};
                none ->
                    none
            end;
        _ ->
            none
    end.
read_txt_transforms_spec(Spec_S) ->
    case string:to_integer(Spec_S) of
        {error, _} -> none;
        {Num_1, <<>>} -> {ok, {index, Num_1}};
        _ -> none
    end.
assign_transforms_to_shapes(List, TIn) ->
    assign_transforms_to_shapes(List, TIn, 1, []).
assign_transforms_to_shapes([_|List], TIn, Index, O) ->
    Index_1 = Index+1,
    case proplists:get_value({index, Index}, TIn, none) of
        none ->
            A1 = [],
            assign_transforms_to_shapes(List, TIn, Index_1, [A1|O]);
        Transforms ->
            A1 = Transforms,
            assign_transforms_to_shapes(List, TIn, Index_1, [A1|O])
    end;
assign_transforms_to_shapes([], _TIn, _Index, O) ->
    lists:reverse(O).


%%%
%%%


%% Calculate a scale so the resulting mesh isn't huge.
%%
calculate_rescale_amount(MaxWidth, MaxHeight, Cmds) ->
    calculate_rescale_amount(MaxWidth, MaxHeight, Cmds, 1.0).
calculate_rescale_amount(_, _, [], Rescale) -> Rescale;
calculate_rescale_amount(MaxWidth, MaxHeight, [A|Cmds], Rescale) ->
    case A of
        {path, List} when length(List) > 0 ->
            Rescale_2 = lists:foldl(fun (SubPath, Rescale_1) ->
                fit(MaxWidth, MaxHeight, SubPath, Rescale_1)
            end, Rescale, List);
        _Other ->
            Rescale_2 = Rescale
    end,
    calculate_rescale_amount(MaxWidth, MaxHeight, Cmds, Rescale_2).

fit(MaxWidth, MaxHeight, Coords, LowestRescale) ->
    WidthFound_0 = 1.0,
    HeightFound_0 = 1.0,
    {WidthFound, HeightFound} = fit_size(Coords, WidthFound_0, HeightFound_0),
    Rescale_0 = MaxWidth / max(WidthFound, 1.0),
    Rescale = case MaxHeight < (HeightFound*Rescale_0) of
        true -> MaxHeight / max(HeightFound, 1.0);
        _    -> Rescale_0
    end,
    min(Rescale, LowestRescale).

fit_size([], WidthFound, HeightFound) ->
    {WidthFound, HeightFound};
fit_size([{X,Y}|Coords], WidthFound_0, HeightFound_0) ->
    WidthFound = max(X, WidthFound_0),
    HeightFound = max(Y, HeightFound_0),
    fit_size(Coords, WidthFound, HeightFound).


rescale(Scaler, Paths) -> rescale(Scaler, Paths, []).
rescale(_, [], OPaths) -> lists:reverse(OPaths);
rescale(Scaler, [A|Paths], OPaths) ->
    case A of
        {tex_bitmap, {{DestXY1, DestXY2, SrcXY1, SrcXY2}, M}} ->
            DestXY1_1 = rescale_coord(Scaler, DestXY1),
            DestXY2_1 = rescale_coord(Scaler, DestXY2),
            SrcXY1_1 = rescale_coord(Scaler, SrcXY1),
            SrcXY2_1 = rescale_coord(Scaler, SrcXY2),
            NewPath = {tex_bitmap, {{DestXY1_1, DestXY2_1, SrcXY1_1, SrcXY2_1}, M}};
        {path, List} when length(List) > 0 ->
            NewPath = {path, [[rescale_coord(Scaler, Coord)
                              || Coord <- SubPath] || SubPath <- List]};
        Unk ->
            NewPath = Unk
    end,
    rescale(Scaler, Paths, [NewPath|OPaths]).
rescale_coord({XS,YS}, {X,Y}) ->
    {XS * X, YS * Y}.

%%%
%%%


%% Remove subsequent identical paths.
%%
remove_double_path(PathsList) ->
    remove_double_path(PathsList, none, []).
remove_double_path([{path, CurPath}=Tup|R], PrevPath, O)
  when length(CurPath) =:= length(PrevPath) ->
    case CurPath =:= PrevPath of
        true ->
            remove_double_path(R, PrevPath, O);
        false ->
            remove_double_path(R, CurPath, [Tup|O])
    end;
remove_double_path([{path,PrevPath}=PathItem|R], _, O) ->
    remove_double_path(R, PrevPath, [PathItem|O]);
remove_double_path([Item|R], PrevPath, O) ->
    remove_double_path(R, PrevPath, [Item|O]);
remove_double_path([], _PrevPath, O) ->
    lists:reverse(O).


%%% All of the following functions come from wpc_ps
%%%
process_islands(Plas) ->
    lists:foldr(fun(Pla, Acc) ->
        %% it was noticed during the tests that some files may contain data that causes
        %% wpc_tt crashes with a key_exists error. We ignore that path definition and go on
        try process_islands_1(Pla) of
            {Vs,Fs,He}=Res when is_list(Vs), is_list(Fs), is_list(He) ->
                [Res|Acc]
        catch
            error:{key_exists,_} ->
                %% While the shape is skipped, mention something in the console for
                %% the user so they can find out why a shape is missing.
                io:format(?__(1, "EMF/WMF Import error on skipped shape~n"), []),
                io:format(?__(2,
                    "~p: NOTE: A shape has been skipped due to key_exists error "
                    "in wpc_ai:polyareas_to_faces~n"), [?MODULE]),
                Acc;
            error:Err:StT ->
                %% Something else went wrong, send an error.
                io:format(?__(3, 
                    "~p: ERROR: an error occurred within wpc_ai:polyareas_to_faces: "
                    "~p~nstack trace: ~p~n"), [?MODULE, Err, StT]),
                erlang:error({error, Err})
        end
    end, [], Plas).
process_islands_1(Pla) ->
    try wpc_ai:polyareas_to_faces([Pla]) of
        Res -> Res
    catch
        error:{key_exists,_} ->
            %% Likely a key_exists error may have happened because a point is
            %% too close or even the same coordinate to another adjacent point.
            %% Try to remove them and try again.
            io:format(?__(1, 
                "~p: NOTE: A first key_exists error, trying to fix path "
                "and trying again.~n"), [?MODULE]),
            Pla_1 = remove_repeat_cedge(Pla),
            process_islands_again(Pla_1)
    end.
process_islands_again(Pla) ->
    wpc_ai:polyareas_to_faces([Pla]).


%% Remove #cedge{} if the vs and ve are very similar, as this can
%% cause weird meshes to be created from wpc_ai, as well as cause 
%% also key_exists errors.
%%
-define(SAME_POINT(V1,V2), (round(V1*1.0e3) =:= round(V2*1.0e3))).
remove_repeat_cedge({polyarea,CEdgesC,CEdgesHL}) ->
    {polyarea,
        remove_repeat_cedge(CEdgesC, []),
        [remove_repeat_cedge(CEdges, []) || CEdges <- CEdgesHL]}.
remove_repeat_cedge([#cedge{vs={X1,Y1}=_,cp1=nil,cp2=nil,ve={X2,Y2}=_}=_|L],OL)
  when ?SAME_POINT(X1,X2) andalso ?SAME_POINT(Y1,Y2) ->
    remove_repeat_cedge(L, OL);
remove_repeat_cedge([Edge|L],OL) ->
    remove_repeat_cedge(L, [Edge|OL]);
remove_repeat_cedge([],OL) ->
    lists:reverse(OL).

