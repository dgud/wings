%%
%%  wpc_svg.erl --
%%
%% The importer is limited to paths. This means you should convert objects
%% to paths before saving. So in Inkscape you would select each object and
%% use Path | Object to Path before saving. Also, it is best if you save as a
%% 'plain' svg. The default svg can use the Arcto (A or a) declaration, and
%% currently there is no handling for this directive.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%                2022-2023 Edward Blake (new tokenizer, xmlerl parsing)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_svg_path).
-export([init/0,menu/2,command/2]).

%% Export path parsing and some other things for other plugins to be able
%% to use.
%%
-export([
    read_svg_content/3,
    parse_svg_content/1,
    style_to_tuple/1,
    x11_color_names/1
]).

-import(lists, [foldl/3,reverse/1,splitwith/2,map/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").



-define(
  IS_DIGIT(ADigit),
    ( ADigit >= $0  andalso ADigit =< $9 ) orelse
      ADigit =:= $.  orelse ADigit =:= $- 
  ).
-define(
  IS_HEX_DIGIT(ADigit),
    (ADigit >= $A andalso ADigit =< $F) orelse
    (ADigit >= $a andalso ADigit =< $f) orelse
    (ADigit >= $0 andalso ADigit =< $9)
  ).

-record(cedge,% polyarea and cedge records must match definitions in wpc_ai.erl
    {vs,cp1=nil,cp2=nil,ve}).    %all are {x,y} pairs

-record(path,
    {ops=[],            %list of pathops
     close=false}).        %true or false

-record(pathop,
    {opkind,            %pmoveto, plineto, or pcurveto
     x1=0.0,
     y1=0.0,
     x2=0.0,
     y2=0.0,
     x3=0.0,
     y3=0.0}).

-record(pstate,
    {op,
     curpath=#path{},        %current path
     osubpaths=[],          %subpaths of an object
     objects=[],            %object list (paths)
     objects_tex=[],         %object color and texture
     first_subpath=true   %first subpath of parse
     }).
    
-type rgb() :: {float(), float(), float()}.

-record(style_colors,
    {
    scol :: none | inherit | opaque, %% Line color (either none, inherit or opaque)
    fcol :: rgb() | inherit , %% Fill color (rgba tuple)
    fopa :: float() | inherit %% Opacity
    }).

-type m3x2() :: {matrix, float(), float(), float(), float(), float(), float()}.

-record(image_tag_r,
    {
    clip_path :: string() | none,
    x :: float(),
    y :: float(),
    width :: float(),
    height :: float(),
    transform :: m3x2(),
    id :: string(),
    data
    }).

%% Standard SVG units and some convenience units
-type svg_units() :: 'px'|'pt'|'pc'|'mm'|'cm'|'in'|'em'|'ex'|'perc'|'dm'|'meter'|'ft'.

-record(document_s, {
    fullfilename :: string(),
    units :: svg_units(),
    docsz :: {float(), float()},
    viewbox :: {float(),float(),float(),float()}
}).

-record(coltex, {
    color=none, %% Color
    tex=none,   %% Texture
    ut=[]       %% User vertex Transformations
}).

init() -> true.

menu({file,import}, Menu) ->
    Menu ++ [{"SVG Paths (.svg|.svgz)...", svg, [option]}];
menu(_, Menu) -> Menu.

more_info() ->
    [?__(1,"<b>Automatic center</b>\n"
    "Automatically center the imported shape.\n\n"
    "<b>Scale fraction</b>\n"
    "Set the scale ratio of 1 unit in wings to the units set in the SVG file. "
    "If set to 100pt, then 100pt is the same as 1 unit in wings. Available "
    "units include pt, px, pc, cm, mm, and in. Note that if "
    "the SVG is set in px and the scale is in points (pt) that the assumed "
    "DPI conversion between the physical unit and the virtual unit is "
    "96 pixels per inch which is what Inkscape now uses.\n\n"
    "<b>Use viewbox coordinates</b>\n"
    "Use the viewbox coordinate system directly instead of the document "
    "coordinate system. this means that the unit-based dimensions of the "
    "document is not used for scaling and the scale factor has to be specified.\n\n"
    "<b>Scale factor</b>\n"
    "When in viewbox coordinate system mode, shapes are scaled from the viewbox to "
    "wings using this factor. A shape that is 10 across in the viewbox, with a scale "
    "factor of 0.01 will be 0.1\n\n"
    "<b>Remove unclosed paths</b>\n"
    "Remove paths from import if they don't have a closing segment, this also "
    "removes filled shapes where the path was not closed. Some drawing software "
    "only show a filled shape if the contour is closed (e.g. LibreOffice). While "
    "other software may still show a filled shape even if the contour is not "
    "closed (e.g. Inkscape). "
    "If some filled shapes appear missing in the import, try again with this "
    "unchecked.\n\n"
    "<b>Scale fit within view</b>\n"
    "Rescale the shape to fit in view. "
    "This means the scaling based on document units is ignored.\n\n"
    "<b>Combine all paths</b>\n"
    "When unchecked, the importer will import paths and create shapes in the "
    "same way as they will appear in the drawing program, this means that to "
    "make holes in a shape, they need to be made as holes in the drawing "
    "program by applying path combination operations (i.e. \"Path Difference\", "
    "\"Combine Path\").\n"
    "When checked, all paths are treated as able to create holes when they are "
    "placed over the top of another larger shape, even if hole shapes were not "
    "created in the drawing program using path combination operations. As all "
    "of the paths of the SVG are combined into one shape object in this mode, colors "
    "and clipped textures are not available.\n\n"
    "<b>Transforms in layer names</b>\n"
    "When checked, layer names of the SVG can contain transform information "
    "such as z=2 to move the shape in the z axis\n\n"
    "The SVG file can contain a clipped or mask image which will be used by "
    "the importer to make a texture.\n\n")].

info_button() ->
    Title = ?__(1,"SVG Import Information"),
    TextFun = fun () -> more_info() end,
    {help,Title,TextFun}.
    
-record(svg_importer_params, {
    nsubdiv,
    auto_scale,
    set_scale,
    use_viewbox_c,
    set_viewbox_s,
    auto_center,
    remove_non_cls,
    all_paths_invert,
    transforms_in_layers
}).

command({file,{import,{svg,Ask}}}, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(wpc_svg_path, svg_bisections, 3),
    AutoScale = wpa:pref_get(wpc_svg_path, svg_auto_scale, false),
    RemoveNonClosed = wpa:pref_get(wpc_svg_path, svg_remove_nonclosed, true),
    AllPathsInvertOverlap = wpa:pref_get(wpc_svg_path, svg_all_paths_invert_overlap, false),
    TransformsInLayerNames = wpa:pref_get(wpc_svg_path, svg_transforms_in_layer_nms, false),
    
    UseViewboxCoords = wpa:pref_get(wpc_svg_path, svg_use_viewbox_coords, false),
    SetViewBoxScale = wpa:pref_get(wpc_svg_path, svg_viewbox_scale, 0.010),
    
    %% Force SetScale to be text
    case wpa:pref_get(wpc_svg_path, svg_set_scale, "100pt") of
        Number when is_float(Number); is_integer(Number) ->
            SetScale = lists:flatten(io_lib:format("~p", [Number]));
        Str when is_list(Str) ->
            SetScale = Str
    end,
    
    Hook_ViewBoxCoords = fun(_Key, Value, Store) ->
        wings_dialog:enable(set_scale, Value =:= false, Store),
        wings_dialog:enable(set_viewbox_scale, Value =:= true, Store)
    end,
    Hook_Auto_Scale = fun(_Key, Value, Store) ->
        wings_dialog:enable(use_viewbox_coords, Value =:= false, Store),
        case Value =:= true of true ->
            wings_dialog:enable(set_scale, Value =:= false, Store),
            wings_dialog:enable(set_viewbox_scale, Value =:= false, Store);
        _ ->
            VBoxCVal = wings_dialog:get_value(use_viewbox_coords, Store) =:= true,
            wings_dialog:enable(set_scale, VBoxCVal =:= false, Store),
            wings_dialog:enable(set_viewbox_scale, VBoxCVal =:= true, Store)
        end
    end,
    Hook_All_Paths_Invert = fun(_Key, Value, Store) ->
        wings_dialog:enable(transforms_in_layer_nms, Value =:= false, Store)
    end,
    Dialog =
      [{hframe,[{label,?__(2,"Number of edge bisections")},
                {text,DefBisect,[{key,bisections}]}]},
       {hframe,[{label,?__(6,"Scale fraction") ++ ": 1 / "},
                {text,SetScale,[{key,set_scale}]}]},
       {?__(7,"Automatic center"),true,[{key,auto_center}]},
       {hframe,[
       {?__(13,"Use viewbox coordinates"),UseViewboxCoords,
            [{key,use_viewbox_coords},
             {hook, Hook_ViewBoxCoords},
             {info,?__(15,"Use the viewbox coordinate system directly instead of "
                          "the document, a scale factor has to be set.")}]},
       {label,?__(14,"scale factor") ++ ":"},
                {text,SetViewBoxScale,[{key,set_viewbox_scale}]}]},
       panel,
       {?__(9,"Remove unclosed paths"),RemoveNonClosed,
            [{info,?__(10,"If some filled shapes appear missing in the import,\n"
                          "try again with \"Remove unclosed paths\" unchecked.")},
             {key,remove_nonclosed}]},
       {?__(5,"Scale fit within view"),AutoScale,
            [{key,auto_scale_fit},
             {hook, Hook_Auto_Scale},
             {info,?__(16,"Automatically rescale shapes to fit within the camera view.")}]},
       {?__(11,"Combine all paths"),AllPathsInvertOverlap,
            [{key,all_paths_invert_overlap},
             {hook,Hook_All_Paths_Invert}]},
       {?__(12,"Transforms in layer names"),TransformsInLayerNames,
            [{key,transforms_in_layer_nms}]},
       panel,
       {hframe,[info_button()]},
       {hframe,[{label,?__(3,
           "(For best results when importing from Inkscape,\n"
           "convert all objects to paths and save as a plain .svg)")}]}
       ],
    wpa:dialog(Ask, ?__(1,".svg Path Import Options"),
        Dialog,
        fun(Res) -> {file,{import, svg, Res}} end);
command({file,{import, svg, List}}, St) when is_list(List) ->
    Nsub = proplists:get_value(bisections, List, 0),
    AutoScale = proplists:get_value(auto_scale_fit, List, false),
    SetScale_S = proplists:get_value(set_scale, List, "100pt"),
    UseViewboxCoords = proplists:get_value(use_viewbox_coords, List, false),
    SetViewBoxScale = proplists:get_value(set_viewbox_scale, List, 0.010),
    
    AutoCenter = proplists:get_value(auto_center, List, true),
    RemoveNonClosed = proplists:get_value(remove_nonclosed, List, true),
    AllPathsInvertOverlap = proplists:get_value(all_paths_invert_overlap, List, false),
    TransformsInLayerNames = proplists:get_value(transforms_in_layer_nms, List, false),
    Props = [{extensions,[
        {".svg",?__(4,".svg File")},
        {".svgz",?__(8,"Compressed .svg File")}]}],
    
    wpa:import(Props, fun(F) ->
        make_svg(F, #svg_importer_params{
            nsubdiv=Nsub,
            auto_scale=AutoScale,
            set_scale=SetScale_S,
            use_viewbox_c=UseViewboxCoords,
            set_viewbox_s=SetViewBoxScale,
            auto_center=AutoCenter,
            remove_non_cls=RemoveNonClosed,
            all_paths_invert=AllPathsInvertOverlap,
            transforms_in_layers=TransformsInLayerNames
        })
    end, St);
command(_, _) ->
    next.

make_svg(Name, #svg_importer_params{
    nsubdiv=Nsubsteps,
    auto_scale=AutoScale,
    set_scale=SetScale_S,
    use_viewbox_c=UseViewboxCoords,
    set_viewbox_s=SetViewBoxScale,
    auto_center=AutoCenter,
    remove_non_cls=RemoveNonClosed,
    all_paths_invert=AllPathsInvertOverlap,
    transforms_in_layers=TransformsInLayerNames
  }=_) ->
    case parse_float_number_w_unit(SetScale_S, 0.0) of
        {ScaleVal, ScaleUnit} when ScaleVal > 0.001 ->
            wpa:pref_set(wpc_svg_path, svg_set_scale, SetScale_S),
            SetScale = {ScaleVal, unit_atom(ScaleUnit)};
        _Unk ->
            SetScale = {100.0, pt}
    end,
    case UseViewboxCoords of
        true  -> SetViewBoxScale_1 = SetViewBoxScale;
        false -> SetViewBoxScale_1 = false
    end,
    try try_import_svg(Name, Nsubsteps, AutoScale, SetScale,
        SetViewBoxScale_1, AutoCenter, RemoveNonClosed,
        AllPathsInvertOverlap, TransformsInLayerNames) of
    {ok, E3dFile} ->
        wpa:pref_set(wpc_svg_path, svg_bisections, Nsubsteps),
        wpa:pref_set(wpc_svg_path, svg_auto_scale, AutoScale),
        wpa:pref_set(wpc_svg_path, svg_remove_nonclosed, RemoveNonClosed),
        wpa:pref_set(wpc_svg_path, svg_all_paths_invert_overlap, AllPathsInvertOverlap),
        wpa:pref_set(wpc_svg_path, svg_transforms_in_layer_nms, TransformsInLayerNames),
        wpa:pref_set(wpc_svg_path, svg_use_viewbox_coords, UseViewboxCoords),
        wpa:pref_set(wpc_svg_path, svg_viewbox_scale, SetViewBoxScale),
        {ok, E3dFile};
    {error, Reason} ->
        {error, ?__(1,"Inkscape .svg path import failed")++": " ++ Reason}
    catch
        EClass:E:ST ->
            io:format("File Import Error Report:\n ~p: ~p\nstack trace: ~p~n",
                [EClass, E, ST]),
            {error, ?__(2,"Inkscape .svg path import internal error")}
    end.

try_import_svg(Name, Nsubsteps, AutoScale, SetScale, SetViewBoxScale_1,
               AutoCenter, RemoveNonClosed, AllPathsInvertOverlap,
               TransformsInLayerNames) ->
    case read_file__svg(Name) of
    {ok,<<Rest/binary>>} ->
    
        ShortFilename = filename:rootname(filename:basename(Name)),
        FullFilename = Name,
        
        {ok, TexList_0, Objs, #document_s{units=DocUnits,docsz=DocSz,viewbox=ViewBox}=_DocSt} = parse_bin_svg(Rest, FullFilename, ShortFilename),
        
        case RemoveNonClosed of
            true ->
                Closedpaths_0 = [ Obj_0
                    || {[P|_]=_SubPaths,_ColTex}=Obj_0 <- Objs,
                       P#path.close =:= true ];
            false ->
                Closedpaths_0 = Objs
        end,
        case {AutoScale, SetViewBoxScale_1} of
            {true, _} ->
                [CamDist] = wpa:camera_info([distance_to_aim]),
                Rescale = calculate_rescale_amount(CamDist, CamDist, pathop_to_simple_list(Closedpaths_0)),
                RescaleW = Rescale,
                RescaleH = Rescale;
            {_, Scale_1} when Scale_1 =/= false ->
                RescaleW = Scale_1,
                RescaleH = Scale_1;
            _ ->
                {VWSc,VHSc} = viewbox_scale(DocSz,ViewBox),
                DocUnitRescale = conv_unit(SetScale, DocUnits),
                RescaleW = VWSc / DocUnitRescale,
                RescaleH = VHSc / DocUnitRescale
        end,
        RescalePair = {RescaleW,RescaleH},
        Closedpaths = rescale(RescalePair, Closedpaths_0),
        TexList = rescale_tex(RescalePair, TexList_0),
        
        Closedpaths_2 = getcontours(Closedpaths),
        Closedpaths_3 = remove_double_objs(Closedpaths_2),
        {Cntrs_0, ColTexs} = lists:unzip(Closedpaths_3),
        
        TexturesList = get_textures_list(ShortFilename, TexList),
        VColors = [ensure_color(FCol,FOpa)
            || #coltex{color=#style_colors{fcol=FCol,fopa=FOpa}=_} <- ColTexs],
        case Cntrs_0 of
            [] ->
                {error, "No paths found"};
                
            Cntrs when length(Cntrs) > 0 ->
                ColTexs_1 = case TransformsInLayerNames of
                    true -> ColTexs;
                    false -> [ CoTx#coltex{ut=[]} || CoTx <- ColTexs]
                end,
            
                {Vs0,Efs,Tx,HEs} = try_import_svg_1(AllPathsInvertOverlap, Cntrs, ColTexs_1, Nsubsteps, TexList),
                case AutoCenter of
                    true ->
                        Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
                        Vec = e3d_vec:sub(e3d_vec:zero(),Center),
                        Vs = [ e3d_vec:add(V,Vec) || V <- Vs0];
                    _ ->
                        Vs = Vs0
                end,
                Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,he=HEs,tx=Tx,vc=VColors},
                Obj = #e3d_object{name=ShortFilename,obj=Mesh},
                {ok, #e3d_file{objs=[Obj],mat=TexturesList}}
                
        end;
    {ok,_} ->
        {error,?__(1,"Not an .svg file")};
        {error,Reason} ->
        {error,file:format_error(Reason)}
    end.


%% Normal mode (can use colors and textures).
try_import_svg_1(false, Cntrs, ColTexs, Nsubsteps, TexList) ->
    Pas_0 = [ wpc_ai:findpolyareas(Cntr) || Cntr <- Cntrs],
    Pas_1 = [ wpc_ai:subdivide_pas(P,Nsubsteps) || P <- Pas_0],
    Pas_2 = filter_polyareas_min_3_cedges(Pas_1),
    
    ColTexs_1 = repeat_coltex_if_needed(ColTexs, Pas_2),
    Pas_3 = lists:append(Pas_2),
    List = process_islands(Pas_3),
    
    {Vs0,Efs,Tx,HEs} = into_mesh_parts(List, ColTexs_1, TexList),
    {Vs0,Efs,Tx,HEs};

%% Combine All paths
try_import_svg_1(true, Cntrs, _ColTexs, Nsubsteps, TexList) ->
    Cntrs_1 = lists:append(Cntrs),
    Pas_0 = wpc_ai:findpolyareas(Cntrs_1),
    Pas_1 = wpc_ai:subdivide_pas(Pas_0,Nsubsteps),
    [Pas_2] = filter_polyareas_min_3_cedges([Pas_1]),
    
    List = process_islands(Pas_2),
    
    {Vs0,Efs,Tx,HEs} = into_mesh_parts(List, [#coltex{} || _ <- List], TexList),
    {Vs0,Efs,Tx,HEs}.
                

ensure_color({R,G,B},Opa) when is_atom(Opa) ->
    {R,G,B,1.0};
ensure_color({R,G,B},Opa) when is_float(Opa) ->
    {R,G,B,Opa};
ensure_color(Atom,_) when is_atom(Atom) ->
    {0.9,0.9,0.9,1.0}.
    

%% Textures list for e3d_file
get_textures_list(Filename, TexList) when is_list(Filename) ->
    get_textures_list(Filename, TexList, []).
get_textures_list(_Filename, [], OMat) ->
    lists:reverse(OMat);
get_textures_list(Filename, [{new_tex,Where,A}|R], OMat) ->
    case A of
        #image_tag_r{data=#e3d_image{}=E3dImage} ->
            Maps = {maps, [{diffuse, E3dImage}]},
            OpenGL = {opengl,
                     [{ambient,{0.0,0.0,0.0,0.0}},
                      {specular, {0.2,0.2,0.2,1.0}},
                      {shininess,0.2},
                      {diffuse, {0.8,0.8,0.7,1.0}},
                      {emission,{0.0,0.0,0.0,1.0}},
                      {metallic,0.1},
                      {roughness,0.8},
                      {vertex_colors, set}]},
            M = {Where, [Maps, OpenGL]},
            get_textures_list(Filename, R, [M | OMat])
    end.


repeat_coltex_if_needed(MatList, Pas) ->
    repeat_coltex_if_needed(MatList, Pas, []).
repeat_coltex_if_needed([], [], O) ->
    lists:append(lists:reverse(O));
repeat_coltex_if_needed([M|MatList], [P|Pas], O) ->
    M_1 = [M || _I <- lists:seq(1, length(P))],
    repeat_coltex_if_needed(MatList, Pas, [M_1|O]).


into_mesh_parts(Objs, ColTex, TexList) ->
    TexList_1 = orddict:from_list([{Atom, Info} || {new_tex, Atom, Info} <- TexList]),
    into_mesh_parts(Objs, ColTex, TexList_1, 0, 0, 0, [], [], [], []).
into_mesh_parts([], _, _, _, _, _, Vs_L, Fs_L, Tx_L, He_L) ->
    {lists:append(lists:reverse(Vs_L)),
     lists:append(lists:reverse(Fs_L)),
     lists:append(lists:reverse(Tx_L)),
     lists:append(lists:reverse(He_L))};
into_mesh_parts([{Vs,Fs0,He0} | Objs], [#coltex{tex=Tex1,ut=T} | ColTex], TexList,
                ColorIdx, VsOffset, TxOffset, Vs_L, Fs_L, Tx_L, He_L) ->
    case Tex1 of
        none ->
            Tx = [],
            TxOffset_1 = TxOffset;
        {tex, TexName} ->
            #image_tag_r{x=SrcX,y=SrcY,width=SrcWidth,height=SrcHeight} =
                orddict:fetch(TexName, TexList),
            Tx = [to_uv({SrcX,SrcY},{SrcX+SrcWidth,SrcY+SrcHeight}, X, Y) || {X,Y,_} <- Vs],
            TxOffset_1 = TxOffset + length(Vs)
    end,
    VsOffset_1 = VsOffset + length(Vs),
    EFs = [ #e3d_face{
        vs=[V+VsOffset || V <- F],
        vc=[ColorIdx   || _ <- F],
        tx=[V+TxOffset || V <- F],
        mat=
            case Tex1 of
                none -> [];
                {tex, M} -> [M]
            end
    } || F <- Fs0],
    He = [{V1+VsOffset,V2+VsOffset} || {V1,V2} <- He0],
    
    CenterOffset = e3d_vec:average(e3d_vec:bounding_box(Vs)),
    Vs_1 = [transform_vs({X,Y,Z}, T, CenterOffset) || {X,Y,Z} <- Vs],
    
    into_mesh_parts(Objs, ColTex, TexList, ColorIdx + 1, VsOffset_1, TxOffset_1, [Vs_1 | Vs_L], [EFs | Fs_L], [Tx | Tx_L], [He | He_L]).

to_uv({DestX1,DestY1}, {DestX2,DestY2}, X, Y) ->
    UV = {(X-DestX1) / (DestX2-DestX1), (Y-DestY1) / (DestY2-DestY1)},
    % ?DEBUG_FMT("UV=~p~n", [UV]),
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
    


read_file__svg(Name) ->
    case string:to_lower(filename:extension(Name)) of
        ".svgz" ->
            case file:read_file(Name) of
                {ok, Data} ->
                    {ok, zlib:gunzip(Data)};
                Err ->
                    Err
            end;
        _ ->
            file:read_file(Name)
    end.


%% State file for xmerl sax.
-record(svgtk,
    {
    list = [],
    in_svg_tag = false :: boolean(), % Is event inside the <svg> tag
    skipping_tag = [],   % Currently inside a tag we are skipping
    units = "px" :: string(),
    docsz = {100.0,100.0} :: {float(), float()},
    viewbox = {0.0,0.0,100.0,100.0} :: {float(),float(),float(),float()},
    e_sty = []
    }).

-record(path_tag_r,
    {
    d :: string(),  %% SVG Path string
    id :: string(), %% id from svg
    style,          %% Color styles
    texture = none, %% Textures
    ta = [],        %% SVG transform attribute (tag nested)
    ut = []         %% User vertex transforms
    }).


parse_bin_svg(Bin, FullFilename, ShortFilename) ->
    {ok, PathsList_0, DocumentSt} = read_svg_content(Bin, FullFilename, ShortFilename),
    
    %% Split out texture information from paths here
    {TexList, PathsList_1} = lists:partition(
        fun({new_tex, _Atom, _Info}) -> true; (_) -> false end,
        PathsList_0),
    
    Objs = parse_svg_content(PathsList_1),
    
    {ok, TexList, Objs, DocumentSt}.

parse_svg_content(List_0) ->
    %% First try to remove double paths at the attribute string level,
    %% a second try is done after getcontours.
    List_1 = remove_double_paths(List_0),
    parse_chars(List_1).

%% % Parse characters and find valid paths
parse_chars(PathsList) ->
    parse_chars(PathsList, #pstate{}).
parse_chars([], #pstate{objects=[]}) ->
    wings_u:error_msg(?__(1,"No closed paths."));
parse_chars([], #pstate{objects=Objs,objects_tex=OTex}) ->
    lists:zip(Objs, OTex);
parse_chars([{path, #path_tag_r{d=Path_0,id=_Id,style=Style,
            texture=Texture,ta=TA,ut=T}=_, _Unused} | Rest],
            #pstate{objects=Objs,objects_tex=OTex_0}) ->
    Path = substitute_commands(tok_svgp(Path_0)),
    #pstate{osubpaths=OSubpaths_1} = Pst = parse_path(Path, 1.0, #pstate{osubpaths=[],first_subpath=true}),
    %% Add style and texture information for the new paths added.
    OTex_1 = prepend_tex(#coltex{color=Style,tex=Texture,ut=T}, OTex_0, 1), % length(Objs_1) - length(Objs)),
    Objs_1 = [apply_path_transform(lists:reverse(OSubpaths_1), TA)|Objs],
    parse_chars(Rest, Pst#pstate{objects=Objs_1,objects_tex=OTex_1}).

%% Prepend color and texture information for each path.
prepend_tex(Tuple, OTex, HowMany) when HowMany > 0 ->
    prepend_tex(Tuple, [Tuple|OTex], HowMany-1);
prepend_tex(_, OTex, 0) ->
    OTex.
    

parse_path([Op|_]=List, F, #pstate{curpath=#path{ops=[_|_]=Ops}=Path0,osubpaths=Objs})
           when Op =:= "M"; Op =:= "m" ->
    Path = Path0#path{ops=reverse(Ops),close=false},
    parse_path(List, F, #pstate{osubpaths=[Path|Objs],first_subpath=false});
parse_path([Op,X0,Y0|Rest], F, #pstate{curpath=#path{ops=Ops}=Path0}=Pst)
           when Op =:= "M" ->
    X = string_to_float(X0)*F,
    Y = string_to_float(Y0)*F,
    PathOp = #pathop{opkind=pmoveto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    L = "L",
    parse_path(Rest, F, Pst#pstate{op=L,curpath=Path});
parse_path([Op,X0,Y0|Rest], F, #pstate{curpath=#path{ops=Ops}=Path0,osubpaths=Objs,first_subpath=FirstSP}=Pst)
           when Op =:= "m" ->
    case {Ops, case FirstSP of true -> []; _ -> Objs end} of
        {[LastPathOp|_],_} ->
            {X2,Y2} = case LastPathOp of
              #pathop{opkind=plineto,x1=X1,y1=Y1} -> {X1,Y1};
              #pathop{opkind=pmoveto,x1=X1,y1=Y1} -> {X1,Y1};
              #pathop{opkind=pcurveto,x3=X1,y3=Y1} -> {X1,Y1}
            end;
        {[],[#path{ops=LastOps}=_LastObj|_]} ->
            %% Normally the reference point is the first coordinate because
            %% "z" returns the current coordinate back to the beginning, so we look at
            %% the first coordinate rather than the last one.
            [LastPathOp|_]=LastOps, 
            {X2,Y2} = case LastPathOp of
              #pathop{opkind=plineto,x1=X1,y1=Y1} -> {X1,Y1};
              #pathop{opkind=pmoveto,x1=X1,y1=Y1} -> {X1,Y1};
              #pathop{opkind=pcurveto,x3=X1,y3=Y1} -> {X1,Y1}
            end;
        {[],[]} ->
            {X2,Y2} = {0.0,0.0}
    end,
    X = string_to_float(X0)*F + X2,
    Y = string_to_float(Y0)*F - Y2,
    PathOp = #pathop{opkind=pmoveto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    L = "l",
    parse_path(Rest, F, Pst#pstate{op=L,curpath=Path});
parse_path([Op|Rest], F, Pst) when Op =:= "C";  Op =:= "c"; Op =:= "L"; Op =:= "l" ->
    parse_path(Rest, F, Pst#pstate{op=Op});
parse_path([Z], _F, #pstate{curpath=#path{ops=Ops}=Path0,osubpaths=Objs}=_Pst) when Z =:= "z"; Z =:= "Z" ->
    Path = Path0#path{ops=reverse(Ops),close=true},
    #pstate{osubpaths=[Path|Objs],first_subpath=false};
parse_path([Z|Rest], F, #pstate{curpath=#path{ops=Ops}=Path0,osubpaths=Objs}) when Z =:= "z"; Z =:= "Z" ->
    Path = Path0#path{ops=reverse(Ops),close=true},
    parse_path(Rest, F, #pstate{osubpaths=[Path|Objs],first_subpath=false});
parse_path([X0,Y0|Rest], F, #pstate{op="L",curpath=#path{ops=Ops}=Path0}=Pst) ->
    X = string_to_float(X0)*F,
    Y = string_to_float(Y0)*F,
    PathOp = #pathop{opkind=plineto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([X0,Y0|Rest], F, #pstate{op="l",curpath=#path{ops=Ops}=Path0}=Pst) ->
    [LastPathOp|_] = Ops,
    {X2,Y2} = case LastPathOp of
      #pathop{opkind=plineto,x1=X1,y1=Y1} -> {X1,Y1};
      #pathop{opkind=pmoveto,x1=X1,y1=Y1} -> {X1,Y1};
      #pathop{opkind=pcurveto,x3=X1,y3=Y1} -> {X1,Y1}
    end,
    X = string_to_float(X0)*F + X2,
    Y = string_to_float(Y0)*F - Y2,
    PathOp = #pathop{opkind=plineto,x1=X,y1=-Y},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([Xa,Ya,Xb,Yb,Xc,Yc|Rest], F, #pstate{op="C",curpath=#path{ops=Ops}=Path0}=Pst) ->
    X1 = string_to_float(Xa)*F,
    Y1 = string_to_float(Ya)*F,
    X2 = string_to_float(Xb)*F,
    Y2 = string_to_float(Yb)*F,
    X3 = string_to_float(Xc)*F,
    Y3 = string_to_float(Yc)*F,
    PathOp = #pathop{opkind=pcurveto,x1=X1,y1=-Y1,x2=X2,y2=-Y2,x3=X3,y3=-Y3},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([Xa,Ya,Xb,Yb,Xc,Yc|Rest], F, #pstate{op="c",curpath=#path{ops=Ops}=Path0}=Pst) ->
    [LastPathOp|_] = Ops,
    {Xa0,Ya0} = case LastPathOp of
      #pathop{opkind=plineto,x1=Xa1,y1=Ya1} -> {Xa1,Ya1};
      #pathop{opkind=pmoveto,x1=Xa1,y1=Ya1} -> {Xa1,Ya1};
      #pathop{opkind=pcurveto,x3=Xa1,y3=Ya1} -> {Xa1,Ya1}
    end,
    X1 = string_to_float(Xa)*F + Xa0,
    Y1 = string_to_float(Ya)*F - Ya0,
    X2 = string_to_float(Xb)*F + Xa0,
    Y2 = string_to_float(Yb)*F - Ya0,
    X3 = string_to_float(Xc)*F + Xa0,
    Y3 = string_to_float(Yc)*F - Ya0,
    PathOp = #pathop{opkind=pcurveto,x1=X1,y1=-Y1,x2=X2,y2=-Y2,x3=X3,y3=-Y3},
    Path = Path0#path{ops=[PathOp|Ops]},
    parse_path(Rest, F, Pst#pstate{curpath=Path});
parse_path([], _F, #pstate{curpath=#path{ops=Ops}=Path0,osubpaths=Objs}) when length(Ops) > 2 ->
    Path = Path0#path{ops=reverse(Ops),close=false},
    #pstate{osubpaths=[Path|Objs],first_subpath=false};
parse_path([], _, Pst) ->
    Pst;
parse_path(_, _, _) ->
    wings_u:error_msg(?__(3,"Couldn't process this file.")).

string_to_float(String) ->
    wings_util:string_to_float(String).

getcontours(Objs) ->
    L=[{[getcedges(P) || P <- Ps],ColTex} || {Ps,ColTex} <- Objs],
    lists:filter(
        fun ({[[]], _}) -> false;
            ({[], _}) -> false;
            (_)       -> true
        end, L).

getcedges(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops]}) ->
    getcedges(Ops,{X,Y},{X,Y},[]);
getcedges(_) -> [].

getcedges([],{X,Y},{X,Y},Acc) ->
    reverse(Acc);
getcedges([],Prev,{X,Y},Acc) ->        % prev != first, so close with line
    reverse([#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=plineto,x1=X,y1=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X,y3=Y}|Ops],
        Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,
        [#cedge{vs=Prev,cp1={X1,Y1},cp2={X2,Y2},ve={X,Y}}|Acc]);
getcedges([_|_],_,_,_) ->
    [].    % funny path (probably moveto in middle), so return nothing


%% Apply transforms within SVG coordinate system to path
%%
apply_path_transform(OSubpaths0, TAL) ->
    lists:foldr(fun (TA, OSubpaths) ->
        [Path#path{ops=[ begin
            {X1,Y1} = vs_m3x2({X1_0,Y1_0},TA),
            {X2,Y2} = vs_m3x2({X2_0,Y2_0},TA),
            {X3,Y3} = vs_m3x2({X3_0,Y3_0},TA),
            PathOp#pathop{x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3}
           end
            || #pathop{x1=X1_0,y1=Y1_0,x2=X2_0,y2=Y2_0,x3=X3_0,y3=Y3_0}=PathOp <- Ops_0]}
            || #path{ops=Ops_0}=Path <- OSubpaths]
    end, OSubpaths0, TAL).
-spec vs_m3x2({float(),float()}, 'none' | m3x2()) -> {float(), float()}.
vs_m3x2({X,Y}, none) ->
    {X,Y};
vs_m3x2({X,Y0}, {matrix, M00,M10,M20, M01,TX,TY}) ->
    Y = -Y0,
    { M00*X + M20*Y + TX,
      -(M10*X + M01*Y + TY) }.


%%
%% New xmerl parsing section for SVG files.
%%

read_svg_content(Bin_0, FullFilename, ShortFilename) ->
    EF = {event_fun, fun svg_tok/3},
    ES = {event_state, #svgtk{}},
    {ok, Bin_1} = svg_change_prolog(Bin_0),
    {Bin_2, StyleTagContents} = extract_styles(Bin_1),
    case xmerl_sax_parser:stream(Bin_2, [EF,ES]) of
        {ok, #svgtk{list=List,units=DocUnits_S,docsz=DocSz,viewbox=ViewBox,e_sty=ESty}=_Es, _} ->
            CurDir = filename:dirname(FullFilename),
            ApplStyle_1 = parse_stylesheet_list(StyleTagContents, CurDir),
            ApplStyle_2 = get_link_rel_css(ESty, CurDir),
            ApplStyle = ApplStyle_1 ++ ApplStyle_2,
            DocUnits = unit_atom(DocUnits_S),
            CommandList_0 = match_css(lists:reverse(List), ApplStyle),
            {ok, {Definitions, CommandList_1}} =
                path_definitions(CommandList_0,
                    #document_s{fullfilename=FullFilename,units=DocUnits,docsz=DocSz,viewbox=ViewBox}),
            {ok, CommandList_2} = retr_d_svg_paths(Definitions, CommandList_1, ShortFilename),
            {ok, CommandList_3} = assign_transforms(CommandList_2),
            {ok, CommandList_3, #document_s{fullfilename=FullFilename,units=DocUnits,docsz=DocSz,viewbox=ViewBox}};
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("SVG:~p: ERROR: ~p:~p~n", [Line, Error, Reason]),
            {error, "unknown/unhandled format, see log window"}
    end.

%% xmerl doesn't like SGML declarations,
%% SVG doesn't have them often though SVG 1.0 files can contain a DOCTYPE
%% declaration, remove them if they are present.
svg_change_prolog(A) ->
    svg_change_prolog(A, []).
svg_change_prolog(<<>>, SoFar) ->
    {ok, iolist_to_binary(lists:reverse(SoFar))};
svg_change_prolog(<<"<!", B/binary>>, SoFar) ->
    [_Before, After] = string:split(B, <<">">>),
    %% Remove SGML things, things that start with <!
    svg_change_prolog(After, SoFar);
svg_change_prolog(<<"<svg", _/binary>> = SVGStart, SoFar) ->
    %% Beginning of SVG
    {ok, iolist_to_binary(lists:reverse(SoFar) ++ [SVGStart])};
svg_change_prolog(<<A, B/binary>>, SoFar) ->
    svg_change_prolog(B, [A | SoFar]).

%%
%% Get the document's units from either the width or height attribute
%%
get_svg_tag_units_from_attrs(Attributes0) ->
    get_svg_tag_units_from_attrs(Attributes0, "px", 1.0, 1.0).
get_svg_tag_units_from_attrs([{_,_,AttrLName,Val}|R], DocUnit, Width, Height) ->
    case string:to_lower(AttrLName) of
        "width" ->
            Height_1 = Height,
            case parse_float_number_w_unit(Val, 1.0) of
                {NewWidth, user}  ->
                    Width_1 = NewWidth,
                    DocUnit_1 = DocUnit;
                {NewWidth, UnitW} ->
                    Width_1 = NewWidth,
                    DocUnit_1 = UnitW
            end;
        "height" ->
            Width_1 = Width,
            case parse_float_number_w_unit(Val, 1.0) of
                {NewHeight, user}  ->
                    Height_1 = NewHeight,
                    DocUnit_1 = DocUnit;
                {NewHeight, UnitH} ->
                    Height_1 = NewHeight,
                    DocUnit_1 = UnitH
            end;
        _ ->
            DocUnit_1 = DocUnit,
            Width_1 = Width,
            Height_1 = Height
    end,
    get_svg_tag_units_from_attrs(R, DocUnit_1, Width_1, Height_1);
get_svg_tag_units_from_attrs([], DocUnit, Width, Height) ->
    {DocUnit,
        if Width =< 0.0 -> 1.0; true -> Width end,
        if Height =< 0.0 -> 1.0; true -> Height end}.
        
get_svg_tag_viewbox([{_,_,AttrLName,Val}|R]) ->
    case string:to_lower(AttrLName) of
        "viewbox" ->
            case [ parse_float_number_no_unit(Num, 0.0)
                || Num <- string:tokens(Val, " ,") ]
            of
                [L,T,W,H|_] -> {L,T,W,H};
                _ -> none
            end;
        _ ->
            get_svg_tag_viewbox(R)
    end;
get_svg_tag_viewbox([]) ->
    none.


%% Some software output SVG files with dimensions in percentages with no
%% indication at all as to the physical dimensions of the shapes in the file.
%% In such cases, substitute non units such as percentages ("100%") to actual
%% units. Use the viewbox's width and height for hinting to which paper format
%% to assume.
%%
substitute_non_units({"%", Width, Height}, {_, _, BWR, BHR}) ->
    Ratio_0 = BHR / BWR,
    substitute_non_units_1(Ratio_0, Width, Height,
        [{1.413, 1.415, "mm", 210.0, 297.0, "A4 portrait"},
         {0.706, 0.708, "mm", 297.0, 210.0, "A4 landscape"},
         {1.291, 1.295, "in", 8.5, 11.0, "letter portrait"},
         {0.770, 0.774, "in", 11.0, 8.5, "letter landscape"}
         ]);
substitute_non_units({"%", Width, Height}, none) ->
    {"mm", 210.0 * (Width / 100.0), 210.0 * (Height / 100.0)};
substitute_non_units({DocUnits_S, Width, Height}, _ViewBox) ->
    {DocUnits_S, Width, Height}.
substitute_non_units_1(Ratio_1, Width, Height, [NextDimen | R]) ->
    {MinRatio, MaxRatio, Unit_S, ExpectedWidth, ExpectedHeight, DimenName} = NextDimen,
    case Ratio_1 > MinRatio andalso Ratio_1 < MaxRatio of
        true ->
            io:format("SVG: Assuming: ~p~n", [DimenName]),
            {Unit_S, ExpectedWidth * (Width / 100.0), ExpectedHeight * (Height / 100.0)};
        false ->
            substitute_non_units_1(Ratio_1, Width, Height, R)
    end;
substitute_non_units_1(Ratio_1, Width, Height, []) ->
    %% No hints to page size so just make a guess
    {"mm", 210.0 * (Width / 100.0), 210.0 * (Height * Ratio_1 / 100.0)}.


%% Some software exporting SVGs, as well as hand made SVGs, may omit the
%% viewbox attribute, create one from the document's width and height.
%%
make_viewbox_if_needed(none, {Width, Height}) ->
    {0.0, 0.0, Width, Height};
make_viewbox_if_needed(ViewBox, _) ->
    ViewBox.



svg_tok({startElement, _, LName, _, Attributes_0}=_Ev, _Loc,
        #svgtk{skipping_tag=SkippingTag,list=List,in_svg_tag=InSVGTag,e_sty=ESty}=State) ->
    case LName of
        "svg" ->
            Wi_He_Units_0 = get_svg_tag_units_from_attrs(Attributes_0),
            ViewBox_0 = get_svg_tag_viewbox(Attributes_0),
            {DocUnits_S, Width, Height} = substitute_non_units(Wi_He_Units_0, ViewBox_0),
            ViewBox_1 = make_viewbox_if_needed(ViewBox_0, {Width, Height}),
            State#svgtk{in_svg_tag=true,units=DocUnits_S,docsz={Width,Height},viewbox=ViewBox_1};
        "namedview" ->
            State#svgtk{skipping_tag=["namedview" | SkippingTag]};
        "pattern" ->
            State#svgtk{skipping_tag=["pattern" | SkippingTag]};
        "link" when SkippingTag =:= [] ->
            LinkAttrs = [
                {string:to_lower(AttrLName), Val}
            || {_,_,AttrLName,Val} <- Attributes_0],
            State#svgtk{e_sty=[{linkrel,LinkAttrs} | ESty]};
        _ when InSVGTag =:= true andalso SkippingTag =:= [] ->
            Attributes_1 = [
                svg_tok_attr_pair(string:to_lower(LName), string:to_lower(AttrLName), Val)
            || {_,_,AttrLName,Val} <- Attributes_0],
            Attributes = lists:filter(fun (unused) -> false; (_) -> true end, Attributes_1),
            State#svgtk{list=[{s, string:to_lower(LName), Attributes} | List]};
        _ ->
            State
    end;
svg_tok({endElement, _, LName, _}=_Ev, _Loc, #svgtk{skipping_tag=SkippingTag,list=List,in_svg_tag=InSVGTag}=State) ->
    case LName of
        "svg" ->
            State#svgtk{ in_svg_tag = false };
        "namedview" ->
            State#svgtk{ skipping_tag = lists:nthtail(1, SkippingTag) };
        "pattern" ->
            State#svgtk{ skipping_tag = lists:nthtail(1, SkippingTag) };
        "link" ->
            State;
        _ when InSVGTag =:= true andalso SkippingTag =:= [] ->
            State#svgtk{list=[{e, string:to_lower(LName)} |List]};
        _ ->
            State
    end;
svg_tok({characters, _}=_Ev, _Loc, State) ->
    State;
svg_tok(startDocument, _, State) -> State;
svg_tok(endDocument, _, State) -> State;
svg_tok({startPrefixMapping,_,_}, _, State) -> State;
svg_tok({endPrefixMapping,_}, _, State) -> State;
svg_tok({ignorableWhitespace, _}, _, State) -> State;
svg_tok({comment, _}, _, State) -> State;
svg_tok(_Ev, _Loc, State) ->
    State.


svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "clippath" ->
    svg_tok_attr_pair_clippath(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "mask" ->
    svg_tok_attr_pair_mask(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "image" ->
    svg_tok_attr_pair_image(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "g" ->
    svg_tok_attr_pair_g(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "path" ->
    svg_tok_attr_pair_path(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "polyline" ->
    svg_tok_attr_pair_polyline(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "polygon" ->
    svg_tok_attr_pair_polygon(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "rect" ->
    svg_tok_attr_pair_rect(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "circle" ->
    svg_tok_attr_pair_circle(AttrLName, Val);
svg_tok_attr_pair(LName, AttrLName, Val) when LName =:= "ellipse" ->
    svg_tok_attr_pair_ellipse(AttrLName, Val);
svg_tok_attr_pair(_LName, _AttrLName, _Val) ->
    unused.




%% <clipPath> elements
%%
svg_tok_attr_pair_clippath(AttrLName, Val)
  when AttrLName =:= "id" ->
    {id, Val};
svg_tok_attr_pair_clippath(AttrLName, Val)
  when AttrLName =:= "class" ->
    {class, Val};
svg_tok_attr_pair_clippath(AttrLName, Val)
  when AttrLName =:= "clippathunits" ->
    {clipPathUnits, Val};
svg_tok_attr_pair_clippath(_AttrLName, _Val) ->
    unused.
svg_tok_attr_pair_mask(AttrLName, Val)
  when AttrLName =:= "id" ->
    {id, Val};
svg_tok_attr_pair_mask(AttrLName, Val)
  when AttrLName =:= "class" ->
    {class, Val};
svg_tok_attr_pair_mask(AttrLName, Val)
  when AttrLName =:= "maskunits" ->
    {clipPathUnits, Val};
svg_tok_attr_pair_mask(_AttrLName, _Val) ->
    unused.
    
%% <image> elements
%%
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "clip-path";
       AttrLName =:= "mask" ->
    {ok, ClipPathId} = parse_clip_path(Val),
    {clippath, ClipPathId};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "x" ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {x, Val_1};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "y" ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {y, Val_1};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "href" ->
    case parse_image_href(Val) of
        {ok, ImageContent} ->
            {data, ImageContent};
        _ ->
            unused
    end;
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "width" ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {width, Val_1};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "height" ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {height, Val_1};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "transform" ->
    {ok, Transform_1} = parse_image_transform(Val),
    {transform, Transform_1};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "id" ->
    {id, Val};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "class" ->
    {class, Val};
svg_tok_attr_pair_image(AttrLName, Val)
  when AttrLName =:= "preserveAspectRatio" ->
    {preserve_aspect_ratio, Val};
svg_tok_attr_pair_image(_AttrLName, _Val) ->
    unused.

%% <g> elements
svg_tok_attr_pair_g(AttrLName, Val)
  when AttrLName =:= "id" ->
    {id, Val};
svg_tok_attr_pair_g(AttrLName, Val)
  when AttrLName =:= "class" ->
    {class, Val};
svg_tok_attr_pair_g(AttrLName, Val)
  when AttrLName =:= "label" ->
    {label, Val};
svg_tok_attr_pair_g(AttrLName, Val)
  when AttrLName =:= "transform" ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_g(_AttrLName, _Val) ->
    unused.

svg_tok_color_attr("style", Val) ->
    {ok, Styles} = parse_style(Val),
    {ok, Styles_1} = style_to_tuple(Styles),
    {style, Styles_1};
svg_tok_color_attr("stroke", SVal) % This is only useful to remove bounding boxes
    ->
    {ok, SVal_1} = tok_svg_attr(SVal),
    {ok, #style_colors{scol=StrokeColor}} = style_to_tuple([{"stroke", SVal_1}]),
    {style_stroke, StrokeColor};
svg_tok_color_attr("fill", FVal) ->
    {ok, FVal_1} = tok_svg_attr(FVal),
    {ok, #style_colors{fcol=FillColor}} = style_to_tuple([{"fill", FVal_1}]),
    {style_fill, FillColor};
svg_tok_color_attr("fill-opacity", FVal) ->
    {ok, FVal_1} = tok_svg_attr(FVal),
    {ok, #style_colors{fopa=FillOpacity}} = style_to_tuple([{"fill-opacity", FVal_1}]),
    {style_fillopa, FillOpacity}.

%% <path> elements
%%
svg_tok_attr_pair_path("id", Val) ->
    {id, Val};
svg_tok_attr_pair_path("class", Val) ->
    {class, Val};
svg_tok_attr_pair_path(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
    svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_path("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_path("d", Val) ->
    {d, Val};
svg_tok_attr_pair_path(_AttrLName, _Val) ->
    unused.
    
%% <polyline> elements
%%
svg_tok_attr_pair_polyline("id", Val) ->
    {id, Val};
svg_tok_attr_pair_polyline("class", Val) ->
    {class, Val};
svg_tok_attr_pair_polyline(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
     svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_polyline("points", Val) ->
    {points, Val};
svg_tok_attr_pair_polyline("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_polyline(_AttrLName, _Val) ->
    unused.
    
%% <polygon> elements
%%
svg_tok_attr_pair_polygon("id", Val) ->
    {id, Val};
svg_tok_attr_pair_polygon("class", Val) ->
    {class, Val};
svg_tok_attr_pair_polygon(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
    svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_polygon("points", Val) ->
    {points, Val};
svg_tok_attr_pair_polygon("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_polygon(_AttrLName, _Val) ->
    unused.
    
%% <rect> elements
%%
svg_tok_attr_pair_rect(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
    svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_rect("id", Val) ->
    {id, Val};
svg_tok_attr_pair_rect("class", Val) ->
    {class, Val};
svg_tok_attr_pair_rect("width", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {width, Val_1};
svg_tok_attr_pair_rect("height", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {height, Val_1};
svg_tok_attr_pair_rect("x", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {x, Val_1};
svg_tok_attr_pair_rect("y", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {y, Val_1};
svg_tok_attr_pair_rect("ry", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {ry, Val_1};
%% Inkscape only seems to use "ry" for rounded corners but this importer can use 
%% both "rx" and "ry" rounding
svg_tok_attr_pair_rect("rx", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {rx, Val_1};
svg_tok_attr_pair_rect("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_rect(_AttrLName, _Val) ->
    unused.


%% <circle> elements
%%
svg_tok_attr_pair_circle(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
    svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_circle("id", Val) ->
    {id, Val};
svg_tok_attr_pair_circle("class", Val) ->
    {class, Val};
svg_tok_attr_pair_circle("cx", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {cx, Val_1};
svg_tok_attr_pair_circle("cy", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {cy, Val_1};
svg_tok_attr_pair_circle("r", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {r, Val_1};
svg_tok_attr_pair_circle("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_circle(_AttrLName, _Val) ->
    unused.

%% <ellipse> elements
%%
svg_tok_attr_pair_ellipse(StyleAttr, Val)
  when StyleAttr =:= "style";
       StyleAttr =:= "stroke";
       StyleAttr =:= "fill";
       StyleAttr =:= "fill-opacity" ->
    svg_tok_color_attr(StyleAttr, Val);
svg_tok_attr_pair_ellipse("id", Val) ->
    {id, Val};
svg_tok_attr_pair_ellipse("class", Val) ->
    {class, Val};
svg_tok_attr_pair_ellipse("cx", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {cx, Val_1};
svg_tok_attr_pair_ellipse("cy", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {cy, Val_1};
svg_tok_attr_pair_ellipse("rx", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {rx, Val_1};
svg_tok_attr_pair_ellipse("ry", Val) ->
    Val_1 = parse_float_number_w_unit(Val, 0.0),
    {ry, Val_1};
svg_tok_attr_pair_ellipse("transform", Val) ->
    case parse_transform_attr(Val) of
        {ok, Val_1} -> {transform, Val_1}
    end;
svg_tok_attr_pair_ellipse(_AttrLName, _Val) ->
    unused.

-spec style_to_tuple([{string(),any()}]) -> {ok, #style_colors{}}.
style_to_tuple(Styles) ->
    DCol = 220 / 255.0,
    case orddict:find("stroke", Styles) of
        {ok, [InhSCol]} when InhSCol =:= "inherit"; InhSCol =:= "initial"; InhSCol =:= "unset" ->
            StrokeColor = inherit;
        {ok, ["none"]} ->
            StrokeColor = none;
        error ->
            StrokeColor = inherit;
        _ ->
            StrokeColor = opaque % just not 'none', used for invisible boundary boxes
    end,
    case orddict:find("fill", Styles) of
        {ok, [InhFCol]} when InhFCol =:= "inherit"; InhFCol =:= "initial"; InhFCol =:= "unset" ->
            FillColor = inherit;
        {ok, ["rgb", open, R_0, G_0, B_0, close]} ->
            FillColor = {float(R_0) / 255.0, float(G_0) / 255.0, float(B_0) / 255.0};
        {ok, ["rgba", open, R_0, G_0, B_0, _, close]} ->
            FillColor = {float(R_0) / 255.0, float(G_0) / 255.0, float(B_0) / 255.0};
        {ok, [FillColor_0]} when is_list(FillColor_0) ->
            FillColor_1 = string:trim(string:lowercase(FillColor_0)),
            case FillColor_1 of
                "none" ->
                    FillColor = none;
                [$# | _] ->
                    {ok, FillColor} = parse_fill_color(FillColor_1);
                [Ch | _] when Ch >= $a, Ch =< $z; Ch >= $A, Ch =< $Z ->
                    case x11_color_names(FillColor_1) of
                        {X11R, X11G, X11B} ->
                            FillColor = {X11R / 255.0, X11G / 255.0, X11B / 255.0};
                        _ ->
                            FillColor = {DCol, DCol, DCol}
                    end;
                _ ->
                    FillColor = {DCol, DCol, DCol}
            end;
        error -> FillColor = inherit
    end,
    case orddict:find("fill-opacity", Styles) of
        {ok, [InhFOpa]} when InhFOpa =:= "inherit"; InhFOpa =:= "initial"; InhFOpa =:= "unset" ->
            FillOpacity = inherit;
        {ok, [FillOpacity_0]} when is_integer(FillOpacity_0) ->
            FillOpacity = float(FillOpacity_0);
        {ok, [FillOpacity_0]} when is_float(FillOpacity_0) ->
            FillOpacity = FillOpacity_0;
        error ->
            FillOpacity = inherit;
        _ ->
            FillOpacity = 1.0
    end,
    {ok, #style_colors{scol=StrokeColor,fcol=FillColor,fopa=FillOpacity}}.

%% Parse hex colors
%%
parse_fill_color([$#, HexR, HexG, HexB])
  when ?IS_HEX_DIGIT(HexR),
       ?IS_HEX_DIGIT(HexG),
       ?IS_HEX_DIGIT(HexB) ->
    %% 'Small' hex notation of three hex digits
    R = ((hex_to_dec(HexR) bsl 4) + hex_to_dec(HexR)) / 255.0,
    G = ((hex_to_dec(HexG) bsl 4) + hex_to_dec(HexG)) / 255.0,
    B = ((hex_to_dec(HexB) bsl 4) + hex_to_dec(HexB)) / 255.0,
    {ok, {R, G, B}};
parse_fill_color(A) ->
    parse_fill_color(A, []).
parse_fill_color([$# | R], []) -> 
    parse_fill_color(R, []);
parse_fill_color([A0, A1 | R], Hexs)
  when ?IS_HEX_DIGIT(A0),
       ?IS_HEX_DIGIT(A1) ->
    CChn = (hex_to_dec(A0) bsl 4) + hex_to_dec(A1),
    parse_fill_color(R, [CChn / 255.0 | Hexs]);
parse_fill_color(_, [B, G, R | _ ]) ->
    {ok, {R, G, B}}.

hex_to_dec(A) when A >= $A, A =< $F ->
    A - $A + 10;
hex_to_dec(A) when A >= $a, A =< $f ->
    A - $a + 10;
hex_to_dec(A) when A >= $0, A =< $9 ->
    A - $0.

parse_float_number_no_unit(Num_S, DVal) ->
    case tok_svg_attr_number_or_kw_1(Num_S) of
        NotNum when is_list(NotNum) -> DVal;
        {Num_I, _Unit} when is_integer(Num_I) -> Num_I * 1.0;
        {Num_F, _Unit} when is_float(Num_F) -> Num_F;
        Num_I when is_integer(Num_I) -> Num_I * 1.0;
        Num_F when is_float(Num_F) -> Num_F;
        _ -> DVal
    end.
parse_float_number_w_unit(Num_S, DVal) ->
    case tok_svg_attr_number_or_kw_1(Num_S) of
        NotNum when is_list(NotNum) -> {DVal, user};
        {Num_I, Unit} when is_integer(Num_I) -> {Num_I * 1.0, Unit};
        {Num_F, Unit} when is_float(Num_F) -> {Num_F, Unit};
        {Num_I, ""} when is_integer(Num_I) -> {Num_I * 1.0, user};
        {Num_F, ""} when is_float(Num_F) -> {Num_F, user};
        Num_I when is_integer(Num_I) -> {Num_I * 1.0, user};
        Num_F when is_float(Num_F) -> {Num_F, user};
        _ -> {DVal, user}
    end.

parse_style(A) ->
    parse_style(A, [], []).

parse_style([], SName, List) when length(SName) > 0 ->
    parse_style([], [], [{string:strip(lists:reverse(SName)), ""} | List]);
parse_style([], [], List) ->
    {ok, orddict:from_list(List)};
parse_style([$: | R], SName, List) ->
    {ok, StyleVal, R_1} = tok_svg_attr(R, [], []),
    parse_style(R_1, [], [{string:strip(lists:reverse(SName)), StyleVal} | List]);
parse_style([A | R], [], List)
  when A =:= 32; A =:= 9; A =:= 10; A =:= 13 ->
    parse_style(R, [], List);
parse_style([A | R], SName, List)
  when A =/= $: , A =/= $; ->
    parse_style(R, [A | SName], List).

%%%
%%% Parse clipPath
%%%

parse_clip_path(A) ->
    %% "url(#clipPath1)"
    case tok_svg_attr(A) of
        {ok, ["url", open, IdTok, close]} ->
            case IdTok of
                [$# | IdTok2] -> {ok, IdTok2};
                _ -> {ok, IdTok}
            end;
        _ ->
            error
    end.

%%%
%%% Parse Image HREF
%%%
    
parse_image_href(A) ->
    case parse_image_href_scheme(A) of
        {ok, "data", Rest} ->
            %% "data:image/png;base64,iV .. ="
            case parse_image_href_data_mime(Rest) of
                {ok, MimeType, Rest_1} ->
                    case parse_image_href_data_enctype(Rest_1) of
                        {ok, EncType, Data} ->
                            {ok, {data, MimeType, {EncType, Data}}};
                        _ ->
                            none
                    end;
                _ ->
                    none
            end;
        {ok, local, RelPath} ->
            %% A relative path to a local file
            {ok, {rel, unesc_url(RelPath)}};
        {ok, dospath, FilePath} ->
            %% An absolute file path that is apparently not in URL format.
            {ok, {abs, FilePath}};
        {ok, "file", URL} ->
            %% An absolute URL path to a local file
            {ok, {abs, url_to_filepath(unesc_url(URL))}};
        
        %% Ignore remote URLs and unparseable URLs
        _ ->
            %% A remote image? we won't connect to it.
            none
    end.

-define(DRIVE_LETTER_RANGE(DriveLetter), (
    (DriveLetter >= $a andalso DriveLetter =< $z) orelse
    (DriveLetter >= $A andalso DriveLetter =< $Z))).

parse_image_href_scheme([C1,C2,C3 | _]=R)
  when ?DRIVE_LETTER_RANGE(C1), C2 =:= $:, (C3 =:= $/ orelse C3 =:= $\\) ->
    {ok, dospath, R};
parse_image_href_scheme([C | _]=R) when C =:= $/ orelse C =:= $\\ orelse C =:= $. ->
    {ok, local, R};
parse_image_href_scheme(A) ->
    parse_image_href_scheme(A, []).
parse_image_href_scheme([$: | R], List) ->
    {ok, string:to_lower(lists:reverse(List)), R};
parse_image_href_scheme([C | _]=R, List) when C =:= $/ orelse C =:= $\\ orelse C =:= $. ->
    {ok, local, lists:reverse(List) ++ R};
parse_image_href_scheme([A | R], List) ->
    parse_image_href_scheme(R, [A | List]);
parse_image_href_scheme([], List) ->
    {ok, local, lists:reverse(List)}.

parse_image_href_data_mime(A) ->
    parse_image_href_data_mime(A, []).
parse_image_href_data_mime([$; | R], List) ->
    {ok, lists:reverse(List), R};
parse_image_href_data_mime([A | R], List) ->
    parse_image_href_data_mime(R, [A | List]);
parse_image_href_data_mime([], _List) ->
    error.

parse_image_href_data_enctype(A) ->
    parse_image_href_data_enctype(A, []).
parse_image_href_data_enctype([$, | R], List) ->
    {ok, lists:reverse(List), R};
parse_image_href_data_enctype([A | R], List) ->
    parse_image_href_data_enctype(R, [A | List]);
parse_image_href_data_enctype([], _List) ->
    error.

url_to_filepath("///" ++ [DriveLetter,C | Path])
  when C =:= $: orelse C =:= $| , ?DRIVE_LETTER_RANGE(DriveLetter) ->
    %% PC style local URL
    DriveLetter ++ ":" ++ Path;
url_to_filepath("//" ++ [DriveLetter,C | Path])
  when C =:= $: orelse C =:= $| , ?DRIVE_LETTER_RANGE(DriveLetter) ->
    DriveLetter ++ ":" ++ Path;
url_to_filepath("///" ++ Path) ->
    "/" ++ Path;
url_to_filepath("//" ++ Path) ->
    Path;
url_to_filepath(Path) ->
    Path.
    
unesc_url(A) ->
    unesc_url(A, []).
unesc_url([C,D1,D2|R], O) when C =:= $% ->
    case (hex_to_dec(D1) bsl 4) + hex_to_dec(D2) of
        Num when Num >= 16#20 andalso Num =< 16#7E ->
            unesc_url(R, [Num |O]);
        _ ->
            %% We don't know the encoding
            unesc_url(R, O)
    end;
unesc_url([C|_R], O) when C =:= $? ; C =:= $; ->
    %% These URL characters shouldn't be in a file url.
    lists:reverse(O);
unesc_url([C|_R], O) when C =:= $# ->
    %% Beginning of a fragment id, return
    lists:reverse(O);
unesc_url([C|R], O) ->
    unesc_url(R, [C|O]);    
unesc_url([], O) ->
    lists:reverse(O).

%%%
%%%

%% SVG transform attribute transformations
%%
parse_transform_attr(A) ->
    case tok_svg_attr(A) of
        {ok, TransformList} ->
            Mat_0 = m3x2_mat(),
            Matrix = parse_transform_attr_1(TransformList, [Mat_0]),
            {ok, Matrix}
    end.
parse_transform_attr_1(["matrix", open, A0,A1,A2,A3, A4,A5, close | R], M) ->
    NewM = {float(A0), float(A1), float(A2), float(A3), float(A4), float(A5)},
    parse_transform_attr_1(R, [NewM|M]);

% translate(<x> [<y>])
parse_transform_attr_1(["translate", open, X, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_translate(float(X), 0.0)|M]);
parse_transform_attr_1(["translate", open, X,Y, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_translate(float(X), float(Y))|M]);

% scale(<x> [<y>])
parse_transform_attr_1(["scale", open, X, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_scale(float(X), float(X))|M]);
parse_transform_attr_1(["scale", open, X,Y, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_scale(float(X), float(Y))|M]);

% rotate(<a> [<x> <y>])
parse_transform_attr_1(["rotate", open, A, close | R], M) ->
    parse_transform_attr_1(R,
        [ m3x2_rotate(-float(A) / 180.0 * math:pi()) | M]);
parse_transform_attr_1(["rotate", open, A,X, close | R], M) ->
    parse_transform_attr_1(R,
        [ m3x2_translate(-float(X), 0.0),
          m3x2_rotate(-float(A) / 180.0 * math:pi()),
          m3x2_translate(float(X), 0.0) | M]);
parse_transform_attr_1(["rotate", open, A,X,Y, close | R], M) ->
    parse_transform_attr_1(R,
        [ m3x2_translate(-float(X), -float(Y)),
          m3x2_rotate(-float(A) / 180.0 * math:pi()),
          m3x2_translate(float(X), float(Y)) | M]);

parse_transform_attr_1(["skewX", open, X, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_skew(float(X) / 180.0 * math:pi(), 0.0)|M]);
parse_transform_attr_1(["skewY", open, Y, close | R], M) ->
    parse_transform_attr_1(R, [m3x2_skew(0.0, float(Y) / 180.0 * math:pi())|M]);

parse_transform_attr_1([_Unknown | R], M) ->
    parse_transform_attr_1(R, M);

parse_transform_attr_1([], MList) ->
    {A0,A1,A2, A3,A4,A5} = m3x2_concat(MList),
    {matrix, float(A0), float(A1), float(A2), float(A3), float(A4), float(A5)}.

%% 3x2 Matrix transformations
m3x2_mat() ->
    { 1.0, 0.0, 0.0,
      1.0, 0.0, 0.0 }.
m3x2_translate(X, Y)
  when is_float(X), is_float(Y) ->
    {1.0, 0.0, 0.0, 1.0, X, Y }.
m3x2_scale(X, Y)
  when is_float(X), is_float(Y) ->
    {X, 0.0, 0.0, Y, 0.0, 0.0}.
m3x2_rotate(Ang)
  when is_float(Ang) ->
    {math:cos(Ang), -math:sin(Ang), math:sin(Ang), math:cos(Ang), 0.0, 0.0}.
m3x2_skew(X, Y)
  when is_float(X), is_float(Y) ->
    {1.0, math:tan(Y), math:tan(X), 1.0, 0.0, 0.0}.

m3x2_combine(L,[Mat]) ->
    m3x2_combine(L,Mat);
m3x2_combine(L,Mat) ->
    m3x2_combine_1([M || M <- L, M =/= none],Mat).
m3x2_combine_1([],none) -> [];
m3x2_combine_1([],Mat) when is_tuple(Mat) -> [Mat];
m3x2_combine_1(L,none) when is_list(L) -> L;
m3x2_combine_1(L,Mat2) -> L ++ [Mat2].

m3x2_concat(L) ->
    lists:foldl(fun (M1, A) -> m3x2_mul(M1, A) end, m3x2_mat(), L).
m3x2_mul({A11,A21,A12,A22,A13,A23}=_MA,{B11,B21,B12,B22,B13,B23}=_MB) ->
    B31=B32=0.0,
    B33=1.0,
    {C11,C12,C31,C21,C22,C32} =
        {A11*B11 + A12*B21 + A13*B31 , A11*B12 + A12*B22 + A13*B32 , A11*B13 + A12*B23 + A13*B33 ,
         A21*B11 + A22*B21 + A23*B31 , A21*B12 + A22*B22 + A23*B32 , A21*B13 + A22*B23 + A23*B33 },
    {C11,C21,C12,C22,C31,C32}.



parse_image_transform(A) ->
    case parse_transform_attr(A) of
        {ok, {matrix, A0, A1, A2, A3, A4, A5}} ->
            {ok, {matrix, float(A0), float(A1), float(A2), float(A3), float(A4), float(A5)}}
    end.

%%% Tokenize SVG attribute contents
%%%
-define(CHR_SPACE, 32).
-define(CHR_DQUOT, 34).

tok_svg_attr(A) ->
    {ok, List, _} = tok_svg_attr(A, [], []),
    {ok, List}.
tok_svg_attr([], Part2, List) when length(Part2) > 0 ->
    tok_svg_attr([], [], [tok_svg_attr_number_or_kw(Part2) | List]);
tok_svg_attr([], [], List) ->
    {ok, lists:reverse(List), []};
tok_svg_attr([Op | _]=R, Part2, List)
  when length(Part2) > 0,
        ( Op =:= ?CHR_SPACE orelse Op =:= $, orelse
          Op =:= $; orelse Op =:= ?CHR_DQUOT orelse
          Op =:= $' orelse Op =:= $( orelse
          Op =:= $) ) ->
    tok_svg_attr(R, [], [tok_svg_attr_number_or_kw(Part2) | List]);
tok_svg_attr([$( | R], [], List) ->
    tok_svg_attr(R, [], [open | List]);
tok_svg_attr([$) | R], [], List) ->
    tok_svg_attr(R, [], [close | List]);
tok_svg_attr([$, | R], [], List) ->
    tok_svg_attr(R, [], List);
tok_svg_attr([$; | R], [], List) ->
    {ok, lists:reverse(List), R};
tok_svg_attr([?CHR_SPACE | R], [], List) ->
    tok_svg_attr(R, [], List);

tok_svg_attr([?CHR_DQUOT | R], [], List) ->
    {StringVal, R_1} = tok_inside_dqstring(R),
    tok_svg_attr(R_1, [], [StringVal | List]);
tok_svg_attr([$' | R], [], List) ->
    {StringVal, R_1} = tok_inside_sqstring(R),
    tok_svg_attr(R_1, [], [StringVal | List]);

tok_svg_attr([A | R], Part2, List)
  when A =/= $: , A =/= $; ->
    tok_svg_attr(R, [A | Part2], List).

%% Tokenizing inside a double quote string that could
%% be inside an XML attribute of an SVG element.
%%
tok_inside_dqstring(A) -> tok_inside_dqstring(A, []).
tok_inside_dqstring([], AL) -> {iolist_to_binary(lists:reverse(AL)), []};
tok_inside_dqstring([BS, EscChar | Rest], AL) when BS =:= 92 ->
    tok_inside_dqstring(Rest, [EscChar|AL]);
tok_inside_dqstring([DQ | Rest], AL) when DQ =:= ?CHR_DQUOT ->
    {iolist_to_binary(lists:reverse(AL)), Rest};
tok_inside_dqstring([Char | Rest], AL) ->
    tok_inside_dqstring(Rest, [Char|AL]).

%% Tokenizing inside a single quote string that could
%% be inside an XML attribute of an SVG element.
%%
tok_inside_sqstring(A) -> tok_inside_sqstring(A, []).
tok_inside_sqstring([], AL) -> {iolist_to_binary(lists:reverse(AL)), []};
tok_inside_sqstring([BS, EscChar | Rest], AL) when BS =:= 92 ->
    tok_inside_sqstring(Rest, [EscChar|AL]);
tok_inside_sqstring([SQ | Rest], AL) when SQ =:= $' ->
    {iolist_to_binary(lists:reverse(AL)), Rest};
tok_inside_sqstring([Char | Rest], AL) ->
    tok_inside_sqstring(Rest, [Char|AL]).
    
number_val_unit(Val, "") ->
    Val;
number_val_unit(Val, Unit) ->
    {Val, Unit}.
    

tok_svg_attr_number_or_kw(A) ->
    tok_svg_attr_number_or_kw_1(lists:reverse(A)).
tok_svg_attr_number_or_kw_1([$.|R]) -> tok_svg_attr_number_or_kw_1([$0,$.|R]);
tok_svg_attr_number_or_kw_1([A0 | _] = Num_S) when (A0 >= $0 andalso A0 =< $9) orelse A0 =:= $- ->
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
tok_svg_attr_number_or_kw_1(NotNumber) ->
    NotNumber.

%%%
%%% SVG Definitions for paths
%%%

path_definitions(CmdLst, DocSt) ->
    path_definitions(CmdLst, DocSt, [], []).
path_definitions([], DocSt, Defs, OCmdLst) ->
    {ok, OCmds_1} = svgtags(lists:reverse(OCmdLst), DocSt),
    {ok, {Defs, OCmds_1}};
path_definitions([Cmd|CmdLst_1], DocSt, Defs, OCmdLst) ->
    case Cmd of
        {s,"defs",_Attrs} ->
            {ok, Defs_1, CmdLst_2} = path_definitions_indefs(CmdLst_1, Defs, DocSt),
            path_definitions(CmdLst_2, DocSt, Defs_1, OCmdLst);
        {e,"defs"} ->
            %% Already outside definitions, just remove this
            path_definitions(CmdLst_1, DocSt, Defs, OCmdLst);
        _ ->
            %% Outside definition other commands are just added along to the
            %% output list.
            path_definitions(CmdLst_1, DocSt, Defs, [Cmd|OCmdLst])
    end.
    
path_definitions_indefs([Cmd|CmdLst_1], Defs, DocSt) ->
    case Cmd of
        {e,"defs"} ->
            {ok, Defs, CmdLst_1};
        {s,ClipLName, Attrs} when
            ClipLName =:= "clippath";
            ClipLName =:= "mask"
        ->
            {ok, Paths, CmdLst_2} = path_definitions_clippath(CmdLst_1, ClipLName, DocSt),
            Id = proplists:get_value(id, Attrs, "none"),
            Defs_1 = orddict:store(Id, {Attrs, Paths}, Defs),
            path_definitions_indefs(CmdLst_2, Defs_1, DocSt);
        
        {s,"defs", _Attrs} ->
            %% Already inside definitions, just remove this
            path_definitions_indefs(CmdLst_1, Defs, DocSt);
        _ ->
            path_definitions_indefs(CmdLst_1, Defs, DocSt)
    end.

%% We are treating clip paths and masks the same way.
path_definitions_clippath(CmdLst, Which, DocSt) ->
    path_definitions_clippath(CmdLst, Which, DocSt, []).
path_definitions_clippath([Cmd|CmdLst_1], Which, DocSt, OCmds) ->
    case Cmd of
        {e,Which} ->
            {ok, OCmds_1} = svgtags(lists:reverse(OCmds), DocSt),
            {ok, OCmds_1, CmdLst_1};
        {s,Which, _Attrs} ->
            %% Already inside definitions, just remove this
            path_definitions_clippath(CmdLst_1, Which, DocSt, OCmds);
        _ ->
            path_definitions_clippath(CmdLst_1, Which, DocSt, [Cmd|OCmds])
    end.

    

%%
%%

svgtags_enter_layer(Attr) ->
    Label = proplists:get_value(label, Attr, ""),
    STrn = proplists:get_value(transform, Attr, none),
    {T, LayerName} = layerdir(Label),
    {layer_s, T, LayerName, STrn}.

svgtags_exit_layer() ->
    {layer_e}.
    

svgtags(CmdLst, DocSt) ->
    svgtags(CmdLst, DocSt, []).
svgtags([], _DocSt, OCmdLst) ->
    {ok, lists:reverse(OCmdLst)};
svgtags([Cmd|CmdLst], DocSt, OCmdLst) ->
    case Cmd of
        {s,"g",Attr} ->
            LayerCmd = svgtags_enter_layer(Attr),
            svgtags(CmdLst, DocSt, [LayerCmd|OCmdLst]);
        {e,"g"} ->
            LayerCmd = svgtags_exit_layer(),
            svgtags(CmdLst, DocSt, [LayerCmd|OCmdLst]);
    
        {s,"image",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_image/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"image"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"path",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_path/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"path"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"polyline",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_polyline/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"polyline"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"polygon",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_polygon/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"polygon"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"rect",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_rect/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"rect"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"circle",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_circle/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"circle"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        {s,"ellipse",Attr} ->
            {CmdLst_1, OCmdLst_1} = shape_element(fun svgtags_ellipse/2, DocSt, Attr, CmdLst, OCmdLst),
            svgtags(CmdLst_1, DocSt, OCmdLst_1);
        {e,"ellipse"} ->
            svgtags(CmdLst, DocSt, OCmdLst);
        _ ->
            svgtags(CmdLst, DocSt, OCmdLst)
    end.

-record(svgtag_s,
    {
    attr :: [any()],
    fullfilename :: string(),
    units :: svg_units(),
    docsz :: {float(), float()},
    viewbox :: {float(), float(), float(), float()}
    }).

shape_element(Fun, #document_s{fullfilename=FullFilename,units=Units,docsz=DocSz,viewbox=ViewBox}=_DocSt, Attr, CmdLst, OCmdLst) ->
    case determine_hidden_element(Attr) of
        false ->
            {ok, PathCmd, CmdLst_1} = Fun(#svgtag_s{attr=Attr,fullfilename=FullFilename,units=Units,docsz=DocSz,viewbox=ViewBox}, CmdLst),
            {CmdLst_1, [PathCmd|OCmdLst]};
        true ->
            {CmdLst, OCmdLst}
    end.

-spec merge_color_to_style([{atom(),any()}]) -> #style_colors{}.
merge_color_to_style(Attr) ->
    case get_colors_from_attrs(Attr) of
        {SCol, FCol, FOpa} ->
            #style_colors{scol=SCol,fcol=FCol,fopa=FOpa}
    end.

get_colors_from_attrs(Attr) ->
    #style_colors{scol=SColInStyle,fcol=FColInStyle,fopa=FOpaInStyle} =
        proplists:get_value(style, Attr,
            #style_colors{scol=inherit,fcol=inherit,fopa=inherit}),
    case proplists:get_value(style_stroke, Attr, inherit) of
        inherit ->
            case SColInStyle of
                inherit -> SCol = opaque;
                SColVal -> SCol = SColVal
            end;
        SColVal -> SCol = SColVal
    end,
    case proplists:get_value(style_fill, Attr, inherit) of
        inherit ->
            case FColInStyle of
                inherit -> FCol = {1.0, 1.0, 1.0};
                FColVal -> FCol = FColVal
            end;
        FColVal -> FCol = FColVal
    end,
    case proplists:get_value(style_fillopa, Attr, inherit) of
        inherit ->
            case FOpaInStyle of
                inherit -> FOpa = 1.0;
                FOpaVal -> FOpa = FOpaVal
            end;
        FOpaVal -> FOpa = FOpaVal
    end,
    {SCol, FCol, FOpa}.

%% Determine if the element is hidden and should not be added as
%% a shape to import.
%%
determine_hidden_element(Attr) ->
    case get_colors_from_attrs(Attr) of
        {none, none, _} ->
            %% This is an element that cannot be seen, some drawing software
            %% use this to make a bounding box.
            true;
        _ ->
            false
    end.

%%%
%%% Unit conversion
%%%

%% Inkscape switched from an assumed 90 dpi to 96 dpi.
-define(ASSUMED_PX_DPI, 96.0).

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
        "%" -> perc;
        
        %% Convenience units for larger models
        "dm" -> dm;
        "m" -> meter;
        "ft" -> ft;
        "yd" -> yd;

        _ -> px  %% The default for user unit is pixels.
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
unit_ratio(Unit1, Unit2) when
    Unit1 =:= Unit2 ->
    1.0;

%% Use pt for the physical units on the document unit side.
unit_ratio(Unit1, Unit2) when
    Unit2 =/= px, Unit2 =/= perc, Unit2 =/= pt ->
    unit_ratio(Unit1, pt) / unit_scaled_pt(Unit2);

%% Physical units on both side
unit_ratio(Unit1, pt) when
    Unit1 =/= px, Unit1 =/= perc ->
    unit_scaled_pt(Unit1);

%% Physical unit document, pixel unit shape
unit_ratio(px, pt) ->
    unit_scaled_pt(in) / ?ASSUMED_PX_DPI;

%% Pixel unit document, physical unit shape
unit_ratio(Unit1, px) ->
    (?ASSUMED_PX_DPI / unit_scaled_pt(in)) * unit_scaled_pt(Unit1).

%% User units
conv_unit(Num, DocUnit) when is_float(Num) ->
    conv_unit({Num, user}, DocUnit);
conv_unit(Num, DocUnit) when is_integer(Num) ->
    conv_unit({float(Num), user}, DocUnit);
conv_unit({Num, user}, _) when is_float(Num); is_integer(Num) ->
    Num * 1.0;

%% Unsupported percent units
conv_unit({Num, Unit}, perc) when is_float(Num), is_atom(Unit) ->
    Num * 1.0;
conv_unit({Num, perc}, DocUnit) when is_float(Num), is_atom(DocUnit) ->
    Num * 1.0;

conv_unit({Num, Unit}, DocUnit) when is_float(Num), is_atom(Unit), is_atom(DocUnit) ->
    Num * unit_ratio(Unit, DocUnit);
conv_unit({Num, Unit}, DocUnit) when is_float(Num), is_list(Unit) ->
    conv_unit({Num, unit_atom(Unit)}, DocUnit);
conv_unit({Num, Unit}, DocUnit) when is_integer(Num) ->
    conv_unit({float(Num), Unit}, DocUnit).


viewbox_scale({Width,Height}=_DocSz,{_ViewBL,_ViewBT,ViewBW,ViewBH}=_ViewBox) ->
    {Width / ViewBW, Height / ViewBH}.

%%%
%%% SVG Tags
%%%


svgtags_image(SVT, CmdLst) ->
    svgtags_image(SVT, CmdLst, []).
svgtags_image(#svgtag_s{fullfilename=FullFilename,units=Units,docsz=_DocSz,viewbox=_ViewBox,attr=Attr}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"image"} ->
            %% Might need to use viewbox_scale/2 for unit'ed coordinates
            ClipPath = proplists:get_value(clippath, Attr, none),
            X = conv_unit(proplists:get_value(x, Attr, 0.0), Units),
            Y = conv_unit(proplists:get_value(y, Attr, 0.0), Units),
            Data = proplists:get_value(data, Attr, none),
            Width = conv_unit(proplists:get_value(width, Attr, 10.0), Units),
            Height = conv_unit(proplists:get_value(height, Attr, 10.0), Units),
            Transform = proplists:get_value(transform, Attr, {matrix,1.0,0.0,0.0,1.0,-0.1,-0.1}),
            Id = proplists:get_value(id, Attr, "none"),
            {ok, Bitmap} = get_bitmap(Data, FullFilename),
            ImgCmd = {image, #image_tag_r{
                clip_path = ClipPath,
                x = X,
                y = Y,
                width = Width,
                height = Height,
                transform = Transform,
                id = Id,
                data = Bitmap
            }, lists:reverse(OCmdLst)},
            {ok, ImgCmd, CmdLst};
            
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_image(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_image(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_image(SVT, CmdLst, OCmdLst)
    end.


svgtags_path(SVT, CmdLst) ->
    svgtags_path(SVT, CmdLst, []).
svgtags_path(#svgtag_s{attr=Attr}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"path"} ->
            D = proplists:get_value(d, Attr, ""),
            Id = proplists:get_value(id, Attr, "none"),
            STrn = proplists:get_value(transform, Attr, none),
            Style = merge_color_to_style(Attr),
            PathCmd = #path_tag_r{
                d = D,
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_path(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_path(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_path(SVT, CmdLst, OCmdLst)
    end.

    
svgtags_polyline(SVT, CmdLst) ->
    svgtags_polyline(SVT, CmdLst, []).
svgtags_polyline(#svgtag_s{attr=Attr}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"polyline"} ->
            Points = proplists:get_value(points, Attr, ""),
            Id = proplists:get_value(id, Attr, "none"),
            STrn = proplists:get_value(transform, Attr, none),
            Style = merge_color_to_style(Attr),
            PathCmd = #path_tag_r{
                d = svg_path_of_polyline(Points),
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_polyline(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_polyline(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_polyline(SVT, CmdLst, OCmdLst)
    end.
    
svgtags_polygon(SVT, CmdLst) ->
    svgtags_polygon(SVT, CmdLst, []).
svgtags_polygon(#svgtag_s{attr=Attr}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"polygon"} ->
            Points = proplists:get_value(points, Attr, ""),
            Id = proplists:get_value(id, Attr, "none"),
            Style = merge_color_to_style(Attr),
            STrn = proplists:get_value(transform, Attr, none),
            PathCmd = #path_tag_r{
                d = svg_path_of_polygon(Points),
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_polygon(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_polygon(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_polygon(SVT, CmdLst, OCmdLst)
    end.
    


svgtags_rect(SVT, CmdLst) ->
    svgtags_rect(SVT, CmdLst, []).
svgtags_rect(#svgtag_s{attr=Attr,units=Units,docsz=_DocSz,viewbox=_ViewBox}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"rect"} ->
            %% Might need to use viewbox_scale/2 for unit'ed coordinates
            X = conv_unit(proplists:get_value(x, Attr, 0.0), Units),
            Y = conv_unit(proplists:get_value(y, Attr, 0.0), Units),
            Width = conv_unit(proplists:get_value(width, Attr, 10.0), Units),
            Height = conv_unit(proplists:get_value(height, Attr, 10.0), Units),
            RY = conv_unit(proplists:get_value(ry, Attr, 0.0), Units),
            RX = conv_unit(proplists:get_value(rx, Attr, RY), Units),
            Id = proplists:get_value(id, Attr, "none"),
            Style = merge_color_to_style(Attr),
            STrn = proplists:get_value(transform, Attr, none),
            PathCmd = #path_tag_r{
                d = svg_path_of_rect(X, Y, Width, Height, RX, RY),
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_rect(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_rect(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_rect(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_rect(SVT, CmdLst, OCmdLst)
    end.
    
svgtags_circle(SVT, CmdLst) ->
    svgtags_circle(SVT, CmdLst, []).
svgtags_circle(#svgtag_s{attr=Attr,units=Units,docsz=_DocSz,viewbox=_ViewBox}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"circle"} ->
            %% Might need to use viewbox_scale/2 for unit'ed coordinates
            CX = conv_unit(proplists:get_value(cx, Attr, 0.0), Units),
            CY = conv_unit(proplists:get_value(cy, Attr, 0.0), Units),
            R  = conv_unit(proplists:get_value(r, Attr, 1.0), Units),
            Id = proplists:get_value(id, Attr, "none"),
            Style = merge_color_to_style(Attr),
            STrn = proplists:get_value(transform, Attr, none),
            PathCmd = #path_tag_r{
                d = svg_path_of_ellipse(CX, CY, R, R),
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_circle(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_circle(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_circle(SVT, CmdLst, OCmdLst)
    end.
    
svgtags_ellipse(SVT, CmdLst) ->
    svgtags_ellipse(SVT, CmdLst, []).
svgtags_ellipse(#svgtag_s{attr=Attr,units=Units,docsz=_DocSz,viewbox=_ViewBox}=SVT, [Cmd|CmdLst], OCmdLst) ->
    case Cmd of
        {e,"ellipse"} ->
            %% Might need to use viewbox_scale/2 for unit'ed coordinates
            CX = conv_unit(proplists:get_value(cx, Attr, 0.0), Units),
            CY = conv_unit(proplists:get_value(cy, Attr, 0.0), Units),
            RX = conv_unit(proplists:get_value(rx, Attr, 1.0), Units),
            RY = conv_unit(proplists:get_value(ry, Attr, 1.0), Units),
            Id = proplists:get_value(id, Attr, "none"),
            Style = merge_color_to_style(Attr),
            STrn = proplists:get_value(transform, Attr, none),
            PathCmd = #path_tag_r{
                d = svg_path_of_ellipse(CX, CY, RX, RY),
                id = Id,
                style = Style,
                ta = [STrn]
            },
            {ok, {path, PathCmd, lists:reverse(OCmdLst)}, CmdLst};
        
        {s,"image",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_image(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_ellipse(SVT, CmdLst_1, OCmdLst);
        {s,"path",Attr1} ->
            {ok, _, CmdLst_1} = svgtags_path(SVT#svgtag_s{attr=Attr1}, CmdLst),
            svgtags_ellipse(SVT, CmdLst_1, OCmdLst);
        _ ->
            svgtags_ellipse(SVT, CmdLst, OCmdLst)
    end.


%%%
%%%

%% Assign transforms and directives from layers to paths
%%
assign_transforms(CmdLst) ->
    assign_transforms(CmdLst, [], []).
assign_transforms([], _Transforms, OCmdLst) ->
    {ok, lists:reverse(OCmdLst)};
assign_transforms([Cmd|CmdLst], TrnList, OCmdLst) ->
    case Cmd of
        {layer_s, UserTransforms, LayerName, STrn} ->
            %% Layer start
            assign_transforms(CmdLst,
                [{UserTransforms, LayerName, STrn} | TrnList], OCmdLst);
        {layer_e} ->
            case TrnList of
                [_|TrnList_1] ->
                    %% Layer end
                    assign_transforms(CmdLst, TrnList_1, OCmdLst);
                [] ->
                    assign_transforms(CmdLst, TrnList, OCmdLst)
            end;
        {path, #path_tag_r{ut=List,ta=STrn_0}=PathTagR, B} ->
            case TrnList of
                [_ |_] ->
                    STrnL = lists:reverse([STrn_1 || {_, _LayerName, STrn_1} <- TrnList]),
                    UTrnL = lists:reverse([UserTransforms || {UserTransforms, _LayerName, _} <- TrnList]),
                    Cmd_1 = {path, PathTagR#path_tag_r{ut=List++lists:append(UTrnL),
                        ta=m3x2_combine(STrnL, STrn_0)}, B};
                [] ->
                    Cmd_1 = Cmd
            end,
            assign_transforms(CmdLst, TrnList, [Cmd_1|OCmdLst]);
        _OtherCmd ->
            assign_transforms(CmdLst, TrnList, [Cmd|OCmdLst])
    end.

%%%
%%%


data_url_dec(EncType, EncData) ->
    case string:to_lower(EncType) of
        "base64" ->
            base64:decode(EncData)
    end.
    

get_bitmap({abs, FilePath}, _FullFilename) ->
    get_bitmap_by_ext(FilePath);
get_bitmap({rel, FilePath_0}, FullFilename) ->
    %% Use FullFilename with the relative path.
    Dir = filename:dirname(FullFilename),
    FilePath = filename:absname_join(Dir, FilePath_0),
    get_bitmap_by_ext(FilePath);
get_bitmap({data, MimeType, {EncType, Data}}, _FullFilename) ->
    %% The two formats SVG supports are png and jpeg.
    case string:to_lower(MimeType) of
        "image/png" ->
            get_bitmap_png(MimeType, data_url_dec(EncType, Data));
        "image/jpeg" ->
            get_bitmap_jpeg(MimeType, data_url_dec(EncType, Data));
        _ ->
            {error, none}
    end.
get_bitmap_by_ext(FilePath) ->
    case string:to_lower(filename:extension(FilePath)) of
        ".jpeg" -> Ext = ".jpg";
        Ext_0   -> Ext = Ext_0
    end,
    case Ext of
        ".png" ->
            F = fun read_png/1;
        ".jpg" ->
            F = fun read_jpeg/1;
        _ ->
            F = none
    end,
    case F of
        none ->
            {error, none};
        _ ->
            F(FilePath)
    end.

get_bitmap_png(MimeType, BinData) ->
    binary_to_tempfile(MimeType, BinData, fun read_png/1).

get_bitmap_jpeg(MimeType, BinData) ->
    binary_to_tempfile(MimeType, BinData, fun read_jpeg/1).

read_jpeg(FileName) ->
    BlockWxMsgs = wxLogNull:new(),
    Ret = read_jpeg_1(FileName),
    wxLogNull:destroy(BlockWxMsgs),
    Ret.
read_jpeg_1(FileName) ->
    Image = wxImage:new(),
    case wxImage:loadFile(Image, FileName) of
        true ->
            E3d = wings_image:wxImage_to_e3d(Image),
            wxImage:destroy(Image),
            {ok, e3d_image:fix_outtype(FileName, E3d, [])};
        false ->
            {error, none}
    end.
read_png(FileName) ->
    E3D = e3d__png:load(FileName),
    {ok, E3D}.
uniq_name(TmpDir, Ext) ->
    FileIdNum = abs(erlang:unique_integer()),
    FileId = "w3d_svg_" ++ integer_to_list(FileIdNum) ++ Ext,
    TempFile = filename:join(TmpDir, FileId),
    case file:read_file_info(TempFile) of
        {ok, _} ->
            uniq_name(TmpDir, Ext);
        _ ->
            TempFile
    end.
binary_to_tempfile(MimeType, Bin,F) ->
    case MimeType of
        "image/jpeg" ->
            Ext = ".jpg";
        "image/png" ->
            Ext = ".png";
        _ ->
            Ext = ".tmp"
    end,
    TmpDir = wings_u:basedir(user_cache),
    TempFile = uniq_name(TmpDir, Ext),
    case file:write_file(TempFile, Bin) of
        ok ->
            Ret = F(TempFile),
            file:delete(TempFile),
            Ret;
        _ ->
            {error, none}
    end.



%% Bring together the SVG paths and the paths in the definitions by
%% substituting the images for the paths.
%%
retr_d_svg_paths(Definitions, CmdLst, ShortFilename) ->
    retr_d_svg_paths(Definitions, CmdLst, ShortFilename, []).
retr_d_svg_paths(_Definitions, [], _ShortFilename, OCmdLst) ->
    {ok, lists:reverse(OCmdLst)};
retr_d_svg_paths(Definitions, [Cmd|CmdLst], ShortFilename, OCmdLst) ->
    case Cmd of
        {image,#image_tag_r{clip_path=ClipPath}=ImageInfo,_ImageSubCmds} when ClipPath =/= none ->
            %% A clipped image, get the corresponding paths.
            {_, PathCmds} = orddict:fetch(ClipPath, Definitions),
            Atom = list_to_atom("svg_tex_" ++ integer_to_list(abs(erlang:unique_integer()))),
            Cmds_1 = [ {path, A#path_tag_r{texture={tex, Atom}}, B} || {path, A, B} <- PathCmds],
            %% Attach information about the texture, with Atom as its texture name
            OCmdLst_1 = lists:reverse(Cmds_1) ++ [{new_tex, Atom, ImageInfo}|OCmdLst],
            retr_d_svg_paths(Definitions, CmdLst, ShortFilename, OCmdLst_1);
        {image,#image_tag_r{clip_path=none}=_ImageInfo,_ImageSubCmds} ->
            %% A loose image was found without a mask or a clip path, ignore
            io:format(?__(1, "SVG Import Warning~n"), []),
            io:format(?__(2, 
                "~p: NOTE: Loose image without any clip path "
                "or mask found.~n"), [?MODULE]),
            retr_d_svg_paths(Definitions, CmdLst, ShortFilename, OCmdLst);
         
        %% Add to output list
        {path, _, _} ->
            retr_d_svg_paths(Definitions, CmdLst, ShortFilename, [Cmd|OCmdLst]);
        {layer_s, _, _, _} ->
            retr_d_svg_paths(Definitions, CmdLst, ShortFilename, [Cmd|OCmdLst]);
        {layer_e} ->
            retr_d_svg_paths(Definitions, CmdLst, ShortFilename, [Cmd|OCmdLst])
    end.


%%
%% SVG Path tokenizer
%% Tokenizes SVG paths even if they are minified with Inkscape's
%% Optimized SVG setting. This handles cases where spaces between
%% numbers are omitted when a minus sign is used as the beginning of
%% a new number.
%%

tok_svgp(Str) ->
    {ok, T} = tok_svgp(Str, [], []),
    T.
tok_svgp([], [], O) ->
    {ok, lists:reverse(O)};
tok_svgp([], W, O) ->
    tok_svgp([], [], [lists:reverse(W)|O]);
tok_svgp([Chr | Rest], W, O)
  when Chr >= $A, Chr =< $D;
       Chr >= $a, Chr =< $d;
       Chr >= $F, Chr =< $Z;
       Chr >= $f, Chr =< $z ->
    tok_svgp(Rest, [Chr|W], O);
tok_svgp([WSp | Rest], W, O)
  when WSp =:= ?CHR_SPACE;
       WSp =:= 7;
       WSp =:= 9;
       WSp =:= 10;
       WSp =:= 13; WSp =:= $,  ->
    case W of
        [] -> tok_svgp(Rest, [], O);
        _  -> tok_svgp(Rest, [], [lists:reverse(W) | O])
    end;
tok_svgp([Chr | _]=Num, W, O)
  when (Chr >= $0 andalso Chr =< $9);
        Chr =:= $.; Chr =:= $-;
        Chr =:= $e; Chr =:= $E ->
    case W of
        [] ->
            {ok, Rest_1, Number} = tok_svgp_num_initial(Num),
            tok_svgp(Rest_1, [], [Number|O]);
        _ ->
            tok_svgp(Num, [], [lists:reverse(W)|O])
    end.

%% Some cases where a dot appears without a zero before it,
%% string:to_float and string:to_integer don't like this so
%% we add a zero before the dot.
%%
tok_svgp_num_initial([Min, Dot | Rest])
  when Min =:= $-, Dot =:= $. ->
    tok_svgp_num_initial([Min, $0, Dot | Rest]);
tok_svgp_num_initial([Dot | Rest])
  when Dot =:= $. ->
    tok_svgp_num_initial([$0, Dot | Rest]);
tok_svgp_num_initial([Chr | Rest])
  when Chr >= $0, Chr =< $9;
       Chr =:= $.; Chr =:= $-;
       Chr =:= $e; Chr =:= $E ->
    tok_svgp_num(Rest, [Chr]).
tok_svgp_num([Chr,$- | Rest], W)
  when Chr =:= $e; Chr =:= $E ->
    tok_svgp_num(Rest, [$-,Chr|W]);
tok_svgp_num([Chr | Rest], W)
  when Chr >= $0, Chr =< $9;
       Chr =:= $.;
       Chr =:= $e; Chr =:= $E ->
    tok_svgp_num(Rest, [Chr|W]);
tok_svgp_num(NotNumber, W) ->
    {ok, NotNumber, lists:reverse(W)}.



%% Tokenizer for polygon points
%%
tok_polypoints(Points) ->
    %% We use the svg path tokenizer since the main delimiters are spaces
    %% and commas, but don't use the implied lineto inserter functionality.
    %% So we use tok_svgp/3 directly.
    %%
    {ok, T} = tok_svgp(Points, [], []),
    T.


polypoints_to_list(List) ->
    polypoints_to_list(List, []).
polypoints_to_list([], O) ->
    lists:reverse(O);
polypoints_to_list([X, Y | Rest], O) ->
    polypoints_to_list(Rest, [
        {parse_float_number_no_unit(X, 0.0),
         parse_float_number_no_unit(Y, 0.0)}|O]).

%%
%% Create a substitute SVG path string that represent a rect, circle
%% ellipse, polyline or polygon.
%%

svg_path_of_polyline(Points_0) ->
    %% Determine if the polyline is closed by start and end points
    %% being the same
    %%
    wr_svg_path(polypoints_to_list(tok_polypoints(Points_0))).
    
svg_path_of_polygon(Points_0) ->
    %% The path is always closed.
    wr_svg_path(polypoints_to_list(tok_polypoints(Points_0))).

svg_path_of_rect(X,Y,W,H,RX,RY) when RX > 0.0; RY > 0.0 ->
    wr_svg_path(paths_round_rect(X,Y,X+W,Y+H,RX,RY));
svg_path_of_rect(X,Y,W,H,_RX,_RY) ->
    wr_svg_path(paths_rect(X,Y,X+W,Y+H)).


svg_path_of_ellipse(CX,CY,RX,RY) ->
    X1 = CX - RX,
    Y1 = CY - RY,
    X2 = CX + RX,
    Y2 = CY + RY,
    wr_svg_path(paths_round_rect(X1,Y1,X2,Y2,(X2-X1)/2.0,(Y2-Y1)/2.0)).

%% Used by <rect>, <polygon>, <polyline>, <circle>, <ellipse> to create SVG
%% path strings from list of coordinates.
%%
wr_svg_path([]) -> [];
wr_svg_path([{X,Y}|R]) ->
    "M" ++ lists:flatten(io_lib:format("~p,~p", [X,Y])) ++ " " ++ wr_svg_path_2(R);
wr_svg_path([{X,Y,_,_}|R]) ->
    "M" ++ lists:flatten(io_lib:format("~p,~p", [X,Y])) ++ " " ++ wr_svg_path_2(R).
wr_svg_path_2([]) ->
    "Z";
wr_svg_path_2([{X,Y,{XC1,YC1},{XC2,YC2}}|R]) ->
    "C" ++ lists:flatten(io_lib:format("~p,~p ~p,~p ~p,~p", [XC1,YC1,XC2,YC2,X,Y])) ++ " " ++ wr_svg_path_2(R);
wr_svg_path_2([{X,Y}|R]) ->
    "L" ++ lists:flatten(io_lib:format("~p,~p", [X,Y])) ++ " " ++ wr_svg_path_2(R).
    
%%
%% Rounded rectangles and circles
%% Same functions as found in wpc_wmf_file_paths.erl
%%
aroundCos(I, RH, Max) ->
    RH * math:cos(((I / Max) * math:pi() * 0.5)).
aroundSin(I, RH, Max) ->
    RH * math:sin(((I / Max) * math:pi() * 0.5)).
    
stepped_curve(Start, End, Acc, F) -> stepped_curve(Start, End, Acc, F, Start).
stepped_curve(Start, End, Acc, F, I) when I >= Start andalso I < End ->
    Acc_2 = F(I, Acc),
    stepped_curve(Start, End, Acc_2, F, I+1);
stepped_curve(_, _, Acc, _, _) -> Acc.


paths_rect(X1, Y1, X2, Y2) when X1 =:= X2; Y1 =:= Y2 ->
    [];
paths_rect(X1, Y1, X2, Y2) when X1 > X2 ->
    paths_rect(X2, Y1, X1, Y2);
paths_rect(X1, Y1, X2, Y2) when Y1 > Y2 ->
    paths_rect(X1, Y2, X2, Y1);
paths_rect(X1, Y1, X2, Y2) ->
    [{X1,Y1}, {X2,Y1}, {X2,Y2}, {X1,Y2}].

%% When paths_round_rect is making a shape with a radius that
%% makes the shape a ellipse (the amount of space between round
%% corners is very close to zero), remove the last point.
%%
paths_round_rect_middle([_|P],X) when X < 0.001 ->
    P;
paths_round_rect_middle(P,_) ->
    P.

paths_round_rect(X1, Y1, X2, Y2, _RW, _RH) when X1 =:= X2; Y1 =:= Y2 ->
    [];
paths_round_rect(X1, Y1, X2, Y2, RW, RH) when X1 > X2 ->
    paths_round_rect(X2, Y1, X1, Y2, RW, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH) when Y1 > Y2 ->
    paths_round_rect(X1, Y2, X2, Y1, RW, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH) when RW*2.0 > (X2-X1) ->
    paths_round_rect(X1, Y1, X2, Y2, X2-X1, RH);
paths_round_rect(X1, Y1, X2, Y2, RW, RH) when RH*2.0 > (Y2-Y1) ->
    paths_round_rect(X1, Y1, X2, Y2, RW, Y2-Y1);
paths_round_rect(X1, Y1, X2, Y2, RW, RH)
  when X1 < X2, Y1 < Y2 ->
    Max = 4,
    %% A small bump outward of control points so it is rounder
    BumpUp = 1.0 + math:pow(0.3, Max),
    RSW = (X2 - X1) - RW,
    RSH = (Y2 - Y1) - RH,
    %% Add a point to start at that isn't the closing point
    Path_0 = [{X1 + RSW + aroundSin(0, RW, Max),
               Y1 + (RH - aroundCos(0, RH, Max))}],
    Path_1_1 = stepped_curve(1, Max+1, Path_0, fun(I, Paths) ->
        XA = X1 + RSW + aroundSin(I, RW, Max),
        YA = Y1 + (RH - aroundCos(I, RH, Max)),
        XCA = X1 + RSW + aroundSin(I - 0.5, RW, Max),
        YCA = Y1 + (RH - aroundCos(I - 0.5, RH * BumpUp, Max)),
        XDA = X1 + RSW + aroundSin(I - 0.5, RW * BumpUp, Max),
        YDA = Y1 + (RH - aroundCos(I - 0.5, RH, Max)),
        C1 = {XCA, YCA},
        C2 = {XDA, YDA},
        [{XA, YA, C1, C2}|Paths]
    end),
    Path_1 = paths_round_rect_middle(Path_1_1, RSH - RH),
    Path_2_1 = stepped_curve(0, Max+1, Path_1, fun(I, Paths) ->
        XA = X1 + RSW + aroundCos(I, RW, Max),
        YA = Y1 + RSH + aroundSin(I, RH, Max),
        XCA = X1 + RSW + aroundCos(I - 0.5, RW * BumpUp, Max),
        YCA = Y1 + RSH + aroundSin(I - 0.5, RH, Max),
        XDA = X1 + RSW + aroundCos(I - 0.5, RW, Max),
        YDA = Y1 + RSH + aroundSin(I - 0.5, RH * BumpUp, Max),
        C1 = {XCA, YCA},
        C2 = {XDA, YDA},
        [{XA, YA, C1, C2}|Paths]
    end),
    Path_2 = paths_round_rect_middle(Path_2_1, RSW - RW),
    Path_3_1 = stepped_curve(0, Max+1, Path_2, fun(I, Paths) ->
        XA = X1 + RW - aroundSin(I, RW, Max),
        YA = Y1 + RSH + aroundCos(I, RH, Max),
        XCA = X1 + RW - aroundSin(I - 0.5, RW, Max),
        YCA = Y1 + RSH + aroundCos(I - 0.5, RH * BumpUp, Max),
        XDA = X1 + RW - aroundSin(I - 0.5, RW * BumpUp, Max),
        YDA = Y1 + RSH + aroundCos(I - 0.5, RH, Max),
        C1 = {XCA, YCA},
        C2 = {XDA, YDA},
        [{XA, YA, C1, C2}|Paths]
    end),
    Path_3 = paths_round_rect_middle(Path_3_1, RSH - RH),
    Path_4 = stepped_curve(0, Max+1, Path_3, fun(I, Paths) ->
        XA = X1 + RW - aroundSin(Max - I, RW, Max),
        YA = Y1 + RH - aroundCos(Max - I, RH, Max),
        XCA = X1 + RW - aroundSin(Max - (I - 0.5), RW * BumpUp, Max),
        YCA = Y1 + RH - aroundCos(Max - (I - 0.5), RH, Max),
        XDA = X1 + RW - aroundSin(Max - (I - 0.5), RW, Max),
        YDA = Y1 + RH - aroundCos(Max - (I - 0.5), RH * BumpUp, Max),
        C1 = {XCA, YCA},
        C2 = {XDA, YDA},
        [{XA, YA, C1, C2}|Paths]
    end),
    lists:reverse(Path_4).


%%
%% List of X11 web color names, in case the color is not a hex color.
%%
x11_color_names(S) ->
    case lists:filter(
            fun (C) when C >= $a, C =< $z -> true; (_) -> false end,
            string:to_lower(S))
    of
        "aliceblue" -> {240,248,255};
        "antiquewhite" -> {250,235,215};
        "aquamarine" -> {127,255,212};
        "azure" -> {240,255,255};
        "beige" -> {245,245,220};
        "bisque" -> {255,228,196};
        "black" -> {0,0,0};
        "blanchedalmond" -> {255,235,205};
        "blue" -> {0,0,255};
        "blueviolet" -> {138,43,226};
        "brown" -> {165,42,42};
        "burlywood" -> {222,184,135};
        "cadetblue" -> {95,158,160};
        "chartreuse" -> {127,255,0};
        "chocolate" -> {210,105,30};
        "coral" -> {255,127,80};
        "cornflowerblue" -> {100,149,237};
        "cornsilk" -> {255,248,220};
        "cyan" -> {0,255,255};
        "darkblue" -> {0,0,139};
        "darkcyan" -> {0,139,139};
        "darkgoldenrod" -> {184,134,11};
        "darkgray" -> {169,169,169};
        "darkgreen" -> {0,100,0};
        "darkgrey" -> {169,169,169};
        "darkkhaki" -> {189,183,107};
        "darkmagenta" -> {139,0,139};
        "darkolivegreen" -> {85,107,47};
        "darkorange" -> {255,140,0};
        "darkorchid" -> {153,50,204};
        "darkred" -> {139,0,0};
        "darksalmon" -> {233,150,122};
        "darkseagreen" -> {143,188,143};
        "darkslateblue" -> {72,61,139};
        "darkslategray" -> {47,79,79};
        "darkslategrey" -> {47,79,79};
        "darkturquoise" -> {0,206,209};
        "darkviolet" -> {148,0,211};
        "deeppink" -> {255,20,147};
        "deepskyblue" -> {0,191,255};
        "dimgray" -> {105,105,105};
        "dimgrey" -> {105,105,105};
        "dodgerblue" -> {30,144,255};
        "firebrick" -> {178,34,34};
        "floralwhite" -> {255,250,240};
        "forestgreen" -> {34,139,34};
        "gainsboro" -> {220,220,220};
        "ghostwhite" -> {248,248,255};
        "gold" -> {255,215,0};
        "goldenrod" -> {218,165,32};
        "gray" -> {190,190,190};
        "green" -> {0,255,0};
        "greenyellow" -> {173,255,47};
        "grey" -> {190,190,190};
        "honeydew" -> {240,255,240};
        "hotpink" -> {255,105,180};
        "indianred" -> {205,92,92};
        "ivory" -> {255,255,240};
        "khaki" -> {240,230,140};
        "lavender" -> {230,230,250};
        "lavenderblush" -> {255,240,245};
        "lawngreen" -> {124,252,0};
        "lemonchiffon" -> {255,250,205};
        "lightblue" -> {173,216,230};
        "lightcoral" -> {240,128,128};
        "lightcyan" -> {224,255,255};
        "lightgoldenrod" -> {238,221,130};
        "lightgoldenrodyellow" -> {250,250,210};
        "lightgray" -> {211,211,211};
        "lightgreen" -> {144,238,144};
        "lightgrey" -> {211,211,211};
        "lightpink" -> {255,182,193};
        "lightsalmon" -> {255,160,122};
        "lightseagreen" -> {32,178,170};
        "lightskyblue" -> {135,206,250};
        "lightslateblue" -> {132,112,255};
        "lightslategray" -> {119,136,153};
        "lightslategrey" -> {119,136,153};
        "lightsteelblue" -> {176,196,222};
        "lightyellow" -> {255,255,224};
        "limegreen" -> {50,205,50};
        "linen" -> {250,240,230};
        "magenta" -> {255,0,255};
        "maroon" -> {176,48,96};
        "mediumaquamarine" -> {102,205,170};
        "mediumblue" -> {0,0,205};
        "mediumorchid" -> {186,85,211};
        "mediumpurple" -> {147,112,219};
        "mediumseagreen" -> {60,179,113};
        "mediumslateblue" -> {123,104,238};
        "mediumspringgreen" -> {0,250,154};
        "mediumturquoise" -> {72,209,204};
        "mediumvioletred" -> {199,21,133};
        "midnightblue" -> {25,25,112};
        "mintcream" -> {245,255,250};
        "mistyrose" -> {255,228,225};
        "moccasin" -> {255,228,181};
        "navajowhite" -> {255,222,173};
        "navy" -> {0,0,128};
        "navyblue" -> {0,0,128};
        "oldlace" -> {253,245,230};
        "olivedrab" -> {107,142,35};
        "orange" -> {255,165,0};
        "orangered" -> {255,69,0};
        "orchid" -> {218,112,214};
        "palegoldenrod" -> {238,232,170};
        "palegreen" -> {152,251,152};
        "paleturquoise" -> {175,238,238};
        "palevioletred" -> {219,112,147};
        "papayawhip" -> {255,239,213};
        "peachpuff" -> {255,218,185};
        "peru" -> {205,133,63};
        "pink" -> {255,192,203};
        "plum" -> {221,160,221};
        "powderblue" -> {176,224,230};
        "purple" -> {160,32,240};
        "red" -> {255,0,0};
        "rosybrown" -> {188,143,143};
        "royalblue" -> {65,105,225};
        "saddlebrown" -> {139,69,19};
        "salmon" -> {250,128,114};
        "sandybrown" -> {244,164,96};
        "seagreen" -> {46,139,87};
        "seashell" -> {255,245,238};
        "sienna" -> {160,82,45};
        "skyblue" -> {135,206,235};
        "slateblue" -> {106,90,205};
        "slategray" -> {112,128,144};
        "slategrey" -> {112,128,144};
        "snow" -> {255,250,250};
        "springgreen" -> {0,255,127};
        "steelblue" -> {70,130,180};
        "tan" -> {210,180,140};
        "thistle" -> {216,191,216};
        "tomato" -> {255,99,71};
        "turquoise" -> {64,224,208};
        "violet" -> {238,130,238};
        "violetred" -> {208,32,144};
        "wheat" -> {245,222,179};
        "white" -> {255,255,255};
        "whitesmoke" -> {245,245,245};
        "yellow" -> {255,255,0};
        "yellowgreen" -> {154,205,50};
        _ -> none
    end.



%% Calculate a scale so the resulting mesh isn't huge.
%%
calculate_rescale_amount(MaxWidth, MaxHeight, Cmds) ->
    calculate_rescale_amount(MaxWidth, MaxHeight, Cmds, 1.0).
calculate_rescale_amount(_, _, [], Rescale) -> Rescale;
calculate_rescale_amount(MaxWidth, MaxHeight, [A|Cmds], Rescale) ->
    case A of
        List when is_list(List), length(List) > 0 ->
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
    
pathop_to_simple_list(Objs) ->
    [[ pathop_to_simple_list_1(Path) || Path <- SubPaths ] || {SubPaths,_Tex} <- Objs].
pathop_to_simple_list_1(#path{ops=OpsList}) ->
    [#pathop{x1=X1,y1=Y1} | List] = OpsList,
    pathop_to_simple_list_1(List, [{X1, Y1}]).
pathop_to_simple_list_1([], O) -> lists:reverse(O);
pathop_to_simple_list_1([#pathop{opkind=A, x1=X2, y1=Y2} | Rest], O) when A =:= plineto; A =:= pmoveto ->
    pathop_to_simple_list_1(Rest, [{X2, Y2} | O]);
pathop_to_simple_list_1([#pathop{opkind=pcurveto, x3=X2, y3=Y2} | Rest], O) ->
    pathop_to_simple_list_1(Rest, [{X2, Y2} | O]).

rescale(Scaler, Objs) ->
    [{rescale_1(Scaler, SubPaths, []),ColTex} || {SubPaths,ColTex} <- Objs].
rescale_1(_, [], OPaths) ->
    lists:reverse(OPaths);
rescale_1(Scaler, [A|Paths], OPaths) ->
    case A of
        #path{ops=List} when length(List) > 0 ->
            NewPath = A#path{ops=[rescale_pathop(Scaler, Op) || Op <- List]};
        Unk ->
            NewPath = Unk
    end,
    rescale_1(Scaler, Paths, [NewPath|OPaths]).
rescale_pathop({XS,YS}, PathOp=#pathop{x1=X1, y1=Y1, x2=X2, y2=Y2, x3=X3, y3=Y3}) ->
    PathOp#pathop{x1=X1*XS, y1=Y1*YS, x2=X2*XS, y2=Y2*YS, x3=X3*XS, y3=Y3*YS}.
rescale_coord({XS,YS}, {X,Y}) ->
    {XS * X, YS * Y}.


rescale_tex(Scaler, Texs) -> rescale_tex(Scaler, Texs, []).
rescale_tex(_, [], OTexs) -> lists:reverse(OTexs);
rescale_tex(Scaler, [{new_tex,Atom,Tex}|Paths], OTexs) ->
    case Tex of
       #image_tag_r{x=SrcX,y=SrcY,width=SrcWidth,height=SrcHeight} ->
            {X1,Y1} = rescale_coord(Scaler, {SrcX,SrcY}),
            {X2,Y2} = rescale_coord(Scaler, {SrcWidth,SrcHeight}),
            NewTex  = Tex#image_tag_r{x=X1,y=Y1,width=X2,height=Y2}
    end,
    rescale_tex(Scaler, Paths, [{new_tex,Atom,NewTex}|OTexs]).


%%%
%%% Layer name transforms parse
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
            io:format(?__(1,
                "~p: NOTE: SVG layer contains a directive "
                "but not known: ~w=~w~n"), [?MODULE,Name_S,Num]),
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



%%%
%%% Preprocess the SVG path tokens by substituting abbreviated
%%% linetos (H, h, V, v) to full linetos (L, l) and
%%% curve shorthands (S, s, Q, q, T, t) to full curves (C, c).
%%%
%%% Preprocess the SVG arc path tokens by substituting Arc SVG
%%% path commands (A, a) with linetos.
%%%

%%%

-record(subst_curv_pos,
    {
    x_abs = 0.0,
    y_abs = 0.0,
    cp2 = {0.0, 0.0},
    curr_cmd = lineto_abs
    }).

substitute_commands(List) ->
    %% Check if we need to do it first
    case lists:any(fun substitute_commands_check/1, List) of
        true ->
            substitute_commands(List, [], #subst_curv_pos{});
        false ->
            List
    end.
substitute_commands_check(A) when A =:= "H"; A =:= "h"; A =:= "V"; A =:= "v" -> true;
substitute_commands_check(A) when A =:= "S"; A =:= "s"; A =:= "Q"; A =:= "q"; A =:= "T"; A =:= "t" -> true;
substitute_commands_check(A) when A =:= "A"; A =:= "a" -> true;
substitute_commands_check(_) -> false.

substitute_commands([Op | Rest], O, CurrPos) when Op =:= "H" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_linehorz_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["L"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "h" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_linehorz_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["l"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "V" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_linevert_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["L"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "v" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_linevert_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["l"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "S" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_cubicshort_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["C"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "s" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_cubicshort_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["c"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "Q" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_quadratic_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["C"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "q" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_quadratic_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["c"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "T" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_quadshort_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["C"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "t" ->
    {ok, NewCurves, Rest_1, CurrPos_1} = substitute_quadshort_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewCurves) ++ ["c"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "A" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_arcs_to_lineto_abs(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["L"] ++ O, CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos) when Op =:= "a" ->
    {ok, NewLineTos, Rest_1, CurrPos_1} = substitute_arcs_to_lineto_rel(Rest, CurrPos),
    substitute_commands(Rest_1, lists:reverse(NewLineTos) ++ ["L"] ++ O, CurrPos_1);

%% Keep track of current drawing position by also keeping track of
%% M, m, L, l, C, c  SVG path commands. The current position is needed
%% to substitute the needed arguments of the other commands correctly.
%%
substitute_commands([Op,X,Y | Rest], O, CurrPos)
  when Op =:= "M" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = lineto_abs,
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y)
    },
    substitute_commands(Rest, [Y,X,Op| O], CurrPos_1);
substitute_commands([Op,DX,DY | Rest], O,
                    #subst_curv_pos{x_abs=XAbs,y_abs=YAbs}=CurrPos)
  when Op =:= "m" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = lineto_rel,
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY)
    },
    substitute_commands(Rest, [DY,DX,Op| O], CurrPos_1);
substitute_commands([Op,X,Y | Rest], O, CurrPos)
  when Op =:= "L" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = lineto_abs,
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y)
    },
    substitute_commands(Rest, [Y,X,Op| O], CurrPos_1);
substitute_commands([Op,DX,DY | Rest], O,
                    #subst_curv_pos{x_abs=XAbs,y_abs=YAbs}=CurrPos)
  when Op =:= "l" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = lineto_rel,
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY)
    },
    substitute_commands(Rest, [DY,DX,Op| O], CurrPos_1);
substitute_commands([Op,X1,Y1,X2,Y2,X,Y | Rest], O, CurrPos)
  when Op =:= "C" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = curveto_abs,
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y),
        cp2 = {subst_str_to_flt(X2), subst_str_to_flt(Y2)}
    },
    substitute_commands(Rest, [Y,X,Y2,X2,Y1,X1,Op| O], CurrPos_1);
substitute_commands([Op,DX1,DY1,DX2,DY2,DX,DY | Rest], O,
                    #subst_curv_pos{x_abs=XAbs,y_abs=YAbs}=CurrPos)
  when Op =:= "c" ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        curr_cmd = curveto_rel,
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY),
        cp2 = { XAbs + subst_str_to_flt(DX2),
                YAbs + subst_str_to_flt(DY2)}
    },
    substitute_commands(Rest, [DY,DX,DY2,DX2,DY1,DX1,Op| O], CurrPos_1);
    
substitute_commands([[NumDigit|_]=X,Y | Rest], O,
                    #subst_curv_pos{curr_cmd=lineto_abs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y)
    },
    substitute_commands(Rest, [Y,X| O], CurrPos_1);
substitute_commands([[NumDigit|_]=DX,DY | Rest], O,
                    #subst_curv_pos{x_abs=XAbs,y_abs=YAbs,curr_cmd=lineto_rel}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY)
    },
    substitute_commands(Rest, [DY,DX| O], CurrPos_1);
substitute_commands([[NumDigit|_]=X1,Y1,X2,Y2,X,Y | Rest], O,
                    #subst_curv_pos{curr_cmd=curveto_abs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y),
        cp2 = {subst_str_to_flt(X2), subst_str_to_flt(Y2)}
    },
    substitute_commands(Rest, [Y,X,Y2,X2,Y1,X1| O], CurrPos_1);
substitute_commands([[NumDigit|_]=DX1,DY1,DX2,DY2,DX,DY | Rest], O,
                    #subst_curv_pos{x_abs=XAbs,y_abs=YAbs,curr_cmd=curveto_rel}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY),
        cp2 = { XAbs + subst_str_to_flt(DX2),
                YAbs + subst_str_to_flt(DY2)}
    },
    substitute_commands(Rest, [DY,DX,DY2,DX2,DY1,DX1| O], CurrPos_1);
substitute_commands([Op | Rest], O, CurrPos)
  when Op =/= "H" andalso Op =/= "h" andalso
       Op =/= "V" andalso Op =/= "v" andalso
       Op =/= "S" andalso Op =/= "s" andalso
       Op =/= "Q" andalso Op =/= "q" andalso 
       Op =/= "T" andalso Op =/= "t" andalso
       Op =/= "A" andalso Op =/= "a" ->
    substitute_commands(Rest, [Op | O], CurrPos);
substitute_commands([], O, _) ->
    lists:reverse(O).


%% Horizontal line abbreviations
%%

substitute_linehorz_abs(List, CurrPos) ->
    substitute_linehorz_abs(List, [], CurrPos).
substitute_linehorz_abs([[NumDigit|_]=X | Rest], O,
                        #subst_curv_pos{y_abs=YAbs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrY = subst_flt_to_str(YAbs),
    NewLineTos = [X, CurrY],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X)
    },
    substitute_linehorz_abs(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_linehorz_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_linehorz_rel(List, CurrPos) ->
    substitute_linehorz_rel(List, [], CurrPos).
substitute_linehorz_rel([[NumDigit|_]=DX | Rest], O,
                        #subst_curv_pos{x_abs=XAbs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrY = subst_flt_to_str(0.0),
    NewLineTos = [DX, CurrY],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = XAbs + subst_str_to_flt(DX)
    },
    substitute_linehorz_rel(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_linehorz_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

%% Vertical line abbreviations
%%

substitute_linevert_abs(List, CurrPos) ->
    substitute_linevert_abs(List, [], CurrPos).
substitute_linevert_abs([[NumDigit|_]=Y | Rest], O,
                        #subst_curv_pos{x_abs=XAbs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrX = subst_flt_to_str(XAbs),
    NewLineTos = [CurrX, Y],
    CurrPos_1 = CurrPos#subst_curv_pos{
        y_abs = subst_str_to_flt(Y)
    },
    substitute_linevert_abs(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_linevert_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_linevert_rel(List, CurrPos) ->
    substitute_linevert_rel(List, [], CurrPos).
substitute_linevert_rel([[NumDigit|_]=DY | Rest], O,
                        #subst_curv_pos{y_abs=YAbs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    CurrX = subst_flt_to_str(0.0),
    NewLineTos = [CurrX, DY],
    CurrPos_1 = CurrPos#subst_curv_pos{
        y_abs = YAbs + subst_str_to_flt(DY)
    },
    substitute_linevert_rel(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_linevert_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.


%% Cubic shorthand (S or s)
%%
substitute_cubicshort_abs(List, CurrPos) ->
    substitute_cubicshort_abs(List, [], CurrPos).
substitute_cubicshort_abs([[NumDigit|_]=X2, Y2, X, Y | Rest], O,
                          #subst_curv_pos{x_abs=X_O,y_abs=Y_O,cp2={CX2_0,CY2_0}}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    %% Reflect the control point with X,Y being the origin
    {CX2,CY2} = subst_bezier_reflect_point_abs(CX2_0,CY2_0, X_O, Y_O),
    X1 = subst_flt_to_str(CX2),
    Y1 = subst_flt_to_str(CY2),
    NewCurve = [X1,Y1, X2,Y2, X,Y],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y),
        cp2 = { subst_str_to_flt(X2), subst_str_to_flt(Y2) }
    },
    substitute_cubicshort_abs(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_cubicshort_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_cubicshort_rel(List, CurrPos) ->
    substitute_cubicshort_rel(List, [], CurrPos).
substitute_cubicshort_rel([[NumDigit|_]=DX2, DY2, DX, DY | Rest], O,
                          #subst_curv_pos{x_abs=XAbs,y_abs=YAbs}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    %% Reflect the control point with X,Y being the origin
    {DCX2,DCY2} = subst_bezier_reflect_point_rel(CurrPos),
    DX1 = subst_flt_to_str(DCX2),
    DY1 = subst_flt_to_str(DCY2),
    NewCurve = [DX1,DY1, DX2,DY2, DX,DY],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = XAbs + subst_str_to_flt(DX),
        y_abs = YAbs + subst_str_to_flt(DY),
        cp2 = { XAbs + subst_str_to_flt(DX2),
                YAbs + subst_str_to_flt(DY2) }
    },
    substitute_cubicshort_rel(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_cubicshort_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.


%% Change quadratic endpoint to cubic endpoint pair.
quadratic_cubic_endpoints(X1, Y1, X, Y, OX, OY) ->
    %% Relative to its point
    X1D = X1 - OX,
    Y1D = Y1 - OY,
    X2D = X1 - X,
    Y2D = Y1 - Y,
    U = 2.0 / 3.0,
    C_X1 = OX + (X1D * U),
    C_Y1 = OY + (Y1D * U),
    C_X2 = X + (X2D * U),
    C_Y2 = Y + (Y2D * U),
    {C_X1, C_Y1, C_X2, C_Y2}.
    
%% Quadratic bezier curve (Q or q)
%%
substitute_quadratic_abs(List, CurrPos) ->
    substitute_quadratic_abs(List, [], CurrPos).
substitute_quadratic_abs([[NumDigit|_]=X1, Y1, X, Y | Rest], O,
                         #subst_curv_pos{x_abs=O_X,y_abs=O_Y}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    {C_X1,C_Y1,C_X2,C_Y2} = quadratic_cubic_endpoints(
        subst_str_to_flt(X1),
        subst_str_to_flt(Y1),
        subst_str_to_flt(X),
        subst_str_to_flt(Y),
        O_X,
        O_Y
    ),
    NewCurve = [
        subst_flt_to_str(C_X1),
        subst_flt_to_str(C_Y1),
        subst_flt_to_str(C_X2),
        subst_flt_to_str(C_Y2), X,Y ],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y),
        cp2 = { subst_str_to_flt(X1), subst_str_to_flt(Y1) }
    },
    substitute_quadratic_abs(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_quadratic_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_quadratic_rel(List, CurrPos) ->
    substitute_quadratic_rel(List, [], CurrPos).
substitute_quadratic_rel([[NumDigit|_]=DX1, DY1, DX, DY | Rest], O,
                         #subst_curv_pos{x_abs=O_X,y_abs=O_Y}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    {C_DX1,C_DY1,C_DX2,C_DY2} = quadratic_cubic_endpoints(
        subst_str_to_flt(DX1) + O_X,
        subst_str_to_flt(DY1) + O_Y,
        subst_str_to_flt(DX) + O_X,
        subst_str_to_flt(DY) + O_Y,
        O_X,
        O_Y
    ),
    NewCurve = [
        subst_flt_to_str(C_DX1 - O_X),
        subst_flt_to_str(C_DY1 - O_Y), 
        subst_flt_to_str(C_DX2 - O_X),
        subst_flt_to_str(C_DY2 - O_Y), DX,DY ],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = O_X + subst_str_to_flt(DX),
        y_abs = O_Y + subst_str_to_flt(DY),
        cp2 = { O_X + subst_str_to_flt(DX1),
                O_Y + subst_str_to_flt(DY1) }
    },
    substitute_quadratic_rel(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_quadratic_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.


%% Quadratic shorthand (T or t)
%%
substitute_quadshort_abs(List, CurrPos) ->
    substitute_quadshort_abs(List, [], CurrPos).
substitute_quadshort_abs([[NumDigit|_]=X, Y | Rest], O,
                         #subst_curv_pos{x_abs=O_X,y_abs=O_Y,cp2={CX2_0,CY2_0}}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    %% Reflect the control point with X,Y being the origin
    {CX2,CY2} = subst_bezier_reflect_point_abs(CX2_0,CY2_0, O_X, O_Y),
    {C_X1,C_Y1,C_X2,C_Y2} = quadratic_cubic_endpoints(
        CX2,
        CY2,
        subst_str_to_flt(X),
        subst_str_to_flt(Y),
        O_X,
        O_Y
    ),
    NewCurve = [
        subst_flt_to_str(C_X1),
        subst_flt_to_str(C_Y1), 
        subst_flt_to_str(C_X2),
        subst_flt_to_str(C_Y2), X,Y],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y),
        cp2 = {CX2, CY2}
    },
    substitute_quadshort_abs(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_quadshort_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_quadshort_rel(List, CurrPos) ->
    substitute_quadshort_rel(List, [], CurrPos).
substitute_quadshort_rel([[NumDigit|_]=DX, DY | Rest], O,
                         #subst_curv_pos{x_abs=O_X,y_abs=O_Y}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    %% Reflect the control point with DX,DY being the origin
    
    {DCX2,DCY2} = subst_bezier_reflect_point_rel(CurrPos),
    {C_DX1,C_DY1,C_DX2,C_DY2} = quadratic_cubic_endpoints(
        DCX2 + O_X,
        DCY2 + O_Y,
        subst_str_to_flt(DX) + O_X,
        subst_str_to_flt(DY) + O_Y,
        O_X,
        O_Y
    ),
    NewCurve = [
        subst_flt_to_str(C_DX1 - O_X),
        subst_flt_to_str(C_DY1 - O_Y), 
        subst_flt_to_str(C_DX2 - O_X),
        subst_flt_to_str(C_DY2 - O_Y), DX,DY],
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = O_X + subst_str_to_flt(DX),
        y_abs = O_Y + subst_str_to_flt(DY),
        cp2 = { O_X + DCX2,
                O_Y + DCY2}
    },
    substitute_quadshort_rel(Rest, lists:reverse(NewCurve) ++ O, CurrPos_1);
substitute_quadshort_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

subst_bezier_reflect_point_abs(CX,CY,OX,OY) ->
    CRX = CX - OX,
    CRY = CY - OY,
    CNX = -CRX,
    CNY = -CRY,
    {OX + CNX, OY + CNY}.
subst_bezier_reflect_point_rel(#subst_curv_pos{x_abs=CurrX,y_abs=CurrY,cp2={CX_0,CY_0}}=_CurrPos) ->
    {CX_1,CY_1} = subst_bezier_reflect_point_abs(CX_0, CY_0, CurrX, CurrY),
    {CX_1 - CurrX, CY_1 - CurrY}.

subst_str_to_flt(F) ->
    parse_float_number_no_unit(F, 0.0).
subst_flt_to_str(F) ->
    lists:flatten(io_lib:format("~p", [F])).

%%
%%

substitute_arcs_to_lineto_abs(List, CurrPos) ->
    substitute_arcs_to_lineto_abs(List, [], CurrPos).
substitute_arcs_to_lineto_abs([[NumDigit|_]=RX, RY, XAxisRotation,
                              LargeArcFlag, SweepFlag, X, Y | Rest], O,
                              #subst_curv_pos{x_abs=CurrX,y_abs=CurrY}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    RX_Num = subst_str_to_flt(RX),
    RY_Num = subst_str_to_flt(RY),
    XAxisRotation_Num = subst_str_to_flt(XAxisRotation),
    LargeArcFlag_Bool = subst_str_to_bool(LargeArcFlag),
    SweepFlag_Bool = subst_str_to_bool(SweepFlag),
    X_Num = subst_str_to_flt(X),
    Y_Num = subst_str_to_flt(Y),
    Points = arc_path(RX_Num, RY_Num, XAxisRotation_Num, LargeArcFlag_Bool,
        SweepFlag_Bool, {CurrX,CurrY}, {X_Num,Y_Num}),
    NewLineTos = lists:append(
        [[subst_flt_to_str(P_X), subst_flt_to_str(P_Y)]
          || {P_X, P_Y} <- Points]),
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = subst_str_to_flt(X),
        y_abs = subst_str_to_flt(Y)
    },
    substitute_arcs_to_lineto_abs(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_arcs_to_lineto_abs(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

substitute_arcs_to_lineto_rel(List, CurrPos) ->
    substitute_arcs_to_lineto_rel(List, [], CurrPos).
substitute_arcs_to_lineto_rel([[NumDigit|_]=RX, RY, XAxisRotation,
                              LargeArcFlag, SweepFlag, DX, DY | Rest], O,
                              #subst_curv_pos{x_abs=CurrX,y_abs=CurrY}=CurrPos)
  when ?IS_DIGIT(NumDigit) ->
    RX_Num = subst_str_to_flt(RX),
    RY_Num = subst_str_to_flt(RY),
    XAxisRotation_Num = subst_str_to_flt(XAxisRotation),
    LargeArcFlag_Bool = subst_str_to_bool(LargeArcFlag),
    SweepFlag_Bool = subst_str_to_bool(SweepFlag),
    X_Num = CurrX + subst_str_to_flt(DX),
    Y_Num = CurrY + subst_str_to_flt(DY),
    Points = arc_path(RX_Num, RY_Num, XAxisRotation_Num, LargeArcFlag_Bool,
        SweepFlag_Bool, {CurrX,CurrY}, {X_Num,Y_Num}),
    NewLineTos = lists:append(
        [[subst_flt_to_str(P_X), subst_flt_to_str(P_Y)]
          || {P_X, P_Y} <- Points]),
    CurrPos_1 = CurrPos#subst_curv_pos{
        x_abs = CurrX + subst_str_to_flt(DX),
        y_abs = CurrY + subst_str_to_flt(DY)
    },
    substitute_arcs_to_lineto_rel(Rest, lists:reverse(NewLineTos) ++ O, CurrPos_1);
substitute_arcs_to_lineto_rel(Rest, O, CurrPos) ->
    {ok, lists:reverse(O), Rest, CurrPos}.

subst_str_to_bool(Str) ->
    case string:to_integer(Str) of
        {N, _} when N > 0 -> true;
        _ -> false
    end.

arc_path(RX, RY, _Rotation, _LargeArcFlag, _SweepFlag, _P1, {X,Y})
  when abs(RX) < ?EPSILON; abs(RY) < ?EPSILON ->
    %% If rx = 0 or ry = 0, we just make a line
    [{X,Y}];
arc_path(RX, RY, Rotation, LargeArc, Sweep, P1, P2) ->
    {_, ArcLines} = arc_path_cmd(P1, P2, {RX,RY}, Rotation, LargeArc, Sweep),
    ArcLines.



%% Determine the two intersection points of two circles.
%%
-spec two_circles_intersect(float(), {float(),float()}, {float(),float()}) ->
    {{float(),float()},{float(),float()}}.
two_circles_intersect(Radius, {X1,Y1}, {X2,Y2}) ->
    %% The two circles are the same radius which simplifies the equation.
    XD = X2 - X1,
    YD = Y2 - Y1,
    D = math:sqrt((XD*XD) + (YD*YD)),
    DAng = angle_of_line({0.0,0.0}, {XD,YD}),
    
    MidPoint = (D * D) / (2.0 * D),
    
    %% MidPoint is the adjacent corner of the triangle
    %% Radius is the hypotenuse of the triangle
    SAng = math:acos(case MidPoint / Radius of
        AC when AC > 1.0 -> 1.0;
        AC -> AC
    end),
    PAng_1 = DAng - SAng,
    PAng_2 = DAng + SAng,
    
    Point1 = {X1 + (math:cos(PAng_1) * Radius), Y1 + (math:sin(PAng_1) * Radius)},
    Point2 = {X1 + (math:cos(PAng_2) * Radius), Y1 + (math:sin(PAng_2) * Radius)},
    {Point1, Point2}.

angles_from_center(CenterPoint, {X1,Y1}, {X2,Y2}) ->
    Angle1_0 = angle_of_line(CenterPoint, {X1,Y1}),
    Angle2   = angle_of_line(CenterPoint, {X2,Y2}),
    {Angle1_0, Angle2}.
    
paths_from_two_circles_points(Radius, {X1,Y1}, {X2,Y2}, LargeArc, Sweep) ->
    {Point1_0, Point2_0} = two_circles_intersect(Radius, {X1,Y1}, {X2,Y2}),
    
    %% Create lines where the ellipse starts to make lines until where it ends.
    {P1Angle1_0, P1Angle2} = angles_from_center(Point1_0, {X1,Y1}, {X2,Y2}),
    {P2Angle1_0, P2Angle2} = angles_from_center(Point2_0, {X1,Y1}, {X2,Y2}),
    
    %% Determine the ellipse to use from the two boolean options
    case LargeArc of
        true ->
            case Sweep of
                true  -> ArcLines_0 = make_circle_path_between_angles(Point1_0, Radius, P1Angle1_0, P1Angle2, 0.1);
                false -> ArcLines_0 = make_circle_path_between_angles(Point2_0, Radius, P2Angle1_0, P2Angle2, -0.1)
            end;
        false ->
            case Sweep of
                true  -> ArcLines_0 = make_circle_path_between_angles(Point2_0, Radius, P2Angle1_0, P2Angle2, 0.1);
                false -> ArcLines_0 = make_circle_path_between_angles(Point1_0, Radius, P1Angle1_0, P1Angle2, -0.1)
            end
    end,
    
    {{Point1_0, Point2_0}, ArcLines_0}.
    
make_circle_path_between_angles(CenterPoint, Radius, Angle1_0, Angle2, Direction) ->
    case Direction > 0.0 of
        true ->
        case Angle1_0 > Angle2 of
            true  -> Angle1 = Angle1_0 - (math:pi() * 2.0);
            false -> Angle1 = Angle1_0
        end;
        false ->
        case Angle1_0 < Angle2 of
            true  -> Angle1 = Angle1_0 + (math:pi() * 2.0);
            false -> Angle1 = Angle1_0
        end
    end,
    make_circle_path_between_angles_1(CenterPoint, Radius, Angle1, Angle2, Direction, []).
make_circle_path_between_angles_1({P_X, P_Y}=CenterPoint, Radius, AngleCurr, AngleEnd, Direction, O) ->
    AX = P_X + (Radius * math:cos(AngleCurr)),
    AY = P_Y + (Radius * math:sin(AngleCurr)),
    O_1 = [{AX, AY} | O],
    case Direction > 0.0 of
        true  when AngleCurr > AngleEnd ->
            lists:reverse(O_1);
        false when AngleCurr < AngleEnd ->
            lists:reverse(O_1);
        _ ->
            make_circle_path_between_angles_1(
                CenterPoint, Radius, AngleCurr+Direction, AngleEnd, Direction, O_1)
    end.

%% 2D rotation used for svg arc calculations
rotate_point({X,Y}, A) ->
    { (X * math:cos(A)) + (Y * math:sin(A)),
      (Y * math:cos(A)) - (X * math:sin(A)) }.

%% For determining out of range radii and the correction
%% to do for it.
get_radius_to_range_ratio({X1, Y1}, {X2, Y2}, {XR_1, YR_1}, Angle) ->
    %%
    AX_0 = ((X1 - X2) / 2.0),
    AY_0 = ((Y1 - Y2) / 2.0),
    {AX, AY} = rotate_point({AX_0, AY_0}, Angle),
    A = ((AX * AX) / (XR_1 * XR_1)) + ((AY * AY) / (YR_1 * YR_1)),
    A.

%% The Arc Path Command specifies a radius and an angle.
-spec arc_path_cmd({float(), float()}, {float(),float()}, {float(),float()},
    float(), boolean(), boolean()) -> any().
arc_path_cmd(ArcFromPoint, ArcToPoint, {XR_0,YR_0}, Angle_0, LargeArc, Sweep)
  when is_float(YR_0), abs(YR_0) > ?EPSILON ->
    %% By making ellipses of the same radius and angle at the start and end point of
    %% the segment, we get two intersection points. At those two intersection points
    %% are the center of the two possible ellipses that can draw an arc for our arc path.
    
    Angle = Angle_0 / 180.0 * math:pi(),
    
    %% Ensure radii are positive numbers
    XR_1 = abs(XR_0),
    YR_1 = abs(YR_0),
    
    %% Rotate the coordinate system by the opposite angle so ellipses are angle=0
    %% (the x radius and y radius of the ellipse are on the x axis and y axis
    %% respectively)
    {X1, Y1_0} = rotate_point(ArcFromPoint, -Angle),
    {X2, Y2_0} = rotate_point(ArcToPoint,   -Angle),
    
    A = get_radius_to_range_ratio(ArcFromPoint, ArcToPoint, {XR_1, YR_1}, Angle),
    case A =< 1.0 of
        true ->
            XR_2 = XR_1,
            YR_2 = YR_1;
        false ->
            io:format("~p: NOTE: under range radii~n", [?MODULE]),
            %% This shouldn't need to ever happen when output from a drawing
            %% program, only on out of range radii, which is more likely by
            %% hand made SVG arcs.
            SqrtA = math:sqrt(A),
            XR_2 = XR_1 * 1.00002 * SqrtA, % A tiny factor for math:acos's 
            YR_2 = YR_1 * 1.00002 * SqrtA  % input to never go over 1.0
    end,
    
    %% Rescale the y-axis of the points so the ellipses that encircle the
    %% start and end points become circles for the next equations to work.
    YRescale = XR_2 / YR_2,
    Y1 = YRescale * Y1_0,
    Y2 = YRescale * Y2_0,
    Radius = YRescale * YR_2,
    
    %% Get the two intersecting points between the two circles 
    {{{Point1_0_X,Point1_0_Y}, {Point2_0_X,Point2_0_Y}}, ArcLines_0}
        = paths_from_two_circles_points(Radius, {X1,Y1}, {X2,Y2}, LargeArc, Sweep),
    
    %% Reverse-rescale the y-axis so the two intersecting points are to the 
    %% expected scale to get points that meet the ellipses centers.
    Point1_1 = {Point1_0_X, Point1_0_Y / YRescale},
    Point2_1 = {Point2_0_X, Point2_0_Y / YRescale},
    ArcLines_1 = [{AX, AY / YRescale} || {AX, AY} <- ArcLines_0],
    
    %% Reverse the rotation of the coordinate system.
    Point1_2 = rotate_point(Point1_1, Angle),
    Point2_2 = rotate_point(Point2_1, Angle),
    ArcLines = [rotate_point(P, Angle) || P <- ArcLines_1],
    
    {{Point1_2, Point2_2}, ArcLines}.

%% Used for arc path calculations.
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

%%%
%%%

%%%
%%%  Extract <style> and <script> tags from SVG
%%%
-define(IS_WS(A), (
    C =:= 32 orelse C =:= 9 orelse
    C =:= 10 orelse C =:= 13
)).

%% Extract style and script tags from the SVG file even before parsing
%% with xmerl.
%%
extract_styles(A) ->
    extract_styles(A, 0, []).
extract_styles(A, At, OL) ->
    case binary:match(A, [<<"<s">>,<<"<S">>], [{scope, {At, byte_size(A)-At}}]) of
        {S1,_} ->
            case string:lowercase(binary:part(A, {S1, 6})) of
                <<"<style">> ->
                    {S1Start,_} = binary:match(A, [<<">">>], [{scope,{S1, byte_size(A)-S1}}]),
                    {S2,_} = binary:match(A, [<<"</s">>,<<"</S">>], [{scope,{S1, byte_size(A)-S1}}]),
                    {S3,_} = binary:match(A, [<<">">>], [{scope,{S2, byte_size(A)-S2}}]),
                    StyleCont = sty_cmt_unwrap(binary:part(A, {S1Start+1, S2-S1Start-1})),
                    NewB = iolist_to_binary([
                        binary:part(A, {0, S1}),
                        binary:part(A, {S3+1, byte_size(A)-S3-1})]),
                    Attrs = binary:part(A, {S1+1+5, S1Start-S1-1-5}),
                    TypeAttr = extract_styles_attr("type", Attrs),
                    extract_styles(NewB, 0, [{TypeAttr, StyleCont}|OL]);
                _ ->
                    case string:lowercase(binary:part(A, {S1, 7})) of
                        <<"<script">> ->
                            {S2,_} = binary:match(A, [<<"</s">>,<<"</S">>], [{scope,{S1, byte_size(A)-S1}}]),
                            {S3,_} = binary:match(A, [<<">">>], [{scope,{S2, byte_size(A)-S2}}]),
                            NewB = iolist_to_binary([
                                binary:part(A, {0, S1}),
                                binary:part(A, {S3+1, byte_size(A)-S3-1})]),
                            extract_styles(NewB, 0, OL);
                        _ ->
                            extract_styles(A, S1+1, OL)
                    end
            end;
        nomatch ->
            {A, lists:reverse(OL)}
    end.

%% This extracts an attribute for a <style ...> tag, it is
%% only used to extract the type attribute.
%%
extract_styles_attr(AttrName, Bin)
  when is_binary(Bin) ->
    extract_styles_attr(AttrName, binary_to_list(Bin));
extract_styles_attr(AttrName, Str) ->
    Str_1 = string:lowercase(string:strip(Str)),
    extract_styles_attr_1(AttrName, extract_styles_attr_2(Str_1)).
extract_styles_attr_1(_AttrName, []) ->
    none;
extract_styles_attr_1(AttrName, [{A,B}|_])
  when AttrName =:= A ->
    B;
extract_styles_attr_1(AttrName, [_|L]) ->
    extract_styles_attr_1(AttrName, L).
extract_styles_attr_2(Str) ->
    extract_styles_attr_2(Str, {1, []}, []).
extract_styles_attr_2([], {_, _}, OL) ->
    lists:reverse(OL);
extract_styles_attr_2([], {_, AtVal, AtName}, OL) ->
    AtVal_1 = lists:reverse(AtVal),
    extract_styles_attr_2([], {1, []}, [{AtName,AtVal_1}|OL]);
extract_styles_attr_2([C|Str], {1, AtName}, OL)
  when ?IS_WS(C); C =:= $'; C =:= 34 ->
    extract_styles_attr_2(Str, {1, [32|AtName]}, OL);
extract_styles_attr_2([C|Str], {1, AtName}, OL)
  when C =:= $= ->
    AtName_1 = string:lowercase(string:strip(lists:reverse(AtName))),
    [AtName_2|_] = lists:reverse(string:tokens(AtName_1, " \t")),
    extract_styles_attr_2(Str, {2, AtName_2}, OL);
extract_styles_attr_2([C|Str], {2, AtName}, OL)
  when C =:= $'; C =:= 34 ->
    extract_styles_attr_2(Str, {3, [], AtName}, OL);
extract_styles_attr_2([C|Str], {3, AtVal, AtName}, OL)
  when C =:= $'; C =:= 34 ->
    AtVal_1 = lists:reverse(AtVal),
    extract_styles_attr_2(Str, {1, []}, [{AtName,AtVal_1}|OL]);
extract_styles_attr_2([C|Str], {4, AtVal, AtName}, OL)
  when ?IS_WS(C) ->
    AtVal_1 = lists:reverse(AtVal),
    extract_styles_attr_2(Str, {1, []}, [{AtName,AtVal_1}|OL]);
extract_styles_attr_2([C|Str], {M, AtVal, AtName}, OL)
  when M =:= 3; M =:= 4 ->
    extract_styles_attr_2(Str, {M, [C|AtVal], AtName}, OL);
extract_styles_attr_2([C|Str], {2, AtName}, OL) ->
    extract_styles_attr_2(Str, {4, [C], AtName}, OL);
extract_styles_attr_2([C|Str], {1, AtName}, OL) ->
    extract_styles_attr_2(Str, {1, [C|AtName]}, OL).
    


%% Sometimes there could be a SGML comment or CDATA within the style tag
%%
sty_cmt_unwrap(<<C,R/binary>>)
  when ?IS_WS(C) ->
    sty_cmt_unwrap(R);
sty_cmt_unwrap(<<"<!--",R/binary>>) ->
    {St, _} = binary:match(R, [<<"-->">>]),
    binary:part(R, {0, St});
sty_cmt_unwrap(<<"<![CDATA[",R/binary>>) ->
    {St, _} = binary:match(R, [<<"]]>">>]),
    binary:part(R, {0, St});
sty_cmt_unwrap(R) ->
    R.


%% Get link-rel CSS files
%%
get_link_rel_css(L, CurDir) ->
    get_link_rel_css(L, CurDir, []).
get_link_rel_css([], _CurDir, OL) ->
    lists:append(lists:reverse(OL));
get_link_rel_css([{linkrel,LinkedCSS}|L], CurDir, OL) ->
    Rel = proplists:get_value("rel", LinkedCSS, none),
    HRef = proplists:get_value("href", LinkedCSS, none),
    Type = proplists:get_value("type", LinkedCSS, none),
    if
        HRef =:= none ->
            get_link_rel_css(L, CurDir, OL);
        true ->
            case string:lowercase(string:strip(Rel)) of
                "stylesheet" ->
                    OL_1 = get_link_rel_css_1(HRef, CurDir, Type, OL),
                    get_link_rel_css(L, CurDir, OL_1);
                _ ->
                    get_link_rel_css(L, CurDir, OL)
            end
    end.
get_link_rel_css_1(HRef, CurDir, Type, OL) ->
    case load_css_import_local(HRef, CurDir) of
        %% Found a local file
        {ok, File_1} ->
            case parse_stylesheet_file(File_1, parse_stylesheet_type(Type)) of
                {ok, CSS} ->
                    [CSS|OL];
                false ->
                    OL
            end;
        false ->
            OL
    end.




%%%
%%%  Style sheet parser for SVG
%%%
%%%  Parse and apply style sheets attached to the SVG file.
%%%

-type stypropstr() :: list().

-type s0_tup_tag() :: none | list().
-type s0_tup_id() :: none | list().
-type s0_tup_cls() :: none | list().
-type s0_tuple() :: {s0_tup_tag(), s0_tup_id(), s0_tup_cls()}.
-type s0() :: [s0_tuple()].
-type style_entry() :: {[tuple()], stypropstr()}.
-type style_list() :: [style_entry()].



%% Match CSS selectors to SVG elements and set styles on them
%% Only basic CSS matching by id, class and/or element, and
%% match of parent elements are currently implemented.
%%
-record(csssty, {
    css,
    s = []
}).
match_css(SVGL, M) ->
    match_css(SVGL, #csssty{css=M}, []).

match_css([{s,Tag,Opt}|SVGL], #csssty{css=CSS,s=S0}=Stt, OL) ->
    Id = proplists:get_value(id, Opt, none),
    Class_0 = proplists:get_value(class, Opt, none),
    Style = proplists:get_value(style, Opt, none),
    Class_1 = if
        is_list(Class_0) ->
            string:tokens(Class_0, " \t");
        true ->
            []
    end,
    T = {Tag, Id, Class_1},
    Style_1 = match_css_1(CSS, [T|S0], Style),
    Opt1 =
        case Style_1 of
            none ->
                Opt;
            _ ->
                [{style, Style_1}|proplists:delete(style, Opt)]
        end,
    match_css(SVGL, Stt#csssty{s=[T|S0]}, [{s,Tag,Opt1}|OL]);
match_css([{e,_}=A0|SVGL], #csssty{s=[_|S0]}=Stt, OL) ->
    match_css(SVGL, Stt#csssty{s=S0}, [A0|OL]);
match_css([A0|SVGL], Stt, OL) ->
    match_css(SVGL, Stt, [A0|OL]);
match_css([], #csssty{}=_Stt, OL) ->
    lists:reverse(OL).


%% Find which selectors in the style sheet list matches to each element,
%% where S0 is a list of the current element as a tuple {TagName, Id, Class}
%% and its parents.
%%
-spec match_css_1(style_list(), s0(), none | #style_colors{}) -> none | #style_colors{}.
match_css_1(CSS, S0, Style) ->
    match_css_1_find(CSS, S0, Style).
match_css_1_find([], _S0, Style) ->
    Style;
match_css_1_find([C|CSS], S0, Style) ->
    Style_1 = case match_css_c(C, S0) of
        {match, StyleFrom_0} ->
            %% Parse the style properties here when the selectors
            %% match an element.
            {ok, StyleFrom_1} = parse_style(StyleFrom_0),
            {ok, StyleFrom} = style_to_tuple(StyleFrom_1),
            match_css_1_set_style(Style, StyleFrom);
        false ->
            Style
    end,
    match_css_1_find(CSS, S0, Style_1).


%% Try to match each comma separated selector, try each {m, ...} tuple until
%% something matches or return false.
%%
-spec match_css_c(style_entry(), s0()) -> false | {match, stypropstr()}.
match_css_c({Match, StyleFrom}, S0) ->
    match_css_c(Match, S0, StyleFrom).
match_css_c([{m,W}|L], S0, StyleFrom) ->
    case match_css_c_1(W, S0) of
        true ->
            {match, StyleFrom};
        false ->
            match_css_c(L, S0, StyleFrom)
    end;
match_css_c([], _S0, _) ->
    false.


-spec match_css_c_1(tuple(), s0()) -> boolean().

-record(matchcss_stt, {
    sl
}).
match_css_c_1(Sel, SL) ->
    case match_css_c_2(Sel, #matchcss_stt{sl=SL}) of
        #matchcss_stt{} -> true;
        _ -> false
    end.
match_css_c_2(_, #matchcss_stt{sl=[]}=_Stt) ->
    false;
match_css_c_2({select, SList}, #matchcss_stt{sl=[S_0|SL]}=Stt) ->
    case match_css_c_2_m(slist(SList), S_0) of
        true ->
            Stt#matchcss_stt{sl=SL};
        false ->
            false
    end;
match_css_c_2({in, A, B}, Stt1) ->
    case match_css_c_2(B, Stt1) of
        false -> false;
        Stt2 ->
            case match_css_c_2(A, Stt2) of
                false -> false;
                Stt3 ->
                    Stt3
            end
    end;
match_css_c_2({ionce, A, B}, Stt1) ->
    case match_css_c_2(B, Stt1) of
        false -> false;
        Stt2 ->
            case match_css_c_2(A, Stt2) of
                false -> false;
                Stt3 ->
                    Stt3
            end
    end;
match_css_c_2({subsib, _A, _B}, _S0) ->
    % unimplemented
    false;
match_css_c_2({nextsib, _A, _B}, _S0) ->
    % unimplemented
    false.

match_css_c_2_m([{MTag,MId,MCls}|SList], {VTag,VId,VCls}=S_0) ->
    case match_css_c_2_m2(MTag,VTag) andalso
         match_css_c_2_m2(MId,VId) andalso
         match_css_c_2_mlist(MCls,VCls) of
        true ->
            match_css_c_2_m(SList, S_0);
        false ->
            false
    end;
match_css_c_2_m([], _) ->
    true.

match_css_c_2_m2(any, _) ->
    true;
match_css_c_2_m2(Str1, Str2)
  when is_list(Str1), is_list(Str2) ->
    string:lowercase(Str1) =:= string:lowercase(Str2);
match_css_c_2_m2(Str1, none)
  when is_list(Str1) ->
    false.


match_css_c_2_mlist(any, _) ->
    true;
match_css_c_2_mlist(B, List) ->
    lists:any(
        fun(C) ->
            match_css_c_2_m2(C, B)
        end, List).



slist([]) -> [];
slist([A|L]) ->
    [slist_1(A)|slist(L)].
slist_1({tag,star})        -> {any,any,any};
slist_1({tag,TagName})     -> {TagName,any,any};
slist_1({dot,ClassName})   -> {any,any,ClassName};
slist_1({hash,IDName})     -> {any,IDName,any};
slist_1({dbc_fun,_})   -> {any,any,any};
slist_1({dbc,_})       -> {any,any,any};
slist_1({colon_fun,_}) -> {any,any,any};
slist_1({colon,_})     -> {any,any,any};
slist_1({b,_})     -> {any,any,any};
slist_1({p,_})     -> {any,any,any}.



%% Apply style sheet style information to the style information of an element
%% whenever it is possible by replacing 'inherit' placeholders with a color.
%%
-spec match_css_1_set_style(none | #style_colors{}, none | #style_colors{}) -> none | #style_colors{}.
match_css_1_set_style(StyleTo_0, #style_colors{scol=SCol,fcol=FCol,fopa=FOpa}=_StyleFrom) ->
    StyleTo_1 = match_css_1_set_style_1(StyleTo_0, {scol, SCol}),
    StyleTo_2 = match_css_1_set_style_1(StyleTo_1, {fcol, FCol}),
    StyleTo_3 = match_css_1_set_style_1(StyleTo_2, {fopa, FOpa}),
    StyleTo_3.
match_css_1_set_style_1(none, Which) ->
    match_css_1_set_style_1(#style_colors{scol=inherit,fcol=inherit,fopa=inherit}, Which);
match_css_1_set_style_1(#style_colors{scol=inherit}=Style, {scol, Atom})
  when Atom =:= none; Atom =:= opaque ->
    Style#style_colors{scol=Atom};
match_css_1_set_style_1(#style_colors{fcol=inherit}=Style, {fcol, RGB})
  when is_tuple(RGB) ->
    Style#style_colors{fcol=RGB};
match_css_1_set_style_1(#style_colors{fopa=inherit}=Style, {fopa, Opacity}) ->
    Style#style_colors{fopa=Opacity};
match_css_1_set_style_1(Style, _) ->
    Style.


%%%
%%%

%%% Style sheet tokenization and parsing
%%%


%% Parse a list of style sheets, such as returned from extract_styles/1
%%
parse_stylesheet_list(List, CurDir) ->
    parse_stylesheet_list(List, CurDir, []).
parse_stylesheet_list([{MimeType, Cont}|List], CurDir, OL) ->
    {ok, M} = parse_stylesheet(Cont, CurDir, parse_stylesheet_type(MimeType)),
    parse_stylesheet_list(List, CurDir, [M|OL]);
parse_stylesheet_list([], _CurDir, OL) ->
    lists:append(lists:reverse(OL)).


%% Parse style sheet, depending on the type, only CSS is supported.
%%
parse_stylesheet(Cont, CurDir, css) ->
    {Toks_1, _} = parse_stylesheet_1(Cont, CurDir, css, gb_sets:new()),
    parse_css_2(Toks_1);
parse_stylesheet(_, _CurDir, unknown) ->
    {ok, []}.

parse_stylesheet_file(File, css)
  when is_list(File) ->
    case parse_stylesheet_file_1(File, css, gb_sets:new()) of
        {ok, {Toks_1, _}} ->
            parse_css_2(Toks_1);
        {false, _} ->
            false
    end;
parse_stylesheet_file(_, unknown) ->
    {ok, []}.


parse_stylesheet_1(Cont, CurDir, css, Loaded) ->
    {Toks, _} = parse_css_1(Cont, []),
    %% Load @import statements
    load_css_import(Toks, CurDir, Loaded).

parse_stylesheet_file_1(File, css, Loaded)
  when is_list(File) ->
    CurDir = filename:dirname(File),
    case string:lowercase(filename:extension(File)) of
        ".css" ->
            case file:read_file(File) of
                {ok, F} ->
                    {ok, parse_stylesheet_1(F, CurDir, css, gb_sets:add(File, Loaded))};
                {error, _} ->
                    {false, Loaded}
            end;
        Ext ->
            io:format("~w: NOTE: CSS File unexpected extension: ~s~n", [?MODULE, Ext]),
            {false, Loaded}
    end.


%% Tokenization
%%
parse_css_1(<<C1,C2, R/binary>>, OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
  parse_css_1(R_1, OL);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= ${ ->
    {Cont, R_1} = css_enclosing_curly(R),
    parse_css_1(R_1, [{style, Cont}|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when ?IS_WS(C) ->
    case OL of
        [ws|_] ->
            parse_css_1(R, OL);
        _ ->
            parse_css_1(R, [ws|OL])
    end;
parse_css_1(<<"@charset", R/binary>>, OL) ->
    case parse_css_charset(R) of
        {Unused, R_1} ->
            io:format("~w: NOTE: Skipped @charset: ~p~n", [?MODULE, Unused]),
            parse_css_1(R_1, OL)
    end;
parse_css_1(<<"@import", R/binary>>, OL) ->
    case parse_css_import(R) of
        {[{s,File}|_], R_1} ->
            parse_css_1(R_1, [{import, File}|OL]);
        {[{w,"url"},{s,File}|_], R_1} ->
            parse_css_1(R_1, [{import, File}|OL]);
        {Unused, R_1} ->
            io:format("~w: NOTE: Skipped @import: ~p~n", [?MODULE, Unused]),
            parse_css_1(R_1, OL)
    end;
parse_css_1(<<"@media", R/binary>>, OL) ->
    {Cond, Cont, R_1} = css_media(R),
    parse_css_1(R_1, [{media, Cond, Cont}|OL]);
parse_css_1(<<"@supports", R/binary>>, OL) -> %% @supports ( .. ) { .. }
    {Cond, Cont, R_1} = css_media(R),
    parse_css_1(R_1, [{supports, Cond, Cont}|OL]);
parse_css_1(<<"@-", R/binary>>, OL) -> %% @-.. { .. }
    %% Quietly skip proprietary @ rule
    {_, R_1} = css_unknown(R),
    parse_css_1(R_1, OL);
parse_css_1(<<"@", R/binary>>, OL) ->
    %% Standard @-rule, skip it but mention it in console in case
    %% it is a bug that it is skipped.
    {_Unk, R_1} = css_unknown(R),
    io:format("~w: NOTE: Unknown @-rule skipped~n", [?MODULE]),
    parse_css_1(R_1, OL);

parse_css_1(<<"(", R/binary>>, OL) ->
    {Cont, R_1} = css_paren(R),
    parse_css_1(R_1, [{p, Cont}|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $* -> %% Wildcard match
    parse_css_1(R, [star|OL]);

%% Match CSS operators
%%
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $> -> % match operator
    parse_css_1(R, [ionce|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $~ -> % subsequent-sibling operator
    parse_css_1(R, [subsib|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $+ ->
    parse_css_1(R, [nextsib|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $, ->
    parse_css_1(R, [cma|OL]);
parse_css_1(<<C1,C2, R/binary>>, OL)
  when C1 =:= $:,C2 =:= $: ->
    parse_css_1(R, [dbc|OL]);
parse_css_1(<<C1, R/binary>>, OL)
  when C1 =:= $: ->
    parse_css_1(R, [colon|OL]);

%% Match CSS selector
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $. ->
    parse_css_1(R, [dot|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $# ->
    parse_css_1(R, [hash|OL]);

%% CSS Word
parse_css_1(<<C, _/binary>>=R, OL)
  when C >= $A andalso C =< $Z;
       C >= $a andalso C =< $z;
       C =:= $-; C =:= $_ ->
    {Cont, R_1} = css_word(R),
    parse_css_1(R_1, [{w, Cont}|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $[ ->
    {Cont, R_1} = css_enclosing_bracket(R),
    parse_css_1(R_1, [{b, Cont}|OL]);
parse_css_1(<<C, R/binary>>, OL)
  when C =:= $} ->
    {lists:reverse(OL), R};
parse_css_1(<<>>, OL) ->
    {lists:reverse(OL), <<>>}.


%% Parse CSS @import
%%
parse_css_import(R) ->
    parse_css_import(R, [], []).
parse_css_import(<<C, _/binary>>=R, [_|_]=W, OL)
  when ?IS_WS(C); C =:= $/; C =:= $;; C =:= $(; C =:= $'; C =:= 34 ->
    parse_css_import(R, [], [{w,lists:reverse(W)}|OL]);
parse_css_import(<<C1,C2, R/binary>>, [], OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    parse_css_import(R_1, [], OL);
parse_css_import(<<C, R/binary>>, [], OL)
  when C =:= $; ->
    {lists:reverse(OL), R};
parse_css_import(<<C, R/binary>>, [], OL)
  when C =:= $( ->
    {Cont, R_1} = css_paren(R, []),
    Cont_1 = string:strip(Cont),
    Cont_3 = case Cont_1 of
        [$'|Cont_2] ->
            {S, _} = css_quot(iolist_to_binary(Cont_2)),
            S;
        [34|Cont_2] ->
            {S, _} = css_dquot(iolist_to_binary(Cont_2)),
            S;
        Cont_2 ->
            Cont_2
    end,
    parse_css_import(R_1, [], [{s, Cont_3}|OL]);
parse_css_import(<<C, R/binary>>, [], OL)
  when C =:= $' ->
    {Cont, R_1} = css_quot(R),
    parse_css_import(R_1, [], [{s,Cont}|OL]);
parse_css_import(<<C, R/binary>>, [], OL)
  when C =:= 34 ->
    {Cont, R_1} = css_dquot(R),
    parse_css_import(R_1, [], [{s,Cont}|OL]);
parse_css_import(<<C, R/binary>>, [], OL)
  when ?IS_WS(C) ->
    parse_css_import(R, [], OL);
parse_css_import(<<C, R/binary>>, L, OL) ->
    parse_css_import(R, [C|L], OL).


%% Parse CSS @charset
%%
parse_css_charset(R) ->
    %% Reuse @import parse code
    parse_css_import(R).


%% The MIME type of the style sheet
%%
parse_stylesheet_type(Str_0)
  when is_list(Str_0) ->
    Str_1 = string:lowercase(string:strip(Str_0)),
    [Str_2|_] = string:tokens(Str_1, " ,;"),
    case Str_2 =:= "text/css" of
        true ->
            css;
        false ->
            unknown
    end;
parse_stylesheet_type(none) ->
    css;
parse_stylesheet_type(_) ->
    unknown.


parse_css_2(T) ->
    parse_css_2(T, [], []).

%% @media and @supports are unimplemented
parse_css_2([{media, _, _}|T], _ML, OL) ->
    parse_css_2(T, [], OL);
parse_css_2([{supports, _, _}|T], _ML, OL) ->
    parse_css_2(T, [], OL);

%% Parse selectors preceeding the style contents
parse_css_2([{style, Style}|T], ML, OL) ->
    parse_css_2(T, [], [{parse_css_ml(lists:reverse(ML)), Style}|OL]);
parse_css_2([M|T], ML, OL) ->
    parse_css_2(T, [M|ML], OL);
parse_css_2([], _, OL) ->
    {ok, lists:reverse(OL)}.


%% Trim white space tokens from both ends and remove white space
%% adjacent to operators: > ~ ; + :: :
%% The remaining white space tokens are operators for nested
%% element matching.
%%
parse_css_re_ws(ML) ->
    ML1 = parse_css_re_ws_l(ML),
    ML2 = parse_css_re_ws_l(parse_css_re_ws_op(ML1)),
    lists:reverse(ML2).
parse_css_re_ws_l([ws|ML]) ->
    parse_css_re_ws_l(ML);
parse_css_re_ws_l(ML) ->
    ML.

parse_css_re_ws_op(ML) ->
    parse_css_re_ws_op(ML, []).
parse_css_re_ws_op([ws|[ionce|_]=ML], OL) ->
    parse_css_re_ws_op(ML, OL);
parse_css_re_ws_op([ws|[subsib|_]=ML], OL) ->
    parse_css_re_ws_op(ML, OL);
parse_css_re_ws_op([ws|[nextsib|_]=ML], OL) ->
    parse_css_re_ws_op(ML, OL);
parse_css_re_ws_op([ws|[cma|_]=ML], OL) ->
    parse_css_re_ws_op(ML, OL);

parse_css_re_ws_op([ionce=T,ws|ML], OL) ->
    parse_css_re_ws_op(ML, [T|OL]);
parse_css_re_ws_op([subsib=T,ws|ML], OL) ->
    parse_css_re_ws_op(ML, [T|OL]);
parse_css_re_ws_op([nextsib=T,ws|ML], OL) ->
    parse_css_re_ws_op(ML, [T|OL]);
parse_css_re_ws_op([cma=T,ws|ML], OL) ->
    parse_css_re_ws_op(ML, [T|OL]);

parse_css_re_ws_op([T|ML], OL) ->
    parse_css_re_ws_op(ML, [T|OL]);
parse_css_re_ws_op([], OL) ->
    OL.


%% Parse
%%
parse_css_ml(L) ->
    parse_css_ml_cma(parse_css_re_ws(L)).

%% Commas between selectors
%%
parse_css_ml_cma(L) ->
    parse_css_ml_cma(L, [], []).
parse_css_ml_cma([cma|L], [], OL) ->
    parse_css_ml_cma(L, [], OL);
parse_css_ml_cma([cma|L], ML, OL) ->
    parse_css_ml_cma(L, [], [{m, parse_css_ml_op(lists:reverse(ML))}|OL]);
parse_css_ml_cma([A|L], ML, OL) ->
    parse_css_ml_cma(L, [A|ML], OL);
parse_css_ml_cma([], [], OL) ->
    lists:reverse(OL);
parse_css_ml_cma([], ML, OL) ->
    parse_css_ml_cma([], [], [{m, parse_css_ml_op(lists:reverse(ML))}|OL]).


%% Operators (white space > ~ +)
%%
parse_css_ml_op(L) ->
    parse_css_ml_op(L, [], [], none).
parse_css_ml_op([Op|L], ML, [], none)
  when Op =:= ws; Op =:= ionce; Op =:= subsib; Op =:= nextsib ->
    ML_1 = parse_css_ml_2(lists:reverse(ML)),
    parse_css_ml_op(L, [], ML_1, sel_op(Op));
parse_css_ml_op([Op|L], ML, ML0, Op0)
  when Op =:= ws; Op =:= ionce; Op =:= subsib; Op =:= nextsib ->
    ML_1 = parse_css_ml_2(lists:reverse(ML)),
    parse_css_ml_op(L, [], {Op0, ML0, ML_1}, sel_op(Op));
parse_css_ml_op([], ML, [], none) ->
    ML_1 = parse_css_ml_2(lists:reverse(ML)),
    ML_1;
parse_css_ml_op([], ML, ML0, Op0) ->
    ML_1 = parse_css_ml_2(lists:reverse(ML)),
    {Op0, ML0, ML_1};
parse_css_ml_op([A|L], ML, ML0, Op0) ->
    parse_css_ml_op(L, [A|ML], ML0, Op0).

sel_op(ws) -> in;
sel_op(ionce) -> ionce;
sel_op(subsib) -> subsib;
sel_op(nextsib) -> nextsib.


parse_css_ml_2_tag(star) ->
    {tag, star};
parse_css_ml_2_tag({w,TagName})
  when is_list(TagName) ->
    {tag, TagName}.

%%
-define(CSSMW(W), (
    W =:= star orelse (is_tuple(W) andalso element(1, W) =:= w)
)).
parse_css_ml_2([TagName])
  when ?CSSMW(TagName) ->
    {select, [parse_css_ml_2_tag(TagName)]};
parse_css_ml_2([TagName|[_|_]=R])
  when ?CSSMW(TagName) ->
    {select, [parse_css_ml_2_tag(TagName)
               | parse_css_ml_2_1(R)]};
parse_css_ml_2(L) ->
    {select, parse_css_ml_2_1(L)}.
parse_css_ml_2_1([dbc,{w,A},{p,B}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{dbc_fun, {A, B}}|L]);
parse_css_ml_2_1([dbc,{w,A}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{dbc, A}|L]);
parse_css_ml_2_1([dot,{w,A}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{dot, A}|L]);
parse_css_ml_2_1([hash,{w,A}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{hash, A}|L]);
parse_css_ml_2_1([colon,{w,A},{p,B}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{colon_fun, {A, B}}|L]);
parse_css_ml_2_1([colon,{w,A}|L])
  when is_list(A) ->
    parse_css_ml_2_1([{colon, A}|L]);
parse_css_ml_2_1([A]) ->
    [A];
parse_css_ml_2_1([A|L]) ->
    [A | parse_css_ml_2_1(L)].


%% Functions for parse_css_1
%%

%% CSS Word
%%
css_word(R) ->
    css_word(R, []).
css_word(<<C, R/binary>>, OL)
  when C >= $A andalso C =< $Z;
       C >= $a andalso C =< $z;
       C >= $0 andalso C =< $9;
       C =:= $-; C =:= $_ ->
    css_word(R, [C|OL]);
css_word(R, OL) ->
    {lists:reverse(OL), R}.

%% The contents of a style enclosed in curlies.
%%
css_enclosing_curly(R) ->
    css_enclosing_curly(R, []).
css_enclosing_curly(<<C, R/binary>>, OL)
  when C =:= 10; C =:= 13; C =:= 9 ->
    css_enclosing_curly(R, OL);
css_enclosing_curly(<<C1,C2, R/binary>>, OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_enclosing_curly(R_1, OL);
css_enclosing_curly(<<C, R/binary>>, OL)
  when C =:= $} ->
    {lists:reverse(OL), R};
css_enclosing_curly(<<C, R/binary>>, OL) ->
    css_enclosing_curly(R, [C|OL]).

%% The contents enclosed in bracket.
%%
css_enclosing_bracket(R) ->
    css_enclosing_bracket(R, []).
css_enclosing_bracket(<<C, R/binary>>, OL)
  when C =:= 10; C =:= 13; C =:= 9 ->
    css_enclosing_bracket(R, OL);
css_enclosing_bracket(<<C1,C2, R/binary>>, OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_enclosing_bracket(R_1, OL);
css_enclosing_bracket(<<C, R/binary>>, OL)
  when C =:= $] ->
    {lists:reverse(OL), R};
css_enclosing_bracket(<<C, R/binary>>, OL) ->
    css_enclosing_bracket(R, [C|OL]).

%% The contents enclosed in parenthesises
%%
css_paren(R) ->
    css_paren(R, []).
css_paren(<<C, R/binary>>, OL)
  when C =:= 10; C =:= 13; C =:= 9 ->
    css_paren(R, OL);
css_paren(<<C1,C2, R/binary>>, OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_paren(R_1, OL);
css_paren(<<C, R/binary>>, OL)
  when C =:= $( ->
    {Cont, R_1} = css_paren(R, []),
    css_paren(R_1, [ $) ] ++ lists:reverse(Cont) ++ [C|OL]);
css_paren(<<C, R/binary>>, OL)
  when C =:= $) ->
    {lists:reverse(OL), R};
css_paren(<<C, R/binary>>, OL) ->
    css_paren(R, [C|OL]).

%% The contents of a css comment
%%
css_cmt(R) ->
    css_cmt(R, []).
css_cmt(<<C1,C2, R/binary>>, OL)
  when C1 =:= $*, C2 =:= $/ ->
    {lists:reverse(OL), R};
css_cmt(<<C, R/binary>>, OL) ->
    css_cmt(R, [C|OL]).


%% The contents of a CSS single quote comment
%%
css_quot(R) ->
    css_quot(R, []).
css_quot(<<C1, R/binary>>, OL)
  when C1 =:= $' ->
    {lists:reverse(OL), R};
css_quot(<<C, R/binary>>, OL) ->
    css_quot(R, [C|OL]).


%% The contents of a CSS double quote string
%%
css_dquot(R) ->
    css_dquot(R, []).
css_dquot(<<C1, R/binary>>, OL)
  when C1 =:= 34 ->
    {lists:reverse(OL), R};
css_dquot(<<C, R/binary>>, OL) ->
    css_dquot(R, [C|OL]).


%% The contents of a media grouping.
%%
css_media(R) ->
    css_media(R, []).
css_media(<<C1,C2, R/binary>>, OL)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_media(R_1, OL);
css_media(<<C, R/binary>>, OL)
  when C =:= ${ ->
    {Cont, R_1} = parse_css_1(R, []),
    {css_media_q(lists:reverse(OL)), Cont, R_1};
css_media(<<C, R/binary>>, OL) ->
    css_media(R, [C|OL]).

css_media_q(L) ->
    css_media_q(iolist_to_binary(L), []).
css_media_q(<<C,_/binary>>=R, OL)
  when C >= $A andalso C =< $Z;
       C >= $a andalso C =< $z;
       C >= $0 andalso C =< $9 ->
    {Cont, R_1} = css_word(R),
    css_media_q(R_1, [{w,Cont}|OL]);
css_media_q(<<C,R/binary>>, OL)
  when C =:= $( ->
    {Cont, R_1} = css_paren(R),
    css_media_q(R_1, [{p,Cont}|OL]);
css_media_q(<<C,R/binary>>, OL)
  when ?IS_WS(C) ->
    css_media_q(R, OL);
css_media_q(<<>>, OL) ->
    lists:reverse(OL).


%% The contents of a unknown @ grouping.
%%
css_unknown(<<C1,C2, R/binary>>)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_unknown(R_1);
css_unknown(<<C, R/binary>>)
  when C =:= ${ ->
    {ok, R_1} = css_unknown_1(R),
    {ok, R_1};
css_unknown(<<_, R/binary>>) ->
    css_unknown(R).
css_unknown_1(<<C1,C2, R/binary>>)
  when C1 =:= $/, C2 =:= $* ->
    {_, R_1} = css_cmt(R),
    css_unknown_1(R_1);
css_unknown_1(<<C, R/binary>>)
  when C =:= ${ ->
    {_, R_1} = css_unknown_1(R),
    css_unknown_1(R_1);
css_unknown_1(<<C, R/binary>>)
  when C =:= $} ->
    {ok, R};
css_unknown_1(<<_, R/binary>>) ->
    css_unknown_1(R).


%% Load @import statements
%%
load_css_import(Toks, CurDir, Loaded) ->
    load_css_import(Toks, CurDir, Loaded, []).
load_css_import([{import, File}|Toks], CurDir, Loaded, OL) ->
    case load_css_import_local(File, CurDir) of
        %% Found a local file
        {ok, File_1} ->
            case gb_sets:is_element(File_1, Loaded) of
                false ->
                    io:format("~w: NOTE: Loading: ~s~n", [?MODULE, File_1]),
                    {ok, {MoreToks, Loaded_1}} = parse_stylesheet_file_1(File_1, css, Loaded),
                    load_css_import(Toks ++ [ws] ++ MoreToks, CurDir, Loaded_1, OL);
                true ->
                    io:format("~w: NOTE: Already loaded ~p~n", [?MODULE, File]),
                    load_css_import(Toks, CurDir, Loaded, OL)
            end;
        %% Not a local file, or not found
        false ->
            load_css_import(Toks, CurDir, Loaded, OL)
    end;
load_css_import([T|Toks], CurDir, Loaded, OL) ->
    load_css_import(Toks, CurDir, Loaded, [T|OL]);
load_css_import([], _CurDir, Loaded, OL) ->
    {lists:reverse(OL), Loaded}.


%% Make sure a file is local, and that it exists
%%
load_css_import_local([D,$:,S|_]=File, CurDir)
  when (S =:= $/ orelse S =:= $\\),
       (D >= $A andalso D =< $Z) orelse
       (D >= $a andalso D =< $z) ->
    load_css_import_local_1(File, CurDir);
load_css_import_local(File, CurDir) ->
    case string:find(File, ":") of
        nomatch ->
            %% No uri scheme
            load_css_import_local_1(File, CurDir);
        _ ->
            %% Might be an URL, return false
            false
    end.

load_css_import_local_1(File, CurDir) ->
    File_1 = filename:join(CurDir, File),
    case string:lowercase(filename:extension(File)) of
        ".css" ->
            case file:read_file_info(File_1) of
                {error, _} ->
                    io:format("~w: NOTE: CSS File not found: ~s~n", [?MODULE, File_1]),
                    false;
                _ ->
                    {ok, File_1}
            end;
        Ext ->
            io:format("~w: NOTE: CSS File unexpected extension: ~s~n", [?MODULE, Ext]),
            false
    end.

%%%
%%%

%% Some software output the same path twice as two elements in their SVG files,
%% one for the fill and another for the outline, this can add extra unnecessary
%% processing time or even confuse the mesh calculations, so the two
%% identical paths are merged.
%%
remove_double_paths(PathsList) ->
    remove_double_paths(PathsList, []).
remove_double_paths([{path, #path_tag_r{d=Path_1,id=Id_1,style=Style_1,texture=Texture_1}=PathTagR1, M_1},
    {path, #path_tag_r{d=Path_2,id=Id_2,style=Style_2,texture=Texture_2}=_, _M_2}|R], O) when Path_1 =:= Path_2 ->
    remove_double_paths(R, [
        {path, PathTagR1#path_tag_r{d=Path_1,
            id=remove_double_paths_merge_id(Id_1, Id_2),
            style=remove_double_paths_merge_style(Style_1, Style_2),
            texture=remove_double_paths_merge_tex(Texture_1, Texture_2)}, M_1}
        |O]);
remove_double_paths([PathItem|R], O) ->
    remove_double_paths(R, [PathItem|O]);
remove_double_paths([], O) ->
    lists:reverse(O).
remove_double_paths_merge_id(Id_1, Id_2) ->
    case Id_1 of
        "none" -> Id_2;
        _      -> Id_1
    end.
-spec remove_double_paths_merge_style(none | #style_colors{}, none | #style_colors{}) -> #style_colors{}.
remove_double_paths_merge_style(#style_colors{scol=S1,fcol=F1,fopa=A1}=_Style_1,
                                #style_colors{scol=S2,fcol=F2,fopa=A2}=_Style_2) ->
    #style_colors{
        scol=whichever_not_none(S1, S2),
        fcol=whichever_not_none(F1, F2),
        fopa=whichever_not_none(A1, A2)};
remove_double_paths_merge_style(#style_colors{}=Style_1, _) ->
    Style_1;
remove_double_paths_merge_style(_, #style_colors{}=Style_2) ->
    Style_2.
whichever_not_none(NotTuple, A2) when not is_tuple(NotTuple) ->
    A2;
whichever_not_none(A1, _) ->
    A1.
remove_double_paths_merge_tex(NotTuple, Texture_2) when not is_tuple(NotTuple) ->
    Texture_2;
remove_double_paths_merge_tex(Texture_1, _) ->
    Texture_1.

%% Second try to merge near identical paths, by comparing paths at the #cedge{} list level.
%% Sometimes there might be a very small difference between the paths.
%%
remove_double_objs(PathsList) ->
    remove_double_objs(PathsList, []).
remove_double_objs([{SubPaths1, {Col1,Tex1}},
                    {SubPaths2, {Col2,Tex2}}|R], O)
  when length(SubPaths1) =:= length(SubPaths2) ->
    case similar_subpaths(SubPaths1, SubPaths2) of
        true ->
            Col3 = remove_double_paths_merge_style(Col1, Col2),
            Tex3 = remove_double_paths_merge_tex(Tex1, Tex2),
            remove_double_objs(R, [{SubPaths1, {Col3,Tex3}}|O]);
        false ->
            remove_double_objs([{SubPaths2, {Col2,Tex2}}|R], [{SubPaths1, {Col1,Tex1}}|O])
    end;
remove_double_objs([{_SubPaths1,_}=PathItem|R], O) ->
    remove_double_objs(R, [PathItem|O]);
remove_double_objs([], O) ->
    lists:reverse(O).

similar_subpaths([CEdge1|R1],[CEdge2|R2])
  when length(CEdge1) =:= length(CEdge2) ->
    case similar_cedge_list(CEdge1, CEdge2) of
        true ->
            similar_subpaths(R1, R2);
        false ->
            false
    end;
similar_subpaths(_,[_CEdge1|_]) ->
    false;
similar_subpaths([_CEdge1|_],_) ->
    false;
similar_subpaths([], []) ->
    true.

similar_cedge_list(CL1,CL2) when length(CL1) =:= length(CL2) ->
    lists:all(fun similar_cedge_list_1/1, lists:zip(CL1,CL2));
similar_cedge_list(_,_) ->
    false.
similar_cedge_list_1({
    #cedge{vs=Vs1,cp1=C11,cp2=C21,ve=Ve1},
    #cedge{vs=Vs2,cp1=C12,cp2=C22,ve=Ve2}
}) ->
    similar_flt(Vs1, Vs2) andalso
    similar_flt(C11, C12) andalso
    similar_flt(C21, C22) andalso
    similar_flt(Ve1, Ve2).
-define(SIMILARITY_MULT, 100000).
similar_flt(nil, nil) ->
    true;
similar_flt(nil, {_, _}) ->
    false;
similar_flt({_, _}, nil) ->
    false;
similar_flt({X1, Y1}, {X2, Y2}) ->
    (round(X1 * ?SIMILARITY_MULT) =:= round(X2 * ?SIMILARITY_MULT)) andalso
    (round(Y1 * ?SIMILARITY_MULT) =:= round(Y2 * ?SIMILARITY_MULT)).


%% Filter polyareas to have at least 3 cedges, it is possible for a
%% well formed SVG to have 2 cedges that are bezier curves for a shape,
%% but at 0 subdivisions, these 2 cedge polyareas will cause an error later on
%% in wpc_ai:polyareas_to_faces/1.
filter_polyareas_min_3_cedges(Pas_1) ->
    [[ P1 || P1={polyarea,CEdges,_} <- L, length(CEdges) > 2 ] || L <- Pas_1].


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
                io:format(?__(1, "SVG Import error on skipped shape~n"), []),
                io:format(?__(2, 
                    "~p: NOTE: A shape has been skipped due to key_exists "
                    "error in wpc_ai:polyareas_to_faces~n"), [?MODULE]),
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


-ifdef(TEST).

%%
%% Style sheet tests
%%

t_sty_0() ->
    S0 = [{"path","path2",["cls3"]},{"g",none,["cls2"]},{"g","layer1",["cls1"]}],
    A = [
            {linkrel,[{"rel", "stylesheet"},
             {"href", "test.css"},
             {"type", "text/css"}]},
            {linkrel,[{"rel", "stylesheet"},
             {"href", "sty2.css"},
             {"type", "text/css"}]},
            {linkrel,[{"rel", "stylesheet"},
             {"href", "sty3.css"},
             {"type", "text/css"}]}
        ],
    CSS = get_link_rel_css(A, "./"),
    match_css_1(CSS, S0, none).

t_sty_1() ->
    S0 = [{"path","path2",["cls3"]},{"g",none,["cls2"]},{"g","layer1",["cls1"]}],
    {ok, CSS} = parse_stylesheet_file("./test2.css", parse_stylesheet_type("text/css")),
    match_css_1(CSS, S0, none).

t_sty_2() ->
    SVGL=[{e,"g"},
          {e,"path"},
          {s,"path",
             [{style,{style_colors,opaque,none,inherit}},
              {d,"m 100.13143,193.47428 5.65714,-26.02285 35.64,-19.8 -10.18286,39.03428 z"},
              {id,"path2"}]},
          {s,"g",[{label,"Layer 1"},{id,"layer1"}]},
          {e,"defs"},
          {s,"defs",[]}],
    {ok, F} = file:read_file("test.css"),    
    SVGL_1 = lists:reverse(SVGL),
    M = parse_stylesheet_list([{"text/css", F}], "./"),
    match_css(SVGL_1, M).

t_sty_3() ->
    extract_styles_attr("type", <<" style=\"a b\" x y z type=\"a/b\" a style=b \"c\" d e">>).
t_sty_4() ->
    S0 = [{"path","path2",["cls3"]},{"g",none,["cls2"]},{"g","layer1",["cls1"]}],
    {ok, CSS} = parse_stylesheet_file("./test.css", parse_stylesheet_type("text/css")),
    match_css_1(CSS, S0, none).
t_sty() ->
    A = <<"<svg>\n",
          "<style type=\"text/css\">\n",
          "<!--\n",
          "circle {color:red}\n",
          "-->\n",
          "</style>\n",
          "<STYLE>circle {color:green}</STYLE>\n",
          "<Style type=\"text/css\">\n",
          "circle {color:blue}\n",
          "</Style>\n",
          "<circle/>\n",
          "<script>\n",
          "etc\n",
          "</script>\n",
          "</svg>\n">>,
    {SVG_1, StyleList} = extract_styles(A),
    {SVG_1, StyleList}.

-endif().

