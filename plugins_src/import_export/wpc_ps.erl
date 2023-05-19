%%
%%  wpc_ps.erl --
%%
%%    Adobe PostScript (*.ps/*.eps) import based on wpc_ai.erl by Howard Trickey
%%    To work, the wpc_ai plugin must also be loaded.
%%
%%  Copyright (c) 2009-2011 Richard Jones.
%%                2017 Micheus (add/fixed support to Adobe Illustrator, LibreOffice, Inkscape and scribus (partial)).
%%                2023 Edward Blake (color support)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_ps).
-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

%%%
%%%

-record(cedge,% polyarea and cedge records must match definitions in wpc_ai.erl
        {vs,cp1=nil,cp2=nil,ve}).   %all are {x,y} pairs

-record(path,
        {ops=[],            %list of pathops
         close=false}).     %true or false

-record(pathop,
        {opkind,            %pmoveto, plineto, or pcurveto
         x1=0.0,
         y1=0.0,
         x2=0.0,
         y2=0.0,
         x3=0.0,
         y3=0.0}).

-type m3x2() :: {float(),float(),float(),float(),float(),float()}.

-type texinfo() :: {m3x2(), integer()}.

-record(coltex,
        {fcol=none :: {float(),float(),float()} | none,
         scol=none :: {float(),float(),float()} | none,
         tex=none  :: texinfo() | none
        }).

-record(pstate,
        {curpath=#path{}   :: #path{},    %current path
         objects=[]        :: [[#path{}]],     %object list (paths)
         objects_coltex=[] :: [#coltex{}],
         curobjs=[]        :: [#path{}],       %current object been processed
         curobj_col=none   :: {float(),float(),float()} | none,
         curobj_clip=[]    :: [[#path{}]],
         ctm = {1.0,0.0,0.0,1.0,0.0,0.0},
         ctms=[]                        % Stack of CTMs
         }).

%%%
%%%

init() -> true.

menu({file,import}, Menu) ->
    Menu ++ [{"Adobe PostScript (.ps|.eps)...", ps, [option]}];
menu(_, Menu) -> Menu.

more_info() ->
    [?__(1,"<b>Automatic center</b>\n"
    "Automatically center the imported shape.\n\n"
    "<b>Scale fraction</b>\n"
    "Set the scale ratio of 1 unit in wings to the units in the PS/EPS. "
    "If set to 100pt, then 100pt is the same as 1 unit in wings. Available "
    "units include pt, pc, cm, mm, and in.\n\n"
    "<b>Scale fit within view</b>\n"
    "Rescale the shape to fit in view. "
    "This means the scaling based on document units is ignored.\n\n"
    "<b>Use custom bind map file</b>\n"
    "When checked, the bind map file will be used when importing PS/EPS files. "
    "An explanation of the bind map file and why they might be useful are "
    "explained further below.\n\n"
    "<b>Custom Bind Map Files:</b>\n\n"
    "Custom bind maps is a file that can be made in a text editor that ends "
    "with the file extension .bindmap. It can be used to help the PS/EPS path importer "
    "to understand the commands in a PS/EPS file. The file contains bind "
    "definitions that maps custom commands (bind def) and also has commands "
    "that set fixes flags and options of the importer.\n\n"
    "The beginning of the file should look like this:\n\n"
    "%%Match: (XFIG)\n"
    "%%Comment: (Xfig)\n"
    "\n"
    "The first line is for the importer to match to the PS/EPS creator string, "
    "this string should be in uppercase and contain a keyword from the PS/EPS "
    "Creator comment line. The second line is just a comment and isn't "
    "currently used. Both lines the strings must be inside parenthesis.\n\n"
    "<b>Define commands from other commands</b>\n\n"
    "Forms recognized:\n\n"
    "/cmd { actualcommand } bind def\n"
    "/cmd { neg actualcommand } bind def\n"
    "/cmd/actualcommand load def\n"
    "\n"
    "Examples:\n\n"
    "/m/moveto load def\n"
    "/l { lineto } bind def\n"
    "/cv { curveto } bind def\n"
    "\n"
    "<b>Fix flags</b>\n\n"
    "The importer can be configured to make special commands available using "
    "the fix command, they follow the following form:\n\n"
    "/flag fix\n"
    "/flag /parameter fix\n"
    "\n"
    "If you want to add a inverted Y curveto command called 'ct', use:\n\n"
    "/curveto_invy /ct fix\n"
    "\n"
    "If you want to add a rectangle command called 're', use:\n\n"
    "/rect /re fix\n"
    "\n"
    "<b>Coordinate system direction:</b>\n\n"
    "If you want to invert the Y axis of the importer, use:\n"
    "1.0 -1.0 coordsys\n"
    "\n"
    "<b>Split mode:</b>\n"
    "\n"
    "Some files may not use a specific keyword or comment to note where the commands start. If so, use:\n\n"
    "/whole_eps beginmode\n"
    "\n"
    "If the file uses a %%Page: 1 1 comment to note where the commands start, use:\n\n"
    "/begin_at_page1 beginmode\n"
    "\n")].




info_button() ->
    Title = ?__(1,"EPS/PS Import Information"),
    TextFun = fun () -> more_info() end,
    {help,Title,TextFun}.

command({file,{import,{ps,Ask}}}, _St) when is_atom(Ask) ->
    DefBisect = wpa:pref_get(?MODULE, ps_bisections, 0),
    AutoScale = wpa:pref_get(?MODULE, ps_auto_scale, false),
    DefCustomBindMap = wpa:pref_get(?MODULE, ps_use_bindmap, false),
    DefCustomBindMapPath = wpa:pref_get(?MODULE, ps_use_bindmap_path, ""),
    %% Force SetScale to be text
    case wpa:pref_get(?MODULE, ps_set_scale, "100pt") of
        Number when is_float(Number); is_integer(Number) ->
            SetScale = lists:flatten(io_lib:format("~p", [Number]));
        Str when is_list(Str) ->
            SetScale = Str
    end,
    Hook_Auto_Scale = fun(_Key, Value, Store) ->
        wings_dialog:enable(set_scale, Value =:= false, Store)
    end,
    Hook_Use_Bind = fun(_Key, Value, Store) ->
        wings_dialog:enable(custom_bind_map_path, Value =:= true, Store)
    end,
    BindMapProps = [{dialog_type,open_dialog}, {extensions,[{".bindmap",?__(16,"Bind Map File")}]}, {title,?__(15,"Choose bind map file")}],
    Dialog =
        [{hframe,[{label,?__(2,"Number of edge bisections")},
                  {text,DefBisect,[{key,bisections}]}]},
         {hframe,[{label,?__(7,"Scale fraction") ++ ": 1 / "},
                  {text,SetScale,[{key,set_scale}]}]},
         {?__(8,"Automatic center"),true,[{key,auto_center}]},
         {?__(9,"Scale fit within view"),AutoScale,
             [{key,auto_scale_fit}, {hook, Hook_Auto_Scale},
              {info, ?__(10,"Automatically rescale shapes to fit within the camera view.")}]},
         {?__(11, "Use custom bind map file"), DefCustomBindMap,
             [{key, custom_bind_map},{hook,Hook_Use_Bind},
              {info, ?__(12,"Use a custom bind map file to define commands needed to parse an EPS file.")}]},
         {hframe,[{button,{text,DefCustomBindMapPath,[{key,custom_bind_map_path},{width,30},{props,BindMapProps}]}}]},
         panel,
         {hframe,[info_button()]},
         {hframe,[{label,?__(13,"Importing an EPS from an unsupported EPS creator might require \n"
                                "creating a bind map file, read help for more info.")}]}
         ],
    wpa:dialog(Ask, ?__(4,"PS/EPS Import Options"), Dialog,
            fun(Res) -> {file,{import, ps, Res}} end);
command({file,{import, ps, Attr}}, St) ->
    Nsub_0 = proplists:get_value(bisections, Attr, 1),
    AutoScale = proplists:get_value(auto_scale_fit, Attr, false),
    SetScale_S = proplists:get_value(set_scale, Attr, "100pt"),
    AutoCenter = proplists:get_value(auto_center, Attr, true),
    UseCustomBindMap = proplists:get_value(custom_bind_map, Attr, false),
    UseCustomBindMapPath = proplists:get_value(custom_bind_map_path, Attr, ""),
    case Nsub_0 < 0 of
        true -> Nsub = 0;
        false -> Nsub = Nsub_0
    end,
    Props = [{extensions,[{".ps",?__(5,"PostScript File")},
                          {".eps",?__(6,"Encapsulated PostScript File")}]}],
    bind_map_chooser(UseCustomBindMap, UseCustomBindMapPath,
        fun(ConfFilename) ->
            wpa:pref_set(?MODULE, ps_use_bindmap, UseCustomBindMap),
            wpa:pref_set(?MODULE, ps_use_bindmap_path, UseCustomBindMapPath),
            wpa:import(Props,
                fun(F) ->
                    make_ps(F, Nsub, AutoScale, SetScale_S, AutoCenter, ConfFilename)
                end, St)
        end);

command(_, _) ->
    next.


%% If the option is enabled, provide the bind map filename.
%%
bind_map_chooser(false, _, Fun) ->
    Fun(none);
bind_map_chooser(true, "", Fun) ->
    Fun(none);
bind_map_chooser(true, Path, Fun) ->
    case file:read_file_info(Path) of
        {error, _} ->
            wpa:error_msg(?__(1,"Bind map not found")),
            keep;
        _ ->
            Fun(Path)
    end.


-record(epq, {
    s,        %% String to find
    comment,  %% Comment about the quirks entry
    id,       %% Id atom
    coordsys, %% Coordinate direction
    fixes :: [atom()|tuple()],    %% List of fixes
    split     %% Split function
}).

%% Built-in quirks table for different EPS creators
%%
-spec default_quirks() -> [#epq{}].
default_quirks() ->
    [
        #epq{ s={str,"ADOBE"},        comment="Adobe",
                                      id=adobe,
                                      coordsys={1.0, 1.0},
                                      fixes=[adobe_cmds],
                                      split=begin_at_page1},
        
        #epq{ s={str,"AFFINITY"},     comment="Affinity",
                                      id=affinity,
                                      coordsys={1.0, 1.0},
                                      fixes=[affinity_cmds],
                                      split=whole_eps},
        
        #epq{ s={str,"LIBREOFFICE"},  comment="LibreOffice",
                                      id=libre_office,
                                      coordsys={1.0, 1.0},
                                      fixes=[libreoff_cmds, {curveto_invy, "ct"}],
                                      split=begin_at_page1},
        
        #epq{ s={str,"CAIRO"},        comment="InkScape",
                                      id=inkscape,
                                      coordsys={1.0, 1.0},
                                      fixes=[fix_close,inkscape_cmds,{concat_nobrackets, "cm"},{rect,"re"}],
                                      split=begin_at_page1},
        
        #epq{ s={str,"SCRIBUS"},      comment="Scribus",
                                      id=scribus,
                                      coordsys={1.0, 1.0},
                                      fixes=[scribus_cmds],
                                      split=begin_at_page1},
        
        %% Cairo is used by both inkscape and ipe, the eps output looks the same
        
        %% An .eps file with "generic" in its creator ID will be assumed to have simple commands
        #epq{ s={str,"GENERIC"},      comment="Generic",
                                      id=generic,
                                      coordsys={1.0, 1.0},
                                      fixes=[],
                                      split=whole_eps}
    ].

%%%
%%%

%% Quirks details for the given EPS file being imported.
-record(quirksdetails, {
    direction = {1.0, -1.0} :: {float(), float()},
    fixes = []              :: [atom()|tuple()]
}).

%% Pseudo-command generated by the importer to substitute embed image data.
-define(W3DEMBEDIMG, "w3dembedimg*").


make_ps(Filename, Nsubsteps, AutoScale, SetScale_S, AutoCenter, ConfFilename) ->
    case parse_float_number_w_unit(SetScale_S, 0.0) of
        {ScaleVal, ScaleUnit} when ScaleVal > 0.001 ->
            wpa:pref_set(?MODULE, ps_set_scale, SetScale_S),
            SetScale = {ScaleVal, unit_atom(ScaleUnit)};
        _Unk ->
            SetScale = {100.0, pt}
    end,
    try try_import_ps(Filename, Nsubsteps, AutoScale, SetScale, AutoCenter, ConfFilename) of
        {ok, E3dFile} ->
            wpa:pref_set(?MODULE, ps_bisections, Nsubsteps),
            wpa:pref_set(?MODULE, ps_auto_scale, AutoScale),
            {ok, E3dFile};
        {error,Reason} ->
            {error, ?__(1,"PS import failed")++": " ++ Reason}
    catch EClass:E:ST ->
            io:format("File Import Error Report:\n ~p ~p\nstack trace: ~p\n",
                [EClass, E, ST]),
            {error, ?__(2,"PS import internal error")}
    end.

setup_quirks_table(none) ->
    %% No custom file, use default quirks table
    default_quirks();
setup_quirks_table(ConfFilename)
  when is_list(ConfFilename) ->
    %% Add custom fixes to the start of the default quirks table
    {ok, Custom} = read_custom_conf(ConfFilename),
    [to_epq(Custom)|default_quirks()].

try_import_ps(Filename, Nsubsteps, AutoScale, SetScale, AutoCenter, ConfFilename) ->

    QuirksTab = setup_quirks_table(ConfFilename),
    case read_ps_content(Filename) of
        {ok,<<"%!PS-Adobe",_/binary>>=Rest} ->
            ShortFilename = filename:rootname(filename:basename(Filename)),
            
            case tokenize_bin_ps(Rest, QuirksTab) of
                {{error,no_token}, Creator} ->
                    {error, Creator ++ "\n"++?__(2,"File doesn't have a valid token structure")};
                {{error,unsupported}, Creator} ->
                    {error, Creator ++ "\n"++?__(3,"File creator unsupported")};
                {{ok, Objs0, ImgList, {QuirksDetails, ColTex0}}, Creator} ->
                    Objs = break_grouped_moveto(Objs0),
                    {Closedpaths, ColTex} = lists:unzip(
                        [{[P || P <- Obj, P#path.close == true, length(P#path.ops) > 2], CoTx}
                            || {Obj,CoTx} <- lists:zip(Objs,ColTex0), Obj=/=[]]),
                    
                    case Closedpaths of
                        [] -> {error, Creator ++ "\n"++?__(4,"File mismatch or doesn't have valid paths")};
                        _ ->
                            case AutoScale of
                                true ->
                                    [CamDist] = wpa:camera_info([distance_to_aim]),
                                    Scale = calculate_rescale_amount(CamDist, CamDist, Closedpaths);
                                _ ->
                                    Rescale_Denom = conv_unit(SetScale, pt),
                                    Scale = 1.0 / Rescale_Denom
                            end,
                            Closedpaths0 = do_fixes(QuirksDetails, Closedpaths),
                            Cntrs0 = getcontours(QuirksDetails, Scale, Closedpaths0),
                            Cntrs = reverse_def(Cntrs0),
                            %% giving some information to the user about possible absent objects
                            if length(Cntrs) =/= length(Objs) ->
                                io:format("~ts: ~ts\n",
                                    [Creator,
                                     ?__(5,"Some token structures were not valid in the file and were ignored")]);
                                true -> ok
                            end,
                            MatList = materials_for_imglist(ShortFilename, ImgList),
                            Pas0 = [ wpc_ai:findpolyareas(Cntr) || Cntr <- Cntrs],
                            Pas1 = [ wpc_ai:subdivide_pas(Pa,Nsubsteps) || Pa <- Pas0],
                            ColTex1 = repeat_coltex_if_needed(ColTex, Pas1),
                            Pas2 = lists:append(Pas1),
                            List = process_islands(Pas2),
                            Colors = [fill_color(C) || #coltex{fcol=C}=_ <- ColTex1],
                            TexList = [tex_atom(TexInfo, ShortFilename, ImgList, Scale)
                                         || #coltex{tex=TexInfo} <- ColTex1],
                            {Vs0,Efs,Tx,HEs} = into_mesh_parts(List, TexList),
                            case AutoCenter of
                                true ->
                                    Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
                                    Vec = e3d_vec:sub(e3d_vec:zero(),Center),
                                    Vs = lists:reverse(center_object(Vec,Vs0));
                                _ ->
                                    Vs = Vs0
                            end,
                            Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,tx=Tx,vc=Colors,he=HEs},
                            Obj = #e3d_object{name=ShortFilename,obj=Mesh,mat=MatList},
                            {ok, #e3d_file{objs=[Obj]}}
                    end
            end;
        {ok,_} ->
            {error,?__(1,"Not an Adobe PostScript file")};
        {error,Reason} ->
            {error,file:format_error(Reason)}
    end.


read_ps_content(Filename) ->
    case file:read_file(Filename) of
        %% Binary wrapped
        {ok, <<16#C5,16#D0,16#D3,16#C6,_/binary>>=Cont} ->
            find_sig(bin_wrapper(Cont));

        %% Postscript text
        {ok, Cont} ->
            find_sig(Cont);
        
        {error, Err} ->
            {error, Err}
    end.


%% The signature should be near the beginning, 
%% allowing for some new lines
find_sig(<<C,R/binary>>)
  when C =:= 10; C =:= 13; C =:= 32 ->
    find_sig(R);
find_sig(<<"%!PS",_/binary>>=Cont) ->
    %% Signature found, return with the contents
    {ok, Cont};
find_sig(_) ->
    {error, no_signature}.


%% Bundled encapsulated postscript and preview image wrapper
-define(UINTLE, little-unsigned-integer).
bin_wrapper(<<16#C5,16#D0,16#D3,16#C6,R1/binary>>=Bin) ->
    <<
      OffsetStart:32/?UINTLE, % Start offset of postscript
      OffsetEnd:32/?UINTLE,   % End offset of postscript
      _Reserved1:32/?UINTLE,  % Usually all zeros
      _Reserved2:32/?UINTLE,  % Usually all zeros
      _PrvOffset:32/?UINTLE,  % Start offset of preview image
      _PrvLength:32/?UINTLE,  % End offset of preview image
      16#FF,16#FF,            % End marker
      _/binary>> = R1,
    binary:part(Bin, {OffsetStart, OffsetEnd-8}).


%%% put the object definition in reverse order to build taces with valid normals
reverse_def(Contours) ->
    [[lists:reverse(Cntr) || Cntr <- Cntrs] || Cntrs <- Contours, Cntrs=/=[[]]].

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
                io:format(?__(1, "PS/EPS Import error on skipped shape~n"), []),
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

%%%
%%%

repeat_coltex_if_needed(MatList, Pas) ->
    repeat_coltex_if_needed(MatList, Pas, []).
repeat_coltex_if_needed([], [], O) ->
    lists:append(lists:reverse(O));
repeat_coltex_if_needed([M|MatList], [P|Pas], O) ->
    M_1 = [M || _I <- lists:seq(1, length(P))],
    repeat_coltex_if_needed(MatList, Pas, [M_1|O]).


%%% fixes the vertex number relative to the entire object to be created.

into_mesh_parts(Objs, TexList) ->
    into_mesh_parts(Objs, TexList, 0, 0, 0, [], [], [], []).
into_mesh_parts([], _, _, _, _, Vs_L, Fs_L, Tx_L, He_L) ->
    {lists:append(lists:reverse(Vs_L)),
     lists:append(lists:reverse(Fs_L)),
     lists:append(lists:reverse(Tx_L)),
     lists:append(lists:reverse(He_L))};
into_mesh_parts([{Vs,Fs0,He0} | Objs], [Tex | TexList], ColorIdx,
                VsOffset, TxOffset, Vs_L, Fs_L, Tx_L, He_L) ->
    case Tex of
        none ->
            Tx = [],
            TxOffset_1 = TxOffset;
        {BitmapCTM, _} ->
            Tx = [to_uv(BitmapCTM, X, Y) || {X,Y,_} <- Vs],
            TxOffset_1 = TxOffset + length(Vs)
    end,
    VsOffset_1 = VsOffset + length(Vs),
    EFs = [ #e3d_face{
        vs=[V+VsOffset || V <- F],
        vc=[ColorIdx   || _ <- F],
        tx=[V+TxOffset || V <- F],
        mat=
            case Tex of
                none -> [];
                {_, M} -> [M]
            end
    } || F <- Fs0],
    He = [{V1+VsOffset,V2+VsOffset} || {V1,V2} <- He0],
    
    Vs_1 = Vs,
    
    into_mesh_parts(Objs, TexList, ColorIdx + 1, VsOffset_1, TxOffset_1,
                    [Vs_1 | Vs_L], [EFs | Fs_L], [Tx | Tx_L], [He | He_L]).

to_uv({ScaleX, _, _, ScaleY0, TrX, TrY0}, X, Y) ->
    ScaleY = -ScaleY0,
    TrY = -TrY0,
    UV = {(X-TrX) * ScaleX, (Y-TrY) * ScaleY},
    UV.

fill_color(none) ->
    {0.4, 0.4, 0.4};
fill_color({_R,_G,_B}=Col) ->
    Col.

new_tex_atom(ShortFilename, TexNumber) ->
    list_to_atom(ShortFilename ++ "_" ++ integer_to_list(TexNumber)).

tex_atom({CTM, TexNumber}, ShortFilename, ImgList, Scale)
  when is_list(ImgList), length(ImgList) > TexNumber ->
    case lists:nth(TexNumber + 1, ImgList) of
        unused ->
            none;
        {A, _} when A =:= jpeg ->
            AtomName = new_tex_atom(ShortFilename, TexNumber),
            {rescale_image_ctm(CTM, Scale), AtomName}
    end;
tex_atom(_, _, _, _) ->
    none.

%% Rescale image texture placement
rescale_image_ctm({M1,M2,M3,M4,M5,M6}, Scale) ->
    {M1*Scale,M2,M3,M4*Scale,M5*Scale,M6*Scale}.

%% Textures list for e3d_file
materials_for_imglist(ShortFilename, ImgList) when is_list(ShortFilename) ->
    materials_for_imglist(ShortFilename, ImgList, 0, []).
materials_for_imglist(_ShortFilename, [], _I, OMat) ->
    lists:reverse(OMat);
materials_for_imglist(ShortFilename, [unused|R], I, OMat) ->
    materials_for_imglist(ShortFilename, R, I + 1, OMat);
materials_for_imglist(ShortFilename, [{_, _}=ImgData|R], I, OMat) ->
    case get_bitmap(ImgData) of
        {ok, #e3d_image{}=E3dImage} ->
            AtomName = new_tex_atom(ShortFilename, I),
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
            M = {AtomName, [Maps, OpenGL]},
            materials_for_imglist(ShortFilename, R, I + 1, [M | OMat])
    end.


%% some paths definitions can contain many 'pmoveto' operators that cannot be understand
%% for the code in wpc_ai module. Then, we break them in separated objects in order to
%% provide user with most objects as he/she expect to get.
break_grouped_moveto(Objs) ->
    [break_grouped_moveto_0(Paths,[]) || Paths <- Objs].
break_grouped_moveto_0([], Acc) -> lists:flatten(Acc);
break_grouped_moveto_0([#path{ops=Ops0}=Path0|Paths], Acc0) ->
    Acc =
        case break_grouped_moveto_1(lists:reverse(Ops0),[],[]) of
            [Ops0] -> Path0;
            NewOps -> [#path{ops=NOps,close=true} || NOps <- NewOps]
        end,
    break_grouped_moveto_0(Paths, [Acc|Acc0]).
break_grouped_moveto_1([], [], Acc) -> Acc;
break_grouped_moveto_1([], Ops0, Acc) -> [Ops0|Acc];
break_grouped_moveto_1([#pathop{opkind=pmoveto}=Path|Paths], Ops0, Acc) ->
    Ops = [Path|Ops0],
    break_grouped_moveto_1(Paths, [], [Ops|Acc]);
break_grouped_moveto_1([Path|Paths], Ops0, Acc) ->
    break_grouped_moveto_1(Paths, [Path|Ops0], Acc).


center_object(Vec,Vs) ->
    lists:foldl(fun(V,Acc) ->
        {X,Y,Z} = e3d_vec:add(V,Vec),
        [{X,Y,Z}|Acc]
    end,[],Vs).

tokenize_bin_ps(Bin, QuirksTab) ->
    {Bin1, Creator, SplitFun} = find_creator_ps(Bin, "Generic", QuirksTab),
    
    %% Split the postscript from the image data early on so tokenize_bin_ps
    %% doesn't have to tokenize through the encoded images.
    {ImgList, Bin2} = get_emb_images(Bin1),
    
    {BeforePageSetup, Chars} = after_end_setup_ps(Bin2, SplitFun),
    
    CommandMap0 = parse_prolog_ps(BeforePageSetup),
    case get_eps_fixes(Creator, QuirksTab) of
        {ok, _CreatorID, QuirksDetails} ->
            CommandMap = add_commands(CommandMap0, QuirksDetails),
            Toks = tokenize(Chars, []), % seems to be the same as what is needed for .ps
            
            case Toks of
                [] ->
                    Return = {error,no_token};
                _ ->
                    case parse_tokens_ps(Toks, CommandMap) of
                        {[], _} ->
                            Return = {error,no_token};
                        {Objs0, ColTex0} ->
                            %% remove duplicated items since some files may
                            %% contain the fill and strock data for the same path definition
                            {Objs, ColTex} = merge_duplicates(Objs0, ColTex0),
                            Return = {ok, Objs, ImgList, {QuirksDetails, ColTex}}
                    end
            end;
        false ->
            Return = {error,unsupported}
    end,
    {Return, Creator}.

-spec add_commands(map(), #quirksdetails{}) -> map().
add_commands(CommandMap, #quirksdetails{fixes=Fixes}=_QuirksDetails) ->
    add_commands_1(CommandMap, Fixes).
add_commands_1(CommandMap0, [{rect,Command}|Fixes]) ->
    CommandMap = CommandMap0#{
        %% 're' means rectangle for Inkscape file. we translate it to regular operations and close the path
        Command => {f4, fun({IX,IY,IW,IH},#pstate{ctm=CTM}=Pst) ->
            {X1,Y1} = ctm_appl(CTM, {IX,IY}),
            {X2,Y2} = ctm_appl(CTM, {IX,IY+IH}),
            {X3,Y3} = ctm_appl(CTM, {IX+IW,IY+IH}),
            {X4,Y4} = ctm_appl(CTM, {IX+IW,IY}),
            Pst0 = finishpop(#pathop{opkind=pmoveto,x1=X1,y1=Y1},Pst),
            Pst1 = finishpop(#pathop{opkind=plineto,x1=X2,y1=Y2}, Pst0),
            Pst2 = finishpop(#pathop{opkind=plineto,x1=X3,y1=Y3}, Pst1),
            Pst3 = finishpop(#pathop{opkind=plineto,x1=X4,y1=Y4}, Pst2),
            finishrop(true,Pst3)
        end}
    },
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [{concat_nobrackets, Command}|Fixes]) ->
    CommandMap = CommandMap0#{
        Command => {f6, fun({M1,M2,M3,M4,M5,M6}, Pst) ->
            finishpop(#pathop{opkind=concat,x1=M1,y1=M2,x2=M3,y2=M4,x3=M5,y3=M6},Pst)
        end}
    },
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [{curveto_invy, Command}|Fixes]) ->
    CommandMap = CommandMap0#{
        Command => {f6, fun({IX1,IY1,IX2,IY2,IX3,IY3},#pstate{ctm=CTM}=Pst) ->
            {X1,Y1} = ctm_appl(CTM, {IX1,-IY1}),
            {X2,Y2} = ctm_appl(CTM, {IX2,-IY2}),
            {X3,Y3} = ctm_appl(CTM, {IX3,-IY3}),
            finishpop(#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3},Pst)
        end}
    },
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [{merge_cmds, Map}|Fixes]) ->
    CommandMap = maps:merge(CommandMap0, Map),
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [inkscape_cmds|Fixes]) ->
    CommandMap = add_commands_if_absent(CommandMap0, [
        %% These will be added if they weren't already by bind def
        {"q", "gsave"},
        {"Q", "grestore"},
        {"m", "moveto"},
        {"l", "lineto"},
        {"c", "curveto"},
        {"h", "closepath"},
        {"S", "stroke"},
        {"f", "fill"},
        {"f*", "eofill"},
        {"n", "newpath"},
        {"W", "clip"},
        {"W*", "eoclip"}
    ]),
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [libreoff_cmds|Fixes]) ->
    CommandMap = add_commands_if_absent(CommandMap0#{"pc" => "closepath"}, [
        %% These will be added if they weren't already by bind def
        {"l", {neg, "lineto"}},
        {"rl", {neg, "rlineto"}},
        {"m", {neg, "moveto"}},
        {"r", "rotate"},
        {"t", {neg, "translate"}},
        {"s", "scale"},
        {"gs", "gsave"},
        {"gr", "grestore"},
        {"p", "closepath"},
        {"ef", "eofill"},
        {"ps", "stroke"}
    ]),
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [scribus_cmds|Fixes]) ->
    CommandMap = add_commands_if_absent(CommandMap0, [
        %% These will be added if they weren't already by bind def
        {"cmyk", "setcmykcolor"},
        {"m", "moveto"},
        {"l", "lineto"},
        {"li", "lineto"},
        {"cu", "curveto"},
        {"cl", "closepath"},
        {"gs", "gsave"},
        {"gr", "grestore"},
        {"tr", "translate"},
        {"ro", "rotate"},
        {"sc", "scale"},
        {"fi", "fill"},
        {"st", "stroke"}
    ]),
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [adobe_cmds|Fixes]) ->
    CommandMap = add_commands_if_absent(CommandMap0, [
        {"cv", "curveto"},
        {"li", "lineto"},
        {"ct", "concat"},
        {"np", "newpath"},
        {"mo", "moveto"},
        {"cp", "closepath"},
        {"clp_npth", "clip"}, %% Actually clip newpath
        {"clp", "clip"},
        {"cmyk", "setcmykcolor"},
        {"rgb", "setrgbcolor"},
        {"gry", "setgray"},
        {"f", "fill"},
        {"ef", "eofill"},
        {"@", "stroke"},
        {"nclp", "clip"},
        {"ct", "concat"}
    ]),
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap0, [affinity_cmds|Fixes]) ->
    CommandMap = add_commands_if_absent(CommandMap0, [
        {"m", "moveto"},
        {"l", "lineto"},
        {"c", "curveto"},
        {"setcolor", "setrgbcolor"}
    ]),
    add_commands_1(CommandMap, Fixes);
    
add_commands_1(CommandMap, [_|Fixes]) ->
    add_commands_1(CommandMap, Fixes);
add_commands_1(CommandMap, []) ->
    CommandMap.

%% Add command mappings but only if they were not already defined.
%% 
add_commands_if_absent(CommandMap0, List) ->
    maps:merge(maps:from_list(List),CommandMap0).


%% Parse the prolog part of the file
parse_prolog_ps(Bin1) ->
    Chars = binary_to_list(Bin1),
    Toks = tokenize(Chars, []),
    CommandMap = ps_pl_p(Toks),
    CommandMap.
    
-record(psplstate, {
    toks = [],
    in_curly = 0,
    cmdmap,
    bdef = unassigned
}).

ps_pl_p(L) ->
    ps_pl_p(L, #psplstate{cmdmap=maps:new()}).

ps_pl_p([{tstring}=A|R], #psplstate{toks=TL}=PS) ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL]});
ps_pl_p([{tnum, _}=A|R], #psplstate{toks=TL}=PS) ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL]});
ps_pl_p([{tlitname, _}=A|R], #psplstate{toks=TL}=PS) ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL]});
ps_pl_p([{tname, "{"}=A|R], #psplstate{toks=TL,in_curly=Crl}=PS) ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL],in_curly=Crl+1});
ps_pl_p([{tname, "}"}=A|R], #psplstate{toks=TL,in_curly=Crl}=PS) ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL],in_curly=max(0,Crl-1)});

ps_pl_p([{tname, C1},{tname, "def"=C2}|R], #psplstate{toks=TL,in_curly=0}=PS) ->
    PS1 = ps_pl_c({C1,C2},lists:reverse(TL),PS),
    ps_pl_p(R, PS1#psplstate{toks=[]});
ps_pl_p([{tname, C1}|R], #psplstate{toks=TL,in_curly=0}=PS) ->
    PS1 = ps_pl_c({C1},lists:reverse(TL),PS),
    ps_pl_p(R, PS1#psplstate{toks=[]});

ps_pl_p([{tname, _}=A|R], #psplstate{toks=TL,in_curly=Crl}=PS)
  when Crl > 0 ->
    ps_pl_p(R, PS#psplstate{toks=[A|TL]});
ps_pl_p([], #psplstate{cmdmap=CommandMap}=_) ->
    CommandMap.


%% Map bind definitions into a CommandMap
ps_pl_c({"load", "def"}, [{tlitname,ShortName},{tlitname,CallName}], #psplstate{cmdmap=CommandMap}=PS) ->
    PS#psplstate{cmdmap=CommandMap#{ShortName => CallName}};
ps_pl_c({"bind", "def"}, [{tlitname,ShortName},{tname,"{"},{tname,CallName},{tname,"}"}], #psplstate{cmdmap=CommandMap}=PS) ->
    PS#psplstate{cmdmap=CommandMap#{ShortName => CallName}};
ps_pl_c({"bind", "def"}, [{tlitname,ShortName},{tname,"{"},{tname,"neg"},{tname,CallName},{tname,"}"}], #psplstate{cmdmap=CommandMap}=PS) ->
    PS#psplstate{cmdmap=CommandMap#{ShortName => {neg, CallName}}};

ps_pl_c({BDef}, [{tlitname,ShortName},{tname,"{"},{tname,CallName},{tname,"}"}], #psplstate{bdef=BDef,cmdmap=CommandMap}=PS) ->
    PS#psplstate{cmdmap=CommandMap#{ShortName => CallName}};
ps_pl_c({BDef}, [{tlitname,ShortName},{tname,"{"},{tname,"neg"},{tname,CallName},{tname,"}"}], #psplstate{bdef=BDef,cmdmap=CommandMap}=PS) ->
    PS#psplstate{cmdmap=CommandMap#{ShortName => {neg, CallName}}};

%% For files that shorten bind def (e.g. libreoffice)
ps_pl_c({"bind", "def"}, [{tlitname,ShortName},{tname,"{"},{tname,"bind"},{tname,"def"},{tname,"}"}], PS) ->
    PS#psplstate{bdef=ShortName};
ps_pl_c(_, _, PS) ->
    PS.



%% Get the split function from the quirks table
%%
get_split_fun(Creator0, QuirksTab) ->
    get_split_fun_1(string:to_upper(Creator0), QuirksTab).
get_split_fun_1(Creator, [#epq{s={str, StrComp},split=SplitFun}|QuirksTab]) ->
    Idx = string:str(Creator, StrComp),
    if Idx > 0 ->
            {ok, split_function_from_atom(SplitFun)};
        true ->
            get_split_fun_1(Creator, QuirksTab)
    end;
get_split_fun_1(_Creator, []) ->
    {ok, split_function_from_atom(whole_eps)}.


%% Get the creator and quirk fixes from the quirks table
%%
get_eps_fixes(Creator0, QuirksTab) ->
    Creator = string:to_upper(Creator0),
    io:format("~w: EPS Creator: ~p\n",[?MODULE, Creator]),
    get_eps_fixes_1(Creator, QuirksTab).
get_eps_fixes_1(Creator, [#epq{s={str, StrComp},id=ID,coordsys=CDir,fixes=Fixes}|QuirksTab]) ->
    Idx = string:str(Creator, StrComp),
    if Idx > 0 ->
            {ok, ID, #quirksdetails{direction=CDir,fixes=Fixes}};
        true ->
            get_eps_fixes_1(Creator, QuirksTab)
    end;
get_eps_fixes_1(_Creator, []) ->
    false.


%% Split content on the <<"%%Page: 1 1">> line,
%% then convert commands part of binary to list of characters
find_creator_ps(<<"%%Creator:",Rest/binary>>, _, QuirksTab) ->
    <<Line1:255/binary,_/binary>> = Rest,
    Line0 = binary_to_list(Line1),
    Idx = string:str(Line0, "%"),
    Line = string:sub_string(Line0, 1, Idx-1),
    Creator0 = string:strip(string:strip(string:strip(Line, right, $\n), right, $\r), both),
    case remove_parenthesis(Creator0) of
        "" ->
            Creator = "Generic";
        _ ->
            Creator = Creator0
    end,
    Rest1 = find_creator_ps_after_nl(Rest),
    find_creator_ps_1(Rest1, Creator, QuirksTab);
find_creator_ps(<<NL,Rest/binary>>, Creator, QuirksTab)
  when NL =:= 10; NL =:= 13 ->
    find_creator_ps(Rest, Creator, QuirksTab);
find_creator_ps(<<CmtChr,Rest/binary>>, Creator, QuirksTab)
  when CmtChr =:= $% ->
    Rest1 = find_creator_ps_after_nl(Rest),
    find_creator_ps(Rest1, Creator, QuirksTab);
find_creator_ps(Rest, Creator, QuirksTab) ->
    %% Went through all the comments and did not see a creator comment
    find_creator_ps_1(Rest, Creator, QuirksTab).

find_creator_ps_1(Rest, Creator, QuirksTab) ->
    case get_split_fun(Creator, QuirksTab) of
        {ok, SplitFun} ->
            {Rest, string:to_upper(Creator), SplitFun}
    end.


find_creator_ps_after_nl(<<"\r\n",Rest/binary>>) ->
    Rest;
find_creator_ps_after_nl(<<NL,Rest/binary>>)
  when NL =:= 10; NL =:= 13 ->
    Rest;
find_creator_ps_after_nl(<<_,Rest/binary>>) ->
    find_creator_ps_after_nl(Rest).


remove_parenthesis(A0) ->
    A1 = lists:append(string:replace(A0, "(", "")),
    lists:append(string:replace(A1, ")", "")).


after_end_setup_ps(Bin, SplitFun) ->
    %% Split the content along "Page: 1 1" or something else depending
    %% on the function used.
    {BeforePageSetup, Rest1} = SplitFun(Bin),
    Rest0 = re:replace(Rest1, " \\.", " 0\\.", [global,{return,list}]),
    Rest = re:replace(Rest0, "\n\\.", "\n0\\.", [global,{return,list}]),
    Commands = Rest,
    {BeforePageSetup, Commands}.


split_function_from_atom(begin_at_page1) ->
    fun split_at_page1/1;
split_function_from_atom(whole_eps) ->
    fun no_split_in_eps/1.

%% Split used by creators that use a standard Page comment
split_at_page1(Cont) ->
    {Idx,_} = binary:match(Cont, <<"%%Page: 1 1">>),
    { binary:part(Cont, {0, Idx}),
      binary:part(Cont, {Idx, byte_size(Cont) - Idx}) }.

%% No split, used by generic EPS
no_split_in_eps(Cont) ->
    {Cont, Cont}.




%% tokenize first list (characters from file) into list of tokens
%% (accumulated reversed in second list, reversed at end).
%% a token is {tnum,Val}, {tname,Val}, {tlitname, Val}, or {tstring}

tokenize([], Toks) ->
    lists:reverse(Toks);
tokenize([C|T], Toks) when C == $\s; C == $\t; C == $\r; C == $\n;
            C == $); C == $> ->    % these 2 are "shouldn't happens"
    tokenize(T, Toks);
tokenize("%" ++ T, Toks) ->
    tokenize(skipline(T), Toks);
tokenize("/" ++ T, Toks) ->
    {Name,TT} = lists:splitwith(fun isnttokbreak/1, T),
    tokenize(TT, [{tlitname,Name}|Toks]);
tokenize("(" ++ T, Toks) ->
    tokenize(skipstring(T), [{tstring}|Toks]);
tokenize("<" ++ T, Toks) ->
    tokenize(skiphexstring(T), [{tstring}|Toks]);
tokenize([C|T], Toks) when C == $[; C == $]; C == ${; C == $} ->
    tokenize(T, [{tname,[C]}|Toks]);
tokenize([C|_] = Arg, Toks) when C >= $0, C =< $9; C==$-; C==$. ->
    {Tok,TT} = parsenum(Arg),
    tokenize(TT, [Tok|Toks]);
tokenize(Arg, Toks) ->
    {Name,TT} = lists:splitwith(fun isnttokbreak/1, Arg),
    tokenize(TT, [{tname,Name}|Toks]).

%% note: this list of chars be exactly those matched explicitly
%% by the non-default cases of tokenize, else get infinite loop
isnttokbreak(C) -> not(lists:member(C, " \t\r\n()<>[]{}/%")).

%% PS numbers are either ints or floats
%% no radix notation for ints, no scientific notation for floats
parsenum([C|Rest]=L) ->
    case re:run(L, "^((\\+|\\-?)([0-9]+\\.[0-9]*)|(\\.[0-9]+))",[{capture,first}]) of
        {match,[{0,Length}]} ->
            Fstr0 = lists:sublist(L, Length),
            Fstr =
            case Fstr0 of
                [$.|_] -> [$0|Fstr0];
                _ -> Fstr0
            end,
            F = list_to_float(Fstr),
            {{tnum,F}, lists:nthtail(Length, L)};
        nomatch ->
            case re:run(L, "^(\\+|-)?[0-9]+", [{capture,first}]) of
                {match, [{0, Length}]} ->
                    Istr = lists:sublist(L, Length),
                    I = list_to_integer(Istr),
                    {{tnum,float(I)}, lists:nthtail(Length, L)};
                nomatch ->
                    {{tname,[C]}, Rest}
            end
    end.

%% skip past next end of line, return rest
skipline("\r\n" ++ T) -> T;
skipline("\r" ++ T) -> T;   % sometimes find files with only CRs!
skipline("\n" ++ T) -> T;
skipline([_|T]) -> skipline(T);
skipline([]) -> [].

%% skip past next ")", but be careful about escaped ones
%% return rest
skipstring([]) -> [];
skipstring("\\") -> [];
skipstring("\\" ++ [_|T]) -> skipstring(T);
skipstring(")" ++ T) -> T;
skipstring([_|T]) -> skipstring(T).

%% skip past next ">", return rest
skiphexstring([]) -> [];
skiphexstring(">" ++ L) -> L;
skiphexstring([_|L]) -> skiphexstring(L).

%% consume tokens, return list of objects.
%% an object is either a path or a compoundpath.
parse_tokens_ps(Toks, CommandMap) ->
    #pstate{objects=Objs,objects_coltex=ColTex,curobjs=[]} = parse_ps(Toks, CommandMap, #pstate{}),
    {Objs, ColTex}.


%% PS Parse
parse_ps([],_CM,#pstate{objects=Objs0,objects_coltex=ColTex0,curobjs=[]}=Pst) -> % done
    Objs = lists:reverse(Objs0),
    ColTex = lists:reverse(ColTex0),
    Pst#pstate{objects=Objs,objects_coltex=ColTex};
parse_ps([],CM,#pstate{objects=Objs0,objects_coltex=ColTex0,curobjs=CObj,curobj_col=Col}=Pst) ->
    ColTex = [#coltex{fcol=Col,tex=none}|ColTex0],
    Objs = [CObj|Objs0],
    parse_ps([],CM,Pst#pstate{objects=Objs,objects_coltex=ColTex,curobjs=[],curobj_col=none,curobj_clip=[]});

parse_ps([{tname,"["},{tnum,X1},{tnum,Y1},{tnum,X2},{tnum,Y2},{tnum,X3},{tnum,Y3},
          {tname,"]"},{tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathopmtx(N,{X1,Y1,X2,Y2,X3,Y3},CM,Pst));
parse_ps([{tname,[_|_]=N} | T ], CM, Pst) ->
    parse_ps(T,CM,ps_dorenderop(N,CM,ps_dopathop0(N,CM,Pst)));
parse_ps([{tnum,X1},{tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathop1(N,{X1},CM,Pst));
parse_ps([{tnum,X1},{tnum,Y1},{tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathop2(N,{X1,Y1},CM,Pst));
parse_ps([{tnum,X1},{tnum,X2},{tnum,X3},{tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathop3(N,{X1,X2,X3},CM,Pst));
parse_ps([{tnum,X1},{tnum,Y1},{tnum,W1},{tnum,H1},{tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathop4(N,{X1,Y1,W1,H1},CM,Pst));
parse_ps([{tnum,X1},{tnum,Y1},{tnum,X2},{tnum,Y2},{tnum,X3},{tnum,Y3},
        {tname,[_|_]=N}|T], CM, Pst) ->
    parse_ps(T,CM,ps_dopathop6(N,{X1,Y1,X2,Y2,X3,Y3},CM,Pst));
parse_ps([_|T], CM, Pst) ->
    parse_ps(T, CM, Pst).

negate_last({X,Y}) ->
    {X,-Y};
negate_last(A) ->
    A.

call_next(CP,CM,Val,Pst,FAtm,F) ->
    case maps:get(CP, CM, false) of
        false ->
            Pst;
        {FAtm, NextFun} when is_function(NextFun) ->
            NextFun(Val, Pst);
        {_, NextFun} when is_function(NextFun) ->
            Pst;
        {neg, NextCall} when is_list(NextCall) ->
            F(NextCall, negate_last(Val));
        NextCall when is_list(NextCall) ->
            F(NextCall, Val)
    end.


%% check if C is a no-arg path operation, and if so, return a modified Pst,
%% otherwise return original Pst
ps_dopathop0(CP,_CM,#pstate{curpath=#path{}=P}=Pst)
  when CP=:="closepath" ->
    Pst#pstate{curpath=P#path{close=true}};
ps_dopathop0(CP,_CM,#pstate{ctm=CTM,ctms=CTMS0}=Pst)
  when CP=:="gsave" ->
    Pst#pstate{ctms=[CTM|CTMS0]};
ps_dopathop0(CP,_CM,#pstate{ctms=[CTM|CTMS0]}=Pst)
  when CP=:="grestore" ->
    Pst#pstate{ctm=CTM,ctms=CTMS0};
%% we use the clip path to get clipping shapes if they aren't bounding boxes.
ps_dopathop0(CP,_CM,#pstate{curpath=#path{}=P,curobjs=CObjs0,curobj_clip=Clip0}=Pst)
  when CP=:="clip"; CP=:="eoclip" ->
    CObjs = add_path(CObjs0, P),
    Pst#pstate{curobjs=[],curpath=#path{},curobj_clip=[CObjs|Clip0]};
ps_dopathop0(CP,CM,Pst) ->
    call_next(CP,CM,none,Pst,f0, fun (NextCall, _) ->
        ps_dopathop0(NextCall,CM,Pst)
    end).

ps_dopathop1(MT,{Val}, _, Pst) when MT=:="setgray" ->
    Pst#pstate{curobj_col={Val,Val,Val}};
ps_dopathop1(MT,{Angle}, _, Pst) when MT=:="rotate" ->
    {M1,M2,M3,M4,M5,M6} = ctm_rotation(Angle),
    finishpop(#pathop{opkind=concat,x1=M1,y1=M2,x2=M3,y2=M4,x3=M5,y3=M6},Pst);
ps_dopathop1(MT,Val, CM, Pst) ->
    call_next(MT,CM,Val,Pst,f1, fun (NextCall, Val1) ->
        ps_dopathop1(NextCall,Val1,CM,Pst)
    end).

ps_dopathop2(MT,{_X,_Y}=Point, _, #pstate{ctm=CTM}=Pst)
  when MT=:="moveto" ->
    {X1,Y1} = ctm_appl(CTM, Point),
    finishpop(#pathop{opkind=pmoveto,x1=X1,y1=Y1},Pst);
ps_dopathop2(LT,{_X,_Y}=Point, _, #pstate{ctm=CTM}=Pst)
  when LT=:="lineto" ->
    {X1,Y1} = ctm_appl(CTM, Point),
    finishpop(#pathop{opkind=plineto,x1=X1,y1=Y1},Pst);
ps_dopathop2(MT,{X1,Y1}, _, Pst)
  when MT=:="scale" ->
    {M1,M2,M3,M4,M5,M6} = ctm_scale(X1, Y1),
    finishpop(#pathop{opkind=concat,x1=M1,y1=M2,x2=M3,y2=M4,x3=M5,y3=M6},Pst);
ps_dopathop2(MT,{X1,Y1}, _, Pst)
  when MT=:="translate" ->
    {M1,M2,M3,M4,M5,M6} = ctm_translate(X1, Y1),
    finishpop(#pathop{opkind=concat,x1=M1,y1=M2,x2=M3,y2=M4,x3=M5,y3=M6},Pst);
ps_dopathop2(MT,{_,Idx0}, _, #pstate{curobjs=[],curobj_clip=Clip0,ctm=CTM0}=Pst)
  when MT=:=?W3DEMBEDIMG ->
    {M1,M2,M3,M4,M5,M6}=CTM0,
    TexIdx = round(Idx0),
    case lists:partition(fun is_path_rectangle/1, Clip0) of
        {[Clip|_], []} -> Clip;
        {_, Clip1} -> Clip = lists:append(Clip1)
    end,
    TexInfo = {{M1,M2,M3,M4,M5,M6}, TexIdx},
    finishrop(obj,Pst#pstate{curpath=#path{},curobjs=Clip,curobj_clip=[]},fill,TexInfo);
ps_dopathop2(MT,Val, CM, Pst) ->
    call_next(MT,CM,Val,Pst,f2, fun (NextCall, Val1) ->
        ps_dopathop2(NextCall,Val1,CM,Pst)
    end).

%% Fill color
ps_dopathop3(RT,{R,G,B},_CM,Pst) when RT=:="setrgbcolor" ->
    Pst#pstate{curobj_col={R,G,B}};
ps_dopathop3(RT,Val,CM,Pst) ->
    call_next(RT,CM,Val,Pst,f3, fun(NextCall, Val1) ->
        ps_dopathop3(NextCall,Val1,CM,Pst)
    end).

%% Commands like 're' for inkscape will be called here
ps_dopathop4(RT,{C,M,Y,K},_CM,Pst) when RT=:="setcmykcolor" ->
    R = (1.0 - C) * (1.0 - K),
    G = (1.0 - M) * (1.0 - K),
    B = (1.0 - Y) * (1.0 - K),
    Pst#pstate{curobj_col={R,G,B}};
ps_dopathop4(RT,Val,CM,Pst) ->
    call_next(RT,CM,Val,Pst,f4, fun(NextCall, Val1) ->
        ps_dopathop4(NextCall,Val1,CM,Pst)
    end).

ps_dopathop6(CT,{IX1,IY1,IX2,IY2,IX3,IY3},_,#pstate{ctm=CTM}=Pst) when CT=:="curveto" ->
    {X1,Y1} = ctm_appl(CTM, {IX1,IY1}),
    {X2,Y2} = ctm_appl(CTM, {IX2,IY2}),
    {X3,Y3} = ctm_appl(CTM, {IX3,IY3}),
    finishpop(#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3},Pst);
ps_dopathop6(CT,Val,CM,Pst) ->
    call_next(CT,CM,Val,Pst,f6, fun(NextCall, Val1) ->
        ps_dopathop6(NextCall,Val1,CM,Pst)
    end).

ps_dopathopmtx(CT,{X1,Y1,X2,Y2,X3,Y3},_,Pst) when CT=:="concat" ->
    finishpop(#pathop{opkind=concat,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X3,y3=Y3},Pst);
ps_dopathopmtx(CT,Val,CM,Pst) ->
    call_next(CT,CM,Val,Pst,fm, fun(NextCall, Val1) ->
        ps_dopathopmtx(NextCall,Val1,CM,Pst)
    end).

ctm_translate(X, Y) ->
    {1.0, 0.0, 0.0, 1.0, X, Y}.
ctm_scale(X, Y) ->
    {X, 0.0, 0.0, Y, 0.0, 0.0}.
ctm_rotation(Ang) ->
    {math:cos(Ang), -math:sin(Ang), math:sin(Ang), math:cos(Ang), 0.0, 0.0}.

ctm_mat() ->
    {1.0,0.0,0.0,1.0,0.0,0.0}.
ctm_concat(L) ->
    lists:foldl(fun (M1, A) -> ctm_mul(M1, A) end, ctm_mat(), L).
ctm_mul({A11,A21,A12,A22,A13,A23}=_MA,{B11,B21,B12,B22,B13,B23}=_MB) ->
    B31=B32=0.0, % _A31=_A32=
    B33=1.0, % _A33=
    {C11,C12,C31,C21,C22,C32} = %,_C13,_C23,_C33} =
    {A11*B11 + A12*B21 + A13*B31 , A11*B12 + A12*B22 + A13*B32 , A11*B13 + A12*B23 + A13*B33 ,
     A21*B11 + A22*B21 + A23*B31 , A21*B12 + A22*B22 + A23*B32 , A21*B13 + A22*B23 + A23*B33 },
    {C11,C21,C12,C22,C31,C32}.

ctm_appl({A11,A21,A12,A22,A13,A23}, {X, Y}) ->
    { A11*X + A12*Y + A13 ,
      A21*X + A22*Y + A23 }.


%% finish job of dopathop[2,6] by putting arg pathop onto curpath's ops list
%% and returning Pst with modified curpath
finishpop(#pathop{opkind=pmoveto}=Pop, #pstate{curpath=#path{ops=[]}}=Pst) ->
    Pst#pstate{curpath=#path{ops=[Pop]}};
finishpop(#pathop{opkind=pmoveto}=Pop, #pstate{curpath=#path{ops=[#pathop{opkind=pmoveto}]}=P}=Pst) ->
    %% note: only one pmoveto is accept by path, so ignore a second one found in Inkscape's files
    Pst#pstate{curpath=P#path{ops=[Pop]}};
finishpop(#pathop{opkind=concat,x1=M1,y1=M2,x2=M3,y2=M4,x3=M5,y3=M6}, #pstate{ctm=CTM0}=Pst) ->
    Pst#pstate{ctm=ctm_concat([CTM0,{M1,M2,M3,M4,M5,M6}])};
finishpop(_, #pstate{curpath=#path{ops=[]}}=Pst) ->
    Pst;    % note: only pmoveto's can start path, so ignore others
finishpop(Pop, #pstate{curpath=#path{ops=Ops}=P}=Pst) ->
    Pst#pstate{curpath=P#path{ops=[Pop|Ops]}}.

ps_dorenderop(CP,_,Pst) when CP=:="closepath" ->
    finishrop(true,Pst);
ps_dorenderop(NP,_,Pst) when NP=:="newpath" ->
    finishrop(false,Pst);
ps_dorenderop(NP,_,Pst) when NP=:="fill"; NP=:="eofill" ->
    finishrop(obj,Pst,fill);
ps_dorenderop(NP,_,Pst) when NP=:="stroke" ->  % Stroke - sing a new object
    finishrop(obj,Pst,stroke);
ps_dorenderop(CP,CM,Pst) ->
    call_next(CP,CM,none,Pst,fr, fun (NextCall, _) ->
        ps_dorenderop(NextCall,CM,Pst)
    end).

%% If Nam is a renderop, finish off curpath and put on objects list.
finishrop(Close, Pst) ->
    finishrop(Close, Pst, none).
finishrop(Close, Pst, DrawOp) ->
    Tex = none,
    finishrop(Close, Pst, DrawOp, Tex).
finishrop(obj,#pstate{objects=Objs0,objects_coltex=ColTex0,curobjs=CObjs,curobj_col=Col}=Pst,DrawOp,Tex)
  when CObjs =/= [] ->
    case DrawOp of
        fill ->
            %% Change the color depending on if there is a texture
            Col1 = case Tex of none -> Col; _ -> {1.0,1.0,1.0} end,
            ColTex = [#coltex{fcol=Col1,tex=Tex}|ColTex0];
        _ ->
            ColTex = [#coltex{scol=Col,tex=Tex}|ColTex0]
    end,
    Objs = [CObjs|Objs0],
    Pst#pstate{objects=Objs,objects_coltex=ColTex,curobjs=[],curobj_col=none,curobj_clip=[]};
finishrop(obj,#pstate{curobjs=CObjs}=Pst,_DrOp,_Tex)
  when CObjs =:= [] ->
    Pst#pstate{curobj_col=none,curobj_clip=[]};
finishrop(Close,#pstate{curpath=#path{close=Pclose,ops=Ops}=P,curobjs=Objs}=Pst,_DrawOp,_Tex)
  when Ops =/= [] ->
    Newp = P#path{close=Close or Pclose,ops=lists:reverse(Ops)},
    Pst#pstate{curobjs=[Newp|Objs],curpath=#path{}};
finishrop(_Close,#pstate{curpath=#path{ops=Ops}=_}=Pst,_DrawOp,_Tex)
  when Ops =:= [] ->
    Pst.


add_path(Objs,#path{ops=[#pathop{opkind=pmoveto}]}=_) ->
    Objs;
add_path(Objs,#path{ops=Ops}=P)
  when Ops =/= [] ->
    Newp = P#path{ops=lists:reverse(Ops)},
    [Newp|Objs];
add_path(Objs,#path{ops=Ops}=_)
  when Ops =:= [] ->
    Objs.



%% Do fixes based on quirks details
%%
do_fixes(#quirksdetails{fixes=Fixes}, Objs) ->
    do_fixes_1(Fixes, Objs).
do_fixes_1([fix_close|FixProps], Objs) ->
    Objs_1 = [[fix_inkscape(Path) || Path <- Paths] || Paths <- Objs],
    do_fixes_1(FixProps, Objs_1);
do_fixes_1([_|FixProps], Objs) ->
    do_fixes_1(FixProps, Objs);
do_fixes_1([], Objs) ->
    Objs.


%%% fixes the inkscape issue that doesn't close the mesh in some situations
%%% in these cases we force a plineto command to the first pmoveto coordinate
fix_inkscape(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops0]=Ops}=Path) ->
    [LastOp|_] = lists:reverse(Ops0),
    case LastOp of
        #pathop{opkind=plineto,x1=X,y1=Y} -> Path;
        #pathop{opkind=plineto} -> Path#path{ops=Ops++[#pathop{opkind=plineto,x1=X,y1=Y}]};
        #pathop{opkind=pcurveto,x3=X,y3=Y} -> Path;
        #pathop{opkind=pcurveto} -> Path#path{ops=Ops++[#pathop{opkind=plineto,x1=X,y1=Y}]};
        _ -> Path
    end.


getcontours(#quirksdetails{direction={XDir,YDir}}, Scale, Ps) ->
    S = {Scale*XDir,Scale*YDir},
    T = {0.0,0.0},
    Ps0 = [lists:map(fun getcedges/1, P) || P <- Ps],
    lists:map(fun(CEs) ->
                lists:foldl(fun(CE, Acc) ->
                                CE0 = [scalece(Cedge, S, T) || Cedge <- CE],
                                case CE0 of
                                    [] -> Acc;
                                    _ -> [CE0|Acc]
                                end
                            end,[],CEs)
            end, Ps0).

getcedges(#path{ops=[#pathop{opkind=pmoveto,x1=X,y1=Y}|Ops]}) ->
    getcedges(Ops,{X,Y},{X,Y},[]);
getcedges(_) -> [].

getcedges([],{X,Y},{X,Y},Acc) -> Acc;
getcedges([],Prev,{X,Y},Acc) -> % prev != first, so close with line
    lists:reverse([#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=plineto,x1=X,y1=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,ve={X,Y}}|Acc]);
getcedges([#pathop{opkind=pcurveto,x1=X1,y1=Y1,x2=X2,y2=Y2,x3=X,y3=Y}|Ops],Prev,First,Acc) ->
    getcedges(Ops,{X,Y},First,[#cedge{vs=Prev,cp1={X1,Y1},cp2={X2,Y2},ve={X,Y}}|Acc]);
getcedges([_|_],_,_,_) ->
    [].    % funny path (probably moveto in middle), so return nothing

scalece(#cedge{vs={Xs,Ys},cp1=nil,cp2=nil,ve={Xe,Ye}},{XS,YS},{XT,YT}) ->
    #cedge{vs={(Xs+XT)*XS,(Ys+YT)*YS},cp1=nil,cp2=nil,ve={(Xe+XT)*XS,(Ye+YT)*YS}};
scalece(#cedge{vs={Xs,Ys},cp1={X1,Y1},cp2={X2,Y2},ve={Xe,Ye}},{XS,YS},{XT,YT}) ->
    #cedge{vs={(Xs+XT)*XS,(Ys+YT)*YS},cp1={(X1+XT)*XS,(Y1+YT)*YS},cp2={(X2+XT)*XS,(Y2+YT)*YS},ve={(Xe+XT)*XS,(Ye+YT)*YS}}.

%%%
%%%

%% Check if a path is a rectangle, used to filter out rectangle clips
is_path_rectangle([#path{ops=[#pathop{opkind=pmoveto,x1=X1A,y1=Y1A},
                             #pathop{opkind=plineto,x1=X1A,y1=Y1B},
                             #pathop{opkind=plineto,x1=X1B,y1=Y1B},
                             #pathop{opkind=plineto,x1=X1B,y1=Y1A}],close=true}]) ->
    true;
is_path_rectangle(_) ->
    false.

merge_duplicates(L, CL) ->
    merge_duplicates(L, CL, []).
merge_duplicates([], _, OL) ->
    lists:unzip(lists:reverse(OL));
merge_duplicates([O1,O2|R1],[C1,C2|R2], OL) when O1 =:= O2 ->
    C3 = merge_duplicates_coltex(C1, C2),
    merge_duplicates(R1, R2, [{O1, C3}|OL]);
merge_duplicates([O1|R1],[C1|R2], OL) ->
    merge_duplicates(R1, R2, [{O1,C1}|OL]).

merge_duplicates_coltex(#coltex{fcol=FCol1,scol=SCol1,tex=Tex1}=C1,#coltex{fcol=FCol2,scol=SCol2,tex=Tex2}=_) ->
    C1#coltex{
        fcol=whichever_not_none(FCol1, FCol2),
        scol=whichever_not_none(SCol1, SCol2),
        tex=whichever_not_none(Tex1, Tex2)
    }.
whichever_not_none(none, A) -> A;
whichever_not_none(A, _) -> A.


%%%
%%%

read_custom_conf(ConfFilename) ->
    case file:read_file(ConfFilename) of
        {ok, Cont} ->
            {Dict1, Cont1} = ps_cfg_cmts(Cont),
            Toks = tokenize(binary_to_list(Cont1), []),
            {Dict2_0, BindPassToks} = ps_cfg_p(Toks),
            CommandMap = ps_pl_p(BindPassToks),
            Dict2 = orddict:append_list(fix, [{merge_cmds, CommandMap}], Dict2_0),
            {ok, orddict:merge(fun(_,_,A) -> A end, Dict1, Dict2)}
    end.
orddict_get_value(K, D, DV) ->
    case orddict:find(K, D) of
        {ok, R} ->
            R;
        error ->
            DV
    end.
to_epq(Custom) ->
    Match = orddict_get_value(match, Custom, "Unknown"),
    Comment = orddict_get_value(comment, Custom, ""),
    [CoordSys|_] = orddict_get_value(coordsys, Custom, [{1.0,1.0}]),
    [BeginMode|_] = orddict_get_value(beginmode, Custom, [begin_at_page1]),
    Fixes = orddict_get_value(fix, Custom, []),
    #epq{
        s={str, Match},
        comment=Comment,
        id=custom,
        coordsys=CoordSys,
        fixes=Fixes,
        split=BeginMode
    }.

%% TODO: Custom bind map files can also be added in in plugin preferences > "Adobe PostScript Paths"


ps_cfg_cmts(Bin) ->
    ps_cfg_cmts(Bin, []).
ps_cfg_cmts(<<"\r\n", R/binary>>, OL) ->
    ps_cfg_cmts(R, OL);
ps_cfg_cmts(<<NL, R/binary>>, OL)
  when NL =:= 10; NL =:= 13 ->
    ps_cfg_cmts(R, OL);
ps_cfg_cmts(<<"%%Match:", R/binary>>, OL) ->
    {A0, R_1} = ps_cfg_cmts_to_nl(R),
    case ps_cfg_cmts_in_parenthesis(A0) of
        {ok, A} ->
            ps_cfg_cmts(R_1, [{match, A} | OL])
    end;
ps_cfg_cmts(<<"%%Comment:", R/binary>>, OL) ->
    {A0, R_1} = ps_cfg_cmts_to_nl(R),
    case ps_cfg_cmts_in_parenthesis(A0) of
        {ok, A} ->
            ps_cfg_cmts(R_1, [{comment, A} | OL])
    end;
ps_cfg_cmts(<<"%", R/binary>>, OL) ->
    {_, R_1} = ps_cfg_cmts_to_nl(R),
    ps_cfg_cmts(R_1, OL);
ps_cfg_cmts(Bin, OL) ->
    {orddict:from_list(OL), Bin}.


ps_cfg_cmts_to_nl(L) ->
    ps_cfg_cmts_to_nl(L, []).
ps_cfg_cmts_to_nl(<<"\r\n", R/binary>>, OL) ->
    {lists:reverse(OL), R};
ps_cfg_cmts_to_nl(<<NL, R/binary>>, OL)
  when NL =:= 10; NL =:= 13 ->
    {lists:reverse(OL), R};
ps_cfg_cmts_to_nl(<<C, R/binary>>, OL) ->
    ps_cfg_cmts_to_nl(R, [C|OL]).


ps_cfg_cmts_in_parenthesis([SC | R])
  when SC =:= 32; SC =:= 9 ->
    ps_cfg_cmts_in_parenthesis(R);
ps_cfg_cmts_in_parenthesis([$( | R]) ->
    ps_cfg_cmts_in_parenthesis_1(R, []);
ps_cfg_cmts_in_parenthesis(_) ->
    {error, not_parenthesis}.
ps_cfg_cmts_in_parenthesis_1([$) | _],OL) ->
    {ok, lists:reverse(OL)};
ps_cfg_cmts_in_parenthesis_1("\\" ++ [C | R],OL) ->
    ps_cfg_cmts_in_parenthesis_1(R, [C|OL]);
ps_cfg_cmts_in_parenthesis_1([C | R],OL) ->
    ps_cfg_cmts_in_parenthesis_1(R, [C|OL]);
ps_cfg_cmts_in_parenthesis_1([],_) ->
    {error, parenthesis_incomplete}.



-record(pscfgstate, {
    toks = [],
    in_curly = 0,
    bindpass = []
}).

ps_cfg_p(L) ->
    ps_cfg_p(L, #pscfgstate{}, orddict:new()).

ps_cfg_p([{tnum, _}=A|R], #pscfgstate{toks=TL}=PS, OL) ->
    ps_cfg_p(R, PS#pscfgstate{toks=[A|TL]}, OL);
ps_cfg_p([{tlitname, _}=A|R], #pscfgstate{toks=TL}=PS, OL) ->
    ps_cfg_p(R, PS#pscfgstate{toks=[A|TL]}, OL);
ps_cfg_p([{tname, "{"}=A|R], #pscfgstate{toks=TL,in_curly=Crl}=PS, OL) ->
    ps_cfg_p(R, PS#pscfgstate{toks=[A|TL],in_curly=Crl+1}, OL);
ps_cfg_p([{tname, "}"}=A|R], #pscfgstate{toks=TL,in_curly=Crl}=PS, OL) ->
    ps_cfg_p(R, PS#pscfgstate{toks=[A|TL],in_curly=max(0,Crl-1)}, OL);
ps_cfg_p([{tname, BindStr}=C1,{tname, "def"}=C2|R], #pscfgstate{bindpass=BP0,toks=TL,in_curly=0}=PS, OL)
  when BindStr =:= "bind"; BindStr =:= "load" ->
    TL1 = lists:reverse([C2,C1|TL]),
    ps_cfg_p(R, PS#pscfgstate{bindpass=[TL1|BP0],toks=[]}, OL);
ps_cfg_p([{tname, Str}|R], #pscfgstate{toks=TL,in_curly=0}=PS, OL) ->
    case ps_cfg_c(Str, lists:reverse(TL)) of
        none ->
            OL1 = OL;
        {A1,A2} ->
            OL1 = orddict:append_list(A1, [A2], OL)
    end,
    ps_cfg_p(R, PS#pscfgstate{toks=[]}, OL1);
ps_cfg_p([{tname, _}=A|R], #pscfgstate{toks=TL,in_curly=Crl}=PS, OL)
  when Crl > 0 ->
    ps_cfg_p(R, PS#pscfgstate{toks=[A|TL]}, OL);
ps_cfg_p([], #pscfgstate{bindpass=BindPass}=_, OL) ->
    {OL, lists:append(lists:reverse(BindPass))}.


ps_cfg_c("fix", [{tlitname, Arg1}]) ->
    {fix, list_to_atom(Arg1)};
ps_cfg_c("fix", [{tlitname, Arg1} | [_|_]=List]) ->
    List1 = [ps_cfg_arg(A) || A <- List],
    {fix, list_to_tuple([list_to_atom(Arg1) | List1])};
ps_cfg_c("coordsys", [{tnum, X}, {tnum, Y}]) ->
    {coordsys, {X, Y}};
ps_cfg_c("beginmode", [{tlitname, Str}]) ->
    {beginmode, list_to_atom(Str)};
ps_cfg_c(_, _) ->
    none.

ps_cfg_arg({tlitname, Str}) ->
    Str;
ps_cfg_arg({tnum, Num}) ->
    Num.


%%%
%%%


%% Calculate a scale to fit the max width and height
%%
calculate_rescale_amount(MaxWidth, MaxHeight, Objs) ->
    calculate_rescale_amount(MaxWidth, MaxHeight, Objs, 1.0).
calculate_rescale_amount(_, _, [], Rescale) -> Rescale;
calculate_rescale_amount(MaxWidth, MaxHeight, [List|Objs], Rescale) ->
    Rescale_2 = lists:foldl(fun (SubPath, Rescale_1) ->
        fit(MaxWidth, MaxHeight, SubPath, Rescale_1)
    end, Rescale, List),
    calculate_rescale_amount(MaxWidth, MaxHeight, Objs, Rescale_2).

fit(MaxWidth, MaxHeight, #path{ops=Ops}, LowestRescale) ->
    WidthFound_0 = 1.0,
    HeightFound_0 = 1.0,
    {WidthFound, HeightFound} = fit_size(Ops, WidthFound_0, HeightFound_0),
    Rescale_0 = MaxWidth / max(WidthFound, 1.0),
    Rescale = case MaxHeight < (HeightFound*Rescale_0) of
        true -> MaxHeight / max(HeightFound, 1.0);
        _    -> Rescale_0
    end,
    min(Rescale, LowestRescale).

fit_size([], WidthFound, HeightFound) ->
    {WidthFound, HeightFound};
fit_size([#pathop{opkind=Kind,x1=X,y1=Y}|Ops0], WidthFound_0, HeightFound_0)
  when Kind =:= pmoveto; Kind =:= plineto; Kind =:= pcurveto ->
    WidthFound = max(X, WidthFound_0),
    HeightFound = max(Y, HeightFound_0),
    fit_size(Ops0, WidthFound, HeightFound);
fit_size([_|Ops0], WidthFound, HeightFound) ->
    fit_size(Ops0, WidthFound, HeightFound).



%%%
%%% Unit conversion
%%%

unit_atom(Unit) when is_atom(Unit) -> Unit;
unit_atom(Unit) when is_list(Unit) ->
    case string:to_lower(Unit) of
        "pt" -> pt;
        "pc" -> pc;
        "mm" -> mm;
        "cm" -> cm;
        "in" -> in;
        "em" -> em;
        "ex" -> ex;

        _ -> pt  %% The default for user unit is points.
    end.

unit_scaled_pt(pt) -> 1.0;
unit_scaled_pt(pc) -> (1.0 / 6.0) * unit_scaled_pt(in);
unit_scaled_pt(mm) -> 0.03937008 * unit_scaled_pt(in);
unit_scaled_pt(cm) -> 10.0   * unit_scaled_pt(mm);
unit_scaled_pt(em) -> 12.0;
unit_scaled_pt(ex) -> 8.0;
unit_scaled_pt(in) -> 72.0.

%% 1.0 if it is the same unit.
unit_ratio(Unit1, Unit2)
  when Unit1 =:= Unit2 ->
    1.0;

%% Physical units on both side
unit_ratio(Unit1, pt) ->
    unit_scaled_pt(Unit1).

conv_unit({Num, Unit}, DocUnit)
  when is_float(Num), is_atom(Unit), is_atom(DocUnit) ->
    Num * unit_ratio(Unit, DocUnit).

number_val_unit(Val, "") ->
    Val;
number_val_unit(Val, Unit) ->
    {Val, Unit}.
parse_float_number_w_unit(Num_S, DVal) ->
    case parse_float_number_w_unit_1(Num_S) of
        NotNum when is_list(NotNum) -> {DVal, pt};
        {Num_I, Unit} when is_integer(Num_I) -> {Num_I * 1.0, Unit};
        {Num_F, Unit} when is_float(Num_F) -> {Num_F, Unit};
        {Num_I, ""} when is_integer(Num_I) -> {Num_I * 1.0, pt};
        {Num_F, ""} when is_float(Num_F) -> {Num_F, pt};
        Num_I when is_integer(Num_I) -> {Num_I * 1.0, pt};
        Num_F when is_float(Num_F) -> {Num_F, pt};
        _ -> {DVal, pt}
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
%%%

-type maybe_image_data() :: {jpeg, binary()} | unused.

-spec get_bitmap(maybe_image_data()) -> {ok, #e3d_image{}} | {error, any()}.

get_bitmap({jpeg, BinData}) ->
    binary_to_tempfile(".jpg", BinData, fun read_jpeg/1).

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
uniq_name(TmpDir, Ext) ->
    FileIdNum = abs(erlang:unique_integer()),
    FileId = "w3d_ps_" ++ integer_to_list(FileIdNum) ++ Ext,
    TempFile = filename:join(TmpDir, FileId),
    case file:read_file_info(TempFile) of
        {ok, _} ->
            uniq_name(TmpDir, Ext);
        _ ->
            TempFile
    end.
binary_to_tempfile(Ext, Bin, F) ->
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




%%%
%%% Read embedded images from PS/EPS
%%%

-type emb_image_matrix() :: {float(), float(), float(), float(), float(), float()}.

-type emb_image_declist() :: [integer()].

-type emb_dec_header_item() :: 
    {atom(), integer() | float() | string() | boolean() | emb_image_matrix() | emb_image_declist()}.

-record(emb_image, {
    encoding :: hex | asc85,
    filetype :: dctdecode | lzwdecode | raw,
    decheader :: [emb_dec_header_item()],
    psenc :: binary()
}).

%% Extract the embedded images from the rest of the postscript, and
%% return them as a list, as well as return the postscript with the images
%% replaced with an importer internal command.
%%
-spec get_emb_images(binary()) -> {[maybe_image_data()], binary()}.
get_emb_images(Cont) ->
    {ImgBins, EPS} = partition_emb_imgs(Cont),
    {[get_image(ImgB) || ImgB <- ImgBins], EPS}.
get_image(#emb_image{encoding=Enc,filetype=FileType,decheader=Hdr,psenc=PSEncBin}) ->
    %% Seek ahead a bit after the image command to get the
    %% start of the encoding.
    PSEncBin_1 = seek_img_start(PSEncBin),
    FileCont = img_to_bin(Enc, PSEncBin_1),
    get_image_1(FileType, Hdr, FileCont).
get_image_1(dctdecode, _Hdr, Cont) ->
    case unscramble_jpg(Cont) of
        unsure ->
            unused;
        Cont1 when is_binary(Cont1) ->
            {jpeg, Cont1}
    end;
get_image_1(_, _Hdr, _Cont) ->
    unused.


%% Some software store the jpeg file verbatim but seem to scramble the header
%% part of it.
%%
-spec unscramble_jpg(binary()) -> binary() | unsure.
unscramble_jpg(<<16#FF,16#D8,16#FF,_/binary>>=Cont) ->
    %% Probably not scrambled.
    Cont;
unscramble_jpg(Content) when byte_size(Content) > 300 ->
    case unscramble_jpg_find(Content, 1) of
        {found, FPartAt} ->
            FirstPart = binary:part(Content, 0, FPartAt),
            Rest = binary:part(Content, FPartAt, byte_size(Content)-FPartAt),
            FirstPart_1 = unscramble_jpg_reverse(FirstPart, []),
            <<FirstPart_1/binary, Rest/binary>>;
        false ->
            unsure
    end.
unscramble_jpg_find(Content, I) when I =< 75 ->
    case binary:part(Content, I * 4, 3) =:= <<16#FF,16#D8,16#FF>> of
        true ->
            {found, (I * 4) + 4};
        false ->
            unscramble_jpg_find(Content, I+1)
    end;
unscramble_jpg_find(_, I) when I > 75 ->
    false.

unscramble_jpg_reverse(<<B:4/binary-unit:8,Cont/binary>>, OL) ->
    unscramble_jpg_reverse(Cont, [B|OL]);
unscramble_jpg_reverse(<<>>, OL) ->
    iolist_to_binary(OL).


%% Look for embedded images and partition them out of the EPS source.
-spec partition_emb_imgs(binary()) -> {[#emb_image{}], binary()}.
partition_emb_imgs(Cont) ->
    partition_emb_imgs(Cont, [], 0, byte_size(Cont)).
partition_emb_imgs(Cont, OL, A, ALn) ->
    case binary:match(Cont, <<"<<">>, [{scope, {A, ALn-A}}]) of
        nomatch ->
            %% There are no image properties so there are probably no embedded
            %% images
            {OL, Cont};
        {S1, _} ->
            %% Look for "/ImageType"
            case is_image_properties_dictionary(Cont, A, S1) of
                {true, S2} ->
                    partition_emb_imgs_1(Cont, OL, A, ALn, {S1, S2});
                false ->
                    {OL, Cont}
            end
    end.
partition_emb_imgs_1(Cont, OL, A, ALn, {_, _}=Pair) ->
    %% We found an image property, we'll need to find out the encoding
    %% to find out the closing delimiter. The closing delimiter depends
    %% on the encoding.
    {_, Enc, FileType, Hdr} = find_file_type(Cont, A, Pair),
    case partition_emb_imgs_enc(Enc, Cont, Pair, ALn, length(OL)) of
        nomatch ->
            {OL, Cont};
        {Enc_1, BinAtt, Cont_3} ->
            Img = #emb_image{encoding=Enc_1,filetype=FileType,decheader=Hdr,psenc=BinAtt},
            partition_emb_imgs(Cont_3, [Img|OL], A, byte_size(Cont_3))
    end.

%% Find out if there is an ImageType
is_image_properties_dictionary(Cont, A, S1) ->
    ALn = byte_size(Cont),
    case binary:match(Cont, <<">>">>, [{scope, {S1, ALn-S1}}]) of
        nomatch ->
            false;
        {S2, _} ->
            case binary:match(Cont, <<"/ImageType">>, [{scope, {A, S2-A}}]) of
                nomatch ->
                    false;
                {_, _} ->
                    {true, S2}
            end
    end.


%% Try to find the encoding for the embedded image, it is needed
%% as the delimiter is different for ASCIIHexDecode and ASCII85Decode.
%% Also get the file type (jpeg, lzw encoded image, etc.), and 
%% return the other image information that will be needed to decode it.
%%
find_file_type(Cont, A, {S1, S2}) ->
    %% Look first inside the << >> fields and try anywhere
    %% before that if nothing is found
    case find_file_enc_1(Cont, S1, S2) of
        unknown ->
            Enc = find_file_enc_1(Cont, A, S1);
        FoundEnc ->
            Enc = FoundEnc
    end,
    %% Embedded image header
    Hdr = parse_image_header(binary:part(Cont, S1+2, S2-S1-2)),
    %% File type (if dctdecode it could be a usable jpeg).
    case find_file_type_1(Cont, S1, S2) of
        raw ->
            Type = find_file_type_1(Cont, A, S1);
        FoundType ->
            Type = FoundType
    end,
    {S2, Enc, Type, Hdr}.


find_file_enc_1(Cont, A, S2) ->
    case binary:match(Cont, <<"ASCII85Decode">>, [{scope, {A, S2-A}}]) of
        nomatch ->
            find_file_enc_2(Cont, A, S2);
        {_, _} ->
            asc85
    end.
find_file_enc_2(Cont, A, S2) ->
    case binary:match(Cont, <<"ASCIIHexDecode">>, [{scope, {A, S2-A}}]) of
        nomatch ->
            unknown;
        {_, _} ->
            hex
    end.


find_file_type_1(Cont, A, S2) ->
    case binary:match(Cont, <<"DCTDecode">>, [{scope, {A, S2-A}}]) of
        nomatch ->
            find_file_type_2(Cont, A, S2);
        {_, _} ->
            dctdecode
    end.
find_file_type_2(Cont, A, S2) ->
    case binary:match(Cont, <<"LZWDecode">>, [{scope, {A, S2-A}}]) of
        nomatch ->
            raw;
        {_, _} ->
            lzwdecode
    end.


partition_emb_imgs_enc(asc85, Cont, Pair, ALn, NextImg) ->
    partition_emb_imgs_85(Cont, Pair, ALn, NextImg);
partition_emb_imgs_enc(hex, Cont, Pair, ALn, NextImg) ->
    partition_emb_imgs_hex(Cont, Pair, ALn, NextImg);
partition_emb_imgs_enc(unknown, Cont, Pair, ALn, NextImg) ->
    %% If we're not sure try ascii85
    partition_emb_imgs_85(Cont, Pair, ALn, NextImg).

%% Split out ASCII85 image data and put a pseudo command in its place
%%
partition_emb_imgs_85(Cont, {S1, S1_B_0}, ALn, NextImg) ->
    case binary:match(Cont, <<"~>">>, [{scope, {S1_B_0, ALn-S1_B_0}}]) of
        nomatch ->
            nomatch;
        {S2_0, _} ->
            S2 = S2_0 + 2,
            Cont_1 = binary:part(Cont, 0, S1),
            Cont_2 = binary:part(Cont, S2, ALn-S2),
            BinAtt = binary:part(Cont, S1_B_0+2, S2-S1_B_0-2),
            IR_T = io_lib:format("\n ~w ~w ~s \n", [0, NextImg, ?W3DEMBEDIMG]),
            IR = iolist_to_binary(IR_T),
            Cont_3 = <<Cont_1/binary, IR/binary, Cont_2/binary>>,
            {asc85, BinAtt, Cont_3}
    end.

%% Split out hex image data and put a pseudo command in its place
%%
partition_emb_imgs_hex(Cont, {S1, S1_B_0}, ALn, NextImg) ->
    %% First skip over image properties closing tag.
    S1_B = S1_B_0 + 2,
    case binary:match(Cont, <<">">>, [{scope, {S1_B, ALn-S1_B}}]) of
        nomatch ->
            nomatch;
        {S2_0, _} ->
            S2 = S2_0 + 1,
            Cont_1 = binary:part(Cont, 0, S1),
            Cont_2 = binary:part(Cont, S2, ALn-S2),
            BinAtt = binary:part(Cont, S1_B_0+2, S2-S1_B_0-2),
            IR_T = io_lib:format("\n ~w ~w ~s \n", [0, NextImg, ?W3DEMBEDIMG]),
            IR = iolist_to_binary(IR_T),
            Cont_3 = <<Cont_1/binary, IR/binary, Cont_2/binary>>,
            {hex, BinAtt, Cont_3}
    end.

%% Seek to the beginning of the encoded bytes, which begins after a newline
%% after a word (sometimes it is image, but can be a macro word), which we need
%% to find first after the close of the image properties block.
-spec seek_img_start(binary()) -> binary().
seek_img_start(Cont) ->
    ALn = byte_size(Cont),
    {S2_0, S2_L} = binary:match(Cont, [<<"\r\n">>,<<"\n">>,<<"\r">>], [{scope, {0, ALn}}]),
    S2 = S2_0 + S2_L,
    seek_img_start_1(Cont, S2, ALn).
seek_img_start_1(Cont, S2, ALn) ->
    S2AB = seek_img_start_mv_after_blank(Cont, S2),
    {S3_0, S3_L} = binary:match(Cont, [<<"\r\n">>,<<"\n">>,<<"\r">>], [{scope, {S2AB, ALn-S2AB}}]),
    S3 = S3_0 + S3_L,
    if S2AB =:= S3_0 ->
            seek_img_start_1(Cont, S3, ALn);
        true ->
            binary:part(Cont, S3, ALn-S3)
    end.
seek_img_start_mv_after_blank(Cont, S2) ->
    case binary:at(Cont, S2) of
        C when C =:= 32; C =:= 9 ->
            seek_img_start_mv_after_blank(Cont, S2+1);
        _ ->
            S2
    end.



img_to_bin(hex, Cont) ->
    %% Remove delimiter
    Cont1 = binary:part(Cont, 0, byte_size(Cont)-1),
    dchex(Cont1);
img_to_bin(asc85, Cont) ->
    %% Remove delimiter
    Cont1 = binary:part(Cont, 0, byte_size(Cont)-2),
    dc85(Cont1).

-spec dc85(binary()) -> binary().
dc85(Cont) ->
    A85Chars = dc85_1(Cont, []),
    dc85_2(A85Chars, []).
dc85_1(<<C,Cont/binary>>, OL) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    dc85_1(Cont, OL);
dc85_1(<<C,Cont/binary>>, OL) ->
    dc85_1(Cont, [C|OL]);
dc85_1(<<>>, OL) ->
    lists:reverse(OL).
dc85_2([$z|Cont], OL) ->
    dc85_2(Cont, [<<0,0,0,0>>,OL]);
dc85_2([C1,C2,C3,C4,C5|Cont], OL) ->
    C1_B = C1 - 33,
    C2_B = C2 - 33,
    C3_B = C3 - 33,
    C4_B = C4 - 33,
    C5_B = C5 - 33,
    Val =
        C1_B*85*85*85*85 +
        C2_B*85*85*85 +
        C3_B*85*85 +
        C4_B*85 +
        C5_B,
    Bytes = <<Val:32/big-unsigned-integer>>,
    dc85_2(Cont, [Bytes|OL]);
dc85_2([C1,C2,C3,C4], OL) ->
    dc85_2([C1,C2,C3,C4,0], [0|OL]);
dc85_2([C1,C2,C3], OL) ->
    dc85_2([C1,C2,C3,0,0], [0|OL]);
dc85_2([C1,C2], OL) ->
    dc85_2([C1,C2,0,0,0], [0|OL]);
dc85_2([C1], OL) ->
    dc85_2([C1,0,0,0,0], [0|OL]);
dc85_2([], OL) ->
    iolist_to_binary(lists:reverse(OL)).

-spec dchex(binary()) -> binary().

-define(HEXDIGIT(H1), (
       (H1 >= $A andalso H1 =< $F) orelse 
       (H1 >= $a andalso H1 =< $f) orelse
       (H1 >= $0 andalso H1 =< $9))).

dchex(Cont) ->
    dchex(Cont, []).
dchex(<<C,Cont/binary>>, OL) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    dchex(Cont, OL);
dchex(<<H1,H2,Cont/binary>>, OL)
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2) ->
      N = (dchex_1(H1) bsl 4) bor dchex_1(H2),
    dchex(Cont, [N|OL]);
dchex(<<>>, OL) ->
    iolist_to_binary(lists:reverse(OL)).

dchex_1(A) when A >= $A, A =< $F ->
    A - $A + 10;
dchex_1(A) when A >= $a, A =< $f ->
    A - $a + 10;
dchex_1(A) when A >= $0, A =< $9 ->
    A - $0.

-spec parse_image_header(binary()) -> [emb_dec_header_item()].
parse_image_header(Cont) ->
    Cont_1 = tokenize(binary_to_list(Cont), []),
    parse_image_header(Cont_1, []).
parse_image_header([{tlitname,"ImageType"},{tnum,A1}|R],OL) ->
    parse_image_header(R, [{imagetype, round(A1)}|OL]);
parse_image_header([{tlitname,"Width"},{tnum,A1}|R],OL) ->
    parse_image_header(R, [{width, round(A1)}|OL]);
parse_image_header([{tlitname,"Height"},{tnum,A1}|R],OL) ->
    parse_image_header(R, [{height, round(A1)}|OL]);
parse_image_header([{tlitname,"BitsPerComponent"},{tnum,A1}|R],OL) ->
    parse_image_header(R, [{bitspercomponent, round(A1)}|OL]);
parse_image_header([{tlitname,"Interpolate"},{tname,A0}|R],OL) ->
    case A0 of
        "true" -> A1 = true;
        _ ->      A1 = false
    end,
    parse_image_header(R, [{interpolate, A1}|OL]);
parse_image_header([{tlitname,"Decode"},{tname,"["},{tnum,A1},{tnum,A2},
                    {tnum,A3},{tnum,A4},{tnum,A5},{tnum,A6},{tname,"]"}|R],OL) ->
    parse_image_header(R, [{decode, [round(A1),round(A2),round(A3),
                                     round(A4),round(A5),round(A6)]}|OL]);
parse_image_header([{tlitname,"ImageMatrix"},{tname,"["},{tnum,A1},{tnum,A2},
                    {tnum,A3},{tnum,A4},{tnum,A5},{tnum,A6},{tname,"]"}|R],OL) ->
    parse_image_header(R, [{imagematrix, {float(A1),float(A2),float(A3),
                                          float(A4),float(A5),float(A6)}}|OL]);
parse_image_header([_|R],OL) ->
    parse_image_header(R, OL);
parse_image_header([], OL) ->
    lists:reverse(OL).

