%%
%%  wpc_amf.erl --
%%
%%     Additive Manufacturing File (.amf) import/export.
%%
%%  Copyright (c) 2023-2024 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_amf).

-export([init/0,menu/2,command/2]).

-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").
-include_lib("stdlib/include/zip.hrl").

init() ->
    wpa:pref_set_default(?MODULE, swap_y_z, true),
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{amf_model,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{amf_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{amf_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.


menu_entry(Menu) ->
    Menu ++ [{?__(1,"Additive Manufacturing (.amf)..."),amf_model,[option]}].

props() ->
    [{ext,".amf"},{ext_desc,?__(1,"Additive Manufacturing File")}].

units() ->
    units(true).
units(W) ->
    [{?__(1, "Meters"),meter}] ++
    if W ->
        [{?__(2, "Decimeters"),dm},
         {?__(3, "Centimeters"),cm}];
       true -> []
    end ++
    [{?__(4, "Millimeters"),mm},
     {?__(5, "Microns"),micron}] ++
    if W ->
         [{?__(6, "Yards"),yd}];
        true -> []
    end ++
    [{?__(7, "Feet"),ft},
     {?__(8, "Inches"),in}].


%%%
%%% Import.
%%%

do_import(Ask, _St) when is_atom(Ask) ->
    ModelUnit = wpa:pref_get(?MODULE, model_units, cm),
    Dialog = [
        {hframe,[
            {label,?__(2,"Model units (for Wings3D Unit):")},
            {menu, units(), ModelUnit, [{key,model_units}]} ]}
    ] ++ common_mesh_options(import),
    wpa:dialog(Ask, ?__(1,"Additive Manufacturing Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{amf_model,Res}}}
           end);
do_import(Attr, St) ->
    wpa:import(props(), import_fun(Attr), St).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

import_transform(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

import_fun(Attr) ->
    fun(Filename) ->
        set_pref(Attr),
        ModelUnits = proplists:get_value(model_units, Attr, cm),
        case open_amf_file(ModelUnits, Filename) of
            {ok, {Objs, Mats}} ->
                {ok, import_transform(#e3d_file{objs=Objs,mat=Mats}, Attr)};
            {error, Err} ->
                {error, Err}
        end
    end.

%%%
%%% Export.
%%%

more_info() ->
    [?__(1,"<h3>Preparation</h3>"
     "If an object has multiple materials the object should be cut apart "
     "by material (use <i>Loop Cut</i> tool), each material volume in "
     "AMF is a closed mesh object. "
     "If an object has faces with different materials an error will be "
     "shown asking for the distinct materials be separated into separate "
     "objects.\n\n"
     "To indicate Wings3D objects "
     "as material volumes of one AMF object, use a colon <b>:</b> character "
     "in its name:\n\n"
     "<i>CommonObjectName</i><b>:</b><i>VolumeName</i> \n\n"
     "Example:\n\n"
     "You have two wings3d objects that together is an object called "
     "<b>Tire</b>, if the two objects are named as:\n\n"
     "<b>Tire:Rubber</b>\n"
     "<b>Tire:Metal</b>\n\n"
     "The AMF exporter will treat the two objects as volumes of the "
     "same object <b>Tire</b>.\n\n"
     "This object naming convention is completely optional and "
     "if Wings3D objects are not named with a common name, they will be "
     "exported as completely distinct objects, the 3D printing software "
     "that imports this file may detect automatically that the objects "
     "are parts of the same object and ask if the object parts should "
     "be combined, usually this is 'Yes'.\n\n"
     "<h3>Units</h3>"
     "The <i>Model units</i> specifies what the Wings3d unit (WU) is "
     "equivalent to. The <i>Convert in AMF</i> specifies what unit "
     "the exported AMF file should be in.\n\n"
     "The reason is you may have an object modeled with the units in "
     "centimeters, but you want to export the file in millimeters, "
     "possibly because a specific unit is required in the AMF delivered. "
     "In this case the object will be rescaled automatically to fit "
     "millimeters (x10). AMF allows 5 units (meter, mm, micron, ft, in), "
     "when <i>same as above</i> is used, the nearest supported unit is "
     "chosen.\n\n"
     "<h3>Compression</h3>"
     "Note that many software don't support compressed AMF.")].


info_button() ->
    Title = ?__(1,"AMF Export Information"),
    {help, Title, fun () -> more_info() end}.

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    ModelUnit  = wpa:pref_get(?MODULE, model_units, cm),
    AMFUnit    = wpa:pref_get(?MODULE, amf_units, mm),
    Compressed = false, % Default to false

    Dialog = [
        {label_column, [
            {?__(3,"Model units (for Wings3D Unit):"),
                {menu, units(), ModelUnit, [{key,model_units}]} },
            {?__(4,"Convert in AMF to:"),
                {menu,
                    [{?__(5, "Same as above"),same}] ++ units(false),
                    AMFUnit, [{key,amf_units}]} }
        ]}
    ] ++ common_mesh_options(export) ++ [
        {?__(6, "Compressed"),Compressed,[{key,compressed}]},
        {hframe,[info_button()]}
    ],
    
    wpa:dialog(Ask, ?__(1,"Additive Manufacturing (.amf) Export Options"), Dialog,
           fun(Res) ->
               {file,{Op,{amf_model,Res}}}
           end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Uvs = proplists:get_bool(include_uvs, Attr),
    
    Ps = [{include_uvs,Uvs},
          {tesselation, triangulate},
          {include_hard_edges, true},
      {subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)).

export_transform(E3dFile, KeyVals) ->
    Mat = wpa:export_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

export_fun(Attr) ->
    fun(Filename, Contents_0) ->
        set_pref(Attr),
        E3DFile=export_transform(Contents_0, Attr),
        
        ModelUnits = proplists:get_value(model_units, Attr, cm),
        AMFUnits = proplists:get_value(amf_units, Attr, mm),
        Compressed = proplists:get_value(compressed, Attr, false),
        
        wr_amf(Filename, units_tuple(ModelUnits, AMFUnits), Compressed, E3DFile)
    end.

units_tuple(dm, same) ->
    units_tuple(dm, mm);
units_tuple(cm, same) ->
    units_tuple(cm, mm);
units_tuple(yd, same) ->
    units_tuple(yd, ft);
units_tuple(ModelUnits, same)
  when ModelUnits =:= meter;
       ModelUnits =:= mm;
       ModelUnits =:= micron;
       ModelUnits =:= ft;
       ModelUnits =:= in ->
    units_tuple(ModelUnits, ModelUnits);
units_tuple(ModelUnits, AMFUnits) ->
    {ModelUnits, AMFUnits}.


common_mesh_options(Type) ->
    T = wpa:dialog_template(?MODULE, Type, [
        include_normals,
        include_colors,
        default_filetype
    ]),
    T_1 = common_mesh_options_remove_filetype(T),
    [T_1].
common_mesh_options_remove_filetype({vframe, L}) ->
    L_1 = common_mesh_options_remove_filetype_2(lists:reverse(L)),
    {vframe, lists:reverse(L_1)}.
common_mesh_options_remove_filetype_2([panel|L]) ->
    common_mesh_options_remove_filetype_2(L);
common_mesh_options_remove_filetype_2([{vframe, [{menu,_,_,PL}],_}|L]=L1) ->
    case proplists:get_value(key, PL) of
        default_filetype ->
            common_mesh_options_remove_filetype_2(L);
        _ ->
            L1
    end;
common_mesh_options_remove_filetype_2(L) ->
    L.


%%
%% Write Additive Manufacturing File
%%

-record(amtexmap, {
    type,
    w,
    h,
    d,
    b64
}).


wr_amf(Filename, Units, Compressed, E3DFile) ->
    Name = filename:basename(filename:rootname(Filename)),
    Ret = try
        wr_amf_1(Name, Units, E3DFile)
    catch _:one_material_per_volume ->
        {error, {one_material_per_volume, any}}
    end,
    case Ret of
        _ when is_list(Ret), Compressed =:= true ->
            AMFContents = iolist_to_binary(Ret),
            ok = wr_zip(Filename, AMFContents),
            ok;
        _ when is_list(Ret) ->
            AMFContents = iolist_to_binary(Ret),
            ok = file:write_file(Filename, AMFContents),
            ok;
        {error, {one_material_per_volume, A}} ->
            {error,
                ?__(1, "Only one material per volume allowed, use the "
                       "\"loop cut\" tool to split objects and apply "
                       "one material to each whole object afterwards.") ++
                case A of
                    any -> 
                        "";
                    "" -> 
                        "";
                    ObjName when is_list(ObjName) ->
                        lists:flatten(io_lib:format(
                            ?__(2, "~n~n'~s' must be single material."),
                            [ObjName]))
                end }
    end.

wr_amf_1(Name, {_WU, AMFUnit}=Units, #e3d_file{objs=Objs_0,mat=Mats,creator=Creator}) ->
    case close_objs_single_mat(Objs_0) of
        {ok, Objs_1} ->
            %% Combine objects with same name
            ScaleF = scale_from_units(export, Units),
            Objs_2 = scale_objects(Objs_1, ScaleF),
            Objs_3 = combine_objs_v(Objs_2),
            Objs = number_objs(Objs_3),
            {Mats_1, Texs, MtlIndices} = split_mats_and_tex(Mats),
            [
                <<"<?xml version='1.0' encoding='utf-8'?>">>,
                to_bin(io_lib:format("<amf unit=\"~s\" version=\"1.1\">",
                                     [to_amf_unit(AMFUnit)])),
                    wr_metadata("name", Name),
                    wr_metadata("cad", Creator),
                    [ wr_amf_obj(Obj, MtlIndices, ObjNum) || {Obj, ObjNum} <- Objs ],
                    [ wr_amf_mat(Mt, MtlIndices) || Mt <- Mats_1],
                    [ wr_amf_tex(Tx, MtlIndices) || Tx <- Texs],
                <<"</amf>">>
            ];
        {error, Err} ->
            {error, Err}
    end.


wr_zip(Filename, AMFContents) ->
    Options = [],
    FilenameOnly = filename:basename(Filename),
    {ok, _} = zip:zip(Filename, [{FilenameOnly, AMFContents}], Options),
    ok.


%% Assign a number for each object
%%
number_objs(Objs) ->
    lists:zip(Objs, lists:seq(1, length(Objs))).


%% Try to split and triangulate along the seam of
%% multi-material objects to turn them into valid 
%% single material volumes, or return an error.
%% 
close_objs_single_mat(Objs_0) ->
    %% TODO
    close_objs_single_mat_1(Objs_0).
close_objs_single_mat_1(Objs) ->
    Objs_M = 
        [ ordsets:from_list(lists:append([ ML || #e3d_face{mat=ML} <- Efs]))
          || #e3d_object{obj=#e3d_mesh{fs=Efs}} <- Objs],
    case lists:all(fun (A) -> length(A) =:= 1 end, Objs_M) of
        true ->
            {ok, Objs};
        false ->
            ObjName = first_obj_name(Objs, [length(B) =/= 1 || B <- Objs_M]),
            {error, {one_material_per_volume, ObjName}}
    end.
    

first_obj_name([_|Objs], [false|L2]) ->
    first_obj_name(Objs, L2);
first_obj_name([#e3d_object{name=Name}|_], [true|_]) ->
    Name;
first_obj_name(_, []) ->
    "".


%% AMF Write Objects
-record(amfwrobj, {
    name = [],
    vs = [],
    vc = [],
    tx = [],
    vl = []
}).
-record(amfwrvl, {
    name = [],
    mat = none,
    fs = []
}).

name_o(ObjName) ->
    [First|_] = string:split(ObjName, ":"),
    First.

name_v(ObjName) ->
    case string:split(ObjName, ":") of
        [_, Second|_] ->
            Second;
        _ ->
            []
    end.


%% Combine separate E3D objects into one AMF object with
%% volumes separated by materials.
%%
combine_objs_v(Objs_0) ->
    %% Combine if the name before a colon is the same.
    CombF = fun({_, #e3d_object{name=ObjName1}},
        {_, #amfwrobj{name=ObjName2}}) ->
        case {name_o(ObjName1), name_o(ObjName2)} of
            {Str1, Str2} ->
                Str1 =:= Str2
        end
    end,
    combine_objs_v(Objs_0, CombF, []).
combine_objs_v([#e3d_object{obj=#e3d_mesh{vs=Vs}}=Obj|R], CombF, OL) ->
    OL_1 = combine_objs_v_m(Vs, CombF, Obj, OL),
    combine_objs_v(R, CombF, OL_1);
combine_objs_v([], _CombF, OL) ->
    lists:reverse([O || {_,O} <- OL]).

combine_objs_v_m(Vs, CombF, Obj, OL) ->
    combine_objs_v_m(gb_sets:from_list(Vs), CombF, Obj, OL, []).
combine_objs_v_m(Vs, CombF, Obj, [Obj2|OL], O2) ->
    case CombF({Vs, Obj}, Obj2) of
        false ->
            combine_objs_v_m(Vs, CombF, Obj, OL, [Obj2|O2]);
        true ->
            O2_1 = lists:reverse(O2) ++ [combine_objs_v_m_1(Obj, Obj2)],
            O2_1 ++ OL
    end;
combine_objs_v_m(Vs, _CombF, #e3d_object{name=ObjName}=Obj, [], O2) ->
    Obj_2 = combine_objs_v_m_2(Obj, #amfwrobj{name=name_o(ObjName)}),
    lists:reverse(O2) ++ [{Vs, Obj_2}].

combine_objs_v_m_1(#e3d_object{obj=#e3d_mesh{vs=Vs}}=Obj, {VsD, Obj_1}) ->
    Obj_2 = combine_objs_v_m_2(Obj, Obj_1),
    VsD2 = gb_sets:union(VsD, gb_sets:from_list(Vs)),
    {VsD2, Obj_2}.

combine_objs_v_m_2(
        #e3d_object{name=VName,obj=#e3d_mesh{vs=Vs1,vc=Vc1,tx=Tx1,fs=Fs1}}=Obj1,
        #amfwrobj{vs=Vs2,vc=Vc2,tx=Tx2,vl=VL0}=Obj2) ->
    case getobjmat(Obj1) of
        [Mat] ->
            Vs3 = Vs2 ++ Vs1,
            Vc3 = Vc2 ++ Vc1,
            Tx3 = Tx2 ++ Tx1,
            Fs3 = offsetfs(Fs1,
                length(Vs2),
                length(Vc2),
                length(Tx2)),
            Volume = #amfwrvl{name=name_v(VName),fs=Fs3,mat=Mat},
            Obj2#amfwrobj{vs=Vs3,vc=Vc3,tx=Tx3,vl=[Volume|VL0]};
        _ ->
            error(one_material_per_volume)
    end.

getobjmat(#e3d_object{obj=#e3d_mesh{fs=Fs}}) ->
    ordsets:from_list([Mat || #e3d_face{mat=[Mat]}=_ <- Fs]).

offsetfs(Fs1, OfsVs, OfsVc, OfsTx) ->
    offsetfs(Fs1, OfsVs, OfsVc, OfsTx, []).
offsetfs([#e3d_face{vs=Vs,vc=Vc,tx=Tx}=Fs1|R], OfsVs, OfsVc, OfsTx, OL) ->
    Vs_1 = [Iv+OfsVs || Iv <- Vs],
    Vc_1 = [Ic+OfsVc || Ic <- Vc],
    Tx_1 = [It+OfsTx || It <- Tx],
    offsetfs(R, OfsVs, OfsVc, OfsTx, [Fs1#e3d_face{vs=Vs_1,vc=Vc_1,tx=Tx_1}|OL]);
offsetfs([], _OfsVs, _OfsVc, _OfsTx, OL) ->
    lists:reverse(OL).


wr_amf_obj(#amfwrobj{name=ObjName,vs=Vs,vc=_Vc,tx=Tx_0,vl=Volumes}, MtlIndices, ObjNum) ->
    %%io:format("Volumes=~p~n", [Volumes]),
    VTx = array:from_list(Tx_0),
    [
        to_bin(io_lib:format("<object id=\"~w\">",[ObjNum])),
            wr_metadata("name", ObjName),
            <<"<mesh>">>,
                <<"<vertices>">>,
                [
                    [
                        <<"<vertex>">>,
                        <<"<coordinates>">>,
                            <<"<x>">>, flt_to_bin(X), <<"</x>">>,
                            <<"<y>">>, flt_to_bin(Y), <<"</y>">>,
                            <<"<z>">>, flt_to_bin(Z), <<"</z>">>,
                        <<"</coordinates>">>,
                        <<"</vertex>">>
                    ]
                || {X,Y,Z} <- Vs ],
                <<"</vertices>">>,
                [ wr_amf_obj_vlm(Tup, MtlIndices, VTx) || Tup <- Volumes ],
            <<"</mesh>">>,
        <<"</object>">>
    ].


wr_metadata(Name, Cont) ->
    to_bin(io_lib:format("<metadata type=\"~s\">~s</metadata>",
        [wr_esc(Name), wr_esc(Cont)])).

wr_esc(A_0) ->
    lists:foldl(
        fun({From, To}, A) ->
            lists:flatten(string:replace(A, From, To, all))
        end,
        A_0,
        [{"&","&amp;"},{"<", "&lt;"},{">", "&gt;"},{"\"","&dquot;"}]).


wr_amf_obj_vlm(#amfwrvl{name=Name,mat=MatName,fs=Efs}, MtlIndices, VTx) ->
    case gb_trees:lookup(MatName, MtlIndices) of
        {value, {mat, MtlIdx}} ->
            [
                to_bin(io_lib:format("<volume materialid=\"~w\">", [MtlIdx])),
                wr_metadata_opt("name", Name),
                [ wr_amf_obj_tri(Tri, none) || #e3d_face{vs=Tri} <- Efs ],
                <<"</volume>">>
            ];
        {value, {tex, MtlIdx, RGB}} ->
            [
                to_bin(io_lib:format("<volume materialid=\"~w\">", [MtlIdx])),
                wr_metadata_opt("name", Name),
                [ wr_amf_obj_tri(Tri, {RGB, TxL, VTx}) || #e3d_face{vs=Tri,tx=TxL} <- Efs ],
                <<"</volume>">>
            ]
    end.

wr_metadata_opt(_Name, "") ->
    [];
wr_metadata_opt(Name, Content) ->
    wr_metadata(Name, Content).

wr_amf_obj_tri([V1,V2,V3], RGBTexId) ->
    [
        <<"<triangle>">>,
        <<"<v1>">>, int_to_bin(V1), <<"</v1>">>,
        <<"<v2>">>, int_to_bin(V2), <<"</v2>">>,
        <<"<v3>">>, int_to_bin(V3), <<"</v3>">>,
        wr_texmap(RGBTexId),
        <<"</triangle>">>
    ].


wr_amf_mat({MatName, MatProp}, MtlIndices) ->
    MatName_S = atom_to_list(MatName),
    OpenGL = proplists:get_value(opengl, MatProp, []),
    {R,G,B,A} = proplists:get_value(diffuse, OpenGL, {0.8,0.8,0.8,1.0}),
    MtlIdx = case gb_trees:lookup(MatName, MtlIndices) of
        {value, {mat, MIdx1}} -> MIdx1;
        {value, {tex, MIdx2, _}} -> MIdx2
    end,
    [
        to_bin(io_lib:format("<material id=\"~w\">", [MtlIdx])),
        wr_metadata("name", MatName_S),
        wr_color({R,G,B,A}),
        <<"</material>">>
    ].

wr_amf_tex({MatName, MatProp}, MtlIndices) ->
    {value, {tex, _, TexIds}} = gb_trees:lookup(MatName, MtlIndices),
    RTexId = wr_amf_tex_1(TexIds),
    Maps = proplists:get_value(maps, MatProp, []),
    case proplists:get_value(diffuse, Maps, []) of
        #e3d_image{}=Img ->
            [
                [
                    to_bin(io_lib:format("<texture id=\"~w\" type=\"grayscale\" ", [RTexId+1+Idx])),
                    to_bin(io_lib:format(" width=\"~w\" height=\"~w\" depth=\"~w\" tiled=\"~w\">", [W,H,Depth,0])),
                    B64Bin,
                    <<"</texture>">>
                ]
            ||
                {Idx, #amtexmap{type=grayscale,w=W,h=H,d=Depth,b64=B64Bin}} <- tx_enc(Img)
            ]
    end.
wr_amf_tex_1({RTexId,_,_}) -> RTexId;
wr_amf_tex_1({RTexId,_,_,_}) -> RTexId.


wr_color({R,G,B,A}) ->
    [
    <<"<color>">>,
    <<"<r>">>, flt_to_bin(R), <<"</r>">>,
    <<"<g>">>, flt_to_bin(G), <<"</g>">>,
    <<"<b>">>, flt_to_bin(B), <<"</b>">>,
    <<"<a>">>, flt_to_bin(A), <<"</a>">>,
    <<"</color>">>
    ].


wr_texmap(none) ->
    [];
wr_texmap({TexIds, [TxI1, TxI2, TxI3], VTx})
  when is_integer(TxI1),
       is_integer(TxI2),
       is_integer(TxI3) ->
    {U1,V1} = array:get(TxI1, VTx),
    {U2,V2} = array:get(TxI2, VTx),
    {U3,V3} = array:get(TxI3, VTx),
    [
        <<"<texmap">>,
        wr_texmap_1(TexIds),
        <<">">>,
        <<"<utex1>">>, flt_to_bin(U1), <<"</utex1>">>,
        <<"<utex2>">>, flt_to_bin(U2), <<"</utex2>">>,
        <<"<utex3>">>, flt_to_bin(U3), <<"</utex3>">>,
        <<"<vtex1>">>, flt_to_bin(V1), <<"</vtex1>">>,
        <<"<vtex2>">>, flt_to_bin(V2), <<"</vtex2>">>,
        <<"<vtex3>">>, flt_to_bin(V3), <<"</vtex3>">>,
        <<"</texmap>">>
    ];
wr_texmap({_TexIds, _, _VTx}) ->
    [].

wr_texmap_1({RTexId, GTexId, BTexId}) ->
    to_bin(io_lib:format(" rtexid=\"~w\" gtexid=\"~w\" btexid=\"~w\"",
        [RTexId+1,GTexId+1,BTexId+1]));
wr_texmap_1({RTexId, GTexId, BTexId, ATexId}) ->
    to_bin(io_lib:format(" rtexid=\"~w\" gtexid=\"~w\" btexid=\"~w\" atexid=\"~w\"",
        [RTexId+1,GTexId+1,BTexId+1,ATexId+1])).


int_to_bin(A) ->
    integer_to_binary(A).

flt_to_bin(A) ->
    float_to_binary(A, [{decimals,10},compact]).

to_bin(A) ->
    iolist_to_binary(A).
    


%% Split materials between solid colors and textures.
%%
split_mats_and_tex(Mats) ->
    split_mats_and_tex(Mats, 0, [], 0, [], gb_trees:empty()).
split_mats_and_tex([{MatName, Prop}=Mt|Mats], Count1, O1, Count2, O2, MI) ->
    Maps = proplists:get_value(maps, Prop, []),
    case proplists:get_value(diffuse, Maps, none) of
        none ->
            MI_1 = gb_trees:insert(MatName, {mat, Count1+1}, MI),
            split_mats_and_tex(Mats, Count1+1, [Mt|O1], Count2, O2, MI_1);
        #e3d_image{type=NoAlpha}=_
          when NoAlpha =:= g8;
               NoAlpha =:= r8g8b8 ->
            MI_1 = gb_trees:insert(MatName, {tex, Count1+1, {Count2,Count2+1,Count2+2}}, MI),
            split_mats_and_tex(Mats, Count1+1, [Mt|O1], Count2+3, [Mt|O2], MI_1);
        #e3d_image{type=HasAlpha}=_
          when HasAlpha =:= g8a8;
               HasAlpha =:= r8g8b8a8 ->
            MI_1 = gb_trees:insert(MatName, {tex, Count1+1, {Count2,Count2+1,Count2+2,Count2+3}}, MI),
            split_mats_and_tex(Mats, Count1+1, [Mt|O1], Count2+4, [Mt|O2], MI_1)
    end;
split_mats_and_tex([], _, O1, _, O2, MI) ->
    {lists:reverse(O1), lists:reverse(O2), MI}.


to_amf_unit(Unit) ->
    case Unit of
        mm -> "millimeter";
        in -> "inch";
        ft -> "feet";
        meter -> "meter";
        micron -> "micron"
    end.


%%
%% Read Additive Manufacturing File
%%

open_amf_file(ModelUnit, Filename) ->
    {ok, Cont} = file:read_file(Filename),
    case Cont of
        <<"PK",_/binary>> ->
            case open_amf_container(Filename) of
                {ok, XmlCont} ->
                    read_amf_content(ModelUnit, XmlCont)
            end;
        _ ->
            read_amf_content(ModelUnit, Cont)
    end.

open_amf_container(Filename) ->
    case zip:zip_open(Filename, [memory]) of
        {ok, ZH} ->
            {ok, FileList} = zip:zip_list_dir(ZH),
            XmlList = open_amf_get_files(ZH, FileList),
            zip:zip_close(ZH),
            [XmlCont|_] = [Bin || {_Name, Bin} <- XmlList],
            {ok, XmlCont};
        Error ->
            Error
    end.
    
open_amf_get_files(ZH, FileList) ->
    open_amf_get_files(ZH, FileList, []).

open_amf_get_files(ZH, [#zip_file{name=Name}=_File|FileList], XmlList) ->
    Ext = string:to_lower(filename:extension(Name)),
    case Ext of
        ".amf" ->
            {ok, Res} = zip:zip_get(Name, ZH),
            XmlList_1 = [Res | XmlList];
        _ ->
            XmlList_1 = XmlList
    end,
    open_amf_get_files(ZH, FileList, XmlList_1);
open_amf_get_files(ZH, [_OtherRec|FileList], XmlList) ->
    open_amf_get_files(ZH, FileList, XmlList);
open_amf_get_files(_ZH, [], XmlList) ->
    XmlList.
    

%%%
%%%

-record(amvrt, {
    x,
    y,
    z
}).

-record(amtri, {
    v1,
    v2,
    v3,
    col=none,
    utx1=none,
    vtx1=none,
    utx2=none,
    vtx2=none,
    utx3=none,
    vtx3=none,
    rgbtexid=none
}).

-record(amcol, {
    r=0.0,
    g=0.0,
    b=0.0,
    a=1.0
}).

-record(amvlm, {
    name = "",
    mtl,
    tl
}).

-record(amobj, {
    name = "",
    id,
    vs,
    vl,
    col
}).

-record(ammtl, {
    id,
    col = none
}).

-record(amtex, {
    id,
    w,
    h,
    d,
    t,
    type,
    bin
}).


%% State file for xmerl sax.
-record(amftk, {
    unit = mm,
    mtls = [],
    objs = [],
    texs = [],
    
    obj_id = 0,

    mesh = [],
    verts = [],
    vt,
    volumes = [],
    tl = [],
    tri_at,
    
    col_at = none,
    mtl_at = [],
    
    mtl_mtl_id = none,
    
    vlm_mtl_id = none,
    tri_mtl_id = none,
    char,
    
    tex_at,
    
    metadat = [],
    mdat_type
    }).

read_amf_content(ModelUnit, Bin) ->
    read_amf_content_1(ModelUnit, Bin).
read_amf_content_1(ModelUnit, Bin_1) ->
    EF = {event_fun, fun amf_tok/3},
    ES = {event_state, #amftk{}},
    case xmerl_sax_parser:stream(Bin_1, [EF,ES]) of
        {ok, #amftk{unit=AMFUnit,mtls=Mats,objs=Objs,texs=Texs}=_Es, _} ->
            Texs_1 = make_e3dtex(Texs, get_tex_rgb(Objs)),
            Objs_1 = make_e3dobj(Objs),
            Mats_1 = make_e3dmat(Mats),
            ScaleF = scale_from_units(import, units_tuple(ModelUnit, AMFUnit)),
            Objs_2 = scale_objects(Objs_1, ScaleF),
            Mats_2 = fill_missing_materials(Mats_1, Objs_1),
            {ok, {Objs_2, Mats_2 ++ Texs_1}};
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("amf:~p: ERROR: ~p:~p~n", [Line, Error, Reason]),
            {error, ?__(1,"unknown/unhandled format, see log window")}
    end.

make_e3dobj(Objs) ->
    make_e3dobj(Objs, []).
make_e3dobj([#amobj{name=ObjName,id=_Num,vs=Vs,vl=Vlm,col=_Col}|R], OL) ->
    %% Create an object for each volume as each volume is a manifold.
    {VVs, VVc} = make_e3dobj_vs(Vs),
    {VF, UVL_1} = make_e3dobj_vlm(Vlm, gb_trees:empty()),
    VTx = get_list_from_vl(UVL_1),
    Obj1 = [#e3d_object{
        name=make_e3dobj_name(ObjName, VlName),
        obj=#e3d_mesh{
            type=triangle,
            vs=VVs, vc=VVc, tx=VTx,
             fs=Efs
        }
    } || {VlName, Efs} <- VF],
    make_e3dobj(R, [Obj1|OL]);
make_e3dobj([], OL) ->
    lists:append(lists:reverse(OL)).


make_e3dobj_name(ObjName, VlName)
  when is_list(ObjName), is_list(VlName),
       length(ObjName) > 0, length(VlName) > 0 ->
    ObjName ++ ":" ++ VlName.


make_e3dobj_vs(Vs) ->
    make_e3dobj_vs(Vs, []).
make_e3dobj_vs([#amvrt{x=X,y=Y,z=Z}=_|R], OL) ->
    V = {X,Y,Z},
    make_e3dobj_vs(R, [V|OL]);
make_e3dobj_vs([], OL) ->
    {lists:reverse(OL), []}.


make_e3dobj_vlm(Vlm, UVL) ->
    make_e3dobj_vlm(Vlm, [], UVL).
make_e3dobj_vlm([#amvlm{name=Name,mtl=MtlV,tl=TL}=_|R], OL, UVL_0) ->
    {Vl_1,UVL_1} = make_e3dfs(TL, MtlV, UVL_0),
    make_e3dobj_vlm(R, [{Name, Vl_1}|OL], UVL_1);
make_e3dobj_vlm([], OL, UVL) ->
    {lists:reverse(OL), UVL}.


make_e3dfs(Vlm, MtlV, UVL) ->
    make_e3dfs(Vlm, MtlV, [], UVL).
make_e3dfs([#amtri{v1=V1,v2=V2,v3=V3,col=_,rgbtexid=Tex}=AmTri|R], MtlV, OL, UVL_0) ->
    {TxL, UVL_1} = make_e3dfs_uv(AmTri, UVL_0),
    Vl_1 = #e3d_face{
        vs=[V1,V2,V3],
        vc=[],
        tx=TxL,
        mat=make_e3dfs_mat(Tex, MtlV)
    },
    make_e3dfs(R, MtlV, [Vl_1|OL], UVL_1);
make_e3dfs([], _MtlV, OL, UVL) ->
    {lists:reverse(OL), UVL}.


make_e3dfs_uv(#amtri{utx1=UTx1,utx2=UTx2,utx3=UTx3,
                     vtx1=VTx1,vtx2=VTx2,vtx3=VTx3}=_, UVL_0)
  when is_float(UTx1), is_float(UTx2), is_float(UTx3),
       is_float(VTx1), is_float(VTx2), is_float(VTx3) ->
    {Idx1, UVL_1} = get_next_idx({UTx1,VTx1}, UVL_0),
    {Idx2, UVL_2} = get_next_idx({UTx2,VTx2}, UVL_1),
    {Idx3, UVL_3} = get_next_idx({UTx3,VTx3}, UVL_2),
    {[Idx1,Idx2,Idx3], UVL_3};
make_e3dfs_uv(_, UVL) ->
    {[], UVL}.

get_next_idx(Val, VL) ->
    case gb_trees:lookup(Val, VL) of
        none ->
            Idx = gb_trees:size(VL),
            {Idx, gb_trees:insert(Val, Idx, VL)};
        {value, Idx} ->
            {Idx, VL}
    end.


make_e3dfs_mat(none, none) ->
    [];
make_e3dfs_mat(none, MtlV)
  when is_integer(MtlV) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("mat_~w", [MtlV]))),
    [MatName];
make_e3dfs_mat({RTexId, GTexId, BTexId}, _) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("tex_~w_~w_~w", [RTexId, GTexId, BTexId]))),
    [MatName].


make_e3dtex(Texs, L) ->
    Texs_0 = orddict:from_list([{Id, Tex} || #amtex{id=Id}=Tex <- Texs]),
    Texs_1 = gb_trees:from_orddict(Texs_0),
    [make_e3dtex_1(Tup, Texs_1) || Tup <- L].
make_e3dtex_1({RTexId, GTexId, BTexId}, Texs_1) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("tex_~w_~w_~w", [RTexId, GTexId, BTexId]))),
    make_e3dtex_3(MatName,
        make_e3dtex_2(
            gb_trees:lookup(RTexId, Texs_1),
            gb_trees:lookup(GTexId, Texs_1),
            gb_trees:lookup(BTexId, Texs_1), none));
make_e3dtex_1({RTexId, GTexId, BTexId, ATexId}, Texs_1) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("tex_~w_~w_~w_~w", [RTexId, GTexId, BTexId, ATexId]))),
    make_e3dtex_3(MatName,
        make_e3dtex_2(
            gb_trees:lookup(RTexId, Texs_1),
            gb_trees:lookup(GTexId, Texs_1),
            gb_trees:lookup(BTexId, Texs_1),
            gb_trees:lookup(BTexId, Texs_1))).
make_e3dtex_2({value, RTex}, {value, GTex}, {value, BTex}, none) ->
    tx_dec([
          make_e3dtex_txmap(RTex),
          make_e3dtex_txmap(GTex),
          make_e3dtex_txmap(BTex) ]);
make_e3dtex_2({value, RTex}, {value, GTex}, {value, BTex}, {value, ATex}) ->
    tx_dec([
          make_e3dtex_txmap(RTex),
          make_e3dtex_txmap(GTex),
          make_e3dtex_txmap(BTex),
          make_e3dtex_txmap(ATex)]).
make_e3dtex_3(MatName, DiffuseImg) ->
    MapsL = [
        {diffuse, DiffuseImg}
    ],
    {MatName, simple_mtl(0.8, 0.8, 0.8, 1.0) ++ [{maps, MapsL}]}.


make_e3dtex_txmap(#amtex{id=_,w=Width,h=Height,d=Depth,type=Type,bin=Str}) ->
    #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=Str}.


get_list_from_vl(UVL_0) ->
    List = [{B,A} || {A,B} <- gb_trees:to_list(UVL_0)],
    [D || {_, D} <- lists:usort(List)].


fill_missing_materials(Mats, Objs) ->
    Mats_S = ordsets:from_list([MatName || {MatName, _} <- Mats]),
    Mats ++ fill_missing_materials_1(Mats_S, Objs, []).
fill_missing_materials_1(Mats_S, [#e3d_object{obj=#e3d_mesh{fs=Efs}}|Objs], OL) ->
    Mats1 = fill_missing_materials_fs(Efs),
    case ordsets:subtract(Mats1, Mats_S) of
        [] ->
            fill_missing_materials_1(Mats_S, Objs, OL);
        NewMats ->
            NewMats_1 = [{M, simple_mtl()} || M <- NewMats],
            Mats_S_1 = ordsets:union(Mats_S, NewMats),
            fill_missing_materials_1(Mats_S_1, Objs, [NewMats_1|OL])
    end;
fill_missing_materials_1(_Mats_S, [], OL) ->
    lists:append(lists:reverse(OL)).
fill_missing_materials_fs(Efs) ->
    ordsets:from_list(lists:flatten([MatL || #e3d_face{mat=MatL} <- Efs])).


simple_mtl() ->
    simple_mtl(0.8, 0.8, 0.8, 1.0).
simple_mtl(ClrR, ClrG, ClrB, ClrA) ->
    OpenGL = [
        {diffuse, {ClrR, ClrG, ClrB, ClrA}},
        {emissive, {0.0, 0.0, 0.0, 1.0}},
        {specular, {1.0, 1.0, 1.0, 1.0}},
        {ambient, {0.0, 0.0, 0.0, 1.0}},
        {metallic, 0.2},
        {roughness, 0.8}
    ],
    [{opengl, OpenGL}].


%% Get a list of the tuples of RGB texture ids.
%%
get_tex_rgb(Objs) ->
    lists:usort(lists:flatten([get_tex_rgb_1(Obj) || Obj <- Objs])).
get_tex_rgb_1(#amobj{vl=Vl}=_) ->
    [get_tex_rgb_2(VV) || VV <- Vl].
get_tex_rgb_2(#amvlm{tl=Tl}=_) ->
    [get_tex_rgb_3(Tri) || Tri <- Tl].
get_tex_rgb_3(#amtri{rgbtexid=none}=_) ->
    [];
get_tex_rgb_3(#amtri{rgbtexid=RGBTexId}=_)
  when is_tuple(RGBTexId) ->
    [RGBTexId].


%%%
%%%

make_e3dmat(Mats) ->
    make_e3dmat(Mats, []).
make_e3dmat([#ammtl{id=Num,col=#amcol{}=Col}|R], OL) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("mat_~w", [Num]))),
    {ClrR,ClrG,ClrB,ClrA} = make_e3dmat_col(Col),
    Mt = {MatName, simple_mtl(ClrR, ClrG, ClrB, ClrA)},
    make_e3dmat(R, [Mt|OL]);
make_e3dmat([#ammtl{id=Num,col=none}|R], OL) ->
    MatName = list_to_atom(lists:flatten(
        io_lib:format("mat_~w", [Num]))),
    Mt = {MatName, simple_mtl()},
    make_e3dmat(R, [Mt|OL]);
make_e3dmat([], OL) ->
    lists:reverse(OL).
    
make_e3dmat_col(#amcol{r=ClrR,g=ClrG,b=ClrB,a=ClrA}) ->
    {ClrR,ClrG,ClrB,ClrA}.


%% xmerl tokenizer
%%
amf_tok({startElement, _, LName, _, Attrs}=_Ev, _Loc, #amftk{}=State) ->
    amf_tok_s(LName, Attrs, State);
amf_tok({endElement, _, LName, _}=_Ev, _Loc, #amftk{}=State) ->
    amf_tok_e(LName, State);
amf_tok({characters, Chars}=_Ev, _Loc, #amftk{}=State) ->
    State#amftk{char=Chars};
amf_tok(startDocument, _, State) -> State;
amf_tok(endDocument, _, State) -> State;
amf_tok(_Ev, _Loc, State) ->
    State.

amf_tok_s("amf", Attrs, State) ->
    push_metadat(State#amftk{unit=amf_tok_get_unit(Attrs)});
amf_tok_s("metadata", Attrs, State) ->
    MDatType = amf_tok_get_str("type", Attrs),
    clear_char(State#amftk{mdat_type=MDatType});
amf_tok_s("object", Attrs, State) ->
    push_metadat(State#amftk{obj_id=amf_tok_get_int("id", Attrs)});
amf_tok_s("mesh", _Attrs, State) ->
    push_metadat(State);
amf_tok_s("vertices", _Attrs, State) ->
    push_metadat(State);
amf_tok_s("vertex", _Attrs, State) ->
    State;
amf_tok_s("coordinates", _Attrs, State) ->
    State#amftk{vt=#amvrt{}};
amf_tok_s("x", _Attrs, State) -> clear_char(State);
amf_tok_s("y", _Attrs, State) -> clear_char(State);
amf_tok_s("z", _Attrs, State) -> clear_char(State);
amf_tok_s("volume", Attrs, State) ->
    push_metadat(State#amftk{vlm_mtl_id=amf_tok_get_int("materialid", Attrs),tl=[]});
amf_tok_s("triangle", Attrs, State) ->
    State#amftk{tri_at=#amtri{},tri_mtl_id=amf_tok_get_int("materialid", Attrs)};
amf_tok_s("v1", _Attrs, State) -> clear_char(State);
amf_tok_s("v2", _Attrs, State) -> clear_char(State);
amf_tok_s("v3", _Attrs, State) -> clear_char(State);

amf_tok_s("normal", _Attrs, State) -> State;
amf_tok_s("nx", _Attrs, State) -> clear_char(State);
amf_tok_s("ny", _Attrs, State) -> clear_char(State);
amf_tok_s("nz", _Attrs, State) -> clear_char(State);

amf_tok_s("texmap", Attrs, #amftk{tri_at=T_At}=State) ->
    State#amftk{
        tri_at=T_At#amtri{
            rgbtexid=amf_tok_tri_texid(Attrs)
        }
    };
amf_tok_s("utex1", _Attrs, State) -> clear_char(State);
amf_tok_s("utex2", _Attrs, State) -> clear_char(State);
amf_tok_s("utex3", _Attrs, State) -> clear_char(State);
amf_tok_s("vtex1", _Attrs, State) -> clear_char(State);
amf_tok_s("vtex2", _Attrs, State) -> clear_char(State);
amf_tok_s("vtex3", _Attrs, State) -> clear_char(State);

amf_tok_s("material", Attrs, State) ->
    push_metadat(State#amftk{mtl_mtl_id=amf_tok_get_int("id", Attrs)});
amf_tok_s("color", _Attrs, State) ->
    State#amftk{col_at=#amcol{}};
amf_tok_s("r", _Attrs, State) -> clear_char(State);
amf_tok_s("g", _Attrs, State) -> clear_char(State);
amf_tok_s("b", _Attrs, State) -> clear_char(State);
amf_tok_s("a", _Attrs, State) -> clear_char(State);

amf_tok_s("texture", Attrs, State) ->
    clear_char(State#amftk{tex_at=#amtex{
        id=amf_tok_get_int("id", Attrs),
        w=amf_tok_get_int("width", Attrs),
        h=amf_tok_get_int("height", Attrs),
        d=amf_tok_get_int("depth", Attrs),
        t=amf_tok_get_int("tiled", Attrs),
        type=amf_tok_get_textype(Attrs)
    }});
amf_tok_s(_, _Attrs, State) ->
    State.


amf_tok_e("amf", State) -> pop_metadat(State);
amf_tok_e("metadata", #amftk{mdat_type=MDatType,char=Char}=State) ->
    add_metadat(clear_char(State),
        { string:lowercase(MDatType), Char });
amf_tok_e("object", #amftk{verts=Verts,volumes=Volumes_0,obj_id=ObjId,col_at=ColAt,objs=Objs_0}=State) ->
    Volumes = lists:reverse(Volumes_0),
    Nm = amf_getname(State, object),
    Obj = #amobj{name=Nm,id=ObjId,vs=Verts,vl=Volumes,col=ColAt},
    pop_metadat(State#amftk{objs=[Obj|Objs_0],col_at=none});

amf_tok_e("mesh", State) ->
    pop_metadat(State);

amf_tok_e("vertices", #amftk{verts=Verts}=State) ->
    pop_metadat(State#amftk{verts=lists:reverse(Verts)});

amf_tok_e("vertex", #amftk{verts=Verts,vt=Vt}=State) ->
    State#amftk{verts=[Vt|Verts],col_at=none};
amf_tok_e("coordinates", State) ->
    State;
amf_tok_e("x", #amftk{vt=Vt}=State) ->
    clear_char(State#amftk{vt=Vt#amvrt{x=amf_parse_flt(State)}});
amf_tok_e("y", #amftk{vt=Vt}=State) ->
    clear_char(State#amftk{vt=Vt#amvrt{y=amf_parse_flt(State)}});
amf_tok_e("z", #amftk{vt=Vt}=State) ->
    clear_char(State#amftk{vt=Vt#amvrt{z=amf_parse_flt(State)}});

amf_tok_e("volume", #amftk{vlm_mtl_id=MtlId,tl=Tl0,volumes=Volumes_0}=State) ->
    Nm = amf_getname(State, volume),
    Volume = #amvlm{name=Nm,mtl=MtlId, tl=lists:reverse(Tl0)},
    pop_metadat(State#amftk{volumes=[Volume|Volumes_0]});

amf_tok_e("triangle", #amftk{tri_at=T_At,col_at=ColAt,tl=Tl0}=State) ->
    Tri = T_At#amtri{col=ColAt},
    State#amftk{tl=[Tri|Tl0],col_at=none};
amf_tok_e("v1", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{v1=amf_parse_int(State)}});
amf_tok_e("v2", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{v2=amf_parse_int(State)}});
amf_tok_e("v3", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{v3=amf_parse_int(State)}});

amf_tok_e("normal", State) -> State;
amf_tok_e("nx", State) -> clear_char(State);
amf_tok_e("ny", State) -> clear_char(State);
amf_tok_e("nz", State) -> clear_char(State);


amf_tok_e("texmap", State) ->
    State;
amf_tok_e("utex1", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{utx1=amf_parse_flt(State)}});
amf_tok_e("utex2", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{utx2=amf_parse_flt(State)}});
amf_tok_e("utex3", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{utx3=amf_parse_flt(State)}});
amf_tok_e("vtex1", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{vtx1=amf_parse_flt(State)}});
amf_tok_e("vtex2", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{vtx2=amf_parse_flt(State)}});
amf_tok_e("vtex3", #amftk{tri_at=T_At}=State) ->
    clear_char(State#amftk{tri_at=T_At#amtri{vtx3=amf_parse_flt(State)}});


amf_tok_e("material", #amftk{mtl_mtl_id=MtlId,col_at=ColAt,mtls=Mtls_0}=State) ->
    Mtl = #ammtl{id=MtlId,col=ColAt},
    pop_metadat(State#amftk{mtls=[Mtl|Mtls_0],col_at=none});
amf_tok_e("color", State) ->
    State;
amf_tok_e("r", #amftk{col_at=ColAt}=State) ->
    clear_char(State#amftk{col_at=ColAt#amcol{r=amf_parse_flt(State)}});
amf_tok_e("g", #amftk{col_at=ColAt}=State) ->
    clear_char(State#amftk{col_at=ColAt#amcol{g=amf_parse_flt(State)}});
amf_tok_e("b", #amftk{col_at=ColAt}=State) ->
    clear_char(State#amftk{col_at=ColAt#amcol{b=amf_parse_flt(State)}});
amf_tok_e("a", #amftk{col_at=ColAt}=State) ->
    clear_char(State#amftk{col_at=ColAt#amcol{a=amf_parse_flt(State)}});

amf_tok_e("texture", #amftk{tex_at=TexAt,char=Char,texs=Texs_0}=State) ->
    Tex = TexAt#amtex{bin=Char},
    clear_char(State#amftk{texs=[Tex|Texs_0],tex_at=none,char=none});

amf_tok_e(_, State) ->
    State.


clear_char(Stt) ->
    Stt#amftk{char=none}.
    

push_metadat(#amftk{metadat=MList}=Stt) ->
    Stt#amftk{metadat=[[]|MList]}.    

pop_metadat(#amftk{metadat=[_|MList]}=Stt) ->
    Stt#amftk{metadat=MList}.

add_metadat(#amftk{metadat=[A|MList]}=Stt, {Key, Val}) ->
    A_1 = orddict:store(Key, Val, A),
    Stt#amftk{metadat=[A_1|MList]}.

get_metadat(#amftk{metadat=[A|_]}=_, Key) ->
    case orddict:find(Key, A) of
        {ok, Val} -> Val;
        _ -> none
    end.


%% Places where to find a name for the object or volume:
%% "name" metadata
%% object id number fallback
%% generated number
%%
amf_getname(State, object) ->
    case get_metadat(State, "name") of
        none ->
            amf_getname_1(State, "obj");
        ObjName when is_list(ObjName) ->
            ObjName
    end;
amf_getname(State, volume) ->
    case get_metadat(State, "name") of
        none ->
            amf_getname_2(State, "volume");
        VolumeName when is_list(VolumeName) ->
            VolumeName
    end.

amf_getname_1(#amftk{objs=V,obj_id=ObjId}=_State, Str) ->
    case ObjId of
        none ->
            lists:flatten(io_lib:format("~s_~w", [Str, length(V)+1]));
        Number when is_integer(Number) ->
            lists:flatten(io_lib:format("~s_~w", [Str, Number]))
    end.

amf_getname_2(#amftk{volumes=V}=_, Str) ->
    lists:flatten(io_lib:format("~s_~w", [Str, length(V)+1])).

    
amf_parse_int(#amftk{char=Char}=_) ->
    CharT = string:trim(Char),
    case string:to_integer(CharT) of
        {Num, []} when is_integer(Num) -> Num;
        _ ->
            case string:to_float(CharT) of
                {Num, _} when is_float(Num) -> round(Num);
                {error, _} ->
                    none
            end
    end.

amf_parse_flt(#amftk{char=Char}=_) ->
    CharT = string:trim(Char),
    case string:to_integer(CharT) of
        {Num, []} when is_integer(Num) -> float(Num);
        _ ->
            case string:to_float(CharT) of
                {Num, _} when is_float(Num) -> Num;
                {error, _} ->
                    none
            end
    end.
    

amf_tok_get_unit(AttrList) ->
    case amf_tok_get_str("unit", AttrList) of
        none -> mm;
        Str when is_list(Str) ->
            case lc(Str) of
                "millimeter" ++ _ -> mm;
                "inch" ++ _ -> in;
                "feet" ++ _ -> ft;
                "meter" ++ _ -> meter;
                "micron" ++ _ -> micron;
                _ -> mm
            end
    end.


amf_tok_get_int(AttrName, [{_, _, AttrName, Val}|_]) ->
    ValT = string:trim(Val),
    case string:to_integer(ValT) of
        {Num, _} when is_integer(Num) -> Num;
        {error, _} ->
            case string:to_float(ValT) of
                {Num, _} when is_float(Num) -> round(Num);
                {error, _} ->
                    none
            end
    end;
amf_tok_get_int(AttrName, [_|R]) ->
    amf_tok_get_int(AttrName, R);
amf_tok_get_int(_, []) ->
    none.


amf_tok_get_textype(AttrList) ->
    case lc(amf_tok_get_str("type", AttrList)) of
        "grayscale" -> grayscale;
        _ -> grayscale
    end.
    

amf_tok_get_str(AttrName, [{_, _, AttrName, Val}|_]) ->
    ValT = string:trim(Val),
    ValT;
amf_tok_get_str(AttrName, [_|R]) ->
    amf_tok_get_str(AttrName, R);
amf_tok_get_str(_, []) ->
    none.


amf_tok_tri_texid(Attrs) ->
    RTexId_0 = amf_tok_get_int("rtexid", Attrs),
    GTexId_0 = amf_tok_get_int("gtexid", Attrs),
    BTexId_0 = amf_tok_get_int("btexid", Attrs),
    ATexId_0 = amf_tok_get_int("atexid", Attrs),
    case {RTexId_0, GTexId_0, BTexId_0, ATexId_0} of
        {RTexId, GTexId, BTexId, ATexId}
          when is_integer(RTexId),
               is_integer(GTexId),
               is_integer(BTexId),
               is_integer(ATexId) ->
            {RTexId, GTexId, BTexId, ATexId};
        {RTexId, GTexId, BTexId, none}
          when is_integer(RTexId),
               is_integer(GTexId),
               is_integer(BTexId) ->
            {RTexId, GTexId, BTexId};
        _ ->
            none
    end.


scale_objects(Objs, ScaleF) ->
    [ scale_objects_1(Obj, ScaleF) || Obj <- Objs].
scale_objects_1(#e3d_object{obj=#e3d_mesh{vs=Vs}=Mesh}=Obj, Scl) ->
    Obj#e3d_object{obj=Mesh#e3d_mesh{vs=[{X*Scl,Y*Scl,Z*Scl} || {X,Y,Z} <- Vs]}}.


%%%
%%% Unit conversion
%%%

scale_from_units(export, {WU, AMFUnit}) ->
    unit_ratio(WU, AMFUnit);
scale_from_units(import, Units) ->
    1.0 / scale_from_units(export, Units).

unit_scaled_mm(micron) ->     0.001 * unit_scaled_mm(mm);
unit_scaled_mm(mm) ->           1.0;
unit_scaled_mm(cm) ->          10.0 * unit_scaled_mm(mm);
unit_scaled_mm(dm) ->         100.0 * unit_scaled_mm(mm);
unit_scaled_mm(meter)  ->    1000.0 * unit_scaled_mm(mm);

unit_scaled_mm(in) ->           (1.0 / 0.03937008) * unit_scaled_mm(mm);
unit_scaled_mm(ft) ->          12.0 * unit_scaled_mm(in);
unit_scaled_mm(yd) ->           3.0 * unit_scaled_mm(ft).

unit_ratio(Unit1, Unit2)
  when Unit1 =:= Unit2 ->
    1.0;
unit_ratio(Unit1, Unit2) ->
    unit_scaled_mm(Unit1) / unit_scaled_mm(Unit2).


lc(A) ->
    string:lowercase(A).

%%%
%%%
%%%

%% Decode from grayscale channels
%%

tx_dec([#amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrR}=_,
        #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrG}=_,
        #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrB}=_]) ->
    R=tex(base64:decode(StrR), Width, Height, Depth, Type),
    G=tex(base64:decode(StrG), Width, Height, Depth, Type),
    B=tex(base64:decode(StrB), Width, Height, Depth, Type),
    Dec = intrgb(R,G,B),
    #e3d_image{type=r8g8b8,bytes_pp=3,alignment=1,order=lower_left,
               width=Width,height=Height,image=Dec};
tx_dec([#amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrR}=_,
        #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrG}=_,
        #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrB}=_,
        #amtexmap{type=Type,w=Width,h=Height,d=Depth,b64=StrA}=_]) ->
    R=tex(base64:decode(StrR), Width, Height, Depth, Type),
    G=tex(base64:decode(StrG), Width, Height, Depth, Type),
    B=tex(base64:decode(StrB), Width, Height, Depth, Type),
    A=tex(base64:decode(StrA), Width, Height, Depth, Type),
    Dec = intrgba(R,G,B,A),
    #e3d_image{type = r8g8b8a8,bytes_pp=4,alignment=1,order=lower_left,
               width=Width,height=Height,image=Dec}.

tex(Bin, W, H, D, T) when D =:= 1, T =:= grayscale ->
    tex_b1(Bin, W, H, H, W, [], []).
tex_b1(<<C,Bin/binary>>, W, H, J, I, Rows, Line)
  when I > 0, J > 0 ->
    tex_b1(Bin, W, H, J, I-1, Rows, [C|Line]);
tex_b1(_, _W, _H, 0, _, Rows, []) ->
    lists:reverse(Rows);
tex_b1(Bin, W, H, J, 0, Rows, Line) ->
    tex_b1(Bin, W, H, J-1, W, [lists:reverse(Line)|Rows], []).

intrgb(R,G,B) ->
    intrgb(R,G,B,[]).
intrgb([R|RR],[G|GR],[B|BR],OL) ->
    Line = intrgb_1(R,G,B,[]),
    intrgb(RR,GR,BR,[Line|OL]);
intrgb([],[],[],OL) ->
    iolist_to_binary(lists:reverse(OL)).
intrgb_1([R|RA],[G|GA],[B|BA],OL) ->
    intrgb_1(RA,GA,BA,[<<R,G,B>>|OL]);
intrgb_1([],[],[],OL) ->
    iolist_to_binary(lists:reverse(OL)).

intrgba(R,G,B,A) ->
    intrgba(R,G,B,A,[]).
intrgba([R|RR],[G|GR],[B|BR],[A|AR],OL) ->
    Line = intrgba_1(R,G,B,A,[]),
    intrgba(RR,GR,BR,AR,[Line|OL]);
intrgba([],[],[],[],OL) ->
    iolist_to_binary(lists:reverse(OL)).
intrgba_1([R|RA],[G|GA],[B|BA],[A|AA],OL) ->
    intrgba_1(RA,GA,BA,AA,[<<R,G,B,A>>|OL]);
intrgba_1([],[],[],[],OL) ->
    iolist_to_binary(lists:reverse(OL)).

    

%% Encode to grayscale channels.
%%
tx_enc(#e3d_image{type=Type,width=Width,height=Height,image=Bin}) ->
    [ {Idx, AmTexMap#amtexmap{w=Width,h=Height}}
      || {Idx, AmTexMap} <- tx_enc(Bin, Type)].
tx_enc(Bin, g8) ->
    t_enc_8u1(Bin, []);
tx_enc(Bin, g8a8) ->
    t_enc_8u2(Bin, [], []);
tx_enc(Bin, r8g8b8) ->
    t_enc_8u3(Bin, [], [], []);
tx_enc(Bin, r8g8b8a8) ->
    t_enc_8u4(Bin, [], [], [], []).


t_enc_8u1(<<G,Bin/binary>>, OG) ->
    t_enc_8u1(Bin, [G|OG]);
t_enc_8u1(<<>>, OG) ->
    BG = list_to_binary(lists:reverse(OG)),
    [{0,#amtexmap{type=grayscale,d=1,b64=BG}},
     {1,#amtexmap{type=grayscale,d=1,b64=BG}},
     {2,#amtexmap{type=grayscale,d=1,b64=BG}}].

t_enc_8u2(<<G,A,Bin/binary>>, OG, OA) ->
    t_enc_8u2(Bin, [G|OG], [A|OA]);
t_enc_8u2(<<>>, OG, OA) ->
    BG = base64:encode(list_to_binary(lists:reverse(OG))),
    BA = base64:encode(list_to_binary(lists:reverse(OA))),
    [{0,#amtexmap{type=grayscale,d=1,b64=BG}},
     {1,#amtexmap{type=grayscale,d=1,b64=BG}},
     {2,#amtexmap{type=grayscale,d=1,b64=BG}},
     {3,#amtexmap{type=grayscale,d=1,b64=BA}}].

t_enc_8u3(<<R,G,B,Bin/binary>>, OR, OG, OB) ->
    t_enc_8u3(Bin, [R|OR], [G|OG], [B|OB]);
t_enc_8u3(<<>>, OR, OG, OB) ->
    BR = base64:encode(list_to_binary(lists:reverse(OR))),
    BG = base64:encode(list_to_binary(lists:reverse(OG))),
    BB = base64:encode(list_to_binary(lists:reverse(OB))),
    [{0,#amtexmap{type=grayscale,d=1,b64=BR}},
     {1,#amtexmap{type=grayscale,d=1,b64=BG}},
     {2,#amtexmap{type=grayscale,d=1,b64=BB}}].

t_enc_8u4(<<R,G,B,A,Bin/binary>>, OR, OG, OB, OA) ->
    t_enc_8u4(Bin, [R|OR], [G|OG], [B|OB], [A|OA]);
t_enc_8u4(<<>>, OR, OG, OB, OA) ->
    BR = base64:encode(list_to_binary(lists:reverse(OR))),
    BG = base64:encode(list_to_binary(lists:reverse(OG))),
    BB = base64:encode(list_to_binary(lists:reverse(OB))),
    BA = base64:encode(list_to_binary(lists:reverse(OA))),
    [{0,#amtexmap{type=grayscale,d=1,b64=BR}},
     {1,#amtexmap{type=grayscale,d=1,b64=BG}},
     {2,#amtexmap{type=grayscale,d=1,b64=BB}},
     {3,#amtexmap{type=grayscale,d=1,b64=BA}}].



-ifdef(TEST).
t() ->
    open_amf_file(mm, "test.amf").
-endif().

