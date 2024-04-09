%%
%%  wpc_3mf.erl --
%%
%%     3D Manufacturing File (.3mf) import/export.
%%
%%  Copyright (c) 2024 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_3mf).

-export([init/0,menu/2,command/2]).
-export([t/0,t2/0,t_o/0]).

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

command({file,{import,{tmf_model,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{tmf_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{tmf_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.


menu_entry(Menu) ->
    Menu ++ [{?__(1,"3D Manufacturing (.3mf)..."),tmf_model,[option]}].

props() ->
    [{ext,".3mf"},{ext_desc,?__(1,"3D Manufacturing File")}].

units() ->
    units(true).
units(W) ->
    [{?__(1,"Meters"),meter}] ++
    if W ->
        [{?__(2,"Decimeters"),dm}];
        true -> []
    end ++
    [{?__(3,"Centimeters"),cm},
     {?__(4,"Millimeters"),mm},
     {?__(5,"Microns"),micron}] ++
    if W ->
        [{?__(6,"Yards"),yd}];
        true -> []
    end ++
    [{?__(7,"Feet"),ft},
     {?__(8,"Inches"),in}].

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
    wpa:dialog(Ask, ?__(1,"3D Manufacturing Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{tmf_model,Res}}}
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
        case open_3mf_file(ModelUnits, Filename) of
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
    [?__(1,
     "<h3>Preparation</h3>"
     "If an object has multiple materials the object should be cut apart \n"
     "by material (use <i>Loop Cut</i> tool).\n\n"
     "<h3>Units</h3>"
     "The <i>model units</i> specifies what the Wings3D unit (WU) is "
     "equivalent to. The <i>Convert in 3MF</i> specifies what unit "
     "the exported 3MF file should be in\n\n"
     "The reason is you may have an object modeled with the units in "
     "decimeters, but you want to export the file in millimeters, "
     "possibly because a specific unit is required in the 3MF delivered. "
     "In this case the object will be rescaled automatically to fit "
     "millimeters (x100). 3MF allows 5 units (meter, centimeter, "
     "millimeter, micron, ft, in), "
     "when <i>same as above</i> is used, the nearest supported unit is "
     "chosen.\n\n")].

info_button() ->
    Title = ?__(1,"3MF Export Information"),
    {help, Title, fun () -> more_info() end}.


do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    ModelUnit = wpa:pref_get(?MODULE, model_units, cm),
    TMFUnit   = wpa:pref_get(?MODULE, tmf_units, mm),
    
    Dialog = [
        {label_column, [
            {?__(3,"Model units (for Wings3D Unit):"),
                {menu, units(), ModelUnit, [{key,model_units}]} },
            {?__(4,"Convert in 3MF to:"),
                {menu, [{"Same as above",same}] ++ units(false),
                TMFUnit, [{key,tmf_units}]} }
        ]}
    ] ++ common_mesh_options(export) ++ [
        {hframe,[info_button()]}
    ],
    
    wpa:dialog(Ask, ?__(1,"3D Manufacturing (.3mf) Export Options"), Dialog,
           fun(Res) ->
               {file,{Op,{tmf_model,Res}}}
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
        TMFUnits = proplists:get_value(tmf_units, Attr, mm),
        
        wr_3mf(Filename, units_tuple(ModelUnits, TMFUnits), E3DFile)
    end.

units_tuple(dm, same) ->
    units_tuple(dm, cm);
units_tuple(yd, same) ->
    units_tuple(yd, ft);
units_tuple(ModelUnits, same)
  when ModelUnits =:= meter;
       ModelUnits =:= cm;
       ModelUnits =:= mm;
       ModelUnits =:= micron;
       ModelUnits =:= ft;
       ModelUnits =:= in ->
    units_tuple(ModelUnits, ModelUnits);
units_tuple(ModelUnits, TMFUnits) ->
    {ModelUnits, TMFUnits}.


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
%% Write 3MF
%%

%% An object with just a material
-record(notexprop, {
    obj,
    mtl
}).

%% An object with a material and a texture
-record(texprop, {
    obj,
    mtl,
    t2dg,
    mprop,
    tex
}).


wr_3mf(Filename, Units, E3DFile) ->
    Name = filename:basename(filename:rootname(Filename)),
    Ret = try
        wr_3mf_1(Name, Units, E3DFile)
    catch _:one_material_per_volume ->
        {error, {one_material_per_volume, any}}
    end,
    case Ret of
        {Ret_1, TexImgs} when is_list(Ret_1) ->
            TMFContents = iolist_to_binary(Ret_1),
            ok = wr_zip(Filename, TMFContents, TexImgs),
            ok;
        {error, {one_material_per_volume, A}} ->
            {error,
                "Only one material per volume allowed, use the "
                "\"loop cut\" tool to split objects and apply "
                "one material to each whole object afterwards." ++
                case A of
                    any -> 
                        "";
                    "" -> 
                        "";
                    ObjName when is_list(ObjName) ->
                        lists:flatten(io_lib:format(
                            "~n~n'~s' must be single material.",
                            [ObjName]))
                end }
    end.

wr_3mf_1(_Name, {_WU, TMFUnit}=Units, #e3d_file{objs=Objs_0,mat=Mats,creator=Creator}) ->
    case close_objs_single_mat(Objs_0) of
        {ok, Objs_1} ->
            %% Combine objects with same name
            ScaleF = scale_from_units(export, Units),
            Objs_2 = scale_objects(Objs_1, ScaleF),
            Objs_3 = Objs_2,
            {MtlIndices, TexImgs, IdxCount} = index_mtls(Mats),
            Objs = number_objs(Objs_3, MtlIndices, IdxCount),
            Xml=[
                <<"<?xml version='1.0' encoding='utf-8'?>\n">>,
                to_bin(io_lib:format(
                        "<!-- ~s -->\n" ++
                        "<model unit=\"~s\" xml:lang=\"en-US\" " ++
                        "xmlns=\"http://schemas.microsoft.com/3dmanufacturing/core/2015/02\" " ++
                        "xmlns:m=\"http://schemas.microsoft.com/3dmanufacturing/material/2015/02\">\n",
                        [Creator, to_3mf_unit(TMFUnit)])),
                <<"<resources>\n">>,
                    [ wr_3mf_mat(Mt, MtlIndices)
                      || Mt <- Mats],
                    [ wr_3mf_obj(Obj, MtlIndices, ObjNum)
                      || {Obj, ObjNum} <- Objs ],
                <<"</resources>\n">>,
                <<"<build>\n">>,
                    [ to_bin(io_lib:format("<item objectid=\"~p\"/>\n", [obj_num_only(ObjNum)]))
                      || {_, ObjNum} <- Objs ],
                <<"</build>\n">>,
                <<"</model>\n">>
            ],
            {Xml, TexImgs};
        {error, Err} ->
            {error, Err}
    end.

obj_num_only(#notexprop{obj=ObjNum})
  when is_integer(ObjNum) ->
    ObjNum;
obj_num_only(#texprop{obj=ObjNum})
  when is_integer(ObjNum) ->
    ObjNum.
    

wr_zip(Filename, TMFContents, TexImgs) ->
    Options = [],
    Rels = [
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
        "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
        "<Relationship Type=\"http://schemas.microsoft.com/3dmanufacturing/2013/01/3dmodel\" Target=\"/3D/3dmodel.model\" Id=\"rel0\" />",
        "</Relationships>"],
    ContentTypes = 
        ["<?xml version=\"1.0\" encoding=\"utf-8\"?>",
         "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">",
         "<Default Extension=\"jpeg\" ContentType=\"image/jpeg\" />",
         "<Default Extension=\"jpg\" ContentType=\"image/jpeg\" />",
         "<Default Extension=\"model\" ContentType=\"application/vnd.ms-package.3dmanufacturing-3dmodel+xml\" />",
         "<Default Extension=\"png\" ContentType=\"image/png\" />",
         "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\" />",
         "<Default Extension=\"texture\" ContentType=\"application/vnd.ms-package.3dmanufacturing-3dmodeltexture\" />",
         "</Types>"],
    {ok, _} = zip:zip(Filename, [
        {"3D/3dmodel.model", to_bin(TMFContents)},
        {"_rels/.rels", to_bin(Rels)},
        {"[Content_Types].xml", to_bin(ContentTypes)}
    ] ++ TexImgs, Options),
    ok.


%% 3MF Write Objects
-record(tmfwrobj, {
    name = [],
    vs = [],
    vc = [],
    tx = [],
    vl = []
}).
-record(tmfwrvl, {
    name = [],
    mat = none,
    fs = []
}).


%% Assign a number for each object
%%
number_objs(Objs, MtlIndices, Num)
  when is_integer(Num) ->
    number_objs(Objs, MtlIndices, Num, []).
number_objs([O|Objs], MtlIndices, Num, OL) ->
    {O_1, Mat} = number_objs_1(O),
    case gb_trees:get(Mat, MtlIndices) of
        {mat, MtlId, none} ->
            ObjId = Num+1,
            number_objs(Objs, MtlIndices, Num+1,
                [{O_1,#notexprop{obj=ObjId,mtl=MtlId}}|OL]);
        {mat, MtlId, {TexId}} ->
            ObjId = Num+1,
            T2DGId = Num+2,
            MPId = Num+3,
            number_objs(Objs, MtlIndices, Num+3,
                [{O_1,#texprop{obj=ObjId,mtl=MtlId,t2dg=T2DGId,mprop=MPId,tex=TexId}}|OL])
    end;
number_objs([], _MtlIndices, _Num, OL) ->
    lists:reverse(OL).
number_objs_1(#e3d_object{name=Name,obj=#e3d_mesh{vs=Vs,vc=Vc,tx=Tx,fs=EFs}=_}=_) ->
    [#e3d_face{mat=[Mat]}|_]=EFs,
    VL = #tmfwrvl{name=Name,mat=Mat,fs=EFs},
    {#tmfwrobj{name=Name,vs=Vs,vc=Vc,tx=Tx,vl=[VL]}, Mat}.




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


wr_3mf_obj(#tmfwrobj{name=ObjName,vs=Vs,vc=_Vc,tx=VTx,vl=Volumes}, MtlIndices, ObjNum) ->
    [#tmfwrvl{mat=MatName}|_]=Volumes,
    case ObjNum of
        #texprop{obj=ObjId,t2dg=T2DGId,mprop=MPId}
          when is_integer(ObjId),
               is_integer(T2DGId),
               is_integer(MPId) ->
            {mat, MtlId, {Tex2DId}} = gb_trees:get(MatName, MtlIndices),
            [
                wr_3mf_tex2dg(T2DGId, Tex2DId, VTx),
                wr_3mf_multiprop(MPId, {MtlId, T2DGId}, VTx),
                to_bin(io_lib:format(
                    "<object id=\"~w\" name=\"~s\" type=\"model\">\n",
                    [ObjId, wr_esc(ObjName)]))
            ];
        #notexprop{obj=ObjId} ->
            {mat, MtlId, none} = gb_trees:get(MatName, MtlIndices),
            [
                to_bin(io_lib:format(
                    "<object id=\"~w\" name=\"~s\" type=\"model\" pid=\"~w\" pindex=\"0\">\n",
                    [ObjId, wr_esc(ObjName), MtlId]))
            ]
    end ++
    [
            <<"<mesh>\n">>,
                <<"<vertices>\n">>,
                [
                    [
                        <<"<vertex ">>,
                        <<"x=\"">>, flt_to_bin(X), <<"\" ">>,
                        <<"y=\"">>, flt_to_bin(Y), <<"\" ">>,
                        <<"z=\"">>, flt_to_bin(Z), <<"\"/>\n">>
                    ]
                || {X,Y,Z} <- Vs ],
                <<"</vertices>\n">>,
                [ wr_3mf_obj_vlm(Tup, ObjNum, MtlIndices) || Tup <- Volumes ],
            <<"</mesh>\n">>,
        <<"</object>\n">>
    ].


wr_3mf_tex2dg(T2DGId, Tex2DId, VTx) ->
    [
        to_bin(io_lib:format(
            "<m:texture2dgroup id=\"~w\" texid=\"~w\">\n",
            [T2DGId, Tex2DId])),
          [ to_bin(io_lib:format("<m:tex2coord u=\"~f\" v=\"~f\" />\n", [U, V]))
            || {U,V} <- VTx],
        <<"</m:texture2dgroup>\n">>
    ].

wr_3mf_multiprop(MPId, {MtlId, T2DGId}, VTx) ->
    [
        to_bin(io_lib:format(
            "<m:multiproperties id=\"~w\" pids=\"~w ~w\">\n",
            [MPId, MtlId, T2DGId])),
          [ to_bin(io_lib:format("<m:multi pindices=\"~w ~w\" />\n", [0, Idx]))
            || Idx <- lists:seq(0, length(VTx)-1)],
        <<"</m:multiproperties>\n">>
    ].


wr_esc(A_0) ->
    lists:foldl(
        fun({From, To}, A) ->
            lists:flatten(string:replace(A, From, To, all))
        end,
        A_0,
        [{"&","&amp;"},{"<", "&lt;"},{">", "&gt;"},{"\"","&dquot;"}]).


wr_3mf_obj_vlm(#tmfwrvl{name=_Name,mat=MatName,fs=Efs}, ObjNum, MtlIndices) ->
    case gb_trees:lookup(MatName, MtlIndices) of
        {value, {mat, MtlIdx, TexUsed}} ->
            [
                to_bin(io_lib:format("<triangles>\n", [])),
                [ wr_3mf_obj_tri(Tri, MtlIdx, TriTx, ObjNum, TexUsed) || #e3d_face{vs=Tri,tx=TriTx} <- Efs ],
                <<"</triangles>\n">>
            ]
    end.


wr_3mf_obj_tri([V1,V2,V3], MatIdx, TexTri, ObjNum, TexTup) ->
    [
        <<"<triangle ">>,
        <<"v1=\"">>, int_to_bin(V1), <<"\" ">>,
        <<"v2=\"">>, int_to_bin(V2), <<"\" ">>,
        <<"v3=\"">>, int_to_bin(V3), <<"\" ">>,
        wr_texmap(MatIdx, TexTri, ObjNum, TexTup),
        <<"/>\n">>
    ].


in_zip_image_name(Tex2DId) ->
    lists:flatten(io_lib:format("3D/Texture/~w.png", [Tex2DId])).


index_mtls(Mats) ->
    index_mtls(Mats, 0, gb_trees:empty(), []).
index_mtls([{MatName, Prop}=_|Mats], Count1, MI, TexImgs) ->
    Maps = proplists:get_value(maps, Prop, []),
    MtlId = Count1+1,
    case proplists:get_value(diffuse, Maps, none) of
        none ->
            MI_1 = gb_trees:insert(
                MatName, {mat, MtlId, none}, MI),
            index_mtls(Mats, Count1+1, MI_1, TexImgs);
        #e3d_image{}=Img ->
            Tex2DId = Count1+2,
            MI_1 = gb_trees:insert(
                MatName, {mat, MtlId, {Tex2DId}}, MI),
            {ok, Bin} = e3d__png:save_bin(Img),
            FileName = in_zip_image_name(Tex2DId),
            TexImgs_1 = [{FileName, Bin}|TexImgs],
            index_mtls(Mats, Count1+2, MI_1, TexImgs_1) % TODO: Add png file binary
    end;
index_mtls([], Count, MI, TexImgs) ->
    {MI, TexImgs, Count}.


wr_3mf_mat_base(MtlId, MatProp) ->
    OpenGL = proplists:get_value(opengl, MatProp, []),
    {R,G,B,_} = proplists:get_value(diffuse, OpenGL, {0.8,0.8,0.8,1.0}),
    [
        to_bin(io_lib:format("<basematerials id=\"~w\">~n", [MtlId])),
        to_bin(io_lib:format("<base name=\"~s\" displaycolor=\"#~s\"/>~n",
            [wr_esc("BaseMaterial"), wr_hex({R,G,B})])),
        <<"</basematerials>\n">>
    ].

wr_3mf_mat_tex2d(Tex2DId, MatProp) ->
    Maps = proplists:get_value(maps, MatProp, []),
    case proplists:get_value(diffuse, Maps, []) of
        #e3d_image{}=_Img ->
            Path = in_zip_image_name(Tex2DId),
            ContentType = "image/png",
            TSU = "clamp",
            TSV = "clamp",
            [
                to_bin(io_lib:format("<m:texture2d id=\"~w\" path=\"/~s\" ", [Tex2DId, wr_esc(Path)])),
                to_bin(io_lib:format(" contenttype=\"~s\" tilestyleu=\"~s\" tilestylev=\"~s\"/>~n",
                    [ContentType, TSU, TSV]))
            ]
    end.


wr_3mf_mat({MatName, MatProp}, MtlIndices) ->
    case gb_trees:get(MatName, MtlIndices) of
        {mat, MtlId, none} ->
            [
                wr_3mf_mat_base(MtlId, MatProp)
            ];
        {mat, MtlId, {Tex2DId}} ->
            [
                wr_3mf_mat_base(MtlId, MatProp),
                wr_3mf_mat_tex2d(Tex2DId, MatProp)
            ]
    end.


wr_texmap(MtlId, _, _, none) ->
    [
        <<"pid=\"">>, int_to_bin(MtlId), <<"\" ">>
    ];
wr_texmap(_MtlId, [TxI1,TxI2,TxI3], #texprop{mprop=MPId}, {_TexId})
  when is_integer(TxI1),
       is_integer(TxI2),
       is_integer(TxI3) ->
    P1 = TxI1,
    P2 = TxI2,
    P3 = TxI3,
    [
        <<"pid=\"">>, int_to_bin(MPId), <<"\" ">>,
        <<"p1=\"">>, int_to_bin(P1), <<"\" ">>,
        <<"p2=\"">>, int_to_bin(P2), <<"\" ">>,
        <<"p3=\"">>, int_to_bin(P3), <<"\" ">>
    ];
wr_texmap(_MtlId, _, _, _) ->
    [].


int_to_bin(A) ->
    integer_to_binary(A).

flt_to_bin(A) ->
    float_to_binary(A, [{decimals,10},compact]).

to_bin(A) ->
    iolist_to_binary(A).
    

wr_hex({R,G,B})
  when is_float(R), is_float(G), is_float(B) ->
    R_1 = floor(R * 255),
    G_1 = floor(G * 255),
    B_1 = floor(B * 255),
    int_hex2(R_1) ++ int_hex2(G_1) ++ int_hex2(B_1).

int_hex2(Num) ->
    [int_hex2_1(Num band 16#F0 bsr 4), int_hex2_1(Num band 16#F)].
int_hex2_1(N) when N >= 0  andalso N =< 9  -> $0 + N;
int_hex2_1(N) when N >= 10 andalso N =< 15 -> $A + (N - 10).


rd_hex([$#|Col]) ->
    rd_hex(Col);
rd_hex([R0,R1,G0,G1,B0,B1|_]) ->
    { hex2_int(R0,R1) / 255.0 ,
      hex2_int(G0,G1) / 255.0 ,
      hex2_int(B0,B1) / 255.0 }.

hex2_int(H1,H2) ->
    (hex2_int_1(H1) bsl 4) bor
    hex2_int_1(H2).
hex2_int_1(C) when C >= $0 andalso C =< $9 -> C - $0;
hex2_int_1(C) when C >= $A andalso C =< $F -> C - $A + 10;
hex2_int_1(C) when C >= $a andalso C =< $f -> C - $A + 10.


to_3mf_unit(Unit) ->
    case Unit of
        cm -> "centimeter";
        mm -> "millimeter";
        in -> "inch";
        ft -> "foot";
        meter -> "meter";
        micron -> "micron"
    end.


%%
%% Read 3MF
%%

open_3mf_file(ModelUnit, Filename) ->
    {ok, Cont} = file:read_file(Filename),
    case Cont of
        <<"PK",_/binary>> ->
            case open_3mf_container(Filename) of
                {ok, {XmlCont, TexBins}} ->
                    read_3mf_content(ModelUnit, XmlCont, TexBins);
                {error,bad_central_directory} ->
                    %% zip:zip_open isn't able to handle this file,
                    %% we'll try our own unzipper.
                    open_3mf_file_lenient(ModelUnit, Filename)
            end;
        <<"<",_/binary>> ->
            %% Normally 3MF should not be outside a zip file
            io:format("~p: INFO: 3MF content outside zip~n", [?MODULE]),
            read_3mf_content(ModelUnit, Cont, [])
    end.

open_3mf_file_lenient(ModelUnit, Filename) ->
    case zip_lenient(Filename) of
        {ok, Files} ->
            {ok, Cont} = orddict:find(<<"3D/3dmodel.model">>, Files),
            TexBins = orddict_tex_list(Files),
            read_3mf_content(ModelUnit, Cont, TexBins);
        error ->
            {error,?__(1,
                "This file is missing a central directory, " ++
                 "please try these steps:\n" ++
                 "1. Rename the 3mf to zip\n2. Unzip\n" ++
                 "3. Re-zip contents\n4. Rename back to 3mf\n" ++
                 "5. Try importing again.")}
    end.

orddict_tex_list(Files) ->
    L1 = [
            case filename:extension(Name) of
                ".png" -> F;
                _ -> false
            end || {Name, _}=F <- Files],
    [A || A <- L1, A =/= false].

open_3mf_container(Filename) ->
    case zip:zip_open(Filename, [memory]) of
        {ok, ZH} ->
            {ok, FileList} = zip:zip_list_dir(ZH),
            {[{_,XmlCont}|_], ImgList} = open_3mf_get_files(ZH, FileList),
            zip:zip_close(ZH),
            {ok, {XmlCont, ImgList}};
        {error,bad_central_directory} ->
            %% Some software produce zips with a missing central
            %% directory?
            {error,bad_central_directory};
        Error ->
            Error
    end.
    
open_3mf_get_files(ZH, FileList) ->
    open_3mf_get_files(ZH, FileList, {[],[]}).

open_3mf_get_files(ZH, [#zip_file{name=Name}=_File|FileList], {XmlList, ImgList}) ->
    Ext = string:to_lower(filename:extension(Name)),
    {XmlList_1, ImgList_1} = case Ext of
        ".model" ->
            {ok, Res} = zip:zip_get(Name, ZH),
            {[Res | XmlList], ImgList};
        ".png" ->
            {ok, Res} = zip:zip_get(Name, ZH),
            {XmlList, [Res | ImgList]};
        _ ->
            {XmlList, ImgList}
    end,
    open_3mf_get_files(ZH, FileList, {XmlList_1, ImgList_1});
open_3mf_get_files(ZH, [_OtherRec|FileList], FileLists) ->
    open_3mf_get_files(ZH, FileList, FileLists);
open_3mf_get_files(_ZH, [], {XmlList, XmlListI}) ->
    {XmlList, XmlListI}.

%%
%% To fix the central directory issue:
%% Some programs specify a central directory offset of FF FF FF FF,
%% which wings zip cannot handle, change to actual offset.
%% 
%%
%%

%%%
%%%

-record(tmvrt, {
    x,
    y,
    z
}).

-record(tmtri, {
    v1,
    v2,
    v3,
    pid=none,
    p1=none,
    p2=none,
    p3=none
}).

-record(tmcol, {
    r=0.0,
    g=0.0,
    b=0.0,
    a=1.0
}).

-record(tmvlm, {
    name = "",
    tl
}).

-record(tmobj, {
    name = "",
    id,
    vs,
    vl,
    col,
    pid,
    pindex
}).

-record(tmmtl, {
    id,
    dispid,
    l = []
}).

-record(tmtex, {
    id,
    path,
    ctype,
    tsu,
    tsv
}).


%% State file for xmerl sax.
-record(tmftk, {
    unit = mm,
    mtls = [],
    objs = [],
    texs = [],
    
    obj_id = 0,
    obj_pid = none,
    obj_pindex = none,

    mesh = [],
    verts = [],
    vt,
    volumes = [],
    tl = [],
    tri_at,
    mtl_base_at = none,
    mtl_base_l = [],
    mtl_at = [],
    mtl_mtl_id = none,
    
    metadat = [],
    mdat_type,

    dispprop = [],
    dispprop_at = none,
    
    tex2dgrp = [],
    tex2dgrp_at = none,
    tex2coord_at = none,
    
    multiprops = [],
    multiprop_at = none,
    multi_at = none
    }).

-record(tmmulti, {
    l = none
}).

-record(tmtex2dgrp, {
    id,
    texid,
    l = []
}).

-record(tmmultiprop, {
    id,
    pids,
    pids_t,
    l = [],
    lb
}).

-record(tmdispprop, {
    id,
    attr = []
}).


read_3mf_content(ModelUnit, Bin, TexImgs) ->
    read_3mf_content_1(ModelUnit, Bin, TexImgs).
read_3mf_content_1(ModelUnit, Bin_1, TexImgs) ->
    EF = {event_fun, fun tmf_tok/3},
    ES = {event_state, #tmftk{}},
    case xmerl_sax_parser:stream(Bin_1, [EF,ES]) of
        {ok, #tmftk{unit=TMFUnit,objs=Objs_0}=TM=_Es, _} ->
            Mats = propmtls_by_id(TM),
            Objs = guess_vlms(Objs_0),
            Objs_1 = make_e3dobj(Objs, Mats),
            Mats_1 = make_e3dmat(Mats, TexImgs),
            ScaleF = scale_from_units(import, units_tuple(ModelUnit, TMFUnit)),
            Objs_2 = scale_objects(Objs_1, ScaleF),
            Mats_2 = fill_missing_materials(Mats_1, Objs_1),
            {ok, {Objs_2, Mats_2}};
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("3mf:~p: ERROR: ~p:~p~n", [Line, Error, Reason]),
            {error, "unknown/unhandled format, see log window"}
    end.

guess_vlms(Objs) ->
    guess_vlms(Objs, []).
guess_vlms([#tmobj{vl=[#tmvlm{tl=Tri}=Vlm0]}=Obj_0|R], OL) ->
    %% There does not seem to be a standardized way to determine manifold 
    %% volumes of an object, different software use their own software
    %% specific tag to differentiate triangles going to different volumes
    %% while keeping all the triangles in the same object.
    %%
    %% If we just let in the object without splitting it into volumes, wings
    %% may create a broken mesh, so we'll try to guess the volumes in the
    %% object first.
    %%
    %% Auto-detect multi-volume mesh by determining sets of vertices
    %% only being used for regions of triangles.
    %%
    Vlm_1 = case disjoint_vs([{V1,V2,V3} || #tmtri{v1=V1,v2=V2,v3=V3} <- Tri]) of
        [_,_|_]=L ->
            divide_vlm(Vlm0, Tri, L);
        [_] ->
            [Vlm0]
    end,
    guess_vlms(R, [Obj_0#tmobj{vl=Vlm_1}|OL]);
guess_vlms([Obj_0|R], OL) ->
    guess_vlms(R, [Obj_0|OL]);
guess_vlms([], OL) ->
    lists:reverse(OL).

%% Determine disjoint vertice indices from the
%% triangles, sometimes the triangles of separate volumes
%% don't share the same vertices.
%%
disjoint_vs(Tri) ->
    G = disjoint_vs(Tri, gb_trees:empty()),
    disjoint_vs_list(Tri, G).
disjoint_vs([{V1,V2,V3}|Tri], G) ->
    Min = min(V1, min(V2, V3)),
    G2 = lists:foldl(
        fun(V, G1) ->
            disjoint_vs_enter_small(V, Min, G1)
        end, G, [V1,V2,V3]),
    disjoint_vs(Tri, G2);
disjoint_vs([], G) ->
    disjoint_vs_2(gb_trees:to_list(G), G).
disjoint_vs_enter_small(V, New, G) ->
    case gb_trees:lookup(V, G) of
        none ->
            gb_trees:insert(V, New, G);
        {value, Old} when Old > New ->
            gb_trees:enter(V, New, G);
        _ ->
            G
    end.
disjoint_vs_2([{V,Min}|L], G) ->
    Min_1 = disjoint_vs_min(Min, G),
    disjoint_vs_2(L, gb_trees:enter(V,Min_1, G));
disjoint_vs_2([], G) ->
    G.
disjoint_vs_min(V, G) ->
    case gb_trees:lookup(V, G) of
        {value, Min2} when Min2 =/= V ->
            disjoint_vs_min(Min2, G);
        {value, Min2} when Min2 =:= V ->
            Min2
    end.
disjoint_vs_list(L, G) ->
    [Min|L_1] = [gb_trees:get(V1,G) || {V1,_,_} <- L],
    disjoint_vs_list(L_1, Min, 1, []).
disjoint_vs_list([Min0|L], Min, Count, OL)
  when Min0 =/= Min ->
    disjoint_vs_list(L, Min0, 1, [Count|OL]);
disjoint_vs_list([Min0|L], Min, Count, OL)
  when Min0 =:= Min ->
    disjoint_vs_list(L, Min, Count+1, OL);
disjoint_vs_list([], _Min, Count, OL) ->
    lists:reverse([Count|OL]).

%% Divide triangles into volumes by a list of list lengths
%%
divide_vlm(Vlm0, Tri, L) ->
    divide_vlm(Vlm0, Tri, L, []).
divide_vlm(Vlm0, Tri, [Num|L], OL) ->
    {NewTri, Tri_1} = lists:split(Num, Tri),
    Name = lists:flatten(io_lib:format("v~p", [length(L)])),
    divide_vlm(Vlm0, Tri_1, L, [Vlm0#tmvlm{name=Name,tl=NewTri}|OL]);
divide_vlm(_, [], [], OL) ->
    lists:reverse(OL).


make_e3dobj(Objs, Mats) ->
    make_e3dobj(Objs, Mats, []).
make_e3dobj([#tmobj{name=ObjName,id=_Num,vs=Vs,vl=Vlm,col=_Col,pid=PId,pindex=PIndex}|R], Mats, OL) ->
    %% Each object has one volume in 3MF.
    {VVs, VVc} = make_e3dobj_vs(Vs),
    {VF, UVL_1} = make_e3dobj_vlm(Vlm, Mats, {PId,PIndex}, gb_trees:empty()),
    VTx = get_list_from_vl(UVL_1),
    Obj1 = [#e3d_object{
        name=make_e3dobj_name(ObjName, VlmName),
        obj=#e3d_mesh{
            type=triangle,
            vs=VVs, vc=VVc, tx=VTx,
             fs=Efs
        }
    } || {VlmName, Efs} <- VF],
    make_e3dobj(R, Mats, [Obj1|OL]);
make_e3dobj([], _, OL) ->
    lists:append(lists:reverse(OL)).


make_e3dobj_name(ObjName, VlmName)
  when is_list(ObjName), length(ObjName) > 0,
       is_list(VlmName), length(VlmName) > 0 ->
    ObjName ++ "__" ++ VlmName;
make_e3dobj_name(ObjName, "")
  when is_list(ObjName), length(ObjName) > 0 ->
    ObjName.


make_e3dobj_vs(Vs) ->
    make_e3dobj_vs(Vs, []).
make_e3dobj_vs([#tmvrt{x=X,y=Y,z=Z}=_|R], OL) ->
    V = {X,Y,Z},
    make_e3dobj_vs(R, [V|OL]);
make_e3dobj_vs([], OL) ->
    {lists:reverse(OL), []}.


make_e3dobj_vlm(Vlm, Mats, {PId,PIndex}, UVL) ->
    DMat = case proplists:get_value(PId, Mats, none) of
        none ->
            default;
        _ when is_integer(PIndex); PIndex =:= none ->
            mat_name_atom(PId, PIndex)
    end,
    make_e3dobj_vlm(Vlm, Mats, DMat, [], UVL).
make_e3dobj_vlm([#tmvlm{name=Name,tl=TL}=_|R], Mats, DMat, OL, UVL_0) ->
    {Vl_1,UVL_1} = make_e3dfs(TL, Mats, DMat, UVL_0),
    make_e3dobj_vlm(R, Mats, DMat, [{Name, Vl_1}|OL], UVL_1);
make_e3dobj_vlm([], _, _DMat, OL, UVL) ->
    {lists:reverse(OL), UVL}.


make_e3dfs(Vlm, Mats, DMat, UVL) ->
    make_e3dfs(Vlm, Mats, DMat, [], UVL).
make_e3dfs([#tmtri{v1=V1,v2=V2,v3=V3}=AmTri|R], Mats, DMat, OL, UVL_0) ->
    {Mat, TxL, UVL_1} = make_e3dfs_uv(AmTri, Mats, DMat, UVL_0),
    Vl_1 = #e3d_face{
        vs=[V1,V2,V3],
        vc=[],
        tx=TxL,
        mat=[Mat]
    },
    make_e3dfs(R, Mats, DMat, [Vl_1|OL], UVL_1);
make_e3dfs([], _, _DMat, OL, UVL) ->
    {lists:reverse(OL), UVL}.


make_e3dfs_uv(#tmtri{pid=PId,p1=P1,p2=P2,p3=P3}=_, Mats, DMat, UVL_0)
  when is_integer(PId),
       is_integer(P1), is_integer(P2), is_integer(P3) ->
    case proplists:get_value(PId, Mats, none) of
        none ->
            {DMat, [], UVL_0};
        MPL ->
            case make_e3dfs_uv_1(MPL) of
                none ->
                    {DMat, [], UVL_0};
                { #tmmultiprop{lb=Mp_L},
                  #tmtex2dgrp{l=T2D_L} } ->
                    {Mat1_N, UV1} = lists:nth(P1+1, Mp_L),
                    {UTx1,VTx1} = lists:nth(UV1+1, T2D_L),
                    {_Mat2_N, UV2} = lists:nth(P2+1, Mp_L),
                    {UTx2,VTx2} = lists:nth(UV2+1, T2D_L),
                    {_Mat3_N, UV3} = lists:nth(P3+1, Mp_L),
                    {UTx3,VTx3} = lists:nth(UV3+1, T2D_L),

                    Mat1_Atom = mat_name_atom(PId, Mat1_N),
                    {Idx1, UVL_1} = get_next_idx({UTx1,VTx1}, UVL_0),
                    {Idx2, UVL_2} = get_next_idx({UTx2,VTx2}, UVL_1),
                    {Idx3, UVL_3} = get_next_idx({UTx3,VTx3}, UVL_2),
                    {Mat1_Atom, [Idx1,Idx2,Idx3], UVL_3}
            end
    end;
make_e3dfs_uv(_, _, DMat, UVL) ->
    {DMat, [], UVL}.

make_e3dfs_uv_1(MPL) ->
    case make_e3dmat_get(tmmultiprop, MPL) of
        none -> none;
        #tmmultiprop{}=Mp ->
            case make_e3dmat_get(tmtex2dgrp, MPL) of
                none -> none;
                #tmtex2dgrp{}=T2D ->
                    {Mp, T2D}
            end
    end.


get_next_idx(Val, VL) ->
    case gb_trees:lookup(Val, VL) of
        none ->
            Idx = gb_trees:size(VL),
            {Idx, gb_trees:insert(Val, Idx, VL)};
        {value, Idx} ->
            {Idx, VL}
    end.


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


%%%
%%%

make_e3dmat(Mats, TexImgs) ->
    make_e3dmat(Mats, TexImgs, []).

make_e3dmat_get_col(none) ->
    {0.9, 0.9, 0.9, 1.0};
make_e3dmat_get_col({"BaseMaterial", Col}) ->
    make_e3dmat_col(Col).
    
make_e3dmat_get(Name, [Tup|_])
  when element(1, Tup) =:= Name ->
    Tup;
make_e3dmat_get(Name, [_|L]) ->
    make_e3dmat_get(Name, L);
make_e3dmat_get(_Name, []) ->
    none.
make_e3dmat([{Num, MPL}|R], TexImgs, OL) ->
    #tmmtl{l=L1} = make_e3dmat_get(tmmtl, MPL),
    
    Tex = make_e3dmat_tex(MPL, TexImgs),
    
    L2 = case make_e3dmat_get(tmdispprop, MPL) of
        none -> none;
        #tmdispprop{attr=L2_0} ->
            L2_0
    end,
    Mt = [ make_e3dmat_l(A, Num, Tex, L2, MPL)
           || A <- lists:zip(L1, lists:seq(1-1, length(L1)-1))],
    make_e3dmat(R, TexImgs, [Mt|OL]);
make_e3dmat([], _TexImgs, OL) ->
    lists:append(lists:reverse(OL)).

make_e3dmat_l({A, NumCol}, Num, Tex, _L2, _MPL) ->
    {ClrR,ClrG,ClrB,ClrA} = make_e3dmat_get_col(A),
    MatName = mat_name_atom(Num, NumCol),
    Mt = {MatName, simple_mtl(ClrR, ClrG, ClrB, ClrA) ++ Tex},
    Mt.

make_e3dmat_col(#tmcol{r=ClrR,g=ClrG,b=ClrB,a=ClrA}) ->
    {ClrR,ClrG,ClrB,ClrA}.

mat_name_atom(PId, PIndex)
  when is_integer(PId), is_integer(PIndex) ->
    list_to_atom(lists:flatten(
        io_lib:format("mat_~w_~w", [PId, PIndex])));
mat_name_atom(PId, none)
  when is_integer(PId) ->
    list_to_atom(lists:flatten(
        io_lib:format("mat_~w_~w", [PId, 0]))).

%%%
%%%

make_e3dmat_tex(MPL, TexImgs) ->
    case make_e3dmat_get(tmtex, MPL) of
        none ->
            [];
        #tmtex{path=Path,ctype=ContentType,tsu=_TSU,tsv=_TSV} ->
            Path_1 = case Path of
                "/" ++ P -> P;
                "\\" ++ P -> P;
                P -> P
            end,
            case proplists:get_value(Path_1, TexImgs, none) of
                none ->
                    [];
                Bin when is_binary(Bin) ->
                    case get_bitmap(ContentType, Bin) of
                        {ok, #e3d_image{}=Img} ->
                            [{maps, [{diffuse, Img}]}];
                        _ ->
                            []
                    end
            end
    end.

%%%
%%%

get_bitmap(MimeType, Bin) ->
    case string:to_lower(MimeType) of
        "image/png" ->
            get_bitmap_png(MimeType, Bin);
        "image/jpeg" ->
            get_bitmap_jpeg(MimeType, Bin);
        _ ->
            {error, none}
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
    FileId = "w3d_3mf_" ++ integer_to_list(FileIdNum) ++ Ext,
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
            Ext = ".png"
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

%%%
%%%

%% xmerl tokenizer
%%
tmf_tok({startElement, _, LName, _, Attrs}=_Ev, _Loc, #tmftk{}=State) ->
    tmf_tok_s(LName, Attrs, State);
tmf_tok({endElement, _, LName, _}=_Ev, _Loc, #tmftk{}=State) ->
    tmf_tok_e(LName, State);
tmf_tok(_Ev, _Loc, #tmftk{}=State) -> State;
tmf_tok(startDocument, _, State) -> State;
tmf_tok(endDocument, _, State) -> State;
tmf_tok(_Ev, _Loc, State) ->
    State.


tmf_tok_s("model", Attrs, State) ->
    push_metadat(State#tmftk{unit=tmf_tok_get_unit(Attrs)});
tmf_tok_s("object", Attrs, State) ->
    Id=tmf_tok_get_int("id", Attrs),
    tmf_tok_get_str("name", Attrs),
    tmf_tok_get_str("type", Attrs),
    PId=tmf_tok_get_int("pid", Attrs),
    PIndex=tmf_tok_get_int("pindex", Attrs),
    push_metadat(State#tmftk{obj_id=Id,obj_pid=PId,obj_pindex=PIndex,volumes=[]});
tmf_tok_s("mesh", _Attrs, State) ->
    push_metadat(State);
tmf_tok_s("vertices", _Attrs, State) ->
    push_metadat(State#tmftk{verts=[]});
tmf_tok_s("vertex", Attrs, State) ->
    State#tmftk{vt=#tmvrt{
        x=tmf_tok_get_flt("x", Attrs),
        y=tmf_tok_get_flt("y", Attrs),
        z=tmf_tok_get_flt("z", Attrs)
    }};

tmf_tok_s("triangles", _Attrs, State) ->
    push_metadat(State#tmftk{tl=[]});
tmf_tok_s("triangle", Attrs, #tmftk{tl=Tl0}=State) ->
    PId = tmf_tok_get_int("pid", Attrs),
    P1 = tmf_tok_get_int("p1", Attrs),
    P2 = tmf_tok_get_int("p2", Attrs),
    P3 = tmf_tok_get_int("p3", Attrs),
    Tri = #tmtri{
        v1=tmf_tok_get_int("v1", Attrs),
        v2=tmf_tok_get_int("v2", Attrs),
        v3=tmf_tok_get_int("v3", Attrs),
        pid=PId,
        p1=P1,
        p2=P2,
        p3=P3
    },
    State#tmftk{
        tl=[Tri|Tl0]
    };



tmf_tok_s("translucentdisplayproperties", Attrs, State) ->
    State#tmftk{dispprop_at=#tmdispprop{
        id=tmf_tok_get_int("id", Attrs),
        attr=[]
    }};
tmf_tok_s("translucent", Attrs, #tmftk{dispprop_at=At}=State) ->
    DPAttrs =
    case tmf_tok_get_str("name", Attrs) of
        none -> [];
        A1   -> [{name, A1}]
            
    end ++
    case tmf_tok_get_flt_list("attenuation", Attrs) of
        none -> [];
        A2   -> [{attenuation, A2}]
            
    end ++
    case tmf_tok_get_flt_list("refractiveindex", Attrs) of
        none -> [];
        A3   -> [{refractiveindex, A3}]
        
    end ++
    case tmf_tok_get_flt("roughness", Attrs) of
        none -> [];
        A4   -> [{roughness, A4}]
    end,

    State#tmftk{dispprop_at=At#tmdispprop{
        attr=DPAttrs ++ At#tmdispprop.attr
    }};


tmf_tok_s("basematerials", Attrs, State) ->
    Id = tmf_tok_get_int("id", Attrs),
    DPId = tmf_tok_get_int("displaypropertiesid", Attrs),
    push_metadat(State#tmftk{
        mtl_base_at={Id, DPId},
        mtl_base_l=[]
    });
tmf_tok_s("base", Attrs, #tmftk{mtl_base_l=MtlBase}=State) ->
    Name = tmf_tok_get_str("name", Attrs),
    {ColR, ColG, ColB} = rd_hex(tmf_tok_get_str("displaycolor", Attrs)),
    State#tmftk{mtl_base_l=[{Name, #tmcol{
        r=ColR,
        g=ColG,
        b=ColB,
        a=1.0
    }}|MtlBase]};

tmf_tok_s("texture2d", Attrs, #tmftk{texs=Texs0}=State) -> %% Namespaced
    Id = tmf_tok_get_int("id", Attrs),
    Tex = #tmtex{
        id=Id,
        path=tmf_tok_get_str("path", Attrs),
        ctype=tmf_tok_get_str("contenttype", Attrs),
        tsu=tmf_tok_get_str("tilestyleu", Attrs),
        tsv=tmf_tok_get_str("tilestylev", Attrs)
    },
    clear_char(State#tmftk{texs=[{Id, Tex}|Texs0]});
    
tmf_tok_s("texture2dgroup", Attrs, State) -> %% Namespaced
    State#tmftk{tex2dgrp_at=#tmtex2dgrp{
        id=tmf_tok_get_int("id", Attrs),
        texid=tmf_tok_get_int("texid", Attrs)
    }};
tmf_tok_s("tex2coord", Attrs, State) -> %% Namespaced
    State#tmftk{tex2coord_at={
        tmf_tok_get_flt("u", Attrs),
        tmf_tok_get_flt("v", Attrs)
    }};

tmf_tok_s("multiproperties", Attrs, State) -> %% Namespaced
    State#tmftk{multiprop_at=#tmmultiprop{
        id=tmf_tok_get_int("id", Attrs),
        pids=tmf_tok_get_int_list("pids", Attrs)
    }};
tmf_tok_s("multi", Attrs, State) -> %% Namespaced
    State#tmftk{multi_at=#tmmulti{
        l=tmf_tok_get_int_list("pindices", Attrs)
    }};

    
tmf_tok_s("build", _Attrs, State) ->
    push_metadat(State);
tmf_tok_s("item", Attrs, State) ->
    tmf_tok_get_int("objectid", Attrs),
    push_metadat(State);

tmf_tok_s(_, _, State) ->
    State.


tmf_tok_e("model", State) ->
    pop_metadat(State);
tmf_tok_e("object", #tmftk{verts=Verts,volumes=Volumes0,obj_id=Id,obj_pid=PId,obj_pindex=PIndex,objs=Objs0}=State) ->
    Volumes = lists:reverse(Volumes0),
    Nm = tmf_getname(State, object),
    Obj = #tmobj{name=Nm,id=Id,pid=PId,pindex=PIndex,vs=Verts,vl=Volumes},
    pop_metadat(State#tmftk{objs=[Obj|Objs0]});

tmf_tok_e("mesh", State) ->
    pop_metadat(State);

tmf_tok_e("vertices", #tmftk{verts=Verts}=State) ->
    pop_metadat(State#tmftk{verts=lists:reverse(Verts)});

tmf_tok_e("vertex", #tmftk{verts=Verts,vt=Vt}=State) ->
    State#tmftk{verts=[Vt|Verts]};

tmf_tok_e("triangles", #tmftk{tl=Tl0,volumes=Volumes_0}=State) ->
    Nm = tmf_getname(State, volume),
    Volume = #tmvlm{name=Nm,tl=lists:reverse(Tl0)},
    pop_metadat(State#tmftk{volumes=[Volume|Volumes_0]});

tmf_tok_e("triangle", State) ->
    State;

tmf_tok_e("translucentdisplayproperties", #tmftk{dispprop_at=#tmdispprop{id=Id}=At,dispprop=DispProp}=State) ->
    State#tmftk{
        dispprop=[{Id,At}|DispProp],
        dispprop_at=none};
tmf_tok_e("translucent", State) ->
    State;
    
tmf_tok_e("basematerials", #tmftk{mtl_base_at={Id,DPId},mtl_base_l=Mtls,mtls=Mtls_0}=State) ->
    pop_metadat(State#tmftk{
        mtls=[{Id, #tmmtl{id=Id,dispid=DPId,l=Mtls}}|Mtls_0],
        mtl_base_at=none,
        mtl_base_l=[]});
tmf_tok_e("base", State) ->
    State;

tmf_tok_e("texture2d", State) -> %% Namespaced
    State;

tmf_tok_e("texture2dgroup", #tmftk{tex2dgrp_at=#tmtex2dgrp{id=Id}=A,tex2dgrp=T2DG}=State) -> %% Namespaced
    State#tmftk{tex2dgrp=[{Id,A}|T2DG],tex2dgrp_at=none};
tmf_tok_e("tex2coord", #tmftk{tex2dgrp_at=At,tex2coord_at=T2CAt}=State) -> %% Namespaced
    State#tmftk{
        tex2dgrp_at=At#tmtex2dgrp{l=[T2CAt|At#tmtex2dgrp.l]},
        tex2coord_at=none};

tmf_tok_e("multiproperties", #tmftk{multiprop_at=#tmmultiprop{id=Id}=A,multiprops=MultiProps_0}=State) -> %% Namespaced
    State#tmftk{multiprops=[{Id,A}|MultiProps_0],multiprop_at=none};
tmf_tok_e("multi", #tmftk{multiprop_at=At,multi_at=MultiAt}=State) -> %% Namespaced
    State#tmftk{
        multiprop_at=At#tmmultiprop{l=[MultiAt|At#tmmultiprop.l]},
        multi_at=none};

tmf_tok_e("build", State) ->
    pop_metadat(State);
tmf_tok_e("item", State) ->
    pop_metadat(State);

tmf_tok_e(_, State) ->
    State.

    

clear_char(Stt) -> Stt.
    

push_metadat(#tmftk{metadat=MList}=Stt) ->
    Stt#tmftk{metadat=[[]|MList]}.    

pop_metadat(#tmftk{metadat=[_|MList]}=Stt) ->
    Stt#tmftk{metadat=MList}.

get_metadat(#tmftk{metadat=[A|_]}=_, Key) ->
    case orddict:find(Key, A) of
        {ok, Val} -> Val;
        _ -> none
    end.


%% Places where to find a name for the object or volume:
%% "name" metadata
%% object id number fallback
%% generated number
%%
tmf_getname(State, object) ->
    case get_metadat(State, "name") of
        none ->
            tmf_getname_1(State, "obj");
        ObjName when is_list(ObjName) ->
            ObjName
    end;
tmf_getname(State, volume) ->
    case get_metadat(State, "name") of
        none ->
            tmf_getname_2(State, "volume");
        VolumeName when is_list(VolumeName) ->
            VolumeName
    end.

tmf_getname_1(#tmftk{objs=V,obj_id=ObjId}=_State, Str) ->
    case ObjId of
        none ->
            lists:flatten(io_lib:format("~s_~w", [Str, length(V)+1]));
        Number when is_integer(Number) ->
            lists:flatten(io_lib:format("~s_~w", [Str, Number]))
    end.

tmf_getname_2(#tmftk{volumes=V}=_, Str) ->
    lists:flatten(io_lib:format("~s_~w", [Str, length(V)+1])).


tmf_tok_get_unit(AttrList) ->
    case tmf_tok_get_str("unit", AttrList) of
        none -> mm;
        Str when is_list(Str) ->
            case lc(Str) of
                "centimeter" ++ _ -> mm;
                "millimeter" ++ _ -> mm;
                "inch" ++ _ -> in;
                "foot" ++ _ -> ft;
                "meter" ++ _ -> meter;
                "micron" ++ _ -> micron;
                _ -> mm
            end
    end.


tmf_tok_get_int(AttrName, [{_, _, AttrName, Val}|_]) ->
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
tmf_tok_get_int(AttrName, [_|R]) ->
    tmf_tok_get_int(AttrName, R);
tmf_tok_get_int(_, []) ->
    none.


tmf_tok_get_int_list(AttrName, Attrs) ->
    case tmf_tok_get_str(AttrName, Attrs) of
        none -> none;
        Str ->
            Ints_S = string:tokens(Str, " ,"),
            [tmf_tok_get_int_list_1(A) || A <- Ints_S]
    end.
tmf_tok_get_int_list_1(ValT) ->
    case string:to_integer(ValT) of
        {Num, _} when is_integer(Num) -> Num;
        {error, _} ->
            case string:to_float(ValT) of
                {Num, _} when is_float(Num) -> round(Num);
                {error, _} ->
                    none
            end
    end.


tmf_tok_get_flt(AttrName, [{_, _, AttrName, Val}|_]) ->
    ValT = string:trim(Val),
    case string:to_integer(ValT) of
        {Num, []} when is_integer(Num) -> float(Num);
        _ ->
            case string:to_float(ValT) of
                {Num, _} when is_float(Num) -> Num;
                {error, _} ->
                    none
            end
    end;    
tmf_tok_get_flt(AttrName, [_|R]) ->
    tmf_tok_get_flt(AttrName, R);
tmf_tok_get_flt(_, []) ->
    none.


tmf_tok_get_flt_list(AttrName, Attrs) ->
    case tmf_tok_get_str(AttrName, Attrs) of
        none -> none;
        Str ->
            Flts_S = string:tokens(Str, " ,"),
            [tmf_tok_get_flt_list_1(A) || A <- Flts_S]
    end.
tmf_tok_get_flt_list_1(ValT) ->
    case string:to_integer(ValT) of
        {Num, []} when is_integer(Num) -> float(Num);
        _ ->
            case string:to_float(ValT) of
                {Num, _} when is_float(Num) -> Num;
                {error, _} ->
                    none
            end
    end.
    

tmf_tok_get_str(AttrName, [{_, _, AttrName, Val}|_]) ->
    ValT = string:trim(Val),
    ValT;
tmf_tok_get_str(AttrName, [_|R]) ->
    tmf_tok_get_str(AttrName, R);
tmf_tok_get_str(_, []) ->
    none.


scale_objects(Objs, ScaleF) ->
    [ scale_objects_1(Obj, ScaleF) || Obj <- Objs].
scale_objects_1(#e3d_object{obj=#e3d_mesh{vs=Vs}=Mesh}=Obj, Scl) ->
    Obj#e3d_object{obj=Mesh#e3d_mesh{vs=[{X*Scl,Y*Scl,Z*Scl} || {X,Y,Z} <- Vs]}}.

%%%
%%%

propmtls_by_id(#tmftk{multiprops=MPLs,tex2dgrp=T2GLs,mtls=MLs,texs=TxLs,dispprop=DPLs}) ->
    % MPLs -> (pids) MLs -> (dispid) DPLs
    %      -> (pids) T2GLs -> (texid) TxLs
    BList1 = T2GLs ++ MLs ++ TxLs ++ DPLs,
    BList2 = DPLs,
    L1 = [ propmtls_by_id_1(Id, A, BList1, PL)
           || {Id,#tmmultiprop{pids=PL}=A} <- MPLs],
    L2 = [{Id,[ {name, rd_name_mtl(Id)},A|lists:flatten(prop(P, BList2))]}
           || {Id,#tmmtl{dispid=P}=A} <- MLs],
    lists:flatten(L1 ++ L2).

propmtls_by_id_1(Id, #tmmultiprop{l=Mp_L}=A, BList1, PL) ->
    Pids_T = [prop1(P, BList1) || P <- PL],
    {Id,[ {name, rd_name_tex(Id)},
          A#tmmultiprop{pids_t=Pids_T,lb=rearrange_mp_l(Pids_T, Mp_L)}
          | lists:flatten([prop(P, BList1) || P <- PL])]}.
    
rearrange_mp_l(Pids_T, Mp_L) ->
    Idxs = rearrange_mp_idxs(Pids_T),
    rearrange_mp_l(Idxs, Mp_L, []).
rearrange_mp_l({IdxMtl, IdxT2G}=Idxs, [#tmmulti{l=IL}|Mp_L], OL) ->
    Mtl = lists:nth(IdxMtl, IL),
    UV = lists:nth(IdxT2G, IL),
    rearrange_mp_l(Idxs, Mp_L, [{Mtl,UV}|OL]);
rearrange_mp_l(_, [], OL) ->
    lists:reverse(OL).


rearrange_mp_idxs(Pids_T) ->
    rearrange_mp_idxs(Pids_T, 1, {1, 1}).
rearrange_mp_idxs([mtl|R], LIdx, {_, B}) ->
    rearrange_mp_idxs(R, LIdx+1, {LIdx, B});
rearrange_mp_idxs([tex2dgrp|R], LIdx, {A, _}) ->
    rearrange_mp_idxs(R, LIdx+1, {A, LIdx});
rearrange_mp_idxs([_|R], LIdx, Tup) ->
    rearrange_mp_idxs(R, LIdx+1, Tup);
rearrange_mp_idxs([], _, {_IdxMtl, _IdxT2G}=Tup) ->
    Tup.


prop(Id, BList) ->
    case proplists:get_value(Id, BList, none) of
        none -> [];
        #tmtex2dgrp{texid=TexId}=A ->
            [A|prop(TexId,BList)];
        #tmmtl{dispid=DPId}=A ->
            [A|prop(DPId,BList)];
        #tmtex{}=A ->
            [A];
        #tmdispprop{}=A ->
            [A]
    end.
prop1(Id, BList) ->
    case proplists:get_value(Id, BList, none) of
        none -> none;
        #tmtex2dgrp{} -> tex2dgrp;
        #tmmtl{} -> mtl;
        #tmtex{} -> tex;
        #tmdispprop{} -> dispprop
    end.


rd_name_tex(Id) ->
    list_to_atom(lists:flatten(io_lib:format("tex_~w", [Id]))).

rd_name_mtl(Id) ->
    list_to_atom(lists:flatten(io_lib:format("mtl_~w", [Id]))).





%%%
%%% Unit conversion
%%%

scale_from_units(export, {WU, TMFUnit}) ->
    unit_ratio(WU, TMFUnit);
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


-define(UINT, unsigned-little-integer).

%%%
%%%

%%
%% Lenient unzip
%% -------------
%%
%% Currently some 3D print software emit 3mf files that zip:zip_open 
%% cannot handle because the function relies on the central directory,
%% whereas these zipped files specify the start of the central directory
%% offset at FFFFFFFF.
%% 
%% As well, these zip files do not specify the file size and compressed size
%% information in the file headers and use the zip64 field header exclusively,
%% even if the file is nowhere near large enough to need it.
%%

zip_lenient(ZipName) ->
    {ok, Bin} = file:read_file(ZipName),
    case zip_l_locfh(Bin) of
        {ok, Files} ->
            {ok, Files};
        error ->
            error
    end.

zip_l_locfh(Bin) ->
    zip_l_locfh(Bin, []).
zip_l_locfh(<<"PK",3,4,_:16/?UINT,GPB:16/?UINT,Com:16/?UINT,_:32/?UINT,
              _CRC:32/?UINT,_:32/?UINT,_:32/?UINT,NameLen:16/?UINT,
              XFLen:16/?UINT,R_0/binary>>, OL)
  when Com =:= 8, XFLen >= 20 ->
    FileName = binary:part(R_0, {0, NameLen}),
    R_1 = binary:part(R_0, {NameLen + XFLen, byte_size(R_0) - NameLen - XFLen}),
    case binary:part(R_0, {NameLen, XFLen}) of
        <<
            1:16/?UINT,
            _:16/?UINT,
            _:32/?UINT,
            _:32/?UINT,
            CompSize:32/?UINT,
            _:32/?UINT,
            _/binary
        >> when CompSize > 0 ->
            CompData = binary:part(R_1, {0, CompSize}),
            Ofs1 = if (GPB band 4) > 0 -> 4+4+4;
                      true -> 0
            end,
            Z = zlib:open(),
            ok = zlib:inflateInit(Z, -15),
            Data = zlib:inflate(Z, CompData),
            ok = zlib:inflateEnd(Z),
            ok = zlib:close(Z),

            R_2 = binary:part(R_1, {CompSize + Ofs1, byte_size(R_1) - CompSize - Ofs1}),
            zip_l_locfh(R_2, [{FileName, iolist_to_binary(Data)}|OL]);
        _ ->
            error
    end;
zip_l_locfh(<<"PK",1,2,_/binary>>, OL) ->
    {ok, lists:reverse(OL)};
zip_l_locfh(_, []) ->
    error.


lc(A) ->
    string:lowercase(A).


t2() ->
    open_3mf_file(mm, "cube.3mf").

t() ->
    open_3mf_file(mm, "cubemat.xml").

t_o() ->
    F1 = #e3d_face{vs=[3,2,0],vc=[],tx=[3,2,0],ns=[3,2,0],mat=[mat1]},
    F2 = #e3d_face{vs=[0,2,1],vc=[],tx=[0,2,1],ns=[0,2,1],mat=[mat1]},
    F3 = #e3d_face{vs=[1,2,3],vc=[],tx=[1,2,3],ns=[1,2,3],mat=[mat1]},
    F4 = #e3d_face{vs=[0,1,3],vc=[],tx=[0,1,3],ns=[0,1,3],mat=[mat1]},
    M1 = #e3d_mesh{vs=[
        {0.0,0.0,0.0},
        {2.0,0.0,0.0},
        {1.0,1.0,0.0},
        {1.0,2.0,1.0}
    ],vc=[],tx=[
        {0.0,0.0},
        {2.0,0.0},
        {1.0,1.0},
        {1.0,2.0}
    ],ns=[
        {0.0,0.0,0.0},
        {2.0,0.0,0.0},
        {1.0,1.0,0.0},
        {1.0,2.0,1.0}
    ],fs=[F1,F2,F3,F4]},

    F1B = #e3d_face{vs=[3,2,0],vc=[],tx=[3,2,0],ns=[3,2,0],mat=[mat2]},
    F2B = #e3d_face{vs=[0,2,1],vc=[],tx=[0,2,1],ns=[0,2,1],mat=[mat2]},
    F3B = #e3d_face{vs=[1,2,3],vc=[],tx=[1,2,3],ns=[1,2,3],mat=[mat2]},
    F4B = #e3d_face{vs=[0,1,3],vc=[],tx=[0,1,3],ns=[0,1,3],mat=[mat2]},
    M2 = #e3d_mesh{vs=[
        {10.0,0.0,0.0},
        {12.0,0.0,0.0},
        {11.0,1.0,0.0},
        {11.0,2.0,1.0}
    ],vc=[],tx=[
        {0.0,0.0},
        {2.0,0.0},
        {1.0,1.0},
        {1.0,2.0}
    ],ns=[
        {0.0,0.0,0.0},
        {2.0,0.0,0.0},
        {1.0,1.0,0.0},
        {1.0,2.0,1.0}
    ],fs=[F1B,F2B,F3B,F4B]},

    F1C = #e3d_face{vs=[3,2,0],vc=[],tx=[3,2,0],ns=[3,2,0],mat=[mat3]},
    F2C = #e3d_face{vs=[0,2,1],vc=[],tx=[0,2,1],ns=[0,2,1],mat=[mat3]},
    F3C = #e3d_face{vs=[1,2,3],vc=[],tx=[1,2,3],ns=[1,2,3],mat=[mat3]},
    F4C = #e3d_face{vs=[0,1,3],vc=[],tx=[0,1,3],ns=[0,1,3],mat=[mat3]},
    M3 = #e3d_mesh{vs=[
        {10.0,0.0,0.0},
        {12.0,0.0,0.0},
        {11.0,1.0,0.0},
        {11.0,2.0,1.0}
    ],vc=[],tx=[
        {0.0,0.0},
        {2.0,0.0},
        {1.0,1.0},
        {1.0,2.0}
    ],ns=[
        {0.0,0.0,0.0},
        {2.0,0.0,0.0},
        {1.0,1.0,0.0},
        {1.0,2.0,1.0}
    ],fs=[F1C,F2C,F3C,F4C]},
    
    Obj1 = #e3d_object{name="obj1",obj=M1},
    Obj2 = #e3d_object{name="obj2",obj=M2},
    Obj3 = #e3d_object{name="obj3",obj=M3},
    Mat1 = {mat1, [
        {opengl, [{diffuse, {0.4,0.5,0.6,1.0}}]}
    ]},
    Mat2 = {mat2, [
        {opengl, [{diffuse, {0.6,0.4,0.2,1.0}}]},
        {maps, [{diffuse, #e3d_image{}}]}
    ]},
    Mat3 = {mat3, [
        {opengl, [{diffuse, {0.6,0.4,0.8,1.0}}]},
        {maps, [{diffuse, #e3d_image{}}]}
    ]},
    E3DFile = #e3d_file{objs=[Obj1,Obj2,Obj3],mat=[Mat1,Mat2,Mat3],creator="Wings3D 3MF Exporter"},

    wr_3mf("debug.out", {mm, mm}, E3DFile).


