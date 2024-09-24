%%
%%  x3d_import.erl
%%
%%     Import X3D and VRML files
%%
%%  Copyright 2022-2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(x3d_import).

-export([init_import/0,do_import/2]).
-export([t_iv/0, t_x3d/0, t_vrml/0, t_x3dj/0]).

-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-define(WPCWRL, wpc_wrl).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-record(materialprops, {
    ambient_intensity :: float(),
    diffuse_color :: {float(), float(), float()},
    emissive_color :: {float(), float(), float()},
    shininess :: float(),
    specular_color :: {float(), float(), float()},
    transparency :: float()
}).

-record(geometry, {
    coords = [],
    coordIndices = [],
    normal = [],
    normalIndices = [],
    texCoords = none,
    tcIndices = none,
    colors = none,
    colIndices = none,
    creaseAngle :: float()
}).

-record(lightsrc, {
    light :: any(),
    ison :: boolean(),
    pos :: {float(),float(),float()},
    color :: {float(),float(),float()},
    intensity :: float(),
    ambintensity :: float()
}).

-record(shape_piece, {
    appearance = none,
    geometry :: #geometry{}
}).
-record(material, {
    material,
    texture,
    txtransform
}).



%%%
%%%

-ifdef(DEBUG_1).
-define(DEBUG_FMT(A,B), io:format(A,B)).
-else.
-define(DEBUG_FMT(A,B), none).
-endif.

%%%
%%% Import.
%%%

init_import() ->
    ok.

props() ->
    [{extensions,
      [{".x3d", "X3D File"},
       {".x3dz", "X3D File Compressed"},
       {".x3d.gz", "X3D File (gzipped)"},
       {".x3dj", "X3D JSON File"},
       {".x3djz", "X3D JSON File Compressed"},
       {".x3dj.gz", "X3D JSON File (gzipped)"},
       {".x3dv", "ClassicVRML File"},
       {".x3dvz", "ClassicVRML File Compressed"},
       {".x3dv.gz", "ClassicVRML File (gzipped)"},
       {".wrl", "VRML World"},
       {".wrz", "VRML World Compressed"},
       {".wrl.gz", "VRML World (gzipped)"},
       {".iv",  "SGI Inventor File"},
       {".iv.gz",  "SGI Inventor File (gzipped)"}]}].

do_import(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"X3D/VRML Import Options"), dialog(import),
           fun(Res) ->
               {file,{import,{wrl,Res}}}
           end);
do_import(Attr, St) ->
    wpa:import(props(), import_fun(Attr), St).
    
set_pref(KeyVals) ->
    wpa:pref_set(?WPCWRL, KeyVals).

import_transform(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

import_fun(KeyVals) ->
    fun(Filename) ->
        set_pref(KeyVals),
        ShortFilename = filename:rootname(filename:basename(Filename)),
        
        FType = get_file_type(Filename), % x3d or wrl
        ets:new(?MODULE, [named_table,public,ordered_set]),
        Return = try 
          {ok, ShapeList} = read_file_content(FType, Filename),
          ?DEBUG_FMT("ShapeList=~w~n", [ShapeList]),
          {ok, E3DObjects, MatsList} = shape_list_to_objects(ShortFilename, ShapeList, Filename),
          E3dFile = #e3d_file{objs=E3DObjects,mat=MatsList},
          {ok,import_transform(E3dFile, KeyVals)}
        catch _:Err:ST ->
          io:format(?__(1,"X3D Import Error: ~P in")++" ~p~n", [Err,30,ST]),
          {error, lists:flatten(?__(2,"VRML/X3D Import Error"))}
        end,
        ets:delete(?MODULE),
        Return
    end.

shape_list_to_objects(ShortFilename, ShapesList_0, X3DFullPath) ->
    %% Since the exporter exports objects in separate pieces based on
    %% materials, it makes sense to recombine the pieces for objects. 
    
    ShapesList_1 = [Shape || Shape <- ShapesList_0,
                             Shape =/= unimp_container],
    
    {_Lights, ShapesList} = lists:partition(
        fun (#lightsrc{}=_) -> true; (_) -> false end,
        ShapesList_1),
    ShapePiecesList = combine_shape_pieces(ShapesList),
    E3DObjectsAndMats = [shape_to_object(ShortFilename, ShapePieces, X3DFullPath)
        || ShapePieces <- ShapePiecesList],
    {E3DObjects, MatsList} = lists:unzip(E3DObjectsAndMats),
    ?DEBUG_FMT("E3DObjects=~w~nMatsList=~w~n", [E3DObjects, MatsList]),
    {ok, E3DObjects, lists:append(MatsList)}.


%% Go through ShapesList, and put in lists together of shapes that
%% share edges so they can be welded together into objects.
%%
combine_shape_pieces(ShapesList) ->
    combine_shape_pieces(ShapesList, []).
combine_shape_pieces([], Grp) ->
    lists:reverse(Grp);
combine_shape_pieces([Shapes], Grp) ->
    lists:reverse([[Shapes]|Grp]);
combine_shape_pieces([Shape1|ShapesList], Grp) ->
    CoordPairs1 = rv_coordpairs_from_shapes([Shape1]),
    combine_shape_pieces_1({Shape1, CoordPairs1}, ShapesList, [], [], Grp, false).
combine_shape_pieces_1(Shp1, [Shape2|ShapesList], SameShape, Other, Grp, Found) ->
    case combine_shape_pieces_same(Shp1, Shape2) of
        true ->
            {Shape1,_}=Shp1,
            combine_shape_pieces_1(
                {Shape1,rv_coordpairs_from_shapes([Shape1,Shape2|SameShape])},
                ShapesList, [Shape2|SameShape], Other, Grp, true);
        false ->
            combine_shape_pieces_1(Shp1, ShapesList, SameShape, [Shape2|Other], Grp, Found)
    end;
combine_shape_pieces_1({Shape1, _}, [], SameShape, Other, Grp, false) ->
    combine_shape_pieces(Other, [lists:reverse([Shape1|SameShape])|Grp]);
combine_shape_pieces_1(Shp1, [], SameShape, Other, Grp, true) ->
    %% Try again with the other shapes
    combine_shape_pieces_1(Shp1, Other, SameShape, [], Grp, false).
combine_shape_pieces_same({_,Coords1Set},#shape_piece{geometry=Geom2}=_) ->
    #geometry{coords=Coords2,coordIndices=Coords2I}=Geom2,
    combine_shape_pieces_same_1(Coords1Set, coord_pairs(Coords2, Coords2I)).
combine_shape_pieces_same_1(Coords1Set, CoordsPair2) ->
    not sets:is_disjoint(Coords1Set, coordpairs_sets(CoordsPair2)).

coordpairs_sets(List) ->
    wings_util:sets_from_list(List).
rv_coordpairs_from_shapes(SL) ->
    CoordPairs = lists:append([rv_coordpairs_from_shapes_1(S) || S <- SL]),
    coordpairs_sets(CoordPairs).
rv_coordpairs_from_shapes_1(#shape_piece{geometry=Geom1}) ->
    #geometry{coords=Coords1,coordIndices=Coords1I}=Geom1,
    reverse_coord_pairs(Coords1,Coords1I).


reverse_coord_pairs(Coords, Indices) ->
    [{E2,E1} || {E1,E2} <- coord_pairs(Coords, Indices)].
    
coord_pairs(Coords, Indices) ->
    Arr = array:from_list(Coords),
    [{array:get(I1,Arr),array:get(I2,Arr)} || {I1,I2} <- all_edges(Indices)].



fill_in_colorlist(Fs0, List) ->
    fill_in_txlist(Fs0, List).

fill_in_txlist(Fs0, []) ->
    fill_in_txlist(Fs0, none, []);
fill_in_txlist(Fs0, B) ->
    fill_in_txlist(Fs0, B, []).
fill_in_txlist([_F|[_|_]=Fs0], [A|[_|_]=B], O) ->
    fill_in_txlist(Fs0, B, [A|O]);
fill_in_txlist([_F|Fs0], none, O) ->
    fill_in_txlist(Fs0, none, [[]|O]);
fill_in_txlist([_F|Fs0], [A]=B, O) ->
    fill_in_txlist(Fs0, B, [A|O]);
fill_in_txlist([], _, O) ->
    lists:reverse(O).
    

fill_in_matname(Fs0, B) ->
    fill_in_matname(Fs0, B, []).
fill_in_matname([_F|Fs0], none, O) ->
    fill_in_matname(Fs0, none, [default|O]);
fill_in_matname([_F|Fs0], MatName=B, O) when is_atom(MatName) ->
    fill_in_matname(Fs0, B, [MatName|O]);
fill_in_matname([], _, O) ->
    lists:reverse(O).

%% Have multiple VRML shape_piece items in the list to combine them together
shape_to_object(ShortFilename, ShapePieces, X3DFullPath) ->
    ShapeId = "_" ++ integer_to_list(abs(erlang:unique_integer())),
    ObjectName = ShortFilename ++ ShapeId,
    
    [#shape_piece{geometry=#geometry{creaseAngle=CrAng}}|_] = ShapePieces,
    
    {Mat1_2, Vs_2, Ns_2, Colors_2, TxList_2, Fs0L_2, _,_,_,_} = lists:foldl(
        fun(Shape,
            {Mat1_0, Vs_0, Ns_0, Colors_0, TxList_0, Fs0L_0, VsOffset,NsOffset,TxOffset,ColOffset})
        ->
            {Mat1_1, Vs_1, Ns_1, Colors_1, TxList_1, Fs0L_1,
             VsOffset_1, NsOffset_1, TxOffset_1, ColOffset_1} =
                shape_piece_for_object(ObjectName, Shape,
                    VsOffset, NsOffset, TxOffset, ColOffset, X3DFullPath),
            {[Mat1_1|Mat1_0], [Vs_1|Vs_0], [Ns_1|Ns_0],
                [Colors_1|Colors_0], [TxList_1|TxList_0], [Fs0L_1|Fs0L_0],
                VsOffset_1, NsOffset_1, TxOffset_1, ColOffset_1 }
        end, {[],[],[],[],[],[],0,0,0,0}, ShapePieces),
    
    Mat1   = lists:reverse(lists:filter(
        fun(none) -> false; (_) -> true end, Mat1_2)),
    Vs     = lists:append(lists:reverse(Vs_2)),
    Ns     = lists:append(lists:reverse(Ns_2)),
    Vc     = lists:append(lists:reverse(Colors_2)),
    TxList = lists:append(lists:reverse(TxList_2)),
    Fs0L   = lists:append(lists:reverse(Fs0L_2)),
    
    Efs = [ #e3d_face{
        vs=L,
        ns=NL,
        tx=LTx,
        vc=C,
        mat=
            case MatName of
                none -> [];
                _ -> [MatName]
            end
    } || {L, NL, C, LTx, MatName} <- Fs0L],
    
    %% Put into a mesh and apply the crease angle if needed
    Mesh = apply_crease(#e3d_mesh{
        type=polygon,
        vs=Vs,
        ns=Ns,
        vc=Vc,
        fs=Efs,
        %%he=HEs,
        tx=TxList }, CrAng),
        
    ?DEBUG_FMT("Mesh=~p~n", [Mesh]),
    
    Obj = #e3d_object{name=ObjectName,obj=Mesh},
    {Obj, Mat1}.

shape_piece_for_object(ObjectName,
    #shape_piece{appearance=Appearance,geometry=Geometry}=_Shape,
    VsOffset, NsOffset, TxOffset, ColOffset, X3DFullPath)
->
    case Appearance of
        #material{ material=MatPs, texture=Filename} ->
            {MatName, Mat1} = appearance_to_material(ObjectName, MatPs, Filename, VsOffset, X3DFullPath);
        _ ->
            MatName = none,
            Mat1 = none
    end,
    #geometry{coords=Vs,coordIndices=Fs0_0,
        normal=Ns_0,normalIndices=Fs0Ns_0,
        texCoords=TxList_0,
        tcIndices=Fs0Tx_0,colors=Colors_0,colIndices=ColIndices_0,
        creaseAngle=_CreaseAngle} = Geometry,
    Fs0 = [ [F+VsOffset || F <- FL] || FL <- Fs0_0],
    case Ns_0 of
        none ->
            Ns = [],
            NsIndices = [];
        _ ->
            Ns = Ns_0,
            NsIndices = [[N+NsOffset || N <- NL] || NL <- Fs0Ns_0]
    end,
    case Colors_0 of
        none ->
            Colors = [],
            ColIndices = [];
        _ ->
            Colors = Colors_0,
            ColIndices = [[C+ColOffset || C <- CL] || CL <- ColIndices_0]
    end,
    ColIndices_1 = fill_in_colorlist(Fs0, ColIndices),
    case TxList_0 of
        none -> TxList = [];
        _ ->    TxList = TxList_0
    end,
    Fs0Tx_1 = case Fs0Tx_0 of
        none -> none;
        _ -> [ [T+TxOffset || T <- TL] || TL <- Fs0Tx_0]
    end,
    Fs0Tx = fill_in_txlist(Fs0, Fs0Tx_1),
    MatNames = fill_in_matname(Fs0, MatName),
    VsOffset_1 = length(Vs) + VsOffset,
    NsOffset_1 = length(Ns) + NsOffset,
    TxOffset_1 = length(TxList) + TxOffset,
    ColOffset_1 = length(Colors) + ColOffset,
    {Mat1, Vs, Ns, Colors, TxList, zip_face_elems(Fs0, NsIndices, ColIndices_1, Fs0Tx, MatNames),
        VsOffset_1, NsOffset_1, TxOffset_1, ColOffset_1}.
    
zip_face_elems(A,Ns,B,C,D) ->
    zip_face_elems(A,Ns,B,C,D,[]).
zip_face_elems([A|AR],NsL_0,CL_0,TxL_0,MtL_0, O) ->
    {NsL,Ns} = case NsL_0 of
        [] -> {[], []};
        [Ns_2|NsL_2] -> {NsL_2,Ns_2}
    end,
    {CL,C} = case CL_0 of
        [] -> {[], []};
        [C_2|CL_2] -> {CL_2,C_2}
    end,
    {TxL,T} = case TxL_0 of
        [] -> {[], []};
        [T_2|TxL_2] -> {TxL_2,T_2}
    end,
    {MtL,D} = case MtL_0 of
        [] -> {[], []};
        [D_2|MtL_2] -> {MtL_2,D_2}
    end,
    zip_face_elems(AR,NsL,CL,TxL,MtL, [{A,Ns,C,T,D}|O]);
zip_face_elems([],[],[],[],[], O) ->
    lists:reverse(O).


%% Use creaseAngle to set hard edges if there are no normals yet.
%%
apply_crease(#e3d_mesh{ns=[_|_],fs=[#e3d_face{ns=[_|_]}|_]}=Mesh0, _) ->
    Mesh0;
apply_crease(Mesh_0, Ang_0) ->
    %% Need to remove duplicate vertices first.
    #e3d_mesh{vs=VS,fs=Efs}=Mesh=e3d_mesh:merge_vertices(Mesh_0),
    io:format("X3D: NOTE: Applying creaseAngle~n",[]),
    try
        Ang = math:cos(Ang_0),
        Arr = array:from_list(VS),
        SNrm_0 = [apply_crease_1(V, Arr) || #e3d_face{vs=V} <- Efs],
        SNrm = apply_crease_3(SNrm_0),
        ELst = lists:append([EL || {EL,_} <- SNrm_0]),
        HE = apply_crease_4(ELst, SNrm, Ang),
        Mesh#e3d_mesh{he=HE}
    catch ErrCls:Err:ST ->
        io:format("X3D: ------------------------------------~n",[]),
        io:format("X3D: ERROR: creaseAngle failed: ~p:~p~n~p~n",[ErrCls,Err,ST]),
        io:format("X3D: ------------------------------------~n",[]),
        Mesh
    end.

apply_crease_1([V1,V2,V3|_]=VIdx, Arr) ->
    Norm = e3d_vec:normal(array:get(V1, Arr), array:get(V2, Arr), array:get(V3, Arr)),
    {apply_crease_2(VIdx), Norm}.

apply_crease_2([V1|_]=L) ->
    apply_crease_2(L, V1).
apply_crease_2([V1], B1) ->
    [{V1,B1}];
apply_crease_2([V1|[V2|_]=L], B1) ->
    [{V1,V2}|apply_crease_2(L, B1)].

apply_crease_3(L) ->
    apply_crease_3(L, gb_trees:empty()).
apply_crease_3([], SNrm) ->
    SNrm;
apply_crease_3([{Edges, Norm}|L], Set0) ->
    apply_crease_3(L, lists:foldl(
        fun(E, Set) ->
            gb_trees:insert(E, Norm, Set)
        end, Set0, Edges)).

%% Used wings_body:auto_smooth/5 as reference for this.
apply_crease_4(ELst, SNrm, Ang) ->
    lists:foldl(fun (E, HE) -> apply_crease_4_2(E, SNrm, Ang, HE) end, [], ELst).
apply_crease_4_2({V1,V2}, SNrm, Ang, HE) ->
    Nrm1 = gb_trees:get({V1,V2},SNrm),
    Nrm2 = gb_trees:get({V2,V1},SNrm),
    case e3d_vec:is_zero(Nrm1) orelse e3d_vec:is_zero(Nrm2) of
        true ->
            HE;
        _ ->
            case e3d_vec:dot(Nrm1, Nrm2) of
                DAng when DAng < Ang ->
                    %% Hard
                    [{V1,V2}|HE];
                _ ->
                    %% Soft
                    HE
            end
    end.


edge_pairs([E|_]=Fs) ->
    edge_pairs(Fs, E, []).
edge_pairs([E1|[E2|_]=Fs], E0, OL) ->
    edge_pairs(Fs, E0, [{E1,E2}|OL]);
edge_pairs([E1], E0, OL) ->
    lists:reverse([{E1,E0}|OL]).

all_edges(FL) ->
    lists:append([edge_pairs(F) || F <- FL]).

    
appearance_opengl(MatPs) ->
    OpenGL = {opengl, 
        appearance_opengl_1(MatPs) ++
    [
        {metallic,0.1},
        {roughness,0.8},
        {vertex_colors, set}
    ]},
    {ok, OpenGL}.

appearance_opengl_1(#materialprops{
    ambient_intensity=AmbInt,
    specular_color=SpecCol,
    shininess=Shine,
    diffuse_color=DifCol,
    transparency=Transparency,
    emissive_color=EmCol
}) ->
    [
        {ambient, intensity_to_rgba(AmbInt)},
        {specular, rgb_to_rgba(SpecCol)},
        {shininess, Shine},
        {diffuse, rgb_to_rgba(DifCol, 1.0 - Transparency)},
        {emission, rgb_to_rgba(EmCol)}
    ];
appearance_opengl_1(none) ->
    [
        {ambient,{0.0,0.0,0.0,0.0}},
        {specular, {0.2,0.2,0.2,1.0}},
        {shininess,0.2},
        {diffuse, {0.8,0.8,0.8,1.0}},
        {emission,{0.0,0.0,0.0,1.0}}
    ].

appearance_to_material(ObjectName, MatPs, none, VsOffset, _FullPath) ->
    {ok, OpenGL} = appearance_opengl(MatPs),
    S = lists:flatten(io_lib:format("_~p", [VsOffset])),
    MatName = list_to_atom("material_" ++ ObjectName ++ S),
    Mat1 = {MatName, [OpenGL]},
    {MatName, Mat1};
appearance_to_material(ObjectName, MatPs, Filename, _VsOffset, X3DFullPath) ->
    {ok, OpenGL} = appearance_opengl(MatPs),
    MatName = list_to_atom("material_" ++ ObjectName),
    Id = "_" ++ integer_to_list(abs(erlang:unique_integer())),
    {ok, MapsL} = load_texture_maps(Filename, Id, X3DFullPath),
    Mat1 = {MatName, MapsL ++ [OpenGL]},
    {MatName, Mat1}.
    
load_texture_maps(<<>>, _Id, _FullPath) ->
    [];
load_texture_maps({image, Width, Height, NumComponents, ImageData}, _Id, _FullPath) ->
    E3DImage = pixel_image({image, Width, Height, NumComponents, ImageData}),
    Maps = {maps, [{diffuse, E3DImage}]},
    {ok, [Maps]};
load_texture_maps(Filename_0, _Id, X3DFullPath) ->
    case get_bitmap(Filename_0, X3DFullPath) of
        {ok, E3DImage} ->
            Maps = {maps, [{diffuse, E3DImage}]},
            {ok, [Maps]};
        {error, Err} ->
            %% Image could not be loaded.
            io:format("X3D: " ++ ?__(1,"INFO: Texture could not be loaded:") ++
                " ~p: ~p~n", [Filename_0, Err]),
            {ok, [{maps, []}]}
    end.


%% Lines ordered from bottom to top
pixel_image({image, Width, Height, NumComponents, ImageData}) ->
    TexName = "piximage" ++ integer_to_list(abs(erlang:unique_integer())) ++ ".bmp",
    Blob = list_to_binary(pixel_image_scan_lines(Width,
        [pixel_image_to_rgba(NumComponents, round(P)) || P <- ImageData])),
    #e3d_image{
        type = b8g8r8a8,
        bytes_pp = 4,
        alignment = 1,
        order = lower_left,
        width = Width,
        height = Height,
        image = Blob,
        filename=none,
        name=TexName,
        extra=[]
    }.
pixel_image_to_rgba(1, P) ->
    R = G = B = P,
    A = 255,
    <<R,G,B,A>>;
pixel_image_to_rgba(2, P) ->
    R = G = B = (P bsr 8 band 16#ff),
    A = P band 16#ff,
    <<R,G,B,A>>;
pixel_image_to_rgba(3, P) ->
    R = P bsr 16 band 16#ff,
    G = P bsr 8 band 16#ff,
    B = P band 16#ff,
    A = 255,
    <<R,G,B,A>>;
pixel_image_to_rgba(4, P) ->
    R = P bsr 24 band 16#ff,
    G = P bsr 16 band 16#ff,
    B = P bsr 8 band 16#ff,
    A = P band 16#ff,
    <<R,G,B,A>>.
pixel_image_scan_lines(Width, Data) ->
    pixel_image_scan_lines(Width, Data, []).
pixel_image_scan_lines(Width, [_|_]=Data, O) ->
    {Line, Data_1} = lists:split(Width, Data),
    pixel_image_scan_lines(Width, Data_1, [Line | O]);
pixel_image_scan_lines(_Width, [], O) ->
    lists:append(lists:reverse(O)).

            
intensity_to_rgba(F) ->
    {F,F,F,F}.
rgb_to_rgba(RGB) ->
    rgb_to_rgba(RGB, 1.0).
rgb_to_rgba({R,G,B}, Alpha) ->
    {R,G,B,Alpha}.
    

dialog(import) ->
    [wpa:dialog_template(?WPCWRL, import, [include_colors])].


get_file_type(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
        ".json" ++ _ -> x3dj;
        ".x3dj" ++ _ -> x3dj;
        ".x3dv" ++ _ -> wrl;
        ".x3d"  ++ _ -> x3d;
        ".wrl"  ++ _ -> wrl;
        ".wrz" -> wrl;
        ".iv"  -> wrl;
        ".gz" ++ _ ->
            case string:to_lower(filename:extension(filename:rootname(FileName))) of
                ".json" ++ _ -> x3dj;
                ".x3dj" ++ _ -> x3dj;
                ".x3dv" ++ _ -> wrl;
                ".x3d"  ++ _ -> x3d;
                ".wrl"  ++ _ -> wrl;
                ".wrz" -> wrl;
                ".iv"  -> wrl
            end
    end.
    
read_file_content(x3d, Filename) ->
    {ok, File} = file:read_file(Filename),
    {ok, Conts} = read_x3d_content(File),
    ShapeList = lists:flatten([ begin
        {ok, C} = trav(def_or_use_var(Cont)),
        C
      end || Cont <- Conts]),
    {ok, ShapeList};
read_file_content(x3dj, Filename) ->
    {ok, File} = file:read_file(Filename),
    {ok, Conts} = read_x3djson_content(File),
    ShapeList = lists:flatten([ begin
        {ok, C} = trav(def_or_use_var(Cont)),
        C
      end || Cont <- Conts]),
    {ok, ShapeList};
read_file_content(wrl, Filename) ->
    {ok, File} = file:read_file(Filename),
    case read_vrml_content(File) of
        {ok, Conts} ->
            ShapeList = lists:flatten([ begin
                {ok, C} = trav(def_or_use_var(Cont)),
                C
              end || Cont <- Conts]),
            {ok, ShapeList};
        _ ->
            error
    end.


%%
%% X3D File
%%

%% State file for xmerl sax.
-record(x3dtk, {
    list = [],
    inscene = false  % Is event inside the <Scene> tag
}).


read_x3d_content(Bin_0) ->
    case Bin_0 of
        <<31,139,_/binary>> ->
            %% A gz header, the file is compressed.
            read_x3d_content_1(zlib:gunzip(Bin_0));
        _ ->
            %% Uncompressed
            read_x3d_content_1(Bin_0)
    end.
read_x3d_content_1(Bin_0) ->
    EF = {event_fun, fun x3d_tok/3},
    ES = {event_state, #x3dtk{}},
    {ok, Bin_1} = x3d_change_prolog(Bin_0),
    case xmerl_sax_parser:stream(Bin_1, [EF,ES]) of
        {ok, #x3dtk{list=List_0}=_Es, _} ->
            List_1 = lists:reverse(List_0),
            {ok, List_2, _} = parse_x3d(<<"Transform">>, List_1),
            ?DEBUG_FMT("~p~n", [List_2]),
            {ok, [{<<"Transform">>, [{{field, <<"children">>}, {multival, List_2}}]}]};
        {Error, {_,_,Line}, Reason, _ET, _St} ->
            io:format("X3D:~p: ERROR: ~p:~p~n", [Line, Error, Reason]),
            {error, "unknown/unhandled format, see log window"}
    end.


%% xmerl doesn't like certain things in the header part of X3D
%% files, namely SGML DOCTYPE.
x3d_change_prolog(A) ->
    x3d_change_prolog(A, []).
x3d_change_prolog(<<>>, SoFar) ->
    {ok, iolist_to_binary(lists:reverse(SoFar))};
x3d_change_prolog(<<"<!", B/binary>>, SoFar) ->
    [_Before, After] = string:split(B, <<">">>),
    %% Remove SGML things, things that start with <!
    x3d_change_prolog(After, SoFar);
x3d_change_prolog(<<"<X3D", _/binary>> = X3DStart, SoFar) ->
    %% Beginning of X3D, we might be good now.
    {ok, iolist_to_binary(lists:reverse(SoFar) ++ [X3DStart])};
x3d_change_prolog(<<A, B/binary>>, SoFar) ->
    x3d_change_prolog(B, [A | SoFar]).

x3d_tok({startElement, _, LName, _, Attributes_0}=_Ev, _Loc, #x3dtk{inscene=InScene,list=List}=State) ->
    case LName of
        "Scene" ->
            State#x3dtk{ inscene = true };
        _ when InScene =:= true ->
            try 
            LName_B = iolist_to_binary(LName),
            Attributes = [
                x3d_tok_attr_pair(LName_B, AttrLName, Val)
            || {_,_,AttrLName,Val} <- Attributes_0],
            State#x3dtk{list=[{s, LName_B, Attributes} | List]}
            catch error:Err:Tr -> io:format("ERR ~p ~p\n", [Err, Tr]) end;
        _ ->
            State
    end;
x3d_tok({endElement, _, LName, _}=_Ev, _Loc, #x3dtk{inscene=InScene,list=List}=State) ->
    case LName of
        "Scene" ->
            State#x3dtk{ inscene = false };
        _ when InScene =:= true ->
            State#x3dtk{list=[{e, iolist_to_binary(LName)} |List]};
        _ ->
            State
    end;
x3d_tok({characters, _}=_Ev, _Loc, State) ->
    State;
x3d_tok(startDocument, _, State) -> State;
x3d_tok(endDocument, _, State) -> State;
x3d_tok({startPrefixMapping,_,_}, _, State) -> State;
x3d_tok({endPrefixMapping,_}, _, State) -> State;
x3d_tok({ignorableWhitespace, _}, _, State) -> State;
x3d_tok({comment, _}, _, State) -> State;
x3d_tok(_Ev, _Loc, State) ->
    State.
    

x3d_tok_attr_pair(LName_B, AttrLName, Val) ->
    AttrLName_B = unicode:characters_to_nfc_binary(AttrLName),
    Val_B = unicode:characters_to_nfc_binary(Val),
    x3d_tok_attr(LName_B, AttrLName_B, Val_B).

x3d_tok_attr(_LName_B, AttrLName_B, Val_B)
    when AttrLName_B =:= <<"DEF">>;
         AttrLName_B =:= <<"USE">> ->
    {ok, [Token]} = tok(Val_B),
    {AttrLName_B, Token};
x3d_tok_attr(LName_B, AttrLName_B, Val_B) ->
    %% Determine type of field and whether to tokenize it.
    FieldType = expected_field_type(LName_B, {word, AttrLName_B}),
    case x3d_xatr_pt(LName_B, AttrLName_B, FieldType) of
        string ->
            %% Verbatim XML attribute not used by the X3D parser.
            {{user, AttrLName_B}, {string, Val_B}};
            
        word ->
            %% It can be possible that for fields that are for strings
            %% such as url, the exporter of the file may omit quotes
            %% around the string.
            case FieldType of
                {multival, string} ->
                    Val_B_1 = x3d_tok_attr_addqu(Val_B);
                string ->
                    Val_B_1 = x3d_tok_attr_addqu(Val_B);
                _ ->
                    Val_B_1 = Val_B
            end,
            %% Parse the contents of the XML attribute with tok
            {ok, Tokens} = tok(iolist_to_binary(x3d_tok_attr_convqu(Val_B_1))),
            case FieldType of
                {multival, _} ->
                    %% We want the field parser to use as much of the XML attribute as
                    %% possible, so add brackets around the tokens.
                    {ok, FA, []} = parse_field(FieldType,
                        {word, AttrLName_B},
                        [open_bracket] ++ Tokens ++ [close_bracket]);
                _ ->
                    {ok, FA, []} = parse_field(FieldType,
                        {word, AttrLName_B},
                        Tokens)
            end,
            FA
    end.
    
%% In X3D attributes for the url might be url="file.png" but
%% we add double quotes for the parser.
%%
x3d_tok_attr_addqu(StrVal) ->
    StrVal_1 = string:trim(StrVal),
    BV = string:find(StrVal_1, "'", leading),
    TV = string:find(StrVal_1, "'", trailing),
    case BV =:= StrVal_1 andalso TV =:= <<"'">> of
        false ->
            x3d_tok_attr_addqu_1(StrVal);
        true ->
            StrVal_1
    end.
x3d_tok_attr_addqu_1(StrVal) ->
    StrVal_1 = string:trim(StrVal),
    BV = string:find(StrVal_1, [34], leading),
    TV = string:find(StrVal_1, [34], trailing),
    case BV =:= StrVal_1 andalso TV =:= <<34>> of
        false ->
            iolist_to_binary(io_lib:format("~p", [binary_to_list(StrVal)]));
        true ->
            StrVal_1
    end.
    
%% If the attribute already contains single quotes, change them
%% to double quotes.
%%
x3d_tok_attr_convqu(StrVal) ->
    case string:find(StrVal, [34]) of
        nomatch ->
            x3d_tok_attr_convqu_1(StrVal);
        _ ->
            StrVal
    end.
x3d_tok_attr_convqu_1(StrVal) ->
    case string:find(StrVal, "'") of
        nomatch ->
            StrVal;
        _ ->
            x3d_tok_attr_convqu_2(StrVal)
    end.
x3d_tok_attr_convqu_2(StrVal) ->
    case string:tokens(binary_to_list(StrVal), "'") of
        L when length(L) rem 2 =:= 1 ->
            string:replace(StrVal, "'", [34], all);
        _ ->
            %% Unbalanced.
            StrVal
    end.
            

%% XML Attribute pre-typing
%% Determine which attributes of which nodes to tokenize for
%% later parsing.
%% 'word' means to parse the content of the attribute for tokens
%% 'string' means to keep the attribute value verbatim as a string
%%
%% We check the expected_field_type/2 function return value
%% to see if the field is defined at all, if it is, then
%% we use 'word', if 'any' is returned, which likely means
%% it isn't part of the VRML/X3D spec, we use string.
%%
x3d_xatr_pt(_Node, _AttrName, any) -> string;
x3d_xatr_pt(_Node, _AttrName, _) -> word.

%%
%% Parse element opening and closing tags after first pass with
%% XML SAX.
%% 

parse_x3d(ContainerType, A) ->
    parse_x3d(ContainerType, A, []).
parse_x3d(_ContainerType, [], SoFar) ->
    {ok, lists:reverse(SoFar), []};
parse_x3d(ContainerType, [{e, ContainerType} | Rest], SoFar) ->
    {ok, lists:reverse(SoFar), Rest};
parse_x3d(ContainerType, [{s, WordC, Attr_0}|Rest], SoFar) ->
    {Fields_0, NameDef, NameUse} = parse_x3d_attr(Attr_0),
    {ok, InnerCont, Rest_1} = parse_x3d(WordC, Rest),
    {ok, Fields_C} = categorize_xml_to_x3d_field(ContainerType, InnerCont),
    Fields = Fields_0 ++ Fields_C,
    ContVal_0 = {container, WordC, Fields},
    case NameDef of
        none ->
            ContVal_1 = ContVal_0;
        _ ->
            ContVal_1 = {def, NameDef, ContVal_0}
    end,
    case NameUse of
        none ->
            ContVal_2 = ContVal_1;
        _ ->
            ContVal_2 = {use, NameUse, ContVal_1}
    end,
    parse_x3d(ContainerType, Rest_1, [ContVal_2 | SoFar]).

parse_x3d_attr(A) ->
    parse_x3d_attr(A, [], none, none).
parse_x3d_attr([], List, NameDef, NameUse) ->
    {lists:reverse(List), NameDef, NameUse};
parse_x3d_attr([{<<"DEF">>, {word, NameDef}} | A], List, _, NameUse) ->
    Variable_S = unicode:characters_to_list(NameDef, utf8),
    parse_x3d_attr(A, List, Variable_S, NameUse);
parse_x3d_attr([{<<"USE">>, {word, NameUse}} | A], List, NameDef, _) ->
    NameUse_S = unicode:characters_to_list(NameUse, utf8),
    parse_x3d_attr(A, List, NameDef, NameUse_S);
parse_x3d_attr([{K, V} | A], List, NameDef, NameUse) ->
    parse_x3d_attr(A, [{K, V} | List], NameDef, NameUse).

%% The X3D specification still distinguishes sub-nodes as part
%% of a specific field of the parent node as it did for VRML, so
%% we will categorize the XML nodes into the X3D field values.
%%
categorize_xml_to_x3d_field(ContT, A) ->
    categorize_xml_to_x3d_field(ContT, A, []).
categorize_xml_to_x3d_field(ContT, [], SoFar) ->
    {ok, [
        {Key, categorize_xml_to_x3d_field_s_or_m(ContT, FName, List)}
    || {{field, FName}=Key, List} <- SoFar]};
categorize_xml_to_x3d_field(ContT, [{container, ContN, _}=Cont | Rest], SoFar) ->
    case categorize_xml_to_x3d_field_get_field(ContN) of
        {ok, FieldToUse} ->
            SoFar_1 = orddict:append(
                {field, FieldToUse}, Cont, SoFar);
        _ -> 
            SoFar_1 = SoFar
    end,
    categorize_xml_to_x3d_field(ContT, Rest, SoFar_1);
categorize_xml_to_x3d_field(ContT, [{use, UseName, {container, ContN, _}}=_Cont | Rest], SoFar) ->
    case categorize_xml_to_x3d_field_get_field(ContN) of
        {ok, FieldToUse} ->
            SoFar_1 = orddict:append(
                {field, FieldToUse}, {use, UseName}, SoFar);
        _ -> 
            SoFar_1 = SoFar
    end,
    categorize_xml_to_x3d_field(ContT, Rest, SoFar_1);
categorize_xml_to_x3d_field(ContT, [{def, _, {container, ContN, _}}=Cont | Rest], SoFar) ->
    case categorize_xml_to_x3d_field_get_field(ContN) of
        {ok, FieldToUse} ->
            SoFar_1 = orddict:append(
                {field, FieldToUse}, Cont, SoFar);
        _ -> 
            SoFar_1 = SoFar
    end,
    categorize_xml_to_x3d_field(ContT, Rest, SoFar_1).

%% If a list has one node and the field type is for a single value,
%% remove the list and just have the node, if the field type is a
%% multival, make the field value a multival.
%%
categorize_xml_to_x3d_field_s_or_m(ContT, FName, [Item|_]=List) ->
    case expected_field_type(ContT, {word, FName}) of
        {multival, _} -> {multival, List};
        _ -> Item
    end.

categorize_xml_to_x3d_field_get_field(A) ->
    case xml_x3d_fld(A) of
        {ndfield, B} -> {ok, B};
        unknown -> unknown
    end.

%% Determine from the node type, as to what the field it is setting
%% on the parent node.
%%

xml_x3d_fld(<<"X3DChildNode">>) -> {ndfield, <<"children">>};
xml_x3d_fld(<<"X3DCoordinateNode">>) -> {ndfield, <<"coord">>};
xml_x3d_fld(<<"X3DAppearanceNode">>) -> {ndfield, <<"appearance">>};
xml_x3d_fld(<<"X3DGeometryNode">>) -> {ndfield, <<"geometry">>};
xml_x3d_fld(<<"X3DMaterialNode">>) -> {ndfield, <<"material">>};
xml_x3d_fld(<<"X3DTextureNode">>) -> {ndfield, <<"texture">>};
xml_x3d_fld(<<"X3DTextureTransformNode">>) -> {ndfield, <<"textureTransform">>};
xml_x3d_fld(<<"AcousticProperties">>) -> {ndfield, <<"acousticProperties">>};
xml_x3d_fld(<<"FillProperties">>) -> {ndfield, <<"fillProperties">>};
xml_x3d_fld(<<"LineProperties">>) -> {ndfield, <<"lineProperties">>};
xml_x3d_fld(<<"PointProperties">>) -> {ndfield, <<"pointProperties">>};
xml_x3d_fld(<<"X3DNormalNode">>) -> {ndfield, <<"normal">>};
xml_x3d_fld(<<"X3DTextureCoordinateNode">>) -> {ndfield, <<"texCoord">>};
xml_x3d_fld(<<"X3DColorNode">>) -> {ndfield, <<"color">>};
xml_x3d_fld(A) ->
    %% Go up the subclasses of node types until we find
    %% a node type associated to a field.
    case x3d_sub_of(A) of
        root -> unknown;
        Another when is_binary(Another) ->
            xml_x3d_fld(Another)
    end.


x3d_sub_of(<<"X3DViewpointNode">>) -> <<"X3DBindableNode">>;
x3d_sub_of(<<"Billboard">>) -> <<"X3DGroupingNode">>;
x3d_sub_of(<<"Collision">>) -> <<"X3DGroupingNode, X3DSensorNode">>;
x3d_sub_of(<<"LOD">>) -> <<"X3DGroupingNode">>;
x3d_sub_of(<<"NavigationInfo">>) -> <<"X3DBindableNode">>;

x3d_sub_of(<<"OrthoViewpoint">>) -> <<"X3DViewpointNode">>;

x3d_sub_of(<<"Viewpoint">>) -> <<"X3DViewpointNode">>;

x3d_sub_of(<<"ViewpointGroup">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"X3DBackgroundNode">>) -> <<"X3DBindableNode">>;

x3d_sub_of(<<"Background">>) -> <<"X3DBackgroundNode">>;

x3d_sub_of(<<"Fog">>) -> <<"X3DBindableNode">>;

x3d_sub_of(<<"FogCoordinate">>) -> <<"X3DGeometricPropertyNode">>;

x3d_sub_of(<<"LocalFog">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"TextureBackground">>) -> <<"X3DBackgroundNode">>;

x3d_sub_of(<<"X3DBindableNode">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"X3DInfoNode">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"X3DSensorNode ">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"WorldInfo">>) -> <<"X3DInfoNode">>;


x3d_sub_of(<<"Appearance">>) -> <<"X3DAppearanceNode">>;

x3d_sub_of(<<"Material">>) -> <<"X3DMaterialNode">>;
x3d_sub_of(<<"PhysicalMaterial">>) -> <<"X3DMaterialNode">>;
x3d_sub_of(<<"UnlitMaterial">>) -> <<"X3DMaterialNode">>;

x3d_sub_of(<<"ImageTexture">>) -> <<"X3DTextureNode">>;
x3d_sub_of(<<"PixelTexture">>) -> <<"X3DTextureNode">>;
x3d_sub_of(<<"MovieTexture">>) -> <<"X3DTextureNode">>;
x3d_sub_of(<<"MultiTexture">>) -> <<"X3DTextureNode">>;

x3d_sub_of(<<"TextureTransform">>) -> <<"X3DTextureTransformNode">>;

x3d_sub_of(<<"Transform">>) -> <<"X3DGroupingNode">>;
x3d_sub_of(<<"X3DGroupingNode">>) -> <<"X3DChildNode">>;
x3d_sub_of(<<"Group">>) -> <<"X3DGroupingNode">>;
x3d_sub_of(<<"StaticGroup">>) -> <<"X3DChildNode">>;
x3d_sub_of(<<"Switch">>) -> <<"X3DGroupingNode">>;

x3d_sub_of(<<"Shape">>) -> <<"X3DShapeNode">>;
x3d_sub_of(<<"X3DShapeNode">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"Box">>) -> <<"X3DGeometryNode">>;
x3d_sub_of(<<"Cone">>) -> <<"X3DGeometryNode">>;
x3d_sub_of(<<"Cylinder">>) -> <<"X3DGeometryNode">>;
x3d_sub_of(<<"ElevationGrid">>) -> <<"X3DGeometryNode">>;
x3d_sub_of(<<"Extrusion">>) -> <<"X3DGeometryNode">>;
x3d_sub_of(<<"IndexedFaceSet">>) -> <<"X3DComposedGeometryNode">>;
x3d_sub_of(<<"Sphere">>) -> <<"X3DGeometryNode">>;

x3d_sub_of(<<"X3DComposedGeometryNode">>) -> <<"X3DGeometryNode">>;

x3d_sub_of(<<"X3DCoordinateNode">>) -> <<"X3DGeometricPropertyNode">>;

x3d_sub_of(<<"X3DNormalNode">>) -> <<"X3DGeometricPropertyNode">>;

x3d_sub_of(<<"ClipPlane">>) -> <<"X3DChildNode">>;

x3d_sub_of(<<"Color">>) -> <<"X3DColorNode">>;
x3d_sub_of(<<"ColorRGBA">>) -> <<"X3DColorNode">>;

x3d_sub_of(<<"Coordinate">>) -> <<"X3DCoordinateNode">>;

x3d_sub_of(<<"IndexedLineSet">>) -> <<"X3DGeometryNode">>;

x3d_sub_of(<<"IndexedTriangleFanSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(<<"IndexedTriangleSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(<<"IndexedTriangleStripSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(<<"LineSet">>) -> <<"X3DGeometryNode">>;

x3d_sub_of(<<"Normal">>) -> <<"X3DNormalNode">>;

x3d_sub_of(<<"TextureCoordinate">>) -> <<"X3DTextureCoordinateNode">>;

x3d_sub_of(<<"PointSet">>) -> <<"X3DGeometryNode">>;

x3d_sub_of(<<"TriangleFanSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(<<"TriangleSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(<<"TriangleStripSet">>) -> <<"X3DComposedGeometryNode">>;

x3d_sub_of(_) -> root.
 



%%
%% Parse X3D JSON
%%

read_x3djson_content(Bin_0) ->
    case Bin_0 of
        <<31,139,_/binary>> ->
            %% A gz header, the file is compressed.
            read_x3djson_content_1(zlib:gunzip(Bin_0));
        _ ->
            %% Uncompressed
            read_x3djson_content_1(Bin_0)
    end.
read_x3djson_content_1(C) ->
    {ok, x3djson_0(json_data(jsone:decode(C, [{object_format,tuple}])))}.

x3djson_0({a,[{o,[{Tag,{o,{a,C}}}]}|_]}) when is_list(C) ->
    x3djson_0_1(Tag, C);
x3djson_0({o,[{Tag,{o,C}}]}) when is_list(C) ->
    x3djson_0_1(Tag, C).

x3djson_0_1(Tag, C) ->
    case string:uppercase(binary_to_list(Tag)) =:= "X3D" of
        true ->
            {o,L}=proplists:get_value(<<"Scene">>, C, none),
            {a,CL} = proplists:get_value(<<"-children">>, L, none),
            [ case x3djson_tag(E) of
                {def,Name,{container,CTag,Cont}} ->
                    {def,Name,{CTag,Cont}};
                {use,Name} ->
                    {use,Name};
                {container,CTag,Cont} ->
                    {CTag,Cont}
              end || E <- CL ]
    end.

x3djson_tag({o,[{Tag,{o,L}}]}) ->
    x3djson_tag(Tag, L);
x3djson_tag({Tag,{o,L}}) ->
    x3djson_tag(Tag, L).
x3djson_tag(Tag, L) when is_list(L) ->
    x3djson_tag_1(Tag, L, none, []).
x3djson_tag_1(Tag, [], DefUse, OL) ->
    ContainerName = Tag,
    Fields = lists:reverse(OL),
    case DefUse of
        none ->
            {container, ContainerName, Fields};
        {def, Str} ->
            {def, Str, {container, ContainerName, Fields}};
        {use, Str} when Fields =:= [] ->
            {use, Str}
    end;
x3djson_tag_1(Tag, [{<<"@DEF">>,{s,C}}|L], _, OL) ->
    x3djson_tag_1(Tag, L, {def, C}, OL);
x3djson_tag_1(Tag, [{<<"@USE">>,{s,C}}|L], _, OL) ->
    x3djson_tag_1(Tag, L, {use, C}, OL);
%% Attribute value
x3djson_tag_1(Tag, [{<<"@",FName/binary>>,C_0}|L], DefUse, OL) ->
    OL_1 = [x3djson_attr(Tag, FName, C_0)|OL],
    x3djson_tag_1(Tag, L, DefUse, OL_1);
%% Containers
x3djson_tag_1(Tag, [{<<"-",FName/binary>>,C_0}|L], DefUse, OL) ->
    case C_0 of
        {a, ValL_0} when is_list(L) ->
            case remove_cmt_objs(ValL_0) of
                [] ->
                    x3djson_tag_1(Tag, L, DefUse, OL);
                ValL_1 when is_list(ValL_1) ->
                    ValL_2 = [x3djson_tag(E) || E <- ValL_1],
                    OL_1 = [
                        {{field, FName},
                         x3djson_tag_field_s_or_m(Tag, FName, ValL_2)}
                        | OL],
                    x3djson_tag_1(Tag, L, DefUse, OL_1)
            end;
        {o, _}=Val1 ->
            Val1_1 = x3djson_tag(Val1),
            OL_1 = [
                {{field, FName},
                 x3djson_tag_field_s_or_m(Tag, FName, [Val1_1])}
                | OL],
            x3djson_tag_1(Tag, L, DefUse, OL_1)
    end;
%% Comment
x3djson_tag_1(Tag, [{<<"#",_/binary>>,_}|L], DefUse, OL) ->
    x3djson_tag_1(Tag, L, DefUse, OL).
    

x3djson_attr(Tag, FName, Val) ->
    %% Determine type of field and whether to tokenize it.
    FieldType = expected_field_type(Tag, {word, FName}),
    case x3d_xatr_pt(Tag, FName, FieldType) of
        string ->
            %% Verbatim XML attribute not used by the X3D parser.
            {{user, FName}, {string, Val}};
        word ->
            %% Change the JSON value into a token list
            Tokens = json_to_tok(Val),
            case FieldType of
                {multival, _} ->
                    %% We want the field parser to use as much of the XML attribute as
                    %% possible, so add brackets around the tokens.
                    {ok, FA, []} = parse_field(FieldType,
                        {word, FName},
                        [open_bracket] ++ Tokens ++ [close_bracket]);
                _ ->
                    {ok, FA, []} = parse_field(FieldType,
                        {word, FName},
                        Tokens)
            end,
            FA
    end.

%% Change the JSON value into a token list that can be used
%% by parse_field/3.
%%
json_to_tok({s,Str}) ->
    [{string,Str}];
json_to_tok({a,List}) ->
    [json_to_tok_1(E) || E <- List];
json_to_tok({number,Number}) ->
    [{number,Number}].

json_to_tok_1({s,Str}) ->
    {string,Str};
json_to_tok_1({number,Number}) ->
    {number,Number}.

%% Remove comment "objects" in the X3D JSON file.
%%
remove_cmt_objs(L) ->
    remove_cmt_objs(L, []).
remove_cmt_objs([], OL) ->
    lists:reverse(OL);
remove_cmt_objs([{o,[{<<"#",_/binary>>,_}]}|L], OL) ->
    remove_cmt_objs(L, OL);
remove_cmt_objs([O|L], OL) ->
    remove_cmt_objs(L, [O|OL]).


x3djson_tag_field_s_or_m(ContT, FName, [Item|_]=List) ->
    case expected_field_type(ContT, {word, FName}) of
        {multival, _} -> {multival, List};
        _ -> Item
    end.


json_data({A}) ->
    {o,[{E1,json_data(E2)} || {E1,E2} <- A]};
json_data([_|_]=A) ->
    {a,[json_data(E) || E <- A]};
json_data(A) when is_binary(A) ->
    {s,binary_to_list(A)};
json_data(A) when is_number(A) ->
    {number,A};
json_data(false) ->
    {number,0};
json_data(true) ->
    {number,1}.


%%
%% VRML File
%%

read_vrml_content(Cont) ->
    case Cont of
        <<31,139,_/binary>> ->
            %% A gz header the file is compressed.
            read_vrml_content_1(zlib:gunzip(Cont));
        _ ->
            %% Uncompressed
            read_vrml_content_1(Cont)
    end.
read_vrml_content_1(Cont) ->
    [FirstLine, VRMLContent] = binary:split(Cont, <<10>>),
    case header(FirstLine) of
        {ok, vrml2, _Enc} ->
            {ok, VRMLTokens} = tok(strip_comments(VRMLContent)),
            parse(VRMLTokens);
            
        {ok, vrml1, _Enc} ->
            %% A VRML 1.0 file was detected.
            
            %% Tokenized the same way but nodes are afterwards rearranged instead
            %% so it fits VRML 2.0 for parsing.
            {ok, VRMLTokens_VRML1} = tok(strip_comments(VRMLContent)),
            {ok, VRMLTokens} = rearrange_v1_to_v2(VRMLTokens_VRML1),
            
            parse(VRMLTokens);
            
        {ok, noheader, _Enc} ->
            %% There was no header, so we'll assume VRML 2.0
            {ok, VRMLTokens} = tok(strip_comments(Cont)),
            parse(VRMLTokens);
        error ->
            error
    end.
    
%%
%% VRML preprocessing of comments
%%

%% Remove comments from VRML code
strip_comments(Content) -> strip_comments(Content, []).
strip_comments(<<>>, AL) -> iolist_to_binary(lists:reverse(AL));
strip_comments(Content, AL) ->
    case binary:split(Content, <<34>>) of
        [BeforeString, InString] -> 
            case strip_comments_outside_string(BeforeString) of
                {no_comment, BeforeString_1} ->
                    {InString_2, Rest_2} = strip_comments_inside_string(InString),
                    strip_comments(Rest_2, [<<34>>,InString_2,<<34>>,BeforeString_1|AL]);
                {had_comment, BeforeString_1} ->
                    %% If the previous part of the source has a comment
                    %% then the double quote inside the comment doesn't carry to the next line.
                    case binary:split(InString, <<10>>) of
                        [_] -> error;
                        [_CommentedString, NextLine] ->
                            strip_comments(NextLine, [BeforeString_1|AL])
                    end
            end;
        [_NoString] ->
            {_, Rest_1} = strip_comments_outside_string(Content),
            iolist_to_binary(lists:reverse([Rest_1 | AL]))
    end.
strip_comments_inside_string(Content) -> strip_comments_inside_string(Content, []).

strip_comments_inside_string(<<>>, AL) ->
    {iolist_to_binary(lists:reverse(AL)), <<>>};
strip_comments_inside_string(<<BS:8, EscChar:8, Rest/binary>>, AL) when BS =:= 92 ->
    strip_comments_inside_string(Rest, [EscChar,BS|AL]);
strip_comments_inside_string(<<DQ:8, Rest/binary>>, AL) when DQ =:= 34 ->
    {iolist_to_binary(lists:reverse(AL)), Rest};
strip_comments_inside_string(<<Char:8, Rest/binary>>, AL) ->
    strip_comments_inside_string(Rest, [Char|AL]).

strip_comments_outside_string(Content) -> strip_comments_outside_string(Content, []).
strip_comments_outside_string(Content, AL) ->
    case binary:split(Content, <<10>>) of
        [ThisLine, R] ->
            case binary:split(ThisLine, <<$#>>) of
                [Keep, _] -> strip_comments_outside_string(R, [<<10>>,Keep | AL]);
                [Keep]    -> strip_comments_outside_string(R, [<<10>>,Keep | AL])
            end;
        [LastLine] ->
            %% Only the last line matters whether a comment character appears
            %% which overrides the string afterwards.
            case binary:split(LastLine, <<$#>>) of
                [Keep, _] -> {had_comment, iolist_to_binary(lists:reverse([<<10>>,Keep|AL]))};
                [Keep]    -> {no_comment,  iolist_to_binary(lists:reverse([<<10>>,Keep|AL]))}
            end
    end.

%%
%% VRML and X3D tokenizer
%%

tok(A) -> tok(A, [], []).
tok(<<>>, [], Toks) -> {ok, lists:reverse(Toks)};
tok(<<>>, Current, Toks) -> tok(<<>>, [], [tok_w(Current)|Toks]);
tok(<<WhiteSpace:8, Rest/binary>>, Current, Toks)
    when WhiteSpace =:= 32;
         WhiteSpace =:= 10; WhiteSpace =:= 13;
         WhiteSpace =:= 9 ->
    case Current of
        [] -> tok(Rest, Current, Toks);
        _  -> tok(Rest, [], [tok_w(Current)|Toks])
    end;
tok(<<DQ:8, Rest_0/binary>>, Current, Toks)
    when DQ =:= 34 ->
    {InsideString, Rest_1} = tok_inside_string(Rest_0),
    case Current of
        [] -> tok(Rest_1, Current, [{string, InsideString} | Toks]);
        _  -> tok(Rest_1, [], [{string, InsideString}, tok_w(Current)|Toks])
    end;
tok(<<SC:8, Rest/binary>>, Current, Toks)
    when SC =:= ${; SC =:= $};
         SC =:= $(; SC =:= $);
         SC =:= $[; SC =:= $];
         SC =:= $, ->
    case Current of
        [] -> tok(Rest, Current, [type_tok(SC)|Toks]);
        _  -> tok(Rest, [], [type_tok(SC),tok_w(Current)|Toks])
    end;
tok(<<Char:8, Rest/binary>>, Current, Toks) ->
    tok(Rest, [Char|Current], Toks).


tok_inside_string(A) ->
    tok_inside_string(A, []).
tok_inside_string(<<>>, AL) ->
    {unicode:characters_to_list(iolist_to_binary(lists:reverse(AL)), utf8), <<>>};
tok_inside_string(<<BS:8, EscChar:8, Rest/binary>>, AL) when BS =:= 92 ->
    tok_inside_string(Rest, [EscChar|AL]);
tok_inside_string(<<DQ:8, Rest/binary>>, AL) when DQ =:= 34 ->
    {unicode:characters_to_list(iolist_to_binary(lists:reverse(AL)), utf8), Rest};
tok_inside_string(<<Char:8, Rest/binary>>, AL) ->
    tok_inside_string(Rest, [Char|AL]).
    
tok_w(A) ->
    word_or_number(iolist_to_binary(lists:reverse(A))).
    
word_or_number(<<B:8,_/binary>>=A)
    when B >= $0, B =< $9; B =:= $.; B =:= $-
    -> {number, parsenumber(A)};
word_or_number(A) -> {word, A}.

parsenumber(A) ->
    parsenumber_1(binary_to_list(A)).
parsenumber_1(Num_S) ->
    case string:split(Num_S, "0x") of
        [[], RHexNum_S] ->
            parsenumber_1_hex(RHexNum_S);
        [_] -> parsenumber_2(Num_S)
    end.
parsenumber_1_hex(A) -> parsenumber_1_hex(A, 0).
parsenumber_1_hex([], B) -> B;
parsenumber_1_hex([A | HR], B)
    when A >= $0, A =< $9
    -> parsenumber_1_hex(HR, (A - $0) + (B bsl 4));
parsenumber_1_hex([A | HR], B)
    when A >= $a, A =< $f
    -> parsenumber_1_hex(HR, (A - $a + 10) + (B bsl 4));
parsenumber_1_hex([A | HR], B)
    when A >= $A, A =< $F
    -> parsenumber_1_hex(HR, (A - $A + 10) + (B bsl 4));
parsenumber_1_hex(_, _) -> 0.
parsenumber_2([$.|R]) -> parsenumber_2([$0,$.|R]);
parsenumber_2(Num_S) ->
    case string:split(Num_S, "e") of
        [LExpNum_S, RExpNum_S] ->
            LExpNum = case string:to_float(LExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(LExpNum_S) of
                        {error, _} -> 0;
                        {Num_1, _} -> Num_1
                    end;
                {Num_1, _} -> Num_1
            end,
            RExpNum = case string:to_float(RExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(RExpNum_S) of
                        {error, _} -> 0;
                        {Num_2, _} -> Num_2
                    end;
                {Num_2, _} -> Num_2
            end,
            LExpNum * math:pow(10, RExpNum);
        [_NoExponent] ->
            case string:to_float(Num_S) of
                {error, no_float} -> 
                    case string:to_integer(Num_S) of
                        {error, _} -> 0;
                        {Num_1, _} -> Num_1
                    end;
                {Num_1, _} -> Num_1
            end
    end.

type_tok($,) -> comma;
type_tok(${) -> open_curly;
type_tok($}) -> close_curly;
type_tok($[) -> open_bracket;
type_tok($]) -> close_bracket;
type_tok($() -> open_paren;
type_tok($)) -> close_paren.

%%
%% VRML Header reader
%%

%% Header
header(<<"#VRML ", Rest_0/binary>>) ->
    %% Detect if VRML 1.0 or VRML 2.0 (97) because they are very different
    case header_version(Rest_0) of
        {ok, <<"V1.0">>, Rest_1} ->
            {ok, Encoding} = header_encoding(Rest_1),
            {ok, vrml1, Encoding};
        {ok, <<"V2.0">>, Rest_1} ->
            {ok, Encoding} = header_encoding(Rest_1),
            {ok, vrml2, Encoding};
        _ ->
            Encoding = <<"utf-8">>,
            {ok, noheader, Encoding}
    end;
header(<<"#X3D ", Rest_0/binary>>) ->
    {ok, _, Rest_1} = header_version(Rest_0),
    {ok, Encoding} = header_encoding(Rest_1),
    {ok, vrml2, Encoding};
header(<<"#Inventor ", Rest_0/binary>>) ->
    %% Try using the VRML 1.0 parser when this file header is encountered.
    case header_version(Rest_0) of
        {ok, <<"V1.0">>, Rest_1} ->
            {ok, Encoding} = header_encoding(Rest_1),
            {ok, vrml1, Encoding};
        {ok, <<"V2.0">>, Rest_1} ->
            {ok, Encoding} = header_encoding(Rest_1),
            {ok, vrml1, Encoding};
        {ok, _, _Rest_1} ->
            Encoding = <<"ascii">>,
            {ok, vrml1, Encoding}
    end;
header(_) ->
    error.

header_version(<<A:8, R/binary>>) when A =:= 32; A =:= 9 ->
    header_version(R);
header_version(A) ->
    header_version(A, []).
header_version(<<A:8, R/binary>>, B) when A >= $A, A =< $Z; A >= $a, A =< $z; A >= $0, A =< $9; A =:= $. ->
    header_version(R, [A | B]);
header_version(R, B) ->
    {ok, iolist_to_binary(lists:reverse(B)), R}.

header_encoding(<<A:8, R/binary>>) when A =:= 32; A =:= 9 ->
    header_encoding(R);
header_encoding(A) ->
    header_encoding(A, []).
header_encoding(<<A:8, R/binary>>, B) when A >= $A, A =< $Z; A >= $a, A =< $z; A >= $0, A =< $9 ->
    header_encoding(R, [A | B]);
header_encoding(_, B) ->
    {ok, iolist_to_binary(lists:reverse(B))}.


%%
%% VRML 2.0 Parser
%%


%%
%% VRML Container Parser
%%

parse(T) -> parse(T, []).
parse(T, Cont) ->
    %% Top level
    case T of
        [] -> {ok, lists:reverse(Cont)};
        [{word, <<"DEF">>}, {word, Variable}, {word, Word}, open_curly|Rest0] ->
            {ok, InnerContent, Rest1} = parse_container(Word, Rest0),
            Variable_S = unicode:characters_to_list(Variable,utf8),
            parse(Rest1, [{def, Variable_S, {Word, InnerContent}} | Cont]);
        [{word, Word}, open_curly|Rest0] ->
            {ok, InnerContent, Rest1} = parse_container(Word, Rest0),
            parse(Rest1, [{Word, InnerContent} | Cont]);
        [{word,<<"ROUTE">>}, {word,_}, {word,<<"TO">>}, {word, _} | Rest0] ->
            parse(Rest0, Cont);
        [{word,<<"PROTO">>}, {word,ProtoName} | Rest0] ->
            {ok, Rest1} = parse_skip_proto(ProtoName,Rest0),
            parse(Rest1, Cont);
        [{word,<<"EXTERNPROTO">>}, {word,ExtProtoName} | Rest0] ->
            {ok, Rest1} = parse_skip_externproto(ExtProtoName, Rest0),
            parse(Rest1, Cont);
        [{word,<<"PROFILE">>},{word,_}|Rest0] ->
            parse(Rest0, Cont);
        [{word,<<"META">>},{string,_},{string,_}|Rest0] ->
            parse(Rest0, Cont)

    end.

parse_container(ContainerType, T) -> parse_container(ContainerType, T, []).
parse_container(ContainerType, T, Contents) ->
    case T of
        [close_curly | Rest] -> {ok, lists:reverse(Contents), Rest};
        [{word, <<"DEF">>}, {word, Variable}, {word, Word}, open_curly | Rest0] ->
            %% Named container.
            {ok, InnerContents, Rest1} = parse_container(Word, Rest0),
            Variable_S = unicode:characters_to_list(Variable, utf8),
            parse_container(ContainerType, Rest1,
                [{def, Variable_S, {Word, InnerContents}}|Contents]);
        [{word, Word} |[open_curly|Rest0]] ->
            %% container.
            {ok, InnerContents, Rest1} = parse_container(Word, Rest0),
            parse_container(ContainerType, Rest1,
                [{Word, InnerContents}|Contents]);
        [FStart | Rest0] ->
            %% Field
            FieldType = expected_field_type(ContainerType, FStart),
            {ok, FA, Rest1} = parse_field(FieldType, FStart, Rest0),
            parse_container(ContainerType, Rest1,
                [FA|Contents])
    end.

%%
%% VRML and X3D Field Parser
%%

%% Because VRML some delimiter syntax is optional, we need to know the
%% type of the field of a node (container) to be able to parse effectively
%% the numbers for that field.
%%
%% For simplicity when parsing, color uses the same type as vec3,
%% bool and int32 uses the same type as float
%%
expected_field_type(<<"Shape">>, {word, F}) ->
    case F of
        <<"appearance">> -> container;
        <<"geometry">> -> container;
        
        _ -> any
    end;
expected_field_type(<<"Transform">>, {word, F}) ->
    case F of
        <<"center">> -> vec3;
        <<"children">> -> {multival, container};
        <<"rotation">> -> rotation;
        <<"scale">> -> vec3;
        <<"scaleOrientation">> -> rotation;
        <<"translation">> -> vec3;
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;
        _ -> any
    end;
expected_field_type(<<"Group">>, {word, F}) ->
    case F of
        <<"children">> -> {multival, container};
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;
        _ -> any
    end;
expected_field_type(<<"Anchor">>, {word, F}) ->
    case F of
        <<"children">> -> {multival, container};
        <<"description">> -> string;
        <<"parameter">> -> {multival, string};
        <<"url">> -> {multival, string};
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;

        _ -> any
    end;
expected_field_type(<<"Billboard">>, {word, F}) ->
    case F of
        <<"axisOfRotation">> -> vec3;
        <<"children">> -> {multival, container};
        <<"metadata">> -> {multival, container};
        <<"visible">> -> float;
        <<"bboxDisplay">> -> float;
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;
        _ -> any
    end;
expected_field_type(<<"Collision">>, {word, F}) ->
    case F of
        <<"children">> -> {multival, container};
        <<"collide">> -> float;
        <<"enabled">> -> float;
        <<"metadata">> -> {multival, container};
        <<"visible">> -> float;
        <<"bboxDisplay">> -> float;
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;
        <<"proxy">> -> container;
        
        _ -> any
    end;
expected_field_type(<<"Inline">>, {word, F}) ->
    case F of
        <<"url">> -> {multival, string};
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;

        _ -> any
    end;
expected_field_type(<<"LOD">>, {word, F}) ->
    case F of
        <<"children">> -> {multival, container};
        <<"level">> -> {multival, container}; % VRML 97 version of LOD has children in level
        <<"center">> -> vec3;
        <<"range">> -> {multival, float};
        <<"metadata">> -> {multival, container};
        <<"visible">> -> float;
        <<"forceTransitions">> -> float;
        <<"bboxDisplay">> -> float;
        <<"bboxCenter">> -> vec3;
        <<"bboxSize">> -> vec3;
        _ -> any
    end;
expected_field_type(<<"Switch">>, {word, F}) ->
    case F of 
        <<"choice">> -> {multival, container};
        <<"whichChoice">> -> float;
        _ -> any
    end;

expected_field_type(<<"IndexedFaceSet">>, {word, F}) ->
    case F of
        <<"color">> -> container;
        <<"coord">> -> container;
        <<"normal">> -> container;
        <<"texCoord">> -> container;
        <<"ccw">> -> float;
        <<"colorIndex">> -> {multival, float};
        <<"colorPerVertex">> -> float;
        <<"convex">> -> float;
        <<"coordIndex">> -> {multival, float};
        <<"creaseAngle">> -> float;
        <<"normalIndex">> -> {multival, float};
        <<"normalPerVertex">> -> float;
        <<"solid">> -> float;
        <<"texCoordIndex">> -> {multival, float};
        <<"fogCoord">> -> container;
        <<"attrib">> -> {multival, container};
        <<"metadata">> -> container;
        _ -> any
    end;

expected_field_type(<<"Box">>, {word, F}) ->
    case F of
        <<"size">> -> vec3;
        <<"solid">> -> float;
        <<"metadata">> -> container;

        _ -> any
    end;
    
expected_field_type(<<"Cone">>, {word, F}) ->
    case F of
        <<"bottomRadius">> -> float;
        <<"height">> -> float;
        <<"side">> -> float;
        <<"bottom">> -> float;
        <<"solid">> -> float;
        <<"metadata">> -> container;

        _ -> any
    end;
    
expected_field_type(<<"Cylinder">>, {word, F}) ->
    case F of
        <<"bottom">> -> float;
        <<"height">> -> float;
        <<"radius">> -> float;
        <<"side">> -> float;
        <<"top">> -> float;
        <<"solid">> -> float;
        <<"metadata">> -> container;
        _ -> any
    end;
    
expected_field_type(<<"Sphere">>, {word, F}) ->
    case F of
        <<"radius">> -> float;
        <<"solid">> -> float;
        <<"metadata">> -> container;
        _ -> any
    end;
    
expected_field_type(<<"ElevationGrid">>, {word, F}) ->
    case F of
        <<"color">> -> container;
        <<"normal">> -> container;
        <<"texCoord">> -> container;
        <<"height">> -> {multival, float};
        <<"ccw">> -> float;
        <<"colorPerVertex">> -> float;
        <<"creaseAngle">> -> float;
        <<"normalPerVertex">> -> float;
        <<"solid">> -> float;
        <<"xDimension">> -> float;
        <<"xSpacing">> -> float;
        <<"zDimension">> -> float;
        <<"zSpacing">> -> float;
        <<"attrib">> -> {multival, container};
        <<"fogCoord">> -> container;
        <<"metadata">> -> container;
        _ -> any
    end;
    
expected_field_type(<<"Extrusion">>, {word, F}) ->
    case F of
        <<"beginCap">> -> float;
        <<"ccw">> -> float;
        <<"convex">> -> float;
        <<"creaseAngle">> -> float;
        <<"crossSection">> -> {multival, vec2};
        <<"endCap">> -> float;
        <<"orientation">> -> {multival, rotation};
        <<"scale">> -> {multival, vec2};
        <<"solid">> -> float;
        <<"spine">> -> {multival, vec3};
        <<"metadata">> -> container;
        _ -> any
    end;
    
expected_field_type(<<"Appearance">>, {word, F}) ->
    case F of
        <<"material">> -> container;
        <<"texture">> -> container;
        <<"textureTransform">> -> container;
        _ -> any
    end;
expected_field_type(<<"Material">>, {word, F}) ->
    case F of
        <<"ambientIntensity">> -> float;
        <<"diffuseColor">> -> vec3;
        <<"emissiveColor">> -> vec3;
        <<"shininess">> -> float;
        <<"specularColor">> -> vec3;
        <<"transparency">> -> float;
        _ -> any
    end;
expected_field_type(<<"Coordinate">>, {word, <<"point">>}) ->
    {multival, vec3};
expected_field_type(<<"TextureCoordinate">>, {word, <<"point">>}) ->
    {multival, vec2};
expected_field_type(<<"TextureTransform">>, {word, F}) ->
    case F of
        <<"center">> -> vec2;
        <<"rotation">> -> float;
        <<"scale">> -> vec2;
        <<"translation">> -> vec2;
        _ -> any
    end;
expected_field_type(<<"Color">>, {word, <<"color">>}) ->
    {multival, vec3};
expected_field_type(<<"Normal">>, {word, <<"vector">>}) ->
    {multival, vec3};


expected_field_type(<<"ImageTexture">>, {word, F}) ->
    case F of
        <<"url">> -> {multival, string};
        <<"repeatS">> -> float;
        <<"repeatT">> -> float;
        
        _ -> any
    end;
expected_field_type(<<"PixelTexture">>, {word, F}) ->
    case F of
        <<"image">> -> image;
        <<"repeatS">> -> float;
        <<"repeatT">> -> float;
        _ -> any
    end;

expected_field_type(<<"DirectionalLight">>, {word, F}) ->
    case F of
        <<"ambientIntensity">> -> float;
        <<"color">> -> vec3;
        <<"direction">> -> vec3;
        <<"intensity">> -> float;
        <<"on">> -> float;

        _ -> any
    end;
expected_field_type(<<"PointLight">>, {word, F}) ->
    case F of
        <<"ambientIntensity">> -> float;
        <<"attenuation">> -> vec3;
        <<"color">> -> vec3;
        <<"intensity">> -> float;
        <<"location">> -> vec3;
        <<"on">> -> float;
        <<"radius">> -> float;
        _ -> any
    end;
expected_field_type(<<"SpotLight">>, {word, F}) ->
    case F of
        <<"ambientIntensity">> -> float;
        <<"attenuation">> -> vec3;
        <<"beamWidth">> -> float;
        <<"color">> -> vec3;
        <<"cutOffAngle">> -> float;
        <<"direction">> -> vec3;
        <<"intensity">> -> float;
        <<"location">> -> vec3;
        <<"on">> -> float;
        <<"radius">> -> float;

        _ -> any
    end;
    

expected_field_type(<<"NavigationInfo">>, {word, F}) ->
    case F of
        <<"avatarSize">> -> {multival, float};
        <<"headlight">> -> float;
        <<"metadata">> -> container;
        <<"speed">> -> float;
        <<"transitionTime">> -> float;
        <<"transitionType">> -> {multival, string};
        <<"type">> -> {multival, string};
        <<"visibilityLimit">> -> float;
        _ -> any
    end;


expected_field_type(<<"OrthoViewpoint">>, {word, F}) ->
    case F of
        <<"centerOfRotation">> -> vec3;
        <<"description">> -> string;
        <<"farDistance">> -> float;
        <<"fieldOfView">> -> {multival, float};
        <<"jump">> -> float;
        <<"metadata">> -> container;
        <<"navigationInfo">> -> container;
        <<"nearDistance">> -> float;
        <<"orientation">> -> rotation;
        <<"position">> -> vec3;
        <<"retainUserOffsets">> -> float;
        <<"viewAll">> -> float;
        _ -> any
    end;

expected_field_type(<<"Viewpoint">>, {word, F}) ->
    case F of
        <<"centerOfRotation">> -> vec3;
        <<"description">> -> string;
        <<"farDistance">> -> float;
        <<"fieldOfView">> -> float;
        <<"jump">> -> float;
        <<"metadata">> -> container;
        <<"navigationInfo">> -> container;
        <<"nearDistance">> -> float;
        <<"orientation">> -> rotation;
        <<"position">> -> vec3;
        <<"retainUserOffsets">> -> float;
        <<"viewAll">> -> float;
        _ -> any
    end;

expected_field_type(<<"ViewpointGroup">>, {word, F}) ->
    case F of
        <<"center">> -> vec3;
        <<"children">> -> {multival, container};
        <<"description">> -> string;
        <<"displayed">> -> float;
        <<"metadata">> -> container;
        <<"retainUserOffsets">> -> float;
        <<"size">> -> vec3;
        _ -> any
    end;

expected_field_type(<<"Background">>, {word, F}) ->
    case F of
        <<"groundAngle">> -> {multival, float};
        <<"groundColor">> -> {multival, vec3};
        <<"backUrl">> -> {multival, string};
        <<"bottomUrl">> -> {multival, string};
        <<"frontUrl">> -> {multival, string};
        <<"leftUrl">> -> {multival, string};
        <<"metadata">> -> container;
        <<"rightUrl">> -> {multival, string};
        <<"topUrl">> -> {multival, string};
        <<"skyAngle">> -> {multival, float};
        <<"skyColor">> -> {multival, vec3};
        <<"transparency">> -> float;
        _ -> any
    end;

expected_field_type(<<"Fog">>, {word, F}) ->
    case F of
        <<"color">> -> vec3;
        <<"fogType">> -> string;
        <<"metadata">> -> container;
        <<"visibilityRange">> -> float;
        _ -> any
    end;

expected_field_type(<<"FogCoordinate">>, {word, F}) ->
    case F of
        <<"depth">> -> {multival, float};
        <<"metadata">> -> container;
        _ -> any
    end;
    
expected_field_type(<<"LocalFog">>, {word, F}) ->
    case F of
        <<"color">> -> vec3;
        <<"enabled">> -> float;
        <<"fogType">> -> string;
        <<"metadata">> -> container;
        <<"visibilityRange">> -> float;
        _ -> any
    end;

expected_field_type(<<"TextureBackground">>, {word, F}) ->
    case F of
        <<"groundAngle">> -> {multival, float};
        <<"groundColor">> -> {multival, vec3};
        <<"backTexture">> -> container;
        <<"bottomTexture">> -> container;
        <<"frontTexture">> -> container;
        <<"leftTexture">> -> container;
        <<"metadata">> -> container;
        <<"rightTexture">> -> container;
        <<"topTexture">> -> container;
        <<"skyAngle">> -> {multival, float};
        <<"skyColor">> -> {multival, vec3};
        <<"transparency">> -> float;
        _ -> any
    end;
    
expected_field_type(<<"WorldInfo">>, {word, F}) ->
    case F of
        <<"metadata">> -> container;
        <<"info">> -> {multival, string};
        <<"title">> -> string;
        _ -> any
    end;


expected_field_type(<<"X3DViewpointNode">>, {word, F}) ->
    case F of
        <<"centerOfRotation">> -> vec3;
        <<"description">> -> string;
        <<"farDistance">> -> float;
        <<"jump">> -> float;
        <<"metadata">> -> container;
        <<"navigationInfo">> -> container;
        <<"nearDistance">> -> float;
        <<"orientation">> -> rotation;
        <<"position">> -> vec3;
        <<"retainUserOffsets">> -> float;
        <<"viewAll">> -> float;
        _ -> any
    end;

expected_field_type(<<"X3DBackgroundNode">>, {word, F}) ->
    case F of
        <<"groundAngle">> -> {multival, float};
        <<"groundColor">> -> {multival, vec3};
        <<"metadata">> -> container;
        <<"skyAngle">> -> {multival, float};
        <<"skyColor">> -> {multival, vec3};
        <<"transparency">> -> float;
        _ -> any
    end;

expected_field_type(<<"X3DFogObject">>, {word, F}) ->
    case F of
        <<"color">> -> vec3;
        <<"fogType">> -> string;
        <<"visibilityRange">> -> float;
        _ -> any
    end;

expected_field_type(<<"X3DBindableNode">>, {word, F}) ->
    case F of
        <<"metadata">> -> container;
        _ -> any
    end;

expected_field_type(<<"X3DInfoNode">>, {word, F}) ->
    case F of
        <<"metadata">> -> container;
        _ -> any
    end;

expected_field_type(<<"X3DMetadataObject">>, {word, F}) ->
    case F of
        <<"name">> -> string;
        <<"reference">> -> string;
        _ -> any
    end;

expected_field_type(<<"X3DPrototypeInstance">>, {word, F}) ->
    case F of
        <<"metadata">> -> container;
        _ -> any
    end;

expected_field_type(<<"X3DSensorNode">>, {word, F}) ->
    case F of
        <<"enabled">> -> float;
        <<"metadata">> -> container;
        _ -> any
    end;




expected_field_type(_, _) ->
    any.

parse_field(FT, {word, Word}, [{word, WordC}, open_curly | Rest0])
    when FT =:= any; FT =:= container; FT =:= {multival, container} ->
    %% Container as value
    {ok, InnerContents, Rest1} = parse_container(WordC, Rest0),
    {ok, {{field, Word}, singleval_wrap(FT, {container, WordC, InnerContents})}, Rest1};
parse_field(FT, {word, Word}, [{word, <<"DEF">>}, {word, Variable}, {word, WordC}, open_curly | Rest0])
    when FT =:= any; FT =:= container; FT =:= {multival, container} ->
    %% Container as value
    {ok, InnerContents, Rest1} = parse_container(WordC, Rest0),
    Variable_S = unicode:characters_to_list(Variable, utf8),
    {ok, {{field, Word},
          {def, Variable_S, singleval_wrap(FT, {container, WordC, InnerContents})}}, Rest1};
parse_field(FT, {word, Word}, [{word, <<"USE">>}, {word, Variable} | Rest0])
    when FT =:= any; FT =:= container; FT =:= {multival, container} ->
    %% Container value recall
    Variable_S = unicode:characters_to_list(Variable, utf8),
    {ok, {{field, Word}, {use, Variable_S}}, Rest0};
parse_field(FT, {word, Word}, [{string, String} | Rest0])
    when FT =:= any; FT =:= string; FT =:= {multival, string} ->
    %% String
    {ok, {{field, Word}, singleval_wrap(FT, {string, String})}, Rest0};
parse_field(FT, {word, Word}, [open_bracket | Rest0])
    when FT =:= any; element(1, FT) =:= multival ->
    %% Multival
    case FT of
        any ->
            {ok, MV, Rest} = get_multival(any, Rest0);
        {multival, FT_1} ->
            {ok, MV, Rest} = get_multival(FT_1, Rest0)
    end,
    {ok, {{field, Word}, {multival, MV}}, Rest};

parse_field(FT, {word, Word}, [
    {number, Width}, {number, Height}, {number, NumComponents} | Rest])
    when FT =:= image; FT =:= {multival, image} ->
    %% Image numbers
    {ImageData, Rest_1} = parse_image_data(Width*Height, NumComponents, Rest),
    {ok, {{field, Word}, singleval_wrap(FT, {image, Width, Height, NumComponents, ImageData})}, Rest_1};
parse_field(FT, {word, Word}, [{number, N1}, {number, N2}, {number, N3}, {number, N4} | Rest])
    when FT =:= rotation; FT =:= {multival, rotation} ->
    %% Rotation
    {ok, {{field, Word}, singleval_wrap(FT, {rotation, N1, N2, N3, N4})}, Rest};
parse_field(FT, {word, Word}, [{number, N1}, {number, N2}, {number, N3} | Rest])
    when FT =:= any; FT =:= vec3; FT =:= {multival, vec3} ->
    %% Vec3
    {ok, {{field, Word}, singleval_wrap(FT, {vec3, N1, N2, N3})}, Rest};
parse_field(FT, {word, Word}, [{number, N1}, {number, N2} | Rest])
    when FT =:= any; FT =:= vec2; FT =:= {multival, vec2} ->
    %% Vec2
    {ok, {{field, Word}, singleval_wrap(FT, {vec2, N1, N2})}, Rest};
parse_field(FT, {word, Word}, [{number, N1} | Rest])
    when FT =:= any; FT =:= float; FT =:= {multival, float} ->
    %% Float
    {ok, {{field, Word}, singleval_wrap(FT, {float, N1})}, Rest};
parse_field(FT, {word, Word}, [{word, BoolF} | Rest])
    when BoolF =:= <<"false">> orelse BoolF =:= <<"FALSE">>,
         FT =:= any orelse FT =:= float orelse FT =:= {multival, float}
    -> {ok, {{field, Word}, singleval_wrap(FT, {float, 0})}, Rest};
parse_field(FT, {word, Word}, [{word, BoolT} | Rest])
    when BoolT =:= <<"true">> orelse BoolT =:= <<"TRUE">>,
         FT =:= any orelse FT =:= float orelse FT =:= {multival, float}
    -> {ok, {{field, Word}, singleval_wrap(FT, {float, 1})}, Rest};
    
parse_field(_Any, _, Rest) ->
    {ok, unimp, Rest}.
    
%% If there is a single value of a multival the brackets are optional.
singleval_wrap({multival, _}, V) ->
    {multival, [V]};
singleval_wrap(_, V) ->
    V.

    
get_multival(FT, A) -> get_multival(FT, A, []).
get_multival(FT, [{word,<<"DEF">>},{word, Variable} | Rest], Cont) ->
    {ok, {{field, _}, FA}, Rest_1} = parse_field(FT, {word, <<"_">>}, Rest),
    Variable_S = unicode:characters_to_list(Variable, utf8),
    get_multival(FT, Rest_1, [{def, Variable_S, FA} | Cont]);
get_multival(FT, [{word,<<"USE">>},{word, Variable} | Rest], Cont) ->
    Variable_S = unicode:characters_to_list(Variable),
    get_multival(FT, Rest, [{use, Variable_S}|Cont]);
get_multival(FT, [comma | Rest], Cont) -> get_multival(FT, Rest, Cont);
get_multival(FT, [{word, WordC}, open_curly |Rest0], Cont)
    when FT =:= any; FT =:= container ->
    %% Container as value
    {ok, InnerContents, Rest1} = parse_container(WordC, Rest0),
    get_multival(FT, Rest1, [{container, WordC, InnerContents}|Cont]);

get_multival(_FT, [close_bracket | Rest], Cont) ->
    {ok, lists:reverse(Cont), Rest};
get_multival(rotation, [{number, N1}, {number, N2}, {number, N3}, {number, N4}|Rest], Cont) ->
    get_multival(rotation, Rest, [{rotation, N1, N2, N3, N4}|Cont]);
get_multival(FT, [{number, N1}, {number, N2}, {number, N3}|Rest], Cont)
    when FT =:= any; FT =:= vec3 ->
    get_multival(FT, Rest, [{vec3, N1, N2, N3}|Cont]);
get_multival(FT, [{number, N1}, {number, N2}|Rest], Cont)
    when FT =:= any; FT =:= vec2 ->
    get_multival(FT, Rest, [{vec2, N1, N2}|Cont]);
get_multival(FT, [{number, N1} |Rest], Cont)
    when FT =:= any; FT =:= float ->
    get_multival(FT, Rest, [{float, N1}|Cont]);
get_multival(FT, [{string, N1} |Rest], Cont)
    when FT =:= any; FT =:= string ->
    get_multival(FT, Rest, [{string, N1}|Cont]);
get_multival(FT, [{word, BoolF} |Rest], Cont)
    when BoolF =:= <<"false">>; BoolF =:= <<"FALSE">> ->
    get_multival(FT, Rest, [{float, 1}|Cont]);
get_multival(FT, [{word, BoolT} |Rest], Cont)
    when BoolT =:= <<"true">>; BoolT =:= <<"TRUE">> ->
    get_multival(FT, Rest, [{float, 0}|Cont]);
get_multival(any, [T|Rest], Cont) ->
    get_multival(any, Rest, [T|Cont]).
    
    
parse_image_data(N, NumComponents, Rest) ->
    parse_image_data(N, NumComponents, Rest, []).
parse_image_data(N, NumComponents, [{number, A} | R], Res) when N > 0 ->
    parse_image_data(N-1, NumComponents, R, [A | Res]);
parse_image_data(0, _, Rest, Res) ->
    {lists:reverse(Res), Rest}.
    


%%
%% VRML and X3D Traversal
%%

def_or_use_var({def, VarName, {container, <<"Material">>=A, AField}}) ->
    ContentValue = {container, A, [{Key, def_or_use_var_field(FVal)} || {Key, FVal} <- AField]},
    ets:insert(?MODULE, {{wrlvar, VarName}, {material, ContentValue}}),
    io:format("X3D:def material ~s~n", [to_iofmt(VarName)]),
    ContentValue;
def_or_use_var({def, VarName, {container, A, AField}}) ->
    ContentValue = {container, A, [{Key, def_or_use_var_field(FVal)} || {Key, FVal} <- AField]},
    ets:insert(?MODULE, {{wrlvar, VarName}, {misc, ContentValue}}),
    io:format("X3D:def ~s~n", [to_iofmt(VarName)]),
    ContentValue;
def_or_use_var({def, VarName, A}) ->
    ets:insert(?MODULE, {{wrlvar, VarName}, {misc, A}}),
    io:format("X3D:def ~s~n", [to_iofmt(VarName)]),
    A;
def_or_use_var({use, VarName}) ->
    io:format("X3D:use ~s~n", [to_iofmt(VarName)]),
    case ets:lookup(?MODULE, {wrlvar, VarName}) of
        [] ->
            none;
        [{_, {misc, Content}}] ->
            Content;
        [{_, {material, Content}}] ->
            Content
    end;
def_or_use_var({container, A, AField}) ->
    {container, A, [{Key, def_or_use_var_field(FVal)} || {Key, FVal} <- AField]};
def_or_use_var(A) ->
    A.
    
def_or_use_var_field({multival, List}) ->
    {multival, [def_or_use_var(Itm) || Itm <- List]};
def_or_use_var_field(Itm) ->
    def_or_use_var(Itm).
    
to_iofmt(A) ->
    %% io:format errors on >127 code point strings
    unicode:characters_to_nfc_binary(A).
    

%% At the top level we'll expect there to be Shape, Transform and Group nodes
%% 
trav({<<"Transform">>, Fields}) ->
    trav({container,<<"Transform">>, Fields});
trav({container,<<"Transform">>, Fields}) ->
    Center = value_from_field(<<"center">>, vec3, Fields, {0.0, 0.0, 0.0}),
    Rotation = value_from_field(<<"rotation">>, rotation, Fields, {0.0, 0.0, 1.0, 0.0}),
    Scale = value_from_field(<<"scale">>, vec3, Fields, {1.0, 1.0, 1.0}),
    ScaleOrientation = value_from_field(<<"scaleOrientation">>, rotation, Fields, {0.0, 0.0, 1.0, 0.0}),
    Translation = value_from_field(<<"translation">>, vec3, Fields, {0.0,0.0,0.0}),

    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            TransformedShapes_0 = [];
        {multival, List} ->
            TransformedShapes_0 = lists:flatten(map_shapes(List))
    end,
    TransformedShapes_1 = translate_children(TransformedShapes_0, vec3_negate(Center)),
    TransformedShapes_2 = rotate_children(TransformedShapes_1, rotation_negate(ScaleOrientation)),
    TransformedShapes_3 = scale_children(TransformedShapes_2, Scale),
    TransformedShapes_4 = rotate_children(TransformedShapes_3, ScaleOrientation),
    TransformedShapes_5 = rotate_children(TransformedShapes_4, Rotation),
    TransformedShapes_6 = translate_children(TransformedShapes_5, Center),
    TransformedShapes_7 = translate_children(TransformedShapes_6, Translation),
    {ok, TransformedShapes_7};
trav({<<"Group">>, Fields}) ->
    trav({container,<<"Group">>, Fields});
trav({container,<<"Group">>, Fields}) ->
    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            TransformedShapes = [];
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};

trav({<<"Anchor">>, Fields}) ->
    trav({container,<<"Anchor">>, Fields});
trav({container,<<"Anchor">>, Fields}) ->
    %% We treat Anchor as just another group
    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            TransformedShapes = [];
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};
trav({<<"Billboard">>, Fields}) ->
    trav({container,<<"Billboard">>, Fields});
trav({container,<<"Billboard">>, Fields}) ->
    %% We treat Billboard as just another group
    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            TransformedShapes = [];
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};
trav({<<"Collision">>, Fields}) ->
    trav({container,<<"Collision">>, Fields});
trav({container,<<"Collision">>, Fields}) ->
    %% We treat Collision as just another group
    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            TransformedShapes = [];
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};
    

trav({<<"Inline">>, Fields}) ->
    trav({container, <<"Inline">>, Fields});
trav({container,<<"Inline">>, _Fields}) ->
    {ok, []};

trav({<<"Shape">>, Fields}) ->
    trav({container, <<"Shape">>, Fields});
trav({container,<<"Shape">>, Fields}) ->
    case proplists:get_value({field,<<"appearance">>}, Fields, none) of
        none ->
            Appearance = none;
        Cont_A ->
            {ok, Appearance} = trav_appear(def_or_use_var(Cont_A))
    end,
    case proplists:get_value({field,<<"geometry">>}, Fields, none) of
        Cont_G ->
            {ok, Geometry} = trav_geom(def_or_use_var(Cont_G))
    end,
    {ok, #shape_piece{ appearance=Appearance, geometry=Geometry}};
trav({<<"Switch">>, Fields}) ->
    trav({container,<<"Switch">>, Fields});
trav({container,<<"Switch">>, Fields}) ->
    %% Just add everything in the switch node
    case proplists:get_value({field,<<"choice">>}, Fields, none) of
        none ->
            TransformedShapes = [];
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};
trav({<<"LOD">>, Fields}) ->
    trav({container,<<"LOD">>, Fields});
trav({container,<<"LOD">>, Fields}) ->
    %% We treat LOD as just another group
    case proplists:get_value({field,<<"children">>}, Fields, none) of
        none ->
            case proplists:get_value({field,<<"level">>}, Fields, none) of
                none ->
                    TransformedShapes = [];
                {multival, List} ->
                    TransformedShapes = lists:flatten(map_shapes(List))
            end;
        {multival, List} ->
            TransformedShapes = lists:flatten(map_shapes(List))
    end,
    {ok, TransformedShapes};
trav({<<"DirectionalLight">>, Fields}) ->
    trav({container, <<"DirectionalLight">>, Fields});
trav({container,<<"DirectionalLight">>, Fields}) ->
    trav_light_directionallight(Fields);
trav({<<"PointLight">>, Fields}) ->
    trav({container, <<"PointLight">>, Fields});
trav({container,<<"PointLight">>, Fields}) ->
    trav_light_pointlight(Fields);
trav({<<"SpotLight">>, Fields}) ->
    trav({container, <<"SpotLight">>, Fields});
trav({container,<<"SpotLight">>, Fields}) ->
    trav_light_spotlight(Fields);
trav({UntupledContainer, Fields}) when is_binary(UntupledContainer), is_list(Fields) ->
    trav({container, UntupledContainer, Fields});
trav({container, _Unknown, _Fields}) ->
    {ok, unimp_container}.


map_shapes([]) -> [];
map_shapes([Shp | R]) ->
    {ok, Shape} = trav(def_or_use_var(Shp)),
    Shapes_0 = [Shape | map_shapes(R)],
    Shapes_1 = lists:filter(fun(unimp_container) -> false ; (_) -> true end, Shapes_0),
    Shapes_1.
    



trav_appear({container,<<"Appearance">>,Fields}) ->
    case proplists:get_value({field,<<"material">>}, Fields, none) of
        none ->
            Material = none;
        Cont_M ->
            {ok, Material} = trav_mat(def_or_use_var(Cont_M))
    end,
    case proplists:get_value({field,<<"texture">>}, Fields, none) of
        none ->
            Texture = none;
        Cont_Tx ->
            {ok, Texture} = trav_tex(def_or_use_var(Cont_Tx))
    end,
    case proplists:get_value({field,<<"textureTransform">>}, Fields, none) of
        none ->
            TextureTransform = none;
        Cont_TT ->
            {ok, TextureTransform} = trav_tex_tran(def_or_use_var(Cont_TT))
    end,
    {ok, #material{ material=Material, texture=Texture, txtransform=TextureTransform }}.
    
trav_tex({container,<<"ImageTexture">>,Fields}) ->
    case proplists:get_value({field,<<"url">>}, Fields, none) of
        {multival, []} ->
            URL = "";
        {multival, [{string, URL_0} | _]} when is_list(URL_0) ->
            {ok, URL} = parse_image_href(URL_0)
    end,
    {ok, URL};
trav_tex({container,<<"PixelTexture">>,Fields}) ->
    Image = proplists:get_value({field,<<"image">>}, Fields, none),
    {ok, Image}.



trav_tex_tran({container,<<"TextureTransform">>,Fields}) ->
    _Center = value_from_field(<<"center">>, vec2, Fields, {0.0, 0.0}),
    _Rotation = value_from_field(<<"rotation">>, float, Fields, 0.0),
    _Scale = value_from_field(<<"scale">>, vec2, Fields, {1.0, 1.0}),
    _Translation = value_from_field(<<"translation">>, vec2, Fields, {0.0, 0.0}),
    {ok, currently_unused}.


trav_mat({container,<<"Material">>,Fields}) ->
    AmbInt = value_from_field(<<"ambientIntensity">>, float, Fields, 0.2),
    {DC_R, DC_G, DC_B} = value_from_field(<<"diffuseColor">>, color, Fields, {0.8,0.8,0.8}),
    {EC_R, EC_G, EC_B} = value_from_field(<<"emissiveColor">>, color, Fields, {0.0,0.0,0.0}),
    Shininess = value_from_field(<<"shininess">>, float, Fields, 0.2),
    {SC_R, SC_G, SC_B} = value_from_field(<<"specularColor">>, color, Fields, {0.0,0.0,0.0}),
    Transp = value_from_field(<<"transparency">>, float, Fields, 0.0),
    {ok, #materialprops{
        ambient_intensity = float(AmbInt),
        diffuse_color = {float(DC_R), float(DC_G), float(DC_B)},
        emissive_color = {float(EC_R), float(EC_G), float(EC_B)},
        shininess = float(Shininess),
        specular_color = {float(SC_R), float(SC_G), float(SC_B)},
        transparency = float(Transp)
    }}.

trav_geom({container,<<"IndexedFaceSet">>,Fields}) -> trav_geom_indexedfaceset(Fields);
trav_geom({container,<<"Box">>,Fields}) -> trav_geom_box(Fields);
trav_geom({container,<<"Cone">>,Fields}) -> trav_geom_cone(Fields);
trav_geom({container,<<"Cylinder">>,Fields}) -> trav_geom_cylinder(Fields);
trav_geom({container,<<"Sphere">>,Fields}) -> trav_geom_sphere(Fields);
trav_geom({container,<<"ElevationGrid">>,Fields}) -> trav_geom_elevationgrid(Fields);
trav_geom({container,<<"Extrusion">>,Fields}) -> trav_geom_extrusion(Fields).

trav_geom_box(Fields) ->
    %% Box
    {X,Y,Z} = value_from_field(<<"size">>, vec3, Fields, {2.0,2.0,2.0}),
    {ok, make_box(float(X),float(Y),float(Z))}.
    
trav_geom_cone(Fields) ->
    %% Cone
    BottomRadius = value_from_field(<<"bottomRadius">>, float, Fields, 1.0),
    Height = value_from_field(<<"height">>, float, Fields, 2.0),
    _HasSide = value_from_field(<<"side">>, float, Fields, 1),
    _HasBottom = value_from_field(<<"bottom">>, float, Fields, 1),
    {ok, make_cone(float(BottomRadius), float(Height))}.
    
trav_geom_cylinder(Fields) ->
    %% Cylinder
    _HasBottom = value_from_field(<<"bottom">>, float, Fields, 1),
    Height = value_from_field(<<"height">>, float, Fields, 2.0),
    Radius = value_from_field(<<"radius">>, float, Fields, 1.0),
    _HasSide = value_from_field(<<"side">>, float, Fields, 1),
    _HasTop = value_from_field(<<"top">>, float, Fields, 1),
    {ok, make_cylinder(float(Radius), float(Height))}.

trav_geom_sphere(Fields) ->
    %% Sphere
    Radius = value_from_field(<<"radius">>, float, Fields, 1.0),
    
    {ok, make_sphere(float(Radius))}.
    

    

trav_geom_elevationgrid(Fields) ->
    %% ElevationGrid
    
    XDimension = value_from_field(<<"xDimension">>, float, Fields, 0),
    ZDimension = value_from_field(<<"zDimension">>, float, Fields, 0),

    case proplists:get_value({field,<<"color">>}, Fields, none) of
        none ->
            Colors = none;
        Cont_C ->
            {ok, Colors_0} = trav_color(def_or_use_var(Cont_C)),
            HasColorPerVertex = value_from_field(<<"colorPerVertex">>, float, Fields, 1),
            Colors = {to_bool(HasColorPerVertex), Colors_0}
    end,
    case proplists:get_value({field,<<"normal">>}, Fields, none) of
        none ->
            _Normals = none;
        Cont_N ->
            {ok, _Normals} = trav_norm(def_or_use_var(Cont_N))
    end,
    case proplists:get_value({field,<<"texCoord">>}, Fields, none) of
        none ->
            TexCoords = none;
        Cont_TC ->
            {ok, TexCoords} = trav_texcoords(def_or_use_var(Cont_TC))
    end,
    
    case proplists:get_value({field,<<"height">>}, Fields, none) of
        {multival, Val} ->
            Heights = [ float(V) || {float, V} <- Val]
    end,
    
    IsCCW = value_from_field(<<"ccw">>, float, Fields, 1),
    CreaseAngle = value_from_field(<<"creaseAngle">>, float, Fields, 0.0),
    _HasNormalPerVertex = value_from_field(<<"normalPerVertex">>, float, Fields, 1),
    _IsSolid = value_from_field(<<"solid">>, float, Fields, 1),
    XSpacing = value_from_field(<<"xSpacing">>, float, Fields, 1.0),
    ZSpacing = value_from_field(<<"zSpacing">>, float, Fields, 1.0),
    
    {ok, set_ccw(to_bool(IsCCW), make_elevationgrid(
        Heights, round(XDimension), round(ZDimension),
        float(XSpacing), float(ZSpacing), float(CreaseAngle),
        Colors, TexCoords))}.
    
trav_geom_extrusion(Fields) ->
    %% Extrusion
    case proplists:get_value({field,<<"crossSection">>}, Fields, none) of
        none ->
            CrossSections_0 = [{1, 1}, {1, -1}, {-1, -1}, {-1, 1}, {1, 1}];
        {multival, Vecs_CS} ->
            CrossSections_0 = [ {float(X),float(Z)} || {vec2,X,Z} <- Vecs_CS]
    end,
    case proplists:get_value({field,<<"orientation">>}, Fields, none) of
        none ->
            Rotations_0 = [{0.0, 0.0, 1.0, 0.0}];
        {multival, Vecs_R} ->
            Rotations_0 = [ {float(X),float(Y),float(Z),float(Ang)} || {rotation,X,Y,Z,Ang} <- Vecs_R]
    end,
    case proplists:get_value({field,<<"scale">>}, Fields, none) of
        none ->
            Scales_0 = [{1.0, 1.0}];
        {multival, Vecs_Sc} ->
            Scales_0 = [ {float(X),float(Z)} || {vec2,X,Z} <- Vecs_Sc]
    end,
    case proplists:get_value({field,<<"spine">>}, Fields, none) of
        none ->
            Spine = [{0.0, 0.0, 0.0}, {0.0, 1.0, 0.0}];
        {multival, Vecs_S} ->
            Spine = [ {float(X),float(Y),float(Z)} || {vec3,X,Y,Z} <- Vecs_S]
    end,
    
    _HasBeginCap = value_from_field(<<"beginCap">>, float, Fields, 1),
    IsCCW = value_from_field(<<"ccw">>, float, Fields, 1),
    _IsConvex = value_from_field(<<"convex">>, float, Fields, 1),
    CreaseAngle = value_from_field(<<"creaseAngle">>, float, Fields, 0.0),
    _HasEndCap = value_from_field(<<"endCap">>, float, Fields, 1),
    _Solid = value_from_field(<<"solid">>, float, Fields, 1),
    
    {ok, set_ccw(to_bool(IsCCW), make_extrusion(
        Spine, CrossSections_0, Scales_0, Rotations_0, float(CreaseAngle)))}.

trav_geom_indexedfaceset(Fields) ->

    case proplists:get_value({field,<<"coord">>}, Fields, none) of
        %none ->
        %    Coords = [];
        Cont_C ->
            {ok, Coords} = trav_coord(def_or_use_var(Cont_C))
    end,
    
    case proplists:get_value({field,<<"texCoord">>}, Fields, none) of
        none ->
            TexCoords = none;
        Cont_TC ->
            {ok, TexCoords} = trav_texcoords(def_or_use_var(Cont_TC))
    end,
    
    IsCCW = value_from_field(<<"ccw">>, float, Fields, 1),
    _IsConvex = value_from_field(<<"convex">>, float, Fields, 1),
    case proplists:get_value({field,<<"coordIndex">>}, Fields, none) of
        {multival, Vals_CI} ->
            CoordIndices = delim_indexes_to_lists([ N || {float,N} <- Vals_CI ])
    end,
    
    %% creaseAngle is in radians
    CreaseAngle = value_from_field(<<"creaseAngle">>, float, Fields, 0.0),
    
    HasNormalPerVertex = value_from_field(<<"normalPerVertex">>, float, Fields, 1),
    case HasNormalPerVertex of
        1 ->
            {Normals, NormalIndices} =
                trav_geom_indexedfaceset_ns(Fields, Coords, CoordIndices);
        _ ->
            Normals = none,
            NormalIndices = none
    end,
    
    _IsSolid = value_from_field(<<"solid">>, float, Fields, 1),
    
    case proplists:get_value({field,<<"texCoordIndex">>}, Fields, none) of
        none when TexCoords =:= none ->
            TCIndices = none;
        none when TexCoords =/= none ->
            TCIndices = CoordIndices;
        {multival, Vals_TC} ->
            TCIndices = delim_indexes_to_lists([ E || {float, E} <- Vals_TC])
    end,
    
    {Colors_1, ColorIndices_1} = 
        trav_geom_indexedfaceset_col(Fields, Coords, CoordIndices),
    
    {ok, set_ccw(to_bool(IsCCW), #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        normal=Normals,
        normalIndices=NormalIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        colors=Colors_1,
        colIndices=ColorIndices_1,
        creaseAngle=CreaseAngle})}.

trav_geom_indexedfaceset_ns(Fields, Coords, CoordIndices) ->
    case proplists:get_value({field,<<"normal">>}, Fields, none) of
        none ->
            Normals = none,
            NormalIndices = none;
        Cont_N ->
            {ok, Normals_1} = trav_norm(def_or_use_var(Cont_N)),
            case proplists:get_value({field,<<"normalIndex">>}, Fields, none) of
                none when length(Coords) =:= length(Normals_1) ->
                    Normals = Normals_1,
                    NormalIndices = CoordIndices;
                {multival, Vals_N} ->
                    Normals = Normals_1,
                    NormalIndices = delim_indexes_to_lists([ E || {float, E} <- Vals_N])
            end
    end,
    {Normals, NormalIndices}.

trav_geom_indexedfaceset_col(Fields, _Coords, CoordIndices) ->
    case proplists:get_value({field,<<"color">>}, Fields, none) of
        none ->
            Colors = none;
        Cont_Col ->
            {ok, Colors_0} = trav_color(def_or_use_var(Cont_Col)),
            case proplists:get_value({field,<<"colorIndex">>}, Fields, none) of
                none ->
                    ColorIndices = none;
                {multival, Vals_ColI} ->
                    ColorIndices = delim_indexes_to_lists([ N || {float,N} <- Vals_ColI ])
            end,
            HasColorPerVertex = value_from_field(<<"colorPerVertex">>, float, Fields, 1),
            Colors = {to_bool(HasColorPerVertex), Colors_0, ColorIndices}
    end,
    case Colors of
        none ->
            {none, none};
        {false, Colors_0_F, [ColorIndices_0_F]} ->
            geom_per_face_colors(Colors_0_F, ColorIndices_0_F, CoordIndices);
        {true, Colors_0_V, ColorIndices_0_V} ->
            geom_per_vertex_colors(Colors_0_V, ColorIndices_0_V, CoordIndices)
    end.


trav_coord({container,<<"Coordinate">>,Fields}) ->
    case proplists:get_value({field,<<"point">>}, Fields, none) of
        {multival, Vecs} ->
            Coords = [{float(X),float(Y),float(Z)} || {vec3, X, Y, Z} <- Vecs]
    end,
    {ok, Coords}.


trav_norm({container,<<"Normal">>, Fields}) ->
    case proplists:get_value({field,<<"vector">>}, Fields, none) of
        {multival, Vecs} ->
            Normals = [ {float(X),float(Y),float(Z)} || {vec3,X,Y,Z} <- Vecs]
    end,
    {ok, Normals}.


trav_color({container,<<"Color">>, Fields}) ->
    case proplists:get_value({field,<<"color">>}, Fields, none) of
        {multival, Vecs} ->
            Colors = [ {float(R),float(G),float(B)} || {vec3,R,G,B} <- Vecs]
    end,
    {ok, Colors}.

trav_texcoords({container,<<"TextureCoordinate">>,Fields}) ->
    case proplists:get_value({field,<<"point">>}, Fields, none) of
        {multival, Vecs} ->
            Coords = [{float(U),float(V)} || {vec2, U, V} <- Vecs]
    end,
    {ok, Coords}.


trav_light_directionallight(Fields) ->
    %% DirectionalLight
    AmbientIntensity = value_from_field(<<"ambientIntensity">>, float, Fields, 0.0),
    {R,G,B} = value_from_field(<<"color">>, color, Fields, {1.0,1.0,1.0}),
    {X,Y,Z} = value_from_field(<<"direction">>, vec3, Fields, {0,0,-1}),
    Intensity = value_from_field(<<"intensity">>, float, Fields, 1.0),
    IsOn = value_from_field(<<"on">>, float, Fields, 1),
    {ok, #lightsrc{
        light=dirlight,
        ison=to_bool(IsOn),
        pos={float(X),float(Y),float(Z)},
        color={float(R),float(G),float(B)},
        intensity=float(Intensity),
        ambintensity=float(AmbientIntensity)}}.
    
trav_light_pointlight(Fields) ->
    %% PointLight

    AmbientIntensity = value_from_field(<<"ambientIntensity">>, float, Fields, 0.0),
    {XA,YA,ZA} = value_from_field(<<"attenuation">>, vec3, Fields, {1.0, 0.0, 0.0}),
    {R,G,B} = value_from_field(<<"color">>, color, Fields, {1.0,1.0,1.0}),
    Intensity = value_from_field(<<"intensity">>, float, Fields, 1.0),
    {X,Y,Z} = value_from_field(<<"location">>, vec3, Fields, {0,0,0}),
    IsOn = value_from_field(<<"on">>, float, Fields, 1),
    Radius = value_from_field(<<"radius">>, float, Fields, 100.0),
    {ok, #lightsrc{
        light={pointlight, float(Radius), {float(XA),float(YA),float(ZA)}},
        ison=to_bool(IsOn),
        pos={float(X),float(Y),float(Z)},
        color={float(R),float(G),float(B)},
        intensity=float(Intensity),
        ambintensity=float(AmbientIntensity)}}.

trav_light_spotlight(Fields) ->
    %% SpotLight
    AmbientIntensity = value_from_field(<<"ambientIntensity">>, float, Fields, 0.0),
    {XA,YA,ZA} = value_from_field(<<"attenuation">>, vec3, Fields, {1.0, 0.0, 0.0}),
    BeamWidth = value_from_field(<<"beamWidth">>, float, Fields, 1.5),
    {R,G,B} = value_from_field(<<"color">>, color, Fields, {1.0, 1.0, 1.0}),
    CutOffAngle = value_from_field(<<"cutOffAngle">>, float, Fields, 0.7),
    {XD,YD,ZD} = value_from_field(<<"direction">>, vec3, Fields, {0,0,-1}),
    Intensity = value_from_field(<<"intensity">>, float, Fields, 1.0),
    {X,Y,Z} = value_from_field(<<"location">>, vec3, Fields, {0,0,0}),
    IsOn = value_from_field(<<"on">>, float, Fields, 1),
    Radius = value_from_field(<<"radius">>, float, Fields, 100),
    {ok, #lightsrc{
        light={spotlight, float(Radius), float(BeamWidth), float(CutOffAngle),
            {float(XD),float(YD),float(ZD)}, {float(XA),float(YA),float(ZA)}},
        ison=to_bool(IsOn),
        pos={float(X),float(Y),float(Z)},
        color={float(R),float(G),float(B)},
        intensity=float(Intensity),
        ambintensity=float(AmbientIntensity)}}.


value_from_field(F, color, Fields, Default) ->
    case proplists:get_value({field, F}, Fields, none) of
        none ->
            Default;
        {vec3, R,G,B} ->
            {R,G,B}
    end;
value_from_field(F, float, Fields, Default) ->
    case proplists:get_value({field, F}, Fields, none) of
        none ->
            Default;
        {float, V} ->
            V
    end;
value_from_field(F, vec3, Fields, Default) ->
    case proplists:get_value({field, F}, Fields, none) of
        none ->
            Default;
        {vec3, X,Y,Z} ->
            {X,Y,Z}
    end;
value_from_field(F, vec2, Fields, Default) ->
    case proplists:get_value({field, F}, Fields, none) of
        none ->
            Default;
        {vec2, X,Y} ->
            {X,Y}
    end;
value_from_field(F, rotation, Fields, Default) ->
    case proplists:get_value({field, F}, Fields, none) of
        none ->
            Default;
        {rotation, XA,YA,ZA,Ang} ->
            {XA,YA,ZA,Ang}
    end.


%% Change VRML "-1" delimited list of integers into a list of lists.
%%
delim_indexes_to_lists(List) ->
    delim_indexes_to_lists(List, [], []).
delim_indexes_to_lists([], [], OList) ->
    lists:reverse(OList);
delim_indexes_to_lists([], OSList, OList) ->
    delim_indexes_to_lists([], [], [lists:reverse(OSList) | OList]);
delim_indexes_to_lists([-1 | List], OSList, OList) ->
    case OSList of
        [] -> delim_indexes_to_lists(List, OSList, OList);
        _  -> delim_indexes_to_lists(List, [], [lists:reverse(OSList) | OList])
    end;
delim_indexes_to_lists([N | List], OSList, OList) when N >= 0 ->
    delim_indexes_to_lists(List, [N | OSList], OList).


%% Unimplemented:
%%
%% AudioClip {}
%% Background {}
%% Fog {}
%% FontStyle {}
%% MovieTexture {}
%% NavigationInfo {}
%% PixelTexture {}
%% Script {}
%% Sound {}
%% Text {}
%% Viewpoint {}
%% WorldInfo {}
%%
%% Interpolators: ColorInterpolator, CoordinateInterpolator, NormalInterpolator,
%%            OrientationInterpolator, PositionInterpolator, ScalarInterpolator
%% Sensors: CylinderSensor, PlaneSensor, ProximitySensor, SphereSensor, 
%%          TimeSensor, TouchSensor, VisibilitySensor
%%
%% Unimplemented non polygon nodes:
%% PointSet {}
%% IndexedLineSet {}
%%



%%
%% VRML 1.0 to VRML 2.0 Rearranger
%%
%% VRML 1.0 semantic behaviour is different from VRML 2.0,
%% nodes (esp. transform nodes) affect the state of nodes after it.
%% Separator nodes act as state push and pop.
%%
%% VRML 1.0 is now less common compared to 2.0, but can still be parsed
%% with some changes to the structure. Instead of having its own complete
%% parser, the tokens will be partially parsed and rearranged instead so
%% it fits the structure of a VRML 2.0 file.
%%
rearrange_v1_to_v2(VRMLTokens_V1) ->
    {_, V1S, []} = v1trn(VRMLTokens_V1, [], []),
    V1S_1 = orddict:erase(<<"TextureCoordinate">>,
            orddict:erase(<<"Normal">>,
            orddict:erase(<<"Coordinate">>,
            orddict:erase(<<"TextureCoordinate">>,
            orddict:erase(<<"Texture">>,
            orddict:erase(<<"Material">>, V1S)))))),
    case orddict:find(<<"children">>, V1S_1) of
        {ok, AllChildren} ->
            VRMLTokens_V2 = lists:append(lists:reverse(AllChildren))
    end,
    {ok, VRMLTokens_V2}.


v1trn([], V1S, V2R) ->
    {lists:append(lists:reverse(V2R)), V1S, []};
v1trn([close_curly | Rest], V1S, V2R) ->
    {lists:append(lists:reverse(V2R)), V1S, Rest};
%% Separator
v1trn([{word, <<"Separator">>}, open_curly | Rest], V1S, V2R) ->
    %% We change "Separator" into "Transform" group nodes.
    {_, V1S_2, Rest_1} = v1trn(Rest, orddict:erase(<<"children">>, V1S), []),
    %% Remove the ones we don't need
    V1S_3 = orddict:erase(<<"TextureCoordinate">>,
            orddict:erase(<<"Normal">>,
            orddict:erase(<<"Coordinate">>,
            orddict:erase(<<"TextureCoordinate">>,
            orddict:erase(<<"Texture">>,
            orddict:erase(<<"Material">>, V1S_2)))))),
    
    case orddict:find(<<"children">>, V1S_3) of
        {ok, Children} ->
            TransformNode =
                [{word, <<"Transform">>}, open_curly] ++
                [{word, <<"children">>}, open_bracket] ++
                lists:append(lists:reverse(Children)) ++
                [close_bracket, close_curly],
            v1trn(Rest_1, v1trn_add_to_children(TransformNode, V1S), V2R);
        _ ->
            v1trn(Rest_1, V1S, V2R)
    end;
%% Group
v1trn([{word, GroupType}, open_curly | Rest], V1S, V2R)
    when GroupType =:= <<"Group">>;
         GroupType =:= <<"Switch">>;
         GroupType =:= <<"WWWAnchor">> ->
    {_, V1S_1, Rest_1} = v1trn(Rest, orddict:erase(<<"children">>, V1S), []),
    
    case orddict:find(<<"children">>, V1S_1) of
        {ok, Children} ->
            GroupNode =
                [{word, <<"Group">>}, open_curly] ++
                [{word, <<"children">>}, open_bracket] ++
                lists:append(lists:reverse(Children)) ++
                [close_bracket, close_curly],
            v1trn(Rest_1, v1trn_add_to_children(GroupNode, V1S), V2R);
        _ ->
            v1trn(Rest_1, V1S, V2R)
    end;
%% TransformSeparator
v1trn([{word, <<"TransformSeparator">>}, open_curly | Rest], V1S, V2R) ->
    {_, V1S_1, Rest_1} = v1trn(Rest, orddict:erase(<<"children">>, V1S), []),
    
    case orddict:find(<<"children">>, V1S_1) of
        {ok, Children} ->
            TransformNode =
                [{word, <<"Transform">>}, open_curly] ++
                [{word, <<"children">>}, open_bracket] ++
                lists:append(lists:reverse(Children)) ++
                [close_bracket, close_curly],
            v1trn(Rest_1, v1trn_add_to_children(TransformNode, V1S), V2R);
        _ ->
            v1trn(Rest_1, V1S, V2R)
    end;

v1trn([{word, <<"ShapeHints">>}, open_curly | Rest], V1S, V2R) ->
    {_, V1S_1, Rest_1} = v1trn(Rest, V1S, []),
    case orddict:find(<<"vertexOrdering">>, V1S_1) of
        {ok, <<"ccw">>} -> 
            V1S_2 = orddict:store(<<"ccw">>, 1.0, V1S);
        _Notfound ->
            V1S_2 = V1S
    end,
    v1trn(Rest_1, V1S_2, V2R);
    
v1trn([{word, <<"Coordinate3">>}, open_curly | Rest], V1S, V2R) ->
    {InsideCoordNode, Rest_1} = v1trn_1(coordinate3, Rest, []),
    V1S_2 = orddict:store(<<"Coordinate">>, InsideCoordNode, V1S),
    v1trn(Rest_1, V1S_2, V2R);
v1trn([{word, <<"Normal">>}, open_curly | Rest], V1S, V2R) ->
    {InsideNormalNode, Rest_1} = v1trn_1(normal, Rest, []),
    V1S_2 = orddict:store(<<"Normal">>, InsideNormalNode, V1S),
    v1trn(Rest_1, V1S_2, V2R);
v1trn([{word, <<"TextureCoordinate2">>}, open_curly | Rest], V1S, V2R) ->
    {InsideTexCoord, Rest_1} = v1trn_1(texturecoordinate2, Rest, []),
    V1S_2 = orddict:store(<<"TextureCoordinate">>, InsideTexCoord, V1S),
    v1trn(Rest_1, V1S_2, V2R);
    
    
v1trn([{word, <<"Texture2">>}, open_curly | Rest], V1S, V2R) ->
    {InsideTextureNode, Rest_1} = v1trn_1(texture2, Rest, []),
    V1S_2 = orddict:store(<<"Texture">>, InsideTextureNode, V1S),
    v1trn(Rest_1, V1S_2, V2R);
v1trn([{word, <<"Material">>}, open_curly | Rest], V1S, V2R) ->
    {InsideMaterialNode, Rest_1} = v1trn_1(material, Rest, []),
    V1S_2 = orddict:store(<<"Material">>, InsideMaterialNode, V1S),
    v1trn(Rest_1, V1S_2, V2R);

v1trn([{word, <<"Cube">>}, open_curly | Rest], V1S, V2R) ->
    {ShpFields, Rest_1} = v1trn_float_fields(Rest, [<<"width">>, <<"height">>, <<"depth">>], []),
    X = proplists:get_value(<<"width">>, ShpFields, 2),
    Y = proplists:get_value(<<"height">>, ShpFields, 2),
    Z = proplists:get_value(<<"depth">>, ShpFields, 2),
    InsideGeometryNode_2 = [{word, <<"size">>}, {number, X}, {number, Y}, {number, Z}],
    ShapeNode = v1trn_make_shape(<<"Box">>, InsideGeometryNode_2, v1trn_gather_shape_mtl(V1S)),
    v1trn(Rest_1, v1trn_add_to_children(ShapeNode, V1S), V2R);
v1trn([{word, <<"Cone">>}, open_curly | Rest], V1S, V2R) ->
    {ShpFields, Rest_1} = v1trn_float_fields(Rest, [<<"bottomRadius">>, <<"height">>], []),
    BottomRadius = proplists:get_value(<<"bottomRadius">>, ShpFields, 1),
    Height = proplists:get_value(<<"height">>, ShpFields, 2),
    InsideGeometryNode_2 = [
        {word, <<"bottomRadius">>}, {number, BottomRadius},
        {word, <<"height">>}, {number, Height}
        ],
    ShapeNode = v1trn_make_shape(<<"Cone">>, InsideGeometryNode_2, v1trn_gather_shape_mtl(V1S)),
    v1trn(Rest_1, v1trn_add_to_children(ShapeNode, V1S), V2R);
v1trn([{word, <<"Cylinder">>}, open_curly | Rest], V1S, V2R) ->
    {ShpFields, Rest_1} = v1trn_float_fields(Rest, [<<"radius">>, <<"height">>], []),
    Radius = proplists:get_value(<<"radius">>, ShpFields, 1),
    Height = proplists:get_value(<<"height">>, ShpFields, 2),
    InsideGeometryNode_2 = [
        {word, <<"radius">>}, {number, Radius},
        {word, <<"height">>}, {number, Height}],
    ShapeNode = v1trn_make_shape(<<"Cylinder">>, InsideGeometryNode_2, v1trn_gather_shape_mtl(V1S)),
    v1trn(Rest_1, v1trn_add_to_children(ShapeNode, V1S), V2R);
v1trn([{word, <<"Sphere">>}, open_curly | Rest], V1S, V2R) ->
    {ShpFields, Rest_1} = v1trn_float_fields(Rest, [<<"radius">>], []),
    Radius = proplists:get_value(<<"radius">>, ShpFields, 1),
    InsideGeometryNode_2 = [{word, <<"radius">>}, {number, Radius}],
    ShapeNode = v1trn_make_shape(<<"Sphere">>, InsideGeometryNode_2, v1trn_gather_shape_mtl(V1S)),
    v1trn(Rest_1, v1trn_add_to_children(ShapeNode, V1S), V2R);
    
v1trn([{word, <<"IndexedLineSet">>}, open_curly | Rest], V1S, V2R) ->
    %% Ignore IndexedLineSet we can't even use it for anything.
    {_Modified, _, Rest_1} = v1trn(Rest, V1S, []),
    v1trn(Rest_1, V1S, V2R);
v1trn([{word, <<"PointSet">>}, open_curly | Rest], V1S, V2R) ->
    %% Ignore PointSet we can't even use it for anything.
    {_Modified, _, Rest_1} = v1trn(Rest, V1S, []),
    v1trn(Rest_1, V1S, V2R);
v1trn([{word, <<"IndexedFaceSet">>}, open_curly | Rest], V1S, V2R) ->
    {InsideGeometryNode, Rest_1} = v1trn_1(indexedfaceset, Rest, []),
    
    InsideGeometryNode_2 = 
        case orddict:find(<<"Coordinate">>, V1S) of
            {ok, InsideCoordNode} ->
                [{word, <<"coord">>}, {word, <<"Coordinate">>}, open_curly] ++
                InsideCoordNode ++
                [close_curly];
            _ -> []
        end ++
        case orddict:find(<<"Normal">>, V1S) of
            {ok, InsideNormalNode} ->
                [{word, <<"normal">>}, {word, <<"Normal">>}, open_curly] ++
                InsideNormalNode ++
                [close_curly];
        
            _ -> []
        end ++
        case orddict:find(<<"TextureCoordinate">>, V1S) of
            {ok, InsideTexCoord} ->
                [{word, <<"texCoord">>}, {word, <<"TextureCoordinate">>}, open_curly] ++
                InsideTexCoord ++
                [close_curly];
            _ -> []
        end ++
        InsideGeometryNode,
    
    ShapeNode = v1trn_make_shape(<<"IndexedFaceSet">>, InsideGeometryNode_2, v1trn_gather_shape_mtl(V1S)),
    v1trn(Rest_1, v1trn_add_to_children(ShapeNode, V1S), V2R);

v1trn([{word, <<"vertexOrdering">>}, {word, <<"COUNTERCLOCKWISE">>} | Rest], V1S, V2R) ->
    v1trn(Rest, orddict:store(<<"vertexOrdering">>, {set, <<"ccw">>}, V1S), V2R);

%% Catch-all
v1trn([{word, AnyNode}, open_curly | Rest], V1S, V2R) ->
    {Modified, _, Rest_1} = v1trn(Rest, V1S, []),
    v1trn(Rest_1, V1S, [[{word, AnyNode}, open_curly] ++ Modified ++ [close_curly] | V2R]);
v1trn([Any | Rest], V1S, V2R) ->
    v1trn(Rest, V1S, [[Any] | V2R]).

    
v1trn_float_fields([close_curly | Rest], _WhichFields, V2R) ->
    {lists:reverse(V2R), Rest};
v1trn_float_fields([{word, FieldMaybe}, {number, Number} | Rest], WhichFields, V2R) ->
    case lists:member(FieldMaybe, WhichFields) of
        true ->
            v1trn_float_fields(Rest, WhichFields, [{FieldMaybe, Number} | V2R]);
        false ->
            v1trn_float_fields(Rest, WhichFields, V2R)
    end;
v1trn_float_fields([{word, _AnyNode}, open_curly | Rest], WhichFields, V2R) ->
    {_, _, Rest_1} = v1trn(Rest, [], []),
    v1trn_float_fields(Rest_1, WhichFields, V2R);
v1trn_float_fields([_ | Rest], WhichFields, V2R) ->
    v1trn_float_fields(Rest, WhichFields, V2R).


v1trn_1(_, [close_curly | Rest], V2R) ->
    {lists:reverse(V2R), Rest};
v1trn_1(coordinate3, [Any | Rest], V2R) ->
    v1trn_1(coordinate3, Rest, [Any | V2R]);
v1trn_1(normal, [Any | Rest], V2R) ->
    v1trn_1(normal, Rest, [Any | V2R]);
v1trn_1(texturecoordinate2, [Any | Rest], V2R) ->
    v1trn_1(texturecoordinate2, Rest, [Any | V2R]);
v1trn_1(texture2, [{word, <<"filename">>} | R], V2R) ->
    v1trn_1(material, R, [{word, <<"url">>} | V2R]);
v1trn_1(material, [{word, <<"ambientColor">>}, {number, ColR}, {number, ColG}, {number, ColB} | R], V2R) ->
    v1trn_1(material, R,
        [ {number, ((ColR + ColG + ColB) / 3.0)},
          {word, <<"ambientIntensity">>} | V2R ]);
v1trn_1(indexedfaceset, [{word, <<"textureCoordIndex">>} | R], V2R) ->
    %% textureCoordIndex was renamed to texCoordIndex
    v1trn_1(indexedfaceset, R, [{word, <<"texCoordIndex">>} | V2R]);
v1trn_1(V1Node, [Any | Rest], V2R) ->
    v1trn_1(V1Node, Rest, [Any | V2R]).

v1trn_gather_shape_mtl(V1S) ->
    case orddict:find(<<"Texture">>, V1S) of
        {ok, InsideTextureNode} -> ok;
        _ -> InsideTextureNode = none
    end,
    case orddict:find(<<"Material">>, V1S) of
        {ok, InsideMaterialNode} -> ok;
        _ -> InsideMaterialNode = none
    end,
    {InsideMaterialNode, InsideTextureNode}.

%% Make new tokens for a VRML 2.0 Shape Node
v1trn_make_shape(GeometryNodeType, InsideGeometryNode, {InsideMaterialNode, InsideTextureNode}) ->
    case InsideMaterialNode of
        none ->
            AppearancePart = [];
        _ ->
            AppearancePart = [{word, <<"appearance">>}] ++
            [{word, <<"Appearance">>}, open_curly] ++
            case InsideMaterialNode of
                none -> [];
                _ ->
                    [{word, <<"material">>}] ++
                    [{word, <<"Material">>}, open_curly] ++
                    InsideMaterialNode ++
                    [close_curly]
            end ++
            case InsideTextureNode of
                none -> [];
                _ ->
                    [{word, <<"texture">>}] ++
                    [{word, <<"ImageTexture">>}, open_curly] ++
                    InsideTextureNode ++
                    [close_curly]
            end ++
            [close_curly]
    end,
    [{word, <<"Shape">>}, open_curly] ++
    AppearancePart ++
    [{word, <<"geometry">>}] ++
    [{word, GeometryNodeType}, open_curly] ++
    InsideGeometryNode ++
    [close_curly] ++
    [close_curly].

v1trn_add_to_children(NewNode, V1S) ->
    case orddict:find(<<"children">>, V1S) of
        {ok, PrevChildren} -> ok;
        _ -> PrevChildren = []
    end,
    V1S_4 = orddict:store(<<"children">>, [NewNode | PrevChildren], V1S),
    V1S_4.

%% Notes:
%% SFbitmask and SFmatrix does not exist in VRML 2.0




%%%
%%% Read Image from URL
%%%

parse_image_href(A) when is_list(A) ->
    case parse_image_href_scheme(A) of
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
    case parsenumber_1_hex([D1,D2]) of
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


get_bitmap({abs, FilePath}, _FullFilename) ->
    get_bitmap_by_ext(FilePath);
get_bitmap({rel, FilePath_0}, FullFilename) ->
    %% Use FullFilename with the relative path.
    Dir = filename:dirname(FullFilename),
    FilePath = filename:absname_join(Dir, FilePath_0),
    get_bitmap_by_ext(FilePath);
get_bitmap(_, _FullFilename) ->
    {error, none}.
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
        
        %% Detect a few file extensions for SGI raster images which could be
        %% used by inventor files and maybe old VRML files.
        SGI when SGI =:= ".bw"; 
                 SGI =:= ".rgb";
                 SGI =:= ".rgba";
                 SGI =:= ".sgi"
        ->
            F = fun read_sgi/1;
        
        %% BMP, TIFF, etc.
        _ ->
            F = fun read_default/1
    end,
    F(FilePath).

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
    case e3d__png:load(FileName) of
        E3D=#e3d_image{} ->
            {ok, E3D};
        {error, Err} -> {error, Err}
    end.
read_sgi(FileName) ->
    case x3d_import__sgi:load(FileName) of
        E3D=#e3d_image{} ->
            {ok, E3D};
        {error, Err} -> {error, Err}
    end.
read_default(FileName) ->
    case e3d_image:load(FileName) of
        E3DImage=#e3d_image{} ->
            {ok, E3DImage};
        {error, Err} -> {error, Err}
    end.



%% Make indices clockwise if ccw is false, normally a VRML/X3D file
%% has indices of all its objects counter clockwise.
%%
-spec set_ccw(boolean(), #geometry{}) -> #geometry{}.
set_ccw(true, AlreadyCCW) ->
    AlreadyCCW;
set_ccw(_, #geometry{coordIndices=CoordIndices,normalIndices=NormalIndices,
                     tcIndices=TCIndices}=Geom) ->
    Geom#geometry{
        coordIndices=[lists:reverse(L) || L <- CoordIndices],
        normalIndices=[lists:reverse(L) || L <- NormalIndices],
        tcIndices=[lists:reverse(L) || L <- TCIndices]}.

%%%
%%% Simple Primitives
%%%

-spec make_box(float(),float(),float()) -> #geometry{}.
make_box(X_0,Y_0,Z_0) ->
    X = X_0 / 2.0,
    Y = Y_0 / 2.0,
    Z = Z_0 / 2.0,
    TopCoords = [{-X,Y,-Z},{X,Y,-Z},{X,Y,Z},{-X,Y,Z}],
    BottomCoords = [{-X,-Y,-Z},{X,-Y,-Z},{X,-Y,Z},{-X,-Y,Z}],
    Coords = TopCoords ++ BottomCoords,
    BottomCoordsOffset = length(BottomCoords),
    TopIndices = [0,1,2,3],
    BottomIndices = lists:reverse([A+BottomCoordsOffset || A <- TopIndices]),
    
    Len = length(BottomCoords),
    
    Sides = [[N, N+BottomCoordsOffset, w_num(N+1,Len)+BottomCoordsOffset, w_num(N+1,Len)] || N <- lists:seq(0, Len-1)],
    CoordIndices_0 = [BottomIndices, TopIndices] ++ Sides,
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    TexCoords = [],
    TCIndices = [],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        creaseAngle=0.0}.


-spec make_cone(float(), float()) -> #geometry{}.
make_cone(Radius, Height_0) ->
    Circle = make_circle(Radius),
    Height = Height_0 / 2.0,
    BottomCoords = [{X, -Height,Z} || {X,Z} <- Circle ],
    TopCoords = [{0.0, Height, 0.0}],
    Coords = BottomCoords ++ TopCoords,
    TopCoordOffset = length(BottomCoords),
    Len = length(BottomCoords),
    
    CoordIndices_Top = [[N, w_num(N+1,Len), TopCoordOffset] || N <- lists:seq(0, Len-1)],
    CoordIndices_Bottom = [lists:reverse([N || N <- lists:seq(0, Len-1)])],
    CoordIndices_0 = CoordIndices_Top ++ CoordIndices_Bottom,
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    TexCoords = [],
    TCIndices = [],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        creaseAngle=0.0}.

-spec make_cylinder(float(), float()) -> #geometry{}.
make_cylinder(Radius, Height_0) ->
    Circle = make_circle(Radius),
    Height = Height_0 / 2.0,
    TopCoords = [{X,-Height,Z} || {X,Z} <- Circle ],
    BottomCoords = [{X,Height,Z} || {X,Z} <- Circle ],
    BottomCoordsOffset = length(BottomCoords),
    Coords = BottomCoords ++ TopCoords,
    Len = length(BottomCoords),
    
    CoordIndices_Top = [[N || N <- lists:seq(0, Len-1)]],
    CoordIndices_Mid = [[N, N+BottomCoordsOffset, w_num(N+1,Len)+BottomCoordsOffset, w_num(N+1,Len)] || N <- lists:seq(0, Len-1)],
    CoordIndices_Bottom = [lists:reverse([N+BottomCoordsOffset || N <- lists:seq(0, Len-1)])],
    CoordIndices_0 = CoordIndices_Top ++ CoordIndices_Mid ++ CoordIndices_Bottom,
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    TexCoords = [],
    TCIndices = [],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        creaseAngle=0.0}.


-spec make_sphere(float()) -> #geometry{}.
make_sphere(Radius) ->

    TopTopCoordsSphere = [ {0.0, Radius * 1.0, 0.0} ],
    TopTopCoordsSphereOffset = 0,
    
    Top1CoordsSphere = make_hsphere(Radius, 1.0, 2, 3),
    Top1CoordsSphereOffset = TopTopCoordsSphereOffset + length(TopTopCoordsSphere),
    
    Top2CoordsSphere = make_hsphere(Radius, 1.0, 1, 3),
    Top2CoordsSphereOffset = Top1CoordsSphereOffset + length(Top1CoordsSphere),
    
    MidCoordsSphere = make_hsphere(Radius, 1.0, 0, 3),
    MidCoordsSphereOffset = Top2CoordsSphereOffset + length(Top2CoordsSphere),
    
    Bottom2CoordsSphere = make_hsphere(Radius, -1.0, 1, 3),
    Bottom2CoordsSphereOffset = MidCoordsSphereOffset + length(MidCoordsSphere),
    
    Bottom1CoordsSphere = make_hsphere(Radius, -1.0, 2, 3),
    Bottom1CoordsSphereOffset = Bottom2CoordsSphereOffset + length(Bottom2CoordsSphere),
    
    BottomBottomCoordsSphere = [ {0.0, Radius * -1.0, 0.0} ],
    BottomBottomCoordsSphereOffset = Bottom1CoordsSphereOffset + length(Bottom1CoordsSphere),
    
    Coords =
        TopTopCoordsSphere ++
        Top1CoordsSphere ++
        Top2CoordsSphere ++
        MidCoordsSphere ++
        Bottom2CoordsSphere ++
        Bottom1CoordsSphere ++
        BottomBottomCoordsSphere,
    
    Len = length(MidCoordsSphere),
    
    CoordIndices_Row1 = [[N+Top1CoordsSphereOffset, w_num(N+1,Len)+Top1CoordsSphereOffset, TopTopCoordsSphereOffset] || N <- lists:seq(0, Len-1)], % Top
    CoordIndices_Row2T = [[N+Top2CoordsSphereOffset, w_num(N+1,Len)+Top2CoordsSphereOffset, w_num(N+1,Len)+Top1CoordsSphereOffset, N+Top1CoordsSphereOffset] || N <- lists:seq(0, Len-1)],
    
    CoordIndices_Row2 = [[N+MidCoordsSphereOffset, w_num(N+1,Len)+MidCoordsSphereOffset, w_num(N+1,Len)+Top2CoordsSphereOffset, N+Top2CoordsSphereOffset] || N <- lists:seq(0, Len-1)],
    CoordIndices_Row3 = [[N+Bottom2CoordsSphereOffset, w_num(N+1,Len)+Bottom2CoordsSphereOffset, w_num(N+1,Len)+MidCoordsSphereOffset, N+MidCoordsSphereOffset] || N <- lists:seq(0, Len-1)],
    
    CoordIndices_Row3B = [[N+Bottom1CoordsSphereOffset, w_num(N+1,Len)+Bottom1CoordsSphereOffset, w_num(N+1,Len)+Bottom2CoordsSphereOffset, N+Bottom2CoordsSphereOffset] || N <- lists:seq(0, Len-1)],
    
    CoordIndices_Row4 = [[N+Bottom1CoordsSphereOffset, BottomBottomCoordsSphereOffset, w_num(N+1,Len)+Bottom1CoordsSphereOffset] || N <- lists:seq(0, Len-1)], % Bottom
    
    CoordIndices_0 =
        CoordIndices_Row1 ++
        CoordIndices_Row2T ++ 
        CoordIndices_Row2 ++ 
        CoordIndices_Row3 ++
        CoordIndices_Row3B ++
        CoordIndices_Row4,
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    TexCoords = [],
    TCIndices = [],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        creaseAngle=0.0}.

make_hsphere(R, Dir, I, N) ->
    Ang = float(I) / float(N) * math:pi() * 0.5,
    Y = math:sin(Ang) * R * Dir,
    RS = math:cos(Ang) * 1.0,
    [ {X*RS,Y,Z*RS} || {X,Z} <- make_circle(1.0 * R)]. % math:cos(float(I) / float(N) * math:pi() * 2)
make_circle(R) ->
    make_circle(R, 0, 4*4).
make_circle(_R, I, N) when I =:= N ->
    [];
make_circle(Radius, I, N) ->
    [ {Radius * math:cos(float(I) / float(N) * math:pi() * 2),
       Radius * math:sin(float(I) / float(N) * math:pi() * 2)}
      | make_circle(Radius, I+1, N)].
    

w_num(N, L) when N =:= L -> 0;
w_num(N, _L) -> N.

%%
%% Elevation Grid
%%
-spec make_elevationgrid([float()], integer(), integer(), float(), float(), float(),
    none | {boolean(), [tuple()]}, [{float(),float()}] | none) -> #geometry{}.
make_elevationgrid(Heights, XDimension, ZDimension, XSpacing, ZSpacing, CreaseAngle, Colors, TexCoords_I) ->
    {Coords_Top, _, _} = lists:foldl(fun(_, {Coords_0, I, J}) ->
        C = {XSpacing * I, lists:nth(1 + I + J * XDimension, Heights), ZSpacing * J},
        case (I+1) >= XDimension of
            true  -> {[C | Coords_0], 0, J+1};
            false -> {[C | Coords_0], I+1, J}
        end
    end, {[], 0, 0}, lists:seq(1, XDimension * ZDimension)),
    Coords_Bottom_Offset = length(Coords_Top),
    Coords_Bottom = [{X,Y-0.005,Z} || {X,Y,Z} <- Coords_Top],
    CoordIndices_Top = elev_coordidx_for_height(0, 0, XDimension, ZDimension, []),
    ?DEBUG_FMT("CoordIndices_Top=~p~n~n", [CoordIndices_Top]),
    CoordIndices_Bottom = [ lists:reverse([ Idx + Coords_Bottom_Offset || Idx <- C1 ]) || C1 <- CoordIndices_Top],
    ?DEBUG_FMT("CoordIndices_Bottom=~p~n~n", [CoordIndices_Bottom]),
    CoordIndices_Sides = elev_coordidx_for_height_sides(XDimension, ZDimension, Coords_Bottom_Offset),
    ?DEBUG_FMT("CoordIndices_Sides=~p~n~n", [CoordIndices_Sides]),
    
    Coords = Coords_Top ++ Coords_Bottom,
    CoordIndices_0 = CoordIndices_Top ++ CoordIndices_Bottom ++ CoordIndices_Sides,
    
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    case Colors of
        none ->
            Colors_1 = none,
            ColorIndices_1 = none;
        {false, Colors_0_F} ->
            {Colors_1, ColorIndices_F_Top} =
                geom_per_face_colors(Colors_0_F, none, CoordIndices_Top),
            ColorIndices_1 = elev_color_indices(ColorIndices_F_Top, CoordIndices_Sides);
        {true, Colors_0_V} ->
            {Colors_1, ColorIndices_V_Top} =
                geom_per_vertex_colors(Colors_0_V, CoordIndices_Top, CoordIndices_Top),
            ColorIndices_1 = elev_color_indices(ColorIndices_V_Top, CoordIndices_Sides)
    end,
    
    TexCoords = elev_fill_tex_coords(
        elev_generate_tex_coords(XDimension, ZDimension), TexCoords_I),
    TCIndices_Top = CoordIndices_Top,
    TCIndices_0 = TCIndices_Top ++ TCIndices_Top ++ 
        elev_coordidx_for_height_sides(XDimension, ZDimension, 0),
    TCIndices = [lists:reverse(L) || L <- TCIndices_0],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        colors=Colors_1,
        colIndices=ColorIndices_1,
        creaseAngle=CreaseAngle}.


elev_coordidx_for_height_sides(XDimension, ZDimension, Coords_Bottom_Offset) ->
    Side1 = [
        lists:reverse([
            I + J * XDimension,
            I + J * XDimension + 1,
            I + J * XDimension + Coords_Bottom_Offset + 1,
            I + J * XDimension + Coords_Bottom_Offset
        ])
    ||
        I <- lists:seq(0, XDimension-2),
        J <- [0]
    ],
    ?DEBUG_FMT("Side1=~p~n~n", [Side1]),
    Side2 = [
        [
            I + J * XDimension,
            I + (J + 1) * XDimension,
            I + (J + 1) * XDimension + Coords_Bottom_Offset,
            I + J * XDimension + Coords_Bottom_Offset
        ]
    ||
        I <- [0],
        J <- lists:seq(0, ZDimension-2)
    ],
    ?DEBUG_FMT("Side2=~p~n~n", [Side2]),
    Side3 = [
        [
            I + J * XDimension,
            I + J * XDimension + 1,
            I + J * XDimension + Coords_Bottom_Offset + 1,
            I + J * XDimension + Coords_Bottom_Offset
        ]
    ||
        I <- lists:seq(0, XDimension-2),
        J <- [ZDimension-1]
    ],
    ?DEBUG_FMT("Side3=~p~n~n", [Side3]),
    Side4 = [
        lists:reverse([
            I + J * XDimension,
            I + (J + 1) * XDimension,
            I + (J + 1) * XDimension + Coords_Bottom_Offset,
            I + J * XDimension + Coords_Bottom_Offset
        ])
    ||
        I <- [XDimension-1],
        J <- lists:seq(0, ZDimension-2)
    ],
    ?DEBUG_FMT("Side4=~p~n~n", [Side4]),
    Side1 ++ Side2 ++ Side3 ++ Side4.

elev_coordidx_for_height(I, J, XDimension, ZDimension, CoordIndices_0) ->
    C = [
        I + J * XDimension,
        I + J * XDimension + 1,
        I + (J + 1) * XDimension + 1,
        I + (J + 1) * XDimension],
    CoordIndices_1 = [C | CoordIndices_0],
    case (J+1) >= (ZDimension - 1) andalso (I+1) >= (XDimension - 1) of
        true  -> lists:reverse(CoordIndices_1);
        false ->
            case (I+1) >= (XDimension - 1) of
                true  -> elev_coordidx_for_height(0, J+1, XDimension, ZDimension, CoordIndices_1);
                false -> elev_coordidx_for_height(I+1, J, XDimension, ZDimension, CoordIndices_1)
            end
    end.

elev_generate_tex_coords(XDimension, ZDimension) when XDimension > 0, ZDimension > 0 ->
    XSpacing = 1.0 / XDimension,
    ZSpacing = 1.0 / ZDimension,
    {UVs, _, _} = lists:foldl(fun(_, {Coords_0, I, J}) ->
        C = {XSpacing * I, ZSpacing * J},
        case (I+1) >= XDimension of
            true  -> {[C | Coords_0], 0, J+1};
            false -> {[C | Coords_0], I+1, J}
        end
    end, {[], 0, 0}, lists:seq(1, XDimension * ZDimension)),
    UVs.

elev_fill_tex_coords(TexCoords_0, none) ->
    elev_fill_tex_coords(TexCoords_0, []);
elev_fill_tex_coords(TexCoords_0, TexCoords_I) ->
    elev_fill_tex_coords(TexCoords_0, TexCoords_I, []).
elev_fill_tex_coords([_|R_0], [TC|R_I], OL) ->
    elev_fill_tex_coords(R_0, R_I, [TC|OL]);
elev_fill_tex_coords([TC|R_0], [], OL) ->
    elev_fill_tex_coords(R_0, [], [TC|OL]);
elev_fill_tex_coords([], _, OL) ->
    lists:reverse(OL).


elev_color_indices(ColorIndices_Top, CoordIndices_Sides) ->
    ColorIndices_0 = ColorIndices_Top ++ ColorIndices_Top ++ 
        [[0 || _ <- L] || L <- CoordIndices_Sides],
    [lists:reverse(L) || L <- ColorIndices_0].



%%
%% Extrusions
%% 

-spec make_extrusion([{float(),float(),float()}], [{float(),float()}],
    [{float(),float()}], [{float(),float(),float(),float()}], float()) -> #geometry{}.
make_extrusion(Spine, CrossSections, Scales_0, Rotations_0, CreaseAngle) ->
    
    %% Fill in lists in case there are fewer than the spine
    Scales = extrusion_fill_arrays(Spine, Scales_0, {1.0, 1.0}, []),
    Rotations = extrusion_fill_arrays(Spine, Rotations_0, {0.0, 0.0, 1.0, 0.0}, []),
    SCPs = determine_scp(Spine),
    Segments = extrusion_zip_to_segments(Spine, Rotations, Scales, SCPs),
    
    Coords = coords_from_extrusion(CrossSections, Segments),
    CoordIndices_0 = coordidx_from_extrusion(CrossSections, Segments),
    
    %% Reverse order of all faces
    CoordIndices = [lists:reverse(L) || L <- CoordIndices_0],
    
    TexCoords = [],
    TCIndices = [],
    #geometry{
        coords=Coords,
        coordIndices=CoordIndices,
        texCoords=TexCoords,
        tcIndices=TCIndices,
        creaseAngle=CreaseAngle}.


extrusion_fill_arrays([], _, _Prev, O) ->
    lists:reverse(O);
extrusion_fill_arrays([_ | R], [S | RS], _Prev, O) ->
    extrusion_fill_arrays(R, RS, S, [S | O]);
extrusion_fill_arrays([_ | R], [], Prev, O) ->
    extrusion_fill_arrays(R, [], Prev, [Prev | O]).

extrusion_zip_to_segments(Spine, Rotations, Scales, SCPs) ->
    extrusion_zip_to_segments(Spine, Rotations, Scales, SCPs, []).
extrusion_zip_to_segments([], _, _, _, O) ->
    lists:reverse(O);
extrusion_zip_to_segments([SP | Spine], [R | Rotations], [SC | Scales], [Axises | SCPs], O) ->
    extrusion_zip_to_segments(Spine, Rotations, Scales, SCPs, [{SP, R, SC, Axises} | O]).
    
    
coords_from_extrusion(CrossSection, Segments) ->
    [First | _] = Segments,
    coords_from_extrusion(CrossSection, Segments, First, []).
coords_from_extrusion(_CrossSection, [], _Prev, O) ->
    lists:append(lists:reverse(O));
coords_from_extrusion(CrossSection, [{Point, Rotation, Scale, SCP} | RSegments], _Prev, O) ->
    RMat_0 = mat_from_axises(SCP, e3d_mat:identity()),
    RMat = e3d_mat:mul(RMat_0, from_extrusion_rotate(Rotation)),
    ExPoints = [
        begin
            {X2, Z2} = point_scale(Scale, C),
            V_1 = {float(X2), 0.0, float(Z2)},
            V_2 = e3d_mat:mul_point(RMat, V_1),
            V_3 = e3d_vec:add(V_2, Point),
            V_3
        end
    || C <- CrossSection],
    O_1 = [ExPoints | O],
    coords_from_extrusion(CrossSection, RSegments, Point, O_1).

point_scale(Scale, Point) ->
    {XS, ZS} = Scale,
    {X, Z} = Point,
    {X * XS, Z * ZS}.

from_extrusion_rotate(Rotation) ->
    {XR, YR, ZR, AngR} = Rotation,
    e3d_mat:rotate(float(AngR), {float(XR), float(YR), float(ZR)}).




    
    
coordidx_from_extrusion(CrossSection, Segments) ->
    Len = length(CrossSection),
    [{_, _, _, _} | Rest] = Segments,
    {CoordIndices_Sides, Offset} = coordidx_from_extrusion(CrossSection, 0, Rest, []),
    CoordIndices_Top = [[N || N <- lists:seq(0, Len-1)]],
    CoordIndices_Bottom = [lists:reverse([N+Offset || N <- CI]) || CI <- CoordIndices_Top],
    
    CoordIndices_Top ++ CoordIndices_Sides ++ CoordIndices_Bottom.
coordidx_from_extrusion(_, Offset, [], O) ->
    {lists:append(lists:reverse(O)), Offset};
coordidx_from_extrusion(CrossSection, Offset, [_ | RSegments], O) ->
    Len = length(CrossSection),
    %% SideIndices = [[N, N+BottomCoordsOffset, w_num(N+1,Len)+BottomCoordsOffset, w_num(N+1,Len)] || N <- lists:seq(0, Len-1)],
    SideIndices = [
        lists:reverse([
            N+Offset,
            w_num(N+1,Len)+Offset,
            w_num(N+1,Len)+Offset+Len,
            N+Offset+Len
        ])
    || N <- lists:seq(0, Len-1)],
    O_1 = [SideIndices | O],
    coordidx_from_extrusion(CrossSection, Offset + Len, RSegments, O_1).



mat_from_axises(Axises, Mat_0) ->
    {_XAxis, YAxis, _ZAxis} = Axises, % TODO: Might need to test further
    Mat = build_mat_from_axis(YAxis, {0.0, 1.0, 0.0}, Mat_0),
        %    build_mat_from_axis(XAxis, {1.0, 0.0, 0.0},
        %        build_mat_from_axis(ZAxis, {0.0, 0.0, 1.0}, Mat_0))),
    Mat.
build_mat_from_axis({X,Y,Z}=_AxisV, _WasAxis, Mat_0) when abs(X+Y+Z) < ?EPSILON ->
    Mat_0;
build_mat_from_axis(AxisV, WasAxis, Mat_0) ->
    Mat = e3d_mat:mul(Mat_0, e3d_mat:rotate_s_to_t(WasAxis, AxisV)),
    Mat.

determine_scp([]) ->
    [];
determine_scp([First | [Next | _]=Rest]) ->
    Axises = determine_scp_point(First, First, Next, {0.0,0.0,1.0}),
    {_, _, LastZ} = Axises,
    determine_scp_1(LastZ, First, Rest, [Axises]).
determine_scp_1(LastZ, Prev, [Last], List) ->
    Axises = determine_scp_point(Prev, Last, Last, LastZ),
    lists:reverse([Axises | List]);
determine_scp_1(LastZ, Prev, [Current | [Next | _]=Rest], List) ->
    Axises = determine_scp_point(Prev, Current, Next, LastZ),
    {_, _, LastZ_1} = Axises,
    determine_scp_1(LastZ_1, Current, Rest, [Axises | List]).

determine_scp_point(Spine_m_1, Spine_I, Spine_p_1, LastZ) ->
    
    YAxis = e3d_vec:norm(e3d_vec:sub(Spine_p_1, Spine_m_1)),
    ZAxis_0 = e3d_vec:cross(e3d_vec:sub(Spine_p_1, Spine_I), e3d_vec:sub(Spine_m_1, Spine_I)),
    case e3d_vec:dot(ZAxis_0, LastZ) of
        N when N < 0.0 ->
            ZAxis_1 = e3d_vec:mul(ZAxis_0, -1.0);
        _ ->
            ZAxis_1 = ZAxis_0
    end,
    ZAxis = e3d_vec:norm(ZAxis_1),
    XAxis = e3d_vec:norm(e3d_vec:cross(YAxis, ZAxis)),
    {XAxis, YAxis, ZAxis}.



%% Geometry color indices to face color lists
%%

geom_per_face_colors(Colors_0, none, CoordIndices) ->
    {lists:sublist(Colors_0, 1, length(CoordIndices)),
        geom_per_face_colors_1(
            lists:seq(0, length(Colors_0)-1),
            CoordIndices)};
geom_per_face_colors(Colors_0, ColorIndices_0, CoordIndices)
    when is_list(ColorIndices_0) ->
        {Colors_0, geom_per_face_colors_1(
            ColorIndices_0, CoordIndices)}.
    
geom_per_face_colors_1(Indices, CoordIndices) ->
    geom_per_face_colors_1(Indices, CoordIndices, []).
geom_per_face_colors_1([Col|Indices], [FaceList|CoordIndices], OL) ->
    OL_1 = [[Col || _L <- FaceList]|OL],
    geom_per_face_colors_1(Indices, CoordIndices, OL_1);
geom_per_face_colors_1(_, [], OL) ->
    lists:reverse(OL).


geom_per_vertex_colors(Colors_0, none, CoordIndices) ->
    {Colors_0, geom_per_vertex_colors_1(
        CoordIndices, CoordIndices)};
geom_per_vertex_colors(Colors_0, ColorIndices_0, CoordIndices)
    when is_list(ColorIndices_0) ->
        {Colors_0, geom_per_vertex_colors_1(
            ColorIndices_0, CoordIndices)}.
    
geom_per_vertex_colors_1(Indices, CoordIndices) ->
    geom_per_vertex_colors_1(Indices, CoordIndices, []).
geom_per_vertex_colors_1([ColFace | Indices], [FaceList|CoordIndices], OL) ->
    OL_1 = [merge_vertex_colors_list(ColFace,FaceList)|OL],
    geom_per_vertex_colors_1(Indices, CoordIndices, OL_1);
geom_per_vertex_colors_1(_, [], OL) ->
    lists:reverse(OL).
merge_vertex_colors_list(L1,L2) ->
    merge_vertex_colors_list(L1,L2,[]).
merge_vertex_colors_list([A|AL],[_|BL],OL) ->
    merge_vertex_colors_list(AL,BL,[A|OL]);
merge_vertex_colors_list([],[B|BL],OL) ->
    merge_vertex_colors_list([],BL,[B|OL]);
merge_vertex_colors_list(_,[],OL) ->
    lists:reverse(OL).



%%
%% Transformations done for Transform Group
%%
vec3_negate({X,Y,Z}) -> {-X, -Y, -Z}.
rotation_negate({XA,YA,ZA,Ang}) -> {XA,YA,ZA,-Ang}. % Perhaps

scale_children(Shapes, {1.0, 1.0, 1.0}) -> Shapes;
scale_children(Shapes, {XS, YS, ZS}) ->
    scale_children(Shapes, {XS, YS, ZS}, []).
scale_children([], _, O) ->
    lists:reverse(O);
scale_children([#shape_piece{geometry=Geometry}=Shape_0 | L], {XS, YS, ZS}, O) ->
    Shape_1 = Shape_0#shape_piece{
        geometry = scale_geometry({XS, YS, ZS}, Geometry)
    },
    scale_children(L, {XS, YS, ZS}, [Shape_1 | O]).

rotate_children(Shapes, {+0.0, +0.0, 1.0, +0.0}) -> Shapes;
rotate_children(Shapes, {RA, RB, RC, RAng}) ->
    rotate_children(Shapes, {RA, RB, RC, RAng}, []).
rotate_children([], _, O) ->
    lists:reverse(O);
rotate_children([#shape_piece{geometry=Geometry}=Shape_0 | L], {RA, RB, RC, RAng}, O) ->
    Shape_1 = Shape_0#shape_piece{
        geometry = rotate_geometry({RA, RB, RC, RAng}, Geometry)
    },
    rotate_children(L, {RA, RB, RC, RAng}, [Shape_1 | O]).

translate_children(Shapes, {+0.0, +0.0, +0.0}) -> Shapes;
translate_children(Shapes, {TX, TY, TZ}) ->
    translate_children(Shapes, {TX, TY, TZ}, []).
translate_children([], _, O) ->
    lists:reverse(O);
translate_children([#shape_piece{geometry=Geometry}=Shape_0 | L], {TX, TY, TZ}, O) ->
    Shape_1 = Shape_0#shape_piece{
        geometry = translate_geometry({TX, TY, TZ}, Geometry)
    },
    translate_children(L, {TX, TY, TZ}, [Shape_1 | O]).

scale_geometry({1.0, 1.0, 1.0}, Geometry) -> Geometry;
scale_geometry({XS, YS, ZS}, #geometry{coords=Coords}=Geometry) ->
    Geometry#geometry{
        coords=[{X*XS, Y*YS, Z*ZS} || {X, Y, Z} <- Coords]
    }.

rotate_geometry({+0.0, +0.0, 1.0, +0.0}, Geometry) -> Geometry;
rotate_geometry(RotateTup, #geometry{coords=Coords}=Geometry) ->
    Mat = rotate_mat(RotateTup),
    Geometry#geometry{
        coords=[mul_point(Mat, {X, Y, Z}) || {X, Y, Z} <- Coords]
    }.
    
translate_geometry({+0.0, +0.0, +0.0}, Geometry) -> Geometry;
translate_geometry({TX, TY, TZ}, #geometry{coords=Coords}=Geometry) ->
    Geometry#geometry{
        coords=[{X+TX, Y+TY, Z+TZ} || {X, Y, Z} <- Coords]
    }.
    
rotate_mat({XA, YA, ZA, Ang}) ->
    e3d_mat:rotate(float(Ang) * (180.0 / math:pi()),
                   {float(XA), float(YA), float(ZA)}).

mul_point(Mat, VPos) ->
    e3d_mat:mul_point(Mat, VPos).


%%
%% Parseable elements of VRML that are skipped (PROTO and EXTERPROTO)
%%

parse_skip_proto(_ProtoName, Rest0) ->
    {ok, Rest1} = parse_skip_proto_bracket(Rest0, -1),
    {ok, Rest2} = parse_skip_proto_curly(Rest1, -1),
    {ok, Rest2}.
    
parse_skip_externproto(_ExtProtoName, Rest0) ->
    {ok, Rest1} = parse_skip_proto_bracket(Rest0, -1),
    {ok, Rest2} = parse_skip_proto_bracket(Rest1, -1),
    {ok, Rest2}.
    
parse_skip_proto_bracket([close_bracket | Rest0], 0) ->
    {ok, Rest0};
parse_skip_proto_bracket([close_bracket | Rest0], I) when I > 0->
    parse_skip_proto_bracket(Rest0, I-1);
parse_skip_proto_bracket([open_bracket | Rest0], I) ->
    parse_skip_proto_bracket(Rest0, I+1);
parse_skip_proto_bracket([_ | Rest0], I) ->
    parse_skip_proto_bracket(Rest0, I).
    
parse_skip_proto_curly([close_curly | Rest0], 0) ->
    {ok, Rest0};
parse_skip_proto_curly([close_curly | Rest0], I) when I > 0 ->
    parse_skip_proto_curly(Rest0, I-1);
parse_skip_proto_curly([open_curly | Rest0], I) ->
    parse_skip_proto_curly(Rest0, I+1);
parse_skip_proto_curly([_ | Rest0], I) ->
    parse_skip_proto_curly(Rest0, I).
    
%%
%%

-spec to_bool(number()) -> boolean().
to_bool(A) when A >= 1 ->
    true;
to_bool(_) ->
    false.



%%
%% For Testing
%%


t_iv() ->
    {ok, File} = file:read_file("test.iv"),
    {ok, [Cont]} = read_vrml_content(File),
    trav(def_or_use_var(Cont)).
t_x3d() ->
    {ok, File} = file:read_file("examples_VRML\\x3d.x3d"),
    {ok, [Cont]} = read_x3d_content(File),
    trav(def_or_use_var(Cont)).
t_x3dj() ->
    {ok, C} = file:read_file("box.x3dj"),
    {ok, [Cont]} = read_x3djson_content(C),
    trav(def_or_use_var(Cont)).
t_vrml() ->
    {ok, File} = file:read_file("examples_VRML\\wrl.wrl"),
    {ok, [Cont]} = read_vrml_content(File),
    trav(def_or_use_var(Cont)).

