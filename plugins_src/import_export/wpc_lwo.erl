%%
%%  wpc_lwo.erl --
%%
%%     LightWave Object File Format (*.lwo) Import/Export
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%                2024 Edward Blake (Added LWO3)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_lwo).
-export([init/0, menu/2, command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({file, import}, Menu) ->
    menu_entry(import, Menu);
menu({file, export}, Menu) ->
    menu_entry(export, Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(export, Menu);
menu(_, Menu) -> Menu.

command({file, {import, lwo}}, St) ->
    Props = props(import),
    wpa:import(Props, fun lwo_import/1, St);
command({file, {export, lwo}}, St) ->
    Props = props(export),
    Fun = fun (Name, E3D) ->
        export(lwo2, Name, E3D)
    end,
    wpa:export(Props, Fun, St);
command({file, {export_selected, lwo}}, St) ->
    Props = props(export),
    Fun = fun (Name, E3D) ->
        export(lwo2, Name, E3D)
    end,
    wpa:export_selected(Props, Fun, St);
command(_, _) -> next.

menu_entry(import, Menu) ->
    Menu ++ [{"LightWave / Modo (.lwo|.lxo)...", lwo}];

menu_entry(export, Menu) ->
    Menu ++ [{"LightWave (.lwo)...", lwo}].

props(import) ->
    [{extensions,
      [{".lwo", ?__(1,"LightWave File")},
       {".lxo", ?__(2,"Modo File")}]}];

props(export) ->
    [{ext, ".lwo"},{ext_desc, ?__(1,"LightWave File")}].

-ifdef(DEBUG).

-define(DEBUG_TAG(Item), begin
    io:fwrite("       ~p\n", [Item]),
    Item
end).

-define(DEBUG(Item), begin
    io:fwrite("~s\n", [Item]),
    ok
end).

-define(DEBUG_B(Item), begin
    if
        is_atom(Item) -> String = atom_to_list(Item);
        is_list(Item) -> String = Item
    end,
    NumChars = length(String),
    io:fwrite("+-~s-+\n", [lists:duplicate(NumChars, "-")]),
    io:fwrite("| ~s |\n", [String]),
    io:fwrite("+-~s-+\n", [lists:duplicate(NumChars, "-")]),
    ok
end).

-else.

-define(DEBUG_TAG(Item), Item).

-define(DEBUG(Item), ok).

-define(DEBUG_B(Item), ok).

-endif.


-define(SINT, big-signed-integer).
-define(UINT, big-unsigned-integer).
-define(UW, :16/?UINT).
-define(FLT, :32/big-float).
-define(FLT8, :64/big-float).
-define(ID, :4/bytes).

gv(A, List, D) ->
    proplists:get_value(A, List, D).

gv(A, List) ->
    proplists:get_value(A, List, none).

%%% ===============================
%%% === LightWave Export (LWO2) ===
%%% ===============================

make_objects_chunk(Objs, MaterialDict) ->
    NumObjs = length(Objs),
    Indices = lists:seq(NumObjs-1, 0, -1),  %% Reversed
    IndexedObjs = lists:zip(Objs, Indices),
    ObjectChunks = [make_object(Obj, ObjIdx, MaterialDict) || {Obj, ObjIdx} <- IndexedObjs],
    ObjectChunks.

make_object(Obj, ObjIdx, MaterialDict) ->
    #e3d_object{name=Name,obj=Mesh} = Obj,
    #e3d_mesh{vs=Vs,fs=Fs,vc=Vcolors,tx=UVcoords} = Mesh,
    Layr = make_layr(Name, ObjIdx),
    Pnts = make_pnts(Vs),
    BBox = make_bbox(Vs),
    Pols = make_pols(Fs),
    Ptag = make_ptag(Fs, MaterialDict),
    HasVCs = length(Vcolors)>0,
    HasUVs = length(UVcoords)>0,
    %io:fwrite("~p HasUV=~p, HasVC=~p\n", [Name, HasUVs, HasVCs]),
    case {HasVCs, HasUVs} of
	{true, true} -> %% Wings can't have both at the same time
	    VmadVC = make_vmad_vc(Mesh),
	    VmadUV = make_vmad_uv(Mesh),
	    ObjectChunks = [Layr, Pnts, BBox, Pols, Ptag, VmadVC, VmadUV];
	{false, true} ->
	    VmadUV = make_vmad_uv(Mesh),
	    ObjectChunks = [Layr, Pnts, BBox, Pols, Ptag, VmadUV];
	{true, false} ->
	    VmadVC = make_vmad_vc(Mesh),
	    ObjectChunks = [Layr, Pnts, BBox, Pols, Ptag, VmadVC];
	{false, false} ->
	    ObjectChunks = [Layr, Pnts, BBox, Pols, Ptag]
    end,
    ObjectChunks.

make_tags(Mats) ->
    Names1 = [atom_to_list(Name) || {Name, _} <- Mats],
    Names2 = [make_nstring(Name) || Name <- Names1],
    TagsChunk = list_to_binary(Names2),
    make_chunk('TAGS', TagsChunk).

make_material_dict(Mats) ->
    Names = [Name || {Name, _} <- Mats],
    A = lists:zip(Names, lists:seq(0, length(Names)-1)),
    MaterialDict = dict:from_list(A),
    MaterialDict.

make_surf(MaterialName, Color) ->
    Creator = "Wings3D",
    SurfName = make_nstring(MaterialName),
    SurfEnd = <<0,0>>,
    {R,G,B,_} = Color,
    RGB = <<R:32/float, G:32/float, B:32/float, 0:16>>,
    RGBLen = size(RGB),
    ColorData =  [<<"COLR">>, <<RGBLen:16>>, RGB],
    Comment = make_nstring(MaterialName ++ ": Exported from " ++ Creator),
    CommentLen = size(Comment),
    CommentData =  [<<"CMNT">>, <<CommentLen:16>>, Comment],
    SurfChunk = list_to_binary([SurfName,SurfEnd,ColorData,CommentData]),
    make_chunk('SURF', SurfChunk).

make_surfaces(_Vers, Mats) ->
    OpMats = [{Name, proplists:get_value(opengl, Mat)} || {Name,Mat} <- Mats],
    RGBAs = [{Name, proplists:get_value(diffuse, OpMat)} || {Name,OpMat} <- OpMats],
    Surfs = [make_surf(atom_to_list(N),C) || {N,C} <- RGBAs],
    Surfs.

make_layr(Name, LayerNum) ->
    LayerData = [
	<<LayerNum:16>>,				%% layer number
	<<0:16>>,					%% flags
	<<0:32/float>>, <<0:32/float>>, <<0:32/float>>, %% pivot
	make_nstring(Name)],				%% name
    LayerChunk = list_to_binary(LayerData),
    make_chunk('LAYR', LayerChunk).

make_pnts(Vs) ->
    VertToBinary = fun(Vertex) ->
	{X,Y,Z} = Vertex,
	<<X:32/float, Y:32/float, Z:32/float>> end,
    VertsData = lists:map(VertToBinary, Vs),
    VertsChunk = list_to_binary(VertsData),
    make_chunk('PNTS', VertsChunk).

make_bbox(Verts) ->
    A = e3d_vec:bounding_box(Verts),
    B = lists:map(fun erlang:tuple_to_list/1, A),
    C = lists:flatten(B),
    D = [<<I:32/float>> || I <- C],
    BboxData = list_to_binary(D),
    make_chunk('BBOX', BboxData).

make_pols(E3dFaces) ->
    FaceToBinary = fun(E3dFace) ->
			   #e3d_face{vs=FaceVs} = E3dFace,
			   NumFaceVerts = <<(length(FaceVs)):16>>,
			   FaceIndices = [generate_vx(Idx) || Idx <- lists:reverse(FaceVs)],
			   [NumFaceVerts | FaceIndices] 
		   end,
    PolygonType = <<"FACE">>,
    Polygons = lists:map(FaceToBinary, E3dFaces),
    FacesChunk = list_to_binary([PolygonType|Polygons]),
    make_chunk('POLS', FacesChunk).

make_ptag(E3dFaces, MaterialDict) ->
    MaterialToIdx = fun(E3dFace) ->
	#e3d_face{mat=FaceMats} = E3dFace,
	FaceMat = lists:nth(1, FaceMats),
	FaceMat2 = dict:fetch(FaceMat, MaterialDict),
	FaceMat2 end,
    TagType = <<"SURF">>,
    SurfaceIdxs = lists:map(MaterialToIdx, E3dFaces),
    NumFaces = length(E3dFaces),
    FaceIdxs = lists:seq(0, NumFaces-1),
    Tags0 = lists:zip(FaceIdxs, SurfaceIdxs),
    Tags = [[generate_vx(Fidx),<<Sidx:16>>] || {Fidx,Sidx} <- Tags0],
    TagsChunk = list_to_binary([TagType|Tags]),
    make_chunk('PTAG', TagsChunk).

make_nstring(String) ->
    case (length(String) rem 2) of
	0 -> Nstring = String ++ "\0\0";
	1 -> Nstring = String ++ "\0"
    end,
    list_to_binary(Nstring).

generate_vx(Index) ->
    case (Index < 65280) of
	true -> <<Index:16>>;			%% 2-byte index
	false -> <<(Index bor 16#FF000000):32>> %% 4-byte index
    end.

make_chunk(ChunkName, ChunkData) ->
    ChunkName2 = list_to_binary(atom_to_list(ChunkName)),
    ChunkSize = size(ChunkData),
    [<<ChunkName2/binary, ChunkSize:32, ChunkData/binary>>].

make_header(C, Chunks) ->
    DataSize = size(list_to_binary(Chunks)),
    FormSize = DataSize + length("LWO2"),
    A = <<"FORM">>,
    B = <<FormSize:32>>,
    [A,B,C].

make_vmad_vc(Mesh) ->
    #e3d_mesh{vc=Vcolors,fs=E3dFaces} = Mesh,
    Type = <<"RGB ">>,
    Dimension = <<3:16>>,
    Name = make_nstring("Wings3D's Vertex Colors"),
    IndexedFaces = add_indices(E3dFaces),
    VCdata = make_vmad_vc2(IndexedFaces, Vcolors),
    VmadData = list_to_binary([Type, Dimension, Name, VCdata]),
    make_chunk('VMAD', VmadData).

make_vmad_vc2(IndexedFaces, Vcolors) ->
    FaceToBinary = fun(IndexedFace) ->
	{E3dFace,FaceIdx} = IndexedFace,
	#e3d_face{vc=FaceVCs,vs=FaceVerts} = E3dFace,
	FaceIdxs = lists:duplicate(length(FaceVerts), FaceIdx),
	RawFaceVCs = [lists:nth(Index+1, Vcolors) || Index <- FaceVCs],
	FaceVertsB = [<<I:16>> || I <- FaceVerts],
	FaceIdxsB  = [<<I:16>> || I <- FaceIdxs],
	RawFaceVCsB = [<<R:32/float, G:32/float, B:32/float>> || {R,G,B} <- RawFaceVCs],
	Line = zip_lists_3(FaceVertsB, FaceIdxsB, RawFaceVCsB),
	list_to_binary(Line)
	end,
    Vmads = lists:map(FaceToBinary, IndexedFaces),
    Vmads.

zip_lists_3(A, B, C) ->
    L = lists:map(fun erlang:tuple_to_list/1, lists:zip3(A,B,C)),
    lists:flatten(L).

make_vmad_uv(Mesh) ->
    #e3d_mesh{tx=UVcoords,fs=E3dFaces} = Mesh,
    Type = <<"TXUV">>,
    Dimension = <<2:16>>,
    Name = make_nstring("Wings3D's UV Coords"),
    IndexedFaces = add_indices(E3dFaces),
    UVdata = make_vmad_uv2(IndexedFaces, UVcoords),
    VmadData = list_to_binary([Type, Dimension, Name, UVdata]),
    make_chunk('VMAD', VmadData).

make_vmad_uv2(IndexedFaces, UVcoords) ->
    FaceToBinary = fun(IndexedFace) ->
	{E3dFace,FaceIdx} = IndexedFace,
	#e3d_face{tx=FaceUVs,vs=FaceVerts} = E3dFace,
	FaceIdxs = lists:duplicate(length(FaceVerts), FaceIdx),
	RawFaceUvs = [lists:nth(Index+1, UVcoords) || Index <- FaceUVs],
	RawFaceUvsB = [<<U:32/float, V:32/float>> || {U,V} <- RawFaceUvs],
	case RawFaceUvs of
	    [] -> [];
	    _ ->
		FaceVertsB = [<<I:16>> || I <- FaceVerts],
		FaceIdxsB  = [<<I:16>> || I <- FaceIdxs],
		Line = zip_lists_3(FaceVertsB, FaceIdxsB, RawFaceUvsB),
		list_to_binary(Line)
	end
    end,
    Vmads = lists:map(FaceToBinary, IndexedFaces),
    lists:flatten(Vmads).

make_auth(Creator) ->
    Comment = make_nstring("----> Exported from: " ++ Creator ++ " <----"),
    make_chunk('AUTH', Comment).

%%% LWO Export
%%% ==========

export(Vers, FileName, Contents) ->
    LWO = make_lwo(Vers, Contents),
    file:write_file(FileName, LWO).

make_lwo(Vers, Contents0) ->
    FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
    Contents = e3d_file:transform(Contents0, FlipX),
    #e3d_file{objs=Objs,mat=Mats,creator=Creator} = Contents,
    Tags = make_tags(Mats),
    Surfs = make_surfaces(Vers, Mats),
    Maker = make_auth(Creator),
    MaterialDict = make_material_dict(Mats),
    ObjectChunks = make_objects_chunk(Objs, MaterialDict),
    Chunks = [Maker, Tags, ObjectChunks|Surfs],
    make_lwo_combine(Vers, Chunks).
make_lwo_combine(lwo2, Chunks) ->
    LwoHeader = make_header(<<"LWO2">>, Chunks),
    LwoData = [LwoHeader|Chunks],
    LwoData.




%%% ========================
%%% === LightWave Import ===
%%% ========================

read_cstring(Data) ->
    read_cstring(Data, []).

read_cstring(<<0, Rest/binary>>, Name) ->
    case (length(Name) rem 2) of
	0 -> <<0, More/binary>> = Rest, {Name, More};
	1 -> {Name, Rest}
    end;
read_cstring(Data, Name) ->
    <<Char:8, Rest/binary>> = Data,
    Name2 = Name ++ [Char],
    read_cstring(Rest, Name2).

read_nstring(String) ->
    NotNull = fun(X) -> X =/= 0 end,
    Name = lists:takewhile(NotNull, String),
    case length(Name)>0 of
	true -> Name;
	false -> "Not Named"
    end.

read_header(<<"FORM", _:32, SubForm:4/bytes, Rest/binary>>=Content) ->
    case SubForm of
        <<"LWOB">> -> {lwob, Rest};
        <<"LWO2">> -> {lwo2, Rest};
        <<"LXOB">> -> {lwo2, Rest};
        <<"LWO3">> -> {lwo3, Content};
        _ ->
            wings_u:error_msg(
                ?__(1,"LWO files in v5.5 format are not supported."))
    end;
read_header(_) ->
    wings_u:error_msg(?__(2,"Not an LWO (FORM) file.")).

read_tags(Data) ->
    TagsList = binary_to_list(Data),
    TagsList2 = string:tokens(TagsList, [0]),
    lists:map(fun erlang:list_to_atom/1, TagsList2).

read_layr(Data) ->
    <<_:16/binary, BiName/binary>> = Data,
    LayerName = read_nstring(binary_to_list(BiName)),
    {{0.0, 0.0, 0.0}, LayerName, -1}.

read_pnts(<<>>) -> [];
read_pnts(Data) ->
    <<X:32/float, Y:32/float, Z:32/float, Rest/binary>> = Data,
    [{X,Y,Z} | read_pnts(Rest)].

read_pols(<<"FACE",Rest/binary>>) ->
    read_point_idxs(Rest);
read_pols(<<"PTCH",Rest/binary>>) ->
    read_point_idxs(Rest);
read_pols(<<"SUBD",_/binary>>) ->
    wings_u:error_msg(?__(1,"LWO files containing \"SUBD\" sub-forms in "
		  "\"POLS\" not supported.")).

read_point_idxs(<<>>) -> [];
read_point_idxs(Data) ->
    <<NumFaceVerts:16/integer, Rest/binary>> = Data,
    {Indexes, More} = read_nvals(Rest, NumFaceVerts),
    [Indexes | read_point_idxs(More)].

read_nvals(Data, N) ->
    read_nvals([], Data, N).

read_nvals(List, Data, 0) -> {List, Data};
read_nvals(List, Data, N) ->
    {Index, Rest} = read_vx(Data),
    List2 = lists:append(List, [Index]),
    read_nvals(List2, Rest, N-1).

read_vx(Data) ->
    case Data of
	<<255:8, Index:24, Rest/binary>> -> Index;
	<<Index:16, Rest/binary>> -> Index
    end,
    {Index, Rest}.


read_ptag(Data) ->
    <<Ptag:4/binary, Rest/binary>> = Data,
    case Ptag of
        <<"SURF">> -> read_ptag_l(surfidx,Rest);
        <<"MATR">> -> read_ptag_l(surfidx,Rest);
        <<"COLR">> -> read_ptag_l(color,Rest);

        _ -> list_to_atom(binary_to_list(Ptag))
    end.

read_ptag_l(Atom, Data) ->
    {Atom, read_ptag_l_1(Data, [])}.
read_ptag_l_1(<<>>, Acc) -> Acc;
read_ptag_l_1(Data, Acc) ->
    {FaceIndex, Rest} = read_vx(Data),
    {SurfIndex, More} = read_vx(Rest),
    [{FaceIndex,SurfIndex} | read_ptag_l_1(More, Acc)].


read_vmap(<<Type?ID, Dim?UW, Values/binary>>) ->
    {Name, Bin1} = read_cstring(Values),
    {Type, Name, read_vmap_list(Dim, Bin1)}.

read_vmap_list(Dim, Bin) ->
    read_vmap_list(Dim, Bin, []).
read_vmap_list(_Dim, <<>>, L) ->
    lists:reverse(L);
read_vmap_list(Dim, Bin, L) ->
    {Vert, Bin2} = read_vx(Bin),
    {Values, Bin3} = read_vmap_dim(Dim, Bin2),
    read_vmap_list(Dim, Bin3, [{Vert, Values}|L]).

read_vmap_dim(1, <<F1?FLT,Bin/binary>>) ->
    {F1, Bin};
read_vmap_dim(2, <<F1?FLT,F2?FLT,Bin/binary>>) ->
    {{F1,F2}, Bin};
read_vmap_dim(3, <<F1?FLT,F2?FLT,F3?FLT,Bin/binary>>) ->
    {{F1,F2,F3}, Bin};
read_vmap_dim(4, <<F1?FLT,F2?FLT,F3?FLT,F4?FLT,Bin/binary>>) ->
    {{F1,F2,F3,F4}, Bin}.

read_bbox(<<MinX?FLT,MinY?FLT,MinZ?FLT,
          MaxX?FLT,MaxY?FLT,MaxZ?FLT>>) ->
    {{MinX, MinY, MinZ}, {MaxX, MaxY, MaxZ}}.


read_vmad(<<Type?ID, Dim?UW, Values/binary>>) ->
    {Name, Bin1} = read_cstring(Values),
    {Type, Name, read_vmad_list(Dim, Bin1)}.

read_vmad_list(Dim, Bin) ->
    read_vmad_list(Dim, Bin, []).
read_vmad_list(_Dim, <<>>, L) ->
    lists:reverse(L);
read_vmad_list(Dim, Bin, L) ->
    {Vert, Bin2} = read_vx(Bin),
    {Poly, Bin3} = read_vx(Bin2),
    {Values, Bin4} = read_vmad_dim(Dim, Bin3),
    read_vmad_list(Dim, Bin4, [{{Vert, Poly}, Values}|L]).

read_vmad_dim(1, <<F1?FLT,Bin/binary>>) ->
    {F1, Bin};
read_vmad_dim(2, <<F1?FLT,F2?FLT,Bin/binary>>) ->
    {{F1,F2}, Bin};
read_vmad_dim(3, <<F1?FLT,F2?FLT,F3?FLT,Bin/binary>>) ->
    {{F1,F2,F3}, Bin};
read_vmad_dim(4, <<F1?FLT,F2?FLT,F3?FLT,F4?FLT,Bin/binary>>) ->
    {{F1,F2,F3,F4}, Bin}.


read_vmpa(<<UVType:32/?UINT,Col:32/?UINT>>) ->
    {UVType, Col}.



%%% LWOB Import
%%% ===========

read_lwob(Bin) ->
    erlang:put(lwob_clipid, 1),
    read_lwob(Bin, []).

read_lwob(<<>>, Chunks) ->
    erlang:erase(lwob_clipid),
    lists:reverse(Chunks);
read_lwob(Data, Chunks) ->
    {Rest, Chunks2} = lwob_read_chunk(Data, Chunks),
    read_lwob(Rest, Chunks2).

lwob_read_chunk(Data, Chunks) ->
    <<ChunkId:4/binary, ChunkSize:32, Rest/binary>> = Data,
    <<ChunkData:ChunkSize/binary, Rest2/binary>> = Rest,
    ChunkSize = size(ChunkData),
    case lwob_c(ChunkId,ChunkData) of
        {srfs,_}=Chunk ->
            {Rest2, lwob_srfs_pnts(Chunk, Chunks)};
        {pnts,_}=Chunk ->
            {Rest2, lwob_srfs_pnts(Chunk, Chunks)};
        {poly, List} ->
            Poly = [Vs || {Vs,_} <- List],
            SurfIdx = lwob_surfidx(List),
            {Rest2, [
                {o,{ptag,{surfidx,SurfIdx}}},
                {o,{poly,Poly}}|Chunks]};
        {srftx,SurfTex} ->
            {Rest2, lwob_surf_2(SurfTex, Chunks)};
        Chunk ->
            {Rest2, [Chunk|Chunks]}
    end.

lwob_c(<<"SRFS">>, ChunkData) ->
    ?DEBUG_TAG({srfs, read_tags(ChunkData)});
lwob_c(<<"PNTS">>, ChunkData) ->
    ?DEBUG_TAG({pnts, read_pnts(ChunkData)});
lwob_c(<<"POLS">>, ChunkData) ->
    ?DEBUG_TAG({poly, lwob_pols(ChunkData)});
lwob_c(<<"SURF">>, ChunkData) ->
    ?DEBUG_TAG({srftx, lwob_surf(ChunkData)});
lwob_c(ChunkId, _ChunkData) ->
    ChunkName = binary_to_list(ChunkId) ++ " Chunk",
    ?DEBUG_TAG(list_to_atom(ChunkName)).

lwob_pols(<<>>) ->
    [];
lwob_pols(<<Num?UW,R0/binary>>) ->
    {Verts, <<S?UW,R/binary>>} = lwob_pols_v(Num, R0, []),
    [{Verts, S}|lwob_pols(R)].
lwob_pols_v(0, R, Vs) ->
    {lists:reverse(Vs), R};
lwob_pols_v(Num, <<V?UW,R/binary>>, Vs) ->
    lwob_pols_v(Num-1, R, [V|Vs]).

-record(lwobmat, {
    name,
    ps,
    tx,
    cl
}).

%% Read surf (for LWOB)
lwob_surf(Data) ->
    {MaterialName, Rest} = read_cstring(Data),
    A = lwob_surf_2c(Rest, []),
    <<R,G,B,_>> = gv(<<"COLR">>, A, <<190,190,190,0>>),
    
    Id = erlang:get(lwob_clipid),
    erlang:put(lwob_clipid, Id+2),
    %% Texture images
    CTex_0 = [
        {t, strz(gv(<<"TIMG">>, A, <<0,0>>)), Id},
        {r, strz(gv(<<"RIMG">>, A, <<0,0>>)), Id+1}
    ],
    
    RGB = {R/255.0,G/255.0,B/255.0},
    Diff = lwob_float_or_fixed(<<"VDIF">>, <<"DIFF">>, A, 1.0),
    Spec = lwob_float_or_fixed(<<"VSPC">>, <<"SPEC">>, A, 0.0),
    Tran = lwob_float_or_fixed(<<"VTRN">>, <<"TRAN">>, A, 0.0),
    Lumi = lwob_float_or_fixed(<<"VLUM">>, <<"LUMI">>, A, 0.0),
    Refl = lwob_float_or_fixed(<<"VRFL">>, <<"REFL">>, A, 0.0),

    CTex = [ T || T <- CTex_0, length(element(2,T)) > 0 ],
    Tex = [
        case T of
            {t,_,Id} -> {diffuse,Id};
            {r,_,Id} -> {metallic,Id}
        end
    || T <- CTex],

    MatPs = #{
        "color"=>RGB,
        "diffuse"=>Diff,
        "specular"=>Spec,
        "transparency"=>Tran,
        "reflection"=>Refl,
        "luminosity"=>Lumi
    },
    #lwobmat{name=list_to_atom(MaterialName), ps=MatPs, tx=Tex, cl=CTex}.

lwob_float_or_fixed(Flt, Fix, A, V) ->
    case {proplists:is_defined(Flt, A),proplists:is_defined(Fix, A)} of
        {true,_} ->
            <<V1?FLT>> = gv(Flt, A, <<0,0,0,0>>),
            V1;
        {_,true} ->
            <<V0?UW>> = gv(Fix, A, <<0,0>>),
            V0 / 255.0;
        _ ->
            V
    end.
    

lwob_surf_2c(<<>>, Acc) ->
    lists:reverse(Acc);
lwob_surf_2c(<<C:4/bytes,S?UW,Data/binary>>, Acc) ->
    CD = binary:part(Data, {0, S}),
    Rest = binary:part(Data, {S, byte_size(Data)-S}),
    lwob_surf_2c(Rest, [{C,CD}|Acc]).

lwob_surfidx(List) ->
    lwob_surfidx(List, 0, []).
lwob_surfidx([], _, IL) ->
    lists:reverse(IL);
lwob_surfidx([{_,Idx}|List], Num, IL) ->
    lwob_surfidx(List, Num+1, [{Num,Idx-1}|IL]).

lwob_srfs_pnts({srfs,List}, [{o,{pnts,_}}=A|Chunks]) ->
    [A,lwob_layr(),{o,{tags,List}}|Chunks];
lwob_srfs_pnts({pnts,_}=Chunk, [{o,{tags,_}}|_]=Chunks) ->
    [{o,Chunk},lwob_layr()|Chunks];
lwob_srfs_pnts({srfs,List}, Chunks) ->
    [{o,{tags,List}}|Chunks];
lwob_srfs_pnts(Chunk, Chunks) ->
    [{o,Chunk}|Chunks].

lwob_layr() ->
    {o, {layr, {none,unnamed,none}}}.

lwob_surf_2(#lwobmat{name=Name,ps=Mat,tx=Tex,cl=TL}, Chunks) ->
    [{m,{surf_tex,{Name,Mat},Tex}}|lwob_surf_2_(TL,Chunks,Name)].
lwob_surf_2_([], Chunks, _Name) ->
    Chunks;
lwob_surf_2_([{_,"",_}|List], Chunks, Name) ->
    lwob_surf_2_(List, Chunks, Name);
lwob_surf_2_([{_,Tex,Id}|List], Chunks, Name) ->
    M = {m,{clip,{Id,{file,unnamed,Tex}}}},
    [M|lwob_surf_2_(List, Chunks, Name)].



%%% LWO2 Import
%%% ===========

read_lwo2(Bin) ->
    read_lwo2(Bin, []).

read_lwo2(<<>>, Chunks) ->
    lists:reverse(Chunks);
read_lwo2(Data, Chunks) ->
    {Rest, Chunks2} = lwo2_read_chunk(Data, Chunks),
    read_lwo2(Rest, Chunks2).

lwo2_read_chunk(Data, Chunks) ->
    <<ChunkId:4/binary, ChunkSize:32, Rest/binary>> = Data,
    <<ChunkData:ChunkSize/binary, Rest2/binary>> = Rest,
    ChunkSize = size(ChunkData),
    case lwo2_c(ChunkId,ChunkData) of
        {surf_tex, SurfChunk, Tex} ->
            Chunk_1 = {m,{surf_tex,SurfChunk, Tex}},
            {Rest2, [Chunk_1|Chunks]};
        Chunk ->
            {Rest2, [Chunk|Chunks]}
    end.

lwo2_c(<<"BBOX">>, ChunkData) ->
    ?DEBUG_TAG({bbox, read_bbox(ChunkData)});
lwo2_c(<<"CLIP">>, ChunkData) ->
    ?DEBUG_TAG({m, {clip, lwo2_clip(ChunkData)}});
lwo2_c(<<"TAGS">>, ChunkData) ->
    ?DEBUG_TAG({o, {tags, read_tags(ChunkData)}});
lwo2_c(<<"LAYR">>, ChunkData) ->
    ?DEBUG_TAG({o, {layr, read_layr(ChunkData)}});
lwo2_c(<<"PNTS">>, ChunkData) ->
    ?DEBUG_TAG({o, {pnts, read_pnts(ChunkData)}});
lwo2_c(<<"POLS">>, ChunkData) ->
    ?DEBUG_TAG({o, {poly, read_pols(ChunkData)}});
lwo2_c(<<"PTAG">>, ChunkData) ->
    ?DEBUG_TAG({o, {ptag, read_ptag(ChunkData)}});
lwo2_c(<<"SURF">>, ChunkData) ->
    ?DEBUG_TAG(lwo2_surf(ChunkData));
lwo2_c(<<"VMAP">>, ChunkData) ->
    ?DEBUG_TAG({o, {vmap, read_vmap(ChunkData)}});
lwo2_c(<<"VMAD">>, ChunkData) ->
    {o, {vmad, read_vmad(ChunkData)}};
lwo2_c(<<"VMPA">>, ChunkData) ->
    {vmpa, read_vmpa(ChunkData)};

lwo2_c(<<"ITEM">>, ChunkData) ->
    {lxob, lxob_ch(ChunkData)};

lwo2_c(ChunkId, _ChunkData) ->
    ChunkName = binary_to_list(ChunkId) ++ " Chunk",
    ?DEBUG_TAG(list_to_atom(ChunkName)).

%% Read surf (for LWO2)
lwo2_surf(Data) ->
    {MaterialName, Rest0} = read_cstring(Data),
    {Source, Rest} = read_cstring(Rest0),
    A = lwo2_surf_2c(Rest),
    RGB = gv(<<"COLR">>, A, {0.9,0.9,0.9}),
    Diff = gv(<<"DIFF">>, A, 1.0),
    Spec = gv(<<"SPEC">>, A, 0.0),
    Lumi = gv(<<"LUMI">>, A, 0.0),
    Refl = gv(<<"REFL">>, A, 0.0),
    Tran = gv(<<"TRAN">>, A, 0.0),

    Tex = lwo2_surf_imap(A),

    MatPs = #{
        "color"=>RGB,
        "diffuse"=>Diff,
        "specular"=>Spec,
        "luminosity"=>Lumi,
        "transparency"=>Tran,
        "reflection"=>Refl
    },
    {surf_tex, {surf_name(MaterialName, Source), MatPs}, Tex}.

lwo2_surf_2c(Bin) ->
    lwo2_surf_2c(Bin, []).
lwo2_surf_2c(<<>>, Acc) ->
    lists:reverse(Acc);
lwo2_surf_2c(<<C:4/bytes,S?UW,Data/binary>>, Acc) ->
    CD = binary:part(Data, {0, S}),
    Rest = binary:part(Data, {S, byte_size(Data)-S}),
    lwo2_surf_2c(Rest, [{C,lwo2_surf_2c_1(C, CD)}|Acc]).

lwo2_surf_2c_1(<<"BLOK">>, Data) ->
    lwo2_surf_2c(Data, []);
lwo2_surf_2c_1(<<"IMAP">>, <<_?UW,Data/binary>>) ->
    lwo2_surf_2c(Data, []);
lwo2_surf_2c_1(<<"TMAP">>, Data) ->
    lwo2_surf_2c(Data, []);
lwo2_surf_2c_1(<<"NODS">>, Data) ->
    lwo2_surf_2c(Data, []);
lwo2_surf_2c_1(<<"COLR">>, <<R?FLT,G?FLT,B?FLT,_/binary>>) ->
    {R,G,B};
lwo2_surf_2c_1(<<"VMAP">>, Data) ->
    {Str, _} = read_cstring(Data),
    Str;
lwo2_surf_2c_1(<<"STIL">>, Data) ->
    {Str, _} = read_cstring(Data),
    Str;

lwo2_surf_2c_1(<<"DIFF">>,<<F?FLT,_Env/binary>>) -> F;
lwo2_surf_2c_1(<<"SPEC">>,<<F?FLT,_Env/binary>>) -> F;
lwo2_surf_2c_1(<<"LUMI">>,<<F?FLT,_Env/binary>>) -> F;
lwo2_surf_2c_1(<<"REFL">>,<<F?FLT,_Env/binary>>) -> F;
lwo2_surf_2c_1(<<"TRAN">>,<<F?FLT,_Env/binary>>) -> F;
lwo2_surf_2c_1(<<"IMAG">>,<<E?UW>>) -> E;

lwo2_surf_2c_1(<<"WRAP">>,<<WWrap?UW,HWrap?UW>>) -> {WWrap,HWrap};
lwo2_surf_2c_1(<<"WRPW">>,<<F?FLT,0,0>>) -> F;
lwo2_surf_2c_1(<<"WRPH">>,<<F?FLT,0,0>>) -> F;
lwo2_surf_2c_1(<<"PIXB">>,<<E?UW>>) -> E;
lwo2_surf_2c_1(_, CD) ->
    CD.

lwo2_surf_imap(L) ->
    lwo2_surf_imap(L, []).
lwo2_surf_imap([{<<"BLOK">>,B}|L], OL) ->
    lwo2_surf_imap(L, [lwo2_surf_imap_1(B)|OL]);
lwo2_surf_imap([_|L], OL) ->
    lwo2_surf_imap(L, OL);
lwo2_surf_imap([], OL) ->
    lists:reverse([A || A <- OL, is_tuple(A)]).

lwo2_surf_imap_1([{<<"IMAP">>,Header}|IMap]) ->
    CType_0 = gv(<<"CHAN">>, Header, <<"COLR">>),
    CType = case CType_0 of
        <<"COLR">> -> diffuse;
        <<"DIFF">> -> diffuse;
        <<"LUMI">> -> emission;
        <<"SPEC">> -> roughness;
        <<"GLOS">> -> gloss;
        <<"REFL">> -> metallic;
        <<"RIND">> -> normal;
        <<"BUMP">> -> bump;
        _ -> unknown
    end,
    Index = gv(<<"IMAG">>, IMap, 0),
    {CType,Index};
lwo2_surf_imap_1([{_, _}|_]) ->
    false.


lwo2_clip(<<Id:32/?UINT,Rest/binary>>) ->
    A = lwo2_surf_2c(Rest),
    case gv(<<"STIL">>, A) of
        none ->
            {-1, {none, none, none}};
        FileName when is_list(FileName) ->
            {Id, {file,unnamed,FileName}}
    end.

lxob_ch(Data) ->
    {Str1, Bin0} = read_cstring(Data),
    {Str2, <<_?UW,Num?UW,Bin/binary>>} = read_cstring(Bin0),
    {Str1, {Num, Str2}, lxob_ch_2c(Bin)}.
    
lxob_ch_2c(<<>>) ->
    [];
lxob_ch_2c(<<Id?ID,Size?UW,Bin/binary>>) ->
    C = binary:part(Bin, {0, Size}),
    CD = binary:part(Bin, {Size, byte_size(Bin)-Size}),
    [{Id, C}|lxob_ch_2c(CD)].




%%% LWO3 Import
%%% ===========

%% Read the nested FORMs.
%% 
read_lwo3(Bin) ->
    read_lwo3(Bin, [], []).
read_lwo3(<<>>, _Context, OL) ->
    lists:reverse(OL);
read_lwo3(<<SubTag?ID, Size:32/?UINT, Bin/binary>>, Context, OL) ->
    A1 = binary:part(Bin, {0, Size}),
    A2 = binary:part(Bin, {Size, byte_size(Bin)-Size}),
    {SubTag_1, Content} = case SubTag of
        <<"FORM">> ->
            <<SubForm?ID,A1_1/binary>> = A1,
            {{form, SubForm}, read_lwo3(A1_1, [SubForm|Context], [])};
        _ ->
            lwo3_c(SubTag, Context, A1)
    end,
    read_lwo3(A2, Context, [{SubTag_1, Content}|OL]).

lwo3_c(<<"PTAG">>, [<<"LWO3">>], Values) ->
    {o, {ptag, read_ptag(Values)}};
lwo3_c(<<"TAGS">>, [<<"LWO3">>], Values) ->
    {o, {tags, read_tags(Values)}};
lwo3_c(<<"PNTS">>, [<<"LWO3">>], Values) ->
    {o, {pnts, read_pnts(Values)}};
lwo3_c(<<"POLS">>, [<<"LWO3">>], Values) ->
    {o, {poly, read_pols(Values)}};
lwo3_c(<<"LAYR">>, [<<"LWO3">>], Bin) ->
    {o, {layr, lwo3_layr(Bin)}};

lwo3_c(<<"OTAG">>, [<<"LWO3">>], <<Type?ID, Values/binary>>) ->
    {otag, {Type, strz(Values)}};
lwo3_c(<<"VMAD">>, [<<"LWO3">>], Values) ->
    {o, {vmad, read_vmad(Values)}};
lwo3_c(<<"VMPA">>, [<<"LWO3">>], Values) ->
    {vmpa, read_vmpa(Values)};
lwo3_c(<<"VMAP">>, [<<"LWO3">>], Values) ->
    {o, {vmap, read_vmap(Values)}};

lwo3_c(<<"BBOX">>, _Context, Values) ->
    {bbox, read_bbox(Values)};

lwo3_c(<<"NLOC">>, [<<"NROT">>|_], <<X:32/?SINT,Y:32/?SINT>>) ->
    {nloc, {X, Y}};
lwo3_c(<<"NZOM">>, [<<"NROT">>|_], <<Zoom?FLT>>) ->
    {nzom, Zoom};
lwo3_c(<<"NSTA">>, [<<"NROT">>|_], <<State?UW>>) ->
    {nsta, State};
lwo3_c(<<"NVER">>, [<<"NODS">>|_], <<Version:32/?UINT>>) ->
    {nver, Version};
lwo3_c(<<"NSRV">>, [<<"NNDS">>|_], Values) ->
    {nsrv, strz(Values)};

lwo3_c(<<"NRNM">>, [<<"NTAG">>|_], Values) ->
    {nrnm, strz(Values)};
lwo3_c(<<"NNME">>, [<<"NTAG">>|_], Values) ->
    {nnme, strz(Values)};
lwo3_c(<<"NCRD">>, [<<"NTAG">>|_], <<X:32/?UINT,Y:32/?SINT>>) ->
    {ncrd, {X, Y}};
lwo3_c(<<"NMOD">>, [<<"NTAG">>|_], <<Mode:32/?UINT>>) ->
    {nmod, Mode};
lwo3_c(<<"NPRW">>, [<<"NTAG">>|_], Values) ->
    {preview, strz(Values)};
lwo3_c(<<"NPLA">>, [<<"NTAG">>|_], <<Placement:32/?UINT>>) ->
    {npla, Placement};

lwo3_c(<<"INME">>, [<<"NCON">>|_], Values) ->
    {inme, strz(Values)};
lwo3_c(<<"IINM">>, [<<"NCON">>|_], Values) ->
    {iinm, strz(Values)};
lwo3_c(<<"IINN">>, [<<"NCON">>|_], Values) ->
    {iinm, strz(Values)};
lwo3_c(<<"IONM">>, [<<"NCON">>|_], Values) ->
    {ionm, strz(Values)};

lwo3_c(<<"FLAG">>, _, <<Flag:32/?UINT>>) ->
    {flag, Flag};
lwo3_c(<<"NAME">>, [<<"ENTR">>|_], Values) ->
    {name, strz(Values)};
lwo3_c(<<"TAG ">>, [<<"ENTR">>|_], Bin) ->
    {nsel, strz_list(Bin)};

lwo3_c(<<"VERS">>, _Context, <<Version:32/?UINT>>) ->
    {vers, Version};

lwo3_c(<<"ENUM">>, [<<"META">>|_], <<Enum:32/?UINT>>) ->
    {enum, Enum};

lwo3_c(<<"CHAN">>, [<<"IMAP">>|_], Values) ->
    {chan, lwo3_chan(Values)};
lwo3_c(<<"NEGA">>, [<<"IMAP">>|_], <<Invert?UW>>) ->
    {nega, Invert};
lwo3_c(<<"CSYS">>, [<<"TMAP">>|_], <<Type?UW>>) ->
    {csys, Type};
lwo3_c(<<"OREF">>, [<<"TMAP">>|_], Values) ->
    {oref, strz(Values)};

lwo3_c(<<"PROJ">>, _Context, <<Proj?UW>>) ->
    {proj, Proj};
lwo3_c(<<"AXIS">>, _Context, <<Axis?UW>>) ->
    {axis, Axis};
lwo3_c(<<"IMAG">>, _Context, Values) ->
    {TImg, <<>>} = read_vx(Values),
    {imag, TImg};
lwo3_c(<<"WRAP">>, _Context, <<WWrap?UW,HWrap?UW>>) ->
    {wrap, {WWrap, HWrap}};
lwo3_c(<<"AAST">>, _Context, <<Flags?UW,AAValue?FLT>>) ->
    {aast, {Flags, AAValue}};
lwo3_c(<<"PIXB">>, _Context, <<Pixb?UW>>) ->
    {pixb, Pixb};

lwo3_c(<<"NCOM">>, _Context, Values) ->
    {comment, strz(Values)};
lwo3_c(<<"SSHN">>, _Context, Values) ->
    {sshn, strz(Values)};
lwo3_c(<<"NSEL">>, _Context, <<NSel:32/?UINT>>) ->
    {nsel, NSel};

lwo3_c(<<"ENAB">>, _Context, <<Enab:16/?UINT>>) ->
    {enab, Enab};

lwo3_c(<<"VMAP">>, _Context, Values) ->
    {vmap, strz(Values)};

lwo3_c(<<"    ">>, [C|FormContext], Values) ->
    {form_data, lwo3_form_data(C, FormContext, Values)};
lwo3_c(A, _Context, B) ->
    {A, B}.


lwo3_form_data(<<"SURF">>, [<<"LWO3">>], Bin) ->
    %% The name of a surface
    {Name, Bin1} = read_cstring(Bin),
    Source = strz(Bin1),
    {Name, Source};
lwo3_form_data(<<"STIL">>, [<<"LWO3">>], Bin) ->
    %% Filename for an image
    FileName = strz(Bin),
    FileName;
lwo3_form_data(<<"VALU">>, [<<"ENTR">>,<<"ADAT">>|_], Values) ->
    %% Describes the data type of nested VALU
    case read_cstring(Values) of
        {Str, <<>>} ->
            Str;
        Pair ->
            Pair
    end;
lwo3_form_data(<<"CNTR">>, _, Values) ->
    lwo3_v_idx_env(Values);
lwo3_form_data(<<"SIZE">>, _, Values) ->
    lwo3_v_idx_env(Values);
lwo3_form_data(<<"ROTA">>, _, Values) ->
    lwo3_v_idx_env(Values);

lwo3_form_data(_, _, Values) ->
    Values.

strz(Values) ->
    case read_cstring(Values) of
        {V, <<>>} -> V
    end.

strz_list(Values) ->
    strz_list(Values, []).
strz_list(<<>>, L) ->
    lists:reverse(L);
strz_list(Values, L) ->
    {V, Bin} = read_cstring(Values),
    strz_list(Bin, [V|L]).

    
lwo3_chan(Bin) ->
    Bin.

lwo3_v_idx_env(<<X?FLT,Y?FLT,Z?FLT,Bin/binary>>) ->
    {Index, Bin1} = read_vx(Bin),
    {{X,Y,Z}, Index, Bin1}.

lwo3_layr(<<_Num?UW,_Flags?UW,X?FLT,Y?FLT,Z?FLT,Bin/binary>>) ->
    {Name, LayrPar} = read_cstring(Bin),
    Parent = case LayrPar of
        <<P?UW>> -> P;
        _ -> -1
    end,
    {{X, Y, Z}, read_nstring(Name), Parent}.


%%
%%

lwo3_flatten([{{form,<<"LWO3">>},Form}]) ->
    lwo3_flatten(Form, []);
lwo3_flatten(_) ->
    error("Not a valid LWO3 file.").

lwo3_flatten([], OL) ->
    lists:reverse(OL);
lwo3_flatten([{{form,<<"CLIP">>},Clip}|R], OL) ->
    lwo3_flatten(R, [{m,lwo3_fl_clip(Clip)}|OL]);
lwo3_flatten([{{form,<<"SURF">>},Surf}|R], OL) ->
    {Name, Surf_1, Tex} = lwo3_fl_surf(Surf),
    lwo3_flatten(R, [{m, {surf_tex, {Name, Surf_1}, Tex}}|OL]);
lwo3_flatten([Tup|R], OL) ->
    lwo3_flatten(R, [Tup|OL]).

lwo3_fl_clip(Form) ->
    <<Id:32/?UINT>>=gv(form_data,Form),
    case gv({form,<<"STIL">>},Form) of
        [{form_data,Bin}|_] ->
            {FileName,_} = read_cstring(Bin),
            {clip, {Id, {file, unnamed, FileName}}};
        _ ->
            {clip, {-1, {none, none, none}}}
    end.

lwo3_fl_surf([{form_data,{Name,Source}}|Form]) ->
    Nodes0 = gvl({form,<<"NODS">>}, Form),
    Nodes1 = gvl({form,<<"NNDS">>}, Nodes0),
    case lwo3_each_surf(Nodes1, none) of
        {Nodes, TexList} when is_list(Nodes) ->
            {surf_name(Name, Source), maps:from_list(Nodes), TexList};
        none ->
            {surf_name(Name, Source), none, []}
    end.

gvl(A, List) ->
    proplists:get_value(A, List, []).

lwo3_each_surf([], D) ->
    D;
lwo3_each_surf([{{form,<<"NTAG">>}, Form0}|L], D) ->
    case lwo3_surf_data(Form0) of
        {[], []} ->
            lwo3_each_surf(L, D);
        {SurfData, TexList} ->
            lwo3_each_surf(L, {SurfData, TexList})
    end;
lwo3_each_surf([_|L], D) ->
    lwo3_each_surf(L, D).

lwo3_surf_data(Form0) ->
    Form1 = gvl({form,<<"NDTA">>}, Form0),
    Form4A = gvl({form,<<"ADAT">>}, gvl({form,<<"META">>}, gvl({form,<<"SATR">>}, Form1))),
    Form4B = gvl({form,<<"ADAT">>}, gvl({form,<<"META">>}, gvl({form,<<"ATTR">>}, Form1))),
    lwo3_surf_data_1(Form4A ++ Form4B, [], []).

lwo3_surf_data_1([], OL, TL) ->
    {OL,TL};
lwo3_surf_data_1([{{form,<<"ENTR">>},Form}|L], OL, TL) ->
    {Name,{Col,Tex}} = lwo3_surf_data_entr(Form),
    TL_1 = if
        Tex =/= false ->
            IType = case Name of
                "bump" -> bump;
                "color" -> diffuse;
                "glossiness" -> gloss;
                "luminosity" -> emission;
                "reflection" -> metallic;
                "diffuse sharpness" -> roughness;
                _ -> unknown
            end,
            [{IType,Tex}|TL];
        true ->
            TL
    end,
    lwo3_surf_data_1(L, [{Name,Col}|OL], TL_1);
lwo3_surf_data_1([_|L], OL, TL) ->
    lwo3_surf_data_1(L, OL, TL).

lwo3_surf_data_entr(Form) ->
    Name0 = gvl(name, Form),
    Val0 = gvl({form,<<"VALU">>}, Form),
    Val1 = gvl({form,<<"VALU">>}, Val0),

    %% Get texture
    Tex = case gvl({form,<<"BLOK">>}, gvl({form,<<"TXTR">>}, Val0)) of
        [{{form,<<"IMAP">>},_}|ValT] ->
            ValT2 = gv(imag, ValT, 0),
            _ = gv(wrap, ValT, {1,1}),
            ValT2;
        _ ->
            false
    end,

    Name = string:lowercase(Name0),
    case gv(form_data, Val1) of
        none ->
            {Name, {none, Tex}};
        Val ->
            {Name, {lwo3_surf_data_val(Val, Name), Tex}}
    end.

lwo3_surf_data_val(<<0,0,0,4,V1?FLT8,V2?FLT8,V3?FLT8,V4?FLT8>>, _) ->
    {V1,V2,V3,V4};
lwo3_surf_data_val(<<0,0,0,3,V1?FLT8,V2?FLT8,V3?FLT8>>, _) ->
    {V1,V2,V3};
lwo3_surf_data_val(<<0,0,0,2,V1?FLT8,V2?FLT8>>, _) ->
    {V1,V2};
lwo3_surf_data_val(<<0,0,0,1,Val?FLT8>>, _) ->
    Val;
lwo3_surf_data_val(Val, _) ->
    Val.


%%%
%%%

-record(lobj, {
    layr,
    pnts,
    poly,
    surfidx,
    vmap_uv=none,
    vmad_uv=none,
    vmap_vc=none,
    vmad_vc=none,
    vmap_ns=none,
    vmad_ns=none
}).

next_layr([{layr,Layr}|Objs]) ->
    {O, Rest} = next_layr_1(Objs),
    {O#lobj{layr=Layr}, Rest};
next_layr([Skipped|Objs]) ->
    io:format("~p: " ++ ?__(1,"NOTE: Skipped: ") ++ "~p~n", [?MODULE,Skipped]),
    next_layr(Objs).
next_layr_1(Objs) ->
    next_layr_1(Objs, #lobj{}).
next_layr_1([]=Rest, O) ->
    {O, Rest};
next_layr_1([{layr,_}|_]=Rest, O) ->
    {O, Rest};
next_layr_1([Tag|Rest], O) ->
    O_1 = case Tag of
        {pnts, Pnts} ->
            O#lobj{pnts=Pnts};
        {poly, Poly} ->
            O#lobj{poly=Poly};
        {ptag, {surfidx, Ptag}} ->
            O#lobj{surfidx=Ptag};
        {ptag, _} ->
            O;
        {vmap, {Str,_,List}} when is_list(List) ->
            case Str of
                <<"TXUV">> -> O#lobj{vmap_uv=List};
                <<"RGBA">> -> O#lobj{vmap_vc=List};
                <<"RGB ">> -> O#lobj{vmap_vc=next_layr_1_rgba(List)};
                <<"NORM">> -> O#lobj{vmap_ns=List};
                _ -> O
            end;
        {vmad, {Str,_,List}} when is_list(List) ->
            case Str of
                <<"TXUV">> -> O#lobj{vmad_uv=List};
                <<"RGBA">> -> O#lobj{vmad_vc=List};
                <<"RGB ">> -> O#lobj{vmad_vc=next_layr_1_rgba(List)};
                <<"NORM">> -> O#lobj{vmad_ns=List};
                _ -> O
            end
    end,
    next_layr_1(Rest, O_1).

next_layr_1_rgba(List) ->
    [{Idx,rgba(RGB, 1.0)}
        || {Idx,RGB} <- List].


make_e3ds([{tags, Tags} | Objs]) ->
    make_e3ds(array:from_list(Tags), Objs).

make_e3ds(_Tags, []) -> [];
make_e3ds(_Tags, [{tags, Tags} | Objs]) ->
    make_e3ds(array:from_list(Tags), Objs);
make_e3ds(Tags, Objs) ->
    {Lobj, Rest} = next_layr(Objs),
    #lobj{layr={_,Name,_},pnts=Vs,
        poly=Fs,surfidx=SurfIdx,
        vmap_uv=Vmap_UV,vmad_uv=Vmad_UV,
        vmap_vc=Vmap_VC,vmad_vc=Vmad_VC,
        vmap_ns=Vmap_Nrm,vmad_ns=Vmad_Nrm
        }=Lobj,
    {_FaceIdxs,MaterialIdxs} = lists:unzip(SurfIdx),
    GetMatName = fun(Idx) -> array:get(Idx, Tags) end,
    Ms = lists:map(GetMatName, MaterialIdxs),
    {TxF, TxL} = vl0(Fs,Vmap_UV,Vmad_UV),
    {VcF, VcL} = vl0(Fs,Vmap_VC,Vmad_VC),
    {NsF, NsL} = vl0(Fs,Vmap_Nrm,Vmad_Nrm),
    FsMs = lists:zip(lists:zip(lists:zip(Fs,Ms),lists:zip(TxF,VcF)),NsF),
    Efs = [make_e3ds_face(Face,Mat,Tx,Vc,Ns) || {{{Face,Mat},{Tx,Vc}},Ns} <- FsMs],
    Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs,tx=TxL,vc=VcL,ns=NsL},
    Obj = #e3d_object{name=Name,obj=Mesh},
    [Obj | make_e3ds(Tags, Rest)].

make_e3ds_face(Face,Mat,Tx,Vc,Ns) ->
    #e3d_face{
        vs=lists:reverse(Face),
        mat=[Mat],
        tx=lists:reverse(Tx),
        vc=lists:reverse(Vc),
        ns=lists:reverse(Ns)
    }.

%% Turn VMAP and VMAD into index list.
vl0(Poly,none,none) ->
    {[[] || _ <- Poly],[]};
vl0(Poly,Vmap,none)
  when is_list(Vmap) ->
    vl0(Poly,Vmap,[]);
vl0(Poly,none,Vmad)
  when is_list(Vmad) ->
    vl0(Poly,[],Vmad);
vl0(Poly,Vmap,Vmad) ->
    Vmap1 = gb_trees:from_orddict(orddict:from_list(Vmap ++ Vmad)),
    Poly1 = lists:zip(lists:seq(0,length(Poly)-1),Poly),
    vl0(Poly1,Vmap1,{gb_trees:empty(),[]},[]).
vl0([],_Vmap1,{_,VList},OL) ->
    {lists:reverse(OL),lists:reverse(VList)};
vl0([{I,P}|Poly1],Vmap1,G1,OL) ->
    {V,G1_1}=vl1(P,Vmap1,I,G1),
    vl0(Poly1,Vmap1,G1_1,[V|OL]).

vl1(List,Vmap1,I,G1) ->
    vl1(List,Vmap1,I,G1,[]).
vl1([],_Vmap1,_I,G1,OL) ->
    {lists:reverse(OL),G1};
vl1([P|List],Vmap1,I,G1,OL) ->
    {Idx,G1_1} = vl_find(P,I,Vmap1,G1),
    vl1(List,Vmap1,I,G1_1,[Idx|OL]).

vl_find(P,I,Vmap1,{Vals,VList}=G1) ->
    case gb_trees:lookup(P,Vals) of
        {value,Found} ->
            {Found,G1};
        _ ->
            case gb_trees:lookup({P,I},Vmap1) of
                {value,V1} ->
                    Idx=length(VList),
                    VList1 = [V1|VList],
                    {Idx,{Vals,VList1}};
                _ ->
                    case gb_trees:lookup(P, Vmap1) of
                        {value,V2} ->
                            Idx=length(VList),
                            VList1 = [V2|VList],
                            Vals1 = gb_trees:insert(P,Idx,Vals),
                            {Idx,{Vals1,VList1}};
                        _ ->
                            {0,G1}
                    end
            end
    end.



make_mats(List, Dir) ->
    {ClipsArr,List1} = get_clips(mat_merge_sources(List)),
    make_mats(ClipsArr, List1, Dir).
make_mats(_ClipsArr, [], _Dir) -> [];
make_mats(ClipsArr, [{surf_tex, {Name,Surf}, Tex} | Rest], Dir)
  when is_atom(Name) ->
    [make_mat(Name, Surf, add_clip_list(Tex, ClipsArr), Dir)
        | make_mats(ClipsArr, Rest, Dir)].

get_clips(List) ->
    get_clips(List, [], gb_trees:empty()).
get_clips([], OL, ClipsArr) ->
    {ClipsArr, lists:reverse(OL)};
get_clips([{clip, {Id, {_Type,_Name,_}=Clip}} | Rest], OL, ClipsArr) ->
    get_clips(Rest, OL, gb_trees:insert(Id,Clip,ClipsArr));
get_clips([B|Rest], OL, ClipsArr) ->
    get_clips(Rest, [B|OL], ClipsArr).

add_clip_list(Tex, ClipsArr) ->
    add_clip_list(Tex, ClipsArr, []).
add_clip_list([], _ClipsArr, OL) ->
    lists:reverse(OL);
add_clip_list([{IType,Index}|Tex], ClipsArr, OL)
  when Index > 0 ->
    {_,Name,FileName} = gb_trees:get(Index,ClipsArr),
    V = {IType,Name,FileName},
    add_clip_list(Tex, ClipsArr, [V|OL]);
add_clip_list([{_IType,Index}|Tex], ClipsArr, OL)
  when Index < 1 ->
    add_clip_list(Tex, ClipsArr, OL).

rgba({R,G,B},Alpha) ->
    {R,G,B,Alpha}.


%% Merge materials that reference a source material.
%%
-record(merge_sources, {
    out=[],
    map=#{},
    unmerged=[],
    changed=false
}).

mat_merge_sources(List) ->
    {Surf0, Clips} = lists:partition(
        fun (Tup) ->
            element(1, Tup) =:= surf_tex
        end,
        List),
    Surf1 = mat_merge_sources(mat_merge_sources_0(Surf0), #merge_sources{}),
    Clips ++ Surf1.

mat_merge_sources(Surf0, State) ->
    case mat_merge_sources_1(Surf0, State) of
        #merge_sources{changed=true,unmerged=UM}=State1 ->
            State2 = State1#merge_sources{changed=false,unmerged=[]},
            mat_merge_sources(UM, State2);
        #merge_sources{changed=false,unmerged=UM,out=L} ->
            L ++ [{surf_tex,{Name,Attr},Tex} || {surf_tex,{{Name,_Source},Attr},Tex} <- UM]
    end.

mat_merge_sources_1([], #merge_sources{}=State) ->
    State;
mat_merge_sources_1([{surf_tex,{{Name,Source},Attr},Tex}=SurfTex0|L], #merge_sources{map=M,unmerged=OL2}=State)
  when is_atom(Name) ->
    State1 = case maps:get(Source, M, none) of
        {AttrM,TexM} ->
            Attr2 = maps:merge_with(fun(_,V1,_) -> V1 end, Attr, AttrM),
            Tex2 = orddict:merge(fun(_,V1,_) -> V1 end,
                        orddict:from_list(Tex),
                        orddict:from_list(TexM)),
            SurfTex = {surf_tex,{Name,Attr2},Tex2},
            mat_merge_sources_2(State, SurfTex);
        none ->
            #merge_sources{unmerged=[SurfTex0|OL2]}
    end,
    mat_merge_sources_1(L, State1);
mat_merge_sources_1([{surf_tex,{Name,_},_}=SurfTex|L], #merge_sources{}=State)
  when is_atom(Name) ->
    State1 = mat_merge_sources_2(State, SurfTex),
    mat_merge_sources_1(L, State1).

mat_merge_sources_2(#merge_sources{map=M,out=OL}=State, {surf_tex,{Name,Attr},Tex}=SurfTex) ->
    State#merge_sources{out=[SurfTex|OL],map=maps:put(Name,{Attr,Tex},M),changed=true}.

%% Change surfaces which the specified source does not exist before
%% trying to merge attributes.
%%
mat_merge_sources_0(Surf0) ->
    Map = maps:from_list(
        [{Name, 1} || {surf_tex,{{Name,_},_},_} <- Surf0, is_atom(Name)] ++
        [{Name, 1} || {surf_tex,{Name,_},_} <- Surf0, is_atom(Name)]),
    lists:map(
        fun
            ({surf_tex,{{Name,Source},Attr},Tex}=SurfTex0) when is_atom(Name) ->
                case maps:is_key(Source, Map) of
                    true ->
                        SurfTex0;
                    false ->
                        {surf_tex,{Name,Attr},Tex}
                end;
            ({surf_tex,{Name,_},_}=SurfTex0) when is_atom(Name) ->
                SurfTex0
        end, Surf0).

%%
%%

make_mat(Name, Surf, Clips, Dir) ->
    {Name,[{yafray,[{autosmooth_angle,60.0},
		    {caus,false},
		    {emit_rad,true},
		    {ior,1.0},
		    {min_refle,0.0},
		    {minimized,true},
		    {modulators,[]},
		    {recv_rad,true},
		    {shadow,true}]},
	   {ribbit,[{'Ka',1.0},
		    {'Kd',1.0},
		    {current_shader,matte},
		    {{ribbit,minimized},true},
		    {{ribbit,select_shader},false}]},
	   opengl(Surf),
	   {maps,make_mat_map(Surf, Clips, Dir)}]}.

opengl(Surf) ->
    List = [
        ambient,
        diffuse,
        emission,
        shininess,
        specular,
        metallic
    ],
    OpenGL = [opengl(Surf, A) || A <- List],
    {opengl, [A || A <- OpenGL, is_tuple(A)]}.

opengl(_, ambient) ->
    {ambient, {1.0,1.0,1.0,1.0}};
opengl(Surf, diffuse) ->
    RGB = case Surf of
        #{"color":=RGB_0} -> RGB_0;
        _ -> {0.7,0.7,0.7}
    end,
    Alpha = case Surf of
        #{"transparency":=T} -> 1.0-T;
        _ -> 1.0
    end,
    {diffuse, rgba(RGB, Alpha)};
opengl(Surf, emission) ->
    Lum = case Surf of
        #{"luminosity":=L} -> L;
        _ -> 0.0
    end,
    {emission,{Lum,Lum,Lum,1.0}};
opengl(_, shininess) ->
    {shininess,1.0};
opengl(#{"specular":=Spec}=_Surf, specular)
  when is_float(Spec) ->
    {specular,{Spec,Spec,Spec,1.0}};
opengl(#{"reflection":=Refl}=_Surf, metallic)
  when is_float(Refl) ->
    {metallic,Refl};
opengl(_, _) ->
    false.
               

%% Load texture
%%
make_mat_map(_, [], _Dir) ->
    [];
make_mat_map(Surf, [{Type,Name,FileName}|Clips], Dir) ->
    case make_mat_map_img(Surf, Name, FileName, Dir) of
        #e3d_image{}=Image ->
            [{Type,Image}|make_mat_map(Surf,Clips,Dir)];
        _ ->
            io:format("~p: " ++ ?__(1,"Could not load: ") ++ "~s~n", [?MODULE,FileName]),
            make_mat_map(Surf,Clips,Dir)
    end.

make_mat_map_img(_Surf, _Name, FileName, Dir) ->
    FileName1 = lists:flatten(string:replace(FileName,"\\","/",all)),
    Paths = [
        case pathtype(FileName1) of
            absolute ->
                FileName1;
            relative ->
                filename:join(Dir,FileName1);
            false ->
                false
        end
    |
        pathsearch(Dir, FileName1)
    ],
    make_mat_map_img_1(Paths).
make_mat_map_img_1([]) ->
    error;
make_mat_map_img_1([false|Paths]) ->
    make_mat_map_img_1(Paths);
make_mat_map_img_1([FileName|Paths]) ->
    io:format("~p: " ++ ?__(1,"trying: ") ++ "~s~n", [?MODULE, FileName]),
    case file:read_file_info(FileName) of
        {ok, _} ->
            get_bitmap(FileName);
        _ ->
            make_mat_map_img_1(Paths)
    end.

pathtype([C,$:|_])
  when C >= $A andalso C =< $Z;
       C >= $a andalso C =< $z ->
    case os:type() of
        {win32,_} -> absolute;
        _ -> false
    end;
pathtype(FileName) ->
    case string:find(FileName, ":") of
        nomatch ->
            filename:pathtype(FileName);
        _ ->
            false
    end.

pathsearch(Dir, FileName) ->
    pathsearch(Dir, filename:split(FileName), []).
pathsearch(Dir, [[Sep]|Path], [])
  when Sep =:= $/ ; Sep =:= $\\ ->
    pathsearch(Dir, Path, []);
pathsearch(Dir, [[C,$:]|Path], [])
  when C >= $A andalso C =< $Z;
       C >= $a andalso C =< $z ->
    pathsearch(Dir, Path, []);
pathsearch(Dir, ([B|Path_1])=Path, OL) ->
    case string:find(B, ":") of
        nomatch ->
            PathF = filename:join(Dir, filename:join(Path)),
            pathsearch(Dir, Path_1, [PathF|OL]);
        _ ->
            pathsearch(Dir, Path_1, OL)
    end;
pathsearch(_Dir, [], OL) ->
    lists:reverse(OL).
    
get_bitmap(FileName) ->
    case string:to_lower(filename:extension(FileName)) of
        ".jpg" ->
            read_jpeg(FileName);
        _ ->
            e3d_image:load(FileName)
    end.

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
            e3d_image:fix_outtype(FileName, E3d, []);
        false ->
            {error, none}
    end.


%%%
%%%

lwo_import(Name) ->
    case file:read_file(Name) of
        {ok,Bin} ->
            ?DEBUG(""),
            ?DEBUG_B(Name),
            Contents = import(Bin, filename:dirname(Name)),
            ?DEBUG("EOF"),
            FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
            Res = e3d_file:transform(Contents, FlipX),
            {ok, Res};
        {error,Reason} ->
            {error,file:format_error(Reason)}
    end.

import(Data, Dir) ->
    case read_header(Data) of
        {lwob, Rest} ->
            import_1(read_lwob(Rest), Dir);
        {lwo2, Rest} ->
            import_1(read_lwo2(Rest), Dir);
        {lwo3, Content} ->
            import_1(lwo3_flatten(read_lwo3(Content)), Dir)
    end.

import_1(Chunks, Dir) ->
    Objs = [Chunk || {o, Chunk} <- Chunks], %% get objects
    Mats = [Chunk || {m, Chunk} <- Chunks], %% get Surfs
    Eobjs = make_e3ds(Objs),
    Emats = make_mats(Mats, Dir),
    #e3d_file{objs=Eobjs, mat=Emats, creator="LWO Import Plugin"}.

%%% ============
%%% === Misc ===
%%% ============

add_indices(List) ->
    ListLen = length(List),
    Indices = lists:seq(0, ListLen-1),
    IndexedList = lists:zip(List, Indices),
    IndexedList.

surf_name(A,"")
  when is_list(A) ->
    list_to_atom(A);
surf_name(A,B)
  when is_list(A), is_list(B) ->
    {list_to_atom(A), list_to_atom(B)}.


