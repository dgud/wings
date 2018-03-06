%%
%%  wpc_lwo.erl --
%%
%%     LightWave Object File Format (*.lwo) Import/Export
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_lwo).
-export([init/0, menu/2, command/2, export/1]).
-include_lib("wings/e3d/e3d.hrl").

init() ->
    true.

menu({file, import}, Menu) ->
    menu_entry(Menu);
menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file, {import, lwo}}, St) ->
    Props = props2(),
    wpa:import(Props, fun lwo_import/1, St);
command({file, {export, lwo}}, St) ->
    Props = props(),
    wpa:export(Props, fun export/2, St);
command({file, {export_selected, lwo}}, St) ->
    Props = props(),
    wpa:export_selected(Props, fun export/2, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"LightWave or Modo (.lwo|.lxo)...", lwo}].

props() ->
    [{ext, ".lwo"},{ext_desc, "LightWave or Modo File"}].

props2() ->
    [{ext, ".lwo"},{ext_desc, "LightWave or Modo File"}].

%%% ===============================
%%% === LightWave Export (LWO2) ===
%%% ===============================

export(E3DFile) ->
    LWO = make_lwo(E3DFile),
    file:write_file("1.lwo", LWO).

export(FileName, Contents) ->
    LWO = make_lwo(Contents),
    file:write_file(FileName, LWO).

make_lwo(Contents0) ->
    FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
    Contents = e3d_file:transform(Contents0, FlipX),
    #e3d_file{objs=Objs,mat=Mats,creator=Creator} = Contents,
    Tags = make_tags(Mats),
    Surfs = make_surfaces(Mats),
    Maker = make_auth(Creator),
    MaterialDict = make_material_dict(Mats),
    ObjectChunks = make_objects_chunk(Objs, MaterialDict),
    Chunks = [Maker, Tags, ObjectChunks|Surfs],
    LwoHeader = make_header(Chunks),
    LwoData = [LwoHeader|Chunks],
    LwoData.

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

make_surfaces(Mats) ->
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

make_header(Chunks) ->
    DataSize = size(list_to_binary(Chunks)),
    FormSize = DataSize + length("LWO2"),
    A = <<"FORM">>,
    B = <<FormSize:32>>,
    C = <<"LWO2">>,
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

%%% ===============================
%%% === LightWave Import (LWO2) ===
%%% ===============================

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

read_header(Data) ->
    <<FormId:4/binary, _FormSize:32, FormType:4/binary, Rest/binary>> = Data,
    FormId = <<"FORM">>,
    case (FormType == <<"LWO2">>) or (FormType == <<"LXOB">>) of
	true -> Rest;
	false -> wings_u:error_msg("LWO files in v5.5 format are not supported.")
    end.

read_tags(Data) ->
    TagsList = binary_to_list(Data),
    TagsList2 = string:tokens(TagsList, [0]),
    lists:map(fun erlang:list_to_atom/1, TagsList2).

read_layr(Data) ->
    <<_:16/binary, BiName/binary>> = Data,
    LayerName = read_nstring(binary_to_list(BiName)),
    LayerName.

read_pnts(<<>>) -> [];
read_pnts(Data) ->
    <<X:32/float, Y:32/float, Z:32/float, Rest/binary>> = Data,
    [{X,Y,Z} | read_pnts(Rest)].

read_pols(<<"FACE",Rest/binary>>) ->
    read_point_idxs(Rest);
read_pols(<<"PTCH",Rest/binary>>) ->
    read_point_idxs(Rest);
read_pols(<<"SUBD",_/binary>>) ->
    wings_u:error_msg("LWO files containing \"SUBD\" sub-forms in "
		  "\"POLS\" not supported.").

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

read_rgb(Data) ->
    try
	<<_:16,"COLR",_:16, R:32/float,G:32/float,B:32/float, _More/binary>> = Data,
	{R,G,B}
    catch _:_ ->
	{0.9,0.9,0.9}
    end.

read_surf(Data) ->
    {MaterialName, Rest} = read_cstring(Data),
    {R,G,B} = read_rgb(Rest),
    {list_to_atom(MaterialName), {R,G,B}}.

read_ptag(Data) ->
    <<Ptag:4/binary, Rest/binary>> = Data,
    case Ptag of
	<<"SURF">> -> read_ptag(Rest, []);
	<<"MATR">> -> read_ptag(Rest, []);
	_ -> list_to_atom(binary_to_list(Ptag))
    end.

read_ptag(<<>>, Acc) -> Acc;
read_ptag(Data, Acc) ->
    {FaceIndex, Rest} = read_vx(Data),
    {SurfIndex, More} = read_vx(Rest),
    [{FaceIndex,SurfIndex} | read_ptag(More, Acc)].

read_chunk(Data, Chunks) ->
    <<ChunkId:4/binary, ChunkSize:32, Rest/binary>> = Data,
    <<ChunkData:ChunkSize/binary, Rest2/binary>> = Rest,
    ChunkSize = size(ChunkData),
    %% io:fwrite("Chunk: ~s, Size: ~p\n", [binary_to_list(ChunkId), ChunkSize]),
    case ChunkId of
	<<"TAGS">> ->
	    Tags = read_tags(ChunkData),
	    pp(Tags),
	    Chunk = Tags;
	<<"LAYR">> ->
	    Layr = read_layr(ChunkData),
	    pp(Layr),
	    Chunk = Layr;
	<<"PNTS">> ->
	    Verts = read_pnts(ChunkData),
	    %pp(Verts),
	    Chunk = Verts;
	<<"POLS">> ->
	    Faces = read_pols(ChunkData),
	    %pp(Faces),
	    Chunk = Faces;
	<<"PTAG">> ->
	    Ptag = read_ptag(ChunkData),
	    %pp(Ptag),
	    Chunk = Ptag;
	<<"SURF">> ->
	    Surf = read_surf(ChunkData),
	    pp(Surf),
	    Chunk = Surf;
	_ ->
	    ChunkName = binary_to_list(ChunkId) ++ " Chunk",
	    %Chunk = term_to_binary(Ptags);
	    Chunk = list_to_atom(ChunkName)
    end,
    Chunks2 = lists:append(Chunks, [Chunk]),
    {Rest2, Chunks2}.

read_chunks(<<>>, Chunks) ->
    Chunks;
read_chunks(Data, Chunks) ->
    {Rest, Chunks2} = read_chunk(Data, Chunks),
    read_chunks(Rest, Chunks2).

make_e3ds(Objs) ->
    [Tags | Rest] = Objs,
    make_e3ds(Tags, Rest).

make_e3ds(_Tags, []) -> [];
make_e3ds(Tags, Objs) ->
    [Name,Vs,Fs,Ptag | Rest] = Objs,
    {_FaceIdxs,MaterialIdxs} = lists:unzip(Ptag),
    GetMatName = fun(Idx) -> lists:nth(Idx+1, Tags) end,
    Ms = lists:map(GetMatName, MaterialIdxs),
    FsMs = lists:zip(Fs,Ms),
    Efs = [#e3d_face{vs=lists:reverse(Face),mat=[Mat]} || {Face,Mat} <- FsMs],
    Mesh = #e3d_mesh{type=polygon,vs=Vs,fs=Efs},
    Obj = #e3d_object{name=Name,obj=Mesh},
    [Obj | make_e3ds(Tags, Rest)].

make_mats([]) -> [];
make_mats(Mats) ->
    [{Name,RGB} | Rest] = Mats,
    {R,G,B} = RGB,
    RGBA = {R,G,B,1.0},
    [make_mat(Name, RGBA) | make_mats(Rest)].

make_mat(Name, RGBA) ->
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
	   {opengl,[{ambient, {1.0,1.0,1.0,1.0}},
		    {diffuse, RGBA},
		    {emission,{0.0,0.0,0.0,1.0}},
		    {shininess,1.0},
		    {specular,{1.0,1.0,1.0,1.0}}]},
	   {maps,[]}]}.

import(Data) ->
    Rest = read_header(Data),
    Chunks = read_chunks(Rest, []),
    Objs = [Chunk || Chunk<-Chunks, is_list(Chunk)],  %% filter unused chunks
    Mats = [Chunk || Chunk<-Chunks, is_tuple(Chunk)], %% get Surfs
    Eobjs = make_e3ds(Objs),
    Emats = make_mats(Mats),
    #e3d_file{objs=Eobjs, mat=Emats, creator="LWO Import Plugin"}.

lwo_import(Name) ->
    case file:read_file(Name) of
	{ok,Bin} ->
	    ps(""),
	    print_boxed(Name),
	    Contents = import(Bin),
	    ps("EOF"),
	    FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
	    Res = e3d_file:transform(Contents, FlipX),
	    {ok, Res};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

%%% ============
%%% === Misc ===
%%% ============
pp(_Item) ->
%%    io:fwrite("       ~p\n", [_Item]),
    ok.

ps(_Item) ->
%%    io:fwrite("~s\n", [_Item]),
    ok.

print_boxed(_Item) -> ok.
%% print_boxed(Item) ->
%%     if
%% 	is_atom(Item) -> String = atom_to_list(Item);
%% 	is_list(Item) -> String = Item
%%     end,
%%     NumChars = length(String),
%%     io:fwrite("+-~s-+\n", [lists:duplicate(NumChars, "-")]),
%%     io:fwrite("| ~s |\n", [String]),
%%     io:fwrite("+-~s-+\n", [lists:duplicate(NumChars, "-")]).

add_indices(List) ->
    ListLen = length(List),
    Indices = lists:seq(0, ListLen-1),
    IndexedList = lists:zip(List, Indices),
    IndexedList.
