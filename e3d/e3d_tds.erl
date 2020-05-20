%%
%%  e3d_tds.erl --
%%
%%     Functions for reading and writing 3D Studio Max files (.tds),
%%     version 3.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d_tds).
-export([import/1,export/2]).

-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,member/2,
		sort/1,keysort/2,usort/1,keydelete/3,keyreplace/4]).

%%-define(DEBUG, 1).

-define(FLOAT, float-little).

-ifdef(DEBUG).
-define(dbg(Format, List), begin io:format(Format, List) end).
-else.
-define(dbg(Format, List), ok).
-endif.


%%%
%%% Import.
%%% 

import(Name) ->
    case file:read_file(Name) of
	{ok,Bin} ->
	    ?MODULE = ets:new(?MODULE, [named_table,ordered_set]),
	    Dir = filename:dirname(Name),
	    Res = import_1(Bin, Dir),
	    ets:delete(?MODULE),
	    Res;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Bin, Dir) ->
    try import_2(Bin) of
	#e3d_file{}=E3dFile ->
	    {ok,E3dFile#e3d_file{dir=Dir}}
    catch
	throw:Error -> Error
    end.

import_2(<<16#4D4D:16/little,_Size:32/little,T/binary>>) ->
    File = fold_chunks(fun main/3, #e3d_file{}, T),
    #e3d_file{objs=Objs0,mat=Mat0} = File,
    Mat = reformat_material(Mat0),
    Objs = case catch fix_transform(Objs0) of
	       {'EXIT',Reason} ->
		   io:format("~P\n", [Reason,20]),
		   reverse(Objs0);
	       Other -> Other
	   end,
    File#e3d_file{objs=Objs,mat=Mat};
import_2(_) -> error_msg("Not a .3ds file").

main(16#0002, <<_Ver:32/little>>, Acc) ->
    ?dbg("3DS Version ~p\n", [_Ver]),
    Acc;
main(16#3D3D, Editor, Acc) ->
    ?dbg("Editor: ~p bytes\n", [byte_size(Editor)]),
    editor(Editor, Acc);
main(16#B000, Keyframer, Acc) ->
    ?dbg("\nKeyframer:\n", []),
    keyframer(Keyframer),
    Acc;
main(_Tag, _Chunk, Acc) ->
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,15]),
    Acc.

%% editor(Bin, E3DFile) -> E3DFile
%%  Collect objects and materials from the sub-chunks of 'editor'.

editor(Bin, Acc) ->
    fold_chunks(fun editor/3, Acc, Bin).

editor(16#0100, <<_Scale:32/?FLOAT>>, Acc) ->
    ?dbg("Object Scale ~p ~n", [_Scale]),
    Acc;
editor(16#3d3e, <<_Ver:32/little>>, Acc) ->
    ?dbg("Mesh Version ~p ~n", [_Ver]),
    Acc;
editor(16#4000, Obj0, #e3d_file{objs=Objs0}=Acc) ->
    {Name,Obj1} = get_cstring(Obj0),
    ets:insert(?MODULE, {current_name,Name}),
    ?dbg("\nObject block: ~s\n", [Name]),
    case block(Obj1) of
	no_mesh -> Acc;
	Obj ->
	    Objs = [#e3d_object{name=Name,obj=Obj}|Objs0],
	    Acc#e3d_file{objs=Objs}
    end;
editor(16#AFFF, Mat0, #e3d_file{mat=M}=Acc) ->
    Mat = material(Mat0, M),
    Acc#e3d_file{mat=Mat};
editor(_Tag, _Chunk, Acc) ->
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,15]),
    Acc.

%% keyframer(Bin)
%%  Go through the keyframer data and collect the only information
%%  we are interested in: how the objects are related in the
%%  hieararchy. We'll need that to calculate the correct transformation
%%  matrix for each object.

keyframer(Bin) ->
    put(e3d_tds_node_id, -1),
    fold_chunks(fun(Tag, Contents, _) -> keyframer(Tag, Contents) end, [], Bin),
    erase(e3d_tds_node_id),
    ok.

keyframer(16#B002, Contents) ->
    %% Node chunk - recurse into it.
    fold_chunks(fun(Tag, Cont, _) -> keyframer(Tag, Cont) end, [], Contents),
    put(e3d_tds_node_id, get(e3d_tds_node_id)+1);
keyframer(16#B030, <<NodeId:16/little-signed>>) ->
    %% Node id for this node.
    put(e3d_tds_node_id, NodeId);
keyframer(16#B010, Bin) ->
    %% Name+parent node.
    NameSz = byte_size(Bin) - 7,
    <<Name0:NameSz/binary,_:5/unit:8,Parent:16/little-signed>> = Bin,
    Name = binary_to_list(Name0),
    NodeId = get(e3d_tds_node_id),
    ?dbg("node ~p: ~p, parent ~p\n", [NodeId,Name,Parent]), 
    ets:insert(?MODULE, {NodeId,Name}),
    ets:insert(?MODULE, {Name,Parent});
keyframer(16#B00A, <<_Rev:16/little,T0/binary>>) ->
    %% Just print information (if debugging).
    {_Str,_T} = get_cstring(T0),
    ?dbg("Keyframe header: ~p ~p ~p\n", [_Rev,_Str,_T]);
keyframer(16#B013, <<_X:32/?FLOAT,_Y:32/?FLOAT,_Z:32/?FLOAT>>) ->
    %% Just print information (if debugging).
    ?dbg(" pivot: ~p ~p ~p\n", [_X,_Y,_Z]);
keyframer(16#B021, <<_A:32/?FLOAT,_X:32/?FLOAT,_Y:32/?FLOAT,_Z:32/?FLOAT,_/binary>>) ->
    %% Just print information (if debugging).
    ?dbg(" rot_track_tag: ~p ~p ~p ~p\n", [_A,_X,_Y,_Z]);
keyframer(_Tag, _Contents) ->
    %% Ignore all other keyframer chunks.
    ?dbg("~.16#: ~P\n", [_Tag,_Contents,15]).

block(Block) ->
    fold_chunks(fun block/3, no_mesh, Block).

block(16#3d3e, <<_Ver:32/little>>, Acc) ->
    ?dbg("Mesh Version ~p ~n", [_Ver]),
    Acc;
block(16#4100, TriMesh0, no_mesh) ->
    ?dbg("Triangular mesh: ~p\n", [byte_size(TriMesh0)]),
    TriMesh1 = fold_chunks(fun trimesh/3, #e3d_mesh{type=triangle}, TriMesh0),
    TriMesh = add_uv_to_faces(TriMesh1),
    clean_mesh(TriMesh);
block(16#4700, _Camera, Mesh) ->
    ?dbg("Camera: ~p\n", [byte_size(_Camera)]),
    Mesh;
block(_Tag, _Chunk, Mesh) ->
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,15]),
    Mesh.

trimesh(16#4110, <<_NumVs:16/little,Vs0/binary>>, Acc) ->
    ?dbg("~p vertices\n", [_NumVs]),
    Vs = get_bin_vectors(Vs0),
    Acc#e3d_mesh{vs=Vs};
trimesh(16#4120, <<NFaces:16/little,Contents/binary>>, Acc) ->
    ?dbg("~p faces\n", [NFaces]),
    Fsz = NFaces * 2 * 4,
    <<Faces0:Fsz/binary,Desc/binary>> = Contents,
    Faces1 = get_faces(Faces0),
    {Faces,Smooth} = face_desc(Desc, Faces1),
    Acc#e3d_mesh{fs=Faces,he=Smooth};
trimesh(16#4140, <<_NumTx:16/little,Tx0/binary>>, Acc) ->
    ?dbg("~p texture coordinates\n", [_NumTx]),
    Tx = get_uv(Tx0),
    Acc#e3d_mesh{tx=Tx};
trimesh(16#4160, <<V1X:32/?FLOAT,V1Y:32/?FLOAT,V1Z:32/?FLOAT,
		  V2X:32/?FLOAT,V2Y:32/?FLOAT,V2Z:32/?FLOAT,
		  V3X:32/?FLOAT,V3Y:32/?FLOAT,V3Z:32/?FLOAT,
		  OX:32/?FLOAT,OY:32/?FLOAT,OZ:32/?FLOAT>>, Acc) ->
    Matrix = {V1X,V1Y,V1Z,
	      V2X,V2Y,V2Z,
	      V3X,V3Y,V3Z,
	      OX,OY,OZ},
    [{current_name,Name}] = ets:lookup(?MODULE, current_name),
    ets:insert(?MODULE, {{local_matrix,Name},Matrix}),
    ?dbg("~p: ~p\n", [Name,Matrix]),
    Acc;
trimesh(_Tag, _Chunk, Acc) ->
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,15]),
    Acc.

face_desc(Bin, Faces) ->
    fold_chunks(fun face_desc/3, {Faces,[]}, Bin).

face_desc(16#4130, MatList0, {Faces0,SG}) ->
    {Name0,MatList} = get_cstring(MatList0),
    Name = list_to_atom(Name0),
    MatFaces0 = get_mat_faces(MatList),
    MatFaces = sort(MatFaces0),
    ?dbg("Material ~p used by ~p face(s)\n", [Name0,length(MatFaces)]),
    Faces = insert_mat(Faces0, MatFaces, 0, Name, []),
    {Faces,SG};
face_desc(16#4150, Smooth, {Faces,_}) ->
    ?dbg("Smoothing groups for ~p faces\n", [byte_size(Smooth) div 4]),
    {Faces,get_smooth_groups(Smooth)}.

%% material(Bin, [Material]) -> [Material]
%%  Collect all materials.

material(Bin, Acc) ->
    fold_chunks(fun material/3, Acc, Bin).

material(16#A000, Name0, Acc) ->
    {Name1,<<>>} = get_cstring(Name0),
    Name = list_to_atom(Name1),
    ?dbg("\nMaterial: ~p\n", [Name]),
    [{Name,[]}|Acc];
material(16#A010, Chunk, Acc) ->
    mat_chunk(ambient, Chunk, Acc);
material(16#A020, Chunk, Acc) ->
    mat_chunk(diffuse, Chunk, Acc);
material(16#A030, Chunk, Acc) ->
    mat_chunk(specular, Chunk, Acc);
material(16#A040, Chunk, Acc) ->
    mat_chunk(shininess, Chunk, Acc);
material(16#A041, Chunk, Acc) ->
    mat_chunk(shininess_level, Chunk, Acc);
material(16#A050, Chunk, [{Name,Props}|Acc]) ->
    Tr = general(Chunk),
    [{Name,[{opacity,1-Tr}|Props]}|Acc];
material(16#A052, Chunk, Acc) ->
    mat_chunk(opacity_falloff, Chunk, Acc);
material(16#A053, Chunk, Acc) ->
    mat_chunk(reflection_blur, Chunk, Acc);
material(16#A081, _, [{Name,Props}|Acc]) ->
    ?dbg("Twosided material\n", []),
    [{Name,[{twosided,true}|Props]}|Acc];
material(16#A084, Chunk, Acc) ->
    mat_chunk(emissive, Chunk, Acc);
material(16#A087, <<_Wiresize:32/?FLOAT>>, Acc) ->
    ?dbg("Wire size: ~p\n", [_Wiresize]),
    Acc;
material(16#A100, <<_Shading:16/little>>, Acc) ->
    ?dbg("Shading: ~s\n",
	 [case _Shading of
	      0 -> "Wire";
	      1 -> "Flat";
	      2 -> "Gouraud";
	      3 -> "Phong";
	      4 -> "Metal";
	      _ -> "Unknown"
	  end]),
    Acc;
material(16#A200, Chunk, Acc) ->
    read_map(diffuse, Chunk, Acc);
material(16#A210, Chunk, Acc) ->
    read_map(opacity, Chunk, Acc);
material(16#A230, Chunk, Acc) ->
    read_map(bump, Chunk, Acc);
material(Tag, Chunk, [{Name,Props}|Acc]) ->
    ?dbg("~.16#: ~P\n", [Tag,Chunk,20]),
    [{Name,[{Tag,Chunk}|Props]}|Acc].

read_map(Type, Chunk, [{Name,Props}|Acc]) ->
    ?dbg("Map: ~p\n", [Type]),
    MapPs = fold_chunks(fun texture/3, [], Chunk),
    Map = proplists:get_value(filename, MapPs),
    [{Name,[{map,Type,Map}|Props]}|Acc].

texture(16#A300, Chunk, Acc) ->
    {Filename,_} = get_cstring(Chunk),
    ?dbg("Filename: ~s\n", [Filename]),
    [{filename,Filename}|Acc];
texture(16#A351, _Params, Acc) ->
    ?dbg("Params: ~p\n", [_Params]),
    Acc;
texture(_Tag, _Chunk, Acc) ->
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,20]),
    Acc.

reformat_material([{Name,Mat}|T]) ->
    Opac = proplists:get_value(opacity, Mat, 1.0),
    [{Name,reformat_mat(Mat, Opac, [], [], [])}|reformat_material(T)];
reformat_material([]) -> [].

reformat_mat([{diffuse,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{ambient,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{specular,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{emission,_}=Col0|T], Opac, Ogl, Maps, Tds) ->
    Col = reformat_color(Col0, Opac),
    reformat_mat(T, Opac, [Col|Ogl], Maps, Tds);
reformat_mat([{shininess,Sh}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, [{shininess,1.0-Sh}|Ogl], Maps, Tds);
reformat_mat([{opacity,_}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, Maps, Tds);
reformat_mat([{map,Name,Data}|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, [{Name,Data}|Maps], Tds);
reformat_mat([Other|T], Opac, Ogl, Maps, Tds) ->
    reformat_mat(T, Opac, Ogl, Maps, [Other|Tds]);
reformat_mat([], _Opac, Ogl, Maps, Tds) ->
    [{opengl,Ogl},{maps,Maps},{tds,Tds}].

reformat_color({Key,{R,G,B}}, Opac) ->
    {Key,{R,G,B,Opac}}.
    
mat_chunk(Type, Chunk, [{Name,Props}|Acc]) ->
    Value = general(Chunk),
    ?dbg("property ~p = ~p\n", [Type,Value]),
    [{Name,[{Type,Value}|Props]}|Acc].

general(<<16#0010:16/little,_Sz:32/little, 
	 R:32/?FLOAT,G:32/?FLOAT,B:32/?FLOAT,
	 T/binary>>) ->
    general_rest(T),
    {R,G,B};
general(<<16#0011:16/little,_Sz:32/little,R:8,G:8,B:8,T/binary>>) ->
    general_rest(T),
    {R/255,G/255,B/255};
general(<<16#0030:16/little,_Sz:32/little,Percent:16/little>>) ->
    Percent/100.

general_rest(<<>>) -> ok;
general_rest(<<_Tag:16/little,Sz0:32/little,T0/binary>>) ->
    Sz = Sz0 - 6,
    <<_Chunk:Sz/binary,T/binary>> = T0,
    ?dbg("~.16#: ~P\n", [_Tag,_Chunk,15]),
    general_rest(T).

fix_transform(Objs) ->
    case ets:member(?MODULE, "$$$DUMMY") of
	false -> reverse(Objs);
	true -> fix_transform_0(Objs)
    end.

fix_transform_0(Objs) ->
    lists:foldl(fun(O, A) -> [fix_transform_1(O)|A] end, [], Objs).

fix_transform_1(#e3d_object{name=Name,obj=Mesh}=Obj) ->
    Matrix = get_transform(Name),
    ?dbg("~p: ~p\n", [Name,Matrix]),
    Obj#e3d_object{obj=Mesh#e3d_mesh{matrix=Matrix}}.

get_transform(Name) ->
    case ets:lookup(?MODULE, Name) of
	[] -> identity;
	[{Name,ParentId}] ->
	    case get_name_and_matrix(ParentId) of
		identity -> identity;
		{Parent,Matrix} ->
		    e3d_mat:mul(get_transform(Parent), Matrix)
	    end
    end.

get_name_and_matrix(Id) ->
    case ets:lookup(?MODULE, Id) of
	[] -> identity;
	[{Id,Name}] ->
	    case ets:lookup(?MODULE, {local_matrix,Name}) of
		[] -> identity;
		[{_,Matrix}] -> {Name,Matrix}
	    end
    end.

%%%
%%% Utilities.
%%%

fold_chunks(Fun, Acc0, <<Tag:16/little,Sz0:32/little,T0/binary>>) ->
    Sz = Sz0 - 6,
    <<Contents:Sz/binary,T/binary>> = T0,
    Acc = Fun(Tag, Contents, Acc0),
    fold_chunks(Fun, Acc, T);
fold_chunks(_Fun, Acc, <<>>) -> Acc.

insert_mat([_|_]=Fs, [MatFace|[MatFace|_]=Mfs], Face, Mat, Acc) ->
    insert_mat(Fs, Mfs, Face, Mat, Acc);
insert_mat([F|Fs], [MatFace|_]=Mfs, Face, Mat, Acc) when Face < MatFace ->
    insert_mat(Fs, Mfs, Face+1, Mat, [F|Acc]);
insert_mat([#e3d_face{mat=Mat0}=F|Fs], [Face|Mfs], Face, Mat, Acc) ->
    case member(Mat, Mat0) of
	true ->
	    %% Don't add an already existing material.
	    insert_mat(Fs, Mfs, Face+1, Mat, [F|Acc]);
	false ->
	    insert_mat(Fs, Mfs, Face+1, Mat, [F#e3d_face{mat=[Mat|Mat0]}|Acc])
    end;
insert_mat([], _, _Face, _Mat, Acc) -> reverse(Acc);
insert_mat(Rest, [], _Face, _Mat, Acc) -> reverse(Acc, Rest).

get_mat_faces(<<_N:16/little,T/binary>>) ->
    ?dbg("Mat num entries: ~p\n", [_N]),
    get_mat_faces(T, []).

get_mat_faces(<<Face:16/little,T/binary>>, Acc) ->
    get_mat_faces(T, [Face|Acc]);
get_mat_faces(<<>>, Acc) -> reverse(Acc).

get_uv(Bin) ->
    get_uv(Bin, []).
get_uv(<<U:32/?FLOAT,V:32/?FLOAT,T/binary>>, Acc) ->
    get_uv(T, [{U,V}|Acc]);
get_uv(<<>>, Acc) -> reverse(Acc).

get_bin_vectors(Bin) ->
    get_bin_vectors(Bin, []).
get_bin_vectors(<<Pos:12/binary,T/binary>>, Acc) ->
    get_bin_vectors(T, [Pos|Acc]);
get_bin_vectors(<<>>, Acc) -> reverse(Acc).

get_faces(Bin) ->
    get_faces(Bin, []).
get_faces(<<A:16/little,B:16/little,C:16/little,Flag:16/little,T/binary>>, Acc) ->
    get_faces(T, [#e3d_face{vs=[A,B,C],vis=Flag band 7}|Acc]);
get_faces(<<>>, Acc) -> reverse(Acc).

get_smooth_groups(Bin) ->
    get_smooth_groups(Bin, []).
get_smooth_groups(<<SG:32/little,T/binary>>, Acc) ->
    get_smooth_groups(T, [SG|Acc]);
get_smooth_groups(<<>>, Acc) -> reverse(Acc).

get_cstring(Bin) ->
    get_cstring(Bin, []).
get_cstring(<<0:8,T/binary>>, Str) ->
    {reverse(Str),T};
get_cstring(<<C:8,T/binary>>, Str) ->
    get_cstring(T, [C|Str]);
get_cstring(<<>>=T, Str) ->
    {reverse(Str),T}.
    
error_msg(Message) ->
    throw({error,Message}).

hard_edges(SmoothGroups, Faces) ->
    hard_edges(SmoothGroups, Faces, []).

hard_edges([SG|SGs], [#e3d_face{vs=[A,B,C]}|Fs], Acc0) ->
    Acc = [edge(A, B, SG),edge(B, C, SG),edge(C, A, SG)|Acc0],
    hard_edges(SGs, Fs, Acc);
hard_edges([], _, Acc) ->
    R = sofs:relation(Acc),
    F0 = sofs:relation_to_family(R),
    F = sofs:to_external(F0),
    foldl(fun({Edge,[SG0|SGs]}, He) ->
		  case foldl(fun(SG, A) -> A band SG end, SG0, SGs) of
		      0 -> [Edge|He];
		      _Other -> He
		  end
	  end, [], F).

edge(A, B, SG) when A < B -> {{A,B},SG};
edge(A, B, SG) -> {{B,A},SG}.

add_uv_to_faces(#e3d_mesh{tx=[]}=Mesh) -> Mesh;
add_uv_to_faces(#e3d_mesh{fs=Fs0}=Mesh) ->
    Fs = foldl(fun(#e3d_face{vs=Vs}=Face, A) ->
		       [Face#e3d_face{tx=Vs}|A]
	       end, [], Fs0),
    Mesh#e3d_mesh{fs=reverse(Fs)}.

clean_mesh(#e3d_mesh{fs=Fs0,vs=Vs0,he=Smooth}=Mesh0) ->
    %% Here we combines vertices that have exactly the same position
    %% and renumber vertices to leave no gaps.
    R = sofs:relation(append_index(Vs0), [{pos,old_vertex}]),
    S = sofs:range(sofs:relation_to_family(R)),
    CR = sofs:canonical_relation(S),
    Map = gb_trees:from_orddict(sofs:to_external(CR)),
    Fs1 = map_faces(Fs0, Map),
    #e3d_mesh{vs=Vs1,fs=Fs} = Mesh =
	e3d_mesh:renumber(Mesh0#e3d_mesh{fs=Fs1,he=[]}),
    He = hard_edges(Smooth, Fs),
    Vs = [{X,Y,Z} || <<X:32/?FLOAT,Y:32/?FLOAT,Z:32/?FLOAT>> <- Vs1],
    Mesh#e3d_mesh{vs=Vs,he=He}.

append_index(L) -> append_index(L, 0, []).
append_index([H|T], I, Acc) -> append_index(T, I+1, [{H,I}|Acc]);
append_index([], _I, Acc) -> Acc.

map_faces(Fs, Map) ->
    map_faces(Fs, Map, []).
map_faces([#e3d_face{vs=Vs0}=Face|Fs], Map, Acc) ->
    Vs = [begin [V|_] = gb_trees:get(V0, Map), V end || V0 <- Vs0],
    map_faces(Fs, Map, [Face#e3d_face{vs=Vs}|Acc]);
map_faces([], _Map, Acc) -> reverse(Acc).

%%%
%%% Export.
%%% 

export(Name, Objs) ->
    Version = make_chunk(16#0002, <<3:32/little>>),
    Editor = make_editor(Name, Objs),
    Main = make_chunk(16#4D4D, [Version,Editor]),
    case file:write_file(Name, Main) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

make_editor(Name, #e3d_file{objs=Objs0,mat=Mat0}) ->
    MeshVer = make_chunk(16#3d3e, <<3:32/little>>),
    Unit = make_chunk(16#0100, <<(1.0):32/?FLOAT>>),
    {Mat1,MatMap} = make_materials_uniq(Mat0),
    Objs1 = make_names_uniq(Objs0),
    Objs = make_objects(Objs1, MatMap),
    Mat2 = make_tx_uniq(Mat1),
    Mat = make_material(Name, Mat2),
    make_chunk(16#3D3D, [MeshVer,Mat,Unit,Objs]).

make_objects([#e3d_object{name=Name,obj=Mesh0}|Objs], MatMap) ->
    Mesh1 = assign_smooth_groups(Mesh0),
    Mesh = e3d_mesh:triangulate(Mesh1),
    MeshChunk = make_mesh(Mesh, MatMap),
    Chunk = make_chunk(16#4000, [utf8(Name),0,MeshChunk]),
    [Chunk|make_objects(Objs, MatMap)];
make_objects([], _) -> [].

make_names_uniq(Objs) ->
    Names = [Name || #e3d_object{name=Name} <- Objs],
    Map0 = e3d_util:make_uniq(Names, 10),
    Map = gb_trees:from_orddict(sort(Map0)),
    [Obj#e3d_object{name=gb_trees:get(Name, Map)} ||
	#e3d_object{name=Name}=Obj <- Objs].

make_tx_uniq(Mat) ->
    Names = foldl(fun({_,Ps}, A) ->
			  case get_map(diffuse, Ps) of
			      none -> A;
			      #e3d_image{name=Name} -> [Name|A]
			  end
		  end, [], Mat),
    MapTrans0 = e3d_util:make_uniq(Names, 8),
    MapTrans = gb_trees:from_orddict(sort(MapTrans0)),
    map(fun({N,Ps0}=M) ->
		case get_map(diffuse, Ps0) of
		    none -> M;
		    #e3d_image{name=Name0}=Image0 ->
			Name = gb_trees:get(Name0, MapTrans),
			Image = Image0#e3d_image{name=Name},
			Ps = replace_map(diffuse, Image, Ps0),
			{N,Ps}
		end
	end, Mat).

get_map(Type, Ps) ->
    Maps = proplists:get_value(maps, Ps, []),
    proplists:get_value(Type, Maps, none).

replace_map(MapType, Val, Ps) ->
    Maps0 = proplists:get_value(maps, Ps, []),
    Maps = [{MapType,Val}|keydelete(MapType, 1, Maps0)],
    keyreplace(maps, 1, Ps, {maps,Maps}).

make_mesh(Mesh0, MatMap) ->
    Mesh = split_vertices(Mesh0),
    make_mesh_1(Mesh, MatMap).

make_mesh_1(#e3d_mesh{vs=Vs,fs=Fs,tx=Tx,matrix=_Matrix0}, MatMap) ->
    %% XXX Matrix0 should be used here.
    VsChunk = make_vertices(Vs),
    FsChunk = make_faces(Fs, MatMap),
    MD = <<1.0:32/?FLOAT,0.0:32/?FLOAT,0.0:32/?FLOAT,
	  0.0:32/?FLOAT,1.0:32/?FLOAT,0.0:32/?FLOAT,
	  0.0:32/?FLOAT,0.0:32/?FLOAT,1.0:32/?FLOAT,
	  0.0:32/?FLOAT,0.0:32/?FLOAT,0.0:32/?FLOAT>>,
    Matrix = make_chunk(16#4160, MD),
    UVs = make_uvs(Tx),
    make_chunk(16#4100, [Matrix,VsChunk,UVs|FsChunk]).

make_vertices(Vs) ->
    Chunk = [<<(length(Vs)):16/little>>|make_vertices(Vs, [])],
    make_chunk(16#4110, Chunk).
    
make_vertices([{X,Y,Z}|Ps], Acc) ->
    Chunk = <<X:32/?FLOAT,Y:32/?FLOAT,Z:32/?FLOAT>>,
    make_vertices(Ps, [Chunk|Acc]);
make_vertices([], Acc) -> reverse(Acc).

make_uvs([]) -> [];
make_uvs(UVs) ->
    Chunk = [<<(length(UVs)):16/little>>|make_uvs(UVs, [])],
    make_chunk(16#4140, Chunk).
    
make_uvs([{U,V}|Ps], Acc) ->
    Chunk = <<U:32/?FLOAT,V:32/?FLOAT>>,
    make_uvs(Ps, [Chunk|Acc]);
make_uvs([], Acc) -> reverse(Acc).

make_faces(Fs, MatMap) ->
    FaceChunk = [<<(length(Fs)):16/little>>|make_faces_1(Fs, [])],
    MatChunk = make_face_mat(Fs, MatMap),
    SmoothChunk = make_smooth_groups(Fs),
    make_chunk(16#4120, [FaceChunk,MatChunk,SmoothChunk]).

make_faces_1([#e3d_face{vs=[A,B,C],vis=Hidden}|Faces], Acc) ->
    Flag = Hidden band 7,
    Face = <<A:16/little,B:16/little,C:16/little,Flag:16/little>>,
    make_faces_1(Faces, [Face|Acc]);
make_faces_1([], Acc) -> reverse(Acc).

make_face_mat(Fs, MatMap) ->
    R0 = make_face_mat_1(Fs, 0, []),
    R = sofs:relation(R0),
    F = sofs:to_external(sofs:relation_to_family(R)),
    map(fun({Name0,Faces}) ->
		Name1 = atom_to_list(Name0),
		Name = gb_trees:get(Name1, MatMap),
		Chunk = [utf8(Name),0,
			 <<(length(Faces)):16/little>>,
			 [<<Face:16/little>> || Face <- Faces]],
		make_chunk(16#4130, Chunk)
	end, F).

make_face_mat_1([#e3d_face{mat=Mat}|Fs], Face, Acc) ->
    make_face_mat_1(Fs, Face+1, [{M,Face} || M <- Mat] ++ Acc);
make_face_mat_1([], _Face, Acc) -> Acc.

make_materials_uniq(Mat0) ->
    Names = [atom_to_list(Name) || {Name,_} <- Mat0],
    Map0 = e3d_util:make_uniq(Names, 16),
    Map = gb_trees:from_orddict(sort(Map0)),
    Mat = [{list_to_atom(gb_trees:get(atom_to_list(Name), Map)),Ps} ||
	      {Name,Ps} <- Mat0],
    {Mat,Map}.

make_material(Filename, Mat) ->
    Base = filename:rootname(Filename, ".3ds"),
    [make_material_1(Base, M) || M <- Mat].

make_material_1(Base, {Name,Mat}) ->
    NameChunk = make_chunk(16#A000, [utf8(atom_to_list(Name)),0]),
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat),
    MatChunks = make_material_2(OpenGL, []),
    TxChunks = make_texture_materials(Maps, Base, []),
    MatShading = make_chunk(16#A100, <<3:16/little>>), %Phong shading.
    make_chunk(16#AFFF, [NameChunk,MatChunks,MatShading|TxChunks]).

make_material_2([{diffuse,{_,_,_,Opac0}=Color}|T], Acc) ->
    Chunk = make_chunk(16#A020, make_rgb(Color)),
    Opacity = make_chunk(16#A050, make_percent(1-Opac0)),
    make_material_2(T, [Chunk,Opacity|Acc]);
make_material_2([{ambient,RGB}|T], Acc) ->
    Chunk = make_chunk(16#A010, make_rgb(RGB)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([{specular,RGB}|T], Acc) ->
    Chunk = make_chunk(16#A030, make_rgb(RGB)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([{shininess,Percent}|T], Acc) ->
    Chunk = make_chunk(16#A040, make_percent(1.0-Percent)),
    make_material_2(T, [Chunk|Acc]);
make_material_2([_|T], Acc) ->
    make_material_2(T, Acc);
make_material_2([], Acc) -> Acc.

make_texture_materials([{diffuse,Map}|T], Base, Acc) ->
    Tx = export_map(16#A200, Map, Base),
    make_texture_materials(T, Base, [Tx|Acc]);
make_texture_materials([_|T], Base, Acc) ->
    make_texture_materials(T, Base, Acc);
make_texture_materials([], _, Acc) -> Acc.

export_map(_, none, _) -> ok;
export_map(ChunkId, #e3d_image{filename=none,name=Name}=Image, Root) ->
    MapFile = filename:join(filename:dirname(Root), Name ++ ".png"),
    ok = e3d_image:save(Image, MapFile),
    export_map(ChunkId, Image#e3d_image{filename=MapFile}, Root);
export_map(ChunkId, #e3d_image{filename=MapFile}, _Root) ->
    FnameChunk = make_chunk(16#A300, [utf8(filename:basename(MapFile)),0]),
    ParamChunk = make_chunk(16#A351, [0,1]),
    make_chunk(ChunkId, [FnameChunk,ParamChunk]).

make_rgb({R0,G0,B0,_}) when is_float(R0), is_float(G0), is_float(B0) ->
    [make_chunk(16#0011, <<(trunc(255*R0)):8,(trunc(255*G0)):8,
			  (trunc(255*B0)):8>>),
     make_chunk(16#0012, <<(trunc(255*R0)):8,(trunc(255*G0)):8,
			  (trunc(255*B0)):8>>)].

make_percent(0) ->
    make_percent(0.0);
make_percent(Percent0) when is_float(Percent0) ->
    Percent = trunc(Percent0*100.0),
    make_chunk(16#0030, <<Percent:16/little>>).
    
make_chunk(Tag, Contents) when is_binary(Contents) ->
    Size = byte_size(Contents) + 6,
    [<<Tag:16/little,Size:32/little>>,Contents];
make_chunk(Tag, Contents) when is_list(Contents) ->
    make_chunk(Tag, list_to_binary(Contents)).

%%%
%%% Create smoothing groups from the hard egdes.
%%%

assign_smooth_groups(#e3d_mesh{fs=Fs0,he=[]}=Mesh) ->
    Fs = [Face#e3d_face{ns=1} || Face <- Fs0],
    Mesh#e3d_mesh{fs=Fs};
assign_smooth_groups(#e3d_mesh{fs=Fs0,he=He}=Mesh) ->
    Es = asg_edges(Fs0, 0, []),
    R0 = sofs:relation(Es),
    ConvR = sofs:converse(R0),
    {Ws0,Rs0} = sofs:partition(2, R0, sofs:set(He)),
    Ws1 = sofs:relative_product(Ws0, ConvR),
    Ws2 = sofs:relation_to_family(Ws1),
    Ws = sofs:to_external(Ws2),
    Rs1 = sofs:restriction(Rs0, sofs:domain(Ws2)),
    Rs2 = sofs:relative_product(Rs1, ConvR),
    Rs3 = sofs:relation_to_family(Rs2),
    Rs = sofs:to_external(Rs3),
    Groups1 = asg_assign_hardness(Ws, gb_trees:empty()),
    Groups2 = asg_assign_softness(Rs, gb_trees:from_orddict(Ws), Groups1),
    Groups3 = gb_trees:to_list(Groups2),
    AllGroupsUsed = asg_all_groups_used(Groups3, 0),
    Groups4 = sofs:from_external(Groups3, [{atom,atom}]),
    Groups5 = sofs:extension(Groups4, sofs:set(lists:seq(0, length(Fs0)-1)),
			     sofs:from_term(AllGroupsUsed)),
    Groups = sofs:to_external(Groups5),
    Fs = asg_assign(Fs0, Groups, []),
    Mesh#e3d_mesh{fs=Fs}.

asg_assign([Face|Fs], [{_,SG}|Sgs], Acc) ->
    asg_assign(Fs, Sgs, [Face#e3d_face{ns=SG}|Acc]);
asg_assign([], [], Acc) -> reverse(Acc).

asg_all_groups_used([{_,G}|T], Acc) ->
    asg_all_groups_used(T, G bor Acc);
asg_all_groups_used([], Acc) -> Acc.
    
asg_edge(A, B) when A < B -> {A,B};
asg_edge(A, B) -> {B,A}.

asg_edges([#e3d_face{vs=[V|_]=Vs}|T], Face, Acc0) ->
    Acc = asg_edges_1(Vs++[V], Face, Acc0),
    asg_edges(T, Face+1, Acc);
asg_edges([], _, Acc) -> Acc.

asg_edges_1([A|[B|_]=Vs], Face, Acc) ->
    asg_edges_1(Vs, Face, [{Face,asg_edge(A, B)}|Acc]);
asg_edges_1([_], _, Acc) -> Acc.

asg_assign_hardness([{Face,HardFs}|T], Groups0) ->
    Groups = asg_assign_hardness_1(Face, HardFs, Groups0, 0),
    asg_assign_hardness(T, Groups);
asg_assign_hardness([], Groups) -> Groups.

asg_assign_hardness_1(Face, [Face|Fs], Groups, Acc) ->
    asg_assign_hardness_1(Face, Fs, Groups, Acc);
asg_assign_hardness_1(Face, [AFace|Fs], Groups, Acc) ->
    case gb_trees:lookup(AFace, Groups) of
	none -> asg_assign_hardness_1(Face, Fs, Groups, Acc);
	{value,G} -> asg_assign_hardness_1(Face, Fs, Groups, Acc bor G)
    end;
asg_assign_hardness_1(Face, [], Groups, NotAllowed) ->
    G = asg_find_group(NotAllowed),
    gb_trees:insert(Face, G, Groups).

asg_assign_softness([{Face,SoftFs}|T], HardTab, Groups0) ->
    Groups = asg_assign_softness_1(Face, SoftFs, HardTab, Groups0),
    asg_assign_softness(T, HardTab, Groups);
asg_assign_softness([], _, Groups) -> Groups.

asg_assign_softness_1(Face, [Face|Fs], HardTab, Groups) ->
    asg_assign_softness_1(Face, Fs, HardTab, Groups);
asg_assign_softness_1(Face, [AFace|Fs], HardTab, Groups0) ->
    case gb_trees:lookup(AFace, Groups0) of
	none ->					%Will get default value.
	    asg_assign_softness_1(Face, Fs, HardTab, Groups0);
	{value,Ga} ->
	    case gb_trees:get(Face, Groups0) of
		G when G band Ga =:= 0 ->
		    Groups = asg_assign_softness_2(Face, AFace, G, Ga,
						   HardTab, Groups0),
		    asg_assign_softness_1(Face, Fs, HardTab, Groups);
		_ ->				%Already soft.
		    asg_assign_softness_1(Face, Fs, HardTab, Groups0)
	    end
    end;
asg_assign_softness_1(_, [], _, Groups) -> Groups.

asg_assign_softness_2(Face, AFace, G0, Ga0, HardTab, Groups0) ->
    NotAllowed = asg_not_allowed(gb_trees:get(Face, HardTab), Groups0) bor
	asg_not_allowed(gb_trees:get(AFace, HardTab), Groups0),
    NewG = asg_find_group(NotAllowed),
    G = G0 bor NewG,
    Ga = Ga0 bor NewG,
    Groups = gb_trees:update(Face, G, Groups0),
    gb_trees:update(AFace, Ga, Groups).
				 
asg_not_allowed(Fs, Groups) ->
    asg_not_allowed(Fs, Groups, 0).
    
asg_not_allowed([F|Fs], Groups, Acc) ->
    asg_not_allowed(Fs, Groups, gb_trees:get(F, Groups) bor Acc);
asg_not_allowed([], _, Acc) -> Acc.
    
asg_find_group(NotAllowed) ->
    asg_find_group(1, NotAllowed).

asg_find_group(B, NotAllowed) when B band NotAllowed =/= 0 ->
    asg_find_group(B bsl 1, NotAllowed);
asg_find_group(B, _) -> B.

make_smooth_groups(Fs) ->
    Contents = [<<Smo:32/little>> || #e3d_face{ns=Smo} <- Fs],
    make_chunk(16#4150, Contents).
    
%%%
%%% Split vertices: Each vertex in the 3D Studio format can only have one
%%% UV coordinate. Therefore, we must split each vertex that have different
%%% UV coordinates in different faces.
%%%

split_vertices(#e3d_mesh{tx=[]}=Mesh) -> Mesh;
split_vertices(Mesh0) ->
    Mesh = split_dummy_uvs(Mesh0),
    #e3d_mesh{vs=Vtab0,fs=Fs0,tx=Tx0} = Mesh,
    F = split_vertices_1(Fs0, []),
    NextV = length(Vtab0),
    Map = split_make_map(F, NextV, []),
    Fs = split_remap_faces(Fs0, gb_trees:from_orddict(sort(Map)), []),
    Rmap = keysort(2, Map),
    Vtab = split_extend_vtab(Rmap, list_to_tuple(Vtab0), reverse(Vtab0)),
    Tx = split_reorder_tx(Fs, list_to_tuple(Tx0), []),
    Mesh#e3d_mesh{vs=Vtab,fs=Fs,tx=Tx}.

split_vertices_1([#e3d_face{vs=[A,B,C],tx=[Ta,Tb,Tc]}|Faces], Acc) ->
    split_vertices_1(Faces, [{A,Ta},{B,Tb},{C,Tc}|Acc]);
split_vertices_1([], Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    sofs:to_external(F).

split_make_map([{_,[_]}|T], NextV, Acc) ->
    split_make_map(T, NextV, Acc);
split_make_map([{V,[_|UVs]}|T], NextV, Acc0) ->
    Acc = split_make_map_1(UVs, V, NextV, Acc0),
    split_make_map(T, NextV+length(UVs), Acc);
split_make_map([], _, Acc) -> Acc.

split_make_map_1([UV|UVs], V, NextV, Acc) ->
    split_make_map_1(UVs, V, NextV+1, [{{V,UV},NextV}|Acc]);
split_make_map_1([], _, _, Acc) -> Acc.
    
split_remap_faces([#e3d_face{vs=[A0,B0,C0],tx=[Ta,Tb,Tc]}=F|Faces], Map, Acc) ->
    A = split_remap_vtx(A0, Ta, Map),
    B = split_remap_vtx(B0, Tb, Map),
    C = split_remap_vtx(C0, Tc, Map),
    split_remap_faces(Faces, Map, [F#e3d_face{vs=[A,B,C]}|Acc]);
split_remap_faces([], _, Acc) -> reverse(Acc).

split_remap_vtx(V, T, Map) ->
    case gb_trees:lookup({V,T}, Map) of
	none -> V;
	{value,NewV} -> NewV
    end.

split_extend_vtab([{{V,_},_}|T], OldVtab, Acc) ->
    split_extend_vtab(T, OldVtab, [element(V+1, OldVtab)|Acc]);
split_extend_vtab([], _, Acc) -> reverse(Acc).

split_reorder_tx([#e3d_face{vs=[A,B,C],tx=[Ta,Tb,Tc]}|Faces], OldTx, Acc0) ->
    Acc = [{A,element(Ta+1, OldTx)},
	   {B,element(Tb+1, OldTx)},
	   {C,element(Tc+1, OldTx)}|Acc0],
    split_reorder_tx(Faces, OldTx, Acc);
split_reorder_tx([], _, Acc) ->
    split_reorder_tx_1(usort(Acc), 0, []).

split_reorder_tx_1([{I,UV}|UVs], I, Acc) ->
    split_reorder_tx_1(UVs, I+1, [UV|Acc]);
split_reorder_tx_1([], _, Acc) -> reverse(Acc).

%% split_dummy_uvs(Mesh0) -> Mesh
%%  Add dummy UVs if necessary to make sure that all vertices
%%  in all faces have UV coordinates.
split_dummy_uvs(#e3d_mesh{fs=Fs0,tx=Tx0}=Mesh) ->
    DummyUV = length(Tx0),
    Tx = Tx0 ++ [{0.0,0.0}],
    Fs = split_dummy_uvs_1(Fs0, DummyUV, []),
    Mesh#e3d_mesh{fs=Fs,tx=Tx}.

split_dummy_uvs_1([#e3d_face{tx=[_,_,_]}=F|T], UV, Acc) ->
    split_dummy_uvs_1(T, UV, [F|Acc]);
split_dummy_uvs_1([F|T], UV, Acc) ->
    split_dummy_uvs_1(T, UV, [F#e3d_face{tx=[UV,UV,UV]}|Acc]);
split_dummy_uvs_1([], _, Acc) -> reverse(Acc).

utf8(Name) ->
    unicode:characters_to_binary(Name).
