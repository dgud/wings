%%
%%  e3d_mesh.erl --
%%
%%     Utility functions for E3D meshes, such as cleanup and triangulation.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d_mesh).
-export([clean_faces/1,orient_normals/1,transform/1,transform/2,
	 merge_vertices/1,triangulate/1,quadrangulate/1,
	 make_quads/1,make_polygons/1,vertex_normals/1,renumber/1,partition/1,
	 split_by_material/1,used_materials/1]).
-export([triangulate_face/2,triangulate_face/3,
	 triangulate_face_with_holes/3]).
-export([quadrangulate_face/2,quadrangulate_face_with_holes/3]).
-export([hard_edges_from_normals/1,slit_hard_edges/1,slit_hard_edges/2]).
-export([face_areas/1,face_areas/2]).

-include("e3d.hrl").
-import(lists, [foreach/2,sort/1,reverse/1,reverse/2,seq/2,
		foldl/3,filter/2,mapfoldl/3,mapfoldr/3,last/1,zip/2]).

%% orient_normals(Mesh0) -> Mesh
%%  Orient the face normals consistently.
orient_normals(Mesh) ->
    e3d__meshclean:orient_normals(Mesh).

%% clean_faces(Mesh0) -> Mesh
%%  Remove duplicate vertices and faces with fewer than three edges.
clean_faces(Mesh) ->
    e3d__meshclean:clean_faces(Mesh).

%% transform(Mesh0) -> Mesh
%%  Transform all vertices in the mesh by the matrix in the e3d_mesh
%%  record.
transform(#e3d_mesh{matrix=Matrix}=Mesh) ->
    transform(Mesh#e3d_mesh{matrix=identity}, Matrix).
    
%% transform(Mesh0, Matrix) -> Mesh
%%  Transform all vertices in the mesh by the matrix.
transform(#e3d_mesh{vs=Vs0,matrix=ObjMatrix}=Mesh, Matrix0) ->
    Matrix = e3d_mat:mul(Matrix0, ObjMatrix),
    case e3d_mat:is_identity(Matrix) of
	true -> Mesh;
	false ->
	    Vs1 = foldl(fun(P, A) ->
				[e3d_mat:mul_point(Matrix, P)|A]
			end, [], Vs0),
	    Vs = reverse(Vs1),
	    Mesh#e3d_mesh{vs=Vs,matrix=identity}
    end.

%% make_quads(Mesh0) -> Mesh
%%  If two adjacent triangles share a hidden edge, combine the
%%  triangles to a quad. Triangles with more than one hidden edge
%%  will never be combined to avoid isolating vertices and/or
%%  creating concave polygons.

make_quads(#e3d_mesh{type=triangle,fs=Fs0}=Mesh) ->
    Fs = filter_hidden_edges(Fs0),
    make_polygons(Mesh#e3d_mesh{fs=Fs});
make_quads(Mesh) -> Mesh.

%% make_polygons(Mesh0) -> Mesh
%%  Eliminate hidden edges to create polygons. Special care must be
%%  taken to eliminate isolated vertices and not to create holes.
%%  XXX There are knowns problems in this function.

make_polygons(#e3d_mesh{type=triangle,fs=Fs0}=Mesh0) ->
    Ftab0 = number_faces(Fs0),
    Es = rhe_collect_edges(Ftab0),
    Ftab1 = gb_trees:from_orddict(Ftab0),
    Cs = components(Es),
    Ftab = merge_components(Cs, Ftab1),
    Fs1 = gb_trees:values(Ftab),
    Fs = filter(fun ({merged,_}) -> false;
		    (_) -> true
		end, Fs1),
    Mesh = Mesh0#e3d_mesh{type=polygon,fs=Fs},
    renumber(Mesh);
make_polygons(Mesh) -> Mesh.

%% merge_vertices(Mesh0) -> Mesh
%%  Combine vertices that have exactly the same position,
%%  then renumber the mesh.
merge_vertices(#e3d_mesh{fs=Fs0,vs=Vs0, he=He0}=Mesh) ->
    R = sofs:relation(append_index(Vs0), [{pos,old_vertex}]),
    S = sofs:range(sofs:relation_to_family(R)),
    CR = sofs:canonical_relation(S),
    Map = gb_trees:from_orddict(sofs:to_external(CR)),
    Fs = map_faces(Fs0, Map),
    MV = fun({A,B}) ->
                 [NewA|_] = gb_trees:get(A, Map),
                 [NewB|_] = gb_trees:get(B, Map),
                 case NewA < NewB of
                     true -> {NewA,NewB};
                     false -> {NewB,NewA}
                 end
         end,
    He = [MV(Edge) || Edge <- He0],
    renumber(Mesh#e3d_mesh{fs=Fs, he=He}).

%% hard_edges_from_normals(Mesh0) -> Mesh
%%  Estimates restrictivly hard edges from normals if no hard_edges are available
%%  and vertex normals are available.
hard_edges_from_normals(#e3d_mesh{fs=Ftab,vs=Vtab,he=[],ns=[_|_]=Ntab0}=Mesh) ->
    FN = face_normals(Ftab, list_to_tuple(Vtab)),
    Ntab = array:from_list([e3d_vec:norm(N) || N <- Ntab0]),
    {_,He0} = foldl(fun(#e3d_face{vs=Vs, ns=VNs}, {N, Acc}) ->
                            {N+1, add_hard_edges(Vs, VNs, {Vs,VNs},
                                                 Ntab, gb_trees:get(N,FN), Acc)}
                    end, {0, []}, Ftab),
    %% Cannot use sofs:relation_to_family because family is a set!!
    He = filter_edges(lists:sort(He0), []),
    Mesh#e3d_mesh{he=He};
%% He already set or no normals available
hard_edges_from_normals(Mesh) ->
    Mesh.

filter_edges([{Edge,N1},{Edge,N2}|Rest], Acc) ->
    case is_equal_v(N1,N2) of
        true -> filter_edges(Rest, Acc);
        false -> filter_edges(Rest, [Edge|Acc])
    end;
filter_edges([{Edge,_}|Rest], Acc) ->
    filter_edges(Rest, [Edge|Acc]);
filter_edges([], Acc) ->
    Acc.

add_hard_edges([V1|[V2|_]=Vs], [N1|[N2|_]=Ns], First, Ntab, FaceN, Acc) ->
    case is_equal_v(array:get(N1,Ntab),FaceN) andalso
         is_equal_v(array:get(N2,Ntab),FaceN)
    of
        true ->
            Edge = {vn_edge_name(V1,V2),FaceN},
            add_hard_edges(Vs, Ns, First, Ntab, FaceN, [Edge|Acc]);
        false ->
            add_hard_edges(Vs, Ns, First, Ntab, FaceN, Acc)
    end;
add_hard_edges([V1], [N1], {[V2|_],[N2|_]}, Ntab, FaceN, Acc) ->
    case is_equal_v(array:get(N1,Ntab),FaceN) andalso
        is_equal_v(array:get(N2,Ntab),FaceN)
    of
        true ->
            Edge = {vn_edge_name(V1,V2),FaceN},
            [Edge|Acc];
        false ->
            Acc
    end;
add_hard_edges(_, _, _, _, _, Acc) ->
    %% Ignore if face is missing some normals
    Acc.

is_equal_v(N,N) -> true;
is_equal_v(N1,N2) -> e3d_vec:dist_sqr(N1,N2) < 0.00001.

%%%
%%% Mesh triangulation.
%%%

triangulate(#e3d_mesh{}=Mesh) ->
    e3d__tri_quad:triangulate(Mesh).

triangulate_face(Face, Vcoords) ->
    e3d__tri_quad:triangulate_face(Face, Vcoords).

triangulate_face(Face, Normal, Vcoords) ->
    e3d__tri_quad:triangulate_face(Face, Normal, Vcoords).

triangulate_face_with_holes(Face, Holes, Vcoords) ->
    e3d__tri_quad:triangulate_face_with_holes(Face, Holes, Vcoords).

%%%
%%% Mesh quadrangulation.
%%%

quadrangulate(#e3d_mesh{}=Mesh) ->
    e3d__tri_quad:quadrangulate(Mesh).

quadrangulate_face(Face, Vcoords) ->
    e3d__tri_quad:quadrangulate_face(Face, Vcoords).

quadrangulate_face_with_holes(Face, Holes, Vcoords) ->
    e3d__tri_quad:quadrangulate_face_with_holes(Face, Holes, Vcoords).

%% vertex_normals(Mesh0) -> Mesh
%%  Calculate vertex normals for each face.
vertex_normals(#e3d_mesh{fs=Ftab,vs=Vtab0,he=He}=Mesh) ->
    Vtab = list_to_tuple(Vtab0),
    FaceNormals = face_normals(Ftab, Vtab),

    %% Calculate normals for vertices with no hard edges.
    HardVs = sofs:field(sofs:relation(He)),
    VtxFace0 = sofs:relation(vtx_to_face_tab(Ftab)),
    HardVtxFace0 = sofs:restriction(VtxFace0, HardVs),
    VtxFace1 = sofs:difference(VtxFace0, HardVtxFace0),
    VtxFace2 = sofs:relation_to_family(VtxFace1),
    VtxFace = sofs:to_external(VtxFace2),
    VtxNormals0 = vertex_normals(VtxFace, 0, FaceNormals),

    %% Calculate normals for vertices surrounded by one or more hard edges.
    HardVtxFace = sofs:to_external(HardVtxFace0),
    VtxNormals1 = vn_hard_normals(He, HardVtxFace, Ftab,
				  FaceNormals, VtxNormals0),

    %% Generate face data.
    VtxNormals = gb_trees:from_orddict(sort(VtxNormals1)),
    Faces = vn_faces(Ftab, VtxNormals, 0, []),
    Normals0 = gb_trees:values(VtxNormals),
    Normals1 = sort(Normals0),
    Normals = [N || {_Vn,N} <- Normals1],
    Mesh#e3d_mesh{fs=Faces,ns=Normals}.

%% renumber(Mesh0) -> Mesh
%%  Removes vertices and vertex attributes such as UV coordinates and
%%  vertex normals that are not referenced from any faces and renumbers
%%  vertices and attributes to remove the gaps.
%%  XXX Vertex colors are not renumbered yet, meaning that there can
%%  remain unused vertex colors.
renumber(#e3d_mesh{tx=Tx,vs=Vtab,ns=Ns}=Mesh) ->
    {UsedVs,UsedUv,UsedNs} = rn_used_vs(Mesh),
    if
	length(Vtab) =/= length(UsedVs);
	length(Tx) =/= length(UsedUv);
	length(Ns) =/= length(UsedNs) ->
	    renumber_1(Mesh, UsedVs, UsedUv, UsedNs);
	true -> Mesh
    end.

%% partition(Mesh0) -> [Mesh]
%%  Partitions a mesh in disjoint sub-meshes.
partition(#e3d_mesh{fs=Faces0,he=He0}=Template) ->
    Faces1 = number_faces(Faces0),
    Faces = sofs:relation(Faces1, [{face,data}]),
    FacePart = partition_1(Faces, He0),
    Res = foldl(fun({Fs0,He}, A) ->
			Fs = strip_index(Fs0),
			Mesh = renumber(Template#e3d_mesh{fs=Fs,he=He}),
			[Mesh|A]
		end, [], sort(FacePart)),
    reverse(Res).

%% split_by_material(Mesh0) -> [Mesh]
%%  Split a mesh into separate meshes where all faces in each
%%  mesh have the same material.
split_by_material(#e3d_mesh{fs=Fs}=Mesh) ->
    R0 = foldl(fun(#e3d_face{mat=Mat}=Face, A) ->
		       [{Mat,Face}|A]
	       end, [], Fs),
    R = sofs:relation(R0, [{mat,face}]),
    F = sofs:relation_to_family(R),
    Ps = sofs:to_external(sofs:range(F)),
    [renumber(Mesh#e3d_mesh{fs=P}) || P <- Ps].

%% used_materials(Mesh) -> [MaterialName]
%%  Returns a sorted list of all materials used in the given mesh.
used_materials(#e3d_mesh{fs=Fs0}) ->
    used_materials_1(Fs0, []).

%% Algorithm for slitting hard edges.
%%
%% The resulting is not a closed body, rather a mess(h),
%% but that does not matter. Some faces may get an extra vertex,
%% so the mesh type may change from 'triangle' to 'polygon',
%% unless the option 'slit_end_vertices' is used which will
%% keep the vertex count.
%%
%% Vertices belonging to a hard edge are simply duplicated.
%% The first time a hard edge is encountered, the face
%% gets to keep the vertices, but subsequent times for the same
%% hard edge - the faces get new duplicate position vertices instead.
%%
%% Without the option 'slit_end_vertices', end vertices of 
%% hard edge chains are not duplicated, since edges that go to the 
%% end vertex might look hard in some renderers, non just the hard ones. 
%% End vertices are those occuring only once in the hard edge list.
%%
%% Solo hard edges (no chain (or rather very short chain)) gets special 
%% treatment. They get cut in two by a new own vertex, unless the option
%% 'slit_end_vertices' is used which will simply duplicate both end vertices
%% as for a longer hard edge chain.
%%
slit_hard_edges(Mesh) -> slit_hard_edges(Mesh, []).
%%
slit_hard_edges(Mesh0=#e3d_mesh{he=[]}, Options) when is_list(Options) -> Mesh0;
slit_hard_edges(Mesh0=#e3d_mesh{vs=Vs0,vc=Vc0,tx=Tx0,ns=Ns0,fs=Fs0,he=He0},
		Options) when is_list(Options) ->
    %%io:format("Before: "),
    %%print_mesh(Mesh0),
    VsT = list_to_tuple(Vs0),
    VcT = list_to_tuple(Vc0),
    TxT = list_to_tuple(Tx0),
    NsT = list_to_tuple(Ns0),
    Old = Mesh0#e3d_mesh{vs=VsT,vc=VcT,tx=TxT,ns=NsT},
    New = #e3d_mesh{vs={size(VsT),[]},vc={size(VcT),[]},
		    tx={size(TxT),[]},ns={size(NsT),[]}},
    VsGt = 
	case proplists:get_bool(slit_end_vertices, Options) of
	    false ->%% Altered - PM (11/8/2004)
		foldl(fun({V1,V2}, Gt) when V1 < V2 -> 
			      gb_trees_increment(V2, 1, 
						 gb_trees_increment(V1, 1, Gt))
		      end, gb_trees:empty(), He0);
	    true ->
		%% Fake a vertex count of 2 (i.e more than 1) for all
		foldl(fun({V1,V2}, Gt) when V1 < V2 -> 
			      gb_trees:enter(V2, 2, 
					     gb_trees:enter(V1, 2, Gt))
		      end, gb_trees:empty(), He0)
	end,
    HeGt = foldl(fun({V1,V2}=E, Gt) when V1 < V2 -> 
			 gb_trees:insert(E, 0, Gt)
		 end, gb_trees:empty(), He0),
    #e3d_mesh{type=Type,vs=Vs1,vc=Vc1,tx=Tx1,ns=Ns1,fs=Fs1} =
	slit_hard_f(Old, VsGt, HeGt, Fs0, New, []),
    Mesh = Mesh0#e3d_mesh{type=Type,vs=Vs0++Vs1,vc=Vc0++Vc1,
			  tx=Tx0++Tx1,ns=Ns0++Ns1,fs=Fs1,he=[]},
    %%io:format("After: "),
    %%print_mesh(Mesh),
    Mesh.

%% Calculate area of faces. Return list of areas for each face.
%% The areas are for one possible triangulation. This is only
%% important if the faces are not flat.

face_areas(#e3d_mesh{vs=Vs,fs=Fs}) -> face_areas(Fs, Vs).

face_areas(Fs, Vs) when is_list(Fs), is_list(Vs) -> 
    face_areas_1(Fs, Vs, list_to_tuple(Vs)).

%%%
%%% End of exported functions. Local functions follow.
%%%

%%%
%%% Help functions for make_quads/1 and make_polygons/1.
%%%

components(Es) ->
    G = digraph:new(),
    components_1(Es, G).

components_1([{_,[{Fa,Va,Vb},{Fb,Vb,Va}]}|Es], G) ->
    digraph:add_vertex(G, Fa),
    digraph:add_vertex(G, Fb),
    digraph:add_edge(G, Fa, Fb, {Va,Vb}),
    components_1(Es, G);
components_1([_|Es], G) ->
    components_1(Es, G);
components_1([], G) ->
    Cs0 = digraph_utils:components(G),
    Cs = [annotate_component(G, C) || C <- Cs0],
    digraph:delete(G),
    Cs.

annotate_component(G, [F|Fs]) ->
    case annotate_out_edges(G, F) of
	[] -> annotate_component(G, Fs);
	Es -> [{F,Es}|annotate_component(G, Fs)]
    end;
annotate_component(_, []) -> [].

annotate_out_edges(G, F) ->
    [annotate_edge(G, F, E) || E <- digraph:out_edges(G, F)].

annotate_edge(G, Fa, E) ->
    {_,Fa,Fb,Vs} = digraph:edge(G, E),
    {Fb,Vs}.

merge_components([C|Cs], Ftab0) ->
    try merge_component(C, Ftab0) of
	Ftab -> merge_components(Cs, Ftab)
    catch
	error:_R ->
	    %%Stk = erlang:get_stacktrace(),
	    %%io:format("\n~p\n", [_R]),
	    %%io:format("~p\n", [Stk]),
	    merge_components(Cs, Ftab0)
    end;
merge_components([], Ftab) -> Ftab.

merge_component([{Fa0,Fs}], Ftab0) ->
    Ftab = merge_comp_faces(Fs, Fa0, Ftab0),
    {_Fa,#e3d_face{vs=Vs}} = lookupMergedFace(Fa0, Ftab),
    %% There must be no repeated vertices.
    true = length(Vs) =:= length(lists:usort(Vs)),
    Ftab;
merge_component([{Fa,Fs}|Es], Ftab0) ->
    Ftab = merge_comp_faces(Fs, Fa, Ftab0),
    merge_component(Es, Ftab);
merge_component([], Ftab) -> Ftab.

merge_comp_faces([{Fb,{Va,Vb}}|Fs], Fa, Ftab0) ->
    Ftab = merge_faces_1(Fa, Fb, Va, Vb, Ftab0),
    merge_comp_faces(Fs, Fa, Ftab);
merge_comp_faces([], _, Ftab) -> Ftab.

merge_faces_1(Fa0, Fb0, Va, Vb, Ftab0) ->
    {Fa,FaInfo} = lookupMergedFace(Fa0, Ftab0),
    {Fb,FbInfo} = lookupMergedFace(Fb0, Ftab0),

%% Since we can now merge polys with more than one invisible edge,
%% we have to watch out for situation where both edges are in the
%% same face (ie, we will be isolating a vertex). The other possibility
%% when Fa == Fb is that the polygon has a hole; but since the e3d_face
%% record doesn't allow for holes, I'm leaving the mesh as is if that's
%% the case.
    if
	Fa =:= Fb ->
	    case FaInfo of
		#e3d_face{vs=Vs1,tx=Tx1}=Rec0 ->
		    case eliminateIsolatedVert(Va, Vb, Vs1) of
			notFound -> Ftab0;
			Vs when is_list(Vs) ->
			    Tx = merge_uvs(Vs, Vs1, Vs1, Tx1, Tx1),
			    Rec = Rec0#e3d_face{vs=Vs,tx=Tx,vis=-1},
			    gb_trees:update(Fa, Rec, Ftab0)
		    end;
		_ -> Ftab0
	    end;
	true ->
	    case {FaInfo,FbInfo} of
		{#e3d_face{vs=Vs1,tx=Tx1,mat=Mat}=Rec0,
		 #e3d_face{vs=Vs2,tx=Tx2,mat=Mat}} ->
		    case merge_faces_2(Va, Vb, Vs1, Vs2) of
			error -> Ftab0;
			Vs when is_list(Vs) ->
			    Tx = merge_uvs(Vs, Vs1, Vs2, Tx1, Tx2),
			    Rec = Rec0#e3d_face{vs=Vs,tx=Tx,vis=-1},
			    Ftab = gb_trees:update(Fa, Rec, Ftab0),
			    gb_trees:update(Fb, {merged,Fa}, Ftab)
		    end;
		{_FaInfo,_FbInfo} ->
		    Ftab0
	    end
    end.

lookupMergedFace(FaceNum, FaceTable) ->
    case gb_trees:get(FaceNum, FaceTable) of
	{merged,MergedFaceNum} ->
	    lookupMergedFace(MergedFaceNum, FaceTable);
	FaceInfo -> {FaceNum,FaceInfo}
    end.

eliminateIsolatedVert(_,_,VList) when length(VList) < 3 ->
    notFound;
eliminateIsolatedVert(Va, Vb, VList) ->
    eliminateIsolatedVert(Va, Vb, VList, length(VList)).

eliminateIsolatedVert(_,_,_,0) -> notFound;
eliminateIsolatedVert(Va, Vb, [Va,Vb,Va|VTail], _) ->
    [Va|VTail];  
eliminateIsolatedVert(Va, Vb, [Vb,Va,Vb|VTail], _) ->
    [Vb|VTail];   
eliminateIsolatedVert(Va,Vb,[V|VTail],Remaining) ->
    eliminateIsolatedVert(Va,Vb,VTail ++ [V], Remaining -1).

merge_uvs(_, _, _, [], []) -> [];
merge_uvs(Vs, Vs1, Vs2, Tx1, Tx2) ->
    R0 = [zip(Vs1, Tx1),zip(Vs2, Tx2)],
    R1 = sofs:set(R0, [[{v,uv}]]),
    R = sofs:union(R1),
    F0 = sofs:relation_to_family(R),
    F = gb_trees:from_orddict(sofs:to_external(F0)),
    merge_uvs_1(Vs, F).

merge_uvs_1([V|T], V2UV) ->
    [UV|_] = gb_trees:get(V, V2UV),
    [UV|merge_uvs_1(T, V2UV)];
merge_uvs_1([], _) -> [].

merge_faces_2(Va, Vb, VsA0, VsB0) ->
    VsA = rot_face(Va, Vb, VsA0),
    VsB = rot_face(Va, Vb, VsB0),
    merge_faces_3(Va, Vb, VsA, VsB).

merge_faces_3(Va, Vb, [Va,Vb,Vx], [Vb,Va,Vy]) -> [Vx,Va,Vy,Vb];

%% Altered - PM (11/8/2004)
%% merge_faces_3(Va, Vb, [Va,Vb,Vx], [Va,Vb,Vy]) -> [Vx,Va,Vy,Vb];
merge_faces_3(Va, Vb, [Vb,Va,Vx], [Va,Vb,Vy]) -> [Vx,Vb,Vy,Va];
%% Altered - end

merge_faces_3(Va, Vb, [Va,Vb|Vs1], [Vb,Va|Vs2]) ->
    [Vb|Vs1]++[Va|Vs2];

%% Altered - PM (11/8/2004)
%% merge_faces_3(Va, Vb, [Va,Vb|Vs1], [Va,Vb|Vs2]) ->
%%    [Vb|Vs1]++[Va|Vs2];
merge_faces_3(Va, Vb, [Vb,Va|Vs1], [Va,Vb|Vs2]) ->
    [Va|Vs1]++[Vb|Vs2];
%% Altered - end

merge_faces_3(_Va, _Vb, _Vs1, _Vs2) -> error.

%% rot_face(Va, Vb, [Vertex]) -> [Vertex]
%%  Rotate the vertices making up the face so that the Va and Vb
%%  vertices are the first in the face (in either order).
%%  This function will cause an exception if that is not possible.
rot_face(Va, Vb, [Va,Vb|_]=Face) -> Face;
rot_face(Va, Vb, [Vb,Va|_]=Face) -> Face;
rot_face(Va, Vb, [Va,Vx,Vb]) -> [Vb,Va,Vx];
rot_face(Va, Vb, [Vb,Vx,Va]) -> [Va,Vb,Vx];
rot_face(Va, Vb, [Vx,Va,Vb]) -> [Va,Vb,Vx];
rot_face(Va, Vb, [Vx,Vb,Va]) -> [Vb,Va,Vx];
rot_face(Va, Vb, Vs) -> rot_face(Va, Vb, Vs, []).

rot_face(Va, Vb, [Va,Vb|_]=Vs, Acc) -> Vs ++ reverse(Acc);
rot_face(Va, Vb, [Vb,Va|_]=Vs, Acc) -> Vs ++ reverse(Acc);
rot_face(Va, Vb, [Va|_]=Vs0, Acc) ->
    %% If the first vertex is Va, but the next is not Vb,
    %% then we must expect to find Vb as the last element of Vs0
    %% and Acc must be an empty list. Otherwise the rotation is
    %% not possible.
    [] = Acc,					%Necessary condition.
    [Vb|Vs] = reverse(Vs0),
    [Vb|reverse(Vs)];
rot_face(Va, Vb, [Vb|_]=Vs0, Acc) ->
    %% See the comment for the previous clause
    %% (exchanging the roles of Va and Vb).
    [] = Acc,
    [Va|Vs] = reverse(Vs0),
    [Va|reverse(Vs)];
rot_face(Va, Vb, [V|Vs], Acc) -> rot_face(Va, Vb, Vs, [V|Acc]).
    
rhe_collect_edges(Fs) ->
    rhe_collect_edges(Fs, []).

rhe_collect_edges([{Face,#e3d_face{vs=Vs,vis=Vis0}}|Fs], Acc0) ->
    Vis = Vis0 band 7,
    Pairs = invis_pairs(Vs, Vis),
    Acc = rhe_edges(Pairs, Face, Acc0),
    rhe_collect_edges(Fs, Acc);
rhe_collect_edges([], Es0) ->
    Es1 = sofs:relation(Es0),
    Es = sofs:relation_to_family(Es1),
    sofs:to_external(Es).

rhe_edges([{Va,Vb}=Name|Ps], Face, Acc) when Va < Vb ->
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb}}|Acc]);
rhe_edges([{Va,Vb}|Ps], Face, Acc) ->
    Name = {Vb,Va},
    rhe_edges(Ps, Face, [{Name,{Face,Va,Vb}}|Acc]);
rhe_edges([], _Face, Acc) -> Acc.

invis_pairs(Vs, Vis) ->
    invis_pairs(Vs, Vs, Vis, []).

invis_pairs([V1|[V2|_]=Vs], More, Vis, Acc0) ->
    Acc = case visible(Vis) of
	      invisible -> [{V1,V2}|Acc0];
	      visible -> Acc0
	  end,
    invis_pairs(Vs, More, Vis bsl 1, Acc);
invis_pairs([V1], [V2|_], Vis, Acc) ->
    case visible(Vis) of
	invisible -> [{V1,V2}|Acc];
	visible -> Acc
    end.

visible(F) when F band 4 =/= 0 -> visible;
visible(_) -> invisible.


%% filter_hidden_edges([#e3d_face{}) -> [#e3d_face{}].
%%  Retain only hidden edges in a face if the other two edges in the
%%  are visible, otherwise set all edges to visible.

filter_hidden_edges(Fs) ->
    filter_hidden_edges(Fs, []).

filter_hidden_edges([#e3d_face{vis=Vis0}=F|Fs], Acc) ->
    Vis = case Vis0 band 2#111 of
	      2#110=V -> V;
	      2#101=V -> V;
	      2#011=V -> V;
	      _ -> 2#111			%More than one edge is invisible.
	  end,
    filter_hidden_edges(Fs, [F#e3d_face{vis=Vis}|Acc]);
filter_hidden_edges([], Acc) -> reverse(Acc).

%%%
%%% Help functions for vertex_normals/1.
%%%

vn_faces([#e3d_face{vs=Vs}=E3DFace|Fs], VtxNormals, Face, Acc) ->
    Ns0 = foldl(fun(V, A) ->
			[vn_lookup(V, Face, VtxNormals)|A]
		end, [], Vs),
    Ns = reverse(Ns0),
    vn_faces(Fs, VtxNormals, Face+1, [E3DFace#e3d_face{ns=Ns}|Acc]);
vn_faces([], _VtxNormals, _Face, Acc) -> reverse(Acc).
    
vn_lookup(V, Face, VtxNormals) ->
    case gb_trees:lookup(V, VtxNormals) of
	{value,{Vn,_}} -> Vn;
	none ->
	    {Vn,_} = gb_trees:get({V,Face}, VtxNormals),
	    Vn
    end.
	    
face_normals(Ftab, Vtab) ->
    {Ns,_} = mapfoldl(fun(#e3d_face{vs=Vs0}, Face) ->
			      Vs = [element(V+1, Vtab) || V <- Vs0],
			      {{Face,e3d_vec:normal(Vs)},Face+1}
		      end, 0, Ftab),
    gb_trees:from_orddict(Ns).

vtx_to_face_tab(Fs) ->
    vtx_to_face_tab(Fs, 0, []).

vtx_to_face_tab([#e3d_face{vs=Vs}|Fs], Face, Acc0) ->
    Acc = [{V,Face} || V <- Vs] ++ Acc0,
    vtx_to_face_tab(Fs, Face+1, Acc);
vtx_to_face_tab([], _Face, Acc) -> Acc.

vertex_normals(Vfs, Vn, FaceNormals) ->
    vertex_normals(Vfs, Vn, FaceNormals, []).

vertex_normals([{V,Fs}|Vfs], Vn, FaceNormals, Acc) ->
    Ns = [gb_trees:get(F, FaceNormals) || F <- Fs],
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    vertex_normals(Vfs, Vn+1, FaceNormals, [{V,{Vn,N}}|Acc]);
vertex_normals([], _Vn, _FaceNormals, Acc) -> Acc.

vn_hard_normals([], _HardVtxFace, _Fs, _FaceNormals, VtxNormals) ->
    VtxNormals;
vn_hard_normals(He, HardVtxFace, Fs, FaceNormals, VtxNormals0) ->
    Hard = sofs:set(He),
    Edges = sofs:relation(vn_face_edges(Fs, 0, [])),
    Soft0 = sofs:drestriction(Edges, Hard),
    Soft = sofs:relation_to_family(Soft0),
    G = digraph:new(),
    make_digraph_1(G, sofs:to_external(Soft)),

    VtxNormals = vn_hard_normals_1(G, HardVtxFace, FaceNormals,
				   length(VtxNormals0), VtxNormals0),
    digraph:delete(G),
    VtxNormals.

vn_hard_normals_1(G, [VF|VFs], FaceNormals, Vn, Acc) ->
    Reachable = digraph_utils:reachable([VF], G),
    Ns0 = [gb_trees:get(Face, FaceNormals) || {_,Face} <- Reachable],
    N = case Ns0 of
	    [N0] -> N0;
	    Ns -> e3d_vec:norm(e3d_vec:add(Ns))
	end,
    vn_hard_normals_1(G, VFs, FaceNormals, Vn+1, [{VF,{Vn,N}}|Acc]);
vn_hard_normals_1(_G, [], _FaceNormals, _Vn, Acc) -> Acc.
    
make_digraph_1(G, [{{Va,Vb},[Fx,Fy]}|T]) ->
    digraph_add_edge(G, {Va,Fx}, {Va,Fy}),
    digraph_add_edge(G, {Vb,Fx}, {Vb,Fy}),
    make_digraph_1(G, T);
make_digraph_1(G, [_|T]) ->
    make_digraph_1(G, T);
make_digraph_1(_G, []) -> ok.

digraph_add_edge(G, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    digraph:add_edge(G, Vb, Va).

vn_face_edges([#e3d_face{vs=Vs}|Fs], Face, Acc) ->
    vn_face_edges(Fs, Face+1, vn_pairs(Vs, Vs, Face, Acc));
vn_face_edges([], _Face, Acc) -> Acc.

vn_pairs([V1|[V2|_]=Vs], More, Face, Acc) ->
    vn_pairs(Vs, More, Face, [{vn_edge_name(V1, V2),Face}|Acc]);
vn_pairs([V1], [V2|_], Face, Acc) ->
    [{vn_edge_name(V1, V2),Face}|Acc].

vn_edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
vn_edge_name(Va, Vb) -> {Vb,Va}.

%%%
%%% Help functions for renumber/1.
%%%

renumber_1(#e3d_mesh{fs=Ftab0,vs=Vs0,tx=Tx0,ns=Ns0,he=He0}=Mesh,
	   UsedVs, UsedUV, UsedNs) ->
    VsMap = rn_make_map(UsedVs, 0, []),
    UVMap = rn_make_map(UsedUV, 0, []),
    NsMap = rn_make_map(UsedNs, 0, []),
    Ftab = renumber_ftab(Ftab0, VsMap, UVMap, NsMap, []),
    He = renumber_hard_edges(He0, VsMap, []),
    Vs = rn_remove_unused(Vs0, VsMap),
    Tx = rn_remove_unused(Tx0, UVMap),
    Ns = rn_remove_unused(Ns0, NsMap),
    Mesh#e3d_mesh{fs=Ftab,vs=Vs,tx=Tx,ns=Ns,he=He}.

renumber_ftab([#e3d_face{vs=Vs0,tx=Tx0,ns=Ns0}=Rec|Fs],
	      VsMap, UVMap, NsMap, Acc) ->
    Vs = [map_vtx(V, VsMap) || V <- Vs0],
    Tx = [map_vtx(V, UVMap) || V <- Tx0],
    Ns = [map_vtx(V, NsMap) || V <- Ns0],
    renumber_ftab(Fs, VsMap, UVMap, NsMap,
		  [Rec#e3d_face{vs=Vs,tx=Tx,ns=Ns}|Acc]);
renumber_ftab([], _, _, _, Acc) -> reverse(Acc).

renumber_hard_edges([{Va0,Vb0}|T], VsMap, Acc) ->
    Va = map_vtx(Va0, VsMap),
    Vb = map_vtx(Vb0, VsMap),
    if
	Va == none; Vb == none ->		%No longer an edge.
	    renumber_hard_edges(T, VsMap, Acc);
	Va < Vb ->
	    renumber_hard_edges(T, VsMap, [{Va,Vb}|Acc]);
	true ->
	    renumber_hard_edges(T, VsMap, [{Vb,Va}|Acc])
    end;
renumber_hard_edges([], _, Acc) -> reverse(Acc).

map_vtx(V0, {map,Low,N}) ->
    case V0-Low of
	V when V < N, V >= 0 -> V;
	_ -> none
    end;
map_vtx(V0, Map) ->
    case gb_trees:lookup(V0, Map) of
	{value,V} -> V;
	none -> none
    end.

rn_remove_unused(Vs, {map,Low,N}) ->
    lists:sublist(Vs, Low+1, N);
rn_remove_unused(Vs, Map) ->
    rn_remove_unused(Vs, gb_trees:to_list(Map), 0, []).

rn_remove_unused([V|Vs], [{I,_}|Map], I, Acc) ->
    rn_remove_unused(Vs, Map, I+1, [V|Acc]);
rn_remove_unused([_|Vs], Map, I, Acc) ->
    rn_remove_unused(Vs, Map, I+1, Acc);
rn_remove_unused(_, [], _, Acc) -> reverse(Acc).

rn_used_vs(#e3d_mesh{fs=Ftab,tx=TxTab,ns=Ntab}) ->
    Vs = foldl(fun(#e3d_face{vs=Vs}, A) -> Vs++A end, [], Ftab),
    UV = case TxTab of
	     [] -> [];
	     _ ->
		 UV0 = foldl(fun(#e3d_face{tx=Tx}, A) -> Tx++A end, [], Ftab),
		 ordsets:from_list(UV0)
	     end,
    Ns = case Ntab of
	     [] -> [];
	     _ ->
		 Ns0 = foldl(fun(#e3d_face{ns=Ns}, A) -> Ns++A end, [], Ftab),
		 ordsets:from_list(Ns0)
	 end,
    {ordsets:from_list(Vs),UV,Ns}.

rn_make_map([V], I, Acc0) ->
    [{Low,_}|_] = Acc = reverse(Acc0, [{V,I}]),
    High = V+1,
    case High-Low of
	Range when Range =:= length(Acc) -> {map,Low,Range};
	_Range -> gb_trees:from_orddict(Acc)
    end;
rn_make_map([V|Vs], I, Acc) ->
    rn_make_map(Vs, I+1, [{V,I}|Acc]);
rn_make_map([], _, []) -> gb_trees:empty().

%%%
%%% Help functions for partition/1.
%%%

partition_1(Faces, He0) ->
    E2FL = par_pairs(sofs:to_external(Faces), []),
    E2F0 = sofs:relation(E2FL, [{edge,face}]),
    %% Remove edges which have more than 2 faces.
    ProblematicEds = prob_eds(lists:sort(E2FL), []),
    Del = sofs:set(ProblematicEds, [edge]),
    E2F = sofs:drestriction(E2F0,Del),
    
    F0 = sofs:relation_to_family(E2F),
    CR = sofs:canonical_relation(sofs:range(F0)),
    F1 = sofs:relation_to_family(CR),
    F = sofs:family_union(F1),
    G = sofs:family_to_digraph(F),
    Cs = digraph_utils:strong_components(G),
    digraph:delete(G),

    F2E = sofs:converse(E2F),
    He = sofs:set(He0, [edge]),
    %% Find faces where all edges are bad and thus the face has been lost.
    LostFaces0 = sofs:difference(sofs:range(E2F0), sofs:range(E2F)),
    LostFaces = sofs:to_external(LostFaces0),
    foldl(fun(C, A) ->
		  Part = sofs:set(C, [face]),
		  FacePart0 = sofs:restriction(Faces, Part),
		  Es0 = sofs:image(F2E, Part),
		  Es1 = sofs:intersection(He, Es0),
		  Es = sofs:to_external(Es1),
		  FNew = sofs:to_external(FacePart0),
		  case prob_eds(lists:sort(par_pairs(FNew,[])), []) of
		      [] -> 
			  [{FNew,Es}|A];
		      Edges -> 
			  %%  io:format("Still got probs ~p ~n", [Edges]),
			  Eds = sofs:set(Edges,[edge]),
			  Bad = sofs:restriction(E2F0, Eds),
			  BadF0 = sofs:relation_to_family(Bad),
			  BadF1 = sofs:to_external(sofs:range(BadF0)),
			  %% I'm desperate 
			  %% Delete some faces that cause problems..
			  BadF2 = [BFs || [_,_|BFs] <- BadF1],
			  DelF = sofs:set(lists:append(BadF2), [face]),
			  Good = sofs:drestriction(FacePart0, DelF),
			  Other0 = sofs:restriction(FacePart0, DelF),
			  Other = [{[Face],Es} || Face <- sofs:to_external(Other0)],
			  Other ++ [{sofs:to_external(Good),Es}|A]
		  end
	  end, [], [LostFaces|Cs]).

par_pairs([{Face,#e3d_face{vs=Vs}}|Fs], Acc) ->
    par_pairs(Fs, par_pairs_1(Vs, Vs, Face, Acc));
par_pairs([], Acc) -> Acc.

par_pairs_1([V1|[V2|_]=Vs], More, Face, Acc) ->
    par_pairs_1(Vs, More, Face, [{par_edge_name(V1, V2),Face}|Acc]);
par_pairs_1([V1], [V2|_], Face, Acc) ->
    [{par_edge_name(V1, V2),Face}|Acc].

par_edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
par_edge_name(Va, Vb) -> {Vb,Va}.

prob_eds([{A,_A}|R], [A|_]=Ack) ->  % Already reported
    prob_eds(R, Ack);
prob_eds([{A,_A},{A,_B},{A,_C}|R], Ack) ->  % 3 eds not ok, save prob
    prob_eds(R, [A|Ack]);
prob_eds([{A,_},{A,_}|R], Ack) ->    % 2 eds ok
    prob_eds(R,Ack);
prob_eds([_A|R], Ack) ->      % 1 ed ok (hole or part of already reported)
    prob_eds(R,Ack);         % holes are taken care of elsewhere hopefully
prob_eds([], Ack) ->
    lists:reverse(Ack).

strip_index(Fs) ->
    strip_index(Fs, []).
strip_index([{_,Data}|T], Acc) ->
    strip_index(T, [Data|Acc]);
strip_index([], Acc) -> reverse(Acc).

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
%%% Help function for used_materials/1.

used_materials_1([#e3d_face{mat=[Mat]}|Fs], [Mat|_]=Acc) ->
    used_materials_1(Fs, Acc);
used_materials_1([#e3d_face{mat=[Mat]}|Fs], Acc) ->
    used_materials_1(Fs, [Mat|Acc]);
used_materials_1([#e3d_face{mat=[]}|Fs], Acc) ->
    used_materials_1(Fs, Acc);
used_materials_1([], Acc) -> ordsets:from_list(Acc).

%%%
%%% Help functions for slit_hard_edges/2.

-ifdef(print_mesh_1).
print_mesh(#e3d_mesh{type=T,vs=Vs,vc=Vc,tx=Tx,ns=Ns,fs=Fs,he=He,matrix=M}) ->
    io:format("#e3d_mesh{type=~p,~nvs=~p,~nvc=~p,~ntx=~p,~nns=~p,~nfs=~p,~n"
	      "he=~p,~nmatrix=~p}.~n",
	      [T,Vs,Vc,Tx,Ns,Fs,He,M]).
-endif.

%% Loop through all faces
%%
slit_hard_f(_Old, _VsGt, _HeGt, [], 
	    New=#e3d_mesh{vs={_,Vs},vc={_,Vc},tx={_,Tx},ns={_,Ns}}, NewFs) ->
    New#e3d_mesh{vs=reverse(Vs),vc=reverse(Vc),tx=reverse(Tx),ns=reverse(Ns),
		 fs=reverse(NewFs)};
slit_hard_f(Old, VcGt, HeGt0, [F0|Fs], New0, NewFs) ->
    {HeGt,OpsR} = slit_hard_q(F0, VcGt, HeGt0), % Reversed edge operations
    {New,#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns}} = slit_hard_x(Old, New0, OpsR),
    F = F0#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns},
    slit_hard_f(Old, VcGt, HeGt, Fs, New, [F|NewFs]).

%% Create a reversed edge operation list from a vertex list, vertex count 
%% and hard edge count.
%% Vertices that are part of a hard edge are marked for duplication
%% except for hard edge chain end vertices,
%% but if the chain is a solo edge (singleton chain) mark for cut.
%%
%% Possible operations are 'nop' - no operation, 'dup' - duplication
%% and 'cut' - cut at midpoint by inserting extra vertex.
%%
slit_hard_q(F, VcGt, HeGt) -> slit_hard_q_1([F], F, VcGt, HeGt, []).
%%
slit_hard_q_1([], #e3d_face{vs=[_]}, _VcGt, HeGt, R) ->
    {HeGt,R};
slit_hard_q_1([#e3d_face{vs=[Vs1|_],vc=Vc1s,tx=Tx1s,ns=Ns1s}], 
	      #e3d_face{vs=[VsN],vc=Vc,tx=Tx,ns=Ns}, VcGt, HeGt, R) ->
    slit_hard_q_1([], 
		  #e3d_face{vs=[VsN,Vs1],vc=Vc++Vc1s,tx=Tx++Tx1s,ns=Ns++Ns1s},
		  VcGt, HeGt, R);
slit_hard_q_1(F1s, #e3d_face{vs=[VsJ|Vs=[VsK|_]],vc=Vc,tx=Tx,ns=Ns},
	      VcGt, HeGt0, R) ->
    E = mk_edge(VsJ, VsK),
    Fk = #e3d_face{vs=VsK,vc=hd2(Vc),tx=hd2(Tx),ns=hd2(Ns)},
    Fs = #e3d_face{vs=Vs,vc=tail(Vc),tx=tail(Tx),ns=tail(Ns)},
    case gb_trees:lookup(E, HeGt0) of
	{value,0} -> % Never seen before -> nop, and mark as seen
	    HeGt = gb_trees:update(E, 1, HeGt0),
	    slit_hard_q_1(F1s, Fs, VcGt, HeGt, [{nop,Fk}|R]);
	{value,_} -> % 1 or above
	    case {gb_trees_count(VsJ, VcGt),gb_trees_count(VsK, VcGt)} of
		{1,1} -> % Solo edge - cut
		    slit_hard_q_1(F1s, Fs, VcGt, HeGt0, [{cut,Fk}|R]);
		{_,1} -> % Dup first
		    slit_hard_q_1(F1s, Fs, VcGt, HeGt0, [{du1,Fk}|R]);
		{1,_} -> % Dup second
		    slit_hard_q_1(F1s, Fs, VcGt, HeGt0, [{du2,Fk}|R]);
		_ -> % Dup both
		    slit_hard_q_1(F1s, Fs, VcGt, HeGt0, [{dup,Fk}|R])
	    end;
	none ->
	    slit_hard_q_1(F1s, Fs, VcGt, HeGt0, [{nop,Fk}|R])
    end.

%% Tolerant list operations
hd2([_,X|_]) -> [X];
hd2([_]) -> [];
hd2([]) -> [].

tail([_|T]) -> T;
tail([]) -> [].

cons([], L) -> L;
cons([X], L) -> [X|L].

cons([], _, L) -> L;
cons([X], Y, L) -> [X,Y|L]. % This one is weird on purpose.

%% Execute the edge operation list.
%%
slit_hard_x(Old, New, Ops=[OpN|_]) -> 
    slit_hard_x_1(Old, New, [OpN], Ops, #e3d_face{}, 0).
%%
slit_hard_x_1(Old, New=#e3d_mesh{type=triangle}, [], Ops=[_OpN], F, 3) ->
    slit_hard_x_1(Old, New#e3d_mesh{type=polygon}, [], Ops, F, 3);
slit_hard_x_1(_Old, New, [], [_OpN], 
	      F=#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns}, _C) ->
    {New#e3d_mesh{type=polygon},F#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns}};
slit_hard_x_1(Old, New, [OpN], [OpK], F, C) ->
    slit_hard_x_1(Old, New, [], [OpK,OpN], F, C);
slit_hard_x_1(Old, New0, OpNs, [OpK|Ops=[OpJ|_]], F0, C) ->
    case {OpJ,OpK} of
	{{nop,Fj},{cut,Fk}} ->
	    {New,F} = slit_hard_x_cut(Old, New0, F0, Fj, Fk),
	    slit_hard_x_1(Old, New, OpNs, Ops, F, C+2);
	{{cut,Fj},{nop,_Fk}} ->
	    slit_hard_x_1(Old, New0, OpNs, Ops, slit_hard_x_nop(F0, Fj), C+1);
	{{nop,Fj},{nop,_Fk}} ->
	    slit_hard_x_1(Old, New0, OpNs, Ops, slit_hard_x_nop(F0, Fj), C+1);
	{{du1,Fj},{du2,_Fk}} ->
	    slit_hard_x_1(Old, New0, OpNs, Ops, slit_hard_x_nop(F0, Fj), C+1);
	{{_,Fj},{_,_Fk}} -> % Duplicate Vj
	    {New,F} = slit_hard_x_dup(Old, New0, F0, Fj),
	    slit_hard_x_1(Old, New, OpNs, Ops, F, C+1)
    end.

slit_hard_x_cut(_Old=#e3d_mesh{vs=VsT,vc=VcT,tx=TxT,ns=NsT},
		New=#e3d_mesh{vs={VsN,VsL},vc={VcN,VcL},
			      tx={TxN,TxL},ns={NsN,NsL}}, 
		F=#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns},
		_Fj=#e3d_face{vs=VsJ,vc=VcJ,tx=TxJ,ns=NsJ},
		_Fk=#e3d_face{vs=VsK,vc=VcK,tx=TxK,ns=NsK}) ->
    Pos = e3d_vec:average([element(VsJ+1, VsT),element(VsK+1, VsT)]),
    Color = case {VcJ,VcK} of
		{[Vcj],[Vck]} ->
		    [mix(0.5, element(Vcj+1, VcT), element(Vck+1, VcT))];
		_ -> []
	    end,
    UV = case {TxJ,TxK} of
	     {[Txj],[Txk]} ->
		 [mix(0.5, element(Txj+1, TxT), element(Txk+1, TxT))];
	     _ -> []
	 end,
    Norm = case {NsJ,NsK} of
	       {[Nsj],[Nsk]} ->
		   [e3d_vec:average(
		      [element(Nsj+1, NsT), element(Nsk+1, NsT)])];
	       _ -> []
	   end,
    {New#e3d_mesh{vs={VsN+1,[Pos|VsL]},
		  vc={VcN+1,cons(Color, VcL)},
		  tx={TxN+1,cons(UV, TxL)},
		  ns={NsN+1,cons(Norm, NsL)}},
     F#e3d_face{vs=[VsJ,VsN|Vs],vc=cons(VcJ, VcN, Vc),
		tx=cons(TxJ, TxN, Tx),ns=cons(NsJ, NsN, Ns)}}.

slit_hard_x_nop(F=#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns},
		_Fj=#e3d_face{vs=VsJ,vc=VcJ,tx=TxJ,ns=NsJ}) ->
    F#e3d_face{vs=[VsJ|Vs],vc=cons(VcJ, Vc),
	       tx=cons(TxJ, Tx),ns=cons(NsJ, Ns)}.

slit_hard_x_dup(_Old=#e3d_mesh{vs=VsT},
		New=#e3d_mesh{vs={VsN,VsL}}, 
		F=#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns},
		_Fj=#e3d_face{vs=VsJ,vc=VcJ,tx=TxJ,ns=NsJ}) ->
    Pos = element(VsJ+1, VsT),
    {New#e3d_mesh{vs={VsN+1,[Pos|VsL]}},
     F#e3d_face{vs=[VsN|Vs],vc=cons(VcJ, Vc),
		tx=cons(TxJ, Tx),ns=cons(NsJ, Ns)}}.

mk_edge(V1, V2) when V1 > V2 -> {V2,V1};
mk_edge(V1, V2) -> {V1,V2}.

mix(_W, Same, Same) -> Same;
mix(Wa, {Ua,Va}, {Ub,Vb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    {Wa*Ua+Wb*Ub,Wa*Va+Wb*Vb};
mix(Wa, {Ra,Ga,Ba}, {Rb,Gb,Bb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    {Wa*Ra+Wb*Rb,Wa*Ga+Wb*Gb,Wa*Ba+Wb*Bb};
mix(Wa, {Ra,Ga,Ba}, {_,_,_,Ab}=B) ->
    mix(Wa, {Ra,Ga,Ba,Ab}, B);
mix(Wa, {_,_,_,Aa}=A, {Rb,Gb,Bb}) ->
    mix(Wa, A, {Rb,Gb,Bb,Aa});
mix(Wa, {Ra,Ga,Ba,Aa}, {Rb,Gb,Bb,Ab}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    {Wa*Ra+Wb*Rb,Wa*Ga+Wb*Gb,Wa*Ba+Wb*Bb,Wa*Aa+Wb*Ab};
mix(_, _, _) -> none.

%%%
%%% Help function for face_areas/1,2.

face_areas_1([], _Vs, _VsT) -> [];
face_areas_1([#e3d_face{vs=[V1,V2,V3]}|T], Vs, VsT) ->
    P1 = element(V1+1, VsT),
    P2 = element(V2+1, VsT),
    P3 = element(V3+1, VsT),
    V21 = e3d_vec:sub(P1, P2),
    V23 = e3d_vec:sub(P3, P2),
    A = e3d_vec:len(e3d_vec:cross(V21, V23)) / 2, 
    [A | face_areas_1(T, Vs, VsT)];
face_areas_1([#e3d_face{}=F|T], Vs, VsT) ->
    Fs = triangulate_face(F, Vs),
    [foldl(fun (A, Acc) -> A + Acc end, 0, face_areas_1(Fs, Vs, VsT))
     |face_areas_1(T, Vs, VsT)].
		  
%%%
%%% Common help functions.
%%%

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([F|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,F}|Acc]);
number_faces([], _Face, Acc) -> reverse(Acc).

gb_trees_increment(Key, Inc, Gt) ->
    case gb_trees:lookup(Key, Gt) of
	{value,V} -> gb_trees:update(Key, V+Inc, Gt);
	none -> gb_trees:insert(Key, Inc, Gt)
    end.

gb_trees_count(Key, Gt) ->
    case gb_trees:lookup(Key, Gt) of
	{value,V} -> V;
	none -> 0
    end.
