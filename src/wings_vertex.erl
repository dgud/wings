%%
%%  wings_vertex.erl --
%%
%%     This module contains utility functions for vertices.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_vertex).
-export([from_edges/2,from_faces/2,
	 fold/4,other/2,other_pos/3,
	 until/4,until/5,
	 center/1,center/2,
	 bounding_box/1,bounding_box/2,bounding_box/3,
	 normal/2,per_face/2,
	 flatten/3,flatten/4,
	 dissolve_isolated/2,
	 connect/3,connect_cut/3,force_connect/4,
	 pos/2,outer_vertices_ccw/2,reachable/2,
	 isolated/1,edge_through/3,edge_through/4]).

-export_type([vertex_num/0]).

-include("wings.hrl").
-import(lists, [member/2,foldl/3,reverse/1,sort/1]).

-type vertex_num() :: non_neg_integer().
-type edge_num() :: wings_edge:edge_num().

%% from_faces(FaceGbSet, We) -> VertexList
%%  Convert a set of faces to a list of vertices.
from_faces(Fs, We) ->
    wings_face:to_vertices(Fs, We).

%% to_vertices(EdgeGbSet, We) -> VertexGbSet
%%  Convert a set of edges to a set of vertices.
from_edges(Es, We) ->
    wings_edge:to_vertices(Es, We).

%%
%% Fold over all edges/faces surrounding a vertex.
%%

fold(F, Acc, V, #we{vc=Vct}=We) ->
    Edge = array:get(V, Vct),
    fold(F, Acc, V, Edge, We).

fold(F, Acc0, V, Edge, #we{es=Etab}) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    fold(F, Acc, V, Other, NextEdge, Edge, Etab).

fold(_F, Acc, _V, _Face, Last, Last, _Etab) -> Acc;
fold(F, Acc0, V, Face, Edge, LastEdge, Etab) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{vs=V,rf=Face,lf=Other,ltsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,rf=Face,lf=Other,ltpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    fold(F, Acc, V, Other, NextEdge, LastEdge, Etab).

%%
%% Fold over all edges/faces surrounding a vertex until the
%% accumulator changes.
%%

until(F, Acc, V, #we{vc=Vct}=We) ->
    Edge = array:get(V, Vct),
    until(F, Acc, V, Edge, We).

until(F, Acc, V, Edge, #we{es=Etab}) ->
    #edge{lf=Face} = array:get(Edge, Etab),
    until(F, Acc, V, Face, Edge, Edge, Etab, not_done).

until(_F, Acc, _V, _Face, Last, Last, _Etab, done) -> Acc;
until(F, Acc0, V, Face, Edge, LastEdge, Etab, _) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{vs=V,rf=Face,lf=Other,ltsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,rf=Face,lf=Other,ltpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    if
	Acc =:= Acc0 ->
	    until(F, Acc, V, Other, NextEdge, LastEdge, Etab, done);
	true -> Acc
    end.

%% other(Vertex, EdgeRecord) -> OtherVertex
%%  Pick up the "other vertex" from an edge record.
other(V, #edge{vs=V,ve=Other}) -> Other;
other(V, #edge{ve=V,vs=Other}) -> Other.

%% pos(Vertex, VtabOrWe) -> {X,Y,Z}
%%  Return the three co-ordinates for a vertex.
pos(V, #we{vp=Vtab}) ->
    array:get(V, Vtab);
pos(V, Vtab) ->
    array:get(V, Vtab).

%% other_pos(Vertex, EdgeRecord, VtabOrWe) -> {X,Y,Z}
%%  Pick up the position for the "other vertex" from an edge record.
other_pos(V, #edge{vs=V,ve=Other}, Tab) -> pos(Other, Tab);
other_pos(V, #edge{ve=V,vs=Other}, Tab) -> pos(Other, Tab).

%% center(We) -> {CenterX,CenterY,CenterZ}
%%  Find the geometric center of a body.
center(#we{vp=Vtab}=We) ->
    Center = e3d_vec:average(array:sparse_to_list(Vtab)),
    Flatten = wings_we:mirror_projection(We),
    e3d_mat:mul_point(Flatten, Center).

%% center(VertexGbSet, We) -> {CenterX,CenterY,CenterZ}
%%  Find the geometric center of all vertices.
center(Vs0, #we{vp=Vtab}) ->
    Vs = if
	     is_list(Vs0) -> Vs0;
	     true -> gb_sets:to_list(Vs0)
	 end,
    center(Vs, Vtab);
center(Vlist, Vtab) ->
    Positions = foldl(fun(V, A) ->
			      [pos(V, Vtab)|A]
		      end, [], Vlist),
    e3d_vec:average(Positions).

bounding_box(We) ->
    bounding_box(We, none).

bounding_box(#we{vp=Vtab}=We, BB) ->
    do_bounding_box(array:sparse_to_list(Vtab), We, BB);
bounding_box(Vs, We) ->
    bounding_box(Vs, We, none).

bounding_box(Vs, We, BB) when is_list(Vs) ->
    bounding_box_1(ordsets:from_list(Vs), We, BB);
bounding_box(Vs, We, BB) ->
    bounding_box(gb_sets:to_list(Vs), We, BB).

bounding_box_1(Vs0, #we{vp=Vtab}=We, BB) ->
    Vs1 = sofs:from_external(Vs0, [vertex]),
    R = sofs:from_external(array:sparse_to_orddict(Vtab), [{vertex,data}]),
    I = sofs:image(R, Vs1),
    Vs = sofs:to_external(I),
    do_bounding_box(Vs, We, BB).

do_bounding_box(Vs, #we{mirror=none}, BB) ->
    do_bounding_box_1(Vs, BB);
do_bounding_box(Vs0, #we{}=We, BB) ->
    Mtx = wings_we:mirror_projection(We),
    Vs = foldl(fun(P0, A) ->
		       P = e3d_mat:mul_point(Mtx, P0),
		       [P,P0|A]
	       end, [], Vs0),
    do_bounding_box_1(Vs, BB).

do_bounding_box_1(Vs, none) ->
    e3d_vec:bounding_box(Vs);
do_bounding_box_1(Vs, [Min,Max]) ->
    e3d_vec:bounding_box([Min,Max|Vs]).

%% normal(Vertex, We) -> Normal
%%  Calculate the normal for a vertex (based on the normals for all
%%  surrounding faces).
normal(V, We) ->
    Ns = fold(fun(_, Face, _, A) ->
		      [wings_face:normal(Face, We)|A]
	      end, [], V, We),
    e3d_vec:norm(e3d_vec:add(Ns)).

%% per_face(Vs, We) -> [{Face,[V]}]
%%  Group vertices according to face.
per_face(Vs, We) when is_list(Vs) ->
    per_face(Vs, We, []);
per_face(Vs, We) ->
    per_face(gb_sets:to_list(Vs), We, []).

per_face([V|Vs], We, Acc0) ->
    Acc = fold(fun(_, Face, _, A) ->
		       [{Face,V}|A]
	       end, Acc0, V, We),
    per_face(Vs, We, Acc);
per_face([], _We, Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    sofs:to_external(F).

%% flatten(Vs, PlaneNormal, We) -> We'
%%  Flatten vertices by projecting them to the given plane.
flatten(Vs, PlaneNormal, We) when is_list(Vs) ->
    Center = wings_vertex:center(Vs, We),
    flatten(Vs, PlaneNormal, Center, We);
flatten(Vs, PlaneNormal, We) ->
    flatten(gb_sets:to_list(Vs), PlaneNormal, We).

flatten(Vs, PlaneNormal, Center, #we{vp=Vtab0}=We0) when is_list(Vs) ->
    Flatten = flatten_matrix(Center, PlaneNormal),
    Vtab = foldl(fun(V, Tab0) ->
			 flatten_move(V, Flatten, Tab0)
		 end, Vtab0, Vs),
    We = We0#we{vp=Vtab},
    wings_we:mirror_flatten(We0, We);
flatten(Vs, PlaneNormal, Center, We) ->
    flatten(gb_sets:to_list(Vs), PlaneNormal, Center, We).

flatten_matrix(Origin, PlaneNormal) ->
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))).

flatten_move(V, Matrix, Vtab0) ->
    Pos0 = array:get(V, Vtab0),
    Pos = e3d_mat:mul_point(Matrix, Pos0),
    array:set(V, Pos, Vtab0).

%% dissolve_isolated_vs([Vertex], We) -> We'
%%  Remove all isolated vertices ("winged vertices", or vertices
%%  having exactly two edges).
dissolve_isolated(Vs, We) ->
    wings_edge:dissolve_isolated_vs(Vs, We).

%% Connect vertices (which must share a face).
connect(_Face, [_], We) -> We;
connect(Face, Vs, #we{} = We0) ->
    case polygon_pairs(Face, Vs, We0) of
	no -> min_distance_pairs(Face, Vs, We0);
	#we{} = We -> We
    end.

%% Connect Va to Vb, maybe trough several faces.
%% Return {#we{},gbset(Edges).

-spec connect_cut(vertex_num(), vertex_num(), #we{}) ->
                         {#we{},gb_sets:set(edge_num())}.

connect_cut(VS0, VE0, #we{}=We0) when is_integer(VS0), is_integer(VE0) ->
    CutPlane = calc_planes(VS0,VE0,We0),
    ETree = collect_edges(CutPlane, ordsets:from_list([VS0, VE0]), We0),
    try
	CutEs = select_cut_edges(VS0, VE0, ETree, We0),
	Cut = fun({edge, Ei, Pos, Face}, {WeAcc,AccVs}) ->
		      {We1,Idx} = wings_edge:fast_cut(Ei, Pos, WeAcc),
		      {We1, [Idx,Face|AccVs]};
		 ({vertex,_,V,Face}, {WeAcc, AccVs}) ->
		      {WeAcc, [V,Face|AccVs]}
	      end,
	{We2,Vs1} = lists:foldl(Cut, {We0, []}, CutEs),
	connect_vs(Vs1, We2, [])
    catch _:_Reason ->
	    %% io:format("~p: ~p~n", [Reason, erlang:get_stacktrace()]),
	    wings_u:error_msg(?__(1, "Could not connect vertices."))
    end.

connect_vs([Va, Face|[Vb|_]=Vs], #we{next_id=Edge}=We0, Acc) ->
    case edge_through(Va, Vb, Face, We0) of
	none ->
	    {We,_} = force_connect(Va, Vb, Face, We0),
	    connect_vs(Vs, We, [Edge|Acc]);
	Exist ->
	    connect_vs(Vs, We0, [Exist|Acc])
    end;
connect_vs([_, undefined], We, Acc) ->
    {We,gb_sets:from_list(Acc)}.

path_length([V1|[V2|_]=Vs], We, Acc) ->
    Dist = e3d_vec:dist(v(V1, We),v(V2, We)),
    path_length(Vs, We, Dist + Acc);
path_length([_], _, Acc) -> Acc.

v({edge, _, Pos, _}, _We) -> Pos;
v({vertex,_, V, _}, We) ->    pos(V, We).

select_cut_edges(VS0, VE0, Es, We) ->
    FS0 = wings_face:from_vs([VS0], We),
    Start00 = wings_face:fold_faces(
		fun(Face, V, _Edge, _E, _Acc) when V =:= VE0 ->
			[{stop, Face}];
		   (Face, _V, EdgeId, E, Acc) ->
			case collect_cut_edge(Face, Es, EdgeId, E, Acc) of
			    Acc -> Acc;
			    [{vertex, _, V,_},{vertex,_,V,_}|_] -> Acc;
			    [S1|Acc] -> [S1,Face|Acc]
			end
		end, [], FS0, We),
    select_start_edges(filter_edges(Start00, []), VS0, VE0, Es, We).

filter_edges([_,Face|[_,Face|_]=Start], Acc) ->
    filter_edges(Start, Acc);
filter_edges([EI,Face|Start], Acc) ->
    filter_edges(Start, [Face,EI|Acc]);
filter_edges([{stop,Face}|_], _) -> [{stop,Face}];
filter_edges([], Acc) -> Acc.

select_start_edges([{stop,Face}], VS0, VE0, _Es, _We) ->
    [{vertex, undefined, VE0, undefined}, {vertex, undefined, VS0, Face}];
select_start_edges([F1, S1], VS0, VE0, Es, We) when is_integer(F1) ->
    case select_cut_edges_2(S1, VE0, Es, We, [{vertex,undefined,VS0, F1}]) of
	fail -> throw(fail);
	R1   -> [VE0|R1]
    end;
select_start_edges([F1, S1, F2, S2], VS0, VE0, Es, We) ->
    R1 = select_cut_edges_2(S1, VE0, Es, We, [{vertex,undefined,VS0,F1}]),
    R2 = select_cut_edges_2(S2, VE0, Es, We, [{vertex,undefined,VS0,F2}]),
    case {R1, R2} of
	{fail, fail} -> throw(fail);
	{fail, R2} -> [{vertex,undefined,VE0,undefined}|R2];
	{R1, fail} -> [{vertex,undefined,VE0,undefined}|R1];
	_ ->
	    Path1 = [{vertex,undefined,VE0,undefined}|R2],
	    Path2 = [{vertex,undefined,VE0,undefined}|R1],
	    case path_length(Path1, We, 0.0) < path_length(Path2, We, 0.0) of
		true -> Path1;
		false -> Path2
	    end
    end.

select_cut_edges_2({stop,_Face}, _, _, _, CEs) ->
    CEs;
select_cut_edges_2({Type, Edge, VId, Face}=New, Stop, Es0, We, CEs) ->
    Es = gb_trees:delete(Edge,Es0),
    Next = wings_face:fold(fun(V, _EdgeId, _E, _Acc) when V =:= Stop ->
				   [{stop,Face}];
			      (_V, _EdgeId, _E, [{stop,_}]=Acc) -> Acc;
			      (_V, EdgeId, E, Acc) ->
				   collect_cut_edge(Face, Es, EdgeId, E, Acc)
			   end, [fail], Face, We),
    case {Type, CEs} of
	{vertex, [{vertex, _, VId, _}|Acc]} -> %% Ignore alread connect VId
	    select_cut_edges_2(hd(Next), Stop, Es, We, [New|Acc]);
	_ ->
	    select_cut_edges_2(hd(Next), Stop, Es, We, [New|CEs])
    end;
select_cut_edges_2(fail, _, _, _, _) -> fail.

collect_cut_edge(_Face, _Es, _Edge, _E, [stop]=R) -> R;
collect_cut_edge(Face, Es, Edge, E, Acc) ->
    case gb_trees:lookup(Edge, Es) of
	none -> Acc;
	{value,{reuse_vertex, V}} ->
	    Next = wings_face:other(Face, E),
	    [{vertex, Edge, V, Next}|Acc];
	{value,Dist} ->
	    Next = wings_face:other(Face, E),
	    [{edge, Edge, Dist, Next}|Acc]
    end.

%% Collect all edges on the cut plane and calc edge cut distance
collect_edges(Plane, Ignore, #we{es=Etab}=We) ->
    Tol = 0.0001,
    Filter =
	fun(Ei, _Val, Acc) ->
		#edge{vs=VS,ve=VE} = array:get(Ei,Etab),
		Pt1 = wings_vertex:pos(VS,We),
		Pt2 = wings_vertex:pos(VE,We),
		S1 = e3d_vec:plane_side(Pt1, Plane),
		S2 = e3d_vec:plane_side(Pt2, Plane),
		D1 = abs(e3d_vec:plane_dist(Pt1, Plane)),
		D2 = abs(e3d_vec:plane_dist(Pt2, Plane)),
		if
		    D1 < Tol ->
			Add = D2 >= Tol andalso
			    ordsets:is_disjoint(ordsets:from_list([VS,VE]),Ignore),
			reuse_vertex(Add, VS, Ei, Acc);
		    D2 < Tol ->
			Add = D1 >= Tol andalso
			    ordsets:is_disjoint(ordsets:from_list([VS,VE]),Ignore),
			reuse_vertex(Add, VE, Ei, Acc);
		    S1 =/= S2 ->
			Dir = e3d_vec:norm(e3d_vec:sub(Pt2,Pt1)),
			Percent = D1/(D1+D2),
			PtX = e3d_vec:add(Pt1, e3d_vec:mul(Dir, Percent*wings_edge:length(Ei,We))),
			gb_trees:enter(Ei,PtX,Acc);
		    true ->
			Acc
		end
	end,
    array:sparse_foldl(Filter, gb_trees:empty(), Etab).

reuse_vertex(false, _, _Ei, Acc) ->
    Acc;
reuse_vertex(true, VS, Ei, Acc) ->
    gb_trees:enter(Ei,{reuse_vertex, VS}, Acc).

calc_planes(VS0, VE0, We) ->
    P1  = wings_vertex:pos(VS0,We),
    P2  = wings_vertex:pos(VE0,We),
    Vec = e3d_vec:norm(e3d_vec:sub(P2, P1)),
    Mid = e3d_vec:average(P1, P2),
    N1 = wings_vertex:normal(VS0,We),
    N2 = wings_vertex:normal(VE0, We),
    AverN = e3d_vec:average(N1, N2),
    Wanted = [N || N <- [AverN, N1, N2],
		   e3d_vec:len(N) > 0.0001,
		   abs(e3d_vec:dot(Vec, N)) < 0.9999],
    Normal = case Wanted of
		 [N|_] -> N;
		 [] ->
		     {X,Y,Z} = Vec,
		     if abs(X) < abs(Z), abs(Y) < abs(Z) -> {1.0, 0.0, 0.0};
			abs(Z) < abs(X), abs(Y) < abs(X) -> {0.0, 0.0, 1.0};
			true -> {1.0, 0.0, 0.0}
		     end
	     end,
    e3d_vec:plane(Mid, e3d_vec:cross(Vec,Normal)).


%% Create pairs by walking the edge of the face. If we can connect
%% all selected vertices for the face we are done. The result will
%% be a polygon.
%%
%% +----*----+
%% |   / \   |
%% |  /	  \  |	     * = Selected vertices
%% | /     \ |	     + = Unselected vertices
%% |/  	    \|
%% *         *
%% |\  	    /|
%% | \ 	   / |
%% |  \	  /  |
%% |   \ /   |
%% +----*----+

polygon_pairs(Face, Vs, We0) ->
    ?ASSERT(length(Vs) > 1),
    Iter = wings_face:iterator(Face, We0),
    {Vstart,_,_,_} = wings_face:next_cw(Iter),
    case pp_make_pairs(Iter, Vs, Vstart, []) of
	[Va,Vb] ->
	    case try_connect({Va,Vb}, Face, We0) of
		no -> We0;
		{We,_} -> We
	    end;
	Pairs -> polygon_pairs_1(Pairs, Face, Pairs, We0)
    end.

polygon_pairs_1([Va|[Vb|_]=T], Face, Pairs, We0) ->
    case try_connect({Va,Vb}, Face, We0) of
	no -> no;
	{We,_} -> polygon_pairs_1(T, Face, Pairs, We)
    end;
polygon_pairs_1([Va], Face, [Vb|_], We) ->
    polygon_pairs_1([Va,Vb], Face, [], We);
polygon_pairs_1([_], _, [], We) -> We.

pp_make_pairs(Iter0, Vs, Vstart, Acc) ->
    case wings_face:next_cw(Iter0) of
	{Vstart,_,_,_} when Acc =/= [] ->
	    reverse(Acc);
	{V,_,_,Iter} ->
	    case member(V, Vs) of
		false -> pp_make_pairs(Iter, Vs, Vstart, Acc);
		true -> pp_make_pairs(Iter, Vs, Vstart, [V|Acc])
	    end
    end.

%% If polygon_pairs/3 failed, we search for the two
%% vertices which are nearest each other. We try to connect,
%% then repeat the search in both the original face and the
%% the newly created face. We continue until no more connections
%% are possible. Two vertices that have been connected cannot be
%% connected again (in the same face).
%%
%% +-----+
%% |	 |   * = Selected vertices
%% *-----*   + = Non-selected vertices
%% |	 |
%% |	 |
%% *-----*
%% |	 |
%% |	 |
%% *-----*
%% |	 |
%% +-----+
%%

min_distance_pairs(Face, Vs, We) ->
    min_distance_pairs_1(gb_sets:singleton(Face), ordsets:from_list(Vs), We).

min_distance_pairs_1(Faces0, Vs0, We0) ->
    case gb_sets:is_empty(Faces0) of
	true -> We0;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    case nearest_pair_smart(Face, Vs0, We0) of  % dgud
		no ->
		    case nearest_pair(Face, Vs0, We0) of
			no ->
			    min_distance_pairs_1(Faces1, Vs0, We0);
			{{Va,Vb},{We,NewFace}} ->
			    Faces = gb_sets:insert(NewFace, Faces0),
			    Vs = ordsets:subtract(Vs0, ordsets:from_list([Va,Vb])),
			    min_distance_pairs_1(Faces, Vs, We)
		    end;
		{{Va,Vb},{We,NewFace}} ->
		    Faces = gb_sets:insert(NewFace, Faces0),
		    Vs = ordsets:subtract(Vs0, ordsets:from_list([Va,Vb])),
		    min_distance_pairs_1(Faces, Vs, We)
	    end
    end.

%% Don't go for position distance - use the topological distance instead.
%% Hopefully fixes this problem:
%%     A  B  C
%%  +__*_ *__*_+
%%   \       |  \
%%    \      |   \
%%     \      |   \
%%      \     |    \
%%       \    |     \
%%        \   |      \
%%         +--*--*--*-+
%%            D  E  F
%%  What we do is to try connecting each selected vertex to the
%%  next selected vertex near it. For instance, we might try the following
%%  connections: A-B, B-C, C-D... The first two connections will be
%%  discarded because the resulting new faces do not have defined
%%  normals. The connection C-D will succeed.

nearest_pair_smart(Face, AllVs, We) ->
    FaceVs = wings_face:vertices_ccw(Face, We),
    Vs0    = ordsets:from_list(FaceVs),
    Vs     = ordsets:intersection(Vs0, AllVs),
    nearest_pair_smart_1(FaceVs, Vs, Face, We, []).

%% If we knew that the intersection was stable this step wouldn't be needed.
nearest_pair_smart_1([V|Vs], Sel, Face, We, Acc) ->
    case ordsets:is_element(V, Sel) of
	true ->
	    nearest_pair_smart_1(Vs, Sel, Face, We, [V|Acc]);
	false ->
	    nearest_pair_smart_1(Vs, Sel, Face, We, Acc)
    end;
nearest_pair_smart_1([], _, Face, We, Acc=[Last|_]) when length(Acc) > 1 ->
    connect_pairs([Last|reverse(Acc)],Face,We);
nearest_pair_smart_1([], _, _, _, _) ->
    no.

nearest_pair(Face, AllVs, #we{vp=Vtab}=We) ->
    Vs0 = ordsets:from_list(wings_face:vertices_ccw(Face, We)),
    Vs = ordsets:intersection(Vs0, AllVs),
    VsPos = [{V,array:get(V, Vtab)} || V <- Vs],
    nearest_pair(VsPos, Face, We, []).

nearest_pair([{V,Pos}|VsPos], Face, We, Acc0) ->
    Acc = nearest_pair_1(VsPos, V, Pos, Acc0),
    nearest_pair(VsPos, Face, We, Acc);
nearest_pair([], Face, We, Acc) ->
    connect_pairs(sort(Acc), Face, We).

nearest_pair_1([{Vb,PosB}|VsPos], Va, PosA, Acc) ->
    Dist = e3d_vec:dist(PosA, PosB),
    nearest_pair_1(VsPos, Va, PosA, [{Dist,{Va,Vb}}|Acc]);
nearest_pair_1([], _, _, Acc) -> Acc.

connect_pairs([{_,Pair}|Pairs], Face, We0) ->
    case try_connect(Pair, Face, We0) of
	no -> connect_pairs(Pairs, Face, We0);
	{_,_}=Res -> {Pair,Res}
    end;
%% <dgud
connect_pairs([Va,Vb|Pairs], Face, We0) ->
    Pair = {Va,Vb},
    case try_connect(Pair, Face, We0) of
	no -> connect_pairs([Vb|Pairs], Face, We0);
	{_,_}=Res -> {Pair,Res}
    end;
connect_pairs([_], _, _) -> no;
%% dgud>
connect_pairs([], _, _) -> no.

try_connect({Va,Vb}, Face, We) ->
    %% Do not try to connect if there is an edge from Va to Vb in this face.
    case edge_through(Va, Vb, Face, We) of
	none -> try_connect_1(Va, Vb, Face, We);
	_ -> no
    end.

try_connect_1(Va, Vb, Face, We0) ->
    {We,NewFace} = Res = force_connect(Va, Vb, Face, We0),

    %% It is crucial that we reject long thin faces, such as
    %%
    %%   A
    %%   |
    %%   |
    %%   C
    %%   |
    %%   |
    %%   B
    %%
    %% (where the edges are A-C, C-B, and B-A), since the
    %% Connect command for vertices will try many combinations
    %% of pair of vertices if the user has selected more than two
    %% vertices.
    case wings_face:is_thin(Face, We) orelse
	wings_face:is_thin(NewFace, We) of
	true -> no;
	false -> Res
    end.

%% force_connect(Vstart, Vend, Face, We0) -> We
%%  Create a new edge between the vertices Vstart and Vend
%%  in face Face, also creating a new face.
%%
%%  The edges between Vstart and Vend (in the clockwise direction)
%%  and the left side of the new edge will belong to the new face.
%%
force_connect(Vstart, Vend, Face, #we{es=Etab0,fs=Ftab0}=We0) ->
    All = wings_face:vertices_ccw(Face, We0),
    true = lists:member(Vstart, All) andalso lists:member(Vend, All),
    {NewFace,We1} = wings_we:new_ids(1, We0),
    NewEdge = NewFace,
    NeRec0 = #edge{vs=Vstart,ve=Vend,lf=NewFace,rf=Face},

    Iter0 = wings_face:iterator(Face, We1),
    Iter1 = wings_face:skip_to_cw(Vstart, Iter0),
    {_,_,_,Iter2} = wings_face:next_ccw(Iter1),
    {Etab1,NeRec1,Iter3} = connect_1(Iter2, Vstart, NewEdge, NeRec0, Etab0),
    {Etab2,NeRec2} = connect_2(Iter3, Vstart, NewEdge, NeRec1, Etab1),
    {Etab3,Iter} = connect_3(Iter3, Face, Vend, NewFace, Etab2),
    Etab = connect_4(Iter, Vend, NewEdge, NeRec2, Etab3),

    Ftab1 = gb_trees:insert(NewFace, NewEdge, Ftab0),
    Ftab = gb_trees:update(Face, NewEdge, Ftab1),
    Mat = wings_facemat:face(Face, We1),
    We2 = We1#we{es=Etab,fs=Ftab},

    LeftAttr = wings_va:vtx_attrs(Vstart, Face, We0),
    RightAttr = wings_va:vtx_attrs(Vend, Face, We0),
    We = wings_va:set_both_edge_attrs(NewEdge, LeftAttr, RightAttr, We2),

    {wings_facemat:assign(Mat, [NewFace], We),NewFace}.

%% connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab0) -> {Etab,NeRec,Iter}
%%  Connect the edge immediately before Vstart.
connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab) ->
    case wings_face:next_cw(Iter0) of
	{_,Edge,#edge{ve=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{rtpr=Edge},
	    Rec = Rec0#edge{rtsu=NewEdge};
	{_,Edge,#edge{vs=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{rtpr=Edge},
	    Rec = Rec0#edge{ltsu=NewEdge}
    end,
    {array:set(Edge, Rec, Etab),NeRec,Iter}.

%% connect_2(Iter0, Vstart, NewEdge, NeRec0, Etab) -> {Etab,NeRec}
%%  Connect the edge immediately after Vstart.
connect_2(Iter, Vstart, NewEdge, NeRec0, Etab) ->
    case wings_face:next_cw(Iter) of
	{_,Edge,#edge{vs=Vstart}=Rec0,_} ->
	    NeRec = NeRec0#edge{ltsu=Edge},
	    Rec = Rec0#edge{rtpr=NewEdge};
	{_,Edge,#edge{ve=Vstart}=Rec0,_} ->
	    NeRec = NeRec0#edge{ltsu=Edge},
	    Rec = Rec0#edge{ltpr=NewEdge}
    end,
    {array:set(Edge, Rec, Etab),NeRec}.

%% connect_3(Iter, Face, Vend, NewFace, Etab0) -> {Etab,Iter}
%%  Replace the face for all edges between Vstart and Vend.
%%  The returned iterator points to the edge immediately before Vend.
connect_3(Iter0, Face, Vend, NewFace, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),

    %% Ignore the record returned by the iterator, because it
    %% is stale for the edge that was updated by connect_2/5.
    Rec = case array:get(Edge, Etab0) of
	      #edge{lf=Face}=Rec0 -> Rec0#edge{lf=NewFace};
	      #edge{rf=Face}=Rec0 -> Rec0#edge{rf=NewFace}
	  end,
    Etab = array:set(Edge, Rec, Etab0),
    case Rec of
	#edge{vs=Vend} -> {Etab,Iter0};
	#edge{ve=Vend} -> {Etab,Iter0};
	_Other -> connect_3(Iter, Face, Vend, NewFace, Etab)
    end.

%% connect_4(Iter, Vend, NewEdge, NeRec, Etab0) -> Etab
%%  Patches the final two edges.
connect_4(Iter0, Vend, NewEdge, NeRec0, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),
    Rec = case array:get(Edge, Etab0) of
	      #edge{ve=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{ltpr=Edge},
		  Rec0#edge{rtsu=NewEdge};
	      #edge{vs=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{ltpr=Edge},
		  Rec0#edge{ltsu=NewEdge}
	  end,
    Etab1 = array:set(Edge, Rec, Etab0),

    %% Now for the final edge.
    FinalRec = case wings_face:next_cw(Iter) of
		   {_,Final,#edge{vs=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{rtpr=NewEdge};
		   {_,Final,#edge{ve=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{ltpr=NewEdge}
	       end,
    Etab = array:set(Final, FinalRec, Etab1),
    array:set(NewEdge, NeRec, Etab).

%% outer_vertices_ccw(Faces, We) -> [V] | error
%%  Faces (non-empty list or gb_set) must comprise a single face region
%%  (each face must share at least one edge with another face in the
%%  region). This functions returns a list of the outer vertices,
%%  ordered in CCW order. (Useful for calculating the normal for
%%  an edge loop, for instance.) The return value is 'error' if
%%  the faces don't comprise a single region.

outer_vertices_ccw(Faces, We) when is_list(Faces) ->
    collect_outer_edges(Faces, gb_sets:from_list(Faces), We, []);
outer_vertices_ccw(Faces, We) ->
    collect_outer_edges(gb_sets:to_list(Faces), Faces, We, []).

collect_outer_edges([Face|Fs], Faces, We, Acc0) ->
    Acc = wings_face:fold(
	    fun(_, _, Erec, A) ->
		    outer_edge(Erec, Face, Faces, A)
	    end, Acc0, Face, We),
    collect_outer_edges(Fs, Faces, We, Acc);

collect_outer_edges([], _Faces, _We, []) ->
    error;

collect_outer_edges([], _Faces, _We, Acc) ->
    R = sofs:relation(Acc),
    F0 = sofs:relation_to_family(R),
    [{Va,Info}|F] = sofs:to_external(F0),
    order_edges(Va, Info, gb_trees:from_orddict(F), []).

outer_edge(Erec, Face, Faces, Acc) ->
    {V,OtherV,OtherFace} =
	case Erec of
	    #edge{vs=Vs,ve=Ve,lf=Face,rf=Other0} ->
		{Vs,Ve,Other0};
	    #edge{vs=Vs,ve=Ve,rf=Face,lf=Other0} ->
		{Ve,Vs,Other0}
	end,
    case gb_sets:is_member(OtherFace, Faces) of
	true -> Acc;				%Not an outer edge.
	false -> [{V,{V,OtherV}}|Acc]
    end.

order_edges(Va, [{Va,Vb}], Es0, Acc0) ->
    Acc = [Va|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none ->
	    %% We have collected all outer vertices for one
	    %% face region. We are done unless more edges remain
	    %% (which is an error).
	    case gb_trees:is_empty(Es0) of
		true -> Acc;
		false -> error
	    end;
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    order_edges(Vb, Val, Es, Acc)
    end;
order_edges(_, [_,_|_], _, _) ->
    %% Two face regions are connected by a single vertex.
    error.

%% reachable([Vertex], We) -> [ReachableVertex]
%%  Returns a list of the vertices that can be reached by following
%%  edges from the given list of vertices.
reachable(Vs0, #we{es=Etab,vc=Vct}) when is_list(Vs0) ->
    Es0 = foldl(fun(V, A) ->
			[array:get(V, Vct)|A]
		end, [], Vs0),
    Es1 = gb_sets:from_list(Es0),
    Es = reachable_edges(Es1, Etab, gb_trees:empty()),
    Vs = foldl(fun(#edge{vs=Va,ve=Vb}, A) ->
		       [Va,Vb|A]
	       end, [], gb_trees:values(Es)),
    ordsets:from_list(Vs).

reachable_edges(Ws0, Etab, Reachable0) ->
    case gb_sets:is_empty(Ws0) of
	true -> Reachable0;
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    Rec = array:get(Edge, Etab),
	    Reachable = gb_trees:insert(Edge, Rec, Reachable0),
	    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
	    reachable_edges_1([LP,LS,RP,RS], Etab, Ws1, Reachable)
    end.

reachable_edges_1([E|Es], Etab, Ws, Reachable) ->
    case gb_trees:is_defined(E, Reachable) of
	true ->
	    reachable_edges_1(Es, Etab, Ws, Reachable);
	false ->
	    reachable_edges_1(Es, Etab, gb_sets:add(E, Ws), Reachable)
    end;
reachable_edges_1([], Etab, Ws, Reachable) ->
    reachable_edges(Ws, Etab, Reachable).

%% isolated(We) -> GbSet
%%  Returns a list containing all isolated vertices in We.

isolated(#we{vp=Vtab}=We) ->
    Vs0 = foldl(fun(V, A) ->
			isolated_1(V, We, A)
		end, [], wings_util:array_keys(Vtab)),
    Vs1 = sofs:relation(Vs0),
    Fs0 = sofs:domain(Vs1),
    Fs = sofs:to_external(Fs0),
    StableFaces = sofs:set(stable_faces(Fs, We)),
    Vs = sofs:image(Vs1, StableFaces),
    sofs:to_external(Vs).

isolated_1(V, We, Acc) ->
    Fs = fold(fun(_, Face, _, A) ->
		      [Face|A]
	      end, [], V, We),
    case Fs of
	[A,B] -> [{A,V},{B,V}|Acc];
	[_|_] -> Acc
    end.

%% stable_faces(Faces, We) -> StableFaces
%%  Returns a list of the stable faces. Stable faces have at least
%%  three corner vertices (vertices with 3 or more neighboring edges),
%%  meaning that the face will not collapse if any vertices with only
%%  two edges are removed from it.

stable_faces(Fs, We) ->
    stable_faces(Fs, We, []).

stable_faces([F|Fs], We, Acc) ->
    case is_face_stable(F, We) of
	true -> stable_faces(Fs, We, [F|Acc]);
	false -> stable_faces(Fs, We, Acc)
    end;
stable_faces([], _, Acc) -> Acc.

is_face_stable(Face, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    is_face_stable_1(Vs, We, 0).

is_face_stable_1([V|Vs], We, N) ->
    case is_corner(V, We) of
	true -> is_face_stable_1(Vs, We, N+1);
	false -> is_face_stable_1(Vs, We, N)
    end;
is_face_stable_1([], _, N) -> N >= 3.

is_corner(V, We) ->
    N = fold(fun(_, _, _, A) -> A+1 end, 0, V, We),
    N >= 3.

%% edge_through(Vertex1, Vertex1, Face, We) -> Edge|none
%%  Returns the edge number of the edge between Vertex1 and Vertex2
%%  in the given face (if there is one).
edge_through(Va, Vb, Face, We) ->
    foldl(fun({Edge,Lf,Rf}, A) ->
		  case Face of
		      Lf -> Edge;
		      Rf -> Edge;
		      _ -> A
		  end
	  end, none, edge_through(Va, Vb, We)).

%% edge_through(Vertex1, Vertex1, We) -> [{Edge,LeftFace,RightFace}]
%%  Returns edge number and faces number for all edges between
%%  Vertex1 and Vertex2.
edge_through(Va, Vb, We) ->
    fold(fun(Edge, _, #edge{lf=Lf,rf=Rf}=Rec, A) ->
		 case other(Va, Rec) of
		     Vb -> [{Edge,Lf,Rf}|A];
		     _ -> A
		 end
	 end, [], Va, We).
