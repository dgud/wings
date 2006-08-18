%%
%%  wings_vertex.erl --
%%
%%     This module contains utility functions for vertices.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex.erl,v 1.51 2004/12/31 07:56:30 bjorng Exp $
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
	 connect/3,force_connect/4,
	 pos/2,outer_partition/2,reachable/2,
	 isolated/1,edge_through/3,edge_through/4]).

-include("wings.hrl").
-import(lists, [member/2,keymember/3,foldl/3,reverse/1,last/1,sort/1]).

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
    Edge = gb_trees:get(V, Vct),
    fold(F, Acc, V, Edge, We).

fold(F, Acc0, V, Edge, #we{es=Etab}) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,rf=Other,rtpr=NextEdge}=E ->
		  F(Edge, Face, E, Acc0);
	      #edge{ve=V,lf=Face,rf=Other,rtsu=NextEdge}=E ->
		  F(Edge, Face, E, Acc0)
	  end,
    fold(F, Acc, V, Other, NextEdge, Edge, Etab).

fold(_F, Acc, _V, _Face, Last, Last, _Etab) -> Acc;
fold(F, Acc0, V, Face, Edge, LastEdge, Etab) ->
    Acc = case gb_trees:get(Edge, Etab) of
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
    Edge = gb_trees:get(V, Vct),
    until(F, Acc, V, Edge, We).

until(F, Acc, V, Edge, #we{es=Etab}) ->
    #edge{lf=Face} = gb_trees:get(Edge, Etab),
    until(F, Acc, V, Face, Edge, Edge, Etab, not_done).

until(_F, Acc, _V, _Face, Last, Last, _Etab, done) -> Acc;
until(F, Acc0, V, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
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
    gb_trees:get(V, Vtab);
pos(V, Vtab) ->
    gb_trees:get(V, Vtab).

%% other_pos(Vertex, EdgeRecord, VtabOrWe) -> {X,Y,Z}
%%  Pick up the position for the "other vertex" from an edge record.
other_pos(V, #edge{vs=V,ve=Other}, Tab) -> pos(Other, Tab);
other_pos(V, #edge{ve=V,vs=Other}, Tab) -> pos(Other, Tab).

%% center(We) -> {CenterX,CenterY,CenterZ}
%%  Find the geometric center of a body.
center(#we{vp=Vtab}=We) ->
    Center = e3d_vec:average(gb_trees:values(Vtab)),
    center_1(Center, We).

center_1(Center, #we{mirror=none}) -> Center;
center_1(Center, #we{mirror=Face}=We) ->
    %% Slide the center point down to the nearest point on the mirror plane.
    MirrorNormal = wings_face:normal(Face, We),
    FaceVs = wings_face:to_vertices(gb_sets:singleton(Face), We),
    Origin = wings_vertex:center(FaceVs, We),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(MirrorNormal)),
    Flatten = e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))),
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
    do_bounding_box(gb_trees:values(Vtab), We, BB);
bounding_box(Vs, We) ->
    bounding_box(Vs, We, none).
    
bounding_box(Vs, We, BB) when list(Vs) ->
    bounding_box_1(ordsets:from_list(Vs), We, BB);
bounding_box(Vs, We, BB) ->
    bounding_box(gb_sets:to_list(Vs), We, BB).

bounding_box_1(Vs0, #we{vp=Vtab}=We, BB) ->
    Vs1 = sofs:from_external(Vs0, [vertex]),
    R = sofs:from_external(gb_trees:to_list(Vtab), [{vertex,data}]),
    I = sofs:image(R, Vs1),
    Vs = sofs:to_external(I),
    do_bounding_box(Vs, We, BB).

do_bounding_box(Vs, #we{mirror=none}, BB) ->
    do_bounding_box_1(Vs, BB);
do_bounding_box(Vs0, #we{id=Id}, BB) ->
    Mtx = wings_dl:mirror_matrix(Id),
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
per_face(Vs, We) when list(Vs) ->
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

flatten_move(V, Matrix, Tab0) ->
    Pos0 = gb_trees:get(V, Tab0),
    Pos = e3d_mat:mul_point(Matrix, Pos0),
    gb_trees:update(V, Pos, Tab0).

%% dissolve_isolated_vs([Vertex], We) -> We'
%%  Remove all isolated vertices ("winged vertices", or vertices
%%  having exactly two edges).
dissolve_isolated(Vs, We) ->
    wings_edge:dissolve_isolated_vs(Vs, We).

%% Connect vertices (which must share a face).

connect(_Face, [_], We) -> We;
connect(Face, Vs, #we{}=We0) ->
    case polygon_pairs(Face, Vs, We0) of
	no -> min_distance_pairs(Face, Vs, We0);
	#we{}=We -> We
    end.

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

%% Don't go for position distance use the topological distance instead.
%% Hopefully fixes this problem
%  +__*_ *__*_+
%   \       |  \
%    \      |   \ 
%     \      |   \
%      \     |    \ 
%       \    |     \
%        \   |      \
%         +--*--*--*-+

nearest_pair_smart(Face, AllVs, We) ->
    FaceVs = wings_face:vertices_ccw(Face, We),
    Vs0    = ordsets:from_list(FaceVs),
    Vs     = ordsets:intersection(Vs0, AllVs),
    nearest_pair_smart_1(FaceVs, Vs, Face, We, []).

%% If we new that the intersection was stable this step wouldn't be needed.
nearest_pair_smart_1([V|Vs], Sel, Face, We, Acc) ->
    case ordsets:is_element(V, Sel) of
	true ->
	    nearest_pair_smart_1(Vs, Sel, Face, We, [V|Acc]);
	false ->
	    nearest_pair_smart_1(Vs, Sel, Face, We, Acc)
    end;
nearest_pair_smart_1([], _, Face, We, Acc=[Last|_]) when length(Acc) > 1 ->
    connect_pairs([Last|lists:reverse(Acc)],Face,We);
nearest_pair_smart_1([], _, _, _, _) ->
    no.
    
nearest_pair(Face, AllVs, #we{vp=Vtab}=We) ->
    Vs0 = ordsets:from_list(wings_face:vertices_ccw(Face, We)),
    Vs = ordsets:intersection(Vs0, AllVs),
    VsPos = [{V,gb_trees:get(V, Vtab)} || V <- Vs],
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
    case wings_face:good_normal(Face, We) andalso
	wings_face:good_normal(NewFace, We) of
	true -> Res;
	false -> no
    end.

force_connect(Vstart, Vend, Face, #we{es=Etab0,fs=Ftab0}=We0) ->
    {NewFace,We} = wings_we:new_ids(1, We0),
    NewEdge = NewFace,
    NeRec0 = #edge{vs=Vstart,ve=Vend,lf=NewFace,rf=Face},

    Iter0 = wings_face:iterator(Face, We),
    Iter1 = wings_face:skip_to_cw(Vstart, Iter0),
    {_,_,_,Iter2} = wings_face:next_ccw(Iter1),
    {Etab1,NeRec1,Iter3} = connect_1(Iter2, Vstart, NewEdge, NeRec0, Etab0),
    {Etab2,NeRec2} = connect_2(Iter3, Vstart, NewEdge, NeRec1, Etab1),
    {Etab3,Iter} = connect_3(Iter3, Face, Vend, NewFace, Etab2),
    Etab = connect_4(Iter, Vend, NewEdge, NeRec2, Etab3),

    Ftab1 = gb_trees:insert(NewFace, NewEdge, Ftab0),
    Ftab = gb_trees:update(Face, NewEdge, Ftab1),
    Mat = wings_facemat:face(Face, We),
    {wings_facemat:assign(Mat, [NewFace], We#we{es=Etab,fs=Ftab}),NewFace}.

%% connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab0) -> {Etab,NeRec,Iter}
%%  Connect the edge immediately before Vstart.
connect_1(Iter0, Vstart, NewEdge, NeRec0, Etab) ->
    case wings_face:next_cw(Iter0) of
	{_,Edge,#edge{b=ColB,ve=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{a=ColB,rtpr=Edge},
	    Rec = Rec0#edge{rtsu=NewEdge};
	{_,Edge,#edge{a=ColA,vs=Vstart}=Rec0,Iter} ->
	    NeRec = NeRec0#edge{a=ColA,rtpr=Edge},
	    Rec = Rec0#edge{ltsu=NewEdge}
    end,
    {gb_trees:update(Edge, Rec, Etab),NeRec,Iter}.

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
    {gb_trees:update(Edge, Rec, Etab),NeRec}.

%% connect_3(Iter, Face, Vend, NewFace, Etab0) -> {Etab,Iter}
%%  Replace the face for all edges between Vstart and Vend.
%%  The returned iterator points to the edge immediately before Vend.
connect_3(Iter0, Face, Vend, NewFace, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),

    %% Ignore the record returned by the iterator, because it
    %% is stale for the edge that was updated by connect_2/5.
    Rec = case gb_trees:get(Edge, Etab0) of
	      #edge{lf=Face}=Rec0 -> Rec0#edge{lf=NewFace};
	      #edge{rf=Face}=Rec0 -> Rec0#edge{rf=NewFace}
	  end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    case Rec of
	#edge{vs=Vend} -> {Etab,Iter0};
	#edge{ve=Vend} -> {Etab,Iter0};
	_Other -> connect_3(Iter, Face, Vend, NewFace, Etab)
    end.

%% connect_4(Iter, Vend, NewEdge, NeRec, Etab0) -> Etab
%%  Patches the final two edges.
connect_4(Iter0, Vend, NewEdge, NeRec0, Etab0) ->
    {_,Edge,_,Iter} = wings_face:next_cw(Iter0),
    Rec = case gb_trees:get(Edge, Etab0) of
	      #edge{b=ColB,ve=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{b=ColB,ltpr=Edge},
		  Rec0#edge{rtsu=NewEdge};
	      #edge{a=ColA,vs=Vend}=Rec0 ->
		  NeRec1 = NeRec0#edge{b=ColA,ltpr=Edge},
		  Rec0#edge{ltsu=NewEdge}
	  end,
    Etab1 = gb_trees:update(Edge, Rec, Etab0),

    %% Now for the final edge.
    FinalRec = case wings_face:next_cw(Iter) of
		   {_,Final,#edge{vs=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{rtpr=NewEdge};
		   {_,Final,#edge{ve=Vend}=FinalRec0,_} ->
		       NeRec = NeRec1#edge{rtsu=Final},
		       FinalRec0#edge{ltpr=NewEdge}
	       end,
    Etab = gb_trees:update(Final, FinalRec, Etab1),
    gb_trees:insert(NewEdge, NeRec, Etab).

%% outer_partition(Faces, We) -> [[V]]
%%  Returns a list of the vertices of the outer edges of the faces.
%%  Vertices are ordered CCW.
outer_partition(Faces, We) when is_list(Faces) ->
    collect_outer_edges(Faces, gb_sets:from_list(Faces), We, []);
outer_partition(Faces, We) ->
    collect_outer_edges(gb_sets:to_list(Faces), Faces, We, []).

collect_outer_edges([Face|Fs], Faces, We, Acc0) ->
    Acc = wings_face:fold(
	    fun(_, E, Erec, A) ->
		    outer_edge(E, Erec, Face, Faces, A)
	    end, Acc0, Face, We),
    collect_outer_edges(Fs, Faces, We, Acc);
collect_outer_edges([], _Faces, _We, Acc) ->
    R = sofs:relation(Acc),
    F = sofs:relation_to_family(R),
    partition_edges(gb_trees:from_orddict(sofs:to_external(F)), []).

outer_edge(Edge, Erec, Face, Faces, Acc) ->
    {V,OtherV,OtherFace} =
	case Erec of
	    #edge{vs=Vs,ve=Ve,lf=Face,rf=Other0} ->
		{Vs,Ve,Other0};
	    #edge{vs=Vs,ve=Ve,rf=Face,lf=Other0} ->
		{Ve,Vs,Other0}
	end,
    case gb_sets:is_member(OtherFace, Faces) of
	true -> Acc;
	false -> [{V,{Edge,V,OtherV,Face}}|Acc]
    end.

partition_edges(Es0, Acc) ->
    case gb_sets:is_empty(Es0) of
	true -> Acc;
	false ->
	    {Key,Val,Es1} = gb_trees:take_smallest(Es0),
	    {Part,Es} = partition_edges(Key, unknown, Val, Es1, []),
	    partition_edges(Es, [Part|Acc])
    end.

partition_edges(Va, _, [{_,Va,Vb,Face}], Es0, Acc0) ->
    Acc = [Va|Acc0],
    case gb_trees:lookup(Vb, Es0) of
	none -> {Acc,Es0};
	{value,Val} ->
	    Es = gb_trees:delete(Vb, Es0),
	    partition_edges(Vb, Face, Val, Es, Acc)
    end;
partition_edges(Va, unknown, [{_,Va,_,Face}|_]=Edges, Es, Acc) ->
    partition_edges(Va, Face, Edges, Es, Acc);
partition_edges(Va, Face, Edges0, Es0, Acc) ->
    [Val] = [E || {_,_,_,AFace}=E <- Edges0, AFace =:= Face],
    Edges = [E || {_,_,_,AFace}=E <- Edges0, AFace =/= Face],
    Es = gb_trees:insert(Va, Edges, Es0),
    partition_edges(Va, Face, [Val], Es, Acc).

%% reachable([Vertex], We) -> [ReachableVertex]
%%  Returns a list of the vertices that can be reached by following
%%  edges from the given list of vertices.
reachable(Vs0, #we{es=Etab,vc=Vct}) when is_list(Vs0) ->
    Es0 = foldl(fun(V, A) ->
			[gb_trees:get(V, Vct)|A]
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
	    Rec = gb_trees:get(Edge, Etab),
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
		end, [], gb_trees:keys(Vtab)),
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
