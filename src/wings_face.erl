%%
%%  wings_face.erl --
%%
%%     This module contains help routines for faces, such as fold functions
%%     face iterators.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_face.erl,v 1.53 2006/04/08 12:35:34 dgud Exp $
%%

-module(wings_face).
-export([from_edges/2,from_vs/2,
	 other/2,vertices/2,
	 to_edges/2,to_vertices/2,
	 normal/2,normal/3,
	 face_normal_cw/2,face_normal_ccw/2,
	 good_normal/2,
	 center/2,area/2,
	 vinfo_cw/2,vinfo_cw/3,
	 vinfo_ccw/2,vinfo_ccw/3,
	 vertices_cw/2,vertices_cw/3,
	 vertices_ccw/2,vertices_ccw/3,
	 vertex_positions/2,vertex_positions/3,
	 vertex_info/2,vertex_info/3,
	 extend_border/2,
	 inner_edges/2,outer_edges/2,inner_outer_edges/2,
	 fold/4,fold/5,fold_vinfo/4,fold_faces/4,
	 iterator/2,skip_to_edge/2,skip_to_cw/2,skip_to_ccw/2,
	 next_cw/1,next_ccw/1,
	 iter2etab/1,
	 patch_face/3,patch_face/4,
	 delete_bad_faces/2,
	 are_neighbors/3,is_planar/3]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

from_edges(Es, #we{es=Etab}) when is_list(Es) ->
    from_edges_1(Es, Etab, []);
from_edges(Es, We) ->
    from_edges(gb_sets:to_list(Es), We).
    
from_edges_1([E|Es], Etab, Acc) ->
    #edge{lf=Lf,rf=Rf} = gb_trees:get(E, Etab),
    from_edges_1(Es, Etab, [Lf,Rf|Acc]);
from_edges_1([], _, Acc) -> gb_sets:from_list(Acc).

%% from_vs([Vertex], We) -> [Face]
%%  Convert a list or gbset of vertices to an ordered list of faces.
from_vs(Vs, We) when is_list(Vs) ->
    Fun = fun(_, F, _, A) -> [F|A] end,
    from_vs_1(Vs, Fun, We, []);
from_vs(Vs, We) ->
    from_vs(gb_sets:to_list(Vs), We).

from_vs_1([V|Vs], Fun, We, Acc0) ->
    Acc = wings_vertex:fold(Fun, Acc0, V, We),
    from_vs_1(Vs, Fun, We, Acc);
from_vs_1([], _, _, Acc) -> ordsets:from_list(Acc).

%% other(Face, EdgeRecord) -> OtherFace
%%  Pick up the "other face" from an edge record.
other(Face, #edge{lf=Face,rf=Other}) -> Other;
other(Face, #edge{rf=Face,lf=Other}) -> Other.

%% to_edges(Faces, We) -> [Edge]
%%  Convert a set or list of faces to a list of edges.
to_edges(Fs, We) ->
    ordsets:from_list(to_edges_raw(Fs, We)).

%% to_vertices(Faces, We) -> [Vertex]
%%  Convert a set or list of faces to a list of vertices.
to_vertices(Fs, We) ->
    Vs = fold_faces(fun(_, V, _, _, A) -> [V|A] end, [], Fs, We),
    ordsets:from_list(Vs).

%% vertices(Face, We) -> NumberOfVertices
%%  Calculate the number of vertices in a face.
vertices(Face, We) ->
    fold(fun(_, _, _, N) -> N+1 end, 0, Face, We).

%% Return the normal for a face.

normal(Face, We) ->
    e3d_vec:normal(vertex_positions(Face, We)).

normal(Face, Edge, We) ->
    e3d_vec:normal(vertex_positions(Face, Edge, We)).

%% face_normal_cw(Vertices, WeOrVtab) -> Normal
%%  Returns the normal for face consisting of Vertices, listed
%%  in clock-wise order. (Slightly more efficient than face_normal_ccw/2.)
face_normal_cw(Vs, #we{vp=Vtab}) ->
    face_normal_cw(Vs, Vtab, []);
face_normal_cw(Vs, Vtab) ->
    face_normal_cw(Vs, Vtab, []).

face_normal_cw([V|Vs], Vtab, Acc) ->
    face_normal_cw(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
face_normal_cw([], _Vtab, Acc) ->
    e3d_vec:normal(Acc).

%% face_normal_ccw(Vertices, WeOrVtab) -> Normal
%%  Returns the normal for face consisting of Vertices, listed
%%  in counter-clock order.
face_normal_ccw(Vs, #we{vp=Vtab}) ->
    face_normal_ccw(Vs, Vtab, []);
face_normal_ccw(Vs, Vtab) ->
    face_normal_ccw(Vs, Vtab, []).

face_normal_ccw([V|Vs], Vtab, Acc) ->
    face_normal_ccw(Vs, Vtab, [gb_trees:get(V, Vtab)|Acc]);
face_normal_ccw([], _Vtab, Acc) ->
    e3d_vec:normal(reverse(Acc)).

%% Tests if the face has a good normal.
good_normal(Face, #we{vp=Vtab}=We) ->
    [Va,Vb|_] = Vpos =
	fold(fun(V, _, _, A) ->
		     [gb_trees:get(V, Vtab)|A]
	     end, [], Face, We),
    D = e3d_vec:sub(Va, Vb),
    good_normal(D, Vpos, Vpos).

good_normal(D1, [_Va|[Vb,Vc|_]=Vs], More) ->
    ?ASSERT(D1 == e3d_vec:sub(_Va, Vb)),
    D2 = e3d_vec:sub(Vb, Vc),
    Cross = e3d_vec:cross(D1, D2),
    case e3d_vec:len(Cross) of
	Zero when abs(Zero) < 1.0e-5 ->
	    good_normal(D2, Vs, More);
	_Len -> true
    end;
good_normal(D1, Vs, [Va,Vb|_]) ->
    good_normal(D1, Vs++[Va,Vb], []);
good_normal(_, _, _) -> false.

%% center(Face, We)
%%  Return the center of the face.
center(Face, We) ->
    wings_vertex:center(vertices_ccw(Face, We), We).

%% area(Face, We)
%%  Return the area of the face, according to a simple triangulation.
area(Face, #we{fs=Ftab,es=Etab,vp=Vtab}=We) ->
    E0 = gb_trees:get(Face, Ftab),
    Edge = gb_trees:get(E0, Etab),
    %% Traverse ccw
    {V0,V1,E1} = 
	case Edge of
	    #edge{vs=Vs,ve=Ve,lf=Face,ltsu=E} -> {Vs,Ve,E};
	    #edge{vs=Vs,ve=Ve,rf=Face,rtsu=E} -> {Ve,Vs,E}
	end,
    P0 = gb_trees:get(V0, Vtab),
    P1 = gb_trees:get(V1, Vtab),
    area_1(Face, We, E0, P0, P1, E1, 0.0).

area_1(_Face, _We, E0, _P0, _P1, E0, Area) when is_float(Area) -> Area * 0.5;
area_1(Face, #we{es=Etab,vp=Vtab}=We, E0, P0, P1, E1, Area) 
  when is_float(Area) ->
    Edge = gb_trees:get(E1, Etab),
    {V2,E2} = 
	case Edge of
	    #edge{ve=V,lf=Face,ltsu=E} -> {V,E};
	    #edge{vs=V,rf=Face,rtsu=E} -> {V,E}
	end,
    P2 = gb_trees:get(V2, Vtab),
    A = e3d_vec:len(e3d_vec:cross(e3d_vec:sub(P1, P0), e3d_vec:sub(P2, P0))),
    if is_float(A) -> area_1(Face, We, E0, P0, P2, E2, Area+A) end.

%% Vertex info for drawing.

vinfo_cw(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vinfo_cw(Face, Edge, We).

vinfo_cw(Face, Edge, #we{es=Etab}) ->
    vinfo_cw_1(Edge, Etab, Face, Edge, []).

vinfo_cw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vinfo_cw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,a=Col,lf=Face,ltpr=NextEdge} ->
	    vinfo_cw_1(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc]);
	#edge{ve=V,b=Col,rtpr=NextEdge} ->
	    vinfo_cw_1(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc])
    end.

vinfo_ccw(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vinfo_ccw(Face, Edge, We).

vinfo_ccw(Face, Edge, #we{es=Etab}) ->
    vinfo_ccw_1(Edge, Etab, Face, Edge, []).

vinfo_ccw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vinfo_ccw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,a=Col,lf=Face,ltsu=NextEdge} ->
	    vinfo_ccw_1(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc]);
	#edge{ve=V,b=Col,rtsu=NextEdge} ->
	    vinfo_ccw_1(NextEdge, Etab, Face, LastEdge, [[V|Col]|Acc])
    end.

vertices_cw(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    vertices_cw_1(Edge, Etab, Face, Edge, []).

vertices_cw(Face, Edge, #we{es=Etab}) ->
    vertices_cw_1(Edge, Etab, Face, Edge, []).

vertices_cw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertices_cw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltpr=NextEdge} ->
	    vertices_cw_1(NextEdge, Etab, Face, LastEdge, [V|Acc]);
	#edge{ve=V,rf=Face,rtpr=NextEdge} ->
	    vertices_cw_1(NextEdge, Etab, Face, LastEdge, [V|Acc])
    end.

vertices_ccw(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    vertices_ccw_1(Edge, Etab, Face, Edge, []).

vertices_ccw(Face, Edge, #we{es=Etab}) ->
    vertices_ccw_1(Edge, Etab, Face, Edge, []).

vertices_ccw_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertices_ccw_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    vertices_ccw_1(NextEdge, Etab, Face, LastEdge, [V|Acc]);
	#edge{ve=V,rf=Face,rtsu=NextEdge} ->
	    vertices_ccw_1(NextEdge, Etab, Face, LastEdge, [V|Acc])
    end.

vertex_positions(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vertex_positions(Face, Edge, We).

vertex_positions(Face, Edge, #we{es=Etab,vp=Vtab}) ->
    vertex_positions_1(Edge, Etab, Vtab, Face, Edge, []).

vertex_positions_1(LastEdge, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertex_positions_1(Edge, Etab, Vtab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Pos = gb_trees:get(V, Vtab),
	    vertex_positions_1(NextEdge, Etab, Vtab, Face, LastEdge, [Pos|Acc]);
	#edge{ve=V,rf=Face,rtsu=NextEdge} ->
	    Pos = gb_trees:get(V, Vtab),
	    vertex_positions_1(NextEdge, Etab, Vtab, Face, LastEdge, [Pos|Acc])
    end.

vertex_info(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vertex_info(Face, Edge, We).

vertex_info(Face, Edge, #we{es=Etab}) ->
    vertex_info_1(Edge, Etab, Face, Edge, []).

vertex_info_1(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
vertex_info_1(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{a=Info,lf=Face,ltsu=NextEdge} ->
	    vertex_info_1(NextEdge, Etab, Face, LastEdge, [Info|Acc]);
	#edge{b=Info,rf=Face,rtsu=NextEdge} ->
	    vertex_info_1(NextEdge, Etab, Face, LastEdge, [Info|Acc])
    end.

%% extend_border(FacesGbSet, We) -> FacesGbSet'
%%  Extend the the given set of faces to include all faces not in the
%%  set that share at least one edge with a face in the set.
extend_border(Fs0, We) ->
    foldl(fun(Face, S0) ->
		  fold(fun(_, _, #edge{lf=Lf,rf=Rf}, S1) ->
			       if
				   Lf =/= Face -> gb_sets:add(Lf, S1);
				   true -> gb_sets:add(Rf, S1)
			       end
		       end, S0, Face, We)
	  end, Fs0, gb_sets:to_list(Fs0)).

%% inner_edges(Faces, We) -> [Edge]
%%  Given a set of faces, return all inner edges.
inner_edges(Faces, We) ->
    S = to_edges_raw(Faces, We),
    inner_edges_1(sort(S), []).

inner_edges_1([E,E|T], In) ->
    inner_edges_1(T, [E|In]);
inner_edges_1([_|T], In) ->
    inner_edges_1(T, In);
inner_edges_1([], In) -> reverse(In).

%% outer_edges(Faces, We) -> [Edge]
%%  Given a set of faces, return all outer edges.
outer_edges(Faces, We) ->
    S = to_edges_raw(Faces, We),
    outer_edges_1(sort(S), []).

outer_edges_1([E,E|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([E|T], Out) ->
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

%% inner_outer_edges(Faces, We) -> {[InnerEdge],[OuterEdge]}
%%  Given a set of faces, return all inner and outer edges.
inner_outer_edges(Faces, We) ->
    S = to_edges_raw(Faces, We),
    inner_outer_edges_1(sort(S), [], []).

inner_outer_edges_1([E,E|T], In, Out) ->
    inner_outer_edges_1(T, [E|In], Out);
inner_outer_edges_1([E|T], In, Out) ->
    inner_outer_edges_1(T, In, [E|Out]);
inner_outer_edges_1([], In, Out) ->
    {reverse(In),reverse(Out)}.


%% Fold over all edges surrounding a face.

fold(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold(Edge, Etab, F, Acc, Face, Edge, not_done).

fold(F, Acc, Face, Edge, #we{es=Etab}) ->
    fold(Edge, Etab, F, Acc, Face, Edge, not_done).

fold(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
fold(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(V, Edge, E, Acc0),
	    fold(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%% Fold over all edges surrounding a face.

fold_vinfo(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold_vinfo(F, Acc, Face, Edge, Edge, Etab, not_done).

fold_vinfo(_F, Acc, _Face, LastEdge, LastEdge, _Etab, done) -> Acc;
fold_vinfo(F, Acc0, Face, Edge, LastEdge, Etab, _) ->
    Acc = case gb_trees:get(Edge, Etab) of
	      #edge{vs=V,a=VInfo,lf=Face,ltsu=NextEdge} ->
		  F(V, VInfo, Acc0);
	      #edge{ve=V,b=VInfo,rf=Face,rtsu=NextEdge} ->
		  F(V, VInfo, Acc0)
	  end,
    fold_vinfo(F, Acc, Face, NextEdge, LastEdge, Etab, done).

%% Fold over a set of faces.

fold_faces(F, Acc0, [Face|Faces], #we{es=Etab,fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = fold_faces_1(Edge, Etab, F, Acc0, Face, Edge, not_done),
    fold_faces(F, Acc, Faces, We);
fold_faces(_F, Acc, [], _We) -> Acc;
fold_faces(F, Acc, Faces, We) ->
    fold_faces(F, Acc, gb_sets:to_list(Faces), We).

fold_faces_1(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
fold_faces_1(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(Face, V, Edge, E, Acc0),
	    fold_faces_1(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{vs=V,rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(Face, V, Edge, E, Acc0),
	    fold_faces_1(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%% Return an unsorted list of edges for the faces (with duplicates).

to_edges_raw(Faces, #we{es=Etab,fs=Ftab}) when is_list(Faces) ->
    to_edges_raw(Faces, Ftab, Etab, []);
to_edges_raw(Faces, We) ->
    to_edges_raw(gb_sets:to_list(Faces), We).

to_edges_raw([Face|Faces], Ftab, Etab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_edges_raw_1(Edge, Etab, Acc0, Face, Edge, not_done),
    to_edges_raw(Faces, Ftab, Etab, Acc);
to_edges_raw([], _, _, Acc) -> Acc.

to_edges_raw_1(LastEdge, _, Acc, _, LastEdge, done) -> Acc;
to_edges_raw_1(Edge, Etab, Acc, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    to_edges_raw_1(NextEdge, Etab, [Edge|Acc], Face, LastEdge, done);
	#edge{rf=Face,rtsu=NextEdge} ->
	    to_edges_raw_1(NextEdge, Etab, [Edge|Acc], Face, LastEdge, done)
    end.

%% Return an iterator which can be used to traverse the face.

iterator(Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    {face_iterator,Edge,Face,Etab}.

skip_to_edge(Edge, {face_iterator,_,_,_}=Iter0) ->
    case next_cw(Iter0) of
	{_,Edge,_,_} -> Iter0;
	{_,_,_,Iter} -> skip_to_edge(Edge, Iter)
    end.

skip_to_cw(V, {face_iterator,_,Face,_}=Iter0) ->
    case next_cw(Iter0) of
	{_,_,#edge{vs=V,lf=Face},Iter} -> Iter;
	{_,_,#edge{ve=V,rf=Face},Iter} -> Iter;
	{_,_,_,Iter} -> skip_to_cw(V, Iter)
    end.

skip_to_ccw(V, {face_iterator,_,Face,_}=Iter0) ->
    case next_ccw(Iter0) of
	{_,_,#edge{ve=V,lf=Face},Iter} -> Iter;
	{_,_,#edge{vs=V,rf=Face},Iter} -> Iter;
	{_,_,_,Iter} -> skip_to_ccw(V, Iter)
    end.

iter2etab({face_iterator,_,_,Etab}) -> Etab.

%% Return next edge clockwise.

next_cw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtsu=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

%% Return next edge counter-clockwise.

next_ccw({face_iterator,Edge,Face,Etab}) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}};
	#edge{vs=V,rf=Face,rtpr=NextEdge}=Rec ->
	    {V,Edge,Rec,{face_iterator,NextEdge,Face,Etab}}
    end.

delete_bad_faces(Fs, #we{fs=Ftab,es=Etab}=We) when is_list(Fs) ->
    Es = bad_edges(Fs, Ftab, Etab, []),
    wings_edge:dissolve_edges(Es, We);
delete_bad_faces(Fs, We) ->
    delete_bad_faces(gb_sets:to_list(Fs), We).

bad_edges([F|Fs], Ftab, Etab, Acc) ->
    case gb_trees:lookup(F, Ftab) of
	{value,Edge} ->
	    case gb_trees:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		    erlang:error({internal_error,one_edged_face,F});
		#edge{ltpr=Same,ltsu=Same} ->
		    bad_edges(Fs, Ftab, Etab, [Edge|Acc]);
		#edge{rtpr=Same,rtsu=Same} ->
		    bad_edges(Fs, Ftab, Etab, [Edge|Acc]);
		_ -> bad_edges(Fs, Ftab, Etab, Acc)
	    end;
	none -> bad_edges(Fs, Ftab, Etab, Acc)
    end;
bad_edges([], _, _, Acc) -> Acc.

patch_face(Face, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	NewEdge -> Ftab;
	_ -> gb_trees:update(Face, NewEdge, Ftab)
    end.

patch_face(Face, Edge, NewEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	Edge -> gb_trees:update(Face, NewEdge, Ftab);
	_ -> Ftab
    end.

%% Test whether two faces are neighbors or not. (In the sense that
%% they share at least one vertex.)
are_neighbors(FaceA, FaceB, We) ->
    VsA = wings_face:vertices_ccw(FaceA, We),
    VsB = wings_face:vertices_ccw(FaceB, We),
    ordsets:intersection(ordsets:from_list(VsA),
			 ordsets:from_list(VsB)) =/= [].

%% Test whether a face is planar
is_planar(Tolerance, Face, We) ->
    Norm = normal(Face, We),
    VertPos = vertex_positions(Face, We),
    [Vert0|Verts] = VertPos,
    Dist = e3d_vec:dot(Norm,Vert0),
    is_planar_1(ture,Norm,Dist,Tolerance,Verts).
    
is_planar_1(Planar,Norm,Dist,Tolerance,[Vert|T]) ->
    case Planar of
        false -> false;
        _ ->
            Diff = abs(e3d_vec:dot(Norm,Vert) - Dist),
            case Diff > Tolerance of
                true ->
                    false;
                _ ->
                    is_planar_1(true,Norm,Dist,Tolerance,T)
            end
    end;
is_planar_1(_,_,_,_,[]) ->
    true.
