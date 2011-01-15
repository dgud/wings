%%
%%  wings_extrude_face.erl --
%%
%%     This module contains the Extrude command for faces and face regions.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_extrude_face).
-export([faces/2,regions/2]).

-include("wings.hrl").
-import(lists, [foldl/3,foreach/2,last/1,reverse/2,sort/1,merge/1]).

%%%
%%% Extrusion of faces individually (used by Extrude, Inset, Bevel).
%%%

faces([], We) ->
    We;
faces(Faces, We) when is_list(Faces) ->
    inner_extrude(Faces, We);
faces(Faces, We) ->
    faces(gb_sets:to_list(Faces), We).
    
inner_extrude([Face|Faces], #we{next_id=AnEdge,fs=Ftab0}=We0) ->
    Mat = wings_facemat:face(Face, We0),
    Ftab = gb_trees:update(Face, AnEdge, Ftab0),
    We1 = We0#we{fs=Ftab},
    Edges = inner_extrude_edges(Face, We0),
    NumVs = length(Edges),
    {Ids,We2} = wings_we:new_wrap_range(NumVs, 2, We1),
    PrevEdge = last(Edges),
    We3 = inner_extrude_1(Edges, PrevEdge, Face, Mat, Ids, We2),
    We = case wings_va:any_attributes(We3) of
	     false -> We3;
	     true -> inner_extrude_attrs(Edges, PrevEdge, Face, Ids, We2, We3)
	 end,
    inner_extrude(Faces, We);
inner_extrude([], We) -> We.

inner_extrude_edges(Face, We) ->
    wings_face:fold(fun(_, E, _Rec, A) -> [E|A] end, [], Face, We).

inner_extrude_1([Edge|Es], PrevEdge, Face, Mat, Ids0, We0) ->
    PrevHor = wings_we:id(2-2, Ids0),
    PrevFace = PrevHor,

    HorEdge = wings_we:id(2, Ids0),
    VertEdge = HorEdge + 1,
    V = NewFace = HorEdge,

    NextHor = wings_we:id(2+2, Ids0),
    NextVert = NextHor + 1,
    NextV = NextHor,

    Ids = wings_we:bump_id(Ids0),
    #we{fs=Ftab0,es=Etab0,vc=Vct0,vp=Vtab0} = We0,
    
    Erec0 = array:get(Edge, Etab0),
    Erec = case Erec0 of
	       #edge{lf=Face,vs=Va}=Erec0 ->
		   Erec0#edge{lf=NewFace,ltsu=VertEdge,ltpr=NextVert};
	       #edge{rf=Face,ve=Va}=Erec0 ->
		   Erec0#edge{rf=NewFace,rtsu=VertEdge,rtpr=NextVert}
	   end,
    Etab1 = array:set(Edge, Erec, Etab0),

    VertEdgeRec = #edge{vs=Va,ve=V,lf=PrevFace,rf=NewFace,
			ltsu=PrevEdge,ltpr=PrevHor,
			rtsu=HorEdge,rtpr=Edge},
    Etab2 = array:set(VertEdge, VertEdgeRec, Etab1),

    Etab = array:set(HorEdge,
		     #edge{vs=NextV,ve=V,
			   lf=NewFace,rf=Face,
			   ltsu=NextVert,ltpr=VertEdge,
			   rtsu=PrevHor,rtpr=NextHor}, Etab2),

    Vct = array:set(V, HorEdge, Vct0),
    Pos = array:get(Va, Vtab0),
    Vtab = array:set(V, Pos, Vtab0),

    Ftab = gb_trees:insert(NewFace, NewFace, Ftab0),
    We1 = wings_facemat:assign(Mat, [NewFace], We0),
    We = We1#we{fs=Ftab,es=Etab,vc=Vct,vp=Vtab},

    inner_extrude_1(Es, Edge, Face, Mat, Ids, We);
inner_extrude_1([], _PrevEdge, _Face, _Mat, _Ids, We) -> We.

inner_extrude_attrs([Edge|Es], PrevEdge, Face, Ids0, OrigWe, We0) ->
    PrevHor = wings_we:id(2-2, Ids0),
    HorEdge = wings_we:id(2, Ids0),
    VertEdge = HorEdge + 1,
    NewFace = HorEdge,

    Ids = wings_we:bump_id(Ids0),

    %% Set the vertex attributes.
    InsideAttr = wings_va:edge_attrs(Edge, Face, OrigWe),
    OtherFace = {other,Face},
    OutsideAttr = wings_va:edge_attrs(Edge, OtherFace, OrigWe),
    OtherOutsideAttr = wings_va:edge_attrs(PrevEdge, OtherFace, OrigWe),
    We1 = wings_va:set_edge_attrs(Edge, NewFace, OutsideAttr, We0),
    We2 = wings_va:set_edge_attrs(HorEdge, right, InsideAttr, We1),
    We3 = wings_va:set_edge_attrs(PrevHor, left, InsideAttr, We2),
    We4 = wings_va:set_edge_attrs(VertEdge, right, InsideAttr, We3),
    We = wings_va:set_edge_attrs(VertEdge, left, OtherOutsideAttr, We4),

    inner_extrude_attrs(Es, Edge, Face, Ids, OrigWe, We);
inner_extrude_attrs([], _PrevEdge, _Face, _Ids, _OrigWe, We) -> We.

%%%
%%% Extrude entire regions (does NOT work for single faces).
%%%

regions(Rs, We) ->
    regions_1(Rs, [], We).

regions_1([Faces|Rs], CollapseEs0, We0) ->
    {We,CollapseEs} = region(Faces, CollapseEs0, We0),
    regions_1(Rs, CollapseEs, We);
regions_1([], CollapseEs, We) ->
    wings_collapse:collapse_edges(CollapseEs, We).

region(Faces, CollapseEs, #we{es=Etab}=We0) ->
    ?ASSERT(gb_sets:size(Faces) > 1),
    Edges0 = wings_face:outer_edges(Faces, We0),
    G = digraph:new(),
    foreach(fun(Edge) ->
		    digraph_edge(G, Faces, array:get(Edge, Etab))
	    end, Edges0),
    Vs0 = digraph:vertices(G),
    Vs1 = sofs:relation(Vs0),
    Vs = sofs:to_external(sofs:domain(Vs1)),
    Edges = gb_sets:from_list(Edges0),
    We = foldl(fun(V, A) ->
		       new_vertices(V, G, Edges, Faces, A)
	       end, We0, Vs),
    WeEdges = connect(G, CollapseEs, We),
    digraph:delete(G),
    WeEdges.

new_vertices(V, G, Edges, Faces, We0) ->
    Pos = wings_vertex:pos(V, We0),
    wings_vertex:fold(
      fun(Edge, _, _, #we{es=Etab}=W0) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> W0;
		  false ->
		      #edge{lf=Lf} = array:get(Edge, Etab),
		      case gb_sets:is_member(Lf, Faces) of
			  true ->
			      {We,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
			      NewE = NewV,
			      Rec = get_edge_rec(V, NewV, Edge, NewE, We),
			      digraph_edge(G, Faces, Rec),
			      We;
			  false -> W0
		      end
	      end
      end, We0, V, We0).

get_edge_rec(Va, Vb, EdgeA, EdgeB, #we{es=Etab}) ->
    case array:get(EdgeA, Etab) of
	#edge{vs=Va,ve=Vb}=Rec -> Rec;
	#edge{vs=Vb,ve=Va}=Rec -> Rec;
	_Other -> array:get(EdgeB, Etab)
    end.

digraph_edge(G, Faces, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    case gb_sets:is_member(Lf, Faces) of
	true -> digraph_insert(G, Va, Vb, Lf);
	false -> ok
    end,
    case gb_sets:is_member(Rf, Faces) of
	true -> digraph_insert(G, Vb, Va, Rf);
	false -> ok
    end.

digraph_insert(G, Va0, Vb0, Face) ->
    Va = {Va0,Face},
    Vb = {Vb0,Face},
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb).

connect(G, CollapseEs, We0) ->
    Cs = get_edge_chains(G),
    foldl(fun(C, {W,A}) ->
		  connect_1(C, W, A)
	  end, {We0,CollapseEs}, Cs).

connect_1(C, We0, Acc) ->
    case C of
	[Va,_,Vb] ->
	    Face = get_face(Va, Vb, We0),
	    {We,NewEdge} = wings_vertex:force_connect(Vb, Va, Face, We0),
	    {We,[NewEdge|Acc]};
	[Va|Path] ->
	    {connect_inner(Va, Path, We0),Acc}
    end.

get_edge_chains(G) ->
    Vs = digraph:source_vertices(G),
    get_edge_chains(G, Vs, []).

get_edge_chains(G, [V|Vs], Acc) ->
    Chain = collect_chain(G, V, []),
    get_edge_chains(G, Vs, [Chain|Acc]);
get_edge_chains(_, [], Acc) -> Acc.

collect_chain(G, {V,_}=Va, Acc) ->
    case digraph:out_neighbours(G, Va) of
	[] -> reverse(Acc, [V]);
	[Vb] -> collect_chain(G, Vb, [V|Acc])
    end.

connect_inner(Current0, [_|[B,_,_|_]=Next], We0) ->
    {We,Current} = connect_one_inner(Current0, B, We0),
    connect_inner(Current, Next, We);
connect_inner(Current, [_|[_,_]=Next], We) ->
    connect_inner(Current, Next, We);
connect_inner(Current, [_,Last], We0) ->
    Face = get_face(Current, Last, We0),
    {We,_} = wings_vertex:force_connect(Last, Current, Face, We0),
    We.

connect_one_inner(Current, B, We0) ->
    Face = get_face(Current, B, We0),
    {We1,Edge} = wings_vertex:force_connect(B, Current, Face, We0),
    Pos = wings_vertex:pos(B, We1),
    wings_edge:fast_cut(Edge, Pos, We1).

get_face(Va, Vb, We) ->
    Vs = [Va,Vb],
    per_face(Vs, Vs, We, []).

per_face([V|Vs], OrigVs, We, Acc) ->
    Fs = wings_vertex:fold(
	   fun(_, Face, _, A) ->
		   [Face|A]
	   end, [], V, We),
    per_face(Vs, OrigVs, We, [Fs|Acc]);
per_face([], OrigVs, We, Acc) ->
    R = sofs:from_term(Acc, [[face]]),
    case sofs:to_external(sofs:intersection(R)) of
	[Face] -> Face;
	Faces -> choose_face(Faces, OrigVs, We)
    end.

choose_face([Face|Faces], [Va,Vb], We) ->
    D = vertex_dist(Face, Va, Vb, We),
    choose_face_1(Faces, Va, Vb, We, D, Face).

choose_face_1([Face|Faces], Va, Vb, We, OldDist, OldFace) ->
    case vertex_dist(Face, Va, Vb, We) of
	Dist when Dist < OldDist ->
	    choose_face_1(Faces, Va, Vb, We, Dist, Face);
	_Dist ->
	    choose_face_1(Faces, Va, Vb, We, OldDist, OldFace)
    end;
choose_face_1([], _, _, _, _, Face) -> Face.

vertex_dist(Face, Va, Vb, We) ->
    NumVerts = wings_face:vertices(Face, We),
    Iter0 = wings_face:iterator(Face, We),
    Iter = wings_face:skip_to_cw(Va, Iter0),
    vertex_dist_1(Iter, Vb, NumVerts, 1).

vertex_dist_1(Iter0, V, NumVerts, D) ->
    case wings_face:next_cw(Iter0) of
	{V,_,_,_} ->
	    if
		D > NumVerts div 2 -> NumVerts-D;
		true -> D
	    end;
	{_,_,_,Iter} -> vertex_dist_1(Iter, V, NumVerts, D+1)
    end.
