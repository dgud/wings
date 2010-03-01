%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_edge).

%% Utilities.
-export([from_vs/2,to_vertices/2,from_faces/2,
	 select_region/1,
	 select_edge_ring/1,select_edge_ring_incr/1,select_edge_ring_decr/1,
	 cut/3,fast_cut/3,screaming_cut/3,
	 dissolve_edges/2,dissolve_edge/2,
	 hardness/3,
	 patch_edge/4,patch_edge/5,
	 select_nth_ring/2]).

-export([dissolve_isolated_vs/2]).

-include("wings.hrl").
-import(lists, [foldl/3,sort/1]).

from_vs(Vs, We) when is_list(Vs) ->
    from_vs(Vs, We, []);
from_vs(Vs, We) ->
    gb_sets:from_list(from_vs(gb_sets:to_list(Vs), We, [])).

from_vs([V|Vs], We, Acc0) ->
    Acc = wings_vertex:fold(fun(E, _, _, A) -> [E|A] end, Acc0, V, We),
    from_vs(Vs, We, Acc);
from_vs([], _, Acc) -> Acc.

%% to_vertices(EdgeGbSet, We) -> VertexGbSet
%%  Convert a set of edges to a set of vertices.

to_vertices(Edges, #we{es=Etab}) when is_list(Edges) ->
    to_vertices(Edges, Etab, []);
to_vertices(Edges, #we{es=Etab}) ->
    to_vertices(gb_sets:to_list(Edges), Etab, []).

to_vertices([E|Es], Etab, Acc) ->
    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
    to_vertices(Es, Etab, [Va,Vb|Acc]);
to_vertices([], _Etab, Acc) -> ordsets:from_list(Acc).

%% from_faces(FaceSet, We) -> EdgeSet
%%  Convert faces to edges.
from_faces(Faces, We) ->
    gb_sets:from_ordset(wings_face:to_edges(Faces, We)).

%% cut(Edge, Parts, We0) -> {We,NewVertex,NewEdge}
%%  Cut an edge into Parts parts.
cut(Edge, 2, We) ->
    fast_cut(Edge, default, We);
cut(Edge, N, #we{es=Etab}=We) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    PosA = wings_vertex:pos(Va, We),
    PosB = wings_vertex:pos(Vb, We),
    Vec = e3d_vec:mul(e3d_vec:sub(PosB, PosA), 1/N),
    cut_1(N, Edge, PosA, Vec, We).

cut_1(2, Edge, _, _, We) ->
    fast_cut(Edge, default, We);
cut_1(N, Edge, Pos0, Vec, We0) ->
    Pos = e3d_vec:add(Pos0, Vec),
    {We,NewE} = fast_cut(Edge, Pos, We0),
    cut_1(N-1, NewE, Pos, Vec, We).

%% fast_cut(Edge, Position, We0) -> {We,NewElement}
%%      NewElement = ID for the new vertex and the new Edge
%%  Cut an edge in two parts. Position can be given as
%%  the atom `default', in which case the position will
%%  be set to the midpoint of the edge.

fast_cut(Edge, Pos0, We0) ->
    {NewEdge=NewV,We1} = wings_we:new_ids(1, We0),
    #we{es=Etab0,vc=Vct0,vp=Vtab0,he=Htab0} = We1,
    Template = array:get(Edge, Etab0),
    #edge{vs=Vstart,ve=Vend,ltpr=EdgeA,rtsu=EdgeB} = Template,
    VendPos = array:get(Vend, Vtab0),
    Vct1 = array:set(Vend, NewEdge, Vct0),
    VstartPos = wings_vertex:pos(Vstart, Vtab0),
    if
	Pos0 =:= default ->
	    NewVPos0 = e3d_vec:average(VstartPos, VendPos);
	true ->
	    NewVPos0 = Pos0
    end,
    NewVPos = wings_util:share(NewVPos0),
    Vct = array:set(NewV, NewEdge, Vct1),
    Vtab = array:set(NewV, NewVPos, Vtab0),

    NewEdgeRec = Template#edge{vs=NewV,ltsu=Edge,rtpr=Edge},
    Etab1 = array:set(NewEdge, NewEdgeRec, Etab0),
    Etab2 = patch_edge(EdgeA, NewEdge, Edge, Etab1),
    Etab3 = patch_edge(EdgeB, NewEdge, Edge, Etab2),
    EdgeRec = Template#edge{ve=NewV,rtsu=NewEdge,ltpr=NewEdge},
    Etab = array:set(Edge, EdgeRec, Etab3),
    Htab = case gb_sets:is_member(Edge, Htab0) of
	       false -> Htab0;
	       true -> gb_sets:insert(NewEdge, Htab0)
	   end,
    We2 = We1#we{es=Etab,vc=Vct,vp=Vtab,he=Htab},

    %% Now interpolate and set vertex attributes.
    Weight = if
		 Pos0 =:= default -> 0.5;
		 VstartPos =:= VendPos -> 0.5;
		 Pos0 =:= VstartPos -> 0.0;
		 Pos0 =:= VendPos -> 1.0;
		 true ->
		     ADist = e3d_vec:dist(Pos0, VstartPos),
		     BDist = e3d_vec:dist(Pos0, VendPos),
		     ADist/(ADist+BDist)
	     end,
    AttrMidLeft = wings_va:edge_attrs(Edge, left, Weight, We1),
    AttrMidRight = wings_va:edge_attrs(Edge, right, Weight, We1),
    AttrEndLeft = wings_va:edge_attrs(Edge, right, We1),

    We3 = wings_va:set_edge_attrs(Edge, right, AttrMidRight, We2),
    We = wings_va:set_both_edge_attrs(NewEdge, AttrMidLeft, AttrEndLeft, We3),

    {We,NewV}.

%% screaming_cut(Edge, Position, We0) -> {We,NewVertex,NewEdge}
%%  Cut an edge in two parts screamlingly fast. Does not handle
%%  vertex colors or UV coordinates.

screaming_cut(Edge, NewVPos, We0) ->
    {NewEdge=NewV,We} = wings_we:new_ids(1, We0),
    #we{es=Etab0,vc=Vct0,vp=Vtab0,he=Htab0} = We,
    Template = array:get(Edge, Etab0),
    #edge{ve=Vend,ltpr=EdgeA,rtsu=EdgeB} = Template,
    Vct1 = array:set(Vend, NewEdge, Vct0),
    Vct = array:set(NewV, NewEdge, Vct1),
    Vtab = array:set(NewV, NewVPos, Vtab0),

    NewEdgeRec = Template#edge{vs=NewV,ltsu=Edge,rtpr=Edge},
    Etab1 = array:set(NewEdge, NewEdgeRec, Etab0),
    Etab2 = patch_edge(EdgeA, NewEdge, Edge, Etab1),
    Etab3 = patch_edge(EdgeB, NewEdge, Edge, Etab2),
    EdgeRec = Template#edge{ve=NewV,rtsu=NewEdge,ltpr=NewEdge},
    Etab = array:set(Edge, EdgeRec, Etab3),

    Htab = case gb_sets:is_member(Edge, Htab0) of
	       false -> Htab0;
	       true -> gb_sets:insert(NewEdge, Htab0)
	   end,
    {We#we{es=Etab,vc=Vct,vp=Vtab,he=Htab},NewV}.

%%%
%%% Dissolve.
%%%

dissolve_edge(Edge, We) ->
    dissolve_edges([Edge], We).

dissolve_edges(Edges0, We0) when is_list(Edges0) ->
    #we{es=Etab} = We1 = foldl(fun internal_dissolve_edge/2, We0, Edges0),
    case [E || E <- Edges0, array:get(E, Etab) =/= undefined] of
	Edges0 ->
	    %% No edge was deleted in the last pass. We are done.
	    We = wings_we:rebuild(We0),
	    wings_we:validate_mirror(We);
	Edges ->
	    dissolve_edges(Edges, We1)
    end;
dissolve_edges(Edges, We) ->
    dissolve_edges(gb_sets:to_list(Edges), We).

internal_dissolve_edge(Edge, #we{es=Etab}=We0) ->
    case array:get(Edge, Etab) of
	undefined -> We0;
	#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
	    EmptyGbTree = gb_trees:empty(),
	    Empty = array:new(),
	    We0#we{vc=Empty,vp=Empty,es=Empty,fs=EmptyGbTree,he=gb_sets:empty()};
	#edge{rtpr=Back,ltsu=Back}=Rec ->
	    merge_edges(backward, Edge, Rec, We0);
	#edge{rtsu=Forward,ltpr=Forward}=Rec ->
	    merge_edges(forward, Edge, Rec, We0);
	Rec ->
	    try dissolve_edge_1(Edge, Rec, We0) of
		We -> We
	    catch
		throw:hole -> We0
	    end
    end.

%% dissolve_edge_1(Edge, EdgeRecord, We) -> We
%%  Remove an edge and a face. If one of the faces is degenerated
%%  (only consists of two edges), remove that one. If no face is
%%  degenerated, prefer to keep an invisible face (if an edge
%%  bordering a hole is dissolved, we except except the hole to
%%  expand). Otherwise, it does not matter which face we keep.
%%
dissolve_edge_1(Edge, #edge{lf=Remove,rf=Keep,ltpr=Same,ltsu=Same}=Rec, We) ->
    dissolve_edge_2(Edge, Remove, Keep, Rec, We);
dissolve_edge_1(Edge, #edge{lf=Keep,rf=Remove,rtpr=Same,rtsu=Same}=Rec, We) ->
    dissolve_edge_2(Edge, Remove, Keep, Rec, We);
dissolve_edge_1(Edge, #edge{lf=Lf,rf=Rf}=Rec, We) ->
    if
	Lf < 0 ->
	    %% Keep left face.
	    if
		Rf < 0 ->
		    %% The right face is also hidden. (Probably unusual
		    %% in practice.) It might also be a hole.
		    Holes = ordsets:del_element(Rf, We#we.holes),
		    dissolve_edge_2(Edge, Rf, Lf, Rec, We#we{holes=Holes});
		true ->
		    dissolve_edge_2(Edge, Rf, Lf, Rec, We)
	    end;
	Rf < 0 ->
	    %% Keep the right face. Remove the (visible) left face.
	    dissolve_edge_2(Edge, Lf, Rf, Rec, We);
	true ->
	    %% It does not matter which one we keep.
	    dissolve_edge_2(Edge, Rf, Lf, Rec, We)
    end.

dissolve_edge_2(Edge, FaceRemove, FaceKeep,
		#edge{vs=Va,ve=Vb,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS},
		#we{fs=Ftab0,es=Etab0,vc=Vct0,he=Htab0}=We0) ->
    %% First change the face for all edges surrounding the face we will remove.
    Etab1 = wings_face:fold(
	      fun (_, E, _, IntEtab) when E =:= Edge -> IntEtab;
		  (_, E, R, IntEtab) ->
		      case R of
			  #edge{lf=FaceRemove,rf=FaceKeep} ->
			      throw(hole);
			  #edge{rf=FaceRemove,lf=FaceKeep} ->
			      throw(hole);
			  #edge{lf=FaceRemove} ->
			      array:set(E, R#edge{lf=FaceKeep}, IntEtab);
			  #edge{rf=FaceRemove} ->
			      array:set(E, R#edge{rf=FaceKeep}, IntEtab)
		      end
	      end, Etab0, FaceRemove, We0),

    %% Patch all predecessors and successor of the edge we will remove.
    Etab2 = patch_edge(LP, RS, Edge, Etab1),
    Etab3 = patch_edge(LS, RP, Edge, Etab2),
    Etab4 = patch_edge(RP, LS, Edge, Etab3),
    Etab5 = patch_edge(RS, LP, Edge, Etab4),

    %% Remove the edge.
    Etab = array:reset(Edge, Etab5),
    Htab = hardness(Edge, soft, Htab0),

    %% Update the incident vertex table for both vertices
    %% to make sure they point to the correct existing edges.
    %% 
    %% We used to simply set the 'vc' field to 'undefined' to
    %% force a complete rebuild of the vertex table, but that
    %% could cause Extrude (for regions) to become slow for certain
    %% selection shapes, as the Extrude command internally does a
    %% collapse of one edge in a triangle face, which in turns causes
    %% a dissolve of one of the remaining edges.
    Vct = case Vct0 of
	      undefined ->
		  Vct0;
	      _ ->
		  %% For the vertices Va and Vb, pick one of the still existing
		  %% edges emanating from the vertex.
		  %%
		  %% The edges LS ('ltsu') and RP ('rtpr') emanate from Va ('vs').
		  %% The edges LP ('ltpr') and RS ('rtsu') emanate from Vb ('ve').
		  Vct1 = array:set(Va, LS, Vct0),
		  array:set(Vb, RS, Vct1)
	  end,

    %% Remove the face. Update the incident face to make sure
    %% the face points to an existing edge.
    Ftab1 = gb_trees:delete(FaceRemove, Ftab0),
    We1 = wings_facemat:delete_face(FaceRemove, We0),
    AnEdge = LP,
    Ftab = gb_trees:update(FaceKeep, AnEdge, Ftab1),

    %% Store all updated tables.
    We = We1#we{es=Etab,fs=Ftab,vc=Vct,he=Htab},

    %% If the kept face (FaceKeep) has become a two-edge face,
    %% we must get rid of that face by dissolving one of its edges.
    case array:get(AnEdge, Etab) of
	#edge{lf=FaceKeep,ltpr=Same,ltsu=Same} ->
	    internal_dissolve_edge(AnEdge, We);
	#edge{rf=FaceKeep,rtpr=Same,rtsu=Same} ->
	    internal_dissolve_edge(AnEdge, We);
	_Other ->
	    case wings_we:is_face_consistent(FaceKeep, We) of
		true ->
		    We;
		false ->
		    wings_u:error(?__(1,"Dissolving would cause a badly formed face."))
	    end
    end.

%% dissolve_isolated_vs([Vertex], We) -> We'
%%  Remove all isolated vertices ("winged vertices", or vertices
%%  having exactly two edges).
dissolve_isolated_vs([_|_]=Vs, We) ->
    dissolve_isolated_vs_1(Vs, We, []);
dissolve_isolated_vs([], We) -> We.

%% Since the dissolve operation will not keep the incident
%% edge table for vertices updated, we'll need to lookup
%% all incident edges now before we have started to dissolve.
dissolve_isolated_vs_1([V|Vs], #we{vc=Vct}=We, Acc) ->
    case array:get(V, Vct) of
	undefined ->
	    %% A previous pass has already removed this vertex.
	    dissolve_isolated_vs_1(Vs, We, Acc);
	Edge ->
	    dissolve_isolated_vs_1(Vs, We, [{V,Edge}|Acc])
    end;
dissolve_isolated_vs_1([], We, Vc) ->
    dissolve_isolated_vs_2(Vc, We, []).

%% Now do all dissolving.
dissolve_isolated_vs_2([{V,Edge}|T], We0, Acc) ->
    case dissolve_vertex(V, Edge, We0) of
	done -> dissolve_isolated_vs_2(T, We0, Acc);
	We -> dissolve_isolated_vs_2(T, We, [V|Acc])
    end;
dissolve_isolated_vs_2([], We, []) ->
    %% Nothing was done in the last pass. We don't need to do a vertex GC.
    We;
dissolve_isolated_vs_2([], We0, Vs) ->
    We = wings_we:rebuild(We0#we{vc=undefined}),

    %% Now do another pass over the vertices still in our list.
    %% Reason:
    %%
    %% 1. An incident edge may have become wrong by a previous
    %%    dissolve (on another vertex). Do another try now that
    %%    the incident table has been rebuilt.
    %%
    %% 2. A vertex may have be connected to two faces that
    %%    have no edge in common. In that case, all edges
    %%    are not reachable from the incident edge.
    dissolve_isolated_vs(Vs, We).

%% dissolve(V, Edge, We0) -> We|done
%%  Dissolve the given vertex. The 'done' return value means
%%  that the vertex is already non-existing (or is not isolated).
%%  If a We is returned, the caller must call this function again
%%  (after rebuilding the incident table) since there might be more
%%  work to do. 
dissolve_vertex(V, Edge, #we{es=Etab}=We0) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,ltsu=AnEdge,rtpr=AnEdge}=Rec ->
	    merge_edges(backward, Edge, Rec, We0);
	#edge{ve=V,rtsu=AnEdge,ltpr=AnEdge}=Rec ->
	    merge_edges(forward, Edge, Rec, We0);

	%% Handle the case that the incident edge is correct for
	%% the given vertex, but the vertex is NOT isolated.
	#edge{vs=V} -> done;
	#edge{ve=V} -> done;

	%% The incident edge is either non-existing or no longer
	%% references the given edge. In this case, we'll need
	%% to try dissolving the vertex again in the next
	%% pass after the incident table has been rebuilt.
	undefined -> We0;
	_ -> We0
    end.

%%
%% We like winged edges, but not winged vertices (a vertex with
%% only two edges connected to it). We will remove the winged vertex
%% by joining the two edges connected to it.
%%

merge_edges(Dir, Edge, Rec, #we{es=Etab}=We) ->
    {Va,Vb,_,_,To,To} = half_edge(Dir, Rec),
    case array:get(To, Etab) of
	#edge{vs=Va,ve=Vb} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	#edge{vs=Vb,ve=Va} ->
	    del_2edge_face(Dir, Edge, Rec, To, We);
	_Other ->
	    merge_1(Dir, Edge, Rec, To, We)
    end.

merge_1(Dir, Edge, Rec, To, #we{es=Etab0,fs=Ftab0,he=Htab0}=We0) ->
    OtherDir = reverse_dir(Dir),
    {Vkeep,Vdelete,Lf,Rf,L,R} = half_edge(OtherDir, Rec),
    Etab1 = patch_edge(L, To, Edge, Etab0),
    Etab2 = patch_edge(R, To, Edge, Etab1),
    Etab3 = patch_half_edge(To, Vkeep, Lf, L, Rf, R, Vdelete, Etab2),
    Htab = hardness(Edge, soft, Htab0),
    Etab = array:reset(Edge, Etab3),
    #edge{lf=Lf,rf=Rf} = Rec,
    Ftab1 = update_face(Lf, To, Edge, Ftab0),
    Ftab = update_face(Rf, To, Edge, Ftab1),
    We1 = We0#we{es=Etab,fs=Ftab,he=Htab,vc=undefined},
    We = case {wings_va:any_attributes(We1),Dir} of
	     {false,_} ->
		 We1;
	     {_,backward} ->
		 Attr = wings_va:edge_attrs(Edge, right, We0),
		 We2 = wings_va:set_edge_attrs(To, Rf, Attr, We1),
		 wings_va:del_edge_attrs(Edge, We2);
	     {_,forward} ->
		 Attr = wings_va:edge_attrs(Edge, left, We0),
		 We2 = wings_va:set_edge_attrs(To, Lf, Attr, We1),
		 wings_va:del_edge_attrs(Edge, We2)
	 end,
    merge_2(To, We).

merge_2(Edge, #we{es=Etab}=We) ->
    %% If the merged edge is part of a two-edge face, we must
    %% remove that edge too.
    case array:get(Edge, Etab) of
	#edge{ltpr=Same,ltsu=Same} ->
	    internal_dissolve_edge(Edge, We);
	#edge{rtpr=Same,rtsu=Same} ->
	    internal_dissolve_edge(Edge, We);
	_Other -> We
    end.

update_face(Face, Edge, OldEdge, Ftab) ->
    case gb_trees:get(Face, Ftab) of
	OldEdge -> gb_trees:update(Face, Edge, Ftab);
	_Other -> Ftab
    end.

del_2edge_face(Dir, EdgeA, RecA, EdgeB,
	       #we{es=Etab0,fs=Ftab0,he=Htab0,holes=Holes0}=We) ->
    {_,_,Lf,Rf,_,_} = half_edge(reverse_dir(Dir), RecA),
    RecB = array:get(EdgeB, Etab0),
    Del = gb_sets:from_list([EdgeA,EdgeB]),
    EdgeANear = stabile_neighbor(RecA, Del),
    EdgeBNear = stabile_neighbor(RecB, Del),
    Etab1 = patch_edge(EdgeANear, EdgeBNear, EdgeA, Etab0),
    Etab2 = patch_edge(EdgeBNear, EdgeANear, EdgeB, Etab1),
    Etab3 = array:reset(EdgeA, Etab2),
    Etab = array:reset(EdgeB, Etab3),

    %% Patch hardness table.
    Htab1 = hardness(EdgeA, soft, Htab0),
    Htab = hardness(EdgeB, soft, Htab1),

    %% Patch the face table.
    #edge{lf=Klf,rf=Krf} = array:get(EdgeANear, Etab),
    KeepFaces = ordsets:from_list([Klf,Krf]),
    EdgeAFaces = ordsets:from_list([Lf,Rf]),
    [DelFace] = ordsets:subtract(EdgeAFaces, KeepFaces),
    Ftab1 = gb_trees:delete(DelFace, Ftab0),
    [KeepFace] = ordsets:intersection(KeepFaces, EdgeAFaces),
    Ftab2 = update_face(KeepFace, EdgeANear, EdgeA, Ftab1),
    Ftab = update_face(KeepFace, EdgeBNear, EdgeB, Ftab2),

    %% It is probably unusual that 2 edge face is a hole,
    %% but better safe than sorry.
    Holes = ordsets:del_element(DelFace, Holes0),

    %% Return result.
    We#we{vc=undefined,es=Etab,fs=Ftab,he=Htab,holes=Holes}.

stabile_neighbor(#edge{ltpr=Ea,ltsu=Eb,rtpr=Ec,rtsu=Ed}, Del) ->
    [Edge] = foldl(fun(E, A) ->
			   case gb_sets:is_member(E, Del) of
			       true -> A;
			       false -> [E|A]
			   end
		   end, [], [Ea,Eb,Ec,Ed]),
    Edge.

%%%
%%% Setting hard/soft edges.
%%%

hardness(Edge, soft, Htab) -> gb_sets:delete_any(Edge, Htab);
hardness(Edge, hard, Htab) -> gb_sets:add(Edge, Htab).

%%%
%%% "Select faces on one side of an edge loop."
%%%
%%% This description is pretty ambigous. If there are
%%% multiple edge loops, it is not clear what to select.
%%%
%%% What we do for each object is to collect all faces
%%% sandwhiched between one or more edge loops. We then
%%% partition all those face collection into one partition
%%% for each sub-object (if there are any). For each
%%% sub-object, we arbitrarily pick the face collection
%%% having the smallest number of faces.
%%%

select_region(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_region/3, [], St),
    wings_sel:set(face, Sel, St);
select_region(St) -> St.

select_region(Edges0, #we{id=Id}=We, Acc) ->
    Part = wings_edge_loop:partition_edges(Edges0, We),
    Edges = select_region_borders(Edges0, We),
    FaceSel0 = select_region_1(Part, Edges, We, []),
    FaceSel = gb_sets:from_ordset(wings_we:visible(FaceSel0, We)),
    [{Id,FaceSel}|Acc].

select_region_1([[AnEdge|_]|Ps], Edges, #we{es=Etab}=We, Acc) ->
    #edge{lf=Lf,rf=Rf} = array:get(AnEdge, Etab),
    Left = collect_faces(Lf, Edges, We),
    Right = collect_faces(Rf, Edges, We),

    %% We'll let AnEdge identify the edge loop that each
    %% face collection borders to.
    select_region_1(Ps, Edges, We, [{Left,AnEdge},{Right,AnEdge}|Acc]);
select_region_1([], _Edges, _We, Acc) ->
    %% Now we have all collections of faces sandwhiched between
    %% one or more edge loops. Using the face collections as keys,
    %% we will partition the edge loop identifiers into groups.

    Rel0 = [{gb_sets:to_list(Set),Edge} || {Set,Edge} <- Acc],
    Rel = sofs:relation(Rel0),
    Fam = sofs:relation_to_family(Rel),
    DirectCs = sofs:to_external(sofs:range(Fam)),

    %% DirectCs now contains lists of edge loop identifiers that
    %% can reach each other through a collection of face.
    %% Using a digraph, partition edge loop into components
    %% (each edge loop in a component can reach any other edge loop
    %% directly or indirectly).

    G = digraph:new(),
    make_digraph(G, DirectCs),
    Cs = digraph_utils:components(G),
    digraph:delete(G),

    %% Now having the components, consisting of edge identifiers
    %% identifying the original edge loop, we now need to partition
    %% the actual collection of faces.

    PartKey0 = [[{K,sofs:from_term(F)} || K <- Ks] || [F|_]=Ks <- Cs],
    PartKey = gb_trees:from_orddict(sort(lists:append(PartKey0))),
    SetFun = fun(S) ->
		     {_,[E|_]} = sofs:to_external(S),
		     gb_trees:get(E, PartKey)
	     end,
    Part = sofs:to_external(sofs:partition(SetFun, Fam)),

    %% We finally have one partition for each sub-object.

    Sel = [select_region_2(P) || P <- Part],
    lists:merge(Sel).

select_region_2(P) ->
    case [Fs || {Fs,[_]} <- P] of
	[_|_]=Fss when length(Fss) < length(P) ->
	    lists:merge(Fss);
	_ ->
	    [{_,Fs}|_] = sort([{length(Fs),Fs} || {Fs,_} <- P]),
	    Fs
    end.

select_region_borders(Edges0, #we{mirror=Mirror,holes=Holes}=We) ->
    Bs = case Mirror of
	     none -> Holes;
	     _ -> [Mirror|Holes]
	 end,
    case Bs of
	[] ->
	    Edges0;
	[_|_] ->
	    BorderEdges = wings_face:to_edges(Bs, We),
	    gb_sets:union(gb_sets:from_list(BorderEdges), Edges0)
    end.

make_digraph(G, [Es|T]) ->
    make_digraph_1(G, Es),
    make_digraph(G, T);
make_digraph(_, []) -> ok.

make_digraph_1(G, [E]) ->
    digraph:add_vertex(G, E);
make_digraph_1(G, [E1|[E2|_]=Es]) ->
    digraph:add_vertex(G, E1),
    digraph:add_vertex(G, E2),
    digraph:add_edge(G, E1, E2),
    make_digraph_1(G, Es).

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
	true -> Acc0;
	false ->
	    {Face,Work1} = gb_sets:take_smallest(Work0),
	    Acc = gb_sets:insert(Face, Acc0),
	    Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
	    collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(_, Edge, Rec, A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      Of = wings_face:other(Face, Rec),
		      case gb_sets:is_member(Of, Res) of
			  true -> A;
			  false -> gb_sets:add(Of, A)
		      end
	      end
      end, Work, Face, We).

%%%
%%% Edge Ring. (Based on Anders Conradi's plug-in.)
%%%

select_edge_ring(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun build_selection/3, [], St),
    wings_sel:set(Sel, St);
select_edge_ring(St) -> St.

select_edge_ring_incr(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun incr_ring_selection/3, [], St),
    wings_sel:set(Sel, St);
select_edge_ring_incr(St) -> St.

select_edge_ring_decr(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun decr_ring_selection/3, [], St),
    wings_sel:set(Sel, St);
select_edge_ring_decr(St) -> St.

-record(r,{id,l,r,
	   ls=gb_sets:empty(),
	   rs=gb_sets:empty()}).

build_selection(Edges, #we{id=Id}=We, ObjAcc) ->
    Init = init_edge_ring([],unknown,Edges,We,0,[]),
    Stops0 = foldl(fun(#r{id=MyId,ls=O},S0) ->
			   gb_sets:fold(fun(E,S) -> [{E,MyId} | S] end,
					S0, O)
		   end,[],Init),
    Stop = gb_trees:from_orddict(lists:sort(Stops0)),
    Sel0 = grow_rings(Init,[],Stop,We,gb_sets:empty()),
    Sel = wings_we:visible_edges(Sel0, We),
    [{Id,gb_sets:union(Sel,Edges)}|ObjAcc].

grow_rings([First = #r{id=This}|R0],Rest0,Stop,We,Acc) ->
    case grow_ring1(First,Stop,We) of
	{stop, This, Edges} ->
	    grow_rings(R0,Rest0,Stop,We,gb_sets:union(Edges,Acc));
	{stop, Id, Edges} ->
	    R = lists:keydelete(Id,2,R0),
	    Rest = lists:keydelete(Id,2,Rest0),
	    grow_rings(R,Rest,Stop,We,gb_sets:union(Edges,Acc));
	{cont,New} ->
	    grow_rings(R0,[New|Rest0],Stop,We,Acc)
    end;
grow_rings([],[],_,_,Acc) -> Acc;
grow_rings([],Rest,Stop,We,Acc) ->
    grow_rings(Rest, [], Stop, We, Acc).

grow_ring1(#r{id=Id,l=unknown,r=unknown,ls=LS,rs=RS},_Stop,_We) ->
    {stop, Id, gb_sets:union(LS,RS)};
grow_ring1(This = #r{id=ID,l=L0,ls=LS0,r=R0,rs=RS0},Stop,We) ->
    case grow_ring2(ID,L0,LS0,Stop,We) of
	{L,LS} ->
	    case grow_ring2(ID,R0,RS0,Stop,We) of
		{R,RS} -> {cont,This#r{l=L,ls=LS,r=R,rs=RS}};
		Break ->  Break
	    end;
	Break -> Break
    end.

grow_ring2(ID,Edge,Edges,Stop,We) ->
    case grow_ring3(Edge,Edges,Stop,We) of
	{stop, ID, Edges} ->  {unknown,Edges};
	Else ->   Else
    end.

grow_ring3(unknown,Edges,_Stop,_We) ->
    {unknown,Edges};
grow_ring3(Edge,Edges,Stop,We) ->
    case gb_trees:lookup(Edge,Stop) of
	{value,Id} -> 
	    {stop,Id,Edges};
	none -> 
	    Left = opposing_edge(Edge, We, left),
	    case gb_sets:is_member(Left,Edges) of
		false ->
		    {Left,gb_sets:add(Edge,Edges)};
		true ->
		    Right = opposing_edge(Edge, We, right),
		    case gb_sets:is_member(Right,Edges) of
			true ->  {unknown, Edges};
			false -> {Right,gb_sets:add(Edge,Edges)}
		    end
	    end
    end.

init_edge_ring([],unknown,Edges0,We,Id,Acc) -> 
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges} = gb_sets:take_smallest(Edges0),
	    Left = opposing_edge(Edge, We, left),
	    Right = opposing_edge(Edge, We, right),
	    init_edge_ring([Left,Right],#r{id=Id,l=Edge,r=Edge},Edges,We,Id+1,Acc)
    end;
init_edge_ring([],EI = #r{ls=LS},Edges0,We,Id,Acc) ->
    init_edge_ring([],unknown,Edges0,We,Id,[EI#r{rs=LS}|Acc]);
init_edge_ring([unknown|Rest],EI,Edges0,We,Id,Acc) ->
    init_edge_ring(Rest,EI,Edges0,We,Id,Acc);
init_edge_ring([Edge|Rest],EI0,Edges0,We,Id,Acc) ->
    case gb_sets:is_member(Edge,Edges0) of
	true -> 
	    {Next,EI}=replace_edge(Edge,EI0,We),
	    init_edge_ring([Next|Rest],EI,gb_sets:delete(Edge,Edges0),We,Id,Acc);
	false ->
	    {_Next,EI}=replace_edge(Edge,EI0,We),
	    init_edge_ring(Rest,EI,Edges0,We,Id,Acc)
    end.

replace_edge(Edge,#r{l=L,r=R,ls=O} = EI,We) ->
    case opposing_edge(Edge,We,left) of
	L -> {opposing_edge(Edge,We,right),EI#r{l=Edge,ls=gb_sets:add(L,O)}};
	R -> {opposing_edge(Edge,We,right),EI#r{r=Edge,ls=gb_sets:add(R,O)}};
	unknown ->
	    case opposing_edge(Edge,We,right) of
		L -> {unknown, EI#r{l=Edge,ls=gb_sets:add(L,O)}};
		R -> {unknown, EI#r{r=Edge,ls=gb_sets:add(R,O)}}
	    end;
	Other -> 
	    case opposing_edge(Edge,We,right) of
		L -> {Other, EI#r{l=Edge,ls=gb_sets:add(L,O)}};
		R -> {Other, EI#r{r=Edge,ls=gb_sets:add(R,O)}}
	    end
    end.

opposing_edge(Edge, #we{es=Es}=We, Side) ->
    #edge{lf=Left,rf=Right} = array:get(Edge, Es),
    Face = case Side of
               left -> Left;
               right -> Right
           end,
    %% Get opposing edge or fail.
    case wings_face:vertices(Face, We) of
        4 -> next_edge(next_edge(Edge, Face, We), Face, We);
        _ -> unknown
    end.

next_edge(Edge, Face, #we{es=Etab})->
    case array:get(Edge, Etab) of
        #edge{lf=Face,ltsu=NextEdge} -> NextEdge;
        #edge{rf=Face,rtsu=NextEdge} -> NextEdge
    end.

incr_ring_selection(Edges, #we{id=Id}=We, ObjAcc) ->
    [{Id,gb_sets:fold(
	   fun(Edge, EdgeAcc) ->
		   Es = incr_from_edge(Edge, We, EdgeAcc),
		   wings_we:visible_edges(Es, We)
	   end, gb_sets:empty(), Edges)}|ObjAcc].

incr_from_edge(Edge, We, Acc) ->
    Selected = gb_sets:add(Edge, Acc),
    LeftSet =
	case opposing_edge(Edge, We, left) of
	    unknown -> Selected;
	    Left -> gb_sets:add(Left, Selected)
	end,
    case opposing_edge(Edge, We, right) of
	unknown -> LeftSet;
	Right -> gb_sets:add(Right, LeftSet)
    end.

decr_ring_selection(Edges, #we{id=Id} = We, ObjAcc) ->
    [{Id,gb_sets:fold(
	   fun(Edge, EdgeAcc) ->
		   decr_from_edge(Edge, We, Edges, EdgeAcc)
	   end, Edges, Edges)}|ObjAcc].

decr_from_edge(Edge, We, Orig, Acc) ->
    Left = opposing_edge(Edge, We, left),
    Right =  opposing_edge(Edge, We, right),
    case (Left == unknown) or (Right == unknown) of
	true ->
	    gb_sets:delete(Edge,Acc);
	false ->
	    case gb_sets:is_member(Left, Orig) and
		gb_sets:is_member(Right, Orig) of
		true ->
		    Acc;
		false ->
		    gb_sets:delete(Edge, Acc)
	    end
    end.

%%%
%%% Utilities.
%%%

reverse_dir(forward) -> backward;
reverse_dir(backward) -> forward.

half_edge(backward, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltsu=L,rtpr=R}) ->
    {Va,Vb,Lf,Rf,L,R};
half_edge(forward, #edge{ve=Va,vs=Vb,lf=Lf,rf=Rf,ltpr=L,rtsu=R}) ->
    {Va,Vb,Lf,Rf,L,R}.

patch_half_edge(Edge, V, FaceA, Ea, FaceB, Eb, OrigV, Etab) ->
    New = case array:get(Edge, Etab) of
	      #edge{vs=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{vs=V,ltsu=Ea,rtpr=Eb};
	      #edge{vs=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{vs=V,ltsu=Eb,rtpr=Ea};
	      #edge{ve=OrigV,lf=FaceA,rf=FaceB}=Rec ->
		  Rec#edge{ve=V,ltpr=Ea,rtsu=Eb};
	      #edge{ve=OrigV,lf=FaceB,rf=FaceA}=Rec ->
		  Rec#edge{ve=V,ltpr=Eb,rtsu=Ea}
	  end,
    array:set(Edge, New, Etab).

patch_edge(Edge, ToEdge, OrigEdge, Etab) ->
    New = case array:get(Edge, Etab) of
	      #edge{ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    array:set(Edge, New, Etab).

patch_edge(Edge, ToEdge, Face, OrigEdge, Etab) ->
    New = case array:get(Edge, Etab) of
	      #edge{lf=Face,ltsu=OrigEdge}=R ->
		  R#edge{ltsu=ToEdge};
	      #edge{lf=Face,ltpr=OrigEdge}=R ->
		  R#edge{ltpr=ToEdge};
	      #edge{rf=Face,rtsu=OrigEdge}=R ->
		  R#edge{rtsu=ToEdge};
	      #edge{rf=Face,rtpr=OrigEdge}=R ->
		  R#edge{rtpr=ToEdge}
	  end,
    array:set(Edge, New, Etab).

%%%% Select every nth ring
select_nth_ring(N, #st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun(Edges, #we{id=Id}=We, ObjAcc) ->
                EdgeRings = nth_ring_1(Edges, {N,N}, We, Edges, gb_sets:new()),
                Sel0 = wings_we:visible_edges(EdgeRings, We),
                [{Id,Sel0}|ObjAcc]
        end,[],St),
    wings_sel:set(Sel, St);
select_nth_ring(_N, St) ->
    St.

nth_ring_1(Edges0, N, We, OrigEs, Acc) ->
    case gb_sets:is_empty(Edges0) of
      true -> Acc;
      false ->
        {Edge,Edges1} = gb_sets:take_smallest(Edges0),
        Rings = nth_ring_2(Edge, N, We, OrigEs, gb_sets:singleton(Edge)),
        Edges = gb_sets:subtract(Edges1, Rings),
        nth_ring_1(Edges, N, We, OrigEs, gb_sets:union(Rings,Acc))
    end.

nth_ring_2(Edge, {N,Int}, We, OrigEs, Acc) ->
    case opposing_edge(Edge, We, left) of
      unknown ->
        case opposing_edge(Edge, We, right) of
          unknown ->
            Acc;
          NextEdge ->
            {_,Edges0} = nth_ring_3(NextEdge,Edge,Edge,{N-1,Int},right,left,We,OrigEs,Acc),
            Edges0
        end;
      NextEdge ->
        {Check0,Edges0} = case gb_sets:is_member(NextEdge,OrigEs) of
          true -> {stop,Acc};
          false -> nth_ring_3(NextEdge,Edge,Edge,{N-1,Int},left,right,We,OrigEs,Acc)
        end,
        case opposing_edge(Edge, We, right) of
          unknown ->
            Edges0;
          PrevEdge ->
            {Check1,Edges1} = case gb_sets:is_member(PrevEdge,OrigEs) of
              true -> {stop,Acc};
              false -> nth_ring_3(PrevEdge,Edge,Edge,{N-1,Int},right,left,We,OrigEs,Acc)
            end,
            nth_ring_4(Check0,Edges0,Check1,Edges1,OrigEs)
        end
    end.

nth_ring_3(CurEdge,PrevEdge,LastEdge,{0,N},Side,Oposite,We,OrigEs,Acc0) ->
    Acc = gb_sets:insert(CurEdge,Acc0),
    nth_ring_3(CurEdge,PrevEdge,LastEdge,{N,N},Side,Oposite,We,OrigEs,Acc);
nth_ring_3(CurEdge,PrevEdge,LastEdge,{N0,Int},Side,Oposite,We,OrigEs,Acc) ->
    case opposing_edge(CurEdge, We, Side) of
      unknown -> {ok,Acc};
      PrevEdge ->
        case opposing_edge(CurEdge, We, Oposite) of
          unknown -> {ok,Acc};
          PrevEdge -> {ok,Acc};
          LastEdge -> {ok,Acc};
          NextEdge ->
            case gb_sets:is_member(NextEdge,OrigEs) of
              true ->
                {stop,Acc};
              false ->
                nth_ring_3(NextEdge,CurEdge,LastEdge,{N0-1,Int},Oposite,Side,We,OrigEs,Acc)
            end
        end;
      LastEdge -> {ok,Acc};
      NextEdge ->
        case gb_sets:is_member(NextEdge,OrigEs) of
          true ->
            {stop,Acc};
          false ->
            nth_ring_3(NextEdge,CurEdge,LastEdge,{N0-1,Int},Side,Oposite,We,OrigEs,Acc)
        end
    end.

nth_ring_4(_,Edges,_,Edges,_) ->
    Edges;
nth_ring_4(Check0,Edges0,Check1,Edges1,OrigEs) ->
    S0 = gb_sets:size(Edges0),
    S1 = gb_sets:size(Edges1),
    if
      S0 =:= 1 -> Edges1;
      S1 =:= 1 -> Edges0;
      true -> nth_ring_5(Check0,S0,Edges0,Check1,S1,Edges1,OrigEs)
    end.

nth_ring_5(ok,_,Edges0,ok,_,Edges1,_) ->
    gb_sets:union(Edges0,Edges1);
nth_ring_5(stop,S0,Edges0,stop,S1,Edges1,_) ->
    case S0 > S1 of
      true -> Edges1;
      false -> Edges0
    end;
nth_ring_5(stop,_,Edges0,_,_,Edges1,OrigEs) ->
    case gb_sets:is_subset(Edges0,OrigEs) of
      true -> Edges1;
      false -> Edges0
    end;
nth_ring_5(_,_,Edges0,stop,_,Edges1,OrigEs) ->
    case gb_sets:is_subset(Edges1,OrigEs) of
      true -> Edges0;
      false -> Edges1
    end.
