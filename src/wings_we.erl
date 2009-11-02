%%
%%  wings_we.erl --
%%
%%     This module contains functions to build and manipulate
%%     we records (winged-edged records, the central data structure
%%     in Wings 3D).
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_we).
-export([build/2,rebuild/1,fast_rebuild/1,
	 new_wrap_range/3,id/2,bump_id/1,
	 new_id/1,new_ids/2,
	 invert_normals/1,
	 merge/1,merge/2,
	 renumber/2,renumber/3,
	 uv_to_color/2,
	 uv_mapped_faces/1,
	 transform_vs/2,
	 separate/1,
	 normals/3,
	 new_items_as_ordset/3,new_items_as_gbset/3,
	 is_consistent/1,is_face_consistent/2,
	 hide_faces/2,show_faces/1,num_hidden/1,
	 create_holes/2,show_faces/2,
	 is_open/1,all_hidden/1,
	 visible/1,visible/2,visible_vs/1,visible_vs/2,
	 visible_edges/1,visible_edges/2,fully_visible_edges/2,
	 validate_mirror/1,mirror_flatten/2,mirror_projection/1,
	 create_mirror/2,freeze_mirror/1,break_mirror/1]).

-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [foreach/2,foldl/3,sort/1,keysort/2,reverse/1,zip/2,partition/2]).
-import(erlang, [max/2]).

%%%
%%% API.
%%%

build(Mode, #e3d_mesh{fs=Fs0,vs=Vs,tx=Tx,he=He}) when is_atom(Mode) ->
    Fs = translate_faces(Fs0, list_to_tuple(Tx), []),
    wings_we_build:we(Fs, Vs, He);
build(Fs, Vs) ->
    wings_we_build:we(Fs, Vs, []).

%% rebuild(We) -> We'
%%  Rebuild any missing 'vc' and 'fs' tables. Also remove any
%%  unused entries in the 'vp' table. Update the 'next_id' field.
%%
rebuild(#we{vc=undefined,fs=undefined,es=Etab0,holes=Holes0}=We0) ->
    Etab = array:sparse_to_orddict(Etab0),
    Ftab = rebuild_ftab(Etab),
    Holes = ordsets:intersection(gb_trees:keys(Ftab), Holes0),
    VctList = rebuild_vct(Etab),
    We = We0#we{vc=array:from_orddict(VctList),fs=Ftab,holes=Holes},
    rebuild_1(VctList, We);
rebuild(#we{vc=undefined,es=Etab}=We) ->
    VctList = rebuild_vct(array:sparse_to_orddict(Etab), []),
    rebuild_1(VctList, We#we{vc=array:from_orddict(VctList)});
rebuild(#we{fs=undefined,es=Etab,holes=Holes0}=We) ->
    Ftab = rebuild_ftab(array:sparse_to_orddict(Etab)),
    Holes = ordsets:intersection(gb_trees:keys(Ftab), Holes0),
    rebuild(We#we{fs=Ftab,holes=Holes});
rebuild(We) -> update_id_bounds(We).

%% fast_rebuild(We) -> We'
%%  Unconditionally rebuild the 'vc' and 'fs' tables. Do not
%%  update the 'next_id' field and do not GC away unused positions
%%  in the 'vp' table.
%%
fast_rebuild(#we{es=Etab0}=We) ->
    Etab = array:sparse_to_orddict(Etab0),
    Ftab = rebuild_ftab(Etab),
    Vct = array:from_orddict(rebuild_vct(Etab)),
    We#we{vc=Vct,fs=Ftab}.

%%% Utilities for allocating IDs.

new_wrap_range(Items, Inc, #we{next_id=Id}=We) ->
    NumIds = Items*Inc,
    {{0,Id,Inc,NumIds},We#we{next_id=Id+NumIds}}.

id(N, {Current,BaseId,_Inc,NumIds}) ->
    BaseId + ((Current+N) rem NumIds).

bump_id({Id,BaseId,Inc,NumIds}) ->
    {Id+Inc,BaseId,Inc,NumIds}.

new_id(#we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+1}}.

new_ids(N, #we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+N}}.

%%% Returns sets of newly created items.

%% new_items_as_ordset(vertex|edge|face, OldWe, NewWe) -> NewItemsSet.
%% new_items_as_gbset(vertex|edge|face, OldWe, NewWe) -> NewItemsSet.
%%  Return all items in NewWe that are not in OldWe.

new_items_as_gbset(Type, OldWe, NewWe) ->
    gb_sets:from_ordset(new_items_as_ordset(Type, OldWe, NewWe)).

new_items_as_ordset(vertex, #we{next_id=Wid}, #we{next_id=NewWid,vp=Tab}) ->
    new_array_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(edge, #we{next_id=Wid}, #we{next_id=NewWid,es=Tab}) ->
    new_array_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(face, #we{next_id=Wid}, #we{next_id=NewWid,fs=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid).

%%% Hiding/showing faces.

hide_faces(Fs, We) when is_list(Fs) ->
    hide_faces_1(gb_sets:from_list(Fs), We);
hide_faces(Fs, We) ->
    hide_faces_1(Fs, We).

%% show_faces(We0) -> We
%%  Show all faces previously hidden by the user (not including
%%  holes hidden by Face|Create Hole).
%%
show_faces(#we{mirror=Face}=We) ->
    case is_open(We) of
	false ->
	    We;
	true ->
	    #we{fs=Ftab,holes=Holes} = We,
	    Hidden = [F || F <- gb_trees:keys(Ftab), F < 0],
	    Unhide = ordsets:subtract(Hidden, Holes) -- [Face],
	    show_faces_1(Unhide, We)
    end.

%% show_faces(Faces, We0) -> We
%%  Show the faces in given in the list Faces. The list must contain
%%  the face numbers in their hidden form (i.e. negative).
%%
%%  The #we.holes list is neither used nor updated.
%%
show_faces(Faces, We) ->
    show_faces_1(Faces, We).

num_hidden(#we{fs=Ftab}=We) ->
    case is_open(We) of
	false -> 0;
	true -> num_hidden_1(gb_trees:keys(Ftab), 0)
    end.

%% is_open(We) -> true|false
%%  Return true if the object has a hidden face or hole through
%%  which the inside of the object can be seen.
%%
is_open(#we{mirror=none}=We) ->
    any_invisible_faces(We);
is_open(#we{fs=Ftab0,mirror=Mirror}=We) ->
    Ftab = gb_trees:delete(Mirror, Ftab0),
    any_invisible_faces(We#we{fs=Ftab}).

%% all_hidden(We) -> true|false
%%  Return true if all faces in the object are hidden.
%%
all_hidden(#we{fs=Ftab}) ->
    not gb_trees:is_empty(Ftab) andalso
	wings_util:gb_trees_largest_key(Ftab) < 0.

%% create_holes([Face], We0) -> We
%%  Mark the given faces as holes and hide them.
%%
create_holes(NewHoles, #we{holes=Holes0}=We) ->
    %% This code is complicated because some of the faces
    %% in the NewHoles list may already be hidden (i.e. negative).
    {ToHide,AlreadyHidden} = partition(fun(F) -> F >= 0 end, NewHoles),
    NewHiddenHoles = ordsets:from_list([-F-1 || F <- ToHide]),
    Holes = ordsets:union([NewHiddenHoles,Holes0,AlreadyHidden]),
    hide_faces(ToHide, We#we{holes=Holes}).

%% visible(We) -> [Face]
%%  Return a list of all visible faces in the object.
%%
visible(#we{fs=Ftab}) ->
    visible_2(gb_trees:keys(Ftab)).

%% visible([{Face,Any}], We) -> [{Face,Any}];
%% visible([Face], We) -> [Face].
%%  Filter the ordered list of Face or {Face,Edge} tuples to only
%%  contain visible faces.
%%
visible([{_,_}|_]=Fs, #we{}) -> visible_1(Fs);
visible([_|_]=Fs, #we{}) -> visible_2(Fs);
visible([], #we{}) -> [].

%% visible_vs(We) -> [Vertex]
%%  Return a list of all visible vertices.
%%
visible_vs(#we{vc=Vct}=We) ->
    case is_open(We) of
	false -> wings_util:array_keys(Vct);
	true -> visible_vs_1(We)
    end.

%% visible_vs([Vertex], We) -> [Vertex];
%% visible_vs([{Vertex,Data}], We) -> [{Vertex,Data}].
%%  Filter the list of vertices to only include the visible vertices.
%%
visible_vs(Vs, We) ->
    case is_open(We) of
	false -> Vs;
	true ->
	    Vis0 = visible_vs_1(We),
	    case Vs of
		[{_,_}|_] ->
		    VsSet = sofs:relation(Vs),
		    VisSet = sofs:from_external(Vis0, [atom]),
		    sofs:to_external(sofs:restriction(VsSet, VisSet));
		[_|_] ->
		    ordsets:intersection(Vis0, Vs);
		[] ->
		    []
	    end
    end.

%% visible_edges(We) -> [Edge]
%%  Return a list of all edges that have a visible face
%%  on at least one side.
%%
visible_edges(#we{es=Etab}=We) ->
    case is_open(We) of
	false -> wings_util:array_keys(Etab);
	true -> visible_es_1(We)
    end.

%% renumber(We0, Start) -> We
%%     Start = integer, >= 0
%%  Renumber all vertex, edge, and face identifiers to consecutive
%%  numbers starting at Start. Hidden faces will be assigned the
%%  lowest numbers.
%%
%%  As long as hidden faces are implemented as negative face numbers,
%%  renumbering will force all hidden faces (including holes) to become
%%  visible.
%%
renumber(We0, Id) ->
    We = do_renumber(We0, Id),
    rebuild(We).

%% renumber(We0, Start, RootSet0) -> {We,RootSet}
%%     Start = integer, >= 0
%%     RootSet = [{vertex,V}|{edge,E}|{face,F}]
%%  Renumber all vertex, edge, and face identifiers to consecutive
%%  exactly as renumber/2, but also renumber all identifiers in the
%%  given root set.
%%
renumber(We0, Id, RootSet0) ->
    {We,RootSet} = do_renumber(We0, Id, RootSet0),
    {rebuild(We),RootSet}.

%% fully_visible_edges(OrderedEdgeSet0, We) -> FullyVisibleEdges
%%     OrderedEdgeSet = An ordered list of edges.
%%  Filter an ordered list of edges, removing any edge from the list
%%  that do not have visible faces on both sides.
%%
fully_visible_edges(Es, #we{es=Etab}=We) ->
    case any_invisible_faces(We) of
	false -> Es;
	true -> fully_visible_edges_1(Es, Etab)
    end.

%% validate_mirror(We0) -> We
%%  Reset the virtual mirror face if it refers to a
%%  non-existing face.
%%
validate_mirror(#we{mirror=none}=We) -> We;
validate_mirror(#we{fs=Ftab,mirror=Face}=We) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> We#we{mirror=none};
	true -> We
    end.

%% mirror_flatten(OldWe, We0) -> We
%%  Project mirror vertices in We0 to the virtual mirror plane
%%  defined by the virtual mirror in OldWe.
%%
mirror_flatten(OldWe, #we{mirror=Face,vp=Vtab0}=We) ->
    case mirror_projection(OldWe) of
	identity ->
	    We;
	Flatten ->
	    Vtab = foldl(fun(V, Vt) ->
				 Pos0 = array:get(V, Vt),
				 Pos = e3d_mat:mul_point(Flatten, Pos0),
				 array:set(V, Pos, Vt)
			 end, Vtab0, wings_face:vertices_ccw(Face, We)),
	    We#we{vp=Vtab}
    end.

%% mirror_projection(We) -> Matrix | 'identity'
%%  If there is a virtual mirror for We, return a matrix that
%%  projects points to the mirror plane. Otherwise return
%%  'identity'.
%%
-spec mirror_projection(#we{}) -> e3d_matrix().
mirror_projection(#we{mirror=none}) ->
    identity;
mirror_projection(#we{mirror=Face}=We) ->
    PlaneNormal = wings_face:normal(Face, We),
    FaceVs = wings_face:to_vertices([Face], We),
    Origin = wings_vertex:center(FaceVs, We),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))).

%% create_mirror(Face, We0) -> We
%%  Make face Face the virtual mirror face for object We0.
%%
create_mirror(Face, We0) when Face >= 0 ->
    We1 = hide_faces([Face], We0),
    We = break_mirror(We1),
    We#we{mirror=-Face-1}.

%% freeze_mirror(We0) -> We
%%  Freeze the virtual mirror (if any) for the object We0.
%%
freeze_mirror(#we{mirror=none}=We) -> We;
freeze_mirror(#we{mirror=Face}=We) ->
    wings_face_cmd:mirror_faces([Face], We#we{mirror=none}).

%% break_mirror(We0) -> We
%%  Break the virtual mirror (if any) for the object We0.
%%
break_mirror(#we{mirror=none}=We) -> We;
break_mirror(#we{mirror=Face}=We0) ->
    show_faces([Face], We0#we{mirror=none}).

%% merge(We0, We1) -> We
%%  Merge two winged-edge structures. See merge/1.
%%
merge(We0, We1) ->
    merge([We0,We1]).

%% merge([We0]) -> We
%%  Merge a list of winged-edge structures.
%%
%%  Holes will be automatically re-hidden in the combined
%%  #we{} record, but invisible faces may become visible.
%%
merge([]) -> [];
merge([We]) -> We;
merge([#we{id=Id,name=Name}|_]=Wes0) ->
    Wes1 = [break_mirror(We) || We <- Wes0],
    Wes = merge_renumber(Wes1),
    Pst = merge_plugins(Wes),
    MatTab = wings_facemat:merge(Wes),
    {Vpt0,Et0,Ht0,Holes} = merge_1(Wes),
    Vpt = array:from_orddict(Vpt0),
    Et = array:from_orddict(Et0),
    Ht = gb_sets:from_ordset(Ht0),
    We0 = rebuild(#we{id=Id,name=Name,vc=undefined,fs=undefined,
		      pst=Pst,vp=Vpt,es=Et,he=Ht,mat=MatTab,
		      holes=Holes}),
    case wings_va:merge(Wes, We0) of
	#we{holes=[]}=We ->
	    We;
	#we{holes=Holes}=We ->
	    %% Re-hide all holes that have become visible.
	    create_holes(Holes, We#we{holes=[]})
    end.

%%%
%%% Local functions.
%%%

rebuild_1(VctList, #we{vp=Vtab0}=We) ->
    Vtab1 = [{V,array:get(V, Vtab0)} || {V,_} <- VctList],
    Vtab = array:from_orddict(Vtab1),
    rebuild(We#we{vp=Vtab}).

rebuild_vct(Es) ->
    rebuild_vct(Es, []).

rebuild_vct([{Edge,#edge{vs=Va,ve=Vb}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Va, Vb, Edge, Acc0),
    rebuild_vct(Es, Acc);
rebuild_vct([], VtoE) ->
    wings_we_build:incident_tab(VtoE).

rebuild_ftab(Es) ->
    rebuild_ftab_1(Es, []).

rebuild_ftab_1([{Edge,#edge{lf=Lf,rf=Rf}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Lf, Rf, Edge, Acc0),
    rebuild_ftab_1(Es, Acc);
rebuild_ftab_1([], FtoE) ->
    gb_trees:from_orddict(wings_we_build:incident_tab(FtoE)).

rebuild_maybe_add(Ka, Kb, E, [_,{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [_,{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, Acc) ->
    [{Ka,E},{Kb,E}|Acc].


%%%
%%% Handling of hidden faces.
%%%

%% any_invisible_faces(We) -> true|false.
%%  Check whether there are any invisible faces (hidden
%%  faces, holes, or virtual mirror face).
%%
any_invisible_faces(#we{fs=Ftab}) ->
    not gb_trees:is_empty(Ftab) andalso
	wings_util:gb_trees_smallest_key(Ftab) < 0.

hide_faces_1(Fs, #we{es=Etab0}=We0) ->
    Map = fun(_, #edge{lf=Lf0,rf=Rf0}=R0) ->
		  Lf = hide_map_face(Lf0, Fs),
		  Rf = hide_map_face(Rf0, Fs),
		  case R0#edge{lf=Lf,rf=Rf} of
		      R0 -> R0;
		      R -> R
		  end
	  end,
    Etab = array:sparse_map(Map, Etab0),
    We = We0#we{es=Etab,fs=undefined},
    wings_facemat:hide_faces(rebuild(We)).

hide_map_face(F, Fs) ->
    case gb_sets:is_member(F, Fs) of
	false -> F;
	true -> -F-1
    end.

num_hidden_1([F|Fs], N) when F < 0 ->
    num_hidden_1(Fs, N+1);
num_hidden_1(_, N) -> N.
	    
visible_1([{F,_}|Fs]) when F < 0 -> visible_1(Fs);
visible_1(Fs) -> Fs.

visible_2([F|Fs]) when F < 0 -> visible_2(Fs);
visible_2(Fs) -> Fs.

visible_vs_1(#we{es=Etab}) ->
    Vs = array:sparse_foldl(
	   fun(_, #edge{lf=Lf,rf=Rf}, A) when Lf < 0, Rf < 0 ->
		   A;
	      (_, #edge{vs=Va,ve=Vb}, A) ->
		   [Va,Vb|A]
	   end, [], Etab),
    ordsets:from_list(Vs).

visible_es_1(#we{es=Etab}) ->
    Es = array:sparse_foldl(
	   fun(_, #edge{lf=Lf,rf=Rf}, A) when Lf < 0, Rf < 0 ->
		   A;
	      (E, _, A) ->
		   [E|A]
	   end, [], Etab),
    ordsets:from_list(Es).

visible_edges(Es, We) ->
    case is_open(We) of
	false -> Es;
	true ->
	    Vis0 = visible_edges(We),
	    if
		is_list(Es) ->
		    ordsets:intersection(Vis0, Es);
		true ->
		    Vis = gb_sets:from_ordset(Vis0),
		    gb_sets:intersection(Vis, Es)
	    end
    end.

fully_visible_edges_1([E|Es], Etab) ->
    case array:get(E, Etab) of
	#edge{lf=Lf,rf=Rf} when Lf < 0; Rf < 0 ->
	    fully_visible_edges_1(Es, Etab);
	_ ->
	    [E|fully_visible_edges_1(Es, Etab)]
    end;
fully_visible_edges_1([], _) -> [].

show_faces_1(Faces, #we{es=Etab0}=We0) ->
    Show = fun(Face, _, E, _, Et) ->
		   R = case array:get(E, Et) of
			   #edge{lf=Face}=R0 ->
			       R0#edge{lf=-Face-1};
			   #edge{rf=Face}=R0 ->
			       R0#edge{rf=-Face-1}
		       end,
		   array:set(E, R, Et)
	   end,
    Etab = wings_face:fold_faces(Show, Etab0, Faces, We0),
    We = We0#we{es=Etab,fs=undefined},
    wings_facemat:show_faces(Faces, rebuild(We)).

%%%
%%% Build Winged-Edges.
%%%

translate_faces([#e3d_face{vs=Vs,tx=Tx0,mat=Mat0}|Fs], Txs, Acc) ->
    Mat = translate_mat(Mat0),
    FaceData = case Tx0 of
		   [] -> {Mat,Vs};
		   Tx1 ->
		       Tx = [element(Tx+1, Txs) || Tx <- Tx1],
		       {Mat,Vs,Tx}
	       end,
    translate_faces(Fs, Txs, [FaceData|Acc]);
translate_faces([], _, Acc) -> reverse(Acc).

translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.

%%% Invert all normals.

invert_normals(#we{es=Etab0}=We0) ->
    Etab1 = invert_edges(array:sparse_to_orddict(Etab0), []),
    Etab = array:from_orddict(Etab1),
    We = We0#we{es=Etab},
    slide_colors(We).

invert_edges([{Edge,Rec0}|Es], Acc) ->
    #edge{vs=Vs,ve=Ve,ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Rec0,
    Rec = Rec0#edge{vs=Ve,ve=Vs,ltpr=Ltsu,ltsu=Ltpr,rtpr=Rtsu,rtsu=Rtpr},
    invert_edges(Es, [{Edge,Rec}|Acc]);
invert_edges([], Acc) -> reverse(Acc).

slide_colors(#we{fs=Ftab}=We) ->
    foldl(fun({Face,Edge}, W) ->
		  slide_colors(Face, Edge, We, W)
	  end, We, gb_trees:to_list(Ftab)).

slide_colors(Face, Edge, OrigWe, #we{es=Etab}=We) ->
    PrevEdge = case array:get(Edge, Etab) of
		   #edge{lf=Face,ltsu=Pe0} -> Pe0;
		   #edge{rf=Face,rtsu=Pe0} -> Pe0
	       end,
    PrevCol = wings_va:edge_attrs(PrevEdge, Face, OrigWe),
    slide_colors(Face, Edge, Edge, PrevCol, OrigWe, We, not_done).

slide_colors(_Face, LastEdge, LastEdge, _, _, We, done) -> We;
slide_colors(Face, Edge, LastEdge, PrevAttrs, #we{es=Etab}=OrigWe, We0, _) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face,ltpr=NextEdge} ->
	    Attrs = wings_va:edge_attrs(Edge, Face, OrigWe),
	    We = wings_va:set_edge_attrs(Edge, Face, PrevAttrs, We0),
	    slide_colors(Face, NextEdge, LastEdge, Attrs, OrigWe, We, done);
	#edge{rf=Face,rtpr=NextEdge} ->
	    Attrs = wings_va:edge_attrs(Edge, Face, OrigWe),
	    We = wings_va:set_edge_attrs(Edge, Face, PrevAttrs, We0),
	    slide_colors(Face, NextEdge, LastEdge, Attrs, OrigWe, We, done)
    end.

merge_1([We]) -> We;
merge_1(Wes) -> merge_1(Wes, [], [], [], []).

merge_1([#we{vp=Vp0,es=Es,he=He,holes=Holes}|Wes], Vpt0, Et0, Ht0, Ho0) ->
    Vpt = [array:sparse_to_orddict(Vp0)|Vpt0],
    Et = [array:sparse_to_orddict(Es)|Et0],
    Ht = [gb_sets:to_list(He)|Ht0],
    Ho = [Holes|Ho0],
    merge_1(Wes, Vpt, Et, Ht, Ho);
merge_1([], Vpt0, Et0, Ht0, Ho0) ->
    Vpt = lists:merge(Vpt0),
    Et = lists:merge(Et0),
    Ht = lists:merge(Ht0),
    Ho = lists:merge(Ho0),
    {Vpt,Et,Ht,Ho}.

merge_plugins(Wes) ->
    Psts  = [gb_trees:keys(We#we.pst) || We <- Wes],
    PMods = lists:usort(lists:append(Psts)),
    Merge = fun(Mod,Acc) ->
		    try
			Pst = Mod:merge_we(Wes),
			[{Mod, Pst}|Acc]
		    catch _:_ -> Acc
		    end
	    end,
    Merged = lists:foldl(Merge, [], PMods),
    gb_trees:from_orddict(Merged).
		    
merge_renumber(Wes0) ->
    [{_,We1}|Wes] = merge_bounds(Wes0, []),
    We = wings_va:gc(We1),
    merge_renumber(Wes, [We], []).

merge_renumber([{Low,We}|Wes], [#we{next_id=Next}|_]=Done, NotDone)
  when Low >= Next ->
    merge_renumber(Wes, [wings_va:gc(We)|Done], NotDone);
merge_renumber([{_,We}|Wes], Done, NotDone) ->
    merge_renumber(Wes, Done, [We|NotDone]);
merge_renumber([], [#we{next_id=Next}|_]=Done, NotDone) ->
    merge_renumber_rest(NotDone, Next, Done).

merge_renumber_rest([We0|Wes], Next0, Acc) ->
    #we{next_id=Next} = We = do_renumber(We0, Next0),
    merge_renumber_rest(Wes, Next, [We|Acc]);
merge_renumber_rest([], _, Acc) -> Acc.

merge_bounds([#we{vp=Vtab,fs=Ftab,es=Etab}=We0|Wes], Acc) ->
    First = case wings_util:array_is_empty(Etab) of
		true -> 0;
		false ->
		    Min = lists:min([wings_util:array_smallest_key(Vtab),
				     wings_util:array_smallest_key(Etab),
				     wings_util:gb_trees_smallest_key(Ftab)]),

		    %% We must not return a negative number here (caused by
		    %% a hidden face), because the list will become sorted
		    %% in the wrong order.
		    max(Min, 0)
	    end,
    We = update_id_bounds(We0),
    merge_bounds(Wes, [{First,We}|Acc]);
merge_bounds([], Acc) -> sort(Acc).

%% Leaves the vc and fs fields undefined.
do_renumber(We0, Id) ->
    {We,_} = do_renumber(We0, Id, []),
    We.

do_renumber(#we{vp=Vtab0,es=Etab0,fs=Ftab0,
		mat=MatTab0,he=Htab0,perm=Perm0,holes=Holes0,
		mirror=Mirror0,pst=Pst0}=We0,
	    Id, RootSet0) ->
    Vtab1 = array:sparse_to_orddict(Vtab0),
    Vmap = make_map(Vtab1, Id),
    Vtab = renumber_vertices(Vtab1, Vmap),

    Fmap = make_map(gb_trees:to_list(Ftab0), Id),
    MatTab = wings_facemat:renumber(MatTab0, Fmap),

    Etab1 = array:sparse_to_orddict(Etab0),
    Emap = make_map(Etab1, Id),

    Etab2 = foldl(fun(E, A) ->
			  renum_edge(E, Emap, Vmap, Fmap, A)
		  end, [], Etab1),
    Etab = array:from_orddict(reverse(Etab2)),

    Htab1 = gb_sets:fold(fun(E, A) ->
				 renum_hard_edge(E, Emap, A)
			 end, [], Htab0),
    Htab = gb_sets:from_list(Htab1),

    Perm = case Perm0 of
	       {SelMode,Elems0} ->
		   Root = [{SelMode,gb_sets:to_list(Elems0),[]}],
		   [{_,Elems,_}] = map_rootset(Root, Emap, Vmap, Fmap),
		   {SelMode,gb_sets:from_list(Elems)};
	       _ -> Perm0
	   end,

    Holes = [gb_trees:get(F, Fmap) || F <- Holes0],

    Mirror = if
		 Mirror0 =:= none -> Mirror0;
		 true -> gb_trees:get(Mirror0, Fmap)
	     end,

    Pst_Elements = wings_plugin:check_plugins(save,Pst0),
    Pst = foldl(fun
        ({_,{Plugin,{vs,VsSet}}}, Pst1) ->
            Vs = gb_sets:to_list(VsSet),
            renum_elements_in_pst(vs,Plugin,Vs,Vmap,Pst1);
        ({_,{Plugin,{es,EsSet}}}, Pst1) ->
            Es = gb_sets:to_list(EsSet),
            renum_elements_in_pst(es,Plugin,Es,Emap,Pst1);
        ({_,{Plugin,{fs,FsSet}}}, Pst1) ->
            Fs = gb_sets:to_list(FsSet),
            renum_elements_in_pst(fs,Plugin,Fs,Fmap,Pst1);
        ({remove,Plugin}, Pst1) ->
            gb_trees:delete(Plugin,Pst1)
        end, Pst0, Pst_Elements),

    RootSet = map_rootset(RootSet0, Emap, Vmap, Fmap),
    We1 = We0#we{vc=undefined,fs=undefined,
		 vp=Vtab,es=Etab,mat=MatTab,he=Htab,
		 perm=Perm,holes=Holes,mirror=Mirror,pst=Pst},
    We = wings_va:renumber(Emap, We1),

    %% In case this function will be used for merging #we records,
    %% it is essential to update the next_id field. Its value can
    %% safely be based on the largest key in the edge table alone.
    LastId = case wings_util:array_is_empty(Etab) of
		 true -> 0;
		 false -> wings_util:array_greatest_key(Etab)
	     end,
    {We#we{next_id=LastId+1},RootSet}.

map_rootset([{vertex,Vs,Data}|T], Emap, Vmap, Fmap) when is_list(Vs) ->
    [map_all(vertex, Vs, Data, Vmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edges,Data}|T], Emap, Vmap, Fmap) when is_list(Edges) ->
    [map_all(edge, Edges, Data, Emap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Faces,Data}|T], Emap, Vmap, Fmap) when is_list(Faces) ->
    [map_all(face, Faces, Data, Fmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{body,_Empty,_Data}=Sel|T], Emap, Vmap, Fmap) ->
    [Sel|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{vertex,V}|T], Emap, Vmap, Fmap) ->
    [{vertex,gb_trees:get(V, Vmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edge}|T], Emap, Vmap, Fmap) ->
    [{edge,gb_trees:get(Edge, Emap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Face}|T], Emap, Vmap, Fmap) ->
    [{face,gb_trees:get(Face, Fmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{body,Empty}|T], Emap, Vmap, Fmap) ->
    [{body,Empty}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([], _, _, _) -> [].

map_all(What, Items, Data, Map) ->
    {What,[gb_trees:get(Key, Map) || Key <- Items],Data}.

make_map(Tab, Id0) ->
    make_map(Tab, Id0, []).
make_map([{Old,_}|T], Id, Map) ->
    make_map(T, Id+1, [{Old,Id}|Map]);
make_map([], _, Map) -> gb_trees:from_orddict(reverse(Map)).

renum_edge({Edge0,Rec0}, Emap, Vmap, Fmap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=Ltpr,ltsu=Ltsu,
	  rtpr=Rtpr,rtsu=Rtsu} = Rec0,
    Rec = Rec0#edge{vs=gb_trees:get(Vs, Vmap),ve=gb_trees:get(Ve, Vmap),
		    lf=gb_trees:get(Lf, Fmap),rf=gb_trees:get(Rf, Fmap),
		    ltpr=gb_trees:get(Ltpr, Emap),
		    ltsu=gb_trees:get(Ltsu, Emap),
		    rtpr=gb_trees:get(Rtpr, Emap),
		    rtsu=gb_trees:get(Rtsu, Emap)},
    [{Edge,Rec}|New].

renumber_vertices(Vtab, Vmap) ->
    renumber_vertices_1(Vtab, Vmap, []).

renumber_vertices_1([{V0,P}|Vtab], Vmap, VtabAcc) ->
    V = gb_trees:get(V0, Vmap),
    renumber_vertices_1(Vtab, Vmap, [{V,P}|VtabAcc]);
renumber_vertices_1([], _, Vtab) ->
    array:from_orddict(keysort(1, Vtab)).
    
renum_hard_edge(Edge0, Emap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    [Edge|New].


% Checks plugins which store elements in the Pst that need to be renumbered
% before saving and returns the new Pst.
renum_elements_in_pst(Key,Plugin,Elements,Map,Pst0) ->
    Renumbered = foldl(fun(Elem, New) ->
      case gb_trees:lookup(Elem,Map) of
          none -> New;
          {_,NewNum} -> [NewNum|New]
      end
    end, [], Elements),
    Data = gb_trees:get(Plugin,Pst0),
    NewElems = gb_sets:from_list(Renumbered),
    NewData = gb_trees:update(Key,NewElems,Data),
    Pst = gb_trees:update(Plugin,NewData,Pst0),
    Pst.

update_id_bounds(#we{vp=Vtab,es=Etab,fs=Ftab}=We) ->
    case wings_util:array_is_empty(Etab) of
	true -> We#we{next_id=0};
	false ->
	    LastId = lists:max([wings_util:array_greatest_key(Vtab),
				wings_util:array_greatest_key(Etab),
				wings_util:gb_trees_largest_key(Ftab),
				-wings_util:gb_trees_smallest_key(Ftab)-1]),
	    We#we{next_id=LastId+1}
    end.

%%%
%%% Separate a combined winged-edge structure.
%%%

separate(We0) ->
    We = break_mirror(We0),
    separate_1(We#we{vc=undefined,fs=undefined}, 0, []).

separate_1(#we{es=Etab0}=We, Smallest0, Acc) ->
    case wings_util:array_is_empty(Etab0) of
	true -> Acc;
	false ->
	    {Edge,Smallest} = smallest(Smallest0, Etab0),
	    Ws = gb_sets:singleton(Edge),
	    {EtabLeft,NewEtab} = separate(Ws, Etab0, array:new()),
	    NewWe = copy_dependents(We#we{es=NewEtab}),
	    separate_1(We#we{es=EtabLeft}, Smallest, [NewWe|Acc])
    end.

separate(Ws0, Etab0, Acc0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Etab0,Acc0};
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    Rec = array:get(Edge, Etab0),
	    Etab = array:reset(Edge, Etab0),
	    Acc = array:set(Edge, Rec, Acc0),
	    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
	    List = [E || E <- [LP,LS,RP,RS],
			 array:get(E, Etab) =/= undefined],
	    Set = gb_sets:from_list(List),
	    Ws = gb_sets:union(Ws1, Set),
	    separate(Ws, Etab, Acc)
    end.

smallest(I, A) ->
    case array:get(I, A) of
	undefined -> smallest(I+1, A);
	_ -> {I,I+1}
    end.

copy_dependents(We0) ->
    #we{es=Etab,he=Htab0,vc=Vct,vp=Vtab0} = We = rebuild(We0),
    Htab = case gb_sets:is_empty(Htab0) of
	       true ->
		   Htab0;
	       false ->
		   Es = wings_util:array_keys(Etab),
		   gb_sets:intersection(Htab0, gb_sets:from_ordset(Es))
	   end,
    Vs = sofs:from_external(wings_util:array_keys(Vct), [vertex]),
    Vtab1 = sofs:relation(array:sparse_to_orddict(Vtab0), [{vertex,edge}]),
    Vtab2 = sofs:restriction(Vtab1, Vs),
    Vtab = array:from_orddict(sofs:to_external(Vtab2)),
    wings_va:gc(wings_facemat:gc(We#we{he=Htab,vp=Vtab})).

%%%
%%% Convert textures to vertex colors.
%%%

uv_to_color(#we{es=Etab}=We0, St) ->
    array:sparse_foldl(
      fun(Edge, #edge{lf=Lf,rf=Rf}, W) ->
	      UVa = wings_va:attr(uv, wings_va:edge_attrs(Edge, left, W)),
	      UVb = wings_va:attr(uv, wings_va:edge_attrs(Edge, right, W)),
	      ColA = wings_material:color(Lf, UVa, We0, St),
	      ColB = wings_material:color(Rf, UVb, We0, St),
	      wings_va:set_edge_color(Edge, ColA, ColB, W)
      end, We0, Etab).

%% uv_mapped_faces(We) -> [Face]
%%  Return an ordered list of all faces that have UV coordinates.
uv_mapped_faces(#we{fs=Ftab}=We) ->
    uv_mapped_faces_1(gb_trees:to_list(Ftab), We, []).

uv_mapped_faces_1([{F,E}|Fs], We, Acc) ->
    Good = foldl(fun({_,_}, Flag) -> Flag;
		    (_, _) -> false
		 end, true, wings_va:face_attr(uv, F, E, We)),
    case Good of
	false -> uv_mapped_faces_1(Fs, We, Acc);
	true -> uv_mapped_faces_1(Fs, We, [F|Acc])
    end;
uv_mapped_faces_1([], _, Acc) -> reverse(Acc).

%%%
%%% Transform all vertices according to the matrix.
%%%

transform_vs({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,Tx,Ty,Tz}, We) ->
    Translate = fun(V, {X,Y,Z}, A) -> [{V,{X+Tx,Y+Ty,Z+Tz}}|A] end,
    transform_vs_1(Translate, We);
transform_vs(Matrix, We) ->
    Transform = fun(V, Pos, A) ->
			[{V,e3d_mat:mul_point(Matrix, Pos)}|A]
		end,
    transform_vs_1(Transform, We).

transform_vs_1(Transform, #we{vp=Vtab0}=We) ->
    Vtab1 = array:sparse_foldl(Transform, [], Vtab0),
    Vtab = array:from_orddict(reverse(Vtab1)),
    We#we{vp=Vtab}.

%%%
%%% Calculate normals.
%%%

%% vertex_normals(FaceNormals, We, MirrorMatrix) -> [{Face,[VertexNormal]}]
%%       MirrorMatrix = Matrix | none
%%  Given the face normals for an object, calculate the
%%  vertex normals. If the object has a virtual mirror face,
%%  the mirror matrix must be given.
%%
normals(Ns, #we{mirror=none}=We, MM) ->
    case is_open(We) of
	false -> normals_2(Ns, We, MM);
	true -> normals_1(Ns, We, MM)
    end;
normals(Ns, We, MM) -> normals_1(Ns, We, MM).

normals_1(FaceNormals, #we{fs=Ftab,he=Htab0}=We, MM) ->
    Edges = case {visible(We),gb_trees:size(Ftab)} of
		{Vis,Sz} when 2*length(Vis) < Sz ->
		    wings_face:outer_edges(Vis, We);
		{Vis,_} ->
		    InVis = ordsets:subtract(gb_trees:keys(Ftab), Vis),
		    wings_face:outer_edges(InVis, We)
	    end,
    Htab = gb_sets:union(Htab0, gb_sets:from_ordset(Edges)),
    normals_2(FaceNormals, We#we{he=Htab}, MM).

normals_2(FaceNormals, #we{he=He}=We, MM) ->
    wings_pb:start(?__(1,"calculating soft normals")),
    Res = case FaceNormals of
	      [_,_] ->
		  two_faced(FaceNormals, We);
	      _ ->
		  case gb_sets:is_empty(He) of
		      true -> all_soft(FaceNormals, We);
		      false -> mixed_edges(FaceNormals, We, MM)
		  end
	  end,
    wings_pb:done(Res).

all_soft(FaceNormals, #we{vp=Vtab}=We) ->
    %% The simple case: There are no hard edges and no
    %% virtual mirror.
    %%
    %% Therefore the normals for all vertices can be
    %% calculated by simply adding the normals for the
    %% faces surrounding the vertex and normalizing.
    %% (Each vertex will have the same normal in each
    %% face it appears in.)
    wings_pb:update(0.10, ?__(1,"preparing")),
    VisVs = visible_vs(array:sparse_to_orddict(Vtab), We),
    VtxNormals = soft_vertex_normals(VisVs, FaceNormals, We),
    FoldFun = fun(V, _, _, A) ->
		      Normal = gb_trees:get(V, VtxNormals),
		      [Normal|A]
	      end,
    wings_pb:update(0.6, ?__(2,"collecting")),
    all_soft_1(FoldFun, FaceNormals, We, []).

all_soft_1(FoldFun, [{Face,_}|FNs], We, Acc) ->
    Vs = wings_face:fold(FoldFun, [], Face, We),
    all_soft_1(FoldFun, FNs, We, [{Face,Vs}|Acc]);
all_soft_1(_, [], _, Acc) -> reverse(Acc).

mixed_edges(FaceNormals0, #we{mirror=MirrorFace}=We, MirrorMatrix) ->
    %% The complicated case: There are some hard edges and/or
    %% a virtual mirror.
    wings_pb:update(0.20, ?__(1,"preparing")),
    G = digraph:new(),
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    wings_pb:update(0.50,  ?__(2,"vertex normals")),

    %% For all vertices that are not connected to any hard
    %% edges, calculate vertex normals as in the simple
    %% case above (all_soft/2). For all other vertices (i.e.
    %% those connected to at least one hard edge), build a
    %% digraph for the faces around the vertices; a pair of
    %% faces will be connected with an edge in the digraph if
    %% the edge between them is soft.
    VtxNormals = vertex_normals(We, G, FaceNormals),
    wings_pb:update(0.99,  ?__(3,"vertex normals per face")),

    %% If there is a virtual mirror face, we have added hard
    %% edges around it, but the hard edges should not actually
    %% be displayed as hard edges. (We have added them as hard
    %% edges to force a virtual face to be handle as a special
    %% case by this function, rather than complicating and
    %% making all_soft/2 slower.)
    %%
    %% Here we want to collect the vertices that surround the
    %% virtual mirror face so that we quickly can test whether
    %% a given vertex is part of the virtual mirror face. We
    %% also need th virtual mirror matrix for transforming
    %% normal vectors.
    Mirror =
	case MirrorFace of
	    none -> none;
	    _ ->
		MirrorVs0 = wings_face:to_vertices([MirrorFace], We),
		MirrorVs = gb_sets:from_ordset(MirrorVs0),
		{MirrorVs,MirrorMatrix}
	end,

    %% Go through each face in the object and calculate the
    %% normals for each vertex in the face.
    Ns = foldl(fun({Face,_}, Acc) ->
		       Vs = n_face(Face, G, FaceNormals,
				   VtxNormals, Mirror, We),
		       [{Face,Vs}|Acc]
	       end, [], FaceNormals0),
    digraph:delete(G),
    reverse(Ns).

n_face(Face, G, FaceNormals, VtxNormals, Mirror, We) ->
    wings_face:fold(
      fun(V, _, _, Acc) ->
	      case gb_trees:lookup(V, VtxNormals) of
		  {value,Normal} ->
		      %% Since the normal for the vertex was found in the
		      %% VtxNormals table, it means that all edges
		      %% surrounding the vertex are soft and that the
		      %% vertex normal is the same in all faces that
		      %% vertex is part of.
		      [Normal|Acc];
		  none ->
		      %% Not found. That means that there is at least
		      %% one hard edge (and/or virtual mirror face) around
		      %% the vertex. We must use the digraph to find the
		      %% faces that are reachable without crossing a
		      %% hard edge.
		      Normal = hard_vtx_normal(G, V, Face, FaceNormals, Mirror),
		      [Normal|Acc]
	      end
      end, [], Face, We).

hard_vtx_normal(G, V, Face, FaceNormals, Mirror) ->
    %% Collect the face normals for all faces that can be reached
    %% from this face without crossing a hard edge.
    Reachable = digraph_utils:reachable([{V,Face}], G),
    Ns = [gb_trees:get(AFace, FaceNormals) || {_,AFace} <- Reachable],

    %% Average the normals.
    average_normals(Ns, V, Mirror).

average_normals(Ns, _, none) ->
    %% No virtual mirror at all.
    average_normals_1(Ns);
average_normals(Ns0, V, {MirrorVs,MirrorMatrix}) ->
    case gb_sets:is_member(V, MirrorVs) of
	false ->
	    %% This vertex is not part of the virtual mirror face.
	    average_normals_1(Ns0);
	true ->
	    %% This vertex is part of the virtual mirror face.
	    %% By adding the list of mirrored faces normals to the
	    %% list of face normals, we will make it appear that the
	    %% edges around the virtual mirror face are soft.
	    Ns = [e3d_mat:mul_vector(MirrorMatrix, N) || N <- Ns0] ++ Ns0,
	    average_normals_1(Ns)
    end.

average_normals_1([N]) -> N;
average_normals_1(Ns) -> e3d_vec:norm(e3d_vec:add(Ns)).

two_faced([{FaceA,Na},{FaceB,Nb}], We) ->
    [{FaceA,two_faced_1(FaceA, Na, We)},
     {FaceB,two_faced_1(FaceB, Nb, We)}].

two_faced_1(Face, Normal, We) ->
    wings_face:fold(fun (_, _, _, Acc) ->
			    [Normal|Acc]
		    end, [], Face, We).

vertex_normals(#we{vp=Vtab}=We, G, FaceNormals) ->
    Vs0 = visible_vs(array:sparse_to_orddict(Vtab), We),
    Vs = sofs:from_external(Vs0, [{vertex,data}]),
    vertex_normals_1(Vs, We, G, FaceNormals).

vertex_normals_1(Vs, #we{es=Etab,he=Htab}=We, G, FaceNormals) ->
    He0 = gb_sets:to_list(Htab),
    He = sofs:from_external(He0, [edge]),
    Es0 = array:sparse_to_orddict(Etab),
    Es1 = sofs:from_external(Es0, [{edge,data}]),
    Es = sofs:image(Es1, He),
    Hvs0 = foldl(fun(#edge{vs=Va,ve=Vb}, A) ->
			 [Va,Vb|A]
		 end, [], sofs:to_external(Es)),
    Hvs = sofs:set(Hvs0, [vertex]),
    Svs = sofs:drestriction(Vs, Hvs),
    SoftVs = sofs:to_external(Svs),
    HardVs = sofs:to_external(Hvs),
    foreach(fun(V) -> update_digraph(G, V, We) end, HardVs),
    soft_vertex_normals(SoftVs, FaceNormals, We).

update_digraph(G, V, #we{he=Htab}=We) ->
    wings_vertex:fold(
      fun(Edge, _, #edge{lf=Lf0,rf=Rf0}, _) ->
	      case gb_sets:is_member(Edge, Htab) of
		  true -> ok;
		  false ->
		      Lf = {V,Lf0},
		      Rf = {V,Rf0},
		      digraph:add_vertex(G, Lf),
		      digraph:add_vertex(G, Rf),
		      digraph:add_edge(G, Lf, Rf),
		      digraph:add_edge(G, Rf, Lf)
	      end
      end, [], V, We).

soft_vertex_normals(Vtab, FaceNormals0, We) when is_list(FaceNormals0) ->
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    soft_vertex_normals(Vtab, FaceNormals, We);
soft_vertex_normals(Vtab, FaceNormals, We) ->
    FoldFun = fun(_, Face, _, A) ->
		      [gb_trees:get(Face, FaceNormals)|A]
	      end,
    Soft = foldl(fun({V,_}, Acc) ->
			 Ns = wings_vertex:fold(FoldFun, [], V, We),
			 N = e3d_vec:norm(e3d_vec:add(Ns)),
			 [{V,N}|Acc]
		 end, [], Vtab),
    gb_trees:from_orddict(reverse(Soft)).

new_items_as_ordset_1(Tab, Wid, NewWid) when NewWid-Wid < 32 ->
    new_items_as_ordset_2(Wid, NewWid, Tab, []);
new_items_as_ordset_1(Tab, Wid, _NewWid) ->
    [Item || Item <- gb_trees:keys(Tab), Item >= Wid].

new_items_as_ordset_2(Wid, NewWid, Tab, Acc) when Wid < NewWid ->
    case gb_trees:is_defined(Wid, Tab) of
	true -> new_items_as_ordset_2(Wid+1, NewWid, Tab, [Wid|Acc]);
	false -> new_items_as_ordset_2(Wid+1, NewWid, Tab, Acc)
    end;
new_items_as_ordset_2(_Wid, _NewWid, _Tab, Acc) -> reverse(Acc).

new_array_items_as_ordset_1(Tab, Wid, NewWid) when NewWid-Wid < 32 ->
    new_array_items_as_ordset_2(Wid, NewWid, Tab, []);
new_array_items_as_ordset_1(Tab, Wid, _NewWid) ->
    [Item || Item <- wings_util:array_keys(Tab), Item >= Wid].

new_array_items_as_ordset_2(Wid, NewWid, Tab, Acc) when Wid < NewWid ->
    case array:get(Wid, Tab) of
	undefined -> new_array_items_as_ordset_2(Wid+1, NewWid, Tab, Acc);
	_ -> new_array_items_as_ordset_2(Wid+1, NewWid, Tab, [Wid|Acc])
    end;
new_array_items_as_ordset_2(_Wid, _NewWid, _Tab, Acc) -> reverse(Acc).

%%%
%%% Test the consistency of a #we{}.
%%%

is_consistent(#we{}=We) ->
    try
	validate_vertex_tab(We),
	validate_faces(We)
    catch error:_ -> false
    end.

is_face_consistent(Face, #we{fs=Ftab,es=Etab}) ->
    Edge = gb_trees:get(Face, Ftab),
    try validate_face(Face, Edge, Etab)
    catch error:_ -> false
    end.
    
validate_faces(#we{fs=Ftab,es=Etab}) ->
    validate_faces_1(gb_trees:to_list(Ftab), Etab).

validate_faces_1([{Face,Edge}|Fs], Etab) ->
    validate_face(Face, Edge, Etab),
    validate_faces_1(Fs, Etab);
validate_faces_1([], _) -> true.

validate_face(Face, Edge, Etab) ->
    Ccw = walk_face_ccw(Edge, Etab, Face, Edge, []),
    Edge = walk_face_cw(Edge, Etab, Face, Ccw),
    [V|Vs] = sort(Ccw),
    validate_face_vertices(Vs, V).

validate_face_vertices([V|_], V) ->
    erlang:error(repeated_vertex);
validate_face_vertices([_], _) ->
    true;
validate_face_vertices([V|Vs], _) ->
    validate_face_vertices(Vs, V).

walk_face_ccw(LastEdge, _, _, LastEdge, [_|_]=Acc) -> Acc;
walk_face_ccw(Edge, Etab, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc])
    end.

walk_face_cw(Edge, _, _, []) -> Edge;
walk_face_cw(Edge, Etab, Face, [V|Vs]) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs)
    end.

validate_vertex_tab(#we{es=Etab,vc=Vct}) ->
    foreach(fun({V,Edge}) ->
		    case array:get(Edge, Etab) of
			#edge{vs=V} -> ok;
			#edge{ve=V} -> ok
		    end
	    end, array:sparse_to_orddict(Vct)).
