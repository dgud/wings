%%
%%  wings_sel.erl --
%%
%%     This module implements selection utilities.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_sel).

-export([clear/1,reset/1,set/2,set/3,
	 conditional_reset/1,
	 map/2,fold/3,mapfold/3,
	 make/3,valid_sel/1,valid_sel/3,
	 center/1,bbox_center/1,bounding_box/1,bounding_boxes/1,
	 face_regions/2,strict_face_regions/2,edge_regions/2,
	 select_object/2,deselect_object/2,
	 get_all_items/2,get_all_items/3,
	 inverse_items/3]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3,keymember/3]).

clear(St) ->
    St#st{sel=[],sh=false}.

reset(#st{selmode=Mode}=St) ->
    case Mode of
	body -> St#st{selmode=face,sel=[],sh=true};
	_ -> St#st{sel=[],sh=true}
    end.

conditional_reset(#st{sel=[]}=St) ->
    reset(St);
conditional_reset(St) ->
    St#st{sel=[],sh=false}.

set([], St) ->
    clear(St);
set(Sel, St) ->
    St#st{sel=sort(Sel),sh=false}.

set(Mode, [], St) ->
    clear(St#st{selmode=Mode});
set(Mode, Sel, St) ->
    St#st{selmode=Mode,sel=sort(Sel),sh=false}.

%%%
%%% Map over the selection, modifying the selected objects.
%%%

map(F, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    Shs = map_1(F, Sel, Shs1, St, []),
    St#st{shapes=Shs}.

map_1(F, [{Id,Items}|Sel], [{Id,We0}|Shs], St, Acc) ->
    ?ASSERT(We0#we.id =:= Id),
    #we{es=Etab} = We = F(Items, We0),
    case wings_util:array_is_empty(Etab) of
	true -> map_1(F, Sel, Shs, St, Acc);
	false -> map_1(F, Sel, Shs, St, [{Id,We}|Acc])
    end;
map_1(F, [_|_]=Sel, [Pair|Shs], St, Acc) ->
    map_1(F, Sel, Shs, St, [Pair|Acc]);
map_1(_F, [], Shs, _St, Acc) ->
    gb_trees:from_orddict(reverse(Acc, Shs)).

%%%
%%% Fold over the selection.
%%%

fold(F, Acc, #st{sel=Sel,shapes=Shapes}) ->
    fold_1(F, Acc, Shapes, Sel).

fold_1(F, Acc0, Shapes, [{Id,Items}|T]) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    fold_1(F, F(Items, We, Acc0), Shapes, T);
fold_1(_F, Acc, _Shapes, []) -> Acc.

%%%
%%% Map and fold over the selection.
%%%

mapfold(F, Acc0, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    {Shs,Acc} = mapfold_1(F, Acc0, Sel, Shs1, St, []),
    {St#st{shapes=Shs},Acc}.

mapfold_1(F, Acc0, [{Id,Items}|Sel], [{Id,We0}|Shs], St, ShsAcc) ->
    ?ASSERT(We0#we.id =:= Id),
    {#we{es=Etab}=We,Acc} = F(Items, We0, Acc0),
    case wings_util:array_is_empty(Etab) of
	true -> mapfold_1(F, Acc0, Sel, Shs, St, ShsAcc);
	false -> mapfold_1(F, Acc, Sel, Shs, St, [{Id,We}|ShsAcc])
    end;
mapfold_1(F, Acc, [_|_]=Sel, [Pair|Shs], St, ShsAcc) ->
    mapfold_1(F, Acc, Sel, Shs, St, [Pair|ShsAcc]);
mapfold_1(_F, Acc, [], Shs, _St, ShsAcc) ->
    {gb_trees:from_orddict(reverse(ShsAcc, Shs)),Acc}.

%% make(Filter, Mode, St) -> [{Id,ItemGbSet}].
%%      Mode = body|face|edge|vertex
%%      Filter(Item, We) -> true|false
%%      Item = face() | edge() | vertex() | 0
%%  Construct a selection by calling Filter(Item, We) for each
%%  item in each object (where Item is either a face number,
%%  edge number, vertex number, or 0 depending on Mode).
%%
%%  Invisible geometry will not be included in the selection.
%%  Filter/2 will never be called for invisible faces, while
%%  invisible edges and vertices will be filtered away after
%%  the call to Filter/2.
%%

-type filter_fun() :: fun((visible_face_num() | edge_num() | vertex_num() | 0,
			   #we{}) -> boolean()).
-spec make(filter_fun(), sel_mode(), #st{}) ->
    #st{sel::[{non_neg_integer(),gb_set()}]}.

make(Filter, Mode, #st{shapes=Shapes}=St) when is_function(Filter, 2) ->
    Sel0 = gb_trees:values(Shapes),
    Sel = make_1(Sel0, Filter, Mode),
    St#st{selmode=Mode,sel=Sel}.

make_1([#we{perm=Perm}|Shs], Filter, Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    make_1(Shs, Filter, Mode);
make_1([We|Shs], Filter, Mode) when ?IS_LIGHT(We) ->
    make_1(Shs, Filter, Mode);
make_1([#we{id=Id}=We|Shs], Filter, body) ->
    case Filter(0, We) of
	false -> make_1(Shs, Filter, body);
	true -> [{Id,gb_sets:singleton(0)}|make_1(Shs, Filter, body)]
    end;
make_1([#we{id=Id,fs=Ftab}=We|Shs], Filter, face=Mode) ->
    Faces = gb_trees:keys(Ftab),
    case [Face || Face <- Faces, Face >= 0, Filter(Face, We)] of
	[] -> make_1(Shs, Filter, Mode);
	Sel -> [{Id,gb_sets:from_ordset(Sel)}|make_1(Shs, Filter, Mode)]
    end;
make_1([#we{id=Id,es=Etab}=We|Shs], Filter, edge=Mode) ->
    Es = wings_util:array_keys(Etab),
    case [E || E <- Es, Filter(E, We)] of
	[] -> make_1(Shs, Filter, Mode);
	Sel0 ->
	    Sel = wings_we:visible_edges(gb_sets:from_ordset(Sel0), We),
	    [{Id,Sel}|make_1(Shs, Filter, Mode)]
    end;
make_1([#we{id=Id,vp=Vtab}=We|Shs], Filter, vertex=Mode) ->
    Vs = wings_util:array_keys(Vtab),
    case [V || V <- Vs, Filter(V, We)] of
	[] -> make_1(Shs, Filter, Mode);
	Sel0 ->
	    Sel = gb_sets:from_ordset(wings_we:visible_vs(Sel0, We)),
	    [{Id,Sel}|make_1(Shs, Filter, Mode)]
    end;
make_1([], _Filter, _Mode) -> [].

%%%
%%% Calculate the center for all selected objects.
%%%

center(#st{selmode=Mode}=St) ->
    Centers = fold(fun(Items, We, A) ->
			   Vs = to_vertices(Mode, Items, We),
			   [wings_vertex:center(Vs, We)|A]
		   end, [], St),
    e3d_vec:average(Centers).

bbox_center(St) ->
    BBox = bounding_box(St),
    e3d_vec:average(BBox).
%%%
%%% Calculate the bounding-box for the selection.
%%%

bounding_box(#st{selmode=Mode}=St) ->
    fold(fun(Items, We, A) ->
		 Vs = to_vertices(Mode, Items, We),
		 wings_vertex:bounding_box(Vs, We, A)
	 end, none, St).

%%%
%%% Calculate the bounding boxes for all selected objects.
%%%

bounding_boxes(#st{selmode=Mode}=St) ->
    reverse(
      fold(
	fun(Items, We, A) ->
		Vs = to_vertices(Mode, Items, We),
		[e3d_vec:average(wings_vertex:bounding_box(Vs, We))|A]
	end, [], St)).


%%%
%%% Divide the face selection into regions where each face shares at least
%%% one edge with another face in the same region. Two faces can share a
%%% vertex without necessarily being in the same region.
%%%

face_regions(Faces, We) when is_list(Faces) ->
    face_regions_1(gb_sets:from_list(Faces), We);
face_regions(Faces, We) ->
    face_regions_1(Faces, We).

face_regions_1(Faces, We) ->
    find_face_regions(Faces, We, fun collect_face_fun/5, []).

find_face_regions(Faces0, We, Coll, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = [Face],
	    {Reg,Faces} = collect_face_region(Ws, We, Coll, [], Faces1),
	    find_face_regions(Faces, We, Coll, [Reg|Acc])
    end.

collect_face_region([_|_]=Ws0, We, Coll, Reg0, Faces0) ->
    Reg = Ws0++Reg0,
    {Ws,Faces} = wings_face:fold_faces(Coll, {[],Faces0}, Ws0, We),
    collect_face_region(Ws, We, Coll, Reg, Faces);
collect_face_region([], _, _, Reg, Faces) ->
    {gb_sets:from_list(Reg),Faces}.

collect_face_fun(Face, _, _, Rec, {Ws,Faces}=A) ->
    Of = case Rec of
	     #edge{lf=Face,rf=Of0} -> Of0;
	     #edge{rf=Face,lf=Of0} -> Of0
	 end,
    case gb_sets:is_member(Of, Faces) of
	true -> {[Of|Ws],gb_sets:delete(Of, Faces)};
	false -> A
    end.

%%%
%%% Divide the face selection into regions where each face shares at least
%%% one vertex with another face in the same region.
%%%

strict_face_regions(Faces, We) when is_list(Faces) ->
    find_strict_face_regions(gb_sets:from_list(Faces), We, []);
strict_face_regions(Faces, We) ->
    find_strict_face_regions(Faces, We, []).

find_strict_face_regions(Faces0, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = gb_sets:singleton(Face),
	    {Reg,Faces} = collect_strict_face_region(Ws, We, [], Faces1),
	    find_strict_face_regions(Faces, We, [Reg|Acc])
    end.

collect_strict_face_region(Ws0, We, Reg0, Faces0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {gb_sets:from_list(Reg0),Faces0};
	false ->
	    {Face,Ws1} = gb_sets:take_smallest(Ws0),
	    Reg = [Face|Reg0],
	    {Ws,Faces} = collect_strict_adj_sel(Face, We, Ws1, Faces0),
	    collect_strict_face_region(Ws, We, Reg, Faces)
    end.

collect_strict_adj_sel(Face, We, Ws0, Faces0) ->
    wings_face:fold(
      fun(V, _, _, A0) ->
	      wings_vertex:fold(
		fun(_, Of, _, {W0,F0}=A1) ->
		   case gb_sets:is_member(Of, F0) of
		       true -> {gb_sets:insert(Of, W0),gb_sets:delete(Of, F0)};
		       false -> A1
		   end
		end, A0, V, We)
      end, {Ws0,Faces0}, Face, We).

%%%
%%% Here we want to divide the selection into regions of connected edges.
%%% We use a standard working-set algorithm.
%%%

edge_regions(Edges, We) when is_list(Edges) ->
    find_edge_regions(gb_sets:from_list(Edges), We, []);
edge_regions(Edges, We) ->
    find_edge_regions(Edges, We, []).

find_edge_regions(Edges0, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    Ws = gb_sets:singleton(Edge),
	    {Reg,Edges} = find_all_adj_edges(Ws, We, [], Edges1),
	    find_edge_regions(Edges, We, [Reg|Acc])
    end.

find_all_adj_edges(Ws0, #we{es=Etab}=We, Reg0, Edges0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {gb_sets:from_list(Reg0),Edges0};
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    Reg = [Edge|Reg0],
	    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
	    Adj0 = add_adjacent_edges(Va, We, []),
	    Adj1 = add_adjacent_edges(Vb, We, Adj0),
	    Adj = gb_sets:from_list(Adj1),
	    AdjSel = gb_sets:intersection(Adj, Edges0),
	    Ws = gb_sets:union(Ws1, AdjSel),
	    Edges = gb_sets:difference(Edges0, AdjSel),
	    find_all_adj_edges(Ws, We, Reg, Edges)
    end.

add_adjacent_edges(V, We, Acc) ->
    wings_vertex:fold(fun(Edge, _, _, A) -> [Edge|A] end, Acc, V, We).
	      
valid_sel(#st{sel=Sel,selmode=Mode}=St) ->
    St#st{sel=valid_sel(Sel, Mode, St)}.
    
valid_sel(Sel0, Mode, #st{shapes=Shapes}) ->
    Sel = foldl(
	    fun({Id,Items0}, A) ->
		    case gb_trees:lookup(Id, Shapes) of
			none -> A;
			{value,#we{perm=Perm}} when ?IS_NOT_SELECTABLE(Perm) ->
			    A;
			{value,We} ->
			    Items = validate_items(Items0, Mode, We),
			    case gb_sets:is_empty(Items) of
				false -> [{Id,Items}|A];
				true -> A
			    end
		    end
	    end, [], Sel0),
    reverse(Sel).

validate_items(Items, body, _We) -> Items;
validate_items(Items, Mode, We) ->
    gb_sets:intersection(Items, get_all_items(Mode, We)).
    
select_object(Id, #st{selmode=Mode,sel=Sel0}=St) ->
    case keymember(Id, 1, Sel0) of
	true -> St;
	false ->
	    Sel = sort([{Id,get_all_items(Mode, Id, St)}|Sel0]),
	    St#st{sel=Sel}
    end.

deselect_object(Id, #st{sel=Sel0}=St) ->
    Sel = keydelete(Id, 1, Sel0),
    St#st{sel=Sel}.

inverse_items(Mode, Elems, We) ->
    gb_sets:difference(get_all_items(Mode, We), Elems).

get_all_items(Mode, Id, #st{shapes=Shapes}) ->
    We = gb_trees:get(Id, Shapes),
    get_all_items(Mode, We).

get_all_items(vertex, We) ->
    gb_sets:from_ordset(wings_we:visible_vs(We));
get_all_items(edge, We) ->
    gb_sets:from_ordset(wings_we:visible_edges(We));
get_all_items(face, We) ->
    gb_sets:from_ordset(wings_we:visible(We));
get_all_items(body, _) ->
    gb_sets:singleton(0).

to_vertices(vertex, Vs, _) -> Vs;
to_vertices(face, Faces, We) ->
    wings_face:to_vertices(Faces, We);
to_vertices(edge, Edges, We) ->
    wings_edge:to_vertices(Edges, We);
to_vertices(body, _, #we{vp=Vtab}) ->
    wings_util:array_keys(Vtab).
