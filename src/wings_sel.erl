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
	 map/2,map_update_sel/2,map_update_sel/3,
	 update_sel/2,update_sel/3,fold/3,dfold/4,mapfold/3,
	 new_sel/3,make/3,valid_sel/1,valid_sel/3,
	 center/1,center_vs/1,
	 bbox_center/1,bounding_box/1,bounding_boxes/1,
	 face_regions/2,strict_face_regions/2,edge_regions/2,
	 select_object/2,deselect_object/2,
	 get_all_items/2,get_all_items/3,
	 inverse_items/3,to_vertices/3]).

-export_type([vertex_set/0,edge_set/0,face_set/0,item_set/0]).

-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3,keymember/3]).

-type vertex_set() :: gb_sets:set(vertex_num()).
-type edge_set() :: gb_sets:set(edge_num()).
-type face_set() :: gb_sets:set(visible_face_num()).

-type item_id() :: visible_face_num() | edge_num() | vertex_num() | 0.
-type item_set() :: gb_sets:set(item_id()).


-type obj_id() :: non_neg_integer().

-spec clear(#st{}) -> #st{}.

clear(St) ->
    St#st{sel=[],sh=false}.

-spec reset(#st{}) -> #st{}.

reset(#st{selmode=Mode}=St) ->
    case Mode of
	body -> St#st{selmode=face,sel=[],sh=true};
	_ -> St#st{sel=[],sh=true}
    end.

-spec conditional_reset(#st{}) -> #st{}.

conditional_reset(#st{sel=[]}=St) ->
    reset(St);
conditional_reset(St) ->
    St#st{sel=[],sh=false}.

-spec set(Sel, #st{}) -> #st{} when
      Sel :: [{item_id(),item_set()}].

set([], St) ->
    clear(St);
set(Sel, St) ->
    St#st{sel=sort(Sel),sh=false}.

-spec set(sel_mode(), Sel, #st{}) -> #st{} when
      Sel :: [{item_id(),item_set()}].

set(Mode, [], St) ->
    clear(St#st{selmode=Mode});
set(Mode, Sel, St) ->
    St#st{selmode=Mode,sel=sort(Sel),sh=false}.

%%%
%%% Map over the selection, modifying the selected objects.
%%%

-spec map(Fun, #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> #we{}),
      Items :: item_set().

map(F, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    Shs = map_1(F, Sel, Shs1, St, []),
    St#st{shapes=Shs}.

%%
%% Map over the selection, modifying the objects and the selection.
%%

-spec map_update_sel(Fun, sel_mode(), #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> {#we{},OutItems}),
      InItems :: item_set(),
      OutItems :: item_set().

map_update_sel(F, Mode, St0) when is_function(F, 2) ->
    St = map_update_sel(F, St0),
    St#st{selmode=Mode}.

-spec map_update_sel(Fun, #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> {#we{},OutItems}),
      InItems :: item_set(),
      OutItems :: item_set().

map_update_sel(F, St0) when is_function(F, 2) ->
    MF = fun(Sel0, #we{id=Id}=We0, Acc) ->
		 {We,Sel} = F(Sel0, We0),
		 case gb_sets:is_empty(Sel) of
		     true -> {We,Acc};
		     false -> {We,[{Id,Sel}|Acc]}
		 end
	 end,
    {St,Sel} = mapfold(MF, [], St0),
    set(Sel, St).

%%
%% Map over the selection, modifying the selection.
%%

-spec update_sel(Fun, sel_mode(), #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> OutItems),
      InItems :: item_set(),
      OutItems :: item_set().

update_sel(F, Mode, St0) when is_function(F, 2) ->
    St = update_sel(F, St0),
    St#st{selmode=Mode}.

-spec update_sel(Fun, #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> Items),
      Items :: gb_sets:set(item_id()).

update_sel(F, #st{sel=Sel0,shapes=Shapes}=St) when is_function(F, 2) ->
    Sel = update_sel_1(Sel0, F, Shapes),
    set(Sel, St).


%%%
%%% Distributed fold over the selection. The Map function
%%% will be called in process holding the #we{}. The
%%% Reduce function will be called in the main Wings
%%% process.
%%%

-spec dfold(Map, Reduce, Acc0, #st{}) -> Acc1 when
      Map :: fun((InItems, #we{}) -> Int),
      Reduce :: fun((Int, AccIn) -> AccOut),
      InItems :: item_set(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

dfold(Map, Reduce, Acc0, St) when is_function(Map, 2),
				  is_function(Reduce, 2) ->
    #st{sel=Sel,shapes=Shapes} = St,
    dfold_1(Sel, Map, Reduce, Shapes, Acc0).

dfold_1([{Id,Items}|T], Map, Reduce, Shapes, Acc0) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    Int = Map(Items, We),
    Acc = Reduce(Int, Acc0),
    dfold_1(T, Map, Reduce, Shapes, Acc);
dfold_1([], _, _, _, Acc) -> Acc.


%%%
%%% Fold over the selection.
%%%

-spec fold(Fun, Acc0, #st{}) -> Acc1 when
      Fun :: fun((Items, #we{}, AccIn) -> AccOut),
      Items :: item_set(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

fold(F, Acc, #st{sel=Sel,shapes=Shapes}) ->
    fold_1(F, Acc, Shapes, Sel).

%%%
%%% Map and fold over the selection.
%%%

-spec mapfold(Fun, Acc0, #st{}) -> {#st{},Acc1} when
      Fun :: fun((Items, #we{}, AccIn) -> {#we{},AccOut}),
      Items :: item_set(),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

mapfold(F, Acc0, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    {Shs,Acc} = mapfold_1(F, Acc0, Sel, Shs1, St, []),
    {St#st{shapes=Shs},Acc}.

%%
%% Construct a new selection based on all visible geometry.
%%
%% Light can only be selected in body mode.
%%

-spec new_sel(Fun, sel_mode(), #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> OutItems),
      InItems :: item_set(),
      OutItems :: item_set().

new_sel(F, Mode, #st{shapes=Shapes}=St) when is_function(F, 2) ->
    Sel0 = gb_trees:values(Shapes),
    Sel = new_sel_1(Sel0, F, Mode),
    St#st{selmode=Mode,sel=Sel}.

%% make(Filter, Mode, St) -> [{Id,ItemGbSet}].
%%      Mode = body|face|edge|vertex
%%      Filter(Item, We) -> true|false
%%      Item = face() | edge() | vertex() | 0
%%  Construct a selection by calling Filter(Item, We) for each
%%  item in each object (where Item is either a face number,
%%  edge number, vertex number, or 0 depending on Mode).
%%

-spec make(Fun, sel_mode(), #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> boolean()),
      Items :: item_set().

make(Filter, Mode, St) when is_function(Filter, 2) ->
    new_sel(fun(Sel, We) ->
		    gb_sets:filter(fun(Item) -> Filter(Item, We) end, Sel)
	    end, Mode, St).

%%%
%%% Calculate the center for all selected objects.
%%%

-spec center(#st{}) -> e3d_vector().

center(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
		 Vs = to_vertices(Mode, Items, We),
		 wings_vertex:center(Vs, We)
	 end,
    RF = fun(V, {N,Acc}) -> {N+1,e3d_vec:add(V, Acc)} end,
    {N,Sum} = dfold(MF, RF, {0,e3d_vec:zero()}, St),
    e3d_vec:divide(Sum, N).


%%%
%%% Calculate the center for all selected vertices.
%%%

-spec center_vs(#st{}) -> e3d_vector().

center_vs(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
		 N = gb_sets:size(Items),
		 Vs = to_vertices(Mode, Items, We),
		 Center = wings_vertex:center(Vs, We),
		 {N,e3d_vec:mul(Center, float(N))}
	 end,
    RF = fun({N0,C0}, {N1,C1}) -> {N0+N1,e3d_vec:add(C0, C1)} end,
    {N,Sum} = dfold(MF, RF, {0,e3d_vec:zero()}, St),
    e3d_vec:divide(Sum, N).

%% Calculate center of bounding box.

-spec bbox_center(#st{}) -> e3d_vector().

bbox_center(St) ->
    BBox = bounding_box(St),
    e3d_vec:average(BBox).

%%%
%%% Calculate the bounding-box for the selection.
%%%

-spec bounding_box(#st{}) -> [e3d_vector()] | 'none'.

bounding_box(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
		 Vs = to_vertices(Mode, Items, We),
		 wings_vertex:bounding_box(Vs, We)
	 end,
    RF = fun(Box, none) -> Box;
	    (Box, Acc) -> e3d_vec:bounding_box(Box++Acc)
	 end,
    dfold(MF, RF, none, St).

%%%
%%% Calculate the bounding boxes for all selected objects.
%%%
%%% FIXME: The name is strange. Maybe bbox_centers/1?

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

-spec face_regions(Faces, #we{}) -> [face_set()] when
      Faces :: face_set() | [visible_face_num()].

face_regions(Faces, We) when is_list(Faces) ->
    face_regions_1(gb_sets:from_list(Faces), We);
face_regions(Faces, We) ->
    face_regions_1(Faces, We).

%%%
%%% Divide the face selection into regions where each face shares at least
%%% one vertex with another face in the same region.
%%%

-spec strict_face_regions(Faces, #we{}) -> [face_set()] when
      Faces :: face_set() | [visible_face_num()].

strict_face_regions(Faces, We) when is_list(Faces) ->
    find_strict_face_regions(gb_sets:from_list(Faces), We, []);
strict_face_regions(Faces, We) ->
    find_strict_face_regions(Faces, We, []).

%%%
%%% Here we want to divide the selection into regions of connected edges.
%%% We use a standard working-set algorithm.
%%%

-spec edge_regions(Edges, #we{}) -> [edge_set()] when
      Edges :: edge_set() | [edge_num()].

edge_regions(Edges, We) when is_list(Edges) ->
    find_edge_regions(gb_sets:from_list(Edges), We, []);
edge_regions(Edges, We) ->
    find_edge_regions(Edges, We, []).

%%%
%%% Validate selection.
%%%

-spec valid_sel(#st{}) -> #st{}.

valid_sel(#st{sel=Sel,selmode=Mode}=St) ->
    St#st{sel=valid_sel(Sel, Mode, St)}.

-spec valid_sel(SelIn, sel_mode(), #st{}) -> SelOut when
      SelIn :: [{item_id(),item_set()}],
      SelOut :: [{item_id(),item_set()}].

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

-spec select_object(obj_id(), #st{}) -> #st{}.

select_object(Id, #st{selmode=Mode,sel=Sel0}=St) ->
    case keymember(Id, 1, Sel0) of
	true -> St;
	false ->
	    Sel = sort([{Id,get_all_items(Mode, Id, St)}|Sel0]),
	    St#st{sel=Sel}
    end.

-spec deselect_object(obj_id(), #st{}) -> #st{}.

deselect_object(Id, #st{sel=Sel0}=St) ->
    Sel = keydelete(Id, 1, Sel0),
    St#st{sel=Sel}.

-spec inverse_items(sel_mode(), item_set(), #we{}) -> item_set().

inverse_items(Mode, Elems, We) ->
    gb_sets:difference(get_all_items(Mode, We), Elems).

-spec get_all_items(sel_mode(), obj_id(), #st{}) -> item_set().

get_all_items(Mode, Id, #st{shapes=Shapes}) ->
    We = gb_trees:get(Id, Shapes),
    get_all_items(Mode, We).

-spec to_vertices(sel_mode(), item_set(), #we{}) -> [vertex_num()].

to_vertices(vertex, Vs, _) -> Vs;
to_vertices(face, Faces, We) ->
    wings_face:to_vertices(Faces, We);
to_vertices(edge, Edges, We) ->
    wings_edge:to_vertices(Edges, We);
to_vertices(body, _, #we{vp=Vtab}) ->
    wings_util:array_keys(Vtab).

%%%
%%% Local functions.
%%%

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

update_sel_1([{Id,Sel0}|T], F, Shapes) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    Sel = F(Sel0, We),
    case gb_sets:is_empty(Sel) of
	false ->
	    [{Id,Sel}|update_sel_1(T, F, Shapes)];
	true ->
	    update_sel_1(T, F, Shapes)
    end;
update_sel_1([], _, _) -> [].

fold_1(F, Acc0, Shapes, [{Id,Items}|T]) ->
    We = gb_trees:get(Id, Shapes),
    ?ASSERT(We#we.id =:= Id),
    fold_1(F, F(Items, We, Acc0), Shapes, T);
fold_1(_F, Acc, _Shapes, []) -> Acc.

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

new_sel_1([#we{perm=Perm}|Shs], F, Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    new_sel_1(Shs, F, Mode);
new_sel_1([We|Shs], F, Mode) when ?IS_LIGHT(We), Mode =/= body ->
    new_sel_1(Shs, F, Mode);
new_sel_1([#we{id=Id}=We|Shs], F, Mode) ->
    Sel0 = get_all_items(Mode, We),
    Sel = F(Sel0, We),
    case gb_sets:is_empty(Sel) of
	false ->
	    [{Id,Sel}|new_sel_1(Shs, F, Mode)];
	true ->
	    new_sel_1(Shs, F, Mode)
    end;
new_sel_1([], _, _) -> [].



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

validate_items(Items, body, _We) -> Items;
validate_items(Items, Mode, We) ->
    gb_sets:intersection(Items, get_all_items(Mode, We)).

get_all_items(vertex, We) ->
    gb_sets:from_ordset(wings_we:visible_vs(We));
get_all_items(edge, We) ->
    gb_sets:from_ordset(wings_we:visible_edges(We));
get_all_items(face, We) ->
    gb_sets:from_ordset(wings_we:visible(We));
get_all_items(body, _) ->
    gb_sets:singleton(0).
