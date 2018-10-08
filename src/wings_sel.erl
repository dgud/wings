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
         selected_ids/1,unselected_ids/1,
	 map/2,map_obj/2,
         map_update_sel/2,map_update_sel/3,
	 update_sel/2,update_sel/3,update_sel_all/2,
         fold_obj/3,fold/3,dfold/4,mapfold/3,
	 new_sel/3,make/3,valid_sel/1,valid_sel/3,
         clone/2,clone/3,combine/2,combine/3,merge/2,
	 center/1,center_vs/1,
	 bbox_center/1,bounding_box/1,
	 face_regions/2,strict_face_regions/2,edge_regions/2,
	 select_object/2,deselect_object/2,
	 get_all_items/2,get_all_items/3,
	 inverse_items/3,to_vertices/3]).

-export_type([mode/0,
              edge_set/0,face_set/0,item_id/0,item_set/0,vertex_set/0]).

-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-import(lists, [foldl/3,reverse/1,reverse/2,sort/1,keydelete/3,keymember/3]).

-type mode() :: 'vertex' | 'edge' | 'face' | 'body'.

-type vertex_num() :: wings_vertex:vertex_num().
-type edge_num() :: wings_edge:edge_num().
-type visible_face_num() :: wings_face:visible_face_num().

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

-spec set(Mode, Sel, #st{}) -> #st{} when
      Mode :: mode(),
      Sel :: [{item_id(),item_set()}].

set(Mode, [], St) ->
    clear(St#st{selmode=Mode});
set(Mode, Sel, St) ->
    St#st{selmode=Mode,sel=sort(Sel),sh=false}.


%%
%% Return the Ids for all selected objects.
%%

-spec selected_ids(#st{}) -> [non_neg_integer()].

selected_ids(#st{sel=Sel}) ->
    [Id || {Id,_} <- Sel].

%%
%% Return the Ids for all selected objects.
%%

-spec unselected_ids(#st{}) -> [non_neg_integer()].

unselected_ids(#st{sel=Sel,shapes=Shs}) ->
    SelIds = [Id || {Id,_} <- Sel],
    AllIds = gb_trees:keys(Shs),
    ordsets:subtract(AllIds, SelIds).

%%%
%%% Map over the selection, modifying the selected #we{}
%%% records.
%%%

-spec map(Fun, #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> #we{}),
      Items :: item_set().

map(F, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = gb_trees:to_list(Shs0),
    Shs = map_1(F, Sel, Shs1, St, []),
    St#st{shapes=Shs}.


%%%
%%% Map over the selection, modifying the object maps.
%%%

-spec map_obj(F, #st{}) -> #st{} when
      F :: fun((InObj) -> OutObj),
      InObj :: wings_obj:obj(),
      OutObj :: wings_obj:obj().

map_obj(F, #st{sel=Sel}=St) ->
    SF = fun(#{id:=Id}=Obj) ->
                 case keymember(Id, 1, Sel) of
                     false -> Obj;
                     true -> F(Obj)
                 end
         end,
    wings_obj:map(SF, St).

%%
%% Map over the selection, modifying the objects and the selection.
%%

-spec map_update_sel(Fun, Mode, #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> {#we{},OutItems}),
      Mode :: mode(),
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

-spec update_sel(Fun, Mode, #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> OutItems),
      Mode :: mode(),
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

%%
%% Map over all objects, modifying the selection.
%%

-spec update_sel_all(Fun, #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> Items),
      Items :: gb_sets:set(item_id()).

update_sel_all(F, #st{sel=Sel0,shapes=Shapes}=St) when is_function(F, 2) ->
    Sel = update_sel_all_1(gb_trees:values(Shapes), Sel0, F),
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
%%% Fold over the selection of objects (not #we{} records).
%%%

-spec fold_obj(Fun, Acc0, #st{}) -> Acc1 when
      Fun :: fun((wings_obj:obj(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term().

fold_obj(F, Acc0, #st{sel=Sel}=St) ->
    FF = fun(#{id:=Id}=Obj, A) ->
                 case keymember(Id, 1, Sel) of
                     false -> A;
                     true -> F(Obj, A)
                 end
         end,
    wings_obj:fold(FF, Acc0, St).

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

-spec new_sel(Fun, Mode, #st{}) -> #st{} when
      Fun :: fun((InItems, #we{}) -> OutItems),
      Mode :: mode(),
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

-spec make(Fun, Mode, #st{}) -> #st{} when
      Fun :: fun((Items, #we{}) -> boolean()),
      Mode :: mode(),
      Items :: item_set().

make(Filter, Mode, St) when is_function(Filter, 2) ->
    new_sel(fun(Sel, We) ->
		    gb_sets:filter(fun(Item) -> Filter(Item, We) end, Sel)
	    end, Mode, St).


%%
%% Clone the selection.
%%

-type suffix() :: 'cut' | 'clone' | 'copy' | 'extract' | 'mirror' | 'sep'.
-type clone_item() :: {#we{},item_set(),suffix()}.
-type clone_out() :: {#we{},item_set(),[clone_item()]}.
-type clone_fun() :: fun((item_set(), #we{}) -> clone_out()).

-spec clone(Fun, #st{}) -> #st{} when
      Fun :: clone_fun().

clone(F, St0) ->
    MF = fun(Items, We, Acc) ->
                 clone_fun(F, Items, We, Acc)
         end,
    #st{sel=Sel} = St = fold(MF, St0#st{sel=[]}, St0),
    St#st{sel=sort(Sel)}.

-spec clone(Fun, Mode, #st{}) -> #st{} when
      Fun :: clone_fun(),
      Mode :: mode().

clone(Fun, Mode, St0) ->
    St = clone(Fun, St0),
    St#st{selmode=Mode}.

%%
%% Combine selected objects to one.
%%

-spec combine(Fun, #st{}) -> #st{} when
      Fun :: fun((item_set(), #we{}) -> {#we{},item_set()}).

combine(F, St) ->
    MF = fun(Wes, Sel, Mode) ->
                 Zipped = combine_zip(Wes, Sel, Mode),
                 {We,Items0} = wings_we:merge_root_set(Zipped),
                 Items1 = combine_items(Mode, Items0),
                 Items = gb_sets:from_ordset(Items1),
                 F(Items, We)
         end,
    comb_merge(MF, St).

-spec combine(Fun, Mode, #st{}) -> #st{} when
      Fun :: fun((item_set(), #we{}) -> {#we{},item_set()}),
      Mode :: mode().

combine(F, Mode, St0) ->
    St = combine(F, St0),
    St#st{selmode=Mode}.

-spec merge(Fun, #st{}) -> #st{} when
      Fun :: fun(({#we{},item_set()}) -> {#we{},item_set()}).

merge(F, St) when is_function(F, 1) ->
    MF = fun(Wes, Sel, _Mode) ->
                 Zipped = merge_zip(Wes, Sel),
                 F(Zipped)
         end,
    comb_merge(MF, St).

%%%
%%% Calculate the center for all selected objects.
%%%

-spec center(#st{}) -> e3d_vec:vector().

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

-spec center_vs(#st{}) -> e3d_vec:vector().

center_vs(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
		 Vs = to_vertices(Mode, Items, We),
                 N = length(Vs),
		 Center = wings_vertex:center(Vs, We),
		 {N,e3d_vec:mul(Center, float(N))}
	 end,
    RF = fun({N0,C0}, {N1,C1}) -> {N0+N1,e3d_vec:add(C0, C1)} end,
    {N,Sum} = dfold(MF, RF, {0,e3d_vec:zero()}, St),
    e3d_vec:divide(Sum, N).

%% Calculate center of bounding box.

-spec bbox_center(#st{}) -> e3d_vec:vector().

bbox_center(St) ->
    BBox = bounding_box(St),
    e3d_vec:average(BBox).

%%%
%%% Calculate the bounding-box for the selection.
%%%

-spec bounding_box(#st{}) -> [e3d_vec:vector()] | 'none'.

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

-spec valid_sel(SelIn, Mode, #st{}) -> SelOut when
      Mode :: mode(),
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

-spec inverse_items(Mode, InItems, #we{}) -> OutItems when
      Mode :: mode(),
      InItems :: item_set(),
      OutItems :: item_set().

inverse_items(Mode, Elems, We) ->
    gb_sets:difference(get_all_items(Mode, We), Elems).

-spec get_all_items(mode(), obj_id(), #st{}) -> item_set().

get_all_items(Mode, Id, #st{shapes=Shapes}) ->
    We = gb_trees:get(Id, Shapes),
    get_all_items(Mode, We).

-spec to_vertices(Mode, Items, #we{}) -> Vertices when
      Mode :: mode(),
      Items :: item_set(),
      Vertices :: [vertex_num()].

to_vertices(vertex, Vs, _) ->
    gb_sets:to_list(Vs);
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

update_sel_all_1([#we{id=Id}=We|Wes], [{Id,Items0}|Sel], F) ->
    Items = F(Items0, We),
    case gb_sets:is_empty(Items) of
	false ->
	    [{Id,Items}|update_sel_all_1(Wes, Sel, F)];
	true ->
            update_sel_all_1(Wes, Sel, F)
    end;
update_sel_all_1([#we{id=Id,perm=P}|Wes]=Wes0, Sel, F) ->
    if
        ?IS_SELECTABLE(P) ->
            update_sel_all_1(Wes0, [{Id,gb_sets:empty()}|Sel], F);
        true ->
            update_sel_all_1(Wes, Sel, F)
    end;
update_sel_all_1([], _, _) -> [].

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

clone_fun(F, Items0, #we{id=Id}=We0, #st{shapes=Shs0}=St0) ->
    {We,Items,New} = F(Items0, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St1 = St0#st{shapes=Shs},
    St = clone_add_sel(Items, Id, St1),
    clone_fun_add(New, St).

clone_fun_add([{#we{name=Name0}=We0,Items,Suffix}|T],
              #st{onext=Id,shapes=Shs0}=St0) ->
    Name = new_name(Name0, Suffix, Id),
    We = We0#we{id=Id,name=Name},
    Shs = gb_trees:insert(Id, We, Shs0),
    St1 = St0#st{shapes=Shs,onext=Id+1},
    St = clone_add_sel(Items, Id, St1),
    clone_fun_add(T, St);
clone_fun_add([], St) -> St.

clone_add_sel(Items, Id, #st{sel=Sel}=St) ->
    case gb_sets:is_empty(Items) of
        false -> St#st{sel=[{Id,Items}|Sel]};
        true -> St
    end.

new_name(OldName, Suffix0, Id) ->
    Suffix = suffix(Suffix0),
    Base = base(reverse(OldName)),
    reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

%% Note: Filename suffixes are intentionally not translated.
%% If we are to translate them in the future, base/1 below
%% must be updated to strip suffixes (both for the current language
%% and for English).

suffix(cut) -> "cut";
suffix(clone) -> "clone";
suffix(copy) -> "copy";
suffix(extract) -> "extract";
suffix(mirror) -> "mirror";
suffix(sep) -> "sep".

%% base_1(ReversedName) -> ReversedBaseName
%%  Given an object name, strip digits and known suffixes to
%%  create a base name. Returns the unchanged name if
%%  no known suffix could be stripped.

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1("enolc_"++Base) -> Base;			%"_clone"
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("rorrim_"++Base) -> Base;		%"_mirror"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1(_Base) -> error.

comb_merge(MF, #st{shapes=Shs0,selmode=Mode,sel=[{Id,_}|_]=Sel0}=St) ->
    Shs1 = sofs:from_external(gb_trees:to_list(Shs0), [{id,object}]),
    Sel1 = sofs:from_external(Sel0, [{id,dummy}]),
    Sel2 = sofs:domain(Sel1),
    {Wes0,Shs2} = sofs:partition(1, Shs1, Sel2),
    Wes = sofs:to_external(sofs:range(Wes0)),
    {We0,Items} = MF(Wes, Sel0, Mode),
    We = case lists:usort([wings_obj:get_folder(We) || We <- Wes]) of
             [Folder] -> wings_obj:set_folder(Folder, We0);
             _ -> We0 %% From different folders add to root folder
         end,
    Shs = gb_trees:from_orddict(sort([{Id,We}|sofs:to_external(Shs2)])),
    Sel = case gb_sets:is_empty(Items) of
              true -> [];
              false -> [{Id,Items}]
          end,
    St#st{shapes=Shs,sel=Sel}.

combine_zip([#we{id=Id}=We|Wes], [{Id,Items0}|Sel], Mode) ->
    RootSet = combine_root_set(Mode, Items0),
    [{We,RootSet}|combine_zip(Wes, Sel, Mode)];
combine_zip([], [], _) -> [].

combine_root_set(body, _Items) ->
    [];
combine_root_set(Mode, Items) ->
    [{Mode,Item} || Item <- gb_sets:to_list(Items)].

combine_items(body, _RootSet) ->
    [0];
combine_items(_, RootSet) ->
    ordsets:from_list([Item || {_,Item} <- RootSet]).

merge_zip([#we{id=Id}=We|Wes], [{Id,Items}|Sel]) ->
    [{We,Items}|merge_zip(Wes, Sel)];
merge_zip([], []) -> [].


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
