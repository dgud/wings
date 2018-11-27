%%
%%  e3d_kd3.erl --
%%
%%     Kd tree implementation.
%%
%%  Copyright (c) 2009-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

%%% @doc kd-tree from wikipedia 1 Jul 2009
%%%      Builds and traverses kd-tree of dimension 3 i.e. keys are {X,Y,Z}
%%%
%%%      Each object is a 2-tuple, {point(), value} where point is a tuple
%%%      of size 3 and value is any term.
%%%
%%%      Currently without any balencing or optimizations
%%%
%%% @end

%% Implementation:
%%  I have placed the data in the leafs and build a slightly larger
%%  record in the nodes to avoid re-balancing nodes after removing 
%%  an element which is needed standard algorithm.
%%  Since take_nearest is probably the most used function in the api.

-module(e3d_kd3).

-export([from_list/1, to_list/1,
 	 empty/0,is_empty/1,is_kd3/1,size/1,
 	 enter/3, update/3,
	 delete/2, delete_object/2,
	 nearest/2, take_nearest/2,
	 fold/5
%% 	 map/2
	]).

-include("e3d.hrl").  %% For types

-define(REBALANCE, 0.7). %% Rebalance after % deletions
-define(NODE(Point,Axis,Left,Right), {Point,Axis,Left,Right}).

-type kd3_object()  :: {term(), e3d_vec:point()}.

%% Internal
-type tree() ::  [kd3_object()] |
		   {Med   :: float(),
		    Axis  :: integer(),
		    Left  :: tree(),
		    Right :: tree()}.

-record(e3d_kd3, { tree=[] :: tree() }).
-type e3d_kd3() :: #e3d_kd3{}.

-export_type([e3d_kd3/0]).

%%% @doc Returns an empty Tree.
-spec empty() -> e3d_kd3().
empty() -> #e3d_kd3{tree=[]}.

%%% @doc Returns true if Tree is an empty tree.
-spec is_empty(e3d_kd3()) -> boolean().
is_empty(#e3d_kd3{tree=[]}) -> true;
is_empty(#e3d_kd3{}) -> false.

-spec is_kd3(e3d_kd3()) -> boolean().
is_kd3(#e3d_kd3{}) -> true;
is_kd3(_) -> false.

%%% @doc Returns the number of objects in the Tree.
%%% Note: This function currently traverses the tree.
-spec size(e3d_kd3()) -> integer().
size(#e3d_kd3{tree=Tree}) ->
    size_1(Tree, 0).

size_1({_, _, L0, R0}, Sz) ->
    size_1(L0, size_1(R0, Sz));
size_1(List, Sz) ->
    Sz + length(List).

%%% @doc Removes all the node(s) with key Point from Tree1 and returns the new Tree2
%%% assumes the Key is present otherwise it crashes.
-spec delete(e3d_vec:point(), e3d_kd3()) -> e3d_kd3().
delete({_,_,_} = Point, #e3d_kd3{tree=Tree}) ->
    #e3d_kd3{tree=delete_1(Point, Tree)}.

delete_1(Key,{Med, Axis, L0, R0}) ->
    case Med < element(Axis, Key)  of
	true ->
	    R = delete_1(Key, R0),
	    delete_2(Med, Axis, L0, R);
	false ->
	    L = delete_1(Key, L0),
	    delete_2(Med, Axis, L, R0)
    end;
delete_1(Key, [{_,Key}|_]) -> [].

delete_2(_, _, [], R)    -> R;
delete_2(_, _, L, [])    -> L;
delete_2(Med, Axis, L, R) -> ?NODE(Med,Axis,L,R).

%%% @doc Removes the Object from Tree1 and returns the new Tree2
%%% crashes if object is not present.
-spec delete_object(kd3_object(), e3d_kd3()) -> e3d_kd3().
delete_object({_,{_,_,_}} = Object, _T=#e3d_kd3{tree=Tree}) ->
    #e3d_kd3{tree=delete_object_1(Object, Tree)}.

delete_object_1({_,Key}=Object,{Med, Axis, L0, R0}) ->
    case Med < element(Axis, Key)  of
	true ->
	    R = delete_object_1(Object, R0),
	    delete_object_2(Med, Axis, L0, R);
	false ->
	    L = delete_object_1(Object, L0),
	    delete_object_2(Med, Axis, L, R0)
    end;
delete_object_1(Object, Leaf) -> 
    delete_object_3(Object, Leaf, []).

delete_object_2(_, _, [], R)    -> R;
delete_object_2(_, _, L, [])    -> L;
delete_object_2(Med, Axis, L, R) -> ?NODE(Med,Axis,L,R).

delete_object_3(Object, [Object|R], Acc) -> Acc ++ R;
delete_object_3(Object, [H|T], Acc) ->
    delete_object_3(Object, T, [H|Acc]).

%% rebalence(Tree = #e3d_kd3{deleted=D, total=T}) when T > 1000, D/T > ?REBALANCE ->        
%%     from_list(to_list(Tree));
%%rebalence(T) -> T.

%%% @doc Builds a kd-tree from a list of objects.
-spec from_list([kd3_object()]) -> e3d_kd3().
from_list([]) ->
    #e3d_kd3{tree=[]};
from_list(List) ->
    {_N,BB} = lists:foldl(fun({_, Point}, {N,BB}) -> {N+1,e3d_bv:union(BB, Point)} end,
			  {0,e3d_bv:box()}, List),
    #e3d_kd3{tree=from_list(List, BB)}. %, total=N}.

from_list(List = [_|_], BB) ->
    case e3d_bv:max_extent(BB) of
	undefined -> %% All positions are exactly the same
	    List;
	Axis ->
	    Split = element(Axis, e3d_bv:center(BB)),
	    {Left,LBB,Right,RBB} = split(List, Axis, Split),
	    ?NODE(Split, Axis, %(Len bsl 2) bor Axis,
		  from_list(Left,  LBB),
		  from_list(Right, RBB))
    end;
from_list(Data, _) -> Data.

%%% @doc  Add node to the tree.
-spec enter(e3d_vec:point(), term(), e3d_kd3()) -> e3d_kd3().
enter(Point, Data, #e3d_kd3{tree=Tree0}) ->
    Tree = add_1({Data, Point}, Tree0),
    #e3d_kd3{tree=Tree}.

%%% @doc Updates the Object with Term from Tree1 and returns the new Tree2
%%% crashes if object is not present.
-spec update(kd3_object(), term(), e3d_kd3()) -> e3d_kd3().
update({_,{_,_,_}} = Object, Val, _T=#e3d_kd3{tree=Tree}) ->
    #e3d_kd3{tree=update_1(Object, Val, Tree)}.

update_1({_,Key}=Object,Val,{Med, Axis, L0, R0}) ->
    case Med < element(Axis, Key)  of
	true ->
	    R = update_1(Object, Val, R0),
	    delete_object_2(Med, Axis, L0, R);
	false ->
	    L = update_1(Object, Val, L0),
	    delete_object_2(Med, Axis, L, R0)
    end;
update_1(Object, Val, Leaf) ->
    update_3(Object, Leaf, Val, []).

update_3({_,Pos}=Object, [Object|R], Val, Acc) -> Acc ++ [{Val,Pos}|R];
update_3(Object, [H|T], Val, Acc) ->
    update_3(Object, T, Val, [H|Acc]).

%%% @doc  Return all nodes in the tree.
-spec to_list(e3d_kd3()) -> [kd3_object()].
to_list(#e3d_kd3{tree=Tree}) ->
    to_list(Tree, []).

to_list({_,_, L,R}, Acc0) ->
    to_list(L,to_list(R,Acc0));
to_list([], Acc) -> Acc;
to_list(Data, Acc) -> Data++Acc.

%%% @spec (Point::tuple(), Tree::e3d_kd3()) -> {kd3_object(), e3d_kd3()} | undefined
%%% @doc Returns the Object nearest the Point and a Tree with the Object deleted,
%%% or undefined
-spec take_nearest(e3d_vec:point(), Tree::e3d_kd3()) -> {kd3_object(), e3d_kd3()} | undefined.
take_nearest({_,_,_}, #e3d_kd3{tree=[]}) -> undefined;
take_nearest({_,_,_} = Point, Orig = #e3d_kd3{tree=Tree}) ->
    [_|[Node|_]=_Closest] = nearest_1(Point, Tree, [undefined|undefined]),
    %%Foo = [P1|| {P1,_} <- _Closest],
    %io:format("Pick: ~p ~p~n=> ~p~n", [Point, Point =:= hd(Foo), Closest]),
    {Node, delete_object(Node,Orig)}.

%%% @doc Returns the object nearest the Key, or undefined if table is empty.
-spec nearest(Key::e3d_vec:point(), Tree::e3d_kd3()) -> kd3_object() | undefined.
nearest({_,_,_} = Point, #e3d_kd3{tree=Tree}) ->
    [_|[Node|_]] = nearest_1(Point, Tree, [undefined|undefined]),
    Node.

nearest_1(Point, {SplitPos,Axis,L,R}, Closest) ->
    PointPos = element(Axis, Point),
    BorderDist = (PointPos - SplitPos),
    Border   = BorderDist * BorderDist,
    case SplitPos < PointPos of
	true  -> nearest_2(Point, R, L, Border, Closest);
	false -> nearest_2(Point, L, R, Border, Closest)
    end;
nearest_1(Point,[{_,Pos}|_] = Leaf, Closest0) ->
    closest([e3d_vec:dist_sqr(Pos, Point)|Leaf],Closest0);
nearest_1(_, [], Close) -> Close.

nearest_2(Point, ThisSide, OtherSide, Border, Closest0) ->
    case nearest_1(Point, ThisSide, Closest0) of
	Closest = [Dist|_] when Dist < Border ->
	    Closest;
	Closest ->
	    nearest_1(Point, OtherSide, Closest)
    end.

%%% @doc Traverse all Objects within Dist distance from point, notice that the order
%%% is not specified.
-spec fold(fun((kd3_object(),Acc::term()) -> term()), term(),
               e3d_vec:point(), float(), e3d_kd3()) -> term().
fold(Fun, Acc, {_,_,_} = Point, Dist, #e3d_kd3{tree=Tree}) ->
    fold_1(Tree, Point, Dist*Dist, Fun, Acc).

fold_1({SplitPos,Axis,L,R}, Point, Dist2, Fun, Acc0) ->
    PointPos = element(Axis, Point),
    BorderDist = (PointPos - SplitPos),
    Border   = BorderDist * BorderDist,
    if Border < Dist2 ->
	    Acc = fold_1(R, Point, Dist2, Fun, Acc0),
	    fold_1(L, Point, Dist2, Fun, Acc);
       SplitPos < PointPos ->
	    fold_1(R, Point, Dist2, Fun, Acc0);
       true -> 
	    fold_1(L, Point, Dist2, Fun, Acc0)
    end;
fold_1([], _Point, _Dist2, _Fun, Acc) -> Acc;
fold_1(List = [{_,Pos}|_], Point, Dist2, Fun, Acc) ->
    case e3d_vec:dist_sqr(Pos, Point) =< Dist2 of
	true ->
	    lists:foldl(Fun, Acc, List);
	false ->
	    Acc
    end.

%%% Internal stuff  %%%
split(List, Axis, Pos) ->
    split(List, Axis, Pos, [], e3d_bv:box(), [], e3d_bv:box()).

split([H = {_,V}|T], Axis, Pos, L, LB, R, RB) ->
    case element(Axis, V) =< Pos of
	true ->
	    split(T, Axis, Pos, [H|L], e3d_bv:union(LB,V), R, RB);
	false ->
	    split(T, Axis, Pos, L, LB, [H|R], e3d_bv:union(RB,V))
    end;
split([], _, _, L,LB,R,RB) ->
    {L,LB,R,RB}.

closest([Dist1|_]=Node1, [Dist2|_]) when Dist1 < Dist2 -> Node1;
closest(_, Node) -> Node.

add_1(P0, []) -> [P0];
add_1({_,P0}=Obj, [{_,P1}|_]=T0) ->
    BB = e3d_bv:box(P0, P1),
    case e3d_bv:max_extent(BB) of
	undefined -> %% All positions are exactly the same
	    [P0|T0];
	Axis ->
	    Split = element(Axis, e3d_bv:center(BB)),
	    {Left,LBB,Right,RBB} = split([Obj|T0], Axis, Split),
	    ?NODE(Split, Axis, %(Len bsl 2) bor Axis,
		  from_list(Left,  LBB),
		  from_list(Right, RBB))
    end;
add_1({_,P0}=Obj, {SplitPos, Axis, L, R}) ->
    PointPos = element(Axis, P0),
    case SplitPos < PointPos of
	true  -> {SplitPos, Axis, L, add_1(Obj, R)};
	false -> {SplitPos, Axis, add_1(Obj, L), R}
    end.


