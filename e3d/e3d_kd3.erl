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
%% 	 enter/3,
	 delete/2, delete_object/2,
	 nearest/2, take_nearest/2,
	 fold/5
%% 	 map/2
	]).

-define(REBALANCE, 0.7). %% Rebalance after % deletions
-define(NODE(Point,Axis,Left,Right), {Point,Axis,Left,Right}). 

-type point()   :: {number(), number(), number()}.
-type object()  :: {point(), term()}.

%% Internal 
-type tree() ::  [object()] |
		   {Med   :: float(), 
		    Axis  :: integer(),
		    Left  :: tree(), 
		    Right :: tree()}.

-record(e3d_kd3, { tree=[] :: tree() }).
  %%, total=0 :: integer(), deleted=0 :: integer()}).

%%% @spec () -> kd-tree().
%%% @doc Returns an empty Tree.
empty() -> #e3d_kd3{tree=nil}.

%%% @spec (Tree::kd-tree()) -> boolean().
%%% @doc Returns true if Tree is an empty tree.
is_empty(#e3d_kd3{tree=nil}) -> true;
is_empty(#e3d_kd3{}) -> false.

is_kd3(#e3d_kd3{}) -> true;
is_kd3(_) -> false.

%%% @spec (Tree::kd-tree()) -> integer().
%%% @doc Returns the number of objects in the Tree.
%%% Note: This function currently traverses the tree.
size(#e3d_kd3{tree=Tree}) ->
    size_1(Tree, 0).

size_1({_, _, L0, R0}, Sz) ->
    size_1(L0, size_1(R0, Sz));
size_1(List, Sz) ->
    Sz + length(List).

%%% @spec (point(), Tree1::e3d_kd3()) -> Tree2::e3d_kd3().
%%% @doc Removes all the node(s) with key Point from Tree1 and returns the new Tree2
%%% assumes the Key is present otherwise it crashes.
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
delete_1(Key, [{Key,_}|_]) -> [].

delete_2(_, _, [], R)    -> R;
delete_2(_, _, L, [])    -> L;
delete_2(Med, Axis, L, R) -> ?NODE(Med,Axis,L,R).
    
%%% @spec (object(), Tree1::e3d_kd3()) -> Tree2::e3d_kd3().
%%% @doc Removes the Object from Tree1 and returns the new Tree2
%%% crashes if object is not present.
delete_object({{_,_,_},_} = Object, _T=#e3d_kd3{tree=Tree}) ->
    #e3d_kd3{tree=delete_object_1(Object, Tree)}.

delete_object_1({Key,_}=Object,{Med, Axis, L0, R0}) ->
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

%%% @spec ([object()]) -> e3d_kd3().
%%% @doc Builds a kd-tree from a list of objects.
from_list([]) -> 
    #e3d_kd3{tree=[]};
from_list(List) ->
    {_N,BB} = lists:foldl(fun({Point, _}, {N,BB}) -> {N+1,e3d_bv:union(BB, Point)} end, 
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

%%% @spec (e3d_kd3()) -> [object()]
%%% @doc  Return all nodes in the tree.
to_list(#e3d_kd3{tree=Tree}) ->
    to_list(Tree, []).

to_list({_,_, L,R}, Acc0) ->
    to_list(L,to_list(R,Acc0));
to_list([], Acc) -> Acc;
to_list(Data, Acc) -> Data++Acc.

%%% @spec (Point::tuple(), Tree::e3d_kd3()) -> {object(), e3d_kd3()} | undefined
%%% @doc Returns the Object nearest the Point and a Tree with the Object deleted,
%%% or undefined
take_nearest({_,_,_}, #e3d_kd3{tree=[]}) -> undefined;
take_nearest({_,_,_} = Point, Orig = #e3d_kd3{tree=Tree}) ->
    [_|[Node|_]] = nearest_1(Point, Tree, [undefined|undefined]),
    {Node, delete_object(Node,Orig)}.

%%% @spec (Key::point(), Tree::e3d_kd3()) -> object() | undefined
%%% @doc Returns the object nearest the Key, or undefined if table is empty.
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
nearest_1(Point,[{Pos,_}|_] = Leaf, Closest0) ->
    closest([e3d_vec:dist_sqr(Pos, Point)|Leaf],Closest0);
nearest_1(_, [], Close) -> Close.

nearest_2(Point, ThisSide, OtherSide, Border, Closest0) ->
    case nearest_1(Point, ThisSide, Closest0) of
	Closest = [Dist|_] when Dist < Border ->
	    Closest;
	Closest ->
	    nearest_1(Point, OtherSide, Closest)
    end.
    
%%% @spec (Fun/2, Acc0::term(), Key::point(), Dist, Tree::e3d_kd3()) -> Acc::term()
%%% @doc Traverse all Objects within Dist distance from point, notice that the order 
%%% is not specified.
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
fold_1(List = [{Pos,_}|_], Point, Dist2, Fun, Acc) ->
    case e3d_vec:dist_sqr(Pos, Point) =< Dist2 of
	true -> 
	    lists:foldl(Fun, Acc, List);
	false ->
	    Acc
    end.

%%% Internal stuff  %%%
split(List, Axis, Pos) ->
    split(List, Axis, Pos, [], e3d_bv:box(), [], e3d_bv:box()).

split([H = {V,_}|T], Axis, Pos, L, LB, R, RB) ->
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

