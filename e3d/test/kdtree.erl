%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% @copyright (C) 2009, Dan Gudmundsson
%%% @doc kd-tree from wikipedia 1 Jul 2009
%%%      Builds and traverses kd-tree of any dimension larger than 1,
%%%      
%%%      Each node is a 2-tuple, {key, value} where key is a tuple
%%%      of numbers and value is any term.
%%%
%%%      Currently without any balencing or optimizations  
%%%      The dimension of nodes must be the same for all nodes.
%%% @end
%%% Created :  1 Jul 2009 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------

-module(kdtree).

-export([from_list/1, to_list/1,
	 empty/0,is_empty/1,
	 enter/3, delete/2,
	 nearest/2, take_nearest/2,
	 map/2
	]).

-record(kdt, {med,
	      left  = nil,
	      right = nil}).

%%% @spec (Tree::kd-tree()) -> boolean().
%%% @doc Returns true if Tree is an empty tree.
is_empty(nil) -> true;
is_empty(#kdt{}) -> false.
     
%%% @spec () -> kd-tree().
%%% @doc Returns an empty Tree.
empty() -> nil.

%%% @spec (Key::tuple(), Val::term(), Tree1::kd-tree()) -> Tree2::kd-tree().
%%% @doc Inserts Key with value Val into Tree1 if the Key is not present in the tree, 
%%% otherwise it updates the value.
enter(Key, Val, nil) when is_tuple(Key) ->
    #kdt{med={Key,Val}};
enter(Key, Val, #kdt{}=Tree) when is_tuple(Key) ->
    enter_1(Key, Val, 1, Tree).

enter_1(Key, Val, _Axis, #kdt{med=Key}=Tree) ->
    Tree#kdt{med={Key,Val}};
enter_1(Key, Val, Axis, #kdt{med={Med,_}, left=L0, right=R0}=Tree) ->
    case element(Axis, Key) > element(Axis, Med) of
	true ->
	    R = enter_1(Key, Val, next_axis(Axis, size(Key)), R0),
	    Tree#kdt{right=R};
	false ->
	    L = enter_1(Key, Val, next_axis(Axis, size(Key)), L0),
	    Tree#kdt{left=L}
    end;
enter_1(Key, Val, _Axis, nil) ->
    {Key,Val};
enter_1(Key, Val, Axis, {PKey,_} = Prev) -> 
    case element(Axis, Key) > element(Axis, PKey) of
	true ->  #kdt{med={Key,Val}, left=Prev};
	false -> #kdt{med=Prev, left={Key,Val}}
    end.

%%% @spec (Key::tuple(), Val::term(), Tree1::kd-tree()) -> Tree2::kd-tree().
%%% @doc Removes the node with key Key from Tree1 and returns the new Tree2
%%% assumes the Key is present otherwise it crashes.
delete(Key, #kdt{}=Tree) when is_tuple(Key) ->
    case delete_1(Key, 1, Tree) of
	#kdt{} = Tree2 -> Tree2;
	nil  -> nil;
	Node -> #kdt{med=Node}
    end.

delete_1(Key, Axis, #kdt{med={Key,_}, left=L, right=R}) ->
    Nodes = to_list(L, to_list(R, [])),
    Length = length(Nodes),
    from_list(Nodes, Length, Axis);
delete_1(Key, Axis, #kdt{med={PKey,_}, left=L0, right=R0}=Tree) ->
    case element(Axis, Key) > element(Axis, PKey) of
	true ->
	    R = delete_1(Key, next_axis(Axis,size(Key)), R0),
	    Tree#kdt{right=R};
	false ->
	    L = delete_1(Key, next_axis(Axis,size(Key)), L0),
	    Tree#kdt{left=L}
    end;
delete_1(Key, _, {Key,_}) -> nil.
    
%%% (Fun::function(), Tree1::kd-tree()) -> Tree2::kd-tree().
%%% @doc  Maps the function Fun(K, V1) -> V2
%%% to all key-value pairs of the tree Tree1 and returns
%%% a new tree Tree2 with the same set of keys as Tree1 and the new set of values V2.

map(_F, nil) -> nil;
map(F, #kdt{}=Tree) ->
    map_1(F, Tree).

map_1(F, #kdt{med={Key,Val}, left=L, right=R}) ->
    #kdt{med={Key,F(Key,Val)}, left=map_1(F,L), right=map_1(F,R)};
map_1(_F, nil) -> nil;
map_1(F, {Key,Value}) -> {Key, F(Key,Value)}.

%%% @spec ([{tuple(), term()}]) -> kd-tree().
%%% @doc Builds a kd-tree from a list of nodes.
from_list([]) -> nil;
from_list(TupleList) when is_list(TupleList) ->
    case from_list(TupleList, length(TupleList), 1) of
	#kdt{} = Tree -> Tree;
	Node -> #kdt{med=Node}
    end.

from_list([], _, _) -> nil;
from_list([Data], _, _) -> Data;
from_list(List, Length, Axis) ->
    Ordered = sort(List, Axis),
    {{Point,_}=Med,LLen,Left,RLen,Right} = split(Length, Axis, Ordered),
    NextAxis = next_axis(Axis, size(Point)),    
    #kdt{med   = Med, 
	 left  = from_list(Left,  LLen, NextAxis),
	 right = from_list(Right, RLen, NextAxis)}.

%%% @spec (kd-tree()) -> [{tuple(), term()}]
%%% @doc  Return all nodes in the tree.
to_list(nil) -> [];
to_list(Tree=#kdt{}) ->
    to_list(Tree, []).

to_list(#kdt{med=Node,left=L,right=R}, Acc0) ->
    to_list(L,[Node|to_list(R,Acc0)]);
to_list(nil, Acc) -> Acc;
to_list(Data, Acc) -> [Data|Acc].

%%% @spec (Key::tuple(), Tree::kd-tree()) -> Node 
%%% @doc Returns the node nearest the Key, assumes the Tree is non-empty.
nearest(Point0, nil) when is_tuple(Point0) -> undefined;
nearest(Point0, #kdt{} = Tree) when is_tuple(Point0) ->
    Point = tuple_to_list(Point0),
    [_|Node] = nearest_1(Point, Tree, 1, [undefined|undefined]),
    Node.

%%% @spec (Key::tuple(), Tree::kd-tree()) -> {Node, Tree}
%%% @doc Returns the node nearest the Key and a Tree with the node deleted,
%%% assumes the Tree is non-empty.
take_nearest(Point0, nil) when is_tuple(Point0) -> undefined;
take_nearest(Point0, #kdt{} = Tree) when is_tuple(Point0) ->
    Point = tuple_to_list(Point0),
    [_|{Key,_}=Node] = nearest_1(Point, Tree, 1, [undefined|undefined]),
    {Node, delete(Key,Tree)}.

nearest_1(_, nil, _, Close) -> Close;
nearest_1(Point, #kdt{med={Split,_}=Node, left=L, right=R}, Axis, Closest0) ->
    PointPos = lists:nth(Axis, Point),
    SplitPos = element(Axis, Split),
    BorderDist = (PointPos - SplitPos),
    Border = BorderDist * BorderDist,
    Closest = closest([dist(Split, Point)|Node],Closest0),
    NextAxis = next_axis(Axis, size(Split)),

    case PointPos > SplitPos of
	true ->
	    nearest_2(Point, R, L, Border, NextAxis, Closest);
	false ->
	    nearest_2(Point, L, R, Border, NextAxis, Closest)
    end;
nearest_1(Point,{Pos,_} = Leaf,_, Closest0) ->
    closest([dist(Pos, Point)|Leaf],Closest0).
    

nearest_2(Point, ThisSide, OtherSide, Border, NextAxis, Closest0) ->
    case nearest_1(Point, ThisSide, NextAxis, Closest0) of
	Closest = [Dist|_] when Dist < Border ->
	    Closest;
	Closest ->
	    nearest_1(Point, OtherSide, NextAxis, Closest)
    end.

%%% Internal stuff  %%%
sort(List, Axis) ->
    lists:sort(fun({A,_}, {B,_}) ->
		       element(Axis, A) =< element(Axis,B)
	       end, List).

split(_, _Axis, [Node]) -> {Node, 0, [], 0, []};
split(Total, Axis, Records) ->
    Length = Total div 2,
    {RLeft,Right} = split_1(Length, Records, []),
    verify_med(RLeft, Right, Axis, Length, Total).

split_1(0, Right, ReverseLeft) ->
    {ReverseLeft,Right};
split_1(N, [L|Right], Acc) ->
    split_1(N-1, Right, [L|Acc]).

verify_med([Node|Right], [], _, _, Total) ->  %% All values the same on that axis
    {Node, Total-1, Right, 0, []};
verify_med([{L,_}|_]=Left0, [{R,_}=Node|Right], Axis, LeftLen, Total) ->
    case element(Axis, L) < element(Axis, R) of
	true -> 
	    {Node, LeftLen, Left0, Total-LeftLen-1, Right};
	false ->
	    verify_med([Node|Left0], Right, Axis, LeftLen+1, Total)
    end.

closest([Dist1|_]=Node1, [Dist2|_]) when Dist1 < Dist2 -> Node1;
closest(_, Node) -> Node.

dist(Point, Origin) ->
    dist_1(tuple_to_list(Point), Origin, 0).

dist_1([X|Xs],[Y|Ys], Acc) ->
    D = X-Y,
    dist_1(Xs, Ys, D*D+Acc);
dist_1([], [], Acc) -> Acc.

next_axis(Axis, Dim) ->
    1 + (Axis rem Dim).
