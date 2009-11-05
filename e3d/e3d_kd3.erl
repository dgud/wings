%%
%%  e3d_kd3.erl --
%%
%%     Kd tree implementation.
%%
%%  Copyright (c) 2009 Dan Gudmundsson
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
 	 empty/0,is_empty/1,is_kd3/1,
%% 	 enter/3,
	 delete/2,
	 nearest/2, take_nearest/2
%% 	 map/2
	]).

%-type(point(),     {X,Y,Z}).
%-type(object(),    {point(), term()}).
%-type(e3d_kd3(),   {e3d_kd3, node()}).

%% Internal 
%-type(node(),      {point(), integer(), node(), node()}).

%% -record(node, {med,
%% 	       axis,
%% 	       left  = nil,
%% 	       right = nil}).

-define(NODE(Point,Axis,Left,Right), {Point,Axis,Left,Right}). 
-define(NIL, nil).
-record(e3d_kd3, {tree=?NIL}).

%%% @spec () -> kd-tree().
%%% @doc Returns an empty Tree.
empty() -> #e3d_kd3{tree=nil}.

%%% @spec (Tree::kd-tree()) -> boolean().
%%% @doc Returns true if Tree is an empty tree.
is_empty(#e3d_kd3{tree=nil}) -> true;
is_empty(#e3d_kd3{}) -> false.

is_kd3(#e3d_kd3{}) -> true;
is_kd3(_) -> false.
    
%%% @spec (point(), Val::term(), Tree1::e3d_kd3()) -> Tree2::e3d_kd3().
%%% @doc Removes the node with key Point from Tree1 and returns the new Tree2
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
delete_1(Key, {Key,_}) -> ?NIL.

delete_2(_, _, ?NIL, R)    -> R;
delete_2(_, _, L, ?NIL)    -> L;
delete_2(Med, Axis, L, R) -> ?NODE(Med,Axis,L,R).
    
%%% @spec ([object()]) -> e3d_kd3().
%%% @doc Builds a kd-tree from a list of objects.
from_list(List) ->
    #e3d_kd3{tree=from_list(List, length(List), 1)}.

from_list([Data], 1, _) -> Data;
from_list(List, Len, Axis) ->
    Ordered = sort(List, Axis),
    {Med,LLen,Left,RLen,Right} = split(Len, Axis, Ordered),    
    NextAxis = next_axis(Axis),
    ?NODE(Med, Axis, %(Len bsl 2) bor Axis,
	  from_list(Left,  LLen, NextAxis),
	  from_list(Right, RLen, NextAxis));
from_list([], 0, _) -> nil.

%%% @spec (e3d_kd3()) -> [object()]
%%% @doc  Return all nodes in the tree.
to_list(#e3d_kd3{tree=Tree}) ->
    to_list(Tree, []).

to_list({_,_, L,R}, Acc0) ->
    to_list(L,to_list(R,Acc0));
to_list(?NIL, Acc) -> Acc;
to_list(Data, Acc) -> [Data|Acc].

%%% @spec (Point::tuple(), Tree::e3d_kd3()) -> {object(), e3d_kd3()} | undefined
%%% @doc Returns the Object nearest the Point and a Tree with the Object deleted,
%%% or undefined
take_nearest({_,_,_}, #e3d_kd3{tree=?NIL}) -> undefined;
take_nearest({_,_,_} = Point, Orig = #e3d_kd3{tree=Tree}) ->
    [_|{Key,_}=Node] = nearest_1(Point, Tree, [undefined|undefined]),
    {Node, delete(Key,Orig)}.

%%% @spec (Key::point(), Tree::e3d_kd3()) -> object() | undefined
%%% @doc Returns the object nearest the Key, or undefined if table is empty.
nearest({_,_,_} = Point, #e3d_kd3{tree=Tree}) ->
    [_|Node] = nearest_1(Point, Tree, [undefined|undefined]),
    Node.

nearest_1(Point, {SplitPos,Axis0,L,R}, Closest) ->
    Axis = axis(Axis0), 
    PointPos = element(Axis, Point),
    BorderDist = (PointPos - SplitPos),
    Border   = BorderDist * BorderDist,
    case SplitPos < PointPos of
	true  -> nearest_2(Point, R, L, Border, Closest);
	false -> nearest_2(Point, L, R, Border, Closest)
    end;
nearest_1(Point,{Pos,_} = Leaf, Closest0) ->
    closest([e3d_vec:dist_sqr(Pos, Point)|Leaf],Closest0);
nearest_1(_, ?NIL, Close) -> Close.

nearest_2(Point, ThisSide, OtherSide, Border, Closest0) ->
    case nearest_1(Point, ThisSide, Closest0) of
	Closest = [Dist|_] when Dist < Border ->
	    Closest;
	Closest ->
	    nearest_1(Point, OtherSide, Closest)
    end.
    
%%% Internal stuff  %%%
sort(List, Axis) ->
    lists:sort(fun({A,_}, {B,_}) ->
		       element(Axis, A) =< element(Axis,B)
	       end, List).

split(Total, Axis, Records) ->
    Length = Total div 2,
    {RLeft,Right} = split_1(Length, Records, []),
    verify_med(RLeft, Right, Axis, Length, Total).

split_1(0, Right, ReverseLeft) ->
    {ReverseLeft,Right};
split_1(N, [L|Right], Acc) ->
    split_1(N-1, Right, [L|Acc]).

verify_med([{Med,_}|_]=Right, [], Axis, _, Total) ->
    %% All values the same on that axis
    {element(Axis, Med), Total, Right, 0, []};
verify_med([{L,_}|_]=Left0, [{R,_}=Node|Right]=RAll, Axis, LeftLen, Total) ->
    Split = element(Axis, L),
    case element(Axis, R) > Split of
	true -> 
	    {Split, LeftLen, Left0, Total-LeftLen, RAll};
	false ->
	    verify_med([Node|Left0], Right, Axis, LeftLen+1, Total)
    end.

next_axis(1) -> 2;
next_axis(2) -> 3;
next_axis(3) -> 1.

axis(Axis) -> Axis. %% band 3.

closest([Dist1|_]=Node1, [Dist2|_]) when Dist1 < Dist2 -> Node1;
closest(_, Node) -> Node.

