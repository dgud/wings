%%
%%  wings_align.erl --
%%
%%     This module contains the Align and Center commands.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_align).
-export([align/2,center/2,copy_bb/1,
	 scale_to_bb/2,scale_to_bb_prop/2,move_to_bb/2,move_bb_to_sel/2,
	 scale_bb_to_sel/2,put_on_ground/1,unitize/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1]).

align(_Axis, #st{sel=[]}=St) ->
    St;
align(Axis, #st{selmode=Mode}=St0) ->
    CF = fun(Items, We) ->
                 Vs = wings_sel:to_vertices(Mode, Items, We),
                 C = e3d_vec:average(wings_vertex:bounding_box(Vs, We)),
                 We#we{temp=C}
         end,
    St = wings_sel:map(CF, St0),
    MF = fun(_, #we{temp=C}) -> [C] end,
    RF = fun erlang:'++'/2,
    Center = e3d_vec:average(wings_sel:dfold(MF, RF, [], St)),
    move_to(Center, Axis, St).

center(_Axis, #st{sel=[]}=St) ->
    St;
center(Axis, St0) ->
    MF = fun(_, We) ->
                 [wings_we:centroid(We)]
         end,
    RF = fun erlang:'++'/2,
    Center = e3d_vec:average(wings_sel:dfold(MF, RF, [], St0)),
    CF = fun(_, We) -> We#we{temp=Center} end,
    St = wings_sel:map(CF, St0),
    move_to(e3d_vec:zero(), Axis, St).

copy_bb(St) ->
    BB = wings_sel:bounding_box(St),
    St#st{bb=BB}.

scale_to_bb(_Dir, #st{bb=none}=St) -> St;
scale_to_bb(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Center = wings_sel:center(St),
	    Matrix = make_scale(Dir, Src, Dest, Center),
	    transform(Matrix, St)
    end.

scale_to_bb_prop(_Dir, #st{bb=none}=St) -> St;
scale_to_bb_prop(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Center = wings_sel:center(St),
	    Matrix = make_prop_scale(Dir, Src, Dest, Center),
	    transform(Matrix, St)
    end.

move_to_bb(_Dir, #st{bb=none}=St) -> St;
move_to_bb(Dir, #st{bb=Dest}=St) ->
    case wings_sel:bounding_box(St) of
	none -> St;
	Src ->
	    Matrix = make_move(Dir, Src, Dest),
	    transform(Matrix, St)
    end.

move_bb_to_sel(_Dir, #st{sel=[]}=St) -> St;
move_bb_to_sel(Dir, #st{bb=[P1,P2]=BB}=St) ->
    CurrentCenter = e3d_vec:average(BB),
    Vec1 = e3d_vec:sub(CurrentCenter,P1),
    Vec2 = e3d_vec:sub(CurrentCenter,P2),
    SelC = filter_coord(Dir, e3d_vec:sub(wings_sel:bbox_center(St),CurrentCenter)),
    SelCenter = e3d_vec:add(CurrentCenter,SelC),
    BB1 = e3d_vec:add(SelCenter, Vec1),
    BB2 = e3d_vec:add(SelCenter, Vec2),
    St#st{bb=[BB1,BB2]};
move_bb_to_sel(_Dir, St) -> St.

scale_bb_to_sel(_Dir, #st{sel=[]}=St) -> St;
scale_bb_to_sel(Dir, #st{bb=[A1,A2]}=St) ->
    [B1,B2] = wings_sel:bounding_box(St),
    {B11x,B11y,B11z} = filter_coord2(Dir,B1),
    {B21x,B21y,B21z} = filter_coord2(Dir,B2),
    {A11x,A11y,A11z} = A1,
    {A21x,A21y,A21z} = A2,
    Point1 = {if B11x =:= none -> A11x; true -> B11x end,
              if B11y =:= none -> A11y; true -> B11y end,
              if B11z =:= none -> A11z; true -> B11z end},
    Point2 = {if B21x =:= none -> A21x; true -> B21x end,
              if B21y =:= none -> A21y; true -> B21y end,
              if B21z =:= none -> A21z; true -> B21z end},
    BBC1 = e3d_vec:average(Point1,Point2),
    BBC2 = e3d_vec:average(A1,A2),
    TransVec = e3d_vec:sub(BBC2,BBC1),
    BB1 = e3d_vec:add(TransVec,Point1),
    BB2 = e3d_vec:add(TransVec,Point2),
    St#st{bb=[BB1,BB2]};
scale_bb_to_sel(_Dir, St) -> St.

transform(Matrix, St) ->
    wings_sel:map(fun(_Items, We0) ->
			  wings_we:transform_vs(Matrix, We0)
		  end, St).

make_move(Dir, Src, Dest0) ->
    SrcMid = e3d_vec:average(Src),
    DestMid = e3d_vec:average(Dest0),
    Tvec = filter_coord(Dir, e3d_vec:sub(DestMid, SrcMid)),
    e3d_mat:translate(Tvec).

make_scale(Dir, Src0, Dest0, Center) ->
    Pre = e3d_mat:translate(Center),
    Post = e3d_mat:translate(e3d_vec:neg(Center)),
    Src1 = e3d_vec:sub(Src0),
    Dest1 = e3d_vec:sub(Dest0),
    Src = filter_coord(Dir, Src1),
    Dest = filter_coord(Dir, Dest1),
    Sc0 = make_scales(Dest, Src),
    [ScX,ScY,ScZ] = map(fun(none) -> 1.0;
			   (Sc) -> Sc end, Sc0),
    e3d_mat:mul(e3d_mat:mul(Pre,e3d_mat:scale(ScX, ScY, ScZ)),Post).

make_prop_scale(Dir, Src0, Dest0, Center) ->
    Pre = e3d_mat:translate(Center),
    Post = e3d_mat:translate(e3d_vec:neg(Center)),
    Src1 = e3d_vec:sub(Src0),
    Dest1 = e3d_vec:sub(Dest0),
    Src = filter_coord(Dir, Src1),
    Dest = filter_coord(Dir, Dest1),
    Sc0 = make_scales(Dest, Src),
    Min = min_scale(Sc0),
    e3d_mat:mul(e3d_mat:mul(Pre,e3d_mat:scale(Min, Min, Min)),Post).

make_scales(Ta, Tb) ->
    make_scales(1, Ta, Tb).

make_scales(I, Ta, Tb) when I > tuple_size(Ta); I > tuple_size(Tb) -> [];
make_scales(I, Ta, Tb) ->
    S = case {element(I, Ta),element(I, Tb)} of
	    {_,0.0} -> none;
	    {A,B} ->
		case catch A / B of		%catch if B is very small
		    {'EXIT',_} -> none;
		    Q -> Q
		end
	end,
    [S|make_scales(I+1, Ta, Tb)].

min_scale([none|Ss]) -> min_scale(Ss);
min_scale([S|Ss]) -> min_scale(Ss, S).

min_scale([none|Ss], Min) ->
    min_scale(Ss, Min);
min_scale([S|Ss], Min) when S < Min ->
    min_scale(Ss, S);
min_scale([_|Ss], Min) ->
    min_scale(Ss, Min);
min_scale([], Min) -> Min.

move_to(Center, Axis, St) ->
    MF = fun(_, #we{vp=Vtab0,temp=MyCenter}=We) ->
                 Offset0 = e3d_vec:sub(Center, MyCenter),
                 case filter_coord(Axis, Offset0) of
                     {0.0,0.0,0.0} ->
                         We#we{temp=[]};
                     Offset ->
                         Vtab = offset(Offset, Vtab0),
                         We#we{vp=Vtab,temp=[]}
                 end
         end,
    wings_sel:map(MF, St).

filter_coord(x, {X,_,_}) -> {X,0.0,0.0};
filter_coord(y, {_,Y,_}) -> {0.0,Y,0.0};
filter_coord(z, {_,_,Z}) -> {0.0,0.0,Z};
filter_coord(radial_x, {_,Y,Z}) -> {0.0,Y,Z};
filter_coord(radial_y, {X,_,Z}) -> {X,0.0,Z};
filter_coord(radial_z, {X,Y,_}) -> {X,Y,0.0};
filter_coord(all, All) -> All.

filter_coord2(x, {X,_,_}) -> {X,none,none};
filter_coord2(y, {_,Y,_}) -> {none,Y,none};
filter_coord2(z, {_,_,Z}) -> {none,none,Z};
filter_coord2(radial_x, {_,Y,Z}) -> {none,Y,Z};
filter_coord2(radial_y, {X,_,Z}) -> {X,none,Z};
filter_coord2(radial_z, {X,Y,_}) -> {X,Y,none};
filter_coord2(all, All) -> All.


offset(Offset, Vtab0) ->
    Vtab = array:sparse_foldl(fun(V, Pos, A) ->
				      [{V,e3d_vec:add(Pos, Offset)}|A]
			      end, [], Vtab0),
    array:from_orddict(reverse(Vtab)).

%% @doc Move selected object(s) vertically until it rests on the ground plane
%% @spec put_on_ground(St::st#) -> St# ?
put_on_ground(St) ->
    wings_sel:map(fun(_, We) -> put_obj_on_ground(We) end, St).

put_obj_on_ground(We) ->
    [{_,Ymin,_},_] = wings_vertex:bounding_box(We),
    Matrix = e3d_mat:translate(0.0, -Ymin, 0.0),
    wings_we:transform_vs(Matrix, We).

%% @doc Scale selected object(s) uniformly until it's bounding box fits
%% inside a sphere of radius 1.0, and move object to origin
%% @spec unitize(St::st#) -> St# ?
unitize(St) ->
    wings_sel:map(fun(_, We) -> unitize_obj(We) end, St).

unitize_obj(We) when ?IS_LIGHT(We) -> We;
unitize_obj(We) ->
    [Min,Max] = wings_vertex:bounding_box(We),
    Size = e3d_vec:sub(Max, Min),
    Center = e3d_vec:average(Min, Max),
    Scale = 2.0 / e3d_vec:len(Size),
    LocMat = e3d_mat:translate(e3d_vec:neg(Center)),
    SizMat = e3d_mat:scale(Scale),
    Matrix = e3d_mat:mul(SizMat, LocMat),
    wings_we:transform_vs(Matrix, We).

