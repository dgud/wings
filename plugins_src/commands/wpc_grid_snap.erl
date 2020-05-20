%%
%%  wpc_grid_snap.erl --
%%
%%  Snap vertices to a virtual grid
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_grid_snap).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-import(lists, [foldl/3,reverse/1]).

-define(EPS, 1.0E+9).

init() ->
    true.

%%%
%%% Insert menu heading
%%%

menu({vertex},Menu) ->
    reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([{_,tighten,_,_}=A|Rest], NewMenu, false) ->
    parse(Rest, [menu_heading(),A|NewMenu], true);
parse([Elem|Rest], NewMenu,  Found) ->
    parse(Rest, [Elem|NewMenu], Found);
parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_heading()|NewMenu].

menu_heading() ->
    X = wings_util:cap(wings_s:dir(x)),
    Y = wings_util:cap(wings_s:dir(y)),
    Z = wings_util:cap(wings_s:dir(z)),
    Rad = ?__(6," around radial"),
    HelpL = ?__(1,"Snap each vertex to a virtual grid"),
    {?__(2,"Grid Snap"),{grid_snap,
      [grid_snap_menu(wings_s:dir(all), all, HelpL),
       grid_snap_menu(X, x, HelpL++?__(3," along X")),
       grid_snap_menu(Y, y, HelpL++?__(4," along Y")),
       grid_snap_menu(Z, z, HelpL++?__(5," along Z")),
       grid_snap_menu(wings_s:dir(radial_x), radial_x, HelpL++Rad),
       grid_snap_menu(wings_s:dir(radial_y), radial_y, HelpL++Rad),
       grid_snap_menu(wings_s:dir(radial_z), radial_x, HelpL++Rad)
      ]
    },HelpL}.

grid_snap_menu(String, Axis, HelpL) ->
    HelpM = ?__(1,"Enter origin numerically"),
    HelpR = ?__(2,"Pick origin point from which to base grid"),
    Fun = fun
     % (help,_) -> {HelpL,[],HelpR};
      (1,_) -> {vertex,{grid_snap,Axis}};
      (2,_) -> {vertex,{grid_snap,{Axis,dialog}}};
      (3,_) -> {vertex,{grid_snap,{Axis,{'ASK',[point]}}}};
      (_,_) -> ignore
    end,
    {String,Fun,{HelpL,HelpM,HelpR},[]}.

command({vertex,{grid_snap,{Axis,{'ASK',Ask}}}}, St) ->
    wings:ask({Ask,[]}, St, fun(Point, St0) ->
        grid_snap(Axis, Point, St0) end);
command({vertex,{grid_snap,{Axis,dialog}}}, _St) ->
    Xs = wings_s:dir(x),
    Ys = wings_s:dir(y),
    Zs = wings_s:dir(z),
    wings_dialog:dialog(?__(1,"Origin of Virtual Grid"),
      [{hframe,[{label,Xs},{text,0.0},
                {label,Ys},{text,0.0},
                {label,Zs},{text,0.0}]}],
    fun([X,Y,Z]) ->
        {vertex,{grid_snap,{Axis,{X,Y,Z}}}}
    end);
command({vertex,{grid_snap,{Axis,Point}}}, St) ->
    grid_snap(Axis, Point, St);
command({vertex,{grid_snap,Axis}}, St) ->
    grid_snap(Axis, center, St);
command(_, _) -> next.

grid_snap(Axis, Point0, St) ->
    wings_drag:fold(
      fun(Vs0, #we{vp=Vtab}=We) ->
              Point = case Point0 of
                          center ->
                              BBox = wings_vertex:bounding_box(Vs0, We, none),
                              e3d_vec:mul(e3d_vec:average(BBox), ?EPS);
                          _ ->
                              e3d_vec:mul(Point0, ?EPS)
                      end,
              Vs = gb_sets:to_list(Vs0),
              VsPos = add_vpos(Vs, Vtab),
              {Vs,grid_snap_fun(VsPos, Axis, Point)}
      end, [{distance,{0.0,infinity}}], St).

grid_snap_fun(VsPos, Axis, Point) ->
    fun
        ([D0],A) ->
           D = round(D0*?EPS),
           foldl(fun({V,Vpos}, VsAcc) ->
           [{V,do_grid_snap(Axis, Vpos, Point, D)}|VsAcc]
           end, A, VsPos)
    end.

do_grid_snap(_, Pos, _, 0) ->
    e3d_vec:divide(Pos, ?EPS);
do_grid_snap(all, {X0,Y0,Z0}, {Px,Py,Pz}, D) ->
    X = round_to_closest(X0, Px, D),
    Y = round_to_closest(Y0, Py, D),
    Z = round_to_closest(Z0, Pz, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(x, {X0,Y,Z}, {Px,_,_}, D) ->
    X = round_to_closest(X0, Px, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(y, {X,Y0,Z}, {_,Py,_}, D) ->
    Y = round_to_closest(Y0, Py, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(z, {X,Y,Z0}, {_,_,Pz}, D) ->
    Z = round_to_closest(Z0, Pz, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(radial_x, {X,Y0,Z0}, {_,Py,Pz}, D) ->
    Y = round_to_closest(Y0, Py, D),
    Z = round_to_closest(Z0, Pz, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(radial_y, {X0,Y,Z0}, {Px,_,Pz}, D) ->
    X = round_to_closest(X0, Px, D),
    Z = round_to_closest(Z0, Pz, D),
    e3d_vec:divide({X,Y,Z}, ?EPS);
do_grid_snap(radial_z, {X0,Y0,Z}, {Px,Py,_}, D) ->
    X = round_to_closest(X0, Px, D),
    Y = round_to_closest(Y0, Py, D),
    e3d_vec:divide({X,Y,Z}, ?EPS).

round_to_closest(A0, Origin, D) ->
    A1 = A0 - Origin,
    case abs(A1) < 1.0e-4 of
      true -> A0;
      false ->
        A2 = round(A1),
        Rem = A2 rem D,
        AR = (Rem*2 div D) * D,
        A = (A2 div D) * D + AR,
        if A == 0 -> D*(A2/abs(A2))+Origin; true -> A+Origin end
    end.

add_vpos(Vs, Vtab) ->
    foldl(fun(V, A) ->
          [{V,e3d_vec:mul(array:get(V, Vtab), ?EPS)}|A]
    end, [], Vs).
