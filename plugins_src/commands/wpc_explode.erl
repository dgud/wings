%%
%%  wpc_explode.erl --
%%
%%    Explode scales distances between whole objects.
%%    Includes standard, user axes, radial and uniform options.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_explode).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-import(lists, [reverse/1]).

init() ->
    true.

%%%
%%% Insert menu heading
%%%

menu({body},Menu) ->
    reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([{_,{flip,_}}=A|Rest], NewMenu, false) ->
    parse(Rest, [menu_heading(),A|NewMenu], true);
parse([Elem|Rest], NewMenu,  Found) ->
    parse(Rest, [Elem|NewMenu], Found);
parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_heading()|NewMenu].

menu_heading() ->
    {?__(1,"Explode"),{explode,explode_menu()}}.

%%%
%%% Build menus
%%%

explode_menu() ->
    fun(help,_) ->
           {?__(1,"Space objects along a standard axis relative to a point"),
            ?__(2,"Pick axis and point"),
            ?__(3,"Pick axis and explode from midpoint of the selection")};
       (1,_) -> standard_axes();
       (2,_) -> {body,{explode,{'ASK',[axis,point]}}};
       (3,_) -> {body,{explode,{'ASK',[axis]}}};
       (_,_) -> ignore
    end.

standard_axes() ->
     [axis_menu(x),
      axis_menu(y),
      axis_menu(z),
      separator,
      axis_menu(uniform),
      axis_menu(radial),
      separator,
      axis_menu(last_axis),
      axis_menu(default_axis)].

axis_menu(radial) ->
    AxisStr = ?__(6,"Radial"),
    Fun = fun
      (help,_) ->
        {?__(3,"Explode objects along radial of standard axis"),
         ?__(4,"Pick radial and point"),
         ?__(5,"Pick radial and explode from midpoint of the selection")};
      (1,_) -> radial_axis();
      (2,_) -> {body,{explode_radial,{'ASK',[axis,point]}}};
      (3,_) -> {body,{explode_radial,{'ASK',[axis]}}};
      (_,_) -> ignore
    end,
    {AxisStr,{explode_radial,Fun}};
axis_menu(uniform) ->
    AxisStr = wings_s:dir(uniform),
    Fun = fun
      (help,_) ->
        {?__(7,"Explode objects uniformly from midpoint of the selection"),[],
         ?__(2,"Pick point")};
      (1,_) -> {body,explode_uniform};
      (3,_) -> {body,{explode_uniform,{'ASK',[point]}}};
      (_,_) -> ignore
    end,
    {AxisStr,{explode_uniform,Fun}};
axis_menu(Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Fun = fun
      (help,_) ->
        Help0 = ?__(1,"Explode objects along ~s axis from midpoint of the selection"),
        Help = wings_util:format(Help0, [AxisStr]),
        {Help, [], ?__(2,"Pick point")};
      (1,_) -> {body,{explode,Axis}};
      (3,_) -> {body,{explode,{Axis,{'ASK',[point]}}}};
      (_,_) -> ignore
    end,
    {AxisStr,{explode,Fun}}.

radial_axis() ->
     [radial_menu(x),
      radial_menu(y),
      radial_menu(z),
      separator,
      radial_menu(last_axis),
      radial_menu(default_axis)].

radial_menu(Axis) ->
    AxisStr = wings_util:cap(wings_s:dir({radial,Axis})),
    Fun = fun
      (help,_) ->
        Help0 = ?__(1,"Explode objects along radial of ~s axis"),
        Help = wings_util:format(Help0, [AxisStr]),
        {Help,[],?__(2,"Pick  point")};
      (1,_) -> {body,{explode_radial,Axis}};
      (3,_) -> {body,{explode_radial,{Axis,{'ASK',[point]}}}};
      (_,_) -> ignore
    end,
    {AxisStr,{explode_radial,Fun}}.

%%%
%%% Commands
%%%

%% Explode Axis commands
command({body,{explode,{Axis,{'ASK',Ask}}}}, St) ->
    wings:ask({Ask,[]}, St, fun (Res,St0) ->
      explode({Axis,Res}, St0)
    end);
command({body,{explode,{'ASK',Ask}}}, St) ->
    wings:ask({Ask,[]}, St, fun
      ({_,_}=Res,St0) -> explode(Res, St0);
      (Axis,St0)->
        Center = wings_sel:center(St0),
        explode({Axis,Center}, St0)
    end);
command({body,{explode,{Axis,Point}}}, St) ->
    explode({Axis,Point}, St);
command({body,{explode,Axis}}, St) ->
    Center = wings_sel:center(St),
    explode({Axis,Center}, St);
%% Explode Radial commands
command({body,{explode_radial,{Axis,{'ASK',Ask}}}}, St) ->
    wings:ask({Ask,[]}, St, fun (Res,St0) ->
      explode_radial({Axis,Res}, St0)
    end);
command({body,{explode_radial,{'ASK',Ask}}}, St) ->
    wings:ask({Ask,[]}, St, fun
      ({_,_}=Res,St0) ->
        explode_radial(Res, St0);
      (Axis,St0)->
        Center = wings_sel:center(St0),
        explode_radial({Axis,Center}, St0)
    end);
command({body,{explode_radial,{Axis,Point}}}, St) ->
    explode_radial({Axis,Point}, St);
command({body,{explode_radial,Axis}}, St) ->
    Center = wings_sel:center(St),
    explode_radial({Axis,Center}, St);
%% Explode Uniform commands
command({body,{explode_uniform,{'ASK',Ask}}}, St) ->
    wings:ask({Ask,[]}, St, fun (Point,St0) ->
      explode_uniform(Point, St0)
    end);
command({body,{explode_uniform,Point}}, St) ->
    explode_uniform(Point, St);
command({body,explode_uniform}, St) ->
    Center = wings_sel:center(St),
    explode_uniform(Center, St);
command(_,_) -> next.

%%%
%%% Process commands
%%%

%% Explode
explode({Axis0,Point}, St) ->
    Axis = wings_util:make_vector(Axis0),
    wings_drag:matrix(
      fun(We) ->
              Center = wings_vertex:center(We),
              explode_fun(Axis, Point, Center)
      end, [percent], St).

%% Explode Uniform
explode_uniform(Point, St) ->
    wings_drag:matrix(
      fun(We) ->
              Center = wings_vertex:center(We),
              explode_uniform_fun(Point, Center)
      end, [percent], St).

%% Explode Radial
explode_radial({Axis0,Point}, St) ->
    Axis = wings_util:make_vector(Axis0),
    wings_drag:matrix(
      fun(We) ->
              Center = wings_vertex:center(We),
              explode_radial_fun(Axis, Point, Center)
      end, [percent], St).


%%%
%%% Explode fun
%%%

explode_fun(Axis, Point, Center) ->
    Dist = dist_along_vector(Center, Point, Axis),
    fun(Matrix, [Percent]) ->
            ScaledAxis = e3d_vec:mul(Axis, Percent * Dist),
            TransVec = e3d_mat:mul_point(Matrix, ScaledAxis),
            e3d_mat:translate(TransVec)
    end.

explode_uniform_fun(Point, Center) ->
    Axis = e3d_vec:norm_sub(Center, Point),
    Dist = e3d_vec:dist(Center, Point),
    fun(Matrix, [Percent]) ->
            ScaledAxis = e3d_vec:mul(Axis, Percent * Dist),
            TransVec = e3d_mat:mul_point(Matrix, ScaledAxis),
            e3d_mat:translate(TransVec)
    end.

explode_radial_fun(Axis, Point, Center) ->
    Vec = e3d_vec:sub(Point, Center),
    Cross = e3d_vec:cross(Vec, Axis),
    Radial = e3d_vec:norm(e3d_vec:cross(Cross, Axis)),
    Dist = dist_along_vector(Center, Point, Radial),
    fun(Matrix, [Percent]) ->
            ScaledRadial = e3d_vec:mul(Radial, Percent * Dist),
            TransVec = e3d_mat:mul_point(Matrix, ScaledRadial),
            e3d_mat:translate(TransVec)
    end.

%%%
%%% Utilities
%%%

dist_along_vector({Xa,Ya,Za},{Xb,Yb,Zb},{Vx,Vy,Vz}) ->
%% Return Distance between PosA and PosB along Normalized Vector
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).
