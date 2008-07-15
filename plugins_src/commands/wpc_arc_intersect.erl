%%
%%  wpc_arc_intersect.erl --
%%
%%    Plugin to rotate selected element to intersect with a secondary selection
%%
%%  Copyright (c) 2008 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_arc_intersect).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.

%%%% Menu
menu({Mode},Menu) when Mode =:= vertex; Mode =:= edge; Mode =:= face; Mode =:= body ->
    case wings_pref:get_value(advanced_menus) of
      false -> Menu;
      true  -> lists:reverse(parse(Mode, Menu, [], false))
    end;
menu(_,Menu) ->
    Menu.

parse(_, [], NewMenu, true) ->
    NewMenu;
parse(Mode, [], NewMenu, false) ->
    [arc_intersect_menu(Mode)|NewMenu];
parse(Mode, [A = {_,{flatten,_}}|Rest], NewMenu, false) when Mode =:= vertex; Mode =:= face ->
    parse(Mode, Rest, [arc_intersect_menu(Mode),A|NewMenu], true);
parse(body, [A = {_,{flip,_}}|Rest], NewMenu, false) ->
    parse(body, Rest, [arc_intersect_menu(body),A|NewMenu], true);
parse(edge, [A = {_,{extrude,_}}|Rest], NewMenu, false) ->
    parse(edge, Rest, [arc_intersect_menu(edge),A|NewMenu], true);
parse(Mode, [Elem|Rest], NewMenu, Found) ->
    parse(Mode, Rest, [Elem|NewMenu], Found).

arc_intersect_menu(Mode) ->
    {?__(1,"Arc Intersect"), arc_intersect_options(Mode),
      {?__(2,"Rotate around specified center from Point A to Point B"),[],
       ?__(3,"Rotate around specified center by an angle defined by Vector A and Vector B")},[]}.

arc_intersect_options(Mode) ->
    fun
      (1,_Ns) -> {Mode,{arc_intersect,{lmb,{'ASK',[rotation_axis,center,point_A,point_B]}}}};
      (3,_Ns) -> {Mode,{arc_intersect,{rmb,{'ASK',[rotation_axis,center,plane_A,plane_B]}}}};
      (_,_) -> ignore
    end.

%%%% Commands
command({_,{arc_intersect,{lmb,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun({Axis,Center,A,B},St0) ->
    arc_intersect_setup(Axis,Center,A,B,St0)
    end);
command({_,{arc_intersect,{lmb,{Axis,Center,A,B}}}},St) ->
    arc_intersect_setup(Axis,Center,A,B,St);

command({_,{arc_intersect,{rmb,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun({Axis,Center,A,B},St0) ->
    arc_intersect_plane_setup(Axis,Center,A,B,St0)
    end);
command({_,{arc_intersect,{rmb,{Axis,Center,A,B}}}},St) ->
    arc_intersect_plane_setup(Axis,Center,A,B,St);

command(_,_) ->
    next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([rotation_axis|Rest],Ask) ->
    Desc = ?__(1,"Pick rotation axis"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([center|Rest],Ask) ->
    Desc = ?__(2,"Pick center of rotation"),
    selection_ask(Rest,[{point,Desc}|Ask]);

selection_ask([point_A|Rest],Ask) ->
    Desc = ?__(3,"Pick Point A"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_B|Rest],Ask) ->
    Desc = ?__(4,"Pick Point B"),
    selection_ask(Rest,[{point,Desc}|Ask]);

selection_ask([plane_A|Rest],Ask) ->
    Desc = ?__(5,"Pick Vector A"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([plane_B|Rest],Ask) ->
    Desc = ?__(6,"Pick Vector B"),
    selection_ask(Rest,[{axis,Desc}|Ask]).

%%%% Setup
arc_intersect_setup(Axis,Center,A,B,St0) ->
    St = wings_sel_conv:mode(vertex,St0),
    State = {none,none,{reciprocal,false}},
    VecA = double_cross(Center,A,Axis),
    VecB = double_cross(Center,B,Axis),
    Deg = e3d_vec:degrees(VecA,VecB),
    Tvs = wings_sel:fold(fun(Vs0,#we{id=Id}=We,Acc) ->
            Vs = gb_sets:to_list(Vs0),
            VsPos = wings_util:add_vpos(Vs,We),
            [{Id,{Vs, arc_intersect_fun(Axis,Center,Deg,VsPos,State)}}|Acc]
            end,[],St),
    Units = [percent],
    Flags = [{mode,{arc_intersect_modes(),State}}],
    wings_drag:setup(Tvs, Units, Flags, St0).

arc_intersect_plane_setup(Axis,Center,A,B,St0) ->
    St = wings_sel_conv:mode(vertex,St0),
    State = {none,none,{reciprocal,false}},
    Deg = e3d_vec:degrees(A, B),
    Tvs = wings_sel:fold(fun(Vs0,#we{id=Id}=We,Acc) ->
            Vs = gb_sets:to_list(Vs0),
            VsPos = wings_util:add_vpos(Vs,We),
            [{Id,{Vs, arc_intersect_fun(Axis,Center,Deg,VsPos,State)}}|Acc]
            end,[],St),
    Units = [percent],
    Flags = [{mode,{arc_intersect_modes(),State}}],
    wings_drag:setup(Tvs, Units, Flags, St0).

arc_intersect_modes() ->
    fun
      (help, State) -> arc_intersect_mode_help(State);
      ({key,$1},{none,none,{reciprocal,false}}) -> {none,none,{reciprocal,true}};
      ({key,$1},{none,none,{reciprocal,true}})  -> {none,none,{reciprocal,false}};
      (_,_) -> none
    end.

arc_intersect_mode_help({_,_,{_,false}}) ->
    ?__(1,"[1] Flip Angle");
arc_intersect_mode_help({_,_,{_,true}}) ->
    ?__(2,"[1] Flip Back").

arc_intersect_fun(Axis,Center,Deg,VsPos,State) ->
    fun
      (new_mode_data,{NewState,_}) ->
          arc_intersect_fun(Axis,Center,Deg,VsPos,NewState);
      ([Percent|_], A) ->
          lists:foldl(fun({V,Vpos}, VsAcc) ->
          [{V,arc_intersect(Axis,Center,Deg,Vpos,State,Percent)}|VsAcc]
          end, A, VsPos)
    end.

arc_intersect(_Axis,_Center,_Deg,Vpos,_State,0.0) ->
    Vpos;

arc_intersect(Axis,Center,Deg,Vpos,_State,Percent) when Deg =:= 0.0; Deg =:= 180.0 ->
    Rotate = 180.0 * Percent,
    rotate(Vpos,Axis,Center,Rotate);

arc_intersect(Axis,Center,Deg,Vpos,{_,_,{_,false}},Percent) when Percent < 0.0 ->
    Rotate = Deg * Percent,
    rotate(Vpos,Axis,Center,Rotate);

arc_intersect(Axis,Center,Deg,Vpos,{_,_,{_,false}},Percent) when Percent > 0.0 ->
    Rotate = (180.0 - Deg) * Percent,
    rotate(Vpos,Axis,Center,Rotate);

arc_intersect(Axis,Center,Deg,Vpos,{_,_,{_,true}},Percent) when Percent < 0.0 ->
    Rotate = (180.0 - Deg) * Percent,
    rotate(Vpos,Axis,Center,Rotate);

arc_intersect(Axis,Center,Deg,Vpos,{_,_,{_,true}},Percent) when Percent > 0.0 ->
    Rotate =  Deg * Percent,
    rotate(Vpos,Axis,Center,Rotate).

%%%% Utilities
double_cross(Center,Point,Axis) ->
    Vec0 = e3d_vec:sub(Center,Point),
    Vec1 = e3d_vec:cross(Vec0, Axis),
    e3d_vec:cross(Vec1, Axis).

rotate(Vpos,Axis,{Cx,Cy,Cz},Angle) ->
    %% return new position as {x,y,z}
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).
