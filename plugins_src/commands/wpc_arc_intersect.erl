%%
%%  wpc_arc_intersect.erl -- Rotate to Target
%%
%%    Plugin to rotate selected element to intersect with a secondary selection
%%
%%  Copyright (c) 2008-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_arc_intersect).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

init() ->
    true.

%%%% Menu
menu({Mode,rotate},Menu) when Mode =:= vertex; Mode =:= edge; Mode =:= face; Mode =:= body ->
    [Menu|[separator,arc_intersect_menu(Mode)]];
menu(_,Menu) ->
    Menu.

arc_intersect_menu(Mode) ->
    Help = {?__(2,"Rotate around specified center from Point A to Point B"),
            ?__(3,"Rotate Point to Plane"),
            ?__(4,"Rotate around specified center by an angle defined by Vector A and Vector B")},
    {?__(1,"Rotate to Target"),
      {arc_intersect, arc_intersect_options(Mode, Help)},magnet_possible(Mode)}.

magnet_possible(body) -> [];
magnet_possible(_) -> [magnet].

arc_intersect_options(body, Help) ->
    fun
      (help,_) -> Help;
      (1,_Ns) -> {body,{arc_intersect,{lmb,{'ASK',[rotation_axis,center,point_A,point_B]}}}};
      (2,_Ns) -> {body,{arc_intersect,{mmb,{'ASK',[rotation_axis,center,point,plane]}}}};
      (3,_Ns) -> {body,{arc_intersect,{rmb,{'ASK',[rotation_axis,center,plane_A,plane_B]}}}};
      (_,_)   -> ignore
    end;
arc_intersect_options(Mode, Help) ->
    fun
      (help,_) -> Help;
      (1,_Ns) -> {Mode,{arc_intersect,{lmb,{'ASK',{[rotation_axis,center,point_A,point_B],[],[magnet]}}}}};
      (2,_Ns) -> {Mode,{arc_intersect,{mmb,{'ASK',{[rotation_axis,center,point,plane],[],[magnet]}}}}};
      (3,_Ns) -> {Mode,{arc_intersect,{rmb,{'ASK',{[rotation_axis,center,plane_A,plane_B],[],[magnet]}}}}};
      (_,_)   -> ignore
    end.

%%%% Commands
command({_,{arc_intersect,{lmb,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun arc_intersect_setup/2);
command({_,{arc_intersect,{lmb,Data}}},St) ->
    arc_intersect_setup(Data,St);

command({_,{arc_intersect,{mmb,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun arc_intersect_point_to_plane_setup/2);
command({_,{arc_intersect,{mmb,Data}}},St) ->
    arc_intersect_point_to_plane_setup(Data,St);

command({_,{arc_intersect,{rmb,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun arc_intersect_plane_setup/2);
command({_,{arc_intersect,{rmb,Data}}},St) ->
    arc_intersect_plane_setup(Data,St);

command(_,_) ->
    next.

%%%% Asks
selection_ask({Asks,_,_}) ->
    selection_ask(Asks);
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
    selection_ask(Rest,[{axis,Desc}|Ask]);

selection_ask([point|Rest],Ask) ->
    Desc = ?__(7,"Pick Point"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([plane|Rest],Ask) ->
    Desc = ?__(8,"Pick Plane"),
    selection_ask(Rest,[{axis,Desc}|Ask]);

selection_ask([magnet|Rest],Ask) ->
    selection_ask(Rest,[magnet|Ask]).

%%%% Setup
%%%% LMB
arc_intersect_setup({Axis,Center,A,B},St) ->
    arc_intersect_setup({Axis,Center,A,B,none},St);

arc_intersect_setup({Axis,Center,A,B,Mag},#st{selmode=body}=St) when Mag =/= none ->
    arc_intersect_setup({Axis,Center,A,B,none},St);

arc_intersect_setup({Axis,Center,A,B,Mag},St) ->
    VecA = double_cross(Center,A,Axis),
    VecB = double_cross(Center,B,Axis),
    Deg = e3d_vec:degrees(VecA,VecB),
    finish_setup(Axis, Center, Deg, Mag, St).

%%%% MMB
arc_intersect_point_to_plane_setup({Axis,Center,A,B},St) ->
    arc_intersect_point_to_plane_setup({Axis,Center,A,B,none},St);

arc_intersect_point_to_plane_setup({Axis,Center,A,B,Mag},#st{selmode=body}=St) when Mag =/= none ->
    arc_intersect_point_to_plane_setup({Axis,Center,A,B,none},St);

arc_intersect_point_to_plane_setup({Axis,Center,A,B,Mag},St) ->
    VecA = double_cross(Center,A,Axis),
    VecB = e3d_vec:cross(Axis,B),
    Deg = e3d_vec:degrees(VecA,VecB),
    finish_setup(Axis, Center, Deg, Mag, St).

%%%% RMB
arc_intersect_plane_setup({Axis,Center,A,B},St) ->
    arc_intersect_plane_setup({Axis,Center,A,B,none},St);

arc_intersect_plane_setup({Axis,Center,A,B,Mag},#st{selmode=body}=St) when Mag =/= none ->
    arc_intersect_plane_setup({Axis,Center,A,B,none},St);

arc_intersect_plane_setup({Axis,Center,A,B,Mag},St) ->
    Deg = e3d_vec:degrees(A, B),
    finish_setup(Axis, Center, Deg, Mag, St).

finish_setup(Axis, Center, Deg, Mag, #st{selmode=Selmode}=St0) ->
    St = case Selmode of
        vertex -> St0;
        _Other -> wings_sel_conv:mode(vertex,St0)
    end,
    MagType = magnet_type(Mag),
    State = {none,MagType,{reciprocal,false}},
    Units = [percent|magnet_unit(Mag)],
    Flags = [{mode,{arc_intersect_modes(Mag),State}}],
    wings_drag:fold(
      fun(Vs0, We) ->
              Vs = gb_sets:to_list(Vs0),
              finish_setup_1(Vs, We, Axis, Center, Deg, Mag, State)
      end, Units, Flags, St).

finish_setup_1(Vs, We, Axis, Center, Deg, none, State) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,arc_intersect_fun(Axis, Center, Deg, VsPos, none, State)};
finish_setup_1(Vs, We, Axis, Center, Deg, Mag, State) ->
    {VsInf,Magnet,Affected} = wings_magnet:setup(Mag, Vs, We),
    {Affected,arc_intersect_fun(Axis, Center, Deg, VsInf, Magnet, State)}.

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

magnet_type(none) -> none;
magnet_type({_,Type,_,_}) -> Type.

arc_intersect_modes(none) ->
    fun
      (help, State) -> arc_intersect_mode_help(State);
      ({key,$1},{none,none,{reciprocal,false}}) -> {none,none,{reciprocal,true}};
      ({key,$1},{none,none,{reciprocal,true}})  -> {none,none,{reciprocal,false}};
      (_,_) -> none
    end;

arc_intersect_modes(_) ->
    fun
      (help, State) -> arc_intersect_mode_help(State);
      ({key,$5},{none,_Type,{reciprocal,false}}) -> {none,_Type,{reciprocal,true}};
      ({key,$5},{none,_Type,{reciprocal,true}})  -> {none,_Type,{reciprocal,false}};
      ({key, K},{none,_Type,_Recip}) when K =:= $1; K =:= $2; K =:= $3; K =:= $4 ->
          {none,wings_magnet:hotkey(K),_Recip};
      (done,{none,Type,_Recip}) -> wings_pref:set_value(magnet_type, Type);
      (_,_) -> none
    end.

arc_intersect_mode_help({_,none,{_,Flip}}) ->
    ?__(1,"[1] ") ++ flip_help_1(Flip);
arc_intersect_mode_help({_,Type,{_,Flip}}) ->
    wings_magnet:drag_help(Type)++flip_help(Flip).
flip_help(Flip) ->
    ?__(1,"  [5] ") ++ flip_help_1(Flip).
flip_help_1(true) ->
    ?__(1,"Flip Angle");
flip_help_1(false) ->
    ?__(2,"Flip Back").

arc_intersect_fun(Axis,Center,Deg,VsPos,none,State) ->
    fun
      (new_mode_data,{NewState,_}) ->
          arc_intersect_fun(Axis,Center,Deg,VsPos,none,NewState);
      ([Percent|_], A) ->
          lists:foldl(fun({V,Vpos}, VsAcc) ->
          [{V,arc_intersect(Axis,Center,Deg,Vpos,State,Percent)}|VsAcc]
          end, A, VsPos)
    end;

arc_intersect_fun(Axis,Center,Deg,VsInf0,{_,R}=Magnet0,State0) ->
    fun
        (new_falloff, Falloff) ->
         VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
         arc_intersect_fun(Axis,Center,Deg,VsInf,Magnet0,State0);

         %% Magnet Type Switch
         (new_mode_data, {State, Falloff}) ->
         {_,MagType,_} = State,
         Magnet = {MagType,R},
         VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
         arc_intersect_fun(Axis,Center,Deg,VsInf,Magnet,State);

        ([Percent|_], A) ->
            lists:foldl(fun({V,Vpos,_,Inf}, Acc) ->
            [{V,arc_intersect(Axis,Center,Deg,Vpos,State0,Percent*Inf)}|Acc]
            end, A, VsInf0)
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
