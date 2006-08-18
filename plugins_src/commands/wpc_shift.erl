%%
%%  wpc_shift.erl --
%%
%%     Plug-in for shifting vertices
%%
%%  Copyright (c) 2005 Dave Rodgers
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_shift.erl,v 1.2 2005/10/04 20:31:15 giniu Exp $
%%


-module(wpc_shift).

-export([init/0,menu/2,command/2]).
-import(lists, [foldl/3]).
-include("wings.hrl").
-include("e3d.hrl").

-define(HUGE, 1.0E307).
-define(EPSILON, 1.0E-6).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Exported functions
%%

init() ->
    true.

menu({vertex}, Menu) ->
    Menu ++ [{advanced,separator},
	     {advanced, {?__(1,"Shift"), {shift,fun adv_submenu/2}}}];
menu(_,Menu) -> Menu.

command({vertex,{shift,Type}}, St) ->
    shift_cmd(Type, St);
command(_,_) -> next.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Create the menus
%%

submenu_items(1) ->
    {{planar_shift}, {'ASK',{[{axis, ?__(1,"Pick axis")},
	                      {point,  ?__(2,"Pick point")}],[],[]}}};
submenu_items(2) ->
    {{spherical_shift}, {'ASK',{[{point, ?__(3,"Pick center")}],[],[]}}};

submenu_items(3) ->
    {{cylindrical_shift}, {'ASK',{[{axis, ?__(4,"Pick axis")},
                                   {point, ?__(5,"Pick point")}],[],[]}}}.
adv_submenu(help, _) ->
    {?__(1,"Planar Shift"),
     ?__(2,"Spherical Shift"),
     ?__(3,"Cylindrical Shift")};
adv_submenu(Button, NS) ->
    wings_menu:build_command(submenu_items(Button), NS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Respond to commands
%%

shift_cmd({Mode, {'ASK',Ask}}, St) ->
    wings:ask(Ask, St, fun (AskResult, St0) ->
                            shift_ask_callback({Mode, AskResult}, St0)
                       end).

shift_ask_callback({{planar_shift}, {Axis, Center}}, St) ->
    shift_verts(planar_shift, {Axis, Center}, St);

shift_ask_callback({{cylindrical_shift},{Axis, Center}}, St) ->
    shift_verts(cylindrical_shift, {Axis, Center}, St);

shift_ask_callback({{spherical_shift},Center}, St) ->
    shift_verts(spherical_shift, {Center}, St).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Drag and iterate through the vertices
%%

shift_verts(Mode, Data, St) ->

  Fun = case Mode of
          planar_shift -> fun shift_planar/3;
          cylindrical_shift -> fun shift_cylindrical/3;
          spherical_shift -> fun shift_spherical/3
        end,
  Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
                               shift_verts({Fun, Data}, Vs, We, Acc)
                       end, [], St),
  wings_drag:setup(Tvs, [distance], St).


shift_verts(FunData, Vs0, #we{id=Id}=We, Acc) ->
  {ShiftFun,Data} = FunData,
  Vs = gb_sets:to_list(Vs0),
  VsPos = wings_util:add_vpos(Vs, We),
  Fun = fun([Distance], A) ->
                foldl(fun({V,Vpos}, VsAcc) ->
                              [{V,ShiftFun(Vpos,Distance,Data)}|VsAcc]
                      end, A, VsPos)
        end,
  [{Id,{Vs,Fun}}|Acc].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  The Main Functions.
%%
%%   The return value is the new position. {X,Y,Z}
%%

shift_planar(Pos, 0.0, {_Axis, _Center}) -> Pos;
shift_planar(Pos, Dist, {Axis, Center}) ->
  V = e3d_vec:sub(Pos,Center),
  D = e3d_vec:dot(V,Axis),
  if
    D < -?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, -Dist)]);
    D > +?EPSILON -> 
      N = e3d_vec:norm(Axis),
      e3d_vec:add([Pos, e3d_vec:mul(N, +Dist)]);
    true ->
      Pos
  end.

shift_cylindrical(Pos, 0.0, {_Axis, _Center}) -> Pos;
shift_cylindrical(Pos, Dist, {Axis, Center}) ->
  V = e3d_vec:sub(Pos,Center),
  OffDist = e3d_vec:dot(V,Axis),
  OffVec = e3d_vec:sub(V,e3d_vec:mul(Axis, OffDist)),
  N = e3d_vec:norm(OffVec),
  e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]).

shift_spherical(Pos, 0.0, {_Center}) -> Pos;
shift_spherical(Pos, Dist, {Center}) ->
  V = e3d_vec:sub(Pos,Center),
  N = e3d_vec:norm(V),
  e3d_vec:add([Pos, e3d_vec:mul(N, Dist)]).
