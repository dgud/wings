%%
%%  wpc_bend.erl --
%%
%%     Plug-in for bending vertices
%%
%%  Copyright (c) 2005-2011 Dave Rodgers
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%


-module(wpc_bend).

-export([init/0,menu/2,command/2]).
-import(lists, [foldl/3]).
-include_lib("wings/src/wings.hrl").

-define(HUGE, 1.0E307).

%% Uncomment the following line to turn on debugging.
%% -define(DEBUG_BEND, 1).

-record(bend_data, {dragMode,          % fixed_length or fixed_radius
                    rodCenter,
                    rodNormal,         % normal along the rod
                    rodLength,         % length from center to top
                    bendCenter,        % used with fixed_radius
                    bendNormal,        % normal from rod to bendCenter
                    pivotNormal,       % rodNormal X bendNormal (rotation axis)
                    posHeightClamp,    % straight lines past this height
                    negHeightClamp}).  % straight lines past this height


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Exported functions
%%

init() ->
    true.

menu({vertex}, Menu) ->
    Menu ++ [separator,
	     {?__(1,"Bend"),{bend,fun adv_submenu_noclamp/2}},
	     {?__(2,"Bend Clamped"),{bend,fun adv_submenu_clamped/2}}];
menu(_,Menu) -> Menu.

command({vertex,{bend,Type}}, St) ->
    bend_cmd(Type, St);
command(_,_) -> next.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Create the menus
%%

submenu_items(1, Clamped) ->
    {{plastic_bend,Clamped},
     {'ASK',{[{point, ?__(1,"Pick rod center")},
	      {point, ?__(2,"Pick rod top")},
	      {axis,  ?__(3,"Pick bend normal")}]
              ++ submenu_clamp(Clamped),[],[]}}};
submenu_items(2, Clamped) ->
    {{pivot_bend,Clamped},
     {'ASK',{[{point, ?__(4,"Pick rod center")},
	      {point, ?__(5,"Pick rod top")},
	      {axis,  ?__(6,"Pick pivot axis")},
	      {point, ?__(7,"Pick pivot location")}]
              ++ submenu_clamp(Clamped),[],[]}}};
submenu_items(3, Clamped) ->
    {{toprad_bend,Clamped},
     {'ASK',{[{point, ?__(8,"Pick rod center")},
	      {point, ?__(9,"Pick rod top")},
	      {axis,  ?__(10,"Pick bend normal")}]
              ++ submenu_clamp(Clamped),[],[]}}}.

submenu_clamp(Clamped) ->
    case Clamped of
      clamped ->
        [{point, ?__(1,"Pick Top Clamp Point")},
         {point, ?__(2,"Pick Bottom Clamp Point")}];
      noclamp ->
        []
    end.


adv_submenu_noclamp(help, _) ->
    {?__(1,"Plastic Bend"),
     ?__(2,"Pivot Bend"),
     ?__(3,"TopRad Bend")};
adv_submenu_noclamp(Button, NS) ->
    wings_menu:build_command(submenu_items(Button, noclamp), NS).

adv_submenu_clamped(help, _) ->
    {?__(1,"Clamped Plastic Bend"),
     ?__(2,"Clamped Pivot Bend"),
     ?__(3,"Clamped TopRad Bend")};
adv_submenu_clamped(Button, NS) ->
    wings_menu:build_command(submenu_items(Button, clamped), NS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Respond to commands
%%

bend_cmd({Mode, {'ASK',Ask}}, St) ->
    wings:ask(Ask, St, fun (AskResult, St0) ->
                            bend_ask_callback({Mode, AskResult}, St0)
                       end);
%%% For repeat drag cmds
bend_cmd({Mode, Data}, St) ->
    bend_ask_callback({Mode, Data}, St).
%%
%% Unclamped
%%

bend_ask_callback({{plastic_bend,noclamp},
                   {RodCenter, RodTop, BendNormal}}, St) ->
    BD = bend_setup(fixed_length, RodCenter, RodTop, BendNormal),
    bend_verts(BD, St);

bend_ask_callback({{pivot_bend,noclamp},
                   {RodCenter, RodTop, PivotNormal, PivotPoint}}, St) ->
    BD = bend_setup(fixed_radius, RodCenter, RodTop,
                                  PivotNormal, PivotPoint),
    bend_verts(BD, St);

bend_ask_callback({{toprad_bend,noclamp},
                   {RodCenter, RodTop, BendNormal}}, St) ->
    BD = bend_setup(fixed_radius, RodCenter, RodTop, BendNormal),
    bend_verts(BD, St);


%%
%% Clamped
%%

bend_ask_callback({{plastic_bend,clamped},
                   {RodCenter, RodTop, BendNormal,
                    PosClamp, NegClamp}}, St) ->
    BD1 = bend_setup(fixed_length, RodCenter, RodTop, BendNormal),
    BD2 = bend_setup_clamps(BD1, PosClamp, NegClamp),
    bend_verts(BD2, St);

bend_ask_callback({{pivot_bend,clamped},
                   {RodCenter, RodTop, PivotNormal, PivotPoint,
                    PosClamp, NegClamp}}, St) ->
    BD1 = bend_setup(fixed_radius, RodCenter, RodTop,
                                   PivotNormal, PivotPoint),
    BD2 = bend_setup_clamps(BD1, PosClamp, NegClamp),
    bend_verts(BD2, St);

bend_ask_callback({{toprad_bend,clamped},
                   {RodCenter, RodTop, BendNormal,
                    PosClamp, NegClamp}}, St) ->
    BD1 = bend_setup(fixed_radius, RodCenter, RodTop, BendNormal),
    BD2 = bend_setup_clamps(BD1, PosClamp, NegClamp),
    bend_verts(BD2, St).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Setup the #bend_data() record
%%

bend_setup(DragMode, RodCenter, RodTop, BendNormal) ->

  Rod = e3d_vec:sub(RodTop, RodCenter),

  BN = e3d_vec:norm(BendNormal),
  RNoff = e3d_vec:mul(BN,e3d_vec:dot(Rod,BN)),  %% off-axis component
  RN = e3d_vec:norm(e3d_vec:sub(Rod,RNoff)),
  PN = e3d_vec:norm(e3d_vec:cross(RN,BN)),
  RL = e3d_vec:dot(Rod, RN),
  BC = e3d_vec:add(RodCenter, e3d_vec:mul(BendNormal,RL)), %% toprad mode
  
  #bend_data{dragMode = DragMode,
             rodCenter = RodCenter,
             rodNormal = RN,
             rodLength = RL,
             bendCenter = BC,
             bendNormal = BN,
             pivotNormal = PN,
             posHeightClamp = +?HUGE,
             negHeightClamp = -?HUGE}.


bend_setup(DragMode, RodCenter, RodTop, PivotNormal, PivotPoint) ->

  Rod = e3d_vec:sub(RodTop, RodCenter),
  VC = e3d_vec:sub(RodCenter,PivotPoint),

  PN = e3d_vec:norm(PivotNormal),
  BC = PivotPoint,
  BNoff = e3d_vec:mul(PN,e3d_vec:dot(VC,PN)),  %% off-axis component
  BN = e3d_vec:norm(e3d_vec:sub(VC,BNoff)),
  RN = e3d_vec:norm(e3d_vec:cross(PN,BN)),
  RL = e3d_vec:dot(Rod, RN),
  
  #bend_data{dragMode = DragMode,
             rodCenter = RodCenter,
             rodNormal = RN,
             rodLength = RL,
             bendCenter = BC,
             bendNormal = BN,
             pivotNormal = PN,
             posHeightClamp = +?HUGE,
             negHeightClamp = -?HUGE}.


bend_setup_clamps(#bend_data{rodCenter=RC, rodNormal=RN}=BD,
                  PosClamp, NegClamp) ->

  PV = e3d_vec:sub(PosClamp, RC),
  NV = e3d_vec:sub(NegClamp, RC),

  PC = e3d_vec:dot(RN, PV),
  NC = e3d_vec:dot(RN, NV),

  PC2 = if
          PC == NC -> +abs(PC);
          PC < NC  -> NC;
          true     -> PC
        end,

  NC2 = if
          NC == PC -> -abs(NC);
          NC > PC  -> PC;
          true     -> NC
        end,

  #bend_data{dragMode = BD#bend_data.dragMode,
             rodCenter = RC,
             rodNormal = RN,
             rodLength = BD#bend_data.rodLength,
             bendCenter = BD#bend_data.bendCenter,
             bendNormal = BD#bend_data.bendNormal,
             pivotNormal = BD#bend_data.pivotNormal,
             posHeightClamp = PC2,
             negHeightClamp = NC2}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Drag and iterate through the vertices
%%

bend_verts(BendData, St) ->
    case BendData#bend_data.rodLength of
        0.0 ->
            wpa:error_msg(?__(1,"Configuration does not result in bending"));
        _ ->
            %% FIXME
            %%   Run a test call. If you don't do this before
            %%   iterating, and there's an error, it locks up
            %%   the mouse under linux.
            bend_vertex({1.0, 1.0, 1.0}, 45.0, BendData),

            wings_drag:fold(fun(Vs, We) ->
                                    bend_verts(BendData, Vs, We)
                            end, [angle], St)
    end.

bend_verts(BendData, Vs0, We) ->
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Angle], A) ->
                  foldl(fun({V,Vpos}, VsAcc) ->
                                [{V,bend_vertex(Vpos,Angle,BendData)}|VsAcc]
                        end, A, VsPos)
          end,
    {Vs,Fun}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  The Main Function.
%%
%%   The return value is the new position. {X,Y,Z}
%%

bend_vertex(Pos, _, #bend_data{rodLength = 0.0}) ->
  Pos;
bend_vertex(Pos, 0.0, #bend_data{dragMode = fixed_length}) ->
  Pos;
bend_vertex(Pos, Angle, #bend_data{dragMode = DragMode,
                                   rodCenter = RC,
                                   rodNormal = RN,
                                   rodLength = RL,
                                   bendCenter = BC,
                                   bendNormal = BN,
                                   pivotNormal = PN,
                                   posHeightClamp = PC,
                                   negHeightClamp = NC}=BD) ->

  maybe_print_bend_data(Angle, BD),

  Radians = Angle * (math:pi()/180.0),

  Center = case DragMode of
             fixed_length -> e3d_vec:add(RC,e3d_vec:mul(BN, RL/Radians));
             fixed_radius -> BC
           end,

  V = e3d_vec:sub(Pos,Center),

  %%% Map to bending axes
  Mr = e3d_vec:dot(V, RN),
  Mb = e3d_vec:dot(V, BN),
  Mp = e3d_vec:dot(V, PN),

  if
    Mr > PC ->
      Rem = Mr - PC,
      MyAngle = Radians * (PC/RL),
      CosA = math:cos(MyAngle),
      SinA = math:sin(MyAngle),
      e3d_vec:add([Center,
                   e3d_vec:mul(PN, Mp),
                   e3d_vec:mul(BN, CosA * +Mb),
                   e3d_vec:mul(RN, SinA * -Mb),
                   e3d_vec:mul(BN, SinA * +Rem),
                   e3d_vec:mul(RN, CosA * +Rem)]);
    Mr < NC ->
      Rem = NC - Mr,
      MyAngle = Radians * (NC/RL),
      CosA = math:cos(MyAngle),
      SinA = math:sin(MyAngle),
      e3d_vec:add([Center,
                   e3d_vec:mul(PN, Mp),
                   e3d_vec:mul(BN, CosA * +Mb),
                   e3d_vec:mul(RN, SinA * -Mb),
                   e3d_vec:mul(BN, SinA * -Rem),
                   e3d_vec:mul(RN, CosA * -Rem)]);
    true ->
      MyAngle = Radians * (Mr/RL),
      CosA = math:cos(MyAngle),
      SinA = math:sin(MyAngle),
      e3d_vec:add([Center,
                   e3d_vec:mul(PN, Mp),
                   e3d_vec:mul(BN, CosA * +Mb),
                   e3d_vec:mul(RN, SinA * -Mb)]) % BN is backwards
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Utilities
%%

-ifndef(DEBUG_BEND).
maybe_print_bend_data(_, _) -> ok.
-else.
maybe_print_bend_data(Angle, BD) ->
    io:format("Angle = ~p: ", [Angle]),
    print_bend_data(BD).

print_bend_data(#bend_data{dragMode = DragMode,
                           rodCenter = RC,
                           rodNormal = RN,
                           rodLength = RL,
                           bendCenter = BC,
                           bendNormal = BN,
                           pivotNormal = PN,
                           posHeightClamp = PC,
                           negHeightClamp = NC}) ->
  case DragMode of
    fixed_length -> io:format("fixed_length\n");
    fixed_radius -> io:format("fixed_radius\n");
    _ -> io:format("bad drag_mode\n")
  end,
  io:format("  RC [~p, ~p, ~p] BC [~p, ~p, ~p]\n" ++
            "  RN [~p, ~p, ~p]\n" ++
            "  BN [~p, ~p, ~p]\n" ++
            "  PN [~p, ~p, ~p]\n", vectorsToArray([RC,BC,RN,BN,PN])),
  io:format("  rodLength = ~p\n", [RL]),
  io:format("  posHeightClamp = ~p\n", [PC]),
  io:format("  negHeightClamp = ~p\n", [NC]).

vectorsToArray(Vectors) ->
  vectorsToArray(Vectors, []).
vectorsToArray([{X,Y,Z}|T], Acc) ->
  vectorsToArray(T, Acc ++ [X,Y,Z]);
vectorsToArray([], Acc) ->
  Acc.
-endif.
