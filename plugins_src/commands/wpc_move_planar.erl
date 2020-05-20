%%
%%  wpc_move_planar.erl --
%%
%%    Plugin for moving elements restricted to a single plane.
%%
%%  Copyright (c) 2008-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_move_planar).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

init() ->
    true.

menu({Mode,move},Menu)
        when Mode==vertex; Mode==edge; Mode==face; Mode==body ->
    [Menu,separator,move_planar_menu(Mode)];
menu(_,Menu) ->
    Menu.

%%%% Menus
move_planar_menu(body) ->
    MenuTitle = ?__(1,"Planar"),
    F = fun(help, _Ns) ->
		Str1 = ?__(2,"Restrict movement to standard plane"),
		Str3 = ?__(3,"Pick plane"),
		{Str1,[],Str3};
	   (1, _Ns) -> xyz(body);
	   (2, _Ns) -> ignore;
	   (3, _Ns) -> {body,{move_planar,{'ASK',[plane]}}}
        end,
    {MenuTitle,{move_planar,F},[]};

move_planar_menu(Mode) ->
    MenuTitle = ?__(1,"Planar"),
    F = fun(help, _Ns) ->
		Str1 = ?__(2,"Restrict movement to standard plane"),
		Str3 = ?__(3,"Pick plane"),
		{Str1,[],Str3};
	   (1, _Ns) -> xyz(Mode);
	   (2, _Ns) -> ignore;
	   (3, _Ns) -> {Mode,{move_planar,{'ASK',{[plane],[],[magnet]}}}}
        end,
    {MenuTitle,{move_planar,F},[magnet]}.

xyz(Mode) ->
    [axis_menu(Mode,x),
     axis_menu(Mode,y),
     axis_menu(Mode,z),
     separator,
     axis_menu(Mode,last_axis),
     axis_menu(Mode,default_axis)].

axis_menu(body,Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Help = axis_menu_string(Axis),
    F = {body,{move_planar,Axis}},
    {AxisStr,{Axis,F},Help};
axis_menu(Mode,Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Help = axis_menu_string(Axis),
    F = {Mode,{move_planar,{Axis,{'ASK',{[],[],[magnet]}}}}},
    {AxisStr,{Axis,F},Help,[magnet]}.

axis_menu_string(Axis) ->
    AxisStr = wings_s:dir(Axis),
    Str = ?__(1,"Restrict all movement to plane defined by ~s"),
    wings_util:format(Str,[AxisStr]).

%%%% Commands
command({_,{move_planar,{Axis,{'ASK',{[],_,_}}}}},St) ->
    planar_setup(Axis,St);
command({_,{move_planar,{Axis,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Mag,St0) ->
        planar_setup({Axis,Mag},St0)
    end);
command({_,{move_planar,{'ASK',Ask}}},St) ->
    wings:ask(selection_ask(Ask), St, fun planar_setup/2);
command({_,{move_planar,Axis}},St) ->
    planar_setup(Axis,St);

command(_,_) -> next.

%%%% Asks
selection_ask({Asks,_,_}) ->
    selection_ask(Asks);
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Choose plane to which all movement will be constrained"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([magnet|Rest],Ask) ->
    selection_ask(Rest,[magnet|Ask]).

%%%% Setup
planar_setup(Axis0, #st{selmode=body}=St) ->
    Axis = wings_util:make_vector(Axis0),
    Fun = translate_fun(Axis),
    F = fun(_) -> Fun end,
    wings_drag:matrix(F, [dx,dy], St);
planar_setup({Axis0,Mag}, #st{selmode=Mode}=St) ->
    Axis = wings_util:make_vector(Axis0),
    Flags = wings_magnet:flags(Mag, []),
    wings_drag:fold(fun(Items, We) ->
                            setup(Mode, Axis, Items, We, Mag)
                    end, [dx,dy,falloff], Flags, St);
planar_setup(Axis0, #st{selmode=Mode}=St) ->
    Axis = wings_util:make_vector(Axis0),
    wings_drag:fold(fun(Items, We) ->
                            setup(Mode, Axis, Items, We, none)
                    end, [dx,dy], St).

%%%% Body
translate_fun({X,Y,Z}) when abs(Y)<1.0e-9 andalso abs(Z)<1.0e-9 ->  %% X
    fun(Matrix, [Dx,Dy]) ->
        {Xt,Yt,Zt} = e3d_mat:mul_point(Matrix, {0.0,Dy,-X*Dx}),
        e3d_mat:translate(Xt, Yt, Zt)
    end;
translate_fun({X,Y,Z}) when abs(X)<1.0e-9 andalso abs(Z)<1.0e-9 ->  %% Y
    fun(Matrix, [Dx,Dy]) ->
        {Xt,Yt,Zt} = e3d_mat:mul_point(Matrix, {Dx,0.0,-Y*Dy}),
        e3d_mat:translate(Xt, Yt, Zt)
    end;
translate_fun({X,Y,Z}) when abs(X)<1.0e-9 andalso abs(Y)<1.0e-9 ->  %% Z
    fun(Matrix, [Dx,Dy]) ->
        {Xt,Yt,Zt} = e3d_mat:mul_point(Matrix, {Z*Dx,Dy,0.0}),
        e3d_mat:translate(Xt, Yt, Zt)
    end;
translate_fun(Axis0) ->
    AxisA = e3d_vec:norm(e3d_vec:cross(Axis0,{0.0,1.0,0.0})),
    AxisB = e3d_vec:norm(e3d_vec:cross(Axis0,AxisA)),
    fun(Matrix, [Dx,Dy]) ->
      {X1,Y1,Z1} = e3d_mat:mul_point(Matrix,e3d_vec:mul(e3d_vec:neg(AxisA),Dx)),
      {X2,Y2,Z2} = e3d_mat:mul_point(Matrix,e3d_vec:mul(e3d_vec:neg(AxisB),Dy)),
      e3d_mat:translate(X1+X2, Y1+Y2, Z1+Z2)
    end.

%%%% Vertex, Edge, and Face selections
setup(Mode, Axis, Items, We, Mag) when not is_list(Items) ->
    setup(Mode, Axis, gb_sets:to_list(Items), We, Mag);
setup(vertex, Axis, Items, We, Mag) ->
    setup_2(Axis, Items, We, Mag);
setup(edge, Axis, Items, We, Mag) ->
    Vs = wings_edge:to_vertices(Items, We),
    setup_2(Axis, Vs, We, Mag);
setup(face, Axis, Items, We, Mag ) ->
    Vs = wings_face:to_vertices(Items, We),
    setup_2(Axis, Vs, We, Mag).

setup_2(Axis, Vs, We, none) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,planar_fun(VsPos, Axis)};
setup_2(Axis, Vs, We, Mag) ->
    {VsInf,Magnet,Affected} = wings_magnet:setup(Mag, Vs, We),
    {Affected,magnet_planar_fun(Axis, VsInf, Magnet)}.

magnet_planar_fun(Axis, VsInf0, {_,R}=Magnet0) ->
    fun
        (new_falloff, Falloff) ->
         VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
         magnet_planar_fun(Axis, VsInf, Magnet0);

         %% Magnet Type Switch
         (new_mode_data, {Type,Falloff}) ->
         Magnet = {Type,R},
         VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
         magnet_planar_fun(Axis, VsInf, Magnet);

        ([Dx,Dy|_], A) ->
            lists:foldl(fun({V,Vpos,_,Inf}, Acc) ->
            [{V,planar(Vpos, Axis, Dx*Inf, Dy*Inf)}|Acc]
            end, A, VsInf0)
    end.

planar_fun(VsPos,Axis) ->
    fun
        ([Dx,Dy|_],A) ->
           lists:foldl(fun({V,Vpos}, VsAcc) ->
           [{V,planar(Vpos,Axis,Dx,Dy)}|VsAcc]
           end, A, VsPos)
    end.

%%%% Planar
planar(Vpos,_Axis,0.0,0.0) ->
    Vpos;
planar(Vpos,{X,Y,Z},Dx,Dy) when abs(Y)<1.0e-9 andalso abs(Z)<1.0e-9 ->  %% X
    {Px,Py,Pz} = Vpos,
    {Px, Py+Dy, Pz-(X*Dx)};

planar(Vpos,{X,Y,Z},Dx,Dy) when abs(X)<1.0e-9 andalso abs(Z)<1.0e-9 ->  %% Y
    {Px,Py,Pz} = Vpos,
    {Px-(Y*Dy), Py, Pz-Dx};

planar(Vpos,{X,Y,Z},Dx,Dy) when abs(X)<1.0e-9 andalso abs(Y)<1.0e-9 ->  %% Z
    {Px,Py,Pz} = Vpos,
    {Px+(Dx*Z), Py+Dy, Pz};

planar(Vpos,{X,Y,Z},Dx,Dy) ->
    A = e3d_vec:norm(e3d_vec:cross({X,Y,Z},{0.0,1.0,0.0})),
    B = e3d_vec:norm(e3d_vec:cross({X,Y,Z},A)),
	C = e3d_vec:add(Vpos, e3d_vec:mul(A, -Dx)),
	e3d_vec:add(C, e3d_vec:mul(B, -Dy)).
