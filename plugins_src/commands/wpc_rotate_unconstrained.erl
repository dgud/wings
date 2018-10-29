%%
%%	wpc_rotate_unconstrained.erl
%%
%%	--	Unconstrained Rotation
%%		(also known as trackball rotation)
%%
%%	Copyright (c) Anna Celarek 2010-2011
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_rotate_unconstrained).
-export([init/0, menu/2, command/2]).
-include_lib("wings/src/wings.hrl").
-import(lists, [foldl/3]).

init() ->
    true.

%% adding the function name into rotate menu
menu({Mode, rotate}, Menu) when Mode =:= vertex; Mode =:= edge; Mode =:= face; Mode =:= body ->
    [Menu|[separator,orbitrot_menu(Mode)]];
menu(_, Menu) -> Menu.

%% function name and help (bottom line)
orbitrot_menu(Mode) ->
    Help = {?__(2,"Rotate freely around selection "), [],	%% lmb help, [] mmb none,
            ?__(3,"Pick rotation center")},
    {?__(1,"Unconstrained"),	% function name visible in menu
    {rotate_unconstrained,orbitrot_options(Mode, Help)}, magnet_possible(Mode)}.

%% magnet (doesn't work yet
magnet_possible(body) -> [];
magnet_possible(_) -> [magnet].

%% lmb, rmb options (have to match with the things after command)
orbitrot_options(body, Help) ->
    fun
      (help, _Ns) -> Help;
      (1,_Ns) -> {body,{rotate, {unconstrained, lmb}}};
      (3,_Ns) -> 	%'ASK' is an atom too
          {body,{rotate, {unconstrained,{rmb,{'ASK',{[center],[],[]}}}}}};
      (_,_)   -> ignore
    end;
orbitrot_options(Mode, Help) ->
    fun
      (help, _Ns) -> Help;
      (1,_Ns) ->
          {Mode,{rotate, {unconstrained,{lmb,{'ASK',{[],[],[magnet]}}}}}};
      (3,_Ns) ->    % [magnet]={magnet,Type,Route,Point}
          {Mode,{rotate, {unconstrained,{rmb,{'ASK',{[center],[],[magnet]}}}}}};
      (_,_)   -> ignore
    end.

%%%% COMMANDS
%% called from menu (see above) or shortkey

% no center, no magnet
command({_,{rotate, {unconstrained,{lmb, {'ASK',{[],_,_}}}}}},St) ->
    no_center_setup(none, St);
% no center, magnet
command({_,{rotate, {unconstrained,{lmb, {'ASK',Ask}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun no_center_setup/2);
% no center
command({_,{rotate, {unconstrained,{lmb, Data}}}},St) ->
    no_center_setup(Data, St);
% body (has no asks at lmb), no center
command({_,{rotate, {unconstrained, lmb}}}, St) ->
    no_center_setup(none, St);
% chose center (and maybe magnet)
command({_,{rotate, {unconstrained,{rmb,{'ASK',Ask}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun center_setup/2);
% Data = {Center, (Magnet)}
command({_,{rotate, {unconstrained,{rmb, Data}}}},St) ->
    center_setup(Data,St);
command(_,_) ->
    next.

%%%% Asks
selection_ask({Asks,_,_}) ->
    selection_ask(Asks);
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([center|T],Ask) ->
    Desc = ?__(1,"Pick center of rotation"),
    selection_ask(T,[{point,Desc}|Ask]);
selection_ask([magnet|T],Ask) ->
    selection_ask(T,[magnet|Ask]).

%%%% Setup
no_center_setup(_, #st{selmode=body}=St) ->
    Center = no_center,
    finish_setup(Center, body, St);
no_center_setup(Mag, St) ->
    Center = no_center,
    finish_setup(Center, Mag, St).

center_setup({Center,_Magnet}, #st{selmode=body}=St) ->
% In case uncon rot + mag is repeated in body mode after being used in v, e, f.
    finish_setup(Center, body, St);
center_setup({X,Y,Z}, #st{selmode=body}=St) ->
    Center = {X,Y,Z},
    finish_setup(Center, body, St);
center_setup({X,Y,Z}, St) ->
    Center = {X,Y,Z},
    finish_setup(Center, none, St);
center_setup({Center, Mag}, St) ->
    finish_setup(Center, Mag, St).

%%%% FOLDING AND DRAGGING
finish_setup(Center, {}, St) -> finish_setup(Center, none, St);
finish_setup(Center0, body, St) ->
    %% body (uses whole matrix instead of single vert coordinates)
    wings_drag:matrix(
      fun(We) ->
              Center = find_center(Center0, body, We),
              rotate(body, Center)
      end, [angle,angle], [screen_relative], St);
finish_setup(Center0, Mag, #st{selmode=Mode}=St) ->
    Type = magnet_type(Mag),
    State = {none,Type,{reciprocal,false}},
    CamX = view_cam(x),
    CamY = view_cam(y),
    Units = [angle, angle| magnet_unit(Mag)],
    wings_drag:fold(
      fun(Vs0, We) ->
              Vs = conversion(Mode, Vs0, We),
              Center = find_center(Center0, Vs, We),
              finish_setup_1(Vs, We, CamX, CamY, Center, Mag, State)
      end, Units, flags(State), St).

%%%% vertex list
finish_setup_1(Vs, We,CamX, CamY, Center, none, State) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,rotate_fun(CamX, CamY, Center, VsPos, none, State)};
finish_setup_1(Vs, We, CamX, CamY, Center, Mag0, State) ->
    {VsInf,Magnet,Affected} = wings_magnet:setup(Mag0, Vs, We),
    {Affected,rotate_fun(CamX, CamY, Center, VsInf, Magnet, State)}.

%%%% Catch view rotations, magnet option changes etc
rotate_fun(CamX,CamY,Center,VsPos,none,State) ->		% no mag
    fun
        (view_changed, NewWe) ->
            NewCamX = view_cam(x),
            NewCamY = view_cam(y),
            NewVsPos = wings_util:update_vpos(VsPos, NewWe),
            rotate_fun(NewCamX,NewCamY,Center,NewVsPos,none,State);

        ([Dx,Dy|_], A) ->
            foldl(fun({V,Vpos}, VsAcc) ->
                [{V,rotate(Vpos,CamX,CamY,Center,State,Dx,Dy)}|VsAcc]
                end,
                A, VsPos)
        end;

rotate_fun(CamX,CamY,Center,VsInf0,{_,R}=Magnet0,State0) ->	% mag
    fun
        (new_falloff, Falloff) ->
            VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
            rotate_fun(CamX,CamY,Center,VsInf,Magnet0,State0);
        (new_mode_data, {State, Falloff}) ->
            {_,MagType,_} = State,
            Magnet = {MagType,R},
            VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
            rotate_fun(CamX,CamY,Center,VsInf,Magnet,State);
        (view_changed, NewWe) ->
            NewCamX = view_cam(x),
            NewCamY = view_cam(y),
            NewVsPos = wings_util:update_vpos(VsInf0, NewWe),
            rotate_fun(NewCamX,NewCamY,Center,NewVsPos,Magnet0,State0);
        ([Dx,Dy|_], A)  ->
            foldl(fun({V,Vpos,_,Inf}, Acc) ->
                [{V,rotate(Vpos,CamX,CamY,Center,State0,Dx*Inf, Dy*Inf)}|Acc]
                end,
                A, VsInf0)
        end.

%%%% ROTATIONS
%%%% (matrix or vector operations)

rotate(Vpos,_CamX,_CamY,_Center,_, 0.0,0.0) ->
    Vpos;

rotate(Vpos,CamX,CamY,{Cx,Cy,Cz},_,Dx, Dy) ->		% vertex rotation
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    Angle =math:sqrt(Dx*Dx + Dy*Dy),				% mouse radius
    Vx = e3d_vec:mul(CamX, Dy),						% horizontal
    Vy = e3d_vec:mul(CamY, Dx),						% vertical
    Axis = e3d_vec:norm(e3d_vec:sub(Vy, Vx)),		% vector _|_ to mouse direction
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    e3d_mat:mul_point(M, Vpos).

rotate(body, {Cx, Cy, Cz}) ->						% body (matrices)
    fun(Matrix, [Dx, Dy]) ->
        CamX = view_cam(x),
        CamY = view_cam(y),
        M0 = e3d_mat:mul(Matrix, e3d_mat:translate(Cx, Cy, Cz)),
        Angle =math:sqrt(Dx*Dx + Dy*Dy),
        Vx = e3d_vec:mul(CamX, Dy),
        Vy = e3d_vec:mul(CamY, Dx),
        Axis = e3d_vec:norm(e3d_vec:sub(Vy, Vx)),
        M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
        e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz))
    end.

%%%% UTILITIES

%%%% Screen coordinate vector
view_cam(Dir) ->		% Dir: x=right, y=up, z=towards camera
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul_point(M, wings_util:make_vector(Dir)).
    %% the last vector does {right, up, towards camera} in screen coordinates

%%%% Convert selection to vertex list
conversion(Mode,Items,We) when not is_list(Items) ->
    conversion(Mode, gb_sets:to_list(Items), We);
conversion(vertex,Items,_) ->
    Items;
conversion(edge,Items,We) ->
    wings_edge:to_vertices(Items, We);
conversion(face,Items,We) ->
    wings_face:to_vertices(Items,We).

%%%% Selection center
find_center(no_center, body, We) ->
    wings_vertex:center(We);
find_center(no_center, Vs, We) ->
    wings_vertex:center(Vs, We);
find_center(Center, _,_) ->
    Center.

%%%% Flags for the drag function
flags({_,none,{_,_}}) -> [screen_relative];
flags(State) ->
    [{mode,{magnet_modes(),State}}| [screen_relative]].

magnet_type(none) -> none;
magnet_type({_,Type,_,_}) -> Type.

%%%% magnet options (bottom line)
magnet_modes() ->
    fun
        (help, State) ->
            {_,Type,{_,_}}=State,
            wings_magnet:drag_help(Type);
        ({key, K},{none,_Type,_Recip}) when K =:= $1; K =:= $2; K =:= $3; K =:= $4 ->
            {none,wings_magnet:hotkey(K),_Recip};
        (done,{none,Type,_Recip}) ->
            wings_pref:set_value(magnet_type, Type);
        (_,_) -> none
    end.

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].
