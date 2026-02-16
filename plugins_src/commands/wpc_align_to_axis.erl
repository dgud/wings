%%
%%  wpc_align_to_axis.erl -- Rotate an object to an axis using a reference
%%
%%    Plugin to rotate selected object to a default axis of a secondary vector selection
%%
%%  Copyright (c) 2023 Micheus Vieira.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_align_to_axis).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

init() ->
    true.

%%%% Menu
menu({Mode},Menu) when Mode=:=body ->
    parse(Menu, Mode);
menu(_,Menu) ->
    Menu.

parse(Menu, Mode) ->
    lists:reverse(parse(Menu, Mode, [], false)).

parse([], _, NewMenu, true) ->
    NewMenu;
parse([{_, {absolute, _}}=AbsCMD|Rest], Mode, NewMenu, false) ->
    parse(Rest, Mode, [align_to_menu()|[AbsCMD|NewMenu]], true);
parse([Elem|Rest], Mode, NewMenu, Found) ->
    parse(Rest, Mode, [Elem|NewMenu], Found).

align_to_menu() ->
    [{?__(2,"Align to"), {align_to, align_to_axis()}}].

align_to_axis() ->
    RotAxisStrL = ?__(1,"Align the object to ~s axis by using a reference axis"),
    RotAxisStrR = ?__(2,"Align the object to ~s axis by setting the reference direction"),
    [{wings_s:dir(x),align_to_axis_fun(x), {io_lib:format(RotAxisStrL, [wings_s:dir(x)]),[],
        io_lib:format(RotAxisStrR, [wings_s:dir(x)])}},
     {wings_s:dir(y),align_to_axis_fun(y), {io_lib:format(RotAxisStrL, [wings_s:dir(y)]),[],
         io_lib:format(RotAxisStrR, [wings_s:dir(y)])}},
     {wings_s:dir(z),align_to_axis_fun(z), {io_lib:format(RotAxisStrL, [wings_s:dir(z)]),[],
         io_lib:format(RotAxisStrR, [wings_s:dir(z)])}},
     separator,
     {?__(3,"Target"), {target, fun align_to_target_opt/2},[],[]}].

align_to_target_opt(help, _) ->
    {?__(1,"Rotate around the specified center by an angle defined by the reference vector and a default axis"), [],
     ?__(2,"Rotate around the specified center using an angle defined by the reference and target vectors")};
align_to_target_opt(1, _Ns) ->
    align_to_target();
align_to_target_opt(3, _Ns) ->
    {body,{align_to_target,{'ASK',[center,point_A,point_B,point_C,point_D]}}};
align_to_target_opt(_,_) ->
    ignore.

align_to_target() ->
    RotAxisStr = ?__(1,"Align the vector to ~s axis"),
    OrientAxisStr = ?__(2,"Point the vector to ~s axis"),
    [{wings_s:dir(x),align_to_target_fun(x), {io_lib:format(RotAxisStr, [wings_s:dir(x)]), [],
        io_lib:format(OrientAxisStr, [wings_s:dir(x)])}},
     {wings_s:dir(y),align_to_target_fun(y), {io_lib:format(RotAxisStr, [wings_s:dir(y)]), [],
        io_lib:format(OrientAxisStr, [wings_s:dir(y)])}},
     {wings_s:dir(z),align_to_target_fun(z), {io_lib:format(RotAxisStr, [wings_s:dir(z)]), [],
        io_lib:format(OrientAxisStr, [wings_s:dir(z)])}}].

align_to_target_fun(Axis) ->
    fun
        (1,_Ns) -> {body,{{align_to_target,{lmb,Axis}},{'ASK',[center,point_A,point_B]}}};
        (3,_Ns) -> {body,{{align_to_target,{rmb,Axis}},{'ASK',[center,vector]}}};
        (_,_)   -> ignore
    end.

align_to_axis_fun(Axis) ->
    fun
        (1,_Ns) -> {body,{{align_to_axis,{lmb,Axis}},{'ASK',[axis_point]}}};
        (3,_Ns) -> {body,{{align_to_axis,{rmb,Axis}},{'ASK',[center,point_A,point_B]}}};
        (_,_)   -> ignore
    end.

%%%% Commands
command({_,{{align_to_target,{lmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rot_target_x_lmb/2;
                                          y -> fun rot_target_y_lmb/2;
                                          z -> fun rot_target_z_lmb/2
                                      end);
command({_,{{align_to_target,{lmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rot_target_x_lmb(Data,St);
        y -> rot_target_y_lmb(Data,St);
        z -> rot_target_z_lmb(Data,St)
    end;
command({_,{{align_to_target,{rmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rot_target_x_rmb/2;
                                          y -> fun rot_target_y_rmb/2;
                                          z -> fun rot_target_z_rmb/2
                                      end);
command({_,{{align_to_target,{rmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rot_target_x_rmb(Data,St);
        y -> rot_target_y_rmb(Data,St);
        z -> rot_target_z_rmb(Data,St)
    end;
command({_,{align_to_target,{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, fun rot_target_setup/2);
command({_,{align_to_target,Data}}, St) ->
    rot_target_setup(Data,St);
command({_,{{align_to_axis,{lmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rot_axis_x_lmb/2;
                                          y -> fun rot_axis_y_lmb/2;
                                          z -> fun rot_axis_z_lmb/2
                                      end);
command({_,{{align_to_axis,{lmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rot_axis_x_lmb(Data,St);
        y -> rot_axis_y_lmb(Data,St);
        z -> rot_axis_z_lmb(Data,St)
    end;
command({_,{{align_to_axis,{rmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rot_axis_x_rmb/2;
                                          y -> fun rot_axis_y_rmb/2;
                                          z -> fun rot_axis_z_rmb/2
                                      end);
command({_,{{align_to_axis,{rmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rot_axis_x_rmb(Data,St);
        y -> rot_axis_y_rmb(Data,St);
        z -> rot_axis_z_rmb(Data,St)
    end;
command(_Ev,_) ->
    next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([center|Rest],Ask) ->
    Desc = ?__(1,"Pick center of rotation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_A|Rest],Ask) ->
    Desc = ?__(2,"Pick Point A for reference vector"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_B|Rest],Ask) ->
    Desc = ?__(3,"Pick Point B for reference vector"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_C|Rest],Ask) ->
    Desc = ?__(4,"Pick Point A for target vector"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_D|Rest],Ask) ->
    Desc = ?__(5,"Pick Point B for target vector"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([vector|Rest],Ask) ->
    Desc = ?__(6,"Pick the reference vector"),
    selection_ask(Rest,[{axis_point,Desc}|Ask]);
selection_ask([axis_point|Rest],Ask) ->
    Desc = ?__(7,"Pick the reference axis"),
    selection_ask(Rest,[{axis,Desc}|Ask]).

%%%% Setup Align to Target
%%%% LMB
rot_target_x_lmb({Center,A,B},St) ->
    rot_target_lmb({x,Center,A,B},St).
rot_target_y_lmb({Center,A,B},St) ->
    rot_target_lmb({y,Center,A,B},St).
rot_target_z_lmb({Center,A,B},St) ->
    rot_target_lmb({z,Center,A,B},St).
%%%% RMB
rot_target_x_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({x,Normal,Center},St).
rot_target_y_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({y,Normal,Center},St).
rot_target_z_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({z,Normal,Center},St).

rot_target_lmb({Axis0,Center,A,B},St) ->
    Vec0 = e3d_vec:sub(B,A),
    Vec = e3d_vec:norm(project(Axis0,Vec0)),
    Axis = wings_util:make_vector(Axis0),
    RotAxis0 = e3d_vec:norm(e3d_vec:cross(Vec,Axis)),
    Valid = valid_vec(RotAxis0),
    RotAxis =
        if not Valid ->
            if (Axis0=:=x) or (Axis0=:=z) ->
                wings_util:make_vector(y);
            true ->
                wings_util:make_vector(z)
            end;
        true -> RotAxis0
        end,
    Deg = e3d_vec:degrees(Axis,Vec),
    Mr = e3d_mat:rotate(Deg, RotAxis),
    rotate_axis(Center, Mr, St).

rotate_vector_rmb({Axis,Vec,Center}, St) ->
    %% checking if the Source vector is laying on a system axis (X/Y/Z)
    SAxis0 = e3d_vec:largest_dir(Vec),
    OnAxis = not valid_vec(e3d_vec:norm(trunc_axis(SAxis0,Vec))),
    case OnAxis of
        true -> St;
        false ->
            if Axis == y ->
                VecS = e3d_vec:norm(trunc_axis(x,Vec)),
                VecD0 = wings_util:make_vector(Axis),
                VecD =
                    case e3d_vec:plane_side(VecS,{VecD0,0.0}) of
                        1 -> e3d_vec:neg(VecD0);
                        -1 -> VecD0
                    end;
            true ->
                VecS = e3d_vec:norm(trunc_axis(y,Vec)),
                VecD0 = wings_util:make_vector(Axis),
                VecD =
                    case e3d_vec:plane_side(VecS,{VecD0,0.0}) of
                        1 -> VecD0;
                        -1 -> e3d_vec:neg(VecD0)
                    end
            end,
            Deg = e3d_vec:degrees(VecD,VecS),
            RotAxis = e3d_vec:norm(e3d_vec:cross(VecS,VecD)),
            Mr = e3d_mat:rotate(Deg, RotAxis),
            rotate_axis(Center, Mr, St)
    end.

%%%% LMB
rot_target_setup({Center,A,B,C,D},St) ->
    VecD0 = e3d_vec:sub(D,C),
    %% checking if the Destination vector is laying on a system axis (X/Y/Z)
    DAxis0 = e3d_vec:largest_dir(VecD0),
    OnAxis = not valid_vec(e3d_vec:norm(trunc_axis(DAxis0,VecD0))),
    case OnAxis of
        true ->
            case e3d_vec:plane_side(VecD0,{wings_util:make_vector(DAxis0),0.0}) of
                1 -> rot_target_lmb({DAxis0,Center,A,B},St);
                -1 -> rot_target_lmb({DAxis0,Center,B,A},St)
            end;
        false ->
            VecS = e3d_vec:norm(e3d_vec:sub(B,A)),
            VecD = e3d_vec:norm(VecD0),
            %% checking if both vectors are overlapping each other
            case VecS =:= VecD of
                true ->
                    wings_u:message(?__(1,"Vectors cannot point to the same direction"));
                false ->
                    %% Checking if Source vector is laying on a system axis (X/Y/Z)
                    OnY = not valid_vec(e3d_vec:norm(trunc_axis(y,VecS))),
                    if OnY ->
                        %% Find an alternative axis to rotate the object around Y
                        %% before align (rotate) the Source to Destination
                        DAxis = e3d_vec:largest_dir(trunc_axis(y,VecD)),
                        FstVecS = translate_axis(DAxis,VecD),
                        FstVecD = e3d_vec:norm(trunc_axis(y,VecD)),
                        FstDeg = e3d_vec:degrees(FstVecS,FstVecD),
                        MrFst = e3d_mat:rotate(FstDeg, translate_axis(y,VecS)),
                        %% rotate it directly to Destination vector
                        SndDeg = e3d_vec:degrees(VecS,VecD),
                        SndAxis = e3d_vec:norm(e3d_vec:cross(VecS,VecD));
                    true ->
                        %% Project Source and Destination vectors to XZ plane and compute
                        %% the angle between them
                        FstVecS = e3d_vec:norm(trunc_axis(y,VecS)),
                        FstVecD = e3d_vec:norm(trunc_axis(y,VecD)),
                        FstDeg = e3d_vec:degrees(FstVecS,FstVecD),
                        MrFst = e3d_mat:rotate(FstDeg, translate_axis(y,VecS)),
                        VecS0 = e3d_vec:norm(e3d_mat:mul_vector(MrFst,VecS)),
                        SndDeg = e3d_vec:degrees(VecS0,VecD),
                        SndAxis = e3d_vec:norm(e3d_vec:cross(VecS0,VecD))
                    end,
                    MrSnd = e3d_mat:rotate(SndDeg,SndAxis),
                    Mr = e3d_mat:mul(MrSnd,MrFst),
                    rotate_axis(Center,Mr,St)
            end
    end.

%%%% Setup Align to Target
%%%% LMB
rot_axis_x_lmb(A,St) ->
    rot_axis({x,center,A},St).
rot_axis_y_lmb(A,St) ->
    rot_axis({y,center,A},St).
rot_axis_z_lmb(A,St) ->
    rot_axis({z,center,A},St).

%%%% RMB
rot_axis_x_rmb({Center,A,B},St) ->
    rot_axis({x,Center,e3d_vec:sub(B,A)},St).
rot_axis_y_rmb({Center,A,B},St) ->
    rot_axis({y,Center,e3d_vec:sub(B,A)},St).
rot_axis_z_rmb({Center,A,B},St) ->
    rot_axis({z,Center,e3d_vec:sub(B,A)},St).

rot_axis({Axis0,Center,A},St) ->
    Vec = e3d_vec:norm(A),
    Axis = wings_util:make_vector(Axis0),
    RotAxis0 = e3d_vec:norm(e3d_vec:cross(Vec,Axis)),
    Valid = valid_vec(RotAxis0),
    RotAxis =
        if not Valid ->
            if (Axis0=:=x) or (Axis0=:=z) ->
                wings_util:make_vector(y);
                true ->
                    wings_util:make_vector(z)
            end;
            true -> RotAxis0
        end,
    Deg = e3d_vec:degrees(Axis,Vec),
    Mr = e3d_mat:rotate(Deg, RotAxis),
    rotate_axis(Center, Mr, St).

%%%% Rotate functions
rotate_axis(Center0, Mr, St0) ->
    F = fun(_, #we{vp=Vtab0}=We, Acc0) ->
            Center =
                case Center0 of
                    center -> wings_we:centroid(We);
                    _ -> Center0
                end,
            VsPos0 = array:sparse_to_orddict(Vtab0),
            VsPos1 = rotate_axis_fun(Center, Mr, VsPos0),
            Vtab = array:from_orddict(VsPos1),
            {We#we{vp=Vtab},Acc0}
        end,
    {St,_Acc} = wings_sel:mapfold(F,[],St0),
    {save_state,St}.

rotate_axis_fun({Cx,Cy,Cz},Mr,VsPos) ->
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, Mr),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    lists:foldr(fun({V,Vpos}, VsAcc) ->
                    [{V,e3d_mat:mul_point(A2,Vpos)}|VsAcc]
                end, [], VsPos).

%%%% misc
project(y, {X,Y,Z}) ->
    case valid_vec({0.0,Y,Z}) of
        false -> {X,Y,0.0};
        true -> {0.0,Y,Z}
    end;
project(Axis, {X,_Y,Z}=Vec) when Axis=:=x; Axis=:=z ->
    Valid = valid_vec({X,0.0,Z}),
    if Valid -> Vec;  %% that means Vec is on Y axis
        true -> {X,0.0,Z}
    end.

valid_vec(Vec) ->
    abs(trunc(e3d_vec:len(Vec)*100000)) > 0.

trunc_axis(x, {_,Y,Z}) -> {0.0,Y,Z};
trunc_axis(y, {X,_,Z}) -> {X,0.0,Z};
trunc_axis(z, {X,Y,_}) -> {X,Y,0.0}.

translate_axis(x, {+0.0,_Y,_Z}) -> {-1.0,0.0,0.0};
translate_axis(x, {X,_Y,_Z}) -> {-1.0*(X/abs(X)),0.0,0.0};
translate_axis(y, {_X,+0.0,_Z}) -> {0.0,1.0,0.0};
translate_axis(y, {_X,Y,_Z}) -> {0.0,Y/abs(Y),0.0};
translate_axis(z, {_X,_Y,+0.0}) -> {0.0,0.0,1.0};
translate_axis(z, {_X,_Y,Z}) -> {0.0,0.0,Z/abs(Z)}.
