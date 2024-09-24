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
menu({Mode,absolute},Menu) when Mode =:= body ->
    [Menu|[separator,align_to_axis_menu()]];
menu(_,Menu) ->
    Menu.

align_to_axis_menu() ->
    {?__(2,"Align to Target"), {rotate_to_axis, fun align_to_axis_options/2},[],[]}.

align_to_axis_options(help, _) ->
    {?__(1,"Rotate around specified center by an angle defined by Vector A and a default axis"), [],
     ?__(2,"Rotate around specified center by an angle defined by Vector A and Vector B")};
align_to_axis_options(1, _Ns) ->
    rotate_def_axis();
align_to_axis_options(3, _Ns) ->
    {body,{rotate_to_axis,{'ASK',[center,point_A,point_B,point_C,point_D]}}};
align_to_axis_options(_,_) ->
    ignore.

rotate_def_axis() ->
    RotAxisStr = ?__(1,"Align the vector to ~s axis"),
    OrientAxisStr = ?__(2,"Point the vector to ~s axis"),
    [{wings_s:dir(x),rotate_def_fun(x), {io_lib:format(RotAxisStr, [wings_s:dir(x)]), [],
                                         io_lib:format(OrientAxisStr, [wings_s:dir(x)])}},
     {wings_s:dir(y),rotate_def_fun(y), {io_lib:format(RotAxisStr, [wings_s:dir(y)]), [],
                                         io_lib:format(OrientAxisStr, [wings_s:dir(y)])}},
     {wings_s:dir(z),rotate_def_fun(z), {io_lib:format(RotAxisStr, [wings_s:dir(z)]), [],
                                         io_lib:format(OrientAxisStr, [wings_s:dir(z)])}}].

rotate_def_fun(Axis) ->
    fun
        (1,_Ns) -> {body,{{rotate_to_axis,{lmb,Axis}},{'ASK',[center,point_A,point_B]}}};
%%        (2,_Ns) -> {body,{{rotate_to_axis,Axis},{'ASK',[center,point_A,point_B]}}};
        (3,_Ns) -> {body,{{rotate_to_axis,{rmb,Axis}},{'ASK',[center,vector]}}};
        (_,_)   -> ignore
    end.

%%%% Commands
command({_,{{rotate_to_axis,{lmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rotate_axis_x_lmb/2;
                                          y -> fun rotate_axis_y_lmb/2;
                                          z -> fun rotate_axis_z_lmb/2
                                      end);
command({_,{{rotate_to_axis,{lmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rotate_axis_x_lmb(Data,St);
        y -> rotate_axis_y_lmb(Data,St);
        z -> rotate_axis_z_lmb(Data,St)
    end;
command({_,{{rotate_to_axis,{rmb,Axis}},{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, case Axis of
                                          x -> fun rotate_axis_x_rmb/2;
                                          y -> fun rotate_axis_y_rmb/2;
                                          z -> fun rotate_axis_z_rmb/2
                                      end);
command({_,{{rotate_to_axis,{rmb,Axis}},Data}}, St) ->
    case Axis of
        x -> rotate_axis_x_rmb(Data,St);
        y -> rotate_axis_y_rmb(Data,St);
        z -> rotate_axis_z_rmb(Data,St)
    end;
command({_,{rotate_to_axis,{'ASK',Ask}}}, St) ->
    wings:ask(selection_ask(Ask), St, fun rotate_axis_setup/2);
command({_,{rotate_to_axis,Data}}, St) ->
    rotate_axis_setup(Data,St);
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
    Desc = ?__(2,"Pick Point A for Vector A"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_B|Rest],Ask) ->
    Desc = ?__(3,"Pick Point B for Vector A"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_C|Rest],Ask) ->
    Desc = ?__(4,"Pick Point A for Vector B"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point_D|Rest],Ask) ->
    Desc = ?__(5,"Pick Point B for Vector B"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([vector|Rest],Ask) ->
    Desc = ?__(6,"Pick the vector"),
    selection_ask(Rest,[{axis_point,Desc}|Ask]).

%%%% Setup
%%%% LMB
rotate_axis_x_lmb({Center,A,B},St) ->
    rotate_axis_lmb({x,Center,A,B},St).
rotate_axis_y_lmb({Center,A,B},St) ->
    rotate_axis_lmb({y,Center,A,B},St).
rotate_axis_z_lmb({Center,A,B},St) ->
    rotate_axis_lmb({z,Center,A,B},St).

rotate_axis_x_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({x,Normal,Center},St).
rotate_axis_y_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({y,Normal,Center},St).
rotate_axis_z_rmb({Center,Normal,_CNormal},St) ->
    rotate_vector_rmb({z,Normal,Center},St).

rotate_axis_lmb({Axis0,Center,A,B},St) ->
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
rotate_axis_setup({Center,A,B,C,D},St) ->
    VecD0 = e3d_vec:sub(D,C),
    %% checking if the Destination vector is laying on a system axis (X/Y/Z)
    DAxis0 = e3d_vec:largest_dir(VecD0),
    OnAxis = not valid_vec(e3d_vec:norm(trunc_axis(DAxis0,VecD0))),
    case OnAxis of
        true ->
            case e3d_vec:plane_side(VecD0,{wings_util:make_vector(DAxis0),0.0}) of
                1 -> rotate_axis_lmb({DAxis0,Center,A,B},St);
                -1 -> rotate_axis_lmb({DAxis0,Center,B,A},St)
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

%%%% Rotate functions
rotate_axis(Center, Mr, St0) ->
    F = fun(_, #we{vp=Vtab0}=We, Acc0) ->
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
