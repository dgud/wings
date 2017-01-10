%%
%%  wings_deform.erl --
%%
%%     This module contains the Deform commands for vertices.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_deform).
-export([sub_menu/1,command/2]).

-include("wings.hrl").
-import(lists, [foldl/3,map/2,reverse/1]).
-define(PI, math:pi()).

sub_menu(_St) ->
    InflateHelp = {?__(1,"Inflate elements"),[],
		   ?__(2,"Pick center and radius")},
    {deform,[{?__(3,"Crumple"),{crumple,crumple_dirs()},
	      ?__(4,"Randomly move vertices")},
	     {?__(5,"Inflate"),inflate_fun(),InflateHelp,[]},
	     {?__(6,"Taper"),{taper,
		       [taper_item(x),
			taper_item(y),
			taper_item(z)]}},
	     {?__(7,"Twist"),{twist,dirs(twist)}},
	     {?__(8,"Torque"),{torque,dirs(torque)}}]}.

crumple_dirs() ->
    Str = ?__(8,"Move each vertex a random amount along") ++ " ",
    [{?__(1,"Random"), random,
      Str ++ ?__(2, "a random direction")},
     {wings_util:cap(wings_s:dir(normal)),normal, 
      Str ++ ?__(4,"its normal")},
     {wings_s:dir(x),x,Str ++ wings_s:dir_axis(x)},
     {wings_s:dir(y),y,Str ++ wings_s:dir_axis(y)},
     {wings_s:dir(z),z,Str ++ wings_s:dir_axis(z)}].

dirs(Cmd) ->
    [dir(x, Cmd),
     dir(y, Cmd),
     dir(z, Cmd)].

dir(Axis, Cmd) ->
    AxisStr = wings_util:upper(atom_to_list(Axis)),
    Help = ?__(1,"Twist selected vertices around the ") ++ AxisStr ++
	case Cmd of
	    twist -> ?__(2," passing through the center of the selection");
	    torque ->?__(3," passing through the origin")
	end,
    {AxisStr,Axis,Help,[]}.

taper_item(Axis) ->
    Effects = effect_menu(Axis),
    AxisStr = wings_util:upper(Axis),
    F = fun(help, _Ns) ->
		[Effect|_] = Effects,
		TaperAlong = ?__(1,"Taper along ") ++ AxisStr,
		{TaperAlong ++?__(2," (with effect on ") 
		 ++wings_util:upper(Effect) 
		 ++ ?__(3,")"),
		 ?__(4,"Choose effect axis"),
		 ?__(5,"Pick axis center location")
		};
	   (1, Ns) ->
		[Effect|_] = Effects,
		wings_menu:build_command(Effect, Ns);
	   (2, _Ns) ->
		expand_effects(Effects, []);
	   (3, Ns) ->
		[Effect|_] = Effects,
		Ask = {'ASK',{[{point,?__(6,"Pick taper origin")}],[Effect]}},
		wings_menu:build_command(Ask, Ns)
	end,
    {AxisStr,{Axis,F},[]}.

effect_menu(x) -> [yz,y,z];
effect_menu(y) -> [xz,x,z];
effect_menu(z) -> [xy,x,y].

expand_effects([H|T], Acc) ->
    Effect = wings_util:upper(H),
    Help = {?STR(taper_item,7,"Effect on ")++Effect,[],
	    ?STR(taper_item,5, "Pick axis center location")},
    Item = {Effect,effect_fun(H),Help,[]},
    expand_effects(T, [Item|Acc]);
expand_effects([], Acc) -> reverse(Acc).

effect_fun(Effect) ->
    fun(1, Ns) -> wings_menu:build_command(Effect, Ns);
       (2, _Ns) -> ignore;
       (3, Ns) ->
	    Ask = {'ASK',{[{point,?STR(taper_item,6,"Pick taper origin")}],[Effect]}},
	    wings_menu:build_command(Ask, Ns)
    end.
    
inflate_fun() ->
    fun(help, _) ->
	    PickHelp = ?__(1,"Pick center and radius"),
	    {?__(2,"Inflate elements"),PickHelp,PickHelp};
       (1, _Ns) -> {vertex,{deform,inflate}};
       (_, Ns) ->
	    Ask = {'ASK',{[{point,?__(3,"Pick center point")},
			   {point,?__(4,"Pick point to define radius")}],[]}},
	    wings_menu:build_command(Ask, [inflate|Ns])
    end.

command({crumple,Dir}, St) -> crumple(Dir, St);
command(inflate, St) -> inflate(St);
command({inflate,What}, St) -> inflate(What, St);
command({taper,Taper}, St) -> taper(Taper, St);
command({twist,Axis}, St) -> twist(Axis, St);
command({torque,Axis}, St) -> torque(Axis, St).

%%
%% The Crumple deformer.
%%

crumple(Dir, St) ->
    U = [{percent,{-20.0,20.0}}],
    wings_drag:fold(fun(Vs, We) ->
                            crumple(Dir, Vs, We)
                    end, U, St).

crumple(normal, Vs0, We) ->
    ExpSeed = rand:export_seed_s(rand:seed_s(exs64)),
    Vs = gb_sets:to_list(Vs0),
    VsPos0 = wings_util:add_vpos(Vs, We),
    VsPos = [{V,Pos,wings_vertex:normal(V, We)} || {V,Pos} <- VsPos0],
    Fun = fun([Dx], A) ->
		  rand:seed(ExpSeed),
		  foldl(fun({V,Pos0,N}, VsAcc) ->
				R1 = rand:normal()*Dx/10,
				Pos = e3d_vec:add_prod(Pos0, N, R1),
				[{V,Pos}|VsAcc]
			end, A, VsPos)
	  end,
    {Vs,Fun};
crumple(Dir, Vs0, We) ->
    {Xmask,Ymask,Zmask} = crumple_mask(Dir),
    ExpSeed = rand:export_seed_s(rand:seed_s(exs64)),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Dx], A) ->
		  rand:seed(ExpSeed),
		  foldl(fun({V,{X0,Y0,Z0}}, VsAcc) ->
				{R1,R2,R3} = rnd(Dx/4),
				X = X0 + R1*Xmask,
				Y = Y0 + R2*Ymask,
				Z = Z0 + R3*Zmask,
				[{V,{X,Y,Z}}|VsAcc]
			end, A, VsPos)
	  end,
    {Vs,Fun}.

crumple_mask(x) -> {1,0,0};
crumple_mask(y) -> {0,1,0};
crumple_mask(z) -> {0,0,1};
crumple_mask(random) -> {1,1,1}.

rnd(Sc) when is_float(Sc) ->
    {Sc*rand:normal(),Sc*rand:normal(),Sc*rand:normal()}.

%%
%% The Inflate deformer.
%%

inflate(St) ->
    wings_drag:fold(fun inflate_1/2, [percent], St).

inflate_1(Vs0, #we{vp=Vtab}=We) ->
    Vs = gb_sets:to_list(Vs0),
    Center = wings_vertex:center(Vs, We),
    Radius = foldl(
	       fun(V, R0) ->
		       VPos = wings_vertex:pos(V, Vtab),
		       case e3d_vec:dist(Center, VPos) of
			   R when R > R0 -> R;
			   _Smaller -> R0
		       end
	       end, 0.0, Vs),
    inflate(Center, Radius, Vs, We).

inflate({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun inflate/2);
inflate({Center,Outer}, St) ->
    Radius = e3d_vec:dist(Center, Outer),
    wings_drag:fold(fun(Vs, We) ->
                            inflate(Center, Radius,
                                    gb_sets:to_list(Vs), We)
                    end, [percent], St).

inflate(Center, Radius, Vs, #we{vp=Vtab}=We) ->
    F = fun(V) ->
                VPos = array:get(V, Vtab),
                D = e3d_vec:dist(Center, VPos),
                Dir = e3d_vec:norm_sub(VPos, Center),
                Vec = e3d_vec:mul(Dir, Radius-D),
                {Vec,[V]}
        end,
    VecVs = [F(V) || V <- Vs],
    wings_drag:translate_fun(VecVs, We).

%%
%% The Taper deformer.
%%

taper({Primary,{{'ASK',Ask},Center}}, St0) ->
    wings:ask(Ask, St0,
	      fun(Effect, St) ->
		      taper({Primary,{Effect,Center}}, St)
	      end);
taper({Primary,{Effect,{'ASK',Ask}}}, St0) ->
    wings:ask(Ask, St0,
	      fun(Center, St) ->
		      taper({Primary,{Effect,Center}}, St)
	      end);
taper({Primary,{'ASK',Ask}}, St0) ->
    wings:ask(Ask, St0,
	      fun(Center, St) ->
		      taper({Primary,Center}, St)
	      end);
taper({Primary,{Effect,Center}}, St) ->
    taper_1(Primary, Effect, Center, St);
taper({Primary,Effect}, St) ->
    taper_1(Primary, Effect, center, St).

taper_1(Primary, Effect, Center, St) ->
    wings_drag:fold(fun(Vs, We) ->
                            taper_2(Vs, We, Primary, Effect, Center)
                    end, [percent], St).

taper_2(Vs, We, Primary, Effect, Center) ->
    [MinR,MaxR] = wings_vertex:bounding_box(Vs, We),
    Key = key(Primary),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Primary),
    taper_3(Vs, We, Key, Effect, MinR, MaxR, Center).

taper_3(Vs0, We, Key, Effect, MinR, MaxR, Center) ->
    Tf = taper_fun(Key, Effect, Center, MinR, MaxR),
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = fun([Dx], A) ->
		  U = Dx + 1.0,
		  foldl(fun({V,Pos}, VsAcc) ->
				[{V,Tf(U, Pos)}|VsAcc]
			end, A, VsPos)
	  end,
    {Vs,Fun}.

taper_fun(Key, Effect, center, MinR, MaxR) ->
    Center = e3d_vec:average(MinR, MaxR),
    taper_fun(Key, Effect, Center, MinR, MaxR);
taper_fun(Key, Effect, Center, {_,_,_}=MinR, {_,_,_}=MaxR) ->
    Origin = element(Key, Center),
    Range = element(Key, MaxR) - element(Key, MinR),
    {Ekey1,Ekey2} = effect(Effect),
    Eoffset1 = (element(Ekey1, MinR)+element(Ekey1, MaxR))/2,
    case Ekey2 of
	none ->
	    fun(U, Pos) when is_float(U), is_float(Origin), is_float(Range) ->
		    S0 = 1.0+(element(Key, Pos)-Origin)/Range,
		    S = mix(S0, U),
		    E = S * (element(Ekey1, Pos)-Eoffset1) + Eoffset1,
		    setelement(Ekey1, Pos, E)
	    end;
	_Other ->
	    Eoffset2 = (element(Ekey2, MinR)+element(Ekey2, MaxR))/2,
	    fun(U, Pos0) when is_float(U), is_float(Origin), is_float(Range) ->
		    S0 = 1.0+(element(Key, Pos0)-Origin)/Range,
		    S = mix(S0, U),
		    E0 = S * (element(Ekey1, Pos0)-Eoffset1) + Eoffset1,
		    E =  S * (element(Ekey2, Pos0)-Eoffset2) + Eoffset2,
		    Pos = setelement(Ekey1, Pos0, E0),
		    setelement(Ekey2, Pos, E)
	    end
    end.

effect(yz) -> {2,3};
effect(xy) -> {1,2};
effect(xz) -> {1,3};
effect(Single) -> {key(Single),none}.

mix(A, F) ->
    F + (1-F)*A.
    
%%%
%%% The Twist deformer.
%%%

twist(Axis, St) ->
    wings_drag:fold(fun(Vs, We) ->
                            twist(Vs, We, Axis)
                    end, [angle], St).

twist(Vs0, We, Axis) ->
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Axis),
    Tf = twist_fun(Axis, e3d_vec:average(MinR, MaxR)),
    Vs = gb_sets:to_list(Vs0),
    Fun = twister_fun(Vs, Tf, Min, Range, We),
    {Vs,Fun}.

twist_fun(x, {_,Cy,Cz}) ->
    fun(U, Min, {X,Y0,Z0})
       when is_float(U), is_float(Min), is_float(X), is_float(Y0), is_float(Z0) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    Y = Y0 - Cy,
	    Z = Z0 - Cz,
	    {X,Y*Cos-Z*Sin+Cy,Y*Sin+Z*Cos+Cz}
    end;
twist_fun(y, {Cx,_,Cz}) ->
    fun(U, Min, {X0,Y,Z0})
       when is_float(U), is_float(Min), is_float(X0), is_float(Y), is_float(Z0) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    X = X0 - Cx,
	    Z = Z0 - Cz,
	    {X*Cos+Z*Sin+Cx,Y,Z*Cos-X*Sin+Cz}
    end;
twist_fun(z, {Cx,Cy,_}) ->
    fun(U, Min, {X0,Y0,Z})
       when is_float(U), is_float(Min), is_float(X0), is_float(Y0), is_float(Z) ->
	    Angle = U*(Z-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    X = X0 - Cx,
	    Y = Y0 - Cy,
	    {X*Cos-Y*Sin+Cx,X*Sin+Y*Cos+Cy,Z}
    end.

%%%
%%% The Twisty Twist deformer.
%%%

torque(Axis, St) ->
    wings_drag:fold(fun(Vs, We) ->
                            torque(Vs, We, Axis)
                    end, [angle], St).

torque(Vs0, We, Axis) ->
    Tf = torque_fun(Axis),
    Key = key(Axis),
    [MinR,MaxR] = wings_vertex:bounding_box(Vs0, We),
    Min = element(Key, MinR),
    Max = element(Key, MaxR),
    Range = Max - Min,
    check_range(Range, Axis),
    Vs = gb_sets:to_list(Vs0),
    Fun = twister_fun(Vs, Tf, Min, Range, We),
    {Vs,Fun}.

torque_fun(x) ->
    fun(U, Min, {X,Y,Z})
       when is_float(U), is_float(Min), is_float(X), is_float(Y), is_float(Z) ->
	    Angle = U*(X-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X,Y*Cos-Z*Sin,Y*Sin+Z*Cos}
    end;
torque_fun(y) ->
    fun(U, Min, {X,Y,Z})
       when is_float(U), is_float(Min), is_float(X), is_float(Y), is_float(Z) ->
	    Angle = U*(Y-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos+Z*Sin,Y,Z*Cos-X*Sin}
    end;
torque_fun(z) ->
    fun(U, Min, {X,Y,Z})
       when is_float(U), is_float(Min), is_float(X), is_float(Y), is_float(Z) ->
	    Angle = U*(Z-Min),
	    Cos = math:cos(Angle),
	    Sin = math:sin(Angle),
	    {X*Cos-Y*Sin,X*Sin+Y*Cos,Z}
    end.

%%%
%%% Utilities.
%%%

key(x) -> 1;
key(y) -> 2;
key(z) -> 3.

twister_fun(Vs, Tf, Min, Range, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    fun([Angle], A) ->
	    U = (Angle / 180.0 * ?PI)/Range,
	    foldl(fun({V,Pos}, VsAcc) ->
			  [{V,Tf(U, Min, Pos)}|VsAcc]
		  end, A, VsPos)
    end.

check_range(Range, Axis0) when Range < 0.01 ->
    Axis = wings_util:upper(atom_to_list(Axis0)),
    Error = lists:concat([?__(1,"Extent along "),Axis,
			  ?__(2,"axis is too short.")]),
    wings_u:error_msg(Error);
check_range(_Range, _Axis) -> ok.
