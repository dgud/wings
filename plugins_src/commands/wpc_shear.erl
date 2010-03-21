%%
%%  wpc_shear.erl --
%%
%%    Plugin for shearing vertex selections
%%
%%  Copyright (c) 2008-2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%    $Id: wpc_shear.erl,v 1.43 Sun Feb 03 21:28:39 EST 2008 optigon Exp $
%%

-module(wpc_shear).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.
menu({vertex,deform}, Menu) ->
    Menu ++ [shear()];
menu(_,Menu) -> Menu.

shear() ->
    MenuTitle = ?__(1,"Shear"),
    F = fun(help, _Ns) ->
		{?__(2,"Shear XYZ"),[],
		 ?__(3,"Pick Glide Plane parallel to which movement will be restricted, Shearing Vector, Origin defining stationary Glide Plane, and Measuring Point for distance")};
	   (1, _Ns) -> xyz();
	   (2, _Ns) -> ignore;
	   (3, _Ns) ->
                Ask = [glide_plane,radial,origin,glide_point],
                {vertex,{deform,{shear,{'ASK',Ask}}}}
	end,
    {MenuTitle,{shear_cmd,F}}.

glide_plane_menu(GlidePlane) ->
    GPStr = wings_s:dir(GlidePlane),
    F = fun(help, _Ns) ->
		Str = ?__(1,"Glide Plane surfaces facing ~s.(Parallel to which vertex movement is restricted)"),
		{wings_util:format(Str, [GPStr]),
		 ?__(2,"Pick Shearing Vector and Origin.(Measuring Point calculated automatically)"),
		 ?__(3,"Pick Shearing Vector, Origin, and Measuring Point")};
	   (1, _Ns) -> xyz2(GlidePlane);
	   (2, _Ns) ->
		AskM = [radial,origin],
		{vertex,{deform,{shear,{GlidePlane,{mmb,{'ASK',AskM}}}}}};
	   (3, _Ns) ->
		AskR = [radial,origin,glide_point],
		{vertex,{deform,{shear,{GlidePlane,{rmb,{'ASK',AskR}}}}}}
	end,
    {GPStr,{GlidePlane,F},[]}.

radial_menu(GlidePlane,Radial) ->
    GPStr = wings_s:dir(GlidePlane),
    RadStr = wings_s:dir(Radial),
    F = fun(help, _Ns) ->
		Str = ?__(3,"Restrict vertex movement to Shearing Vector ~s across their individual Glide Planes facing ~s")++
		    ?__(4,".(Origin and Measuring Point caluculated automatically)"),
		Help = wings_util:format(Str, [RadStr,GPStr]),
		{Help,[],?__(5,"Pick Origin and Measuring Point")};
	   (1, _Ns) -> {vertex,{deform,{shear,{GlidePlane,{Radial,lmb}}}}};
	   (2, _Ns) -> ignore;
	   (3, _Ns) ->
		Ask = [origin,glide_point],
		{vertex,{deform,{shear,{GlidePlane,{Radial,{rmb,{'ASK',Ask}}}}}}}
        end,
    {RadStr,{Radial,F},[]}.

%%%% Axis menus
xyz() ->
    [glide_plane_menu(x),
     glide_plane_menu(y),
     glide_plane_menu(z)].

xyz2(x) ->
    [radial_menu(x,y),
     radial_menu(x,z)];

xyz2(y) ->
    [radial_menu(y,x),
     radial_menu(y,z)];

xyz2(z) ->
    [radial_menu(z,x),
     radial_menu(z,y)].

%%%% Commands
command({vertex,{deform,{shear,{GlidePlane0,{Radial0,{rmb,{'ASK',Ask}}}}}}},St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    Radial = axis_conversion(Radial0),
    wings:ask(selection_ask(Ask), St, fun ({Origin,GlidePoint}, St0) ->
        check_selection({GlidePlane,Radial,Origin,GlidePoint},St0)
    end);

command({vertex,{deform,{shear,{GlidePlane0,{Radial0,lmb}}}}},St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    Radial = axis_conversion(Radial0),
    [Origin,GlidePoint] = determine_boundaries(GlidePlane,St),
    check_selection({GlidePlane,Radial,Origin,GlidePoint},St);

command({vertex,{deform,{shear,{GlidePlane0,{rmb,{'ASK',Ask}}}}}},St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    wings:ask(selection_ask(Ask), St, fun ({Radial,Origin,GlidePoint}, St0) ->
        check_selection({GlidePlane,Radial,Origin,GlidePoint},St0)
    end);

command({vertex,{deform,{shear,{GlidePlane0,{mmb,{'ASK',Ask}}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun ({Radial,Origin}, St0) ->
        GlidePlane = axis_conversion(GlidePlane0),
        GlidePoint = get_glide_point(GlidePlane,Origin,St0),
        check_selection({GlidePlane,Radial,Origin,GlidePoint},St0)
    end);

command({vertex,{deform,{shear,{'ASK',Ask}}}}, St) ->
    wings:ask(selection_ask(Ask), St, fun (Data, St0) ->
        check_selection(Data,St0)
    end);

%%%% commands to match 'Repeat Drag' arguments from 'ASK' selections follow
command({vertex,{deform,{shear,{GlidePlane0,{Radial0,{rmb,{Origin,GlidePoint}}}}}}},St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    Radial = axis_conversion(Radial0),
    shear_callback({GlidePlane,Radial,Origin,GlidePoint},St);

command({vertex,{deform,{shear,{GlidePlane0,{rmb,{Radial,Origin,GlidePoint}}}}}}, St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    shear_callback({GlidePlane,Radial,Origin,GlidePoint},St);

command({vertex,{deform,{shear,{GlidePlane0,{mmb,{Radial,Origin}}}}}}, St) ->
    GlidePlane = axis_conversion(GlidePlane0),
    GlidePoint = get_glide_point(GlidePlane,Origin,St),
    shear_callback({GlidePlane,Radial,Origin,GlidePoint},St);

command({vertex,{deform,{shear,Data}}},St) ->
    shear_callback(Data,St);

command(_,_) -> next.

%%%%
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([AskType|Rest],Ask) ->
    {Pick,Desc} = case AskType of
      glide_plane ->
        {axis,[?__(1,"Select Glide Plane"),
               ?__(2," (plane to which all vertices move parallel)")]};
      origin ->
        {point,?__(3,"Select Origin (point defining stationary Glide Plane)")};
      radial ->
        {axis,[?__(4,"Select Shearing Vector"),
               ?__(5," (direction vertices will move across their individual Glide Planes)")]};
      glide_point ->
        {point,[?__(6,"Select Measuring Point"),
                ?__(7," (point according to which movement across Glide Plane will be measured)")]};
      re_radial ->
        {axis,[?__(8,"Reselect Shearing Vector"),
               ?__(5," (direction vertices will move across their individual Glide Planes)"),
               ?__(9,". Can't be perpendicular to specified Glide Plane")]};
      re_glide_point ->
        {point,[?__(10,"Reselect Measuring Point"),
                ?__(7," (point according to which movement across Glide Plane will be measured)"),
                ?__(11,". Must be on unique Glide Plane to specified Origin")]};
      re_glide_plane ->
        {axis,[?__(12,"Reselect Glide Plane"),
               ?__(2," (plane to which all vertices move parallel)"),
               ?__(13," Can't be parallel to specified Shearing Vector")]}
    end,
    selection_ask(Rest,[{Pick,Desc}|Ask]).

%%%%
get_glide_point(GlidePlane,Origin,St) ->
    [MinPoint,MaxPoint] = determine_boundaries(GlidePlane,St),
    Dist = abs(dist_along_vector(MaxPoint,Origin,GlidePlane)),
    case Dist < 1.0E-9 of
      true -> MinPoint;
      false -> MaxPoint
    end.

determine_boundaries(GlidePlane,St) ->
    GpNorm = e3d_vec:norm(GlidePlane),
    Center = wings_sel:center(St),
    DistsFromCntr = largest_dist_along_axis(GlidePlane,St),
    Max = abs(lists:max(DistsFromCntr)),
    Min = abs(lists:min(DistsFromCntr)),
    MaxPoint = e3d_vec:add(Center, e3d_vec:mul(GpNorm,Max)),
    MinPoint = e3d_vec:add(Center, e3d_vec:mul(GpNorm,-Min)),
    [MinPoint,MaxPoint].

%%%%
check_selection(AskResult,St) ->
    check_radial_glide_plane(AskResult,St).

check_radial_glide_plane({GlidePlane,Radial,Origin,GlidePoint},St) ->
    Deg = e3d_vec:degrees(GlidePlane,Radial),
    case ((Deg == 0.0) or (Deg == 180.0)) of
      true ->
        wings:ask(selection_ask([re_radial]), St, fun (Radial2, St0) ->
          check_selection({GlidePlane,Radial2,Origin,GlidePoint},St0)
      end);
      false ->
        check_glide_plane_norm({GlidePlane,Radial,Origin,GlidePoint},St)
    end.

check_glide_plane_norm({GlidePlane,Radial,Origin,GlidePoint},St) ->
    Point = intersect_vec_plane(GlidePoint,Origin,GlidePlane),
    Dist = abs(dist_along_vector(GlidePoint,Point,GlidePlane)),
    case Dist < 1.0E-9 of
      true ->
            wings:ask(selection_ask([re_glide_point]),
		      St, fun (GlidePoint2, St0) ->
				  check_selection({GlidePlane,Radial,Origin,GlidePoint2},St0)
			  end);
	false ->
	    shear_callback({GlidePlane,Radial,Origin,GlidePoint},St)
    end.

%%%%
axis_conversion(Axis) ->
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      _ -> Axis
    end.

%%%%
shear_callback({GlidePlane,Radial,Origin,GlidePoint},St) ->
    Mode = wings_pref:get_value(shear_mode,absolute),
    Dir = wings_pref:get_value(shear_drag,false),
    Anchor = wings_pref:get_value(shear_anchor,false),
    State = {Mode,Anchor,Dir},
    Norm = shear_norm(GlidePlane,Radial),
    Sf = shear_factor(GlidePlane,Origin,GlidePoint),
    DistsFromCntr = largest_dist_along_axis(Norm,St),
    DBbox = abs(lists:max(DistsFromCntr)) + abs(lists:min(DistsFromCntr)),
    ShearData = {Sf,Norm,DBbox,Dir,Anchor},
    Data = {GlidePlane,Radial,Origin,GlidePoint},
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
            shear_verts(ShearData,State,Data,Vs,We,Acc)
            end, [], St),
    Units = shear_units(Mode),
    Flags = [{mode,{shear_modes(),State}},{initial,[0.0,0.0,1.0]}],
    wings_drag:setup(Tvs, Units, Flags, St);

%%%% To catch 'repeat drag' arguments where the user was re-asked selections
shear_callback(_,St) ->
    Ask = [radial,glide_plane,origin,glide_point],
    wings:ask(selection_ask(Ask),
    St, fun (Data, St0) ->
        check_selection(Data,St0)
    end).

shear_units(absolute) ->
    [distance,skip,{curve,{1.0,infinity}}];
shear_units(relative) ->
    [percent,skip,{curve,{1.0,infinity}}];
shear_units(angle) ->
    Limit = 90.0 - 1.0E-9,
    [{angle,{-Limit,Limit}},skip,{curve,{1.0,infinity}}].

shear_modes() ->
    fun(help, State) -> shear_help(State);
      ({key,$1},{absolute,Anchor,Dir}) -> {relative,Anchor,Dir};
      ({key,$1},{relative,Anchor,Dir}) -> {angle,Anchor,Dir};
      ({key,$1},{angle,Anchor,Dir}) -> {absolute,Anchor,Dir};
      ({key,$2},{Mode,Anchor,true}) -> {Mode,Anchor,false};
      ({key,$2},{Mode,Anchor,false}) -> {Mode,Anchor,true};
      ({key,$3},{Mode,true,Dir}) -> {Mode,false,Dir};
      ({key,$3},{Mode,false,Dir}) -> {Mode,true,Dir};
      (units, {NewType,_Anchor,_Dir}) -> shear_units(NewType);
      (done, {NewMode,NewAnchor,NewDir}) ->
           wings_pref:set_value(shear_mode, NewMode),
           wings_pref:set_value(shear_drag, NewDir),
           wings_pref:set_value(shear_anchor, NewAnchor);
      (_,_) -> none
    end.
shear_help({Mode,Anchor,Dir}) ->
    ["[1] ",shear_mode_help(Mode),
     "  [2] ",shear_dir_help(Dir),
     "  [3] ",shear_anchor_help(Anchor)].

shear_mode_help(absolute) -> ?__(1,"Relative");
shear_mode_help(relative) -> ?__(2,"Angle");
shear_mode_help(angle) ->    ?__(3,"Absolute").

shear_dir_help(false) -> ?__(1,"Asymmetric");
shear_dir_help(true) -> ?__(2,"Symmetric").

shear_anchor_help(true) -> ?__(1,"Anchor Origin");
shear_anchor_help(false) -> ?__(2,"Free Origin").

shear_verts(ShearData,State,Data,Vs0,#we{id=Id}=We,Acc) ->
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs, We),
    [{Id,{Vs,shear_fun(ShearData,Data,VsPos,State)}}|Acc].

shear_fun({Sf,Norm,DBbox,Dir,Anchor},Data,VsPos,State) ->
    fun(new_mode_data, {NewState,_}) ->
          shear_fun({Sf,Norm,DBbox,Dir,Anchor},Data,VsPos,NewState);
       ([Dist,_,CurveFactor|_], A) ->
          shear_verts_by_mode({Sf,Norm,DBbox,Dir,Anchor},State,Data,VsPos,-Dist,CurveFactor,A)
    end.

shear_verts_by_mode(ShearData,State,Data,VsPositions,Dist,Cf,A) ->
    {ShearFac,Norm,DBbox,_Dir,_Anchor} = ShearData,
    lists:foldl(fun({V,Vpos}, VsAcc) ->
      case State of
        {relative,Anchor,Dir} ->
          [{V,relative_shear({ShearFac,Norm,DBbox,Dir,Anchor},Vpos,Dist,Cf,Data)}|VsAcc];
        {absolute,Anchor,Dir} ->
          [{V,distance_shear({ShearFac,Norm,DBbox,Dir,Anchor},Vpos,Dist,Cf,Data)}|VsAcc];
        {angle,Anchor,Dir} ->
          [{V,angle_shear({ShearFac,Norm,DBbox,Dir,Anchor},Vpos,Dist,Cf,Data)}|VsAcc]
      end
    end, A, VsPositions).

%%%% Main Functions
relative_shear(_ShearData,Vpos, 0.0, _Cf, _Data) ->
    Vpos;

relative_shear({Sf,Norm,DBbox,Dir,Anchor},Vpos,Dist,Cf,{GlidePlane,_Radial,Origin,_GlidePoint}) ->
    D = dist_along_vector(Vpos,Origin,GlidePlane),
    {Dist1,Factor} = shear_dist_factor(Cf,Sf,Dir,Anchor,Dist,D),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, -Dist1 * DBbox * Factor)).

distance_shear(_ShearData,Vpos, 0.0, _Cf, _Data) ->
    Vpos;

distance_shear({Sf,Norm,_DBbox,Dir,Anchor},Vpos,Dist,Cf,{GlidePlane,_Radial,Origin,_GlidePoint}) ->
    D = dist_along_vector(Vpos,Origin,GlidePlane),
    {Dist1,Factor} = shear_dist_factor(Cf,Sf,Dir,Anchor,Dist,D),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, -Dist1 * Factor)).

angle_shear(_ShearData,Vpos, 0.0, _Cf, _Data) ->
    Vpos;

angle_shear({Sf,Norm,_DBbox,Dir,Anchor},Vpos,Dist,Cf,{GlidePlane,Radial,Origin,GlidePoint}) ->
    V = intersect_vec_plane(GlidePoint,Origin,GlidePlane),
    {Xv,Yv,Zv} = V,

    %Rotate
    Axis = e3d_vec:cross(GlidePlane,Radial),
    A0 = e3d_mat:translate(Xv,Yv,Zv),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Dist, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Xv,-Yv,-Zv)),
    Pos = e3d_mat:mul_point(A2,GlidePoint),

    %Move back to original plane along angle's vector
    Pn0 = e3d_vec:sub(V,GlidePoint),
    Ln0 = e3d_vec:sub(Pos,V),
    Pn1 = e3d_vec:norm(Pn0),
    Ln1 = e3d_vec:norm(Ln0),
    Dp1 = e3d_vec:dot(Ln1,Pn1),

    Pos1 = case Dp1 of
             0.0 -> Vpos;
             _ -> Int1 = e3d_vec:dot(e3d_vec:sub(GlidePoint,Pos),Pn1)/Dp1,
                  e3d_vec:add(Pos, e3d_vec:mul(Ln1, Int1))
           end,

    NewDist = dist_along_vector(GlidePoint,Pos1,Norm),
    D = dist_along_vector(Vpos,Origin,GlidePlane),
    {Dist1,Factor} = shear_dist_factor(Cf,Sf,Dir,Anchor,NewDist,D),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Dist1 * Factor)).

%%%% Helper Functions
shear_dist_factor(Cf,Sf,Dir,Anchor,Dist,D) ->
    D1 = abs(D),
    Dist1 = case {D1 < 1.0e-12, D > 0,  Dir, Dist > 0} of %% <- fudge factor for
              {true, _,true,true} -> abs(Dist);           %%  near zero elements
              {true, _,true,false} -> -abs(Dist);
              {false,true,true,true} -> Dist;
              {false,true,true,false} -> Dist;
              {_,_,true,_} -> -Dist;
              {_,_,false,_} -> Dist
             end,

    Anch = case Anchor of
             true ->  abs(Sf - D1);
             false -> D1
           end,
    Factor = math:pow(Anch/Sf,Cf),
    {Dist1,Factor}.

dist_along_vector(PosA,PosB,Vector) ->
    %% Return Distance between PosA and PosB along Vector
    {Xa,Ya,Za} = PosA,
    {Xb,Yb,Zb} = PosB,
    {Vx,Vy,Vz} = e3d_vec:norm(Vector),
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

intersect_vec_plane(PosA,PosB,Vector) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    PlaneNorm = e3d_vec:norm(Vector),
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

shear_norm(GlidePlane,Radial) ->
    Radial0 = e3d_vec:cross(GlidePlane,Radial),
    Radial1 = e3d_vec:cross(Radial0,GlidePlane),
    e3d_vec:norm(Radial1).

shear_factor(GlidePlane,Origin,GlidePoint) ->
    abs(dist_along_vector(GlidePoint,Origin,GlidePlane)).

%%%% Gets the greatest distance along Norm for the selected vertices
largest_dist_along_axis(Norm,St) ->
    Center = wings_sel:center(St),
    Distances = wings_sel:fold(fun(Vs,We,Acc) ->
                    V = gb_sets:to_list(Vs),
                    get_dist_list(Center,Norm,V,We,Acc)
               end, [], St),
    lists:merge(Distances).

get_dist_list(Center,Norm,V,We,Acc) ->
    G = lists:foldl(fun(Vert,A) ->
            #we{vp=Vtab}=We,
            Pos = array:get(Vert,Vtab),
            [dist_along_vector(Pos,Center,Norm)|A]
        end, [], V),
    G1 = lists:max(G),
    G2 = lists:min(G),
    [[G1,G2]|Acc].
