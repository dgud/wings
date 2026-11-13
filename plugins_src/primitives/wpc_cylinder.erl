%%
%%  wpc_cylinder --
%%
%%     Cylinder, Gear and Tube Plugin
%%
%%  Copyright (c) 2003-2012 Anthony D'Agostino
%%  
%%  Former wpc_gear.erl adapted to single preview dialog.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_cylinder).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").
-import(math, [cos/1,sin/1,pi/0]).

init() -> true.

menu({shape}, []) ->
    menu();
menu({shape}, Menu) ->
    menu()++Menu;
menu(_, Menu) -> Menu.

menu() ->
    [{cylinder(),cylinder,?__(2,"Create a cylinder"),[option]}].

cylinder() ->
    cylinder_type(cylinder).

cylinder_type(cylinder) -> ?__(1,"Cylinder");
cylinder_type(tube) -> ?__(2,"Tube");
cylinder_type(gear) -> ?__(3,"Gear");
cylinder_type(pie) -> ?__(4,"Pie").

command({shape,{cylinder, Ask}}, St) -> make_cylinder(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Cylinder
%%%

cylinder_dialog() ->
    Hook = fun(Var, Val, Sto) ->
	case Var of
	    cylinder_type ->
		wings_dialog:enable(thickness, (Val=/=cylinder) and (Val=/=pie) , Sto),
		wings_dialog:enable(angle_offset, Val==pie, Sto),
		wings_dialog:enable(degrees, Val==pie, Sto);
	    _ -> ok
	end
    end,

    [{label_column, [
	{?__(1,"Sections"), {text,16,[{key,sections},{range,{3,infinity}}]}},
	{?__(2,"Height"), {text,2.0,[{key,height},{range,{0.0,infinity}}]}},
	{" ", separator},
	{?__(3,"Top X Radius"), {text,1.0,[{key,top_x},{range,{0.0,infinity}}]}},
	{?__(4,"Top Z Radius"), {text,1.0,[{key,top_z},{range,{0.0,infinity}}]}},
	{" ", separator},
	{?__(5,"Bottom X Radius"), {text,1.0,[{key,bottom_x},{range,{0.0,infinity}}]}},
	{?__(6,"Bottom Z Radius"), {text,1.0,[{key,bottom_z},{range,{0.0,infinity}}]}}]
     },
     {hradio, [
	{cylinder_type(cylinder),cylinder},
	{cylinder_type(tube),tube},
	{cylinder_type(gear),gear},
	{cylinder_type(pie),pie}],
		cylinder, [{key,cylinder_type},{hook, Hook},{title,?__(10,"Cylinder Type")}]},
     {label_column,[
	 {?__(11,"Thickness"), {text,0.25,[{key,thickness},{range,{0.0,infinity}}]}},
	 {?__(12,"Degrees"), {text,360.0,[{key,degrees},{range,{0.0,360.0}}]}},
	 {?__(13,"Angle Offset"), {text,0.0,[{key,angle_offset},{range,{-360.0,360.0}}]}}]},
     wings_shapes:transform_obj_dlg()].

make_cylinder(Arg, St) when is_atom(Arg) ->
    Qs = cylinder_dialog(),
    Label = ?__(1,"Cylinder Options"),
    wings_dialog:dialog_preview({shape,cylinder}, Arg, Label, Qs, St);
make_cylinder(Arg, _St) ->
    ArgDict = dict:from_list(Arg),
    Sections = dict:fetch(sections, ArgDict),
    Height = dict:fetch(height, ArgDict),
    TopX = dict:fetch(top_x, ArgDict),
    TopZ = dict:fetch(top_z, ArgDict),
    BotX = dict:fetch(bottom_x, ArgDict),
    BotZ = dict:fetch(bottom_z, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),
    Degrees = dict:fetch(degrees, ArgDict),
    AngleOffset = dict:fetch(angle_offset, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
	      {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
	      dict:fetch(ground, ArgDict)],

    Type = dict:fetch(cylinder_type, ArgDict),
    case Type of
        cylinder ->
            make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, Modify);
        tube ->
            make_tube(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, Modify);
        gear ->
            [Min|_] = lists:sort([TopX, TopZ, BotX, BotZ]),
            Thickness1 = min(Min, Thickness),
            make_gear(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness1, Modify);
        pie ->
            make_pie(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, Modify)
    end.

%%%
%%% Cylinder
%%%

make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, [Rot, Mov, Ground]) ->
    Vs0 = cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = cylinder_uv_faces(Sections, Vs0, cylinder_faces(Sections)),
    {new_shape,cylinder_type(cylinder),Fs,Vs}.

cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    Top = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    Bottom = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    Top ++ Bottom.

cylinder_faces(N) ->
    Ns =lists:reverse(lists:seq(0, N-1)),
    Upper= Ns,
    Lower= lists:seq(N, N+N-1),
    Sides= [[I, (I+1) rem N, N + (I+1) rem N, N + I] || I <- Ns],
    [Upper, Lower | Sides].

%%%
%%% Gear
%%%

make_gear(Sections0, TopX, TopZ, BotX, BotZ, Height, ToothHeight, [Rot, Mov, Ground]) ->
    Sections = (Sections0 div 2)*2,
    Vs0 = gear_verts(Sections, TopX, TopZ, BotX, BotZ, Height, ToothHeight),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = gear_uv_faces(Sections, Vs0, gear_faces(Sections)),
    {new_shape,cylinder_type(gear),Fs,Vs}.

gear_verts(Sections, TopX, TopZ, BotX, BotZ, Height, ToothHeight) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-ToothHeight, TopZ-ToothHeight, 0.0),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-ToothHeight, BotZ-ToothHeight, 0.0),
    OuterVerts = TopOuter ++ TopInner,
    InnerVerts = BotOuter ++ BotInner,
    OuterVerts ++ InnerVerts.

gear_faces(Nres) ->
    Offset = Nres*2,
    A = lists:seq(Nres-1, 0, -1),
    B = lists:seq(2*Nres-2, Nres, -1) ++ [2*Nres-1],
    TopFace = zip_lists_2e(A,B),
    BotFace = [Index+Offset || Index <- lists:reverse(TopFace)],
    InnerFaces = [[I, I+1, I+Offset+1, I+Offset]
		  || I <- lists:seq(0, Nres-2, 2)],
    OuterFaces = [[I, I+1, I+Offset+1, I+Offset]
		  || I <- lists:seq(Nres+1, 2*Nres-3, 2)]
		  ++ [[2*Nres-1, Nres, 3*Nres, 4*Nres-1]], % the last face
    SideFacesO = [[I, I+Nres, 3*Nres+I, 2*Nres+I]
		  || I <- lists:seq(1, Nres-1, 2)],
    SideFacesE = [[I+Nres, I, 2*Nres+I, 3*Nres+I]
		  || I <- lists:seq(2, Nres-2, 2)]
		  ++ [[Nres, 0, 2*Nres, 3*Nres]], % the last face
    [TopFace]++[BotFace] ++ InnerFaces++OuterFaces ++ SideFacesO++SideFacesE.

%%%
%%% Tube
%%%

make_tube(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness, [Rot, Mov, Ground]) ->
    Vs0 = tube_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = tube_uv_faces(Sections, Vs0, tube_faces(Sections)),
    {new_shape,cylinder_type(tube),Fs,Vs}.

tube_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, 0.0),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, 0.0),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-Thickness, TopZ-Thickness, 0.0),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-Thickness, BotZ-Thickness, 0.0),
    OuterVerts = TopOuter ++ BotOuter,
    InnerVerts = TopInner ++ BotInner,
    OuterVerts ++ InnerVerts.

tube_faces(Nres) ->
    Offset = 2*Nres,
    TopFaces =
	[[I, I+1, I+Nres+1, I+Nres] || I <- lists:seq(0, Nres-2)] ++
	[[Nres-1, 0, Nres, 2*Nres-1] ],        % the last face
    BotFaces = [[D+Offset,C+Offset,B+Offset,A+Offset] || [A,B,C,D] <- TopFaces],
    InnerFaces =
	[[I, I-1, I+Offset-1, I+Offset] || I <- lists:seq(1, Nres-1)] ++
	[[0, Nres-1, 3*Nres-1, 2*Nres] ],      % the last face
    OuterFaces =
	[[I, I+1, I+Offset+1, I+Offset] || I <- lists:seq(Nres, 2*Nres-2)] ++
	[[2*Nres-1, Nres, 3*Nres, 4*Nres-1] ], % the last face
    TopFaces ++ BotFaces ++ InnerFaces ++ OuterFaces.

cylinder_uv_faces(N, Vs, [Upper,Lower|Sides]) ->
    Vt = list_to_tuple(Vs),
    Pos = fun(I, Lv) -> element(Lv*N + (I rem N) + 1, Vt) end,
    Cell = fun(V) -> {V rem N, V div N} end,
    {Caps, WallFs} = prm_uv_unfold:layout({Pos, N, 2, false, Cell, Sides}, [Upper,Lower], Vt),
    Caps ++ WallFs.

%% The outline of the gear walks the tips, the flanks and the valleys; the
%% vertices of a ring are numbered outer first and then inner.
gear_uv_faces(Nres, Vs, [Top,Bot|Walls]) ->
    Vt = list_to_tuple(Vs),
    Count = Nres*2,
    Steps = [[2*J, 2*J+1, Nres+2*J+1, Nres+((2*J+2) rem Nres)]
             || J <- lists:seq(0, (Nres div 2)-1)],
    Outline = list_to_tuple(lists:append(Steps)),
    Index = maps:from_list(lists:zip(tuple_to_list(Outline), lists:seq(0, Count-1))),
    Pos = fun(I, Lv) -> element(Lv*Count + element((I rem Count)+1, Outline) + 1, Vt) end,
    Cell = fun(V) -> {maps:get(V rem Count, Index), V div Count} end,
    {Caps, WallFs} = prm_uv_unfold:layout({Pos, Count, 2, false, Cell, Walls}, [Top,Bot], Vt),
    Caps ++ WallFs.

%% The levels of the loop around the wall are the top outer, bottom outer,
%% bottom inner and top inner rings, so the vertex groups are swapped 2 <-> 3.
tube_uv_faces(Nres, Vs, Fs) ->
    Vt = list_to_tuple(Vs),
    Levels = {0, 1, 3, 2},
    Pos = fun(I, Lv) -> element(element(Lv+1, Levels)*Nres + (I rem Nres) + 1, Vt) end,
    Cell = fun(V) -> {V rem Nres, element((V div Nres)+1, Levels)} end,
    {[], WallFs} = prm_uv_unfold:layout({Pos, Nres, 4, true, Cell, Fs}, [], Vt),
    WallFs.

ring_of_verts(Rings, Delta, YAxis, XAxis, ZAxis, Offset) ->
    [{XAxis*cos(Offset+I*Delta), YAxis, ZAxis*sin(Offset+I*Delta)} || I <- Rings].

zip_lists_2e([], []) -> [];   % Zip two lists together, two elements at a time.
zip_lists_2e(A, B) ->	      % Both lists must be equal in length
    [HA1,HA2 | TA] = A,       % and must have an even number of elements
    [HB1,HB2 | TB] = B,
    lists:flatten([[HA1,HA2,HB1,HB2] | zip_lists_2e(TA, TB)]).

%%%
%%% Pie
%%%

make_pie(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset, [Rot, Mov, Ground]) ->
    Vs0 = pie_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Count = trunc(length(Vs0)/2),
    Fs = cylinder_uv_faces(Count, Vs0, cylinder_faces(Count)),
    {new_shape,cylinder_type(pie),Fs,Vs}.

pie_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Degrees, AngleOffset) ->
    YAxis = Height/2,
    DtoRad = math:pi()/180.0,
    Offset = AngleOffset*DtoRad,
    Delta = (Degrees*DtoRad)/Sections,
    Rings = lists:seq(0, Sections),
    [Top0|_] = Top = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ, Offset),
    [Bottom0|_] = Bottom = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ, Offset),
    {TopExt,BottomExt} =
        case Degrees of
            360.0 -> {[Top0,{0.0,YAxis,0.0}],[Bottom0,{0.0,-YAxis,0.0}]};
            _ -> {[{0.0,YAxis,0.0}],[{0.0,-YAxis,0.0}]}
        end,
    Top ++ TopExt ++ Bottom ++ BottomExt.
