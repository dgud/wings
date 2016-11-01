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
    ?__(1,"Cylinder").

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
		wings_dialog:enable(thickness, Val=/=cylinder, Sto);
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
	{cylinder(),cylinder},
	{?__(8,"Tube"),tube},
	{?__(9,"Gear"),gear}],
		cylinder, [{key,cylinder_type},{hook, Hook},{title,?__(10,"Cylinder Type")}]},
     {label_column,[
	 {?__(11,"Thickness"), {text,0.25,[{key,thickness},{range,{0.0,infinity}}]}}]},
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
            make_gear(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness1, Modify)
    end.

%%%
%%% Cylinder
%%%

make_cylinder(Sections, TopX, TopZ, BotX, BotZ, Height, [Rot, Mov, Ground]) ->
    Vs0 = cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground, Vs0),
    Fs = cylinder_faces(Sections),
    {new_shape,cylinder(),Fs,Vs}.

cylinder_verts(Sections, TopX, TopZ, BotX, BotZ, Height) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    Top = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ),
    Bottom = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ),
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
    Fs = gear_faces(Sections),
    {new_shape,?__(1,"Gear"),Fs,Vs}.

gear_verts(Sections, TopX, TopZ, BotX, BotZ, Height, ToothHeight) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-ToothHeight, TopZ-ToothHeight),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-ToothHeight, BotZ-ToothHeight),
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
    Fs = tube_faces(Sections),
    {new_shape,?__(1,"Tube"),Fs,Vs}.

tube_verts(Sections, TopX, TopZ, BotX, BotZ, Height, Thickness) ->
    YAxis = Height/2,
    Delta = pi()*2/Sections,
    Rings = lists:seq(0, Sections-1),
    TopOuter = ring_of_verts(Rings, Delta, YAxis, TopX, TopZ),
    BotOuter = ring_of_verts(Rings, Delta, -YAxis, BotX, BotZ),
    TopInner = ring_of_verts(Rings, Delta, YAxis, TopX-Thickness, TopZ-Thickness),
    BotInner = ring_of_verts(Rings, Delta, -YAxis, BotX-Thickness, BotZ-Thickness),
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

ring_of_verts(Rings, Delta, YAxis, XAxis, ZAxis) ->
    [{XAxis*cos(I*Delta), YAxis, ZAxis*sin(I*Delta)} || I <- Rings].

zip_lists_2e([], []) -> [];   % Zip two lists together, two elements at a time.
zip_lists_2e(A, B) ->	      % Both lists must be equal in length
    [HA1,HA2 | TA] = A,       % and must have an even number of elements
    [HB1,HB2 | TB] = B,
    lists:flatten([[HA1,HA2,HB1,HB2] | zip_lists_2e(TA, TB)]).
