%%
%%  wpc_gear.erl --
%%
%%     Gear and Tube Plugin
%%
%%  Copyright (c) 2003-2006 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_gear.erl,v 1.1 2006/08/02 18:50:48 antoneos Exp $
%%

-module(wpc_gear).
-export([init/0,menu/2,command/2]).

-import(math, [sqrt/1,cos/1,sin/1,pi/0,pow/2,exp/1]).

init() -> true.

menu({shape,more}, []) ->
    gear_menu();
menu({shape,more}, Menu) ->
    Menu ++ [separator|gear_menu()];
menu(_, Menu) -> Menu.

gear_menu() ->
    [{"Gear",gear,[option]},
     {"Tube",tube,[option]}].

command({shape,{more,{gear, Ask}}},_St) -> make_gear(Ask);
command({shape,{more,{tube, Ask}}},_St) -> make_tube(Ask);
command(_, _) -> next.

%%% The rest are local functions.

% ============
% === Gear ===
% ============
make_gear(Arg) when is_atom(Arg) ->
    wpa:dialog(Arg, "Gear Options", gear_dialog(),
	fun(Res) -> {shape,{more,{gear,Res}}} end);
make_gear(Arg) ->
    ArgDict = dict:from_list(Arg),
    NumTeeth = dict:fetch(numteeth, ArgDict),
    Radius1 = dict:fetch(radius1, ArgDict),
    ToothHeight = dict:fetch(toothheight, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),
    Vs = gear_verts(NumTeeth, Radius1, ToothHeight, Thickness),
    Fs = gear_faces(NumTeeth),
    {new_shape,"Gear",Fs,Vs}.

gear_dialog() ->
    NumTeeth = get_pref(numteeth, 50),
    Radius1 = get_pref(radius1, 1.0),
    ToothHeight = get_pref(toothheight, 0.04),
    Thickness = get_pref(thickness, 0.1),
    [{hframe, [{label, "NumTeeth"},
	       {slider, {text, NumTeeth,
	       [{key, numteeth}, {range, {5, 200}}]}}]},
     {hframe, [{label, "Radius"},
	       {slider, {text, Radius1,
	       [{key, radius1}, {range, {0.1, 20.0}}]}}]},
     {hframe, [{label, "ToothHeight"},
	       {slider, {text, ToothHeight,
	       [{key, toothheight}, {range, {0.0, 20.0}}]}}]},
     {hframe, [{label, "Thickness"},
	       {slider, {text, Thickness,
	       [{key, thickness}, {range, {0.0, 20.0}}]}}]}
    ].

gear_verts(NumTeeth, Radius1, ToothHeight, Thickness) ->
    Radius2 = Radius1-ToothHeight,
    Nres = NumTeeth*2,
    Delta = 2*pi()/Nres,
    InnerVerts = [{Radius2*cos(I*Delta), 0.0, Radius2*sin(I*Delta)}
		   || I <- lists:seq(0, Nres-1)],
    OuterVerts = [{Radius1*cos(I*Delta), 0.0, Radius1*sin(I*Delta)}
		   || I <- lists:seq(0, Nres-1)],
    Nverts = InnerVerts ++ OuterVerts,
    Tverts = [{X, +Thickness/2, Z} || {X,_,Z} <- Nverts],
    Bverts = [{X, -Thickness/2, Z} || {X,_,Z} <- Nverts],
    Tverts ++ Bverts.

gear_faces(NumTeeth) ->
    Nres = NumTeeth*2,
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

% ============
% === Tube ===
% ============
make_tube(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, "Tube Options",
	[{"Resolution",16},
	 {"Outer Radius",1.0},
	 {"Inner Radius",0.8},
	 {"Length",2.0}],
	fun(Res) -> {shape,{more,{tube,Res}}} end);
make_tube([Nres, Radius1, Radius2, Length]) ->
    Vs = tube_verts(Nres, Radius1, Radius2, Length),
    Fs = tube_faces(Nres),
    {new_shape,"Tube",Fs,Vs}.

tube_verts(Nres, Radius1, Radius2, Length) ->
    Delta = 2*pi()/Nres,
    InnerVerts = [{Radius2*cos(I*Delta), 0.0, Radius2*sin(I*Delta)}
		   || I <- lists:seq(0, Nres-1)],
    OuterVerts = [{Radius1*cos(I*Delta), 0.0, Radius1*sin(I*Delta)}
		   || I <- lists:seq(0, Nres-1)],
    Nverts = InnerVerts ++ OuterVerts,
    Tverts = [{X, +Length/2, Z} || {X,_,Z} <- Nverts],
    Bverts = [{X, -Length/2, Z} || {X,_,Z} <- Nverts],
    Tverts ++ Bverts.

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

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

% set_pref(KeyVals) ->
%     wpa:pref_set(?MODULE, KeyVals).

zip_lists_2e([], []) -> [];   % Zip two lists together, two elements at a time.
zip_lists_2e(A, B) ->	      % Both lists must be equal in length
    [HA1,HA2 | TA] = A,       % and must have an even number of elements
    [HB1,HB2 | TB] = B,
    lists:flatten([[HA1,HA2,HB1,HB2] | zip_lists_2e(TA, TB)]).

