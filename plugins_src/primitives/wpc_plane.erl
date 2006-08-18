%%
%%  wpc_plane.erl --
%%
%%     Plane Plugin (generates four classic types of planes)
%%
%%  Copyright (c) 2002-2006 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_plane.erl,v 1.3 2006/07/29 20:48:58 antoneos Exp $
%%

-module(wpc_plane).
-export([init/0,menu/2,command/2]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0,pow/2,exp/1]).
-include("wings_intl.hrl").

init() -> true.

menu({shape,more}, []) ->
    plane_menu();
menu({shape,more}, Menu) ->
    Menu ++ [separator|plane_menu()];
menu(_, Menu) -> Menu.

plane_menu() ->
    [{?__(1,"Regular Plane"),regularplane,[option]},
     {?__(2,"Lumpy Plane"),lumpyplane,[option]},
     {?__(3,"Wavy Plane"),wavyplane,[option]},
     {?__(4,"Sombrero Plane"),sombreroplane,[option]}].

command({shape,{more,{regularplane  ,Ask}}},_St) -> make_regular_plane(Ask);
command({shape,{more,{lumpyplane    ,Ask}}},_St) -> make_lumpy_plane(Ask);
command({shape,{more,{wavyplane     ,Ask}}},_St) -> make_wavy_plane(Ask);
command({shape,{more,{sombreroplane ,Ask}}},_St) -> make_sombrero_plane(Ask);
command(_, _) -> next.

%%% The rest are local functions.

% ======= Regular Plane =======
make_regular_plane(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Create Regular Plane"),
	    [{?__(2,"Resolution"),7},
	     {?__(3,"Size"),2.0},
	     {?__(4,"Thickness"), 0.1}],
	    fun(Res) -> {shape,{more,{regularplane,Res}}} end);
make_regular_plane([Nres, Size, Thickness]) ->
    Vs = regular_plane_verts(Nres, Size, +Thickness/2) ++
	 regular_plane_verts(Nres, Size, -Thickness/2),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Regular Plane",Fs,Vs}.

% ======= Lumpy Plane =======
make_lumpy_plane(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Create Lumpy Plane"),
	    [{?__(2,"Resolution"),30},
	     {?__(3,"Size"),2.0},
	     {?__(4,"Lumps"),2},
	     {?__(5,"Thickness"), 0.1}],
	    fun(Res) -> {shape,{more,{lumpyplane,Res}}} end);
make_lumpy_plane([Nres, Size, Lumps, Thickness]) ->
    Vs = lumpy_plane_verts(Nres, Size, Lumps, +Thickness/2) ++
	 lumpy_plane_verts(Nres, Size, Lumps, -Thickness/2),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Lumpy Plane",Fs,Vs}.

% ======= Wavy Plane =======
make_wavy_plane(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Make Wavy Plane"),
	    [{?__(2,"Resolution"),60},
	     {?__(3,"Size"),2.0},
	     {?__(4,"Waves"),4},
	     {?__(5,"Height"), 0.2},
	     {?__(6,"Thickness"), 0.1}],
	    fun(Res) -> {shape,{more,{wavyplane,Res}}} end);
make_wavy_plane([Nres, Size, Waves, Height, Thickness]) ->
    Vs = wavy_plane_verts(Nres, Size, Waves, Height, +Thickness/2) ++
	 wavy_plane_verts(Nres, Size, Waves, Height, -Thickness/2),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Wavy Plane",Fs,Vs}.

% ======= Sombrero Plane =======
make_sombrero_plane(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Make Sombrero Plane"),
	    [{?__(2,"Resolution"),60},
	     {?__(3,"Size"),2.0},
	     {?__(4,"Waves"),4},
	     {?__(5,"Falloff"),1},
	     {?__(6,"Height"), 1},
	     {?__(7,"Thickness"), 0.1}],
	    fun(Res) -> {shape,{more,{sombreroplane,Res}}} end);
make_sombrero_plane([Nres, Size, Waves, Falloff, Height, Thickness]) ->
    Vs = sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, +Thickness/2) ++
	 sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, -Thickness/2),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Sombrero Plane",Fs,Vs}.

regular_plane_verts(Nres, Size, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

lumpy_plane_verts(Nres, Size, Lumps, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(cos(Lumps*(I/(Nres-1.0)*2-1)*pi())*
	     cos(Lumps*(J/(Nres-1.0)*2-1)*pi())/(Lumps*3)))+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

wavy_plane_verts(Nres, Size, Waves, Height, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(1.0/Waves *
	  cos(2*pi()*sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			+ pow((Waves*(J/(Nres-1.0)*2-1)), 2))))*Height)+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(1.0/Waves *
	  cos(2*pi()*sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			 +pow((Waves*(J/(Nres-1.0)*2-1)), 2)))
	      * exp(-sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			 +pow((Waves*(J/(Nres-1.0)*2-1)), 2))*Falloff))*Height)+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

plane_faces(Nres, Nres) ->
    plane_faces_top(Nres, Nres) ++
    plane_faces_bot(Nres, Nres) ++
    plane_faces_left(Nres, Nres) ++
    plane_faces_right(Nres, Nres) ++
    plane_faces_front(Nres, Nres) ++
    plane_faces_back(Nres, Nres).

plane_faces_top(Ures, Vres) ->
    [[I*Vres+J,I*Vres+J+1,(I+1)*Vres+J+1,(I+1)*Vres+J]
      || I <- lists:seq(0, Vres-2), J <- lists:seq(0, Ures-2)].

plane_faces_bot(Ures, Vres) ->
    Offset = Ures*Vres,
    [[Offset + ((I+1)*Vres+J),
      Offset + ((I+1)*Vres+J+1),
      Offset + (I*Vres+J+1),
      Offset + (I*Vres+J)]
      || I <- lists:seq(0, Vres-2), J <- lists:seq(0, Ures-2)].

plane_faces_left(Ures, Vres) ->
    A = lists:seq(0, Vres-1),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_right(Ures, Vres) ->
    A = lists:seq(Ures*Vres-1, Ures*Vres-Vres, -1),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_front(Ures, Vres) ->
    A = lists:seq(Ures*Vres-Vres, 0, -Vres),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_back(Ures, Vres) ->
    A = lists:seq(Vres-1, Ures*Vres-1, Vres),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

