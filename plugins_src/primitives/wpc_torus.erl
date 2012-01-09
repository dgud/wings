%%
%%  wpc_torus.erl --
%%
%%     Torus Plugin plus two other types of tori borrowed from LightFlow.
%%
%%  Copyright (c) 2002-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% The U resolution is the number of faces that will be generated
%% along the path of the major radius; the V resolution, along the path
%% of the minor radius.
%%
%% To generate square faces, rather than long and thin rectangles,
%% maintain the following ratio: URes/VRes = MajorRad/MinorRad
%% Or you can enter a zero for either U or V and it will automatically adjust.

-module(wpc_torus).
-export([init/0,menu/2,command/2]).

-import(math, [cos/1,sin/1,pi/0]).
-include("wings_intl.hrl").

init() -> true.

menu({shape}, []) ->
    torus_menu();
menu({shape}, Menu) ->
    Menu ++ [separator|torus_menu()];
menu(_, Menu) -> Menu.

torus_menu() ->
    [{?__(1,"UV Torus")     ,uvtorus,[option]},
     {?__(2,"Lumpy Torus")  ,lutorus,[option]},
     {?__(3,"Spiral Torus") ,sptorus,[option]}].

command({shape,{uvtorus,Ask}}, _St) -> make_uv_torus(Ask);
command({shape,{lutorus,Ask}}, _St) -> make_lu_torus(Ask);
command({shape,{sptorus,Ask}}, _St) -> make_sp_torus(Ask);
command(_, _) -> next.

%%% The rest are local functions.

% ======= Regular Torus =======
make_uv_torus(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"UV Torus Options"),
	    [{?__(2,"U Resolution"),80,
	    	[{range,{0,infinity}},{info,?__(6,"Number of segments along major circumference (U, minimum is 3)")}]},
	     {?__(3,"V Resolution"),16,
	     	[{range,{0,infinity}},{info,?__(7,"Number of segments along minor circumference (V, minimum is 3)")}]},
	     {?__(4,"Major Radius"),1.0},
	     {?__(5,"Minor Radius"),0.2}],
	    fun(Res) ->
    		{shape,{uvtorus,Res}}
	    end);
make_uv_torus([URES, VRES, MajR, MinR]) ->
    case {URES>0, VRES>0} of
	{true, true} ->
	    Ures = URES,
	    Vres = VRES;
	{false, true} ->
	    Ures = trunc(VRES*(MajR/MinR)),
	    Vres = VRES;
	{true, false} ->
	    Vres = trunc(URES/(MajR/MinR)),
	    Ures = URES;
	{false, false} ->
	    Vres = 6,
	    Ures = trunc(Vres*(MajR/MinR))
    end,
	Ures0=min_uv_torus_res(Ures),
	Vres0=min_uv_torus_res(Vres),
    Vs = make_verts(Ures0, Vres0, MajR, MinR, none, none, 1),
    Fs = make_faces(Ures0, Vres0),
    {new_shape,"UV Torus",Fs,Vs}.
min_uv_torus_res(Res) when Res =< 3 -> 3;
min_uv_torus_res(Res) -> Res.

% ======= Lumpy Torus =======
make_lu_torus(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Lumpy Torus Options"),
	    [{?__(2,"U Resolution"),125,
	    	[{range,{0,infinity}},{info,?__(8,"Number of segments along major circumference (U, minimum is 3)")}]},
	     {?__(3,"V Resolution"),25,
	     	[{range,{0,infinity}},{info,?__(9,"Number of segments along minor circumference (V, minimum is 3)")}]},
	     {?__(4,"Major Radius"),1.0},
	     {?__(5,"Minor Radius"),0.2},
	     {?__(6,"Lumps"),8},
	     {?__(7,"Lump Amplitude"),0.5}],
	    fun(Res) -> 
			{shape,{lutorus,Res}}
		end);
make_lu_torus([Ures0, Vres0, MajR, MinR, Loops, LoopRad]) ->
	Ures=min_uv_torus_res(Ures0),
	Vres=min_uv_torus_res(Vres0),
    Vs = make_verts(Ures, Vres, MajR, MinR, Loops, LoopRad, 2),
    Fs = make_faces(Ures, Vres),
    {new_shape,"Lumpy Torus",Fs,Vs}.

% ======= Spiral Torus =======
make_sp_torus(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Spiral Torus Options"),
	    [{?__(2,"U Resolution"),200,
	    	[{range,{0,infinity}},{info,?__(8,"Number of segments along major circumference (U, minimum is 3)")}]},
	     {?__(3,"V Resolution"),20,
	     	[{range,{0,infinity}},{info,?__(9,"Number of segments along minor circumference (V, minimum is 3)")}]},
	     {?__(4,"Major Radius"),1.0},
	     {?__(5,"Minor Radius"),0.2},
	     {?__(6,"Loops"),8},
	     {?__(7,"Loop Radius "),0.2}],
	    fun(Res) -> 
			{shape,{sptorus,Res}}
		end);
make_sp_torus([Ures0, Vres0, MajR, MinR, Loops, LoopRad]) ->
	Ures=min_uv_torus_res(Ures0),
	Vres=min_uv_torus_res(Vres0),
    Vs = make_verts(Ures, Vres, MajR, MinR, Loops, LoopRad, 3),
    Fs = make_faces(Ures, Vres),
    {new_shape,"Spiral Torus",Fs,Vs}.

make_verts(Ures, Vres, MajR, MinR, Loops, LoopRad, Type) ->
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Du = 2*pi()/Ures,
    Dv = 2*pi()/Vres,
    Make_Vert = case Type of
	1->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		X = (MajR + MinR*A) * C,
		Y =	  -(MinR*B),
		Z = (MajR + MinR*A) * D,
		{X,Y,Z}
	    end;
	2 ->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		N = 1+cos(I*Du*Loops)*LoopRad,
		X = (MajR + MinR*A*N) * C,
		Y =	  -(MinR*B*N),
		Z = (MajR + MinR*A*N) * D,
		{X,Y,Z}
	    end;
	3 ->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		N = sin(I*Du*Loops)*LoopRad,
		O = cos(I*Du*Loops)*LoopRad,
		X = (MajR + MinR*A + N) * C,
		Y =	  -(MinR*B + O),
		Z = (MajR + MinR*A + N) * D,
		{X,Y,Z}
	    end
	end,
    [Make_Vert(I,J) || I <- Us, J <- Vs].

make_faces(Ures, Vres) ->
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Make_Face = fun(I,J) ->
	Idx1 = (J+1) rem Vres + I*Vres,
	Idx2 = (J+1) rem Vres + ((I+1) rem Ures)*Vres,
	Idx3 = J + ((I+1) rem Ures) * Vres,
	Idx4 = J + I*Vres,
	[Idx4,Idx3,Idx2,Idx1]
    end,
    [Make_Face(I,J) || I <- Us, J <- Vs].
