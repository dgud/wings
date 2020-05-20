%%% File    : sphere.erl
%%% Author  :  <dgud@LINA>
%%% Description : Creates a sphere of triangles
%%% Created : 19 Mar 2009 by  <dgud@LINA>

-module(sphere).
-export([tris/0, tris/1, norm/1, norm/3]).

-define(XPLUS, {1.0,0.0,0.0}).
-define(XMIN, {-1.0,0.0,0.0}).
-define(YPLUS, {0.0,1.0,0.0}).
-define(YMIN, {0.0,-1.0,0.0}).
-define(ZPLUS, {0.0,0.0,1.0}).
-define(ZMIN, {0.0,0.0,-1.0}).

-define(octahedron, 
	[{?ZPLUS, ?XPLUS, ?YPLUS}, 
	 {?XMIN,  ?ZPLUS, ?YPLUS}, 
	 {?ZPLUS, ?XMIN,  ?YMIN }, 
	 {?ZPLUS, ?YMIN,  ?XPLUS}, 
	 {?YPLUS, ?XPLUS, ?ZMIN }, 
	 {?XMIN,  ?YPLUS, ?ZMIN }, 
	 {?YMIN,  ?XMIN,  ?ZMIN }, 
	 {?XPLUS, ?YMIN,  ?ZMIN }]).

-define(F32, 32/float-native).

%% equal tris() -> tris([]).
tris() ->
    tris([]).

%% func tris(Options) -> {Size::integer(), Tris::term(), [Extra]}
%% Returns the number of triangles and the triangles in a list  
%% or in a binary if option binary is true.
%% Extra contents depends on Options.
%% Options: 
%%     subd    Subdivision level default 1
%%     binary  All output is binary default false
%%     ccw     Winding order counter clockwise default true
%%     scale   Scale output default 1,
%%     normals Add normals to the extra list default false
%%     Extra = [NormalsIfIncluded]

tris(Opts) when is_list(Opts) ->
    Subd   = proplists:get_value(subd, Opts, 1),
    Binary = proplists:get_value(binary, Opts, false),
    CCW    = proplists:get_value(ccw, Opts, true),
    Scale  = proplists:get_value(scale, Opts, 1),
    Normal = proplists:get_value(normals, Opts, false),
    %% Do the work
    Tris   = subd_tris(1, Subd, ?octahedron),
    case Binary of
	true ->
	    BinTris = list_to_bin(Tris, CCW, Scale),
	    Ns = if not Normal -> [];
		    Scale =:= 1 -> [BinTris];
		    true -> [list_to_bin(Tris, CCW, 1)]
		 end,
	    {size(BinTris) div (9*4), BinTris, Ns};
	false ->
	    Scaled = convert_list(Tris, CCW, Scale),
	    Ns = if not Normal -> [];
		    Scale =:= 1 -> Scaled;
		    true -> convert_list(Tris, CCW, 1)
		 end,
	    {length(Tris), Scaled, Ns}
    end.

subd_tris(Level, MaxLevel, Sphere0) when Level < MaxLevel ->
    Sphere = subd_tris(Sphere0, []),
    subd_tris(Level+1, MaxLevel, Sphere);
subd_tris(_,_, Sphere) -> Sphere.

%%	  2             create a, b, c in the middle
%%	 /\		Normalize a, b, c
%%	/  \
%%    c/____\ b		Construct new triangles
%%    /\    /\		    [0,b,a]
%%   /	\  /  \		    [a,b,c]
%%  /____\/____\	    [a,c,2]
%% 0	  a	1	    [b,1,c]
%%

subd_tris([{V0,V1,V2}|Rest], Acc) ->
    A = norm(midpoint(V0,V1)),
    B = norm(midpoint(V1,V2)),
    C = norm(midpoint(V0,V2)),
    T1 = {V0,A,C},
    T2 = {A,B,C},
    T3 = {A,V1,B},
    T4 = {C,B,V2},
    subd_tris(Rest, [T1,T2,T3,T4|Acc]);
subd_tris([],Acc) ->
    Acc.

midpoint({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    {(X1+X2)*0.5, (Y1+Y2)*0.5, (Z1+Z2)*0.5}.

norm({X,Y,Z}) -> 
    norm(X,Y,Z).
norm(X,Y,Z) ->
    Mag = X*X+Y*Y+Z*Z,
    if 
	Mag =:= 0.0 -> {X,Y,Z};
	true -> 
	    Div = 1/math:sqrt(Mag),
	    {X*Div, Y*Div, Z*Div}
    end.
	     
%%%%%%%%%%%%%% Converters %%%%%%%%%%%
list_to_bin(Tris, true, 1) ->
    << <<(X1):?F32,(Y1):?F32,(Z1):?F32,
	(X2):?F32,(Y2):?F32,(Z2):?F32,
	(X3):?F32,(Y3):?F32,(Z3):?F32>> 
     || {{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}} <- Tris >>;
list_to_bin(Tris, false, 1) ->
    << <<(X1):?F32,(Y1):?F32,(Z1):?F32,
	(X2):?F32,(Y2):?F32,(Z2):?F32,
	(X3):?F32,(Y3):?F32,(Z3):?F32>> 
     || {{X1,Y1,Z1},{X3,Y3,Z3},{X2,Y2,Z2}} <- Tris >>;
list_to_bin(Tris, true, Size) ->
    << <<(X1*Size):?F32,(Y1*Size):?F32,(Z1*Size):?F32,
	(X2*Size):?F32,(Y2*Size):?F32,(Z2*Size):?F32,
	(X3*Size):?F32,(Y3*Size):?F32,(Z3*Size):?F32>> 
     || {{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}} <- Tris >>;
list_to_bin(Tris, false, Size) ->
    << <<(X1*Size):?F32,(Y1*Size):?F32,(Z1*Size):?F32,
	(X2*Size):?F32,(Y2*Size):?F32,(Z2*Size):?F32,
	(X3*Size):?F32,(Y3*Size):?F32,(Z3*Size):?F32>> 
     || {{X1,Y1,Z1},{X3,Y3,Z3},{X2,Y2,Z2}} <- Tris >>.

convert_list(List, true, 1) ->
    List;
convert_list(List, false, 1) ->
    [{V1,V2,V3} || {V1,V3,V2} <- List];
convert_list(List, true, Size) ->
    [{{(X1*Size),(Y1*Size),(Z1*Size)},
      {(X2*Size),(Y2*Size),(Z2*Size)},
      {(X3*Size),(Y3*Size),(Z3*Size)}}
     || {{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}} <- List];
convert_list(List, false, Size) ->
    [{{(X1*Size),(Y1*Size),(Z1*Size)},
      {(X2*Size),(Y2*Size),(Z2*Size)},
      {(X3*Size),(Y3*Size),(Z3*Size)}}
     || {{X1,Y1,Z1},{X3,Y3,Z3},{X2,Y2,Z2}} <- List].
