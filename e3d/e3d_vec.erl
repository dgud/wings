%%
%%  e3d_vec.erl --
%%
%%     Arithmetic on vectors and points (represented as three-tuples).
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d_vec).

-export([zero/0,is_zero/1,add/1,add/2,add_prod/3,sub/1,sub/2,lerp/3,
	 norm_sub/2,mul/2,divide/2,neg/1,dot/2,cross/2,
	 len/1,dist/2,dist_sqr/2,
	 norm/1,norm/3,normal/3,normal/1,average/1,average/2,average/4,
	 bounding_box/1,area/3,degrees/2,plane/3,plane_side/2,plane_dist/2]).

-include("e3d.hrl").

-compile(inline).
-compile({inline_size,24}).

-spec zero() -> e3d_vector().
    
zero() ->
    {0.0,0.0,0.0}.

-spec is_zero(e3d_vector()) -> boolean().
     
is_zero({0.0,0.0,0.0}) -> true;
is_zero(_) -> false.

-spec add(e3d_vector(), e3d_vector()) -> e3d_vector().

add({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12) ->
    {V10+V20,V11+V21,V12+V22}.

add_prod({V10,V11,V12}, {V20,V21,V22}, S) when is_float(S) ->
    {S*V20+V10,S*V21+V11,S*V22+V12}.

-spec add([e3d_vector()]) -> e3d_vector().

add([{V10,V11,V12}|T]) ->
    add(T, V10, V11, V12).

-spec sub(e3d_vector(), e3d_vector()) -> e3d_vector().

sub({V10,V11,V12}, {V20,V21,V22}) ->
    {V10-V20,V11-V21,V12-V22}.

-spec norm_sub(e3d_vector(), e3d_vector()) -> e3d_vector().

norm_sub({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12) ->
    Nx = V10-V20,
    Ny = V11-V21,
    Nz = V12-V22,
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz).

-spec sub([e3d_vector()]) -> e3d_vector().

sub([{V10,V11,V12}|T]) ->
    sub(V10, V11, V12, T).

-spec mul(e3d_vector(), S::float()) -> e3d_vector().

mul({V10,V11,V12}, S) when is_float(S) ->
    {V10*S,V11*S,V12*S}.

-spec divide(e3d_vector(), S::float()) -> e3d_vector().

divide({V10,V11,V12}, S) ->
    InvS = 1/S,
    {V10*InvS,V11*InvS,V12*InvS}.

-spec neg(e3d_vector()) -> e3d_vector().

neg({X,Y,Z}) -> {-X,-Y,-Z}.

-spec dot(e3d_vector(), e3d_vector()) -> float().

dot({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12) ->
    V10*V20 + V11*V21 + V12*V22.

-spec cross(e3d_vector(), e3d_vector()) -> e3d_vector().

cross({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22) ->
    {V11*V22-V12*V21,V12*V20-V10*V22,V10*V21-V11*V20}.

-spec len(e3d_vector()) -> float().

len({X,Y,Z}) when is_float(X), is_float(Y), is_float(Z) ->
    math:sqrt(X*X+Y*Y+Z*Z).

-spec lerp(e3d_vector(), e3d_vector(), float()) -> e3d_vector().

lerp({V10,V11,V12}, {V20,V21,V22}, T)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22) ->
    {V10+(V20-V10)*T, V11+(V21-V11)*T, V12+(V22-V12)*T}.

-spec dist(e3d_vector(), e3d_vector()) -> float().

dist({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12),
					is_float(V20), is_float(V21), is_float(V22) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    math:sqrt(X*X+Y*Y+Z*Z).

-spec dist_sqr(e3d_vector(), e3d_vector()) -> float().

dist_sqr({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    X*X+Y*Y+Z*Z.

-spec norm(e3d_vector()) -> e3d_vector().

norm({V1,V2,V3}) ->
    norm(V1, V2, V3).

-spec norm(X::float(), Y::float(), Z::float()) -> e3d_vector().

norm(V1, V2, V3) when is_float(V1), is_float(V2), is_float(V3) ->
    norm(V1*V1+V2*V2+V3*V3, V1, V2, V3).

-spec normal(e3d_vector(), e3d_vector(), e3d_vector()) -> e3d_vector().

normal({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
    D10 = V10-V20,
    D11 = V11-V21,
    D12 = V12-V22,
    D20 = V20-V30,
    D21 = V21-V31,
    D22 = V22-V32,
    N0 = D11*D22-D12*D21,
    N1 = D12*D20-D10*D22,
    N2 = D10*D21-D11*D20,
    D = math:sqrt(N0*N0+N1*N1+N2*N2),
    try {N0/D,N1/D,N2/D}
    catch
	error:badarith -> {0.0,0.0,0.0}
    end.

-spec area(e3d_vector(), e3d_vector(), e3d_vector()) -> float().

area({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
    D10 = V10-V20,
    D11 = V11-V21,
    D12 = V12-V22,
    D20 = V20-V30,
    D21 = V21-V31,
    D22 = V22-V32,
    N0 = D11*D22-D12*D21,
    N1 = D12*D20-D10*D22,
    N2 = D10*D21-D11*D20,
    math:sqrt(N0*N0+N1*N1+N2*N2)*0.5.
    
%% Calculate plane coefficients for a plane on which the triangle lies   
plane({X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}) ->
        {A,B,C} = e3d_vec:normal({X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}),
        {CX,CY,CZ} = e3d_vec:average([{X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}]),
        D = -A*CX-B*CY-C*CZ,                  
        {{A,B,C},D}.

%% Helper function used to sort points according to which side of a plane they are on.	
plane_side({X,Y,Z},{{A,B,C},D}) ->
    Temp=A*X+B*Y+C*Z+D,
    if  Temp < 0.0000 -> -1;  true -> 1 end.

%% Using Coeff to calculate signed distance from point to plane.
plane_dist({X,Y,Z},{{A,B,C},D}) ->
	(A*X+B*Y+C*Z+D)/math:sqrt(A*A+B*B+C*C).
        

%% normal([{X,Y,Z}]) ->
%%  Calculate the averaged normal for the polygon using Newell's method.

-spec normal([e3d_vector()]) -> e3d_vector().

normal([{Ax,Ay,Az},{Bx,By,Bz},{Cx,Cy,Cz}])
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    Sx = (Ay-By)*(Az+Bz) + (By-Cy)*(Bz+Cz) + (Cy-Ay)*(Cz+Az),
    Sy = (Az-Bz)*(Ax+Bx) + (Bz-Cz)*(Bx+Cx) + (Cz-Az)*(Cx+Ax),
    Sz = (Ax-Bx)*(Ay+By) + (Bx-Cx)*(By+Cy) + (Cx-Ax)*(Cy+Ay),
    SqrLen = Sx*Sx + Sy*Sy + Sz*Sz,
    norm(SqrLen, Sx, Sy, Sz);
normal([{Ax,Ay,Az},{Bx,By,Bz},{Cx,Cy,Cz},{Dx,Dy,Dz}])
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    %% The same result as the Newell normal (after normalization)
    %% can be calculated by taking the cross product of the vectors
    %% formed by the diagonals of the quad. (From Christer Ericson:
    %% "Real-Time Collision Detection", Chapter 12.)
    V10 = Dx-Bx, V11 = Dy-By, V12 = Dz-Bz,
    V20 = Ax-Cx, V21 = Ay-Cy, V22 = Az-Cz,
    Nx = V11*V22-V12*V21,
    Ny = V12*V20-V10*V22,
    Nz = V10*V21-V11*V20,
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz);
normal([{Ax,Ay,Az},{Bx,By,Bz}|[{Cx,Cy,Cz}|_]=T]=First)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    Sx = (Ay-By)*(Az+Bz) + (By-Cy)*(Bz+Cz),
    Sy = (Az-Bz)*(Ax+Bx) + (Bz-Cz)*(Bx+Cx),
    Sz = (Ax-Bx)*(Ay+By) + (Bx-Cx)*(By+Cy),
    normal_1(T, First, Sx, Sy, Sz).

normal_1([{Ax,Ay,Az}], [{Bx,By,Bz}|_], Sx, Sy, Sz)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Sx), is_float(Sy), is_float(Sz) ->
    Nx = Sx + (Ay-By)*(Az+Bz),
    Ny = Sy + (Az-Bz)*(Ax+Bx),
    Nz = Sz + (Ax-Bx)*(Ay+By),
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz);
normal_1([{Ax,Ay,Az}|[{Bx,By,Bz}|_]=T], First, Sx0, Sy0, Sz0)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Sx0), is_float(Sy0), is_float(Sz0) ->
    Sx = Sx0 + (Ay-By)*(Az+Bz),
    Sy = Sy0 + (Az-Bz)*(Ax+Bx),
    Sz = Sz0 + (Ax-Bx)*(Ay+By),
    normal_1(T, First, Sx, Sy, Sz).

%% average([{X,Y,Z}]) -> {Ax,Ay,Az}
%%  Average the given list of points.

-spec average([e3d_vector()]) -> e3d_vector().

average([{V10,V11,V12},B]) ->
    {V20,V21,V22} = B,
    V0 = if
	     V10 =:= V20 -> V10;
	     is_float(V10) -> 0.5*(V10+V20)
	 end,
    V1 = if
	     V11 =:= V21 -> V11;
	     is_float(V11) -> 0.5*(V11+V21)
	 end,
    if
	V12 =:= V22 -> {V0,V1,V12};
	is_float(V12) -> {V0,V1,0.5*(V12+V22)}
    end;
average([{V10,V11,V12}|T]=All) ->
    average(T, V10, V11, V12, length(All)).

-spec average(e3d_vector(), e3d_vector()) -> e3d_vector().

average({V10,V11,V12}, {V20,V21,V22}) ->
    V0 = if
	     V10 =:= V20 -> V10;
	     is_float(V10) -> 0.5*(V10+V20)
	 end,
    V1 = if
	     V11 =:= V21 -> V11;
	     is_float(V11) -> 0.5*(V11+V21)
	 end,
    if
	V12 =:= V22 -> {V0,V1,V12};
	is_float(V12) -> {V0,V1,0.5*(V12+V22)}
    end.

-spec average(e3d_vector(), e3d_vector(), e3d_vector(), e3d_vector()) -> e3d_vector().

average({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32}, {V40,V41,V42})
    when is_float(V10), is_float(V11), is_float(V12) ->
    L = 0.25,
    {L*(V10+V20+V30+V40),L*(V11+V21+V31+V41),L*(V12+V22+V32+V42)}.


%% Should be removed and calls should be changed to e3d_bv instead.
-spec bounding_box([e3d_vector()]) -> [e3d_vector()].
bounding_box(List) when is_list(List) ->
    tuple_to_list(e3d_bv:box(List)).

-spec degrees(e3d_vector(), e3d_vector()) -> float().
    
degrees(V0, V1) ->
    Dot = e3d_vec:dot(V0,V1),
    LenMul = e3d_vec:len(V0) * e3d_vec:len(V1),
    %%% protect against divide-by-zero
    RawCos = if (abs(LenMul) > 1.0E-30) -> Dot / LenMul;
               true -> 1.0
             end,
    %%% protect against invalid cosine values
    Cos = if
            (RawCos > +1.0) -> +1.0;
            (RawCos < -1.0) -> -1.0;
            true -> RawCos
          end,
    math:acos(Cos) * (180.0 / math:pi()).

%%%
%%% Internal functions.
%%% 

add([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32);
add([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10+V20, A1+V11+V21, A2+V12+V22);
add([{V10,V11,V12}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10, A1+V11, A2+V12);
add([], A0, A1, A2) -> {A0,A1,A2}.

sub(A0, A1, A2, [{V10,V11,V12}|T]) ->
    sub(A0-V10, A1-V11, A2-V12, T);
sub(A0, A1, A2, []) -> {A0,A1,A2}.

norm(SqrLen, _, _, _) when SqrLen < 1.0E-16 ->
    {0.0,0.0,0.0};
norm(SqrLen, V1, V2, V3) ->
    D = math:sqrt(SqrLen),
    try {V1/D,V2/D,V3/D}
    catch
	error:badarith -> {0.0,0.0,0.0}
    end.

average([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32, L);
average([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10+V20, A1+V11+V21, A2+V12+V22, L);
average([{V10,V11,V12}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10, A1+V11, A2+V12, L);
average([], A0, A1, A2, L0) ->
    L = 1.0/float(L0),
    {A0*L,A1*L,A2*L}.
