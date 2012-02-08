%%
%%  e3d_q.erl --
%%
%%     Operations on unit quaternions.
%%
%%  Copyright (c) 2003-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_q.erl,v 1.6 2006/02/01 16:20:11 dgud Exp $
%%

%% Quaternions are represented as a {{Qx,Qy,Qz},Qw}
%% to differ them from Vectors.
%% The following is implemented from
%% "The Matrix and Quaternions FAQ".

-module(e3d_q).

-export([identity/0,inverse/1,norm/1,mul/1,mul/2,
	 add/2, scale/2, slerp/3,
	 magnitude/1, conjugate/1,
	 to_rotation_matrix/1, from_rotation_matrix/1,
	 to_angle_axis/1, from_angle_axis/2, from_angle_axis_rad/2,
	 from_euler_rad/1, from_euler_rad/3, to_euler_rad/1,
	 from_compact/1, to_compact/1,
	 rotate_s_to_t/2,
	 vec_rotate/2]).

-compile(inline).

%% Multiplicative identity
identity() ->
    {{0.0,0.0,0.0},1.0}.

magnitude({{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    MagSqr = Qx*Qx+Qy*Qy+Qz*Qz+Qw*Qw,
    if MagSqr > 0.99999, MagSqr < 1.00001 ->
	    1.0;
       true ->
	    math:sqrt(MagSqr)
    end.
conjugate({{Qx,Qy,Qz},Qw})
  when is_float(Qx), is_float(Qy), is_float(Qz), is_float(Qw) ->
    {{-Qx,-Qy,-Qz},Qw}.

inverse(Q) ->
    conjugate(Q).

norm(Q = {{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    M = magnitude(Q),
    if M =:= 1.0 -> Q;
       M < 0.00001 -> {{0.0,0.0,0.0},0.0};
       true -> 
	    IM = 1/M,
	    {{Qx*IM,Qy*IM,Qz*IM},Qw*IM}
    end.

add({{X1,Y1,Z1},W1}, {{X2,Y2,Z2},W2})
  when is_float(X1),is_float(Y1),is_float(Z1),is_float(W1),
       is_float(X2),is_float(Y2),is_float(Z2),is_float(W2) ->
    {{X1+X2,Y1+Y2,Z1+Z2},W1+W2}.

scale({{Qx,Qy,Qz},Qw}, S)
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw), is_float(S) ->
    {{Qx*S,Qy*S,Qz*S},Qw*S}.

slerp(Q0={{X0,Y0,Z0},W0}, {{X1,Y1,Z1},W1}, T)
  when is_float(X1),is_float(Y1),is_float(Z1),is_float(W1),
       is_float(X0),is_float(Y0),is_float(Z0),is_float(W0),
       is_float(T) ->
    HalfCosTheta = X0*X1+Y0*Y1+Z0*Z1+W0*W1,
    if abs(HalfCosTheta) >= 0.9999 ->
	    Q0;
       true ->
	    HalfTheta = math:acos(HalfCosTheta),
	    HalfSinTheta = math:sin(HalfTheta),
	    if abs(HalfSinTheta) < 0.00001 ->
		    R0 = 1.0 - T,
		    R1 = T,
		    {{X0*R0+X1*R1, Y0*R0+Y1*R1, Z0*R0+Z1*R1},W0*R0+W1*R1};
	       true ->
		    R0 = math:sin((1.0 - T)*HalfTheta) / HalfSinTheta,
		    R1 = math:sin(T*HalfTheta) / HalfSinTheta,
		    {{X0*R0+X1*R1, Y0*R0+Y1*R1, Z0*R0+Z1*R1},W0*R0+W1*R1}
	    end
    end.

mul([H|R]) ->
    mmult(R, H).

mmult([H|R], A) ->
    mmult(R, mul(A,H));
mmult([], A) -> A.

mul({{X1,Y1,Z1},W1}, {{X2,Y2,Z2},W2})
  when is_float(X1),is_float(Y1),is_float(Z1),is_float(W1),
       is_float(X2),is_float(Y2),is_float(Z2),is_float(W2) ->
    {{W1*X2+X1*W2+Y1*Z2-Z1*Y2,
      W1*Y2+Y1*W2+Z1*X2-X1*Z2,
      W1*Z2+Z1*W2+X1*Y2-Y1*X2},
     W1*W2-X1*X2-Y1*Y2-Z1*Z2}.

to_rotation_matrix({{Qx,Qy,Qz},Qw})
  when is_float(Qx),is_float(Qy),is_float(Qz),is_float(Qw) ->
    Zero = 0.0,    One  = 1.0,    Two = 2.0,
    XX = Qx*Qx,    XY = Qx*Qy,    XZ = Qx*Qz,    XW = Qx*Qw,
    YY = Qy*Qy,    YZ = Qy*Qz,    YW = Qy*Qw,
    ZZ = Qz*Qz,    ZW = Qz*Qw,

    M0=One-Two*(YY+ZZ), M1=Two*(XY-ZW),     M2=Two*(XZ+YW),
    M4=Two*(XY+ZW),     M5=One-Two*(XX+ZZ), M6=Two*(YZ-XW),
    M8=Two*(XZ-YW),     M9=Two*(YZ+XW),     M10=One-Two*(XX+YY),

    M3=M7=M11=Zero,
    {M0, M4, M8,
     M1, M5, M9,
     M2, M6, M10,
     M3, M7, M11}.

from_rotation_matrix({M0, M4, M8,
		      M1, M5, M9,
		      M2, M6, M10,
		      M3, M7, M11})
  when is_float(M0),is_float(M1),is_float(M2),is_float(M3),
       is_float(M4),is_float(M5),is_float(M6),is_float(M7),
       is_float(M8),is_float(M9),is_float(M10),is_float(M11) ->
    One = 1.0,  Two = 2.0, Eps = 0.000000001,
    T = One + M0 + M5 + M10,
    if T > Eps ->
	    S = math:sqrt(T) * Two,
	    {{(M9-M6)/S, (M2-M8)/S,(M4-M1)/S},0.25*S};
       (M0>M5) and (M0>M10) ->
	    S = math:sqrt(One+M0-M5-M10) * Two,
	    {{0.25*S,(M4+M1)/S,(M2+M8)/S},(M9-M6)/S};
       (M5>M10) ->
	    S = math:sqrt(One+M5-M0-M10),
	    {{(M4+M1)/S,0.25*S,(M9+M6)/S},(M2-M8)/S};
       true ->
	    S = math:sqrt(One+M10-M0-M5),
	    {{(M2+M8)/S,(M9+M6)/S,0.25*S},(M4-M1)/S}
    end;
from_rotation_matrix(M) when size(M) =:= 16 ->
    from_rotation_matrix(e3d_mat:compress(M)).

%% From/to euler 
%% From http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToEuler/index.htm
%% Note: We rotate X first then Y and last Z as in: 
%% RX = e3d_q:from_angle_axis_rad(X, ?X),
%% RY = e3d_q:from_angle_axis_rad(Y, ?Y),
%% RZ = e3d_q:from_angle_axis_rad(Z, ?Z),
%% e3d_q:mul(RZ, e3d_q:mul(RY, RX)).
%% 
from_euler_rad({X, Y, Z}) ->
    from_euler_rad(X, Y, Z).

from_euler_rad(RX, RY, RZ) ->
    RX2 = RX*0.5, RY2 = RY*0.5, RZ2 = RZ*0.5,
    CX = math:cos(RX2),
    SX = math:sin(RX2),
    CY = math:cos(RY2),
    SY = math:sin(RY2),
    CZ = math:cos(RZ2),
    SZ = math:sin(RZ2),
    CXCY = CX*CY, CXSY = CX*SY,
    SXSY = SX*SY, SXCY = SX*CY,
    W = CXCY*CZ + SXSY*SZ,
    X = SXCY*CZ - CXSY*SZ,
    Y = CXSY*CZ + SXCY*SZ,
    Z = CXCY*SZ - SXSY*CZ,
    {{X,Y,Z},W}.

to_euler_rad({{P1,P2,P3}, P0}) ->
    P1P1 = P1*P1, P2P2 = P2*P2, P3P3 = P3*P3, P0P0 = P0*P0,
    Unit = P1P1+P2P2+P3P3+P0P0,
    Test = P0*P2 - P1*P3,
    Limit = 0.4999*Unit,
    if Test > Limit ->  %% singularity at north pole
	    {math:atan2(P1,P0), math:pi()/2, 0.0};
       Test < -Limit -> %% singularity at south pole
	    {math:atan2(P1,P0), -math:pi()/2, 0.0};
       true ->	    
	    RX = math:atan2(2.0*(P0*P1+P2*P3), 1.0-2.0*(P1P1+P2P2)),
	    RY = math:asin(2.0*Test),
	    RZ = math:atan2(2.0*(P0*P3+P1*P2), 1.0-2.0*(P2P2+P3P3)),
	    {RX,RY,RZ}
    end.

%% The Axis must be a unit-length vector.
from_angle_axis(Angle, Axis) ->
    HalfAngle = Angle*(math:pi()/180.0/2.0),
    Sin = math:sin(HalfAngle),
    Cos = math:cos(HalfAngle),
    {X,Y,Z} = Axis,
    {{X*Sin,Y*Sin,Z*Sin},Cos}.

from_angle_axis_rad(Angle, Axis) ->
    HalfAngle = Angle*0.5,
    Sin = math:sin(HalfAngle),
    Cos = math:cos(HalfAngle),
    {X,Y,Z} = Axis,
    {{X*Sin,Y*Sin,Z*Sin},Cos}.

to_angle_axis(Q) ->
    {{Qx,Qy,Qz},Qw} = norm(Q),
    Cos = Qw,
    Angle = math:acos(Cos) * (2*180/math:pi()),
    Sin0  = math:sqrt(1.0 - Cos*Cos),
    Sin   = if
		abs(Sin0) < 0.000005 -> 1.0;
		true -> Sin0
	    end,
    {Angle,{Qx/Sin,Qy/Sin,Qz/Sin}}.

%% The Axis must be a unit-length vector.
from_compact(R={Rx,Ry,Rz}) ->
    T = 1.0 - (Rx * Rx) - (Ry * Ry) - (Rz * Rz),
    case T < 0.0 of
	true  -> {R, 0.0};
	false -> {R, -math:sqrt(T)}
    end.

to_compact(Q) ->
    case norm(Q) of
	{Vec, W} when W < 0.0 ->  Vec;
	{{Rx,Ry,Rz}, _} -> {-Rx,-Ry,-Rz}
    end.

%% vec_rotate(Vec, Q)
%%  Rotate a vector or point using quaternion Q.
vec_rotate({X2,Y2,Z2}, {{X1,Y1,Z1},W1})
  when is_float(X1), is_float(Y1), is_float(Z1), is_float(W1),
       is_float(X2), is_float(Y2), is_float(Z2)  ->
    %% Calculate Q*{V,0}*^Q.
    X3 = W1*X2+Y1*Z2-Z1*Y2,
    Y3 = W1*Y2+Z1*X2-X1*Z2,
    Z3 = W1*Z2+X1*Y2-Y1*X2,
    W3 = -X1*X2-Y1*Y2-Z1*Z2,
    {-W3*X1+X3*W1-Y3*Z1+Z3*Y1,
     -W3*Y1+Y3*W1-Z3*X1+X3*Z1,
     -W3*Z1+Z3*W1-X3*Y1+Y3*X1}.

%% rotate_s_to_t(S,T) -> Q   Both S and T should be normalized before usage
%%
%% Code converted from David Eberly's Geometric Tools website 
%%    www.geometrictools.com

rotate_s_to_t(V1={X,Y,Z},V2) 
  when is_float(X), is_float(Y), is_float(Z) ->
    Bisector = e3d_vec:norm(e3d_vec:add(V1,V2)),
    CosHalfAngle = e3d_vec:dot(V1,Bisector),
    if CosHalfAngle =/= 0.0 ->
	    {e3d_vec:cross(V1,Bisector), CosHalfAngle};
       abs(X) >= abs(Y) ->
	    %% V1.x or V1.z is the largest magnitude component
	    InvLength = 1.0/math:sqrt(X*X + Z*Z),
	    {{-Z*InvLength,0.0,X*InvLength},CosHalfAngle};
       true ->
	    %% V1.y or V1.z is the largest magnitude component
	    InvLength = 1.0/math:sqrt(Y*Y + Z*Z),
	    {{0.0,Z*InvLength,-Y*InvLength},CosHalfAngle}
    end.
