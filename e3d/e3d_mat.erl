%%
%%  e3d_mat.erl --
%%
%%     Operations on matrices.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson and Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d_mat).

-export([identity/0,is_identity/1,determinant/1,print/1,
	 compress/1,expand/1,
	 translate/1,translate/3,scale/1,scale/3,
	 rotate/2,rotate_from_euler_rad/1, rotate_from_euler_rad/3,
	 rotate_to_z/1,rotate_s_to_t/2,
	 project_to_plane/1,
	 transpose/1,invert/1,
	 add/2,mul/2,mul_point/2,mul_vector/2,eigenv3/1]).
-compile(inline).
-include("e3d.hrl").

-spec identity() -> e3d_compact_matrix().

-define(EPSILON, 1.0e-06).
    
identity() ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,
     Zero,One,Zero,
     Zero,Zero,One,
     Zero,Zero,Zero}.

-spec is_identity(e3d_matrix()) -> boolean().

is_identity(identity) -> true;
is_identity({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0}) -> true;
is_identity({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,
	     0.0,0.0,0.0,0.1}) -> true;
is_identity({_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}) -> false;
is_identity({_,_,_,_,_,_,_,_,_,_,_,_}) -> false.

-spec compress(e3d_matrix()) -> e3d_compact_matrix().

compress(identity=I) -> I;
compress({A,B,C,Z1,D,E,F,Z2,G,H,I,Z3,Tx,Ty,Tz,One}) ->
    case eps(Z1) andalso eps(Z2) andalso eps(Z3) andalso eps(One-1.0) of
	false -> exit(not_compressable);
	true -> ok
    end,
    {A,B,C,D,E,F,G,H,I,Tx,Ty,Tz};
compress(Mat) 
  when tuple_size(Mat) =:= 12 -> 
    Mat.

-spec expand(e3d_matrix()) -> e3d_matrix().
    
expand(identity) ->
    {1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0};
expand({_A,_B,_C,_,_D,_E,_F,_,_G,_H,_I,_,_Tx,_Ty,_Tz,_}=Mat) -> Mat;
expand({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}) ->
    {A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}.

-spec translate(e3d_vector()) -> e3d_compact_matrix().

translate({X,Y,Z}) -> translate(X, Y, Z).

-spec translate(X::float(), Y::float(), Z::float()) -> e3d_compact_matrix().

translate(Tx, Ty, Tz) ->
    Zero = 0.0,
    One = 1.0,
    {One,Zero,Zero,
     Zero,One,Zero,
     Zero,Zero,One,
     Tx,Ty,Tz}.

-spec scale({float(),float(),float()} | float()) -> e3d_compact_matrix().

scale({X,Y,Z}) -> scale(X, Y, Z);
scale(Sc) when is_float(Sc) -> scale(Sc, Sc, Sc).

scale(Sx, Sy, Sz) ->
    Zero = 0.0,
    {Sx,Zero,Zero,
     Zero,Sy,Zero,
     Zero,Zero,Sz,
     Zero,Zero,Zero}.

-spec rotate(Angle::number(), Vector::e3d_vector()) -> e3d_compact_matrix().

rotate(A0, {X,Y,Z}) when is_float(X), is_float(Y), is_float(Z) ->
    A = A0*(math:pi()/180),
    CosA = math:cos(A),
    SinA = math:sin(A),
    XSinA = X*SinA,
    YSinA = Y*SinA,
    ZSinA = Z*SinA,
    {C2,C3, C4,C6, C7,C8} =
	{-ZSinA,YSinA,
	 ZSinA,-XSinA,
	 -YSinA,XSinA},
    {U1,U2,U3, U5,U6, U9} =
	{X*X,X*Y,X*Z,
	     Y*Y,Y*Z,
	         Z*Z},
    U4 = U2,
    U7 = U3,
    U8 = U6,
    S = CosA,
    NegS = -S,
    {U1+S*(1.0-U1), U4+NegS*U4+C4, U7+NegS*U7+C7,
     U2+NegS*U2+C2, U5+S*(1.0-U5), U8+NegS*U8+C8,
     U3+NegS*U3+C3, U6+NegS*U6+C6, U9+S*(1.0-U9),
     0.0,0.0,0.0}.

%% rotate_from_euler_rad is a shortcut for
%% Rad2deg = 180/math:pi(),
%% Mx = e3d_mat:rotate(Rx * Rad2deg, {1.0, 0.0, 0.0}),
%% My = e3d_mat:rotate(Ry * Rad2deg, {0.0, 1.0, 0.0}),
%% Mz = e3d_mat:rotate(Rz * Rad2deg, {0.0, 0.0, 1.0}),
%% Rot = e3d_mat:mul(Mz, e3d_mat:mul(My,Mx)),
-spec rotate_from_euler_rad(Vector::e3d_vector()) -> e3d_compact_matrix().
rotate_from_euler_rad({X,Y,Z}) ->
    rotate_from_euler_rad(X,Y,Z).

-spec rotate_from_euler_rad(X::number(), Y::number(), Z::number()) -> 
				   e3d_compact_matrix().
rotate_from_euler_rad(Rx,Ry,Rz) ->
    Cz = math:cos(Rz), Sz = math:sin(Rz),
    Cy = math:cos(Ry), Sy = math:sin(Ry),
    Cx = math:cos(Rx), Sx = math:sin(Rx),
    Sxsy=Sx*Sy,  Cxsy=Cx*Sy,
    TZ = 0.0,
    {Cy*Cz,         Cy*Sz,	   -Sy,
     Sxsy*Cz-Cx*Sz, Sxsy*Sz+Cx*Cz, Sx*Cy,
     Cxsy*Cz+Sx*Sz, Cxsy*Sz-Sx*Cz, Cx*Cy,
     TZ,            TZ,            TZ}.


%% Project to plane perpendicular to vector Vec.

-spec project_to_plane(Vector::e3d_vector()) -> e3d_compact_matrix().

project_to_plane(Vec) ->
    %%       T
    %% P = QQ
    %% (Strang: Linear Algebra and its Applications, 3rd edition, p 170.)
    {Ux,Vx,_,
     Uy,Vy,_,
     Uz,Vz,_,
     _,_,_} = rotate_to_z(Vec),
    if
	is_float(Ux), is_float(Uy), is_float(Uz),
	is_float(Vx), is_float(Vy), is_float(Vz) ->
	    {Ux*Ux+Vx*Vx,Uy*Ux+Vy*Vx,Uz*Ux+Vz*Vx,
	     Ux*Uy+Vx*Vy,Uy*Uy+Vy*Vy,Uz*Uy+Vz*Vy,
	     Ux*Uz+Vx*Vz,Uy*Uz+Vy*Vz,Uz*Uz+Vz*Vz,
	     0.0,0.0,0.0}
    end.

-spec rotate_to_z(Vector::e3d_vector()) -> e3d_compact_matrix().

rotate_to_z(Vec) ->
    {Vx,Vy,Vz} = V =
	case e3d_vec:norm(Vec) of
	    {Wx,Wy,Wz}=W when abs(Wx) < abs(Wy), abs(Wx) < abs(Wz) ->
		e3d_vec:norm(0.0, Wz, -Wy);
	    {Wx,Wy,Wz}=W when abs(Wy) < abs(Wz) ->
		e3d_vec:norm(Wz, 0.0, -Wx);
	    {Wx,Wy,Wz}=W ->
		e3d_vec:norm(Wy, -Wx, 0.0)
	end,
    {Ux,Uy,Uz} = e3d_vec:cross(V, W),
    {Ux,Vx,Wx,
     Uy,Vy,Wy,
     Uz,Vz,Wz,
     0.0,0.0,0.0}.

-spec rotate_s_to_t(S::e3d_vector(), T::e3d_vector()) -> e3d_compact_matrix().

rotate_s_to_t(S, T) ->
    %% Tomas Moller/Eric Haines: Real-Time Rendering (ISBN 1-56881-101-2).
    %%  3.3. Quaternions; Rotating one vector to another
    case e3d_vec:dot(S, T) of
 	E when abs(E) > 0.999999 ->
 	    almost_parallel(S, T);
	E ->
	    V = e3d_vec:cross(S, T),
	    rotate_s_to_t_1(V, E)
    end.

-spec transpose(e3d_compact_matrix() | 'identity') -> e3d_compact_matrix();
	       (e3d_matrix()) -> e3d_matrix().

transpose(identity=I) -> I;
transpose({M1,M2,M3,M4,M5,M6,M7,M8,M9,0.0=Z,0.0,0.0}) ->
    {M1,M4,M7,
     M2,M5,M8,
     M3,M6,M9,
     Z,Z,Z};
transpose({A,B,C,WX,D,E,F,WY,G,H,I,WZ,Tx,Ty,Tz,WW}) ->
    { A, D, G,Tx,
      B, E, H,Ty,
      C, F, I,Tz,
     WX,WY,WZ,WW}.


-spec add(M::e3d_matrix(), N::e3d_matrix()) -> e3d_matrix().
add({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_tx,B_ty,B_tz},
      {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d), is_float(A_e),
       is_float(A_f), is_float(A_g), is_float(A_h), is_float(A_i), is_float(A_tx),
       is_float(A_ty), is_float(A_tz),
       is_float(B_a), is_float(B_b), is_float(B_c), is_float(B_d), is_float(B_e),
       is_float(B_f), is_float(B_g), is_float(B_h), is_float(B_i), is_float(B_tx),
       is_float(B_ty), is_float(B_tz) ->
    {A_a+B_a,   A_b+B_b, A_c+B_c,
     A_d+B_d,   A_e+B_e, A_f+B_f,
     A_g+B_g,   A_h+B_h, A_i+B_i,
     A_tx+B_tx, A_ty+B_ty, A_tz+B_tz};
add({B_a,B_b,B_c,B_w0,B_d,B_e,B_f,B_w1,B_g,B_h,B_i,B_w2,B_tx,B_ty,B_tz,B_w3},
    {A_a,A_b,A_c,A_w0,A_d,A_e,A_f,A_w1,A_g,A_h,A_i,A_w2,A_tx,A_ty,A_tz,A_w3})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d), is_float(A_e),
       is_float(A_f), is_float(A_g), is_float(A_h), is_float(A_i), is_float(A_tx),
       is_float(A_ty), is_float(A_tz),
       is_float(A_w0),is_float(A_w1),is_float(A_w2),is_float(A_w3),
       is_float(B_a), is_float(B_b), is_float(B_c), is_float(B_d), is_float(B_e),
       is_float(B_f), is_float(B_g), is_float(B_h), is_float(B_i), is_float(B_tx),
       is_float(B_ty), is_float(B_tz),
       is_float(B_w0),is_float(B_w1),is_float(B_w2),is_float(B_w3) ->
    {A_a+B_a,   A_b+B_b, A_c+B_c, A_w0+B_w0,
     A_d+B_d,   A_e+B_e, A_f+B_f, A_w1+B_w1,
     A_g+B_g,   A_h+B_h, A_i+B_i, A_w2+B_w2,
     A_tx+B_tx, A_ty+B_ty, A_tz+B_tz, A_w3+B_w3};

add(M1,M2) when tuple_size(M1) =:= 12; tuple_size(M2) =:= 12 ->
    add(e3d_mat:expand(M1), e3d_mat:expand(M2)).


-spec mul(M::e3d_matrix(), N::e3d_matrix()) ->
		 e3d_matrix();
	 (M::e3d_matrix(), number()) ->
		 e3d_matrix();
	 (M::e3d_matrix(), {float(),float(),float(),float()}) ->
		 {float(),float(),float(),float()}.
mul(M, identity) -> M;
mul(identity, M) when not is_number(M) -> M;
mul(identity,M2) -> mul(expand(identity), M2);
mul({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,B_tx,B_ty,B_tz},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when is_float(A_tx), is_float(A_ty), is_float(A_tz),
       is_float(B_tx), is_float(B_ty), is_float(B_tz) ->
    {A_a, A_b, A_c,
     A_d, A_e, A_f,
     A_g, A_h, A_i,
     A_tx + B_tx,
     A_ty + B_ty,
     A_tz + B_tz};
mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_tx,B_ty,B_tz},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_tx,A_ty,A_tz})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d), is_float(A_e),
       is_float(A_f), is_float(A_g), is_float(A_h), is_float(A_i), is_float(A_tx),
       is_float(A_ty), is_float(A_tz),
       is_float(B_a), is_float(B_b), is_float(B_c), is_float(B_d), is_float(B_e),
       is_float(B_f), is_float(B_g), is_float(B_h), is_float(B_i), is_float(B_tx),
       is_float(B_ty), is_float(B_tz) ->
    {A_a*B_a + A_b*B_d + A_c*B_g,
     A_a*B_b + A_b*B_e + A_c*B_h,
     A_a*B_c + A_b*B_f + A_c*B_i,
     A_d*B_a + A_e*B_d + A_f*B_g,
     A_d*B_b + A_e*B_e + A_f*B_h,
     A_d*B_c + A_e*B_f + A_f*B_i,
     A_g*B_a + A_h*B_d + A_i*B_g,
     A_g*B_b + A_h*B_e + A_i*B_h,
     A_g*B_c + A_h*B_f + A_i*B_i,
     A_tx*B_a + A_ty*B_d + A_tz*B_g + B_tx,
     A_tx*B_b + A_ty*B_e + A_tz*B_h + B_ty,
     A_tx*B_c + A_ty*B_f + A_tz*B_i + B_tz};
mul({B_a,B_b,B_c,B_d,B_e,B_f,B_g,B_h,B_i,B_j,B_k,B_l,B_tx,B_ty,B_tz,B_w},
    {A_a,A_b,A_c,A_d,A_e,A_f,A_g,A_h,A_i,A_j,A_k,A_l,A_tx,A_ty,A_tz,A_w})
  when is_float(A_a), is_float(A_b), is_float(A_c), is_float(A_d),
       is_float(A_e), is_float(A_f), is_float(A_g), is_float(A_h),
       is_float(A_i), is_float(A_j), is_float(A_k), is_float(A_l),
       is_float(A_tx),is_float(A_ty), is_float(A_tz), is_float(A_w) ->
    {A_a*B_a + A_b*B_e + A_c*B_i + A_d*B_tx,
     A_a*B_b + A_b*B_f + A_c*B_j + A_d*B_ty,
     A_a*B_c + A_b*B_g + A_c*B_k + A_d*B_tz,
     A_a*B_d + A_b*B_h + A_c*B_l + A_d*B_w,

     A_e*B_a + A_f*B_e + A_g*B_i + A_h*B_tx,
     A_e*B_b + A_f*B_f + A_g*B_j + A_h*B_ty,
     A_e*B_c + A_f*B_g + A_g*B_k + A_h*B_tz,
     A_e*B_d + A_f*B_h + A_g*B_l + A_h*B_w,

     A_i*B_a + A_j*B_e + A_k*B_i + A_l*B_tx,
     A_i*B_b + A_j*B_f + A_k*B_j + A_l*B_ty,
     A_i*B_c + A_j*B_g + A_k*B_k + A_l*B_tz,
     A_i*B_d + A_j*B_h + A_k*B_l + A_l*B_w,

     A_tx*B_a + A_ty*B_e + A_tz*B_i + A_w*B_tx,
     A_tx*B_b + A_ty*B_f + A_tz*B_j + A_w*B_ty,
     A_tx*B_c + A_ty*B_g + A_tz*B_k + A_w*B_tz,
     A_tx*B_d + A_ty*B_h + A_tz*B_l + A_w*B_w};
mul({A,B,C,Q0,D,E,F,Q1,G,H,I,Q2,Tx,Ty,Tz,Q3}, {X,Y,Z,W})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I),
       is_float(Tx), is_float(Ty), is_float(Tz),
       is_float(Q0), is_float(Q1), is_float(Q2), is_float(Q3),
       is_float(X), is_float(Y), is_float(Z) ->
    {X*A + Y*D + Z*G + W*Tx,
     X*B + Y*E + Z*H + W*Ty,
     X*C + Y*F + Z*I + W*Tz,
     X*Q0 + Y*Q1 + Z*Q2 + W*Q3};
mul({A,B,C,Q0,D,E,F,Q1,G,H,I,Q2,Tx,Ty,Tz,Q3}, W)
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I),
       is_float(Tx), is_float(Ty), is_float(Tz),
       is_float(Q0), is_float(Q1), is_float(Q2), is_float(Q3),
       is_number(W) ->
    {A*W,  B*W, C*W, Q0*W,
     D*W,  E*W, F*W, Q1*W,
     G*W,  H*W, I*W, Q2*W,
     Tx*W,Ty*W,Tz*W, Q3*W};
mul({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, W)
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I),
       is_float(Tx), is_float(Ty), is_float(Tz), is_number(W) ->
    {A*W,  B*W, C*W,
     D*W,  E*W, F*W,
     G*W,  H*W, I*W,
     Tx*W,Ty*W,Tz*W};
mul(M1,M2)
  when tuple_size(M1) =:= 12; tuple_size(M2) =:= 12 ->
    mul(expand(M1), expand(M2)).

-spec mul_point(Matrix::e3d_matrix(), Point::e3d_vector()) -> e3d_vector().
    
mul_point(identity, P) -> P;
mul_point({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz);
mul_point({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G + Tx,
	  X*B + Y*E + Z*H + Ty,
	  X*C + Y*F + Z*I + Tz);
mul_point({A,B,C,WX,D,E,F,WY,G,H,I,WZ,Tx,Ty,Tz,WW}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), 
       is_float(WX), is_float(WY), is_float(WZ), is_float(WW), 
       is_float(X), is_float(Y), is_float(Z) ->
    W = WX*X + WY*Y + WZ * Z + WW,
    case W =:= 1.0 of
	true -> 
	    share(X*A + Y*D + Z*G + Tx,
		  X*B + Y*E + Z*H + Ty,
		  X*C + Y*F + Z*I + Tz);
	false ->
	    share((X*A + Y*D + Z*G + Tx)/W,
		  (X*B + Y*E + Z*H + Ty)/W,
		  (X*C + Y*F + Z*I + Tz)/W)
    end.

-spec mul_vector(Matrix::e3d_matrix(), Vector::e3d_vector()) -> e3d_vector().

mul_vector(identity, Vec) -> Vec;
mul_vector({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G,
	  X*B + Y*E + Z*H,
	  X*C + Y*F + Z*I);
mul_vector({A,B,C,_,D,E,F,_,G,H,I,_,Tx,Ty,Tz,_}, {X,Y,Z})
  when is_float(A), is_float(B), is_float(C), is_float(D), is_float(E),
       is_float(F), is_float(G), is_float(H), is_float(I), 
       is_float(Tx), is_float(Ty), is_float(Tz), is_float(X), is_float(Y), is_float(Z) ->
    share(X*A + Y*D + Z*G,
	  X*B + Y*E + Z*H,
	  X*C + Y*F + Z*I).

%%--------------------------------------------------------------------
%% @doc  Calculates the determinant
%% @end
%%--------------------------------------------------------------------
-spec determinant(e3d_matrix()) -> float().
determinant(identity) -> 1.0;
determinant(Mat) 
  when tuple_size(Mat) =:= 12 ->
    determinant(e3d_mat:expand(Mat));
determinant({M0,M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15}) ->
    A0 = M0*M5 - M1*M4,    A1 = M0*M6 - M2*M4,
    A2 = M0*M7 - M3*M4,    A3 = M1*M6 - M2*M5,
    A4 = M1*M7 - M3*M5,    A5 = M2*M7 - M3*M6,
    
    B0 =  M8*M13 -  M9*M12,  B1 =  M8*M14 - M10*M12,
    B2 =  M8*M15 - M11*M12,  B3 =  M9*M14 - M10*M13,
    B4 =  M9*M15 - M11*M13,  B5 = M10*M15 - M11*M14,
    
    A0*B5 - A1*B4 + A2*B3 + A3*B2 - A4*B1 + A5*B0.

%%--------------------------------------------------------------------
%% @doc  Calculates the inverse matrix
%% @end
%%--------------------------------------------------------------------
-spec invert(e3d_matrix()) -> e3d_matrix().
invert(identity) -> identity;
invert(Mat) 
  when tuple_size(Mat) =:= 12 ->
    invert(e3d_mat:expand(Mat));
invert({M0, M1,  M2, M3,
	M4, M5,  M6, M7,
	M8, M9, M10,M11,
	M12,M13,M14,M15}) ->
    A0 = M0*M5 - M1*M4,    A1 = M0*M6 - M2*M4,
    A2 = M0*M7 - M3*M4,    A3 = M1*M6 - M2*M5,
    A4 = M1*M7 - M3*M5,    A5 = M2*M7 - M3*M6,

    B0 =  M8*M13 -  M9*M12,  B1 =  M8*M14 - M10*M12,
    B2 =  M8*M15 - M11*M12,  B3 =  M9*M14 - M10*M13,
    B4 =  M9*M15 - M11*M13,  B5 = M10*M15 - M11*M14,
    
    Det = A0*B5 - A1*B4 + A2*B3 + A3*B2 - A4*B1 + A5*B0,
    case abs(Det) > ?EPSILON of
	true ->
	    InvDet = 1.0/Det,
	    {(+  M5*B5 -  M6*B4 +  M7*B3) * InvDet,
	     (-  M1*B5 +  M2*B4 -  M3*B3) * InvDet,
	     (+ M13*A5 - M14*A4 + M15*A3) * InvDet,
	     (-  M9*A5 + M10*A4 - M11*A3) * InvDet,
	     (-  M4*B5 +  M6*B2 -  M7*B1) * InvDet,
	     (+  M0*B5 -  M2*B2 +  M3*B1) * InvDet,
	     (- M12*A5 + M14*A2 - M15*A1) * InvDet,
	     (+  M8*A5 - M10*A2 + M11*A1) * InvDet,
	     (+  M4*B4 -  M5*B2 +  M7*B0) * InvDet,
	     (-  M0*B4 +  M1*B2 -  M3*B0) * InvDet,
	     (+ M12*A4 - M13*A2 + M15*A0) * InvDet,
	     (-  M8*A4 +  M9*A2 - M11*A0) * InvDet,
	     (-  M4*B3 +  M5*B1 -  M6*B0) * InvDet,
	     (+  M0*B3 -  M1*B1 +  M2*B0) * InvDet,
	     (- M12*A3 + M13*A1 - M14*A0) * InvDet,
	     (+  M8*A3 -  M9*A1 + M10*A0) * InvDet
	    };
	false ->
	    exit(singular_matrix)
    end.

%%--------------------------------------------------------------------
%% @doc  Prints a matrix
%% @end
%%--------------------------------------------------------------------
-spec print(e3d_transform()) -> ok.

print(#e3d_transf{mat=Mat}) ->
    print_1(Mat);
print(identity) ->
    print_1(e3d_mat:identity());
print(Mat) when tuple_size(Mat) =:= 12; 
		tuple_size(Mat) =:= 16 ->
    print_1(Mat).

print_1({A,B,C,D,E,F,G,H,I,Tx,Ty,Tz}) ->
    print_1({A,B,C,0.0,D,E,F,0.0,G,H,I,0.0,Tx,Ty,Tz,1.0});
print_1(_Mat = {A,B,C,D,E,F,G,H,I,J,K,L,TX,TY,TZ,W}) ->
    io:format(" ~7.3f ~7.3f ~7.3f ~7.3f~n ~7.3f ~7.3f ~7.3f ~7.3f~n"
    	      " ~7.3f ~7.3f ~7.3f ~7.3f~n ~7.3f ~7.3f ~7.3f ~7.3f~n~n", 
    	      [A,E,I,TX,
    	       B,F,J,TY,
    	       C,G,K,TZ,
    	       D,H,L,W]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculates Eigenvalues and vectors
%%   This is converted from Dave Eberly's MAGIC library
%% Returns ordered by least EigenValue first
%% {Evals={V1,V2,V3},Evects={X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3}}

eigenv3(Mat0={A,B,C,D,E,F,G,H,I}) 
  when is_float(A), is_float(B), is_float(C), 
       is_float(D), is_float(E), is_float(F), 
       is_float(G), is_float(H), is_float(I) ->
    {Mat1,Diag,SubD} = eig_triDiag3(Mat0),
    {{Va1,Va2,Va3},Vecs} = eig_ql(0,0,Diag,SubD,Mat1),
    {X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3} = Vecs,
    if (Va1 =< Va2), (Va2 =< Va3) ->
	    {{Va1,Va2,Va3},{X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3}};
       (Va1 =< Va3), (Va3 =< Va2) ->
	    {{Va1,Va3,Va2},{X1,Y1,Z1,X3,Y3,Z3,X2,Y2,Z2}};       
       (Va2 =< Va1), (Va1 =< Va3) ->
	    {{Va2,Va1,Va3},{X2,Y2,Z2,X1,Y1,Z1,X3,Y3,Z3}};
       (Va2 =< Va3), (Va3 =< Va1) ->
	    {{Va2,Va3,Va1},{X2,Y2,Z2,X3,Y3,Z3,X1,Y1,Z1}};
       (Va3 =< Va1), (Va1 =< Va2) ->
	    {{Va3,Va1,Va2},{X3,Y3,Z3,X1,Y1,Z1,X2,Y2,Z2}};
       (Va3 =< Va2), (Va2 =< Va1) ->
	    {{Va3,Va2,Va1},{X3,Y3,Z3,X2,Y2,Z2,X1,Y1,Z1}}
    end.

%%%
%%% Local functions.
%%%

share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.


almost_parallel(S, T) ->
    %% Parallel case as in Moller/Hughes:
    %%  http://www.acm.org/jgt/papers/MollerHughes99
    Axis = closest_axis(S),
    U = e3d_vec:sub(Axis, S),
    V = e3d_vec:sub(Axis, T),

    C1 = 2.0 / e3d_vec:dot(U, U),
    C2 = 2.0 / e3d_vec:dot(V, V),
    C3 = C1 * C2 * e3d_vec:dot(U, V),
    C = {C1,C2,C3,U,V},

    {1.0+ael(C, 1, 1),ael(C, 2, 1),ael(C, 3, 1),
     ael(C, 1, 2),1.0+ael(C, 2, 2),ael(C, 3, 2),
     ael(C, 1, 3),ael(C, 2, 3),1.0+ael(C, 3, 3),
     0.0,0.0,0.0}.

ael({C1,C2,C3,U,V}, I, J) ->
    -C1 * element(I, U) * element(J, U) -
	C2 * element(I, V) * element(J, V) +
	C3 * element(I, V) * element(J, U).

closest_axis({X0,Y0,Z0}) ->
    X = abs(X0),
    Y = abs(Y0),
    Z = abs(Z0),
    if
	X < Y ->
	    if
		X < Z -> {1.0,0.0,0.0};
		true -> {0.0,0.0,1.0}
	    end;
	true ->
	    if
		Y < Z -> {0.0,1.0,0.0};
		true -> {0.0,0.0,1.0}
	    end
    end.
    
rotate_s_to_t_1({Vx,Vy,Vz}, E) when is_float(Vx), is_float(Vy), is_float(Vz) ->
    H = (1.0 - E)/(Vx*Vx+Vy*Vy+Vz*Vz),
    HVx = H*Vx,
    HVz = H*Vz,
    HVxy = HVx*Vy,
    HVxz = HVx*Vz,
    HVyz = HVz*Vy,
    {E+HVx*Vx,HVxy+Vz,HVxz-Vy,
     HVxy-Vz,E+H*Vy*Vy,HVyz+Vx,
     HVxz+Vy,HVyz-Vx,E+HVz*Vz,
     0.0,0.0,0.0}.

eig_triDiag3({A0,B0,C0,_,D0,E0,_,_,F0}) 
  when is_float(A0), is_float(B0), is_float(C0), 
       is_float(D0), is_float(E0), is_float(F0) ->
    Di0 = A0,
    if abs(C0) >= ?EPSILON ->
	    Ell = math:sqrt(B0*B0+C0*C0),
	    B = B0/Ell,
	    C = C0/Ell,
	    Q = 2*B*E0+C*(F0-D0),
	    Di1 = D0+C*Q,
	    Di2 = F0-C*Q,
	    Su0 = Ell,
	    Su1 = E0-B*Q,
	    Mat = {1.0, 0.0, 0.0,
		   0.0,   B,   C,
		   0.0,   C,  -B},
	    {Mat,{Di0,Di1,Di2},{Su0,Su1,0.0}};
       true ->
	    Mat = {1.0, 0.0, 0.0,
		   0.0, 1.0, 0.0,
		   0.0, 0.0, 1.0},
	    {Mat,{Di0,D0,F0},{B0,E0,0.0}}
    end.

eps(E) ->
    abs(E) < ?EPSILON.

-define(S(I),element(I+1,Subd0)).
-define(D(I),element(I+1,Diag0)).
-define(Set(I,Val,Tup),setelement(I+1,Tup,Val)).

-define(M_SIZE,3).
-define(MAX_ITER,32).

eig_ql(I0,I1,Diag0,Subd0,Mat0) when I0 < ?M_SIZE, I1 < ?MAX_ITER ->
    case eig_cont(I0,Diag0,Subd0) of
	I0 ->
	    eig_ql(I0+1,0,Diag0,Subd0,Mat0);
	I2 ->
	    FG0 = (?D(I0+1)-?D(I0))/(2.0*?S(I0)),
	    FR = math:sqrt(FG0*FG0+1.0),
	    FG1 = if FG0 < 0.0 ->
			  ?D(I2)-?D(I0)+?S(I0)/(FG0-FR);
		     true ->
			  ?D(I2)-?D(I0)+?S(I0)/(FG0+FR)
		  end,
	    {FP,FG,Diag1,Subd1,Mat} = 
		eig_ql2(I2-1,I0,1.0,1.0,0.0,FG1,Diag0,Subd0,Mat0),
	    Diag  = ?Set(I0,element(I0+1,Diag1)-FP,Diag1),
	    Subd2 = ?Set(I0,FG,Subd1),
	    Subd  = ?Set(I2,0.0,Subd2),
	    eig_ql(I0,I1+1,Diag,Subd,Mat)
    end;
eig_ql(I0,I1,Diag,Subd,Mat) when I1 >= ?MAX_ITER -> 
    io:format("Hmm I1>MAX_ITER,original algo break fails here~n",[]),
    eig_ql(I0+1,0,Diag,Subd,Mat);
eig_ql(_,_,D,_,M) -> 
    {D,M}.

eig_cont(I2,Diag0,Subd0) when I2 =< ?M_SIZE-2 ->
    Ftmp = abs(?D(I2))+abs(?D(I2+1)),
    if (abs(?S(I2))+Ftmp) == Ftmp ->
	    I2;
       true ->
	    eig_cont(I2+1,Diag0,Subd0)
    end;
eig_cont(I2,_,_) -> I2.

eig_ql2(I3,I0,Sin0,Cos0,FP0,FG0,Diag0,Subd0,Mat0) when I3 >= I0 ->
    FF = Sin0*?S(I3),
    FB = Cos0*?S(I3),
    {Si3p1,Sin1,Cos1} =
	if abs(FF) >= abs(FG0) -> 
		eig_up(FG0,FF,pos);
	   true -> 
		eig_up(FF,FG0,neg)
	end,
    FG1 = ?D(I3+1)-FP0,
    FR  = (?D(I3)-FG1)*Sin1+2.0*FB*Cos1,
    FP1 = Sin1*FR, 
    Di3p1 = FG1+FP1,
    Mat = eig_vec(0,I3,Sin1,Cos1,Mat0),
    eig_ql2(I3-1,I0,Sin1,Cos1,FP1,Cos1*FR-FB,
	    ?Set(I3+1,Di3p1,Diag0),?Set(I3+1,Si3p1,Subd0),Mat);
eig_ql2(_,_,_,_,FP,FG,Diag,Subd,Mat) ->
    {FP,FG,Diag,Subd,Mat}.

eig_up(FG,FF,Type) ->
    Cos = FG/FF,
    FR = math:sqrt(Cos*Cos+1.0),
    FSin = 1.0/FR,
    case Type of
	pos -> 
	    {FF*FR,FSin,Cos*FSin};
	neg ->
	    {FF*FR,Cos*FSin,FSin}
    end.

eig_vec(I4,I3,Sin,Cos,Mat0) when I4 < ?M_SIZE ->
    Idx43p1 = I4*?M_SIZE+I3+1+1,
    Idx43 = I4*?M_SIZE+I3+1,
    Mat43 = element(Idx43,Mat0),
    FF    = element(Idx43p1,Mat0),
    Mat1  = setelement(Idx43p1, Mat0, Sin*Mat43+Cos*FF),
    Mat2  = setelement(Idx43,   Mat1, Cos*Mat43-Sin*FF),
    eig_vec(I4+1,I3,Sin,Cos,Mat2);
eig_vec(_,_,_,_,Mat) -> Mat.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
