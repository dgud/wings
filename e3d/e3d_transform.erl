%%
%%  e3d_transf.erl --
%%
%%     More transformation matrix utilities
%%
%%  Copyright (c) 2010-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%%
%%% @doc   More transformation matrix utilities
%%%
%%%         All of the matrix operations operates in the following order
%%%          I = identity(),
%%%          T = translate(I, Vec),
%%%          R = rotate(T, Vec),
%%%          Matrix = Rotate(Translate())
%%%
%%%         Also using opengl right handed coordinate system
%%% @end
-module(e3d_transform).

-export([%% Initilizes matrices
	 identity/0, init/1,
	 lookat/3, ortho/2, ortho/6, perspective/3, perspective/4, pick/5,
	 %% Get the actual matrices
	 matrix/1, inv_matrix/1,
	 %% Transform the matrices
	 inverse/1, translate/2, rotate/2, rotate/3, scale/2, mul/1, mul/2
	]).

-type matrix() :: e3d_mat:matrix().
-type vector() :: e3d_vec:vector().
-type point() :: e3d_vec:point().

-include("e3d.hrl").

%%%-------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc  Returns the identity transform
%% @end
%%--------------------------------------------------------------------
-spec identity() -> e3d_transform().
identity() ->
    #e3d_transf{}.

%%--------------------------------------------------------------------
%% @doc  Initilizes transform from matrix mat
%% @end
%%--------------------------------------------------------------------
-spec init(matrix()) -> e3d_transform().
init(Mat) when tuple_size(Mat) =:= 12 ->
    init(e3d_mat:expand(Mat));
init(Mat) ->
    #e3d_transf{mat=Mat, inv=e3d_mat:invert(Mat)}.

%%--------------------------------------------------------------------
%% @doc  Returns the matrix
%% @end
%%--------------------------------------------------------------------
-spec matrix(e3d_transform()) -> matrix().
matrix(#e3d_transf{mat=M}) -> e3d_mat:expand(M).

%%--------------------------------------------------------------------
%% @doc  Returns the inverse matrix
%% @end
%%--------------------------------------------------------------------
-spec inv_matrix(e3d_transform()) -> matrix().
inv_matrix(#e3d_transf{inv=I}) -> e3d_mat:expand(I).

%%%-------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @doc  Inverses the transform
%% @end
%%--------------------------------------------------------------------
-spec inverse(e3d_transform()) -> e3d_transform().
inverse(#e3d_transf{mat=M, inv=I}) ->
    #e3d_transf{mat=I, inv=M}.

%%--------------------------------------------------------------------
%% @doc  Translates the matrix with vector
%% @end
%%--------------------------------------------------------------------
-spec translate(e3d_transform(), vector()) -> e3d_transform().
translate(#e3d_transf{mat=M,inv=I}, {Dx,Dy,Dz}) ->
    #e3d_transf{mat = e3d_mat:mul(M, e3d_mat:translate(Dx,Dy,Dz)),
		inv = e3d_mat:mul(e3d_mat:translate(-Dx,-Dy,-Dz), I)}.

%%--------------------------------------------------------------------
%% @doc  Rotates the matrix with rotation matrix
%% @end
%%--------------------------------------------------------------------
-spec rotate(e3d_transform(), matrix()) -> e3d_transform().
rotate(#e3d_transf{mat=M,inv=I}, Rot)
  when tuple_size(Rot) =:= 12; tuple_size(Rot) =:= 16 ->
    #e3d_transf{mat = e3d_mat:mul(M, Rot),
		inv = e3d_mat:mul(e3d_mat:transpose(Rot), I)}.

%%--------------------------------------------------------------------
%% @doc  Rotates the matrix with angle (in degress) and direction
%% @end
%%--------------------------------------------------------------------
-spec rotate(e3d_transform(), number(), vector()) -> e3d_transform().
rotate(Mat = #e3d_transf{}, A, Vec) ->
    rotate(Mat, e3d_mat:rotate(A,Vec)).

%%--------------------------------------------------------------------
%% @doc  Scales the matrix with {ScaleX, ScaleY, ScaleZ}
%% @end
%%--------------------------------------------------------------------
-spec scale(e3d_transform(), vector()) -> e3d_transform().
scale(#e3d_transf{mat=M,inv=I}, {X,Y,Z}) ->
    #e3d_transf{mat = e3d_mat:mul(M, e3d_mat:scale(X,Y,Z)),
		inv = e3d_mat:mul(e3d_mat:scale(1/X,1/Y,1/Z), I)}.

%%----------------------------------------------------------------------
%% @doc  Multiplies the current matrix (at right) with new Mat (at left)
%%       Trans(Vec) = Mat(Current(Vec))
%% @end
%%----------------------------------------------------------------------
-spec mul(e3d_transform(), e3d_transform()) -> e3d_transform().
mul(#e3d_transf{mat=M1,inv=I1}, #e3d_transf{mat=M2,inv=I2}) ->
    #e3d_transf{mat = e3d_mat:mul(M1, M2), inv = e3d_mat:mul(I2, I1)}.

%%--------------------------------------------------------------
%% mul([Rx,Ry,Rz]) = mul([mul(Ry,Rx),Rz])
%%--------------------------------------------------------------
-spec mul([e3d_transform()]) -> e3d_transform().
mul([#e3d_transf{}=A,#e3d_transf{}=B | T ]) -> mul([mul(B,A) | T]);
mul([#e3d_transf{}=A]) -> A.

%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc  Generates a world to camera transformation
%% @end
%%--------------------------------------------------------------------
-spec lookat(point(), vector(), vector()) -> e3d_transform().
lookat(Pos, Look, Up) ->
    Dir = e3d_vec:norm_sub(Look, Pos),
    Right = e3d_vec:norm(e3d_vec:cross(Dir, e3d_vec:norm(Up))),
    NewUp = e3d_vec:norm(e3d_vec:cross(Right, Dir)),
    AsList = [tuple_to_list(Right),             0.0,
	      tuple_to_list(NewUp),             0.0,
	      tuple_to_list(e3d_vec:neg(Dir)),  0.0,
	      0.0, 0.0, 0.0,                    1.0],
    CamToWorld = list_to_tuple(lists:flatten(AsList)),
    WorldToCam = e3d_mat:invert(CamToWorld),
    translate(#e3d_transf{mat=WorldToCam,inv=CamToWorld}, e3d_vec:neg(Pos)).

%%--------------------------------------------------------------------
%% @doc  Generates a ortho transformation
%% @end
%%--------------------------------------------------------------------
-spec ortho(float(), float()) -> e3d_transform().
ortho(Near, Far) ->
    ortho(-1.0, 1.0, -1.0, 1.0, Far, Near).

ortho(Left, Right, Bottom, Top, Near, Far) ->
    O = 0.0,
    IDx = 1/max(Right-Left, 0.00000001),
    IDy = 1/max(Top-Bottom, 0.00000001),
    IDz = 1/(Far-Near),

    Mat0 = {2.0, O, O,
	    O, 2.0, O,
	    O,   O, -2.0,
	    O,   O, O},
    %% Do this in 3 steps to avoid inverse calculation problems
    Mat1 = scale(init(Mat0), {IDx,IDy,IDz}),
    Trans = translate(identity(), {-(Right+Left)*IDx, 
				   -(Top+Bottom)*IDy, 
				   -(Far+Near)*IDz}),
    mul(Trans, Mat1).

    

%%--------------------------------------------------------------------
%% @doc  Generates a perspective transformation
%%       Fov = Field Of View (in degrees)
%%       Projects from camera space: Z = {-near, -far} 
%%                  to screen space: Z' = {0.0, 1.0}
%% @end
%%--------------------------------------------------------------------
-spec perspective(Fov::float(),
		  Near::float(), Far::float()) -> e3d_transform().
perspective(Fov, Near, Far) ->
    perspective(Fov, 1.0, Near, Far).

-spec perspective(Fov::float(), Aspect::float(),
		  Near::float(), Far::float()) -> e3d_transform().
perspective(Fov, Aspect, Near, Far) ->
    T = 1.0 / math:tan((Fov*math:pi()/180)/2.0),
    %% Perform projective divide
    D = 1.0 / (Far-Near),  %% Inverted Denom
    I = 1.0, O = 0.0,
    Persp = {I/Aspect, O,               O,  O,
	     O,        I,               O,  O,
	     O,        O,   -(Near+Far)*D, -I,
	     O,        O, -2.0*Far*Near*D,  O},
    InvPersp = e3d_mat:invert(Persp),
    scale(#e3d_transf{mat=Persp, inv=InvPersp}, {T,T,1.0}).


%%--------------------------------------------------------------------
%% @doc  Generates a pick matrix transformation
%%       Equiv to glu:pickMatrix/5
%% @end
%%--------------------------------------------------------------------
-spec pick(X::float(), Y::float(),
	   Width::float(), Height::float(),
	   Viewport::{integer(),integer(),integer(),integer()}
	  ) -> e3d_transform().
pick(X, Y, W, H, {X0,Y0,X1,Y1}) ->
    Sx = X1 / W,
    Sy = Y1 / H,
    Tx = (X1+2.0*(X0-X)) / W,
    Ty = (Y1+2.0*(Y0-Y)) / H,
    I = 1.0, O = 0.0,
    Pick = {Sx, O, O, O,
	    O, Sy, O, O,
	    O,  O, I, O,
	    Tx,Ty, O, I},
    init(Pick).
