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
	 lookat/3, ortho/2, perspective/3,
	 %% Get the actual matrices
	 matrix/1, inv_matrix/1,
	 %% Transform the matrices
	 inverse/1, translate/2, rotate/2, rotate/3, scale/2, mul/1, mul/2
	]).


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
-spec init(e3d_matrix()) -> e3d_transform().
init(Mat) when tuple_size(Mat) =:= 12 ->
    init(e3d_mat:expand(Mat));
init(Mat) ->
    #e3d_transf{mat=Mat, inv=e3d_mat:invert(Mat)}.

%%--------------------------------------------------------------------
%% @doc  Returns the matrix
%% @end
%%--------------------------------------------------------------------
-spec matrix(e3d_transform()) -> e3d_matrix().
matrix(#e3d_transf{mat=M}) -> M.

%%--------------------------------------------------------------------
%% @doc  Returns the inverse matrix
%% @end
%%--------------------------------------------------------------------
-spec inv_matrix(e3d_transform()) -> e3d_matrix().
inv_matrix(#e3d_transf{inv=I}) -> I.

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
-spec translate(e3d_transform(), e3d_vector()) -> e3d_transform().
translate(#e3d_transf{mat=M,inv=I}, {Dx,Dy,Dz}) ->
    #e3d_transf{mat = e3d_mat:mul(e3d_mat:translate(Dx,Dy,Dz), M),
		inv = e3d_mat:mul(I, e3d_mat:translate(-Dx,-Dy,-Dz))}.

%%--------------------------------------------------------------------
%% @doc  Rotates the matrix with rotation matrix
%% @end
%%--------------------------------------------------------------------
-spec rotate(e3d_transform(), e3d_matrix()) -> e3d_transform().
rotate(#e3d_transf{mat=M,inv=I}, Rot) 
  when tuple_size(Rot) =:= 12; tuple_size(Rot) =:= 16 ->
    #e3d_transf{mat = e3d_mat:mul(Rot, M),
		inv = e3d_mat:mul(I, e3d_mat:transpose(Rot))}.

%%--------------------------------------------------------------------
%% @doc  Rotates the matrix with angle (in degress) and direction
%% @end
%%--------------------------------------------------------------------
-spec rotate(e3d_transform(), number(), e3d_vector()) -> e3d_transform().
rotate(Mat = #e3d_transf{}, A, Vec) ->
    rotate(Mat, e3d_mat:rotate(A,Vec)).

%%--------------------------------------------------------------------
%% @doc  Scales the matrix with {ScaleX, ScaleY, ScaleZ}
%% @end
%%--------------------------------------------------------------------
-spec scale(e3d_transform(), e3d_vector()) -> e3d_transform().
scale(#e3d_transf{mat=M,inv=I}, {X,Y,Z}) ->
    #e3d_transf{mat = e3d_mat:mul(e3d_mat:scale(X,Y,Z), M),
		inv = e3d_mat:mul(I, e3d_mat:scale(1/X,1/Y,1/Z))}.

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
-spec lookat(e3d_point(), e3d_vector(), e3d_vector()) -> e3d_transform().
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
    Translate = translate(identity(), e3d_vec:neg(Pos)),
    mul(#e3d_transf{mat=WorldToCam,inv=CamToWorld}, Translate).

%%--------------------------------------------------------------------
%% @doc  Generates a ortho transformation
%% @end
%%--------------------------------------------------------------------
-spec ortho(float(), float()) -> e3d_transform().
ortho(Near, Far) ->
    Trans = translate(identity(), {0.0, 0.0, -Near}),
    scale(Trans, {1.0, 1.0, 1.0/ (Far-Near)}).

%%--------------------------------------------------------------------
%% @doc  Generates a perspective transformation
%%       Fov = Field Of View (in degrees)
%%       Projects from camera space: Z = {-near, -far} 
%%                  to screen space: Z' = {0.0, 1.0}
%% @end
%%--------------------------------------------------------------------
-spec perspective(Fov::float(), float(), float()) -> e3d_transform().
perspective(Fov, Near, Far) ->
    T = 1.0 / math:tan((Fov*math:pi()/180)/2.0),
    %% Perform projective divide
    D = 1.0 / (Far-Near),  %% Inverted Denom
    I = 1.0, O = 0.0,    
    Persp = {I,O,O,O, 
	     O,I,O,O, 
	     O,O, -Far*D,-I,
	     O,O, -Far*Near*D,  O},
    InvPersp = e3d_mat:invert(Persp),
    e3d_transform:scale(#e3d_transf{mat=Persp, inv=InvPersp},
			{T,T,1.0}).

			



