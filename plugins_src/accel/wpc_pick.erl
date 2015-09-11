%%
%%  wpc_pick.erl --
%%
%%     This module handles picking using our own driver.
%%
%%  Copyright (c) 2009-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This module and driver implements picking of faces, edges, and vertices
%% without using the selection support in OpenGL. The selection support in
%% OpenGL is not use by many applications, so it may not work reliably in
%% all implementations of OpenGL.
%%
%% Conceptually, we use the same algorithm as OpenGL. We set up a special
%% view volume that only includes a few pixels on each side of the mouse
%% cursor (or what is inside the marquee) using a special pick matrix.
%% We then go through all objects in the scene to which faces/edges/vertices
%% fall inside the view volume.
%%
%% We use a different pick matrix and view volume than OpenGL, namely
%% 0 <= X <= 1, 0 <= Y <= 1, 0 <= Z <= 1 instead of -1 <= X <= 1,
%% -1 <= Y <= 1, -1 <= Z <= 1 as suggested by Jim Blinn in
%% A Trip Down the Graphics Pipeline.
%%
%% Main references:
%%
%% Jim Blinn: A Trip Down the Graphics Pipeline. Chapter 13: Line Clipping.
%%
%% Paul Heckbert: Generic Convex Polygon Scan Conversion and Clipping in
%%                Graphics Gems.
%%
-module(wpc_pick).
-export([init/0,pick_matrix/5,matrix/2,cull/1,front_face/1,
	 faces/2,vertices/1,edges/1]).

%% Comment out the following line to use the pure Erlang
%% reference implementation.
-define(USE_DRIVER, 1).

-import(lists, [foldl/3,last/1,sort/1]).
-define(FL, 32/native-float).

init() ->
    case get(wings_not_running) of
	undefined ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    Name = "wings_pick_drv",
	    case erl_ddll:load_driver(Dir, Name) of
		ok -> ok;
		{error,Reason} ->
		    io:format("Failed to load ~s in ~s\n~s\n",
			      [Name,Dir,erl_ddll:format_error(Reason)]),
		    erlang:halt()
	    end,
	    try
		Port = open_port({spawn_driver,Name}, [binary]),
		register(wings_pick_port, Port)
	    catch error:_ ->
		    io:format("Failed to open port ~s.\n", [Name]),
		    erlang:halt()
	    end;
	_ ->
	    ignore
    end,
    false.

%% pick_matrix(X, Y, Xs, Ys, ViewPort) -> PickMatrix
%%  Set up a pick matrix like glu:pickMatrix/5,
%%  but with a diffrent viewing volume (the cube (0...1)^3
%%  instead of (-1...1)^3).
%%
pick_matrix(X, Y, Xs, Ys, ViewPort) ->
    M0 = e3d_transform:translate(e3d_transform:identity(), {0.5, 0.5, 0.5}),
    M1 = e3d_transform:scale(M0, {0.5,0.5,0.5}),
    Pick = e3d_transform:pick(X, Y, Xs, Ys, ViewPort),
    e3d_transform:mul(M1, Pick).

%% matrix(ModelViewMatrix, ProjectionMatrix)
%%  Set the matrix to use for picking by combining the model
%%  view and projection matrices.
%%
matrix(Model, Proj) when is_list(Model) ->
    matrix(list_to_tuple(Model), Proj);
matrix(Model, Proj) when is_list(Proj) ->
    matrix(Model, list_to_tuple(Proj));
matrix(Model, Proj) ->
    Mat = e3d_mat:mul(Proj, Model),
    case put({?MODULE,matrix}, Mat) of
	Mat -> ok;
	_ -> drv_matrix(Mat)
    end.

%% cull(true|false)
%%  Enable or disable backface culling when picking.
cull(false) ->
    case erase({?MODULE,cull}) of
	undefined -> ok;
	_ -> drv_cull(0)
    end;
cull(true) ->
    case put({?MODULE,cull}, true) of
	true -> ok;
	_ -> drv_cull(1)
    end.

%% front_face(ccw|cw)
%%  Define the vertex order for front facing triangles.
front_face(ccw) ->
    case erase({?MODULE,front_face}) of
	undefined -> ok;
	_ -> drv_ccw_is_front(1)
    end;
front_face(cw) ->
    case put({?MODULE,front_face}, cw) of
	cw -> ok;
	_ -> drv_ccw_is_front(0)
    end.

%% faces({Stride,VertexBuffer}, OneHit) -> {Index,Depth} | [Index]
%%     Depth = 0..2^32-1 (0 means the near clipping plane)
%%  Given a vertex buffer containing triangles, either return
%%  {Index,Depth} (if OneHit is 'true'), or a list of indices for each
%%  triangle that are wholly or partly inside the viewing volume
%%  (if OneHit is 'false').
%%
-ifdef(USE_DRIVER).
faces({_,<<>>}, _) ->
    %% An empty binary is most probably not reference-counted,
    %% so we must *not* send it down to the driver. (The length
    %% of the I/O vector will be 2, not 3, and the driver will
    %% ignore the request without sending any data back to us.)
    [];
faces({Stride,Bin}, OneHit0) ->
    OneHit = case OneHit0 of
		 false -> <<0>>;
		 true -> <<1>>
	     end,
    erlang:port_control(wings_pick_port, 3, OneHit),
    erlang:port_command(wings_pick_port, [<<Stride:32/native>>,Bin]),
    receive
	{Port,{data,Data}} when is_port(Port) ->
	    case OneHit0 of
		false ->
		    [Hit || <<Hit:32/native>> <= Data];
		true ->
		    case Data of
			<<>> ->
			    [];
			<<Hit:32/native,Depth:32/native>> ->
			    {Hit,Depth}
		    end
	    end
    end.
-else.
faces({Stride,Bin}, OneHit) ->
    Matrix = get({?MODULE,matrix}),
    Cull0 = get({?MODULE,cull}) =:= true,
    CwIsFront = get({?MODULE,front_face}) =:= cw,
    Cull = case {Cull0,CwIsFront} of
	       {false,_} -> none;
	       {true,true} -> cull_ccw;
	       {true,false} -> cull_cw
	   end,
    faces_1(Bin, Stride-12, Matrix, Cull, OneHit, 0, []).
-endif.

%% vertices([{Vertex,Position}]) -> [Vertex]
%%  Return a list of all vertices that are inside the
%%  viewing volume.
%%
vertices(VsPos) ->
    Matrix = get({?MODULE,matrix}),
    vertices_1(VsPos, Matrix, []).

%% vertices([{Edge,Position}]) -> [Edges]
%%  Return a list of all edges that are inside the
%%  viewing volume.
%%
edges(EsPos) ->
    Matrix = get({?MODULE,matrix}),
    edges_1(EsPos, Matrix, []).

%%%
%%% Communication with the driver.
%%%

-ifndef(USE_DRIVER).
%% Dummies when there is no driver.
drv_matrix(_) -> ok.
drv_cull(_) -> ok.
drv_ccw_is_front(_) -> ok.
-else.
drv_matrix(Mat0) ->
    Mat = << <<F:?FL>> || F <- tuple_to_list(Mat0) >>,
    drv(0, Mat).

drv_cull(Bool) -> drv(1, [Bool]).

drv_ccw_is_front(Bool) -> drv(2, [Bool]).

drv(Cmd, Data) ->
    erlang:port_control(wings_pick_port, Cmd, Data).
-endif.

%%%
%%% Internal functions.
%%%

vertices_1([{V,{X0,Y0,Z0}}|T], Mat, Acc) ->
    {X,Y,Z,W} = e3d_mat:mul(Mat, {X0,Y0,Z0,1.0}),
    Inside = X >= 0 andalso W-X >= 0 andalso
	Y >= 0 andalso W-Y >= 0 andalso
	Z >= 0 andalso W-Z >= 0,
    case Inside of
	true ->
	    vertices_1(T, Mat, [V|Acc]);
	false ->
	    vertices_1(T, Mat, Acc)
    end;
vertices_1([], _, Acc) -> Acc.

edges_1([{E,{P0,P1}}|T], Mat, Acc) ->
    case edge_visible(P0, P1, Mat) of
	false -> edges_1(T, Mat, Acc);
	true -> edges_1(T, Mat, [E|Acc])
    end;
edges_1([], _, Acc) ->
    Acc.

edge_visible({X0,Y0,Z0}, {X1,Y1,Z1}, Mat) ->
    Pos0 = e3d_mat:mul(Mat, {X0,Y0,Z0,1.0}),
    Pos1 = e3d_mat:mul(Mat, {X1,Y1,Z1,1.0}),
    OutCode0 = outcode(Pos0),
    OutCode1 = outcode(Pos1),
    case OutCode0 band OutCode1 of
	0 ->
	    case OutCode0 bor OutCode1 of
		0 ->
		    %% Both endpoints are inside all planes. Trivial accept.
		    true;
		Clip ->
		    non_trivial_edge_visible(OutCode0, Clip, Pos0, Pos1,
					     32, 0.0, 1.0)
	    end;
	_ ->
	    %% Both endpoints are outside the same plane. Trivial reject.
	    false
    end.

non_trivial_edge_visible(_Code0, _Clip, _P0, _P1, 0, _A0, _B0) ->
    %% The line has been clipped against all planes and
    %% rejected. Non-trivial accept.
    true;
non_trivial_edge_visible(Code0, Clip, P0, P1, Plane, A0, B0) ->
    case Clip band Plane of
	0 ->
	    %% Both endpoints are on the inside side of the plane.
	    %% Continue with the next plane.
	    non_trivial_edge_visible(Code0, Clip, P0, P1, Plane bsr 1, A0, B0);
	_ ->
	    %% One endpoint outside, one inside.
	    Dot0 = pdot2(Plane, P0),
	    Dot1 = pdot2(Plane, P1),
	    NewAlpha = Dot0 / (Dot0 - Dot1),
	    {A,B} = case Code0 band Plane of
			0 ->
			    {A0,min(B0, NewAlpha)};
			_ ->
			    {max(A0, NewAlpha),B0}
		    end,
	    if
		B < A ->
		    %% The line is completely outside of all planes.
		    %% Non-trivial reject.
		    false;
		true ->
		    non_trivial_edge_visible(Code0, Clip, P0, P1, Plane bsr 1, A, B)
	    end
    end.

-define(SHIFT_OUT(Code, Dot), if Dot < 0.0 -> (Code bsl 1) bor 1;
				 true -> Code bsl 1 end).
outcode({X,_,_,_}=P) ->
    outcode_1(P, ?SHIFT_OUT(0, X)).

outcode_1({X,_,_,W}=P, Code) ->
    outcode_2(P, ?SHIFT_OUT(Code, W-X)).

outcode_2({_,Y,_,_}=P, Code) ->
    outcode_3(P, ?SHIFT_OUT(Code, Y)).

outcode_3({_,Y,_,W}=P, Code) ->
    outcode_4(P, ?SHIFT_OUT(Code, W-Y)).

outcode_4({_,_,Z,_}=P, Code) ->
    outcode_5(P, ?SHIFT_OUT(Code, Z)).

outcode_5({_,_,Z,W}, Code) ->
    ?SHIFT_OUT(Code, W-Z).

pdot2(32, {X,_,_,_}) -> X;
pdot2(16, {X,_,_,W}) -> W-X;
pdot2(8, {_,Y,_,_}) -> Y;
pdot2(4, {_,Y,_,W}) -> W-Y;
pdot2(2, {_,_,Z,_}) -> Z;
pdot2(1, {_,_,Z,W}) -> W-Z.


%%%
%%% Reference implementation in pure Erlang of face picking.
%%%

-ifndef(USE_DRIVER).
faces_1(Bin0, Unused, Mat, Cull, OneHit, I, Acc) ->
    case Bin0 of
	<<X1:?FL,Y1:?FL,Z1:?FL,_:Unused/binary,
	 X2:?FL,Y2:?FL,Z2:?FL,_:Unused/binary,
	 X3:?FL,Y3:?FL,Z3:?FL,_:Unused/binary,
	 Bin/binary>> ->
	    Tri0 = [{X1,Y1,Z1,1.0},{X2,Y2,Z2,1.0},{X3,Y3,Z3,1.0}],
	    Tri = [e3d_mat:mul(Mat, P) || P <- Tri0],
	    case clip_tri(Tri, Cull) of
		[] ->
		    %% Outside.
		    faces_1(Bin, Unused, Mat, Cull, OneHit, I+3, Acc);
		[{_,_,Z,W}|_] ->
		    %% Inside. Now clipped to the view frustum.
		    Depth = round((Z/W)*16#FFFFFFFF),
		    faces_1(Bin, Unused, Mat, Cull, OneHit, I+3, [{Depth,I}|Acc])
	    end;
	<<>> ->
	    Hits = sort(Acc),
	    case OneHit of
		false ->
		    sort([Hit || {_,Hit} <- Hits]);
		true ->
		    case Hits of
			[] -> [];
			[{Depth,Hit}|_] -> {Hit,Depth}
		    end
	    end
    end.

clip_tri(Tri, Cull) ->
    case clip_tri_1(Tri) of
	[] -> [];
	Vs ->
	    Inside = case Cull of
			 none -> true;
			 cull_cw -> is_ccw(Vs);
			 cull_ccw -> not is_ccw(Vs)
		     end,
	    case Inside of
		true -> Vs;
		false -> []
	    end
    end.

clip_tri_1(Tri) ->
    OutCodes = [outcode(P) || P <- Tri],
    And = foldl(fun(Code, Acc) -> Code band Acc end, 16#3F, OutCodes),
    case And of
	0 ->
	    Or = foldl(fun(Code, Acc) -> Code bor Acc end, 0, OutCodes),
	    case Or of
		0 ->
		    %% All vertices are inside all planes. Trivial accept.
		    Tri;
		_ ->
		    %% Handle the non-trivial cases.
		    non_trivial(Tri)
	    end;
	_ ->
	    %% All vertices are outside at least one of the planes.
	    %% Trivial reject.
	    []
    end.

non_trivial(Ps) ->
    non_trivial_1(Ps, 6).

non_trivial_1(Ps, 0) ->
    %% Checked against all planes and not rejected. Non-trivial accept.
    Ps;
non_trivial_1(Ps0, Plane) ->
    case non_trivial_2(last(Ps0), Ps0, Plane) of
	Ps when length(Ps) < 3 ->
	    %% No longer a triangle or polygon. Non-trivial reject.
	    [];
	Ps ->
	    non_trivial_1(Ps, Plane-1)
    end.

non_trivial_2(Prev, [P|T], Plane) ->
    case {out(Plane, Prev),out(Plane, P)} of
	{false,false} ->
	    %% Both inside. Keep current vertex (P).
	    [P|non_trivial_2(P, T, Plane)];
	{false,true} ->
	    %% Previous inside, current outside.
	    %% Keep intersection of edge and clipping plane.
	    [intersection(Prev, P, Plane)|non_trivial_2(P, T, Plane)];
	{true,false} ->
	    %% Previous outside, current inside.
	    %% Keep intersection of edge and clipping plane,
	    %% followed by current.
	    [intersection(Prev, P, Plane),P|non_trivial_2(P, T, Plane)];
	{true,true} ->
	    %% Both outside. Remove the current vertex.
	    non_trivial_2(P, T, Plane)
    end;
non_trivial_2(_, [], _) -> [].

is_ccw([{X1,Y1,_,W1},{X2,Y2,_,W2},{X3,Y3,_,W3}|_]) ->
    XWinC = X3/W3,
    YWinC = Y3/W3,
    DxAC = X1/W1 - XWinC,
    DxBC = X2/W2 - XWinC,
    DyAC = Y1/W1 - YWinC,
    DyBC = Y2/W2 - YWinC,
    Area = DxAC * DyBC - DxBC * DyAC,
    Area >= 0.0.

intersection(P0, P1, Plane) ->
    Dot0 = pdot(Plane, P0),
    Dot1 = pdot(Plane, P1),
    A = Dot0 / (Dot0 - Dot1),
    add(P0, mul(sub(P1, P0), A)).

mul({V10,V11,V12,V13}, S) when is_float(S) ->
    {V10*S,V11*S,V12*S,V13}.

sub({V10,V11,V12,V13}, {V20,V21,V22,V23}) ->
    {V10-V20,V11-V21,V12-V22,V13-V23}.

add({V10,V11,V12,V13}, {V20,V21,V22,V23})
  when is_float(V10), is_float(V11), is_float(V12) ->
    {V10+V20,V11+V21,V12+V22,V13+V23}.

out(Plane, P) -> pdot(Plane, P) < 0.0.

pdot(1, {X,_,_,_}) -> X;
pdot(2, {X,_,_,W}) -> W-X;
pdot(3, {_,Y,_,_}) -> Y;
pdot(4, {_,Y,_,W}) -> W-Y;
pdot(5, {_,_,Z,_}) -> Z;
pdot(6, {_,_,Z,W}) -> W-Z.
-endif.
