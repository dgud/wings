%%
%%  pbr_grid.erl
%%
%%     Pbr grid accel lookup
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_grid).
-export([init/3, nearest/2]).

-include("pbr.hrl").

-record(grid, 
	{bbMin,
	 maxIndx,
	 invCellSz,
	 grid}).

init(HPs, MaxDist, {Min = {Nx,Ny,Nz},{Xx,Xy,Xz}}) ->
    InvCellSz = 1 / (math:sqrt(MaxDist)*2),
    MaxIndx = trunc((Xx-Nx) * InvCellSz),
    MaxIndy = trunc((Xy-Ny) * InvCellSz),
    MaxIndz = trunc((Xz-Nz) * InvCellSz),
    MaxI = {MaxIndx,MaxIndy, MaxIndz},

    Add = fun(Index, {Pos, PhotonR2}, Grid) ->
		  R = math:sqrt(PhotonR2),
		  Rad = {R,R,R},
		  Bmin = e3d_vec:mul(e3d_vec:sub(e3d_vec:sub(Pos, Rad), Min),InvCellSz),
		  Bmax = e3d_vec:mul(e3d_vec:sub(e3d_vec:add(Pos, Rad), Min),InvCellSz),
		  Start = clamp_int(Bmin,MaxI),
		  Stop  = clamp_int(Bmax, MaxI),
		  add(Index, Start, Start, Stop, Grid)
	  end,
    
    Grid0 = pbr_hp:fold_surface(Add, array:new({default,[]}), HPs),

    Grid = #grid{bbMin = Min, invCellSz = InvCellSz, maxIndx = MaxI, grid = Grid0},
    Grid.

nearest(Point, #grid{bbMin=Min, invCellSz=InvCellSz, maxIndx=MaxI={Mx,My,Mz}, grid=Grid}) ->
    GridPos = {X,Y,Z} = e3d_vec:mul(e3d_vec:sub(Point, Min), InvCellSz),
    if X < 0.0, X > Mx -> [];
       Y < 0.0, Y > My -> [];
       Z < 0.0, Z > Mz -> [];
       true ->
	    Hash = erlang:phash2(clamp_int(GridPos,MaxI)),
	    array:get(Hash, Grid)
    end.

clamp_int({X,Y,Z}, {Mx,My,Mz}) ->
    {min(max(0,trunc(X)), Mx),
     min(max(0,trunc(Y)), My),
     min(max(0,trunc(Z)), Mz)}.
    
add(Index, GridPos = {Nx,Ny,Nz},Start, Stop = {Xx,_,_}, Grid) when Nx =< Xx ->
    Hash = erlang:phash2(GridPos),
    List = array:get(Hash, Grid),
    add(Index, {Nx+1,Ny,Nz}, Start, Stop, array:set(Hash, [Index|List], Grid));
add(Index, {_,Ny,Nz}, Start = {Sx,_,_}, Stop = {_,Xy,_}, Grid) when Ny =< Xy ->
    add(Index, {Sx,Ny+1,Nz}, Start,Stop, Grid);
add(Index, {_,_,Nz}, Start = {Sx,Sy,_}, Stop = {_,_,Xz}, Grid) when Nz =< Xz ->
    add(Index, {Sx,Sy,Nz+1}, Start,Stop, Grid);
add(_, _, _, _, Grid) ->
    Grid.
    

    
