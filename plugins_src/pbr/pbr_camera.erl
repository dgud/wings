%%
%%  pbr_camera.erl
%%
%%     Pbr camera handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_camera).

-include("pbr.hrl").

-export([init/2,
	 get_size/1,
	 get_fdist/1,
	 get_near_far/1,
	 pack_camera/2,
	 generate_ray/3]).

-record(cam, 
	{
	  w,h,		                        % Width Height
	  near,far,
	  f_dist,                               % Focus distance
	  c2w,					% Camera to world
	  r2c,					% Raster to camera
	  %%  TODO unneeded? keeping temporary for debugging.
	  s2r,					% Screen to raster
	  c2s					% Camera to screen
	}).

init(Attr, R=#renderer{}) ->
    CamProps = proplists:get_value(cam, Attr),
    {XRes, YRes} = proplists:get_value(resolution, Attr, {512,256}),
    {Pos,Dir,Up} = proplists:get_value(pos_dir_up, CamProps),
    %% io:format("Pos Dir Up ~p ~p~n", [(Pos), (Dir)]),
    World2Cam = e3d_transform:lookat(Pos, e3d_vec:add(Pos,Dir), Up),
    
    AspectRatio = XRes/YRes,

    Near = proplists:get_value(hither, CamProps),
    Far  = proplists:get_value(yon,  CamProps),
    Camera2Screen = 
	case proplists:get_value(camera_type, Attr, perspective) of
	    perspective ->
		Fov  = proplists:get_value(fov, CamProps),
		e3d_transform:perspective(Fov, Near, Far);
	    ortho ->
		e3d_transform:ortho(Near, Far)
	end,
    Screen2Camera = e3d_transform:inverse(Camera2Screen),
    
    {Xmin,Xmax,Ymin,Ymax} = 
	case AspectRatio >= 1.0 of
	    true  -> {-AspectRatio,AspectRatio,-1.0,1.0};
	    false -> {-1.0,1.0,-1.0/AspectRatio,1.0/AspectRatio}
	end,
    Center = e3d_transform:translate(e3d_transform:identity(),
    				     {-Xmin, Ymax, 0.0}),
    AspectScale = e3d_transform:scale(Center, 
    				      {1.0/(Xmax-Xmin),1.0/(Ymax-Ymin),1.0}),
    Screen2Raster = e3d_transform:scale(AspectScale, {XRes,YRes, 1.0}),

    Raster2Screen = e3d_transform:inverse(Screen2Raster),
    Raster2Camera = e3d_transform:mul(Raster2Screen, Screen2Camera),
    
    Cam = #cam{w=XRes, h=YRes,
	       near=Near, far=Far,
	       f_dist = e3d_vec:len(Dir),
	       c2w = e3d_transform:inverse(World2Cam),
	       r2c = Raster2Camera,
	       s2r = Screen2Raster,
	       c2s = Camera2Screen
	      },
    R#renderer{cam=Cam}.

get_size(#renderer{cam=#cam{w=W, h=H}}) ->
    {W,H}.

get_fdist(#renderer{cam=#cam{f_dist=FDist}}) ->
    FDist.

get_near_far(#renderer{cam=#cam{near=Near, far=Far}}) ->
    {Near,Far}.

pack_camera(LensR, #renderer{cam=#cam{c2w=C2W,r2c=R2C, 
				      f_dist=FDist, near=Near, far=Far}}) ->
    Bin0 = <<LensR:?F32, FDist:?F32, Far:?F32, Near:?F32>>,
    %% The opencl renderer code expects transposed matrixes.
    %% and swap left - right handedness 
    Ray2Cam = e3d_mat:transpose(e3d_transform:matrix(R2C)),
    Bin = pack_matrix(Ray2Cam, Bin0),
    Cam2W = e3d_mat:transpose(e3d_transform:matrix(C2W)),
    pack_matrix(Cam2W, Bin).

pack_matrix({A,B,C,WX,D,E,F,WY,G,H,I,WZ,Tx,Ty,Tz,WW}, Bin) ->
    <<Bin/binary, 
      A:?F32,B:?F32,C:?F32,WX:?F32,
      D:?F32,E:?F32,F:?F32,WY:?F32,
      G:?F32,H:?F32,I:?F32,WZ:?F32,
      Tx:?F32,Ty:?F32,Tz:?F32,WW:?F32>>.


generate_ray(#renderer{cam=Cam},X,Y) when is_float(X), is_float(Y) ->
    #cam{r2c=R2C, c2w=C2W, near=Near, far=Far} = Cam,
    Origo = e3d_mat:mul_point(e3d_transform:matrix(R2C), {X,Y,0.0}),
    Vec = case false of
	      true -> 
		  %% Adjust for camera lens
		  ok;
	      false ->
		  e3d_vec:norm(Origo)
	  end,
    {_,_,Z} = Vec,
    C2Wm = e3d_transform:matrix(C2W),
    #ray{o=e3d_mat:mul_point(C2Wm,Origo),
	 d=e3d_mat:mul_vector(C2Wm,Vec),
	 n=?RAY_EPS,
	 f=(Far-Near)/-Z}.

