%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_pathgpu).
-export([start/2]).

-include("pbr.hrl").

-record(ropt, 
	{max_path_depth,
	 rr_depth,
	 rr_imp_cap,
	 sampler,
	 filter, 
	 lens_r,
	 refresh
	}).
	
-record(sampler, {type, opts=[]}).
-record(filter,  {type, dim={0.0,0.0}, opts=[]}).

-record(ps, { %% path_state 
	  %% Work buffers
	  rays, 
	  hits,
	  framebuffer,
	  task,
	  taskstats,
	  %% Scene buffers
	  meshids,  
	  mesh2mat, 
	  mats,     
	  colors,   
	  normals,  
	  triangles,
	  vertices,
	  %% Light
	  arealightn, 
	  arealight,  
	  inflight,  
	  inflightmap,
	  sunlight,   
	  skylight,   
	  %% Textures
	  texmaprgb,
	  texmapalpha,
	  texmapdesc,
	  meshtexs,
	  meshbumps,
	  meshbumpsscale,
	  uvsb,
	  %% Camera
	  cam}).	 

-define(ifelse(A,B,C), if (A) -> (B); true -> (C) end).
-define(FSz, 4).
-define(ISz, 4).
-define(PathStateDLSz, 2*?ISz + 3*?FSz + ?FSz+ ?ISz + ?RAY_SZ + 2*3*?FSz).
-define(PathStateSz, 2*?ISz + 3*?FSz).
-define(TASKSTAT_SZ, ?ISz).

start(Attrs, SceneS) ->
    ROpt = #ropt{max_path_depth = proplists:get_value(max_path_depth, Attrs, 5),
		 rr_depth       = proplists:get_value(rr_depth, Attrs, 3),
		 rr_imp_cap     = proplists:get_value(rr_imp_cap, Attrs, 0.125),
		 sampler        = get_sampler(Attrs),
		 filter         = get_filter(Attrs),
		 lens_r         = (proplists:get_value(aperture, Attrs, 0.0) / 2.0),
		 refresh        = (proplists:get_value(refresh_interval, Attrs, 10)) * 1000
		},
    Res = start_processes(ROpt, SceneS),
    io:format("Rendering done ~n",[]),
    Res.


start_processes(Ropt, SceneS0) ->
    %% NoThreads = erlang:system_info(schedulers),
    random:seed(now()),
    {SceneS, PS, SAs} = init_render(1, random:uniform(1 bsl 32), 0.00, Ropt, SceneS0),
    render(SceneS, PS, Ropt, SAs).

render(SceneS = #renderer{cl=CL}, #ps{framebuffer=FB, rays=RaysB, hits=HitsB}, 
       #ropt{refresh=RI}, IAs) ->
    {X,Y} = pbr_film:resolution(SceneS),
    W0 = wings_cl:cast('InitFrameBuffer', [FB], X*Y, [], CL),    
    W1 = wings_cl:cast('Init', IAs, ?MAX_RAYS, [], CL),
    StartTime = os:timestamp(),
    {Qn,Qt,LocalMem} = pbr_scene:intersect_data(SceneS),
    wings_cl:set_args('Intersect', [RaysB, HitsB, Qn, Qt, ?MAX_RAYS, LocalMem], CL),
    render_loop(10, [W0,W1], CL, {StartTime, FB, RI, SceneS}).

render_loop(N, Wait, CL, Data) when N > 0 ->
    W1 = wings_cl:cast('Sampler', ?MAX_RAYS, Wait, CL),
    W2 = wings_cl:cast('Intersect', ?MAX_RAYS, [W1], CL),
    W3 = wings_cl:cast('AdvancePaths', ?MAX_RAYS, [W2], CL),
    cl:wait(W3),
    exit(foo),
    render_loop(N-1, [W3], CL, Data);
render_loop(0, Wait, CL, Data = {StartTime, FB, RI, SceneS0}) ->
    receive stop -> 
	    update_film(Wait, FB, SceneS0, CL),
	    normal
    after 0 ->
	    Elapsed = timer:now_diff(StartTime, os:timestamp()) *1000,
	    case Elapsed > RI of
		true ->
		    SceneS1 = update_film(Wait, FB, SceneS0, CL),
		    render_loop(10, [], CL, {os:timestamp(), FB, RI, SceneS1});
		false ->
		    render_loop(10, Wait, CL, Data)
	    end
    end.

update_film(Wait, FB, SceneS, CL) ->
    {X,Y} = pbr_film:resolution(SceneS),
    W0 = cl:read(FB, X*Y*?RAYHIT_SZ, Wait, CL),
    {ok, SampleBuff} = cl:wait(W0),
    Scene = pbr_film:set_raw(SampleBuff),
    pbr_film:show(Scene),
    Scene.

init_render(_Id, Seed, Start, Opts, SceneS0) ->
    PS1 = #ps{},
    MeshBuffs = {_, _, Materials} = pbr_scene:mesh2mat(SceneS0),
    PS2 = create_scene_buffs(PS1, MeshBuffs, SceneS0),
    PS3 = create_light_buffs(PS2, SceneS0),
    PS4 = create_tex_buffs(PS3, SceneS0),    
    CamBin = pbr_camera:pack_camera(Opts#ropt.lens_r, SceneS0), 
    PS5 = PS4#ps{cam=wings_cl:buff(CamBin, SceneS0#renderer.cl)},
    PS  = create_work_buffs(PS5, calc_task_size(PS5, Opts), SceneS0), 
    io:format("Everything packed~n",[]),    

    CompilerParams = create_params(Seed, Start, Materials, Opts, PS, SceneS0),
    SceneS = compile(SceneS0, CompilerParams),
    SamplerArgs = sampler_args(PS, Opts, SceneS),
    AdvancePathsArgs = advance_paths_args(PS), 
    wings_cl:set_args('Sampler', SamplerArgs, SceneS#renderer.cl),
    wings_cl:set_args('AdvancePaths', AdvancePathsArgs, SceneS#renderer.cl),
    %% FIXME check kernel workgroup sizes
    {SceneS, PS, SamplerArgs}.

create_work_buffs(PS, TaskSize, SceneS = #renderer{cl=CL}) -> 
    {X,Y} = pbr_film:resolution(SceneS),
    %% Work areas
    PS#ps{rays        = wings_cl:buff(?RAYBUFFER_SZ,CL),
	  hits        = wings_cl:buff(?RAYHIT_SZ*?MAX_RAYS,CL),
	  framebuffer = wings_cl:buff(?RAYHIT_SZ*X*Y,CL),
	  task        = wings_cl:buff(TaskSize*?MAX_RAYS,CL),
	  taskstats   = wings_cl:buff(?TASKSTAT_SZ * ?MAX_RAYS,CL)}.

create_scene_buffs(PS, {Face2Mesh, Mesh2Mat, Mats}, SceneS = #renderer{cl=CL}) ->
    %% Static Scene buffers
    PS#ps{meshids   = wings_cl:buff(Face2Mesh,CL),
	  mesh2mat  = wings_cl:buff(Mesh2Mat,CL),
	  mats      = wings_cl:buff(pbr_mat:pack_materials(Mats),CL),
	  colors    = wings_cl:buff(pbr_scene:vertex_colors(SceneS),CL),
	  normals   = wings_cl:buff(pbr_scene:normals(SceneS),CL),
	  triangles = wings_cl:buff(pbr_scene:triangles(SceneS),CL),
	  vertices  = wings_cl:buff(pbr_scene:vertices(SceneS),CL)}.

create_light_buffs(PS0, SceneS = #renderer{cl=CL}) ->    
    %% Lights
    Lights = pbr_scene:get_lights(SceneS),
    %% AreaLight = StatBuff(pbr_light:pack_area(SceneS)),
    PS = PS0#ps{arealightn  = 0,      %% Fixme
		arealight   = false,  %% Fixme
		inflight    = false,
		inflightmap = false,
		sunlight    = wings_cl:buff(pbr_light:pack_light(sunlight, Lights),CL),
		skylight    = wings_cl:buff(pbr_light:pack_light(skylight, Lights),CL)},    
    PS#ps.sunlight /= false orelse PS#ps.skylight /= false orelse exit(no_light),
    PS.

create_tex_buffs(PS, _SceneS) ->
    %% Textures   Fixme
    PS#ps{texmaprgb      = false,
	  texmapalpha    = false,
	  texmapdesc     = false,
	  meshtexs       = false,
	  meshbumps      = false,
	  meshbumpsscale = false,
	  uvsb           = false}.

create_params(Seed, Start, Materials, Opt, PS, SceneS) ->
    {X,Y} = pbr_film:resolution(SceneS),
    {_WCenter, WRad} = e3d_bv:sphere(pbr_scene:bb(SceneS)),
    StartLine = trunc(Start*Y),
    Ps = [param("PARAM_STARTLINE", StartLine),
	  param("PARAM_TASK_COUNT", ?MAX_RAYS),
	  param("PARAM_IMAGE_WIDTH",  X),
	  param("PARAM_IMAGE_HEIGHT", Y),
	  param("PARAM_RAY_EPSILON", ?RAY_EPS),
	  param("PARAM_SEED", Seed),
	  param("PARAM_MAX_PATH_DEPTH", Opt#ropt.max_path_depth),
	  param("PARAM_RR_DEPTH",  Opt#ropt.rr_depth),
	  param("PARAM_RR_CAP", Opt#ropt.rr_imp_cap),
	  param("PARAM_WORLD_RADIUS", WRad*1.01)
	 ],
    MatPs = [mat_param(pbr_mat:type(Mat)) || Mat <- Materials],
    MatPs == [] andalso exit(no_materials),
    CamLens = Opt#ropt.lens_r > 0.0 ,  
    CamPs = if CamLens -> " -D PARAM_CAMERA_HAS_DOF";
	       true -> []
	    end,
    LightPs = light_params(PS),
    TexPs = [],
    
    FilterPs = filter_params(Opt#ropt.filter),
    %% PixelAtomics = " -D PARAM_USE_PIXEL_ATOMICS";
    SamplerPs = sampler_params(Opt#ropt.sampler),
    %% Apple and ATI specific addons fixme
    lists:flatten([Ps, MatPs, CamPs, LightPs, TexPs, FilterPs, SamplerPs]).

sampler_args(PS=#ps{task=TaskB, taskstats=TaskStatsB, rays=RaysB, cam=CamB},
	     Opts, #renderer{cl=CL}) ->
    Args0 = stratified_sampler_size(Opts, PS) * wings_cl:get_wg_sz('Sampler',CL),
    case Args0 of
	0 -> [TaskB, TaskStatsB, RaysB, CamB];
	_ -> [TaskB, TaskStatsB, RaysB, CamB | Args0]
    end.

advance_paths_args(#ps{task=TaskB, rays=RaysB, hits=HitsB,
		       cam=CamB, framebuffer=FrameBufferB,
		       mats=MaterialsB, mesh2mat=Mesh2MatB, meshids=MeshIdsB,
		       colors=ColorsB, normals=NormalsB, vertices=VerticesB, triangles=TrianglesB, 
		       %% Optional
		       inflight=InfLightB, inflightmap=InfLightMapB, 
		       sunlight=SunLightB, skylight=SkyLightB, arealight=AreaLightB,
		       texmaprgb=TexMapRGBB, texmapalpha=TexMapAlphaB,
		       texmapdesc=TexMapDescB,  meshtexs=MeshTexsB,
		       meshbumps=MeshBumpsB,  meshbumpsscale=MeshBumpsScaleB,
		       uvsb=UvsB
		      }) ->
    %% Note order is very important
    Optional =  [InfLightB, InfLightMapB, SunLightB, SkyLightB, AreaLightB,
		 TexMapRGBB, TexMapAlphaB, TexMapDescB, MeshTexsB,
		 MeshBumpsB, MeshBumpsScaleB, UvsB],
    
    [TaskB, RaysB, HitsB, FrameBufferB, 
     MaterialsB, Mesh2MatB, MeshIdsB,
     %% TriangleIdBuff, MeshDescBuff  %% mqbvh only
     ColorsB, NormalsB, VerticesB, TrianglesB, CamB 
     | [ Buff || Buff <- Optional, Buff /= false]].

compile(RS=#renderer{cl=CL0}, Params) -> 
    Fs = ["pbr/pathgpu2_kernel_datatypes.cl", 
	  "pbr/pathgpu2_kernel_core.cl",
	  "pbr/pathgpu2_kernel_filters.cl",
	  "pbr/pathgpu2_kernel_scene.cl",
	  "pbr/pathgpu2_kernel_samplers.cl",
	  "pbr/pathgpu2_kernels.cl"],
    io:format("Defines: ~p~n",[Params]),
    CL = wings_cl:compile(Fs, Params, CL0),
    RS#renderer{cl=CL}.

param(Str, Value) when is_integer(Value) -> 
    io_lib:format(" -D ~s=~p", [Str,Value]);
param(Str, Value) when is_float(Value) -> 
    io_lib:format(" -D ~s=~ff", [Str,Value]).

mat_param(matte)       -> " -D PARAM_ENABLE_MAT_MATTE";
mat_param(arealight)   -> " -D PARAM_ENABLE_MAT_AREALIGHT";
mat_param(mirror)      -> " -D PARAM_ENABLE_MAT_MIRROR";
mat_param(glass)       -> " -D PARAM_ENABLE_MAT_GLASS";
mat_param(mattemirror) -> " -D PARAM_ENABLE_MAT_MATTEMIRROR";
mat_param(metal)       -> " -D PARAM_ENABLE_MAT_METAL";
mat_param(mattemetal)  -> " -D PARAM_ENABLE_MAT_MATTEMETAL";
mat_param(alloy)       -> " -D PARAM_ENABLE_MAT_ALLOY";
mat_param(archglass)   -> " -D PARAM_ENABLE_MAT_ARCHGLASS".

light_params(#ps{skylight=SkyLightB, sunlight=SunLightB, arealightn=AreaLightN, inflight=InfLight}) ->
    [?ifelse(InfLight /= false, " -D PARAM_HAS_INFINITELIGHT", []),
     ?ifelse(SkyLightB /= false, " -D PARAM_HAS_SKYLIGHT", []),
     if SunLightB /= false, AreaLightN == 0 ->
	     " -D PARAM_HAS_SUNLIGHT " ++
		 " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 " -D PARAM_DL_LIGHT_COUNT=0";	     
	SunLightB ->
	     " -D PARAM_HAS_SUNLIGHT" ++ 
		 " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 param("PARAM_DL_LIGHT_COUNT", AreaLightN);
	AreaLightN ->
	     " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 param("PARAM_DL_LIGHT_COUNT", AreaLightN)
     end].

filter_params(#filter{type=none}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=0";
filter_params(#filter{type=box, dim={X,Y}}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=1" ++
	param("PARAM_IMAGE_FILTER_WIDTH_X", X) ++
	param("PARAM_IMAGE_FILTER_WIDTH_Y", Y);
filter_params(#filter{type=gaussion, dim={X,Y}, opts=[{alpha,GA}]}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=2" ++
	param("PARAM_IMAGE_FILTER_WIDTH_X", X) ++
	param("PARAM_IMAGE_FILTER_WIDTH_Y", Y) ++
	param("PARAM_IMAGE_FILTER_GAUSSIAN_ALPHA",GA);
filter_params(#filter{type=mitchell, dim={X,Y}, opts=[{b,B}, {c,C}]}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=3" ++
	param("PARAM_IMAGE_FILTER_WIDTH_X", X) ++
	param("PARAM_IMAGE_FILTER_WIDTH_Y", Y) ++
	param("PARAM_IMAGE_FILTER_MITCHELL_B",B) ++
	param("PARAM_IMAGE_FILTER_MITCHELL_C",C).

sampler_params(#sampler{type=inlined_random}) ->
    " -D PARAM_SAMPLER_TYPE=0";    
sampler_params(#sampler{type=random}) ->
    " -D PARAM_SAMPLER_TYPE=1";
sampler_params(#sampler{type=metropolis, opts=Opts}) ->
    [{rate, Rate}, {reject, Rej}] = Opts,
    " -D PARAM_SAMPLER_TYPE=2"++
	param("PARAM_SAMPLER_METROPOLIS_LARGE_STEP_RATE", Rate) ++
	param("PARAM_SAMPLER_METROPOLIS_MAX_CONSECUTIVE_REJECT", Rej);
sampler_params(#sampler{type=stratified, opts=[{X,Y}]}) ->
        " -D PARAM_SAMPLER_TYPE=3"++
	param("PARAM_SAMPLER_STRATIFIED_X_SAMPLES", X) ++
	param("PARAM_SAMPLER_STRATIFIED_Y_SAMPLES", Y).

calc_task_size(PS, Opts) ->
    Seed = 3*?ISz,    
    InDirL = PS#ps.arealightn > 0 orelse PS#ps.sunlight /= false,
    SamplerSz = sampler_size(PS, Opts),
    PathStateSz = ?ifelse(InDirL, ?PathStateDLSz, ?PathStateSz),
    Seed + SamplerSz + PathStateSz.

sampler_size(PS = #ps{arealightn=AN, sunlight=SL, texmapalpha=TMA},
	     Opts=#ropt{max_path_depth=PathDepth, 
			sampler=#sampler{type=Type}, lens_r=CamLens}) ->
    CameraSz = ?ifelse(CamLens > 0.0, ?FSz*2, 0),
    DataEyePath = ?FSz * 2 + CameraSz, %% IDX_SCREEN_X, IDX_SCREEN_Y
    DataPerPath = 
	?ifelse(TMA /= false, ?FSz, 0) +
	?FSz * 3 + %% IDX_BSDF_X, IDX_BSDF_Y, IDX_BSDF_Z
	%% IDX_DIRECTLIGHT_X, IDX_DIRECTLIGHT_Y, IDX_DIRECTLIGHT_Z
	?ifelse(AN > 0 orelse SL /= false, ?FSz*3, 0) + 
	%% IDX_RR
	?FSz,
    DataSz = 
	case Type of
	    inlined_random -> 
		?FSz*2;
	    metropolis -> 
		?FSz*2+?ISz*5+?FSz*3+2*(DataEyePath+DataPerPath*PathDepth);
	    _ -> 
		DataEyePath+DataPerPath*PathDepth
	end,
    case Type of
	metropolis -> 
	    DataSz + ?FSz*3;
	stratified -> 
	    ?ISz + DataSz + ?FSz*3 + 
		stratified_sampler_size(Opts, PS);
	_ -> 
	    ?ISz + DataSz + ?FSz*3
    end.

stratified_sampler_size(#ropt{sampler=#sampler{type=stratified, opts={X,Y}}, 
			      lens_r=LensR}, PS) ->
    CamL = LensR > 0.0,
    InDirL = PS#ps.arealightn > 0 orelse PS#ps.sunlight /= false,
    TexA = PS#ps.texmapalpha /= false,
    %% stratifiedScreen2D
    ?FSz * X * Y * 2 +
	%% stratifiedDof2D
	?ifelse(CamL, ?FSz * X * Y * 2, 0) +
	%% stratifiedAlpha1D
	?ifelse(TexA, ?FSz * X, 0) +
	%% stratifiedBSDF2D
	?FSz * X * Y * 2 +
	%% stratifiedBSDF1D
	?FSz * X +
	%% stratifiedLight2D
	%% stratifiedLight1D
	?ifelse(InDirL, ?FSz *X*Y*2 + ?FSz *X, 0);
stratified_sampler_size(_, _) -> 0.

%% Helpers

get_sampler(Attrs) ->
    get_sampler(proplists:get_value(sampler, Attrs, metropolis), Attrs).

get_sampler(random, _Attrs) ->
    #sampler{type=random};
get_sampler(stratified, Attrs) ->
    Samples = proplists:get_value(stratified_samples, Attrs, {3,3}),
    #sampler{type=stratified, opts=[Samples]};
get_sampler(metropolis, Attrs) ->
    #sampler{type=metropolis,
	     opts=[{rate,proplists:get_value(metropolis_rate,Attrs,0.4)},
		   {reject,proplists:get_value(metropolis_reject,Attrs,512)}]}.

get_filter(Attrs) ->
    get_filter(proplists:get_value(filter, Attrs, none), Attrs).

get_filter(none, _Attrs) ->
    #filter{type=none};
get_filter(box, Attrs) ->
    Dim = proplists:get_value(filter_dim, Attrs, {1.5,1.5}),
    #filter{type=box, dim=Dim};
get_filter(gaussian, Attrs) ->
    Dim = proplists:get_value(filter_dim, Attrs, {1.5,1.5}),
    Alpha = proplists:get_value(filter_alpha, Attrs, 2.0),
    #filter{type=gaussian, dim=Dim, opts=[{alpha,Alpha}]};
get_filter(mitchell, Attrs) ->
    Dim = proplists:get_value(filter_dim, Attrs, {1.5,1.5}),
    B = proplists:get_value(filter_b, Attrs, 1/3),
    C = proplists:get_value(filter_c, Attrs, 1/3),
    #filter{type=mitchell, dim=Dim, opts=[{b,B},{c,C}]}.


