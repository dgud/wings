%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_pathgpu).
-export([start/2]).

-include_lib("wings/src/wings.hrl").
-include("pbr.hrl").
-include_lib("wings/e3d/e3d.hrl").

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
	  sample_fb,
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
	  %% Light buffers
	  arealightn, 
	  arealight,  
	  inflight,  
	  inflightmap,
	  sunlight,   
	  skylight,   
	  %% Texture buffers
	  texmaprgb   = false,
	  texmapalpha = false,
	  texmapdesc  = false,
	  meshtexs    = false,
	  meshbumps   = false,
	  meshbumpsscale = false,
	  uvsb = false,
	  %% Camera buffers
	  cam}).

-define(ifelse(A,B,C), if (A) -> (B); true -> (C) end).
-define(FSz, 4).
-define(ISz, 4).
-define(PathStateSz, 2*?ISz + 3*?FSz).
-define(PathStateDLSz, ?PathStateSz + ?FSz+ ?ISz + ?RAY_SZ + 2*3*?FSz).
-define(TASKSTAT_SZ, ?ISz).

start(Attrs, SceneS0) ->
    SceneS = SceneS0, %#renderer{force_wg=64},
    ROpt = #ropt{
      max_path_depth = proplists:get_value(max_path_depth, Attrs, 5),
      rr_depth       = proplists:get_value(rr_depth, Attrs, 3),
      rr_imp_cap     = proplists:get_value(rr_imp_cap, Attrs, 0.125),
      sampler        = get_sampler(Attrs),
      filter         = get_filter(Attrs),
      lens_r         = (proplists:get_value(aperture, Attrs, 0.0) / 2.0),
      refresh        = (proplists:get_value(refresh_interval, Attrs, 1)) * 1000
     },
    start_processes(ROpt, SceneS).

start_processes(Ropt, SceneS0) ->
    %% NoThreads = erlang:system_info(schedulers),
    random:seed(now()),
    {SceneS, PS, SAs} = init_render(1, random:uniform(1 bsl 32), 0.00, Ropt, SceneS0),
    render(SceneS, PS, Ropt, SAs).

render(SceneS = #renderer{cl=CL}, 
       PS=#ps{sample_fb=FB, rays=RaysB, hits=HitsB}, 
       #ropt{refresh=RI}, IAs) ->
    io:format("Initilizes buffers~n",[]),
    {X,Y} = pbr_film:resolution(SceneS),    
    wings_cl:cast('InitFrameBuffer', [FB], X*Y, [], CL),
    W1 = wings_cl:cast('Init', IAs, ?TASK_SIZE, [], CL),
    {Qn,Qt,LocalMem} = pbr_scene:intersect_data(SceneS),
    IArgs = [RaysB, HitsB, Qn, Qt, ?MAX_RAYS, LocalMem],
    wings_cl:set_args('Intersect', IArgs, CL),
    {ok, completed} = cl:wait(W1),
    io:format("Starting rendering~n",[]),
    Start = os:timestamp(),
    erlang:send_after(RI, self(), refresh),
    ok = cl:finish(wings_cl:get_queue(CL)),
    Res = render_loop(10, 0, CL, {FB, RI, PS, SceneS}),
    Time = timer:now_diff(os:timestamp(),Start),
    io:format("Rendered in: ~s #rays:  ~wK/secs ~n",
	      [format_time(Time), (Res*?TASK_SIZE) div (Time div 1000)]),
    normal.

render_loop(N, C, CL, Data) when N > 0 ->
    wings_cl:cast('Sampler', ?TASK_SIZE, nowait, CL),
    wings_cl:cast('Intersect', ?TASK_SIZE, nowait, CL),
    wings_cl:cast('AdvancePaths', ?TASK_SIZE, nowait, CL),
    render_loop(N-1, C, CL, Data);
render_loop(0, C0, CL, Data = {FB, RI, _PS, SceneS0}) ->
    %% Intel drivers need a sync here for some reason
    wings_cl:cast('Sampler', ?TASK_SIZE, nowait, CL),
    wings_cl:cast('Intersect', ?TASK_SIZE, nowait, CL),
    Wait = wings_cl:cast('AdvancePaths', ?TASK_SIZE, [], CL),
    cl:wait(Wait),
    Count = C0+10,
    receive 
	refresh ->
	    SceneS1 = update_film(FB, SceneS0),
	    erlang:send_after(RI, self(), refresh),
	    render_loop(10, Count, CL, {FB,RI,_PS,SceneS1});
	stop -> 
	    update_film(FB, SceneS0),
	    Count;
	Msg ->
	    io:format("Renderer got Msg ~p~n",[Msg]),
	    render_loop(10, Count, CL, Data)
    after 0 ->
	    render_loop(10, Count, CL, Data)
    end.

update_film(FB, SceneS) ->
    Scene = pbr_film:set_sample_frame_buffer(FB, SceneS),
    pbr_film:show(Scene),
    Scene.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_render(_Id, Seed, Start, Opts, SceneS0) ->
    PS1 = #ps{},
    io:format("Creating data~n",[]),
    MeshMatBuffs = pbr_scene:mesh2mat(SceneS0),
    PS2 = create_scene_buffs(PS1, MeshMatBuffs, SceneS0),
    PS3 = create_light_buffs(PS2, SceneS0),
    PS4 = create_tex_buffs(PS3, MeshMatBuffs, SceneS0),
    CamBin = pbr_camera:pack_camera(Opts#ropt.lens_r, SceneS0), 
    PS5 = PS4#ps{cam=wings_cl:buff(CamBin, SceneS0#renderer.cl)},
    PS  = create_work_buffs(PS5, calc_task_size(PS5, Opts), SceneS0), 
    io:format("Compiling OpenCL code~n",[]),
    CompilerParams = create_params(Seed, Start, MeshMatBuffs, Opts, PS, SceneS0),
    SceneS = ?TC(compile(SceneS0, CompilerParams, Opts, PS)),
    SamplerArgs = sampler_args(PS, Opts, SceneS),
    AdvancePathsArgs = advance_paths_args(PS), 
    wings_cl:set_args('Sampler', SamplerArgs, SceneS#renderer.cl),
    io:format("~p:~p: MemObjs ~p~n", 
	      [?MODULE,?LINE, [element(2,cl:get_mem_object_info(E, size)) || 
				  E <- AdvancePathsArgs]]),
    wings_cl:set_args('AdvancePaths', AdvancePathsArgs, SceneS#renderer.cl),
    {SceneS, PS, SamplerArgs}.

create_work_buffs(PS, TaskSize, SceneS = #renderer{cl=CL}) -> 
    {X,Y} = pbr_film:resolution(SceneS),
    %% Work areas
    PS#ps{rays        = wings_cl:buff(?RAYBUFFER_SZ,CL),
	  hits        = wings_cl:buff(?RAYHIT_SZ*?MAX_RAYS,CL),
	  sample_fb   = wings_cl:buff(?SAMPLE_PIXEL_SZ*X*Y,CL),
	  task        = wings_cl:buff(TaskSize*?TASK_SIZE,CL),
	  taskstats   = wings_cl:buff(?TASKSTAT_SZ * ?TASK_SIZE,CL)}.

create_scene_buffs(PS, {Face2Mesh, Mesh2Mat, _, Mats}, SceneS = #renderer{cl=CL}) ->
    %% Static Scene buffers
    MatBuff = pbr_mat:pack_materials(Mats, pbr_scene:get_materials(SceneS)),
    VCs = pbr_scene:vertex_colors(SceneS),
    Ns  = pbr_scene:normals(SceneS),
    Ts  = pbr_scene:triangles(SceneS),
    Vs  = pbr_scene:vertices(SceneS),
    io:format("Materials   ~w ~wb~n", [size(MatBuff) div 52, size(MatBuff) ]),
    io:format("Mesh2Mat    ~w ~wb~n", [size(Mesh2Mat) div 4, size(Mesh2Mat)]),
    io:format("Meshids F2M ~w ~wb~n", [size(Face2Mesh) div 4, size(Face2Mesh)]),
    io:format("color       ~w ~wb~n", [size(VCs) div 36, size(VCs)]),
    io:format("normals     ~w ~wb~n", [size(Ns)  div 36, size(Ns)]),
    io:format("vertices    ~w ~wb~n", [size(Vs)  div 36, size(Vs)]),
    io:format("triangles   ~w ~wb~n", [size(Ts)  div 12, size(Ts)]),
    
    PS#ps{meshids   = wings_cl:buff(Face2Mesh, CL),
	  mesh2mat  = wings_cl:buff(Mesh2Mat, CL),
	  mats      = wings_cl:buff(MatBuff, CL),
	  colors    = wings_cl:buff(VCs, CL),
	  normals   = wings_cl:buff(Ns, CL),
	  triangles = wings_cl:buff(Ts, CL),
	  vertices  = wings_cl:buff(Vs, CL)}.

create_light_buffs(PS0, SceneS = #renderer{cl=CL}) ->    
    %% Lights
    Lights = pbr_scene:get_lights(SceneS),
    {ALN, AreaLight} = pbr_light:pack_arealights(Lights),
    PS = PS0#ps{arealightn  = ALN,
		arealight   = opt_buff(AreaLight,CL), 
		inflight    = false,
		inflightmap = false,
		sunlight    = opt_buff(pbr_light:pack_light(sunlight,Lights),CL),
		skylight    = opt_buff(pbr_light:pack_light(skylight,Lights),CL)},
    %% Assert there is some lights
    PS#ps.sunlight /= false orelse PS#ps.skylight /= false orelse 
	PS#ps.arealight /= false orelse PS#ps.inflightmap /= false
	orelse exit(no_light),
    ALN > 0 andalso io:format("Area lights: ~w (~wb)~n", 
			      [ALN, byte_size(AreaLight)]),
    PS.

create_tex_buffs(PS, {_, _, Mesh2Mat, _}, SceneS = #renderer{cl=CL}) ->
    %% io:format("Mats ~w~n",[Mesh2Mat]),
    Materials = pbr_scene:get_materials(SceneS),
    case pbr_mat:mesh2tex(Mesh2Mat, Materials) of
	{_, _, _, []} -> PS;
	{M2Tex, M2Bumps, M2BScale, Textures} ->
	    {TexRGB, TexAlpha, TexDesc} = pbr_mat:pack_textures(Textures),
	    PS#ps{texmaprgb      = wings_cl:buff(TexRGB, CL),
		  texmapalpha    = opt_buff(TexAlpha, CL),
		  texmapdesc     = wings_cl:buff(TexDesc, CL),
		  meshtexs       = opt_buff(M2Tex, CL),
		  meshbumps      = opt_buff(M2Bumps, CL),
		  meshbumpsscale = opt_buff(M2BScale, CL),
		  uvsb           = wings_cl:buff(pbr_scene:uvs(SceneS), CL)}
    end.

opt_buff(Buff, CL) when is_binary(Buff) -> 
    wings_cl:buff(Buff, CL);
opt_buff(_, _) -> 
    false.

create_params(Seed, Start, {_,_,_,Materials}, Opt, PS, SceneS) ->
    {X,Y} = pbr_film:resolution(SceneS),
    StartLine = trunc(Start*Y),
    Ps = [param("PARAM_STARTLINE", StartLine),
	  param("PARAM_TASK_COUNT", ?TASK_SIZE),
	  param("PARAM_IMAGE_WIDTH",  X),
	  param("PARAM_IMAGE_HEIGHT", Y),
	  param("PARAM_RAY_EPSILON", ?RAY_EPS),
	  param("PARAM_SEED", Seed),
	  param("PARAM_MAX_PATH_DEPTH", Opt#ropt.max_path_depth),
	  param("PARAM_RR_CAP", Opt#ropt.rr_imp_cap),
	  param("PARAM_RR_DEPTH",  Opt#ropt.rr_depth)
	 ],
    Mats  = pbr_scene:get_materials(SceneS),
    MatPs = lists:usort([mat_param(pbr_mat:type(Mat,Mats)) || Mat <- Materials]),
    MatPs == [] andalso exit(no_materials),
    CamLens = Opt#ropt.lens_r > 0.0 ,  
    CamPs = if CamLens -> " -D PARAM_CAMERA_HAS_DOF";
	       true -> []
	    end,
    LightPs = light_params(PS),
    TexPs = tex_params(PS),
    
    FilterPs = filter_params(Opt#ropt.filter),
    %% PixelAtomics = " -D PARAM_USE_PIXEL_ATOMICS";
    SamplerPs = sampler_params(Opt#ropt.sampler),
    %% Host and OpenCL specific addons fixme
    HostOpts = host_params(SceneS#renderer.cl),
    lists:flatten([Ps, MatPs, CamPs, LightPs, TexPs, 
		   FilterPs, SamplerPs, HostOpts]).

sampler_args(PS=#ps{task=TaskB, taskstats=TaskStatsB, rays=RaysB, cam=CamB},
	     Opts, #renderer{cl=CL}) ->
    case (Opts#ropt.sampler)#sampler.type of
	stratified ->
	    SSSz = stratified_sampler_size(Opts, PS),
	    Args0 = [{local, SSSz * wings_cl:get_wg_sz('Sampler',CL)}],
	    [TaskB, TaskStatsB, RaysB, CamB | Args0];
	_ -> [TaskB, TaskStatsB, RaysB, CamB]
    end.

advance_paths_args(#ps{task=TaskB, rays=RaysB, hits=HitsB,
		       cam=CamB, sample_fb=FrameBufferB,
		       mats=MaterialsB, mesh2mat=Mesh2MatB, meshids=MeshIdsB,
		       colors=ColorsB, normals=NormalsB, 
		       vertices=VerticesB, triangles=TrianglesB, 
		       %% Optional
		       inflight=InfLightB, inflightmap=InfLightMapB, 
		       sunlight=SunLightB, skylight=SkyLightB, 
		       arealight=AreaLightB,

		       texmaprgb=TexMapRGBB, texmapalpha=TexMapAlphaB,
		       texmapdesc=TexMapDescB,  meshtexs=MeshTexsB,
		       meshbumps=MeshBumpsB,  meshbumpsscale=MeshBumpsScaleB,
		       uvsb=UvsB
		      }) ->
    %% Note order is very important
    Optional = [InfLightB, InfLightMapB, SunLightB, SkyLightB, AreaLightB,
		TexMapRGBB, TexMapAlphaB, TexMapDescB, MeshTexsB,
		MeshBumpsB, MeshBumpsScaleB, UvsB],
    
    [TaskB, RaysB, HitsB, FrameBufferB, 
     MaterialsB, Mesh2MatB, MeshIdsB,
     %% TriangleIdBuff, MeshDescBuff  %% mqbvh only
     ColorsB, NormalsB, VerticesB, TrianglesB, CamB 
     | [ Buff || Buff <- Optional, Buff /= false]].

compile(RS=#renderer{cl=CL0, force_wg=ForceWg}, Params, 
	Opts=#ropt{sampler=Sampler}, PS) -> 
    Fs = ["pbr/pathgpu2_kernel_datatypes.cl", 
	  "pbr/pathgpu2_kernel_core.cl",
	  "pbr/pathgpu2_kernel_filters.cl",
	  "pbr/pathgpu2_kernel_scene.cl",
	  "pbr/pathgpu2_kernel_samplers.cl",
	  "pbr/pathgpu2_kernels.cl"],
    io:format("Defines: ~p~n",[Params]),
    CL1 = wings_cl:compile(Fs, Params, CL0),
    Ks = ['Init', 'InitFrameBuffer', 'AdvancePaths', 'Sampler'],
    CL3 = if ForceWg > 0 ->
		  lists:foldl(fun(Kernel, State) ->
				      wings_cl:set_wg_sz(Kernel, ForceWg, State)
			      end, CL1, Ks);
	     true -> 
		  case Sampler of
		      #sampler{type=stratified} ->
			  SSSz  = stratified_sampler_size(Opts, PS),
			  InitMax = wings_cl:get_lmem_sz('Init', CL1),
			  InitSz = decrease_wg(wings_cl:get_wg_sz('Init',CL1), 
					       SSSz, InitMax),
			  CL2 = wings_cl:set_wg_sz('Init', InitSz, CL1),
			  SamplerMax = wings_cl:get_lmem_sz('Sampler', CL2),
			  SamplerSz = decrease_wg(wings_cl:get_wg_sz('Init',CL1), 
						  SSSz, SamplerMax),
			  wings_cl:set_wg_sz('Sampler', SamplerSz, CL1);
		      _ -> 
			  CL1
		  end
	  end,
    %% Can not trust the wg sizes (on at least) Nvidia when TASK_SIZE > 512
    %% I get: CL_OUT_OF_RESOURCES error waiting for idle on GeForce
    %% Tweak them:
    MaxWGs = [ {'Intersect', 256}, 
	       {'AdvancePaths', 128}, 
	       {'Sampler', 2048}],
    CL = lists:foldl(fun({Kernel, MaxWg}, State) ->
			     Curr = wings_cl:get_wg_sz(Kernel, State),
			     wings_cl:set_wg_sz(Kernel, min(MaxWg, Curr), State)
		     end, CL3, MaxWGs),
    
    io:format("Kernel:         WorkGroupSize: ~n",[]),
    [io:format("~-15s ~5w~n", [atom_to_list(Kernel),
			       wings_cl:get_wg_sz(Kernel, CL)])
     || Kernel <- ['Intersect'|Ks]],
    RS#renderer{cl=CL}.

decrease_wg(WG, Size, Max) when WG > 64 ->
    case WG*Size > Max of
	true -> decrease_wg(WG div 2, Size, Max);
	false -> WG
    end;
decrease_wg(WG, _, _) when WG >= 64 -> WG;
decrease_wg(WG, _, _) ->
    io:format("Not enough local memory to run,"
	      "try to reduce stratified xsamples and ysamples values"),
    io:format("Or set cl_max_workgroup_sz to ~p or lower", [WG]),
    exit(out_of_local_mem).

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

light_params(#ps{skylight=SkyLightB, sunlight=SunLightB, 
		 arealightn=AreaLightN, inflight=InfLight}) ->
    [?ifelse(InfLight /= false, " -D PARAM_HAS_INFINITELIGHT", []),
     ?ifelse(SkyLightB /= false, " -D PARAM_HAS_SKYLIGHT", []),
     if SunLightB /= false, AreaLightN == 0 ->
	     " -D PARAM_HAS_SUNLIGHT " ++
		 " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 " -D PARAM_DL_LIGHT_COUNT=0";	     
	SunLightB, AreaLightN > 0 ->
	     " -D PARAM_HAS_SUNLIGHT" ++ 
		 " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 param("PARAM_DL_LIGHT_COUNT", AreaLightN);
	AreaLightN > 0 ->
	     " -D PARAM_DIRECT_LIGHT_SAMPLING" ++
		 param("PARAM_DL_LIGHT_COUNT", AreaLightN);
	true -> []
     end].

tex_params(#ps{texmaprgb=RGB,texmapalpha=A,meshbumps=Bump}) ->
    [?ifelse(RGB /= false, " -D PARAM_HAS_TEXTUREMAPS", []),
     ?ifelse(A /= false, " -D PARAM_HAS_ALPHA_TEXTUREMAPS", []),
     ?ifelse(Bump /= false, " -D PARAM_HAS_BUMPMAPS", [])].
    
filter_params(#filter{type=none}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=0";
filter_params(#filter{type=box, dim={X,Y}}) ->
    " -D PARAM_IMAGE_FILTER_TYPE=1" ++
	param("PARAM_IMAGE_FILTER_WIDTH_X", X) ++
	param("PARAM_IMAGE_FILTER_WIDTH_Y", Y);
filter_params(#filter{type=gaussian, dim={X,Y}, opts=[{alpha,GA}]}) ->
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

host_params(CL) ->
    HostOpts0 = case os:type() of
		    {unix, darwin} -> [" -D __APPLE__"];
		    _ -> []
		end,
    case wings_cl:get_vendor(CL) of
	"NVIDIA" ++ _ ->             HostOpts0;
	"Advanced Micro Dev" ++ _ -> [" -fno-alias"|HostOpts0];
	_ ->                 	     HostOpts0
    end.


calc_task_size(PS, Opts) ->
    Seed = 3*?ISz,    
    InDirL = PS#ps.arealightn > 0 orelse PS#ps.sunlight /= false,
    SamplerSz = sampler_size(PS, Opts),
    PathStateSz = ?ifelse(InDirL, ?PathStateDLSz, ?PathStateSz),
    TaskSize = Seed + SamplerSz + PathStateSz,
    io:format("TaskSize ~p + ~p + ~p = ~p => ~p*~p = ~p~n",
	      [Seed,SamplerSz,PathStateSz,TaskSize,
	       TaskSize,?TASK_SIZE,TaskSize*?TASK_SIZE]),
    TaskSize.

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
    DataSz = case Type of
		 inlined_random -> ?FSz*2;
		 metropolis -> ?FSz*2+?ISz*5+?FSz*3+2*
				   (DataEyePath+DataPerPath*PathDepth);
		 _ -> DataEyePath+DataPerPath*PathDepth
	     end,
    case Type of
	metropolis -> DataSz + ?FSz*3;
	stratified -> ?ISz + DataSz + ?FSz*3 + stratified_sampler_size(Opts, PS);
	_ ->          ?ISz + DataSz + ?FSz*3
    end.

stratified_sampler_size(#ropt{sampler=_S=#sampler{type=stratified, opts=[{X,Y}]},
			      lens_r=LensR}, PS) ->
    CamL = LensR > 0.0,
    InDirL = PS#ps.arealightn > 0 orelse PS#ps.sunlight /= false,
    TexA = PS#ps.texmapalpha /= false,
    %% stratifiedScreen2D
    Size = ?FSz * X * Y * 2 +
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
	?ifelse(InDirL, ?FSz *X*Y*2 + ?FSz *X, 0),
    Size;
stratified_sampler_size(_, _) -> 0.

%% Helpers

get_sampler(Attrs) ->
    get_sampler(proplists:get_value(sampler, Attrs, inlined_random), Attrs).

get_sampler(inlined_random, _Attrs) ->
    #sampler{type=inlined_random};
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

format_time(Micro) ->
    Units = [{"us", 1000},{"ms", 1000},{"sec", 60}, {"min", 60}, {"hour", 1000}],
    format_time(Micro, [], Units).

format_time(T, Acc, [{Unit,Len}|Units]) when T > 0 ->
    Str = io_lib:format("~p~s ",[T rem Len, Unit]),
    format_time(T div Len, [Str|Acc], Units);
format_time(_, [Acc1,Acc2,_,_|_], _) ->
    [Acc1,Acc2];
format_time(_, [Acc1|_], _) ->
    [Acc1];
format_time(_, [], _) -> "0ms".


%%%%%%%%%%% Debug

-ifdef(DEBUG).

-record(task, {seed, sample, pathstate}).
-record(sample, {rad, 
		 i, 
		 u, %% inlined_random
		 rest}).
-record(paths, {s, d, tp}).

debug_rays({_,_,_,#ps{rays=RaysB, hits=HitsB, task=TaskB}, SceneS0}, Wait, CL) ->
    W1 = wings_cl:read(RaysB, ?RAYBUFFER_SZ, Wait, CL),
    {ok, RaysBin} = cl:wait(W1),
    W2 = wings_cl:read(TaskB, 56*?TASK_SIZE, [], CL),
    {ok, TaskBin} = cl:wait(W2),
    Tasks = bin2tasks56(TaskBin),
%%    [io:format("~s~n", [w_task(T)]) || T <- Tasks],
    Rays = bin2rays(RaysBin),
    #renderer{scene=Scene} = SceneS0,
    QBVH = element(size(Scene), Scene),
    D = fun(R,N) ->
		Hit = e3d_qbvh:ray_trace(R, QBVH),
		Task = #task{sample=#sample{i=I,u=[U1,U2]}}= lists:nth(N,Tasks),
		io:format("CL ~s: ~s => ~s~n", [w_task(Task), w_ray(R), w_hit(Hit)]),
		Ray = pbr_camera:generate_ray(SceneS0, float(I rem 256)+U1-0.5, 
					      256 - (I / 256)-1+U2-0.5),
		Hit2 = e3d_qbvh:ray_trace(Ray, QBVH),
		io:format("MY ~p(~p,~p): ~s ~s~n", 
			  [I, I rem 256, 256-(I div 256)-1, w_ray(Ray), w_hit(Hit2)]),
		N+1
	end,
    %% {R0,_} = lists:split(10, Rays),
    lists:foldl(D, 1, Rays),
    exit(foo),
    ok.


w_ray(#ray{o={OX,OY,OZ},d={DX,DY,DZ},n=Min,f=Max}) ->
    io_lib:format("o{~.2g,~.2g,~.2g} d{~.2g,~.2g,~.2g} n=~.2g f=~.2g",
		  [OX,OY,OZ,DX,DY,DZ,Min,Max]).
w_hit(#hit{t=Dist,f=Face}) when Dist < ?E3D_INFINITY ->
    io_lib:format("=> ~p ~.2g", [Face,Dist]);
w_hit(#hit{}) ->
    io_lib:format("=> miss", []).

w_task(#task{sample=#sample{i=I,u=[U1,U2|_]}, pathstate=#paths{s=PS,d=PD}}) ->
    %%io_lib:format("Task ~p(~p) ~p(~.2g,~.2g)", [PS,PD,I,U1,U2]).
    io_lib:format("~p(~p,~p)", [I,I rem 256, 256 - (I div 256) -1]).
    
bin2tasks56(TasksBin) when is_binary(TasksBin) ->
    0 = byte_size(TasksBin) rem 56,
    [#task{seed={S1,S2,S3},
	   sample=#sample{rad={SR,SG,SB}, i=SI, u=[U1,U2]},
	   pathstate=#paths{s=PS, d=PD, tp={PR,PG,PB}}}
     || <<S1:?UI32,S2:?UI32,S3:?UI32,  %% Seed
	  SR:?F32, SG:?F32, SB:?F32,   %% Sampler data
	  SI:?UI32, U1:?F32, U2:?F32,
	  PS:?UI32, PD:?UI32,          %% PathState
	  PR:?F32, PG:?F32, PB:?F32>> <= TasksBin].

bin2rays(RaysBin) when is_binary(RaysBin) ->
    0 = byte_size(RaysBin) rem (8*4),
    [#ray{o={OX,OY,OZ},d={DX,DY,DZ},n=Min,f=Max} ||
	<<OX:?F32, OY:?F32, OZ:?F32,
	  DX:?F32, DY:?F32, DZ:?F32,
	  Min:?F32, Max:?F32>> <= RaysBin].

rays2bin(Rays) ->
    << << OX:?F32, OY:?F32, OZ:?F32, 
	  DX:?F32, DY:?F32, DZ:?F32, 
	  N:?F32,  F:?F32
       >> || #ray{o={OX,OY,OZ},d={DX,DY,DZ},n=N,f=F} <- Rays >>.

-endif.
