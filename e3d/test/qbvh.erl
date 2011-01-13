%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010-2011, Dan Gudmundsson
%%% @doc  Some test cases for qbvh
%%%
%%% @end
%%% Created :  9 Jun 2010-2011 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(qbvh).

-compile(export_all).
-include_lib("../e3d.hrl").
-include_lib("cl/include/cl.hrl").

-define(F32, 32/float-native).
-define(I32, 32/native).

-define(VS, {{ 0.4,  1.0, -0.4},  %1
             { 0.4,  0.0, -0.4},  %2
             {-0.4,  0.0, -0.4},
             {-0.4,  1.0, -0.4},  %4
             {-0.4,  1.0,  0.4},
             { 0.4,  1.0,  0.4},  %6
             { 0.4,  0.0,  0.4},
             {-0.4,  0.0,  0.4}}).%8

-define(FS, [[0,1,2,3],
	     [2,7,4,3],
	     [0,5,6,1],
	     [5,4,7,6],
	     [5,0,3,4],
	     [6,7,2,1]]).

-define(RAYHIT_SZ, 16).

-record(ray, 
	{o,d, 					% Origin, Direction vector
	 n, f}).				% Near far (or MinT MaxT)

-record(hit, {t, b1, b2, f = 16#ffffffff}).

go()  -> start().

start() ->
    Fs = lists:append([ [[V1,V2,V3],[V1,V3,V4]] || [V1,V2,V3,V4] <- ?FS]),
    TFs = list_to_tuple(Fs),
    format_faces(0, Fs, ?VS),
    Qbvh = e3d_qbvh:init([{tuple_size(TFs), 
			   fun(Face) -> 
				   [V1,V2,V3] = element(Face+1, TFs),
	        		   {element(V1+1,?VS), element(V2+1,?VS), element(V3+1, ?VS)}
			   end}]),
    %% {Rays, Res} = init_cl(Qbvh),
    %% check_rays(Rays, Res, array:from_list(Fs), ?VS),
    
    erl_ray_trace([e3d_qbvh:ray({0.0, 0.5, 5.0},{0.0,0.0,-1.0})], Qbvh),
    %%erl_ray_trace(Rays, Qbvh),
    ok.
  
check_rays([_Miss|Ds],  <<_:12/binary, 16#FFFFFFFF:32, Rest/binary>>, Fs, Vs) ->
    check_rays(Ds, Rest, Fs, Vs);
check_rays([#ray{d=Dir}|Ds],  <<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>, Fs, Vs) ->
    io:format("~s => ~s ~s ~s ~s~n",[f(Dir), f(T),f(B1),f(B2),f(Face)]),
    FVs = [V1,V2,V3] = array:get(Face, Fs),
    io:format("   ~p => [~s,~s,~s]~n",
	      [FVs, f(element(V1+1,Vs)),f(element(V2+1,Vs)),f(element(V3+1,Vs))]),
    check_rays(Ds, Rest, Fs, Vs);
check_rays([],<<>>, _, _) ->
    ok.

erl_ray_trace([Ray|Rays], Qbvh) ->
    case e3d_qbvh:ray_trace(Ray,Qbvh) of
	#hit{f=16#ffffffff} ->
	    io:format("E miss~n",[]),
	    ok;
	#hit{t=T, b1=B1, b2=B2, f=Face} ->
	    io:format("E ~s => ~s ~s ~s ~s~n",
		      [f(Ray#ray.d), f(T),f(B1),f(B2),f(Face)])
    end,
    erl_ray_trace(Rays, Qbvh);
erl_ray_trace([], _) ->
    ok.


format_faces(I, [FVs=[V1,V2,V3]|Fs], Vs) ->
    io:format("~p ~p => [~s,~s,~s]~n",
	      [I, FVs, f(element(V1+1,Vs)),f(element(V2+1,Vs)),f(element(V3+1,Vs))]),
    format_faces(I+1, Fs,Vs);
format_faces(_,[],_) -> ok.


f({X,Y,Z}) ->
    io_lib:format("{~.2f, ~.2f, ~.2f}", [X,Y,Z]);
f(X) when is_float(X) ->
    io_lib:format("~.2f", [X]);
f(X) when is_integer(X) ->
    io_lib:format("~p", [X]).
    
init_cl(Qbvh) ->
    CL = clu:setup(all),
    try 
	{ok,Name} = cl:get_platform_info(CL#cl.platform, name),
	io:format("Platform: ~s~n~n", [Name]),
	[Device] = CL#cl.devices,
	{ok,DeviceInfo} = cl:get_device_info(Device),
	ImageSupport = proplists:get_value(image_support, DeviceInfo, false),
	MaxWorkGroupSize = proplists:get_value(max_work_group_size, DeviceInfo, 4),
	
	%% io:format("DeviceInfo: ~p\n", [DeviceInfo])
	%%Kernel = compile(CL, "test_kernel.cl"),
	Kernel = compile(CL, "qbvh_kernel.cl"),
	{ok,Queue} = cl:create_queue(CL#cl.context,Device,[]),
	{ok,Local} = cl:get_kernel_workgroup_info(Kernel, Device, work_group_size),
	io:format("work_group_size = ~p ~p\n", [Local, MaxWorkGroupSize]),

	standard(CL#cl.context, Kernel, Queue, 64, Qbvh)
    catch 
	_Error:Reason ->
	    io:format("Error ~p~n", [Reason])
    after
	io:format("CL ~p~n",[CL]),
	clu:teardown(CL)
    end.

compile(CL, File) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(filename:join([Dir, File])),
    {ok, Program} = clu:build_source(CL, Bin),
    {ok, [Kernel]} = cl:create_kernels_in_program(Program),
    Kernel.

standard(Context, Kernel, Queue, WorkGroupSz, {_BB, QnodesBin, QtrisBin, _}) ->
    {ok, QNodes} = cl:create_buffer(Context, [read_only, copy_host_ptr], byte_size(QnodesBin), QnodesBin),
    {ok, QTris}  = cl:create_buffer(Context, [read_only, copy_host_ptr], byte_size(QtrisBin), QtrisBin),
    
    {RaysBin, Dirs, NoRays} = create_rays(),
    {ok, Rays} = cl:create_buffer(Context, [read_only],  byte_size(RaysBin), RaysBin),
    {ok, Hits} = cl:create_buffer(Context, [write_only], NoRays * ?RAYHIT_SZ),
    
    clu:apply_kernel_args(Kernel, [Rays,Hits,QNodes,QTris,NoRays, {local,24*WorkGroupSz*4}]),
    %% {ok,Event} = cl:enqueue_task(Queue, Kernel, []),
    {ok, Run} = cl:enqueue_nd_range_kernel(Queue, Kernel, 
					   [NoRays], [WorkGroupSz], []),
    {ok, Done} = cl:enqueue_read_buffer(Queue,Hits,0,  NoRays*?RAYHIT_SZ, [Run]),
    {ok, Result} = cl:wait(Done,1000),

    {Dirs, Result}.

create_rays() ->
    Origo = {0.0, 0.5, 5.0},
    MinT = 0.000005,
    MaxT = ?E3D_INFINITY, 
    Dirs = [calc_dir(Dir/255) || Dir <- lists:seq(0, 255)],
    {<< << (create_ray(Origo, Dir, MinT, MaxT))/binary >>
	|| Dir <- Dirs >>, 
     [e3d_qbvh:ray(Origo, Dir, MinT, MaxT) || Dir <- Dirs], 
     256}.
    
calc_dir(Dir) ->
    e3d_vec:norm(e3d_q:vec_rotate({1.0,0.0,0.0}, e3d_q:from_angle_axis(Dir*360, {0.0,1.0,0.0}))).

create_ray({OX,OY,OZ}, {DX,DY,DZ}, MinT, MaxT) ->
    <<OX:?F32, OY:?F32, OZ:?F32, 
      DX:?F32, DY:?F32, DZ:?F32, 
      MinT:?F32, MaxT:?F32>>.

max_float() ->
    max_float(3.000000e+38, 1.0e+37, 50000).

max_float(X, Next, N) when N > 0, Next > 1.0 ->
    Bin = <<X:?F32>>,
    Op = try 
	     <<Y:?F32>> = Bin,
	     erlang:display({Y, Next}),
	     inc
	 catch error:_ ->
		 erlang:display({fail,X,Next}),
		 dec
	 end,
    if
	Op == inc ->
	    max_float(X+Next, Next*1.2, N);
	Op == dec ->
	    max_float(X-Next, Next / 2, N-1)
    end;
max_float(X,_,_) -> 
    X.

