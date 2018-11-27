%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010-2011, Dan Gudmundsson
%%% @doc  Some test cases for qbvh
%%%
%%% @end
%%% Created :  9 Jun 2010-2011 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(ray_test).

-compile(export_all).
-include_lib("wings/e3d/e3d.hrl").
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
%-record(hit, {t, b1, b2, f = 16#ffffffff}).

go()  -> start().

start() ->
    Fs = make_model(),
    Rays = make_rays(),
    RaysBin = ray_bin(Rays),
    CL = init_cl(),

    Ts = [
	  {"BVH  bin",
	   fun() -> e3d_bvh:init(Fs, [{binary, true}]) end,
	   fun(Data) -> erl_ray_trace(Rays, e3d_bvh, Data, []) end},
	  {"BVH  erl",
	   fun() -> e3d_bvh:init(Fs, []) end,
	   fun(Data) -> erl_ray_trace(Rays, e3d_bvh, Data, []) end},
	  {"BVH cerl",
	   fun() -> e3d_bvh:init(Fs, [{no_binary, true}, {compile,true}]) end,
	   fun(Data) -> erl_ray_trace(Rays, e3d_bvh, e3d_bvh:init(Data), []) end},
	  {"BVH  OCL",
	   fun() -> e3d_bvh:init(Fs, [{binary, true}]) end,
	   fun(Data) -> Bin = run_cl(CL, bvh, RaysBin, Data), check_rays_bvh(Bin, []) end}
	 ],
    lists:foldl(fun(Test, Ref) -> run_test(Test, Ref) end, undefined, Ts),
    %%debug(0, R4, R5),
    ok.

run_test({String, Init, Test}, Reference) ->
    Me = self(),
    {T0, InitData} = timer:tc(Init),
    spawn_link(fun() -> test(Me, String, T0, InitData, Test) end),
    TestRes = receive {result, Res} -> Res end,
    case Reference of
	undefined ->
	    TestRes;
	_ ->
	    %% io:format("Check ~s: ~p ~p~n",[String, length(TestRes), verify(Reference, TestRes)]),
	    verify(Reference, TestRes, true),
	    Reference
    end.

test(Parent, String, T0, InitData, Test) ->
    % io:format("~s: ~p ~p~n",[String, Init, Test]),
    {T1, Res} = timer:tc(fun() -> Test(InitData) end),
    io:format("~s: ~.5w ~.5w ~w~n", [String, T1 div 1000, T0 div 1000, length(Res)]),
    Parent ! {result, Res}.

debug(I, [{_, #{f:=F1,b2:=S1}}|H1], [{_, #{f:=F2,b2:=S2}}|H2]) ->
    io:format("~.2w: ~p (~p) ~p(~p)~n",[I, F1,S1,F2,S2]),
    debug(I+1, H1,H2);
debug(_, [], []) ->
    ok.

erl_ray_trace([Ray|Rays], Mod, Qbvh, Acc) ->
    case Mod:ray_trace(Ray,Qbvh) of
	false -> erl_ray_trace(Rays, Mod, Qbvh, Acc);
	Hit ->   erl_ray_trace(Rays, Mod, Qbvh, [Hit|Acc])
    end;
erl_ray_trace([], _, _, Acc) ->
    Acc.

check_rays_bvh(<<_:16/binary, 16#FFFFFFFF:32, Rest/binary>>, Acc) ->
    check_rays_bvh(Rest, Acc);
check_rays_bvh(<<T:?F32,B1:?F32,B2:?F32,MeshId:?I32, Face:?I32, Rest/binary>>, Acc) ->
    check_rays_bvh(Rest, [#{t=>T,b1=>B1,b2=>B2,mesh=>MeshId,face=>Face}|Acc]);
check_rays_bvh(<<>>, Acc) ->
    Acc.

verify([#{face:=F0,t:=T0,b1:=B10,b2:=B20}=H0|VL],
       [#{face:=F1,t:=T1,b1:=B11,b2:=B21}=H1|HL], Res)->
    T  = abs(r(T0)  - r(T1)) < 2,
    B1 = abs(r(B10) - r(B11)) < 2,
    B2 = abs(r(B20) - r(B21)) < 2,
    if F0 =/= F1, not (is_tuple(F1) andalso F0 == element(2, F1)) ->
	    io:format("Id:  ~p ~p~n", [F0, F1]),
	    verify(VL, HL, false);
       T andalso B1 andalso B2 ->
	    verify(VL, HL, Res);
       T == false ->
	    io:format("T:  ~p ~p~n  ~p~n  ~p~n",[r(T0),r(T1),H0,H1]),
	    verify(VL, HL, false);
       B1 == false ->
	    io:format("B1: ~p ~p~n  ~p~n  ~p~n",[r(B10),r(B11),H0,H1]),
	    verify(VL, HL, false);
       B2 == false ->
	    io:format("B2: ~p ~p~n  ~p~n  ~p~n",[r(B20),r(B21),H0,H1]),
	    verify(VL, HL, false)
    end;
verify([],[], Res) -> Res;
verify(L1, L2, _Res) ->
    io:format("~p~n~p~n",[catch hd(L1),catch hd(L2)]),
    fale.

r(V) -> erlang:round(V*10000). %% Opencl 32b floats

make_rays() ->
    %% rand:seed(exs64, {234,123,1235231}),
    Origo = {0.0, 0.5, 5.0},
    N0 = 16384, % 1 Mega rays
    %% N0 = 1,
    N = 64*N0-1,
    [e3d_bvh:ray(Origo,calc_dir(Dir/N)) || Dir <- lists:seq(0, N)].

%% calc_dir(Dir) ->
%%     e3d_vec:norm(e3d_q:vec_rotate({1.0,0.0,-1.0}, e3d_q:from_angle_axis(Dir*90, {0.0,1.0,0.0}))).

calc_dir(_) ->
    U1 = rand:uniform(),
    U2 = rand:uniform(),
    R = math:sqrt(U1),
    Theta = 2*math:pi()*U2,
    {math:cos(Theta)*R, math:sin(Theta)*R, -math:sqrt(max(0.0, 1.0-U1))}.

make_model() ->
    %% Fs = lists:append([ [{V1,V2,V3},{V1,V3,V4}] || [V1,V2,V3,V4] <- ?FS]),
    %% TFs = list_to_tuple(Fs),
    %% %% format_faces(0, Fs, ?VS),
    %% GetFace = fun(Face) when is_integer(Face) ->
    %% 		      {V1,V2,V3} = element(Face+1, TFs),
    %% 		      {element(V1+1,?VS), element(V2+1,?VS), element(V3+1, ?VS)};
    %% 		 ({verts,Face}) -> element(Face+1, TFs);
    %% 		 (meshId) -> 0;
    %% 		 (verts) -> tuple_to_list(?VS)
    %% 	      end,
    %% [{tuple_size(TFs), GetFace}].
    {Size, Tris, _} = sphere:tris([{subd, 4}, {scale, 1.5}]),
    {Faces, Verts} = make_verts(Tris, 0, [], #{}),
    %% io:format("~p ~n", [Verts]),
    %% io:format("~p ~n", [Faces]),
    VertsL = array:from_list(tuple_to_list(Verts)),
    GetFace = fun(Face) when is_integer(Face) ->
		      Res = lists:nth(Face+1, Tris),
		      %% {V1,V2,V3} = element(Face+1, Faces),
		      %% R2 = {element(V1+1,Verts), element(V2+1,Verts), element(V3+1, Verts)},
		      %% case Res of
		      %% 	  R2 -> Res;
		      %% 	  _ -> io:format("~p~n~p~n",[Res, R2]),
		      %% 	       Res = R2
		      %% end;
		      Res;
		 ({verts, Face}) ->
		      element(Face+1, Faces);
		 (meshId) -> 0;
		 (verts) -> VertsL
	      end,
    [{Size, GetFace}].

make_verts([{V0,V1,V2}|Ts], Next0, Fs0, Vs0) ->
    {Next1, Id1,Vs1} = make_verts1(V0, Next0, Vs0),
    {Next2, Id2,Vs2} = make_verts1(V1, Next1, Vs1),
    {Next3, Id3,Vs3} = make_verts1(V2, Next2, Vs2),
    make_verts(Ts, Next3, [{Id1, Id2, Id3}|Fs0], Vs3);
make_verts([], _, Fs, Vs0) ->
    Vs = [V || {V,_} <- lists:keysort(2, maps:to_list(Vs0))],
    {list_to_tuple(lists:reverse(Fs)), list_to_tuple(Vs)}.

make_verts1(V, Next, Vs0) ->
    case maps:find(V, Vs0) of
	{ok, Id} -> {Next, Id, Vs0};
	error -> {Next+1, Next, Vs0#{V=>Next}}
    end.

format_faces(I, [FVs=[V1,V2,V3]|Fs], Vs) ->
    io:format("~p ~p => [~s,~s,~s]~n",
	      [I, FVs, f(element(V1+1,Vs)),f(element(V2+1,Vs)),f(element(V3+1,Vs))]),
    format_faces(I+1, Fs,Vs);
format_faces(_,[],_) -> ok.


f({X,Y,Z}) ->
    io_lib:format("{~.2f, ~.2f, ~.2f}", [X,Y,Z]);
f(X) when is_float(X) ->
    io_lib:format("~.3f", [X]);
f(X) when is_integer(X) ->
    io_lib:format("~p", [X]).

init_cl() ->
    CL = clu:setup(all),
    try
	%%{ok,Name} = cl:get_platform_info(CL#cl.platform, name),
	%% io:format("Platform: ~s~n~n", [Name]),
	[Device] = CL#cl.devices,
	{ok,DeviceInfo} = cl:get_device_info(Device),
	%% ImageSupport = proplists:get_value(image_support, DeviceInfo, false),
	_MaxWorkGroupSize = proplists:get_value(max_work_group_size, DeviceInfo, 4),

	%% io:format("DeviceInfo: ~p\n", [DeviceInfo])
	Bvh  = compile(CL, "bvh_kernel.cl"),
	{ok,Queue} = cl:create_queue(CL#cl.context,Device,[]),
	%%io:format("work_group_size = ~p ~p\n", [_Local, _MaxWorkGroupSize]),

	#{cl=>CL, bvh=>Bvh, q=>Queue, wgsz=>64}
    catch
	_Error:Reason ->
	    io:format("Error ~p ~p~n", [Reason, erlang:get_stacktrace()]),
	    exit(error)
    %% after
	%% io:format("CL ~p~n",[CL]),
	%% clu:teardown(CL)
    end.

compile(CL, File) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(filename:join([Dir, File])),
    {ok, Program} = clu:build_source(CL, Bin, "-D USE_LOCAL_MEM"),
    {ok, [Kernel]} = cl:create_kernels_in_program(Program),
    Kernel.

run_cl(#{cl:=CL, q:=Queue, wgsz:=WorkGroupSz}=Ps, What, RaysBin, #{ns:=QnodesBin}=Bvh) ->
    Context = CL#cl.context,
    #{What:=Kernel}= Ps,
    {ok, Nodes} = cl:create_buffer(Context, [read_only, copy_host_ptr], byte_size(QnodesBin), QnodesBin),
    MeshData =
	case What of
	    qbvh ->
		QtrisB = maps:get(ts, Bvh),
		{ok, QTris} = cl:create_buffer(Context, [read_only, copy_host_ptr],
					       byte_size(QtrisB), QtrisB),
		QTris;
	    bvh ->
		VertsB = maps:get(vs, Bvh),
		{ok, Verts} = cl:create_buffer(Context, [read_only, copy_host_ptr],
					       byte_size(VertsB), VertsB),
		Verts
	end,
    RayHitSz = rayhit_size(What),
    NoRays   = byte_size(RaysBin) div 32,
    {ok, RaysM} = cl:create_buffer(Context, [read_only],  byte_size(RaysBin), RaysBin),
    {ok, HitsM} = cl:create_buffer(Context, [write_only], NoRays * RayHitSz),
    case What of
	qbvh ->
	    clu:apply_kernel_args(Kernel, [RaysM,HitsM,Nodes,MeshData,NoRays, {local,24*WorkGroupSz*4}]);
	bvh ->
	    clu:apply_kernel_args(Kernel, [RaysM,HitsM,NoRays,MeshData,Nodes])
    end,
    %% {ok,Event} = cl:enqueue_task(Queue, Kernel, []),
    %% io:format("~p ~p~n", [NoRays, WorkGroupSz]),
    {ok, Run}  = cl:enqueue_nd_range_kernel(Queue, Kernel, [NoRays], [WorkGroupSz], []),
    {ok, Done} = cl:enqueue_read_buffer(Queue, HitsM, 0,  NoRays*RayHitSz, [Run]),
    {ok, Result} = cl:wait(Done,1000),
    Result.

ray_bin(Rays) ->
    << << (bin_ray(Ray))/binary >> || Ray <- Rays >>.

bin_ray(#ray{o={OX,OY,OZ}, d={DX,DY,DZ}, n=MinT, f=MaxT}) ->
    <<OX:?F32, OY:?F32, OZ:?F32,
      DX:?F32, DY:?F32, DZ:?F32,
      MinT:?F32, MaxT:?F32>>.

rayhit_size(qbvh) -> ?RAYHIT_SZ;
rayhit_size(bvh) -> 20.
