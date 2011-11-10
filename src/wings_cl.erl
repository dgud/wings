%%
%%  wings_cc_ref.erl --
%%
%%     This module contains helper functions for OpenCL.
%% 
%%  Copyright (c) 2010-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_cl).
-include_lib("cl/include/cl.hrl").

-export([is_available/0,
	 setup/0, compile/2, compile/3,
	 %% Queries
	 get_context/1, get_device/1, get_queue/1, get_vendor/1,
	 have_image_support/1,

	 buff/2, buff/3, image/4, image/5, write/3, read/4,
	 cast/4, cast/5, tcast/4, tcast/5, set_args/3,
	 get_wg_sz/2, set_wg_sz/3,
	 get_lmem_sz/2
	]).

-record(cli, {context, kernels=[], q, cl, device}).
-record(kernel, {name, id, wg}).

is_available() ->
    try 
	true == erlang:system_info(smp_support) orelse throw({error, no_smp_support}),
	ok == cl:start() orelse throw({error, no_opencl_loaded}),
	{ok, Ps} = cl:get_platform_ids(),
	[] /= Ps
    catch _:Reason ->
	    io:format("OpenCL not available ~p ~n",[Reason]),
	    false
    end.


%% setup() -> cli().
setup() ->
    Prefered = wings_pref:get_value(cl_type, gpu),
    Other = [gpu,cpu] -- [Prefered],
    CL = case clu:setup(Prefered) of 
	     {error, _} -> 
		 case clu:setup(Other) of
		     {error, R} -> 
			 exit({no_opencl_device, R});
		     Cpu -> Cpu
		 end;
	     Gpu ->
		 Gpu
	 end,
    [Device|_] = CL#cl.devices,
    {ok,Queue} = cl:create_queue(CL#cl.context,Device,[]),
    #cli{context=CL#cl.context, q=Queue, device=Device, cl=CL}.

%% compile(File,cli()) -> cli().
%%
compile(File = [A|_], CLI) when is_integer(A) ->
    compile_1([File], "", CLI);
compile(Files, CLI) ->
    compile_1(Files, "", CLI).

compile(File = [A|_], Defs, CLI) when is_integer(A) ->
    compile_1([File], Defs, CLI);
compile(Files, Defs, CLI) ->
    compile_1(Files, Defs, CLI).

compile_1(Files, Defs, CLI = #cli{cl=CL, device=Device, kernels=Kernels0}) ->
    Dir = filename:join(code:lib_dir(wings),"shaders"),
    Bins = lists:map(fun(File) ->
			     AbsFile = filename:join([Dir, File]),
			     case file:read_file(AbsFile) of
				 {ok, Bin} -> {AbsFile, Bin};
				 {error, Reason} ->
				     error({error,{Reason,AbsFile}})
			     end
		     end, Files),
    {ok, Program} = build_source(CL, Bins, Defs),
    {ok, MaxWGS} = cl:get_device_info(Device, max_work_group_size),
    {ok, KernelsIds} = cl:create_kernels_in_program(Program),
    Kernels = [kernel_info(K,Device, MaxWGS) || K <- KernelsIds],
    cl:release_program(Program),
    CLI#cli{kernels=Kernels++Kernels0}.

build_source(E, Sources, Defines) ->
    Source = [Bin || {_, Bin} <- Sources],
    {ok,Program} = cl:create_program_with_source(E#cl.context,Source),
    case cl:build_program(Program, E#cl.devices, Defines) of
	ok ->
	    Status = [{Dev, cl:get_program_build_info(Program, Dev, status)}
		      || Dev <- E#cl.devices],
	    case lists:filter(fun({_, {ok, success}}) -> false;
				 (_) -> true end, Status)
	    of
		[] ->
		    {ok,Program};
		Errs ->
		    ErrDevs = [Dev || {Dev, _} <- Errs],
		    display_error(?LINE, Program, Sources, Defines, ErrDevs)
	    end;
	_Error ->
	    display_error(?LINE, Program, Sources, Defines, E#cl.devices)
    end.



display_error(Line, Program, Sources, Defines, DeviceList) ->
    SFs = [S || {S,_} <- Sources],
    io:format("~n~p:~p: Error in source file(s):~n",[?MODULE, Line]),
    [io:format(" ~s~n",[Source]) || Source <- SFs],
    lists:map(fun(Device) ->
		      {ok, DevName} = cl:get_device_info(Device, name),
		      io:format("Device: ~s~n",[DevName]),
		      {ok,Log} = cl:get_program_build_info(Program,Device,log),
		      io:format("~s~n",[Log])
	      end, DeviceList),
    io:format("~n",[]),
    DbgOutDir = filename:dirname(element(1, hd(Sources))),
    {ok, Fd} = file:open(filename:join(DbgOutDir, "cl_compilation_fail.cl"), [write]),
    Write = fun({File, Source}) ->
		    io:format(Fd, "// ****************************~n", []),
		    io:format(Fd, "// Start ~s~n", [File]),
		    io:put_chars(Fd, Source),
		    io:format(Fd, "// End ~s~n", [File]),
		    io:format(Fd, "// ****************************~n", [])
	    end,
    [Write(S) || S <- Sources],
    file:close(Fd),
    {ok, Fd1} = file:open(filename:join(DbgOutDir, "cl_compilation_fail.config"), [write]),
    io:format(Fd1, "~s~n",[Defines]),
    file:close(Fd1),
    io:format("Debug written to: ~s ~n", [filename:join(DbgOutDir, "cl_compilation_fail.cl")]),
    exit({error, build_program_failure}).


kernel_info(K,Device,MaxWGS) ->
    {ok, WG} = cl:get_kernel_workgroup_info(K, Device, work_group_size),
    {ok, CWG} = cl:get_kernel_workgroup_info(K, Device, compile_work_group_size),
    {ok, Name} = cl:get_kernel_info(K, function_name),
    %% io:format("~s WG sizes ~p ~p~n", [Name, WG, WG1]),
    case CWG of
	[0,0,0] ->
	    #kernel{name=list_to_atom(Name), wg=min(WG,MaxWGS), id=K};
	[Max,1,1] ->
	    #kernel{name=list_to_atom(Name), wg=min(Max,MaxWGS), id=K};
	MaxD ->
	    #kernel{name=list_to_atom(Name), wg=MaxD, id=K}
    end.

get_context(#cli{context=Context}) ->
    Context.
get_device(#cli{device=Device}) ->
    Device.
get_queue(#cli{q=Q}) ->
    Q.

have_image_support(#cli{device=Dev}) ->
    {ok, Bool} = cl:get_device_info(Dev, image_support),
    Bool.

get_vendor(#cli{device=ClDev}) ->
    {ok, ClPlat} = cl:get_device_info(ClDev, platform),
    {ok, Vendor} = cl:get_platform_info(ClPlat, vendor),
    Vendor.

set_args(Name, Args, #cli{kernels=Ks}) ->
    #kernel{id=K} = lists:keyfind(Name, 2, Ks),
    set_args_1(Name, K, Args).

get_lmem_sz(Name, #cli{kernels=Ks, device=Device}) ->
    #kernel{id=Kernel} = lists:keyfind(Name, 2, Ks),
    {ok,Mem} = cl:get_kernel_workgroup_info(Kernel, Device, local_mem_size),
    Mem.

get_wg_sz(Name, #cli{kernels=Ks}) ->
    #kernel{wg=Wg} = lists:keyfind(Name, 2, Ks),
    Wg.

set_wg_sz(Name, Wg, CL=#cli{kernels=Ks0}) ->
    K  = lists:keyfind(Name, 2, Ks0),
    Ks = lists:keyreplace(Name, 2, Ks0, K#kernel{wg=Wg}),
    CL#cli{kernels=Ks}.

%% cast(Kernel, Args, NoInvocations, [Wait], cli()) -> Wait
tcast(Name, No, Wait, #cli{q=Q, kernels=Ks}) ->
    Kernel = lists:keyfind(Name, 2, Ks),
    cast(Name, No, Wait, true, Q, Kernel).
cast(Name, No, Wait, #cli{q=Q, kernels=Ks}) ->
    Kernel = lists:keyfind(Name, 2, Ks),
    cast(Name, No, Wait, false, Q, Kernel).

tcast(Name, Args, No, Wait, #cli{q=Q, kernels=Ks}) ->
    Kernel = #kernel{id=K} = lists:keyfind(Name, 2, Ks),
    set_args_1(Name, K, Args),
    cast(Name, No, Wait, true, Q, Kernel).
cast(Name, Args, No, Wait, #cli{q=Q, kernels=Ks}) ->
    Kernel = #kernel{id=K} = lists:keyfind(Name, 2, Ks),
    set_args_1(Name, K, Args),
    cast(Name, No, Wait, false, Q, Kernel).

cast(Name, No, Wait, Time, Q, Kernel) ->
    Event = enqueue_kernel(No, Wait, Q, Kernel),
    Time andalso time_wait(Name, Q, Event),
    Event.

buff(Sz, CL)
  when is_integer(Sz) ->
    buff(Sz, [read_write], CL);
buff(Bin, CL)
  when is_binary(Bin) ->
    buff(Bin, [read_only, copy_host_ptr], CL).

buff(Sz, Type, #cli{context=Context})
  when is_integer(Sz) ->
    {ok, Buff} = cl:create_buffer(Context, Type, Sz),
    Buff;
buff(Bin, Type, #cli{context=Context})
  when is_binary(Bin) ->
    {ok, Buff} = cl:create_buffer(Context, Type, byte_size(Bin), Bin),
    Buff.

image(Bin, Dim, Format, CL) ->
    image(Bin, Dim, Format, [read_only, copy_host_ptr], CL).
image(Bin, {W,H}, Format = {_,_}, Alloc, #cli{context=Context})
  when is_binary(Bin) ->
    {ok, Buff} = cl:create_image2d(Context, Alloc, Format, W, H, 0, Bin),
    Buff.

%% write(CLMem, Bin, cli()) -> Wait
write(CLMem, Bin, #cli{q=Q}) ->
    {ok, W1} = cl:enqueue_write_buffer(Q, CLMem, 0, byte_size(Bin), Bin, []),
    W1.

%% read(CLMem, Sz, [Wait], cli()) -> Wait
read(CLMem, Sz, Wait, #cli{q=Q}) ->
    {ok, W} = cl:enqueue_read_buffer(Q,CLMem,0,Sz, Wait),
    W.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_args_1(Name, K, Args) ->
    try clu:apply_kernel_args(K, Args) of
	ok -> ok
    catch error:Reason ->
	    io:format("Bad args ~p: ~p~n",[Name, Args]),
	    erlang:raise(error,Reason, erlang:get_stacktrace())
    end.

enqueue_kernel(No, Wait, Q, #kernel{id=K, wg=WG0}) ->
    {GWG,WG} = calc_wg(No, WG0),
%%    io:format("GWG ~w WG ~w~n",[GWG,WG]),
    case Wait of
	nowait ->
	    ok = cl:nowait_enqueue_nd_range_kernel(Q,K,GWG,WG,[]);
	    _ ->
	    {ok, Event} = cl:enqueue_nd_range_kernel(Q,K,GWG,WG,Wait),
	    Event
    end.

calc_wg(No, WG)
  when is_integer(No), is_integer(WG), No =< WG ->
    {[No],[No]};
calc_wg(No, WG)
  when is_integer(No), is_integer(WG), (No rem WG) == 0 ->
    {[No], [WG]};
calc_wg(No, WG)
  when is_integer(No), is_integer(WG) ->
    {[(1+(No div WG))*WG], [WG]};
calc_wg([WH|WT], [H|T]) ->
    {[CW], [CS]} = calc_wg(WH,H),
    {CT, CST} = calc_wg(WT,T),
    {[CW|CT], [CS|CST]};
calc_wg([], [1]) ->
    {[],[]};
calc_wg([], [H]) ->
    {[H],[H]}.


time_wait(Name, _Q, Event) ->
    Before = os:timestamp(),
    %% io:format("Event ~p ~w~n",[Event, cl:get_event_info(Event)]),
    %% io:format("Finish result ~p~n", [cl:finish(Q)]),
    %% receive
    %% 	Foo -> io:format("Foo ~p~n",[Foo])
    %% after 100 -> ok
    %% end,
    case cl:wait(Event) of
	{ok,completed} ->
	    io:format("CL ~p Time: ~p\n", [Name, timer:now_diff(os:timestamp(),Before)]);
	Error ->
	    receive
		EMsg -> io:format("Error ~p~n",[EMsg])
	    after 100 -> ok end,
	    exit(Error)
    end.
