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
	 setup/0, compile/2, get_context/1, get_device/1,
	 cast/5, write/3, read/4,
	 tcast/5
	]).

-record(cli, {context, kernels, q, cl, device}).
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
compile(File, CLI = #cli{cl=CL, device=Device}) ->
    Dir = filename:join(code:lib_dir(wings),"shaders"),
    {ok, Bin} = file:read_file(filename:join([Dir, File])),
    case clu:build_source(CL, Bin) of
	{error, {Err={error,build_program_failure}, _}} ->
	    %% io:format("~s", [Str]),
	    exit(Err);
	{ok, Program} -> 
	    {ok, MaxWGS} = cl:get_device_info(Device, max_work_group_size),
	    {ok, Kernels0} = cl:create_kernels_in_program(Program),
	    Kernels = [kernel_info(K,Device, MaxWGS) || K <- Kernels0],
	    cl:release_program(Program),
	    CLI#cli{kernels=Kernels}
    end.

kernel_info(K,Device, MaxWGS) ->
    {ok, WG} = cl:get_kernel_workgroup_info(K, Device, work_group_size),
    {ok, Name} = cl:get_kernel_info(K, function_name),
    #kernel{name=list_to_atom(Name), wg=min(WG,MaxWGS), id=K}.

get_context(#cli{context=Context}) ->
    Context.
get_device(#cli{device=Device}) ->
    Device.


%% cast(Kernel, Args, NoInvocations, [Wait], cli()) -> Wait
tcast(Name, Args, No, Wait, CL) ->
    cast(Name, Args, No, Wait, true, CL).

cast(Name, Args, No, Wait, CL) ->
    cast(Name, Args, No, Wait, false, CL).
cast(Name, Args, No, Wait, Time, #cli{q=Q, kernels=Ks}) ->
    #kernel{id=K, wg=WG0} = lists:keyfind(Name, 2, Ks),
    try clu:apply_kernel_args(K, Args) of
	ok -> ok
    catch error:Reason ->
	    io:format("Bad args ~p: ~p~n",[Name, Args]),
	    erlang:raise(error,Reason, erlang:get_stacktrace())
    end,
    {GWG,WG} = if  No > WG0  -> 
		       {(1+(No div WG0))*WG0, WG0};
		   true -> {No,No}
	       end,
    %% io:format("X ~p GWG ~p WG ~p~n", [Name, GWG, WG]),
    {ok, Event} = cl:enqueue_nd_range_kernel(Q,K,[GWG],[WG],Wait),
    Time andalso time_wait(Name, Event),
    Event.

time_wait(Name, Event) ->
    Before = os:timestamp(),
    {ok,completed} = cl:wait(Event),
    io:format("CL ~p Time: ~p\n", [Name, timer:now_diff(os:timestamp(),Before)]).


%% write(CLMem, Bin, cli()) -> Wait
write(CLMem, Bin, #cli{q=Q}) ->
    {ok, W1} = cl:enqueue_write_buffer(Q, CLMem, 0, byte_size(Bin), Bin, []),
    W1.

%% read(CLMem, Sz, [Wait], cli()) -> Wait
read(CLMem, Sz, Wait, #cli{q=Q}) -> 
    {ok, W} = cl:enqueue_read_buffer(Q,CLMem,0,Sz, Wait),
    W.

