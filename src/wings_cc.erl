%%% @author  <dgud@users.sf.net>
%%% @copyright (C) 2009, 
%%% @doc
%%%    
%%%    Some ideas taken from:
%%%    "Parallel View-Dependent Tesselation of Catmull-Clark Subdivision Surfaces"
%%%       by Anjul Patney, Mohamed S. Ebeida, John D. Owens
%%%    www.idav.ucdavis.edu/func/return_pdf?pub_id=964
%%%
%%% The intention is to create an OpenCL version later.
%%% @end
%%% Created : 22 Nov 2009 by  <dgud@erlang.org>

-module(wings_cc).
-export([setup/1, subdiv/2]).

-import(lists, [reverse/1, foldl/3]).

-include("wings.hrl").
-include_lib("cl/include/cl.hrl").

-compile(export_all).

-define(EPS, 0.0005).

-record(v, {pos,                     % position
	    n = {0.0,0.0,0.0},       % normal
	    vc}).                    % Valence

%%-record(flags, {ap, a, vs, es, f3, f4}).  Bitfield

%% The different working sets

-record(base, {v,   %% array of #v{}           nv	       
	       f,   %% array of [v0,v1..,vn]   nf
	       fi,  %% array of {Start,Size}   nf
	       e    %% array of v0,v1,f1,f2    ne
	      }).

%% OpenCL defines
-record(cli, {context, kernels, q, cl}).
-record(cl_mem, {v, v_no, f, fi, fi_no, e, e_no, vab}).
-record(kernel, {name, id, wg}).

-define(EDGE_SZ, (4*4)).
-define(FACE_SZ, (4*4)).
-define(VERTEX_SZ, ((3*4)+4)).
-define(LOCK_SZ, 32).
%%-define(DEFAULT, erlang).
-define(DEFAULT, opencl).


%%%% API %%%%%%%%%

%% @spec(#we) -> #base{}
setup(We) ->
    setup(We, ?DEFAULT).

setup(#we{} = We, Type) ->
    {Faces, Htab} = wings_subdiv:smooth_faces_htab(We),
    CreateMap = fun(Face,[N|Map]) -> [N+1,{Face,N}|Map] end,
    [_N|Map0] = foldl(CreateMap, [0], Faces),
    FMap = array:from_orddict(reverse(Map0)),
    Empty = array:new(),
    Acc = {Empty, Empty, {0, Empty}},
    {Vtab0, Etab0, Ftab0} = setup(Faces, [], Acc, FMap, We#we{he=Htab}),
    case Type of 
	erlang ->
	    Etab = array:from_list(array:sparse_to_list(Etab0)),
	    Ftab = array:from_list(reverse(Ftab0)),
	    #base{v=Vtab0, e=Etab, f=Ftab};
	opencl ->
	    GetFs = fun(Vs, {N, FI, Fs}) ->
			    Len = length(Vs),
			    {N+Len, <<FI/binary, N:?I32, Len:?I32>>,
			     << Fs/binary, 
				(<< <<V:?I32>> || V <- Vs >>)/binary >>}
		    end,
	    {_, FI, Ftab} = foldl(GetFs, {0, <<>>, <<>>}, reverse(Ftab0)),
	    
	    GetEs = fun(_, {V0,V1,F1,F2,H}, Bin) ->
			    %% Code neg vertex as hardedge
			    V0H = if H -> -1-V0; true -> V0 end,
			    <<Bin/binary, V0H:?I32, V1:?I32, F1:?I32, F2:?I32>>
		    end,
	    Etab = array:sparse_foldl(GetEs, <<>>,  Etab0),

	    GetVs = fun(_, #v{pos={X1,Y1,Z1}, vc={Vc,Hc}}, Bin) ->
			    VI = (Vc bsl 2) bor Hc,
			    << Bin/binary,
			       X1:?F32,Y1:?F32,Z1:?F32,VI:?F32 >>
		    end,
	    Vtab = array:foldl(GetVs, <<>>, Vtab0),
	    #base{v=Vtab, e=Etab, f=Ftab, fi=FI}
    end.

subdiv(Data, N) -> 
    subdiv(Data, N, ?DEFAULT).

subdiv(#base{f=Fs, e=Es, v=Vs}, N, erlang) ->
    try
	assert(Vs, Es, Fs),
	subdiv_erl(Fs, Es, Vs, N)
    catch
	{done,Res} ->
	    io:format("~p ~n", [Res])
    end;

subdiv(Base, N, opencl) ->
    try
	io:format("Compiling~n", []),
	CL = cl_setup(),
	io:format("Allocating~n", []),
	{In, Out} = cl_allocate(N, Base, CL),
	io:format("Subdiv ~n", []),
	VabBin = subdiv_cl(N, In, Out, CL, []),
	cl_release([In,Out]),
	VabBin
    catch
	_:Reas ->
	    io:format("Error ~p:~p ~n", [Reas, erlang:get_stacktrace()])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subdiv_cl(N, 
	  In = #cl_mem{v=VsIn, f=FsIn, fi=FiIn, e=EsIn,
		       v_no=NoVs, fi_no=NoFs, e_no=NoEs},
	  Out= #cl_mem{v=VsOut, f=FsOut, e=EsOut},
	  CL, Wait0)
  when N > 0 ->
    Args1 = [VsIn, FsIn, FiIn, VsOut, FsOut, NoFs, NoVs],
    W0 = cl_apply(gen_faces, Args1, NoFs, Wait0, CL),
    Args2 = [FsIn, FiIn, VsOut, NoFs, NoVs],
    W1 = cl_apply(add_center, Args2, 1, [W0], CL),
    Args3 = [VsIn, FsIn, EsIn, FiIn, 
	     VsOut, FsOut, EsOut, 
	     NoFs, NoVs, NoEs],
    W2 = cl_apply(gen_edges, Args3, NoEs, [W1], CL),    
    Args4 = [VsIn, VsOut, EsIn, NoEs],
    W3 = cl_apply(add_edge_verts, Args4, 1, [W2], CL),
    Args5 = [VsIn,VsOut,NoVs],
    Wait = cl_apply(move_verts, Args5, NoVs, [W3], CL),
    subdiv_cl(N-1, Out, In, CL, [Wait]);
subdiv_cl(_,#cl_mem{v=Vs, f=Fs, fi_no=NoFs, vab=Vab,
		    e=_Es, e_no=_NoEs},
	  _, CL = #cli{q=Q}, Wait) ->
    %% {ok, WR1} = cl:enqueue_read_buffer(Q,Fs,0,NoFs*?FACE_SZ,Wait),
    %% {ok, WR2} = cl:enqueue_read_buffer(Q,Es,0,NoEs*?EDGE_SZ,Wait),
    %% {ok, FsBin} = cl:wait(WR1),
    %% io:format("Fs ~p ~n",[[F || <<F:?I32>> <= FsBin]]),
    %% {ok, EsBin} = cl:wait(WR2),
    %% io:format("Es ~p ~n",[[F || <<F:?I32>> <= EsBin]]),

    %% Create #vab{}
    io:format("~p:~p ~n",[?MODULE,?LINE]),
    WVab = cl_apply(collect_face_info,[Vs,Fs,Vab,NoFs], NoFs, Wait,CL),
    {ok, WData} = cl:enqueue_read_buffer(Q,Vab,0,NoFs*4*6*4,[WVab]),
    {ok, VabBin} = cl:wait(WData),
    VabBin.

setup([F|Fs], Ftab0, Acc0, FMap, We) when F >= 0 ->
    {Vs, Acc} = wings_face:fold(fun(V,Eid,E,Acc) ->  %% CCW
					setup_1(V,Eid,E,Acc,FMap,We)
				end, {[], Acc0}, F, We),
    setup(Fs, [Vs|Ftab0], Acc, FMap, We);
setup([_|Fs], Ftab, Acc, FMap, We) ->
    setup(Fs, Ftab, Acc, FMap, We);
setup([], Ftab, {Vtab, Etab, _}, _, _) ->
    {Vtab, Etab, Ftab}.

setup_1(Orig, Eid, E, {Vs, {Vtab0, Etab0, VMap0}}, FMap, We) ->
    {V, Vtab, VMap1} = setup_vertex(Orig, Vtab0, VMap0, We),
    {Etab, VMap} = setup_edge(Eid, E, VMap1, FMap, Etab0, We#we.he),
    {[V|Vs], {Vtab, Etab, VMap}}.

setup_vertex(Orig, Vtab, VMap0, We = #we{vp=Vpos,he=He}) ->
    {V,VMap} = update_vmap(Orig, VMap0),
    case array:get(V, Vtab) of
	undefined ->
	    Count = fun(E,_,_,{C,H}) -> {C+1,add_hard(E,He,H)} end,
	    N  = wings_vertex:fold(Count, {0, 0}, Orig, We),
	    VP = #v{pos=array:get(Orig,Vpos), vc=N},
	    {V,array:set(V,VP,Vtab),VMap};
	_ ->
	    {V,Vtab,VMap}
    end.

add_hard(_Edge, _Htab, H) when H > 3 -> 3;
add_hard(Edge, Htab, H) ->
    case gb_sets:is_member(Edge,Htab) of
	true ->  H+1;
	false -> H
    end.

setup_edge(Eid, #edge{vs=OV1,ve=OV2,lf=F1,rf=F2}, VMap0, FMap, Etab, Htab) ->
    case array:get(Eid, Etab) of
	undefined ->
	    {V1,VMap1} = update_vmap(OV1, VMap0),
	    {V2,VMap}  = update_vmap(OV2, VMap1),
	    {array:set(Eid, {V1,V2,
			     array:get(F1, FMap),
			     array:get(F2, FMap),
			     gb_sets:is_member(Eid,Htab)
			    }, 
		       Etab), VMap};
	_ ->
	    {Etab, VMap0}
    end.

update_vmap(Orig, VM={N,VMap}) ->
    case array:get(Orig, VMap) of
	undefined -> 
	    {N, {N+1,array:set(Orig, N, VMap)}};
	V ->
	    {V, VM}
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subdiv_erl(InFs, Es0, InVs, N) when N > 0 ->
    SFs0 = array:sparse_foldl(fun(_F, Vs, R=[Prev|_]) ->
				      [Prev+length(Vs)|R]
			      end, [0], InFs),
    SFs = array:from_list(reverse(SFs0)),
    {OutVs1, OutFs1} = gen_face_points(InFs, InVs, InVs, SFs),
    {OutVs2, OutFs, Es} = gen_edge_points(Es0, InVs, OutVs1, 
					  SFs, InFs, OutFs1, undefined),
    %io:format("edge Vs ~p Es ~p Fs ~p ~n",[array:size(OutVs2), array:size(Es), array:size(OutFs)]),
    OutVs = move_vertex_points(InVs,OutVs2),
    %% io:format("Fs ~p~n",[array:to_list(OutFs)]),
    %% io:format("Es ~p ~n",[array:to_list(Es)]),
    subdiv_erl(OutFs, Es, OutVs, N-1);
subdiv_erl(Fs0, _Es0, Vtab, _N) ->
    assert(Vtab, _Es0, Fs0),
    array:foldl(fun(_, Vs0, Acc) ->
			Vs = [(array:get(V,Vtab))#v.pos || V <- Vs0],
			{Nx,Ny,Nz} = e3d_vec:normal(Vs),
			<<Acc/binary, (<< <<X:?F32,Y:?F32,Z:?F32,
					    Nx:?F32,Ny:?F32,Nz:?F32 >> 
					  || {X,Y,Z} <- Vs >>)/binary >>
		end, <<>>, Fs0).
	    
%%%  Step 2 build vertices/edges and faces

gen_face_points(Fs0, InVs, OutVs0, SFs) ->
    Face = fun(Face, Vs, {OutVs, Fs}) ->
		   gen_face_point(Face, Vs, InVs, SFs, OutVs, Fs)
	   end,
    array:foldl(Face, {OutVs0, Fs0}, Fs0).

gen_face_point(Face, FVs, InVs, SFs, OutVs0, Fs0) ->
    VsPos = [(array:get(V, InVs))#v.pos || V <- FVs],
    Center = e3d_vec:average(VsPos),
    OutVs1 = add_center(FVs, Center, OutVs0, face), %% atomicly 
    Vid = array:size(InVs) + Face,
    NewFid = face_id(Face, SFs),
    %% Face == 5 andalso 
    %% 	io:format("F ~p c ~p~n",[{V0,V1,V2,V3}, Vid]),
    New = #v{pos=Center, vc=4},
    OutVs = array:set(Vid, New, OutVs1),
    {_, Fs} = foldl(fun(V, {Fid, Fs}) ->		  
			    {Fid+1, array:set(Fid, [V, -1, Vid, -1], Fs)}
		    end, {NewFid, Fs0}, FVs),
    {OutVs, Fs}.

gen_edge_points(Es, InVs, OutVs, FsNo, InFs, OutFs, EdsNo) ->
    Edge = fun(Edge, Vs, Acc) ->
		   gen_edge_point(Edge, Vs, Acc, InVs, FsNo, InFs, EdsNo)
	   end,
    array:foldl(Edge, {OutVs,OutFs,Es}, Es).

gen_edge_point(Edge, {V0,V1,F1,F2,Hard}, {OutVs0,Fs0,Es0}, 
	       InVs,FsNo,InFs,EdsNo) ->
    #v{pos=V0Pos} = array:get(V0, InVs),
    #v{pos=V1Pos} = array:get(V1, InVs),

    OutVs1 = add_center([V0], V1Pos, OutVs0, Hard),
    OutVs2 = add_center([V1], V0Pos, OutVs1, Hard),
    VStart = array:size(InVs),    
    %% Edge Split position
    %% if Edge is hard Epoint = Mid,
    %% else    
    EP = case Hard of 
	     true -> 
		 e3d_vec:average(V0Pos, V1Pos);
	     false ->
		 e3d_vec:average([(array:get(VStart+F1, OutVs2))#v.pos,
				  (array:get(VStart+F2, OutVs2))#v.pos,
				  V0Pos, V1Pos])
	 end,
    FId = array:size(InFs) + VStart,
    VId = FId + Edge,
    OutVs = array:set(VId, #v{pos=EP, vc=4}, OutVs2), 
    %% Complete faces 
    SF1 = array:get(F1, FsNo),
    {F11,F12,CCW1} = find_new_faces(V0,V1,array:get(F1, InFs),SF1),
    SF2 = array:get(F2, FsNo),
    {F21,F22,CCW2} = find_new_faces(V0,V1,array:get(F2, InFs),SF2),
    %% Edge == 0 andalso 
    %% 	io:format("Up ~p ~p~n ~p => ~p ~p~n ~p => ~p~p~n",
    %% 		  [Edge, {V0,V1}, 
    %% 		   F1, {F11,F12}, array:get(F1, InFs),
    %% 		   F2, {F21,F22}, array:get(F2, InFs)]),
    Fs1 = update_face(F11, CCW1, VId, Fs0),
    Fs2 = update_face(F12, not CCW1, VId, Fs1),
    Fs3 = update_face(F21, CCW2, VId, Fs2),
    Fs4 = update_face(F22, not CCW2, VId, Fs3),
    %% New Edges
    SEdId = edge_id(Edge,EdsNo),
    Es1 = array:set(SEdId+0, {V0,VId,F11,F21,Hard}, Es0),
    Es2 = array:set(SEdId+1, {VId,V1,F12,F22,Hard}, Es1),
    Es3 = array:set(SEdId+2, {VId,VStart+F1,F11,F12,false}, Es2),
    Es4 = array:set(SEdId+3, {VId,VStart+F2,F21,F22,false}, Es3),
    
    {OutVs,Fs4,Es4}.

move_vertex_points(In, Out) ->
    Move = fun(V, #v{pos=InPos,vc=Cs={VC,HC}}, VP) ->
		   if HC < 2 ->
			   {A,B} = vc_div(VC),
			   #v{pos=PosOut} = array:get(V,Out),
			   %% We started with Inpos remove it
			   Ps  = e3d_vec:sub(PosOut, InPos),
			   Pos = e3d_vec:add_prod(e3d_vec:mul(Ps, A), InPos, B),
			   array:set(V, #v{pos=Pos, vc=Cs}, VP);
		      HC =:= 2 ->
			   #v{pos=Hard} = array:get(V,Out),
			   Pos0 = e3d_vec:add_prod(Hard, InPos, 6.0),
			   Pos  = e3d_vec:mul(Pos0, 1/8),
			   array:set(V, #v{pos=Pos, vc=Cs}, VP);
		      true ->
			   VP
		   end
	   end,
    array:sparse_foldl(Move, Out, In).

vc_div(3) -> {1/9,  1/3};
vc_div(4) -> {1/16, 2/4};
vc_div(5) -> {1/25, 3/5};
vc_div(N) -> {1/(N*N), (N-2.0/N)}.

%% The order is important to get ccw winding
%% find_new_faces(V0,V1,[V0,V1,_,_],Sid) -> {Sid+0,Sid+1,true};
%% find_new_faces(V0,V1,[V1,V0,_,_],Sid) -> {Sid+1,Sid+0,false};
%% find_new_faces(V0,V1,[_,V0,V1,_],Sid) -> {Sid+1,Sid+2,true};
%% find_new_faces(V0,V1,[_,V1,V0,_],Sid) -> {Sid+2,Sid+1,false};
%% find_new_faces(V0,V1,[_,_,V0,V1],Sid) -> {Sid+2,Sid+3,true};
%% find_new_faces(V0,V1,[_,_,V1,V0],Sid) -> {Sid+3,Sid+2,false};
%% find_new_faces(V0,V1,[V1,_,_,V0],Sid) -> {Sid+3,Sid+0,true};
%% find_new_faces(V0,V1,[V0,_,_,V1],Sid) -> {Sid+0,Sid+3,false}.

find_new_faces(V0,V1,[V0,VN|R],Sid) -> 
    case VN of
	V1 -> {Sid,Sid+1,true};
	_ ->  {Sid,Sid+1+length(R),false}
    end;
find_new_faces(V0,V1,[V1,VN|R],Sid) -> 
    case VN of
	V0 -> {Sid+1,Sid,false};
	_  -> {Sid+1+length(R),Sid,true}
    end;
find_new_faces(V0,V1,[_|R],Sid) ->
    find_new_faces(V0,V1,R,Sid+1).
    

update_face(Face,true,VId, Fs) ->
    try 
	[A,-1,C,D] = array:get(Face,Fs),
	array:set(Face, [A,VId,C,D], Fs)
    catch Class:Reason ->
	    io:format("Fs: ~p~n",[array:sparse_to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,1,VId,array:get(Face, Fs)]),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end;
update_face(Face,false,VId, Fs) ->
    try
	[A,B,C,-1] = array:get(Face,Fs),
	array:set(Face, [A,B,C,VId], Fs)
    catch Class:Reason ->
	    io:format("XXX ~p ~n",[Fs]),
	    io:format("Fs: ~p~n",[array:sparse_to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,3,VId,array:get(Face, Fs)]),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

add_center([V|Vs], Center, OutVs, Hard) ->
    Vx = #v{pos=Pos, vc={_,He}} = array:get(V,OutVs),
    if He > 2 ->
	    add_center(Vs, Center, OutVs, Hard);
       He < 2 -> 
	    Updated = array:set(V, Vx#v{pos = e3d_vec:add(Pos,Center)}, OutVs),
	    add_center(Vs, Center, Updated, Hard);
       Hard =:= face ->  %% Reset
	    Updated = array:set(V, Vx#v{pos = {0.0,0.0,0.0}}, OutVs),
	    add_center(Vs, Center, Updated, Hard);
       Hard ->
	    Updated = array:set(V, Vx#v{pos = e3d_vec:add(Pos,Center)}, OutVs),
	    add_center(Vs, Center, Updated, Hard);
       true ->
	    add_center(Vs, Center, OutVs, Hard)
    end;
add_center([],_,OutVs,_) ->
    OutVs.
	     
face_id(Face, Fs) ->
    array:get(Face, Fs).

edge_id(Edge, undefined) -> Edge*4;
edge_id(Edge, Es) -> array:get(Edge, Es).

%%%%%%%%
%%assert(_OutVs, _Es, _OutFs) -> ok;
assert(OutVs, Es, OutFs) ->
    io:format("Asserting .. ",[]),
    Assert = 
	fun(Edge, E={V1,V2,F1,F2,_H}, Fs0) ->
		try 
		    #v{} = array:get(V1, OutVs),
		    #v{} = array:get(V2, OutVs),
		    find_new_faces(V1,V2,array:get(F1, OutFs),0),
		    find_new_faces(V1,V2,array:get(F2, OutFs),0),
		    Fs1 = array:set(F1, [Edge|array:get(F1, Fs0)], Fs0),
		    _Fs = array:set(F2, [Edge|array:get(F2, Fs0)], Fs1)
		catch _:Reason ->
			io:format("assert failed ~p ~n", [Reason]),
			io:format("Edge ~p ~p ~n", [Edge, E]),
			io:format("  F ~p: ~p ~n", [F1,array:get(F1, OutFs)]),
			io:format("  F ~p: ~p ~n", [F2,array:get(F2, OutFs)]),
			exit(Reason)
		end
	end,
    _ = array:foldl(Assert, array:new([{default, []}]), Es),
    %% ok = array:foldl(fun(_Face, [_,_,_,_], Ok) -> Ok;
    %% 			(Face, Eds, _) -> {Face,Eds}
    %% 		     end, ok, F2Es),
    io:format("Asserted~n",[]).

cl_release([#cl_mem{v=Vs, f=Fs, fi=Fi, e=Es}|R]) ->
    Vs /= undefined andalso cl:release_mem_object(Vs),
    Fs /= undefined andalso cl:release_mem_object(Fs),
    Fi /= undefined andalso cl:release_mem_object(Fi),
    Es /= undefined andalso cl:release_mem_object(Es),
    cl_release(R);
cl_release([]) ->
    %% During DEBUGGING REMOVE LATER BUGBUG
    #cli{q=Q,kernels=Ks,context=C} = get({?MODULE, cl}),
    erase({?MODULE, cl}),
    cl:release_queue(Q),
    [cl:release_kernel(K) || #kernel{id=K} <- Ks],
    cl:release_context(C),
    ok.

%%%%%%% Opencl

cl_setup() ->
    case get({?MODULE, cl}) of 
	undefined -> 	    
	    cl_setup_1();
	CL ->
	    CL
    end.

cl_setup_1() ->
    Opts = [],
    Prefered = proplists:get_value(cl_type, Opts, cpu),
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
    %%% Compile
    Dir = filename:join(code:lib_dir(wings),"shaders"),
    {ok, Bin} = file:read_file(filename:join([Dir, "cc_subdiv.cl"])),
    case clu:build_source(CL, Bin) of
	{error, {Err={error,build_program_failure}, _}} ->
	    %% io:format("~s", [Str]),
	    exit(Err);
	{ok, Program} -> 
	    {ok, Kernels0} = cl:create_kernels_in_program(Program),
	    Kernels = [kernel_info(K,Device) || K <- Kernels0],
	    io:format("Kernels ~p~n",[Kernels]),
	    CLI = #cli{context=CL#cl.context,kernels=Kernels,
		       q=Queue, cl=CL},
	    cl:release_program(Program),
	    put({?MODULE, cl}, CLI),
	    CLI
    end.

cl_apply(Name, Args, No, Wait, #cli{q=Q, kernels=Ks}) ->
    #kernel{id=K, wg=WG0} = lists:keyfind(Name, 2, Ks),
    clu:apply_kernel_args(K, Args),
    {GWG,WG} = if  No > WG0  -> 
		       {(1+(No div WG0))*WG0, WG0};
		   true -> {No,No}
	       end,    
    io:format("Exec: ~p(#~p) ~p GW ~p ~p~n", 
	      [Name, length(Args), No, GWG, WG]),
    {ok, Event} = cl:enqueue_nd_range_kernel(Q,K,[GWG],[WG],Wait),
    Event.

kernel_info(K,Device) ->
    {ok, WG} = cl:get_kernel_workgroup_info(K, Device, work_group_size),
    {ok, Name} = cl:get_kernel_info(K, function_name),
    #kernel{name=list_to_atom(Name), wg=WG, id=K}.

%% For now only one level at the time
cl_allocate(N, #base{f=Fs, fi=Fi, e=Es, v=Vs}, #cli{context=Ctxt,q=Q}) ->
    NoFs = size(Fi) div 8,
    NoEs = size(Es) div ?EDGE_SZ,
    NoVs = size(Vs) div ?VERTEX_SZ,

    io:format("Fs ~p Es ~p Vs ~p~n",[NoFs, NoEs, NoVs]),
    
    Skip = size(Fi) - 8,
    <<_:Skip/binary, NoFs0:?I32, LastFc:?I32>> = Fi,
    NextFs = NoFs0 + LastFc,
    
    Copy = [copy_host_ptr],
    {ok,FsIn} = cl:create_buffer(Ctxt, Copy, byte_size(Fs), Fs),
    {ok,FiIn} = cl:create_buffer(Ctxt, Copy, byte_size(Fi), Fi),
    {ok,EsIn} = cl:create_buffer(Ctxt, Copy, byte_size(Es), Es),
    {ok,VsIn} = cl:create_buffer(Ctxt, Copy, byte_size(Vs), Vs),
    {ok,FsOut} = cl:create_buffer(Ctxt, [], NextFs*?FACE_SZ),
    %% {ok,FiOut} = cl:create_buffer(Context, Copy, NoFs*Fi),
    {ok,EsOut} = cl:create_buffer(Ctxt, [], NoEs*4*?EDGE_SZ),
    {ok,VsOut} = cl:create_buffer(Ctxt, [], (NoVs+NoFs+NoEs)*?VERTEX_SZ),
    {ok, WW} = cl:enqueue_write_buffer(Q, VsOut, 0, byte_size(Vs), Vs, []),
    {ok,VabOut} = cl:create_buffer(Ctxt, [write_only], NextFs*(3+3)*4*4),
    cl:wait(WW),
    {#cl_mem{v=VsIn, f=FsIn, fi=FiIn, e=EsIn,
	     v_no=NoVs, fi_no=NoFs, e_no=NoEs, 
	     vab=VabOut
	    },
     #cl_mem{v=VsOut, f=FsOut, %% fi=FiIn, 
	     e=EsOut,
	     v_no=NoVs+NextFs+NoEs, fi_no=NextFs, e_no=NoEs*4, 
	     vab=VabOut}}.
