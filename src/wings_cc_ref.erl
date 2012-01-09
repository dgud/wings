%%
%%  wings_cc_ref.erl --
%%
%%     This module implements the smooth proxy in erlang, this is a
%%     reference implementation to the one using opencl.
%% 
%%  Copyright (c) 2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_cc_ref).

-import(lists, [reverse/1, foldl/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("cl/include/cl.hrl").

-compile(export_all).

-record(v, {pos,                     % position
	    vc}).                    % Valence

%% The different working sets

-record(base, {v,    %% array of #v{}           nv
	       f,    %% array of [v0,v1..,vn]   nf
	       fi,   %% array of {Start,Size}   nf
	       e,    %% array of v0,v1,f1,f2    ne
	       as,   %% Vertex attrs
	       level,%% Subdiv levels
	       n,    %% Number of faces 
	       mmap, %% Material map, {mat, start, vertex_count}
	       vmap, %% array Wings Vertex Id -> CL vertex id
	       fmap, %% gb_tree Wings face id -> CL face id
	       type  %% Type of data plain,uv,color,color_uv
	      }).

-define(EDGE_SZ, (4*4)).
-define(FACE_SZ, (4*4)).
-define(VERTEX_SZ, ((3*4)+4)).
-define(LOCK_SZ, 32).

subdiv(#base{v=Vs,e=Es,f=Fs, as=As, level=N}) ->
    subdiv_erl_1(Fs, Es, Vs, As, N).

subdiv_erl_1(InFs, Es0, InVs, As, N) when N > 0 ->
    SFs0 = array:sparse_foldl(fun(_F, Vs, R=[Prev|_]) ->
				      [Prev+length(Vs)|R]
			      end, [0], InFs),
    SFs = array:from_list(reverse(SFs0)),
    {OutVs1, OutFs1} = gen_face_points(InFs, InVs, InVs, SFs),
    {OutVs2, OutFs, Es} = gen_edge_points(Es0, InVs, OutVs1, 
					  SFs, InFs, OutFs1, undefined),
    OutVs = move_vertex_points(InVs,OutVs2),
    OutAs = subdiv_attrs(As),
    %% erl_vs("evs_out3", N, OutVs),
    subdiv_erl_1(OutFs, Es, OutVs, OutAs, N-1);
subdiv_erl_1(Fs0, _Es0, Vtab, As, _N) ->
    %% assert(Vtab, _Es0, Fs0),    
    {Vtab, As, Fs0}.

update(Changed, B=#base{v=Vs0,vmap=Map}) ->
    Vs = case Changed of
	     [_|_] ->
		 Move = fun({Vid, Pos}, Vs) ->
				try 
				CLid = array:get(Vid, Map),
				V = array:get(CLid, Vs),
				array:set(CLid, V#v{pos=Pos}, Vs)
				catch _:badarg ->
					io:format("VMap ~w~n", 
						  [array:to_orddict(Map)]),
					io:format("Vid ~p ~n", [Vid]),
					io:format("CLid ~p ~n", [catch array:get(Vid, Map)]),
					exit(foo)
				end
			end,
		 %% io:format("Changed ~p~n",[Changed]),
		 foldl(Move, Vs0, Changed);
	     #we{vp=Vpos} ->
		 Move = fun(Vid, CLid, Vs) ->
				Pos = array:get(Vid, Vpos),
				V = array:get(CLid, Vs),
				array:set(CLid, V#v{pos=Pos}, Vs)
			end,
		 array:sparse_foldl(Move, Vs0, Map)
	 end,
    B#base{v=Vs}.

gen_vab({Vtab, As, Ftab}, #base{n=Total, type=Type, mmap=Mats}) -> 
    Gen = fun(Id, Vs0, Bin) when Id < Total ->
		  Vs = [(array:get(V,Vtab))#v.pos || V <- Vs0],
		  {Nx,Ny,Nz} = e3d_vec:normal(Vs),
		  Face = << <<X:?F32,Y:?F32,Z:?F32, 
			      Nx:?F32,Ny:?F32,Nz:?F32 >> 
			    || {X,Y,Z} <- Vs >>,
		  <<Bin/binary, Face/binary >>;
	     (_,_, Bin) ->
		  throw(Bin)
	  end,
    Bin = try 
	      array:foldl(Gen, <<>>, Ftab)
	  catch throw:Bin1 ->
		  Bin1
	  end,    
    AsBin = case As of
		undefined -> <<>>;
		_ ->
		    try 		
			lists:foldl(gen_attrib_fun(Type), <<>>, As)
		    catch throw:Bin2 ->
			    Bin2
		    end
	    end,
    {Bin, <<>>, AsBin, Mats}.

%% print( <<R:?F32, G:?F32, B:?F32, U:?F32, V:?F32, Rest/binary>>, Face) ->
%%     io:format("~w: {~.4f,~.4f,~.4f}, {~.4f,~.4f}~n", [Face,R,G,B,U,V]),
%%     print(Rest, Face+1);
%% print(<<>>, _) -> ok.

gen_attrib_fun(color) ->
    fun(Colors, Bin) ->
	    Face = << <<(col_bin(Col))/binary>> || Col <- Colors >>,
	    <<Bin/binary, Face/binary >>
    end;
gen_attrib_fun(uv) ->
    fun(Uvs, Bin) ->
	    Face = << <<(uv_bin(Uv))/binary>> || Uv <- Uvs >>,
	    <<Bin/binary, Face/binary >>
    end;
gen_attrib_fun(color_uv) ->
    Gen = fun([Col|Uv], Bin) ->
		  <<Bin/binary, (col_bin(Col))/binary,(uv_bin(Uv))/binary>>;
	     (none, Bin) ->
		  <<Bin/binary, (col_bin(none))/binary,(uv_bin(none))/binary>>
	  end,
    fun(Uvs, Bin) ->
	    foldl(Gen,Bin, Uvs)
    end;
gen_attrib_fun(_) -> fun(_, _) -> throw(<<>>) end.

uv_bin({U,V}) -> <<U:?F32, V:?F32>>;
uv_bin(_) -> <<0.0:?F32, 0.0:?F32>>.
     
col_bin({R,G,B}) -> <<R:?F32, G:?F32, B:?F32>>;
col_bin(_) -> <<1.0:?F32, 1.0:?F32, 1.0:?F32>>.

gen_vab({_Type, MatFs}, Data, #base{fmap=FMap}) ->
    gen_vab(MatFs, Data, FMap, 0, <<>>, []).

gen_vab([{Mat,Fs}|T], D={Vtab, _, Ftab},FMap, Start, Bin0, MI) ->
    %%io:format("Mat: ~p Fs: ~p ~p~n",[Mat, length(Fs), Fs]),
    Build = fun(WFace, {C,Acc}) ->
		    {FStart,Fsz} = gb_trees:get(WFace,FMap),
		    Bin = gen_faces(FStart, FStart+Fsz, Ftab, Vtab, Acc),
		    {C+Fsz*4, Bin}
	    end,
    {Stop, Bin} = lists:foldl(Build, {Start, Bin0}, Fs),
    MatInfo = {Mat,?GL_QUADS,Start,Stop-Start},
    gen_vab(T, D, FMap, Stop, Bin, [MatInfo|MI]);
gen_vab([], _, _,_, Bin, Mi) ->
    {Bin, <<>>, <<>>, Mi}.

gen_faces(N, End, Ftab, Vtab, Acc0) when N < End -> 
    Vs0 = array:get(N, Ftab),
    Vs = [(array:get(V,Vtab))#v.pos || V <- Vs0],
    {Nx,Ny,Nz} = e3d_vec:normal(Vs),
    Face = (<< <<X:?F32,Y:?F32,Z:?F32, Nx:?F32,Ny:?F32,Nz:?F32 >> 
	       || {X,Y,Z} <- Vs >>),
    Acc = <<Acc0/binary, Face/binary >>,
    gen_faces(N+1, End,  Ftab, Vtab, Acc);
gen_faces(_,_,_,_,Acc) -> Acc.
	    
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
    New = #v{pos=Center, vc={length(FVs),0}},
    OutVs = array:set(Vid, New, OutVs1),
    {_, Fs} = foldl(fun(V, {Fid, Fs}) ->		  
			    {Fid+1, array:set(Fid, [V, -5, Vid, -5], Fs)}
		    end, {NewFid, Fs0}, FVs),
    {OutVs, Fs}.

gen_edge_points(Es, InVs, OutVs, FsNo, InFs, OutFs, EdsNo) ->
    Edge = fun(Edge, Vs, Acc) ->
		   gen_edge_point(Edge, Vs, Acc, InVs, FsNo, InFs, EdsNo)
	   end,
    array:foldl(Edge, {OutVs,OutFs,Es}, Es).

gen_edge_point(Edge, {-1,-1,-1,-1,_Hard}, {OutVs0,Fs0,Es0}, 
	       InVs,_FsNo,InFs,EdsNo) ->
    SEdId = edge_id(Edge,EdsNo),
    VStart = array:size(InVs),
    FId = array:size(InFs) + VStart,
    VId = FId + Edge,
    Es1 = array:set(SEdId+0, {-1,-1,-1,-1,false}, Es0),
    Es2 = array:set(SEdId+1, {-1,-1,-1,-1,false}, Es1),
    Es3 = array:set(SEdId+2, {-1,-1,-1,-1,false}, Es2),
    Es4 = array:set(SEdId+3, {-1,-1,-1,-1,false}, Es3),
    OutVs = array:set(VId, #v{pos={0.0,0.0,0.0}, vc={4, 0}}, OutVs0),
    {OutVs,Fs0,Es4};

gen_edge_point(Edge, {V0,V1,F1,F2,Hard}, {OutVs0,Fs0,Es0}, 
	       InVs,FsNo,InFs,EdsNo) ->
    #v{pos=V0Pos} = array:get(V0, InVs),
    #v{pos=V1Pos} = array:get(V1, InVs),

    OutVs1 = add_center([V0], V1Pos, OutVs0, Hard),
    OutVs2 = add_center([V1], V0Pos, OutVs1, Hard),
    VStart = array:size(InVs),
    %% Edge Split position
    {EP,HC} = case Hard of 
		  true ->  %% if Edge is hard Epoint = Mid,
		      {e3d_vec:average(V0Pos, V1Pos),2};
		  false -> %% Otherwise center of all
		      {e3d_vec:average([(array:get(VStart+F1, OutVs2))#v.pos,
					(array:get(VStart+F2, OutVs2))#v.pos,
					V0Pos, V1Pos]),0}
	      end,
    FId = array:size(InFs) + VStart,
    VId = FId + Edge,
    OutVs = array:set(VId, #v{pos=EP, vc={4, HC}}, OutVs2),
    SEdId = edge_id(Edge,EdsNo),

    %% Complete faces 
    case F1 >= 0 of
	true ->
	    SF1 = array:get(F1, FsNo),
	    {F11,F12,CCW1} = 
		find_new_faces(V0,V1,array:get(F1, InFs),SF1),
	    Fs1 = update_face(F11, CCW1, VId, Fs0),
	    Fs2 = update_face(F12, not CCW1, VId, Fs1),
	    Es1 = array:set(SEdId+0, {VId,VStart+F1,F11,F12,false}, Es0);
	false ->
	    F11 = -1, 
	    F12 = -1,
	    Es1 = array:set(SEdId+0, {-1,-1,-1,-1,false}, Es0),
	    Fs2 = Fs0
    end,
    case F2 >= 0 of
	true ->
	    SF2 = array:get(F2, FsNo),
	    {F21,F22,CCW2} = 
		find_new_faces(V0,V1,array:get(F2, InFs),SF2),
	    Fs3 = update_face(F21, CCW2, VId, Fs2),
	    Fs4 = update_face(F22, not CCW2, VId, Fs3),
	    Es2 = array:set(SEdId+1, {VId,VStart+F2,F21,F22,false}, Es1);
	false ->
	    F21 = -1, 
	    F22 = -1,
	    Es2 = array:set(SEdId+1, {-1,-1,-1,-1,false}, Es1),
	    Fs4 = Fs2
    end,
    %% Edge == 0 andalso 
    	%% io:format("Up ~p ~p~n ~p => ~p ~p~n ~p => ~p~p~n",
    	%% 	  [Edge, {V0,V1}, 
    	%% 	   F1, {F11,F12}, get_face(F1, InFs),
    	%% 	   F2, {F21,F22}, get_face(F2, InFs)]),
    %% New Edges
    Es3 = array:set(SEdId+2, {V0,VId,F11,F21,Hard}, Es2),
    Es4 = array:set(SEdId+3, {VId,V1,F12,F22,Hard}, Es3),
    {OutVs,Fs4,Es4}.

move_vertex_points(In, Out) ->
    Move = fun(V, InV=#v{pos=InPos,vc=Cs={VC,HC}}, VP) ->
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
			   array:set(V, InV, VP)
		   end
	   end,
    array:sparse_foldl(Move, Out, In).

vc_div(3) -> {1/9,  1/3};
vc_div(4) -> {1/16, 2/4};
vc_div(5) -> {1/25, 3/5};
vc_div(N) -> {1/(N*N), (N-2.0)/N}.

subdiv_attrs(undefined) -> undefined;
subdiv_attrs(As0) -> 
    {_,As} = lists:foldl(fun(Attrs = [First|_], {F,Out}) ->
			     Last = lists:last(Attrs),			     
			     Center = average(Attrs),
			     {F+1,subdiv_attrs(Last, Attrs, Center, First, Out)}
		     end, {0,[]}, As0),
    reverse(As).

subdiv_attrs(Last, [Curr|Attrs=[Next|_]], Center, First, Out) ->
    FaceAttr = [Curr, average(Curr,Next),
		Center, average(Curr,Last)],
    subdiv_attrs(Curr, Attrs, Center, First, [FaceAttr|Out]);
subdiv_attrs(Last, [Curr], Center, Next, Out) ->
    FaceAttr = [Curr, average(Curr,Next),
		Center, average(Curr,Last)],
    [FaceAttr|Out].

average(Attrs = [[_|_]|_]) ->
    wings_va:average_attrs(Attrs);
average(Attrs) ->
    wings_color:average(Attrs).
average(Attr1 = [_|_], Attr2) ->
    wings_va:average_attrs(Attr1, Attr2);
average(A1, A2) ->
    wings_color:average(A1,A2).

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
    find_new_faces(V0,V1,R,Sid+1);
find_new_faces(_,_, -1, _) ->
    ok.
    

update_face(Face,true,VId, Fs) ->
    try 
	[A,-5,C,D] = array:get(Face,Fs),
	array:set(Face, [A,VId,C,D], Fs)
    catch Class:Reason ->
	    io:format("Fs: ~p~n",[array:sparse_to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,1,VId,array:get(Face, Fs)]),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end;
update_face(Face,false,VId, Fs) ->
    try
	[A,B,C,-5] = array:get(Face,Fs),
	array:set(Face, [A,B,C,VId], Fs)
    catch Class:Reason ->
	    io:format("XXX ~p ~n",[Fs]),
	    io:format("Fs: ~p~n",[array:sparse_to_orddict(Fs)]),
	    io:format("~p ~p ~p ~p~n",
		      [Face,3,VId,array:get(Face, Fs)]),
	    erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

add_center([V|Vs], Center, OutVs, Hard=true) ->
    Vx = #v{pos=Pos} = array:get(V,OutVs),
    Updated = array:set(V, Vx#v{pos = e3d_vec:add(Pos,Center)}, OutVs),
    add_center(Vs, Center, Updated, Hard);
add_center([V|Vs], Center, OutVs, Hard) ->
    Vx = #v{pos=Pos, vc={_,He}} = array:get(V,OutVs),
    if 
	He < 2 -> 
	    Updated = array:set(V, Vx#v{pos = e3d_vec:add(Pos,Center)}, OutVs),
	    add_center(Vs, Center, Updated, Hard);
	He =:= 2, Hard =:= face ->  %% Reset
	    Updated = array:set(V, Vx#v{pos = {0.0,0.0,0.0}}, OutVs),
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
	fun(_, {-1,-1,-1,-1,_H}, _Fs0) -> ok;
	   (Edge, E={V1,V2,F1,F2,_H}, _Fs0) ->
		try 
		    #v{} = array:get(V1, OutVs),
		    #v{} = array:get(V2, OutVs),
		    find_new_faces(V1,V2,get_face2(F1, OutFs),0),
		    find_new_faces(V1,V2,get_face2(F2, OutFs),0)
		catch _:Reason ->
			io:format("assert failed ~p ~p~n", 
				  [Reason, erlang:get_stacktrace()]),
			io:format("Edge ~p ~p ~n", [Edge, E]),
			io:format("  F ~p: ~p ~n", [F1,get_face2(F1, OutFs)]),
			io:format("  F ~p: ~p ~n", [F2,get_face2(F2, OutFs)]),
			exit(Reason)
		end
	end,
    array:foldl(Assert, array:new([{default, []}]), Es),
    %% ok = array:foldl(fun(_Face, [_,_,_,_], Ok) -> Ok;
    %% 			(Face, Eds, _) -> {Face,Eds}
    %% 		     end, ok, F2Es),
    io:format("Asserted~n",[]).

get_face2(F, Fs) when F >= 0 ->
    array:get(F,Fs);
get_face2(_,_) -> -1.

erl_vs(Str, N, Vs) ->
    {ok, F} = file:open(Str ++ "_" ++ integer_to_list(N), [write]),
    try 
	W = fun(V, #v{pos={X,Y,Z}, vc=VI},_) -> 
		    io:format(F,"{~w, {~.4f,~.4f,~.4f}, ~w}~n", [V,X,Y,Z,VI])
	    end,
	ok = array:foldl(W, ok, Vs)
    catch E:R ->
	    io:format("Failed ~p:~p ~p~n",[E,R,erlang:get_stacktrace()])
    after 
	file:close(F)	
    end.

erl_face(Str, N, Fs) ->
    {ok, FD} = file:open(Str ++ "_" ++ integer_to_list(N), [write]),
    try 
	W = fun(F, [V1,V2,V3,V4], _) -> 
		    io:format(FD,"{~w, {~w,~w,~w,~w}}.~n", [F,V1,V2,V3,V4])
	    end,
	ok = array:foldl(W, ok, Fs)
    catch E:R ->
	    io:format("Failed ~p:~p ~p~n",[E,R,erlang:get_stacktrace()])
    after 
	file:close(FD)	
    end.

