%%
%%  wings_cc_ref.erl --
%%
%%     This module implements the smooth proxy in erlang, this is a
%%     reference implementation to the one using opencl.
%% 
%%  Copyright (c) 2010-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%%%    Inspiration taken from originally from: 
%%% "Parallel View-Dependent Tesselation of Catmull-Clark Subdivision
%%% Surfaces" by Anjul Patney, Mohamed S. Ebeida, John D. Owens
%%% www.idav.ucdavis.edu/func/return_pdf?pub_id=964
%%%
%%%  They implemented an adaptiv solution, I've later changed my mind and
%%%  done my own solution but if an adaptiv solution shall be implemented
%%%  check out the paper above.
%%%  
%%%  wings_cc_ref: contains an erlang reference implementation which
%%%                can be debugged. Keep it up to date.
%%%

-module(wings_cc).
-export([init/2, update/2, gen_vab/1, gen_vab/2]).

%% debug 
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

%% OpenCL defines
-record(cli,      
	{context, kernels, q, cl, device, 
	 %% CL temp buffers and respective sizes
	 vab, vab_sz=0, fl, fl_sz=0, fi, fi_sz=0,
	 sn, sn_vz %% Smooth normals ? can reuse fl?
	}).

-record(cl_mem,   {v, v_no, f, fs_no, e, e_no, fi, fi0, as}).
-record(cc_cache, {mem, old, wait}).
-record(kernel,   {name, id, wg}).

-define(EDGE_SZ, (4*4)).
-define(FACE_SZ, (4*4)).
-define(VERTEX_SZ, ((3*4)+4)).
-define(LOCK_SZ, 32).
%%-define(DEFAULT, erlang).
-define(DEFAULT, opencl).

%%%% API %%%%%%%%%

%% Returns opaque data
init(Plan, We) ->
    ?TC(build_data(Plan,4,We,?DEFAULT)).

%% Update state with new vertex positions
update(ChangedVs, Data) ->
    case ?DEFAULT of 
	erlang ->  wings_cc_ref:update(ChangedVs, Data);
	_ ->       update_1(ChangedVs, Data)
    end.

%% Generates a subdivided #vab{}
gen_vab(Base) ->
    case subdiv(Base, ?DEFAULT) of
	skip -> create_vab(<<>>, []);
	Data ->
	    gen_vab_1(Data, Base)
    end.
%% Generates a subdivided #vab{} from Material Plan
gen_vab(Plan, Base) ->
    case subdiv(Base, ?DEFAULT) of
	skip -> create_vab(<<>>, []);
	Data ->
	    gen_vab_1(Plan, Data, Base)
    end.

%% Subdivide mesh
subdiv(Base = #base{level=N, n=Total, type=Type}, Impl) when Total > 0 ->
    try 
	case Impl of
	    opencl ->
		{In,Out,CL} = cl_allocate(Base, cl_setup()),
		Wait = cl_write_input(Base, In, Out, CL),
		subdiv_1(N, In, Out, Type, CL, Wait);
	    erlang ->
		wings_cc_ref:subdiv(Base)
	end
    catch
	_:Reas ->
	    io:format("Error ~p:~p ~n", [Reas, erlang:get_stacktrace()])
    end;
subdiv(_, _) ->
    skip.

%% Generates a vab (and updates Data)
%% Returns Vab
gen_vab_1(Data0, Base) ->
    try 
	case ?DEFAULT of
	    erlang -> ?TC(create_vab(wings_cc_ref:gen_vab(Data0, Base), Base));
	    opencl -> create_vab(gen_vab_2(Data0, Base), Base)
	end
    catch
	_:Reas ->
	    io:format("Error ~p:~p ~n", [Reas, erlang:get_stacktrace()])
    end.

gen_vab_1(Plan, Data, Base) ->
    try 
	case ?DEFAULT of
	    erlang -> create_vab(wings_cc_ref:gen_vab(Plan, Data, Base), Base);
	    opencl -> create_vab(gen_vab_2(Plan, Data, Base), Base)
	end
    catch
	_:Reas ->
	    io:format("Error ~p:~p ~n", [Reas, erlang:get_stacktrace()])
    end.

create_vab({Vs, SNs0, Attrs0, MatInfo}, #base{type=Type}) ->
    Ns = case Vs of
	     <<>> -> Vs;
	     <<_:3/unit:32,NsP/bytes>> ->
		 NsP
	 end,
    S = 3*4+3*4,
    SNs = case SNs0 of
	      <<>> -> {S, Ns};
	      _ -> {3*4, SNs0}
	  end,
    Colors = case Attrs0 of 
		 <<>> -> none;
		 _ when Type =:= color ->
		     {3*4, Attrs0};
		 _ when Type =:= color_uv ->
		     {5*4, Attrs0};
		 _ -> none		     
	     end,
    UVs = case Attrs0 of
	      <<>> -> none;
	      _ when Type =:= uv ->
		  {2*4, Attrs0};
	      <<_:3/unit:32,UVBin/bytes>> 
		when Type =:= color_uv ->
		  {5*4, UVBin};
	      _ -> none
	  end,
    #vab{face_vs={S,Vs},face_fn={S,Ns}, 
	 face_sn=SNs, face_vc=Colors, face_uv=UVs,
	 mat_map=MatInfo}.

update_1(ChangedVs0, #base{vmap=VMap, v=VsBin0}=Base)
  when is_list(ChangedVs0) ->
    ChangedVs = [{array:get(Id,VMap),Pos} || {Id,Pos} <- ChangedVs0],
    VsList = update_vs(lists:sort(ChangedVs), 0, VsBin0),
    VsBin  = iolist_to_binary(VsList),
    Base#base{v=VsBin};

update_1(#we{vp=Vpos}, #base{vmap=VMap, v=VsBin0}=Base) ->
    Change = fun(Vid, CLid, Acc) ->
		     Pos = array:get(Vid, Vpos),
		     Skip = CLid*16+12,
		     <<_:Skip/binary, VI:4/binary, _/binary>> = VsBin0,
		     [{CLid, Pos, VI}|Acc]
	     end,
    VsInfo = array:sparse_foldl(Change, [], VMap),
    VsBin = << <<X:?F32,Y:?F32,Z:?F32,VI/binary>> 
	       || {_,{X,Y,Z}, VI} <- lists:sort(VsInfo) >>,
    true = size(VsBin) == size(VsBin0), %% Assert
    Base#base{v=VsBin}.
    
update_vs([{Id, {X,Y,Z}}|Vs], Where, Bin) ->
    Skip = Id*4*4-Where,
    <<Head:Skip/binary, _:(4*3)/binary, Rest/binary>> = Bin,
    New = <<X:?F32,Y:?F32,Z:?F32>>,
    [Head, New | update_vs(Vs, Where+Skip+12, Rest)];
update_vs([], _, Bin) -> [Bin].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_data({Type, MatFs}, N, We0, Impl) ->
    {FMap0, We} = gen_fmap(MatFs, We0),
    FMap  = gb_trees:from_orddict(lists:sort(FMap0)),
    Empty = array:new(),
    StartAcc  = {Empty, Empty, {0, Empty}},
    Get = fun(V,Eid,E,Acc) ->  
		  get_face_info(V,Eid,E,Acc,FMap,We)
	  end,
    B0 = case Type of
	     plain -> build_data_plain(FMap0, [], StartAcc, Get, We);
	     _ ->  
		 {Attrs, _Sz} = attrs(Type),
		 build_data_attrs(FMap0, [], [], StartAcc, Get, Attrs, We)
	 end,
    B1 = calc_matmap(MatFs, B0#base{type=Type, level=N}),
    pack_data(B1, Impl).

build_data_plain([{F,_}|Fs], Ftab0, Acc0, Get, We) -> 
    {Vs, Acc} = wings_face:fold(Get, {[], Acc0}, F, We), %% CCW
    build_data_plain(Fs, [Vs|Ftab0], Acc, Get, We);
build_data_plain([], Ftab0, {Vtab, Etab, {_,VMap}}, _, _) ->
    Ftab = reverse(Ftab0),
    #base{v=Vtab, e=Etab, f=Ftab, vmap=VMap}.

build_data_attrs([{F,_}|Fs], Ftab0, Attrs, Acc0, Get, Type, We) -> 
    Attr = wings_va:face_attr(Type, F, We),
    {Vs, Acc} = wings_face:fold(Get, {[], Acc0}, F, We), %% CCW
    build_data_attrs(Fs, [Vs|Ftab0], [Attr|Attrs], Acc, Get, Type, We);
build_data_attrs([], Ftab, Attrs, {Vtab, Etab, {_,VMap}}, _, _, _) ->
    #base{v=Vtab, e=Etab, f=reverse(Ftab), 
	  vmap=VMap, as=reverse(Attrs)}.

gen_fmap(Plan, We = #we{}) ->
    Fmap0=[{_,Last}|_] = gen_fmap(Plan, 0, []),
    %% Add hidden faces but not holes
    {Hidden, Htab} = hidden_fs(We),
    Fmap = reverse(gen_fmap2(Hidden, Last+1, Fmap0)),
    {Fmap, We#we{he=Htab}}.

gen_fmap([{_Mat, Fs}|MatFs], Curr, Fmap0) ->
    Fmap = [{_,Last}|_] = gen_fmap2(Fs, Curr, Fmap0),
    gen_fmap(MatFs, Last+1, Fmap);
gen_fmap([], _, Fmap) ->
    Fmap.

gen_fmap2([Face|Fs], Id, Fmap) ->
    gen_fmap2(Fs, Id+1, [{Face, Id}|Fmap]);
gen_fmap2([],_,Fmap) -> Fmap.

hidden_fs(#we{mirror=none, holes=[], fs=Ftab, he=Htab}) -> 
    {hidden_fs2(gb_trees:iterator(Ftab),none,[]), Htab};
hidden_fs(We = #we{mirror=Mirror, holes=Holes, fs=Ftab, he=Htab}) -> 
    Exclude = case Mirror of
		  none -> Holes;
		  _ -> ordsets:add_element(Mirror, Holes)
	      end,
    He0 = wings_face:to_edges(Exclude, We),
    He = gb_sets:union(gb_sets:from_list(He0), Htab),
    {hidden_fs2(gb_trees:iterator(Ftab),Exclude,[]), He}.

hidden_fs2(Iter0, none, Hidden) ->
    case gb_trees:next(Iter0) of
	{Face, _, Iter} when Face < 0 ->
	    hidden_fs2(Iter, none, [Face|Hidden]);
	_ ->
	    Hidden
    end;
hidden_fs2(Iter0, Exclude, Hidden) ->
    case gb_trees:next(Iter0) of
	{Face, _, Iter} when Face < 0 ->
	    case ordsets:is_element(Face, Exclude) of
		true -> 
		    hidden_fs2(Iter, Exclude, Hidden);
		false ->
		    hidden_fs2(Iter, Exclude, [Face|Hidden])
	    end;
	_ ->
	    Hidden
    end.

calc_matmap(MatMap0, B=#base{f=Ftab0, level=N}) ->
    Mul = trunc(math:pow(4,N-1)),  %% Subd level 
    {Total, MatMap, FMap} = calc_matmap(MatMap0, 0, Ftab0, Mul, [], []),
    B#base{n=Total, mmap=MatMap, fmap=FMap}.

calc_matmap([{Mat,Fs}|Mfs], Start, Ftab0, Mul, FMap0, Acc) ->
    {Next, Ftab, FMap} = calc_matmap_1(Fs, Ftab0, Mul, Start, FMap0),
    calc_matmap(Mfs, Next, Ftab, Mul, FMap, 
		[{Mat, ?GL_QUADS, Start*4, (Next-Start)*4}|Acc]);
calc_matmap([], Total, _, _, FMap, Acc) ->
    {Total, reverse(Acc), gb_trees:from_orddict(lists:sort(FMap))}.

calc_matmap_1([Id|Fs], [Vs|Ftab], Mul, Count, FMap) ->
    Size = length(Vs),
    FInfo = {Id, {Count, Size*Mul}},
    calc_matmap_1(Fs, Ftab, Mul, Count + Size*Mul, [FInfo|FMap]);
calc_matmap_1([], Ftab, _, Count, FMap) ->
    {Count, Ftab, FMap}.

get_face_info(Orig, Eid, E, {Vs, {Vtab0, Etab0, VMap0}}, FMap, We) ->
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

setup_edge(Eid, #edge{vs=OV1,ve=OV2,lf=F1,rf=F2}, 
	   VMap0, FMap, Etab, Htab) ->
    case array:get(Eid, Etab) of
	undefined ->
	    {V1,VMap1} = update_vmap(OV1, VMap0),
	    {V2,VMap}  = update_vmap(OV2, VMap1),
	    {array:set(Eid, {V1,V2,
			     get_face(F1, FMap),
			     get_face(F2, FMap),
			     gb_sets:is_member(Eid,Htab)
			    }, 
		       Etab), VMap};
	_ ->
	    {Etab, VMap0}
    end.

get_face(F, FMap) -> %% when F >= 0 ->
    case gb_trees:lookup(F, FMap) of
	none -> -1;
	{value,Mapped} -> Mapped
    end.

update_vmap(Orig, VM={N,VMap}) ->
    case array:get(Orig, VMap) of
	undefined -> 
	    {N, {N+1,array:set(Orig, N, VMap)}};
	V ->
	    {V, VM}
    end.

pack_data(B=#base{e=Etab0, f=Ftab0}, erlang) ->
    Etab = array:from_list(array:sparse_to_list(Etab0)),
    Ftab = array:from_list(Ftab0),
    B#base{e=Etab, f=Ftab};
pack_data(B0=#base{v=Vtab0, e=Etab0, f=Ftab0}, opencl) ->
    GetFs = fun(Vs, {No, FI, Fs}) ->
		    Len = length(Vs),
		    {No+Len, <<FI/binary, No:?I32, Len:?I32>>,
		     << Fs/binary, 
			(<< <<V:?I32>> || V <- Vs >>)/binary >>}
	    end,
    {_, FI, Ftab} = foldl(GetFs, {0, <<>>, <<>>}, Ftab0),

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
    B = pack_attrs(B0), 
    B#base{v=Vtab, e=Etab, f=Ftab, fi=FI}.

pack_attrs(B=#base{type=plain}) -> B;
pack_attrs(B=#base{type=uv, as=AS0}) -> 
    Pack = fun(FaceUVs, Acc) ->
		   << Acc/binary, 
		      (<< <<(uv_bin(UV))/binary>> 
			  || UV <- FaceUVs >>)/binary >>
	   end,
    AS = foldl(Pack, <<>>, AS0),
    B#base{as=AS};
pack_attrs(B=#base{type=color, as=AS0}) -> 
    Pack = fun(FaceUVs, Acc) ->
		   << Acc/binary, 
		      (<< <<(col_bin(UV))/binary>> 
			  || UV <- FaceUVs >>)/binary >>
	   end,
    AS = foldl(Pack, <<>>, AS0),
    B#base{as=AS};
pack_attrs(B=#base{type=color_uv, as=AS0}) -> 
    Pack = fun(FaceUVs, Acc) ->
		   << Acc/binary, 
		      (<< <<(col_uv_bin(ColUV))/binary>> 
			  || ColUV <- FaceUVs >>)/binary >>
	   end,
    AS = foldl(Pack, <<>>, AS0),
    B#base{as=AS}.

    
uv_bin({U,V}) -> <<U:?F32,V:?F32>>;
uv_bin(_) -> <<0.0:?F32,0.0:?F32>>.
     
col_bin({R,G,B}) -> <<R:?F32,G:?F32,B:?F32>>;
col_bin(_) -> <<1.0:?F32,1.0:?F32,1.0:?F32>>.

col_uv_bin([{R,G,B}|{U,V}]) ->
    <<R:?F32,G:?F32,B:?F32, U:?F32,V:?F32>>;
col_uv_bin([none|{U,V}]) ->
    <<1.0:?F32,1.0:?F32,1.0:?F32, U:?F32,V:?F32>>;
col_uv_bin([{R,G,B}|none]) ->
    <<R:?F32,G:?F32,B:?F32, 0.0:?F32,0.0:?F32>>;
col_uv_bin([none|none]) ->
    <<1.0:?F32,1.0:?F32,1.0:?F32, 0.0:?F32,0.0:?F32>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Opencl Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subdiv_1(N,
	 In = #cl_mem{v=VsIn, f=FsIn, fi=FiIn, e=EsIn, as=AsIn,
		      v_no=NoVs, fs_no=NoFs, e_no=NoEs},
	 Out= #cl_mem{v=VsOut, f=FsOut, e=EsOut, fi=FiOut, as=AsOut,
		      v_no=NoVs1,fs_no=NoFs1, e_no=NoEs1},
	 Type, CL, Wait0)
  when N > 0 ->
    Args1 = [VsIn, FsIn, FiIn, VsOut, FsOut, NoFs, NoVs],
    W0 = cl_apply(gen_faces, Args1, NoFs, Wait0, CL),
    [cl:release_event(Ev) || Ev <- Wait0],
    Args2 = [FsIn, FiIn, VsOut, NoFs, NoVs],
    W1 = cl_apply(add_center, Args2, 1, [W0], CL),

    Args3 = [VsIn, FsIn, EsIn, FiIn, 
	     VsOut, FsOut, EsOut, 
	     NoFs, NoVs, NoEs],
    W2 = cl_apply(gen_edges, Args3, NoEs, [W1], CL),
    Args4 = [VsIn, VsOut, EsIn, NoEs],
    W3 = cl_apply(add_edge_verts, Args4, 1, [W2], CL),

    Args5 = [VsIn,VsOut,NoVs,NoVs1],
    W4 = cl_apply(move_verts, Args5, NoVs1, [W3], CL),
    Wait = case Type of 
	       plain -> W4;
	       color -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   cl_apply(subd_vcolor, Args6, NoFs, [W4], CL);
	       uv -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   cl_apply(subd_uv, Args6, NoFs, [W4], CL);
	       color_uv -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   cl_apply(subd_col_uv, Args6, NoFs, [W4], CL);
	       _ -> %%  unknown ignore
		   W4
	   end,
    [cl:release_event(Ev) || Ev <- [W0,W1,W2,W3]],
    subdiv_1(N-1, Out, 
	     In#cl_mem{fi=FiOut, v_no=NoVs1+NoFs1+NoEs1,
		       fs_no=NoFs1*4, e_no=NoEs1*4},
	     Type, CL, [Wait]);
subdiv_1(_C, ResultBuffs, OutBuffs, _, _, Wait) ->
    #cc_cache{mem=ResultBuffs, old=OutBuffs, wait=Wait}.

gen_vab_2(#cc_cache{mem=Mem,old=Old,wait=Wait}, B=#base{n=NoFs, type=Type}) ->
    CL=#cli{q=Q, vab=Vab} = cl_setup(),
    %% Create #vab{}
    #cl_mem{v=Vs, f=Fs, as=As} = Mem,
    WVab = cl_apply(create_vab_all,[Vs,Fs,Vab,NoFs], NoFs, Wait,CL),
    Attrs = case attrs(Type) of
		{plain,_} -> <<>>;
		{_, Sz} -> 
		    {ok, AData} = cl:enqueue_read_buffer(Q,As,0,NoFs*4*Sz,Wait),
		    {ok, A0} = cl:wait(AData),
		    A0;
		_ -> 
		    <<>>
	    end,
    {ok, WData} = cl:enqueue_read_buffer(Q,Vab,0,NoFs*4*6*4,[WVab]),
    {ok, VabBin} = cl:wait(WData),
    [cl:release_event(Ev) || Ev <- [WVab,WData|Wait]],
    cl_release(Mem, true),
    cl_release(Old, false),
    {VabBin, <<>>, Attrs, B#base.mmap}.

gen_vab_2({_Type, MatFs}, 
	  #cc_cache{mem=Mem,old=Old,wait=Wait}, 
	  #base{fmap=FMap, type=Type}) ->
    CL=#cli{q=Q, vab=Vab, fl=FL} = cl_setup(),
    %% Create #vab{}
    #cl_mem{v=Vs, f=Fs, as=AsIn} = Mem,
    CountF = fun(WFace, [Start|Bin0]) ->
		     {Fstart,FSz} = gb_trees:get(WFace,FMap),
		     Bin = <<
			     Bin0/binary,
			     Fstart:?I32,       %% Start in Fs
			     FSz:?I32,          %% No of faces
			     Start:?I32         %% Start in vab
			   >>,
		     [Start+FSz|Bin]
	     end,
    {NoOutFs,FsBin,MatI} = mat_index(MatFs, CountF, [0|<<>>], []),
    NoInFs = byte_size(FsBin) div 12,
    case NoInFs > 0 of 
	true ->
	    {ok,W1} = cl:enqueue_write_buffer(Q, FL, 0, byte_size(FsBin), 
					      FsBin, []),
	    WVab = cl_apply(create_vab_sel, [Vs,Fs,FL,Vab,NoInFs], 
			    NoInFs, [W1|Wait],CL),
	    Attrs = 
		case attrs(Type) of
		    {plain,_} -> <<>>;
		    {T,Sz} ->
			Func = case T of 
				   color    -> get_sel_vcolor;
				   uv       -> get_sel_uv;
				   [color|uv] -> get_sel_col_uv
			       end,
			#cl_mem{as=AsOut} = Old,
			Args = [FL,AsIn,AsOut,NoInFs],
			AW = cl_apply(Func,Args,NoInFs,[W1|Wait],CL),
			{ok, AData} = 
			    cl:enqueue_read_buffer(Q,AsOut,0,NoOutFs*4*Sz,[AW]),
			{ok, A0} = cl:wait(AData),
			A0
		end,
	    {ok, WData}  = cl:enqueue_read_buffer(Q,Vab,0,NoOutFs*4*6*4,[WVab]),
	    {ok, VabBin} = cl:wait(WData),
	    [cl:release_event(Ev) || Ev <- [WVab,W1|Wait]],
	    cl_release(Mem, true),
	    cl_release(Old, false),
	    {VabBin, <<>>, Attrs, MatI};
	false ->
	    {<<>>, <<>>, <<>>, []}
    end.

mat_index([{Mat,Fs}|MFs], Fun, Acc0 = [Start|_], MI) ->
    Acc = [Next|_] = foldl(Fun, Acc0, Fs),
    NoVs = (Next-Start)*4,
    MatInfo = {Mat,?GL_QUADS,Start*4,NoVs},
    mat_index(MFs, Fun, Acc, [MatInfo|MI]);
mat_index([], _, [Total|Bin], MatInfo) ->
    {Total, Bin, MatInfo}.

attrs(uv) -> % wings_va type and byte size
    {uv, 2*4};
attrs(color) ->
    {color, 3*4};
attrs(color_uv) ->
    {[color|uv], 3*4+2*4};
attrs(plain) ->
    {plain, 0}.

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
	    {ok, MaxWGS} = cl:get_device_info(Device, max_work_group_size),
	    {ok, Kernels0} = cl:create_kernels_in_program(Program),
	    Kernels = [kernel_info(K,Device, MaxWGS) || K <- Kernels0],
	    %% io:format("Kernels ~p~n",[Kernels]),
	    CLI = #cli{context=CL#cl.context,kernels=Kernels,
		       q=Queue, device=Device, cl=CL},
	    cl:release_program(Program),
	    put({?MODULE, cl}, CLI),
	    CLI
    end.

kernel_info(K,Device, MaxWGS) ->
    {ok, WG} = cl:get_kernel_workgroup_info(K, Device, work_group_size),
    {ok, Name} = cl:get_kernel_info(K, function_name),
    #kernel{name=list_to_atom(Name), wg=min(WG,MaxWGS), id=K}.

cl_apply(Name, Args, No, Wait, #cli{q=Q, kernels=Ks}) ->
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
    Event.

%% For now only one level at the time
cl_allocate(Base=#base{fi=Fi, type=Type}, CL0=#cli{context=Ctxt}) ->
    {NoFs,NoEs,NoVs, NoFs1, MaxFs,MaxEs,MaxVs} = verify_size(Base, CL0),
    {ok,FiIn}  = cl:create_buffer(Ctxt, [], byte_size(Fi), Fi),
    {ok,FsIn}  = cl:create_buffer(Ctxt, [], MaxFs*?FACE_SZ),
    {ok,EsIn}  = cl:create_buffer(Ctxt, [], MaxEs*?EDGE_SZ),
    {ok,VsIn}  = cl:create_buffer(Ctxt, [], MaxVs*?VERTEX_SZ),
    
    {ok,FsOut} = cl:create_buffer(Ctxt, [], MaxFs*?FACE_SZ),
    {ok,EsOut} = cl:create_buffer(Ctxt, [], MaxEs*?EDGE_SZ),
    {ok,VsOut} = cl:create_buffer(Ctxt, [], MaxVs*?VERTEX_SZ),
    
    CL = #cli{fi=FiOut} = check_temp_buffs(CL0, MaxFs),

    {AsIn,AsOut} = 
	case attrs(Type) of
	    {_, 0} -> {undefined, undefined};
	    {_, ASz} -> 
		{ok,As1} = cl:create_buffer(Ctxt, [], MaxFs*ASz*4),
		{ok,As2} = cl:create_buffer(Ctxt, [], MaxFs*ASz*4),
		{As1, As2}
	end,
		
    {#cl_mem{v=VsIn, f=FsIn, e=EsIn, fi=FiIn, fi0=FiIn, as=AsIn,
	     v_no=NoVs, fs_no=NoFs, e_no=NoEs},
     #cl_mem{v=VsOut, f=FsOut, e=EsOut, fi=FiOut, fi0=FiIn, as=AsOut,
	     v_no=NoVs+NoFs+NoEs, fs_no=NoFs1, e_no=NoEs*4},
     CL}.

cl_write_input(#base{f=Fs,e=Es,v=Vs, as=As}, 
	       #cl_mem{v=VsIn,f=FsIn,e=EsIn,as=AsIn}, #cl_mem{v=VsOut}, 
	       #cli{q=Q}) ->
    {ok, W1} = cl:enqueue_write_buffer(Q,  VsIn, 0, byte_size(Vs), Vs, []),
    {ok, W2} = cl:enqueue_write_buffer(Q, VsOut, 0, byte_size(Vs), Vs, []),
    {ok, W3} = cl:enqueue_write_buffer(Q,  FsIn, 0, byte_size(Fs), Fs, []),
    {ok, W4} = cl:enqueue_write_buffer(Q,  EsIn, 0, byte_size(Es), Es, []),
    Last = case As of
	       undefined -> [];
	       _ ->
		   Sz = byte_size(As),
		   {ok, W5} = cl:enqueue_write_buffer(Q, AsIn, 0, Sz, As, []),
		   [W5]
	   end,
    [W1,W2,W3,W4|Last].
    
cl_release(#cl_mem{v=Vs,f=Fs,e=Es,fi0=Fi0,as=As}, All) ->
    Vs /= undefined andalso cl:release_mem_object(Vs),
    Fs /= undefined andalso cl:release_mem_object(Fs),
    Es /= undefined andalso cl:release_mem_object(Es),
    As /= undefined andalso cl:release_mem_object(As),
    All andalso cl:release_mem_object(Fi0).

check_temp_buffs(CL=#cli{context=Ctxt, 
			 vab=Vab0, vab_sz=VabSz0, 
			 fl=FL0, fl_sz=FLSz0, 
			 fi=Fi0, fi_sz=FiSz0
			 %%sn = SN0, sn_vz = SNSz0
			}, MaxFs0) ->
    MaxFs = trunc(MaxFs0*1.5),  
    %% Overallocate so we don't need new buffers all the time
    GenFi = fun() -> 
		    << <<(C*4):?I32, 4:?I32>> || 
			C <- lists:seq(0, MaxFs-1) >> 
	    end,
    {Vab,VabSz} = check_temp(Vab0,VabSz0,MaxFs*(3+3)*4*4,
			     Ctxt,[write_only],none),
    {FL,FLSz} = check_temp(FL0,FLSz0,MaxFs*3*4,
			   Ctxt,[read_only],none),
    {Fi,FiSz} = check_temp(Fi0,FiSz0,MaxFs*2*4,
			   Ctxt,[read_only],GenFi),

    CLI = CL#cli{vab=Vab, vab_sz=VabSz, 
		 fl=FL,   fl_sz=FLSz, 
		 fi=Fi,   fi_sz=FiSz},
    put({?MODULE, cl}, CLI),
    CLI.

check_temp(Buff, Current, Req, _, _, _) 
  when Current >= Req ->
    {Buff, Current};
check_temp(undefined, _, Req, Ctxt, Opt, none) ->
    {ok, Buff} = cl:create_buffer(Ctxt, Opt, Req),
    {Buff, Req};
check_temp(undefined, _, Req, Ctxt, Opt, Fun) ->
    {ok,Buff} = cl:create_buffer(Ctxt, Opt, Req, Fun()),
    {Buff, Req};
check_temp(Buff0, _, Req, Ctxt, Opt, Data) ->
    cl:release_mem_object(Buff0),
    check_temp(undefined, 0, Req, Ctxt, Opt, Data).

verify_size(#base{fi=Fi, e=Es, v=Vs, level=N}, #cli{device=Device}) ->
    NoFs = size(Fi) div 8,
    NoEs = size(Es) div ?EDGE_SZ,
    NoVs = size(Vs) div ?VERTEX_SZ,
    
    Skip = size(Fi) - 8,
    <<_:Skip/binary, NoFs0:?I32, LastFc:?I32>> = Fi,
    NoFs1 = NoFs0+LastFc,
    {ok, DevTotal} = cl:get_device_info(Device, max_mem_alloc_size),
    case verify_size_1(N-1, N, NoFs1, NoEs*4, NoVs+NoEs+NoFs, DevTotal) of
	false -> 
	    io:format("Can not subdivide, out of memory~n",[]),
	    exit(out_of_memory);
	{MaxFs, MaxEs, MaxVs} ->
	    {NoFs, NoEs, NoVs, NoFs1, MaxFs, MaxEs, MaxVs}
    end.

%% Does this function do anything good?
verify_size_1(N, No, Fs, Es, Vs, CardMax) ->
    VertexSz = (3+3)*4*4,
    Total = Fs*VertexSz+2*(Fs*?FACE_SZ+Es*?EDGE_SZ+Vs*?VERTEX_SZ),
    case Total < CardMax of
	true when N == 0 ->
	    {Fs,Es,Vs};
	true -> 
	    case verify_size_1(N-1, No, Fs*4, Es*4, Vs+Fs+Es, CardMax) of
		false -> 
		    io:format("Out of memory, does not meet the number of sub-division"
			      "levels ~p(~p)~n",[No-N,No]),
		    {Fs,Es,Vs};
		Other -> Other
	    end;
	false ->
	    false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DEBUG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(DO_DEBUG).
%-ifdef(true).
cl_vs(Str, N, Vs, Count, #cli{q=Q}, Wait0) ->
    Wait = if is_list(Wait0) -> Wait0; true -> [Wait0] end,
    {ok, W1} = cl:enqueue_read_buffer(Q,Vs,0,Count*?VERTEX_SZ,Wait),
    {ok, VsBin} = cl:wait(W1),

    {ok, F} = file:open(Str ++ "_" ++ integer_to_list(N), [write]),
    try 
	W = fun(<<X:?F32,Y:?F32,Z:?F32,VI0:?F32>>,V) -> 
		    VI = {trunc(VI0) div 4, trunc(VI0) rem 4},
		    ok=io:format(F,"{~w, {~.4f,~.4f,~.4f}, ~w}~n", [V,X,Y,Z,VI]),
		    V+1
	    end,
	lists:foldl(W, 0, [D || <<D:?VERTEX_SZ/binary>> <= VsBin])
    catch E:R ->
	    io:format("Failed ~p:~p ~p~n",[E,R,erlang:get_stacktrace()])
    after 
	file:close(F)
    end.

cl_face(Str, N, Vs, Count, #cli{q=Q}, Wait) ->
    {ok, W1} = cl:enqueue_read_buffer(Q,Vs,0,Count*?FACE_SZ,[Wait]),
    {ok, FsBin} = cl:wait(W1),

    {ok, FD} = file:open(Str ++ "_" ++ integer_to_list(N), [write]),
    try 
	W = fun(<<X:?I32,Y:?I32,Z:?I32,VI0:?I32>>,V) -> 
		    ok=io:format(FD,"{~w, {~w,~w,~w,~w}}.~n", 
				 [V,X,Y,Z,VI0]),
		    V+1
	    end,
	lists:foldl(W, 0, [D || <<D:?FACE_SZ/binary>> <= FsBin])
    catch E:R ->
	    io:format("Failed ~p:~p ~p~n",[E,R,erlang:get_stacktrace()])
    after 
	file:close(FD)	
    end.


cl_destroy(Mem) ->
    %% During DEBUGGING REMOVE LATER BUGBUG
    cl_release(Mem, true),
    #cli{q=Q,kernels=Ks,context=C, 
	 fl=Fl, fi=Fi,vab=Vab} = get({?MODULE, cl}),

    cl:release_mem_object(Vab),
    cl:release_mem_object(Fi),
    cl:release_mem_object(Fl),

    erase({?MODULE, cl}),
    cl:release_queue(Q),
    [cl:release_kernel(K) || #kernel{id=K} <- Ks],
    cl:release_context(C),
    ok.

print_base(#base{v=Vs,f=Fs,fi=Fi,e=Es,n=N}) ->
    io:format("Vs = ~w~n",[Vs]),
    io:format("Fs = ~w~n",[Fs]),
    io:format("Es = ~w~n",[Es]),
    ok.

-endif.

