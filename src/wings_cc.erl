%%
%%  wings_cc.erl --
%%
%%     This module implements the smooth proxy in opencl.
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
-export([init/3, update/2, gen_vab/1, gen_vab/2]).

%% debug 
-import(lists, [reverse/1, foldl/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("cl/include/cl.hrl").

-compile(export_all).

-define(PL_UNITS, 4).

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
-record(cls,      
	{cl,
	 %% CL temp buffers and respective sizes	  
	 vab, vab_sz=0, fl, fl_sz=0, fi, fi_sz=0,
	 sn, sn_sz %% Smooth normals ? can reuse fl?
	}).

-record(cl_mem,   {v, v_no, f, fs_no, e, e_no, fi, fi0, as, max_vs}).
-record(cc_cache, {mem, old, wait}).

-define(EDGE_SZ, (4*4)).
-define(FACE_SZ, (4*4)).
-define(VERTEX_SZ, ((3*4)+4)).
-define(LOCK_SZ, 32).
%%-define(DEFAULT, erlang).
-define(DEFAULT, opencl).

%%%% API %%%%%%%%%

%% Returns opaque data
init(Plan, Level, We) when is_integer(Level) ->
    build_data(Plan,Level,We,?DEFAULT);

init(Plan, #base{level=Level}, We)  ->
    build_data(Plan,Level,We,?DEFAULT).

%% Update state with new vertex positions
update(ChangedVs, Data) ->
    case ?DEFAULT of 
	erlang ->  wings_cc_ref:update(ChangedVs, Data);
	_ ->       update_1(ChangedVs, Data)
    end.
    
%% Generates a subdivided #vab{}
gen_vab(Base) ->
    try
	case subdiv(Base, ?DEFAULT) of
	    skip -> create_vab(<<>>, []);
	    Data ->
		gen_vab_1(Data, Base)
	end
    catch
	exit:{out_of_resources, Wanted, CardMax} ->
	    io:format(?__(1,"OpenCL subd failed: wanted ~pMB only ~pMB available~n"), 
		      [Wanted, CardMax]),
	    DecBase = decrease_level(Base),
	    gen_vab(DecBase)	   
    end.
%% Generates a subdivided #vab{} from Material Plan
gen_vab(Plan, Base) ->
    try 
	case subdiv(Base, ?DEFAULT) of
	    skip -> create_vab(<<>>, []);
	    Data ->
		gen_vab_1(Plan, Data, Base)
	end
    catch
	exit:{out_of_resources, Wanted, CardMax} ->
	    io:format(?__(1,"OpenCL subd failed: wanted ~pMB only ~pMB available~n"), 
		      [Wanted, CardMax]),
	    DecBase = decrease_level(Base),
	    gen_vab(Plan, DecBase)
    end.

%% Subdivide mesh
subdiv(Base = #base{level=N, n=Total, type=Type}, Impl) when Total > 0 ->
    case Impl of
	opencl ->
	    {In,Out,CL} = cl_allocate(Base, cl_setup()),
	    Wait = cl_write_input(Base, In, Out, CL),
	    subdiv_1(N, In, Out, Type, CL, Wait);
	erlang ->		
	    wings_cc_ref:subdiv(Base)
    end;
subdiv(_, _) ->
    skip.

%% Generates a vab (and updates Data)
%% Returns Vab
gen_vab_1(Data0, Base) ->
    case ?DEFAULT of
	erlang -> create_vab(wings_cc_ref:gen_vab(Data0, Base), Base);
	opencl -> create_vab(gen_vab_2(Data0, Base), Base)
    end.

gen_vab_1(Plan, Data, Base) ->
    case ?DEFAULT of
	erlang -> create_vab(wings_cc_ref:gen_vab(Plan, Data, Base), Base);
	opencl -> create_vab(gen_vab_2(Plan, Data, Base), Base)
    end.

create_vab({Vs, SNs0, {Attrs0, Tangents0}, Edges, MatInfo}, #base{type=Type}) ->
    Ns = case Vs of
	     <<>> -> Vs;
	     <<_:3/unit:32,NsP/bytes>> ->
		 NsP
	 end,
    S = 3*4+3*4,
    SNs = case SNs0 of
	      <<>> -> {S, Ns};
	      _ -> {4*4, SNs0}  %% Special case easier cl code
	  end,
    Colors = case Attrs0 of 
		 <<>> -> none;
		 _ when Type =:= color ->
		     {3*4, Attrs0};
		 _ when Type =:= color_uv; Type =:= color_uv_tangent ->
		     {5*4, Attrs0};
		 _ -> none		     
	     end,
    UVs = case Attrs0 of
	      <<>> -> none;
	      _ when Type =:= uv; Type =:= uv_tangent ->
		  {2*4, Attrs0};
	      <<_:3/unit:32,UVBin/bytes>> 
		when Type =:= color_uv; Type =:= color_uv_tangent ->
		  {5*4, UVBin};
	      _ -> none
	  end,
    Tangents = case Tangents0 of
		   none -> none;
		   Ts when is_binary(Ts) ->
		       {4*4, Tangents0}
	       end,
    #vab{face_vs={S,Vs},face_fn={S,Ns}, face_es=Edges,
	 face_sn=SNs, face_vc=Colors, face_uv=UVs, face_ts=Tangents,
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
    VSize = length(Vs),
    Size = VSize*Mul,
    FInfo = {Id, {Count, Size}},
    calc_matmap_1(Fs, Ftab, Mul, Count + Size, [FInfo|FMap]);
calc_matmap_1([], Ftab, _, Count, FMap) ->
    {Count, Ftab, FMap}.

decrease_level(#base{level=Level}) when Level =< 1 ->
    throw(to_large);
decrease_level(Base = #base{n=Total, level=Level, mmap=MatMap0, fmap=Fmap0}) ->
    io:format(?__(1,"Could not acquire needed memory, " 
		  "decreasing to subd level ~p~n"), [Level-1]),
    MatMap = [{Mat, ?GL_QUADS, Start div 4, Stop div 4} || 
		 {Mat, ?GL_QUADS, Start, Stop} <- MatMap0],
    Fmap   = gb_trees:map(fun(_, {Count, Size}) -> 
				  {Count div 4, Size div 4} 
			  end,
			  Fmap0),
    Base#base{n=Total div 4, level=Level-1, mmap=MatMap, fmap=Fmap}.

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
		    VI = (Vc bsl 2) bor min(Hc,3),
		    << Bin/binary,
		       X1:?F32,Y1:?F32,Z1:?F32,VI:?F32 >>
	    end,
    Vtab = array:foldl(GetVs, <<>>, Vtab0),
    B = pack_attrs(B0), 
    B#base{v=Vtab, e=Etab, f=Ftab, fi=FI}.

pack_attrs(B=#base{type=plain}) -> B;
pack_attrs(B=#base{type=color, as=AS0}) -> 
    Pack = fun(FaceUVs, Acc) ->
		   << Acc/binary, 
		      (<< <<(col_bin(UV))/binary>> 
			  || UV <- FaceUVs >>)/binary >>
	   end,
    AS = foldl(Pack, <<>>, AS0),
    B#base{as=AS};
pack_attrs(B=#base{type=Type, as=AS0}) when Type =:= uv; Type =:= uv_tangent -> 
    Pack = fun(FaceUVs, Acc) ->
		   << Acc/binary, 
		      (<< <<(uv_bin(UV))/binary>> 
			  || UV <- FaceUVs >>)/binary >>
	   end,
    AS = foldl(Pack, <<>>, AS0),
    B#base{as=AS};
pack_attrs(B=#base{type=Type, as=AS0}) when Type =:= color_uv; Type =:= color_uv_tangent -> 
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
		      v_no=NoVsOut,fs_no=NoFs1, e_no=NoEs1},
	 Type, CLS=#cls{cl=CL}, Wait0)
  when N > 0 ->
    Args1 = [VsIn, FsIn, FiIn, VsOut, FsOut, NoFs, NoVs],
    W0 = wings_cl:cast(gen_faces, Args1, NoFs, Wait0, CL),
    [cl:release_event(Ev) || Ev <- Wait0],
    Args2 = [FsIn, FiIn, VsOut, NoFs, NoVs, NoVsOut],
    W1 = wings_cl:cast(add_center, Args2, ?PL_UNITS, [W0], CL),

    Args3 = [VsIn, FsIn, EsIn, FiIn,VsOut, FsOut, EsOut, NoFs, NoVs, NoEs],
    W2 = wings_cl:cast(gen_edges, Args3, NoEs, [W1], CL),
    Args4 = [VsIn, VsOut, EsIn, NoEs, NoVsOut],
    W3 = wings_cl:cast(add_edge_verts, Args4, ?PL_UNITS, [W2], CL),

    Args5 = [VsIn,VsOut,NoVs,NoVsOut],
    W4 = wings_cl:cast(move_verts, Args5, NoVsOut, [W3], CL),
    Wait = case Type of 
	       plain -> W4;
	       color -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   wings_cl:cast(subd_vcolor, Args6, NoFs, [W4], CL);
	       T when T =:= uv; T=:= uv_tangent -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   wings_cl:cast(subd_uv, Args6, NoFs, [W4], CL);
	       T when T =:= color_uv; T=:= color_uv_tangent -> 
		   Args6 = [AsIn, FiIn, AsOut, NoFs],
		   wings_cl:cast(subd_col_uv, Args6, NoFs, [W4], CL);
	       _ -> %%  unknown ignore
		   W4
	   end,
    [cl:release_event(Ev) || Ev <- [W0,W1,W2,W3]],
    subdiv_1(N-1, Out, 
	     In#cl_mem{fi=FiOut, v_no=NoVsOut+NoFs1+NoEs1,
		       fs_no=NoFs1*4, e_no=NoEs1*4},
	     Type, CLS, [Wait]);
subdiv_1(_C, ResultBuffs, OutBuffs, _, _, Wait) ->
    #cc_cache{mem=ResultBuffs, old=OutBuffs, wait=Wait}.

gen_vab_2(#cc_cache{mem=Mem,old=Old,wait=Wait}, B=#base{n=NoFs,level=N,type=Type}) ->
    #cls{cl=CL, vab=Vab} = cl_setup(),
    %% Create #vab{}
    #cl_mem{v=Vs, v_no=NoVs, f=Fs, e=Es, e_no=NoEs, as=As} = Mem,
    WVab = wings_cl:cast(create_vab_all,[Vs,Fs,Vab,NoFs], NoFs, Wait,CL),
    Attrs = gen_attrs(attrs(Type), As, NoFs, Wait, CL),
    WData = wings_cl:read(Vab,NoFs*4*6*4,[WVab],CL),
    {ok, VabBin} = cl:wait(WData),
    Tangents = gen_tangents(Type, Vs, Fs, As, Vab, NoVs, NoFs, Old#cl_mem.as, CL),
    Edges = gen_edges([Es, Vs, As, N, NoEs], [], CL), %% Writes to As must be after gen_tangents
    Smooth = gen_smooth_normals(Vab, NoFs, Mem, Old, [], CL),
    
    [cl:release_event(Ev) || Ev <- [WVab,WData|Wait]],
    cl_release(Mem, true),
    cl_release(Old, false),
    {VabBin, Smooth, {Attrs, Tangents}, Edges, B#base.mmap}.

gen_vab_2({_Type, MatFs}, 
	  #cc_cache{mem=Mem, wait=Wait0, old=Old}, 
	  #base{fmap=FMap, type=Type}) ->
    #cls{cl=CL, vab=Vab, fl=FL} = cl_setup(),
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
	    W1 = wings_cl:write(FL, FsBin, CL),
	    Wait = [W1|Wait0],
	    Args0 = [Vs,Fs,FL,Vab,NoInFs],
	    WVab = wings_cl:cast(create_vab_sel,Args0,NoInFs,Wait,CL),
	    #cl_mem{as=AsOut} = Old,
	    VabSz = NoOutFs*4*6*4,
	    WData = wings_cl:read(Vab,VabSz,[WVab],CL),	    
	    {ok, VabBin} = cl:wait(WData),
	    Args1 = [FL,AsIn,AsOut,NoInFs],
	    Attrs = gen_sel_attr(Type, Args1, NoOutFs,Wait,CL),
	    [cl:release_event(Ev) || Ev <- [WVab,W1|Wait]],
	    cl_release(Mem, true),
	    cl_release(Old, false),
	    {VabBin, <<>>, {Attrs, none}, none, MatI};
	false ->
	    {<<>>, <<>>, {<<>>, none}, none, []}
    end.
    
mat_index([{Mat,Fs}|MFs], Fun, Acc0 = [Start|_], MI) ->
    Acc = [Next|_] = foldl(Fun, Acc0, Fs),
    NoVs = (Next-Start)*4,
    MatInfo = {Mat,?GL_QUADS,Start*4,NoVs},
    mat_index(MFs, Fun, Acc, [MatInfo|MI]);
mat_index([], _, [Total|Bin], MatInfo) ->
    {Total, Bin, MatInfo}.

gen_attrs({plain,_}, _As, _Sz, _Wait, _CL) -> <<>>;
gen_attrs({_,Sz}, As, NoFs, Wait, CL) -> 
    AData = wings_cl:read(As,NoFs*4*Sz,Wait,CL),
    {ok, A0} = cl:wait(AData),
    A0.

gen_edges(Args, Wait, CL) ->
    Type = wings_pref:get_value(proxy_shaded_edge_style),
    gen_edges(Type, Args, Wait, CL).
gen_edges(all, _Args, _Wait, _CL) -> none;
gen_edges(cage, _Args, _Wait, _CL) -> none;
gen_edges(some, [Es,Vs,As,N,TotNoEs], Wait, CL) -> 
    Ediv = trunc(math:pow(4,N)),
    EMul = trunc(math:pow(2,N)),
    NoEs = EMul * (TotNoEs div Ediv),
    Args = [Es,Vs,As,N,NoEs],
    EWait0 = wings_cl:cast(gen_some_edges, Args, NoEs, Wait, CL),
    EWait1 = wings_cl:read(As, NoEs*2*3*4, [EWait0], CL),
    {ok, EsBin} = cl:wait(EWait1),
    {0, EsBin}.

gen_tangents(Type, Vs, Fs, As, Vab, NoVs, NoFs, Temp, CL) 
  when Type =:= uv_tangent; Type =:= color_uv_tangent ->
    C1 = wings_cl:cast(clearf, [Temp,4,NoVs*2], NoVs*2, [], CL),
    C2 = case Type of 
	     uv_tangent -> 
		 wings_cl:cast(gen_tangents_uv, [Vs, Fs, As, Temp, NoFs, NoVs], ?PL_UNITS, [C1], CL);
	     color_uv_tangent ->
		 wings_cl:cast(gen_tangents_col_uv, [Vs, Fs, As, Temp, NoFs, NoVs], ?PL_UNITS, [C1], CL)
	 end,
    Out = As,
    C3 = wings_cl:cast(gen_tangents, [Fs, Vab, Temp, Out, NoFs, NoVs], NoFs, [C2], CL),
    C4 = wings_cl:read(Out,NoFs*4*4*4,[C3],CL),
    {ok, OutBin} = cl:wait(C4),
    OutBin;
gen_tangents(_, _, _, _, _, _, _, _, _) ->
    none.

gen_smooth_normals(Vab, NoFs, 
		   #cl_mem{f=Fs,e=Es,e_no=NoEs,v=VsNs,v_no=NoVs,as=Out1}, 
		   #cl_mem{v=Vs,as=Out2}, Wait, CL) ->
    VsOut = NoFs*4,
    C1 = wings_cl:cast(clearf, [VsNs,4,NoVs], NoVs, Wait, CL),
    C2 = wings_cl:cast(clearf, [Out1,4,VsOut], VsOut, Wait, CL),
    C3 = wings_cl:cast(clearf, [Out2,4,VsOut], VsOut, Wait, CL),
    Args0 = [Fs,Vab,VsNs,NoFs,NoVs],
    Pass0 = wings_cl:cast(smooth_ns_pass0, Args0, ?PL_UNITS, [C1], CL),
    Args1 = [Es,Fs,Vab,Out1,Out2,NoEs],
    Pass1 = wings_cl:cast(smooth_ns_pass1, Args1, NoEs, [C3,C2,Pass0], CL),
    Args2 = [Fs,Vs,VsNs,Vab,Out1,Out2,NoFs,NoVs],
    Pass2 = wings_cl:cast(smooth_ns, Args2, NoFs, [Pass1], CL),

    WData = wings_cl:read(Out1,VsOut*4*4,[Pass2],CL),
    {ok, OutBin} = cl:wait(WData),
    OutBin.

gen_sel_attr(plain, _, _, _, _) ->
    <<>>;
gen_sel_attr(Type,[FL,AsIn,AsOut,NoInFs],NoOutFs,Wait,CL) ->
    {T,Sz} = attrs(Type), 
    Func = case T of 
	       color    -> get_sel_vcolor;
	       uv       -> get_sel_uv;
	       [color|uv] -> get_sel_col_uv
	   end,
    Args = [FL,AsIn,AsOut,NoInFs],
    AW = wings_cl:cast(Func,Args,NoInFs,Wait,CL),
    AData = wings_cl:read(AsOut,NoOutFs*4*Sz,[AW],CL),
    {ok, A0} = cl:wait(AData),
    A0.

attrs(uv) -> % wings_va type and byte size
    {uv, 2*4};
attrs(uv_tangent) -> % wings_va type and byte size
    {uv, 2*4};
attrs(color) ->
    {color, 3*4};
attrs(color_uv) ->
    {[color|uv], 3*4+2*4};
attrs(color_uv_tangent) ->
    {[color|uv], 3*4+2*4};
attrs(plain) ->
    {plain, 0}.

cl_setup() ->
    case get({?MODULE, cl}) of 
	undefined -> 
	    try 
		cl_setup_1()
	    catch _:Reason ->
		    io:format("CL setup error: ~p ~p~n",[Reason, erlang:get_stacktrace()]),
		    wings_pref:set_value(proxy_opencl_level, 0),
		    wings_u:error_msg(?__(1, "Could not setup OpenCL, disabling proxy smooth."))
	    end;
	CL ->
	    CL
    end.

cl_setup_1() ->
    CL0 = wings_cl:compile("cc_subdiv.cl", wings_cl:setup()),
    CL = #cls{cl=CL0},
    put({?MODULE, cl}, CL),
    CL.


%% For now only one level at the time
cl_allocate(Base=#base{fi=Fi, type=Type}, CL0=#cls{cl=CLI}) ->
    Ctxt = wings_cl:get_context(CLI),
    {NoFs,NoEs,NoVs, NoFs1, MaxFs,MaxEs,MaxVs} = verify_size(Base, CL0),

    {_, ASz0} = attrs(Type),
    ASz = max(ASz0, 4*4),  %% Also used for smooth normals 

    Sizes = [Fi, MaxFs*?FACE_SZ, MaxEs*?EDGE_SZ, MaxVs*?VERTEX_SZ*?PL_UNITS,
	     MaxFs*?FACE_SZ, MaxEs*?EDGE_SZ, MaxVs*?VERTEX_SZ*?PL_UNITS,
	     MaxFs*ASz*4, MaxFs*ASz*4],
	     
    Buffs = create_buffers(Ctxt, Sizes, []),
    [FiIn,FsIn,EsIn,VsIn,  FsOut,EsOut,VsOut,  AsIn,AsOut] = Buffs,
    
    CL = #cls{fi=FiOut} = check_temp_buffs(CL0, Ctxt, MaxFs, Buffs),
    put({?MODULE, cl}, CL),

    
    {#cl_mem{v=VsIn, f=FsIn, e=EsIn, fi=FiIn, fi0=FiIn, as=AsIn,
	     v_no=NoVs, fs_no=NoFs, e_no=NoEs, max_vs=MaxVs},
     #cl_mem{v=VsOut, f=FsOut, e=EsOut, fi=FiOut, fi0=FiIn, as=AsOut,
	     v_no=NoVs+NoFs+NoEs, fs_no=NoFs1, e_no=NoEs*4, max_vs=MaxVs},
     CL}.

create_buffers(Ctxt, [Size|Szs], Acc) when is_integer(Size) ->
    case cl:create_buffer(Ctxt, [], Size) of	
	{ok,Buffer} ->
	    create_buffers(Ctxt, Szs, [Buffer|Acc]);
	{error, out_of_resources} ->
	    release_buffers(Acc, true)
    end;
create_buffers(Ctxt, [Binary|Szs], Acc) ->
    case cl:create_buffer(Ctxt, [], byte_size(Binary), Binary) of	
	{ok,Buffer} ->
	    create_buffers(Ctxt, Szs, [Buffer|Acc]);
	{error, out_of_resources} ->
	    release_buffers(Acc, true)
    end;
create_buffers(_, [], Buffers) ->
    lists:reverse(Buffers).

release_buffers(Buffers, true) ->
    case get({?MODULE, cl}) of
	undefined ->
	    ok;
	#cls{cl=CL, vab=Vab, fl=FL, fi=FI} ->
	    Vab /= undefined andalso cl:release_mem_object(Vab),
	    FL /= undefined andalso cl:release_mem_object(FL),
	    FI /= undefined andalso cl:release_mem_object(FI),
	    put({?MODULE, cl}, #cls{cl=CL})
    end,
    release_buffers(Buffers, false);
release_buffers(Buffers, false) ->
    [cl:release_mem_object(Buff) || Buff <- Buffers],
    exit({out_of_resources, unknown, unknown}).


cl_write_input(#base{f=Fs,e=Es,v=Vs, as=As}, 
	       #cl_mem{v=VsIn,f=FsIn,e=EsIn,as=AsIn,max_vs=MaxVs}, 
	       #cl_mem{v=VsOut},
	       #cls{cl=CL}) ->
    NoVs = MaxVs * ?PL_UNITS,
    Wait0 = wings_cl:cast(clearf, [VsIn, 4, NoVs], NoVs, [], CL),
    Wait1 = wings_cl:cast(clearf, [VsOut, 4, NoVs], NoVs, [], CL), 
    W3 = wings_cl:write(FsIn,  Fs, CL), 
    W4 = wings_cl:write(EsIn,  Es, CL), 
    {ok, _} = cl:wait(Wait0), {ok, _} = cl:wait(Wait1),
    W1 = wings_cl:write(VsIn,  Vs, CL),
    W2 = wings_cl:write(VsOut, Vs, CL),
    Last = case As of
	       undefined -> [];
	       _ ->
		   W5 = wings_cl:write(AsIn, As, CL), 
		   [W5]
	   end,
    [W1,W2,W3,W4|Last].
    
cl_release(#cl_mem{v=Vs,f=Fs,e=Es,fi0=Fi0,as=As}, All) ->
    Vs /= undefined andalso cl:release_mem_object(Vs),
    Fs /= undefined andalso cl:release_mem_object(Fs),
    Es /= undefined andalso cl:release_mem_object(Es),
    As /= undefined andalso cl:release_mem_object(As),
    All andalso cl:release_mem_object(Fi0).

check_temp_buffs(CL=#cls{
			 vab=Vab0, vab_sz=VabSz0, 
			 fl=FL0, fl_sz=FLSz0, 
			 fi=Fi0, fi_sz=FiSz0
			 %%sn = SN0, sn_vz = SNSz0
			}, Ctxt, MaxFs0, Buffs) ->
    MaxFs = MaxFs0,
    GenFi = fun() -> 
		    << <<(C*4):?I32, 4:?I32>> || 
			C <- lists:seq(0, MaxFs-1) >> 
	    end,
    {Vab,VabSz} = try 
		      check_temp(Vab0,VabSz0,MaxFs*(3+3)*4*4,
				 Ctxt,[write_only],none)
		  catch error:{badmatch, _} ->
			  cl:release_mem_object(FL0),
			  cl:release_mem_object(Fi0),
			  release_buffers(Buffs, false)
		  end,
    {FL,FLSz} = try check_temp(FL0,FLSz0,MaxFs*3*4,
			       Ctxt,[read_only],none)
		catch error:{badmatch, _} ->
			cl:release_mem_object(Vab),
			cl:release_mem_object(Fi0),
			release_buffers(Buffs, false)
		end,

    {Fi,FiSz} = try check_temp(Fi0,FiSz0,MaxFs*2*4,
			       Ctxt,[read_only],GenFi)
		catch error:{badmatch, _} ->
			cl:release_mem_object(Vab),
			cl:release_mem_object(FL),
			release_buffers(Buffs, false)
		end,

    CL#cls{vab=Vab, vab_sz=VabSz, 
	   fl=FL,   fl_sz=FLSz, 
	   fi=Fi,   fi_sz=FiSz
	  }.

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

verify_size(#base{fi=Fi, e=Es, v=Vs, level=N}, #cls{cl=CL}) ->
    NoFs = size(Fi) div 8,
    NoEs = size(Es) div ?EDGE_SZ,
    NoVs = size(Vs) div ?VERTEX_SZ,
    
    Skip = size(Fi) - 8,
    <<_:Skip/binary, NoFs0:?I32, LastFc:?I32>> = Fi,
    NoFs1 = NoFs0+LastFc,
    Device = wings_cl:get_device(CL),
    {ok, DevTotal} = cl:get_device_info(Device, max_mem_alloc_size),
    {MaxFs, MaxEs, MaxVs} = verify_size_1(N-1, N, NoFs1, NoEs*4, NoVs+NoEs+NoFs, DevTotal),
    {NoFs, NoEs, NoVs, NoFs1, MaxFs, MaxEs, MaxVs}.

%% Does this function do anything good?
verify_size_1(N, No, Fs, Es, Vs, CardMax) ->
    VertexSz = (3+3)*4*4,
    Temp  = Fs*VertexSz+Fs*3*4+Fs*2*4,
    Total = Temp+2*(Fs*?FACE_SZ+Es*?EDGE_SZ+Vs*?VERTEX_SZ*?PL_UNITS+Fs*4*4),
    case Total < CardMax of
	true when N == 0 ->
	    {Fs,Es,Vs};
	true -> 
	    verify_size_1(N-1, No, Fs*4, Es*4, Vs+Fs+Es, CardMax);
	false ->
	    exit({out_of_resources,Total div 1024, CardMax div 1024})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DEBUG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(DO_DEBUG).
%-ifdef(PL_UNITS).
cl_vs(Str, N, Vs, Count, CL, Wait0) ->
    Wait = if is_list(Wait0) -> Wait0; true -> [Wait0] end,
    W1 = wings_cl:read(Vs,Count*?VERTEX_SZ,Wait,CL),
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

cl_es(Str, N, Es, Count, CL, Wait0) ->
    Wait = if is_list(Wait0) -> Wait0; true -> [Wait0] end,
    W1 = wings_cl:read(Es,Count*?EDGE_SZ,Wait,CL),
    {ok, EsBin} = cl:wait(W1),

    {ok, F} = file:open(Str ++ "_" ++ integer_to_list(N), [write]),
    try 
	W = fun(<<X:?I32,Y:?I32,Z:?I32,W:?I32>>,E) -> 
		    ok=io:format(F,"{~w, {~.4w,~.4w,~.4w,~.4w}~n", [E,X,Y,Z,W]),
		    E+1
	    end,
	lists:foldl(W, 0, [D || <<D:?EDGE_SZ/binary>> <= EsBin])
    catch E:R ->
	    io:format("Failed ~p:~p ~p~n",[E,R,erlang:get_stacktrace()])
    after 
	file:close(F)
    end.

cl_face(Str, N, Vs, Count, CL, Wait0) ->
    Wait = if is_list(Wait0) -> Wait0; true -> [Wait0] end,
    W1 = wings_cl:read(Vs,Count*?FACE_SZ,Wait,CL),
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
    #cls{fl=Fl, fi=Fi,vab=Vab} = get({?MODULE, cl}),

    cl:release_mem_object(Vab),
    cl:release_mem_object(Fi),
    cl:release_mem_object(Fl),

    erase({?MODULE, cl}),
    %% cl:release_queue(Q),
    %% [cl:release_kernel(K) || #kernel{id=K} <- Ks],
    %% cl:release_context(C),
    ok.

print_base(#base{v=Vs,f=Fs,fi=Fi,e=Es,n=N}) ->
    io:format("Vs = ~w~n",[Vs]),
    io:format("Fs = ~w~n",[Fs]),
    io:format("Es = ~w~n",[Es]),
    ok.

-endif.

