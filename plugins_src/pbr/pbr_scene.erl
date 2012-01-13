%%
%%  pbr_scene.erl
%%
%%     Pbr scene handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_scene).

-define(NEED_OPENGL,1).
-include_lib("wings/src/wings.hrl").
-include("pbr.hrl").

-export([init/3,
	 intersect/2, intersect_data/1,	 
	 get_lights/1,
	 get_materials/1,
	 get_infinite_light/1,
	 get_face_info/6,
	 vertices/1,
	 triangles/1,
	 normals/1,
	 uvs/1,
	 vertex_colors/1,
	 mesh2mat/1,
	 bb/1]).

-export([coord_sys/1]).

-record(scene, 
	{info,
	 isect,
	 lights,
	 world_bb,
	 no_fs,
	 data,
	 type, 
	 fsmap,
	 mats,
	 qbvh
	}).

-record(face, {vs,				% Vertices
	       ns,				% Normals
	       uvs,				% Uvs
	       vcs,				% Vertex Colors
	       mat				% Material
	      }).

-define(VERTEX_SZ, (11*4)).
-define(TRIANGLE_SZ, (3*?VERTEX_SZ)).
-define(QUAD_SZ, (4*?VERTEX_SZ)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(St = #st{shapes=Shapes,mat=Mtab0}, Opts, R = #renderer{cl=CL0}) ->
    Lights0 = proplists:get_value(lights, Opts),
    wings_pb:start("Setup data"),
    wings_pb:update(0.01, ?__(1,"setup lights")),
    Lights  = pbr_light:init(Lights0),
    wings_pb:update(0.05, ?__(2,"setup materials")),
    Mtab    = pbr_mat:init(Mtab0),
    Scene0  = #scene{lights=Lights, mats=Mtab},
    Scene1  = ?TC(prepare_mesh(gb_trees:values(Shapes),Opts,St,Scene0)),
    GetTri  = make_get_tri_fun(Scene1),
    GetFace = make_get_face_fun(Scene1),
    #scene{no_fs=Size} = Scene1,
    io:format("No of faces ~p ~n", [Size]),
    wings_pb:update(0.8, ?__(4,"calc accelerated data structures")),
    AccelBin = {WBB,_,_,_} = ?TC(e3d_qbvh:init([{Size,GetTri}])),
    wings_pb:update(0.95, ?__(5,"compile program")),
    wings_pb:done(),
    Scene2 = Scene1#scene{info=GetFace, world_bb=WBB, qbvh=AccelBin},
    {CL, Scene} = init_accel(CL0, AccelBin, Scene2),
    R#renderer{cl=CL, scene=Scene}.

make_get_tri_fun(#scene{data=Data, type=tris}) -> 
    fun(Face) ->
	    Skip = Face * ?TRIANGLE_SZ,
	    <<_:Skip/binary, 
	      V1x:?F32, V1y:?F32, V1z:?F32, _:(?VERTEX_SZ-12)/binary,
	      V2x:?F32, V2y:?F32, V2z:?F32, _:(?VERTEX_SZ-12)/binary,
	      V3x:?F32, V3y:?F32, V3z:?F32, _/binary>> = Data,
	    {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}}
    end;
make_get_tri_fun(#scene{data=Data, type=quads}) -> 
    fun(Face0) ->
	    Face = Face0 div 2,	    
	    Skip = Face * ?QUAD_SZ,
	    case Face rem 2 of
		0 ->
		    <<_:Skip/binary, 
		      V1x:?F32, V1y:?F32, V1z:?F32, _:(?VERTEX_SZ-12)/binary,
		      V2x:?F32, V2y:?F32, V2z:?F32, _:(?VERTEX_SZ-12)/binary,
		      V3x:?F32, V3y:?F32, V3z:?F32, _/binary>> = Data,
		    {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}};
		1 ->
		    <<_:Skip/binary, 
		      _:?VERTEX_SZ/binary,
		      V2x:?F32, V2y:?F32, V2z:?F32, _:(?VERTEX_SZ-12)/binary,
		      V3x:?F32, V3y:?F32, V3z:?F32, _:(?VERTEX_SZ-12)/binary,
		      V4x:?F32, V4y:?F32, V4z:?F32,_/binary>> = Data,
		    { {V2x,V2y,V2z}, {V4x,V4y,V4z}, {V3x,V3y,V3z} }
	    end
    end.
       
make_get_face_fun(#scene{data=Data, type=tris, fsmap=F2M}) ->
    fun(Face) -> 
	    Skip = Face * ?TRIANGLE_SZ,
	    <<_:Skip/binary, 
	      V1x:?F32, V1y:?F32, V1z:?F32,
	      N1x:?F32, N1y:?F32, N1z:?F32,
	      V1r:?F32, V1g:?F32, V1b:?F32,
	      U1:?F32, V1:?F32,
	      V2x:?F32, V2y:?F32, V2z:?F32,
	      N2x:?F32, N2y:?F32, N2z:?F32,
	      V2r:?F32, V2g:?F32, V2b:?F32,
	      U2:?F32, V2:?F32,
	      V3x:?F32, V3y:?F32, V3z:?F32, 
	      N3x:?F32, N3y:?F32, N3z:?F32,
	      V3r:?F32, V3g:?F32, V3b:?F32,
	      U3:?F32, V3:?F32,
	      _/binary>> = Data,
	    #face{vs  = [{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}],
		  ns  = {{N1x,N1y,N1z}, {N2x,N2y,N2z}, {N3x,N3y,N3z}},
		  uvs = {{U1,V1}, {U2,V2}, {U3,V3}},
		  vcs = {{V1r,V1g,V1b}, {V2r,V2g,V2b}, {V3r,V3g,V3b}},
		  mat = array:get(Face, F2M)}
    end;
make_get_face_fun(#scene{data=Data, type=quads, fsmap=F2M}) ->
    fun(Face0) -> 
	    Face = Face0 div 2,	    
	    Skip = Face * ?QUAD_SZ,
	    case Face rem 2 of
		0 ->
		    <<_:Skip/binary, 
		      V1x:?F32, V1y:?F32, V1z:?F32,
		      N1x:?F32, N1y:?F32, N1z:?F32,
		      V1r:?F32, V1g:?F32, V1b:?F32,
		      U1:?F32, V1:?F32,
		      V2x:?F32, V2y:?F32, V2z:?F32,
		      N2x:?F32, N2y:?F32, N2z:?F32,
		      V2r:?F32, V2g:?F32, V2b:?F32,
		      U2:?F32, V2:?F32,
		      V3x:?F32, V3y:?F32, V3z:?F32, 
		      N3x:?F32, N3y:?F32, N3z:?F32,
		      V3r:?F32, V3g:?F32, V3b:?F32,
		      U3:?F32, V3:?F32,
		      _/binary>> = Data,
		    #face{vs  = [{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}],
			  ns  = {{N1x,N1y,N1z}, {N2x,N2y,N2z}, {N3x,N3y,N3z}},
			  uvs = {{U1,V1}, {U2,V2}, {U3,V3}},
			  vcs = {{V1r,V1g,V1b}, {V2r,V2g,V2b}, {V3r,V3g,V3b}},
			  mat = array:get(Face, F2M)};
		1 ->
		    <<_:Skip/binary, 
		      _:?VERTEX_SZ/binary,
		      V2x:?F32, V2y:?F32, V2z:?F32,
		      N2x:?F32, N2y:?F32, N2z:?F32,
		      V2r:?F32, V2g:?F32, V2b:?F32,
		      U2:?F32, V2:?F32,
		      V3x:?F32, V3y:?F32, V3z:?F32, 
		      N3x:?F32, N3y:?F32, N3z:?F32,
		      V3r:?F32, V3g:?F32, V3b:?F32,
		      U3:?F32, V3:?F32,
		      V4x:?F32, V4y:?F32, V4z:?F32,
		      N4x:?F32, N4y:?F32, N4z:?F32,
		      V4r:?F32, V4g:?F32, V4b:?F32,
		      U4:?F32,  V4:?F32,
		      _/binary>> = Data,
		    #face{vs  = [{V2x,V2y,V2z}, {V4x,V4y,V4z}, {V3x,V3y,V3z}],
			  ns  = {{N2x,N2y,N2z}, {N4x,N4y,N4z}, {N3x,N3y,N3z}},
			  uvs = {{U2,V2}, {U4,V4}, {U3,V3}},
			  vcs = {{V2r,V2g,V2b}, {V4r,V4g,V4b}, {V3r,V3g,V3b}},
			  mat = array:get(Face, F2M)}
	    end
    end.


triangles(#renderer{scene=#scene{no_fs=Size, type=tris}}) ->
    triangles_3(<<>>, 0, Size*3);
triangles(#renderer{scene=#scene{no_fs=Size, type=quads}}) ->
    triangles_4(<<>>, 0, Size*4).

triangles_3(Bin, Id, Size) when Id < Size ->
    triangles_3(<<Bin/binary, Id:?UI32>>, Id+1, Size);
triangles_3(Bin,_,_) -> Bin.

triangles_4(Bin, Id1, Size) when Id1 < Size ->
    Id2 = Id1+1, Id3 = Id1+2, Id4 = Id1+3,
    triangles_4(<<Bin/binary,
		  Id1:?UI32, Id2:?UI32, Id3:?UI32,
		  Id2:?UI32, Id4:?UI32, Id3:?UI32>>,
		Id1+4, Size);
triangles_4(Bin,_,_) -> Bin.

vertices(#renderer{scene=#scene{data=Data}}) ->
    << <<V1x:?F32, V1y:?F32, V1z:?F32>> || 
	<<V1x:?F32, V1y:?F32, V1z:?F32,
	  _N1x:?F32, _N1y:?F32, _N1z:?F32,
	  _V1r:?F32, _V1g:?F32, _V1b:?F32,
	  _U1:?F32, _V1:?F32 >> <= Data >>.

normals(#renderer{scene=#scene{data=Data}}) ->
    << <<N1x:?F32, N1y:?F32, N1z:?F32>> || 
	<<_V1x:?F32, _V1y:?F32, _V1z:?F32,
	  N1x:?F32, N1y:?F32, N1z:?F32,
	  _V1r:?F32, _V1g:?F32, _V1b:?F32,
	  _U1:?F32, _V1:?F32 >> <= Data >>.

uvs(#renderer{scene=#scene{data=Data}}) ->
    << <<U1:?F32, V1:?F32>> || 
	<<_V1x:?F32, _V1y:?F32, _V1z:?F32,
	  _N1x:?F32, _N1y:?F32, _N1z:?F32,
	  _V1r:?F32, _V1g:?F32, _V1b:?F32,
	  U1:?F32, V1:?F32 >> <= Data >>.

vertex_colors(#renderer{scene=#scene{data=Data}}) ->
    << <<V1r:?F32, V1g:?F32, V1b:?F32>> ||
	<<_V1x:?F32, _V1y:?F32, _V1z:?F32,
	  _N1x:?F32, _N1y:?F32, _N1z:?F32,
	  V1r:?F32, V1g:?F32, V1b:?F32,
	  _U1:?F32, _V1:?F32 >> <= Data >>.

%% Returns used {<<Face2MeshId:?UI32>>, <<MeshId2MatId:?UI32>>, [Mats]}
mesh2mat(#renderer{scene=#scene{fsmap=FsMap}}) ->
    Collect = fun(_Id,Mat,{MId,Mat,Mats,Bin}) ->
		      {MId,Mat,Mats,<<Bin/binary,MId:?UI32>>};
		 (_Id,NewMat,{MId,_Mat,Mats,Bin}) ->
		      {MId+1,NewMat,[NewMat|Mats],<<Bin/binary,(MId+1):?UI32>>}
	      end,
    {_MId,_Mat,Mats0,Face2Mesh} = 
	array:foldl(Collect, {-1, undefined, [], <<>>}, FsMap),
    Mats = lists:reverse(Mats0),
    MatOrder = fun(Mat, Acc={Id, MatsT, MatsL}) ->
		       case gb_trees:is_defined(Mat,MatsT) of
			   true -> Acc;
			   false -> {Id+1, 
				     gb_trees:insert(Mat, Id, MatsT), 
				     [Mat|MatsL]}
		       end
	       end,
    {_, MatsT, OrderdMats} = lists:foldl(MatOrder, {0, gb_trees:empty(), []}, Mats),
    Mesh2Mat = << << (gb_trees:get(MatId, MatsT)):?UI32 >> || MatId <- Mats >>,
    {Face2Mesh, Mesh2Mat, Mats, lists:reverse(OrderdMats)}.

%% Returns world bounding box
bb(#renderer{scene=#scene{world_bb=WBB}}) ->
    WBB.

%% Returns all lights for the scene
get_lights(#renderer{scene=#scene{lights=Ls}}) ->
    Ls.

get_infinite_light(#renderer{scene=#scene{lights=Ls}}) ->
    pbr_light:get_infinite_light(Ls).

get_materials(#renderer{scene=#scene{mats=Ms}}) ->
    Ms.

%% Given Rayhit info return face info:
%%  {HitPoint, Material, SurfaceColor, Normal, ShadingNormal} 
%%   | {light, HitPoint, lightId} | {transparent, HitPoint}
get_face_info(#ray{o=RayO,d=RayD},T,B1,B2,FId,
	      #renderer{scene=#scene{info=GetFace}}) ->
    #face{vs=Vs,ns=Ns,uvs=UVs,vcs=VCs,mat=Mat} = GetFace(FId),
    Point = e3d_vec:add_prod(RayO, RayD, T),
    case pbr_mat:is_light(Mat) of
	true -> {light, Point, Mat};
	false -> 
	    B0 = 1.0 - B1 - B2,
	    SC0 = i_col(B0,B1,B2,VCs),
	    N = e3d_vec:normal(Vs),
	    ShadeN0 = i_norm(B0,B1,B2,Ns),
	    UV = i_uv(B0,B1,B2,UVs),
	    case pbr_mat:lookup_texture(Mat, UV, texture) of
		false -> 
		    ShadeN = check_normal_bumpmap(ShadeN0, Mat, UV),
		    {Point, Mat, SC0, N, ShadeN};
		TexCol = {_,_,_,A} ->
		    SC = pbr_mat:smul(SC0,TexCol),
		    case A =:= 0.0 orelse A < sfmt:uniform() of
			true ->
			    {transparent, Point};
			false ->
			    ShadeN = check_normal_bumpmap(ShadeN0, Mat, UV),
			    {Point, Mat, SC, N, ShadeN}
		    end
	    end
    end.

intersect({NoRays, RaysBin}, #renderer{scene=Scene, cl=CL}) ->
    {Qn, Qt, Rays, Hits, WorkMem} = Scene#scene.isect,
    Wait = wings_cl:write(Rays, RaysBin, CL),
    HitSz = NoRays * ?RAYHIT_SZ,
    Args = [Rays, Hits, Qn, Qt, NoRays, WorkMem],
    
    Run = wings_cl:cast('Intersect', Args, NoRays, [Wait], CL),
    Running = wings_cl:read(Hits, HitSz, [Run], CL),
    {ok, <<Result:HitSz/binary, _/binary>>} = cl:wait(Running, 1000),
    {Rays, Result}.

intersect_data(#renderer{scene=Scene}) ->    
    {Qnodes, Qpoints, _Rays, _Hits, WorkBuff} = Scene#scene.isect,
    {Qnodes, Qpoints, WorkBuff}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_mesh(Wes, Opts, St, Scene) ->
    Part = 0.7*1/length(Wes),
    {_, {S=#scene{type=Type}, AccData, MatList}} = 
	lists:foldl(fun(We, {N, {S, AccData, MatList}}) ->
			    wings_pb:update(0.1+N*Part, ?__(3,"calc object data")),
			    Res = prepare_mesh(We, Opts, St, S, AccData, MatList),
			    {N+1, Res}
		    end, {1, {Scene, [], []}}, Wes),
    Bin = list_to_binary(AccData),
    FaceSz = if Type =:= tris ->?TRIANGLE_SZ; true -> ?QUAD_SZ end,
    S#scene{no_fs=byte_size(Bin) div FaceSz, 
	    data=Bin, 
	    fsmap=array:from_list(lists:append(MatList))}.

prepare_mesh(We, Opts, St, S, AccData, MatList)
  when ?IS_VISIBLE(We#we.perm), (not ?IS_ANY_LIGHT(We)) ->
    SubDiv = proplists:get_value(subdivisions, Opts, 0),
    Options = [{smooth, true}, {subdiv, SubDiv},
	       {attribs, color_uv}, {vmirror, freeze}],
    Vab = wings_draw_setup:we(We, Options, St),
    %% I know something about this implementation :-)
    try Vab of
	#vab{face_vs={?VERTEX_SZ,Vs}, face_sn={0,Ns}, mat_map=MatMap} ->
	    %% Change Normals
	    Data = swap_normals(Vs, Ns, <<>>),
	    MM = lists:reverse(lists:keysort(3, MatMap)),
	    Type = get_face_type(MatMap, S),
	    Mats = fix_matmap(MM, Type, S#scene.mats, []), %% This should be a tree?
	    {S#scene{type=Type}, [Data|AccData], [Mats|MatList]}
    catch _:badmatch ->
	    erlang:error({?MODULE, vab_internal_format_changed})
    end;
prepare_mesh(We = #we{name=Name}, Opts, St, S, AccData, MatList)
  when ?IS_VISIBLE(We#we.perm), ?IS_AREA_LIGHT(We) ->
    SubDiv = proplists:get_value(subdivisions, Opts, 0),
    Options = [{attribs, color_uv}, {subdiv, SubDiv}, {vmirror, freeze}],
    Vab = wings_draw_setup:we(We, Options, St),
    %% I know something about this implementation :-)
    try Vab of
	#vab{face_vs={?VERTEX_SZ,Vs}, mat_map=MatMap0} ->
	    AreaLMat = Name ++ "_al",
	    Type = get_face_type(MatMap0, S),
	    GetTri = make_get_tri_fun(#scene{data=Vs, type=Type}),
	    Ntris = case Type of 
			tris ->  byte_size(Vs) div ?TRIANGLE_SZ;
			quads -> byte_size(Vs) div ?QUAD_SZ
		    end,
	    {Ls,Gain} = pbr_light:init_arealight(Name, Ntris, GetTri, S#scene.lights),
	    Mtab = pbr_mat:create_arealight_mat(AreaLMat, Gain, S#scene.mats),
	    MatMap = [{AreaLMat, ?GL_TRIANGLES, 0, byte_size(Vs) div ?VERTEX_SZ}],
	    Mats = fix_matmap(MatMap, Type, Mtab, []), 
	    {S#scene{lights=Ls, type=Type, mats=Mtab}, [Vs|AccData], [Mats|MatList]}
    catch _:badmatch ->
	    erlang:error({?MODULE, vab_internal_format_changed})
    end;
prepare_mesh(_, _Opts, _St, S, AccData, MatList) ->
    {S, AccData, MatList}.
    						       
swap_normals(<<Vs:12/binary, _:12/binary, ColUv:20/binary, NextVs/binary>>, 
	     <<Ns:12/binary, NextNs/binary>>, Acc) ->
    swap_normals(NextVs, NextNs, <<Acc/binary, Vs/binary, Ns/binary, ColUv/binary>>);
swap_normals(<<>>,<<>>, Acc) -> Acc.

get_face_type(MatMap, #scene{type=Old}) ->
    New = if  element(2, hd(MatMap)) == ?GL_TRIANGLES -> tris;
	      element(2, hd(MatMap)) == ?GL_QUADS -> quads
	  end,
    case Old of
	undefined -> New;
	New -> New;
	_ -> %% Assert Failed
	    io:format("Internal error type mismatch ~p ~p~n",[Old,New]),
	    error({type, mismatch})
    end.

fix_matmap([_MI={Name, _, _Start, Count}|Mats], tris, Mtab, Acc0) ->
    Mat = pbr_mat:lookup_id(Name, Mtab),
    Acc = append_color(Count div 3, Mat, Acc0),
    fix_matmap(Mats, tris, Mtab, Acc);
fix_matmap([_MI={Name, _, _Start, Count}|Mats], quads, Mtab, Acc0) ->
    Mat = gb_trees:get(Name, Mtab),
    Acc = append_color(Count div 4, Mat, Acc0),
    fix_matmap(Mats, quads, Mtab, Acc);
fix_matmap([], _Type, _Mtab, Acc) -> Acc.

append_color(N, Diff, Acc) when N > 0 ->
    append_color(N-1, Diff, [Diff|Acc]);
append_color(_, _, Acc) -> Acc.

init_accel(CL0, {_BB, Qnodes0, Qtris0, _Map}, Scene) ->
    Defs0 = " -D USE_LOCAL_MEM",  %% BUGBUG only use if local_mem_size /= 0
    {Defs1,Local} = case wings_cl:get_vendor(CL0) of
			"NVIDIA" ++ _ -> 
			    {[], false};
			"Advanced Micro Dev" ++ _ -> 
			    {[" -fno-alias"|Defs0],true};
			_ -> 
			    {Defs0,true}
		    end,
    {QN,QT,Defs} = 
	try  
	    exit(no_image_support), %% BUGBUG
	    wings_cl:have_image_support(CL0) orelse exit(no_image_support),
	    Dfs = [" -D USE_IMAGE_STORAGE"|Defs1],
	    Dev = wings_cl:get_device(CL0),
	    {ok, MaxH} = cl:get_device_info(Dev, image2d_max_height),
	    {ok, MaxW} = cl:get_device_info(Dev, image2d_max_width),
	    {NDim, Qnodes, TDim, Qtris} = 
		e3d_qbvh:convert_to_image2d({MaxW,MaxH}, Qnodes0, Qtris0),
	    QN0 = wings_cl:image(Qnodes, NDim, {rgba,unsigned_int32}, CL0),
	    QT0 = wings_cl:image(Qtris,  TDim, {rgba,unsigned_int32}, CL0),
	    io:format("Image support is used for qbvh_kernel~n"),
	    {QN0,QT0,Dfs}
	catch exit:no_image_support ->
		QN1 = wings_cl:buff(Qnodes0, CL0),
		QT1 = wings_cl:buff(Qtris0, CL0),
		{QN1,QT1,Defs1}
	end,

    CL1 = wings_cl:compile("utils/qbvh_kernel.cl", lists:flatten(Defs), CL0),
    Rays = wings_cl:buff(?RAYBUFFER_SZ, CL1),
    Hits = wings_cl:buff(?RAYHIT_SZ*?MAX_RAYS, CL1),
    
    %%Device  = wings_cl:get_device(CL),
    %%LocalWGS = wings_cl:get_wg_sz('Intersect', CL1),
    %% {ok,Mem} = cl:get_kernel_workgroup_info(Kernel, Device, local_mem_size),
    %% io:format("Scene: WG ~p LMem ~p~n",[Local,Mem]),
    {CL, WorkMem} = case Local of 
			true -> 
			    WGSz = 64,
			    CL2 = wings_cl:set_wg_sz('Intersect', WGSz, CL1),
			    {CL2, {local,24*WGSz*4}};
			false ->
			    WorkBuff = wings_cl:buff(24*?MAX_RAYS*4, CL1),
			    {CL1, WorkBuff}
		    end,
    {CL, Scene#scene{isect={QN, QT, Rays, Hits, WorkMem}}}.

i_col(B0,B1,B2, {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}}) 
  when is_float(B0), is_float(B1), is_float(B2),
       is_float(V1x), is_float(V1y), is_float(V1z),
       is_float(V2x), is_float(V2y), is_float(V2z),
       is_float(V3x), is_float(V3y), is_float(V3z) ->
    {B0*V1x + B1*V2x + B2*V3x,
     B0*V1y + B1*V2y + B2*V3y,
     B0*V1z + B1*V2z + B2*V3z}.

i_norm(B0,B1,B2, {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}}) 
  when is_float(B0), is_float(B1), is_float(B2),
       is_float(V1x), is_float(V1y), is_float(V1z),
       is_float(V2x), is_float(V2y), is_float(V2z),
       is_float(V3x), is_float(V3y), is_float(V3z) ->
    e3d_vec:norm({B0*V1x + B1*V2x + B2*V3x,
		  B0*V1y + B1*V2y + B2*V3y,
		  B0*V1z + B1*V2z + B2*V3z}).

i_uv(B0,B1,B2, {{V1x,V1y}, {V2x,V2y}, {V3x,V3y}}) 
  when is_float(B0), is_float(B1), is_float(B2),
       is_float(V1x), is_float(V1y),
       is_float(V2x), is_float(V2y),
       is_float(V3x), is_float(V3y) ->
    {B0*V1x + B1*V2x + B2*V3x,
     B0*V1y + B1*V2y + B2*V3y}.

check_normal_bumpmap(Normal = {NX,NY,NZ}, Mat, UV) ->
    case pbr_mat:lookup_texture(Mat, UV, normal) of
	false -> 
	    case pbr_mat:lookup_texture(Mat, UV, bump) of
		false -> Normal;
		_Color ->
		    %% const UV &dudv = map->GetDuDv();
			
		    %% UV uvdu(triUV.u + dudv.u, triUV.v);
		    %% float bu = map->GetColor(uvdu).Filter();

		    %% UV uvdv(triUV.u, triUV.v + dudv.v);
		    %% float bv = map->GetColor(uvdv).Filter();
		    
		    %% float scale = bm->GetScale();
		    %% Vector bump(scale * (bu - b0), scale * (bv - b0), 1.f);
		    
		    %%{{V1X,V1Y,V1Z}, {V2X,V2Y,V2Z}} = coord_sys(Normal),

		    %%e3d_vec:norm({V1X * BX + V2X * BY + NX * BZ,
		    %% V1Y * BX + V2Y * BY + NY * BZ,
		    %% V1Z * BX + V2Z * BY + NZ * BZ});
		    Normal
	    end;
	{X0,Y0,Z0} ->
	    X = 2.0*(X0-0.5),
	    Y = 2.0*(Y0-0.5),
	    Z = 2.0*(Z0-0.5),
	    {{V1X,V1Y,V1Z}, {V2X,V2Y,V2Z}} = coord_sys(Normal),
	    e3d_vec:norm({V1X * X + V2X * Y + NX * Z,
			  V1Y * X + V2Y * Y + NY * Z,
			  V1Z * X + V2Z * Y + NZ * Z})
    end.

coord_sys(N = {NX,NY,NZ}) ->
    V2 = case abs(NX) > abs(NY) of
	     true ->
		 ILen = 1.0 / math:sqrt(NX*NX+NZ*NZ),
		 {-NZ*ILen, 0.0, NX * ILen};
	     false ->
		 ILen = 1.0 / math:sqrt(NY*NY+NZ*NZ),
		 {0.0, NZ*ILen, -NY * ILen}
	 end,
    {V2, e3d_vec:cross(N, V2)}.


%% test(QBVH, GetMat, Cam) ->
%%     Rays = [{X,pbr_camera:generate_ray(Cam, float(X), 127.0)} || 
%% 	       X <- [260,261]], %%lists:seq(260, 264, 2)],
%%     Test = fun({Pos, {hit,Dist,B1,B2,F}}) ->
%% 		   io:format("~n~n**** HIT ~p: ~p ~w *****~n",[Pos, F, GetMat(F)])
%% 	   end,
%%     [Test({X,e3d_qbvh:ray_trace(R, QBVH)}) || {X,R} <- Rays].
    
    


   
    
    
