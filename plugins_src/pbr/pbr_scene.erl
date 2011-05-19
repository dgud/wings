%%
%%  pbr_scene.erl
%%
%%     Pbr scene handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_scene).

-include_lib("wings/src/wings.hrl").
-include("pbr.hrl").

-export([init/3,
	 intersect/2, intersect_data/1,	 
	 get_lights/1,
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
    Lights  = pbr_light:create_lookup(Lights0),
    Mtab    = pbr_mat:init(Mtab0),
    {Size,Data,F2M} = ?TC(prepare_mesh(gb_trees:values(Shapes),Opts,
				       Mtab,Lights,St,[],[])),    
    io:format("No of faces ~p ~n", [Size]),
    SubDiv = 0, %% SubDiv = proplists:get_value(subdivisions, Opts, 0),
    GetTri = make_get_tri_fun(Data, SubDiv),
    GetFace = make_get_face_fun(Data, F2M, SubDiv),
    AccelBin = {WBB,_,_,_} = ?TC(e3d_qbvh:init([{Size,GetTri}])),
    
    Type = if SubDiv == 0 -> triangles; true -> quads end,
    Scene0 = #scene{info=GetFace, lights=pbr_light:init(Lights, WBB), 
		    world_bb=WBB, data=Data, no_fs=Size, fsmap=F2M,
		    qbvh=AccelBin, type=Type},
    {CL, Scene} = init_accel(CL0, AccelBin, Scene0),
    R#renderer{cl=CL, scene=Scene}.

make_get_tri_fun(Data, 0) -> 
    fun(Face) ->
	    Skip = Face * ?TRIANGLE_SZ,
	    <<_:Skip/binary, 
	      V1x:?F32, V1y:?F32, V1z:?F32, _:(?VERTEX_SZ-12)/binary,
	      V2x:?F32, V2y:?F32, V2z:?F32, _:(?VERTEX_SZ-12)/binary,
	      V3x:?F32, V3y:?F32, V3z:?F32, _/binary>> = Data,
	    {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}}
    end;
make_get_tri_fun(Data, _N) -> 
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
       
make_get_face_fun(Data, F2M, 0) ->
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
make_get_face_fun(Data, F2M, _) ->
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


triangles(#renderer{scene=#scene{no_fs=Size, type=triangles}}) ->
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
    Mesh2Mat = << <<(gb_trees:get(MatId, MatsT)):?UI32>> || MatId <- Mats >>,
    {Face2Mesh, Mesh2Mat, lists:reverse(OrderdMats)}.

%% Returns world bounding box
bb(#renderer{scene=#scene{world_bb=WBB}}) ->
    WBB.

%% Returns all lights for the scene
get_lights(#renderer{scene=#scene{lights=Ls}}) ->
    Ls.

get_infinite_light(#renderer{scene=#scene{lights=Ls}}) ->
    pbr_light:get_infinite_light(Ls).

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
	    case pbr_mat:lookup(Mat, UV, texture) of
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
prepare_mesh([We = #we{name=_Name}|Wes], Opts, Mtab, Ls, St, AccData, MatList)
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
	    Mats = fix_matmap(MM, false, true, %% SubDiv == 0, 
			      Mtab, []), %% This should be a tree?
	    prepare_mesh(Wes, Opts, Mtab, Ls, St, [Data|AccData], [Mats|MatList])
    catch _:badmatch ->
	    erlang:error({?MODULE, vab_internal_format_changed})
    end;
prepare_mesh([_|Wes], Opts, Mtab, Ls, St, AccData, MatList) ->
    prepare_mesh(Wes, Opts, Mtab, Ls, St, AccData, MatList);
prepare_mesh([], Opts, _, _, _, AccData, MatList) ->
    Bin = list_to_binary(AccData),
    FaceSz = case proplists:get_value(subdivisions, Opts, 0) of
		 0 -> ?TRIANGLE_SZ;
		 _N -> 2*?TRIANGLE_SZ
	     end,
    {byte_size(Bin) div ?TRIANGLE_SZ, %% FaceSz
     Bin, array:from_list(lists:append(MatList))}.
    						       
swap_normals(<<Vs:12/binary, _:12/binary, ColUv:20/binary, NextVs/binary>>, 
	     <<Ns:12/binary, NextNs/binary>>, Acc) ->
    swap_normals(NextVs, NextNs, <<Acc/binary, Vs/binary, Ns/binary, ColUv/binary>>);
swap_normals(<<>>,<<>>, Acc) -> Acc.

fix_matmap([_MI={Name, _, _Start, Count}|Mats], false, true, Mtab, Acc0) ->
    Mat = gb_trees:get(Name, Mtab),
    Acc = append_color(Count div 3, Mat, Acc0),
    fix_matmap(Mats, false, true, Mtab, Acc);
fix_matmap([_MI={Name, _, _Start, Count}|Mats], false, false, Mtab, Acc0) ->
    Mat = gb_trees:get(Name, Mtab),
    Acc = append_color(Count div 4, Mat, Acc0),
    fix_matmap(Mats, false, false, Mtab, Acc);
fix_matmap([_MI={_, _, _Start, Count}|Mats], LightId, true, Mtab, Acc0) ->
    Acc = append_color(Count div 3, LightId, Acc0),
    fix_matmap(Mats, LightId, true, Mtab, Acc);
fix_matmap([_MI={_, _, _Start, Count}|Mats], LightId, false, Mtab, Acc0) ->
    Acc = append_color(Count div 4, LightId, Acc0),
    fix_matmap(Mats, LightId, false, Mtab, Acc);
fix_matmap([], _LightId, _Triangles, _Mtab, Acc) -> Acc.

append_color(N, Diff, Acc) when N > 0 ->
    append_color(N-1, Diff, [Diff|Acc]);
append_color(_, _, Acc) -> Acc.

init_accel(CL0, {_BB, Qnodes0, Qtris0, _Map}, Scene) ->
    Defs0 = "-D USE_LOCAL_MEM",  %% BUGBUG only use if local_mem_size /= 0
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
	    Dfs = ["-D USE_IMAGE_STORAGE"|Defs1],
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
    LocalWGS = wings_cl:get_wg_sz('Intersect', CL1),
    %% {ok,Mem} = cl:get_kernel_workgroup_info(Kernel, Device, local_mem_size),
    %% io:format("Scene: WG ~p LMem ~p~n",[Local,Mem]),
    {CL, WorkMem} = case Local of 
			true -> 
			    CL2 = wings_cl:set_wg_sz('Intersect', 64, CL1),
			    WGSz = 64,
			    {CL2, {local,24*WGSz*4}};
			false ->
			    WorkBuff = wings_cl:buff(24*LocalWGS*4, CL1),
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
    case pbr_mat:lookup(Mat, UV, normal) of
	false -> 
	    case pbr_mat:lookup(Mat, UV, bump) of
		false -> Normal;
		Color ->
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
    
    


   
    
    
