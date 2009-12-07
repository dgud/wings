%%
%%  wings_proxy.erl --
%%
%%     This module implements the smooth proxy.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson & Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_proxy).
-export([setup/1,quick_preview/1,update/2,draw/3,draw_smooth_edges/1,
	 smooth/2, smooth_dl/1, invalidate/2,
	 split_proxy/3, update_dynamic/3, reset_dynamic/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,foldl/3,reverse/1,any/2,sort/1]).

-record(split,
	{upd_fs,			        % Update only these faces
	  dyn,					% Update tables
	  info					% proxy drag info
	 }).

-record(sp,
	{src_we=#we{},	     % Previous source we.
	 we=none,	     % Previous smoothed we.
	 split,
	 %% Display Lists
	 faces = none,
	 smooth = none,
	 proxy_edges = none,
	 
	 vab = none
	 %% Note: face_map is a list ordered in face appearance order in bins above
	 
	}).

quick_preview(_St) ->
    case any_proxy() of
	false ->
	    setup_all(true),
	    wings_wm:set_prop(workmode, false);
	true  ->
	    setup_all(false),
	    wings_wm:set_prop(workmode, true)
    end.

-spec invalidate('none'|#sp{}, 'vab'|'dl'|'edges'|'maybe') ->
    'none'|#sp{}.

invalidate(none, _) -> none;
invalidate(#sp{}=Pd, 'vab') ->
    %% Invalidate vertex buffers - implies invalidating displays lists.
    %% Used when there are changes having to do with UV coordinates and/or
    %% vertex colors (including toggling their visibility using
    %% View|Show Colors or View|Show Textures).
    Pd#sp{faces=none,smooth=none,vab=none};
invalidate(#sp{}=Pd, 'dl') ->
    %% Invalidate displays lists.
    Pd#sp{faces=none,smooth=none};
invalidate(#sp{faces=none}=Pd, maybe) ->
    Pd;
invalidate(#sp{faces=FL}=Pd, maybe) ->
    Pd#sp{faces=[FL]};
invalidate(#sp{}=Pd, edges) ->
    Pd#sp{proxy_edges=none}.

smooth_dl(#sp{smooth=Smooth}) when Smooth =/= none -> Smooth;
smooth_dl(#sp{smooth=none, faces=FL}) when FL =/= none -> {[FL,[]], false};
smooth_dl(_) -> {none, false}.

setup(#st{sel=OrigSel}=St) ->
    wings_dl:map(fun(D, Sel) -> setup_1(D, Sel) end, OrigSel),
    {save_state,wings_sel:reset(St)}.

setup_1(#dlo{src_we=#we{id=Id}=We}=D, [{Id,_}|Sel]) when ?IS_ANY_LIGHT(We) ->
    %% Never use proxies on lights.
    {D,Sel};
setup_1(#dlo{src_we=#we{id=Id},proxy=false, proxy_data=Pd}=D, [{Id,_}|Sel]) ->
    case Pd of
	none ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:add(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{proxy=true, proxy_data=#sp{}},Sel};
	_ ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:add(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{proxy=true},Sel}
    end;
setup_1(#dlo{src_we=#we{id=Id},proxy=true}=D, [{Id,_}|Sel]) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:delete_any(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    {D#dlo{proxy=false},Sel};
setup_1(D, Sel) -> {D,Sel}.

setup_all(Activate) ->
    wings_dl:map(fun(D, _) -> setup_all(D, Activate) end, []).

setup_all(#dlo{src_we=We}=D, _) when ?IS_ANY_LIGHT(We) ->
    %% Never use proxies on lights.
    D;
setup_all(#dlo{src_we=#we{id=Id},proxy_data=none}=D, true) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:add(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{proxy=true, proxy_data=#sp{}};
setup_all(#dlo{src_we=#we{id=Id}}=D, true) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:add(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{proxy=true};
setup_all(#dlo{proxy=false}=D, false) -> D;
setup_all(#dlo{src_we=#we{id=Id}}=D, false) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:delete_any(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{proxy=false};
setup_all(D, _) -> D.

update(#dlo{proxy=false}=D, _) -> D;
%% Proxy data is not up to date. Recalculate!
update(#dlo{proxy_data=#sp{faces=[_]}=Pd0}=D, St) ->
    update(D#dlo{proxy_data=Pd0#sp{faces=none}},St);
update(#dlo{src_we=We0,proxy_data=#sp{faces=none}=Pd0}=D, St) ->
    Pd = proxy_smooth(We0, Pd0, St),
    Faces = wings_draw:draw_flat_faces(Pd#sp.vab, St),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_data=Pd#sp{faces=Faces,proxy_edges=ProxyEdges}};
update(#dlo{proxy_data=#sp{proxy_edges=none}=Pd}=D, _) ->
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_data=Pd#sp{proxy_edges=ProxyEdges}};
update(#dlo{src_we=We0,proxy_data=none}=D, St) ->
    Pd = proxy_smooth(We0, #sp{}, St),
    Faces = wings_draw:draw_flat_faces(Pd#sp.vab, St),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_data=Pd#sp{faces=Faces,proxy_edges=ProxyEdges}};
update(D, _) -> D.

update_edges(D, Pd) ->
    update_edges_1(D, Pd, wings_pref:get_value(proxy_shaded_edge_style)).

update_edges_1(_, _, cage) -> none;
update_edges_1(#dlo{src_we=#we{vp=OldVtab}}, #sp{we=#we{vp=Vtab,es=Etab}=We}, some) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    Edges0 = wings_edge:from_vs(wings_util:array_keys(OldVtab), We),
    case wings_we:is_open(We) of
	true ->
	    Visible = wings_we:visible_edges(gb_sets:from_list(Edges0), We),
	    Edges   = gb_sets:to_list(Visible);
	false ->
	    Edges = Edges0
    end,
    Bin = lists:foldl(fun(E, Bin) ->
			      #edge{vs=Va,ve=Vb} = array:get(E, Etab),
			      {X1,Y1,Z1} = array:get(Va,Vtab),
			      {X2,Y2,Z2} = array:get(Vb,Vtab),
			      <<Bin/binary,X1:?F32,Y1:?F32,Z1:?F32,
			       X2:?F32,Y2:?F32,Z2:?F32>>
		      end, <<>>, Edges),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    wings_draw:drawVertices(?GL_LINES, Bin),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:endList(),
    Dl;
update_edges_1(_, #sp{vab=#vab{face_vs=BinVs,face_fn=Ns,mat_map=MatMap}}, all) ->
    wings_draw_setup:enableVertexPointer(BinVs),
    wings_draw_setup:enableNormalPointer(Ns),
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    [{_Mat,_Type,Start,MCount}|_] = MatMap,
    Count = Start+MCount,
    gl:drawArrays(?GL_QUADS, 0, Count),
    gl:endList(),
    wings_draw_setup:disableVertexPointer(BinVs),
    wings_draw_setup:disableNormalPointer(Ns),
    Dl.

smooth(D=#dlo{proxy=false},_) -> D;
smooth(D=#dlo{drag=Active},_) when Active =/= none -> D;
smooth(D=#dlo{src_we=We},_) when ?IS_ANY_LIGHT(We) -> D;
smooth(D=#dlo{proxy_data=#sp{smooth=none,
			     vab=#vab{face_map=FN}=Vab0,
			     we=We}=Pd0,
	      mirror=MM},St) ->
    PartialNs = lists:sort(FN),
    Flist = wings_we:normals(PartialNs, We, MM),
    Ftab  = array:from_orddict(Flist),
    SN    = setup_smooth_normals(FN, Ftab, <<>>),
    Vab   = Vab0#vab{face_sn={0,SN}},
    DL    = wings_draw:draw_smooth_faces(Vab, St),
    D#dlo{proxy_data=Pd0#sp{smooth=DL, vab=Vab}};
smooth(D,_) ->
    D.

setup_smooth_normals([{Face,_Normal}|Fs], Ftab, SN0) ->
    [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}] = array:get(Face, Ftab),
    SN = <<SN0/binary,
	  X1:?F32,Y1:?F32,Z1:?F32,
	  X2:?F32,Y2:?F32,Z2:?F32,
	  X3:?F32,Y3:?F32,Z3:?F32,
	  X4:?F32,Y4:?F32,Z4:?F32>>,
    setup_smooth_normals(Fs,Ftab,SN);
setup_smooth_normals([], _, SN) -> SN.

any_proxy() ->
    wings_dl:fold(fun(#dlo{proxy=false}, A) -> A;
		     (#dlo{}, _) -> true end, false).


draw(#dlo{proxy=false}, _Wire, _) -> ok;
draw(#dlo{proxy_data=#sp{faces=Dl},drag=none}=D, Wire, SceneLights) ->
    draw_1(D, Dl, Wire, proxy_static_opacity, cage, SceneLights);
draw(#dlo{proxy_data=#sp{faces=Dl}}=D, _Wire, SceneLights) ->
    draw_1(D, Dl, true, proxy_moving_opacity, cage, SceneLights).

draw_1(#dlo{proxy_data=#sp{src_we=We}} = D, Dl, Wire,
       Key, EdgeStyleKey, SceneLights) ->
    gl:shadeModel(?GL_SMOOTH),
    wings_render:enable_lighting(SceneLights),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    wings_render:polygonOffset(2),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    case wings_gl:is_ext('GL_ARB_imaging') of
	false -> ok;
	true ->
	    case wings_pref:get_value(Key) of
		1.0 -> ok;
		Opacity ->
		    gl:enable(?GL_BLEND),
		    gl:blendFunc(?GL_CONSTANT_ALPHA, ?GL_ONE_MINUS_CONSTANT_ALPHA),
		    gl:blendColor(0, 0, 0, Opacity)
	    end
    end,
    case wings_we:is_open(We) of
	true -> gl:disable(?GL_CULL_FACE);
	false -> ignore
    end,
    wings_dl:call(Dl),
    gl:enable(?GL_CULL_FACE),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    wings_render:disable_lighting(),
    gl:shadeModel(?GL_FLAT),
    draw_edges(D, Wire, EdgeStyleKey),
    gl:disable(?GL_BLEND).

draw_smooth_edges(#dlo{drag=none}=D) ->
    draw_edges(D, true, wings_pref:get_value(proxy_shaded_edge_style));
draw_smooth_edges(D) ->
    draw_edges(D, true, cage).

draw_edges(_, false, _) -> ok;
draw_edges(D, true, EdgeStyle) ->
    case wings_pref:get_value(aa_edges) of
	true ->
	    gl:enable(?GL_LINE_SMOOTH),
	    gl:enable(?GL_BLEND),
	    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
	    gl:hint(?GL_LINE_SMOOTH_HINT, ?GL_NICEST);
	false ->
	    ok
    end,
    draw_edges_1(D, EdgeStyle).

draw_edges_1(#dlo{edges=Edges}, cage) ->
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    gl:enable(?GL_POLYGON_OFFSET_LINE),
    wings_render:polygonOffset(1),
    gl:disable(?GL_CULL_FACE),
    wings_dl:call(Edges),
    gl:enable(?GL_CULL_FACE);
draw_edges_1(#dlo{proxy_data=#sp{proxy_edges=ProxyEdges}}, _) ->
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
    wings_dl:call(ProxyEdges).

proxy_smooth(We0, Pd0, St) ->
    case proxy_smooth_1(We0, Pd0) of
	{false,_} ->
	    Pd0;
	{true,#we{fs=Ftab}=We} ->
	    %% Could incremental smooth be optimized?
	    Plan = wings_draw_setup:prepare(gb_trees:to_list(Ftab), We, St),
	    flat_faces(Plan, #sp{src_we=We0,we=We})
    end.

proxy_smooth_1(We, #sp{we=SWe,src_we=We,vab=#vab{face_vs=Bin}})
  when Bin =/= none ->
    %% Nothing important changed - just recreate the display lists
    {false,SWe};
proxy_smooth_1(#we{es=Etab,he=Hard,mat=M,next_id=Next,mirror=Mirror}=We0,
	       #sp{we=OldWe,src_we=#we{es=Etab,he=Hard,mat=M,next_id=Next,
				       mirror=Mirror}}) ->
    {true,wings_subdiv:inc_smooth(We0, OldWe)};
proxy_smooth_1(We0, #sp{we=SWe}) ->
    if ?IS_ANY_LIGHT(We0) -> {false,SWe};
       true -> {true,wings_subdiv:smooth(We0)}
    end.

split_proxy(#dlo{proxy=true,proxy_data=Pd0,src_we=SrcWe}, DynVs0, St) ->
    DynFs0 = wings_face:from_vs(DynVs0, SrcWe),
    #we{mirror=Mirror,holes=Holes} = SrcWe,
    DynFs = ordsets:subtract(DynFs0, ordsets:union([Mirror], Holes)),

    DynVs = wings_vertex:from_faces(DynFs, SrcWe),
    {_,#we{fs=Ftab0}=We0} = proxy_smooth_1(SrcWe, Pd0),
    Fs0 = wings_face:from_vs(DynVs, We0),
    OutEs = wings_face:outer_edges(Fs0, We0),
    UpdateVs0 = gb_sets:from_ordset(wings_face:to_vertices(Fs0, We0)),
    OuterVs  = gb_sets:from_ordset(wings_edge:to_vertices(OutEs, We0)),
    UpdateVs = gb_sets:subtract(UpdateVs0,OuterVs),

    Ftab = sofs:from_external(gb_trees:to_list(Ftab0), [{face,data}]),
    Fs = sofs:from_external(Fs0, [face]),
    {DynFtab0,StaticFtab0} = sofs:partition(1, Ftab, Fs),
    DynFtab = sofs:to_external(DynFtab0),
    StaticFtab = sofs:to_external(StaticFtab0),
    StaticPlan = wings_draw_setup:prepare(StaticFtab, We0, St),

    #sp{vab=StaticVab} = flat_faces(StaticPlan, #sp{we=We0}),
    StaticDL = wings_draw:draw_flat_faces(StaticVab, St),
    DynPlan  = wings_draw_setup:prepare(DynFtab, We0, St),
    Info = wings_subdiv:get_proxy_info(DynVs, UpdateVs, SrcWe),
    Split = #split{upd_fs=DynFs,dyn=DynPlan,info=Info},
    Sp = #sp{we=We0,src_we=SrcWe,split=Split},
    DynD = flat_faces(DynPlan, Sp),
    Temp = wings_draw:draw_flat_faces(DynD#sp.vab, St),
    DynD#sp{faces=[StaticDL,Temp]};

split_proxy(#dlo{proxy_data=PD},_, _St) ->
    PD.

update_dynamic(ChangedVs, St, #dlo{proxy=true,proxy_data=Pd0}=D0) ->
    #sp{faces=[SDL|_],we=SmoothedWe,split=Split,
	src_we=SrcWe0=#we{vp=Vtab0}}=Pd0,
    #split{upd_fs=Upd,dyn=DynPlan,info=Info} = Split,
    Vtab = foldl(fun({V,Pos},Acc) -> array:set(V,Pos,Acc) end,
		 Vtab0, ChangedVs),
    SrcWe = SrcWe0#we{vp=Vtab},
    We   = wings_subdiv:inc_smooth(SrcWe, Upd, Info, SmoothedWe),
    Pd1  = flat_faces(DynPlan, Pd0#sp{we=We, src_we=SrcWe}),
    Temp = wings_draw:draw_flat_faces(Pd1#sp.vab, St),
    D0#dlo{proxy_data=Pd1#sp{faces=[SDL,Temp]}};
update_dynamic(_, _, D) ->
    D.

reset_dynamic(#sp{we=We, src_we=We0}) ->
    #sp{we=We,src_we=We0};
reset_dynamic(D) ->
    D.

%%% Setup binaries and meta info
flat_faces({plain,MatFaces}, Pd) ->
    plain_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({uv,MatFaces}, Pd) ->
    uv_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({color,MatFaces}, Pd) ->
    col_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({color_uv,MatFaces}, Pd) ->
    col_uv_faces(MatFaces, Pd, 0, <<>>, [], []).

plain_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], Pd, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs
    end,
    S = 24,
    Pd#sp{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_uv=none,
		   face_map=reverse(FaceMap),mat_map=MatInfo}}.

flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    VsPos  = wings_face:vertex_positions(Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    flat_faces_1(Fs,We,Start+4,add_quad(Vs,Normal,VsPos),[{Face,Normal}|FaceMap]);
flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

uv_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = uv_flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    uv_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
uv_flat_faces([], Pd, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = UV = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,UV/bytes>> = Ns
    end,
    S = 32,
    Pd#sp{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_uv={S,UV},
		  face_map=reverse(FaceMap),mat_map=MatInfo}}.

uv_flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    {VsPos,UV} = wings_va:face_pos_attr(uv, Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    uv_flat_faces_1(Fs, We, Start+4,
		    add_quad_uv(Vs, Normal, VsPos, UV),
		    [{Face,Normal}|FaceMap]);
uv_flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

col_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    col_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
col_flat_faces([], Pd, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = Col = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,Col/bytes>> = Ns
    end,
    S = 36,
    Pd#sp{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_vc={S,Col},
		   face_uv=none,face_map=reverse(FaceMap),mat_map=MatInfo}}.

col_flat_faces_1([{Face,Edge}|T], We, Start, Vs, Fmap) ->
    {VsPos,Col} = wings_va:face_pos_attr(color, Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    col_flat_faces_1(T, We, Start+4,
		     add_quad_col(Vs, Normal, VsPos, Col),
		     [{Face,Normal}|Fmap]);
col_flat_faces_1([], _, Start, Vs, Fmap) ->
    {Start,Vs,Fmap}.

col_uv_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_uv_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    col_uv_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
col_uv_faces([], Pd, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = Col = UV = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,Col/bytes>> = Ns,
	    <<_:3/unit:32,UV/bytes>> = Col
    end,
    S = 44,
    Pd#sp{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},
		   face_vc={S,Col},face_uv={S,UV},
		   face_map=reverse(FaceMap),mat_map=MatInfo}}.

col_uv_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    {VsPos,UV} = wings_va:face_pos_attr([color|uv], Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    col_uv_faces_1(Fs, We, Start+4,
		   add_quad_col_uv(Vs, Normal, VsPos, UV),
		   [{Face,Normal}|FaceMap]);
col_uv_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

add_quad(Bin, {NX,NY,NZ},
	 [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32>>.

add_quad_uv(Bin, {NX,NY,NZ},
	    [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
	    [{U1,V1},{U2,V2},{U3,V3},{U4,V4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U3:?F32,V3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U4:?F32,V4:?F32>>;
add_quad_uv(Bin, N, Pos, _) ->
    Z = {0.0,0.0},
    add_quad_uv(Bin, N, Pos, [Z,Z,Z,Z]).


add_quad_col(Bin, {NX,NY,NZ},
	     [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
	     [{R1,G1,B1},{R2,G2,B2},{R3,G3,B3},{R4,G4,B4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R4:?F32,G4:?F32,B4:?F32>>;
add_quad_col(Bin, N, Pos, Cols0) ->
    Cols = [def_color(C) || C <- Cols0],
    add_quad_col(Bin, N, Pos, Cols).

add_quad_col_uv(Bin, {NX,NY,NZ},
		[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
		[[{R1,G1,B1}|{U1,V1}],
		 [{R2,G2,B2}|{U2,V2}],
		 [{R3,G3,B3}|{U3,V3}],
		 [{R4,G4,B4}|{U4,V4}]]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     U3:?F32,V3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R4:?F32,G4:?F32,B4:?F32,
     U4:?F32,V4:?F32>>;
add_quad_col_uv(Bin, N, Pos, Attrs0) ->
    Attrs = fix_color_uv(Attrs0),
    add_quad_col_uv(Bin, N, Pos, Attrs).

fix_color_uv(Attrs) ->
    case good_uvs(Attrs) of
	false ->
	    %% Bad UVs, possibly bad vertex colors too. Fix both.
	    Zuv = {0.0,0.0},
	    [[def_color(C)|Zuv] || [C|_] <- Attrs];
	true ->
	    %% Good UVs, bad vertex colors.
	    [[def_color(C)|UV] || [C|UV] <- Attrs]
    end.

good_uvs([[_|{_,_}]|T]) -> good_uvs(T);
good_uvs([_|_]) -> false;
good_uvs([]) -> true.

def_color({_,_,_}=C) -> C;
def_color(_) -> {1.0,1.0,1.0}.
