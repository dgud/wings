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
-export([setup/1,quick_preview/1,update/2,draw/2,draw_smooth_edges/1,
	 smooth/2, smooth_dl/1, smooth_faces_all/2, invalidate_dl/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2, foldl/3, reverse/1, any/2]).

-record(sp,
	{src_we=#we{},				%Previous source we.
	 we=none,				%Previous smoothed we.
	 %% Display Lists
	 faces = none,
	 smooth = none,
	 proxy_edges = none,
	 %% Vertex Array Data
	 face_vs,
	 face_fn,
	 face_sn,
	 face_uv,
	 face_vc,
	 mat_map,
	 face_ns  %% List ordered in face appearance order in bins above
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

invalidate_dl(none, _) -> none;
invalidate_dl(Pd=#sp{}, all) ->
    Pd#sp{faces=none, smooth=none};
invalidate_dl(Pd=#sp{faces=none}, maybe) ->
    Pd;
invalidate_dl(Pd=#sp{faces=FL}, maybe) ->
    Pd#sp{faces=[FL]};
invalidate_dl(Pd=#sp{}, edges) ->
    Pd#sp{proxy_edges=none};
invalidate_dl(Pd=#sp{}, material) ->
    Pd#sp{faces=none, smooth=none}.

smooth_dl(#sp{smooth=none, faces=FL}) -> {FL, false};
smooth_dl(#sp{smooth=Smooth}) -> Smooth.

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
%% Proxy data is not up to date recalc!
update(#dlo{proxy_data=#sp{faces=[_]}=Pd0}=D, St) ->
    update(D#dlo{proxy_data=Pd0#sp{faces=none}},St);
update(#dlo{src_we=We0,proxy_data=#sp{faces=none}=Pd0}=D, St) ->
    Pd = proxy_smooth(We0, Pd0, St),
    Faces = draw_faces(Pd, St),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_data=Pd#sp{faces=Faces,proxy_edges=ProxyEdges}};
update(#dlo{proxy_data=#sp{proxy_edges=none}=Pd}=D, _) ->
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_data=Pd#sp{proxy_edges=ProxyEdges}};
update(D, _) -> D.

update_edges(D, Pd) ->
    update_edges_1(D, Pd, wings_pref:get_value(proxy_shaded_edge_style)).

update_edges_1(_, _, cage) -> none;
update_edges_1(#dlo{src_we=#we{vp=OldVtab}}, #sp{we=#we{vp=Vtab,es=Etab}=We}, some) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    Edges = wings_edge:from_vs(wings_util:array_keys(OldVtab), We),
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
update_edges_1(_, #sp{face_vs=BinVs,face_fn=Ns,mat_map=MatMap}, all) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    wings_draw_setup:vertexPointer(BinVs),
    wings_draw_setup:normalPointer(Ns),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    Count = case MatMap of
		[{_Mat,Start,MCount}|_] ->
		    Start+MCount;
		{color,Num} ->
		    Num
	    end,
    gl:drawArrays(?GL_QUADS, 0, Count),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:endList(),
    Dl.

smooth(D=#dlo{proxy=false},_) -> D;
smooth(D=#dlo{src_we=We},_) when ?IS_ANY_LIGHT(We) -> D;
smooth(D=#dlo{proxy_data=#sp{smooth=none, face_ns=FN, we=We}=Pd0},St) ->
    PartialNs = lists:sort(FN),
    Flist = wings_we:normals(PartialNs, We),
    Ftab  = array:from_orddict(Flist),
    SN    = setup_smooth_normals(FN, Ftab, <<>>),
    smooth_faces_all(D#dlo{proxy_data=Pd0#sp{face_sn={0,SN}}},St);
smooth(D,_) ->
    D.

setup_smooth_normals([{Face,_Normal}|Fs], Ftab, SN0) ->
    [[_|{X1,Y1,Z1}],[_|{X2,Y2,Z2}],
     [_|{X3,Y3,Z3}],[_|{X4,Y4,Z4}]] = array:get(Face, Ftab),
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


draw(#dlo{proxy=false}, _Wire) -> ok;
draw(#dlo{proxy_data=#sp{faces=Dl}, drag=none}=D, Wire) ->
    draw_1(D, Dl, Wire, proxy_static_opacity, cage);
draw(#dlo{proxy_data=#sp{faces=Dl}}=D, _Wire) ->
    draw_1(D, Dl, true, proxy_moving_opacity, cage).

draw_1(D, Dl, Wire, Key, EdgeStyleKey) ->
    gl:shadeModel(?GL_SMOOTH),
    wings_render:enable_lighting(),
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
    wings_dl:call(Dl),
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

proxy_smooth(We, #sp{src_we=We,face_vs=Bin}=Pd, _St)
  when Bin =/= none ->
    %% Nothing important changed, recreate lists
    Pd;
proxy_smooth(#we{es=Etab,he=Hard,mat=M,next_id=Next,mirror=Mirror}=We0,
	     #sp{we=OldWe,src_we=#we{es=Etab,he=Hard,mat=M,next_id=Next,
				     mirror=Mirror}}, St) ->
    We = #we{fs=Ftab} = wings_subdiv:inc_smooth(We0, OldWe),
    %% This could be optimized further
    Plan = wings_draw_setup:prepare(gb_trees:to_list(Ftab), We, St),
    flat_faces(Plan, #sp{src_we=We0,we=We});

proxy_smooth(We0, _Pd, St) ->
    #we{fs=Ftab} = We = if ?IS_ANY_LIGHT(We0) -> We0;
			   true -> wings_subdiv:smooth(We0)
			end,
    Plan = wings_draw_setup:prepare(gb_trees:to_list(Ftab), We, St),
    flat_faces(Plan, #sp{src_we=We0,we=We}).

%%%
%%% Specialized drawing routines that exploits the fact that
%%% a sub-divided surface only can contain quads.
%%%

draw_faces(#sp{face_vs=BinVs,face_fn=Ns,face_vc=Col,
	       mat_map={color,NumElements}}, #st{mat=Mtab}) ->
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    wings_draw_setup:vertexPointer(BinVs),
    wings_draw_setup:normalPointer(Ns),
    wings_draw_setup:colorPointer(Col),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    gl:enableClientState(?GL_COLOR_ARRAY),
    wings_material:apply_material(default, Mtab),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:drawArrays(?GL_QUADS, 0, NumElements),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:disableClientState(?GL_COLOR_ARRAY),
    gl:endList(),
    Faces;
draw_faces(#sp{face_vs=BinVs,face_fn=Ns,face_uv=UV,mat_map=MatMap}, #st{mat=Mtab}) ->
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    wings_draw_setup:vertexPointer(BinVs),
    wings_draw_setup:normalPointer(Ns),
    wings_draw_setup:texCoordPointer(UV),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    foreach(fun(MatFs) -> draw_mat_fs(MatFs, Mtab) end, MatMap),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:endList(),
    Faces.

draw_mat_fs({Mat,Start,NumElements}, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    case wings_material:apply_material(Mat, Mtab) of
	false ->
	    gl:drawArrays(?GL_QUADS, Start, NumElements);
	true ->
	    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
	    gl:drawArrays(?GL_QUADS, Start, NumElements),
	    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY)
    end,
    gl:popAttrib().

%%%
%%% Smooth drawing.
%%%

smooth_faces_all(#dlo{proxy_data=#sp{face_vs=BinVs,face_sn=Ns,
				     face_vc=Col,mat_map={color,NumElements}}=PD}=D,
		 #st{mat=Mtab}) ->
    wings_draw_setup:vertexPointer(BinVs),
    wings_draw_setup:normalPointer(Ns),
    wings_draw_setup:colorPointer(Col),

    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    gl:enableClientState(?GL_COLOR_ARRAY),
    wings_material:apply_material(default, Mtab),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:drawArrays(?GL_QUADS, 0, NumElements),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:disableClientState(?GL_COLOR_ARRAY),
    gl:endList(),
    free(D),
    D#dlo{proxy_data=PD#sp{smooth={[Dl,none],false}}};
smooth_faces_all(#dlo{proxy_data=#sp{face_vs=BinVs,face_sn=Ns,face_uv=UV,mat_map=MatMap}=PD}=D,
		 #st{mat=Mtab}) ->
    ListOp = gl:genLists(1),
    wings_draw_setup:vertexPointer(BinVs),
    wings_draw_setup:normalPointer(Ns),
    wings_draw_setup:texCoordPointer(UV),

    DrawSolid = fun(Data={Mat,_,_}, Tr) ->
			case wings_material:is_transparent(Mat, Mtab) of
			    false ->
				draw_mat_fs(Data,Mtab),
				Tr;
			    true ->
				[Data|Tr]
			end
		end,

    gl:newList(ListOp, ?GL_COMPILE),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    Trans = foldl(DrawSolid, [], MatMap),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_NORMAL_ARRAY),
    gl:endList(),

    case Trans of
	[] ->
	    D#dlo{proxy_data=PD#sp{smooth={[ListOp,none],false}}};
	_ ->
	    ListTr = gl:genLists(1),
	    gl:newList(ListTr, ?GL_COMPILE),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:enableClientState(?GL_NORMAL_ARRAY),
	    foreach(fun(MatFs) -> draw_mat_fs(MatFs,Mtab) end, Trans),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:disableClientState(?GL_NORMAL_ARRAY),
	    gl:endList(),
	    free(D),
	    D#dlo{proxy_data=PD#sp{smooth={[ListOp,ListTr],true}}}
    end.

free(_) -> ok.

%%% Setup binaries and meta info
flat_faces({material,MatFaces,#st{mat=Mtab}}, Pd) ->
    mat_flat_faces(MatFaces, Pd, Mtab);
flat_faces({color,Ftab,We}, Pd) ->
    col_flat_faces(Ftab, We, Pd).

mat_flat_faces(MatFs, Pd, Mtab) ->
    IncludeUVs = wings_pref:get_value(show_textures) andalso
	any(fun({Mat,_}) ->
		    wings_material:has_texture(Mat, Mtab)
	    end, MatFs),
    case IncludeUVs of
	false ->
	    plain_flat_faces(MatFs, Pd, 0, <<>>, [], []);
	true ->
	    uv_flat_faces(MatFs, Pd, 0, <<>>, [], [])
    end.

plain_flat_faces([{Mat,Fs}|T], Pd=#sp{we=We}, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], Pd, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs
    end,
    S = 24,
    Pd#sp{face_vs={S,Vs},face_fn={S,Ns},face_uv=none,
	  face_ns=reverse(FaceMap),mat_map=MatInfo}.

flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    VsPos  = wings_face:vertex_positions(Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    flat_faces_1(Fs,We,Start+4,add_quad(Vs,Normal,VsPos),[{Face,Normal}|FaceMap]);
flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

uv_flat_faces([{Mat,Fs}|T], Pd = #sp{we=We}, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = uv_flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,Start0,Start-Start0}|MatInfo0],
    uv_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
uv_flat_faces([], D, _Start, Vs, FaceMap, MatInfo) ->
    case Vs of
	<<>> ->
	    Ns = UV = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,UV/bytes>> = Ns
    end,
    S = 32,
    D#sp{face_vs={S,Vs},face_fn={S,Ns},face_uv={S,UV},
	 face_ns=reverse(FaceMap),mat_map=MatInfo}.

uv_flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    {VsPos,UV} = wings_face:vpos_info_ccw(Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    uv_flat_faces_1(Fs,We,Start+4, add_quad_uv(Vs,Normal,VsPos,UV),
		    [{Face,Normal}|FaceMap]);
uv_flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

col_flat_faces(Fs, We, Pd) ->
    {Start,Vs,FaceMap} = col_flat_faces_1(Fs, We, 0, <<>>, []),
    case Vs of
	<<>> ->
	    Normals = Col = Vs;
	_ ->
	    <<_:3/unit:32,Normals/bytes>> = Vs,
	    <<_:3/unit:32,Col/bytes>> = Normals
    end,
    MatInfo = {color,Start},
    S = 36,
    Pd#sp{face_vs={S,Vs},face_fn={S,Normals},face_vc={S,Col},face_uv=none,
	  face_ns=reverse(FaceMap),mat_map=MatInfo}.

col_flat_faces_1([{Face,Edge}|T], We, Start, Vs, Fmap) ->
    {VsPos,Col} = wings_face:vpos_info_ccw(Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    col_flat_faces_1(T,We,Start+4, add_quad_col(Vs,Normal,VsPos,Col),
		     [{Face,Normal}|Fmap]);

col_flat_faces_1([], _, Start, Vs, Fmap) ->
    {Start,Vs,Fmap}.

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

def_color({_,_,_}=C) -> C;
def_color(_) -> {1.0,1.0,1.0}.
