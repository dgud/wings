%%
%%  wings_proxy.erl --
%%
%%     This module implements the smooth proxy.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_proxy).
-export([setup/1,quick_preview/1,update/2,draw/2,draw_smooth_edges/1,
	 clean/1,smooth_we/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foreach/2]).

-record(sp,
	{src_we=#we{},				%Previous source we.
	 we=none,				%Previous smoothed we.
	 plan=none
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

setup(#st{sel=OrigSel}=St) ->
    wings_dl:map(fun(D, Sel) -> setup_1(D, Sel) end, OrigSel),
    {save_state,wings_sel:reset(St)}.

setup_1(#dlo{src_we=#we{id=Id}=We}=D, [{Id,_}|Sel]) when ?IS_ANY_LIGHT(We) ->
    %% Never use proxies on lights.
    {D,Sel};
setup_1(#dlo{src_we=#we{id=Id},proxy_data=Pd}=D, [{Id,_}|Sel]) ->
    case Pd of
	none ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:add(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{smooth=none,proxy_data=#sp{}},Sel};
	_ ->
	    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
	    Wire = gb_sets:delete_any(Id, Wire0),
	    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
	    {D#dlo{edges=none,smooth=none,proxy_faces=none,proxy_data=none},Sel}
    end;
setup_1(D, Sel) -> {D,Sel}.

setup_all(Activate) ->
    wings_dl:map(fun(D, _) -> setup_all(D, Activate) end, []).

setup_all(#dlo{src_we=#we{id=Id},proxy_data=none}=D, true) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:add(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{smooth=none,proxy_data=#sp{}};
setup_all(#dlo{proxy_data=none}=D, false) -> D;
setup_all(#dlo{src_we=#we{id=Id}}=D, false) ->
    Wire0 = wings_wm:get_prop(wings_wm:this(), wireframed_objects),
    Wire = gb_sets:delete_any(Id, Wire0),
    wings_wm:set_prop(wings_wm:this(), wireframed_objects, Wire),
    D#dlo{edges=none,smooth=none,proxy_faces=none,proxy_data=none};
setup_all(D, _) -> D.

update(#dlo{proxy_data=none}=D, _) -> D;
update(#dlo{proxy_faces=none,src_we=We0,proxy_data=Pd0}=D, St) ->
    Pd1 = clean(Pd0),
    Pd = proxy_smooth(We0, Pd1, St),
    #sp{we=We,plan=Plan} = Pd,
    {Faces,Edges} = draw_faces(Plan, We),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{edges=Edges,proxy_faces=Faces,proxy_edges=ProxyEdges,proxy_data=[Faces,Pd]};
update(#dlo{proxy_edges=none,proxy_data=Pd0}=D, _) ->
    Pd = clean(Pd0),
    ProxyEdges = update_edges(D, Pd),
    D#dlo{proxy_edges=ProxyEdges};
update(D, _) -> D.

update_edges(D, #sp{we=We}) ->
    update_edges_1(D, We, wings_pref:get_value(proxy_shaded_edge_style)).

update_edges_1(_, _, cage) -> none;
update_edges_1(#dlo{src_we=#we{vp=OldVtab}}, #we{vp=Vtab,es=Etab}=We, some) ->
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
update_edges_1(_, #we{es=Etab,vp=Vtab}, all) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    Bin = array:sparse_foldl(fun(_, #edge{vs=Va,ve=Vb}, Bin) ->
				     {X1,Y1,Z1} = array:get(Va,Vtab),
				     {X2,Y2,Z2} = array:get(Vb,Vtab),
				     <<Bin/binary,X1:?F32,Y1:?F32,Z1:?F32,
				      X2:?F32,Y2:?F32,Z2:?F32>>
			     end, <<>>, Etab),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    wings_draw:drawVertices(?GL_LINES, Bin),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:endList(),
    Dl.

smooth_we(#dlo{proxy_data=none,src_we=We}) -> We;
smooth_we(#dlo{src_we=We}) when ?IS_ANY_LIGHT(We) -> We;
smooth_we(#dlo{proxy_data=Pd0,src_we=We0}) ->
    case clean(Pd0) of
	#sp{we=none} -> We0;
	#sp{we=We} -> We
    end.

any_proxy() ->
    wings_dl:fold(fun(#dlo{proxy_data=none}, A) -> A;
		     (#dlo{}, _) -> true end, false).

draw(#dlo{proxy_faces=none,proxy_data=[Dl|_]}=D, Wire) ->
    draw_1(D, Dl, Wire, proxy_moving_opacity, cage);
draw(#dlo{proxy_faces=none}, _Wire) -> ok;
draw(#dlo{proxy_faces=Dl}=D, Wire) ->
    draw_1(D, Dl, Wire, proxy_static_opacity, cage).

draw_1(D, Dl, Wire, Key, EdgeStyleKey) ->
    draw_edges(D, Wire, EdgeStyleKey),
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
    gl:disable(?GL_BLEND),
    gl:disable(?GL_POLYGON_OFFSET_FILL),
    wings_render:disable_lighting(),
    gl:shadeModel(?GL_FLAT).

draw_smooth_edges(D) ->
    draw_edges(D, true, wings_pref:get_value(proxy_shaded_edge_style)).

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
draw_edges_1(#dlo{proxy_edges=ProxyEdges}, _) ->
    gl:color3fv(wings_pref:get_value(edge_color)),
    gl:lineWidth(1),
    wings_dl:call(ProxyEdges).

clean([_,#sp{}=Pd]) -> Pd;
clean(Other) -> Other.

proxy_smooth(#we{es=Etab,he=Hard,mat=M,next_id=Next,mirror=Mirror}=We0,
	     #sp{we=OldWe,src_we=#we{es=Etab,he=Hard,mat=M,next_id=Next,
			    mirror=Mirror}}=Pd, _St) ->
    We = wings_subdiv:inc_smooth(We0, OldWe),
    Pd#sp{src_we=We0,we=We};
proxy_smooth(We0, Pd, St) ->
    #we{fs=Ftab} = We = if ?IS_ANY_LIGHT(We0) -> We0;
			   true -> wings_subdiv:smooth(We0)
			end,
    Plan = prepare(gb_trees:to_list(Ftab), We, St),
    Pd#sp{src_we=We0,we=We,plan=Plan}.

%%%
%%% Specialized drawing routines that exploits the fact that
%%% a sub-divided surface only can contain quads.
%%%

draw_faces({material,MatFaces,St}, We) ->
    Faces = gl:genLists(1),
    gl:newList(Faces, ?GL_COMPILE),
    mat_faces(MatFaces, We, St),
    gl:endList(),
    {Faces,none};
draw_faces({color,Colors,#st{mat=Mtab}}, We) ->
    BasicFaces = gl:genLists(2),
    Dl = BasicFaces+1,
    gl:newList(BasicFaces, ?GL_COMPILE),
    draw_vtx_faces(Colors, We),
    gl:endList(),

    gl:newList(Dl, ?GL_COMPILE),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:callList(BasicFaces),
    gl:disable(?GL_COLOR_MATERIAL),
    gl:endList(),

    Edges = force_flat_color(BasicFaces, wings_pref:get_value(edge_color)),
    {{call,Dl,BasicFaces},Edges}.

draw_vtx_faces({Same,Diff}, We) ->
    gl:'begin'(?GL_QUADS),
    draw_vtx_faces_1(Same, We),
    draw_vtx_faces_3(Diff, We),
    gl:'end'().

draw_vtx_faces_1([{none,Faces}|Fs], We) ->
    gl:color3f(1.0, 1.0, 1.0),
    draw_vtx_faces_2(Faces, We),
    draw_vtx_faces_1(Fs, We);
draw_vtx_faces_1([{Col,Faces}|Fs], We) ->
    gl:color3fv(Col),
    draw_vtx_faces_2(Faces, We),
    draw_vtx_faces_1(Fs, We);
draw_vtx_faces_1([], _) -> ok.

draw_vtx_faces_2([F|Fs], We) ->
    mat_face(F, We),
    draw_vtx_faces_2(Fs, We);
draw_vtx_faces_2([], _) -> ok.

draw_vtx_faces_3([[F|Cols]|Fs], We) ->
    vcol_face(F, We, Cols),
    draw_vtx_faces_3(Fs, We);
draw_vtx_faces_3([], _) -> ok.

mat_faces(MatFaces, We, #st{mat=Mtab}) ->
    mat_faces_1(MatFaces, We, Mtab).

mat_faces_1([{Mat,Faces}|T], We, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    case wings_material:apply_material(Mat, Mtab) of
	false ->
	    gl:'begin'(?GL_QUADS),
	    draw_mat_faces(Faces, We),
	    gl:'end'();
	true ->
	    gl:'begin'(?GL_QUADS),
	    draw_uv_faces(Faces, We),
	    gl:'end'()
    end,
    gl:popAttrib(),
    mat_faces_1(T, We, Mtab);
mat_faces_1([], _, _) -> ok.

draw_mat_faces([{Face,Edge}|Fs], We) ->
    mat_face(Face, Edge, We),
    draw_mat_faces(Fs, We);
draw_mat_faces([], _) -> ok.

draw_uv_faces([{Face,Edge}|Fs], We) ->
    uv_face(Face, Edge, We),
    draw_uv_faces(Fs, We);
draw_uv_faces([], _) -> ok.

mat_face(Face, #we{fs=Ftab}=We) ->
    mat_face(Face, gb_trees:get(Face, Ftab), We).

mat_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs = wings_face:vertices_cw(Face, Edge, We),
    mat_face_1(Vs, Vtab, []).

mat_face_1([V|Vs], Vtab, Acc) ->
    mat_face_1(Vs, Vtab, [array:get(V, Vtab)|Acc]);
mat_face_1([], _, VsPos) ->
    N = e3d_vec:normal(VsPos),
    gl:normal3fv(N),
    case VsPos of
	[A,B,C,D] ->
	    gl:vertex3fv(A),
	    gl:vertex3fv(B),
	    gl:vertex3fv(C),
	    gl:vertex3fv(D);
	_ ->		       %Could only be the virtual mirror face.
	    ok
    end.

uv_face(Face, Edge, #we{vp=Vtab}=We) ->
    Vs0 = wings_face:vinfo_cw(Face, Edge, We),
    uv_face_1(Vs0, Vtab, [], []).

uv_face_1([[V|Col]|Vs], Vtab, Nacc, VsAcc) ->
    Pos = array:get(V, Vtab),
    uv_face_1(Vs, Vtab, [Pos|Nacc], [[Pos|Col]|VsAcc]);
uv_face_1([], _, Nacc, Vs) ->
    N = e3d_vec:normal(Nacc),
    gl:normal3fv(N),
    uv_face_2(Vs).

uv_face_2([[Pos|Attr]|T]) ->
    case Attr of
	{_,_}=UV -> gl:texCoord2fv(UV);
	_ -> gl:texCoord2f(0.0, 0.0)
    end,
    gl:vertex3fv(Pos),
    uv_face_2(T);
uv_face_2([]) -> ok.

vcol_face(Face, We, Cols) ->
    VsPos = wings_face:vertex_positions(Face, We),
    gl:normal3fv(e3d_vec:normal(VsPos)),
    vcol_face_1(VsPos, Cols).

vcol_face_1([P|Ps], [{_,_,_}=Col|Cols]) ->
    gl:color3fv(Col),
    gl:vertex3fv(P),
    vcol_face_1(Ps, Cols);
vcol_face_1([P|Ps], [_|Cols]) ->
    gl:color3f(1.0, 1.0, 1.0),
    gl:vertex3fv(P),
    vcol_face_1(Ps, Cols);
vcol_face_1([], []) -> ok.

%% force_flat_color(OriginalDlist, Color) -> NewDlist.
%%  Wrap a previous display list (that includes gl:color*() calls)
%%  into a new display lists that forces the flat color Color
%%  on all elements.
force_flat_color(Dl, RGB) ->
    force_flat_color(Dl, RGB, fun() -> ok end).

force_flat_color(OriginalDlist, {R,G,B}, DrawExtra) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_POLYGON_BIT bor ?GL_LINE_BIT bor
		  ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    DrawExtra(),
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT1),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0,0,0,0}),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, {R,G,B,1}),
    wings_dl:call(OriginalDlist),
    gl:popAttrib(),
    gl:endList(),
    {call,Dl,OriginalDlist}.

%%%
%%% Prepare for drawing.
%%%

prepare(Ftab0, We, St) ->
    Ftab = wings_we:visible(Ftab0, We),
    prepare_1(Ftab, We, St).

prepare_1(Ftab, #we{mode=vertex}=We, St) ->
    case {wings_pref:get_value(show_colors),Ftab} of
	{false,[{_,Edge}|_]} when is_integer(Edge) ->
	    Fs0 = sofs:from_external(Ftab, [{face,edge}]),
	    Fs1 = sofs:domain(Fs0),
	    Fs = sofs:to_external(Fs1),
	    {color,{[{wings_color:white(),Fs}],[]},St};
	{false,_} ->
	    {color,{[{wings_color:white(),Ftab}],[]},St};
	{true,_} ->
	    {color,vtx_color_split(Ftab, We),St}
    end;
prepare_1(Ftab, #we{mode=material}=We, St) ->
    {material,prepare_mat(Ftab, We),St}.

prepare_mat(Ftab, We) ->
    case wings_pref:get_value(show_materials) of
	false -> [{default,Ftab}];
	true -> wings_facemat:mat_faces(Ftab, We)
    end.

vtx_color_split([{_,Edge}|_]=Ftab0, We) when is_integer(Edge) ->
    vtx_color_split_1(Ftab0, We, [], []);
vtx_color_split(Ftab, _) ->
    vtx_smooth_color_split(Ftab).

vtx_color_split_1([{Face,Edge}|Fs], We, SameAcc, DiffAcc) ->
    Cols = wings_face:vertex_info(Face, Edge, We),
    case vtx_color_split_2(Cols) of
	different -> vtx_color_split_1(Fs, We, SameAcc, [[Face|Cols]|DiffAcc]);
	Col -> vtx_color_split_1(Fs, We, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_color_split_1([], _, SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_color_split_2(Cols0) ->
    case no_colors(Cols0) of
	true ->
	    wings_color:white();
	false ->
	    case Cols0 of
		[C,C|Cols] -> vtx_color_split_3(Cols, C);
		_ -> different
	    end
    end.

vtx_color_split_3([C|Cols], C) -> vtx_color_split_3(Cols, C);
vtx_color_split_3([_|_], _) -> different;
vtx_color_split_3([], C) -> C.

no_colors([{_,_,_}|_]) -> false;
no_colors([_|Cols]) -> no_colors(Cols);
no_colors([]) -> true.

vtx_smooth_color_split(Ftab) ->
    vtx_smooth_color_split_1(Ftab, [], []).

vtx_smooth_color_split_1([{_,Vs}=Face|Fs], SameAcc, DiffAcc) ->
    case vtx_smooth_face_color(Vs) of
	different ->
	    vtx_smooth_color_split_1(Fs, SameAcc, [Face|DiffAcc]);
	Col ->
	    vtx_smooth_color_split_1(Fs, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_smooth_color_split_1([], SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_smooth_face_color(Vs) ->
    case smooth_no_colors(Vs) of
	true ->
	    wings_color:white();
	false ->
	    case Vs of
		[[Col|_],[Col|_]|T] ->
		    vtx_smooth_face_color_1(T, Col);
		_ ->
		    different
	    end
    end.

vtx_smooth_face_color_1([[Col|_]|T], Col) ->
    vtx_smooth_face_color_1(T, Col);
vtx_smooth_face_color_1([_|_], _) -> different;
vtx_smooth_face_color_1([], Col) -> Col.

smooth_no_colors([[{_,_,_}|_]|_]) -> false;
smooth_no_colors([_|Cols]) -> smooth_no_colors(Cols);
smooth_no_colors([]) -> true.
