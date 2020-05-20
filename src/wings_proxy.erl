%%
%%  wings_proxy.erl --
%%
%%     This module implements the smooth proxy.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson & Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_proxy).
-export([setup/1,quick_preview/1,update/2,draw_smooth_edges/3,
	 smooth/2, smooth_dl/1, flat_dl/1, invalidate/2,
	 split_proxy/3, update_dynamic/3, reset_dynamic/1]).
-export_type([sp/0]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,reverse/1]).

-record(split,
	{upd_fs,			        % Update only these faces
	 dyn,					% Update tables
	 info					% proxy drag info
	}).

-record(sp,
	{src_we=#we{},	     % Previous source we.
	 we=none,	     % Previous smoothed we.
	 split,
	 type = ?MODULE,
	 %% Display Lists
	 faces = none,
	 smooth = none,
	 proxy_edges = none,
	 
	 vab = none
	 %% Note: face_map is a list ordered in face appearance order in bins above
	 
	}).

-type sp() :: #sp{}.

quick_preview(_St) ->
    case any_proxy() of
	false ->
	    setup_all(true),
	    wings_wm:set_prop(workmode, false);
	true  ->
	    setup_all(false),
	    wings_wm:set_prop(workmode, true)
    end.

-spec invalidate('none'|#sp{}, 'vab'|'dl'|'edges'|'maybe'|'all') ->
    'none'|#sp{}.

invalidate(none, _) -> none;
invalidate(#sp{}=Pd, 'vab') ->
    %% Invalidate vertex buffers - implies invalidating displays lists.
    %% Used when there are changes having to do with UV coordinates and/or
    %% vertex colors (including toggling their visibility using
    %% View|Show Colors or View|Show Textures).
    Pd#sp{faces=none,smooth=none,vab=none,proxy_edges=none};
invalidate(#sp{}=Pd, 'dl') ->
    %% Invalidate displays lists.
    Pd#sp{faces=none,smooth=none};
invalidate(#sp{faces=none}=Pd, maybe) ->
    Pd;
invalidate(#sp{faces=FL}=Pd, maybe) ->
    Pd#sp{faces=[FL]};
invalidate(#sp{}=Pd, edges) ->
    Pd#sp{proxy_edges=none};
invalidate(#sp{}, all) ->
    none.

smooth_dl(#sp{smooth=Smooth}) when Smooth =/= none -> Smooth;
smooth_dl(#sp{smooth=none, faces=FL}) when FL =/= none -> {[FL,[]], false};
smooth_dl(_) -> {none, false}.

flat_dl(#sp{faces=FL}) -> FL.

setup(#st{sel=OrigSel}=St) ->
    wings_dl:map(fun(D, Sel) -> setup_1(D, Sel) end, OrigSel),
    {save_state,wings_sel:reset(St)}.

setup_1(#dlo{src_we=#we{id=Id}=We}=D, [{Id,_}|Sel]) when ?IS_ANY_LIGHT(We) ->
    %% Never use proxies on lights.
    {D#dlo{proxy=false},Sel};
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
    D#dlo{proxy=false};
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
update(#dlo{src_we=We}=D, _) when ?IS_ANY_LIGHT(We) ->
    D#dlo{proxy=false}; %% Never use proxies on lights.
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
update_edges_1(_, #sp{vab=#vab{mat_map=MatMap}=Vab}, all) ->
    %% expects the materials to be reversed
    Count = case MatMap of
                [{_Mat,_Type,0,MCount}] -> MCount;
                [{_Mat,_Type,Start,MCount}|_]
                  when Start =/= 0 ->  %% Assert order of material faces
                    Start + MCount
            end,
    fun(RS0) ->
	    Extra = [face_normals],
	    RS = wings_draw_setup:enable_pointers(Vab, Extra,RS0),
	    gl:drawArrays(?GL_QUADS, 0, Count),
            wings_draw_setup:disable_pointers(Vab,RS)
    end;
update_edges_1(#dlo{}, #sp{type={wings_cc,_}, vab=#vab{face_es={0,Bin}}}, some) ->
    vbo_draw_arrays(?GL_LINES, Bin);
update_edges_1(#dlo{src_we=#we{vp=OldVtab}}, #sp{we=#we{vp=Vtab,es=Etab}=We}, some) ->
    Edges0 = wings_edge:from_vs(wings_util:array_keys(OldVtab), We),
    case wings_we:is_open(We) of
	true ->
	    Visible = wings_we:visible_edges(gb_sets:from_list(Edges0), We),
	    Edges   = gb_sets:to_list(Visible);
	false ->
	    Edges = Edges0
    end,
    Bin = foldl(fun(E, Bin) ->
                        #edge{vs=Va,ve=Vb} = array:get(E, Etab),
                        {X1,Y1,Z1} = array:get(Va,Vtab),
                        {X2,Y2,Z2} = array:get(Vb,Vtab),
                        <<Bin/binary,X1:?F32,Y1:?F32,Z1:?F32,
                          X2:?F32,Y2:?F32,Z2:?F32>>
                end, <<>>, Edges),
    vbo_draw_arrays(?GL_LINES, Bin).

vbo_draw_arrays(Type, Data) ->
    N = byte_size(Data) div 12,
    D = fun(RS) ->
		gl:drawArrays(Type, 0, N),
                RS
	end,
    wings_vbo:new(D, Data).

smooth(D=#dlo{proxy=false},_) -> D;
smooth(D=#dlo{drag=Active},_) when Active =/= none -> D;
smooth(D=#dlo{src_we=We},_) when ?IS_ANY_LIGHT(We) -> D;
smooth(D=#dlo{proxy_data=#sp{smooth=none, 
			     vab=#vab{face_map=FN}=Vab0,
			     type=Type,
			     we=We}=Pd0,
	      mirror=MM},St) ->
    Vab = case Type of 
	      ?MODULE ->
		  PartialNs = lists:sort(FN),
		  Flist = wings_we:normals(PartialNs, We, MM),
		  Ftab  = array:from_orddict(Flist),
		  SN    = setup_smooth_normals(FN, Ftab, <<>>),
		  [Vbo] = gl:genBuffers(1),
		  gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
		  gl:bufferData(?GL_ARRAY_BUFFER, byte_size(SN), SN,
				?GL_STATIC_DRAW),
		  gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
		  Vab0#vab{face_sn={vbo,Vbo}};
	      _ ->
		  Vab0
	  end,
    DL  = wings_draw:draw_smooth_faces(Vab, St),
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

draw_smooth_edges(#dlo{drag=none}=D, Style, RS) ->
    draw_edges(D, Style, RS);
draw_smooth_edges(D, _, RS) ->
    draw_edges(D, cage, RS).

draw_edges(#dlo{edges=Edges}, cage, RS) ->
    wings_dl:call(Edges, RS);
draw_edges(#dlo{proxy_data=#sp{proxy_edges=ProxyEdges}}, _, RS) ->
    wings_dl:call(ProxyEdges, RS).

proxy_smooth(We0, Pd0, St) ->
    Level = wings_pref:get_value(proxy_opencl_level),
    Impl = if is_integer(Level),Level > 0 -> wings_cc;
              true ->?MODULE
           end,
    case proxy_needs_update(We0, Pd0) of
	{false,_} ->
	    Pd0;
	{_, _} = Info when Impl =:= ?MODULE ->
	    create_proxy_subdiv(Info, We0, St);
	{Op, _} ->
	    case Pd0 of
		#sp{type={wings_cc,Data}} when Op =:= update ->
		    update_proxy_cc(We0, Data);
		_ ->
		    try 
			create_proxy_cc(We0, Level, St)
		    catch to_large -> %% Fallback if we can't allocate memory
			    create_proxy_subdiv({smooth,We0}, We0, St)
		    end
	    end
    end.

proxy_needs_update(We, #sp{we=SWe,src_we=We,vab=#vab{face_vs=Bin}})
  when Bin =/= none ->
    %% Nothing important changed - just recreate the display lists
    {false, SWe};
proxy_needs_update(#we{es=Etab,he=Hard,mat=M,next_id=Next,
		       lv=Lv,rv=Rv,mirror=Mirror},
		   #sp{we=OldWe,src_we=
			   #we{es=Etab,he=Hard,mat=M,next_id=Next,
			       lv=Lv,rv=Rv,mirror=Mirror}}) ->
    {update,OldWe};
proxy_needs_update(We0, #sp{}) ->
    if ?IS_ANY_LIGHT(We0) -> 
	    {false,We0};
       true ->
	    {smooth,We0}
    end.

update_proxy_subdiv({false, We}, _) ->
    We;
update_proxy_subdiv({update, OldWe}, We0) ->
    wings_subdiv:inc_smooth(We0, OldWe);
update_proxy_subdiv({smooth, We}, _) ->
    wings_subdiv:smooth(We).

create_proxy_subdiv(Info, We0, St) ->
    We = update_proxy_subdiv(Info, We0),
    Plan = wings_draw_setup:prepare(gb_trees:to_list(We#we.fs), We, St),
    flat_faces(Plan, #sp{src_we=We0,we=We}).

update_proxy_cc(We0, Data0) ->
    Data = wings_cc:update(We0, Data0),
    Vab  = wings_cc:gen_vab(Data),
    #sp{src_we=We0,we=We0,vab=Vab,type={wings_cc,Data}}.

create_proxy_cc(We = #we{fs=Ftab}, Level, St) ->
    Plan = wings_draw_setup:prepare(gb_trees:keys(Ftab), We, St),
    Data = wings_cc:init(Plan, Level, We),
    Vab  = wings_cc:gen_vab(Data),
    #sp{src_we=We,we=We,vab=Vab,type={wings_cc,Data}}.

split_proxy(#dlo{proxy=true, src_we=We=#we{fs=Ftab},
		 proxy_data=Pd=#sp{type={wings_cc,Data0}}},
	    DynVs0, St) ->
    Fs0 = gb_trees:keys(Ftab),
    DynFs0 = wings_face:from_vs(DynVs0, We),

    %% Expand once (to get the split drawing faces)
    DynVs1 = wings_face:to_vertices(DynFs0, We),
    DynFs = wings_face:from_vs(DynVs1, We),

    Data = case proxy_needs_update(We, Pd) of
	       {false, _} -> 
		   Data0;
	       {update, _} ->
		   wings_cc:update(DynVs0, Data0);
	       {_, _} ->
		   Plan = wings_draw_setup:prepare(Fs0, We, St),
		   wings_cc:init(Plan, Data0, We)
	   end,
    StaticFsSet = gb_sets:subtract(gb_sets:from_ordset(Fs0), 
				   gb_sets:from_ordset(DynFs)),
    StaticFs = gb_sets:to_list(StaticFsSet),
    StaticPlan = wings_draw_setup:prepare(StaticFs, We, St),
    StaticVab = wings_cc:gen_vab(StaticPlan, Data),
    StaticDL = wings_draw:draw_flat_faces(StaticVab, St),
    
    %% To get the subdiv correct we need outer layer of faces during calc
    SubdivVs = wings_face:to_vertices(DynFs, We),
    SubdivFs = wings_face:from_vs(SubdivVs, We),
    SubdivPlan = wings_draw_setup:prepare(SubdivFs, We, St),
    SubdivData = wings_cc:init(SubdivPlan, Data0, We),
    
    DynPlan  = wings_draw_setup:prepare(DynFs, We, St),
    DynVab   = wings_cc:gen_vab(DynPlan, SubdivData),
    DynDL = wings_draw:draw_flat_faces(DynVab, St),
    Split = #split{dyn=DynPlan, info=SubdivData},
    #sp{we=We,src_we=We,type={wings_cc,Data},
	faces=[StaticDL,DynDL],split=Split};

split_proxy(#dlo{proxy=true,proxy_data=Pd0,src_we=SrcWe}, DynVs0, St) ->
    DynFs0 = wings_face:from_vs(DynVs0, SrcWe),
    #we{mirror=Mirror,holes=Holes} = SrcWe,
    DynFs = ordsets:subtract(DynFs0, ordsets:union([Mirror], Holes)),

    DynVs = wings_vertex:from_faces(DynFs, SrcWe),
    #we{fs=Ftab0}=We0 = update_proxy_subdiv(proxy_needs_update(SrcWe, Pd0),SrcWe),
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

update_dynamic(ChangedVs, St, 
	       #dlo{proxy=true,proxy_data=#sp{type=?MODULE}=Pd0}=D0) ->
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
update_dynamic(ChangedVs, St, 
	       D0=#dlo{proxy=true,proxy_data=#sp{type={wings_cc,_}}=Pd0}) ->
    #sp{faces=[SDL|_],split=SP=#split{dyn=DynPlan, info=Data0}}=Pd0,
    Data = wings_cc:update(ChangedVs, Data0),
    Vab  = wings_cc:gen_vab(DynPlan, Data),
    DL   = wings_draw:draw_flat_faces(Vab, St),
    D0#dlo{proxy_data=Pd0#sp{faces=[SDL,DL], split=SP#split{info=Data}}};
update_dynamic(_, _, D) ->
    D.

reset_dynamic(#sp{we=We, src_we=We0, type=Type}) ->
    #sp{we=We,src_we=We0,type=Type};
reset_dynamic(D) ->
    D.

%%% Setup binaries and meta info
flat_faces({plain,MatFaces}, Pd) ->
    plain_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({uv,MatFaces}, Pd) ->
    uv_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({uv_tangent,MatFaces}, Pd) ->
    Z = e3d_vec:zero(),
    Array = array:new([{default, {Z,Z}}]),
    tangent_flat_faces(MatFaces, Pd, 0, <<>>, [], [], {Array, []});
flat_faces({color,MatFaces}, Pd) ->
    col_flat_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({color_uv,MatFaces}, Pd) ->
    col_uv_faces(MatFaces, Pd, 0, <<>>, [], []);
flat_faces({color_uv_tangent,MatFaces}, Pd) -> 
    Z = e3d_vec:zero(),
    Array = array:new([{default, {Z,Z}}]),
    col_tangent_faces(MatFaces, Pd, 0, <<>>, [], [], {Array, []}).

plain_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], Pd, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = reverse(FaceMap0),
    What = [vertices,face_normals],
    Vab = wings_draw_setup:create_vab(What, Vs, FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

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
uv_flat_faces([], Pd, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = reverse(FaceMap0),
    What = [vertices,face_normals,uvs],
    Vab = wings_draw_setup:create_vab(What, Vs, FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

uv_flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    {VsPos,UV} = wings_va:face_pos_attr(uv, Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    uv_flat_faces_1(Fs, We, Start+4,
		    add_quad_uv(Vs, Normal, VsPos, UV),
		    [{Face,Normal}|FaceMap]);
uv_flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

tangent_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0, Ts0) ->
    {Start,Vs,FaceMap, Ts} = tangent_flat_faces_1(Fs, We, Start0, Vs0, Fmap0, Ts0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    tangent_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo, Ts);
tangent_flat_faces([], Pd, _Start, Vs, FaceMap0, MatInfo, {VsTs0, RevF2V}) ->
    FaceMap = reverse(FaceMap0),
    VsTs = array:map(fun(_V, {T, BT}) ->
			     {e3d_vec:norm(T), e3d_vec:norm(BT)}
		     end, VsTs0),
    Data = wings_draw_setup:add_tangents(reverse(RevF2V), VsTs, Vs),
    What = [vertices,face_normals,uvs],
    Vab = wings_draw_setup:create_tangent_vab(What, Vs, Data,
					      FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

tangent_flat_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap, Ts0) ->
    {VsPos,UV} = wings_va:face_pos_attr(uv, Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    tangent_flat_faces_1(Fs, We, Start+4,
			 add_quad_uv(Vs, Normal, VsPos, UV),
			 [{Face,Normal}|FaceMap],
			 add_ts(VsPos, UV, Normal,wings_face:vertices_ccw(Face, We), Ts0));
tangent_flat_faces_1([], _, Start, Vs, FaceMap, Ts) ->
    {Start,Vs,FaceMap, Ts}.

col_flat_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_flat_faces_1(Fs, We, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    col_flat_faces(T, Pd, Start, Vs, FaceMap, MatInfo);
col_flat_faces([], Pd, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = reverse(FaceMap0),
    What = [vertices,face_normals,colors],
    Vab = wings_draw_setup:create_vab(What, Vs, FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

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
col_uv_faces([], Pd, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = reverse(FaceMap0),
    What = [vertices,face_normals,colors,uvs],
    Vab = wings_draw_setup:create_vab(What, Vs, FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

col_uv_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap) ->
    {VsPos,UV} = wings_va:face_pos_attr([color|uv], Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    col_uv_faces_1(Fs, We, Start+4,
		   add_quad_col_uv(Vs, Normal, VsPos, UV),
		   [{Face,Normal}|FaceMap]);
col_uv_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

col_tangent_faces([{Mat,Fs}|T], #sp{we=We}=Pd, Start0, Vs0, Fmap0, MatInfo0, Ts0) ->
    {Start,Vs,FaceMap, Ts} = col_tangent_faces_1(Fs, We, Start0, Vs0, Fmap0, Ts0),
    MatInfo = [{Mat,?GL_QUADS,Start0,Start-Start0}|MatInfo0],
    col_tangent_faces(T, Pd, Start, Vs, FaceMap, MatInfo, Ts);
col_tangent_faces([], Pd, _Start, Vs, FaceMap0, MatInfo, {VsTs0, RevF2V}) ->
    FaceMap = reverse(FaceMap0),
    VsTs = array:map(fun(_V, {T,BT}) ->
			     {e3d_vec:norm(T),e3d_vec:norm(BT)}
		     end, VsTs0),
    Data = wings_draw_setup:add_tangents(reverse(RevF2V), VsTs, Vs),
    What = [vertices,face_normals,colors,uvs],
    Vab = wings_draw_setup:create_tangent_vab(What, Vs, Data,
					      FaceMap, MatInfo),
    Pd#sp{vab=Vab}.

col_tangent_faces_1([{Face,Edge}|Fs], We, Start, Vs, FaceMap, Ts0) ->
    {VsPos,ColUV} = wings_va:face_pos_attr([color|uv], Face, Edge, We),
    Normal = e3d_vec:normal(VsPos),
    col_tangent_faces_1(Fs, We, Start+4,
			add_quad_col_uv(Vs, Normal, VsPos, ColUV),
			[{Face,Normal}|FaceMap],
			add_ts(VsPos, [UV || [_|UV] <- ColUV], Normal,
			       wings_face:vertices_ccw(Face, We), Ts0));
col_tangent_faces_1([], _, Start, Vs, FaceMap, Ts) ->
    {Start,Vs,FaceMap, Ts}.

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

add_ts([P1,P2,P3,_P4], [UV1,UV2,UV3,_UV4], N, Vs, Ts0) ->  
    %% Quads, subdivided so rougly the same tangents for both tris.
    wings_draw_setup:add_ts([P1,P2,P3],[UV1,UV2,UV3], N, Vs, Ts0).

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
