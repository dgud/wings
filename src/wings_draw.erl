%%
%%  wings_draw.erl --
%%
%%     This module draws objects using OpenGL.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_draw).
-export([refresh_dlists/1,
	 invalidate_dlists/1,
	 update_sel_dlist/0,
	 changed_we/2, update_normals/1,
	 split/3,original_we/1,update_dynamic/2,join/1,abort_split/1]).

%% Export for plugins (and wings_proxy) that need to draw stuff
-export([draw_flat_faces/2,draw_smooth_faces/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2,reverse/1,reverse/2,member/2,
		foldl/3,sort/1,keysort/2]).

-record(split,
	{static_vs,
	 dyn_vs,
	 dyn_plan,		      %Plan for drawing dynamic faces.
	 orig_ns,
	 orig_we,
	 orig_st		      %For materials
	}).

%%%
%%% Refresh the display lists from the contents of St.
%%%
%%% Invisible objects no longer have any #dlo{} entries.
%%%

refresh_dlists(St) ->
    invalidate_dlists(St),
    build_dlists(St),
    update_sel_dlist(),
    wings_develop:gl_error_check("Refresh of display lists").

invalidate_dlists(#st{selmode=Mode,sel=Sel}=St) ->
    prepare_dlists(St),
    case wings_dl:changed_materials(St) of
	[] -> ok;
	ChangedMat -> invalidate_by_mat(ChangedMat)
    end,
    wings_dl:map(fun(D0, Data) ->
			 D = update_mirror(D0),
			 invalidate_sel(D, Data, Mode)
		 end, Sel),
    update_needed(St).

prepare_dlists(#st{shapes=Shs}) ->
    wings_dl:update(fun(D, A) ->
			    prepare_fun(D, A)
		    end, gb_trees:values(Shs)).

prepare_fun(eol, [#we{perm=Perm}|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    prepare_fun(eol, Wes);
prepare_fun(eol, [We|Wes]) ->
    D = #dlo{src_we=We,open=wings_we:is_open(We)},
    {changed_we(D, D),Wes};
prepare_fun(eol, []) ->
    eol;
prepare_fun(#dlo{src_we=#we{id=Id}},
	    [#we{id=Id,perm=Perm}|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    {deleted,Wes};
prepare_fun(#dlo{}=D, [#we{perm=Perm}|Wes]) when ?IS_NOT_VISIBLE(Perm) ->
    prepare_fun(D, Wes);
prepare_fun(#dlo{src_we=We,split=#split{}=Split}=D, [We|Wes]) ->
    {D#dlo{src_we=We,split=Split#split{orig_we=We}},Wes};

prepare_fun(#dlo{src_we=We}=D, [We|Wes]) ->
    %% No real change - take the latest We for possible speed-up
    %% of further comparisons.
    {D#dlo{src_we=We},Wes};

prepare_fun(#dlo{src_we=#we{id=Id}}=D, [#we{id=Id}=We1|Wes]) ->
    prepare_fun_1(D, We1, Wes);

prepare_fun(#dlo{}, Wes) ->
    {deleted,Wes}.

prepare_fun_1(#dlo{src_we=#we{perm=Perm0}=We0}=D, #we{perm=Perm1}=We, Wes) ->
    case only_permissions_changed(We0, We) of
	true ->
	    %% More efficient, and prevents an object from disappearing
	    %% if lockness was toggled while inside a secondary selection.
	    case {Perm0,Perm1} of
		{0,1} -> {D#dlo{src_we=We},Wes};
		{1,0} -> {D#dlo{src_we=We},Wes};
		_ -> prepare_fun_2(D, We, Wes)
	    end;
	false -> prepare_fun_2(D, We, Wes)
    end.

prepare_fun_2(#dlo{proxy=IsUsed, proxy_data=Proxy,ns=Ns}=D, We, Wes) ->
    Open = wings_we:is_open(We),
    {changed_we(D, #dlo{src_we=We,open=Open,mirror=none,
			proxy=IsUsed,
			proxy_data=wings_proxy:invalidate(Proxy, maybe),
			ns=Ns}),Wes}.

only_permissions_changed(#we{perm=P}, #we{perm=P}) -> false;
only_permissions_changed(We0, We1) -> We0#we{perm=0} =:= We1#we{perm=0}.
    
invalidate_by_mat(Changed0) ->
    Changed = ordsets:from_list(Changed0),
    wings_dl:map(fun(D, _) -> invalidate_by_mat(D, Changed) end, []).

invalidate_by_mat(#dlo{work=none,vs=none,smooth=none,proxy=false}=D, _) ->
    %% Nothing to do.
    D;
invalidate_by_mat(#dlo{src_we=We,proxy_data=Pd}=D, Changed) ->
    Used = wings_facemat:used_materials(We),
    case ordsets:is_disjoint(Used, Changed) of
	true ->
	    %% The changed material is not used on any face in this object.
	    D;
	false ->
	    %% The changed material is used by this object. We'll need to
	    %% invalidate the vertex buffers as well as the display lists,
	    %% because the vertex colors settings in the material may have
	    %% been changed.
	    D#dlo{work=none,edges=none,vs=none,smooth=none,vab=none,
		  proxy_data=wings_proxy:invalidate(Pd, vab)}
    end.

invalidate_sel(#dlo{src_we=#we{id=Id},src_sel=SrcSel}=D,
	       [{Id,Items}|Sel], Mode) ->
    case SrcSel of
	{Mode,Items} -> {D,Sel};
	_ -> {D#dlo{sel=none,normals=none,src_sel={Mode,Items}},Sel}
    end;
invalidate_sel(D, Sel, _) ->
    {D#dlo{sel=none,src_sel=none},Sel}.

changed_we(#dlo{ns={_}=Ns}, D) ->
    D#dlo{ns=Ns};
changed_we(#dlo{ns=Ns}, D) ->
    D#dlo{ns={Ns}}.

update_normals(#dlo{ns={Ns0},src_we=#we{fs=Ftab}=We}=D) ->
    Ns = update_normals_1(Ns0, gb_trees:to_list(Ftab), We),
    D#dlo{ns=Ns};
update_normals(D) -> D.

update_normals_1(none, Ftab, We) ->
    update_normals_2(Ftab, [], We);
update_normals_1(Ns, Ftab, We) ->
    update_normals_2(Ftab, array:sparse_to_orddict(Ns), We).

update_normals_2(Ftab0, Ns, We) ->
    Ftab = wings_we:visible(Ftab0, We),
    update_normals_3(Ftab, Ns, We, []).

update_normals_3([{Face,Edge}|Fs], [{Face,Data}=Pair|Ns], We, Acc) ->
    Ps = wings_face:vertex_positions(Face, Edge, We),
    case Data of
	[_|Ps] ->
	    update_normals_3(Fs, Ns, We, [Pair|Acc]);
	{_,_,Ps} ->
	    update_normals_3(Fs, Ns, We, [Pair|Acc]);
	_ ->
	    update_normals_3(Fs, Ns, We, [{Face,face_ns_data(Ps)}|Acc])
    end;
update_normals_3([{Fa,_}|_]=Fs, [{Fb,_}|Ns], We, Acc) when Fa > Fb ->
    update_normals_3(Fs, Ns, We, Acc);
update_normals_3([{Face,Edge}|Fs], Ns, We, Acc) ->
    Ps = wings_face:vertex_positions(Face, Edge, We),
    update_normals_3(Fs, Ns, We, [{Face,face_ns_data(Ps)}|Acc]);
update_normals_3([], _, _, Acc) -> array:from_orddict(reverse(Acc)).

%% face_ns_data([Position]) ->
%%    [Normal|[Position]] |                        Tri or quad
%%    {Normal,[{VtxA,VtxB,VtxC}],[Position]}       Tesselated polygon
%%  Given the positions for a face, calculate the normal,
%%  and tesselate the face if necessary.
face_ns_data([_,_,_]=Ps) ->
    [e3d_vec:normal(Ps)|Ps];
face_ns_data([A,B,C,D]=Ps) ->
    N = e3d_vec:normal(Ps),
    case wings_tesselation:is_good_triangulation(N, A, B, C, D) of
	false -> {N,[{1,2,4},{4,2,3}],Ps};
	true -> [N|Ps]
    end;
face_ns_data(Ps0) ->
    N = e3d_vec:normal(Ps0),
    {Fs0,Ps} = wings_gl:triangulate(N, Ps0),
    Fs = face_ns_data_1(Fs0, []),
    {N,Fs,Ps}.

%% "Chain" vertices if possible so that the last vertex in
%% one triangle occurs as the first in the next triangle.
face_ns_data_1([{A,S,C}|Fs], [{_,_,S}|_]=Acc) ->
    face_ns_data_1(Fs, [{S,C,A}|Acc]);
face_ns_data_1([{A,B,S}|Fs], [{_,_,S}|_]=Acc) ->
    face_ns_data_1(Fs, [{S,A,B}|Acc]);
face_ns_data_1([F|Fs], Acc) ->
    face_ns_data_1(Fs, [F|Acc]);
face_ns_data_1([], Acc) -> Acc.

update_mirror(#dlo{mirror=none,src_we=#we{mirror=none}}=D) -> D;
update_mirror(#dlo{mirror=none,src_we=#we{fs=Ftab,mirror=Face}=We}=D) ->
    case gb_trees:is_defined(Face, Ftab) of
	false ->
	    D#dlo{mirror=none};
	true ->
	    D#dlo{mirror=wings_face:mirror_matrix(Face, We)}
    end;
update_mirror(D) -> D.

%% Find out which display lists are needed based on display modes.
%% (Does not check whether they exist or not; that will be checked
%% later.)

update_needed(#st{selmode=vertex}=St) ->
    case wings_pref:get_value(vertex_size) of
	0.0 ->
	    update_needed_1([], St);
	PointSize->
	    update_needed_1([{vertex,PointSize}], St)
    end;
update_needed(St) -> update_needed_1([], St).

update_needed_1(Need, St) ->
    case wings_pref:get_value(show_normals) of
	false -> update_needed_2(Need, St);
	true -> update_needed_2([normals|Need], St)
    end.

update_needed_2(CommonNeed, St) ->
    Wins = wins_of_same_class(),
    wings_dl:map(fun(D, _) ->
			 update_needed_fun(D, CommonNeed, Wins, St)
		 end, []).

update_needed_fun(#dlo{src_we=#we{perm=Perm}=We}=D, _, _, _)
  when ?IS_LIGHT(We), ?IS_VISIBLE(Perm) ->
    D#dlo{needed=[light]};
update_needed_fun(#dlo{src_we=#we{id=Id,he=Htab,pst=Pst},proxy=Proxy}=D,
		  Need0, Wins, _) ->
    Need1 = case gb_sets:is_empty(Htab) orelse
		not wings_pref:get_value(show_edges) of
		false -> [hard_edges|Need0];
		true -> Need0
	    end,
    Need2 = wings_plugin:check_plugins(update_dlist,Pst) ++ Need1,
    Need = if
	       Proxy -> [proxy|Need2];
	       true  -> Need2
	   end,
    D#dlo{needed=more_need(Wins, Id, Need)}.

more_need([W|Ws], Id, Acc0) ->
    Wire = wings_wm:get_prop(W, wireframed_objects),
    IsWireframe = gb_sets:is_member(Id, Wire),
    Acc = case {wings_wm:get_prop(W, workmode),IsWireframe} of
	      {false,true} ->
		  [edges,smooth|Acc0];
	      {false,false} ->
		  [smooth|Acc0];
	      {true,true} ->
		  [edges|Acc0];
	      {true,false} ->
		  case wings_pref:get_value(show_edges) of
		      false -> [work|Acc0];
		      true -> [edges,work|Acc0]
		  end
	  end,
    more_need(Ws, Id, Acc);
more_need([], _, Acc) ->
    ordsets:from_list(Acc).

wins_of_same_class() ->
    case wings_wm:get_dd() of
	geom_display_lists -> wings_u:geom_windows();
	_ -> [wings_wm:this()]
    end.


%%
%% Rebuild all missing display lists, based on what is
%% actually needed in D#dlo.needed.
%%

build_dlists(St) ->
    wings_dl:map(fun(#dlo{needed=Needed}=D0, S0) ->
			 D = update_normals(D0),
			 S = update_materials(D, S0),
			 update_fun(D, Needed, S)
		 end, St).

update_fun(D0, [H|T], St) ->
    D = update_fun_2(H, D0, St),
    update_fun(D, T, St);
update_fun(D, [], _) -> D.

update_materials(D, St) ->
    We = original_we(D),
    if ?IS_AREA_LIGHT(We) ->
	    wings_light:shape_materials(We#we.light, St);
       true -> St
    end.

update_fun_2(light, D, _) ->
    wings_light:update(D);
update_fun_2(work, #dlo{work=none}=D0, St) ->
    D  = wings_draw_setup:work(D0, St),
    Dl = draw_flat_faces(D, St),
    D#dlo{work=Dl};
update_fun_2(smooth, #dlo{smooth=none,proxy=false}=D0, St) ->
    D  = wings_draw_setup:smooth(D0, St),
    {List,Tr} = draw_smooth_faces(D, St),
    D#dlo{smooth=List,transparent=Tr};
update_fun_2(smooth, #dlo{proxy=true}=D0, St) ->
    wings_proxy:smooth(D0,St);
update_fun_2({vertex,_PtSize}, #dlo{vs=none,src_we=We}=D, _) ->
    Data = vertices_f32(visible_vertices(We)),
    Unsel = vbo_draw_arrays(?GL_POINTS, Data),
    D#dlo{vs=Unsel};
update_fun_2(hard_edges, #dlo{hard=none,src_we=#we{he=Htab}=We}=D, _) ->
    Edges = gb_sets:to_list(wings_we:visible_edges(Htab, We)),
    Data = edges_f32(Edges, We),
    Hard = vbo_draw_arrays(?GL_LINES, Data),
    D#dlo{hard=Hard};
update_fun_2(edges, #dlo{edges=none,ns=Ns}=D, _) ->
    EdgeDl = make_edge_dl(array:sparse_to_list(Ns)),
    D#dlo{edges=EdgeDl};
update_fun_2(normals, D, _) ->
    make_normals_dlist(D);
update_fun_2(proxy, D, St) ->
    wings_proxy:update(D, St);

%%%% Check if plugins using the Pst need a draw list updated.
update_fun_2({plugin,{Plugin,{_,_}=Data}},#dlo{plugins=Pdl}=D,St) ->
    case lists:keytake(Plugin,1,Pdl) of
        false ->
            Plugin:update_dlist(Data,D,St);
        {_,{Plugin,{_,none}},Pdl0} ->
            Plugin:update_dlist(Data,D#dlo{plugins=Pdl0},St);
        _ -> D
    end;

update_fun_2(_, D, _) -> D.

make_edge_dl(Ns) ->
    {Tris,Quads,Polys,PsLens} = make_edge_dl_bin(Ns, <<>>, <<>>, <<>>, []),
    T = vbo_draw_arrays(?GL_TRIANGLES, Tris),
    Q = vbo_draw_arrays(?GL_QUADS, Quads),
    DP = case wings_util:min_wx({1,8}) of
             true ->
                 {_,Ss,Ls} = foldl(fun(N, {Start, Ss, Ls}) ->
                                           gl:drawArrays(?GL_POLYGON, Start, N),
                                           {N+Start, <<Ss/binary, Start:?UI32>>, <<Ls/binary, N:?UI32>>}
                                   end, {0, <<>>, <<>>}, PsLens),
                 fun(RS) ->
                         gl:multiDrawArrays(?GL_POLYGON, Ss, Ls),
                         RS
                 end;
             false ->
                 fun(RS) ->
                         foldl(fun(Length, Start) ->
                                       gl:drawArrays(?GL_POLYGON, Start, Length),
                                       Length+Start
                               end, 0, PsLens),
                         RS
                 end
         end,
    P = wings_vbo:new(DP, Polys),
    [T,Q,P].

make_edge_dl_bin([[_|[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}]]|Ns],
		 Tris0, Quads, Polys, PsLens) ->
    Tris = <<Tris0/binary,X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32,
	    X3:?F32,Y3:?F32,Z3:?F32>>,
    make_edge_dl_bin(Ns, Tris, Quads, Polys, PsLens);
make_edge_dl_bin([[_|[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}]]|Ns],
		 Tris, Quads0, Polys, PsLens) ->
    Quads = <<Quads0/binary,X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32,
	     X3:?F32,Y3:?F32,Z3:?F32,X4:?F32,Y4:?F32,Z4:?F32>>,
    make_edge_dl_bin(Ns, Tris, Quads, Polys, PsLens);
make_edge_dl_bin([{_,_,VsPos}|Ns], Tris, Quads, Polys, PsLens) ->
    Poly = vertices_f32(VsPos),
    NoVs = byte_size(Poly) div 12,
    make_edge_dl_bin(Ns,Tris,Quads,<<Polys/binary,Poly/binary>>,[NoVs|PsLens]);
make_edge_dl_bin([], Tris, Quads, Polys, PsLens) ->
    {Tris, Quads, Polys, reverse(PsLens)}.

edges_f32(Edges, #we{es=Etab,vp=Vtab}) ->
    edges_f32(Edges,Etab,Vtab,<<>>).
edges_f32([Edge|Edges],Etab,Vtab,Bin0) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    {X1,Y1,Z1} = array:get(Va, Vtab),
    {X2,Y2,Z2} = array:get(Vb, Vtab),
    Bin = <<Bin0/binary,X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32>>,
    edges_f32(Edges, Etab, Vtab, Bin);
edges_f32([],_,_,Bin) ->
    Bin.

vertices_f32([{_,{_,_,_}}|_]=List) ->
    << <<X:?F32,Y:?F32,Z:?F32>> || {_,{X,Y,Z}} <- List >>;
vertices_f32([{_,_,_}|_]=List) ->
    << <<X:?F32,Y:?F32,Z:?F32>> || {X,Y,Z} <- List >>;
vertices_f32([]) -> <<>>.

vbo_draw_arrays(Type, Data) ->
    N = byte_size(Data) div 12,
    D = fun(RS) ->
		gl:drawArrays(Type, 0, N),
                RS
	end,
    wings_vbo:new(D, Data).

visible_vertices(#we{vp=Vtab0}=We) ->
    case wings_we:is_open(We) of
	false ->
	    array:sparse_to_list(Vtab0);
	true ->
	    Vis0 = wings_we:visible_vs(We),
	    Vis  = sofs:from_external(Vis0, [vertex]),
	    Vtab = sofs:from_external(array:sparse_to_orddict(Vtab0),
				      [{vertex,position}]),
	    sofs:to_external(sofs:image(Vtab, Vis))
    end.

%%%
%%% Update the selection display list.
%%%

update_sel_dlist() ->
    wings_dl:map(fun(D, _) ->
			 update_sel(D)
		 end, []).

update_sel(#dlo{src_we=We}=D) when ?IS_LIGHT(We) -> {D,[]};
update_sel(#dlo{sel=none,src_sel={body,_}}=D) ->
    update_sel_all(D);
update_sel(#dlo{split=none,sel=none,src_sel={face,Faces},
		src_we=#we{fs=Ftab}}=D) ->
    %% If we are not dragging (no dlists splitted), we can
    %% optimize the showing of the selection.
    case gb_trees:size(Ftab) =:= gb_sets:size(Faces) of
	true -> update_sel_all(D);
	false -> update_face_sel(gb_sets:to_list(Faces), D)
    end;
update_sel(#dlo{sel=none,src_sel={face,Faces}}=D) ->
    %% We are dragging. Don't try to be tricky here.
    update_face_sel(gb_sets:to_list(Faces), D);
update_sel(#dlo{sel=none,src_sel={edge,Edges}}=D) ->
    #dlo{src_we=We} = D,
    Data = edges_f32(gb_sets:to_list(Edges), We),
    Sel = vbo_draw_arrays(?GL_LINES, Data),
    D#dlo{sel=Sel};
update_sel(#dlo{sel=none,src_sel={vertex,Vs}}=D) ->
    #dlo{src_we=#we{vp=Vtab0}} = D,
    Vtab1 = array:sparse_to_orddict(Vtab0),
    Sel = case length(Vtab1) =:= gb_sets:size(Vs) of
	      true ->
		  Vtab1;
	      false ->
		  Vtab = sofs:from_external(Vtab1, [{vertex,data}]),
		  R = sofs:from_external(gb_sets:to_list(Vs), [vertex]),
		  sofs:to_external(sofs:image(Vtab, R))
	  end,
    Data = vertices_f32(Sel),
    Points = vbo_draw_arrays(?GL_POINTS, Data),
    D#dlo{sel=Points};
update_sel(#dlo{}=D) -> D.

%% Select all faces.
update_sel_all(#dlo{vab=#vab{face_vs=Vs}=Vab}=D) when Vs =/= none ->
    Count = wings_draw_setup:face_vertex_count(D),
    F = fun(RS) ->
		wings_draw_setup:enable_pointers(Vab, []),
		gl:drawArrays(?GL_TRIANGLES, 0, Count),
		wings_draw_setup:disable_pointers(Vab, []),
                RS
	end,
    D#dlo{sel={call,F,Vab}};
update_sel_all(#dlo{src_we=#we{fs=Ftab}}=D) ->
    %% No vertex arrays to re-use. Rebuild from scratch.
    update_face_sel(gb_trees:keys(Ftab), D).

update_face_sel(Fs0, #dlo{src_we=We,vab=#vab{face_vs=Vs,face_map=Map}=Vab}=D)
  when Vs =/= none ->
    Fs = wings_we:visible(Fs0, We),
    F = case wings_util:min_wx({1,8}) of
            true ->
                Collect = fun(Face, {Ss,Es}) ->
                                  {Start,NoElements} = array:get(Face, Map),
                                  {<<Ss/binary, Start:?UI32>>, <<Es/binary, NoElements:?UI32>>}
                          end,
                {Start,NoElements} = lists:foldl(Collect, {<<>>,<<>>}, lists:reverse(Fs)),
                fun(RS) ->
                        wings_draw_setup:enable_pointers(Vab, []),
                        gl:multiDrawArrays(?GL_TRIANGLES, Start, NoElements),
                        wings_draw_setup:disable_pointers(Vab, []),
                        RS
                end;
            false ->
                SN = [array:get(Face, Map) || Face <- Fs],
                fun(RS) ->
                        wings_draw_setup:enable_pointers(Vab, []),
                        [gl:drawArrays(?GL_TRIANGLES, S, N) || {S,N} <- SN],
                        wings_draw_setup:disable_pointers(Vab, []),
                        RS
                end
        end,
    Sel = {call,F,Vab},
    D#dlo{sel=Sel};
update_face_sel(Fs0, #dlo{src_we=We}=D) ->
    Fs = wings_we:visible(Fs0, We),
    Data = update_face_sel_1(Fs, D, <<>>),
    Sel = vbo_draw_arrays(?GL_TRIANGLES, Data),
    D#dlo{sel=Sel}.

update_face_sel_1(Fs, #dlo{ns=none,src_we=We}, Bin) ->
    update_face_sel_2(Fs, We, Bin);
update_face_sel_1(Fs, #dlo{ns={_},src_we=We}, Bin) ->
    update_face_sel_2(Fs, We, Bin);
update_face_sel_1(Fs, D, Bin) ->
    update_face_sel_2(Fs, D, Bin).

update_face_sel_2([F|Fs], D, Bin0) ->
    Bin = unlit_face_bin(F, D, Bin0),
    update_face_sel_2(Fs, D, Bin);
update_face_sel_2([], _, Bin) -> Bin.

%% Draw a face without any lighting.
unlit_face_bin(Face, #dlo{ns=Ns}, Bin) ->
    case array:get(Face, Ns) of
	[_|VsPos] ->    unlit_plain_face(VsPos, Bin);
	{_,Fs,VsPos} -> unlit_plain_face(Fs, VsPos, Bin)
    end;
unlit_face_bin(Face, #we{fs=Ftab}=We, Bin) ->
    Edge = gb_trees:get(Face, Ftab),
    unlit_face_bin(Face, Edge, We, Bin).

unlit_face_bin(Face, Edge, We, Bin) ->
    Ps = wings_face:vertex_positions(Face, Edge, We),
    case face_ns_data(Ps) of
	[_|VsPos] -> unlit_plain_face(VsPos, Bin);
	{_,Fs,VsPos} -> unlit_plain_face(Fs, VsPos, Bin)
    end.

unlit_plain_face([{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}], Bin) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32>>;
unlit_plain_face([{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}], Bin) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32>>.

unlit_plain_face(Fs, VsPos, Bin) ->
    unlit_plain_face_1(Fs, list_to_tuple(VsPos), Bin).

unlit_plain_face_1([{A,B,C}|Fs], Vtab, Bin0) ->
    {X1,Y1,Z1} = element(A, Vtab),
    {X2,Y2,Z2} = element(B, Vtab),
    {X3,Y3,Z3} = element(C, Vtab),
    Bin = <<Bin0/binary,
	   X1:?F32,Y1:?F32,Z1:?F32,
	   X2:?F32,Y2:?F32,Z2:?F32,
	   X3:?F32,Y3:?F32,Z3:?F32>>,
    unlit_plain_face_1(Fs, Vtab, Bin);
unlit_plain_face_1([], _, Bin) -> Bin.

%%%
%%% Splitting of objects into two display lists.
%%%

split(#dlo{ns={Ns}}=D, Vs, St) ->
    split_1(D#dlo{ns=Ns}, Vs, St);
split(D, Vs, St) -> split_1(D, Vs, St).

split_1(#dlo{split=#split{orig_we=#we{}=We,orig_ns=Ns}}=D, Vs, St) ->
    split_2(D#dlo{src_we=We,ns=Ns}, Vs, update_materials(D, St));
split_1(D, Vs, St) ->
    split_2(D, Vs, update_materials(D, St)).

split_2(#dlo{mirror=M,src_sel=Sel,src_we=#we{fs=Ftab}=We,
	     proxy=UsesProxy,needed=Needed,open=Open}=D,
	Vs0, St) ->
    Vs = sort(Vs0),
    Faces = wings_we:visible(wings_face:from_vs(Vs, We), We),
    StaticVs = static_vs(Faces, Vs, We),

    VisFtab = wings_we:visible(gb_trees:to_list(Ftab), We),
    {Work,#dlo{ns=Ns},FtabStatic,FtabDyn} =
	split_faces(D, VisFtab, Faces, St),

    %% To support commands that create new objects with static
    %% geometry (such as Shell Extrude), we must be sure to pass
    %% on the new normals table or the static edges will not be
    %% visible.
    StaticEdgeDl = make_static_edges(FtabStatic, D#dlo{ns=Ns}),
    {DynVs,VsDlist} = split_vs_dlist(Vs, StaticVs, Sel, We),

    WeDyn = wings_facemat:gc(We#we{fs=gb_trees:from_orddict(FtabDyn)}),
    DynPlan = wings_draw_setup:prepare(FtabDyn, We, St),
    StaticVtab = insert_vtx_data(StaticVs, We#we.vp, []),

    Pd = wings_proxy:split_proxy(D, Vs, St),

    Split = #split{static_vs=StaticVtab,dyn_vs=DynVs,
		   dyn_plan=DynPlan,orig_ns=Ns,
		   orig_we=We,orig_st=St},
    #dlo{work=Work,edges=[StaticEdgeDl],mirror=M,vs=VsDlist,
	 src_sel=Sel,src_we=WeDyn,split=Split,
	 proxy=UsesProxy, proxy_data=Pd,
	 needed=Needed,open=Open}.

static_vs(Fs, Vs, We) ->
    VsSet = gb_sets:from_ordset(Vs),
    Fun = fun(V, _, _, A) ->
		  case gb_sets:is_member(V, VsSet) of
 		      false -> [V|A];
 		      true -> A
 		  end
 	  end,
    static_vs_1(Fs, Fun, We, []).

static_vs_1([F|Fs], Fun, We, Acc) ->
    static_vs_1(Fs, Fun, We, wings_face:fold(Fun, Acc, F, We));
static_vs_1([], _, _, Acc) -> ordsets:from_list(Acc).

split_faces(#dlo{needed=Need}=D0, Ftab0, Fs0, St) ->
    Ftab = sofs:from_external(Ftab0, [{face,data}]),
    Fs = sofs:from_external(Fs0, [face]),
    {DynFtab0,StaticFtab0} = sofs:partition(1, Ftab, Fs),
    DynFtab = sofs:to_external(DynFtab0),
    StaticFtab = sofs:to_external(StaticFtab0),
    case member(work, Need) orelse member(smooth, Need) of
	false ->
	    %% This is wireframe mode. We don't need any
	    %% 'work' display list for faces.
	    {none,D0,StaticFtab,DynFtab};
	true ->
	    %% Faces needed. (Either workmode or smooth mode.)
	    %%
	    %% Make sure that every static face has an entry
	    %% in the normals array. Most of the time, this is
	    %% not needed because newly created faces are
	    %% dynamic, but it can happen in rare circumstances,
	    %% for instance with the Intrude command if there are holes
	    %% or with the new Shell Extrude command.
	    D1 = split_new_normals(StaticFtab, D0),

	    StaticPlan = wings_draw_setup:prepare(StaticFtab, D1, St),
	    D = wings_draw_setup:flat_faces(StaticPlan, D1),
	    Dl = draw_flat_faces(D, St),
	    {[Dl],D1,StaticFtab,DynFtab}
    end.

split_new_normals(Ftab, #dlo{ns=Ns0,src_we=We}=D) ->
    Ns = split_new_normals(Ftab, We, Ns0),
    D#dlo{ns=Ns}.

split_new_normals([{Face,Edge}|T], We, none) ->
    %% No normals means that a new object was created in an
    %% interative command (Shell Extrude).
    Ps = wings_face:vertex_positions(Face, Edge, We),
    Ns = array:set(Face, face_ns_data(Ps), array:new()),
    split_new_normals(T, We, Ns);
split_new_normals([{Face,Edge}|T], We, Ns0) ->
    case array:get(Face, Ns0) of
	undefined ->
	    Ps = wings_face:vertex_positions(Face, Edge, We),
	    Ns = array:set(Face, face_ns_data(Ps), Ns0),
	    split_new_normals(T, We, Ns);
	_ ->
	    split_new_normals(T, We, Ns0)
    end;
split_new_normals([], _, Ns) -> Ns.

make_static_edges(_Ftab, #dlo{ns=none}) ->
    make_edge_dl([]);
make_static_edges(Ftab, #dlo{ns=Ns}) ->
    make_edge_dl([array:get(F, Ns) || {F,_} <- Ftab]).

insert_vtx_data([V|Vs], Vtab, Acc) ->
    insert_vtx_data(Vs, Vtab, [{V,array:get(V, Vtab)}|Acc]);
insert_vtx_data([], _, Acc) -> reverse(Acc).

split_vs_dlist(Vs, StaticVs, {vertex,SelVs0}, #we{vp=Vtab}=We) ->
    case wings_pref:get_value(vertex_size) of
	0.0 -> {none,none};
	_PtSize -> 
	    DynVs = sofs:from_external(lists:merge(Vs, StaticVs), [vertex]),
	    SelVs = sofs:from_external(gb_sets:to_list(SelVs0), [vertex]),
	    UnselDyn0 = case wings_pref:get_value(hide_sel_while_dragging) of
			    false -> sofs:difference(DynVs, SelVs);
			    true -> DynVs
			end,
	    UnselDyn = sofs:to_external(UnselDyn0),
	    List0 = sofs:from_external(array:sparse_to_orddict(Vtab),
				       [{vertex,info}]),
	    List1 = sofs:drestriction(List0, DynVs),
	    List2 = sofs:to_external(List1),
	    List = wings_we:visible_vs(List2, We),
	    Data = << <<X:?F32,Y:?F32,Z:?F32>> || {_,{X,Y,Z}} <- List >>,
	    Unsel = vbo_draw_arrays(?GL_POINTS, Data),
	    {UnselDyn,[Unsel]}
    end;
split_vs_dlist(_, _, _, _) -> {none,none}.
    
original_we(#dlo{split=#split{orig_we=We}}) -> We;
original_we(#dlo{src_we=We}) -> We.

%%%
%%% Updating of the dynamic part of a split dlist.
%%%

update_dynamic(#dlo{src_we=We}=D0, Vtab) when ?IS_LIGHT(We) ->
    D1 = wings_light:update_dynamic(D0, Vtab),

    %% We actually don't need the normal table (yet) for a light when
    %% dragging it, but we'll need the table when picking a light.
    %% It is easiest to always keep the normals up-to-date than attempting
    %% to build when have finished dragging (since there is also the
    %% Tweak mode, which does dragging in a slightly different way).
    D = changed_we(D0, D1),
    update_normals(D);
update_dynamic(#dlo{src_we=We0,
		    split=#split{orig_st=St,static_vs=StaticVs}}=D0,
	       Vtab0) ->
    Vtab1 = keysort(1, StaticVs++Vtab0),
    Vtab = array:from_orddict(Vtab1),
    We = We0#we{vp=Vtab},
    D1 = D0#dlo{src_we=We},
    D2 = changed_we(D0, D1),
    D3 = update_normals(D2),
    D4 = dynamic_faces(D3),
    D5 = dynamic_edges(D4),
    D6 = dynamic_influence(D5),
    D  = wings_proxy:update_dynamic(Vtab0, St, D6),
    dynamic_vs(D).

dynamic_faces(#dlo{work=[Work|_],
		   split=#split{dyn_plan=DynPlan,orig_st=St}}=D0) ->
    D = wings_draw_setup:flat_faces(DynPlan, D0),
    Dl = draw_flat_faces(D, St),
    D#dlo{work=[Work,Dl],vab=none};
dynamic_faces(#dlo{work=none}=D) -> D.

dynamic_edges(#dlo{edges=[StaticEdge|_],ns=Ns}=D) ->
    EdgeDl = make_edge_dl(array:sparse_to_list(Ns)),
    D#dlo{edges=[StaticEdge,EdgeDl]}.

dynamic_vs(#dlo{split=#split{dyn_vs=none}}=D) -> D;
dynamic_vs(#dlo{src_we=#we{vp=Vtab},vs=[Static|_],
		split=#split{dyn_vs=DynVs}}=D) ->
    Data = foldl(fun(V, Bin) ->
			 {X1,Y1,Z1} = array:get(V, Vtab),
			 <<Bin/binary, X1:?F32,Y1:?F32,Z1:?F32>>
		 end, <<>>, DynVs),
    Unsel = vbo_draw_arrays(?GL_POINTS, Data),
    D#dlo{vs=[Static,Unsel]}.

% added by Micheus
dynamic_influence(#dlo{src_we=#we{pst=Pst},split=#split{orig_st=St}}=D) ->
   % it was necessary to leave only the wings_tweak's pst information in 
   % order to avoid problems when drawing if magnet mask is enabled.
    case gb_trees:lookup(wings_tweak,Pst) of
    none -> D;
    {value, WeakData} ->
      TmpPst=gb_trees:enter(wings_tweak,WeakData, gb_trees:empty()),
      Needed = wings_plugin:check_plugins(update_dlist,TmpPst),
      % plugins=[] will force the wings_tweak:update_dlist function be called 
      update_fun(D#dlo{plugins=[]},Needed,St)
    end.

%%%
%%% Abort a split.
%%%

abort_split(#dlo{split=#split{orig_ns=Ns}}=D) ->
    %% Restoring the original normals will probably be beneficial.
    D#dlo{split=none,ns=Ns};
abort_split(D) -> D.

%%%
%%% Re-joining of display lists that have been split.
%%%

join(#dlo{src_we=#we{vp=Vtab0}=SrcWe,ns=Ns1,split=#split{orig_we=We0,orig_ns=Ns0},
	  proxy_data=PD}=D) ->
    #we{vp=OldVtab} = We0,
    Vtab = join_update(Vtab0, OldVtab),
    We = wings_va:merge([SrcWe], We0#we{vp=Vtab}),
    Ns = join_ns(We, Ns1, Ns0),
    D#dlo{vs=none,drag=none,sel=none,split=none,src_we=We,ns=Ns,
	  proxy_data=wings_proxy:reset_dynamic(PD)}.

join_ns(_, NsNew, none) ->
    NsNew;
join_ns(#we{fs=Ftab}, NsNew, NsOld) ->
    join_ns_1(array:sparse_to_orddict(NsNew),
	      array:sparse_to_orddict(NsOld), Ftab, []).

join_ns_1([{Face,_}=El|FsNew], [{Face,_}|FsOld], Ftab, Acc) ->
    %% Same face: Use new contents.
    join_ns_1(FsNew, FsOld, Ftab, [El|Acc]);
join_ns_1([{Fa,_}|_]=FsNew, [{Fb,_}=El|FsOld], Ftab, Acc) when Fa > Fb ->
    %% Face only in old list: Check in Ftab if it should be kept.
    case gb_trees:is_defined(Fb, Ftab) of
	false -> join_ns_1(FsNew, FsOld, Ftab, Acc);
	true -> join_ns_1(FsNew, FsOld, Ftab, [El|Acc])
    end;
join_ns_1([El|FsNew], FsOld, Ftab, Acc) ->
    %% Fa < Fb: New face.
    join_ns_1(FsNew, FsOld, Ftab, [El|Acc]);
join_ns_1([], Fs, _, Acc) ->
    array:from_orddict(reverse(Acc, Fs)).

join_update(New, Old) ->
    join_update(array:sparse_to_orddict(New), array:sparse_to_orddict(Old), Old).

join_update([Same|New], [Same|Old], Acc) ->
    join_update(New, Old, Acc);
join_update([{V,P0}|New], [{V,OldP}|Old], Acc) ->
    P = tricky_share(P0, OldP),
    join_update(New, Old, array:set(V, P, Acc));
join_update(New, [_|Old], Acc) ->
    join_update(New, Old, Acc);
join_update([], _, Acc) -> Acc.

%% Too obvious to comment. :-)
tricky_share({X,Y,Z}=New, {OldX,OldY,OldZ})
  when X =/= OldX, Y =/= OldY, Z =/= OldZ -> New;
tricky_share({X,Y,Z}, {X,Y,_}=Old) ->
    setelement(3, Old, Z);
tricky_share({X,Y,Z}, {X,_,Z}=Old) ->
    setelement(2, Old, Y);
tricky_share({X,Y,Z}, {_,Y,Z}=Old) ->
    setelement(1, Old, X);
tricky_share({X,Y,Z}, {X,_,_}=Old) ->
    {element(1, Old),Y,Z};
tricky_share({X,Y,Z}, {_,Y,_}=Old) ->
    {X,element(2, Old),Z};
tricky_share({X,Y,Z}, {_,_,Z}=Old) ->
    {X,Y,element(3, Old)}.

%%%
%%% Drawing routines for workmode.
%%%

draw_flat_faces(#dlo{vab=Vab}, St) ->
    draw_flat_faces(Vab, St);
draw_flat_faces(#vab{mat_map=MatMap}=Vab, #st{mat=Mtab}) ->
    Extra = [colors,face_normals,uvs,tangents],
    draw_mat_faces(Vab, Extra, MatMap, Mtab).

%%%
%%% Smooth drawing.
%%%

draw_smooth_faces(#dlo{vab=Vab},St) ->
    draw_smooth_faces(Vab,St);

draw_smooth_faces(#vab{mat_map=MatMap}=Vab, #st{mat=Mtab}) ->
    Extra = [colors,vertex_normals,uvs,tangents],

    %% Partition into transparent and solid material face groups.
    {Transparent,Solid} =
	lists:partition(fun({Mat,_,_,_}) ->
				wings_material:is_transparent(Mat, Mtab)
			end, MatMap),

    %% Create display list for solid faces.
    DrawSolid = draw_mat_faces(Vab, Extra, Solid, Mtab),

    %% Create display list for transparent faces if there are
    %% any transparent faces.
    case Transparent of
	[] ->
	    %% All faces are solid.
	    {[DrawSolid,none],false};
	_ ->
	    %% Create the display list for the transparent faces.
	    DrawTr = draw_mat_faces(Vab, Extra, Transparent, Mtab),
	    {[DrawSolid,DrawTr],true}
    end.

draw_mat_faces(Vab, Extra, MatGroups, Mtab) ->
    ActiveColor = wings_draw_setup:has_active_color(Vab),
    D = fun(RS0) ->
		wings_draw_setup:enable_pointers(Vab, Extra),
		RS = do_draw_mat_faces(MatGroups, Mtab, ActiveColor, RS0),
		wings_draw_setup:disable_pointers(Vab, Extra),
                RS
	end,
    {call,D,Vab}.

do_draw_mat_faces(MatGroups, Mtab, ActiveColor, RS0) ->
    %% Show materials.
    foldl(
      fun({Mat,Type,Start,NumElements}, RS1) ->
	      DeApply = wings_material:apply_material(Mat, Mtab, ActiveColor, RS1),
	      gl:drawArrays(Type, Start, NumElements),
              DeApply()
      end, RS0, MatGroups).

%%
%% Draw normals for the selected elements.
%%

make_normals_dlist(#dlo{normals=none,src_we=We,src_sel={Mode,Elems}}=D) ->
    Normals = make_normals_dlist_1(Mode, Elems, We),
    VectorColor = wings_pref:get_value(normal_vector_color),
    N = byte_size(Normals) div 12,
    Draw0 = fun() ->
		    gl:color3fv(VectorColor),
		    gl:drawArrays(?GL_LINES, 0, N)
	    end,
    Draw = wings_vbo:new(Draw0, Normals),
    D#dlo{normals=Draw};
make_normals_dlist(#dlo{src_sel=none}=D) -> D#dlo{normals=none};
make_normals_dlist(D) -> D.

make_normals_dlist_1(vertex, Vs, #we{vp=Vtab}=We) ->
    Length = wings_pref:get_value(normal_vector_size),
    gb_sets:fold(
      fun(V, Bin) ->
	      {X1,Y1,Z1} = Pos = array:get(V, Vtab),
	      N = wings_vertex:normal(V, We),
	      {X2,Y2,Z2} = e3d_vec:add_prod(Pos, N, Length),
	      <<Bin/binary, X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32>>
      end, <<>>, Vs);
make_normals_dlist_1(edge, Edges, #we{es=Etab,vp=Vtab}=We) ->
    Et0 = sofs:relation(array:sparse_to_orddict(Etab), [{edge,data}]),
    Es = sofs:from_external(gb_sets:to_list(Edges), [edge]),
    Et1 = sofs:restriction(Et0, Es),
    Et = sofs:to_external(Et1),
    Length = wings_pref:get_value(normal_vector_size),
    foldl(fun({_,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}, Bin) ->
		  PosA = array:get(Va, Vtab),
		  PosB = array:get(Vb, Vtab),
		  {X1,Y1,Z1} = Mid = e3d_vec:average(PosA, PosB),
		  N = e3d_vec:average(wings_face:normal(Lf, We),
				      wings_face:normal(Rf, We)),
		  {X2,Y2,Z2} = e3d_vec:add_prod(Mid, N, Length),
		  <<Bin/binary, X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32>> 
	  end, <<>>, Et);
make_normals_dlist_1(face, Faces, We) ->
    Length = wings_pref:get_value(normal_vector_size),
    foldl(fun(Face, Bin) ->
		  Vs = wings_face:vertices_cw(Face, We),
		  {X1,Y1,Z1} = C = wings_vertex:center(Vs, We),
		  N = wings_face:face_normal_cw(Vs, We),
		  {X2,Y2,Z2} = e3d_vec:add_prod(C, N, Length),
		  <<Bin/binary, X1:?F32,Y1:?F32,Z1:?F32,X2:?F32,Y2:?F32,Z2:?F32>> 
	    end, <<>>, wings_we:visible(gb_sets:to_list(Faces), We));
make_normals_dlist_1(body, _, #we{fs=Ftab}=We) ->
    make_normals_dlist_1(face, gb_sets:from_list(gb_trees:keys(Ftab)), We).
