%%
%%  wings_we.erl --
%%
%%     This module contains functions to build and manipulate
%%     we records (winged-edged records, the central data structure
%%     in Wings 3D).
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_we.erl,v 1.110 2006/01/27 09:20:55 dgud Exp $
%%

-module(wings_we).
-export([build/2,rebuild/1,
	 new_wrap_range/3,id/2,bump_id/1,
	 new_id/1,new_ids/2,
	 invert_normals/1,
	 merge/1,merge/2,
	 renumber/2,renumber/3,
	 uv_to_color/2,
	 uv_mapped_faces/1,
	 transform_vs/2,
	 separate/1,
	 normals/2,
	 new_items_as_ordset/3,new_items_as_gbset/3,
	 is_consistent/1,is_face_consistent/2,
	 hide_faces/2,show_faces/1,num_hidden/1,
	 any_hidden/1,all_hidden/1,
	 visible/1,visible/2,visible_vs/1,visible_vs/2,
	 visible_edges/1,visible_edges/2,
	 validate_mirror/1,mirror_flatten/2]).

-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [map/2,foreach/2,foldl/3,sort/1,keysort/2,
		last/1,reverse/1,duplicate/2,seq/2,filter/2,zip/2]).

%%%
%%% API.
%%%

build(Mode, #e3d_mesh{fs=Fs0,vs=Vs,tx=Tx,he=He}) when is_atom(Mode) ->
    Fs = translate_faces(Fs0, list_to_tuple(Tx), []),
    build(Mode, Fs, Vs, He);
build(Fs, Vs) ->
    build(material, Fs, Vs, []).

%% rebuild(We) -> We'
%%  Rebuild any missing 'vc' and 'fs' tables. If there are
%%  fewer elements in the 'vc' table than in the 'vp' table,
%%  remove redundant entries in the 'vp' table. Updated id
%%  bounds.
rebuild(#we{vc=undefined,fs=undefined,es=Etab0}=We0) ->
    Etab = gb_trees:to_list(Etab0),
    Ftab = rebuild_ftab(Etab),
    VctList = rebuild_vct(Etab),
    We = We0#we{vc=gb_trees:from_orddict(VctList),fs=Ftab},
    rebuild_1(VctList, We);
rebuild(#we{vc=undefined,es=Etab}=We) ->
    VctList = rebuild_vct(gb_trees:to_list(Etab), []),
    rebuild_1(VctList, We#we{vc=gb_trees:from_orddict(VctList)});
rebuild(#we{fs=undefined,es=Etab}=We) ->
    Ftab = rebuild_ftab(gb_trees:to_list(Etab)),
    rebuild(We#we{fs=Ftab});
rebuild(We) -> update_id_bounds(We).

%%% Utilities for allocating IDs.

new_wrap_range(Items, Inc, #we{next_id=Id}=We) ->
    NumIds = Items*Inc,
    {{0,Id,Inc,NumIds},We#we{next_id=Id+NumIds}}.

id(N, {Current,BaseId,_Inc,NumIds}) ->
    BaseId + ((Current+N) rem NumIds).

bump_id({Id,BaseId,Inc,NumIds}) ->
    {Id+Inc,BaseId,Inc,NumIds}.

new_id(#we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+1}}.

new_ids(N, #we{next_id=Id}=We) ->
    {Id,We#we{next_id=Id+N}}.

%%% Returns sets of newly created items.

%% new_items_as_ordset(vertex|edge|face, OldWe, NewWe) -> NewItemsSet.
%% new_items_as_gbset(vertex|edge|face, OldWe, NewWe) -> NewItemsSet.
%%  Return all items in NewWe that are not in OldWe.

new_items_as_gbset(Type, OldWe, NewWe) ->
    gb_sets:from_ordset(new_items_as_ordset(Type, OldWe, NewWe)).

new_items_as_ordset(vertex, #we{next_id=Wid}, #we{next_id=NewWid,vp=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(edge, #we{next_id=Wid}, #we{next_id=NewWid,es=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid);
new_items_as_ordset(face, #we{next_id=Wid}, #we{next_id=NewWid,fs=Tab}) ->
    new_items_as_ordset_1(Tab, Wid, NewWid).

%%% Hiding/showing faces.

hide_faces(Fs, We) when is_list(Fs) ->
    hide_faces_1(gb_sets:from_list(Fs), We);
hide_faces(Fs, We) ->
    hide_faces_1(Fs, We).

num_hidden(#we{fs=Ftab}=We) ->
    case any_hidden(We) of
	false -> 0;
	true -> num_hidden_1(gb_trees:keys(Ftab), 0)
    end.

any_hidden(#we{fs=Ftab}) ->
    not gb_trees:is_empty(Ftab) andalso
	wings_util:gb_trees_smallest_key(Ftab) < 0.

all_hidden(#we{fs=Ftab}) ->
    not gb_trees:is_empty(Ftab) andalso
	wings_util:gb_trees_largest_key(Ftab) < 0.

%%%
%%% Local functions.
%%%

rebuild_1(VctList, #we{vc=Vct,vp=Vtab0}=We) ->
    case {gb_trees:size(Vct),gb_trees:size(Vtab0)} of
	{Same,Same} -> rebuild(We);
	{Sz1,Sz2} when Sz1 < Sz2 ->
	    Vtab = vertex_gc_1(VctList, gb_trees:to_list(Vtab0), []),
	    rebuild(We#we{vp=Vtab})
    end.

rebuild_vct(Es) ->
    rebuild_vct(Es, []).

rebuild_vct([{Edge,#edge{vs=Va,ve=Vb}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Va, Vb, Edge, Acc0),
    rebuild_vct(Es, Acc);
rebuild_vct([], VtoE) ->
    build_incident_tab(VtoE).

rebuild_ftab(Es) ->
    rebuild_ftab_1(Es, []).

rebuild_ftab_1([{Edge,#edge{lf=Lf,rf=Rf}}|Es], Acc0) ->
    Acc = rebuild_maybe_add(Lf, Rf, Edge, Acc0),
    rebuild_ftab_1(Es, Acc);
rebuild_ftab_1([], FtoE) ->
    gb_trees:from_orddict(build_incident_tab(FtoE)).

rebuild_maybe_add(Ka, Kb, E, [_,{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [_,{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Ka,_}|_]=Acc) ->
    [{Kb,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, [{Kb,_}|_]=Acc) ->
    [{Ka,E}|Acc];
rebuild_maybe_add(Ka, Kb, E, Acc) ->
    [{Ka,E},{Kb,E}|Acc].

vertex_gc_1([{V,_}|Vct], [{V,_}=Vtx|Vpos], Acc) ->
    vertex_gc_1(Vct, Vpos, [Vtx|Acc]);
vertex_gc_1([_|_]=Vct, [_|Vpos], Acc) ->
    vertex_gc_1(Vct, Vpos, Acc);
vertex_gc_1([], _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Handling of hidden faces.
%%%

hide_faces_1(Fs, #we{es=Etab0}=We0) ->
    Map = fun(_, #edge{lf=Lf0,rf=Rf0}=R0) ->
		  Lf = hide_map_face(Lf0, Fs),
		  Rf = hide_map_face(Rf0, Fs),
		  case R0#edge{lf=Lf,rf=Rf} of
		      R0 -> R0;
		      R -> R
		  end
	  end,
    Etab = wings_util:gb_trees_map(Map, Etab0),
    We = We0#we{es=Etab,fs=undefined},
    wings_facemat:hide_faces(rebuild(We)).

hide_map_face(F, Fs) ->
    case gb_sets:is_member(F, Fs) of
	false -> F;
	true -> -F-1
    end.

num_hidden_1([F|Fs], N) when F < 0 ->
    num_hidden_1(Fs, N+1);
num_hidden_1(_, N) -> N.
	    

visible(#we{mirror=none,fs=Ftab}) ->
    visible_2(gb_trees:keys(Ftab));
visible(#we{mirror=Face,fs=Ftab}) ->
    visible_2(gb_trees:keys(gb_trees:delete(Face, Ftab))).

visible([{_,_}|_]=Fs, #we{mirror=none}) ->
    visible_1(Fs);
visible([{_,_}|_]=Fs0, #we{mirror=Face}) ->
    Fs = lists:keydelete(Face, 1, Fs0),
    visible_1(Fs);
visible(Fs, #we{mirror=none}) -> visible_2(Fs);
visible(Fs, #we{mirror=Face}) -> visible_2(Fs--[Face]).

visible_1([{F,_}|Fs]) when F < 0 -> visible_1(Fs);
visible_1(Fs) -> Fs.

visible_2([F|Fs]) when F < 0 -> visible_2(Fs);
visible_2(Fs) -> Fs.

visible_vs(#we{mirror=Face,vc=Vct,es=Etab}=We) ->
    case any_hidden(We) of
	false -> gb_trees:keys(Vct);
	true -> visible_vs_1(gb_trees:values(Etab), Face, [])
    end.

visible_vs_1([#edge{lf=Mirror,rf=Rf}|Es], Mirror, Acc) when Rf < 0 ->
    visible_vs_1(Es, Mirror, Acc);
visible_vs_1([#edge{rf=Mirror,lf=Lf}|Es], Mirror, Acc) when Lf < 0 ->
    visible_vs_1(Es, Mirror, Acc);
visible_vs_1([#edge{lf=Lf,rf=Rf}|Es], Mirror, Acc) when Lf < 0, Rf < 0 ->
    visible_vs_1(Es, Mirror, Acc);
visible_vs_1([#edge{vs=Va,ve=Vb}|Es], Mirror, Acc) ->
    visible_vs_1(Es, Mirror, [Va,Vb|Acc]);
visible_vs_1([], _, Acc) -> ordsets:from_list(Acc).

visible_vs(Vs, #we{mirror=Face,es=Etab}=We) ->
    case any_hidden(We) of
	false -> Vs;
	true ->
	    Vis0 = visible_vs_1(gb_trees:values(Etab), Face, []),
	    case Vs of
		[{_,_}|_] ->
		    VsSet = sofs:relation(Vs),
		    VisSet = sofs:from_external(Vis0, [atom]),
		    sofs:to_external(sofs:restriction(VsSet, VisSet));
		[_|_] ->
		    ordsets:intersection(Vis0, Vs);
		[] ->
		    [];
		true ->
		    Vis = gb_sets:from_ordset(Vis0),
		    gb_sets:intersection(Vis, Vs)
	    end
    end.

visible_edges(#we{es=Etab,mirror=Face}=We) ->
    case any_hidden(We) of
	false -> gb_trees:keys(Etab);
	true -> visible_es_1(gb_trees:to_list(Etab), Face, [])
    end.

visible_es_1([{E,#edge{lf=Lf,rf=Rf}}|Es], Face, Acc) ->
    if
	Lf < 0 ->
	    %% Left face hidden.
	    if
		Rf < 0; Rf =:= Face ->
		    %% Both faces invisible (in some way).
		    visible_es_1(Es, Face, Acc);
		true ->
		    %% Right face is visible.
		    visible_es_1(Es, Face, [E|Acc])
	    end;
	Lf =:= Face, Rf < 0 ->
	    %% Left face mirror, right face hidden.
	    visible_es_1(Es, Face, Acc);
	true ->
	    %% At least one face visible.
	    visible_es_1(Es, Face, [E|Acc])
    end;
visible_es_1([], _, Acc) -> ordsets:from_list(Acc).

visible_edges(Es, We) ->
    case any_hidden(We) of
	false -> Es;
	true ->
	    Vis0 = visible_edges(We),
	    if
		is_list(Es) ->
		    ordsets:intersection(Vis0, Es);
		true ->
		    Vis = gb_sets:from_ordset(Vis0),
		    gb_sets:intersection(Vis, Es)
	    end
    end.

show_faces(We) ->
    case any_hidden(We) of
	false -> We;
	true -> show_faces_1(We)
    end.

show_faces_1(#we{es=Etab0}=We0) ->
    Map = fun(_, #edge{lf=Lf0,rf=Rf0}=R) when Lf0 < 0; Rf0 < 0 ->
		  Lf = show_face(Lf0),
		  Rf = show_face(Rf0),
		  R#edge{lf=Lf,rf=Rf};
	     (_, R) -> R
	  end,
    Etab = wings_util:gb_trees_map(Map, Etab0),
    We = We0#we{es=Etab,fs=undefined},
    wings_facemat:show_faces(rebuild(We)).

show_face(F) when F < 0 -> -F-1;
show_face(F) -> F.

validate_mirror(#we{mirror=none}=We) -> We;
validate_mirror(#we{fs=Ftab,mirror=Face}=We) ->
    case gb_trees:is_defined(Face, Ftab) of
	false -> We#we{mirror=none};
	true -> We
    end.

mirror_flatten(_, #we{mirror=none}=We) -> We;
mirror_flatten(#we{mirror=OldFace}=OldWe, #we{mirror=Face,vp=Vtab0}=We) ->
    PlaneNormal = wings_face:normal(OldFace, OldWe),
    FaceVs = wings_face:to_vertices(gb_sets:singleton(OldFace), OldWe),
    Origin = wings_vertex:center(FaceVs, OldWe),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    Flatten = e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))),
    Vtab = foldl(fun(V, Vt) ->
			 Pos0 = gb_trees:get(V, Vt),
			 Pos = e3d_mat:mul_point(Flatten, Pos0),
			 gb_trees:update(V, Pos, Vt)
		 end, Vtab0, wings_face:vertices_ccw(Face, We)),
    We#we{vp=Vtab}.
    
%%%
%%% Build Winged-Edges.
%%%

translate_faces([#e3d_face{vs=Vs,tx=Tx0,mat=Mat0}|Fs], Txs, Acc) ->
    Mat = translate_mat(Mat0),
    FaceData = case Tx0 of
		   [] -> {Mat,Vs};
		   Tx1 ->
		       Tx = [element(Tx+1, Txs) || Tx <- Tx1],
		       {Mat,Vs,Tx}
	       end,
    translate_faces(Fs, Txs, [FaceData|Acc]);
translate_faces([], _, Acc) -> reverse(Acc).

translate_mat([]) -> default;
translate_mat([Mat]) -> Mat;
translate_mat([_|_]=List) -> List.

build(Type, Fs0, Vs, HardEdges) ->
    {Es0,Fs} = build_and_fix_holes(Fs0, 0),
    Es = number_edges(Es0),
    build_rest(Type, Es, Fs, Vs, HardEdges).

build_and_fix_holes(Fs, N) 
  when N < 10 -> %% Assure that we don't loop forever
    case build_edges(Fs) of
	{Good, []} ->
	    {Good,Fs};
	{_, Bad} ->
	    HF = fill_holes(Bad),
	    [_|_] = HF, %% Assert that we could fix something
	    build_and_fix_holes(HF++Fs, N+1)
    end.

build_rest(Type, Es, Fs, Vs, HardEdges) ->
    Htab = vpairs_to_edges(HardEdges, Es),
    {Vct0,Etab,Ftab0} = build_tables(Es),
    Ftab = build_faces(Ftab0),
    Vct = gb_trees:from_orddict(build_incident_tab(Vct0)),
    Vpos = number_vertices(Vs, 0, []),
    We = update_id_bounds(#we{mode=Type,es=Etab,fs=Ftab,vc=Vct,vp=Vpos,he=Htab}),
    assign_materials(Fs, We).

assign_materials([L|_], We) when is_list(L) -> We;
assign_materials(Fs, We) ->
    MatFace = mat_face(Fs),
    wings_facemat:assign(MatFace, We).

mat_face(Fs) ->
    mat_face(Fs, 0, []).

mat_face([T|Ts], Face, Acc) ->
    mat_face(Ts, Face+1, [{Face,element(1, T)}|Acc]);
mat_face([], _, Acc) -> Acc.

number_vertices([P|Ps], V, Acc) ->
    number_vertices(Ps, V+1, [{V,P}|Acc]);
number_vertices([], _, Acc) ->
    gb_trees:from_orddict(reverse(Acc)).
    
build_edges(Fs) ->
    build_edges(Fs, 0, []).

build_edges([{_Material,Vs,Tx}|Fs], Face, Eacc0) ->
    build_edges_1(Vs, Tx, Fs, Face, Eacc0);
build_edges([{_Material,Vs}|Fs], Face, Eacc0) ->
    build_edges_1(Vs, tx_filler(Vs), Fs, Face, Eacc0);
build_edges([Vs|Fs], Face, Eacc0) ->
    build_edges_1(Vs, tx_filler(Vs), Fs, Face, Eacc0);
build_edges([], _Face, Eacc) ->
    combine_half_edges(wings_util:rel2fam(Eacc)).

build_edges_1(Vs, UVs, Fs, Face, Acc0) ->
    Vuvs = zip(Vs, UVs),
    Pairs = pairs(Vuvs),
    Acc = build_face_edges(Pairs, Face, Acc0),
    build_edges(Fs, Face+1, Acc).

build_face_edges([{Pred,_}|[{E0,{_UVa,UVb}},{Succ,_}|_]=Es], Face, Acc0) ->
    Acc = case E0 of
	      {Vs,Ve}=Name when Vs < Ve ->
		  enter_half_edge(right, Name, Face, Pred, Succ, UVb, Acc0);
	      {Vs,Ve} when Ve < Vs ->
		  Name = {Ve,Vs},
		  enter_half_edge(left, Name, Face, Pred, Succ, UVb, Acc0)
	  end,
    build_face_edges(Es, Face, Acc);
build_face_edges([_,_], _Face, Acc) -> Acc.

enter_half_edge(Side, Name, Face, Pred, Succ, UV,Tab0) ->
    Rec = {Face,UV,edge_name(Pred),edge_name(Succ)},
    [{Name,{Side,Rec}}|Tab0].

pairs(Vs) ->
    pairs(Vs, Vs, []).
    
pairs([{V1,T1}|[{V2,T2}|_]=Vs], First, Acc) ->
    pairs(Vs, First, [{{V2,V1},{T2,T1}}|Acc]);
pairs([{V,T}], [{V1,T1},{V2,T2},{V3,T3}|_], Acc) ->
    [{{V3,V2},{T3,T2}},{{V2,V1},{T2,T1}},{{V1,V},{T1,T}}|Acc].

edge_name({Vs,Ve}=Name) when Vs < Ve -> Name;
edge_name({Vs,Ve}) -> {Ve,Vs}.

tx_filler(Vs) ->
    tx_filler(Vs, none, []).

tx_filler([_|Vs], Col, Acc) ->
    tx_filler(Vs, Col, [Col|Acc]);
tx_filler([], _Col, Acc) -> Acc.

combine_half_edges(HalfEdges) ->
    combine_half_edges(HalfEdges, [], []).
    
combine_half_edges([{Name,[{left,Ldata},{right,Rdata}]}|Hes], Good, Bad) ->
    combine_half_edges(Hes, [{Name,{Ldata,Rdata}}|Good], Bad);
combine_half_edges([{_,[_]}=BadEdge|Hes], Good, Bad) ->
    combine_half_edges(Hes, Good, [BadEdge|Bad]);
combine_half_edges([], Good, Bad) ->
    {reverse(Good),reverse(Bad)}.

number_edges(Es) ->
    number_edges(Es, 1, []).

number_edges([{Name,{_Ldata,_Rdata}=Data}|Es], Edge, Tab0) ->
    Tab = [{Name,{Edge,Data}}|Tab0],
    number_edges(Es, Edge+1, Tab);
number_edges([], _Edge, Tab) -> reverse(Tab).

vpairs_to_edges([], _) -> gb_sets:empty();
vpairs_to_edges(HardNames0, Es) ->
    HardNames = sofs:set([edge_name(He) || He <- HardNames0], [name]),
    SofsEdges = sofs:from_external(Es, [{name,{edge,info}}]),
    SofsHard = sofs:image(SofsEdges, HardNames),
    Htab = sofs:to_external(sofs:domain(SofsHard)),
    gb_sets:from_list(Htab).

build_tables(Edges) ->
    Emap = make_edge_map(Edges),
    build_tables(Edges, Emap, [], [], []).

build_tables([H|T], Emap, Vtab0, Etab0, Ftab0) ->
    {{Vs,Ve},{Edge,{Ldata,Rdata}}} = H,
    {Lf,LUV,Lpred,Lsucc} = Ldata,
    {Rf,RUV,Rpred,Rsucc} = Rdata,
    Erec = #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,a=LUV,b=RUV,
		 ltpr=edge_num(Lf, Lpred, Emap),
		 ltsu=edge_num(Lf, Lsucc, Emap),
		 rtpr=edge_num(Rf, Rpred, Emap),
		 rtsu=edge_num(Rf, Rsucc, Emap)},
    Etab = [{Edge,Erec}|Etab0],
    Ftab = [{Lf,Edge},{Rf,Edge}|Ftab0],
    Vtab = [{Vs,Edge},{Ve,Edge}|Vtab0],
    build_tables(T, Emap, Vtab, Etab, Ftab);
build_tables([], _Emap, Vtab, Etab0, Ftab) ->
    Etab = gb_trees:from_orddict(reverse(Etab0)),
    {Vtab,Etab,Ftab}.

make_edge_map(Es) ->
    make_edge_map(Es, []).

make_edge_map([{Name,{Edge,{{Lf,_,_,_},{Rf,_,_,_}}}}|Es], Acc) ->
    make_edge_map(Es, [{{Lf,Name},Edge},{{Rf,Name},Edge}|Acc]);
make_edge_map([], Acc) -> gb_trees:from_orddict(keysort(1, Acc)).

edge_num(Face, Name, Emap) ->
    gb_trees:get({Face,Name}, Emap).

build_faces(Ftab0) ->
    Ftab = wings_util:rel2fam(Ftab0),
    build_faces_1(Ftab, []).

build_faces_1([{Face,[Edge|_]}|Fs], Acc) ->
    build_faces_1(Fs, [{Face,Edge}|Acc]);
build_faces_1([], Acc) -> gb_trees:from_orddict(reverse(Acc)).

fill_holes([]) ->  [];
fill_holes(Es) ->
    G = digraph:new(),
    make_digraph(Es, G),
    C = digraph_utils:cyclic_strong_components(G),
    Holes = make_hole_faces(G, C, []),
    digraph:delete(G),
    Holes.

make_hole_faces(G, [[V|_]|Cs], Acc) ->
    case digraph:get_cycle(G, V) of
	[_|Vs] when length(Vs) >= 3 ->
	    length(Vs) =:= length(ordsets:from_list(Vs)),
	    make_hole_faces(G, Cs, [{'_hole_',Vs}|Acc]);
	_Other ->
	    make_hole_faces(G, Cs, Acc)
    end;
make_hole_faces(_G, [], Acc) -> Acc.
    
make_digraph([{{Va,Vb},[{right,_Data}]}|Es], G) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    make_digraph(Es, G);
make_digraph([{{Vb,Va},[{left,_Data}]}|Es], G) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb),
    make_digraph(Es, G);
make_digraph([], _G) -> ok.

%%% Invert all normals.

invert_normals(#we{es=Etab0}=We0) ->
    Etab1 = invert_edges(gb_trees:to_list(Etab0), []),
    Etab = gb_trees:from_orddict(Etab1),
    We = We0#we{es=Etab},
    slide_colors(We).

invert_edges([{Edge,Rec0}|Es], Acc) ->
    #edge{vs=Vs,ve=Ve,ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Rec0,
    Rec = Rec0#edge{vs=Ve,ve=Vs,ltpr=Ltsu,ltsu=Ltpr,rtpr=Rtsu,rtsu=Rtpr},
    invert_edges(Es, [{Edge,Rec}|Acc]);
invert_edges([], Acc) -> reverse(Acc).

slide_colors(#we{fs=Ftab}=We) ->
    foldl(fun({Face,Edge}, W) ->
		  slide_colors(Face, Edge, W)
	  end, We, gb_trees:to_list(Ftab)).

slide_colors(Face, Edge, #we{es=Etab0}=We) ->
    PrevEdge = case gb_trees:get(Edge, Etab0) of
		   #edge{lf=Face,ltsu=Pe0} -> Pe0;
		   #edge{rf=Face,rtsu=Pe0} -> Pe0
	       end,
    PrevCol = case gb_trees:get(PrevEdge, Etab0) of
		  #edge{lf=Face,a=A} -> A;
		  #edge{rf=Face,b=B} -> B
	      end,
    Etab = slide_colors(Face, Edge, Edge, Etab0, PrevCol, not_done),
    We#we{es=Etab}.

slide_colors(_Face, LastEdge, LastEdge, Etab, _, done) -> Etab;
slide_colors(Face, Edge, LastEdge, Etab0, PrevCol, _) ->
    case gb_trees:get(Edge, Etab0) of
	#edge{a=Col,lf=Face,ltpr=NextEdge}=Rec ->
	    Etab = gb_trees:update(Edge, Rec#edge{a=PrevCol}, Etab0),
	    slide_colors(Face, NextEdge, LastEdge, Etab, Col, done);
	#edge{b=Col,rf=Face,rtpr=NextEdge}=Rec ->
	    Etab = gb_trees:update(Edge, Rec#edge{b=PrevCol}, Etab0),
	    slide_colors(Face, NextEdge, LastEdge, Etab, Col, done)
    end.

%% Merge two winged-edge structures.
merge(We0, We1) ->
    merge([We0,We1]).

%% Merge a list of winged-edge structures.
merge([]) -> [];
merge([We]) -> We;
merge([#we{id=Id,name=Name}|_]=Wes0) ->
    Pst  = merge_plugins(Wes0),
    Wes1 = merge_renumber(Wes0),
    MatTab = wings_facemat:merge(Wes1),
    {Vpt0,Et0,Ht0} = merge_1(Wes1),
    Vpt = gb_trees:from_orddict(Vpt0),
    Et = gb_trees:from_orddict(Et0),
    Ht = gb_sets:from_ordset(Ht0),
    rebuild(#we{id=Id,name=Name,vc=undefined,fs=undefined,
		pst=Pst,vp=Vpt,es=Et,he=Ht,mat=MatTab}).

merge_1([We]) -> We;
merge_1(Wes) -> merge_1(Wes, [], [], []).

merge_1([#we{vp=Vp0,es=Es,he=He}|Wes], Vpt0, Et0, Ht0) ->
    Vpt = [gb_trees:to_list(Vp0)|Vpt0],
    Et = [gb_trees:to_list(Es)|Et0],
    Ht = [gb_sets:to_list(He)|Ht0],
    merge_1(Wes, Vpt, Et, Ht);
merge_1([], Vpt0, Et0, Ht0) ->
    Vpt = lists:merge(Vpt0),
    Et = lists:merge(Et0),
    Ht = lists:merge(Ht0),
    {Vpt,Et,Ht}.

merge_plugins(Wes) ->
    Psts  = [gb_trees:keys(We#we.pst) || We <- Wes],
    PMods = lists:usort(lists:append(Psts)),
    Merge = fun(Mod,Acc) ->
		    try
			Pst = Mod:merge_we(Wes),
			[{Mod, Pst}|Acc]
		    catch _:_ -> Acc
		    end
	    end,
    Merged = lists:foldl(Merge, [], PMods),
    gb_trees:from_orddict(Merged).
		    
merge_renumber(Wes0) ->
    [{_,We}|Wes] = merge_bounds(Wes0, []),
    merge_renumber(Wes, [We], []).

merge_renumber([{Low,We}|Wes], [#we{next_id=Next}|_]=Done, NotDone)
  when Low >= Next ->
    merge_renumber(Wes, [We|Done], NotDone);
merge_renumber([{_,We}|Wes], Done, NotDone) ->
    merge_renumber(Wes, Done, [We|NotDone]);
merge_renumber([], [#we{next_id=Next}|_]=Done, NotDone) ->
    merge_renumber_rest(NotDone, Next, Done).

merge_renumber_rest([We0|Wes], Next0, Acc) ->
    #we{next_id=Next} = We = do_renumber(We0, Next0),
    merge_renumber_rest(Wes, Next, [We|Acc]);
merge_renumber_rest([], _, Acc) -> Acc.

merge_bounds([#we{vp=Vtab,fs=Ftab,es=Etab}=We0|Wes], Acc) ->
    First = case gb_trees:is_empty(Etab) of
		true -> 0;
		false ->
		    lists:min([wings_util:gb_trees_smallest_key(Vtab),
			       wings_util:gb_trees_smallest_key(Etab),
			       wings_util:gb_trees_smallest_key(Ftab)])
	    end,
    We = update_id_bounds(We0),
    merge_bounds(Wes, [{First,We}|Acc]);
merge_bounds([], Acc) -> sort(Acc).

%%% Renumber a winged-edge structure.
renumber(We0, Id) ->
    We = do_renumber(We0, Id),
    rebuild(We).

renumber(We0, Id, RootSet0) ->
    {We,RootSet} = do_renumber(We0, Id, RootSet0),
    {rebuild(We),RootSet}.

%% Leaves the vc and fs fields undefined.
do_renumber(We0, Id) ->
    {We,_} = do_renumber(We0, Id, []),
    We.

do_renumber(#we{mode=Mode,vp=Vtab0,es=Etab0,fs=Ftab0,
		mat=MatTab0,he=Htab0,perm=Perm0,mirror=Mirror0}=We0,
	    Id, RootSet0) ->
    Vtab1 = gb_trees:to_list(Vtab0),
    Vmap = make_map(Vtab1, Id),
    Vtab = renumber_vertices(Vtab1, Vmap),

    Fmap = make_map(gb_trees:to_list(Ftab0), Id),
    MatTab = wings_facemat:renumber(MatTab0, Fmap),

    Etab1 = gb_trees:to_list(Etab0),
    Emap = make_map(Etab1, Id),

    Etab2 = foldl(fun(E, A) ->
			  renum_edge(E, Emap, Vmap, Fmap, A)
		  end, [], Etab1),
    Etab = gb_trees:from_orddict(reverse(Etab2)),


    Htab1 = foldl(fun(E, A) ->
			  renum_hard_edge(E, Emap, A)
		  end, [], gb_sets:to_list(Htab0)),
    Htab = gb_sets:from_list(Htab1),

    Perm = case Perm0 of
	       {SelMode,Elems0} ->
		   Root = [{SelMode,gb_sets:to_list(Elems0),[]}],
		   [{_,Elems,_}] = map_rootset(Root, Emap, Vmap, Fmap),
		   {SelMode,gb_sets:from_list(Elems)};
	       _ -> Perm0
	   end,

    Mirror = if
		 Mirror0 == none -> Mirror0;
		 true -> gb_trees:get(Mirror0, Fmap)
	     end,

    RootSet = map_rootset(RootSet0, Emap, Vmap, Fmap),
    We = We0#we{mode=Mode,vc=undefined,fs=undefined,
		vp=Vtab,es=Etab,mat=MatTab,he=Htab,
		perm=Perm,mirror=Mirror},

    %% In case this function will be used for merging #we records,
    %% it is essential to update the next_id field. Its value can
    %% safely be based the largest key in the edge table only.
    LastId = case gb_trees:size(Etab) of
		 0 -> 0;
		 _ -> wings_util:gb_trees_largest_key(Etab)
	     end,
    {We#we{next_id=LastId+1},RootSet}.

map_rootset([{vertex,Vs,Data}|T], Emap, Vmap, Fmap) when is_list(Vs) ->
    [map_all(vertex, Vs, Data, Vmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edges,Data}|T], Emap, Vmap, Fmap) when is_list(Edges) ->
    [map_all(edge, Edges, Data, Emap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Faces,Data}|T], Emap, Vmap, Fmap) when is_list(Faces) ->
    [map_all(face, Faces, Data, Fmap)|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{body,_Empty,_Data}=Sel|T], Emap, Vmap, Fmap) ->
    [Sel|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{vertex,V}|T], Emap, Vmap, Fmap) ->
    [{vertex,gb_trees:get(V, Vmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{edge,Edge}|T], Emap, Vmap, Fmap) ->
    [{edge,gb_trees:get(Edge, Emap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{face,Face}|T], Emap, Vmap, Fmap) ->
    [{face,gb_trees:get(Face, Fmap)}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([{body,Empty}|T], Emap, Vmap, Fmap) ->
    [{body,Empty}|map_rootset(T, Emap, Vmap, Fmap)];
map_rootset([], _, _, _) -> [].

map_all(What, Items, Data, Map) ->
    {What,[gb_trees:get(Key, Map) || Key <- Items],Data}.

make_map(Tab, Id0) ->
    make_map(Tab, Id0, []).
make_map([{Old,_}|T], Id, Map) ->
    make_map(T, Id+1, [{Old,Id}|Map]);
make_map([], _, Map) -> gb_trees:from_orddict(reverse(Map)).

renum_edge({Edge0,Rec0}, Emap, Vmap, Fmap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=Ltpr,ltsu=Ltsu,
	  rtpr=Rtpr,rtsu=Rtsu} = Rec0,
    Rec = Rec0#edge{vs=gb_trees:get(Vs, Vmap),ve=gb_trees:get(Ve, Vmap),
		    lf=gb_trees:get(Lf, Fmap),rf=gb_trees:get(Rf, Fmap),
		    ltpr=gb_trees:get(Ltpr, Emap),
		    ltsu=gb_trees:get(Ltsu, Emap),
		    rtpr=gb_trees:get(Rtpr, Emap),
		    rtsu=gb_trees:get(Rtsu, Emap)},
    [{Edge,Rec}|New].

renumber_vertices(Vtab, Vmap) ->
    renumber_vertices_1(Vtab, Vmap, []).

renumber_vertices_1([{V0,P}|Vtab], Vmap, VtabAcc) ->
    V = gb_trees:get(V0, Vmap),
    renumber_vertices_1(Vtab, Vmap, [{V,P}|VtabAcc]);
renumber_vertices_1([], _, Vtab) ->
    gb_trees:from_orddict(keysort(1, Vtab)).
    
renum_hard_edge(Edge0, Emap, New) ->
    Edge = gb_trees:get(Edge0, Emap),
    [Edge|New].

update_id_bounds(#we{vp=Vtab,es=Etab,fs=Ftab}=We) ->
    case gb_trees:is_empty(Etab) of
	true -> We#we{next_id=0};
	false ->
	    LastId = lists:max([wings_util:gb_trees_largest_key(Vtab),
				wings_util:gb_trees_largest_key(Etab),
				wings_util:gb_trees_largest_key(Ftab)]),
	    We#we{next_id=LastId+1}
    end.

%%%
%%% Separate a combined winged-edge structure.
%%%

separate(We) ->
    separate(We#we{mirror=none,vc=undefined,fs=undefined}, []).

separate(#we{es=Etab0}=We, Acc) ->
    case gb_trees:is_empty(Etab0) of
	true -> Acc;
	false ->
	    {Edge,_,_} = gb_trees:take_smallest(Etab0),
	    Ws = gb_sets:singleton(Edge),
	    {EtabLeft,NewEtab} = separate(Ws, Etab0, gb_trees:empty()),
	    NewWe = copy_dependents(We#we{es=NewEtab}),
	    separate(We#we{es=EtabLeft}, [NewWe|Acc])
    end.

separate(Ws0, Etab0, Acc0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {Etab0,Acc0};
	false ->
	    {Edge,Ws1} = gb_sets:take_smallest(Ws0),
	    case gb_trees:is_defined(Edge, Acc0) of
		true ->
		    separate(Ws1, Etab0, Acc0);
		false ->
		    Rec = gb_trees:get(Edge, Etab0),
		    Etab = gb_trees:delete(Edge, Etab0),
		    Acc = gb_trees:insert(Edge, Rec, Acc0),
		    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
		    Set = gb_sets:from_list([LP,LS,RP,RS]),
		    Ws = gb_sets:union(Ws1, Set),
		    separate(Ws, Etab, Acc)
	    end
    end.

copy_dependents(We0) ->
    #we{es=Etab,he=Htab0,vc=Vct,vp=Vtab0} = We = rebuild(We0),
    Es = gb_trees:keys(Etab),
    Htab = case gb_sets:is_empty(Htab0) of
	       true ->
		   Htab0;
	       false ->
		   gb_sets:intersection(Htab0, gb_sets:from_ordset(Es))
	   end,
    Vs = sofs:from_external(gb_trees:keys(Vct), [vertex]),
    Vtab1 = sofs:relation(gb_trees:to_list(Vtab0), [{vertex,edge}]),
    Vtab2 = sofs:restriction(Vtab1, Vs),
    Vtab = gb_trees:from_orddict(sofs:to_external(Vtab2)),
    wings_facemat:gc(We#we{he=Htab,vp=Vtab}).

%% build_incident_tab([{Elem,Edge}]) -> [{Elem,Edge}]
%%      Elem = Face or Vertex
%%  Build the table of incident edges for either faces or vertices.
%%  Returns an ordered list where each Elem is unique.

build_incident_tab(ElemToEdgeRel) ->
    T = ets:new(?MODULE, [ordered_set]),
    ets:insert(T, ElemToEdgeRel),
    R = ets:tab2list(T),
    ets:delete(T),
    R.

%% Reference implementation below. On my Mac is twice as slow as the
%% implementation above (lists:keysort/2 takes most of the time).

%% build_incident_tab(ElemToEdgeRel) ->
%%     build_incident_tab_1(keysort(1, ElemToEdgeRel), []).
    
%% build_incident_tab_1([{V,_}|VsEs], [{V,_}|_]=Acc) ->
%%     build_incident_tab_1(VsEs, Acc);
%% build_incident_tab_1([Pair|VsEs], Acc) ->
%%     build_incident_tab_1(VsEs, [Pair|Acc]);
%% build_incident_tab_1([], Acc) -> reverse(Acc).

%%%
%%% Convert textures to vertex colors.
%%%

uv_to_color(#we{mode=material,es=Etab0}=We, St) ->
    Etab1 = foldl(
	      fun({Edge,#edge{lf=Lf,rf=Rf,a=UVa,b=UVb}=Rec}, A) ->
		      ColA = wings_material:color(Lf, UVa, We, St),
		      ColB = wings_material:color(Rf, UVb, We, St),
		      [{Edge,Rec#edge{a=ColA,b=ColB}}|A]
	      end, [], gb_trees:to_list(Etab0)),
    Etab = gb_trees:from_orddict(reverse(Etab1)),
    We#we{mode=vertex,es=Etab};
uv_to_color(We, _St) -> We.

%% uv_mapped_faces(We) -> [Face]
%%  Return an ordered list of all faces that have UV coordinates.
uv_mapped_faces(#we{fs=Ftab}=We) ->
    uv_mapped_faces_1(gb_trees:to_list(Ftab), We, []).

uv_mapped_faces_1([{F,E}|Fs], We, Acc) ->
    Good = foldl(fun([_|{_,_}], Flag) -> Flag;
		    (_, _) -> false
		 end, true, wings_face:vinfo_ccw(F, E, We)),
    case Good of
	false -> uv_mapped_faces_1(Fs, We, Acc);
	true -> uv_mapped_faces_1(Fs, We, [F|Acc])
    end;
uv_mapped_faces_1([], _, Acc) -> reverse(Acc).

%%%
%%% Transform all vertices according to the matrix.
%%%

transform_vs({1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0,Tx,Ty,Tz}, We) ->
    Translate = fun({V,{X,Y,Z}}, A) -> [{V,{X+Tx,Y+Ty,Z+Tz}}|A] end,
    transform_vs_1(Translate, We);
transform_vs(Matrix, We) ->
    Transform = fun({V,Pos}, A) ->
			[{V,e3d_mat:mul_point(Matrix, Pos)}|A]
		end,
    transform_vs_1(Transform, We).

transform_vs_1(Transform, #we{vp=Vtab0}=We) ->
    Vtab1 = foldl(Transform, [], gb_trees:to_list(Vtab0)),
    Vtab = gb_trees:from_orddict(reverse(Vtab1)),
    We#we{vp=Vtab}.

%%%
%%% Calculate normals.
%%%

normals(Ns, #we{mirror=none}=We) ->
    case any_hidden(We) of
	false -> normals_2(Ns, We);
	true -> normals_1(Ns, We)
    end;
normals(Ns, We) -> normals_1(Ns, We).

normals_1(FaceNormals, #we{fs=Ftab,he=Htab0}=We) ->
    Edges = case {visible(We),gb_trees:size(Ftab)} of
		{Vis,Sz} when 2*length(Vis) < Sz ->
		    wings_face:outer_edges(Vis, We);
		{Vis,_} ->
		    InVis = ordsets:subtract(gb_trees:keys(Ftab), Vis),
		    wings_face:outer_edges(InVis, We)
	    end,
    Htab = gb_sets:union(Htab0, gb_sets:from_ordset(Edges)),
    normals_2(FaceNormals, We#we{he=Htab}).

normals_2(FaceNormals, #we{he=He}=We) ->
    wings_pb:start(?__(1,"calculating soft normals")),
    Res = case FaceNormals of
	      [_,_] ->
		  two_faced(FaceNormals, We);
	      _ ->
		  case gb_sets:is_empty(He) of
		      true -> all_soft(FaceNormals, We);
		      false -> mixed_edges(FaceNormals, We)
		  end
	  end,
    wings_pb:done(Res).

all_soft(FaceNormals, #we{vp=Vtab}=We) ->
    wings_pb:update(0.10, ?__(1,"preparing")),
    VisVs = visible_vs(gb_trees:to_list(Vtab), We),
    VtxNormals = soft_vertex_normals(VisVs, FaceNormals, We),
    FoldFun = fun(V, VInfo, A) ->
		      Normal = gb_trees:get(V, VtxNormals),
		      [[VInfo|Normal]|A]
	      end,
    wings_pb:update(0.6, ?__(2,"collecting")),
    all_soft_1(FoldFun, FaceNormals, We, []).

all_soft_1(FoldFun, [{Face,_}|FNs], We, Acc) ->
    Vs = wings_face:fold_vinfo(FoldFun, [], Face, We),
    all_soft_1(FoldFun, FNs, We, [{Face,Vs}|Acc]);
all_soft_1(_, [], _, Acc) -> reverse(Acc).

mixed_edges(FaceNormals0, We) ->
    wings_pb:update(0.20, ?__(1,"preparing")),
    G = digraph:new(),
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    wings_pb:update(0.50,  ?__(2,"vertex normals")),
    VtxNormals = vertex_normals(We, G, FaceNormals),
    wings_pb:update(0.99,  ?__(3,"vertex normals per face")),
    Ns = foldl(fun({Face,_}, Acc) ->
		       Vs = n_face(Face, G, FaceNormals, VtxNormals, We),
		       [{Face,Vs}|Acc]
	       end, [], FaceNormals0),
    digraph:delete(G),
    reverse(Ns).

n_face(Face, G, FaceNormals, VtxNormals, We) ->
    wings_face:fold_vinfo(
	   fun(V, VInfo, Acc) ->
		   case gb_trees:lookup(V, VtxNormals) of
		       {value,Normal} ->
			   [[VInfo|Normal]|Acc];
		       none ->
			   Normal = hard_vtx_normal(G, V, Face, FaceNormals),
 			   [[VInfo|Normal]|Acc]
		   end
	   end, [], Face, We).

hard_vtx_normal(G, V, Face, FaceNormals) ->
    Reachable = digraph_utils:reachable([{V,Face}], G),
    case [gb_trees:get(AFace, FaceNormals) || {_,AFace} <- Reachable] of
 	[N] -> N;
 	Ns -> e3d_vec:norm(e3d_vec:add(Ns))
    end.

two_faced([{FaceA,Na},{FaceB,Nb}], We) ->
    [{FaceA,two_faced_1(FaceA, Na, We)},
     {FaceB,two_faced_1(FaceB, Nb, We)}].

two_faced_1(Face, Normal, We) ->
    wings_face:fold_vinfo(fun (_, VInfo, Acc) ->
				  [[VInfo|Normal]|Acc]
			  end, [], Face, We).

vertex_normals(#we{vp=Vtab}=We, G, FaceNormals) ->
    Vs0 = visible_vs(gb_trees:to_list(Vtab), We),
    Vs = sofs:from_external(Vs0, [{vertex,data}]),
    vertex_normals_1(Vs, We, G, FaceNormals).

vertex_normals_1(Vs, #we{es=Etab,he=Htab}=We, G, FaceNormals) ->
    He0 = gb_sets:to_list(Htab),
    He = sofs:from_external(He0, [edge]),
    Es0 = gb_trees:to_list(Etab),
    Es1 = sofs:from_external(Es0, [{edge,data}]),
    Es = sofs:image(Es1, He),
    Hvs0 = foldl(fun(#edge{vs=Va,ve=Vb}, A) ->
			 [Va,Vb|A]
		 end, [], sofs:to_external(Es)),
    Hvs = sofs:set(Hvs0, [vertex]),
    Svs = sofs:drestriction(Vs, Hvs),
    SoftVs = sofs:to_external(Svs),
    HardVs = sofs:to_external(Hvs),
    foreach(fun(V) -> update_digraph(G, V, We) end, HardVs),
    soft_vertex_normals(SoftVs, FaceNormals, We).

update_digraph(G, V, #we{he=Htab}=We) ->
    wings_vertex:fold(
      fun(Edge, _, #edge{lf=Lf0,rf=Rf0}, _) ->
	      case gb_sets:is_member(Edge, Htab) of
		  true -> ok;
		  false ->
		      Lf = {V,Lf0},
		      Rf = {V,Rf0},
		      digraph:add_vertex(G, Lf),
		      digraph:add_vertex(G, Rf),
		      digraph:add_edge(G, Lf, Rf),
		      digraph:add_edge(G, Rf, Lf)
	      end
      end, [], V, We).

soft_vertex_normals(Vtab, FaceNormals0, We) when is_list(FaceNormals0) ->
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    soft_vertex_normals(Vtab, FaceNormals, We);
soft_vertex_normals(Vtab, FaceNormals, We) ->
    FoldFun = fun(_, Face, _, A) ->
		      [gb_trees:get(Face, FaceNormals)|A]
	      end,
    Soft = foldl(fun({V,_}, Acc) ->
			 Ns = wings_vertex:fold(FoldFun, [], V, We),
			 N = e3d_vec:norm(e3d_vec:add(Ns)),
			 [{V,N}|Acc]
		 end, [], Vtab),
    gb_trees:from_orddict(reverse(Soft)).


new_items_as_ordset_1(Tab, Wid, NewWid) when NewWid-Wid < 32 ->
    new_items_as_ordset_2(Wid, NewWid, Tab, []);
new_items_as_ordset_1(Tab, Wid, _NewWid) ->
    [Item || Item <- gb_trees:keys(Tab), Item >= Wid].

new_items_as_ordset_2(Wid, NewWid, Tab, Acc) when Wid < NewWid ->
    case gb_trees:is_defined(Wid, Tab) of
	true -> new_items_as_ordset_2(Wid+1, NewWid, Tab, [Wid|Acc]);
	false -> new_items_as_ordset_2(Wid+1, NewWid, Tab, Acc)
    end;
new_items_as_ordset_2(_Wid, _NewWid, _Tab, Acc) -> reverse(Acc).

%%%
%%% Test the consistency of a #we{}.
%%%

is_consistent(#we{}=We) ->
    try
	validate_vertex_tab(We),
	validate_faces(We)
    catch error:_ -> false
    end.

is_face_consistent(Face, #we{fs=Ftab,es=Etab}) ->
    Edge = gb_trees:get(Face, Ftab),
    try validate_face(Face, Edge, Etab)
    catch error:_ -> false
    end.
    
validate_faces(#we{fs=Ftab,es=Etab}) ->
    validate_faces_1(gb_trees:to_list(Ftab), Etab).

validate_faces_1([{Face,Edge}|Fs], Etab) ->
    validate_face(Face, Edge, Etab),
    validate_faces_1(Fs, Etab);
validate_faces_1([], _) -> true.

validate_face(Face, Edge, Etab) ->
    Ccw = walk_face_ccw(Edge, Etab, Face, Edge, []),
    Edge = walk_face_cw(Edge, Etab, Face, Ccw),
    [V|Vs] = sort(Ccw),
    validate_face_vertices(Vs, V).

validate_face_vertices([V|_], V) ->
    erlang:error(repeated_vertex);
validate_face_vertices([_], _) ->
    true;
validate_face_vertices([V|Vs], _) ->
    validate_face_vertices(Vs, V).

walk_face_ccw(LastEdge, _, _, LastEdge, [_|_]=Acc) -> Acc;
walk_face_ccw(Edge, Etab, Face, LastEdge, Acc) ->
    case gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Next, Etab, Face, LastEdge, [V|Acc])
    end.

walk_face_cw(Edge, _, _, []) -> Edge;
walk_face_cw(Edge, Etab, Face, [V|Vs]) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Next, Etab, Face, Vs)
    end.

validate_vertex_tab(#we{es=Etab,vc=Vct}) ->
    foreach(fun({V,Edge}) ->
		    case gb_trees:get(Edge, Etab) of
			#edge{vs=V} -> ok;
			#edge{ve=V} -> ok
		    end
	    end, gb_trees:to_list(Vct)).
