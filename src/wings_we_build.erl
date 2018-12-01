%%
%%  wings_we_build.erl --
%%
%%     This module builds a winged-edge data structure from a polygon mesh.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_we_build).
-export([we/3,incident_tab/1]).

-include("wings.hrl").

-import(lists, [keysort/2,reverse/1,zip/2,foldl/3,member/2]).

%% we([Face], VertexPositions, HardEdges) -> We | error
%%      Face = {Material,Vertices} | {Material,Vertices,UVS}
%%      Vertices = [VertexIndex]
%%      VertexPositions = [{X,Y,Z}]
%%      HardEdges = [{VertexIndex1,VertexIndex2}]
%%  Construct a new #we{} record.
%%
we(Fs0, Vs0, HardEdges0) ->
    try
	{Es1,Fs,Vs,HardEdges} = build_etab(Fs0, Vs0, HardEdges0),
	Es = number_edges(Es1),
	build_rest(Es, Fs, Vs, HardEdges)
    catch
	throw:bad_model ->
	    error
    end.

%% incident_tab([{Elem,Edge}]) -> [{Elem,Edge}]
%%      Elem = Face or Vertex
%%  Build the table of incident edges for either faces or vertices.
%%  Returns an ordered list where each Elem is unique.
%%
incident_tab(ElemToEdgeRel) ->
    %% Measurements show that using an ETS table is faster than
    %% an implementation based on lists:keysort/2.
    T = ets:new(?MODULE, [ordered_set]),
    ets:insert(T, ElemToEdgeRel),
    R = ets:tab2list(T),
    ets:delete(T),
    R.

%%%
%%% Local functions.
%%%

build_etab(Fs0, Vs0, HardEdges0) ->
    case eliminate_shared_vertices(Fs0, Vs0, HardEdges0) of
	{_,_,_,_}=Done ->
	    %% There were neither any shared vertices and nor holes.
	    Done;
	{Fs1,Vs1,HardEdges} ->
	    %% Shared vertices and/or holes have been eliminated.
	    %% Re-try the build. (We may need several iterations
	    %% to fill all holes.)
	    {Es,Fs} = build_and_fix_holes(Fs1, 0),
	    {Es,Fs,Vs1,HardEdges}
    end.

build_and_fix_holes(Fs, N) when N < 9 ->
    case build_edges(Fs) of
	{Good,[]} ->
	    {Good,Fs};
	{_,Unconnectable} ->
	    case fill_holes(Unconnectable) of
		[_|_]=HF ->
		    build_and_fix_holes(HF++Fs, N+1);
		[] ->
		    %% No possible to fill any more holes, but there are
		    %% still unconnectable edges. Give up.
		    throw(bad_model)
	    end
    end;
build_and_fix_holes(_, _) ->
    %% Too many attempts to fill holes. Give up.
    throw(bad_model).

build_rest(Es, Fs, Vs, HardEdges) ->
    Htab = vpairs_to_edges(HardEdges, Es),
    {Vct0,Etab,Ftab0,UvVcTab} = build_tables(Es),
    Ftab = build_faces(Ftab0),
    Vct = array:from_orddict(incident_tab(Vct0)),
    Vpos = number_vertices(Vs, 0, []),
    NextId = case wings_util:array_is_empty(Etab) of
		 true ->
		     0;
		 false ->
		     %% Since this We is newly created, we KNOW (Euler's formula)
		     %% that the greatest key is found in the edge table.
		     wings_util:array_greatest_key(Etab)+1
	     end,
    We0 = #we{next_id=NextId,es=Etab,fs=Ftab,vc=Vct,vp=Vpos,he=Htab},
    We = wings_va:set_edge_attrs(UvVcTab, We0),
    assign_materials(Fs, We).

assign_materials([L|_], We) when is_list(L) -> We;
assign_materials(Fs, We0) ->
    {MatFace,Holes} = mat_face(Fs),
    We = wings_facemat:assign(MatFace, We0),
    wings_we:create_holes(Holes, We).

mat_face(Fs) ->
    mat_face(Fs, 0, [], []).

mat_face([T|Ts], Face, Acc, HoleAcc) ->
    case element(1, T) of
	{hole} ->
	    mat_face(Ts, Face+1, Acc, [Face|HoleAcc]);
	Mat when is_atom(Mat) ->
	    mat_face(Ts, Face+1, [{Face,Mat}|Acc], HoleAcc)
    end;
mat_face([], _, Acc, HoleAcc) ->
    {Acc,ordsets:from_list(HoleAcc)}.

number_vertices([P|Ps], V, Acc) ->
    number_vertices(Ps, V+1, [{V,P}|Acc]);
number_vertices([], _, Acc) ->
    array:from_orddict(reverse(Acc)).

build_edges(Fs) ->
    HalfEdges = build_half_edges(Fs),
    combine_half_edges(wings_util:rel2fam(HalfEdges)).

build_half_edges(Fs) ->
    build_half_edges(Fs, 0, []).

build_half_edges([{_Material,Vs,none,Vc}|Fs], Face, Eacc0) ->	% imported with vertex color only
    build_half_edges_1(Vs, tx_filler(Vs), Vc, Fs, Face, Eacc0);
build_half_edges([{_Material,Vs,Tx,none}|Fs], Face, Eacc0) ->	% imported with textures only
    build_half_edges_1(Vs, Tx, tx_filler(Vs), Fs, Face, Eacc0);
build_half_edges([{_Material,Vs,Tx,Vc}|Fs], Face, Eacc0) ->	% imported with textures and vertex color
    build_half_edges_1(Vs, Tx, Vc, Fs, Face, Eacc0);
build_half_edges([{_Material,Vs}|Fs], Face, Eacc0) ->	% imported without textures or vertex color
    build_half_edges_1(Vs, tx_filler(Vs), tx_filler(Vs), Fs, Face, Eacc0);
build_half_edges([Vs|Fs], Face, Eacc0) ->	% new primitives
    build_half_edges_1(Vs, tx_filler(Vs), tx_filler(Vs), Fs, Face, Eacc0);
build_half_edges([], _Face, HalfEdges) -> HalfEdges.

build_half_edges_1(Vs, UVs, VCs, Fs, Face, Acc0) ->
    UvsVcs = zip(UVs,VCs),
    Vuvs = zip(Vs, UvsVcs),
    Pairs = pairs(Vuvs),
    Acc = build_face_edges(Pairs, Face, Acc0),
    build_half_edges(Fs, Face+1, Acc).

build_face_edges([{Pred,_}|[{E0,{{_UVa,_VCa},{UVb,VCb}}},{Succ,_}|_]=Es], Face, Acc0) ->
    Acc = case E0 of
	      {Vs,Ve}=Name when Vs < Ve ->
		  enter_half_edge(right, Name, Face, Pred, Succ, {UVb,VCb}, Acc0);
	      {Vs,Ve} when Ve < Vs ->
		  Name = {Ve,Vs},
		  enter_half_edge(left, Name, Face, Pred, Succ, {UVb,VCb}, Acc0)
	  end,
    build_face_edges(Es, Face, Acc);
build_face_edges([_,_], _Face, Acc) -> Acc.

enter_half_edge(Side, Name, Face, Pred, Succ, UVVC,Tab0) ->
    Rec = {Face,UVVC,edge_name(Pred),edge_name(Succ)},
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
    number_edges(Es, 0, []).

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
    build_tables(Edges, Emap, [], [], [], []).

build_tables([H|T], Emap, Vtab0, Etab0, Ftab0, UvVcTab0) ->
    {{Vs,Ve},{Edge,{Ldata,Rdata}}} = H,
    {Lf,{LUV,LVC},Lpred,Lsucc} = Ldata,
    {Rf,{RUV,RVC},Rpred,Rsucc} = Rdata,
    Erec = #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,
		 ltpr=edge_num(Lf, Lpred, Emap),
		 ltsu=edge_num(Lf, Lsucc, Emap),
		 rtpr=edge_num(Rf, Rpred, Emap),
		 rtsu=edge_num(Rf, Rsucc, Emap)},
    Etab = [{Edge,Erec}|Etab0],
    Ftab = [{Lf,Edge},{Rf,Edge}|Ftab0],
    Vtab = [{Vs,Edge},{Ve,Edge}|Vtab0],
    UvVcTab = case {LUV,RUV,LVC,RVC} of
		{none,none,none,none} -> UvVcTab0;
		{_,_,_,_} -> [{Edge,LUV,RUV,LVC,RVC}|UvVcTab0]
	    end,
    build_tables(T, Emap, Vtab, Etab, Ftab, UvVcTab);
build_tables([], _Emap, Vtab, Etab0, Ftab, UvVcTab) ->
    Etab = array:from_orddict(reverse(Etab0)),
    {Vtab,Etab,Ftab,UvVcTab}.

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
	    true = length(Vs) =:= length(ordsets:from_list(Vs)),
	    %% We'll mark holes with {hole} instead of a material.
	    make_hole_faces(G, Cs, [{{hole},Vs}|Acc]);
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

%%%
%%% Eliminate "shared vertices", that is vertices that are shared
%%% between geometry that does not share edges. Wings does not
%%% allow creation of shared vertices using modeling commands, so
%%% it should not create them when importing models.
%%%
%%% Example of a shared vertex:
%%%
%%%    A +------------+ B
%%%    	 |     	      |
%%%    	 |	      |
%%%    	 |	      |
%%%    	 |	      |
%%%    C +------------+-------------+ E
%%%		    D |	       	    |
%%%		      |		    |
%%%		      |		    |
%%%		      |		    |
%%%		    F +-------------+ G
%%%
%%% We eliminate shared vertices by introducing a new vertex
%%% at the same position as the shared vertex (no vertices are
%%% moved):
%%%
%%%    A +------------+ B
%%%    	 |     	      |
%%%    	 |	      |
%%%    	 |	      |
%%%    	 |	      |
%%%    C +------------+ D
%%%                   H +-------------+ E
%%%		        |	      |
%%%		        |	      |
%%%		        |	      |
%%%		        |	      |
%%%		      F +-------------+	G
%%%
%%% If there is face sharing the edge D-E, that face would be
%%% modified to have the edge H-E instead.
%%%

eliminate_shared_vertices(Fs0, Vs0, HardEdges0) ->
    {Edges,Uncombinable} = build_edges(Fs0),
    case find_shared_vertices(Edges) of
	[] ->
	    %% There were no shared vertices.
	    case Uncombinable of
		[] ->
		    %% No holes. We are done.
		    {Edges,Fs0,Vs0,HardEdges0};
		_ ->
		    %% There were some edges that could not be combined.
		    %% Trying filling in holes.
		    HF = fill_holes(Uncombinable),
		    {HF++Fs0,Vs0,HardEdges0}
	    end;
	Shared ->
	    do_eliminate_shared_vs(Shared, Fs0, Vs0, HardEdges0)
    end.

do_eliminate_shared_vs(Shared, Fs0, Vs0, HardEdges0) ->
    R0 = sofs:relation(Shared, [{vertex,[face]}]),

    %% Compute DupVs, an ordered list of vertices to be duplicated.
    DupVs = sofs:to_external(sofs:domain(R0)),

    %% First add the duplicated vertex positions to the end of
    %% vertex list.
    Vtab = list_to_tuple(Vs0),
    Vs = Vs0 ++ [element(V+1, Vtab) || V <- DupVs],

    %% Create a map from old vertex number to new vertex number.
    VtxMap = elim_vtx_map(DupVs, length(Vs0), []),

    %% Compute which vertices to rename in which faces:
    %%   [{Face,[Vertex]}]
    R1 = sofs:family_to_relation(R0),
    R2 = sofs:converse(R1),
    R3 = sofs:relation_to_family(R2),
    R = sofs:to_external(R3),

    %% Renumber vertices in the appropriate faces.
    Fs = elim_renum_vs(Fs0, R, 0, VtxMap, []),

    %% XXX There is no easy way to safely renumber hard edges
    %% with the information we have collected. For the moment,
    %% accept that hard edges around shared vertices may get
    %% lost. I will fix it if it turns out to be an issue in
    %% practice.
    HardEdges = HardEdges0,

    {Fs,Vs,HardEdges}.

elim_vtx_map([V|Vs], Id, Acc) ->
    elim_vtx_map(Vs, Id+1, [{V,Id}|Acc]);
elim_vtx_map([], _, Acc) ->
    array:from_orddict(reverse(Acc)).

elim_renum_vs([{Mat,Vs0}|Faces], [{Face,DupVs}|ToDo], Face, VtxMap, Acc) ->
    Vs = elim_renum_vs_1(Vs0, DupVs, VtxMap),
    elim_renum_vs(Faces, ToDo, Face+1, VtxMap, [{Mat,Vs}|Acc]);
elim_renum_vs([{Mat,Vs0,Tx,Vc}|Faces], [{Face,DupVs}|ToDo], Face, VtxMap, Acc) ->
    Vs = elim_renum_vs_1(Vs0, DupVs, VtxMap),
    elim_renum_vs(Faces, ToDo, Face+1, VtxMap, [{Mat,Vs,Tx,Vc}|Acc]);
elim_renum_vs([MatVs|Faces], ToDo, Face, VtxMap, Acc) ->
    elim_renum_vs(Faces, ToDo, Face+1, VtxMap, [MatVs|Acc]);
elim_renum_vs([], [], _, _, Acc) -> reverse(Acc).

elim_renum_vs_1(Vs, DupVs, VtxMap) ->
    [begin
	 case member(V, DupVs) of
	     false -> V;
	     true -> array:get(V, VtxMap)
	 end
     end || V <- Vs].

find_shared_vertices(Es) ->
    R0 = shared_vs_edges(Es, []),
    R1 = sofs:relation(R0, [{vertex,data}]),
    R2 = sofs:relation_to_family(R1),
    R = sofs:to_external(R2),
    shared_vs_1(R, []).

shared_vs_edges([{{Va,Vb}=E,Data}|T], Acc0) ->
    Acc = [{Va,{E,Data}},{Vb,{E,Data}}|Acc0],
    shared_vs_edges(T, Acc);
shared_vs_edges([], Acc) -> Acc.

shared_vs_1([{V,Es}|T], Acc0) ->
    case are_all_connected(V, Es) of
	true ->
	    shared_vs_1(T, Acc0);
	false ->
	    case shared_digraph(Es) of
		[_] ->
		    shared_vs_1(T, Acc0);
		[_|Cs] ->
		    Acc = foldl(fun(Face, A) ->
					[{V,Face}|A]
				end, Acc0, Cs),
		    shared_vs_1(T, Acc)
	    end
    end;
shared_vs_1([], Acc) -> Acc.

are_all_connected(V, [E|Es]) ->
    are_all_connected_1(V, E, Es).

are_all_connected_1(V, {{V,_}=E,{_,{F,_,Pred,_}}}, Es) ->
    are_all_connected_2(V, F, Pred, E, Es);
are_all_connected_1(V, {{_,V}=E,{_,{F,_,_,Succ}}}, Es) ->
    are_all_connected_2(V, F, Succ, E, Es).

are_all_connected_2(_, _, Last, Last, []) -> true;
are_all_connected_2(_, _, Last, Last, [_|_]) -> false;
are_all_connected_2(V, Face, Edge, Last, Es0) ->
    case orddict:find(Edge, Es0) of
	error ->
	    false;
	{ok,Data} ->
	    Es = orddict:erase(Edge, Es0),
	    case {Edge,Data} of
		{{V,_},{{Face,_,_,_},{Other,_,NextEdge,_}}} ->
		    are_all_connected_2(V, Other, NextEdge, Last, Es);
		{{_,V},{{Face,_,_,_},{Other,_,_,NextEdge}}} ->
		    are_all_connected_2(V, Other, NextEdge, Last, Es);
		{{V,_},{{Other,_,_,NextEdge},{Face,_,_,_}}} ->
		    are_all_connected_2(V, Other, NextEdge, Last, Es);
		{{_,V},{{Other,_,NextEdge,_},{Face,_,_,_}}} ->
		    are_all_connected_2(V, Other, NextEdge, Last, Es)
	    end
    end.

shared_digraph(Es) ->
    G = digraph:new(),
    shared_digraph_1(Es, G),
    Res = digraph_utils:components(G),
    digraph:delete(G),
    Res.

shared_digraph_1([{_,{{Lf,_,_,_},{Rf,_,_,_}}}|Es], G) ->
    digraph:add_vertex(G, Lf),
    digraph:add_vertex(G, Rf),
    digraph:add_edge(G, Lf, Rf),
    shared_digraph_1(Es, G);
shared_digraph_1([], _) -> ok.
