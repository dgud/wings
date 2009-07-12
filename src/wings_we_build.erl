%%
%%  wings_we_build.erl --
%%
%%     This module builds a winged-edge data structure from a polygon mesh.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_we_build).
-export([we/3,incident_tab/1]).

-include("wings.hrl").

-import(lists, [keysort/2,reverse/1,zip/2]).

%% we([Face], VertexPositions, HardEdges) -> We
%%      Face = {Material,Vertices} | {Material,Vertices,UVS}
%%      Vertices = [VertexIndex]
%%      VertexPositions = [{X,Y,Z}]
%%      HardEdges = [{VertexIndex1,VertexIndex2}]
%%  Construct a new #we{} record.
%%
we(Fs0, Vs, HardEdges) ->
    {Es0,Fs} = build_and_fix_holes(Fs0, 0),
    Es = number_edges(Es0),
    build_rest(Es, Fs, Vs, HardEdges).

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

build_and_fix_holes(Fs, N)
  when N < 10 -> %% Ensure that we don't loop forever
    case build_edges(Fs) of
	{Good,[]} ->
	    {Good,Fs};
	{_,Bad} ->
	    HF = fill_holes(Bad),
	    [_|_] = HF, %% Assert that we could fix something
	    build_and_fix_holes(HF++Fs, N+1)
    end.

build_rest(Es, Fs, Vs, HardEdges) ->
    Htab = vpairs_to_edges(HardEdges, Es),
    {Vct0,Etab,Ftab0,UvTab} = build_tables(Es),
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
    We = wings_va:set_edge_uvs(UvTab, We0),
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
    array:from_orddict(reverse(Acc)).

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
    build_tables(Edges, Emap, [], [], [], []).

build_tables([H|T], Emap, Vtab0, Etab0, Ftab0, UvTab0) ->
    {{Vs,Ve},{Edge,{Ldata,Rdata}}} = H,
    {Lf,LUV,Lpred,Lsucc} = Ldata,
    {Rf,RUV,Rpred,Rsucc} = Rdata,
    Erec = #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,
		 ltpr=edge_num(Lf, Lpred, Emap),
		 ltsu=edge_num(Lf, Lsucc, Emap),
		 rtpr=edge_num(Rf, Rpred, Emap),
		 rtsu=edge_num(Rf, Rsucc, Emap)},
    Etab = [{Edge,Erec}|Etab0],
    Ftab = [{Lf,Edge},{Rf,Edge}|Ftab0],
    Vtab = [{Vs,Edge},{Ve,Edge}|Vtab0],
    UvTab = case {LUV,RUV} of
		{none,none} -> UvTab0;
		{_,_} -> [{Edge,LUV,RUV}|UvTab0]
	    end,
    build_tables(T, Emap, Vtab, Etab, Ftab, UvTab);
build_tables([], _Emap, Vtab, Etab0, Ftab, UvTab) ->
    Etab = array:from_orddict(reverse(Etab0)),
    {Vtab,Etab,Ftab,UvTab}.

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
