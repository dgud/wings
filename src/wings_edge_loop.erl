%%
%%  wings_edge_loop.erl --
%%
%%     This module handles edge-loop commands.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_edge_loop.erl,v 1.21 2005/08/16 11:13:46 dgud Exp $
%%

-module(wings_edge_loop).
-export([select_next/1,select_prev/1,stoppable_sel_loop/1, select_loop/1, 
	 select_link_decr/1, select_link_incr/1]).

%% Utilities.
-export([edge_loop_vertices/2,edge_links/2,partition_edges/2]).

-include("wings.hrl").
-import(lists, [sort/1,append/1,reverse/1,foldl/3]).

%%%
%%% Select next/previous edge loop.
%%%

select_next(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, next);
select_next(St) -> St.

select_prev(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, previous);
select_prev(St) -> St.

find_loop(#st{sel=[{Id,Edges}=PrevSel],shapes=Shapes}=St, Dir0) ->
    We = gb_trees:get(Id, Shapes),
    #we{es=Etab} = We,
    G = digraph:new(),
    build_digraph(G, gb_sets:to_list(Edges), Edges, Etab),
    Cs0 = digraph_utils:components(G),
    Cs1 = get_edges(G, Cs0),
    Cs = [C || C <- Cs1, is_closed_loop(C, We)],
    digraph:delete(G),
    {Dir,PrevLoop} = prev_loop(Dir0, St),
    Sel = case pick_loop(Cs, Dir, PrevLoop, St) of
	      none ->
		  case pick_loop(Cs, Dir, PrevLoop, St) of
		      none -> PrevSel;
		      Sel0 -> Sel0
		  end;
	      Sel0 -> Sel0
	  end,
    St#st{sel=[Sel],edge_loop={Dir0,PrevSel}}.

is_closed_loop(Edges, We) ->
    case edge_loop_vertices(Edges, We) of
	[_] -> true;
        _ -> false
    end.

get_edges(G, [C|Cs]) ->
    Es = gb_sets:from_list(append([digraph:edges(G, V) || V <- C])),
    [Es|get_edges(G, Cs)];
get_edges(_, []) -> [].

prev_loop(_, #st{edge_loop=none}) -> {none,none};
prev_loop(Same, #st{sel=[{Id,_}],edge_loop={Same,{Id,L}}}) ->
    {away,L};
prev_loop(_, #st{sel=[{Id,_}],edge_loop={_,{Id,L}}}) ->
    {towards,L};
prev_loop(_, _) -> {away,none}.
    
pick_loop([C|Cs], Dir, PrevLoop, #st{sel=[{Id,_}]}=St) ->
    IsPrev = PrevLoop =:= C,
    if
	(Dir == away) and IsPrev ->
	    pick_loop(Cs, Dir, PrevLoop, St);
	(Dir == towards) and (not IsPrev) ->
	    pick_loop(Cs, Dir, PrevLoop, St);
	true -> {Id,C}
    end;
pick_loop([], _, _, #st{sel=[_]}) -> none.

build_digraph(G, [E|Es], Edges, Etab) ->
    #edge{ltpr=Lp,ltsu=Ls,rtpr=Rp,rtsu=Rs} = gb_trees:get(E, Etab),
    follow_edge(G, Ls, Edges, Etab),
    follow_edge(G, Rp, Edges, Etab),
    follow_edge(G, Lp, Edges, Etab),
    follow_edge(G, Rs, Edges, Etab),
    build_digraph(G, Es, Edges, Etab);
build_digraph(_, [], _, _) -> ok.

follow_edge(G, E, Edges, Etab) ->
    case gb_sets:is_member(E, Edges) of
	true -> ok;
	false ->
	    #edge{ltpr=Lp,ltsu=Ls,rtpr=Rp,rtsu=Rs} =
		gb_trees:get(E, Etab),
	    follow_edge_1(G, Lp, Edges, Etab),
	    follow_edge_1(G, Ls, Edges, Etab),
	    follow_edge_1(G, Rp, Edges, Etab),
	    follow_edge_1(G, Rs, Edges, Etab)
    end.

follow_edge_1(G, E, Edges, Etab) ->
    case gb_sets:is_member(E, Edges) of
	true -> ok;
	false ->
	    #edge{vs=Va,ve=Vb} = gb_trees:get(E, Etab),
	    add_edge(G, E, Va, Vb)
    end.

add_edge(G, E, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, E, Va, Vb, []).

%%%
%%% The Select Edge Loop command.
%%%

stoppable_sel_loop(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun stoppable_select_loop/3, [], St),
    wings_sel:set(Sel, St);
stoppable_sel_loop(St) -> St.

select_loop(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_loop/3, [], St),
    wings_sel:set(Sel, St);
select_loop(St) -> St.

select_loop(Edges0, #we{id=Id,es=Etab}=We, Acc) ->
    Edges1 = select_loop_1(Edges0, Etab, gb_sets:empty()),
    Edges2 = add_mirror_edges(Edges1, We),
    Edges = wings_we:visible_edges(Edges2, We),
    [{Id,Edges}|Acc].

select_loop_1(Edges0, Etab, Sel0) ->
    case gb_sets:is_empty(Edges0) of
	true -> Sel0;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    Sel = gb_sets:insert(Edge, Sel0),
	    Edges = select_loop_edges(Edge, Etab, Sel, Edges1),
	    select_loop_1(Edges, Etab, Sel)
    end.

select_loop_edges(Edge, Etab, Sel, Edges0) ->
    #edge{vs=Va,ve=Vb} = Erec = gb_trees:get(Edge, Etab),
    Edges = try_edge_from(Va, Edge, Erec, Etab, Sel, Edges0),
    try_edge_from(Vb, Edge, Erec, Etab, Sel, Edges).

try_edge_from(V, FromEdge, Erec, Etab, Sel, Edges) ->
    case try_edge_from_1(V, FromEdge, Erec, Etab) of
	none -> Edges;
	Edge ->
	    case gb_sets:is_member(Edge, Sel) of
		true -> Edges;
		false -> gb_sets:add(Edge, Edges)
	    end
    end.

try_edge_from_1(V, From, Erec, Etab) ->
    case Erec of
	#edge{vs=V,lf=FL,rf=FR,ltsu=EL,rtpr=ER} -> ok;
	#edge{ve=V,lf=FL,rf=FR,ltpr=EL,rtsu=ER} -> ok
    end,
    if
	EL =:= ER -> EL;
	true ->
	    case {next_edge(From, V, FL, EL, Etab),
		  next_edge(From, V, FR, ER, Etab)} of
		{Edge,Edge} -> Edge;
		{_,_} -> none
	    end
    end.

next_edge(From, V, Face, Edge, Etab) ->
    case gb_trees:get(Edge, Etab) of
	#edge{vs=V,rf=Face,rtpr=From,ltsu=To} -> To;
	#edge{vs=V,lf=Face,ltsu=From,rtpr=To} -> To;
	#edge{ve=V,rf=Face,rtsu=From,ltpr=To} -> To;
	#edge{ve=V,lf=Face,ltpr=From,rtsu=To} -> To
    end.

add_mirror_edges(Edges, We) ->
    MirrorEdges = gb_sets:from_list(mirror_edges(We)),
    case gb_sets:is_empty(gb_sets:intersection(Edges, MirrorEdges)) of
	true -> Edges;
	false -> gb_sets:union(Edges, MirrorEdges)
    end.

mirror_edges(#we{mirror=none}) -> [];
mirror_edges(#we{mirror=Face}=We) -> wings_face:to_edges([Face], We).

select_link_decr(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_link_decr/3, [], St),
    wings_sel:set(Sel, St);
select_link_decr(St) -> St.

select_link_decr(Edges0, #we{id=Id,es=Etab}, Acc) ->
    EndPoints = lists:append(init_expand(Edges0, Etab)),
    Edges = decrease_edge_link(EndPoints, Edges0),
    [{Id,Edges}|Acc].

decrease_edge_link([{_V,Edge}|R], Edges) ->
    decrease_edge_link(R, gb_sets:delete_any(Edge, Edges));
decrease_edge_link([], Edges) -> Edges.

stoppable_select_loop(Edges0, #we{id=Id}=We, Acc) ->
    Edges1 = loop_incr(Edges0, We),
    Edges = wings_we:visible_edges(Edges1, We),
    [{Id,Edges}|Acc].

loop_incr(Edges0, #we{es=Etab}=We) ->
    %% Setup everything
    EndPoints0 = init_expand(Edges0,Etab),
    {_,EndPoints} = foldl(fun(Link0, {No, Acc}) -> 
				  Link = [{V,Edge,[]}||{V,Edge}<-Link0],
				  {No+1, [{No+1,Link}|Acc]} 
			  end, 
			  {0,[]}, EndPoints0),
    %% Could be nicer
    Edges2Link0 = [[{Edge,LinkNo}|| {_,Edge,_} <- Link] || {LinkNo, Link} <- EndPoints],
    Edges2Link = gb_trees:from_orddict(lists:usort(lists:append(Edges2Link0))),
    MEds = gb_sets:from_list(mirror_edges(We)),
    loop_incr(EndPoints, [], Edges2Link, We, MEds, Edges0).

loop_incr([],[], _Stop, _We, _Meds, Selected) -> 
    Selected;
loop_incr([],Prev,Stop,We,Meds,Selected) -> 
    loop_incr(Prev,[],Stop,We,Meds,Selected);
loop_incr([{Id,This}|Rest],Prev, Stop, We, Meds, Selected) ->
    case expand_loop(This,Id,Stop,We,Meds) of
	{stop, Sel, Link} ->
	    loop_incr(lists:keydelete(Link,1,Rest),
		      lists:keydelete(Link,1,Prev),
		      Stop,We,Meds,gb_sets:union(Sel,Selected));
	{done,Sel} ->
	    loop_incr(Rest,Prev,Stop,We,Meds,gb_sets:union(Sel,Selected));
	{cont, New} ->
	    loop_incr(Rest,[{Id,New}|Prev],Stop,We,Meds,Selected)
    end.

expand_loop(Eds,Id,Stop,We,Meds) ->
    expand_loop(Eds,Id,Stop,We,Meds,done,[]).
expand_loop([Done={done,_}|R],Id,Stop,We,Meds,Res,Acc) ->
    expand_loop(R,Id,Stop,We,Meds,Res,[Done|Acc]);
expand_loop([This|R],Id,Stop,We,Meds,Res,Acc) ->
    case expand_loop2(This,Stop,We,Meds) of
	{stop, Sel, Id} ->
	    expand_loop(R,Id,Stop,We,Meds,Res,[{done,Sel}|Acc]);
	{stop, _, _} = Stopped ->
	    Stopped;
	{done,_} = Done ->
	    expand_loop(R,Id,Stop,We,Meds,Res,[Done|Acc]);
	{cont,Updated} ->
	    expand_loop(R,Id,Stop,We,Meds,cont,[Updated|Acc])
    end;
expand_loop([],_,_,_,_,done,Res) ->
    {done, foldl(fun({done,Sel},All) -> gb_sets:union(Sel,All) end,
		 gb_sets:empty(), Res)};
expand_loop([],_,_,_,_,cont,Res) -> {cont,Res}.

expand_loop2({V,OrigEdge,Sel},Stop,#we{es=Etab}=We,MirrorEdges) ->
    Eds = get_edges(V,OrigEdge,We,MirrorEdges),
    NumEdges = length(Eds),
    case NumEdges rem 2 of
	0 ->
	    Edge = lists:nth(1+(NumEdges div 2), Eds),
	    case gb_trees:lookup(Edge,Stop) of
		{value, Link} ->
		    {stop, gb_sets:from_list([OrigEdge|Sel]), Link};
		none ->	
%		    io:format("Adding Edge ~p to ~p ~n",[Edge,OrigEdge]),
		    Rec = gb_trees:get(Edge,Etab),
		    {cont,{wings_vertex:other(V,Rec),Edge,[OrigEdge|Sel]}}
	    end;
	1 -> 
	    {done, gb_sets:from_list([OrigEdge|Sel])}
    end.

get_edges(V,OrigEdge,We,MirrorEdges) ->
    {Eds0,Eds1} = wings_vertex:fold(fun(E,_,_,{Acc,false}) -> 
					    case gb_sets:is_member(E,MirrorEdges) of
						true -> {[],[E|Acc]};
						false ->{[E|Acc],false}
					    end;
				       (E,_,_,{Acc,Mirror}) -> 
					    case gb_sets:is_member(E,MirrorEdges) of
						true -> {reverse([E|Acc]),Mirror};
						false ->{[E|Acc],Mirror}
					    end
				    end,
				    {[],false}, V, We),
    Eds = if Eds1 == false -> Eds0;
	     true -> %% Add mirror edges
		  reverse(Eds1) ++ Eds1 ++ Eds0 ++ reverse(Eds0)
	  end,
    reorder(Eds, OrigEdge, []).

select_link_incr(#st{selmode=edge}=St) ->
    Sel = wings_sel:fold(fun select_link_incr/3, [], St),
    wings_sel:set(Sel, St);
select_link_incr(St) -> St.

select_link_incr(Edges0, #we{id=Id,es=Etab}=We, Acc) ->
    EndPoints = lists:append(init_expand(Edges0, Etab)),
    MirrorEdges = gb_sets:from_list(mirror_edges(We)),
    Edges1 = expand_edge_link(EndPoints, We, MirrorEdges, Edges0),
    Edges = wings_we:visible_edges(Edges1, We),
    [{Id,Edges}|Acc].

expand_edge_link([{V,OrigEdge}|R], We, MirrorEdges, Sel0) ->
    Eds = get_edges(V,OrigEdge,We,MirrorEdges),
    NumEdges = length(Eds),
    case NumEdges rem 2 of	
	0 ->
	    NewEd = lists:nth(1+(NumEdges div 2), Eds),
	    Sel = gb_sets:add(NewEd, Sel0),
	    expand_edge_link(R, We, MirrorEdges, Sel);
	1 ->
	    expand_edge_link(R, We, MirrorEdges, Sel0)
    end;
expand_edge_link([], _, _, Sel) -> Sel.

reorder([Edge|R], Edge, Acc) ->
    [Edge|Acc ++ reverse(R)];
reorder([E|R], Edge, Acc) ->
    reorder(R, Edge, [E|Acc]).

%% Returns start and end tuple {vertex, edge} for each link
init_expand(Edges, Etab) ->
    G = digraph:new(),
    init_expand(gb_sets:to_list(Edges), Etab, G), 
    Cs = digraph_utils:components(G),
    Expand = [find_end_vs(C, G, [])||C <- Cs],
    digraph:delete(G),
    Expand.

init_expand([Edge|R], Etab, G) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    add_edge(G, Edge, Va, Vb),
    init_expand(R, Etab, G);
init_expand([], _Etab, G) -> G.

find_end_vs([V|R], G, Acc) ->
    New = digraph:in_edges(G,V) ++ digraph:out_edges(G,V),
    case New of
	[Edge] ->
	    find_end_vs(R,G,[{V,Edge}|Acc]);
	_ ->
	    find_end_vs(R,G,Acc)
    end;
find_end_vs([], _G, Acc) -> Acc.
    

%% edge_loop_vertices(EdgeSet, WingedEdge) -> [[Vertex]] | none
%%  Given a set of edges that is supposed to form
%%  one or more simple closed loops, this function returns
%%  the vertices that make up each loop in the correct order.
edge_loop_vertices(Edges, We) when is_list(Edges) ->
    edge_loop_vertices(gb_sets:from_list(Edges), We, []);
edge_loop_vertices(Edges, We) ->
    edge_loop_vertices(Edges, We, []).

edge_loop_vertices(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    #edge{vs=V,ve=Vend} = gb_trees:get(Edge, Etab),
	    case edge_loop_vertices1(Edges1, V, Vend, We, [Vend]) of
		none -> none;
		{Vs,Edges} -> edge_loop_vertices(Edges, We, [Vs|Acc])
	    end
    end.

edge_loop_vertices1(Edges, Vend, Vend, _We, Acc) -> {Acc,Edges};
edge_loop_vertices1(Edges0, V, Vend, We, Acc) ->
    Res = wings_vertex:until(
	    fun(Edge, _, Rec, A) ->
		    case gb_sets:is_member(Edge, Edges0) of
			true -> {Edge,wings_vertex:other(V, Rec)};
			false -> A
		    end
	    end, none, V, We),
    case Res of
	none -> none;
	{Edge,OtherV} -> 
	    Edges = gb_sets:delete(Edge, Edges0),
	    edge_loop_vertices1(Edges, OtherV, Vend, We, [V|Acc])
    end.

%% edge_link, find links in edges set and returns [[{Edge,Vs,Ve}]] in
%% order.
edge_links(Edges, We) when is_list(Edges) ->
    edge_links(gb_sets:from_list(Edges), We, []);
edge_links(Edges, We) ->
    edge_links(Edges, We, []).

edge_links(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    #edge{vs=V,ve=Vend} = gb_trees:get(Edge, Etab),
	    case edge_link(Edges1, V, Vend, back, We, [{Edge,V,Vend}]) of
		{Vs,Edges} -> edge_links(Edges, We, [Vs|Acc]);
		{incomplete,Vs1,Edges2} ->
		    case edge_link(Edges2, Vend,V,front,We,reverse(Vs1)) of 
			{incomplete,Vs,Edges} ->
			    edge_links(Edges, We, [Vs|Acc]);
			{Vs,Edges} -> 
			    edge_links(Edges, We, [Vs|Acc])
		    end
	    end
    end.

edge_link(Edges, Vend, Vend, _, _We, Acc) -> {Acc,Edges};
edge_link(Edges0, V, Vend, Dir, We, Acc) ->
    Res = wings_vertex:until(
	    fun(Edge, _, Rec, A) ->
		    case gb_sets:is_member(Edge, Edges0) of
			true -> {Edge,wings_vertex:other(V, Rec)};
			false -> A
		    end
	    end, none, V, We),
    case Res of
	none -> {incomplete,Acc,Edges0};
	{Edge,OtherV} when Dir == back -> 
	    Edges = gb_sets:delete(Edge, Edges0),
	    edge_link(Edges,OtherV,Vend,Dir,We,[{Edge,OtherV,V}|Acc]);
	{Edge,OtherV} when Dir == front -> 
	    Edges = gb_sets:delete(Edge, Edges0),
	    edge_link(Edges,OtherV,Vend,Dir,We,[{Edge,V,OtherV}|Acc])
    end.

%% partition_edges(EdgeSet, WingedEdge) -> [[EdgeSet']]
%%  Given a set of edges, partition the edges into connected groups.

partition_edges(Edges, We) when is_list(Edges) ->
    partition_edges(gb_sets:from_list(Edges), We, []);
partition_edges(Edges, We) ->
    partition_edges(Edges, We, []).

partition_edges(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,_} = gb_sets:take_smallest(Edges0),
	    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
	    Ws = gb_sets:from_list([{Va,Edge},{Vb,Edge}]),
	    {Part,Edges} = partition_edges_1(Ws, We, Edges0, gb_sets:empty()),
	    partition_edges(Edges, We, [Part|Acc])
    end.

partition_edges_1(Ws0, We, Edges0, EdgeAcc0) ->
    case gb_sets:is_empty(Ws0) of
	true -> {gb_sets:to_list(EdgeAcc0),Edges0};
	false ->
	    {{V,Edge},Ws1} = gb_sets:take_smallest(Ws0),
	    EdgeAcc = gb_sets:add(Edge, EdgeAcc0),
	    Edges = gb_sets:delete_any(Edge, Edges0),
	    Ws = wings_vertex:fold(
		   fun(E, _, Rec, A) ->
			   case gb_sets:is_member(E, Edges) of
			       true ->
				   case gb_sets:is_member(E, EdgeAcc0) of
				       true -> A;
				       false ->
					   OtherV = wings_vertex:other(V, Rec),
					   gb_sets:add({OtherV,E}, A)
				   end;
			       false -> A
			   end
		   end, Ws1, V, We),
	    partition_edges_1(Ws, We, Edges, EdgeAcc)
    end.
