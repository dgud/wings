%%
%%  wings_edge_loop.erl --
%%
%%     This module handles edge-loop commands.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_edge_loop).

%% Commands.
-export([select_next/1,select_prev/1,stoppable_sel_loop/1,select_loop/1,
	 select_link_decr/1,select_link_incr/1]).

%% Utilities.
-export([edge_loop_vertices/2,edge_links/2,partition_edges/2,
         select_loop/2]).

-include("wings.hrl").
-import(lists, [append/1,reverse/1,foldl/3,usort/1,member/2,any/2]).

%% select_next(St0) -> St.
%%  Implement the Select|Edge Loop|Next Edge Loop command.
%%
select_next(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, next);
select_next(St) -> St.

%% select_next(St0) -> St.
%%  Implement the Select|Edge Loop|Previous Edge Loop command.
%%
select_prev(#st{selmode=edge,sel=[_]}=St) ->
    find_loop(St, previous);
select_prev(St) -> St.

%% stoppable_sel_loop(St0) -> St.
%%  Implement the Select|Edge Loop|Edge Loop command.
%%
%%  If there are two paths that can connect two selected edges,
%%  only include the shorter path in the selection.
%%
stoppable_sel_loop(#st{selmode=edge}=St) ->
    wings_sel:update_sel(fun stoppable_select_loop/2, St);
stoppable_sel_loop(St) -> St.

%% select_loop(St0) -> St.
%%  Implement the Select|Edge Loop|To Complete Loops command.
%%
%%  For each selected edge, select as many loop edges as
%%  possible in both directions.
%%
select_loop(#st{selmode=edge}=St) ->
    wings_sel:update_sel(fun select_loop/2, St);
select_loop(St) -> St.

%% select_link_decr(St0) -> St.
%%  Implement the Select|Edge Loop|Shrink Edge Loop command.
%%
select_link_decr(#st{selmode=edge}=St) ->
    wings_sel:update_sel(fun select_link_decr/2, St);
select_link_decr(St) -> St.

%% select_link_incr(St0) -> St.
%%  Implement the Select|Edge Loop|Grow Edge Loop command.
%%
select_link_incr(#st{selmode=edge}=St) ->
    wings_sel:update_sel(fun select_link_incr/2, St);
select_link_incr(St) -> St.

%% edge_loop_vertices(EdgeSet, WingedEdge) -> [[Vertex]] | none
%%  Given a set of edges that is supposed to form
%%  one or more simple closed loops, this function returns
%%  the vertices that make up each loop in the correct order.
edge_loop_vertices(Edges, We) when is_list(Edges) ->
    edge_loop_vertices(gb_sets:from_list(Edges), We, []);
edge_loop_vertices(Edges, We) ->
    edge_loop_vertices(Edges, We, []).

%% edge_links(Edges, We0) -> [[{Edge,Vs,Ve}]]
%%   Return a list of edge links.
edge_links(Edges, We) when is_list(Edges) ->
    edge_links(gb_sets:from_list(Edges), We, []);
edge_links(Edges, We) ->
    edge_links(Edges, We, []).

%% partition_edges(EdgeSet, WingedEdge) -> [[EdgeSet']]
%%  Given a set of edges, partition the edges into connected groups.

partition_edges(Edges, We) when is_list(Edges) ->
    partition_edges(gb_sets:from_list(Edges), We, []);
partition_edges(Edges, We) ->
    partition_edges(Edges, We, []).

%% Create edge loops from the given edges.

-spec select_loop(EdgesIn, #we{}) -> EdgesOut when
      EdgesIn ::wings_sel:edge_set(),
      EdgesOut ::wings_sel:edge_set().

select_loop(Edges0, #we{es=Etab}=We) ->
    Edges1 = select_loop_1(Edges0, Etab, gb_sets:empty()),
    Edges2 = add_mirror_edges(Edges1, We),
    wings_we:visible_edges(Edges2, We).

%%%
%%% Local functions and data structures.
%%%

%% Border edges that need to be handled specially.
-record(be,
	{
	  mirror,				%Mirror edges.
	  hole					%Interesting hole edges.
	 }).

%%% find_loop/2 and helpers.

find_loop(#st{sel=PrevSel,edge_loop=PrevLoop0}=St0, Dir0) ->
    MF = fun(Edges0, #we{id=Id,es=Etab}=We) ->
                 G = digraph:new(),
                 build_digraph(G, gb_sets:to_list(Edges0), Edges0, Etab),
                 Cs0 = digraph_utils:components(G),
                 Cs1 = get_edges(G, Cs0),
                 Cs = [C || C <- Cs1, is_closed_loop(C, We)],
                 digraph:delete(G),
                 {Dir,PrevLoop} = prev_loop(Dir0, Id, PrevLoop0),
                 case pick_loop(Cs, Dir, PrevLoop) of
                     none -> gb_sets:empty();
                     Edges -> Edges
                 end
         end,
    St1 = St0#st{edge_loop={Dir0,PrevSel}},
    case wings_sel:update_sel(MF, St1) of
        #st{sel=[]}=St ->
            wings_sel:set(PrevSel, St);
        #st{}=St ->
            St
    end.

is_closed_loop(Edges, We) ->
    case edge_loop_vertices(Edges, We) of
	[_] -> true;
        _ -> false
    end.

get_edges(G, [C|Cs]) ->
    Es = gb_sets:from_list(append([digraph:edges(G, V) || V <- C])),
    [Es|get_edges(G, Cs)];
get_edges(_, []) -> [].

prev_loop(_, _, none) ->
    {none,none};
prev_loop(Dir, Id, {Dir,[{Id,L}]}) ->
    {away,L};
prev_loop(_, Id, {_,[{Id,L}]}) ->
    {towards,L};
prev_loop(_, _, _) ->
    {away,none}.

pick_loop([C|Cs], Dir, PrevLoop) ->
    IsPrev = PrevLoop =:= C,
    case Dir of
	away when IsPrev ->
	    pick_loop(Cs, Dir, PrevLoop);
	towards when not IsPrev ->
	    pick_loop(Cs, Dir, PrevLoop);
	_ ->
            C
    end;
pick_loop([], _, _) -> none.

build_digraph(G, [E|Es], Edges, Etab) ->
    #edge{ltpr=Lp,ltsu=Ls,rtpr=Rp,rtsu=Rs} = array:get(E, Etab),
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
		array:get(E, Etab),
	    follow_edge_1(G, Lp, Edges, Etab),
	    follow_edge_1(G, Ls, Edges, Etab),
	    follow_edge_1(G, Rp, Edges, Etab),
	    follow_edge_1(G, Rs, Edges, Etab)
    end.

follow_edge_1(G, E, Edges, Etab) ->
    case gb_sets:is_member(E, Edges) of
	true -> ok;
	false ->
	    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
	    add_edge(G, E, Va, Vb)
    end.

%%% Helpers for select_loop/1.

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
    #edge{vs=Va,ve=Vb} = Erec = array:get(Edge, Etab),
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
    case array:get(Edge, Etab) of
	#edge{vs=V,rf=Face,rtpr=From,ltsu=To} -> To;
	#edge{vs=V,lf=Face,ltsu=From,rtpr=To} -> To;
	#edge{ve=V,rf=Face,rtsu=From,ltpr=To} -> To;
	#edge{ve=V,lf=Face,ltpr=From,rtsu=To} -> To
    end.

%%% Helpers for select_link_decr/1.

select_link_decr(Edges0, #we{es=Etab}) ->
    EndPoints = append(component_endpoints(Edges0, Etab)),
    decrease_edge_link(EndPoints, Edges0).

decrease_edge_link([{_V,Edge}|R], Edges) ->
    decrease_edge_link(R, gb_sets:delete_any(Edge, Edges));
decrease_edge_link([], Edges) -> Edges.

%%% Helpers for stoppable_select_loop/1 and select_link_incr/1.

stoppable_select_loop(Edges0, We) ->
    Edges1 = loop_incr(Edges0, We),
    wings_we:visible_edges(Edges1, We).

loop_incr(Edges, #we{es=Etab}=We) ->
    %% Group the selected edges into connected components and for each
    %% components find its end points. Note that there can be more than
    %% two end points (for example if three edges are selected in a T
    %% pattern).
    EndPoints0 = component_endpoints(Edges, Etab),

    %% Flatten the list of components to a list with one element for
    %% each end point in all components. Each element will look like
    %% this:
    %%
    %%    {CompId,Vertex,Edge,EmptySelection}
    %%
    %% where CompId is an arbitrary term used for identifying the
    %% components (we use negative integers as they cannot be confused
    %% with vertex and edge numbers), and EmptySelection is an
    %% empty list.
    {_,EndPoints} =
	foldl(fun(Link0, {LinkNum,Acc0}) ->
		      Acc = [{LinkNum,V,Edge,[]} || {V,Edge} <- Link0] ++ Acc0,
		      {LinkNum-1,Acc}
	      end, {-1,[]}, EndPoints0),

    %% Construct a gb_tree mapping edge numbers to component
    %% identifiers.
    Edge2Link0 = [{Edge,LinkNum} || {LinkNum,_,Edge,_} <- EndPoints],
    Edge2Link = gb_trees:from_orddict(usort(Edge2Link0)),

    %% Get all border edges (for the virtual mirror face and for holes).
    BorderEdges = border_edges(Edges, We),

    %% Queue all end points and start working.
    Q = queue:from_list(EndPoints),
    loop_incr_1(Q, Edge2Link, BorderEdges, We, Edges, []).

%% The basic idea is to extend each end point by one edge at
%% the time. If the new end point meets an edge that was originally
%% selected, we take it out of the queue.
loop_incr_1(Q0, Edge2Link, BorderEdges, We, Sel0, Stuck0) ->
    case queue:out(Q0) of
	{empty,_} ->
	    %% Now we will add all "stuck" selections to
	    %% the selection.
	    gb_sets:union([Sel || {_,Sel} <- Stuck0] ++ [Sel0]);
	{{value,Item0},Q1} ->
	    case loop_incr_2(Item0, Edge2Link, BorderEdges, We) of
		{stop,Sel1} ->
		    %% We have hit one of the original edges in the
		    %% same component. We will update the selection,
		    %% but we will continue to extend the selection
		    %% for the other end points of this component.
		    Sel = gb_sets:union(Sel0, Sel1),
		    loop_incr_1(Q1, Edge2Link, BorderEdges, We, Sel, Stuck0);
		{stop,Sel1,KillLinks} ->
		    %% Stop because we have met an edge that was
		    %% part of the original selection but in another
		    %% component. In this case, we don't want to expand
		    %% the other end points in either component.
		    Sel = gb_sets:union(Sel0, Sel1),

		    %% Make sure that we don't collect any more
		    %% edges for the two components that met.
		    Q = queue:filter(fun({L,_,_,_}) ->
					     not member(L, KillLinks)
				     end, Q1),

		    %% Also, make sure that we discard any selection
		    %% resulting from getting stuck when expanding
		    %% the other end points.
		    Stuck = lists:filter(fun({L,_}) ->
						 not member(L, KillLinks)
					 end, Stuck0),
		    loop_incr_1(Q, Edge2Link, BorderEdges, We, Sel, Stuck);
		{stuck,Stuck1} ->
		    %% Stuck. Save this selection. We will only use it if none
		    %% of the other end points hit another component.
		    Stuck = [Stuck1|Stuck0],
		    loop_incr_1(Q1, Edge2Link, BorderEdges, We, Sel0, Stuck);
		{update,Item} ->
		    %% One more edge was added to the selection for the
		    %% link in this direction.
		    Q = queue:in(Item, Q1),
		    loop_incr_1(Q, Edge2Link, BorderEdges, We, Sel0, Stuck0)
	    end
    end.

loop_incr_2({CompId,V,Edge0,Sel}, Edge2Link, BorderEdges, #we{es=Etab}=We) ->
    case find_middle_edge(V, Edge0, BorderEdges, We) of
	Edge when is_integer(Edge) ->
	    case gb_trees:lookup(Edge, Edge2Link) of
		{value,CompId} ->
		    {stop,gb_sets:from_list(Sel)};
		{value,OtherCompId} ->
		    %% We have met an edge that was part of the original
		    %% selection, so we should stop here. Don't collect
		    %% any more edges for CompId or OtherCompId.
		    {stop,gb_sets:from_list(Sel),[CompId,OtherCompId]};
		none ->
		    %% Nothing to stop us to continue.
		    Rec = array:get(Edge, Etab),
		    OtherV = wings_vertex:other(V, Rec),
		    {update,{CompId,OtherV,Edge,[Edge|Sel]}}
	    end;
	none ->
	    %% Stuck. We don't know which edge to follow. Save this
	    %% selection for later.
	    {stuck,{CompId,gb_sets:from_list(Sel)}}
    end.

find_middle_edge(V, Edge0, #be{mirror=Mirror,hole=Holes}, We) ->
    Edges0 = wings_vertex:fold(fun(E, _, _, Acc) -> [E|Acc] end, [], V, We),

    %% Find out which border edges (virtual mirror or hole edges) to
    %% handle specially. We want to give hole edges higher priority
    %% than virtual mirror edges.
    %%
    %% To ensure termination of the algorithm, we must make sure that
    %% we follow the same path regardless of direction when we pass this
    %% vertex. That means that we must base the decision on all edges
    %% around vertex V. We could get stuck in an infinite loop if we
    %% would base the decision only on Edge0.
    Edges = case any(fun(E) -> gb_sets:is_member(E, Holes) end, Edges0) of
		true ->
		    %% At least one edge is a hole edge. Use only the
		    %% hole edges as border edges.
		    get_edges(Edges0, Edge0, Holes);
		false ->
		    %% None of the edges around this vertex is an hole edge.
		    %% Use the mirror edges as border edges.
		    get_edges(Edges0, Edge0, Mirror)
	    end,
    NumEdges = length(Edges),
    case NumEdges band 1 of
	0 ->
	    %% Pick the middle edge.
	    lists:nth(1+(NumEdges bsr 1), Edges);
	1 ->
	    none
    end.

get_edges(Edges0, Edge, BorderEdges) ->
    case get_edges_1(Edges0, BorderEdges, [], []) of
	{Edges,[]} ->
	    reorder(Edges, Edge, []);
	{Edges1,Edges2} ->
	    Edges = reverse(Edges2) ++ Edges2 ++ Edges1 ++ reverse(Edges1),
	    reorder(Edges, Edge, [])
    end.

get_edges_1([E|Es], BorderEdges, Acc, []) ->
    case gb_sets:is_member(E, BorderEdges) of
	false ->
	    get_edges_1(Es, BorderEdges, [E|Acc], []);
	true ->
	    get_edges_1(Es, BorderEdges, [], [E|Acc])
    end;
get_edges_1([E|Es], BorderEdges, Acc, Bacc) ->
    case gb_sets:is_member(E, BorderEdges) of
	false ->
	    get_edges_1(Es, BorderEdges, [E|Acc], Bacc);
	true ->
	    get_edges_1(Es, BorderEdges, reverse([E|Acc]), Bacc)
    end;
get_edges_1([], _, Acc, Bacc) -> {Acc,Bacc}.

reorder([Edge|R], Edge, Acc) ->
    [Edge|Acc ++ reverse(R)];
reorder([E|R], Edge, Acc) ->
    reorder(R, Edge, [E|Acc]).

select_link_incr(Edges0, #we{es=Etab}=We) ->
    EndPoints = append(component_endpoints(Edges0, Etab)),
    BorderEdges = border_edges(Edges0, We),
    Edges1 = expand_edge_link(EndPoints, We, BorderEdges, Edges0),
    wings_we:visible_edges(Edges1, We).

expand_edge_link([{V,OrigEdge}|R], We, BorderEdges, Sel0) ->
    case find_middle_edge(V, OrigEdge, BorderEdges, We) of
	Edge when is_integer(Edge) ->
	    Sel = gb_sets:add(Edge, Sel0),
	    expand_edge_link(R, We, BorderEdges, Sel);
	none ->
	    expand_edge_link(R, We, BorderEdges, Sel0)
    end;
expand_edge_link([], _, _, Sel) -> Sel.

%% component_endpoints(Edges, Etab) -> [[{Vertex,Edge}]]
%%  Group the selected edges into connected components and for each
%%  components find its end points. An end point is a vertex connected
%%  to only one edge within the component.
%%
%%  If a single edge is selected, the return value will look like:
%%
%%      [ [{Va,Edge},{Vb,Edge}] ]
%%
%%  If any number of edges are selected in a continous chain, the
%%  return value will look like:
%%
%%      [ [{Va,EdgeA},{Vb,EdgeB}] ]
%%
%%  If the three edges emanating from the same vertex on a cube,
%%  the return value will look like:
%%
%%      [ [{Va,EdgeA},{Vb,EdgeB},{Vc,EdgeC}] ]
%%
component_endpoints(Edges, Etab) ->
    G = digraph:new(),
    component_endpoints_1(G, gb_sets:to_list(Edges), Etab),
    Cs = digraph_utils:components(G),
    EndPoints = [find_end_vs(C, G, []) || C <- Cs],
    digraph:delete(G),
    EndPoints.

component_endpoints_1(G, [E|Es], Etab) ->
    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
    add_edge(G, E, Va, Vb),
    component_endpoints_1(G, Es, Etab);
component_endpoints_1(_, [], _) -> ok.

find_end_vs([V|R], G, Acc) ->
    New = digraph:in_edges(G, V) ++ digraph:out_edges(G, V),
    case New of
	[Edge] ->
	    find_end_vs(R, G ,[{V,Edge}|Acc]);
	_ ->
	    find_end_vs(R, G, Acc)
    end;
find_end_vs([], _G, Acc) -> Acc.

%%% Helpers for edge_loop_vertices/2.

edge_loop_vertices(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    #edge{vs=V,ve=Vend} = array:get(Edge, Etab),
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
			false -> A;
			true -> {Edge,wings_vertex:other(V, Rec)}
		    end
	    end, none, V, We),
    case Res of
	none -> none;
	{Edge,OtherV} -> 
	    Edges = gb_sets:delete(Edge, Edges0),
	    edge_loop_vertices1(Edges, OtherV, Vend, We, [V|Acc])
    end.

%%% Helpers for edge_links/2.

edge_links(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,Edges1} = gb_sets:take_smallest(Edges0),
	    #edge{vs=V,ve=Vend} = array:get(Edge, Etab),
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

%% Helpers for partition_edges/2.

partition_edges(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
	true -> Acc;
	false ->
	    {Edge,_} = gb_sets:take_smallest(Edges0),
	    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
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

%%% Common utilities.

add_edge(G, E, Va, Vb) ->
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, E, Va, Vb, []).

add_mirror_edges(Edges, We) ->
    MirrorEdges = gb_sets:from_list(mirror_edges(We)),
    case gb_sets:is_disjoint(Edges, MirrorEdges) of
	true -> Edges;
	false -> gb_sets:union(Edges, MirrorEdges)
    end.

%% border_edges(SelectedEdges, We) -> #be{}
%%  Collect border edges that are to be treated specially.
%%
%%  The virtual mirror face edges should always be treated
%%  specially, but we only want to include edges around holes
%%  that has at least one edge included in the initial selection.
%%
border_edges(Edges, #we{holes=Holes}=We) ->
    MirrorEdges = gb_sets:from_list(mirror_edges(We)),
    HoleEdges = hole_edges(Holes, Edges, We, gb_sets:empty()),
    #be{mirror=MirrorEdges,hole=HoleEdges}.

mirror_edges(#we{mirror=none}) -> [];
mirror_edges(#we{mirror=Face}=We) -> wings_face:to_edges([Face], We).

hole_edges([F|Fs], Edges, We, Acc) ->
    Bes = gb_sets:from_list(wings_face:to_edges([F], We)),
    case gb_sets:is_disjoint(Bes, Edges) of
	false ->
	    %% One of the edges around this hole is selected.
	    %% Include all of the edges for this hole in
	    %% the border edges.
	    hole_edges(Fs, Edges, We, gb_sets:union(Bes, Acc));
	true ->
	    hole_edges(Fs, Edges, We, Acc)
    end;
hole_edges([], _, _, Acc) -> Acc.
