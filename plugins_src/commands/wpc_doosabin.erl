%%
%%  wpc_doosabin.erl --
%%
%%     Plug-in for Doo-Sabin subdivision according to Wasamonkey.
%%
%%  Copyright (c) 2005-2009 Dan Gudmundsson, Wasamonkey
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_doosabin).

-export([init/0,menu/2,command/2]).

-include_lib("wings.hrl").

init() ->
    true.

menu({body},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([{_,smooth,_}=A|Rest], NewMenu, false) ->
    parse(Rest, [menu_entry(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found);
parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_entry()|NewMenu].

menu_entry() ->
    {?__(1,"Doo Sabin Subdivision"), doo_sabin,
      ?__(2,"Makes a Doo-Sabin subdivision according to WasaMonkey (WARNING: messes up all UV coordinates)")}.

command({body,doo_sabin}, St0) ->
    %% Do for each selected object
    St = wpa:sel_fold(fun(_,We,St) -> doo_sabin(We,St) end,
		      St0#st{sel=[],selmode=edge}, St0),
    wpa:sel_set(St#st.sel, St);
command(_Cmd, _) -> next.

doo_sabin(We0 = #we{es=Etab0, id=Id}, St0 = #st{shapes=Sh0,sel=OrigSel}) ->
    wings_pb:start(?__(1,"Doo-Sabin")),
    OrigEdges = wings_util:array_keys(Etab0),
    %% Set all edges hard. 
    We1 = We0#we{he=gb_sets:from_ordset(OrigEdges)},
    wings_pb:update(0.25, ?__(2,"Smoothing pass1")),
    We2 = #we{es=Etab1} = wings_subdiv:smooth(We1),
    SubdEdges = wings_util:array_keys(Etab1),
    wings_pb:update(0.70, ?__(3,"Smoothing pass 2")),
    We3 = wings_subdiv:smooth(We2),
    wings_pb:update(0.85, ?__(4,"Dissolving pass 1")),
    %% Set selection to the subd-edges and update the object we are subd'ing
    %%
    %% I do the dissolving (not like Wasamonkey) in two steps to be able to
    %% select the correct edges to return which can be scalade..
    
    %% First find the Original Edge loops
    St1 = St0#st{sel=[{Id,gb_sets:from_ordset(OrigEdges)}],
		 shapes=gb_trees:update(Id,We3,Sh0)},
    #st{sel=[{Id,DissolveEds1}]} = wings_edge_loop:select_loop(St1),
    %% Dissolve Edges..
    We4 = wings_edge:dissolve_edges(DissolveEds1, We3),
    %% Find and dissolve isolated vertices.
    IsolatedVs1 = wings_vertex:isolated(We4),
    We5 = wings_edge:dissolve_isolated_vs(IsolatedVs1, We4),
    %% Step 2
    %% Find the SubdEdges loops
    wings_pb:update(0.98, ?__(5,"Dissolving pass 2")),
    CurrEds = gb_sets:from_ordset(wings_util:array_keys(We5#we.es)),
    SubdEdges2 = gb_sets:intersection(gb_sets:from_ordset(SubdEdges),CurrEds),
     St2 = St0#st{sel=[{Id,SubdEdges2}],
		 shapes=gb_trees:update(Id,We5,Sh0)},
    #st{sel=[{Id,DissolveEds2}]} = wings_edge_loop:select_loop(St2),
    %% Find the faces of these edges.
    CurrFaces = wings_face:from_edges(DissolveEds2,We5),
    ScaleFaces = wings_sel:inverse_items(face, CurrFaces, We5),
    %% Dissolve Edges..
    We6 = wings_edge:dissolve_edges(DissolveEds2, We5),
    %% Find and dissolve isolated vertices.
    IsolatedVs2 = wings_vertex:isolated(We6),
    We7 =#we{fs=Ftab}= wings_edge:dissolve_isolated_vs(IsolatedVs2, We6),
    %% Fix the selection
    wings_pb:update(1.0, ?__(6,"Updating Selection")),
    Fs = gb_sets:from_ordset(gb_trees:keys(Ftab)),
    CornerEds = wings_edge:from_faces(gb_sets:intersection(ScaleFaces,Fs),We7),
    wings_pb:done(),
    %% Return the result
    St1#st{sel=[{Id,CornerEds}|OrigSel],
	   shapes=gb_trees:update(Id,We7,Sh0)}.
