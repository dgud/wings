%%
%%  wpc_doosabin.erl --
%%
%%     Plug-in for Doo-Sabin subdivision according to Wasamonkey.
%%
%%  Copyright (c) 2005-2011 Dan Gudmundsson, Wasamonkey
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_doosabin).

-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").

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
    wings_pb:start(?__(1,"Doo-Sabin")),
    Pass1 = fun(_, We) -> doo_sabin(We) end,
    St = wings_sel:map_update_sel(Pass1, edge, St0),
    wings_pb:done(),
    St;
command(_Cmd, _) -> next.

doo_sabin(#we{es=Etab0}=We0) ->
    OrigEdges0 = wings_util:array_keys(Etab0),
    OrigEdges = gb_sets:from_ordset(OrigEdges0),

    %% Set all edges hard and sub-divide twice.
    We1 = We0#we{he=OrigEdges},
    We2 = #we{es=Etab1} = wings_subdiv:smooth(We1),
    SubdEdges0 = wings_util:array_keys(Etab1),
    SubdEdges1 = gb_sets:from_ordset(SubdEdges0),
    We3 = wings_subdiv:smooth(We2),

    %% Find the original edge loops and dissolve them.
    ToDissolve1 = wings_edge_loop:select_loop(OrigEdges, We3),
    We4 = dissolve_edges(ToDissolve1, We3),

    %% Find the other edges to dissolve.
    Es = gb_sets:from_ordset(wings_util:array_keys(We4#we.es)),
    SubdEdges = gb_sets:intersection(SubdEdges1, Es),
    ToDissolve2 = wings_edge_loop:select_loop(SubdEdges, We4),

    %% Find the faces of those edges.
    Faces = wings_face:from_edges(ToDissolve2, We4),
    ScaleFaces0 = wings_sel:inverse_items(face, Faces, We4),

    %% Dissolve edges.
    #we{fs=Ftab} = We5 = wings_edge:dissolve_edges(ToDissolve2, We4),

    %% Fix the selection.
    AllFaces = gb_sets:from_ordset(gb_trees:keys(Ftab)),
    ScaleFaces = gb_sets:intersection(ScaleFaces0, AllFaces),
    CornerEdges = wings_edge:from_faces(ScaleFaces, We5),

    %% Return updated #we{} and selection.
    {We5,CornerEdges}.

%% Disolve edges and isolated vertices.
dissolve_edges(Es, We0) ->
    We1 = wings_edge:dissolve_edges(Es, We0),
    IsolatedVs = wings_vertex:isolated(We1),
    wings_edge:dissolve_isolated_vs(IsolatedVs, We1).
