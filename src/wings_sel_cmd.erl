%%
%%  wings_sel_cmd.erl --
%%
%%     This module implements the commands in the selection menu.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_sel_cmd).

-export([menu/1,command/2]).

%% Utilities.
-export([select_all/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,keymember/3,keyfind/3,usort/1]).
-import(erlang, [max/2]).

menu(St) ->
    Help = ?__(99," (from selection or all visible objects (if no selection))"),
    RHelp = random_help(),
    [{?__(1,"Deselect"),deselect,?__(2,"Clear the selection")},
     separator,
     {?__(3,"More"),more,more_help(St)},
     {?__(4,"Less"),less,less_help(St)},
     {?__(5,"Similar"),similar,similar_help(St)}]
    ++ oriented_faces_menu(St) ++ similar_area_faces_menu(St)
    ++ similar_material_faces(St) ++
    [separator,
     {?__(6,"Edge Loop"),
      {edge_loop,
       [{?__(7,"Edge Loop"),
	 edge_loop,?__(8,"Expand edge selection to loop; ")++
	 ?__(9,"convert face selection to selected border edges")++
	 ?__(98,"; convert consecutive vertices to edges")},
	{?__(10,"Edge Loop to Region"),edge_loop_to_region,
	 ?__(11,"Select all faces on one side of an edge loop")},
	{?__(12,"Edge Ring"),
	 edge_ring,?__(13,"Expand edge selection to ring")},
	separator,
	{?__(14,"Previous Edge Loop"),
	 prev_edge_loop,?__(15,"Select the previous edge loop")},
	{?__(16,"Next Edge Loop"),
	 next_edge_loop,?__(17,"Select the next edge loop")},
	separator,
	{?__(18,"Grow Edge Loop"),edge_link_incr,
	 ?__(19,"Grow edge selection by one edge in loop directions")},
	{?__(20,"Shrink Edge Loop"),edge_link_decr,
	 ?__(21,"Shrink edge selection by one in loop direction")},
	{?__(22,"Grow Edge Ring"),edge_ring_incr,
	 ?__(23,"Grow edge selection by one edge in ring direction")},
	{?__(24,"Shrink Edge Ring"),edge_ring_decr,
	 ?__(25,"Shrink edge selection by one edge in ring directions")},
	separator,
	{?__(96,"To Complete Loops"),complete_loops,
	 ?__(97,"Switches to Edge mode and completes loop selection")}]}},
     separator,
     {?__(26,"Adjacent"),
      {adjacent,[{?__(27,"Vertices"),vertex},
		 {?__(28,"Edges"),edge},
		 {?__(29,"Faces"),face},
		 {?__(30,"Objects"),body}]}},
     {?__(31,"By"),
      {by,[{?__(32,"Hard Edges"),
	    hard_edges,?__(33,"Select all hard edges")++Help},
	   {?__(34,"Isolated Vertices"),
	    isolated_vertices,?__(35,"Select all isolated vertices")++Help},
	   {?__(85,"Non-planar Faces"),
	    nonplanar_faces,?__(86,"Select all non-planar faces")++Help,[option]},
	   {?__(36,"Vertices With"),
	    {vertices_with,
	     [{?__(37,"2 Edges"),2,Help},
	      {?__(38,"3 Edges"),3,Help},
	      {?__(39,"4 Edges"),4,Help},
	      {?__(40,"5 Edges"),5,Help},
	      {?__(401,"6 or More"),6,Help}]}},
	   {?__(41,"Faces With"),
	    {faces_with,
	     [{?__(42,"2 Edges"),2,Help},
	      {?__(43,"3 Edges"),3,Help},
	      {?__(44,"4 Edges"),4,Help},
	      {?__(45,"5 or More"),5,Help}]}},
	   {?__(nq0,"Non Quadrangle Faces"),
	    {non_quad,
	     [{?__(nq1,"All Non Quadrangle Faces"),all,Help},
	      {?__(nq2,"Odd Non Quadrangle Faces"),odd,Help},
	      {?__(nq3,"Even Non Quadrangle Faces"),even,Help}]}},
	   {?__(46,"Random"),
	    {random,[{"10%",10, RHelp},
		     {"20%",20, RHelp},
		     {"30%",30, RHelp},
		     {"40%",40, RHelp},
		     {"50%",50, RHelp},
		     {"60%",60, RHelp},
		     {"70%",70, RHelp},
		     {"80%",80, RHelp},
		     {"90%",90, RHelp},
		     {"__%",pick, RHelp}
		     ]}},
	   {?__(56,"Short Edges"),
	    short_edges,?__(57,"Select (too) short edges")++Help,[option]},
	   {?__(87,"Sharp Edges"),
	    sharp_edges,?__(88,"Select sharp edges")++Help,[option]},
	   {?__(95,"Vertex Path"),
	    {vertex_path,
	     [{?__(89,"Fewest Edges Path"),
	       fewest_edges_path,?__(90,"Select the path with the fewest edges between two vertices")},
	      {?__(91,"Shortest Path (Dijkstra)"),
	       dijkstra_shortest_path,?__(92,"Select the shortest path between two vertices (Dijkstra)")},
	      {?__(93,"Shortest Path (A-Star)"),
	       astar_shortest_path,?__(94,"Select the shortest path between two vertices (A-Star)")}]}},
	   {?__(58,"Material Edges"),material_edges,
	    ?__(59,"Select all edges between different materials")++Help},
	   {?__(60,"UV-Mapped Faces"),uv_mapped_faces,
	    ?__(61,"Select all edges that have UV coordinates")++Help},
	   {?__(62,"Id..."),id,?__(63,"Select by numeric id")}]}},
     {?__(64,"Lights"),lights,?__(65,"Select all lights")},
     separator,
     {sel_all_str(St),all,?__(66,"Select all elements")},
     separator,
     {?__(67,"Inverse"),inverse,?__(68,"Invert the selection")},
     separator,
     {?__(69,"Hide Selected"),
      hide_selected,
      ?__(70,"Hide all (partly or wholly) selected objects")},
     {?__(71,"Hide Unselected"),
      hide_unselected,?__(72,"Hide objects that have no selection")},
     {?__(73,"Lock Unselected"),
      lock_unselected,?__(74,"Lock objects that have no selection")},
     separator,
     {?__(75,"Show All"),
      show_all,?__(76,"Show all objects that have been hidden")},
     {?__(77,"Unlock All"),
      unlock_all,?__(78,"Unlock all locked objects")},
     separator,
     {?__(79,"Store Selection"),store_selection,
      ?__(80,"Store the selection into the selection group named \"StoredSelection\"")},
     {?__(81,"Recall Selection"),recall_selection,
      ?__(82,"Recall the selection from the selection group named \"StoredSelection\"")},
     separator,
     {?__(83,"New Group..."),new_group,?__(84,"Create a new selection group")}|groups_menu(St)].

random_help() ->
    ?__(1,"Select random elements from current selection, or all visible objects (no selection)").

sel_all_str(#st{selmode=vertex}) -> ?__(1,"All Vertices");
sel_all_str(#st{selmode=edge}) -> ?__(2,"All Edges");
sel_all_str(#st{selmode=face}) -> ?__(3,"All Faces");
sel_all_str(#st{selmode=body}) -> ?__(4,"All Objects").

oriented_faces_menu(#st{selmode=face}) ->
  [{?__(1,"Similar Normals"), oriented_faces,
    ?__(2,"Select faces with normals similar to those of the already selected faces"),[option]}];
oriented_faces_menu(_) ->
  [].

similar_area_faces_menu(#st{selmode=face}) ->
  [{?__(1,"Similar Area"), similar_area,
    ?__(2,"Select faces with areas similar to that of the already selected face"),[option]}];
similar_area_faces_menu(_) ->
  [].

similar_material_faces(#st{selmode=face}) ->
    [{?__(1,"Similar Material"),similar_material,
      ?__(2,"Select faces with a similar material to those already selected"),[option]}];
similar_material_faces(_) ->
    [].

groups_menu(#st{ssels=Ssels}=St) ->
    case gb_trees:is_empty(Ssels) of
	true -> [];
	false ->
	    [{?__(1,"Delete Group"),
	      {delete_group,
	       groups_and_help(?__(2,"Delete group \""), "\"", St)}},
	     separator,
	     {?__(4,"Add to Group"),
	      {add_to_group,
	       groups_and_help(?__(5,"Add current selection to group \""),"\"", St)}},
	     {?__(7,"Subtract from Group"),
	      {subtract_from_group,
	       groups_and_help(?__(8,"Subtract current selection from group \""),"\"", St)}},
	     separator,
	     {?__(10,"Select Group"),
	      {select_group,
	       groups_and_help(?__(11,"Select group \""), "\"", St)}},
	     separator,
	     {?__(13,"Union Group"),
	      {union_group,
	       groups_and_help(?__(14,"Union group \""),?__(15,"\" with current selection"), St)}},
	     {?__(16,"Subtract Group"),
	      {subtract_group,
	       groups_and_help(?__(17,"Subtract group \""),?__(18,"\" from current selection"), St)}},
	     {?__(19,"Intersect Group"),
	      {intersect_group,
	       groups_and_help(?__(20,"Intersect group \""),?__(21,"\" with current selection"), St)}}]
    end.

groups_and_help(Help0, Help1, #st{ssels=Ssels}) ->
    map(fun({Mode,Name}=Key) ->
		Title = group_title(Name, Mode),
		{Title,fun(help, _) -> {Help0++Name++Help1};
			  (_, Ns) -> wings_menu:build_command(Key, Ns)
		       end,Help0++Name++Help1}
	end,
	gb_trees:keys(Ssels)).

group_title(Name, vertex) -> ?__(1,"vertex: ")++Name;
group_title(Name, edge) -> ?__(2,"edge: ")++Name;
group_title(Name, face) -> ?__(3,"face: ")++Name;
group_title(Name, body) -> ?__(4,"body: ")++Name.

more_help(#st{selmode=vertex}) ->
    ?__(1,"Select all vertices adjacent to a selected vertex");
more_help(#st{selmode=edge}) ->
     ?__(2,"Select all edges adjacent to a selected edge");
more_help(#st{selmode=face}) ->
     ?__(3,"Select all faces sharing a vertex with a selected face");
more_help(_) -> "".

less_help(#st{selmode=vertex}) ->
     ?__(1,"Deselect all vertices adjacent to an unselected vertex");
less_help(#st{selmode=edge}) ->
    ?__(2,"Deselect all edges adjacent to an unselected edge");
less_help(#st{selmode=face}) ->
    ?__(3,"Deselect all faces sharing a vertex with an unselected face");
less_help(_) -> "".

similar_help(#st{selmode=vertex}) ->
    ?__(1,"Select vertices similar to the already selected vertices");
similar_help(#st{selmode=edge}) ->
    ?__(2,"Select edges similar to the already selected edges");
similar_help(#st{selmode=face}) ->
    ?__(3,"Select faces similar to the already selected faces");
similar_help(#st{selmode=body}) ->
    ?__(4,"Select objects with the same number of edges, faces, and vertices").

command({edge_loop,edge_loop}, #st{selmode=vertex}=St) ->
    {save_state,vs_to_edge_loop(St)};
command({edge_loop,edge_loop}, #st{selmode=face}=St) ->
    {save_state,face_region_to_edge_loop(St)};
command({edge_loop,edge_loop}, St) ->
    {save_state,wings_edge_loop:stoppable_sel_loop(St)};
command({edge_loop,edge_link_decr}, St) ->
    {save_state,wings_edge_loop:select_link_decr(St)};
command({edge_loop,edge_link_incr}, St) ->
    {save_state,wings_edge_loop:select_link_incr(St)};
command({edge_loop,edge_ring}, St) ->
    {save_state,wings_edge:select_edge_ring(St)};
command({edge_loop,edge_ring_incr}, St) ->
    {save_state,wings_edge:select_edge_ring_incr(St)};
command({edge_loop,edge_ring_decr}, St) ->
    {save_state,wings_edge:select_edge_ring_decr(St)};
command({edge_loop,next_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_next(St)};
command({edge_loop,prev_edge_loop}, St) ->
    {save_state,wings_edge_loop:select_prev(St)};
command({edge_loop,edge_loop_to_region}, St) ->
    {save_state,wings_edge:select_region(St)};
command({edge_loop,complete_loops}, St) ->
    {save_state,complete_loops(St)};
command(deselect, St) ->
    {save_state,deselect(St)};
command(more, St) ->
    wings_sel_conv:more(St);
command(less, St) ->
    wings_sel_conv:less(St);
command(all, St) ->
    {save_state,select_all(St)};
command(lights, St) ->
    {save_state,select_lights(St)};
command({by,Command}, St) ->
    by_command(Command, St);
command(similar, St) ->
    {save_state,similar(St)};
command({oriented_faces,Ask}, St) ->
    oriented_faces(Ask, St);
command({similar_area,Ask}, St) ->
    similar_area(Ask, St);
	
command({similar_material,Ask}, St) ->
    similar_material(Ask, St);
	
command({select_group,Id}, St) ->
    {save_state,select_group(Id, St)};
command({union_group, Id}, St) ->
    {save_state,union_group(Id, St)};
command({subtract_group, Id}, St) ->
    {save_state,subtract_group(Id, St)};
command({intersect_group, Id}, St) ->
    {save_state,intersect_group(Id, St)};
command({add_to_group, Id}, St) ->
    {save_state,add_to_group(Id, St)};
command({subtract_from_group, Id}, St) ->
    {save_state,subtract_from_group(Id, St)};
command({new_group_name, Name}, St) ->
    {save_state,new_group_name(Name, St)};
command(new_group, St) ->
    new_group(St);
command({delete_group,Id}, #st{ssels=Ssels}=St) ->
    {save_state,St#st{ssels=gb_trees:delete(Id, Ssels)}};
command(inverse, St) ->
    {save_state,inverse(St)};
command(hide_selected, St) ->
    {save_state,hide_selected(St)};
command(hide_unselected, St) ->
    {save_state,hide_unselected(St)};
command(lock_unselected, St) ->
    {save_state,lock_unselected(St)};
command(show_all, St) ->
    {save_state,wings_shape:show_all(St)};
command(unlock_all, St) ->
    {save_state,wings_shape:unlock_all(St)};
command({adjacent,Type}, St) ->
    set_select_mode(Type, St);
command(store_selection, #st{ssels=Ssels0,selmode=Mode,sel=Sel}=St) ->
    Key = {Mode,"StoredSelection"},
    Ssels = gb_trees:enter(Key, Sel, Ssels0),
    {save_state,St#st{ssels=Ssels}};
command(recall_selection, #st{selmode=Mode,ssels=Ssels}=St0) ->
    Key = {Mode, "StoredSelection"},
    case gb_trees:is_defined(Key, Ssels) of
	false -> St0;
	true ->
	    St = select_group(Key, St0),
	    {save_state,St}
    end;
command(Type, St) ->
    set_select_mode(Type, St).

by_command(hard_edges, St) ->
    hard_edges(St);
by_command(isolated_vertices, St) ->
    {save_state,select_isolated(St)};
by_command({nonplanar_faces,Ask}, St) ->
    nonplanar_faces(Ask, St);
by_command({vertices_with,N}, St) ->
    {save_state,vertices_with(N, St)};
by_command({non_quad,all}, St) ->
    {save_state,faces_with({non_quad,all}, St)};
by_command({non_quad,odd}, St) ->
    {save_state,faces_with({non_quad,odd}, St)};
by_command({non_quad,even}, St) ->
    {save_state,faces_with({non_quad,even}, St)};
by_command({faces_with,5}, St) ->
    {save_state,faces_with({faces_with,5}, St)};
by_command({faces_with,N}, St) ->
    {save_state,faces_with({faces_with,N}, St)};
by_command(material_edges, St) ->
    material_edges(St);
by_command({random, Percent}, St) ->
    random(Percent, St);
by_command({short_edges,Ask}, St) ->
    short_edges(Ask, St);
by_command({sharp_edges,Ask}, St) ->
    sharp_edges(Ask, St);
by_command({vertex_path,fewest_edges_path}, St) ->
    shortest_path(fewest_edges, St);
by_command({vertex_path,dijkstra_shortest_path}, St) ->
    shortest_path(dijkstra, St);
by_command({vertex_path,astar_shortest_path}, St) ->
    shortest_path(astar, St);
by_command(uv_mapped_faces, St) ->
    uv_mapped_faces(St);
by_command(id, St) ->
    by_id(St);
by_command({id,Sel}, St) ->
    {save_state,sel_by_id(Sel, St)}.

face_region_to_edge_loop(St) ->
    Sel = wings_sel:fold(
	    fun(Fs, #we{id=Id}=We, Acc) ->
		    Es0 = wings_face:outer_edges(Fs, We),
		    Es1 = subtract_mirror_edges(Es0, We),
		    Es = gb_sets:from_list(Es1),
		    [{Id,Es}|Acc]
	    end, [], St),
    wings_sel:set(edge, Sel, St).

subtract_mirror_edges(Es, #we{mirror=none}) -> Es;
subtract_mirror_edges(Es, #we{mirror=Face}=We) ->
    Es -- wings_face:to_edges([Face], We).

vs_to_edge_loop(St) ->
    Sel = wings_sel:fold(
      fun(Vs, #we{id=Id}=We, Acc) ->
        Es0 = vs_to_edges(Vs, We, []),
        Es1 = subtract_mirror_edges(Es0, We),
        Es = gb_sets:from_list(Es1),
        [{Id,Es}|Acc]
      end, [], St),
    wings_sel:set(edge, Sel, St).

vs_to_edges(Vs0, We, Es0) ->
    case gb_sets:is_empty(Vs0) of
      true -> lists:usort(Es0);
      false ->
        {Va,Vs} = gb_sets:take_smallest(Vs0),
        Es = wings_vertex:fold(
          fun(Edge, _, EdgeRec, Es1) ->
            Vb = wings_vertex:other(Va, EdgeRec),
            case gb_sets:is_element(Vb, Vs) of
              true -> [Edge|Es1];
              _ -> Es1
            end
          end, Es0, Va, We),
        vs_to_edges(Vs, We, Es)
    end.

%%%
%%% Selection commands.
%%%

set_select_mode(Type, St) ->
    {save_state,wings_sel_conv:mode(Type, St)}.

select_all(#st{selmode=body,shapes=Shapes}=St) ->
    Items = gb_sets:singleton(0),
    Sel = [{Id,Items} || #we{id=Id,perm=Perm} <- gb_trees:values(Shapes),
			 ?IS_SELECTABLE(Perm)],
    St#st{sel=Sel};
select_all(#st{selmode=Mode,sel=[],shapes=Shapes}=St) ->
    Sel = [{Id,wings_sel:get_all_items(Mode, We)} ||
	      #we{id=Id,perm=Perm}=We <- gb_trees:values(Shapes),
	      ?IS_SELECTABLE(Perm)],
    St#st{sel=Sel};
select_all(#st{selmode=Mode,sel=Sel0}=St) ->
    Sel = [{Id,wings_sel:get_all_items(Mode, Id, St)} || {Id,_} <- Sel0],
    St#st{sel=Sel}.

%%%
%%% Select Inverse.
%%%

inverse(#st{selmode=body,sel=Sel0,shapes=Shapes}=St) ->
    Items = gb_sets:singleton(0),
    All = [{Id,Items} || #we{id=Id,perm=Perm} <- gb_trees:values(Shapes),
			 ?IS_SELECTABLE(Perm)],
    Sel = ordsets:subtract(All, Sel0),
    St#st{sel=Sel};
inverse(#st{selmode=Mode}=St) ->
    Sel = wings_sel:fold(
	    fun(Items, #we{id=Id}=We, A) ->
		    Diff = wings_sel:inverse_items(Mode, Items, We),
		    case gb_sets:is_empty(Diff) of
			true -> [{Id,Items}|A]; %Can't inverse.
			false -> [{Id,Diff}|A]
		    end
	    end, [], St),
    St#st{sel=reverse(Sel)}.

%%%
%%% Deselect
%%%

deselect(St) ->
    case wings_pref:get_value(conditional_deselect) of
      true -> wings_sel:conditional_reset(St);
      false -> wings_sel:reset(St)
    end.

%%%
%%% Hide Selected
%%% Hide Unselected
%%% Lock Unselected
%%%

hide_selected(#st{selmode=Mode,shapes=Shs0,sel=Sel}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) ->
		       case keyfind(Id, 1, Sel) of
			   false -> {Id,We};
			   {_,Set} -> {Id,We#we{perm={Mode,Set}}}
		       end
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    St#st{shapes=Shs,sel=[]}.

hide_unselected(St) ->
    update_unsel([], St).

lock_unselected(St) ->
    update_unsel(1, St).

update_unsel(Perm, #st{shapes=Shs0,sel=Sel}=St) ->
    Shs1 = map(fun(#we{id=Id,perm=0}=We) ->
		       case keymember(Id, 1, Sel) of
			   true -> {Id,We};
			   false -> {Id,We#we{perm=Perm}}
		       end;
		  (#we{id=Id}=We) -> {Id,We}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    St#st{shapes=Shs}.

%%%
%%% Selection Groups
%%%

union_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
    Sel = union(Sel0, Ssel),
    St#st{sel=Sel}.

union(Sa, Sb) ->
    combine_sel(fun(Ss) -> gb_sets:union(Ss) end, Sa, Sb).

subtract_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
    Sel = subtract(Sel0, Ssel),
    St#st{sel=Sel}.

subtract([{Id1,_}=E1|Es1], [{Id2,_}|_]=Set2) when Id1 < Id2 ->
    [E1|subtract(Es1, Set2)];
subtract([{Id1,_}|_]=Set1, [{Id2,_}|Es2]) when Id1 > Id2 ->
    subtract(Set1, Es2);
subtract([{Id,E1}|Es1], [{Id,E2}|Es2]) ->	%E1 == E2
    E = gb_sets:subtract(E1, E2),
    case gb_sets:is_empty(E) of
	true -> subtract(Es1, Es2);
	false -> [{Id,E}|subtract(Es1, Es2)]
    end;
subtract([], _Es2) -> [];
subtract(Es1, []) -> Es1.

intersect_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
    Sel = intersection(Sel0, Ssel),
    St#st{sel=Sel}.

intersection(Sa, Sb) ->
    Empty = gb_sets:empty(),
    combine_sel(fun([_]) -> Empty;
		   (Ss) -> gb_sets:intersection(Ss)
		end, Sa, Sb).

combine_sel(Combine, Sa, Sb) ->
    combine_sel(Combine, lists:merge(Sa, Sb)).
combine_sel(Combine, [{Id,Sa},{Id,Sb}|T]) ->
    S = Combine([Sa,Sb]),
    case gb_sets:is_empty(S) of
	true -> combine_sel(Combine, T);
	false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(Combine, [{Id,S0}|T]) ->
    S = Combine([S0]),
    case gb_sets:is_empty(S) of
	true -> combine_sel(Combine, T);
	false -> [{Id,S}|combine_sel(Combine, T)]
    end;
combine_sel(_Combine, []) -> [].

coerce_ssel({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    case gb_trees:is_defined(Key,Ssels) of
	true ->
	    Ssel = gb_trees:get(Key, Ssels),
	    coerce_ssel(Mode, Ssel, St);
	false ->
	    []
    end.

coerce_ssel(Mode, Ssel, #st{selmode=Mode}) -> Ssel;
coerce_ssel(Smode, Ssel0, #st{selmode=Mode}=St) ->
    StTemp = St#st{selmode=Smode,sel=wings_sel:valid_sel(Ssel0, Smode, St)},
    #st{sel=Ssel} = wings_sel_conv:mode(Mode, StTemp),
    Ssel.

select_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel = gb_trees:get(Key, Ssels),
    ValidSel = wings_sel:valid_sel(Ssel, Mode, St),
    St#st{selmode=Mode,sel=ValidSel}.

add_to_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel0 = gb_trees:get(Key, Ssels),
    Ssel1 = wings_sel:valid_sel(Ssel0, Mode, St),
    #st{sel=Sel} = possibly_convert(Mode, St),
    Ssel = union(Ssel1, Sel),
    save_group(Key, Ssel, St).

subtract_from_group({Mode,_}=Key, #st{ssels=Ssels}=St) ->
    Ssel0 = gb_trees:get(Key, Ssels),
    Ssel1 = wings_sel:valid_sel(Ssel0, Mode, St),
    #st{sel=Sel} = possibly_convert(Mode, St),
    Ssel = subtract(Ssel1, Sel),
    save_group(Key, Ssel, St).

possibly_convert(Mode, #st{selmode=Mode}=St) -> St;
possibly_convert(Mode, St) -> wings_sel_conv:mode(Mode, St).

save_group(Key, Sel, #st{ssels=Ssels0}=St) ->
    Ssels = gb_trees:update(Key, Sel, Ssels0),
    St#st{ssels=Ssels}.

new_group(_) ->
    wings_ask:ask(?__(1,"Create New Group"),
		  [{?__(2,"Group Name"), ""}],
		  fun([String]) -> {select,{new_group_name,String}} end).

new_group_name(Name, #st{ssels=Ssels0,selmode=Mode,sel=Sel}=St) ->
    Key = {Mode,Name},
    case gb_trees:is_defined(Key, Ssels0) of
	false -> ok;
	true ->
	    %% Careful: don't use io_lib:format/2 here. The group name
	    %% may contain Unicode characters.
	    GroupMode = group_mode_string(Mode),
	    Exists = ?__(exists,"already exists."),
	    Msg0 = [GroupMode," \"",Name,"\" ",Exists],
	    Msg = lists:flatten(Msg0),
	    wings_u:error(Msg)
    end,
    Ssels = gb_trees:insert(Key, Sel, Ssels0),
    St#st{ssels=Ssels}.

group_mode_string(vertex) ->
    ?__(vertex, "Vertex selection group");
group_mode_string(edge) ->
    ?__(edge, "Edge selection group");
group_mode_string(face) ->
    ?__(face, "Face selection group");
group_mode_string(body) ->
    ?__(body, "Body selection group").

%%%
%%% Select Similar.
%%%

similar(#st{selmode=vertex}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_vertex_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = consolidate_templates(Seed),
    wings_sel:make(
      fun(V, W) ->
	      match_templates(make_vertex_template(V, W), Templates)
      end, vertex, St);
similar(#st{selmode=edge}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_edge_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = consolidate_templates(Seed),
    wings_sel:make(
      fun(Edge, W) ->
	      match_templates(make_edge_template(Edge, W), Templates)
      end, edge, St);
similar(#st{selmode=face}=St) ->
    Seed = wings_sel:fold(fun(Sel0, We, A) ->
				  [make_face_template(SelI, We) ||
				      SelI <- gb_sets:to_list(Sel0)] ++ A
			  end, [], St),
    Templates = consolidate_templates(Seed),
    wings_sel:make(
      fun(Face, WeI) ->
	      match_templates(make_face_template(Face, WeI), Templates)
      end, face, St);
similar(#st{selmode=body}=St) ->
    Template0 = wings_sel:fold(fun(_, #we{vp=Vtab,es=Etab,fs=Ftab}, Acc) ->
				       [{wings_util:array_entries(Vtab),
					 wings_util:array_entries(Etab),
					 gb_trees:size(Ftab)}|Acc]
			       end, [], St),
    Template = ordsets:from_list(Template0),
    wings_sel:make(fun(_, We) -> match_body(Template, We) end, body, St).

consolidate_templates(L) ->
    case usort(L) of
	[] -> [];
	[H|T] -> consolidate_templates_1(H, T)
    end.

consolidate_templates_1(Templ0, [Templ|T]) ->
    case match_template(Templ0, Templ) of
	true ->
	    consolidate_templates_1(Templ0, T);
	false->
	    [Templ0|consolidate_templates_1(Templ, T)]
    end;
consolidate_templates_1(Templ, []) -> [Templ].

match_body(Template, #we{vp=Vtab,es=Etab,fs=Ftab}) ->
    Sizes = {wings_util:array_entries(Vtab),
	     wings_util:array_entries(Etab),
	     gb_trees:size(Ftab)},
    match_body_1(Template, Sizes).

match_body_1([Sizes|_], Sizes) -> true;
match_body_1([_|T], Sizes) -> match_body_1(T, Sizes);
match_body_1([], _) -> false.

match_templates(F, [Template|Ts]) ->
    case match_template(F, Template) of
	true -> true;
	false -> match_templates(F, Ts)
    end;
match_templates(_, []) -> false.

match_template({Len,Ad,As}, {Len,Bd,Bs}) ->
    compare(Ad, Bd) andalso compare(As, Bs);
match_template(_, _) -> false.

make_face_template(Face, We) ->
    VsPos = wings_face:vertex_positions(Face, We),
    {DotSum,SqSum} = face_dots_and_sqlens(VsPos),
    {length(VsPos),DotSum,SqSum}.

face_dots_and_sqlens([Va,Vb|_]=Vpos) ->
    D = e3d_vec:sub(Va, Vb),
    face_dots_and_sqlens_2(D, Vpos, Vpos, 0, 0).

face_dots_and_sqlens_2(D1, [_|[Vb,Vc|_]=Vs], More, Dot0, Sq0) ->
    D2 = e3d_vec:sub(Vb, Vc),
    Dot = Dot0 + e3d_vec:dot(D1, D2),
    Sq = Sq0 + e3d_vec:dot(D1, D1),
    face_dots_and_sqlens_2(D2, Vs, More, Dot, Sq);
face_dots_and_sqlens_2(D1, Vs, [Va,Vb|_], Dot, Sq) ->
    face_dots_and_sqlens_2(D1, Vs++[Va,Vb], [], Dot, Sq);
face_dots_and_sqlens_2(_D1, _Other, _More, Dot, Sq) -> {Dot,Sq}.

make_edge_template(Edge, #we{vp=Vtab,es=Etab}=We) ->
    #edge{vs=Va,ve=Vb,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} =
	array:get(Edge, Etab),
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    Vec = e3d_vec:sub(VaPos, VbPos),
    DotSum = edge_dot(LP, Vb, VbPos, Vec, We) +
	edge_dot(RS, Vb, VbPos, Vec, We) +
	edge_dot(LS, Va, VaPos, Vec, We) +
	edge_dot(RP, Va, VaPos, Vec, We),
    {0,DotSum,e3d_vec:dot(Vec, Vec)}.

edge_dot(Edge, V, Pos, Vec, #we{es=Etab}=We) ->
    Rec = array:get(Edge, Etab),
    OtherPos = wings_vertex:other_pos(V, Rec, We),
    ThisVec = e3d_vec:sub(Pos, OtherPos),
    abs(e3d_vec:dot(ThisVec, Vec)).

make_vertex_template(V, #we{vp=Vtab}=We) ->
    Center = array:get(V, Vtab),
    Vecs = wings_vertex:fold(
	     fun(_, _, Rec, Acc0) ->
		     Pos = wings_vertex:other_pos(V, Rec, Vtab),
		     Vec = e3d_vec:sub(Pos, Center),
		     [Vec|Acc0]
	     end, [], V, We),
    {DotSum,SqSum} = vertex_dots_and_sqlens(Vecs, Vecs, 0, 0),
    {length(Vecs),DotSum,SqSum}.

vertex_dots_and_sqlens([VecA|[VecB|_]=T], More, Dot0, Sq0) ->
    Dot = Dot0 + abs(e3d_vec:dot(VecA, VecB)),
    Sq = Sq0 + e3d_vec:dot(VecA, VecA),
    vertex_dots_and_sqlens(T, More, Dot, Sq);
vertex_dots_and_sqlens(Vecs, [VecB|_], Dot, Sq) ->
    vertex_dots_and_sqlens(Vecs++[VecB], [], Dot, Sq);
vertex_dots_and_sqlens(_Other, _More, Dot, Sq) -> {Dot,Sq}.

-define(EPSILON, 1.0E-5).
compare(A, B) ->
    %% Comparison with a relative tolerance for large
    %% values and an absolute tolerance for small values
    %% as described by Christer Ericson in
    %% http://realtimecollisiondetection.net/blog/?p=89
    %% and in his book "Real-Time Collision Detection".
    %%
    abs(A-B) =< ?EPSILON*max(1.0, max(abs(A), abs(B))).

%%
%% Select Random.
%%

random(pick, _) ->
    Qs = [{hframe,[{slider,{text,wings_pref:get_value(random_select, 25.0),
            [{range,{0.0,100.0}}]}}]}],
    wings_ask:dialog(true, ?__(1,"Select Random"),
    [{vframe,Qs}],
    fun([Res]) ->
      wings_pref:set_value(random_select, Res),
      {select,{by,{random,Res}}} end);
random(Percent, #st{selmode=Mode, sel=[]}=St) ->
    P = Percent / 100,
    {save_state, wings_sel:make(fun(_, _) -> random:uniform() < P end, Mode, St)};
random(Percent, St) ->
    P = Percent / 100,
    NewSel = wings_sel:fold(fun(Sel0, #we{id=Id}, Acc) ->
            Sel1 = gb_sets:to_list(Sel0),
            Sel2 = [Elem || Elem <- Sel1, random:uniform() < P ],
            case Sel2 of
              [] -> Acc;
              _ -> [{Id,gb_sets:from_list(Sel2)}|Acc]
            end
        end,[],St),
    {save_state, wings_sel:set(NewSel, St)}.

%%
%% Select short edges.
%%

short_edges(Ask, _St) when is_atom(Ask) ->
    Qs = [{label,?__(1,"Length tolerance")},
	  {text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
    wings_ask:dialog(Ask, ?__(2,"Select Short Edges"),
		     [{hframe,Qs}],
		     fun(Res) -> {select,{by,{short_edges,Res}}} end);
short_edges([Tolerance], #st{sel=[]}=St0) ->
    St = wings_sel:make(fun(Edge, We) ->
				short_edge(Tolerance, Edge, We)
			end, edge, St0),
    {save_state,St#st{selmode=edge}};
short_edges([Tolerance], #st{selmode=Mode}=St0) ->
    St = if Mode =:= edge -> St0; true -> wings_sel_conv:mode(edge, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, Acc) ->
				Sel1 = gb_sets:to_list(Sel0),
				ShortEdges = [Edge || Edge <- Sel1, short_edge(Tolerance, Edge, We)],
				case ShortEdges of
				  [] -> Acc;
				  _ -> [{Id,gb_sets:from_list(ShortEdges)}|Acc]
				end
			end, [], St),
    {save_state,wings_sel:set(edge,Sel,St0)}.

short_edge(Tolerance, Edge, #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    abs(e3d_vec:dist(VaPos, VbPos)) < Tolerance.

%%
%% Select all edges between materials.
%%

material_edges(#st{sel=[]}=St) ->
    wings_sel:make(fun material_edges_fun/2, edge, St);
material_edges(#st{selmode=Mode}=St0) ->
    St = if Mode =:= edge -> St0; true -> wings_sel_conv:mode(edge, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We,Acc) ->
		  Sel1 = gb_sets:to_list(Sel0),
		  MatEdges = [Edge || Edge <- Sel1, material_edges_fun(Edge, We)],
		  case MatEdges of
		    [] -> Acc;
		    _ -> [{Id, gb_sets:from_list(MatEdges)}|Acc]
		  end
	  end,[],St),
    wings_sel:set(edge, Sel, St0).

material_edges_fun(E, #we{es=Etab}=We) ->
    #edge{lf=Lf,rf=Rf} = array:get(E, Etab),
    wings_facemat:face(Lf, We) =/= wings_facemat:face(Rf, We).

%%
%% Select all faces that have (proper) UV coordinates.
%%

uv_mapped_faces(#st{shapes=Shs,sel=[]}=St) ->
    Sel = foldl(fun(#we{id=Id}=We, A) ->
			case wings_we:uv_mapped_faces(We) of
			    [] -> A;
			    Fs -> [{Id,gb_sets:from_ordset(Fs)}|A]
			end
		end, [], gb_trees:values(Shs)),
    wings_sel:set(face, Sel, St);
uv_mapped_faces(#st{selmode=Mode}=St0) ->
    St = if Mode =:= face -> St0; true -> wings_sel_conv:mode(face, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, Acc) ->
				Sel1 = gb_sets:to_list(Sel0),
				UVF = wings_we:uv_mapped_faces(We),
				Faces = [Face || Face <- Sel1, lists:member(Face, UVF)],
				case Faces of
				  [] -> Acc;
				  _ -> [{Id,gb_sets:from_list(Faces)}|Acc]
				end
			end, [], St),
	wings_sel:set(face,Sel,St0).


%%
%% Select by numerical item id.
%%

by_id(#st{selmode=body}) ->
    ask([{"Object Id",1}],
	fun([Id]) ->
		{"",[{Id,gb_sets:singleton(0)}]}
	end);
by_id(#st{selmode=vertex}=St) ->
    item_by_id("Vertex Id", St);
by_id(#st{selmode=edge}=St) ->
    item_by_id("Edge Id", St);
by_id(#st{selmode=face}=St) ->
    item_by_id("Face Id", St).

item_by_id(Prompt, #st{sel=[{Id,_}]}) ->
    ask([{Prompt,0}],
	fun([Item]) ->
		{Prompt,[{Id,gb_sets:singleton(Item)}]}
	end);
item_by_id(Prompt, #st{shapes=Shs}) ->
    case gb_trees:to_list(Shs) of
	[] -> wings_u:error(?__(1,"Nothing to select."));
	[{Id,_}] ->
	    ask([{Prompt,0}],
		fun([Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end);
	[{Id0,_}|_] ->
	    ask([{?__(2,"Object Id"),Id0},
		 {Prompt,0}],
		fun([Id,Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end)
    end.

sel_by_id({Prompt,Sel}, St) ->
    wings_sel:set(valid_sel(Prompt, Sel, St), St).

valid_sel(Prompt, Sel, #st{shapes=Shs,selmode=Mode}=St) ->
    case wings_sel:valid_sel(Sel, Mode, St) of
	[] ->
	    [{Id,Item0}] = Sel,
	    [Item] = gb_sets:to_list(Item0),
	    case gb_trees:is_defined(Id, Shs) of
		false ->
		    wings_u:error(?__(1,"The Object Id ")++
				  integer_to_list(Id)++
				  ?__(2," is invalid."));
		true ->
		    wings_u:error(?__(3,"The ")++Prompt++" "++
				  integer_to_list(Item)++
				  ?__(4," is invalid."))
	    end;
	Sel -> Sel
    end.

ask(Qs, Fun) ->
    wings_ask:ask(?__(1,"Select By Id"), Qs,
		  fun(Res) ->
			  Sel = Fun(Res),
			  {select,{by,{id,Sel}}}
		  end).

%%%
%%% Select lights.
%%%

select_lights(#st{selmode=Mode,shapes=Shapes}=St) ->
    Sel = select_lights_1(gb_trees:values(Shapes), Mode),
    St#st{selmode=Mode,sel=Sel}.

select_lights_1([#we{perm=Perm}|Shs], Mode) when ?IS_NOT_SELECTABLE(Perm) ->
    select_lights_1(Shs, Mode);
select_lights_1([We|Shs], Mode) when not ?IS_LIGHT(We) ->
    select_lights_1(Shs, Mode);
select_lights_1([#we{id=Id}|Shs], body) ->
    [{Id,gb_sets:singleton(0)}|select_lights_1(Shs, body)];
select_lights_1([#we{id=Id,vp=Vtab,es=Etab,fs=Ftab}|Shs], Mode) ->
    Sel = case Mode of
	      vertex -> wings_util:array_keys(Vtab);
	      edge -> wings_util:array_keys(Etab);
	      face -> gb_trees:keys(Ftab)
	  end,
    [{Id,gb_sets:from_ordset(Sel)}|select_lights_1(Shs, Mode)];
select_lights_1([], _) -> [].

%%%
%%% Select isolated vertices.
%%%

select_isolated(#st{shapes=Shs, sel=[]}=St) ->
    Sel = foldl(fun(#we{perm=Perm}=We, A) when ?IS_SELECTABLE(Perm) ->
			select_isolated_1(We, A);
		   (_, A) -> A
		end, [], gb_trees:values(Shs)),
    wings_sel:set(vertex, Sel, St);
select_isolated(#st{selmode=Mode}=St0) ->
    St = if Mode =:= vertex -> St0; true -> wings_sel_conv:mode(vertex, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, A) ->
			Isolated0 = gb_sets:from_list(wings_vertex:isolated(We)),
			Isolated = gb_sets:intersection(Sel0, Isolated0),
			case gb_sets:is_empty(Isolated) of
			  true -> A;
			  false -> [{Id,Isolated}|A]
			end
		end, [], St),
    wings_sel:set(vertex, Sel, St).

select_isolated_1(#we{id=Id}=We, A) ->
    Isolated = gb_sets:from_list(wings_vertex:isolated(We)),
    case gb_sets:is_empty(Isolated) of
	true -> A;
	false -> [{Id,Isolated}|A]
    end.

%%%
%%% Select nonplanar faces
%%%

nonplanar_faces(Ask, _St) when is_atom(Ask) ->
    Qs = [{label,?__(1,"Distance tolerance")},
	  {text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
    wings_ask:dialog(Ask, ?__(2,"Select Non-planar Faces"),
		     [{hframe,Qs}],
		     fun(Res) -> {select,{by,{nonplanar_faces,Res}}} end);
nonplanar_faces([Tolerance], #st{sel=[]}=St) ->
    Sel = fun(Face, We) ->
		  not wings_face:is_planar(Tolerance,Face,We)
	  end,
    {save_state,wings_sel:make(Sel, face, St)};
nonplanar_faces([Tolerance], #st{selmode=Mode}=St0) ->
    St = if Mode =:= face -> St0; true -> wings_sel_conv:mode(face, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, Acc) ->
		  Sel1 = gb_sets:to_list(Sel0),
		  NPFaces = [Face || Face <- Sel1, not wings_face:is_planar(Tolerance,Face,We)],
		  case NPFaces of
		    [] -> Acc;
		    _ -> [{Id,gb_sets:from_list(NPFaces)}|Acc]
		  end
	  end,[],St),
    {save_state,wings_sel:set(face, Sel, St0)}.


%%%
%%% Select similarly oriented faces.
%%%

oriented_faces(_, #st{selmode=Mode}) when Mode =/= face ->
    keep;					%Wrong mode (invoked through hotkey).

oriented_faces(Ask, _St) when is_atom(Ask) ->
    Connected = wings_pref:get_value(similar_normals_connected,false),
    {Save,Angle} = case wings_pref:get_value(similar_normals_angle,{false,1.0E-3}) of
        {true,A} -> {true,A};
        {false,_} -> {false,1.0E-3}
    end,
    Qs = [{vframe,
           [{hframe,[{slider,{text,Angle,[{range,{1.0E-3,180.0}}]}}]}],
           [{title,?__(1,"Angle Tolerance")}]},
          {?__(3,"Connected Faces Only"),Connected},
          {?__(5,"Save Angle"),Save}],
    wings_ask:dialog(Ask, ?__(2,"Select Similarly Oriented Faces"),
    [{vframe,Qs}],
    fun(Res) -> {select,{oriented_faces,Res}} end);

oriented_faces([Tolerance,Connected,Save], #st{selmode=face, sel=[]}) ->
    wings_pref:set_value(similar_normals_connected,Connected),
    wings_pref:set_value(similar_normals_angle,{Save,Tolerance}),
    wings_u:error(?__(4,"At least one face must be selected"));

oriented_faces([Tolerance,false,Save], St) ->
    wings_pref:set_value(similar_normals_connected,false),
    wings_pref:set_value(similar_normals_angle,{Save,Tolerance}),
    CosTolerance = math:cos(Tolerance * (math:pi() / 180.0)),
    Normals0 = wings_sel:fold(fun(Faces, We, A) ->
                [wings_face:normal(F, We) ||
                    F <- gb_sets:to_list(Faces)] ++ A
              end, [], St),
	Normals = lists:usort(Normals0),
    Sel = fun(Face, We) ->
          Normal = wings_face:normal(Face,We),
          any_matching_normal(CosTolerance, Normal, Normals)
      end,
    {save_state,wings_sel:make(Sel, face, St)};

oriented_faces([Tolerance,true,Save], St0) ->
    wings_pref:set_value(similar_normals_angle, {Save,Tolerance}),
    wings_pref:set_value(similar_normals_connected, true),
    CosTolerance = math:cos(Tolerance * (math:pi() / 180.0)),
    Sel = wings_sel:fold(
	    fun(Faces, #we{id=Id}=We, A) ->
		    Normals0 = gb_sets:fold(
				 fun(F,Acc) ->
					 [wings_face:normal(F, We)|Acc]
				 end, [], Faces),
		    Normals = lists:usort(Normals0),
		    [{Id,norm_search(Faces, Normals, CosTolerance, We, Faces)}|A]
	    end, [], St0),
    wings_sel:set(face, Sel, St0).

norm_search(Faces,Normals,CosTolerance,We,LastSel) ->
    Fs0 = wings_face:extend_border(LastSel, We),
    Fs1 = gb_sets:subtract(Fs0,Faces),
    AddSel = check_face_normals(Fs1,Normals,CosTolerance,We,gb_sets:empty()),
    case gb_sets:is_empty(AddSel) of
        true -> Faces;
        false ->
            Faces1 = gb_sets:union(AddSel,Faces),
            norm_search(Faces1,Normals,CosTolerance,We,AddSel)
    end.

check_face_normals(Fs0,Normals,CosTolerance,We,Selection)->
    case gb_sets:is_empty(Fs0) of
      true -> Selection;
      false ->
        {F,Fs1} = gb_sets:take_smallest(Fs0),
        Normal = wings_face:normal(F, We),
        Sel = case any_matching_normal(CosTolerance, Normal, Normals) of
          true -> gb_sets:add(F,Selection);
          false -> Selection
        end,
        check_face_normals(Fs1,Normals,CosTolerance,We,Sel)
    end.

any_matching_normal(_,_,[]) ->
    false;
any_matching_normal(CosTolerance, Norm, [N|T]) ->
    Dot = e3d_vec:dot(N, Norm),
    if
      Dot >= CosTolerance -> true;
      true ->  any_matching_normal(CosTolerance, Norm, T)
    end.

%%%
%%% Select faces of the same material.
%%%
similar_material(_, #st{selmode=Mode}) when Mode =/= face ->
    keep;					%Wrong mode (invoked through hotkey).

similar_material(Ask, _St) when is_atom(Ask) ->
    Connected = wings_pref:get_value(similar_materials_connected,false),
    Qs = [{?__(2,"Connected Faces Only"),Connected},
	  {vradio,[{?__(5,"Materials"),material},
		   {?__(6,"Vertex Color"),vertex_color}],
	   material}],
    wings_ask:dialog(Ask, ?__(4,"Select Faces with the same Material"),
    [{vframe,Qs}],
    fun(Res) -> {select,{similar_material,Res}} end);

similar_material([Connected|_], #st{selmode=face,sel=[]}) ->
    wings_pref:set_value(similar_materials_connected,Connected),
    wings_u:error(?__(3,"At least one face must be selected"));

similar_material([false,Mode], St) ->
    Materials = wings_sel:fold(fun
       (Faces, We, A) when Mode =:= vertex_color ->
        gb_sets:fold(fun(F, Acc) ->
                   [average_colors(F, We)|Acc]
        end,A,Faces);
       (Faces, We, A) ->
               [wings_facemat:face(SelI, We) ||
                   SelI <- gb_sets:to_list(Faces)] ++ A
             end, [], St),
    Sel = fun	
        (F, We) when Mode =:= vertex_color ->
          Col = average_colors(F, We),
          any_matching_material(Col,Materials);
        (Face, We) ->
          Mat = wings_facemat:face(Face,We),
          any_matching_material(Mat, Materials)
      end,
    wings_pref:set_value(similar_materials_connected,false),
    {save_state,wings_sel:make(Sel, face, St)};

similar_material([true,Mode], St0) ->
    Selection = wings_sel:fold(fun
        (Faces, #we{id=Id}=We, A) when Mode =:= vertex_color ->
            AllCols = gb_sets:fold(fun(F,Acc) ->
                [average_colors(F, We)|Acc]
            end,A,Faces),
            Colours = lists:usort(AllCols),
            [{Id,mat_search(Faces,Colours,We,Mode,Faces)}|A];
        (Faces, #we{id=Id}=We, A) ->
            AllMats = [wings_facemat:face(Face, We) || Face <- gb_sets:to_list(Faces)],
            Materials = lists:usort(AllMats),
            [{Id,mat_search(Faces,Materials,We,Mode,Faces)}|A]
    end, [], St0),
    wings_sel:set(face,Selection,St0).

average_colors(Face, We) ->
    wings_color:average([C || C <- wings_va:face_attr(color, Face, We)]).

mat_search(Faces,Colours,We,Mode,LastSel) ->
    Fs0 = wings_face:extend_border(LastSel, We),
    Fs1 = gb_sets:subtract(Fs0,Faces),
    AddSel = check_face_colours(Fs1,Colours,We,Mode,gb_sets:empty()),
    case gb_sets:is_empty(AddSel) of
        true -> Faces;
        false ->
            Faces1 = gb_sets:union(AddSel,Faces),
            mat_search(Faces1,Colours,We,Mode,AddSel)
    end.

check_face_colours(Fs0,Colours,We,Mode,Selection)->
    case gb_sets:is_empty(Fs0) of
      true -> Selection;
      false ->
        {F,Fs1} = gb_sets:take_smallest(Fs0),
        Colour = face_info(F,We,Mode),
        Sel = case any_matching_material(Colour,Colours) of
          true -> gb_sets:add(F,Selection);
          false -> Selection
        end,
        check_face_colours(Fs1,Colours,We,Mode,Sel)
    end.

face_info(F,We,vertex_color) ->
    average_colors(F, We);
face_info(F,We,_Mode) ->
    wings_facemat:face(F, We).

any_matching_material(_,[]) ->
    false;
any_matching_material(Material, [Mat|T]) ->
    if
      Material == Mat -> true;
      true -> any_matching_material(Material, T)
    end.

%%%
%%% Select sharp edges
%%%

sharp_edges(Ask, _St) when is_atom(Ask) ->
    Qs = [{label,?__(1,"Max Angle")},
	  {text,120.0,[{range,{0.0,180.0}}]}],
    wings_ask:dialog(Ask,
	?__(2,"Select Sharp Edges"), [{hframe,Qs}],
	fun(Res) ->
	    {select,{by,{sharp_edges,Res}}}
	end);
sharp_edges([Tolerance], #st{sel=[]}=St0) ->
    CosTolerance = -math:cos(Tolerance * math:pi() / 180.0),
    St = wings_sel:make(fun(Edge, We) ->
	     sharp_edge(CosTolerance, Edge, We)
	 end, edge, St0),
    {save_state,St};
sharp_edges([Tolerance], #st{selmode=Mode}=St0) ->
    St = if Mode =:= edge -> St0; true -> wings_sel_conv:mode(edge, St0) end,
    CosTolerance = -math:cos(Tolerance * math:pi() / 180.0),
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, Acc) ->
		  Sel1 = gb_sets:to_list(Sel0),
		  SharpEdges = [Edge || Edge <- Sel1, sharp_edge(CosTolerance, Edge, We)],
		  case SharpEdges of
		    [] -> Acc;
		    _ -> [{Id, gb_sets:from_list(SharpEdges)}|Acc]
		  end
	  end,[],St),
    {save_state,wings_sel:set(edge, Sel, St0)}.


sharp_edge(CosTolerance, Edge, #we{es=Etab}=We) ->
    #edge{lf=Lf,rf=Rf} = array:get(Edge, Etab),
    Lfn = wings_face:normal(Lf, Edge, We),
    Rfn = wings_face:normal(Rf, Edge, We),
    e3d_vec:dot(Lfn,Rfn) < CosTolerance.

%%%
%%% Select shortest path
%%%

shortest_path(Method, St) ->
    #st{shapes=Shapes,selmode=Mode,sel=Sel} = St,
    case (Mode==vertex) andalso (length(Sel)==1) of
	true -> ok;
	false -> wings_u:error(?__(1,"Exactly two vertices must be\n selected on the same object."))
    end,
    [{Id,SelectedVs}] = Sel,
    case gb_sets:size(SelectedVs)==2 of
	true -> ok;
	false -> wings_u:error(?__(2,"Exactly two vertices must be selected."))
    end,
    We = gb_trees:get(Id, Shapes),
    [Pa,Pb] = [wings_vertex:pos(V, We) || V <- gb_sets:to_list(SelectedVs)],
    #we{es=Etab,vp=Vtab} = We,
    Graph = digraph:new(),
    Add_Edge = fun(_, EdgeRec, _) ->
		       build_digraph(Graph, EdgeRec, Vtab)
	       end,
    array:sparse_foldl(Add_Edge, [], Etab),
    PathVs = find_path_verts(Method, Graph, Pa, Pb),
    digraph:delete(Graph),
    SelFun = fun(Vert, We2) -> is_vert_in_path(PathVs, Vert, We2) end,
    St2 = wings_sel:make(SelFun, vertex, St),
    St3 = wings_sel_conv:mode(edge, St2),
    {save_state,wings_sel_conv:less(St3)}.

find_path_verts(Method, Graph, Pa, Pb) ->
    case Method of
	fewest_edges ->
	    StartTime = now(),
	    PathVs = digraph:get_short_path(Graph, Pa, Pb),
	    EndTime = now(),
	    io:fwrite("\nLength: ~p",[path_len(PathVs)]),
	    io:fwrite(" (~.2f seconds",[timer:now_diff(EndTime,StartTime)/1.0e6]),
	    io:fwrite(" Digraph)"),
	    ok;
	astar ->
	    StartTime = now(),
	    {Gcosts,PrevNodes,Closed,Open} = astar_init(Graph, Pa),
	    {Pm,Gm} = astar_loop(Open, Closed, Gcosts, PrevNodes, Graph, Pb, false),
	    PathVs = get_path(Pb, Pm),
	    EndTime = now(),
	    io:fwrite("\nLength: ~p",[dict:fetch(Pb,Gm)]),
	    io:fwrite(" (~.2f seconds",[timer:now_diff(EndTime,StartTime)/1.0e6]),
	    io:fwrite(" Astar)"),
	    %io:fwrite(" Len: ~p",[path_len(PathVs)]),
	    ok;
	dijkstra ->
	    StartTime = now(),
	    {Gcosts,PrevNodes,Open} = dijkstra_init(Graph, Pa),
	    {Pm,Gm} = dijkstra_loop(Open, Gcosts, PrevNodes, Graph, Pb, false),
	    PathVs = get_path(Pb, Pm),
	    EndTime = now(),
	    io:fwrite("\nLength: ~p",[dict:fetch(Pb,Gm)]),
	    io:fwrite(" (~.2f seconds",[timer:now_diff(EndTime,StartTime)/1.0e6]),
	    io:fwrite(" Dijkstra)"),
	    %io:fwrite(" Len: ~p",[path_len(PathVs)]),
	    ok
    end,
    PathVs.

%%%
%%% Dijkstra shortest path algorithm
%%%

dijkstra_init(Graph, Pa) ->
    Vs = digraph:vertices(Graph),
    Gcosts = dict:from_list([{V,1.0e6} || V <- Vs] ++ [{Pa,0.0}]),
    PrevNodes = gb_trees:from_orddict([{V,none} || V <- lists:sort(Vs)]),
    Open = gb_sets:from_list([{V,K} || {K,V} <- dict:to_list(Gcosts)]),
    {Gcosts,PrevNodes,Open}.

dijkstra_loop(_Open, Gcosts, PrevNodes, _Graph, _Pb, Done) when Done==true ->
    {PrevNodes,Gcosts};
dijkstra_loop(Open, Gcosts, PrevNodes, Graph, Pb, false) ->
    {{_,U},Open2} = gb_sets:take_smallest(Open),
    OutNeighbours = digraph:out_neighbours(Graph, U),
    {Gm,Om,Pm} = dijkstra_relax_all(U, OutNeighbours, Gcosts, Open2, PrevNodes),
    Done = (gb_sets:is_empty(Om)) or (U==Pb),
    dijkstra_loop(Om, Gm, Pm, Graph, Pb, Done).

dijkstra_relax(U, V, Gcosts, Open, PrevNodes) ->
    NewDist = dict:fetch(U,Gcosts) + e3d_vec:dist(U,V),
    case NewDist < dict:fetch(V,Gcosts) of
	true ->
	    Om = gb_sets:add({NewDist,V}, Open),
	    Pm = gb_trees:update(V, U, PrevNodes),	% U is parent of V
	    Gm = dict:store(V, NewDist, Gcosts),
	    {Gm,Om,Pm};
	false ->
	    {Gcosts,Open,PrevNodes}
    end.

dijkstra_relax_all(_, [], Gcosts, Open, PrevNodes) ->
    {Gcosts,Open,PrevNodes};
dijkstra_relax_all(U, ONs, Gcosts, Open, PrevNodes) ->
    [V|Tail] = ONs,
    {Gm,Om,Pm} = dijkstra_relax(U, V, Gcosts, Open, PrevNodes),
    dijkstra_relax_all(U, Tail, Gm, Om, Pm).

%%%
%%% A-star shortest path algorithm
%%%

astar_init(_Graph, Pa) ->
    Gcosts = dict:from_list([{Pa,0.0}]),
    PrevNodes = gb_trees:from_orddict([{Pa,none}]),
    Closed = sets:new(),
    Open = gb_sets:from_list([{0.0,Pa}]),
    {Gcosts,PrevNodes,Closed,Open}.

astar_loop(_, _Closed, Gcosts, PrevNodes, _Graph, _Pb, Done) when Done==true ->
    {PrevNodes,Gcosts};
astar_loop(Open, Closed, Gcosts, PrevNodes, Graph, Pb, false) ->
    {{_,U}, Open2} = gb_sets:take_smallest(Open),
    %{{_,U}, Open2} = gb_sets:take_largest(Open),
    Closed2 = sets:add_element(U, Closed),
    OutNeighbours = sets:from_list(digraph:out_neighbours(Graph, U)),
    OutNeighbours2 = sets:to_list(sets:subtract(OutNeighbours, Closed2)),
    {Gm,Om,Pm} = astar_relax_all(U, OutNeighbours2, Gcosts, Open2, PrevNodes, Pb),
    Done = (gb_sets:is_empty(Om)) or (U==Pb),
    astar_loop(Om, Closed2, Gm, Pm, Graph, Pb, Done).

astar_relax(U, V, Gcosts, Open, PrevNodes, Pb) ->
    Gcost = dict:fetch(U,Gcosts) + e3d_vec:dist(U,V),
    Hcost = e3d_vec:dist(V,Pb),
    Fcost = Gcost + Hcost,
    %Open2 = gb_sets:from_list([Val || {_Key,Val} <- gb_sets:to_list(Open)]),
    %case gb_sets:is_member(V, Open2) of
    case gb_trees:is_defined(V, PrevNodes) of
	false ->
	    Om = gb_sets:add({Fcost,V}, Open),		    % add to Open
	    Pm = gb_trees:enter(V, U, PrevNodes),	    % U is parent of V
	    Gm = dict:store(V, Gcost, Gcosts),		    % record cost
	    {Gm,Om,Pm};
	true ->
	    case Gcost < dict:fetch(V,Gcosts) of
		true ->
		    Om = gb_sets:add({Fcost,V}, Open),	    % update Open
		    Pm = gb_trees:update(V, U, PrevNodes),
		    Gm = dict:store(V, Gcost, Gcosts),
		    {Gm,Om,Pm};
		false ->
		    {Gcosts,Open,PrevNodes}
	    end
    end.

astar_relax_all(_, [], Gcosts, Open, PrevNodes, _Pb) ->
    {Gcosts,Open,PrevNodes};
astar_relax_all(U, ONs, Gcosts, Open, PrevNodes, Pb) ->
    [V|Tail] = ONs,
    {Gm,Om,Pm} = astar_relax(U, V, Gcosts, Open, PrevNodes, Pb),
    astar_relax_all(U, Tail, Gm, Om, Pm, Pb).

%%%
%%% Shortest path helper functions
%%%

is_vert_in_path(PathVs, Vert, We) ->
    #we{vp=Vtab} = We,
    Vpos = array:get(Vert, Vtab),
    lists:member(Vpos, PathVs).

get_path(none, _) -> [];
get_path(Key, Tree) ->
    Val = gb_trees:get(Key, Tree),
    [Key | get_path(Val,Tree)].

path_len(PathVs) ->
    calc_path_len(PathVs, 0.0).

calc_path_len([], Length) -> Length;
calc_path_len(PathVs, Length) ->
    [U|Tail] = PathVs,
    case length(Tail) of
	0 -> V = U;
	_ -> V = hd(Tail)
    end,
    Dist = e3d_vec:dist(U,V),
    calc_path_len(Tail, Length+Dist).

build_digraph(Graph, E, Vtab) ->
    #edge{vs=Va,ve=Vb} = E,
    PosA = wings_vertex:pos(Va, Vtab),
    PosB = wings_vertex:pos(Vb, Vtab),
    digraph:add_vertex(Graph, PosA),
    digraph:add_vertex(Graph, PosB),
    digraph:add_edge(Graph, PosA, PosB),
    digraph:add_edge(Graph, PosB, PosA).

%%%
%%% Select faces with similar area
%%%

similar_area(Ask, _St) when is_atom(Ask) ->
    Qs = [{label,?__(1,"Area Tolerance")},
	  {text,0.001,[{range,{0.0,100.0}}]}],
    wings_ask:dialog(Ask,
	?__(2,"Select Similar Area"), [{hframe,Qs}],
	fun(Res) ->
	    {select,{similar_area,Res}}
	end);
similar_area([Tolerance], St) ->
    #st{shapes=Shapes,selmode=Mode,sel=Sel} = St,
    case (Mode==face) and (length(Sel)==1) of
	true -> ok;
	false -> wings_u:error(?__(3,"Exactly one face must be\n selected on a single object."))
    end,
    [{Id,SelectedFs}] = Sel,
    case gb_sets:size(SelectedFs)==1 of
	true -> ok;
	false -> wings_u:error(?__(4,"Exactly one face must be selected."))
    end,
    We = gb_trees:get(Id, Shapes),
    Face1 = hd(gb_sets:to_list(SelectedFs)),
    Area1 = wings_face:area(Face1, We),
    SelFun = fun(Face, We2) ->
	is_area_similar(Area1, Tolerance, Face, We2)
    end,
    St2 = wings_sel:make(SelFun, face, St),
    {save_state,St2}.

is_area_similar(Area1, Tolerance, Face, We) ->
    Area2 = wings_face:area(Face, We),
    AreaDiff = abs(Area1-Area2),
    AreaDiff =< Tolerance.

%%%
%%% Change selection to complete loops from any mode
%%%
complete_loops(#st{sel=[]}=St) ->
    St;
complete_loops(#st{selmode=edge}=St) ->
    wings_edge_loop:select_loop(St);
complete_loops(St0) ->
    St = wings_sel_conv:mode(edge,St0),
    wings_edge_loop:select_loop(St).

hard_edges(#st{sel=[]}=St) ->
    Sel = fun(Edge, #we{he=Htab}) ->
		  gb_sets:is_member(Edge, Htab)
	  end,
    {save_state,wings_sel:make(Sel, edge, St)};
hard_edges(#st{selmode=Mode}=St0) ->
    St = if Mode =:= edge -> St0; true -> wings_sel_conv:mode(edge, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id,he=Htab},Acc) ->
		  Sel1 = gb_sets:to_list(Sel0),
		  HardEdges = [Edge || Edge <- Sel1, gb_sets:is_member(Edge, Htab)],
		  case HardEdges of
		    [] -> Acc;
		    _ -> [{Id, gb_sets:from_list(HardEdges)}|Acc]
		  end
	  end,[],St),
    {save_state,wings_sel:set(edge, Sel, St0)}.

vertices_with(N, #st{sel=[]}=St) ->
    Sel = fun(V, We) ->
		  Cnt = wings_vertex:fold(
			  fun(_, _, _, Cnt) ->
				  Cnt+1
			  end, 0, V, We),
		  Cnt =:= N
	  end,
    wings_sel:make(Sel, vertex, St);
vertices_with(N, #st{selmode=Mode}=St0) ->
    St = if Mode =:= vertex -> St0; true -> wings_sel_conv:mode(vertex, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We,Acc) ->
		  Sel1 = gb_sets:to_list(Sel0),
		  Vertices = [Vertex || Vertex <- Sel1, vertices_with(N, Vertex, We)],
		  case Vertices of
		    [] -> Acc;
		    _ -> [{Id, gb_sets:from_list(Vertices)}|Acc]
		  end
	  end,[],St),
    wings_sel:set(vertex, Sel, St0).

vertices_with(6, V, We) ->
    Cnt = wings_vertex:fold(
	  fun(_, _, _, Cnt) ->
		  Cnt+1
	  end, 0, V, We),
    Cnt >= 6;
vertices_with(N, V, We) ->
    Cnt = wings_vertex:fold(
	  fun(_, _, _, Cnt) ->
		  Cnt+1
	  end, 0, V, We),
    Cnt =:= N.


faces_with(Filter, #st{sel=[]}=St) ->
    Sel = fun(Face, We) ->
		  faces_with(Filter, Face, We)
	    end,
    wings_sel:make(Sel, face, St);
faces_with(Filter, #st{selmode=Mode}=St0) ->
    St = if Mode =:= face -> St0; true -> wings_sel_conv:mode(face, St0) end,
    Sel = wings_sel:fold(fun(Sel0, #we{id=Id}=We, Acc) ->
				Sel1 = gb_sets:to_list(Sel0),
				Faces = [Face || Face <- Sel1, faces_with(Filter, Face, We)],
				case Faces of
				  [] -> Acc;
				  _ -> [{Id,gb_sets:from_list(Faces)}|Acc]
				end
			end, [], St),
	wings_sel:set(face,Sel,St0).

faces_with(Filter, Face, We) ->
    Vs = wings_face:vertices(Face, We),
    case Filter of
      {non_quad,all} -> Vs =/= 4;
      {non_quad,odd} -> Vs rem 2 =/= 0;
      {non_quad,even} -> Vs =/= 4 andalso Vs rem 2 =:= 0;
      {faces_with,5} -> Vs >= 5;
      {faces_with,N} -> Vs =:= N
    end.
