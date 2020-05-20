%%
%%  wings_sel_cmd.erl --
%%
%%     This module implements the commands in the selection menu.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_sel_cmd).

-export([menu/0,update_menu/1,command/2]).

%% Utilities.
-export([init/0,select_all/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,member/2,keymember/3,keyfind/3,
		usort/1,any/2]).

init() ->
    wings_pref:set_default(saved_selections_cycle_by_mode,false).

update_menu(#st{selmode=Mode}) ->
    FaceMode = Mode =:= face,
    [wings_menu:update_menu_enabled(select, Cmd, FaceMode)
     || {_, Cmd, _} <- faces_menu()].

menu() ->
    Help = ?__(99," (from selection or all visible objects (if no selection))"),
    RHelp = random_help(),
    Objects = ?__(106," Objects"),
    [{?__(1,"Deselect"),deselect,?__(2,"Clear the selection")},
     separator,
     {?__(3,"More"),more,more_help()},
     {?__(4,"Less"),less,less_help()},
     {?__(5,"Similar"),similar,similar_help()} | faces_menu()] ++
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
	{?__(100,"Every Nth Ring"),{nth_edge_ring,
	    [{?__(101,"Second"),2},
	     {?__(102,"Third"),3},
	     {?__(103,"Nth..."),true}]}},
	{?__(109,"Every Nth Loop"),{nth_edge_loop,
	    [{?__(101,"Second"),2},
	     {?__(102,"Third"),3},
	     {?__(103,"Nth..."),true}]}},
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
	   {?__(85,"Non-planar Faces..."),
	    nonplanar_faces,?__(86,"Select all non-planar faces")++Help},
	   {?__(36,"Vertices With"),
	    {vertices_with,
	     [{?__(37,"2 Edges"),2,Help},
	      {?__(38,"3 Edges"),3,Help},
	      {?__(39,"4 Edges"),4,Help},
	      {?__(40,"5 Edges"),5,Help},
	      {?__(401,"6 or More"),6,Help},
	      {?__(402,"Specify..."),true,Help}]}},
	   {?__(41,"Faces With"),
	    {faces_with,
	     [{?__(42,"2 Edges"),2,Help},
	      {?__(43,"3 Edges"),3,Help},
	      {?__(44,"4 Edges"),4,Help},
	      {?__(45,"5 or More"),5,Help},
	      {?__(402,"Specify..."),true,Help}]}},
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
		     {"__%",true, RHelp}
		     ]}},
	   {?__(56,"Short Edges..."),
	    short_edges,?__(57,"Select (too) short edges")++Help},
	   {?__(87,"Sharp Edges..."),
	    sharp_edges,?__(88,"Select sharp edges")++Help},
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
	   {?__(62,"Id..."),id,?__(63,"Select by numeric id")},
	   {?__(107,"Name..."),
	    by_name,?__(108,"Select objects by name. *'s may be used as wildcards")}]}},
     {?__(64,"Lights"),lights,?__(65,"Select all lights")},
     separator,
     {?__(661, "All"), all,?__(66,"Select all elements")},
     separator,
     {?__(67,"Inverse"),inverse,?__(68,"Invert the selection")},
     separator,
     {?__(69,"Hide Selected")++Objects,
      hide_selected,
      ?__(70,"Hide all (partly or wholly) selected objects")},
     {?__(71,"Hide Unselected")++Objects,
      hide_unselected,?__(72,"Hide objects that have no selection")},
     {?__(73,"Lock Unselected")++Objects,
      lock_unselected,?__(74,"Lock objects that have no selection")},
     separator,
     {?__(104,"Unhide All Objects"),
      show_all,?__(76,"Show all objects that have been hidden")},
     {?__(105,"Unlock All Objects"),
      unlock_all,?__(78,"Unlock all locked objects")},
     separator | groups_menu()].

random_help() ->
    ?__(1,"Select random elements from current selection, or all visible objects (no selection)").

faces_menu() ->
  [{?__(1,"Similar Normals..."), oriented_faces,
    ?__(11,"Select faces with normals similar to those of the already selected faces")},
   {?__(2,"Similar Area"), similar_area,
    ?__(21,"Select faces with areas similar to that of the already selected face")},
   {?__(3,"Similar Material..."),similar_material,
    ?__(31,"Select faces with a similar material to those already selected")}].

groups_menu() ->
    [{?__(22,"Selection Groups"),
      {ssels,
       [{?__(79,"Store Selection"),store_selection,
	 ?__(80,"Store the selection into the selection group named \"StoredSelection\"")},
	{?__(81,"Recall Selection"),recall_selection,
	 ?__(82,"Recall the selection from the selection group named \"StoredSelection\"")},
	separator,
	{?__(83,"New Group..."),new_group,?__(84,"Create a new selection group")},
	separator,
	{?__(24,"Next Group"),next_group},
	{?__(25,"Previous Group"),prev_group},
	{?__(26,"Cycle In Selection Mode"),saved_selections_cycle_by_mode,
	 ?__(27,"Cycle Prev/Next only within active selection mode"),
	 wings_menu_util:crossmark(saved_selections_cycle_by_mode)}]}}].

more_help() ->
    ?__(1,"Select all elements adjacent to the selected elements").

less_help() ->
    ?__(1,"Deselect all elements adjacent to the unselected elements").

similar_help() ->
    ?__(1,"Select elements similar to the already selected elements").

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
command({edge_loop,{nth_edge_ring,N}}, St) ->
    select_nth_ring(N, St);
command({edge_loop,{nth_edge_loop,N}}, St) ->
    select_nth_loop(N, St);
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
    {save_state,wings_sel_conv:more(St)};
command(less, St) ->
    {save_state,wings_sel_conv:less(St)};
command(all, St) ->
    {save_state,select_all(St)};
command(lights, St) ->
    {save_state,select_lights(St)};
command({by,Command}, St) ->
    by_command(Command, St);
command(similar, St) ->
    {save_state,similar(St)};
command(oriented_faces, St) ->
    oriented_faces(true, St);
command({oriented_faces,Ask}, St) ->
    oriented_faces(Ask, St);
command(similar_area, St) ->
    similar_area(true, St);
command({similar_area,Ask}, St) ->
    similar_area(Ask, St);
command(similar_material, St) ->
    similar_material(true, St);
command({similar_material,Ask}, St) ->
    similar_material(Ask, St);
command(inverse, St) ->
    {save_state,inverse(St)};
command(hide_selected, St) ->
    {save_state,hide_selected(St)};
command(hide_unselected, St) ->
    {save_state,hide_unselected(St)};
command(lock_unselected, St) ->
    {save_state,lock_unselected(St)};
command(show_all, St0) ->
    All = wings_obj:fold(fun(#{id:=I}, A) -> [I|A] end, [], St0),
    St = wings_obj:unhide(All, St0),
    {save_state,St};
command(unlock_all, St0) ->
    All = wings_obj:fold(fun(#{id:=I}, A) -> [I|A] end, [], St0),
    St = wings_obj:unlock(All, St0),
    {save_state,St};
command({adjacent,Type}, St) ->
    set_select_mode(Type, St);
command({ssels, store_selection}, #st{ssels=Ssels0,selmode=Mode,sel=Sel}=St) ->
    Key = {Mode,"StoredSelection"},
    Ssels = gb_trees:enter(Key, Sel, Ssels0),
    {save_state,St#st{ssels=Ssels}};
command({ssels, recall_selection}, #st{selmode=Mode,ssels=Ssels}=St0) ->
    Key = {Mode, "StoredSelection"},
    case gb_trees:is_defined(Key, Ssels) of
	false -> St0;
	true ->
	    St = select_group(Key, St0),
	    {save_state,St}
    end;
command({ssels,saved_selections_cycle_by_mode}, _St) ->
    Pref = wings_pref:get_value(saved_selections_cycle_by_mode),
    wings_pref:set_value(saved_selections_cycle_by_mode, not Pref),
    keep;
command({ssels,next_group}, St) ->
    {save_state,cycle_group(next_group, St)};
command({ssels,prev_group}, St) ->
    {save_state,cycle_group(prev_group, St)};
command({ssels,{select_group,Id}}, St) ->
    {save_state,select_group(Id, St)};
command({ssels,{union_group,Id}}, St) ->
    {save_state,union_group(Id, St)};
command({ssels,{subtract_group,Id}}, St) ->
    {save_state,subtract_group(Id, St)};
command({ssels,{intersect_group,Id}}, St) ->
    {save_state,intersect_group(Id, St)};
command({ssels,{add_to_group,Id}}, St) ->
    {save_state,add_to_group(Id, St)};
command({ssels,{replace_group,Id}}, St) ->
    {save_state,replace_group(Id, St)};
command({ssels,{subtract_from_group,Id}}, St) ->
    {save_state,subtract_from_group(Id, St)};
command({new_group_name, Name}, St) ->
    {save_state,new_group_name(Name, St)};
command({ssels, new_group}, St) ->
    new_group(St);
command({ssels,{delete_group,invalid}}, St) ->
    {save_state,delete_invalid_groups(St)};
command({ssels,{delete_group,all}}, St) ->
    {save_state,St#st{ssels=gb_trees:empty()}};
command({ssels,{delete_group,Id}}, #st{ssels=Ssels}=St) ->
    {save_state,St#st{ssels=gb_trees:delete(Id, Ssels)}};
command(Type, St) ->
    set_select_mode(Type, St).

by_command(hard_edges, St) ->
    hard_edges(St);
by_command(isolated_vertices, St) ->
    {save_state,select_isolated(St)};
by_command(nonplanar_faces, St) ->
    nonplanar_faces(true, St);
by_command({nonplanar_faces,Ask}, St) ->
    nonplanar_faces(Ask, St);
by_command({vertices_with,N}, St) ->
    vertices_with(N, St);
by_command({non_quad,Type}, St) ->
    faces_with({non_quad,Type}, St);
by_command({faces_with,N}, St) ->
    faces_with({faces_with,N}, St);
by_command(material_edges, St) ->
    material_edges(St);
by_command({random, Percent}, St) ->
    random(Percent, St);
by_command(short_edges, St) ->
    short_edges(true, St);
by_command({short_edges,Ask}, St) ->
    short_edges(Ask, St);
by_command(sharp_edges, St) ->
    sharp_edges(true, St);
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
    {save_state,sel_by_id(Sel, St)};
by_command(by_name, St) ->
    by_name(St);
by_command({by_name_with, Name}, St) ->
    {save_state,by_name_with(Name, St)}.

face_region_to_edge_loop(St) ->
    wings_sel:update_sel(
      fun(Fs, We) ->
	      Es0 = wings_face:outer_edges(Fs, We),
	      Es = subtract_mirror_edges(Es0, We),
	      gb_sets:from_list(Es)
      end, edge, St).

subtract_mirror_edges(Es, #we{mirror=none}) -> Es;
subtract_mirror_edges(Es, #we{mirror=Face}=We) ->
    Es -- wings_face:to_edges([Face], We).

vs_to_edge_loop(St) ->
    wings_sel:update_sel(
      fun(Vs, We) ->
	      Es0 = vs_to_edges(Vs, We, []),
	      Es = subtract_mirror_edges(Es0, We),
	      gb_sets:from_list(Es)
      end, edge, St).

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

select_all(#st{selmode=body}=St) ->
    wings_sel:new_sel(fun(Sel, _) -> Sel end, body, St);
select_all(#st{selmode=Mode,sel=[]}=St) ->
    wings_sel:new_sel(fun(Sel, _) -> Sel end, Mode, St);
select_all(#st{selmode=Mode}=St) ->
    wings_sel:update_sel(
      fun(_Sel, We) ->
              wings_sel:get_all_items(Mode, We)
      end, Mode, St).

%%%
%%% Select Inverse.
%%%

inverse(#st{selmode=body}=St) ->
    Inverse = wings_sel:unselected_ids(St),
    wings_sel:make(fun(_, #we{id=Id}) ->
                           member(Id, Inverse)
                   end, body, St);
inverse(#st{selmode=Mode}=St) ->
    wings_sel:update_sel(
      fun(Items, We) ->
	      Diff = wings_sel:inverse_items(Mode, Items, We),
	      case gb_sets:is_empty(Diff) of
		  true -> Items;		%Can't inverse.
		  false -> Diff
	      end
      end, St).

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

hide_selected(St) ->
    Selected = wings_sel:selected_ids(St),
    wings_obj:hide(Selected, St).

hide_unselected(St) ->
    Unselected = wings_sel:unselected_ids(St),
    wings_obj:hide(Unselected, St).

lock_unselected(St) ->
    Unselected = wings_sel:unselected_ids(St),
    wings_obj:lock(Unselected, St).

%%%
%%% Selection Groups
%%%

union_group(Key, #st{sel=Sel0}=St) ->
    Ssel = coerce_ssel(Key, St),
    Sel = union(Sel0, Ssel),
    wings_sel:valid_sel(St#st{sel=Sel}).

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

%%%% Delete Groups that return an empty selection. Invalid ssels can result from
%%%% creating or deleting geomerty.
delete_invalid_groups(#st{ssels=Ssels}=St0) ->
    case gb_trees:is_empty(Ssels) of
      true ->
        St0;
      false ->
        Keys = gb_trees:keys(Ssels),
        lists:foldl(fun(Key,#st{ssels=Ss,selmode=Mode}=St) ->
                Ssel = gb_trees:get(Key,Ss),
                ValidSel = wings_sel:valid_sel(Ssel, Mode, St),
                case ValidSel of
                  [] -> St#st{ssels=gb_trees:delete(Key,Ss)};
                  _ -> St
                end
        end,St0,Keys)
    end.

add_to_group({Mode,_}=Key, #st{ssels=Ssels}=St) -> 
    Ssel0 = gb_trees:get(Key, Ssels),
    Ssel1 = wings_sel:valid_sel(Ssel0, Mode, St),
    #st{sel=Sel} = possibly_convert(Mode, St),
    Ssel = union(Ssel1, Sel),
    save_group(Key, Ssel, St).

replace_group({_,Name}=Key, #st{ssels=Ssels0, selmode=Mode, sel=Sel}=St) ->
    case Key of
	{Mode,Name} ->	% same mode - no need to check for name duplication
	    Ssels = gb_trees:update(Key, Sel, Ssels0);
	_ ->
	    NewKey =
		case gb_trees:is_defined({Mode,Name}, Ssels0) of
		    true ->
			Names = [Name0 || {Mode0,Name0} <- gb_trees:keys(Ssels0), Mode0=:=Mode],
			{Mode, wings_util:unique_name(Name, Names)};
		    false ->
			{Mode, Name}
		end,
	    Ssels = gb_trees:insert(NewKey, Sel, gb_trees:delete(Key, Ssels0))
    end,
    St#st{ssels=Ssels}.

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
    wings_dialog:ask(?__(1,"Create New Group"),
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
	    wings_u:error_msg(Msg)
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

%%%% Cycle Through Save Selections
cycle_group(Dir, #st{selmode=SelMode,ssels=Ssels,sh=Sh}=St) ->
    case gb_trees:is_empty(Ssels) of
	true -> St;
	false ->
	    Keys0 = gb_trees:keys(Ssels),
	    Keys1 = case wings_pref:get_value(saved_selections_cycle_by_mode) of
			true when Sh -> Keys0;
			true -> [Key || {Mode,_}=Key <- Keys0, Mode =:= SelMode];
			false -> Keys0
		    end,
	    Keys = case Dir of
		       next_group -> Keys1;
		       prev_group -> lists:reverse(Keys1)
		   end,
	    cycle_ss_keys(Keys,St)
    end.

cycle_ss_keys([],St) -> St;
cycle_ss_keys(Keys,St) ->
    case search_ssel_keys(Keys,St,[]) of
      {none,[]} -> [Key|_] = Keys;
      {none,Acc} ->
        Key = lists:last(Acc);
      [] ->
        [Key|_] = Keys;
      Other ->
        [Key|_] = Other
    end,
    case select_group(Key,St) of
      #st{sel=[]} -> cycle_ss_keys(lists:delete(Key,Keys),St);
      NewSt -> NewSt
    end.

search_ssel_keys([{Mode,_}=PKey|Keys],#st{selmode=Mode,sel=Sel,ssels=Ssels}=St,Acc) ->
    PSel0 = gb_trees:get(PKey,Ssels),
    PSel = wings_sel:valid_sel(PSel0, Mode, St),
    case PSel =:= Sel of
      true ->
        Keys;
      false ->
        search_ssel_keys(Keys,St,[PKey|Acc])
    end;
search_ssel_keys([_|Keys],St,Acc) ->
    search_ssel_keys(Keys,St,Acc);
search_ssel_keys([],_St,Acc) ->
    {none,Acc}.

%%%
%%% Select Similar.
%%%

similar(#st{selmode=vertex}=St) ->
    do_similar(fun make_vertex_template/2, St);
similar(#st{selmode=edge}=St) ->
    do_similar(fun make_edge_template/2, St);
similar(#st{selmode=face}=St) ->
    do_similar(fun make_face_template/2, St);
similar(#st{selmode=body}=St) ->
    MF = fun(_, #we{vp=Vtab,es=Etab,fs=Ftab}) ->
		 {wings_util:array_entries(Vtab),
		  wings_util:array_entries(Etab),
		  gb_trees:size(Ftab)}
	 end,
    RF = fun(T, A) -> [T|A] end,
    Templates0 = wings_sel:dfold(MF, RF, [], St),
    Templates = usort(Templates0),
    Zero = gb_sets:singleton(0),
    wings_sel:make(fun(_, We) ->
			   Template = MF(Zero, We),
			   member(Template, Templates)
		   end, body, St).

do_similar(MakeTemplate, #st{selmode=Mode}=St) ->
    MF = fun(Sel, We) ->
		 Ts = gb_sets:fold(
			fun(I, A) ->
				[MakeTemplate(I, We)|A]
			end, [], Sel),
		 usort(Ts)
	 end,
    RF = fun lists:umerge/2,
    Templates0 = wings_sel:dfold(MF, RF, [], St),
    Templates = consolidate_templates(Templates0),
    wings_sel:make(
      fun(Item, We) ->
	      Template = MakeTemplate(Item, We),
	      match_templates(Template, Templates)
      end, Mode, St).

consolidate_templates([H|T]) ->
    consolidate_templates_1(H, T).

consolidate_templates_1(Templ0, [Templ|T]) ->
    case match_template(Templ0, Templ) of
	true ->
	    consolidate_templates_1(Templ0, T);
	false->
	    [Templ0|consolidate_templates_1(Templ, T)]
    end;
consolidate_templates_1(Templ, []) -> [Templ].

match_templates(T, Templates) ->
    any(fun(Template) ->
		match_template(T, Template)
	end, Templates).

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

random(true, St) ->
    Qs0 = [{hframe,[{slider,{text,float(wings_pref:get_value(random_select, 25.0)),
            [{range,{0.0,100.0}}]}}]}],
    Qs = [{vframe,Qs0}],
    Title =  ?__(1,"Select Random"),
    Cmd = {select,by,random},
    wings_dialog:dialog_preview(Cmd, true, Title, Qs, St);
random([Percent], St) ->
    random(Percent, St);
random(Percent, #st{selmode=Mode}=St0) ->
    P = Percent / 100,
    wings_pref:set_value(random_select, Percent),
    St = intersect_sel_items(fun(_, _) -> rand:uniform() < P end, Mode, St0),
    {save_state,St}.

%%
%% Select short edges.
%%

short_edges(Ask, St) when is_atom(Ask) ->
    Qs0 = [{label,?__(1,"Length tolerance")},
      {text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
    Qs = [{hframe,Qs0}],
    Title = ?__(2,"Select Short Edges"),
    Cmd = {select,by,short_edges},
    wings_dialog:dialog_preview(Cmd, Ask, Title, Qs, St);
short_edges([Tolerance], St0) ->
    St = intersect_sel_items(fun(Edge, We) ->
				     is_short_edge(Tolerance, Edge, We)
			     end, edge, St0),
    {save_state,St}.

is_short_edge(Tolerance, Edge, #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    e3d_vec:dist(VaPos, VbPos) < Tolerance.

%%
%% Select all edges between materials.
%%

material_edges(St) ->
    intersect_sel_items(
      fun(Edge, #we{es=Etab}=We) ->
	      #edge{lf=Lf,rf=Rf} = array:get(Edge, Etab),
	      wings_facemat:face(Lf, We) =/= wings_facemat:face(Rf, We)
      end, edge, St).

%%
%% Select all faces that have (proper) UV coordinates.
%%

uv_mapped_faces(St0) ->
    St1 = possibly_convert(face, St0),
    St = intersect_sel(
	   fun(We) ->
		   UVF = wings_we:uv_mapped_faces(We),
		   gb_sets:from_ordset(UVF)
	   end, face, St1),
    {save_state,St}.

%%
%% Select by numerical item id.
%%

by_id(#st{selmode=body}=St) ->
    ask([{"Object Id",1}],
	fun([Id]) ->
		{"",[{Id,gb_sets:singleton(0)}]}
	end, St);
by_id(#st{selmode=vertex}=St) ->
    item_by_id("Vertex Id", St);
by_id(#st{selmode=edge}=St) ->
    item_by_id("Edge Id", St);
by_id(#st{selmode=face}=St) ->
    item_by_id("Face Id", St).

item_by_id(Prompt, #st{sel=[_]}=St) ->
    MF = fun(_, #we{id=Id}) -> Id end,
    RF = fun(I, []) -> I end,
    Id = wings_sel:dfold(MF, RF, [], St),
    ask([{Prompt,0}],
	fun([Item]) ->
		{Prompt,[{Id,gb_sets:singleton(Item)}]}
	end, St);
item_by_id(Prompt, St) ->
    FF = fun(#{id:=Id}, A) -> [Id|A] end,
    case wings_obj:fold(FF, [], St) of
        [] ->
            wings_u:error_msg(?__(1,"Nothing to select."));
        [Id] ->
	    ask([{Prompt,0}],
		fun([Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end, St);
	[Id0|_] ->
	    ask([{?__(2,"Object Id"),Id0},
		 {Prompt,0}],
		fun([Id,Item]) ->
			{Prompt,[{Id,gb_sets:singleton(Item)}]}
		end, St)
    end.


by_name(St) ->
    case wings_obj:num_objects(St) of
        0 ->
            wings_u:error_msg(?__(1,"Nothing to select."));
        _ ->
            wings_dialog:ask(?__(2,"Select by name"),
			     [{?__(3,"Name"), ""}],
			     fun([String]) ->
                                     {select,{by,{by_name_with,String}}}
                             end)
    end.

by_name_with(Filter, #st{selmode=Mode}=St0) ->
    FF = fun(#{id:=Id,name:=Name}, A) ->
                 case wings_util:is_name_masked(Name, Filter) of
                     true -> [Id|A];
                     false -> A
                 end
         end,
    Ids = gb_sets:from_list(wings_obj:fold(FF, [], St0)),
    SF = fun(_, #we{id=Id}) -> gb_sets:is_member(Id, Ids) end,
    St = wings_sel:make(SF, body, St0),
    wings_sel_conv:mode(Mode, St).

valid_sel(Prompt, Sel, #st{selmode=Mode}=St) ->
    case wings_sel:valid_sel(Sel, Mode, St) of
	[] ->
	    [{Id,Item0}] = Sel,
	    [Item] = gb_sets:to_list(Item0),
            FF = fun(#{id:=ObjId}, A) -> (Id =:= ObjId) or A end,
            case wings_obj:fold(FF, false, St) of
		false ->
		    wings_u:error_msg(?__(1,"The Object Id ")++
				  integer_to_list(Id)++
				  ?__(2," is invalid."));
		true ->
		    wings_u:error_msg(?__(3,"The ")++Prompt++" "++
				  integer_to_list(Item)++
				  ?__(4," is invalid."))
	    end;
	Sel -> Sel
    end.

ask(Qs, Fun, St) ->
    wings_dialog:ask(?__(1,"Select By Id"), {preview,Qs},
    fun
        ({dialog_preview,Res}) ->
            try
                Sel = Fun(Res),
                {preview,St,sel_by_id(Sel, St)}
            catch throw:_ -> {preview,St,St}
            end;
        (cancel) -> St;
        (Res) ->
            Sel = Fun(Res),
            {commit,St,sel_by_id(Sel, St)}
    end).

sel_by_id({Prompt,Sel}, St) ->
    wings_sel:set(valid_sel(Prompt, Sel, St), St).

%%%
%%% Select lights.
%%%

select_lights(#st{selmode=Mode}=St0) ->
    SF = fun(_, We) when ?IS_LIGHT(We) ->
                 gb_sets:singleton(0);
            (_, _) ->
                 gb_sets:empty()
         end,
    St = wings_sel:new_sel(SF, body, St0),
    wings_sel_conv:mode(Mode, St).

%%%
%%% Select isolated vertices.
%%%

select_isolated(St) ->
    intersect_sel(
      fun(We) ->
	      gb_sets:from_list(wings_vertex:isolated(We))
      end, vertex, St).

%%%
%%% Select nonplanar faces
%%%

nonplanar_faces(Ask, St) when is_atom(Ask) ->
    Qs = [{label,?__(1,"Distance tolerance")},
	  {text,1.0E-3,[{range,{1.0E-5,10.0}}]}],
    Title = ?__(2,"Select Non-planar Faces"),
    wings_dialog:dialog_preview({select,by,nonplanar_faces}, Ask, Title,
	  [{hframe,Qs}], St);
nonplanar_faces([Tolerance], St0) ->
    St = intersect_sel_items(
	   fun(Face, We) ->
		   not wings_face:is_planar(Tolerance, Face, We)
	   end, face, St0),
    {save_state,St}.


%%%
%%% Select similarly oriented faces.
%%%

oriented_faces(_, #st{selmode=Mode, sel=Sel}) when Mode =/= face; Sel =:= [] ->
    wings_u:error_msg(?__(4,"At least one face must be selected")),
    keep;
oriented_faces(Ask, St) when is_atom(Ask) ->
    Connected = wings_pref:get_value(similar_normals_connected,false),
    {Save,Angle} = case wings_pref:get_value(similar_normals_angle,{false,1.0E-3}) of
        {true,A} -> {true,A};
        {false,_} -> {false,1.0E-3}
    end,
    Qs0 = [{vframe,
           [{hframe,[{slider,{text,Angle,[{range,{1.0E-3,180.0}}]}}]}],
           [{title,?__(1,"Angle Tolerance")}]},
          {?__(3,"Connected Faces Only"),Connected},
          {?__(5,"Save Angle"),Save}],
    Qs = [{vframe,Qs0}],
    Title = ?__(2,"Select Similarly Oriented Faces"),
    Cmd = {select,oriented_faces},
    wings_dialog:dialog_preview(Cmd, Ask, Title, Qs, St);
oriented_faces([Tolerance,Connected,Save], #st{selmode=face, sel=[]}) ->
    wings_pref:set_value(similar_normals_connected, Connected),
    wings_pref:set_value(similar_normals_angle, {Save,Tolerance}),
    wings_u:error_msg(?__(4,"At least one face must be selected"));
oriented_faces([Tolerance,false,Save], St) ->
    wings_pref:set_value(similar_normals_connected, false),
    wings_pref:set_value(similar_normals_angle, {Save,Tolerance}),
    MF = fun collect_normals/2,
    RF = fun lists:umerge/2,
    Normals = wings_sel:dfold(MF, RF, [], St),
    Pred = matching_normal_pred(Tolerance, Normals),
    {save_state,wings_sel:make(Pred, face, St)};
oriented_faces([Tolerance,true,Save], St0) ->
    wings_pref:set_value(similar_normals_connected, true),
    wings_pref:set_value(similar_normals_angle, {Save,Tolerance}),
    St = wings_sel:update_sel(
	   fun(Faces, We) ->
		   Normals = collect_normals(Faces, We),
		   Pred = matching_normal_pred(Tolerance, Normals),
		   find_similar_connected(Pred, Faces, We)
	   end, St0),
    {save_state,St}.

matching_normal_pred(Tolerance, Normals) ->
    CosTolerance = math:cos(Tolerance * (math:pi() / 180.0)),
    fun(Face, We) ->
	    N = wings_face:normal(Face, We),
	    any_matching_normal(CosTolerance, N, Normals)
    end.

any_matching_normal(CosTolerance, Norm, Normals) ->
    any(fun(N) -> e3d_vec:dot(N, Norm) >= CosTolerance end, Normals).

collect_normals(Faces, We) ->
    Ns = gb_sets:fold(fun(Face, A) ->
			      [wings_face:normal(Face, We)|A]
		      end, [], Faces),
    usort(Ns).

%%%
%%% Select faces of the same material.
%%%
similar_material(_, #st{selmode=Mode, sel=Sel}) when Mode =/= face; Sel =:= [] ->
    wings_u:error_msg(?__(3,"At least one face must be selected")),
    keep; %Wrong mode (invoked through hotkey).
similar_material(Ask, St) when is_atom(Ask) ->
    Connected = wings_pref:get_value(similar_materials_connected, false),
    Mode = wings_pref:get_value(similar_materials, material),
    Qs0 = [{?__(2,"Connected Faces Only"),Connected},
	  {vradio,[{?__(5,"Materials"),material},
		   {?__(6,"Vertex Color"),vertex_color}],
	   Mode}],
    Qs = [{vframe,Qs0}],
    Title = ?__(4,"Select Faces with the same Material"),
    Cmd = {select,similar_material},
    wings_dialog:dialog_preview(Cmd, Ask, Title, Qs, St);
similar_material([Connected,Mode], #st{selmode=face,sel=[]}) ->
    wings_pref:set_value(similar_materials, Mode),
    wings_pref:set_value(similar_materials_connected, Connected),
    wings_u:error_msg(?__(3,"At least one face must be selected"));

similar_material([false,Mode], St) ->
    wings_pref:set_value(similar_materials, Mode),
    wings_pref:set_value(similar_materials_connected, false),
    MF = case Mode of
	     vertex_color -> fun collect_colors/2;
	     material -> fun collect_materials/2
	 end,
    RF = fun lists:umerge/2,
    Materials = wings_sel:dfold(MF, RF, [], St),
    Pred = similar_material_pred(Mode, Materials),
    {save_state,wings_sel:make(Pred, face, St)};
similar_material([true,Mode], St0) ->
    wings_pref:set_value(similar_materials, Mode),
    wings_pref:set_value(similar_materials_connected, true),
    F = case Mode of
	    vertex_color ->
		fun(Faces, We) ->
			Colors = collect_colors(Faces, We),
			Pred = similar_material_pred(Mode, Colors),
			find_similar_connected(Pred, Faces, We)
		end;
	    material ->
		fun(Faces, We) ->
			Materials = collect_materials(Faces, We),
			Pred = similar_material_pred(Mode, Materials),
			find_similar_connected(Pred, Faces, We)
		end
	end,
    St = wings_sel:update_sel(F, St0),
    {save_state,St}.

similar_material_pred(vertex_color, Colors) ->
    fun(Face, We) ->
	    Color = average_colors(Face, We),
	    member(Color, Colors)
    end;
similar_material_pred(material, Materials) ->
    fun(Face, We) ->
	    Mat = wings_facemat:face(Face, We),
	    member(Mat, Materials)
    end.

collect_colors(Faces, We) ->
    Cs = gb_sets:fold(fun(F, A) ->
			      [average_colors(F, We)|A]
		      end, [], Faces),
    usort(Cs).

collect_materials(Faces, We) ->
    Ms = gb_sets:fold(fun(F, A) ->
			      [wings_facemat:face(F, We)|A]
		      end, [], Faces),
    usort(Ms).

average_colors(Face, We) ->
    wings_color:average([C || C <- wings_va:face_attr(color, Face, We)]).

%%%
%%% Select sharp edges
%%%

sharp_edges(Ask, St) when is_atom(Ask) ->
    Qs = [{hframe,[{label,?__(1,"Max Angle")},
      {slider,{text,120.0,[{range,{0.0,180.0}}]}}]},
      {hradio,[{?__(3,"Peaks"),convex},
               {?__(4,"Valleys"),concave},
               {?__(5,"Both"),both}],both}],
    wings_dialog:dialog_preview({select,by,sharp_edges}, Ask,
      ?__(2,"Select Sharp Edges"), [{vframe,Qs}], St);
sharp_edges([Tolerance,Type], St0) ->
    CosTolerance = -math:cos(Tolerance * math:pi() / 180.0),
    St = intersect_sel_items(
	   fun(Edge, We) ->
		   is_sharp_edge(CosTolerance, Type, Edge, We)
	   end, edge, St0),
    {save_state,St}.

is_sharp_edge(CosTolerance, Type, Edge, #we{es=Etab,vp=Vtab}=We) ->
    #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb} = array:get(Edge, Etab),
    Lfn = wings_face:normal(Lf, Edge, We),
    Rfn = wings_face:normal(Rf, Edge, We),
    Dot = e3d_vec:dot(Lfn,Rfn),
    case Dot < CosTolerance of
      false -> false;
      true when Type =:= both -> true;
      true ->
        EdgeNormal = e3d_vec:add(Lfn,Rfn),
        EdgeCenter = e3d_vec:average(array:get(Va, Vtab), array:get(Vb, Vtab)),
        FacesCenter = e3d_vec:average(wings_face:center(Lf,We),wings_face:center(Rf,We)),
        Vec = e3d_vec:sub(EdgeCenter,FacesCenter),
        case Type of
           convex -> e3d_vec:dot(EdgeNormal,Vec) > 0.0;
           concave -> e3d_vec:dot(EdgeNormal,Vec) < 0.0
        end
    end.

%%%
%%% Select shortest path
%%%

shortest_path(Method, #st{selmode=Mode}=St0) ->
    MF = fun(Items, _) ->
                 [Mode =:= vertex andalso gb_sets:size(Items)]
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St0) of
        [2] ->
            ok;
        _ ->
            wings_u:error_msg(?__(1,"Exactly two vertices must be\n"
                                  "selected on the same object."))
    end,
    St = wings_sel:update_sel(fun(Vs, We) ->
                                      shortest_path_1(Vs, We, Method)
                              end, edge, St0),
    {save_state,St}.

shortest_path_1(Vs, We, Method) ->
    #we{es=Etab,vp=Vtab} = We,
    [Pa,Pb] = [wings_vertex:pos(V, We) || V <- gb_sets:to_list(Vs)],
    Graph = digraph:new(),
    AddEdge = fun(_, EdgeRec, _) ->
                      build_digraph(Graph, EdgeRec, Vtab)
              end,
    array:sparse_foldl(AddEdge, [], Etab),
    PathVs = find_path_verts(Method, Graph, Pa, Pb),
    digraph:delete(Graph),
    SelFun = fun(V) -> is_vert_in_path(PathVs, V, We) end,
    AllVs = wings_sel:get_all_items(vertex, We),
    SelVs = gb_sets:filter(SelFun, AllVs),
    gb_sets:from_ordset(vs_to_edges(SelVs, We, [])).

find_path_verts(Method, Graph, Pa, Pb) ->
    case Method of
	fewest_edges ->
	    StartTime = os:timestamp(),
	    PathVs = digraph:get_short_path(Graph, Pa, Pb),
	    EndTime = os:timestamp(),
	    io:fwrite("\nLength: ~p",[path_len(PathVs)]),
	    io:fwrite(" (~.2f seconds",[timer:now_diff(EndTime,StartTime)/1.0e6]),
	    io:fwrite(" Digraph)"),
	    ok;
	astar ->
	    StartTime = os:timestamp(),
	    {Gcosts,PrevNodes,Closed,Open} = astar_init(Graph, Pa),
	    {Pm,Gm} = astar_loop(Open, Closed, Gcosts, PrevNodes, Graph, Pb, false),
	    PathVs = get_path(Pb, Pm),
	    EndTime = os:timestamp(),
	    io:fwrite("\nLength: ~p",[dict:fetch(Pb,Gm)]),
	    io:fwrite(" (~.2f seconds",[timer:now_diff(EndTime,StartTime)/1.0e6]),
	    io:fwrite(" Astar)"),
	    %io:fwrite(" Len: ~p",[path_len(PathVs)]),
	    ok;
	dijkstra ->
	    StartTime = os:timestamp(),
	    {Gcosts,PrevNodes,Open} = dijkstra_init(Graph, Pa),
	    {Pm,Gm} = dijkstra_loop(Open, Gcosts, PrevNodes, Graph, Pb, false),
	    PathVs = get_path(Pb, Pm),
	    EndTime = os:timestamp(),
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

similar_area(Ask, St) when is_atom(Ask) ->
    _ = get_area(St),
    Qs = [{label,?__(1,"Area Tolerance")},
          {text,0.001,[{range,{0.0,100.0}}]}],
    wings_dialog:dialog_preview({select,similar_area}, Ask,
                                ?__(2,"Select Similar Area"),
                                [{hframe,Qs}], St);
similar_area([Tolerance], St) ->
    Area = get_area(St),
    SelFun = fun(Face, We) ->
                     is_area_similar(Area, Tolerance, Face, We)
             end,
    St2 = wings_sel:make(SelFun, face, St),
    {save_state,St2}.

is_area_similar(Area, Tolerance, Face, We) ->
    AreaDiff = abs(wings_face:area(Face, We) - Area),
    AreaDiff =< Tolerance.

get_area(#st{selmode=face}=St) ->
    MF = fun(Faces, We) ->
                 case gb_sets:size(Faces) of
                     1 ->
                         [Face] = gb_sets:to_list(Faces),
                         [wings_face:area(Face, We)];
                     _ ->
                         [none]
                 end
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St) of
        [Area] when is_float(Area) ->
            Area;
        _ ->
            wings_u:error_msg(?__(1,"Exactly one face must be selected"))
    end;
get_area(_) ->
    wings_u:error_msg(?__(1,"Exactly one face must be selected")).


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

hard_edges(St0) ->
    St = intersect_sel(fun(#we{he=Htab}) -> Htab end, edge, St0),
    {save_state,St}.

vertices_with(true, St) ->
    Qs = [{vframe,
           [{hframe,[{label,?__(1,"Number of Edges")},
		     {text,2,[{range,{2,1000}}]}]},
	    {hradio,[{?__(3,"More"),more},
		     {?__(4,"Less"),less},
		     {?__(5,"Exactly"),exactly}],exactly}]}],
    wings_dialog:dialog_preview({select,by,vertices_with}, true,
				?__(2,"Select Vertices"), [{hframe,Qs}], St);
vertices_with(N, St0) ->
    St = intersect_sel_items(
	   fun(V, We) ->
		   vertices_with(N, V, We)
	   end, vertex, St0),
    {save_state,St}.

vertices_with(6, V, We) ->
    Cnt = wings_vertex:fold(
	  fun(_, _, _, Cnt) ->
		  Cnt+1
	  end, 0, V, We),
    Cnt >= 6;
vertices_with(N, V, We) when is_integer(N) ->
    Cnt = wings_vertex:fold(
	  fun(_, _, _, Cnt) ->
		  Cnt+1
	  end, 0, V, We),
    Cnt =:= N;
vertices_with([N,Mode], V, We) ->
    Cnt = wings_vertex:fold(
	  fun(_, _, _, Cnt) ->
		  Cnt+1
	  end, 0, V, We),
    case Mode of
        more -> Cnt >= N;
        less -> Cnt =< N;
        exactly -> Cnt =:= N
    end.

faces_with({faces_with,true}, St) ->
    Qs = [{vframe,
           [{hframe,[{label,?__(1,"Number of Edges")},
             {text,2,[{range,{2,1000}}]}]},
             {hradio,[{?__(3,"More"),more},
               {?__(4,"Less"),less},
               {?__(5,"Exactly"),exactly}],exactly}]}],
    wings_dialog:dialog_preview({select,by,faces_with}, true,
        ?__(2,"Select Faces"), [{hframe,Qs}], St);
faces_with(Filter, St0) ->
    St = intersect_sel_items(
	   fun(Face, We) ->
		   faces_with(Filter, Face, We)
	   end, face, St0),
    {save_state,St}.

faces_with(Filter, Face, We) ->
    Vs = wings_face:vertices(Face, We),
    case Filter of
      {non_quad,all} -> Vs =/= 4;
      {non_quad,odd} -> Vs rem 2 =/= 0;
      {non_quad,even} -> Vs =/= 4 andalso Vs rem 2 =:= 0;
      {faces_with,5} -> Vs >= 5;
      {faces_with,N} when is_integer(N)-> Vs =:= N;
      {faces_with,[N,more]} -> Vs >= N;
      {faces_with,[N,less]} -> Vs =< N;
      {faces_with,[N,exactly]} -> Vs =:= N
    end.

select_nth_ring(true, #st{selmode=edge}=St) ->
    Qs = [{label,?__(1,"Interval")},
	  {text,2,[{range,{1,1000}}]}],
    wings_dialog:dialog_preview({select,edge_loop,nth_edge_ring}, true,
				?__(2,"Select Every Nth Edge Ring"), [{hframe,Qs}], St);
select_nth_ring([N], #st{selmode=edge}=St) ->
    {save_state,wings_edge:select_nth_ring(N,St)};
select_nth_ring(N, #st{selmode=edge}=St) ->
    {save_state,wings_edge:select_nth_ring(N,St)};
select_nth_ring(_, St) ->
    {save_state,St}.

select_nth_loop(true, #st{selmode=edge}=St) ->
    Qs = [{label,?__(1,"Interval")},
	  {text,2,[{range,{1,1000}}]}],
    wings_dialog:dialog_preview({select,edge_loop,nth_edge_loop}, true,
				?__(2,"Select Every Nth Edge Ring"), [{hframe,Qs}], St);
select_nth_loop([N], #st{selmode=edge}=St) ->
    {save_state,wings_edge:select_nth_loop(N,St)};
select_nth_loop(N, #st{selmode=edge}=St) ->
    {save_state,wings_edge:select_nth_loop(N,St)};
select_nth_loop(_, St) ->
    {save_state,St}.

%%%
%%% Utilities.
%%%

find_similar_connected(Pred, FaceSel, We) when is_function(Pred, 2) ->
    find_similar_connected_1(FaceSel, Pred, We, FaceSel).

find_similar_connected_1(Sel0, Pred, We, LastSel0) ->
    LastSel1 = wings_face:extend_border(LastSel0, We),
    LastSel2 = gb_sets:subtract(LastSel1, Sel0),
    LastSel = find_similar_connected_2(LastSel2, Pred, We),
    case gb_sets:is_empty(LastSel) of
        true ->
	    Sel0;
        false ->
	    Sel = gb_sets:union(Sel0, LastSel),
	    find_similar_connected_1(Sel, Pred, We, LastSel)
    end.

find_similar_connected_2(Sel, Pred, We) ->
    gb_sets:fold(
      fun(Face, A) ->
	      case Pred(Face, We) of
		  true -> gb_sets:add(Face, A);
		  false -> A
	      end
      end, gb_sets:empty(), Sel).

intersect_sel(F, Mode, St0) when is_function(F, 1) ->
    St = possibly_convert(Mode, St0),
    FF = fun(Sel, We) ->
		 gb_sets:intersection(Sel, F(We))
	 end,
    case St of
	#st{sel=[]} ->
	    wings_sel:new_sel(FF, Mode, St);
	#st{} ->
	    wings_sel:update_sel(FF, Mode, St)
    end.

intersect_sel_items(F, Mode, St0) when is_function(F, 2) ->
    St = possibly_convert(Mode, St0),
    FF = fun(Sel, We) ->
		 gb_sets:filter(fun(Item) -> F(Item, We) end, Sel)
	 end,
    case St of
	#st{sel=[]} ->
	    wings_sel:new_sel(FF, Mode, St);
	#st{} ->
	    wings_sel:update_sel(FF, Mode, St)
    end.
