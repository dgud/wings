%%
%%  wpc_turnedge.erl --
%%
%%     Plug-in for turning edges
%%
%%  Copyright (c) 2002 Chris Osgood,
%%		  2003-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_turnedge).

-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").
-import(lists, [foldl/3]).

-define(MENU_ENTRY(Key), {command_name(Key),Key,command_help(Key)}).

init() ->
    true.

menu({edge}, Menu) -> menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    TurnMenu = turns(),
    Menu ++ [separator,{command_name(turn),{turn,TurnMenu}}].

%% Advanced Menus
command({edge,turn_cw}, St) ->
    turn_edges(fun cw_mode/2, false, St);
command({edge,turn_ccw}, St) ->
    turn_edges(fun ccw_mode/2, false, St);
command({edge,turn_optimized}, St) ->
    turn_edges(fun cw_mode/2, true, St);
command(_Cmd, _) -> next.

turns() ->
    fun(B, _Ns) -> turn_menu(B) end.

turn_menu(1) -> {edge,turn_cw};
turn_menu(2) -> {edge,turn_ccw};
turn_menu(3) -> {edge,turn_optimized};
turn_menu(help) ->
    {command_help(turn_cw),command_help(turn_ccw),command_help(turn_optimized)};
turn_menu(_) -> ignore.

command_name(turn) -> ?__(turn, "Turn").
    
command_help(turn_cw) ->
    ?__(turn_cw, "Turn edge clockwise");
command_help(turn_ccw) ->
    ?__(turn_ccw, "Turn edge counter-clockwise");
command_help(turn_optimized) ->
    ?__(turn_optimized, "Turn edge (clockwise) only if new edge would be shorter").

%%
%% Edge turning
%%

turn_edges(ModeFun, Opt, St) ->
    F = fun(Es0, We0) ->
                Es1 = gb_sets:to_list(Es0),
                case wings_we:fully_visible_edges(Es1, We0) of
                    [] ->
                        {We0,gb_sets:empty()};
                    Es ->
                        {We,Sel} = turn_edges(Es, ModeFun, Opt, We0),
                        {We,gb_sets:from_list(Sel)}
                end
        end,
    wings_sel:map_update_sel(F, St).

turn_edges(Edges, ModeFun, Opt, #we{es=Etab}=We0) ->
    validate_edges(Edges, Etab),
    foldl(fun(E0, {W0,Sel}) ->
		  {E,W} = try_turn(E0, ModeFun, Opt, W0),
		  {W,[E|Sel]}
	  end, {We0,[]}, Edges).

try_turn(Edge, ModeFun, Opt, #we{vp=Vtab,he=Htab0}=We0) ->
    {Vstart,Vend,V1,V2} = ModeFun(Edge, We0),
    case optimize(Opt, Vstart, Vend, V1, V2, Vtab) of
    {Vert1,Vert2} ->
        We1 = wings_edge:dissolve_edge(Edge, We0),
        case array:get(Edge, We1#we.es) of
                    #edge{} ->
                      {Edge, We0};
                    undefined ->
                      Vs0 = gb_sets:from_list([Vert1,Vert2]),
                      We = wings_vertex_cmd:connect(Vs0, We1),
                      NewEdge = We1#we.next_id,
                      if
                        NewEdge =/= We#we.next_id ->
                          Htab = check_hard_edges(Edge, NewEdge, Htab0),
                          {NewEdge,We#we{he=Htab}};
                        true ->
                          {Edge,We0}
                      end
        end;
    _ ->
        {Edge,We0}
    end.

check_hard_edges(Edge, NewEdge, Htab0) ->
    case gb_sets:is_member(Edge, Htab0) of
      true ->
          Htab = gb_sets:delete(Edge, Htab0),
          gb_sets:add(NewEdge, Htab);
      false ->
          Htab0
    end.

cw_mode(Edge, #we{es=Etab}) ->
    #edge{vs=Vstart,ve=Vend,rtsu=RNext,ltsu=LNext} = array:get(Edge, Etab),
    RV = wings_vertex:other(Vend, array:get(RNext, Etab)),
    LV = wings_vertex:other(Vstart, array:get(LNext, Etab)),
    {Vstart,Vend,RV,LV}.

ccw_mode(Edge, #we{es=Etab}) ->
    #edge{vs=Vstart,ve=Vend,rtpr=RNext,ltpr=LNext} = array:get(Edge, Etab),
    RV = wings_vertex:other(Vstart, array:get(RNext, Etab)),
    LV = wings_vertex:other(Vend, array:get(LNext, Etab)),
    {Vstart,Vend,RV,LV}.

optimize(false, _, _, Va, Vb, _) -> {Va,Vb};
optimize(true, Evs1, Evs2, V1, V2, Vtab) ->
    Dist1 = e3d_vec:dist(wings_vertex:pos(Evs1, Vtab), wings_vertex:pos(Evs2, Vtab)),
    Dist2 = e3d_vec:dist(wings_vertex:pos(V1, Vtab), wings_vertex:pos(V2, Vtab)),
    if Dist2 < Dist1 -> {V1, V2};
       true -> none
    end.

%% validate_edges([Edge], Etab) -> ok.
%%  Ensure that edges don't share the same face. Cause an error if not.
validate_edges(Edges, Etab) ->
    Faces = ve_collect_edge_faces(Edges, Etab, []),
    SortedFaces = lists:sort(Faces),
    case lists:usort(SortedFaces) of
	SortedFaces -> ok;
	_ -> wings_u:error_msg(?__(1, "Selected edges must not be in the same face."))
    end.
    
ve_collect_edge_faces([E|Es], Etab, Acc) ->
    #edge{lf=Lf,rf=Rf} = array:get(E, Etab),
    ve_collect_edge_faces(Es, Etab, [Lf,Rf|Acc]);
ve_collect_edge_faces([], _, Acc) -> Acc.
