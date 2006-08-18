%%
%%  wpc_turnedge.erl --
%%
%%     Plug-in for turning edges
%%
%%  Copyright (c) 2002 Chris Osgood,
%%		  2003 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_turnedge.erl,v 1.7 2006/07/10 18:28:18 giniu Exp $
%%

-module(wpc_turnedge).

-export([init/0,menu/2,command/2]).

-include_lib("wings.hrl").

init() ->
    init_pref(),
    true.

menu({edge}, Menu) ->
    case is_enabled() of
	true -> menu_entry(Menu);
	false -> Menu
    end;
menu({edit,plugin_preferences}, Menu) ->
    Menu ++ [{?__(1,"Turn Edge"),turn_edge}];
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    case wings_pref:get_value(advanced_menus) of
	true ->
	    TurnMenu = turns(),
	    Menu ++ [separator,
		     {?__(1,"Turn"),{turn,TurnMenu}}];
	false ->
	    Menu ++ [separator,
		     {?__(2,"Turn"),turn,
		      ?__(3,"Turn edge")},
		     {?__(4,"Optimized Turn"),optimized_turn,
		      ?__(5,"Turn edge only if new edge length would be shorter")}]
    end.

command({edge,turn}, St0) ->
    {Sel, St} = wpa:sel_fold(
		  fun(Edges, We, {Sel,StAcc}) ->
			  {Sel1, StAcc0} = turn_edges(gb_sets:to_list(Edges),
						      We, false, StAcc),
			  {[Sel1|Sel], StAcc0}
		  end, {[],St0}, St0),
    St#st{sel=Sel};
command({edge,optimized_turn}, St0) ->
    {Sel, St} = wpa:sel_fold(
		  fun(Edges, We, {Sel, StAcc}) ->
			  {Sel1, StAcc0} = turn_edges(gb_sets:to_list(Edges),
						      We, true, StAcc),
			  {[Sel1|Sel], StAcc0}
		  end, {[],St0}, St0),
    St#st{sel=Sel};
command({edit,{plugin_preferences,turn_edge}}, St) ->
    pref_edit(St);
command(_Cmd, _) -> next.

turns() ->
    fun(B, _Ns) ->
	    turn_menu(B)
    end.

turn_menu(1) -> {edge,turn};
turn_menu(3) -> {edge,optimized_turn};
turn_menu(help) -> turn_help();
turn_menu(_) -> ignore.

turn_help() ->
    {?__(1,"Turn edge"), "", ?__(2,"Turns edge only if new edge length would be shorter")}.

%%
%% Edge turning
%%

turn_edges(Edges, #we{id=Id}=We0, Opt, St) ->
    {Sel0, We} = lists:foldl(
		   fun(Edge, {Sel0, WeAcc}) ->
			   {NewEdge, We1} = try_turn(Edge, WeAcc, Opt),
			   {[NewEdge|Sel0], We1}
		   end, {[],We0}, Edges),
    Shapes = gb_trees:enter(Id, We, St#st.shapes),
    Sel = {Id,gb_sets:from_list(Sel0)},
    {Sel, St#st{shapes=Shapes}}.

try_turn(Edge, #we{vp=Vtab}=We0, Opt) ->
    {Vstart, Vend, V1, V2} = choosemode(Edge,We0),
    case optimize(Vstart, Vend, V1, V2, Opt, Vtab) of
	{Vert1, Vert2} ->
	    We1 = wings_edge:dissolve_edge(Edge, We0),
	    case gb_trees:is_defined(Edge, We1#we.es) of
                true -> {Edge, We0};
                false ->
                    Vs0 = gb_sets:from_list([Vert1, Vert2]),
                    We = wings_vertex_cmd:connect(Vs0, We1),
                    if 
                        We1#we.next_id =/= We#we.next_id ->
                            {We1#we.next_id, We};
                        true -> {Edge, We0}
                    end
	    end;
	_ ->
	    {Edge, We0}
    end.

choosemode(Edge,#we{es=Etab}=We0) ->
    case get_pref(mode, turn) of
        turn ->
            #edge{vs=Vstart,ve=Vend,rf=RightFace,lf=LeftFace} = gb_trees:get(Edge, Etab),
            LeftVs0 = wings_face:to_vertices([LeftFace], We0),
            RightVs0 = wings_face:to_vertices([RightFace], We0),
            LeftVs = lists:delete(Vstart, lists:delete(Vend, LeftVs0)),
            RightVs = lists:delete(Vstart, lists:delete(Vend, RightVs0)),
            V1 = hd(LeftVs),
            V2 = hd(RightVs),
            {Vstart,Vend,V1,V2};
        cw ->
            #edge{vs=Vstart,ve=Vend,rtsu=RNext,ltsu=LNext} = gb_trees:get(Edge, Etab),
            RV = wings_vertex:other(Vend,gb_trees:get(RNext,Etab)),
            LV = wings_vertex:other(Vstart,gb_trees:get(LNext,Etab)),
            {Vstart,Vend,RV,LV};
        ccw ->
            #edge{vs=Vstart,ve=Vend,rtpr=RNext,ltpr=LNext} = gb_trees:get(Edge, Etab),
            RV = wings_vertex:other(Vstart,gb_trees:get(RNext,Etab)),
            LV = wings_vertex:other(Vend,gb_trees:get(LNext,Etab)),
            {Vstart,Vend,RV,LV}
    end.

optimize(Evs1, Evs2, V1, V2, Opt, Vtab) ->
    Dist1 = e3d_vec:dist(wings_vertex:pos(Evs1, Vtab), wings_vertex:pos(Evs2, Vtab)),
    Dist2 = e3d_vec:dist(wings_vertex:pos(V1, Vtab), wings_vertex:pos(V2, Vtab)),
    case Opt of 
        true when Dist2 < Dist1 -> {V1, V2};
        true -> none;
        false -> {V1, V2}
    end.

%%%
%%% Preference support.
%%%

pref_edit(St) ->
    Enabled = get_pref(enabled, false),
    wpa:dialog(?__(1,"Turn Edge Preferences"),
	       [{vframe,[{?__(2,"Enabled"),Enabled,[{key,enabled}]},
	                 {label,?__(3,"Turn mode")++":"},
	                 {vradio,[{?__(4,"Turn Edge"),turn},
	                          {?__(5,"Rotate clockwise"),cw},
	                          {?__(6,"Rotate counterclockwise"),ccw}],
	                          get_pref(mode,turn), 
	                         [{key,mode},
	                          {hook,fun (is_disabled, {_Var,_I,Store}) ->
                                                not gb_trees:get(enabled, Store);
                                            (_, _) -> void
                                        end}]}]}],
	                 fun(Attr) -> pref_result(Attr, St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.

set_pref(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

init_pref() ->
    Enabled = get_pref(enabled, false),
    put({?MODULE,enabled}, Enabled),
    case Enabled of
	true ->
	    wpa:bind_unicode($t, {edge,turn}),
	    wpa:bind_unicode($T, {edge,optimized_turn});
	false -> ok
    end.

is_enabled() -> get({?MODULE,enabled}).
