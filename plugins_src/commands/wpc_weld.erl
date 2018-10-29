%%
%%  wpc_weld --
%%
%%     Plug-in for vertex weld
%%
%%  Copyright (c) 2006-2011 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_weld).

-include_lib("wings/src/wings.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [member/2,foldl/3]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({vertex},Menu) ->
    parse(Menu,[],false);
menu(_,Menu) -> Menu.

parse([],MenuNew,true) ->
    MenuNew;
parse([],MenuNew,false) ->
    MenuNew ++ [separator|draw_menu()];
parse([{_,dissolve,_}=Diss|Rest],MenuNew,_) ->
    MenuNew2 = MenuNew ++ [Diss],
    [Now|Rest2] = Rest,
    case Now of
        {_,collapse,_} ->
            MenuNew3 = MenuNew2 ++ [Now] ++ draw_menu();
        _ ->
            MenuNew3 = MenuNew2 ++ draw_menu() ++ [Now]
    end,
    parse(Rest2,MenuNew3,true);
parse([{_,collapse,_}=Coll|Rest],MenuNew,_) ->
    MenuNew2 = MenuNew ++ [Coll] ++ draw_menu(),
    parse(Rest,MenuNew2,true);
parse([First|Rest],MenuNew,State) ->
    MenuNew2 = MenuNew ++ [First],
    parse(Rest,MenuNew2,State).

draw_menu() ->
    [{?__(1,"Weld"),weld,
      ?__(2,"Weld selected vertex to another (sharing common edge)")}].

command({vertex,weld}, St) ->
    weld(St);
command(_,_) -> next.

%%
%% Weld one vertex to another.
%%

weld(St) ->
    MirrorVertex = ?__(1,"You cannot weld a mirror vertex"),
    AtLeastFour = ?__(2,"Object must have at least 4 vertices"),
    OnlyOne = ?__(3,"You can weld only one vertex"),
    MF = fun(Vs, #we{id=Id,vp=Vpos}=We) ->
                 case gb_sets:size(Vs) of
                     1 -> ok;
                     _ -> wings_u:error_msg(OnlyOne)
                 end,
                 [V] = gb_sets:to_list(Vs),
                 case is_mirror_vs(V, We) of
                     true -> wings_u:error_msg(MirrorVertex);
                     false -> ok
                 end,
                 case array:size(Vpos) < 4 of
                     true -> wings_u:error_msg(AtLeastFour);
                     false -> ok
                 end,
                 [{Id,V}]
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St) of
        [Sel] ->
            wings:ask(weld_select(Sel), St, fun weld/2);
        _ ->
            wings_u:error_msg(OnlyOne)
    end.

is_mirror_vs(_, #we{mirror=none}) ->
    false;
is_mirror_vs(V, #we{mirror=Mirror}=We) ->
    member(V, wings_face:vertices_cw(Mirror, We)).

weld_select(Sel) ->
    Desc = ?__(1,"Select target vertex for weld operation (vertices must share an edge)"),
    Fun = fun(check, St) ->
                  case weld_check_selection(Sel, St) of
                      {ok,_} ->
                          {none,""};
                      Error ->
                          {none,Error}
                  end;
	     (exit, {_,_,St}) ->
		  case weld_check_selection(Sel, St) of
                      {ok,VE} -> {[],[VE]};
		      _ -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[vertex]}.

weld_check_selection({OrigId,OrigV}, St) ->
    MF = fun(Vs, #we{id=Id}=We) when Id =:= OrigId ->
                 case gb_sets:to_list(Vs) of
                     [V] ->
                         weld_check_selection_1(V, OrigV, We);
                     [V|_] ->
                         [V,V]
                 end;
            (_, _) ->
                 [wrong_object]
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St) of
        [] ->
            ?__(4,"Nothing selected");
        [wrong_object] ->
            ?__(5,"You can weld only in the same object");
        [waist] ->
            ?__(6,"Weld would leave a waist");
        [no_shared_edge] ->
            ?__(2,"Vertices you want to weld must share an edge");
        [{_,_}=VE] ->
            {ok,VE};
        [_,_|_] ->
            ?__(3,"You can weld to only one point")
    end.

weld_check_selection_1(VKeep, OrigV, We) ->
    case wings_vertex:edge_through(OrigV, VKeep, We) of
        [{Edge,_,_}] ->
            [{VKeep,Edge}];
        [_,_|_] ->
            [waist];
        [] ->
            [no_shared_edge]
    end.

weld({VKeep,Edge}, St) ->
    MF = fun(_Vs, We0) ->
                 %% Collapse the edge going through the vertices,
                 %% keeping vertex VKeep but at the wrong position.
                 We1 = wings_collapse:collapse_edge(Edge, VKeep, We0),

                 %% Move back vertex VKeep to its original position.
                 Pos = wings_vertex:pos(VKeep, We0),
                 We = We1#we{vp=array:set(VKeep, Pos, We1#we.vp)},
                 {We,gb_sets:singleton(VKeep)}
         end,
    {save_state,wings_sel:map_update_sel(MF, St)}.
