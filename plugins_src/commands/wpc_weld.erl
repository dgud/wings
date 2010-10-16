%%
%%  wpc_weld --
%%
%%     Plug-in for vertex weld
%%
%%  Copyright (c) 2006-2009 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_weld).

-include("wings.hrl").

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
%% Weld one vertex to other
%%

weld(#st{sel=[{Obj,VertSel}],shapes=Shs}=St) ->
   case gb_sets:size(VertSel)==1 of
    true ->
      We = gb_trees:get(Obj, Shs),
      Vertices = wings_util:array_entries(We#we.vp),
      Mirror = We#we.mirror,
      if
         Mirror /= none -> 
            Verts = wings_face:vertices_cw(Mirror,We),
            Vert = gb_sets:smallest(VertSel),
            case member(Vert,Verts) of
               true ->
                  wings_u:error_msg(?__(1,"You cannot weld at mirror plane")),
                  St;
               _ -> ok
            end;
         true -> ok
      end,
      if
         Vertices < 4 -> 
            wings_u:error_msg(?__(2,"Object must have at least 4 vertices")),
            St;
         true ->
            wings:ask(weld_select(St), St, fun weld/2)
      end;
    false ->
      wings_u:error_msg(?__(3,"You can weld only one vertex")),
      St
   end;
weld(St) ->
   wings_u:error_msg(?__(3,"You can weld only one vertex")),
   St.

weld_select(OrigSt) ->
    Desc = ?__(1,"Select target vertex for weld operation (both vertices must share a common edge) "),
    Fun = fun(check, St) -> weld_check_selection(St, OrigSt);
	     (exit, {_,_,#st{sel=Vert2}=St}) ->
		  case weld_check_selection(St, OrigSt) of
		      {_,[]} -> {[],[Vert2]};
		      {_,_} -> error
		  end
	  end,
    {[{Fun,Desc}],[],[],[vertex]}.

weld_check_selection(#st{shapes=Shs,sel=[{Obj,VertSel2}]},#st{sel=[{Obj,VertSel1}]}=St) ->
   case gb_sets:size(VertSel2)==1 andalso gb_sets:size(VertSel1)==1 of
     true ->
      if
         VertSel2==VertSel1 -> {none,?__(1,"You cannot weld a vertex to itself")};
         true -> 
            St2=wings_sel_conv:mode(vertex,St),
            [{_,Sel2}]=St2#st.sel,
            Vert2 = gb_sets:smallest(VertSel2),
            CanDo = gb_sets:is_element(Vert2,Sel2),
            if
               CanDo ->
                 Vert1 = gb_sets:smallest(VertSel1),
                 We = gb_trees:get(Obj,Shs),
                 case wings_vertex:edge_through(Vert1, Vert2, We) of
                   [{_,_,_}] -> {none,""};
                   _Otherwise -> {none,?__(6,"Weld would leave a waist")}
                 end;
               true -> {none,?__(2,"Vertices you want to weld must share edge")}
            end
      end;
     false ->
      {none,?__(3,"You can weld to only one point")}
   end;
weld_check_selection(#st{sel=[{_Obj,_}]},#st{sel=[{_Obj,_}]}) ->
   {none,?__(3,"You can weld to only one point")};
weld_check_selection(#st{sel=[]},_) ->
   {none,?__(4,"Nothing selected")};
weld_check_selection(_,_) ->
   {none,?__(5,"You can weld only in same object")}.

weld([{_,VertSel2}]=NewSel, #st{sel=[{Obj,VertSel1}],shapes=Shs0}=St) ->
    Vremove = gb_sets:smallest(VertSel1),
    Vkeep = gb_sets:smallest(VertSel2),
    We0 = gb_trees:get(Obj, Shs0),

    %% It has already been verified that the vertices share an edge.
    [{Edge,_,_}] = wings_vertex:edge_through(Vkeep, Vremove, We0),

    %% Collapse the edge going through the vertices,
    %% keeping vertex Vkeep but at the wrong position.
    We1 = wings_collapse:collapse_edge(Edge, Vkeep, We0),

    %% Move back vertex Vkeep to its original position.
    Pos = wings_vertex:pos(Vkeep, We0),
    We = We1#we{vp=array:set(Vkeep, Pos, We1#we.vp)},

    Shs = gb_trees:update(Obj, We, Shs0),
    {save_state,St#st{shapes=Shs,sel=NewSel}}.
