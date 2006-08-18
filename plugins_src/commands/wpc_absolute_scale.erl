%%
%%  wpc_absolute_scale.erl --
%%
%%     Plug-in for scale -> absolute
%%
%%  Copyright (c) 2006 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_absolute_scale.erl,v 1.2 2006/07/24 19:39:12 giniu Exp $
%%
-module(wpc_absolute_scale).

-include("wings.hrl").

-export([init/0,menu/2,command/2]).

-define(EPSILON,0.0000005).

%%%
%%% plugin interface
%%%

init() -> true.

menu({Mode},Menu) when Mode == vertex; Mode == edge; Mode == face; Mode == body -> 
    parse(Menu);
menu(_,Menu) -> 
    Menu.

parse(Menu) -> 
    lists:reverse(parse(Menu, [], false)).

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [draw(all), separator|NewMenu];
parse([{Name, {absolute, Commands}}|Rest], NewMenu, false) ->
    parse(Rest, [{Name, {absolute, Commands++[draw(menu)]}}|NewMenu], true);
parse([separator|Rest], NewMenu, false) ->
    parse(Rest, [separator, draw(all)|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

draw(all) ->
    {?__(1, "Absolute commands"), {absolute, [draw(menu)]}};
draw(menu) ->
     {?__(2,"Scale"),scale,
      ?__(3,"Scale to exact size in absolute coordinates.")}.

command({_,{absolute,scale}},St) ->
    abs_scale(St);
command(_,_) -> next.

abs_scale(St) ->
    {Options,Selection} = extract(St),
    draw_window(Options, Selection, St).

%%
%% extract(State)
%%
%% functions that extracts all needed information from state
%%  it returns {Options,Selection}, where options is:
%%   {{center,{CX,CY,CZ}},{size,{SX,SY,SZ}},{scalewhole,WholeObject},{oneobject,OneObject}}
%%  (return values: {{atom,{float,float,float}},{atom,{float,float,float}},{atom,atom(always/never/ask)},{atom,bool}})
%%

extract(#st{shapes=Shapes}=St) ->
    Sel = get_selection(St),
    {Center,Size0} = get_center_and_size(Sel,Shapes),
    WholeObject = case Sel of
                      [{_,{1,_}}] -> always;
                      _ -> case check_whole_obj(St#st{sel=Sel,selmode=vertex}) of
                               true -> never;
                               _ -> ask
                      end
                  end,
    Size = if
               WholeObject == always -> 
                   [{Obj,_}] = Sel,
                   We = gb_trees:get(Obj, Shapes),
                   BB = wings_vertex:bounding_box(We),
                   bb2size(BB);
               true -> Size0
           end,
    OneObject = check_single_obj(Sel),
    {{{center,Center},{size,Size},{scalewhole,WholeObject},{oneobject,OneObject}},Sel}.

get_selection(#st{selmode=SelMode}=St) ->
    #st{sel=Sel} = case SelMode of
        vertex -> St;
        _ -> wings_sel_conv:mode(vertex,St)
    end,
    Sel.

get_center_and_size(Sel,Shapes) ->
    get_center_and_size(Sel,Shapes,[]).

get_center_and_size([],_,Now) ->
    BB = e3d_vec:bounding_box(Now),
    {e3d_vec:average(Now),bb2size(BB)};
get_center_and_size([{Obj,Vset}|Rest],Shapes,Now) ->
    We = gb_trees:get(Obj, Shapes),
    Positions = gb_sets:fold(fun(Vert, Acc) ->
                    [wings_vertex:pos(Vert, We)|Acc]
                end, [], Vset),
    get_center_and_size(Rest,Shapes,Now++Positions).

bb2size([{X1,Y1,Z1},{X2,Y2,Z2}]) ->
    {abs(X1-X2),abs(Y1-Y2),abs(Z1-Z2)}.

check_whole_obj(St0) ->
    St1 = wings_sel_conv:mode(body,St0),
    St2 = wings_sel_conv:mode(vertex,St1),
    St2 == St0.

check_single_obj([{_,_}]) -> true;
check_single_obj(_) -> false.

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls do_scale(ProcessedOptions,Selection,State)
%%

draw_window({{_,{CX,CY,CZ}}, {_,{SX,SY,SZ}=Size}, {_,Whole}, {_,Single}}, Sel, St) ->
    Frame1 = [{hframe,
                 [draw_window1(size, {SX,SY,SZ}),
                  draw_window1(center, {CX,CY,CZ})]}],
    Frame2 = if
                  Whole == ask -> [draw_window1(whole, Single)];
                  true -> []
              end,
    Frame3 = [{vframe, Frame1 ++ Frame2}],
    Name = draw_window1(name,default),
    wings_ask:dialog(Name, Frame3,
       fun(Scale) ->
           translate(Scale,Size,Sel,St)
       end).

draw_window1(name,_) ->
    ?__(1,"Absolute scale options");
draw_window1(size, {X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set new size")++":"}]},
        {hframe,[{label,"X:"},{text,X,[disable(eq,X,0.0)]}]},
        {hframe,[{label,"Y:"},{text,Y,[disable(eq,Y,0.0)]}]},
        {hframe,[{label,"Z:"},{text,Z,[disable(eq,Z,0.0)]}]}
    ]};
draw_window1(center,{X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,?__(3,"Set scale center")++":"}]},
        {hframe,[{label,"X:"},{text,X}]},
        {hframe,[{label,"Y:"},{text,Y}]},
        {hframe,[{label,"Z:"},{text,Z}]}
    ]};
draw_window1(whole,true) ->
    {?__(4,"Scale whole object"),false,[{key,whole}]};
draw_window1(whole,false) ->
    {?__(5,"Scale whole objects"),false,[{key,whole}]}.

disable(eq,X,Value) ->
    {hook, fun (is_disabled, _) -> X == Value;
               (_, _) -> void
           end}.

translate([SX,SY,SZ,CX,CY,CZ],{OX,OY,OZ},Sel,St) ->
    do_scale([{SX,SY,SZ},{OX,OY,OZ},{CX,CY,CZ},{whole,true}],Sel,St);
translate([SX,SY,SZ,CX,CY,CZ,{whole,Whole}],{OX,OY,OZ},Sel,St) ->
    do_scale([{SX,SY,SZ},{OX,OY,OZ},{CX,CY,CZ},{whole,Whole}],Sel,St).

%%
%% do_scale(Options,Selection,State)
%%
%% this is main absolute scale command, it returns new state.
%%

do_scale([{SX,SY,SZ},{OX,OY,OZ},{CX,CY,CZ},{_,Whole}], Sel, St) ->
    SX2 = if
        OX == 0.0 -> 1.0;
        Whole andalso (OX =< ?EPSILON) andalso (SX =< ?EPSILON) -> 1.0;
        true -> SX/OX
    end,
    SY2 = if
        OY == 0.0 -> 1.0;
        Whole andalso (OY =< ?EPSILON) andalso (SY =< ?EPSILON) -> 1.0;
        true -> SY/OY
    end,
    SZ2 = if
        OZ == 0.0 -> 1.0;
        Whole andalso (OZ =< ?EPSILON) andalso SZ =< ?EPSILON -> 1.0;
        true -> SZ/OZ
    end,
    {TX,TY,TZ} = {CX - CX*SX2, CY - CY*SY2, CZ - CZ*SZ2},
    NewSel = if
               Whole ->
                   St1 = wings_sel_conv:mode(body,St),
                   St2 = wings_sel_conv:mode(vertex,St1),
                   St2#st.sel;
               true ->
                   Sel
           end,
    do_scale1({SX2,SY2,SZ2},{TX,TY,TZ},NewSel,St).

do_scale1(_,_,[],St) -> St;
do_scale1(Scale, Transform, [{WeId,Vset}|Rest], #st{shapes=Shapes}=St) ->
    We = gb_trees:get(WeId, Shapes),
    Vtab = We#we.vp,
    Empty = gb_trees:empty(),
    NewVtab = execute_scale(Scale, Transform, Vset, Vtab, Empty),
    NewWe = We#we{vp=NewVtab},
    NewShapes = gb_trees:update(WeId,NewWe,Shapes),
    NewSt = St#st{shapes=NewShapes},
    do_scale1(Scale, Transform, Rest, NewSt).

execute_scale({SX,SY,SZ},{TX,TY,TZ}, Vset, Vtab, Now) ->
    case gb_trees:size(Vtab) of
        0 -> Now;
        _ ->
            {Vertex,{X,Y,Z},Vtab2} = gb_trees:take_smallest(Vtab),
            NewXYZ = case gb_sets:is_element(Vertex,Vset) of
                true -> {TX + SX*X, TY + SY*Y, TZ + SZ*Z};
                _ -> {X,Y,Z}
            end,
            NewNow = gb_trees:insert(Vertex,NewXYZ,Now),
            execute_scale({SX,SY,SZ},{TX,TY,TZ}, Vset, Vtab2, NewNow)
    end.
