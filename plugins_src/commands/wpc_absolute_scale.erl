%%
%%  wpc_absolute_scale.erl --
%%
%%     Plug-in for scale -> absolute
%%
%%  Copyright (c) 2006-2011 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
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
    parse(Menu, Mode);
menu(_,Menu) -> 
    Menu.

parse(Menu, Mode) -> 
    lists:reverse(parse(Menu, Mode, [], false)).

parse([], _, NewMenu, true) ->
    NewMenu;
parse([], Mode, NewMenu, false) ->
    [draw(all, Mode), separator|NewMenu];
parse([{Name, {absolute, Commands}}|Rest], Mode, NewMenu, false) ->
    parse(Rest, Mode, [{Name, {absolute, Commands++draw(menu, Mode)}}|NewMenu], true);
parse([separator|Rest], Mode, NewMenu, false) ->
    parse(Rest, Mode, [separator, draw(all, Mode)|NewMenu], true);
parse([Elem|Rest], Mode, NewMenu, Found) ->
    parse(Rest, Mode, [Elem|NewMenu], Found).

draw(all, Mode) ->
    {?__(1, "Absolute Commands"), {absolute, draw(menu, Mode)}};
draw(menu, Mode) ->
    [{?__(2,"Scale"),scale_fun(Mode),
     {?__(3,"Scale to exact size in absolute coordinates."),
      ?__(4,"Scale to target size."),
      ?__(5,"Scale with centre picking.")},[]}].

scale_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{absolute,scale}};
       (2, _Ns) ->
	    {Mode,{absolute,ctscale}};
       (3, _Ns) ->
	    {Mode,{absolute,cscale}};
       (_, _) -> ignore
    end.

command({_,{absolute,Mode}},St) when Mode == scale; Mode == ctscale; Mode == cscale ->
            case Mode of
                scale -> scale(St);
                ctscale -> wings:ask(selection_ask([centre,target]), St, fun ctscale/2);
                cscale -> wings:ask(selection_ask([centre]), St, fun cscale/2)
            end;
command(_,_) -> next.

scale(St) ->
    {Options,Sel} = extract([],St),
    draw_window(Options, Sel, St).

ctscale({Center, TA, TB}, St) ->
    {Options, Sel} = extract([{center,Center},{ta,TA},{tb,TB}],St),
    draw_window(Options, Sel, St).

cscale(Center, St) ->
    {Options, Sel} = extract([{center,Center}],St),
    draw_window(Options, Sel, St).

%%
%% extract(State)
%%
%% functions that extracts all needed information from state
%%  it returns {Options,Selection}, where options is:
%%   {{center,{CX,CY,CZ}},{size,{SX,SY,SZ}},{scalewhole,WholeObject},{oneobject,OneObject}}
%%  (return values: {{atom,{float,float,float}},{atom,{float,float,float}},{atom,atom(always/never/ask)},{atom,bool}})
%%

extract(Options, #st{shapes=Shapes}=St) ->
    Sel = get_selection(St),
    {Center,Size0,BB0} = get_center_and_size(Sel,Shapes),
    WholeObject = case Sel of
                      [{_,GbSet}] ->
			  case gb_sets:size(GbSet) of
			      1 -> always;
			      _ -> check_whole_obj(St#st{sel=Sel,selmode=vertex})
			  end;
                      _ -> check_whole_obj(St#st{sel=Sel,selmode=vertex})
                  end,
    {BB,Size} = if
               WholeObject == always -> 
                   [{Obj,_}] = Sel,
                   We = gb_trees:get(Obj, Shapes),
                   BB1 = wings_vertex:bounding_box(We),
                   {normalizeBB(BB1),bb2size(BB1)};
               true -> 
                   {normalizeBB(BB0),Size0}
           end,
    TA = lookup(ta, Options, {0.0, 0.0, 0.0}),
    TB = lookup(tb, Options, Size),
    [{TX1,TY1,TZ1},{TX2,TY2,TZ2}] = TBB = normalizeBB([TA,TB]),
    NCenter = lookup(center, Options, Center),
    OneObject = check_single_obj(Sel),
    SugSize = {abs(TX1-TX2),abs(TY1-TY2),abs(TZ1-TZ2)},
    SugCenter = case lookup(ta, Options, false) of
                    false -> none;
                    _ -> getSugestedCenter(BB, TBB)
                end,
    {{{center,NCenter},{sugestcenter,SugCenter},{size,Size},{sugestsize,SugSize},{scalewhole,WholeObject},{oneobject,OneObject}},Sel}.

lookup(Key, List, Default) ->
   case lists:keysearch(Key, 1, List) of
      {value,{_,Value}} -> Value;
      _ -> Default
   end.

normalizeBB([{AX1,AY1,AZ1},{AX2,AY2,AZ2}]) ->
    [{min(AX1,AX2), min(AY1,AY2), min(AZ1, AZ2)},
     {max(AX1,AX2), max(AY1,AY2), max(AZ1, AZ2)}].

getSugestedCenter([{AX1,AY1,AZ1},{AX2,AY2,AZ2}], 
                  [{BX1,BY1,BZ1},{BX2,BY2,BZ2}]) -> 
    {getC(AX1,AX2,BX1,BX2),
     getC(AY1,AY2,BY1,BY2),
     getC(AZ1,AZ2,BZ1,BZ2)}.

getC(A1, A2, B1, B2) when A1 =/= A2, B1 =/= B2 ->
    (B1*A2-B2*A1)/((A2-A1) - (B2-B1));
getC(A1, A2, _, _) -> (A1+A2)/2.

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
    {e3d_vec:average(Now),bb2size(BB),BB};
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
    case St2 == St0 of
	true -> never;
	false -> ask
    end.

check_single_obj([{_,_}]) -> true;
check_single_obj(_) -> false.

selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face, body]}.

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([centre|Rest],Ask) ->
    Desc = ?__(1,"Select scale centre"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([target|Rest],Ask) ->
    Desc1 = ?__(2,"Select target size - base point"),
    Desc2 = ?__(3,"Select target size - range point"),
    selection_ask(Rest,[{point,Desc2},{point,Desc1}|Ask]).

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls do_scale(ProcessedOptions,Selection,State)
%%

draw_window({{_,{CX,CY,CZ}}, {_, SugCenter}, {_,{SX,SY,SZ}=Size}, {_, {SugX, SugY, SugZ}}, {_,Whole}, {_,Single}}, Sel, St) ->
    Frame1 = [{hframe,
                 [draw_window1(size, {{SX,SY,SZ},{SugX,SugY,SugZ}}),
                  draw_window1(aspect, {SX,SY,SZ}),
                  draw_window1(center, {CX,CY,CZ})]}],
    Frame2 = if
                  Whole == ask -> [draw_window1(whole, Single)];
                  true -> []
              end,
    Frame3 = if
                  SugCenter == none -> [];
                  true -> [draw_window1(sugc, true)]
              end,
    Frame = [{vframe, Frame1 ++ Frame2 ++ Frame3}],
    Name = draw_window1(name,default),
    wings_ask:dialog(Name, {{preview,ungrab},Frame},
       fun
           ({dialog_preview,Scale}) ->
               {preview,St,translate(Scale,SugCenter,Size,Sel,St)};
           (cancel) -> St;
           (Scale) ->
               {commit,St,translate(Scale,SugCenter,Size,Sel,St)}
       end).

draw_window1(name,_) ->
    ?__(1,"Absolute scale options");
draw_window1(size, {{X,Y,Z},{SugX,SugY,SugZ}}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set new size")++":"}]},
        {hframe,[{label,"X:"},{text,SugX,[{key,sx},disable(X,0.0)]}]},
        {hframe,[{label,"Y:"},{text,SugY,[{key,sy},disable(Y,0.0)]}]},
        {hframe,[{label,"Z:"},{text,SugZ,[{key,sz},disable(Z,0.0)]}]}
    ]};
draw_window1(center,{X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,?__(3,"Set scale center")++":"}]},
        {hframe,[{label,"X:"},{text,X,[{key,cx},disable(sugc,true)]}]},
        {hframe,[{label,"Y:"},{text,Y,[{key,cy},disable(sugc,true)]}]},
        {hframe,[{label,"Z:"},{text,Z,[{key,cz},disable(sugc,true)]}]}
    ]};
draw_window1(aspect,{X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,?__(6,"Link")++":"}]},
        {hframe,[{"",false,[{key,ax},disable(X,0.0)]}]},
        {hframe,[{"",false,[{key,ay},disable(Y,0.0)]}]},
        {hframe,[{"",false,[{key,az},disable(Z,0.0)]}]}
    ]};
draw_window1(whole,true) ->
    {?__(4,"Scale whole object"),false,[{key,whole}]};
draw_window1(whole,false) ->
    {?__(5,"Scale whole objects"),false,[{key,whole}]};
draw_window1(sugc,_) ->
    {?__(7,"Fit selection to target"),false,[{key,sugc}]}.

disable(sugc,_) ->
    {hook, fun (is_disabled, {_Var,_I,Store}) ->
                   gb_trees:is_defined(sugc,Store) andalso gb_trees:get(sugc, Store);
               (_, _) -> void
           end};
disable(X,Value) ->
    {hook, fun (is_disabled, _) -> X == Value;
               (_, _) -> void
           end}.

checkChained(Data) -> 
    [{_,Factor}|_]=lists:keysort(1,checkChained(Data, [{0.0,1.0}])),
    Factor.
checkChained([], List) -> List;
checkChained([{SX,OX,true}|Rest], List) ->
    checkChained(Rest, [{-abs(SX/OX-1.0),SX/OX}|List]);
checkChained([_|Rest], List) ->
    checkChained(Rest, List).

translate(Options,SugCenter,{OX,OY,OZ}=Original,Sel,St) ->
    SX = lookup(sx, Options, OX),
    SY = lookup(sy, Options, OY),
    SZ = lookup(sz, Options, OZ),
    CX = lookup(cx, Options, 0.0),
    CY = lookup(cy, Options, 0.0),
    CZ = lookup(cz, Options, 0.0),
    AX = lookup(ax, Options, false),
    AY = lookup(ay, Options, false),
    AZ = lookup(az, Options, false),
    Whole = lookup(whole, Options, true),
    SugC = lookup(sugc, Options, false),
    ChainedFactor = checkChained([{SX,OX,AX},{SY,OY,AY},{SZ,OZ,AZ}]),
    NX = if
             AX -> OX*ChainedFactor;
             true -> SX
         end,
    NY = if
             AY -> OY*ChainedFactor;
             true -> SY
         end,
    NZ = if
             AZ -> OZ*ChainedFactor;
             true -> SZ
         end,
    Center = if
                 SugC -> SugCenter;
                 true -> {CX,CY,CZ}
             end,
    do_scale([{NX,NY,NZ},Original,Center,{whole,Whole}],Sel,St).

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
    NewVtab = execute_scale(Scale, Transform, Vset, Vtab),
    NewWe = We#we{vp=NewVtab},
    NewShapes = gb_trees:update(WeId,NewWe,Shapes),
    NewSt = St#st{shapes=NewShapes},
    do_scale1(Scale, Transform, Rest, NewSt).

execute_scale(S, T, Vset, Vtab) ->
    execute_scale(array:sparse_size(Vtab)-1, S, T, Vset, Vtab).

execute_scale(-1, _, _, _, Vtab) ->
    Vtab;
execute_scale(Vertex, {SX,SY,SZ}=S, {TX,TY,TZ}=T, Vset, Vtab0) ->
    case array:get(Vertex, Vtab0) of
	undefined ->
	    execute_scale(Vertex-1, S, T, Vset, Vtab0);
	{X,Y,Z} ->
            Vtab = case gb_sets:is_element(Vertex, Vset) of
		       true ->
			   NewXYZ = {TX + SX*X, TY + SY*Y, TZ + SZ*Z},
			   array:set(Vertex, NewXYZ, Vtab0);
		       false ->
			   Vtab0
		   end,
            execute_scale(Vertex-1, S, T, Vset, Vtab)
    end.
