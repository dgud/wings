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

-include_lib("wings/src/wings.hrl").

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
     {?__(3,"Scale to exact size in absolute coordinates"),
      ?__(4,"Scale to target size"),
      ?__(5,"Scale with center picking")},[]}].

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
        scale ->
            scale(St);
        ctscale ->
            wings:ask(selection_ask([center,target]), St, fun ctscale/2);
        cscale ->
            wings:ask(selection_ask([center]), St, fun cscale/2)
    end;
command(_,_) -> next.

scale(St) ->
    Options = extract([], St),
    draw_window(Options, St).

ctscale({Center, TA, TB}, St) ->
    Options = extract([{center,Center},{ta,TA},{tb,TB}], St),
    draw_window(Options, St).

cscale(Center, St) ->
    Options = extract([{center,Center}], St),
    draw_window(Options, St).

%%
%% extract(State)
%%
%% functions that extracts all needed information from state
%%  it returns {Options,Selection}, where options is:
%%   {{center,{CX,CY,CZ}},{size,{SX,SY,SZ}},{scalewhole,WholeObject},{oneobject,OneObject}}
%%  (return values: {{atom,{float,float,float}},{atom,{float,float,float}},{atom,atom(always/never/ask)},{atom,bool}})
%%

extract(Options, St) ->
    Center = wings_sel:center_vs(St),
    WholeObject = check_whole_obj(St),
    BB = case check_whole_obj(St) of
             always -> whole_bbox(St);
             _ -> wings_sel:bounding_box(St)
         end,
    Size = bb2size(BB),
    TA = lookup(ta, Options, {0.0, 0.0, 0.0}),
    TB = lookup(tb, Options, Size),
    [{TX1,TY1,TZ1},{TX2,TY2,TZ2}] = TBB = normalizeBB([TA,TB]),
    NCenter = lookup(center, Options, Center),
    OneObject = check_single_obj(St),
    SugSize = {abs(TX1-TX2),abs(TY1-TY2),abs(TZ1-TZ2)},
    SugCenter = case lookup(ta, Options, false) of
                    false -> none;
                    _ -> getSuggestedCenter(BB, TBB)
                end,
    {{center,NCenter},{suggestcenter,SugCenter},{size,Size},
     {suggestsize,SugSize},{scalewhole,WholeObject},{oneobject,OneObject}}.

whole_bbox(St) ->
    MF = fun(_, We) -> wings_vertex:bounding_box(We) end,
    RF = fun(W, []) -> W end,
    wings_sel:dfold(MF, RF, [], St).

lookup(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
       {_,Value} -> Value;
       false -> Default
   end.

normalizeBB([{AX1,AY1,AZ1},{AX2,AY2,AZ2}]) ->
    [{min(AX1,AX2), min(AY1,AY2), min(AZ1, AZ2)},
     {max(AX1,AX2), max(AY1,AY2), max(AZ1, AZ2)}].

getSuggestedCenter([{AX1,AY1,AZ1},{AX2,AY2,AZ2}],
                   [{BX1,BY1,BZ1},{BX2,BY2,BZ2}]) ->
    {getC(AX1,AX2,BX1,BX2),
     getC(AY1,AY2,BY1,BY2),
     getC(AZ1,AZ2,BZ1,BZ2)}.

getC(A1, A2, B1, B2) when A1 =/= A2, B1 =/= B2, ((A1 - A2)-(B1 - B2))=/=0.0 ->
    (B1*A2-B2*A1)/((A2-A1) - (B2-B1));
getC(A1, A2, _, _) -> (A1+A2)/2.

bb2size([LL,UR]) ->
    e3d_vec:sub(UR, LL).

check_whole_obj(#st{selmode=Mode}=St0) ->
    MF = fun(Items, We) ->
                 Vs = wings_sel:to_vertices(Mode, Items, We),
                 case {Vs,wings_we:visible_vs(We)} of
                     {[_],_} -> always;
                     {Vs,Vs} -> never;
                     {_,_} -> ask
                 end
         end,
    RF = fun(When, never) -> When;
            (_, _) -> ask
         end,
    wings_sel:dfold(MF, RF, never, St0).

check_single_obj(#st{sel=[_]}) -> true;
check_single_obj(_) -> false.

selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face, body]}.

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([center|Rest], Ask) ->
    Desc = ?__(1,"Select scale center"),
    selection_ask(Rest, [{point,Desc}|Ask]);
selection_ask([target|Rest], Ask) ->
    Desc1 = ?__(2,"Select target size - base point"),
    Desc2 = ?__(3,"Select target size - range point"),
    selection_ask(Rest, [{point,Desc2},{point,Desc1}|Ask]).

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls do_scale(ProcessedOptions,Selection,State)
%%

draw_window({{_,{CX,CY,CZ}}, {_, SugCenter}, {_,{SX,SY,SZ}=Size}, {_, {SugX, SugY, SugZ}}, {_,Whole}, {_,Single}}, St) ->
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
    F = fun({dialog_preview,Scale}) ->
                {preview,St,translate(Scale,SugCenter, Size, St)};
           (cancel) ->
                St;
           (Scale) ->
                {commit,St,translate(Scale, SugCenter, Size, St)}
        end,
    wings_dialog:dialog(Name, {preview,Frame}, F).

draw_window1(name,_) ->
    ?__(1,"Absolute scale options");
draw_window1(size, {{X,Y,Z},{SugX,SugY,SugZ}}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set new size")++":"}]},
        {label_column,[
            {"X:",{text,SugX,[{key,sx},disable(X==0.0)]}},
            {"Y:",{text,SugY,[{key,sy},disable(Y==0.0)]}},
            {"Z:",{text,SugZ,[{key,sz},disable(Z==0.0)]}}
        ]}
    ]};
draw_window1(center,{X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,?__(3,"Set scale center")++":"}]},
        {label_column,[
             {"X:",{text,X,[{key,cx}]}},
             {"Y:",{text,Y,[{key,cy}]}},
             {"Z:",{text,Z,[{key,cz}]}}
        ]}
    ]};
draw_window1(aspect,{X,Y,Z}) ->
    {vframe,[
        {hframe,[{label,"   " ++ ?__(6,"Link")++":"}]},
        {label_column,[
            {" ",{hframe,[{"",false,[{key,ax},disable(X==0.0)]}],[{border, 3}]}},
            {" ",{hframe,[{"",false,[{key,ay},disable(Y==0.0)]}],[{border, 3}]}},
            {" ",{hframe,[{"",false,[{key,az},disable(Z==0.0)]}],[{border, 3}]}}
        ]}
    ]};
draw_window1(whole,true) ->
    {?__(4,"Scale whole object"),false,[{key,whole}]};
draw_window1(whole,false) ->
    {?__(5,"Scale whole objects"),false,[{key,whole}]};
draw_window1(sugc,_) ->
    {?__(7,"Fit selection to target"),false,
     [{key,sugc}, {hook, fun disable/3}]}.

disable(sugc, Bool, Store) ->
    _ = [wings_dialog:enable(Key, not Bool, Store) || Key <- [cx,cy,cz]].

disable(Bool) ->
    {hook, fun(Key, _, Store) ->
		   wings_dialog:enable(Key, not Bool, Store)
	   end}.

checkChained(Data) -> 
    [{_,Factor}|_]=lists:keysort(1,checkChained(Data, [{0.0,1.0}])),
    Factor.
checkChained([], List) -> List;
checkChained([{SX,OX,true}|Rest], List) ->
    checkChained(Rest, [{-abs(SX/OX-1.0),SX/OX}|List]);
checkChained([_|Rest], List) ->
    checkChained(Rest, List).

translate(Options, SugCenter, {OX,OY,OZ}=Original, St) ->
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
    do_scale([{NX,NY,NZ},Original,Center,{whole,Whole}], St).

%%
%% do_scale(Options,Selection,State)
%%
%% this is main absolute scale command, it returns new state.
%%

do_scale([{SX,SY,SZ},{OX,OY,OZ},{CX,CY,CZ},{_,Whole}], St) ->
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
    Pre = e3d_mat:translate(CX, CY, CZ),
    Scale = e3d_mat:scale(SX2, SY2, SZ2),
    Post = e3d_mat:translate(-CX, -CY, -CZ),
    Mat = e3d_mat:mul(e3d_mat:mul(Pre, Scale), Post),
    if
        Whole ->
            MF = fun(_, We) ->
                         wings_we:transform_vs(Mat, We)
                 end,
            wings_sel:map(MF, St);
        true ->
            do_scale_1(Mat, St)
    end.

do_scale_1(Mat, #st{selmode=Mode}=St) ->
    MF = fun(Items, #we{vp=Vtab0}=We) ->
                 Vs0 = wings_sel:to_vertices(Mode, Items, We),
                 Vs = gb_sets:from_list(Vs0),
                 Vtab = execute_scale(Mat, Vs, Vtab0),
                 We#we{vp=Vtab}
         end,
    wings_sel:map(MF, St).

execute_scale(Mat, Vset, Vtab) ->
    execute_scale(array:sparse_size(Vtab)-1, Mat, Vset, Vtab).

execute_scale(-1, _, _, Vtab) ->
    Vtab;
execute_scale(Vertex, Mat, Vset, Vtab0) ->
    case array:get(Vertex, Vtab0) of
	undefined ->
	    execute_scale(Vertex-1, Mat, Vset, Vtab0);
	Pos0 ->
            Vtab = case gb_sets:is_element(Vertex, Vset) of
		       true ->
                           Pos = e3d_mat:mul_point(Mat, Pos0),
			   array:set(Vertex, Pos, Vtab0);
		       false ->
			   Vtab0
		   end,
            execute_scale(Vertex-1, Mat, Vset, Vtab)
    end.
