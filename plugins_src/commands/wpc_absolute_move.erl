%%
%%  wpc_absolute_move.erl --
%%
%%     Plug-in for absolute commands -> move and snap
%%
%%  Copyright (c) 2006-2011 Andrzej Giniewicz
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_absolute_move).

-include_lib("wings/src/wings.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({Mode},Menu) when Mode == vertex; Mode == edge; Mode == face; Mode == body; Mode == light ->
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
    [{?__(2,"Move"), move_fun(Mode),
      {?__(3,"Move to exact position in absolute coordinates"),
       ?__(8,"Move using a secondary selection as reference"),
       ?__(9,"Move using a secondary selection and orientation as reference")},[]},
     {?__(4,"Snap"), snap_fun(Mode),
      {?__(5,"Move to secondary selection"),
       ?__(6,"Move using center as reference"),
       ?__(7,"Move and display numeric entry")},[]}].

move_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{absolute,move}};
       (2, _Ns) ->
	    {Mode,{absolute,rmove}};
       (3, _Ns) ->
	    {Mode,{absolute,omove}};
       (_, _) -> ignore
    end.

snap_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{absolute,snap}};
       (2, _Ns) ->
	    {Mode,{absolute,csnap}};
       (3, _Ns) ->
	    {Mode,{absolute,nsnap}};
       (_, _) -> ignore
    end.

command({_,{absolute,Mode}},St) when Mode == move; Mode == rmove; Mode == omove; Mode == snap; Mode == csnap; Mode == nsnap ->
    check_mirror(St),
    case Mode of
        move ->
            move(St);
        rmove ->
            wings:ask(selection_ask([reference]), St, fun rmove/2);
        omove ->
            wings:ask(selection_ask([reference,axis,axis_point]), St,
                      fun omove/2);
        snap ->
            wings:ask(selection_ask([reference,target]), St, fun snap/2);
        csnap ->
            wings:ask(selection_ask([target]), St, fun csnap/2);
        nsnap ->
            wings:ask(selection_ask([reference,target]), St, fun nsnap/2)
    end;
command(_,_) -> next.

%%%
%%% absolute move
%%%

move(St) ->
    move(center, St).

rmove(Reference, St) ->
    move(Reference, St).

omove({From,LineDir0,PlaneNorm0,PlanePoint}, #st{selmode=Mode}=St0) ->
    PlaneNorm = e3d_vec:norm(PlaneNorm0),
    LineDir = e3d_vec:norm(LineDir0),
    DotProd = e3d_vec:dot(LineDir, PlaneNorm),
    if
	abs(DotProd) =< 0.001 ->
	    wpa:error_msg(?__(1,"Line and plane are nearly parallel:\n"
		      "can't find intersection."));
        true ->
            ok
    end,
    X = e3d_vec:dot(e3d_vec:sub(PlanePoint, From), PlaneNorm)/DotProd,
    Offset = e3d_vec:mul(LineDir, X),
    MF = fun(Items, #we{vp=Vtab0}=We) ->
                 Vs = wings_sel:to_vertices(Mode, Items, We),
                 Vtab = foldl(
                          fun(V, Vt) ->
                                  Pos0 = array:get(V, Vt),
                                  Pos = e3d_vec:add(Pos0, Offset),
                                  array:set(V, Pos, Vt)
                          end, Vtab0, Vs),
                 We#we{vp=Vtab}
         end,
    St = wings_sel:map(MF, St0),
    {save_state,St}.

move(Reference0, St) ->
    Reference = case Reference0 of
                    center -> wings_sel:center_vs(St);
                    _ -> Reference0
                end,
    {OneObject,SinglePoints,Lights} = analyze_selection(St),
    WholeObjects = if
                       SinglePoints or Lights ->
                           false;
                       true ->
                           all_items_selected(St)
                   end,
    MoveObj = if
                  WholeObjects or Lights -> dup_only;
                  OneObject -> one;
                  true -> many
              end,
    Flatten = if
                  SinglePoints or WholeObjects or Lights -> false;
                  true -> true
              end,
    Align = not OneObject,
    draw_window({{move_obj,MoveObj},
                 {flatten,Flatten},
                 {align,Align},
                 {from,Reference},
                 {to,Reference},
                 {lock,false}}, St).

%% -> {SingleObject,SinglePoints,AllLights}
analyze_selection(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
                 IsLight = ?IS_LIGHT(We),
                 {1,gb_sets:size(Items) =:= 1,IsLight}
         end,
    RF = fun({B,S,L}, {B0,S0,L0}) ->
                 {B+B0,S0 and S,L and L0}
         end,
    Acc0 = {0,Mode =:= vertex,true},
    {N,Single,Lights} = wings_sel:dfold(MF, RF, Acc0, St),
    {N =:= 1,Single,Lights}.

all_items_selected(#st{selmode=Mode}=St) ->
    MF = fun(Items, We) ->
                 wings_sel:get_all_items(Mode, We) =:= Items
         end,
    RF = fun erlang:'and'/2,
    wings_sel:dfold(MF, RF, true, St).

%%%
%%% absolute snap
%%%

snap({From,To}, St) ->
    {save_state,do_move([From, To, false, {false, false, false},
                         {false, false, false}, 0], St)}.

%%%
%%% absolute snap with center as reference
%%%

csnap(To, St) ->
    From = wings_sel:center_vs(St),
    {save_state,do_move([From, To, false, {false, false, false},
                         {false, false, false}, 0], St)}.

%%%
%%% absolute snap with numeric entry
%%%

nsnap({From,To}, St) ->
    {OneObject,SinglePoints,Lights} = analyze_selection(St),
    WholeObjects = if
                       SinglePoints or Lights ->
                           false;
                       true ->
                           all_items_selected(St)
                   end,
    MoveObj = if
                  WholeObjects or Lights -> dup_only;
                  OneObject -> one;
                  true -> many
              end,
    Flatten = if
                  SinglePoints or WholeObjects or Lights -> false;
                  true -> true
              end,
    Align = not OneObject,
    draw_window({{move_obj,MoveObj},
                 {flatten,Flatten},
                 {align,Align},
                 {from,From},
                 {to,To},
                 {lock,true}}, St).

%%%
%%% some helpful test and investigation functions
%%%

check_mirror(#st{selmode=Mode}=St) ->
    MF = fun(_Items, #we{mirror=none}) ->
                 true;
            (Items, #we{mirror=Mirror}=We) ->
                 Vs = wings_sel:to_vertices(Mode, Items, We),
                 MirrorVs0 = wings_face:vertices_cw(Mirror, We),
                 MirrorVs = ordsets:from_list(MirrorVs0),
                 ordsets:is_disjoint(Vs, MirrorVs)
         end,
    RF = fun erlang:'and'/2,
    case wings_sel:dfold(MF, RF, true, St) of
        false -> mirror_error();
        true -> ok
    end.

-spec mirror_error() -> no_return().
mirror_error() ->
    wings_u:error_msg(?__(1,"You cannot move vertices from mirror plane")).

selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face, body]}.

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([reference|Rest],Ask) ->
    Desc = ?__(1,"Select reference point for snap operation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([target|Rest],Ask) ->
    Desc = ?__(2,"Select target point for snap operation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([axis|Rest],Ask) ->
    Desc = ?__(3,"Select reference axis for orientation"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([axis_point|Rest],Ask) ->
    Desc = ?__(4,"Pick axis perpendicular to plane (plane will pass through axis's base)"),
    selection_ask(Rest,[{axis_point,Desc}|Ask]).

%%%
%%% Core functions
%%%

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for
%% further processing and calls
%% do_move(ProcessedOptions, Selection, State)
%%

draw_window({{_,MoveObj},{_,Flatten},{_,Align},{_,Center},{_,Default},{_,Lock}},
	    #st{selmode=SelMode}=St) ->
    MoveD = case MoveObj of
                one ->
                    {hframe,
                     [{?__(3,"Move object"),false,[{key,all},
                                                   {hook, fun disable/3}]},
                      panel | duplicate(true)]};
                many ->
                    {hframe,
                     [{?__(4,"Move objects"),false,[{key,all},
                                                    {hook, fun disable/3}]},
                      panel | duplicate(true)]};
                dup_only ->
                    {hframe, duplicate(false)}
            end,

    {Headers, RX,RY,RZ} = foldl(fun draw_window1/2, {[],[],[],[]},
                                [{center, Default},  {align,Align},
                                 {flatten, Flatten}, {lock, Lock}]),

    Frame1 = {label_column,[{" ", lists:reverse(Headers)},
			    {"X:", lists:reverse(RX)},
			    {"Y:", lists:reverse(RY)},
			    {"Z:", lists:reverse(RZ)}]},

    Frame6 = if
                 %% Lock is true only for Snap and this extra check box
                 %% must be used only with it.
		 Lock, SelMode =:= body ->
		     [{label_column,
		       [{?__(10,"Between reference and target")++":",
			 {hframe,[{"",false,[{key,dup_rt}]}]}}
		       ]}];
                 true -> []
             end,
    Reference = {label,?__(8,"Reference point is") ++ ": " ++
                     wings_util:nice_vector(Center)},

    Frame = [{vframe,[Frame1, MoveD|Frame6++[separator,Reference]]}],
    wings_dialog:dialog(?__(1,"Absolute move options"), {preview,Frame},
       fun({dialog_preview,Move}) ->
               {preview,St,translate(Move, Center, St)};
          (cancel) ->
               St;
           (Move) ->
               {commit,St,translate(Move, Center, St)}
       end).

duplicate(CheckAll) when is_boolean(CheckAll) ->
    [{value, CheckAll, [{key, dupli_check}]},
     {text,0,[{key,dupli},{range,{0,infinity}},{width,6}]},
     {label," "++?__(5,"Duplicates")}].

draw_window1({center,{XC,YC,ZC}}, {Header, X,Y,Z}) ->
    {[{label,?__(2,"Set position")++":", [{proportion, 2}]}|Header],
     [{text,XC,[{key,x},{proportion,2}]}|X],
     [{text,YC,[{key,y},{proportion,2}]}|Y],
     [{text,ZC,[{key,z},{proportion,2}]}|Z]};
draw_window1({align, true}, {Header, X,Y,Z}) ->
    {[{label,?__(6,"Align")++":", [{proportion, 1}]}|Header],
     [{"",false,[{key,ax},{proportion,1}]}|X],
     [{"",false,[{key,ay},{proportion,1}]}|Y],
     [{"",false,[{key,az},{proportion,1}]}|Z]};
draw_window1({flatten,true}, {Header, X,Y,Z}) ->
    {[{label,?__(7,"Flatten")++":", [{proportion, 1}]}|Header],
     [{"",false,[{key,fx},{proportion,1}]}|X],
     [{"",false,[{key,fy},{proportion,1}]}|Y],
     [{"",false,[{key,fz},{proportion,1}]}|Z]};
draw_window1({lock,true},  {Header, X,Y,Z}) ->
    {[{label,?__(9,"Lock")++":", [{proportion, 1}]}|Header],
     [{"",false,[{key,lx},{proportion,1},{hook, fun disable/3}]}|X],
     [{"",false,[{key,ly},{proportion,1},{hook, fun disable/3}]}|Y],
     [{"",false,[{key,lz},{proportion,1},{hook, fun disable/3}]}|Z]};
draw_window1({_, false}, Acc) ->
    Acc.

disable(all, Bool, Store) ->
    try
	Use = wings_dialog:get_value(dupli_check, Store),
	wings_dialog:enable(dupli, Bool andalso Use, Store)
    catch _:_ ->
            ignore
    end,
    try
	Dupli = wings_dialog:get_value(dupli, Store),
	try
	    wings_dialog:enable(ax, Bool andalso Dupli > 1, Store),
	    wings_dialog:enable(ay, Bool andalso Dupli > 1, Store),
	    wings_dialog:enable(az, Bool andalso Dupli > 1, Store)
	catch _:_ -> ignore end,
	wings_dialog:enable(dup_rt, Bool andalso Dupli < 1, Store)
    catch _:_ -> ignore end;
disable(What, Bool, Store) ->
    try
	wings_dialog:enable(depend(What), not Bool, Store)
    catch _:_ -> ignore end.

depend(lx) -> x;
depend(ly) -> y;
depend(lz) -> z.

lookup(Key, List, Default) ->
   case lists:keyfind(Key, 1, List) of
       {_,Value} -> Value;
       false -> Default
   end.

translate(Options, {CX,CY,CZ}=Center, St) ->
    X = lookup(x, Options, 0.0),
    Y = lookup(y, Options, 0.0),
    Z = lookup(z, Options, 0.0),
    NX = case lookup(lx, Options, false) of
             true -> CX;
             false -> X
         end,
    NY = case lookup(ly, Options, false) of
             true -> CY;
             false -> Y
         end,
    NZ = case lookup(lz, Options, false) of
             true -> CZ;
             false -> Z
         end,
    Obj = lookup(all, Options, true),
    Dupli = case lookup(dupli, Options, 0) of
                N when Obj -> N;
                _ -> 0
            end,
    DupRT = lookup(dup_rt, Options, false),
    Ax = lookup(ax, Options, false) and (Dupli > 0),
    Ay = lookup(ay, Options, false) and (Dupli > 0),
    Az = lookup(az, Options, false) and (Dupli > 0),
    Fx = lookup(fx, Options, false),
    Fy = lookup(fy, Options, false),
    Fz = lookup(fz, Options, false),
    do_move([Center,{NX,NY,NZ},Obj,{Ax,Ay,Az},{Fx,Fy,Fz},Dupli,DupRT], St).

%%
%% do_move(Options,Selection,State)
%%
%% this is main absolute move command, it returns new state.
%%

do_move([_,XYZ,_,_,_,Dupli]=Move, St) ->
    do_move_1(XYZ, Dupli, false, Move, St);
do_move([C,Origin,Wo,A,F,Dupli,DupRT], St) ->
    if
        DupRT, Dupli > 0 ->
            Move = [C,Origin,Wo,{false,false,false},F,Dupli],
            do_move_1(Origin, Dupli, true, Move, St);
        true ->
            Move = [C,Origin,Wo,A,F,Dupli],
            do_move_1(Origin, Dupli, DupRT, Move, St)
    end.

do_move_1(Origin, DuOrg, DupRT, Move, #st{selmode=Mode}=St) ->
    CF = fun(Items, We0) ->
                 Vs0 = wings_sel:to_vertices(Mode, Items, We0),
                 Vs = gb_sets:from_ordset(Vs0),
                 [We|Wes] = do_move_2(Vs, We0, Origin, DuOrg, DupRT, Move),
                 Empty = gb_sets:empty(),
                 New = [{W,Empty,copy} || W <- Wes],
                 {We,Items,New}
         end,
    wings_sel:clone(CF, St).

do_move_2(Vs, We, Origin, DuOrg, DupRT, Params) when DuOrg > 0 ->
    [We|do_move_3(Vs, We, Origin, DuOrg, DupRT, Params)];
do_move_2(Vs, We, Origin, DuOrg, DupRT, Params) ->
    do_move_3(Vs, We, Origin, DuOrg, DupRT, Params).

do_move_3(Vs, We0, Origin, DuOrg, DupRT,
          [CommonCenter,Pos,Wo,Align,Flatten,Du]) ->
    Center = wings_vertex:center(Vs, We0),
    D0 = d(Pos, Align, CommonCenter, Center),
    D = if
            DupRT, DuOrg > 0 ->
                e3d_vec:mul(D0, Du/DuOrg);
            true ->
                D0
        end,
    Vtab0 = We0#we.vp,
    Vtab = execute_move(D, Pos, Flatten, Wo or ?IS_LIGHT(We0), Vs, Vtab0),
    We = We0#we{vp=Vtab},
    [We|
     if
         Du > 1 ->
             if DupRT ->
                     do_move_3(Vs, We0, Origin, DuOrg, DupRT,
                               [CommonCenter,Pos,Wo,Align,
                                Flatten,Du-1]);
                true ->
                     do_move_3(Vs, We0, Origin, DuOrg, DupRT,
                               [CommonCenter,e3d_vec:add(Origin, D),Wo,
                                Align,Flatten,Du-1])
             end;
         true ->
             []
     end].

d(Pos0, Align, CommonCenter, Center) ->
    PosAlign = e3d_vec:sub(Pos0, Center),
    Pos = e3d_vec:sub(Pos0, CommonCenter),
    d_1(1, Align, PosAlign, Pos).

d_1(I, Align, PosAlign, Pos0) when I =< tuple_size(Align) ->
    case element(I, Align) of
        true ->
            Pos = setelement(I, Pos0, element(I, PosAlign)),
            d_1(I+1, Align, PosAlign, Pos);
        false ->
            d_1(I+1, Align, PosAlign, Pos0)
    end;
d_1(_, _, _, Pos) -> Pos.

execute_move(D,N,F,Wo,Vset,Vtab) ->
    execute_move(array:sparse_size(Vtab)-1,D,N,F,Wo,Vset,Vtab).

execute_move(-1,_D,_N,_F,_Wo,_Vset,Vtab) ->
    Vtab;
execute_move(Vertex,{Dx,Dy,Dz}=D,{Nx,Ny,Nz}=N,{Fx,Fy,Fz}=F,Wo,Vset,Vtab) ->
    case array:get(Vertex, Vtab) of
	undefined ->
	    execute_move(Vertex-1,D,N,F,Wo,Vset,Vtab);
	{X,Y,Z} ->
            case gb_sets:is_element(Vertex,Vset) of
                true ->
                    X1 = case Fx of
                        true -> Nx;
                        _ -> X+Dx
                    end,
                    Y1 = case Fy of
                        true -> Ny;
                        _ -> Y+Dy
                    end,
                    Z1 = case Fz of
                        true -> Nz;
                        _ -> Z+Dz
                    end;
                _ ->
                    if
                        Wo ->
                            X1 = X+Dx,
                            Y1 = Y+Dy,
                            Z1 = Z+Dz;
                        true ->
                            X1 = X,
                            Y1 = Y,
                            Z1 = Z
                    end
            end,
	    Vtab2 = case {X1,Y1,Z1} of
			{X,Y,Z} -> Vtab;
			NewPos -> array:set(Vertex,NewPos,Vtab)
		    end,
            execute_move(Vertex-1,D,N,F,Wo,Vset,Vtab2)
    end.
