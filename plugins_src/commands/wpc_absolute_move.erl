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
      {?__(3,"Move to exact position in absolute coordinates."),
       ?__(8,"Move using a secondary selection as reference."),
       ?__(9,"Move using a secondary selection and orientation as reference.")},[]},
     {?__(4,"Snap"), snap_fun(Mode),
      {?__(5,"Move to secondary selection."),
       ?__(6,"Move using center as reference."),
       ?__(7,"Move and display numeric entry.")},[]}].

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
    Mirror = check_mirror(St),
    if
        Mirror ->
            mirror_error(),
            St;
        true ->
            case Mode of
                move -> move(St);
                rmove -> wings:ask(selection_ask([reference]), St, fun rmove/2);
                omove -> wings:ask(selection_ask([reference,axis,axis_point]), St, fun omove/2);
                snap -> wings:ask(selection_ask([reference,target]), St, fun snap/2);
                csnap -> wings:ask(selection_ask([target]), St, fun csnap/2);
                nsnap -> wings:ask(selection_ask([reference,target]), St, fun nsnap/2)
            end
    end;
command(_,_) -> next.

%%%
%%% absolute move
%%%

move(St) ->
    move(center, St).

rmove(Reference, St) ->
    move(Reference, St).

omove({From, LineDir0, PlaneNorm0, PlanePoint}, St) ->
    Sel = get_selection(St),

    PlaneNorm = e3d_vec:norm(PlaneNorm0),
    LineDir = e3d_vec:norm(LineDir0),
    DotProd = e3d_vec:dot(LineDir, PlaneNorm),

    if
	abs(DotProd) > 0.001 ->
        X = e3d_vec:dot(e3d_vec:sub(PlanePoint, From),PlaneNorm)/DotProd,
        Offset= e3d_vec:mul(LineDir, X),
        #st{shapes=Shps0}=St,
        Shps=lists:foldr(fun({Obj0,Vset}, ShpsAcc) ->
            #we{vp=Vtab0}=We=gb_trees:get(Obj0, ShpsAcc),
            Vtab=lists:foldl(fun(V, VtabAcc) ->
                Vs0=array:get(V,VtabAcc),
                Vs=e3d_vec:add(Vs0, Offset),
                array:set(V,Vs,VtabAcc)
            end, Vtab0, gb_sets:to_list(Vset)),
            gb_trees:update(Obj0, We#we{vp=Vtab}, ShpsAcc)
        end, Shps0, Sel),
        {save_state,St#st{shapes=Shps}};
	true ->
	    wpa:error_msg(?__(1,"Line and plane are nearly parallel:\n"
		      "can't find intersection.")),
	    keep
    end.

move(Reference0, #st{shapes=Shapes}=St) ->
    Sel = get_selection(St),
    {Center,Lights} = get_center_and_lights(Sel,Shapes),
    Reference = if Reference0 =:= center -> Center;
        true -> Reference0
    end,
    OneObject = check_single_obj(Sel),
    SinglePoints = check_single_vert(Sel),
    WholeObjects = if
                       SinglePoints or Lights ->
                           false;
                       true ->
                           check_whole_obj(St)
                   end,
    MoveObj = if
                  WholeObjects or Lights -> duplionly;
                  OneObject -> one;
                  true -> many
              end,
    Flatten = if
                  SinglePoints or WholeObjects or Lights -> false;
                  true -> true
              end,
    Align = not OneObject,
    draw_window({{move_obj,MoveObj},{flatten,Flatten},{align,Align},{from,Reference},{to,Reference},{lock,false}},Sel,St).

%%%
%%% absolute snap
%%%

snap({From,To}, St) ->
    Sel = get_selection(St),
    {save_state,do_move([From, To, false, {false, false, false}, {false, false, false}, 0], Sel, St)}.

%%%
%%% absolute snap with center as reference
%%%

csnap(To, #st{shapes=Shs}=St) ->
    Sel = get_selection(St),
    From = get_center(Sel, Shs),
    {save_state,do_move([From, To, false, {false, false, false}, {false, false, false}, 0], Sel, St)}.

%%%
%%% absolute snap with numeric entry
%%%

nsnap({From,To}, #st{shapes=Shs}=St) ->
    Sel = get_selection(St),
    Lights = get_lights(Sel, Shs),
    OneObject = check_single_obj(Sel),
    SinglePoints = check_single_vert(Sel),
    WholeObjects = if
                       SinglePoints or Lights ->
                           false;
                       true ->
                           check_whole_obj(St)
                   end,
    MoveObj = if
                  WholeObjects or Lights -> duplionly;
                  OneObject -> one;
                  true -> many
              end,
    Flatten = if
                  SinglePoints or WholeObjects or Lights -> false;
                  true -> true
              end,
    Align = not OneObject,
    draw_window({{move_obj,MoveObj},{flatten,Flatten},{align,Align},{from,From},{to,To},{lock,true}},Sel,St).

%%%
%%% some helpful test and investigation functions
%%%

check_mirror(#st{shapes=Shs}=St) ->
    Sel = get_selection(St),
    check_mirror(Sel, Shs).

check_mirror([],_) ->
    false;
check_mirror([{Obj,VSet}|Rest],Shs) ->
    We = gb_trees:get(Obj, Shs),
    Mirror = We#we.mirror,
    case Mirror of
        none ->
            check_mirror(Rest,Shs);
        _ ->
            MirrorVerts = wings_face:vertices_cw(Mirror,We),
            MVSet = gb_sets:from_list(MirrorVerts),
            case gb_sets:is_disjoint(MVSet,VSet) of
                true -> check_mirror(Rest,Shs);
                false -> true
            end
    end.

-spec mirror_error() -> no_return().
mirror_error() ->
    wings_u:error_msg(?__(1,"You cannot move vertices from mirror plane")).

get_selection(#st{selmode=SelMode}=St) ->
    #st{sel=Sel} = case SelMode of
        vertex -> St;
        _ -> wings_sel_conv:mode(vertex,St)
    end,
    Sel.

get_center(Sel,Shapes) ->
    {Center,_} = get_center_and_lights(Sel,Shapes,[],false),
    Center.

get_lights(Sel,Shapes) ->
    {_,Lights} = get_center_and_lights(Sel,Shapes,[],false),
    Lights.

get_center_and_lights(Sel,Shapes) ->
    get_center_and_lights(Sel,Shapes,[],true).

get_center_and_lights([],_,Now,Lights) ->
    {e3d_vec:average(Now),Lights};
get_center_and_lights([{Obj,Vset}|Rest],Shapes,Now,Lights) ->
    We = gb_trees:get(Obj, Shapes),
    Positions = gb_sets:fold(fun(Vert, Acc) ->
                         [wings_vertex:pos(Vert, We)|Acc]
                      end, [], Vset),
    get_center_and_lights(Rest,Shapes,Now++Positions,Lights and ?IS_LIGHT(We)).

check_single_obj([{_,_}]) -> true;
check_single_obj(_) -> false.

check_single_vert(L) ->
    lists:all(fun({_,GbSet}) -> gb_sets:size(GbSet) =:= 1 end, L).

check_whole_obj(#st{selmode=SelMode}=St0) ->
    St1 = wings_sel_conv:mode(body,St0),
    St2 = wings_sel_conv:mode(SelMode,St1),
    St2#st.sel == St0#st.sel.

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
%% functions that draws interface and translates entered options for further processing
%%  and calls do_move(ProcessedOptions,Selection,State)
%%

draw_window({{_,MoveObj},{_,Flatten},{_,Align},{_,{CX,CY,CZ}=Center},{_,Default},{_,Lock}},
	    Sel,#st{selmode=SelMode}=St) ->
    MoveD =  case MoveObj of
		 one ->
		     {hframe,
		      [{?__(3,"Move object"),false,[{key,all}, {hook, fun disable/3}]},
		       panel | duplicate(true)]};
		 many ->
		     {hframe,
		      [{?__(4,"Move objects"),false,[{key,all}, {hook, fun disable/3}]},
		       panel | duplicate(true)]};
		 duplionly ->
		     {hframe, duplicate(false)}
             end,

    {Headers, RX,RY,RZ} = lists:foldl(fun draw_window1/2, {[],[],[],[]},
				      [{center, Default},  {align,Align},
				       {flatten, Flatten}, {lock, Lock}]),

    Frame1 = {label_column,[{" ", lists:reverse(Headers)},
			    {"X:", lists:reverse(RX)},
			    {"Y:", lists:reverse(RY)},
			    {"Y:", lists:reverse(RZ)}]},

    Frame6 = if % Lock is true only for Snap and this extra check box must be used only with it
		 Lock and (SelMode == body) ->
		     [{label_column,
		       [{?__(10,"Between reference and target")++":",
			 {hframe,[{"",false,[{key,dup_rt}]}]}}
		       ]}];
                 true -> []
             end,
    Reference = {label,?__(8,"Reference point is") ++ ": (" ++
		     wings_util:nice_float(CX)++", "++
		     wings_util:nice_float(CY)++", "++
		     wings_util:nice_float(CZ)++")"},

    Frame = [{vframe,[Frame1, MoveD|Frame6++[separator,Reference]]}],
    wings_dialog:dialog(?__(1,"Absolute move options"), {preview,Frame},
       fun
           ({dialog_preview,Move}) ->
               {preview,St,translate(Move,Center,Sel,St)};
           (cancel) -> St;
           (Move) ->
               {commit,St,translate(Move,Center,Sel,St)}
       end).

duplicate(CheckAll) when is_boolean(CheckAll) ->
    [
	     {value, CheckAll, [{key, dupli_check}]},
	     {text,0,[{key,dupli},{range,{0,infinity}},{width,6}]},
             {label," "++?__(5,"Duplicates")}
    ].

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
    catch _:_ -> ignore end,
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
	wings_dialog:enable(depend(What), Bool, Store)
    catch _:_ -> ignore end.

depend(lx) -> x;
depend(ly) -> y;
depend(lz) -> z.

lookup(Key, List, Default) ->
   case lists:keysearch(Key, 1, List) of
      {value,{_,Value}} -> Value;
      _ -> Default
   end.

translate(Options,{CX,CY,CZ}=Center,Sel,St) ->
   X = lookup(x, Options, 0.0),
   Y = lookup(y, Options, 0.0),
   Z = lookup(z, Options, 0.0),
   NX = case lookup(lx,Options,false) of
           true -> CX;
           _ -> X
        end,
   NY = case lookup(ly,Options,false) of
           true -> CY;
           _ -> Y
        end,
   NZ = case lookup(lz, Options, false) of
           true -> CZ;
           _ -> Z
        end,
   Obj = lookup(all,Options,true),
   Dupli = case lookup(dupli,Options,0) of
              N when Obj -> N;
              _ -> 0
           end,
   Dup_rt = lookup(dup_rt,Options,false),
   Ax = lookup(ax,Options,false) and (Dupli>0),
   Ay = lookup(ay,Options,false) and (Dupli>0),
   Az = lookup(az,Options,false) and (Dupli>0),
   Fx = lookup(fx,Options,false),
   Fy = lookup(fy,Options,false),
   Fz = lookup(fz,Options,false),
   do_move([Center,{NX,NY,NZ},Obj,{Ax,Ay,Az},{Fx,Fy,Fz},Dupli,Dup_rt],Sel,St).

%%
%% do_move(Options,Selection,State)
%%
%% this is main absolute move command, it returns new state.
%%

do_move([_,XYZ,_,_,_,Dupli]=Move,Sel,St) ->
    do_move1(XYZ,Dupli,false,Move,Sel,St);
do_move([_,XYZ,_,_,_,Dupli,Dup_rt]=Move0,Sel,St) ->
    {Move,_}=lists:split(length(Move0) -1, Move0),
    do_move1(XYZ,Dupli,Dup_rt,Move,Sel,St).

do_move1(_,_,_,_,[],St) ->
    St;
do_move1({XO,YO,ZO},DuOrg,Dup_rt,[{Cx,Cy,Cz},{X,Y,Z},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},Du],[{Obj0,Vset}|Rest]=Sel,#st{shapes=Shapes0}=St0) ->
    We0 = gb_trees:get(Obj0, Shapes0),
    #st{shapes=Shapes1,onext=Oid} = St1 = if
                                              Du > 0 ->
                                                  wings_shape:insert(We0, copy, St0);
                                              true ->
                                                  St0
                                          end,
    if
        Du > 0 ->
            Obj1 = Oid-1,
            We1 = gb_trees:get(Obj1, Shapes1);
        true ->
            Obj1 = Obj0,
            We1 = We0
    end,
    Vtab = We1#we.vp,
    {Ox,Oy,Oz} = get_center([{Obj1,Vset}],Shapes1),
    if (DuOrg > 0) and Dup_rt ->
        Dx = ((X - Cx)/DuOrg)*Du,
        Dy = ((Y - Cy)/DuOrg)*Du,
        Dz = ((Z - Cz)/DuOrg)*Du;
    true ->
        Dx = if
                 Ax -> X - Ox;
                 true -> X - Cx
             end,
        Dy = if
                 Ay -> Y - Oy;
                 true -> Y - Cy
             end,
        Dz = if
                 Az -> Z - Oz;
                 true -> Z - Cz
             end
    end,
    NewVtab = execute_move({Dx,Dy,Dz},{X,Y,Z},{Fx,Fy,Fz},Wo or ?IS_LIGHT(We1),Vset,Vtab),
    NewWe = We1#we{vp=NewVtab},
    NewShapes = gb_trees:update(Obj1,NewWe,Shapes1),
    NewSt = St1#st{shapes=NewShapes},
    if
        Du > 1 ->
            if Dup_rt ->
                do_move1({XO,YO,ZO},DuOrg,Dup_rt,[{Cx,Cy,Cz},{X,Y,Z},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},Du-1],Sel,NewSt);
            true ->
                do_move1({XO,YO,ZO},DuOrg,Dup_rt,[{Cx,Cy,Cz},{XO+Dx,YO+Dy,ZO+Dz},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},Du-1],Sel,NewSt)
            end;
        true ->
            do_move1({XO,YO,ZO},DuOrg,Dup_rt,[{Cx,Cy,Cz},{XO,YO,ZO},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},DuOrg],Rest,NewSt)
    end.

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

