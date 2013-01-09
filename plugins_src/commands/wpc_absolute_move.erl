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

-include("wings.hrl").

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
    [{?__(2,"Move"),move,
      ?__(3,"Move to exact position in absolute coordinates.")},
     {?__(4,"Snap"), snap_fun(Mode), 
      {?__(5,"Move to secondary selection."),
       ?__(6,"Move using center as reference."),
       ?__(7,"Move and display numeric entry.")},[]}].

snap_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{absolute,snap}};
       (2, _Ns) ->
	    {Mode,{absolute,csnap}};
       (3, _Ns) ->
	    {Mode,{absolute,nsnap}};
       (_, _) -> ignore
    end.

command({_,{absolute,Mode}},St) when Mode == move; Mode == snap; Mode == csnap; Mode == nsnap ->
    Mirror = check_mirror(St),
    if
        Mirror -> 
            mirror_error(),
            St;
        true ->
            case Mode of
                move -> move(St);
                snap -> wings:ask(selection_ask([reference,target]), St, fun snap/2);
                csnap -> wings:ask(selection_ask([target]), St, fun csnap/2);
                nsnap -> wings:ask(selection_ask([reference,target]), St, fun nsnap/2)
            end
    end;
command(_,_) -> next.

%%%
%%% absolute move
%%%

move(#st{shapes=Shapes}=St) ->
    Sel = get_selection(St),
    {Center,Lights} = get_center_and_lights(Sel,Shapes),
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
    draw_window({{move_obj,MoveObj},{flatten,Flatten},{align,Align},{from,Center},{to,Center},{lock,false}},Sel,St).

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
    selection_ask(Rest,[{point,Desc}|Ask]).

%%%
%%% Core functions
%%%

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls do_move(ProcessedOptions,Selection,State)
%%

draw_window({{_,MoveObj},{_,Flatten},{_,Align},{_,Center},{_,Default},{_,Lock}},Sel,St) ->
    Frame1 = [{vframe,
                 [draw_window1(center,Default)] ++
                 [draw_window1(object,MoveObj)]}],
    Frame2 = if
                 Align ->
                     [draw_window1(align,default)];
                 true ->
                     []
             end,
    Frame3 = if
                 Flatten ->
                     [draw_window1(flatten,default)];
                 true ->
                     []
             end,
    Frame35 = if
                 Lock ->
                     [draw_window1(lock,default)];
                 true ->
                     []
              end,
    Frame4 = if
                 MoveObj =/= duplionly -> 
                     [draw_window1(duplicate,true)];
                 true ->
                     []
             end,
    Frame5 = if
                Frame2 =/= [] orelse Frame3 =/= [] orelse Frame35 =/= [] -> 
                    [{hframe,Frame1++[{vframe,[{hframe,Frame2++Frame3++Frame35}]++Frame4}]}];
                true ->
                    [{vframe,Frame1++Frame4}]
            end,
    Frame = [{vframe,Frame5++[separator,draw_window1(reference,Center)]}],
    Name = draw_window1(name,default),
    wings_ask:dialog(Name, {preview,Frame},
       fun
           ({dialog_preview,Move}) ->
               {preview,St,translate(Move,Center,Sel,St)};
           (cancel) -> St;
           (Move) ->
               {commit,St,translate(Move,Center,Sel,St)}
       end).

draw_window1(name,_) ->
    ?__(1,"Absolute move options");
draw_window1(center,{XC,YC,ZC}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set position")++":"}]},
        {hframe,[{label,"X:"},{text,XC,[{key,x},disable(lx)]}]},
        {hframe,[{label,"Y:"},{text,YC,[{key,y},disable(ly)]}]},
        {hframe,[{label,"Z:"},{text,ZC,[{key,z},disable(lz)]}]}
    ]};
draw_window1(object,one) ->
    {?__(3,"Move object"),false,[{key,all}]};
draw_window1(object,many) ->
    {?__(4,"Move objects"),false,[{key,all}]};
draw_window1(object,duplionly) ->
    draw_window1(duplicate,false);
draw_window1(duplicate,CheckAll) when is_boolean(CheckAll) ->
    Label = if
        CheckAll -> [disable(all)];
        true -> []
    end,
    {hframe,[
        {text,0,[{key,dupli},{range,{0,infinity}}]++Label},
        {label,?__(5,"Duplicates")}
    ]};
draw_window1(align,_) ->
    {vframe,[
        {hframe,[{label,?__(6,"Align")++":"}]},
        {hframe,[{"",false,[{key,ax},disable(dupli)]}]},
        {hframe,[{"",false,[{key,ay},disable(dupli)]}]},
        {hframe,[{"",false,[{key,az},disable(dupli)]}]}
    ]};
draw_window1(flatten,_) ->
    {vframe,[
        {hframe,[{label,?__(7,"Flatten")++":"}]},
        {hframe,[{"",false,[{key,fx}]}]},
        {hframe,[{"",false,[{key,fy}]}]},
        {hframe,[{"",false,[{key,fz}]}]}
    ]};
draw_window1(lock, _) ->
    {vframe,[
        {hframe,[{label,?__(9,"Lock")++":"}]},
        {hframe,[{"",false,[{key,lx}]}]},
        {hframe,[{"",false,[{key,ly}]}]},
        {hframe,[{"",false,[{key,lz}]}]}
    ]};
draw_window1(reference,{X,Y,Z}) ->
    {label,?__(8,"Reference point is") ++ ": (" ++
    wings_util:nice_float(X)++", "++
    wings_util:nice_float(Y)++", "++
    wings_util:nice_float(Z)++")"}.

disable(all) ->
    {hook,fun (is_disabled, {_Var,_I,Store}) ->
                  not gb_trees:get(all, Store);
              (_, _) -> void
          end};
disable(dupli) ->
    {hook,fun (is_disabled, {_Var,_I,Store}) ->
                  (gb_trees:get(dupli, Store) > 1) and 
                  not ((gb_trees:is_defined(all,Store)) andalso (not gb_trees:get(all, Store)));
              (_, _) -> void
          end};
disable(Other) ->
    {hook,fun (is_disabled, {_Var,_I,Store}) ->
                  gb_trees:is_defined(Other,Store) andalso gb_trees:get(Other, Store);
              (_, _) -> void
          end}.

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
   Ax = lookup(ax,Options,false) and (Dupli>0),
   Ay = lookup(ay,Options,false) and (Dupli>0),
   Az = lookup(az,Options,false) and (Dupli>0),
   Fx = lookup(fx,Options,false),
   Fy = lookup(fy,Options,false),
   Fz = lookup(fz,Options,false),
   do_move([Center,{NX,NY,NZ},Obj,{Ax,Ay,Az},{Fx,Fy,Fz},Dupli],Sel,St).

%%
%% do_move(Options,Selection,State)
%%
%% this is main absolute move command, it returns new state.
%%

do_move([_,XYZ,_,_,_,Dupli]=Move,Sel,St) ->
    do_move1(XYZ,Dupli,Move,Sel,St).

do_move1(_,_,_,[],St) ->
    St;
do_move1({XO,YO,ZO},DuOrg,[{Cx,Cy,Cz},{X,Y,Z},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},Du],[{Obj0,Vset}|Rest]=Sel,#st{shapes=Shapes0}=St0) ->
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
         end,
    NewVtab = execute_move({Dx,Dy,Dz},{X,Y,Z},{Fx,Fy,Fz},Wo or ?IS_LIGHT(We1),Vset,Vtab),
    NewWe = We1#we{vp=NewVtab},
    NewShapes = gb_trees:update(Obj1,NewWe,Shapes1),
    NewSt = St1#st{shapes=NewShapes},
    if
        Du > 1 ->
            do_move1({XO,YO,ZO},DuOrg,[{Cx,Cy,Cz},{XO+Dx,YO+Dy,ZO+Dz},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},Du-1],Sel,NewSt);
        true ->
            do_move1({XO,YO,ZO},DuOrg,[{Cx,Cy,Cz},{XO,YO,ZO},Wo,{Ax,Ay,Az},{Fx,Fy,Fz},DuOrg],Rest,NewSt)
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

