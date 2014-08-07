%%
%%  wpc_absolute_rotate.erl --
%%
%%     Plug-in for rotate -> absolute
%%
%%  Copyright (c) 2014 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_absolute_rotate).

-include("wings.hrl").

-export([init/0,menu/2,command/2]).

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
    [{?__(2,"Rotate"),rotate_fun(Mode),
      {?__(3,"Rotate selection and align reference and target elements."),
       ?__(4,"Rotate selection and align reference and target elements by using two reference axis."),
       ?__(5,"Rotate selection and display numeric entry.")},[]}].

rotate_fun(Mode) ->
    fun(1, _Ns) ->
	    {Mode,{absolute,lrotate}};
       (2, _Ns) ->
	    {Mode,{absolute,mrotate}};
       (3, _Ns) ->
	    {Mode,{absolute,rrotate}};
       (_, _) -> ignore
    end.

command({_,{absolute,Mode}},St) when Mode == lrotate; Mode == mrotate; Mode == rrotate ->
            case Mode of
                lrotate -> wings:ask(selection_ask([from_axis,to_axis,reference,axis_point]), St, fun lrotate/2);
                mrotate -> wings:ask(selection_ask([from_axis,to_axis,reference,target,axis_ref,axis_target]), St, fun mrotate/2);
                rrotate -> wings:ask(selection_ask([from_axis,to_axis,reference,target,axis_ref,axis_target]), St, fun rrotate/2)
            end;
command(_,_) -> next.

%%%
%%% absolute rotate (LMB option)
%%%
lrotate({FromAxis, ToAxis, From, Axis}, St) ->
    Sel = get_selection(false,St),
    Angle = calc_angle(FromAxis, ToAxis, Axis),
    {save_state,rotate([From,none,Axis,Angle],Sel,St)}.

%%%
%%% absolute rotate align axis and moving to target (MMB option)
%%%
mrotate({FromAxis, ToAxis, From, To, RefAxis, TargAxis}, St) ->
    Sel = get_selection(false,St),
    Norm = e3d_vec:normal(RefAxis,{0.0,0.0,0.0},TargAxis),
    Angle0 = calc_angle(RefAxis, TargAxis, Norm),
    St0=rotate([From,none,Norm,Angle0],Sel,St),

    M=e3d_mat:rotate(Angle0,Norm),
    FromAxis0=e3d_vec:norm(e3d_mat:mul_point(M, FromAxis)),
    Angle1 = calc_angle(FromAxis0, ToAxis, TargAxis),
    {save_state,rotate([From,To,TargAxis,Angle1],Sel,St0)}.

%%%
%%% absolute rotate with numeric entry (RMB option)
%%%
rrotate({_FromAxis, _ToAxis, _From, _To, _RefAxis, _TargAxis}=Params, #st{shapes=Shapes}=St) ->
    Sel = get_selection(false,St),
    Lights = get_lights(Sel,Shapes),
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
    draw_window(erlang:append_element(Params,MoveObj),Sel,St).

%%
%% draw_window(Options,Selection,State)
%%
%% functions that draws interface and translates entered options for further processing
%%  and calls translate(ProcessedOptions,Parameters,Selection,State)
%%

draw_window({_FromAxis, _ToAxis, From, To, _RefAxis, _TargAxis, MoveObj}=Options, Sel, #st{selmode=_SelMode}=St) ->
    Frame1 = [{vframe, [draw_window1(center,To)]}],
    Frame2 = [draw_window1(lock,default)],
    Frame25 = [draw_window1(object,MoveObj)],
    Frame3 = if
                 MoveObj =/= duplionly ->
                     [draw_window1(duplicate,true)];
                 true ->
                     []
             end,
    Frame35 = [draw_window1(distance,e3d_vec:dist(To,From))],
    Frame4 = [draw_window1(dup_rt,default)]++
             [draw_window1(dup_around,default)]++
             [draw_window1(twist,default)]++
             [draw_window1(scale,default)],

    Frame5 = if
                 Frame2 =/= [] ->
                     [{hframe,Frame1++Frame2}]++[{vframe,Frame25++Frame3++Frame35}];
                 true ->
                     [{vframe,Frame1++Frame25++Frame3++Frame35}]
             end,
    Frame = [{vframe,Frame5++Frame4++[separator,draw_window1(reference,From)]}],
    Name = draw_window1(name,default),
    wings_ask:dialog(Name, {preview,Frame},
       fun (cancel) -> St;
           ({dialog_preview,Rotate}) ->
               {preview,St,translate(Rotate,Options,Sel,St)};
           (Rotate) ->
               {commit,St,translate(Rotate,Options,Sel,St)}
       end).

draw_window1(name,_) ->
    ?__(1,"Absolute rotate options");
draw_window1(center,{XC,YC,ZC}) ->
    {vframe,[
        {hframe,[{label,?__(2,"Set position")++":"}]},
        {hframe,[{label,"X:"},{text,XC,[{key,x},disable(lx)]}]},
        {hframe,[{label,"Y:"},{text,YC,[{key,y},disable(ly)]}]},
        {hframe,[{label,"Z:"},{text,ZC,[{key,z},disable(lz)]}]}
    ]};
draw_window1(distance,Dist) ->
    {vframe,[
      {hframe,[
        {label,?__(10,"Distance")++":"},
        {text,Dist,[{key,dist}]}
      ],[disable(dup_rt)]}
    ]};
draw_window1(object,one) ->
    {vframe, [{?__(3,"Move object"),false,[{key,all}]}]};
draw_window1(object,many) ->
    {vframe, [{?__(4,"Move objects"),false,[{key,all}]}]};
draw_window1(object,duplionly) ->
    draw_window1(duplicate,false);
draw_window1(duplicate,CheckAll) when is_boolean(CheckAll) ->
    Label = if
        CheckAll -> [disable(all)];
        true -> []
    end,
    {vframe,[
      {hframe,[
        {text,0,[{key,dupli},{range,{0,infinity}},Label]},
        {label,?__(5,"Duplicates")}
      ],[disable(dupli)]}
    ]};
draw_window1(dup_rt,_) ->
    {vframe,[
      {hframe,[
        {label,?__(9,"Between reference and target")++" "},
        {"",false,[{key,dup_rt}]}
      ],[disable(dup_rt)]}
    ]};
draw_window1(twist,_) ->
    {vframe,[
        {vframe,[
          {hframe,[
            {label,?__(14,"Degrees")},
            {text,0.0,[{key,twist_ax},{width,5}]}
          ]},
          {hradio,[
            {?__(15,"Target"),target},
            {?__(16,"From previous object"),object}],target,
            [{title,?__(13,"Reference axis")},{key,twist_op_ax}]},
          {hradio,[
            {?__(18,"Preserve"),preserve},
            {?__(19,"Aligned to direction vector"),direction}],direction,
            [{title,?__(20,"Objects alignment")},{key,twist_ob_al},
             {info, ?__(21,"Direction vector is defined by the source-target segment")}]}
      ],[{title, ?__(12,"Around the target axis")}]},
        {vframe,[
          {hframe,[
            {label,?__(22,"Degrees")},
            {text,0.0,[{key,twist_dir},{width,5}]}
          ]}
      ],[{title, ?__(17,"Around the direction vector")}]}
    ],[{title,?__(11,"Twist options")},disable(dup_rt)]};
draw_window1(dup_around,_) ->
    {vframe,[
      {hframe,[
        {text,0.0,[{key,degrees},{width,5}]},
        {label,?__(24,"Degrees")++"  "},
        {text,0,[{key,steps},{width,3},{range,{0,infinity}}]},
        {label,?__(25,"Steps")}
      ],[disable(dupli)]}
    ],[{title,?__(23,"Duplicate around options")}]};
draw_window1(scale,_) ->
    {vframe,[
      {hframe,[
        {label,?__(28,"In")++":"},
        {text,100.0,[{key,scl_in},{width,5},{range,{0.0001,infinity}}]},
        {label,"%"++"  "},
        {label,?__(29,"Out")++":"},
        {text,100.0,[{key,scl_out},{width,5},{range,{0.0001,infinity}}]},
        {label,"%"}
      ],[{title, ?__(27,"Objects")}]},
      {hframe,[
        {label,?__(28,"In")++":"},
        {text,100.0,[{key,scl_seg_in},{width,5},{range,{0.0001,infinity}}]},
        {label,"%"++"  "},
        {label,?__(29,"Out")++":"},
        {text,100.0,[{key,scl_seg_out},{width,5},{range,{0.0001,infinity}}]},
        {label,"%"}
      ],[{title, ?__(30,"Distance segments")}]}
    ],[{title,?__(26,"Scale options")},disable(dup_rt)]};
draw_window1(lock, _) ->
    {vframe,[
        {hframe,[{label,?__(8,"Lock")++":"}]},
        {hframe,[{"",false,[{key,lx}]}]},
        {hframe,[{"",false,[{key,ly}]}]},
        {hframe,[{"",false,[{key,lz}]}]}
    ]};
draw_window1(reference,{X,Y,Z}) ->
    {label,?__(7,"Reference point is") ++ ": (" ++
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
                  ((gb_trees:is_defined(all,Store)) andalso (not gb_trees:get(all, Store)));
              (_, _) -> void
          end};
disable(dup_rt) ->
    {hook,fun (is_disabled, {_Var,_I,Store}) ->
                  (gb_trees:get(dupli, Store) < 1) and
                  (not (gb_trees:is_defined(all,Store) andalso (gb_trees:get(all, Store))));
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

translate(Options, {FromAxis, ToAxis, {CX,CY,CZ}=From, _To, RefAxis, TargAxis, _}, _Sel, St) ->
    Obj = lookup(all,Options,true),
    Sel = get_selection(Obj,St),
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
    Dist = lookup(dist, Options, 0.0),
    DirVec = e3d_vec:norm_sub({NX,NY,NZ},From),
    Obj = lookup(all,Options,true),
    Dupli = case lookup(dupli,Options,0) of
                N when Obj -> N;
                _ -> 0
            end,
    Dup_rt = lookup(dup_rt,Options,false),
    TwistAx = lookup(twist_ax,Options,0.0),
    TwOpAx = lookup(twist_op_ax,Options,target),
    TwistDir = lookup(twist_dir,Options,0.0),
    TwObjAl = lookup(twist_ob_al,Options,direction),
    Steps = lookup(steps,Options,0.0),
    Degrees = lookup(degrees,Options,0.0),
    SclIn = lookup(scl_in,Options,100.0),
    SclOut = lookup(scl_out,Options,100.0),
    SclSegIn = lookup(scl_seg_in,Options,100.0),
    SclSegOut = lookup(scl_seg_out,Options,100.0),
    SclOff = if (Dupli > 0) ->
        (SclOut-SclIn)/Dupli;
    true ->
        0.0
    end,
    SclSegOff = if (Dupli > 0) ->
        (SclSegOut-SclSegIn)/Dupli;
    true ->
        0.0
    end,
    rotate(FromAxis,ToAxis,RefAxis,TargAxis,Dupli,Dup_rt,
           {TwistAx,TwOpAx,TwistDir,TwObjAl},Steps,Degrees,
           {SclIn,SclOff,SclSegIn,SclSegOff},DirVec,Dist,[From,{NX,NY,NZ},Dupli],Sel,St).

rotate(_,[],St0) ->
    St0;
rotate([From,To,TargAxis,Angle]=Params,[{Obj,Vset}|Rest],#st{shapes=Shapes}=St) ->
    #we{vp=Vtab} = We = gb_trees:get(Obj, Shapes),
    NewVtab = execute_rotate([From,To,TargAxis,Angle,none],Vset,Vtab),
    NewShapes = gb_trees:update(Obj,We#we{vp=NewVtab},Shapes),
    rotate(Params,Rest,St#st{shapes=NewShapes}).

%% Absolute Rotate - RMB option routine
rotate(_,_,_,_,_,_,_,_,_,_,_,_,_,[],St) -> St;
rotate(FromAx,ToAx,RefAx,TargAx,Dupli,Dup_rt,TwistOpt,Steps,Degrees,SclOpt,DirVec,Dist,[From0,To0,Du],[{Obj0,Vset}|Rest]=Sel,#st{shapes=Shapes0}=St0) ->
    {TwistAx,TwOpAx,TwistDir,TwObjAl} = TwistOpt,
    {SclIn,SclOff,SclSegIn,SclSegOff} = SclOpt,
    We0 = gb_trees:get(Obj0, Shapes0),
    #st{shapes=Shapes1,onext=Oid} = St1 = if
                                              Du > 0 ->
                                                  wings_shape:insert(We0, copy, St0);
                                              true ->
                                                  St0
                                          end,
    We1 = if
        Du > 0 ->
            Obj1 = Oid-1,
            gb_trees:get(Obj1, Shapes1);
        true ->
            Obj1 = Obj0,
            We0
    end,
    #we{vp=Vtab0} = We1,
    Norm = e3d_vec:normal(RefAx,{0.0,0.0,0.0},TargAx),
    Angle0 = calc_angle(RefAx, TargAx, Norm),
    NewVtab0 = execute_rotate([From0,none,Norm,Angle0,none],Vset,Vtab0),

    Scale = SclIn+(SclOff*Du),
    M = e3d_mat:rotate(Angle0,Norm),
    FromAx0 = e3d_vec:norm(e3d_mat:mul_point(M, FromAx)),
    Angle1 = calc_angle(FromAx0, ToAx, TargAx),
    NewVtab = execute_rotate([From0,To0,TargAx,Angle1,Scale],Vset,NewVtab0),

    Vtab = if
        Du > 0 ->  % it has Duplicate set
            ScaleSeg = SclSegIn+(SclSegOff*Du),
            CalcDist=fun(Dist0, 0.0) -> Dist0;
                (Dist0, _) -> Dist0*ScaleSeg/100.0
            end,

            NewVtab3 = if TwistDir =/= 0.0 ->  % object twisted around the direction vector
                execute_rotate([To0,none,DirVec,TwistDir*Du,none],Vset,NewVtab);
            true ->
                NewVtab
            end,

            ObjPos = if TwistAx =:= 0.0 ->  % no twist angle defined
                SegDist = if Dup_rt ->  % Duplicate between source->target
                    -Dist/Dupli*Du;
                true ->
                    Dist*Du
                end,
                e3d_vec:add(From0,e3d_vec:mul(DirVec,CalcDist(SegDist,SclSegOff)));
            true ->
                DirVec0 = case TwOpAx of
                    target ->  % Target
                        M0=e3d_mat:rotate(TwistAx/Dupli*Du,TargAx),
                        e3d_vec:norm(e3d_mat:mul_point(M0, DirVec));
                    _ ->  % From previous object
                        DirVec
                end,
                ObjPos0 = case Dup_rt of  % Duplicate between source->target
                    true ->
                        ObjPos9 = case TwOpAx of  % Twist reference axis:
                            target ->  % twist reference axis: Target
                                e3d_vec:mul(DirVec0,CalcDist(Dist/Dupli*Du,SclSegOff));  %%% OK - target
                            _ ->
                                calc_twisted_location(DirVec0,CalcDist(Dist/Dupli,SclSegOff),TargAx,TwistAx,Du)  %%% OK - from previous object
                        end,
                        e3d_vec:mul(ObjPos9,-1.0);
                    _ ->
                        case TwOpAx of  % Twist reference axis:
                            target ->  % Target
                                e3d_vec:mul(DirVec0,CalcDist(Dist*Du,SclSegOff));  %%% OK - target
                            _ ->  % From previous object
                                calc_twisted_location(DirVec0,CalcDist(Dist,SclSegOff),TargAx,TwistAx,Du)  %%% OK - from previous object
                        end
                end,
                e3d_vec:add(From0,ObjPos0)
            end,
            NewVtab2 = case TwObjAl of
                direction ->
                    case TwOpAx of  % Twist reference axis:
                        target ->  % Target
                            execute_rotate([To0,none,TargAx,TwistAx/Dupli*Du,none],Vset,NewVtab3);  %%% OK - target
                        _ ->  % From previous object
                            execute_rotate([To0,none,TargAx,TwistAx*Du,none],Vset,NewVtab3)  %%% OK - from previous object
                    end;
                _ ->
                    NewVtab3
            end,
            execute_translate([From0,ObjPos],Vset,NewVtab2);  %%% OK - target / from previous object
    true ->
        NewVtab
    end,

    NewWe = We1#we{vp=Vtab},
    NewShapes = gb_trees:update(Obj1,NewWe,Shapes1),
    NewSt0=St1#st{shapes=NewShapes},

    NewSt = if
        (Steps > 0) and (Degrees =/= 0.0) ->  % duplicating around
            Deg0 = Degrees/(Steps+1),
            lists:foldr(fun(Step, St2) ->
                #st{shapes=Shp1,onext=Oid1} = St3 = wings_shape:insert(NewWe, copy, St2),
                Obj2=Oid1-1,
                We2=gb_trees:get(Obj2, Shp1),
                NewVtab1 = execute_rotate([To0,none,TargAx,Deg0*Step,none],Vset,Vtab),
                NewWe1 = We2#we{vp=NewVtab1},
                NewShapes0 = gb_trees:update(Obj2,NewWe1,Shp1),
                St3#st{shapes=NewShapes0}
            end,NewSt0,lists:seq(1,Steps));
    true ->
        NewSt0
    end,

    if Du > 0 ->
        rotate(FromAx,ToAx,RefAx,TargAx,Dupli,Dup_rt,TwistOpt,Steps,Degrees,SclOpt,DirVec,Dist,[From0,To0,Du-1],Sel,NewSt);
    true ->
        rotate(FromAx,ToAx,RefAx,TargAx,Dupli,Dup_rt,TwistOpt,Steps,Degrees,SclOpt,DirVec,Dist,[From0,To0,Dupli],Rest,NewSt)
    end.

calc_twisted_location(SDir, SLen, Axis, Ang, Dup) ->
    calc_twisted_location_0(SDir,SLen,Axis,Ang,Dup, {0.0,0.0,0.0}).

calc_twisted_location_0(_, _, _, _, 0, Pto) -> Pto;
calc_twisted_location_0(SDir1, SLen, Axis, Ang, Dup, Pto) ->
    M=e3d_mat:rotate(Ang,Axis),
    SDir0=e3d_vec:mul(e3d_vec:norm(SDir1),SLen),
    SDir=e3d_mat:mul_point(M, SDir0),
    calc_twisted_location_0(SDir,SLen,Axis,Ang,Dup-1,e3d_vec:add(Pto,SDir)).

execute_rotate([From, To, Axis, Angle, Scale], Vset, Vtab) ->
    {Cx,Cy,Cz}=From,
    M0 = case To of
        {Cxo,Cyo,Czo} -> e3d_mat:translate(Cxo,Cyo,Czo);
        _ -> e3d_mat:translate(Cx, Cy, Cz)
    end,
    M2 = case Scale of
        none -> M0;
        _ ->
            S = Scale/100.0,
            e3d_mat:mul(M0, e3d_mat:scale(S,S,S))
    end,
    M1 = e3d_mat:mul(M2, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    calc_transformation(M, Vset, Vtab).

execute_translate([{Sx,Sy,Sz}, {Dx,Dy,Dz}], Vset, Vtab) ->
    M0 = e3d_mat:translate(-Sx,-Sy,-Sz),
    M = e3d_mat:mul(M0, e3d_mat:translate(Dx,Dy,Dz)),
    calc_transformation(M, Vset, Vtab).

calc_transformation(M, Vset, Vtab) ->
    lists:foldl(fun(V, Acc) ->
        Vs0=array:get(V,Acc),
        Vs=e3d_mat:mul_point(M, Vs0),
        array:set(V,Vs,Acc)
    end, Vtab, gb_sets:to_list(Vset)).

%%%
%%% some helpful test and investigation functions
%%%
calc_angle(FromAxis, ToAxis, Axis) ->
    FromNorm = e3d_vec:normal(Axis,{0.0,0.0,0.0},FromAxis),
    ToNorm = e3d_vec:normal(Axis,{0.0,0.0,0.0},ToAxis),
    Degree = e3d_vec:degrees(ToNorm,FromNorm),
    Norm = e3d_vec:normal(ToNorm,{0.0,0.0,0.0},FromNorm),
    Size = abs(e3d_vec:len(e3d_vec:add(Axis,Norm))),
    if Size < 1.0 -> -Degree;
      true -> Degree
    end.

get_selection(true,St) ->
    St0=wings_sel_conv:mode(body, St),
    get_selection(false,St0);
get_selection(_,#st{selmode=SelMode}=St) ->
    #st{sel=Sel} = case SelMode of
        vertex -> St;
        _ -> wings_sel_conv:mode(vertex,St)
    end,
    Sel.

selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face, body]}.

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([reference|Rest],Ask) ->
    Desc = ?__(1,"Define point through which rotate axis will pass"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([target|Rest],Ask) ->
    Desc = ?__(2,"Select target point for snap operation"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([from_axis|Rest],Ask) ->
    Desc = ?__(3,"Define reference element on selection (to be rotated)"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([to_axis|Rest],Ask) ->
    Desc = ?__(4,"Define target element on stationary object"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([axis_point|Rest],Ask) ->
    Desc = ?__(5,"Define rotate axis. (Both reference and target elements lie on one of its axial planes)"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([axis_ref|Rest],Ask) ->
    Desc = ?__(6,"Define rotate axis for reference element (it will match to axis for target element)"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([axis_target|Rest],Ask) ->
    Desc = ?__(7,"Define rotate axis for target element"),
    selection_ask(Rest,[{axis,Desc}|Ask]).

check_whole_obj(#st{selmode=SelMode}=St0) ->
    St1 = wings_sel_conv:mode(body,St0),
    St2 = wings_sel_conv:mode(SelMode,St1),
    St2#st.sel == St0#st.sel.

get_lights(Sel,Shapes) ->
    get_lights(Sel,Shapes,true).

get_lights([],_,Lights) ->
    Lights;
get_lights([{Obj,_}|Rest],Shapes,Lights) ->
    We = gb_trees:get(Obj, Shapes),
    get_lights(Rest,Shapes,Lights and ?IS_LIGHT(We)).

check_single_obj([{_,_}]) -> true;
check_single_obj(_) -> false.

check_single_vert(L) ->
    lists:all(fun({_,GbSet}) -> gb_sets:size(GbSet) =:= 1 end, L).

