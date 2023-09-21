%%
%%  wpc_duplicate_radial.erl --
%%
%%    Duplicates selected objects placing them around an axis.
%%
%%  Copyright (c) 2023 Micheus Vieira.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_duplicate_radial).

-include_lib("wings/src/wings.hrl").
-export([init/0,menu/2,command/2]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({body}, Menu0) ->
    case parse_menu(Menu0,[]) of
        [] -> Menu0 ++ menu();
        Menu -> Menu
    end;
menu(_, Menu) -> Menu.


menu() ->
    [{menu(caption),sub_menu(),menu(info)}].

menu(caption) ->
    ?__(1,"Duplicate Radial");
menu(info) ->
    ?__(2,"Duplicate selected objects around an axis");
menu(pick) ->
    ?__(3,"Pick");
menu(pick_info) ->
    ?__(4,"Pick axis to duplicate the object around").


parse_menu([],_) -> [];
parse_menu([{_,{duplicate,_}}=Duplicate|Rest],Acc) ->
    lists:reverse([Duplicate|Acc]) ++ menu() ++ Rest;
parse_menu([Elem|Rest],Acc) ->
    parse_menu(Rest, [Elem|Acc]).

sub_menu() ->
    {radial_dup,[{wings_s:dir(x), x, wings_s:dir(radial_x)},
                 {wings_s:dir(y), y, wings_s:dir(radial_y)},
                 {wings_s:dir(z), z, wings_s:dir(radial_z)},
                 separator,
                 {menu(pick), pick, menu(pick_info)}]}.

command({body,{radial_dup,Ask}}, St) -> radial_duplicator(Ask, St);
command(_, _) -> next.

radial_duplicator(pick, St) ->
    wings:ask(selection_ask([axis,center,point]), St, fun radial_duplicator/2);
radial_duplicator(Dir, St) when is_atom(Dir) ->
    SelRef = wings_sel:center(St),
    radial_duplicator({Dir,e3d_vec:zero(),SelRef}, St);
radial_duplicator({_,_,_}=Params, #st{sel=Sel}=St) ->
    Qs = readial_dup_dlg(Params,length(Sel)),
    wings_dialog:dialog_preview({body,radial_dup}, true, menu(caption), Qs, St);
radial_duplicator(Params, #st{onext=NextId0,shapes=Shs0}=St) ->
    CF = fun(_Items, We, Acc) ->
            duplicate_rotate(Params, We, Acc)
         end,
    {NextId,Shs} = wings_sel:fold(CF, {NextId0,Shs0}, St),
    St#st{onext=NextId,shapes=Shs}.

readial_dup_dlg(Dir, SelCount) ->
    Seed = {erlang:phash2([node()]),erlang:monotonic_time(),erlang:unique_integer()},
    Hook = fun(Key, Value, Sto) ->
               if (SelCount == 1) ->
                   case Key of
                       4 ->
                           Shift = wings_dialog:get_value(7,Sto),
                           if (abs(Value) > 360.0) and (abs(Shift) == 0.0) ->
                               wings_dialog:set_value(4,360.0,Sto),
                               wings_dialog:update(4,Sto);
                           true -> ignore
                           end;
                       5 ->
                           %% Disable random rotation if keep orientation
                           wings_dialog:enable(6,not Value,Sto);
                       _ -> ignore
                   end;
               true ->
                   wings_dialog:enable(5,false,Sto),
                   wings_dialog:enable(6,false,Sto)
               end
           end,
    [{value, Dir},
     {value, Seed},
     {label_column, [
        {?__(1,"Duplicate"), {text, 1, [{range,{1,infinity}}]}},
        {?__(2,"Angle"), {text, 360.0, [{hook,Hook},{info,?__(8,"Limited to 360° without shift on axis")}]}},
        separator,
        {?__(3,"Keep orientation"), {"", false, [{hook,Hook},{info,?__(9,"Only when one object is selected")}]}},
        {?__(4,"Rotate randomly"), {"", false, [{hook,Hook},{info,?__(9,"Only when one object is selected")}]}},
        separator,
        {?__(5,"Shift on axis"), {text, 0.0, []}},
        {?__(6,"Shift on radius"), {text, 0.0, []}}
     ]}
    ].

duplicate_rotate([{Axis, Center, SelRef}, Seed, Count0, Angle, Oriented, RandRot, AxisShift, RadShift], We, Acc) ->
    rand:seed(exs64, Seed),
    if abs(Angle) == (360.0) ->
        AngOfs = Angle/(Count0+1);
    true ->
        AngOfs = Angle/Count0
    end,

    AxisOfs = AxisShift/(Count0),
    RadOfs = RadShift/(Count0),
    clone_add_sel(We, Count0, [Center,wings_util:make_vector(Axis),SelRef,
                              Oriented,RandRot,AngOfs,AxisOfs,RadOfs], Acc).

clone_add_sel(_, 0, _, Acc) -> Acc;
clone_add_sel(#we{name=Name0,vp=Vtab0}=We0, Count,
              [Center,Axis,SelRef,Oriented,RandRot,
               AngOfs,AxisOfs,RadShift]=Params, {Id,Shs0}) ->
    Name = new_name(Name0, Id),
    Vtab = do_rotate(Center, Axis, SelRef, Oriented, RandRot,
                     AxisOfs*Count, RadShift*Count, AngOfs*Count, Vtab0),
    We = We0#we{id=Id,name=Name,vp=Vtab},
    Shs = gb_trees:insert(Id, We, Shs0),
    clone_add_sel(We0, Count-1, Params, {Id+1,Shs}).

new_name(OldName, Id) ->
    OldName ++ "_dup" ++ integer_to_list(Id).

do_rotate(Center, Axis, SelRef, Oriented, RandRot, AxisOfs0, RadShift0, Angle, VsPos0) ->
    RadVec = e3d_vec:norm(e3d_vec:sub(SelRef,Center)),
    RadShift = e3d_vec:mul(RadVec, RadShift0),
    M3 = e3d_mat:translate(e3d_vec:neg(Center)),
    M2 = e3d_mat:mul(e3d_mat:rotate(Angle, Axis),M3),
    if Oriented ->
        ObjCenter0 = e3d_vec:add(SelRef,RadShift),
        NewCenter = e3d_mat:mul_point(M2, ObjCenter0),
        Offset = e3d_vec:sub(NewCenter,SelRef),
        M1 = e3d_mat:translate(Offset),
        VsPos = VsPos0;
    true ->
        if RandRot ->
            M6 = e3d_mat:translate(SelRef),
            M5 = e3d_mat:mul(M6, e3d_mat:rotate(rand:uniform()*360.0, Axis)),
            M4 = e3d_mat:mul(M5, e3d_mat:translate(e3d_vec:neg(SelRef))),
            VsPos =
                array:sparse_foldl(fun(V, Pos0, Acc) ->
                                       Pos = e3d_mat:mul_point(M4, Pos0),
                                       array:set(V, Pos, Acc)
                                   end, VsPos0, VsPos0);
        true ->
            VsPos = VsPos0
        end,

        M1 = e3d_mat:mul(M2,e3d_mat:translate(RadShift))
    end,
    AxisOfs = e3d_vec:mul(Axis, AxisOfs0),
    M0 = e3d_mat:translate(e3d_vec:add(Center,AxisOfs)),
    M = e3d_mat:mul(M0,M1),
    array:sparse_foldl(fun(V, Pos0, Acc) ->
                           Pos = e3d_mat:mul_point(M, Pos0),
                           array:set(V, Pos, Acc)
                       end, VsPos, VsPos).

selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) -> lists:reverse(Ask);
selection_ask([axis|Rest],Ask) ->
    Desc = ?__(1,"Pick axis for rotation"),
    selection_ask(Rest,[{axis,Desc}|Ask]);
selection_ask([center|Rest],Ask) ->
    Desc = ?__(2,"Pick axis center location"),
    selection_ask(Rest,[{point,Desc}|Ask]);
selection_ask([point|Rest],Ask) ->
    Desc = ?__(3,"Select reference point on selection"),
    selection_ask(Rest,[{point,Desc}|Ask]).
