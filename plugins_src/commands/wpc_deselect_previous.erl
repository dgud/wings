%%
%%  wpc_deselect_previous.erl --
%%
%%    Subtract current selection from previous selection.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_deselect_previous).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

init() ->
    true.

%%% Menu
menu({select},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [deselect_previous(), separator|NewMenu];
parse([A = {_,inverse,_}|Rest], NewMenu, false) ->
    parse(Rest, [deselect_previous(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

deselect_previous() ->
    {?__(1,"Deselect Previous"),deselect_previous,
     ?__(2,"Subtract the previous selection state from the current selection")}.

%%% Commands
command({select,deselect_previous}, St) ->
    {save_state,deselect_previous(St)};
command(_, _) ->
    next.

deselect_previous(St) ->
    PrevSt = wings_undo:undo(St),
    deselect_previous(PrevSt, St).

deselect_previous(#st{sel=PrevSel,selmode=Mode}, #st{sel=Sel,selmode=Mode}=St) ->
    NewSel = subtract(Sel, PrevSel),
    St#st{sel=NewSel};
deselect_previous(PrevSt0, #st{selmode=Mode}=St) ->
    PrevSt = wings_sel_conv:mode(Mode, PrevSt0),
    deselect_previous(PrevSt, St).

subtract([{Id1,_}=E1|Es1], [{Id2,_}|_]=Set2) when Id1 < Id2 ->
    [E1|subtract(Es1, Set2)];
subtract([{Id1,_}|_]=Set1, [{Id2,_}|Es2]) when Id1 > Id2 ->
    subtract(Set1, Es2);
subtract([{Id,E1}|Es1], [{Id,E2}|Es2]) ->
    E = gb_sets:subtract(E1, E2),
    case gb_sets:is_empty(E) of
        true -> subtract(Es1, Es2);
        false -> [{Id,E}|subtract(Es1, Es2)]
    end;
subtract([], _Es2) -> [];
subtract(Es1, []) -> Es1.
