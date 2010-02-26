%%
%%  wings_menu.erl --
%%
%%     Implementation of pulldown and popup menus.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_menu).
-export([is_popup_event/1,menu/5,popup_menu/4,build_command/2,
	 kill_menus/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,keyfind/3,foreach/2]).

-define(SUB_MENU_TIME, 150).
-define(SEPARATOR_HEIGHT, (wings_text:height()-4)).
-define(INITIAL_LEVEL, 1).

-type unicode_char() :: 0..2097151.
-type unicode_string() :: list(unicode_char()).

-type menu_help() :: unicode_string() |
  {unicode_string(),unicode_string(),unicode_string()}.

-type menu_item() :: {unicode_string(),any(),menu_help(),unicode_string(),list()} |
  'separator'.

%% Menu information kept for menus.
-record(mi,
	{ymarg :: non_neg_integer(),		%Margin at top and bottom
	 shortcut :: non_neg_integer(),		%Position for shortcut (pixels)
	 w :: pos_integer(),			%Width of menu (pixels)
	 h :: pos_integer(),			%Height of menu (pixels)
	 hs :: list(pos_integer()),		%Height of each entry.
	 sel=none :: 'none'|pos_integer(),	%Selected item (1..tuple_size(Menu))
	 sel_side=left :: 'left'|'right',	%Selection on left or right.
	 ns=[],					%Name stack.
	 menu :: tuple(menu_item()),		%Normalized menu.
	 timer=make_ref(),			%Active submenu timer.
	 level=?INITIAL_LEVEL,			%Menu level.
	 type :: 'plain'|'popup',	        %Type of menu.
	 owner,					%Owning window.
	 flags=[] :: list()			%Flags (magnet/dialog).
	}).

%%%
%%% Inside this module, each entry in a menu is kept in the following
%%% normalized format:
%%%
%%%   separator      OR
%%%   {Text,Name,Hotkey,Help,Properties}
%%%
%%%   The Help field is normalized to
%%%      String       for plain (pull-down) menus
%%%      {L,M,R}      for pop-up menus
%%%

is_popup_event(#mousebutton{button=3,x=X0,y=Y0,state=State,mod=Mod}) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    case State of
	?SDL_RELEASED ->
	    {yes,X,Y,Mod};
	_Other -> no
    end;
is_popup_event(_Event) -> no.

menu(X, Y, Owner, Name, Menu) ->
    menu_setup(plain, X, Y, Name, Menu, #mi{owner=Owner}).

popup_menu(X, Y, Name, Menu) ->
    menu_setup(popup, X, Y, Name, Menu, #mi{owner=wings_wm:this()}).

menu_setup(Type, X0, Y0, Name, Menu0, #mi{ns=Names0}=Mi0) ->
    Names = [Name|Names0],
    Menu1 = wings_plugin:menu(list_to_tuple(reverse(Names)), Menu0),
    Hotkeys = wings_hotkey:matching(Names),
    Menu = normalize_menu(Menu1, Hotkeys, Type =:= popup),
    {MwL,MwM,MwR,Hs} = menu_dims(Menu),
    Cw = wings_text:width(),
    TotalW = MwL + MwM + MwR + 8* Cw,
    Mh = lists:sum(Hs),
    Margin = 3,
    InfoLine = ?CHAR_HEIGHT + 14,
    {X1,Y1} = case Type of
		  plain ->
		      {X0,Y0};
		  popup ->
		      {X0-TotalW div 2,Y0 - Margin - ?CHAR_HEIGHT}
	      end,
    {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin+InfoLine, Mi0),
    W = TotalW-10,
    Mi = Mi0#mi{ymarg=Margin,shortcut=MwL+Cw,w=TotalW-10,h=Mh,hs=Hs,
		sel=none,ns=Names,menu=Menu,type=Type},
    #mi{level=Level} = Mi,
    setup_menu_killer(Mi),
    Op = {seq,push,get_menu_event(Mi)},
    WinName = {menu,Level},
    wings_wm:delete({menu,Level}),
    wings_wm:new(WinName, {X,Y,highest}, {W,Mh+10}, Op),
    delete_from(Level+1).

delete_from(Level) ->
    Name = {menu,Level},
    case wings_wm:is_window(Name) of
	false -> keep;
	true ->
	    wings_wm:delete(Name),
	    delete_from(Level+1)
    end.

setup_menu_killer(#mi{owner=Owner,level=Level}) ->
    %% The menu killer window lies below all menus, covering the entire screen.
    %% It will kill all menus if clicked.
    case wings_wm:is_window(menu_killer) of
	true ->
	    if
		Level =:= ?INITIAL_LEVEL ->
		    %% A new top-level menu will be created. Make sure
		    %% that the menu killer window will be directly below it.
		    wings_wm:raise(menu_killer),
		    raise_menubar(Owner);
		true ->
		    %% A sub-menu will be created. Don't move the menu killer.
		    ok
	    end;
	false ->
	    Op = {push,fun(Ev) -> menu_killer(Ev, Owner) end},
	    {TopW,TopH} = wings_wm:top_size(),
	    wings_wm:new(menu_killer, {0,0,highest}, {TopW,TopH}, Op),
	    raise_menubar(Owner)
    end.

raise_menubar(Owner) ->
    Menubar = {menubar,Owner},
    case wings_wm:is_window(Menubar) of
	false -> ok;
	true -> wings_wm:raise(Menubar)
    end.

menu_killer(#mousebutton{button=1,state=?SDL_PRESSED}, Owner) ->
    wings_wm:notify(menu_aborted),
    kill_menus(Owner);
menu_killer(#keyboard{sym=27}, Owner) -> %Escape.
    wings_wm:notify(menu_aborted),
    kill_menus(Owner);
menu_killer(kill_menus, Owner) ->
    kill_menus(Owner);
menu_killer(_, _) -> keep.

kill_menus(Owner) ->
    foreach(fun({menu,_}=Name) ->
		    wings_wm:delete(Name);
	       ({menubar,Win}=Name) when Win =:= Owner ->
		    wings_wm:send(Name, clear_menu_selection);
	       (_) -> ok
	    end, wings_wm:windows()),
    delete.

menu_show(#mi{ymarg=Margin,shortcut=Shortcut,w=Mw,h=Mh}=Mi) ->
    wings_io:blend(wings_pref:get_value(menu_color),
		   fun(Color) ->
			   wings_io:border(0, 0, Mw-1, Mh + 2*Margin+3, Color)
		   end),
    menu_draw(3*?CHAR_WIDTH, Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi#mi.hs, Mi).

normalize_menu(Menu, Hotkeys, Adv) ->
    normalize_menu(Menu, Hotkeys, Adv, []).

normalize_menu([[_|_]=List|Els], Hotkeys, Adv, Acc) ->
    normalize_menu(List++Els, Hotkeys, Adv, Acc);
normalize_menu([Elem0|Els], Hotkeys, Adv, Acc) ->
    Elem1 = case Elem0 of
		{S,Name,Help,Ps} ->
		    {S,Name,[],Help,Props=Ps};
		{S,Name,[C|_]=Help} when is_integer(C) ->
		    {S,Name,[],Help,Props=[]};
		{S,Name,Ps} ->
		    {S,Name,[],[],Props=Ps};
		{S,Name} ->
		    {S,Name,[],[],Props=[]};
		separator ->
		    Name = none,
		    Props = [],
		    separator
	    end,
    Elem2 = norm_add_hotkey(Name, Elem1, Hotkeys, Props),
    Elem = norm_help(Elem2, Adv),
    normalize_menu(Els, Hotkeys, Adv, [Elem|Acc]);
normalize_menu([], _Hotkeys, _Adv, Acc) -> list_to_tuple(reverse(Acc)).

norm_add_hotkey(_, separator, _, _) -> separator;
norm_add_hotkey(_, Elem, [], _) -> Elem;
norm_add_hotkey({_,[_|_]}, Elem, _, _) -> Elem;
norm_add_hotkey({Key,Fun}, Elem, Hotkeys, Props) when is_function(Fun) ->
    case Fun(1, []) of
	[_|_] -> Elem;				%Submenu.
	Name0 ->
	    Name = {Key,reduce_ask(Name0)},
	    norm_add_hotkey(Name, Elem, Hotkeys, Props)
    end;
norm_add_hotkey(Fun, Elem, Hotkeys, Props) when is_function(Fun) ->
    Name = reduce_name(Fun(1, [])),
    norm_add_hotkey(Name, Elem, Hotkeys, Props);
norm_add_hotkey({'ASK',_}=Ask, Elem, Hotkeys, Props) ->
    norm_add_hotkey(reduce_ask(Ask), Elem, Hotkeys, Props);
norm_add_hotkey(Name, Elem, Hotkeys, Props) ->
    Key = match_hotkey(Name, Hotkeys, have_option_box(Props)),
    setelement(3, Elem, Key).

match_hotkey(Name, [{Name,Key}|_], false) -> Key;
match_hotkey(Name, [{{Name,false},Key}|_], true) -> Key;
match_hotkey(Name, [{{Name,true},Key}|_], true) -> Key;
match_hotkey(Name, [_|T], OptionBox) ->
    match_hotkey(Name, T, OptionBox);
match_hotkey(_, [], _) -> [].

reduce_name({'ASK',_}=Ask) -> Ask;
reduce_name({Key,{_,_}=Tuple}) when is_atom(Key) ->
    reduce_name(Tuple);
reduce_name({Key,Val}) when is_atom(Key) -> Val;
reduce_name(Name) -> Name.

reduce_ask({'ASK',Ask}) -> reduce_ask_1(Ask);
reduce_ask(Term) -> Term.
    
reduce_ask_1({A,B,_}) -> reduce_ask_1({A,B});
reduce_ask_1({[],[Res]}) -> Res;
reduce_ask_1({[],Res}) ->
    list_to_tuple(reverse(Res));
reduce_ask_1(_) -> none.

norm_help(separator=Item, _) -> Item;
norm_help(Item, false) -> norm_help_basic(Item);
norm_help(Elem, true) ->  norm_help_adv(Elem, {[],[],[]}).

norm_help_basic({_,_,_,Help,_}=Item) when is_list(Help) ->
    Item;
norm_help_basic({T,N,Hot,Help,Ps}) when tuple_size(Help) > 0 ->
    {T,N,Hot,element(1, Help),Ps}.

norm_help_adv({T,N,Hot,[],Ps}, Empty)  ->
    norm_help_adv({T,N,Hot,Empty,Ps}, Empty);
norm_help_adv({T,N,Hot,[_|_]=L,Ps}, Empty)  ->
    norm_help_adv({T,N,Hot,{L,[],[]},Ps}, Empty);
norm_help_adv({T,N,Hot,{L},Ps}, Empty)  ->
    norm_help_adv({T,N,Hot,{L,[],[]},Ps}, Empty);
norm_help_adv({T,N,Hot,{L,M},Ps}, Empty)  ->
    norm_help_adv({T,N,Hot,{L,M,[]},Ps}, Empty);
norm_help_adv({_,_,_,{_,_,[_|_]},_}=Item, _) ->
    Item;
norm_help_adv({T,N,Hot,{Hl,Hm,[]},Ps}=Item, _) ->
    case have_option_box(Ps) of
	false -> Item;
	true ->
	    Hr = ?__(1, "Open option dialog"),
	    {T,N,Hot,{Hl,Hm,Hr},Ps}
    end.

menu_dims(Menu) ->
    menu_dims(Menu, tuple_size(Menu), 0, 0, 0, []).

menu_dims(_Menu, 0, MaxA, MaxB, MaxC, H) -> {MaxA,MaxB,MaxC,H};
menu_dims(Menu, I, MaxA0, MaxB0, MaxC0, Hacc) ->
    {Wa,Wb,Wc,H} =
	case element(I, Menu) of
 	    {S,ignore,[],[],[]} when I == 1 ->
		case wings_text:width([$\s|S]) - (MaxA0+MaxB0+MaxC0) of
		    W when W < 0 ->
			{0,0,0,?LINE_HEIGHT};
		    W ->
			{MaxA0+W,0,0,?LINE_HEIGHT}
		end;
	    {S,{_,_},Hotkey,_,_} ->
		{wings_text:width([$.,$.|S]),wings_text:width(Hotkey),
		 wings_text:width(),?LINE_HEIGHT};
	    {S,_,Hotkey,_,Ps} ->
		{wings_text:width([$.,$.|S]),wings_text:width(Hotkey),
		 right_width(Ps),?LINE_HEIGHT};
	    separator -> {0,0,0,?SEPARATOR_HEIGHT}
	end,
    menu_dims(Menu, I-1, max(Wa, MaxA0), max(Wb, MaxB0),
	      max(Wc, MaxC0), [H|Hacc]).

max(A, B) when A > B -> A;
max(_A, B) -> B.

right_width(Ps) ->
    Cw = wings_text:width(),
    case have_option_box(Ps) of
	true -> Cw;
	false ->
	    case proplists:get_value(color, Ps, none) of
		none -> 0;
		_  -> Cw
	    end
    end.

%%%
%%% Event loop for menus.
%%%

get_menu_event(Mi) ->
    {replace,fun(Ev) -> handle_menu_event(Ev, Mi) end}.

handle_menu_event(redraw, Mi) ->
    redraw(Mi),
    keep;
handle_menu_event(lost_focus, Mi) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {_,H} = wings_wm:win_size(),
    if
	Y < 0 -> mousemotion(X, 0, Mi);
	Y >= H -> mousemotion(X, H-2, Mi);
	true -> keep
    end;
handle_menu_event(#keyboard{}=Ev, Mi) ->
    handle_key(Ev, Mi);
handle_menu_event(#mousemotion{x=X,y=Y}, Mi) ->
    mousemotion(X, Y, Mi);
handle_menu_event(#mousebutton{}=Ev, Mi) ->
    button_pressed(Ev, Mi);
handle_menu_event(#mi{}=Mi, _) ->		%Key bound/unbound.
    help_text(Mi),
    wings_wm:dirty(),
    get_menu_event(Mi);
handle_menu_event(clear_submenu, #mi{level=Level}) ->
    delete_from(Level+1);
handle_menu_event(quit, Mi) ->
    clear_menu_selection(Mi),
    wings_wm:later(quit),
    delete_all(Mi);
handle_menu_event(_, _) -> keep.

clear_menu_selection(#mi{owner=Owner}) ->
    wings_wm:send({menubar,Owner}, clear_menu_selection).

mousemotion(X, Y, Mi0) ->
    Mi1 = update_highlight(X, Y, Mi0),
    Mi = set_submenu_timer(Mi1, Mi0, X, Y),
    get_menu_event(Mi).

button_pressed(#mousebutton{button=B,x=X,y=Y,mod=Mod,state=?SDL_RELEASED},
	       Mi) when B =< 3 ->
    wings_wm:dirty(),
    button_pressed(B, Mod, X, Y, Mi);
button_pressed(_, _) -> keep.

button_pressed(Button, Mod, X, Y, #mi{ns=Names,menu=Menu,type=Type}=Mi0) ->
    clear_timer(Mi0),
    Mi1 = update_highlight(X, Y, Mi0),
    Mi = update_flags(Mod, Mi1),
    case selected_item(Y, Mi) of
	none ->
	    get_menu_event(Mi);
	Item when is_integer(Item) ->
	    case element(Item, Menu) of
		{_,{'VALUE',Act0},_,_,Ps} ->
		    Act = was_option_hit(Button, Act0, X, Ps, Mi),
		    do_action(Act, Names, Ps, Mi);
		{_,{Name,Submenu},_,_,_} when Type =:= popup ->
		    popup_submenu(Button, X, Y, Name, Submenu, Mi);
		{_,{Name,Submenu},_,_,_} when Type =:= plain ->
		    submenu(Item, Name, Submenu, Mi);
		{_,Act0,_,_,Ps} when is_function(Act0) ->
		    call_action(Act0, Button, Names, Ps, Mi);
		{_,Act0,_,_,Ps} when is_atom(Act0); is_integer(Act0) ->
		    Act = was_option_hit(Button, Act0, X, Ps, Mi),
		    do_action(Act, Names, Ps, Mi)
	    end
    end.

call_action(Act, Button, Ns, Ps, Mi) ->
    case Act(Button, Ns) of
	ignore -> keep;
	Cmd0 when is_tuple(Cmd0) ->
	    Cmd = case is_magnet_active(Ps, Mi) of
		      false -> Cmd0;
		      true -> insert_magnet_flags(Cmd0, Mi)
		  end,
	    send_action(Cmd, Mi)
    end.

do_action(Act0, Ns, Ps, Mi) ->
    Act = case is_magnet_active(Ps, Mi) of
	      false -> build_command(Act0, Ns);
	      true -> build_command({'ASK',{[magnet],[Act0]}}, Ns)
	  end,
    send_action(Act, Mi).

send_action(Action, #mi{owner=Owner}=Mi) ->
    clear_menu_selection(Mi),
    wings_wm:send_after_redraw(Owner, {action,Action}),
    delete_all(Mi).

is_magnet_active(Ps, #mi{flags=Flags}) ->
    have_magnet(Ps) andalso have_magnet(Flags).

handle_key(Ev, Mi) ->
    handle_key_1(key(Ev), Mi).

handle_key_1(cancel, _) ->
    wings_wm:send(menu_killer, #keyboard{sym=27});
handle_key_1(delete, Mi0) ->
    %% Delete hotkey bound to this entry.
    case current_command(Mi0) of
      [] -> keep;
      [_|_]=Cmds0 ->
        Cmds = case Cmds0 of
          [{1,{A,{B,false}}}] -> [{A,{B,false}},{A,{B,true}}];
          [{1,{A,{B,{C,false}}}}] -> [{A,{B,{C,false}}},{A,{B,{C,true}}}];
          _Cmd -> [C || {_,C} <- Cmds0]
        end,
        case wings_hotkey:hotkeys_by_commands(Cmds) of
          [] -> keep;			%No hotkeys for this entry.
          Hotkeys -> hotkey_delete_dialog(Hotkeys)
        end
    end;
handle_key_1(insert, Mi) ->
    %% Define new hotkey for this entry.
    case current_command(Mi) of
	[] -> keep;
	[_|_]=Cmds -> get_hotkey(Cmds, Mi)
    end;
handle_key_1(_, _) -> keep.

key(#keyboard{sym=27}) -> cancel;
key(#keyboard{sym=?SDLK_INSERT}) -> insert;
key(#keyboard{unicode=$/}) -> insert;
key(#keyboard{sym=?SDLK_DELETE}) -> delete;
key(#keyboard{unicode=$\\}) -> delete;
key(_) -> none.

current_command(#mi{sel=none}) -> [];
current_command(#mi{sel=Sel,menu=Menu,ns=Names,owner=Owner}=Mi)
  when Owner =:= geom; element(1, Owner) =:= geom ->
    case element(Sel, Menu) of
	{_,Name,_,_,Ps} when is_atom(Name); is_integer(Name) ->
	    Cmd0 = build_command(Name, Names),
	    OptionBox = have_option_box(Ps),
	    Cmd = add_option(OptionBox, Cmd0, false),
	    [{1,simplify_command(Cmd)}];
	{_,Fun,_,_,Ps} when is_function(Fun), is_function(Fun, 2) ->
	    Try = fun(B) ->
			  Cmd0 = Fun(B, Names),
			  case Cmd0 =/= ignore andalso is_ascii_clean(Cmd0) of
			      true ->
				  OptionBox = have_option_box(Ps),
				  Cmd = add_option(OptionBox, Cmd0, false),
				  simplify_command(Cmd);
			      false -> none
			  end
		  end,
	    all_current_commands(Try, Mi);
	{_,{Name,Fun},_,_,Ps} when is_function(Fun), is_function(Fun, 2) ->
	    Try = fun(B) ->
			  Cmd0 = Fun(B, [Name|Names]),
			  case is_tuple(Cmd0) andalso is_ascii_clean(Cmd0) of
			      true ->
				  OptionBox = have_option_box(Ps),
				  Cmd = add_option(OptionBox, Cmd0, false),
				  simplify_command(Cmd);
			      false -> none
			  end
		  end,
	    all_current_commands(Try, Mi);
	_Other ->
	    io:format("~p\n", [_Other]),
	    []
    end;
current_command(_) -> [].

all_current_commands(Fun, #mi{type=plain}) ->
    %% This is a pull-down menu in plain mode, so we must
    %% only return the command for LMB.
    all_current_commands_1([1], Fun);
all_current_commands(Fun, #mi{type=popup}) ->
    all_current_commands_1([1,2,3], Fun).

all_current_commands_1([B|Bs], Fun) ->
    case Fun(B) of
	none -> all_current_commands_1(Bs, Fun);
	Cmd -> [{B,Cmd}|all_current_commands_1(Bs, Fun)]
    end;
all_current_commands_1([], _) -> [].

%% Test if a term can be represented in a text file and read back.
is_ascii_clean([H|T]) ->
    is_ascii_clean(H) andalso is_ascii_clean(T);
is_ascii_clean([]) -> true;
is_ascii_clean(T) when is_tuple(T) ->
    is_tuple_ascii_clean(1, tuple_size(T), T);
is_ascii_clean(Num) when is_number(Num) -> true;
is_ascii_clean(Atom) when is_atom(Atom) -> true;
is_ascii_clean(_) -> false.

is_tuple_ascii_clean(I, N, T) when I =< N ->
    is_ascii_clean(element(I, T)) andalso is_tuple_ascii_clean(I+1, N, T);
is_tuple_ascii_clean(_, _, _) -> true.

add_option(false, Cmd, _) -> Cmd;
add_option(true, Cmd, Val) ->
    add_option_1(Cmd, Val).

add_option_1({Desc,Tuple}, Val) when is_tuple(Tuple) ->
    {Desc,add_option_1(Tuple, Val)};
add_option_1({Desc,Leave}, Val) ->
    {Desc,{Leave,Val}}.

simplify_command({'ASK',{[],[Res],_}}) ->
    Res;
simplify_command({'ASK',{[],Res,_}}) ->
    list_to_tuple(reverse(Res));
simplify_command(Cmd0) when is_tuple(Cmd0) ->
    Cmd = [simplify_command(El) || El <- tuple_to_list(Cmd0)],
    list_to_tuple(Cmd);
simplify_command(Term) -> Term.

set_hotkey(Val, #mi{sel=Sel,menu=Menu0}=Mi) ->
    case element(Sel, Menu0) of
	{A,B,_,D,E} ->
	    %% First nuke any other menu entry that happens to be
	    %% bound to this hotkey.
	    Menu1 = map(fun({_,_,Key,_,_}=T) when Key =:= Val ->
				setelement(3, T, []);
			   (El) ->
				El
			end,
			tuple_to_list(Menu0)),
	    Menu2 = list_to_tuple(Menu1),
	    %% Insert the hotkey.
	    Menu = setelement(Sel, Menu2, {A,B,Val,D,E}),
	    Mi#mi{menu=Menu};
	_Other -> Mi
    end.

popup_submenu(Button, X0, Y0, SubName, SubMenu0,
	      #mi{owner=Owner,level=Level}=Mi) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi) of
	ignore -> keep;
	Action0 when is_tuple(Action0); is_atom(Action0) ->
	    Action = insert_magnet_flags(Action0, Mi),
	    clear_menu_selection(Mi),
	    wings_wm:send(Owner, {action,Action}),
	    delete_all(Mi);
	SubMenu when is_list(SubMenu) ->
	    clear_timer(Mi),
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    menu_setup(popup, X, Y, SubName, SubMenu,
		       Mi#mi{level=Level+1}),
	    delete
    end.

update_flags(Mod, Mi) ->
    case wings_msg:free_rmb_modifier() of
	RmbMod when Mod band RmbMod =/= 0 ->
	    Mi#mi{flags=[magnet]};
	_ ->
	    Mi
    end.

insert_magnet_flags(Action, #mi{flags=[magnet]}) ->
    insert_magnet_flags_0(Action);
insert_magnet_flags(Action,_) -> Action.

insert_magnet_flags_0({'ASK',{PickList,Done,Flags}}=Cmd) ->
    case have_magnet(Flags) of
	false -> Cmd;
	true -> {'ASK',{PickList++[magnet],Done,Flags}}
    end;
insert_magnet_flags_0(Tuple0) when is_tuple(Tuple0) ->
    Tuple = [insert_magnet_flags_0(El) || El <- tuple_to_list(Tuple0)],
    list_to_tuple(Tuple);
insert_magnet_flags_0(Term) -> Term.

%% Handle sub-menus of plain (pull-down) menus
submenu(I, Name, Menu0, #mi{w=W,hs=Hs,level=Level}=Mi0) ->
    Menu = expand_submenu(1, Name, Menu0, Mi0),
    X0 = W-?CHAR_WIDTH,
    Y0 = get_item_pos(I, Hs, -?LINE_HEIGHT),
    Mi = Mi0#mi{level=Level+1},
    {X,Y} = wings_wm:local2global(X0, Y0),
    menu_setup(plain, X, Y, Name, Menu, Mi).

get_item_pos(0, _Hs, Pos) -> Pos;
get_item_pos(I, [H|Hs], Pos) -> get_item_pos(I-1, Hs, Pos+H).

expand_submenu(B, Name, Submenu, #mi{ns=Ns}) when is_function(Submenu) ->
    Submenu(B, [Name|Ns]);
expand_submenu(_Button, _Name, Submenu, _Mi) -> Submenu.

clear_timer(#mi{timer=Timer}) -> wings_wm:cancel_timer(Timer).

set_submenu_timer(#mi{sel=Sel}=Mi, #mi{sel=Sel}, _X, _Y) -> Mi;
set_submenu_timer(#mi{sel=Sel}=Mi, OldMi, X0, Y0) ->
    clear_timer(OldMi),
    clear_timer(Mi),
    case is_submenu(Sel, Mi) of
	false ->
	    Timer = wings_wm:set_timer(?SUB_MENU_TIME, clear_submenu),
	    Mi#mi{timer=Timer};
	true ->
	    {X,Y} = wings_wm:local2global(X0, Y0),
	    Event = #mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
	    Timer = wings_wm:set_timer(?SUB_MENU_TIME, Event),
	    Mi#mi{timer=Timer}
    end.

delete_all(Mi) ->
    clear_timer(Mi),
    kill_menus(),
    delete.

kill_menus() ->
    case wings_wm:is_window(menu_killer) of
	true -> wings_wm:send(menu_killer, kill_menus);
	false -> ok
    end.

redraw(Mi) ->
    wings_io:ortho_setup(),
    menu_show(Mi).

update_highlight(X, Y, #mi{menu=Menu,sel=OldSel,sel_side=OldSide,w=W}=Mi0) ->
    case selected_item(Y, Mi0) of
	OldSel when is_integer(OldSel) ->
	    Ps = element(5, element(OldSel, Menu)),
	    RightWidth = right_width(Ps),
	    Right = W - (2*RightWidth) - ?CHAR_WIDTH,
	    Side = if
		       X < Right; RightWidth == 0 -> left;
		       true -> right
		   end,
	    if
		Side =:= OldSide -> Mi0;
		true ->
		    wings_wm:dirty(),
		    help_text(Mi0),
		    Mi0#mi{sel_side=Side}
	    end;
	OldSel -> Mi0;
	NoSel when NoSel == outside; NoSel == none ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=none},
	    help_text(Mi),
	    Mi;
	Item when is_integer(Item), OldSel == none ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=Item},
	    help_text(Mi),
	    Mi;
	Item when is_integer(Item) ->
	    wings_wm:dirty(),
	    Mi = Mi0#mi{sel=Item},
	    help_text(Mi),
	    Mi
    end.

was_option_hit(Button, Act, X, Ps, Mi) ->
    case have_option_box(Ps) of
	false -> Act;
	true -> {Act,hit_right(Button, X, Mi)}
    end.

hit_right(B, _, #mi{type=popup}) when B > 1 -> true;
hit_right(_, X, #mi{w=W}) -> X >= W-3*?CHAR_WIDTH.

selected_item(Y, #mi{type=Type,ymarg=Margin,h=H,menu=Menu}=Mi) ->
    %% The tests are simplified because we know that the mouse cursor
    %% must be over the menu window.
    if
	Margin =< Y, Y < H+Margin ->
	    %% Clearly inside the menu area.
	    selected_item_1(Y-Margin, Mi);
	Y < Margin ->
	    %% Above upper margin. If advanced menus, it is safe
	    %% to count that as if the mouse is over the first row.
	    %% (The menu doesn't popup until the RMB has been released,
	    %% unlike the plain (pull-down) menus.)
	    case Type of
		popup -> selected_item_1(0, Mi);
		plain -> none
	    end;
	true ->
	    %% Below the lower margin. Pretend that the cursor is over the
	    %% last row (we assume that it cannot be inactive text
	    %% or a separator).
	    tuple_size(Menu)
    end.

selected_item_1(Y, #mi{hs=Hs}=Mi) ->
    selected_item_1(Y, 1, Hs, Mi).

selected_item_1(Y0, I, [H|Hs], #mi{sel=OldSel,menu=Menu}=Mi) ->
    case Y0 - H of
	Y when Y =< 0 ->
	    case element(I, Menu) of
		separator ->
		    if
			OldSel =:= I-1; OldSel =:= I+1 -> OldSel;
			true -> none
		    end;
		{_Text,ignore,_,_,_} ->
		    if
			I-1 =< OldSel, OldSel =< I+2-> OldSel;
			true -> none
		    end;
		_Other -> I
	    end;
	Y -> selected_item_1(Y, I+1, Hs, Mi)
    end.

is_submenu(_I, #mi{type=popup}) -> false;
is_submenu(I, #mi{type=plain,menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{_Text,{'VALUE',_},_Hotkey,_Help,_Ps} -> false;
	{_Text,{_,_},_Hotkey,_Help,_Ps} -> true;
	_Other -> false
    end;
is_submenu(_, _) -> false.

build_command(Name, Names) ->
    foldl(fun(N, A) -> {N,A} end, Name, Names).

menu_draw(_X, _Y, _Shortcut, _Mw, _I, [], _Mi) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, [H|Hs], #mi{menu=Menu,type=Type}=Mi) ->
    ?CHECK_ERROR(),
    Elem = element(I, Menu),
    Text = menu_text(Elem, Type),
    case Elem of
	separator -> draw_separator(X, Y, Mw);
	{_,ignore,_,_,Ps} ->
	    menu_draw_1(Y, Ps, I, Mi,
			fun() -> wings_io:unclipped_text(X, Y, Text) end);
	{_,{'VALUE',_},Hotkey,_Help,Ps} ->
	    %% Not a sub-menu.
	    menu_draw_1(Y, Ps, I, Mi,
			fun() ->
				draw_menu_text(X, Y, Text, Ps),
				draw_hotkey(X, Y, Shortcut, Hotkey)
			end,
			fun() ->
				draw_right(X, Y, Mw, Ps)
			end);
	{_,{_,_}=Sub,Hotkey,_Help,Ps} ->
	    %% Submenu.
	    menu_draw_1(Y, Ps, I, Mi,
			fun() ->
				wings_io:unclipped_text(X, Y, Text),
				draw_hotkey(X, Y, Shortcut, Hotkey)
			end),
	    
	    draw_submenu_marker(Type, Sub,
				X+Mw-5*?CHAR_WIDTH, Y-?CHAR_HEIGHT div 3);
	{_,_,Hotkey,_Help,Ps} ->
	    menu_draw_1(Y, Ps, I, Mi,
			fun() ->
				draw_menu_text(X, Y, Text, Ps),
				draw_hotkey(X, Y, Shortcut, Hotkey)
			end,
			fun() ->
				draw_right(X, Y, Mw, Ps)
			end)
    end,
    ?CHECK_ERROR(),
    menu_draw(X, Y+H, Shortcut, Mw, I+1, Hs, Mi).

menu_draw_1(Y, Ps, Sel, Mi, DrawLeft) ->
    menu_draw_1(Y, Ps, Sel, Mi, DrawLeft, ignore).

menu_draw_1(Y, Ps, Sel, #mi{sel=Sel,sel_side=Side,w=W},
	    DrawLeft, DrawRight) ->
    %% Draw blue background for highlighted item.
    wings_io:set_color(wings_pref:get_value(menu_hilite)),
    Color = wings_pref:get_value(menu_hilite),
    Cw = wings_text:width(),
    Right = W - (2*right_width(Ps)) - Cw,
    case Side of
	right ->
	    {X1,Y1,X2,Y2} = {Right, Y-?CHAR_HEIGHT, Right+3*Cw-2, Y+3},
	    wings_io:gradient_rect(X1, Y1, X2-X1, Y2-Y1, Color),
	    wings_io:set_color(wings_pref:get_value(menu_text));
	left ->
	    {X1,Y1,X2,Y2} = {?CHAR_WIDTH, Y-?CHAR_HEIGHT, Right, Y+3},
	    wings_io:gradient_rect(X1, Y1, X2-X1, Y2-Y1, Color),
	    wings_io:set_color(wings_pref:get_value(menu_hilited_text))
    end,
    DrawLeft(),
    case {DrawRight,Side} of
	{ignore,_} -> ok;
	{_,left} ->
	    wings_io:set_color(wings_pref:get_value(menu_text)),
	    DrawRight();
	{_,right} ->
	    wings_io:set_color(wings_pref:get_value(menu_hilited_text)),
	    DrawRight()
    end;
menu_draw_1(_, _, _, _, DrawLeft, DrawRight) ->
    wings_io:set_color(wings_pref:get_value(menu_text)),
    DrawLeft(),
    case DrawRight of
	ignore -> ok;
	_ -> DrawRight()
    end.

menu_text({Text,{_,Fun},_,_,_}, popup) when is_function(Fun) -> [$.,Text,$.];
menu_text({Text,Fun,_,_,_}, popup) when is_function(Fun) -> [$.,Text,$.];
menu_text({Text,_,_,_,_}, _) -> Text;
menu_text(separator, _) -> [].

draw_hotkey(_, _, _, []) -> ok;
draw_hotkey(X, Y, Pos, Hotkey) -> wings_io:text_at(X+Pos, Y, Hotkey).

draw_menu_text(X, Y, Text, Props) ->
    case proplists:is_defined(crossmark, Props) of
	true ->
	    wings_io:unclipped_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text]);
	false ->
	    case proplists:is_defined(grey_crossmark, Props) of
		false -> ok;
		true ->
		    gl:pushAttrib(?GL_CURRENT_BIT),
		    gl:color3f(0.25, 0.25, 0.25),
		    wings_io:unclipped_text(X-2*?CHAR_WIDTH, Y, [crossmark]),
		    gl:popAttrib()
	    end,
	    wings_io:unclipped_text(X, Y, Text)
    end.

help_text(#mi{sel=none}) ->
    wings_wm:message("");
help_text(#mi{menu=Menu,sel=Sel}=Mi) ->
    Elem = element(Sel, Menu),
    help_text_1(Elem, Mi).

help_text_1({Text,{Sub,_},_,_,_}, #mi{type=plain}) when Sub =/= 'VALUE' ->
    %% No specific help text for submenus in plain mode.
    Help = [Text|?__(1," submenu")],
    wings_wm:message(Help, "");
help_text_1({_,{Name,Fun},_,_,Ps}, #mi{ns=Ns}=Mi) when is_function(Fun) ->
    %% "Submenu" in advanced mode.
    Help0 = Fun(help, [Name|Ns]),
    Help = help_text_2(Help0),
    magnet_help(Help, Ps, Mi);
help_text_1({_,_,_,Help0,Ps}, Mi) ->
    %% Plain entry - not submenu.
    Help = help_text_2(Help0),
    magnet_help(Help, Ps, Mi);
help_text_1(separator, _) -> ok.

help_text_2({S1,S2,S3}) -> wings_msg:button_format(S1, S2, S3);
help_text_2(Help) -> Help.

magnet_help(Msg0, Ps, #mi{flags=Flags}) ->
    case have_magnet(Ps) of
	false ->
	    wings_wm:message(Msg0);
	true ->
	    case have_magnet(Flags) of
		false ->
		    ModRmb = wings_msg:free_rmb_modifier(),
		    ModName = wings_msg:mod_name(ModRmb),
		    MagMsg = [ModName,$+,
			      ?__(2,"Click for Magnet")],
		    Msg = wings_msg:join(Msg0, MagMsg),
		    wings_wm:message(Msg);
		true ->
		    Msg = wings_msg:join(Msg0,
					 wings_magnet:info_string()),
		    wings_wm:message(Msg, "")
	    end
    end.

draw_right(X0, Y0, Mw, Ps) ->
    case have_option_box(Ps) of
	true ->
	    X = X0 + Mw - 5*?CHAR_WIDTH,
	    Y = Y0 - ?CHAR_HEIGHT div 3,
	    wings_io:text_at(X, Y, [option_box]);
	false -> draw_right_1(X0, Y0, Mw, Ps)
    end.

draw_right_1(X0, Y0, Mw, Ps) ->
    case proplists:get_value(color, Ps, none) of
	none -> ok;
	Color ->
	    Cw = wings_text:width(),
	    Ch = wings_text:height(),
	    X = X0 + Mw - 5*Cw,
	    Y = Y0 - Ch + 1,
	    wings_io:border(X, Y, Cw, Ch-1, Color)
    end.

draw_submenu_marker(popup, _Item, _X, _Y) -> ok;
draw_submenu_marker(plain, _Item, X, Y) ->
    Cw = wings_text:width(),
    H = (wings_text:height()+2) div 3,
    ?CHECK_ERROR(),
    gl:'begin'(?GL_TRIANGLES),
    gl:vertex2i(X-Cw div 2, Y),
    gl:vertex2i(X-Cw, Y-H),
    gl:vertex2i(X-Cw, Y+H),
    gl:'end'(),
    ?CHECK_ERROR().

draw_separator(X, Y, Mw) ->
    ?CHECK_ERROR(),
    Cw = wings_text:width(),
    LeftX = X-2*Cw+0.5,
    RightX = X+Mw-4*Cw+0.5,
    UpperY = Y-?SEPARATOR_HEIGHT+0.5,
    gl:lineWidth(1),
    gl:color3f(0.10, 0.10, 0.10),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(LeftX, UpperY),
    gl:vertex2f(RightX, UpperY),
    gl:'end'(),
    gl:color3b(0, 0, 0).

move_if_outside(X0, Y, Mw, Mh, Mi) ->
    {W,H} = wings_wm:top_size(),
    if
	X0+Mw > W ->
	    X = left_of_parent(Mw, W, Mi),
	    move_if_outside_1(X, Y, Mh, H);
	true ->
	    move_if_outside_1(X0, Y, Mh, H)
    end.

move_if_outside_1(X, Y, Mh, H) when Y+Mh > H ->
    move_if_outside_2(X, H-Mh);
move_if_outside_1(X, Y, _, _) ->
    move_if_outside_2(X, Y).

move_if_outside_2(X, Y) when X < 0 ->
    move_if_outside_3(0, Y);
move_if_outside_2(X, Y) ->
    move_if_outside_3(X, Y).

move_if_outside_3(X, Y) when Y < 1 -> {X,1};
move_if_outside_3(X, Y) -> {X,Y}.

left_of_parent(Mw, W, #mi{level=?INITIAL_LEVEL}) -> W-Mw;
left_of_parent(Mw, _, #mi{level=Level}) ->
    {X,_,_,_} = wings_wm:viewport({menu,Level-1}),
    X-Mw+10.

have_option_box(Ps) ->
    proplists:is_defined(option, Ps).

have_magnet(Ps) ->
    proplists:is_defined(magnet, Ps).

%%%
%%% Get a key to bind a command to.
%%%

get_hotkey([{1,Cmd}], Mi) ->
    wings_wm:dirty(),
    case Cmd of
      {A,{B,false}} -> get_hotkey([{1,Cmd},{3,{A,{B,true}}}],Mi);
      {A,{B,{C,false}}} -> get_hotkey([{1,Cmd},{3,{A,{B,{C,true}}}}],Mi);
      Cmd ->
        wings_wm:message(hotkey_key_message(Cmd)),
        {push,fun(Ev) ->
          handle_key_event(Ev, Cmd, Mi)
          end}
    end;
get_hotkey([_|_]=Cmds, Mi) ->
    wings_wm:dirty(),
    wings_wm:message(hotkey_mouse_message(Cmds)),
    {push,fun(Ev) ->
		  handle_button_event(Ev, Cmds, Mi)
	  end}.

handle_button_event(redraw, _Cmds, Mi) ->
    redraw(Mi),
    keep;
handle_button_event(#mousebutton{button=B,state=?SDL_RELEASED}, Cmds, Mi) ->
    case keyfind(B, 1, Cmds) of
	{B,Cmd} ->
	    wings_wm:message(hotkey_key_message(Cmd)),
	    {replace,fun(Ev) ->
			     handle_key_event(Ev, Cmd, Mi)
		     end};
	false ->
	    keep
    end;
handle_button_event(_, _, _) ->keep.

handle_key_event(redraw, _Cmd, Mi) ->
    redraw(Mi),
    keep;
handle_key_event(#keyboard{sym=Sym}, _, _) when Sym >= ?SDLK_NUMLOCK ->
    keep;
handle_key_event(#keyboard{}=Ev, Cmd, Mi) ->
    Win = wings_wm:this(),
    case wings_hotkey:event(Ev, Cmd) of
	next -> do_bind(Win, Ev, Cmd, Mi);
	OtherCmd ->
	    C = wings_util:stringify(OtherCmd),
	    Q = ?__(1,"This key is already bound to the ") ++ C ++
		?__(2," command. Do you want to re-define it?"),
	    wings_u:yes_no(Q, fun() -> do_bind(Win, Ev, Cmd, Mi) end)
    end,
    pop;
handle_key_event(_, _, _) -> keep.

do_bind(Win, Ev, Cmd, Mi0) ->
    Keyname = wings_hotkey:bind_from_event(Ev, Cmd),
    Mi = set_hotkey(Keyname, Mi0),
    wings_wm:send(Win, Mi),
    ignore.

hotkey_mouse_message(Cmds) ->
    {Lmb,Mmb,Rmb} = hotkey_mouse_message_1(Cmds, [], [], []),
    [?__(1,"Click a mouse button to choose command to bind"),
     ":   ",wings_msg:button_format(Lmb, Mmb, Rmb)].

hotkey_mouse_message_1([{1,Cmd}|T], _L, M, R) ->
    hotkey_mouse_message_1(T, wings_util:stringify(Cmd), M, R);
hotkey_mouse_message_1([{2,Cmd}|T], L, _M, R) ->
    hotkey_mouse_message_1(T, L, wings_util:stringify(Cmd), R);
hotkey_mouse_message_1([{3,Cmd}|T], L, M, _R) ->
    hotkey_mouse_message_1(T, L, M, wings_util:stringify(Cmd));
hotkey_mouse_message_1([], L, M, R) -> {L,M,R}.

hotkey_key_message(Cmd) ->
    [?__(1,"Press the key to bind the \""),
     wings_util:stringify(Cmd),
     ?__(2,"\" command to.")].

%%%
%%% Hotkey deletion dialog.
%%%

hotkey_delete_dialog(Hotkeys) ->
    Fun = fun(Res) ->
		  [wings_hotkey:unbind(K) || {K,true} <- Res],
		  ignore
	  end,
    Dialog = mk_dialog(Hotkeys),
    wings_ask:dialog(?__(1,"Delete Hotkeys"), Dialog, Fun).

mk_dialog([{Key,Keyname,Cmd,Src}|T]) ->
    [mk_key_item(Key, Keyname, Cmd, Src)|mk_dialog(T)];
mk_dialog([]) ->
    [separator,{label,?__(1,"Check all hotkeys to be deleted.")}].

mk_key_item(Key, Keyname, Cmd, _Src) ->
    {Keyname ++ ": " ++ Cmd,false,[{key,Key}]}.
