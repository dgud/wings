%%
%%  wings_menu.erl --
%%
%%     Implementation of pulldown and popup menus.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_menu).
-export([is_popup_event/1,menu/5,popup_menu/4,build_command/2,
	 kill_menus/0]).
-export([wx_menubar/1, wx_command_event/1, check_item/1, str_clean/1]).
-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,keyfind/3,foreach/2]).
-compile(export_all).

-define(SUB_MENU_TIME, 150).
-define(SEPARATOR_HEIGHT, (?CHAR_HEIGHT-4)).
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
	 menu :: menu_item(),		        %Normalized menu.
	 timer=make_ref(),			%Active submenu timer.
	 level=?INITIAL_LEVEL,			%Menu level.
	 type :: 'plain'|'popup',	        %Type of menu.
	 owner,					%Owning window.
	 flags=[] :: list(),			%Flags (magnet/dialog).
	 orig_xy				%Originally input global X and Y
	}).

-record(menu_entry, {wxid, name, object, type}).

-record(menu_pop,
	{wxid,
	 name,
	 type,
	 opts,
	 msg=""
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
    %%menu_setup(popup, X, Y, Name, Menu, #mi{owner=wings_wm:this()}).
    wx_popup_menu_init(X,Y,[Name],Menu).

menu_setup(Type, X0, Y0, Name, Menu0, #mi{ns=Names0,level=Level0}=Mi0) ->
    Menu = case Name of
        more ->
            Names = Names0,
            AddToolbar = false,
            Hotkeys = wings_hotkey:matching(Names),
            normalize_menu(Menu0, Hotkeys, Type =:= popup);
        _ ->
            Names = [Name|Names0],
            Toolbar = wings_pref:get_value(menu_toolbar),
            Menu1 = wings_plugin:menu(list_to_tuple(reverse(Names)), Menu0),
            AddToolbar = menu_toolbar_allowed(Toolbar, Type, Names),
            case Menu1 of
                [] -> empty;
                _ -> 
                    Menu2 = if AddToolbar -> [menu_toolbar|Menu1]; true -> Menu1 end,
                    Hotkeys = wings_hotkey:matching(Names),
                    normalize_menu(Menu2, Hotkeys, Type =:= popup)
            end
    end,
    Level = case Menu of
    empty -> Level0;
    _ ->
        {MwL,MwM,MwR,Hs} = menu_dims(Menu),
        ToolbarSize = wings_pref:get_value(menu_toolbar_size),
        Cw = ?CHAR_WIDTH,
        TotalW = total_width(AddToolbar, MwL + MwM + MwR + 8*Cw, ToolbarSize),
        Mh = lists:sum(Hs),
        Margin = 3,
        InfoLine = ?CHAR_HEIGHT + 14,
        {X1,Y1} = case Type of
              plain ->
                  {X0,Y0};
              popup when ToolbarSize =:= big ->
                  % snap mouse to space between Repeat and Vertex icon
                  {(X0-TotalW div 2)+22*3, Y0 - Margin - ?CHAR_HEIGHT};
              popup when ToolbarSize =:= small ->
                  % snap mouse to space between Repeat and Vertex icon
                  {(X0-TotalW div 2)+15*3, Y0 - Margin - ?CHAR_HEIGHT}
              end,
        {X,Y} = move_if_outside(X1, Y1, TotalW, Mh+2*Margin+InfoLine, Mi0),
        move_cursor_to_toolbar(AddToolbar, Type, Y),
        W = TotalW-10,
        Mi = Mi0#mi{ymarg=Margin,shortcut=MwL+Cw,w=W,h=Mh,hs=Hs,
            sel=none,ns=Names,menu=Menu,type=Type,orig_xy={X0,Y0}},
        #mi{level=Level1} = Mi,
        setup_menu_killer(Mi),
        Op = {seq,push,get_menu_event(Mi)},
        WinName = {menu,Level1},
        wings_wm:delete({menu,Level1}),
        wings_wm:new(WinName, {X,Y,highest}, {W,Mh+10}, Op),
        Level1+1
    end,
    delete_from(Level).

total_width(true, W, Size) ->
    case Size of
        big -> max(260, W);
        small -> max(180, W)
    end;
total_width(_, W, _) -> W.

menu_toolbar_allowed(true, popup, Ns) ->
    Name = lists:last(Ns),
    menu_toolbar_allowed(Name);
menu_toolbar_allowed(_, _, _) -> false.

menu_toolbar_allowed(Name) ->
    case Name of
      shape -> true;
      vertex -> true;
      edge -> true;
      face -> true;
      body -> true;
      select -> true;
      tools -> true;
      tweak -> true;
      _ -> false
    end.

move_cursor_to_toolbar(true, popup, Y) ->
    Windows = wings_wm:windows(),
    Pref = wings_pref:get_value(menu_toolbar_snap_cursor),
    case lists:keymember(menu, 1, Windows) of
        false when not Pref -> ok;
        _ ->
            {_,X0,Y0} = wings_wm:local_mouse_state(),
            {X,_} = wings_wm:local2global(X0, Y0),
            wings_io:warp(X, Y + 1 + ?LINE_HEIGHT div 2)
    end;
move_cursor_to_toolbar(_,_,_) -> ok.

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

menu_killer(#mousebutton{state=?SDL_PRESSED}=Ev, Owner) ->
    wings_wm:notify(menu_aborted),
    wings_wm:send_after_redraw(geom, {adv_menu_abort,Ev}),
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
    MenuColor = wings_pref:get_value(menu_color),
    wings_io:blend(MenuColor,
		   fun(Color) ->
			   wings_io:border(0, 0, Mw-1, Mh + 2*Margin+3, Color)
		   end),
    menu_draw(3*?CHAR_WIDTH, Margin+?CHAR_HEIGHT,
	      Shortcut, Mw, 1, Mi#mi.hs, Mi).

normalize_menu(Menu, Hotkeys, Adv) ->
    Pref = wings_pref:get_value(max_menu_height),
    MaxHeight =
      if Pref < 1 -> % auto menu clipping
             {_,Y} = wings_wm:win_size(geom),
             Y - 48;
         true ->
             Pref
      end,
    CurrentHeight = 0,
    normalize_menu(Menu, Hotkeys, Adv, MaxHeight, CurrentHeight, []).

normalize_menu([[_|_]=List|Els], Hotkeys, Adv, MaxH, CurH, Acc) ->
    normalize_menu(List++Els, Hotkeys, Adv, MaxH, CurH, Acc);
normalize_menu([Elem0|Els0], Hotkeys, Adv, MaxH, CurH, Acc) when MaxH > CurH ->
    Elem1 = case Elem0 of
        {S,Name,Help,Ps} ->
            H = ?LINE_HEIGHT,
            {S,Name,[],Help,Props=Ps};
        {S,Name,[C|_]=Help} when is_integer(C) ->
            H = ?LINE_HEIGHT,
            {S,Name,[],Help,Props=[]};
        {S,Name,Ps} ->
            H = ?LINE_HEIGHT,
            {S,Name,[],[],Props=Ps};
        menu_toolbar ->
            Name = none,
            Props = [],
            H = case wings_pref:get_value(menu_toolbar_size) of
                big -> 28;
                small -> 20
            end,

            menu_toolbar;
        {S,Name} ->
            H = ?LINE_HEIGHT,
            {S,Name,[],[],Props=[]};
        separator ->
            H = ?SEPARATOR_HEIGHT,
            Name = none,
            Props = [],
            separator
    end,
    Elem2 = norm_add_hotkey(Name, Elem1, Hotkeys, Props),
    Elem = norm_help(Elem2, Adv),
    normalize_menu(Els0, Hotkeys, Adv, MaxH, CurH+H, [Elem|Acc]);
normalize_menu([Elem0|Els0]=Els1, _, _, _, _, Acc) ->
    Els = case Elem0 of
        separator -> Els0;
        _ -> Els1
    end,
    E = {?__(1,"More..."),Els,[],[],[more]},
    [LastElem|_] = Acc,
    case LastElem of
        separator -> list_to_tuple(reverse([E|Acc]));
        _ -> list_to_tuple(reverse([E,separator|Acc]))
    end;
normalize_menu([], _Hotkeys, _Adv, _, _, Acc) -> list_to_tuple(reverse(Acc)).

norm_add_hotkey(_, separator, _, _) -> separator;
norm_add_hotkey(_, menu_toolbar, _, _) -> menu_toolbar;
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

match_hotkey(Name, [{{_,Name},Key}|_], false) -> Key;
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
norm_help(menu_toolbar=Item, _) -> Item;
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
 	    {S,ignore,[],[],[]} when I =:= 1 ->
		case wings_text:width([$\s|S]) - (MaxA0+MaxB0+MaxC0) of
		    W when W < 0 ->
			{0,0,0,?LINE_HEIGHT};
		    W ->
			{MaxA0+W,0,0,?LINE_HEIGHT}
		end;
	    {S,{_,_},Hotkey,_,_} ->
		{wings_text:width([$.,$.|S]),wings_text:width(Hotkey),
		 ?CHAR_WIDTH,?LINE_HEIGHT};
	    {S,_,Hotkey,_,Ps} ->
		{wings_text:width([$.,$.|S]),wings_text:width(Hotkey),
		 right_width(Ps),?LINE_HEIGHT};
	    separator -> {0,0,0,?SEPARATOR_HEIGHT};
	    menu_toolbar ->
		case wings_pref:get_value(menu_toolbar_size) of
		    big -> {0,0,0,28};
		    small -> {0,0,0,20}
		end
	end,
    menu_dims(Menu, I-1, max(Wa, MaxA0), max(Wb, MaxB0),
	      max(Wc, MaxC0), [H|Hacc]).

right_width(Ps) ->
    Cw = ?CHAR_WIDTH,
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
    {replace,fun(Ev) ->
        handle_menu_event(Ev, Mi) end}.

handle_menu_event(redraw, Mi) ->
    redraw(Mi),
    keep;
handle_menu_event(#mousemotion{x=X,y=Y}, Mi) ->
    mousemotion(X, Y, Mi);
handle_menu_event(#mousebutton{}=Ev, Mi) ->
    button_pressed(Ev, Mi);
handle_menu_event(#keyboard{}=Ev, Mi) ->
    handle_key(Ev, Mi);
handle_menu_event(lost_focus, Mi) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    {_,H} = wings_wm:win_size(),
    if
	Y < 0 -> mousemotion(X, 0, Mi);
	Y >= H -> mousemotion(X, H-2, Mi);
	true -> keep
    end;
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
button_pressed(#mousebutton{button=Button,x=X,y=Y,state=?SDL_PRESSED},
  #mi{menu=Menu,sel_side=Side,w=Mw}=Mi)
  when Button =:= 4; Button =:= 5 ->
    case selected_item(Y, Mi) of
        1 ->
          case element(1,Menu) of
              menu_toolbar when Side =:= right; Side =:= left ->
                  menu_toolbar_action(Button, button_check(X, Mw), Mi);
              menu_toolbar ->
                  menu_toolbar_action(Button, Side, Mi);
              _ -> keep
          end;
        _ -> keep
    end;
button_pressed(_, _) -> keep.

button_pressed(Button, Mod, X, Y,
  #mi{ns=Names,menu=Menu,type=Type,sel_side=Side,w=Mw,level=Level,owner=Owner}=Mi0) ->
    clear_timer(Mi0),
    Mi = update_flags(Mod, Mi0),
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
		    do_action(Act, Names, Ps, Mi);
		menu_toolbar ->
		    case Side of
		      right ->
		        menu_toolbar_action(Button, button_check(X, Mw), Mi);
		      left ->
		        menu_toolbar_action(Button, button_check(X, Mw), Mi);
		      history when Button =:= 2 ->
		        keep;
		      tools when Button =/= 1 ->
		        keep;
		      _ ->
		        menu_toolbar_action(Button, Side, Mi)
		    end;
		{_,More,[],[],[more]} ->
		    clear_timer(Mi),
		    X0 = Mw-?CHAR_WIDTH,
		    {X1,Y1} = wings_wm:local2global(X0, Y),
		    menu_setup(Type, X1, Y1, more, More,
		      #mi{ns=Names,level=Level+1,owner=Owner})
	    end
    end.

menu_toolbar_action(Button, Side, #mi{ns=Names,owner=Owner,orig_xy=OrigXY}) ->
    case lists:last(Names) of
      N when N =:= select; N =:= tools; N =:= tweak ->
        wings_wm:send_after_redraw(Owner, {menu_toolbar,{new_mode,Button,OrigXY,Side}}),
        keep;
      _ ->
        wings_wm:send_after_redraw(Owner, {menu_toolbar,{Button,OrigXY,Side}}),
        keep
    end.

call_action(Act, Button, Ns, Ps, Mi = #mi{flags=Flags}) ->
    case Act(Button, Ns) of
	ignore -> keep;
	Cmd0 when is_tuple(Cmd0) ->
	    Cmd = case is_magnet_active(Ps, Flags) of
		      false -> Cmd0;
		      true -> insert_magnet_flags(Cmd0, have_magnet(Flags))
		  end,
	    send_action(Cmd, Mi)
    end.

do_action(Act0, Ns, Ps, #mi{flags=Flags} = Mi) ->
    Act = case is_magnet_active(Ps, Flags) of
	      false -> build_command(Act0, Ns);
	      true -> build_command({'ASK',{[magnet],[Act0]}}, Ns)
	  end,
    send_action(Act, Mi).

send_action(Action, #mi{type=popup,ns=Names,owner=Owner,orig_xy=OrigXY}=Mi) ->
    Name = lists:last(Names),
    io:format("Send action: ~p~n",[Action]),
    case wings_pref:get_value(menu_toolbar) of
      true when Name =:= select ->
        wings_wm:send_after_redraw(Owner, {menu_toolbar, OrigXY}),
        wings_wm:send(Owner, {action,Action}),
        keep;
      _ ->
        clear_menu_selection(Mi),
        wings_wm:send_after_redraw(Owner, {action,Action}),
        delete_all(Mi)
    end;
send_action(Action, #mi{owner=Owner}=Mi) ->
    clear_menu_selection(Mi),
    wings_wm:send_after_redraw(Owner, {action,Action}),
    delete_all(Mi).

is_magnet_active(Ps, Flags) when is_list(Flags) ->
    have_magnet(Ps) andalso have_magnet(Flags);
is_magnet_active(Ps, Bool) when is_boolean(Bool) ->
    Bool andalso have_magnet(Ps).

handle_key(Ev, #mi{owner=Owner,orig_xy=OrigXY}=Mi) ->
    case handle_key_1(key(Ev), Mi) of
        none ->
            wings_wm:send_after_redraw(Owner, {hotkey_in_menu,Ev,OrigXY}),
            keep;
        Other -> Other
    end.

handle_key_1(cancel, _) ->
    wings_wm:send(menu_killer, #keyboard{sym=27});
handle_key_1(delete, Mi0) ->
    %% Delete hotkey bound to this entry.
    case current_command(Mi0) of
      [] -> none;
      [_|_]=Cmds0 ->
        Cmds = case Cmds0 of
          [{1,{A,{B,false}}}] -> [{A,{B,false}},{A,{B,true}}];
          [{1,{A,{B,{C,false}}}}] -> [{A,{B,{C,false}}},{A,{B,{C,true}}}];
          _Cmd -> [C || {_,C} <- Cmds0]
        end,
        case wings_hotkey:hotkeys_by_commands(Cmds) of
          [] -> none;			%No hotkeys for this entry.
          Hotkeys -> hotkey_delete_dialog(Hotkeys)
        end
    end;
handle_key_1(insert, Mi) ->
    %% Define new hotkey for this entry.
    case current_command(Mi) of
	[] -> none;
	[_|_]=Cmds -> get_hotkey(Cmds, Mi)
    end;
handle_key_1(_, _) -> none.

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
is_ascii_clean(Fun) when is_function(Fun) -> true;
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
	      #mi{owner=Owner,level=Level, flags=Flags}=Mi) ->
    %% Only in advanced menu mode.
    case expand_submenu(Button, SubName, SubMenu0, Mi) of
	ignore -> keep;
	Action0 when is_tuple(Action0); is_atom(Action0) ->
	    Action = insert_magnet_flags(Action0, have_magnet(Flags)),
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

insert_magnet_flags(Action, true) ->
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

update_highlight(X, Y, #mi{ns=Ns,menu=Menu,sel=OldSel,sel_side=OldSide,w=W}=Mi0) ->
    case selected_item(Y, Mi0) of
	OldSel when is_integer(OldSel) ->
	    case element(OldSel, Menu) of
	      menu_toolbar ->
	        case button_check(X, W) of
	            OldSide -> Mi0;
	            Icon ->
	                menu_toolbar_help(lists:last(Ns), Icon),
	                wings_wm:dirty(),
	                Mi0#mi{sel_side=Icon}
	        end;
	      MenuItemData ->
	        Ps = element(5, MenuItemData),
	        RightWidth = right_width(Ps),
	        Right = W - (2*RightWidth) - ?CHAR_WIDTH,
	        Side = if
	                 X < Right; RightWidth == 0 -> left;
	                 true -> right
	               end,
	        if
	          Side =:= OldSide -> Mi0;
	          true ->
	              help_text(Mi0),
	              wings_wm:dirty(),
	              Mi0#mi{sel_side=Side}
	        end
	    end;
	Item when is_integer(Item) ->
	    case element(Item, Menu) of
	      menu_toolbar ->
	        Icon = button_check(X, W),
	        menu_toolbar_help(lists:last(Ns), Icon),
	        wings_wm:dirty(),
	        Mi0#mi{sel=Item,sel_side=Icon};
	      _other ->
	        Mi = Mi0#mi{sel=Item,sel_side=left},
	        help_text(Mi),
	        wings_wm:dirty(),
	        Mi
	    end;
	_ ->
	    Mi = Mi0#mi{sel=none},
	    help_text(Mi),
	    wings_wm:dirty(),
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
		menu_toolbar -> I;
		_Other -> I
	    end;
	Y -> selected_item_1(Y, I+1, Hs, Mi)
    end.

is_submenu(I, #mi{type=popup,menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	{_,_More,[],[],[more]} -> true;
	_Other -> false
    end;
is_submenu(I, #mi{type=plain,menu=Menu}) when is_integer(I) ->
    case element(I, Menu) of
	separator -> false;
	{_Text,{'VALUE',_},_Hotkey,_Help,_Ps} -> false;
	{_Text,{_,_},_Hotkey,_Help,_Ps} -> true;
	{_,_More,[],[],[more]} -> true;
	_Other -> false
    end;
is_submenu(_, _) -> false.

build_command(Name, Names) ->
    foldl(fun(N, Use={N, _}) -> Use;
	     (N, A) -> {N,A} end,
	  Name, Names).
build_command(Name, Names, true) ->
    build_command({'ASK',{[magnet],[Name]}}, Names);
build_command(Name, Names, false) ->
    build_command(Name, Names).


menu_draw(_X, _Y, _Shortcut, _Mw, _I, [], _Mi) -> ok;
menu_draw(X, Y, Shortcut, Mw, I, [H|Hs], #mi{sel_side=Side,menu=Menu,type=Type}=Mi) ->
    ?CHECK_ERROR(),
    Elem = element(I, Menu),
    Text = menu_text(Elem, Type),
    case Elem of
	separator ->
	    draw_separator(X, Y, Mw);
	menu_toolbar ->
	    IconSize = wings_pref:get_value(menu_toolbar_size),
	    draw_menu_toolbar(IconSize, Mw, Side);
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
	{_,Sub,_,_,[more]=Ps} ->
	    menu_draw_1(Y, Ps, I, Mi,
			fun() ->
				draw_menu_text(X, Y, Text, Ps)
			end),
	    draw_submenu_marker(plain, Sub,
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
    %% Draw background for highlighted item.
    wings_io:set_color(wings_pref:get_value(menu_hilite)),
    Color = wings_pref:get_value(menu_hilite),
    Cw = ?CHAR_WIDTH,
    Right = W - (2*right_width(Ps)) - Cw,
    case Side of
	right ->
	    {X1,Y1,X2,Y2} = {Right, Y-?CHAR_HEIGHT, Right+3*Cw-2, Y+3},
	    wings_io:gradient_rect(X1, Y1, X2-X1, Y2-Y1, Color),
	    wings_io:set_color(wings_pref:get_value(menu_text));
	_left ->
	    {X1,Y1,X2,Y2} = {?CHAR_WIDTH, Y-?CHAR_HEIGHT, Right, Y+3},
	    wings_io:gradient_rect(X1, Y1, X2-X1, Y2-Y1, Color),
	    wings_io:set_color(wings_pref:get_value(menu_hilited_text))
    end,
    DrawLeft(),
    case {DrawRight,Side} of
	{ignore,_} -> ok;
	{_,right} ->
	    wings_io:set_color(wings_pref:get_value(menu_hilited_text)),
	    DrawRight();
	{_,_} ->
	    wings_io:set_color(wings_pref:get_value(menu_text)),
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
menu_text(separator, _) -> [];
menu_text(menu_toolbar, _) -> [].

draw_hotkey(_, _, _, []) -> ok;
draw_hotkey(X, Y, Pos, Hotkey) -> wings_io:text_at(X+Pos, Y, Hotkey).

draw_menu_text(X, Y, Text, Props) ->
    case proplists:get_value(crossmark, Props) of
	undefined ->
	    wings_io:unclipped_text(X, Y, Text);
	false ->
	    wings_io:unclipped_text(X, Y, Text);
	true ->
	    wings_io:unclipped_text(X-2*?CHAR_WIDTH, Y, [crossmark,$\s|Text]);
	grey ->
	    gl:pushAttrib(?GL_CURRENT_BIT),
	    gl:color3f(0.25, 0.25, 0.25),
	    wings_io:unclipped_text(X-2*?CHAR_WIDTH, Y, [crossmark]),
	    gl:popAttrib(),
	    wings_io:unclipped_text(X, Y, Text)
    end.

menu_toolbar_help(_,tools) ->
    Msg = wings_msg:button_format(?__(1,"Open the Tools menu")),
    wings_wm:message(Msg);
menu_toolbar_help(_,select) ->
    Msg1 = wings_msg:button_format(?__(2,"Open the Select menu")),
    Msg2 = wings_msg:button_format([], ?__(3,"Recall Stored Selection")),
    Msg3 = wings_msg:button_format([], [], ?__(4,"Store Selection")),
    Message = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Message);
menu_toolbar_help(SelMode, deselect) ->
    Msg1 = wings_msg:button_format(?__(5,"Deselect | Select All")),
    Msg2 = [],
    Msg3 = wings_msg:button_format([], [], ?__(6,"Deselect and close menu")),
    Msg4 = scroll_help(SelMode, deselect),
    Message = wings_msg:join([Msg1,Msg2,Msg3,Msg4]),
    wings_wm:message(Message);
menu_toolbar_help(SelMode, body) ->
    Msg1 = wings_toolbar:button_help_2(body, SelMode),
    Msg2 = scroll_help(SelMode, body),
    Message = wings_msg:join([Msg1,Msg2]),
    wings_wm:message(Message);
menu_toolbar_help(SelMode, history) ->
    Msg1 = wings_msg:button_format(wings_toolbar:button_help_2(undo, SelMode)),
    Msg2 = wings_msg:button_format([],[],wings_toolbar:button_help_2(redo, SelMode)),
    Msg3 = scroll_help(SelMode, history),
    Message = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Message);
menu_toolbar_help(SelMode, repeat) ->
    Msg1 = wings_msg:button_format(?__(7,"Repeat Drag"),?__(8,"Repeat Args"),
          ?__(9,"Repeat")),
    Msg2 = scroll_help(SelMode, repeat),
    Message = wings_msg:join([Msg1,Msg2]),
    wings_wm:message(Message);
menu_toolbar_help(SelMode, Icon) ->
    Msg1 = wings_msg:button_format(wings_toolbar:button_help_2(Icon, SelMode)),
    Msg2 = mmb_menu_toolbar_help(SelMode, Icon),
    Msg3 = rmb_menu_toolbar_help(SelMode, Icon),
    Msg4 = scroll_help(SelMode, Icon),
    Message = wings_msg:join([Msg1,Msg2,Msg3,Msg4]),
    wings_wm:message(Message).

scroll_help(SelMode, Icon) ->
    Scroll = wings_s:scroll() ++ ": ",
    case Icon of
        repeat -> Scroll ++ ?__(1,"Repeat Drag | Undo");
        history ->  Scroll ++ ?__(2,"Undo | Redo");
        edge when SelMode =:= edge ->
            None = Scroll ++ ?__(3,"Next/Previous Edge Loop"),
            Ctrl = wings_s:key(ctrl)++"+"++Scroll ++ ?__(4,"Grow/Shrink Edge Loop"),
            Alt = wings_s:key(alt)++"+"++Scroll ++ ?__(5,"Grow/Shrink Edge Ring"),
            wings_msg:join([None,Ctrl,Alt]);
        _ -> Scroll ++ ?__(6,"Select More | Select Less")
    end.

mmb_menu_toolbar_help(edge,Icon) ->
    Msg = case Icon of
      vertex -> [];
      edge -> ?__(1,"Edge Ring");
      face -> [];
      body -> []
    end,
    wings_msg:button_format([], Msg, []);
mmb_menu_toolbar_help(_,_) -> [].

rmb_menu_toolbar_help(SelMode,Icon) ->
    Msg = case Icon of
      _ when SelMode =:= body -> [];
      vertex -> [];
      edge when SelMode =:= vertex ->
          ?__(3,"Select edges which have both vertices selected");
      edge -> ?__(1,"Edge Loop");
      face when SelMode =:= edge -> ?__(2,"Edge Loop to Region");
      face -> [];
      body -> []
    end,
    wings_msg:button_format([], [], Msg).

help_text(#mi{sel=none}) ->
    wings_wm:message("");
help_text(#mi{menu=Menu,sel=Sel}=Mi) ->
    Elem = element(Sel, Menu),
    help_text_1(Elem, Mi).

help_text_1({Text,{Sub,_},_,[],_}, #mi{type=plain}) when Sub =/= 'VALUE' ->
    %% No specific help text for submenus in plain mode.
    Help = [Text|?__(1," submenu")],
    wings_wm:message(Help, "");
help_text_1({_,{Sub,_},_,SubMenuHelp,_}, #mi{type=plain}) when Sub =/= 'VALUE' ->
    wings_wm:message(SubMenuHelp, "");
help_text_1({_,{Name,Fun},_,_,Ps}, #mi{ns=Ns}=Mi) when is_function(Fun) ->
    %% "Submenu" in advanced mode.
    Help0 = Fun(help, [Name|Ns]),
    Help = help_text_2(Help0),
    magnet_help(Help, Ps, Mi);
help_text_1({_,_,_,Help0,Ps}, Mi) ->
    %% Plain entry - not submenu.
    Help = help_text_2(Help0),
    magnet_help(Help, Ps, Mi);
help_text_1(separator, _) -> ok;
help_text_1(menu_toolbar, _) -> ok.

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
	    Cw = ?CHAR_WIDTH,
	    Ch = ?CHAR_HEIGHT,
	    X = X0 + Mw - 5*Cw,
	    Y = Y0 - Ch + 1,
	    wings_io:border(X, Y, Cw, Ch-1, Color)
    end.

draw_submenu_marker(popup, _Item, _X, _Y) -> ok;
draw_submenu_marker(plain, _Item, X, Y) ->
    Cw = ?CHAR_WIDTH,
    H = (?CHAR_HEIGHT+2) div 3,
    ?CHECK_ERROR(),
    gl:'begin'(?GL_TRIANGLES),
    gl:vertex2i(X-Cw div 2, Y),
    gl:vertex2i(X-Cw, Y-H),
    gl:vertex2i(X-Cw, Y+H),
    gl:'end'(),
    ?CHECK_ERROR().

draw_separator(X, Y, Mw) ->
    ?CHECK_ERROR(),
    Cw = ?CHAR_WIDTH,
    LeftX = X-2*Cw+0.5,
    RightX = X+Mw-4*Cw+0.5,
    UpperY = Y-?SEPARATOR_HEIGHT+0.5,
    gl:lineWidth(1),
    wings_io:set_color(wings_pref:get_value(menu_text)),
    gl:'begin'(?GL_LINES),
    gl:vertex2f(LeftX, UpperY),
    gl:vertex2f(RightX, UpperY),
    gl:'end'(),
    gl:color3b(0, 0, 0).

%% Icon bar in context menus for quick selection mode changes using the mouse
draw_menu_toolbar(Size, Mw, Icon) ->
    ?CHECK_ERROR(),
    %Colors
    Col1 = wings_pref:get_value(menu_text),
    Col2 = wings_pref:get_value(selected_color),
    Col3 = wings_pref:get_value(menu_color),
    Col4 = case Col3 of
      {R,G,B,_} -> {R,G,B};
      RGB -> RGB
    end,
    % X positions
    IconX = case Size of
        big -> 24;
        small -> 16
    end,
    MidX = Mw div 2,
    Hx = MidX-IconX*5,
    Rx = MidX-IconX*4,
    Vx = MidX-IconX*2,
    Ex = MidX-IconX,
    Fx = MidX,
    Bx = MidX+IconX,
    Sx = MidX+IconX*3,
    Tx = MidX+IconX*4,
    % menu_toolbar background
    ToolbarCol = e3d_vec:mul(Col4, 0.8),
    wings_io:gradient_border(0, 0, Mw-1, IconX+4, ToolbarCol),
    % toolbar box highlight
    wings_io:set_color({1,1,1}),
    SelBox = outline_bitmap(Size),
    case Icon of
        vertex -> draw_icon(Size, Vx, SelBox);
        edge -> draw_icon(Size, Ex, SelBox);
        face -> draw_icon(Size, Fx, SelBox);
        body -> draw_icon(Size, Bx, SelBox);
        history -> draw_icon(Size, Hx, SelBox);
        repeat -> draw_icon(Size, Rx, SelBox);
        select -> draw_icon(Size, Sx, SelBox);
        tools -> draw_icon(Size, Tx, SelBox);
        _deselect -> ok
    end,
    % Draw Icons
    gl:color3fv(e3d_vec:mul(Col1,0.6)),
    draw_icon(Size, Hx, history_bitmap(Size)),
    draw_icon(Size, Rx, repeat_bitmap(Size)),
    draw_icon(Size, Sx, select_bitmap(Size)),
    draw_icon(Size, Tx, tools_bitmap(Size)),
    CubeBitmap = cube_bitmap(Size),
    draw_icon(Size, Vx, CubeBitmap),
    draw_icon(Size, Ex, CubeBitmap),
    draw_icon(Size, Fx, CubeBitmap),
    draw_icon(Size, Bx, CubeBitmap),

    % Add Selection Color to Cube icons
    gl:color3fv(Col2),
    draw_icon(Size, Vx, vertex_sel_bitmap(Size)),
    draw_icon(Size, Ex, edge_sel_bitmap(Size)),
    draw_icon(Size, Fx, face_sel_bitmap(Size)),
    draw_icon(Size, Bx, body_sel_bitmap(Size)),
    gl:color3b(0, 0, 0).

draw_icon(big, X, Bitmap) ->
    gl:rasterPos2i(X, 26),
    gl:bitmap(24, 24, 0, 0, 24, 0, Bitmap);
draw_icon(small, X, Bitmap) ->
    gl:rasterPos2i(X, 18),
    gl:bitmap(16, 16, 0, 0, 16, 0, Bitmap).

button_check(X, Mw) ->
    IconX = case wings_pref:get_value(menu_toolbar_size) of
        big -> 24;
        small -> 16
    end,
    MidX = Mw div 2,
    if X < MidX-IconX*5 -> deselect;
       X < MidX-IconX*4 -> history;
       X < MidX-IconX*3 -> repeat;
       X < MidX-IconX*2 -> deselect;
       X < MidX-IconX -> vertex;
       X < MidX -> edge;
       X < MidX+IconX -> face;
       X < MidX+IconX*2 -> body;
       X < MidX+IconX*3 -> deselect;
       X < MidX+IconX*4 -> select;
       X < MidX+IconX*5 -> tools;
       true -> deselect
    end.

repeat_bitmap(small) ->
    <<
    2#0000000000000000:16,
    2#0000001111000000:16,
    2#0000110000110000:16,
    2#0001000000001000:16,
    2#0010001111000100:16,
    2#0010010000100100:16,
    2#0100100000010010:16,
    2#0100100000010010:16,
    2#0100100100010010:16,
    2#0100100110010010:16,
    2#0010011111000100:16,
    2#0010000110000100:16,
    2#0001000100001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16>>;
repeat_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000111111000000000:24,
    2#000000011111111110000000:24,
    2#000000111000000111000000:24,
    2#000001100000000001100000:24,
    2#000011000011110000110000:24,
    2#000110001111111100011000:24,
    2#000110001100001100011000:24,
    2#001100011000000110001100:24,
    2#001100011000000110001100:24,
    2#001100011000100110001100:24,
    2#001100011000110000001100:24,
    2#001100001100111000001100:24,
    2#001100001111111100001100:24,
    2#000110000011111100011000:24,
    2#000110000000111000011000:24,
    2#000011000000110000110000:24,
    2#000001100000100001100000:24,
    2#000000111000000111000000:24,
    2#000000011111111110000000:24,
    2#000000000111111000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.
	
history_bitmap(small) ->
    <<
    2#0000000000000000:16,
    2#0000001111000000:16,
    2#0000110000110000:16,
    2#0001000001001000:16,
    2#0010000001100100:16,
    2#0010011111110100:16,
    2#0100000001100010:16,
    2#0100000001000010:16,
    2#0100001000000010:16,
    2#0100011000000010:16,
    2#0010111111100100:16,
    2#0010011000000100:16,
    2#0001001000001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16>>;
history_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000111111000000000:24,
    2#000000011111111110000000:24,
    2#000000111000000111000000:24,
    2#000001100000010001100000:24,
    2#000011000000011000110000:24,
    2#000110000000011100011000:24,
    2#000110001111111110011000:24,
    2#001100011111111110001100:24,
    2#001100010000011100001100:24,
    2#001100000010011000001100:24,
    2#001100000110010000001100:24,
    2#001100001110000010001100:24,
    2#001100011111111110001100:24,
    2#000110011111111100011000:24,
    2#000110001110000000011000:24,
    2#000011000110000000110000:24,
    2#000001100010000001100000:24,
    2#000000111000000111000000:24,
    2#000000011111111110000000:24,
    2#000000000111111000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

select_bitmap(small) ->
    <<
    2#0000000000000000:16,
    2#0000001111000000:16,
    2#0000110000110000:16,
    2#0001000000001000:16,
    2#0010000010000100:16,
    2#0010000011000100:16,
    2#0100000111100010:16,
    2#0100000111110010:16,
    2#0100001111000010:16,
    2#0100001100000010:16,
    2#0010010000000100:16,
    2#0010000000000100:16,
    2#0001000000001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16>>;
select_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000111111000000000:24,
    2#000000011111111110000000:24,
    2#000000111000000111000000:24,
    2#000001100000000001100000:24,
    2#000011000000000000110000:24,
    2#000110000000000100011000:24,
    2#000110000010001110011000:24,
    2#001100000011011100001100:24,
    2#001100000111111000001100:24,
    2#001100000111110000001100:24,
    2#001100000111111000001100:24,
    2#001100001111111100001100:24,
    2#001100001111110000001100:24,
    2#000110011110000000011000:24,
    2#000110011000000000011000:24,
    2#000011000000000000110000:24,
    2#000001100000000001100000:24,
    2#000000111000000111000000:24,
    2#000000011111111110000000:24,
    2#000000000111111000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

tools_bitmap(small) ->
    <<
    2#0000000000000000:16,
    2#0000001111000000:16,
    2#0000110000110000:16,
    2#0001000110001000:16,
    2#0010000110000100:16,
    2#0010000110000100:16,
    2#0100000110000010:16,
    2#0100000110000010:16,
    2#0100001111000010:16,
    2#0100011111100010:16,
    2#0010011001100100:16,
    2#0010001001000100:16,
    2#0001000000001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16>>;
tools_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000111111000000000:24,
    2#000000011111111110000000:24,
    2#000000111000000111000000:24,
    2#000001100001100001100000:24,
    2#000011000011110000110000:24,
    2#000110000011110000011000:24,
    2#000110000011110000011000:24,
    2#001100000011110000001100:24,
    2#001100000011110000001100:24,
    2#001100000011110000001100:24,
    2#001100000111111000001100:24,
    2#001100001111111100001100:24,
    2#001100001111111100001100:24,
    2#000110001110011100011000:24,
    2#000110001110011100011000:24,
    2#000011000110011000110000:24,
    2#000001100000000001100000:24,
    2#000000111000000111000000:24,
    2#000000011111111110000000:24,
    2#000000000111111000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

outline_bitmap(small) ->
    <<
    2#1111111111111111:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1000000000000001:16,
    2#1111111111111111:16>>;
outline_bitmap(big) ->
    <<
    2#111111111111111111111111:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#100000000000000000000001:24,
    2#111111111111111111111111:24>>.

cube_bitmap(small) ->
    wings_shape:cube_bitmap();
cube_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000011110000000000:24,
    2#000000001111111100000000:24,
    2#000000111101101111000000:24,
    2#000011110001100011110000:24,
    2#000111000001100000111000:24,
    2#000110000001100000011000:24,
    2#000110000001100000011000:24,
    2#000110000001100000011000:24,
    2#000110000001100000011000:24,
    2#000110000001100000011000:24,
    2#000110000011110000011000:24,
    2#000110001111111100011000:24,
    2#000110111100001111011000:24,
    2#000111110000000011111000:24,
    2#000111100000000001111000:24,
    2#000011111100001111110000:24,
    2#000000111111111111000000:24,
    2#000000000011110000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

vertex_sel_bitmap(small) ->
    wings_shape:vertex_sel_cube_bitmap();
vertex_sel_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000110000000000000000000:24,
    2#001111000000000000000000:24,
    2#001111000000000000000000:24,
    2#000110000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000001100000000000:24,
    2#000000000011110000000000:24,
    2#000000000011110000000000:24,
    2#000000000001100000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

edge_sel_bitmap(small) ->
    wings_shape:edge_sel_cube_bitmap();
edge_sel_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000110000000000:24,
    2#000000000001111100000000:24,
    2#000000000001101111000000:24,
    2#000000000001100011110000:24,
    2#000000000001100000111000:24,
    2#000000000001100000011000:24,
    2#000000000001100000011000:24,
    2#000000000001100000011000:24,
    2#000000000001100000011000:24,
    2#000000000001100000011000:24,
    2#000000000001110000011000:24,
    2#000000000000111100011000:24,
    2#000000000000001111011000:24,
    2#000000000000000011111000:24,
    2#000000000000000001110000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

face_sel_bitmap(small) ->
    wings_shape:face_sel_cube_bitmap();
face_sel_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000010000000000:24,
    2#000000000000011100000000:24,
    2#000000000000011111000000:24,
    2#000000000000011111100000:24,
    2#000000000000011111100000:24,
    2#000000000000011111100000:24,
    2#000000000000011111100000:24,
    2#000000000000011111100000:24,
    2#000000000000001111100000:24,
    2#000000000000000011100000:24,
    2#000000000000000000100000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

body_sel_bitmap(small) ->
    wings_shape:selcube_bitmap();
body_sel_bitmap(big) ->
    <<
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000010010000000000:24,
    2#000000001110011100000000:24,
    2#000000111110011111000000:24,
    2#000001111110011111100000:24,
    2#000001111110011111100000:24,
    2#000001111110011111100000:24,
    2#000001111110011111100000:24,
    2#000001111110011111100000:24,
    2#000001111100001111100000:24,
    2#000001110000000011100000:24,
    2#000001000011110000100000:24,
    2#000000001111111100000000:24,
    2#000000011111111110000000:24,
    2#000000000011110000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24,
    2#000000000000000000000000:24>>.

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

have_magnet(_, true) -> activated;
have_magnet(Ps, _) ->
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
    wings_dialog:dialog(?__(1,"Delete Hotkeys"), Dialog, Fun).

mk_dialog([{Key,Keyname,Cmd,Src}|T]) ->
    [mk_key_item(Key, Keyname, Cmd, Src)|mk_dialog(T)];
mk_dialog([]) ->
    [separator,{label,?__(1,"Check all hotkeys to be deleted.")}].

mk_key_item(Key, Keyname, Cmd, _Src) ->
    {Keyname ++ ": " ++ Cmd,false,[{key,Key}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wx stuff

wx_popup_menu_init(X0,Y0,Names,Menus0) ->
    Owner = wings_wm:this(),
    wx_popup_menu(X0,Y0,Names,Menus0,false,Owner),
    {push, fun(Ev) -> popup_event_handler(Ev, Owner) end}.

wx_popup_menu(X0,Y0,Names,Menus0,Magnet,Owner) ->
    Parent = get(top_frame),
    Pos = wxWindow:clientToScreen(get(gl_canvas), X0-10,Y0-10),
    HotKeys = wings_hotkey:matching(Names),
    is_list(Menus0) orelse erlang:error(Menus0),
    Menus1  = wings_plugin:menu(list_to_tuple(reverse(Names)), Menus0),
    Entries0 = [normalize_menu_wx(Entry, HotKeys, Names) || Entry <- lists:flatten(Menus1)],
    CreateMenu = fun() -> setup_dialog(Parent,Entries0, Magnet, Pos) end,
    Env = wx:get_env(),
    spawn_link(fun() ->
		       try
			   wx:set_env(Env),
			   {Dialog, Panel, Entries} = wx:batch(CreateMenu),
			   popup_events(Dialog, Panel, Entries, Magnet, undefined, Names, Owner)
		       catch _:Reason ->
			       io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()])
		       end,
		       normal
	       end),
    keep.

setup_dialog(Parent, Entries0, Magnet, Pos) ->
    Dialog = wxFrame:new(Parent, ?wxID_ANY, "Operation",
			 [{style, ?wxSTAY_ON_TOP bor ?wxFRAME_NO_TASKBAR},
			  {size, {100, 600}},
			  {pos, Pos}
			 ]),
    Panel = wxPanel:new(Dialog),
    %% wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_TRANSPARENT),
    wxPanel:setBackgroundColour(Panel, colorB(wings_pref:get_value(menu_color))),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MinHSzs = calc_min_sizes(Entries0, Panel, 0, 0),
    Entries = setup_popup(Entries0, 500, Sizer, MinHSzs, Panel, Magnet, []),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Main, Sizer, [{proportion, 1},{border, 5},
			      {flag, ?wxEXPAND bor ?wxALL}]),
    wxPanel:setSizer(Panel, Main),
    wxSizer:setSizeHints(Main, Dialog),
    wxPanel:setFocusIgnoringChildren(Panel),
    wxPanel:connect(Panel, kill_focus),
    wxFrame:show(Dialog),
    {Dialog, Panel, Entries}.

popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner) ->
    receive
	#wx{id=Id, obj=Obj, event=#wxMouse{type=enter_window}} ->
	    Line = wx:batch(fun() ->
				    setup_colors(Previous, colorB(menu_color), colorB(menu_text)),
				    setup_colors(Obj, colorB(menu_hilite),colorB(menu_hilited_text))
			    end),
	    #menu_pop{msg=Msg} = lists:keyfind(Id, 2, Entries),
	    wings_wm:psend(Owner, {message, Msg}),
	    popup_events(Dialog, Panel, Entries, Magnet, Line, Ns, Owner);
	#wx{event=#wxFocus{type=kill_focus}} ->
	    wxFrame:destroy(Dialog),
	    wings_wm:psend(Owner, cancel);
	#wx{id=Id, event=Ev=#wxMouse{type=What}}
	  when What =:= left_up; What =:= right_up; What =:= middle_up ->
	    wxFrame:destroy(Dialog),
	    MagnetClick = Magnet orelse magnet_pressed(wings_msg:free_rmb_modifier(), Ev),
	    popup_result(lists:keyfind(Id, 2, Entries), {What, MagnetClick}, Ns, Owner);
	Ev = #wx{} ->
	    io:format("Got Ev ~p ~n", [Ev]),
	    popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner)
    end.

magnet_pressed(?CTRL_BITS, #wxMouse{controlDown=true}) -> true;
magnet_pressed(?ALT_BITS, #wxMouse{altDown=true}) -> true;
magnet_pressed(?SHIFT_BITS, #wxMouse{shiftDown=true}) -> true;
magnet_pressed(?META_BITS, #wxMouse{metaDown=true}) -> true;
magnet_pressed(_, _) -> false.

popup_result(#menu_pop{type=submenu, name={Name, Menus}, opts=Opt}, What, Names, Owner) ->
    wings_wm:psend(Owner, {submenu, What, [Name|Names], Opt, Menus});
popup_result(#menu_pop{type=opt,name=Name}, _Click, Names, Owner) ->
    wings_wm:psend(Owner, {activate, build_command({Name,true}, Names)});
popup_result(#menu_pop{type=menu,name=CmdFun,opts=Opts}, {Click,MagnetClick}, Names, Owner)
  when is_function(CmdFun) ->
    Cmd = CmdFun(mouse_index(Click), Names),
    Magnet = is_magnet_active(Opts, MagnetClick),
    wings_wm:psend(Owner, {activate, insert_magnet_flags(Cmd, Magnet)});
popup_result(#menu_pop{type=menu,name=Name,opts=Opts}, {Click,MagnetClick}, Names, Owner) ->
    case {have_option_box(Opts), Name} of
	{true,_} ->
	    wings_wm:psend(Owner, {activate, build_command({Name,Click=/=left_up}, Names)});
	{false, {'VALUE', Cmd}} ->
	    Magnet = is_magnet_active(Opts, MagnetClick),
	    wings_wm:psend(Owner, {activate, build_command(Cmd, Names, Magnet)});
	{false, {_Name, Cmd}} when is_tuple(Cmd) ->
	    Magnet = is_magnet_active(Opts, MagnetClick),
	    wings_wm:psend(Owner, {activate, insert_magnet_flags(Cmd, Magnet)});
	{false, _} ->
	    Magnet = is_magnet_active(Opts, MagnetClick),
	    wings_wm:psend(Owner, {activate, build_command(Name, Names, Magnet)})
    end.

mouse_index(left_up) -> 1;
mouse_index(middle_up) -> 2;
mouse_index(right_up) -> 3.

popup_event_handler(cancel, _) ->
    wxPanel:setFocus(get(gl_canvas)),
    pop;
popup_event_handler({activate, Cmd}, Owner) ->
    Cmd =:= ignore orelse wings_wm:send_after_redraw(Owner, {action,Cmd}),
    wxPanel:setFocus(get(gl_canvas)),
    pop;
popup_event_handler({submenu, {What, MagnetClick}, Names, Opts, Menus}, Owner) ->
    {_, X, Y} = wings_io:get_mouse_state(),
    case is_function(Menus) of
	true ->
	    case Menus(mouse_index(What), Names) of
		ignore ->
		    wx_popup_menu(X,Y,Names, Menus(1, Names), MagnetClick, Owner);
		Next when is_list(Next) ->
		    wx_popup_menu(X,Y, Names, Next, MagnetClick, Owner);
		Action when is_tuple(Action); is_atom(Action) ->
		    Magnet = is_magnet_active(Opts, MagnetClick),
		    popup_event_handler({activate, insert_magnet_flags(Action, Magnet)}, Owner)
	    end;
	false ->
	    wx_popup_menu(X,Y,Names,Menus,MagnetClick,Owner)
    end;
popup_event_handler({message, Msg}, _Owner) ->
    wings_wm:message(Msg, ""),
    keep;
popup_event_handler(redraw,_) ->
    defer;
popup_event_handler(#mousemotion{},_) -> defer;
popup_event_handler(_Ev,_) ->
    io:format("Hmm ~p ~n",[_Ev]),
    keep.

calc_min_sizes([separator|Es], Win, C1, C2) ->
    calc_min_sizes(Es, Win, C1, C2);
calc_min_sizes([{submenu, Desc, _, _, _, _}|Es], Win, C1, C2) ->
    {W, _, _, _} = wxWindow:getTextExtent(Win, Desc),
    calc_min_sizes(Es, Win, max(W, C1), C2);
calc_min_sizes([{Desc, _, _, _, HK}|Es], Win, C1, C2) ->
    {WStr, _, _, _} = wxWindow:getTextExtent(Win, Desc),
    {WHK, _, _, _} = wxWindow:getTextExtent(Win, HK),
    calc_min_sizes(Es, Win, max(WStr, C1), max(WHK, C2));
calc_min_sizes([], _, C1, C2) ->
    {C1, C2}.

setup_popup([separator|Es], Id, Sizer, Sz, Parent, Magnet, Acc) ->
    Line = wxStaticLine:new(Parent),
    wxSizer:addSpacer(Sizer, 4),
    wxSizer:add(Sizer, Line,
		[{border, 10}, {proportion, 0},
		 {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}]),
    wxSizer:addSpacer(Sizer, 4),
    setup_popup(Es, Id, Sizer, Sz, Parent, Magnet, Acc);
setup_popup([{submenu, Desc, Name, Help0, Ps, _HK}|Es], Id, Sizer, Sz, Parent, Magnet, Acc) ->
    Panel = wxPanel:new(Parent, [{winid, Id}]),
    setup_colors([Panel], colorB(menu_color), colorB(menu_text)),
    Line = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addSpacer(Line, 3),
    wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc),[{proportion, 1}]),
    wxPanel:setSizerAndFit(Panel, Line),
    wxSizer:add(Sizer, Panel, [{flag, ?wxEXPAND},{proportion, 1}]),
    Help = if Help0 =:= [] -> Desc ++ ?__(1," submenu");
	      true -> Help0
	   end,
    {TipMsg, CmdMsg} = tooltip(Help, false, have_magnet(Ps, Magnet)),
    [wxWindow:setToolTip(Win, wxToolTip:new(TipMsg)) || Win <- [Panel,T1]],
    menu_connect([Panel,T1], [left_up, middle_up, right_up, enter_window]),
    Pop = #menu_pop{wxid=Id, type=submenu, name=Name, opts=Ps, msg=CmdMsg},
    setup_popup(Es, Id+2, Sizer, Sz, Parent, Magnet, [Pop|Acc]);
setup_popup([{Desc, Name, Help, Props, HK}|Es], Id, Sizer, Sz = {Sz1,Sz2}, Parent, Magnet, Acc) ->
    Panel = wxPanel:new(Parent, [{winid, Id}]),
    setup_colors([Panel], colorB(menu_color), colorB(menu_text)),
    Line  = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addSpacer(Line, 3),
    wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc),[{proportion, 0}]),
    wxSizer:setItemMinSize(Line, T1, Sz1, -1),
    wxSizer:addSpacer(Line, 10),
    wxSizer:add(Line, T2 = wxStaticText:new(Panel, Id, HK),  [{proportion, 0}]),
    wxSizer:setItemMinSize(Line, T2, Sz2, -1),
    wxSizer:addSpacer(Line, 10),
    BM = case OpBox = have_option_box(Props) of
	     true ->
		 Bitmap = wxArtProvider:getBitmap("wxART_LIST_VIEW",[{client, "wxART_MENU"}]),
		 wxSizer:add(Line, SBM = wxStaticBitmap:new(Panel, Id+1, Bitmap),
			     [{flag, ?wxALIGN_CENTER}]),
		 [SBM];
	     false ->
		 wxSizer:add(Line, 16, 16),
		 []
	 end,
    wxSizer:addSpacer(Line, 3),
    %% Windows doesn't catch enter_window on Panel below statictext
    %% so we need to set tooltip on all sub-windows
    {TipMsg, CmdMsg} = tooltip(Help, OpBox, have_magnet(Props, Magnet)),
    [wxWindow:setToolTip(Win, wxToolTip:new(TipMsg)) || Win <- [Panel,T1,T2|BM]],
    wxPanel:setSizerAndFit(Panel, Line),
    wxSizer:add(Sizer, Panel, [{flag, ?wxEXPAND}, {proportion, 1}]),
    menu_connect([Panel,T1,T2|BM], [left_up, middle_up, right_up, enter_window]),
    Pop = #menu_pop{wxid=Id, type=menu, name=Name, opts=Props, msg=CmdMsg},
    setup_popup(Es, Id+2, Sizer, Sz, Parent,Magnet,[Pop#menu_pop{wxid=Id+1, type=opt},Pop|Acc]);
setup_popup([], _, _, _, _, _, Acc) -> lists:reverse(Acc).

menu_connect(Windows, Evs) ->
    [ [wxWindow:connect(Win, Ev) || Ev <- Evs] || Win <- Windows].

setup_colors(Windows, Background, Foreground) when is_list(Windows) ->
    [wxWindow:setBackgroundColour(Win, Background) || Win <- Windows],
    [wxWindow:setForegroundColour(Win, Foreground) || Win <- Windows],
    ok;
setup_colors(undefined, _, _) -> ok;
setup_colors(Window, Background, Foreground) ->
    case wx:getObjectType(Window) of
	wxPanel ->
	    setup_colors([Window|wxWindow:getChildren(Window)], Background, Foreground),
	    wxWindow:refresh(Window),
	    Window;
	_ -> %% Get Parent who is a Panel
	    setup_colors(wx:typeCast(wxWindow:getParent(Window), wxPanel), Background, Foreground)
    end.

tooltip("", false, false) -> {"",""};
tooltip(Help, OptBox, Magnet) when is_list(Help) ->
    tooltip(Help, "", opt_help(OptBox), Magnet);
tooltip({Help}, OptBox, Magnet) ->
    tooltip(Help, "", opt_help(OptBox), Magnet);
tooltip({HelpL, HelpM}, OptBox, Magnet) ->
    tooltip(HelpL, HelpM, opt_help(OptBox), Magnet);
tooltip({HelpL, HelpM, ""}, OptBox, Magnet) ->
    tooltip(HelpL, HelpM, opt_help(OptBox), Magnet);
tooltip({HelpL, HelpM, HelpR}, _, Magnet) ->
    tooltip(HelpL, HelpM, HelpR, Magnet).

tooltip("", "", "", Magnet) ->
    Str = magnet_help(str, Magnet),
    {Str, Str};
tooltip(HelpL, "", "", Magnet) ->
    {str_clean(HelpL) ++ magnet_help(tip, Magnet),
     wings_msg:join(HelpL,magnet_help(str, Magnet))};
tooltip(HelpL, HelpM, "", Magnet) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~n" ++
		       ?__(2, "Middle mouse button") ++ ": ~ts",
		   [HelpL, HelpM]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, HelpM, ""),magnet_help(str, Magnet))};
tooltip(HelpL, "", HelpR, Magnet) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~n" ++
		       ?__(3, "Right mouse button") ++ ": ~ts",
		   [HelpL, HelpR]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, "", HelpR), magnet_help(str, Magnet))};
tooltip(HelpL, HelpM, HelpR, Magnet) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~n" ++
		       ?__(2, "Middle mouse button") ++ ": ~ts~n" ++
		       ?__(3, "Right mouse button") ++ ": ~ts",
		   [HelpL, HelpM, HelpR]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, HelpM, HelpR), magnet_help(str, Magnet))}.

str_clean([Char|Cs]) when is_integer(Char) ->
    [Char|str_clean(Cs)];
str_clean([List|Cs]) when is_list(List) ->
    [str_clean(List)|str_clean(Cs)];
str_clean([{_,Str}|Cs]) ->
    ["\n",str_clean(Str)|str_clean(Cs)];
str_clean([]) -> [].



opt_help(true) ->  ?__(1, "Open option dialog");
opt_help(false) -> "".

magnet_help(str, activated) -> wings_magnet:info_string();
magnet_help(Type, true) ->
    ModRmb = wings_msg:free_rmb_modifier(),
    ModName = wings_msg:mod_name(ModRmb),
    case Type of
	str -> "+" ++ ?__(2,"Click for Magnet") ++ ModName;
	tip -> "\n+" ++ ?__(2,"Click for Magnet") ++ ModName
    end;
magnet_help(_, _) -> "".

submenu_help([], Fun, Ns) when is_function(Fun) ->
    Fun(help, Ns);
submenu_help(Help, _, _) ->
    Help.

wx_command_event(Id) ->
    case ets:lookup(wings_menus, Id) of
	[#menu_entry{name=Name}] -> {menubar, {action, Name}};
	[] ->
	    io:format("~p:~p: Unmatched event Id ~p~n",[?MODULE,?LINE,Id]),
	    ignore
    end.

check_item(Name) ->
    case ets:match_object(wings_menus, #menu_entry{name=Name, type=?wxITEM_CHECK, _ = '_'}) of
	[] -> ok;
	[#menu_entry{object=MenuItem}] -> %% Toggle checkmark
	    Checked = wxMenuItem:isChecked(MenuItem),
	    wxMenuItem:check(MenuItem, [{check, not Checked}])
    end.

wx_menubar(Menus) ->
    ets:new(wings_menus, [named_table, {keypos,2}]),
    WinName = {menubar, geom},
    put(wm_active, WinName),
    MB = wxFrame:getMenuBar(get(top_frame)),
    Enter = fun({Str, Name, Fun}, Id) ->
		    {Menu, NextId} = setup_menu([Name], Id, Fun),
		    wxMenuBar:append(MB, Menu, Str),
		    NextId
	    end,
    try
	lists:foldl(Enter,100, Menus),
	%% Let wings handle the accelerators by itself
	Accel = wxAcceleratorTable:new(),
	wxWindow:setAcceleratorTable(get(top_frame), Accel),
	ok
    catch _ : Reason ->
	    io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()]),
	    error(Reason)
    end,
    erase(wm_active),
    ok.

setup_menu(Names, Id, Menus0) ->
    Menu   = wxMenu:new(),
    Menus1 = if is_function(Menus0) -> Menus0(#st{});
		is_list(Menus0) -> Menus0
	     end,
    Menus2  = wings_plugin:menu(list_to_tuple(reverse(Names)), Menus1),
    HotKeys = wings_hotkey:matching(Names),
    Menus = [normalize_menu_wx(Entry, HotKeys, Names) || Entry <- Menus2],
    Next  = create_menu(Menus, Id, Names, Menu),
    {Menu, Next}.

normalize_menu_wx(separator, _, _) -> separator;
normalize_menu_wx({S,Fun,Help,Ps}, Hotkeys, Ns) when is_function(Fun) ->
    Name = Fun(1, Ns),
    HK = match_hotkey(Name, Hotkeys, have_option_box(Ps)),
    %% io:format("Norm: ~p~n",[Name]),
    {S,Fun,Help,Ps,HK};
normalize_menu_wx({S, {Name, SubMenu}}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    HK = match_hotkey(Name, Hotkeys, false),
    {submenu, S, {Name, SubMenu}, submenu_help("", SubMenu, [Name|Ns]), [], HK};
normalize_menu_wx({S, {Name, SubMenu}, Ps}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    HK = match_hotkey(Name, Hotkeys, false),
    {submenu, S, {Name, SubMenu}, submenu_help("", SubMenu, [Name|Ns]), Ps, HK};
normalize_menu_wx({S,{Name,Fun},Help,Ps}, Hotkeys, Ns)
  when is_function(Fun) ->
    HK = match_hotkey(Name, Hotkeys, have_option_box(Ps)),
    {submenu, S, {Name, Fun}, submenu_help(Help, Fun, [Name|Ns]), Ps, HK};
normalize_menu_wx({S,Name,Help,Ps}, Hotkeys, _Ns) ->
    HK = match_hotkey(Name, Hotkeys, have_option_box(Ps)),
    {S,Name,Help,Ps,HK};
normalize_menu_wx({S,Name}, Hotkeys, _Ns) ->
    HK = match_hotkey(Name, Hotkeys, false),
    {S,Name,[],[],HK};
normalize_menu_wx({S,Name,[C|_]=Help}, Hotkeys, _Ns)
  when is_integer(C) ->
    HK = match_hotkey(Name, Hotkeys, false),
    {S,Name,Help,[], HK};
normalize_menu_wx({S,Name,Help}, Hotkeys, _Ns)
  when is_tuple(Help), tuple_size(Help) =< 3 ->
    HK = match_hotkey(Name, Hotkeys, false),
    {S,Name,Help,[], HK};
normalize_menu_wx({S,Name,Ps},Hotkeys, _Ns) ->
    HK = match_hotkey(Name, Hotkeys, have_option_box(Ps)),
    {S,Name,[],Ps, HK}.

create_menu([separator|Rest], Id, Names, Menu) ->
    wxMenu:appendSeparator(Menu),
    create_menu(Rest, Id, Names, Menu);
create_menu([{submenu, Desc, {Name, SubMenu0}, Help, _Ps, _HK}|Rest], Id, Names, Menu)
  when is_list(SubMenu0) ->
    {SMenu, NextId} = setup_menu([Name|Names], Id, SubMenu0),
    wxMenu:append(Menu, ?wxID_ANY, Desc, SMenu, [{help, Help}]),
    create_menu(Rest, NextId, Names, Menu);
create_menu([MenuEntry|Rest], Id, Names, Menu) ->
    {MenuItem, Check} = menu_item(MenuEntry, Menu, Id, Names),
    wxMenu:append(Menu, MenuItem),
    Check andalso wxMenuItem:check(MenuItem), %% Can not check until appended to menu..
    create_menu(Rest, Id+1, Names, Menu);
create_menu([], NextId, _, _) ->
    NextId.

menu_item({Desc0, Name, Help, Props, HotKey}, Parent, Id, Names) ->
    Desc = case HotKey of
	       [] -> Desc0;
	       KeyStr -> Desc0 ++ "\t' " ++ KeyStr ++ " '"
	       %%KeyStr -> Desc0 ++ "\t" ++ KeyStr
	   end,
    MenuId = case predefined_item(hd(Names),Name) of
		 false -> Id;
		 PId  -> PId
	     end,
    Command = case have_option_box(Props) of
		  true ->
		      case lists:reverse(Desc0) of
			  "..." ++ _ -> ok;
			  _ ->
			      io:format("Menu have option box ~p ~s~n",[Name, Desc0]),
			      io:format("  it should be marked with ...~n",[])
		      end,
		      {Name, true};
		  false ->
		      Name
	      end,
    {Type,Check} = case proplists:get_value(crossmark, Props) of
		       undefined -> {?wxITEM_NORMAL, false};
		       false -> {?wxITEM_CHECK, false};
		       _ -> {?wxITEM_CHECK, true} %% grey or true
		   end,
    MI = wxMenuItem:new([{parentMenu, Parent}, {id,MenuId},
			 {text,Desc}, {kind, Type}, {help,Help}]),
    true = ets:insert(wings_menus, #menu_entry{name=build_command(Command, Names),
					       object=MI, wxid=MenuId, type=Type}),
    {MI, Check}.

%% We want to use the prefdefined id where they exist (mac) needs for it's
%% specialized menus but we want our shortcuts hmm.
%% We also get little predefined icons for OS's that have that.
predefined_item(help, about)   -> ?wxID_ABOUT;
predefined_item(help, help)    -> ?wxID_HELP;
predefined_item(file, quit)    -> ?wxID_EXIT;
predefined_item(file, new)     -> ?wxID_NEW;
predefined_item(file, open)    -> ?wxID_OPEN;
predefined_item(file, save)    -> ?wxID_SAVE;
predefined_item(file, save_as) -> ?wxID_SAVEAS;
predefined_item(file, revert)  -> ?wxID_REVERT;
predefined_item(edit, undo)    -> ?wxID_UNDO;
predefined_item(edit, redo)    -> ?wxID_REDO;
predefined_item(_, _) ->  false.

colorB({R,G,B,A}) -> {trunc(R*255),trunc(G*255),trunc(B*255),trunc(A*255)};
colorB({R,G,B}) -> {trunc(R*255),trunc(G*255),trunc(B*255),255};
colorB(Pref) when is_atom(Pref) ->
    colorB(wings_pref:get_value(Pref)).
