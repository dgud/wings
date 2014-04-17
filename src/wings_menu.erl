%%
%%  wings_menu.erl --
%%
%%     Implementation of pulldown and popup menus.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%                     2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_menu).
-export([is_popup_event/1,menu/5,popup_menu/4,build_command/2,
	 kill_menus/0]).
-export([wx_menubar/1, id_to_name/1, check_item/1, str_clean/1]).
-export([update_menu/3, update_menu/4]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

-define(REPEAT, 99).
-define(REPEAT_ARGS, 98).
-define(REPEAT_DRAG, 97).

-record(menu_pop,
	{wxid,
	 name,
	 type,
	 opts,
	 msg=""
	}).

%% Top level menu entries
-record(menu_entry, {wxid, name, object, type}).

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
    wings_wm_menu:menu(X, Y, Owner, Name, Menu).

popup_menu(X, Y, Name, Menu) ->
    wx_popup_menu_init(X,Y,[Name],Menu).

kill_menus() ->
    case wings_wm:is_window(menu_killer) of
	true -> wings_wm:send(menu_killer, kill_menus);
	false -> ok
    end.

match_hotkey(Name, [{{_,Name},Key}|_], false) -> Key;
match_hotkey(Name, [{Name,Key}|_], false) -> Key;
match_hotkey(Name, [{{Name,false},Key}|_], true) -> Key;
match_hotkey(Name, [{{Name,true},Key}|_], true) -> Key;
match_hotkey(Name, [_|T], OptionBox) ->
    match_hotkey(Name, T, OptionBox);
match_hotkey(_N, [], _) ->
    [].

reduce_name({'ASK',_}=Ask) -> Ask;
reduce_name({Key,{_,_}=Tuple}) when is_atom(Key) ->
    reduce_name(Tuple);
reduce_name({Key,Val}) when is_atom(Key) -> Val;
reduce_name(Name) -> Name.

is_magnet_active(Ps, Flags) when is_list(Flags) ->
    have_magnet(Ps) andalso have_magnet(Flags);
is_magnet_active(Ps, Bool) when is_boolean(Bool) ->
    Bool andalso have_magnet(Ps).

%% key(#keyboard{sym=27}) -> cancel;
%% key(#keyboard{sym=?SDLK_INSERT}) -> insert;
%% key(#keyboard{unicode=$/}) -> insert;
%% key(#keyboard{sym=?SDLK_DELETE}) -> delete;
%% key(#keyboard{unicode=$\\}) -> delete;
%% key(_) -> none.

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

build_command(Name, Names) ->
    foldl(fun(N, Use={N, _}) -> Use;
	     (N, A) -> {N,A} end,
	  Name, Names).
build_command(Name, Names, true) ->
    build_command({'ASK',{[magnet],[Name]}}, Names);
build_command(Name, Names, false) ->
    build_command(Name, Names).

have_option_box(Ps) ->
    proplists:is_defined(option, Ps).

have_magnet(Ps) ->
    proplists:is_defined(magnet, Ps).

have_magnet(_, true) -> activated;
have_magnet(Ps, _) ->
    proplists:is_defined(magnet, Ps).


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
    Dialog = wxPopupTransientWindow:new(Parent, [{style, ?wxBORDER_NONE}]),
    Panel = wxPanel:new(Dialog),
    %% wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_TRANSPARENT),
    wxPanel:setBackgroundColour(Panel, colorB(wings_pref:get_value(menu_color))),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MinHSzs = calc_min_sizes(Entries0, Panel, 5, 5),
    Entries = setup_popup(Entries0, 500, Sizer, MinHSzs, Panel, Magnet, []),
    wxSizer:setMinSize(Sizer, 150, -1),
    wxSizer:add(Main, Sizer, [{proportion, 1}, {border, 5}, {flag, ?wxEXPAND bor ?wxALL}]),
    wxPanel:setSizer(Panel, Main),
    wxSizer:fit(Main, Panel),
    wxPopupTransientWindow:setClientSize(Dialog, wxWindow:getSize(Panel)),
    wxPopupTransientWindow:connect(Dialog, show),
    wxPopupTransientWindow:position(Dialog, Pos, {0,0}),
    wxPopupTransientWindow:popup(Dialog),
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
	#wx{id=Id, event=Ev=#wxMouse{type=What}}
	  when What =:= left_up; What =:= right_up; What =:= middle_up ->
	    wxWindow:destroy(Dialog),
	    MagnetClick = Magnet orelse magnet_pressed(wings_msg:free_rmb_modifier(), Ev),
	    popup_result(lists:keyfind(Id, 2, Entries), {What, MagnetClick}, Ns, Owner);
	#wx{event=#wxShow{}} ->
	    case wxTopLevelWindow:isShown(Dialog) of
		false ->
		    wxWindow:destroy(Dialog),
		    wings_wm:psend(Owner, cancel);
		true ->
		    popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner)
	    end;
	_Ev ->
	    io:format("Got Ev ~p ~n", [_Ev]),
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
    wings_wm:send_after_redraw(Owner, {action,Cmd}),
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
    calc_min_sizes(Es, Win, max(W+5, C1), C2);
calc_min_sizes([{Desc, _, _, _, HK}|Es], Win, C1, C2) ->
    {WStr, _, _, _} = wxWindow:getTextExtent(Win, Desc),
    {WHK, _, _, _} = wxWindow:getTextExtent(Win, HK),
    calc_min_sizes(Es, Win, max(WStr+5, C1), max(WHK+5, C2));
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
    wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc),[{proportion, 1},{flag, ?wxALIGN_CENTER}]),
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
    wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc),[{proportion, 0},{flag, ?wxALIGN_CENTER}]),
    wxSizer:setItemMinSize(Line, T1, Sz1, -1),
    wxSizer:addSpacer(Line, 10),
    wxSizer:addStretchSpacer(Line),
    wxSizer:add(Line, T2 = wxStaticText:new(Panel, Id, HK),  [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
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

check_item(Name) ->
    case ets:match_object(wings_menus, #menu_entry{name=Name, type=?wxITEM_CHECK, _ = '_'}) of
	[] -> ok;
	[#menu_entry{object=MenuItem}] -> %% Toggle checkmark
	    Checked = wxMenuItem:isChecked(MenuItem),
	    wxMenuItem:check(MenuItem, [{check, not Checked}])
    end.

update_menu(Menu, Item, Cmd) ->
    update_menu(Menu, Item, Cmd, undefined).
update_menu(Menu, Item, Cmd, Help) ->
    Id = predefined_item(Menu, Item),
    MI = case ets:lookup(wings_menus, Id) of
	     [#menu_entry{object=MO}] ->
		 MO;
	     [] when Menu =:= file, element(1, Item) =:= recent_file ->
		 FileId = predefined_item(menu, Menu),
		 [#menu_entry{object=File, type=submenu}] = ets:lookup(wings_menus, FileId),
		 N = wxMenu:getMenuItemCount(File),
		 MO=wxMenu:insert(File, N-2, Id),
		 ME=#menu_entry{name=build_command(Item,[file]), object=MO,
				wxid=Id, type=?wxITEM_NORMAL},
		 true = ets:insert(wings_menus, ME),
		 Id =:= ?wxID_FILE1 andalso wxMenu:insertSeparator(File, N-2),
		 MO
	 end,
    wxMenuItem:setText(MI, Cmd),
    is_list(Help) andalso wxMenuItem:setHelp(MI, Help).


wx_menubar(Menus) ->
    ets:new(wings_menus, [named_table, {keypos,2}]),
    WinName = {menubar, geom},
    put(wm_active, WinName),
    MB = wxMenuBar:new(),
    Enter = fun({Str, Name, Fun}, Id) ->
		    {Menu, NextId} = setup_menu([Name], Id, Fun),
		    wxMenuBar:append(MB, Menu, Str),
		    MenuId = predefined_item(menu, Name, NextId),
		    ME=#menu_entry{name=[Name], object=Menu, wxid=MenuId, type=submenu},
		    true = ets:insert(wings_menus, ME),
		    NextId+1
	    end,
    try
	lists:foldl(Enter, 200, Menus),
	wxFrame:setMenuBar(get(top_frame), MB),
	ok
    catch _ : Reason ->
	    io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()]),
	    error(Reason)
    end,
    erase(wm_active),
    ok.

id_to_name(Id) ->
    [#menu_entry{name=Name}] = ets:lookup(wings_menus, Id),
    Name.

setup_menu(Names, Id, Menus0) ->
    Menu   = wxMenu:new(),
    Menus1 = if is_function(Menus0) ->
		     io:format("Menus0 ~p~n", [Menus0]),
		     Menus0(#st{});
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
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    %% io:format("Norm: ~p~n",[Name]),
    {S,Fun,Help,Ps,HK};
normalize_menu_wx({S, {Name, SubMenu}}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    {submenu, S, {Name, SubMenu}, submenu_help("", SubMenu, [Name|Ns]), [], HK};
normalize_menu_wx({S, {Name, SubMenu}, Ps}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    {submenu, S, {Name, SubMenu}, submenu_help("", SubMenu, [Name|Ns]), Ps, HK};
normalize_menu_wx({S,{Name,Fun},Help,Ps}, Hotkeys, Ns)
  when is_function(Fun); is_list(Fun) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    {submenu, S, {Name, Fun}, submenu_help(Help, Fun, [Name|Ns]), Ps, HK};
normalize_menu_wx({S,Name,Help,Ps}, Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    {S,Name,Help,Ps,HK};
normalize_menu_wx({S,Name}, Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    {S,Name,[],[],HK};
normalize_menu_wx({S,Name,[C|_]=Help}, Hotkeys, _Ns)
  when is_integer(C) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    {S,Name,Help,[], HK};
normalize_menu_wx({S,Name,Help}, Hotkeys, _Ns)
  when is_tuple(Help), tuple_size(Help) =< 3 ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    {S,Name,Help,[], HK};
normalize_menu_wx({S,Name,Ps},Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    {S,Name,[],Ps, HK}.

create_menu([separator|Rest], Id, Names, Menu) ->
    wxMenu:appendSeparator(Menu),
    create_menu(Rest, Id, Names, Menu);
create_menu([{submenu, Desc, {Name, SubMenu0}, Help, _Ps, _HK}|Rest], Id, Names, Menu)
  when is_list(SubMenu0) ->
    {SMenu, NextId} = setup_menu([Name|Names], Id, SubMenu0),
    wxMenu:append(Menu, ?wxID_ANY, Desc, SMenu, [{help, Help}]),
    ME=#menu_entry{name=[Name|Names], object=SMenu, wxid=NextId, type=submenu},
    true = ets:insert(wings_menus, ME),
    create_menu(Rest, NextId+1, Names, Menu);
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
	       KeyStr ->
		   %% Quote to avoid windows stealing keys
		   case os:type() of
		       {win32, _} -> Desc0 ++ "\t'" ++ KeyStr ++ "'";
		       _ -> Desc0 ++ "\t" ++ KeyStr
		   end
	   end,
    MenuId = predefined_item(hd(Names),Name, Id),
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
    Cmd = case is_function(Command) of
	      true -> Command(1, #st{});
	      false -> build_command(Command, Names)
	  end,
    true = ets:insert(wings_menus,
		      #menu_entry{name=Cmd,object=MI, wxid=MenuId, type=Type}),
    {MI, Check}.

%% We want to use the prefdefined id where they exist (mac) needs for it's
%% specialized menus but we want our shortcuts hmm.
%% We also get little predefined icons for OS's that have that.
predefined_item(Menu, Item, DefId) ->
    case predefined_item(Menu,Item) of
	false -> DefId;
	PId  -> PId
    end.

predefined_item(help, about)   -> ?wxID_ABOUT;
predefined_item(help, help)    -> ?wxID_HELP;
predefined_item(menu, file)    -> ?wxID_FILE;
predefined_item(file, quit)    -> ?wxID_EXIT;
predefined_item(file, new)     -> ?wxID_NEW;
predefined_item(file, open)    -> ?wxID_OPEN;
predefined_item(file, save)    -> ?wxID_SAVE;
predefined_item(file, save_as) -> ?wxID_SAVEAS;
predefined_item(file, revert)  -> ?wxID_REVERT;
predefined_item(file, {recent_file,N}) -> ?wxID_FILE + N; %% Zero numbered
predefined_item(menu, edit)    -> ?wxID_EDIT;
predefined_item(edit, undo)    -> ?wxID_UNDO;
predefined_item(edit, redo)    -> ?wxID_REDO;
predefined_item(edit, preferences) -> ?wxID_PREFERENCES;
predefined_item(edit, Fun) when is_function(Fun) -> ?wxID_PREFERENCES;
%% Make it easy to find repeat
predefined_item(edit, repeat)  -> ?REPEAT;
predefined_item(edit, repeat_args) -> ?REPEAT_ARGS;
predefined_item(edit, repeat_drag) -> ?REPEAT_DRAG;
predefined_item(_M, _C) ->
    %% io:format("Ignore ~p ~p~n",[_M,_C]),
    false.

colorB({R,G,B,A}) -> {trunc(R*255),trunc(G*255),trunc(B*255),trunc(A*255)};
colorB({R,G,B}) -> {trunc(R*255),trunc(G*255),trunc(B*255),255};
colorB(Pref) when is_atom(Pref) ->
    colorB(wings_pref:get_value(Pref)).
