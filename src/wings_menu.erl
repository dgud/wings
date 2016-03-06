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
	 kill_menus/0, predefined_item/2]).
-export([setup_menus/2, id_to_name/1, check_item/1, str_clean/1]).
-export([update_menu/3, update_menu/4,
	 update_menu_enabled/3, update_menu_hotkey/2]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,sort/1]).

-define(REPEAT, 99).
-define(REPEAT_ARGS, 98).
-define(REPEAT_DRAG, 97).
-define(SEL_VERTEX, 96).
-define(SEL_EDGE, 95).
-define(SEL_FACE, 94).
-define(SEL_BODY, 93).
-define(VIEW_WORKMODE, 92).
-define(VIEW_ORTHO, 91).
-define(VIEW_AXES, 90).
-define(VIEW_GROUND, 89).

%% -record(menu_pop,
%% 	{wxid,
%% 	 name,
%% 	 type,
%% 	 opts,
%% 	 msg=""
%% 	}).

%% menu entries, the name, type and opts are used differently
%% depending on if they are used in top_level menus or pop menus
%%
%%   The Help field is normalized to
%%      String       for plain (pull-down) menus
%%      {L,M,R}      for pop-up menus
%%
-record(menu, {wxid,      % wxId number
	       object,    % wxObject
	       name,      % menu_name atom or fun {atom, fun}
	       type=menu, % menu | submenu | separator
	       desc,      % Displayed string
	       help = [], % Strings
	       opts = [], % options
	       hk=[]}).   % hotkey


is_popup_event(#mousebutton{button=3,x=X0,y=Y0,state=?SDL_RELEASED,mod=Mod}) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    {yes,X,Y,Mod};
is_popup_event(#wx{obj=Win, event=#wxMouse{type=right_up, x=X0, y=Y0}}) ->
    {yes, wxWindow:clientToScreen(Win, X0, Y0)};
is_popup_event(#wx{event=#wxCommand{type=command_right_click}}) ->
    {yes, wx_misc:getMousePosition()};
is_popup_event(_Event) ->
    no.


menu(X, Y, Owner, Name, Menu) ->
    wings_wm_menu:menu(X, Y, Owner, Name, Menu).

popup_menu(X, Y, Name, Menu) %% Should be removed, the next should be used !!
  when is_number(X), is_number(Y) ->
    Win = ?GET(gl_canvas),
    wx_popup_menu_init(Win, wxWindow:clientToScreen(Win, X, Y), [Name], Menu);
popup_menu(Parent, {_,_} = GlobalPos, Name, Menu) ->
    wx_popup_menu_init(Parent, GlobalPos, [Name], Menu).

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
reduce_name({tweak,Val}) -> Val;
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

have_color(Ps) ->
    proplists:is_defined(color, Ps).

have_magnet(Ps) ->
    proplists:is_defined(magnet, Ps).

have_magnet(_, true) -> activated;
have_magnet(Ps, _) ->
    proplists:is_defined(magnet, Ps).


wx_popup_menu_init(Parent,GlobalPos,Names,Menus0) ->
    Owner = wings_wm:this(),
    Entries = wx_popup_menu(Parent,GlobalPos,Names,Menus0,false,Owner),
    {push, fun(Ev) -> popup_event_handler(Ev, {Parent,Owner}, Entries) end}.

wx_popup_menu(Parent,Pos,Names,Menus0,Magnet,Owner) ->
    HotKeys = wings_hotkey:matching(Names),
    is_list(Menus0) orelse erlang:error(Menus0),
    Menus1   = wings_plugin:menu(list_to_tuple(reverse(Names)), Menus0),
    Entries0 = [normalize_menu_wx(Entry, HotKeys, Names) || Entry <- lists:flatten(Menus1)],
    Entries1 = format_hotkeys(Entries0, pretty),
    {Entries2,_}  = lists:foldl(fun(ME, {List, Id}) ->
					{[ME#menu{wxid=Id},
					  ME#menu{wxid=Id+1, type=opt}|List],
					 Id+2}
				end, {[],500}, Entries1),
    Entries = reverse(Entries2),
    MEs0 = [ME#menu{name=undefined} || ME <- Entries],
    CreateMenu = fun() -> setup_dialog(Parent, MEs0, Magnet, Pos) end,
    Env = wx:get_env(),
    spawn_link(fun() ->
		       try
			   wx:set_env(Env),
			   {Dialog, Panel, MEs} = wx:batch(CreateMenu),
			   popup_events(Dialog, Panel, MEs, Magnet, undefined, Names, Owner),
			   wxPopupTransientWindow:destroy(Dialog)
		       catch _:Reason ->
			       io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()])
		       end,
		       normal
	       end),
    Entries.

setup_dialog(Parent, Entries0, Magnet, {X0,Y0}=ScreenPos) ->
    X  = X0-20,
    Y1 = Y0-10,
    Dialog = wxPopupTransientWindow:new(Parent, [{style, ?wxBORDER_SIMPLE}]),
    Panel = wxPanel:new(Dialog),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    %% wxPanel:setBackgroundStyle(Panel, ?wxBG_STYLE_TRANSPARENT),
    wxWindow:setBackgroundColour(Panel, colorB(wings_pref:get_value(menu_color))),
    wxWindow:setBackgroundColour(Dialog, colorB(wings_pref:get_value(menu_color))),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MinHSzs = calc_min_sizes(Entries0, Panel, 5, 5),
    Entries = setup_popup(Entries0, Sizer, MinHSzs, Panel, Magnet, []),
    wxSizer:setMinSize(Sizer, 225, -1),
    wxSizer:addSpacer(Main, 5),
    wxSizer:add(Main, Sizer, [{proportion, 1}, {border, 5}, {flag, ?wxEXPAND bor ?wxALL}]),
    wxSizer:addSpacer(Main, 5),
    wxPanel:setSizer(Panel, Main),
    wxSizer:fit(Main, Panel),
    wxPopupTransientWindow:setClientSize(Dialog, wxWindow:getSize(Panel)),
    wxPopupTransientWindow:connect(Dialog, show),
    {_, MaxH} = wx_misc:displaySize(),
    {_,H} = wxPopupTransientWindow:getSize(Dialog),
    Y = if ((Y1+H) > MaxH) -> max(0, (MaxH-H-5));
            true -> Y1
        end,
    menu_connect([Panel], [left_up, middle_up, right_up]),
    wxPopupTransientWindow:position(Dialog, {X,Y}, {0,0}),
    wxPopupTransientWindow:connect(Panel, char_hook, [{skip, true}]),
    wxPopupTransientWindow:popup(Dialog),
    wxPanel:setFocusIgnoringChildren(Panel),
    %% Color active menuitem
    {MX, MY} = wxWindow:screenToClient(Panel, ScreenPos),
    case find_active_panel(Panel, MX, MY) of
	{false,_} -> ignore;
	{ActId, ActPanel} ->
	    self() ! #wx{id=ActId, obj= ActPanel, event=#wxMouse{type=enter_window}}
    end,
    {Dialog, Panel, Entries}.

%% If the mouse is not moved after popping up the menu, the meny entry
%% is not active, find_active_panel finds the active row.
find_active_panel(Panel, MX, MY) ->
    {_,_,WinWidth,_} = wxWindow:getRect(Panel),
    case MX > 0 andalso MX < WinWidth of
	true -> find_active_panel_1(Panel, MY);
	false -> {false, outside}
    end.

find_active_panel_1(Panel, MY) ->
    MainSizer = wxWindow:getSizer(Panel),
    [_,SizerItem,_] = wxSizer:getChildren(MainSizer),
    Sizer = wxSizerItem:getSizer(SizerItem),
    Children = wxSizer:getChildren(Sizer),
    Active = fun(SItem, _) ->
		     {_, SY, _, SH} = wxSizerItem:getRect(SItem),
		     case MY > (SY) andalso MY < (SY+SH) of
			 false -> {false, outside};
			 true ->
			     Active = wxSizerItem:getWindow(SItem),
			     case wx:is_null(Active) orelse wxWindow:getId(Active) of
				 true -> {false, inside};
				 Id when Id < 0 -> {false, inside};
				 Id -> throw({Id, wx:typeCast(Active,wxPanel)})
			     end
		     end
	     end,
    try
	lists:foldl(Active, {false, inside}, Children)
    catch Found ->
	    Found
    end.

popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner) ->
    receive
	#wx{id=Id, obj=Obj, event=#wxMouse{type=enter_window}} ->
	    Set = fun() ->
			  setup_colors(Previous, colorB(menu_color), colorB(menu_text)),
			  setup_colors(Obj, colorB(menu_hilite),colorB(menu_hilited_text))
		  end,
	    Line = wx:batch(Set),
	    #menu{help=Msg} = lists:keyfind(Id, 2, Entries),
	    wings_wm:psend(Owner, {message, Msg}),
	    popup_events(Dialog, Panel, Entries, Magnet, Line, Ns, Owner);
	#wx{id=Id0, event=Ev=#wxMouse{y=Y, x=X}} ->
	    Id = case Id0 > 0 orelse find_active_panel(Panel, X, Y) of
		     true -> Id0;
		     {false, _} = No -> No;
		     {AId, _} -> AId
		 end,
	    What = mouse_button(Ev),
	    case Id of
		{false, outside} when What =:= right_up ->
		    Pos = wxWindow:clientToScreen(Dialog,{X,Y}),
		    wxPopupTransientWindow:position(Dialog, Pos, {0,0}),
		    popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner);
		{false, _} ->
		    popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner);
		_Integer ->
		    wxPopupTransientWindow:dismiss(Dialog),
		    MagnetClick = Magnet orelse
			magnet_pressed(wings_msg:free_rmb_modifier(), Ev),
		    wings_wm:psend(Owner, {click, Id, {What, MagnetClick}, Ns})
	    end;
	#wx{event=#wxShow{}} ->
	    case wxTopLevelWindow:isShown(Dialog) of
		false ->
		    wings_wm:psend(Owner, cancel);
		true ->
		    popup_events(Dialog, Panel, Entries, Magnet, Previous, Ns, Owner)
	    end;
	#wx{event=#wxKey{keyCode=Key}} ->
	    if Key =:= ?WXK_ESCAPE ->
		    wxPopupTransientWindow:dismiss(Dialog),
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
magnet_pressed(?META_BITS, #wxMouse{metaDown=true}) -> true;
magnet_pressed(_, _) -> false.

mouse_index(left_up) -> 1;
mouse_index(middle_up) -> 2;
mouse_index(right_up) -> 3.

mouse_button(#wxMouse{type=What, controlDown = Ctrl, altDown = Alt, metaDown = Meta}) ->
    case wings_pref:get_value(num_buttons) of
        1 ->
            case {What,Alt,Ctrl} of
                {left_up,true,false} -> middle_up;
                {left_up,false,true} -> right_up;
                _ -> What
            end;
        2 ->
            case {What,(Ctrl or Meta)} of
                {right_up,true} -> middle_up;
                _ -> What
            end;
        _ -> What
    end.

popup_event_handler(cancel, _, _) ->
    wxPanel:setFocus(get(gl_canvas)),
    pop;
popup_event_handler({click, Id, Click, Ns}, {Parent,Owner}=Own, Entries0) ->
    case popup_result(lists:keyfind(Id, 2, Entries0), Click, Ns, Owner) of
	pop -> pop;
	{submenu, Names, Menus, MagnetClick} ->
	    {_, X, Y} = wings_io:get_mouse_state(),
	    Entries = wx_popup_menu(Parent, {X,Y}, Names, Menus, MagnetClick, Owner),
	    {replace, fun(Ev) -> popup_event_handler(Ev, Own, Entries) end}
    end;
popup_event_handler({message, Msg}, _Owner, _) ->
    wings_wm:message(Msg, ""),
    keep;
popup_event_handler(redraw,_,_) ->
    defer;
popup_event_handler(#mousemotion{},_,_) -> defer;
popup_event_handler(_Ev,_,_) ->
    %% io:format("Hmm ~p ~n",[_Ev]),
    keep.

popup_result(#menu{type=submenu, name={Name, Menus}, opts=Opts}, {What, MagnetClick}, Names0, Owner) ->
    Names = [Name|Names0],
    case is_function(Menus) of
	true ->
	    case Menus(mouse_index(What), Names) of
		ignore ->
		    {submenu, Names, Menus(1, Names), MagnetClick};
		Next when is_list(Next) ->
		    {submenu, Names, Next, MagnetClick};
		Action when is_tuple(Action); is_atom(Action) ->
		    Magnet = is_magnet_active(Opts, MagnetClick),
		    Cmd = insert_magnet_flags(Action, Magnet),
		    Cmd =:= ignore orelse wings_wm:send_after_redraw(Owner, {action,Cmd}),
		    pop
	    end;
	false ->
	    {submenu, Names, Menus, MagnetClick}
    end;

popup_result(#menu{type=opt,name=Name}, _Click, Names, Owner) ->
    Cmd = build_command({Name,true}, Names),
    Cmd =:= ignore orelse wings_wm:send_after_redraw(Owner, {action,Cmd}),
    pop;
popup_result(#menu{type=menu,name=CmdFun,opts=Opts}, {Click,MagnetClick}, Names, Owner)
  when is_function(CmdFun) ->
    Cmd0 = CmdFun(mouse_index(Click), Names),
    Magnet = is_magnet_active(Opts, MagnetClick),
    Cmd = insert_magnet_flags(Cmd0, Magnet),
    Cmd =:= ignore orelse wings_wm:send_after_redraw(Owner, {action,Cmd}),
    pop;
popup_result(#menu{type=menu,name=Name,opts=Opts}, {Click,MagnetClick}, Names, Owner) ->
    Cmd = case {have_option_box(Opts), Name} of
	      {true,_} -> build_command({Name,Click=/=left_up}, Names);
	      {false, {'VALUE', Cmd0}} ->
		  Magnet = is_magnet_active(Opts, MagnetClick),
		  build_command(Cmd0, Names, Magnet);
	      {false, {_Name, Cmd0}} when is_tuple(Cmd0) ->
		  Magnet = is_magnet_active(Opts, MagnetClick),
		  insert_magnet_flags(Cmd0, Magnet);
	      {false, _} ->
		  Magnet = is_magnet_active(Opts, MagnetClick),
		  build_command(Name, Names, Magnet)
	  end,
    Cmd =:= ignore orelse wings_wm:send_after_redraw(Owner, {action,Cmd}),
    pop.

calc_min_sizes([#menu{type=Type, desc=Desc, hk=HK}|Es], Win, C1, C2)
  when Type=:=menu;Type=:=submenu ->
    {WStr, _, _, _} = wxWindow:getTextExtent(Win, Desc),
    {WHK, _, _, _} = wxWindow:getTextExtent(Win, HK),
    calc_min_sizes(Es, Win, max(WStr+5, C1), max(WHK+5, C2));
calc_min_sizes([#menu{}|Es], Win, C1, C2) ->
    calc_min_sizes(Es, Win, C1, C2);
calc_min_sizes([], _, C1, C2) ->
    {C1, C2}.

setup_popup([#menu{type=separator}|Es], Sizer, Sz, Parent, Magnet, Acc) ->
    Line = wxStaticLine:new(Parent, [{size,{-1,2}}]),
    wxSizer:addSpacer(Sizer, 4),
    wxSizer:add(Sizer, Line, [{border, 10}, {proportion, 0},
			      {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}]),
    wxSizer:addSpacer(Sizer, 4),
    setup_popup(Es, Sizer, Sz, Parent, Magnet, Acc);
setup_popup([#menu{type=submenu, wxid=Id, desc=Desc, help=Help0, opts=Ps, hk=HK}=ME|Es],
	    Sizer, Sz = {Sz1,Sz2}, Parent, Magnet, Acc) ->
    Panel = wxPanel:new(Parent, [{winid, Id}]),
    setup_colors([Panel], colorB(menu_color), colorB(menu_text)),
    Line = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addSpacer(Line, 3),
    Controls =
	case HK of
	    [] ->
		wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc), [{proportion, 1},{flag, ?wxALIGN_CENTER}]),
		[Panel,T1];
	    _ ->
		wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc), [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
		wxSizer:setItemMinSize(Line, T1, Sz1, -1),
		wxSizer:addSpacer(Line, 10),
		wxSizer:addStretchSpacer(Line),
		wxSizer:add(Line, T2 = wxStaticText:new(Panel, Id, HK),  [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
		wxSizer:setItemMinSize(Line, T2, Sz2, -1),
		wxSizer:addSpacer(Line, 10),
		wxSizer:add(Line, 16, 16),
		[Panel,T1,T2]
	end,
    Help = if Help0 =:= [] -> Desc ++ ?__(1," submenu");
	      true -> Help0
	   end,
    {TipMsg, CmdMsg} = tooltip(Help, false, have_magnet(Ps, Magnet)),
    [wxWindow:setToolTip(Win, wxToolTip:new(TipMsg)) || Win <- [Panel,T1]],
    wxPanel:setSizerAndFit(Panel, Line),
    wxSizer:add(Sizer, Panel, [{flag, ?wxEXPAND},{proportion, 1}]),
    menu_connect(Controls, [left_up, middle_up, right_up, enter_window]),
    setup_popup(Es, Sizer, Sz, Parent, Magnet, [ME#menu{help=CmdMsg}|Acc]);
setup_popup([#menu{type=menu, wxid=Id, desc=Desc, help=Help, opts=Props, hk=HK}=ME|Es],
	    Sizer, Sz = {Sz1,Sz2}, Parent, Magnet, Acc) ->
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
    BM = case {OpBox = have_option_box(Props),have_color(Props)} of
	     {true,_} ->
		 Bitmap = wxArtProvider:getBitmap("wxART_LIST_VIEW",[{client, "wxART_MENU"}]),
		 SBM = wxStaticBitmap:new(Panel, Id+1, Bitmap),
		 wxSizer:add(Line, SBM, [{flag, ?wxALIGN_CENTER}]),
		 [SBM];
	     {false, true} ->
		 {_,H} = wxWindow:getSize(T1),
		 SBM = create_color_box(Id, Panel, H, Props),
		 wxSizer:add(Line, SBM, [{flag, ?wxALIGN_CENTER}]),
		 [SBM];
	     {false, false} ->
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
    setup_popup(Es, Sizer, Sz, Parent,Magnet, [ME#menu{help=CmdMsg}|Acc]);
setup_popup([#menu{type=opt}=ME|Es], Sizer, Sz, Parent, Magnet, Acc) ->
    setup_popup(Es, Sizer, Sz, Parent, Magnet, [ME|Acc]);
setup_popup([], _, _, _, _, Acc) -> lists:reverse(Acc).

create_color_box(Id, Panel, H, Props) ->
    {R,G,B} = wings_color:rgb3bv(proplists:get_value(color, Props)),
    Image = wxImage:new(1,1,<<R,G,B>>),
    wxImage:rescale(Image,10,H-2),
    Bitmap = wxBitmap:new(Image),
    SBM = wxStaticBitmap:new(Panel, Id+1, Bitmap),
    wxImage:destroy(Image),
    wxBitmap:destroy(Bitmap),
    SBM.

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
    case ets:match_object(wings_menus, #menu{name=Name, type=?wxITEM_CHECK, _ = '_'}) of
	[] -> ok;
	[#menu{object=MenuItem}] -> %% Toggle checkmark
	    Checked = wxMenuItem:isChecked(MenuItem),
	    wxMenuItem:check(MenuItem, [{check, not Checked}])
    end.

update_menu(Menu, Item, Cmd) ->
    update_menu(Menu, Item, Cmd, undefined).

update_menu(file, Item = {recent_file, _}, delete, _) ->
    Id = menu_item_id(file, Item),
    FileId = predefined_item(menu, file),
    [#menu{object=File, type=submenu}] = ets:lookup(wings_menus, FileId),
    true = wxMenu:delete(File, Id),
    ets:delete(wings_menus, Id),
    ok;
update_menu(Menu, Item, Cmd0, Help) ->
    Id = menu_item_id(Menu, Item),
    MI = case ets:lookup(wings_menus, Id) of
	     [#menu{object=MO}] ->
		 MO;
	     [] when Menu =:= file, element(1, Item) =:= recent_file ->
		 FileId = predefined_item(menu, Menu),
		 [#menu{object=File, type=submenu}] = ets:lookup(wings_menus, FileId),
		 N  = wxMenu:getMenuItemCount(File),
		 MO = wxMenu:insert(File, N-2, Id, [{text, Cmd0}]),
		 ME=#menu{name=build_command(Item,[file]), object=MO,
			  wxid=Id, type=?wxITEM_NORMAL},
		 true = ets:insert(wings_menus, ME),
		 Id =:= ?wxID_FILE1 andalso wxMenu:insertSeparator(File, N-2),
		 MO
	 end,
    Cmd = setup_hotkey(MI, Cmd0),
    wxMenuItem:setText(MI, Cmd),
    is_list(Help) andalso wxMenuItem:setHelp(MI, Help).

update_menu_enabled(Menu, Item, Enabled)
  when is_boolean(Enabled) ->
    case menu_item_id(Menu, Item) of
	false -> ignore;
	Id ->
	    [#menu{object=MI}] = ets:lookup(wings_menus, Id),
	    case wxMenuItem:isCheckable(MI) of 
		true  -> wxMenuItem:check(MI, [{check,Enabled}]);
		false -> wxMenuItem:enable(MI, [{enable, Enabled}])
	    end
    end;
update_menu_enabled(_Menu, _Item, _Enabled) ->
    ignore.

update_menu_hotkey(Action, HotKeyStr) ->
    case ets:match_object(wings_menus, #menu{name=Action, _='_'}) of
	[] -> ok; %% Ignore it is a popupmenu entry
	[#menu{object=MI}] ->
	    Label = wxMenuItem:getLabel(MI),
	    case HotKeyStr of
		"" -> wxMenuItem:setText(MI, Label);
		_ -> wxMenuItem:setText(MI, Label ++ "\t" ++ HotKeyStr)
	    end
    end.

menu_item_id(Menu, Item) ->
    case predefined_item(Menu, Item) of
	Id when is_integer(Id) ->
	    Id;
	false ->
	    case ets:match_object(wings_menus, #menu{name={Menu,Item}, _='_'}) of
		[#menu{wxid=Id}] -> Id;
		[] when Menu =:= view -> %% Auto find {view, {show, Item}} used for toolbar
		    case ets:match_object(wings_menus, #menu{name={view,{show,Item}}, _='_'}) of
			[#menu{wxid=Id}] -> Id;
			[] -> false
		    end;
		[] -> false
	    end
    end.

setup_hotkey(MI, Cmd) ->
    case lists:member($\t, Cmd) of
	true -> %% Already have one use the new one
	    Cmd;
	false ->
	    try wxMenuItem:getText(MI) of
		Old ->
		    case string:chr(Old, $\t) of
			0 -> Cmd; %% Old string have no hotkey
			Idx ->
			    HotKeyStr = string:substr(Old, Idx),
			    string:concat(Cmd, HotKeyStr)
		    end
	    catch _:Reason ->
		    io:format("~p:~p GetTextFailed ~p ~p~n",[?MODULE,?LINE, MI, Reason]),
		    Cmd
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_menus(MB, Menus) ->
    Enter = fun({Str, Name, List}, Id) ->
		    {Menu, NextId} = setup_menu([Name], Id, List),
		    wxMenuBar:append(MB, Menu, Str),
		    MenuId = predefined_item(menu, Name, NextId),
		    ME=#menu{name=[Name], object=Menu, wxid=MenuId, type=submenu},
		    true = ets:insert(wings_menus, ME),
		    NextId+1
	    end,
    lists:foldl(Enter, 200, Menus).


id_to_name(?SEL_VERTEX) -> {select, vertex};
id_to_name(?SEL_EDGE) -> {select, edge};
id_to_name(?SEL_FACE) -> {select, face};
id_to_name(?SEL_BODY) -> {select, body};
id_to_name(Id) ->
    [#menu{name=Name}] = ets:lookup(wings_menus, Id),
    Name.

setup_menu(Names, Id, Menus1) when is_list(Menus1) ->
    Menu   = wxMenu:new(),
    Menus2  = wings_plugin:menu(list_to_tuple(reverse(Names)), Menus1),
    HotKeys = wings_hotkey:matching(Names),
    Menus3 = [normalize_menu_wx(Entry, HotKeys, Names) || Entry <- Menus2],
    Menus = format_hotkeys(Menus3, wx),
    Next  = create_menu(Menus, Id, Names, Menu),
    {Menu, Next}.

normalize_menu_wx(separator, _, _) ->
    #menu{type=separator};
normalize_menu_wx({S,Fun,Help,Ps}, Hotkeys, Ns) when is_function(Fun) ->
    Name = Fun(1, Ns),
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    #menu{type=menu, desc=S, name=Fun, help=Help, opts=Ps, hk=HK};
normalize_menu_wx({S, {Name, SubMenu}}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    Name0 = name_for_hotkey(Name, Ns, SubMenu),
    HK = match_hotkey(reduce_name(Name0), Hotkeys, false),
    #menu{type=submenu, desc=S, name={Name, SubMenu},
	  help=submenu_help("", SubMenu, [Name|Ns]), hk=HK};
normalize_menu_wx({S, {Name, SubMenu}, Ps}, Hotkeys, Ns)
  when is_list(SubMenu); is_function(SubMenu) ->
    Name0 = name_for_hotkey(Name, Ns, SubMenu),
    HK = match_hotkey(reduce_name(Name0), Hotkeys, false),
    #menu{type=submenu, desc=S, name={Name, SubMenu},
	  help=submenu_help("", SubMenu, [Name|Ns]), opts=Ps, hk=HK};
normalize_menu_wx({S,{Name,Fun},Help,Ps}, Hotkeys, Ns)
  when is_function(Fun); is_list(Fun) ->
    Name0 = name_for_hotkey(Name, Ns, Fun),
    HK = match_hotkey(reduce_name(Name0), Hotkeys, have_option_box(Ps)),
    #menu{type=submenu, desc=S, name={Name, Fun},
	  help=submenu_help(Help, Fun, [Name|Ns]), opts=Ps, hk=HK};
normalize_menu_wx({S,Name,Help,Ps}, Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    #menu{desc=S, name=Name, help=Help, opts=Ps, hk=HK};
normalize_menu_wx({S,Name}, Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    #menu{desc=S, name=Name, hk=HK};
normalize_menu_wx({S,Name,[C|_]=Help}, Hotkeys, _Ns)
  when is_integer(C) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    #menu{desc=S,name=Name,help=Help,hk=HK};
normalize_menu_wx({S,Name,Help}, Hotkeys, _Ns)
  when is_tuple(Help), tuple_size(Help) =< 3 ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, false),
    #menu{desc=S,name=Name,help=Help,hk=HK};
normalize_menu_wx({S,Name,Ps},Hotkeys, _Ns) ->
    HK = match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps)),
    #menu{desc=S,name=Name,opts=Ps,hk=HK}.

name_for_hotkey(Name, Ns, Fun) when is_function(Fun) ->
    case Fun(1, Ns) of
	SM when is_tuple(SM) -> SM;
	_ -> Name
    end;
name_for_hotkey(Name, _, _) -> Name.

format_hotkeys([#menu{type=separator}=H|T], Style) ->
    [H|format_hotkeys(T, Style)];
format_hotkeys([#menu{hk=HK0}=H|T], Style) ->
    Hotkey = wings_hotkey:format_hotkey(HK0, Style),
    [H#menu{hk=Hotkey}|format_hotkeys(T, Style)];
format_hotkeys([], _Style) -> [].

create_menu([#menu{type=separator}|Rest], Id, Names, Menu) ->
    wxMenu:appendSeparator(Menu),
    create_menu(Rest, Id, Names, Menu);
create_menu([#menu{type=submenu, desc=Desc, name={Name,SubMenu0}, help=Help}=ME0|Rest], Id, Names, Menu)
  when is_list(SubMenu0) ->
    {SMenu, NextId} = setup_menu([Name|Names], Id, SubMenu0),
    wxMenu:append(Menu, ?wxID_ANY, Desc, SMenu, [{help, Help}]),
    ME=ME0#menu{name=[Name|Names], object=SMenu, wxid=NextId},
    true = ets:insert(wings_menus, ME),
    create_menu(Rest, NextId+1, Names, Menu);
create_menu([MenuEntry|Rest], Id, Names, Menu) ->
    {MenuItem, Check} = menu_item(MenuEntry, Menu, Id, Names),
    wxMenu:append(Menu, MenuItem),
    Check andalso wxMenuItem:check(MenuItem), %% Can not check until appended to menu..
    create_menu(Rest, Id+1, Names, Menu);
create_menu([], NextId, _, _) ->
    NextId.

menu_item(#menu{desc=Desc0, name=Name, help=Help, opts=Props, hk=HotKey}=ME, Parent, Id, Names) ->
    Desc = menu_item_desc(Desc0, HotKey),
    MenuId = predefined_item(hd(Names),Name, Id),
    Command = case have_option_box(Props) of
		  true ->
		      case lists:reverse(Desc0) of
			  "..." ++ _ -> ok;
			  _ ->
			      io:format("Menu have option box ~p ~ts~n",[Name, Desc0]),
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
    true = ets:insert(wings_menus, ME#menu{name=Cmd,object=MI, wxid=MenuId, type=Type}),
    {MI, Check}.

menu_item_desc(Desc, []) ->
    Desc;
menu_item_desc(Desc, HotKey) ->
    %% Quote to avoid Windows stealing keys.
    case os:type() of
	{win32, _} -> Desc ++ "\t'" ++ HotKey ++ "'";
	_ -> Desc ++ "\t" ++ HotKey
    end.

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
%% Also all toolbar stuff (only once)
predefined_item(select, vertex) -> ?SEL_VERTEX;
predefined_item(select, edge) -> ?SEL_EDGE;
predefined_item(select, face) -> ?SEL_FACE;
predefined_item(select, body) -> ?SEL_BODY;

predefined_item(view, workmode) -> ?VIEW_WORKMODE;
predefined_item(view, orthogonal_view) -> ?VIEW_ORTHO;
predefined_item(show, show_axes) -> ?VIEW_AXES;
predefined_item(show, show_groundplane) -> ?VIEW_GROUND;

predefined_item(toolbar, open) -> predefined_item(file, open);
predefined_item(toolbar, save) -> predefined_item(file, save);
predefined_item(toolbar, undo) -> predefined_item(edit, undo);
predefined_item(toolbar, redo) -> predefined_item(edit, redo);
predefined_item(toolbar, pref) -> predefined_item(edit, preferences);

predefined_item(toolbar, vertex) -> predefined_item(select, vertex);
predefined_item(toolbar, edge)   -> predefined_item(select, edge);
predefined_item(toolbar, face)   -> predefined_item(select, face);
predefined_item(toolbar, body)   -> predefined_item(select, body);

predefined_item(toolbar, workmode)      -> predefined_item(view, workmode);
predefined_item(toolbar, orthogonal_view) -> predefined_item(view, orthogonal_view);
predefined_item(toolbar, show_groundplane) -> predefined_item(show, show_groundplane);
predefined_item(toolbar, show_axes)        -> predefined_item(show, show_axes);

predefined_item(_M, _C) ->
%%    io:format("Ignore ~p ~p~n",[_M,_C]),
    false.

colorB({R,G,B,A}) -> {trunc(R*255),trunc(G*255),trunc(B*255),trunc(A*255)};
colorB({R,G,B}) -> {trunc(R*255),trunc(G*255),trunc(B*255),255};
colorB(Pref) when is_atom(Pref) ->
    colorB(wings_pref:get_value(Pref)).
