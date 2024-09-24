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
-export([is_popup_event/1,popup_menu/4,build_command/2,
	 kill_menus/0, predefined_item/2]).
-export([setup_menus/2, id_to_name/1, check_item/1, str_clean/1]).
-export([update_menu/3, update_menu/4,
	 update_menu_enabled/3, update_menu_hotkey/2]).

%% Reuse in tweak windows
-export([normalize_menu_wx/3, calc_min_sizes/4,
	 format_hotkeys/2, setup_popup/7,
	 setup_colors/3,
	 entry_msg/2, entry_cmd/2, entry_wins/1, set_entry_id/2]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

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
-define(CHECK_MARK, [10003,32]).  % 10003 = unicode for check mark character

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

is_popup_event(#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED,mod=Mod}) ->
    {yes,X,Y,Mod};
is_popup_event(#wx{obj=Win, event=#wxMouse{type=right_up, x=X0, y=Y0}}) ->
    {yes, wxWindow:clientToScreen(Win, X0, Y0)};
is_popup_event(#wx{event=#wxCommand{type=command_right_click}}) ->
    {yes, wx_misc:getMousePosition()};
is_popup_event(_Event) ->
    no.

popup_menu(X, Y, Name, Menu) %% Should be removed, the next should be used !!
  when is_number(X), is_number(Y) ->
    Win = wings_wm:this_win(),
    wx_popup_menu_init(Win, wxWindow:clientToScreen(Win, X, Y), [Name], Menu);
popup_menu(Parent, {_,_} = GlobalPos, Name, Menu) ->
    wx_popup_menu_init(Parent, GlobalPos, [Name], Menu).

kill_menus() ->
    case wings_wm:is_window(menu_killer) of
	true -> wings_wm:send(menu_killer, kill_menus);
	false -> ok
    end.

match_hotkey([Cmd1, Cmd2, Cmd3], HotKeys, Opt) ->
    [match_hotkey(Cmd1, HotKeys, Opt),
     match_hotkey(Cmd2, HotKeys, Opt),
     match_hotkey(Cmd3, HotKeys, Opt)];
match_hotkey(Name, [{{_,Name},Key}|_], false) -> Key;
match_hotkey(Name, [{Name,Key}|_], false) -> Key;
match_hotkey(Name, [{{Name,false},Key}|_], true) -> Key;
match_hotkey(Name, [{{Name,true},Key}|_], true) -> Key;
match_hotkey(Name, [_|T], OptionBox) ->
    match_hotkey(Name, T, OptionBox);
match_hotkey(_N, [], _) ->
    [].

reduce_name([Cmd1, Cmd2, Cmd3]) ->
    [reduce_name(Cmd1),
     reduce_name(Cmd2),
     reduce_name(Cmd3)];
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

build_names(Term, Acc)
  when not is_tuple(Term) ->
    lists:reverse([Term|Acc]);
build_names({Term,Term2}, Acc)
  when is_boolean(Term2) ->
    lists:reverse([Term|Acc]);
build_names({Term1,Term2}, Acc) ->
    build_names(Term2, [Term1|Acc]).

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
    case wings_wm:grabbed_focus_window() of
        dialog_blanket ->
            wings_wm:send(dialog_blanket, user_attention);
        _ ->
            Owner = wings_wm:this(),
            Entries = wx_popup_menu(Parent,GlobalPos,Names,Menus0,false,dialog_blanket),
            {TopW,TopH} = wings_wm:top_size(),
            Op = {push, fun(Ev) -> popup_event_handler(Ev, {Parent,Owner}, Entries) end},
            wings_wm:new(dialog_blanket, {0,0,highest}, {TopW,TopH}, Op),
            wings_wm:grab_focus(dialog_blanket)
	end,
    keep.

wx_popup_menu(Parent,Pos,Names,Menus0,Magnet,Owner) ->
    Entries0 = make_entries(Names, Menus0, pretty),
    {Entries1,_}  = lists:foldl(fun(ME, {List, Id}) ->
					{[ME#menu{wxid=Id},
					  ME#menu{wxid=Id+1, type=opt}|List],
					 Id+2}
				end, {[],500}, Entries0),
    Entries = reverse(Entries1),
    MEs = [ME#menu{name=undefined} || ME <- Entries],
    MenuData = wx:batch(fun() -> setup_dialog(Parent, MEs, Magnet, Pos, ?GET(menu_cache)) end),
    Env = wx:get_env(),
    spawn_link(fun() ->
		       try
			   wx:set_env(Env),
                           register(wings_menu_process, self()),
                           send_enter_window(MenuData, Pos),
			   popup_events(MenuData, Magnet, undefined, Names, Owner),
                           close_menu_frame(MenuData),
                           wxWindow:setFocus(Parent)
		       catch _:Reason:ST ->
			       io:format("CRASH ~p ~p~n",[Reason, ST])
		       end,
		       normal
	       end),
    Entries.

setup_dialog(Parent, Entries, Magnet, ScreenPos, ignore) ->
    do_setup_dialog(Parent, Entries, Magnet, ScreenPos);
setup_dialog(Parent, Entries, Magnet, ScreenPos, undefined) ->
    setup_dialog(Parent, Entries, Magnet, ScreenPos, #{});
setup_dialog(Parent, Entries, Magnet, ScreenPos, Cache) ->
    TopParent = get_toplevel(Parent),
    case maps:get({Entries, TopParent}, Cache, undefined) of
        undefined ->
            MenuData = do_setup_dialog(TopParent, Entries, Magnet, ScreenPos),
            case maps:get(overlay, MenuData) of
                none ->
                    %% Note we leak popup windows menus here,
                    %% like autouv, should it be cleaned up?
                    ?SET(menu_cache, Cache#{{Entries, TopParent} => MenuData});
                _ ->  %% Only for popuptransient windows
                    ?SET(menu_cache,ignore)
            end,
            MenuData;
        #{frame := Frame} = MenuData ->
            Entries0 = maps:get(entries, MenuData),
            Col = maps:get(colors, MenuData),
            menu_sel_cleanup(Col, Entries0),

            Pos = fit_menu_on_display(Frame,ScreenPos),
            wxWindow:move(Frame, Pos),
            wxPopupTransientWindow:popup(Frame),
            MenuData
    end.

do_setup_dialog(TopParent, Entries0, Magnet, ScreenPos) ->
    {Overlay, Frame, KbdFocus} = make_menu_frame(TopParent, ScreenPos),
    Panel = wxPanel:new(Frame),
    wxWindow:setFont(Panel, ?GET(system_font_wx)),
    {{R,G,B,A},FG} = {colorB(menu_color),colorB(menu_text)},
    Cols = {{R,G,B,A}, FG},
    catch wxFrame:setTransparent(Frame, 240),
    wxWindow:setBackgroundColour(Frame, {R,G,B, 240}),
    wxWindow:setBackgroundColour(Panel, {R,G,B, 240}),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MinHSzs = calc_min_sizes(Entries0, Panel, 5, 5),
    Entries = setup_popup(Entries0, Sizer, MinHSzs, Cols, Panel, Magnet, []),
    wxSizer:setMinSize(Sizer, 225, -1),
    wxSizer:addSpacer(Main, 5),
    wxSizer:add(Main, Sizer, [{proportion, 1}, {border, 5}, {flag, ?wxEXPAND bor ?wxALL}]),
    wxSizer:addSpacer(Main, 5),
    wxPanel:setSizer(Panel, Main),
    wxSizer:fit(Main, Panel),
    wxWindow:setClientSize(Frame, wxWindow:getSize(Panel)),
    wxWindow:move(Frame, fit_menu_on_display(Frame, ScreenPos)),
    show_menu_frame(Overlay, Frame, KbdFocus),
    #{overlay=>Overlay, frame=>Frame, panel=>Panel, entries=>Entries, colors=>Cols}.

get_toplevel(Win) ->
    Parent = try wxWindow:isTopLevel(Win) of
                 true  -> wx:null();
                 false -> wxWindow:getParent(Win)
             catch _:_ ->
                     no_exists
             end,
    case wx:is_null(Parent) of
        true -> Win;
        false -> get_toplevel(Parent)
    end.

show_menu_frame(none, Frame, _Focus) ->
    wxPopupTransientWindow:popup(Frame);
show_menu_frame(_, Frame, KbdFocus) ->
    KbdFocus(),  %% Set keyboard focus so we can catch ESC on mac
    wxFrame:show(Frame).

close_menu_frame(#{overlay:=none, frame:=Frame}) ->
    wxPopupTransientWindow:dismiss(Frame);
close_menu_frame(#{overlay:=Overlay, frame:=Frame}) ->
    wxWindow:hide(Frame),
    wxFrame:destroy(Overlay).

send_enter_window(#{panel:=Panel}, ScreenPos) ->
    %% Color active menuitem
    {MX, MY} = wxWindow:screenToClient(Panel, ScreenPos),
    case find_active_panel(Panel, MX, MY) of
	{false,_} -> ignore;
	{ActId, ActPanel} ->
	    self() ! #wx{id=ActId, obj=ActPanel,
			 event=#wxMouse{type=enter_window,x=0,y=0,
					leftDown=false,middleDown=false,rightDown=false,
					controlDown=false,shiftDown=false,altDown=false,metaDown=false,
					wheelRotation=0, wheelDelta=0, linesPerAction=0}}
    end.

make_menu_frame(Parent, Pos) ->
    case os:type() of
        {_, darwin} ->
            %% PopupTransientWindow did not work on Mac on wxWidgets 3.1.3 atleast
            %% So we make our on own overlay handling there
            %% on the other transparent windows don't work on some linux'es
            %% So we can't use this for all OS's
            make_overlay(Parent,Pos);
        _ ->
            Frame = wxPopupTransientWindow:new(Parent, [{style, ?wxBORDER_SIMPLE}]),
            EvH = fun(Ev, _) -> catch wings_menu_process ! Ev end,
            wxPopupTransientWindow:connect(Frame, show, [{callback, EvH}]),
            {none, Frame, none}
    end.

make_overlay(Parent, ScreenPos) ->
    OL = wxFrame:new(),
    Flags = ?wxFRAME_TOOL_WINDOW bor ?wxFRAME_FLOAT_ON_PARENT bor ?wxFRAME_NO_TASKBAR,
    TCol = case {os:type(), {?wxMAJOR_VERSION, ?wxMINOR_VERSION}} of
               {{_, linux}, Ver} when Ver >= {3,0} ->
                   wxFrame:setBackgroundStyle(OL, 3), %% ?wxBG_STYLE_TRANSPARENT
                   0;
               {{_, darwin}, _} ->
                   13;  %% No events received if completely transparent ??
               _ ->
                   1
           end,
    DisplayID = wxDisplay:getFromPoint(ScreenPos),
    Display = wxDisplay_new(DisplayID),
    {DX,DY,DW,DH} = wxDisplay:getClientArea(Display),
    true = wxFrame:create(OL, Parent, -1, "", [{pos,{DX,DY}},{size, {DW,DH}},{style, Flags}]),
    wxFrame:setBackgroundColour(OL, {0,0,0,TCol}),
    catch wxFrame:setTransparent(OL, TCol),
    Panel = wxWindow:new(OL, -1, [{size, {DW,DH}}, {style, ?wxWANTS_CHARS}]),
    EvH = fun(#wx{event=#wxKey{keyCode=Key}}, _) ->
                  if Key =:= ?WXK_ESCAPE -> wings_menu_process ! cancel;
                     true -> ok
                  end;
             (#wx{event=#wxMouse{type=right_up,y=Y, x=X}}, _) ->
                  wings_menu_process ! {move, wxWindow:clientToScreen(OL,{X,Y})};
             (_Ev, Obj) ->
                  wxEvent:skip(Obj),
                  %% ?dbg("Cancel menu: ~w~n",[_Ev]),
                  catch wings_menu_process ! cancel
          end,
    [wxWindow:connect(Panel, Ev, [{callback, EvH}]) ||
        Ev <- [left_up, middle_up, right_up, char, char_hook]],
    wxFrame:show(OL),

    FrameFlags = case os:type() of
                     {_, linux} -> ?wxSTAY_ON_TOP;  %% Hmm needed for some reason
                     _ -> ?wxFRAME_FLOAT_ON_PARENT
                 end,
    Flags = ?wxFRAME_TOOL_WINDOW bor FrameFlags bor ?wxFRAME_NO_TASKBAR,
    Frame = wxFrame:new(OL, -1, "", [{style, FrameFlags}]),

    {OL, Frame, fun() -> wxWindow:setFocus(Panel) end}.

wxDisplay_new(DisplayID) ->
    New = wings_u:id(new),
    try  %% new
        wxDisplay:New(DisplayID)
    catch _:_ -> %% old
            wxDisplay:New([{n, DisplayID}])
    end.

popup_events(MenuData, Magnet, Previous, Ns, Owner) ->
    receive
	#wx{id=Id, obj=Obj,event=#wxMouse{type=enter_window}} ->
            Set = fun() ->
                          if Obj =/= Previous ->
                                  {BG,FG} = maps:get(colors, MenuData),
                                  setup_colors(Previous, BG, FG),
                                  setup_colors(Obj, colorB(menu_hilite),colorB(menu_hilited_text)),
                                  Obj;
                             true ->
                                  Previous
                          end
                  end,
	    Line = wx:batch(Set),
	    wings_status:message(Owner, entry_msg(Id, maps:get(entries, MenuData)), ""),
            popup_events(MenuData, Magnet, Line, Ns, Owner);
	#wx{id=Id0, event=Ev=#wxMouse{y=Y, x=X}} ->
            Id = case Id0 > 0 orelse find_active_panel(maps:get(panel, MenuData), X, Y) of
		     true -> Id0;
		     {false, _} = No -> No;
		     {AId, _} -> AId
		 end,
            %% ?dbg("Ev: ~w ~w ~w~n",[Ev, Id0, Id]),
	    case Id of
		{false, outside} ->
                    {BG,FG} = maps:get(colors, MenuData),
                    setup_colors(Previous, BG, FG),
		    wings_wm:psend(Owner, cancel);
                {false, inside} ->
                    popup_events(MenuData, Magnet, Previous, Ns, Owner);
                Active when is_integer(Active) ->
                    {BG,FG} = maps:get(colors, MenuData),
                    setup_colors(Previous, BG, FG),
		    MagnetClick = Magnet orelse
			magnet_pressed(wings_msg:free_rmb_modifier(), Ev),
		    wings_wm:psend(Owner, {click, Id, {mouse_button(Ev), MagnetClick}, Ns})
	    end;
        cancel ->
            wings_wm:psend(Owner, cancel);
        {move, {X,Y}} ->
            Frame = maps:get(frame, MenuData),
            Pos = fit_menu_on_display(Frame, {X,Y}),
            wxWindow:move(Frame, Pos),
            wings_wm:psend(Owner, redraw),
            popup_events(MenuData, Magnet, Previous, Ns, Owner);
        #wx{event=#wxShow{show=false}} ->
            wings_wm:psend(Owner, cancel);
	_Ev ->
	    %% ?dbg("Got Ev ~p ~n", [_Ev]),
	    popup_events(MenuData, Magnet, Previous, Ns, Owner)
    end.

fit_menu_on_display(Frame, {MX,MY} = Pos) ->
    {WW,WH} = wxWindow:getSize(Frame),
    %% When multiple resolution displays are present, there is a situation which
    %% the window being shared partially by two of them - and the window being
    %% scaled up - the Display ID returned is -1. In order to avoid a crash it we
    %% get the ID for the main window (Frame's parent) - the menu is shown on it.
    DisplayID =
        case wxDisplay:getFromPoint(Pos) of
            -1 -> wxDisplay:getFromWindow(wxWindow:getParent(Frame));
            Id -> Id
        end,
    Display = wxDisplay_new(DisplayID),
    {DX,DY,DW,DH} = wxDisplay:getClientArea(Display),
    MaxW = abs(DX-MX)+WW,
    PX = if MaxW > DW -> (DX+DW)-(WW+5);
            true -> max(DX+5, (MX-5)) %% Move so mouse is inside menu
         end,
    MaxH = abs(DY-MY)+WH,
    PY = if MaxH > DH -> (DY+DH)-(WH+5);
             true -> max(DY+5, (MY-5)) %% Move so mouse is inside menu
         end,
    wxDisplay:destroy(Display),
    {PX,PY}.

%% If the mouse is not moved after popping up the menu, the menu entry
%% is not active, find_active_panel finds the active row.
find_active_panel(Panel, MX, MY) ->
    {_,_,WinW,WinH} = wxWindow:getRect(Panel),
    case MX > 0 andalso MX < WinW andalso MY > 0 andalso MY < WinH of
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
    wings_wm:release_focus(),
    delete;
popup_event_handler({click, Id, Click, Ns}, {Parent,Owner}, Entries0) ->
    case popup_result(lists:keyfind(Id, 2, Entries0), Click, Ns, Owner) of
	pop ->
            wings_wm:release_focus(),
            delete;
	{submenu, Names, Menus, MagnetClick} ->
	    Entries = wx_popup_menu(Parent, wx_misc:getMousePosition(), Names, Menus, MagnetClick, dialog_blanket),
	    {replace, fun(Ev) -> popup_event_handler(Ev, {Parent,Owner}, Entries) end}
    end;
popup_event_handler(redraw,_,_) ->
    keep;
popup_event_handler(#keyboard{sym=?SDLK_ESCAPE}, {_, _}, _) ->
    wings_menu_process ! cancel, %% Keyboard focus fails on mac wxWidgets-3.1.3
    keep;
popup_event_handler(#mousemotion{}, _, _) ->
    keep;
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

calc_min_sizes([#menu{type=Type, desc=Desc, opts=Ps, hk=HK}|Es], Win, C1, C2)
  when Type=:=menu;Type=:=submenu ->
    case proplists:get_value(crossmark, Ps) of
	true -> {WChk, _, _, _} = wxWindow:getTextExtent(Win, ?CHECK_MARK);
	_ -> WChk = 0
    end,
    {WStr, _, _, _} = wxWindow:getTextExtent(Win, Desc),
    {WHK, _, _, _} = wxWindow:getTextExtent(Win, get_hotkey(1,HK)),
    calc_min_sizes(Es, Win, max(WChk+WStr+5, C1), max(WHK+5, C2));
calc_min_sizes([#menu{}|Es], Win, C1, C2) ->
    calc_min_sizes(Es, Win, C1, C2);
calc_min_sizes([], _, C1, C2) ->
    {C1, C2}.

setup_popup([#menu{type=separator}|Es], Sizer, Sz, Cs, Parent, Magnet, Acc) ->
    Line = wxStaticLine:new(Parent, [{size,{-1,2}}]),
    wxSizer:addSpacer(Sizer, 4),
    wxSizer:add(Sizer, Line, [{border, 10}, {proportion, 0},
			      {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}]),
    wxSizer:addSpacer(Sizer, 4),
    setup_popup(Es, Sizer, Sz, Cs, Parent, Magnet, Acc);
setup_popup([#menu{type=submenu, wxid=Id, desc=Desc, help=Help0, opts=Ps, hk=HK}=ME|Es],
	    Sizer, Sz = {Sz1,Sz2}, Cs, Parent, Magnet, Acc) ->
    Panel = wxPanel:new(Parent, [{winid, Id}]),
    setup_colors([Panel], Cs),
    Line = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addSpacer(Line, 3),
    Controls =
	case get_hotkey(1,HK) of
	    [] ->
		wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc), [{proportion, 1},{flag, ?wxALIGN_CENTER}]),
		[Panel,T1];
	    HK0 ->
		wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, Desc), [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
		wxSizer:setItemMinSize(Line, T1, Sz1, -1),
		wxSizer:addSpacer(Line, 10),
		wxSizer:addStretchSpacer(Line),
		wxSizer:add(Line, T2 = wxStaticText:new(Panel, Id, HK0),  [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
		wxSizer:setItemMinSize(Line, T2, Sz2, -1),
		wxSizer:addSpacer(Line, 10),
		wxSizer:add(Line, 16, 16),
		[Panel,T1,T2]
	end,
    Help = if Help0 =:= [] -> Desc ++ ?__(1," submenu");
	      true -> Help0
	   end,
    {TipMsg, CmdMsg} = tooltip(Help, false, have_magnet(Ps, Magnet), HK),
    [wxWindow:setToolTip(Win, wxToolTip:new(TipMsg)) || Win <- Controls],
    wxPanel:setSizerAndFit(Panel, Line),
    wxSizer:add(Sizer, Panel, [{flag, ?wxEXPAND},{proportion, 1}]),
    menu_connect(Controls, [left_up, middle_up, right_up, enter_window]),
    setup_popup(Es, Sizer, Sz, Cs, Parent, Magnet, [ME#menu{help=CmdMsg}|Acc]);
setup_popup([#menu{type=menu, wxid=Id, desc=Desc, help=Help, opts=Props, hk=HK}=ME|Es],
	    Sizer, Sz = {Sz1,Sz2}, Cs, Parent, Magnet, Acc) ->
    Panel = wxPanel:new(Parent, [{winid, Id}]),
    setup_colors([Panel], Cs),
    Line  = wxBoxSizer:new(?wxHORIZONTAL),
    Checked = proplists:get_value(crossmark, Props) =:= true,
    wxSizer:addSpacer(Line, 3),
    ChkM = if Checked -> ?CHECK_MARK; true -> "" end,
    wxSizer:add(Line, T1 = wxStaticText:new(Panel, Id, ChkM++Desc),[{proportion, 0},{flag, ?wxALIGN_CENTER}]),
    wxSizer:setItemMinSize(Line, T1, Sz1, -1),
    wxSizer:addSpacer(Line, 10),
    wxSizer:addStretchSpacer(Line),
    wxSizer:add(Line, T2 = wxStaticText:new(Panel, Id, get_hotkey(1,HK)), [{proportion, 0},{flag, ?wxALIGN_CENTER}]),
    wxSizer:setItemMinSize(Line, T2, Sz2, -1),
    wxSizer:addSpacer(Line, 10),
    BM = case {OpBox = have_option_box(Props),have_color(Props)} of
	     {true,_} ->
		 Bitmap = get_pref_bitmap(),
                 SBM = case os:type() of
                           {_, darwin} ->
                               wxBitmapButton:new(Panel, Id+1, Bitmap,
                                                  [{style,?wxNO_BORDER}]);
                           _ ->
                               wxStaticBitmap:new(Panel, Id+1, Bitmap)
                       end,
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
    {TipMsg, CmdMsg} = tooltip(Help, OpBox, have_magnet(Props, Magnet), HK),
    [wxWindow:setToolTip(Win, wxToolTip:new(TipMsg)) || Win <- [Panel,T1,T2|BM]],
    wxPanel:setSizerAndFit(Panel, Line),
    wxSizer:add(Sizer, Panel, [{flag, ?wxEXPAND}, {proportion, 1}]),
    menu_connect([Panel,T1,T2|BM], [left_up, middle_up, right_up, enter_window]),
    Win = #{panel=>Panel, label=>T1, hotkey=>T2},
    Pop = ME#menu{wxid=Id, help=CmdMsg, object=Win},
    setup_popup(Es, Sizer, Sz, Cs, Parent, Magnet, [Pop|Acc]);
setup_popup([#menu{type=opt}=ME|Es], Sizer, Sz, Cs, Parent, Magnet, Acc) ->
    setup_popup(Es, Sizer, Sz, Cs, Parent, Magnet, [ME|Acc]);
setup_popup([], _, _, _, _, _, Acc) -> lists:reverse(Acc).

menu_connect(Windows, Evs) ->
    EvH = fun(Ev, _) -> wings_menu_process ! Ev end,
    [ [wxWindow:connect(Win, Ev, [{callback, EvH}]) || Ev <- Evs] || Win <- Windows].

get_pref_bitmap() ->
    case ?GET(small_pref_bm) of
	undefined ->
	    Images = wings_frame:get_icon_images(),
	    {_, _Sz, Img} = lists:keyfind(small_pref, 1, Images),
	    BM = wxBitmap:new(wxImage:copy(Img)),
	    ?SET(small_pref_bm, BM),
	    BM;
	BM ->
	    BM
    end.

create_color_box(Id, Panel, H, Props) ->
    {R,G,B} = wings_color:rgb3bv(proplists:get_value(color, Props)),
    Image = wxImage:new(1,1,<<R,G,B>>),
    wxImage:rescale(Image,10,H-2),
    Bitmap = wxBitmap:new(Image),
    SBM = wxStaticBitmap:new(Panel, Id+1, Bitmap),
    wxImage:destroy(Image),
    wxBitmap:destroy(Bitmap),
    SBM.

entry_msg(Id, Entries) ->
    #menu{help=Msg} = lists:keyfind(Id, 2, Entries),
    Msg.

entry_cmd(Id, Entries) ->
    #menu{name=Cmd} = lists:keyfind(Id, 2, Entries),
    Cmd.
entry_wins(#menu{object=Win}) ->
    Win.

set_entry_id(Id, ME) ->
    ME#menu{wxid=Id}.

setup_colors(Windows, {BG, FG}) ->
    setup_colors(Windows, BG, FG).

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

tooltip("", false, false, _) -> {"",""};
tooltip(Help, OptBox, Magnet, HK) when is_list(Help) ->
    tooltip(Help, "", opt_help(OptBox), Magnet, HK);
tooltip({Help}, OptBox, Magnet, HK) ->
    tooltip(Help, "", opt_help(OptBox), Magnet, HK);
tooltip({HelpL, HelpM}, OptBox, Magnet, HK) ->
    tooltip(HelpL, HelpM, opt_help(OptBox), Magnet, HK);
tooltip({HelpL, HelpM, ""}, OptBox, Magnet, HK) ->
    tooltip(HelpL, HelpM, opt_help(OptBox), Magnet, HK);
tooltip({HelpL, HelpM, HelpR}, _, Magnet, HK) ->
    tooltip(HelpL, HelpM, HelpR, Magnet, HK).

tooltip("", "", "", Magnet, _) ->
    Str = magnet_help(str, Magnet),
    {Str, Str};
tooltip(HelpL, "", "", Magnet, _) ->
    {str_clean(HelpL) ++ magnet_help(tip, Magnet),
     wings_msg:join(HelpL,magnet_help(str, Magnet))};
tooltip(HelpL, HelpM, "", Magnet, HK) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~ts~n" ++
		       ?__(2, "Middle mouse button") ++ ": ~ts~ts",
		   [HelpL, tooltip_hk(1,HK), HelpM, tooltip_hk(2,HK)]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, HelpM, ""),magnet_help(str, Magnet))};
tooltip(HelpL, "", HelpR, Magnet, HK) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~ts~n" ++
		       ?__(3, "Right mouse button") ++ ": ~ts~ts",
		   [HelpL, tooltip_hk(1,HK), HelpR, tooltip_hk(3,HK)]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, "", HelpR), magnet_help(str, Magnet))};
tooltip(HelpL, HelpM, HelpR, Magnet, HK) ->
    {io_lib:format(?__(1, "Left mouse button") ++ ": ~ts~ts~n" ++
		       ?__(2, "Middle mouse button") ++ ": ~ts~ts~n" ++
		       ?__(3, "Right mouse button") ++ ": ~ts~ts",
		   [HelpL, tooltip_hk(1,HK), HelpM, tooltip_hk(2,HK), HelpR, tooltip_hk(3,HK)]) ++ magnet_help(tip, Magnet),
     wings_msg:join(wings_msg:button_format(HelpL, HelpM, HelpR), magnet_help(str, Magnet))}.

tooltip_hk(Mb, HK) ->
    case get_hotkey(Mb, HK) of
	[] -> "";
	HKStr -> io_lib:format("  | ~ts", [HKStr])
    end.

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
update_menu(Menu, Item, delete, _) ->
    case menu_item_id(Menu, Item) of
	false -> ok;
	Id ->
	    case ets:lookup(wings_menus, Id) of
		[#menu{type=submenu}=SubMenu] ->
		    remove_submenu(SubMenu),
		    ok;
		[#menu{object=MenuItem}] ->
		    ParentMenu = wxMenuItem:getMenu(MenuItem),
		    true = wxMenu:delete(ParentMenu, Id),
		    ets:delete(wings_menus, Id),
		    ok;
		_ ->
		    ok
	    end
    end;
update_menu(Menu, Item, {append, Pos0, Cmd0}, Help) ->
    update_menu(Menu, Item, {append, Pos0, Cmd0, []}, Help);
update_menu(Menu, Item, {append, Pos0, Cmd0, Props}, Help) ->
    case menu_item_id(Menu, Item) of
	false ->
	    AddItem =
		fun(SubMenu, Name) ->
		    Pos =
			if Pos0 >= 0 -> min(wxMenu:getMenuItemCount(SubMenu), Pos0);
			    true -> wxMenu:getMenuItemCount(SubMenu)
			end,
		    {Type,Check} = case proplists:get_value(crossmark, Props) of
				       undefined -> {?wxITEM_NORMAL, false};
				       false -> {?wxITEM_CHECK, false};
				       _ -> {?wxITEM_CHECK, true} %% grey or true
				   end,
		    MO = wxMenu:insert(SubMenu, Pos, -1, [{text, Cmd0},{kind, Type}]),
		    Id = wxMenuItem:getId(MO),
		    ME=#menu{name=Name, object=MO,
			     wxid=Id, type=Type},
		    true = ets:insert(wings_menus, ME),
		    Cmd = setup_hotkey(MO, Cmd0),
		    wxMenuItem:setText(MO, Cmd),
		    (Type==?wxITEM_CHECK) andalso wxMenuItem:check(MO,[{check, Check}]),
		    is_list(Help) andalso wxMenuItem:setHelp(MO, Help)
		end,

	    Names0 = build_names(Item, [Menu]),
	    Names = lists:droplast(Names0),
	    case ets:match_object(wings_menus, #menu{name=Names, _ = '_'}) of
		[#menu{object=SubMenu}] ->
		    AddItem(SubMenu, build_command(Item, [Menu]));
		_ ->
		    io:format("update_menu: Item rejected (~p|~p)\n",[Menu,Item])
	    end;
	_ ->
	    ok
    end;
update_menu(Menu, Item, Cmd0, Help) ->
    Id = menu_item_id(Menu, Item),
    MI = case ets:lookup(wings_menus, Id) of
	     [#menu{object=MO}] ->
		 MO;
	     [] when Menu =:= file, element(1, Item) =:= load_pref ->
		 Names0 = build_names(Item, [Menu]),
		 Names = lists:droplast(Names0),
		 [#menu{object=CustomTheme, type=submenu}] = ets:match_object(wings_menus, #menu{name=Names, _ = '_'}),
		 N  = wxMenu:getMenuItemCount(CustomTheme),
		 MO = wxMenu:insert(CustomTheme, N, ?wxITEM_NORMAL, [{text, Cmd0}]),
		 ME=#menu{name=build_command(Item,[Menu]), object=MO,
			  wxid=Id, type=?wxITEM_NORMAL},
		 true = ets:insert(wings_menus, ME),
		 wxMenu:insertSeparator(CustomTheme, N),
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

parent_menu(This) when is_list(This) ->
    Parent = lists:droplast(This),
    [#menu{}=PMenu] = ets:match_object(wings_menus, #menu{name=Parent,type=submenu, _='_'}),
    PMenu.

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
		[] ->  %% find for submenu root (level 0)
                    MenuPath = build_names(Item, [Menu]),
                    case ets:match_object(wings_menus, #menu{name=MenuPath,type=submenu, _='_'}) of
                        [#menu{wxid=Id}] -> Id;
                        _ -> false
                    end
            end
    end.

remove_submenu(#menu{object=SubMenu, name=Name, wxid=SubId}) ->
    #menu{object=ParentMenu} = parent_menu(Name),
    wxMenu:delete(ParentMenu, SubId),
    ets:delete(wings_menus, SubId),
    wxMenu:destroy(SubMenu).

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

setup_menu(Names, Id, Menus) ->
    Menu   = wxMenu:new(),
    Entries = make_entries(Names, Menus, wx),
    Next  = create_menu(Entries, Id, Names, Menu),
    {Menu, Next}.

make_entries(Names, Menus0, Style) when is_list(Menus0) ->
    Menus1  = wings_plugin:menu(list_to_tuple(reverse(Names)), Menus0),
    HotKeys = wings_hotkey:matching(Names),
    Menus2 = [normalize_menu_wx(Entry, HotKeys, Names) || Entry <- lists:flatten(Menus1)],
    format_hotkeys(Menus2, Style).

normalize_menu_wx(separator, _, _) ->
    #menu{type=separator};
normalize_menu_wx({S,Fun,Help,Ps}, Hotkeys, Ns) when is_function(Fun) ->
    HK = case proplists:get_value(hotkey, Ps) of
	     undefined -> match_hotkey(reduce_name([Fun(1, Ns),Fun(2, Ns),Fun(3, Ns)]), Hotkeys, have_option_box(Ps));
	     String -> String
	 end,
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
    HK = case proplists:get_value(hotkey, Ps) of
	     undefined -> match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps));
	     String -> String
	 end,
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
    HK = case proplists:get_value(hotkey, Ps) of
	     undefined -> match_hotkey(reduce_name(Name), Hotkeys, have_option_box(Ps));
	     String -> String
	 end,
    #menu{desc=S,name=Name,opts=Ps,hk=HK}.

name_for_hotkey(Name, Ns, Fun) when is_function(Fun) ->
    case Fun(1, Ns) of
	SM when is_tuple(SM) -> [SM, Fun(2, Ns), Fun(3, Ns)];
	_ -> Name
    end;
name_for_hotkey(Name, _, _) -> Name.

get_hotkey(1,{HK1, _, _}) -> HK1;
get_hotkey(2,{_, HK2, _}) -> HK2;
get_hotkey(3,{_, _, HK3}) -> HK3;
get_hotkey(1,HK) -> HK;
get_hotkey(_,_) -> "".

format_hotkeys([#menu{type=separator}=H|T], Style) ->
    [H|format_hotkeys(T, Style)];
format_hotkeys([#menu{hk=[HK1,HK2,HK3]}=H|T], Style) ->
    Hotkey1 = wings_hotkey:format_hotkey(HK1, Style),
    Hotkey2 = wings_hotkey:format_hotkey(HK2, Style),
    Hotkey3 = wings_hotkey:format_hotkey(HK3, Style),
    [H#menu{hk={Hotkey1, Hotkey2, Hotkey3}}|format_hotkeys(T, Style)];
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
    wxMenu:append(Menu, NextId, Desc, SMenu, [{help, Help}]),
    ME=ME0#menu{name=lists:reverse([Name|Names]), object=SMenu, wxid=NextId},
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

menu_item_desc(Desc, {[],[],[]}) -> Desc;
menu_item_desc(Desc, []) -> Desc;
menu_item_desc(Desc, HotKey) ->
    %% Quote to avoid Windows stealing keys.
    case os:type() of
	{win32, _} -> Desc ++ "\t'" ++ HotKey ++ "'";
	_ -> Desc ++ "\t" ++ HotKey
    end.

menu_sel_cleanup(_, []) -> ok;
menu_sel_cleanup({BG,FG}=Col, [#menu{type=submenu, object=undefined, wxid=Id}|Menu]) ->
    Panel = wxWindow:findWindowById(Id),
    Set = fun() ->
            setup_colors([Panel|wxWindow:getChildren(Panel)], BG, FG)
        end,
    wx:batch(Set),
    menu_sel_cleanup(Col,Menu);
menu_sel_cleanup({BG,FG}=Col, [#menu{type=menu, object=Obj}|Menu]) ->
    Panel = maps:get(panel, Obj),
    Set = fun() ->
            setup_colors(Panel, BG, FG)
        end,
    wx:batch(Set),
    menu_sel_cleanup(Col,Menu);
menu_sel_cleanup(Col, [_|Menu]) ->
    menu_sel_cleanup(Col,Menu).

%% We want to use the predefined id where they exist (mac) needs for it's
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

colorB(Pref) when is_atom(Pref) ->
    wings_color:rgb4bv(wings_pref:get_value(Pref)).

