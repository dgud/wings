%%
%%  wings_tweak_win.erl --
%%
%%     Tweak tools windows.
%%
%%  Copyright (c) 2016 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_tweak_win).
-export([window/2,window/5]).

-export([init/1,
	 handle_call/3, handle_cast/2,
	 handle_event/2, %% handle_sync_event/3,
	 handle_info/2, code_change/3, terminate/2
	]).

-include("wings.hrl").

%%%
%%% Geometry Graph window.
%%%

window(Name, St) ->
    case wings_wm:is_window({tweak,Name}) of
	true ->
	    wings_wm:raise({tweak,Name}),
	    keep;
	false ->
	    Pos = case Name of
		      tweak_palette   -> {5,150};
		      tweak_magnet    -> {25,170};
		      axis_constraint -> {45,190}
		  end,
	    wings_tweak_win:window(Name, Pos, {-1,-1}, [], St),
	    keep
    end.

window(Name0, Pos, Size, Ps0, St) ->
    Name = {tweak, Name0},
    State = get_state(Name),
    {Frame,Ps} = wings_frame:make_win(title(Name), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Name, State]),
    wings_wm:toplevel(Name, Window, Ps, {push,change_state(Window, St)}),
    keep.

%%%%%%%% Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event(init_opengl, _Window, _St) -> keep;
forward_event({apply, ReturnSt, Fun}, Window, St0) ->
    %% Apply ops from window in wings process
    case ReturnSt of
	true ->
	    St = Fun(St0),
	    {replace, change_state(Window, St)};
	false ->
	    Fun(St0)
    end;
forward_event(update_palette, Window, _) ->
    wx_object:cast(Window, {update, get_state()}),
    keep;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.

get_state() ->
    get_state(wings_wm:this()).
get_state({_,tweak_palette}) ->
    {tweak_tool(1, {false, false, false}), valid_menu_items(wings_tweak:menu())};
get_state({_,tweak_magnet}) ->
    {wings_pref:get_value(tweak_magnet),valid_menu_items(wings_tweak:tweak_magnet_menu())};
get_state({_,axis_constraint}) ->
    {none,valid_menu_items(wings_tweak:constraints_menu())}.

tweak_tool(Button, Modifiers) ->
    TweakKeys = wings_tweak:tweak_keys(),
    case orddict:find({Button,Modifiers},TweakKeys) of
	{ok,Mode} -> Mode;
	error -> none
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {me, name, shown, mode, menu, prev, cols}).

init([Frame, Name, {Mode, Menus}]) ->
    Panel = wxPanel:new(Frame, [{style, ?wxBORDER_SIMPLE}]),
    HotKeys = wings_hotkey:matching([tweak]),
    Entries0 = [wings_menu:normalize_menu_wx(Entry, HotKeys, [tweak])
		|| Entry <- lists:flatten(Menus)],
    Entries1 = wings_menu:format_hotkeys(Entries0, pretty),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    #{bg:=BG, text:=FG} = Cols = wings_frame:get_colors(),
    wxWindow:setBackgroundColour(Panel, BG),
    Main = wxBoxSizer:new(?wxHORIZONTAL),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    MinHSzs = wings_menu:calc_min_sizes(Entries1, Panel, 5, 5),
    {Entries2,_} = lists:mapfoldl(fun(ME, Id) -> {wings_menu:set_entry_id(Id, ME), Id+1} end,
				  500, Entries1),
    Entries = wings_menu:setup_popup(Entries2, Sizer, MinHSzs, {BG,FG}, Panel, false, []),
    update_lines(Menus, Entries, Cols),
    wxSizer:setMinSize(Sizer, 225, -1),
    wxSizer:addSpacer(Main, 5),
    wxSizer:add(Main, Sizer, [{proportion, 1}, {border, 5}, {flag, ?wxALL}]),
    wxSizer:addSpacer(Main, 5),
    wxPanel:setSizer(Panel, Main),
    wxPanel:connect(Panel, enter_window),
    wxSizer:fit(Main, Panel),
    ?GET(top_frame) =:= Frame orelse wxSizer:setSizeHints(Main, Frame),
    {Panel, #state{me=Panel, name=Name, shown=Entries, cols=Cols, mode=Mode, menu=Menus}}.

handle_event(#wx{id=Id, obj=Obj, event=#wxMouse{type=enter_window}}=Ev,
	     #state{me=Me, name=Name, shown=Entries, prev=_Prev} = State) ->
    case wings_util:wxequal(Obj, Me) of
	true ->  wings_status:message(Name, "");
	false -> wings_status:message(Name, wings_menu:entry_msg(Id, Entries))
    end,
    wings_frame ! Ev#wx{userData={win, Me}},
    {noreply, State#state{prev=line}};
handle_event(#wx{id=Id, event=#wxMouse{}=ME},
	     #state{name=Name, shown=Entries} = State) ->
    Cmd = case wings_menu:entry_cmd(Id, Entries) of
	      {_, {Mode, _}} -> tweak_mode_cmd(Mode, ME);
	      Command -> Command
	  end,
    Do = fun(St) -> wings_tweak:command({element(2,Name),Cmd}, St) end,
    wings_wm:psend(Name, {apply, true, Do}),
    {noreply, State};
handle_event(#wx{} = _Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(_Req, _From, State) ->
    %% io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, Req]),
    {reply, ok, State}.

handle_cast({update, {_Mode, Menus}}, #state{cols=Cs, shown=Entries0} = State) ->
    update_lines(Menus, Entries0, Cs),
    {noreply, State};
handle_cast(_Req, State) ->
    %% io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, _Req]),
    {noreply, State}.

handle_info(parent_changed, State) ->
    {noreply, State};
handle_info(_Msg, State) ->
    %% io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{name=Name}) ->
    %% io:format("terminate: ~p:~p (~p)~n",[?MODULE, Name, _Reason]),
    wings ! {wm, {delete, Name}},
    normal.

%%%%%%%%%%%%%%%%%%%%%%

update_lines([separator|Ms], Es, Cs) ->
    update_lines(Ms, Es, Cs);
update_lines([{Name,_Cmd,_,Os}|Ms], [Entry|Es], Cs) ->
    #{label:=T1, hotkey:=T2, panel:=Panel} = wings_menu:entry_wins(Entry),
    wxStaticText:setLabel(T1, Name),
    case proplists:get_value(hotkey, Os) of
    	undefined -> wxStaticText:setLabel(T2, "");
    	String    -> wxStaticText:setLabel(T2, String)
    end,
    case proplists:get_value(crossmark, Os, false) of
	true  -> wings_menu:setup_colors(Panel, maps:get(hl_bg, Cs), maps:get(hl_text, Cs));
	false -> wings_menu:setup_colors(Panel, maps:get(bg, Cs), maps:get(text, Cs))
    end,
    update_lines(Ms, Es, Cs);
update_lines([], [], _) ->
    ok.

title({_,tweak_palette}) ->
    ?__(1,"Tweak");
title({_,tweak_magnet}) ->
    ?__(2,"Tweak Magnet");
title({_,axis_constraint}) ->
    ?__(3,"Tweak Axis").

%% Get valid items form the tweak menu for the palettes
valid_menu_items([separator,{_,cycle_magnet,_}|Menu]) ->
    valid_menu_items(Menu);
valid_menu_items([separator,{_,{tweak_magnet,_}},{_,{axis_constraint,_}}|Menu]) ->
    valid_menu_items(Menu);
valid_menu_items([{_,mag_adjust,_}|Menu]) ->
    valid_menu_items(Menu);
valid_menu_items([I|Menu]) ->
    case I of
	{Name,Cmd,Help,Bound} when is_function(Cmd) -> %% menu format
	    C = Cmd(1,[]), %% change fun to cmd name.. 1 is for lmb
	    [{Name,C,Help,Bound}|valid_menu_items(Menu)];
	{Name,Cmd,Help,Bound} when is_atom(Cmd) -> %% menu format
	    [{Name,Cmd,Help,Bound}|valid_menu_items(Menu)];
	{Name, Cmd, Help} when is_atom(Cmd) ->
	    [{Name,Cmd,Help,[]}|valid_menu_items(Menu)];
	separator ->
	    [separator|valid_menu_items(Menu)]
    end;
valid_menu_items([]) -> [].

tweak_mode_cmd(Mode, #wxMouse{type=Type, controlDown=Ctrl,shiftDown=Shift,altDown=Alt}) ->
    B = case Type of
	    left_up -> 1;
	    middle_up -> 2;
	    right_up -> 3
	end,
    {set_tweak_pref, Mode, B, {Ctrl, Shift, Alt}}.

