%%
%%  wings_frame.erl --
%%
%%     The top frame and window manager of Wings 3D.
%%
%%  Copyright (c) 2015 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_frame).

-export([make_win/2, register_win/3, close/1, set_focus/1,set_title/2,
         get_top_frame/0,
         show_toolbar/1,
	 export_layout/0, import_layout/2, reinit_layout/0,
	 get_overlay/0, overlay_draw/3, overlay_hide/1,
	 get_icon_images/0, get_colors/0, get_border/0, update_theme/0]).

-export([start_link/0, forward_event/1]).

%% Internal
-behaviour(wx_object).
-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2]).

-define(NEED_ESDL, 1). %% event mapping
-include("wings.hrl").

-define(IS_GEOM(Name),
	((Name =:= geom)
	 orelse (is_tuple(Name) andalso element(1, Name) =:= geom)
	 orelse (is_tuple(Name) andalso element(1, Name) =:= autouv))).

-define(IS_SPLIT(WinProp), (element(1, WinProp) =:= split
			    orelse element(1, WinProp) =:= split_rev)).

-ifndef(wxID_OSX_HIDE).
-define(wxID_OSX_HIDE, 5250).
-endif.
-ifndef(wxID_OSX_HIDEOTHERS).
-define(wxID_OSX_HIDEOTHERS, 5251).
-endif.
-ifndef(wxID_OSX_SHOWALL).
-define(wxID_OSX_SHOWALL, 5252).
-endif.
-ifndef(wxID_OSX_MENU_LAST).
-define(wxID_OSX_MENU_LAST, 5255).
-endif.

-define(GL_WAIT, 200).

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Frame = wx_object:start_link({local, ?MODULE}, ?MODULE, [args], []),
    ?SET(top_frame, Frame),
    {ok, wx_object:get_pid(Frame)}.

top_menus(WorkAround) ->
    Tail0 = [{?__(7,"Help"),help,wings_help:menu()}],
    Tail = case wings_pref:get_value(show_develop_menu) of
	       true ->
		   [{"Develop",develop,wings_develop:menu()}|Tail0];
	       false ->
		   Tail0
	   end,
    WinStr = case WorkAround of
                 true -> ?__(16,"Windows"); %% Temporary
                 false -> ?__(6,"Window")
             end,
    [{?__(1,"File"),  file,wings_file:menu()},
     {?__(2,"Edit"),  edit,wings:edit_menu()},
     {?__(3,"View"),  view,wings_view:menu()},
     {?__(4,"Select"),select,wings_sel_cmd:menu()},
     {?__(5,"Tools"), tools, wings:tools_menu()},
     {WinStr,window,wings:window_menu()}
     |Tail].

make_win(Title, Opts) ->
    wx_object:call(?MODULE, {make_win, [{title, Title}|Opts]}).

register_win(Window, Name, Ps) ->
    wx_object:call(?MODULE, {register_window, Window, Name, Ps}).

reset_layout() ->
    {Contained, Free} = wx_object:call(?MODULE, get_windows),
    Delete = fun(Name) ->
		     wings_wm:delete(Name),     %% Autouv needs to be reset...
		     case ?IS_GEOM(Name) of
			 true  -> ignore;
			 false -> receive {wm, {delete, Name}} -> ok end
		     end
	     end,
    [Delete(Name) || {Name, _, _, _} <- Contained ++ Free, Name =/= geom],
    ok.

export_layout() ->
    {Contained0, Free0} = wx_object:call(?MODULE, get_windows),
    AddProps = fun({_, _, _}=Split) -> Split;
		  ({Win, Pos, Size, []}) -> {Win, Pos, Size, window_prop(Win)}
	       end,
    Contained = filter_contained(Contained0, AddProps, []),
    Free = [AddProps(Win) || Win <- Free0, save_window(element(1,Win))],
    {Contained, Free}.

reinit_layout() ->
    {Contained, Free} = wx_object:call(?MODULE, get_windows),
    Delete = fun(Name) ->
	wings_wm:delete(Name),
	case ?IS_GEOM(Name) of
	    true  -> ignore;
	    false -> receive {wm, {delete, Name}} -> ok end
	end
	     end,
    [Delete(Win) || {Win, _, _, _} <- Contained ++ Free, not save_window(Win)],
    ok.

get_icon_images() ->
    wx_object:call(?MODULE, get_images).

get_colors() ->
    #{bg   => wings_color:rgb4bv(wings_pref:get_value(outliner_geograph_bg)),
      text => wings_color:rgb4bv(wings_pref:get_value(outliner_geograph_text)),
      hl_bg   => wings_color:rgb4bv(wings_pref:get_value(outliner_geograph_hl)),
      hl_text => wings_color:rgb4bv(wings_pref:get_value(outliner_geograph_hl_text))
     }.

get_border() ->
    %% Alternatives are:
    %%  ?wxBORDER_DEFAULT ?wxBORDER_THEME
    %%  ?wxBORDER_SUNKEN ?wxBORDER_RAISED
    %%  ?wxBORDER_STATIC ?wxBORDER_SIMPLE
    %%  ?wxBORDER_NONE
    case os:type() of
        {win32, _}  -> ?wxBORDER_NONE;
        {_, darwin} -> ?wxBORDER_NONE;
        _ -> ?wxBORDER_NONE
    end.

update_theme() ->
    wx_object:call(?MODULE, update_theme).

close(Win) ->
    case wx_object:call(?MODULE, {close, Win}) of
        {ignored, _} ->
            ok;
        {closed, NeedReset} ->
            reset_opengl(NeedReset),
            ok
    end.


set_focus(Win) ->
    wx_object:cast(?MODULE, {active, Win}).

set_title(Win, Title) ->
    wx_object:cast(?MODULE, {set_title, Win, Title}).

get_overlay() ->
    wx_object:call(?MODULE, get_overlay).

show_toolbar(Show) ->
    wx_object:call(?MODULE, {show_toolbar, Show}).

get_top_frame() ->
    wx_object:call(?MODULE, get_top_frame).

need_gl_reset() ->
    wx_object:call(?MODULE, need_gl_reset).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_pos({X,Y}=Pos, {Sx, Sy}, {DW, DH}) when X > 0, Y > 0 ->
    if (X+Sx) < DW, (Y+Sy) < DH -> Pos;
       true -> {-1, -1}
    end;
validate_pos(_, _, _) -> {-1, -1}.

import_layout({Contained, Free}, St) ->
    reset_layout(),
    Contained =/= [] andalso imp_layout(Contained, [], undefined, St),
    wx_object:call(?MODULE, {update_layout,Contained}),
    _ = [restore_window(Win, St) || Win <- Free],
    reset_opengl(need_gl_reset()),
    ok.

%% Remove "don't save" windows and the corresponding Splitter
filter_contained([Win|Wins], AddProps, Acc) ->
    case {save_window(element(1, Win)), Acc} of
	{true, _} ->
	    filter_contained(Wins, AddProps, [AddProps(Win)|Acc]);
	{false, [Split|Rest]} when ?IS_SPLIT(Split) ->
	    filter_contained(Wins, AddProps, Rest);
	{false, [First,Split|Rest]} when ?IS_SPLIT(Split) ->
	    filter_contained(Wins, AddProps, [First|Rest])
    end;
filter_contained([], _, Acc) -> lists:reverse(Acc).

save_window(console) -> true;
save_window(palette) -> true;
save_window({tweak, _}) -> true;
save_window(outliner) -> true;
save_window(wings_outliner) -> true;
save_window(wings_view_win) -> true;
save_window({object,_}) -> true;
save_window(geom) -> true;
save_window({geom,_}) -> true;
save_window({plugin,_}) -> true;
save_window(split) -> true;
save_window(split_rev) -> true;
save_window(_) -> false.

window_prop(Geom) when ?IS_GEOM(Geom) ->
    Ps = wings_wm:get_props(Geom),
    lists:foldl(fun save_geom_props/2, [], Ps);
window_prop({plugin, _}=Name) ->
    case wings_plugin:get_win_data(Name) of
	{M, {_,CtmData}} when is_list(CtmData) ->
	    [{module, M}|CtmData];
	_ -> []
    end;
window_prop(_) -> [].

save_geom_props({show_axes,_}=P, Acc) -> [P|Acc];
save_geom_props({show_cam_imageplane,_}=P, Acc) -> [P|Acc];
save_geom_props({show_groundplane,_}=P, Acc) -> [P|Acc];
save_geom_props({show_info_text,_}=P, Acc) -> [P|Acc];
save_geom_props({current_view,View}, Acc) ->
    #view{fov=Fov,hither=Hither,yon=Yon} = View,
    [{fov,Fov},{clipping_planes,{Hither,Yon}}|Acc];
save_geom_props(_, Acc) -> Acc.

imp_layout([{split, Mode, Pos}|Rest], Path, Split0, St) ->
    Cont0 = imp_layout(Rest, Path, Split0, St),
    imp_layout(Cont0, [second|Path], {Mode,Pos}, St);
imp_layout([{split_rev, Mode, Pos}|Rest], Path, Split0, St) ->
    Cont0 = imp_layout(Rest, Path, Split0, St),
    imp_layout(Cont0, [first|Path], {Mode, Pos}, St);

imp_layout([{geom, _, _, Ps}|Rest], _, _, St) ->
    restore_window({geom, {50,50}, {50,40}, Ps}, St),
    Rest;
imp_layout([{Win, _, _, Ps}|Rest], [Last|Path], {Mode, Pos}, St) ->
    Restore = {lists:reverse([split(Mode,Last)|Path]), Pos},
    restore_window({Win, {50,50}, {50,40}, [{internal, Restore}|Ps]}, St),
    Rest.

restore_window({geom, _Pos, _Size, Ps}, _St) ->
    wings_wm:set_win_props(geom, [{tweak_draw,true}|Ps]);
restore_window({Geom, Pos, Size, Ps}, St)
  when ?IS_GEOM(Geom) ->
    wings:new_viewer(Geom, Pos, Size, [{tweak_draw,true}|Ps], St);
restore_window({Name,Pos,Size}, St) -> % OldFormat
    restore_window({Name,Pos,Size,[]}, St);
restore_window({{object,_}=Name,Pos,{_,_}=Size,Ps}, St) ->
    wings_geom_win:window(Name, Pos, Size, Ps, St);
restore_window({wings_outliner,Pos,{_,_}=Size, Ps}, St) ->
    wings_outliner:window(Pos, Size, Ps, St);
restore_window({wings_view_win,Pos,{_,_}=Size, Ps}, St) ->
    wings_view_win:window(Pos, Size, Ps, St);
restore_window({palette,Pos,{_,_}=Size, Ps}, St) ->
    wings_palette:window(Pos, Size, Ps, St);
restore_window({console,Pos,{_,_}=Size, Ps}, _St) ->
    wings_console:window(console, Pos, Size, Ps);
restore_window({{tweak, Win},Pos, Size, Ps}, St) ->
    wings_tweak_win:window(Win, Pos, Size, Ps, St);
restore_window({{plugin,_}=Name,Pos,{_,_}=Size,CtmData0}, St) ->
    Module = proplists:get_value(module, CtmData0),
    CtmData = proplists:delete(module, CtmData0),
    wings_plugin:restore_window(Module, Name, Pos, Size, CtmData, St);
restore_window({Module,{{plugin,_}=Name,Pos,{_,_}=Size,CtmData}}, St) -> %% Old plugin format
    wings_plugin:restore_window(Module, Name, Pos, Size, CtmData, St);
restore_window(_, _) -> ok.


%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forward_event({current_state, #st{selmode=Mode, sh=Sh}}) ->
    wx_object:cast(?MODULE, {selmode, geom, Mode, Sh}),
    keep;
forward_event({current_state, Win, #st{selmode=Mode, sh=Sh}}) ->
    wx_object:cast(?MODULE, {selmode, Win, Mode, Sh}),
    keep;
forward_event({mode_restriction, _}=Restrict) ->
    wx_object:cast(?MODULE, Restrict),
    keep;
forward_event({menu, _}=Menu) ->
    wx_object:cast(?MODULE, Menu),
    keep;
forward_event({got_focus, _, _}=Focus) ->
    wx_object:cast(?MODULE, Focus),
    keep;
forward_event(_Ev) ->
    %% io:format("Dropped ~P~n", [_Ev, 20]),
    keep.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {toolbar, windows, overlay, images, active, wip=[]}).
-record(split, {obj, mode, w1, w2}).
-record(win, {frame, win, name, title, bar, ps}).

init(_Opts) ->
    try
	process_flag(trap_exit, true),
	wings_pref:set_default(window_size, {780,570}),
	TopSize = wings_pref:get_value(window_size),
	Frame0 = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),
        Frame = wx_object:set_pid(Frame0, self()),

	IconImgs = make_icons(),
	set_icon(Frame),
	Sizer = wxBoxSizer:new(?wxVERTICAL),

	Top = make(Frame),
	Canvas = make_splash(wxPanel:new(win(Top)), IconImgs),
	wxSizer:add(Sizer, win(Top), [{proportion, 1}, {border, 0},
                                      {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}]),
	wxSplitterWindow:initialize(win(Top), Canvas),
	Toolbar = wings_toolbar:init(Frame, IconImgs),
	wxSizer:setSizeHints(Sizer, win(Top)),
	wxFrame:setSizer(Frame, Sizer),

	wxWindow:connect(Frame, close_window, [{callback, fun terminate_frame/2}]),
	wxWindow:connect(Frame, command_menu_selected, []),
	wxWindow:connect(Frame, activate, [{skip, true}]),
        init_menubar(Frame),
        wxFrame:show(Frame),
	Wins0 = #{frame=>Frame, ch=>Top#split{w1=Canvas}, szr=>Sizer,
                  loose=>#{}, action=>undefined, op=>undefined},
	Overlay = make_overlay(Frame),
        %% Init OpenGL from wings process
        Opts = [top, {title, wings:geom_title(geom)}],
        {{Win, Info}, Wins} = create_internal_win(undefined, Opts, Wins0),
        wings ! {frame_created, Frame, Win, Info},
        receive opengl_initialized -> ok end,
	{Frame, #state{toolbar=Toolbar, images=IconImgs, windows=Wins, overlay=Overlay}}
    catch _:Reason:ST ->
	    io:format("CRASH: ~p ~p ~p~n",[?MODULE, Reason, ST])
    end.

make_splash(Canvas, Imgs) ->
    BG = wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOWFRAME),
    case os:type() of %% Workaround black panel color on gtk and wxWidgets-3.1.3
        {_, linux} -> wxWindow:setBackgroundColour(Canvas, BG);
        _ -> ok
    end,
    Szr = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addStretchSpacer(Szr),
    {Splash, _} = wings_help:about_panel(Canvas,Imgs),
    wxSizer:add(Szr, Splash, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(Szr),
    wxPanel:setSizer(Canvas, Szr),
    Canvas.

%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{obj=Obj, event=#wxMove{}}, #state{windows=#{loose:=Loose}} = State) ->
    MS = wx_misc:getMouseState(),
    {X,Y} = wx_misc:getMousePosition(),
    Find = fun(#win{frame=Frame, name=Name}) ->
                   wings_util:wxequal(Frame, Obj) andalso save_window(Name)
           end,
    case lists:any(Find, maps:values(Loose)) of
        true ->
            {noreply, preview_attach(stopped_moving(MS), {X,Y}, Obj, State)};
        false ->
            {noreply, State}
    end;

handle_event(#wx{userData={move,Win}, event=Ev}, #state{windows=Wins0} = State) ->
    Wins = detach_window(Ev, Win, Wins0),
    {noreply, State#state{windows=Wins}};

handle_event(#wx{id=OSX, event=#wxCommand{type=command_menu_selected}},
	     #state{windows=#{frame:=Frame}}=State) when
      ?wxID_OSX_HIDE =< OSX, OSX =< ?wxID_OSX_MENU_LAST ->
    case OSX of
	?wxID_OSX_HIDE -> wxFrame:iconize(Frame);
	_ -> ignore
    end,
    {noreply, State};
handle_event(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}, State) ->
    Name = wings_menu:id_to_name(Id),
    ME = case ets:match(wings_state, {{bindkey,'$1'}, Name, '_'}) of
	     [] -> {menubar, {action, Name}};
	     [[KeyComb]|_] -> wings_io_wx:make_key_event(KeyComb)
	 end,
    wings ! ME,
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=enter_window}, userData={win, Obj}},
	     #state{windows=#{ch:=Root, loose:=Loose}, active=Active} = State) ->
    case find_win(Obj, Root) of
	false ->
	    case [Win || Win <- maps:values(Loose), wings_util:wxequal(Win#win.win, Obj)] of
		[#win{name=Name}] when not ?IS_GEOM(Name) ->
		    wings ! {wm, {active, Name}},
		    wings_status:active(Name);
		_ ->
		    ignore
	    end,
	    {noreply, update_active(undefined, State)};
	#win{name=Active} -> %% Already active
	    {noreply, State};
	#win{name=Name} when ?IS_GEOM(Name) ->
	    {noreply, State}; % handled by wings_wm
	#win{name=Name} ->
	    wings ! {wm, {active, Name}},
	    wings_status:active(Name),
	    {noreply, update_active(Name, State)}
    end;

handle_event(#wx{event=#wxActivate{active=Active}}=Ev, State) ->
    %% io:format("Active ~p~n",[Active]),
    wings ! Ev,
    case Active of
	false -> {noreply, update_active(undefined, State)};
	true  -> {noreply, State}
    end;

handle_event(#wx{obj=Obj, event=#wxClose{}}, #state{windows=Wins}=State) ->
    case Wins of
	#{loose:=#{Obj:=#win{name=Name, win=Win}}=Loose} ->
	    try wx_object:get_pid(Win) of
		_Pid  ->
		    wxWindow:destroy(Obj),
		    {noreply, State#state{windows=Wins#{loose:=maps:remove(Obj, Loose)}}}
	    catch _:_ ->
		    wings_wm:psend(Name, close),
		    {noreply, State}
	    end;
	_ ->
	    ?dbg("\nUnexpected window closing\n\tObj: ~p\n\tWins: ~p\n",[Obj,Wins]),
	    wings_u:message("Unexpected window closing.\n" ++
			    "Please check the log window and report the information there."),
	    {noreply, State}
    end;
handle_event(_Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, _Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call({make_win, Opts}, _From, #state{windows=Wins0}=State) ->
    case proplists:get_value(internal, Opts, false) of
	false ->
            {reply, create_external_win(?GET(top_frame), Opts), State};
	WinProps  ->
            {Reply, Wins} = create_internal_win(WinProps, Opts, Wins0),
            {reply, Reply, State#state{windows=Wins}}
    end;

handle_call({register_window, Window, Name, Ps}, _From,
	    #state{windows=Wins=#{frame:=_TopFrame, loose:=Loose, ch:=Top}}=State) ->
    External = proplists:get_value(external, Ps),
    Internal = proplists:get_value(internal, Ps, false),
    IsGeom = proplists:get_value(top, Ps),
    Win0 = proplists:get_value(gui_win, Ps),
    #win{frame = Frame} = Win0,
    Win = Win0#win{win=Window, name=Name},
    if External ->
	    wxFrame:show(Frame),
	    {reply, ok, State#state{windows=Wins#{loose:=Loose#{Frame => Win}}}};
       Internal =/= false ->
	    Ws = insert_internal_win(Win, Wins),
	    {reply, ok, State#state{windows=Ws}};
       IsGeom -> %% Specialcase for geom window
            #split{w1=Splash} = Top,
	    wxSplitterWindow:replaceWindow(win(Top), Splash, Frame),
	    wxWindow:destroy(Splash),
            layout_new_win(Win),
	    {reply, ok, State#state{windows=Wins#{ch:=Top#split{w1=Win}}}}
    end;

handle_call({close, Win}, _From, State0) ->
    {Res, NeedReset, State} = close_win(Win, State0),
    {reply, {Res, NeedReset}, State};

handle_call(get_windows, _From, #state{windows=#{loose:=Loose, ch:=Top}}=State) ->
    Contained = export_contained(Top),
    Free = export_loose(maps:values(Loose)),
    {reply, {Contained, Free}, State};

handle_call(get_images, _From, #state{images=Icons} = State) ->
    {reply, Icons, State};

handle_call(get_overlay, _From, #state{overlay=Overlay}=State) ->
    {reply, Overlay, State};

handle_call({update_layout,Contained0}, _From, #state{windows=#{ch:=Split}}=State) ->
    Contained = [C || C = {Type, _, _} <- Contained0, Type=:=split orelse Type=:=split_rev],
    update_layout(Contained, Split),
    {reply, ok, State};

handle_call(update_theme, _From, State) ->
    update_theme(State),
    {reply, ok, State};

handle_call({show_toolbar, Bool}, _From, #state{windows=#{frame:=Frame}}=State) ->
    TB = wxFrame:getToolBar(Frame),
    wxToolBar:show(TB, [{show, Bool}]),
    wxFrame:layout(Frame),
    wxWindow:refresh(Frame),
    {reply, ok, State};

handle_call(get_top_frame, _, #state{windows=#{frame:=Frame}}=State) ->
    {reply, Frame, State};

handle_call(need_gl_reset, _, State0) ->
    {Reset, State} = need_gl_reset(State0),
    {reply, Reset, State};

handle_call(Req, _From, State) ->
    io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, Req]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
handle_cast({selmode, _, _, _}=Sel, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Sel, TB)}};
handle_cast({mode_restriction, _}=Restrict, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Restrict, TB)}};
handle_cast({menu, {Menu, Key, Value}=Update}, #state{toolbar=TB}=State) ->
    wings_menu:update_menu_enabled(Menu, Key, Value),
    wings_toolbar:update(Update, TB),
    wings_view_win:update(Update),
    {noreply, State};
handle_cast({got_focus, Window, Props}, #state{toolbar=TB0}=State) ->
    Fun = fun(Menu, Key, Value) ->
		  wings_menu:update_menu_enabled(Menu, Key, Value),
		  wings_toolbar:update({Menu, Key, Value}, TB0)
	  end,
    [Fun(view, Key, Value) || {Key, Value} <- Props, is_boolean(Value)],
    ModeRest = proplists:get_value(mode_restriction, Props, none),
    TB = wings_toolbar:update({active, Window, ModeRest}, TB0),
    wings_status:active(Window),
    {noreply, update_active(Window, State#state{toolbar=TB})};

handle_cast({set_title, Win, Title}, #state{windows=#{ch:=Root, loose:=Loose}=Wins} = State) ->
    case find_win(Win, Root) of
	false ->
	    case lists:keyfind(Win, 1, maps:to_list(Loose)) of
		false -> {noreply, State};
		{_,#win{win=Obj, frame=Frame}=WinR} ->
		    wxTopLevelWindow:setTitle(Frame, Title),
		    {noreply, State#state{windows=Wins#{loose:=Loose#{Obj:=WinR#win{title=Title}}}}}
	    end;
	#win{frame=Obj, bar={_, ST}} = WinR ->
	    wxStaticText:setLabel(ST, Title),
	    SetLabel = fun(#split{w1=W1}=Where, _Other, _Grand) when W1=:=WinR ->
			       {ok, Where#split{w1=WinR#win{title=Title}}};
			  (#split{w2=W2}=Where, _Other, _Grand) when W2=:=WinR ->
			       {ok, Where#split{w2=WinR#win{title=Title}}}
		       end,
	    {ok, Tree} = update_win(Obj, Root, Root, SetLabel),
	    check_tree(Tree, Root),
	    {noreply, State#state{windows=Wins#{ch:=Tree}}}
    end;
handle_cast(Req, State) ->
    io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, Req]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%
handle_info(check_stopped_move, #state{overlay=Overlay, windows=Wins0} = State) ->
    Wins = attach_floating(stopped_moving(wx_misc:getMouseState()), Overlay, Wins0),
    {noreply, State#state{windows=Wins}};
handle_info({close_window, Obj}, #state{windows=#{ch:=Root}} = State0) ->
    State = case find_win(Obj, Root) of
		false -> State0;
		#win{win=Win, name=Name} ->
		    try wx_object:get_pid(Win) of
			_Pid  ->
                            After = fun({_,NeedReset,_}) -> reset_opengl(NeedReset) end,
                            Close = fun() -> close_win(Win, State0) end,
                            {_, _, State1} = wings_io:lock(whereis(wings), Close, After),
                            State1
                    catch _:_ -> %% Geom window, let wings_wm handle it first
                            wings_wm:psend(Name, close),
			    State0
		    end
	    end,
    {noreply, State};
handle_info(Msg, State) ->
    io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{windows=#{frame:=Frame}}) ->
    %% io:format("~p: terminate: ~p~n",[?MODULE, _Reason]),
    catch wxFrame:destroy(Frame),
    wx:destroy(),
    shutdown.

terminate_frame(_Ev, CB) ->
    %% always veto if we can, solves hanging on Mac
    %% that wants answer or a direct quit
    case wxCloseEvent:canVeto(CB) of
	true -> wxCloseEvent:veto(CB);
	false -> ok
    end,
    wings ! quit,
    ok.


%%%%%%%%%%%%%%%%%%%%%%
%% Window Management

update_active(Name, #state{active=Prev, windows=#{ch:=Root}}=State) ->
    ABG = wings_color:rgb4bv(wings_pref:get_value(title_active_color)),
    PBG = wings_color:rgb4bv(wings_pref:get_value(title_passive_color)),
    AFG = wings_color:rgb4bv(wings_pref:get_value(title_text_color)),
    PFG0 = wings_color:rgb4bv(wings_pref:get_value(title_passive_text_color, AFG)),
    PFG = passive_color(PFG0,AFG),
    try
	#win{bar={PBar,_}} = find_win(Prev, Root),
	_ = wxWindow:getSize(PBar), %% Sync to check PBar validity
	PChildren = wxWindow:getChildren(PBar),
	[wxWindow:setForegroundColour(PChild, PFG) ||
            PChild <- PChildren,
            wx:getObjectType(PChild) == wxWindow],
	wxWindow:setBackgroundColour(PBar, PBG),
	wxWindow:refresh(PBar)
    catch _:_ -> ignore
    end,
    try find_win(Name, Root) of
	false ->
	    State#state{active=undefined};
	#win{bar={ABar,_}} ->
	    AChildren = wxWindow:getChildren(ABar),
	    [wxWindow:setForegroundColour(AChild, AFG) ||
                AChild <- AChildren,
                wx:getObjectType(AChild) == wxWindow],
	    wxWindow:setBackgroundColour(ABar, ABG),
	    wxWindow:refresh(ABar),
	    State#state{active=Name}
    catch _:_ ->
	    State
    end.

passive_color({R,G,B,A} = PFG0,AFG) ->
    if PFG0 =:= AFG ->
            if R+G+B < 180 -> %% Dark text
                    {R+50,G+50,B+50,A};
               true ->
                    {R-50,G-50,B-50,A}
            end;
       true ->
            PFG0
    end.

update_theme(#state{windows=#{ch:=Root,loose:=Loose}, active=Active}) ->
    update_theme(Root, Active),
    [update_theme_0(Win, Active) || Win <- maps:values(Loose)],
    wxWindow:refresh(?GET(top_frame)).

update_theme(#split{w1=#win{}=W1, mode=undefined, w2=undefined}, Active) ->
    update_theme(W1, Active);
update_theme(#split{w1=W1, mode=Mode, w2=W2}, Active) when Mode =/= undefined ->
    update_theme(W1, Active),
    update_theme(W2, Active);
update_theme(#win{}=Win, Active) ->
    update_theme_0(Win, Active).

update_theme_0(#win{win=Win, name=WinName, bar=Bar}, Active) ->
    TBG = case WinName of
              Active -> wings_color:rgb4bv(wings_pref:get_value(title_active_color));
              _ -> wings_color:rgb4bv(wings_pref:get_value(title_passive_color))
          end,
    WBG = wings_color:rgb4bv(wings_pref:get_value(outliner_geograph_bg)),

    WChildren = wxWindow:getChildren(Win),
    [wxWindow:setBackgroundColour(WChild, WBG) ||
        Parent <- [Win|WChildren],
        WChild <- wxWindow:getChildren(Parent) ++ [Parent],
        not wx:is_null(WChild)],
    case Bar of
	{TBar,_} -> wxWindow:setBackgroundColour(TBar, TBG);
	_ -> ignore
    end.

preview_attach(false, Pos, Frame,
	       #state{windows=#{action:=Action}=Wins, overlay=Overlay}=State)
  when Action =:= undefined; Action =:= preview_attach ->
    case get_split_path(Pos, Wins) of
	ignore -> %% Outside attached window
            wings_status:message(?MODULE, ""),
	    overlay_hide(Overlay),
	    State#state{windows=delete_timer(Wins)};
	Path ->
            wings_status:message(?MODULE, ?__(1, "Hold SHIFT to not attach window")),
	    {Rect,_} = preview_rect(Path, Frame),
	    overlay_draw(Overlay, Rect, 170),
	    State#state{windows=setup_timer(Frame, Path, delete_timer(Wins))}
    end;
preview_attach(StoppedMove, _Pos, _Frame, #state{windows=Wins, overlay=Overlay}=State) ->
    case StoppedMove of
        ignore ->
            overlay_hide(Overlay),
            State#state{windows=delete_timer(Wins)};
        _ ->
            %% There comes an initial move event when window is created
            %% ignore that
            State
    end.

preview_rect({Obj, Path}, Frame) ->
    Dir = lists:last(Path),
    {X,Y,W0,H0} = wxWindow:getScreenRect(win(Obj)),
    {MaxW, MaxH} = wxWindow:getClientSize(win(Frame)),
    W = min(W0 div 2, MaxW),
    H = min(H0 div 2, MaxH),
    case Dir of
	left  -> {{X,      Y, W, H0}, W-(W0-25)};
	right -> {{X+W0-W, Y, W, H0}, -W-25};
	up    -> {{X,      Y, W0, H}, H-(H0-25)};
	down  -> {{X, Y+H0-H, W0, H}, -H-25}
    end.

make_overlay(Parent) ->
    Flags0 =
        ?wxFRAME_TOOL_WINDOW bor
	?wxFRAME_NO_TASKBAR bor
        ?wxSTAY_ON_TOP,
    Overlay = wxFrame:new(),
    Flags = case {os:type(), {?wxMAJOR_VERSION, ?wxMINOR_VERSION}} of
                {{_, linux}, Ver} when Ver >= {3,2} ->
                    wxFrame:setBackgroundStyle(Overlay, ?wxBG_STYLE_TRANSPARENT),
                    Flags0 bor ?wxNO_BORDER;
                {{_, linux}, Ver} when Ver >= {3,0} ->
                    wxFrame:setBackgroundStyle(Overlay, ?wxBG_STYLE_TRANSPARENT),
                    Flags0;
                _ ->
                    Flags0 bor ?wxNO_BORDER
            end,
    true = wxFrame:create(Overlay, Parent, -1, "", [{style, Flags}, {size, {300,300}}]),
    wxFrame:setBackgroundColour(Overlay, {95,138,255,200}),
    Panel = wxPanel:new(Overlay),
    wxFrame:setBackgroundColour(Panel, {95,138,255,200}),
    catch wxFrame:setTransparent(Overlay, 120),
    Overlay.

overlay_draw(Overlay, Rect, Alpha) ->
    catch wxFrame:setTransparent(Overlay, Alpha),
    wxFrame:setSize(Overlay, Rect),
    wxFrame:show(Overlay).

overlay_hide(Overlay) ->
    wxFrame:hide(Overlay).

attach_floating(false, _, #{op:=Op}=State) when Op =/= undefined ->
    setup_timer(State);
attach_floating(true, Overlay, #{op:=#{mwin:=Frame, mpath:=Path}, loose:=Loose}=State) ->
    wings_status:message(?MODULE, ""),
    case maps:get(Frame, Loose, false) of
	false ->
	    State#{action:=undefined, op:=undefined};
	#win{win=Win}=Window ->
	    overlay_hide(Overlay),
	    %% ?dbg("Attach window ~p ~p ~p~n",[Frame,Path,Window]),
	    St0 = State#{loose:=maps:remove(Frame,Loose), action:=undefined, op:=undefined},
	    DoWhileLocked = fun() -> attach_window(Path, Frame, Window, St0) end,
	    After = fun({_, NeedReset}) ->
                            reset_opengl(NeedReset),
                            inform_parent_changed(Win)
		    end,
	    {St, _ } = wings_io:lock(whereis(wings), DoWhileLocked, After),
            St
    end;
attach_floating(_B, Overlay, State) ->
    %% Spurious Move events on windows
    wings_status:message(?MODULE, ""),
    overlay_hide(Overlay),
    State#{action:=undefined, op:=undefined}.

attach_window({_,Path}=Split, WinFrame, NewWin, #{frame:=TopFrame, szr:=Szr, ch:=Child} = State) ->
    Attach = fun() ->
		     {_,Pos} = preview_rect(Split, NewWin),
                     MakeWin = fun(Parent) -> make_internal_win(Parent, NewWin) end,
		     {Root, _NewWin, NeedReset} = split_win(Path, MakeWin, Child, Pos),
		     wxWindow:destroy(WinFrame),
		     case win(Root) =:= win(Child) of
			 false -> wxSizer:replace(Szr, win(Child), win(Root));
			 true  -> ignore
		     end,
                     wxFrame:layout(TopFrame),
                     wxWindow:refresh(win(Root)),
		     check_tree(Root, Child),
		     {State#{ch:=Root}, NeedReset}
	     end,
    wings_io:batch(Attach).

split_win([Dir], MakeWin, Node, Pos0) ->
    {Mode, Which} = split(Dir),
    Pos = pos_from_permille(Pos0, Mode, win(Node)),
    case Node of
	#split{mode=undefined, w1=Root, obj=Splitter} -> %% Top Window
            NewWin = MakeWin(Splitter),
	    {set_splitter(Which, Mode, Pos, Node, Root, NewWin), NewWin, false};
	_Leaf ->
	    Parent = wxWindow:getParent(win(Node)),
	    NewNode = make(Parent),
	    NewWin = MakeWin(win(NewNode)),
	    reparent(Node, NewNode),
            Bool = node_needs_to_reset(Node),
            %% ?dbg("~p ~p~n", [Bool, Node]),
	    {set_splitter(Which, Mode, Pos, NewNode, Node, NewWin), NewWin, Bool}
    end;
split_win([Which|Path], MakeWin, #split{mode=Mode} = Node, Pos) ->
    {Child0, Child1} = child(Which, Node),
    {Child, NewWin, NeedReset} = split_win(Path, MakeWin, Child0, Pos),
    wxSplitterWindow:replaceWindow(win(Node), win(Child0), win(Child)),
    {set_splitter(Which, Mode, false, Node, Child1, Child), NewWin, NeedReset};
split_win([_|Path], MakeWin, Node, Pos0) ->
    %% Something went wrong here a plugin or window we don't know about
    %% in this release was not created as it should so the path was incorrect
    split_win(Path, MakeWin, Node, Pos0).

make(Parent) ->
    Style = case os:type() of
		{unix, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
		{win32, _} -> ?wxSP_BORDER bor ?wxSP_LIVE_UPDATE;
		_ -> ?wxSP_3DBORDER bor ?wxSP_LIVE_UPDATE
	    end,
    New = wxSplitterWindow:new(Parent, [{style, Style}]),
    wxSplitterWindow:setSashGravity(New, 0.5),
    wxSplitterWindow:setMinimumPaneSize(New, 50),
    #split{obj=New}.

set_splitter(first, Mode, Pos, Sp, W2, W1) ->
    case Pos of
	false -> ignore;
	_ -> wxSplitterWindow:Mode(win(Sp), win(W1), win(W2), [{sashPosition, Pos}])
    end,
    Sp#split{mode=Mode, w1=W1, w2=W2};
set_splitter(second, Mode, Pos, Sp, W1, W2) ->
    case Pos of
	false -> ignore;
	_ -> wxSplitterWindow:Mode(win(Sp), win(W1), win(W2), [{sashPosition, Pos}])
    end,
    Sp#split{mode=Mode, w1=W1, w2=W2}.

reparent(Child, Obj) ->
%%    ?dbg("Reparent(~w, ~w)~n  ~p~n", [win(Child), win(Obj),
%%            element(2, process_info(self(), current_stacktrace))]),
    true = wxWindow:reparent(win(Child), win(Obj)),
    ok.


stopped_moving(#wxMouseState{leftDown=L, middleDown=M, rightDown=R, shiftDown=Shift}=_MS) ->
    %% io:format("~p ~p ~p ~p~n",[L, M, R, _MS]),
    case Shift of
        true  -> ignore;
        false -> not (L orelse M orelse R)
    end.

get_split_path(ScreenPos, #{ch:=Child}) ->
    get_split_path(ScreenPos, Child, []).

get_split_path(Pos, #split{mode=undefined, w1=W1, w2=undefined}, Path) ->
    get_split_path(Pos, W1, Path);
get_split_path(Pos, #split{obj=Obj, mode=Mode, w1=W1, w2=W2}=Split, Path) ->
    Sash = wxSplitterWindow:getSashPosition(Obj),
    Win = wxWindow:getScreenRect(Obj),
    case is_inside(Pos, Win) of
	true ->
	    case get_split_op(Mode, Pos, Win, Sash) of
		first  -> get_split_path(Pos, W1, [first|Path]);
		second -> get_split_path(Pos, W2, [second|Path]);
		split  ->
		    Dir0 = get_split_op(other(Mode), Pos, Win, middle),
		    Dir = split(other(Mode),Dir0),
		    {Split, lists:reverse([Dir|Path])}
	    end;
	false ->
	    ignore
    end;
get_split_path(Pos, #win{frame=Leaf}, Path) ->
    Win = wxWindow:getScreenRect(Leaf),
    case is_inside(Pos, Win) of
	true  -> {Leaf, lists:reverse([get_split_side(Pos, Win)|Path])};
	false -> ignore
    end.

-define(EPS, 0.02).
-define(MIN, (1.0 - ?EPS)).
-define(MAX, (1.0 + ?EPS)).
is_inside({PX,PY}, {WX,WY,WW,WH}) ->
    (WX * ?MIN) < PX andalso PX < (WX+WW)*?MAX
	andalso (WY * ?MIN) < PY andalso PY < (WY+WH)*?MAX.

get_split_op(splitHorizontally, {_,Y}, {_, WY, _, WW}, Sash) ->
    get_split_op2(Y, WY, WW, Sash);
get_split_op(splitVertically, {X, _}, {WX, _, WH, _}, Sash) ->
    get_split_op2(X, WX, WH, Sash).

get_split_op2(Pos, Top, Size, Sash) ->
    ScreenSash = case Sash of
		     middle -> Top + Size div 2;
		     _ -> Top + Sash
		 end,
    if abs(Pos - ScreenSash) < 20, Sash =/= middle -> split;
       Pos < ScreenSash -> first;
       true -> second
    end.

%% ___
%% |X| Figure out in which part of X we are
%% ---
get_split_side({PX,PY}, {AX,AY,W,H}) ->
    BX = AX+W, BY = AY+H,
    CX = AX,   CY = AY+H,
    DX = AX+W, DY = AY,
    case {((BX-AX)*(PY-AY)-(BY-AY)*(PX-AX)) > 0, ((DX-CX)*(PY-CY)-(DY-CY)*(PX-CX)) > 0} of
	{true, true}   -> down;
	{true, false}  -> left;
	{false, true}  -> right;
	{false, false} -> up
    end.

find_win(Frame, Root) ->
    case find_win(Frame, Root, []) of
	false -> false;
	{Win, _Path} -> Win
    end.

find_path(Name, Root) ->
    case find_win(Name, Root, []) of
	{_, RevPath} ->
	    lists:reverse(RevPath);
	false -> false
    end.

find_win(Frame, #split{w1=W1,w2=W2}, Acc) ->
    case find_win(Frame, W1, [left|Acc]) of
	false -> W2 =/= undefined andalso find_win(Frame, W2, [right|Acc]);
	Win -> Win
    end;
find_win(Name, #win{name=Name} = Win, Path)  -> {Win, Path};
find_win(Frame, #win{frame=F, win=W}=Win, Path) ->
    IsRef = is_tuple(Frame) andalso element(1, Frame) =:= wx_ref,
    case IsRef andalso (wings_util:wxequal(Frame,F) orelse
			wings_util:wxequal(Frame,W)) of
	true -> {Win, Path};
	false -> false
    end.

update_win(Win, #split{w1=#win{frame=Win}, w2=Other}=Parent, GrandParent, Fun) ->
    Fun(Parent, Other, GrandParent);
update_win(Win, #split{w2=#win{frame=Win}, w1=Other}=Parent, GrandParent, Fun) ->
    Fun(Parent, Other, GrandParent);
update_win(Win, #split{w1=W1, w2=W2}=Parent, _, Fun) ->
    case update_win(Win, W1, Parent, Fun) of
	false ->
	    case update_win(Win, W2, Parent, Fun) of
		false -> false;
		{Acc, UpdW2} -> {Acc, Parent#split{w2=UpdW2}}
	    end;
	{Acc, UpdW1} ->
	    {Acc, Parent#split{w1=UpdW1}}
    end;
update_win(_, _, _, _) -> false.

close_win(Win, #state{windows=#{frame:=TopFrame,ch:=Tree,loose:=Loose,szr:=Szr}=Wins}=State) ->
    catch wx_object:stop(Win),
    case find_win(Win, Tree) of
	false ->
	    case lists:keyfind(Win, #win.win, maps:values(Loose)) of
		#win{frame=Frame} = _Win ->
		    wxFrame:destroy(Frame),
                    _ = wxWindow:findFocus(), %% Sync the destroy
		    {closed, false, State#state{windows=Wins#{loose:=maps:remove(Frame, Loose)}}};
		false ->
                    {ignored, false, State}
	    end;
	#win{frame=Obj} ->
            Close = fun(Where, Other, GrandP) -> close_window(Obj, Where, Other, GrandP, Szr) end,
            {NeedReset, Root} =
                wx:batch(fun() ->
                                 {NeedReset, Root} = update_win(Obj, Tree, Tree, Close),
                                 check_tree(Root, Tree),
                                 wxFrame:layout(TopFrame),
                                 wxWindow:refresh(TopFrame),
                                 {NeedReset, Root}
                         end),
	    {closed, NeedReset, State#state{windows=Wins#{ch:=Root}}}
    end.

close_window(Delete, Split, Other, GrandP, Szr) ->
    case GrandP of
	Split when is_record(Other, win) -> %% TopLevel
            wxWindow:hide(Delete),
	    %% reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:unsplit(win(GrandP), [{toRemove, Delete}]),
	    wxWindow:destroy(Delete),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {false, GrandP#split{mode=undefined, w1=Other, w2=undefined}};
	Split when is_record(Other, split) ->
	    Frame = wxWindow:getParent(win(GrandP)),
	    reparent(win(Other), Frame),
            wxSizer:replace(Szr, win(GrandP), win(Other)),
            wxWindow:hide(win(GrandP)),
	    wxWindow:destroy(win(GrandP)),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {node_needs_to_reset(Other), Other};
	#split{} ->
	    reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
            wxWindow:hide(win(Split)),
	    wxWindow:destroy(win(Split)),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {node_needs_to_reset(Other), Other}
    end.

detach_window(#wxMouse{type=motion, leftDown=true, x=X,y=Y}, _OldFrame,
	      #{action:=detach_move, op:=#{frame:=Frame, win:=Win, disp:={XD,YD}}}=State) ->
    #win{bar={Bar,_}}=Win,
    {X1,Y1} = wxWindow:clientToScreen(Bar, {X,Y}),
    wxFrame:move(Frame, X1-XD, Y1-YD),
    State;
detach_window(#wxMouse{type=motion, leftDown=true, x=X,y=Y}, OldFrame,
	      #{action:=detach_init, op:=#{pos:={X0,Y0}, win:=Win}=Op,
		loose:=Loose}=State) ->
    #win{bar={Bar,_}} = Win,
    {X1,Y1} = wxWindow:clientToScreen(Bar, {X,Y}),
    case start_drag(abs(X1-X0), abs(Y1-Y0)) of
	false ->
	    %% io:format("Ignore: ~p~n",[?LINE]),
	    State;
	true  ->
	    {XB,YB} = wxWindow:getScreenPosition(Bar),
	    Displace = {X1-XB, Y1-YB},
	    WhileLocked = fun() ->
				  case wings_io:batch(fun() -> setup_detach(Win, State) end) of
				      {{true, Res, NeedReset}, Root} ->
					  receive
					      #wx{obj=OldFrame, event=#wxShow{}} ->
						  {Res, NeedReset, Root}
					  end;
				      {{false, Res, NeedReset}, Root} ->
					  {Res, NeedReset, Root}
				  end
			  end,
            DoAfter =  fun({_, NeedReset, _}) -> reset_opengl(NeedReset) end,
	    {NewWin, _, Root} = wings_io:lock(whereis(wings), WhileLocked, DoAfter),
	    check_tree(Root, maps:get(ch, State)),
	    #win{frame=Frame,win=W1}=NewWin,
            inform_parent_changed(W1),
	    wxWindow:captureMouse(Bar),
	    State#{action:=detach_move,
		   op:=Op#{frame=>Frame, disp=>Displace},
		   ch:=Root, loose:=Loose#{Frame=>NewWin}}
    end;
detach_window(#wxMouse{type=left_up}, F, #{action:=Action, op:=#{win:=Win}}=State) ->
    case Action of
	detach_move ->
	    #win{bar={Bar,_}} = Win,
	    ok = wxWindow:releaseMouse(Bar),
	    wxWindow:destroy(F);
	_ -> ignore
    end,
    State#{action:=undefined, op:=undefined};
detach_window(#wxMouse{type=left_down, x=X, y=Y}, Frame, #{ch:=Top} = State) ->
    case find_win(Frame, Top) of
        false ->
            State;
        #win{bar={Bar,_}} = Win ->
            Pos = wxWindow:clientToScreen(Bar, {X,Y}),
            State#{action:=detach_init, op:=#{win=>Win, pos=>Pos}}
    end;
detach_window(_Ev, _, State) ->
    %% io:format("Ignore: ~p~n",[_Ev]),
    State.

setup_detach(#win{frame=Container}=Win, #{szr:=Szr, ch:=Child, frame:=Parent}) ->
    Detach = fun(Where, Other, GrandP) ->
		     Res = do_detach_window(Win, Where, Other, GrandP, Parent, Szr),
		     wings ! {wm, dirty},
		     Res
	     end,
    Res = update_win(Container, Child, top, Detach),
    %% wxSizer:layout(Szr),
    wxFrame:layout(Parent),
    wxWindow:refresh(Parent),
    Res.

do_detach_window(#win{frame=Container, win=Child, title=Label}=Win,
		 Split, Other, GrandP, Top, Szr) ->
    {X,Y,_,_} = wxWindow:getScreenRect(Container),
    {W,H} = wxWindow:getClientSize(Container),
    PosSz = {X,Y,W,H},
    wxWindow:setSize(Container, {-1,-1,1,1}),
    reparent(Container, Top),
    FrameW = detach_to_external_win(Top, Child, PosSz, Label),
    Frame = Win#win{frame=FrameW, bar=undefined},
    wxWindow:hide(Container),
    Sizer = wxWindow:getSizer(Container),
    wxSizer:detach(Sizer, Child),
    case GrandP of
	top when is_record(Other, split) ->
	    reparent(win(Other), Top),
	    wxSizer:replace(Szr, win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
            Reset = node_needs_to_reset(Other),
	    {{show_detached(Container), Frame, Reset}, Other};
	top ->
            %% Other must be geom
	    wxSplitterWindow:unsplit(win(Split), [{toRemove, Container}]),
	    {{show_detached(Container), Frame, false},
	     Split#split{mode=undefined, w1=Other, w2=undefined}};
	#split{} ->
	    reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
            Reset = node_needs_to_reset(Other),
	    {{show_detached(Container), Frame, Reset}, Other}
    end.

show_detached(Window) ->
    case os:type() of
	{unix, linux} ->
	    %% We are grabbing mouse events to move the detached window
	    %% but on linux/gtk we can not grab hidden or non realized
	    %% windows so we sync on show to before we grab
	    wxWindow:connect(Window, show),
	    wxWindow:show(Window),
	    true;
	_ ->
	    false
    end.

start_drag(DX, DY) ->
    (DX > wxSystemSettings:getMetric(?wxSYS_DRAG_X))
	orelse (DY > wxSystemSettings:getMetric(?wxSYS_DRAG_Y)).

detach_to_external_win(Parent, Child, {X0,Y0, W, H} = _Dim, Label) ->
    {Frame,_} = create_external_win(Parent, [{title,Label}, {pos, {X0,Y0}}, {size, {W,H}}]),
    wxWindow:connect(Frame, move),
    wxWindow:connect(Frame, close_window),
    reparent(Child, Frame),
    wxWindow:refresh(Child),
    wxWindow:update(Child),
    wxWindow:show(Frame),
    Frame.

inform_parent_changed(Win) ->
    try wx_object:get_pid(Win) ! parent_changed
    catch _:_ -> wings ! parent_changed
    end.

split(left)  -> {splitVertically, first};
split(right) -> {splitVertically, second};
split(up)    -> {splitHorizontally, first};
split(down)  -> {splitHorizontally, second}.

split(splitVertically, first) -> left;
split(splitVertically, second) -> right;
split(splitHorizontally, first) -> up;
split(splitHorizontally, second) -> down.

other(splitHorizontally) -> splitVertically;
other(splitVertically) -> splitHorizontally.

child(first,  #split{w1=W1, w2=W2}) -> {W1, W2};
child(second, #split{w1=W1, w2=W2}) -> {W2, W1}.

win(#split{obj=Obj}) -> Obj;
win(#win{frame=Obj}) -> Obj;
win(Obj) -> Obj.

pos_from_permille({permille, Permille}, Mode, Obj) ->
    {W,H} = wxWindow:getClientSize(Obj),
    case Mode of
	splitVertically   -> round(W * Permille / 1000)-W;
	splitHorizontally -> round(H * Permille / 1000)-H
    end;
pos_from_permille(Pos, _, _) -> Pos.

delete_timer(#{op:= #{mtimer:=Ref}}=St)
  when Ref =/= undefined ->
    timer:cancel(Ref),
    St#{action:=undefined, op:=undefined};
delete_timer(St) ->
    St.

setup_timer(Frame, Path, St) ->
    setup_timer(St#{action:=preview_attach, op=>#{mwin=>Frame, mpath=>Path}}).
setup_timer(#{op:=Op} = St) ->
    {ok, TRef} = timer:send_after(200, check_stopped_move),
    St#{op:=Op#{mtimer=>TRef}}.

create_external_win(Parent, Ps) ->
    FStyle = {style, ?wxCAPTION bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER},
    {Size, MinSize} = case lists:keyfind(size, 1, Ps) of
			  {size, Sz} -> {Sz, Sz};
			  false  -> {false, {100, 100}}
	   end,
    Opts = case lists:keyfind(pos, 1, Ps) of
	       {pos, Pos0} ->
		   TopFrame = ?GET(top_frame),
		   Pos1 = wxWindow:clientToScreen(TopFrame, Pos0),
		   Pos  = validate_pos(Pos1, MinSize, wx_misc:displaySize()),
		   [{pos,Pos}];
	       false  ->
		   []
	   end,
    Title = proplists:get_value(title, Ps),
    Frame = (useframe()):new(Parent, ?wxID_ANY, Title, [FStyle|Opts]),
    wxWindow:connect(Frame, move),
    wxWindow:connect(Frame, close_window),
    Size =/= false andalso wxWindow:setClientSize(Frame, Size),
    Win = #win{frame=Frame, title=Title, ps=#{close=>true, move=>true}},
    {Frame, [{gui_win, Win},external|Ps]}.

create_internal_win(PathPos, Ps, #{szr:=Szr, ch:=Root0} = State) ->
    Title = proplists:get_value(title, Ps),
    Win0  = case proplists:get_value(top, Ps) of
                undefined -> #win{title=Title, ps=#{close=>true, move=>true}};
                true -> #win{title=Title, ps=#{close=>false, move=>false}}
            end,
    case PathPos of
        undefined ->
            Win = make_internal_win(win(Root0), Win0),
            {{win(Win), [{gui_win, Win}|Ps]}, State};
        {Path, Pos} ->
            MakeWin = fun(Parent) -> make_internal_win(Parent, Win0) end,
            {Root, Win, NeedReset} = split_win(Path, MakeWin, Root0, {permille, Pos}),
            case win(Root) =:= win(Root0) of
                false -> wxSizer:replace(Szr, win(Root0), win(Root));
                true  -> ignore
            end,
            check_tree(Root, Root0),
            Reset = NeedReset orelse maps:get(reset, State, false),
            {{win(Win), [{gui_win, Win}|Ps]}, State#{ch:=Root, reset => Reset}}
    end.

insert_internal_win(#win{frame=Frame} = Win, #{frame:=TopFrame, ch:=Root} = State) ->
    layout_new_win(Win),
    SetLabel = fun(#split{w1=W1}=Where, _Other, _Grand) when W1#win.frame=:=Frame ->
                       {ok, Where#split{w1=Win}};
                  (#split{w2=W2}=Where, _Other, _Grand) when W2#win.frame=:=Frame ->
                       {ok, Where#split{w2=Win}}
               end,
    {ok, Tree} = update_win(Frame, Root, Root, SetLabel),
    wxFrame:layout(TopFrame),
    wxWindow:refresh(win(Root)),
    State#{ch:=Tree}.

layout_new_win(#win{frame=Frame, win=Win}) ->
    Sizer = wxWindow:getSizer(Frame),
    wxSizer:add(Sizer, Win,  [{proportion, 1}, {flag, ?wxEXPAND}, {border, 2}]),
    wxWindow:layout(Frame).

make_internal_win(Parent,#win{title=Label, win=Child, ps=#{close:=Close, move:=Move}}=WinC) ->
    Win = wxPanel:new(Parent, []),
    PBG = wings_color:rgb4bv(wings_pref:get_value(title_passive_color)),
    {Bar,ST} = Wins = make_bar(Win, PBG, Label, Close),
    Top = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Top, Bar, [{proportion, 0}, {flag, ?wxEXPAND}, {border, 2}]),
    case Child =:= undefined of
        true -> ok;
        false ->
            reparent(Child, Win), %% Send event here
            wxSizer:add(Top, Child,  [{proportion, 1}, {flag, ?wxEXPAND}, {border, 2}])
    end,
    wxWindow:setSizer(Win, Top),
    wxWindow:layout(Win),
    wxWindow:connect(Bar, enter_window, [{userData, {win, Win}}]),
    case Move of
        true ->
            [wxWindow:connect(W, Ev, [{userData, {move, Win}}])
             || Ev <- [left_down, left_up, motion], W <- [Bar,ST]];
        false ->
            wxWindow:connect(Bar, left_down)
    end,
    WinC#win{frame=Win, bar=Wins}.

-define(WIN_BAR_HEIGHT, 16).

make_bar(Parent, BG, Label, Close) ->
    Bar = wxPanel:new(Parent, [{style, ?wxBORDER_NONE}, {size, {-1, ?WIN_BAR_HEIGHT}}]),
    case os:type() of
	{win32, _} -> wxWindow:setDoubleBuffered(Bar, true);
	_ -> ignore
    end,
    FG = wings_pref:get_value(title_text_color),
    #{size:=Sz} = FI0 = wings_text:get_font_info(?GET(system_font_wx)),
    FI = FI0#{size:=Sz-1, weight=>bold},
    {Font,Space} = case os:type() of
		       {unix, darwin} -> {wings_text:make_wxfont(FI), 6};
		       _ -> {wings_text:make_wxfont(FI), 4}
		   end,
    wxPanel:setFont(Bar, Font),
    wxWindow:setBackgroundColour(Bar, BG),
    wxWindow:setForegroundColour(Bar, wings_color:rgb4bv(FG)),
    WBSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addStretchSpacer(WBSz),
    wxSizer:add(WBSz, ST=wxStaticText:new(Bar, ?wxID_ANY, Label), [{flag, ?wxALIGN_CENTER}]),
    {_, H} = wxWindow:getSize(ST),
    wxSizer:addStretchSpacer(WBSz),
    Close andalso make_close_button(Parent, Bar, WBSz, H),
    wxSizer:addSpacer(WBSz, 3),
    wxWindow:setMinSize(Bar, {-1, H+Space}),
    wxWindow:setSizer(Bar, WBSz),
    {Bar,ST}.

make_close_button(Parent, Bar, WBSz, H) ->
    Self = self(),
    CB = fun(_, Ev) ->
		 wxMouseEvent:skip(Ev),
		 Self ! {close_window, Parent}
	 end,
    SBM = case {os:type(), {?wxMAJOR_VERSION, ?wxMINOR_VERSION}} of
              {{_, linux}, Ver} when Ver < {3,1} ->
                  Bitmap = wxArtProvider:getBitmap("wxART_CLOSE", [{size,{16,16}}]),
                  Butt = wxStaticBitmap:new(Bar, ?wxID_EXIT, Bitmap),
                  wxWindow:connect(Butt, left_down, [{callback, CB}]),
                  Butt;
              {{win32, _}, Ver} when Ver < {3,1} ->
                  Bitmap = wxArtProvider:getBitmap("wxART_CLOSE", []),
                  Butt = wxStaticBitmap:new(Bar, ?wxID_EXIT, Bitmap),
                  wxWindow:connect(Butt, left_down, [{callback, CB}]),
                  Butt;
              {_, _} ->
                  Bitmap = wxArtProvider:getBitmap("wxART_CLOSE", []),
                  Butt = wxBitmapButton:new(Bar, ?wxID_EXIT, Bitmap,
                                            [{size, {H,H}}, {style,?wxNO_BORDER}]),
                  wxWindow:connect(Butt, command_button_clicked, [{callback, CB}]),
                  Butt
          end,
    %% io:format("SBM = ~p~n",[wxWindow:getSize(SBM)]),
    wxSizer:add(WBSz, SBM, [{flag, ?wxALIGN_CENTER}]),
    wxWindow:connect(SBM, command_button_clicked, [{callback, CB}]).

useframe() ->
    %% Miniframes can't be resized in gtk and wxWidgets 3.1
    case {os:type(), {?wxMAJOR_VERSION, ?wxMINOR_VERSION}} of
        {{_, linux}, Ver} when Ver > {3,0} ->  wxFrame;
        _ -> wxMiniFrame
    end.

export_loose(Windows) ->
    Exp = fun(#win{name=Name, win=Win, frame=Frame}) ->
		  {X0,Y0,_,_} = wxWindow:getRect(Frame),
		  Pos = wxWindow:screenToClient(?GET(top_frame),{X0,Y0}),
		  Size = wxWindow:getClientSize(Win),
		  {Name, Pos, Size, []}
	  end,
    [Exp(Win) || Win <- Windows].

export_contained(#split{mode=undefined, w2=undefined}) ->
    [{geom, {-1,-1}, {-1,-1}, []}]; %% Special case
export_contained(Root) ->
    Path = find_path(geom, Root),
    lists:reverse(tree_to_list(Root, Path, [])).

update_layout([{SpltMode,Mode,Permille}|Contained], #split{obj=Obj, mode=Mode, w1=W10, w2=W20}) ->
    {W,H} = wxWindow:getClientSize(Obj),
    Pos =
	case Mode of
	    splitVertically   -> round(W * Permille / 1000);
	    splitHorizontally -> round(H * Permille / 1000)
	end,
    wxSplitterWindow:setSashPosition(Obj, Pos),
    {W1,W2} =
	case SpltMode of
	    split -> {W10,W20};
	    split_rev -> {W20,W10}
	end,
    Cont = update_layout(Contained, W1),
    update_layout(Cont, W2);
update_layout(Contained, _) -> Contained.

tree_to_list(#split{obj=Obj, mode=Mode,w1=W1,w2=W2}, Path0, Acc0) ->
    SashPos = wxSplitterWindow:getSashPosition(Obj),
    {W,H} = wxSplitterWindow:getClientSize(Obj),
    Pos = case Mode of
	      splitHorizontally -> round(1000 * (SashPos / H));
	      splitVertically -> round(1000 * (SashPos / W))
	  end,
    case Path0 of
	[right|Path] ->
	    Acc = tree_to_list(W2, Path, [{split_rev,Mode,Pos}|Acc0]),
	    tree_to_list(W1, [], Acc);
	[left|Path] ->
	    Acc = tree_to_list(W1, Path, [{split,Mode,Pos}|Acc0]),
	    tree_to_list(W2, [], Acc);
	[] ->
	    Acc = tree_to_list(W1, [], [{split,Mode,Pos}|Acc0]),
	    tree_to_list(W2, [], Acc)
    end;
tree_to_list(#win{name=Name}, _, Acc) ->
    [{Name, {-1,-1}, {-1,-1}, []} | Acc].

check_tree(#split{} = T, Orig) ->
    try
	true = tree_consistent(T)
    catch _:Reason:ST ->
	    io:format("Failed ~p ~p~n", [Reason, ST]),
	    io:format("Orig ~p~nNew ~p~n",[Orig, T])
    end;
check_tree(T, Orig) ->
    io:format("Failed no root ~n", []),
    io:format("Orig ~p~nNew ~p~n",[Orig, T]).

tree_consistent(#split{w1=#win{}, mode=undefined, w2=undefined}) ->
    true;
tree_consistent(#split{w1=W1, mode=Mode, w2=W2})
  when Mode =/= undefined ->
    tree_consistent(W1) andalso tree_consistent(W2);
tree_consistent(#win{}) -> true.

%%%%%%%%%%%%%%%%%%%%%%
set_icon(Frame) ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Ebin, "wings_icon_379x379"),
    wings_io:set_icon(Frame, IconFile).

%% Returns a list of wxImages
make_icons() ->
    MakeImage = fun({Name, {W, H, Bin}}) ->
			Image = wxImage:new(W,H,Bin),
			{Name, {W,H}, Image};
		   ({Name, {W, H, Rgb, Alpha}}) ->
			Image = wxImage:new(W,H,Rgb),
			wxImage:setAlpha(Image, Alpha),
			{Name, {W,H}, Image}
		end,
    [MakeImage(Raw) || Raw <- wings_io:read_icons()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menubar

init_menubar(Frame) ->
    ets:new(wings_menus, [named_table, public, {keypos,2}]),
    put(wm_active, {menubar, geom}),
    WorkAround =
        case os:type() of
            {_, darwin} ->
                try
                    %% Fool dialyzer
                    WxMB = list_to_atom("wxMenuBar"),
                    WxMB:setAutoWindowMenu(false),
                    false
                catch _:_ ->
                        %% Exists in newer wx (erlang release)
                        true
                end;
            _ ->
                false
        end,
    MB = wxMenuBar:new(),
    wings_menu:setup_menus(MB, top_menus(WorkAround)),
    wxFrame:setMenuBar(Frame, MB),
    erase(wm_active),
    MB.

%%%%%%%%%%%%%

need_gl_reset(#state{windows = Wins} = State) ->
    case maps:get(reset, Wins, false) of
        false -> {false, State};
        true -> {true, State#state{windows = Wins#{reset := false}}}
    end.

node_needs_to_reset(#split{w1=W1,w2=W2}) ->
    node_needs_to_reset(W1) orelse
        node_needs_to_reset(W2);
node_needs_to_reset(#win{win=Win}) ->
    wx:getObjectType(Win) =:= wxGLCanvas.

reset_opengl(false) ->
%%    ?dbg("reset FALSE~n", []),
    ok;
reset_opengl(true) ->
%%    ?dbg("reset TRUE~n", []),
    case os:type() of
        {unix, linux} ->
            timer:sleep(?GL_WAIT); %% give wx time to realize windows on X11
        _ ->
            ok
    end,
    wings_io:reset_video_mode_for_gl(true),
    ok.
