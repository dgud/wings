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

-export([top_menus/0, make_win/2, register_win/3, close/1, set_focus/1,set_title/2,
         get_top_frame/0,
         show_toolbar/1,
	 export_layout/0, import_layout/2, reinit_layout/0,
	 get_overlay/0, overlay_draw/3, overlay_hide/1,
	 get_icon_images/0, get_colors/0, update_theme/0]).

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

-define(wxID_OSX_HIDE, 5250).
-define(wxID_OSX_HIDEOTHERS, 5251).
-define(wxID_OSX_SHOWALL, 5252).
-define(wxID_OSX_MENU_LAST, 5255).

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    Frame = wx_object:start_link({local, ?MODULE}, ?MODULE, [args], []),
    ?SET(top_frame, Frame),
    {ok, wx_object:get_pid(Frame)}.

top_menus() ->
    Tail0 = [{?__(7,"Help"),help,wings_help:menu()}],
    Tail = case wings_pref:get_value(show_develop_menu) of
	       true ->
		   [{"Develop",develop,wings_develop:menu()}|Tail0];
	       false ->
		   Tail0
	   end,
    [{?__(1,"File"),  file,wings_file:menu()},
     {?__(2,"Edit"),  edit,wings:edit_menu()},
     {?__(3,"View"),  view,wings_view:menu()},
     {?__(4,"Select"),select,wings_sel_cmd:menu()},
     {?__(5,"Tools"), tools, wings:tools_menu()},
     {?__(6,"Window"),window,wings:window_menu()}|Tail].

make_win(Title, Opts) ->
    case proplists:get_value(internal, Opts, false) of
	false -> {make_win(?GET(top_frame), Title, Opts), [external|Opts]};
	_WinProps  -> {?GET(top_frame), [{title, Title}|Opts]}
    end.

make_win(Parent, Title, Ps) ->
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
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title, [FStyle|Opts]),
    Size =/= false andalso wxWindow:setClientSize(Frame, Size),
    Frame.

register_win(Window, Name, Ps) ->
    wx_object:call(?MODULE, {new_window, Window, Name, Ps}).

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

update_theme() ->
    wx_object:call(?MODULE, update_theme).

close(Win) ->
    wx_object:call(?MODULE, {close, Win}).

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
	wxSizer:add(Sizer, win(Top), [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSplitterWindow:initialize(win(Top), Canvas),
	Toolbar = wings_toolbar:init(Frame, IconImgs),
	wxSizer:setSizeHints(Sizer, win(Top)),
	wxFrame:setSizer(Frame, Sizer),

	wxWindow:connect(Frame, close_window, [{callback, fun terminate_frame/2}]),
	wxWindow:connect(Frame, command_menu_selected, []),
	wxWindow:connect(Frame, activate, []),
	case os:type() of
	    {_, darwin} ->
		Me = self(),
		CB = fun(_,_) -> Me ! {init_menus, Frame} end,
		wxWindow:connect(Frame, show, [{callback, CB}]);
	    _ ->
		init_menubar(Frame)
	end,
	Wins = #{frame=>Frame, ch=>Top#split{w1=Canvas}, szr=>Sizer,
		 loose=>#{}, action=>undefined, op=>undefined},
	Overlay = make_overlay(Frame),
        %% Init OpenGL from wings process
        wings ! {frame_created, Frame},
        receive opengl_initialized -> ok end,
	{Frame, #state{toolbar=Toolbar, images=IconImgs, windows=Wins, overlay=Overlay}}
    catch _:Reason ->
	    io:format("CRASH: ~p ~p ~p~n",[?MODULE, Reason, erlang:get_stacktrace()])
    end.

make_splash(Canvas, Imgs) ->
    Szr = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addStretchSpacer(Szr),
    {Splash, _} = wings_help:about_panel(Canvas,Imgs),
    wxSizer:add(Szr, Splash, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(Szr),
    wxPanel:setSizer(Canvas, Szr),
    Canvas.

%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{obj=Win, event=#wxMove{}}, State) ->
    MS = wx_misc:getMouseState(),
    {X,Y} = wx_misc:getMousePosition(),
    %% io:format("Move ~p ~p ~p ~n", [Win, X,Y]),
    {noreply, preview_attach(stopped_moving(MS), {X,Y}, Win, State)};

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
	    end
    end;
handle_event(_Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, _Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call({new_window, Window, Name, Ps}, _From,
	    #state{windows=Wins=#{loose:=Loose, ch:=Top}}=State) ->
    External = proplists:get_value(external, Ps),
    Internal = proplists:get_value(internal, Ps, false),
    Geom = proplists:get_value(top, Ps),
    Win0 = #win{win=Window, name=Name},
    if External ->
	    Frame = wx:typeCast(wxWindow:getParent(Window), wxMiniFrame),
	    Title = wxFrame:getTitle(Frame),
	    Win = Win0#win{frame=Frame, title=Title, ps=#{close=>true, move=>true}},
	    wxWindow:connect(Frame, move),
	    wxWindow:connect(Frame, close_window),
	    wxFrame:show(Frame),
	    {reply, ok, State#state{windows=Wins#{loose:=Loose#{Frame => Win}}}};
       Internal =/= false ->
	    Title = proplists:get_value(title, Ps),
	    Win = Win0#win{title=Title, ps=#{close=>true, move=>true}},
	    Ws = make_internal_win(Internal, Win, Wins),
	    {reply, ok, State#state{windows=Ws}};
       Geom -> %% Specialcase for geom window
	    Title = proplists:get_value(title, Ps),
	    #split{w1=Dummy} = Top,
	    Win1 = Win0#win{title=Title, ps=#{close=>false, move=>false}},
	    Win = make_internal_win(win(Top), Win1),
	    wxSplitterWindow:replaceWindow(win(Top), Dummy, win(Win)),
	    wxWindow:destroy(Dummy),
	    {reply, ok, State#state{windows=Wins#{ch:=Top#split{w1=Win}}}}
    end;

handle_call({close, Win}, _From, State) ->
    {reply, ok, close_win(Win, State)};

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
handle_cast({init_menus, Frame}, State) ->
    case os:type() of
	{_, darwin} ->
	    _MB = init_menubar(Frame),
	    %io:format("HO ~p~n", [wxMenuBar:findItem(MB, ?wxID_OSX_HIDEOTHERS)]),
	    %io:format("SA ~p~n", [wxMenuBar:findItem(MB, ?wxID_OSX_SHOWALL)]),
	    ok;
	_ -> ignore
    end,
    {noreply, State};

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
			_Pid  -> close_win(Win, State0)
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
    try
	#win{bar={PBar,_}} = find_win(Prev, Root),
	_ = wxWindow:getSize(PBar), %% Sync to check PBar validity
	wxWindow:setBackgroundColour(PBar, PBG),
	wxWindow:refresh(PBar)
    catch _:_ -> ignore
    end,
    try find_win(Name, Root) of
	false ->
	    State#state{active=undefined};
	#win{bar={ABar,_}} ->
	    wxWindow:setBackgroundColour(ABar, ABG),
	    wxWindow:refresh(ABar),
	    State#state{active=Name}
    catch _:_ ->
	    State
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
    [wxWindow:setBackgroundColour(WChild, WBG) || WChild <- WChildren],
    case Bar of
	{TBar,_} -> wxWindow:setBackgroundColour(TBar, TBG);
	_ -> ignore
    end.

preview_attach(false, Pos, Frame,
	       #state{windows=#{action:=Action}=Wins, overlay=Overlay}=State)
  when Action =:= undefined; Action =:= preview_attach ->
    case get_split_path(Pos, Wins) of
	ignore -> %% Outside attached window
	    overlay_hide(Overlay),
	    State#state{windows=delete_timer(Wins)};
	Path ->
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
    Flags = ?wxFRAME_TOOL_WINDOW bor
	?wxFRAME_FLOAT_ON_PARENT bor
	?wxFRAME_NO_TASKBAR bor
	?wxNO_BORDER,
    Overlay = wxFrame:new(Parent, -1, "", [{style, Flags}]),
    wxFrame:setBackgroundColour(Overlay, {95,138,255,200}),
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
    case maps:get(Frame, Loose, false) of
	false ->
	    State#{action:=undefined, op:=undefined};
	#win{win=Win}=Window ->
	    overlay_hide(Overlay),
	    %% io:format("Attach window ~p ~p ~p~n",[Frame,Path,wxWindow:getClientSize(Window)]),
	    St = State#{loose:=maps:remove(Frame,Loose), action:=undefined, op:=undefined},
	    DoWhileLocked = fun() -> attach_window(Path, Frame, Window, St) end,
	    After = fun() ->
			    timer:sleep(200),
			    wings_io:reset_video_mode_for_gl(0,0),
                            inform_parent_changed(Win)
		    end,
	    wings_io:lock(whereis(wings), DoWhileLocked, After)
    end;
attach_floating(_B, Overlay, State) ->
    %% Spurious Move events on windows
    overlay_hide(Overlay),
    State#{action:=undefined, op:=undefined}.

attach_window({_,Path}=Split, WinFrame, NewWin, #{frame:=TopFrame, szr:=Szr, ch:=Child} = State) ->
    Attach = fun() ->
		     {_,Pos} = preview_rect(Split, NewWin),
		     Win = make_internal_win(win(Child), NewWin),
		     wxWindow:destroy(WinFrame),
		     Root = split_win(Path, Win, Child, Pos),
		     case win(Root) =:= win(Child) of
			 false -> wxSizer:replace(Szr, win(Child), win(Root));
			 true  -> ignore
		     end,
		     %wxSizer:layout(Szr),
                     wxFrame:layout(TopFrame),
                     wxWindow:refresh(win(Root)),
		     check_tree(Root, Child),
		     State#{ch:=Root}
	     end,
    wings_io:batch(Attach).

split_win([Dir], NewWin, Node, Pos0) ->
    {Mode, Which} = split(Dir),
    Pos = pos_from_permille(Pos0, Mode, win(Node)),
    case Node of
	#split{mode=undefined, w1=Root} -> %% Top Window
	    set_splitter(Which, Mode, Pos, Node, Root, NewWin);
	_Leaf ->
	    Parent = wxWindow:getParent(win(Node)),
	    NewNode = make(Parent),
	    reparent(NewWin, NewNode),
	    reparent(Node, NewNode),
	    set_splitter(Which, Mode, Pos, NewNode, Node, NewWin)
    end;
split_win([Which|Path], NewWin, #split{mode=Mode} = Node, Pos) ->
    {Child0, Child1} = child(Which, Node),
    Child = split_win(Path, NewWin, Child0, Pos),
    wxSplitterWindow:replaceWindow(win(Node), win(Child0), win(Child)),
    set_splitter(Which, Mode, false, Node, Child1, Child).

make(Parent) ->
    Style = case os:type() of
		{unix, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
		{win32, _} -> ?wxSP_LIVE_UPDATE;
		_ -> ?wxSP_3D bor ?wxSP_LIVE_UPDATE
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

reparent(Child, #split{obj=Obj}) ->
    wxWindow:reparent(win(Child), Obj),
    Child.

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
    case find_win(Win, Tree) of
	false ->
	    case lists:keyfind(Win, #win.win, maps:values(Loose)) of
		#win{frame=Frame} = _Win ->
		    wxMiniFrame:destroy(Frame),
                    _ = wxWindow:findFocus(), %% Sync the destroy
		    State#state{windows=Wins#{loose:=maps:remove(Frame, Loose)}};
		false ->
		    %% wxWindow:destroy(Win),
		    State
	    end;
	#win{frame=Obj} ->
	    Close = fun(Where, Other, GrandP) -> close_window(Obj, Where, Other, GrandP, Szr) end,
	    {ok, Root} = update_win(Obj, Tree, Tree, Close),
	    check_tree(Root, Tree),
	    %% wxSizer:layout(Szr),
            wxFrame:layout(TopFrame),
            wxWindow:refresh(TopFrame),
	    State#state{windows=Wins#{ch:=Root}}
    end.

close_window(Delete, Split, Other, GrandP, Szr) ->
    case GrandP of
	Split when is_record(Other, win) -> %% TopLevel
	    wxWindow:reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:unsplit(win(GrandP), [{toRemove, Delete}]),
	    wxWindow:destroy(Delete),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {ok, GrandP#split{mode=undefined, w1=Other, w2=undefined}};
	Split when is_record(Other, split) ->
	    Frame = wxWindow:getParent(win(GrandP)),
	    wxWindow:reparent(win(Other), Frame),
	    wxSizer:replace(Szr, win(GrandP), win(Other)),
	    wxWindow:destroy(win(GrandP)),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {ok, Other};
	#split{} ->
	    wxWindow:reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
            _ = wxWindow:findFocus(), %% Sync the destroy
	    {ok, Other}
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
				      {{true, Res}, Root} ->
					  receive
					      #wx{obj=OldFrame, event=#wxShow{}} ->
						  {Res, Root}
					  end;
				      {{false, Res}, Root} ->
					  {Res, Root}
				  end
			  end,
	    {NewWin, Root} = wings_io:lock(whereis(wings), WhileLocked),
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
    #win{bar={Bar,_}} = Win = find_win(Frame, Top),
    Pos = wxWindow:clientToScreen(Bar, {X,Y}),
    State#{action:=detach_init, op:=#{win=>Win, pos=>Pos}};
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
    wxWindow:reparent(Container, Top),
    FrameW = make_external_win(Top, Child, PosSz, Label),
    Frame = Win#win{frame=FrameW, bar=undefined},
    wxWindow:hide(Container),
    Sizer = wxWindow:getSizer(Container),
    wxSizer:detach(Sizer, Child),
    case GrandP of
	top when is_record(Other, split) ->
	    wxWindow:reparent(win(Other), Top),
	    wxSizer:replace(Szr, win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
	    {{show_detached(Container), Frame}, Other};
	top ->
	    wxWindow:reparent(win(Other), win(Split)),
	    wxSplitterWindow:unsplit(win(Split), [{toRemove, Container}]),
	    {{show_detached(Container), Frame},
	     Split#split{mode=undefined, w1=Other, w2=undefined}};
	#split{} ->
	    wxWindow:reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
	    {{show_detached(Container), Frame}, Other}
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

make_external_win(Parent, Child, {X0,Y0, W, H} = _Dim, Label) ->
    Frame = make_win(Parent, Label, [{pos, {X0,Y0}}, {size, {W,H}}]),
    wxWindow:connect(Frame, move),
    wxWindow:connect(Frame, close_window),
    wxWindow:reparent(Child, Frame),
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

make_internal_win({Path, Pos}, NewWin, #{frame:=TopFrame, szr:=Szr, ch:=Child} = State) ->
    Win  = make_internal_win(win(Child), NewWin),
    Root = split_win(Path, Win, Child, {permille, Pos}),
    case win(Root) =:= win(Child) of
	false -> wxSizer:replace(Szr, win(Child), win(Root));
	true  -> ignore
    end,
    %% wxSizer:layout(Szr),
    wxFrame:layout(TopFrame),
    wxWindow:refresh(win(Root)),
    check_tree(Root, Child),
    State#{ch:=Root}.

-define(WIN_BAR_HEIGHT, 16).

make_internal_win(Parent, #win{title=Label, win=Child, ps=#{close:=Close, move:=Move}}=WinC) ->
    Win = wxPanel:new(Parent, []),
    PBG = wings_color:rgb4bv(wings_pref:get_value(title_passive_color)),
    {Bar,ST} = Wins = make_bar(Win, PBG, Label, Close),
    case os:type() of
	{win32, _} ->  wxWindow:setDoubleBuffered(Bar, true);
	_ -> ignore
    end,
    Top = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Top, Bar, [{proportion, 0}, {flag, ?wxEXPAND}, {border, 2}]),
    wxWindow:reparent(Child, Win), %% Send event here
    wxSizer:add(Top, Child,  [{proportion, 1}, {flag, ?wxEXPAND}, {border, 2}]),
    wxWindow:setSizer(Win, Top),
    wxWindow:connect(Bar, enter_window, [{userData, {win, Win}}]),
    case Move of
        true ->
            [wxWindow:connect(W, Ev, [{userData, {move, Win}}])
             || Ev <- [left_down, left_up, motion], W <- [Bar,ST]];
        false ->
            wxWindow:connect(Bar, left_down)
    end,
    WinC#win{frame=Win, bar=Wins}.

make_bar(Parent, BG, Label, Close) ->
    Bar = wxPanel:new(Parent, [{style, ?wxBORDER_SIMPLE}, {size, {-1, ?WIN_BAR_HEIGHT}}]),
    FG = wings_pref:get_value(title_text_color),
    #{size:=Sz} = FI = wings_text:get_font_info(?GET(system_font_wx)),
    {Font,Space} = case os:type() of
		       {unix, darwin} -> {wings_text:make_wxfont(FI#{size:=Sz-1}), 4};
		       _ -> {wings_text:make_wxfont(FI#{size:=Sz-2}), 2}
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
    Bitmap0 = wxArtProvider:getBitmap("wxART_CROSS_MARK",[{client, "wxART_MESSAGE_BOX"}]),
    Bitmap = case os:type() of
		{unix, linux} ->
		     Im0 = wxBitmap:convertToImage(Bitmap0),
		     Im1 = wxImage:scale(Im0,H,H,[{quality, ?wxIMAGE_QUALITY_HIGH}]),
		     BM = wxBitmap:new(Im1),
		     wxImage:destroy(Im1), wxImage:destroy(Im0),
		     BM;
		 {_, _} -> %% Do not scale on windows
		     Bitmap0
	     end,
    SBM = wxStaticBitmap:new(Bar, ?wxID_EXIT, Bitmap),
    %% io:format("SBM = ~p~n",[wxWindow:getSize(SBM)]),
    wxSizer:add(WBSz, SBM, [{flag, ?wxALIGN_CENTER}]),
    Self = self(),
    CB = fun(_, Ev) ->
		 wxMouseEvent:skip(Ev),
		 Self ! {close_window, Parent}
	 end,
    wxWindow:connect(SBM, left_up, [{callback, CB}]).

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
    catch _:Reason ->
	    io:format("Failed ~p ~p~n", [Reason, erlang:get_stacktrace()]),
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
    MB = wxMenuBar:new(),
    try
	wings_menu:setup_menus(MB, top_menus()),
	wxFrame:setMenuBar(Frame, MB),
	erase(wm_active),
	MB
    catch _ : Reason ->
	    io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()]),
	    error(Reason)
    end.
