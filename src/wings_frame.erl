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

-export([top_menus/0]).

-export([start/1, forward_event/1]).

%% Internal
-behaviour(wx_object).
-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2]).

-define(NEED_ESDL, 1). %% event mapping
-include("wings.hrl").

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(File0) ->
    wx:new(),
    macosx_workaround(),
    Frame = wx_object:start_link({local, ?MODULE}, ?MODULE, [args], []),
    put(top_frame, Frame),
    wxGLCanvas:setCurrent(?GET(gl_canvas)),
    {Frame, start_file(File0)}.

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

forward_event({current_state, #st{selmode=Mode, sh=Sh}}) ->
    wx_object:cast(?MODULE, {selmode, Mode, Sh}),
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

%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_file(File0) ->
    %% On the Mac, if Wings was started by clicking on a .wings file,
    %% we must retrieve the name of the file here.
    Msgs0 = wxe_master:fetch_msgs(),
    Msgs = [F || F <- Msgs0, filelib:is_regular(F)],
    case Msgs of
	[F|_] -> F;
	[] -> File0
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {frame, toolbar}).

init(_Opts) ->
    try
	wings_pref:set_default(window_size, {780,570}),
	TopSize = wings_pref:get_value(window_size),
	Frame = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),
	?SET(top_frame, Frame),
	set_icon(),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Top = wxSplitterWindow:new(Frame),
	wxSizer:add(Sizer, Top, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:setSizeHints(Sizer, Top),
	wxFrame:setSizer(Frame, Sizer),
	Canvas = wings_init:create(Top),
	wxSplitterWindow:initialize(Top, Canvas),
	?SET(gl_canvas, Canvas),

	put(wm_active, {menubar, geom}),
	init_menubar(Frame),
	erase(wm_active),

	Icons = wings_io:read_icons(),
	Toolbar = wings_toolbar:init(Frame, Icons),

	wxWindow:connect(Frame, close_window),
	wxWindow:connect(Frame, command_menu_selected, []),
	wxWindow:connect(Frame, activate, []),
	wxWindow:connect(Frame, show),
	wxFrame:show(Frame),
	%% Must be shown to initialize OpenGL context.
	receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
	put(raw_icons, Icons), %% Do not want this in state (large crashdumps while debugging)
	{Frame, #state{frame=Frame, toolbar=Toolbar}}
    catch _:Reason ->
	    io:format("CRASH: ~p ~p ~p~n",[?MODULE, Reason, erlang:get_stacktrace()])
    end.

%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}, State) ->
    Name = wings_menu:id_to_name(Id),
    ME = case ets:match(wings_state, {{bindkey,'$1'}, Name, '_'}) of
	     [] -> {menubar, {action, Name}};
	     [[KeyComb]|_] -> wings_io_wx:make_key_event(KeyComb)
	 end,
    %% io:format("ME ~p~n",[ME]),
    wings ! ME,
    {noreply, State};
handle_event(#wx{event=#wxActivate{active=Active}}, State) ->
    Active == true andalso wxWindow:setFocus(?GET(gl_canvas)),
    wings ! #expose{active=Active},
    {noreply, State};
handle_event(#wx{event=#wxClose{}}, State) ->
    wings ! {quit},
    {noreply, State};
handle_event(Ev, State) ->
    io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(Req, _From, State) ->
    io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, Req]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
handle_cast({selmode, _, _}=Sel, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Sel, TB)}};
handle_cast({mode_restriction, _}=Restrict, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Restrict, TB)}};
handle_cast({menu, {Menu, Key, Value}=Update}, #state{toolbar=TB}=State) ->
    wings_menu:update_menu_enabled(Menu, Key, Value),
    wings_toolbar:update(Update, TB),
    {noreply, State};
handle_cast({got_focus, _Window, Props}, #state{toolbar=TB}=State) ->
    Fun = fun(Menu, Key, Value) ->
		  wings_menu:update_menu_enabled(Menu, Key, Value),
		  wings_toolbar:update({Menu, Key, Value}, TB)
	  end,
    [Fun(view, Key, Value) || {Key, Value} <- Props, is_boolean(Value)],
    {noreply, State};
handle_cast(Req, State) ->
    io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, Req]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_info(Msg, State) ->
    io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{frame=Frame}) ->
    catch wxFrame:destroy(Frame),
    normal.

%%%%%%%%%%%%%%%%%%%%%%

set_icon() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Ebin, "wings_icon_379x379"),
    wings_io:set_icon(IconFile).

macosx_workaround() ->
    try 1.0/zero()
    catch
	error:_ -> ok
    end.

zero() ->
    0.0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menubar

init_menubar(Frame) ->
    ets:new(wings_menus, [named_table, {keypos,2}]),
    put(wm_active, {menubar, geom}),
    MB = wxMenuBar:new(),
    try
	wings_menu:setup_menus(MB, top_menus()),
	wxFrame:setMenuBar(Frame, MB),
	ok
    catch _ : Reason ->
	    io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()]),
	    error(Reason)
    end,
    erase(wm_active),
    ok.
