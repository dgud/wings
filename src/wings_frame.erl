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

-export([start/0]).

%% Internal
-behaviour(wx_object).
-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2]).

-define(NEED_ESDL, 1). %% event mapping
-include("wings.hrl").

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    wx:new(),
    macosx_workaround(),
    Frame = wx_object:start_link({local, ?MODULE}, ?MODULE, [args], []),
    put(top_frame, Frame),
    wxGLCanvas:setCurrent(?GET(gl_canvas)),
    %% On the Mac, if Wings was started by clicking on a .wings file,
    %% we must retrieve the name of the file here.
    Msgs0 = wxe_master:fetch_msgs(),
    Msgs = [F || F <- Msgs0, filelib:is_regular(F)],
    case Msgs of
	[F|_] -> F;
	[] -> none
    end.

%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {frame}).

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

	wxWindow:connect(Frame, close_window),
	wxWindow:connect(Frame, command_menu_selected, []),
	wxWindow:connect(Frame, activate, []),
	wxWindow:connect(Frame, show),
	wxFrame:show(Frame),
	%% Must be shown to initilize OpenGL context.
	receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
	{Frame, #state{frame=Frame}}
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

