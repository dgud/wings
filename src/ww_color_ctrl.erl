
%%
%%  ww_color_ctrl.erl --
%%
%%     This module create a variant to wxColorPickerCtrl
%%     so we can override the default color chooser and let the user choose
%%     which color picker (s)he wants
%%
%%     Also handles the color palette better than the orig color_picker
%%
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(ww_color_ctrl).
-behaviour(wx_object).
%% Callbacks
-export([init/1, terminate/2, code_change/3,
	 handle_event/2, handle_cast/2, handle_info/2,
	 handle_call/3]).

%% API
-export([new/3,
	 getColor/1, setColor/2,
	 connect/2, connect/3
	]).

new(Parent, Id, Opts) ->
    wx_object:start(?MODULE, [Parent, Id, Opts], []).

getColor(Ctrl) ->
    wx_object:call(Ctrl, get_color).

setColor(Ctrl, Col) ->
    wx_object:cast(Ctrl, {set_color, Col}).

connect(Ctrl, Msg) ->
    connect(Ctrl, Msg, []).

connect(Ctrl, col_changed, Opts) ->
    wx_object:call(Ctrl, {connect, [{col_changed, true}|Opts]});
connect(Ctrl, col_user_set, Opts) ->
    wx_object:call(Ctrl, {connect, Opts});
connect(Ctrl, What, Opts) ->
    wxBitmapButton:connect(Ctrl, What, Opts).

%% Callbacks

-include_lib("wx/include/wx.hrl").

-record(state,
	{this, bitmap, brush, static,
	 current, def, dialog,
	 bg,
	 handlers=[]  %% Listeners or callbacks
	}).

init([Parent, Id, O0]) ->
    Bitmap = wxBitmap:new(60,13),
    {Style, O1} = default(style, ?wxCLRP_DEFAULT_STYLE, O0),
    {DefColor, O2} = default(col, {0.0,0.0,0.0,1.0}, O1),
    {Static, O3} = default(static, false, O2),
    {Dialog, Opts} = default(native_dialog, wings_pref:get_value(color_dialog_native), O3),
    Button = wxBitmapButton:new(Parent, Id, Bitmap,
				[{style, ?wxBU_AUTODRAW bor Style}|Opts]),
    wxBitmapButton:connect(Button, command_button_clicked),

    State = #state{this=Button, bitmap=Bitmap, dialog=Dialog,
		   brush=wxBrush:new(wings_color:rgb3bv(DefColor)),
		   def=DefColor, current=DefColor, static=Static},
    update_color(State),
    {Button, State}.

handle_event(#wx{event=#wxCommand{type=command_button_clicked}},
	     #state{static=true} = State) ->
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_button_clicked}},
	     #state{current=Current, dialog=Dialog} = State) ->
    Res = fun(Res) -> self() ! {color_changed, Res}, ignore end,
    wings_color:choose(Current, Res, Dialog),
    {noreply, State}.

handle_call(get_color, _From, #state{current=Curr} = State) ->
    {reply, Curr, State};
handle_call({connect, Opts}, From, #state{handlers=Curr} = State) ->
    All = proplists:get_value(col_changed, Opts, false),
    case proplists:get_value(callback, Opts) of
	undefined ->
	    {reply, ok, State#state{handlers=[{From, All}|Curr]}};
	CB when is_function(CB) ->
	    {reply, ok, State#state{handlers=[{CB, All}|Curr]}};
	Bad ->
	    {reply, {error, {badarg, Bad}}, State}
    end.

handle_cast({set_color, Col}, State0 = #state{brush=Old}) ->
    wxBrush:destroy(Old),
    Brush = wxBrush:new(wings_color:rgb3bv(Col)),
    State = State0#state{current=Col, brush=Brush},
    update_color(State),
    {noreply, State};

handle_cast(_, State) -> State.

handle_info({color_changed, RGB}, State) ->
    Updated = update_state(RGB, State),
    update_color(Updated),
    #state{current=Col, handlers=Handlers} = Updated,
    [apply_callback(H, Col, true) || H <- Handlers],
    {noreply, Updated};

handle_info(_, State) -> State.

terminate(_Reason, #state{this=_This, brush=Brush}) ->
    wxBrush:destroy(Brush),
    %% wxBitmapButton:destroy(This), %% Is destroyed by the owner
    ok.

code_change(_, _, State) -> State.

default(Key, Def, Opts) ->
    {proplists:get_value(Key, Opts, Def),
     proplists:delete(Key,Opts)}.

update_state(RGB, #state{brush=Old} = State) ->
    wxBrush:destroy(Old),
    State#state{brush=wxBrush:new(wings_color:rgb3bv(RGB)), current=RGB}.

update_color(#state{this=This, brush=FG, bitmap=BM}) ->
    DC = wxMemoryDC:new(BM),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBrush(DC, FG),
    wxDC:drawRectangle(DC, {0,0, wxBitmap:getWidth(BM), wxBitmap:getHeight(BM)}),
    wxMemoryDC:selectObject(DC, ?wxNullBitmap),
    wxMemoryDC:destroy(DC),
    wxBitmapButton:setBitmapLabel(This, BM),
    ok.

apply_callback({Pid, Changed}, Col, UserSet)
  when is_pid(Pid), UserSet orelse Changed ->
    Pid ! {col_changed, Col};
apply_callback({CB, Changed}, Col, UserSet)
  when is_function(CB), UserSet orelse Changed ->
    CB({col_changed, Col});
apply_callback(_, _, _) -> ok.


