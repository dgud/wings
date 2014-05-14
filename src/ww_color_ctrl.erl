
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
%%
%% Option palette = {InitPalette = fun() -> [Col0,,Col16] end,
%%                   UpdatePalette = fun([Col0,,Col16]) -> ... end}
%%
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
	{this, bitmap, brush,
	 current, def,
	 bg,
	 init_palette, update_palette,
	 handlers=[]  %% Listeners or callbacks
	}).

init([Parent, Id, O0]) ->
    Bitmap = wxBitmap:new(60,13),
    {Style, O1} = default(style, ?wxCLRP_DEFAULT_STYLE, O0),
    {DefColor, O2} = default(col, {0.0,0.0,0.0,1.0}, O1),
    {{GetPalette, SetPalette}, Opts} = default(palette, {undefined, undefined}, O2),

    Button = wxBitmapButton:new(Parent, Id, Bitmap,
				[{style, ?wxBU_AUTODRAW bor Style}|Opts]),
    wxBitmapButton:connect(Button, command_button_clicked),

    State = #state{this=Button, bitmap=Bitmap,
		   brush=wxBrush:new(rgb256(DefColor)),
		   def = DefColor, current = DefColor,
		   init_palette=GetPalette, update_palette=SetPalette},
    update_color(State),
    {Button, State}.

handle_event(#wx{event=#wxCommand{type=command_button_clicked}},
	     #state{this=This, brush=Brush, init_palette=Set} = State) ->
    Data = wxColourData:new(),
    wxColourData:setColour(Data, wxBrush:getColour(Brush)),
    wxColourData:setChooseFull(Data, true),
    set_palette(Set, Data),
    Dlg = wxColourDialog:new(This, [{data, Data}]),
    wxColourData:destroy(Data),
    case wxDialog:showModal(Dlg) of
	?wxID_CANCEL ->
	    wxColourDialog:destroy(Dlg),
	    {noreply, State};
	?wxID_OK ->
	    NewData = wxColourDialog:getColourData(Dlg),
	    Updated = update_state(NewData, State),
	    update_color(Updated),
	    wxColourDialog:destroy(Dlg),
	    #state{current=Col, handlers=Handlers} = Updated,
	    [apply_callback(H, Col, true) || H <- Handlers],
	    {noreply, Updated}
    end.

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

handle_cast({set_color, Col}, State0 = #state{brush=Old, handlers=Handlers}) ->
    wxBrush:destroy(Old),
    Brush = wxBrush:new(rgb256(Col)),
    State = State0#state{current=Col, brush=Brush},
    update_color(State),
    [apply_callback(H, Col, false) || H <- Handlers],
    {noreply, State};

handle_cast(_, State) -> State.
handle_info(_, State) -> State.

terminate(_Reason, #state{this=_This, brush=Brush}) ->
    wxBrush:destroy(Brush),
    %% wxBitmapButton:destroy(This), %% Is destroyed by the owner
    ok.

code_change(_, _, State) -> State.

default(Key, Def, Opts) ->
    {proplists:get_value(Key, Opts, Def),
     proplists:delete(Key,Opts)}.
update_state(Data, #state{brush=Old, update_palette=Update, def=Def} = State) ->
    Col = wxColourData:getColour(Data),
    Palette = get_palette(Data, 0),
    wxBrush:destroy(Old),
    case Update of
	undefined ->
	    State#state{brush=wxBrush:new(Col), current=rgb(Col, Def),
			init_palette=fun() -> Palette end};
	_Fun when is_function(Update) ->
	    Update(Palette),
	    State#state{current=rgb(Col, Def), brush=wxBrush:new(Col)}
    end.

update_color(#state{this=This, brush=FG, bitmap=BM}) ->
    DC = wxMemoryDC:new(BM),
    wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
    wxDC:setBrush(DC, FG),
    wxDC:drawRectangle(DC, {0,0, wxBitmap:getWidth(BM), wxBitmap:getHeight(BM)}),
    wxMemoryDC:selectObject(DC, ?wxNullBitmap),
    wxMemoryDC:destroy(DC),
    wxBitmapButton:setBitmapLabel(This, BM),
    ok.


set_palette(undefined, Data) ->
    DefPalette = [{G,G,G} || G <- lists:seq(0, 255, 255 div 15)],
    set_palette(DefPalette, Data, 0);
set_palette(Get, Data) when is_function(Get) ->
    set_palette(Get(), Data, 0).

set_palette([Col|Pal], Data, I) when I < 16 ->
    wxColourData:setCustomColour(Data, I, rgb256(Col)),
    set_palette(Pal, Data, I+1);
set_palette(_, _, _) -> ok.

get_palette(Data, I) when I < 16 ->
    [rgb(wxColourData:getCustomColour(Data,I), {r,g,b})|get_palette(Data, I+1)];
get_palette(_, _) -> [].

rgb256({R,G,B}) -> {round(R*255),round(G*255),round(B*255)};
rgb256({R,G,B,_A}) -> {round(R*255),round(G*255),round(B*255)}.

rgb({R,G,B,A}, {_, _, _, _}) -> {R/255, G/255, B/255, A/255};
rgb({R,G,B,_A}, {_, _, _}) -> {R/255, G/255, B/255}.

apply_callback({Pid, Changed}, Col, UserSet)
  when is_pid(Pid), UserSet orelse Changed ->
    Pid ! {col_changed, Col};
apply_callback({CB, Changed}, Col, UserSet)
  when is_function(CB), UserSet orelse Changed ->
    CB({col_changed, Col});
apply_callback(_, _, _) -> ok.


