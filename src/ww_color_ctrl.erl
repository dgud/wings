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
-export([new/3, getColor/1]).
%%
%% Option palette = {InitPalette = fun() -> [Col0,,Col16] end,
%%                   UpdatePalette = fun([Col0,,Col16]) -> ... end}
%%
new(Parent, Id, Opts) ->
    wx_object:start(?MODULE, [Parent, Id, Opts], []).

getColor(Ctrl) ->
    wx_object:call(Ctrl, get_color).

%% Callbacks

-include_lib("wx/include/wx.hrl").

-record(state, {this, bitmap, brush, bg, init_palette, update_palette}).

init([Parent, Id, O0]) ->
    Bitmap = wxBitmap:new(60,13),
    {Style, O1} = default(style, ?wxCLRP_DEFAULT_STYLE, O0),
    {DefColor, O2} = default(col, {0,0,0,0}, O1),
    {{GetPalette, SetPalette}, Opts} = default(palette, {undefined, undefined}, O2),

    Button = wxBitmapButton:new(Parent, Id, Bitmap,
				[{style, ?wxBU_AUTODRAW bor Style}|Opts]),
    wxBitmapButton:connect(Button, command_button_clicked),

    State = #state{this=Button, bitmap=Bitmap,
		   brush=wxBrush:new(DefColor),
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
	    wxBrush:destroy(Brush),
	    wxColourDialog:destroy(Dlg),
	    {noreply, Updated}
    end.

handle_call(get_color, _From, #state{brush=Brush} = State) ->
    {reply, wxBrush:getColour(Brush), State}.

terminate(_Reason, #state{this=_This, brush=Brush}) ->
    wxBrush:destroy(Brush),
    %% wxBitmapButton:destroy(This), %% Is destroyed by the owner
    ok.

handle_cast(_, State) -> State.
handle_info(_, State) -> State.

code_change(_, _, State) -> State.

default(Key, Def, Opts) ->
    {proplists:get_value(Key, Opts, Def),
     proplists:delete(Key,Opts)}.
update_state(Data, #state{update_palette=Update} = State) ->
    Col = wxColourData:getColour(Data),
    Palette = get_palette(Data, 0),
    case Update of
	undefined ->
	    State#state{brush=wxBrush:new(Col), init_palette=fun() -> Palette end};
	_Fun when is_function(Update) ->
	    Update(Palette),
	    State#state{brush=wxBrush:new(Col)}
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
    wxColourData:setCustomColour(Data, I, Col),
    set_palette(Pal, Data, I+1);
set_palette(_, _, _) -> ok.

get_palette(Data, I) when I < 16 ->
    [wxColourData:getCustomColour(Data,I)|get_palette(Data, I+1)];
get_palette(_, _) -> [].

