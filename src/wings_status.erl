%%
%%  wings_status.erl --
%%
%%     Statusbar handling
%%
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_status).

-export([start_link/0,
         active/1, message/2, message/3, message_right/2,
         get_statusbar/0, update_theme/0]).

-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-record(state, {sb,
		frame,
		prev,
		msgs=gb_trees:empty()}).

-behaviour(wx_object).

start_link() ->
    Status = wx_object:start_link({local,?MODULE}, ?MODULE, [wings_frame:get_top_frame()], []),
    {ok, wx_object:get_pid(Status)}.

message(Win, Str) ->
    catch wx_object:cast(?MODULE, {message, Win, Str, undefined}).

message(Win, Left, Right) ->
    catch wx_object:cast(?MODULE, {message, Win, Left, Right}).

message_right(Win, Right) ->
    catch wx_object:cast(?MODULE, {message, Win, undefined, Right}).

active(Win) ->
    catch wx_object:cast(?MODULE, {active, Win}).

get_statusbar() ->
    wx_object:call(?MODULE, get_statusbar).

update_theme() ->
    wx_object:call(?MODULE, update_theme).

init([Frame]) ->
    try
	SB0 = wxStatusBar:new(Frame),
	SB = wx_object:set_pid(SB0, self()),
	SBG = wings_color:rgb4bv(wings_pref:get_value(info_line_bg)),
	SFG = wings_color:rgb4bv(wings_pref:get_value(info_line_text)),
	wxStatusBar:setBackgroundColour(SB, SBG),
	wxStatusBar:setForegroundColour(SB, SFG),
	wxStatusBar:setFieldsCount(SB, 2),
	wxStatusBar:setStatusWidths(SB, [-1, 500]),
	wxStatusBar:setStatusStyles(SB, [?wxSB_FLAT, ?wxSB_NORMAL]),
	wxFrame:setStatusBar(Frame, SB),
	wxFrame:setStatusBarPane(Frame, 0),
	%% this will "fix" the custom painting theme
    wxWindow:connect(SB, paint, [{callback, fun custom_draw/2}]),
	{SB, #state{sb=SB, frame=Frame}}
    catch _:Reason:ST ->
	    io:format("Error ~p ~p ~n",[Reason, ST]),
	    {error, Reason}
    end.

handle_event(_Ev, State) ->
    {noreply, State}.

handle_cast({message, Win, Left, Right}, #state{sb=SB, prev=Prev, msgs=GB0}=State) ->
    GB = case gb_trees:lookup(Win, GB0) of
	     none ->
		 gb_trees:insert(Win, {str(Left,""), str(Right,"")}, GB0);
	     {value, {OldL, OldR}} ->
		 gb_trees:update(Win, {str(Left, OldL), str(Right, OldR)}, GB0)
	 end,
    {noreply, State#state{msgs=GB, prev=update_status(gb_trees:lookup(Win, GB), Prev, SB)}};
handle_cast({active, Win}, #state{sb=SB, prev=Prev, msgs=GB}=State) ->
    {noreply, State#state{prev=update_status(gb_trees:lookup(Win, GB), Prev, SB)}}.

handle_call(update_theme, _From, #state{sb=SB}=State) ->
    SBG = wings_color:rgb4bv(wings_pref:get_value(info_line_bg)),
    SFG = wings_color:rgb4bv(wings_pref:get_value(info_line_text)),
    wxStatusBar:setBackgroundColour(SB, SBG),
    wxStatusBar:setForegroundColour(SB, SFG),
    wxStatusBar:refresh(SB),
    {reply, keep, State};
handle_call(get_statusbar, _From, #state{sb=SB}=State) ->
    {reply, SB, State};
handle_call(_Call, _From, State) ->
    %% io:format("Call ~p~n",[_Call]),
    {reply, keep, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, _, State) ->
    State.

terminate(_, _) ->
    ok.

custom_draw(#wx{obj=Obj, event=#wxPaint{}}, _) ->
    Size = wxWindow:getSize(Obj),
    DC = case os:type() of
             {win32, _} -> %% Flicker on windows
                 wx:typeCast(wxBufferedPaintDC:new(Obj), wxPaintDC);
             _ ->
                 wxPaintDC:new(Obj)
         end,
    wxDC:clear(DC),
    case wxStatusBar:getFieldsCount(Obj) of
        2 ->
            {true,{Xl,Yl,_,_Hl}} = wxStatusBar:getFieldRect(Obj, 0),
            {true,{Xr,Yr,_Wr,_Hr}} = wxStatusBar:getFieldRect(Obj, 1),
            wxDC:drawText(DC, wxStatusBar:getStatusText(Obj,[{number,0}]), {Xl+3,Yl+2}),
            wxDC:drawText(DC, wxStatusBar:getStatusText(Obj,[{number,1}]), {Xr+3,Yr+2});
        _ -> ok
    end,
    GrpPen = wxPen:new(wxSystemSettings:getColour(?wxSYS_COLOUR_3DSHADOW)),
    wxDC:setPen(DC,GrpPen),
    draw_grip(DC, Size, 3),
    wxPen:destroy(GrpPen),
    wxPaintDC:destroy(DC).

update_status({value, Prev}, Prev, _SB) ->
    Prev;
update_status(none, _, SB) ->
    set_status(none, SB);
update_status({value, Text = {_, Right}}, _Prev, SB) ->
    RSz = case Right of
	      "" -> 0;
	      _ ->
		  {WW, _} = wxWindow:getSize(SB),
		  {TW,_, _, _} = wxStatusBar:getTextExtent(SB, Right),
		  Extra = case os:type() of
			      {unix, darwin} -> 30;
			      {win32, nt} -> 40;
			      _ -> 10
			  end,
		  min(TW+Extra, WW div 2)
	  end,
    wxStatusBar:setStatusWidths(SB, [-1, RSz]),
    set_status(Text, SB).

set_status(none, SB) ->
    wxStatusBar:setStatusText(SB, "", [{number, 0}]),
    wxStatusBar:setStatusText(SB, "", [{number, 1}]),
    none;
set_status(Msgs={Left, Right}, SB) ->
    wxStatusBar:setStatusText(SB, Left,  [{number, 0}]),
    wxStatusBar:setStatusText(SB, Right, [{number, 1}]),
    Msgs.

draw_grip(_, _, 0) -> ok;
draw_grip(DC, {W,H}=Size, C) ->
    [wxDC:drawRectangle(DC, {W-(3*I)-1, H-(3*(4-C))-1}, {2,2}) || I <- lists:seq(C,1,-1)],
    draw_grip(DC, Size, C-1).

str(undefined, Old) -> Old;
str(New, _) -> str_clean(New).

str_clean([Char|Cs]) when is_integer(Char) ->
    [Char|str_clean(Cs)];
str_clean([List|Cs]) when is_list(List) ->
    [str_clean(List)|str_clean(Cs)];
str_clean([{bold,Str}|Cs]) ->
    [$*,str_clean(Str),$*|str_clean(Cs)];
str_clean([{_,Str}|Cs]) ->
    [str_clean(Str)|str_clean(Cs)];
str_clean([]) -> [].
