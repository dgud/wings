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

-export([start/1, active/1, message/2, message/3, message_right/2]).

-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-record(state, {sb,
		frame,
		prev,
		msgs=gb_trees:empty()}).

-behaviour(wx_object).

start(Frame) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, [Frame], []).

message(Win, Str) ->
    wx_object:cast(?MODULE, {message, Win, Str, undefined}).

message(Win, Left, Right) ->
    wx_object:cast(?MODULE, {message, Win, Left, Right}).

message_right(Win, Right) ->
    wx_object:cast(?MODULE, {message, Win, undefined, Right}).

active(Win) ->
    wx_object:cast(?MODULE, {active, Win}).

init([Frame]) ->
    try
	SB = wxStatusBar:new(Frame),
	wxStatusBar:setFieldsCount(SB, 2),
	wxStatusBar:setStatusWidths(SB, [-1, 500]),
	wxStatusBar:setStatusStyles(SB, [?wxSB_FLAT, ?wxSB_NORMAL]),
	wxFrame:setStatusBar(Frame, SB),
	wxFrame:setStatusBarPane(Frame, 0),
	{SB, #state{sb=SB, frame=Frame}}
    catch _:Reason ->
	    io:format("Error ~p ~p ~n",[Reason, erlang:get_stacktrace()]),
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

handle_call(_Call, _From, State) ->
    %% io:format("Call ~p~n",[_Call]),
    {reply, keep, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, _, State) ->
    State.

terminate(_, _) ->
    ok.

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
		  min(TW+Extra, WW div 3)
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

str(undefined, Old) -> Old;
str(New, _) -> wings_menu:str_clean(New).
