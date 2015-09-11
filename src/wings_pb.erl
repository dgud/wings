%%
%%  wings_pb.erl --
%%
%%     This module contains a progress bar
%%
%%  Copyright (c) 2004-2011 Dan Gudmundsson and Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_pb).
-include("wings.hrl").

-export([start/1,update/1,update/2,pause/0,
	 done/0,done/1,done_stat/0,done_stat/1,
	 cancel/0]).

-export([start_link/1, init/1,
	 handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-behaviour(wx_object).
-define(PB, progress_bar).
-define(PH, 20).

start(Msg) when is_list(Msg) ->
    case get(wings_not_running) of
	undefined ->  wx_object:cast(?PB,{start,Msg,percent});
	_ -> ignore
    end.

update(Percent) when is_float(Percent) ->
    wx_object:cast(?PB, {update,"",Percent}).

update(Percent, Msg) when is_list(Msg), is_float(Percent) ->
    wx_object:cast(?PB,{update,Msg,Percent}).

pause() ->
    wx_object:call(?PB,pause).

done() ->
    wx_object:call(?PB,done).

done(Ret) ->
    done(),
    Ret.

done_stat() ->
    Stat = done(),
    Stat().

done_stat(Ret) ->
    Stat = done(),
    Stat(),
    Ret.

cancel() ->
    wx_object:call(?PB,cancel).

%% Helpers

%%%%%%%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(REFRESH_T, 200).			%Refresh interval.

-record(state,
	{ refresh=infinity,
	  level=0,
	  msg=[],
	  pos=0.0,
	  next_pos=0.0,
	  t0,
	  offset,
	  scale,
	  stats=[],
	  frame,
	  pb
	}).

start_link(Frame) ->
    wx_object:start_link({local, ?PB}, ?MODULE, [Frame], []).

init([Frame]) ->
    Dummy = wxPanel:new(Frame),
    wxPanel:hide(Dummy),
    {Dummy, #state{frame=Frame}}.

handle_event(_Ev, State) ->
    io:format("PB ~p~n",[_Ev]),
    {noreply, State}.

handle_cast({start, Msg, percent}, #state{frame=Frame, level=Level})
  when Level =:= 0 ->
    SB = wxFrame:getStatusBar(Frame),
    {X,Y} = wxWindow:getPosition(SB),
    {W,_H} = wxWindow:getSize(SB),
    Pos  = {pos, {X,Y-?PH-1}},
    Size = {size, {W,?PH}},
    PB = wxGauge:new(Frame, ?wxID_ANY, 100, [Pos, Size]),
    S = #state{msg=["",Msg], t0=os:timestamp(),
	       refresh=?REFRESH_T, level=1,
	       pb=PB, frame=Frame},
    wxGauge:show(PB),
    wxGauge:raise(PB),
    draw_position(S),
    {noreply, S, ?REFRESH_T};
handle_cast({start, Msg, percent}, #state{level=Level,next_pos=Next,
					  pos=Pos,msg=Msg0,
					  refresh=Ref
					 } = S0) ->
    Scale = Next-Pos,
    S = S0#state{level=Level+1,msg=[Msg|Msg0], scale=Scale,offset=Pos},
    {noreply, S, Ref};

handle_cast({update, Curr, Percent}, S0 = #state{level=Level})
  when Level =:= 1 ->
    S1 = update(Curr, Percent, S0),
    S  = calc_position(S1),
    draw_position(S),
    {noreply, S#state{refresh=?REFRESH_T}, ?REFRESH_T};
handle_cast({update, Curr, Percent}, S0 = #state{refresh=Refresh}) ->
    S1 = update(Curr, S0#state.offset+Percent*S0#state.scale, S0),
    S = calc_position(S1),
    {noreply, S, Refresh};

handle_cast(Cast, State) ->
    io:format("Cast ~p~n",[Cast]),
    {noreply, State}.

handle_call(done, _From,  #state{pb=PB, frame=Frame, level=Level} = S0)
  when Level =:= 1 ->
    S = update(?__(1,"done"), 1.0, S0#state{next_pos=1.0,pos=1.0}),
    draw_position(S),
    wxGauge:destroy(PB),
    {reply, fun() -> print_stats(S) end, #state{frame=Frame}};
handle_call(done, _From,  #state{msg=[]} = S0) ->
    {reply, ok, S0};
handle_call(done, _From,  #state{level=Level, msg=[_|Msg], refresh=Refresh} = S0) ->
    {reply, ok, S0#state{level=Level-1,msg=Msg}, Refresh};

handle_call(cancel, _From, #state{frame=Frame, pb=PB}) ->
    {reply, ok, #state{frame=Frame, pb=PB}};

handle_call(pause, _From, S0) ->
    S = draw_position(calc_position(S0)),
    {reply, ok, S#state{refresh=infinity}}.

handle_info(timeout, S0=#state{refresh=Refresh}) ->
    S = draw_position(calc_position(S0)),
    {noreply, S, Refresh};

handle_info(Cast, State) ->
    io:format("Info ~p~n",[Cast]),
    {noreply, State}.

code_change(_, _, State) ->
    State.

terminate(_, _) ->
    ok.

draw_position(#state{pb=PB, msg=Msg, pos=Pos} = S) ->
    wings_status:message(?PB, build_msg(Msg)),
    PB =:= undefined orelse wxGauge:setValue(PB, trunc(Pos * 100)),
    S.

update(Msg, Percent, #state{msg=[_|Msg0],stats=Stats0,t0=Time0}=S) ->
    NowDiff = timer:now_diff(os:timestamp(), Time0),
    Stats = [Percent,NowDiff|Stats0],
    S#state{msg=[Msg|Msg0],next_pos=Percent,stats=Stats}.

calc_position(#state{pos=Pos0,next_pos=NextPos}=S) when Pos0 < NextPos ->
    Pos = Pos0 + (NextPos - Pos0) / 5,
    S#state{pos=Pos};
calc_position(S) ->
    %% Do nothing if the position has not advanced. This is not really
    %% supposed to happen, but can happen if the position has reached
    %% 1.0 because of limited floating point precision.
    S.

print_stats(#state{t0=Time0,stats=[_|Stats0]}) ->
    Total = timer:now_diff(os:timestamp(), Time0),
    [_|Stats] = lists:reverse(Stats0),
    io:nl(),
    print_stats_1(Stats, Total).

print_stats_1([Est,TimeDiff|T], Total) ->
    io:format("Est: ~f Real: ~f\n",
	      [Est,TimeDiff/Total]),
    print_stats_1(T, Total);
print_stats_1([], _) -> ok.

build_msg([M]) -> M;
build_msg([[]|T]) -> build_msg(T);
build_msg([H|T]) -> build_msg(T) ++ ": " ++ H.
