%%
%%  wings_pb.erl --
%%
%%     This module contains a progress bar
%%
%%  Copyright (c) 2004 Dan Gudmundsson and Bjorn Gustavsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pb.erl,v 1.17 2004/12/14 20:29:39 bjorng Exp $
%%

-module(wings_pb).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-export([start/1,update/1,update/2,pause/0,
	 done/0,done/1,done_stat/0,done_stat/1,
	 cancel/0]).

-export([init/0,loop/1]).

-define(PB, progress_bar).

start(Msg) when is_list(Msg) ->
    WinInfo = wings_wm:viewport(message),
    cast({start,Msg,percent,WinInfo}).

update(Percent) when is_float(Percent) -> 
    cast({update,"",Percent}).

update(Percent, Msg) when is_list(Msg), is_float(Percent) -> 
    cast({update,Msg,Percent}).

pause() ->
    call(pause).

done() ->
    call(done).

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
    call(cancel).

%% Helpers

call(What) ->
    try ?PB ! {self(),?PB,What} of
	_Any ->
	    receive
		{?PB,Res} -> Res
	    end
    catch
	error:badarg -> ok
    end.

cast(What) ->
    try ?PB ! {?PB,What} of
	_Any -> ok
    catch
	error:badarg -> ok
    end.

reply(Pid, What) ->
    Pid ! {?PB,What},
    ok.
	   
%%%%%%%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(REFRESH_T, 200).			%Refresh interval.

-record(state,
	{refresh=infinity,
	 level=0,
	 msg=[], 
	 pos=0.0,
	 next_pos=0.0,
	 t0,
	 offset,
	 scale,
	 stats=[]
	}).

%% Start progressbar process
init() ->
    case wings_pref:get_value(no_progress_bar) of
	true ->
	    cast(terminate),
	    sent_termination_request;
	false ->
	    case whereis(?PB) of
		undefined ->
		    Pid = spawn_link(?MODULE, loop, [#state{}]),
		    register(?PB, Pid),
		    started;
		_ -> already_started
	    end
    end.

loop(#state{refresh=After,level=Level,msg=Msg0}=S0) ->
    receive
	{?PB,terminate} ->
	    exit(normal);
	{Pid,?PB,cancel} ->
	    reply(Pid, ok),
	    loop(#state{});
	{?PB,{start,Msg,_Data,{X,Y,W,H}}} when Level =:= 0 ->
	    S = #state{refresh=?REFRESH_T,level=1,
		       msg=["",Msg],t0=now()},
	    put(wm_viewport, {X,Y,W-17,H}),
	    loop(draw_position(S));
	{?PB,{update,Msg,Time}} when Level =:= 1 ->
	    S1 = update(Msg, Time, S0),
	    S = calc_position(S1),
	    loop(draw_position(S#state{refresh=?REFRESH_T}));
	{Pid,?PB,done} when Level =:= 1 ->
	    S = update(?__(1,"done"), 1.0, S0#state{next_pos=1.0,pos=1.0}),
	    draw_position(S),
	    reply(Pid, fun() -> print_stats(S) end),
	    loop(#state{});
	{Pid,?PB,pause} ->
	    S = draw_position(calc_position(S0)),
	    reply(Pid, ok),
	    loop(S#state{refresh=infinity});
	{?PB,{start,Msg,_Data,_}} ->
	    #state{next_pos=Next,pos=Pos} = S0,
	    Scale = Next-Pos,
	    S = S0#state{level=Level+1,msg=[Msg|Msg0],
			 scale=Scale,offset=Pos},
	    loop(S);
	{?PB,{update,Msg,Time}} ->
	    S1 = update(Msg, S0#state.offset+Time*S0#state.scale, S0),
	    S = calc_position(S1),
	    loop(S);
	{Pid,?PB,done} ->
	    reply(Pid, ok),
	    case Msg0 of
		[] ->
		    loop(S0);
		[_|Msg] ->
		    loop(S0#state{level=Level-1,msg=Msg})
	    end;
	Msg ->
	    io:format("~p: got unexpected msg ~p~n", [?MODULE, Msg]),
	    loop(S0)
    after After ->
	    S = calc_position(S0),
	    loop(draw_position(S))
    end.

update(Msg, Percent, #state{msg=[_|Msg0],stats=Stats0,t0=Time0}=S) ->
    NowDiff = now_diff(now(), Time0),
    Stats = [Percent,NowDiff|Stats0],
    S#state{msg=[Msg|Msg0],next_pos=Percent,stats=Stats}.

calc_position(#state{pos=Pos0,next_pos=NextPos}=S) when Pos0 < NextPos ->
    Pos = Pos0 + (NextPos - Pos0) / 5,
    S#state{pos=Pos}.

print_stats(#state{t0=Time0,stats=[_|Stats0]}) ->
    Total = now_diff(now(), Time0),
    [_|Stats] = lists:reverse(Stats0),
    io:nl(),
    print_stats_1(Stats, Total).

print_stats_1([Est,TimeDiff|T], Total) ->
    io:format("Est: ~f Real: ~f\n",
	      [Est,TimeDiff/Total]),
    print_stats_1(T, Total);
print_stats_1([], _) -> ok.

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

%% Draw Progress Bar 

draw_position(#state{t0=T0}=S) ->
    case now_diff(now(), T0) of
	Diff when Diff < 500000 -> ok;
	_Diff -> draw_position_1(S)
    end,
    S.
	    
draw_position_1(#state{msg=Msg,pos=Pos}=S) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    {X,Y,W,H} = get(wm_viewport),
    gl:viewport(X, Y, W, H),
    gl:drawBuffer(?GL_FRONT),

    wings_io:ortho_setup(),
    wings_io:set_color(?PANE_COLOR),
    gl:recti(0, 0, W, H),

    BarLen = trunc((W-4)*Pos),
    double_gradient(4, 2, BarLen, W, ?LINE_HEIGHT),
    wings_io:set_color(wings_pref:get_value(info_color)),
    wings_io:text_at(6, ?CHAR_HEIGHT, build_msg(Msg)),

    gl:finish(),
    gl:drawBuffer(?GL_BACK),
    gl:popAttrib(),

    %%io:format("~p ~s: ~.3f ~w~n", [time(), Msg, Pos,S]),    
    S.

double_gradient(X, Y, BarW, W, H) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),

    gl:color3f(1, 1, 1),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:vertex2f(X+W, Y+H),
    gl:vertex2f(X, Y+H),

    gl:color3f(0.5, 0.73, 1),
    gl:vertex2f(X+BarW, Y+H div 2),
    gl:vertex2f(X, Y+H div 2),

    gl:color3f(0.66, 0.83, 1),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+BarW, Y),

    gl:color3f(0.62, 0.78, 1),
    gl:vertex2f(X+BarW, Y+H),
    gl:vertex2f(X, Y+H),

    gl:color3f(0.5, 0.73, 1),
    gl:vertex2f(X, Y+H div 2),
    gl:vertex2f(X+BarW, Y+H div 2),
    gl:'end'(),
    gl:shadeModel(?GL_FLAT).

build_msg([M]) -> M;
build_msg([[]|T]) -> build_msg(T);
build_msg([H|T]) -> build_msg(T) ++ ": " ++ H.
