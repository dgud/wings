%%
%%  wings_develop.erl --
%%
%%     This module implements a Develop menu with useful functions
%%     for Wings and plug-in developers.
%%
%%  Copyright (c) 2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_develop).
-export([menu/1,command/2,time_command/3]).

-include("wings.hrl").

menu(_) ->
    [{"Time Commands",time_commands,
      "Print each command's execution time to the console",
      crossmark(develop_time_commands)},
     {"Undo Stat",undo_stat,
      "Show statistics for how much memory each Undo state consumes",
      crossmark(develop_undo_stat)}].

command(time_commands, _) ->
    wings_pref:toggle_value(develop_time_commands),
    keep;
command(undo_stat, St) ->
    case wings_pref:toggle_value(develop_undo_stat) of
	true ->
	    wings_undo:mem_stat_help(),
	    wings_undo:save(St, St);		%Nothing will be saved.
	false -> ok
    end,
    keep.

time_command(CmdFun, Cmd, St) ->
    case wings_pref:get_value(develop_time_commands) of
	false ->
	    CmdFun(Cmd, St);
	true ->
	    Before = erlang:now(),
	    Res = CmdFun(Cmd, St),
	    After = erlang:now(),
	    Time = timer:now_diff(After, Before),
	    Str = format_time(Time),
	    case Res of
		next ->
		    ok;
		_ ->
		    io:format("~14s  ~s\n",
			      [Str,wings_util:stringify(Cmd)])
	    end,
	    Res
    end.

%%%
%%% Internal functions.
%%%

crossmark(Key) ->
    case wings_pref:get_value(Key) of
	false -> [];
	true -> [crossmark]
    end.

format_time(Ms) ->
    MsStr = integer_to_list(Ms rem 1000000) ++ "us",
    case Ms div 1000000 of
	0 ->
	    MsStr;
	Sec ->
	    integer_to_list(Sec) ++ "s " ++ MsStr
    end.
