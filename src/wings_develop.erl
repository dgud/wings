%%
%%  wings_develop.erl --
%%
%%     This module implements a Develop menu with useful functions
%%     for Wings and plug-in developers.
%%
%%  Copyright (c) 2009-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_develop).
-export([init/0,menu/1,command/2,
	 time_command/2,gl_error_check/1]).

-include("wings.hrl").

init() ->
    [wings_pref:set_default(Key, false) ||
	Key <- [develop_time_commands,
		develop_undo_stat,
		develop_gl_errors]],
    ok.

menu(_) ->
    [{"Time Commands",time_commands,
      "Print each command's execution time to the console",
      wings_menu_util:crossmark(develop_time_commands)},
     {"Undo Stat",undo_stat,
      "Show statistics for how much memory each Undo state consumes",
      wings_menu_util:crossmark(develop_undo_stat)},
     {"OpenGL Errors",opengl_errors,
      "Print information about OpenGL errors to the console",
      wings_menu_util:crossmark(develop_gl_errors)},
     separator,
     {"Print Scene Size",print_scene_size,
      "Print the scene size to the console"},
     separator,
     {"Loaded Font Glyphs",font_libs},
     {"Show Cursor",show_cursor,
      "Unhide the cursor in case of a crash and it disappears"}].

command(time_commands, _) ->
    toggle(develop_time_commands),
    keep;
command(undo_stat, St) ->
    case toggle(develop_undo_stat) of
	true ->
	    wings_undo:mem_stat_help(),
	    wings_undo:save(St, St);		%Nothing will be saved.
	false -> ok
    end,
    keep;
command(opengl_errors, _) ->
    toggle(develop_gl_errors),
    keep;
command(print_scene_size, St) ->
    Words = erts_debug:size(St),
    io:format("The current scene is using ~p words\n", [Words]),
    keep;
command(font_libs, _) ->
    Sf = length(ets:tab2list(system_font)),
    Cf = length(ets:tab2list(console_font)),
    Str =io_lib:format("system_font glyphs loaded: ~p\n console_font glyphs loaded: ~p\n",[Sf,Cf]),
    wings_u:message(Str);
command(show_cursor, _) ->
    case wings_io:is_grabbed() of
      true ->
          {_,X,Y} = wings_io:get_mouse_state(),
          wings_io:ungrab(X,Y);
      false -> ok
    end,
    keep.

time_command(CmdFun, Cmd) ->
    case wings_pref:get_value(develop_time_commands, false) of
	false ->
	    Res = CmdFun(),
	    gl_error_check(Cmd),
	    Res;
	true ->
	    Before = os:timestamp(),
	    Res = CmdFun(),
	    After = os:timestamp(),
	    Time = timer:now_diff(After, Before),
	    Str = format_time(Time),
	    case Res of
		next ->
		    ok;
		_ ->
		    io:format("~14s  ~s\n",
			      [Str,wings_util:stringify(Cmd)])
	    end,
	    gl_error_check(Cmd),
	    Res
    end.

gl_error_check(Cmd) ->
    case wings_pref:get_value(develop_gl_errors) of
	false -> ok;
	true -> gl_error_check_1(Cmd)
    end.

%%%
%%% Internal functions.
%%%

gl_error_check_1(Cmd0) ->
    case gl:getError() of
	0 -> ok;
	ErrorCode ->
	    Error = wings_gl:error_string(ErrorCode),
	    Cmd = if is_list(Cmd0) -> Cmd0;
		     true -> wings_util:stringify(Cmd0)
		  end,
	    io:format("~s caused OpenGL error ~s\n", [Cmd,Error])
    end.

toggle(Key) ->
    OldVal = wings_pref:get_value(Key),
    NewVal = not OldVal,
    wings_pref:set_value(Key, NewVal),
    NewVal.

format_time(Ms) ->
    MsStr = integer_to_list(Ms rem 1000000) ++ "us",
    case Ms div 1000000 of
	0 ->
	    MsStr;
	Sec ->
	    integer_to_list(Sec) ++ "s " ++ MsStr
    end.
