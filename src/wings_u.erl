%%
%%  wings_u.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_u).
-export([error_msg/1,error_msg/2,message/1,debug/1,
	 get_matrices/2, geom_windows/0,
	 yes_no/2,yes_no/3,yes_no_cancel/3,
	 win_crash/1,win_crash/2,crash_log/2,crash_log/3,
	 pretty_filename/1,relative_path_name/2,caption/1,
         win32_special_folder/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [member/2,foreach/2,all/2]).

-spec error_msg([any()]) -> no_return().

error_msg(Message) when is_list(Message) ->
    wings_pb:cancel(),
    throw({command_error,Message}).

-spec error_msg([any()], [any()]) -> no_return().

error_msg(Format, Arg) ->
    error_msg(lists:flatten(io_lib:format(Format, Arg))).

message(Message) ->
    Qs = {vframe_dialog,
	  [{label,Message}],
	  [{buttons,[ok]}]},
    wings_dialog:dialog("", Qs, fun(_) -> ignore end).

geom_windows() ->
    geom_windows_1(wings_wm:windows()).

get_matrices(Id, MM) ->
    {TPM, TMV0, _} = wings_view:load_matrices(false),
    TMV = case MM of
	      mirror ->
		  Matrix = wings_dl:mirror_matrix(Id),
		  e3d_mat:mul(e3d_transform:matrix(TMV0),Matrix);
	      original -> e3d_transform:matrix(TMV0)
	  end,
    {_,_,W,H} =  wings_wm:viewport(),
    {TMV,e3d_transform:matrix(TPM),{0,0,W,H}}.

yes_no(Question, Yes) ->
    yes_no(Question, Yes, ignore).

yes_no(Question, Yes, No) ->
    Qs = {vframe_dialog,
	  [{label,Question,[{break,75*?CHAR_WIDTH}]}],
	  [{buttons, [yes, no]}, {key, result}, {position, mouse}]
	 },
    wings_dialog:dialog("", Qs, fun([{result,Res}]) -> yes_no_cancel(Res, Yes, No, ignore) end).

yes_no_cancel(Question, Yes, No) ->
    Qs = {vframe_dialog,
	  [{label,Question,[{break,75*?CHAR_WIDTH}]}],
	  [{buttons, [yes, no, cancel]}, {key, result}, {position, mouse}]
	 },
    wings_dialog:dialog("", Qs, fun([{result,Res}]) -> yes_no_cancel(Res, Yes, No, ignore) end).

win_crash(Reason) ->
    win_crash(wings_wm:this(), Reason).

win_crash(Window, Reason) ->
    LogName = crash_log(Window, Reason),
    wings_wm:send(geom, {crash_in_other_window,LogName}).

crash_log(WinName, Reason) ->
    StackTrace = erlang:get_stacktrace(),
    crash_log(WinName, Reason, StackTrace).

crash_log(WinName, Reason, StackTrace) ->
    catch wings_pb:cancel(),
    LogFileDir = log_file_dir(),
    LogName = filename:absname("wings_crash.dump", LogFileDir),
    F = open_log_file(LogName),
    io:format("Internal Error~n",[]),
    [io:format(Fd, "Version: ~s\n", [?WINGS_VERSION]) || Fd <- [F, group_leader()]],
    try
        OsDesc = wx_misc:getOsDescription(),
        {GLVend, GLRend} = {gl:getString(?GL_VENDOR), gl:getString(?GL_RENDERER)},
	[io:format(Fd, "OS: ~ts\n", [OsDesc])  || Fd <- [F, group_leader()]],
	[io:format(Fd, "GPU: ~ts | ~ts\n",[GLVend, GLRend])  || Fd <- [F, group_leader()]]
    catch
	_:_ -> ignore
    end,
    [io:format(Fd, "Window: ~p\n", [WinName])  || Fd <- [F, group_leader()]],
    [io:format(Fd, "Reason: ~p\n\n", [Reason]) || Fd <- [F, group_leader()]],
    report_stacktrace(F, StackTrace),
    analyse(F, StackTrace),
    file:close(F),
    LogName.

report_stacktrace(F, [_|_]=StackTrace) ->
    ShortStackTrace = [{M,N,if
				is_list(A) -> length(A);
				true -> A
			    end, FL} || {M,N,A,FL} <- StackTrace],
    format(group_leader(), "Stack trace:\n~P\n\n", [StackTrace, 20]),
    case ShortStackTrace =:= StackTrace of
	false ->
	    format(F, "Short stack trace:\n~p\n\n", [ShortStackTrace]),
	    format(F, "Long stack trace:\n~P\n\n", [StackTrace, 50]);
	true ->
	    format(F, "Stack trace:\n~p\n\n", [StackTrace])
    end;
report_stacktrace(_, []) ->
    %% Make sure we don't write anything if there is no stacktracke.
    %% (There will be no stacktrace if we were called from the
    %% process that runs wings:halt_loop/1.)
    ok.

format(To,F,A) ->
    %% Format in this process to avoid sending 'St' to another process
    io:put_chars(To, unicode:characters_to_binary(io_lib:format(F,A))).

caption(#st{file=undefined}=St) ->
    Caption = wings(),
    wings_io:set_title(Caption),
    St;
caption(#st{saved=true,file=Name}=St) ->
    Caption = wings() ++ " - " ++ Name,
    wings_io:set_title(Caption),
    St;
caption(#st{saved=auto,file=Name}=St) ->
    Caption = wings() ++ " - " ++ Name ++
	"* [" ++ ?__(1,"auto-saved") ++ "]",
    wings_io:set_title(Caption),
    St;
caption(#st{file=Name}=St) ->
    Caption = wings() ++ " - " ++ Name ++ "*",
    wings_io:set_title(Caption),
    St.

%% pretty_filename(Name) -> PrettyName
%% This function tried to handle utf8 conversions
%% but now filename may be full Unicode lists.
pretty_filename(Name0) ->
    Name0.

%% it returns a new file name using relative path
%% Dst: it's the relative folder (used to be the project directory)
%% Src: file name (with full path) to be referenced
%% it was implemented to be used in the exporters (see wpc_pov.erl)
relative_path_name(Dst,Src) ->
    Fn=filename:basename(Src),
    Dir0=filename:split(get_dir(Dst)),
    Dir1=filename:split(get_dir(Src)),
    case get_rel_path(Dir0,Dir1) of
        [] -> Fn;
        Dir3 -> filename:join(Dir3++[Fn])
    end.

get_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> Dir;
        _ -> filename:dirname(Dir)
    end.

get_rel_path([D|_]=Dst,[D|_]=Src) ->    % paths are in the same drive - check relative path
    get_rel_path(Dst,Src,[]);
get_rel_path(_,Src) -> Src.   % paths are in a different Drive - ignore routine

get_rel_path([]=_Dst, []=_Src, Acc) -> Acc;     % export and file dir are the same
get_rel_path([_|T], [], Acc) ->             % file is in some dir level in the Dst
    get_rel_path(T,[],["../"]++Acc);
get_rel_path([], [H|T], Acc)->              % all previous dir match - file is in uppper dir level
    get_rel_path([],T,[H]++Acc);
get_rel_path([H|T0], [H|T1], Acc) ->        % continue checking in next dir level
    get_rel_path(T0,T1,Acc);
get_rel_path(Dst, Src, _Acc) ->             % no more compatible subdir and file is in some down dir level
    get_rel_path_0(Dst,Src).

get_rel_path_0([], Acc) -> Acc;
get_rel_path_0([_|T], Acc) ->
    get_rel_path_0(T, ["../"]++Acc).


wings() ->
    case wings_branch() of
	"" -> debug("Wings3D");
	_ -> "Wings3D " ++ ?wings_version ++ " (" ++ ?wings_branch ++ ")"
    end.

wings_branch() ->
    %% Fool dialyzer
    case is_process_alive(self()) of
        true -> {branch, ?wings_branch};
        false -> ""
    end.

-ifdef(DEBUG).
debug(Caption) -> Caption ++ " [debug]".
-else.
debug(Caption) -> Caption.
-endif.

win32_special_folder(R, FolderType) ->
    Key = "\\hkcu\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders",
    case win32reg:change_key(R, Key) of
	ok ->
	    case win32reg:value(R, FolderType) of
		{error,_} -> none;
		{ok,Value} -> Value
	    end;
	_ -> error
    end.

%%%
%%% Local functions.
%%%

yes_no_cancel(ok,  Yes, _, _) -> invoke(Yes);
yes_no_cancel(yes, Yes, _, _) -> invoke(Yes);
yes_no_cancel(no, _, No, _)   -> invoke(No);
yes_no_cancel(cancel, _, _, Cancel)   -> invoke(Cancel).

invoke(Fun) when is_function(Fun) -> Fun();
invoke(ignore) -> ignore.

%% yes_no_fun(Fun) ->
%%     This = wings_wm:this(),
%%     fun(_) ->
%% 	    case Fun() of
%% 		ignore -> ignore;
%% 		Action -> wings_wm:send(This, {action,Action})
%% 	    end
%%     end.

geom_windows_1([geom|T]) ->
    [geom|geom_windows_1(T)];
geom_windows_1([{geom,_}=Name|T]) ->
    [Name|geom_windows_1(T)];
geom_windows_1([_|T]) ->
    geom_windows_1(T);
geom_windows_1([]) -> [].

log_file_dir() ->
    case catch log_file_dir(os:type()) of
	Dir when is_list(Dir) -> Dir;
	_Other -> "."
    end.

log_file_dir({unix,_}) ->
    os:getenv("HOME");
log_file_dir({win32,_}) ->
    {ok,R} = win32reg:open([read]),
    Res = case win32_special_folder(R, "Desktop") of
	      none ->
		  {ok,[Drive,$:|_]} = file:get_cwd(),
		  [Drive,$:,$/];
	      Path -> Path
	  end,
    ok = win32reg:close(R),
    Res.

open_log_file(Name) ->
    {ok,F} = file:open(Name, [write]),
    {{Y,Mo,D},{H,Mi,_}} = erlang:localtime(),
    io:format(F, "Dump written ~p-~p-~p_~p-~p\n", [Y,Mo,D,H,Mi]),
    F.

analyse(F, [{_Mod,_Fun,Args,_Loc}|_]) when is_list(Args) ->
    try_args(F, Args, 1);
analyse(_, _) -> ok.

try_args(F, [A|As], Num) ->
    try_arg(F, A, Num),
    try_args(F, As, Num+1);
try_args(_, _, _) -> ok.

try_arg(F, #st{}=St, N) ->
    arg(F, N),
    Dump = fun(Obj, _) -> dump_object(F, Obj, St) end,
    _ = wings_obj:fold(Dump, [], St);
try_arg(F, #we{}=We, N) ->
    arg(F, N),
    dump_we(F, We);
try_arg(F, Tab, N) ->
    try array:sparse_to_orddict(Tab) of
	[{_,#edge{}}|_]=Es ->
	    arg(F, N),
	    dump_edges(F, Es);
	_ ->
	    ok
    catch _:_ ->
	    ok
    end.

arg(F, N) ->
    io:format(F, "Argument #~p:\n", [N]).

dump_object(F, #{id:=Id,name:=Name}, St) ->
    io:put_chars(F, "\n"),
    io:format(F, "OBJECT ~p: ~p\n", [Id,Name]),
    io:format(F, "=======================\n", []),
    wings_obj:with_we(fun(We) -> dump_we(F, We) end, Id, St).

dump_we(F, #we{es=Etab,fs=Ftab,next_id=Next}) ->
    io:format(F, "   next_id=~p\n", [Next]),
    dump_faces(F, gb_trees:to_list(Ftab)),
    dump_edges(F, array:sparse_to_orddict(Etab)).

dump_edges(F, Es) ->
    io:put_chars(F, "\n"),
    io:format(F, "Edge table\n", []),
    io:format(F, "===========\n\n", []),
    foreach(fun({Edge,Erec}) -> show_edge(F, Edge, Erec) end, Es).

dump_faces(F, Fs) ->
    io:put_chars(F, "\n"),
    io:format(F, "Face table\n", []),
    io:format(F, "===========\n\n", []),
    foreach(fun({Face,Frec}) -> show_face(F, Face, Frec) end, Fs).


%%
%% Dumping of data structures.
%%

show_edge(F, Edge, #edge{vs=Vs,ve=Ve,lf=Lf,rf=Rf,ltpr=Lpred,ltsu=Lsucc,
			 rtpr=Rpred,rtsu=Rsucc}) ->
    io:format(F, "~p: vs=~p ve=~p\n", [Edge,Vs,Ve]),
    io:format(F, "  left: face=~p pred=~p succ=~p\n", [Lf,Lpred,Lsucc]),
    io:format(F, "  right: face=~p pred=~p succ=~p\n", [Rf,Rpred,Rsucc]).

show_face(F, Face, Edge) ->
    io:format(F, "~p: edge=~p\n", [Face,Edge]).
