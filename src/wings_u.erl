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
-export([error_msg/1,error_msg/2,message/1,get_matrices/2,
	 geom_windows/0,menu_restriction/2,
	 yes_no/2,yes_no/3,yes_no_cancel/3,
	 export_we/2,win_crash/1,crash_log/2,crash_log/3,
	 pretty_filename/1,caption/1,win32_special_folder/2]).

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
    Qs = {vframe,
	  [{label,Message},
	   {hframe,[{button,ok,[ok]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

menu_restriction(Win, Allowed) ->
    case wings_wm:get_menubar(Win) of
	none -> wings_wm:menubar(Win, []);
	Mb0 ->
	    Mb = [Item || {_,Name,_}=Item <- Mb0, member(Name, Allowed)],
	    wings_wm:menubar(Win, Mb)
    end.

geom_windows() ->
    geom_windows_1(wings_wm:windows()).

get_matrices(Id, MM) ->
    wings_view:load_matrices(false),
    case MM of
	mirror ->
	    Matrix = wings_dl:mirror_matrix(Id),
	    gl:multMatrixf(Matrix);
	original -> ok
    end,
    {_,_,W,H} =  wings_wm:viewport(),
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    {ModelMatrix,ProjMatrix,{0,0,W,H}}.

yes_no(Question, Yes) ->
    yes_no(Question, Yes, ignore).

yes_no(Question, Yes, No) ->
    Qs = {vframe,
	  [{label,Question,[{break,45*?CHAR_WIDTH}]},
	   {hframe,[{button,wings_s:yes(),yes_no_fun(Yes)},
		    {button,wings_s:no(),yes_no_fun(No),[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

yes_no_cancel(Question, Yes, No) ->
    Qs = {vframe,
	  [{label,Question,[{break,45*?CHAR_WIDTH}]},
	   {hframe,[{button,wings_s:yes(),yes_no_fun(Yes)},
		    {button,wings_s:no(),yes_no_fun(No)},
		    {button,wings_s:cancel(),
		     yes_no_fun(ignore),[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

%% export_we(Filename, State)
%%  Dump the winged-edge structure in a textual format.
export_we(Name, #st{shapes=Shs}) ->
    case file:open(Name, [write,delayed_write]) of
	{ok,F} ->
	    foreach(fun(We) -> dump_we(F, We) end, gb_trees:values(Shs)),
	    file:close(F);
	{error,_}=Error ->
	    Error
    end.

win_crash(Reason) ->
    LogName = crash_log(wings_wm:this(), Reason),
    wings_wm:send(geom, {crash_in_other_window,LogName}).

crash_log(WinName, Reason) ->
    StackTrace = erlang:get_stacktrace(),
    crash_log(WinName, Reason, StackTrace).

crash_log(WinName, Reason, StackTrace) ->
    wings_pb:cancel(),
    LogFileDir = log_file_dir(),
    LogName = filename:absname("wings_crash.dump", LogFileDir),
    F = open_log_file(LogName),
    io:format(F, "Version: ~s\n", [?WINGS_VERSION]),
    io:format(F, "Window: ~p\n", [WinName]),
    io:format(F, "Reason: ~p\n\n", [Reason]),
    report_stacktrace(F, StackTrace),
    analyse(F, StackTrace),
    file:close(F),
    LogName.

report_stacktrace(F, [_|_]=StackTrace) ->
    ShortStackTrace = [{M,N,if
				is_list(A) -> length(A);
				true -> A
			    end} || {M,N,A} <- StackTrace],
    case ShortStackTrace =:= StackTrace of
	false ->
	    io:format(F, "Short stack trace:\n~p\n\n", [ShortStackTrace]),
	    io:format(F, "Long stack trace:\n~p\n\n", [StackTrace]);
	true ->
	    io:format(F, "Stack trace:\n~p\n\n", [StackTrace])
    end;
report_stacktrace(_, []) ->
    %% Make sure we don't write anything if there is no stacktracke.
    %% (There will be no stacktrace if we were called from the
    %% process that runs wings:halt_loop/1.)
    ok.

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
%%  If the Name *seems* to be encoded in UTF8, convert to a
%%  list of Unicode characters. Leave it alone otherwise.
%%
pretty_filename(Name0) ->
    case all(fun(C) -> C < 128 end, Name0) of
	true ->
	    %% Only 7-bit ASCII characters. Already fine.
	    Name0;
	false ->
	    %% Try to convert it from UTF8 to a list of Unicode characters.
	    Name1 = list_to_binary(Name0),
	    case unicode:characters_to_list(Name1, utf8) of
		Name when is_list(Name) -> unicode_combine(Name);
		_ -> Name0
	    end
    end.


wings() ->
    case ?wings_branch of
	"" -> debug("Wings3D");
	_ -> "Wings3D " ++ ?wings_version ++ " (" ++ ?wings_branch ++ ")"
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

yes_no_fun(ignore) -> fun(_) -> ignore end;
yes_no_fun(Fun) ->
    This = wings_wm:this(),
    fun(_) ->
	    case Fun() of
		ignore -> ignore;
		Action -> wings_wm:send(This, {action,Action})
	    end
    end.

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

analyse(F, [{_Mod,_Fun,Args}|_]) when is_list(Args) ->
    try_args(F, Args, 1);
analyse(_, _) -> ok.

try_args(F, [A|As], Num) ->
    try_arg(F, A, Num),
    try_args(F, As, Num+1);
try_args(_, _, _) -> ok.

try_arg(F, #st{shapes=Shapes}, N) ->
    arg(F, N),
    foreach(fun({Id,Sh}) ->
		    io:format(F, "Shape ~p\n", [Id]),
		    dump_shape(F, Sh)
	    end, gb_trees:to_list(Shapes));
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

dump_shape(F, #we{}=We) ->
    dump_we(F, We).

dump_we(F, #we{name=Name,id=Id,es=Etab,fs=Ftab,
	       next_id=Next}) ->
    io:put_chars(F, "\n"),
    io:format(F, "OBJECT ~p: ~p\n", [Id,Name]),
    io:format(F, "=======================\n", []),
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


%% unicode_combine(String0) -> String
%%  Combine diacritical characters with the previous character.
%%  (We need to do this because Mac OS X encodes filenames in
%%  this way. Sigh!)
%%
%%  For instance, the characters 'LATIN CAPITAL LETTER A' (U+0041) and
%%  'COMBINING DIAERESIS' (U+0308) combines to form the letter
%%  'LATIN CAPITAL LETTER A WITH DIAERESIS' (U+00C4).
%%
%%  Currently, we only handle a few of the possible character combinations,
%%  namely those that occur in Swedish/Finnish, German, and Russian.
%%
unicode_combine([$A,16#0308|T]) ->
    [16#C4|unicode_combine(T)];			%"Ä"
unicode_combine([$a,16#0308|T]) ->
    [16#E4|unicode_combine(T)];			%"ä"
unicode_combine([$A,16#030A|T]) ->
    [16#C5|unicode_combine(T)];			%"Å"
unicode_combine([$a,16#030A|T]) ->
    [16#E5|unicode_combine(T)];			%"å"
unicode_combine([$O,16#0308|T]) ->
    [16#D6|unicode_combine(T)];			%"Ö"
unicode_combine([$o,16#0308|T]) ->
    [16#F6|unicode_combine(T)];			%"ö"
unicode_combine([$U,16#0308|T]) ->
    [16#DC|unicode_combine(T)];			%"Ü"
unicode_combine([$u,16#0308|T]) ->
    [16#FC|unicode_combine(T)];			%"ü"
%% Russian.
unicode_combine([16#0415,16#0308|T]) ->
    [16#0401|unicode_combine(T)];		%"Ё"
unicode_combine([16#0435,16#0308|T]) ->
    [16#0451|unicode_combine(T)];		%"ё"
unicode_combine([16#0418,16#0306|T]) ->
    [16#0419|unicode_combine(T)];		%"Й"
unicode_combine([16#0438,16#0306|T]) ->
    [16#0439|unicode_combine(T)];		%"й"
unicode_combine([H|T]) ->
    [H|unicode_combine(T)];
unicode_combine([]) -> [].
