%%
%%  wings_u.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_u.erl,v 1.4 2005/06/04 05:53:21 dgud Exp $
%%

-module(wings_u).
-export([error/1,error/2,message/1,get_matrices/2,
	 geom_windows/0,menu_restriction/2,
	 yes_no/2,yes_no/3,yes_no_cancel/3,
	 export_we/2,win_crash/1,crash_log/2,crash_log/3,
	 caption/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [member/2,foreach/2]).

error(Message) when is_list(Message) ->
    wings_pb:cancel(),
    throw({command_error,Message}).

error(Format, Arg) ->
    error(lists:flatten(io_lib:format(Format, Arg))).

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
	  [{label,Question,[{break,45}]},
	   {hframe,[{button,wings_s:yes(),yes_no_fun(Yes)},
		    {button,wings_s:no(),yes_no_fun(No),[cancel]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

yes_no_cancel(Question, Yes, No) ->
    Qs = {vframe,
	  [{label,Question,[{break,45}]},
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
    end,
    analyse(F, StackTrace),
    file:close(F),
    LogName.

caption(#st{file=undefined}=St) ->
    Caption = wings_util:wings(),
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{saved=true,file=Name}=St) ->
    Caption = wings_util:wings() ++ " - " ++ filename:basename(Name),
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{saved=auto,file=Name}=St) ->
    Caption = wings_util:wings() ++ " - " ++ filename:basename(Name) ++
	"* [" ++ ?__(1,"auto-saved") ++ "]",
    sdl_video:wm_setCaption(Caption, Caption),
    St;
caption(#st{file=Name}=St) ->
    Caption = wings_util:wings() ++ " - " ++ filename:basename(Name) ++ "*",
    sdl_video:wm_setCaption(Caption, Caption),
    St.

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

log_file_dir({unix,_}) -> os:getenv("HOME");
log_file_dir({win32,_}) ->
    Root = code:root_dir(),
    case filelib:is_file("Wings3D.exe") of
        true -> Root;
        false ->
            %% Development system.
            case code:which(?MODULE) of
                Name0 when is_list(Name0) ->
                    Name = filename:dirname(Name0),
                    case filename:basename(Name) of
                        "ebin" -> filename:dirname(Name);
                        "patches" -> filename:dirname(Name);
                        _Other -> Name
                    end
            end
    end.
	
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
try_arg(F, {I,_}=GbTree, N) when integer(I) ->
    case catch gb_trees:to_list(GbTree) of
	{'EXIT',_} -> ok;
	[{_,#edge{}}|_]=Es ->
	    arg(F, N),
	    dump_edges(F, Es);
	_ -> ok
    end;
try_arg(_, _, _) -> ok.

arg(F, N) ->
    io:format(F, "Argument #~p:\n", [N]).

dump_shape(F, #we{}=We) ->
    dump_we(F, We).

dump_we(F, #we{name=Name,id=Id,mode=Mode,es=Etab,fs=Ftab,
	       next_id=Next}) ->
    io:put_chars(F, "\n"),
    io:format(F, "OBJECT ~p: ~p\n", [Id,Name]),
    io:format(F, "=======================\n", []),
    io:format(F, "   mode=~p next_id=~p\n", [Mode,Next]),
    dump_faces(F, gb_trees:to_list(Ftab)),
    dump_edges(F, gb_trees:to_list(Etab)).
    
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

show_edge(F, Edge, #edge{vs=Vs,ve=Ve,a=A,b=B,lf=Lf,rf=Rf,ltpr=Lpred,ltsu=Lsucc,
			 rtpr=Rpred,rtsu=Rsucc}) ->
    io:format(F, "~p: vs=~p ve=~p\n", [Edge,Vs,Ve]),
    io:format(F, "  a=~p b=~p\n", [A,B]),
    io:format(F, "  left: face=~p pred=~p succ=~p\n", [Lf,Lpred,Lsucc]),
    io:format(F, "  right: face=~p pred=~p succ=~p\n", [Rf,Rpred,Rsucc]).

show_face(F, Face, Edge) ->
    io:format(F, "~p: edge=~p\n", [Face,Edge]).
