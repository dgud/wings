%%
%%  wings_console.erl --
%%
%%     Console for Wings.
%%
%%  Copyright (c) 2004-2011 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_console).

%% I/O server and console server
-export([start_link/1,init/2,get_pid/0,stop/0,stop/1,
	 setopts/1,getopts/1]).

%% Also duplicates as event_handler for process crashes
-export([init/1, handle_event/2, handle_info/2]).

%% Wings window
-export([window/0,window/4,popup_window/0]).

-define(SERVER_NAME, ?MODULE).
-define(WIN_NAME, console).
-define(DIRTY_TIME, 250).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1]).

%% Debug exports
-export([code_change/0,get_state/0]).

%% Internal exports
-export([do_code_change/3]).

%% Slim Event handler for error logging and forwarding events to wings process.
init(_Type) ->
    {ok, #{state=>normal}}.

handle_event({info_report,_,{_,progress,_}}, St) ->
    {ok, St};
handle_event(_, #{state:=shutdown}=St) ->
    {ok, St};
handle_event({error_report,_GL,{_,supervisor_report,Report}}, #{state:=normal}=St) ->
    Reason = proplists:get_value(reason, Report),
    Context = proplists:get_value(errorContext, Report),
    if Reason =:= shutdown  -> {ok, #{state=>shutdown}};
       Reason =:= normal    -> {ok, #{state=>shutdown}};
       Context =:= shutdown -> {ok, #{state=>shutdown}};
       Context =:= child_terminated -> {ok, #{state=>shutdown}};
       true ->
            Off = proplists:get_value(offender, Report),
            case proplists:get_value(restart_type, Off) of
                permanent ->
                    log_error(Off, Reason, St);
                _Other ->
                    Pid = proplists:get_value(pid, Off),
                    wings ! {'EXIT', Pid, Reason},
                    log_error(Off, Reason, St)
            end
    end;
handle_event({error_report,_GL,{_Pid,crash_report,[Report,[]]}}, #{state:=normal}=St) ->
    Error = proplists:get_value(error_info, Report),
    log_error(Report, Error, St);
handle_event({error_report,_GL,{Pid,crash_report,Report}}, #{state:=normal}=St) ->
    log_error(Pid, Report, St),
    {ok, St#{error=>Pid}};
handle_event({_Type, _GL, _Msg}, State) ->
    %% io:format("~p:~p:~p ~p ~p~n", [?MODULE, ?LINE, State, _Msg, Type]),
    {ok, State}.

handle_info(_, State)  ->
    {ok, State}.

log_error(_Off, _, #{error:=_} = St) ->
    %% Already wrote one crash dump
    {ok, St};
log_error(Off, {exit, {Reason, Stacktrace}, [_|_]}, St) ->
    log_error(Off, {Reason, Stacktrace}, St);
log_error(Off, {Reason, [_|_]=Stacktrace}, St) ->
    {Pid, Who} = who(Off),
    LogName = wings_u:crash_log(Who, Reason, Stacktrace),
    catch wings_wm:psend(geom, {crash_in_other_window,LogName}),
    {ok, St#{error=>Pid}};
log_error(Off, Reason, St) ->
    {Pid, Who} = who(Off),
    LogName = wings_u:crash_log(Who, Reason, []),
    catch wings_wm:psend(geom, {crash_in_other_window,LogName}),
    {ok, St#{error=>Pid}}.

who(Pid) when is_pid(Pid) ->
    {Pid, Pid};
who([_|_]=PL) ->
    Pid = proplists:get_value(pid, PL),
    Id  = proplists:get_value(id, PL, undefined),
    Name = proplists:get_value(registered_name, PL, []),
    {Mod,_,_} = proplists:get_value(initial_call, PL, undefined),
    Info = case {Id, Name, Mod} of
               {undefined, [], undefined} -> Pid;
               {undefined, [], Mod} -> [Mod, Pid];
               {undefined, Name, undefined} -> [Name, Pid];
               {undefined, Name, Mod} -> [Name, Mod, Pid];
               {Id, [], Mod} -> [Id, Mod, Pid];
               {Id, Name, Mod} -> [Id, Name, Mod, Pid]
           end,
    {Pid, Info}.

%%% I/O server state record ---------------------------------------------------

-record(state, {gmon,			% Monitor ref of original group leader
		group_leader,		% pid()
		win,
		ctrl,
		save_lines=200,		% -"-
		cnt=1,			% Queued lines incl last
		lines=queue:new(),	% Queue of binaries, head is oldest
		last = <<>>             % Last line without eol
	       }).

-define(STATE, {state,Gmon,GroupLeader,Win,Ctrl,SaveLines,Cnt,Lines,Last}).

%%% API -----------------------------------------------------------------------

start_link(Env) ->
    GroupLeader = group_leader(),
    proc_lib:start_link(?MODULE, init, [Env, GroupLeader]).

init(Env, GroupLeader) ->
    process_flag(trap_exit, true),
    error_logger:add_report_handler(?MODULE),
    case catch register(?SERVER_NAME, self()) of
        true ->
            wx:set_env(Env),
            Gmon = erlang:monitor(process, GroupLeader),
            group_leader(self(), whereis(wings_sup)),
            proc_lib:init_ack({ok, self()}),
            server_loop(#state{gmon=Gmon, group_leader=GroupLeader});
        _ ->
            exit(already_started)
    end.

get_pid() ->
    case whereis(?SERVER_NAME) of
	Server when is_pid(Server) ->
	    Server;
	undefined ->
	    exit(not_started)
    end.

stop() -> req({stop,shutdown}).
stop(Reason) -> req({stop,Reason}).

setopts(Opts) when is_list(Opts) -> req({setopts,Opts}).

getopts(Opts) when is_list(Opts) -> req({getopts,Opts}).

window() ->
    popup_window().

window(Name, Pos, Size, Ps) ->
    do_window(Name, [{pos,Pos},{size,Size}|Ps]).

popup_window() ->
    case wings_wm:is_window(?WIN_NAME) of
	true ->
	    wings_wm:show(?WIN_NAME);
	false ->
	    do_window(?WIN_NAME, [])
    end.

%%%
%%% Debug API
%%%

get_state() -> req(get_state).

code_change() -> req(code_change).

%%% End of API ----------------------------------------------------------------

%%%
%%% Scrollable console window.
%%%

do_window(Name, Opts) ->
    Title = ?STR(wc_open_window,1,"Wings3D Log"),
    Font = ?GET(console_font_wx),
    Size = case proplists:get_value(size, Opts) of
	       undefined ->
		   Width0  = wings_pref:get_value(console_width),
		   Height0 = wings_pref:get_value(console_height),
		   {CW,CH,_,_} = wxWindow:getTextExtent(?GET(top_frame), "W", [{theFont,Font}]),
		   W = max(3 + (Width0*CW) + 3, 400),
		   H = max(1 + (Height0*CH) + 4, 100),
		   {W,H};
	       SavedSize ->
		   SavedSize
	   end,
    Pos = case proplists:get_value(pos, Opts) of
	      undefined -> {-1, -1};
	      SavedPos -> SavedPos
	  end,
    {Win, Ps} = wings_frame:make_win(Title, [{size, Size}, {pos, Pos}|Opts]),
    {ok, Window} = req({window, wings_io:get_process_option(), Win, Font}),
    wings_wm:toplevel(Name, Window, Ps, {push, fun(Ev) -> req({event, Ev}), keep end}).

%%% I/O server ----------------------------------------------------------------

req(Request) ->
    case whereis(?SERVER_NAME) of
	Server when is_pid(Server) ->
	    req(Server, Request);
	_ ->
	    exit(not_started)
    end.

req(Server, Request) ->
    Mref = erlang:monitor(process, Server),
    Server ! {wings_console_request,self(),Mref,Request},
    receive
	{wings_console_reply,Mref,Reply} ->
	    console_demonitor(Mref),
	    Reply;
	{'DOWN',Mref,_,_,Reason} ->
	    exit(Reason)
    end.

console_demonitor(Mref) ->
    demonitor(Mref),
    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end.

server_loop(#state{gmon=Gmon, win=Win}=State) ->
    receive
	{io_request,From,ReplyAs,Request}=Msg when is_pid(From) ->
	    case io_request(State, Request) of
		{NewState,_,forward} ->
		    forward(Msg),
		    server_loop(NewState);
		{NewState,Reply,_} ->
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState)
	    end;
	{wings_console_request,From,ReplyAs,{stop,Reason}} when is_pid(From) ->
	    Win =:= undefined orelse wxFrame:destroy(Win),
	    wings_console_reply(From, ReplyAs, State#state.group_leader),
            error_logger:delete_report_handler(?MODULE),
	    exit(Reason);
	{wings_console_request,From,ReplyAs,code_change} when is_pid(From) ->
	    %% Code change exit point from old module
	    ?MODULE:do_code_change(State, From, ReplyAs);
	{wings_console_request,From,ReplyAs,Request} when is_pid(From) ->
	    {NewState,Reply} = wings_console_request(State, Request),
	    wings_console_reply(From, ReplyAs, Reply),
	    server_loop(NewState);
	{'DOWN',Gmon,_,_,Reason} ->
	    %% Group leader is down - die
            error_logger:delete_report_handler(?MODULE),
	    exit(Reason);
	#wx{} = WxEvent ->
	    NewState = wings_console_event(State, WxEvent),
	    server_loop(NewState);
	{'EXIT', _, _} ->
	    %% Wings main process down die
            error_logger:delete_report_handler(?MODULE),
	    exit(shutdown);
	Unknown ->
	    io:format(?MODULE_STRING++?STR(server_loop,1,":~w Received unknown: ~p~n"),
		      [?LINE,Unknown]),
	    server_loop(State)
    end.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

wings_console_reply(From, ReplyAs, Reply) ->
    From ! {wings_console_reply,ReplyAs,Reply}.

forward({io_request,From,ReplyAs,Req0}) ->
    Req = forward_1(Req0),
    group_leader() ! {io_request,From,ReplyAs,Req}.

forward_1({put_chars,unicode,Chars}) when is_binary(Chars) ->
    {put_chars,Chars};
forward_1({put_chars,unicode,Chars}) when is_list(Chars) ->
    try
	{put_chars,list_to_binary(Chars)}
    catch
	error:badarg ->
	    {put_chars,filter_chars(Chars)}
    end;
forward_1({put_chars,Chars}) when is_list(Chars) ->
    try
	{put_chars,list_to_binary(Chars)}
    catch
	error:badarg ->
	    {put_chars,filter_chars(Chars)}
    end;
forward_1({put_chars,Chars}=Req) when is_binary(Chars) ->
    Req;
forward_1({put_chars,unicode,Mod,Func,Args}) ->
    forward_1({put_chars,unicode,apply(Mod, Func, Args)});
forward_1({put_chars,Mod,Func,Args}) ->
    forward_1({put_chars,apply(Mod, Func, Args)}).

filter_chars([H|T]) when is_list(H) ->
    [filter_chars(H)|filter_chars(T)];
filter_chars([H|T]) when is_integer(H), 255 < H ->
    [$?,filter_chars(T)];
filter_chars([H|T]) ->
    [H|filter_chars(T)];
filter_chars([]) -> [].

%%%
%%% I/O requests
%%%

io_request(State, {put_chars,Chars}) ->
    {put_chars(State, Chars),ok,forward};
io_request(State, {put_chars,unicode,Chars}) ->
    {put_chars(State, Chars),ok,forward};
io_request(State, {put_chars,unicode,Mod,Func,Args}) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request(State, {put_chars,unicode,Chars});
	_ ->
	    {State,{error,Func},error}
    end;
io_request(State, {put_chars,Mod,Func,Args}) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request(State, {put_chars,Chars});
	_ ->
	    {State,{error,Func},error}
    end;
io_request(State, {requests,Requests}) when is_list(Requests) ->
    io_request_loop(Requests, {State,ok,ok});
io_request(State, {setopts,Opts}) when is_list(Opts) ->
    {State,{error,badarg},error};
io_request(State, Request) ->
    %% Probably a new version of Erlang/OTP with extensions to the
    %% I/O protocol. We could generate an error here, but the
    %% stack dump in wings_crash.dump would generate the caller of
    %% io:format/2 in the main Wings process with no indication that
    %% this module is the culprit. Therefore, we choose to ignore
    %% the request, but write a message to the console to point out
    %% the problem.
    S = io_lib:format("Internal error in Console - unknown I/O request:\n~P\n",
		      [Request,10]),
    {put_chars(State, iolist_to_binary(S)),ok,forward}.

io_request_loop([], Result) ->
    Result;
io_request_loop([_|_], {_,_,error}=Result) ->
    Result;
io_request_loop([Request|Requests], {State,_,ok}) ->
    io_request_loop(Requests, io_request(State, Request)).

put_chars(#state{ctrl=Ctrl} = State, Chars) when is_binary(Chars) ->
    is_tuple(Ctrl) andalso wxTextCtrl:appendText(Ctrl, [Chars]),
    put_chars_1(State, Chars);
put_chars(#state{ctrl=Ctrl} = State, Chars) when is_list(Chars) ->
    is_tuple(Ctrl) andalso wxTextCtrl:appendText(Ctrl, Chars),
    put_chars_1(State, unicode:characters_to_binary(Chars)).

put_chars_1(#state{cnt=Cnt0, lines=Lines0,
		   last=Last0, save_lines=Save}=State, IoBin0) ->
    IoBin = erlang:iolist_to_binary([Last0, IoBin0]),
    NewLines = binary:split(IoBin, <<"\n">>, [global]),
    {Lines, Cnt, Last} = put_chars_1(NewLines, Lines0, Cnt0, Save),
    State#state{cnt=Cnt, lines=Lines, last=Last}.

put_chars_1([<<>>], Lines, Cnt, _Save) ->
    {Lines, Cnt, <<>>};
put_chars_1([LastWOeol], Lines, Cnt, _) ->
    {Lines, Cnt, LastWOeol};
put_chars_1([Line|NLs], Lines, Cnt, Save) when Cnt < Save ->
    put_chars_1(NLs, queue:in(Line, Lines), Cnt+1, Save);
put_chars_1([Line|NLs], Lines, Cnt, Save) ->
    put_chars_1(NLs, queue:in(Line, queue:drop(Lines)), Cnt, Save).

%%%
%%% Wings console requests
%%%

wings_console_event(State, #wx{event=#wxWindowDestroy{}}) ->
    wings ! {wm, {delete, ?WIN_NAME}},
    State#state{win=undefined, ctrl=undefined};
wings_console_event(#state{ctrl=Ctrl} = State, #wx{event=#wxSize{size={W0,H0}}}) ->
    {CW,CH,_,_} = wxWindow:getTextExtent(Ctrl, "W"),
    W=W0-6, H=H0-5,
    wings_pref:set_value(console_width, W div CW),
    wings_pref:set_value(console_height, H div CH),
    State;
wings_console_event(State, #wx{event=#wxMouse{}}=Ev) ->
    wings_frame ! Ev,
    State.

wings_console_request(State0, {window, WxEnv, Win, Font}) ->
    wings_io:set_process_option(WxEnv),
    wc_open_window(State0, Win, Font);
wings_console_request(State, {setopts,Opts}) ->
    wc_setopts(State, Opts);
wings_console_request(State, {getopts,Opts}) ->
    wc_getopts(State, Opts, []);
wings_console_request(State, get_state) ->
    {State,State};
wings_console_request(State, {event, Ev}) ->
    case Ev of
	close -> wings ! {wm, {delete, ?WIN_NAME}};
	_ -> %% io:format("~p: Got ~p~n",[?MODULE, Ev]),
	    ignore
    end,
    {State,State};
wings_console_request(State, Request) ->
    {State,{error,{request,Request}}}.

wc_setopts(#state{save_lines=SaveLines0}=State,Opts) ->
    SaveLines = proplists:get_value(save_lines, Opts, SaveLines0),
    if is_integer(SaveLines), SaveLines >= 0 ->
	    {State#state{save_lines=SaveLines},ok};
       true ->
	    {State,{error,badarg}}
    end.

wc_getopts(State, [], R) ->  {State,reverse(R)};
wc_getopts(#state{save_lines=SaveLines}=State, [save_lines|Opts], R) ->
    wc_getopts(State, Opts, [{save_lines,SaveLines}|R]);
wc_getopts(State, _, _) ->
    {State,{error,badarg}}.

wc_open_window(#state{lines=Lines}=State, Win, Font) ->
    TStyle = ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2,
    Ctrl = wxTextCtrl:new(Win, ?wxID_ANY, [{style, TStyle}]),

    wxWindow:setFont(Ctrl, Font),
    wxWindow:setBackgroundColour(Ctrl, wings_color:rgb4bv(wings_pref:get_value(console_color))),
    wxWindow:setForegroundColour(Ctrl, wings_color:rgb4bv(wings_pref:get_value(console_text_color))),
    wxTextCtrl:appendText(Ctrl, [[Line,$\n] || Line <- queue:to_list(Lines)]),
    wxWindow:connect(Ctrl, destroy, [{skip, true}]),
    wxWindow:connect(Ctrl, size, [{skip, true}]),
    wxWindow:connect(Ctrl, enter_window, [{userData, {win, Ctrl}}]),
    {State#state{win=Win, ctrl=Ctrl}, {ok, Ctrl}}.

%%% Other support functions
%%%

%% Code change entry point in the new module. Called in the new module
%% with the state from the old module.
%%
%% Intended for development.
%%
do_code_change(?STATE, From, ReplyAs) ->
    wings_console_reply(From, ReplyAs, ok),
    server_loop(#state{gmon=Gmon,group_leader=GroupLeader,
		       win=Win, ctrl=Ctrl,
		       save_lines=SaveLines,last=Last,
		       cnt=Cnt,lines=Lines}).
