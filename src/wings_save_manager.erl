%%
%%  wings_save_manager.erl --
%%
%%     Centralized process manager for persistent state saving (AutoSave & Manual Save).
%%     Acts as a gatekeeper to ensure only one write operation occurs at a time,
%%     preventing race conditions and blocking the main GUI thread during I/O.
%%
%%  Copyright (c) 2026 Micheus / AI Assistant
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_save_manager).
-behaviour(gen_server).

-export([start_link/0, save_state/3]).

-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).

-include("wings.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Asynchronous save state.
%%   St  = #st{}
%%   TargetFile  = full path to the target file
%%   Type = save type (manual or autosave)
save_state(St, TargetFile, Type) ->
    gen_server:cast(?MODULE, {save_request, St, TargetFile, Type}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% Internal state: #{busy => boolean(), worker => pid() | undefined}
init([]) ->
    process_flag(trap_exit, true),
    io:format(?__(1, "Save manager ready...~n")),
    {ok, #{busy => false, worker => undefined}}.

%%--------------------------------------------------------------------
%% Cast: saving request from the main thread.
%%--------------------------------------------------------------------
handle_cast({save_request, _St, _TargetFile, _Type}, #{busy := true}=State) ->
    %% There is already an save/autosave in progress – we can discard or put in queue.
    %% Here we are chosen to discard and inform the user.
    Msg = ?__(1, "There is already an save/autosave in progress; the new request was ignored.
                  ~nIncrease the autosave interval if necessary."),
    wings_wm:psend(geom, {external, fun(_) -> wings_u:message(Msg) end}),
    {noreply, State};

handle_cast({save_request, St, TargetFile, Type}, #{busy := false}=State) ->
    %% Start the worker and mark the state as busy.
    Parent = self(),
    Worker = spawn_link(fun() ->
        Result = try
                     Execute = fun() -> wings_ff_wings:export(TargetFile, false, St) end,
                     wings_develop:time_command(Execute, autosave)
                 catch
                     Class:Reason:Stack ->
                         {error, {Class, Reason, Stack}}
                 end,
        Parent ! {save_done, self(), {Result, Type}, TargetFile, St}
    end),
    NewState = State#{busy => true, worker => Worker},
    {noreply, NewState};

handle_cast(stop, State) ->
    case maps:get(worker, State) of
        undefined -> ok;
        Pid      -> exit(Pid, kill)
    end,
    {stop, normal, State};

handle_cast(_Other, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Info: receives the worker's message when it finishes.
%%--------------------------------------------------------------------
handle_info({save_done, WorkerPid, {Result, Type}, TargetFile, _St}, State) ->
    %% Ensure the message came from the expected worker.
    case maps:get(worker, State) of
        WorkerPid ->
            case Result of
                ok ->
                    case Type of
                        manual   -> Saved = true;
                        autosave -> Saved = auto
                    end,
                    %% Notify the main thread (UI) that the save was successful and update the saved status.
                    wings_wm:psend(geom, {action,{saved,{Saved,TargetFile}}});
                {error, {_Class, Reason, Stack}} ->
                    io:format(?__(1, "[Save Manager] failed to save file ~s~n") ++ "  ~p~n  ~p~n",
                                ["\""++TargetFile++"\"",Reason,Stack]),
                    %% Message via UI (right thread)
                    Msg = ?__(2, "[Save Manager] worker error! Check the log window for details."),
                    wings_wm:psend(geom, {external, fun(_) -> wings_u:message(Msg), keep end})
            end,

            %% Clear the busy flag and the worker PID.
            NewState = State#{busy => false, worker => undefined},
            {noreply, NewState};
        _Other ->
            {noreply, State}
    end;

%% In case the worker crashes unexpectedly (for example, crash)
handle_info({'EXIT', WorkerPid, Reason}, State) ->
    case maps:get(worker, State) of
        WorkerPid ->
            io:format("[Save Manager] worker crashed: ~p~n", [Reason]),
            NewState = State#{busy => false, worker => undefined},
            {noreply, NewState};
        _Other ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Call (not used, but maintained for compatibility)
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, {error, ?__(1, "[Save Manager] Use the async save_state/2 API~n")}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
