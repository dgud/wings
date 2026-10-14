%%
%%  wings_save_manager.erl --
%%
%%     Centralized process manager for persistent state saving (AutoSave & Manual Save).
%%     Acts as a gatekeeper to ensure only one write operation occurs at a time,
%%     preventing race conditions and blocking the main GUI thread during I/O.
%%
%%  Copyright (c) 2026 Micheus & Dan
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_save_manager).
-behaviour(gen_server).

-export([start_link/0, save/3, autosave/2]).

-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).

-include("wings.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

autosave(TargetFile, St0) ->
    St = wings_undo:init(St0),  %% Clear undo
    gen_server:cast(?MODULE, {autosave, TargetFile, St}).

save(TargetFile, SaveSelParts, St0) ->
    St = wings_undo:init(St0),  %% Clear undo
    gen_server:call(?MODULE, {save, TargetFile, SaveSelParts, St}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% Internal state: #{busy => boolean(), worker => pid() | undefined}
init([]) ->
    process_flag(trap_exit, true),
    {ok, #{busy => false, worker => undefined}}.

%%--------------------------------------------------------------------
%% Cast: saving request from the main thread.
%%--------------------------------------------------------------------
handle_cast({autosave, TargetFile, St}, #{busy := false}=State) ->
    %% Start the worker and mark the state as busy.
    Parent = self(),
    Worker = spawn_link(fun() ->
                                Result = do_save(TargetFile, St, false, autosave),
                                Parent ! {save_done, self(), Result, TargetFile}
                        end),
    NewState = State#{busy => true, worker => Worker},
    {noreply, NewState, hibernate};
handle_cast({autosave, _TargetFile, _St}, #{busy := Busy}=State) ->
    Msg = ?__(1, "There is already an autosave in progress; the new request was ignored.
                  ~nIncrease the autosave interval if necessary."),
    Busy =:= true andalso wings_wm:psend(geom, {external, fun(_) -> wings_u:message(Msg) end}),
    {noreply, State};

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
handle_info({save_done, WorkerPid, Result, TargetFile}, #{busy := Busy} = State) ->
    case maps:get(worker, State) of
        WorkerPid ->
            wings_wm:psend(autosaver, {auto_saved, TargetFile, Result}),
            %% Clear the busy flag and the worker PID.
            State1 = State#{busy => false, worker => undefined},
            case Busy of
                true -> {noreply, State1};
                {SaveCall, From} ->
                    {reply, Res, State2} = handle_call(SaveCall, From, State1),
                    gen_server:reply(From, Res),
                    {noreply, State2, hibernate}
            end;
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

handle_call({save, TargetFile, Sel, St}, _From, #{busy := false}=State) ->
    Result = do_save(TargetFile, St, Sel, save),
    {reply, Result, State, hibernate};
handle_call({save, _TargetFile, _St, _} = Msg, From, #{busy := true}=State) ->
    {noreply, State#{busy := {Msg,From}}};
handle_call(_Msg, _From, State) ->
    {reply, {error, ?__(1, "[Save Manager] Use the async save_state/2 API~n")}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_save(TargetFile, St, SaveParts, Type) ->
    try
        Execute = fun() -> wings_ff_wings:export(TargetFile, SaveParts, St) end,
        wings_develop:time_command(Execute, Type)
    catch
        Class:Reason:Stack ->
            {error, {Class, Reason, Stack}}
    end.
