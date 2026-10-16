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

init([]) ->
    process_flag(trap_exit, true),
    {ok, #{}}.


handle_call({save, TargetFile, Sel, St}, _From, #{}=State) ->
    Result = do_save(TargetFile, St, Sel, save),
    {reply, Result, State, hibernate};
handle_call(_Msg, _From, State) ->
    {reply, {error, ?__(1, "[Save Manager] Use the async save_state/2 API~n")}, State}.

handle_cast({autosave, TargetFile, St}, #{}=State) ->
    Result = do_save(TargetFile, St, false, autosave),
    wings_wm:psend(autosaver, {auto_saved, TargetFile, Result}),
    {noreply, State, hibernate};

handle_cast(stop, State) ->
    case maps:get(worker, State) of
        undefined -> ok;
        Pid      -> exit(Pid, kill)
    end,
    {stop, normal, State};

handle_cast(_Other, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


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
