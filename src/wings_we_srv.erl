%%
%%  wings_we_srv.erl --
%%
%%     We process, holder of the we record
%%
%%  Copyright (c) 2019 Bjorn Gustavsson & Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_we_srv).
-behavior(gen_server).

%% gen_server callbacks
-export([start/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).


-include("wings.hrl").

%% API
-spec start(#we{}, non_neg_integer()) -> pid().
start(We, Gen) ->
    wings_sup:object([We,Gen]).

%% Internal

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([We, Gen]) ->
    WstV = #wst_versioned{we=We, sel=gb_sets:empty()},
    Dlo = #dlo_src{we=We},
    Wst = #wst{insts=#{Gen=>WstV}, dlo_src=Dlo},
    {ok, Wst}.

handle_call(Req, _From, S) ->
    ?dbg("~w Unhandled call: ~w~n", [self(), Req]),
    {reply, error, S}.

handle_cast(Req, S) ->
    ?dbg("~w: Unhandled cast: ~w~n", [self(), Req]),
    {noreply, error, S}.

terminate(Reason, _State) ->
    ?dbg("~w: terminating due to: ~p~n",[self(), Reason]),
    ok.
