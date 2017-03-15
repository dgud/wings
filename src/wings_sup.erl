%%  wings_sup.erl --
%%
%%     Wings supervisor
%%
%%  Starts all supervisors for in a single module
%%
%%  Copyright (c) 2017 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(wings_sup).

-behaviour(supervisor).

%% API
-export([start/2, start_link/0, stop/1, window/3]).

%% Supervisor callbacks
-export([init/1, init_done/0, window_sup/1, mandatory_sup/1, wx_object/3]).

%% Main start function

start(_, _) ->
    start_link().

stop(_) ->
    case application:get_env(wings, halt) of
        {ok, true} -> erlang:halt();
        {ok, false} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [top]).

window(Name, Module, Args) ->
    {ok, _, Window} = supervisor:start_child(windows, [Name, Module, Args]),
    Window.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

wx_object(Name, Module, Args) ->
    Window = case Name of
                 undefined ->
                     wx_object:start_link(Module, Args, []);
                 _ ->
                     wx_object:start_link({local,Name}, Module, Args, [])
             end,
    {ok, wx_object:get_pid(Window), Window}.

%% Starts the window supervisor
window_sup(Env) ->
    supervisor:start_link({local, windows}, ?MODULE, [windows, Env]).

mandatory_sup(Env) ->
    supervisor:start_link({local, mandatory}, ?MODULE, [mandatory, Env]).

init_done() ->
    Pid = spawn_link(fun() -> wings ! supervisor_initialization_done, normal end),
    {ok, Pid}.

init([top]) ->
    wx:new(),
    Env = wx:get_env(),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 5},
    Main = #{id => wings,
             start => {wings, start_link, [Env]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [wings]},
    Console = #{id => console,
                start => {wings_console, start_link, [Env]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [wings_console]},
    WindowSup = #{id => windows,
                  start => {?MODULE, window_sup, [Env]},
                  restart => permanent,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [?MODULE]},
    MandatorySup = #{id => mandatory,
                     start => {?MODULE, mandatory_sup, [Env]},
                     restart => permanent,
                     shutdown => 5000,
                     type => supervisor,
                     modules => [?MODULE]},
    {ok, {SupFlags, [Console, WindowSup, Main, MandatorySup]}};

init([mandatory, Env]) ->
    wx:set_env(Env),
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 5},
    %% Order is important
    Top = #{id=>top_frame,
            start=>{wings_frame, start_link, []},
            restart=>permanent,
            shutdown=>1000,
            type=>worker
           },
    PB = #{id=>pb,
           start=>{wings_pb, start_link, []},
           restart=>permanent,
           shutdown=>1000,
           type=>worker
          },
    SB = #{id=>sb,
           start=>{wings_status, start_link, []},
           restart=>permanent,
           shutdown=>1000,
           type=>worker
          },
    IH = #{id=>wings_image,
           start=>{wings_image, start_link, []},
           restart=>permanent,
           shutdown=>1000,
           type=>worker
          },

    %% Must be invoked last
    Last = #{id=>init_done,
             start=>{?MODULE, init_done, []},
             restart=>temporary,
             shutdown=>1000,
             type=>worker
            },
    {ok, {SupFlags, [Top, PB, SB, IH, Last]}};

init([windows, Env]) ->
    wx:set_env(Env),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    ChildSpec = #{id=>window,
                  start=>{?MODULE, wx_object, []},
                  restart=>temporary,
                  shutdown=>1000,
                  type=>worker
                 },
    {ok, {SupFlags, [ChildSpec]}}.


