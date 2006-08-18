%%
%%  pnoise.erl --
%%
%%     Driver for accelerating perlin noise 
%%
%%  Copyright (c) 2005 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: pnoise.erl,v 1.2 2006/01/17 15:48:37 dgud Exp $
%%

-module(pnoise).

-export([start/0,stop/0]).
-export([val/1,val/2,val/3, map1d/1, map2d/1,map3d/1]).
-export([s_val/1,s_val/2,s_val/3,s_val/4,s_map1d/1,s_map2d/1,s_map3d/1]).

-export([test/1]).

-define(FL64, :64/native-float).
-define(PORT, perlin_noise_port).

-define(PNOISE3, 3).
-define(SNOISE1, 4).
-define(SNOISE2, 5).
-define(SNOISE3, 6).
-define(SNOISE4, 7).

-define(PNOISE_MAP1, 11).
-define(PNOISE_MAP2, 12).
-define(PNOISE_MAP3, 13).

-define(SNOISE_MAP3, 16).


start() ->
    Dir = filename:dirname(code:which(?MODULE)),
    Name = "perlin_noise_drv",
    case erl_ddll:load_driver(Dir, Name) of
	ok -> ok;
	{error,Reason} ->
	    io:format("Failed to load ~s in ~s\n~s\n",
		      [Name,Dir,erl_ddll:format_error(Reason)]),
	    erlang:fault(startup_fault)
    end,
    case open_port({spawn,Name},[binary]) of
	Port when is_port(Port) ->
	    register(?PORT, Port);
	_ ->
	    io:format("Failed to open port ~s\n", [Name]),
	    erlang:fault(startup_fault)
    end,
    ok.

stop() ->
    erlang:port_close(?PORT),
    erl_ddll:unload_driver("perlin_noise_drv").

val({X,Y}) -> val(X,Y);
val({X,Y,Z}) -> val(X,Y,Z);
val(X) when is_number(X) -> val(X,X,X).
val(X,Y) -> val(X,Y,X).

val(X,Y,Z) ->
    Bin = <<X?FL64,Y?FL64,Z?FL64>>,
    <<Res?FL64>> = erlang:port_control(?PORT, ?PNOISE3, Bin),
    Res.

s_val({X,Y}) -> s_val(X,Y);
s_val({X,Y,Z}) -> s_val(X,Y,Z);
s_val({X,Y,Z,W}) -> s_val(X,Y,Z,W);
s_val(X) when is_number(X) -> 
    Bin = <<X?FL64>>,
    <<Res?FL64>> = erlang:port_control(?PORT, ?SNOISE1, Bin),
    Res.
s_val(X,Y) -> 
    Bin = <<X?FL64,Y?FL64>>,
    <<Res?FL64>> = erlang:port_control(?PORT, ?SNOISE2, Bin),
    Res.
s_val(X,Y,Z) ->
    Bin = <<X?FL64,Y?FL64,Z?FL64>>,
    <<Res?FL64>> = erlang:port_control(?PORT, ?SNOISE3, Bin),
    Res.
s_val(X,Y,Z,W) ->
    Bin = <<X?FL64,Y?FL64,Z?FL64,W?FL64>>,
    <<Res?FL64>> = erlang:port_control(?PORT, ?SNOISE4, Bin),
    Res.

%% Generates luminance maps i.e. noise values between in 0-255
map1d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?PNOISE_MAP1, Bin).
map2d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?PNOISE_MAP2, Bin).
map3d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?PNOISE_MAP3, Bin).

s_map1d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?SNOISE_MAP3, Bin).
s_map2d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?SNOISE_MAP3, Bin).
s_map3d(Sz) ->
    Bin = <<Sz:32/unsigned-native>>,
    erlang:port_control(?PORT, ?SNOISE_MAP3, Bin).

%%%%%%%%%%%%%%%%%%% 
%% Test
test(Func) ->
    {Time,Bin} = timer:tc(pnoise,Func,[(128)]),
    List = binary_to_list(Bin),
    io:format("Time ~p ~p ~p ~p~n",[Time,lists:sum(List)/size(Bin),lists:min(List),lists:max(List)]),
    pb(Bin, 0).

pb(<<Bin:32/binary,R/binary>>, N) when N < (4*64) ->
    io:format("~p: ~p~n",[N div 4, Bin]),
    pb(R,N+1);
pb(_,_) -> ok.
