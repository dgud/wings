%%
%%  wings_start.erl --
%%
%%     Starter of Wings 3D; might enable installed patches.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_start).
-export([start/0,start/1,start_halt/0,start_halt/1]).
-export([get_patches/0,enable_patches/0,disable_patches/0]).

-include("wings.hrl").

start() ->
    application:set_env(wings, halt, false),
    common_start(fun() -> application:start(wings) end).

start(Args) ->
    application:set_env(wings, args, Args),
    application:set_env(wings, halt, false),
    common_start(fun() -> application:start(wings) end).

start_halt() ->
    application:set_env(wings, halt, true),
    common_start(fun() -> application:start(wings) end).

start_halt(Args) ->
    application:set_env(wings, args, Args),
    application:set_env(wings, halt, true),
    common_start(fun() -> application:start(wings) end).

common_start(Start) ->
    %% Limit error logger output in crash (OTP-20)
    application:set_env(kernel, error_logger_format_depth, 50),
    %% Re-read error_logger_format_depth set above
    error_logger:tty(false),
    error_logger:tty(true),
    case get_patches() of
	none -> ok;
	{disabled,_} -> ok;
	{enabled,_} -> code:add_patha(patch_dir())
    end,
    Start().

get_patches() ->
    Patches = patch_dir(),
    case filelib:wildcard(filename:join(Patches, "*.beam")) of
	[] -> none;
	_ ->
	    case filelib:is_file(filename:join(Patches, "PATCHES_ENABLED")) of
		true -> {enabled,patch_name(Patches)};
		false -> {disabled,patch_name(Patches)}
	    end
    end.

patch_name(Dir) ->
    case file:read_file(filename:join(Dir, "DESCRIPTION")) of
	{ok,<<Str:20/binary,_/binary>>} -> binary_to_list(Str);
	{ok,Bin} -> binary_to_list(Bin);
	_Other -> "Installed Patches"
    end.

enable_patches() ->
    Name = filename:join(patch_dir(), "PATCHES_ENABLED"),
    file:write_file(Name, "").

disable_patches() ->
    Name = filename:join(patch_dir(), "PATCHES_ENABLED"),
    file:delete(Name).

patch_dir() ->
    Dir = filename:basedir(user_data, "Wings3D"),
    filename:join([Dir, "patches", ?WINGS_VERSION]).



