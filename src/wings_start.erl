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

start() ->
    common_start(fun() -> wings:start() end).

start(Args) ->
    common_start(fun() -> wings:start_halt(Args) end).

start_halt() ->
    common_start(fun() -> wings:start_halt() end).

start_halt(Args) ->
    common_start(fun() -> wings:start_halt(Args) end).

common_start(Start) ->
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
    filename:join(wings_util:lib_dir(wings), "patches").



