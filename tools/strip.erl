%%
%%  strip.erl --
%%
%%     Utility module for stripping debug information from beam files
%%     in the current working directory and in all sub-directories.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(strip).
-export([strip/0]).

strip() ->
    case catch strip_dir(".") of
        {'EXIT',Reason} ->
            io:format("~P\n", [Reason,40]),
            halt(1);
        _ ->
            halt(0)
    end.

strip_dir(Dir) ->
    io:format("Stripping (beam) files in ~p\n", [Dir]),
    {ok,Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    {ok,Files} = file:list_dir("."),
    strip_files(Files),
    ok = file:set_cwd(Cwd).

strip_files([F|Fs]) ->
    case filelib:is_dir(F) of
        true ->
            strip_dir(F);
        false ->
            case filename:extension(F) of
                ".beam" ->
                    %% io:format("Stripping ~p\n", [F]),
                    {ok,{_,F}} = beam_lib:strip(F);
                _ ->
                    %% io:format("Ignoring ~p\n", [F]),
                    ok
            end
    end,
    strip_files(Fs);
strip_files([]) -> ok.
