%%
%%  collect_bmp --
%%
%%     Collect BMP and TGA icons for Wings.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(collect_bmp).
-export([start/0,start/1]).

-include_lib("wings/e3d/e3d_image.hrl").

start() ->
    start(["icons","wings_icon.bundle"]).

start(Args) ->
    do_start(Args, []).

do_start([InDir|[_|_]=T], Files) ->
    do_start(T, add_files(InDir, Files));
do_start([OutFile], Files) ->
    Icons = load_icons(Files),
    Bin = term_to_binary(Icons, [compressed]),
    %% io:format("Writing ~s\n", [OutFile]),
    ok = file:write_file(OutFile, Bin).

add_files(Dir, Acc) ->
    filelib:wildcard(filename:join(Dir, "*.{bmp,tga,png}")) ++ Acc.

load_icons([Name|Ns]) ->
    Id = list_to_atom(filename:rootname(filename:basename(Name))),
    Icon = load_icon(Name),
    [{Id,Icon}|load_icons(Ns)];
load_icons([]) -> [].

load_icon(Name) ->
    case e3d_image:load(Name) of
        #e3d_image{bytes_pp=4, width=W, height=H} = I ->
            #e3d_image{image=RGB} = e3d_image:convert(I, r8g8b8, 1, upper_left),
            #e3d_image{image=Alpha} = e3d_image:convert(I, a8, 1, upper_left),
            {4,W,H,RGB,Alpha};
        #e3d_image{bytes_pp=3, width=W, height=H} = I ->
            #e3d_image{image=RGB} = e3d_image:convert(I, r8g8b8, 1, upper_left),
            {3,W,H,RGB,<<>>};
        _ ->
            exit({failed_to_load, Name})
    end.


