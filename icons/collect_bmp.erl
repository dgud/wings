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
-import(lists, [reverse/1]).

start() ->
    start(["icons","wings_icon.bundle"]).

start(Args) ->
    io:put_chars("Loading"),
    do_start(Args, []).

do_start([InDir|[_|_]=T], Files) ->
    do_start(T, add_files(InDir, Files));
do_start([OutFile], Files) ->
    wx:new(),
    Icons = load_icons(Files),
    io:nl(),
    Bin = term_to_binary(Icons, [compressed]),
    io:format("Writing ~s\n", [OutFile]),
    ok = file:write_file(OutFile, Bin).

add_files(Dir, Acc) ->
    filelib:wildcard(filename:join(Dir, "*.{bmp,tga,png}")) ++ Acc.

load_icons([Name|Ns]) ->
    Id = list_to_atom(filename:rootname(filename:basename(Name))),
    Icon = load_icon(Name),
    [{Id,Icon}|load_icons(Ns)];
load_icons([]) -> [].

load_icon(Name) ->
    Image = wxImage:new(Name),
    wxImage:ok(Image) orelse exit({failed_to_load, Name}),
    W = wxImage:getWidth(Image),
    H = wxImage:getHeight(Image),
    RGB = wxImage:getData(Image),
    case wxImage:hasAlpha(Image) of
	true ->  {4,W,H,RGB,wxImage:getAlpha(Image)};
	false -> {3,W,H,RGB, <<>>}
    end.


