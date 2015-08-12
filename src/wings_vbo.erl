%%
%%  wings_vbo.erl --
%%
%%     Utilities for handling Vertex Buffer Objects.
%%
%%  Copyright (c) 2015 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_vbo).
-export([draw/2,draw/3,new/2,new/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

new(Draw, Data) ->
    new(Draw, Data, [vertex]).

new(Draw, Data0, Layout) when is_list(Data0) ->
    Data = << <<A:?F32,B:?F32,C:?F32>> || {A,B,C} <- Data0 >>,
    new(Draw, Data, Layout);
new(Draw, Data, Layout) when is_binary(Data) ->
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    D = parse_layout(Layout, Vbo, Draw),
    {call,D,{vbo,Vbo}}.

draw(Draw, Data) ->
    draw(Draw, Data, [vertex]).

draw(Draw0, Data0, Layout) ->
    Data = << <<A:?F32,B:?F32,C:?F32>> || {A,B,C} <- Data0 >>,
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    Draw = parse_layout(Layout, Vbo, Draw0),
    Draw(),
    ok.

parse_layout([vertex], Vbo, Draw) ->
    fun() ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    Draw(),
	    gl:disableClientState(?GL_VERTEX_ARRAY)
    end;
parse_layout(Layout, Vbo, Draw) ->
    Stride = case length(Layout) of
		 1 -> 0;
		 N -> N*3*4
	     end,
    Bufs = parse_layout_1(Layout, Stride, 0),
    fun() ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    enable_buffers(Bufs),
	    Draw(),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    disable_buffers(Bufs)
    end.

parse_layout_1([Type0|T], Stride, Addr) ->
    Type = case Type0 of
	       vertex -> ?GL_VERTEX_ARRAY;
	       color -> ?GL_COLOR_ARRAY
	   end,
    Buf = {Type,Stride,Addr},
    [Buf|parse_layout_1(T, Stride, Addr+3*4)];
parse_layout_1([], _, _) -> [].

enable_buffers([{Type,Stride,Addr}|T]) ->
    case Type of
	?GL_VERTEX_ARRAY ->
	    gl:vertexPointer(3, ?GL_FLOAT, Stride, Addr);
	?GL_COLOR_ARRAY ->
	    gl:colorPointer(3, ?GL_FLOAT, Stride, Addr)
    end,
    gl:enableClientState(Type),
    enable_buffers(T);
enable_buffers([]) -> ok.

disable_buffers([{Type,_,_}|T]) ->
    gl:disableClientState(Type),
    disable_buffers(T);
disable_buffers([]) -> ok.
