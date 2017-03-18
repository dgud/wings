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
-export([draw/2,draw/3,new/2,new/3, delete/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

new(Draw, Data) ->
    new(Draw, Data, [vertex]).

new(Draw, Data0, Layout) when is_list(Data0) ->
    Data = make_bin(Data0, Layout),
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

draw(Draw0, Data0, Layout) when is_list(Data0) ->
    Data = make_bin(Data0, Layout),
    draw(Draw0, Data, Layout);
draw(Draw0, Data, Layout) ->
    [Vbo] = Buffers = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    Draw = parse_layout(Layout, Vbo, Draw0),
    _ = Draw(#{}),
    gl:deleteBuffers(Buffers),
    ok.

delete({vbo, Vbo}) ->
    gl:deleteBuffers([Vbo]);
delete({call,_D,{vbo,Vbo}}) ->
    gl:deleteBuffers([Vbo]).

parse_layout([vertex], Vbo, Draw) ->
    fun(RS0) ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    gl:vertexPointer(3, ?GL_FLOAT, 0, 0),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    RS = Draw(RS0),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
            RS
    end;
parse_layout({predefined, Bufs}, Vbo, dynamic) ->
    fun(Draw, RS0) ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    enable_buffers(Bufs),
	    RS = Draw(RS0),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    disable_buffers(Bufs),
            RS
    end;
parse_layout(Layout, Vbo, Draw) ->
    Stride = case Layout of
		 [_] -> 0;
		 _ -> lists:sum([fsize(L) || L <- Layout])
	     end,
    Bufs = parse_layout_1(Layout, Stride, 0),
    fun(RS0) ->
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    enable_buffers(Bufs),
	    RS = Draw(RS0),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    disable_buffers(Bufs),
            RS
    end.

parse_layout_1([Type|T], Stride, Addr) ->
    Buf = {Type,Stride,Addr},
    [Buf|parse_layout_1(T, Stride, Addr+fsize(Type))];
parse_layout_1([], _, _) -> [].

make_bin(List, Layout) ->
    make_bin(List, Layout, Layout, <<>>).

make_bin([{A,B}|T], [Two|L], Layout, Bin)
  when Two =:= vertex2d; Two =:= uv ->
    make_bin(T, L, Layout, <<Bin/binary, A:?F32,B:?F32>>);
make_bin([{A,B,C}|T], [_|L], Layout, Bin) ->
    make_bin(T, L, Layout, <<Bin/binary, A:?F32,B:?F32,C:?F32>>);
make_bin([], [], _, Bin) ->
    Bin;
make_bin(T, [], Layout, Bin) ->
    make_bin(T, Layout, Layout, Bin).

fsize(vertex) -> 3*4;
fsize(vertex2d) -> 2*4;
fsize(normal) -> 3*4;
fsize(color) -> 3*4;
fsize(uv) -> 2*4;
fsize({tex,_,Size}) -> Size*4.

type(vertex) -> ?GL_VERTEX_ARRAY;
type(vertex2d) -> ?GL_VERTEX_ARRAY;
type(color) -> ?GL_COLOR_ARRAY;
type(normal) -> ?GL_NORMAL_ARRAY;
type(uv) -> ?GL_TEXTURE_COORD_ARRAY;
type({tex,Which,_}) ->
    gl:clientActiveTexture(?GL_TEXTURE0+Which),
    ?GL_TEXTURE_COORD_ARRAY.

enable_buffers([{Type,Stride,Addr}|T]) ->
    case Type of
	vertex -> gl:vertexPointer(3, ?GL_FLOAT, Stride, Addr);
        color  -> gl:colorPointer(3, ?GL_FLOAT, Stride, Addr);
        normal -> gl:normalPointer(?GL_FLOAT, Stride, Addr);
        uv     -> gl:texCoordPointer(2, ?GL_FLOAT, Stride, Addr);
        vertex2d -> gl:vertexPointer(2, ?GL_FLOAT, Stride, Addr);
        {tex,Which,Size} ->
            gl:clientActiveTexture(?GL_TEXTURE0+Which),
            gl:texCoordPointer(Size, ?GL_FLOAT, Stride, Addr),
            gl:clientActiveTexture(?GL_TEXTURE0)
    end,
    gl:enableClientState(type(Type)),
    enable_buffers(T);
enable_buffers([]) -> ok.

disable_buffers([{Type,_,_}|T]) ->
    gl:disableClientState(type(Type)),
    gl:clientActiveTexture(?GL_TEXTURE0),
    disable_buffers(T);
disable_buffers([]) -> ok.
