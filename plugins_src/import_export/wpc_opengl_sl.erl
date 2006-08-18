%%
%%  wpc_opengl_sl.erl --
%%
%%     Renderer implemented using the Shading Language in OpenGL.
%%
%%  Copyright (c) 2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_opengl_sl.erl,v 1.1 2004/12/18 19:36:02 bjorng Exp $

-module(wpc_opengl_sl).

-export([init/0,menu/2,command/2]).
-export([enable/0,disable/0]).

-import(lists, [reverse/1,reverse/2]).

enable() -> wpa:pref_set(?MODULE, enabled, true).

disable() -> wpa:pref_delete(?MODULE, enabled).

enabled() -> wpa:pref_get(?MODULE, enabled).

init() ->
    case enabled() of
	true -> init_1();
	_ -> false
    end.

init_1() ->
    wings_gl:is_ext('GL_ARB_vertex_shader') andalso
	wings_gl:is_ext('GL_ARB_fragment_shader').

menu({file,render}, Menu0) ->
    [{"OpenGL (SL)",opengl_sl,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{opengl_sl,Ask}}}, St) ->
    render(Ask, St);
command(_, _) -> next.

render(Ask, St) ->
    io:format("~p\n", [Ask]),
    keep.

