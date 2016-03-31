%%
%%  wings_init.erl --
%%
%%     Initialization of Wings geom window video and event handling.
%%
%%  Copyright (c) 2003-2015 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_init).
-export([create/2, gl_attributes/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-ifndef(WX_GL_SAMPLE_BUFFERS).     %% New in wxWidgets-3.0
-define(WX_GL_SAMPLE_BUFFERS,17).  %% 1 for multisampling support (antialiasing)
-define(WX_GL_SAMPLES,18).         %% 4 for 2x2 antialiasing supersampling on most graphics cards
-endif.

gl_attributes() ->
    [?WX_GL_RGBA,
     ?WX_GL_MIN_RED,8,?WX_GL_MIN_GREEN,8,?WX_GL_MIN_BLUE,8,
     ?WX_GL_DEPTH_SIZE, 24, ?WX_GL_STENCIL_SIZE, 8,
     ?WX_GL_DOUBLEBUFFER,
     ?WX_GL_SAMPLE_BUFFERS,1,
     ?WX_GL_SAMPLES, 4,
     0].

create(Frame, Context) ->
    Style = ?wxFULL_REPAINT_ON_RESIZE bor ?wxWANTS_CHARS,
    Flags = [{attribList, gl_attributes()}, {style, Style}],
    GL = case Context of
	     undefined ->
		 Win = wxGLCanvas:new(Frame, Flags),
		 ?SET(gl_canvas, Win),
		 Win;
	     _ ->
		 wxGLCanvas:new(Frame, Context, Flags)
	 end,
    connect_events(GL),
    wxFrame:connect(Frame, show),
    wxFrame:show(Frame),
    receive #wx{event=#wxShow{}} -> ok end,
    wxGLCanvas:setCurrent(GL),
    wxWindow:disconnect(Frame, show),
    GL.

connect_events(Canvas) ->
    %% Re-attaches the OpenGL Context to Window when is [re] created/showed
    wxWindow:connect(Canvas, create, [{callback, fun(_, _) -> wxGLCanvas:setCurrent(Canvas) end}]),
    wxWindow:connect(Canvas, show, [{callback, fun(_, _) -> catch wxGLCanvas:setCurrent(Canvas) end}]),
    case os:type() of
	{unix, _} ->
	    wxWindow:connect(Canvas, paint, [{skip, true}]),
	    ok;
	{win32, _} ->
	    wxWindow:connect(Canvas, paint, [{callback, fun redraw/2}]),
	    wxWindow:connect(Canvas, erase_background, [{callback, fun redraw/2}])
    end,

    wxWindow:connect(Canvas, size),
    catch wxWindow:connect(Canvas, mouse_capture_lost), %% Not available in old wx's.

    setup_std_events(Canvas),
    wxWindow:setFocus(Canvas), %% Get keyboard focus
    ok.

%% init_context(GL) ->
%%     wxWindow:connect(GL, show),
%%     wxWindow:connect(GL, create),
%%     wxWindow:show(GL),
%%     receive #wx{event=#wxShow{}} -> ok end,
%%     %% Must be shown to initialize OpenGL context.
%%     wxGLCanvas:setCurrent(GL),
%%     wxWindow:disconnect(GL, show).

%%%%%%%%%%%%%%%%%%%%%%

redraw(#wx{obj=Canvas, event=#wxPaint{}},_) ->
    %% Must do a PaintDC and destroy it
    DC = wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    %% wings ! #wx{event=#wxPaint{}}; No need activate handle this
    ok;
redraw(_, _) ->  %% For erase background events
    wings ! #wx{event=#wxPaint{}}.

setup_std_events(Canvas) ->
    wxWindow:connect(Canvas, motion),
    wxWindow:connect(Canvas, left_up),
    wxWindow:connect(Canvas, left_down),
    wxWindow:connect(Canvas, middle_up),
    wxWindow:connect(Canvas, middle_down),
    wxWindow:connect(Canvas, left_dclick),
    wxWindow:connect(Canvas, right_up),
    wxWindow:connect(Canvas, right_down),
    wxWindow:connect(Canvas, mousewheel),
    %% wxWindow:connect(Canvas, char_hook, []),
    wxWindow:connect(Canvas, key_down, [{callback, fun key_callback_win32/2}]),
    wxWindow:connect(Canvas, key_up), %% Normally suppressed
    wxWindow:connect(Canvas, char).

key_callback_win32(Ev = #wx{event=Key=#wxKey{rawFlags=Raw}},Obj) ->
    %% See WM_SYSKEYDOWN message in msdn
    %% https://msdn.microsoft.com/en-us/library/windows/desktop/ms646286(v=vs.85).aspx
    Repeat = (Raw band (1 bsl 30)) > 1,
    %% AltGr  = (Raw band (1 bsl 24)) > 1,
    %% Repeat orelse io:format("Ev ~p~n   ~.2B => Repeat ~p AltGr ~p~n",
    %%  			    [Key, Raw, Repeat, AltGr]),
    case forward_key(Key) of
	true when Repeat -> ignore;
	%% true when AltGr -> ignore;
	true -> wings ! Ev;
	false -> wxEvent:skip(Obj)
    end.

forward_key(#wxKey{controlDown=true}) -> true;
forward_key(#wxKey{altDown=true}) -> true;
forward_key(#wxKey{metaDown=true}) -> true;
forward_key(#wxKey{shiftDown=true, keyCode=?WXK_SHIFT}) -> true;
forward_key(_) -> false.

