%%
%%  wings_init.erl --
%%
%%     Initialization of Wings video and event handling.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_init).
-export([init/0, gl_attributes/0]).

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

init() ->
    wx:new(),
    macosx_workaround(),
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),

    wings_pref:set_default(window_size, {780,570}),
    TopSize = wings_pref:get_value(window_size),
    Frame = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),

    Style = ?wxFULL_REPAINT_ON_RESIZE,
    Canvas = wxGLCanvas:new(Frame, [{attribList, gl_attributes()},
				    {style, Style}]),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Canvas, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:setSizeHints(Sizer, Canvas),
    wxFrame:setSizer(Frame, Sizer),

    put(top_frame, Frame),
    put(gl_canvas, Canvas),

    case os:type() of
	{unix, _} ->  wxWindow:connect(Canvas, paint, [skip]);
	{win32, _} -> 
	    wxWindow:connect(Canvas, paint, [{callback, fun redraw/2}]),
	    wxWindow:connect(Frame, erase_background, [{callback, fun redraw/2}])
    end,

    wxWindow:connect(Canvas, size),
    wxWindow:connect(Frame, close_window),
    wxWindow:connect(Frame, command_menu_selected, [skip]),
    wxWindow:connect(Frame, activate, [skip]),
    try wxWindow:connect(Canvas, mouse_capture_lost)
    catch _:_ -> ok end, %% Not available in all wx versions yet.

    setup_std_events(Canvas),
    set_icon(),
    wxWindow:setFocus(Canvas), %% Get keyboard focus
    wxWindow:connect(Frame, show),

    %% Must be shown to initilize OpenGL context.
    wxWindow:show(Frame),
    receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
    wxGLCanvas:setCurrent(Canvas),

    wings_gl:init_extensions(),
    wings_gl:init_restrictions(),

    %% On the Mac, if Wings was started by clicking on a .wings file,
    %% we must retrieve the name of the file here.
    Msgs0 = wxe_master:fetch_msgs(),
    Msgs = [F || F <- Msgs0, filelib:is_regular(F)],
    case Msgs of
	[F|_] -> F;
	[] -> none
    end.

redraw(#wx{obj=Canvas, event=#wxPaint{}},_) ->
    %% Must do a PaintDC and destroy it
    DC = wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    wings ! #wx{event=#wxPaint{}};
redraw(_, _) ->  %% For erase background events
    wings ! #wx{event=#wxPaint{}}.

setup_std_events(Canvas) ->
    wxWindow:connect(Canvas, motion),
    wxWindow:connect(Canvas, left_up),
    wxWindow:connect(Canvas, left_down),
    wxWindow:connect(Canvas, middle_up),
    wxWindow:connect(Canvas, middle_down),
    wxWindow:connect(Canvas, right_up),
    wxWindow:connect(Canvas, right_down),
    wxWindow:connect(Canvas, mousewheel),
    %% wxWindow:connect(Canvas, char_hook, []),
    wxWindow:connect(Canvas, key_down, [{callback, fun key_callback/2}]),
    wxWindow:connect(Canvas, key_up), %% Normally suppressed
    wxWindow:connect(Canvas, char).

key_callback(Ev = #wx{event=Key},Obj) ->
    %% io:format("Ev ~p~n",[Ev]),
    case forward_key(Key) of
	true -> wings ! Ev;
	false -> wxEvent:skip(Obj)
    end.

forward_key(#wxKey{controlDown=true}) -> true;
forward_key(#wxKey{altDown=true}) -> true;
forward_key(#wxKey{metaDown=true}) -> true;
forward_key(#wxKey{shiftDown=true, keyCode=?WXK_SHIFT}) -> true;
forward_key(_) -> false.

set_icon() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Ebin,
			     case os:type() of
				 {unix,darwin} -> "wings_icon_379x379";
				 _ -> "wings_icon_379x379"
			     end),
    wings_io:set_icon(IconFile).

macosx_workaround() ->
    try 1.0/zero()
    catch
	error:_ -> ok
    end.

zero() ->
    0.0.

%% setup_aui(Frame) ->
    %% Manager = wxAuiManager:new([{managed_wnd, Frame}]),
    %% Pane = wxAuiPaneInfo:new(),
    %% wxAuiPaneInfo:centrePane(Pane),
    %% wxAuiPaneInfo:paneBorder(Pane, [{visible, false}]),
    %% wxAuiManager:addPane(Manager, Canvas, Pane),
    %% %% Test
    %% TextCtrl = wxTextCtrl:new(Frame, ?wxID_ANY,
    %% 			      [{size, {300,200}},
    %% 			       {value, "An empty pane"},
    %% 			       {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    %% wxAuiManager:addPane(Manager, TextCtrl,
    %% 			 wxAuiPaneInfo:caption(
    %% 			   wxAuiPaneInfo:right(
    %% 			     wxAuiPaneInfo:new()), "One")),
    %% wxAuiManager:update(Manager),
