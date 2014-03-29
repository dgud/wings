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
-export([init/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-ifndef(WX_GL_SAMPLE_BUFFERS).     %% New in wxWidgets-3.0
-define(WX_GL_SAMPLE_BUFFERS,17).  %% 1 for multisampling support (antialiasing)
-define(WX_GL_SAMPLES,18).         %% 4 for 2x2 antialiasing supersampling on most graphics cards
-endif.

init() ->
    wx:new(),
    macosx_workaround(),
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),

    wings_pref:set_default(window_size, {780,570}),
    TopSize = wings_pref:get_value(window_size),
    Frame = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),

    GLAttrs = [?WX_GL_RGBA,
	       ?WX_GL_MIN_RED,8,?WX_GL_MIN_GREEN,8,?WX_GL_MIN_BLUE,8,
	       ?WX_GL_DEPTH_SIZE, 24, ?WX_GL_STENCIL_SIZE, 8,
	       ?WX_GL_DOUBLEBUFFER,
	       ?WX_GL_SAMPLE_BUFFERS,1,
	       ?WX_GL_SAMPLES, 4,
	       0],
    Canvas = wxGLCanvas:new(Frame, [
				    {attribList, GLAttrs},
				    {style,
				     %% Let us handle redrawing (GTK)
				     ?wxTRANSPARENT_WINDOW bor
					 ?wxFULL_REPAINT_ON_RESIZE}
				   ]),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Canvas, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:setSizeHints(Sizer, Canvas),
    wxFrame:setSizer(Frame, Sizer),

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

    put(top_frame, Frame),
    put(gl_canvas, Canvas),

    Redraw = fun(Ev,_) ->
		     wxGLCanvas:setCurrent(Canvas),
		     DC = wxPaintDC:new(Canvas),
		     wxPaintDC:destroy(DC),
		     wings ! Ev
		     %%, io:format("Refresh (~p)~n", [wxWindow:getSize(Canvas)])
	     end,

    wxWindow:connect(Frame, close_window),
    wxWindow:connect(Frame, command_menu_selected, [skip]),
    %% MenuCB = fun(Ev,Obj) ->
    %% 		     io:format("Menu ~p~n", [Ev]),
    %% 		     wxEvent:skip(Obj)
    %% 	     end,
    %% wxWindow:connect(Frame, menu_open, [{callback, MenuCB}]),
    %% wxWindow:connect(Frame, menu_highlight, [{callback, MenuCB}]),
    %% wxWindow:connect(Frame, menu_close, [{callback, MenuCB}]),

    wxWindow:connect(Canvas, paint, [{callback, Redraw}]),
    wxWindow:connect(Canvas, size,  []),
    %% wxWindow:connect(Canvas, erase_background),
    %% wxWindow:connect(Canvas, enter_window,
    %% 		     [{callback, fun(_, _) ->
    %% 					 wxWindow:setFocus(Canvas)
    %% 				 end}]),

    wxWindow:connect(Canvas, motion, [{skip, true}]),
    wxWindow:connect(Canvas, left_up),
    wxWindow:connect(Canvas, left_down),
    wxWindow:connect(Canvas, middle_up),
    wxWindow:connect(Canvas, middle_down),
    wxWindow:connect(Canvas, right_up),
    wxWindow:connect(Canvas, right_down),
    wxWindow:connect(Canvas, mousewheel),
    wxWindow:connect(Canvas, char_hook, []),
    wxWindow:connect(Canvas, key_down),
    set_icon(),
    wxWindow:setFocus(Canvas), %% Get keyboard focus
    wxWindow:connect(Frame, show),

    wxWindow:show(Frame),   %% Must be shown to initilize context.

    receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
    wxGLCanvas:setCurrent(Canvas),

    wings_gl:init_extensions(),
    wings_gl:init_restrictions(),

    try
	Msgs0 = wxe_master:fetch_msgs(),
	Msgs = lists:flatten([io_lib:format("~p~n",[Msg]) || Msg <- Msgs0]),
	Msgs == [] orelse wxMessageDialog:showModal(wxMessageDialog:new(Frame, Msgs, [{caption, "OpenMsgs"}]))
    catch error:undef -> ok %% Req new wx
    end,
    ok.

set_icon() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Ebin,
			     case os:type() of
				 {unix,darwin} -> "wings_icon_big";
				 _ -> "wings_icon_small"
			     end),
    wings_io:set_icon(IconFile).

macosx_workaround() ->
    try 1.0/zero()
    catch
	error:_ -> ok
    end.

zero() ->
    0.0.

