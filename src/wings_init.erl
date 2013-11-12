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

-ifndef(USE_WX).
init() ->
    macosx_workaround(),
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),

    wings_pref:set_default(window_size, {780,570}),
    TopSize = wings_pref:get_value(window_size),
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_NOPARACHUTE),
    set_icon(),
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),

    %% Make sure that some video mode works. Otherwise crash early.
    %% From best to worst.
    try
	sdl_video:gl_setAttribute(?SDL_GL_MULTISAMPLEBUFFERS, 1),
	sdl_video:gl_setAttribute(?SDL_GL_MULTISAMPLESAMPLES, 4),
	try_video_modes(opengl_modes(), TopSize)
    catch
	error:_ ->
	io:fwrite("\nRetrying with multisampling disabled.\n"),
	sdl_video:gl_setAttribute(?SDL_GL_MULTISAMPLEBUFFERS, 0),
	sdl_video:gl_setAttribute(?SDL_GL_MULTISAMPLESAMPLES, 0),
	try_video_modes(opengl_modes(), TopSize)
    end,
    wings_gl:init_extensions(),
    wings_gl:init_restrictions(),

    %% Initialize event handling and other stuff.
    sdl_events:eventState(?SDL_ALLEVENTS,?SDL_IGNORE),
    sdl_events:eventState(?SDL_MOUSEMOTION, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONUP, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_KEYDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEOEXPOSE, ?SDL_ENABLE),
    sdl_keyboard:enableUNICODE(true),
    sdl_keyboard:enableKeyRepeat(?SDL_DEFAULT_REPEAT_DELAY,
				 ?SDL_DEFAULT_REPEAT_INTERVAL),
    ok.

opengl_modes() ->
    [[{buffer_size,32},{depth_size,32},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,32},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,24},{stencil_size,8},{accum_size,16}],
     [{buffer_size,24},{depth_size,24},{stencil_size,0},{accum_size,16}],
     [{buffer_size,16},{depth_size,24},{stencil_size,8},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,8},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,0},{accum_size,16}],
     [{buffer_size,16},{depth_size,16},{stencil_size,0},{accum_size,0}],
     [{buffer_size,15},{depth_size,16},{stencil_size,8},{accum_size,16}],
     [{buffer_size,15},{depth_size,16},{stencil_size,0},{accum_size,16}],
     [{buffer_size,15},{depth_size,16},{stencil_size,0},{accum_size,0}],

     %% Fallback - use default for all.
     [{buffer_size,0},{depth_size,0},{stencil_size,0},{accum_size,0}]].

try_video_modes(Modes, TopSize) ->
    io:put_chars(?__(1,"Trying OpenGL modes\n")),
    case try_video_modes_1(Modes, TopSize) of
	ok -> ok;
	error -> video_mode_failure()
    end.

video_mode_failure() ->
    io:format("\n###########################################\n\n"),
    io:format(?__(2,"Failed to find any suitable OpenGL mode.\n\n")),
    io:format(?__(3,"Make sure that OpenGL drivers are installed.\n\n")),
    io:format("\n###########################################\n\n"),
    error(?__(5,"No suitable OpenGL mode found (are OpenGL drivers installed?)")).

try_video_modes_1([Mode|Modes], TopSize) ->
    io:format("  ~p\n", [Mode]),
    case try_video_mode(Mode, TopSize) of
	ok -> ok;
	error -> try_video_modes_1(Modes, TopSize)
    end;
try_video_modes_1([], _) -> error.

try_video_mode(Ps, {W,H}) ->
    set_video_props(Ps),
    case catch set_video_mode(W, H) of
	ok ->
	    display_actual_mode(),
	    ok;
	_ -> error
    end.

set_video_props([{Prop,Val}|Ps]) ->
    set_video_prop(Prop, Val),
    set_video_props(Ps);
set_video_props([]) -> ok.

set_video_prop(buffer_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_BUFFER_SIZE, Bits);
set_video_prop(depth_size, Depth) ->
    sdl_video:gl_setAttribute(?SDL_GL_DEPTH_SIZE, Depth);
set_video_prop(stencil_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_STENCIL_SIZE, Bits);
set_video_prop(accum_size, Bits) ->
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_RED_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_GREEN_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_BLUE_SIZE, Bits),
    sdl_video:gl_setAttribute(?SDL_GL_ACCUM_ALPHA_SIZE, Bits).

display_actual_mode() ->
    Attrs = [?GL_RED_BITS,
	     ?GL_GREEN_BITS,
	     ?GL_BLUE_BITS,
	     ?GL_ALPHA_BITS,
	     ?GL_DEPTH_BITS,
	     ?GL_STENCIL_BITS,
	     ?GL_ACCUM_RED_BITS,
	     ?GL_ACCUM_GREEN_BITS,
	     ?GL_ACCUM_BLUE_BITS,
	     ?GL_ACCUM_ALPHA_BITS],
    io:format(?__(1,"Actual: RGBA: ~p ~p ~p ~p Depth: ~p Stencil: ~p Accum: ~p ~p ~p ~p\n"),
	      [hd(gl:getIntegerv(A)) || A <- Attrs]).

set_video_mode(W, H) ->
    {surfacep,_} = sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE),
    ok.

-else.

init() ->
    wx:new(),
    macosx_workaround(),
    os:putenv("SDL_HAS3BUTTONMOUSE", "true"),

    wings_pref:set_default(window_size, {780,570}),
    TopSize = wings_pref:get_value(window_size),
    Frame = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),

    MB = wxMenuBar:new(),
    wxFrame:setMenuBar(Frame, MB),

    GLAttrs = [?WX_GL_RGBA,
	       ?WX_GL_MIN_RED,8,?WX_GL_MIN_GREEN,8,?WX_GL_MIN_BLUE,8,
	       ?WX_GL_DEPTH_SIZE, 24, ?WX_GL_STENCIL_SIZE, 8,
	       ?WX_GL_DOUBLEBUFFER,0],
    Canvas = wxGLCanvas:new(Frame, [{attribList, GLAttrs},
				    {style,
				     %% Let us handle redrawing (GTK)
				     ?wxTRANSPARENT_WINDOW bor
					 ?wxFULL_REPAINT_ON_RESIZE}
				   ]),

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
    wxWindow:connect(Frame, command_menu_selected),
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
    wxWindow:connect(Canvas, key_down),
    set_icon(),
    wxWindow:setFocus(Canvas), %% Get keyboard focus
    wxWindow:connect(Frame, show),
    wxWindow:show(Frame),   %% Must be shown to initilize context.
    receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
    wxGLCanvas:setCurrent(Canvas),

    wings_gl:init_extensions(),
    wings_gl:init_restrictions(),

    ok.
-endif.

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

