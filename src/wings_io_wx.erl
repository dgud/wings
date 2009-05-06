%%
%%  wings_io_wx.erl --
%%
%%     This module contains most of the low-level WX GUI for Wings.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_io_wx).

-ifdef(USE_WX).
-export([init/1, quit/0, version_info/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 get_mouse_state/0, is_modkey_pressed/1, is_key_pressed/1,
	 get_buffer/2, read_buffer/3, get_bin/1,
	 is_maximized/0, set_title/1, set_icon/1,
	 get_process_option/0,set_process_option/1
	]).
-export([change_event_handler/2, read_events/1]).
-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([reset_video_mode_for_gl/2, swapBuffers/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).  %% Keyboard and mouse mappings

-include("wings.hrl").
-import(erlang, [max/2]).
-import(lists, [flatmap/2,keysearch/3,member/2,reverse/1,reverse/2]).

-import(wings_io, [put_state/1, get_state/0]).

init(Icons) ->
    Cursors = build_cursors(),
    put_state(#io{raw_icons=Icons,cursors=Cursors}).

quit() ->
    wx:destroy().

get_process_option() ->
    Canvas = get(gl_canvas),
    What = [{wx:get_env(), Canvas}],
    What.
set_process_option([{Env, Canvas}]) ->
    wx:set_env(Env),
    wxGLCanvas:setCurrent(Canvas),
    ok.

is_maximized() ->
    wxTopLevelWindow:isMaximized(get(top_frame)).

reset_video_mode_for_gl(_W, _H) ->
    ok.

set_title(Title) ->
    wxTopLevelWindow:setTitle(get(top_frame), Title).

set_icon(IconBase) ->
    %% Use PNG instead!!
    Bmp = wxImage:new(IconBase ++ ".bmp"),
%%     Mask = wings_io:get_mask(IconBase ++ ".wbm"),
%%     wxImage:setAlpha(Bmp, Mask),
    Bitmap = wxBitmap:new(Bmp),

    Icon = wxIcon:new(),
    wxIcon:copyFromBitmap(Icon, Bitmap),
    wxFrame:setIcon(get(top_frame), Icon).

swapBuffers() ->
    wxGLCanvas:swapBuffers(get(gl_canvas)).

version_info() ->
    Ver = io_lib:format("~p.~p.~p.~p",
			[?wxMAJOR_VERSION, ?wxMINOR_VERSION,
			 ?wxRELEASE_NUMBER, ?wxSUBRELEASE_NUMBER]),
    "wxWidgets: " ++ lists:flatten(Ver).

build_cursors() ->
    [
     {stop,      wxCursor:new(?wxCURSOR_NO_ENTRY)},
     {pointing_hand, wxCursor:new(?wxCURSOR_POINT_LEFT)},
     {closed_hand, wxCursor:new(?wxCURSOR_HAND)},
     {arrow, wxCursor:new(?wxCURSOR_ARROW)},
     {hourglass, wxCursor:new(?wxCURSOR_WAIT)},
     {eyedropper, wxCursor:new(?wxCURSOR_QUESTION_ARROW)}, %% :-/  Fix me
     {blank, wxCursor:new(?wxCURSOR_BLANK)}
    ].

%% TODO
hourglass() ->
    set_cursor(hourglass),
    ok.

eyedropper() ->
    set_cursor(eyedropper),
    ok.

set_cursor(CursorId) ->
    #io{cursors=Cursors} = get_state(),
    Cursor = proplists:get_value(CursorId, Cursors),
    wxWindow:setCursor(get(gl_canvas),Cursor),
    ok.

get_mouse_state() ->
    MS = wx_misc:getMouseState(),
    #wxMouseState{x=X, y=Y,  %% integer()
		  leftDown=Left,
		  middleDown=Middle,
		  rightDown=Right %% bool()
		 } = MS,
    {gui_state([{Left,   ?SDL_BUTTON_LMASK},
		{Middle, ?SDL_BUTTON_MMASK},
		{Right,  ?SDL_BUTTON_RMASK}], 0), X, Y}.

gui_state([{true,What}|Rest],Acc) ->
    gui_state(Rest, What bor Acc);
gui_state([_|Rest],Acc) ->
    gui_state(Rest,Acc);
gui_state([],Acc) -> Acc.

is_modkey_pressed(Key) ->
    wx_misc:getKeyState(sdl_key_map(Key)).

is_key_pressed(Key) ->
    wx_misc:getKeyState(sdl_key_map(Key)).

%%%
%%% Mouse grabbing.
%%%
reset_grab() ->
    Io = get_state(),
    put_state(Io#io{grab_count=0}),
    %%sdl_mouse:showCursor(true),
    set_cursor(arrow),
    wxWindow:releaseMouse(get(gl_canvas)).

grab() ->
    %%io:format("Grab mouse~n", []),
    #io{grab_count=Cnt} = Io = get_state(),
    %%sdl_mouse:showCursor(false),
    set_cursor(blank),
    do_grab(Cnt),
    put_state(Io#io{grab_count=Cnt+1}).

do_grab(0) ->
    %% On MacOS X, we used to not do any grab. With newer versions of SDL,
    %% it seems to works better if we do a grab.
    %%
    %% Good for Linux to read out any mouse events here.
    %sdl_events:peepEvents(1, ?SDL_MOUSEMOTIONMASK),
    %sdl_video:wm_grabInput(?SDL_GRAB_ON);
    wxWindow:captureMouse(get(gl_canvas));
do_grab(_N) -> ok.

ungrab(X, Y) ->
    %%io:format("UNGRAB mouse~n", []),
    case get_state() of
	#io{grab_count=0} -> no_grab;
	#io{grab_count=Cnt}=Io ->
	    put_state(Io#io{grab_count=Cnt-1}),
	    case Cnt-1 of
		0 ->
		    wxWindow:releaseMouse(get(gl_canvas)),
		    warp(X, Y),
		    set_cursor(arrow),
		    no_grab;
		_ ->
		    still_grabbed
	    end
    end.

is_grabbed() ->
    case get_state() of
	#io{grab_count=0} -> false;
	_ -> true
    end.

warp(X, Y) ->
    wxWindow:warpPointer(get(top_frame), X, Y).


%%% Memory

get_buffer(Size,Type) ->
    wx:create_memory(mem_size(Type,Size)).

mem_size(?GL_BYTE, Size) -> Size;
mem_size(?GL_UNSIGNED_BYTE, Size) -> Size;
mem_size(?GL_UNSIGNED_SHORT, Size) -> 2*Size;
mem_size(?GL_SHORT, Size) -> 2*Size;
mem_size(?GL_UNSIGNED_INT, Size) -> 4*Size;
mem_size(?GL_INT, Size) -> 4*Size;
mem_size(?GL_FLOAT, Size) -> 4*Size;
mem_size(?GL_DOUBLE, Size) -> 8*Size.

read_buffer(Buff, Type, Size) ->
    TypeSize = mem_size(Type,1),
    Bytes = TypeSize*Size,
    <<Data:Bytes/binary, _/binary>> = wx:get_memory_bin(Buff),
    BitSize = TypeSize*8,
    [ Element || <<Element:BitSize/native>> <= Data ].
get_bin(Buff) ->
    wx:get_memory_bin(Buff).

%%%
%%% Input.
%%%

change_event_handler(?SDL_KEYUP, ?SDL_ENABLE) ->
    wxWindow:connect(get(gl_canvas), key_up);
change_event_handler(?SDL_KEYUP, ?SDL_IGNORE) ->
    wxWindow:disconnect(get(gl_canvas), key_up).

read_events(Eq0) ->
    read_events(Eq0, 0).

read_events(Eq, Wait) ->
    receive
	Ev = #wx{} ->
	    read_events(enter_event(Ev, Eq));
	{timeout,Ref,{event,Event}} when is_reference(Ref) ->
	    {Event,Eq};
	External = {external, _} ->
	    read_events(queue:in(External, Eq))
    after Wait ->
	    read_out(Eq)
    end.

enter_event(#wx{event=#wxPaint{}}, Eq) ->
    queue:in(redraw, Eq);
enter_event(#wx{event=Ev=#wxMouse{}}, Eq) ->
    queue:in(sdl_mouse(Ev), Eq);
enter_event(#wx{event=Ev=#wxKey{}}, Eq) ->
    queue:in(sdl_key(Ev), Eq);
enter_event(#wx{event=#wxClose{}}, Eq) ->
    queue:in(quit, Eq);
enter_event(#wx{event=#wxSize{size={W,H}}}, Eq) ->
    queue:in(#resize{w=W,h=H}, Eq);
enter_event(Ev, Eq) ->
    io:format("~p: Ignored ~p~n",[?MODULE, Ev]),
    Eq.

read_out(Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,Event},Eq} ->
	    {Event,Eq};
	{empty,Eq} ->
	    read_events(Eq, infinity)
    end.

read_out(Motion, Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	_Other -> {Motion,Eq0}
    end.

sdl_mouse(#wxMouse{type=Type,
		   x = X, y = Y,
		   leftDown=Left, middleDown=Middle, rightDown=Right,
		   controlDown = Ctrl, shiftDown = Shift,
		   altDown = Alt,      metaDown = Meta,
		   wheelRotation=Wheel
		  }) ->
    Mods = [{Ctrl, ?KMOD_CTRL}, {Shift, ?KMOD_SHIFT},
	    {Alt, ?KMOD_ALT}, {Meta, ?KMOD_META}],
    ModState = gui_state(Mods, 0),
    case Type of
	motion ->
	    Mouse = [{Left,   ?SDL_BUTTON_LMASK},
		     {Middle, ?SDL_BUTTON_MMASK},
		     {Right,  ?SDL_BUTTON_RMASK}],
	    MouseState = gui_state(Mouse, 0),
	    #mousemotion{which=0, state=MouseState, mod=ModState,
			 x=X,y=Y, xrel=0, yrel=0};
	left_down ->
	    #mousebutton{button=?SDL_BUTTON_LEFT, state=?SDL_PRESSED,
			 which=0, mod=ModState, x=X,y=Y};
	left_up ->
	    #mousebutton{button=?SDL_BUTTON_LEFT, state=?SDL_RELEASED,
			 which=0, mod=ModState, x=X,y=Y};
	middle_down ->
	    #mousebutton{button=?SDL_BUTTON_MIDDLE, state=?SDL_PRESSED,
			 which=0, mod=ModState, x=X,y=Y};
	middle_up ->
	    #mousebutton{button=?SDL_BUTTON_MIDDLE, state=?SDL_RELEASED,
			 which=0, mod=ModState, x=X,y=Y};
	right_down ->
	    #mousebutton{button=?SDL_BUTTON_RIGHT, state=?SDL_PRESSED,
			 which=0, mod=ModState, x=X,y=Y};
	right_up ->
	    #mousebutton{button=?SDL_BUTTON_RIGHT, state=?SDL_RELEASED,
			 which=0, mod=ModState, x=X,y=Y};
	mousewheel ->
	    Butt = case Wheel > 0 of
		       true -> ?SDL_BUTTON_X1;
		       false -> ?SDL_BUTTON_X2
		   end,
	    #mousebutton{button=Butt, state=?SDL_RELEASED,
			 which=0, mod=ModState, x=X,y=Y}
    end.


sdl_key(#wxKey{type=Type,controlDown = Ctrl, shiftDown = Shift,
	       altDown = Alt, metaDown = Meta,
	       keyCode = Code, uniChar = Uni, rawCode = Raw}) ->
    Mods = [{Ctrl, ?KMOD_CTRL}, {Shift, ?KMOD_SHIFT},
	    {Alt, ?KMOD_ALT}, {Meta, ?KMOD_META}],
    %% key_down events are always uppercase
    %% maybe we should use (the translated) char events instead?
    ModState = gui_state(Mods, 0),
    Pressed = case Type of
		  key_up -> ?SDL_PRESSED;
		  key_down -> ?SDL_RELEASED
	      end,
    #keyboard{which=0, state=Pressed, scancode=Raw, unicode=lower(Shift, Uni),
	      mod=ModState, sym=wx_key_map(lower(Shift, Code))}.

lower(false, Char) -> string:to_lower(Char);
lower(_, Char) -> Char.


wx_key_map(?WXK_ALT) -> ?KMOD_ALT;
wx_key_map(?WXK_F1) -> ?SDLK_F1;
wx_key_map(?WXK_F2) -> ?SDLK_F2;
wx_key_map(?WXK_F3) -> ?SDLK_F3;
wx_key_map(Code) -> Code.

sdl_key_map(?KMOD_ALT) ->  ?WXK_ALT;
sdl_key_map(?SDLK_F1)  ->  ?WXK_F1;
sdl_key_map(?SDLK_F2)  ->  ?WXK_F2;
sdl_key_map(?SDLK_F3)  ->  ?WXK_F3;
sdl_key_map(Key) -> Key.



-endif.
