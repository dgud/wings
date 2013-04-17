%%
%%  wings_io_wx.erl --
%%
%%     This module contains most of the low-level WX GUI for Wings.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
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
	 is_maximized/0, maximize/0, set_title/1, set_icon/1,
	 get_process_option/0,set_process_option/1
	]).
-export([batch/1, foreach/2]).
-export([change_event_handler/2, read_events/1]).
-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([reset_video_mode_for_gl/2, swapBuffers/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).  %% Keyboard and mouse mappings

-include("wings.hrl").
-import(lists, [flatmap/2,member/2,reverse/1,reverse/2]).

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

batch(Fun) ->  wx:batch(Fun).
foreach(Fun, List) -> wx:foreach(Fun, List).


is_maximized() ->
    wxTopLevelWindow:isMaximized(get(top_frame)).

maximize() ->
    wxTopLevelWindow:maximize(get(top_frame)).

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
     {blank, blank(wxCursor:new(?wxCURSOR_BLANK))}
    ].

%% TODO
hourglass() ->
    set_cursor(hourglass),
    ok.

eyedropper() ->
    set_cursor(eyedropper),
    ok.

blank(PreDef) ->
    case wxCursor:ok(PreDef) of
	true -> PreDef;
	false ->
	    wxCursor:destroy(PreDef),
	    Image = wxImage:new(16,16),
	    Black = <<0:(16*16*3*8)>>,
	    wxImage:setData(Image, Black),
	    wxImage:setMaskColour(Image, 0,0,0),
	    wxImage:setMask(Image),
	    Cursor = wxCursor:new(Image),
	    wxImage:destroy(Image),
	    Cursor
    end.

set_cursor(CursorId) ->
    #io{cursors=Cursors} = get_state(),
    Cursor = proplists:get_value(CursorId, Cursors),
    wxWindow:setCursor(get(gl_canvas),Cursor),
    ok.

get_mouse_state() ->
    wx:batch(fun() ->
		     MS = wx_misc:getMouseState(),
		     #wxMouseState{x=X0, y=Y0,  %% integer()
				   leftDown=Left,
				   middleDown=Middle,
				   rightDown=Right %% bool()
				  } = MS,
		     {X,Y} = wxWindow:screenToClient(get(gl_canvas), {X0,Y0}),
		     {gui_state([{Left,   ?SDL_BUTTON_LMASK},
				 {Middle, ?SDL_BUTTON_MMASK},
				 {Right,  ?SDL_BUTTON_RMASK}], 0), X, Y}
	     end).

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
    put(wm_cursor, arrow),
    set_cursor(arrow),
    wxWindow:releaseMouse(get(gl_canvas)).

grab() ->
    %%io:format("Grab mouse~n", []),
    #io{grab_count=Cnt} = Io = get_state(),
    %%sdl_mouse:showCursor(false),
    put(wm_cursor, blank),
    set_cursor(blank),
    do_grab(Cnt),
    put_state(Io#io{grab_count=Cnt+1}).

do_grab(0) ->
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
		    put(wm_cursor, arrow),
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
	Ev = #wx{event=#wxKey{keyCode=Code}} ->
	    case Code of
		%% We don't want modifiers as keypresses,
		%% we get it on windows
		?WXK_ALT     -> read_events(Eq, Wait);
		?WXK_SHIFT   -> read_events(Eq, Wait);
		?WXK_CONTROL -> read_events(Eq, Wait);
		_ ->
		    read_events(queue:in(Ev, Eq))
	    end;
	Ev = #wx{} ->
	    read_events(queue:in(Ev, Eq));
	{timeout,Ref,{event,Event}} when is_reference(Ref) ->
	    {Event,Eq};
	External = {external, _} ->
	    read_events(queue:in(External, Eq))
    after Wait ->
	    R = read_out(Eq),
	    R
    end.

read_out(Eq0) ->
    case queue:out(Eq0) of
	{{value,#wx{event=#wxMouse{}}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,#wx{event=#wxPaint{}}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,#wx{event=#wxSize{}}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,#wx{} = Event},Eq} ->
	    {wx_translate(Event),Eq};
	{{value,Event},Eq} ->
	    {Event,Eq};
	{empty,Eq} ->
	    read_events(Eq, infinity)
    end.

read_out(Event=#wx{event=Rec1}, Eq0) ->
    case queue:out(Eq0) of
	{{value,New=#wx{event=Rec2}},Eq}
	  when element(2,Rec1) =:= element(2,Rec2) ->
	    read_out(New, Eq);
	{{value,#wx{event=Rec2}},Eq}
	  when element(2,Rec1) =:= size,
	       element(2,Rec2) =:= paint ->
	    %% wx sends along a paint with each size event.
	    read_out(Event, Eq);
	_Other ->
	    {wx_translate(Event),Eq0}
    end.

wx_translate(Event) ->
    R = wx_translate_1(Event),
    %%erlang:display(R),
    R.

wx_translate_1(#wx{event=#wxPaint{}}) ->
    redraw;
wx_translate_1(#wx{event=Ev=#wxMouse{}}) ->
    sdl_mouse(Ev);
wx_translate_1(#wx{event=Ev=#wxKey{}}) ->
    sdl_key(Ev);
wx_translate_1(#wx{event=#wxClose{}}) ->
    quit;
wx_translate_1(#wx{event=#wxSize{size={W,H}}}) ->
    #resize{w=W,h=H};
wx_translate_1(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
    wings_menu:wx_command_event(Id);
wx_translate_1(Ev) ->
    io:format("~p: Bug Ignored Event~p~n",[?MODULE, Ev]),
    redraw.

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
		  key_up -> ?SDL_RELEASED;
		  key_down -> ?SDL_PRESSED
	      end,
    #keyboard{which=0, state=Pressed, scancode=Raw, unicode=lower(Shift, Uni),
	      mod=ModState, sym=wx_key_map(lower(Shift, Code))}.

lower(false, Char) -> string:to_lower(Char);
lower(_, Char) -> Char.

wx_key_map(?WXK_ALT) -> ?KMOD_ALT;
wx_key_map(?WXK_F1) -> ?SDLK_F1;
wx_key_map(?WXK_F2) -> ?SDLK_F2;
wx_key_map(?WXK_F3) -> ?SDLK_F3;
wx_key_map(?WXK_F4) -> ?SDLK_F4;
wx_key_map(?WXK_F5) -> ?SDLK_F5;
wx_key_map(?WXK_F6) -> ?SDLK_F6;
wx_key_map(?WXK_F7) -> ?SDLK_F7;
wx_key_map(?WXK_F8) -> ?SDLK_F8;
wx_key_map(?WXK_F9) -> ?SDLK_F9;
wx_key_map(?WXK_F10) -> ?SDLK_F10;
wx_key_map(?WXK_F11) -> ?SDLK_F11;
wx_key_map(?WXK_F12) -> ?SDLK_F12;
wx_key_map(?WXK_F13) -> ?SDLK_F13;
wx_key_map(?WXK_F14) -> ?SDLK_F14;
wx_key_map(?WXK_F15) -> ?SDLK_F15;

wx_key_map(?WXK_UP)    -> ?SDLK_UP;
wx_key_map(?WXK_LEFT)  -> ?SDLK_LEFT;
wx_key_map(?WXK_DOWN)  -> ?SDLK_DOWN;
wx_key_map(?WXK_RIGHT) -> ?SDLK_RIGHT;

%%wx_key_map(?WXK_DELETE) -> ?SDLK_DELETE; same
wx_key_map(?WXK_INSERT) -> ?SDLK_INSERT;
wx_key_map(?WXK_END) -> ?SDLK_END;
wx_key_map(?WXK_HOME) -> ?SDLK_HOME;
wx_key_map(?WXK_PAGEUP) -> ?SDLK_PAGEUP;
wx_key_map(?WXK_PAGEDOWN) -> ?SDLK_PAGEDOWN;

wx_key_map(?WXK_NUMPAD0) -> ?SDLK_KP0;
wx_key_map(?WXK_NUMPAD1) -> ?SDLK_KP1;
wx_key_map(?WXK_NUMPAD2) -> ?SDLK_KP2;
wx_key_map(?WXK_NUMPAD3) -> ?SDLK_KP3;
wx_key_map(?WXK_NUMPAD4) -> ?SDLK_KP4;
wx_key_map(?WXK_NUMPAD5) -> ?SDLK_KP5;
wx_key_map(?WXK_NUMPAD6) -> ?SDLK_KP6;
wx_key_map(?WXK_NUMPAD7) -> ?SDLK_KP7;
wx_key_map(?WXK_NUMPAD8) -> ?SDLK_KP8;
wx_key_map(?WXK_NUMPAD9) -> ?SDLK_KP9;
wx_key_map(?WXK_NUMPAD_MULTIPLY) -> ?SDLK_KP_MULTIPLY;
wx_key_map(?WXK_NUMPAD_ADD)      -> ?SDLK_KP_PLUS;
wx_key_map(?WXK_NUMPAD_SUBTRACT) -> ?SDLK_KP_MINUS;
wx_key_map(?WXK_NUMPAD_DECIMAL)  -> ?SDLK_KP_PERIOD;
wx_key_map(?WXK_NUMPAD_DIVIDE)   -> ?SDLK_KP_DIVIDE;
wx_key_map(?WXK_NUMPAD_ENTER)    -> ?SDLK_KP_ENTER;

wx_key_map(?WXK_WINDOWS_LEFT)  -> ?SDLK_LSUPER;
wx_key_map(?WXK_WINDOWS_RIGHT) -> ?SDLK_RSUPER;
%%wx_key_map(?) -> ?;
wx_key_map(Code) -> Code.

sdl_key_map(?KMOD_ALT) ->  ?WXK_ALT;
sdl_key_map(?SDLK_F1)  ->  ?WXK_F1;
sdl_key_map(?SDLK_F2)  ->  ?WXK_F2;
sdl_key_map(?SDLK_F3)  ->  ?WXK_F3;
sdl_key_map(?SDLK_F4)  ->  ?WXK_F4;
sdl_key_map(?SDLK_F5)  ->  ?WXK_F5;
sdl_key_map(?SDLK_F6)  ->  ?WXK_F6;
sdl_key_map(?SDLK_F7)  ->  ?WXK_F7;
sdl_key_map(?SDLK_F8)  ->  ?WXK_F8;
sdl_key_map(?SDLK_F9)  ->  ?WXK_F9;
sdl_key_map(?SDLK_F10)  ->  ?WXK_F10;
sdl_key_map(?SDLK_F11)  ->  ?WXK_F11;
sdl_key_map(?SDLK_F12)  ->  ?WXK_F12;
sdl_key_map(?SDLK_F13)  ->  ?WXK_F13;
sdl_key_map(?SDLK_F14)  ->  ?WXK_F14;
sdl_key_map(?SDLK_F15)  ->  ?WXK_F15;

sdl_key_map(?SDLK_UP)    -> ?WXK_UP;
sdl_key_map(?SDLK_LEFT)  -> ?WXK_LEFT;
sdl_key_map(?SDLK_DOWN)  -> ?WXK_DOWN;
sdl_key_map(?SDLK_RIGHT) -> ?WXK_RIGHT;

%%sdl_key_map(?SDLK_DELETE) -> ?WXK_DELETE; same
sdl_key_map(?SDLK_INSERT) -> ?WXK_INSERT;
sdl_key_map(?SDLK_END) -> ?WXK_END;
sdl_key_map(?SDLK_HOME) -> ?WXK_HOME;
sdl_key_map(?SDLK_PAGEUP) -> ?WXK_PAGEUP;
sdl_key_map(?SDLK_PAGEDOWN) -> ?WXK_PAGEDOWN;

sdl_key_map(?SDLK_KP0) -> ?WXK_NUMPAD0;
sdl_key_map(?SDLK_KP1) -> ?WXK_NUMPAD1;
sdl_key_map(?SDLK_KP2) -> ?WXK_NUMPAD2;
sdl_key_map(?SDLK_KP3) -> ?WXK_NUMPAD3;
sdl_key_map(?SDLK_KP4) -> ?WXK_NUMPAD4;
sdl_key_map(?SDLK_KP5) -> ?WXK_NUMPAD5;
sdl_key_map(?SDLK_KP6) -> ?WXK_NUMPAD6;
sdl_key_map(?SDLK_KP7) -> ?WXK_NUMPAD7;
sdl_key_map(?SDLK_KP8) -> ?WXK_NUMPAD8;
sdl_key_map(?SDLK_KP9) -> ?WXK_NUMPAD9;
sdl_key_map(?SDLK_KP_MULTIPLY) -> ?WXK_NUMPAD_MULTIPLY;
sdl_key_map(?SDLK_KP_PLUS)      -> ?WXK_NUMPAD_ADD;
sdl_key_map(?SDLK_KP_MINUS) -> ?WXK_NUMPAD_SUBTRACT;
sdl_key_map(?SDLK_KP_PERIOD)  -> ?WXK_NUMPAD_DECIMAL;
sdl_key_map(?SDLK_KP_DIVIDE)   -> ?WXK_NUMPAD_DIVIDE;
sdl_key_map(?SDLK_KP_ENTER)    -> ?WXK_NUMPAD_ENTER;

sdl_key_map(?SDLK_LSUPER)  -> ?WXK_WINDOWS_LEFT;
sdl_key_map(?SDLK_RSUPER) -> ?WXK_WINDOWS_RIGHT;

sdl_key_map(Key) -> Key.

-endif.
