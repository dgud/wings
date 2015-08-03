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

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).  %% Keyboard and mouse mappings

-include("wings.hrl").

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

-export([make_key_event/1]).

-import(lists, [flatmap/2,member/2,reverse/1,reverse/2]).

-import(wings_io, [put_state/1, get_state/0]).

init(Icons) ->
    Cursors = build_cursors(),
    put_state(#io{raw_icons=Icons,cursors=Cursors}).

quit() ->
    Frame = get(top_frame),
    wxFrame:destroy(Frame),
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
    %% Needed on mac for some reason
    wxGLCanvas:setCurrent(get(gl_canvas)),
    ok.

set_title(Title) ->
    wxTopLevelWindow:setTitle(get(top_frame), Title).

set_icon(IconBase) ->
    Bmp = wxImage:new(IconBase ++ ".png"),
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
     {pointing_hand, wxCursor:new(?wxCURSOR_HAND)},
     {closed_hand, wxCursor:new(?wxCURSOR_BULLSEYE)},
     {arrow, wxCursor:new(?wxCURSOR_ARROW)},
     {hourglass, wxCursor:new(?wxCURSOR_WAIT)},
     {eyedropper, wxCursor:new(?wxCURSOR_MAGNIFIER)}, %% :-/  Fix me
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
    wx_misc:setCursor(Cursor),
    put(active_cursor, CursorId),
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

is_modkey_pressed(Key) when (Key band ?KMOD_LCTRL) > 0 ->
    wx_misc:getKeyState(?WXK_CONTROL);
is_modkey_pressed(Key) when (Key band ?KMOD_LALT)  > 0 ->
    wx_misc:getKeyState(?WXK_ALT);
is_modkey_pressed(Key) when (Key band ?KMOD_LSHIFT) > 0 ->
    wx_misc:getKeyState(?WXK_SHIFT);
is_modkey_pressed(Key) when (Key band ?KMOD_META) > 0 ->
    wx_misc:getKeyState(?WXK_WINDOWS_LEFT) orelse
	wx_misc:getKeyState(?WXK_WINDOWS_RIGHT).


is_key_pressed(Key) ->
    wx_misc:getKeyState(sdl_key_map(Key)).

%%%
%%% Mouse grabbing.
%%%
reset_grab() ->
    reset_grab(true).

reset_grab(Release) ->
    Io = get_state(),
    put_state(Io#io{grab_count=0}),
    %%sdl_mouse:showCursor(true),
    put(wm_cursor, arrow),
    set_cursor(arrow),
    Release andalso wxWindow:releaseMouse(get(gl_canvas)).

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
    case get_state() of
	#io{key_up=true} -> false;
	Io -> put_state(Io#io{key_up=true})
    end;
change_event_handler(?SDL_KEYUP, ?SDL_IGNORE) ->
    case get_state() of
	#io{key_up=false} -> false;
	Io -> put_state(Io#io{key_up=false})
    end.

read_events(Eq0) ->
    R = read_events(Eq0, 0),
    %% Eq0 =:= {[],[]} orelse io:format("Q ~P~n",[Eq0, 10]),
    %% case R of
    %% 	{{wm,{_,console,_}}, _} -> ok;
    %% 	{#mousemotion{state=0},_} -> ok;
    %% 	{Ev, _} -> io:format("Ev: ~p ~P~n", [erlang:system_time(), Ev, 30]);
    %% 	_ -> ok
    %% end,
    R.

read_events(Eq0, Wait) ->
    receive
	#wx{} = Ev ->
	    read_events(queue:in(Ev, Eq0), 0);
	{timeout,Ref,{event,Event}} when is_reference(Ref) ->
	    {Event,Eq0};
	External = {external, _} ->
	    read_events(queue:in(External, Eq0), 0)
    after Wait ->
	    case read_one(Eq0) of
		{Ev = #wxMouse{type=motion}, Eq} -> 
		    get_motion(Ev, Eq); % Throw old motions
		{empty, Eq} ->
		    read_events(Eq, infinity);
		{Ev, Eq} ->
		    {wx_translate(Ev), Eq}
	    end
    end.

read_one(Eq0) ->
    case queue:out(Eq0) of
	{{value,#wx{event=#wxMouse{type=motion}=Event}},Eq} ->
	    case put(prev_mouse, Event) of
		Event -> read_one(Eq);
		_ -> {Event, Eq}
	    end;
	{{value,#wx{event=#wxKey{type=key_up}=Event}},Eq} ->
	    erase(prev_key),
	    case get_state() of
		#io{key_up=true} -> {Event, Eq};
		_ -> read_one(Eq)
	    end;
	{{value,#wx{event=#wxKey{}=Event}},Eq} ->
	    %% Avoid Keyboard repeat
	    case put(prev_key, Event) of
		Event -> read_one(Eq);
		_ -> {Event, Eq}
	    end;
	{{value,Event},Eq} ->
	    {Event,Eq};
	Empty = {empty,_} ->
	    Empty
    end.

get_motion(Motion, Eq0) ->
    case read_one(Eq0) of
	{New=#wxMouse{type=motion},Eq} ->
	    get_motion(New, Eq);
	_Other ->
	    {wx_translate(Motion),Eq0}
    end.

wx_translate(Event) ->
    R = wx_translate_1(Event),
    %%erlang:display(R),
    R.

wx_translate_1(Ev=#wxMouse{}) ->
    sdl_mouse(Ev); % Motion
wx_translate_1(#wx{event=Ev=#wxMouse{}}) ->
    sdl_mouse(Ev); % Mouse Buttons
wx_translate_1(Ev=#wxKey{}) ->
    R = sdl_key(Ev),
    %% erlang:display({sdlkey, R}),
    R;
wx_translate_1(#wx{event=#wxPaint{}}) ->
    #expose{active=true};
wx_translate_1(#wx{event=#wxSize{size={W,H}}}) ->
    #resize{w=W,h=H};
wx_translate_1(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
    Name = wings_menu:id_to_name(Id),
    ME = case ets:match(wings_state, {{bindkey,'$1'}, Name, '_'}) of
	     [] -> {menubar, {action, Name}};
	     [[KeyComb]|_] -> make_key_event(KeyComb)
	 end,
    %% io:format("ME ~p~n",[ME]),
    ME;
wx_translate_1(#wx{event={wxMouseCaptureLost, _}}) ->
    reset_grab(false),
    grab_lost;
wx_translate_1(#wx{event=#wxActivate{active=Active}}) ->
    Active == true andalso wxWindow:setFocus(get(gl_canvas)),
    #expose{active=Active};
wx_translate_1(Ev) ->
    Ev.

sdl_mouse(M=#wxMouse{type=Type,
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
			 which=0, mod=ModState, x=X,y=Y};
	enter_window -> M;
	leave_window -> M
    end.

sdl_key(#wxKey{type=Type,controlDown = Ctrl, shiftDown = Shift,
	       altDown = Alt, metaDown = Meta,
	       keyCode = Code, uniChar = Uni0, rawCode = Raw}) ->
    Mods = [{Shift, ?KMOD_SHIFT},{Ctrl, ?KMOD_CTRL},
	    {Alt, ?KMOD_ALT}, {Meta, ?KMOD_META}],
    %% key_down events are always uppercase
    %% maybe we should use (the translated) char events instead?
    ModState = gui_state(Mods, 0),
    Pressed = case Type of
		  char -> ?SDL_PRESSED;
		  char_hook -> ?SDL_PRESSED;
		  key_up    -> ?SDL_RELEASED;
		  key_down  -> ?SDL_PRESSED
	      end,
    {Uni, Sym} = case wx_key_map(Code) of
		     undefined -> {lower(ModState, Uni0), lower(ModState, Code)};
		     Sym0 -> {0, lower(ModState, Sym0)}
		 end,
    %% io:format("EV ~p: ~p ~p ~p ~p ~p ~p => ~p ~p ~n",
    %% 	      [Type, Ctrl, Shift, Alt, Meta, Code, Uni0, Sym, Uni]),
    #keyboard{which=0, state=Pressed, scancode=Raw, unicode=Uni,
	      mod=ModState, sym=Sym}.

lower([], Char) -> Char;
lower(?KMOD_SHIFT, Char) -> Char;
lower(_, Char) ->
    string:to_lower(Char).

make_key_event({Key, Mods}) ->
    Map = fun(ctrl) -> {true, ?KMOD_CTRL};
	     (alt)  -> {true, ?KMOD_ALT};
	     (meta) -> {true, ?KMOD_META};
	     (shift) -> {true, ?KMOD_SHIFT};
	     (command) -> {true, ?KMOD_META}
	  end,
    ModState = gui_state([Map(Mod) || Mod <- Mods], 0),
    %% io:format("make key {~p, ~p} => ~p ~n",[Key, Mods, ModState]),
    #keyboard{which=menubar, state=true, unicode=Key, mod=ModState, sym=Key};
make_key_event(Key) when is_integer(Key) ->
    make_key_event({Key, []}).

wx_key_map(?WXK_SHIFT) -> ?SDLK_LSHIFT;
wx_key_map(?WXK_ALT) -> ?SDLK_LALT;
wx_key_map(?WXK_CONTROL) -> ?SDLK_LCTRL;
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

wx_key_map(?WXK_DELETE) -> ?SDLK_DELETE; %% same
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
wx_key_map(_) -> undefined.

sdl_key_map(?SDLK_LSHIFT) -> ?WXK_SHIFT;
sdl_key_map(?SDLK_LALT) -> ?WXK_ALT;
sdl_key_map(?SDLK_LCTRL) -> ?WXK_CONTROL;

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

sdl_key_map(?SDLK_DELETE) -> ?WXK_DELETE; %% same
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
