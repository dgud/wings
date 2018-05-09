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

-export([init/0, quit/0, version_info/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 get_mouse_state/0, is_modkey_pressed/1, is_key_pressed/1,
	 get_buffer/2, read_buffer/3, get_bin/1,
	 is_maximized/0, maximize/0, set_title/1, set_icon/2,
	 get_process_option/0,set_process_option/1
	]).
-export([batch/1, foreach/2]).
-export([change_event_handler/2, read_events/1]).
-export([reset_grab/0,grab/1,ungrab/2,is_grabbed/0,warp/3]).
-export([reset_video_mode_for_gl/2]).

-export([make_key_event/1]).

-import(lists, [flatmap/2,member/2,reverse/1,reverse/2]).

-import(wings_io, [put_state/1, get_state/0]).

init() ->
    Cursors = build_cursors(),
    ?SET(cursors, Cursors).

quit() ->
    Frame = ?GET(top_frame),
    wxFrame:destroy(Frame),
    wx:destroy().

get_process_option() ->
    Canvas = ?GET(gl_canvas),
    What = [{wx:get_env(), Canvas}],
    What.
set_process_option([{Env, Canvas}]) ->
    wx:set_env(Env),
    wxGLCanvas:setCurrent(Canvas),
    ok.

batch(Fun) ->  wx:batch(Fun).
foreach(Fun, List) -> wx:foreach(Fun, List).

is_maximized() ->
    wxTopLevelWindow:isMaximized(?GET(top_frame)).

maximize() ->
    wxTopLevelWindow:maximize(?GET(top_frame)).

reset_video_mode_for_gl(_W, _H) ->
    %% Needed on mac for some reason
    wxWindow:setFocus(?GET(gl_canvas)),
    wxGLCanvas:setCurrent(?GET(gl_canvas)),
    ok.

set_title(Title) ->
    wxTopLevelWindow:setTitle(?GET(top_frame), Title).

set_icon(Frame, IconBase) ->
    Bmp = wxImage:new(IconBase ++ ".png"),
    Bitmap = wxBitmap:new(Bmp),
    Icon = wxIcon:new(),
    wxIcon:copyFromBitmap(Icon, Bitmap),
    wxFrame:setIcon(Frame, Icon).

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
    case get(active_cursor) of
        CursorId -> ok;
        _ ->
            Cursors = ?GET(cursors),
            Cursor = proplists:get_value(CursorId, Cursors),
            case os:type() of
                {win32, _} ->
                    Frame = ?GET(top_frame),
                    wxWindow:setCursor(Frame, Cursor);
                {_, _} ->
                    wx_misc:setCursor(Cursor)
            end,
            put(active_cursor, CursorId),
            ok
    end.

get_mouse_state() ->
    wx:batch(fun() ->
		     MS = wx_misc:getMouseState(),
		     #wxMouseState{x=X, y=Y,  %% integer()
				   leftDown=Left,
				   middleDown=Middle,
				   rightDown=Right %% bool()
				  } = MS,
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
    case get_state() of
	#io{grab_stack=[]} -> ignore;
	#io{grab_stack=[Win|_]} = Io ->
	    put_state(Io#io{grab_stack=[]}),
	    Release andalso wxWindow:releaseMouse(Win)
    end,
    put(wm_cursor, arrow),
    set_cursor(arrow).

grab(Win) ->
    %%io:format("Grab mouse~n", []),
    %%wings_util:profile_start(eprof),
    case get_state() of
	#io{grab_stack=[]} = Io ->
	    put(wm_cursor, blank),
	    set_cursor(blank),
	    wxWindow:captureMouse(Win),
	    put_state(Io#io{grab_stack=[Win]});
	#io{grab_stack=[Win|_]=Stack} = Io ->
	    put_state(Io#io{grab_stack=[Win|Stack]});
	#io{grab_stack=[Other|_]} ->
	    exit({mouse_grabbed_by_other_window,Other,Win})
    end.

ungrab(X, Y) ->
    %%io:format("UNGRAB mouse~n", []),
    %% wings_util:profile_stop(eprof),
    case get_state() of
	#io{grab_stack=[]} -> no_grab;
	#io{grab_stack=[Win]}=Io ->
	    wxWindow:releaseMouse(Win),
	    warp(Win, X, Y),
	    put(wm_cursor, arrow),
	    set_cursor(arrow),
	    put_state(Io#io{grab_stack=[]}),
	    no_grab;
	#io{grab_stack=[_Win|Stack]}=Io ->
	    put_state(Io#io{grab_stack=Stack}),
	    still_grabbed
    end.

is_grabbed() ->
    case get_state() of
	#io{grab_stack=[]} -> false;
	_ -> true
    end.

warp(Win, X, Y) ->
    put(mouse_warp, {X,Y}),
    wxWindow:warpPointer(Win, X, Y).

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

change_event_handler(?SDL_KEYUP, true) ->
    case get_state() of
	#io{key_up=true} -> ok;
	Io -> put_state(Io#io{key_up=true})
    end;
change_event_handler(?SDL_KEYUP, false) ->
    case get_state() of
	#io{key_up=false} -> ok;
	Io -> put_state(Io#io{key_up=false})
    end.

read_events(Eq0) ->
    case queue:is_empty(Eq0) of
        true ->
            Eq1 = rec_events(Eq0, undefined, 0),
            read_events_q(Eq1);
        false ->
            read_events_q(Eq0)
    end.

rec_events(Eq0, Prev, Wait) ->
    receive
	#wx{event=#wxMouse{type=motion,x=X,y=Y}} = Ev ->
	    case get(mouse_warp) of
		{X,Y} ->
                    erase(mouse_warp),
                    rec_events(Eq0, Prev, Wait);
                _ ->
                    rec_events(Eq0, Ev, 5)
	    end;
	#wx{} = Ev ->
	    rec_events(q_in(Ev, q_in(Prev, Eq0)), undefined, 0);
	{timeout,Ref,{event,Event}} when is_reference(Ref) ->
	    q_in(Event, q_in(Prev, Eq0));
	{lock, Pid} -> %% Order ?
	    Pid ! {locked, self()},
	    F = fun GetUnlock () ->
			receive {unlock, Pid, Fun} -> Fun()
			after 2000 ->
				io:format("~p: waiting for unlock from ~p~n", [self(), Pid]),
				GetUnlock()
			end
		end,
	    F(),
	    rec_events(Eq0, Prev, 0);
        {'_wxe_error_', Op, Error} ->
            [{_,{M,F,A}}] = ets:lookup(wx_debug_info,Op),
	    Msg = io_lib:format("~p in ~w:~w/~w", [Error, M, F, A]),
	    wxe_master ! {wxe_driver, error, Msg},
            rec_events(Eq0, Prev, Wait);
	External ->
	    q_in(External, q_in(Prev, Eq0))
    after Wait ->
            q_in(Prev, Eq0)
    end.

read_events_q(Eq0) ->
    case queue:out(Eq0) of
        {{value, #wx{event=#wxMouse{type=motion}} = Ev}, Eq} ->
            {wx_translate(Ev),Eq};
        {{value,#wx{event=#wxKey{type=key_up}}=Ev},Eq} ->
	    case get_state() of
		#io{key_up=true} -> {wx_translate(Ev), Eq};
		_ -> read_events_q(Eq)
	    end;
        {{value,Ev}, Eq} ->
            filter_resize(Ev, Eq);
        {empty, Eq} ->
            read_events_q(rec_events(Eq, undefined, infinity))
    end.

q_in(undefined, Eq) -> Eq;
q_in(Ev, Eq) -> queue:in(Ev, Eq).

% Resize window causes all strange of events and order
% on different platforms
filter_resize(#wx{obj=Obj, event=#wxSize{}}=Ev0, Eq0) ->
    case queue:out(Eq0) of
	{{value, #wx{event=#wxPaint{}}}, Eq} ->
	    filter_resize(Ev0, Eq);
	{{value, #wx{obj=Obj, event=#wxSize{}}=Ev}, Eq} ->
	    filter_resize(Ev, Eq);
	_ ->
	    {wx_translate(Ev0), Eq0}
    end;
filter_resize(#wx{event=#wxPaint{}}=Ev0, Eq0) ->
    case queue:out(Eq0) of
	{{value, #wx{event=#wxPaint{}}},Eq} ->
	    filter_resize(Ev0, Eq);
	{{value, #wx{event=#wxSize{}}=Ev}, Eq} ->
	    filter_resize(Ev, Eq);
	_ ->
	    {wx_translate(Ev0), Eq0}

    end;
filter_resize(Event, Eq) ->
    {wx_translate(Event), Eq}.

wx_translate(Event) ->
    R = wx_translate_1(Event),
    %%erlang:display(R),
    R.

wx_translate_1(#wx{event=#wxMouse{}}=Ev) ->
    sdl_mouse(Ev); % Mouse Buttons
wx_translate_1(#wx{event=#wxKey{}}=Ev) ->
    R = sdl_key(Ev),
    %% erlang:display({sdlkey, R}),
    R;
wx_translate_1(#wx{event={wxMouseCaptureLost, _}}) ->
    reset_grab(false),
    grab_lost;
wx_translate_1(Ev) ->
    Ev.

sdl_mouse(M=#wx{obj=Obj,
		event=#wxMouse{ type=Type,
				x = X, y = Y,
				leftDown=Left, middleDown=Middle, rightDown=Right,
				controlDown = Ctrl, shiftDown = Shift,
				altDown = Alt,      metaDown = Meta,
				wheelRotation=Wheel
			      }}) ->
    ModState = translate_modifiers(Shift, Ctrl, Alt, Meta),
    case Type of
	motion ->
	    Mouse = [{Left,   ?SDL_BUTTON_LMASK},
		     {Middle, ?SDL_BUTTON_MMASK},
		     {Right,  ?SDL_BUTTON_RMASK}],
	    MouseState = gui_state(Mouse, 0),
	    #mousemotion{which=Obj, state=MouseState, mod=ModState,
			 x=X,y=Y, xrel=0, yrel=0};
	left_down ->
	    #mousebutton{button=?SDL_BUTTON_LEFT, state=?SDL_PRESSED,
			 which=Obj, mod=ModState, x=X,y=Y};
	left_up ->
	    #mousebutton{button=?SDL_BUTTON_LEFT, state=?SDL_RELEASED,
			 which=Obj, mod=ModState, x=X,y=Y};
	middle_down ->
	    #mousebutton{button=?SDL_BUTTON_MIDDLE, state=?SDL_PRESSED,
			 which=Obj, mod=ModState, x=X,y=Y};
	middle_up ->
	    #mousebutton{button=?SDL_BUTTON_MIDDLE, state=?SDL_RELEASED,
			 which=Obj, mod=ModState, x=X,y=Y};
	right_down ->
	    #mousebutton{button=?SDL_BUTTON_RIGHT, state=?SDL_PRESSED,
			 which=Obj, mod=ModState, x=X,y=Y};
	right_up ->
	    #mousebutton{button=?SDL_BUTTON_RIGHT, state=?SDL_RELEASED,
			 which=Obj, mod=ModState, x=X,y=Y};
	left_dclick ->
	    #mousebutton{button=?SDL_BUTTON_LEFT, state=?SDL_PRESSED,
			 which=Obj, mod=ModState, x=X,y=Y};

	mousewheel ->
	    Butt = case Wheel > 0 of
		       true -> ?SDL_BUTTON_X1;
		       false -> ?SDL_BUTTON_X2
		   end,
	    #mousebutton{button=Butt, state=?SDL_RELEASED,
			 which=Obj, mod=ModState, x=X,y=Y};
	enter_window -> M;
	leave_window -> M
    end.

sdl_key(#wx{obj=Obj, event=#wxKey{type=Type,controlDown = Ctrl, shiftDown = Shift,
				  altDown = Alt, metaDown = Meta,
				  keyCode = Code, uniChar = Uni0, rawCode = Raw}}) ->
    ModState = translate_modifiers(Shift, Ctrl, Alt, Meta),

    %% key_down events are always uppercase
    %% maybe we should use (the translated) char events instead?
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
    #keyboard{which=Obj, state=Pressed, scancode=Raw, unicode=Uni,
	      mod=ModState, sym=Sym}.

translate_modifiers(Shift, Ctrl, Alt, Meta) ->
    Mods0 = [{Shift,?KMOD_SHIFT},{Alt,?KMOD_ALT}],
    Mods = case os:type() of
	       {unix,darwin} ->
		   %% WX on Mac returns Command as Ctrl and Ctrl
		   %% as Meta. Switch it back here so we don't have
		   %% to deal with that confusion in the rest of
		   %% the Wings code.
		   [{Ctrl,?KMOD_META},{Meta,?KMOD_CTRL}|Mods0];
	       _ ->
		   [{Ctrl,?KMOD_CTRL},{Meta,?KMOD_META}|Mods0]
	   end,
    gui_state(Mods, 0).

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
    Uni = case wx_key_map(sdl_key_map(Key)) of
              undefined -> Key;
              _Other -> 0
          end,
    %io:format("make key {~p, ~p} => ~p ~p ~n",[Key, Mods, Uni, ModState]),
    #keyboard{which=menubar, state=?SDL_PRESSED, sym=Key, mod=ModState, unicode=Uni};
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

