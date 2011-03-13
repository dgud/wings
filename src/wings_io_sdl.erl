%%
%%  wings_sdl_io.erl --
%%
%%     This module contains most of the low-level SDL GUI for Wings.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_io_sdl).

-ifndef(USE_WX).
-export([init/1, quit/0, version_info/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 get_mouse_state/0, is_modkey_pressed/1, is_key_pressed/1,
	 get_buffer/2, read_buffer/3, get_bin/1,
	 is_maximized/0, maximize/0, set_title/1, set_icon/1,
	 get_process_option/0,set_process_option/1]).
-export([batch/1, foreach/2]).
-export([change_event_handler/2, read_events/1]).
-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([reset_video_mode_for_gl/2, swapBuffers/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,keyfind/3,member/2,reverse/1,reverse/2]).

-import(wings_io, [put_state/1, get_state/0]).


init(Icons) ->
    Cursors = build_cursors(),
    put_state(#io{raw_icons=Icons,cursors=Cursors}).

quit() ->
    sdl:quit().

-ifndef(USE_WX_OPENGL).
get_process_option() ->     
    [].
set_process_option(_) ->
    ok.
-else.
get_process_option() ->
    [{opengl_port,get(opengl_port)}].
set_process_option([{opengl_port,Port}]) ->
    put(opengl_port, Port),
    ok.
-endif.

batch(Fun) ->  Fun().
foreach(Fun, List) -> lists:foreach(Fun, List).

is_maximized() ->
    sdl_video:wm_isMaximized().

maximize() ->
    sdl_video:wm_maximize().

reset_video_mode_for_gl(W, H) ->
    {surfacep,_} =
	sdl_video:setVideoMode(W, H, 0, ?SDL_OPENGL bor ?SDL_RESIZABLE),
    ok.

set_title(Caption) ->
    sdl_video:wm_setCaption(Caption, Caption).

set_icon(IconBase) ->
    Bmp = sdl_video:loadBMP(IconBase ++ ".bmp"),
    Mask = wings_io:get_mask(IconBase ++ ".wbm"),
    sdl_video:wm_setIcon(Bmp, Mask).

swapBuffers() ->
    sdl_video:gl_swapBuffers().

version_info() ->
    {SdlVersion0,_} = sdl_video:wm_getInfo(),
    SdlVersion1 = [integer_to_list(V) || V <- tuple_to_list(SdlVersion0)],
    SdlVersion = string:join(SdlVersion1, "."),
    "SDL: " ++ SdlVersion.

hourglass() ->
    set_cursor(hourglass).

eyedropper() ->
    set_cursor(eyedropper).

set_cursor(Cursor) ->
    #io{cursors=Cursors} = get_state(),
    set_cursor_1(Cursors, Cursor).

set_cursor_1([{Name,none}|_], Name) ->
    ok;
set_cursor_1([{Name,Cursor}|_], Name) ->
    sdl_mouse:setCursor(Cursor);
set_cursor_1([_|Cs], Name) ->
    set_cursor_1(Cs, Name).

get_mouse_state() ->
    sdl_mouse:getMouseState().

is_modkey_pressed(Key) ->
    (sdl_keyboard:getModState() band Key) =/= 0.

is_key_pressed(Key) ->
    Keys = sdl_keyboard:getKeyState(),
    element(Key+1,Keys) =/= 0.

%%%
%%% Input.
%%%

change_event_handler(EventType, What) ->
    sdl_events:eventState(EventType, What).

read_events(Eq0) ->
    case sdl_events:peepEvents() of
	[] -> read_out(Eq0);
	[_|_]=Evs -> read_events(enter_events(Evs, Eq0))
    end.

enter_events([no_event|Evs], Eq) ->
    enter_events(Evs, queue:in(redraw, Eq));
enter_events([E|Evs], Eq) ->
    enter_events(Evs, queue:in(E, Eq));
enter_events([], Eq) -> Eq.

read_out(Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	{{value,Event},Eq} ->
	    {Event,Eq};
	{empty,Eq} ->
	    wait_for_event(Eq)
    end.

read_out(Motion, Eq0) ->
    case queue:out(Eq0) of
	{{value,#mousemotion{}=Event},Eq} ->
	    read_out(Event, Eq);
	_Other -> {Motion,Eq0}
    end.

wait_for_event(Eq) ->
    Time = case sdl_active:getAppState() of
	       0 ->				%Iconified.
		   650;
	       7 ->				%Fully active.
		   10;
	       _ ->				%Cursor outside of window.
		   120
	   end,
    receive
	{timeout,Ref,{event,Event}} when is_reference(Ref) ->
	    {Event,Eq};
	External = {external, _} ->
	    read_events(enter_events([External], Eq))
    after Time ->
	    case sdl_events:peepEvents() of
		[] -> wait_for_event(Eq);
		[_|_]=Evs -> read_events(enter_events(Evs, Eq))
	    end
    end.


%%%
%%% Mouse grabbing.
%%%
reset_grab() ->
    Io = get_state(),
    put_state(Io#io{grab_count=0}),
    sdl_mouse:showCursor(true),
    wm_grabInput(?SDL_GRAB_OFF),
    ok.

grab() ->
    %%io:format("Grab mouse~n", []),
    #io{grab_count=Cnt} = Io = get_state(),
    sdl_mouse:showCursor(false),
    do_grab(Cnt),
    put_state(Io#io{grab_count=Cnt+1}).

do_grab(0) ->
    %% On MacOS X, we used to not do any grab. With newer versions of SDL,
    %% it seems to works better if we do a grab.
    %%
    %% Good for Linux to read out any mouse events here.
    sdl_events:peepEvents(1, ?SDL_MOUSEMOTIONMASK),
    wm_grabInput(?SDL_GRAB_ON),
    ok;
do_grab(_N) -> ok.

ungrab(X, Y) ->
    %%io:format("UNGRAB mouse~n", []),
    case get_state() of
	#io{grab_count=0} -> no_grab;
	#io{grab_count=Cnt}=Io ->
	    put_state(Io#io{grab_count=Cnt-1}),
	    case Cnt-1 of
		0 ->
		    wm_grabInput(?SDL_GRAB_OFF),
		    sdl_mouse:warpMouse(X, Y),
		    sdl_mouse:showCursor(true),
		    no_grab;
		_ ->
		    still_grabbed
	    end
    end.

wm_grabInput(Kind) ->
    case wings_pref:get_value(ungrab_bug) of
	true -> 
	    ignore;
	_ -> 
	    sdl_video:wm_grabInput(Kind)
    end.

is_grabbed() ->
    case get_state() of
	#io{grab_count=0} -> false;
	_ -> true
    end.

warp(X, Y) ->
    %% Strangely enough, on Solaris the warp doesn't seem to
    %% work unless the mouse cursor is visible.
    %% On Windows, the mouse cursor must not be visible.
    case os:type() of
	{unix,sunos} ->
	    sdl_mouse:showCursor(true),
	    sdl_mouse:warpMouse(X, Y),
	    sdl_mouse:showCursor(false);
	_ ->
	    sdl_mouse:warpMouse(X, Y)
    end.

%%% Memory
get_buffer(Size,Type) ->
    sdl_util:alloc(Size, Type).

read_buffer(Buff, _Type, Size) ->
    sdl_util:read(Buff, Size).
get_bin(Buff) ->
    sdl_util:getBin(Buff).

%%%
%%% Cursors.
%%%

build_cursors() ->
    [{stop,build_cursor(stop_data(), 8, 8)},
     {pointing_hand,build_cursor(pointing_hand_data(), 0, 0)},
     {closed_hand,build_cursor(closed_hand_data(), 8, 8)}|
     case os:type() of
	 {unix,darwin} ->
	     [{arrow,sdl_mouse:getCursor()},
	      {hourglass,none},
	      {eyedropper,build_cursor(eyedropper_data(), 0, 15)}];
	 _ ->
	     [{arrow,build_cursor(arrow_data())},
	      {hourglass,build_cursor(hourglass_data())},
	      {eyedropper,build_cursor(eyedropper_data(), 0, 15)}]
     end].

build_cursor(Data) ->
    build_cursor(Data, 0, 0).

build_cursor(Data, HotX, HotY) ->
    case length(Data) of
	Bytes when Bytes =:= 256 ->
	    build_cursor_1(Data, {HotX,HotY,16,16}, 0, 0);
	Bytes when Bytes =:= 1024 ->
	    build_cursor_1(Data, {HotX,HotY,32,32}, 0, 0)
    end.

build_cursor_1([$.|T], Hot, Mask, Bits) ->
    build_cursor_1(T, Hot, (Mask bsl 1) bor 1, Bits bsl 1);
build_cursor_1([$X|T], Hot, Mask, Bits) ->
    build_cursor_1(T, Hot, (Mask bsl 1) bor 1, (Bits bsl 1) bor 1);
build_cursor_1([$x|T], Hot, Mask, Bits) ->
    build_cursor_1(T, Hot, (Mask bsl 1) bor 1, (Bits bsl 1) bor 1);
build_cursor_1([_|T], Hot, Mask, Bits) ->
    build_cursor_1(T, Hot, Mask bsl 1, Bits bsl 1);
build_cursor_1([], {HotX,HotY,W,H}, Mask0, Bits0) ->
    Size = W*H,
    Bits = <<Bits0:Size>>,
    Mask = <<Mask0:Size>>,
    sdl_mouse:createCursor(Bits, Mask, W, H, HotX, HotY).

hourglass_data() ->
	"  ............................  "
	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
	"X..............................X"
	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
	"   ..   X..............X   ..   "
	"   ..   X..............X   ..   "
	"   ..   X..............X   ..   "
	"   ..   X..............X   ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..     X..........X     ..   "
	"   ..     X.X......X.X     ..   "
	"   ..     X.X.X..X.X.X     ..   "
	"   ..      X.X.X.X..X      ..   "
	"   ..       X...X..X       ..   "
	"   ..       X......X       ..   "
	"   ..      X........X      ..   "
	"   ..     X..........X     ..   "
	"   ..     X..........X     ..   "
	"   ..     X..........X     ..   "
	"   ..    X............X    ..   "
	"   ..    X............X    ..   "
	"   ..    X......X.....X    ..   "
	"   ..   X....X.X.X.X...X   ..   "
	"   ..   X...X.X.X.X.X..X   ..   "
	"   ..   X..X.X.X.X.X.X.X   ..   "
	"   ..   X.X.X.X.X.X.XX.X   ..   "
	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
	"X..............................X"
	" XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX "
	"  ............................  ".

eyedropper_data() ->
	"             XXX"
	"            X.XX"
	"         X X..XX"
	"        X.X..XX "
	"       X....XX  "
	"        XXXXX   "
	"       X.XXXXX  "
	"      X...XXX   "
	"     X...X X    "
	"    X...X       "
	"   X...X        "
	"  X...X         "
	" X...X          "
	" X..X           "
	"X.XX            "
	" X              ".

arrow_data() ->
	"X                               "
	"XX                              "
	"X.X                             "
	"X..X                            "
	"X...X                           "
	"X....X                          "
	"X.....X                         "
	"X......X                        "
	"X.......X                       "
	"X........X                      "
	"X.....XXXXX                     "
	"X..X..X                         "
	"X.X X..X                        "
	"XX  X..X                        "
	"X    X..X                       "
	"     X..X                       "
	"      X..X                      "
	"      X..X                      "
	"       XX                       "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                "
	"                                ".

stop_data() ->
	"     xxxxxx     "
	"   xxxxxxxxxx   "
	"   xxx    xxx   "
	" xxxxx      xxx "
	" xxxxx      xxx "
	"xxxxxxx       xx"
	"xx   xxx      xx"
	"xx    xxx     xx"
	"xx     xxx    xx"
	"xx      xxx   xx"
	"xx       xxxxxxx"
	" xxx      xxxxx "
	" xxx      xxxxx "
	"   xxx    xxx   "
	"   xxxxxxxxxx   "
	"     xxxxxx     ".

pointing_hand_data() ->
	"      XX        "
	"     X..X       "
	"     X..X       "
	"     X..XXX     "
	"     X..X.XXX   "
	"     X..X.X.XXX "
	"  XX X..X.X.X.X "
	" X..XX......X.X "
	" X...X........X "
	"  X...........X "
	"   X..........X "
	"   X.........X  "
	"    X........X  "
	"     X......X   "
	"     X......X   "
	"     XXXXXXXX   ".

closed_hand_data() ->
	"                "
	"   xx xx xx     "
	"  x..x..x.. xx  "
	"  x..x..x..x..x "
	"   x.......x..x "
	" xxx..........x "
	" x.x.........x  "
	" x...........x  "
	" x...........x  "
	"  x.........x   "
	"   x........x   "
	"    x......x    "
	"    x......x    "
	"    x......x    "
	"                "
	"                ".

-endif.

