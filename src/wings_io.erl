%%
%%  wings_io.erl --
%%
%%     This module is a wrapper for the different backends
%%
%%  Copyright (c) 2001-2013 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_io).

-export([init/0,quit/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 info/1, info/3, version_info/0,

	 is_maximized/0, maximize/0, set_title/1, reset_video_mode_for_gl/2,
	 change_event_handler/2,
	 read_icons/0, set_icon/2,

	 get_buffer/2, read_buffer/3, get_bin/1,
	 get_mouse_state/0, is_modkey_pressed/1, is_key_pressed/1,
	 get_process_option/0,set_process_option/1,

	 batch/1, foreach/2,
	 lock/1, unlock/2, lock/2, lock/3,

	 unclipped_text/3,
	 draw_bitmap/1,
	 set_color/1]).

-export([putback_event/1,putback_event_once/1,get_event/0,get_matching_events/1,
	 set_timer/2,cancel_timer/1,enter_event/1]).

-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([ortho_setup/0,ortho_setup/1]).

-export([put_state/1, get_state/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Init and Quit

init() ->
    put(?EVENT_QUEUE, queue:new()),
    put_state(#io{}),
    wings_io_wx:init().

quit() ->
    wings_io_wx:quit().

get_process_option() ->
    wings_io_wx:get_process_option().

set_process_option(Opts) ->
    wings_io_wx:set_process_option(Opts).

lock(Pid, Fun) ->
    lock(Pid, Fun, fun() -> ok end).

lock(Pid, Fun, Other) when is_function(Fun), is_function(Other) ->
    lock(Pid),
    try Fun()
    after
	unlock(Pid, Other)
    end.

lock(Pid) when is_pid(Pid) ->
    case self() == Pid of
	true -> error(locking_self);
	false ->
	    Monitor = erlang:monitor(process, Pid),
	    Pid ! {lock, self()},
	    F = fun GetLock () ->
			receive
			    {locked, Pid} ->
				erlang:demonitor(Monitor, [flush]),
				ok;
			    {'DOWN',Monitor, _,_,_} ->
				ok
			after 2000 ->
				io:format("~p: Can not lock ~p~n", [self(), Pid]),
				GetLock()
			end
		end,
	    F()
    end.

unlock(Pid, Fun) ->
    Pid ! {unlock, self(), Fun},
    ok.

%% Batch processing
foreach(Fun, List) ->
    wings_io_wx:foreach(Fun, List).

batch(Fun) ->
    wings_io_wx:batch(Fun).

%% Cursor support and Mouse handling

eyedropper() ->
    wings_io_wx:eyedropper().

hourglass() ->
    case get(wings_not_running) of
	undefined -> 
	    wings_io_wx:hourglass();
	_ ->
	    ignore
    end.

set_cursor(Cursor) ->
    wings_io_wx:set_cursor(Cursor).

grab() ->
    wings_io_wx:grab(wings_wm:this_win()).
ungrab(X,Y) ->
    wings_io_wx:ungrab(X,Y).

reset_grab() ->
    wings_io_wx:reset_grab().

is_grabbed() ->
    wings_io_wx:is_grabbed().

warp(X,Y) ->
    wings_io_wx:warp(wings_wm:this_win(), X,Y).

get_mouse_state() ->
    wings_io_wx:get_mouse_state().

is_modkey_pressed(Key) ->
    wings_io_wx:is_modkey_pressed(Key).

is_key_pressed(Key) ->
    wings_io_wx:is_key_pressed(Key).

%% Window handling
is_maximized() ->
    wings_io_wx:is_maximized().

maximize() ->
    wings_io_wx:maximize().

set_title(Title) ->
    wings_io_wx:set_title(Title).

reset_video_mode_for_gl(W,H) ->
    wings_io_wx:reset_video_mode_for_gl(W,H).

version_info() ->
    wings_io_wx:version_info().

set_icon(Frame, IconBase) ->
    wings_io_wx:set_icon(Frame, IconBase).

%% Memory handling

get_buffer(Size, Type) ->
    wings_io_wx:get_buffer(Size, Type).

read_buffer(Buff, Size, Type) ->
    wings_io_wx:read_buffer(Buff, Size, Type).

get_bin(Buff) ->
    wings_io_wx:get_bin(Buff).

%% Events
%%  This is probably slow in wx (should be avoided)
change_event_handler(EvType,What) ->
    wings_io_wx:change_event_handler(EvType,What).

enter_event(Ev) ->
    Eq0 = get(?EVENT_QUEUE),
    Eq = queue:in(Ev, Eq0),
    put(?EVENT_QUEUE, Eq),
    ok.

get_event() ->
    Eq0 = get(?EVENT_QUEUE),
    {Event,Eq} = wings_io_wx:read_events(Eq0),
    put(?EVENT_QUEUE, Eq),
    Event.


%% get_matching_events(FilterFun) -> [Event].
%%       FilterFun(Element) -> true|false.
%%  Remove from the queue and return in a list
%%  all elements for which FilterFun(Element)
%%  returns true. The elements in the returned
%%  list will be in the same order as in the queue.
%%
get_matching_events(Filter) when is_function(Filter, 1) ->
    Eq = get(?EVENT_QUEUE),
    {Match,NoMatch} = lists:partition(Filter, queue:to_list(Eq)),
    case Match of
	[] -> [];
	_ ->
	    put(?EVENT_QUEUE, queue:from_list(NoMatch)),
	    Match
    end.

putback_event(Ev) ->
    Q = get(?EVENT_QUEUE),
    put(?EVENT_QUEUE, queue:in_r(Ev, Q)).

putback_event_once(Ev) ->
    Q = get(?EVENT_QUEUE),
    case queue:member(Ev, Q) of
	true -> ok;
	false -> put(?EVENT_QUEUE, queue:in_r(Ev, Q))
    end.

%%%%%%%%%%%

info(Info) ->
    info(0, 0, Info).

info(X, Y, Info) ->
    ortho_setup(),
    blend(wings_pref:get_value(info_background_color),
	  fun(Color) ->
		  set_color(Color),
		  N = info_lines(Info),
		  {W,_} = wings_wm:win_size(),
		  gl:recti(X, Y, W, Y + N*?LINE_HEIGHT + 2)
	  end),
    set_color(wings_pref:get_value(info_color)),
    text_at(X + 4, Y + ?CHAR_HEIGHT, Info).

info_lines(Info) ->
    info_lines_1(Info, 1).

info_lines_1([$\n|T], Lines) ->
    info_lines_1(T, Lines+1);
info_lines_1([H|T], Lines) when is_list(H) ->
    info_lines_1(T, info_lines_1(H, Lines));
info_lines_1([_|T], Lines) ->
    info_lines_1(T, Lines);
info_lines_1([], Lines) -> Lines.

blend({_,_,_,0.0}, _) -> ok;
blend({_,_,_,1.0}=Color, Draw) -> Draw(Color);
blend(Color, Draw) ->
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    Draw(Color),
    gl:disable(?GL_BLEND).

set_color({_,_,_}=RGB) -> gl:color3fv(RGB);
set_color({_,_,_,_}=RGBA) -> gl:color4fv(RGBA).

text_at(X, Y, S) ->
    case wings_gl:is_restriction(broken_scissor) of
	true ->
%% Scissor cannot clip text, but slows down text drawing.
	    unclipped_text(X, Y, S);
	false ->
	    {Vx,Vy,W,H} = wings_wm:viewport(),
	    gl:scissor(Vx, Vy, W, H),
	    gl:enable(?GL_SCISSOR_TEST),
	    unclipped_text(X, Y, S),
	    gl:disable(?GL_SCISSOR_TEST)
    end.

unclipped_text(X, Y, S) ->
    wings_text:render(X, Y, S).

draw_bitmap({A,B,C,D,E,F,Bitmap}) ->
    gl:bitmap(A, B, C, D, E, F, Bitmap).

ortho_setup() ->
    gl:color3b(0, 0, 0),
    ortho_setup_1().

ortho_setup(none) ->
    ortho_setup_1();
ortho_setup(Color) ->
    set_color(Color),
    ortho_setup_1().

ortho_setup_1() ->
    ?CHECK_ERROR(),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_DEPTH_TEST),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {_,_,W,H} = wings_wm:viewport(),
    glu:ortho2D(0.0, float(W), float(H), 0.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().

get_state() ->
    get(wings_io).

put_state(Io) ->
    put(wings_io, Io).

read_icons() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = case wings_pref:get_value(interface_icons) of
		   classic    -> filename:join(Ebin, "wings_icon_classic.bundle");
		   bluecube   -> filename:join(Ebin, "wings_icon_bluecube.bundle");
		   purpletube -> filename:join(Ebin, "wings_icon_purpletube.bundle")
	       end,
    Patch = fun({about_wings, {3, W, H, Bin0, <<>>}}) ->
		    {about_wings, {W, H, Bin0}};
	       ({Name, {3, W, H, Bin, <<>>}}) ->
		    {Rgb, Alpha} = rgb3(Bin, <<>>, <<>>),
		    {Name, {W, H, Rgb, Alpha}};
	       ({Name, {4, W, H, Rgb, Alpha}}) ->
		    {Name, {W, H, Rgb, Alpha}}
	    end,
    {ok, Bin} = file:read_file(IconFile),
    [Patch(Raw) || Raw <- binary_to_term(Bin)].

%% FIXME when icons are fixed
%% Poor mans version of alpha channel
rgb3(<<8684676:24, Rest/binary>>, Cs, As) ->
    rgb3(Rest, <<Cs/binary, 8684676:24>>, <<As/binary, 0:8>>);
rgb3(<<R:8,G:8,B:8, Rest/binary>>, Cs, As) ->
    A0 = abs(R-132)/123,
    A1 = abs(G-132)/123,
    A2 = abs(B-132)/123,
    A = trunc(255*min(1.0, max(max(A0,A1),A2))),
    rgb3(Rest, <<Cs/binary, R:8, G:8, B:8>>, <<As/binary, A:8>>);
rgb3(<<>>, Cs, As) ->
    {Cs,As}.

%% pad_image(Image, W, W, H, TxH) ->
%%     pad_image_1(Image, H, TxH);
%% pad_image(Image0, W, TxW, H, TxH) ->
%%     Image = pad_image_hor(Image0, W, TxW, H),
%%     pad_image_1(Image, H, TxH).

%% pad_image_1(Image, H, TxH) ->
%%     <<Image/binary,0:((byte_size(Image)*TxH) div H)/unit:8>>.

%% pad_image_hor(Image, W, TxW, H) ->
%%     OldRowSize = byte_size(Image) div H,
%%     NewRowSize = (OldRowSize * TxW) div W,
%%     Diff = NewRowSize - OldRowSize,
%%     << <<Row/binary,0:Diff/unit:8>> || <<Row:OldRowSize/binary>> <= Image >>.


%%%
%%% Timer support.
%%%

set_timer(Time, Event) ->
    erlang:start_timer(Time, self(), {event,Event}).

cancel_timer(Ref) ->
    Left = erlang:cancel_timer(Ref),
    receive
	{timeout,Ref,_} -> ok
    after 0 -> ok
    end,
    Left.

