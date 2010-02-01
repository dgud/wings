%%
%%  wings_io.erl --
%%
%%     This module is a wrapper for the different backends
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_io).

-export([init/0,quit/0,
	 resize/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 info/1, version_info/0,

	 is_maximized/0, set_title/1, reset_video_mode_for_gl/2,
	 change_event_handler/2,
	 set_icon/1, get_mask/1,

	 get_buffer/2, read_buffer/3, get_bin/1,
	 get_mouse_state/0, is_modkey_pressed/1, is_key_pressed/1,
	 get_process_option/0,set_process_option/1,

	 batch/1, foreach/2,

	 blend/2,
	 border/5,border/6,border_only/4,border_only/5,
	 gradient_border/5,gradient_border/7,
	 sunken_rect/4,sunken_rect/5,sunken_rect/6,sunken_rect/7,
	 sunken_gradient/7,
	 raised_rect/4,raised_rect/5,raised_rect/6,
	 gradient_rect/5,
	 use_font/2,text_at/2,text_at/3,unclipped_text/3,
	 draw_icons/1,draw_icon/3,draw_char/1,
	 set_color/1]).
-export([putback_event/1,putback_event_once/1,get_event/0,get_matching_events/1,
	 set_timer/2,cancel_timer/1,enter_event/1]).

-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([ortho_setup/0,ortho_setup/1]).
-export([swapBuffers/0]).

-export([put_state/1, get_state/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(erlang, [max/2]).
-import(lists, [flatmap/2,keyfind/3,member/2,reverse/1,reverse/2]).

-ifdef(USE_WX).
-define(BACKEND_MOD, wings_io_wx).
-else.
-define(BACKEND_MOD, wings_io_sdl).
-endif.

%% Init and Quit

init() ->
    Icons = read_icons(),
    put(?EVENT_QUEUE, queue:new()),
    ?BACKEND_MOD:init(Icons).

quit() ->
    ?BACKEND_MOD:quit().

swapBuffers() ->
    ?BACKEND_MOD:swapBuffers().

get_process_option() ->
    ?BACKEND_MOD:get_process_option().

set_process_option(Opts) ->
    ?BACKEND_MOD:set_process_option(Opts).

%% Batch processing
foreach(Fun, List) ->
    ?BACKEND_MOD:foreach(Fun, List).

batch(Fun) ->
    ?BACKEND_MOD:batch(Fun).

%% Cursor support and Mouse handling

eyedropper() ->
    ?BACKEND_MOD:eyedropper().

hourglass() ->
    ?BACKEND_MOD:hourglass().

set_cursor(Cursor) ->
    ?BACKEND_MOD:set_cursor(Cursor).

grab() ->
    ?BACKEND_MOD:grab().
ungrab(X,Y) ->
    ?BACKEND_MOD:ungrab(X,Y).

reset_grab() ->
    ?BACKEND_MOD:reset_grab().

is_grabbed() ->
    ?BACKEND_MOD:is_grabbed().

warp(X,Y) ->
    ?BACKEND_MOD:warp(X,Y).

get_mouse_state() ->
    ?BACKEND_MOD:get_mouse_state().

is_modkey_pressed(Key) ->
    ?BACKEND_MOD:is_modkey_pressed(Key).

is_key_pressed(Key) ->
    ?BACKEND_MOD:is_key_pressed(Key).

%% Window handling
is_maximized() ->
    ?BACKEND_MOD:is_maximized().

set_title(Title) ->
    ?BACKEND_MOD:set_title(Title).

reset_video_mode_for_gl(W,H) ->
    ?BACKEND_MOD:reset_video_mode_for_gl(W,H).

version_info() ->
    ?BACKEND_MOD:version_info().

set_icon(IconBase) ->
    ?BACKEND_MOD:set_icon(IconBase).

%% get_mask(WBMFileName) -> Binary | null
%%  Read a mask from a WBM file.
%%  Wbmp format reference: http://en.wikipedia.org/wiki/Wbmp
%%
get_mask(IconBase) ->
    case file:read_file(IconBase) of
	{ok,Bin} -> get_mask_1(Bin);
	{error,_} -> null
    end.

get_mask_1(<<0,0,T0/binary>>) ->
    try
	{_W,T} = get_uintvar(T0, 0),
	{_H,Bits} = get_uintvar(T, 0),
	Bits
    catch _:_ ->
	    null
    end.

get_uintvar(<<1:1,N:7,T/binary>>, Acc) ->
    get_uintvar(T, (Acc bsl 7) bor N);
get_uintvar(<<0:1,N:7,T/binary>>, Acc) ->
    {(Acc bsl 7) bor N,T}.

%% Memory handling

get_buffer(Size, Type) ->
    ?BACKEND_MOD:get_buffer(Size, Type).

read_buffer(Buff, Size, Type) ->
    ?BACKEND_MOD:read_buffer(Buff, Size, Type).

get_bin(Buff) ->
    ?BACKEND_MOD:get_bin(Buff).

%% Events
%%  This is probably slow in wx (should be avoided)
change_event_handler(EvType,What) ->
    ?BACKEND_MOD:change_event_handler(EvType,What).

enter_event(Ev) ->
    Eq0 = get(?EVENT_QUEUE),
    Eq = queue:in(Ev, Eq0),
    put(?EVENT_QUEUE, Eq),
    ok.

get_event() ->
    case get_event2() of
	{quit} -> quit;
	Other -> Other
    end.

get_event2() ->
    Eq0 = get(?EVENT_QUEUE),
    {Event,Eq} = ?BACKEND_MOD:read_events(Eq0),
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


read_icons() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    case wings_pref:get_value(interface_icons) of
	classic -> IconFile = filename:join(Ebin, "wings_icon_classic.bundle");
	bluecube -> IconFile = filename:join(Ebin, "wings_icon_bluecube.bundle");
	purpletube -> IconFile = filename:join(Ebin, "wings_icon_purpletube.bundle")
    end,
    {ok,Bin} = file:read_file(IconFile),
    Bin.

resize() ->
    #io{raw_icons=RawIcons} = Io = get_state(),
    Tex = load_textures(RawIcons),
    put_state(Io#io{tex=Tex}),
    wings_text:resize().

info(Info) ->
    ortho_setup(),
    blend(wings_pref:get_value(info_background_color),
	  fun(Color) ->
		  set_color(Color),
		  N = info_lines(Info),
		  {W,_} = wings_wm:win_size(),
		  gl:recti(0, 0, W, N*?LINE_HEIGHT)
	  end),
    set_color(wings_pref:get_value(info_color)),
    text_at(4, ?CHAR_HEIGHT, Info).

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

border(X, Y, W, H, FillColor) ->
    border(X, Y, W, H, FillColor, {0.0,0.0,0.0}).

border(X0, Y0, Mw, Mh, FillColor, BorderColor)
  when is_integer(X0), is_integer(Y0), is_integer(Mw), is_integer(Mh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    set_color(FillColor),
    gl:rectf(X0, Y0, X0+Mw, Y0+Mh),
    set_color(BorderColor),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:'end'(),
    gl:color3b(0, 0, 0).

gradient_border(X, Y, W, H, FillColor) ->
    gradient_border(X, Y, W, H, FillColor, {0,0,0}, false).

gradient_border(X0, Y0, Mw, Mh, FillColor, BorderColor, Double)
  when is_integer(X0), is_integer(Y0), is_integer(Mw), is_integer(Mh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gradient_rect(X0, Y0, Mw, Mh, FillColor),
    set_color(BorderColor),
    border_only(X, Y, Mw, Mh, Double).

border_only(X, Y, Mw, Mh) ->
    border_only(X, Y, Mw, Mh, false).

border_only(X, Y, Mw, Mh, Double) ->
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:'end'(),
    case Double of
	false -> ok;
	true ->
	    gl:'begin'(?GL_LINE_LOOP),
	    gl:vertex2f(X-1, Y+Mh+1),
	    gl:vertex2f(X-1, Y-1),
	    gl:vertex2f(X+Mw+1, Y-1),
	    gl:vertex2f(X+Mw+1, Y+Mh+1),
	    gl:'end'()
    end,
    gl:color3b(0, 0, 0).

add_color({R,G,B}, N) -> {R+N,G+N,B+N};
add_color({R,G,B,A}, N) -> {R+N,G+N,B+N,A}.

mul_color({R,G,B}, N) -> {R*N,G*N,B*N};
mul_color({R,G,B,A}, N) -> {R*N,G*N,B*N,A}.

set_color({_,_,_}=RGB) -> gl:color3fv(RGB);
set_color({_,_,_,_}=RGBA) -> gl:color4fv(RGBA).

raised_rect(X, Y, Mw, Mh) ->
    raised_rect(X, Y, Mw, Mh, ?PANE_COLOR, ?PANE_COLOR).

raised_rect(X, Y, Mw, Mh, FillColor) ->
    raised_rect(X, Y, Mw, Mh, FillColor, ?PANE_COLOR).

raised_rect(X, Y, Mw, Mh, FillColor, PaneColor) ->
    sunken_rect(X+Mw, Y+Mh, -Mw, -Mh, FillColor, PaneColor).

sunken_rect(X, Y, Mw, Mh) ->
    sunken_rect(X, Y, Mw, Mh, ?PANE_COLOR, ?PANE_COLOR, false).

sunken_rect(X, Y, Mw, Mh, FillColor) ->
    sunken_rect(X, Y, Mw, Mh, FillColor, ?PANE_COLOR, false).

sunken_rect(X, Y, Mw, Mh, FillColor, PaneColor) ->
    sunken_rect(X, Y, Mw, Mh, FillColor, PaneColor, false).

sunken_rect(X0, Y0, Mw0, Mh0, FillColor, PaneColor, Active) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    Mw = Mw0 + 0.5,
    Mh = Mh0 + 0.5,
    set_color(FillColor),
    gl:rectf(X0, Y0, X0+Mw0, Y0+Mh0),
    sunken_border(X, Y, Mw, Mh, PaneColor, Active),
    gl:color3b(0, 0, 0).

sunken_gradient(X0, Y0, Mw0, Mh0, FillColor, PaneColor, Active) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    Mw = Mw0 + 0.5,
    Mh = Mh0 + 0.5,
    gradient_rect(X0, Y0, Mw, Mh, FillColor),
    sunken_border(X, Y, Mw, Mh, PaneColor, Active),
    gl:color3b(0, 0, 0).

sunken_border(X, Y, Mw, Mh, _, true) ->
    gl:color3b(0, 0, 0),
    border_only(X-1, Y, Mw, Mh, true);
sunken_border(X, Y, Mw, Mh, PaneColor, false) ->
    gl:'begin'(?GL_LINES),
    set_color(wings_color:mix(?BEVEL_LOWLIGHT_MIX, {0,0,0}, PaneColor)),
    gl:vertex2f(X, Y+Mh),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+Mw, Y),
    set_color(wings_color:mix(?BEVEL_HIGHLIGHT_MIX, {1,1,1}, PaneColor)),
    gl:vertex2f(X+Mw, Y),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X+Mw, Y+Mh),
    gl:vertex2f(X, Y+Mh),
    gl:'end'().

gradient_rect(X, Y, W, H=18, Color) ->
    GradColors = [0.882353, 0.882353, 0.850980, 0.807843, 0.776471, 0.729412,
		  0.701961, 0.666667, 0.619608, 0.741176, 0.733333, 0.760784,
		  0.784314, 0.811765, 0.854902, 0.890196, 0.890196],
    Draw_Line = fun(Idx) ->
			GreyValue = lists:nth(Idx+1, GradColors),
			LineColor = mul_color(Color, GreyValue),
			set_color(LineColor),
			gl:vertex2f(X-0.5+W, Y-0.5+H-Idx),
			gl:vertex2f(X-0.5,   Y-0.5+H-Idx)
		end,
    gl:lineWidth(1),
    gl:'begin'(?GL_LINES),
    lists:foreach(Draw_Line, lists:seq(0, 16)),
    gl:'end'();
gradient_rect(X, Y, W, H, Color) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    set_color(Color),
    gl:vertex2f(X+W, Y+H),
    gl:vertex2f(X, Y+H),
    set_color(add_color(Color, 0.09)),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:'end'(),
    gl:shadeModel(?GL_FLAT).

use_font(Font, Fun) ->
    case wings_wm:this() of
	none ->
	    OldFont = wings_pref:get_value(new_system_font),
	    wings_pref:set_value(new_system_font, Font),
	    Res = Fun(),
	    wings_pref:set_value(new_system_font, OldFont),
	    Res;
	This ->
	    OldFont = wings_wm:get_prop(This, font),
	    wings_wm:set_prop(This, font, Font),
	    Res = Fun(),
	    wings_wm:set_prop(This, font, OldFont),
	    Res
    end.

text_at(X, S) ->
    text_at(X, 0, S).

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
    gl:rasterPos2i(X, Y),
    try
	text(S, X, Y, [])
    catch
	throw:{newline,More} ->
	    unclipped_text(X, Y+?LINE_HEIGHT, More)
    end.

text([$\n|Cs], _, _, []) ->
    throw({newline,Cs});
text([$\n|Cs], _, _,Acc) ->
    draw_reverse(Acc),
    throw({newline,Cs});
text([{bold,Str}|Cs], X0, Y, Acc) ->
    draw_reverse(Acc),
    X = X0 + wings_text:width(Acc) + wings_text:width(Str) + length(Str),
    wings_text:bold(Str),
    text(Cs, X, Y, []);
text([{ul,Str}|Cs], X0, Y, Acc) ->
    X1 = X0 + wings_text:width(Acc),
    draw_reverse(Acc),
    wings_text:draw(Str),
    W = wings_text:width(Str),
    X = X1 + W,
    LineY = Y+2,
    gl:'begin'(?GL_LINES),
    gl:vertex2i(X1, LineY),
    gl:vertex2i(X+1, LineY),
    gl:'end'(),
    gl:rasterPos2i(X, Y),
    text(Cs, X, Y, []);
text([{space,W}|Cs], X0, Y, Acc) ->
    X = X0+W,
    draw_reverse(Acc),
    gl:rasterPos2i(X, Y),
    text(Cs, X, Y, []);
text([C|Cs], X0, Y, Acc) when is_integer(C) ->
    if
	C < 256 ->
	    text(Cs, X0, Y, [C|Acc]);
	true ->
%% Unicode character.
	    X = X0 + wings_text:width([C|Acc]),
	    draw_reverse(Acc),
	    wings_text:char(C),
	    text(Cs, X, Y, [])
    end;
text([Atom|Cs], X0, Y, Acc) when is_atom(Atom) ->
    X = X0 + wings_text:width([Atom|Acc]),
    draw_reverse(Acc),
    wings_text:char(Atom),
    text(Cs, X, Y, []);
text([L|Cs], X0, Y, Acc) when is_list(L) ->
    X = X0 + wings_text:width(Acc),
    draw_reverse(Acc),
    text(L++Cs, X, Y, []);
text([], _, _, Acc) -> draw_reverse(Acc).

draw_reverse([]) -> ok;
draw_reverse(S0) ->
    S = reverse(S0),
    wings_text:draw(S).

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
    glu:ortho2D(0, W, H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().

get_state() ->
    get(wings_io).

put_state(Io) ->
    put(wings_io, Io).

draw_icons(Body) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    Body(),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D),
    erase(?ACTIVE_TX).

draw_icon(X, Y, Icon) ->
    #io{tex=Tex} = get_state(),
    case keyfind(Icon, 1, Tex) of
	false -> ok;
	{Icon,{Id,W,H,MinU,MinV,MaxU,MaxV}} ->
	    case get(?ACTIVE_TX) of
		Id -> ok;
		_ ->
		    gl:bindTexture(?GL_TEXTURE_2D, Id),
		    put(?ACTIVE_TX, Id)
	    end,
	    gl:'begin'(?GL_QUADS),
	    gl:texCoord2f(MinU, MaxV),
	    gl:vertex2i(X, Y),
	    gl:texCoord2f(MinU, MinV),
	    gl:vertex2i(X, Y+H),
	    gl:texCoord2f(MaxU, MinV),
	    gl:vertex2i(X+W, Y+H),
	    gl:texCoord2f(MaxU, MaxV),
	    gl:vertex2i(X+W, Y),
	    gl:'end'()
    end.

draw_char({A,B,C,D,E,F,Bitmap}) ->
    gl:bitmap(A, B, C, D, E, F, Bitmap).

load_textures(Bin) ->
    case catch binary_to_term(Bin) of
	{'EXIT',_} -> [];
	Icons0 ->
	    gl:pushAttrib(?GL_TEXTURE_BIT),
	    Icons1 = create_buttons(Icons0),
	    Icons = lists:keysort(2, Icons1),
	    Tex = create_textures(Icons),
	    gl:popAttrib(),
	    Tex
    end.

create_textures(Icons) ->
    [TxId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    Mem0 = wings_io:get_buffer(3*?TX_WIDTH*?TX_HEIGHT, ?GL_BYTE),
    Mem = wings_io:get_bin(Mem0),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB,
		  ?TX_WIDTH, ?TX_HEIGHT, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    create_textures_1(Icons, TxId, 0, 0, 0).

create_textures_1([{_,{3,W,H,_}}|_]=Icons, Id, U, V, RowH)
  when W =< 32, H =< 32, U+W > ?TX_WIDTH ->
    create_textures_1(Icons, Id, 0, V+RowH, 0);
create_textures_1([{Name,{3,W,H,Icon}}|T], Id, U, V, RowH0)
  when W =< 32, H =< 32 ->
    gl:texSubImage2D(?GL_TEXTURE_2D, 0, U, V,
		     W, H, ?GL_RGB, ?GL_UNSIGNED_BYTE, Icon),
    MinU = div_uv(U, ?TX_WIDTH),
    MinV = div_uv(V, ?TX_HEIGHT),
    MaxU = (U+W) / ?TX_WIDTH,
    MaxV = (V+H) / ?TX_HEIGHT,
    RowH = lists:max([RowH0,H]),
    [{Name,{Id,W,H,MinU,MinV,MaxU,MaxV}}|create_textures_1(T, Id, U+W, V, RowH)];
create_textures_1(Icons, _, _, _, _) ->
    create_textures_2(Icons).

create_textures_2([{Name,{Bpp,W,H,Icon0}}|T]) ->
    TxW = max(nearest_power_of_two(W), W),
    TxH = max(nearest_power_of_two(H), H),
    Icon = pad_image(Icon0, W, TxW, H, TxH),
    [TxId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP),
    Format = case Bpp of
		 3 -> ?GL_RGB;
		 4 -> ?GL_RGBA
	     end,
    gl:texImage2D(?GL_TEXTURE_2D, 0, Format,
		  TxW, TxH, 0, Format, ?GL_UNSIGNED_BYTE, Icon),
    [{Name,{TxId,W,H,0,0,W/TxW,H/TxH}}|create_textures_2(T)];
create_textures_2([]) -> [].

div_uv(0, _) -> 0;
div_uv(X, X) -> 1;
div_uv(X, Y) -> X/Y.

nearest_power_of_two(N) ->
    nearest_power_of_two(N, 1).

nearest_power_of_two(N, B) when N =< B -> B;
nearest_power_of_two(N, B) -> nearest_power_of_two(N, B bsl 1).

pad_image(Image, W, W, H, TxH) ->
    pad_image_1(Image, H, TxH);
pad_image(Image0, W, TxW, H, TxH) ->
    Image = pad_image_hor(Image0, W, TxW, H),
    pad_image_1(Image, H, TxH).

pad_image_1(Image, H, TxH) ->
    <<Image/binary,0:((byte_size(Image)*TxH) div H)/unit:8>>.

pad_image_hor(Image, W, TxW, H) ->
    OldRowSize = byte_size(Image) div H,
    NewRowSize = (OldRowSize * TxW) div W,
    Diff = NewRowSize - OldRowSize,
    << <<Row/binary,0:Diff/unit:8>> || <<Row:OldRowSize/binary>> <= Image >>.

create_buttons(Icons0) ->
    flatmap(fun({Name,{3,32,28,Icon}}) ->
		    [{{Name,down},create_button(fun active/5, Icon)},
		     {{Name,up},create_button(fun inactive/5, Icon)}];
	       (Other) -> [Other]
	    end, Icons0).

create_button(Tr, Icon) ->
    create_button(Tr, Icon, 0, 0, []).

create_button(Tr, T, 32, Y, Acc) ->
    create_button(Tr, T, 0, Y+1, Acc);
create_button(_Tr, <<>>, _X, _Y, Acc) ->
    {3,?ICON_WIDTH,?ICON_HEIGHT,list_to_binary(reverse(Acc))};
create_button(Tr, <<R:8,G:8,B:8,T/binary>>, X, Y, Acc) ->
    create_button(Tr, T, X+1, Y, [Tr(X, Y, R, G, B)|Acc]).

active(X, Y, R, G, B) ->
    if
	X < 1; X > 30; Y < 1; Y > 26 -> [255,255,255];
	true -> [R,G,B]
    end.

inactive(_X, _Y, R, G, B) -> [R,G,B].

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

