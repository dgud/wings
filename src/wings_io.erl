%%
%%  wings_io.erl --
%%
%%     This module contains most of the low-level GUI for Wings.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_io.erl,v 1.143 2006/08/23 02:56:49 antoneos Exp $
%%

-module(wings_io).
-export([init/0,resize/0,
	 set_cursor/1,hourglass/0,eyedropper/0,
	 info/1,
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
	 set_timer/2,cancel_timer/1]).

-export([reset_grab/0,grab/0,ungrab/2,is_grabbed/0,warp/2]).
-export([ortho_setup/0,ortho_setup/1]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [flatmap/2,foldl/3,keysearch/3,member/2,
		reverse/1,reverse/2,foreach/2,last/1]).

-define(ICON_WIDTH, 32).
-define(ICON_HEIGHT, 28).

-define(TX_WIDTH, 256).
-define(TX_HEIGHT, 128).

-define(EVENT_QUEUE, wings_io_event_queue).
-define(ACTIVE_TX, wings_io_active_tx).

-record(io,
	{tex=[],				%Textures.
	 grab_count=0,				%Number of grabs.
	 cursors,				%Mouse cursors.
	 raw_icons				%Raw icon bundle.
	}).

init() ->
    Cursors = build_cursors(),
    Icons = read_icons(),
    put(?EVENT_QUEUE, queue:new()),
    put_state(#io{raw_icons=Icons,cursors=Cursors}).

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
    border(X, Y, W, H, FillColor, {0.20,0.20,0.20}).

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
    case keysearch(Icon, 1, Tex) of
	false -> ok;
	{value,{Icon,{Id,W,H,MinU,MinV,MaxU,MaxV}}} ->
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
    Mem = sdl_util:alloc(3*?TX_WIDTH*?TX_HEIGHT, ?GL_BYTE),
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

create_textures_2([{Name,{Bpp,W,H,Icon}}|T]) ->
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
		  W, H, 0, Format, ?GL_UNSIGNED_BYTE, Icon),
    [{Name,{TxId,W,H,0,0,1,1}}|create_textures_2(T)];
create_textures_2([]) -> [].

div_uv(0, _) -> 0;
div_uv(X, X) -> 1;
div_uv(X, Y) -> X/Y.

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
%%% Input.
%%%

putback_event(Event) ->
    {In,Out} = get(?EVENT_QUEUE),
    put(?EVENT_QUEUE, {In,[Event|Out]}).

putback_event_once(Ev) ->
    {In,Out} = get(?EVENT_QUEUE),
    case member(Ev, In) orelse member(Ev, Out) of
	true -> ok;
	false -> put(?EVENT_QUEUE, {In,[Ev|Out]})
    end.

get_event() ->
    case get_sdl_event() of
	{quit} -> quit;
	Other -> Other
     end.

get_matching_events(Filter) ->
    Eq = get(?EVENT_QUEUE),
    get_matching_events_1(Filter, Eq, [], []).

get_matching_events_1(Filter, Eq0, Match, NoMatch) ->
    case queue:out(Eq0) of
	{{value,Ev},Eq} ->
	    case Filter(Ev) of
		false ->
		    get_matching_events_1(Filter, Eq, Match, [Ev|NoMatch]);
		true ->
		    get_matching_events_1(Filter, Eq, [Ev|Match], NoMatch)
	    end;
	{empty,{In,Out}} ->
	    case Match of
		[] -> [];
		_ ->
		    put(?EVENT_QUEUE, {In,reverse(NoMatch, Out)}),
		    Match
	    end
    end.

get_sdl_event() ->
    Eq0 = get(?EVENT_QUEUE),
    {Event,Eq} = read_events(Eq0),
    put(?EVENT_QUEUE, Eq),
    Event.

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

%%%
%%% Mouse grabbing.
%%%
reset_grab() ->
    Io = get_state(),
    put_state(Io#io{grab_count=0}),
    sdl_mouse:showCursor(true),
    sdl_video:wm_grabInput(?SDL_GRAB_OFF).

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
    sdl_video:wm_grabInput(?SDL_GRAB_ON);
do_grab(_N) -> ok.

ungrab(X, Y) ->
    %%io:format("UNGRAB mouse~n", []),
    case get_state() of
	#io{grab_count=0} -> no_grab;
	#io{grab_count=Cnt}=Io ->
	    put_state(Io#io{grab_count=Cnt-1}),
	    case Cnt-1 of
		0 ->
		    sdl_video:wm_grabInput(?SDL_GRAB_OFF),
		    sdl_mouse:warpMouse(X, Y),
		    sdl_mouse:showCursor(true),
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
        "  ............................	 "
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
       	"  ............................	 ".

eyedropper_data() ->
        "             XXX"
	"            X.XX"
	"         X X..XX"
	"        X.X..XX "
	"       X....XX  "
	"        XXXXX   "
	"      	X.XXXXX  "
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
        "       xx       "
        "      x..x      "
        "      x..x      "
       	"      x..x      "
       	"  xx  x..xx     "
        " x..x x..x.xx   "
        " x...xx..x.x.xx "
       	" x....x..x.x.x.x"
       	"  x...x......x.x"
	"   x...........x"
	"   x...........x"
       	"   x..........x "
        "    x.........x "
        "    x........x  "
        "    x........x  "
        "    x........x  ".

closed_hand_data() ->
        "                "
        "                "
        "                "
       	"                "
        "  x xx xx xx    "
        " x.x..x..x.x.xx "
       	"x..x..x..x..x..x"
       	"x...........x..x"
	" x.............x"
	"x..............x"
       	"x..............x"
        "x.............x "
        "x.............x "
        " x...........x  "
        "  x..........x  "
        "  x..........x  ".

