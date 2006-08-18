%%
%%  wings_text.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_text.erl,v 1.33 2005/04/23 16:52:18 bjorng Exp $
%%

-module(wings_text).
-export([init/0,resize/0,width/0,width/1,height/0,draw/1,char/1,bold/1]).
-export([break_lines/2]).
-export([fonts/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-compile({parse_transform,ms_transform}).

-import(lists, [reverse/1,foldl/3,foreach/2]).

init() ->
    ets:new(wings_fonts, [named_table,ordered_set]),
    load_fonts(),
    verify_font(new_system_font),
    verify_font(new_console_font),
    wings_pref:set_default(new_system_font, '7x14'),
    wings_pref:set_default(new_console_font, 'fixed7x14').

resize() ->
    %% Force rebuild of display lists next time each font
    %% is needed.
    MatchSpec = ets:fun2ms(fun({_Key,Font,_Desc}) -> Font end),
    foreach(fun(Font) -> erase(Font) end,
	    ets:select(wings_fonts, MatchSpec)).

width(S) ->
    Mod = current_font(),
    CwFun = case width() of
		W when W < 7 -> fun cw_small/1;
		_ -> fun cw_large/1
	    end,
    WF0 = Mod:width_fun(),
    WF = fun(C, W) when is_atom(C) ->
		 W+CwFun(C);
	    (C, W) ->
		 W+WF0(C)
	 end,
    width_1(S, WF, 0).

width_1([{bold,S}|Cs], WF, W) ->
    width_1(Cs, WF, width_1(S, WF, W+length(S)));
width_1([C|Cs], WF, W) ->
    width_1(Cs, WF, WF(C, W));
width_1([], _, W) -> W.

width() -> (current_font()):width().

height() -> (current_font()):height().

draw(S) ->
    Font = current_font(),
    case wings_pref:get_value(text_display_lists, false) of
	true ->
	    ListBase = case get(Font) of
			    undefined -> make_font_dlists(Font);
			    Base -> Base
			end,
	    gl:listBase(ListBase),
	    gl:callLists(length(S), ?GL_UNSIGNED_BYTE, S);
	false ->
	    Font:draw(S)
    end.

make_font_dlists(Font) ->
    Base = gl:genLists(256),
    put(Font, Base),
    make_font_dlists_1(0, Base).

make_font_dlists_1(256, Base) -> Base;
make_font_dlists_1(C, Base) ->
    gl:newList(Base+C, ?GL_COMPILE),
    char(C),
    gl:endList(),
    make_font_dlists_1(C+1, Base).

char(C) when is_atom(C) ->
    special(C);
char(C) ->
    try
	(current_font()):char(C)
    catch
	error:function_clause ->
	    bad_char(C);
	  error:{badchar,_} ->
	    bad_char(C)
    end.

bold(S) ->
    (current_font()):bold(S).

current_font() ->
    case wings_wm:this() of
	none ->
	    FontKey = wings_pref:get_value(new_system_font),
	    [{_,Font,_}] = ets:lookup(wings_fonts, FontKey),
	    Font;
	This ->
	    FontKey = wings_wm:get_prop(This, font),
	    [{_,Font,_}] = ets:lookup(wings_fonts, FontKey),
	    Font
    end.

verify_font(PrefKey) ->
    case wings_pref:get_value(PrefKey) of
	undefined -> ok;
	FontKey ->
	    case is_font(FontKey) of
		true -> ok;
		false ->
		    wings_pref:delete_value(PrefKey),
		    case atom_to_list(FontKey) of
			"wpf_"++Key ->
			    NewFontKey = list_to_atom(Key),
			    case is_font(NewFontKey) of
				false -> ok;
				true ->
				    wings_pref:set_value(PrefKey, NewFontKey)
			    end;
			_ -> ok
		    end
	    end
    end.

is_font(FontKey) ->
    case ets:lookup(wings_fonts, FontKey) of
	[] -> false;
	[_] -> true
    end.

fonts() ->
    MatchSpec = ets:fun2ms(fun({Key,_Font,Desc}) -> {Desc,Key} end),
    ets:select(wings_fonts, MatchSpec).

break_lines(Lines, Limit) ->
    break_lines(Lines, Limit, 0, []).

break_lines([S|T], Limit, Rows, Acc) ->
    break_line(S, T, Limit, Rows, Acc);
break_lines([], _, Rows, Lines) ->
    {Rows,reverse(Lines)}.

break_line(S, T, Limit, Rows, Acc) ->
    case break_line_1(S, Limit) of
	done when T =/= [] ->
	    break_lines(T, Limit, Rows+1, [[]|Acc]);
	done ->
	    break_lines(T, Limit, Rows, Acc);
	{Line,More} ->
	    break_line(More, T, Limit, Rows+1, [Line|Acc])
    end.

break_line_1([$\n|T], Limit) -> break_line_1(T, Limit);
break_line_1([$\s|T], Limit) -> break_line_1(T, Limit);
break_line_1([], _) -> done;
break_line_1(T, Limit) -> break_line_2(T, 0, Limit, [], []).

break_line_2(_, N, Limit, _Acc, {Bef,More}) when N > Limit ->
    {reverse(Bef),More};
break_line_2([$\n|T], _N, _Limit, Acc, _Break) ->
    {reverse(Acc),T};
break_line_2([$\s|T0], N, Limit, Acc, _Break) ->
    T = skip_blanks(T0),
    break_line_2(T, N+1, Limit, [$\s|Acc], {Acc,T});
break_line_2([{_,Str}=C|T], N, Limit, Acc, Break) ->
    break_line_2(T, N+length(Str), Limit, [C|Acc], Break);
break_line_2([C|T], N, Limit, Acc, Break) ->
    break_line_2(T, N+1, Limit, [C|Acc], Break);
break_line_2([], _, _Limit, Acc, _Break) -> {reverse(Acc),[]}.

skip_blanks([$\n|T]) -> skip_blanks(T);
skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks(T) -> T.

%%%
%%% Special characters.
%%%

cw_small(bullet) -> 5;
cw_small(option_box) -> 6;
cw_small(command) -> 8;
cw_small(folder) -> 12;
cw_small(option) -> 12;
cw_small(shift) -> 13;
cw_small(caret) -> 2;
cw_small(crossmark) -> 8.

cw_large(bullet) -> 6;
cw_large(option_box) -> 7;
cw_large(command) -> 8;
cw_large(folder) -> 14;
cw_large(option) -> 14;
cw_large(shift) -> 14;
cw_large(caret) -> 2;
cw_large(crossmark) -> 8.

special(C) ->
    case width() of
	W when W < 7 ->
	    special_small(C);
	_ ->
	    special_large(C)
    end.

special_small(bullet) ->
    B = <<
	 2#01100000,
	 2#11110000,
	 2#01100000
	 >>,
    gl:bitmap(4, 3, 0, -2, 5, 0, B);
special_small(option_box) ->
    B = <<
	 2#11111100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#11111100,
	 2#11111100
	 >>,
    gl:bitmap(6, 8, 0, 3, 6, 0, B);

special_small(command) ->
    B = <<
       	 2#01000100,
       	 2#10101010,
       	 2#10101010,
	 2#01111100,
	 2#00101000,
	 2#01111100,
       	 2#10101010,
       	 2#10101010,
       	 2#01000100>>,
    gl:bitmap(7, 9, 0, 0, 8, 0, B);

special_small(option) ->
    B = <<
	 2#00000000111000000:16,
	 2#00000001000000000:16,
	 2#00000010000000000:16,
       	 2#00000100000000000:16,
       	 2#00001000000000000:16,
       	 2#11110001111000000:16>>,
    gl:bitmap(11, 6, 0, 0, 12, 0, B);

special_small(shift) ->
    B = <<
	 2#0000111110000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
	 2#0011100011100000:16,
	 2#0001100011000000:16,
       	 2#0000110110000000:16,
       	 2#0000011100000000:16,
       	 2#0000001000000000:16>>,
    gl:bitmap(12, 8, 0, 0, 13, 0, B);

special_small(caret) ->
    caret();

special_small(crossmark) ->
    B = <<
	 2#00100000,
	 2#01110000,
	 2#11111000,
	 2#11011100,
	 2#10001110,
	 2#00000110,
	 2#00000010
	 >>,
    gl:bitmap(7, 7, 0, 0, 8, 0, B);

special_small(axisx) ->
    B = <<16#63,16#63,16#36,16#3e,16#1c,16#3e,16#36,16#63,16#63>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(axisy) ->
    B = <<16#18,16#18,16#18,16#18,16#3c,16#3c,16#66,16#e7,16#c3>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(axisz) ->
    B = <<16#7f,16#60,16#70,16#38,16#1c,16#0e,16#07,16#03,16#7f>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_small(folder) ->
    B = <<
       	 2#0111111111000000:16,
	 2#0100000001000000:16,
	 2#0100000001000000:16,
       	 2#0100000001000000:16,
	 2#0100000001000000:16,
	 2#0111111111000000:16,
       	 2#0010001000000000:16,
       	 2#0001110000000000:16,
       	 2#0000000000000000:16>>,
    gl:bitmap(11, 9, 0, 0, 12, 0, B).

special_large(bullet) ->
    B = <<
	 2#01110000,
	 2#11111000,
	 2#11111000,
	 2#01110000
	 >>,
    gl:bitmap(5, 4, 0, -2, 6, 0, B);
special_large(option_box) ->
    B = <<
	 2#11111100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#10000100,
	 2#11111100,
	 2#11111100
	 >>,
    gl:bitmap(6, 10, 0, 4, 7, 0, B);

special_large(command) ->
    B = <<
       	 2#01000100,
       	 2#10101010,
       	 2#10101010,
	 2#01111100,
	 2#00101000,
	 2#01111100,
       	 2#10101010,
       	 2#10101010,
       	 2#01000100>>,
    gl:bitmap(7, 9, 0, 0, 8, 0, B);

special_large(option) ->
    B = <<
	 2#0000000001111000:16,
	 2#0000000010000000:16,
	 2#0000000100000000:16,
	 2#0000001000000000:16,
       	 2#0000010000000000:16,
       	 2#0000100000000000:16,
       	 2#1111000111111000:16>>,
    gl:bitmap(13, 7, 0, 0, 14, 0, B);

special_large(shift) ->
    B = <<
	 2#0000111110000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
	 2#0000100010000000:16,
       	 2#0111100011110000:16,
	 2#0011000001100000:16,
	 2#0001100011000000:16,
       	 2#0000110110000000:16,
       	 2#0000011100000000:16,
       	 2#0000001000000000:16>>,
    gl:bitmap(13, 10, 0, 0, 14, 0, B);

special_large(caret) ->
    caret();

special_large(crossmark) ->
    B = <<
	 2#00100000,
	 2#01110000,
	 2#11111000,
	 2#11011100,
	 2#10001110,
	 2#00000110,
	 2#00000010
	 >>,
    gl:bitmap(7, 7, 0, 0, 8, 0, B);

special_large(axisx) ->
    B = <<16#63,16#63,16#36,16#3e,16#1c,16#3e,16#36,16#63,16#63>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(axisy) ->
    B = <<16#18,16#18,16#18,16#18,16#3c,16#3c,16#66,16#e7,16#c3>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(axisz) ->
    B = <<16#7f,16#60,16#70,16#38,16#1c,16#0e,16#07,16#03,16#7f>>,
    gl:bitmap(8, 9, 0, 0, 8, 0, B);

special_large(folder) ->
    B = <<
       	 2#0111111111110000:16,
	 2#0100000000010000:16,
	 2#0100000000010000:16,
	 2#0100000000010000:16,
       	 2#0100000000010000:16,
	 2#0100000000010000:16,
	 2#0111111111110000:16,
       	 2#0010000100000000:16,
       	 2#0001111000000000:16,
       	 2#0000000000000000:16>>,
    gl:bitmap(13, 10, 0, 0, 14, 0, B).

caret() ->
    H = height(),
    B = list_to_binary([2#11011000,
			lists:duplicate(H-2, 2#00100000),
			2#11011000]),
    gl:bitmap(5, H, 2, 2, 2, 0, B).

bad_char(C) ->
    W = width([C]),
    H = height(),
    B = list_to_binary(lists:duplicate(((W+7) div 8)*H, 16#FF)),
    gl:bitmap(W, H, 0, 0, W+1, 0, B).

%%%
%%% Load a Wings font.
%%%

load_fonts() ->
    Wc = filename:join([code:lib_dir(wings),"fonts","*.wingsfont"]),
    Fonts = filelib:wildcard(Wc),
    foreach(fun(F) -> load_font(F) end, Fonts).

load_font(Name) ->
    {ok,Bin} = file:read_file(Name),
    Font = binary_to_term(Bin),
    Mod = load_font_1(Font),
    Key = Mod:key(),
    Desc = Mod:desc(),
    ets:insert(wings_fonts, {Key,Mod,Desc}).

load_font_1({wings_font,?wings_version,Font}) ->
    load_font_2(Font).

load_font_2({Key,Desc,Width,Height,GlyphInfo,Bitmaps}) ->
    T = ets:new(font, [set]),
    ets:insert(T, GlyphInfo),
    wings__font:new(Key, Desc, Width, Height, T, Bitmaps).
