%%
%%  wings_text.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_text).
-export([init/0,resize/0,width/0,width/1,height/0,draw/1,char/1,bold/1]).
-export([break_lines/2]).
-export([fonts/0]).
-export([current_font/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-compile({parse_transform,ms_transform}).

-import(lists, [reverse/1,foreach/2]).

init() ->
    wings_pref:set_default(new_system_font, '7x14'),
    wings_pref:set_default(new_console_font, 'fixed7x14'),
    ets:new(system_font, [named_table,ordered_set,public]),
    ets:new(console_font, [named_table,ordered_set,public]),
    ets:new(wings_fonts, [named_table,ordered_set,public]),
    load_fonts().

resize() ->
    %% Force rebuild of display lists next time each font
    %% is needed.
    MatchSpec = ets:fun2ms(fun({_Key,Font,_Desc}) -> Font end),
    foreach(fun(Font) -> erase(Font) end,
	    ets:select(wings_fonts, MatchSpec)).

width(S) when is_list(S)->
    width_1(S, 0).

width_1([{bold,S}|Cs], W) ->
    BSW = bold_string_width(S, 0),
    width_1(Cs, BSW+W);
width_1([{ul,S}|Cs], W) ->
    SW = width_1(S, 0),
    width_1(Cs, SW+W);
width_1([C|Cs], W) when is_atom(C) ->
    CW = case ?CHAR_WIDTH < 7 of
       true -> cw_small(C);
       false -> cw_large(C)
    end,
    width_1(Cs, CW+W);
width_1([[C|R]|Cs], W0) ->
    CW = char_width(C),
    W = width_1(R, CW+W0),
    width_1(Cs, W);
width_1([C|Cs], W) ->
    CW = char_width(C),
    width_1(Cs, CW+W);
width_1([], W) -> W.

bold_string_width([C|S], W) ->
    BCW = case wings_font_table:bold_char_width(C) of
        undefined -> (current_font()):bold_char_width(C);
        Other -> Other
    end,
    bold_string_width(S, BCW+W);
bold_string_width([], W) ->
    W.

char_width(C) ->
    case wings_font_table:char_width(C) of
        undefined -> (current_font()):char_width(C);
        Other -> Other
    end.

width() ->
    case wings_font_table:char(char_width) of
        undefined -> (current_font()):width();
        Other -> Other
    end.

height() ->
    case wings_font_table:char(char_height) of
        undefined -> (current_font()):height();
        Other -> Other
    end.

draw([{bold,S}|Cs]) ->
    bold(S),
    draw(Cs);
draw([{ul,S}|Cs]) ->
    draw(S),
    draw(Cs);
draw([C|Cs]) when is_atom(C) ->
    special(C),
    draw(Cs);
draw([[C|R]|Cs]) ->
    char(C),
    draw(R),
    draw(Cs);
draw([C|Cs]) ->
    char(C),
    draw(Cs);
draw([]) -> ok.

char(C) when is_atom(C) -> special(C);
char(C) ->
    case wings_font_table:char(C) of
        undefined -> (current_font()):char(C);
        Other -> Other
    end.

bold([C|S]) ->
    case wings_font_table:bold_char(C) of
        undefined -> (current_font()):bold_char(C);
        Other -> Other
    end,
    bold(S);
bold([]) -> ok.

%% Table of characters already seen.
%% Because the CJK fonts are HUGE (+30000 glyphs), I wrote a character
%% accumulator. The reason for this is due to the nature of the ets, which when
%% accessed, copies the requested data to the memory of the local process. With
%% the smaller font libraries, this wasn't a problem, but with the CJK font for
%% supporting Chinese, Japanese, and Korean - this became an issue.

current_font() ->
    case wings_wm:this() of
	none ->
	    FontKey = wings_pref:get_value(new_system_font),
	    ets:lookup_element(wings_fonts, FontKey, 2);
	This ->
	    FontKey = wings_wm:get_prop(This, font),
	    ets:lookup_element(wings_fonts, FontKey, 2)
    end.

fonts() ->
    MatchSpec = ets:fun2ms(fun({Key,_Font,Desc}) -> {Desc,Key} end),
    ets:select(wings_fonts, MatchSpec).


%% Formats strings to fit the width of a line length given in PIXELS

-record(tb,		% text box record
    {text=[],	% input string
     lw=0,		% length of current line in pixels
     max=600,		% max allowed line length in pixels
     word=[], 	% accumulated letters from the current word
     line=[],	% current line acc of words
     res=[]}).	% output string with line breaks

break_lines(InputText, infinite) ->
    {W,_} = wings_wm:top_size(),
    break_lines(InputText, W-40);
break_lines(InputText, W) ->
%% Returns Text formatted with line breaks to max width of W and height in Rows
    CW = ?CHAR_WIDTH,
    Text0 = lists:flatten(InputText),
    Width = max(W-CW*2, CW*3),
    #tb{res=Text} = string_to_text_box(#tb{text=Text0, max=Width}),
    OutputText = reverse(Text),
    Rows = length(Text),
    {Rows,OutputText}.

%% String parsing for Text Box
string_to_text_box(#tb{lw=Lw,line=[]}=Tb) when Lw =/= 0 ->
    string_to_text_box(Tb#tb{lw=0});
string_to_text_box(#tb{lw=LineWidth,max=Max,line=Line0,res=Res0}=Tb)
  when LineWidth > Max ->
  %% Make a New Line when the LineWidth reaches the Max
    {Word,Line} = lists:splitwith(fun(Char) -> Char=/=$\s end, Line0),
    case Line of
        [] ->
            [NextLine|HyphinateLine] = Line0,
            Res = [reverse([$\n,$-|HyphinateLine])|Res0],
            LW = wings_text:width([NextLine]),
            string_to_text_box(Tb#tb{lw=LW,line=[NextLine],res=Res});
        _ ->
            Res = [reverse([$\n|Line])|Res0],
            LW = wings_text:width(Word),
            string_to_text_box(Tb#tb{lw=LW,line=Word,res=Res})
    end;

string_to_text_box(#tb{text=[$\n|Text],line=Line,res=Res0}=Tb) ->
    Res = [reverse([$\n|Line])|Res0],
    string_to_text_box(Tb#tb{text=Text,line=[],res=Res});

string_to_text_box(#tb{text=[$\t|Text],lw=LW,line=Line0}=Tb) ->
    CharWidth = wings_text:width([$\s])*2,
    Line = [$\s,$\s|Line0],
    string_to_text_box(Tb#tb{text=Text,lw=LW+CharWidth,line=Line});
    
string_to_text_box(#tb{text=[{Style,String}=Stylized|Text],lw=Lw, max=Max, line=Line0,res=Res}=Tb0) ->
    Sw = width([Stylized]),
    NLW = Lw + Sw,
    case NLW =< Max of
        true ->
            Line = [Stylized|Line0],
            string_to_text_box(Tb0#tb{text=Text,lw=NLW,line=Line});
        false ->
            Rem = Max - Lw,
            Parts = Max/Sw,
            Section = Sw * Parts/1.5,
            {Line,ResAcc,_} = lists:foldl(fun
                (C, {Acc,R,I}) when I > Section -> {[],[reverse_list([$\n,$-,{Style,[C]}|Acc])|R],0};
                (C, {Acc,R,I}) -> {[{Style,[C]}|Acc],R,I+width([C])}
            end, {[],Line0,Rem}, String),
            string_to_text_box(Tb0#tb{text=Text,lw=width(Line),line=Line,res=[reverse_list(ResAcc)|Res]})
    end;

string_to_text_box(#tb{text=[Char|Text],lw=LineWidth0,line=Line}=Tb) ->
    CharWidth = wings_text:width([Char]),
    LW = LineWidth0+CharWidth,
    string_to_text_box(Tb#tb{text=Text,lw=LW,line=[Char|Line]});

string_to_text_box(#tb{text=[],line=Line,res=Res0}=Tb) ->
    Res = [reverse_list(Line)|Res0],
    Tb#tb{line=[],res=Res}.

%% Avoid single item list reverse error.
reverse_list(A) when length(A) < 2 -> A;
reverse_list(A) -> reverse(A).

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

%%%
%%% Load a Wings font.
%%%

load_fonts() ->
    SystemFont = wings_pref:get_value(new_system_font),
    ConsoleFont = wings_pref:get_value(new_console_font),
    WingsDir = wings_util:lib_dir(wings),
    WF = ".wingsfont",
    SFont = filename:join([WingsDir,"fonts",atom_to_list(SystemFont)++WF]),
    CFont = filename:join([WingsDir,"fonts",atom_to_list(ConsoleFont)++WF]),
    %% Make sure font is available, otherwise load default font
    System = case filelib:is_file(SFont) of
        true -> SystemFont;
        false ->
            wings_pref:set_value(new_system_font, '7x14'),
            '7x14'
    end,
    Console = case filelib:is_file(CFont) of
        true -> ConsoleFont;
        false ->
            wings_pref:set_value(new_console_font, 'fixed7x14'),
            'fixed7x14'
    end,
    Wc = filename:join([WingsDir,"fonts","*.wingsfont"]),
    Fonts = filelib:wildcard(Wc),
    foreach(fun(F) ->
        load_font(System, Console, F)
    end, Fonts).

load_font(SystemFont, ConsoleFont, FontDir) ->
    FontNameStr = filename:basename(FontDir, ".wingsfont"),
    FontNameAtom = list_to_atom(FontNameStr),
    case FontNameAtom of
        SystemFont -> load_font_0(FontDir);
        ConsoleFont -> load_font_0(FontDir);
        _other -> ets:insert(wings_fonts, {FontNameAtom,ok,FontNameStr})
    end.

load_font_0(FontDir) ->
    {ok,Bin} = file:read_file(FontDir),
    Font = binary_to_term(Bin),
    Mod = load_font_1(Font),
    Key = Mod:key(),
    Desc = Mod:desc(),
    ets:insert(wings_fonts, {Key,Mod,Desc}).

load_font_1({wings_font,?wings_version,Font}) ->
    load_font_2(Font).

load_font_2({Key,Desc,Width,Height,GlyphInfo,Bitmaps}) ->
    T = ets:new(font, [set,public]),
    ets:insert(T, GlyphInfo),
    wings__font:new(Key, Desc, Width, Height, T, Bitmaps).
