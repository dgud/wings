%%
%%  wings_text.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2001-2013 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_text).
-export([init/0,resize/0,width/0,width/1,height/0,render/3]).
-export([font_cw_lh/1]).
-export([break_lines/2]).
-export([make_wxfont/1, reload_font/2, get_font_info/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("wings/intl_tools/wings_chars.hrl").
-compile({parse_transform,ms_transform}).

-import(lists, [reverse/1]).

init() ->
    set_font_default(system_font),
    set_font_default(console_font),

    Ranges = char_ranges(wings_pref:get_value(language)),

    WxSys = make_wxfont(wings_pref:get_value(system_font)),
    {ok, Sys} = wings_glfont:load_font(WxSys, [{range, Ranges}]),
    WxCon = make_wxfont(wings_pref:get_value(console_font)),
    %% {ok, Console} = wings_glfont:load_font(WxCon, [{range, Ranges}]),

    ?SET(system_font, Sys),
    ?SET(system_font_wx, WxSys),
    %% ?SET(console_font, Console),
    ?SET(console_font_wx, WxCon),
    ok.

reload_font(PrefKey, FontInfo) ->
    Ranges = char_ranges(wings_pref:get_value(language)),
    WxFont = make_wxfont(FontInfo),
    {ok, GLFont} = wings_glfont:load_font(WxFont, [{range, Ranges}]),
    PrefKey =:= system_font andalso ?SET(system_font_wx, WxFont),
    ?SET(PrefKey, GLFont).

set_font_default(PrefKey) ->
    case is_atom(wings_pref:get_value(PrefKey)) of
	true ->
	    wings_pref:set_value(PrefKey, get_font_default(PrefKey));
	false ->
	    wings_pref:set_default(PrefKey, get_font_default(PrefKey))
    end.

get_font_default(system_font) ->
    get_font_info(?wxNORMAL_FONT);
get_font_default(console_font) ->
    get_font_info(?wxSWISS_FONT).

make_wxfont(#{type:=font, face:=FaceName, size:=Size,
	      style:=Style0, weight:=Weight0}) ->
    Style  = wxfont_style(Style0),
    Weight = wxfont_weight(Weight0),
    Font = wxFont:new(Size, ?wxDEFAULT, Style, Weight, [{face, FaceName}]),
    case wxFont:ok(Font) of
	true  -> Font;
	false -> wxFont:new(Size, ?wxDEFAULT, Style, Weight)
    end.

get_font_info(DefFont) ->
    case {(catch wx:getObjectType(DefFont) =:= wxFont), DefFont} of
	{true, _} -> get_font_info_1(DefFont);
	{_, Info = #{type:=font}} ->
	    Info
    end.

get_font_info_1(Font) ->
    Name  = wxFont:getFaceName(Font),
    Style = wxFont:getStyle(Font),
    Weight= wxFont:getWeight(Font),
    Size = wxFont:getPointSize(Font),
    #{type=>font, face=>Name, size=>Size,
      style=>font_style(Style), weight=>font_weight(Weight)}.

font_style(?wxFONTSTYLE_NORMAL) -> normal;
font_style(?wxFONTSTYLE_ITALIC) -> italic;
font_style(?wxFONTSTYLE_SLANT) ->  slant.

wxfont_style(normal) -> ?wxFONTSTYLE_NORMAL;
wxfont_style(italic) -> ?wxFONTSTYLE_ITALIC;
wxfont_style(slant)  -> ?wxFONTSTYLE_SLANT.

font_weight(?wxFONTWEIGHT_NORMAL) -> normal;
font_weight(?wxFONTWEIGHT_LIGHT) -> light;
font_weight(?wxFONTWEIGHT_BOLD) ->  bold.

wxfont_weight(normal) -> ?wxFONTWEIGHT_NORMAL;
wxfont_weight(bold) -> ?wxFONTWEIGHT_BOLD;
wxfont_weight(light)  -> ?wxFONTWEIGHT_LIGHT.

char_ranges("jp") -> ?WINGS_CHARS_JP;
char_ranges("ko") -> ?WINGS_CHARS_KO;
char_ranges("zh-cn") -> ?WINGS_CHARS_ZH_CN;
char_ranges("zh-tw") -> ?WINGS_CHARS_ZH_TW;
char_ranges(_) -> ?WINGS_CHARS_EURO.

resize() ->  ok.

width(S) when is_list(S)->
    width_1(S, [], 0).

width_1([{bold,S}|Cs], Acc, W) ->
    BSW = bold_string_width(S),
    width_1(Cs, Acc, BSW+W);
width_1([{ul,S}|Cs], Acc, W) ->
    width_1(Cs, [S, Acc], W);
width_1([C0|Cs], Acc, W) when is_atom(C0) ->
    case special(C0) of
	C when is_integer(C) ->
	    width_1([C|Cs], Acc, W);
	_ ->
	    width_1(Cs, Acc, W)
    end;
width_1([[C|R]|Cs], Acc, W0) ->
    W = width_1(R, [C|Acc], W0),
    width_1(Cs, [], W);
width_1([C|Cs], Acc, W) ->
    width_1(Cs, [C|Acc], W);
width_1([], [], W) -> W;
width_1([], RS, W) ->
    {W1,_} = wings_glfont:text_size(current_font(), reverse(RS)),
    W1 + W.

bold_string_width(String) ->
    {W,_} = wings_glfont:text_size(current_font(), String),
    W+2.

width() ->
    glyph_info(width).

height() ->
    glyph_info(height).

font_cw_lh(Font) ->
    {glyph_info(Font, width), glyph_info(Font, height)}.

render(X, Y, S) ->
    Res = render_1(S, Font=current_font(), {X, Y, <<>>}),
    gl:pushAttrib(?GL_TEXTURE_BIT bor ?GL_ENABLE_BIT),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
    wings_glfont:render(Font, Res),
    gl:popAttrib(),
    ok.

render_1([{bold, S}|Rest], Font, {X0, Y0, _} = Acc0) ->
    {_, _, Bin} = wings_glfont:render_to_binary(Font, S, Acc0),
    Acc = wings_glfont:render_to_binary(Font, S, {X0+1, Y0, Bin}),
    render_1(Rest, Font, Acc);
render_1([{ul,S}|Cs], Font, Acc0) -> %% ignore for now
    Acc = wings_glfont:render_to_binary(Font, S, Acc0),
    render_1(Cs, Font, Acc);
render_1([List|Cs], Font, Acc0) when is_list(List) ->
    Acc = render_1(List, Font, Acc0),
    render_1(Cs, Font, Acc);
render_1([C|Cs], Font, Acc) ->
    render_1(Cs, Font, char(C, Font, Acc));
render_1([], _, Acc) -> Acc.

char(C, Font, Acc) when is_integer(C) -> 
    wings_glfont:render_to_binary(Font, [C], Acc);
char(C, Font, Acc) when is_atom(C) ->
    case special(C) of
	Int when is_integer(Int) ->
	    char(Int, Font, Acc);
	false ->
	    io:format("Special missing ~p ~n",[C]),
	    Acc
    end.

current_font() ->
    case wings_wm:this() of
	none ->
	    ?GET(system_font);
	This ->
            ?GET(wings_wm:get_prop(This, font))
    end.

%% Formats strings to fit the width of a line length given in PIXELS

-record(tb,		% text box record
    {text=[],	% input string
     lw=0,		% length of current line in pixels
     max=600,		% max allowed line length in pixels
     word=[], 	% accumulated letters from the current word
     line=[],	% current line acc of words
     res=[]}).	% output string with line breaks

break_lines(InputText, infinite) ->
    {1, InputText};
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

glyph_info(C) ->
    glyph_info_1(current_font(), C).

glyph_info(Font, C) ->
    wings_glfont:C(?GET(Font)).

glyph_info_1(Font, C) ->
    wings_glfont:C(Font).

%%%
%%% Special characters.
%%%

%% cw_large(bullet) -> 6;
%% cw_large(option_box) -> 7;
%% cw_large(command) -> 8;
%% cw_large(folder) -> 14;
%% cw_large(option) -> 14;
%% cw_large(shift) -> 14;
%% cw_large(caret) -> 2;
%% cw_large(crossmark) -> 8.

special(axisx) -> $X;
special(axisy) -> $Y;
special(axisz) -> $Z;
special(caret) -> $¦;
special(crossmark) -> $✓;
special(_C) -> false.

