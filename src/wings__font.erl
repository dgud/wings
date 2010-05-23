%%
%%  wings__font.erl --
%%
%%     Generic font operations.
%%
%%  Copyright (c) 2005-2010 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings__font, [Key,Desc,Width,Height,GlyphTab,Bitmaps]).
-export([key/0,desc/0,width/0,height/0,draw/1,char/1,char_width/1,
	bold_char/1,bold_char_width/1]).

draw([C|T]) ->
    char(C),
    draw(T);
draw([]) -> ok.

key() -> Key.
desc() -> Desc.

char_width(C) ->
    element(1, glyph_info(C)).
width() ->
    insert_glyph(char_width, Width),
    Width.
height() ->
    insert_glyph(char_height, Height),
    Height.

bold_char(C) ->
    Glyph = glyph_info(C),
    draw_glyph(Glyph),
    Cw = glyph_width(Glyph),
    gl:bitmap(1, 1, 0, 0, -Cw+1, 0, <<0>>),
    draw_glyph(Glyph).

bold_char_width(C) ->
    Glyph = glyph_info(C),
    glyph_width(Glyph)+1.

char(C) ->
    draw_glyph(glyph_info(C)).

draw_glyph({W,H,Xorig,Yorig,Xmove,B}) ->
    gl:bitmap(W, H, Xorig, Yorig, Xmove, 0, B).

glyph_info(C) ->
    BitMap = case ets:lookup(GlyphTab, C) of
	[] when is_integer(C), C > 0 ->
	    %% Undefined character. Return a filled box.
	    NumBytes = ((Width+7) div 8) * Height,
	    B = <<(-1):NumBytes/unit:8>>,
	    {Width,Height,0,0,Width+1,B};
	[{_,W,H,Xorig,Yorig,Xmove,Offset}] ->
	    %% Valid character.
	    NumBytes = ((W+7) div 8)*H,
	    <<_:Offset/binary,B:NumBytes/binary,_/binary>> = Bitmaps,
	    {W,H,Xorig,Yorig,Xmove,B}
    end,
    insert_glyph(C, BitMap),
    BitMap.

glyph_width({_,_,_,_,Xmove,_}) -> Xmove.

insert_glyph(C, BitMap) ->
    Font = case wings_wm:this() of
        console -> console_font;
        _ -> system_font
    end,
    ets:insert(Font, {C,BitMap}).
