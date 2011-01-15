%%
%%  wings_font_table.erl --
%%
%%     Functions to access the accumulated font table in ets comprised of seen
%%     glyphs. Seen returns the glyph, unseen returns 'undefined' and then
%%     proceeds to look up the glyph in the full font table.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_font_table).
-export([draw/1,char/1,bold_char/1,bold_char_width/1,char_width/1]).

draw([C|T]) ->
    char(C),
    draw(T);
draw([]) -> ok.

bold_char_width(C) ->
    case glyph_info(C) of
        undefined -> undefined;
    Glyph ->
        glyph_width(Glyph)+1
    end.

char_width(C) ->
    case glyph_info(C) of
        undefined -> undefined;
    Glyph ->
        glyph_width(Glyph)
    end.

bold_char(C) ->
    case glyph_info(C) of
        undefined -> undefined;
    Glyph ->
        draw_glyph(Glyph),
        Cw = glyph_width(Glyph),
        gl:bitmap(1, 1, 0, 0, -Cw+1, 0, <<0>>),
        draw_glyph(Glyph)
    end.

char(C) when C=:=char_width; C=:=char_height ->
    case glyph_info(C) of
        undefined -> undefined;
        Glyph -> Glyph
    end;
char(C) ->
    case glyph_info(C) of
        undefined -> undefined;
        Glyph -> draw_glyph(Glyph)
    end.

draw_glyph({W,H,Xorig,Yorig,Xmove,B}) -> 
    gl:bitmap(W, H, Xorig, Yorig, Xmove, 0, B).

glyph_info(C) ->
    Font = case wings_wm:this() of
        console -> console_font;
        _ -> system_font
    end,
    case ets:lookup(Font, C) of
        [] ->
            undefined;
        [{C,BitMap}] ->
            BitMap
    end.

glyph_width({_,_,_,_,Xmove,_}) -> Xmove.
