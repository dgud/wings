%%
%%  bdf2wingsfont.erl --
%%
%%     Conversion of BDF fonts to Wings' own font format.
%%
%%  Copyright (c) 2005-2013 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(bdf2wingsfont).
-export([convert/1]).
-include("font_version.hrl").

-import(lists, [reverse/1,sort/1,foldl/3]).

-record(glyph,
	{code,					%Unicode
	 bbx,					%Bounding box.
	 dwidth,				%Width.
	 bitmap}).

convert([Out|SrcFonts]) ->
    G = read_fonts(SrcFonts, dict:new()),
    io:format("  Writing ~s (~p glyphs)\n", [Out,length(G)]),
    write_font(G, Out),
    init:stop().

read_fonts([N|Ns], Acc0) ->
    io:format("Reading ~s\n", [N]),
    {ok,F} = file:open(N, [binary,read,read_ahead,raw]),
    Acc = read_bdf_font(F, Acc0),
    file:close(F),
    read_fonts(Ns, Acc);
read_fonts([], Acc0) ->
    Font = dict:fold(fun(_,CharSet, Acc) ->
        dict:to_list(CharSet) ++ Acc
        end, [], Acc0),
    lists:sort(Font).

read_bdf_font(F, Acc0) ->
    case read_line(F) of
	["STARTFONT","2.1"] ->
	    Ps = read_props(F),
	    {Map,CharSet} = charset(Ps, Acc0),
	    Acc = read_font_glyphs(F, CharSet),
	    Set = to_unicode(Acc, Ps),
	    dict:store(Map, Set, Acc0);
	Other ->
	    io:format("~p\n", [Other]),
	    error_msg(invalid_bdf_file)
    end.

read_props(F) ->
    case read_line(F) of
	["STARTPROPERTIES",N0] ->
	    N = list_to_integer(N0),
	    read_props_1(F, N, []);
	_ ->
	    read_props(F)
    end.

read_props_1(_, 0, Acc) -> Acc;
read_props_1(F, N, Acc) ->
    P = read_one_prop(F),
    read_props_1(F, N-1, [P|Acc]).

read_one_prop(F) ->
    read_one_prop_1(raw_read_line(F), []).

read_one_prop_1(<<C,Cs/bytes>>, Key) when C =< $\s ->
    read_one_prop_2(Cs, reverse(Key));
read_one_prop_1(<<C,Cs/bytes>>, Key) ->
    read_one_prop_1(Cs, [C|Key]).

read_one_prop_2(<<C,Cs/bytes>>, Key) when C =< $\s ->
    read_one_prop_2(Cs, Key);
read_one_prop_2(Cs, Key) ->
    Val0 = reverse(skip_whitespace(reverse(binary_to_list(Cs)))),
    Val = convert_val(Val0),
    {Key,Val}.

convert_val("\""++Str0) ->
    "\""++Str = reverse(Str0),
    reverse(Str);
convert_val(Str) ->
    list_to_integer(Str).

charset(Ps, Sets) ->
    R = proplists:get_value("CHARSET_REGISTRY", Ps),
    E = proplists:get_value("CHARSET_ENCODING", Ps),
    CharMap = R ++ E,
    case dict:find(CharMap, Sets) of
      error -> {CharMap, dict:new()};
      {ok,CharSet} -> {CharMap, CharSet}
    end.

read_font_glyphs(F, Acc0) ->
    case read_line(F) of
	["CHARS",N0] ->
	    N = list_to_integer(N0),
	    try
		Acc = read_font_glyphs(F, N, Acc0),
		case read_line(F) of
		    ["ENDFONT"] -> Acc;
		    ["STARTCHAR"|_] ->
			io:format("CHARS declaration said there were ~p glyphs;"
				  " but there is at least one more glyph.\n",
				  [N]),
			exit(error);
		    _Other ->
			io:format("Garbage instead of ENDFONT after last glyph\n"),
			exit(error)
		end
	    catch
		throw:{endfont,Left} ->
		    io:format("There are only ~p glyphs in this font; "
			      "CHARS declaration said there were ~p glyphs.\n",
			      [N-Left,N]),
		    exit(error)
	    end;
	_ ->
	    read_font_glyphs(F, Acc0)
    end.

read_font_glyphs(_, 0, Acc) -> Acc;
read_font_glyphs(F, N, Acc0) ->
    case read_line(F) of
	["STARTCHAR"|_] ->
	    #glyph{code=C} = G = read_one_glyph(F, #glyph{}),
	     Acc = dict:store(C, G, Acc0),
	     read_font_glyphs(F, N-1, Acc);
	["ENDFONT"] ->
	    throw({endfont,N})
    end.

read_one_glyph(F, G) ->
    case read_line(F) of
	["ENCODING",Code0] ->
	    Code = list_to_integer(Code0),
	    read_one_glyph(F, G#glyph{code=Code});
	["DWIDTH"|Ints] ->
	    Dwidth = [list_to_integer(S) || S <- Ints],
	    read_one_glyph(F, G#glyph{dwidth=Dwidth});
	["BBX"|Ints] ->
	    BBx = [list_to_integer(S) || S <- Ints],
	    read_one_glyph(F, G#glyph{bbx=BBx});
	["SWIDTH"|_] ->
	    read_one_glyph(F, G);
	["BITMAP"] ->
	    Bitmap = read_bitmap(F, []),
	    G#glyph{bitmap=Bitmap}
    end.

read_bitmap(F, Acc0) ->
    case raw_read_line(F) of
	<<"ENDCHAR",_/bytes>> ->
	    list_to_binary(Acc0);
	<<Line0/bytes>> ->
	    [_|Line] = reverse(binary_to_list(Line0)),
	    Acc = read_bitmap_line(Line, Acc0),
	    read_bitmap(F, Acc)
    end.

read_bitmap_line([Hex1,Hex2|Line], Acc) ->
    Hex = erlang:list_to_integer([Hex2,Hex1], 16),
    read_bitmap_line(Line, [Hex|Acc]);
read_bitmap_line([], Acc) ->
    Acc.

to_unicode(Gs, Ps) ->
    case proplists:get_value("CHARSET_REGISTRY", Ps) of
	"ISO10646" ->				%Already in Unicode.
	    Gs;
	"ISO8859" ->
	    case proplists:get_value("CHARSET_ENCODING", Ps) of
		"1" -> Gs;
		"-"++Enc ->
		    to_unicode_1(Gs, "map-ISO8859-"++Enc);
		Enc ->
		    to_unicode_1(Gs, "map-ISO8859-"++Enc)
	    end;
	"ksx1001.1998" ->
	    to_unicode_1(Gs, "map-KSX1001");
	"KOI8" ->
	    to_unicode_1(Gs, "map-KOI8-R")
    end.

to_unicode_1(Gs, MapName) ->
    Map = read_map(MapName),
    dict:fold(fun
        (N, #glyph{code=C0}=G, Acc) ->
            case gb_trees:is_defined(C0, Map) of
              true ->
                C = gb_trees:get(C0, Map),
    %%% Throw away any Unicode characters falling in the 0x00 - 0xFF
    %%% (ISO-8859-1) range. They are already defined in the ISO-8859-1 font.
                case C =< 256 of
                  true -> Acc;
                  false -> dict:store(N, G#glyph{code=C}, Acc)
                end;
              false ->
                Acc
            end
    end, dict:new(), Gs).

read_map(MapName) ->
    {ok,F} = file:open(MapName, [binary,read,read_ahead,raw]),
    Map = read_map_1(F, []),
    file:close(F),
    Map.

read_map_1(F, Acc) ->
    case read_map_line(F) of
	eof ->
	    gb_trees:from_orddict(orddict:from_list(Acc));
	["0x"++From0,"0x"++To0|_] ->
	    {ok,[From],[]} = io_lib:fread("~16u", From0),
	    {ok,[To],[]} = io_lib:fread("~16u", To0),
	    read_map_1(F, [{From,To}|Acc])
    end.

error_msg(Term) ->
    throw({error,Term}).

read_map_line(F) ->
    case skip_whitespace(raw_read_line(F)) of
	eof -> eof;
	<<$#,_/bytes>> -> read_map_line(F);
	Cs -> collect_tokens(Cs)
    end.

read_line(F) ->
    case read_line_1(raw_read_line(F), F) of
	["COMMENT"|_] -> read_line(F);
	Line -> Line
    end.

read_line_1(eof, _) ->
    error_msg(eof);
read_line_1(<<>>, Fd) ->
    %% Blank line - ignore and read the next line.
    read_line(Fd);
read_line_1(<<Ctrl,Line/bytes>>, Fd) when Ctrl =< $\s ->
    %% Ignore any leading whitespace (especially TAB and spaces).
    read_line_1(Line, Fd);
read_line_1(Line, _) ->
    collect_tokens(Line).

collect_tokens(Line) ->
    collect_tokens_1(Line, [], []).

collect_tokens_1(<<C,T/bytes>>, [], Tokens) when C =< $\s ->
    collect_tokens_1(T, [], Tokens);
collect_tokens_1(<<C,T/bytes>>, Curr, Tokens) when C =< $\s ->
    collect_tokens_1(T, [], [reverse(Curr)|Tokens]);
collect_tokens_1(<<C,T/bytes>>, Curr, Tokens) ->
    collect_tokens_1(T, [C|Curr], Tokens);
collect_tokens_1(<<>>, [], Tokens) ->
    reverse(Tokens);
collect_tokens_1(<<>>, Curr, Tokens) ->
    collect_tokens_1(<<>>, [], [reverse(Curr)|Tokens]).

skip_whitespace(<<C,Cs/bytes>>) when C =< $\s ->
    skip_whitespace(Cs);
skip_whitespace([C|Cs]) when C =< $\s ->
    skip_whitespace(Cs);
skip_whitespace(Cs) -> Cs.

raw_read_line(F) ->
    case file:read_line(F) of
	eof -> eof;
	{ok,Line} -> Line
    end.

%%%
%%% Writing the font.
%%%

write_font(G, Out) ->
    {0,#glyph{bbx=[W,H,_,_]}} = lists:keyfind(0, 1, G),
    {Gl,Bit} = write_font_1(G, 0, [], []),
    Key = list_to_atom(filename:rootname(filename:basename(Out))),
    Desc = atom_to_list(Key),
    Font = {Key,Desc,W,H,Gl,Bit},
    Term = {wings_font,?FONT_VERSION,Font},
    Bin = term_to_binary(Term, [compressed]),
    file:write_file(Out, Bin).

write_font_1([{_,#glyph{code=C,bbx=BBx,dwidth=Dwidth,bitmap=B}}|Gs],
	     Offset, GlAcc, BiAcc) ->
    [W,H,Xorig,Yorig] = BBx,
    [Xmove,0] = Dwidth,
    G = {C,W,H,-Xorig,-Yorig,Xmove,Offset},
    write_font_1(Gs, Offset+size(B), [G|GlAcc], [B|BiAcc]);
write_font_1([], _, GlAcc, BiAcc) ->
    {reverse(GlAcc),list_to_binary(reverse(BiAcc))}.
