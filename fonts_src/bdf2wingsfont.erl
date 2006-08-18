%%
%%  bdf2wingsfont.erl --
%%
%%     Conversion of BDF fonts to Wings' own font format.
%%
%%  Copyright (c) 2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: bdf2wingsfont.erl,v 1.5 2005/04/10 16:41:54 bjorng Exp $
%%

-module(bdf2wingsfont).
-export([convert/1]).

-import(lists, [reverse/1,sort/1,foldl/3]).

-record(glyph,
	{code,					%Unicode for glyph.
	 bbx,					%Bounding box.
	 dwidth,				%Width.
	 bitmap}).

convert([Out|SrcFonts]) ->
    G = read_fonts(SrcFonts, []),
    io:format("  Writing ~s (~p glyphs)\n", [Out,length(G)]),
    write_font(G, Out),
    init:stop().

read_fonts([N|Ns], Acc) ->
    io:format("Reading ~s\n", [N]),
    {ok,F} = file:open(N, [read,read_ahead]),
    G = read_font(F),
    file:close(F),
    read_fonts(Ns, G++Acc);
read_fonts([], Acc) ->
    sort(Acc).

read_font(F) ->
    case read_line(F) of
	["STARTFONT","2.1"] ->
	    Ps = read_props(F),
	    G = read_font_glyphs(F),
	    to_unicode(G, Ps);
	Other ->
	    io:format("~p\n", [Other]),
	    error(invalid_bdf_file)
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
    read_one_prop_1(io:get_line(F, ''), []).

read_one_prop_1([C|Cs], Key) when C =< $\s ->
    read_one_prop_2(Cs, reverse(Key));
read_one_prop_1([C|Cs], Key) ->
    read_one_prop_1(Cs, [C|Key]).

read_one_prop_2([C|Cs], Key) when C =< $\s ->
    read_one_prop_2(Cs, Key);
read_one_prop_2(Cs, Key) ->
    Val0 = reverse(skip_whitespace(reverse(Cs))),
    Val = convert_val(Val0),
    {Key,Val}.

convert_val("\""++Str0) ->
    "\""++Str = reverse(Str0),
    reverse(Str);
convert_val(Str) ->
    list_to_integer(Str).

read_font_glyphs(F) ->
    case read_line(F) of
	["CHARS",N0] ->
	    N = list_to_integer(N0),
	    try
		Gl = read_font_glyphs(F, N, []),
		case read_line(F) of
		    ["ENDFONT"] -> Gl;
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
	    read_font_glyphs(F)
    end.

read_font_glyphs(_, 0, Acc) -> Acc;
read_font_glyphs(F, N, Acc) ->
    case read_line(F) of
	["STARTCHAR"|_] ->
	    G = read_one_glyph(F),
	    read_font_glyphs(F, N-1, [G|Acc]);
	["ENDFONT"] ->
	    throw({endfont,N})
    end.

read_one_glyph(F) ->
    read_one_glyph_1(F, #glyph{}).

read_one_glyph_1(F, G) ->
    case read_line(F) of
	["ENCODING",Code0] ->
	    Code = list_to_integer(Code0),
	    read_one_glyph_1(F, G#glyph{code=Code});
	["DWIDTH"|Ints] ->
	    Dwidth = [list_to_integer(S) || S <- Ints],
	    read_one_glyph_1(F, G#glyph{dwidth=Dwidth});
	["BBX"|Ints] ->
	    BBx = [list_to_integer(S) || S <- Ints],
	    read_one_glyph_1(F, G#glyph{bbx=BBx});
	["SWIDTH"|_] ->
	    read_one_glyph_1(F, G);
	["BITMAP"] ->
	    Bitmap = read_bitmap(F, []),
	    G#glyph{bitmap=Bitmap}
    end.
    
read_bitmap(F, Acc) ->
    case read_line(F) of
	["ENDCHAR"] ->
	    list_to_binary(Acc);
	[Hex0] ->
	    {ok,[Hex],[]} = io_lib:fread("~16u", Hex0),
	    read_bitmap(F, [Hex|Acc])
    end.

to_unicode(Gs, Ps) ->
    case proplists:get_value("CHARSET_REGISTRY", Ps) of
	"ISO10646" ->				%Already in Unicode.
	    filter_unicode(Gs);
	"ISO8859" ->
	    case proplists:get_value("CHARSET_ENCODING", Ps) of
		"1" -> Gs;
		"-"++Enc ->
		    to_unicode_1(Gs, "map-ISO8859-"++Enc);
		Enc ->
		    to_unicode_1(Gs, "map-ISO8859-"++Enc)
	    end
    end.

to_unicode_1(Gs0, MapName) ->
    Map = read_map(MapName),
    Gs = [G#glyph{code=gb_trees:get(C, Map)} || #glyph{code=C}=G <- Gs0],

    %% Throw away any Unicode characters falling in the 0x00 - 0xFF
    %% (ISO-8859-1) range. They are already defined in the ISO-8859-1 font.
    [G || #glyph{code=C}=G <- Gs, C >= 256].

filter_unicode(Gs) ->
    MapFiles = filelib:wildcard("map-ISO8859-*"),
    io:put_chars("  Filtering Unicode font to only include characters in:"),
    Map = foldl(fun(F, A) ->
			"map-"++CharSet = filename:basename(F),
			io:format(" ~s", [CharSet]),
			gb_trees:to_list(read_map(F)) ++ A
		end, [], MapFiles),
    io:nl(),
    Needed0 = [To || {_From,To} <- Map],
    Needed = gb_sets:from_list(Needed0),
    [G || #glyph{code=C}=G <- Gs, gb_sets:is_member(C, Needed)].
    

read_map(MapName) ->
    {ok,F} = file:open(MapName, [read,read_ahead]),
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

error(Term) ->
    throw({error,Term}).

read_map_line(F) ->
    case skip_whitespace(io:get_line(F, '')) of
	eof -> eof;
	"#"++_ -> read_map_line(F);
	Cs -> collect_tokens(Cs)
    end.

read_line(F) ->
    case read_line_1(io:get_line(F, ''), F) of
	["COMMENT"|_] -> read_line(F);
	Line -> Line
    end.

read_line_1(eof, _) ->
    error(eof);
read_line_1([], Fd) ->
    %% Blank line - ignore and read the next line.
    read_line(Fd);
read_line_1([Ctrl|Line], Fd) when Ctrl =< $\s ->
    %% Ignore any leading whitespace (especially TAB and spaces).
    read_line_1(Line, Fd);
read_line_1(Line, _) ->
    collect_tokens(Line).

collect_tokens(Line) ->
    collect_tokens_1(Line, [], []).

collect_tokens_1([C|T], [], Tokens) when C =< $\s ->
    collect_tokens_1(T, [], Tokens);
collect_tokens_1([C|T], Curr, Tokens) when C =< $\s ->
    collect_tokens_1(T, [], [reverse(Curr)|Tokens]);
collect_tokens_1([H|T], Curr, Tokens) ->
    collect_tokens_1(T, [H|Curr], Tokens);
collect_tokens_1([], [], Tokens) ->
    reverse(Tokens);
collect_tokens_1([], Curr, Tokens) ->
    collect_tokens_1([], [], [reverse(Curr)|Tokens]).

skip_whitespace([C|Cs]) when C =< $\s ->    
    skip_whitespace(Cs);
skip_whitespace(Cs) -> Cs.

%%%
%%% Writing the font.
%%%

write_font(G, Out) ->
    #glyph{bbx=[W,H,_,_]} = findchar(0, G),
    {Gl,Bit} = write_font_1(G, 0, [], []),
    Key = list_to_atom(filename:rootname(filename:basename(Out))),
    Desc = atom_to_list(Key),
    Font = {Key,Desc,W,H,Gl,Bit},
    Term = {wings_font,?wings_version,Font},
    Bin = term_to_binary(Term, [compressed]),
    file:write_file(Out, Bin).

write_font_1([#glyph{code=C,bbx=BBx,dwidth=Dwidth,bitmap=B}|Gs],
	     Offset, GlAcc, BiAcc) ->
    [W,H,Xorig,Yorig] = BBx,
    [Xmove,0] = Dwidth,
    G = {C,W,H,-Xorig,-Yorig,Xmove,Offset},
    write_font_1(Gs, Offset+size(B), [G|GlAcc], [B|BiAcc]);
write_font_1([], _, GlAcc, BiAcc) ->
    {reverse(GlAcc),list_to_binary(reverse(BiAcc))}.

findchar(C, [#glyph{code=C}=G|_]) -> G;
findchar(C, [_|Gs]) -> findchar(C, Gs).
