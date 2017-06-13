%%
%%  wings_glfont.erl --
%%
%%     Text and font support.
%%
%%  Copyright (c) 2009-2015 Dan Gudmundsson
%%
%%  Some ideas and code from Olivier Girondel
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%%
%%% @doc A api for generating textures and rendering strings via opengl.
%%%
%%% The 'load_font' functions loads and generates a texture from an
%%% #wxFont{}. The textures are white on black with an alpha channel.
%%% The default mode for the texture created is ?GL_MODULATE.
%%% That way you can use gl:color3x to specify the color of the text.
%%%
%%% This is a low level interface a library the intention is that
%%% you should be able to use it to build a text rendering interface
%%% above that changes fonts and colors and maybe even sizes.

-module(wings_glfont).

%% API
-export([load_font/1, load_font/2,
	 text_size/2, width/1, height/1, tex_id/1,
	 render/2,
	 render_to_binary/2, render_to_binary/3]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-record(glyph, {u, v, w, h}).
-record(font,  {wx, tex, glyphs, height, width, ih, iw}).

-define(F32, 32/float-native).
-define(SPACE_X, 3).  % Space between chars
-define(SPACE_Y, 2).  % Space between rows
-define(BIN_XTRA, (64+16)). %% Safe off-heap alloc


%%====================================================================
%% API
%%====================================================================

%-type font_info() :: opaque().

%% @spec(::wxFont()) -> font_info() | {error, Reason}
%% @equiv(load_font(WxFont, [])
load_font(WxFont) ->
    load_font(WxFont, []).

%% @spec(::wxFont(), [Options]) -> font_info() | {error, Reason}
%% Option = {range, [{CS1,CE1},...]} |
%%            {pots, false} | %% where POTS=PowerOfTwoTextureS
%%              {tex_mode,GL_MODE} |
%%              {tex_min, MinFilter} | {tex_mag, MagFilter}
%%              {tex_wrap_s, WrapS}  | {tex_wrap_s, WrapS}
%%              {tex_gen_mipmap, ?GL_TRUE|?GL_FALSE}
%% @desc Prepare a font to be used in a wxGLCANVAS.
%% Renders each character in the range into a texture.
%% Range is a list of unicode code points ranges to be used when
%% rendering, default [{32, 256}].
load_font(WxFont, Options) ->
    case wxFont:ok(WxFont) of
	true ->
	    GLFont = gen_glfont(WxFont, Options),
	    {ok, GLFont};
	false ->
	    {error, not_ok}
    end.

%% @spec(font_info()) -> {integer(), integer()}.
%% @desc Returns the size of a string (in scale 1.0).
text_size(#font{wx=Font}, String) ->
    MDC  = memory_dc(Font),
    Size = wxDC:getTextExtent(MDC, String),
    wxMemoryDC:destroy(MDC),
    Size.

%% @spec(font_info()) -> integer().
%% @desc Returns the height of characters and rows.
height(#font{height=Height}) ->
    Height.

%% @spec(font_info()) -> integer().
%% @desc Returns the Width of characters.
width(#font{width=W}) ->
    W.


%% @spec(font_info()) -> integer()
%% @desc Returns the texture id.
tex_id(#font{tex=TexId}) ->
    TexId.

%% @spec(font_info(), unicode:charlist()) -> ok.
%% @desc Directly renders the string.
render(#font{} = GLFont, String) when is_list(String) ->
    render_text(GLFont, String);
render(#font{}, {_, _, <<>>}) -> ok;
render(#font{tex=TexId, height=H}, {X,Y, Bin0}) ->
    Size = byte_size(Bin0),
    Bin = <<_:2/unit:32, TxBin/bytes>> =
	if Size < ?BIN_XTRA -> <<Bin0/bytes, 0:(?BIN_XTRA*8)>>;
	   true -> Bin0
	end,

    wx:retain_memory(Bin),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    gl:vertexPointer(2, ?GL_FLOAT, 16, Bin),
    gl:texCoordPointer(2, ?GL_FLOAT, 16, TxBin),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
    gl:drawArrays(?GL_QUADS, 0, Size div 16),
    gl:disableClientState(?GL_VERTEX_ARRAY),
    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY),
    wx:release_memory(Bin),
    {X,abs(Y-H)}.

%% @spec(Font::font_info(), unicode:charlist()) ->
%%       {W::integer(), H::integer(), binary()}.
%% @desc Renders the string to an interleaved vertex array.
%% @equiv render_to_binary(Font, String, {0,0,<<>>}).
render_to_binary(#font{glyphs=Gs, height=H, ih=IH, iw=IW}, String) ->
    render_text3(String, Gs, IH, IW, H, {0,0, <<>>}).

%% @spec(Font::font_info(), unicode:charlist(), {X0,Y0,Bin}) ->
%%       {X::integer(), Y::integer(), Bin::binary()}.
%% @desc Renders the string to an interleaved vertex array.
%% The render starts at position X0 and Y0 and appends to Bin,
%% and returns the next position.
%%
%% The binary can be rendered by the following code:
%%
%%     gl:bindTexture(?GL_TEXTURE_2D, wx_glfont:tex_id(Font)),</br>
%%     gl:vertexPointer(2, ?GL_FLOAT, 16, Bin), </br>
%%     gl:texCoordPointer(2, ?GL_FLOAT, 16, TxBin),</br>
%%     gl:enableClientState(?GL_VERTEX_ARRAY),</br>
%%     gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),</br>
%%     gl:drawArrays(?GL_QUADS, 0, (byte_size(Bin)-?BIN_XTRA) div 16),</br>
render_to_binary(#font{glyphs=Gs, height=H, ih=IH, iw=IW},
		 String, Data = {X,Y,D})
  when is_integer(X), is_integer(Y), is_binary(D) ->
    render_text3(String, Gs, IH, IW, H, Data).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
gen_glfont(Font, Options) ->
    Ranges0 = proplists:get_value(range, Options, [{32, 256}]),
    {NoChars, Chars0} = all_chars(Ranges0),
    {W, H, Chars} = get_char_info(Chars0, Font),
    {TW,TH0} = calc_tex_size(NoChars, W, H),
    {UsedHeight, {Bin, Glyphs0}} = make_glyphs(Font,Chars,H,TW,TH0),
    TH = tex_size(Options, UsedHeight),
    TexId = gen_texture(TW,TH,Bin,Options),
    %% debug(TW,TH,Bin),
    Glyphs = recalc_glyphs(Glyphs0, TH0, TH),

    Tab = ets:new(font_chars, [public]),
    ets:insert(Tab, array:sparse_to_orddict(Glyphs)),
    #font{wx=Font, tex=TexId, glyphs=Tab,
	  height=H, width=W, ih=1/TH, iw=1/TW}.

all_chars(Ranges) when is_list(Ranges) ->
    Chars0 = lists:foldl(fun({Min, Max}, Acc) when Min =< Max ->
				 [lists:seq(Min, Max)|Acc];
			    (Char, Acc) when is_integer(Char) ->
				 [Char|Acc]
			 end, [], Ranges),
    Chars = lists:usort(lists:flatten(Chars0)),
    {length(Chars), Chars}.

get_char_info(Chars0, Font) ->
    MDC = memory_dc(Font),
    Info = get_char_info(Chars0, MDC, Font, 0, 0, []),
    wxMemoryDC:destroy(MDC),
    Info.

get_char_info([Char|Cs], DC, Font, W0, H0, Acc) ->
    {W, H, _, _} = wxDC:getTextExtent(DC, [Char], [{theFont, Font}]),
    get_char_info(Cs, DC, Font, max(W,W0), max(H,H0), [{W,H,Char}|Acc]);
get_char_info([], _, _, W, H, Acc) ->
    {W, H, lists:keysort(2, Acc)}.

recalc_glyphs(Glyphs, TH, TH) -> Glyphs;
recalc_glyphs(Glyphs, Old, New) ->
    Recalc = fun(_, G=#glyph{v=V0}) -> G#glyph{v=V0*Old/New} end,
    array:sparse_map(Recalc, Glyphs).

make_glyphs(Font,Chars,H, TW,TH) ->
    MDC = memory_dc(Font),
    Bitmap = wxBitmap:new(TW, TH, [{depth,32}]),
    ok = wxMemoryDC:selectObject(MDC, Bitmap),

    BG = {0, 0, 0, 0},
    Brush = wxBrush:new(BG, [{style, ?wxSOLID}]),
    wxMemoryDC:setBackground(MDC, Brush),
    wxMemoryDC:clear(MDC),

    FG = {255, 255, 255, 255},
    wxMemoryDC:setTextForeground(MDC, FG),
    wxMemoryDC:setTextBackground(MDC, BG),
    {UsedHeight,Glyphs} = make_glyphs(MDC, Chars, 0, 0, H, TW, TH, array:new()),
    Image = wxBitmap:convertToImage(Bitmap),

    BinData = wxImage:getData(Image),
    Alpha = case wxImage:hasAlpha(Image) of
		true ->
		    %%io:format("A = ~p ~n", [wxImage:hasAlpha(Image)]),
		    wxImage:getAlpha(Image);
		false ->
		    false
	    end,

    wxBrush:destroy(Brush),
    wxImage:destroy(Image),
    wxBitmap:destroy(Bitmap),
    wxMemoryDC:destroy(MDC),
    {UsedHeight, greyscale(BinData, Alpha, Glyphs)}.

%% Minimize texture space, use greyscale images
greyscale(BinData, false, Glyphs) ->  %% Alpha use gray scale value
    Bin = << <<255:8, A:8>> || <<A:8,_:8,_:8>> <= BinData>>,
    {Bin, Glyphs};
greyscale(BinData, Alpha, Glyphs) ->
    {greyscale2(BinData, Alpha, <<>>), Glyphs}.

greyscale2(<<R:8,_:8,_:8, Cs/bytes>>, <<A:8, As/bytes>>, Acc) ->
    greyscale2(Cs, As, <<Acc/bytes, R:8, A:8>>);
greyscale2(<<>>, <<>>, Acc) ->
    Acc.

make_glyphs(DC, [Char|Chars], X, Y, H, TW, TH, Acc0) ->
    {Acc,Xp,Yp} = make_glyph(DC, Char, X, Y, H, TW, TH, Acc0),
    make_glyphs(DC, Chars, Xp, Yp, H, TW, TH, Acc);
make_glyphs(_DC, [], _X, Y, H, _TW, _TH, Acc) ->
    {Y+H+?SPACE_Y, Acc}.

make_glyph(DC, {Width, CharH, Char}, X0, Y0, Height, TW, TH, Acc0) ->
    Xt = X0+Width,
    case (Y0 + Height) =< TH of
	true -> %% Assert that we fit inside texture
	    case Xt > TW of
		true ->
		    X = Width,  Y = Y0+CharH+?SPACE_Y,
		    X1 = 0, Y1 = Y;
		false ->
		    X  = Xt,  Y = Y0,
		    X1 = X0, Y1 = Y0
	    end,
	    wxMemoryDC:drawText(DC, [Char], {X1, Y1}),
	    G = #glyph{w=Width, h=CharH, u=X1/TW, v=(Y1)/TH},
	    {array:set(Char, G, Acc0), X+?SPACE_X, Y};
	false ->
	    io:format("Tex ~p,~p to small Ignore Char ~p(~ts)~n",
		      [TW,TH,Char,[Char]]),
	    {Acc0, X0, Y0}
    end.


gen_texture(TW,TH,Bin,Options) ->
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),

    %% gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    Mode = proplists:get_value(tex_mode, Options, ?GL_MODULATE),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, Mode),

    MinF = proplists:get_value(tex_min, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,MinF),
    MagF = proplists:get_value(tex_mag, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,MagF),

    WS = proplists:get_value(tex_wrap_s, Options, ?GL_CLAMP),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S, WS),
    WT = proplists:get_value(tex_wrap_t, Options, ?GL_CLAMP),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T, WT),

    GEN_MM = proplists:get_value(tex_gen_mipmap, Options, ?GL_FALSE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, GEN_MM),
    if GEN_MM == ?GL_TRUE ->
	    gl:generateMipmap(?GL_TEXTURE_2D);
       true -> ignore
    end,

    %% io:format("HaveAlpha ~p ~n",[HaveAlpha]),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE8_ALPHA8,
                  TW, TH,  0, ?GL_LUMINANCE_ALPHA,
                  ?GL_UNSIGNED_BYTE, Bin),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    TexId.


memory_dc(Font) ->
    MDC = wxMemoryDC:new(),
    wxMemoryDC:setFont(MDC, Font),
    MDC.

render_text(Font=#font{glyphs=Gs, height=H, ih=IH, iw=IW}, String) ->
    Res = render_text3(String, Gs, IH, IW, H, {0,0, <<>>}),
    render(Font, Res).

render_text3([$\n|String], Gs, IH, IW, H, Data0) ->
    %% New line
    {_,H0,Bin} = Data0,
    render_text3(String, Gs, IH, IW, H, {0, H0+H, Bin});
render_text3([$\t|String], Gs, IH, IW, H, Data0) ->
    %% Tab
    Space = array:get(32, Gs),
    Data  = lists:foldl(fun(_, Data) ->
				render_glyph(Space,IW,IH,Data)
			end, Data0, "        "),
    render_text3(String, Gs, IH, IW, H, Data);
render_text3([Char|String], Gs, IH, IW, H, Data0) when is_integer(Char) ->
    case ets:lookup(Gs, Char) of
	[{_, #glyph{}=Glyph}] ->
	    Data = render_glyph(Glyph,IW,IH,Data0),
	    render_text3(String, Gs, IH, IW, H, Data);
	[] -> %% Should we render something strange here
	    render_text3(String, Gs, IH, IW, H, Data0)
    end;
render_text3([Other|String], Gs, IH, IW, H, Data0) ->
    Data = render_text3(Other, Gs, IH, IW, H, Data0),
    render_text3(String, Gs, IH, IW, H, Data);
render_text3(Bin, Gs, IH, IW, H, Data) when is_binary(Bin) ->
    render_text3(unicode:characters_to_list(Bin), Gs, IH, IW, H, Data);
render_text3([], _Gs, _IH, _IW, _H, {W,H0,Bin}) ->
    {W, H0, Bin}.

render_glyph(#glyph{u=U,v=V,w=W,h=H},IW,IH, {X0,Y0,Bin}) ->
    X1 = X0 + W,
    UD = U + W*IW,
    VD = V + H*IH,
    YH = Y0 - H,
    {X1,Y0,
     <<Bin/binary,         %% wxImage: 0,0 is upper left turn each
       X0:?F32,Y0:?F32, U:?F32, VD:?F32, % Vertex lower left, UV-coord up-left
       X1:?F32,Y0:?F32, UD:?F32,VD:?F32, % Vertex lower right,UV-coord up-right
       X1:?F32,YH:?F32, UD:?F32, V:?F32, % Vertex upper right,UV-coord down-right
       X0:?F32,YH:?F32, U:?F32,  V:?F32  % Vertex upper left, UV-coord down-left
     >>
    }.

%% Calculate texture size

tex_size(Options, UsedHeight) ->
    tex_size_1(proplists:get_value(pots, Options, false), UsedHeight).

tex_size_1(true, UsedHeight) -> UsedHeight;
tex_size_1(false, UsedHeight) ->
    case UsedHeight rem 4 of %% Make it rem 4 for compression
	0 -> UsedHeight;
	Diff -> UsedHeight + 4-Diff
    end.

calc_tex_size(No, CW, CH) ->
    %% Add some extra chars to be sure it fits.
    calc_tex_size(No+45, 1, No+45, CW, CH, {undefined, undefined}, undefined).

calc_tex_size(X, Y, No, CW, CH, Prev = {BestArea,Dec}, BestCoord)
  when Y =< No ->
    Xp = tsize(X*CW+X*?SPACE_X), %% + Empty space between chars
    Yp = tsize(Y*CH+Y*?SPACE_Y), %% + Empty space between rows
    Area = Xp * Yp,
    Square = abs(Xp - Yp),
    NextX = ((No-1) div (Y+1)) + 1,
    if Area < BestArea ->
	    %io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
	    calc_tex_size(NextX, Y+1, No, CW, CH, {Area,Square}, {Xp,Yp});
       Area == BestArea, Square < Dec ->
	    %io:format("Best is ~p ~p ~p ~n", [Area, Xp,Yp]),
	    calc_tex_size(NextX, Y+1, No, CW, CH, {Area,Square}, {Xp,Yp});
       true ->
	    calc_tex_size(NextX, Y+1, No, CW, CH, Prev, BestCoord)
    end;
calc_tex_size(_, _, _, _, _, _, BestCoord) ->
    BestCoord.

tsize(X0) ->
    Pow = trunc(log2(X0)),
    case (1 bsl Pow) of
	X0 -> X0;
	_ -> 1 bsl (Pow+1)
    end.

log2(X) ->
    math:log(X) / math:log(2).

%% debug(W,H, Bin0) ->
%%     Bin = << <<G:8, G:8, G:8>> || <<_:8, G:8>> <= Bin0>>,
%%     Image = wxImage:new(W,H,Bin),
%%     Title = io_lib:format("DEBUG ~px~p", [W,H]),
%%     Frame = wxFrame:new(wx:null(), ?wxID_ANY, Title, [{size, {W+40, H+40}}]),
%%     Panel = wxPanel:new(Frame),
%%     Paint = fun(_,_) ->
%% 		    DC=wxPaintDC:new(Panel),
%% 		    Bmp = wxBitmap:new(Image),
%% 		    wxDC:drawBitmap(DC, Bmp, {0,0}),
%% 		    wxPaintDC:destroy(DC),
%% 		    wxBitmap:destroy(Bmp)
%% 	    end,
%%     %% wxImage:destroy(Image),
%%     wxFrame:connect(Panel, paint, [{callback, Paint}]),
%%     wxFrame:show(Frame).
