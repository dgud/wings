%%
%%  e3d_image.erl --
%%
%%     Handle images (2D) and different file formats.
%%
%%  Copyright (c) 2001-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d_image).

-include("e3d_image.hrl").

%% Basic functionality

-export([load/1, load/2, 
	 convert/2, convert/3, convert/4, 
	 save/2, save/3, save_bin/2, save_bin/3,
         add_alpha/2,
         channel/2, expand_channel/2, replace_channel/3, invert_channel/2,
	 bytes_pp/1, pad_len/2, format_error/1,
	 fix_outtype/3
	]).

%% Normal map handing
-export([height2normal/2,
	 height2normal/3,
	 buildNormalMipmaps/1]).

-compile({inline, [gray/3]}).
-define(SINGLE_CHANNEL(T), (T =:= g8 orelse T =:= a8 orelse T =:= g32)).

%% Func: load(FileName[, Options])  
%% Args: FileName = [Chars], Options = [Tagged Tuple]
%% Rets: #e3d_image | {error, Reason}
%% Desc: Loads an image file.
%%       Default loads image with type/alignment/order set to what is 
%%       stored in the file.
%%       Conversion between fileformats type/alignment/order can be done with 
%%       Options {type, Type} and/or {alignment, N} and/or {order, O} see e3d_image.hrl
load(FileName) ->
    load(FileName, []).
load(FileName, Opts) when is_list(FileName), is_list(Opts) ->
    Extension = file_extension(FileName),
    Res = case ext_to_type(Extension) of
	      tga -> e3d__tga:load(FileName, Opts);
	      bmp -> e3d__bmp:load(FileName, Opts);
	      tif -> e3d__tif:load(FileName, Opts);
	      png -> e3d__png:load(FileName, Opts);
              dds -> e3d__dds:load(FileName, Opts);
	      _ -> return_error({not_supported,Extension})
	  end,
    fix_outtype(FileName, Res, Opts).

%% Func: save(#e3d_image, Filename [, Opts])
%% Rets: ok | {error, Reason}
%% Desc: Saves image to file. Using extension to 
%%       know which fileformat to use. 
%%       Opts is a list of options. 
%%       Available options: compress 
%%        compress - compresses the file if it is possible/implemented (currently tif).
save(Image, Filename) ->
    save(Image, Filename, []).
save(Image = #e3d_image{}, Filename, Opts) ->
    Extension = file_extension(Filename),
    case ext_to_type(Extension) of
	tga -> e3d__tga:save(Image, Filename, Opts);
	bmp -> e3d__bmp:save(Image, Filename, Opts);
	tif -> e3d__tif:save(Image, Filename, Opts);
	png -> e3d__png:save(Image, Filename, Opts);
        dds -> e3d__dds:save(Image, Filename, Opts);
	_ -> return_error({not_supported,Extension})
    end.

%% Func: save_bin(#e3d_image, Extension [, Opts])
%% Rets: {ok,Binary} | {error, Reason}
%% Desc: Saves image to binary. The Extension gives the
%%       the file format to use.
%%       Opts is a list of options.
%%       Available options: compress
%%        compress - compresses the file if it is possible/implemented (currently tif).
save_bin(Image, Extension) ->
    save_bin(Image, Extension, []).
save_bin(#e3d_image{}=Image, Extension, Opts) ->
    case ext_to_type(Extension) of
	tga -> e3d__tga:save_bin(Image, Opts);
	bmp -> e3d__bmp:save_bin(Image, Opts);
	tif -> e3d__tif:save_bin(Image, Opts);
	png -> e3d__png:save_bin(Image, Opts);
	_ -> return_error({not_supported,Extension})
    end.

format_error({not_supported,Extension}) ->
    io_lib:format("Files of type ~s are not supported", [Extension]);
format_error(Other) ->
    file:format_error(Other).

%% Func: add_alpha(#e3d_image, Alpha : binary())
%% Rets: #e3d_image | {error, Reason}
%% Desc: Merges an alpha channel to rgb images
add_alpha(I = #e3d_image{type=Type,bytes_pp=3,alignment=1,image=Data0}, Alpha) ->
    Data = add_alpha(Data0, Alpha, <<>>),
    I#e3d_image{bytes_pp=4, type=with_alpha(Type), image=Data}.
%%    I#e3d_image{bytes_pp=1, type=g8, image=Alpha}.

add_alpha(<<Col:24, Data/binary>>, <<A:8, Alpha/binary>>, Acc) ->
    add_alpha(Data,Alpha, <<Acc/binary, Col:24, A:8>>);
add_alpha(<<>>, <<>>, Acc) -> Acc.

with_alpha(r8g8b8) -> r8g8b8a8;
with_alpha(b8g8r8) -> b8g8r8a8.


%% Func: channel(r|g|b|a, #e3d_image{})
%% Rets: #e3d_image
%% Desc: Make a gray image from a channel from Indata image

-spec channel(r|g|b|a, #e3d_image{}) -> #e3d_image{}.
channel(_Ch, #e3d_image{type=Single}=Img) when ?SINGLE_CHANNEL(Single) ->
    Img;
channel(r, #e3d_image{type=r8g8b8a8, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<C:8,_:24>> <= In>>,
    channel_img(Ch, "_r", Img);
channel(r, #e3d_image{type=r8g8b8, bytes_pp=3, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<C:8,_:16>> <= In>>,
    channel_img(Ch, "_r", Img);
channel(g, #e3d_image{type=_, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:8,C:8,_:16>> <= In>>,
    channel_img(Ch, "_g", Img);
channel(g, #e3d_image{type=_, bytes_pp=3, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:8,C:8,_:8>> <= In>>,
    channel_img(Ch, "_g", Img);
channel(b, #e3d_image{type=r8g8b8a8, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:16,C:8,_:8>> <= In>>,
    channel_img(Ch, "_b", Img);
channel(b, #e3d_image{type=r8g8b8, bytes_pp=3, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:16,C:8>> <= In>>,
    channel_img(Ch, "_b", Img);
channel(a, #e3d_image{type=_, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:24,C:8>> <= In>>,
    channel_img(Ch, "_a", Img);
%% b8g8r8a8..
channel(b, #e3d_image{type=b8g8r8a8, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<C:8,_:24>> <= In>>,
    channel_img(Ch, "_b", Img);
channel(b, #e3d_image{type=b8g8r8, bytes_pp=3, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<C:8,_:16>> <= In>>,
    channel_img(Ch, "_b", Img);
channel(r, #e3d_image{type=b8g8r8a8, bytes_pp=4, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:16,C:8,_:8>> <= In>>,
    channel_img(Ch, "_r", Img);
channel(r, #e3d_image{type=b8g8r8, bytes_pp=3, alignment=1, image=In}=Img) ->
    Ch = << <<C:8>> || <<_:16,C:8>> <= In>>,
    channel_img(Ch, "_r", Img).

channel_img(Data, CN, #e3d_image{name=N, extra=X}=Img) ->
    Name = filename:rootname(N) ++ CN,
    Img#e3d_image{type=g8, bytes_pp=1,
                  name=Name, filename=none,
                  extra=lists:keydelete(mipmaps, 1, X),
                  image=Data
                 }.

-spec expand_channel(Ch::r|g|b|a, #e3d_image{}) -> #e3d_image{}.
%% Desc: Create a RGBA image from gray image Indata inserted to channel Ch
%%       the other channels will be white
expand_channel(r, #e3d_image{type=g8, image=In}=Img) ->
    RGBA = << <<C:8, -1:24>> || <<C:8>> <= In>>,
    Img#e3d_image{type=r8g8b8a8, bytes_pp=4, name=[], filename=none, extra=[], image=RGBA};
expand_channel(g, #e3d_image{type=g8, image=In}=Img) ->
    RGBA = << <<-1:8, C:8, -1:16>> || <<C:8>> <= In>>,
    Img#e3d_image{type=r8g8b8a8, bytes_pp=4, name=[], filename=none, extra=[], image=RGBA};
expand_channel(b, #e3d_image{type=g8, image=In}=Img) ->
    RGBA = << <<-1:16, C:8, -1:8>> || <<C:8>> <= In>>,
    Img#e3d_image{type=r8g8b8a8, bytes_pp=4, name=[], filename=none, extra=[], image=RGBA};
expand_channel(a, #e3d_image{type=g8, image=In}=Img) ->
    RGBA = << <<-1:24, C:8>> || <<C:8>> <= In>>,
    Img#e3d_image{type=r8g8b8a8, bytes_pp=4, name=[], filename=none, extra=[], image=RGBA}.


-spec replace_channel(Which::r|g|b|a, ChG8::#e3d_image{}, Orig::#e3d_image{})  -> #e3d_image{}.
%% Desc replace 'Which' channel in 'Orig' with data from 'ChG8' image
replace_channel(r, #e3d_image{type=g8,image=Ch}, #e3d_image{type=r8g8b8a8,image=Orig,extra=X}=Img) ->
    Img#e3d_image{image=replace_r(Ch, Orig, <<>>), extra=lists:keydelete(mipmaps, 1, X)};
replace_channel(g, #e3d_image{type=g8,image=Ch}, #e3d_image{type=r8g8b8a8,image=Orig,extra=X}=Img) ->
    Img#e3d_image{image=replace_g(Ch, Orig, <<>>), extra=lists:keydelete(mipmaps, 1, X)};
replace_channel(b, #e3d_image{type=g8,image=Ch}, #e3d_image{type=r8g8b8a8,image=Orig,extra=X}=Img) ->
    Img#e3d_image{image=replace_b(Ch, Orig, <<>>), extra=lists:keydelete(mipmaps, 1, X)};
replace_channel(a, #e3d_image{type=g8,image=Ch}, #e3d_image{type=r8g8b8a8,image=Orig,extra=X}=Img) ->
    Img#e3d_image{image=replace_a(Ch, Orig, <<>>), extra=lists:keydelete(mipmaps, 1, X)}.

replace_r(<<C:8, Ch/binary>>, <<_:8, GBA:24, Orig/binary>>, Acc) ->
    replace_r(Ch, Orig, <<Acc/binary, C:8, GBA:24>>);
replace_r(<<>>, <<>>, Acc) -> Acc.

replace_g(<<C:8, Ch/binary>>, <<R:8, _:8, BA:16, Orig/binary>>, Acc) ->
    replace_g(Ch, Orig, <<Acc/binary, R:8, C:8, BA:16>>);
replace_g(<<>>, <<>>, Acc) -> Acc.

replace_b(<<C:8, Ch/binary>>, <<RG:16, _:8, A:8, Orig/binary>>, Acc) ->
    replace_b(Ch, Orig, <<Acc/binary, RG:16, C:8, A:8>>);
replace_b(<<>>, <<>>, Acc) -> Acc.

replace_a(<<C:8, Ch/binary>>, <<RGB:24, _:8, Orig/binary>>, Acc) ->
    replace_a(Ch, Orig, <<Acc/binary, RGB:24, C:8>>);
replace_a(<<>>, <<>>, Acc) -> Acc.


-spec invert_channel(Ch::r|g|b|a, #e3d_image{}) -> #e3d_image{}.
%% Desc: Create a copy of image with Ch channel inverted
invert_channel(_, #e3d_image{type=G8orA8, image=Image}=Img)
  when G8orA8 =:= g8; G8orA8 =:= a8 ->
    Bin = << << (255-C):8 >> || <<C:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(r, #e3d_image{type=r8g8b8, image=Image}=Img) ->
    Bin = << << (255-C):8, P:16 >> || <<C:8, P:16>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(g, #e3d_image{type=Type, image=Image}=Img)
  when Type =:= r8g8b8; Type =:= b8g8r8 ->
    Bin = << << Pre:8, (255-C):8, P:8 >> || <<Pre:8, C:8, P:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(b, #e3d_image{type=r8g8b8, image=Image}=Img) ->
    Bin = << << P:16, (255-C):8 >> || <<P:16, C:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(r, #e3d_image{type=r8g8b8a8, image=Image}=Img) ->
    Bin = << << (255-C):8, P:24 >> || <<C:8, P:24>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(g, #e3d_image{type=Type, image=Image}=Img)
  when Type =:= r8g8b8a8; Type =:= b8g8r8a8 ->
    Bin = << << Pre:8, (255-C):8, P:16 >> || <<Pre:8, C:8, P:16>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(b, #e3d_image{type=r8g8b8a8, image=Image}=Img) ->
    Bin = << << Pre:16, (255-C):8, P:8 >> || <<Pre:16, C:8, P:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(a, #e3d_image{type=WA, image=Image}=Img)
  when WA =:= r8g8b8a8; WA =:= b8g8r8a8 ->
    Bin = << << Pre:16, (255-C):8, P:8 >> || <<Pre:16, C:8, P:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(b, #e3d_image{type=b8g8r8, image=Image}=Img) ->
    Bin = << << (255-C):8, P:16 >> || <<C:8, P:16>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(r, #e3d_image{type=b8g8r8, image=Image}=Img) ->
    Bin = << << P:16, (255-C):8 >> || <<P:16, C:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(b, #e3d_image{type=b8g8r8a8, image=Image}=Img) ->
    Bin = << << (255-C):8, P:24 >> || <<C:8, P:24>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none};
invert_channel(r, #e3d_image{type=b8g8r8a8, image=Image}=Img) ->
    Bin = << << Pre:16, (255-C):8, P:8 >> || <<Pre:16, C:8, P:8>> <= Image >>,
    Img#e3d_image{image=Bin, filename=none}.

%% Func: convert(#e3d_image, NewType [,NewAlignment [,NewOrder ]])
%% Rets: #e3d_image | {error, Reason}
%% Desc: Converts an image to new type optionally NewAlignment and NewOrder
convert(In, ToType) when is_atom(ToType) ->
    convert(In, ToType, In#e3d_image.alignment, In#e3d_image.order).

convert(In, ToType, NewAlignment) when is_atom(ToType) ->
    convert(In, ToType, NewAlignment, In#e3d_image.order).

convert(#e3d_image{type=Type, alignment=Al,order=O}=In, Type, Al, O) ->
    In;
convert(#e3d_image{width=W, bytes_pp=Bpp, alignment=Al,
                   type=FromType, order=FromOrder, image=Bin, extra=Extra0}=In,
        ToType, ToAlm, ToOrder) ->
    try
	Convert = col_conv(FromType, ToType),
        ToBpp = if Convert =:= keep -> Bpp; true -> bytes_pp(ToType) end,
	Reverse = order_conv(FromOrder, ToOrder),
	New = map_rows(Bin, W, Bpp, Al, Convert, Reverse, ToBpp, ToAlm),
        DoConvert = fun(EBin, Width) ->
                            map_rows(EBin, Width, Bpp, Al, Convert, Reverse, ToBpp, ToAlm)
                    end,
        Extra = convert_extra(DoConvert, W, Extra0),
	In#e3d_image{image=New,type=ToType,
		     bytes_pp=ToBpp,
		     alignment=ToAlm,order=ToOrder, extra=Extra}
    catch Error ->
	    Error
    end.

convert_extra(Convert, W, Extra0) ->
    MMs0 = proplists:get_value(mipmaps, Extra0, undefined),
    CMs0 = proplists:get_value(cubemaps, Extra0, undefined),
    Extra1 = case MMs0 of
                 undefined -> Extra0;
                 _ ->
                     E1 = proplists:delete(mipmaps, Extra0),
                     MMs = convert_mipmaps(Convert, MMs0),
                     [{mipmaps, MMs}|E1]
             end,
    case CMs0 of
        undefined -> Extra1;
        _ ->
            E2 = proplists:delete(cubemaps, Extra1),
            CMs = convert_cubemaps(Convert, W, CMs0),
            [{cubemaps, CMs}|E2]
    end.

convert_mipmaps(Convert, MMs) ->
    [{Convert(Img, W), W, H, Level} || {Img,W,H,Level} <- MMs].

convert_cubemaps(Convert, W, CMs) ->
    [CM#{tx:=Convert(Img, W), mipmaps:=convert_mipmaps(Convert, MMs)}
     || CM = #{tx:=Img, mipmaps:=MMs} <- CMs].

ext_to_type(".tga") -> tga;
ext_to_type(".bmp") -> bmp;
ext_to_type(".png") -> png;
ext_to_type(".tif") -> tif;
ext_to_type(".tiff") -> tif;
ext_to_type(".dds") -> dds;
ext_to_type(_) -> unknown.

%% Func: pad_len(RowLength (in bytes), Alignment)
%% Rets: integer()
%% Desc: Get the number of bytes each row is padded with
pad_len(RL, Align) ->
    case RL rem Align of
	0 -> 0;
	Rem -> Align - Rem
    end.

%% Func: bytes_pp(Type)
%% Rets: integer()
%% Desc: Get the number of bytes per pixel for type Type
bytes_pp(a8) -> 1;
bytes_pp(g8) -> 1;
bytes_pp(r8g8) -> 2;
bytes_pp(r8g8b8) -> 3;
bytes_pp(b8g8r8) -> 3;
bytes_pp(r8g8b8a8) -> 4;
bytes_pp(b8g8r8a8) -> 4;
bytes_pp(#e3d_image{bytes_pp = Bpp}) ->
    Bpp.


%% Func: height2normal(Image, Scale, GenMipMap)
%% Args: Image = #e3d_image, Scale = number, GenMipMap == Bool
%% Rets: #e3d_image{}  | {error, Reason}
%% Desc: Filter and build a normalmap from a heightmap.
%%       assumes the heightmap is greyscale.
height2normal(Image, Options, GenMipMap) ->
    NM  = height2normal(Image, Options),
    case GenMipMap of
        true  -> NM#e3d_image{extra=[{mipmaps,buildNormalMipmaps(NM)}]};
        false -> NM
    end.

%% Func: height2normal(Image, Params)
%% Args: Image = #e3d_image, Params = #{scale::float(), inv_x::boolean(), inv_y::boolean()}
%% Rets: #e3d_image | {error, Reason}
%% Desc: Filter and build a normalmap from a heightmap.
%%       the heightmap can be a greyscale or colored image.
height2normal(Old, Opts) ->
    InvX = maps:get(inv_x, Opts, false),
    InvY = maps:get(inv_y, Opts, false),
    Scale  = maps:get(scale, Opts, 4.0),
    ScaleXY = {Scale*inv_multiply(InvX), Scale*inv_multiply(InvY)},
    #e3d_image{width=W,height=H,image=I,name=Name} = e3d_image:convert(Old, g8, 1),
    New = bumps(H, W, I, ScaleXY),
    Old#e3d_image{bytes_pp=3,type=r8g8b8, image=New, alignment=1,
		  filename=none, name=filename:rootname(Name)++"_normal"}.

bumps(Rows, Cols, Bin, Scale) ->
    Offset = (Rows-2)*Cols,
    <<RowFirst:Cols/binary, Bin0/binary>> = Bin,
    <<_:Offset/binary,RowLast/binary>> = Bin0,
    bumps_0(Cols, <<RowLast/binary,Bin/binary,RowFirst/binary>>, Scale, <<>>).

bumps_0(Cols, Bin0, _, Acc) when size(Bin0) =:= Cols*2 -> Acc;
bumps_0(Cols, Bin0, Scale, Acc) ->
    Offset = Cols -2,
    <<RowUp:Cols/binary,Bin/binary>> = Bin0,
    <<Row:Cols/binary,RowDown:Cols/binary,_/binary>> = Bin,
    <<Ci,_:Offset/binary,Cf>> = Row,
    Acc0 = bumpmapRow(<<Cf,Row/binary,Ci>>, RowUp, RowDown, Scale, <<>>),
    bumps_0(Cols, Bin, Scale, <<Acc/binary,Acc0/binary>>).

bumpmapRow(<<Cl,Row/binary>>, <<Ru,RowUp/binary>>, <<Rd,RowDown/binary>>, Scale, Acc) ->
    <<_, Cr, _/binary>> = Row,
    {R,G,B} = bumpmapRGB(Cl, Cr, Ru, Rd, Scale),
    bumpmapRow(Row, RowUp, RowDown, Scale, <<Acc/binary, R:8, G:8, B:8>>);
bumpmapRow(_, <<>>, <<>>, _, NormalRow) -> NormalRow.

bumpmapRGB(Cl, Cr, Ru, Rd, {MulX,MulY}) ->
    Z1 = (Cr-Cl)*-1.0*MulX,
    Z2 = (Ru-Rd)*MulY,
    {Nr,Ng,Nb} = e3d_vec:norm({Z1, Z2, 255.0}),
    {R0,G0,B0} = {(Nr+1)*0.5,(Ng+1)*0.5,(Nb+1)*0.5},
    {round(R0 *255), round(G0 *255), round(B0 *255)}.

inv_multiply(true) -> -1.0;
inv_multiply(_) -> 1.0.

%  buildNormalMipmaps(Image) -> [{Bin,W,H,Level}]
%  Generates all mipmap levels from an Normalmap
%% Perfect for
%% gl:texImage2D(?GL_TEXTURE_2D, Level, ?GL_RGB8, HW, HH, 0,
%%		    ?GL_RGB, ?GL_UNSIGNED_BYTE, Down);

buildNormalMipmaps(#e3d_image{width=W,height=H,image=Bin,
			      alignment=1, type=r8g8b8}) ->
    buildNormalMipmaps(1, W, H, Bin);
buildNormalMipmaps(Image) ->
    buildNormalMipmaps(convert(Image,r8g8b8,1)).
buildNormalMipmaps(Level, W, H, Bin) ->
    %% Half width and height but not beyond one.
    HW = case W div 2 of Tw when Tw == 0 -> 1; Tw -> Tw end,
    HH = case H div 2 of Th when Th == 0 -> 1; Th -> Th end,
    Down = downSampleNormalMap(W,H,Bin),
    if (HW>1) or (HH>1) ->
	    [{Down,HW,HH,Level} | buildNormalMipmaps(Level+1, HW,HH,Down)];
       true ->
	    [{Down,HW,HH,Level}]
    end.

-define(N2RGB(XX), round(128.0+127.0*(XX))).
downSampleNormalMap(W,H,Bin) ->
    I = if H == 1 -> 0; true -> H-2 end,    
    J = if W == 1 -> 0; true -> W-2 end,
    downSampleNormalMap(I,J,W,H,Bin,[]).

downSampleNormalMap(I,_J,_W,_H,_Bin,Acc) when I < 0 ->
    list_to_binary(Acc);
downSampleNormalMap(I,J0,W,H,Bin,Acc) when J0 < 0 ->
    J = if W == 1 -> 0; true -> W-2 end,
    downSampleNormalMap(I-2,J,W,H,Bin,Acc);
downSampleNormalMap(I,J,W,H,Bin,Acc) ->
    OneOver127 = 1.0/127.0,
    %% OneOver255 = 1.0/255.0,

    %% The "%w2" and "%h2" modulo arithmetic makes sure that
    %% Nx1 and 1xN normal map levels are handled correctly.

    %% Fetch the four vectors (and magnitude) to be downsampled. 

    %% Don't have 32bits normal-maps for yet..
    
    %% M0=OneOver255*M00, M1=OneOver255*M10,
    %% M2=OneOver255*M20, M3=OneOver255*M30,

    P0 = (I*W+J)*3,
    P1 = ((I*W+((J+1) rem W))*3),
    P2 = ((((I+1) rem H)*W+J)*3),
    P3 = ((((I+1) rem H)*W+((J+1) rem W))*3),
    <<_:P0/binary, X0,Y0,Z0, _/binary>> = Bin,
    <<_:P1/binary, X1,Y1,Z1, _/binary>> = Bin,
    <<_:P2/binary, X2,Y2,Z2, _/binary>> = Bin,
    <<_:P3/binary, X3,Y3,Z3, _/binary>> = Bin,

    M0=1.0,M1=1.0,M2=1.0,M3=1.0,

    %% Sum 2x2 footprint of red component scaled 
    %% back to [-1,1] floating point range. 
    X = M0*(OneOver127*float(X0)-1.0)+M1*(OneOver127*float(X1)-1.0)+
	M2*(OneOver127*float(X2)-1.0)+M3*(OneOver127*float(X3)-1.0),

    %%  Sum 2x2 footprint of green component scaled back to [-1,1] 
    %%  floating point range. 
    Y = M0*(OneOver127*float(Y0)-1.0)+M1*(OneOver127*float(Y1)-1.0)+
	M2*(OneOver127*float(Y2)-1.0)+M3*(OneOver127*float(Y3)-1.0),

    %% Sum 2x2 footprint of blue component scaled back to [-1,1]
    %% floating point range. 
    Z = M0*(OneOver127*float(Z0)-1.0)+M1*(OneOver127*float(Z1)-1.0)+
	M2*(OneOver127*float(Z2)-1.0)+M3*(OneOver127*float(Z3)-1.0),

    L = math:sqrt(X*X+Y*Y+Z*Z),
    if L > 0.00005 ->
%	    SL = L / 4,
% 	    M = if SL > 1.0 -> 255;
% 		   true -> 255*SL
% 		end,
	    %% Normalize the vector to unit length and convert to RGB
	    InvL = 1.0/L,
	    New = [?N2RGB(X*InvL),?N2RGB(Y*InvL),?N2RGB(Z*InvL)], %M],
	    downSampleNormalMap(I,J-2,W,H,Bin, [New|Acc]);
       true ->
	    %% Ugh, a zero length vector.  Avoid division by zero and just
	    %% use the unpeturbed normal (0,0,1). 
	    New = [128,128,255], %,0], 
	    downSampleNormalMap(I,J-2,W,H,Bin, [New|Acc])
    end.

%% Helpers 
file_extension(FileName) ->
    lowercase(filename:extension(FileName)).

lowercase([H|R]) when H >= $A, H =< $Z ->
    [H + $a - $A | lowercase(R)];
lowercase([H|R]) ->
    [H | lowercase(R)];
lowercase([]) ->
    [].

fix_outtype(File, Res = #e3d_image{}, Opts) ->
    Type =
	case lists:keysearch(type, 1, Opts) of
	    {value, {type, T}} -> T;
	    false -> Res#e3d_image.type
	end,
    Alignment =
	case lists:keysearch(alignment, 1, Opts) of
	    {value, {alignment, A}} -> A;
	    false -> Res#e3d_image.alignment
	end,
    Order =
	case lists:keysearch(order, 1, Opts) of
	    {value, {order, O}} -> O;
	    false -> Res#e3d_image.order
	end,
    convert(Res#e3d_image{filename=File}, Type, Alignment, Order);
fix_outtype(_, Res, _) ->  %% Propagate Error Case
    Res.

map_rows(Bin, _, _, 1, keep, {false,false}, _, 1) ->
    Bin;
map_rows(Bin, _, _, 1, Convert, {false,false}, _, 1) ->
    Convert(Bin); %% No need to handle rows specifically
map_rows(Bin, W, BPP, FromAlm, Convert, {RRs, RCs}, ToBpp, ToAlm) ->
    RowL = W*BPP,
    FoldRow = row_conv(Convert, ToBpp, RCs, make_pad(W, ToBpp, ToAlm)),
    map_bin(Bin, RowL, pad_len(RowL, FromAlm), FoldRow, RRs).

col_conv(Type, Type) -> keep;
col_conv(g8, Type) ->
    case bytes_pp(Type) of
	1 -> keep;
	3 -> fun(Bin) -> << <<G:8,G:8,G:8>> || <<G:8>> <= Bin >> end;
	4 -> fun(Bin) -> << <<G:8,G:8,G:8,255:8>> || <<G:8>>  <= Bin >> end
    end;
col_conv(a8, Type) ->
    case bytes_pp(Type) of
	1 -> keep;
	3 -> fun(Bin) -> << <<G:8,G:8,G:8>> || <<G:8>> <= Bin >> end;
	4 -> fun(Bin) -> << <<255:8,255:8,255:8,A:8>> || <<A:8>> <= Bin >> end
    end;

col_conv(r8g8b8, r8g8b8a8) -> fun(Bin) -> << <<RGB:24,255:8>> || <<RGB:24>> <= Bin >> end;
col_conv(r8g8b8a8, r8g8b8) -> fun(Bin) -> << <<RGB:24>> || <<RGB:24,_:8>> <= Bin >> end;
col_conv(b8g8r8, b8g8r8a8) -> fun(Bin) -> << <<RGB:24,255:8>> || <<RGB:24>> <= Bin >> end;
col_conv(b8g8r8a8, b8g8r8) -> fun(Bin) -> << <<RGB:24>> || <<RGB:24,_:8>> <= Bin >> end;
col_conv(r8g8b8, g8)       -> fun(Bin) -> << <<(gray(R,G,B)):8>> || <<R:8,G:8,B:8>> <= Bin >> end;
col_conv(r8g8b8a8, g8)     -> fun(Bin) -> << <<(gray(R,G,B)):8>> || <<R:8,G:8,B:8,_:8>> <= Bin >> end;
col_conv(b8g8r8, g8)       -> fun(Bin) -> << <<(gray(R,G,B)):8>> || <<B:8,G:8,R:8>> <= Bin >> end;
col_conv(b8g8r8a8, g8)     -> fun(Bin) -> << <<(gray(R,G,B)):8>> || <<B:8,G:8,R:8,_:8>> <= Bin >> end;

col_conv(In, Out) ->
    InSz  = bytes_pp(In),
    OutSz = bytes_pp(Out),
    if InSz =:= OutSz, InSz =:= 3 ->
	    fun(Bin) -> << <<R:8,G:8,B:8>> || <<B:8,G:8,R:8>> <= Bin >> end;
       InSz =:= OutSz, InSz =:= 4 ->
	    fun(Bin) -> << <<R:8,G:8,B:8,A:8>> || <<B:8,G:8,R:8,A:8>> <= Bin >> end;
       InSz =:= 3, OutSz =:= 4 ->
	    fun(Bin) -> << <<R:8,G:8,B:8,255:8>> || <<B:8,G:8,R:8>> <= Bin >> end;
       InSz =:= 4, OutSz =:= 3 ->
	    fun(Bin) -> << <<R:8,G:8,B:8>> || <<B:8,G:8,R:8, _:8>> <= Bin >> end;
       InSz =:= 4, Out =:= a8  ->
	    fun(Bin) -> << <<A:8>> || <<_:8,_:8,_:8, A:8>> <= Bin >> end;
       InSz =:= 3, Out =:= a8 -> col_conv(In, g8);
       true ->
	    {error, {not_supported,conversion,In,Out}}
    end.

gray(G,G,G) -> G;
gray(R,G,B) when B>G,B>R ->
    (R+G+B) div 3; %% The formula below becomes very dark for "blueish" images
gray(R,G,B) ->
    round(0.2126*R + 0.7152*G + 0.0722*B).

%% row_conv(ColorConvert, ReverseColumns, RowPad)
row_conv(keep, Sz, Reverse, Pad) ->  %% Keep rows add padding
    fun(Row) ->  reverse_and_pad(Row, Sz, Reverse, Pad) end;
row_conv(Convert, Sz, Reverse, Pad) when is_function(Convert, 1) ->
    fun(InRow) ->
	    reverse_and_pad(Convert(InRow),Sz,Reverse,Pad)
    end.

reverse_and_pad(Bin, _, false, <<>>) -> Bin;
reverse_and_pad(Bin, _, false, Pad)  -> <<Bin/binary, Pad/binary>>;
reverse_and_pad(Bin, 1, true, Pad) -> reverse_and_pad(Bin, 1, false, Pad);
reverse_and_pad(Bin0, Sz, true, Pad) ->
    %% Unoptimized but should happen really rare..
    Bin = bin_reverse(Bin0, Sz, 0, fun(C) -> C end, []),
    <<Bin/binary, Pad/binary>>.

map_bin(InBin, InSz, InSkip, Convert, Reverse) ->
    case Reverse of
	false ->
	    << <<(Convert(Cut))/binary>> || <<Cut:InSz/binary, _:InSkip/binary>> <= InBin >>;
	true ->
	    bin_reverse(InBin, InSz, InSkip, Convert, [])
    end.

bin_reverse(<<>>, _InSz, _InSkip, _Convert, Acc) -> list_to_binary(Acc);
bin_reverse(Bin, InSz, InSkip, Convert, Acc) ->
    <<Col:InSz/binary, _:InSkip/binary, Rest/binary>> = Bin,
    bin_reverse(Rest, InSz, InSkip, Convert, [Convert(Col)|Acc]).

make_pad(W, ToBpp, ToAlm) ->
    NewRowLength  = W * ToBpp,
    NewPadLength = 8*pad_len(NewRowLength, ToAlm),
    <<0:NewPadLength>>.

order_conv(Order, Order) -> {false, false};   %% {SwapColumns, SwapRows}
order_conv(lower_left, upper_right) -> {true,true};
order_conv(upper_right, lower_left) -> {true,true};
order_conv(lower_right, upper_left) -> {true,true};
order_conv(upper_left, lower_right) -> {true,true};

order_conv(lower_left, upper_left) ->  {true,false};
order_conv(upper_right, lower_right) ->{true,false};
order_conv(lower_right, upper_right) ->{true,false};
order_conv(upper_left, lower_left) ->  {true,false};

order_conv(lower_left,  lower_right) -> {false,true};
order_conv(upper_right, upper_left) ->  {false,true};
order_conv(lower_right, lower_left) ->  {false,true};
order_conv(upper_left,  upper_right) -> {false,true}.

return_error(Reason) ->
    {error, {none, ?MODULE, Reason}}.
