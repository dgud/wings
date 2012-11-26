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
	 bytes_pp/1, pad_len/2, format_error/1]).

%% Normal map handing
-export([height2normal/2,
	 height2normal/3,
	 buildNormalMipmaps/1]).

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
	_ -> return_error({not_supported,Extension})
    end.

%% Func: save_bin(#e3d_image, Extension [, Opts])
%% Rets: {ok,Binary} | {error, Reason}
%% Desc: Saves image to file. The Extension gives the
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

%% Func: convert(#e3d_image, NewType [,NewAlignment [,NewOrder ]])
%% Rets: #e3d_image | {error, Reason}
%% Desc: Converts an image to new type optionally NewAlignment and NewOrder
convert(In, ToType) when is_atom(ToType) ->
    convert(In, ToType, In#e3d_image.alignment, In#e3d_image.order).

convert(In, ToType, NewAlignment) when is_atom(ToType) ->
    convert(In, ToType, NewAlignment, In#e3d_image.order).

convert(#e3d_image{type=Type, alignment=Al,order=O}=In, Type, Al, O) ->
    In;
convert(#e3d_image{type=FromType, order=FromOrder}=In, ToType, ToAlm, ToOrder) ->
    try
	Convert = col_conv(FromType, ToType),
	Reverse = order_conv(FromOrder, ToOrder),
	New = map_rows(In, Convert, Reverse, ToType, ToAlm),
	In#e3d_image{image=New,type=ToType,
		     bytes_pp=bytes_pp(ToType),
		     alignment=ToAlm,order=ToOrder}
    catch Error ->
	    Error
    end.

ext_to_type(".tga") -> tga;
ext_to_type(".bmp") -> bmp;
ext_to_type(".png") -> png;
ext_to_type(".tif") -> tif;
ext_to_type(".tiff") -> tif;
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
bytes_pp(r8g8b8) -> 3;
bytes_pp(b8g8r8) -> 3;
bytes_pp(r8g8b8a8) -> 4;
bytes_pp(b8g8r8a8) -> 4;
bytes_pp(#e3d_image{bytes_pp = Bpp}) ->
    Bpp.


%% Func: height2normal(Image, Scale, GenMipMap)  
%% Args: Image = #e3d_image, Scale = number, GenMipMap == Bool
%% Rets: {#e3d_image,[{MM_Lev,W,H,Bin}]}  | {error, Reason}
%% Desc: Filter and build a normalmap from a heightmap.
%%       assumes the heightmap is greyscale.
height2normal(Image, Scale, GenMipMap) ->
    NM  = height2normal(Image, Scale),
    MMs = case GenMipMap of
	      true -> buildNormalMipmaps(NM);
	      false -> []
	  end,
    {NM, MMs}.

%% Func: height2normal(Image, Scale)  
%% Args: Image = #e3d_image, Scale = number
%% Rets: #e3d_image | {error, Reason}
%% Desc: Filter and build a normalmap from a heightmap.
%%       assumes the heightmap is greyscale.
height2normal(Old = #e3d_image{width=W,bytes_pp=B,alignment=A,image=I,name=Name}, Scale) ->
    Extra = (A - (W*B rem A)) rem A, 
    RSz  = W*B + Extra,
    {Row1,Row2,Rest} = 
	case I of 
	    <<R1:RSz/binary,R2:RSz/binary,Rt/binary>> ->
		{R1,R2,Rt};
	    <<R1:RSz/binary, _/binary>> ->
		{R1,<<>>,<<>>}
	end,
    New = bumps(Row1,Row2,Rest,RSz,B,Row1,Scale,[]),
    Old#e3d_image{bytes_pp=3,type=r8g8b8, image=New, alignment=1,
		  filename=none, name=Name++"bump"}.
	
%bumps(Row1,Row2,Rest,RSz,B,First,Acc) ->
bumps(R1,R2,Rest,RSz,B,First,S,Bump) 
  when size(Rest) < RSz, size(R2) < RSz ->
    <<F:8,_/binary>> = R1,
    Row = bumpmapRow(R1,First,F,B,S,[]),
    list_to_binary(lists:reverse([Row|Bump]));
bumps(R1,R2,Rest0,RSz,B,First,S,Bump) ->
    <<F:8,_/binary>> = R1,
    Row = bumpmapRow(R1,R2,F,B,S,[]),
    case Rest0 of 
	<<R3:RSz/binary,Rest/binary>> ->
	    bumps(R2,R3,Rest,RSz,B,First,S,[Row|Bump]);
	_ ->
	    bumps(R2,<<>>,<<>>,RSz,B,First,S,[Row|Bump])
    end.

bumpmapRow(R1,<<>>,_,B,_,Br) when size(R1) < B ->
    list_to_binary(lists:reverse(Br));
bumpmapRow(R1,R2,F,B,Scale,BR) ->
    Skip = (B-1)*8,
    <<C0:8, _:Skip,Row1/binary>> = R1,
    <<Cy0:8,_:Skip,Row2/binary>> = R2,
    ToFloat = 1.0/255.0,
    C = C0*ToFloat,
    Cx = case Row1 of
	     <<Cx0:8, _/binary>> ->
		 Cx0*ToFloat;
	     <<>> ->
		 F*ToFloat
	 end,
    Cy = Cy0*ToFloat,
    DCX = Scale * (C-Cx),
    DCY = Scale * (C-Cy),
    %% Normalize
    Sqlen = DCX*DCX+DCY*DCY+1,
    Recip = 1.0/math:sqrt(Sqlen),
    Nx = DCY*Recip,
    Ny = -DCX*Recip,
    Nz = Recip,
    %% Pack in RGB
    RGB = [round(128.0+127.0*Nx),
	   round(128.0+127.0*Ny),
	   round(128.0+127.0*Nz)],
    bumpmapRow(Row1,Row2,F,B,Scale,[RGB|BR]).

%  buildNormalMipmaps(Image) -> [{Level,W,H,Bin}]
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
	    [{Level,HW,HH,Down} | buildNormalMipmaps(Level+1, HW,HH,Down)];
       true ->
	    [{Level,HW,HH,Down}]
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

map_rows(#e3d_image{image=Bin, alignment=1}, keep, {false,false}, _, 1) ->
    Bin;
map_rows(#e3d_image{image=Bin, alignment=1}, Convert, {false,false}, _, 1) ->
    Convert(Bin); %% No need to handle rows specifically
map_rows(In=#e3d_image{image=Bin, alignment=Alm, width=W, bytes_pp=BPP}, Convert,
	 {RRs, RCs}, ToType, ToAlm) ->
    RowL = W*BPP,
    FoldRow = row_conv(Convert, bytes_pp(ToType), RCs, make_pad(In, ToType, ToAlm)),
    map_bin(Bin, RowL, pad_len(RowL, Alm), FoldRow, RRs).

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

make_pad(#e3d_image{width=W}, ToType, ToAlm) ->
    NewRowLength  = W * bytes_pp(ToType),
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
