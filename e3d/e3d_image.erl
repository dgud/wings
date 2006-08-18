%%
%%  e3d_image.erl --
%%
%%     Handle images (2D) and different file formats.
%%
%%  Copyright (c) 2001-2004 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_image.erl,v 1.18 2005/06/20 20:22:01 dgud Exp $
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

%% internal exports
-export([noswap1/8,noswap3/8,noswap4/8,
	 noswap1to3/8,noswap1ato4/8,noswap1gto4/8,
	 noswap3to4/8,noswap4to3/8, 
	 swap3/8,swap4/8,swap3to4/8,swap4to3/8]).

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
load(FileName, Opts) when list(FileName), list(Opts) ->
    Extension = file_extension(FileName),
    Res = case ext_to_type(Extension) of
	      tga -> e3d__tga:load(FileName, Opts);
	      bmp -> e3d__bmp:load(FileName, Opts);
	      tif -> e3d__tif:load(FileName, Opts);
	      png -> e3d__png:load(FileName, Opts);
	      _ -> return_error({not_supported,Extension})
	  end,
    fix_outtype(Res, Opts).

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
convert(In, ToType) when atom(ToType) ->
    convert(In, ToType, In#e3d_image.alignment, In#e3d_image.order).

convert(In, ToType, NewAlignment) when atom(ToType) ->
    convert(In, ToType, NewAlignment, In#e3d_image.order).

convert(#e3d_image{type=Type, alignment=Al,order=O}=In, Type, Al, O) -> In;
convert(#e3d_image{type=FromType,image=Image,alignment=FromAlm,order=FromOrder}=In,
	ToType, ToAlm, ToOrder) ->

    OldRowLength  = In#e3d_image.width * In#e3d_image.bytes_pp,
    NewRowLength  = In#e3d_image.width * bytes_pp(ToType),

    OldPaddLength = pad_len(OldRowLength, FromAlm),
    NewPaddLength = pad_len(NewRowLength, ToAlm),
    NewPadd = lists:duplicate(NewPaddLength, 0),
    W = In#e3d_image.width,
    
    case type_conv(FromType, ToType) of
	{error, _Reason} = Err ->
	    Err;
	TypeConv ->
	    OrderConv = order_conv(FromOrder, ToOrder),
	    New = ?MODULE:TypeConv(0, W, Image, 
				   OldPaddLength, NewPadd, 
				   OrderConv, [], []),
	    In#e3d_image{image=New,type=ToType,
			 bytes_pp=bytes_pp(ToType), 
			 alignment=ToAlm,order=ToOrder}
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

fix_outtype(Res = #e3d_image{}, Opts) ->
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
    convert(Res, Type, Alignment, Order);
fix_outtype(Res, _) ->  %% Propagate Error Case
    Res.


%-define(C3(A,B,C), A,B,C).
%-define(C4(A,B,C,D), A,B,C,D). 
-define(C3(A,B,C), [A,B,C]).
-define(C4(A,B,C,D), [A,B,C,D]).   %% Seems faster if I make a binary of each row!!
%-define(C3(A,B,C), <<A:8,B:8,C:8>>).
%-define(C4(A,B,C,D), <<A:8,B:8,C:8,D:8>>).

swap(Action, W, W, Bin, OPL, NP, {SC,SR}, Row, Acc) ->
    <<_Skip:OPL/binary, Rest/binary>> = Bin,    
    NewRow = if 
		 SR == true ->
		     list_to_binary([NP|Row]);
		 SR == false ->
		     list_to_binary(lists:reverse([NP|Row]))
	     end,
    ?MODULE:Action(0, W, Rest, OPL, NP, {SC,SR}, [], [NewRow|Acc]);
swap(_, _, _, <<>>, _, _, {SC,_SR}, Row, Acc) ->
    NewImage = 
	if 
	    SC == true -> [Row|Acc];
	    SC == false -> lists:reverse(Acc, [Row])
	end,
    list_to_binary(NewImage).

noswap1(0, W, <<>>, OPL,NP,OC,[],Acc) ->
    swap(noswap1, 0, W, <<>>, OPL,NP,OC,[],Acc);
noswap1(0, W, Bin, OPL, NP, {_,SR}=OC, [], Acc) ->
    <<Row:W/binary, R/binary>> = Bin,
    case SR of 
	false -> swap(noswap1,W,W,R,OPL, NP, OC, [Row], Acc);
	true ->
	    RowList = lists:reverse(binary_to_list(Row)),
	    swap(noswap1,W,W,R,OPL, NP, OC, RowList, Acc)
    end.
%%% RGB or BGR 
noswap3(0, W, Bin, OPL, NP, {_,false}=OC, RowAcc, Acc) ->
    case Bin of
	<<Row:W/binary-unit:24,R/binary>> ->
	    swap(noswap3, W, W, R, OPL, NP, OC, [Row], Acc);
	_ ->
	    noswap3_1(0, W, Bin, OPL, NP, OC, RowAcc, Acc)
    end;
noswap3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    noswap3_1(C, W, Bin, OPL, NP, OC, Row, Acc).

noswap3_1(C, W, <<RGB:3/binary,R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap3_1(C+1, W, R, OPL, NP, OC, [RGB|Row], Acc);
noswap3_1(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap3, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Alpha8 or Grey8 to RGB
noswap1to3(C, W, <<G0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1to3(C+1, W, R, OPL, NP, OC, [?C3(G0,G0,G0)|Row], Acc);
noswap1to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1to3, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Alpha8 to RGBA
noswap1ato4(C, W, <<A0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1ato4(C+1, W, R, OPL, NP, OC, [?C4(255,255,255,A0)|Row], Acc);
noswap1ato4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1ato4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% Grey8 to RGB
noswap1gto4(C, W, <<G0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap1gto4(C+1, W, R, OPL, NP, OC, [?C4(G0,G0,G0, 255)|Row], Acc);
noswap1gto4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap1gto4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGB to RGBA 
noswap3to4(C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap3to4(C+1, W, R, OPL, NP, OC, [?C4(B0,G0,R0,255)|Row], Acc);
noswap3to4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap3to4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGBA 
noswap4(C, W, <<Col:4/binary, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap4(C+1, W, R, OPL, NP, OC, [Col|Row], Acc);
noswap4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap4, C, W, Bin, OPL, NP, OC, Row, Acc).
%% RGBA to RGB
noswap4to3(C, W, <<RGB:3/binary,_:8,R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    noswap4to3(C+1, W, R, OPL, NP, OC, [RGB|Row], Acc);
noswap4to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(noswap4to3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap3(0, W, Bin0, OPL, NP, OC, [], Acc) when size(Bin0) >= 3*W ->
    <<Row0:W/binary-unit:24,Bin/binary>> = Bin0,
    Row = swap3_row(binary_to_list(Row0), []),
    swap(swap3, W, W, Bin, OPL, NP, OC, Row, Acc);
swap3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap3_row([B,G,R|T], Acc) ->
    swap3_row(T, [[R,G,B]|Acc]);
swap3_row([], Acc) -> Acc.

swap4(0, W, Bin0, OPL, NP, OC, [], Acc) when size(Bin0) >= 4*W ->
    <<Row0:W/binary-unit:32,Bin/binary>> = Bin0,
    Row = swap4_row(binary_to_list(Row0), []),
    swap(swap4, W, W, Bin, OPL, NP, OC, Row, Acc);
swap4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap4, C, W, Bin, OPL, NP, OC, Row, Acc).

swap4_row([B,G,R,A|T], Acc) ->
    swap4_row(T, [[R,G,B,A]|Acc]);
swap4_row([], Acc) -> Acc.

swap4to3(C, W, <<B0:8,G0:8,R0:8,_:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W->
    swap4to3(C+1, W, R, OPL, NP, OC, [?C3(R0,G0,B0)|Row],  Acc);
swap4to3(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap4to3, C, W, Bin, OPL, NP, OC, Row, Acc).

swap3to4(C, W, <<B0:8,G0:8,R0:8, R/binary>>, OPL, NP, OC, Row, Acc) when C =/= W ->
    swap3to4( C+1, W, R, OPL, NP, OC, [?C4(R0,G0,B0,255)|Row], Acc);
swap3to4(C, W, Bin, OPL, NP, OC, Row, Acc) ->
    swap(swap3to4, C, W, Bin, OPL, NP, OC, Row, Acc).

%fix_alignment(<<>>, RL, OldP, NewP, Acc) ->
%    list_to_binary(lists:reverse(Acc));
%fix_alignment(Image, RL, OldP, NewP, Acc) ->
%    <<Row:RL/binary, Skip:OldP/binary, Rest/binary>> = Image,
%    fix_alignment(Rest, RL, OldP, NewP, [NewP, Row | Acc]).

type_conv(Type, Type) ->  %% No swap
    case bytes_pp(Type) of
	1 -> noswap1;
	3 -> noswap3;
	4 -> noswap4
    end;
type_conv(a8, r8g8b8) ->
    noswap1to3;
type_conv(g8, r8g8b8) ->
    noswap1to3;
type_conv(a8, r8g8b8a8) ->
    noswap1ato4;
type_conv(g8, r8g8b8a8) ->
    noswap1gto4;
type_conv(r8g8b8a8, r8g8b8) ->
    noswap4to3;
type_conv(b8g8r8a8, b8g8r8) ->
    noswap4to3;
type_conv(r8g8b8, r8g8b8a8) ->
    noswap3to4;
type_conv(b8g8r8, b8g8r8a8) ->
    noswap3to4;
type_conv(FromType, ToType) ->
    case {bytes_pp(FromType), bytes_pp(ToType)} of
	{3,3} -> swap3;
	{4,4} -> swap4;
	{3,4} -> swap3to4;
	{4,3} -> swap4to3;
	_ -> {error, {not_supported,conversion,FromType,ToType}}
    end.

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
