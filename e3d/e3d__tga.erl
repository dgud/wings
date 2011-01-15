%%
%%  e3d__tga.erl --
%%
%%     Functions for reading and writing TGA files.
%%
%%  Copyright (c) 2001-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d__tga).
-export([load/2,save/3,save_bin/2]).
-export([format_error/1]).
-include("e3d_image.hrl").

format_error(unsupported_format) ->
    "Unsupported format or bad TGA file";
format_error(bad_image_specifiction) ->
    "Bad image specification".

load(FileName, _Opts) ->
    case file:read_file(FileName) of
	%% Uncompressed image
	{ok, <<S,0,2,0,0,0,0,0,_,_,_,_, Image/binary>>} ->
	    load_uncomp(S,Image);
	%% Compressed image
	{ok, <<S,0,10,0,0,0,0,0,_,_,_,_, Image/binary>>} ->
	    load_comp(S,Image);
	%% Black&White Image 
	{ok, <<S,0,3,0,0,0,0,0,_,_,_,_, Image/binary>>} ->
	    load_uncomp(S,Image);
	%% Black&White Image Compressed
	{ok, <<S,0,11,0,0,0,0,0,_,_,_,_,Image/binary>>} ->
	    load_comp(S,Image);
	%% ColorMap Image (uncompressed)
	{ok, <<S,1,1,0:16/little,Len:16/little,B,_,_,_,_, Image/binary>>} ->
	    load_mapped(Len,B,false,S,Image);
 	%% ColorMap Image (compressed)
	{ok, <<S,1,9,0:16/little,Len:16/little,B,_,_,_,_, Image/binary>>} ->
	    load_mapped(Len,B,true,S,Image);
	{ok, <<_Bin:12/binary, _Rest/binary>>} ->
	    io:format("~p~n", [_Bin]),
	    {error, {none,?MODULE,unsupported_format}};
	Error ->
	    Error
    end.

load_mapped(Len,Bitpp,Comp,Skip,
	    <<W:16/little,H:16/little,8:8,0:1,0:1,
	     Order:2,Alpha:4,Raw/binary>>) ->
    Size = W * H,
    Bpp = (Bitpp + 1) div 8,
    MapSz = (Len*Bpp),
    <<_:Skip/binary,MapBin0:MapSz/binary,Image0/binary>> = Raw,
    case to_8bitpp(MapBin0,Bitpp,Alpha) of
	MapBin when is_binary(MapBin) ->
	    Map = get_map(MapSz,Bpp,MapBin,[]),
	    Image = case Comp of 
			false -> 
			    <<Image1:Size/binary, _/binary>> = Image0,
			    Image1;
			true ->
			    load_comp(Image0, Size, Bpp, [])
		    end,
	    RealImage = convert(Size,Image,Map,[]),
	    
	    Type = e3d_type(Bpp,Alpha),
	    #e3d_image{width = W, height = H, type = Type, 
		       order = get_order(Order),
		       bytes_pp = e3d_image:bytes_pp(Type), 
		       alignment = 1,
		       image = RealImage};
	Else ->
	    Else
    end.

load_uncomp(Skip,<<W:16/little,H:16/little,BitsPP:8,0:1,0:1,
		  Order:2,Alpha:4,Image/binary>>) ->
    BytesPerPixel = (BitsPP+1) div 8,
    Type = e3d_type(BytesPerPixel,Alpha),
    Size = BytesPerPixel * W * H,
    <<_:Skip/binary, RealImage:Size/binary, _Rest/binary>> = Image,
    case to_8bitpp(RealImage,BitsPP,Alpha) of
	Bin when is_binary(Bin) ->
	    #e3d_image{width = W, height = H, type = Type, 
		       order = get_order(Order),
		       bytes_pp = e3d_image:bytes_pp(Type), 
		       alignment = 1,
		       image = Bin};
	Else ->
	    Else
    end.

load_comp(Skip, Bin0) -> 
    <<W:16/little,H:16/little,BitsPP:8,0:1,0:1,
     Order:2,Alpha:4,_:Skip/binary,CImage/binary>> = Bin0,
    BytesPerPixel = (BitsPP+1) div 8,
    Type = e3d_type(BytesPerPixel,Alpha),
    Size  = W * H,
    Image = load_comp(CImage, Size, BytesPerPixel, []),
    case to_8bitpp(Image,BitsPP,Alpha) of
	Bin when is_binary(Bin) ->
	    #e3d_image{width = W, height = H, type = Type, 
		       order = get_order(Order),
		       bytes_pp = e3d_image:bytes_pp(Type),
		       alignment = 1,
		       image = Bin};
	Else ->
	    Else
    end.

load_comp(_, PL, _ByPP, Acc) when PL =< 0 ->
    list_to_binary(lists:reverse(Acc));

load_comp(<<0:1, Len:7, Image/binary>>, PLeft, ByPP, Acc) ->
    Bytes = (Len+1) * ByPP,
    <<Pixels:Bytes/binary, RestImage/binary>> = Image,
    load_comp(RestImage, PLeft-(Len+1), ByPP, [Pixels| Acc]);
load_comp(<<1:1, Len:7, RestImage0/binary>>, PLeft, ByPP, Acc) ->
    <<Pixel:ByPP/binary, RestImage/binary>> = RestImage0,
    Pixels = lists:duplicate(Len+1, Pixel),
    load_comp(RestImage, PLeft-(Len+1), ByPP, [Pixels| Acc]).

save(Image, Filename, _Opts) ->
    Tga = save_1(Image),
    file:write_file(Filename, Tga).

save_bin(Image, _Opts) ->
    Tga = save_1(Image),
    {ok,list_to_binary(Tga)}.

save_1(#e3d_image{bytes_pp=Bpp,type=Type}=Image0) ->
    Order = get_order(Image0#e3d_image.order),
    {TC,Image,BitsPP,Def} = 
	if 
	    Bpp == 1, Type == g8 ->
		{3,e3d_image:convert(Image0, g8, 1), 8, <<Order:4, 0:4>> };
	    Bpp == 1, Type == a8 ->
		{3,e3d_image:convert(Image0, a8, 1), 8, <<Order:4, 8:4>> };
	    Bpp == 3 ->
		{2,e3d_image:convert(Image0, b8g8r8, 1), 24, <<Order:4, 0:4>>};
	    Bpp == 4 ->
		{2,e3d_image:convert(Image0, b8g8r8a8, 1), 32, <<Order:4, 8:4>>}
	end,
    [<<0,0,TC,0,0,0,0,0,0,0,0,0,
      (Image#e3d_image.width):16/little, 
      (Image#e3d_image.height):16/little,
      BitsPP:8>>,Def|Image#e3d_image.image].

get_order(lower_left) -> 0;
get_order(lower_right) ->1;
get_order(upper_left) -> 2;
get_order(upper_right) ->3;
get_order(0) -> lower_left;
get_order(1) -> lower_right;
get_order(2) -> upper_left;
get_order(3) -> upper_right.

e3d_type(Bpp,Alpha) ->
    case Bpp of
	1 when Alpha == 8 -> a8;
	1 -> g8;
	2 when Alpha == 0 ->  b8g8r8;
	2 when Alpha > 0 ->   b8g8r8a8;
	3 -> b8g8r8;
	4 when Alpha == 0 -> b8g8r8;
	4 -> b8g8r8a8
    end.

-define(B8(C,B),  (trunc((C)/((1 bsl (B))-1) *255))).
to_8bitpp(Image, 8, _)  -> Image;
to_8bitpp(Image, 24, _) -> Image;
to_8bitpp(Image, 32, 8) -> Image;
to_8bitpp(Image, 32, 0) -> 
    to_8bitpp(size(Image), Image, 4,8,8,8,0,8,[]);
to_8bitpp(Image, Bitpp, Alpha) ->
    ByPP = (Bitpp + 1) div 8,
    Type =
	if Alpha == 0, Bitpp == 15 -> {5,5,5,0,1};
	   Alpha == 0, Bitpp == 16 -> {5,6,5,0,0};
	   (Bitpp rem 8) == 0, ((Bitpp-Alpha) rem 3) == 0 -> % Assert
		BpC = (Bitpp - Alpha) div 3,
		{BpC,BpC,BpC, Alpha, 0};
	   true -> {error, unsupported_format}
	end,
    case Type of
	{R,G,B,A,S} ->
	    to_8bitpp(size(Image), Image, ByPP,R,G,B,A,S,[]);
	Else ->
	    Else
    end.
to_8bitpp(0, _Image, _ByPP,_R,_G,_B,_A,_S, Acc) ->
    list_to_binary(Acc);
to_8bitpp(N, Image, ByPP = 2,Rs,Gs,Bs,As,Ss, Acc) ->
    Prev = N - ByPP,
    <<_:Prev/binary, RGB:16/little, _/binary>> = Image,
    <<_A:As,_:Ss,R:Rs,G:Gs,B:Bs>> = <<RGB:16/big>>,
    %% Alpha don't seem to be valid for 16bit images
    %% atleast not the ones created with photoshop.
    A = (1 bsl As) - 1, 
    Pixel = if As == 0 -> [?B8(B,Bs),?B8(G,Gs),?B8(R,Rs)];
	       As > 0 ->  [?B8(B,Bs),?B8(G,Gs),?B8(R,Rs),?B8(A,As)]
	    end,
    to_8bitpp(Prev,Image,ByPP,Rs,Gs,Bs,As,Ss,[Pixel|Acc]);
to_8bitpp(N, Image, ByPP,Rs,Gs,Bs,0,Ss, Acc) ->
    Prev = N - ByPP,
    <<_:Prev/binary, R:Rs,G:Gs,B:Bs,_:Ss, _/binary>> = Image,
    Pixel = [?B8(R,Rs),?B8(G,Gs),?B8(B,Bs)],
    to_8bitpp(Prev,Image,ByPP,Rs,Gs,Bs,0,Ss,[Pixel|Acc]);
to_8bitpp(N, Image, ByPP,Rs,Gs,Bs,As,Ss, Acc) ->
    Prev = N - ByPP,
    <<_:Prev/binary, A:As,R:Rs,G:Gs,B:Bs, _/binary>> = Image,
    Pixel = [?B8(R,Rs),?B8(G,Gs),?B8(B,Bs),?B8(A,As)],
    to_8bitpp(Prev,Image,ByPP,Rs,Gs,Bs,As,Ss,[Pixel|Acc]).

get_map(0,_, _, Acc) ->
    list_to_tuple(Acc);
get_map(I,B,Map,Acc) ->
    Prev = I-B,
    <<_:Prev/binary,C:B/binary,_/binary>> = Map,
    get_map(Prev,B,Map,[C|Acc]).

convert(0,_, _, Acc) ->
    list_to_binary(Acc);
convert(I, Image, Map, Acc) ->
    Prev = I-1,
    <<_:Prev/binary,Index:8,_/binary>> = Image,
    Col = element(Index+1, Map),
    convert(Prev,Image, Map, [Col|Acc]).
