%%
%%  e3d__bmp.erl --
%%
%%     Functions for reading and writing DIB BMP files.
%%
%%  Copyright (c) 2001-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d__bmp).
-export([load/2,save/3,save_bin/2]).
-export([format_error/1]).

-include("e3d_image.hrl").

-define(BITMAPFILEHEADER,  
	$B:8,$M:8, _BiFileSz:32/little, 0:16, 0:16, _BiOffset:32/little).
-define(BITMAPINFOHEADER,  
	_BiHeaderSz:32/little, _BiW:32/little, _BiH:32/little, 
	_BiPlanes:16/little, _BiBitCount:16/little, _BiCompression:32/little, 
	_BiSizeImage:32/little,
	_BiXPelsPerMeter:32/little,_BiYPelsPerMeter:32/little,
	_BiClrUsed:32/little, _BiClrImportant:32/little).

-define(BITMAPFILEHEADERSZ, 1+1+4+2+2+4).
-define(BITMAPINFOHEADERSZ, 4+4+4+2+2+4+4+4+4+4+4).

-define(DBGOUT(), io:format(" FileSz = ~p Offset = ~p BiHeaderSz = ~p ~n "
			    " BiW = ~p  BiH = ~p  BiPlanes = ~p ~n "
			    " BiBitCount = ~p  BiCompression = ~p ~n"
			    " BiSizeImage = ~p BiXPelsPerMeter = ~p , BiYPelsPerMeter = ~p~n"
			    " BiClrUsed = ~p , BiClrImportant = ~p ~n",
			    [_BiFileSz, _BiOffset,  _BiHeaderSz, _BiW, _BiH, _BiPlanes, _BiBitCount,_BiCompression,
			     _BiSizeImage, _BiXPelsPerMeter, _BiYPelsPerMeter,
			     _BiClrUsed, _BiClrImportant])).	    

format_error(unsupported_format) ->
    "Unsupported format or bad BMP file".

load(FileName, _Opts) ->
    %% Currently only supported format 
    case file:read_file(FileName) of	
	{ok, <<?BITMAPFILEHEADER, ?BITMAPINFOHEADER, TmpImage/binary>>} 
	when _BiPlanes == 1, _BiCompression == 0 -> % Supported formats
%%	    ?DBGOUT(),	    
	    MapSz =  if  % No map for 24 and 32 bit images
			 _BiBitCount > 16 ->   0;
			 true ->  (1 bsl _BiBitCount) * 4
		     end,
	    RbitLen = _BiW*_BiBitCount,
	    PadBits = e3d_image:pad_len(RbitLen, 32),
	    RowLength = (RbitLen + PadBits) div 8,
	    InSz = _BiH * RowLength,
	    <<MapBin:MapSz/binary, Image0:InSz/binary, _/binary>> = TmpImage,
	    Map = get_map(size(MapBin), 4, MapBin, []),
	    Orig = #e3d_image{width = _BiW, height = _BiH, 
			      type = b8g8r8,
			      bytes_pp = 3, 
			      alignment = 4},
	    case _BiBitCount of
		1 -> 
		    Image1 = convert1(size(Image0), Image0, Map, []),
		    PaddRow = _BiW*3+(PadBits div 1)*3,
		    Image  = strip(_BiH, Image1, _BiW*3, PaddRow, []),
		    Orig#e3d_image{image=Image,alignment=1};
		4 -> 
		    Image1 = convert4(size(Image0), Image0, Map, []),
		    PaddRow = _BiW*3+(PadBits div 4)*3,
		    Image  = strip(_BiH, Image1, _BiW*3, PaddRow, []),
		    Orig#e3d_image{image=Image,alignment=1};
		8 ->  
		    Image1 = convert8(size(Image0), Image0, Map, []),
		    PaddRow = _BiW*3+(PadBits div 8)*3,
		    Image  = strip(_BiH, Image1, _BiW*3, PaddRow, []),
		    Orig#e3d_image{image=Image,alignment=1};
		16 ->
		    Image1  = to_8bitpp(Image0),
		    PaddRow = _BiW*3+(PadBits div 16)*3,
		    Image   = strip(_BiH, Image1, _BiW*3, PaddRow, []),
		    Orig#e3d_image{image=Image,alignment=1};
		24 -> 
		    Orig#e3d_image{image=Image0};
		32 -> 
		    %% 32bit BMP don't contain alpha, only a trailing 0 at each pixel
		    Fake = Orig#e3d_image{image=Image0,type=b8g8r8a8,bytes_pp=3},
		    e3d_image:convert(Fake, b8g8r8, 1)
	    end;
	{ok,_Bin} ->
	    {error, {none,?MODULE,unsupported_format}};
	Error ->
	    Error
    end.

save(Image, Filename, _Opts) ->
    Bmp = save_1(Image),
    file:write_file(Filename, Bmp).

save_bin(Image, _Opts) ->
    Bmp = save_1(Image),
    {ok,list_to_binary(Bmp)}.

save_1(Image0) ->
    Image = e3d_image:convert(Image0, b8g8r8, 4, lower_left),
    _BiFileSz = ?BITMAPFILEHEADERSZ + ?BITMAPINFOHEADERSZ + size(Image#e3d_image.image),
    _BiOffset = ?BITMAPFILEHEADERSZ + ?BITMAPINFOHEADERSZ,
    _BiHeaderSz = ?BITMAPINFOHEADERSZ,
    _BiW = Image#e3d_image.width, 
    _BiH = Image#e3d_image.height, 
    _BiPlanes = 1, 
    _BiBitCount = 24, 
    _BiCompression = 0,
    _BiSizeImage = 0, %% If RGB set to 0
    _BiXPelsPerMeter = 0, _BiYPelsPerMeter = 0,
    _BiClrUsed = 0, _BiClrImportant = 0,
%    ?DBGOUT(),
%    io:format("Size ~p ~n", [size(Image#e3d_image.image)]),
    [<<?BITMAPFILEHEADER,?BITMAPINFOHEADER>>,Image#e3d_image.image].
     
% debug(I1, I2) ->
%     debug(I1#e3d_image.image, I2#e3d_image.image, 0).
    
% debug(<<R:8,G:8,B:8, R1/binary>>, <<R:8,G:8,B:8, R2/binary>>, N) ->
%     debug(R1,R2, N+1);
% debug(I1, I2, N) ->
%     io:format("Diff: in Pixel ~p ~n~P~n~P~n", [N, I1, 10, I2, 10]).

strip(0, _Image, _RowL, _PaddL, Acc) ->
    list_to_binary(Acc);
strip(Row, Image, RowL, PaddL, Acc) ->
    Skip = (Row-1)*PaddL,
    <<_:Skip/binary, RowBin:RowL/binary, _/binary>> = Image,
    strip(Row-1, Image, RowL, PaddL, [RowBin|Acc]).

-define(B8(C,B),  (trunc((C)/((1 bsl (B))-1) *255))).
to_8bitpp(Image) ->
    to_8bitpp(size(Image), Image, []).
to_8bitpp(0, _Image, Acc) ->
    list_to_binary(Acc);
to_8bitpp(N, Image, Acc) ->
    Prev = N - 2,
    <<_:Prev/binary, RGB:16/little, _/binary>> = Image,
    <<_:1,R:5,G:5,B:5>> = <<RGB:16/big>>,
    Pixel = [?B8(B,5),?B8(G,5),?B8(R,5)],
    to_8bitpp(Prev,Image,[Pixel|Acc]).

get_map(0,_, _, Acc) ->
    list_to_tuple(Acc);
get_map(I,B,Map,Acc) ->
    Prev = I-B,
    <<_:Prev/binary,C:3/binary,_/binary>> = Map,
    get_map(Prev,B,Map,[C|Acc]).

convert8(0,_, _, Acc) ->
    list_to_binary(Acc);
convert8(N, Image, Map, Acc) ->
    Prev = N-1,
    <<_:Prev/binary,Index:8,_/binary>> = Image,
    Col = element(Index+1, Map),
    convert8(Prev,Image, Map, [Col|Acc]).
convert4(0,_, _, Acc) ->
    list_to_binary(Acc);
convert4(N, Image, Map, Acc) ->
    Prev = N-1,
    <<_:Prev/binary,I1:4,I2:4,_/binary>> = Image,
    Col1 = element(I1+1, Map),
    Col2 = element(I2+1, Map),
    convert4(Prev,Image, Map, [Col1,Col2|Acc]).
convert1(0,_, _, Acc) ->
    list_to_binary(Acc);
convert1(N, Image, Map, Acc) ->
    Prev = N-1,
    <<_:Prev/binary,I1:1,I2:1,I3:1,I4:1,I5:1,I6:1,I7:1,I8:1,_/binary>> = Image,
    Col1 = element(I1+1, Map),
    Col2 = element(I2+1, Map),
    Col3 = element(I3+1, Map),
    Col4 = element(I4+1, Map),
    Col5 = element(I5+1, Map),
    Col6 = element(I6+1, Map),
    Col7 = element(I7+1, Map),
    Col8 = element(I8+1, Map),

    convert1(Prev,Image, Map, 
	     [Col1,Col2,Col3,Col4,Col5,Col6,Col7,Col8|Acc]).

