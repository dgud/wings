%%
%%  e3d__tif.erl --
%%
%%     Functions for reading and writing TIF files.
%%
%%  Copyright (c) 2001-2011Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d__tif).
-export([load/2,save/3,save_bin/2]).
-export([format_error/1]).
-include("e3d_image.hrl").

format_error(unsupported_format) ->
    "Unsupported format or bad TIFF file";
format_error({none,?MODULE,{tif_decode,_}}) ->
    "Decoding error";
format_error({unsupported_compression,Comp}) ->
    io_lib:format("Unsupported compression type (~p)", [Comp]).

load(FileName, _Opts) ->
    case catch load1(FileName,_Opts) of
	{'EXIT', Reason} ->
	    {error, {?MODULE,Reason}};
	Else -> 
	    Else
    end.
    
load1(FileName, _Opts) ->
    case file:read_file(FileName) of
	{ok, Orig = <<16#4949:16, 42:16/little, IFDOffset:32/little, _Rest/binary>>} ->
%%	    io:format("Little tiff file~n", []),
	    IFDs = getIFDs(little, IFDOffset, Orig),	    
	    load_image(little, hd(IFDs), Orig);
	{ok, Orig = <<16#4D4D:16, 42:16, IFDOffset:32/big, _Rest/binary>>} ->
	    IFDs = getIFDs(big, IFDOffset, Orig),
%	    io:format("Big tiff file ~p ofs: ~p ~n ~p ~n", 
%		      [size(Orig), IFDOffset, IFDs]),
	    load_image(big, hd(IFDs), Orig);
	{ok, _Bin} ->
	    {error, {none,?MODULE,unsupported_format}};
	Error ->
	    Error
    end.

save(Image, Filename, Opts) ->
    do_save(Image, Filename, Opts).

save_bin(Image, Opts) ->
    do_save(Image, binary, Opts).

do_save(Image, Name, Opts) ->
    Self = self(),
    Saver = spawn(fun() -> save(Self, Image, Name, Opts) end),
    receive 
	{Saver,save,Res} ->
	    Res
    end.

save(Father, Image, Name, Opts) ->
    Father ! {self(),save,(catch save_1(Image, Name, Opts))}.

save_1(Image0, Name, Opts) ->
    if 
	Image0#e3d_image.bytes_pp == 4 ->
	    Image = e3d_image:convert(Image0, r8g8b8a8, 1, upper_left);
	true ->	    
	    Image = e3d_image:convert(Image0, r8g8b8, 1, upper_left)
    end,
    Compress = lists:member(compress, Opts),    
    {Where,BinList} = save_image(Image, Compress, 8),
    Tiff = [<<16#4D4D:16,42:16,Where:32/big>>|BinList],
    if
	Name == binary ->
	    {ok,list_to_binary(Tiff)};
	true ->
	    file:write_file(Name, Tiff)
    end.

%% Some really ugly macros to get different formats..
%% !!OBS!! Binds variables or Matches them !!OBS!!
-define(BIG_DIRENTRIES, _:IFDOffset/binary, DirEntries:16/big).
-define(LITTLE_DIRENTRIES, _:IFDOffset/binary, DirEntries:16/little).
-define(BIG_ENTRIESB_NEXT, EntriesB:EntSz/binary, NextIFD:32/big).
-define(LITTLE_ENTRIESB_NEXT, EntriesB:EntSz/binary, NextIFD:32/little).

getIFDs(_T, 0, _Orig) -> [];  %% Previous was last IFD
getIFDs(T, IFDOffset, Orig) ->
    if 
	T == big ->    <<?BIG_DIRENTRIES, Temp/binary>> = Orig;
	T == little -> <<?LITTLE_DIRENTRIES, Temp/binary>> = Orig
    end,               
    EntSz = (12*DirEntries),        
    if                     
	T == big ->     <<?BIG_ENTRIESB_NEXT, _Skip2/binary>> = Temp;
	T == little ->  <<?LITTLE_ENTRIESB_NEXT, _Skip2/binary>> = Temp
    end,                   
    Entries = getDirEntries(T, DirEntries, EntriesB),
    [Entries|getIFDs(T, NextIFD, Orig)].

-define(BIG_GETDIRENTRIES, Tag:16/big, IType:16/big, Count:32/big, ValOrOfBin:4/binary).
-define(LITTLE_GETDIRENTRIES, Tag:16/little, IType:16/little, Count:32/little, ValOrOfBin:4/binary).

getDirEntries(_, 0, <<>>)   ->    [];
getDirEntries(T, N, Bin) ->
    if                 
        T == big ->     <<?BIG_GETDIRENTRIES, Next/binary>> = Bin;
        T == little ->  <<?LITTLE_GETDIRENTRIES, Next/binary>> = Bin
    end,
    
    if 
	%% Ignore Private fields
	Tag >= 32768 -> 
%%	    io:format("Skipped private tag: ~w~n", [Tag]),
	    getDirEntries(T, N-1, Next);
	true -> %% Not a private field read value or get pointer
	    Type = type2type(IType),
	    Length = typeSz(Type) * Count,
	    ValOrOffset = 
		if 
		    Length =< 4 ->
			Value = getdata(T, Type, Count, ValOrOfBin),
%			io:format("Tag ~p, ~p~n", 
%				  [{Tag,Type, Count, Value}, ValOrOfBin]),
			{value, Value};
		    true ->
			if 
			    T == big -> <<Offset:32/big>> = ValOrOfBin;
			    T == little -> <<Offset:32/little>> = ValOrOfBin
			end,
			{offset, Offset}
		end,
	    [{Tag, Type, Count, ValOrOffset} | getDirEntries(T, N-1, Next)]
    end.

-define(NewSubFileType, 254).   %%% Subfile contents %% Not Supported
-define(ImageWidth,  256).      %%% No of pixels per row
-define(ImageLength, 257).      %%% No of rows

-define(BitsPerSample, 258).    %%%  
-define(Compression, 259).      %%% 1 No Comp; 2 CCITT G3 1-D Modified Huffman RLE
                                %%% 5 Extension ; 32773 PackBits

-define(DocumentName, 269).     %%% Skipped
-define(ImageDescription, 270). %%% Skipped

-define(Predictor, 317).        %%% 1 No differencing (default)
                                %%% 2 Horizontal differencing

-define(PhotoMetricInt, 262).   %%% 0 WhiteisZero;1 BlackIsZero;2 RGB;3 RGB ColMap

-define(StripOffsets, 273).     %% Pointer to each chunk
-define(SamplesPerPixel, 277).  %%   
-define(RowsPerStrip, 278).     %% No Rows in each chunk
-define(StripByteCounts, 279).  %% No of bytes in each strip after compression

-define(XResolution, 282).      %%  
-define(YResolution, 283).      %%  
-define(ResolutionUnit, 296).   %%  1; 2 Inch; 3 cm

%% Optional
-define(Orientation, 274).      
-define(PlanarConf, 284).       

-define(ColorMap, 320).
-define(ExtraSamples, 338).
-record(tif, {w,h,type,order = upper_left, bpp, bps, spp, rps, so, sbc, 
	      map,comp, pred}).

load_image(Enc, IFDs, Orig) ->
    Tif = get_info(IFDs, #tif{}, Orig, Enc),
    %%    io:format("IFD ~p ~nFileSize ~p ~n ~p", 
    %%               [IFDs, size(Orig), Tif]),
    Strips = get_strips(Tif#tif.so, Tif#tif.sbc, Orig, Enc, []),    
    Size = Tif#tif.w * Tif#tif.h * (Tif#tif.bpp div 8),
%     io:format("Tif size ~w ~w ~w ~n", 
% 	      [Size, {Tif#tif.w, Tif#tif.h,Tif#tif.bpp, 
% 		      Tif#tif.order, Tif#tif.rps},
% 	       {Tif#tif.type,Tif#tif.bps, Tif#tif.spp, length(Strips)}]),
    #tif{type=Type, bpp=Bpp} = Tif,
    case decompress(Strips, Tif#tif.comp, Tif#tif.h, Tif, []) of
	BinImage = <<Image0:Size/binary, _/binary>> ->
	    {ResType,Image2} = 
		if
		    Type =:= bw, Bpp =:= 1 -> 			
			Map = {0,255},
			Image1 = convert1(size(BinImage), BinImage, Map, []),
			PaddL = Tif#tif.w + e3d_image:pad_len(Tif#tif.w, 8),
			Image = strip(Tif#tif.h, Image1, Tif#tif.w, PaddL, []),
			{g8, Image};
		    Type == bw,     Bpp == 8 -> {g8, Image0};
		    Type == alpha,  Bpp == 8 -> {a8, Image0};
		    Type == maprgb -> 
			Image = convert(size(Image0), Image0, Tif#tif.map, []),
			{r8g8b8, Image};
		    Type == rgb, Bpp > 32, Tif#tif.spp > 3 -> 
			Image = remove_extra_samples(3,(Tif#tif.bpp-24),Image0,[]),
			{r8g8b8, 3, Image};
		    Type =:= rgb, Bpp =:= 24 ->
			{r8g8b8, Image0};
		    Type =:= rgb, Bpp =:= 32 ->
			{r8g8b8a8, Image0};
		    true ->
			exit(unsupported_format)
		end,
	    #e3d_image{width = Tif#tif.w, height = Tif#tif.h, 
		       alignment = 1,%% Correct ??
		       image = Image2, order = Tif#tif.order, 
		       type = ResType, 
		       bytes_pp = e3d_image:bytes_pp(ResType)};
 	{error,_}=Error ->
 	    Error
    end.

remove_extra_samples(_RGBits, _DiscardBits, <<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
remove_extra_samples(RGBits, DiscardBits, Image, Acc) ->
    <<Keep:3/binary, _Del:DiscardBits, Rest/binary>> = Image,
    remove_extra_samples(RGBits, DiscardBits, Rest, [Keep|Acc]).

save_image(Image, Compress, Offset1) ->
    W = <<?ImageWidth:16, (type2type(long)):16, 1:32, (Image#e3d_image.width):32>> , 
    H = <<?ImageLength:16, (type2type(long)):16, 1:32, (Image#e3d_image.height):32>>,
    BPS = <<?BitsPerSample:16, (type2type(short)):16, (Image#e3d_image.bytes_pp):32, Offset1:32>> ,
    BPSBin =  %% Should be placed on Offset1
	if 
	    Image#e3d_image.type == r8g8b8 -> 
		<<8:16, 8:16, 8:16, 0:16>> ;  %% Extra word alignment
	    Image#e3d_image.type == r8g8b8a8 -> 
		<<8:16, 8:16, 8:16, 8:16>>
        end,
    Offset2 = Offset1 + 8,
    {Comp, Strips} = 
	if Compress == true ->
		{<<?Compression:16, (type2type(short)):16, 1:32, 5:16, 0:16>>,
		 lzw_compress(Image)};
	   Compress == false ->
		{<<?Compression:16, (type2type(short)):16, 1:32, 1:16, 0:16>>, 
		 Image#e3d_image.image}
        end,
    Photo = <<?PhotoMetricInt:16, (type2type(short)):16, 1:32, 2:16, 0:16>> ,
    STO  =  <<?StripOffsets:16, (type2type(long)):16, 1:32, Offset2:32>> ,
     %%    Strips =  %% should be placed at Offset2

    StripPadd = case size(Strips) rem 4 of
		    0 -> <<>> ;
		    I -> 
			PaddSz = (4-I)*8,  %% word align
			Padd = <<0:PaddSz>> ,
			Padd
   	        end,
    Offset3 = Offset2 + size(Strips) + size(StripPadd),
    SPP  = <<?SamplesPerPixel:16, (type2type(short)):16, 1:32, (Image#e3d_image.bytes_pp):16, 0:16>>,
    RPS  = <<?RowsPerStrip:16, (type2type(long)):16, 1:32, (Image#e3d_image.height):32>>,
    STBC =  <<?StripByteCounts:16, (type2type(long)):16, 1:32, (size(Strips)):32>>,

    %% I don't know what to put here but they are requried fields so I add
    %% some stolen values..
    XRes = <<?XResolution:16, (type2type(rational)):16, 1:32, Offset3:32>> ,
    XResBin = %% should be placed Offset3
	<<314572800:32, 262144:32>>,
    Offset4 = Offset3 + 8,
    YRes = <<?YResolution:16, (type2type(rational)):16, 1:32, Offset4:32>> ,
    YResBin = %% should be placed Offset4
	<<314572800:32, 262144:32>>,
    Offset5 = Offset4 + 8,
    ResU = <<?ResolutionUnit:16, (type2type(short)):16, 1:32, 2:16, 0:16>> ,
    EndOfBlock = <<0:32>> ,

    Bin = 
	[BPSBin,Strips, StripPadd, XResBin, YResBin,  %% Pointer Blocks
	 <<12:16>>, %% No of IFD entries  Offset5 should point here
	 W, H, BPS, Comp, Photo, STO, SPP, RPS, STBC, XRes, YRes, ResU, 
	 EndOfBlock],
    {Offset5, Bin}.
         
get_info([], Tif, _Orig,_Enc) -> Tif;
get_info([{?PhotoMetricInt, _, 1, {value, Val}}| R], Tif, Orig,Enc) ->  
    Type = case Val of
	       1 -> bw;
	       2 -> rgb;
	       3 -> maprgb;
	       4 -> alpha;
	       _ -> exit(unsupported_format)
	   end,
    get_info(R, Tif#tif{type=Type}, Orig,Enc);
get_info([{?ImageWidth, _, 1, {value, W}}|R], Tif, Orig,Enc) ->
    get_info(R, Tif#tif{w = W}, Orig,Enc);
get_info([{?ImageLength, _, 1, {value, H}}|R], Tif, Orig,Enc) ->
    get_info(R, Tif#tif{h = H}, Orig,Enc);
get_info([{?BitsPerSample, short, 1, {value, Bpp}}|R], Tif, Orig,Enc) ->
    get_info(R, Tif#tif{bps = [Bpp]}, Orig,Enc);
get_info([{?BitsPerSample, Type = short, Count, {offset, Off}}|R], Tif, Orig,Enc) ->
    Len = typeSz(Type) * Count,
    <<_:Off/binary, BPS:Len/binary, _/binary>> = Orig,
    Bpp = getdata(Enc, Type, Count, BPS),
% 	case getdata(Enc, Type, Count, BPS) of
% 	    Bps = [8,8,8|_] -> Bps;
% 	    Err ->
% 		io:format("~p: Unsupported BitsPerSample ~p ~n", [?MODULE, Err]),
% 		error({?MODULE, unsupported, bitsPerSample})
% 	end,
    get_info(R, Tif#tif{bps = Bpp}, Orig,Enc);
get_info([{?SamplesPerPixel, _, 1, {value, SPP}}|R], Tif, Orig,Enc) ->
    SPP = length(Tif#tif.bps), %% Assert
    get_info(R, Tif#tif{spp = SPP, bpp = lists:sum(Tif#tif.bps)}, Orig,Enc);	
get_info([{?ColorMap, Type = short, Count, {offset, Off}}|R], Tif, Orig,Enc) ->
    Len = typeSz(Type) * Count,
    <<_:Off/binary, MapBin:Len/binary, _/binary>> = Orig,
    Map  = get_map(MapBin, Enc),
    get_info(R, Tif#tif{map=Map}, Orig, Enc);

get_info([{?ExtraSamples, Type = short, Count, {offset, Off}}|R], Tif, Orig,Enc) ->
    Len = typeSz(Type) * Count,
    <<_:Off/binary, Data:Len/binary, _/binary>> = Orig,
    _What = getdata(Enc, short, Count, Data),
%%    io:format("Tif Extra Samples ~p~n", [What]),
    get_info(R, Tif, Orig, Enc);
get_info([{?Compression, _, 1, {value, Comp}}|R], Tif, Orig,Enc) ->
%%    io:format("Compression ~p ~n", [Comp]),
    get_info(R, Tif#tif{comp = Comp}, Orig,Enc);
get_info([{?Predictor, _, 1, {value, Pred}}|R], Tif, Orig,Enc) ->
%%    io:format("Predictor ~p ~n", [Pred]),
    get_info(R, Tif#tif{pred = Pred}, Orig,Enc);

get_info([{?StripOffsets, Type = long, Count, Where}|R], Tif, Orig,Enc) ->
    Sofs = 
	case Where of 
	    {value, Offset} when Count == 1 ->
		[Offset];
	    {offset, Offset} -> %% Offset to Offsets
		Len = typeSz(Type) * Count,
		<<_:Offset/binary, OFSB:Len/binary, _/binary>> = Orig,
		getdata(Enc, Type, Count, OFSB)	
	end,
    get_info(R, Tif#tif{so = Sofs}, Orig,Enc);
get_info([{?RowsPerStrip, _Type, 1, {value, Rows}}|R], Tif, Orig,Enc) ->
    get_info(R, Tif#tif{rps = Rows}, Orig,Enc);
get_info([{?StripByteCounts, Type, Count, What}|R], Tif, Orig,Enc) ->
    SBCS = 
	case What of 
	    {value, Length} when Count == 1 ->
		[Length];
	    {offset, Offset} -> %% Offset to Offsets
		Len = typeSz(Type) * Count,
		<<_:Offset/binary, OFSB:Len/binary, _/binary>> = Orig,
		getdata(Enc, Type, Count, OFSB)	
	end,	
    get_info(R, Tif#tif{sbc = SBCS}, Orig,Enc);

get_info([{?Orientation, _Type, 1, {value, Orient}}|R], Tif, Orig,Enc) ->
    Order = 
	case Orient of
	    1 -> upper_left;
	    2 -> upper_right;
	    3 -> lower_rigth;
	    4 -> lower_left;
	    Err ->
		
		io:format("~p: Unsupported orientation ~p ~n", [?MODULE, Err]),
		exit({orientation, Err})
	end,
    get_info(R, Tif#tif{order = Order}, Orig,Enc);
get_info([{?PlanarConf, _Type, 1, {value, 1}}|R], Tif, Orig,Enc) ->
    get_info(R, Tif, Orig,Enc);

%% SKIP these 
get_info([{?XResolution, _, _, {offset, _W}}|R], Tif, Orig,Enc) -> %SKIP    
%    <<_:W/binary, BPS:8/binary, _/binary>> = Orig,
%    What = getdata(Enc, rational, 1, BPS),
%    io:format("XREs ~p~n", [What]),
    get_info(R, Tif, Orig,Enc);
get_info([{?YResolution, _, _, {offset,_W}}|R], Tif, Orig,Enc) -> %SKIP
    get_info(R, Tif, Orig,Enc);
get_info([{?ResolutionUnit, _, _, _}|R], Tif, Orig,Enc) -> %SKIP
    get_info(R, Tif, Orig,Enc);
get_info([{?NewSubFileType, _, _, _}|R], Tif, Orig,Enc) -> 
    %% Skip it we don't support subfile contents anyway.
    get_info(R, Tif, Orig,Enc);
get_info([{?DocumentName, _, _, _}|R], Tif, Orig,Enc) -> %% Skip
    get_info(R, Tif, Orig,Enc);
get_info([{?ImageDescription, _, _, _}|R], Tif, Orig,Enc) -> %% Skip
    get_info(R, Tif, Orig,Enc);


get_info([Err|R], Tif, Orig,Enc) ->  
    io:format("~p: Unsupported TAG ~p ~n", [?MODULE, Err]),
    get_info(R, Tif, Orig,Enc).  

get_strips([], [], _Orig, _Enc, Acc) ->
    lists:reverse(Acc);
get_strips([StripOff|R1], [SBC|R2], Orig, Enc, Acc) ->
    case Orig of
	<<_:StripOff/binary, Strip:SBC/binary, _/binary>> ->
	    get_strips(R1, R2, Orig, Enc, [Strip|Acc])
    end.

-define(c16to8(C), round(((C) *255) / ((1 bsl 16) -1))).
%% Color Map
get_map(Bin,Enc) ->
    Sz = size(Bin) div 3,
    <<R:Sz/binary,G:Sz/binary,B:Sz/binary>> = Bin,
    get_map(Sz,R,G,B,Enc,[]).
    
get_map(0,_,_, _, _Enc,Acc) ->
    list_to_tuple(Acc);
get_map(I,RB,GB,BB,little,Acc) ->
    Prev = I-2,
    <<_:Prev/binary,R:16/little,_/binary>> = RB,
    <<_:Prev/binary,G:16/little,_/binary>> = GB,
    <<_:Prev/binary,B:16/little,_/binary>> = BB,
    get_map(Prev,RB,GB,BB,little,[[?c16to8(R),?c16to8(G),?c16to8(B)]|Acc]);
get_map(I,RB,GB,BB,big,Acc) ->
    Prev = I-2,
    <<_:Prev/binary,R:16/big,_/binary>> = RB,
    <<_:Prev/binary,G:16/big,_/binary>> = GB,
    <<_:Prev/binary,B:16/big,_/binary>> = BB,
    get_map(Prev,RB,GB,BB,big,[[?c16to8(R),?c16to8(G),?c16to8(B)]|Acc]).

convert(0,_, _, Acc) ->
    list_to_binary(Acc);
convert(I, Image, Map, Acc) ->
    Prev = I-1,
    <<_:Prev/binary,Index:8,_/binary>> = Image,
    Col = element(Index+1, Map),
    convert(Prev,Image, Map, [Col|Acc]).

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

strip(0, _Image, _RowL, _PaddL, Acc) ->
    list_to_binary(Acc);
strip(Row, Image, RowL, PaddL, Acc) ->
    Skip = (Row-1)*PaddL,
    <<_:Skip/binary, RowBin:RowL/binary, _/binary>> = Image,
    strip(Row-1, Image, RowL, PaddL, [RowBin|Acc]).

%% Compression

-define(LZW_CLEAR,           256).
-define(LZW_EOI,             257).
-define(LZW_FIRST,           258).
-define(LZW_STARTBITLEN,       9).
-define(LZW_SWAP_9,          510).
-define(LZW_SWAP_10,        1022).
-define(LZW_SWAP_11,        2046).
-define(LZW_MAX,            4094).

-define(get_lzw(Code), get(Code)).
-define(add_lzw(Code, Str), put(Code, Str)).

decompress(RevStrips, 1, _, _, _) ->  %% No Compression
    list_to_binary(RevStrips);
decompress([CompStrip|Rest], Comp = 5, RowsLeft, Tif, Acc) -> %% LZW-Compression
    ReadCode = fun(BitLen, UsedBits) ->
		       Shift = case ((UsedBits + BitLen) rem 8) of
				   0 -> 0;
				   I -> 8 - I
			       end,
		       case CompStrip of
			   <<_:UsedBits, Code:BitLen, _:Shift, _/binary>> ->
						%io:format("~p ",[Code]),
			       {Code, UsedBits + BitLen};
			   _ ->
			       io:format("~nEOI ~p ~p'~n",[BitLen, UsedBits]),
			       {?LZW_EOI, size(CompStrip) * 8}
		       end
	       end,
    Decomp = lzw_decomp(0, ReadCode, 0, 258, ?LZW_STARTBITLEN, []),    
    Differented = 
	case Tif#tif.pred of
	    2 when Tif#tif.bpp == 32 -> %% Horizontal differencing
		undo_differencing4(0, Tif#tif.w, lists:append(lists:reverse(Decomp)), 
				   0,0,0,0, []);
	    2 when Tif#tif.bpp == 24 -> %% Horizontal differencing
		undo_differencing3(0, Tif#tif.w, lists:append(lists:reverse(Decomp)), 
				   0,0,0, []);
	    _ -> %% No differencing
		Decomp
	end,
    %% Some pictures seem to fail to create correct size in rows per strip
    Rows = case RowsLeft > Tif#tif.rps of
	       true -> Tif#tif.rps;
	       false -> RowsLeft
	   end,

    Size = Tif#tif.w * Rows * Tif#tif.bpp div 8,    
%     io:format("Lens ~p ~p ~p ~p~n", 
% 	      [Size, Rows, length(lists:flatten(Decomp)), 
% 	       size(list_to_binary(Differented))]),
    <<StripBin:Size/binary, _/binary>> = list_to_binary(lists:reverse(Differented)),
    decompress(Rest, Comp, RowsLeft-Rows, Tif, [StripBin|Acc]);

decompress([CompStrip|Rest], Comp = 32773, RowsLeft, Tif, Acc) ->  %% PackBits
    W = Tif#tif.w * (Tif#tif.bpp div 8),
    Rows = case RowsLeft > Tif#tif.rps of
	       true -> Tif#tif.rps;
	       false -> RowsLeft
	   end,
    Bins = unpack_bits(0, W, 0, Rows, CompStrip, []),
    Size = Tif#tif.w * Rows * Tif#tif.bpp div 8,    
    <<StripBin:Size/binary, _/binary>> = list_to_binary(lists:reverse(Bins)),
    decompress(Rest, Comp, RowsLeft-Rows, Tif, [StripBin|Acc]);
decompress([], _Comp, 0, _, Acc) -> 
    list_to_binary(lists:reverse(Acc));
decompress(_RevStrips, Comp, 0, _, _Acc) ->
    io:format("~p: Unsupported Compression ~p ~n", [?MODULE, Comp]),
    {error, {none,?MODULE,{unsupported_compression,Comp}}}.

undo_differencing4(W, W, Rest, _,_,_,_,Ack) ->
    undo_differencing4(0,W, Rest, 0,0,0,0,Ack);
undo_differencing4(C, W, [R,G,B,A|Rest], AR,AG,AB,AA, Ack) ->
    RR = (R + AR) rem 256,    
    RG = (G + AG) rem 256, 
    RB = (B + AB) rem 256,
    RA = (A + AA) rem 256,
    undo_differencing4(C+1, W, Rest, RR,RG,RB,RA, [RA,RB,RG,RR|Ack]); 
undo_differencing4(_, _, [], _,_,_,_, Ack) ->
    Ack.
undo_differencing3(W, W, Rest, _,_,_,Ack) ->
    undo_differencing3(0,W, Rest, 0,0,0,Ack);
undo_differencing3(C, W, [R,G,B|Rest], AR,AG,AB, Ack) ->
    RR = (R + AR) rem 256,    
    RG = (G + AG) rem 256, 
    RB = (B + AB) rem 256,
    undo_differencing3(C+1, W, Rest, RR,RG,RB, [RB,RG,RR|Ack]);
undo_differencing3(_, _, [], _,_,_, Ack) ->
    Ack.

unpack_bits(0, _W, H, H, _, Acc) ->
    Acc;    
unpack_bits(WC, W, HC, H, Bin, Acc) when WC == W ->
    unpack_bits(0, W, HC+1, H, Bin, Acc);

unpack_bits(BC, W, HC, H, <<Code:8/signed, Rest/binary>>, Acc) when BC < W ->
%    io:format("~p ", [{Code, BC}]),
    if 
	Code == 128 ->
	    unpack_bits(BC, W, HC, H, Rest, Acc);
	Code >= 0 ->
	    Count = Code + 1,
	    <<Bin:Count/binary, Cont/binary>> = Rest, 
		    unpack_bits(BC + Count, W, HC, H, Cont, [Bin|Acc]);
	Code < 0 ->
	    Count = abs(Code) + 1,
	    <<Re:8, Cont/binary>> = Rest,
	    Bin = list_to_binary(lists:duplicate(Count, Re)),
	    unpack_bits(BC + Count, W, HC, H, Cont, [Bin|Acc])
    end.

%%%% Test !!
% Ex = <<256:9, 7:9,258:9, 8:9,8:9,258:9,6:9, 257:9>>.
% <<128,1,224,64,128,68,8,13,1>>
% 99> Raw = e3d__tif:decompress([Ex], 5, []).                          
% <<7,7,7,8,8,7,7,6>>.
% e3d__tif:lzw_init_compress(Raw, size(Raw), 9, {0,[]},[]).
% 
lzw_decomp(S, Read, PrevCode, Count, BitLen, Acc) ->
    case (catch Read(BitLen,S)) of
	{?LZW_EOI, _Where} ->	    	 
%	    io:format("~nEOI-1 ~p ~n", [{S, PrevCode, Count, BitLen, _Where}]),
	    Acc;
	{?LZW_CLEAR, NS} -> 
	    lzw_init(0),
%	    io:format("~nClear table ~p~n", [{S, PrevCode, Count, BitLen}]),
	    case catch Read(9, NS) of
		{?LZW_EOI, _} ->
%%		    io:format("~nEOI-2 ~p ~n", [{S, PrevCode, Count, BitLen}]),
		    Acc;
		{NewCode, NS2} when is_integer(NewCode) -> 
		    Str = ?get_lzw(NewCode),
		    lzw_decomp(NS2, Read, NewCode, 258, 9, [Str|Acc]);
	    	Else ->
		    io:format("~n~p: Error ~p Args: ~p ~n", 
			      [?MODULE, Else, {NS, PrevCode, Count, BitLen}]),
		    exit({badly_compressed_data})
	    end;
	{NewCode, NS} when is_integer(NewCode) ->
	    case ?get_lzw(NewCode) of
		undefined when Count == NewCode ->
		    OldStr = [H|_] = ?get_lzw(PrevCode),
		    NewStr = OldStr ++ [H],
		    ?add_lzw(Count, NewStr),
		    lzw_decomp(NS, Read, NewCode, Count +1, lzw_bl(Count, BitLen), [NewStr|Acc]);
		Str = [H|_]->
		    ?add_lzw(Count, ?get_lzw(PrevCode) ++ [H]),
		    lzw_decomp(NS, Read, NewCode, Count +1, lzw_bl(Count, BitLen), [Str|Acc]);
		Else ->
		    io:format("~n~p: Error Case Clause ~p ~p Args ~p ~n", 
			      [?MODULE, Else, NewCode, {S, PrevCode, Count, BitLen}]),
		    exit({badly_compressed_data})
	    end;
	Else ->
	    io:format("~n~p: Error ~p Args: ~p ~n", 
		      [?MODULE, Else, {S, PrevCode, Count, BitLen}]),
	    exit({badly_compressed_data})
    end.

lzw_bl(?LZW_SWAP_9, Len) ->  Len +1;
lzw_bl(?LZW_SWAP_10, Len) -> Len +1;
lzw_bl(?LZW_SWAP_11, Len) -> Len +1;
lzw_bl(_, Len) -> Len.

lzw_init(No) when No > 257 ->
    case erase(No) of
	undefined ->
	    ok;
	_ ->
	    lzw_init(No+1)
    end;
lzw_init(No) ->
    ?add_lzw(No, [No]),
    lzw_init(No +1).

lzw_init_comp() ->
    erase(),
    lzw_init_comp(257).
lzw_init_comp(No) when No >= 0 ->
    ?add_lzw([No], No),
    lzw_init_comp(No - 1);
lzw_init_comp(_) ->
    ok.

-define(lzw_write(Aa,Ba,Ca), lzw_write(Aa,Ba,Ca)).

lzw_compress(Image) ->
    lzw_init_compress(Image#e3d_image.image, Image#e3d_image.width * Image#e3d_image.bytes_pp,
		      ?LZW_STARTBITLEN, {0, []}, []).

lzw_init_compress(Bin, W, BitLen, Build, Acc) ->
    lzw_init_comp(),
    {NBuild, Nacc} = lzw_write({BitLen,?LZW_CLEAR}, Build, Acc),
    lzw_compress(Bin, 0, W, [], ?LZW_STARTBITLEN, ?LZW_FIRST, NBuild, Nacc).

lzw_compress(<<>>, CC, _W, Omega, BitLen, _TabCount, Build, Acc) ->
    Code =?get_lzw(Omega),
    {NBuild, Nacc} =  ?lzw_write({BitLen,Code}, Build, Acc),

    %%    NewBL = lzw_bl(TabCount, BitLen),
    NewBL = BitLen,
    {{TotBitLen, Codes}, N2acc} = lzw_write({NewBL,?LZW_EOI}, NBuild, Nacc),
    PaddL = 8 - (TotBitLen rem 8),
    case catch lzw_buildbin(lists:reverse([{PaddL, 0}|Codes])) of
        Bin when is_binary(Bin) -> 
            list_to_binary(lists:reverse([Bin|N2acc]));
	_Else ->
	    io:format("~p:~p Error ~p ~p ~n", [?MODULE, ?LINE, {PaddL, Codes}, CC]),
	    error({?MODULE, decoder, {internal_error, ?LINE}})
    end;
lzw_compress(Bin, CC, W, Omega, BitLen, TabCount, Build, Acc) when CC == W ->
    Code =?get_lzw(Omega),
    {NBuild, Nacc} = lzw_write({BitLen,Code}, Build, Acc),    
    lzw_init_compress(Bin, W, lzw_bl(TabCount-1,BitLen), NBuild, Nacc);

lzw_compress(<<Char:8, Bin/binary>>, CC, W, Omega, BitLen, TabC, Build, Acc) ->
    NewOmega = [Char|Omega],
    case ?get_lzw(NewOmega) of
	undefined ->
	    Code = ?get_lzw(Omega),
	    {NBuild, Nacc} = lzw_write({BitLen,Code}, Build, Acc),
	    ?add_lzw(NewOmega, TabC),
	    case TabC of
		?LZW_MAX - 1 ->
		    Code2 =?get_lzw([Char]),
		    {NBuild2, Nacc2} = ?lzw_write({BitLen,Code2}, NBuild, Nacc),
		    lzw_init_compress(Bin, W, lzw_bl(TabC-1,BitLen), NBuild2, Nacc2);
		_ ->
		    lzw_compress(Bin, CC+1, W, [Char], lzw_bl(TabC-1, BitLen), TabC + 1, 
				 NBuild, Nacc)
	    end;
	_ ->
	    lzw_compress(Bin, CC +1, W, NewOmega, BitLen, TabC, Build, Acc)
    end.

lzw_write({_,undefined}, _, _) -> 
    error({undef,value});
lzw_write({CLen, Code}, {Totlen, List}, Acc) ->
    NewLen = CLen + Totlen,
    if 
	NewLen rem 8 == 0 ->
	    case catch lzw_buildbin(lists:reverse([{CLen,Code}|List])) of
		Bin when is_binary(Bin) ->
		    {{0, []}, [Bin|Acc]};
		{Bin, NewList} when is_binary(Bin) ->
		    Sum = lists:foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
		    {{Sum, lists:reverse(NewList)}, [Bin|Acc]};
		Else ->
		    io:format("~p:~p Error ~p ~p ~n", [?MODULE, ?LINE, Else, 
						       [{CLen, Code}, {Totlen, List}]]),
		    error({?MODULE, decoder, {internal_error, ?LINE}})
	    end;
	NewLen > 100 -> 
	    case catch lzw_buildbin(lists:reverse([{CLen,Code}|List])) of
		{Bin, NewList} when is_binary(Bin) ->		    
		    Sum = lists:foldl(fun({X,_}, Sum) -> X + Sum end, 0, NewList),
		    {{Sum, lists:reverse(NewList)}, [Bin|Acc]};
		Else ->
		    io:format("~p:~p Error ~p ~p ~n", [?MODULE, ?LINE, Else, 
						       [{CLen, Code}, {Totlen, List}]]),
		    error({?MODULE, decoder, {internal_error, ?LINE}})
	    end;
	true ->
	    {{Totlen + CLen,[{CLen,Code}|List]}, Acc}
    end.
lzw_buildbin([{8,0}]) ->
    <<>>;
lzw_buildbin([{L1, C1},{L2,C2}]) ->
    <<C1:L1, C2:L2>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3}]) ->
    <<C1:L1,C2:L2,C3:L3>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6},{L7,C7}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7>>;
lzw_buildbin([{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6},{L7,C7},{L8,C8}]) ->
    <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7,C8:L8>>;

lzw_buildbin(_DBG = [{L1,C1},{L2,C2},{L3,C3},{L4,C4},{L5,C5},{L6,C6},{L7,C7},{L8,C8},{L9,C9} | Rest]) ->
    RemL = (L1+L2+L3+L4+L5+L6+L7+L8) rem 8,
    AddL = 8 - RemL,
    KeepL = L9 - AddL,
    SkipL = 16 - (AddL + KeepL),
    TempFill = 8 - (L9 rem 8),    
%    io:format("~w~n ~p ~p ~p~n", [DBG, {AddL, KeepL}, {C9,L9}, TempFill]),
    <<P9:AddL,Keep9:KeepL, _:SkipL>> = <<C9:L9,0:TempFill>>,
    Bin = <<C1:L1,C2:L2,C3:L3,C4:L4,C5:L5,C6:L6,C7:L7,C8:L8,P9:AddL>>,
    NewList = [{KeepL, Keep9}|Rest],
    {Bin, NewList}.

%% getdata(Enc, Type, Count, Bin)
getdata(Enc, Type, 1, Bin) ->
    [Val] = getdata2(Enc, Type, 1, Bin),
    Val;
getdata(Enc, Type, N, Bin) ->
    getdata2(Enc, Type, N, Bin).

getdata2(_, _, 0, _Bin) -> 
    [];

getdata2(_, byte, Count, Bin) -> 
    <<Data:Count/binary, _Rest/binary>> = Bin,
    binary_to_list(Data);
getdata2(Enc, sbyte, Count, <<V:8/signed, Bin/binary>>) -> 
    [V| getdata2(Enc, sbyte, Count -1, Bin)];

getdata2(_, ascii, Count, Bin) ->
    <<Data:Count/binary, _Rest/binary>> = Bin,
    binary_to_list(Data);

getdata2(big, short, Count, <<V:16/big, Bin/binary>>) ->
    [V| getdata2(big, short, Count -1, Bin)];
getdata2(little, short, Count, <<V:16/little, Bin/binary>>) ->
    [V| getdata2(little, short, Count -1, Bin)];
getdata2(big, sshort, Count, <<V:16/big-signed, Bin/binary>>) ->
    [V| getdata2(big, sshort, Count -1, Bin)];
getdata2(little, sshort, Count, <<V:16/little-signed, Bin/binary>>) ->
    [V| getdata2(little, sshort, Count -1, Bin)];

getdata2(big, long, Count, <<V:32/big, Bin/binary>>)  ->
    [V| getdata2(big, long, Count -1, Bin)];
getdata2(little, long, Count, <<V:32/little, Bin/binary>>)->
    [V| getdata2(little, long, Count -1, Bin)];
getdata2(big, slong, Count, <<V:32/big-signed, Bin/binary>>)  ->
    [V| getdata2(big, slong, Count -1, Bin)];
getdata2(little, slong, Count, <<V:32/little-signed, Bin/binary>>)->
    [V| getdata2(little, slong, Count -1, Bin)];

getdata2(big, rational, Count, <<V1:32/big, V2:32/big, Bin/binary>>)  ->
    [{V1,V2}| getdata2(big, rational, Count -1, Bin)];
getdata2(little, rational, Count, <<V1:32/little, V2:32/little,Bin/binary>>)->
    [{V1,V2}| getdata2(little, rational, Count -1, Bin)];
getdata2(big, srational, Count, <<V1:32/big-signed, V2:32/big-signed, Bin/binary>>)  ->
    [{V1,V2}| getdata2(big, rational, Count -1, Bin)];
getdata2(little, srational, Count, <<V1:32/little-signed, V2:32/little-signed,Bin/binary>>)->
    [{V1,V2}| getdata2(little, rational, Count -1, Bin)];

getdata2(big, float, Count, <<V:32/float-big, Bin/binary>>)  ->
    [V| getdata2(big, float, Count -1, Bin)];
getdata2(little, float, Count, <<V:32/float-little, Bin/binary>>)->
    [V| getdata2(little, float, Count -1, Bin)];
getdata2(big, double, Count, <<V:64/float-big, Bin/binary>>)  ->
    [V| getdata2(big, double, Count -1, Bin)];
getdata2(little, double, Count, <<V:64/float-little, Bin/binary>>)->
    [V| getdata2(little, double, Count -1, Bin)];
getdata2(_, _, _, Bin) ->
    Bin.


%% Conversions

typeSz(Type) ->
    case Type of
	byte -> 1;
	ascii -> 1;
	short -> 2;
	long -> 4;
	rational -> 8;
	sbyte -> 1;
	undefined -> 1;
	sshort -> 2;
	slong -> 4;
	srational -> 8;
	float -> 4;
	double -> 4;
	_ -> 1
    end.
     
type2type(Id) ->
    case Id of
	1 -> byte;
	2 -> ascii;
	3 -> short;
	4 -> long;
	5 -> rational;
	6 -> sbyte;
	7 -> undefined;
	8 -> sshort;
	9 -> slong;
	10 -> srational;
	11 -> float;
	12 -> double;
%	byte -> 1;
%	ascii -> 2;
	short -> 3;
	long -> 4;
	rational -> 5;
%	sbyte -> 6;
%	undefined -> 7;
%	sshort -> 8;
%	slong -> 9;
%	srational -> 10;
%	float -> 11;
%	double -> 12;
	_ -> Id
    end.
