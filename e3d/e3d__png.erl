%%
%%  e3d__png.erl --
%%
%%     Functions for reading and writing PNG files.
%%
%%  Copyright (c) 2005-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(e3d__png).

-export([load/1,load/2,save/2,save/3,save_bin/1,save_bin/2]).
-export([format_error/1, test/1, test/0]).

%%-compile(export_all). %% testing
-compile(inline).
-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

-include("e3d_image.hrl").

%%-define(TC(Cmd), wings_util:tc(fun() -> Cmd end, ?MODULE, ?LINE)).
-define(MAGIC, 137,$P,$N,$G,$\r,$\n,26,$\n).

-define(GREYSCALE,   0).
-define(TRUECOLOUR,  2).
-define(INDEXED,     3).
-define(GREYSCALE_A, 4).
-define(TRUECOLOUR_A,6).

-define(MAX_WBITS,15).

-record(png, {w,h,bpc,type,palette,
	      interlace=0,trns,bkgd=[0,0,0,0],
	      restype,chunks=[],z}).

%% Chunk sz of 240 bytes is good divideable with 2|3|4 which always givs complete
%% pixels in the chunks.
-define(CHUNK, 240).

-define(get4p1(Idx),((Idx)  bsr 4)).
-define(get4p2(Idx),((Idx)  band 16#0F)).
-define(get2p1(Idx),((Idx)  bsr 6)).
-define(get2p2(Idx),(((Idx) bsr 4) band 3)).
-define(get2p3(Idx),(((Idx) bsr 2) band 3)).
-define(get2p4(Idx),((Idx)  band 3)).
-define(get1p1(Idx),((Idx)  bsr 7)).
-define(get1p2(Idx),(((Idx) bsr 6) band 1)).
-define(get1p3(Idx),(((Idx) bsr 5) band 1)).
-define(get1p4(Idx),(((Idx) bsr 4) band 1)).
-define(get1p5(Idx),(((Idx) bsr 3) band 1)).
-define(get1p6(Idx),(((Idx) bsr 2) band 1)).
-define(get1p7(Idx),(((Idx) bsr 1) band 1)).
-define(get1p8(Idx),((Idx)  band 1)).

format_error({?MODULE,unsupported_format}) ->
    "Unsupported format or bad PNG file";
format_error({none,?MODULE,_}) ->
    "Decoding error";
format_error({unsupported_compression,Comp}) ->
    io_lib:format("Unsupported compression type (~p)", [Comp]).

load(FileName) ->
    load(FileName, []).
load(FileName, _Opts) ->
    Z = zlib:open(),  
    try load1(FileName,_Opts,Z)
    catch 
	throw:Reason ->
	    ST = erlang:get_stacktrace(),
	    io:format("~n~p: Bad File: ~p ~P~n",[?MODULE,Reason,ST,30]),
	    {error, {?MODULE,Reason}};
        error:Reason ->
	    ST = erlang:get_stacktrace(),
	    io:format("~n~p: Internal Error: ~P ~P~n",[?MODULE,Reason,30,ST,30]),
	    {error, {?MODULE,Reason}}
    after 
	zlib:close(Z)
    end.

load1(FileName, _Opts,Z) ->
    case file:read_file(FileName) of
	{ok, <<?MAGIC, Chunks/binary>>} ->
	    decode_chunks(0, Chunks, #png{z=Z});
	{ok, _Bin} ->
	    {error, {?MODULE,corrupt_file}};
	Error ->
	    Error
    end.

save_bin(Img) ->
    save(Img,undefined,binary,[]).
save_bin(Img,Opts) ->
    save(Img,undefined,binary,Opts).

save(Img, File) ->
    save(Img, File, undefined,[]).
save(Img, File,Opts) ->
    save(Img, File, undefined,Opts).
save(Img, File, Type, Options) ->
    Z = zlib:open(),
    try 
	Binary = save1(Img,Options,Z),
	case Type of
	    binary -> {ok, Binary};
	    _ -> file:write_file(File, Binary)
	end
    catch 
	throw:Reason ->
	    ST = erlang:get_stacktrace(),
	    io:format("~n~p: Bad File: ~p ~P~n",[?MODULE,Reason,ST,30]),
	    {error, {?MODULE,Reason}};
	  error:Reason ->
	    ST = erlang:get_stacktrace(),
	    io:format("~n~p: Internal Error: ~P ~P~n",[?MODULE,Reason,30,ST,30]),
	    {error, {?MODULE,Reason}}
    after 
	zlib:close(Z)
    end.

get_chunk(Pos,Chunks,#png{z=Z}) ->
    case Chunks of
	<<_:Pos/binary, Sz:32, Type:4/binary, Chunk:Sz/binary, Crc:32, _/binary>> ->
	    Pos1=Pos+4, Sz1=4+Sz,
	    <<_:Pos1/binary,CRCdata:Sz1/binary,_/binary>> = Chunks,
	    check_crc(CRCdata,Crc,Z),
	    {Pos+12+Sz,{binary_to_list(Type),Chunk}};
	_ -> 
	    throw(unsupported_format)
    end.

decode_chunks(Pos,Chunks,_PNG) when Pos >= size(Chunks) -> 
    throw(unsupported_format);
decode_chunks(Pos,Chunks,PNG0) ->
    {NewPos,Chunk} = get_chunk(Pos,Chunks,PNG0),
    case decode_chunk(Chunk,PNG0) of
	#png{} = PNG ->  
	    decode_chunks(NewPos,Chunks,PNG);
	#e3d_image{} = Image ->
	    Image
    end.

decode_chunk({"IHDR",Header},PNG0) ->
    case Header of
	<<W:32,H:32,Bpc:8,ColT:8,0:8,0:8,IL:8>> ->
	    PNG0#png{w=W,h=H,bpc=Bpc,type=ColT,interlace=IL};
	_ ->
	    throw(unsupported_format)
    end;
decode_chunk({"IDAT", Chunk},PNG0=#png{chunks=Prev}) ->
    PNG0#png{chunks=[Chunk|Prev]};
decode_chunk({"IEND",_Chunk},PNG0=#png{chunks=Prev}) ->
    create_image(PNG0#png{chunks=lists:reverse(Prev)});
decode_chunk({"PLTE",Chunk},PNG0) ->
    PNG0#png{palette=Chunk};
decode_chunk({"bKGD",Chunk},PNG0=#png{type=Type}) ->
    Color = 
	case Chunk of
	    <<G:16>> when Type == ?GREYSCALE -> 
		GR = rescale(G,16,8),
		[GR,GR,GR,0];
	    <<R:16,G:16,B:16>> when Type == ?TRUECOLOUR_A -> 
		[rescale(R,16,8),rescale(G,16,8),rescale(B,16,8),0];
	    <<Ind:8>> when Type == ?INDEXED -> 
		Pos = Ind*3,
		<<_:Pos/binary,R:8,G:8,B:8,_/binary>> = PNG0#png.palette,
		[R,G,B,0];
	    _ -> 
		[0,0,0,0]
	end,
    PNG0#png{bkgd=Color};
decode_chunk({"tRNS",Chunk},PNG0=#png{type=Type}) ->
    Color = 
	case Chunk of
	    <<G:16>> when Type == ?GREYSCALE -> 
		rescale(G,16,8);
	    <<R:16,G:16,B:16>> when Type == ?TRUECOLOUR -> 
		[rescale(R,16,8),rescale(G,16,8),rescale(B,16,8)];
	    _ when Type == ?INDEXED -> 
		Chunk
	end,
    PNG0#png{trns=Color};
decode_chunk({_Type,_Chunk},PNG0) ->
%%    io:format("Skipped ~s ~n", [Type]),
    PNG0.

check_crc(Data,Crc,Z) ->   
    case zlib:crc32(Z,Data) of
	Crc -> ok;
	_E ->
	    throw(decode_error)
    end.
     
create_image(P=#png{w=W,h=H,chunks=Chunks}) ->
    Image0 = zlib:uncompress(list_to_binary(Chunks)),
    Image1  = unfilter(Image0,P#png{chunks=used}),
    {#png{restype=ResType},Image} = convert(Image1,P),
%%     io:format("Sz Uncompressed ~p Unfiltered ~p Converted ~p~n", 
%% 	      [size(Image0),size(Image1),size(Image)]),
    #e3d_image{width=W,height=H,alignment=1,
	       type=ResType,bytes_pp=e3d_image:bytes_pp(ResType),
	       order=upper_left,image=Image}.

merge_interlace(R,_C,_W,H,_BSz,_All,[],Acc) when R >= H ->
    Res = list_to_binary(lists:reverse(Acc)),
    Res;
merge_interlace(R,C,W,H,BSz,All,RA,Acc) when C >= W ->
    Row0 = list_to_binary(lists:reverse(RA)),
    RL = W*BSz,
    RowLen = if (RL rem 8) == 0 -> RL div 8;true -> RL div 8 + 1 end,
    %% Remove extra columns added by the interlacing
    <<Row:RowLen/binary,_/binary>> = Row0,
    merge_interlace(R+1,0,W,H,BSz,All,[],[Row|Acc]);
merge_interlace(R,C,W,H,BSz,All=[P1,P2,_P3,P4,_P5,P6,_P7],RA,Acc) 
  when (R rem 8) == 0, C < W ->    
    Blocks = gurka,
    [P1c1] = get_pixels(R,8,C,1,BSz,Blocks,P1),
    [P6c1,P6c2,P6c3,P6c4] = get_pixels(R,2,C,4,BSz,Blocks,P6),
    [P4c1,P4c2] = get_pixels(R,4,C,2,BSz,Blocks,P4),
    [P2c1] = get_pixels(R,8,C,1,BSz,Blocks,P2),
    Part = il_create_row([P1c1,P6c1,P4c1,P6c2,P2c1,P6c3,P4c2,P6c4],BSz),
    merge_interlace(R,C+8,W,H,BSz,All,[Part|RA],Acc);
merge_interlace(R,0,W,H,BSz,All=[_,_,_,_,_,_,{BytPR,P7}],[],Acc) 
  when (R rem 2) == 1 ->
    P7S = (R div 2)*BytPR,
    P7Row = case P7 of
		<<_:P7S/binary,Row:BytPR/binary,_/binary>> -> Row;
		<<_:P7S/binary,Row/binary>> ->  Row
	    end,
    merge_interlace(R,W,W,H,BSz,All,[P7Row],Acc);
merge_interlace(R,C,W,H,BSz,All=[_P1,_P2,_P3,_P4,P5,P6,_P7],RA,Acc) 
  when (R rem 4) == 2 ->
    Blocks = if W < 8 -> 1; true -> W div 8 end,
    [P5c1,P5c2,P5c3,P5c4] = get_pixels(R,4,C,4,BSz,Blocks,P5),
    [P6c1,P6c2,P6c3,P6c4] = get_pixels(R,2,C,4,BSz,Blocks,P6),
    Part = il_create_row([P5c1,P6c1,P5c2,P6c2,P5c3,P6c3,P5c4,P6c4],BSz),
    merge_interlace(R,C+8,W,H,BSz,All,[Part|RA],Acc);
merge_interlace(R,C,W,H,BSz,All=[_P1,_P2,P3,P4,_P5,P6,_P7],RA,Acc) 
  when (R rem 8) == 4 ->
    Blocks = if W < 8 -> 1; true -> W div 8 end,
    [P3c1,P3c2] = get_pixels(R,8,C,2,BSz,Blocks,P3),
    [P6c1,P6c2,P6c3,P6c4] = get_pixels(R,2,C,4,BSz,Blocks,P6),
    [P4c1,P4c2] = get_pixels(R,4,C,2,BSz,Blocks,P4),
    Part = il_create_row([P3c1,P6c1,P4c1,P6c2,P3c2,P6c3,P4c2,P6c4],BSz),
    merge_interlace(R,C+8,W,H,BSz,All,[Part|RA],Acc).

il_create_row(B,BSz) when (BSz rem 8) == 0 ->
    list_to_binary(B);
il_create_row([C1,C2,C3,C4,C5,C6,C7,C8],Sz) ->
%    io:format("~.16B~.16B~.16B~.16B~.16B~.16B~.16B~.16B ",[C1,C2,C3,C4,C5,C6,C7,C8]),
    <<C1:Sz,C2:Sz,C3:Sz,C4:Sz,C5:Sz,C6:Sz,C7:Sz,C8:Sz>>.

get_pixels(R,RD,C,CM,BSz,_Tot,{RowBytes,Bin}) ->
    BI = C div 8,
    Row = (R div RD)*RowBytes,
    Col = BI*CM,
    BitPos = Row*8 + Col*BSz,
    {SBy,SBi} = {BitPos div 8,BitPos rem 8},
%    io:format("~p: ~p => ~p ~p ~n", [[R,RD,C,CM,BSz,RowBytes,Bin],[], SBy,SBi]),
    Bits = CM*BSz,
    case Bits rem 8 of
	0 ->
	    Bytes = Bits div 8,
	    case Bin of 
		<<_:SBy/binary,Cols:Bytes/binary,_/binary>> ->		
		    split_cols(Cols,CM,BSz);
		_ ->
		    lists:duplicate(CM,0)
	    end;
	_ ->
	    Skip = (8-((Bits+SBi) rem 8)) rem 8,
	    case Bin of
		<<_:SBy/binary,_:SBi,Cols:Bits,_:Skip,_/binary>> ->
		    split_cols(Cols,CM,BSz);
		_ -> %% Outside Image return pad bits
		    lists:duplicate(CM,0)
	    end
    end.

split_cols(Col,1,_) -> [Col];
split_cols(Col,2,Bsz) when Bsz rem 8 == 0 ->
    By = Bsz div 8,
    <<C1:By/binary,C2:By/binary>> = Col,
    [C1,C2];
split_cols(Col,4,Bsz) when Bsz rem 8 == 0 ->
    By = Bsz div 8,
    <<C1:By/binary,C2:By/binary,C3:By/binary,C4:By/binary>> = Col,
    [C1,C2,C3,C4];
split_cols(Byte,2,Bsz) when is_binary(Byte) ->
    <<C1:Bsz,C2:Bsz>> = Byte,
    [C1,C2];
split_cols(Byte,2,Bsz) ->
    M = (1 bsl Bsz)-1,
    [Byte bsr Bsz,Byte band M];
split_cols(Byte,4,Bsz) when is_binary(Byte) ->
    <<C1:Bsz,C2:Bsz,C3:Bsz,C4:Bsz>> = Byte,
    [C1,C2,C3,C4];
split_cols(Byte,4,Bsz) ->
    M = (1 bsl Bsz)-1,
    [Byte bsr (Bsz*4-Bsz),
     (Byte bsr (Bsz*4-2*Bsz)) band M, 
     (Byte bsr (Bsz*4-3*Bsz)) band M, 
     Byte band M].

il_pass_pixels(1,WR,HR) -> {1,(WR+7) div 8,1,(HR +7) div 8};
il_pass_pixels(2,WR,HR) -> {1,if WR > 4 -> 1; true -> 0 end,1,(HR+7) div 8};
il_pass_pixels(3,WR,HR) -> {2,(WR+3) div 4,1,HR div 5};
il_pass_pixels(4,WR,HR) -> {2,(WR+1) div 4,2,(HR+3) div 4};
il_pass_pixels(5,WR,HR) -> {4,(WR +1) div 2,2,(HR+1) div 4};
il_pass_pixels(6,WR,HR) -> {4,WR div 2,4,(HR+1) div 2};
il_pass_pixels(7,WR,HR) -> {8,WR,4,HR div 2}.

il_bytes_psl(Pass,P=#png{w=W,h=H}) ->
    WBlocks = W div 8,
    HBlocks = H div 8,
    {Bpix,Epix,HBl,EBl} = il_pass_pixels(Pass, W rem 8, H rem 8),
    WPixels = (WBlocks*Bpix+Epix),
    RowBytes = case WPixels*pixelsz(P) of
		   RB when trunc(RB) == RB -> trunc(RB);
		   RB -> trunc(RB) +1 
	       end,
    Rows = HBlocks*HBl+EBl,
    RowsWithFilter = if RowBytes > 0 -> RowBytes + 1; true -> 0 end,
%    io:format("Pass ~p ~p~n", [Pass,{RowBytes,Rows,Rows*RowsWithFilter}]),
    {RowBytes,Rows,Rows*RowsWithFilter}.

unfilter(_Uncompressed,#png{interlace=I}) when I > 1 ->
    throw(bad_file);
unfilter(Uncompressed,P = #png{w=W,h=H,interlace=1}) ->
%    io:format("Interlaced ~p Sz ~p ~n", [P, size(Uncompressed)]),
    {P1Sl,_,P1Sz} = il_bytes_psl(1,P),
    {P2Sl,_,P2Sz} = il_bytes_psl(2,P),
    {P3Sl,_,P3Sz} = il_bytes_psl(3,P),
    {P4Sl,_,P4Sz} = il_bytes_psl(4,P),
    {P5Sl,_,P5Sz} = il_bytes_psl(5,P),
    {P6Sl,_,P6Sz} = il_bytes_psl(6,P),
    {P7Sl,_,P7Sz} = il_bytes_psl(7,P),
%%     io:format("Sizes ~p ~p ~n",[[P1Sz,P2Sz,P3Sz,P4Sz,P5Sz,P6Sz,P7Sz],
%%  				P1Sz+P2Sz+P3Sz+P4Sz+P5Sz+P6Sz+P7Sz]),
    FPSz = case trunc(pixelsz(P)) of 0 -> 1; PtSz -> PtSz end,
    Filters = {FPSz,unfilters(FPSz)},
    case Uncompressed of
	<<P1:P1Sz/binary,P2:P2Sz/binary,P3:P3Sz/binary,
	 P4:P4Sz/binary,P5:P5Sz/binary,P6:P6Sz/binary,P7:P7Sz/binary>> ->
	    Def = [{P1Sl,P1},{P2Sl,P2},{P3Sl,P3},{P4Sl,P4},
		   {P5Sl,P5},{P6Sl,P6},{P7Sl,P7}],
	    Filtered = lists:map(fun({SL,Pass}) ->
					 ScanBits = SL *8,
					 Prev = binary_to_list(<<0:ScanBits>>),
					 {SL,unfilter(0,Pass,Prev,Filters,SL,[])}
				 end,Def),
	    PSz = trunc(pixelsz(P)*8),
	    merge_interlace(0,0,W,H,PSz,Filtered,[],[]);
	_ -> 
	    exit(size_calc_wrong)
    end;

unfilter(Uncompressed,P) ->
    ScanLen = scanlen(P),
%    io:format("Scanlen ~p ~p Sz ~p ~n", [ScanLen,P, size(Uncompressed)]),
    Sz = case trunc(pixelsz(P)) of 0 -> 1; PSz -> PSz end,
    ScanBits = (ScanLen)*8,
    Prev = binary_to_list(<<0:ScanBits>>),
    unfilter(0,Uncompressed,Prev,{Sz,unfilters(Sz)},ScanLen,[]).
unfilter(Row,Uncompressed,Prev,I={Sz,Filter},ScanLen,Acc) ->
    Skip = Row*(ScanLen+1),
    case Uncompressed of
	<<_:Skip/binary,FilterIdx:8,Curr0:ScanLen/binary,_/binary>> ->
	    Curr = binary_to_list(Curr0),
%%	    io:format("~p: Filter ~p ",[Row,FilterIdx]),
	    Filtered = 
		case FilterIdx of
		    0 -> Curr;
		    1 -> sub_filter(Curr,Sz,element(FilterIdx,Filter),[]);
		    2 -> up_filter(Prev,Curr,[]); 
		    3 -> average_filter(Prev,Curr,Sz,element(FilterIdx,Filter),[]);
		    4 -> paeth_filter(Prev,Curr,Sz,element(FilterIdx,Filter),[])
		end,
	    unfilter(Row+1,Uncompressed,Filtered,I,ScanLen,
		     [list_to_binary(Filtered)|Acc]);
	_ ->
	    list_to_binary(lists:reverse(Acc))
    end.

unfilters(1) ->{fun sub_filter1/2,up_filter,fun 
		average_filter1/3,fun paeth_filter1/3};
unfilters(2) ->{fun sub_filter2/2,up_filter,
		fun average_filter2/3,fun paeth_filter2/3};
unfilters(3) ->{fun sub_filter3/2,up_filter,
		fun average_filter3/3,fun paeth_filter3/3};
unfilters(4) ->{fun sub_filter4/2,up_filter,
		fun average_filter4/3,fun paeth_filter4/3};
unfilters(6) ->{fun sub_filter6/2,up_filter,
		fun average_filter6/3,fun paeth_filter6/3};
unfilters(8) ->{fun sub_filter8/2,up_filter,
		fun average_filter8/3,fun paeth_filter8/3}.

paeth_filter8([C1,C2,C3,C4,C5,C6,C7,C8|Prev = [B1,B2,B3,B4,B5,B6,B7,B8|_]],
	      [X1,X2,X3,X4,X5,X6,X7,X8|Curr],
	      Acc=[A8,A7,A6,A5,A4,A3,A2,A1|_])  ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(A2,B2,C2,X2),
    RX3 = paeth_filter(A3,B3,C3,X3),
    RX4 = paeth_filter(A4,B4,C4,X4),
    RX5 = paeth_filter(A5,B5,C5,X5),
    RX6 = paeth_filter(A6,B6,C6,X6),
    RX7 = paeth_filter(A7,B7,C7,X7),
    RX8 = paeth_filter(A8,B8,C8,X8),
    paeth_filter8(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter8(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,8,fun paeth_filter8/3,Acc).
paeth_filter6([C1,C2,C3,C4,C5,C6|Prev = [B1,B2,B3,B4,B5,B6|_]],
	      [X1,X2,X3,X4,X5,X6|Curr],
	      Acc=[A6,A5,A4,A3,A2,A1|_]) ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(A2,B2,C2,X2),
    RX3 = paeth_filter(A3,B3,C3,X3),
    RX4 = paeth_filter(A4,B4,C4,X4),
    RX5 = paeth_filter(A5,B5,C5,X5),
    RX6 = paeth_filter(A6,B6,C6,X6),
    paeth_filter6(Prev,Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter6(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,6,fun paeth_filter6/3,Acc).
paeth_filter4([C1,C2,C3,C4,B1,B2,B3,B4|Prev = [B5,B6,B7,B8|_]],
	      [X1,X2,X3,X4,X5,X6,X7,X8|Curr],
	      Acc=[A4,A3,A2,A1|_]) ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(A2,B2,C2,X2),
    RX3 = paeth_filter(A3,B3,C3,X3),
    RX4 = paeth_filter(A4,B4,C4,X4),
    RX5 = paeth_filter(RX1,B5,B1,X5),
    RX6 = paeth_filter(RX2,B6,B2,X6),
    RX7 = paeth_filter(RX3,B7,B3,X7),
    RX8 = paeth_filter(RX4,B8,B4,X8),
    paeth_filter4(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter4(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,4,fun paeth_filter4/3,Acc).
paeth_filter3([C1,C2,C3,B1,B2,B3|Prev = [B4,B5,B6|_]],
	      [X1,X2,X3,X4,X5,X6|Curr],
	      Acc=[A3,A2,A1,_,_,_|_]) ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(A2,B2,C2,X2),
    RX3 = paeth_filter(A3,B3,C3,X3),
    RX4 = paeth_filter(RX1,B4,B1,X4),
    RX5 = paeth_filter(RX2,B5,B2,X5),
    RX6 = paeth_filter(RX3,B6,B3,X6),
    paeth_filter3(Prev,Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter3(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,3,fun paeth_filter3/3,Acc).
paeth_filter2([C1,C2,B1,B2,B3,B4,B5,B6|Prev = [B7,B8|_]],
	      [X1,X2,X3,X4,X5,X6,X7,X8|Curr],
	      Acc=[A2,A1|_]) ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(A2,B2,C2,X2),
    RX3 = paeth_filter(RX1,B3,B1,X3),
    RX4 = paeth_filter(RX2,B4,B2,X4),
    RX5 = paeth_filter(RX3,B5,B3,X5),
    RX6 = paeth_filter(RX4,B6,B4,X6),
    RX7 = paeth_filter(RX5,B7,B5,X7),
    RX8 = paeth_filter(RX6,B8,B6,X8),
    paeth_filter2(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter2(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,2,fun paeth_filter2/3,Acc).
paeth_filter1([C1,B1,B2,B3,B4,B5,B6,B7|Prev = [B8|_]],
	      [X1,X2,X3,X4,X5,X6,X7,X8|Curr],
	      Acc=[A1|_]) ->
    RX1 = paeth_filter(A1,B1,C1,X1),
    RX2 = paeth_filter(RX1,B2,B1,X2),
    RX3 = paeth_filter(RX2,B3,B2,X3),
    RX4 = paeth_filter(RX3,B4,B3,X4),
    RX5 = paeth_filter(RX4,B5,B4,X5),
    RX6 = paeth_filter(RX5,B6,B5,X6),
    RX7 = paeth_filter(RX6,B7,B6,X7),
    RX8 = paeth_filter(RX7,B8,B7,X8),
    paeth_filter1(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
paeth_filter1(Prev,Curr,Acc) ->
    paeth_filter(Prev,Curr,1,fun paeth_filter1/3,Acc).

paeth_filter(Prev0=[C|Prev],[X|Curr],Sz,Fun,Acc) ->
    case pget(Sz,Acc) of
	none -> 
	    Pos = length(Acc)+1,
	    B = pget(Pos,Prev0),
	    RX = paeth_filter(0,B,0,X),
	    Fun(Prev0,Curr,[RX|Acc]);
	A ->
	    B = pget(Sz,Prev),
	    RX = paeth_filter(A,B,C,X),
	    Fun(Prev,Curr,[RX|Acc])
    end;
paeth_filter(_,[],_Sz,_Fun,Acc) ->
    lists:reverse(Acc).

paeth_filter(A,B,C,X) ->
    P = A + B - C,
    PA = abs(P - A),
    PB = abs(P - B),
    PC = abs(P - C),
    if PA =< PB, PA =< PC -> (A+X) band 255;
       PB =< PC -> (B+X) band 255;
       true -> (C+X) band 255
    end.

sub_filter8([X1,X2,X3,X4,X5,X6,X7,X8|Curr],
	    Acc=[A8,A7,A6,A5,A4,A3,A2,A1|_]) ->
    RX1 = (X1+A1) band 255,  RX2 = (X2+A2) band 255,
    RX3 = (X3+A3) band 255,  RX4 = (X4+A4) band 255,
    RX5 = (X5+A5) band 255,  RX6 = (X6+A6) band 255,
    RX7 = (X7+A7) band 255,  RX8 = (X8+A8) band 255,
    sub_filter8(Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter8(Curr,Acc) ->
    sub_filter(Curr,8,fun sub_filter8/2,Acc).
sub_filter6([X1,X2,X3,X4,X5,X6|Curr],Acc=[A6,A5,A4,A3,A2,A1|_]) ->
    RX1 = (X1+A1) band 255,  RX2 = (X2+A2) band 255,
    RX3 = (X3+A3) band 255,  RX4 = (X4+A4) band 255,
    RX5 = (X5+A5) band 255,  RX6 = (X6+A6) band 255,
    sub_filter6(Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter6(Curr,Acc) ->
    sub_filter(Curr,6,fun sub_filter6/2,Acc).
sub_filter4([X1,X2,X3,X4,X5,X6,X7,X8|Curr],Acc=[A4,A3,A2,A1|_]) ->
    RX1 = (X1+A1) band 255,    RX2 = (X2+A2) band 255,
    RX3 = (X3+A3) band 255,    RX4 = (X4+A4) band 255,
    RX5 = (X5+RX1) band 255,   RX6 = (X6+RX2) band 255,
    RX7 = (X7+RX3) band 255,   RX8 = (X8+RX4) band 255,
    sub_filter4(Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter4(Curr,Acc) ->
    sub_filter(Curr,4,fun sub_filter4/2,Acc).
sub_filter3([X1,X2,X3,X4,X5,X6|Curr],Acc=[A3,A2,A1|_]) ->
    RX1 = (X1+A1) band 255,    RX2 = (X2+A2) band 255,
    RX3 = (X3+A3) band 255,    RX4 = (X4+RX1) band 255,
    RX5 = (X5+RX2) band 255,   RX6 = (X6+RX3) band 255,
    sub_filter3(Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter3(Curr,Acc) ->
    sub_filter(Curr,3,fun sub_filter3/2,Acc).
sub_filter2([X1,X2,X3,X4,X5,X6,X7,X8|Curr],Acc=[A2,A1|_]) ->
    RX1 = (X1+A1) band 255,    RX2 = (X2+A2) band 255,
    RX3 = (X3+RX1) band 255,   RX4 = (X4+RX2) band 255,
    RX5 = (X5+RX3) band 255,   RX6 = (X6+RX4) band 255,
    RX7 = (X7+RX5) band 255,   RX8 = (X8+RX6) band 255,
    sub_filter2(Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter2(Curr,Acc) ->
    sub_filter(Curr,2,fun sub_filter2/2,Acc).
sub_filter1([X1,X2,X3,X4,X5,X6,X7,X8|Curr],Acc=[A1|_]) ->
    RX1 = (X1+A1) band 255,    RX2 = (X2+RX1) band 255,
    RX3 = (X3+RX2) band 255,   RX4 = (X4+RX3) band 255,
    RX5 = (X5+RX4) band 255,   RX6 = (X6+RX5) band 255,
    RX7 = (X7+RX6) band 255,   RX8 = (X8+RX7) band 255,
    sub_filter1(Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
sub_filter1(Curr,Acc) ->
    sub_filter(Curr,1,fun sub_filter1/2,Acc).

sub_filter([X|Curr],Sz,Fun,Acc) ->
    case pget(Sz,Acc) of
	none ->
	    Fun(Curr,[X|Acc]);
	A ->
	    RX = (X+A) band 255,
	    Fun(Curr,[RX|Acc])
    end;
sub_filter([],_,_,Acc) ->
    lists:reverse(Acc).

up_filter([B1,B2,B3,B4,B5,B6,B7,B8|Prev],[X1,X2,X3,X4,X5,X6,X7,X8|Curr],Acc) ->
    RX1 = (X1+B1) band 255,    RX2 = (X2+B2) band 255,
    RX3 = (X3+B3) band 255,    RX4 = (X4+B4) band 255,
    RX5 = (X5+B5) band 255,    RX6 = (X6+B6) band 255,
    RX7 = (X7+B7) band 255,    RX8 = (X8+B8) band 255,
    up_filter(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
up_filter([B1|Prev],[X1|Curr],Acc) ->
    RX = (X1+B1) band 255,
    up_filter(Prev,Curr,[RX|Acc]);
up_filter(__Prev,[],Acc) ->
    lists:reverse(Acc).

average_filter8([B1,B2,B3,B4,B5,B6,B7,B8|Prev],
		[X1,X2,X3,X4,X5,X6,X7,X8|Curr],
		Acc=[A8,A7,A6,A5,A4,A3,A2,A1|_])  ->
    RX1 = (X1+trunc((B1+A1)/2)) band 255,    RX2 = (X2+trunc((B2+A2)/2)) band 255,
    RX3 = (X3+trunc((B3+A3)/2)) band 255,    RX4 = (X4+trunc((B4+A4)/2)) band 255,
    RX5 = (X5+trunc((B5+A5)/2)) band 255,    RX6 = (X6+trunc((B6+A6)/2)) band 255,
    RX7 = (X7+trunc((B7+A7)/2)) band 255,    RX8 = (X8+trunc((B8+A8)/2)) band 255,
    average_filter8(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter8(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,8,fun average_filter8/3,Acc).
average_filter6([B1,B2,B3,B4,B5,B6|Prev],
		[X1,X2,X3,X4,X5,X6|Curr],
		Acc=[A6,A5,A4,A3,A2,A1|_])  ->
    RX1 = (X1+trunc((B1+A1)/2)) band 255, RX2 = (X2+trunc((B2+A2)/2)) band 255,
    RX3 = (X3+trunc((B3+A3)/2)) band 255, RX4 = (X4+trunc((B4+A4)/2)) band 255,
    RX5 = (X5+trunc((B5+A5)/2)) band 255, RX6 = (X6+trunc((B6+A6)/2)) band 255,
    average_filter6(Prev,Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter6(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,6,fun average_filter6/3,Acc).
average_filter4([B1,B2,B3,B4,B5,B6,B7,B8|Prev],
		[X1,X2,X3,X4,X5,X6,X7,X8|Curr],
		Acc=[A4,A3,A2,A1|_]) ->
    RX1 = (X1+trunc((B1+A1) /2)) band 255, RX2 = (X2+trunc((B2+A2) /2)) band 255,
    RX3 = (X3+trunc((B3+A3) /2)) band 255, RX4 = (X4+trunc((B4+A4) /2)) band 255,
    RX5 = (X5+trunc((B5+RX1)/2)) band 255, RX6 = (X6+trunc((B6+RX2)/2)) band 255,
    RX7 = (X7+trunc((B7+RX3)/2)) band 255, RX8 = (X8+trunc((B8+RX4)/2)) band 255,
    average_filter4(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter4(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,4,fun average_filter4/3,Acc).
average_filter3([B1,B2,B3,B4,B5,B6|Prev],
		[X1,X2,X3,X4,X5,X6|Curr],
		Acc=[A3,A2,A1|_])  ->
    RX1 = (X1+trunc((B1+A1) /2))band 255, RX2 = (X2+trunc((B2+A2) /2))band 255,
    RX3 = (X3+trunc((B3+A3) /2))band 255, RX4 = (X4+trunc((B4+RX1)/2)) band 255,
    RX5 = (X5+trunc((B5+RX2)/2)) band 255,RX6 = (X6+trunc((B6+RX3)/2)) band 255,
    average_filter3(Prev,Curr,[RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter3(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,3,fun average_filter3/3,Acc).
average_filter2([B1,B2,B3,B4,B5,B6,B7,B8|Prev],
		[X1,X2,X3,X4,X5,X6,X7,X8|Curr],
		Acc=[A2,A1|_])  ->
    RX1 = (X1+trunc((B1+A1) /2)) band 255, RX2 = (X2+trunc((B2+A2) /2)) band 255,
    RX3 = (X3+trunc((B3+RX1)/2)) band 255, RX4 = (X4+trunc((B4+RX2)/2)) band 255,
    RX5 = (X5+trunc((B5+RX3)/2)) band 255, RX6 = (X6+trunc((B6+RX4)/2)) band 255,
    RX7 = (X7+trunc((B7+RX5)/2)) band 255, RX8 = (X8+trunc((B8+RX6)/2)) band 255,
    average_filter2(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter2(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,2,fun average_filter2/3,Acc).
average_filter1([B1,B2,B3,B4,B5,B6,B7,B8|Prev],
		[X1,X2,X3,X4,X5,X6,X7,X8|Curr],
		Acc=[A1|_])  ->
    RX1 = (X1+trunc((B1+A1) /2)) band 255, RX2 = (X2+trunc((B2+RX1)/2)) band 255,
    RX3 = (X3+trunc((B3+RX2)/2)) band 255, RX4 = (X4+trunc((B4+RX3)/2)) band 255,
    RX5 = (X5+trunc((B5+RX4)/2)) band 255, RX6 = (X6+trunc((B6+RX5)/2)) band 255,
    RX7 = (X7+trunc((B7+RX6)/2)) band 255, RX8 = (X8+trunc((B8+RX7)/2)) band 255,
    average_filter1(Prev,Curr,[RX8,RX7,RX6,RX5,RX4,RX3,RX2,RX1|Acc]);
average_filter1(Prev,Curr,Acc) ->
    average_filter(Prev,Curr,1,fun average_filter1/3,Acc).

average_filter([B|Prev],[X|Curr],Sz,Fun,Acc) ->
    case pget(Sz,Acc) of
	none ->
	    RX = (X + trunc(B/2)) band 255;
	A ->
	    RX = (X + trunc((A+B)/2)) band 255
    end,
    Fun(Prev,Curr,[RX|Acc]);
average_filter(_,[],_,_,Acc) ->
    lists:reverse(Acc).

pget(1,[A|_]) -> A;  
pget(2,[_,A|_]) -> A; 
pget(3,[_,_,A|_]) -> A; 
pget(4,[_,_,_,A|_]) -> A;  
pget(5,[_,_,_,_,A|_]) -> A;
pget(6,[_,_,_,_,_,A|_]) -> A;  
pget(7,[_,_,_,_,_,_,A|_]) -> A;  
pget(8,[_,_,_,_,_,_,_,A|_]) -> A; 
pget(_,_) -> none.

pixelsz(#png{type=Type,bpc=Bpc}) ->
    PixelBitSz
	= case Type of
	      ?GREYSCALE    -> 1*Bpc;
	      ?GREYSCALE_A  -> 2*Bpc;
	      ?TRUECOLOUR   -> 3*Bpc;
	      ?TRUECOLOUR_A -> 4*Bpc;
	      ?INDEXED      -> 1*Bpc
	  end,
    PixelBitSz / 8.
    
scanlen(P=#png{w=W}) -> 
    Len = W * pixelsz(P),
    if trunc(Len) == Len ->
	    trunc(Len);
       true ->
	    trunc(Len) + 1
    end.

output_type(#png{type=?GREYSCALE,  trns=undefined}) -> g8;
output_type(#png{type=?TRUECOLOUR, trns=undefined}) -> r8g8b8;
output_type(_) -> r8g8b8a8.

convert(Image, P= #png{type=?INDEXED,trns=Trns}) ->
    Type = if Trns == undefined -> r8g8b8;
	      true -> r8g8b8a8
	   end,
    Png = P#png{restype=Type},
    {Png,convert_pal(0,Image,Png,[])};
convert(Image, P=#png{trns=Trns,bkgd=BG}) ->
    TrnsInfo = if Trns == undefined -> undefined;
		  true -> {Trns,BG}
	       end,
    Png = P#png{restype=output_type(P)},
    {Png,convert(Image,TrnsInfo,Png)}.

convert(Image0, Trns, #png{bpc=8,type=?GREYSCALE}) -> 
    add_transperancyG(Trns,Image0);
convert(Image0, Trns, P=#png{type=?GREYSCALE}) -> 
    add_transperancyG(Trns,convert(0,Image0,P,8,[]));
convert(Image0, _, #png{bpc=8,type=?GREYSCALE_A}) ->  
    ga2rgba(Image0);
convert(Image0, _, P=#png{type=?GREYSCALE_A}) -> 
    ga2rgba(convert(0,Image0,P,8,[]));
convert(Image0, Trns, #png{bpc=8}) -> 
    add_transperancyRGB(Trns,Image0);
convert(Image0, Trns, Png) -> 
    add_transperancyRGB(Trns,convert(0,Image0,Png,8,[])).

convert(Pos,Image,P = #png{bpc=From},To,Acc) ->
    case Image of
	<<_:Pos/binary,Data:?CHUNK/binary,_/binary>> ->
	    DL = binary_to_list(Data),
	    Res = convert_bytes(DL,From,To),
	    convert(Pos+?CHUNK,Image,P,To,[list_to_binary(Res)|Acc]);
	<<_:Pos/binary,Data/binary>> ->
	    DL = binary_to_list(Data),
	    Last = convert_bytes(DL,From,To),
	    list_to_binary(lists:reverse([Last|Acc]))
    end.

convert_bytes([B1,B2|T],16,8) -> 
    [rescale((B1 bsl 8) bor B2, 16,8)|convert_bytes(T,16,8)];
convert_bytes([B1|T],4,8) ->
    [rescale(?get4p1(B1),4,8),rescale(?get4p2(B1),4,8)|convert_bytes(T,4,8)];
convert_bytes([B1|T],2,8) -> 
    [rescale(?get2p1(B1),2,8),rescale(?get2p2(B1),2,8),
     rescale(?get2p3(B1),2,8),rescale(?get2p4(B1),2,8)
     |convert_bytes(T,2,8)];
convert_bytes([B1|T],1,8) -> 
    [rescale(?get1p1(B1),1,8),rescale(?get1p2(B1),1,8),
     rescale(?get1p3(B1),1,8),rescale(?get1p4(B1),1,8),
     rescale(?get1p5(B1),1,8),rescale(?get1p6(B1),1,8),
     rescale(?get1p7(B1),1,8),rescale(?get1p8(B1),1,8)
     |convert_bytes(T,1,8)];
convert_bytes([],_,_) -> [].
    
%% Depth rescaling should be done with!!
%%output = floor((input * MAXOUTSAMPLE / MAXINSAMPLE) + 0.5)
rescale(0,_,_) -> 0;
rescale(1,1,8) -> 255;
rescale(1,2,8) -> 85;
rescale(2,2,8) -> 170;
rescale(3,2,8) -> 255;
rescale(Input,Inbits,Outbits) ->
    trunc((Input*((1 bsl Outbits)-1)/((1 bsl Inbits)-1))+0.5).

ga2rgba(Bin) ->
    ga2rgba(0,Bin,[]).
ga2rgba(Pos,Bin,Acc) ->
    case Bin of
	<<_:Pos/binary,Data:?CHUNK/binary,_/binary>> ->
	    Res = do_ga2rgb(binary_to_list(Data)),
	    ga2rgba(Pos+?CHUNK,Bin,[list_to_binary(Res)|Acc]);
	<<_:Pos/binary,Data/binary>> ->
	    Last = do_ga2rgb(binary_to_list(Data)),
	    list_to_binary(lists:reverse([Last|Acc]))
    end.

do_ga2rgb([G,A|R]) -> [G,G,G,A|do_ga2rgb(R)];
do_ga2rgb([]) -> [].

add_transperancyG(undefined,Bin) -> 
    Bin;
add_transperancyG({Trns,BG},Bin) ->
    add_trnsG(0,Trns,BG,Bin,[]).
add_trnsG(Pos,Trns,BG,Bin,Acc) ->
    case Bin of
	<<_:Pos/binary,Data:?CHUNK/binary,_/binary>> ->
	    Res = do_addtrnsG(Trns,BG,binary_to_list(Data)),
	    add_trnsG(Pos+?CHUNK,Trns,BG,Bin,[list_to_binary(Res)|Acc]);
	<<_:Pos/binary,Data/binary>> ->
	    Last = do_addtrnsG(Trns,BG,binary_to_list(Data)),
	    list_to_binary(lists:reverse([Last|Acc]))
    end.
do_addtrnsG(Trns,BG,[Trns|R]) -> BG ++ do_addtrnsG(Trns,BG,R);
do_addtrnsG(Trns,BG,[G|R]) -> [G,G,G,255|do_addtrnsG(Trns,BG,R)];
do_addtrnsG(_,_,[]) -> [].

add_transperancyRGB(undefined,Bin) -> 
    Bin;
add_transperancyRGB({Trns,BG},Bin) ->    
    Res = add_trnsRGB(0,Trns,BG,Bin,[]),
    Res.
add_trnsRGB(Pos,Trns,BG,Bin,Acc) ->
    case Bin of
	<<_:Pos/binary,Data:?CHUNK/binary,_/binary>> ->
	    Res = do_addtrnsRGB(Trns,BG,binary_to_list(Data)),
	    add_trnsRGB(Pos+?CHUNK,Trns,BG,Bin,[list_to_binary(Res)|Acc]);
	<<_:Pos/binary,Data/binary>> ->
	    Last = do_addtrnsRGB(Trns,BG,binary_to_list(Data)),
	    list_to_binary(lists:reverse([Last|Acc]))
    end.

do_addtrnsRGB(Trns=[R,G,B],BG,[R,G,B|T]) -> BG ++ do_addtrnsRGB(Trns,BG,T);
do_addtrnsRGB(Trns,BG,[R,G,B|T]) -> [R,G,B,255|do_addtrnsRGB(Trns,BG,T)];
do_addtrnsRGB(_,_,[]) -> [].

convert_pal(Pos,Image,P=#png{w=W,bpc=Bpc,palette=Pal,trns=Trns,restype=ResType},Acc) ->
    ResW = e3d_image:bytes_pp(ResType)*W,
    ScanLen = scanlen(P),
    case Image of
	<<_:Pos/binary,Data:ScanLen/binary,_/binary>> ->
	    Res0 = list_to_binary(do_lookup_pal(binary_to_list(Data),Bpc,Pal,Trns)),
	    <<Res:ResW/binary, _G/binary>> = Res0,
%	    io:format("Cutting ~p ~p ~p bytes ~n",[size(Res0),size(Res),size(_G)]),
	    convert_pal(Pos+ScanLen,Image,P,[Res|Acc]);
	<<_:Pos/binary,_Pads/binary>> ->
%	    io:format("Skipping ~p bytes~n",[size(_Pads)]),
	    list_to_binary(lists:reverse(Acc))
    end.
do_lookup_pal([],_,_,_) -> [];
do_lookup_pal([Idx|Rest],8,Pal,Trns) ->
    [palette_color(Idx,Pal,Trns)|do_lookup_pal(Rest,8,Pal,Trns)];
do_lookup_pal([Idx|Rest],4,Pal,Trns) ->
    [palette_color(?get4p1(Idx),Pal,Trns),palette_color(?get4p2(Idx),Pal,Trns)|
     do_lookup_pal(Rest,4,Pal,Trns)];
do_lookup_pal([Idx|Rest],2,Pal,Trns) ->
    [palette_color(?get2p1(Idx),Pal,Trns),palette_color(?get2p2(Idx),Pal,Trns),
     palette_color(?get2p3(Idx),Pal,Trns),palette_color(?get2p4(Idx),Pal,Trns)|
     do_lookup_pal(Rest,2,Pal,Trns)];
do_lookup_pal([Idx|Rest],1,Pal,Trns) ->
    [palette_color(?get1p1(Idx),Pal,Trns),palette_color(?get1p2(Idx),Pal,Trns),
     palette_color(?get1p3(Idx),Pal,Trns),palette_color(?get1p4(Idx),Pal,Trns),
     palette_color(?get1p5(Idx),Pal,Trns),palette_color(?get1p6(Idx),Pal,Trns),
     palette_color(?get1p7(Idx),Pal,Trns),palette_color(?get1p8(Idx),Pal,Trns)|
     do_lookup_pal(Rest,1,Pal,Trns)].

palette_color(Int,Pal,TrnsMap) ->
    A = lookup_trns(Int,TrnsMap),
    Skip = Int*3,
    <<_:Skip/binary,R:8,G:8,B:8,_/binary>> = Pal,
    [R,G,B|A].
   
lookup_trns(_I,undefined) -> [];
lookup_trns(I,Map) ->
    case Map of 
	<<_:I/binary,A:8,_/binary>> -> [A];
	_ -> [255]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

save1(Orig=#e3d_image{},_Opts,Z) ->
    #e3d_image{width=W,height=H,type=T,image=Image,bytes_pp=Bpp} = 
	e3d_image:convert(Orig,rgb_order(Orig),1,upper_left),
    HDR = create_chunk(<<"IHDR",W:32,H:32,8:8,(png_type(T)):8,0:8,0:8,0:8>>,Z),
    DATA = create_chunk(["IDAT",compress_image(0,Bpp*W,Image,[])],Z),
    END  = create_chunk(<<"IEND">>,Z),
    list_to_binary([?MAGIC,HDR,DATA,END]).

compress_image(I,RowLen, Bin, Acc) ->
    Pos = I*RowLen,
    case Bin of
	<<_:Pos/binary,Row:RowLen/binary,_/binary>> ->
	    Filtered = filter_row(Row,RowLen),
	    compress_image(I+1,RowLen,Bin,[Filtered|Acc]);
	_ when Pos == size(Bin) ->
	    Filtered = list_to_binary(lists:reverse(Acc)),
	    Compressed = zlib:compress(Filtered),
	    Compressed
    end.

filter_row(Row,_RowLen) ->
    [0,Row].

png_type(g8) -> ?GREYSCALE;
png_type(a8) -> ?GREYSCALE;
png_type(r8g8b8) -> ?TRUECOLOUR;
png_type(r8g8b8a8) -> ?TRUECOLOUR_A.

rgb_order(#e3d_image{type=b8g8r8}) -> r8g8b8;
rgb_order(#e3d_image{type=b8g8r8a8}) -> r8g8b8a8;
rgb_order(#e3d_image{type=Type}) -> Type.
    
create_chunk(Bin,Z) when is_list(Bin) ->
    create_chunk(list_to_binary(Bin),Z);
create_chunk(Bin,Z) when is_binary(Bin) ->
    Sz = size(Bin)-4,
    Crc = zlib:crc32(Z,Bin),
    <<Sz:32,Bin/binary,Crc:32>>.

test() ->
    {ok, Fs} = file:list_dir("."),
    test2(Fs).
test(File) ->
    test2([File]).

test2([]) -> ok;
test2([File|Rest]) ->
    case lists:reverse(File) of 
	"gnp." ++ F ->
	    case catch load(File,[]) of
		Img = #e3d_image{} when hd(File) == $x ->
		    io:format("~n Didn't Fail with ~p ~p ~n~n",[File,Img]);
		Else when hd(File) == $x ->
		    io:format("~n Good Fail with ~p ~p ~n~n",[File,Else]),
		    test2(Rest);
		Img = #e3d_image{width=W,height=H,bytes_pp=Bpp,image=Image} 
		when W*H*Bpp==size(Image) ->
		    io:format("~n Loaded ~p ~n~n",[File]),
		    ok = e3d_image:save(Img, lists:reverse(F)++".tga"),
		    test2(Rest);
		#e3d_image{width=W,height=H,bytes_pp=Bpp,image=Image} ->
		    io:format("~n~p Failed: Size differ W*H*Bpp=~p*~p*~p=~p Isz=~p~n",
			      [File,W,H,Bpp, W*H*Bpp,size(Image)]);
	    
		{'EXIT',not_implemented} ->
		    io:format("~nNot implemented skipped ~p ~n~n",[File]),
		    test2(Rest);
		Else ->
		    io:format("~n ~p Failed with ~p~n~n",[File,Else])
	    end;
	_ ->
	    test2(Rest)
    end.

