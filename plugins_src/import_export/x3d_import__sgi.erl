%%
%%  x3d_import__sgi.erl --
%%
%%     SGI Raster Image Format Reader.
%%
%%  Copyright 2022-2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(x3d_import__sgi).
-export([load/1]).

%% File Extensions used:
%% .sgi
%%
%% In some contexts these file extensions can also be .sgi files:
%% .bw  .rgb  .rgba

-include_lib("wings/e3d/e3d_image.hrl").
-define(INT, big-integer-unsigned).

-define(LINT, little-integer-unsigned).

-record(sgiraster, {
    compressed :: integer(),
    bpp :: integer(),
    idim :: integer(),
    xsize :: integer(),
    ysize :: integer(),
    numchannels :: integer(),
    minpixel :: integer(),
    maxpixel :: integer(),
    name,
    colormapid :: integer()
}).

%% Load an SGI raster image file
%%
-spec load(file:name_all()) -> #e3d_image{} | {error, any()}.
load(Filename) ->
    case file:open(Filename, [binary,read]) of
        {ok, FH} ->
            case file:read(FH, 2) of
                {ok, <<16#01, 16#DA>>} ->
                    load_1(FH);
                {ok, _} ->
                    file:close(FH),
                    {error, wrong_format};
                {error, Err} ->
                    file:close(FH),
                    {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.
load_1(FH) ->
    case read_header(FH) of
        {ok, Hdr} ->
            Channels = read_image_data(FH, Hdr),
            file:close(FH),
            channels_to_e3dimage(Channels, Hdr);
        {error, Err} ->
            file:close(FH),
            {error, Err}
    end.
    
read_header(FH) ->
    case file:read(FH, 510) of
        {ok, <<
            Compressed:8/?INT,
            BPP:8/?INT,
            IDim:16/?INT,
            XSize:16/?INT,
            YSize:16/?INT,
            NumChannels:16/?INT,
            MinimumPixVal:32/?INT,
            MaximumPixVal:32/?INT,
            _Unused_0:4/unit:8,
            ImageName_0:80/unit:8,
            ColorMapID:32/?INT,
            _Unused_1:404/unit:8 >>
        } ->
        {ok, #sgiraster{
            compressed = Compressed,
            bpp = BPP,
            idim = IDim,
            xsize = XSize,
            ysize = YSize,
            numchannels = NumChannels,
            minpixel = MinimumPixVal,
            maxpixel = MaximumPixVal,
            name = ImageName_0,
            colormapid = ColorMapID
        }};
        {error, Err} ->
            {error, Err}
    end.

read_image_data(FH, #sgiraster{colormapid=ColorMapId}=Hdr) ->
    case ColorMapId of
        0 -> read_image_data_0(FH, Hdr, []);
        1 -> read_image_data_packed(FH, Hdr, []);
        2 -> read_image_data_screen(FH, Hdr, []);
        3 -> error
    end.
read_image_data_0(FH, #sgiraster{numchannels=NC,compressed=0}=Hdr, O) when NC > 0 ->
    Data = read_ch_scanlines_unc(FH, Hdr),
    read_image_data_0(FH, Hdr#sgiraster{numchannels=NC-1}, [Data|O]);
read_image_data_0(FH, #sgiraster{bpp=BPP,numchannels=NC,compressed=1,xsize=XSize,ysize=YSize}=_Hdr, O) when NC > 0 ->
    Offset = read_rle_offset_table(FH, NC, YSize),
    Sizes  = read_rle_size_table(FH, NC, YSize),
    case BPP of
        1 -> read_rle_image_1bpp(FH, NC, XSize, YSize, Offset, Sizes, O);
        2 -> read_rle_image_2bpp(FH, NC, XSize, YSize, Offset, Sizes, O)
    end;
read_image_data_0(_FH, #sgiraster{numchannels=NC}=_Hdr, O) when NC =:= 0 ->
    lists:reverse(O).

read_image_data_packed(FH, #sgiraster{numchannels=NC,xsize=XSize,ysize=YSize,compressed=0}=_Hdr, _O) when NC > 0 ->
    {ok, R,G,B} = read_ch_scanlines_packed_1(FH, XSize, YSize, [],[],[]),
    [R,G,B].

read_image_data_screen(FH, #sgiraster{numchannels=NC,compressed=0}=Hdr, O) ->
    if NC > 0 ->
        Data = read_ch_scanlines_unc(FH, Hdr),
        read_image_data_screen(FH, Hdr#sgiraster{numchannels=NC-1}, [Data|O]);
    true ->
        lists:reverse(O)
    end.



read_ch_scanlines_unc(FH, #sgiraster{bpp=BPP,xsize=XSize,ysize=YSize}=_Hdr) ->
    case BPP of
        1 -> read_ch_scanlines_unc_1bpp(FH, XSize, YSize, []);
        2 -> read_ch_scanlines_unc_2bpp(FH, XSize, YSize, [])
    end.
read_ch_scanlines_unc_1bpp(FH, XSize, YSize, O) ->
    if YSize > 0 ->
        {ok, Line} = read_byte_list(FH, XSize),
        read_ch_scanlines_unc_1bpp(FH, XSize, YSize-1, [Line|O]);
    true ->
        O % No need to reverse
    end.
read_ch_scanlines_unc_2bpp(FH, XSize, YSize, O) ->
    if YSize > 0 ->
        {ok, Line} = read_short_list(FH, XSize),
        read_ch_scanlines_unc_2bpp(FH, XSize, YSize-1, [Line|O]);
    true ->
        O % No need to reverse
    end.


read_rle_offset_table(FH, Ch, YSize) when is_integer(Ch) ->
    read_rle_offset_table(FH, Ch, YSize, []).
read_rle_offset_table(FH, Ch, YSize, O) ->
    if Ch > 0 ->
        {ok, List} = read_long_list(FH, YSize),
        read_rle_offset_table(FH, Ch-1, YSize, [List|O]);
    true ->
        lists:reverse(O)
    end.

read_rle_size_table(FH, Ch, YSize) ->
    read_rle_size_table(FH, Ch, YSize, []).
read_rle_size_table(FH, Ch, YSize, O) ->
    if Ch > 0 ->
        {ok, List} = read_long_list(FH, YSize),
        read_rle_size_table(FH, Ch-1, YSize, [List|O]);
    true ->
        lists:reverse(O)
    end.

read_rle_image_1bpp(FH, NC, XSize, YSize, [COff|Offset], [CSize|Sizes], O) ->
    if NC > 0 ->
        {ok, Plane} = read_rle_image_1bpp_ch(FH, XSize, YSize, COff, CSize, []),
        read_rle_image_1bpp(FH, NC-1, XSize, YSize, Offset, Sizes, [Plane|O]);
    true ->
        lists:reverse(O)
    end.

read_rle_image_2bpp(FH, NC, XSize, YSize, [COff|Offset], [CSize|Sizes], O) ->
    if NC > 0 ->
        {ok, Plane} = read_rle_image_2bpp_ch(FH, XSize, YSize, COff, CSize, []),
        read_rle_image_2bpp(FH, NC-1, XSize, YSize, Offset, Sizes, [Plane|O]);
    true ->
        lists:reverse(O)
    end.

read_rle_image_1bpp_ch(FH, XSize, YSize, [LOff|Offset], [LSize|Sizes], O) ->
    file:position(FH, {bof, LOff}),
    ScanLine = read_rle_line(FH, 1, LSize, XSize, []),
    read_rle_image_1bpp_ch(FH, XSize, YSize, Offset, Sizes, [ScanLine|O]);
read_rle_image_1bpp_ch(_FH, _XSize, _YSize, [], [], O) ->
    {ok, O}.

read_rle_image_2bpp_ch(FH, XSize, YSize, [LOff|Offset], [LSize|Sizes], O) ->
    file:position(FH, {bof, LOff}),
    ScanLine = read_rle_line(FH, 2, LSize, XSize, []),
    read_rle_image_2bpp_ch(FH, XSize, YSize, Offset, Sizes, [ScanLine|O]);
read_rle_image_2bpp_ch(_FH, _XSize, _YSize, [], [], O) ->
    {ok, O}.

%% RLE
read_rle_line(FH, BPP, SLSize, XSize, O) when SLSize > 0 ->
    case BPP of
        1 ->
            {ok, <<Count:8/?INT>>} = file:read(FH, 1);
        2 ->
            {ok, <<Count:16/?INT>>} = file:read(FH, 2)
    end,
    Count_1 = Count band 16#7F,
    if Count_1 > 0 ->
        if (Count band 16#80) > 0 ->
            case BPP of
                1 ->
                    {ok, Next} = read_byte_list(FH, Count_1),
                    BytesRead = 1 + Count_1;
                2 ->
                    {ok, Next} = read_short_list(FH, Count_1),
                    BytesRead = 1 + Count_1
            end,
            read_rle_line(FH, BPP, SLSize-BytesRead, XSize - Count_1, [Next|O]);
        true ->
            case BPP of
                1 ->
                    {ok, <<Value:8/?INT>>} = file:read(FH, 1),
                    BytesRead = 2;
                2 ->
                    {ok, <<Value:16/?INT>>} = file:read(FH, 2),
                    BytesRead = 4
            end,
            read_rle_line(FH, BPP, SLSize-BytesRead, XSize - Count_1, [lists:duplicate(Count_1, Value)|O])
        end;
    true ->
        read_rle_line_fill(FH, XSize, O)
    end;
read_rle_line(FH, _BPP, 0, XSize, O) ->
    read_rle_line_fill(FH, XSize, O).
read_rle_line_fill(FH, XSize, O) ->
    if XSize > 0 ->
        read_rle_line_fill(FH, XSize-1, [[0] | O]);
    true ->
        lists:append(lists:reverse(O))
    end.
    
read_byte_list(FH, N) ->
    {ok, Bytes} = file:read(FH, N),
    {ok, binary_to_list(Bytes)}.

read_short_list(FH, N) ->
    {ok, Data} = file:read(FH, N bsl 1),
    read_short_list(Data, N, []).
read_short_list(<<Samp:16/?INT, R/binary>>, N, O) ->
    if N > 0 ->
        read_short_list(R, N-1, [Samp|O]);
    true ->
        {ok, lists:reverse(O)}
    end.

read_long_list(FH, N) ->
    {ok, Data} = file:read(FH, N bsl 2),
    read_long_list(Data, N, []).
read_long_list(<<Samp:32/?INT, R/binary>>, N, O) ->
    if N > 0 ->
        read_long_list(R, N-1, [Samp|O]);
    true ->
        {ok, lists:reverse(O)}
    end.

read_ch_scanlines_packed_1(FH, XSize, YSize, OR,OG,OB) ->
    if YSize > 0 ->
        {ok, LR,LG,LB} = read_ch_scanlines_packed_1_l(FH, XSize, [],[],[]),
        read_ch_scanlines_packed_1(FH, XSize, YSize-1, [LR|OR],[LG|OG],[LB|OB]);
    true ->
        {ok, OR,OG,OB}
    end.

read_ch_scanlines_packed_1_l(FH, XSize, OR,OG,OB) ->
    if XSize > 0 ->
        {ok, <<R:3,G:3,B:2>>} = file:read(FH, 1),
        read_ch_scanlines_packed_1_l(FH, XSize-1, [R*85|OR],[G*85|OG],[B*127|OB]);
    true ->
        {ok, lists:reverse(OR),lists:reverse(OG),lists:reverse(OB)}
    end.


channels_to_e3dimage(Channels, #sgiraster{bpp=1,xsize=Width,ysize=Height}=_Hdr) when length(Channels) > 0 ->
    case Channels of
        [GC] -> Data = ch_to_rgba8(GC, []);
        [GC,AC] -> Data = ch_to_rgba8(GC,AC, []);
        [RC,GC,BC] -> Data = ch_to_rgba8(RC,GC,BC, []);
        [RC,GC,BC,AC|_] -> Data = ch_to_rgba8(RC,GC,BC,AC, [])
    end,
    #e3d_image{
        type = b8g8r8a8,
        bytes_pp = 4,
        alignment = 1,
        order = lower_left,
        width = Width,
        height = Height,
        image = iolist_to_binary(Data)
    };
channels_to_e3dimage(Channels, #sgiraster{bpp=2,xsize=Width,ysize=Height}=_Hdr) when length(Channels) > 0 ->
    case Channels of
        [GC] -> Data = ch_to_rgba16(GC, []);
        [GC,AC] -> Data = ch_to_rgba16(GC,AC, []);
        [RC,GC,BC] -> Data = ch_to_rgba16(RC,GC,BC, []);
        [RC,GC,BC,AC|_] -> Data = ch_to_rgba16(RC,GC,BC,AC, [])
    end,
    #e3d_image{
        type = b16g16r16a16,
        bytes_pp = 4,
        alignment = 1,
        order = lower_left,
        width = Width,
        height = Height,
        image = iolist_to_binary(Data)
    };
channels_to_e3dimage([], _Hdr) ->
    error.

%%% 8 bit interleave
%%%

%% Interleave 1 channel
ch_to_rgba8([G|GC], OL) ->
    ch_to_rgba8(GC, [ch_sl_to_rgba8(G,[])|OL]);
ch_to_rgba8([], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba8([G|GC], OL) ->
    ch_sl_to_rgba8(GC, [<<G,G,G,255>>|OL]);
ch_sl_to_rgba8([], OL) ->
    lists:reverse(OL).

%% Interleave 2 channels
ch_to_rgba8([G|GC],[A|AC], OL) ->
    ch_to_rgba8(GC,AC, [ch_sl_to_rgba8(G,A,[])|OL]);
ch_to_rgba8([],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba8([G|GC],[A|AC], OL) ->
    ch_sl_to_rgba8(GC,AC, [<<G,G,G,A>>|OL]);
ch_sl_to_rgba8([],[], OL) ->
    lists:reverse(OL).

%% Interleave 3 channels
ch_to_rgba8([R|RC],[G|GC],[B|BC], OL) ->
    ch_to_rgba8(RC,GC,BC, [ch_sl_to_rgba8(R,G,B,[])|OL]);
ch_to_rgba8([],[],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba8([R|RC],[G|GC],[B|BC], OL) ->
    ch_sl_to_rgba8(RC,GC,BC, [<<R,G,B,255>>|OL]);
ch_sl_to_rgba8([],[],[], OL) ->
    lists:reverse(OL).

%% Interleave 4 channels 
ch_to_rgba8([R|RC],[G|GC],[B|BC],[A|AC], OL) ->
    ch_to_rgba8(RC,GC,BC,AC, [ch_sl_to_rgba8(R,G,B,A,[])|OL]);
ch_to_rgba8([],[],[],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba8([R|RC],[G|GC],[B|BC],[A|AC], OL) ->
    ch_sl_to_rgba8(RC,GC,BC,AC, [<<R,G,B,A>>|OL]);
ch_sl_to_rgba8([],[],[],[], OL) ->
    lists:reverse(OL).

%%% 16 bit interleave
%%%

%% Interleave 1 channel
ch_to_rgba16([G|GC], OL) ->
    ch_to_rgba16(GC, [ch_sl_to_rgba16(G,[])|OL]);
ch_to_rgba16([], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba16([G|GC], OL) ->
    ch_sl_to_rgba16(GC, [<<G:16/?LINT,G:16/?LINT,G:16/?LINT,255:16/?LINT>>|OL]);
ch_sl_to_rgba16([], OL) ->
    lists:reverse(OL).

%% Interleave 2 channels
ch_to_rgba16([G|GC],[A|AC], OL) ->
    ch_to_rgba16(GC,AC, [ch_sl_to_rgba16(G,A,[])|OL]);
ch_to_rgba16([],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba16([G|GC],[A|AC], OL) ->
    ch_sl_to_rgba16(GC,AC, [<<G:16/?LINT,G:16/?LINT,G:16/?LINT,A:16/?LINT>>|OL]);
ch_sl_to_rgba16([],[], OL) ->
    lists:reverse(OL).

%% Interleave 3 channels
ch_to_rgba16([R|RC],[G|GC],[B|BC], OL) ->
    ch_to_rgba16(RC,GC,BC, [ch_sl_to_rgba16(R,G,B,[])|OL]);
ch_to_rgba16([],[],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba16([R|RC],[G|GC],[B|BC], OL) ->
    ch_sl_to_rgba16(RC,GC,BC, [<<R:16/?LINT,G:16/?LINT,B:16/?LINT,255:16/?LINT>>|OL]);
ch_sl_to_rgba16([],[],[], OL) ->
    lists:reverse(OL).

%% Interleave 4 channels 
ch_to_rgba16([R|RC],[G|GC],[B|BC],[A|AC], OL) ->
    ch_to_rgba16(RC,GC,BC,AC, [ch_sl_to_rgba16(R,G,B,A,[])|OL]);
ch_to_rgba16([],[],[],[], OL) ->
    lists:reverse(OL).
ch_sl_to_rgba16([R|RC],[G|GC],[B|BC],[A|AC], OL) ->
    ch_sl_to_rgba16(RC,GC,BC,AC, [<<R:16/?LINT,G:16/?LINT,B:16/?LINT,A:16/?LINT>>|OL]);
ch_sl_to_rgba16([],[],[],[], OL) ->
    lists:reverse(OL).
