%%
%%  e3d__dds.erl --
%%
%%     Functions for reading .dds files.
%%
%%  Copyright (c) 2018 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(e3d__dds).
-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

-export([load/2, save/3]).
-export([format_error/1]).
-include("e3d_image.hrl").

% debug
-export([rgb16/1,rgb32/1,rgb16to32/1, bc1/1, bc2/1]).

format_error(unsupported_format) ->
    "Unsupported format or bad DDS file";
format_error({unsupported_format, _Line}) ->
    "Unsupported format or bad DDS file";
format_error(bad_image_specifiction) ->
    "Bad image specification".

-define(DW, 32/little-unsigned).
-define(PF_SIZE, (4*8)).

-define(IS(Flags, Flag), ((Flags band (Flag)) > 0)).

-define(ERROR(What), throw({What, ?LINE})).

save(#e3d_image{type=InType}=Image0, FileName, _Opts) ->
    Image = e3d_image:convert(Image0, InType, 1, lower_left),
    try
        IoData = save_1(Image),
        file:write_file(FileName, IoData)
    catch throw:Error -> {error, Error};
          _:Error ->
            io:format("~p ~p ~p~n", [?MODULE, Error, erlang:get_stacktrace()]),
            {error, internal_error}
    end.

save_1(#e3d_image{width=W, height=H, type=Type, bytes_pp=Bpp, image=Img})
  when Bpp =:= 3; Bpp =:= 4 ->
    Pitch = W*Bpp,
    Flags = 16#1 bor 16#2 bor 16#4 bor 16#1000,
    PFBin = case Type of
                r8g8b8a8 ->
                    PFFlags = 16#1 bor 16#40,
                    RM = 16#FF, GM = 16#FF00, BM = 16#FF0000, AM = 16#FF000000,
                    <<32:?DW, PFFlags:?DW, 0:?DW, (Bpp*8):?DW, RM:?DW, GM:?DW, BM:?DW, AM:?DW>>;
                r8g8b8 ->
                    PFFlags = 16#40,
                    RM = 16#FF, GM = 16#FF00, BM = 16#FF0000, AM = 16#FF000000,
                    <<32:?DW, PFFlags:?DW, 0:?DW, (Bpp*8):?DW, RM:?DW, GM:?DW, BM:?DW, AM:?DW>>;
                b8g8r8a8 ->
                    PFFlags = 16#1 bor 16#40,
                    RM = 16#FF0000, GM = 16#FF00, BM = 16#FF, AM = 16#FF000000,
                    <<32:?DW, PFFlags:?DW, 0:?DW, (Bpp*8):?DW, RM:?DW, GM:?DW, BM:?DW, AM:?DW>>;
                b8g8r8 ->
                    PFFlags = 16#40,
                    RM = 16#FF0000, GM = 16#FF00, BM = 16#FF, AM = 16#FF000000,
                    <<32:?DW, PFFlags:?DW, 0:?DW, (Bpp*8):?DW, RM:?DW, GM:?DW, BM:?DW, AM:?DW>>
            end,
    32 = byte_size(PFBin),
    Header = <<"DDS ", 124:?DW, Flags:?DW,
               H:?DW, W:?DW, Pitch:?DW,
               0:?DW, 1:?DW, 0:(11*4*8),
               PFBin:?PF_SIZE/binary,
               16#1000:?DW, 0:?DW, 0:?DW, 0:?DW, 0:?DW
             >>,
    124 = byte_size(Header) - 4,
    [Header, Img].


load(FileName, _Opts) ->
    try case file:read_file(FileName) of
            {ok, <<"DDS ", 124:?DW, Flags:?DW, H:?DW, W:?DW, PorLS:?DW,
                   _Depth:?DW, MipMapN:?DW, _:(11*4)/binary,
                   PFBin:?PF_SIZE/binary,
                   Caps:?DW, Caps2:?DW, _:?DW, _:?DW, _:?DW,
                   Rest/binary>>} ->
                Im0 = #{w=>W, h=>H},
                Im1 = parse_pitch(Flags, PorLS, Im0),
                Im2 = parse_pf(PFBin, Im1),
                Im3 = parse_caps(Caps, Caps2, MipMapN, Im2),
                {Bin, Im4} = parse_dxt10(Rest, Im3),
                get_images(Bin, Im4);
            Error ->
                Error
        end
    catch throw:Err -> Err
    end.

parse_pitch(Flags, PitchOrLs, Im0) ->
    if ?IS(Flags, 16#8) -> Im0#{pitch=>PitchOrLs};
       ?IS(Flags, 16#80000) -> Im0#{linearsz=>PitchOrLs};
       true -> Im0
    end.

parse_pf(<<32:?DW,Flags:?DW,FCC:4/binary,BitC:?DW,RM:?DW,GM:?DW,BM:?DW,AM:?DW>>, Im0) ->
    if ?IS(Flags,16#2) -> ?ERROR(unsupported_format);
       ?IS(Flags,16#200) -> ?ERROR(unsupported_format);
       ?IS(Flags,16#20000) -> ?ERROR(unsupported_format);
       true -> ok
    end,
    PF0 = if ?IS(Flags,16#1) -> Im0#{a_mask=>AM};
             true -> Im0
          end,
    PF1 = if ?IS(Flags,16#04) -> PF0#{fourCC => FCC};
             true -> PF0
          end,
    PF2 = if ?IS(Flags,16#40) -> PF1#{bytes_pp=>BitC div 8, r_mask=>RM, g_mask=>GM, b_mask=>BM};
             true -> PF1
          end,
    PF2;
parse_pf(_, _) -> ?ERROR(unsupported_format).

parse_caps(Caps1, Caps2, MipMapN, Im0) ->
    Im = if ?IS(Caps1, 16#400000) -> Im0#{mipmaps=>MipMapN};
            true -> Im0#{mipmaps=>1}
         end,
    All = 16#400 bor 16#800 bor 16#1000 bor 16#2000 bor 16#4000 bor 16#8000,
    if ?IS(Caps2, 16#200), ?IS(Caps2, All) -> Im#{cubemap=>true};
       ?IS(Caps2, 16#200) -> ?ERROR(unsupported_format);
       true -> Im#{cubemap=>false}
    end.

parse_dxt10(<<Format:?DW, Dim:?DW, Flags1:?DW, Sz:?DW, _Flags2:?DW, Rest/binary>>,
            #{fourCC := <<"DX10">>}=Im) ->
    if Dim =/= 3 -> ?ERROR(unsupported_format);
       Sz =/= 1 -> ?ERROR(unsupported_format);
       true -> ok
    end,
    Cubemap = ?IS(Flags1, 16#4),
    {Rest, Im#{fourCC:=dxgi_format(Format), cubemap=>Cubemap, count=>Sz}};
%% Convert old fourCC to newer format
parse_dxt10(Rest, #{fourCC := <<"DXT", Variant:8>>}=Im) ->
    Alg = case Variant of
              $1 -> {bc1, uint};
              $2 -> {bc2, uint};
              $3 -> {bc2, uint};
              $4 -> {bc3, uint};
              $5 -> {bc3, uint}
          end,
    {Rest, Im#{fourCC:=Alg}};
parse_dxt10(Rest, #{fourCC := <<"ATI1">>}=Im) ->
    {Rest, Im#{fourCC:={bc4, uint}}};
parse_dxt10(Rest, #{fourCC := <<"BC4U">>}=Im) ->
    {Rest, Im#{fourCC:={bc4, uint}}};
parse_dxt10(Rest, #{fourCC := <<"BC4S">>}=Im) ->
    {Rest, Im#{fourCC:={bc4, sint}}};
parse_dxt10(Rest, #{fourCC := <<"ATI2">>}=Im) ->
    {Rest, Im#{fourCC:={bc5, uint}}};
parse_dxt10(Rest, #{fourCC := <<"BC5U">>}=Im) ->
    {Rest, Im#{fourCC:={bc5, uint}}};
parse_dxt10(Rest, #{fourCC := <<"BC5S">>}=Im) ->
    {Rest, Im#{fourCC:={bc5, sint}}};
%% Non-spec usage
parse_dxt10(Rest, #{fourCC := <<"RGBG">>}=Im) ->
    {Rest, Im#{fourCC:={r8g8_b8g8,uint}}};
parse_dxt10(Rest, #{fourCC := <<"GRGB">>}=Im) ->
    {Rest, Im#{fourCC:={g8r8_g8b8,uint}}};
parse_dxt10(Rest, #{fourCC := <<Enum:32/unsigned-little>>}=Im) ->
    %% From https://docs.microsoft.com/en-us/windows/uwp/gaming/complete-code-for-ddstextureloader
    Format = case Enum of
                 36 -> %% D3DFMT_A16B16G16R16
                     dxgi_format(11); %DXGI_FORMAT_R16G16B16A16_UNORM
                 110 -> %% D3DFMT_Q16W16V16U16
                     dxgi_format(13); %DXGI_FORMAT_R16G16B16A16_SNORM;
                 111 -> %% D3DFMT_R16F
                     dxgi_format(54); % DXGI_FORMAT_R16_FLOAT;
                 112 -> %% D3DFMT_G16R16F
                     dxgi_format(34); % DXGI_FORMAT_R16G16_FLOAT;
                 113 -> %% D3DFMT_A16B16G16R16F
                     dxgi_format(11); % DXGI_FORMAT_R16G16B16A16_FLOAT;
                 114 -> %% D3DFMT_R32F
                     dxgi_format(41); % DXGI_FORMAT_R32_FLOAT;
                 115 -> %% D3DFMT_G32R32F
                     dxgi_format(16); % DXGI_FORMAT_R32G32_FLOAT;
                 116 -> %% D3DFMT_A32B32G32R32F
                     dxgi_format(2); % DXGI_FORMAT_R32G32B32A32_FLOAT;
                 _ ->
                     dxgi_format(0)
             end,
    {Rest, Im#{fourCC:=Format}};
parse_dxt10(Rest, #{fourCC := _}=Im) ->
    {Rest, Im};
parse_dxt10(Rest, Im) ->
    {Rest, Im#{fourCC=>none}}.

get_images(Bin, #{w:=W, h:=H, mipmaps:=MM, cubemap:=false}=Opts) ->
    {Type, Bpp, Div, BSz, Decomp} = comp_alg(Opts),
    {[{Image,_,_,_}|MMs],_Pad} = get_mipmaps(Bin, W, H, Div, BSz, 0, MM-1, Decomp, []),
    #e3d_image{width=W, height=H, bytes_pp=Bpp, type=Type,
               order=upper_left, alignment=1, image=Image,
               extra=add_mm(MMs)};

get_images(Bin0, #{w:=W, h:=H, mipmaps:=MM, cubemap:=true} = Opts) ->
    {Type, Bpp, Div, BSz, Decomp} = comp_alg(Opts),
    Get = fun(Dir, Bin) ->
                  {[{Image,_,_,_}|MMs], Rest} = get_mipmaps(Bin, W, H, Div, BSz, 0, MM-1, Decomp, []),
                  {#{dir=>Dir, tx=>Image, mipmaps=>MMs}, Rest}
          end,
    {CubeTx, _Pad} = lists:mapfoldl(Get, Bin0, [pos_x, neg_x, pos_y, neg_y, pos_z, neg_z]),
    [#{dir:=pos_x,tx:=Image, mipmaps:=Mipmaps}|Rest] = CubeTx,
    #e3d_image{width=W, height=H, bytes_pp=Bpp, type=Type,
               order=upper_left, alignment=1, image=Image,
               extra=[{cubemaps, Rest}|add_mm(Mipmaps)]}.

get_mipmaps(Bin, W, H, Div, BSz, Level, Level, Decomp, Acc) ->
    Sz = b_size(W, H, Div, BSz),
    <<MM:Sz/binary, Rest/binary>> = Bin,
    Image = Decomp(MM,W,H),
    {lists:reverse([{Image,W,H,Level}|Acc]), Rest};
get_mipmaps(Bin, W, H, Div, BSz, Level, Max, Decomp, Acc) when Level < Max ->
    Sz = b_size(W, H, Div, BSz),
    <<MM:Sz/binary, Rest/binary>> = Bin,
    Image = Decomp(MM,W,H),
    get_mipmaps(Rest, W div 2, H div 2, Div, BSz, Level+1, Max, Decomp,
                [{Image,W,H,Level}|Acc]).

b_size(W, H, Div, BSz) ->
    ((W+Div-1) div Div) * ((H+Div-1) div Div) *BSz.

add_mm([]) -> [];
add_mm(MMs) -> [{mipmaps, MMs}].

type(#{b_mask:=16#FF, a_mask:=_, bytes_pp:=4}) -> b8g8r8a8;
type(#{r_mask:=16#FF, a_mask:=_, bytes_pp:=4}) -> r8g8b8a8;
type(#{b_mask:=16#FF, bytes_pp:=3}) -> b8g8r8;
type(#{r_mask:=16#FF, bytes_pp:=3}) -> r8g8b8;
type({Atom, uint}) -> Atom;
type({Atom, undefined}) -> Atom;  %% Just keep it (we don't know)
type({Atom, sint})  -> list_to_atom(atom_to_list(Atom) ++ "s");
type({Atom, float}) -> list_to_atom(atom_to_list(Atom) ++ "f");
type(_) -> ?ERROR(unsupported_format).

comp_alg(#{fourCC:={bc1, uint}}) ->
    Expand = fun(Block) -> bc1(Block) end,
    Decompress = fun(Compressed, MmW, MmH) ->
                         uncompress(Compressed, MmW, MmH, 8, Expand, 32)
                 end,
    {r8g8b8a8, 4, 4, 8, Decompress};
comp_alg(#{fourCC:={bc2, uint}}) ->
    Expand = fun(Block) -> bc2(Block) end,
    Decompress = fun(Compressed, MmW, MmH) ->
                         uncompress(Compressed, MmW, MmH, 16, Expand, 32)
                 end,
    {r8g8b8a8, 4, 4, 16, Decompress};
comp_alg(#{fourCC:={bc3, uint}}) ->
    Expand = fun(Block) -> bc3(Block) end,
    Decompress = fun(Compressed, MmW, MmH) ->
                         uncompress(Compressed, MmW, MmH, 16, Expand, 32)
                 end,
    {r8g8b8a8, 4, 4, 16, Decompress};
comp_alg(#{fourCC:={bc4, Signed}}) ->
    {Expand, Type} = case Signed of
                         uint -> {fun(Block) -> bc4(Block) end, g8};
                         sint -> {fun(Block) -> bc4s(Block) end, g8s}
                     end,
    Decompress = fun(Compressed, MmW, MmH) ->
                         uncompress(Compressed, MmW, MmH, 8, Expand, 8)
                 end,
    {Type, 1, 4, 8, Decompress};
comp_alg(#{fourCC:={bc5, Signed}}) ->
    {Expand, Type} = case Signed of
                         uint -> {fun(Block) -> bc5(Block) end, r8g8};
                         sint -> {fun(Block) -> bc5s(Block) end, r8g8s}
                     end,
    Decompress = fun(Compressed, MmW, MmH) ->
                         uncompress(Compressed, MmW, MmH, 16, Expand, 16)
                 end,
    {Type, 2, 4, 16, Decompress};

%% Not compressed
comp_alg(#{fourCC:={r16g16b16a16,_}=T}) ->
    {type(T), 8, 1, 8, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r32g32b32a32,_}=T}) ->
    {type(T), 16, 1, 16, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r32g32b32,_}=T}) ->
    {type(T), 12, 1, 12, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r8g8b8a8,_}=T}) ->
    {type(T), 4, 1, 4, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={b8g8r8a8,_}}) ->
    {b8g8r8a8, 4, 1, 4, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={b8g8r8x8,_}}) ->
    {b8g8r8a8, 4, 1, 4, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r8g8,_}=T}) ->
    {type(T), 2, 1, 2, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r32,_}=T}) ->
    {type(T), 4, 1, 4, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r16,_}=T}) ->
    {type(T), 2, 1, 2, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={r8,_}=T}) ->
    {type(T), 1, 1, 1, fun(Orig, _, _) -> Orig end};
comp_alg(#{fourCC:={a8,_}=T}) ->
    {type(T), 1, 1, 1, fun(Orig, _, _) -> Orig end};

comp_alg(#{fourCC:=none, bytes_pp:=Bpp} = Opts) ->
    {type(Opts), Bpp, 1, Bpp, fun(Orig, _, _) -> Orig end};

comp_alg(Variant) ->
    io:format("~p: unsupported_format / compression: ~p~n",[?MODULE, Variant]),
    ?ERROR(unsupported_compression).

uncompress(Compressed, W, H, BSz, Expand, BiPP) ->
    CRowSz = ((W+3) div 4) * BSz,
    CRows = [CRow || <<CRow:CRowSz/binary>> <= Compressed],
    Decomp = fun(CRow, Rows) ->
                     Uncomp = [Expand(Bits) || <<Bits:BSz/binary>> <= CRow],
                     join_rows(Uncomp, Rows, BiPP)
             end,
    Decompressed = lists:foldl(Decomp, <<>>, CRows),
    %% Crop image to actual size (for non multiple of 4 pixels images or mipmaps)
    ImgSz = W*H*(BiPP div 8),
    <<Img:ImgSz/binary, _/binary>> = Decompressed,
    Img.

join_rows(R4x4, Prev, BiPP) ->
    [R1,R2,R3,R4] = join_block(R4x4, <<>>, <<>>, <<>>, <<>>, BiPP),
    <<Prev/binary, R1/binary, R2/binary, R3/binary, R4/binary>>.

join_block([[C00,C01,C02,C03, C10,C11,C12,C13, C20,C21,C22,C23, C30,C31,C32,C33]|Tl],
           R1,R2,R3,R4, BiPP) ->
    join_block(Tl,
               <<R1/binary, C00:BiPP,C01:BiPP,C02:BiPP,C03:BiPP>>,
               <<R2/binary, C10:BiPP,C11:BiPP,C12:BiPP,C13:BiPP>>,
               <<R3/binary, C20:BiPP,C21:BiPP,C22:BiPP,C23:BiPP>>,
               <<R4/binary, C30:BiPP,C31:BiPP,C32:BiPP,C33:BiPP>>, BiPP);
join_block([], R1,R2,R3,R4, _) ->
    [R1,R2,R3,R4].

-define(EXP5TO8R(Col16),((((Col16) bsr 8) band 16#f8) bor (((Col16) bsr 13) band 16#7))).
-define(EXP6TO8G(Col16),((((Col16) bsr 3) band 16#fc) bor (((Col16) bsr  9) band 16#3))).
-define(EXP5TO8B(Col16),((((Col16) bsl 3) band 16#f8) bor (((Col16) bsr  2) band 16#7))).

-define(RGB(R,G,B), ((R bsl 24) bor (G bsl 16) bor (B bsl 8) bor 255)).
-define(RGB(R,G,B,A), ((R bsl 24) bor (G bsl 16) bor (B bsl 8) bor A)).

bc1(<<C0:16/unsigned-little, C1:16/unsigned-little,
       Inds:32/unsigned-little>>) ->
    Cs = case C0 > C1 of
             true ->
                 C2R = (2*?EXP5TO8R(C0)+?EXP5TO8R(C1)) div 3,
                 C2G = (2*?EXP6TO8G(C0)+?EXP6TO8G(C1)) div 3,
                 C2B = (2*?EXP5TO8B(C0)+?EXP5TO8B(C1)) div 3,
                 C3R = (?EXP5TO8R(C0)+2*?EXP5TO8R(C1)) div 3,
                 C3G = (?EXP6TO8G(C0)+2*?EXP6TO8G(C1)) div 3,
                 C3B = (?EXP5TO8B(C0)+2*?EXP5TO8B(C1)) div 3,
                 {?RGB(?EXP5TO8R(C0), ?EXP6TO8G(C0), ?EXP5TO8B(C0)),
                  ?RGB(?EXP5TO8R(C1), ?EXP6TO8G(C1), ?EXP5TO8B(C1)),
                  ?RGB(C2R,C2G,C2B),
                  ?RGB(C3R,C3G,C3B)};
             false ->
                 C2R = (?EXP5TO8R(C0)+?EXP5TO8R(C1)) div 2,
                 C2G = (?EXP6TO8G(C0)+?EXP6TO8G(C1)) div 2,
                 C2B = (?EXP5TO8B(C0)+?EXP5TO8B(C1)) div 2,
                 {?RGB(?EXP5TO8R(C0), ?EXP6TO8G(C0), ?EXP5TO8B(C0)),
                  ?RGB(?EXP5TO8R(C1), ?EXP6TO8G(C1), ?EXP5TO8B(C1)),
                  ?RGB(C2R,C2G,C2B),
                  0}
         end,
    bc1(Inds, 16, Cs).

bc1(I, N, Cs) when N > 0 ->
    [element((I band 3)+1, Cs) | bc1(I bsr 2, N-1, Cs)];
bc1(_, 0, _) -> [].

bc2(<<A:64/unsigned-little,
       C0:16/unsigned-little, C1:16/unsigned-little,
       Inds:32/unsigned-little>>) ->
    C2R = (2*?EXP5TO8R(C0)+?EXP5TO8R(C1)) div 3,
    C2G = (2*?EXP6TO8G(C0)+?EXP6TO8G(C1)) div 3,
    C2B = (2*?EXP5TO8B(C0)+?EXP5TO8B(C1)) div 3,
    C3R = (?EXP5TO8R(C0)+2*?EXP5TO8R(C1)) div 3,
    C3G = (?EXP6TO8G(C0)+2*?EXP6TO8G(C1)) div 3,
    C3B = (?EXP5TO8B(C0)+2*?EXP5TO8B(C1)) div 3,
    Cs = {?RGB(?EXP5TO8R(C0), ?EXP6TO8G(C0), ?EXP5TO8B(C0)),
          ?RGB(?EXP5TO8R(C1), ?EXP6TO8G(C1), ?EXP5TO8B(C1)),
          ?RGB(C2R,C2G,C2B),
          ?RGB(C3R,C3G,C3B)},
    bc2(Inds, A, 16, Cs).

bc2(I, A0, N, Cs) when N > 0 ->
    A = A0 band 15,
    RGBA = element((I band 3)+1, Cs) band (((1 bsl 24)-1 bsl 8) bor (A bsl 4 bor A)),
    [RGBA | bc2(I bsr 2, A0  bsr 4, N-1, Cs)];
bc2(_, _, 0, _) -> [].

bc3(<<AC0:8, AC1:8, AInds:48/unsigned-little,
       C0:16/unsigned-little, C1:16/unsigned-little,
       Inds:32/unsigned-little>>) ->
    C2R = (2*?EXP5TO8R(C0)+?EXP5TO8R(C1)) div 3,
    C2G = (2*?EXP6TO8G(C0)+?EXP6TO8G(C1)) div 3,
    C2B = (2*?EXP5TO8B(C0)+?EXP5TO8B(C1)) div 3,
    C3R = (?EXP5TO8R(C0)+2*?EXP5TO8R(C1)) div 3,
    C3G = (?EXP6TO8G(C0)+2*?EXP6TO8G(C1)) div 3,
    C3B = (?EXP5TO8B(C0)+2*?EXP5TO8B(C1)) div 3,
    Cs = {?RGB(?EXP5TO8R(C0), ?EXP6TO8G(C0), ?EXP5TO8B(C0)),
          ?RGB(?EXP5TO8R(C1), ?EXP6TO8G(C1), ?EXP5TO8B(C1)),
          ?RGB(C2R,C2G,C2B),
          ?RGB(C3R,C3G,C3B)},
    As = as(AC0,AC1),
    bc3(Inds, AInds, 16, Cs, As).

bc3(I, AI, N, Cs, As) when N > 0 ->
    A = element((AI band 7)+1, As),
    [element((I band 3)+1, Cs) band (((1 bsl 24)-1 bsl 8) bor A) |
     bc3(I bsr 2, AI bsr 3, N-1, Cs, As)];
bc3(_, _, _, _, _) ->
    [].

bc4(<<AC0:8/unsigned, AC1:8/unsigned, AInds:48/unsigned-little>>) ->
    As = as(AC0,AC1),
    bc4(AInds, 16, As).
bc4s(<<AC0:8/signed, AC1:8/signed, AInds:48/unsigned-little>>) ->
    As = as(AC0,AC1),
    bc4(AInds, 16, As).

bc4(AI, N, As) when N > 0 ->
    A = element((AI band 7)+1, As),
    [A | bc4(AI bsr 3, N-1, As)];
bc4(_, _, _) ->
    [].

bc5(<<AC0:8/unsigned, AC1:8/unsigned, AInds1:48/unsigned-little,
      AC3:8/unsigned, AC4:8/unsigned, AInds2:48/unsigned-little>>) ->
    As1 = as(AC0,AC1),
    As2 = as(AC3,AC4),
    bc5(AInds1, AInds2, 16, As1, As2).
bc5s(<<AC0:8/signed, AC1:8/signed, AInds1:48/unsigned-little,
       AC3:8/signed, AC4:8/signed, AInds2:48/unsigned-little>>) ->
    As1 = as(AC0,AC1),
    As2 = as(AC3,AC4),
    bc5(AInds1, AInds2, 16, As1, As2).

bc5(A1, B1, N, As1, As2) when N > 0 ->
    A = element((A1 band 7)+1, As1),
    B = element((B1 band 7)+1, As2),
    [(A bsl 8) bor B | bc5(A1 bsr 3, B1 bsr 3, N-1, As1, As2)];
bc5(_, _, _, _, _) ->
    [].

as(AC0, AC1) when AC0 > AC1 ->
    {AC0, AC1, (6*AC0+1*AC1) div 7, (5*AC0+2*AC1) div 7,
     (4*AC0+3*AC1) div 7, (3*AC0+4*AC1) div 7,
     (2*AC0+5*AC1) div 7, (1*AC0+6*AC1) div 7};
as(AC0, AC1) ->
    {AC0, AC1, (4*AC0+1*AC1) div 5, (3*AC0+2*AC1) div 5,
     (2*AC0+3*AC1) div 5, (1*AC0+4*AC1) div 5, 0, 255}.

%% Debug
rgb16(<<RGB:16/unsigned-little>>) ->
    rgb16(RGB);
rgb16(RGB) ->
    {?EXP5TO8R(RGB), ?EXP6TO8G(RGB), ?EXP5TO8B(RGB)}.

rgb32({R,G,B}) -> <<?RGB(R,G,B):32>>.

rgb16to32(RGB) ->
    rgb32(rgb16(RGB)).

dxgi_format(Format) ->
    case Format of
        0 -> {unknown, undefined};            % DXGI_FORMAT_UNKNOWN                     = 0,
        1 -> {r32g32b32a32,undefined};        % DXGI_FORMAT_R32G32B32A32_TYPELESS       = 1,
        2 -> {r32g32b32a32,float};            % DXGI_FORMAT_R32G32B32A32_FLOAT          = 2,
        3 -> {r32g32b32a32,uint};             % DXGI_FORMAT_R32G32B32A32_UINT           = 3,
        4 -> {r32g32b32a32,sint};             % DXGI_FORMAT_R32G32B32A32_SINT           = 4,
        5 -> {r32g32b32,undefined};           % DXGI_FORMAT_R32G32B32_TYPELESS          = 5,
        6 -> {r32g32b32,float};               % DXGI_FORMAT_R32G32B32_FLOAT             = 6,
        7 -> {r32g32b32,uint};                % DXGI_FORMAT_R32G32B32_UINT              = 7,
        8 -> {r32g32b32,sint};                % DXGI_FORMAT_R32G32B32_SINT              = 8,
        9 -> {r16g16b16a16,undefined};        % DXGI_FORMAT_R16G16B16A16_TYPELESS       = 9,
        10 -> {r16g16b16a16,float};           % DXGI_FORMAT_R16G16B16A16_FLOAT          = 10,
        11 -> {r16g16b16a16,uint};            % DXGI_FORMAT_R16G16B16A16_UNORM          = 11,
        12 -> {r16g16b16a16,uint};            % DXGI_FORMAT_R16G16B16A16_UINT           = 12,
        13 -> {r16g16b16a16,sint};            % DXGI_FORMAT_R16G16B16A16_SNORM          = 13,
        14 -> {r16g16b16a16,sint};            % DXGI_FORMAT_R16G16B16A16_SINT           = 14,
        15 -> {r32g32,undefined};             % DXGI_FORMAT_R32G32_TYPELESS             = 15,
        16 -> {r32g32,float};                 % DXGI_FORMAT_R32G32_FLOAT                = 16,
        17 -> {r32g32,uint};                  % DXGI_FORMAT_R32G32_UINT                 = 17,
        18 -> {r32g32,sint};                  % DXGI_FORMAT_R32G32_SINT                 = 18,
        19 -> {r32g8x24,undefined};           % DXGI_FORMAT_R32G8X24_TYPELESS           = 19,
        20 -> {d32_float_s8x24,uint};         % DXGI_FORMAT_D32_FLOAT_S8X24_UINT        = 20,
        21 -> {r32_float_x8x24,undefined};    % DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS    = 21,
        22 -> {x32_undefined_g8x24,uint};     % DXGI_FORMAT_X32_TYPELESS_G8X24_UINT     = 22,
        23 -> {r10g10b10a2,undefined};        % DXGI_FORMAT_R10G10B10A2_TYPELESS        = 23,
        24 -> {r10g10b10a2,uint};             % DXGI_FORMAT_R10G10B10A2_UNORM           = 24,
        25 -> {r10g10b10a2,uint};             % DXGI_FORMAT_R10G10B10A2_UINT            = 25,
        26 -> {r11g11b10,float};              % DXGI_FORMAT_R11G11B10_FLOAT             = 26,
        27 -> {r8g8b8a8,undefined};           % DXGI_FORMAT_R8G8B8A8_TYPELESS           = 27,
        28 -> {r8g8b8a8,uint};                % DXGI_FORMAT_R8G8B8A8_UNORM              = 28,
        29 -> {r8g8b8a8,uint};                % DXGI_FORMAT_R8G8B8A8_UNORM_SRGB         = 29,
        30 -> {r8g8b8a8,uint};                % DXGI_FORMAT_R8G8B8A8_UINT               = 30,
        31 -> {r8g8b8a8,sint};                % DXGI_FORMAT_R8G8B8A8_SNORM              = 31,
        32 -> {r8g8b8a8,sint};                % DXGI_FORMAT_R8G8B8A8_SINT               = 32,
        33 -> {r16g16,undefined};             % DXGI_FORMAT_R16G16_TYPELESS             = 33,
        34 -> {r16g16,float};                 % DXGI_FORMAT_R16G16_FLOAT                = 34,
        35 -> {r16g16,uint};                  % DXGI_FORMAT_R16G16_UNORM                = 35,
        36 -> {r16g16,uint};                  % DXGI_FORMAT_R16G16_UINT                 = 36,
        37 -> {r16g16,sint};                  % DXGI_FORMAT_R16G16_SNORM                = 37,
        38 -> {r16g16,sint};                  % DXGI_FORMAT_R16G16_SINT                 = 38,
        39 -> {r32,undefined};                % DXGI_FORMAT_R32_TYPELESS                = 39,
        40 -> {d32,float};                    % DXGI_FORMAT_D32_FLOAT                   = 40,
        41 -> {r32,float};                    % DXGI_FORMAT_R32_FLOAT                   = 41,
        42 -> {r32,uint};                     % DXGI_FORMAT_R32_UINT                    = 42,
        43 -> {r32,sint};                     % DXGI_FORMAT_R32_SINT                    = 43,
        44 -> {r24g8,undefined};              % DXGI_FORMAT_R24G8_TYPELESS              = 44,
        45 -> {d24_s8,uint};                  % DXGI_FORMAT_D24_UNORM_S8_UINT           = 45,
        46 -> {r24_x8,undefined};             % DXGI_FORMAT_R24_UNORM_X8_TYPELESS       = 46,
        47 -> {x24_undefined_g8,uint};        % DXGI_FORMAT_X24_TYPELESS_G8_UINT        = 47,
        48 -> {r8g8,undefined};               % DXGI_FORMAT_R8G8_TYPELESS               = 48,
        49 -> {r8g8,uint};                    % DXGI_FORMAT_R8G8_UNORM                  = 49,
        50 -> {r8g8,uint};                    % DXGI_FORMAT_R8G8_UINT                   = 50,
        51 -> {r8g8,sint};                    % DXGI_FORMAT_R8G8_SNORM                  = 51,
        52 -> {r8g8,sint};                    % DXGI_FORMAT_R8G8_SINT                   = 52,
        53 -> {r16,undefined};                % DXGI_FORMAT_R16_TYPELESS                = 53,
        54 -> {r16,float};                    % DXGI_FORMAT_R16_FLOAT                   = 54,
        55 -> {d16,uint};                     % DXGI_FORMAT_D16_UNORM                   = 55,
        56 -> {r16,uint};                     % DXGI_FORMAT_R16_UNORM                   = 56,
        57 -> {r16,uint};                     % DXGI_FORMAT_R16_UINT                    = 57,
        58 -> {r16,sint};                     % DXGI_FORMAT_R16_SNORM                   = 58,
        59 -> {r16,sint};                     % DXGI_FORMAT_R16_SINT                    = 59,
        60 -> {r8,undefined};                 % DXGI_FORMAT_R8_TYPELESS                 = 60,
        61 -> {r8,uint};                      % DXGI_FORMAT_R8_UNORM                    = 61,
        62 -> {r8,uint};                      % DXGI_FORMAT_R8_UINT                     = 62,
        63 -> {r8,sint};                      % DXGI_FORMAT_R8_SNORM                    = 63,
        64 -> {r8,sint};                      % DXGI_FORMAT_R8_SINT                     = 64,
        65 -> {a8,uint};                      % DXGI_FORMAT_A8_UNORM                    = 65,
        66 -> {r1,uint};                      % DXGI_FORMAT_R1_UNORM                    = 66,
        67 -> {r9g9b9e5,sharedexp};           % DXGI_FORMAT_R9G9B9E5_SHAREDEXP          = 67,
        68 -> {r8g8_b8g8,uint};               % DXGI_FORMAT_R8G8_B8G8_UNORM             = 68,
        69 -> {g8r8_g8b8,uint};               % DXGI_FORMAT_G8R8_G8B8_UNORM             = 69,
        70 -> {bc1,undefined};                % DXGI_FORMAT_BC1_TYPELESS                = 70,
        71 -> {bc1,uint};                     % DXGI_FORMAT_BC1_UNORM                   = 71,
        72 -> {bc1,uint};                     % DXGI_FORMAT_BC1_UNORM_SRGB              = 72,
        73 -> {bc2,undefined};                % DXGI_FORMAT_BC2_TYPELESS                = 73,
        74 -> {bc2,uint};                     % DXGI_FORMAT_BC2_UNORM                   = 74,
        75 -> {bc2,uint};                     % DXGI_FORMAT_BC2_UNORM_SRGB              = 75,
        76 -> {bc3,undefined};                % DXGI_FORMAT_BC3_TYPELESS                = 76,
        77 -> {bc3,uint};                     % DXGI_FORMAT_BC3_UNORM                   = 77,
        78 -> {bc3,uint};                     % DXGI_FORMAT_BC3_UNORM_SRGB              = 78,
        79 -> {bc4,undefined};                % DXGI_FORMAT_BC4_TYPELESS                = 79,
        80 -> {bc4,uint};                     % DXGI_FORMAT_BC4_UNORM                   = 80,
        81 -> {bc4,sint};                     % DXGI_FORMAT_BC4_SNORM                   = 81,
        82 -> {bc5,undefined};                % DXGI_FORMAT_BC5_TYPELESS                = 82,
        83 -> {bc5,uint};                     % DXGI_FORMAT_BC5_UNORM                   = 83,
        84 -> {bc5,sint};                     % DXGI_FORMAT_BC5_SNORM                   = 84,
        85 -> {b5g6r5,uint};                  % DXGI_FORMAT_B5G6R5_UNORM                = 85,
        86 -> {b5g5r5a1,uint};                % DXGI_FORMAT_B5G5R5A1_UNORM              = 86,
        87 -> {b8g8r8a8,uint};                % DXGI_FORMAT_B8G8R8A8_UNORM              = 87,
        88 -> {b8g8r8x8,uint};                % DXGI_FORMAT_B8G8R8X8_UNORM              = 88,
        89 -> {r10g10b10_xr_bias_a2,uint};    % DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM  = 89,
        90 -> {b8g8r8a8,undefined};           % DXGI_FORMAT_B8G8R8A8_TYPELESS           = 90,
        91 -> {b8g8r8a8,uint};                % DXGI_FORMAT_B8G8R8A8_UNORM_SRGB         = 91,
        92 -> {b8g8r8x8,undefined};           % DXGI_FORMAT_B8G8R8X8_TYPELESS           = 92,
        93 -> {b8g8r8x8,uint};                % DXGI_FORMAT_B8G8R8X8_UNORM_SRGB         = 93,
        94 -> {bc6h,undefined};               % DXGI_FORMAT_BC6H_TYPELESS               = 94,
        95 -> {bc6h,uf16};                    % DXGI_FORMAT_BC6H_UF16                   = 95,
        96 -> {bc6h,sf16};                    % DXGI_FORMAT_BC6H_SF16                   = 96,
        97 -> {bc7,undefing};                 % DXGI_FORMAT_BC7_TYPELESS                = 97,
        98 -> {bc7,uint};                     % DXGI_FORMAT_BC7_UNORM                   = 98,
        99 -> {bc7,uint};                     % DXGI_FORMAT_BC7_UNORM_SRGB              = 99,
        100 -> {ayuv, uint};                  % DXGI_FORMAT_AYUV                        = 100,
        101 -> {y410, uint};                  % DXGI_FORMAT_Y410                        = 101,
        102 -> {y416, uint};                  % DXGI_FORMAT_Y416                        = 102,
        103 -> {nv12, uint};                  % DXGI_FORMAT_NV12                        = 103,
        104 -> {p010, uint};                  % DXGI_FORMAT_P010                        = 104,
        105 -> {p016, uint};                  % DXGI_FORMAT_P016                        = 105,
        106 -> {'420_opaque', uint};          % DXGI_FORMAT_420_OPAQUE                  = 106,
        107 -> {yuy2, uint};                  % DXGI_FORMAT_YUY2                        = 107,
        108 -> {y210, uint};                  % DXGI_FORMAT_Y210                        = 108,
        109 -> {y216, uint};                  % DXGI_FORMAT_Y216                        = 109,
        110 -> {nv11, uint};                  % DXGI_FORMAT_NV11                        = 110,
        111 -> {ai44, uint};                  % DXGI_FORMAT_AI44                        = 111,
        112 -> {ia44, uint};                  % DXGI_FORMAT_IA44                        = 112,
        113 -> {p8, uint};                    % DXGI_FORMAT_P8                          = 113,
        114 -> {a8p8, uint};                  % DXGI_FORMAT_A8P8                        = 114,
        115 -> {b4g4r4a4, uint};              % DXGI_FORMAT_B4G4R4A4_UNORM              = 115,
        130 -> {p208, uint};                  % DXGI_FORMAT_P208                        = 130,
        131 -> {v208, uint};                  % DXGI_FORMAT_V208                        = 131,
        132 -> {v408, uint};                  % DXGI_FORMAT_V408                        = 132,
        _ -> {unknown, undefined}
    end.
