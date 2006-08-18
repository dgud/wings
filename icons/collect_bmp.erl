%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 2001, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%% Original Author: Bjorn Gustavsson
%% 
%%     $Id: collect_bmp.erl,v 1.5 2004/10/08 13:32:24 bjorng Exp $
%%

-module(collect_bmp).
-export([start/0,start/1]).
-import(lists, [reverse/1]).

start() ->
    start(["icons","wings_icon.bundle"]).

start([InDir,OutFile]) ->
    Icons = load_icons(filelib:wildcard(filename:join(InDir, "*.{bmp,tga}"))),
    Bin = term_to_binary(Icons, [compressed]),
    io:format("Writing ~s\n", [OutFile]),
    ok = file:write_file(OutFile, Bin),
    ok.

load_icons([Name|Ns]) ->
    Id = list_to_atom(filename:rootname(filename:basename(Name))),
    Bin = load_file(Name),
    [{Id,Bin}|load_icons(Ns)];
load_icons([]) -> [].
    
load_file(File) ->
    io:format("Loading ~s\n", [File]),
    {ok,Bin} = file:read_file(File),
    case Bin of
	<<$B:8,$M:8,_/binary>> -> load_bmp(Bin);
	_ -> load_tga(Bin)
    end.

%%%
%%% Loding of BMP files.
%%%

load_bmp(<<$B:8,$M:8,_:8/binary,Offset:32/little,Bin/binary>>=Bin0) ->
    <<_:32/little,W:32/little,H:32/little,
     _:16,BitCount:16/little,Compression:32/little,
     _ImageSize:32/little,_/binary>> = Bin,
    BitCount = 24,
    Compression = 0,
    RowLength = W * 3 + pad_len(W * 3, 4),
    PixelsLen = H * RowLength,
    <<_:Offset/binary,Pixels0:PixelsLen/binary,_/binary>> = Bin0,
    Pixels1 = skip_padding(Pixels0, 3*W, pad_len(W * 3, 4), []),
    Pixels = shuffle_colors(Pixels1, []),
    {3,W,H,Pixels}.

skip_padding(<<>>, _, _, Acc) ->
    list_to_binary(reverse(Acc));
skip_padding(B, Bytes, Skip, Acc) ->
    <<Row:Bytes/binary,_:Skip/binary,Rest/binary>> = B,
    skip_padding(Rest, Bytes, Skip, [Row|Acc]).

shuffle_colors(<<B:8,G:8,R:8,T/binary>>, Acc) ->
    shuffle_colors(T, [[R,G,B]|Acc]);
shuffle_colors(<<>>, Acc) -> list_to_binary(reverse(Acc)).

pad_len(RL, Align) ->
    case RL rem Align of
	0 -> 0;
	Rem -> Align - Rem
    end.	     	   

%%%
%%% Loading of Targa files.
%%%

load_tga(<<0,0,2,0,0,0,0,0,0,0,0,0,  Image/binary>>) ->
    load_uncomp(Image);
load_tga(<<0,0,3,0,0,0,0,0,0,0,0,0, Image/binary>>) ->
    load_uncomp(Image).

load_uncomp(<<W:16/little,H:16/little,BitsPP:8,0:1,0:1,Order:2, Alpha:4,Image/binary>>) ->
    BytesPerPixel = BitsPP div 8,
    SpecOK = (W > 0) and (H > 0) and ((BitsPP == 32) 
				      or (BitsPP == 24) 
				      or (BitsPP == 8)),
    if 
	SpecOK == false ->
	    io:format("Unsupported image spec W ~p H ~p BitsPP ~p Order ~p Alpha ~p ~n",
		      [W,H,BitsPP,Order,Alpha]),
	    {error, {none,?MODULE,bad_image_specifiction}};
	true ->
	    Size = BytesPerPixel * W * H,
	    <<Pixels0:Size/binary, _Rest/binary>> = Image,
	    Pixels1 = tga_shuffle_colors(BytesPerPixel, Pixels0),
	    Pixels = tga_turn(Order, W*BytesPerPixel, Pixels1),
	    {BytesPerPixel,W,H,Pixels}
    end.

tga_shuffle_colors(3, Bin) ->
    tga_shuffle_colors_3(Bin, []);
tga_shuffle_colors(4, Bin) ->
    tga_shuffle_colors_4(Bin, []).

tga_shuffle_colors_3(<<B:8,G:8,R:8,T/binary>>, Acc) ->
    tga_shuffle_colors_3(T, [Acc|<<R:8,G:8,B:8>>]);
tga_shuffle_colors_3(<<>>, Acc) -> list_to_binary(Acc).

tga_shuffle_colors_4(<<B:8,G:8,R:8,A:8,T/binary>>, Acc) ->
    tga_shuffle_colors_4(T, [Acc|<<R:8,G:8,B:8,A:8>>]);
tga_shuffle_colors_4(<<>>, Acc) -> list_to_binary(Acc).

tga_turn(2, RowLen, Pixels) -> tga_turn_1(RowLen, Pixels, []);
tga_turn(0, _, Pixels) -> Pixels.

tga_turn_1(_, <<>>, Acc) -> list_to_binary(Acc);
tga_turn_1(L, Pixels0, Acc) ->
    <<Row:L/binary,T/binary>> = Pixels0,
    tga_turn_1(L, T, [Row|Acc]).
