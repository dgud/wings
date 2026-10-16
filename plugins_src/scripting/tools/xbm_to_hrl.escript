#!/bin/env escript
-module(xbm_to_hrl).

%%
%%  Turn xbm files into hrl files
%%
%%  Copyright 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

main([]) ->
    io:format("xbm_to_hrl <xbm-file>\n\n",[]);
main([Xbmfile]) ->
    NewFile_0 = filename:rootname(Xbmfile),
    case file:read_file_info(Xbmfile) of
        {ok, _} ->
            NewFile = NewFile_0 ++ ".hrl",
            {ok, XBMF} = file:open(Xbmfile, [read]),
            {ok, HRLF} = file:open(NewFile, [write]),
		    io:format(HRLF, "%% This file is generated from the .xbm file.\n", []),
            to_hrl(XBMF, HRLF),
            file:close(XBMF),
            file:close(HRLF);
	    _ ->
		    io:format("Could not find $xbmfile\n\n",[])
    end.

to_hrl(XBMF, HRLF) ->
    Bits = [],
    Bitsname = "??",
    to_hrl(XBMF, HRLF, Bits, Bitsname).
to_hrl(XBMF, HRLF, Bits, Bitsname) ->
    case io:get_line(XBMF, "") of
        [_|_]=Line_0 ->
            Line = string:trim(Line_0),
            case re:run(Line, "^#define +(\\w+)(_width|_height) +([0-9]+)",[{capture,all,list}]) of
                {match,[_,S1,S2,S3]} -> 
   				    Name = string:to_upper(S1 ++ S2),
				    io:format(HRLF, "-define(~s, ~s).\n", [Name, S3]),
                    Bitsname_1 = Bitsname,
                    Bits_1 = Bits;
			    _ ->
                    case re:run(Line, "static +unsigned +char +(\\w+)(_bits) *\\[ *\\] *= *{",[{capture,all,list}]) of
                        {match,[_,S1,S2]} ->
        				    Bitsname_1 = string:to_upper(S1 ++ S2);
                        _ ->
                            Bitsname_1 = Bitsname
                    end,
                    case re:run(Line, "^ *0x") of
                        {match,_} ->
                            Vals = string:split(Line, ",", all),
                            Bits_1 = lists:foldl(fun(Val, Acc) ->
                                case re:run(Val, "0x([0-9A-Za-z]+)",[{capture,all,list}]) of
						            {match,[_,M1]} ->
    							        ["16#" ++ M1|Acc];
                                    _ ->
                                        Acc
						        end
                            end, Bits, Vals);
                        _ ->
                            Bits_1 = Bits
                    end
            end,
            to_hrl(XBMF, HRLF, Bits_1, Bitsname_1);
        eof ->
		    io:format(HRLF, "-define(~s, <<~s>>).\n", [Bitsname, lists:join(",", lists:reverse(Bits))])
    end.

