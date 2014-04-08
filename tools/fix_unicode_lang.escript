#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main(Files) ->
    [check_file(File) || File <- Files],
    ok.

check_file(File) ->
    case file:consult(File) of
	{ok, _Content} -> ignore;
	{error, _Reason} ->
	    io:format("Fixing ~s~n~n",[File]),
	    {ok, Bin} = file:read_file(File),
	    [Line1|Rest] = binary:split(Bin, [<<"\n">>]),
	    Header = add_encoding(binary_to_list(Line1)),
	    Utf8 = unicode:characters_to_binary(Rest, latin1, utf8),
	    ok = file:write_file(File, [Header|Utf8]),
	    ok
    end.

add_encoding([$%|Rest]) -> add_encoding(Rest);
add_encoding([$  |Rest]) -> add_encoding(Rest);
add_encoding("-*- " ++ L1) ->
    "mode:erlang" ++ _ = L1, %% Assert
    <<"%% -*- coding:utf-8; "/utf8,
      (list_to_binary(L1))/binary, "\n"/utf8>>.
