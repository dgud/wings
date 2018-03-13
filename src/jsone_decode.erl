%%% @doc JSON decoding module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2013-2016, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(jsone_decode).

-ifdef(ENABLE_HIPE).
-compile([native, {hipe, [o3]}]).
-endif.

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([decode/1, decode/2]).

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).

-ifdef('NO_MAP_TYPE').
-define(DEFAULT_OBJECT_FORMAT, tuple).
-define(LIST_TO_MAP(X), error({this_erts_does_not_support_maps, X})).
-else.
-define(DEFAULT_OBJECT_FORMAT, map).
-define(LIST_TO_MAP(X), maps:from_list(X)).
-endif.

-type next() :: {array_next, [jsone:json_value()]}
              | {object_value, jsone:json_object_members()}
              | {object_next, jsone:json_string(), jsone:json_object_members()}.

-type whitespace_next() :: value
                         | array
                         | object
                         | {array_next, [jsone:json_value()]}
                         | {object_key, jsone:json_object_members()}
                         | {object_value, jsone:json_string(), jsone:json_object_members()}
                         | {object_next, jsone:json_object_members()}.

-type decode_result() :: {ok, jsone:json_value(), Rest::binary()} | {error, {Reason::term(), [jsone:stack_item()]}}.

-record(decode_opt_v2,
        {
          object_format=?DEFAULT_OBJECT_FORMAT :: tuple | proplist | map,
          allow_ctrl_chars=false :: boolean(),
          keys=binary :: 'binary' | 'atom' | 'existing_atom' | 'attempt_atom',
          undefined_as_null=false :: boolean()
        }).
-define(OPT, #decode_opt_v2).
-type opt() :: #decode_opt_v2{}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec decode(binary()) -> decode_result().
decode(Json) ->
    decode(Json, []).

-spec decode(binary(), [jsone:decode_option()]) -> decode_result().
decode(<<Json/binary>>, Options) ->
    Opt = parse_options(Options),
    whitespace(Json, value, [], <<"">>, Opt).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next(binary(), jsone:json_value(), [next()], binary(), opt()) -> decode_result().
next(<<Bin/binary>>, Value, [], _Buf, _Opt) ->
    {ok, Value, Bin};
next(<<Bin/binary>>, Value, [Next | Nexts], Buf, Opt) ->
    case Next of
        {array_next, Values}        -> whitespace(Bin, {array_next, [Value | Values]}, Nexts, Buf, Opt);
        {object_value, Members}     -> whitespace(Bin, {object_value, Value, Members}, Nexts, Buf, Opt);
        {object_next, Key, Members} -> whitespace(Bin, {object_next, [{Key, Value} | Members]}, Nexts, Buf, Opt)
    end.

-spec whitespace(binary(), whitespace_next(), [next()], binary(), opt()) -> decode_result().
whitespace(<<$  , Bin/binary>>, Next, Nexts, Buf, Opt) -> whitespace(Bin, Next, Nexts, Buf, Opt);
whitespace(<<$\t, Bin/binary>>, Next, Nexts, Buf, Opt) -> whitespace(Bin, Next, Nexts, Buf, Opt);
whitespace(<<$\r, Bin/binary>>, Next, Nexts, Buf, Opt) -> whitespace(Bin, Next, Nexts, Buf, Opt);
whitespace(<<$\n, Bin/binary>>, Next, Nexts, Buf, Opt) -> whitespace(Bin, Next, Nexts, Buf, Opt);
whitespace(<<Bin/binary>>,      Next, Nexts, Buf, Opt) ->
    case Next of
        value  -> value(Bin, Nexts, Buf, Opt);
        array  -> array(Bin, Nexts, Buf, Opt);
        object -> object(Bin, Nexts, Buf, Opt);
        {object_key, Members}        -> object_key(Bin, Members, Nexts, Buf, Opt);
        {array_next, Values}         -> array_next(Bin, Values, Nexts, Buf, Opt);
        {object_value, Key, Members} -> object_value(Bin, Key, Members, Nexts, Buf, Opt);
        {object_next, Members}       -> object_next(Bin, Members, Nexts, Buf, Opt)
    end.

-spec value(binary(), [next()], binary(), opt()) -> decode_result().
value(<<"false", Bin/binary>>, Nexts, Buf, Opt) -> next(Bin, false, Nexts, Buf, Opt);
value(<<"true", Bin/binary>>, Nexts, Buf, Opt)  -> next(Bin, true, Nexts, Buf, Opt);
value(<<"null", Bin/binary>>, Nexts, Buf,
      Opt = ?OPT{undefined_as_null = true})     -> next(Bin, undefined, Nexts, Buf, Opt);
value(<<"null", Bin/binary>>, Nexts, Buf, Opt)  -> next(Bin, null, Nexts, Buf, Opt);
value(<<$[, Bin/binary>>, Nexts, Buf, Opt)      -> whitespace(Bin, array, Nexts, Buf, Opt);
value(<<${, Bin/binary>>, Nexts, Buf, Opt)      -> whitespace(Bin, object, Nexts, Buf, Opt);
value(<<$", Bin/binary>>, Nexts, Buf, Opt)      -> string(Bin, byte_size(Buf), Nexts, Buf, Opt);
value(<<Bin/binary>>, Nexts, Buf, Opt)          -> number(Bin, Nexts, Buf, Opt).

-spec array(binary(), [next()], binary(), opt()) -> decode_result().
array(<<$], Bin/binary>>, Nexts, Buf, Opt) -> next(Bin, [], Nexts, Buf, Opt);
array(<<Bin/binary>>, Nexts, Buf, Opt)     -> value(Bin, [{array_next, []} | Nexts], Buf, Opt).

-spec array_next(binary(), [jsone:json_value()], [next()], binary(), opt()) -> decode_result().
array_next(<<$], Bin/binary>>, Values, Nexts, Buf, Opt) -> next(Bin, lists:reverse(Values), Nexts, Buf, Opt);
array_next(<<$,, Bin/binary>>, Values, Nexts, Buf, Opt) -> whitespace(Bin, value, [{array_next, Values} | Nexts], Buf, Opt);
array_next(Bin,                Values, Nexts, Buf, Opt) -> ?ERROR(array_next, [Bin, Values, Nexts, Buf, Opt]).

-spec object(binary(), [next()], binary(), opt()) -> decode_result().
object(<<$}, Bin/binary>>, Nexts, Buf, Opt) -> next(Bin, make_object([], Opt), Nexts, Buf, Opt);
object(<<Bin/binary>>, Nexts, Buf, Opt)     -> object_key(Bin, [], Nexts, Buf, Opt).

-spec object_key(binary(), jsone:json_object_members(), [next()], binary(), opt()) -> decode_result().
object_key(<<$", Bin/binary>>, Members, Nexts, Buf, Opt) -> string(Bin, byte_size(Buf), [{object_value, Members} | Nexts], Buf, Opt);
object_key(<<Bin/binary>>, Members, Nexts, Buf, Opt)     -> ?ERROR(object_key, [Bin, Members, Nexts, Buf, Opt]).

-spec object_value(binary(), jsone:json_string(), jsone:json_object_members(), [next()], binary(), opt()) -> decode_result().
object_value(<<$:, Bin/binary>>, Key, Members, Nexts, Buf, Opt) ->
    whitespace(Bin, value, [{object_next, object_key(Key, Opt), Members} | Nexts], Buf, Opt);
object_value(Bin,                Key, Members, Nexts, Buf, Opt) -> ?ERROR(object_value, [Bin, Key, Members, Nexts, Buf, Opt]).

-compile({inline, [object_key/2]}).
object_key(Key, ?OPT{keys = binary}) -> Key;
object_key(Key, ?OPT{keys = atom}) -> binary_to_atom(Key, utf8);
object_key(Key, ?OPT{keys = existing_atom}) -> binary_to_existing_atom(Key, utf8);
object_key(Key, ?OPT{keys = attempt_atom}) ->
    try binary_to_existing_atom(Key, utf8)
    catch error:badarg -> Key
    end.

-spec object_next(binary(), jsone:json_object_members(), [next()], binary(), opt()) -> decode_result().
object_next(<<$}, Bin/binary>>, Members, Nexts, Buf, Opt) -> next(Bin, make_object(Members, Opt), Nexts, Buf, Opt);
object_next(<<$,, Bin/binary>>, Members, Nexts, Buf, Opt) -> whitespace(Bin, {object_key, Members}, Nexts, Buf, Opt);
object_next(Bin,                Members, Nexts, Buf, Opt) -> ?ERROR(object_next, [Bin, Members, Nexts, Buf, Opt]).

-spec string(binary(), non_neg_integer(), [next()], binary(), opt()) -> decode_result().
string(<<Bin/binary>>, Start, Nexts, Buf, Opt) ->
    string(Bin, Bin, Start, Nexts, Buf, Opt).

-spec string(binary(), binary(), non_neg_integer(), [next()], binary(), opt()) -> decode_result().
string(<<$", Bin/binary>>, Base, Start, Nexts, Buf, Opt) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(Bin) - 1),
    case Start =:= byte_size(Buf) of
        true  -> next(Bin, Prefix, Nexts, Buf, Opt);
        false ->
            Buf2 = <<Buf/binary, Prefix/binary>>,
            next(Bin, binary:part(Buf2, Start, byte_size(Buf2) - Start), Nexts, Buf2, Opt)
    end;
string(<<$\\, B/binary>>, Base, Start, Nexts, Buf, Opt) ->
    Prefix = binary:part(Base, 0, byte_size(Base) - byte_size(B) - 1),
    case B of
        <<$", Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $">>, Opt);
        <<$/, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $/>>, Opt);
        <<$\\,Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\\>>, Opt);
        <<$b, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\b>>, Opt);
        <<$f, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\f>>, Opt);
        <<$n, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\n>>, Opt);
        <<$r, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\r>>, Opt);
        <<$t, Bin/binary>> -> string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary, $\t>>, Opt);
        <<$u, Bin/binary>> -> unicode_string(Bin, Start, Nexts, <<Buf/binary, Prefix/binary>>, Opt);
        _                  -> ?ERROR(string, [<<$\\, B/binary>>, Base, Start, Nexts, Buf, Opt])
    end;
string(<<_, Bin/binary>>, Base, Start, Nexts, Buf, Opt) when Opt?OPT.allow_ctrl_chars ->
    string(Bin, Base, Start, Nexts, Buf, Opt);
string(<<C, Bin/binary>>, Base, Start, Nexts, Buf, Opt) when 16#20 =< C ->
    string(Bin, Base, Start, Nexts, Buf, Opt);
 string(Bin, Base, Start, Nexts, Buf, Opt) ->
     ?ERROR(string, [Bin, Base, Start, Nexts, Buf, Opt]).

-spec unicode_string(binary(), non_neg_integer(), [next()], binary(), opt()) -> decode_result().
unicode_string(<<N:4/binary, Bin/binary>>, Start, Nexts, Buf, Opt) ->
    try binary_to_integer(N, 16) of
        High when 16#D800 =< High, High =< 16#DBFF ->
            %% surrogate pair
            case Bin of
                <<$\\, $u, N2:4/binary, Bin2/binary>> ->
                    try binary_to_integer(N2, 16) of
                        Low when 16#DC00 =< Low, Low =< 16#DFFF ->
                            <<Unicode/utf16>> = <<High:16, Low:16>>,
                            string(Bin2, Start, Nexts, <<Buf/binary, Unicode/utf8>>, Opt);
                        _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opt])
                    catch error:badarg -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opt])
                    end;
                _ -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opt])
            end;
        Unicode when 16#DC00 =< Unicode, Unicode =< 16#DFFF;  % second part of surrogate pair (without first part)
                     0 > Unicode ->
            ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opt]);
        Unicode ->
            string(Bin, Start, Nexts, <<Buf/binary, Unicode/utf8>>, Opt)
    catch error:badarg -> ?ERROR(unicode_string, [<<N/binary, Bin/binary>>, Start, Nexts, Buf, Opt])
    end;
unicode_string(Bin, Start, Nexts, Buf, Opt) ->
    ?ERROR(unicode_string, [Bin, Start, Nexts, Buf, Opt]).

-spec number(binary(), [next()], binary(), opt()) -> decode_result().
number(<<$-, Bin/binary>>, Nexts, Buf, Opt) -> number_integer_part(Bin, -1, Nexts, Buf, Opt);
number(<<Bin/binary>>,     Nexts, Buf, Opt) -> number_integer_part(Bin,  1, Nexts, Buf, Opt).

-spec number_integer_part(binary(), 1|-1, [next()], binary(), opt()) -> decode_result().
number_integer_part(<<$0, Bin/binary>>, Sign, Nexts, Buf, Opt) ->
    number_fraction_part(Bin, Sign, 0, Nexts, Buf, Opt);
number_integer_part(<<C, Bin/binary>>, Sign, Nexts, Buf, Opt) when $1 =< C, C =< $9 ->
    number_integer_part_rest(Bin, C - $0, Sign, Nexts, Buf, Opt);
number_integer_part(Bin, Sign, Nexts, Buf, Opt) ->
    ?ERROR(number_integer_part, [Bin, Sign, Nexts, Buf, Opt]).

-spec number_integer_part_rest(binary(), non_neg_integer(), 1|-1, [next()], binary(), opt()) -> decode_result().
number_integer_part_rest(<<C, Bin/binary>>, N, Sign, Nexts, Buf, Opt) when $0 =< C, C =< $9 ->
    number_integer_part_rest(Bin, N * 10 + C - $0, Sign, Nexts, Buf, Opt);
number_integer_part_rest(<<Bin/binary>>, N, Sign, Nexts, Buf, Opt) ->
    number_fraction_part(Bin, Sign, N, Nexts, Buf, Opt).

-spec number_fraction_part(binary(), 1|-1, non_neg_integer(), [next()], binary(), opt()) -> decode_result().
number_fraction_part(<<$., Bin/binary>>, Sign, Int, Nexts, Buf, Opt) ->
    number_fraction_part_rest(Bin, Sign, Int, 0, Nexts, Buf, Opt);
number_fraction_part(<<Bin/binary>>, Sign, Int, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, Sign * Int, 0, Nexts, Buf, Opt).

-spec number_fraction_part_rest(binary(), 1|-1, non_neg_integer(), non_neg_integer(), [next()], binary(), opt()) -> decode_result().
number_fraction_part_rest(<<C, Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf, Opt) when $0 =< C, C =< $9 ->
    number_fraction_part_rest(Bin, Sign, N * 10 + C - $0, DecimalOffset + 1, Nexts, Buf, Opt);
number_fraction_part_rest(<<Bin/binary>>, Sign, N, DecimalOffset, Nexts, Buf, Opt) when DecimalOffset > 0 ->
    number_exponation_part(Bin, Sign * N, DecimalOffset, Nexts, Buf, Opt);
number_fraction_part_rest(Bin, Sign, N, DecimalOffset, Nexts, Buf, Opt) ->
    ?ERROR(number_fraction_part_rest, [Bin, Sign, N, DecimalOffset, Nexts, Buf, Opt]).

-spec number_exponation_part(binary(), integer(), non_neg_integer(), [next()], binary(), opt()) -> decode_result().
number_exponation_part(<<$e, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<$E, $+, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<$e, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<$E, $-, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, -1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<$e, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<$E, Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    number_exponation_part(Bin, N, DecimalOffset, 1, 0, true, Nexts, Buf, Opt);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, Nexts, Buf, Opt) ->
    case DecimalOffset of
        0 -> next(Bin, N, Nexts, Buf, Opt);
        _ -> next(Bin, N / math:pow(10, DecimalOffset), Nexts, Buf, Opt)
    end.

-spec number_exponation_part(binary(), integer(), non_neg_integer(), 1|-1, non_neg_integer(), boolean(), [next()], binary(), opt()) -> decode_result().
number_exponation_part(<<C, Bin/binary>>, N, DecimalOffset, ExpSign, Exp, _, Nexts, Buf, Opt) when $0 =< C, C =< $9 ->
    number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp * 10 + C - $0, false, Nexts, Buf, Opt);
number_exponation_part(<<Bin/binary>>, N, DecimalOffset, ExpSign, Exp, false, Nexts, Buf, Opt) ->
    Pos = ExpSign * Exp - DecimalOffset,
    try N * math:pow(10, Pos)
    of Res -> next(Bin, Res, Nexts, Buf, Opt)
    catch error:badarith ->
        ?ERROR(number_exponation_part, [Bin, N, DecimalOffset, ExpSign, Exp, false, Nexts, Buf, Opt])
    end;
number_exponation_part(Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf, Opt) ->
    ?ERROR(number_exponation_part, [Bin, N, DecimalOffset, ExpSign, Exp, IsFirst, Nexts, Buf, Opt]).

-spec make_object(jsone:json_object_members(), opt()) -> jsone:json_object().
make_object(Members, ?OPT{object_format = tuple}) -> {lists:reverse(Members)};
make_object(Members, ?OPT{object_format = map})   -> ?LIST_TO_MAP(Members);
make_object([],      _)                           -> [{}];
make_object(Members, _)                           -> lists:reverse(Members).

-spec parse_options([jsone:decode_option()]) -> opt().
parse_options(Options) ->
    parse_option(Options, ?OPT{}).

-spec parse_option([jsone:decode_option()], opt()) -> opt().
parse_option([], Opt) -> Opt;
parse_option([{object_format,F}|T], Opt) when F =:= tuple; F =:= proplist; F =:= map ->
    parse_option(T, Opt?OPT{object_format=F});
parse_option([{allow_ctrl_chars,B}|T], Opt) when is_boolean(B) ->
    parse_option(T, Opt?OPT{allow_ctrl_chars=B});
parse_option([{keys, K}|T], Opt)
  when K =:= binary; K =:= atom; K =:= existing_atom; K =:= attempt_atom ->
    parse_option(T, Opt?OPT{keys = K});
parse_option([undefined_as_null|T], Opt) ->
    parse_option(T, Opt?OPT{undefined_as_null = true});
parse_option(List, Opt) ->
    error(badarg, [List, Opt]).
