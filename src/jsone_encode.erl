%%% @doc JSON encoding module
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
-module(jsone_encode).

-ifdef(ENABLE_HIPE).
-compile([native, {hipe, [o3]}]).
-endif.

%%--------------------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------------------
-export([encode/1, encode/2]).

%%--------------------------------------------------------------------------------
%% Macros & Records & Types
%%--------------------------------------------------------------------------------
-define(ERROR(Function, Args), {error, {badarg, [{?MODULE, Function, Args, [{line, ?LINE}]}]}}).
-define(IS_STR(X), (is_binary(X) orelse is_atom(X))).
-define(IS_UINT(X), (is_integer(X) andalso X >= 0)).
-define(IS_PNUM(X), (is_number(X) andalso X >=0)).
-define(IS_DATETIME(Y,M,D,H,Mi,S), (?IS_UINT(Y) andalso ?IS_UINT(M) andalso ?IS_UINT(D) andalso
                                    ?IS_UINT(H) andalso ?IS_UINT(Mi) andalso
                                    ?IS_PNUM(S))).

-ifdef('NO_MAP_TYPE').
-define(IS_MAP(X), is_tuple(X)).
-define(ENCODE_MAP(Value, Nexts, Buf, Opt), ?ERROR(value, [Value, Nexts, Buf, Opt])).
-else.
-define(IS_MAP(X), is_map(X)).
-define(ENCODE_MAP(Value, Nexts, Buf, Opt), object(maps:to_list(Value), Nexts, Buf, Opt)).
-endif.

-type encode_result() :: {ok, binary()} | {error, {Reason::term(), [jsone:stack_item()]}}.
-type next() :: {array_values, [jsone:json_value()]}
              | {object_value, jsone:json_value(), jsone:json_object_members()}
              | {object_members, jsone:json_object_members()}
              | {char, binary()}.

-record(encode_opt_v2, {
          native_utf8 = false :: boolean(),
          canonical_form = false :: boolean(),
          float_format = [{scientific, 20}] :: [jsone:float_format_option()],
          datetime_format = {iso8601, 0} :: {jsone:datetime_format(), jsone:utc_offset_seconds()},
          object_key_type = string :: string | scalar | value,
          space = 0 :: non_neg_integer(),
          indent = 0 :: non_neg_integer(),
          undefined_as_null = false :: boolean()
         }).
-define(OPT, #encode_opt_v2).
-type opt() :: #encode_opt_v2{}.

%%--------------------------------------------------------------------------------
%% Exported Functions
%%--------------------------------------------------------------------------------
-spec encode(jsone:json_value()) -> encode_result().
encode(Value) ->
    encode(Value, []).

-spec encode(jsone:json_value(), [jsone:encode_option()]) -> encode_result().
encode(Value, Options) ->
    Opt = parse_options(Options),
    value(Value, [], <<"">>, Opt).

%%--------------------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------------------
-spec next([next()], binary(), opt()) -> encode_result().
next([], Buf, _)                       -> {ok, Buf};
next(Level = [Next | Nexts], Buf, Opt) ->
    case Next of
        {array_values, Values} ->
            case Values of
                [] -> array_values(Values, Nexts, Buf, Opt);
                _  -> array_values(Values, Nexts, pp_newline_or_space(<<Buf/binary, $,>>, Level, Opt), Opt)
            end;
        {object_value, Value, Members} ->
            object_value(Value, Members, Nexts, pp_space(<<Buf/binary, $:>>, Opt), Opt);
        {object_members, Members} ->
            case Members of
                [] -> object_members(Members, Nexts, Buf, Opt);
                _  -> object_members(Members, Nexts, pp_newline_or_space(<<Buf/binary, $,>>, Level, Opt), Opt)
            end;
        {char, C} ->
            next(Nexts, <<Buf/binary, C>>, Opt)
    end.

-spec value(jsone:json_value(), [next()], binary(), opt()) -> encode_result().
value(null, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "null">>, Opt);
value(undefined, Nexts, Buf, Opt = ?OPT{undefined_as_null = true}) -> next(Nexts, <<Buf/binary, "null">>, Opt);
value(false, Nexts, Buf, Opt)                        -> next(Nexts, <<Buf/binary, "false">>, Opt);
value(true, Nexts, Buf, Opt)                         -> next(Nexts, <<Buf/binary, "true">>, Opt);
value({{json, T}}, Nexts, Buf, Opt) ->
    try
        next(Nexts, <<Buf/binary, (iolist_to_binary(T))/binary>>, Opt)
    catch
         error:badarg ->
            ?ERROR(value, [{json, T}, Nexts, Buf, Opt])
    end;
value({{json_utf8, T}}, Nexts, Buf, Opt) ->
    try unicode:characters_to_binary(T) of
        {error, OK, Invalid} ->
            {error, {{invalid_json_utf8, OK, Invalid}, [{?MODULE, value, [{json_utf8, T}, Nexts, Buf, Opt], [{line, ?LINE}]}]}};
        B when is_binary(B) ->
            next(Nexts, <<Buf/binary, B/binary>>, Opt)
    catch
        error:badarg ->
            ?ERROR(value, [{json_utf8, T}, Nexts, Buf, Opt])
    end;
value(Value, Nexts, Buf, Opt) when is_integer(Value) -> next(Nexts, <<Buf/binary, (integer_to_binary(Value))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when is_float(Value)   -> next(Nexts, <<Buf/binary, (float_to_binary(Value, Opt?OPT.float_format))/binary>>, Opt);
value(Value, Nexts, Buf, Opt) when ?IS_STR(Value)    -> string(Value, Nexts, Buf, Opt);
value({{_,_,_},{_,_,_}} = Value, Nexts, Buf, Opt)    -> datetime(Value, Nexts, Buf, Opt);
value({Value}, Nexts, Buf, Opt)                      -> object(Value, Nexts, Buf, Opt);
value([{}], Nexts, Buf, Opt)                         -> object([], Nexts, Buf, Opt);
value([{{_,_,_},{_,_,_}}|_] = Value, Nexts, Buf, Opt)-> array(Value, Nexts, Buf, Opt);
value([{_, _}|_] = Value, Nexts, Buf, Opt)           -> object(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when ?IS_MAP(Value)    -> ?ENCODE_MAP(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt) when is_list(Value)    -> array(Value, Nexts, Buf, Opt);
value(Value, Nexts, Buf, Opt)                        -> ?ERROR(value, [Value, Nexts, Buf, Opt]).

-spec string(jsone:json_string(), [next()], binary(), opt()) -> encode_result().
string(<<Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, $">>, Opt);
string(Str, Nexts, Buf, Opt) ->
    string(atom_to_binary(Str, utf8), Nexts, Buf, Opt).

-spec datetime(calendar:datetime(), [next()], binary(), opt()) -> encode_result().
datetime({{Y,M,D}, {H,Mi,S}}, Nexts, Buf, Opt) when ?IS_DATETIME(Y,M,D,H,Mi,S) ->
    {iso8601, Tz} = Opt?OPT.datetime_format,
    Str =
    [format_year(Y), $-, format2digit(M), $-, format2digit(D), $T,
     format2digit(H), $:, format2digit(Mi), $:, format_seconds(S),
     format_tz(Tz)],
    next(Nexts, <<Buf/binary, $", (list_to_binary(Str))/binary, $">>, Opt);
datetime(Datetime, Nexts, Buf, Opt) ->
    ?ERROR(datetime, [Datetime, Nexts, Buf, Opt]).

-ifndef(NO_DIALYZER_SPEC).
-dialyzer({no_improper_lists, [format_year/1]}).
-endif.
-spec format_year(non_neg_integer()) -> iodata().
format_year(Y) when Y > 999 -> integer_to_binary(Y);
format_year(Y) ->
    B = integer_to_binary(Y),
    [lists:duplicate(4-byte_size(B), $0)|B].

-spec format2digit(non_neg_integer()) -> iolist().
format2digit(0) -> "00";
format2digit(1) -> "01";
format2digit(2) -> "02";
format2digit(3) -> "03";
format2digit(4) -> "04";
format2digit(5) -> "05";
format2digit(6) -> "06";
format2digit(7) -> "07";
format2digit(8) -> "08";
format2digit(9) -> "09";
format2digit(X) -> integer_to_list(X).

-spec format_seconds(non_neg_integer() | float()) -> iolist().
format_seconds(S) when is_integer(S) -> format2digit(S);
format_seconds(S) when is_float(S) -> io_lib:format("~6.3.0f", [S]).

-spec format_tz(integer()) -> byte() | iolist().
format_tz(0) -> $Z;
format_tz(Tz) when Tz > 0 -> [$+|format_tz_(Tz)];
format_tz(Tz) -> [$-|format_tz_(-Tz)].

-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-spec format_tz_(integer()) -> iolist().
format_tz_(S) ->
    H = S div ?SECONDS_PER_HOUR,
    S1 = S rem ?SECONDS_PER_HOUR,
    M = S1 div ?SECONDS_PER_MINUTE,
    [format2digit(H), $:, format2digit(M)].

-spec object_key(jsone:json_value(), [next()], binary(), opt()) -> encode_result().
object_key(Key, Nexts, Buf, Opt) when ?IS_STR(Key) ->
    string(Key, Nexts, Buf, Opt);
object_key(Key, Nexts, Buf, Opt = ?OPT{object_key_type = scalar}) when is_number(Key) ->
    value(Key, [{char, $"} | Nexts], <<Buf/binary, $">>, Opt);
object_key(Key = {{Y,M,D},{H,Mi,S}}, Nexts, Buf, Opt = ?OPT{object_key_type = Type}) when ?IS_DATETIME(Y,M,D,H,Mi,S), Type =/= string ->
    value(Key, Nexts, Buf, Opt);
object_key(Key, Nexts, Buf, Opt = ?OPT{object_key_type = value}) ->
    case value(Key, [], <<>>, Opt) of
        {error, Reason} -> {error, Reason};
        {ok, BinaryKey} -> string(BinaryKey, Nexts, Buf, Opt)
    end;
object_key(Key, Nexts, Buf, Opt) ->
    ?ERROR(object_key, [Key, Nexts, Buf, Opt]).

-define(H8(X), (hex(X)):16).
-define(H16(X), ?H8(X bsr 8), ?H8(X band 16#FF)).

-ifdef(ENABLE_HIPE).
-define(COPY_UTF8,
escape_string(<<2#110:3, C1:5, C2, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, (2#11000000+C1), C2>>, Opt);
escape_string(<<2#1110:4, C1:4, C2:16, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, (2#11100000+C1), C2:16>>, Opt);
escape_string(<<2#11110:5, C1:3, C2:24, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, (2#11110000+C1), C2:24>>, Opt)
    ).
-else.
-define(COPY_UTF8,
escape_string(<<Ch/utf8, Str/binary>>, Nexts, Buf, Opt) ->
    escape_string(Str, Nexts, <<Buf/binary, Ch/utf8>>, Opt)
    ).
-endif.

-spec escape_string(binary(), [next()], binary(), opt()) -> encode_result().
escape_string(<<"">>,                   Nexts, Buf, Opt) -> next(Nexts, <<Buf/binary, $">>, Opt);
escape_string(<<$", Str/binary>>,       Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $">>, Opt);
escape_string(<<$\/, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\/>>, Opt);
escape_string(<<$\\, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $\\>>, Opt);
escape_string(<<$\b, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $b>>, Opt);
escape_string(<<$\f, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $f>>, Opt);
escape_string(<<$\n, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $n>>, Opt);
escape_string(<<$\r, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $r>>, Opt);
escape_string(<<$\t, Str/binary>>,      Nexts, Buf, Opt) -> escape_string(Str, Nexts, <<Buf/binary, $\\, $t>>, Opt);
escape_string(<<0:1, C:7, Str/binary>>, Nexts, Buf, Opt) ->
    case C < 16#20 of
        true  -> escape_string(Str, Nexts, <<Buf/binary, "\\u00", ?H8(C)>>, Opt);
        false -> escape_string(Str, Nexts, <<Buf/binary, C>>, Opt)
    end;
escape_string(<<Ch/utf8, Str/binary>>,  Nexts, Buf, Opt = ?OPT{native_utf8 = false}) ->
    NewBuf = if
                 Ch =< 16#FFFF -> <<Buf/binary, $\\, $u, ?H16(Ch)>>;
                 true ->
                     <<H1, H2, L1, L2>> = <<Ch/utf16>>,
                     <<Buf/binary, $\\, $u, ?H8(H1), ?H8(H2), $\\, $u, ?H8(L1), ?H8(L2)>>
             end,
    escape_string(Str, Nexts, NewBuf, Opt);
?COPY_UTF8;
escape_string(Str, Nexts, Buf, Opt) ->
    ?ERROR(escape_string, [Str, Nexts, Buf, Opt]).

-compile({inline, [hex/1]}).
-spec hex(byte()) -> 0..16#FFFF.
hex(X) ->
  element(
    X+1,
    {16#3030, 16#3031, 16#3032, 16#3033, 16#3034, 16#3035, 16#3036, 16#3037,
     16#3038, 16#3039, 16#3061, 16#3062, 16#3063, 16#3064, 16#3065, 16#3066,
     16#3130, 16#3131, 16#3132, 16#3133, 16#3134, 16#3135, 16#3136, 16#3137,
     16#3138, 16#3139, 16#3161, 16#3162, 16#3163, 16#3164, 16#3165, 16#3166,
     16#3230, 16#3231, 16#3232, 16#3233, 16#3234, 16#3235, 16#3236, 16#3237,
     16#3238, 16#3239, 16#3261, 16#3262, 16#3263, 16#3264, 16#3265, 16#3266,
     16#3330, 16#3331, 16#3332, 16#3333, 16#3334, 16#3335, 16#3336, 16#3337,
     16#3338, 16#3339, 16#3361, 16#3362, 16#3363, 16#3364, 16#3365, 16#3366,
     16#3430, 16#3431, 16#3432, 16#3433, 16#3434, 16#3435, 16#3436, 16#3437,
     16#3438, 16#3439, 16#3461, 16#3462, 16#3463, 16#3464, 16#3465, 16#3466,
     16#3530, 16#3531, 16#3532, 16#3533, 16#3534, 16#3535, 16#3536, 16#3537,
     16#3538, 16#3539, 16#3561, 16#3562, 16#3563, 16#3564, 16#3565, 16#3566,
     16#3630, 16#3631, 16#3632, 16#3633, 16#3634, 16#3635, 16#3636, 16#3637,
     16#3638, 16#3639, 16#3661, 16#3662, 16#3663, 16#3664, 16#3665, 16#3666,
     16#3730, 16#3731, 16#3732, 16#3733, 16#3734, 16#3735, 16#3736, 16#3737,
     16#3738, 16#3739, 16#3761, 16#3762, 16#3763, 16#3764, 16#3765, 16#3766,
     16#3830, 16#3831, 16#3832, 16#3833, 16#3834, 16#3835, 16#3836, 16#3837,
     16#3838, 16#3839, 16#3861, 16#3862, 16#3863, 16#3864, 16#3865, 16#3866,
     16#3930, 16#3931, 16#3932, 16#3933, 16#3934, 16#3935, 16#3936, 16#3937,
     16#3938, 16#3939, 16#3961, 16#3962, 16#3963, 16#3964, 16#3965, 16#3966,
     16#6130, 16#6131, 16#6132, 16#6133, 16#6134, 16#6135, 16#6136, 16#6137,
     16#6138, 16#6139, 16#6161, 16#6162, 16#6163, 16#6164, 16#6165, 16#6166,
     16#6230, 16#6231, 16#6232, 16#6233, 16#6234, 16#6235, 16#6236, 16#6237,
     16#6238, 16#6239, 16#6261, 16#6262, 16#6263, 16#6264, 16#6265, 16#6266,
     16#6330, 16#6331, 16#6332, 16#6333, 16#6334, 16#6335, 16#6336, 16#6337,
     16#6338, 16#6339, 16#6361, 16#6362, 16#6363, 16#6364, 16#6365, 16#6366,
     16#6430, 16#6431, 16#6432, 16#6433, 16#6434, 16#6435, 16#6436, 16#6437,
     16#6438, 16#6439, 16#6461, 16#6462, 16#6463, 16#6464, 16#6465, 16#6466,
     16#6530, 16#6531, 16#6532, 16#6533, 16#6534, 16#6535, 16#6536, 16#6537,
     16#6538, 16#6539, 16#6561, 16#6562, 16#6563, 16#6564, 16#6565, 16#6566,
     16#6630, 16#6631, 16#6632, 16#6633, 16#6634, 16#6635, 16#6636, 16#6637,
     16#6638, 16#6639, 16#6661, 16#6662, 16#6663, 16#6664, 16#6665, 16#6666}
          ).

-spec array(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array(List, Nexts, Buf, Opt) ->
    array_values(List, Nexts, pp_newline(<<Buf/binary, $[>>, Nexts, 1, Opt), Opt).

-spec array_values(jsone:json_array(), [next()], binary(), opt()) -> encode_result().
array_values([],       Nexts, Buf, Opt) -> next(Nexts, <<(pp_newline(Buf, Nexts, Opt))/binary, $]>>, Opt);
array_values([X | Xs], Nexts, Buf, Opt) -> value(X, [{array_values, Xs} | Nexts], Buf, Opt).

-spec object(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object(Members, Nexts, Buf, ?OPT{canonical_form = true}=Opt) ->
  object_members(lists:sort(Members), Nexts, pp_newline(<<Buf/binary, ${>>, Nexts, 1, Opt), Opt);
object(Members, Nexts, Buf, Opt) ->
    object_members(Members, Nexts, pp_newline(<<Buf/binary, ${>>, Nexts, 1, Opt), Opt).

-spec object_members(jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_members([],                  Nexts, Buf, Opt) -> next(Nexts, <<(pp_newline(Buf, Nexts, Opt))/binary, $}>>, Opt);
object_members([{Key, Value} | Xs], Nexts, Buf, Opt) -> object_key(Key, [{object_value, Value, Xs} | Nexts], Buf, Opt);
object_members(Arg, Nexts, Buf, Opt)                 -> ?ERROR(object_members, [Arg, Nexts, Buf, Opt]).

-spec object_value(jsone:json_value(), jsone:json_object_members(), [next()], binary(), opt()) -> encode_result().
object_value(Value, Members, Nexts, Buf, Opt) ->
    value(Value, [{object_members, Members} | Nexts], Buf, Opt).

-spec pp_space(binary(), opt()) -> binary().
pp_space(Buf, Opt) -> padding(Buf, Opt?OPT.space).

-spec pp_newline(binary(), list(), opt()) -> binary().
pp_newline(Buf, Level, Opt) -> pp_newline(Buf, Level, 0, Opt).

-spec pp_newline(binary(), list(), non_neg_integer(), opt()) -> binary().
pp_newline(Buf, _, _,     ?OPT{indent = 0}) -> Buf;
pp_newline(Buf, L, Extra, ?OPT{indent = N}) -> padding(<<Buf/binary, $\n>>, Extra * N + length(L) * N).

-spec pp_newline_or_space(binary(), list(), opt()) -> binary().
pp_newline_or_space(Buf, _, Opt = ?OPT{indent = 0}) -> pp_space(Buf, Opt);
pp_newline_or_space(Buf, L, Opt)                    -> pp_newline(Buf, L, Opt).

-spec padding(binary(), non_neg_integer()) -> binary().
padding(Buf, 0) -> Buf;
padding(Buf, 1) -> <<Buf/binary, " ">>;
padding(Buf, 2) -> <<Buf/binary, "  ">>;
padding(Buf, 3) -> <<Buf/binary, "   ">>;
padding(Buf, 4) -> <<Buf/binary, "    ">>;
padding(Buf, 5) -> <<Buf/binary, "     ">>;
padding(Buf, 6) -> <<Buf/binary, "      ">>;
padding(Buf, 7) -> <<Buf/binary, "       ">>;
padding(Buf, 8) -> <<Buf/binary, "        ">>;
padding(Buf, N) -> padding(<<Buf/binary, "         ">>, N - 9).

-spec parse_options([jsone:encode_option()]) -> opt().
parse_options(Options) ->
    parse_option(Options, ?OPT{}).

-spec parse_option([jsone:encode_option()], opt()) -> opt().
parse_option([], Opt) -> Opt;
parse_option([native_utf8|T], Opt) ->
    parse_option(T, Opt?OPT{native_utf8=true});
parse_option([canonical_form|T], Opt) ->
    parse_option(T, Opt?OPT{canonical_form=true});
parse_option([{float_format, F}|T], Opt) when is_list(F) ->
    parse_option(T, Opt?OPT{float_format = F});
parse_option([{space, N}|T], Opt) when is_integer(N), N >= 0 ->
    parse_option(T, Opt?OPT{space = N});
parse_option([{indent, N}|T], Opt) when is_integer(N), N >= 0 ->
    parse_option(T, Opt?OPT{indent = N});
parse_option([{object_key_type, Type}|T], Opt) when Type =:= string; Type =:= scalar; Type =:= value ->
    parse_option(T, Opt?OPT{object_key_type = Type});
parse_option([{datetime_format, Fmt}|T], Opt) ->
    case Fmt of
        iso8601                                 -> parse_option(T, Opt?OPT{datetime_format = {iso8601, 0}});
        {iso8601, utc}                          -> parse_option(T, Opt?OPT{datetime_format = {iso8601, 0}});
        {iso8601, local}                        -> parse_option(T, Opt?OPT{datetime_format = {iso8601, local_offset()}});
        {iso8601, N} when -86400 < N, N < 86400 -> parse_option(T, Opt?OPT{datetime_format = {iso8601, N}});
        _                                       -> error(badarg, [[{datetime_format, Fmt}|T], Opt])
    end;
parse_option([undefined_as_null|T],Opt) ->
    parse_option(T, Opt?OPT{undefined_as_null = true});
parse_option(List, Opt) ->
    error(badarg, [List, Opt]).

-spec local_offset() -> jsone:utc_offset_seconds().
local_offset() ->
    UTC = {{1970, 1, 2}, {0,0,0}},
    Local = calendar:universal_time_to_local_time({{1970, 1, 2}, {0,0,0}}),
    calendar:datetime_to_gregorian_seconds(Local) - calendar:datetime_to_gregorian_seconds(UTC).
