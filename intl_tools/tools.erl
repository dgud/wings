%%
%%  tools.erl --
%%
%%     Tools for translation.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: tools.erl,v 1.2 2005/09/26 09:34:56 giniu Exp $
%%
-module(tools).

%% Translation support tools.
-export([generate_template/1,generate_template_file/1,diff/1,diff/2]).

%% Parse transform ASPI (called by compiler).
-export([parse_transform/2,format_error/1]).

-import(lists, [reverse/1,reverse/2,foreach/2,foldl/3]).

%%%%%%%%%% Tools %%%%%%%%%%%

diff(LangFile) ->
    EngTemplFile = get_en_template(LangFile),
    diff(LangFile, EngTemplFile).
    
diff(LangFile, EngTmplFile) ->
    {ok, Lang} = file:consult(LangFile),
    {ok, Eng} = file:consult(EngTmplFile),
    case diff(Eng,Lang,LangFile,[],[]) of
	[] -> ok;
	Miss ->
	    io:nl(),
	    io:put_chars("%%\n"),
	    io:put_chars("%% The following strings have no translation.\n"),
	    io:put_chars("%%\n\n"),
	    Out = group_leader(),
	    lists:foreach(fun(M) -> output_strings(M,Out) end,
			  reverse(Miss))
    end.
    
diff([{Key,Info}|ER],Lang0,LF,Lev,Miss0) ->
    case get_key(Key,Lang0) of
	{Info2, Lang} ->
	    Miss = case diff(Info,Info2,LF,Lev++[Key],[]) of
		       [] -> Miss0;
		       Miss1 -> [{Key,Miss1}|Miss0]
		   end,
	    diff(ER,Lang,LF,Lev,Miss);
	Lang ->
	    diff(ER,Lang,LF,Lev,[{Key,Info}|Miss0])
    end;
diff([],Keys,LF,Lev,Miss) ->
    Info = fun({Key,_}) -> 
		   io:format("%% Not used ~p in ~p~n",[Lev++[Key],LF])
	   end,
    lists:foreach(Info, Keys),
    Miss;
diff([Char|_], _Lang, _LF, _Level, Miss) when is_integer(Char) ->
    Miss.

get_key(Key, List) ->
    get_key(List,Key,[]).
get_key([{Key,Found}|Rest],Key,Acc) -> 
    case get_key(Rest, Key, Acc) of
	Res = {Found2, _Rest2} when is_integer(hd(Found)) ->
	    io:format("%% Warning ~p found twice ~p ~p~n",[Key,Found,Found2]),
	    Res;
	{Found2, Rest2} ->
	    {Found2++Found,lists:reverse(Rest2)};
	_ ->
	    {Found,lists:reverse(Acc,Rest)}
    end;

get_key([Miss|Rest],Key,Acc) -> 
    get_key(Rest,Key,[Miss|Acc]);
get_key([],_,Acc) -> lists:reverse(Acc).

generate_template([Dir]) ->
    OutFile = case filename:basename(Dir) of
		  "ebin" -> "wings";
		  DirName -> DirName
	      end ++ "_en.lang",
    Fs = filelib:wildcard(filename:join(Dir, "*.beam")),
    io:format("Writing: ~p\n", [filename:absname(OutFile)]),
    {ok,Out} = file:open(OutFile, [write]),
    io:put_chars(Out, "%% -*- mode:erlang; erlang-indent-level: 2 -*-\n"),
    try foreach(fun(File) -> scan_file(File, Out) end, Fs)
	after
	    file:close(Out)
	end.

generate_template_file([Dir, File]) ->
	OutFile = filename:join(Dir, File ++ "_en.lang"),
	io:format("Writing: ~p\n", [filename:absname(OutFile)]),
	{ok,Out} = file:open(OutFile, [write]),
	io:put_chars(Out, "%% -*- mode:erlang; erlang-indent-level: 2 -*-\n"),
	scan_file(filename:join(Dir, File ++ ".beam"), Out),
	file:close(Out).

scan_file(Filename, Out) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Forms}}]}} ->
	    Strs = get_strings(Forms),
	    output_strings(Strs, Mod, Out);
	{ok,{Mod,_}} ->
	    io:format("~p: Missing or wrong version of abstract format.\n",
		      [Mod]);
	{error,{Mod,Error}} ->
	    io:format("~p: Problems: ~p\n", [Mod,Error])
    end.

get_strings(Forms) ->
    put(?MODULE, []),
    get_str_1(Forms),
    erase(?MODULE).

get_str_1({call,_,{remote,_,{atom,_,wings_lang},{atom,_,str}},
	   [{tuple,_,[{atom,_,_},{atom,_,N},{_,_,Key}]},
	    {string,_,S}]}) ->
    put(?MODULE, [{N,{Key,S}}|get(?MODULE)]);
get_str_1({string,_,_}) -> ok;			%Optimization only.
get_str_1([H|T]) ->
    get_str_1(H),
    get_str_1(T);
get_str_1(Tuple) when is_tuple(Tuple) ->
    get_str_1(tuple_to_list(Tuple));
get_str_1(_) -> ok.

output_strings([], _, _) -> ok;
output_strings(S0, Mod, Out) ->
    S1 = sofs:relation(S0),
    S2 = sofs:relation_to_family(S1),
    S = sofs:to_external(S2),
    output_strings({Mod, S}, Out).

output_strings({Mod, S}, Out) ->
    io:format(Out, "{~p,\n [\n", [Mod]),
    foldl(fun(E, Sep) -> output_strings_1(E, Sep, Out) end, [], S),
    io:put_chars(Out, "\n ]}.\n").

output_strings_1({Name,List}, OuterSep, Out) ->
    io:put_chars(Out, OuterSep),
    io:format(Out,    "  {~p,\n", [Name]),
    io:put_chars(Out, "   [\n"),
    foldl(fun(E, Sep) -> output_string(E, Sep, Out) end, [], List),
    io:format(Out,    "\n   ]}", []),
    ",\n".

output_string({Key,Str}, Sep, Out) ->
    io:put_chars(Out, Sep),
    io:format(Out, "    {~p,~p}", [Key,Str]),
    ",\n".

get_en_template(Name) ->
    get_en_template_1(reverse(filename:rootname(Name))).

get_en_template_1([$_|T]) ->
    reverse(T, "_en.lang");
get_en_template_1([_|T]) ->
    get_en_template_1(T).
    
%%%
%%% Parse transform follows.
%%%

-define(STRINGS, wings_lang_transform_strings).
-define(FUNCTION_NAME, wings_lang_transform_function_name).
-define(ERRORS, wings_lang_transform_errors).
-define(FILENAME, wings_lang_transform_filename).

parse_transform(Forms0, _Opts) ->
    put(?ERRORS, []),
    put(?STRINGS, []),
    Forms = transform(Forms0),
    check_strings(erase(?STRINGS)),
    erase(?FILENAME),
    case erase(?ERRORS) of
	[] ->
	    Forms;
 	Errors ->
	    {error,reverse(Errors),[]}
    end.

format_error({duplicate_key,Key,Line}) ->
    io_lib:format("Key ~p already used for a different string at line ~p",
		  [Key,Line]).

transform({attribute,_,file,{Filename,_}}=Form) ->
    put(?FILENAME, Filename),
    Form;
transform({function,L,Name,Arity,Cs}) ->
    put(?FUNCTION_NAME, Name),
    {function,L,Name,Arity,transform(Cs)};
transform({call,L,{remote,_,{atom,_,wings_lang},{atom,_,str}}=Rem,
	   [{tuple,_,[{atom,_,M}=Mod,Key]},
	    {string,_,S}=Str]}) ->
    FunName = get(?FUNCTION_NAME),
    K = literal_key(Key),
    add_string({{M,FunName,K},{S,L}}),
    {call,L,Rem,[{tuple,L,[Mod,{atom,L,FunName},Key]},Str]};
transform({string,_,_}=Str) ->
    Str;
transform({Tag,Line}=Tuple) when is_constant(Tag), is_integer(Line) ->
    Tuple;
transform({Tag,Line,Term}=Tuple)
  when is_constant(Tag), is_integer(Line), is_constant(Term) ->
    Tuple;
transform({Tag,Line,Term}) when is_constant(Tag), is_integer(Line) ->
    {Tag,Line,transform(Term)};
transform([H|T]) ->
    [transform(H)|transform(T)];
transform(Tuple) when is_tuple(Tuple) ->
    transform_tuple(1, size(Tuple), Tuple, []);
transform(Term) -> Term.

transform_tuple(I, Size, Tuple, Acc) when I =< Size ->
    E = transform(element(I, Tuple)),
    transform_tuple(I+1, Size, Tuple, [E|Acc]);
transform_tuple(_, _, _, Acc) ->
    list_to_tuple(reverse(Acc)).

check_strings(Strs0) ->
    Strs1 = sofs:relation(Strs0),
    Strs2 = sofs:relation_to_family(Strs1),
    Strs = sofs:to_external(Strs2),
    foreach(fun check_string/1, Strs).

check_string({_,[_]}) -> ok;
check_string({{_,_,Key},[{Str,Line}|Ss]}) ->
    foreach(fun({S,_}) when S =:= Str -> ok;
	       ({_,L}) -> add_error(L, {duplicate_key,Key,Line})
	    end, Ss).

literal_key({atom,_,A}) -> A;
literal_key({integer,_,I}) -> I;
literal_key(Term) ->
    add_error(element(2, Term), bad_literal),
    bad_literal.
		   
add_string(S) ->
    put(?STRINGS, [S|get(?STRINGS)]).

add_error(L, E0) ->
    E = {get(?FILENAME),[{L,?MODULE,E0}]},
    put(?ERRORS, [E|get(?ERRORS)]).
