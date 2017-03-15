%%
%%  tools.erl --
%%
%%     Tools for translation.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: tools.erl,v 1.2 2005/09/26 09:34:56 giniu Exp $
%%
-module(tools).

%% Translation support tools.
-export([generate_template/1,generate_template_files/1,
	 diff/1,diff/2,diff_lang_files/1]).

%% Parse transform ASPI (called by compiler).
-export([parse_transform/2,format_error/1]).

-import(lists, [reverse/1,reverse/2,foreach/2,foldl/3]).

%%%%%%%%%% Tools %%%%%%%%%%%

%%% The function tries to load the preferences files without getting an error if it isn't unicode
%%% It's applied to the .lang files
local_consult(PrefFile) ->
    case file:consult(PrefFile) of
        {error,{_,file_io_server,invalid_unicode}} ->
            latin1_file_to_unicode(PrefFile),
            file:consult(PrefFile);
        Res -> Res
    end.

%%% convert file from latin1 to unicode
latin1_file_to_unicode(PrefFile) ->
    {ok, Latin1} = file:read_file(PrefFile),
    Utf8 = unicode:characters_to_binary(Latin1, latin1, utf8),
    ok = file:write_file(PrefFile, Utf8).


diff_lang_files(Dir) ->
    Ns = filelib:wildcard(filename:join(Dir, "*.lang")),
    R0 = [{get_en_template(N),N} || N <- Ns],
    R1 = sofs:relation(R0),
    R2 = sofs:relation_to_family(R1),
    R = sofs:to_external(R2),
    diff_files_1(R, []),
    erlang:halt().

diff_files_1([{EngTemplateFile,LangFiles}|T], Acc0) ->
    case local_consult(EngTemplateFile) of
	{ok,Eng} ->
	    Acc = [fun() ->
			   diff_file(N, Eng),
			   io:format("~s ", [filename:basename(N)])
		   end || N <- LangFiles, N =/= EngTemplateFile] ++ Acc0,
	    diff_files_1(T, Acc);
	{error,Reason} ->
	    io:format("Failed to open ~s: ~s\n",
		      [EngTemplateFile,file:format_error(Reason)]),
	    erlang:halt(1)
    end;
diff_files_1([], Acc) ->
    io:format("Processing: "),
    prun(Acc),
    io:nl().

prun(Fs) ->
    N = erlang:system_info(schedulers),
    prun_1(Fs, N, 0).

prun_1([F|Fs], N, W) when W < N ->
    {_,_} = spawn_monitor(erlang, apply, [F,[]]),
    prun_1(Fs, N, W+1);
prun_1([], _, 0) -> ok;
prun_1([], N, W) when W < N -> prun_1([], W, W);
prun_1(Fs, N, N) ->
    receive
	{'DOWN',_,process,_,normal} ->
	    prun_1(Fs, N, N-1);
	{'DOWN',_,process,_,Error} ->
	    io:format("~p\n", [Error]),
	    exit(Error)
    end.

diff_file(LangFile, Eng) ->
    {ok,Lang} = local_consult(LangFile),
    {ok,Fd} = file:open(LangFile, [write,append]),
    OldGroupLeader = group_leader(),
    group_leader(Fd, self()),
    diff_2(Eng, Lang, LangFile),
    group_leader(OldGroupLeader, self()),
    file:close(Fd).

diff(LangFile) ->
    EngTemplFile = get_en_template(LangFile),
    diff(LangFile, EngTemplFile).
    
diff(LangFile, EngTmplFile) ->
    {ok,Lang} = local_consult(LangFile),
    {ok,Eng} = local_consult(EngTmplFile),
    diff_2(Eng, Lang, LangFile).

diff_2(Eng, Lang, LangFile) ->
    case diff(Eng, Lang, LangFile, [], []) of
	[] -> ok;
	Miss ->
	    io:nl(),
	    io:put_chars("%%\n"),
	    io:put_chars("%% The following strings have no translation.\n"),
	    io:put_chars("%%\n\n"),
	    Out = group_leader(),
	    [output_strings(M, Out) || M <- reverse(Miss)],
	    ok
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
diff([],Keys,_LF,Lev,Miss) ->
    [io:format("%% ~p is not used\n",[Lev++[Key]]) ||
	{Key,_} <- Keys],
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
    io:format("Writing: ~s\n", [filename:absname(OutFile)]),
    {ok,Out} = file:open(OutFile, [write]),
    io:put_chars(Out, "%% -*- mode:erlang; erlang-indent-level: 2 -*-\n"),
    try foreach(fun(File) -> scan_file(File, Out) end, Fs)
	after
	    file:close(Out)
	end.

generate_template_files([Dir]) ->
    Fs = filelib:wildcard(filename:join(Dir, "*.beam")),
    [do_generate_template_file(filename:rootname(F)) ||
	F <- Fs],
    erlang:halt().

do_generate_template_file(Base) ->
    OutFile = Base ++ "_en.lang",
    {ok,Out} = file:open(OutFile, [write]),
    io:put_chars(Out, "%% -*- mode:erlang; erlang-indent-level: 2 -*-\n"),
    Res = scan_file(Base ++ ".beam", Out),
    file:close(Out),
    if
	Res =:= no_strings ->
	    io:format("Nothing translatable in ~s\n",
		      [filename:absname(Base ++ ".beam")]),
	    file:delete(OutFile);
	true ->
	    io:format("Wrote ~s\n", [filename:absname(OutFile)]),
	    ok
    end.

scan_file(Filename, Out) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
	{ok,{Mod,[{abstract_code,{raw_abstract_v1,Forms}}]}} ->
	    case get_strings(Forms) of
		[] ->
		    no_strings;
		Strs ->
		    output_strings(Strs, Mod, Out),
		    ok
	    end;
	{ok,{Mod,_}} ->
	    io:format("~p: Missing or wrong version of abstract format.\n",
		      [Mod]),
	    no_strings;
	{error,{Mod,Error}} ->
	    io:format("~p: Problems: ~p\n", [Mod,Error]),
	    no_strings
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

-define(ERRORS, wings_lang_transform_errors).
-define(FILENAME, wings_lang_transform_filename).

parse_transform(Forms, _Opts) ->
    put(?ERRORS, []),
    Strings = collect_strings(Forms),
    check_strings(Strings),
    erase(?FILENAME),
    case erase(?ERRORS) of
	[] ->
	    Forms;
 	Errors ->
	    {error,reverse(Errors),[]}
    end.

format_error({duplicate_key,Key,Line}) ->
    io_lib:format("Key ~p already used for a different string at line ~p",
		  [Key,Line]);
format_error(bad_key) ->
    "bad key in ?__() or ?STR()".

collect_strings({attribute,_,file,{Filename,_}}) ->
    put(?FILENAME, Filename),
    [];
collect_strings({function,_Line,_Name,_Arity,Cs}) ->
    collect_strings(Cs);
collect_strings({call,L,{remote,_,{atom,_,wings_lang},{atom,_,str}},
	   [{tuple,_,[{atom,_,M},{atom,_,FunName},Key]},
	    {string,_,S}]}) ->
    K = literal_key(Key),
    [{{M,FunName,K},{S,L}}];
collect_strings({string,_,_}) ->
    [];
collect_strings({Tag,Line}) when is_atom(Tag), is_integer(Line) ->
    [];
collect_strings({Tag,Line,Term}) when is_atom(Tag), is_integer(Line) ->
    collect_strings(Term);
collect_strings([H|T]) ->
    collect_strings(H) ++ collect_strings(T);
collect_strings(Tuple) when is_tuple(Tuple) ->
    collect_from_tuple(1, tuple_size(Tuple), Tuple);
collect_strings(_) -> [].

collect_from_tuple(I, Size, Tuple) when I =< Size ->
    collect_strings(element(I, Tuple)) ++ collect_from_tuple(I+1, Size, Tuple);
collect_from_tuple(_, _, _) -> [].

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
    add_error(element(2, Term), bad_key),
    bad_key.
		   
add_error(L, E0) ->
    E = {get(?FILENAME),[{L,?MODULE,E0}]},
    put(?ERRORS, [E|get(?ERRORS)]).
