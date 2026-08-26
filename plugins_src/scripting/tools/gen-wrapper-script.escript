#!/usr/bin/env escript

%%
%%  Generates we interface functions for python and scheme.
%%
%%  Copyright 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Usage:
%% escript gen-wrapper-script.escript <path/callable.conf> <path/modnames> <py|scm>
%%
%% <path/callable.conf> = path to callable.conf
%% <path/modnames> = path to modnames file
%% <py|scm> = py for python source, scm for scheme source
%%
%% Script is generated to standard output

changes_we(_Arity,Args,Returns) ->
    lists:member(we, Args) andalso
        changes_we_1(Returns).

changes_we_1(we) ->
    true;
changes_we_1(List) when is_list(List) ->
    lists:member(we, List);
changes_we_1(_) ->
    false.

wings_we(py, false) ->
    "wings_we__get";
wings_we(py, true) ->
    "wings_we__change";
wings_we(scm, false) ->
    "wings-we";
wings_we(scm, true) ->
    "wings-we!".

arg_list(List,Cap) ->
    arg_list(List,Cap,1).
arg_list([],_,_) ->
    [];
arg_list([Atom|List],Cap,I) when Atom =:= we; Atom =:= st ->
    arg_list(List,Cap,I);
arg_list([_|List],Cap,I) ->
    [ arg_list_1(I,Cap) |
      arg_list(List,Cap,I+1) ].
arg_list_1(I,false) ->
    "arg" ++ [$0 + I];
arg_list_1(I,true) ->
    "Arg" ++ [$0 + I].


arg_types(List,Cap) ->
    arg_types(List,Cap,1).
arg_types([],_,_) ->
    [];
arg_types([Atom|List],Cap,I) when Atom =:= we; Atom =:= st ->
    arg_types(List,Cap,I);
arg_types([Type|List],Cap,I) ->
    [ arg_types_1(Type) |
      arg_types(List,Cap,I+1) ].
arg_types_1(Type) ->
    atom_to_list(Type).

arg_types_ret(we) -> [];
arg_types_ret(Arg) when is_atom(Arg) ->
    arg_types_1(Arg);
arg_types_ret(Return0) when is_list(Return0) ->
    Return = lists:filter(
        fun (Atom) when Atom =:= we; Atom =:= st -> false; (_) -> true end,
        Return0),
    arg_types(Return,true).

str_excl(true) ->
    "!";
str_excl(_) ->
    "".

str_excl_py(CommentStr, true) ->
    [CommentStr, "Changes #we{}"];
str_excl_py(_, _) ->
    "".


arg_if_py([]) ->
    "";
arg_if_py(ArgList0) ->
    lists:flatten(lists:join(", ",ArgList0)) ++ ", ".

arg_if_py2([]) ->
    "";
arg_if_py2(ArgList0) ->
    ", " ++ lists:flatten(lists:join(", ",ArgList0)).

arg_if_scm([]) ->
    "";
arg_if_scm(ArgList0) ->
    " " ++ lists:flatten(lists:join(" ", ArgList0)).

arg_list_smallest([{_,{Args,_}}|List]) ->
    arg_list_smallest(List, Args).
arg_list_smallest([], Args0) ->
    Args0;
arg_list_smallest([{_,{Args,_}}|List], Args0) ->
    case length(Args0) >= length(Args) of
        true ->
            arg_list_smallest(List, Args);
        _ ->
            arg_list_smallest(List, Args0)
    end.

arg_types_string(CommentStr,_ArgsL0,Return) ->
    Return1 = arg_types_ret(Return),
    Str0 =
        [%[[A, " "] || A <- arg_types(ArgsL0,true)],
         case Return1 of [] -> []; _ -> ["",Return1] end], % "-> "
    Str = string:trim(lists:flatten(Str0)),
    case Str of
        "" ->
            "";
        _ ->
            [CommentStr,string:trim(lists:flatten(Str))]
    end.

print_fun(_,{_,[]},_) ->
    ok;
print_fun(py,{_,[{{Mod,Name,Arity}, {Args,Return}}]},MList) ->
    {ok, MNames0} = maps:find(Mod,MList),
    MNames = [atom_to_list(A) || A <- MNames0 ++ [Name]],
    ChangesWe = changes_we(Arity,Args,Return),
    ArgList = lists:join(", ",arg_list(Args,false)),
    Types = arg_types_string(" ## ",Args,Return),
    ArgList1 = case ArgList of
        [] -> "";
        _ ->
            [", ", ArgList]
    end,
    io:format("def ~s(~s):~s~n", [lists:join("__",MNames),ArgList,str_excl_py(" ## ",ChangesWe)]),
    io:format("    return ~s('~w', '~w'~s)~s~n", [wings_we(py,ChangesWe),Mod,Name,ArgList1,Types]),
    ok;
print_fun(py,{_,List0},MList) ->
    List = lists:reverse(List0),
    [{{Mod0,Name0,Arity0},{Args0,Return0}}|_] = List,
    {ok, MNames0} = maps:find(Mod0,MList),
    MNames = [atom_to_list(A) || A <- MNames0 ++ [Name0]],
    ChangesWe = changes_we(Arity0,Args0,Return0),
    ArgList0 = arg_list(arg_list_smallest(List),false),
    io:format("def ~s(~s*args):~s~n",
        [ lists:join("__",MNames),
          arg_if_py(ArgList0),str_excl_py(" ## ",ChangesWe)]),
    io:format("    argsize = len(args)~n", []),

    each_arg(fun
        (First, Extra, {{Mod,Name,_Arity}, {ArgsL0,Return}}) ->
            Types = arg_types_string(" ## ",ArgsL0,Return),
            case First of
                true ->
                    io:format("    if argsize == ~w:~n", [length(Extra)]);
                _ ->
                    io:format("    elif argsize == ~w:~n", [length(Extra)])
            end,
            lists:foldl(fun (Arg, I) ->
                io:format("        ~s = args[~w]~n", [Arg, I]),
                I+1
            end, 0, Extra),
            io:format("        return ~s('~w', '~w'~s~s)~s~n",
                [ wings_we(py,ChangesWe),
                  Mod,
                  Name,
                  arg_if_py2(ArgList0),
                  arg_if_py2(Extra),
                  Types])
    end, ArgList0, false, List),

    io:format("    else:~n", []),
    io:format("        raise 'invalid arguments'~n", []),
    ok;

print_fun(scm,{_,[{{Mod,Name,Arity},{Args,Return}}]},MList) ->
    {ok, MNames0} = maps:find(Mod,MList),
    ChangesWe = changes_we(Arity,Args,Return),
    MNames = [atom_to_list(A) || A <- MNames0 ++ [Name]],
    ArgList = arg_list(Args,true),
    Types = arg_types_string(" ;; ",Args,Return),
    io:format("(define (~s~s~s) (~s '~w '~w~s))~s~n",
        [ lists:join(":",MNames),
          str_excl(ChangesWe),
          case ArgList of [] -> ""; _ -> " " ++ lists:join(" ",ArgList) end,
          wings_we(scm,ChangesWe),
          Mod,
          Name,
          arg_if_scm(ArgList),
          Types]),
    ok;
print_fun(scm,{_,List0},MList) ->
    List = lists:reverse(List0),
    [{{Mod0,Name0,Arity0},{Args0,Return0}}|_] = List,
    {ok, MNames0} = maps:find(Mod0,MList),
    ChangesWe = changes_we(Arity0,Args0,Return0),
    MNames = [atom_to_list(A) || A <- MNames0 ++ [Name0]],
    ArgList0 = arg_list(arg_list_smallest(List),true),

    io:format("(define (~s~s~s . Args)~n",
        [ lists:join(":",MNames),
          str_excl(ChangesWe),
          arg_if_scm(ArgList0)]),
    io:format("    (case (length Args)~n", []),

    each_arg(fun
        (_First, [], {{Mod,Name,_Arity}, {ArgsL0,Return}}) ->
            Types = arg_types_string(" ;; ",ArgsL0,Return),
            io:format("        ((0) (~s '~w '~w~s))~s~n",
                [ wings_we(scm,ChangesWe),
                  Mod,
                  Name,
                  arg_if_scm(ArgList0),
                  Types]);

        (_First, Extra, {{Mod,Name,_Arity}, {ArgsL0,Return}}) ->
            Types = arg_types_string(" ;; ",ArgsL0,Return),
            io:format("        ((~w) (let-values (((~s) (apply values Args) ))~n",
                [ length(Extra), lists:join(" ",Extra) ]),
            io:format("                (~s '~w '~w~s~s)))~s~n",
                [ wings_we(scm,ChangesWe),
                  Mod,
                  Name,
                  arg_if_scm(ArgList0),
                  arg_if_scm(Extra),
                  Types])

    end, ArgList0, true, List),

    io:format("    ))~n", []),
    ok.

each_arg(Fun, Args0, UVar, List) ->
    each_arg(Fun, Args0, UVar, List, true).

each_arg(_, _, _, [], _) ->
    ok;
each_arg(Fun, Args0, UVar, [{{_,_,_Arity},{Args,_}}=Attr|List], First) ->
    ArgList = lists:nthtail(length(Args0),arg_list(Args,UVar)),
    Fun(First, ArgList, Attr),
    each_arg(Fun, Args0, UVar, List, false).
    

group_by_module_1({{Mod,Name,_},_}=Attr, {{Mod1,Name1},Acc}, _Fun)
  when Name =:= Name1, Mod =:= Mod1 ->
    {{Mod,Name},[Attr|Acc]};
group_by_module_1({{Mod,Name,_},_}=Attr, {{Mod1,Name1},_}=Acc, Fun) when Name =/= Name1 ->
    Fun(Acc),
    case Mod1 =:= Mod of
        true ->
            ok;
        false ->
            io:format("~n",[])
    end,
    {{Mod,Name},[Attr]}.

group_by_module(FunList,ModuleNames,Type) ->
    ModuleNames_1 = maps:from_list(ModuleNames),
    Fun = case Type of
        "py" ->
            fun (A) -> print_fun(py,A,ModuleNames_1) end;
        "scm" ->
            fun (A) -> print_fun(scm,A,ModuleNames_1) end
    end,
    Fun(lists:foldl(
        fun (A,B) ->
            group_by_module_1(A,B,Fun)
        end, {{none,none},[]}, FunList)).

main([File,ModNames,Type]) ->
    {ok, [FunList]} = file:consult(File),
    {ok, [ModuleNames]} = file:consult(ModNames),
    group_by_module(FunList,ModuleNames,Type).


