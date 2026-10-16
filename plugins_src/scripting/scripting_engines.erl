%%
%%  Scripting for Shapes Engines
%%
%%  Read script engine config files and set up the arguments for when
%%  a script interpreter is invoked.
%%
%%  Copyright 2024-2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(scripting_engines).

-export([init/1, all_engines/0, script_interp/3, extra_script_dirs/0]).

-include_lib("kernel/include/file.hrl").

init(DirName) ->
    InitDir = filename:absname(code:where_is_file(DirName)),
    catch ets:new(script_eng, [public,ordered_set,named_table]),
    find_engines(InitDir),
    find_script_dirs(InitDir).

all_engines() ->
    [{all, AllEng}] = ets:lookup(script_eng, all),
    AllEng.

script_interp(Type, Settings, InitScriptsDir) ->
    case init_file(Type) of
        false ->
            Interpreter = none,
            Arguments = [],
            InitFile = "";
        InitFile0 when is_list(InitFile0) ->
            Atom1 = list_to_atom("setting_" ++ Type ++ "_int_path"),
            Atom2 = list_to_atom("setting_" ++ Type ++ "_arguments"),
            Interpreter = auto_fill_int_path(Type,
                proplists:get_value(Atom1, Settings, "")),
            Arguments = auto_fill_arguments(Type, Interpreter,
                string:split(proplists:get_value(Atom2, Settings, ""), " ", all),
                InitScriptsDir),
            InitFile = filename:join(InitScriptsDir, InitFile0)
    end,
    {Interpreter, Arguments, InitFile}.

auto_fill_int_path(Type, Str) ->
    Atom = list_to_atom("setting_autointrp_" ++ Type),
    auto_fill_int_path(Type, Str, Atom, int_list(Type)).
auto_fill_int_path(_, Str, Atom, List) ->
    case Str of
        "" ->
            find_first_interpreter(Atom, List);
        _ ->
            Str
    end.

auto_fill_arguments(Type, Interpreter, Extra, BaseDir) ->
    Interpreter0 = filename:basename(filename:rootname(Interpreter)),
    Extra_1 = auto_fill_arguments_no_empty(Extra),
    case get_auto_fill(Type, Interpreter0) of
        false -> Extra_1;
        Args ->
            [ expand_arg(A, BaseDir) || A <- Args]
            ++ Extra_1
    end.

expand_arg("%BASEDIR%" ++ Path, BaseDir) ->
    case Path of
        [] ->
            BaseDir;
        [C|Path1] when C =:= $/; C =:= $\\ ->
            filename:join(BaseDir,Path1)
    end;
expand_arg(A, _) when is_list(A) -> A;
expand_arg({wrap, A, B}, BaseDir) when is_list(A) ->
    lists:flatten(string:replace(A, "%s", expand_arg(B, BaseDir), all));
expand_arg({concat, A, B}, BaseDir) ->
    expand_arg(A, BaseDir) ++ expand_arg(B, BaseDir);
expand_arg({esc, A}, BaseDir) ->
    esc(expand_arg(A, BaseDir));
expand_arg({absname, A, B}, BaseDir) ->
    filename:absname(expand_arg(A, BaseDir), expand_arg(B, BaseDir)).

esc(File) ->
    File_1 = lists:flatten(string:replace(File, "\\", "\\\\", all)),
    lists:flatten(string:replace(File_1, "\"", "\\\"", all)).

auto_fill_arguments_no_empty(Extra) ->
    case Extra of
        [[]] ->
            [];
        _ ->
            Extra
    end.

find_first_interpreter(_AutoSetting, List) ->
    Found = find_first_interpreter_1(List),
    Found.
find_first_interpreter_1([TryCmd | List]) ->
    case os:find_executable(TryCmd) of
        Found when is_list(Found) -> TryCmd;
        false ->
            find_first_interpreter_1(List)
    end;
find_first_interpreter_1([]) ->
    "".

%%
%%

find_engines(Path) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            Found = 
                [ read_eng_conf(Path,AName) 
                || AName <- Filenames],
            Found_1 = [A || A <- Found, A =/= false ],
            ets:insert(script_eng, {all, Found_1}),
            ok;
        _ ->
            false
    end.

read_eng_conf(Path,Name) ->
    case filename:extension(Name) of
        ".script-init-conf" ->
            case file:consult(filename:join(Path,Name)) of
                {ok, [Tuple|_]} ->
                    {A,B,C,D,E} = Tuple,
                    engine_conf(A,B,C,D,E),
                    {A,B}
            end;
        _ -> false
    end.

engine_conf(Type, _HasArgs, InitFile, Commands, Extra) ->
    ets:insert(script_eng, {{init_file,Type},InitFile}),
    ets:insert(script_eng, {{int_list,Type}, Commands}),

    [ engine_conf_auto_fill(Type,Cmd,Args)
    || {Cmd,Args} <- proplists:get_value(auto_fill, Extra, []) ].

engine_conf_auto_fill(Type,Cmd,Args) ->
    ets:insert(script_eng, {{auto_fill,{Type,Cmd}}, Args}).

init_file(Type) ->
    case ets:lookup(script_eng, {init_file, Type}) of
        [{_,InitFile}] -> InitFile;
        _ -> false
    end.

int_list(Type) ->
    case ets:lookup(script_eng, {int_list, Type}) of
        [{_,IntList}] -> IntList;
        _ -> false
    end.

get_auto_fill(Type,Cmd) ->
    case ets:lookup(script_eng, {auto_fill, {Type,Cmd}}) of
        [{_,Args}] -> Args;
        _ -> false
    end.

%%
%%

find_script_dirs(InitDir) ->
    [_|Dir0] = lists:reverse(filename:split(InitDir)),
    Dir = filename:join(lists:reverse(Dir0)),

    ScriptDir = filename:join(Dir, "scripts"),
    case file:read_file_info(ScriptDir) of
        {ok,#file_info{type=directory}=_} ->
            ets:insert(script_eng, {scriptdir1, [ScriptDir]});
        _ ->
            ok
    end,
    ScriptPaths = filename:join(InitDir, "paths"),
    case file:list_dir(ScriptPaths) of
        {ok,List} ->
            Paths_0 =
                [read_path_file(filename:join(ScriptPaths, File))
                    || File <- List],
            Paths = lists:append(Paths_0),
            ets:insert(script_eng, {scriptdirs, Paths});
        _ ->
            ok
    end.

read_path_file(File) ->
    case filename:extension(File) of
        ".list" ->
            case file:read_file(File) of
                {ok, List0} ->
                    List = [read_path_file_1(A)
                        || A <- string:split(binary_to_list(List0), "\n", all)],
                    [A || A <- List, A =/= false, A =/= ""];
                _ -> []
            end;
        _ -> []
    end.

read_path_file_1("#" ++ _) -> false;
read_path_file_1(File) ->
    Path = string:trim(File),
    case Path of
        "" -> false;
        "/" ++ _ ->
            read_path_file_2(Path);
        "~/" ++ Path_1 ->
            read_path_file_2(filename:join(os:getenv("HOME"), Path_1));
        _ -> false
    end.

read_path_file_2(File) ->
    case file:read_file_info(File) of
        {ok,#file_info{type=directory}=_} ->
            File;
        _ ->
            false
    end.

%%
%%

%% Get extra script directories apart from user supplied ones.
%%
extra_script_dirs() ->
    lists:append([
        case ets:lookup(script_eng, Atom) of
            [{_,List}] -> List;
            _ -> []
        end
        || Atom <- [scriptdir1, scriptdirs]]).

