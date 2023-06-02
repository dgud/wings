#!/usr/bin/env escript
%% -*- erlang -*-

main(Args) ->
    argparse:run(Args, cli(), #{progname => "dialyze"}).

cli() ->
    #{
      arguments => [ #{name => verbose, long => "-verbose", short => $v, type => boolean} ],
      handler => fun(Args) -> dialyze(Args) end
     }.

dialyze(Opts) ->
    Verbose = maps:get(verbose, Opts, false),
    CacheDir = filename:basedir(user_cache, "erlang"),
    IpltFile = filename:join(CacheDir, "wings.dialyzer_iplt"),
    Verbose andalso io:format("Using cache at: ~p~n", [IpltFile]),
    os:putenv("DIALYZER_IPLT", IpltFile),
    Analyze = [core, plugins, tools],
    Dirs = lists:flatmap(
             fun(core) ->
                     [filename:join(wings_dir(), "ebin")];
                (plugins) ->
                     get_plugin_dirs();
                (tools) ->
                     [filename:join(wings_dir(), "intl_tools")]
             end, Analyze),

    CL = filename:join([wings_dir(),"_deps", "cl", "ebin"]),

    Verbose andalso io:format("Analyzing files in: ~p~n", [Dirs]),
    Apps = [erts, kernel, stdlib, wx, xmerl, CL, tools],
    AppDirs = get_lib_dir(Apps),

    DOpts = [{files_rec,AppDirs ++ Dirs},
             {warning_files_rec, Dirs},
             {warnings, [no_improper_lists]},
             {analysis_type, incremental},
             {report_mode, if Verbose -> verbose; true -> normal end},
             {metrics_file, filename:join(CacheDir, "wings_dialyzer.metrics")}
            ],

    Res = dialyzer:run(DOpts),
    case dialyze_res(Res) of
        0 when Verbose -> io:format("No errors or warnings~n",[]);
        0 -> ok;
        N -> io:format("Found ~w errors or warnings~n", [N]),
             erlang:halt(1)
    end.

dialyze_res(Ws) ->
    File = dump_file(),
    file:write_file(File, term_to_binary(Ws)),
    diana().

diana() ->
    {ok,B} = file:read_file(dump_file()),
    case binary_to_term(B) of
	Ws when is_list(Ws) ->
	    diana_1(lists:keysort(2, Ws), 0)
    end.

diana_1([W|Ws], N) ->
    io:format("~s", [dialyzer:format_warning(W)]),
    diana_1(Ws, N+1);
diana_1([], N) -> N.

dump_file() ->
    filename:join(filename:basedir(user_cache, "erlang"), "wings_dialyzer_warnings.raw").

get_plugin_dirs() ->
    PluginDir = filename:join(wings_dir(), "plugins"),
    {ok, PluginDirs} = file:list_dir(PluginDir),
    lists:filtermap(fun(Dir0) ->
                            Dir = filename:join(PluginDir, Dir0),
                            Pattern = filename:join(Dir, "*.beam"),
                            filelib:is_dir(Dir) andalso
                                filelib:wildcard(Pattern) /= [] andalso
                                {true, Dir}
                    end, PluginDirs).

get_lib_dir(Apps) ->
    get_lib_dir(Apps, []).

get_lib_dir([H|T], Acc) ->
    NewElem =
        case code:lib_dir(H) of
            {error, bad_name} -> H;
            LibDir when H =:= erts -> % hack for including erts in an un-installed system
                EbinDir = filename:join([LibDir,"ebin"]),
                case file:read_file_info(EbinDir) of
                    {error,enoent} ->
                        filename:join([LibDir,"preloaded","ebin"]);
                    _ ->
                        EbinDir
                end;
            LibDir -> filename:join(LibDir,"ebin")
        end,
    get_lib_dir(T, [NewElem|Acc]);
get_lib_dir([], Acc) ->
    lists:reverse(Acc).

wings_dir() ->
    case code:lib_dir(wings) of
        {error, bad_name} ->
            WhoAmI = filename:absname(escript:script_name()),
            WingsTop = filename:dirname(filename:dirname(WhoAmI)),
            %% io:format("NAME: ~p => ~p~n", [WhoAmI, WingsTop]),
            WingsTop;
        Dir ->
            Dir
    end.
