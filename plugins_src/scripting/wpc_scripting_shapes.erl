%%
%%  Scripting for Shapes  (Scheme and Python)
%%
%%  This plugin makes it possible to create shape makers, importers
%%  and exporters in Wings3D using scripting languages like Scheme and
%%  Python.
%%  Requires Gauche for Scheme runtime
%%  Requires Python 3.6 or later
%%
%%  Copyright 2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_scripting_shapes).
-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-record(command_rec, {
    wscrcont,
    scrfile :: filename:filename(),
    scrtype :: string(),
    extrafileinputs=[]
}).

-include("wpc_scripting_shapes_btn_refresh.hrl").

-type wings_op_mode() :: atom().

-ifdef(DEBUG_1).
-define(DEBUG_FMT(A,B), io:format(A,B)).
-else.
-define(DEBUG_FMT(A,B), none).
-endif.

%% For the queries mini language
-record(crun_state, {
    p = none, %% Current value when in etp_run
    sett_vars = [], %% Variables such as for script_params, import_params, export_params
    temp_vars = []  %% Temporary variable storage
}).


init() ->
    true.

menu({shape}, []) ->
    case wpa:pref_get(?MODULE, setting_enable, true) of
        true -> shape_from_script_menu();
        _    -> []
    end;
menu({shape}, Menu) ->
    case wpa:pref_get(?MODULE, setting_enable, true) of
        true -> Menu ++ [separator|shape_from_script_menu()];
        _    -> Menu
    end;
menu({file,import}, Menu) ->
    case    wpa:pref_get(?MODULE, setting_enable, true)
    andalso wpa:pref_get(?MODULE, setting_enable_import, true) of
        true -> Menu ++ import_from_script_menu();
        _    -> Menu
    end;
menu({file,export}, Menu) ->
    case    wpa:pref_get(?MODULE, setting_enable, true)
    andalso wpa:pref_get(?MODULE, setting_enable_export, true) of
        true -> Menu ++ export_from_script_menu();
        _    -> Menu
    end;
menu({file,export_selected}, Menu) ->
    case    wpa:pref_get(?MODULE, setting_enable, true)
    andalso wpa:pref_get(?MODULE, setting_enable_export, true) of
        true -> Menu ++ export_from_script_menu();
        _    -> Menu
    end;
    
menu({Mode},Menu) when Mode == body ->
    case    wpa:pref_get(?MODULE, setting_enable, true)
    andalso wpa:pref_get(?MODULE, setting_enable_commands, true) of
        true -> Menu ++ command_from_script_menu(Mode);
        _    -> Menu
    end;
    


menu({edit,plugin_preferences}, Menu) ->
    Menu ++ [{?__(1,"Scripts Preference"), shapes_from_scripts_preference}];
menu({help}, Menu) ->
    case    wpa:pref_get(?MODULE, setting_enable, true) of
        true -> help_script_menu(Menu);
        _    -> Menu
    end;
menu(_, Menu) -> Menu.

shape_from_script_menu() ->
    [{?__(1,"Shape from Script"), {shape_from_script, mouse_choice()}}].
import_from_script_menu() ->
    [{?__(1,"Script-based Importers..."),
         {import_export_from_script, select}}].
export_from_script_menu() ->
    [{?__(1,"Script-based Exporters..."),
         {import_export_from_script, select}}].
command_from_script_menu(_Mode) ->
    [{?__(1,"Script-based Commands..."), {command_from_script, select}}].
    
help_script_menu(Menu) ->
    lists:reverse(help_script_menu(2, lists:reverse(Menu))).

help_script_menu(0, Menu) ->
    [{?__(1,"Using Scripts"), {help_info_script, [
        {?__(2,"Scripting Information"),script_basics,
              ?__(2,"Scripting Information")},
        {?__(3,"WSCR Resource File Guide (PDF)"),wscr_guide,
              ?__(3,"WSCR Resource File Guide (PDF)")}
       ]}}] ++ Menu;
help_script_menu(_, []) -> []; %% This shouldn't happen.
help_script_menu(N, [Item | Menu]) when Item =/= separator, N > 0 ->
    [Item | help_script_menu(N, Menu)];
help_script_menu(N, [separator | Menu]) when N > 0 ->
    [separator | help_script_menu(N-1, Menu)].



mouse_choice() ->
    fun (help,_) -> {?__(1,"From Script"), [], []};
        (1,_) -> {shape, {shape_from_script, select}};
        (2,_) -> {shape, {shape_from_script, select}};
        (3,_) -> {shape, {shape_from_script, select}};
        (_,_) -> ignore
    end.

-define(PATH_INIT_POSTFIX, "_init").

command({file, {import, {import_export_from_script, select}}}, fetch_props) ->
    %% TODO: Wings sends this to find out which importer can handle a given file extension.
    {[], fun () -> keep end};
command({file,{Op,{import_export_from_script, select}}}, St)
  when Op =:= import;
       Op =:= export;
       Op =:= export_selected ->
    case Op of
        import -> ScrTyp = import;
        _      -> ScrTyp = export
    end,
    cmd_select_from_list(setting_paths_import_export, ScrTyp,
        fun (Params, CR) ->
            import_export_from_script(Op, Params, CR, St)
        end);
command({file,{Op,{import_export_from_script, {#command_rec{}=CommandRec, Params}}}}, St)
  when Op =:= import;
       Op =:= export;
       Op =:= export_selected ->
    import_export_from_script(Op, Params, CommandRec, St);


command({shape, {shape_from_script, select}}, St) ->
    cmd_select_from_list(setting_paths_shapes, false,
        fun (Params, CR) ->
            make_shape_from_script(Params, CR, St)
        end);
command({shape, {shape_from_script, {#command_rec{}=CommandRec, Params}}}, St) ->
    make_shape_from_script(Params, CommandRec, St);
    
    
command({Op, {command_from_script, select}}, St)
  when Op =:= body ->
    cmd_select_from_list(setting_paths_commands, Op,
        fun(Params, CR) ->
            command_from_script(Op, Params, CR, St)
        end);
command({Op, {command_from_script, {#command_rec{}=CommandRec, Params}}}, St) ->
    command_from_script(Op, Params, CommandRec, St);
    
    
command({edit, {plugin_preferences, shapes_from_scripts_preference}}, St) ->
    Parent = wings_dialog:get_dialog_parent(),
    Defaults = [
        {setting_enable, true},
        {setting_enable_commands, false},
        {setting_enable_import, false},
        {setting_enable_export, false},
        {setting_paths_shapes, ""},
        {setting_paths_commands, ""},
        {setting_paths_import_export, ""},
        
        {setting_py_int_path, ""},
        {setting_scm_int_path, ""},
        {setting_scm_arguments, ""},
        {setting_show_tuple, false}
    ],
    Result = init_dlg_script_preference(
        [ {parent, Parent} ] ++
        [ {SettName, wpa:pref_get(?MODULE, SettName, SettDefault)}
            || {SettName, SettDefault} <- Defaults], Defaults),
    [
        wpa:pref_set(?MODULE, SettName, case proplists:get_value(SettName, Result) of
            undefined -> proplists:get_value(SettName, Defaults);
            Val -> Val
        end)
    || SettName <- [
        setting_enable, setting_enable_commands, 
        setting_enable_import, setting_enable_export, setting_paths_shapes,
        setting_paths_commands, setting_paths_import_export,
        setting_py_int_path, setting_scm_int_path, setting_scm_arguments, setting_show_tuple
    ]],
    St;
    
command({help, {help_info_script, script_basics}}, _St) ->
    {Title, Text} = help_information(script_basics),
    wings_dialog:info(Title, Text, []);
command({help, {help_info_script, Which}}, _St)
  when Which =:= wscr_guide;
       Which =:= help_info_script;
       Which =:= help_info_script ->
    %% The PDFs are with the init script files
    InitScriptsDir = filename:absname(code:where_is_file(
        atom_to_list(?MODULE) ++ ?PATH_INIT_POSTFIX)),
    open_pdf(pdf_filename(Which), InitScriptsDir),
    keep;

command(_, _) -> next.
    
cmd_select_from_list(DirsSetting, ScrOp, F) ->
    ScriptDirs = script_dirs(wpa:pref_get(?MODULE, DirsSetting, "")),
    case scripts_menu_select_plugin(ScriptDirs, ScrOp) of
        {load_script, ScriptFile, ScriptType, WSCRFile} ->
            Params = true,
            case read_wscr_file_content(WSCRFile) of
                {ok, WSCRContent} ->
                    F(Params, #command_rec{
                        wscrcont=WSCRContent,
                        scrfile=ScriptFile,
                        scrtype=ScriptType});
                _ ->
                    keep
            end;
        _ ->
            keep
    end.

    
help_information(script_basics) ->
    {
        ?__(1,"Scripting Information"),
        [
            ?__(2,"Scripting Information\n\nTo Be Written")
        ]
    }.

pdf_filename(Which) ->
    %% The filenames for the PDFs, using the lang file, different filenames
    %% can be used for other languages.
    %%
    case Which of
        wscr_guide   -> ?__(1, "WSCRGuide_en.pdf")
    end.

path_for_cmd(win, FullPath_1) ->
    FullPath_2 = lists:flatten(string:replace(FullPath_1, "/", "\\", all)),
    FullPath_3 = lists:flatten(string:replace(FullPath_2, "\"", "\"\"", all)),
    "\"" ++ FullPath_3 ++ "\"";
path_for_cmd(unix, FullPath_1) ->
    FullPath_2 = lists:flatten(string:replace(FullPath_1, "\\", "\\\\", all)),
    FullPath_3 = lists:flatten(string:replace(FullPath_2, "'", "\\'", all)),
    "'" ++ FullPath_3 ++ "'".

open_pdf(PDFFile, PathOfPDF) ->
    case os:type() of
        {win32, _} ->
            PathOfPDF_1 = path_for_cmd(win, PathOfPDF),
            CmdLine = "start /D " ++ PathOfPDF_1 ++ " " ++ PDFFile ++ "";
        {unix, macos} ->
            FullPath_1 = filename:absname(PDFFile, PathOfPDF),
            FullPath_2 = path_for_cmd(unix, FullPath_1),
            CmdLine = "finder " ++ FullPath_2;
        {unix, _} ->
            FullPath_1 = filename:absname(PDFFile, PathOfPDF),
            FullPath_2 = path_for_cmd(unix, FullPath_1),
            CmdLine = "xdg-open " ++ FullPath_2
    end,
    os:cmd(CmdLine).


askdialog(Ask, Title, ParamList, Templates, F) ->
    Dialog = [ askdialog_e(B) || B <- ParamList ] ++
        lists:append([askdialog_extra(T) || T <- Templates]),
    wpa:dialog(Ask, Title, Dialog, F).
    
askdialog_w_prev(Ask, Title, ParamList, Templates, F, St, FP) ->
    erlang:put(scripting_shape_fun, FP),
    Dialog = [ askdialog_e(B) || B <- ParamList ] ++
        lists:append([askdialog_extra(T) || T <- Templates]),
    Fun = fun({dialog_preview,Params}) ->
                  {preview,F(Params),St};
             (cancel) ->
                  FP(close),
                  erlang:erase(scripting_shape_fun),
                  St;
             (Params) ->
                  erlang:put(scripting_shape_fun_close, true),
                  {commit,F(Params),St}
          end,
    wings_dialog:dialog(Ask, Title, {preview, Dialog}, Fun).

    
askdialog_extra({import, Opts}) ->
    [wpa:dialog_template(?MODULE, import, Opts)];
askdialog_extra({export, Opts}) ->
    [wpa:dialog_template(?MODULE, export, Opts)].

askdialog_e({Text, Number}) ->
    {hframe,[{label,Text},{text,Number}]};
askdialog_e({Text, Number, C}) ->
    {hframe,[{label,Text},{text,Number,C}]}.


%% Start and run a script and get the return value
%%
run_script_once(ScriptType, ScriptFileName, ScriptParams, MoreParams, Settings, DefaultReturn) ->
    case erlang:get(scripting_shape_fun) of
        FP when is_function(FP) ->
            Ret = FP({run, ScriptParams, MoreParams, DefaultReturn}),
            case erlang:get(scripting_shape_fun_close) of
                true ->
                    FP(close),
                    erlang:erase(scripting_shape_fun_close),
                    erlang:erase(scripting_shape_fun);
                _ ->
                    ok
            end,
            Ret;
        undefined ->
            case run_script_w_preview(ScriptType, ScriptFileName, Settings) of
                {error, Err} -> {error, Err};
                {ok, F} when is_function(F) ->
                    Ret = F({run, ScriptParams, MoreParams, DefaultReturn}),
                    F(close),
                    Ret
            end
    end.


%% Start a script but return a function to run with parameters
%%
run_script_w_preview(ScriptType, ScriptFileName, Settings)
  when is_list(ScriptFileName) ->
    {ok, StrList} = load_lang_file(ScriptFileName, current_lang_code()),
    case get_script_pid(ScriptType, Settings) of
        {ok, PID} ->
            PID ! {load_script, ScriptFileName, StrList, self()},
            run_script_w_preview_1(PID);
        {error, Err} ->
            {error, Err}
    end;
run_script_w_preview(_ScriptType, none, _Settings) ->
    {error, ?__(1, "Script file not found, check if script has the same name "
                   "as the .wscr file, and 'type' matches script file type.")}.

run_script_w_preview_1(PID) ->
    case run_script_getting_data_once() of
        {[[{atom,<<"ok">>}|_]|_], _} ->
            PID ! next,
            {ok, fun
                ({run, ScriptParams, MoreParams, DefaultReturn}) ->
                    run_script_1(ScriptParams, MoreParams, DefaultReturn, PID);
                (close) ->
                    PID ! {close, self()},
                    receive
                        exited -> ok
                    after 20 ->
                        io:format("ERROR: Did not recv 'exited'~n", []),
                        ok
                    end
            end};
        Returned ->
            io:format("ERROR: Returned to run_script_1=~p~n", [Returned]),
            {error, {unexpected, Returned}}
    end.


run_script_1(ScriptParams, MoreParams, DefaultReturn, PID) ->
    ShowTupleDebug = wpa:pref_get(?MODULE, setting_show_tuple, false),
    PID ! {run_script, ScriptParams, MoreParams, self()},
    case run_script_getting_data_until_ret(DefaultReturn) of
        error -> {error, ?__(2,"No results")};
        keep -> {ok, keep};
        {[Lisp], _} ->
            PID ! next,
            Tuplefied = run_script_tuplefy(Lisp),
            if ShowTupleDebug =:= true ->
                    io:format("Tuplefied=~p~n", [Tuplefied]);
                true -> ok
            end,
            {ok, Tuplefied}
    end.


run_script_getting_data_once() ->
    ShowTupleDebug = wpa:pref_get(?MODULE, setting_show_tuple, false),
    run_script_getting_data_once(#crun_state{}, ShowTupleDebug).
run_script_getting_data_once(QueryState, ShowTupleDebug) ->
    receive 
        {reply, SendPID, {[[{atom, <<"%",_/binary>>} | _] | _]=Ret_1, _}}
          when is_pid(SendPID) ->
            QueryState_1 = run_script_getting_data_special(
                QueryState, ShowTupleDebug,
                SendPID, Ret_1),
            run_script_getting_data_once(QueryState_1, ShowTupleDebug);
        {reply, SendPID, Ret}
          when is_pid(SendPID) ->
            ?DEBUG_FMT("reply ~w~n",[Ret]),
            Ret;
        exited       ->
            error(exited)
    end.

run_script_getting_data_until_ret(CurrentReturn) ->
    run_script_getting_data_until_exit(CurrentReturn).

run_script_getting_data_until_exit(CurrentReturn) ->
    ShowTupleDebug = wpa:pref_get(?MODULE, setting_show_tuple, false),
    run_script_getting_data_until_exit(CurrentReturn, #crun_state{}, ShowTupleDebug).
run_script_getting_data_until_exit(CurrentReturn, QueryState, ShowTupleDebug) ->
    receive 
        {reply, SendPID, {[[{atom, <<"%ok">>} | _] | _], _}}
          when is_pid(SendPID) ->
            ?DEBUG_FMT("Got ok return~n",[]),
            CurrentReturn;
        {reply, SendPID, {[[{atom, <<"%",_/binary>>} | _] | _]=Ret_1, _}}
          when is_pid(SendPID) ->
            QueryState_1 = run_script_getting_data_special(
                QueryState, ShowTupleDebug,
                SendPID, Ret_1),
            run_script_getting_data_until_exit(CurrentReturn, QueryState_1, ShowTupleDebug);
        {reply, SendPID, Ret}
          when is_pid(SendPID) ->
            ?DEBUG_FMT("reply ~w~n",[Ret]),
            run_script_getting_data_until_exit(Ret, QueryState, ShowTupleDebug);
        exited       -> 
            ?DEBUG_FMT("exited~n",[]),
            CurrentReturn
    end.

run_script_getting_data_special(QueryState, ShowTupleDebug, SendPID, Ret_1) ->
    case run_script_tuplefy(Ret_1) of
        [{'%setvar', VarName, VarValue}|_]
          when is_list(VarName) ->
            QueryState_1 = etp_store_temp(binstr(VarName), VarValue, QueryState),
            if ShowTupleDebug =:= true ->
                    io:format("setvar: ~s = ~w~n", [VarName, VarValue]);
                true -> ok
            end,
            SendPID ! {reply_to_script, {ok}};
        
        [{'%query', QueryStr}|_]
          when is_list(QueryStr) ->
            {QueryRes, QueryState_1} = crun_pv(QueryStr, QueryState, []),
            if ShowTupleDebug =:= true ->
                    io:format("query: ~s -> ~p~n", [QueryStr, QueryRes]);
                true -> ok
            end,
            SendPID ! {reply_to_script, {ok, QueryRes}};
        
        %% An intermittent message sent to prevent the scripting plugin from
        %% timing out.
        [{'%keepalive', _Number}|_] ->
            QueryState_1 = QueryState;
        
        %% Send a progress bar message to the plugin to update the user
        %% on what the script is doing.
        [{'%pbmessage', Percent, Str}|_]
          when is_float(Percent), is_list(Str) ->
            wings_pb:update(Percent, Str),
            wings_pb:pause(),
            QueryState_1 = QueryState;
        
        %% Display a panel window with a text box that shows the results of 
        %% processing to the user. For example, a script that analyzes the
        %% model and displays statistics.
        [{'%resulttext', Text}|_]
          when is_list(Text) ->
            info_dialog(Text),
            QueryState_1 = QueryState;
        [{'%resulttext', Text, OptList}|_]
          when is_list(Text), is_list(OptList) ->
            info_dialog(Text),
            QueryState_1 = QueryState;
        
        _ ->
            QueryState_1 = QueryState,
            SendPID ! {reply_to_script, {error}}
    end,
    QueryState_1.

info_dialog(List) ->
    info_dialog("Info", List).
info_dialog(Title, [Str|_]=List)
  when is_list(Str) ->
    wings_dialog:info(Title, List, []);
info_dialog(Title, [C|_]=Str)
  when is_integer(C) ->
    info_dialog(Title, [Str]).
    

%%
%% Some schemes do not like having vector arrays containing data other than
%% strings and numbers, so if a list has a symbol as its first item,
%% we will tuple it ourselves.
%%
run_script_tuplefy({string, BString1}) when is_binary(BString1) ->
    unbinstr(BString1);
run_script_tuplefy({{atom, Atom1},Val}) when is_binary(Atom1) ->
    {list_to_atom(unbinstr(Atom1)), run_script_tuplefy(Val)};
run_script_tuplefy([{atom, <<"!list">>}]) ->
    [];
%% Specifies this is actually a list of atoms.
run_script_tuplefy([{atom, <<"!list">>}, {atom, TupleName} | Everything])
  when is_binary(TupleName) ->
    [list_to_atom(unbinstr(TupleName)) | run_script_tuplefy_args(Everything)];
run_script_tuplefy([{atom, TupleName} | Everything])
  when is_binary(TupleName) ->
    list_to_tuple([list_to_atom(unbinstr(TupleName)) | run_script_tuplefy_args(Everything)]);
run_script_tuplefy(Tuple)
  when is_tuple(Tuple),
       is_tuple(element(1, Tuple)),
       element(1,element(1,Tuple)) =:= atom  ->
    run_script_tuplefy(tuple_to_list(Tuple));
run_script_tuplefy([TupleName | Everything]) when is_binary(TupleName) ->
    list_to_tuple([list_to_atom(unbinstr(TupleName)) | run_script_tuplefy_args(Everything)]);
run_script_tuplefy([NonSymbol | _]=List)
  when is_number(NonSymbol);
       is_tuple(NonSymbol);
       is_list(NonSymbol) ->
    lists:map(fun(A1) -> run_script_tuplefy(A1) end, List);
run_script_tuplefy({atom, Atom1})
  when is_binary(Atom1) ->
    list_to_atom(unbinstr(Atom1));
run_script_tuplefy(Unk) ->
    Unk.
run_script_tuplefy_args([]) -> [];
run_script_tuplefy_args([A | R]) ->
    [run_script_tuplefy(A) | run_script_tuplefy_args(R)].
    
get_script_pid(ScriptType_S, Settings) ->
    case ScriptType_S of
        "scm" -> RunnerPIDAtom = shape_from_scripts_script_runner_scm;
        "py"  -> RunnerPIDAtom = shape_from_scripts_script_runner_py;
        _     -> RunnerPIDAtom = list_to_atom(
                    "shape_from_scripts_script_runner_" ++ ScriptType_S)
    end,
    case whereis(RunnerPIDAtom) of
        undefined ->
            %% The init folder contain code that runs before our scripts
            InitScriptsDir = filename:absname(code:where_is_file(
                atom_to_list(?MODULE) ++ ?PATH_INIT_POSTFIX)),
            case ScriptType_S of
                "scm" ->
                    {InterpreterNameOnly, Interpreter} = scm_auto_fill_int_path(
                        proplists:get_value(setting_scm_int_path, Settings, "")),
                    Arguments = scm_auto_fill_arguments(InterpreterNameOnly,
                        string:split(proplists:get_value(setting_scm_arguments, Settings, ""), " ", all),
                        InitScriptsDir),
                    InitFile = filename:join(InitScriptsDir, "init.scm");
                
                "py"  ->
                    Interpreter = py_auto_fill_intpath(
                        proplists:get_value(setting_py_int_path, Settings, "")),
                    Arguments   = [],
                    InitFile = filename:join(InitScriptsDir, "init.py");
                
                _Other ->
                    Interpreter = none,
                    Arguments = [],
                    InitFile = ""
            end,
            case Interpreter of
                none ->
                    {error, no_interpreter};
                _ ->
                    case os:find_executable(Interpreter) of
                        false ->
                            {error, interpreter_not_found};
                        InterpreterFullPath ->
                            PIDNew = spawn(fun () ->
                                run_script_runner(InterpreterFullPath, Arguments, InitFile)
                            end),
                            register(RunnerPIDAtom, PIDNew),
                            {ok, PIDNew}
                    end
            end;
        PID ->
            {ok, PID}
    end.

run_script_runner(Interpreter, Arguments, InitFile) ->
    try Port =
            open_port(
                {spawn_executable, Interpreter},
                [exit_status,
                    {args, Arguments ++ [InitFile]},
                    {line, 64000},
                    binary,
                    stderr_to_stdout,
                    use_stdio,
                    hide]),
        port_connect(Port, self()),
        run_script_runner_loop(Port)
    catch
        error:enoent ->
            io:format("~p" ++
                ?__(1,"could not run script interpreter:")++" ~s~n",
                [?MODULE, Interpreter])
    end.
run_script_runner_loop(Port) ->
    receive
        {load_script, Script, StrList, RetPID} ->
            send_to_scr_port(Port, [
                {atom, "run_init"},
                {string, Script},
                prepare_string_pairs(StrList)], RetPID),
            run_script_runner_inner_loop(Port, RetPID, false, []);
        
        {run_script, Params, MoreParams, RetPID} ->
            ?DEBUG_FMT(" *** ~p~n", [prepare_more_parameters(MoreParams)]),
            send_to_scr_port(Port, [
                {atom, "run"},
                {string, ""},
                prepare_parameter_list_for_scm(Params),
                prepare_more_parameters(MoreParams)], RetPID),
            run_script_runner_inner_loop(Port, RetPID, false, []);
        {close, PID} ->
            port_close(Port),
            PID ! exited;
        next ->
            run_script_runner_loop(Port);
        M ->
            io:format("Unexp=~p~n", [M]),
            run_script_runner_loop(Port)
    end.
run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc) ->
    receive
        next ->
            %% Go back to init commands
            run_script_runner_loop(Port);

        {close, _} ->
            port_close(Port),
            RetPID ! exited;
        
        {reply_to_script, Tuple} ->
            ?DEBUG_FMT("sending: ~p~n", [Tuple]),
            try
                OutP = iolist_to_binary([
                    write_scm(prepare_more_parameters_r(Tuple)), <<"\n">>]),
                ?DEBUG_FMT(" ** ~p~n", [OutP]),
                port_command(Port, OutP)
            catch
                _:Error ->
                    io:format("ERROR: ~p~n", [Error]),
                    port_command(Port, <<"(error output_error)">>)
            end,
            run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc);
        {Port, {data, SReply0}} ->
            case SReply0 of
                %% Shorter lines
                {eol, <<>>} when StartedL =:= false ->
                    run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc);
                {eol, <<$(, _/binary>>=SReply} when StartedL =:= false ->
                    ?DEBUG_FMT("Data ~s~n", [SReply]),
                    RetPID ! {reply, self(), scm_parse(SReply)},
                    run_script_runner_inner_loop(Port, RetPID, false, []);
                {eol, SReply} when StartedL =:= false ->
                    io:format("DEBUG: ~s~n", [SReply]),
                    run_script_runner_inner_loop(Port, RetPID, false, []);
                    
                %% Longer lines are split into chunks
                {eol, SReply} when StartedL =:= true ->
                    case LAcc of
                        debug ->
                            io:format("DEBUG: ~s~n", [SReply]),
                            run_script_runner_inner_loop(Port, RetPID, false, []);
                        
                        _ ->
                            SReply_1 = iolist_to_binary(lists:reverse([SReply | LAcc])),
                            ?DEBUG_FMT("Data ~p~n", [SReply_1]),
                            RetPID ! {reply, self(), scm_parse(SReply_1)},
                            run_script_runner_inner_loop(Port, RetPID, false, [])
                    end;
                {noeol, <<$(, _/binary>>=SReply} when StartedL =:= false ->
                    run_script_runner_inner_loop(Port, RetPID, true, [SReply | LAcc]);
                {noeol, _} when StartedL =:= false ->
                    run_script_runner_inner_loop(Port, RetPID, true, debug);
                {noeol, SReply} when StartedL =:= true ->
                    case LAcc of
                        debug ->
                            io:format("DEBUG: ~s~n", [SReply]),
                            run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc);
                        _ ->
                            run_script_runner_inner_loop(Port, RetPID, true, [SReply | LAcc])
                    end;
                
                %% Uncertain data
                _ ->
                    run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc)
            end;
        {Port, {exit_status, ExitStatus}} ->
            case ExitStatus of
                0 -> ok;
                _ -> io:format(?__(1,"NOTE: Script runtime exited with code:")
                        ++ " ~w~n", [ExitStatus])
            end,
            RetPID ! exited;
        Unk ->
            io:format("Unexpected=~p~n", [Unk]),
            run_script_runner_inner_loop(Port, RetPID, StartedL, LAcc)
    after 60000 ->
        io:format(?__(2, "NOTE: Closing script due to timeout") ++ "~n", []),
        port_close(Port),
        RetPID ! exited
    end.
send_to_scr_port(Port, L, RetPID) ->
    try
        OutP = iolist_to_binary([write_scm(L), <<"\n">>]),
        port_command(Port, OutP)
    catch
        _:Error ->
            io:format("ERROR: ~p", [Error]),
            RetPID ! exited,
            exit(error)
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

scm_auto_fill_int_path(Str) ->
    case Str of
        "" ->
            Found = find_first_interpreter(setting_autointrp_scm, [
                "gosh",
                "csi"
            ]),
            {Found, Found};
        _ ->
            Str,
            NameOnly = filename:basename(filename:rootname(Str)),
            {NameOnly, Str}
    end.
scm_auto_fill_arguments(Interpreter, Extra, BaseDir) ->
    case Extra of
        [[]] ->
            Extra_1 = [];
        _ ->
            Extra_1 = Extra
    end,
    case Interpreter of
        "csi" ->
            %% Required arguments for Chicken Scheme interpreter (csi)
            File = filename:absname("init_env_csi.scm", BaseDir),
            File_1 = lists:flatten(string:replace(File, "\\", "\\\\", all)),
            File_2 = lists:flatten(string:replace(File_1, "\"", "\\\"", all)),
            [
                "-q", "-n", "-b",
                "-eval", "(load \"" ++ File_2 ++ "\")"
                | Extra_1
            ];
        "gosh" ->
            %% Required arguments for Gauche shell (gosh)
            [
                "-q", "-b",
                "-A" ++ BaseDir,
                "-l" ++ "init_env_gauche.scm"
                | Extra_1
            ];
        _ ->
            Extra_1
    end.
py_auto_fill_intpath(Str) ->
    case Str of
        "" ->
            find_first_interpreter(setting_autointrp_py, [
                "python",
                "python38",
                "python36",
                "python34",
                "python32"
            ]);
        _ ->
            Str
    end.


%%
%% Select Script 
%%

-spec scripts_menu_select_plugin([filename:filename()], wings_op_mode()) ->
        any().
scripts_menu_select_plugin(ScriptDirs, WingsOpMode) ->
    Parent = wings_dialog:get_dialog_parent(),
    Result = init_dlg_select_script([
        {parent, Parent},
        {script_dirs, ScriptDirs},
        {import_export_mode, WingsOpMode}
    ]),
    Result.

init_dlg_select_script(Config) -> wx:batch(fun() -> dlg_select_script_do_init(Config) end).
dlg_select_script_do_init(Config) ->
    Parent = proplists:get_value(parent, Config),
    ScriptDirs = proplists:get_value(script_dirs, Config),
    WingsOpMode = proplists:get_value(import_export_mode, Config),
    
    ScriptTable = ets:new(temp, [public,ordered_set]),

    F = wxDialog:new(Parent, ?wxID_ANY, ?__(1, "Select Script"),
        [ {size, {630, 380}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER} ]),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    Frameparts = wxPanel:new(F, []),
    wxBoxSizer:add(MainSizer, Frameparts,
        [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}, {proportion, 1}]),
    Frameparts_1_sizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(Frameparts, Frameparts_1_sizer),
    
    Frameparts_0 = wxPanel:new(Frameparts, []),
    wxBoxSizer:add(Frameparts_1_sizer, Frameparts_0,
        [{flag, ?wxEXPAND bor ?wxALL}, {border, 1}, {proportion, 8}]),
    Frameparts_0_sizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(Frameparts_0, Frameparts_0_sizer),
    
    ScriptListPanel = wxPanel:new(Frameparts_0),
    wxBoxSizer:add(Frameparts_0_sizer, ScriptListPanel,
        [{flag, ?wxEXPAND bor ?wxALL}, {border, 0}, {proportion, 1}]),
    ScriptListPanel_sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(ScriptListPanel, ScriptListPanel_sizer),
    
    ScriptList = wxTreeCtrl:new(ScriptListPanel,
        [{id, ?wxID_ANY}, {style, ?wxTR_DEFAULT_STYLE bor ?wxTR_HIDE_ROOT}]),
    wxBoxSizer:add(ScriptListPanel_sizer, ScriptList,
        [{flag, ?wxEXPAND bor ?wxALL}, {border, 3}, {proportion, 10}]),
    
    ScriptListButtonbar = wxPanel:new(ScriptListPanel),
    wxBoxSizer:add(ScriptListPanel_sizer, ScriptListButtonbar,
        [{flag, ?wxALL}, {border, 3}, {proportion, 1}]),
    ScriptListButtonbar_sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(ScriptListButtonbar, ScriptListButtonbar_sizer),
    
    Btn_Refresh_BitBit = wxBitmap:new(
        ?WPC_SHAPE_FROM_SCRIPTS_BTN_REFRESH_BITS,
        ?WPC_SHAPE_FROM_SCRIPTS_BTN_REFRESH_WIDTH,
        ?WPC_SHAPE_FROM_SCRIPTS_BTN_REFRESH_HEIGHT, [{depth, 1}]),
    wxBitmap:setMask(Btn_Refresh_BitBit,
        wxMask:new(Btn_Refresh_BitBit, {255,255,255})),
    Btnrefresh = wxBitmapButton:new(ScriptListButtonbar, ?wxID_ANY, Btn_Refresh_BitBit, [ {size, {36, 30}} ]),
    wxWindow:setToolTip(Btnrefresh, ?__(2, "Refresh")),
    wxBoxSizer:add(ScriptListButtonbar_sizer, Btnrefresh),
    
    
    
    wxWindow:connect(Btnrefresh, command_button_clicked, [{callback,
        fun(#wx{event=#wxCommand{commandInt=_Int}}, _Obj) ->
            dlg_select_script_refresh(ScriptDirs, WingsOpMode, ScriptTable, ScriptList)
        end}]),
    
    InfoBox = wxTextCtrl:new(Frameparts, ?wxID_ANY,
        [{style, ?wxTE_RICH bor ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_BESTWRAP }]),
    wxBoxSizer:add(Frameparts_1_sizer, InfoBox,
        [{flag, ?wxEXPAND bor ?wxALL}, {border, 3}, {proportion, 1}]),
    
    {Btnokay, _BtnCancel} = dlg_button_bar({Frameparts, Frameparts_1_sizer}, F, ?__(3,"OK"), ?__(4,"Cancel")),
    
    wxDialog:setSizer(F, MainSizer),
    wxBoxSizer:recalcSizes(Frameparts_1_sizer),
    
    wxWindow:connect(ScriptList, command_tree_sel_changed, [{callback,
        fun(#wx{event=#wxTree{item=Indx}}, _Obj) ->
            dlg_select_script_show_info(ScriptTable, InfoBox, Btnokay, Indx)
        end}]),
    wxWindow:connect(ScriptList, command_tree_item_activated, [{callback,
        fun(#wx{event=#wxTree{item=_Indx}}, _Obj) ->
            wxDialog:endModal(F, ?wxID_OK)
        end}]),
    
    dlg_select_script_refresh(ScriptDirs, WingsOpMode, ScriptTable, ScriptList),
    
    wxWindow:disable(Btnokay),
    
    catch wings_dialog:set_dialog_parent(F),
    case wxDialog:showModal(F) of
        ?wxID_OK ->
            Indx = wxTreeCtrl:getSelection(ScriptList),
            case ets:lookup(ScriptTable, {script, Indx}) of
                []                  -> Result = keep;
                [{_, {_Name, ScriptFile, WSCRFile, ScriptType, _ScriptDesc}}] ->
                    Result = {load_script, ScriptFile, ScriptType, WSCRFile}
            end;
            
        _ ->
            Result = keep
    end,
    ets:delete(ScriptTable),
    catch wings_dialog:reset_dialog_parent(F),
    wxDialog:destroy(F),
    Result.


script_dirs(A) -> 
    lists:filter(fun ("") -> false; (_) -> true end,
        [ string:trim(A1) || A1 <- string:tokens(A, "\r\n") ]).

not_the_full_path(A) ->
    B = lists:reverse(filename:split(A)),
    case B of
        ["" | R] -> C = R;
        R        -> C = R
    end,
    case C of
        [Within1] ->
            lists:flatten(io_lib:format("[~s]", [Within1]));
        [Within1, _] ->
            lists:flatten(io_lib:format("[~s]", [Within1]));
        [Within1, Within2, _] ->
            lists:flatten(io_lib:format("[~s] ~s", [Within2, Within1]));
        [A1, A2 | R_2] ->
            Within1 = A1,
            Within2 = A2,
            case lists:reverse(R_2) of
                [_, R_3 | _] -> Within3 = R_3;
                [R_3]        -> Within3 = R_3
            end,
            lists:flatten(io_lib:format("[~s] ~s/~s", [Within3, Within2, Within1]))
    end.
    
-spec dlg_select_script_refresh([filename:filename()], wings_op_mode(),
        any(), wx:wxobject()) -> ok.
dlg_select_script_refresh(ScriptDirs, WingsOpMode, ScriptTable, ScriptList) ->
    wxTreeCtrl:deleteAllItems(ScriptList),
    ERoot = wxTreeCtrl:addRoot(ScriptList, "*"),
    lists:foreach(fun({Directory, SList}) ->
        Elm = script_list_adddir(ERoot, ScriptList, not_the_full_path(Directory)),
        lists:foreach(fun({WSCRFile, {ScriptFile, ScriptType, ScriptName, ScriptDesc}}) ->
            script_list_add(
                ScriptTable, ScriptList, Elm, ScriptName, ScriptFile,
                WSCRFile, ScriptType, ScriptDesc)
        end, SList)
    end, read_script_dirs(ScriptDirs, WingsOpMode)),
    ok.

    
script_list_adddir(ERoot, ScriptList, Name) ->
    Elm = wxTreeCtrl:appendItem(ScriptList, ERoot, Name),
    wxTreeCtrl:ensureVisible(ScriptList, Elm),
    % Elm = wxTreeCtrl:addRoot(ScriptList, Name),
    Elm.
    
script_list_add(ScriptTable, ScriptList, ParentNode, Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc) ->
    Elm = wxTreeCtrl:appendItem(ScriptList, ParentNode, Name),
    ets:insert(ScriptTable, {{script, Elm}, {Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc}}).

dlg_select_script_insert_txt(E, Txt) ->
    Sty_nrm = wxTextAttr:new({0,0,0}),
    Eb = wxTextCtrl:getLastPosition(E),
    wxTextCtrl:appendText(E, Txt),
    wxTextCtrl:setStyle(E, Eb, wxTextCtrl:getLastPosition(E), Sty_nrm),
    wxTextCtrl:appendText(E, "\n").

dlg_select_script_display_about_details(InfoBox, Name, ScriptFile, _WSCRFile, ScriptType, ScriptDesc0) ->
    wxTextCtrl:clear(InfoBox),
    case ScriptDesc0 of
        "" ->
            ScriptDesc = Name;
        _ ->
            ScriptDesc = ScriptDesc0
    end,
    dlg_select_script_insert_txt(InfoBox, io_lib:format("~s~n~n~s [~s]~n", [ScriptDesc, ScriptFile, ScriptType])),
    wxTextCtrl:setSelection(InfoBox, 0, 0).
    
dlg_select_script_show_info(ScriptTable, InfoBox, Btnokay, Indx) ->
    case ets:lookup(ScriptTable, {script, Indx}) of
        [] ->
            wxWindow:disable(Btnokay);
        [{_, {Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc}}] ->
            wxWindow:enable(Btnokay),
            dlg_select_script_display_about_details(InfoBox, Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc)
    end.

-spec read_script_dirs([filename:filename()], wings_op_mode()) ->
        [{filename:filename(), [filename:filename()]}].
read_script_dirs(ScriptDirs, WingsOpMode) ->
    read_script_dirs(ScriptDirs, WingsOpMode, []).
read_script_dirs([], _, OList) -> lists:reverse(OList);
read_script_dirs([Path | ScriptDirs], WingsOpMode, OList) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            {ok, LResults} = iterate_filenames(Filenames, Path, WingsOpMode);
        _ ->
            LResults = []
    end,
    read_script_dirs(ScriptDirs, WingsOpMode, [{Path, LResults} | OList]).

-spec iterate_filenames([filename:filename()], filename:filename(),
        wings_op_mode()) -> {ok, [filename:filename()]}.
iterate_filenames([FlN | Filenames], Path, WingsOpMode) ->
    {ok, iterate_filenames([FlN | Filenames], Path, WingsOpMode, [])}.
iterate_filenames([], _, _, OList) -> lists:reverse(OList);
iterate_filenames([FlN | Filenames], Path, WingsOpMode, OList) ->
    FullFilename = filename:join(Path, FlN),
    case file:read_file_info(FullFilename) of
        {ok, FileInfo} ->
            OList_1 = iterate_filenames_1(element(3, FileInfo), Filenames,
                Path, WingsOpMode, OList, FlN, FullFilename);
        _ ->
            OList_1 = OList
    end,
    iterate_filenames(Filenames, Path, WingsOpMode, OList_1).
iterate_filenames_1(directory, _Filenames, _Path, WingsOpMode, OList, _FlN, FullFilename) ->
    SubDir = FullFilename,
    case file:list_dir(SubDir) of
        {ok, SubDir_Filenames} ->
            OList_1 = iterate_filenames(SubDir_Filenames, SubDir, WingsOpMode, OList);
        _ ->
            OList_1 = OList
    end,
    OList_1;
iterate_filenames_1(_, _Filenames, _Path, WingsOpMode, OList, FlN, FullFilename) ->
    Extension = string:to_lower(filename:extension(FlN)),
    case Extension of
        ".wscr" ->
            case read_script_info_file(FullFilename, WingsOpMode) of
                {ok, W} -> OList_1 = [{FullFilename, W} | OList];
                _       -> OList_1 = OList
            end;
        _ ->
            OList_1 = OList
    end,
    OList_1.



-spec read_script_info_file(filename:filename(), wings_op_mode()) ->
        error | {ok, {filename:filename(), string(), string(), string()}}.
read_script_info_file(FlN, WingsOpMode) ->
    NameOnly = string:substr(FlN, 1, string:rchr(FlN, $.)-1),
    case file:read_file(FlN) of
        {ok, BCont} ->
            {ok, StrList} = load_lang_file(FlN, current_lang_code()),
            {ok, Cont_0} = read_wscr_content(BCont, StrList),
            Cont = wscr_to_proplist(Cont_0),
            case orddict:find("mode", Cont) of
                error          -> ScriptMode = [false];
                {ok, "import"} -> ScriptMode = [import];
                {ok, "export"} -> ScriptMode = [export];
                {ok, "simple_face_command"} -> ScriptMode = [face,body];
                {ok, "simple_body_command"} -> ScriptMode = [body];
                {ok, "simple_light_command"} -> ScriptMode = [light];
                
                {ok, "simple_multi_command"} ->
                    ScriptMode = [vertex, edge, face, body, light];
                _              -> ScriptMode = [false]
            end,
            
            case lists:member(WingsOpMode, ScriptMode) of
                false -> unused;
                true ->
                    case orddict:find("type", Cont) of
                        error      -> WSCRType_0 = detect;
                        {ok, Val1} -> WSCRType_0 = Val1
                    end,
                    case orddict:find("name", Cont) of
                        error      -> WSCRName = "";
                        {ok, Val2} -> WSCRName = Val2
                    end,
                    case orddict:find("description", Cont) of
                        error      -> WSCRDesc = "";
                        {ok, Val3} -> WSCRDesc = Val3
                    end,
                    case WSCRType_0 of
                        detect ->
                            case lists:concat(lists:map(fun(T) ->
                                ScriptFound_0 = NameOnly ++ "." ++ T,
                                case file:read_file_info(ScriptFound_0) of
                                    {ok, _} -> [T];
                                    _       -> []
                                end
                            end, ["py", "scm"])) of
                                [FoundType | _] ->
                                    WSCRType = FoundType,
                                    ScriptFound = NameOnly ++ "." ++ FoundType;
                                []          ->
                                    WSCRType = none,
                                    ScriptFound = none
                            end;
                        _ ->
                            WSCRType = WSCRType_0,
                            ScriptFound_0 = NameOnly ++ "." ++ WSCRType_0,
                            case file:read_file_info(ScriptFound_0) of
                                {ok, _} -> ScriptFound = ScriptFound_0;
                                _       -> ScriptFound = none
                            end
                    end,
                    {ok, {ScriptFound, WSCRType, WSCRName, WSCRDesc}}
            end;
        _ ->
            error
    end.
    
wscr_to_proplist(L) -> wscr_to_proplist(L, []).
wscr_to_proplist([], O) -> orddict:from_list(lists:reverse(O));
wscr_to_proplist([[A1, A2] | R], O) when is_list(A1), is_list(A2) ->
    wscr_to_proplist(R, [{A1, A2}|O]);
wscr_to_proplist([_ | R], O) ->
    wscr_to_proplist(R, O).


read_wscr_until_eol(<<C, R/binary>>) when C =:= $\n ->
    R;
read_wscr_until_eol(<<_, R/binary>>) ->
    read_wscr_until_eol(R).

read_wscr_content(R, StrList) ->
    {Cont, []} = read_wscr_content(R, StrList, [], [], []),
    {ok, Cont}.
read_wscr_content(<<>>, _StrList, [], [], Lines) ->
    { lists:reverse(Lines), [] };
read_wscr_content(<<>>, StrList, [], CLine, Lines) ->
    read_wscr_content(<<>>, StrList, [], [], [lists:reverse(CLine) | Lines]);
read_wscr_content(<<>>, StrList, CWord, CLine, Lines) ->
    read_wscr_content(<<>>, StrList, [], [lists:reverse(CWord) | CLine], Lines);

read_wscr_content(<<C, R/binary>>, StrList, CWord, CLine, Lines)
  when C =:= $# ->
        R_1 = read_wscr_until_eol(R),
        read_wscr_content(R_1, StrList, CWord, CLine, Lines);
%% String, not localized
read_wscr_content(<<C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= 34 ->
        case CWord of
            [] ->
                {Str, R_1} = read_wscr_string(R),
                read_wscr_content(R_1, StrList, CWord, [Str | CLine], Lines);
            _  ->
                read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
        end;
%% String localized, the parser will use the string in StrList indicated by
%% the first argument integer, or the second argument is used as the string
%% if not found
read_wscr_content(<<$?, $_, $_, C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= 40 -> %% Opening parenthesis
        case CWord of
            [] ->
                {LocStrID, Str, R_1} = read_wscr_string_locale(R),
                case orddict:find(LocStrID, StrList) of
                    {ok, StrTranslated} ->
                        read_wscr_content(R_1, StrList, CWord, [StrTranslated | CLine], Lines);
                    _ ->
                        read_wscr_content(R_1, StrList, CWord, [Str | CLine], Lines)
                end;
            _  ->
                read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
        end;
read_wscr_content(<<C, R/binary>>, StrList, CWord, CLine, Lines)
  when C =:= 32; C =:= 9 ->
        case CWord of
            [] ->
                read_wscr_content(R, StrList, [], CLine, Lines);
            _  ->
                read_wscr_content(R, StrList, [], [lists:reverse(CWord) | CLine], Lines)
        end;
read_wscr_content(<<C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= ${ ->
        case CWord of
            [] ->
                {SubList, R_2} = read_wscr_content(R, StrList, [], [], []),
                read_wscr_content(R_2, StrList, [], [SubList | CLine], Lines);
            _  ->
                read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
        end;
read_wscr_content(<<C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= $} ->
        case CWord of
            [] -> 
                case CLine of
                    [] -> {lists:reverse(Lines), R};
                    _  ->
                        read_wscr_content(RInp, StrList, [],
                            [], [lists:reverse(CLine) | Lines])
                end;
            _  ->
                read_wscr_content(RInp, StrList, [],
                    [lists:reverse(CWord) | CLine], Lines)
        end;
read_wscr_content(<<C, R/binary>>, StrList, [], [], Lines)
  when C =:= 10; C =:= 13 ->
        read_wscr_content(R, StrList, [],
            [], Lines);
read_wscr_content(<<C, R/binary>>, StrList, [], CLine, Lines)
  when C =:= 10; C =:= 13 ->
        read_wscr_content(R, StrList, [],
            [], [lists:reverse(CLine) | Lines]);
read_wscr_content(<<C, _/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= 10; C =:= 13 ->
        read_wscr_content(RInp, StrList, [],
            [lists:reverse(CWord) | CLine], Lines);
read_wscr_content(<<C, R/binary>>, StrList, CWord, CLine, Lines) ->
        read_wscr_content(R, StrList, [C | CWord], CLine, Lines).

read_wscr_string(R) -> read_wscr_string(R, []).
read_wscr_string(<<$\\, C, R/binary>>, Str) ->
    read_wscr_string(R, [C | Str]);
read_wscr_string(<<C, R/binary>>, Str) when C =:= 34 ->
    {unicode:characters_to_list(iolist_to_binary(lists:reverse(Str)), utf8), R};
read_wscr_string(<<C, R/binary>>, Str) ->
    read_wscr_string(R, [C | Str]).

read_wscr_string_locale(R) -> read_wscr_string_locale(R, 1, [], []).
read_wscr_string_locale(<<C, R/binary>>, 1, Digits, Str)
  when C >= $0, C =< $9 ->
    read_wscr_string_locale(R, 1, [C | Digits], Str);
read_wscr_string_locale(<<C, R/binary>>, 2, Digits, _)
  when C =:= 34 ->
    {Str, R_1} = read_wscr_string(R),
    read_wscr_string_locale(R_1, 2, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, N, Digits, Str)
  when C =:= $, ->
    read_wscr_string_locale(R, N+1, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, N, Digits, Str)
  when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    read_wscr_string_locale(R, N, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, 2, Digits, Str)
  when C >= 41 -> %% Closing parenthesis
    case string:to_integer(lists:reverse(Digits)) of
        {error, _} -> LocStrID = 0;
        {LocStrID, []} -> LocStrID
    end,
    {LocStrID, Str, R};
read_wscr_string_locale(_, _, _, _) ->
    {error, not_formatted_right}.
    
%% Get the lang code.
current_lang_code() ->
    atom_to_list(get(wings_lang)).

%% Load a language file for the script wscr, they are in the same
%% format as a wings plugin's lang file.
load_lang_file(WSCRFile, LangCode) ->
    load_lang_file(WSCRFile, LangCode, wscr).
load_lang_file(WSCRFile, LangCode, Which) ->
    LangFile = filename:rootname(WSCRFile) ++ "_" ++ LangCode ++ ".lang",
    case file:consult(LangFile) of
        {ok, [{_, LangSectionList}]} ->
            case orddict:find(Which, LangSectionList) of
                {ok, StrList} -> {ok, StrList};
                error -> {ok, []}
            end;
        {error, _} ->
            {ok, []}
    end.


read_wscr_includes(Cont_0, WSCRAndStrLst) ->
    case Cont_0 of
        {ok, Cont} ->
            {ok, Cont_1} = read_wscr_includes(Cont, WSCRAndStrLst, []),
            {ok, Cont_1}
    end.
read_wscr_includes([], _, O) ->
    {ok, lists:reverse(O)};
read_wscr_includes([["include", InclFile_0] | Cont], {WSCRFile, StrList}=WSCRAndStrLst, O) when is_list(WSCRFile) ->
    InclFile = read_wscr_includes_shorthands(InclFile_0, filename:basename(WSCRFile)),
    InclFile_1 = filename:absname(InclFile, filename:dirname(WSCRFile)),
    case file:read_file(InclFile_1) of
        {ok, BCont} ->
            {ok, Cont_0} = read_wscr_includes(read_wscr_content(BCont, StrList), {InclFile_1, StrList}),
            read_wscr_includes(Cont, WSCRAndStrLst, lists:reverse(Cont_0) ++ O);
        _ ->
            {error, file_not_included}
    end;
read_wscr_includes([C | Cont], WSCRAndStrLst, OCont) ->
    read_wscr_includes(Cont, WSCRAndStrLst, [C | OCont]).

%% Certain short hands for an included wscr file
read_wscr_includes_shorthands(InclFile_0, WSCRFile) ->
    WSCRFile_R = filename:rootname(WSCRFile),
    lists:flatten(string:replace(InclFile_0, "(%)", WSCRFile_R)).


%%
%% Script Preference Dialog 
%%

init_dlg_script_preference(Config, Defaults) -> wx:batch(fun() -> dlg_script_preference_do_init(Config, Defaults) end).
dlg_script_preference_do_init(Config, Defaults) ->
    Parent = proplists:get_value(parent, Config),

    F = wxDialog:new(Parent, ?wxID_ANY, ?__(1,"Script Preference"), [ {size, {600, 500}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER} ]),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    Frameparts = wxPanel:new(F, []),
    wxBoxSizer:add(MainSizer, Frameparts, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}, {proportion, 1}]),
    Frameparts_sizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(Frameparts, Frameparts_sizer),
    
    ChkEnableScripts = dlg_check_box({Frameparts, Frameparts_sizer}, ?__(2,"Enable Scripts"),
        dlg_script_pref_value(setting_enable, Config, Defaults)),
    dlg_spacer({Frameparts, Frameparts_sizer}, 8),
    
    SetTabs = wxNotebook:new(Frameparts, ?wxID_ANY),
    wxBoxSizer:add(Frameparts_sizer, SetTabs, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}, {proportion, 1}]),
    
    Elems = lists:append([
        tab_panel(SetTabs, TabTitle, TF, Config, Defaults)
    || {TabTitle, TF} <- [
        {?__(3,"Shapes"), fun dlg_script_preference_do_init_page1/3},
        {?__(4,"Commands"), fun dlg_script_preference_do_init_page2com/3},
        {?__(5,"Import / Export"), fun dlg_script_preference_do_init_page2/3},
        {?__(6,"Interpreters"), fun dlg_script_preference_do_init_page3/3},
        {?__(8,"Debug"), fun dlg_script_preference_do_init_page5/3}
    ]]) ++ [{setting_enable, ChkEnableScripts}],
    
    {_Btnokay, _BtnCancel} = dlg_button_bar({Frameparts, Frameparts_sizer}, F, ?__(9,"OK"), ?__(10,"Cancel")),

    wxDialog:setSizer(F, MainSizer),
    wxBoxSizer:recalcSizes(Frameparts_sizer),
    
    
    wings_dialog:set_dialog_parent(F),
    case wxDialog:showModal(F) of
        ?wxID_OK ->
            Result_Settings = 
            [ {SettAtom, WxCntl:getValue(Handle)}
                || {SettAtom, {WxCntl, Handle}} <- Elems ];
        ?wxID_CANCEL ->
            Result_Settings = 
            [ {SettAtom,
                case proplists:get_value(SettAtom, Config) of
                    undefined -> proplists:get_value(SettAtom, Defaults);
                    PrevVal -> PrevVal
                end}
                || {SettAtom, _} <- Elems ]
    end,
    
    wings_dialog:reset_dialog_parent(F),
    wxDialog:destroy(F),
    Result_Settings.


tab_panel(SetTabs, Label, Page, Config, Defaults) ->
    FrameTab = wxPanel:new(SetTabs, []),
    FrameTab_sizer = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(FrameTab, FrameTab_sizer),
    Ret = Page({FrameTab, FrameTab_sizer}, Config, Defaults),
    wxNotebook:addPage(SetTabs, FrameTab, Label),
    Ret.
dlg_label_line({Frameparts, Frameparts_sizer}, LabelStr) ->
    LabelCtl = wxStaticText:new(Frameparts, ?wxID_ANY, LabelStr),
    wxBoxSizer:add(Frameparts_sizer, LabelCtl, [ {flag, ?wxALL}, {border, 5}]).
dlg_text_line({Frameparts, Frameparts_sizer}, TextVal) ->
    Ctl = wxTextCtrl:new(Frameparts, ?wxID_ANY),
    wxBoxSizer:add(Frameparts_sizer, Ctl, [ {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxTextCtrl:setValue(Ctl, TextVal),
    {wxTextCtrl, Ctl}.
dlg_check_box({Frameparts, Frameparts_sizer}, LabelStr, Val) ->
    Ctl = wxCheckBox:new(Frameparts, ?wxID_ANY, LabelStr),
    wxBoxSizer:add(Frameparts_sizer, Ctl, [ {flag, ?wxALL}, {border, 5}]),
    wxCheckBox:setValue(Ctl, Val),
    {wxCheckBox, Ctl}.
dlg_spacer({_Frameparts, Frameparts_sizer}, N) -> 
    wxBoxSizer:addSpacer(Frameparts_sizer, N).
dlg_button_bar({Frameparts, Frameparts_sizer}, F, OkayLabelStr, CancelLabelStr) ->
    Buttonbar = wxPanel:new(Frameparts),
    Buttonbar_sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(Buttonbar, Buttonbar_sizer),
    Btnokay = wxButton:new(Buttonbar, ?wxID_ANY, [ {label, OkayLabelStr} ]),
    wxWindow:connect(Btnokay, command_button_clicked, [{callback, fun(#wx{event=#wxCommand{commandInt=_Int}}, _Obj) ->
         wxDialog:endModal(F, ?wxID_OK)
    end}]),
    wxBoxSizer:add(Buttonbar_sizer, Btnokay),
    BtnCancel = wxButton:new(Buttonbar, ?wxID_ANY, [ {label, CancelLabelStr} ]),
    wxWindow:connect(BtnCancel, command_button_clicked, [{callback, fun(#wx{event=#wxCommand{commandInt=_Int}}, _Obj) ->
         wxDialog:endModal(F, ?wxID_CANCEL)
    end}]),
    wxBoxSizer:add(Buttonbar_sizer, BtnCancel),
    wxBoxSizer:add(Frameparts_sizer, Buttonbar, [ {flag, ?wxALL}, {border, 3}]),
    {Btnokay, BtnCancel}.
dlg_multitext({Frameparts, Frameparts_sizer}, TextVal) ->
    Ctl = wxTextCtrl:new(Frameparts, ?wxID_ANY,
        [{style, ?wxTE_MULTILINE bor ?wxTE_BESTWRAP }]),
    wxBoxSizer:add(Frameparts_sizer, Ctl, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}, {proportion, 3}]),
    wxTextCtrl:setValue(Ctl, TextVal),
    {wxTextCtrl, Ctl}.
dlg_multiline({Frameparts, Frameparts_sizer}, LabelStr) ->
    Aboutline2 = wxStaticText:new(Frameparts, ?wxID_ANY, LabelStr),
    wxBoxSizer:add(Frameparts_sizer, Aboutline2, [ {flag, ?wxALL bor ?wxEXPAND}, {border, 5}, {proportion, 1}]).


dlg_script_pref_value(SettAtom, Config, Defaults) ->
    case proplists:get_value(SettAtom, Config) of
        undefined ->
            proplists:get_value(SettAtom, Defaults);
        Val -> Val
    end.
    


dlg_script_preference_do_init_page1(FF, Config, Defaults) ->
    dlg_label_line(FF, ?__(1,"Paths for Shape Scripts (one per line):")),
    PathsBox = dlg_multitext(FF,
        dlg_script_pref_value(setting_paths_shapes, Config, Defaults)),
    
    dlg_multiline(FF, ?__(2,"Each script must have with it a file with the "
        "extension .wscr that contains its name, description and parameters.")),
    dlg_spacer(FF, 8),
    
    [{setting_paths_shapes, PathsBox}].
    
    
dlg_script_preference_do_init_page2com(FF, Config, Defaults) ->
    dlg_label_line(FF, ?__(1,"Paths for Command Scripts (one per line):")),
    PathsBox = dlg_multitext(FF,
        dlg_script_pref_value(setting_paths_commands, Config, Defaults)),
    
    dlg_multiline(FF, ?__(2,"Each script must have with it a file with the "
        "extension .wscr that contains its name, description and parameters.")),
    dlg_spacer(FF, 8),
    ChkEnableScriptsCommands = dlg_check_box(FF, ?__(3,"Enable Scripts for Commands Menu"),
        dlg_script_pref_value(setting_enable_commands, Config, Defaults)),
    dlg_spacer(FF, 8),
    [{setting_paths_commands, PathsBox},
     {setting_enable_commands, ChkEnableScriptsCommands}].
    


dlg_script_preference_do_init_page2(FF, Config, Defaults) ->
    dlg_label_line(FF, ?__(1,"Paths for Import/Export Scripts (one per line):")),
    PathsBox = dlg_multitext(FF,
        dlg_script_pref_value(setting_paths_import_export, Config, Defaults)),
    
    dlg_multiline(FF, ?__(2,"Each script must have with it a file with the "
        "extension .wscr that contains its name, description and parameters.")),
    dlg_spacer(FF, 8),
    ChkEnableScriptsImport = dlg_check_box(FF, ?__(3,"Enable Scripts for Import Menu"),
        dlg_script_pref_value(setting_enable_import, Config, Defaults)),
    ChkEnableScriptsExport = dlg_check_box(FF, ?__(4,"Enable Scripts for Export Menu"),
        dlg_script_pref_value(setting_enable_export, Config, Defaults)),
    dlg_spacer(FF, 8),
    [{setting_paths_import_export, PathsBox},
     {setting_enable_import, ChkEnableScriptsImport},
     {setting_enable_export, ChkEnableScriptsExport}].
    

dlg_script_preference_do_init_page3(FF, Config, Defaults) ->
    dlg_label_line(FF, ?__(1,"Python Interpreter Path:")),
    PathPyInt = dlg_text_line(FF,
        dlg_script_pref_value(setting_py_int_path, Config, Defaults)),
    dlg_spacer(FF, 3),
    
    dlg_label_line(FF, ?__(2,"Scheme Interpreter Path:")),
    PathScmInt = dlg_text_line(FF,
        dlg_script_pref_value(setting_scm_int_path, Config, Defaults)),
    dlg_label_line(FF, ?__(3,"Scheme Extra Interpreter Arguments:")),
    PathScmArguments = dlg_text_line(FF,
        dlg_script_pref_value(setting_scm_arguments, Config, Defaults)),
    dlg_spacer(FF, 3),
    [{setting_py_int_path, PathPyInt},
     {setting_scm_int_path, PathScmInt},
     {setting_scm_arguments, PathScmArguments}].


dlg_script_preference_do_init_page5(FF, Config, Defaults) ->
    
    ChkEnableShowTuple = dlg_check_box(FF, ?__(4,"Enable Debug Return Data"),
        dlg_script_pref_value(setting_show_tuple, Config, Defaults)),
    dlg_spacer(FF, 3),
    
    [{setting_show_tuple, ChkEnableShowTuple}].



%%
%% Prepare mainly list data structures for write_scm
%%

prepare_parameter_list_for_scm(List) ->
    lists:map(fun ({K, A1}) when is_atom(K) ->
                      [{atom, list_to_binary(atom_to_list(K))}, prepare_parameter_list_for_scm([A1])];
                  (A1)  when is_binary(A1) -> {string, A1};
                  (Str) when is_list(Str) -> {string, Str};
                  (A1) -> A1
    end, List).
prepare_more_parameters(List) ->
    lists:map(fun
        ([{string, A1}, A2]) ->
            [ {string, A1}, prepare_more_parameters_r(A2) ];
            
        ([A1, A2]) ->
            [ {string, A1}, prepare_more_parameters_r(A2) ]
    end, List).
prepare_more_parameters_r({string, A1}) when is_binary(A1) -> {string, A1};
prepare_more_parameters_r({atom, A1}) when is_binary(A1) -> {atom, A1};
prepare_more_parameters_r(A1) when is_binary(A1) -> {string, A1};
prepare_more_parameters_r(Tuple)
  when is_tuple(Tuple), is_atom(element(1, Tuple)),
       element(1, Tuple) =/= atom,
       element(1, Tuple) =/= string ->
    [FrontAtom | List2] = tuple_to_list(Tuple),
    [   {atom, binstr(atom_to_list(FrontAtom))} |
        lists:map(fun(A1) -> prepare_more_parameters_r(A1) end, List2)];
prepare_more_parameters_r(List) when is_list(List) ->
    lists:map(fun(A1) -> prepare_more_parameters_r(A1) end, List);
prepare_more_parameters_r(A1) when is_atom(A1) -> {atom, binstr(atom_to_list(A1))};
prepare_more_parameters_r(A1) -> A1.


prepare_string_pairs(StrList) ->
    [prepare_string_pairs_1(P) || P <- StrList].
prepare_string_pairs_1({Int, Str})
  when is_list(Str) ->
    {Int, {string, binstr(Str)}};
prepare_string_pairs_1(A) -> A.
    

%%
%% Symbolic expression writer
%%

write_scm(A) -> ?DEBUG_FMT("write_scm -> ~p~n~n", [A]),
    write_scm(A, [<<"(">>], false).
write_scm([], BList, _) ->
    lists:reverse([<<")">>|BList]);
write_scm(AList, BList, true) ->
    write_scm(AList, [<<" ">> | BList], false);
write_scm([{atom, A} | AList], BList, false) ->
    write_scm(AList, [A | BList], true);
write_scm([{string, {string, A}} | AList], BList, false) -> 
    write_scm([{string, A} | AList], BList, false);
write_scm([{string, A} | AList], BList, false) ->
    A_1 = string:replace(A, "\\", "\\\\", all),
    A_2 = binstr(string:replace(A_1, "\"", "\\\"", all)),
    write_scm(AList, [<<"\"", A_2/binary, "\"">> | BList], true);
write_scm([{X, Y} | AList], BList, false)
  when is_number(X), is_number(Y) -> %% coordinate
    TS = iolist_to_binary(io_lib:format("#(~w ~w)", [X, Y])),
    write_scm(AList, [TS | BList], true);
write_scm([{X, Y} | AList], BList, false)
  when is_list(X), is_list(Y); 
       is_tuple(X), is_tuple(Y);
       is_number(X), is_atom(Y);
       is_number(X), is_tuple(Y);   %% orddict element
       is_number(X), is_list(Y) ->  %% orddict element
    [_ | X_1] = lists:reverse(write_scm([X], [], false)),
    [_ | Y_1] = lists:reverse(write_scm([Y], [], false)),
    Args = [<<")">> | Y_1] ++ [<<" ">> | X_1],
    write_scm(AList, [lists:reverse(Args), <<"#(">> | BList], true);
write_scm([{X, Y, Z} | AList], BList, false)
  when is_number(X), is_number(Y), is_number(Z) ->
    TS = iolist_to_binary(io_lib:format("#(~w ~w ~w)", [X, Y, Z])),
    write_scm(AList, [TS | BList], true);
write_scm([{X, Y, Z} | AList], BList, false)
  when is_list(X), is_list(Y), is_list(Z);
       is_tuple(X), is_tuple(Y), is_tuple(Z) ->
    [_ | X_1] = lists:reverse(write_scm([X], [], false)),
    [_ | Y_1] = lists:reverse(write_scm([Y], [], false)),
    [_ | Z_1] = lists:reverse(write_scm([Z], [], false)),
    Args = [<<")">> | Z_1] ++ [<<" ">> | Y_1] ++ [<<" ">> | X_1],
    write_scm(AList, [lists:reverse(Args), <<"#(">> | BList], true);
write_scm([BigTuple | AList], BList, false) when is_tuple(BigTuple) ->
    A_1 = write_scm_tuple(tuple_to_list(BigTuple)),
    write_scm(AList, [A_1 | BList], true);
write_scm([A | AList], BList, false) when is_list(A) ->
    A_1 = write_scm(A),
    write_scm(AList, [A_1 | BList], true);
write_scm([true | AList], BList, false) ->
    write_scm(AList, [<<"#t">> | BList], true);
write_scm([false | AList], BList, false) ->
    write_scm(AList, [<<"#f">> | BList], true);
write_scm([A | AList], BList, false) when is_number(A) ->
    write_scm(AList, [io_lib:format("~w", [A]) | BList], true).

write_scm_tuple(A) ->
    write_scm(A, [<<"#(">>], false).


%%
%% Symbolic expression reader for script input/output
%%

scm_parse(A) -> scm_parse(A, [], []).
scm_parse(<<>>, List, []) -> {lists:reverse(List), <<>>};
scm_parse(<<>>, List, Tok) when length(Tok) > 0 ->
    scm_parse(<<>>, [bare_word(Tok) | List], []);
scm_parse(<<34, R/binary>>, List, Tok) ->
    {Str, R_0} = scm_parse_str(R),
    scm_parse(R_0, [{string, Str} | List], Tok);
scm_parse(<<$#, C, R/binary>>=RInp, List, Tok) when C =:= $( ->
    case Tok of
        [] ->
            {SubList, R_0} = scm_parse(R, [], []),
            scm_parse(R_0, [list_to_tuple(SubList) | List], []);
        _ when length(Tok) > 0 ->
            scm_parse(RInp, [bare_word(Tok) | List], [])
    end;
scm_parse(<<$', C, R/binary>>=RInp, List, Tok) when C =:= $( ->
    case Tok of
        [] ->
            {SubList, R_0} = scm_parse(R, [], []),
            scm_parse(R_0, [SubList | List], []);
        _ when length(Tok) > 0 ->
            scm_parse(RInp, [bare_word(Tok) | List], [])
    end;
scm_parse(<<C, R/binary>>=RInp, List, Tok) when C =:= $( ->
    case Tok of
        [] ->
            {SubList, R_0} = scm_parse(R, [], []),
            scm_parse(R_0, [SubList | List], []);
        _ when length(Tok) > 0 ->
            scm_parse(RInp, [bare_word(Tok) | List], [])
    end;
scm_parse(<<C, R/binary>>=RInp, List, Tok) when C =:= $) ->
    case Tok of
        [] -> {lists:reverse(List), R};
        _ when length(Tok) > 0 ->
            scm_parse(RInp, [bare_word(Tok) | List], [])
    end;
scm_parse(<<C, R/binary>>=RInp, List, Tok)
  when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    case Tok of
        [] -> scm_parse(R, List, []);
        _  -> scm_parse(RInp, [bare_word(Tok) | List], [])
    end;
scm_parse(<<C, _/binary>>=Num_S, List, [])
  when C >= $0, C =< $9; C =:= $.; C =:= $- ->
        {Num, RInp} = scm_parse_num(Num_S),
        scm_parse(RInp, [Num | List], []);
scm_parse(<<C, R/binary>>, List, Tok) ->
    scm_parse(R, List, [C | Tok]).
    

scm_parse_num(A) -> scm_parse_num(A, []).
scm_parse_num(<<C, _/binary>>=R, Num_S0)
  when C =:= 32; C =:= 9; C =:= $) ->
        Num_S = lists:reverse(Num_S0),
        case string:to_float(Num_S) of
            {error, _} ->
                case string:to_integer(Num_S) of
                    {error, _} -> {<<"?">>, R};
                    {Num_0, []} -> {Num_0, R}
                end;
            {Num_0, []} -> {Num_0, R}
        end;
scm_parse_num(<<C, R/binary>>, Num_S) ->
    scm_parse_num(R, [C | Num_S]).

scm_parse_str(A) -> scm_parse_str(A, []).
scm_parse_str(<<$\\, C, R/binary>>, Str) ->
    scm_parse_str(R, [C | Str]);
scm_parse_str(<<34, R/binary>>, Str) ->
    {iolist_to_binary(lists:reverse(Str)), R};
scm_parse_str(<<C, R/binary>>, Str) ->
    scm_parse_str(R, [C | Str]).

bare_word(A_0) ->
    A_1 = lists:reverse(A_0),
    case A_1 of
        "#t" -> true;
        "#f" -> false;
        _    -> {atom, iolist_to_binary(A_1)}
    end.

get_wscr_param_number(Num_S) ->
    case string:to_float(Num_S) of
        {error, _} ->
            case string:to_integer(Num_S) of
                {error, _} -> Num_S;
                {Num_0, []} -> Num_0
            end;
        {Num_0, []} -> Num_0
    end.
    
read_wscr_file_content(WSCRFile) ->
    case file:read_file(WSCRFile) of
        {ok, BCont} ->
            {ok, StrList} = load_lang_file(WSCRFile, current_lang_code()),
            {ok, Cont_0} =
                read_wscr_includes(read_wscr_content(BCont, StrList),
                    {WSCRFile, StrList}),
            {ok, Cont_0};
        _ ->
            error
    end.
    
get_wscr_params_default_param_title() ->
    ?__(1,"Set Parameters").
    
get_template_opts(A, B) ->
    get_template_opts(A, B, []).
get_template_opts(A, ["include_colors"|Opts], OL) ->
    get_template_opts(A, Opts, [include_colors|OL]);
get_template_opts(A, [_|Opts], OL) ->
    get_template_opts(A, Opts, OL);
get_template_opts(_A, [], OL) ->
    lists:reverse(OL).
    
get_wscr_templates(Cont) ->
    case orddict:find("params_templates", Cont) of
        error      -> ParamsTplsLists_0 = [];
        {ok, Val3} -> ParamsTplsLists_0 = Val3
    end,
    lists:map(fun
        (["template", "import", Opts | _]) ->
            {import, get_template_opts(import, string:tokens(Opts, " ,"))};
        (["template", "import"]) ->
            {import, []};
        (["template", "export", Opts | _]) ->
            {export, get_template_opts(export, string:tokens(Opts, " ,"))};
        (["template", "export"]) ->
            {export, []};
        (_) ->
            {unknown, []}
    end, ParamsTplsLists_0).

get_wscr_params(Cont_0) ->
    Cont = wscr_to_proplist(Cont_0),
    ParamsTitle = case orddict:find("params_title", Cont) of
        error      -> get_wscr_params_default_param_title();
        {ok, Val1} -> Val1
    end,
    ParamsPreview = case orddict:find("params_preview", Cont) of
        error      -> false;
        {ok, _}    -> true
    end,
    ParamsLists_0 = case orddict:find("params", Cont) of
        error      -> [];
        {ok, Val2} -> Val2
    end,
    ParamsTplsLists = get_wscr_templates(Cont),
    ParamsLists = 
        lists:map(fun
            (["param", ParamName, ParamDefault, ParamKey | _]) ->
                {ParamName, get_wscr_param_number(ParamDefault),
                    [{key,list_to_atom(ParamKey)}]};
            (["param", ParamName, ParamDefault]) ->
                {ParamName, get_wscr_param_number(ParamDefault)};
            (_) ->
                {"???", 0}
        end, ParamsLists_0),
    
    {ok, ParamsTitle, ParamsPreview, ParamsLists, ParamsTplsLists}.

get_wscr_import_export_params(Cont_0) ->
    Cont = wscr_to_proplist(Cont_0),
    case orddict:find("extensions", Cont) of
        error      -> ExtensionsLists = [];
        {ok, Val2} -> ExtensionsLists = Val2
    end,
    Templates = get_wscr_templates(Cont),
    ExtList = lists:map(fun
            (["ext", Extension, ExtensionDesc | _]) ->
                {Extension, ExtensionDesc};
            (_) ->
                {".txt", "???"}
        end, ExtensionsLists),
    {ok, ExtList, Templates}.

        
fill_extra_file_inputs(ExtraFileChoosers, #command_rec{extrafileinputs=ExtraFiles}=CommandRec, _St, F)
  when length(ExtraFileChoosers) > length(ExtraFiles) ->
    {FileVarName, ChooserPs} = lists:nth(length(ExtraFiles)+1, ExtraFileChoosers),
    Title = proplists:get_value(title, ChooserPs, ?__(1, "Choose File")),
    Exts  = proplists:get_value(exts,  ChooserPs, []),
    case get_wscr_import_export_params([Exts]) of
        {ok, Extensions, _} ->
            Ps = [{extensions,Extensions},{title,Title}],
            wpa:import_filename(Ps, fun(N) ->
                F( CommandRec#command_rec{
                    extrafileinputs=[{FileVarName, N} | CommandRec#command_rec.extrafileinputs]
                } )
            end)
    end;
fill_extra_file_inputs(ExtraFileChoosers, #command_rec{extrafileinputs=ExtraFiles}=_CommandRec, _St, _F)
  when length(ExtraFileChoosers) =:= length(ExtraFiles) ->
    {file_inputs, ExtraFiles}.


fill_extra_files(ExtraFileChoosers, #command_rec{extrafileinputs=ExtraFiles}=_CommandRec)
  when length(ExtraFileChoosers) =:= length(ExtraFiles) ->
    {file_inputs, ExtraFiles}.

to_dialog_params(List, State, Dict) ->
    to_dialog_params(List, State, Dict, []).
to_dialog_params([{A,Number,C} | List], State, Dict, OList) when is_integer(Number); is_float(Number) ->
    to_dialog_params(List, State, Dict, [{A, Number, C} | OList]);
to_dialog_params([{A,Number} | List], State, Dict, OList) when is_integer(Number); is_float(Number) ->
    to_dialog_params(List, State, Dict, [{A, Number} | OList]);
to_dialog_params([{A,PV,C} | List], State, Dict, OList) ->
    {Val, _} = crun_pv(PV, State, Dict),
    to_dialog_params(List, State, Dict, [{A, Val, C} | OList]);
to_dialog_params([{A,PV} | List], State, Dict, OList) ->
    {Val, _} = crun_pv(PV, State, Dict),
    to_dialog_params(List, State, Dict, [{A, Val} | OList]);
to_dialog_params([], _, _, OList) ->
    lists:reverse(OList).

append_extra_files(ExtraFiles, PSScriptParams) ->
    [{K1,{string,V1}} || {K1,V1} <- ExtraFiles] ++ PSScriptParams.


make_shape_askdialog(
  Params, Title, false, SettableParams, State_1, Dict, Templates,
  CommandRec, _St) ->
    askdialog(Params, Title,
        to_dialog_params(SettableParams, State_1, Dict), Templates,
        fun(Res) -> {shape,{shape_from_script, {CommandRec, Res}}} end
        );
make_shape_askdialog(
  Params, Title, true, SettableParams, State_1, Dict, Templates,
  #command_rec{scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St) ->
    case run_script_w_preview(ScriptType, ScriptFileName,
        get_settings_for_run_script())
    of {ok, FP} ->
            askdialog_w_prev(Params, Title,
                to_dialog_params(SettableParams, State_1, Dict), Templates,
                fun(Res) -> {shape,{shape_from_script, {CommandRec, Res}}} end, St, FP
                )
    end.


make_shape_from_script(Params, #command_rec{wscrcont=WSCRContent}=CommandRec, St)
  when is_atom(Params) ->
    Dict = orddict:from_list([{"st", St}]),
    {_, State_1} = crun_section("params_init", WSCRContent, Dict),
    ExtraFileChoosers = find_extra_file_sections(WSCRContent),
    case fill_extra_file_inputs(ExtraFileChoosers, CommandRec, St, fun (CommandRec_1) ->
            {shape,{shape_from_script, {CommandRec_1, Params}}}
    end) of
        {file_inputs, _ExtraFiles} ->
            case get_wscr_params(WSCRContent) of
                {ok, _Title, _, [], []} ->
                    {shape,{shape_from_script, {CommandRec, []}}};
                {ok, Title, PrevMode, SettableParams, Templates}
                  when length(SettableParams) > 0;
                       length(Templates) > 0 ->
                    make_shape_askdialog(
                      Params, Title, PrevMode, SettableParams, State_1,
                      Dict, Templates, CommandRec, St)
            end;
        ReturnBack -> ReturnBack
    end;
make_shape_from_script(ScriptParams, #command_rec{wscrcont=WSCRContent,scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St)
  when is_list(ScriptParams) ->
    case fill_extra_files(find_extra_file_sections(WSCRContent), CommandRec) of
        {file_inputs, ExtraFiles} ->
            Dict = orddict:from_list([{"st", St}, {"params", ScriptParams}]),
            {ParamsSetVars, _} = crun_section("params_set", WSCRContent, Dict),
            case orddict:find(script_params, ParamsSetVars) of
                {ok, PSScriptParams} -> PSScriptParams;
                _ -> PSScriptParams = []
            end,
            PSScriptParams_1 = [[binstr(K), V]
                || {K, V} <- append_extra_files(ExtraFiles, PSScriptParams)],
            case run_script_once(ScriptType, ScriptFileName, ScriptParams,
                PSScriptParams_1,
                get_settings_for_run_script(), keep)
            of
                {ok, {new_shape, Pfx, #e3d_object{}=Obj, Mat}} ->
                    %% The e3d_file returned needs to be adjusted.
                    TempFolder = temp_folder(),
                    {#e3d_file{mat=Mat_1}=_, TempFiles} =
                        e3df_from_script(#e3d_file{mat=Mat}, TempFolder, []),
                    delete_temps(TempFolder, TempFiles),
                    {new_shape, Pfx, Obj, Mat_1};
                {ok, Return} ->
                    Return;
                {error, Err} ->
                    {error, Err}
            end
    end.


command_askdialog(
  Params, Title, false, SettableParams, State_1, Dict, Templates, Op,
  CommandRec, _St) ->
    askdialog(Params, Title,
        to_dialog_params(SettableParams, State_1, Dict), Templates,
        fun(Res) -> {Op,{command_from_script, {CommandRec, Res}}} end
        );
command_askdialog(
  Params, Title, true, SettableParams, State_1, Dict, Templates, Op,
  #command_rec{scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St) ->
    case run_script_w_preview(ScriptType, ScriptFileName,
        get_settings_for_run_script())
    of {ok, FP} ->
        askdialog_w_prev(Params, Title,
            to_dialog_params(SettableParams, State_1, Dict), Templates,
            fun(Res) -> {Op,{command_from_script, {CommandRec, Res}}} end, St, FP
            )
    end.

command_from_script(Op, Params, #command_rec{wscrcont=WSCRContent}=CommandRec, St)
  when is_atom(Params) ->
    Dict = orddict:from_list([{"st", St}]),
    {_, State_1} = crun_section("params_init", WSCRContent, Dict),
    case fill_extra_file_inputs(find_extra_file_sections(WSCRContent), CommandRec, St, fun (CommandRec_1) ->
            {Op,{command_from_script, {CommandRec_1, Params}}}
    end) of
        {file_inputs, _ExtraFiles} ->
            case get_wscr_params(WSCRContent) of
                {ok, _Title, _, [], []} ->
                    {Op, {command_from_script, {CommandRec, []}}};
                {ok, Title, PrevMode, SettableParams, Templates}
                  when length(SettableParams) > 0;
                       length(Templates) > 0 ->
                    command_askdialog(
                      Params, Title, PrevMode, SettableParams, State_1, Dict,
                      Templates, Op, CommandRec, St)
            end;
        ReturnBack -> ReturnBack
    end;
command_from_script(Op, ScriptParams, #command_rec{wscrcont=WSCRContent,
    scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St)
  when is_list(ScriptParams) ->

    case fill_extra_files(find_extra_file_sections(WSCRContent), CommandRec) of
        {file_inputs, ExtraFiles} ->
            Dict = orddict:from_list([{"st", St}, {"params", ScriptParams}, {"op", Op}]),
            {ParamsSetVars, _} = crun_section("params_set", WSCRContent, Dict),
            case orddict:find(script_params, ParamsSetVars) of
                {ok, PSScriptParams} -> PSScriptParams;
                _ -> PSScriptParams = []
            end,
            %% TODO: Extra param: edges
            %% TODO: Extra param: selected
            %% TODO:
            %%  change_points scripts can be in either 'body' or 'face' selection mode.
            %%  When in 'body' mode, the whole mesh is changed.
            %%  When in 'face' mode, selected contains the points inside the face
            %%  selection (to figure out: if all points of face is included, or only those
            %%  inside the region selection (all points, less points of the outer edges).
            %%
            {CmdInputs,CmdChanges} = wscr_command(WSCRContent),
            PSScriptParams_1 = [[binstr(K), V]
                || {K, V} <- append_extra_files(ExtraFiles, PSScriptParams)],
            {St1, Changed} = wings_sel:mapfold(fun (_ShapeBody, We0, Changed_0) ->
                {CmdExtraParams, ModCache} = command_extra_params(CmdInputs, We0),
                case run_script_once(ScriptType, ScriptFileName, ScriptParams,
                    [[<<"op">>, Op] |
                     CmdExtraParams ++ PSScriptParams_1],
                    get_settings_for_run_script(), error)
                of
                    {ok, Return} ->
                        command_modifications(Return, ModCache, CmdChanges, We0, Changed_0);
                    {error, Err} ->
                        io:format("Error: ~p: ~p~n", [ScriptFileName, Err]),
                        %% wings_error:error(Err},
                        {We0, Changed_0}
                end
            end, false, St),
            case Changed of
                true ->
                    {save_state, St1};
                false ->
                    St
            end
    end.
    
wscr_command(Cont_0) ->
    Cont = wscr_to_proplist(Cont_0),
    case orddict:find("command_changes", Cont) of
        error      -> CmdChanges = [];
        {ok, Val1} -> CmdChanges = string:tokens(Val1, " ,")
    end,
    case orddict:find("command_inputs", Cont) of
        error      -> CmdInputs = CmdChanges;
        {ok, Val2} -> CmdInputs = string:tokens(Val2, " ,")
    end,
    {wscr_command_1(CmdInputs), wscr_command_1(CmdChanges)}.
wscr_command_1(List) ->
    wscr_command_1(List, []).
wscr_command_1([A|List], OL) ->
    A_1 = case A of
        "points" -> points;
        "selected" -> selected;
        "faces" -> faces;
        "face_colors" -> face_colors;
        "face_uvs" -> face_uvs;
        "edges" -> edges;
        _ -> unknown
    end,
    wscr_command_1(List, [A_1|OL]);
wscr_command_1([], OL) ->
    [A || A <- lists:reverse(OL), A =/= unknown].


%% Choose extra parameters to send to command scripts
%%
command_extra_params(R, We0) ->
    command_extra_params(R, We0, [], []).
command_extra_params([points | R], #we{vp=Vtab}=We0, EP, MC) ->
    Vtab_0 = array:to_orddict(Vtab),
    %% Remove the undefined atoms before sending it to the script.
    Vtab_1 = lists:filter(fun ({_, CoordTup}) -> is_tuple(CoordTup) end, Vtab_0),
    P = [<<"points">>, Vtab_1],
    command_extra_params(R, We0, [P | EP], [{points, Vtab_1} | MC]);
command_extra_params([selected | R], #we{vp=_Vtab}=We0, EP, MC) ->
    P = [<<"selected">>, []],
    command_extra_params(R, We0, [P | EP], MC);

command_extra_params([faces | R], #we{fs=Ftab}=We0, EP, MC) ->
    FL = gb_trees:keys(Ftab),
    Colors = [{FIdx, bool_none(wings_va:face_attr(color, FIdx, We0))} || FIdx <- FL],
    UVs = [{FIdx, bool_none(wings_va:face_attr(uv, FIdx, We0))} || FIdx <- FL],
    Faces = [{FIdx, {
        wings_face:vertices_ccw(FIdx, We0),
        bool_none(wings_va:face_attr(color, FIdx, We0)),
        bool_none(wings_va:face_attr(uv, FIdx, We0))
    }} || FIdx <- FL],
    P = [<<"faces">>, Faces],
    command_extra_params(R, We0, [P | EP],
        [{face_uvs, UVs}, {face_colors, Colors} | MC]);
command_extra_params([face_colors | R], #we{fs=Ftab}=We0, EP, MC) ->
    FL = gb_trees:keys(Ftab),
    Colors = [{FIdx, bool_none(wings_va:face_attr(color, FIdx, We0))} || FIdx <- FL],
    P = [<<"face_colors">>, Colors],
    command_extra_params(R, We0, [P | EP], [{face_colors, Colors} | MC]);
command_extra_params([face_uvs | R], #we{fs=Ftab}=We0, EP, MC) ->
    FL = gb_trees:keys(Ftab),
    UVs = [{FIdx, bool_none(wings_va:face_attr(uv, FIdx, We0))} || FIdx <- FL],
    P = [<<"face_uvs">>, UVs],
    command_extra_params(R, We0, [P | EP], [{face_uvs, UVs} | MC]);
command_extra_params([edges | R], #we{vp=_Vtab}=We0, EP, MC) ->
    P = [<<"edges">>, []],
    command_extra_params(R, We0, [P | EP], MC);
command_extra_params([], _We0, EP, MC) ->
    {lists:reverse(EP), lists:reverse(MC)}.


%% Modify the We structure with modifications returned from the command script
%%
command_modifications(Returned, MC, Changeable, We0, Changed) ->
    command_modifications(Returned, MC, Changeable, We0, false, Changed).
command_modifications([{set_points, Vtab_2} | R], MC, Changeable, We0, LocalChange, Changed) ->
    Vtab_1 = proplists:get_value(points, MC, none),
    case proplists:get_value(points, Changeable, false) of
        true when length(Vtab_2) =:= length(Vtab_1) ->
            We1 = We0#we{vp=array:from_orddict(Vtab_2)},
            ?DEBUG_FMT("Got points:~p~n", [Vtab_2]),
            command_modifications(R, MC, Changeable, We1, true, Changed);
        _ ->
            %% Returned points should be exactly the same length as input
            command_modifications(R, MC, Changeable, We0, LocalChange, Changed)
    end;
command_modifications([{set_face_uvs, Vtab_2} | R], MC, Changeable, We0, LocalChange, Changed) ->
    Vtab_1 = proplists:get_value(face_uvs, MC, none),
    case proplists:get_value(face_uvs, Changeable, false) of
        true when length(Vtab_2) =:= length(Vtab_1) ->
            We1 = lists:foldl(fun({FNum, UVList}, W) ->
                set_face_va_uv(FNum, unbool_none(UVList), W)
            end, We0, Vtab_2),
            command_modifications(R, MC, Changeable, We1, true, Changed);
        _ ->
            %% Returned face_uvs should be exactly the same length as input
            command_modifications(R, MC, Changeable, We0, LocalChange, Changed)
    end;
command_modifications([{set_face_colors, Vtab_2} | R], MC, Changeable, We0, LocalChange, Changed) ->
    Vtab_1 = proplists:get_value(face_colors, MC, none),
    case proplists:get_value(face_colors, Changeable, false) of
        true when length(Vtab_2) =:= length(Vtab_1) ->
            We1 = lists:foldl(fun({FNum, ColorList}, W) ->
                set_face_va_color(FNum, unbool_none(ColorList), W)
            end, We0, Vtab_2),
            command_modifications(R, MC, Changeable, We1, true, Changed);
        _ ->
            %% Returned face_colors should be exactly the same length as input
            command_modifications(R, MC, Changeable, We0, LocalChange, Changed)
    end;
command_modifications([], _MC, _Changeable, We0, false, Changed) ->
    ?DEBUG_FMT("Did not change:~p~n", [Return]),
    {We0, Changed};
command_modifications([], _MC, _Changeable, We0, true, _) ->
    {We0, true}.
    

set_face_va_uv(Face, NewUV, #we{fs=Ftab}=We0) ->
    Edge = gb_trees:get(Face, Ftab),
    set_face_va_uv(Face, Edge, Edge, lists:reverse(NewUV), We0).
set_face_va_uv(_, LastEdge, LastEdge, [], We) -> We;
set_face_va_uv(Face, Edge, LastEdge, [NUV|NewUV], #we{es=Etab}=We0) ->
    case array:get(Edge, Etab) of
    #edge{lf=Face,ltsu=NextEdge} ->
        set_face_va_uv(Face, NextEdge, LastEdge, NewUV, 
            set_face_va_uv_set(Edge, Face, NUV, We0));
    #edge{rf=Face,rtsu=NextEdge} ->
        set_face_va_uv(Face, NextEdge, LastEdge, NewUV, 
            set_face_va_uv_set(Edge, Face, NUV, We0))
    end.
set_face_va_uv_set(Edge, Face, NUV, We0) ->
    wings_va:set_edge_attrs(Edge, Face,
        new_uv(NUV, wings_va:edge_attrs(Edge, Face, We0)),
        We0).
new_uv(UV, OA) ->
    wings_va:new_attr(wings_va:attr(uv, OA), UV).

set_face_va_color(Face, NewColors, #we{fs=Ftab}=We0) ->
    Edge = gb_trees:get(Face, Ftab),
    set_face_va_color(Face, Edge, Edge, lists:reverse(NewColors), We0).
set_face_va_color(_, LastEdge, LastEdge, [], We) -> We;
set_face_va_color(Face, Edge, LastEdge, [NColor|NewColors], #we{es=Etab}=We0) ->
    case array:get(Edge, Etab) of
    #edge{lf=Face,ltsu=NextEdge} ->
        set_face_va_color(Face, NextEdge, LastEdge, NewColors,
            set_face_va_color_set(Edge, Face, NColor, We0));
    #edge{rf=Face,rtsu=NextEdge} ->
        set_face_va_color(Face, NextEdge, LastEdge, NewColors,
            set_face_va_color_set(Edge, Face, NColor, We0))
    end.
set_face_va_color_set(Edge, Face, NColor, We0) ->
    wings_va:set_edge_attrs(Edge, Face,
        new_color(NColor, wings_va:edge_attrs(Edge, Face, We0)),
        We0).
new_color(Color, OA) ->
    wings_va:new_attr(Color, wings_va:attr(uv, OA)).


bool_none(L) ->
    [bool_none_1(A) || A <- L].
bool_none_1(none) -> false;
bool_none_1(A) -> A.

unbool_none(L) ->
    [unbool_none_1(A) || A <- L].
unbool_none_1(false) -> none;
unbool_none_1(A) -> A.



import_export_from_script(Op, Params, #command_rec{wscrcont=WSCRContent}=CommandRec, St)
  when is_atom(Params) ->
    Dict = orddict:from_list([{"st", St}]),
    {_, State_1} = crun_section("params_init", WSCRContent, Dict),
    case get_wscr_params(WSCRContent) of
        {ok, _Title, _, [], []} ->
            {file, {Op, {import_export_from_script, {CommandRec, []}}}};
        {ok, Title, _, SettableParams, Templates}
            when length(SettableParams) > 0;
                 length(Templates) > 0 ->
            askdialog(Params, Title,
                to_dialog_params(SettableParams, State_1, Dict), Templates,
                fun(Res) -> {file, {Op,{import_export_from_script, {CommandRec, Res}}}} end)
    end;
import_export_from_script(import, ScriptParams, #command_rec{wscrcont=WSCRContent,scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St) ->
    case fill_extra_file_inputs(find_extra_file_sections(WSCRContent), CommandRec, St, fun (CommandRec_1) ->
            {file, {import, {import_export_from_script, {CommandRec_1, ScriptParams}}}}
    end) of
        {file_inputs, ExtraFiles} ->
            Dict = orddict:from_list([{"st", St}, {"params", ScriptParams}]),
            {ParamsSetVars, _} = crun_section("params_set", WSCRContent, Dict),
            case orddict:find(import_params, ParamsSetVars) of
                {ok, ImportParams_0} -> ImportParams_0;
                _ -> ImportParams_0 = []
            end,
            case orddict:find(script_params, ParamsSetVars) of
                {ok, PSScriptParams} -> PSScriptParams;
                _ -> PSScriptParams = []
            end,
            {ScrSpecPs, ImportParams} = lists:partition(
                fun script_specific_param/1, ImportParams_0),
            
            case get_wscr_import_export_params(WSCRContent) of
                {ok, Extensions, Templates} ->
                    Props = [{extensions, Extensions} | ImportParams],
                    wpa:import(Props, fun(F) ->
                        PSScriptParams_1 = [[binstr(K), V]
                            || {K, V} <- append_extra_files(ExtraFiles, PSScriptParams)],
                        TempFolder = temp_folder(),
                        case run_script_once(ScriptType, ScriptFileName, ScriptParams,
                            [ [<<"filename">>, binstr(F)],
                              [<<"temp_folder">>, binstr(TempFolder)]
                              | PSScriptParams_1],
                            get_settings_for_run_script(), error)
                        of
                            {ok, ReturnedE3D} ->
                                case ReturnedE3D of
                                    {ok, #e3d_file{}=ReturnedE3D_1} ->
                                        %% The e3d_file returned needs to be adjusted.
                                        {Ret_1, TempFiles} = e3df_from_script(ReturnedE3D_1, TempFolder, ScrSpecPs),
                                        delete_temps(TempFolder, TempFiles),
                                        {ok, import_transform(Ret_1, ScriptParams, Templates)};
                                    _ ->
                                        ReturnedE3D
                                end;
                            {error, Err} -> {error, Err}
                        end
                    end, St)
            end;
        ReturnBack -> ReturnBack
    end;
import_export_from_script(Op, ScriptParams, #command_rec{wscrcont=WSCRContent,
  scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St)
  when Op =:= export; Op =:= export_selected ->
    case fill_extra_file_inputs(find_extra_file_sections(WSCRContent), CommandRec, St, fun (CommandRec_1) ->
            {file, {Op, {import_export_from_script, {CommandRec_1, ScriptParams}}}}
    end) of
        {file_inputs, ExtraFiles} ->
            Dict = orddict:from_list([{"st", St}, {"params", ScriptParams}]),
            {ParamsSetVars, _} = crun_section("params_set", WSCRContent, Dict),
            case orddict:find(export_params, ParamsSetVars) of
                {ok, ExportParams_0} -> ExportParams_0;
                _ -> ExportParams_0 = []
            end,
            case orddict:find(script_params, ParamsSetVars) of
                {ok, PSScriptParams} -> PSScriptParams;
                _ -> PSScriptParams = []
            end,
            {ScrSpecPs, ExportParams} = lists:partition(
                fun script_specific_param/1, ExportParams_0),
            
            case get_wscr_import_export_params(WSCRContent) of
                {ok, Extensions, Templates} ->
                    Props = [{extensions, Extensions} | ExportParams],
                    wpa:Op(Props, fun(F, E3DCont_00) ->
                        E3DCont_0 = mesh_export_changes(export_transform(E3DCont_00, ScriptParams, Templates), Props),
                        E3DCont = export_texture_options(E3DCont_0, F, ScriptParams, ScrSpecPs),
                        
                        PSScriptParams_1 = [[binstr(K), V]
                            || {K, V} <- append_extra_files(ExtraFiles, PSScriptParams)],
                        TempFolder = temp_folder(),
                        {E3DCont_1, TempFiles} = e3df_filename_tup(E3DCont, TempFolder, ScrSpecPs),
                        case run_script_once(ScriptType, ScriptFileName, ScriptParams,
                            [ [<<"filename">>, binstr(F)],
                              [<<"content">>, E3DCont_1],
                              [<<"temp_folder">>, binstr(TempFolder)]
                              | PSScriptParams_1],
                            get_settings_for_run_script(), error)
                        of
                            {ok, Return} ->
                                delete_temps(TempFolder, TempFiles),
                                ?DEBUG_FMT("RETURNED: ~p~n", [Return]),
                                case Return of
                                    {ok} -> ok;
                                    _    -> {error, "Error"}
                                end;
                            {error, Err} ->
                                delete_temps(TempFolder, TempFiles),
                                {error, Err}
                        end
                    end, St)
            end;
        ReturnBack -> ReturnBack
    end.
    
export_texture_options(E3DContent, FileName, Attr, ScrSpecPs) ->
    Conv = proplists:get_value(script_texture_convert, ScrSpecPs, bytes),
    case Conv of
        %% Set from exporter template settings
        user ->
            Dir = filename:dirname(FileName),
            Filetype = proplists:get_value(default_filetype, Attr, ".png"),
            E3DContent_1 = wpa:save_images(E3DContent, Dir, Filetype),
            E3DContent_1;
        %% Something else
        _ ->
            E3DContent
    end.
    
    

script_specific_param({script_texture_convert,_}) -> true;
script_specific_param(_) -> false.


import_transform(E3dFile, KeyVals, [{import, _}|Templates]) ->
    import_transform(import_transform_1(E3dFile, KeyVals), KeyVals, Templates);
import_transform(E3dFile, KeyVals, [_|Templates]) ->
    import_transform(E3dFile, KeyVals, Templates);
import_transform(E3dFile, _, []) ->
    E3dFile.
import_transform_1(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

export_transform(E3dFile, KeyVals, [{export,_}|Templates]) ->
    export_transform(export_transform_1(E3dFile, KeyVals), KeyVals, Templates);
export_transform(E3dFile, KeyVals, [_|Templates]) ->
    export_transform(E3dFile, KeyVals, Templates);
export_transform(E3dFile, _, []) ->
    E3dFile.
export_transform_1(E3dFile, KeyVals) ->
    Mat = wpa:export_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).


mesh_export_changes(#e3d_file{objs=O}=E3DCont, Props) ->
    case proplists:get_value(include_normals, Props, false) of
        true ->
            O_1 = [mesh_export_changes_1(Mesh) || Mesh <- O],
            E3DCont#e3d_file{objs=O_1};
        _ ->
            E3DCont
    end.
mesh_export_changes_1(#e3d_object{obj=Mesh}=Obj) ->
    Mesh_1 = e3d_mesh:vertex_normals(Mesh),
    Obj#e3d_object{obj=Mesh_1}.



%% Change the e3d_file tuple a little bit for the script
e3df_filename_tup(#e3d_file{objs=Objs,creator=Creator,dir=Dir,mat=Mats}=E3DCont, TempFolder, ScrSpecPs) ->
    {NewMats, Temps} = lists:foldl(
        fun({Id, MatAL}, {L1_Mat, L1_Temp}) ->
            ListMaps = proplists:get_value(maps, MatAL, []),
            
            {NewListMaps, NewTemps} = lists:foldl(fun ({MapType, MapVal}, {A_Mat, A_Temp}) ->
                case e3df_image(MapVal, length(A_Mat), TempFolder, ScrSpecPs) of
                    {temp, File, T} -> {[{MapType, T} | A_Mat], [File | A_Temp]};
                    {no_temp, T}    -> {[{MapType, T} | A_Mat], A_Temp}
                end
            end, {[], []}, ListMaps),
            
            {[{Id, orddict:store(maps, NewListMaps, orddict:from_list(MatAL))} | L1_Mat], NewTemps ++ L1_Temp}
        end, {[], []}, Mats),
    {E3DCont#e3d_file{
        objs=[e3df_filename_tup_object(E3DO) || E3DO <- Objs],
        mat=lists:reverse(NewMats),
        creator=stringtup_if_not_atom(Creator),
        dir=stringtup_if_not_atom(Dir)
    }, Temps}.
e3df_filename_tup_object(#e3d_object{name=Name}=E3DO) ->
    E3DO#e3d_object{
        name=stringtup_if_not_atom(Name)
    }.
e3df_image(T, Number, TempFolder, ScrSpecPs) ->
    Conv = proplists:get_value(script_texture_convert, ScrSpecPs, bytes),
    case Conv of
        user ->
            #e3d_image{filename=FlNm}=T,
            T_1=T#e3d_image{filename=liststr_to_binstring(FlNm)},
            Empty = <<>>,
            {no_temp, T_1#e3d_image{image=Empty}};
        _ ->
            e3df_image_1(T, Number, TempFolder, Conv)
    end.
e3df_image_1(#e3d_image{image=ImageBin}=T, Number, TempFolder, Conv) ->
    case is_binary(ImageBin) of
        true ->
            C = abs(erlang:unique_integer()),
            FName = "raw_image_" ++ integer_to_list(C) ++ "_" ++ integer_to_list(Number),
            TempFile = filename:join(TempFolder, FName),
            ok = filelib:ensure_dir(TempFile),
            {ok, TempFile_1} = e3df_image_conv(Conv, TempFile, T),
            {temp, TempFile, T#e3d_image{image=binstr(TempFile_1)}};
        false ->
            {no_temp, T}
    end.
e3df_image_conv(".png", TempFile_0, E3DImg) ->
    TempFile = TempFile_0 ++ ".png",
    ok = e3d__png:save(E3DImg, TempFile),
    {ok, TempFile};
e3df_image_conv(".bmp", TempFile_0, E3DImg) ->
    TempFile = TempFile_0 ++ ".bmp",
    ok = e3d__bmp:save(E3DImg, TempFile),
    {ok, TempFile};
e3df_image_conv(".tga", TempFile_0, E3DImg) ->
    TempFile = TempFile_0 ++ ".tga",
    ok = e3d__tga:save(E3DImg, TempFile),
    {ok, TempFile};
e3df_image_conv(bytes, TempFile, #e3d_image{image=ImageBin}=_) ->
    ok = file:write_file(TempFile, ImageBin),
    {ok, TempFile}.

stringtup_if_not_atom(Str) when is_atom(Str) -> Str;
stringtup_if_not_atom(Str) when is_list(Str) ->
    {string, liststr_to_binstring(Str)}.
    


e3df_from_script(#e3d_file{mat=Mats}=ReturnedE3D, TempFolder, ScrSpecPs) ->
    {NewMats, Temps} = lists:foldl(
        fun({Id, MatAL}, {L1_Mat, L1_Temp}) ->
            ListMaps = proplists:get_value(maps, MatAL, []),
            
            {NewListMaps, NewTemps} = lists:foldl(fun ({MapType, MapVal}, {A_Mat, A_Temp}) ->
                case e3df_from_script_image(MapVal, TempFolder, ScrSpecPs) of
                    {temp, File, T} -> {[{MapType, T} | A_Mat], [File | A_Temp]};
                    {no_temp, T}    -> {[{MapType, T} | A_Mat], A_Temp}
                end
            end, {[], []}, ListMaps),
            
            MatAL_1 = orddict:store(maps, NewListMaps, orddict:from_list(MatAL)),
            {[{Id, MatAL_1} | L1_Mat], NewTemps ++ L1_Temp}
        end, {[], []}, Mats),
    {ReturnedE3D#e3d_file{
        mat=NewMats
    }, Temps}.
e3df_from_script_image(T, TempFolder, ScrSpecPs) ->
    Conv = proplists:get_value(script_texture_convert, ScrSpecPs, bytes),
    e3df_from_script_image_1(Conv, T, TempFolder).
e3df_from_script_image_1(bytes, #e3d_image{image=TempFile_0}=T, TempFolder) ->
    TempFile_1 = filename:absname(TempFile_0, TempFolder),
    case file:read_file_info(TempFile_1) of
        {ok, _} ->
            T_1 = e3df_from_script_image_conv(bytes, T, TempFile_1),
            case path_contains_dots(TempFile_1)
            of
                false ->
                    {temp, TempFile_1, T_1};
                true ->
                    %% This is not an expected temp file so we won't try to delete it.
                    {no_temp, T_1}
            end;
        _ ->
            io:format("~p: Filename not found: ~p~n", [?MODULE, TempFile_1]),
            error(e3d_image_raw_filename_not_found)
    end;
e3df_from_script_image_1(Conv, #e3d_image{filename=TempFile_1}=T, _TempFolder) ->
    case file:read_file_info(TempFile_1) of
        {ok, _} ->
            T_1 = e3df_from_script_image_conv(Conv, T, TempFile_1),
            {no_temp, T_1};
        _ ->
            io:format("~p: Filename not found: ~p~n", [?MODULE, TempFile_1]),
            error(e3d_image_filename_not_found)
    end.

path_contains_dots(FilePath) ->
    TempFile_Split = filename:split(FilePath),
    lists:member(<<"..">>, TempFile_Split) orelse
    lists:member(<<".">>, TempFile_Split)  orelse
    lists:member(  ".."  , TempFile_Split) orelse
    lists:member(  "."  , TempFile_Split).


e3df_from_script_image_conv(auto, T, TempFile) ->
    {ok, E3DIm} = get_bitmap_by_ext(TempFile),
    merge_e3d_image(E3DIm, T);
e3df_from_script_image_conv(".png", T, TempFile) ->
    {ok, E3DIm} = read_png(TempFile),
    merge_e3d_image(E3DIm, T);
e3df_from_script_image_conv(".jpg", T, TempFile) ->
    {ok, E3DIm} = read_jpeg(TempFile),
    merge_e3d_image(E3DIm, T);
e3df_from_script_image_conv(bytes, T, TempFile) ->
    {ok, ImageBin} = file:read_file(TempFile),
    T#e3d_image{image=ImageBin}.
merge_e3d_image(
      #e3d_image{name=NA,extra=EA}=A,
      #e3d_image{name=NB,extra=EB}) ->
    A#e3d_image{
        name=merge_e3d_image_name(NA,NB),
        extra=merge_e3d_image_extra(EA,EB)
    }.
merge_e3d_image_name(A, []) -> A;
merge_e3d_image_name(_, A) -> A.
merge_e3d_image_extra(A, []) -> A;
merge_e3d_image_extra(_, A) -> A.

get_bitmap_by_ext(FilePath) ->
    case string:to_lower(filename:extension(FilePath)) of
        ".jpeg" -> Ext = ".jpg";
        Ext_0   -> Ext = Ext_0
    end,
    case Ext of
        ".png" ->
            F = fun read_png/1;
        ".jpg" ->
            F = fun read_jpeg/1;
        _ ->
            F = fun read_default/1
    end,
    F(FilePath).

read_jpeg(FileName) ->
    BlockWxMsgs = wxLogNull:new(),
    Ret = read_jpeg_1(FileName),
    wxLogNull:destroy(BlockWxMsgs),
    Ret.
read_jpeg_1(FileName) ->
    Image = wxImage:new(),
    case wxImage:loadFile(Image, FileName) of
        true ->
            E3d = wings_image:wxImage_to_e3d(Image),
            wxImage:destroy(Image),
            {ok, e3d_image:fix_outtype(FileName, E3d, [])};
        false ->
            {error, none}
    end.
read_png(FileName) ->
    case e3d__png:load(FileName) of
        E3D=#e3d_image{} ->
            {ok, E3D};
        {error, Err} -> {error, Err}
    end.
read_default(FileName) ->
    case e3d_image:load(FileName) of
        E3DImage=#e3d_image{} ->
            {ok, E3DImage};
        {error, Err} -> {error, Err}
    end.


temp_folder() ->
    TempFolder = filename:basedir(user_cache, "wings_" ++ atom_to_list(?MODULE)),
    TempFolder.


%% Delete temporary files left by e3d_image script interfacing, the files
%% must be inside the expected temporary folder or they won't be deleted.
%%
delete_temps(_TempFolder, []) -> ok;
delete_temps(TempFolder, [_TempFile | TempFiles]) ->
    %% TODO: Delete _TempFile
    delete_temps(TempFolder, TempFiles).

get_settings_for_run_script() ->
[
    {setting_py_int_path, wpa:pref_get(?MODULE, setting_py_int_path, "python")},
    {setting_scm_int_path, wpa:pref_get(?MODULE, setting_scm_int_path, "csi")},
    {setting_scm_arguments, wpa:pref_get(?MODULE, setting_scm_arguments, "-q -n -b")}
].



%% WSCR Configuration Statements
%%
%% Statements to add specific variables for use by the script,
%% add configuation keys to the wings:import and wings:export
%% functions, and load and save preference on a per file or
%% global context.
%%

crun_section(Name, WSCRCont, Dict) ->
    crun_section(Name, WSCRCont, Dict, #crun_state{}).

crun_section(_, [], _, State_1) ->
    {[], State_1};
crun_section(Name, [[Name, W1] | _], Dict, State) when is_list(W1) ->
    {_, State_1} = crun_list(W1, State, Dict),
    {State_1#crun_state.sett_vars, State_1};
crun_section(Name, [_ | Rest_WSCRCont], Dict, State) ->
    crun_section(Name, Rest_WSCRCont, Dict, State).

find_extra_file_sections(WSCRCont) ->
    find_extra_file_sections("extra_file", WSCRCont).

find_extra_file_sections(Name, WSCRCont) ->
    find_extra_file_sections(Name, WSCRCont, []).

find_extra_file_sections(_, [], OSections) ->
    lists:reverse(OSections);
find_extra_file_sections(Name, [[Name, FileVarName, SubSections] | List], OSections) when is_list(SubSections) ->
    Section = extra_file_section(SubSections),
    OSections_1 = [{FileVarName, Section} | OSections],
    find_extra_file_sections(Name, List, OSections_1);
find_extra_file_sections(Name, [_ | Rest_WSCRCont], OSections) ->
    find_extra_file_sections(Name, Rest_WSCRCont, OSections).


extra_file_section(OSections) ->
    extra_file_section(OSections, []).

extra_file_section([], OSections) ->
    lists:reverse(OSections);
extra_file_section([["title", Title] | List], OSections) when is_list(Title) ->
    OSections_1 = orddict:store(title, Title, OSections),
    extra_file_section(List, OSections_1);
extra_file_section([["extensions", Extensions] | List], OSections) when is_list(Extensions) ->
    OSections_1 = orddict:store(exts, ["extensions", Extensions], OSections),
    extra_file_section(List, OSections_1);
extra_file_section([_ | Rest_WSCRCont], OSections) ->
    extra_file_section(Rest_WSCRCont, OSections).


%%%
%%%

crun("ifdef", [PV, OtherCode], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    if
        PVRes =/= error ->
            crun_list(OtherCode, State_1, Dict);
        true ->
            {ok, State_1}
    end;

crun("ifndef", [PV, OtherCode], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    if
        PVRes =:= error ->
            crun_list(OtherCode, State_1, Dict);
        true ->
            {ok, State_1}
    end;

crun("if", [PV1, Op, PV2, OtherCode], State, Dict)
  when Op =:= "eq" ; Op =:= "ne" ;
       Op =:= "gt" ; Op =:= "lt" ;
       Op =:= "ge" ; Op =:= "le" ->
    {PV1Res, State_1} = crun_pv(PV1, State, Dict),
    {PV2Res, State_2} = crun_pv(PV2, State_1, Dict),
    case case Op of
            "eq" -> PV1Res =:= PV2Res;
            "ne" -> PV1Res =/= PV2Res;
            "gt" -> PV1Res > PV2Res;
            "lt" -> PV1Res < PV2Res;
            "ge" -> PV1Res >= PV2Res;
            "le" -> PV1Res =< PV2Res;
            _    -> false
        end of
        true -> crun_list(OtherCode, State_2, Dict);
        false -> {ok, State_2}
    end;

crun("do", [PV], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    ?DEBUG_FMT("do ~p~n", [PVRes]),
    {PVRes, State_1};

crun("script_param", [K, PV], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    ?DEBUG_FMT("script_param ~s ~p~n", [K, PVRes]),
    case orddict:find(script_params, State_1#crun_state.sett_vars) of
        {ok, ScriptParams} -> ScriptParams;
        _ -> ScriptParams = []
    end,
    ScriptParams_1 = orddict:store(K, PVRes, ScriptParams),
    {PVRes, State_1#crun_state{
        sett_vars = orddict:store(
            script_params,
            ScriptParams_1,
            State_1#crun_state.sett_vars)
    }};

crun("import_param", [K, PV], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    %% Add to wings:import parameters key K from path value PV
    ?DEBUG_FMT("import_param ~s ~p~n", [K, PVRes]),
    case orddict:find(import_params, State_1#crun_state.sett_vars) of
        {ok, ImportParams} -> ImportParams;
        _ -> ImportParams = []
    end,
    ImportParams_1 = orddict:store(list_to_atom(K), PVRes, ImportParams),
    {PVRes, State_1#crun_state{
        sett_vars = orddict:store(
            import_params,
            ImportParams_1,
            State_1#crun_state.sett_vars)
    }};

crun("export_param", [K, PV], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    %% Add to wings:export parameters key K from path value PV
    ?DEBUG_FMT("export_param ~s ~p~n", [K, PVRes]),
    case orddict:find(export_params, State_1#crun_state.sett_vars) of
        {ok, ExportParams} -> ExportParams;
        _ -> ExportParams = []
    end,
    ExportParams_1 = orddict:store(list_to_atom(K), PVRes, ExportParams),
    {PVRes, State_1#crun_state{
        sett_vars = orddict:store(
            export_params,
            ExportParams_1,
            State_1#crun_state.sett_vars)
    }};

crun("save_pref", [K, PV], State, Dict) ->
    {PVRes, State_1} = crun_pv(PV, State, Dict),
    %% Save into preference from path value PV
    io:format("save_pref ~s ~p~n", [K, PVRes]),
    {PVRes, State_1};

crun("load_pref", [K, VS, PVDefault], State, Dict) ->
    case false of
        false ->
            {_PVRes, _State} = crun_pv(PVDefault, State, Dict)
            
    end,
    
    %% Load from preference and into value slot VS
    io:format("load_pref ~s ~p~n", [K, VS]),
    State_1 = State,
    {ok, State_1}.
    
%% Evaluate a path value
crun_pv(PV, State, Dict) ->
    T = etp_tok(PV, [], []),
    {PPV, []} = etp_start_parse(T),
    etp_run(PPV, State, Dict).
    

crun_list([], State, _Dict) ->
    {ok, State};
crun_list([C | List], State, Dict) ->
    [C0 | CArgs] = C,
    {_, State_1} = crun(C0, CArgs, State, Dict),
    crun_list(List, State_1, Dict).


%% WSCR Erlang term path notation

%% Path starts with the name of the variable to traverse
%% Options are: st

%% {N}   Get the Nth tuple element
%% /Name As an orddict, find Name
%% [N]   Get the Nth list element
%% Mod:Name(...) Call Mod:Name with arguments
%% >$N  Store current value into $N
%% <$N  Get value from $N
%% tuple(...) Construct tuple with arguments
%% list(...) Construct list with arguments
%% map(.., ...) Map to the inside code (1st arg) the list (2nd arg)
%% filter(.., ..) Filter to the inside code (1st arg) the list (2nd arg)
%% void(..., ...) Run the first argument only for its side effects and return the second argument
%% foldl(..., ..., ...)
%% foldr(..., ..., ...)
%% '...'  Atom
%% "..."  String
%% "," Separator

%% Run etp
etp_run([], State, _Dict) ->
    {State#crun_state.p, State};
etp_run([{start,{integer,N}} | R], State, Dict) ->
    etp_run(R, State#crun_state{p=N}, Dict);
etp_run([{start,{float,N}} | R], State, Dict) ->
    etp_run(R, State#crun_state{p=N}, Dict);
etp_run([{start,{string,N}} | R], State, Dict) ->
    etp_run(R, State#crun_state{p=unbinstr(N)}, Dict);
etp_run([{start,{atom,N}} | R], State, Dict) when is_binary(N) ->
    etp_run(R, State#crun_state{p=list_to_atom(unbinstr(N))}, Dict);
etp_run([{start,{call,Mod,Name,Args}} | R], State, Dict) ->
    %% Our config path language is lazy evaluated, evaluate all the arguments first
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    %% We actually call an erlang function here.
    Return = apply(list_to_atom(Mod), list_to_atom(Name), lists:reverse(Args_2)),
    etp_run(R, State_3#crun_state{p=Return}, Dict);
etp_run([{start,{call,Name,Args}} | R], State, Dict) ->
    {Return, State_1} = etp_run_call(Name, Args, State, Dict),
    etp_run(R, State_1#crun_state{p=Return}, Dict);
etp_run([{start,{word,Name}} | R], State, Dict) ->
    case orddict:find(Name, Dict) of
        {ok, V} ->
            etp_run(R, State#crun_state{p=V}, Dict);
        _ ->
            {error, State}
    end;

etp_run([{start,{get_from,Name}} | R], State, Dict) ->
    case orddict:find(Name, State#crun_state.temp_vars) of
        {ok, V} ->
            etp_run(R, State#crun_state{p=V}, Dict);
        _ ->
            {error, State}
    end;
    
etp_run([{rec_f, FieldName} | R], State, Dict) ->
    case tuple_n_from_field(State#crun_state.p, FieldName) of
        {ok, N} ->
            if
                N < tuple_size(State#crun_state.p) ->
                    V_1 = element(N+1, State#crun_state.p),
                    etp_run(R, State#crun_state{p=V_1}, Dict);
                true -> {error, State}
            end;
        _ -> {error, State}
    end;
etp_run([{tuple_nth,Ae} | R], State, Dict) ->
    {N, _State_1} = etp_get_integer(Ae, State, Dict),
    if
        N < tuple_size(State#crun_state.p) ->
            V_1 = element(N+1, State#crun_state.p),
            etp_run(R, State#crun_state{p=V_1}, Dict);
        true -> {error, State}
    end;
etp_run([{list_nth,Ae} | R], State, Dict) ->
    {N, _State_1} = etp_get_integer(Ae, State, Dict),
    V_1 = lists:nth(N, State#crun_state.p),
    etp_run(R, State#crun_state{p=V_1}, Dict);
etp_run([{orddict_f,Ae} | R], State, Dict) ->
    N = etp_get_term(Ae, State, Dict),
    if
        is_list(State#crun_state.p) ->
            try 
                V_1 = proplists:get_value(N, State#crun_state.p, error),
                etp_run(R, State#crun_state{p=V_1}, Dict)
            catch _ ->
                {error, State}
            end;
        true -> {error, State}
    end;
etp_run([{store_to, K} | R], State, Dict) ->
    State_2 = etp_store_temp(K, State#crun_state.p, State),
    etp_run(R, State_2, Dict).

etp_get_integer([{start,_}]=C, State, Dict) ->
    case etp_run(C, State, Dict) of
        {Ret, State_1} when is_integer(Ret) ->
            {Ret, State_1}
    end.
    
etp_get_term({word,Word}, _State, _Dict) ->
    list_to_atom(Word);
etp_get_term([{start,_}]=C, State, Dict) ->
    case etp_run(C, State, Dict) of
        {Ret, State_1} when is_binary(Ret) ->
            {list_to_atom(binary_to_list(Ret)), State_1};
        {Ret, State_1} when is_atom(Ret) ->
            {Ret, State_1}
    end.

etp_run_call("list", Args, State, Dict) ->
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    {lists:reverse(Args_2), State_3};
    
etp_run_call("tuple", Args, State, Dict) ->
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    {list_to_tuple(lists:reverse(Args_2)), State_3};

etp_run_call("bool", Args, State, Dict) ->
    {[Arg1 | _], State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    Arg1Bool = case Arg1 of
        Arg1 when is_integer(Arg1), (Arg1 > 0) orelse (Arg1 =:= -1) -> true;
        Arg1 when is_float(Arg1), Arg1 >= 1.0 -> true;
        0 -> false;
        0.0 -> false;
        Str when is_list(Str) ->
            case Str of
                [$t | _] -> true;
                [$T | _] -> true;
                _ -> false
            end;
        _ -> false
    end,
    {Arg1Bool, State_3};

etp_run_call("int", Args, State, Dict) ->
    {[Arg1 | _], State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    Arg1Int = case Arg1 of
        Arg1 when is_integer(Arg1) -> Arg1;
        Arg1 when is_float(Arg1) -> round(Arg1);
        true -> 1;
        false -> 0;
        Str when is_list(Str) ->
            case string:to_float(Str) of
                {Num_F, _} when is_float(Num_F) -> round(Num_F);
                _ ->
                    case string:to_integer(Str) of
                        {Num_I, _} when is_integer(Num_I) -> Num_I;
                        _ -> 0
                    end
            end;
        _ -> 0
    end,
    {Arg1Int, State_3};
    
etp_run_call("float", Args, State, Dict) ->
    {[Arg1 | _], State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args),
    Arg1Float = case Arg1 of
        Arg1 when is_integer(Arg1) -> float(Arg1);
        Arg1 when is_float(Arg1) -> Arg1;
        true -> 1.0;
        false -> 0.0;
        Str when is_list(Str) ->
            case string:to_float(Str) of
                {Num_F, _} when is_float(Num_F) -> Num_F;
                _ ->
                    case string:to_integer(Str) of
                        {Num_I, _} when is_integer(Num_I) -> float(Num_I);
                        _ -> 0.0
                    end
            end;
        _ -> 0.0
    end,
    {Arg1Float, State_3};
    
etp_run_call("ok_test", [Arg1, Arg2 | _], State, Dict) ->
    {Ret_1, State_1} = etp_run(Arg1, State, Dict),
    case Ret_1 of
        {ok, Ret_1_OkValue} ->
            {Ret_1_OkValue, State_1};
        _ ->
            etp_run(Arg2, State_1, Dict)
    end;
etp_run_call("ok_test", [Arg1], State, Dict) ->
    {Ret_1, State_1} = etp_run(Arg1, State, Dict),
    case Ret_1 of
        {ok, Ret_1_OkValue} ->
            {Ret_1_OkValue, State_1};
        _ ->
            {error, State_1}
    end;
    
etp_run_call("value_test", [ArgAtom, Arg1, Arg2 | _], State, Dict) ->
    {AtomTest_2, _} = etp_run(ArgAtom, State, Dict),
    {Ret_1, State_1} = etp_run(Arg1, State, Dict),
    case Ret_1 of
        {AtomTest_1, Ret_1_OkValue} when AtomTest_1 =:= AtomTest_2 ->
            {Ret_1_OkValue, State_1};
        _ ->
            etp_run(Arg2, State_1, Dict)
    end;
etp_run_call("value_test", [ArgAtom, Arg1], State, Dict) ->
    {AtomTest_2, _} = etp_run(ArgAtom, State, Dict),
    {Ret_1, State_1} = etp_run(Arg1, State, Dict),
    case Ret_1 of
        {AtomTest_1, Ret_1_OkValue} when AtomTest_1 =:= AtomTest_2 ->
            {Ret_1_OkValue, State_1};
        _ ->
            {error, State_1}
    end;
    
etp_run_call("if", [ArgTest, ArgThen, ArgElse | _], State, Dict) ->
    {AtomTest_2, State_1} = etp_run(ArgTest, State, Dict),
    case AtomTest_2 of
        true ->
            etp_run(ArgThen, State_1, Dict);
        _ ->
            etp_run(ArgElse, State_1, Dict)
    end;
etp_run_call("if", [ArgTest, ArgThen], State, Dict) ->
    {AtomTest_2, State_1} = etp_run(ArgTest, State, Dict),
    case AtomTest_2 of
        true ->
            etp_run(ArgThen, State_1, Dict);
        _ ->
            {false, State_1}
    end;
    
    
    
    
etp_run_call("void", Args, State, Dict) ->
    {Ret_2, State_3} = lists:foldl(fun(A, {_, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {Ret_1, State_2}
    end, {[], State}, Args),
    {Ret_2, State_3};
    
etp_run_call("map", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        case orddict:find(0, State_1#crun_state.temp_vars) of
            {ok, Save_0} -> Save_0; _ -> Save_0 = none
        end,
        State_1_1 = etp_store_temp(0, A, State_1),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, State_2),
        {[Ret_1|Args_1], State_2_1}
    end, {[], State}, List_1),
    {Args_2, State_3};
    
etp_run_call("filter", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, State_1}) ->
        case orddict:find(0, State_1#crun_state.temp_vars) of
            {ok, Save_0} -> Save_0; _ -> Save_0 = none
        end,
        State_1_1 = etp_store_temp(0, A, State_1),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, State_2),
        {[Ret_1|Args_1], State_2_1}
    end, {[], State}, List_1),
    {Args_2, State_3};
    
etp_run_call("foldl", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Ret_2, State_3} = lists:foldl(fun(A, {Acc, State_1}) ->
        case orddict:find(0, State_1#crun_state.temp_vars) of
            {ok, Save_0} -> Save_0; _ -> Save_0 = none
        end,
        case orddict:find(1, State_1#crun_state.temp_vars) of
            {ok, Save_1} -> Save_0; _ -> Save_1 = none
        end,
        State_1_1 = etp_store_temp(0, A, etp_store_temp(1, Acc, State_1)),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, etp_store_temp(1, Save_1, State_2)),
        {Ret_1, State_2_1}
    end, {none, State}, List_1),
    {Ret_2, State_3};
    
etp_run_call("foldr", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Ret_2, State_3} = lists:foldr(fun(A, {Acc, State_1}) ->
        case orddict:find(0, State_1#crun_state.temp_vars) of
            {ok, Save_0} -> Save_0; _ -> Save_0 = none
        end,
        case orddict:find(1, State_1#crun_state.temp_vars) of
            {ok, Save_1} -> Save_0; _ -> Save_1 = none
        end,
        State_1_1 = etp_store_temp(0, A, etp_store_temp(1, Acc, State_1)),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, etp_store_temp(1, Save_1, State_2)),
        {Ret_1, State_2_1}
    end, {none, State}, List_1),
    {Ret_2, State_3}.

etp_store_temp(K, V, State) ->
    State#crun_state{
        temp_vars = orddict:store(K, V,
            State#crun_state.temp_vars)
    }.

%% Easier access to some record fields than by tuple index
tuple_n_from_field(Tuple, FieldName) when is_tuple(Tuple) ->
    TT = element(1, Tuple),
    tuple_n_from_f(TT, FieldName).
tuple_n_from_f(st, F) ->
    tuple_n_from_f(st, F, 1, [
        "shapes",
        "selmode",
        "sh",
        "sel",
        "ssels",
        "temp_sel",
        "mat",
        "pal",
        "file",
        "saved",
        "onext",
        "bb",
        "edge_loop",
        "views",
        "pst",
        "repeatable",
        "ask_args",
        "drag_args",
        "def",
        "last_cmd",
        "undo",
        "next_is_undo",
        "undone"
    ]);
tuple_n_from_f(we, F) ->
    tuple_n_from_f(we, F, 1, [
        "id",
        "perm",
        "name",
        "es",
        "lv",
        "rv",
        "fs",
        "he",
        "vc",
        "vp",
        "pst",
        "mat",
        "next_id",
        "mirror",
        "light",
        "holes",
        "temp"
    ]);
tuple_n_from_f(edge, F) ->
    tuple_n_from_f(edge, F, 1, [
        "vs",
        "ve",
        "lf",
        "rf",
        "ltpr",
        "ltsu",
        "rtpr",
        "rtsu"
    ]);
tuple_n_from_f(view, F) ->
    tuple_n_from_f(view, F, 1, [
        "origin",
        "distance",
        "azimuth",
        "elevation",
        "pan_x",
        "pan_y",
        "along_axis",
        "fov",
        "hither",
        "yon"
    ]);
tuple_n_from_f(dlo, F) ->
    tuple_n_from_f(dlo, F, 1, [
        "work",
        "smooth",
        "edges",
        "vs",
        "hard",
        "sel",
        "orig_sel",
        "normals",
        "vab",
        "tri_map",
        "hilite",
        "mirror",
        "ns",
        "plugins",
        "proxy",
        "proxy_data",
        "src_we",
        "src_sel",
        "split",
        "drag",
        "transparent",
        "open",
        "needed"
    ]);
tuple_n_from_f(vab, F) ->
    tuple_n_from_f(vab, F, 1, [
        "id",
        "data",
        "face_vs",
        "face_fn",
        "face_sn",
        "face_uv",
        "face_ts",
        "face_vc",
        "face_es",
        "face_map",
        "mat_map"
    ]);
tuple_n_from_f(e3d_transf, F) ->
    tuple_n_from_f(e3d_transf, F, 1, [
        "mat",
        "inv"
    ]);
tuple_n_from_f(ray, F) ->
    tuple_n_from_f(ray, F, 1, [
        "o",
        "d",
        "n",
        "f",
        "bfc"
    ]);
tuple_n_from_f(e3d_face, F) ->
    tuple_n_from_f(e3d_face, F, 1, [
        "vs",
        "vc",
        "tx",
        "ns",
        "mat",
        "sg",
        "vis"
    ]);
tuple_n_from_f(e3d_mesh, F) ->
    tuple_n_from_f(e3d_mesh, F, 1, [
        "type",
        "vs",
        "vc",
        "tx",
        "ns",
        "fs",
        "he",
        "matrix"
    ]);
tuple_n_from_f(e3d_object, F) ->
    tuple_n_from_f(e3d_object, F, 1, [
        "name",
        "obj",
        "mat",
        "attr"
    ]);
tuple_n_from_f(e3d_file, F) ->
    tuple_n_from_f(e3d_file, F, 1, [
        "objs",
        "mat",
        "creator",
        "dir"
    ]);
tuple_n_from_f(e3d_image, F) ->
    tuple_n_from_f(e3d_image, F, 1, [
        "type",
        "bytes_pp",
        "alignment",
        "order",
        "width",
        "height",
        "image",
        "filename",
        "name",
        "extra"
    ]);
tuple_n_from_f(keyboard, F) ->
    tuple_n_from_f(keyboard, F, 1, [
        "which",
        "state",
        "scancode",
        "sym",
        "mod",
        "unicode"
    ]);
tuple_n_from_f(mousemotion, F) ->
    tuple_n_from_f(mousemotion, F, 1, [
        "which",
        "state",
        "mod",
        "x",
        "y",
        "xrel",
        "yrel"
    ]);
tuple_n_from_f(mousebutton, F) ->
    tuple_n_from_f(mousebutton, F, 1, [
        "which",
        "button",
        "state",
        "mod",
        "x",
        "y"
    ]);
tuple_n_from_f(mousewheel, F) ->
    tuple_n_from_f(mousewheel, F, 1, [
        "which",
        "dir",
        "wheel",
        "mod",
        "x",
        "y"
    ]);
tuple_n_from_f(io, F) ->
    tuple_n_from_f(io, F, 1, [
        "grab_stack",
        "key_up"
    ]);
tuple_n_from_f(_, _) -> error.

tuple_n_from_f(_, _, _, []) -> error;
tuple_n_from_f(_, F, I, [F | _]) -> {ok, I};
tuple_n_from_f(TN, F, I, [_ | R]) ->
    tuple_n_from_f(TN, F, I+1, R).
    

liststr_to_binstring(A) ->
    binstr(A).
    
binstr(Str) when is_list(Str) ->
    unicode:characters_to_nfc_binary(Str);
binstr(Str) when is_binary(Str) ->
    Str.
    
unbinstr(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin, utf8).


%% Starting parses need to begin with something
etp_start_parse(A) ->
    {Thing, R} = etp_next_thing(A),
    etp_parse(R, [{start, Thing}]).

etp_parse([], Cont) ->
    {lists:reverse(Cont), []};
etp_parse([close_b | R], Cont) ->
    {lists:reverse(Cont), R};
etp_parse([close_c | R], Cont) ->
    {lists:reverse(Cont), R};
etp_parse([close_p | R], Cont) ->
    {lists:reverse(Cont), R};
etp_parse([cma | R], Cont) ->
    {cma, lists:reverse(Cont), R};

etp_parse([open_c | R], Cont) ->
    {Cont_1, R_1} = etp_start_parse(R),
    etp_parse(R_1, [{tuple_nth, Cont_1} | Cont]);
etp_parse([open_b | R], Cont) ->
    {Cont_1, R_1} = etp_start_parse(R),
    etp_parse(R_1, [{list_nth, Cont_1} | Cont]);
etp_parse([slash | R], Cont) ->
    {Cont_1, R_1} = etp_next_thing(R),
    etp_parse(R_1, [{orddict_f, Cont_1} | Cont]);
etp_parse([dot, {word, Word} | R], Cont) ->
    %% Find record field name
    etp_parse(R, [{rec_f, Word} | Cont]);
 
 
 
etp_parse([store_to, {integer, N} | R], Cont) ->
    etp_parse(R, [{store_to, N} | Cont]);

etp_parse([store_to, {atom, A} | R], Cont) ->
    etp_parse(R, [{store_to, binstr(A)} | Cont]);

etp_parse([store_to, {string, S} | R], Cont) ->
    etp_parse(R, [{store_to, binstr(S)} | Cont]).
    
etp_list_parse(A) ->
    etp_list_parse(A, []).
etp_list_parse(A, O) ->
    case etp_start_parse(A) of
        {cma, Cont, R} ->
            etp_list_parse(R, [Cont | O]);
        {Cont, R} ->
            {lists:reverse([Cont | O]), R}
    end.
    
etp_next_thing([{word, Mod}, col, {word, W}, open_p | R]) ->
    {List, R_1} = etp_list_parse(R),
    {{call, Mod, W, List}, R_1};
etp_next_thing([{word, W}, open_p | R]) ->
    {List, R_1} = etp_list_parse(R),
    {{call, W, List}, R_1};
etp_next_thing([{float, W} | R]) ->
    {{float, W}, R};
etp_next_thing([{integer, W} | R]) ->
    {{integer, W}, R};
etp_next_thing([{atom, W} | R]) ->
    {{atom, W}, R};
etp_next_thing([{string, W} | R]) ->
    {{string, W}, R};
etp_next_thing([{word, StartWord} | R]) ->
    {{word, StartWord}, R};
etp_next_thing([get_from, {integer, N} | R]) ->
    {{get_from, N}, R};
etp_next_thing([get_from, {atom, A} | R]) ->
    {{get_from, binstr(A)}, R};
etp_next_thing([get_from, {string, N} | R]) ->
    {{get_from, binstr(N)}, R}.

etp_tok([], [], O) ->
    lists:reverse(O);
etp_tok([], WO, O) ->
    etp_tok([], [], [etp_word_or_number(WO) | O]);
etp_tok([$. | Rest], [Alph | _] = WO, O)
  when Alph >= $a andalso Alph =< $z ->
    %% Tuple record find
    etp_tok(Rest, [], [dot, etp_word_or_number(WO) | O]);
etp_tok([$. | Rest], [], O) ->
    %% Tuple record find
    etp_tok(Rest, [], [dot | O]);
etp_tok([Digit | Rest], [], [dot | O])
  when Digit >= $0 andalso Digit =< $9 ->
    %% Revert dot from tuple record to number
    etp_tok(Rest, [Digit,$.], O);
etp_tok([$/ | Rest], [], O) ->
    %% Assoc list find
    etp_tok(Rest, [], [slash | O]);
etp_tok([$( | Rest], [], O) ->
    %% Parenthesis
    etp_tok(Rest, [], [open_p | O]);
etp_tok([$) | Rest], [], O) ->
    %% Closing parenthesis
    etp_tok(Rest, [], [close_p | O]);
etp_tok([${ | Rest], [], O) ->
    %% Tuple nth
    etp_tok(Rest, [], [open_c | O]);
etp_tok([$} | Rest], [], O) ->
    %% Closing tuple nth
    etp_tok(Rest, [], [close_c | O]);
etp_tok([$[ | Rest], [], O) ->
    %% List nth
    etp_tok(Rest, [], [open_b | O]);
etp_tok([$] | Rest], [], O) ->
    %% Closing list nth
    etp_tok(Rest, [], [close_b | O]);
%etp_tok([$$ | Rest], [], O) ->
    %% Error
%    {error, syntax};
etp_tok([$>, $$ | Rest], [], O) ->
    %% Store value
    %% Get number, atom or string
    etp_tok(Rest, [], [store_to | O]);
etp_tok([$<, $$ | Rest], [], O) ->
    %% Fetch value
    %% Get number, atom or string
    etp_tok(Rest, [], [get_from | O]);
etp_tok([$' | Rest], [], O) ->
    %% Atom
    %% Get until end of '
    {Atom, Rest_1} = etp_read_atom_in_q(Rest),
    etp_tok(Rest_1, [], [{atom, Atom} | O]);
    
etp_tok([34 | Rest], [], O) ->
    %% String
    %% Get until end of double quote
    {Str, Rest_1} = etp_read_string_in_dq(Rest),
    etp_tok(Rest_1, [], [{string, Str} | O]);
    
etp_tok([$: | Rest], [], O) ->
    %% Colon
    etp_tok(Rest, [], [col | O]);
etp_tok([$, | Rest], [], O) ->
    %% Separator
    etp_tok(Rest, [], [cma | O]);
    
etp_tok([X | _]=Str, WO, O)
  when length(WO) > 0,
        (X =:= $/ orelse
         X =:= ${ orelse X =:= $} orelse
         X =:= $[ orelse X =:= $] orelse
         X =:= $( orelse X =:= $) orelse
         X =:= $> orelse X =:= $< orelse
         X =:= $, orelse X =:= $' orelse X =:= 34 orelse X =:= $: ) ->
    etp_tok(Str, [], [etp_word_or_number(WO) | O]);
    
etp_tok([A | Rest], WO, O) ->
    etp_tok(Rest, [A | WO], O).
    
etp_word_or_number(WO) ->
    A = lists:reverse(WO),
    case string:to_float(A) of
        {Num_F, _} when is_float(Num_F) -> {float, Num_F};
        _ ->
            case string:to_integer(A) of
                {Num_I, _} when is_integer(Num_I) -> {integer, Num_I};
                _ -> {word, A}
            end
    end.

%% Read the contents of a string
%%
etp_read_string_in_dq(A) -> etp_read_string_in_dq(A, []).
etp_read_string_in_dq([], AL) -> {binstr(lists:reverse(AL)), []};
etp_read_string_in_dq([BS, EscChar | Rest], AL) when BS =:= 92 ->
    etp_read_string_in_dq(Rest, [EscChar|AL]);
etp_read_string_in_dq([DQ | Rest], AL) when DQ =:= 34 ->
    {binstr(lists:reverse(AL)), Rest};
etp_read_string_in_dq([Char | Rest], AL) ->
    etp_read_string_in_dq(Rest, [Char|AL]).

%% Read the contents of a atom
%%
etp_read_atom_in_q(A) -> etp_read_atom_in_q(A, []).
etp_read_atom_in_q([], AL) -> {binstr(lists:reverse(AL)), []};
etp_read_atom_in_q([BS, EscChar | Rest], AL) when BS =:= 92 ->
    etp_read_atom_in_q(Rest, [EscChar|AL]);
etp_read_atom_in_q([AQ | Rest], AL) when AQ =:= $' ->
    {binstr(lists:reverse(AL)), Rest};
etp_read_atom_in_q([Char | Rest], AL) ->
    etp_read_atom_in_q(Rest, [Char|AL]).




-ifdef(TEST).
test_write_scm() ->
    binstr(write_scm([{atom, <<"test">>}, {string, <<"string">>}, false, [1,2,3,4], [5,6,7,8]])).
test_scm_parse() ->
    scm_parse(<<"(test '(0 1 2 3) #(2 3 4) '(\"Test\" 2 #f))">>).
test_wscr_content() ->
    read_wscr_content(
        <<"type \"py\"\n"
          "name \"My Example Script\"\n",
          "params {\n"
          "  param \"Top\" \"top\"\n"
          "  param \"Bottom\" \"bottom\"\n"
          "  param \"Sides\" \"sides\"\n"
          "}\n">>, []).
t() ->
    Settings = [],
    run_script_once("py", "C:\\Stuff\\___w3d\\plugins\\primitives\\src\\w3d_scripts\\Script3.py", [7, 2.0, 0.1], [], Settings, keep).
    % run_script_once("scm", "C:\\Stuff\\___w3d\\plugins\\primitives\\src\\w3d_scripts\\Script1.scm", [7, 2.0, 0.1], []).

test() ->
    Dict = [{"st", {st, 1,2,3,4}}],
    L =
    [["ifdef", "st{3}/plugins/wpc_example{3}", []],
     ["if", "st{2}/plugins/wpc_example{3}", "eq", "0", []],
     ["if", "st{1}/plugins/wpc_example{3}", "gt", "0", []],
     ["if", "st{2}/plugins/wpc_example{3}", "lt", "0", []],
     ["script_param", "paramname", "st.shapes"],
     ["script_param", "paramname2", "tuple(1,2,3){0}"],
     ["script_param", "paramname3", "lists:nth(1,list(1,2,3,4))>$'test1'"],
     ["script_param", "paramname4", "<$'test1'"],
     ["import_param", "key", "\"val\""],
     ["export_param", "tesselation", "'triangulation'"],
     ["save_pref", "k", "'v'"],
     ["load_pref", "k", "v", "'v'"]],
    crun_list(L, #crun_state{}, Dict).
-endif.

