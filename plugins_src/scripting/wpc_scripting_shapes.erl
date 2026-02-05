%%
%%  Scripting  (Scheme and Python)
%%
%%  Use New shape, importers, exporters and command scripts in Wings3D
%%  using scripting languages like Scheme and Python.
%%
%%  Requires Gauche for Scheme runtime
%%  Requires Python 3.6 or later
%%
%%  Copyright 2023-2025 Edward Blake
%%
%%  Thanks micheus for help with Script Preference Dialog
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_scripting_shapes).
-export([init/0,menu/2,command/2,installing_archive/2]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-record(command_rec, {
    wscrcont,
    scrfile :: file:filename_all() | none,
    scrtype :: string() | none,
    extrafileinputs=[]
}).

-include("resource/wpc_scripting_shapes_btn_refresh.hrl").

-type wings_op_mode() :: atom().

-ifdef(DEBUG_1).
-define(DEBUG_FMT(A,B), io:format(A,B)).
-else.
-define(DEBUG_FMT(A,B), none).
-endif.

%% For the queries mini language
-record(crun_state, {
    p = none, %% Current value when in etp_run
    sett_vars = #{}, %% Variables such as for script_params, import_params, export_params
    temp_vars = #{}  %% Temporary variable storage, 'we' is stored here for command scripts
}).

-define(PATH_INIT, "_init").
init_dir() ->
    atom_to_list(?MODULE) ++ ?PATH_INIT.

init() ->
    scripting_engines:init(init_dir()),
    set_pref_defaults(),
    catch ets:new(script_eng, [public,ordered_set,named_table]),

    %% Directories in the user_data folder for plugin_scripts and script_folders.
    UserDataDir = filename:join(wings_u:basedir(user_data), wpa:version()),
    PluginScriptsDir = filename:join(UserDataDir, "plugin_scripts"),
    ScriptFoldersDir = filename:join(UserDataDir, "script_folders"),
    
    %% Note: to add another directory to find plugin_scripts and script_folders
    %%       (for example in the program's folder), add the extra directory to 
    %%       the list before PluginScriptsDir and ScriptFoldersDir.
    {Plugins,PluginsMenu}=get_pluginscripts([PluginScriptsDir]),
    {List,ListMenu}=get_scriptfolders([ScriptFoldersDir]),
    List1 = get_pluginscripts_1(PluginsMenu++ListMenu, Plugins++List),
    ets:insert(script_eng, {plugin_scripts,List1}),
    true.

menu_enabled(Where,Menu,Fun) ->
    case wpa:pref_get(?MODULE, setting_enable, true) of
        true -> Fun(menu_1(Where,Menu));
        _    -> Menu
    end.
menu_enabled(Atom,Where,Menu,Fun)
  when is_atom(Atom) ->
    case wpa:pref_get(?MODULE, setting_enable, true) of
        true ->
            Menu1 = menu_1(Where,Menu),
            case wpa:pref_get(?MODULE, Atom, true) of
                true -> Fun(Menu1);
                _    -> Menu1
            end;
        _ ->
            Menu
    end.

menu({shape}=Where, Menu) ->
    menu_enabled(Where, Menu,
        fun(Menu1) -> Menu1 ++ [separator|from_script_menu(shape)] end);
menu({file,import}=Where, Menu) ->
    menu_enabled(setting_enable_import, Where, Menu,
        fun(Menu1) -> Menu1 ++ from_script_menu(import) end);
menu({file,Op}=Where, Menu)
  when Op =:= export;
       Op =:= export_selected ->
    menu_enabled(setting_enable_export, Where, Menu,
        fun(Menu1) -> Menu1 ++ from_script_menu(export) end);
menu({Mode}=Where,Menu)
  when Mode =:= vertex;
       Mode =:= edge;
       Mode =:= face;
       Mode =:= body ->
    menu_enabled(setting_enable_commands, Where, Menu,
        fun(Menu1) -> Menu1 ++ from_script_menu(Mode) end);
menu({edit,plugin_preferences}, Menu) ->
    Menu ++ [{?__(1,"Scripts Preference"), shapes_from_scripts_preference}];
menu({help}=Where, Menu) ->
    menu_enabled(Where, Menu, fun(Menu1) -> help_script_menu(Menu1) end);
menu(Where,Menu) ->
    %% Menu items are added from plugin scripts in other submenus.
    menu_enabled(Where, Menu, fun(Menu1) -> Menu1 end).


%% Append menus generated for plugin scripts and script folders.
menu_1(Where,Menu) ->
    [{plugin_scripts, L}] = ets:lookup(script_eng, plugin_scripts),
    pluginscripts_make_menu(Where, Menu, L).


from_script_menu(shape) ->
    [{?__(1,"Shape from Script"),
         {shape_from_script, mouse_choice()}}];
from_script_menu(import) ->
    [{?__(2,"Script-based Importers..."),
         {import_export_from_script, select}}];
from_script_menu(export) ->
    [{?__(3,"Script-based Exporters..."),
         {import_export_from_script, select}}];
from_script_menu(_Mode) ->
    [{?__(4,"Script-based Commands..."),
         {command_from_script, select}}].

help_script_menu(Menu) ->
    lists:reverse(help_script_menu(2, lists:reverse(Menu))).

help_script_menu(0, Menu) ->
    [{?__(1,"Using Scripts"), {help_info_script, [
        {?__(2,"Scripting Information"),script_basics,
              ?__(2,"Scripting Information")},
        {?__(3,"Scripting Reference"),scripting_reference,
              ?__(3,"Scripting Reference")}
       ]}}] ++ Menu;
help_script_menu(_, []) -> []; %% This shouldn't happen.
help_script_menu(N, [Item | Menu])
  when Item =/= separator, N > 0 ->
    [Item | help_script_menu(N, Menu)];
help_script_menu(N, [separator | Menu])
  when N > 0 ->
    [separator | help_script_menu(N-1, Menu)].



mouse_choice() ->
    fun (help,_) -> {?__(1,"From Script"), [], []};
        (1,_) -> {shape, {shape_from_script, select}};
        (2,_) -> ignore;
        (3,_) -> {shape, {shape_from_script, select}};
        (_,_) -> ignore
    end.


command({file, {import, {import_export_from_script, select}}}, fetch_props) ->
    %% TODO: Wings sends this to find out which importer can handle a given file extension.
    {[], fun (_) -> keep end};
command({file,{Op,{import_export_from_script, select}}}, St)
  when Op =:= import;
       Op =:= export;
       Op =:= export_selected ->
    case Op of
        import -> ScrTyp = import;
        _      -> ScrTyp = export
    end,
    cmd_select_from_list(setting_paths_import_export, ScrTyp,
        from_script_fun(Op, St));
command({file,{Op,{import_export_from_script, {#command_rec{}=CommandRec, Params}}}}, St)
  when Op =:= import;
       Op =:= export;
       Op =:= export_selected ->
    Fun=from_script_fun(Op, St),
    Fun(Params, CommandRec);


command({shape, {shape_from_script, select}}, St) ->
    cmd_select_from_list(setting_paths_shapes, false,
        from_script_fun(shape, St));
command({shape, {shape_from_script, {#command_rec{}=CommandRec, Params}}}, St) ->
    Fun=from_script_fun(shape, St),
    Fun(Params, CommandRec);


command({Op, {command_from_script, select}}, St)
  when Op =:= vertex; Op =:= edge; Op =:= face; Op =:= body ->
    cmd_select_from_list(setting_paths_commands, Op,
        from_script_fun(Op, St));
command({Op, {command_from_script, {#command_rec{}=CommandRec, Params}}}, St) ->
    Fun=from_script_fun(Op, St),
    Fun(Params, CommandRec);

command({edit, {plugin_preferences, shapes_from_scripts_preference}}, St) ->
    dlg_script_preference(St);

command({help, {help_info_script, script_basics}}, _St) ->
    {Title, Text} = help_information(script_basics),
    wings_dialog:info(Title, Text, []);
command({help, {help_info_script, scripting_reference}}, _St) ->
    wings_dialog:manual(filename:join(
        init_dir(),
        "scripting-reference-:LANG:.manual")),
    keep;

%% Command is searched among plugin scripts and returns 'next'
%% if nothing is found.
command(Cmd, St) ->
    [{plugin_scripts, L}] = ets:lookup(script_eng, plugin_scripts),
    pluginscripts_command(Cmd,St,L).



installing_archive(plugin_script, _List) ->
    {"plugin_scripts", ?__(1,"plugin script"),
        fun (_Dest) -> ok end};
installing_archive(_, _List) ->
    next.


%%%
%%%


from_script_fun(Op, St)
  when Op =:= import;
       Op =:= export;
       Op =:= export_selected ->
    fun (Params, CR) ->
        import_export_from_script(Op, Params, CR, St)
    end;
from_script_fun(Op, St)
  when Op =:= shape ->
    fun (Params, CR) ->
        make_shape_from_script(Params, CR, St)
    end;
from_script_fun(Op, St) ->
    fun(Params, CR) ->
        command_from_script(Op, Params, CR, St)
    end.


cmd_select_from_list(DirsSetting, ScrOp, F) ->
    ScriptDirs = script_dirs(DirsSetting),
    case scripts_menu_select_plugin(ScriptDirs, ScrOp) of
        {load_script, Script} ->
            select_script(Script, F);
        _ ->
            keep
    end.

select_script({ScriptFile, ScriptType, WSCRFile}, F)
  when is_list(ScriptFile) ->
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
select_script(false, _F) ->
    keep.

script_info(WSCRFile, shape) ->
    script_info(WSCRFile, false);
script_info(WSCRFile, export_selected) ->
    script_info(WSCRFile, export_selected);
script_info(WSCRFile, Op) ->
    case read_script_info_file(WSCRFile, Op) of
        {ok, {ScriptFile, ScriptType, _, _}} when is_list(ScriptFile), is_list(ScriptType) ->
            {ScriptFile, ScriptType, WSCRFile};
        _ ->
            false
    end.

%% Gets a list of callable functions, which also provides the dynamic
%% type information for each argument, and the return types.
%%
get_callable() ->
    InitScriptsDir = filename:absname(code:where_is_file(init_dir())),
    {ok,[L]} = file:consult(filename:join(InitScriptsDir,"callable.conf")),
    maps:from_list(L).


%% Very basic information for script users.
help_information(script_basics) ->
    {
        ?__(1,"Scripting Information"),
        [
            ?__(2,"Scripting Information\n\n"
                  "Scripts are small programs that can be used to create new "
                  "shapes, importers, exporters and commands. Before "
                  "scripts can be used, the scripting interpreters needs to be "
                  "installed and its path set in 'Scripting Preference'.\n\n"
                  "The two scripting languages that are available are Python "
                  "and Scheme.\n\n"
                  "Each script needs the script itself (.py or .scm) and "
                  "a .wscr file with the same name before the extension for "
                  "the script to work.")
        ]
    }.

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
                  erlang:erase(scripting_callable),
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

askdialog_e({Text, Number})
  when is_list(Text) ->
    {hframe,[{label,Text},{text,Number}]};
askdialog_e({Text, Number, C})
  when is_list(Text) ->
    {hframe,[{label,Text},{text,Number,C}]};
askdialog_e({menu,{menu,List},Text,Default}) ->
    {hframe,[{label,Text},{menu,List,askdialog_e_mval(Default,List),[]}]};
askdialog_e({menu,{menu,List},Text,Default,C}) ->
    {hframe,[{label,Text},{menu,List,askdialog_e_mval(Default,List),C}]};
askdialog_e({menu,{vradio,List},Text,Default}) ->
    {hframe,[{label,Text},{vradio,List,askdialog_e_mval(Default,List)}]};
askdialog_e({menu,{vradio,List},Text,Default,C}) ->
    {hframe,[{label,Text},{vradio,List,askdialog_e_mval(Default,List),C}]};
askdialog_e({menu,{hradio,List},Text,Default}) ->
    {hframe,[{label,Text},{hradio,List,askdialog_e_mval(Default,List)}]};
askdialog_e({menu,{hradio,List},Text,Default,C}) ->
    {hframe,[{label,Text},{hradio,List,askdialog_e_mval(Default,List),C}]};
askdialog_e({checkbox,_,Text,Default}) ->
    {Text, Default};
askdialog_e({checkbox,_,Text,Default,C}) ->
    {Text, Default, C};
askdialog_e({browse,_,Text,Default}) ->
    {hframe,[{label,Text},{button,{text,askdialog_e_bval(Default),[]}}]};
askdialog_e({browse,_,Text,Default,C}) ->
    {hframe,[{label,Text},{button,{text,askdialog_e_bval(Default),C}}]};
askdialog_e({adv_params,_,_}=Tuple) ->
    advp_merge_vals(Tuple).

%% Adjust the default value if it doesn't appear in the
%% list.
%%
askdialog_e_mval(Default,List) ->
    Atoms = [V || {_,V} <- List],
    case lists:member(Default, Atoms) of
        true ->
            Default;
        _ ->
            [V1|_] = Atoms,
            V1
    end.

%% Adjust the browse default value if it isn't a string.
%%
askdialog_e_bval(Str)
  when is_binary(Str) ->
    binary_to_list(Str);
askdialog_e_bval(Str)
  when is_list(Str) ->
    binary_to_list(iolist_to_binary(Str));
askdialog_e_bval(_) ->
    "".

%% Merge list of values with adv_param dialog term.
%%
advp_merge_vals({adv_params,Tuple,Vals}) ->
    advp_m_vals(Tuple,maps:from_list(Vals)).
advp_m_vals(List,Vals)
  when is_list(List)->
    [ advp_m_vals(A,Vals) || A <- List];
advp_m_vals(Tuple,Vals)
  when is_tuple(Tuple)->
    List = tuple_to_list(Tuple),
    list_to_tuple([ advp_m_vals(A,Vals) || A <- List]);
advp_m_vals(Atom,Vals)
  when is_atom(Atom) ->
    case maps:find(Atom,Vals) of
        {ok, Val} -> Val;
        _ -> Atom
    end;
advp_m_vals(Anything,_Vals) ->
    Anything.


%% Start and run a script and get the return value
%%
run_script_once(DefaultReturn, Vars) when is_map(Vars) ->
    #{script_type:=ScriptType,
      script_filename:=ScriptFileName,
      script_params:=ScriptParams,
      more_params:=MoreParams,
      settings:=Settings}=Vars,
    case erlang:get(scripting_shape_fun) of
        FP when is_function(FP) ->
            Ret = FP({run, ScriptParams, MoreParams, DefaultReturn, Vars}),
            case erlang:get(scripting_shape_fun_close) of
                true ->
                    FP(close),
                    erlang:erase(scripting_shape_fun_close),
                    erlang:erase(scripting_shape_fun),
                    erlang:erase(scripting_callable);
                _ ->
                    ok
            end,
            Ret;
        undefined ->
            case run_script_w_preview(ScriptType, ScriptFileName, Settings) of
                {error, Err} -> {error, Err};
                {ok, F} when is_function(F) ->
                    Ret = F({run, ScriptParams, MoreParams, DefaultReturn, Vars}),
                    F(close),
                    Ret
            end
    end.


%% Start a script but return a function to run with parameters
%%
run_script_w_preview(ScriptType, ScriptFileName, Settings)
  when is_list(ScriptFileName) ->
    {ok, StrList} = load_lang_file(ScriptFileName, current_lang_code()),
    erlang:put(scripting_callable, get_callable()),
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
        {[[ok|_]|_], _} ->
            PID ! next,
            {ok, run_script_w_preview_1_fun(PID)};
        Returned ->
            err(io_lib:format(?__(2,"Returned: ") ++ "run_script_1=~p", [Returned])),
            {error, {unexpected, Returned}}
    end.
run_script_w_preview_1_fun(PID) ->
    fun
        ({run, ScriptParams, MoreParams, DefaultReturn, TempVars}) ->
            run_script_1(ScriptParams, MoreParams, DefaultReturn, PID, TempVars);
        (close) ->
            PID ! {close, self()},
            receive
                exited -> ok
            after 20 ->
                err(?__(1,"Did not recv 'exited'")),
                ok
            end
    end.


run_script_1(ScriptParams, MoreParams, DefaultReturn, PID, TempVars) ->
    ShowTupleDebug = wpa:pref_get(?MODULE, setting_show_tuple, false),
    PID ! {run_script, ScriptParams, MoreParams, self()},
    case run_script_getting_data_until_ret(DefaultReturn, TempVars) of
        {error, _} -> {error, ?__(2,"No results")};
        {keep, _} -> {ok, {keep, #{}}};
        {{[ReturnList], _}, RetTempVars_0} ->
            RetTempVars = RetTempVars_0#{
                script_type=>none,
                script_filename=>none,
                script_params=>none,
                more_params=>none,
                settings=>none},
            PID ! next,
            Tuplefied = run_script_tuplefy(ReturnList),
            if ShowTupleDebug =:= true ->
                    io:format(?__(1,"Tuple:") ++ "~p~n", [Tuplefied]);
                true -> ok
            end,
            {ok, {Tuplefied, RetTempVars}}
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

run_script_getting_data_until_ret(CurrentReturn, TempVars) ->
    run_script_getting_data_until_exit(CurrentReturn, TempVars).

run_script_getting_data_until_exit(CurrentReturn, TempVars) ->
    ShowTupleDebug = wpa:pref_get(?MODULE, setting_show_tuple, false),
    ScrSt = #crun_state{temp_vars=TempVars},
    {Ret, #crun_state{temp_vars=TempVars_1}=_ScrSt_1} =
        run_script_getting_data_until_exit(CurrentReturn, ScrSt, ShowTupleDebug),
    {Ret, TempVars_1}.
run_script_getting_data_until_exit(CurrentReturn, QueryState, ShowTupleDebug) ->
    receive 
        {reply, SendPID, {[[{atom, <<"%ok">>} | _] | _], _}}
          when is_pid(SendPID) ->
            ?DEBUG_FMT("got ok return~n",[]),
            {CurrentReturn, QueryState};
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
            {CurrentReturn, QueryState}
    end.

run_script_getting_data_special(QueryState, ShowTupleDebug, SendPID, Ret_1) ->
    run_script_getting_data_special_1(run_script_tuplefy(Ret_1),
        QueryState, ShowTupleDebug, SendPID).

%% Set a query variable
run_script_getting_data_special_1([['%setvar', VarName, VarValue]|_],
        QueryState, ShowTupleDebug, SendPID) ->
    {Ret,QueryState_1} = set_var(VarName, VarValue, QueryState),
    if ShowTupleDebug =:= true ->
            io:format("setvar: ~s = ~w~n", [VarName, VarValue]);
        true -> ok
    end,
    SendPID ! {reply_to_script, [Ret]},
    QueryState_1;

%% Get a query variable
run_script_getting_data_special_1([['%getvar', VarName]|_], 
        QueryState, ShowTupleDebug, SendPID) ->
    QueryState_1 = QueryState,
    Ret = get_var(VarName, QueryState),
    if ShowTupleDebug =:= true ->
            io:format("getvar: ~s~n", [VarName]);
        true -> ok
    end,
    SendPID ! {reply_to_script, [Ret]},
    QueryState_1;

%% Do a query.
run_script_getting_data_special_1([['%query', QueryStr]|_], 
        QueryState, ShowTupleDebug, SendPID)
  when is_list(QueryStr) ->
    {QueryRes, QueryState_1} = crun_pv({etp,QueryStr}, QueryState, []),
    if ShowTupleDebug =:= true ->
            io:format("query: ~s -> ~p~n", [QueryStr, QueryRes]);
        true -> ok
    end,
    SendPID ! {reply_to_script, [QueryRes]},
    QueryState_1;

%% Call a we function with the current #we{} and get a return value 
%% and change #we{}
run_script_getting_data_special_1([['%we!', [Mod,FunName|ApplyArgs]]|_], 
        QueryState, _ShowTupleDebug, SendPID)
  when is_list(ApplyArgs) ->
    {Ret,QueryState_1} = 
        change_we(
            Mod,FunName,ApplyArgs,
            QueryState,erlang:get(scripting_callable)),
    SendPID ! {reply_to_script, Ret},
    QueryState_1;

%% Call a we function with the current #we{} and get a return value
run_script_getting_data_special_1([['%we', [Mod,FunName|ApplyArgs]]|_], 
        QueryState, _ShowTupleDebug, SendPID)
  when is_list(ApplyArgs) ->
    QueryState_1 = QueryState,
    Ret = val_from_we(
        Mod,FunName,ApplyArgs,
        QueryState,erlang:get(scripting_callable)),
    SendPID ! {reply_to_script, [Ret]},
    QueryState_1;

%% Call a we function with a previous #we{} in the temporary stack 
%% and get a return value and change #we{}
run_script_getting_data_special_1([['%we-previous!', [StackIndex,Mod,FunName|ApplyArgs]]|_], 
        QueryState, _ShowTupleDebug, SendPID)
  when is_list(ApplyArgs) ->
    {Ret,QueryState_1} = 
        change_with_previous_we(
            StackIndex,Mod,FunName,ApplyArgs,
            QueryState,erlang:get(scripting_callable)),
    SendPID ! {reply_to_script, Ret},
    QueryState_1;

%% Call a we function with a previous #we{} in the temporary stack 
%% and get a return value
run_script_getting_data_special_1([['%we-previous', [StackIndex,Mod,FunName|ApplyArgs]]|_], 
        QueryState, _ShowTupleDebug, SendPID)
  when is_list(ApplyArgs) ->
    QueryState_1 = QueryState,
    Ret = val_from_previous_we(
        StackIndex,Mod,FunName,ApplyArgs,
        QueryState,erlang:get(scripting_callable)),
    SendPID ! {reply_to_script, [Ret]},
    QueryState_1;

%% Push a copy of the #we{} variable into the temporary stack
run_script_getting_data_special_1([['%push-we', _]|_], 
        #crun_state{temp_vars=TempVars}=QueryState, _ShowTupleDebug, SendPID) ->
    We1 = etp_get_temp_var(we, TempVars),
    WeStack = etp_get_temp_var(prev_we_stack, TempVars),
    QueryState_1 = etp_store_temp(prev_we_stack, [We1|WeStack], QueryState),
    SendPID ! {reply_to_script, [0]},
    QueryState_1;

%% Pop the most recent #we{} from the temporary stack
run_script_getting_data_special_1([['%pop-we', [How|_Args]]|_], 
        #crun_state{temp_vars=TempVars}=QueryState, _ShowTupleDebug, SendPID) ->
    case How of
        "discard" -> ok;
        _ -> ok
    end,
    [_|WeStack] = etp_get_temp_var(prev_we_stack, TempVars),
    QueryState_1 = etp_store_temp(prev_we_stack, WeStack, QueryState),
    SendPID ! {reply_to_script, [0]},
    QueryState_1;


%% An intermittent message sent to prevent the scripting plugin from
%% thinking the script has stalled and timing out.
run_script_getting_data_special_1([['%keepalive', _Number]|_], 
        QueryState, _ShowTupleDebug, _SendPID) ->
    QueryState_1 = QueryState,
    QueryState_1;

%% Send a progress bar message to the plugin to update the user
%% on what the script is doing.
run_script_getting_data_special_1([['%pbmessage', Percent, Str]|_], 
        QueryState, _ShowTupleDebug, _SendPID)
  when is_float(Percent), is_list(Str) ->
    wings_pb:update(Percent, Str),
    wings_pb:pause(),
    QueryState_1 = QueryState,
    QueryState_1;

%% Display a panel window with a text box that shows the results of 
%% processing to the user. For example, a script that analyzes the
%% model and displays statistics.
run_script_getting_data_special_1([['%resulttext', Text]|_], 
        QueryState, _ShowTupleDebug, _SendPID)
  when is_list(Text) ->
    info_dialog(Text),
    QueryState_1 = QueryState,
    QueryState_1;
run_script_getting_data_special_1([['%resulttext', Text, OptList]|_], 
        QueryState, _ShowTupleDebug, _SendPID)
  when is_list(Text), is_list(OptList) ->
    info_dialog(Text),
    QueryState_1 = QueryState,
    QueryState_1;

run_script_getting_data_special_1(_, QueryState, _ShowTupleDebug, SendPID) ->
    QueryState_1 = QueryState,
    SendPID ! {reply_to_script, [error]},
    QueryState_1.

    


%% Some variables such as 'we' are special and should not be set by
%% the script to any kind of data. Changes to 'we' should be done with
%% the '%we!' command.
set_var(we, _, QueryState) ->
    {read_only, QueryState};
set_var(VarName, VarValue, QueryState)
  when is_atom(VarName) ->
    {ok, etp_store_temp(VarName, VarValue, QueryState)};
set_var(VarName, VarValue, QueryState)
  when is_list(VarName) ->
    {ok, etp_store_temp(binstr(VarName), VarValue, QueryState)}.

get_var(VarName, #crun_state{temp_vars=TempVars}=_)
  when is_atom(VarName) ->
    etp_get_temp_var(VarName, TempVars);
get_var(VarName, #crun_state{temp_vars=TempVars}=_)
  when is_list(VarName) ->
    etp_get_temp_var(binstr(VarName), TempVars).


%% Change #we{} by calling a function with given arguments
change_we(Mod,FunName,ApplyArgs0,#crun_state{temp_vars=TempVars}=QueryState,Callables) ->
    Ret0 = apply_fun(Mod,FunName,ApplyArgs0,TempVars,Callables),
    case Ret0 of
        #we{}=We1 ->
            QueryState_1 = etp_store_temp(we, We1, QueryState),
            {[],QueryState_1};
        [_|_]=Ret1 ->
            {[We2], Ret2} = lists:partition(fun (#we{}) -> true; (_) -> false end, Ret1),
            QueryState_1 = etp_store_temp(we, We2, QueryState),
            {Ret2,QueryState_1}
    end.

%% Get a value from #we{}
val_from_we(Mod,FunName,ApplyArgs0,#crun_state{temp_vars=TempVars}=_,Callables) ->
    apply_fun(Mod,FunName,ApplyArgs0,TempVars,Callables).

apply_fun(Mod,FunName,ApplyArgs0,TempVars,Callables) ->
    {ApplyArgs,RetTypes} = apply_args_we({Mod,FunName,length(ApplyArgs0)}, ApplyArgs0, TempVars,Callables),
    returned_vals(erlang:apply(Mod,FunName,ApplyArgs),RetTypes).



%% Change current #we{} by calling a function with given arguments and previous we on stack
change_with_previous_we(StackIndex,Mod,FunName,ApplyArgs0,#crun_state{temp_vars=TempVars}=QueryState,Callables) ->
    Ret0 = tempvars_with_previous_we(
        fun (TempVars1) ->
            apply_fun(Mod,FunName,ApplyArgs0,TempVars1,Callables)
        end, TempVars, StackIndex),
    case Ret0 of
        #we{}=We1 ->
            QueryState_1 = etp_store_temp(we, We1, QueryState),
            {[],QueryState_1};
        [_|_]=Ret1 ->
            {[We2], Ret2} = lists:partition(fun (#we{}) -> true; (_) -> false end, Ret1),
            QueryState_1 = etp_store_temp(we, We2, QueryState),
            {Ret2,QueryState_1}
    end.

%% Get a value from previous #we{} in stack
val_from_previous_we(StackIndex,Mod,FunName,ApplyArgs0,#crun_state{temp_vars=TempVars}=_,Callables) ->
    tempvars_with_previous_we(
        fun (TempVars1) ->
            apply_fun(Mod,FunName,ApplyArgs0,TempVars1,Callables)
        end, TempVars, StackIndex).


tempvars_with_previous_we(Fun, TempVars, StackIndex)
  when is_map(TempVars), is_integer(StackIndex) ->
    WeStack = maps:get(prev_we_stack, TempVars),
    We1 = lists:nth(StackIndex, WeStack),
    Fun(TempVars#{we=>We1}).


returned_vals(Tuple, [_|_]=Types) ->
    returned_vals_1(tuple_to_list(Tuple), Types);
returned_vals(Ret, Type) ->
    returned_vals_2(Ret, Type).

returned_vals_1([Ret|List], [Type|Types]) ->
    [returned_vals_2(Ret, Type)|returned_vals_1(List, Types)];
returned_vals_1([], []) ->
    [].

returned_vals_2(Ret, false) ->
    Ret;
returned_vals_2(Ret, gbset) ->
    gb_sets:to_list(Ret);
returned_vals_2(Ret, gbtree) ->
    gb_trees:to_list(Ret);
returned_vals_2(Ret, _Type) ->
    Ret.


%% Add #we{} into argument list.
apply_args_we(ModFun, ApplyArgs0, TempVars, Callables) ->
    {ArgTypes,ReturnTypes} = case maps:find(ModFun, Callables) of
        {ok,ArgRet} -> ArgRet;
        _ -> {false,list}
    end,
    Args = apply_args_we_1(ArgTypes, ApplyArgs0, TempVars),
    {Args, ReturnTypes}.
apply_args_we_1(false, ApplyArgs0, TempVars) ->
    We0 = etp_get_temp_var(we, TempVars),
    lists:map(fun
        (we) -> We0;
        ([list_to_gbset, V]) -> gb_sets:from_list(V);
        ([gbset_to_list, V]) -> gb_sets:to_list(V);
        (B) -> B
    end, ApplyArgs0);
apply_args_we_1(List, ApplyArgs0, TempVars) ->
    apply_args_we_2(List, ApplyArgs0, TempVars, []).

apply_args_we_2([], _ApplyArgs0, _TempVars, OL) ->
    lists:reverse(OL);
apply_args_we_2([we|List], ApplyArgs0, TempVars, OL) ->
    We0 = etp_get_temp_var(we, TempVars),
    apply_args_we_2(List, ApplyArgs0, TempVars, [We0|OL]);
apply_args_we_2([st|List], ApplyArgs0, TempVars, OL) ->
    St0 = etp_get_temp_var(st, TempVars),
    apply_args_we_2(List, ApplyArgs0, TempVars, [St0|OL]);
apply_args_we_2([Type|List], [Arg|ApplyArgs0], TempVars, OL) ->
    Arg_1 = case Type of
        atom when is_atom(Arg) -> Arg;
        atom when is_list(Arg) -> list_to_atom(Arg);
        float when is_number(Arg) -> float(Arg);
        gbset -> gb_sets:from_list(Arg);
        gbtree -> gb_trees:from_orddict(orddict:from_list(Arg));
        int when is_integer(Arg) -> Arg;
        int when is_number(Arg) -> round(Arg);
        list when is_list(Arg) -> Arg;
        matrix when is_atom(Arg) -> Arg;
        matrix ->
            Arg_2 = if is_list(Arg) -> list_to_tuple(Arg); true -> Arg end,
            case Arg_2 of
                {A11,A21,A31,
                 A12,A22,A32,
                 A13,A23,A33,
                 A14,A24,A34} ->
                    {float(A11),float(A21),float(A31),
                     float(A12),float(A22),float(A32),
                     float(A13),float(A23),float(A33),
                     float(A14),float(A24),float(A34)};
                {A11,A21,A31,A41,
                 A12,A22,A32,A42,
                 A13,A23,A33,A43,
                 A14,A24,A34,A44} ->
                    {float(A11),float(A21),float(A31),float(A41),
                     float(A12),float(A22),float(A32),float(A42),
                     float(A13),float(A23),float(A33),float(A43),
                     float(A14),float(A24),float(A34),float(A44)}
            end;
        ordset when is_list(Arg) -> ordsets:from_list(Arg);
        val -> Arg;
        vec2 ->
            case Arg of
                {X,Y} when is_number(X),is_number(Y) ->
                    {float(X),float(Y)};
                [X,Y] when is_number(X),is_number(Y) ->
                    {float(X),float(Y)}
            end;
        vec3 ->
            case Arg of
                {X,Y,Z} when is_number(X),is_number(Y),is_number(Z) ->
                    {float(X),float(Y),float(Z)};
                [X,Y,Z] when is_number(X),is_number(Y),is_number(Z) ->
                    {float(X),float(Y),float(Z)}
            end;
        vec3_or_atom ->
            case Arg of
                {X,Y,Z} when is_number(X),is_number(Y),is_number(Z) ->
                    {float(X),float(Y),float(Z)};
                [X,Y,Z] when is_number(X),is_number(Y),is_number(Z) ->
                    {float(X),float(Y),float(Z)};
                _ when is_atom(Arg) ->
                    Arg
            end
    end,
    apply_args_we_2(List, ApplyArgs0, TempVars, [Arg_1|OL]).



info_dialog(List) ->
    info_dialog(?__(1,"Info"), List).
info_dialog(Title, [Str|_]=List)
  when is_list(Str) ->
    wings_dialog:info(Title, List, []);
info_dialog(Title, [C|_]=Str)
  when is_integer(C) ->
    info_dialog(Title, [Str]).


run_script_tuplefy({string, BString1})
  when is_binary(BString1) ->
    unbinstr(BString1);
run_script_tuplefy({{atom, Atom1},Val})
  when is_binary(Atom1) ->
    {list_to_atom(unbinstr(Atom1)), run_script_tuplefy(Val)};
run_script_tuplefy(['!list']) ->
    [];
%% Specifies this is actually a list of atoms.
run_script_tuplefy(['!list', Atom | Everything])
  when is_atom(Atom) ->
    [Atom | run_script_tuplefy_args(Everything)];
run_script_tuplefy([ok, Tuple])
  when is_list(Tuple) ->
    {ok, run_script_tuplefy(Tuple)};
run_script_tuplefy([TupleName | Everything])
  when is_atom(TupleName) ->
    case is_wings_rec(TupleName, length(Everything)) of
        true ->
            list_to_tuple([TupleName | run_script_tuplefy_args(Everything)]);
        false ->
            [TupleName | run_script_tuplefy_args(Everything)]
    end;
run_script_tuplefy(Tuple)
  when is_tuple(Tuple),
       is_tuple(element(1, Tuple)),
       element(1,element(1,Tuple)) =:= atom  ->
    run_script_tuplefy(tuple_to_list(Tuple));
run_script_tuplefy([TupleName | Everything])
  when is_binary(TupleName) ->
    list_to_tuple(
        [list_to_atom(unbinstr(TupleName))
        | run_script_tuplefy_args(Everything)]);
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


%% Test if a given atom and a length means this might be a list
%% that represents a record.
%%
is_wings_rec(e3d_transf, 2) -> true;
is_wings_rec(e3d_face, 7) -> true;
is_wings_rec(e3d_mesh, 8) -> true;
is_wings_rec(e3d_object, 4) -> true;
is_wings_rec(e3d_file, 4) -> true;
is_wings_rec(e3d_image, 10) -> true;

is_wings_rec(new_shape, 3) -> true;

is_wings_rec(_, _) -> false.

%% Get the process id of the current interpreter handling process.
%%
get_script_pid(ScriptType_S, Settings) ->
    RunnerPIDAtom = list_to_atom(
        "shape_from_scripts_script_runner_" ++ ScriptType_S),
    case whereis(RunnerPIDAtom) of
        undefined ->
            %% The init folder contain code that runs before our scripts
            InitScriptsDir = filename:absname(code:where_is_file(init_dir())),
            {Interpreter, Arguments, InitFile} =
                scripting_engines:script_interp(ScriptType_S, Settings, InitScriptsDir),
            case Interpreter of
                none ->
                    {error, no_interpreter};
                _ ->
                    case os:find_executable(Interpreter) of
                        false ->
                            {error, interpreter_not_found};
                        InterpreterFullPath ->
                            WingsLang = erlang:get(wings_lang), %% for wings_lang
                            PIDNew = spawn(fun () ->
                                erlang:put(wings_lang,WingsLang),
                                run_script_runner(InterpreterFullPath, Arguments, InitFile)
                            end),
                            register(RunnerPIDAtom, PIDNew),
                            {ok, PIDNew}
                    end
            end;
        PID ->
            {ok, PID}
    end.

%% This runs from the interpreter handling process.
%%
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
            err(io_lib:format(?__(1,"could not run script interpreter:")++" ~s",
                [Interpreter]))
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
            io:format(?__(1,"~p: Unexpected ret: ~p~n"), [?MODULE,M]),
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
                _:Error:StTr ->
                    err(io_lib:format("~p~nStTr:~n~p", [Error, StTr])),
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
                    scr_debug(SReply),
                    run_script_runner_inner_loop(Port, RetPID, false, []);
                    
                %% Longer lines are split into chunks
                {eol, SReply} when StartedL =:= true ->
                    case LAcc of
                        debug ->
                            scr_debug(SReply),
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
                            scr_debug(SReply),
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
            io:format("~p: Unexpected ret: ~p~n", [?MODULE, Unk]),
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
        _:Error:StTr ->
            err(io_lib:format("~p~nStTr:~n~p", [Error, StTr])),
            RetPID ! exited,
            exit(error)
    end.


%%
%% Select Script 
%%

-spec scripts_menu_select_plugin([file:filename_all()], wings_op_mode()) ->
        any().
scripts_menu_select_plugin(ScriptDirs, Op) ->
    Parent = wings_dialog:get_dialog_parent(),
    Result = init_dlg_select_script([
        {parent, Parent},
        {script_dirs, ScriptDirs},
        {op, Op}
    ]),
    Result.

init_dlg_select_script(Config) -> wx:batch(fun() -> dlg_select_script_do_init(Config) end).
dlg_select_script_do_init(Config) ->
    Parent = proplists:get_value(parent, Config),
    ScriptDirs = proplists:get_value(script_dirs, Config),
    Op = proplists:get_value(op, Config),
    
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
            dlg_select_script_refresh(ScriptDirs, Op, ScriptTable, ScriptList)
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
    
    dlg_select_script_refresh(ScriptDirs, Op, ScriptTable, ScriptList),
    
    wxWindow:disable(Btnokay),
    
    catch wings_dialog:set_dialog_parent(F),
    case wxDialog:showModal(F) of
        ?wxID_OK ->
            Indx = wxTreeCtrl:getSelection(ScriptList),
            case ets:lookup(ScriptTable, {script, Indx}) of
                []                  -> Result = keep;
                [{_, {_Name, ScriptFile, WSCRFile, ScriptType, _ScriptDesc}}] ->
                    Result = {load_script, {ScriptFile, ScriptType, WSCRFile}}
            end;
            
        _ ->
            Result = keep
    end,
    ets:delete(ScriptTable),
    catch wings_dialog:reset_dialog_parent(F),
    wxDialog:destroy(F),
    Result.


%% Get a list of paths for scripts.
%%
script_dirs(DirsSetting) -> 
    Str = wpa:pref_get(?MODULE, DirsSetting, ""),
    scripting_engines:extra_script_dirs() ++
    lists:filter(fun ("") -> false; (_) -> true end,
        [ string:trim(A1) || A1 <- string:tokens(Str, "\r\n") ]).


%% Make paths shorter for the UI
%%
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

-spec dlg_select_script_refresh([file:filename_all()], wings_op_mode(),
        any(), wx:wx_object()) -> ok.
dlg_select_script_refresh(ScriptDirs, Op, ScriptTable, ScriptList) ->
    wxTreeCtrl:deleteAllItems(ScriptList),
    ERoot = wxTreeCtrl:addRoot(ScriptList, "*"),
    lists:foreach(fun({Directory, SList}) ->
        Elm = script_list_adddir(ERoot, ScriptList, not_the_full_path(Directory)),
        lists:foreach(fun({WSCRFile, {ScriptFile, ScriptType, ScriptName, ScriptDesc}}) ->
            script_list_add(
                ScriptTable, ScriptList, Elm, ScriptName, ScriptFile,
                WSCRFile, ScriptType, ScriptDesc)
        end, SList)
    end, read_script_dirs(ScriptDirs, Op)),
    ok.


script_list_adddir(ERoot, ScriptList, Name) ->
    Elm = wxTreeCtrl:appendItem(ScriptList, ERoot, Name),
    wxTreeCtrl:ensureVisible(ScriptList, Elm),
    Elm.

script_list_add(ScriptTable, ScriptList, ParentNode, Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc) ->
    Elm = wxTreeCtrl:appendItem(ScriptList, ParentNode, Name),
    ets:insert(ScriptTable, {{script, Elm}, {Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc}}).

dlg_select_script_insert_txt(E, Txt) ->
    Sty_nrm = wxTextAttr:new(),
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
    Text = io_lib:format("~s~n~n~s [~s]~n", [ScriptDesc, ScriptFile, ScriptType]),
    dlg_select_script_insert_txt(InfoBox, Text),
    wxTextCtrl:setSelection(InfoBox, 0, 0).

dlg_select_script_show_info(ScriptTable, InfoBox, Btnokay, Indx) ->
    case ets:lookup(ScriptTable, {script, Indx}) of
        [] ->
            wxWindow:disable(Btnokay);
        [{_, {Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc}}] ->
            wxWindow:enable(Btnokay),
            dlg_select_script_display_about_details(InfoBox, Name, ScriptFile, WSCRFile, ScriptType, ScriptDesc)
    end.

-spec read_script_dirs([file:filename_all()], wings_op_mode()) ->
        [{file:filename_all(), [file:filename_all()]}].
read_script_dirs(ScriptDirs, shape) ->
    read_script_dirs(ScriptDirs, false);
read_script_dirs(ScriptDirs, Op) ->
    read_script_dirs(ScriptDirs, Op, []).
read_script_dirs([], _, OList) -> lists:reverse(OList);
read_script_dirs([Path | ScriptDirs], Op, OList) ->
    case file:list_dir(Path) of
        {ok, Filenames} ->
            {ok, LResults} = iterate_filenames(Filenames, Path, Op);
        _ ->
            LResults = []
    end,
    read_script_dirs(ScriptDirs, Op, [{Path, LResults} | OList]).

-spec iterate_filenames([file:filename_all()], file:filename_all(),
        wings_op_mode()) -> {ok, [file:filename_all()]}.
iterate_filenames([FlN | Filenames], Path, Op) ->
    {ok, iterate_filenames([FlN | Filenames], Path, Op, [])}.
iterate_filenames([], _, _, OList) -> lists:reverse(OList);
iterate_filenames([FlN | Filenames], Path, Op, OList) ->
    FullFilename = filename:join(Path, FlN),
    OList_1 = case file:read_file_info(FullFilename) of
        {ok, FileInfo} ->
            iterate_filenames_1(element(3, FileInfo), Filenames,
                Path, Op, OList, FlN, FullFilename);
        _ ->
            OList
    end,
    iterate_filenames(Filenames, Path, Op, OList_1).
iterate_filenames_1(directory, _Filenames, _Path, Op, OList, _FlN, FullFilename) ->
    SubDir = FullFilename,
    OList_1 = case file:list_dir(SubDir) of
        {ok, SubDir_Filenames} ->
            iterate_filenames(SubDir_Filenames, SubDir, Op, OList);
        _ ->
            OList
    end,
    OList_1;
iterate_filenames_1(_, _Filenames, _Path, Op, OList, FlN, FullFilename) ->
    Extension = string:to_lower(filename:extension(FlN)),
    case Extension of
        ".wscr" ->
            case read_script_info_file(FullFilename, Op) of
                {ok, W} -> [{FullFilename, W} | OList];
                _       -> OList
            end;
        _ ->
            OList
    end.



-spec read_script_info_file(file:filename_all(), wings_op_mode()) ->
        error | {ok, {file:filename_all() | none, string() | none, string(), string()}}.
read_script_info_file(FlN, Op) ->
    NameOnly = string:substr(FlN, 1, string:rchr(FlN, $.)-1),
    case file:read_file(FlN) of
        {ok, BCont} ->
            {ok, StrList} = load_lang_file(FlN, current_lang_code()),
            {ok, Cont_0} = read_wscr_content(BCont, StrList),
            Cont = wscr_map(Cont_0),
            ScriptMode = script_mode(Cont),
            
            case lists:member(Op, ScriptMode) of
                false -> unused;
                true ->
                    WSCRType_0 = map_find(type, Cont, fun () -> detect end),
                    WSCRName = map_find(name, Cont, fun () -> "" end),
                    WSCRDesc = map_find(description, Cont, fun () -> "" end),
                    case WSCRType_0 of
                        detect ->
                            {WSCRType, ScriptFound} = case file_of_this_type(NameOnly) of
                                [FoundType | _] ->
                                    {FoundType,
                                     NameOnly ++ "." ++ FoundType};
                                [] ->
                                    {none,
                                     none}
                            end;
                        _ ->
                            WSCRType = WSCRType_0,
                            ScriptFound_0 = NameOnly ++ "." ++ WSCRType_0,
                            ScriptFound = case file:read_file_info(ScriptFound_0) of
                                {ok, _} -> ScriptFound_0;
                                _       -> none
                            end
                    end,
                    {ok, {ScriptFound, WSCRType, WSCRName, WSCRDesc}}
            end;
        _ ->
            error
    end.

script_mode(Cont) ->
    case maps:find(mode, Cont) of
        error -> [false];
        {ok, ModeStr} when is_list(ModeStr) ->
            case string:trim(string:lowercase(ModeStr)) of
                "import" -> [import];
                "export" -> [export];
                _ ->
                    script_mode_list(ModeStr)
            end
    end.

script_mode_list("simple_body_command") ->
    [body];
script_mode_list(ModeList) ->
    List1 = [list_to_atom(S) || S <- string:tokens(ModeList," ,")],
    List2 = lists:filter(fun (B) -> lists:member(B, [vertex, edge, face, body, light]) end, List1),
    if
        List2 =:= [] -> [false];
        true -> List2
    end.

file_of_this_type(NameOnly) ->
    AllEng = scripting_engines:all_engines(),
    lists:concat(
        lists:map(file_of_this_type_1(NameOnly),
            [Eng || {Eng,_} <- AllEng])).

file_of_this_type_1(NameOnly) ->
    fun(T) ->
        ScriptFound_0 = NameOnly ++ "." ++ T,
        case file:read_file_info(ScriptFound_0) of
            {ok, _} -> [T];
            _       -> []
        end
    end.


wscr_map(L) -> wscr_map(L, []).
wscr_map([], O) -> maps:from_list(O);
wscr_map([[A1, A2] | R], O) when is_list(A1), is_list(A2) ->
    case lists:member(A1, [
        "mode",
        "type",
        "name",
        "description",
        "extensions",
        "command_changes",
        "command_inputs",
        "params_title",
        "params_preview",
        "params",
        "params_templates"
    ]) of
        true ->
            wscr_map(R, [{list_to_atom(A1), A2}|O]);
        false ->
            wscr_map(R, [{A1, A2}|O])
    end;
wscr_map([_ | R], O) ->
    wscr_map(R, O).


-define(CR_ONLY(C), (C =:= 10 orelse C =:= 13)).
-define(SPACE_ONLY(C), (C =:= 32 orelse C =:= 9)).
-define(SPACE_CR(C), (?CR_ONLY(C) orelse ?SPACE_ONLY(C))).

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
%% Quoted atom
read_wscr_content(<<C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= $' ->
    case CWord of
        [] ->
            {Str, R_1} = read_wscr_atom(R),
            read_wscr_content(R_1, StrList, CWord, [Str | CLine], Lines);
        _  ->
            read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
    end;
%% Syntax for term path
read_wscr_content(<<$%, $[, R/binary>>=RInp, StrList, CWord, CLine, Lines) ->
    case CWord of
        [] ->
            {Str, R_1} = read_wscr_etp(R),
            read_wscr_content(R_1, StrList, CWord, [Str | CLine], Lines);
        _  ->
            read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
    end;



%% Advanced dialog structuring:
%% Example:
%%     adv_params begin
%%       {vframe,[
%%        ], [{title, ?__("Test"))}]}
%%     end.
%%
read_wscr_content(<<WS,$b,$e,$g,$i,$n,CR,Bin/binary>>, StrList, [], CLine, Lines)
  when ?SPACE_CR(WS), ?CR_ONLY(CR) ->
    {A, Bin_1} = parse_advparam(Bin, StrList),
    read_wscr_content(Bin_1, StrList, [], [A|CLine], Lines);
read_wscr_content(<<WS,$b,$e,$g,$i,$n,CR,_/binary>>=RInp, StrList, CWord, CLine, Lines)
  when ?SPACE_CR(WS), ?CR_ONLY(CR) ->
    read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines);

%% Multi-line string
%% Example:
%%    """
%%        String contents
%%    """
%%
read_wscr_content(<<DQ,DQ,DQ, Bin/binary>>, StrList, [], CLine, Lines)
  when DQ =:= 34 ->
    {A, Bin_1} = advparam_mstr(Bin),
    read_wscr_content(Bin_1, StrList, [], [A|CLine], Lines);
read_wscr_content(<<DQ,DQ,DQ, _/binary>>=RInp, StrList, CWord, CLine, Lines)
  when DQ =:= 34 ->
    read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines);


%% String localized, the parser will use the string in StrList indicated by
%% the first argument integer, or the second argument is used as the string
%% if not found.
%%
read_wscr_content(<<$?, $_, $_, C, R/binary>>=RInp, StrList, CWord, CLine, Lines)
  when C =:= 40 -> %% Opening parenthesis
    case CWord of
        [] ->
            {LocStrID, Str, R_1} = read_wscr_string_locale(R),
            case orddict:find(LocStrID, StrList) of
                {ok, StrTranslated} ->
                    read_wscr_content(R_1, StrList, [], [StrTranslated | CLine], Lines);
                _ ->
                    read_wscr_content(R_1, StrList, [], [Str | CLine], Lines)
            end;
        _  ->
            read_wscr_content(RInp, StrList, [], [lists:reverse(CWord) | CLine], Lines)
    end;
read_wscr_content(<<C, R/binary>>, StrList, CWord, CLine, Lines)
  when ?SPACE_ONLY(C) ->
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
  when ?CR_ONLY(C) ->
    read_wscr_content(R, StrList, [],
        [], Lines);
read_wscr_content(<<C, R/binary>>, StrList, [], CLine, Lines)
  when ?CR_ONLY(C) ->
    read_wscr_content(R, StrList, [],
        [], [lists:reverse(CLine) | Lines]);
read_wscr_content(<<C, _/binary>>=RInp, StrList, CWord, CLine, Lines)
  when ?CR_ONLY(C) ->
    read_wscr_content(RInp, StrList, [],
        [lists:reverse(CWord) | CLine], Lines);
read_wscr_content(<<C, R/binary>>, StrList, CWord, CLine, Lines) ->
    read_wscr_content(R, StrList, [C | CWord], CLine, Lines).


-spec backsl(integer()) -> integer().
backsl($n) -> $\n;
backsl($r) -> $\r;
backsl($t) -> $\t;
backsl(C) when is_integer(C) -> C.


%% Reads the content of '...' in a .wscr
%%
-spec read_wscr_atom(binary()) -> {{atom,list()},binary()}.
read_wscr_atom(R_0)
  when is_binary(R_0) ->
    {Str, R} = read_wscr_atom(R_0, []),
    {{atom,unicode:characters_to_list(iolist_to_binary(Str), utf8)}, R}.
read_wscr_atom(<<$\\, C, R/binary>>, Str) ->
    read_wscr_atom(R, [backsl(C) | Str]);
read_wscr_atom(<<C, R/binary>>, Str) when C =:= $' ->
    {lists:reverse(Str), R};
read_wscr_atom(<<C, R/binary>>, Str) ->
    read_wscr_atom(R, [C | Str]).


%% Reads the content of "..." in a .wscr
%%
-spec read_wscr_string(binary()) -> {list(),binary()}.
read_wscr_string(R_0)
  when is_binary(R_0) ->
    {Str, R} = read_wscr_string(R_0, []),
    {unicode:characters_to_list(iolist_to_binary(Str), utf8), R}.
read_wscr_string(<<$\\, C, R/binary>>, Str) ->
    read_wscr_string(R, [backsl(C) | Str]);
read_wscr_string(<<C, R/binary>>, Str) when C =:= 34 ->
    {lists:reverse(Str), R};
read_wscr_string(<<C, R/binary>>, Str) ->
    read_wscr_string(R, [C | Str]).

%% Reads the content of ?__(Digits,"...") in a .wscr
%%
-spec read_wscr_string_locale(binary()) ->
            {integer(),string(),binary()} | {error, atom()}.
read_wscr_string_locale(R)
  when is_binary(R) ->
    read_wscr_string_locale(R, 1, [], []).
read_wscr_string_locale(<<C, R/binary>>, 1, Digits, Str)
  when C >= $0, C =< $9 ->
    read_wscr_string_locale(R, 1, [C | Digits], Str);
read_wscr_string_locale(<<DQ,DQ,DQ, Bin/binary>>, 2, Digits, _)
  when DQ =:= 34->
    {Str, R_1} = advparam_mstr(Bin),
    read_wscr_string_locale(R_1, 2, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, 2, Digits, _)
  when C =:= 34 ->
    {Str, R_1} = read_wscr_string(R),
    read_wscr_string_locale(R_1, 2, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, N, Digits, Str)
  when C =:= $, ->
    read_wscr_string_locale(R, N+1, Digits, Str);
read_wscr_string_locale(<<C, R/binary>>, N, Digits, Str)
  when ?SPACE_CR(C) ->
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


%% Reads the content of %[...] in a .wscr
%%
-spec read_wscr_etp(binary()) -> {{etp,string()},binary()}.
read_wscr_etp(R)
  when is_binary(R) ->
    read_wscr_etp(R, [], []).
read_wscr_etp(<<C, R/binary>>, Str, Context) when C =:= $[ ->
    read_wscr_etp(R, [C | Str], [$[|Context]);
read_wscr_etp(<<$], R/binary>>, Str, []) ->
    {{etp, unicode:characters_to_list(iolist_to_binary(lists:reverse(Str)), utf8)}, R};
read_wscr_etp(<<C, R/binary>>, Str, [$[|Context]) when C =:= $] ->
    read_wscr_etp(R, [C | Str], Context);
read_wscr_etp(<<$", R_0/binary>>, Str, Context) ->
    {Str1, R} = read_wscr_string(R_0, []),
    read_wscr_etp(R, [$",Str1,$" | Str], Context);
read_wscr_etp(<<$', R_0/binary>>, Str, Context) ->
    {Str1, R} = read_wscr_atom(R_0, []),
    read_wscr_etp(R, [$',Str1,$' | Str], Context);
read_wscr_etp(<<C, R/binary>>, Str, Context) ->
    read_wscr_etp(R, [C | Str], Context).



parse_advparam(A,StrList)
  when is_binary(A), is_list(StrList) ->
    erlang:put({?MODULE,expr_idx}, 0),
    Ret = parse_advparam(A, StrList, []),
    erlang:erase({?MODULE,expr_idx}),
    Ret.
parse_advparam(<<WS,$e,$n,$d,$.,CR, Bin/binary>>, StrList, OL)
  when ?CR_ONLY(CR), ?SPACE_CR(WS) ->
    Str_0 = string:trim(lists:reverse(OL)),
    {Exprs, Str} = advparam_bp1(Str_0),
    {ok, Toks_0, _} = erl_scan:string(Str ++ "."),
    Toks = advparam_bp2(Toks_0, StrList),
    {ok, Term} = erl_parse:parse_term(Toks),
    {{adv_term, Term, Exprs}, Bin};
parse_advparam(<<C, Bin/binary>>, StrList, OL) ->
    parse_advparam(Bin, StrList, [C|OL]).

%% Substitute "%[<expr>]" with query expressions
%%
advparam_bp1(Str)
  when is_list(Str) ->
    advparam_bp1(Str, [], []).
advparam_bp1([$%,$[|Str], OL, OL2) ->
    {IStr_0, Str_1} = advparam_bp1_1(Str),
    {Exprs, IStr} = advparam_make_expr(IStr_0),
    advparam_bp1(Str_1, lists:reverse(IStr) ++ OL, [Exprs|OL2]);
advparam_bp1([DQ|Str], OL, OL2)
  when DQ =:= 34 ->
    {InsideRev, Str_1} = advparam_bp1_str(Str),
    advparam_bp1(Str_1, [DQ] ++ InsideRev ++ [DQ|OL], OL2);
advparam_bp1([C|Str], OL, OL2) ->
    advparam_bp1(Str, [C|OL], OL2);
advparam_bp1([], OL, OL2) ->
    {lists:reverse(OL2),lists:reverse(OL)}.

advparam_bp1_1(Str)
  when is_list(Str) ->
    advparam_bp1_1(Str, []).
advparam_bp1_1([DQ|Str], OL)
  when DQ =:= 34 ->
    {InsideRev, Str_1} = advparam_bp1_str(Str),
    advparam_bp1_1(Str_1, [DQ] ++ InsideRev ++ [DQ|OL]);
advparam_bp1_1([$[|Str], OL) ->
    {Inside, Str_1} = advparam_bp1_1(Str),
    advparam_bp1_1(Str_1, [$]] ++ Inside ++ [$[|OL]);
advparam_bp1_1([$]|Str], OL) ->
    {lists:reverse(OL), Str};
advparam_bp1_1([C|Str], OL) ->
    advparam_bp1_1(Str, [C|OL]);
advparam_bp1_1([], OL) ->
    {lists:reverse(OL), []}.

advparam_bp1_str(Str)
  when is_list(Str) ->
    advparam_bp1_str(Str, []).
advparam_bp1_str([$\\,DQ|Str], OL)
  when DQ =:= 34 ->
    advparam_bp1_str(Str, [DQ,$\\|OL]);
advparam_bp1_str([DQ|Str], OL)
  when DQ =:= 34 ->
    {OL, Str};
advparam_bp1_str([C|Str], OL) ->
    advparam_bp1_str(Str, [C|OL]).


%% Substitute locale strings
%%
advparam_bp2(Toks_0, StrList) when is_list(Toks_0), is_list(StrList) ->
    advparam_bp2(Toks_0, StrList, []).
advparam_bp2([{'?',_},{var,_,'__'},{'(',_},{integer,_,LocStrID},
             {',',A}|Toks_0], StrList, OL) ->
    {StrConcat, Toks_1} = advparam_bp2_1(Toks_0),
    Str = case orddict:find(LocStrID, StrList) of
        {ok, StrTranslated} ->
            StrTranslated;
        _ ->
            StrConcat
    end,
    T = {string,A,Str},
    advparam_bp2(Toks_1, StrList, [T|OL]);
advparam_bp2([T|Toks_0], StrList, OL) ->
    advparam_bp2(Toks_0, StrList, [T|OL]);
advparam_bp2([], _StrList, OL) ->
    lists:reverse(OL).

advparam_bp2_1(Toks) when is_list(Toks) ->
    advparam_bp2_1(Toks, []).
advparam_bp2_1([{string,_,Str}|Toks], OL) ->
    advparam_bp2_1(Toks, [Str|OL]);
advparam_bp2_1([{')',_}|Toks], OL) ->
    {lists:append(lists:reverse(OL)), Toks}.


advparam_mstr(A) when is_binary(A) ->
    advparam_mstr(A, []).
advparam_mstr(<<CR,DQ,DQ,DQ, Bin/binary>>, OL)
  when ?CR_ONLY(CR), DQ =:= 34 ->
    Str = string:trim(lists:reverse(OL)),
    {Str, Bin};
advparam_mstr(<<C, Bin/binary>>, OL) ->
    advparam_mstr(Bin, [C|OL]).

advparam_make_expr(Expr) ->
    Num = erlang:get({?MODULE,expr_idx}),
    erlang:put({?MODULE,expr_idx},Num+1),
    Ident = "%%" ++ integer_to_list(Num),
    {{list_to_atom(Ident), Expr}, "'" ++ Ident ++ "'"}.


%% Get the lang code.
current_lang_code() ->
    atom_to_list(erlang:get(wings_lang)).

%% Load a language file for the script wscr, they are in the same
%% format as a wings plugin's lang file.
-spec load_lang_file(file:name_all(), string()) -> {ok, list()} | error.
load_lang_file(WSCRFile, [_,_|_]=LangCode) when is_list(WSCRFile) ->
    load_lang_file(WSCRFile, LangCode, wscr).
load_lang_file(WSCRFile, LangCode, Which) ->
    LangFile = filename:rootname(WSCRFile) ++ "_" ++ LangCode ++ ".lang",
    case file:consult(LangFile) of
        {ok, [{_, LangSectionList}]} ->
            case orddict:find(Which, LangSectionList) of
                {ok, StrList} when is_list(StrList) -> {ok, StrList};
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
%% wx UI
%%

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




%%%
%%% Script Preference Dialog
%%%

dlg_script_preference(#st{}=St) ->
    Pag_Shapes =
        {?__(3,"Shapes"), {vframe, dlg_script_preference_paths(setting_paths_shapes)}},
    Pag_Commands =
        {?__(4,"Commands"), {vframe, dlg_script_preference_paths(setting_paths_commands)}},
    Pag_Imp_Export =
        {?__(5,"Import/Export"), {vframe, dlg_script_preference_paths(setting_paths_import_export)}},
    Pag_Interpreters =
        {?__(6,"Interpreters"), {vframe, dlg_script_preference_paths(interpreters)}},
    Pag_Debug =
        {?__(7,"Debug"),{vframe, dlg_script_preference_paths(setting_show_tuple)}},

    Dialog =
        [
            {?__(2,"Enable Scripts"), get_pref(setting_enable), [{key,setting_enable}]},
            {vframe, [
                {oframe, [
                    Pag_Shapes,
                    Pag_Commands,
                    Pag_Imp_Export,
                    Pag_Interpreters,
                    Pag_Debug], 1, [{style, buttons}]}
            ], [{key,pnl_script_pref}, {show,true}, {margin,false}]}
         
        ],
    wpa:dialog(?__(1,"Script Preference"), Dialog, fun(Attr) -> dlg_script_result(Attr, St) end).

dlg_script_result(Attr0, #st{}=St) ->
    Defaults = get_user_prefs(get_defaults()),
    Attr1 = pref_path_result([
        setting_paths_shapes,
        setting_paths_commands,
        setting_paths_import_export], Attr0),
    Attr = [{Key,proplists:get_value(Key,Attr1,DVal)} || {Key,DVal} <- Defaults],
    set_user_prefs(Attr),
    St.



dlg_script_pref_eng(_, no_bin) -> [];
dlg_script_pref_eng(Eng, HasArgs)
  when is_boolean(HasArgs) ->
    Atom = list_to_atom("setting_" ++ Eng ++ "_int_path"),
    dlg_script_preference_path_item({int_path, Atom, Eng}, get_pref(Atom))
    ++ dlg_script_pref_eng_1(Eng, HasArgs).

dlg_script_pref_eng_1(_, false) -> [];
dlg_script_pref_eng_1(Eng, true) ->
    Atom = list_to_atom("setting_" ++ Eng ++ "_arguments"),
    dlg_script_preference_path_item({arguments, Atom, Eng}, get_pref(Atom)).


dlg_script_preference_paths(setting_show_tuple=Key) ->
    ChkEnableShowTuple = get_pref(Key),
    [{?__(4,"Enable Debug Return Data"), ChkEnableShowTuple, [{key,Key}]}];
dlg_script_preference_paths(interpreters) ->
    AllEng = scripting_engines:all_engines(),
    lists:append(
        [ dlg_script_pref_eng(Eng, HasArgs)
        || {Eng, HasArgs} <- AllEng]);
dlg_script_preference_paths(Key) ->
    Paths = get_pref(Key),
    [{label, ?__(9,"Each script must have with it a file with the extension "
                   ".wscr that contains its name, description and parameters.")},
     panel] ++
    dlg_script_preference_path_item(Key, Paths).

dlg_script_preference_path_item(setting_paths_shapes=Key, Paths) ->
    [{label, ?__(1,"Paths for Shape Scripts")},
     pref_path_qs(Paths, Key, table_labels())
     ];
dlg_script_preference_path_item(setting_paths_commands=Key, Paths) ->
    ChkEnableScriptsCommands = get_pref(setting_enable_commands),
    [{label, ?__(2,"Paths for Commands Scripts")},
     pref_path_qs(Paths, Key, table_labels()),
     {?__(3,"Enable Scripts for Commands Menu"), ChkEnableScriptsCommands,
        [{key,setting_enable_commands}]}
    ];
dlg_script_preference_path_item(setting_paths_import_export=Key, Paths) ->
    ChkEnableScriptsImport = get_pref(setting_enable_import),
    ChkEnableScriptsExport = get_pref(setting_enable_export),
    [{label, ?__(4,"Paths for Import/Export Scripts")},
     pref_path_qs(Paths, Key, table_labels()),
     {?__(5,"Enable Scripts for Import Menu"), ChkEnableScriptsImport,
        [{key,setting_enable_import}]},
     {?__(6,"Enable Scripts for Export Menu"), ChkEnableScriptsExport,
        [{key,setting_enable_export}]}
    ];
dlg_script_preference_path_item({int_path, Key, EngName}, Path) ->
    [{label_column, [
        {string:replace(?__(8,"% Interpreter Path"), "%", EngName),
        {button, {text, Path, [{key,Key}, {width,30}, wings_job:browse_props()]}}}
        ]}];
dlg_script_preference_path_item({arguments, Key, EngName}, Path) ->
    [{label_column, [
        {string:replace(?__(9,"% Extra Interpreter Arguments"), "%", EngName),
        {text, Path, [{key,Key}, {width,30}]}}
        ]}].

table_labels() ->
    {?__(1,"Paths"),
     ?__(2,"Add"),
     ?__(3,"Edit"),
     ?__(4,"Del")}.


get_defaults() ->
    AllEng = scripting_engines:all_engines(),
    Defaults = [
        {setting_enable, true},
        {setting_enable_commands, false},
        {setting_enable_import, false},
        {setting_enable_export, false},
        {setting_paths_shapes, ""},
        {setting_paths_commands, ""},
        {setting_paths_import_export, ""},
        {setting_show_tuple, false}
    ],
    get_defaults_conf(Defaults ++
        lists:append(
            [get_defaults_eng(Eng, HasArgs)
            || {Eng, HasArgs} <- AllEng])).


%%
%% A defaults.conf file can specify some default settings
%%
get_defaults_conf(List0) ->
    Dir = filename:absname(code:where_is_file(init_dir())),
    case file:consult(filename:join(Dir, "defaults.conf")) of
        {ok, List} ->
            orddict:merge(
                fun (_,V1,_) -> V1 end,
                orddict:from_list(List),
                orddict:from_list(List0));
        _ ->
            List0
    end.


get_defaults_eng(_, no_bin) -> [];
get_defaults_eng(Eng, HasArgs)
  when is_boolean(HasArgs) ->
    [{list_to_atom("setting_" ++ Eng ++ "_int_path"), ""}] ++
    get_defaults_eng_1(Eng, HasArgs).

get_defaults_eng_1(_, false) -> [];
get_defaults_eng_1(Eng, true) ->
    [{list_to_atom("setting_" ++ Eng ++ "_arguments"), ""}].



set_pref_defaults() ->
    [wpa:pref_set_default(?MODULE,Key,Val) || {Key,Val} <- get_defaults()].

get_pref(Key) ->
    wpa:pref_get(?MODULE, Key).

set_user_prefs(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_user_prefs(KeyDefs) when is_list(KeyDefs) ->
    [{Key, wpa:pref_get(?MODULE, Key, Def)} || {Key, Def} <- KeyDefs].


%%%
%%%

%%
%%  Preference path qs for Scripting

pref_path_qs(Paths, Key, {LabelTab, LabelAdd, LabelEdit, LabelDel}) ->
    List0 = [string:trim(A) || A <- string:split(Paths,"\n",all)],
    List1 = [A || A <- List0, length(A) > 0],
    List2 = [{{A,A}} || A <- List1],
    {vframe, [
        {table, [{LabelTab}|List2],
            [{key,{Key,table}},{max_rows,10},{col_widths,{50}},
             {sel_style,single},{hook,fun pref_path_dlg_script_hook/3}]},
        {hframe, [
            %{button,
            {text, "", [{key,{Key,text}}, {width,30}]},%},  {dialog_type,dir_dialog}
            {button, LabelAdd,  add,  [{key,{Key,add}},{hook,fun pref_path_dlg_script_hook/3}]},
            {button, LabelEdit, edit, [{key,{Key,edit}},{hook,fun pref_path_dlg_script_hook/3}]},
            {button, LabelDel,  del,  [{key,{Key,del}},{hook,fun pref_path_dlg_script_hook/3}]}
         ]}
    ]}.
pref_path_dlg_script_hook(Key, _Val, Sto) ->
    case Key of
        {Tab,add} ->
            NewPath = wings_dialog:get_value({Tab,text},Sto),
            case wings_dialog:get_value({Tab,table},Sto) of
                {_, List} when is_list(List), length(NewPath) > 0 ->
                    Val1 = {[], List ++ [{{NewPath, NewPath}}]},
                    wings_dialog:set_value({Tab,table},Val1,Sto),
                    wings_dialog:set_value({Tab,text},"",Sto);
                _ ->
                    ok
            end;
        {Tab,edit} ->
            NewPath = wings_dialog:get_value({Tab,text},Sto),
            case wings_dialog:get_value({Tab,table},Sto) of
                {[Sel], List} when is_integer(Sel), length(List) > Sel, length(NewPath) > 0 ->
                    case lists:split(Sel,List) of
                        {List1_1, [_|List2_1]} ->
                            Val1 = {[Sel], List1_1++[{{NewPath, NewPath}}|List2_1]},
                            wings_dialog:set_value({Tab,table},Val1,Sto);
                        _ ->
                            ok
                    end;
                {_, List} when is_list(List), length(NewPath) > 0 ->
                    Val1 = {[], List ++ [{{NewPath, NewPath}}]},
                    wings_dialog:set_value({Tab,table},Val1,Sto),
                    wings_dialog:set_value({Tab,text},"",Sto);
                _ ->
                    ok
            end;
        {Tab,del} ->
            case wings_dialog:get_value({Tab,table},Sto) of
                {[Sel], List} when is_integer(Sel), length(List) > Sel ->
                    case lists:split(Sel,List) of
                        {List1_1, [_|List2_1]} ->
                            Val1 = {[Sel], List1_1++List2_1},
                            wings_dialog:set_value({Tab,table},Val1,Sto);
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end;
        {Tab,table} ->
            case wings_dialog:get_value({Tab,table},Sto) of
                {[Sel], List} when is_integer(Sel), length(List) > Sel ->
                    {{_, Path}} = lists:nth(Sel+1, List),
                    wings_dialog:set_value({Tab,text},Path,Sto),
                    ok;
                _ ->
                    ok
            end
    end,
    ok.

pref_path_result(PathConfigKeys, Attr0) when is_list(PathConfigKeys) ->
    EntPaths = maps:from_list([{A,proplists:get_value({A,text}, Attr0, "")} || A <- PathConfigKeys]),
    Set = sets:from_list(PathConfigKeys),
    F = fun ({{Key,table},Val0}=Tuple) ->
        case sets:is_element(Key, Set) of
            true ->
                {_Sel,Val} = Val0,
                EntPath = maps:get(Key, EntPaths),
                List1 = lists:append([[B || B <- string:split(P,";",all)] || {{_,P}} <- Val++[{{"",EntPath}}]]),
                {Key, string:join(pref_path_clean_path_list([string:trim(P) || P <- List1]),"\n")};
            _ ->
                Tuple
        end;
        ({{_,text},_}) -> false;
        (Tuple) -> Tuple
    end,
    [A || A <- lists:map(F, Attr0), A =/= false].


%% Clean path list of empty strings and duplicates.
%%
pref_path_clean_path_list(List) when is_list(List) ->
    pref_path_clean_path_list(List,sets:new(),[]).
pref_path_clean_path_list([],_,OL) ->
    lists:reverse(OL);
pref_path_clean_path_list([""|List],Seen,OL) ->
    pref_path_clean_path_list(List,Seen,OL);
pref_path_clean_path_list([Path|List],Seen,OL) ->
    case sets:is_element(Path,Seen) of
        true ->
            pref_path_clean_path_list(List,Seen,OL);
        _ ->
            pref_path_clean_path_list(List,sets:add_element(Path,Seen),[Path|OL])
    end.

%%%
%%%


%%
%% Prepare mainly list data structures for write_scm
%%

prepare_parameter_list_for_scm(List) when is_list(List) ->
    lists:map(fun ({K, A1}) when is_atom(K) ->
                      [A1_1] = prepare_parameter_list_for_scm([A1]),
                      [{atom, list_to_binary(atom_to_list(K))}, A1_1];
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


prepare_string_pairs(StrList) when is_list(StrList) ->
    [prepare_string_pairs_1(P) || P <- StrList].
prepare_string_pairs_1({Int, Str})
  when is_list(Str) ->
    {Int, {string, binstr(Str)}};
prepare_string_pairs_1(A) -> A.


%%
%% Symbolic expression writer
%%
%% Values sent to the running script are always sent in a list
%% so the expression parser always can expect all communication
%% to be between opening and closing parenthesis.
%%

write_scm(A) when is_list(A) ->
    ?DEBUG_FMT("write_scm -> ~p~n~n", [A]),
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
write_scm([A | AList], BList, false)
  when is_atom(A) ->
    write_scm(AList, [io_lib:format("~w", [A]) | BList], true);
write_scm([A | AList], BList, false) when is_number(A) ->
    write_scm(AList, [io_lib:format("~w", [A]) | BList], true).

write_scm_tuple(A) when is_list(A) ->
    write_scm(A, [<<"#(">>], false).


%%
%% Symbolic expression reader for script input/output
%%

scm_parse(A) when is_binary(A) ->
    scm_parse(A, [], []).
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
  when ?SPACE_CR(C) ->
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


scm_parse_num(A) when is_binary(A) ->
    scm_parse_num(A, []).
scm_parse_num(<<C, _/binary>>=R, Num_S0)
  when ?SPACE_ONLY(C); C =:= $) ->
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

scm_parse_str(A) when is_binary(A) ->
    scm_parse_str(A, []).
scm_parse_str(<<$\\, C, R/binary>>, Str) ->
    scm_parse_str(R, [backsl(C) | Str]);
scm_parse_str(<<34, R/binary>>, Str) ->
    {iolist_to_binary(lists:reverse(Str)), R};
scm_parse_str(<<C, R/binary>>, Str) ->
    scm_parse_str(R, [C | Str]).

bare_word(A_0) ->
    A_1 = lists:reverse(A_0),
    case A_1 of
        "#t" -> true;
        "#f" -> false;
        [C|_] when C =:= $% ->
            {atom, iolist_to_binary(A_1)};
        _ -> list_to_atom(A_1)
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


get_template_opts(A, B)
  when is_atom(A), is_list(B) ->
    get_template_opts(A, B, []).
get_template_opts(A, ["include_"++_=Opt|Opts], OL)
  when A =:= import;
       A =:= export ->
    Opt_1 = case Opt of
        "include_normals" -> include_normals;
        "include_uvs"     -> include_uvs;
        "include_colors"  -> include_colors;
        _ -> false
    end,
    if
        Opt_1 =:= false ->
            get_template_opts(A, Opts, OL);
        true ->
            get_template_opts(A, Opts, [Opt_1|OL])
    end;
get_template_opts(A, [_|Opts], OL) ->
    get_template_opts(A, Opts, OL);
get_template_opts(_A, [], OL) ->
    lists:reverse(OL).

get_wscr_templates(Cont) when is_map(Cont) ->
    ParamsTplsLists_0 = map_find(params_templates, Cont, fun () -> [] end),
    lists:map(fun
        (["template" | L]) ->
            case L of
                ["import", Opts | _] ->
                    {import, get_template_opts(import, string:tokens(Opts, " ,"))};
                ["import"] ->
                    {import, []};
                ["export", Opts | _] ->
                    {export, get_template_opts(export, string:tokens(Opts, " ,"))};
                ["export"] ->
                    {export, []};
                _ ->
                    {unknown, []}
            end;
        (_) ->
            {unknown, []}
    end, ParamsTplsLists_0).

get_wscr_params(Cont_0) ->
    Cont = wscr_map(Cont_0),
    ParamsTitle = map_find(params_title, Cont, fun () -> get_wscr_params_default_param_title() end),
    ParamsPreview = dlg_menu_item_bool(map_find(params_preview, Cont, fun () -> "0" end)),
    ParamsLists_0 = map_find(params, Cont, fun () -> [] end),
    ParamsTplsLists = get_wscr_templates(Cont),
    ParamsLists = lists:map(get_wscr_params_1(), ParamsLists_0),
    
    {ok, ParamsTitle, ParamsPreview, ParamsLists, ParamsTplsLists}.

map_find(Key, Cont, NoVal) when is_function(NoVal) ->
    case maps:find(Key, Cont) of
        error -> NoVal();
        {ok, Val} -> Val
    end.


get_wscr_params_1() ->
    fun
        (["param", ParamName | List]) ->
            get_wscr_params_1_opt_param(ParamName, List);

        (["adv_params", {adv_term,Dialog,Vals}]) ->
            {adv_params,Dialog,Vals};
        
        (["menu", Str | List]) ->
            get_wscr_params_1_opt_menu(Str, List, menu);
        (["vradio", Str | List]) ->
            get_wscr_params_1_opt_menu(Str, List, vradio);
        (["hradio", Str | List]) ->
            get_wscr_params_1_opt_menu(Str, List, hradio);

        (["checkbox", Str | List]) ->
            get_wscr_params_1_opt_3(Str, List, fun dlg_checkbox_param/3);
        (["browse", Str | List]) ->
            get_wscr_params_1_opt_3(Str, List, fun dlg_browse_param/3);

        (_) ->
            {"???", 0}
    end.

get_wscr_params_1_opt_param(ParamName, [ParamDefault, ParamKey | _]) ->
    {ParamName, get_wscr_param_number(ParamDefault),
        [{key,list_to_atom(ParamKey)}]};
get_wscr_params_1_opt_param(ParamName, [ParamDefault]) ->
    {ParamName, get_wscr_param_number(ParamDefault)}.


get_wscr_params_1_opt_3(Str, [Default, Opts_S], F) ->
    F(Str, Default, Opts_S);
get_wscr_params_1_opt_3(Str, [Default], F) ->
    F(Str, Default, "");
get_wscr_params_1_opt_3(Str, [], F) ->
    F(Str, "", "").

get_wscr_params_1_opt_menu(Str, [Default_S, Opts_S, [[_|_]=_|_]=List], Atom) ->
    dlg_menu_param(Atom, Str, Default_S, Opts_S, List);
get_wscr_params_1_opt_menu(Str, [Default_S, [[_|_]=_|_]=List], Atom) ->
    dlg_menu_param(Atom, Str, Default_S, "", List);
get_wscr_params_1_opt_menu(Str, [[[_|_]=_|_]=List], Atom) ->
    dlg_menu_param(Atom, Str, dlg_menu_param_first_item(List), "", List).


dlg_menu_param(Type, Str, Default_S, Opts_S, List) ->
    Default = dlg_menu_item_val(Default_S),
    L2 = [dlg_menu_item(A) || A <- List],
    case string:tokens(Opts_S, ",") of
        [] ->
            {menu,{Type,L2},Str,Default};
        Opts1 when is_list(Opts1) ->
            {menu,{Type,L2},Str,Default,dlg_menu_opts(Opts1)}
    end.

dlg_menu_param_first_item([["item",_,Val]=_|_]=_) ->
    dlg_menu_item_val(Val).

dlg_checkbox_param(Str, Default_S, Opts_S) ->
    Default = dlg_menu_item_bool(Default_S),
    case string:tokens(Opts_S, ",") of
        [] ->
            {checkbox,{checkbox},Str,Default};
        Opts1 when is_list(Opts1) ->
            {checkbox,{checkbox},Str,Default,dlg_menu_opts(Opts1)}
    end.

dlg_browse_param(Str, Default, Opts_S) ->
    case string:tokens(Opts_S, ",") of
        [] ->
            {browse,{browse},Str,Default};
        Opts1 when is_list(Opts1) ->
            {browse,{browse},Str,Default,dlg_menu_opts(Opts1)}
    end.

dlg_menu_opts(Opts) ->
    [begin
        case string:split(O,"=") of
            [Key] ->
                {key,list_to_atom(Key)};
            [Key,Val] ->
                case lists:member(Key, ["key", "dialog_type"]) of
                    true ->
                        {list_to_atom(Key),list_to_atom(Val)};
                    _ -> %% "title"
                        {list_to_atom(Key),Val}
                end
        end
     end || O <- Opts].

dlg_menu_item(["item", Str, Val]) ->
    {Str, dlg_menu_item_val(Val)};
dlg_menu_item(_) ->
    false.

dlg_menu_item_bool(Val)
  when is_list(Val) ->
    case string:lowercase(string:trim(Val)) of
        "t" ++ _ -> true;
        "f" ++ _ -> false;
        "y" ++ _ -> true;
        "n" ++ _ -> false;
        "1" -> true;
        "0" -> false;
        _ -> Val
    end;
dlg_menu_item_bool({atom,Val}) ->
    dlg_menu_item_bool(Val).

dlg_menu_item_val({atom,Val})
  when is_list(Val) ->
    list_to_atom(Val);
dlg_menu_item_val(Val)
  when is_list(Val) ->
    case string:to_float(Val) of
        {Num, []} -> Num;
        _ -> dlg_menu_item_val_1(Val)
    end.
dlg_menu_item_val_1(Val)
  when is_list(Val) ->
    case string:to_integer(Val) of
        {Num, []} -> Num;
        _ -> dlg_menu_item_val_2(Val)
    end.
dlg_menu_item_val_2(Val)
  when is_list(Val) ->
    F = fun (C) when C >= $a, C =< $z;
                     C >= $A, C =< $Z;
                     C >= $0, C =< $9;
                     C =:= $_        -> true;
            (_) -> false
    end,
    case lists:all(F, Val) of
        true ->
            list_to_atom(Val);
        _ ->
            Val
    end.


get_wscr_import_export_params(Cont_0) ->
    Cont = wscr_map(Cont_0),
    ExtensionsLists = map_find(extensions, Cont, fun () -> [] end),
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
    Title = proplists:get_value(title, ChooserPs, ?__(1,"Choose File")),
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
to_dialog_params([{A,Val,C} | List], State, Dict, OList)
  when is_list(A) ->
    to_dialog_params(List, State, Dict,
        [{A, to_dialog_param_1(Val, State, Dict), C}
        | OList]);
to_dialog_params([{A,Val} | List], State, Dict, OList)
  when is_list(A) ->
    to_dialog_params(List, State, Dict,
        [{A, to_dialog_param_1(Val, State, Dict)}
        | OList]);
to_dialog_params([{adv_params,Tuple,Vals} | List], State, Dict, OList)
  when is_list(Vals) ->
    to_dialog_params(List, State, Dict,
        [{adv_params,Tuple,[{Label,to_dialog_param_1({etp,Val}, State, Dict)} || {Label,Val} <- Vals]}
        | OList]);
to_dialog_params([{Atom,AtomSet,A,Val,C} | List], State, Dict, OList)
  when is_list(A),is_atom(Atom),is_tuple(AtomSet) ->
    to_dialog_params(List, State, Dict,
        [{Atom,AtomSet,A, to_dialog_param_1(Val, State, Dict), C}
        | OList]);
to_dialog_params([{Atom,AtomSet,A,Val} | List], State, Dict, OList)
  when is_list(A),is_atom(Atom),is_tuple(AtomSet) ->
    to_dialog_params(List, State, Dict,
        [{Atom,AtomSet,A, to_dialog_param_1(Val, State, Dict)}
        | OList]);
to_dialog_params([], _, _, OList) ->
    lists:reverse(OList).

to_dialog_param_1(Number, _State, _Dict)
  when is_integer(Number);
       is_float(Number);
       is_atom(Number) ->
    Number;
to_dialog_param_1(PV, State, Dict) ->
    {Val, _} = crun_pv(PV, State, Dict),
    Val.


append_extra_files(ExtraFiles, ParamsSetVars) ->
    PSScriptParams = get_map(script_params, ParamsSetVars),
    List1 = append_extra_files_1(ExtraFiles, PSScriptParams),
    [[binstr(K), V] || {K, V} <- List1].
append_extra_files_1(ExtraFiles, PSScriptParams) ->
    [{K1,{string,V1}} || {K1,V1} <- ExtraFiles] ++ maps:to_list(PSScriptParams).


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
            PSScriptParams_1 = append_extra_files(ExtraFiles, ParamsSetVars),

            ScrSt = #{script_type=>ScriptType,
              script_filename=>ScriptFileName,
              script_params=>ScriptParams,
              more_params=>PSScriptParams_1,
              settings=>get_settings_for_run_script()},
            case run_script_once(keep, ScrSt) of
                {ok, {{new_shape, Pfx, #e3d_object{}=Obj, Mat}, _}} ->
                    %% The e3d_file returned needs to be adjusted.
                    TempFolder = temp_folder(),
                    {#e3d_file{mat=Mat_1}=_, TempFiles} =
                        e3df_from_script(#e3d_file{mat=Mat}, TempFolder, []),
                    delete_temps(TempFolder, TempFiles),
                    {new_shape, Pfx, Obj, Mat_1};
                {ok, {Return, _}} ->
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
        fun(Res) -> {Op,{command_from_script, {CommandRec, Res}}} end);
command_askdialog(
  Params, Title, true, SettableParams, State_1, Dict, Templates, Op,
  #command_rec{scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St) ->
    case run_script_w_preview(ScriptType, ScriptFileName,
        get_settings_for_run_script())
    of {ok, FP} ->
        askdialog_w_prev(Params, Title,
            to_dialog_params(SettableParams, State_1, Dict), Templates,
            fun(Res) -> {Op,{command_from_script, {CommandRec, Res}}} end,
            St, FP)
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
            
            %%  change_points scripts can be in either 'body' or 'face' selection mode.
            %%  When in 'body' mode, the whole mesh is changed.
            %%  When in 'face' mode, selected contains the points inside the face
            %%  selection (to figure out: if all points of face is included, or only those
            %%  inside the region selection (all points, less points of the outer edges).
            %%
            {CmdInputs,CmdChanges} = wscr_command(WSCRContent),
            PSScriptParams_1 = append_extra_files(ExtraFiles, ParamsSetVars),
            CmdType = {mapfold, CmdInputs, CmdChanges},
            command_from_script_1(
                CmdType, ScriptType, ScriptFileName,
                Op, ScriptParams, PSScriptParams_1, St)
    end.


%% Using wings_sel:mapfold/3, the script is called for each We0 object,
%% and a list of changes is applied to We0.
command_from_script_1({mapfold, CmdInputs, CmdChanges},
                      ScriptType, ScriptFileName,
                      Op, ScriptParams, PSScriptParams_1,
                      #st{selmode=SelMode}=St) ->
    {St1, Changed} = wings_sel:mapfold(fun (Items, We0, Changed_0) ->
        CmdExtraParams = command_extra_params(CmdInputs, We0),

        ScrSt = #{script_type=>ScriptType,
            script_filename=>ScriptFileName,
            script_params=>ScriptParams,
            more_params=>[
                [<<"op">>, Op],
                [<<"selmode">>, SelMode],
                [<<"sel">>, gb_sets:to_list(Items)]
                | CmdExtraParams ++ PSScriptParams_1],
            settings=>get_settings_for_run_script(),
            we=>We0,prev_we_stack=>[],st=>St},
        case run_script_once(error, ScrSt) of
            {ok, {Return, #{we:=We1}}} ->
                Changed_1 = if We0 =/= We1 -> true; true -> Changed_0 end,
                SelChanges = sel_changes(SelMode,Items,We0,We1),
                command_modifications(Return, CmdChanges, SelChanges, We1, Changed_1);
            {error, Err} ->
                err(io_lib:format("~p: ~p", [ScriptFileName, Err])),
                {We0, Changed_0}
        end
    end, false, St),
    case Changed of
        true ->
            {save_state, update_selection(St,St1)};
        false ->
            St
    end.

%% Update the selection in #st{}.
%%
update_selection(#st{selmode=SelMode,sel=S1,shapes=Shs1}=_St1,#st{shapes=Shs2}=St2) ->
    S2 = update_selection_1(SelMode, S1, Shs1, Shs2, []),
    St2#st{sel=S2}.
update_selection_1(SelMode, [{Id,Sel}|S1], Shs1, Shs2, S2) ->
    #we{}=We1 = gb_trees:get(Id,Shs1),
    #we{}=We2 = gb_trees:get(Id,Shs2),
    Sel2 = update_selection_2(SelMode, Sel, We1, We2),
    update_selection_1(SelMode, S1, Shs1, Shs2, [{Id,Sel2}|S2]);
update_selection_1(_SelMode, [], _Shs1, _Shs2, S2) ->
    lists:reverse(S2).

update_selection_2(body,Sel,_,_) ->
    Sel;
update_selection_2(SelMode,Sel,We0,We1) ->
    sel_changes_1(SelMode,Sel,We0,We1).


%% Get selection and new items to apply changes from
%% set_points, set_face_uvs, set_face_colors
%%
sel_changes(edge,Items,We0,We1) ->
    Items2 = sel_changes_1(edge,Items,We0,We1),
    {edge,wings_face:from_edges(Items,We1),wings_vertex:from_edges(Items2,We1)};
sel_changes(face,Items,We0,We1) ->
    Items2 = sel_changes_1(face,Items,We0,We1),
    {face,Items,wings_vertex:from_faces(Items2,We1)};
sel_changes(body,_Items,_We0,#we{vp=Vtab,fs=Ftab}=_We1) ->
    Items1 = gb_sets:from_list(gb_trees:keys(Ftab)),
    Vs = orddict:fetch_keys(array:sparse_to_orddict(Vtab)),
    {body,Items1,gb_sets:from_list(Vs)};
sel_changes(SelMode,Items,We0,We1) ->
    Items2 = sel_changes_1(SelMode,Items,We0,We1),
    {SelMode,Items2,Items2}.

%% Get difference in selected items 
%%
sel_changes_1(edge,Items,We0,#we{es=Etab}=We1) ->
    Etab1 = gb_sets:from_list(orddict:fetch_keys(array:sparse_to_orddict(Etab))),
    Items1 = wings_we:new_items_as_gbset(edge, We0, We1),
    Items2 = gb_sets:union(gb_sets:intersection(Items,Etab1),Items1),
    Items2;
sel_changes_1(face,Items,We0,#we{fs=Ftab}=We1) ->
    Ftab1 = gb_sets:from_list(gb_trees:keys(Ftab)),
    Items1 = wings_we:new_items_as_gbset(face, We0, We1),
    Items2 = gb_sets:union(gb_sets:intersection(Items,Ftab1),Items1),
    Items2;
sel_changes_1(SelMode,Items,We0,#we{vp=Vtab}=We1) ->
    Vs = gb_sets:from_list(orddict:fetch_keys(array:sparse_to_orddict(Vtab))),
    Items1 = wings_we:new_items_as_gbset(SelMode, We0, We1),
    gb_sets:union(gb_sets:intersection(Items,Vs),Items1).




wscr_command(Cont_0) ->
    Cont = wscr_map(Cont_0),
    CmdChanges_0 = map_find(command_changes, Cont, fun () -> "" end),
    CmdChanges_1 = string:tokens(CmdChanges_0, " ,"),
    CmdInputs_0 = map_find(command_inputs, Cont, fun () -> CmdChanges_0 end),
    CmdInputs_1 = string:tokens(CmdInputs_0, " ,"),
    CmdChanges = wscr_command_1(CmdChanges_1),
    CmdInputs = wscr_command_1(CmdInputs_1),
    {CmdInputs, gb_sets:from_list(CmdChanges)}.
wscr_command_1(List) ->
    wscr_command_1(List, []).
wscr_command_1([A|List], OL) ->
    A_1 = case A of
        "points" -> points;
        "faces" -> faces;
        "face_colors" -> face_colors;
        "face_uvs" -> face_uvs;
        "e3d_mesh" -> e3d_mesh;
        _ -> unknown
    end,
    wscr_command_1(List, [A_1|OL]);
wscr_command_1([], OL) ->
    [A || A <- lists:reverse(OL), A =/= unknown].


%% Choose extra parameters to send to command scripts
%%
command_extra_params(R, We0) ->
    command_extra_params(R, We0, []).
command_extra_params([points | R], #we{vp=Vtab}=We0, EP) ->
    Vtab_1 = array:sparse_to_orddict(Vtab),
    %% Remove the undefined atoms before sending it to the script.
    P = [<<"points">>, Vtab_1],
    command_extra_params(R, We0, [P | EP]);
command_extra_params([faces | R], #we{fs=Ftab}=We0, EP) ->
    FL = gb_trees:keys(Ftab),
    Faces = [{FIdx, {
        wings_face:vertices_ccw(FIdx, We0),
        bool_none(wings_va:face_attr(color, FIdx, We0)),
        bool_none(wings_va:face_attr(uv, FIdx, We0))
    }} || FIdx <- FL],
    P = [<<"faces">>, Faces],
    command_extra_params(R, We0, [P | EP]);
command_extra_params([face_colors | R], #we{fs=Ftab}=We0, EP) ->
    FL = gb_trees:keys(Ftab),
    Colors = [{FIdx, bool_none(wings_va:face_attr(color, FIdx, We0))}
        || FIdx <- FL],
    P = [<<"face_colors">>, Colors],
    command_extra_params(R, We0, [P | EP]);
command_extra_params([face_uvs | R], #we{fs=Ftab}=We0, EP) ->
    FL = gb_trees:keys(Ftab),
    UVs = [{FIdx, bool_none(wings_va:face_attr(uv, FIdx, We0))}
        || FIdx <- FL],
    P = [<<"face_uvs">>, UVs],
    command_extra_params(R, We0, [P | EP]);
command_extra_params([e3d_mesh | R], #we{}=We0, EP) ->
    Mesh = sendout_mesh(We0),
    P = [<<"e3d_mesh">>, Mesh],
    command_extra_params(R, We0, [P | EP]);
command_extra_params([], _We0, EP) ->
    lists:reverse(EP).


%% Modify the We structure with modifications returned from the command script
%%
command_modifications(Returned, Changeable, SelChanges, We0, Changed) ->
    {We1,Changed1} = command_modifications_1(Returned, Changeable, SelChanges, {We0, false}),
    {We1,Changed1 or Changed}.
command_modifications_1([[set_points, Vtab_2] | R], Changeable, SelChanges, Ret0) ->
    Ret1 = case gb_sets:is_element(points, Changeable) of
        false ->
            Ret0;
        true ->
            command_modifications_2(set_points, Vtab_2, SelChanges, Ret0)
    end,
    command_modifications_1(R, Changeable, SelChanges, Ret1);
command_modifications_1([[set_face_uvs, Vtab_2] | R], Changeable, SelChanges, Ret0) ->
    Ret1 = case gb_sets:is_element(face_uvs, Changeable) of
        false ->
            Ret0;
        true ->
            command_modifications_2(set_face_uvs, Vtab_2, SelChanges, Ret0)
    end,
    command_modifications_1(R, Changeable, SelChanges, Ret1);
command_modifications_1([[set_face_colors, Vtab_2] | R], Changeable, SelChanges, Ret0) ->
    Ret1 = case gb_sets:is_element(face_colors, Changeable) of
        false ->
            Ret0;
        true ->
            command_modifications_2(set_face_colors, Vtab_2, SelChanges, Ret0)
    end,
    command_modifications_1(R, Changeable, SelChanges, Ret1);
command_modifications_1([[set_e3d_mesh, Mesh_1] | R], Changeable, SelChanges, Ret0) ->
    Ret1 = case gb_sets:is_element(e3d_mesh, Changeable) of
        false ->
            Ret0;
        true when is_record(Mesh_1, e3d_mesh) ->
            command_modifications_2(set_e3d_mesh, Mesh_1, SelChanges, Ret0);
        _ ->
            %% Returned face_colors should be exactly the same length as input
            warn(?__(4,"Returned e3d_mesh is not a valid record'.")),
            Ret0
    end,
    command_modifications_1(R, Changeable, SelChanges, Ret1);

command_modifications_1([], _Changeable, _SelChanges, {_We0, false}=Ret) ->
    ?DEBUG_FMT("Did not change:~p~n", [Ret]),
    Ret;
command_modifications_1([], _Changeable, _SelChanges, Ret) ->
    Ret.


command_modifications_2(set_points, Vtab_2, {_,_,SelVs}, {#we{vp=Vtab_1_0}=We0, _}) ->
    Vtab_1 = array:sparse_to_orddict(Vtab_1_0),
    Vtab_2_1 = array:from_orddict(Vtab_2),
    Vtab_3 = orddict:map(
        fun (Idx,Val) ->
            case gb_sets:is_element(Idx,SelVs) of
                true ->
                    Val0 = array:get(Idx,Vtab_2_1),
                    if Val0 =/= undefined -> Val0; true -> Val end;
                false ->
                    Val
            end
        end, Vtab_1),
    We1 = We0#we{vp=array:from_orddict(Vtab_3)},
    {We1, true};
command_modifications_2(set_face_uvs, Vtab_2, {SelMode,SelFaces,_}, {#we{fs=Ftab}=We0, _})
  when SelMode =:= face; SelMode =:= body ->
    We1 = lists:foldl(fun({FNum, UVList}, W) ->
        case gb_sets:is_element(FNum, SelFaces) of
            true ->
                case gb_trees:is_defined(FNum, Ftab) of
                    true ->
                        wings_va:set_face_attr_vs(uv, FNum, unbool_none(UVList), W);
                    _ ->
                        W
                end;
            false ->
                W
        end
    end, We0, Vtab_2),
    {We1, true};
command_modifications_2(set_face_colors, Vtab_2, {SelMode,SelFaces,_}, {#we{fs=Ftab}=We0, _})
  when SelMode =:= face; SelMode =:= body ->
    We1 = lists:foldl(fun({FNum, ColorList}, W) ->
        case gb_sets:is_element(FNum, SelFaces) of
            true ->
                case gb_trees:is_defined(FNum, Ftab) of
                    true ->
                        wings_va:set_face_attr_vs(color, FNum, unbool_none(ColorList), W);
                    _ ->
                        W
                end;
            _ ->
                W
        end
    end, We0, Vtab_2),
    {We1, true};
command_modifications_2(set_e3d_mesh, Mesh_1, {body,_,_}, {We0, _}) ->
    We1 = merge_we_from_e3d(We0, returned_mesh(Mesh_1)),
    {We1, true};
command_modifications_2(Change, _Mesh_1, _, Ret0) ->
    io:format(?__(1,"~w: Returned '~w' values cannot be used to change "
                    "because of the selection mode."), [?MODULE, Change]),
    Ret0.



merge_we_from_e3d(We0, We2) ->
    #we{
        es=W1Es,lv=W1Lv,rv=W1Rv,fs=W1Fs,he=W1He,vc=W1Vc,vp=W1Vp,
        mat=W1Mat,next_id=W1NextId,holes=W1Holes} = We2,
    We0#we{
        es=W1Es,lv=W1Lv,rv=W1Rv,fs=W1Fs,he=W1He,vc=W1Vc,vp=W1Vp,
        mat=W1Mat,next_id=W1NextId,mirror=none,holes=W1Holes}.


sendout_mesh(We0) ->
    wings_export:make_mesh(We0, []).

returned_mesh(#e3d_mesh{}=Mesh0) ->
    Mesh1 = e3d_mesh:merge_vertices(Mesh0),
    Mesh2 = e3d_mesh:clean_faces(Mesh1),
    Mesh3 = e3d_mesh:transform(Mesh2),
    Mesh  = e3d_mesh:hard_edges_from_normals(Mesh3),
    We0 = wings_import:import_mesh(material, Mesh),
    We0.

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
            ImportParams_0 = get_map(import_params, ParamsSetVars),
            {ScrSpecPs, ImportParams} = lists:partition(
                fun script_specific_param/1, maps:to_list(ImportParams_0)),
            
            case get_wscr_import_export_params(WSCRContent) of
                {ok, Extensions, Templates} ->
                    Props = [{extensions, Extensions} | ImportParams],
                    wpa:import(Props, fun(F) ->
                        PSScriptParams_1 = append_extra_files(ExtraFiles, ParamsSetVars),
                        TempFolder = temp_folder(),

                        ScrSt = #{script_type=>ScriptType,
                          script_filename=>ScriptFileName,
                          script_params=>ScriptParams,
                          more_params=>[ [<<"filename">>, binstr(F)],
                              [<<"temp_folder">>, binstr(TempFolder)]
                              | PSScriptParams_1],
                          settings=>get_settings_for_run_script()},
                        case run_script_once(error, ScrSt) of
                            {ok, {ReturnedE3D, _}} ->
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
import_export_from_script(Op, ScriptParams,
    #command_rec{wscrcont=WSCRContent,
                 scrfile=ScriptFileName,scrtype=ScriptType}=CommandRec, St)
  when Op =:= export; Op =:= export_selected ->
    case fill_extra_file_inputs(find_extra_file_sections(WSCRContent), CommandRec, St, fun (CommandRec_1) ->
            {file, {Op, {import_export_from_script, {CommandRec_1, ScriptParams}}}}
    end) of
        {file_inputs, ExtraFiles} ->
            Dict = orddict:from_list([{"st", St}, {"params", ScriptParams}]),
            {ParamsSetVars, _} = crun_section("params_set", WSCRContent, Dict),
            ExportParams_0 = get_map(export_params, ParamsSetVars),
            {ScrSpecPs, ExportParams} = lists:partition(
                fun script_specific_param/1, maps:to_list(ExportParams_0)),
            
            case get_wscr_import_export_params(WSCRContent) of
                {ok, Extensions, Templates} ->
                    Props = [{extensions, Extensions} | ExportParams],
                    wpa:Op(Props, fun(F, E3DCont_00) ->
                        E3DCont_0 = mesh_export_changes(export_transform(E3DCont_00, ScriptParams, Templates), Props),
                        E3DCont = export_texture_options(E3DCont_0, F, ScriptParams, ScrSpecPs),
                        
                        PSScriptParams_1 = append_extra_files(ExtraFiles, ParamsSetVars),
                        TempFolder = temp_folder(),
                        {E3DCont_1, TempFiles} = e3df_filename_tup(E3DCont, TempFolder, ScrSpecPs),

                        ScrSt = #{script_type=>ScriptType,
                          script_filename=>ScriptFileName,
                          script_params=>ScriptParams,
                          more_params=>[ [<<"filename">>, binstr(F)],
                              [<<"content">>, E3DCont_1],
                              [<<"temp_folder">>, binstr(TempFolder)]
                              | PSScriptParams_1],
                          settings=>get_settings_for_run_script()},
                        case run_script_once(error, ScrSt) of
                            {ok, {Return, _}} ->
                                delete_temps(TempFolder, TempFiles),
                                ?DEBUG_FMT("RETURNED: ~p~n", [Return]),
                                case Return of
                                    {ok} -> ok;
                                    [ok] -> ok;
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
e3df_filename_tup(#e3d_file{objs=Objs,creator=Creator,dir=Dir,mat=Mats}=E3DCont_0, TempFolder, ScrSpecPs) ->
    {NewMats, Temps} = lists:foldl(e3df_filename_tup_fun(TempFolder, ScrSpecPs), {[], []}, Mats),
    E3DCont = E3DCont_0#e3d_file{
        objs=[e3df_filename_tup_object(E3DO) || E3DO <- Objs],
        mat=lists:reverse(NewMats),
        creator=stringtup_if_not_atom(Creator),
        dir=stringtup_if_not_atom(Dir)
    },
    {E3DCont, Temps}.
e3df_filename_tup_fun(TempFolder, ScrSpecPs) ->
    fun({Id, MatAL}, {L1_Mat, L1_Temp}) ->
        ListMaps = proplists:get_value(maps, MatAL, []),
        
        {NewListMaps, NewTemps} = lists:foldl(fun ({MapType, MapVal}, {A_Mat, A_Temp}) ->
            case e3df_image(MapVal, length(A_Mat), TempFolder, ScrSpecPs) of
                {temp, File, T} -> {[{MapType, T} | A_Mat], [File | A_Temp]};
                {no_temp, T}    -> {[{MapType, T} | A_Mat], A_Temp}
            end
        end, {[], []}, ListMaps),
        
        {[{Id, orddict:store(maps, NewListMaps, orddict:from_list(MatAL))} | L1_Mat], NewTemps ++ L1_Temp}
    end.
e3df_filename_tup_object(#e3d_object{name=Name}=E3DO) ->
    E3DO#e3d_object{name=stringtup_if_not_atom(Name)}.
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
    ok = e3d__bmp:save(E3DImg, TempFile, []),
    {ok, TempFile};
e3df_image_conv(".tga", TempFile_0, E3DImg) ->
    TempFile = TempFile_0 ++ ".tga",
    ok = e3d__tga:save(E3DImg, TempFile, []),
    {ok, TempFile};
e3df_image_conv(bytes, TempFile, #e3d_image{image=ImageBin}=_) ->
    ok = file:write_file(TempFile, ImageBin),
    {ok, TempFile}.

stringtup_if_not_atom(Str) when is_atom(Str) -> Str;
stringtup_if_not_atom(Str) when is_list(Str) ->
    {string, liststr_to_binstring(Str)}.



e3df_from_script(#e3d_file{mat=Mats}=ReturnedE3D_0, TempFolder, ScrSpecPs) ->
    {NewMats, Temps} = lists:foldl(e3df_from_script_fun(TempFolder, ScrSpecPs), {[], []}, Mats),
    ReturnedE3D = ReturnedE3D_0#e3d_file{mat=NewMats},
    {ReturnedE3D, Temps}.
e3df_from_script_fun(TempFolder, ScrSpecPs) ->
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
    end.
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
            warn(io_lib:format("Filename not found: ~p", [TempFile_1])),
            error(e3d_image_raw_filename_not_found)
    end;
e3df_from_script_image_1(Conv, #e3d_image{filename=TempFile_1}=T, _TempFolder) ->
    case file:read_file_info(TempFile_1) of
        {ok, _} ->
            T_1 = e3df_from_script_image_conv(Conv, T, TempFile_1),
            {no_temp, T_1};
        _ ->
            warn(io_lib:format("Filename not found: ~p", [TempFile_1])),
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
    AllEng = scripting_engines:all_engines(),
    lists:append([
        get_settings_for_run_script_eng(Eng, HasArgs)
    || {Eng, HasArgs} <- AllEng ]).

get_settings_for_run_script_eng(_, no_bin) -> [];
get_settings_for_run_script_eng(Eng, HasArgs)
  when is_boolean(HasArgs) ->
    Atom = list_to_atom("setting_" ++ Eng ++ "_int_path"),
    [{Atom, wpa:pref_get(?MODULE, Atom, "")}] ++
    get_settings_for_run_script_eng_1(Eng, HasArgs).

get_settings_for_run_script_eng_1(_, false) -> [];
get_settings_for_run_script_eng_1(Eng, true) ->
    Atom = list_to_atom("setting_" ++ Eng ++ "_arguments"),
    [{Atom, wpa:pref_get(?MODULE, Atom, "")}].

%%%
%%%

%%
%% Plugin Scripts
%%
%% Plugin scripts are scripts that get added into wing's menu
%% system so they can be accessed the same way as plugins, this
%% is done by having a config file that points to the wscr file,
%% and some information of the menu to create for the script.
%%

%% Example plugin script .conf file:
%% 
%% {"com.example.my.plugin.1", [
%%     {wscr, "my-plugin.wscr"},
%%     {menu, [
%%         {[tools, newsubmenu, newnewsubmenu, myplugin], []}
%%     ]}
%% ]}.
%% 
%% {strings, [
%%     {plugin, "com.example.my.plugin.1"},
%%     {"en", [
%%         {{plugin,name}, "My Plugin"},
%%         {{menu,newsubmenu}, "New Submenu"},
%%         {{menu,newnewsubmenu}, "New new Submenu"},
%%         {{menu,myplugin}, "My Plugin"},
%%         {{info,myplugin}, "My Plugin menu tooltip"}
%%     ]}
%% ]}.
%% 


pluginscripts_make_menu(Tuple, Menu, L) ->
    case proplists:get_value(menus, L, undefined) of
        Menus when is_map(Menus) ->
            case maps:find(Tuple, Menus) of
                {ok, List} when is_list(List) ->
                    Menu ++ [Do || {_,Do} <- List];
                _ ->
                    Menu
            end;
        _ ->
            Menu
    end.

pluginscripts_command({Op0,_}=Tuple, St, L) ->
    Op = case Op0 of
        {file, {Op1, _}} -> Op1;
        _ -> Op0
    end,
    case proplists:get_value(command_find, L) of
        Start when is_map(Start) ->
            pluginscripts_command_1(Tuple, Start, L, Op, St);
        _ ->
            next
    end.
pluginscripts_command_1({Item,{{wscr,folder},WSCRFile}}, C, _L, Op, St) ->
    case maps:find(Item, C) of
        {ok, _Next} ->
            select_script(script_info(WSCRFile, Op), from_script_fun(Op, St));
        _ ->
            next
    end;
pluginscripts_command_1({Item,{Item2,_}=Tuple}, C, L, Op, St) when is_atom(Item2) ->
    case maps:find(Item, C) of
        {ok, Next} ->
            pluginscripts_command_1(Tuple, Next, L, Op, St);
        _ ->
            next
    end;
pluginscripts_command_1({Item,Action}, C, L, Op, St) when is_atom(Action) ->
    case maps:find(Item, C) of
        {ok, _} when Action =:= ignore ->
            keep;
        {ok, Next} ->
            case maps:find(Action, Next) of
                {ok, Plugin} ->
                    pluginscripts_command_2(Plugin, L, Op, St);
                _ ->
                    next
            end;
        _ ->
            next
    end;
pluginscripts_command_1(_, _C, _L, _Op, _St) ->
    next.

pluginscripts_command_2({Plugin,_Ask}, L, Op, St) when is_atom(Plugin) ->
    case command_map_find(plugins, Plugin, L) of
        {ok, WSCRFile} ->
            select_script(script_info(WSCRFile, Op), from_script_fun(Op, St));
        _ ->
            next
    end.

command_map_find(Atom, Find, L) ->
    case proplists:get_value(Atom, L, undefined) of
        M when is_map(M) ->
            maps:find(Find, M);
        _ ->
            undefined
    end.


-record(pluginscripts,{
    list=[],
    menus=[],
    strings=[]
}).

get_pluginscripts(Dirnames) ->
    get_pluginscripts(Dirnames, #pluginscripts{}).
get_pluginscripts([Dirname|L], Acc0) ->
    case file:list_dir(Dirname) of
        {ok, List1} when is_list(List1) ->
            Acc2=lists:foldl(
                fun(F, Acc1) ->
                    read_conf(filename:join(Dirname,F), Acc1, fun pluginscripts_conf/3)
                end, Acc0, List1),
            get_pluginscripts(L, Acc2);
        _ ->
            get_pluginscripts(L, Acc0)
    end;
get_pluginscripts([], #pluginscripts{list=[]}) ->
    {[], []};
get_pluginscripts([], #pluginscripts{list=L1,menus=L3,strings=L2}) ->
    Strings = maps:from_list(
        [{Key, maps:from_list(Val)}
            || {Key,Val} <- L2]),

    L3_1 = lists:foldl(fun ({Plugin, [{Menu,Opts}]}, Acc) ->
        [{Menu, {Opts, Plugin, maps:get(Plugin, Strings)}}|Acc]
    end, [], L3),
    
    {[{plugins, maps:from_list(L1)}], L3_1}.

get_pluginscripts_1(L3_0, L) ->
    L3 = lists:usort(L3_0),
    %% Generate Menu Constructions
    MenuC = pluginscripts_menuc(L3),
    %% Generate Command Finders
    CommandFinder = pluginscripts_cmdf(L3),
    [
        {menus, maps:from_list(MenuC)},
        {command_find, CommandFinder}
    | L ].


%% Menu Constructions
pluginscripts_menuc(L3) ->
    MenuC_0 = lists:foldl(fun ({Menu, {Opts, Plugin, PluginStrings}}, Acc) ->
        Ret = pluginscripts_menus(
            Menu, {Opts, Plugin, PluginStrings}),
        [[T || T <- Ret, is_tuple(T)]|Acc]
    end, [], L3),
    MenuC = lists:usort(lists:append(MenuC_0)),
    pluginscripts_menuc_1(MenuC, []).

%% Regroup commands together into lists for each submenu.
pluginscripts_menuc_1([{A,B1}|L], [{A,B2}|OL]) ->
    pluginscripts_menuc_1(L, [{A,[B1|B2]}|OL]);
pluginscripts_menuc_1([{A,B1}|L], OL) ->
    pluginscripts_menuc_1(L, [{A,[B1]}|OL]);
pluginscripts_menuc_1([], OL) ->
    lists:reverse([{A,lists:reverse(B)} || {A,B} <- OL]).

%% Generate command finders, which is a hierarchical map of
%% submenu atoms that is matched against when command/2 is
%% called.
pluginscripts_cmdf(L3) ->
    lists:foldl(fun ({Menu, {Opts, Plugin, _PluginStrings}}, Acc) ->
        pluginscripts_command_find(Menu, Opts, Plugin, Acc)
    end, #{}, L3).

read_conf(Filename, Acc, F) ->
    case filelib:is_regular(Filename) of
        true ->
            Ext = filename:extension(Filename),
            case string:lowercase(Ext) =:= ".conf" of
                true ->
                    {ok, Cont} = file:consult(Filename),
                    F(Cont, filename:dirname(Filename), Acc);
                _ ->
                    Acc
            end;
        _ ->
            Acc
    end.


pluginscripts_conf(Cont, Dirname, Acc0) ->
    F = fun
        ({strings, Info}, #pluginscripts{strings=Strings}=Acc) ->
            Acc#pluginscripts{strings=read_conf_strings(plugin, Info, Strings)};
        ({[C|_]=Name, Info}, Acc) when is_integer(C) ->
            pluginscripts_conf_push(Info, Name, Dirname, Acc)
    end,
    lists:foldl(F, Acc0, Cont).
pluginscripts_conf_push(Info, Name, Dirname, #pluginscripts{list=L1,menus=L2}=Acc) ->
    Atom = list_to_atom(Name),
    %% The .wscr is always a relative path to the .conf file.
    Item={Atom,filename:join(Dirname, proplists:get_value(wscr, Info))},
    MItem={Atom,proplists:get_value(menu, Info)},
    Acc#pluginscripts{list=[Item|L1],menus=[MItem|L2]}.

read_conf_strings(Atom, Info, Strings) ->
    {Langs, Info1} = lists:partition(
        fun
            ({A,_}) when is_list(A) -> true;
            (_) -> false
        end, Info),
    Plugin = proplists:get_value(Atom, Info1),
    orddict:append_list(list_to_atom(Plugin),Langs,Strings).

pluginscripts_menus([Op|_]=MenuList,Command) ->
    CreateLists = pluginscripts_menus_3(MenuList),
    [pluginscripts_menus_2(M,Command) || M <- CreateLists] ++
    [pluginscripts_menus_1(Op, lists:reverse(MenuList),Command)].

pluginscripts_menus_1(Op, [Command|List],{Opts,_Plugin,Strings}) ->
    Lang = current_lang_code(),
    Str1 = case pluginscripts_lang({menu,Command}, Lang, Strings) of
        false -> atom_to_list(Command);
        "" -> atom_to_list(Command);
        Str1_1 -> Str1_1
    end,
    Str2 = case pluginscripts_lang({info,Command}, Lang, Strings) of
        false -> Str1;
        "" -> Str1;
        Str2_1 -> Str2_1
    end,
    Command_1 = case Opts of
        [{folder,FolderList}] ->
            menu_script_folder(Op,Command,Str1,FolderList);
        _ ->
            {Str1, Command, Str2}
    end,
    {list_to_tuple(lists:reverse(List)), {command, Command_1}}.

menu_script_folder(Op,Atom,Name,FolderList) ->
    F = fun
        (help, _Ns) ->
            {?__(1,"Open Script Folder"),
             "",
             ""};
        (1, _Ns) -> menu_script_folder_1(Op,Atom,FolderList);
        (2, _Ns) -> ignore;
        (3, _Ns) -> menu_script_folder_1(Op,Atom,FolderList)
    end,
    {Name,{Atom,F}}.

menu_script_folder_1(Op,Atom,FolderList) ->
    List3 = lists:foldl(fun({_Dir, SList},List1) ->
        lists:foldl(fun(WSCR, List2) ->
            [menu_script_folder_2(Op,Atom,WSCR)|List2]
        end, List1, SList)
    end, [], read_script_dirs(FolderList, Op)),
    case List3 of
        [] ->
            [{?__(1,"No scripts available"),ignore,?__(2,"None")}];
        _ ->
            List3
    end.

menu_script_folder_2(Op,Atom,{WSCRFile, {_ScriptFile, _ScriptType, ScriptName, ScriptDesc}}) ->
    F = fun
        (help, _Ns) ->
            {ScriptDesc,
             "",
             ""};
        (1, _Ns) -> {Op,{Atom,{{wscr,folder},WSCRFile}}};
        (2, _Ns) -> ignore;
        (3, _Ns) -> ignore
    end,
    {ScriptName,{Atom,F}}.



%% Submenus
pluginscripts_menus_2([_],_) ->
    false;
pluginscripts_menus_2([SubMenu|List_1],{_Opts,_Plugin,Strings}) ->
    Lang = current_lang_code(),
    Str1 = case pluginscripts_lang({menu,SubMenu}, Lang, Strings) of
        false -> atom_to_list(SubMenu);
        Str1_1 -> Str1_1
    end,
    {list_to_tuple(lists:reverse(List_1)), {submenu, {Str1,{SubMenu,[]}}}}.

pluginscripts_menus_3(List) ->
    [_|List1] = lists:reverse(List),
    pluginscripts_menus_3(List1, []).
pluginscripts_menus_3([_|List]=Full, OL) ->
    pluginscripts_menus_3(List, [Full|OL]);
pluginscripts_menus_3([], OL) ->
    OL.

pluginscripts_lang(Key, Lang, Strings) ->
    case pluginscripts_lang_1(Key, Lang, Strings) of
        false ->
            case pluginscripts_lang_1(Key, "en", Strings) of
                false ->
                    false;
                Str ->
                    Str
            end;
        Str ->
            Str
    end.
pluginscripts_lang_1(Key, Lang, Strings) ->
    case maps:find(Lang, Strings) of
        {ok, List} ->
            proplists:get_value(Key, List, false);
        _ ->
            false
    end.


pluginscripts_command_find([M], Opts, Plugin, Acc) ->
    Acc#{M => {Plugin,Opts}};
pluginscripts_command_find([M|Menu], Opts, Plugin, Acc) ->
    case maps:find(M, Acc) of
        {ok, Found} ->
            Acc#{M => pluginscripts_command_find(Menu, Opts, Plugin, Found)};
        _ ->
            Acc#{M => pluginscripts_command_find(Menu, Opts, Plugin, #{})}
    end.

%%%
%%%

%%
%% Script Folders
%% 
%% Script folder files are config files in the user data
%% area where other software or the user can add
%% new paths to find scripts.
%%

%% Example script folder .conf file:
%%
%% {"com.example.wings-scripts-1",[
%%     {type,[shape,face,body]},
%%     {path,["a1/","a2/"]}
%% ]}.
%% 
%% {"com.example.scripts.2",[
%%     {type,[shape]},
%%     {path,["a/"]}
%% ]}.
%% 

-record(scriptfolders,{
    folders_op=[],
    folders=[],
    strings=[]
}).

get_scriptfolders(Dirnames) ->
    get_scriptfolders(Dirnames, #scriptfolders{}).

get_scriptfolders([Dirname|L], Acc0) ->
    case file:list_dir(Dirname) of
        {ok, List} when is_list(List) ->
            Acc2=lists:foldl(
                fun(F, Acc1) ->
                    read_conf(filename:join(Dirname,F), Acc1, fun scriptfolder_conf/3)
                end, Acc0, List),
            get_scriptfolders(L, Acc2);
        _ ->
            get_scriptfolders(L, Acc0)
    end;
get_scriptfolders([], #scriptfolders{folders=[]}) ->
    {[],[]};
get_scriptfolders([], #scriptfolders{folders=L1,folders_op=L2,strings=L3_0}) ->
    L3 = get_scriptfolders_addname(L1, L3_0),

    Strings = maps:from_list(
        [{Key, maps:from_list([{Key2, get_scriptfolders_3(Val2, Key)} || {Key2,Val2} <- Val])}
            || {Key,Val} <- L3]),

    L1_1 = maps:from_list(L1),

    L2_1 = lists:foldl(fun ({Op, List2}, Acc) ->
        lists:foldl(fun (Plugin, Acc2) ->
            #{Plugin:=FolderList}=L1_1,
            [{[Op,Plugin], {[{folder,FolderList}], Plugin, maps:get(Plugin, Strings)}}|Acc2]
        end, Acc, List2)
    end, [], L2),

    {[{folders, L1_1}], L2_1}.

%% Add a directory name if there are no strings.
get_scriptfolders_addname(L1, L3_0) ->
    Lang = current_lang_code(),
    lists:foldl(fun({Key1,[Dir|_]}, L3_1) ->
        case proplists:get_value(Key1, L3_1) of
            undefined ->
                DirName = filename:basename(Dir),
                [{Key1,[
                    {"en",[{{folder,name},DirName}]},
                    {Lang,[{{folder,name},DirName}]}
                    ]}|L3_1];
            [_ | _] ->
                L3_1
        end
    end, L3_0, L1).


get_scriptfolders_3(Val, Name) ->
    lists:map(
        fun
            ({{folder,name},V}) -> {{menu,Name},V};
            ({Key,V}) -> {Key,V}
        end,
        Val).

scriptfolder_conf(Cont, Dirname, Acc0) ->
    F = fun
        ({strings, Info}, #scriptfolders{strings=Strings}=Acc) ->
            Acc#scriptfolders{strings=read_conf_strings(folder, Info, Strings)};
        ({[C|_]=Name, Info}, Acc) when is_integer(C) ->
            List0 = proplists:get_value(path, Info, []),
            Type = proplists:get_value(type, Info, []),
            List = [scriptfolder_path(Dirname, Path) || Path <- List0],
            read_scriptfolder_conf_push(Type, List, list_to_atom(Name), Acc)
    end,
    lists:foldl(F, Acc0, Cont).
read_scriptfolder_conf_push(Type0, List, Name, #scriptfolders{folders=Items}=Acc0)
  when length(List) > 0 ->
    Types = [atom_to_list(Type) || Type <- Type0],
    Item={Name,List},
    lists:foldl(
        fun (Type, #scriptfolders{folders_op=Folders}=Acc) ->
            Atom = list_to_atom(string:lowercase(string:trim(Type))),
            Acc#scriptfolders{folders_op=orddict:append(Atom,Name,Folders)}
        end, Acc0#scriptfolders{folders=[Item|Items]}, Types).

%% Only use relative paths (relative to the .conf file), if the path
%% starts with ./ or ../

scriptfolder_path(Dirname, Path0) ->
    Path=lists:append(string:replace(Path0, "\\", "/", all)),
    scriptfolder_path_1(Dirname, Path).
scriptfolder_path_1(Dirname, "./"++_=Path) ->
    filename:join(Dirname, Path);
scriptfolder_path_1(Dirname, "../"++_=Path) ->
    filename:join(Dirname, Path);
scriptfolder_path_1(_, [C,$:|_]=Path)
  when C >= $A, C =< $Z; C >= $a, C =< $z ->
    Path;
scriptfolder_path_1(_, "/"++_=Path) ->
    Path;
scriptfolder_path_1(_, Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        _ ->
            filename:join(os:getenv("HOME"), Path)
    end.

%%%
%%%


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
    {#{}, State_1};
crun_section(Name, [[Name, W1] | _], Dict, State)
  when is_list(W1) ->
    {_, #crun_state{sett_vars=SettVars}=State_1} = crun_list(W1, State, Dict),
    {SettVars, State_1};
crun_section(Name, [_ | Rest_WSCRCont], Dict, State) ->
    crun_section(Name, Rest_WSCRCont, Dict, State).

find_extra_file_sections(WSCRCont) ->
    find_extra_file_sections("extra_file", WSCRCont).

find_extra_file_sections(Name, WSCRCont) ->
    find_extra_file_sections(Name, WSCRCont, []).

find_extra_file_sections(_, [], OSections) ->
    lists:reverse(OSections);
find_extra_file_sections(Name, [[Name, FileVarName, SubSections] | List], OSections)
  when is_list(SubSections) ->
    Section = extra_file_section(SubSections),
    OSections_1 = [{FileVarName, Section} | OSections],
    find_extra_file_sections(Name, List, OSections_1);
find_extra_file_sections(Name, [_ | Rest_WSCRCont], OSections) ->
    find_extra_file_sections(Name, Rest_WSCRCont, OSections).


extra_file_section(OSections) ->
    extra_file_section(OSections, []).

extra_file_section([], OSections) ->
    lists:reverse(OSections);
extra_file_section([["title", Title] | List], OSections)
  when is_list(Title) ->
    OSections_1 = orddict:store(title, Title, OSections),
    extra_file_section(List, OSections_1);
extra_file_section([["extensions", Extensions] | List], OSections)
  when is_list(Extensions) ->
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

crun("script_param", [K_0, PV], State, Dict) ->
    {K, State_1} = crun_pv(K_0, State, Dict),
    {PVRes, #crun_state{sett_vars=SettVars}=State_2} = crun_pv(PV, State_1, Dict),
    ?DEBUG_FMT("script_param ~s ~p~n", [K, PVRes]),
    ScriptParams = get_map(script_params, SettVars),
    ScriptParams_1 = ScriptParams#{K=>PVRes},
    {PVRes, State_2#crun_state{sett_vars=SettVars#{script_params=>ScriptParams_1}}};

crun("import_param", [K_0, PV], State, Dict) ->
    {K, State_1} = crun_pv(K_0, State, Dict),
    {PVRes, #crun_state{sett_vars=SettVars}=State_2} = crun_pv(PV, State_1, Dict),
    %% Add to wings:import parameters key K from path value PV
    ?DEBUG_FMT("import_param ~s ~p~n", [K, PVRes]),
    ImportParams = get_map(import_params, SettVars),
    ImportParams_1 = ImportParams#{list_to_atom(K)=>PVRes},
    {PVRes, State_2#crun_state{sett_vars=SettVars#{import_params=>ImportParams_1}}};

crun("export_param", [K_0, PV], State, Dict) ->
    {K, State_1} = crun_pv(K_0, State, Dict),
    {PVRes, #crun_state{sett_vars=SettVars}=State_2} = crun_pv(PV, State_1, Dict),
    %% Add to wings:export parameters key K from path value PV
    ?DEBUG_FMT("export_param ~s ~p~n", [K, PVRes]),
    ExportParams = get_map(export_params, SettVars),
    ExportParams_1 = ExportParams#{list_to_atom(K)=>PVRes},
    {PVRes, State_2#crun_state{sett_vars=SettVars#{export_params=>ExportParams_1}}};

crun("save_pref", [K_0, PV], State, Dict) ->
    {K, State_1} = crun_pv(K_0, State, Dict),
    {PVRes, State_2} = crun_pv(PV, State_1, Dict),
    %% Save into preference from path value PV
    io:format("TODO: save_pref ~s ~p~n", [K, PVRes]),
    {PVRes, State_2};

crun("load_pref", [K, VS, PVDefault], State, Dict) ->
    case false of
        false ->
            {_PVRes, _State} = crun_pv(PVDefault, State, Dict)
            
    end,
    
    %% Load from preference and into value slot VS
    io:format("TODO: load_pref ~s ~p~n", [K, VS]),
    State_1 = State,
    {ok, State_1}.

%% Evaluate a path value
crun_pv({etp,PV}, State, Dict) ->
    %% If the special %[...] syntax was used then evaluate the
    %% the contents based on the query mini language.
    T = etp_tok(PV, [], []),
    {PPV, []} = etp_start_parse(T),
    etp_run(PPV, State, Dict);
crun_pv({atom,Atom}, State, _Dict) ->
    {list_to_atom(Atom), State};
crun_pv(PV, State, _Dict) ->
    Val = case string:to_float(PV) of
        {Num_0, []} -> Num_0;
        _ ->
            case string:to_integer(PV) of
                {Num_0, []} -> Num_0;
                _ -> PV
            end
    end,
    {Val, State}.


crun_list([], State, _Dict) ->
    {ok, State};
crun_list([C | List], State, Dict) ->
    [C0 | CArgs] = C,
    {_, State_1} = crun(C0, CArgs, State, Dict),
    crun_list(List, State_1, Dict).

get_map(Key, SettVars) when is_atom(Key), is_map(SettVars) ->
    case maps:find(Key, SettVars) of
        {ok, Map} -> Map;
        _ -> #{}
    end.



%% WSCR term path notation (when using %[...])

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
    {Args_2, State_3} = etp_map_args(Args, State, Dict),
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

etp_run([{start,{get_from,Name}} | R], #crun_state{temp_vars=TempVars}=State, Dict) ->
    case maps:find(Name, TempVars) of
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

etp_map_args(Args, State, Dict) ->
    lists:foldl(fun(A, {Args_1, State_1}) ->
        {Ret_1, State_2} = etp_run(A, State_1, Dict),
        {[Ret_1|Args_1], State_2}
    end, {[], State}, Args).

etp_run_call("list", Args, State, Dict) ->
    {Args_2, State_3} = etp_map_args(Args, State, Dict),
    {lists:reverse(Args_2), State_3};

etp_run_call("tuple", Args, State, Dict) ->
    {Args_2, State_3} = etp_map_args(Args, State, Dict),
    {list_to_tuple(lists:reverse(Args_2)), State_3};

etp_run_call("bool", Args, State, Dict) ->
    {[Arg1 | _], State_3} = etp_map_args(Args, State, Dict),
    Arg1Bool = case Arg1 of
        Arg1 when is_integer(Arg1), (Arg1 > 0) orelse (Arg1 =:= -1) -> true;
        Arg1 when is_float(Arg1), Arg1 >= 1.0 -> true;
        0 -> false;
        Zero when abs(Zero) < ?EPSILON -> false;
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
    {[Arg1 | _], State_3} = etp_map_args(Args, State, Dict),
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
    {[Arg1 | _], State_3} = etp_map_args(Args, State, Dict),
    Arg1Float = case Arg1 of
        Arg1 when is_integer(Arg1) -> float(Arg1);
        Arg1 when is_float(Arg1) -> Arg1;
        true -> 1.0;
        false -> +0.0;
        Str when is_list(Str) ->
            case string:to_float(Str) of
                {Num_F, _} when is_float(Num_F) -> Num_F;
                _ ->
                    case string:to_integer(Str) of
                        {Num_I, _} when is_integer(Num_I) -> float(Num_I);
                        _ -> +0.0
                    end
            end;
        _ -> +0.0
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
    lists:foldl(fun(A, {_, State_1}) ->
        etp_run(A, State_1, Dict)
    end, {[], State}, Args);

etp_run_call("map", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, #crun_state{temp_vars=TempVars}=State_1}) ->
        Save_0 = etp_get_temp_var(0, TempVars),
        State_1_1 = etp_store_temp(0, A, State_1),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, State_2),
        {[Ret_1|Args_1], State_2_1}
    end, {[], State}, List_1),
    {Args_2, State_3};

etp_run_call("filter", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Args_2, State_3} = lists:foldl(fun(A, {Args_1, #crun_state{temp_vars=TempVars}=State_1}) ->
        Save_0 = etp_get_temp_var(0, TempVars),
        State_1_1 = etp_store_temp(0, A, State_1),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, State_2),
        {[Ret_1|Args_1], State_2_1}
    end, {[], State}, List_1),
    {Args_2, State_3};

etp_run_call("foldl", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Ret_2, State_3} = lists:foldl(fun(A, {Acc, #crun_state{temp_vars=TempVars}=State_1}) ->
        Save_0 = etp_get_temp_var(0, TempVars),
        Save_1 = etp_get_temp_var(1, TempVars),
        State_1_1 = etp_store_temp(0, A, etp_store_temp(1, Acc, State_1)),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, etp_store_temp(1, Save_1, State_2)),
        {Ret_1, State_2_1}
    end, {none, State}, List_1),
    {Ret_2, State_3};

etp_run_call("foldr", Args, State_0, Dict) ->
    [DoEr, ListArg | _] = Args,
    {List_1, State} = etp_run(ListArg, State_0, Dict),
    {Ret_2, State_3} = lists:foldr(fun(A, {Acc, #crun_state{temp_vars=TempVars}=State_1}) ->
        Save_0 = etp_get_temp_var(0, TempVars),
        Save_1 = etp_get_temp_var(1, TempVars),
        State_1_1 = etp_store_temp(0, A, etp_store_temp(1, Acc, State_1)),
        {Ret_1, State_2} = etp_run(DoEr, State_1_1#crun_state{p=A}, Dict),
        State_2_1 = etp_store_temp(0, Save_0, etp_store_temp(1, Save_1, State_2)),
        {Ret_1, State_2_1}
    end, {none, State}, List_1),
    {Ret_2, State_3}.

etp_store_temp(K, V, #crun_state{temp_vars=TempVars}=State) ->
    State#crun_state{temp_vars=TempVars#{K=>V}}.

etp_get_temp_var(Key, TempVars) when is_map(TempVars) ->
    case maps:find(Key, TempVars) of
        {ok, Val} -> Val;
        _ -> none
    end.


%% Easier access to some record fields than by tuple index
tuple_n_from_field(Tuple, FieldName)
  when is_tuple(Tuple) ->
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
etp_read_string_in_dq([BS, EscChar | Rest], AL)
  when BS =:= 92 -> %% Backslash
    etp_read_string_in_dq(Rest, [EscChar|AL]);
etp_read_string_in_dq([DQ | Rest], AL)
  when DQ =:= 34 -> %% Double quote
    {binstr(lists:reverse(AL)), Rest};
etp_read_string_in_dq([Char | Rest], AL) ->
    etp_read_string_in_dq(Rest, [Char|AL]).

%% Read the contents of an atom
%%
etp_read_atom_in_q(A) -> etp_read_atom_in_q(A, []).
etp_read_atom_in_q([], AL) -> {binstr(lists:reverse(AL)), []};
etp_read_atom_in_q([BS, EscChar | Rest], AL)
  when BS =:= 92 -> %% Back slash
    etp_read_atom_in_q(Rest, [EscChar|AL]);
etp_read_atom_in_q([AQ | Rest], AL)
  when AQ =:= $' -> %% Single quote
    {binstr(lists:reverse(AL)), Rest};
etp_read_atom_in_q([Char | Rest], AL) ->
    etp_read_atom_in_q(Rest, [Char|AL]).



warn(Str) ->
    io:format("~p: WARNING: ~s~n", [?MODULE, Str]).

scr_debug(Str) ->
    io:format("DEBUG: ~s~n", [Str]).

err(Str) ->
    io:format("~p: ERROR: ~s~n", [?MODULE,Str]).



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

test() ->
    Dict = [{"st", {st, 1,2,3,4}}],
    L =
    [["ifdef", {etp,"st{3}/plugins/wpc_example{3}"}, []],
     ["if", {etp,"st{2}/plugins/wpc_example{3}"}, "eq", {etp,"0"}, []],
     ["if", {etp,"st{1}/plugins/wpc_example{3}"}, "gt", {etp,"0"}, []],
     ["script_param", "paramname", {etp,"st.shapes"}],
     ["script_param", "paramname2", {etp,"tuple(1,2,3){0}"}],
     ["script_param", "paramname3", {etp,"lists:nth(1,list(1,2,3,4))>$'test1'"],
     ["script_param", "paramname4", {etp,"<$'test1'"}],
     ["import_param", "key", {etp,"\"val\""}],
     ["export_param", "tesselation", {atom,"triangulation"],
     ["save_pref", "k", {atom,"v"}],
     ["load_pref", "k", "v", {atom,"v"}]],
    crun_list(L, #crun_state{}, Dict).

t() ->
    Strings = #{
        "en" => [
            {{plugin,name}, "My Plugin"},
            {{menu,newsubmenu}, "New Submenu"},
            {{menu,newnewsubmenu}, "New new Submenu"},
            {{menu,newnewsubmenu2}, "New new Submenu"},
            {{menu,myplugin}, "My Plugin"},
            {{info,myplugin}, "My Plugin menu tooltip"},
            {{menu,myplugin2}, "My Plugin"},
            {{info,myplugin2}, "My Plugin menu tooltip"}
        ]},
    
    lists:usort(lists:append([
        pluginscripts_menus([file,newsubmenu,newnewsubmenu,myplugin],{[],'test',Strings}),
        pluginscripts_menus([file,newsubmenu,newnewsubmenu,myplugin2],{[],'test',Strings}),
        pluginscripts_menus([file,newsubmenu,newnewsubmenu2,myplugin],{[],'test',Strings}),
        pluginscripts_menus([file,newsubmenu,newnewsubmenu2,myplugin2],{[],'test',Strings})
    ])).

t3() ->
    {L0,L3}=get_pluginscripts(["pluginscripts"]),
    L = get_pluginscripts_1(L3, L0),
    io:format("file -> ~p~n", [pluginscripts_make_menu({file}, [], L)]),
    io:format("tools -> ~p~n", [pluginscripts_make_menu({tools}, [], L)]),
    io:format("tools/newsubmenu -> ~p~n", [pluginscripts_make_menu({tools,newsubmenu}, [], L)]),
    io:format("tools/newsubmenu/newnewsubmenu -> ~p~n", [pluginscripts_make_menu({tools,newsubmenu,newnewsubmenu}, [], L)]).

t2() ->
    {L0,L3}=get_scriptfolders(["scriptfolders"]),
    L = get_pluginscripts_1(L3, L0),
    io:format("body -> ~p~n", [pluginscripts_make_menu({body}, [], L)]),
    io:format("face -> ~p~n", [pluginscripts_make_menu({face}, [], L)]),
    io:format("shape -> ~p~n", [pluginscripts_make_menu({shape}, [], L)]).

-endif.


