%%
%%  wings_plugin.erl --
%%
%%     Plug-In support, including the Plug-In Manager.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%  Copyright (c) 2002-2008 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_plugin).
-export([init/0,menu/2,dialog/2,dialog_result/2,command/2,call_ui/1]).
-export([install/1]).

-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [append/1,flatmap/2,foreach/2,sort/1,reverse/1,foldl/3,
		member/2]).

%%%
%%% Currently, there can be a single directory for plugins, but
%%% sub-directories to any level will be searched.
%%% The plugin directory must be named 'plugins'. It must be located
%%% either in the same directory as the beam files, or in a directory
%%% parallell to the 'ebin' directory if the beam files are kept in
%%% a 'ebin' directory.
%%%
%%% To avoid name space clashing, plugins must be named according to
%%% the following convention:
%%%    wpT_*.beam
%%% where the T should be replaced with the type of the plugin.
%%%
%%% The types are defined as following:
%%%
%%% c   Command extension plugin-in.
%%% 8   External user-interface plugin.
%%% 9   Default user-interface plugin.
%%%

init() ->
    ets:new(wings_seen_plugins, [named_table,public,ordered_set]),
    put(wings_plugins, []),
    put(wings_ui, def_ui_plugin()),
    case try_dir(wings_util:lib_dir(wings), "plugins") of
	none -> ok;
	PluginDir -> init_dir(PluginDir)
    end,
    wings_pref:set_default(disabled_plugins, []),
    AllPlugins = get(wings_plugins),
    put(wings_all_plugins, AllPlugins),
    put(wings_plugins, AllPlugins -- wings_pref:get_value(disabled_plugins)),
    ok.

call_ui(What) ->
    Ui = get(wings_ui),
    Ui(What).

menu(Name, Menu0) ->
    Menu = manager_menu(Name, Menu0),
    menu_1(get(wings_plugins), Name, Menu).

menu_1([M|Ps], Name, Menu0) ->
    case catch M:menu(Name, Menu0) of
	Menu when is_list(Menu) ->
	    menu_1(Ps, Name, Menu);
	Other ->
	    io:format("~w:menu/2: bad return value: ~P\n", [M,Other,20]),
	    menu_1(Ps, Name, Menu0)
    end;
menu_1([], _Name, Menu) -> Menu.


dialog(Dialog, Ps) when is_list(Ps) ->
    dialog_1(Dialog, Ps, get(wings_plugins)).

dialog_1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_u:error("~w:dialog/2: crashed", [M]);
	NewPs when is_list(NewPs) ->
	    dialog_1(Dialog, NewPs, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_u:error("~w:dialog/2: bad return value", [M])
    end;
dialog_1(_Dialog, Ps, []) -> 
    Ps.

dialog_result(Dialog, Ps) when is_tuple(Dialog), is_list(Ps) ->
    dialog_result1(Dialog, Ps, get(wings_plugins)).

dialog_result1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_result1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_u:error("~w:dialog/2: crashed", [M]);
	{Content,NewPs} when is_list(NewPs) ->
	    dialog_result1(setelement(tuple_size(Dialog), Dialog, Content), 
			   NewPs, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_u:error("~w:dialog/2: bad return value", [M])
    end;
dialog_result1(Dialog, Ps, []) -> 
    {element(tuple_size(Dialog), Dialog),Ps}.

command(Cmd, St) ->
    Ps = get(wings_plugins),
    case manager_command(Cmd, St) of
	next -> command(Ps, Cmd, St);
	Other ->
	    case check_result(?MODULE, Other, St) of
		next -> command(Ps, Cmd, St);
		Res -> Res
	    end
    end.

command([M|Ps], Cmd, St) ->
    case catch M:command(Cmd, St) of
	next -> command(Ps, Cmd, St);
	Other ->
	    case check_result(M, Other, St) of
		next -> command(Ps, Cmd, St);
		Res -> Res
	    end
    end;
command([], _Cmd, _St) -> next.

%%%
%%% Local functions.
%%%

init_dir(Dir) ->
    TypeMods = reverse(sort(list_dir(Dir))),
    init_plugins(TypeMods).

init_plugins([{Type,M}|T]) ->
    case ets:member(wings_seen_plugins, M) of
	false ->
	    init_plugin(Type, M),
	    ets:insert(wings_seen_plugins, {M});
	true -> ok
    end,
    init_plugins(T);
init_plugins([]) -> ok.

init_plugin(user_interface, M) ->
    Ui0 = get(wings_ui),
    case catch M:init(Ui0) of
	Ui when is_function(Ui) ->
	    put(wings_ui, Ui);
	Other ->
	    io:format("~w:init/1 bad return value: ~P\n", [M,Other,20])
    end;
init_plugin(_, M) ->
    case catch M:init() of
	true ->
	    put(wings_plugins, [M|get(wings_plugins)]);
	false ->
	    ok;
	Other ->
	    io:format("~w:init/0 bad return value: ~P\n", [M,Other,20])
    end.
    
def_ui_plugin() ->
    fun(Missing) ->
	    Msg = io_lib:format(?__(1,"Reinstall Wings. Missing plugin for ~p."),
				[Missing]),
	    wings_wm:message(lists:flatten(Msg)),
	    aborted
    end.

try_dir(Base, Dir0) ->
    Dir = filename:join(Base, Dir0),
    case filelib:is_dir(Dir) of
	true -> Dir;
	false -> none
    end.

list_dir(Dir) ->
    list_dir([Dir], []).

list_dir([Dir|Dirs0], Beams0) ->
    case file:list_dir(Dir) of
	{ok,List} ->
	    case list_dir_1(List, Dir, Dirs0, Beams0) of
		{Dirs,Beams0} ->
		    %% No new beam files found in this directory.
		    list_dir(Dirs, Beams0);
		{Dirs,Beams} ->
		    %% Beam files found here -- include directory in code path.
		    code:add_patha(Dir),
		    list_dir(Dirs, Beams)
	    end;
	{error,_} -> list_dir(Dirs0, Beams0)
    end;
list_dir([], Beams) -> Beams.

list_dir_1(["~"++_|Ns], Dir, Dirs, Beams) ->
    %% We skip any file or directory starting with a "~".
    list_dir_1(Ns, Dir, Dirs, Beams);
list_dir_1([[$w,$p,Type0,$_|_]=N|Ns], Dir, Dirs, Beams) ->
    case filename:extension(N) of
	".beam" ->
	    case convert_type(Type0) of
		undefined ->
		    list_dir_1(Ns, Dir, Dirs, Beams);
		Type ->
		    Mod = list_to_atom(filename:rootname(N)),
		    list_dir_1(Ns, Dir, Dirs, [{Type,Mod}|Beams])
	    end;
	_ -> list_dir_1(Ns, Dir, Dirs, Beams)
    end;
list_dir_1([N|Ns], Dir0, Dirs, Beams) ->
    case try_dir(Dir0, N) of
	none -> list_dir_1(Ns, Dir0, Dirs, Beams);
	Dir -> list_dir_1(Ns, Dir0, [Dir|Dirs], Beams)
    end;
list_dir_1([], _Dir, Dirs, Beams) -> {Dirs,Beams}.
    
convert_type($c) -> command;
convert_type($8) -> user_interface;
convert_type($9) -> user_interface;
convert_type(_) -> undefined.

check_result(_M, {command_error,_}=Error, _St) -> throw(Error);
check_result(_M, {new_shape,Prefix,#e3d_object{}=Obj,Mat}, St) ->
    Name = object_name(Prefix, St),
    File = #e3d_file{objs=[Obj#e3d_object{name=Name,mat=Mat}]},
    wings_import:import(File, St);
check_result(_M, {new_shape,Prefix,Fs,Vs}, St) ->
    Name = object_name(Prefix, St),
    We = wings_we:build(Fs, Vs),
    wings_shape:new(Name, We, St);
check_result(_M, aborted, _St) -> aborted;
check_result(_M, {drag,_}=Drag, _) -> Drag;
check_result(_M, #st{}=St, _) -> St;
check_result(_M, {save_state,#st{}}=SS, _) -> SS;
check_result(_M, {push,_}=Push, _) -> Push;
check_result(_M, {seq,_,_}=Seq, _) -> Seq;
check_result(_M, keep, _) -> keep;
check_result(M, Other, St) ->
    io:format("~w:command/3: bad return value: ~P\n", [M,Other,20]),
    St.

object_name(Prefix, #st{onext=Oid}) ->
    Prefix++integer_to_list(Oid).

%%%
%%% Installing a plug-in.
%%%

install(Name) ->
    case install_file_type(Name) of
	beam -> install_beam(Name);
	tar -> install_tar(Name)
    end,
    init_dir(plugin_dir()),
    wings_u:message(?__(1,"The plug-in was successfully installed.")).

install_file_type(Name) ->
    case filename:extension(Name) of
	".tgz" -> tar;
	".beam" -> beam;
	".tar" -> tar;
	".gz" ->
	    case filename:extension(filename:rootname(Name, ".gz")) of
		".tar" -> tar;
		".beam" -> beam;
		_ ->
		    wings_u:error(?__(1,"File \"~s\": Unknown file type"),
				  [filename:basename(Name)])
	    end
    end.

install_beam(Name) ->
    case is_plugin(Name) of
	true ->
	    PluginDir = plugin_dir(),
	    DestBase = filename:rootname(filename:basename(Name), ".gz"),
	    Dest = filename:join(PluginDir, DestBase),
	    case file:copy(Name, Dest) of
		{ok,_} -> ok;
		{error,Reason} ->
 		 wings_u:error(?__(1,"Install of \"~s\" failed: ~p"),
			       [filename:basename(Name),
				file:format_error(Reason)])
	    end;
	false ->
	    wings_u:error(?__(2,"File \"~s\" is not a Wings plug-in module"),
			  [filename:basename(Name)])
    end.

install_tar(Name) ->
    {ok,Files} = erl_tar:table(Name, [compressed]),
    install_verify_files(Files, Name),
    erl_tar:extract(Name, [compressed,{cwd,plugin_dir()}]).

install_verify_files(["/"++_|_], Name) ->
    wings_u:error(?__(1,"File \"~s\" contains a file with an absolute path"),
		  [filename:basename(Name)]);
install_verify_files([F|Fs], Name) ->
    case is_plugin(F) of
	false -> install_verify_files(Fs, Name);
	true -> ok
    end;
install_verify_files([], Name) ->
    wings_u:error(?__(2,"File \"~s\" does not contain any Wings plug-in modules"),
		  [filename:basename(Name)]).

is_plugin(Name) ->
    case filename:basename(Name) of
	"wpc_"++_ -> true;
	_ -> false
    end.

plugin_dir() ->
    case try_dir(wings_util:lib_dir(wings), "plugins") of
	none -> wings_u:error(?__(1,"No \"plugins\" directory found"));
	PluginDir -> PluginDir
    end.
    
%%%
%%% Plug-in manager.
%%%
%%% We call the menu/2 function in each function to classify the plug-in
%%% (as command, primitive, import/export, and so on), and to find out
%%% what commands it implements.
%%%

manager_menu({edit}, Menu) ->
    manager_menu_1(Menu);
manager_menu(_, Menu) -> Menu.

manager_menu_1([{_,Fun,_,_}=H|T]) ->
    case erlang:fun_info(Fun, module) of
	{module,wings_pref_dlg} ->
	    %% Preferred position.
	    [H,manager_entry()|T];
	_Other ->
	    [H|manager_menu_1(T)]
    end;
manager_menu_1([H|T]) ->
    [H|manager_menu_1(T)];
manager_menu_1([]) ->
    %% Fallback position.
    [manager_entry()].

manager_entry() ->
    {"Plug-in Manager...",plugin_manager,[]}.

manager_command({edit,plugin_manager}, _St) ->
    Ps = get(wings_all_plugins),
    Categories = [cat_command_fun(),fun cat_tool/1,fun cat_select/1,
		  fun cat_render/1,fun cat_import_export/1,
		  fun cat_primitive/1],
    Cps0 = [{category(Categories, P),P} || P <- Ps],
    Cps = wings_util:rel2fam(Cps0),
    Fun = fun(Res) -> 
		  Disabled = [M || {M,false} <- Res],
		  put(wings_plugins, get(wings_plugins) -- Disabled),
		  wings_pref:set_value(disabled_plugins, Disabled),
		  ignore
	  end,
    Dialog = mk_dialog(Cps, false),
    wings_ask:dialog("Plug-In Manager", Dialog, Fun);
manager_command(_, _) -> next.

mk_dialog(Cs, _Min) ->
    [{oframe,mk_dialog_1(Cs),1,[{style,buttons}]}].

mk_dialog_1([{C,Ms}|Cs]) ->
    [{cat_label(C),plugin_modules(C, Ms)}|mk_dialog_1(Cs)];
mk_dialog_1([]) -> [].

plugin_modules(C, Ms) ->
    {hframe,[{vframe,[{atom_to_list(M),member(M, get(wings_plugins)),
		       [{key,M}]} || M <- Ms]},
	     {vframe,[plugin_info(C, M) || M <- Ms]}]}.

cat_label(command) -> "Commands";
cat_label(export_import) -> "Import/export";
cat_label(primitive) -> "Primitives";
cat_label(render) -> "Render";
cat_label(select) -> "Select";
cat_label(tool) -> "Tools";
cat_label(unknown) -> "Unclassified".

category([F|Fs], M) ->
    try F(M) of
	next -> category(Fs, M);
	Cat -> Cat
    catch
	_:_ ->
	    category(Fs, M)
    end;
category([], _) -> unknown.

cat_render(M) ->
    try_menu([{file,render}], M, render).

cat_import_export(M) ->
    try_menu([{file,import},{file,export}], M, export_import).

cat_primitive(M) ->
    try_menu([{shape},{shape,more}], M, primitive).

cat_select(M) ->
    try_menu([{select}], M, select).

cat_tool(M) ->
    try_menu([{tools}], M, tool).

cat_command_fun() ->
    Names = command_menus(),
    fun(M) ->
	    try_menu(Names, M, command)
    end.

command_menus() ->
    Modes = [{vertex},{edge},{face},{body}],
    Cmds = [move,scale,rotate,flatten],
    Modes ++ [{Mode,Cmd} || {Mode} <- Modes, Cmd <- Cmds] ++ [{face,tesselate}].
    
try_menu([N|Ns], M, Category) ->
    DefaultMenu = [plugin_manager_category],
    case M:menu(N, DefaultMenu) of
	DefaultMenu -> try_menu(Ns, M, Category);
	[_|_] -> Category
    end;
try_menu([], _, _) -> next.

plugin_info(export_import, M) -> export_import_info(M);
plugin_info(render, M) -> export_import_info(M);
plugin_info(command, M) ->
    Names = command_menus(),
    Menus = collect_menus(Names, M),
    plugin_menu_info(Menus);
plugin_info(primitive, M) ->
    Menus = collect_menus([{shape},{shape,more}], M),
    plugin_menu_info(Menus);
plugin_info(select, M) ->
    Menus = collect_menus([{select}], M),
    plugin_menu_info(Menus);
plugin_info(tool, M) ->
    Menus = collect_menus([{tools}], M),
    plugin_menu_info(Menus);
plugin_info(_, _) -> panel.

export_import_info(M) ->
    read_menu([{file,import},{file,export}], M).

read_menu([N|Ns], M) ->
    try M:menu(N, []) of
	[{Str,_}|_] when is_list(Str) ->
	    read_menu_label(Str);
	[{Str,_,_}|_] when is_list(Str) ->
	    read_menu_label(Str);
	_Other ->
	    read_menu(Ns, M)
    catch
	_:_Error ->
	    read_menu(Ns, M)
    end;
read_menu([], _) -> panel.

read_menu_label(Str) ->
    {label,string:strip(Str, right, $.)}.

collect_menus([N|Ns], M) ->
    try M:menu(N, []) of
	[_|_]=Menu0 ->
	    Menu = clean_menu(Menu0),
	    [{N,Mi} || Mi <- Menu] ++ collect_menus(Ns, M);
	[] ->
	    collect_menus(Ns, M)
    catch
	_:_Error ->
	    collect_menus(Ns, M)
    end;
collect_menus([], _) -> [].

clean_menu([{basic,Menu}|T]) ->
    case wings_pref:get_value(advanced_menus) of
	false -> clean_menu([Menu|T]);
	true -> clean_menu(T)
    end;
clean_menu([{advanced,Menu}|T]) ->
    case wings_pref:get_value(advanced_menus) of
	false -> clean_menu(T);
	true -> clean_menu([Menu|T])
    end;
clean_menu([separator|T]) ->
    clean_menu(T);
clean_menu([H|T]) ->
    [H|clean_menu(T)];
clean_menu([]) -> [].

plugin_menu_info([{Name,Menu}|T]) when is_tuple(Name) ->
    case plugin_key(Menu) of
	none -> panel;
	Key ->
	    Cmd = wings_menu:build_command(Key, reverse(tuple_to_list(Name))),
	    S0 = wings_util:stringify(Cmd),
	    S = if
		    T =/= [] -> S0 ++ "...";
		    true -> S0
		end,
	    {label,S}
    end;
plugin_menu_info(_Other) ->
    panel.

plugin_key({_,{Key,_}}) when is_atom(Key) -> Key;
plugin_key({_,Key,_}) when is_atom(Key) -> Key;
plugin_key({_,Key,_,_}) when is_atom(Key) -> Key;
plugin_key(_Other) -> none.
