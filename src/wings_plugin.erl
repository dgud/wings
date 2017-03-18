%%
%%  wings_plugin.erl --
%%
%%     Plug-In support, including the Plug-In Manager.
%%
%%  Copyright (c) 2001 Jakob Cederlund, Bjorn Gustavsson
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_plugin).
-export([init/0,menu/2,dialog/2,dialog_result/2,command/2,call_ui/1]).
-export([install/1]).
-export([draw/4,check_plugins/2,get_win_data/1,restore_window/6]).
-include("wings.hrl").
-include("e3d.hrl").
-import(lists, [sort/1,reverse/1,member/2]).

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
    ?SET(wings_plugins, []),
    put(wings_ui, def_ui_plugin()),
    case try_dir(wings_util:lib_dir(wings), "plugins") of
	none -> ok;
	PluginDir -> init_dir(PluginDir)
    end,
    wings_pref:set_default(disabled_plugins, []),
    AllPlugins = ?GET(wings_plugins),
    put(wings_all_plugins, AllPlugins),
    ?SET(wings_plugins, AllPlugins -- wings_pref:get_value(disabled_plugins)),
    ok.

call_ui(What) ->
    Ui = get(wings_ui),
    Ui(What).

menu(Name, Menu0) ->
    Menu = manager_menu(Name, Menu0),
    menu_1(?GET(wings_plugins), Name, Menu).

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
    dialog_1(Dialog, Ps, ?GET(wings_plugins)).

dialog_1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_u:error_msg("~w:dialog/2: crashed", [M]);
	NewPs when is_list(NewPs) ->
	    Checked = check_dialog(NewPs, M),
	    dialog_1(Dialog, Checked, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_u:error_msg("~w:dialog/2: bad return value", [M])
    end;
dialog_1(_Dialog, Ps, []) -> 
    Ps.

% Check (and fix) plugin dialogs results (new format)
check_dialog([Ok = {Name, Tuple}|Rest], Mod)
  when is_list(Name), is_tuple(Tuple) ->
    [Ok|check_dialog(Rest, Mod)];
check_dialog([[PPs]|Rest], Mod) ->
    [{atom_to_list(Mod), PPs}|check_dialog(Rest, Mod)];
check_dialog([{Name, []}|Rest], Mod)
  when is_list(Name) ->
    [check_dialog(Rest, Mod)];
check_dialog([], _) -> [].

dialog_result(Dialog, Ps) when is_tuple(Dialog), is_list(Ps) ->
    dialog_result1(Dialog, Ps, ?GET(wings_plugins)).

dialog_result1(Dialog, Ps, [M|Tail]) ->
    case catch M:dialog(Dialog, Ps) of
	{'EXIT',{undef,_}} ->
	    dialog_result1(Dialog, Ps, Tail);
	{'EXIT',Reason} ->
	    io:format("~w:dialog/2: crashed: ~P\n", [M,Reason,20]),
	    wings_u:error_msg("~w:dialog/2: crashed", [M]);
	{Content,NewPs} when is_list(NewPs) ->
	    dialog_result1(setelement(tuple_size(Dialog), Dialog, Content), 
			   NewPs, Tail);
	Other ->
	    io:format("~w:dialog/2: bad return value: ~P\n", [M,Other,20]),
	    wings_u:error_msg("~w:dialog/2: bad return value", [M])
    end;
dialog_result1(Dialog, Ps, []) -> 
    {element(tuple_size(Dialog), Dialog),Ps}.

command(Cmd, St) ->
    Ps = ?GET(wings_plugins),
    case manager_command(Cmd, St) of
	next -> command(Ps, Cmd, St);
	Other ->
	    case check_result(?MODULE, Other, St) of
		next -> command(Ps, Cmd, St);
		Res -> Res
	    end
    end.

command([M|Ps], Cmd, St) ->
    CmdFun = fun() -> M:command(Cmd, St) end,
    case catch wings_develop:time_command(CmdFun, Cmd) of
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
	    ?SET(wings_plugins, [M|?GET(wings_plugins)]);
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
    #we{} = We = wings_we:build(Fs, Vs),
    wings_obj:new(Name, We, St);
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
		    wings_u:error_msg(?__(1,"File \"~s\": Unknown file type"),
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
 		 wings_u:error_msg(?__(1,"Install of \"~s\" failed: ~p"),
			       [filename:basename(Name),
				file:format_error(Reason)])
	    end;
	false ->
	    wings_u:error_msg(?__(2,"File \"~s\" is not a Wings plug-in module"),
			  [filename:basename(Name)])
    end.

install_tar(Name) ->
    {ok,Files} = erl_tar:table(Name, [compressed]),
    install_verify_files(Files, Name),
    case erl_tar:extract(Name, [compressed,{cwd,plugin_dir()}]) of
	ok -> ok;
	{error, {_File, Reason}} -> 
	    wings_u:error_msg(?__(1,"Install of \"~s\" failed: ~p"),
			      [filename:basename(Name),
			       file:format_error(Reason)]);
	{error, Reason} ->
	    wings_u:error_msg(?__(1,"Install of \"~s\" failed: ~p"),
			      [filename:basename(Name),
			       file:format_error(Reason)])
    end.

install_verify_files(["/"++_|_], Name) ->
    wings_u:error_msg(?__(1,"File \"~s\" contains a file with an absolute path"),
		  [filename:basename(Name)]);
install_verify_files([F|Fs], Name) ->
    case is_plugin(F) of
	false -> install_verify_files(Fs, Name);
	true -> ok
    end;
install_verify_files([], Name) ->
    wings_u:error_msg(?__(2,"File \"~s\" does not contain any Wings plug-in modules"),
		  [filename:basename(Name)]).

is_plugin(Name) ->
    case filename:basename(Name) of
	"wpc_"++_ -> true;
	_ -> false
    end.

plugin_dir() ->
    case try_dir(wings_util:lib_dir(wings), "plugins") of
	none -> wings_u:error_msg(?__(1,"No \"plugins\" directory found"));
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
    {?__(1,"Plug-in Manager..."),plugin_manager,[]}.

manager_command({edit,plugin_manager}, St) ->
    Ps = get(wings_all_plugins),
    Categories = [cat_command_fun(),fun cat_tool/1,fun cat_select/1,
		  fun cat_render/1,fun cat_import_export/1,
		  fun cat_primitive/1],
    Cps0 = [{category(Categories, P),P} || P <- Ps],
    Cps = wings_util:rel2fam(Cps0),
    Fun = fun(Res) -> 
		  Disabled = [M || {M,false} <- Res],
		  ?SET(wings_plugins, Ps -- Disabled),
		  update_menus(Cps, Disabled),
		  plugin_close_win(Disabled),
		  update_disabled(Disabled, St)
	  end,
    Dialog = mk_dialog(Cps, false),
    wings_dialog:dialog(?__(1,"Plug-In Manager"), Dialog, Fun);
manager_command(_, _) -> next.

update_disabled(Disabled, St) ->
    case wings_pref:get_value(disabled_plugins) of
	Disabled -> ignore;
	_ ->
	    %% If any plug-in was enabled or disabled, we'll clear
	    %% the last repeatable command, as it might not be safe
	    %% to repeat it.
	    wings_pref:set_value(disabled_plugins, Disabled),
	    St#st{repeatable=ignore}
    end.

update_menus(Ps, Disabled) ->
    DisabledS = sets:from_list(Disabled),
    ToUpdate =
	lists:foldr(fun({Category, Plugins}, Acc) ->
	    PluginsS = sets:from_list(Plugins),
	    case sets:to_list(sets:intersection(PluginsS,DisabledS)) of
		[] -> Acc;
		Acc0 -> Acc ++[{Category,Acc0}]
	    end
	end, [], Ps),
    update_plugin_menus(ToUpdate).

update_plugin_menus([]) -> ignore;
update_plugin_menus([{Category, Ms}|Cps]) ->
    [update_menu_category(Category, M) || M <- Ms],
    update_plugin_menus(Cps).

update_menu_category(_, []) -> ignore;
update_menu_category(_, M) ->
    Items = collect_menus([{file,render},{file,import},
                           {file,export},{file,export_selected},
			   {edit},{edit,plugin_preferences},
                           {view},{select},{tools},{window},{help}], M),
    [delete_menu_item(Item) || Item <- Items].


delete_menu_item({{Menu},{_,SubMenu,_}}) ->
    delete_menu_item(Menu, SubMenu, ignore);
delete_menu_item({{Menu},{_,{SubMenu,_}}}) ->
    delete_menu_item(Menu, SubMenu, ignore);
delete_menu_item({{Menu,SubMenu},{_,Tag}}) ->
    delete_menu_item(Menu, SubMenu, Tag);
delete_menu_item({{Menu,SubMenu},{_,Tag,_}}) ->
    delete_menu_item(Menu, SubMenu, {Tag,true}).

delete_menu_item(Menu, SubMenu, ignore) ->
    wings_menu:update_menu(Menu,SubMenu, delete);
delete_menu_item(Menu, SubMenu, Tag) ->
    wings_menu:update_menu(Menu, {SubMenu, Tag}, delete).

mk_dialog(Cs, _Min) ->
    [{oframe,mk_dialog_1(Cs),1,[{style,buttons}]}].

mk_dialog_1([{C,Ms}|Cs]) ->
    [{cat_label(C),plugin_modules(C, Ms)}|mk_dialog_1(Cs)];
mk_dialog_1([]) -> [].

plugin_modules(C, Ms) ->
    Ps = [{info,?__(1,"Enable or disable this plug-in ")++
	       ?__(2,"(a disbled plug-in does not show up in menus)")},
	  {proportion, 1}],
    {vframe,
     [{hframe, [{atom_to_list(M), member(M, ?GET(wings_plugins)), [{key,M}|Ps]},
		plugin_info(C,M)]}
      || M <- Ms]}.

plugin_info(C, M) ->
    case plugin_info_1(C, M) of
	panel -> panel;
	{label, Str} -> {label, Str, [{proportion, 2}]}
    end.


cat_label(command) -> ?__(1,"Commands");
cat_label(export_import) -> ?__(2,"Import/export");
cat_label(primitive) -> ?__(3,"Primitives");
cat_label(render) -> ?__(4,"Render");
cat_label(select) -> ?__(5,"Select");
cat_label(tool) -> ?__(6,"Tools");
cat_label(unknown) -> ?__(7,"Unclassified").

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
    try_menu([{shape},{shape}], M, primitive).

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
    Modes ++ [{Mode,Cmd} || {Mode} <- Modes, Cmd <- Cmds]
    ++ [{vertex,deform},{face,tesselate}].

try_menu([N|Ns], M, Category) ->
    DefaultMenu = plugin_default_menu(),
    case M:menu(N, DefaultMenu) of
	DefaultMenu -> try_menu(Ns, M, Category);
	[_|_] -> Category
    end;
try_menu([], _, _) -> next.

plugin_info_1(export_import, M) -> export_import_info(M);
plugin_info_1(render, M) -> export_import_info(M);
plugin_info_1(command, M) ->
    Names = command_menus(),
    Menus = collect_menus(Names, M),
    plugin_menu_info(Menus);
plugin_info_1(primitive, M) ->
    Menus = collect_menus([{shape},{shape}], M),
    plugin_menu_info(Menus);
plugin_info_1(select, M) ->
    Menus = collect_menus([{select}], M),
    plugin_menu_info(Menus);
plugin_info_1(tool, M) ->
    Menus = collect_menus([{tools}], M),
    plugin_menu_info(Menus);
plugin_info_1(_, _) -> panel.

export_import_info(M) ->
    case collect_menus([{file,import},{file,export},{file,render}], M) of
	[{_,{Str,_}}|_] when is_list(Str) ->
	    export_menu_label(Str);
	[{_,{Str,_,_}}|_] when is_list(Str) ->
	    export_menu_label(Str);
	_Other ->
	    panel
    end.

export_menu_label(Str) ->
    {label,string:strip(Str, right, $.)}.

collect_menus([N|Ns], M) when is_tuple(N) ->
    DefaultMenu = plugin_default_menu(),
    try M:menu(N, DefaultMenu) of
	DefaultMenu ->
	    collect_menus(Ns, M);
	[_|_]=Menu0 ->
	    Menu = clean_menu(Menu0),
	    [{N,Mi} || Mi <- Menu] ++ collect_menus(Ns, M)
    catch
	_:_Error ->
	    collect_menus(Ns, M)
    end;
collect_menus([], _) -> [].

clean_menu([separator|T]) ->
    clean_menu(T);
clean_menu([plugin_manager_category|T]) ->
    clean_menu(T);
clean_menu([H|T]) ->
    [H|clean_menu(T)];
clean_menu([]) -> [].

plugin_menu_info([]) -> panel;
plugin_menu_info([_|_]=Menus0) ->
    Menus = normalize_menu(Menus0),
    plugin_menu_info_1(Menus).

plugin_menu_info_1(Cmds0) ->
    Cmds = wings_util:rel2fam(Cmds0),
    case Cmds of
	[{Root,SubCmds0}] ->
	    SubCmds = [wings_util:stringify(C) || C <- SubCmds0],
	    Str = string:join(SubCmds, ", "),
	    {label,plugin_root_menu(Root)++Str};
	_ ->
	    plugin_menu_info_2(Cmds0)
    end.

plugin_menu_info_2(Cmds) ->
    R0 = sofs:from_external(Cmds, [{root,command}]),
    R1 = sofs:converse(R0),
    R2 = sofs:relation_to_family(R1),
    R = sofs:to_external(R2),
    case R of
	[{Cmd,Modes}] ->
	    {label,string:join([atom_to_list(M) || M <- Modes], ", ") ++ ": " ++
	     wings_util:stringify(Cmd)};
	_ ->
	    plugin_menu_info_3(Cmds)
    end.

plugin_menu_info_3(Cmds0) ->
    Cmds = [plugin_root_menu(Mode) ++ wings_util:stringify(C) ||
	       {Mode,C} <- Cmds0],
    Str = case Cmds of
	      [] -> ?__(1,"(The Plug-In Manager was unable to find more information)");
	      _ -> string:join(Cmds, "; ")
	  end,
    {label,Str}.

plugin_root_menu(body) -> ?__(1,"body: ");
plugin_root_menu(edge) -> ?__(2,"edge: ");
plugin_root_menu(face) -> ?__(3,"face: ");
plugin_root_menu(vertex) -> ?__(4,"vertex: ");
plugin_root_menu(select) -> ?__(5,"Select|");
plugin_root_menu(tools) -> ?__(6,"Tools|");
plugin_root_menu(shape) -> "".

normalize_menu([{Name,[_|_]=Menu}|T]) ->
    normalize_menu([{Name,M} || M <- Menu] ++ T);
normalize_menu([{Name,Menu}|T]) ->
    case plugin_key(Menu) of
	none ->
	    normalize_menu(T);
	Key ->
	    Cmd = wings_menu:build_command(Key, reverse(tuple_to_list(Name))),
	    [Cmd|normalize_menu(T)]
    end;
normalize_menu([]) -> [].

plugin_key({_,{Key,_}}) when is_atom(Key) -> Key;
plugin_key({_,{Key,_},_}) when is_atom(Key) -> Key;
plugin_key({_,{Key,_},_, _}) when is_atom(Key) -> Key;
plugin_key({_,Key,_}) when is_atom(Key) -> Key;
plugin_key({_,Key,_,_}) when is_atom(Key) -> Key;
plugin_key(Other) when is_tuple(Other) -> list_to_atom(element(1, Other));
plugin_key(_) -> none.

plugin_default_menu() ->
    [plugin_manager_category].

%%%
%%% Plugin Draw Utilities
%%%

% There are 2 draw flags originating from wings_render.erl: 'plain' and 'smooth'
% which relate to the view style of the model and determine if the Plugin should
% be drawn in that view mode. Other restrictions can be stated from within the
% Plugin itself. See wpc_magnet_mask.erl as an example.
draw(Flag, #dlo{plugins=Pdl}=D, Selmode, RS) ->
    draw_1(Flag, Pdl, D, Selmode, RS).

draw_1(Flag, [{Plugin,List}|Pdl], D, Selmode, RS0) ->
    RS = Plugin:draw(Flag, List, D, Selmode, RS0),
    draw_1(Flag, Pdl, D, Selmode, RS);

draw_1(_, [], _, _, RS) -> RS.

% PstTree -> { PluginName , PluginValue }
% PluginValue is a gb_tree.
check_plugins(Flag,Pst) ->
    case gb_trees:is_empty(Pst) of
      true -> [];
      false ->
        Plugins = gb_trees:keys(Pst),
        check_each_plugin(Flag, Plugins, Pst, [])
    end.

check_each_plugin(Flag, [P|Plugins],Pst,Acc) ->
    PData = gb_trees:get(P, Pst),
    Result = check_plugin_against_flag(Flag, PData, P, Acc),
    check_each_plugin(Flag, Plugins, Pst, Result);
check_each_plugin(_, [], _, Acc) -> Acc.

% There are 2 flags which can be passed into this function:
% 'update_dlist' and 'save'.

% 'save' originates from wings_we.erl and is passed to the Plugin to check
% if the elements stored in the pst should be renumbered when saving.

% 'update_dlist' originates from wings_draw.erl and is passed to the
% Plugin to check if the it should have its draw list updated.

% Further restrictions to bar the output may be stated from within the plugin.
% See wpc_magnet_mask.erl
check_plugin_against_flag(_Flag, _PData, wings_shape, Acc) ->
    %% FIXME: In the future, we really should stop using the plug-in
    %% mechanism for handling folders (a built-in feature).
    %% For now, make sure that we fail quickly without attempting
    %% to load the non-existing wings_shape module from disk.
    Acc;
check_plugin_against_flag(Flag, PData, Plugin, Acc) ->
    case catch Plugin:get_data(Flag,PData,Acc) of
      {ok, Result} -> Result;
      _otherwise -> Acc
    end.


%%%
%%% Plugin Save/Restore Windows Utilities
%%%

% It allows wings.erl module get/set the plugin window/s information (if there is/are any)
% See wpc_sel_win.erl as an example.
get_win_data(WinName) ->
    Ps = ?GET(wings_plugins),
    get_win_data_1(Ps, WinName).

%% win_data/1 function allows many plugin's windows to be saved.
%% it should returns: {Name, {Horiz alignment, Custom_data}}
get_win_data_1([M|Ps], WinName) ->
	case catch M:win_data(WinName) of
	  {WinName,Data} -> {M,Data};
	  _ -> get_win_data_1(Ps, WinName)
	end;
get_win_data_1([], _) -> none.

restore_window(M, WinName, Pos, Size, CtmData, St) ->
    Ps = ?GET(wings_plugins),
    case module_found(M,Ps) of
	true ->
	    catch M:window(WinName, Pos, Size, CtmData, St),
	    keep;
	_ -> keep
    end.

module_found(M,[M|_]) -> true;
module_found(M,[_|T]) -> module_found(M,T);
module_found(_,[]) -> false.


%%%
%%% Plugin Close Windows Utility
%%%

% It allows this module to close any plugin's window when a module is disabled.
plugin_close_win([M|Ps]) ->
    try M:win_name() of
	WinName ->
	    plugin_close_win_0(WinName),
	    plugin_close_win(Ps)
    catch
    	_Exception:_Reason ->
	    plugin_close_win(Ps)
    end;
plugin_close_win([]) -> none.

plugin_close_win_0([]) -> ignore;
plugin_close_win_0([Win|Wins]) ->
    plugin_close_win_0(Win),
    plugin_close_win_0(Wins);
plugin_close_win_0(Win) ->
    wings_frame:close(Win).
