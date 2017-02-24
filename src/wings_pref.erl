%%
%%  wings_pref.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_pref).
-export([init/0,finish/0,load/0, get_dir/0,
	 lowpass/2,
	 get_value/1,get_value/2,set_value/2,set_default/2,
	 delete_value/1,
	 get_scene_value/0,get_scene_value/1,get_scene_value/2,
	 set_scene_value/2,set_scene_default/2,
	 delete_scene_value/0,delete_scene_value/1]).
-export([pref/1,pref/2,recent_prefs/0]).

-define(NEED_ESDL, 1).	  %% Some keybindings
-include("wings.hrl").
-import(lists, [foreach/2,reverse/1,sort/1,last/1,foldl/3,member/2]).

init() ->
    ets:new(wings_state, [named_table,public,ordered_set]),
    ets:new(wings_delayed_update, [named_table,public,ordered_set]),
    ets:new(wings_scene_prefs, [named_table,public,ordered_set]),
    ets:insert(wings_state, defaults()).

load() ->
    case old_pref_file() of
        none ->
            %% No preference file found. We must turn
            %% on the advanced menus.
            set_value(advanced_menus, true),
            set_value(legacy_colors_checked, true),
            ok;
        PrefFile ->
            io:format("wings-~s\nReading preferences from: ~ts\n",
                [?WINGS_VERSION, PrefFile]),
            case local_consult(PrefFile) of
                {ok,List0} ->
                    List = clean(List0),
                    catch ets:insert(wings_state, List),
                    check_user_keys(List),
                    check_legacy_colors(List),
                    win32_window_layout(),
                    no_more_basic_menus();
                {error,_Reason} ->
                    ok
            end
    end.

%%% The function tries to load the preferences files without getting an error if it isn't unicode
%%% It's applied to the Preferences.txt and Preference Subset.pref files
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

get_dir() ->
    PFile = case get_value(pref_directory) of
		undefined -> 
		    File = get_pref_directory("backup_prefs.txt"),
		    set_value(pref_directory,File),
		    File;
		File -> File
	    end,
    filename:dirname(PFile).


%%%% Restore window layout on MS Windows
win32_window_layout() ->
    Max = get_value(win32_start_maximized),
    Os = os:type(),
    case Os of
      {win32,_} when Max ->
        self() ! {external, win32_start_maximized},
        ok;
      _ -> ok
    end.

no_more_basic_menus() ->
    %% If Wings is launched the first time, the setting of
    %% advanced_menus will determine whether an information
    %% dialog should be shown. At subsequent launch, the
    %% value of no_basic_menu_info decides.
    case get_value(advanced_menus) andalso get_value(no_basic_menu_info) of
	true ->
	    %% The user either already used advanced menus or
	    %% (s)he has chosen to turn off the informtional dialog.
	    ok;
	false ->
	    %% Either this is the first launch and the user used
	    %% the basic menus, or (s)he has still not turned off
	    %% the informational dialog.
	    set_value(no_basic_menu_info, true),
	    self() ! {external,no_more_basic_menus},
	    ok
    end,

    %% Advanced menus are now always on. We must force this value
    %% to true until we have cleaned away all code that test it.
    set_value(advanced_menus, true).

finish() ->
    win32_save_maximized(),
    foreach(fun({Key,Val}) ->
		    set_value(Key, Val)
	    end, ets:tab2list(wings_delayed_update)),
    PrefFile = new_pref_file(),
    filelib:ensure_dir(PrefFile),
    finish_save_prefs(PrefFile).

finish_save_prefs(PrefFile) ->
    List0 = prune_temp(ets:tab2list(wings_state)),
    List  = prune_defaults(List0),
    Format = "~p. \n",
    PostProcess = case os:type() of
		      {win32,_} -> fun insert_crs/1;
		      _ -> fun(L) -> L end
		  end,
    Write = fun({{bindkey,_},_,default}) -> [];
	       ({{bindkey,_},_,plugin}) -> [];
	       ({{bindkey,_,_},_,default}) -> [];
	       ({{bindkey,_,_},_,plugin}) -> [];
	       (Else) -> PostProcess(io_lib:format(Format, [Else]))
	    end,
    Str = lists:map(Write, List),
    Bin = unicode:characters_to_binary(io_lib:format("~ts",[Str])),
    catch file:write_file(PrefFile, Bin),
    ok.

lowpass(X, Y) ->
    case get_value(jumpy_camera) of
	false -> {X,Y};
	true -> {lowpass(X),lowpass(Y)}
    end.

lowpass(N) when N > 0 -> lowpass_1(N);
lowpass(N) -> -lowpass_1(-N).

lowpass_1(N) when N =< 7 -> N;
lowpass_1(N) when N =< 15 -> (N-7)*0.6 + lowpass_1(7);
lowpass_1(N) when N =< 30 -> (N-15)*0.5 + lowpass_1(15);
lowpass_1(N) when N =< 50 -> (N-30)*0.05 + lowpass_1(30);
lowpass_1(_) -> lowpass_1(50).

%% Insert CRs into a deep list to produce a correct Windows
%% text file.
insert_crs([H|T]) -> [insert_crs(H)|insert_crs(T)];
insert_crs([]) -> [];
insert_crs($\n) -> "\r\n";
insert_crs(C) when is_integer(C) -> C.

prune_defaults(List) ->
    List -- defaults().

prune_temp([{{temp,_}, _}|List]) ->
    prune_temp(List);
prune_temp([First|Rest]) ->
    [First | prune_temp(Rest)];
prune_temp([]) -> [].

win32_save_maximized() ->
    case os:type() of
	{win32,_} ->
	    set_value(win32_start_maximized, wings_io:is_maximized());
	_ ->
	    ok
    end.

%%%
%%% Search for a pre-existing preference file.
%%%

old_pref_file() ->
    PrefFiles = pref_dirs(),
    old_pref_file(PrefFiles).

old_pref_file([{Dir, File}|Rest]) ->
    case try_location(Dir, File) of
        none -> old_pref_file(Rest);
        Path -> Path
    end;
old_pref_file([]) ->
    none.


pref_dirs() ->
    Dir = filename:basedir(user_config, "Wings3D"),
    Old = case os:type() of
              {win32,_} -> win32_pref();
              {unix,darwin} -> mac_pref();
              {unix,_} -> unix_pref()
          end,
    [{Dir, "Preferences.txt"}|Old].

unix_pref() ->
    Home = os:getenv("HOME"),
    [{filename:join([Home, ".wings3d"]), "preferences.txt"},
     {Home, ".wings"}].

mac_pref() ->
    Home = os:getenv("HOME"),
    [{filename:join([Home,"Library","Preferences","Wings3D"]), "Preferences.txt"},
     {filename:join([Home,"Library","Preferences"]), "Wings 3D Preferences.txt"} |
     unix_pref()].

win32_pref() ->
    win32_pref_registry(["AppData","Personal"]).

%% Try to find old preferences using the registry.
win32_pref_registry(RegistryLocations) ->
    try
        {ok,R} = win32reg:open([read]),
        Res = win32_pref_1(R, RegistryLocations),
        ok = win32reg:close(R),
        Res
    catch _:_ -> []
    end.

%% Search for a preference file in "special folders", such as "AppData"
%% and "My Documents".
win32_pref_1(R, [FolderType|T]) ->
    case wings_u:win32_special_folder(R, FolderType) of
	none -> win32_pref_1(R, T);
	Path ->
            [{filename:join([Path,"Wings3D"]),"Preferences.txt"}|
             win32_pref_1(R, T)]
    end;
win32_pref_1(_, []) -> [].

%%%
%%% Return a suitable path for a new preference file.
%%%

new_pref_file() ->
    [{Dir,File}|_] = pref_dirs(),
    filename:join(Dir,File).

%%%
%%% Utilities.
%%%

try_location(Dir, File) ->
    Name = filename:join(Dir, File),
    case filelib:is_file(Name) of
	true -> Name;
	false -> none
    end.

get_value(Key) ->
    get_value(Key, undefined).

get_value(Key, Default) ->
    case ets:lookup(wings_state, Key) of
	[] -> Default;
	[{Key,Val}] -> Val
    end.

set_value(Key, Value) ->
    ets:insert(wings_state, {Key,Value}),
    ok.

set_default(Key, Value) ->
    case ets:member(wings_state, Key) of
	true -> ok;
	false ->
	    ets:insert(wings_state, {Key,Value}),
	    ok
    end.

delete_value(Key) ->
    ets:delete(wings_state, Key),
    ok.


%%% Scene prefs

get_scene_value() ->
    ets:tab2list(wings_scene_prefs).

get_scene_value(Key) ->
    get_value(Key, undefined).

get_scene_value(Key, Default) ->
    case ets:lookup(wings_scene_prefs, Key) of
	[] -> Default;
	[{Key,Val}] -> Val
    end.

set_scene_value(Key, Value) ->
    true = ets:insert(wings_scene_prefs, {Key,Value}),
    ok.

set_scene_default(Key, Value) ->
    case ets:member(wings_scene_prefs, Key) of
	true -> ok;
	false ->
	    ets:insert(wings_scene_prefs, {Key,Value}),
	    ok
    end.

delete_scene_value() ->
    true = ets:delete_all_objects(wings_scene_prefs),
    ok.

delete_scene_value(Key) ->
    true = ets:delete(wings_scene_prefs, Key),
    ok.

%%% End of scene prefs


defaults() ->
    wings_theme:native_theme()++
    [
     %% Put any non-constant preferences here.
     {jumpy_camera,os:type() =:= {unix,darwin}},

     %% Advanced menus are always turned on now.
     %% The default must still be false for compatibility
     %% with older Wings versions. (We force it to true
     %% later after laoding the user's preference file.)
     {advanced_menus,false},
     {no_basic_menu_info,true},

     %% The remaining items are constants. The generated code
     %% will be much more compact.
     {info_enhanced_text,false},
     {vertex_size,4.0},
     {selected_vertex_size,5.0},
     {masked_vertex_size,8.0},
     {edge_width,2.0},
     {selected_edge_width,2.0},
     {hard_edge_width,2.0},
     {constrain_axes,true},
     {mini_axis,true},
     {force_show_along_grid,false},
     {force_ortho_along_axis,false},
     {body_hilite,true},
     {auto_rotate_angle,5.0},
     {autosave,true},
     {autosave_time,10},
     {active_vector_size,1.0},
     {active_vector_width,2.0},
     {normal_vector_size,0.3},
     {normal_vector_width,2.0},
     {hide_sel_while_dragging,false},
     {hide_sel_in_camera_moves,false},
     {negative_height,24},
     {negative_width,36},

     %% Compatibility preferences.
     {dummy_axis_letter,false},
     {polygon_offset_f,1.0},
     {polygon_offset_r,1.0},
     {multisample, true},
     {ungrab_bug, false},

     %% Advanced features.
     {default_commands,false},
     {clip_plane_size,1.5},
     {hilite_select,false},
     {conditional_deselect, false},

     %% Constraints preferences.
     {con_dist_alt,10.0},
     {con_dist_shift,1.0},
     {con_dist_ctrl,0.10},
     {con_dist_ctrl_shift,0.010},
     {con_dist_ctrl_alt,0.0010},
     {con_dist_shift_alt,0.250},
     {con_dist_ctrl_shift_alt,0.0250},
     {con_dist_a_alt,1/64},
     {con_dist_a_shift,1/4},
     {con_dist_a_ctrl,1/8},
     {con_dist_a_ctrl_shift,1/32},
     {con_dist_a_ctrl_alt,5/8},
     {con_dist_a_shift_alt,1/16},
     {con_dist_a_ctrl_shift_alt,3/64},
     {con_rot_alt,180.0},
     {con_rot_shift,15.0},
     {con_rot_ctrl,1.0},
     {con_rot_ctrl_shift,0.10},
     {con_rot_ctrl_alt,22.50},
     {con_rot_shift_alt,135.0},
     {con_rot_ctrl_shift_alt,144.0},
     {con_scale_alt,10.0},
     {con_scale_shift,1.0},
     {con_scale_ctrl,0.10},
     {con_scale_ctrl_shift,0.010},
     {con_scale_ctrl_alt,0.0010},
     {con_scale_shift_alt,0.250},
     {con_scale_ctrl_shift_alt,0.0250},
     {con_alternate,false},
     {con_dist_set,con_dist_},
     {con_dist_default,alt},
     {con_rot_default,alt},
     {con_scale_default,alt},

     %% Drag preferences.
     {drag_custom,false},
     {drag_speed_abs,8.5},
     {drag_speed_relative,8.5},
     {drag_speed_rotate,8.5},

     %% Proxy preferences.
     {proxy_shaded_edge_style,some},
     {proxy_static_opacity,1.0},
     {proxy_moving_opacity,1.0},
     {proxy_opencl_level, 0},

     %% User interface preferences.
     {interface_icons,bluecube},
     {aa_edges,false},
     {show_toolbar, true},
     {extended_toolbar,true},
     {bitmap_icons, false},
     {menu_toolbar, true},
     {menu_toolbar_snap_cursor, false},
     {menu_toolbar_size, big},

     %% Console
     {console_width,80},
     {console_height,12},
     {console_save_lines,100},

     %% Undos.
     {num_undo_levels,32},

     %% Languages.
     {language,"en"},

     %% Develop menu.
     {show_develop_menu,false},

     %% Max Menu Length
     {max_menu_height,0}
    ].

clean(List) ->
    clean(List, []).

clean([{Key,Val}=Pair|T], Acc) ->
    case not_bad(Key, Val) of
	true -> clean(T, [Pair|Acc]);
	false ->
	    io:format("Removed pref: ~p\n", [Pair]),
	    clean(T, Acc)
    end;
clean([{{bindkey,Key},{vector,{pick,[],Res0,Ns}},Type}|T], Acc) ->
    Res = list_to_tuple(reverse(Res0)),
    Mode = last(Ns),
    Bk = {{bindkey,Mode,Key},build_command(Res, Ns),Type},
    clean(T, [Bk|Acc]);
clean([{{bindkey,_}=Bk,{view,{virtual_mirror,Cmd}},user}|T], Acc) ->
    clean(T, [{Bk,{tools,{virtual_mirror,Cmd}},user}|Acc]);
clean([{{bindkey,_}=Bk,{tools,tweak},user}|T], Acc) ->
    clean(T, [{Bk,{tools,{tweak,false}},user}|Acc]);

clean([{{bindkey,_,_}=Bk,{face,{Cmd,Dir}},user}|T], Acc)
        when Cmd==extrude; Cmd==extrude_region; Cmd==extract_region; Cmd==extract_faces ->
    {Type,Mode} = case Cmd of
        extrude_region -> {extrude,region};
        extrude -> {extrude,faces};
        extract_region -> {extract,region};
        extract_faces -> {extract,faces}
    end,
    C = case Dir of
      {E,_} when E==region; E==faces -> Dir;
      Dir -> {Mode,Dir}
    end,
    clean(T, [{Bk,{face,{Type,C}},user}|Acc]);

clean([{{bindkey,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([{{bindkey,_,_},Cmd,user}=Bk|T], Acc) ->
    case bad_command(Cmd) of
	false -> clean(T, [Bk|Acc]);
	true ->
	    io:format("Removed pref: ~p\n", [Bk]),
	    clean(T, Acc)
    end;
clean([H|T], Acc) ->
    clean(T, [H|Acc]);
clean([], Acc) -> Acc.

%% First, get rid of obsolete stuff.
not_bad(last_point, _) -> false;
not_bad(default_point, _) -> false;
not_bad(smooth_preview, _) -> false;
not_bad(wire_mode, _) -> false;
not_bad(none, _) -> false;
not_bad(use_front_buffer, _) -> false;
not_bad(one_button_mouse, _) -> false;
not_bad(face_color, _) -> false;
not_bad(workmode, _) -> false;
not_bad(orthogonal_view, _) -> false;
not_bad(show_memory_used, _) -> false;
not_bad(show_axes, _) -> false;
not_bad(show_cam_imageplane, _) -> false;
not_bad(show_groundplane, _) -> false;
not_bad(current_view, _) -> false;
not_bad(camera_fov, _) -> false;
not_bad(camera_hither, _) -> false;
not_bad(camera_yon, _) -> false;
not_bad(right_click_sel_in_ss, _) -> false;
not_bad(right_click_sel_in_geom, _) -> false;
not_bad(wire_edge_color, _) -> false;
not_bad(show_wire_backfaces, _) -> false;	%Now a window property.
not_bad(smoothed_preview_cage, _) -> false;
not_bad(smoothed_preview_edges, _) -> false;
not_bad(contour, _) -> false;
not_bad(material_hole, _) -> false;

%% Crashes have occurred.
not_bad(last_axis, Val) -> is_wings_vector(Val);
not_bad(default_axis, Val) -> is_wings_vector(Val);
not_bad(magnet_radius, Val) -> is_number(Val);
not_bad(_, _) -> true.

is_wings_vector({{Px,Py,Pz},{Vx,Vy,Vz}})
  when is_number(Px), is_number(Py), is_number(Pz),
       is_number(Vx), is_number(Vy), is_number(Vz) ->
    true;
is_wings_vector(_) -> false.

bad_command({_,{rotate,Atom}}) when is_atom(Atom) -> true;
bad_command({view,virtual_mirror}) -> true;
bad_command({select,edge_loop}) -> true;
bad_command({select,select_region}) -> true;
bad_command({select,edge_ring}) -> true;
bad_command({select,prev_edge_loop}) -> true;
bad_command({select,next_edge_loop}) -> true;
bad_command({tweak,{screen,1}}) -> true;
bad_command({tweak,{normal,1}}) -> true;
bad_command({tweak,{tangent,1}}) -> true;
bad_command({tweak,{constrainXYZ,_}}) -> true;
bad_command(_) -> false.

build_command(Name, Names) ->
    foldl(fun(N, A) -> {N,A} end, Name, Names).

%% check for default keys replaced by user defined ones and remove them
check_user_keys(List0) ->
    List=lists:foldl(fun({{bindkey,_},_,user}=Hk, Acc) ->
            Acc++[Hk];
        ({{bindkey,_,_},_,user}=Hk, Acc) ->
            Acc++[Hk];
        (_, Acc) -> Acc
    end,[],List0),
    lists:foreach(fun({_,Cmd,user}) ->
        Clst=wings_hotkey:hotkeys_by_commands([Cmd]),
        lists:foreach(fun({Key,_,_,default}) ->
                delete_value(Key);
            (_) -> ok
        end,Clst)
    end,List).

%% Check for legacy colors so as not to overwrite older prefs with new defaults
check_legacy_colors(List) ->
    case get_value(legacy_colors_checked) of
      true -> ok;
      _ ->
        foreach(fun({Key,Value}) ->
          case lists:keymember(Key, 1, List) of
            true -> ok;
            _ -> set_value(Key, Value)
          end
        end, wings_theme:legacy_colors()),
        set_value(legacy_colors_checked, true)
    end.

colors(ColorPrefs, List) ->
%% Return a list of {key,value} pairs, for the ColorPrefs keys found in List
    foldl(fun({Pref,_},Acc) ->
        case lists:keyfind(Pref, 1, List) of
          false -> Acc;
          P -> [P|Acc]
        end
    end,[],ColorPrefs).

%% Load or Save a basic preference file (not the main Wings preference file)
%% Command is initiated in wings_file.erl
pref({load,Request,St}) ->
    case Request of
	custom_theme ->
	    pref(load);
	Theme when is_atom(Theme) ->
	    Colors = wings_theme:Theme(),
	    load_pref_category([{graphical,true}],[{graphical,Colors}],St),
	    init_opengl(),
	    keep;
	Key when is_integer(Key) ->
	    Recent0 = get_value(recent_prefs),
	    PrefDir = lists:nth(Key, Recent0),
	    case filelib:is_file(PrefDir) of
		true ->
		    wings_pref:set_value(pref_directory, PrefDir),
		    pref(load);
		false ->
		    Recent = delete_nth(Recent0, Key),
		    wings_pref:set_value(recent_prefs, Recent),
		    wings_u:error_msg(?__(11,"This file has been moved or deleted."))
	    end
    end;

pref(Action) -> %% load|save dialog
    FileName = "Preference Subset.pref",
    Directory = case get_value(pref_directory) of
		    undefined -> get_pref_directory(FileName);
		    Dir -> Dir
		end,

    Keys = case file:consult(Directory) of
	       {ok,List} when Action =:= load -> orddict:fetch_keys(List);
	       _ -> []
	   end,
    Disable = fun (_Var, Enable, Store) ->
		      Action =:= load andalso
			  wings_dialog:enable(hotkey_radio, Enable, Store)
              end,
    case Action of
        load ->
	    Title = ?__(1,"Load Preference Subset"),
	    Dialog = open_dialog,
	    Options =
		[separator,
		 {vradio,[{?__(9,"Merge hotkeys"),merge},
			  {?__(10,"Remove existing hotkeys first"),remove}],
		  merge, [{key, hotkey_radio}]},
		 panel];
        save ->
	    Title = ?__(2,"Save Preference Subset"),
	    Dialog = save_dialog,
	    Options = [panel]
    end,
    PrefFeilds =
        [{hframe,
          [{vframe,
            [{?__(3,"Graphical Settings"),member(graphical,Keys),[{key,graphical}]},
             {?__(4,"Camera Settings"),member(camera,Keys),[{key,camera}]},
             {?__(5,"Hotkeys"),member(hotkeys,Keys),[{key,hotkeys}, {hook, Disable}]}]},
           {vframe,
            [{?__(6,"Window and View Settings"),member(windows,Keys),[{key,windows}]},
             {?__(7,"Constraints"),member(constraints,Keys),[{key,constraints}]},
             {?__(8,"General Settings"),member(settings,Keys),[{key,settings}]}]}
	  ]}],
    FileBrowser =
        [{button, {text, Directory,
		   [{key, pref_directory},
		    {props, [{dialog_type, Dialog},
			     {extensions, [{".pref", "Preference Subset"}]}]}]}}],
    Qs = PrefFeilds ++ Options ++ FileBrowser,
    Do = fun(Res) ->
		 case lists:keyfind(update, 1, Res) of
		     {_,true} ->
			 {_,PrefDir} = lists:keyfind(pref_directory, 1, Res),
			 case filelib:is_file(PrefDir) of
			     true -> wings_pref:set_value(pref_directory, PrefDir);
			     false -> ok
			 end,
			 {file,{load_pref,custom_theme}};
		     _ -> {file,{pref,{Action,Res}}}
		 end
	 end,
    wings_dialog:dialog(Title, Qs, Do).

pref({save, Res}, St) -> %save a .pref
    DelayedPrefs = ets:tab2list(wings_delayed_update),
    Prefs0 = prune_temp(ets:tab2list(wings_state)),
    List = foldl(fun({Key, _}=Pref, Acc) ->
            lists:keystore(Key, 1, Acc, Pref)
        end, Prefs0, DelayedPrefs),
    Defaults = defaults(),
    Prefs = save_pref_category(Res, List, Defaults, St, []),
    {_,Dir} = lists:keyfind(pref_directory,1,Res),
    write_pref(Dir, Prefs);
pref({load, Res0}, St) -> %% load a .pref
    Res = treat_hotkeys(Res0),
    {_,Dir} = lists:keyfind(pref_directory,1,Res),
    case lists:suffix(".pref",Dir) of
      true ->
        case local_consult(Dir) of
          {ok,List} ->
            load_pref_category(sort(Res),List,St),
            init_opengl();
          {error,Reason} ->
            io:format(".pref loading error: ~p\n",[Reason]),
            ok
        end;
      false ->
        wings_u:error_msg(?__(13,"Not a .pref file"))
    end.

%% Find the preferenece directory
get_pref_directory(FileName) ->
    [{Dir, _}|_] = pref_dirs(),
    filename:join(Dir, FileName).

write_pref(Dir, ColorPref) ->
    Format = "~p. \n",
    PostProcess = case os:type() of
                      {win32,_} -> fun insert_crs/1;
                      _ -> fun(L) -> L end
                  end,
    Write = fun(Entry) -> PostProcess(io_lib:format(Format, [Entry])) end,
    Str = lists:map(Write, ColorPref),
    catch file:write_file(Dir, Str),
    update_recent_prefs(Dir),
    set_value(pref_directory,Dir).

treat_hotkeys(Res) ->
    case proplists:get_value(hotkeys, Res, false) of
	false -> Res;
	true ->
	    case proplists:get_value(hotkey_radio, Res) of
		merge -> Res;
		remove ->
		    List = prune_temp(ets:tab2list(wings_state)),
		    lists:foreach(fun({Hotkey,_,_})
					when element(1, Hotkey) =:= bindkey ->
					  ets:delete(wings_state,Hotkey);
				     (_) -> ok
				  end, List),
		    Res
	    end
    end.

init_opengl() ->
    wings_wm:dirty().

update_recent_prefs(Dir) ->
    Recent0 = get_value(recent_prefs,[]),
    PrevLen = length(Recent0),
    Recent1 = [Dir|lists:delete(Dir,Recent0)],
    Recent = lists:sublist(Recent1, 5),
    Update = fun(File, N) when N =< PrevLen ->
                     Pref = filename:basename(File),
                     wings_menu:update_menu(file, {load_pref, N}, Pref, recent_pref_help()),
                     N+1;
                (_, N) ->
                     N
             end,
    Next = lists:foldl(Update, 1, Recent),
    case length(Recent) > PrevLen of
        false -> ok;
        true -> %% Add the last one
            Pref = filename:basename(Recent),
            wings_menu:update_menu(file, {load_pref, Next},
                                   {append, -1, Pref},
                                   recent_pref_help())
    end,
    set_value(recent_prefs, Recent).

recent_prefs() ->
    Recent = get_value(recent_prefs,[]),
    recent_prefs(Recent,1).
recent_prefs([Dir|Recent],N) ->
    case lists:suffix(".pref",Dir) of
        true ->
            Pref = filename:basename(Dir),
            PrefEntries = [{Pref,N,recent_pref_help()}|recent_prefs(Recent,N+1)],
            case N of
                1 -> [separator|PrefEntries];
                _ -> PrefEntries
            end;
        false ->
            P0 = get_value(recent_prefs),
            P = P0 -- Dir,
            set_value(recent_prefs,P),
            recent_prefs(Recent,N)
    end;
recent_prefs([],_) -> [].

recent_pref_help() -> ?__(1,"Load a recent Preference Subset").

find_size_string([$s,$i,$z,$e|_]) -> true;
find_size_string([$w,$i,$d,$t,$h|_]) -> true;
find_size_string([$p,$r,$o,$x,$y|_]) -> true;
find_size_string([_|Str]) ->
    find_size_string(Str);
find_size_string([]) -> false.

find_graphical_string("show_develop_menu") -> false;
find_graphical_string("show_colors") -> false;
find_graphical_string("last_axis") -> false;
find_graphical_string("tweak_axis") -> false;
find_graphical_string("tweak_axis_toggle") -> false;
find_graphical_string("legacy_colors_checked") -> false;
find_graphical_string("aa_edges") -> true;

find_graphical_string([$h,$i,$g,$h,$l,$i,$g,$h,$t|_]) -> false;
find_graphical_string([$f,$o,$r,$c,$e|_]) -> false;
find_graphical_string([$c,$o,$l,$o,$r|_]) -> true;
find_graphical_string([$l,$a,$n,$g|_]) -> false;
find_graphical_string([$f,$o,$n,$t|_]) -> true;
find_graphical_string([$_,$h,$i,$l,$i,$t,$e|_]) -> true;
find_graphical_string([$t,$o,$o,$l,$b,$a,$r|_]) -> true;
find_graphical_string([$p,$r,$o,$x,$y|_]) -> true;
find_graphical_string([$l,$i,$g,$h,$t|_]) -> true;
find_graphical_string([$o,$b,$j,$e,$c,$t,$s,$_,$i,$n,$_|_]) -> true;
find_graphical_string([$i,$c,$o,$n|_]) -> true;
find_graphical_string([$a,$x,$i,$s|_]) -> true;
find_graphical_string([$s,$e,$l,$e,$c,$t,$i,$o,$n,$_,$s|_]) -> true;
find_graphical_string([_|Str]) -> find_graphical_string(Str);
find_graphical_string([]) -> false.

find_const_string([$c,$o,$n,$_|_]) -> true;
find_const_string([_|Str]) -> find_const_string(Str);
find_const_string([]) -> false.

find_cam_string([$n,$u,$m,$_,$b,$u,$t,$t,$o|_]) -> true;
find_cam_string([$n,$e,$g,$a,$t,$i,$v,$e,$_|_]) -> true;
find_cam_string([$h,$i,$g,$h,$l,$i,$g,$h,$t,$_,$a,$i,$m|_]) -> true;
find_cam_string([$c,$a,$m,$_|_]) -> true;
find_cam_string([$p,$a,$n,$_|_]) -> true;
find_cam_string([$z,$o,$o,$m|_]) -> true;
find_cam_string([$w,$h,$_|_]) -> true;
find_cam_string([$w,$h,$e,$e,$l,$_|_]) -> true;
find_cam_string([$v,$i,$e,$w,$e,$r,$_,$f|_]) -> true;
find_cam_string([$g,$_,$c,$a,$m,$_|_]) -> false;
find_cam_string([$c,$a,$m,$e,$r,$a|_]) -> true;
find_cam_string([_|Str]) -> find_cam_string(Str);
find_cam_string([]) -> false.

%% Save .pref
save_pref_category([{pref_directory,_}|Options], List, Defaults, St, Acc) ->
    save_pref_category(Options, List, Defaults, St, Acc);
save_pref_category([{constraints,Bool}|Options], List, Defaults, St, Acc0) ->
    Const = foldl(fun({Key,Value}, A) when is_atom(Key),is_float(Value);
                                           is_atom(Key),is_atom(Value) ->
                        KeyStr = atom_to_list(Key),
                        case find_const_string(KeyStr) of
                          true ->
                            [lists:keyfind(Key, 1, List)|A];
                          false -> A
                        end;
                     (_, A) -> A
        end, [], Defaults),
    Acc = if Bool -> [{constraints,sort(Const)}|Acc0]; true -> Acc0 end,
    save_pref_category(Options, List--Const, Defaults--Const, St, Acc);
save_pref_category([{hotkeys,Bool}|Options], List, Defaults, St, Acc0) ->
    Hotkeys = foldl(fun({_,_,_}=H, A) ->
                          [H|A];
                       (_,A) -> A
                    end, [], List),
    Acc = if Bool -> [{hotkeys,sort(Hotkeys)}|Acc0]; true -> Acc0 end,
    save_pref_category(Options, List--Hotkeys, Defaults--Hotkeys, St, Acc);
save_pref_category([{camera,Bool}|Options], List, Defaults, St, Acc0) ->
    Camera = foldl(fun({_,Value},A) when  is_tuple(Value) -> A;
                     ({{_,_},_},A) -> A;
                     ({Key,_}=P, A) when is_atom(Key)->
                        KeyStr = atom_to_list(Key),
                        case find_cam_string(KeyStr) of
                          true -> [P|A];
                          false -> A
                        end;
                     (_,A) -> A
        end, [], List),
    Acc = if Bool -> [{camera,sort(Camera)}|Acc0]; true -> Acc0 end,
    save_pref_category(Options, List--Camera, Defaults--Camera, St, Acc);
save_pref_category([{settings,Bool}|Options], List, Defaults, St, Acc0) ->
    Settings = foldl(fun({recent_files,_},A) -> A;
                   ({last_axis,_},A) -> A;
                   ({current_directory,_},A) -> A;
                   ({recent_prefs,_},A) -> A;
                   ({_,_}=P,A) -> [P|A];
                   (_, A) -> A
        end, [], List),
    Acc = if Bool -> [{settings,sort(Settings)}|Acc0]; true -> Acc0 end,
    save_pref_category(Options, List--Settings, Defaults--Settings, St, Acc);
save_pref_category([{graphical,Bool}|Options], List, Defaults, St, Acc0) ->
    Visual = foldl(fun({Key,Value}, A) when is_atom(Key),is_float(Value) ->
                         KeyStr = atom_to_list(Key),
                         case find_size_string(KeyStr) of
                           true ->
                             [lists:keyfind(Key, 1, List)|A];
                           false -> A
                         end;
                    % ({_,Value},A) when is_tuple(Value) -> A;
                     ({Key,_},A) when is_atom(Key) ->
                         KeyStr = atom_to_list(Key),
                         case find_graphical_string(KeyStr) of
                           true ->
                             [lists:keyfind(Key, 1, List)|A];
                           false -> A
                         end;
                     (_,A) -> A
        end, [], List),
    LegacyColors = wings_theme:legacy_colors(),
    Colors = colors(LegacyColors, List),
    CS = lists:usort(lists:merge(Visual,Colors)),
    Acc = if Bool -> [{graphical,CS}|Acc0];
             true -> Acc0 end,
    save_pref_category(Options, List--CS, Defaults--CS, St, Acc);
save_pref_category([{windows,Bool}|Options], List, Defaults, St, Acc0) ->
    C1 = lists:keyfind(console_height,1,List),
    C2 = lists:keyfind(console_save_lines,1,List),
    C3 = lists:keyfind(console_width,1,List),
    WinSize = lists:keyfind(window_size,1,List),
    WCWF = wings_frame:export_layout(),
    Windows = [WinSize,C1,C2,C3],
    Acc = if Bool -> [{saved_windows2,WCWF}|Acc0];
             true -> Acc0
          end,
    NewList = (List--Windows)--[{saved_windows2,WCWF}],
    save_pref_category(Options, NewList,  Defaults--Windows, St, Acc);
save_pref_category([], _, _, _, Acc) -> Acc.

%% Load .pref
load_pref_category([{pref_directory,Dir}|Options], List, St) ->
    set_value(pref_directory,Dir),
    update_recent_prefs(Dir),
    load_pref_category(Options, List, St);
load_pref_category([{_,false}|Options], List, St) ->
    load_pref_category(Options, List, St);
load_pref_category([{graphical,true}|Options], List, St) ->
    %% Load color preferences
    Colors = case lists:keyfind(graphical, 1, List) of
		 {_,C} -> C;
		 false -> []
	     end,
    catch wings_pref_dlg:set_values(Colors, St),
    load_pref_category(Options, List, St);
load_pref_category([{windows,true}|Options], List, St) ->
    %% Load preference windows and remove any old windows
    case lists:keyfind(saved_windows2, 1, List) of
        {saved_windows2, {_WC,_WF} = WCWF} ->
            wings_frame:import_layout(WCWF, St);
        _ -> ok
    end,
    load_pref_category(Options, List, St);
%% Load Hotkeys
load_pref_category([{hotkeys,true}|Options], List, St) ->
    case lists:keyfind(hotkeys, 1, List) of
        {_,Prefs} ->
	    foreach(fun(Hkey) ->
			    ets:insert(wings_state, Hkey)
		    end, clean(Prefs));
        false -> wings_hotkey:set_default()
    end,
    load_pref_category(Options, List, St);
%% Load all other prefs
load_pref_category([{Other,true}|Options], List, St) ->
    case lists:keyfind(Other, 1, List) of
        {_,Prefs} ->
	    foreach(fun(P) ->
			    ets:insert(wings_state, P)
		    end, clean(Prefs));
        false -> ok
    end,
    load_pref_category(Options, List, St);
load_pref_category([{hotkey_radio, _}|Options], List, St) ->
    load_pref_category(Options, List, St);
load_pref_category([], _, _) -> ok.

delete_nth([_|T], 1) -> T;
delete_nth([H|T], N) -> [H|delete_nth(T, N-1)];
delete_nth([], _) -> [].
