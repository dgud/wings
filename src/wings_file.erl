%%
%%  wings_file.erl --
%%
%%     This module contains the commands in the File menu.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_file).
-export([init/0,init_autosave/0,menu/0,command/2]).
-export([import_filename/2,export_filename/2,export_filename/3]).
-export([unsaved_filename/0,del_unsaved_file/0,autosave_filename/1]).
-export([file_filters/1]).

-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("kernel/include/file.hrl").

-import(lists, [foldl/3,foreach/2,keymember/3,reverse/1]).
-import(filename, [dirname/1]).

-define(WINGS, ".wings").
-define(UNSAVED_NAME, "unsaved" ++ ?WINGS).

%% export_filename([Prop], St, Continuation).
%%   The St will only be used to setup the default filename.
%%   The Continuation fun will be called like this: Continuation(Filename).
export_filename(Prop0, #st{file=File}, Cont) ->
    Prop = case proplists:get_value(ext, Prop0) of
	       undefined -> Prop0;
	       Ext ->
		   BaseName =
		       case File of
                           undefined -> "untitled";
                           _ -> filename:basename(File)
		       end,
		   Def = filename:rootname(BaseName, ?WINGS) ++ Ext,
		   [{default_filename,Def}|Prop0]
	   end,
    export_filename(Prop, Cont).

%% import_filename([Prop], Continuation).
%%   The Continuation fun will be called like this: Continuation(Filename).
import_filename(Prop, Cont) ->
    case get(wings_not_running) of
	undefined ->
	    import_filename_1(Prop, Cont);
	{import, FileName} ->
	    Cont(FileName)
    end.
import_filename_1(Ps0, Cont) ->
    This = wings_wm:this(),
    Dir = wings_pref:get_value(current_directory),
    String = case os:type() of
		 {win32,_} -> "Import";
		 _Other    -> ?__(1,"Import")
	     end,
    Ps = Ps0 ++ [{title,String},{directory,Dir}],
    Fun = fun(Name0) ->
		  Name=test_unc_path(Name0),
		  case catch Cont(Name) of
		      {command_error,Error} ->
			  wings_u:message(Error);
		      #st{}=St ->
			  set_cwd(dirname(Name)),
			  wings_wm:send(This, {new_state,St});
		      Tuple when is_tuple(Tuple) ->
			  wings_wm:send(This, {action,Tuple});
		      ignore -> keep;
		      keep -> keep
		  end
	  end,
    wings_plugin:call_ui({file,open_dialog,Ps,Fun}).

%% export_filename([Prop], Continuation).
%%   The Continuation fun will be called like this: Continuation(Filename).
export_filename(Prop, Cont) ->
    case get(wings_not_running) of
	undefined ->
	    export_filename_1(Prop, Cont);
	{export, FileName} ->
	    Cont(FileName)
    end.

export_filename_1(Prop0, Cont) ->
    This = wings_wm:this(),
    Dir = wings_pref:get_value(current_directory),
    Prop = Prop0 ++ [{directory,Dir}],
    Fun = fun(Name0) ->
		  Name=test_unc_path(Name0),
		  case catch Cont(Name) of
		      {command_error,Error} ->
			  wings_u:message(Error);
		      #st{}=St ->
			  set_cwd(dirname(Name)),
			  wings_wm:send(This, {new_state,St});
		      Tuple when is_tuple(Tuple) ->
			  wings_wm:send(This, {action,Tuple});
		      ignore -> keep;
		      keep -> keep;
		      ok -> keep
		  end
	  end,
    String = case os:type() of
		 {win32,_} -> "Export";
		 _Other    -> ?__(1,"Export")
	     end,
    wings_plugin:call_ui({file,save_dialog,Prop++[{title,String}],Fun}).

init() ->
    wings_pref:set_default(save_unused_materials,false),
    case wings_pref:get_value(current_directory) of
	undefined ->
	    case file:get_cwd() of
		{ok,Cwd} -> wings_pref:set_value(current_directory, Cwd);
		{error,_} -> wings_pref:set_value(current_directory, "/")
	    end;
	Cwd ->
	    case filelib:is_dir(Cwd) of
		false ->
		    wings_pref:delete_value(current_directory),
		    init();
		true -> ok
	    end
    end.

menu() ->
    ImpFormats = [{"Nendo (.ndo)...",ndo}],
    ExpFormats = [{"Nendo (.ndo)...",ndo}],
    Tail = [{?__(25,"Exit"),quit,?__(28,"Exit Wings 3D")}],
    [{?__(3,"New"),new,
      ?__(4,"Create a new, empty scene")},
     {?__(5,"Open..."),open,
      ?__(6,"Open a previously saved scene")},
     {?__(7,"Merge..."),merge,
      ?__(8,"Merge a previously saved scene into the current scene")},
      separator,
     {?__(9,"Save"),save,
      ?__(10,"Save the current scene")},
     {?__(11,"Save As..."),save_as,
      ?__(12,"Save the current scene under a new name")},
     {?__(13,"Save Selected..."),save_selected,
      ?__(14,"Save only the selected objects or faces")},
     {?__(15,"Save Incrementally"),save_incr,
      ?__(26,"Generate new filename and save")},
      %% if there are more options we'll make a panel
     {?__(29,"Save Unused Materials"),save_unused_materials,
      ?__(30,"Include unused materials when saving a .wings file"),
      save_unused_mats()},
     separator,
     {?__(16,"Revert"),revert,
      ?__(17,"Revert current scene to the saved contents")},
      separator,
     {?__(18,"Import"),{import,ImpFormats}},
     {?__(19,"Export"),{export,ExpFormats}},
     {?__(20,"Export Selected"),{export_selected,ExpFormats}},
     separator,
     {?__(21,"Import Image..."),import_image,?__(22,"Import an image file")},
     separator,
     {?__(23,"Render"),{render,[]}},
     separator,
     {?__(24,"Install Plug-In"),install_plugin,
      ?__(27,"Install a plug-in")},
     separator,
     {?__(31,"Save Preference Subset..."),save_pref,
      ?__(32,"Save a preference subset from your current settings")},
     {?__(33,"Load Preference Subset"),
       {load_pref,
         [{?__(35,"Load..."),custom_theme,
           ?__(36,"Load a previously saved preference subset")}]
          ++wings_pref:recent_prefs()}},
     separator|recent_files(Tail)].

save_unused_mats() ->
    wings_menu_util:crossmark(save_unused_materials).

command(new, St) ->
    new(St);
command(confirmed_new, St) ->
    del_unsaved_file(),
    confirmed_new(St);
command(open, St) ->
    open(St);
command(confirmed_open_dialog, _) ->
    confirmed_open_dialog();
command({confirmed_open,Filename}, St) ->
    del_unsaved_file(),
    confirmed_open(Filename, St);
command({confirmed_open,Next,Filename}, _) ->
    Next(Filename);
command(merge, _) ->
    merge();
command({merge,Filename}, St) ->
    merge(Filename, St);
command(save, St) ->
    save(ignore, St);
command({save,Next}, St) ->
    save(Next, St);
command(save_as, St) ->
    save_as(ignore, St);
command({save_as,{Filename,Next}}, St) ->
    save_now(Next, St#st{file=Filename});
command(save_selected, St) ->
    save_selected(St);
command({save_selected,Filename}, St) ->
    save_selected(Filename, St);
command(save_incr, St) ->
    save_incr(St);
command(revert, St0) ->
    case revert(St0) of
	{error,Reason} ->
	    wings_u:error_msg(?__(1,"Revert failed: ") ++ Reason),
	    St0;
	#st{}=St -> {save_state,St}
    end;
command(save_unused_materials, St) ->
    Bool = wings_pref:get_value(save_unused_materials),
    wings_pref:set_value(save_unused_materials, not Bool),
	St;
command({import,ndo}, _St) ->
    import_ndo();
command({import,{ndo,Filename}}, St) ->
    import_ndo(Filename, St);
command(import_image, _St) ->
    import_image();
command({import_image,Name}, _) ->
    import_image(Name);
command({import_files, Fs}, St) ->
    {save_state, wpa:import(Fs, St)};
command({export,ndo}, St) ->
    String = case os:type() of
        {win32,_} -> "Export";
        _Other    -> ?__(2,"Export")
    end,
    export_ndo(export, String, St);
command({export_selected,ndo}, St) ->
    String = case os:type() of
        {win32,_} -> "Export Selected";
        _Other    -> ?__(3,"Export Selected")
    end,
    export_ndo(export_selected, String, St);
command({export,{ndo,Filename}}, St) ->
    do_export_ndo(Filename, St);
command({export_selected,{ndo,Filename}}, St0) ->
    St = delete_unselected(St0),
    do_export_ndo(Filename, St);
command(install_plugin, _St) ->
    install_plugin();
command({install_plugin,Filename}, _St) ->
    wings_plugin:install(Filename);

command(save_pref, _St) ->
    wings_pref:pref(save);
command({load_pref,Request}, St) ->
    wings_pref:pref({load,Request,St});
command({pref,Request}, St) ->
    wings_pref:pref(Request,St),
    keep;

command(quit, #st{saved=true}) ->
    quit;
command(quit, _) ->
    wings_u:yes_no_cancel(?__(4,"Do you want to save your changes before quitting?"),
			  fun() -> {file,{save,{file,quit}}} end,
			  fun() -> {file,confirmed_quit} end);
command(confirmed_quit, _) ->
    del_unsaved_file(),
    quit;
command({recent_file,Key}, St) when is_integer(Key), 1 =< Key ->
    Recent0 = wings_pref:get_value(recent_files, []),
    {_,File} = lists:nth(Key, Recent0),
    case filelib:is_file(File) of
	true ->
	    named_open(File, St);
	false ->
	    Last = length(Recent0),
	    Recent = delete_nth(Recent0, Key),
	    wings_pref:set_value(recent_files, Recent),
	    wings_menu:update_menu(file, {recent_file, Last}, delete, []),
	    lists:foreach(fun({Str, RKey, Help}) ->
				  wings_menu:update_menu(file, RKey, Str, Help);
			     (separator) -> ok
			  end, recent_files(Recent, [])),
	    wings_u:error_msg(?__(5,"This file has been moved or deleted."))
    end.

delete_nth([_|T], 1) -> T;
delete_nth([H|T], N) -> [H|delete_nth(T, N-1)];
delete_nth([], _) -> [].

confirmed_new(#st{file=File}=St) ->
    %% Remove autosaved file; user has explicitly said so.
    catch file:delete(autosave_filename(File)),
    new(St#st{saved=true}).

new(#st{saved=true}=St0) ->
    St1 = clean_st(St0#st{file=undefined}),
    %% clean_st/1 will remove all saved view, but will not reset the view. For a new project we should reset it.
    wings_frame:reinit_layout(),
    wings_view:reset(),
    St2 = clean_images(wings_undo:init(St1)),
    St = wings_obj:create_folder_system(St2),
    wings_u:caption(St),
    {new,St#st{saved=true}};
new(#st{}=St0) ->		      %File is not saved or autosaved.
    wings_u:caption(St0#st{saved=false}),
    wings_u:yes_no_cancel(str_save_changes(),
			  fun() -> {file,{save,{file,new}}} end,
			  fun() -> {file,confirmed_new} end).

open(#st{saved=Saved}=St) ->
    case Saved =:= true orelse wings_obj:num_objects(St) =:= 0 of
        true ->
            confirmed_open_dialog();
        false ->
            %% Clear any autosave flag.
            wings_u:caption(St#st{saved=false}),
            Confirmed = {file,confirmed_open_dialog},
            wings_u:yes_no_cancel(str_save_changes(),
                                  fun() -> {file,{save,Confirmed}} end,
                                  fun() -> Confirmed end)
    end.

confirmed_open_dialog() ->
    %% All confirmation questions asked. The former contents has either
    %% been saved, or the user has accepted that it will be discarded.
    %% Go ahead and ask for the filename.

    Cont = fun(Filename) -> {file,{confirmed_open,Filename}} end,
    Dir = wings_pref:get_value(current_directory),
    String = case os:type() of
        {win32,_} -> "Open";
        _Other    -> ?__(1,"Open")
    end,
    Ps = [{directory,Dir},
	  {title,String}|wings_prop()],
    import_filename(Ps, Cont).

confirmed_open(Name, St0) ->
    Fun = fun(File) ->
		  %% We now have:
		  %%   Name: Original name of file to be opened.
		  %%   File: Either original file or the autosave file
		  St1 = clean_st(St0#st{file=undefined}),
		  wings_frame:reinit_layout(),
		  St2 = wings_obj:create_folder_system(wings_undo:init(St1)),
		  case ?SLOW(wings_ff_wings:import(File, St2)) of
		      #st{}=St3 ->
			  set_cwd(dirname(File)),
			  St4 = clean_images(St3),
			  St = wings_obj:recreate_folder_system(St4),
			  add_recent(Name),
			  wings_u:caption(St#st{saved=true,file=Name});
		      {error,Reason} ->
			  clean_new_images(St2),
			  wings_u:error_msg(?__(1,"Read failed: ") ++ Reason)
		  end
	  end,
    use_autosave(Name, Fun).

named_open(Name, #st{saved=Saved}=St) ->
    case Saved =:= true orelse wings_obj:num_objects(St) =:= 0 of
        true ->
            confirmed_open(Name, St);
        false ->
            %%Clear any autosave flag.
            wings_u:caption(St#st{saved=false}),
            Confirmed = {file,{confirmed_open,Name}},
            wings_u:yes_no_cancel(str_save_changes(),
                                  fun() -> {file,{save,Confirmed}} end,
                                  fun() -> Confirmed end)
    end.

str_save_changes() ->
    ?__(1,"Do you want to save your changes?").

merge() ->
    Cont = fun(Filename) -> {file,{merge,Filename}} end,
    Dir = wings_pref:get_value(current_directory),
    String = case os:type() of
        {win32,_} -> "Merge";
        _Other    -> ?__(1,"Merge")
    end,
    Ps = [{title,String},{directory,Dir}|wings_prop()],
    import_filename(Ps, Cont).

merge(Name, St0) ->
    Fun = fun(File) ->
		  %% We now have:
		  %%   Name: Original name of file to be opened.
		  %%   File: Either original file or the autosave file
		  St1 = St0#st{saved=wings_image:next_id()},
		  case wings_ff_wings:merge(File, St0) of
		      {error,Reason} ->
			  clean_new_images(St1),
			  wings_u:error_msg(?__(2,"Read failed: ") ++ Reason);
		      #st{}=St ->
			  set_cwd(dirname(Name)),
			  wings_u:caption(St#st{saved=false}),
			  wings_obj:recreate_folder_system(St#st{saved=false})
		  end
	  end,
    use_autosave(Name, Fun).

save(Next, #st{saved=true}) ->
    maybe_send_action(Next);
save(Next, #st{file=undefined}=St) ->
    save_as(Next, St);
save(Next, St) ->
    save_now(Next, St).

save_as(Next, St) ->
    Cont = fun(Name) ->
       set_cwd(dirname(Name)),
       {file,{save_as,{Name,Next}}}
    end,
    Title = case os:type() of
        {win32,_} -> "Save";
        _Other    -> ?__(1,"Save")
    end,
    Ps = [{title,Title}|wings_prop()],
    export_filename(Ps, St, Cont).

save_now(Next, #st{file=Name0}=St) ->
    Name=test_unc_path(Name0),
    Backup = backup_filename(Name),
    case wings_pref:get_value(file_recovered, false) of
        true -> del_unsaved_file();
        _ -> ok
    end,
    file:rename(Name, Backup),
    file:delete(autosave_filename(Name)),
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok ->
	    set_cwd(dirname(Name)),
	    add_recent(Name),
	    maybe_send_action(Next),
	    {saved,wings_u:caption(St#st{saved=true})};
	{error,Reason} ->
	    wings_u:error_msg(?__(1,"Save failed: ") ++ Reason)
    end.

del_unsaved_file() ->
    File = autosave_filename(unsaved_filename()),
    catch file:delete(File),
    wings_pref:set_value(file_recovered, false).

test_unc_path([H|_]=FileName) when H=:=47 ->
	case string:str(FileName, "//") of
		1 -> FileName;
		_ -> "/"++FileName  % 47 is ascii code for "/"
	end;
test_unc_path(FileName) -> FileName.

maybe_send_action(ignore) -> keep;
maybe_send_action(Action) -> wings_wm:later({action,Action}).

save_selected(#st{sel=[]}) ->
    wings_u:error_msg(?__(1,"This command requires a selection."));
save_selected(St) ->
    String = case os:type() of
        {win32,_} -> "Save Selected";
        _Other    -> ?__(2,"Save Selected")
    end,
    Ps = [{title,String}|wings_prop()],
    Cont = fun(Name) -> {file,{save_selected,Name}} end,
    export_filename(Ps, St, Cont).

save_selected(Name, St0) ->
    St = delete_unselected(St0),
    case ?SLOW(wings_ff_wings:export(Name, St)) of
	ok -> keep;
	{error,Reason} -> wings_u:error_msg(Reason)
    end.

%%%
%%% Save incrementally. Original code submitted by Clacos.
%%%

save_incr(#st{saved=true}=St) -> St;
save_incr(#st{file=undefined}=St0) ->
    save_as(ignore, St0);
save_incr(#st{file=Name0}=St) ->
    Name = increment_name(Name0),
    save_now(ignore, St#st{file=Name}).

increment_name(Name0) ->
    Name1 = reverse(filename:rootname(Name0)),
    Name = case find_digits(Name1)  of
	       {[],Base} ->
		   Base ++ "_01" ++ ?WINGS;
	       {Digits0,Base} ->
		   Number = list_to_integer(Digits0),
		   Digits = integer_to_list(Number+1),
		   Zs = case length(Digits0)-length(Digits) of
			    Neg when Neg =< 0 -> [];
			    Nzs -> lists:duplicate(Nzs, $0)
			end,
		   Base ++ Zs ++ Digits ++ ?WINGS
	   end,
    update_recent(Name0, Name),
    Name.

find_digits(List) ->
    find_digits1(List, []).

find_digits1([H|T], Digits) when $0 =< H, H =< $9 ->
    find_digits1(T, [H|Digits]);
find_digits1([_|_]=Rest, Digits) ->
    {Digits,reverse(Rest)};
find_digits1([], Digits) ->
    {Digits,[]}.

wings_prop() ->
    %% Should we add autosaved wings files ??
    %% It's pretty nice to NOT see them in the file chooser /Dan

    %% Disabled translations for win32
    String = case os:type() of
        {win32,_} -> "Wings File";
        _Other    -> ?__(1,"Wings File")
    end,
    [{ext,?WINGS},{ext_desc, String}].

use_autosave(File, Body) ->
    case file:read_file_info(File) of
	{ok,SaveInfo} ->
	    use_autosave_1(SaveInfo, File, Body);
	{error, _} ->			     % use autosaved file if it exists
	    Auto = autosave_filename(File),
	    Body(case filelib:is_file(Auto) of
		     true -> Auto;
		     false -> File			%Let reader handle error.
		 end)
    end.

use_autosave_1(#file_info{mtime=SaveTime0}, File, Body) ->
    Auto = autosave_filename(File),
    case file:read_file_info(Auto) of
	{ok,#file_info{mtime=AutoInfo0}} ->
	    SaveTime = calendar:datetime_to_gregorian_seconds(SaveTime0),
	    AutoTime = calendar:datetime_to_gregorian_seconds(AutoInfo0),
	    if
		AutoTime > SaveTime ->
		    Msg = ?__(1,"An autosaved file with a later time stamp exists;"
                              " do you want to load the autosaved file instead?"),
		    wings_u:yes_no(Msg, autosave_fun(Body, Auto),
				   autosave_fun(Body, File));
		true ->
		    Body(File)
	    end;
	{error, _} ->				% No autosave file
	    Body(File)
    end.

autosave_fun(Next, Filename) ->
    fun() -> {file,{confirmed_open,Next,Filename}} end.

set_cwd(Cwd) ->
    wings_pref:set_value(current_directory, Cwd).

init_autosave() ->
    Name = autosaver,
    case wings_wm:is_window(Name) of
	true -> ok;
	false ->
	    Op = {seq,push,get_autosave_event(make_ref(), #st{saved=auto})},
	    wings_wm:new(Name, {0,0,1}, {0,0}, Op),
	    wings_wm:hide(Name),
	    wings_wm:set_dd(Name, geom_display_lists)
    end,
    wings_wm:send(Name, start_timer).

get_autosave_event(Ref, St) ->
    {replace,fun(Ev) -> autosave_event(Ev, Ref, St) end}.

autosave_event(start_timer, OldTimer, St) ->
    wings_wm:cancel_timer(OldTimer),
    case {wings_pref:get_value(autosave),wings_pref:get_value(autosave_time)} of
	{false,_} -> delete;
	{true,0} ->
	    N = 24*60,
	    wings_pref:set_value(autosave_time, N),
	    Timer = wings_wm:set_timer(N*60000, autosave),
	    get_autosave_event(Timer, St);
	{true,N} ->
	    Timer = wings_wm:set_timer(N*60000, autosave),
	    get_autosave_event(Timer, St)
    end;
autosave_event(autosave, _, St) ->
    autosave(St),
    wings_wm:later(start_timer);
autosave_event({current_state,St}, Timer, _) ->
    get_autosave_event(Timer, St);
autosave_event(_, _, _) -> keep.

autosave(#st{file=undefined} = St) ->
	autosave(St#st{file=unsaved_filename()});
autosave(#st{saved=true} = St) -> St;
autosave(#st{saved=auto} = St) -> St;
autosave(#st{file=Name}=St) ->
    Auto = autosave_filename(Name),
    %% Maybe this should be spawned to another process
    %% to let the autosaving be done in the background.
    %% But I don't want to copy a really big model either.

    %% Set the current view export views read it..
    %% Fix this later
    View = wings_wm:get_prop(geom, current_view),
    wings_view:set_current(View),
    filelib:ensure_dir(Auto),
    case ?SLOW(wings_ff_wings:export(Auto, St)) of
	ok ->
	    wings_u:caption(St#st{saved=auto});
	{error,Reason} ->
	    F = ?__(1,"Autosaving \"~s\" failed: ~s"),
	    Msg = lists:flatten(wings_util:format(F, [Auto,Reason])),
	    wings_u:message(Msg)
    end.

autosave_filename(File) ->
    Base = filename:basename(File),
    Dir = filename:dirname(File),
    filename:join(Dir, "#" ++ Base ++ "#").

unsaved_filename() ->
    Dir = wings_pref:get_dir(),
    filename:join(Dir, ?UNSAVED_NAME).

backup_filename(File) ->
    File ++ "~".

add_recent(Name) ->
    Base = filename:basename(Name),
    case filename:extension(Base) of
	?WINGS ->
	    File = {Base,Name},
	    Recent0 = wings_pref:get_value(recent_files, []),
	    Recent1 = Recent0 -- [File],
	    Recent = add_recent(File, Recent1),
	    wings_pref:set_value(recent_files, Recent);
	_Other -> ok
    end.

update_recent(Old, New) ->
    OldFile = {filename:basename(Old),Old},
    NewFile = {filename:basename(New),New},
    Recent0 = wings_pref:get_value(recent_files, []),
    Recent1 = Recent0 -- [OldFile,NewFile],
    Recent = add_recent(NewFile, Recent1),
    wings_pref:set_value(recent_files, Recent).

add_recent(NewFile, Present) ->
    Recent = add_recent_1(NewFile, Present),
    lists:foreach(fun({Str, Key, Help}) ->
			  wings_menu:update_menu(file, Key, Str, Help);
		     (separator) -> ok
		  end, recent_files(Recent, [])),
    Recent.

add_recent_1(File, [A,B,C,D,E|_]) -> [File,A,B,C,D,E];
add_recent_1(File, Recent) -> [File|Recent].

recent_files(Tail) ->
    recent_files(wings_pref:get_value(recent_files, []), Tail).

recent_files([], Tail) -> Tail;
recent_files(Files, Tail) ->
    Help = ?__(1,"Open this recently used file"),
    recent_files_1(Files, 1, Help, [separator|Tail]).

recent_files_1([{Base0,Base1}|T], I, Help0, Tail) ->
    Base = wings_u:pretty_filename(Base0),
    Help = lists:flatten([Help0," -- "|wings_u:pretty_filename(Base1)]),
    [{Base,{recent_file,I},Help}|recent_files_1(T, I+1, Help0, Tail)];
recent_files_1([], _, _, Tail) -> Tail.

%%
%% The Revert command.
%%

revert(#st{file=undefined}=St) -> St;
revert(#st{file=File}=St0) ->
    St1 = wings_obj:create_folder_system(clean_st(St0)),
    case ?SLOW(wings_ff_wings:import(File, St1)) of
	#st{}=St2 ->
	    St = wings_obj:recreate_folder_system(St2),
	    clean_images(St);
	{error,_}=Error ->
	    Error
    end.

%%
%% Import.
%%

import_ndo() ->
    Ps = [{ext,".ndo"},{ext_desc,"Nendo File"}],
    Cont = fun(Name) -> {file,{import,{ndo,Name}}} end,
    import_filename(Ps, Cont).

import_ndo(Name, St0) ->
    case ?SLOW(wings_ff_ndo:import(Name, St0)) of
	#st{}=St ->
	    {save_state,St};
	{error,Reason} ->
	    wings_u:error_msg(?__(1,"Import failed: ") ++ Reason),
	    St0
    end.

import_image() ->
    Ps = [{extensions,wings_image:image_formats()},{multiple,true}],
    Cont = fun(Name) -> {file,{import_image,Name}} end,
    import_filename(Ps, Cont).

import_image(Name) ->
    case wings_image:from_file(Name) of
	Im when is_integer(Im) ->
	    keep;
	{error,100902=Error} ->  % GLU_OUT_OF_MEMORY
            wings_u:error_msg(?__(2,"The image cannot be loaded.~nFile: "
                                  "\"~ts\"~n GLU Error: ~p - ~s~n"),
                              [Name, Error, glu:errorString(Error)]);
        {error,Error} ->
            case file:format_error(Error) of
                "unknown" ++ _ ->
                    wings_u:error_msg(?__(1,"Failed to load") ++ " \"~ts\": ~p\n",
                                      [Name,Error]);
                ErrStr ->
                    wings_u:error_msg(?__(1,"Failed to load") ++ " \"~ts\": ~s\n",
                                      [Name,ErrStr])
            end
    end.

%%
%% Export.
%%

export_ndo(Cmd, Title, St) ->
    Ps = [{title,Title},{ext,".ndo"},{ext_desc,"Nendo File"}],
    Cont = fun(Name) -> {file,{Cmd,{ndo,Name}}} end,
    export_filename(Ps, St, Cont).

do_export_ndo(Name, St) ->
    case wings_ff_ndo:export(Name, St) of
	ok -> keep;
	{error,Reason} -> wings_u:error_msg(Reason)
    end.

%%%
%%% Install a plug-in.
%%%

install_plugin() ->
    Props = case os:type() of
		{win32,_} ->
		    [{title,"Install Plug-In"},
		     {extensions,
		      [{".gz", "GZip Compressed File"},
		       {".tar", "Tar File"},
		       {".tgz", "Compressed Tar File"},
		       {".beam", "Beam File"}]}];
		_Other    ->
		    [{title,?__(1,"Install Plug-In")},
		     {extensions,
		      [{".gz",?__(2,"GZip Compressed File")},
		       {".tar",?__(3,"Tar File")},
		       {".tgz",?__(4,"Compressed Tar File")},
		       {".beam",?__(5,"Beam File")}]}]
	    end,
    Cont = fun(Name) -> {file,{install_plugin,Name}} end,
    import_filename(Props, Cont).

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,
						 ?__(1,"Wings File")),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    lists:flatten([file_add_all(Exts),
		   file_filters_0(Exts++[{".*", ?__(2,"All Files")}])]).

file_filters_0(Exts) ->
    file_filters_1(lists:reverse(Exts),[]).

file_filters_1([{Ext,Desc}|T], Acc) ->
    Wildcard = "*" ++ Ext,
    ExtString = [Desc," (",Wildcard,")","|",Wildcard|Acc],
    case T of
	[] -> ExtString;
	_  ->
	    file_filters_1(T, ["|"|ExtString])
    end.

file_add_all([_]) -> [];
file_add_all(Exts) ->
    All0 = ["*"++E || {E,_} <- Exts],
    All = file_add_semicolons(All0),
    [?__(1,"All Formats")++" (",All,")", "|", All, "|"].

file_add_semicolons([E1|[_|_]=T]) ->
    [E1,";"|file_add_semicolons(T)];
file_add_semicolons(Other) -> Other.


%%% Utilities.
%%%

clean_st(St) ->
    foreach(fun(Win) ->
		    wings_wm:set_prop(Win, wireframed_objects, gb_sets:empty())
	    end, wings_u:geom_windows()),
    DefMat = wings_material:default(),
    Empty = gb_trees:empty(),
    Limit = wings_image:next_id(),
    wings_pref:delete_scene_value(),
    wings_view:delete_all(St#st{onext=1,shapes=Empty,mat=DefMat,pst=Empty,
				sel=[],ssels=Empty,saved=Limit}).

clean_images(#st{saved=Limit}=St) when is_integer(Limit) ->
    wings_dl:map(fun(D, _) -> D#dlo{proxy_data=none, proxy=false} end, []),
    wings_image:delete_older(Limit),
    St#st{saved=false}.

clean_new_images(#st{saved=Limit}) when is_integer(Limit) ->
    wings_image:delete_from(Limit).

delete_unselected(St) ->
    Unselected = wings_sel:unselected_ids(St),
    foldl(fun wings_obj:delete/2, St, Unselected).
