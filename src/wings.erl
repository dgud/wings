%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings.erl,v 1.341 2006/09/06 22:59:55 antoneos Exp $
%%

-module(wings).
-export([start/0,start_halt/0,start_halt/1]).
-export([redraw/1,redraw/2,init_opengl/1,command/2]).
-export([mode_restriction/1,clear_mode_restriction/0,get_mode_restriction/0]).
-export([ask/3]).
-export([init_menubar/0]).

-export([register_postdraw_hook/3,unregister_postdraw_hook/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,filter/2,foldl/3,sort/1,
		keymember/3,reverse/1]).

start() ->
    do_spawn(none).

start_halt() ->
    spawn_halt(none).

start_halt([File|_]) ->
    spawn_halt(File).

spawn_halt(File) ->
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  Wings = do_spawn(File, [link]),
		  halt_loop(Wings)
	  end).

halt_loop(Wings) ->
    receive
	{'EXIT',Wings,normal} ->
	    halt();
	{'EXIT',Wings,{window_crash,Name,Reason,StkTrace}} ->
	    Log = wings_u:crash_log(Name, Reason, StkTrace),
	    io:format("\n\n"),
	    io:format(?__(1,"Fatal internal error - log written to ~s\n"),
		      [Log]),
	    ok;
	{'EXIT',Wings,Reason} ->
	    Log = wings_u:crash_log(?__(2,"<Unknown Window Name>"),
				    Reason),
	    io:format("\n\n"),
	    io:format(?__(3,"Fatal internal error - log written to ~s\n"),
		      [Log]),
	    ok;
	_Other ->				%Can't happen.
	    halt_loop(Wings)
    end.

do_spawn(File) ->
    do_spawn(File, []).

do_spawn(File, Flags) ->
    %% Set a minimal heap size to avoiding garbage-collecting
    %% all the time. Don't set it too high to avoid keeping binaries
    %% too long.
    Fun = fun() -> init(File) end,
    spawn_opt(erlang, apply, [Fun,[]],
	      [{fullsweep_after,16384},{min_heap_size,32*1204}|Flags]).

init(File) ->
    register(wings, self()),
    
    OsType = os:type(),
    put(wings_os_type, OsType),
    wings_pref:init(),
    wings_hotkey:set_default(),
    wings_pref:load(),
    wings_lang:init(),
    
    group_leader(wings_console:start(), self()),
    wings_init:init(),
    wings_text:init(),
    wings_image:init(),
    wings_plugin:init(),
    wings_color:init(),
    wings_io:init(),

    wings_camera:init(),
    wings_vec:init(),
        
    Empty = gb_trees:empty(),
    St0 = #st{shapes=Empty,
	      selmode=face,
	      sel=[],
	      ssels=Empty,
	      mat=wings_material:default(),
	      saved=true,
	      onext=1,
	      repeatable=ignore,
	      ask_args=none,
	      drag_args=none,
	      def={ignore,ignore}
	     },
    St1 = wings_sel:reset(St0),
    St = wings_undo:init(St1),
    wings_view:init(),
    wings_file:init(),
    wings_u:caption(St),
    put(wings_hitbuf, sdl_util:alloc(?HIT_BUF_SIZE, ?GL_INT)),
    wings_wm:init(),
    wings_file:init_autosave(),
    init_menubar(),
    wings_pb:init(),
    wings_ask:init(),
    wings_job:init(),
    
    Op = main_loop_noredraw(St),		%Replace crash handler
						%with this handler.
    
    Props = initial_properties(),
    {{X,Y},{W,H}} = wings_wm:win_rect(desktop),
    wings_wm:toplevel(geom, geom_title(geom),
		      {X,Y,highest}, {W,H-80},
		      [resizable,{anchor,nw},
		       {toolbar,
			fun(A, B, C) -> wings_toolbar:create(A, B, C) end},
		       menubar,{properties,Props}],
		      Op),
    wings_wm:menubar(geom, get(wings_menu_template)),
    set_drag_filter(geom),

    open_file(File),
    restore_windows(St),
    case catch wings_wm:enter_event_loop() of
	{'EXIT',normal} ->
	    wings_pref:finish(),
	    erase(wings_hitbuf),
	    sdl:quit();
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,20]),
	    erase(wings_hitbuf),
	    sdl:quit(),
	    exit(Reason)
    end.

new_viewer(St) ->
    {Pos,{W,H}} = wings_wm:win_rect(desktop),
    Size = {W div 2-40,H div 2-40},
    N = free_viewer_num(2),
    Active = wings_wm:this(),
    Props = wings_wm:get_props(Active),
    ToolbarHidden = wings_wm:is_hidden({toolbar,Active}),
    Name = {geom,N},
    new_viewer(Name, Pos, Size, Props, ToolbarHidden, St).

new_viewer(Name, {X,Y}, Size, Props, ToolbarHidden, St) ->
    Op = main_loop_noredraw(St),
    Title = geom_title(Name),
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [resizable,closable,{anchor,nw},
		       {toolbar,
			fun(A, B, C) -> wings_toolbar:create(A, B, C) end},
		       menubar,
		       {properties,Props}],
		      Op),
    wings_wm:menubar(Name, get(wings_menu_template)),
    wings_wm:send({menubar,Name}, {current_state,St}),
    set_drag_filter(Name),
    if
	ToolbarHidden -> wings_wm:hide({toolbar,Name});
	true -> ok
    end,
    Name.

free_viewer_num(N) ->
    case wings_wm:is_window({geom,N}) of
	false -> N;
	true -> free_viewer_num(N+1)
    end.

open_file(none) -> ok;
open_file(Name) -> wings_wm:send(geom, {open_file,Name}).

init_opengl(St) ->
    wings_draw_util:init(),
    wings_render:init(),
    wings_dl:init(),
    wings_draw:refresh_dlists(St),
    keep.

redraw(St) ->
    redraw(info(St), St).

redraw(Info, St) ->
    wings_wm:clear_background(),
    wings_render:render(St),
    call_post_hook(St),
    case Info =/= [] andalso wings_wm:get_prop(show_info_text) of
	true -> wings_io:info(Info);
	false -> ok
    end.

call_post_hook(St) ->
    case wings_wm:lookup_prop(postdraw_hook) of
	none -> ok;
	{value,{_Id,Fun}} -> Fun(St)
    end.

register_postdraw_hook(Window, Id, Fun) ->
    case wings_wm:lookup_prop(Window, postdraw_hook) of
	none ->
	    wings_wm:set_prop(Window, postdraw_hook, {Id,Fun});
	{value,{Id,_}} ->
	    wings_wm:set_prop(Window, postdraw_hook, {Id,Fun});
	{value,{OtherId,_}} ->
	    erlang:fault({in_use_by,OtherId}, [Window,Id,Fun])
    end.

unregister_postdraw_hook(Window, Id) ->
    case wings_wm:lookup_prop(Window, postdraw_hook) of
	{value,{Id,_}} ->
	    wings_wm:erase_prop(Window, postdraw_hook);
	_ ->
	    ok
    end.

save_state(St0, St1) ->
    St2 = wings_undo:save(St0, St1),
    St = case St2 of
	     #st{saved=false} -> St2;
	     _Other -> wings_u:caption(St2#st{saved=false})
	 end,
    main_loop(clear_temp_sel(St)).

ask(Ask, St, Cb) ->
    wings_vec:do_ask(Ask, St, Cb).

main_loop(St) ->
    clear_mode_restriction(),
    wings_draw:refresh_dlists(St),
    wings_wm:current_state(St),
    wings_wm:dirty(),
    main_loop_noredraw(St).

main_loop_noredraw(St) ->
    {replace,fun(Event) -> handle_event(Event, St) end}.

handle_event({crash,Crash}, St) ->
    crash_logger(Crash, St);
handle_event({crash_in_other_window,LogName}, St) ->
    get_crash_event(LogName, St);
handle_event({open_file,Name}, St0) ->
    case catch ?SLOW(wings_ff_wings:import(Name, St0)) of
	#st{}=St ->
	    wings_pref:set_value(current_directory, filename:dirname(Name)),
	    main_loop(wings_u:caption(St#st{saved=true,file=Name}));
	{error,_} ->
	    main_loop(St0)
    end;
handle_event(Ev, St) ->
    case wings_camera:event(Ev, St) of
	next -> handle_event_0(Ev, St);
	Other -> Other
    end.

handle_event_0(#mousebutton{button=But,state=ButSt,mod=Mod}=Ev, St)
  when But < 3, Mod band ?CTRL_BITS =/= 0 ->
    case wings_pref:get_value(default_commands) of
	false ->
	    handle_event_1(Ev, St);
	true ->
	    if
		Mod band ?SHIFT_BITS =/= 0 ->
		    define_command(ButSt, But, St);
		true ->
		    use_command(Ev, St)
	    end
    end;
handle_event_0(Ev, St) -> handle_event_1(Ev, St).

handle_event_1(Ev, St) ->
    case wings_pick:event(Ev, St) of
	next -> handle_event_2(Ev, St);
	Other -> Other
    end.

handle_event_2(#mousebutton{x=X,y=Y}=Ev0, #st{sel=Sel}=St0) ->
    case wings_menu:is_popup_event(Ev0) of
	no ->
	    handle_event_3(Ev0, St0);
	{yes,Xglobal,Yglobal,_} ->
	    case Sel =:= [] andalso wings_pref:get_value(use_temp_sel) of
		false ->
		    popup_menu(Xglobal, Yglobal, St0);
		true ->
		    case wings_pick:do_pick(X, Y, St0) of
			{add,_,St} ->
			    Ev = wings_wm:local2global(Ev0),
			    wings_io:putback_event(Ev),
			    wings_wm:later({temporary_selection,St});
			_ ->
			    popup_menu(Xglobal, Yglobal, St0)
		    end
	    end;
	Other -> Other
    end;
handle_event_2(Ev, St) -> handle_event_3(Ev, St).
	    
handle_event_3(#keyboard{}=Ev, St0) ->
    case do_hotkey(Ev, St0) of
	next -> keep;
	{Cmd,St} -> do_command(Cmd, St)
    end;
handle_event_3({action,Callback}, _) when is_function(Callback) ->
    Callback();
handle_event_3({action,Cmd}, St) ->
    do_command(Cmd, St);
handle_event_3({vec_command,Command,St}, _) when is_function(Command) ->
    %% Use to execute command with vector arguments (see wings_vec.erl).
    command_response(Command(), none, St);
handle_event_3(#mousebutton{}, _St) -> keep;
handle_event_3(#mousemotion{}, _St) -> keep;
handle_event_3(init_opengl, St) ->
    wings_wm:current_state(St),
    init_opengl(St),
    wings_draw:refresh_dlists(St),
    keep;
handle_event_3(#expose{}, St) ->
    handle_event_3(redraw, St);
handle_event_3(resized, _) -> keep;
handle_event_3(close, _) ->
    Active = wings_wm:this(),
    wings_wm:delete({object,Active}),
    delete;
handle_event_3(redraw, St) ->
    redraw(St),
    main_loop_noredraw(St);
handle_event_3(quit, St) ->
    case wings_wm:this() of
	geom -> do_command({file,quit}, St);
	_ -> keep
    end;
handle_event_3({new_state,St}, St0) ->
    save_state(St0, St);
handle_event_3({temporary_selection,St}, St0) ->
    main_loop(set_temp_sel(St0, St));
handle_event_3({current_state,St}, _) ->
    main_loop_noredraw(St);
handle_event_3({current_state,_,_}, _) ->
    keep;
handle_event_3(revert_state, St) ->
    main_loop(clear_temp_sel(St));
handle_event_3(need_save, St) ->
    main_loop(wings_u:caption(St#st{saved=false}));
handle_event_3({new_default_command,DefCmd}, St) ->
    main_loop_noredraw(St#st{def=DefCmd});
handle_event_3(got_focus, _) ->
    Msg1 = wings_msg:button_format(?__(1,"Select")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(2,"Show menu")),
    Message = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Message),
    wings_wm:dirty();
handle_event_3(lost_focus, _) -> keep;
handle_event_3({note,menu_aborted}, St) ->
    main_loop(clear_temp_sel(St));
handle_event_3({note,_}, _) ->
    keep;
handle_event_3({drop,Pos,DropData}, St) ->
    handle_drop(DropData, Pos, St);
handle_event_3(language_changed, _) ->
    This = wings_wm:this(),
    wings_wm:toplevel_title(This, geom_title(This)),
    wings_wm:menubar(This, get(wings_menu_template)),
    keep;
handle_event_3({external,Op}, St) ->
    wpa:handle_external(Op,St),
    keep;
handle_event_3(ignore, _St) -> keep.

do_hotkey(Ev, #st{sel=[]}=St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false ->
	    do_hotkey_1(Ev, St0);
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} ->
		    case wings_hotkey:event(Ev, St) of
			next -> next;
			Cmd ->
			    case highlight_sel_style(Cmd) of
				none -> {Cmd,St0};
				temporary -> {Cmd,set_temp_sel(St0, St)};
				permanent -> {Cmd,St}
			    end
		    end;
		_Other -> do_hotkey_1(Ev, St0)
	    end
    end;
do_hotkey(Ev, St) -> do_hotkey_1(Ev, St).

do_hotkey_1(Ev, St) ->
    case wings_hotkey:event(Ev, St) of
 	next -> next;
	Cmd -> {Cmd,St}
    end.

highlight_sel_style({vertex,_}) -> temporary;
highlight_sel_style({edge,_}) -> temporary;
highlight_sel_style({face,_}) -> temporary;
highlight_sel_style({body,_}) -> temporary;
highlight_sel_style({edit,repeat}) -> temporary;
highlight_sel_style({edit,repeat_args}) -> temporary;
highlight_sel_style({edit,repeat_drag}) -> temporary;
highlight_sel_style({select,vertex}) -> none;
highlight_sel_style({select,edge}) -> none;
highlight_sel_style({select,face}) -> none;
highlight_sel_style({select,body}) -> none;
highlight_sel_style({select,{adjacent,_}}) -> none;
highlight_sel_style({select,_}) -> permanent;
highlight_sel_style({view,align_to_selection}) -> temporary;
highlight_sel_style({view,aim}) -> temporary;
highlight_sel_style(_) -> none.

do_command(Cmd, St0) ->
    St = remember_command(Cmd, St0),
    {replace,
     fun(Ev) -> handle_event(Ev, St) end,
     fun() -> raw_command(Cmd, none, St) end}.

raw_command(Cmd, Args, St) ->
    command_response(do_command_1(Cmd, St), Args, St).

command_response(#st{}=St, _, _) ->
    main_loop(clear_temp_sel(St));
command_response({drag,Drag}, Args, _) ->
    wings_drag:do_drag(Drag, Args);
command_response({save_state,#st{}=St}, _, St0) ->
    save_state(St0, St);
command_response({saved,St}, _, _) ->
    main_loop(St);
command_response({new,St}, _, _) ->
    main_loop(wings_u:caption(wings_undo:init(St)));
command_response({push,_}=Push, _, _) ->
    Push;
command_response({init,_,_}=Init, _, _) ->
    Init;
command_response({seq,_,_}=Seq, _, _) ->
    Seq;
command_response({replace,_}=Replace, _, _) ->
    Replace;
command_response({replace,_,_}=Replace, _, _) ->
    Replace;
command_response(keep, _, _) ->
    keep;
command_response(quit, _, _) ->
    save_windows(),
    exit(normal).

do_command_1(Cmd, St0) ->
    case wings_plugin:command(Cmd, St0) of
	next -> command(Cmd, St0);
	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end.

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
				     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,ask_args=none,drag_args=none};
remember_command(_Cmd, St) -> St.

%% Test if the saved command can be safely repeated, and
%% rewrite it with the current selection mode if needed.
repeatable(Mode, Cmd) ->
    case Cmd of
	{Mode,_} -> Cmd;			%Same mode is always OK.

	%% Commands safe in all modes.
	{_,{move,normal}} when Mode == body -> no;
	{_,{move,_}=C} -> {Mode,C};
	{_,{rotate,normal}} when Mode == body -> no;
	{_,{rotate,_}=C} -> {Mode,C};
	{_,{scale,_}=C} -> {Mode,C};

	%% Some special cases.
	{_,tighten=C} when Mode == vertex; Mode == body -> {Mode,C};
	{_,smooth=C} when Mode == face; Mode == body -> {Mode,C};
	
	%% No more commands are safe in body mode.
	{_,_} when Mode == body -> no;
	{_,{flatten,_}=C} when Mode == vertex; Mode == face -> {Mode,C};
	{_,dissolve} when Mode == vertex -> no;
	{_,dissolve=C} -> {Mode,C};
	{_,bevel=C} -> {Mode,C};
	{_,{extrude,_}=C} -> {Mode,C};
	{_,collapse=C} -> {Mode,C};

	%% Other special commands.
	{_,connect} when Mode == face -> no;
	{_,connect=C} -> {Mode,C};

	%% Other commands only work in the saved mode.
	_ -> no
    end.

%% Vector and secondary-selection commands.
command({shape,Shape}, St0) ->
    case wings_shapes:command(Shape, St0) of
    	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end;
command({help,What}, St) ->
    wings_help:command(What, St);

%% Drag & drop.
command({drop,What}, St) ->
    drop_command(What, St);

%% File menu.
command({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command({edit,undo_toggle}, St) ->
    wings_u:caption(wings_undo:undo_toggle(St));
command({edit,undo}, St) ->
    wings_u:caption(wings_undo:undo(St));
command({edit,redo}, St) ->
    wings_u:caption(wings_undo:redo(St));
command({edit,repeat}, #st{sel=[]}=St) -> St;
command({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd when is_tuple(Cmd) -> raw_command(Cmd, none, St)
    end;
command({edit,repeat}, St) -> St;
command({edit,repeat_args}, #st{sel=[]}=St) -> St;
command({edit,repeat_args}, #st{selmode=Mode,repeatable=Cmd0,
				ask_args=AskArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, none, St)
    end;
command({edit,repeat_args}, St) -> St;
command({edit,repeat_drag}, #st{sel=[]}=St) -> St;
command({edit,repeat_drag}, #st{selmode=Mode,repeatable=Cmd0,
				ask_args=AskArgs,drag_args=DragArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, DragArgs, St)
    end;
command({edit,repeat_drag}, St) -> St;
command({edit,purge_undo}, St) ->
    purge_undo(St);
command({edit,confirmed_purge_undo}, St) ->
    wings_undo:init(St);
command({edit,enable_patches}, St) ->
    wings_start:enable_patches(),
    St;
command({edit,disable_patches}, St) ->
    wings_start:disable_patches(),
    St;
command({edit,{preferences,Pref}}, St) ->
    wings_pref_dlg:command(Pref, St);

%% Select menu.
command({select,Command}, St) ->
    wings_sel_cmd:command(Command, St);

%% View menu.
command({view,Command}, St) ->
    wings_view:command(Command, St);

%% Window menu.
command({window,geom_viewer}, St) ->
    new_viewer(St),
    keep;
command({window,outliner}, St) ->
    wings_outliner:window(St);
command({window,object}, St) ->
    wings_shape:window(St);
command({window,palette}, St) ->
    wings_palette:window(St);
command({window,console}, _St) ->
    wings_console:window(),
    keep;

%% Body menu.
command({body,Cmd}, St) ->
    wings_body:command(Cmd, St);

%% Face menu.
command({face,Cmd}, St) ->
    wings_face_cmd:command(Cmd, St);

%% Edge commands.
command({edge,Cmd}, St) ->
    wings_edge_cmd:command(Cmd, St);

%% Vertex menu.
command({vertex,Cmd}, St) ->
    wings_vertex_cmd:command(Cmd, St);

%% Light menu.
command({light,Cmd}, St) ->
    wings_light:command(Cmd, St);

%% Material commands.
command({material,Cmd}, St) ->
    wings_material:command(Cmd, St);

%% Tools menu.

command({tools,set_default_axis}, St) ->
    wings:ask({[axis,point],[]}, St,
	      fun({Axis,Point}, _) ->
		      wings_pref:set_value(default_axis, {Point,Axis}),
		      keep
	      end);
command({tools,{align,Dir}}, St) ->
    {save_state,wings_align:align(Dir, St)};
command({tools,{center,Dir}}, St) ->
    {save_state,wings_align:center(Dir, St)};
command({tools,save_bb}, St) ->
    wings_align:copy_bb(St);
command({tools,{scale_to_bb,Dir}}, St) ->
    {save_state,wings_align:scale_to_bb(Dir, St)};
command({tools,{scale_to_bb_prop,Dir}}, St) ->
    {save_state,wings_align:scale_to_bb_prop(Dir, St)};
command({tools,{move_to_bb,Dir}}, St) ->
    {save_state,wings_align:move_to_bb(Dir, St)};
command({tools,{virtual_mirror,Cmd}}, St) ->
    wings_view:virtual_mirror(Cmd, St);
command({tools, screenshot}, St) ->
    wings_image:screenshot(),
    St;
command({tools, area_volume_info}, St) ->
    area_volume_info(St),
    St;
command({tools, put_on_ground}, St) ->
    {save_state,wings_align:put_on_ground(St)};
command({tools, unitize}, St) ->
    {save_state,wings_align:unitize(St)};

%% wings_job action events.
command({wings_job,Command}, St) ->
    wings_job:command(Command, St).


popup_menu(X, Y, #st{sel=[]}=St) ->
    wings_shapes:menu(X, Y, St);
popup_menu(X, Y, #st{selmode=Mode}=St) ->
    case wings_light:is_any_light_selected(St) of
	true -> wings_light:menu(X, Y, St);
	false ->
	    case Mode of
		vertex -> wings_vertex_cmd:menu(X, Y, St);
		edge -> wings_edge_cmd:menu(X, Y, St);
		face -> wings_face_cmd:menu(X, Y, St);
		body -> wings_body:menu(X, Y, St)
	    end
    end.

init_menubar() ->
    Menus = [{?__(1,"File"),file,fun(St) -> wings_file:menu(St) end},
          {?__(2,"Edit"),edit,fun edit_menu/1},
          {?__(3,"View"),view,fun(St) -> wings_view:menu(St) end},
          {?__(4,"Select"),select,fun(St) -> wings_sel_cmd:menu(St) end},
          {?__(5,"Tools"),tools,fun tools_menu/1},
          {?__(6,"Window"),window,fun window_menu/1},
          {?__(7,"Help"),help,fun(St) -> wings_help:menu(St) end}],
    put(wings_menu_template, Menus).

edit_menu(St) ->
    UndoInfo = lists:flatten([?__(1,
				   "Delete undo history to reclaim memory"),
			      " (",undo_info(St),")"]),
    [{?__(3,"Undo/Redo"),undo_toggle,
      ?__(4,"Undo or redo the last command")},
     {?__(5,"Redo"),redo,
      ?__(6,"Redo the last command that was undone")},
     {?__(7,"Undo"),undo,
      ?__(8,"Undo the last command")},
     separator,
     {command_name(?__(9,"Repeat"), St),repeat},
     {command_name(?__(10,"Repeat Args"), St),repeat_args},
     {command_name(?__(11,"Repeat Drag"), St),repeat_drag},
     separator,
     wings_pref_dlg:menu(St),
     {?__(12,"Plug-in Preferences"),{plugin_preferences,[]}},
     separator,
     {?__(13,"Purge Undo History"),purge_undo,UndoInfo}|patches()].

undo_info(St) ->
    {Un,Rn} = wings_undo:info(St),
    Undo = case Un of
	       0 -> ?__(1,"there are no undo states");
	       1 -> ?__(2,"there is one undo state");
	       _ -> io_lib:format(?__(3,"there are ~p undo states"), [Un])
	   end,
    case Rn of
	0 -> Undo;
	1 -> [Undo|?__(4,"; one operation can be redone")];
	_ -> [Undo|io_lib:format(?__(5,"; ~p operations can be redone"), [Rn])]
    end.

tools_menu(_) ->
    Dirs = [{wings_s:dir(all),all},
	    {wings_s:dir(x),x},
	    {wings_s:dir(y),y},
	    {wings_s:dir(z),z},
	    {wings_s:dir(radial_x),radial_x},
	    {wings_s:dir(radial_y),radial_y},
	    {wings_s:dir(radial_z),radial_z}],
    [{?__(8,"Align"),{align,Dirs}},
     {?__(9,"Center"),{center,Dirs}},
     separator,
     {?__(10,"Save Bounding Box"),save_bb},
     {?__(11,"Scale to Saved BB"),{scale_to_bb,Dirs}},
     {?__(12,"Scale to Saved BB Proportionally"),{scale_to_bb_prop,Dirs}},
     {?__(13,"Move to Saved BB"),{move_to_bb,wings_menu_util:all_xyz()}},
     separator,
     {?__(14,"Set Default Axis"),set_default_axis,
      ?__(15,"Define and store axis (with ref. point) for later use with any ")++
      ?__(16,"\"Default Axis\" command (e.g. Scale|Default Axis)")},
     separator,
     {?__(17,"Virtual Mirror"),
      {virtual_mirror,
       [{?__(18,"Create"),create,
	 ?__(19,"Given a face selection, set up a virtual mirror")},
	{?__(20,"Break"),break,
	 ?__(21,"Remove virtual mirrors for all objects")},
	{?__(22,"Freeze"),freeze,
	 ?__(23,"Create real geometry from the virtual mirrors")}]}},
     separator,
     {?__(24,"Screenshot"), screenshot,
      ?__(25,"Grab an image of the window (export it from the outliner)")},
     separator,
     {?__(26,"Scene Info: Area & Volume"), area_volume_info,
      ?__(27,"Calculate area and volume for each object in the scene")},
     {?__(28,"Put on Ground"), put_on_ground,
      ?__(29,"Put selected objects on the ground plane")},
     {?__(30,"Unitize"), unitize,
      ?__(31,"Scale selected objects to fit inside a unit sphere and move to origin")}].

window_menu(_) ->
    Name = case wings_wm:this() of
	       {_,geom} ->
		   ?__(1,"Geometry Graph");
	       {_,{geom,N}} ->
		   ?__(2,"Geometry Graph #") ++ integer_to_list(N)
	   end,
    [{?__(3,"Outliner"),outliner,
      ?__(4,"Open the outliner window (showing materials and objects)")},
     {Name,object,
      ?__(5,"Open a Geometry Graph window (showing objects)")},
     {?__(6,"Palette"), palette,?__(7,"Open the color palette window")},
     separator,
     {?__(8,"New Geometry Window"),geom_viewer, ?__(9,"Open a new Geometry window")},
     {?__(10,"Console"),console,?__(11,"Open a console window for information messages")}].

patches() ->
    case wings_start:get_patches() of
	none -> [];
	{enabled,Desc} ->
	    [separator,{?__(1,"Use ")++Desc,disable_patches,[crossmark]}];
	{disabled,Desc} ->
	    [separator,{?__(1,"Use ")++Desc,enable_patches}]
    end.

set_temp_sel(#st{sh=Sh,selmode=Mode}, St) ->
    St#st{temp_sel={Mode,Sh}}.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.


purge_undo(St) ->
    This = wings_wm:this(),
    {Un,Rn} = wings_undo:info(St),
    Qs = {vframe,
         [{label,?__(1,"Undo states: ") ++ integer_to_list(Un)},
          {label,?__(2,"Redo states: ")  ++ integer_to_list(Rn)},
	   separator|
	   if
	       Un+Rn =:= 0 ->
		   [{label,?__(3,"Nothing to remove")},
		    {hframe,[{button,ok}]}];
	       true ->
		   [{label,?__(4,"Remove all states (NOT undoable)?")},
		    {hframe,[{button,wings_s:yes(),
			      fun(_) ->
				      Action = {action,{edit,confirmed_purge_undo}},
				      wings_wm:send(This, Action)
			      end},
			     {button,wings_s:no(),cancel,[cancel]}]}]
	   end]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

info(#st{sel=[]}) -> [];
info(St) ->
    case wings_wm:get_prop(show_info_text) of
	false -> [];
	true -> info_1(St)
    end.
	    
info_1(#st{shapes=Shapes,selmode=body,sel=[{Id,_}]}) ->
    Sh = gb_trees:get(Id, Shapes),
    shape_info(Sh);
info_1(#st{shapes=Shapes,selmode=body,sel=Sel}) ->
    shape_info(Sel, Shapes);
info_1(#st{selmode=vertex,sel=[{_Id,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [V] = gb_sets:to_list(Sel),
	    measure(io_lib:format(?__(1,"Vertex ~p selected"), [V]), St);
	N when N < 5 ->
	    Vs = gb_sets:to_list(Sel),
	    measure(item_list(Vs, ?__(2,"Vertices")), St);
	N ->
	    io_lib:format(?__(3,"~p vertices selected"), [N])
    end;
info_1(#st{selmode=edge,sel=[{_,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Edge] = gb_sets:to_list(Sel),
	    measure(io_lib:format(?__(4,"Edge ~p selected"), [Edge]), St);
	N when N < 5 ->
	    Edges = gb_sets:to_list(Sel),
	    measure(item_list(Edges, ?__(5,"Edges")), St);
	N ->
	    measure(io_lib:format(?__(6,"~p edges selected"), [N]), St)
    end;
info_1(#st{selmode=face,sel=[{_,Sel}]}=St) ->
    case gb_sets:size(Sel) of
	0 -> "";
	1 ->
	    [Face] = gb_sets:to_list(Sel),
	    measure(io_lib:format(?__(7,"Face ~p selected"), [Face]), St);
	2 ->
	    Faces = gb_sets:to_list(Sel),
	    measure(item_list(Faces,?__(8,"Faces")), St);
	N when N < 5 ->
	    Faces = gb_sets:to_list(Sel),
	    item_list(Faces,?__(8,"Faces"));
	N ->
	    io_lib:format(?__(9,"~p faces selected"), [N])
    end;
info_1(#st{selmode=Mode,sel=Sel}=St) ->
    On = length(Sel),
    N = foldl(fun({_,S}, A) -> A+gb_sets:size(S) end, 0, Sel),
    Str = case Mode of
	      vertex ->
		  io_lib:format(?__(10,"~p vertices selected in ~p objects"), [N,On]);
	      edge ->
		  io_lib:format(?__(11,"~p edges selected in ~p objects"), [N,On]);
	      face ->
		  io_lib:format(?__(12,"~p faces selected in ~p objects"), [N,On])
	  end,
    measure(Str, St).

measure(Base, #st{selmode=vertex,sel=[{Id,Vs}],shapes=Shs}) ->
    case gb_sets:size(Vs) of
	1 ->
	    We = gb_trees:get(Id, Shs),
 	    [Va] = gb_sets:to_list(Vs),
	    {X,Y,Z} = wings_vertex:pos(Va, We),
	    [Base|io_lib:format(?__(1,". Position ~s ~s ~s"),
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z)])];
	2 ->
	    We = gb_trees:get(Id, Shs),
 	    [Va,Vb] = gb_sets:to_list(Vs),
            {Xa,Ya,Za} = wings_vertex:pos(Va, We),
            {Xb,Yb,Zb} = wings_vertex:pos(Vb, We),
 	    Dist = e3d_vec:dist(wings_vertex:pos(Va, We),
				wings_vertex:pos(Vb, We)),
	    [Base|io_lib:format(?__(5,". Distance ~s  <~s, ~s, ~s>"),
				[wings_util:nice_float(Dist),
				 wings_util:nice_float(Xb - Xa),
				 wings_util:nice_float(Yb - Ya),
				 wings_util:nice_float(Zb - Za)])];
	_ -> Base
    end;
measure(Base, #st{selmode=vertex,sel=[{IdA,VsA},{IdB,VsB}],shapes=Shs}) ->
    case gb_sets:size(VsA) == 1 andalso gb_sets:size(VsB) == 1 of
	true ->
	    WeA = gb_trees:get(IdA, Shs),
	    WeB = gb_trees:get(IdB, Shs),
 	    [Va] = gb_sets:to_list(VsA),
 	    [Vb] = gb_sets:to_list(VsB),
 	    Dist = e3d_vec:dist(wings_vertex:pos(Va, WeA),
				wings_vertex:pos(Vb, WeB)),
	    [Base|io_lib:format(?__(2,". Distance ~s"),
				[wings_util:nice_float(Dist)])];
	_ -> Base
    end;
measure(Base, #st{selmode=edge,sel=[{Id,Es}],shapes=Shs}) ->
    case gb_sets:size(Es) of
        1 ->
            We = gb_trees:get(Id, Shs),
            [Edge] = gb_sets:to_list(Es),
            #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, We#we.es),
            PosA = wings_vertex:pos(Va, We),
            PosB = wings_vertex:pos(Vb, We),
            Length = e3d_vec:dist(PosA, PosB),
            {X,Y,Z} = e3d_vec:average([PosA,PosB]),
            [Base|io_lib:format(?__(3,". Midpt ~s ~s ~s. Length ~s"),
                                [wings_util:nice_float(X),
                                 wings_util:nice_float(Y),
                                 wings_util:nice_float(Z),
                                 wings_util:nice_float(Length)])];
        2 ->
            We = gb_trees:get(Id, Shs),
            Edges = gb_sets:to_list(Es),
            [E0,E1] = Edges,
            #edge{vs=V0s,ve=V0e} = gb_trees:get(E0, We#we.es),
            #edge{vs=V1s,ve=V1e} = gb_trees:get(E1, We#we.es),
            {X0s,Y0s,Z0s} = wings_vertex:pos(V0s, We),
            {X0e,Y0e,Z0e} = wings_vertex:pos(V0e, We),
            {X1s,Y1s,Z1s} = wings_vertex:pos(V1s, We),
            {X1e,Y1e,Z1e} = wings_vertex:pos(V1e, We),
            V0 = {X0e-X0s, Y0e-Y0s, Z0e-Z0s},
            V1 = {X1e-X1s, Y1e-Y1s, Z1e-Z1s},

            RawAngle = e3d_vec:degrees(V0, V1),
            Angle = case {V0s,V0e} of
                      {V1s,_} -> RawAngle;
                      {_,V1e} -> RawAngle;
                      {V1e,_} -> 180.0 - RawAngle;
                      {_,V1s} -> 180.0 - RawAngle;
                      {_,_}   -> RawAngle   %%% unconnected
                    end,

            [Base|io_lib:format(?__(6,". Angle ~s"),
                                [wings_util:nice_float(Angle)])];
        _ -> 
            Base
    end;
measure(Base, #st{selmode=face,sel=[{Id,Fs}],shapes=Shs}) ->
    case gb_sets:size(Fs) of
	1 ->
	    We = gb_trees:get(Id, Shs),
 	    [Face] = gb_sets:to_list(Fs),
	    {X,Y,Z} = wings_face:center(Face, We),
	    Mat = wings_facemat:face(Face, We),
	    [Base|wings_util:format(?__(4,". Midpt ~s ~s ~s.\nMaterial ~s"),
				[wings_util:nice_float(X),
				 wings_util:nice_float(Y),
				 wings_util:nice_float(Z),
				 Mat])];
        2 ->
	    We = gb_trees:get(Id, Shs),
 	    [F0,F1] = gb_sets:to_list(Fs),
	    N0 = wings_face:normal(F0, We),
	    N1 = wings_face:normal(F1, We),
	    Angle = e3d_vec:degrees(N0,N1),
	    [Base|wings_util:format(?__(6,". Angle ~s"),
				    [wings_util:nice_float(Angle)])];
	_ -> Base
    end;
measure(Base, _) -> Base.

item_list(Items, Desc) ->
    item_list(Items, " ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", [Desc,Sep|integer_to_list(Item)]);
item_list([], _Sep, Desc) -> [Desc|?__(1," selected")].

shape_info(We) when ?IS_LIGHT(We) ->
    wings_light:info(We);
shape_info(#we{id=Id,name=Name,fs=Ftab,es=Etab,vp=Vtab,mode=Mode}) ->
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    wings_util:format(?__(object_info,
			  "Object ~p \"~s\" has ~p polygons, "
			  "~p edges, ~p vertices.\n"
			  "Mode is ~s"),
		      [Id,Name,Faces,Edges,Vertices,object_mode(Mode)]).

object_mode(vertex) -> ?__(vertex,"vertex color");
object_mode(material) -> ?__(material,"material").

shape_info(Objs, Shs) ->
    shape_info(Objs, Shs, 0, 0, 0, 0).

shape_info([{Id,_}|Objs], Shs, On, Vn, En, Fn) ->
    #we{fs=Ftab,es=Etab,vp=Vtab} = gb_trees:get(Id, Shs),
    Faces = gb_trees:size(Ftab),
    Edges = gb_trees:size(Etab),
    Vertices = gb_trees:size(Vtab),
    shape_info(Objs, Shs, On+1, Vn+Vertices, En+Edges, Fn+Faces);
shape_info([], _Shs, N, Vertices, Edges, Faces) ->
    io_lib:format(?__(2,
		      "~p objects, ~p faces, ~p edges, ~p vertices"),
		  [N,Faces,Edges,Vertices]).

command_name(_Repeat, #st{repeatable=ignore}) ->
    "("++cannot_repeat()++")";
command_name(Repeat, #st{repeatable={_,Cmd}}=St) ->
    CmdStr = wings_util:stringify(Cmd),
    command_name(Repeat, CmdStr, St).

command_name(_Repeat, CmdStr, #st{sel=[]}) ->
    cannot_repeat(CmdStr);
command_name(Repeat, CmdStr, #st{selmode=Mode,repeatable=Cmd}) ->
    case repeatable(Mode, Cmd) of
	no -> cannot_repeat(CmdStr);
	_ ->  lists:flatten([Repeat," ",wings_util:quote(CmdStr)])
    end.

cannot_repeat(Cmd) ->
    lists:flatten(["(",cannot_repeat(),wings_util:quote(Cmd),")"]).

cannot_repeat() -> ?__(1,"Can't repeat").

replace_ask(Term, none) -> Term;
replace_ask({'ASK',_}, AskArgs) -> AskArgs;
replace_ask(Tuple0, AskArgs) when is_tuple(Tuple0) ->
    Tuple = [replace_ask(El, AskArgs) || El <- tuple_to_list(Tuple0)],
    list_to_tuple(Tuple);
replace_ask(Term, _) -> Term.

define_command(?SDL_RELEASED, N, #st{repeatable=Cmd,def=DefCmd0}) ->
    This = wings_wm:this(),
    CmdStr = wings_util:stringify(Cmd),
    Button = case N of
		 1 -> wings_s:lmb();
		 2 -> wings_s:mmb()
	     end,
    Q = lists:flatten([?__(1,"Do you want to define"),
		       " ",wings_util:quote(CmdStr), " ",
		       ?__(2,"as a default command"),
		       " (",wings_s:key(ctrl),"+",Button,")?"]),
    wings_u:yes_no(Q,
		   fun() ->
			   DefCmd = setelement(N, DefCmd0, Cmd),
			   wings_wm:send(This, {new_default_command,DefCmd}),
			   ignore
		   end, ignore);
define_command(_, _, _) -> keep.

use_command(#mousebutton{state=?SDL_RELEASED,button=N}=Ev,
	    #st{selmode=Mode,def=DefCmd}=St) ->
    case repeatable(Mode, element(N, DefCmd)) of
	no -> keep;
	Cmd when is_tuple(Cmd) ->
	    do_use_command(Ev, Cmd, St)
    end;
use_command(_, _) -> keep.

geom_title(geom) ->
    ?__(1,"Geometry");
geom_title({geom,N}) ->
    ?__(2,"Geometry #") ++ integer_to_list(N).

do_use_command(#mousebutton{x=X,y=Y}, Cmd0, #st{sel=[]}=St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false ->
	    keep;
	true ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,#st{selmode=Mode}=St} ->
		    %% The selection mode may have changed.
		    %% Must check (and possibly convert) the command again.
		    case repeatable(Mode, Cmd0) of
			no -> keep;
			Cmd ->
			    wings_wm:later({action,Cmd}),
			    main_loop_noredraw(set_temp_sel(St0, St))
		    end;
		_Other -> keep
	    end
    end;
do_use_command(_, Cmd, _) -> wings_wm:later({action,Cmd}).

crash_logger(Crash, St) ->
    LogName = wings_u:crash_log(geom, Crash),
    get_crash_event(LogName, St).

get_crash_event(Log, St) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> crash_handler(Ev, Log, St) end}.

crash_handler(redraw, Log, _St) ->
    wings_wm:clear_background(),
    wings_io:ortho_setup(),
    wings_io:text_at(10, 2*?LINE_HEIGHT,
		     ?__(1,
			  "Internal error - log written to") ++
		     " " ++ Log),
    wings_io:text_at(10, 4*?LINE_HEIGHT,
		     ?__(2,
			  "Click a mouse button to continue working")),
    wings_msg:button(?__(3,"Continue working")),
    keep;
crash_handler(#mousebutton{}, _, St) ->
    wings_wm:message(""),
    wings_wm:menubar(wings_wm:this(), get(wings_menu_template)),
    main_loop(St);
crash_handler(_, Log, St) ->
    get_crash_event(Log, St).

%%%
%%% Drag & Drop.
%%%

set_drag_filter(Name) ->
    F = fun({material,_}) -> yes;
	   (_) -> no
	end,
    wings_wm:set_prop(Name, drag_filter, F).

handle_drop(DropData, {X0,Y0}, St) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    handle_drop_1(DropData, X, Y, St).

handle_drop_1(_, X, Y, #st{sel=[]}) ->
    wings_menu:popup_menu(X, Y, drop,
			  [{?__(1,"No Selection"),cancel_drop, ?__(2,"Cancel drop operation")}]);
handle_drop_1({material,Name}, X, Y, #st{selmode=face}) ->
    Menu = [{ ?__(3,"Assign material to selected faces"),menu_cmd(assign_to_sel, Name),
	      ?__(4,"Assign material \"")++Name++ ?__(5,"\" only to selected faces")},
	    { ?__(6,"Assign material to all faces"),
	     menu_cmd(assign_to_body, Name),
	      ?__(7,"Assign material \"")++Name++
              ?__(8,"\" to all faces in objects having a selection")}],
    wings_menu:popup_menu(X, Y, drop, Menu);
handle_drop_1({material,Name}, X, Y, _) ->
    Menu = [{ ?__(9,"Assign material to all faces"),
	      menu_cmd(assign_to_body, Name),
	      ?__(10,"Assign material \"")++Name++
	      ?__(11,"\" to all faces in objects having a selection")}],
    wings_menu:popup_menu(X, Y, drop, Menu).
    
menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

drop_command({assign_to_sel,Name}, St) ->
    wings_material:command({assign,Name}, St);
drop_command({assign_to_body,Name}, #st{selmode=Mode}=St0) ->
    St = wings_material:command({assign,Name}, St0#st{selmode=body}),
    St#st{selmode=Mode};
drop_command(cancel_drop, St) -> St.

%%%
%%% Saving and restoring of window layouts.
%%%

save_windows() ->
    Saved = save_windows_1(wings_wm:windows()),
    wings_pref:set_value(saved_windows, Saved).

save_windows_1([console|Ns]) ->
    save_window(console, Ns);
save_windows_1([palette|Ns]) ->
    save_window(palette, Ns);
save_windows_1([outliner|Ns]) ->
    save_window(outliner, Ns);
save_windows_1([{object,_}=N|Ns]) ->
    save_window(N, Ns);
save_windows_1([geom=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([{geom,_}=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([_|T]) -> save_windows_1(T);
save_windows_1([]) -> [].

save_window(Name, Ns) ->
    Pos = wings_wm:win_ur({controller,Name}),
    Size = wings_wm:win_size(Name),
    W = {Name,Pos,Size},
    [W|save_windows_1(Ns)].

save_geom_window(Name, Ns) ->
    {Pos,Size} = wings_wm:win_rect(Name),
    Ps0 = [{toolbar_hidden,wings_wm:is_hidden({toolbar,Name})}],
    Ps = save_geom_props(wings_wm:get_props(Name), Ps0),
    Geom = {Name,Pos,Size,Ps},
    [Geom|save_windows_1(Ns)].

save_geom_props([{show_axes,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([{show_groundplane,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([{current_view,View}|T], Acc) ->
    #view{fov=Fov,hither=Hither,yon=Yon} = View,
    save_geom_props(T, [{fov,Fov},{clipping_planes,Hither,Yon}|Acc]);
save_geom_props([{show_info_text,_}=P|T], Acc) ->
    save_geom_props(T, [P|Acc]);
save_geom_props([_|T], Acc) ->
    save_geom_props(T, Acc);
save_geom_props([], Acc) -> Acc.

restore_windows(St) ->
    %% Sort windows using names as keys to make sure we
    %% create the geometry windows before the object windows.
    %% (Because we set up links.)
    Windows0 = wings_pref:get_value(saved_windows, []),
    Windows1 = sort([{element(1, W),W} || W <- Windows0]),
    Windows = [W || {_,W} <- Windows1],
    restore_windows_1(Windows, St).

restore_windows_1([{geom,{_,_}=Pos0,{_,_}=Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    case proplists:get_bool(toolbar_hidden, Ps) of
	true -> wings_wm:hide({toolbar,geom});
	false -> ok
    end,
    Pos = geom_pos(Pos0),
    wings_wm:move(geom, Pos, Size),
    set_geom_props(Ps, geom),
    restore_windows_1(Ws, St);
restore_windows_1([{{geom,_}=Name,Pos0,Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    ToolbarHidden = proplists:get_bool(toolbar_hidden, Ps),
    new_viewer(Name, {0,0}, Size, initial_properties(), ToolbarHidden, St),
    Pos = geom_pos(Pos0),
    wings_wm:move(Name, Pos, Size),
    set_geom_props(Ps, Name),
    restore_windows_1(Ws, St);
restore_windows_1([{{object,_}=Name,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_shape:window(Name, Pos, Size, St),
    restore_windows_1(Ws, St);
restore_windows_1([{outliner,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_outliner:window(Pos, Size, St),
    restore_windows_1(Ws, St);
restore_windows_1([{console,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_console:window(console, Pos, Size),
    restore_windows_1(Ws, St);
restore_windows_1([{palette,{_,_}=Pos,{_,_}=Size}|Ws], St) ->
    wings_palette:window(Pos,Size,St),
    restore_windows_1(Ws, St);
restore_windows_1([_|Ws], St) ->
    restore_windows_1(Ws, St);
restore_windows_1([], _) -> ok.

geom_pos({X,Y}=Pos) ->
    {_,Upper0} = wings_wm:win_ul(desktop),
    Upper1 = case wings_wm:is_hidden({toolbar,geom}) of
		 true -> Upper0;
		 false ->
		     {_,ToolbarH} = wings_wm:win_size({toolbar,geom}),
		     Upper0+ToolbarH
	     end,
    {_,TitleH} = wings_wm:win_size({controller,geom}),
    case Upper1 + TitleH of
	Upper when Y < Upper -> {X,Upper};
	_ -> Pos
    end.

geom_props(B) when B == false; B == true ->
    [{toolbar_hidden,B}];
geom_props(L) when is_list(L) -> L;
geom_props(_) -> [].

set_geom_props([{show_axes,B}|T], Name) ->
    wings_wm:set_prop(Name, show_axes, B),
    set_geom_props(T, Name);
set_geom_props([{show_groundplane,B}|T], Name) ->
    wings_wm:set_prop(Name, show_groundplane, B),
    set_geom_props(T, Name);
set_geom_props([{show_info_text,B}|T], Name) ->
    wings_wm:set_prop(Name, show_info_text, B),
    set_geom_props(T, Name);
set_geom_props([{fov,Fov}|T], Name) ->
    View = wings_wm:get_prop(Name, current_view),
    wings_wm:set_prop(Name, current_view, View#view{fov=Fov}),
    set_geom_props(T, Name);
set_geom_props([{clipping_planes,Hither,Yon}|T], Name)
  when Hither > 0, Hither < Yon  ->
    View = wings_wm:get_prop(Name, current_view),
    wings_wm:set_prop(Name, current_view, View#view{hither=Hither,yon=Yon}),
    set_geom_props(T, Name);
set_geom_props([_|T], Name) ->
    set_geom_props(T, Name);
set_geom_props([], _) -> ok.

initial_properties() ->
    [{display_lists,geom_display_lists}|wings_view:initial_properties()].

mode_restriction(Modes) ->
    Win = {toolbar,wings_wm:this()},
    wings_wm:send(Win, {mode_restriction,Modes}),
    case Modes of
	none ->
	    wings_wm:erase_prop(Win, mode_restriction);
	_ ->
	    wings_wm:set_prop(Win, mode_restriction, Modes)
    end.

clear_mode_restriction() ->
    mode_restriction(none).

get_mode_restriction() ->
    Name = wings_wm:this(),
    Toolbar = {toolbar,Name},
    case wings_wm:lookup_prop(Toolbar, mode_restriction) of
	none -> [edge,vertex,face,body];
	{value,Other} -> Other
    end.

area_volume_info(St) ->
    #st{shapes=Shapes} = St,
    Header = io_lib:fwrite("~s: '~s' [~s] (~s)\n", ["#", "Object Name", "Area", "Volume"]),
    Info = [get_object_info(Id, Shapes) || Id <- gb_trees:keys(Shapes)],
    Msg = lists:flatten(Header ++ Info),
    wings_help:help_window("Scene Info: Area & Volume", [Msg]).

get_object_info(Id, Shapes) ->
    We0 = gb_trees:get(Id, Shapes),
    We = wings_tesselation:triangulate(We0),
    #we{id=Id,name=Name,fs=Ftab} = We,
    Both = [area_volume(Face, We) || Face <- gb_trees:keys(Ftab)],
    Area =  lists:sum([A || {A,_} <- Both]),
    Volume =lists:sum([V || {_,V} <- Both]),
    io_lib:fwrite("~p: '~s' [~f] (~f)\n", [Id,Name,Area,Volume]).

area_volume(Face, We) ->
    [V1,V2,V3] = wings_face:vertex_positions(Face, We),
    E1 = e3d_vec:sub(V1, V2),
    E2 = e3d_vec:sub(V3, V2),
    Cp = e3d_vec:cross(E1, E2),
    Bc = e3d_vec:cross(V2, V3),
    Area = e3d_vec:len(Cp)/2.0,
    Volume = e3d_vec:dot(V1, Bc)/6.0,
    {Area, Volume}.

