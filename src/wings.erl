%%
%%  wings.erl --
%%
%%     The main module of Wings 3D.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings).
-export([start/0,start_halt/0,start_halt/1]).
-export([redraw/1,redraw/2,init_opengl/1,command/2]).
-export([mode_restriction/1,clear_mode_restriction/0,get_mode_restriction/0]).
-export([ask/3]).
-export([save_windows/0,save_windows_1/1,restore_windows_1/2,set_geom_props/2]).
-export([handle_drop/3, popup_menu/3]).
-export([highlight_aim_setup/1]).
-export([register_postdraw_hook/3,unregister_postdraw_hook/2]).
-export([info_line/0, command_name/2]).
-export([edit_menu/0, tools_menu/0, window_menu/0]).

-export([new_st/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1]).

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
    %% Handle normal and abnormal termination of the Wings process.
    %% None of the strings are translated because when this code
    %% executes, the ETS table that holds the translated strings
    %% has been deleted.
    receive
	{'EXIT',Wings,normal} ->
	    %% Normal termination.
	    init:stop();
	{'EXIT',Wings,{window_crash,Name,Reason,StkTrace}} ->
	    %% Crash in a window with an error reason and stack trace.
	    Log = wings_u:crash_log(Name, Reason, StkTrace),
	    io:format("\n\n"),
	    %% Intentionally not translated.
	    io:format("Fatal internal error - log written to ~s\n",
		      [Log]),
	    ok;
	{'EXIT',Wings,Reason} ->
	    %% Some other crash.
	    Log = wings_u:crash_log("<Unknown Window Name>", Reason, []),
	    io:format("\n\n"),
	    %% Intentionally not translated.
	    io:format("Fatal internal error - log written to ~s\n",
		      [Log]),
	    ok;
	_Other ->
	    %% We are not supposed to receive any other messages,
	    %% but it is good practice to always throw away
	    %% unexpected messages.
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
    erlang:system_flag(backtrace_depth, 25),
    wings_pref:init(),
    wings_hotkey:set_default(),
    wings_pref:load(),
    wings_lang:init(),
    wings_plugin:init(),
    wings_sel_cmd:init(),
    wings_file:init(),

    St0 = new_st(),
    St1 = wings_sel:reset(St0),
    St2 = wings_undo:init(St1),
    St = wings_shape:create_folder_system(St2),

    group_leader(wings_console:start(), self()),

    Frame = wings_frame:start(),
    GeomGL = wings_gl:init(Frame),
    %% Needs to be initialized before make_geom_window
    %% and before the others that use gl functions
    wings_text:init(),
    wings_wm:init(Frame),
    check_requirements(),

    wings_image:init(wings_io:get_process_option()),
    wings_color:init(),
    wings_io:init(),

    wings_camera:init(),
    wings_vec:init(),

    wings_view:init(),
    wings_u:caption(St),
    wings_file:init_autosave(),
    wings_pb:start_link(Frame),
    wings_dialog:init(),
    wings_job:init(),
    wings_develop:init(),
    wings_tweak:init(),

    open_file(File),
    make_geom_window(GeomGL, St),
    restore_windows(St),
    case catch wings_wm:enter_event_loop() of
	{'EXIT',normal} ->
	    wings_pref:finish(),
	    wings_io:quit();
	{'EXIT',Reason} ->
	    io:format("~P\n", [Reason,20]),
	    wings_io:quit(),
	    exit(Reason)
    end.

make_geom_window(GeomGL, St) ->
    Op = main_loop_noredraw(St),	%Replace crash handler
    Props = initial_properties(),        %with this handler.
    wings_wm:new(geom, GeomGL, Op),
    [wings_wm:set_prop(geom, K, V)|| {K,V} <- Props],
    wings_wm:set_dd(geom, geom_display_lists),
    set_drag_filter(geom),
    wings_frame:register_win(GeomGL, geom, [top, {title, geom_title(geom)}]),
    GeomGL.

%% Check minimum system requirements.
check_requirements() ->
    minor_gl_version(),
    have_fbo().

minor_gl_version() ->
    Major = 2,
    Minor = 1,
    Req = {Major,Minor,0},
    case ets:lookup(wings_gl_ext, version) of
    [{_,{MajorCurrent,MinorCurrent,_}=VerTuple}] when VerTuple < Req ->
        fatal("Wings3D requires OpenGL ~p.~p or higher.\nYour available version ~p.~p",
          [Major,Minor,   MajorCurrent,MinorCurrent]);
    [{_,{_,_,_}=_VerTuple}]  ->  % assert that a 3 tuple is returned as expected
        ok;
    Unexpected ->
        fatal("Unexpected current OpenGL info : ~p", [ Unexpected ])
    end.

have_fbo() ->
    case wings_gl:have_fbo() of
	true ->
	    ok;
	false ->
	    fatal("Wings3D requires the OpenGL frame buffer "
		  "object extension.")
    end.

-spec fatal(any(),any()) -> no_return().
fatal(Format, Args) ->
    fatal(io_lib:format(Format, Args)).

fatal(Str) ->
    Parent = ?GET(top_frame),
    Dialog = wxMessageDialog:new(Parent, Str, [{caption,"Fatal Error"}]),
    wxMessageDialog:showModal(Dialog),
    wings_io:quit(),
    exit(normal).

new_st() ->
    Empty = gb_trees:empty(),
    #st{shapes=Empty,
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
       }.

new_viewer(St) ->
    {W,H} = wings_wm:top_size(),
    Size = {W div 2-40,H div 2-40},
    N = free_viewer_num(2),
    Props = [{display_data,geom_display_lists}|wings_wm:get_props(geom)],
    Name = {geom,N},
    new_viewer(Name, {50,50}, Size, Props, St).

new_viewer(Name, Pos, Size, Props, St) ->
    Op = main_loop_noredraw(St),
    Title = geom_title(Name),
    Frame = wings_frame:make_win(Title, [{size, Size}, {pos, Pos}]),
    Context = wxGLCanvas:getContext(?GET(gl_canvas)),
    Canvas = wings_gl:window(Frame, Context, true),
    wings_wm:toplevel(Name, Canvas, Props, Op),
    set_drag_filter(Name),
    Name.

free_viewer_num(N) ->
    case wings_wm:is_window({geom,N}) of
	false -> N;
	true -> free_viewer_num(N+1)
    end.

open_file(File0) ->
    USFile = wings_file:autosave_filename(wings_file:unsaved_filename()),
    Recovered = filelib:is_file(USFile),
    wings_pref:set_value(file_recovered, Recovered),
    %% On the Mac, if Wings was started by clicking on a .wings file,
    Msgs0 = wxe_master:fetch_msgs(),
    Msgs = [F || F <- Msgs0, filelib:is_regular(F)],
    File = case Msgs of
	       [F|_] -> F;
	       [] -> File0
	   end,
    if Recovered ->
	    wings_u:message(?__(1,"Wings3D has recovered an unsaved file.")),
	    wings_wm:send_after_redraw(geom, {open_file,USFile});
       File =:= none ->
	    timer:sleep(200), %% For splash screen :-)
	    ignore;
       true ->
	    timer:sleep(200), %% For splash screen :-)
	    wings_wm:send_after_redraw(geom, {open_file,USFile})
    end.

init_opengl(St) ->
    wings_render:init(),
    wings_dl:init(),
    wings_draw:refresh_dlists(St),
    keep.

redraw(St) ->
    redraw(info(St), St).

redraw(Info, St) ->
    Render =
	fun() ->
		wings_wm:clear_background(),
		wings_render:render(St),
		call_post_hook(St),
		TweakInfo = wings_tweak:statusbar(),
		case Info =/= [] andalso wings_wm:get_prop(show_info_text) of
		    true when TweakInfo =:= [] ->
		        wings_io:info(Info);
		    true ->
		        wings_io:info([TweakInfo,"\n",Info]);
		    false when TweakInfo =:= [] ->
		        ok;
		    false ->
		        wings_io:info(TweakInfo)
		end,
		wings_tweak:tweak_keys_info()
	end,
    wings_io:batch(Render).

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
        error({in_use_by,OtherId}, [Window,Id,Fun])
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
    update_menus(St),
    main_loop(clear_temp_sel(St)).

update_menus(St) ->
    wings_menu:update_menu(edit, repeat, command_name(?__(1,"Repeat"), St)),
    wings_menu:update_menu(edit, repeat_args, command_name(?__(2,"Repeat Args"), St)),
    wings_menu:update_menu(edit, repeat_drag, command_name(?__(3,"Repeat Drag"), St)),
    wings_sel_cmd:update_menu(St).

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
    crash_logger(Crash),
    main_loop(St);
handle_event({crash_in_other_window,LogName}, St) ->
    crash_dialog(LogName),
    main_loop(St);
handle_event({open_file,Name}, St0) ->
    case catch ?SLOW(wings_ff_wings:import(Name, St0)) of
	#st{}=St1 ->
	    St2 = wings_shape:recreate_folder_system(St1),
	    USFile = wings_file:autosave_filename(wings_file:unsaved_filename()),
	    St = case USFile of
		     Name ->
			 St2#st{saved=auto,file=undefined};
		     _ ->
			 wings_pref:set_value(current_directory, filename:dirname(Name)),
			 St2#st{saved=true,file=Name}
		 end,
	    update_menus(St),
	    main_loop(wings_u:caption(St));
	{error,_} ->
	    main_loop(St0)
    end;
handle_event(Ev, St) ->
    case wings_camera:event(Ev, St) of
      next -> handle_event_tweak(Ev, St);
      Other -> Other
    end.

handle_event_tweak(Ev, St) ->
%% Check for Tweak events
    case wings_tweak:tweak_event(Ev, St) of
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

handle_event_2(#mousebutton{}=Ev, St) ->
    case wings_menu:is_popup_event(Ev) of
	no ->
	    handle_event_3(Ev, St);
	{yes,Xglobal,Yglobal,Mod} ->
	    TweakBits = wings_msg:free_rmb_modifier(),
	    case Mod band TweakBits =/= 0 of
		true ->
		    wings_tweak:menu(Xglobal, Yglobal);
		false ->
		    handle_popup_event(Ev, Xglobal, Yglobal, St)
	    end
    end;
handle_event_2(Ev, St) -> handle_event_3(Ev, St).

handle_event_3(#keyboard{}=Ev, St0) ->
    case do_hotkey(Ev, St0) of
	next -> keep;
	{Cmd,St} -> do_command(Cmd, Ev, St)
    end;
handle_event_3({action,Callback}, _) when is_function(Callback) ->
    Callback();
handle_event_3({action,Cmd}, St) ->
    do_command(Cmd, none, St);
handle_event_3({vec_command,Command,St}, _) when is_function(Command) ->
    %% Use to execute command with vector arguments (see wings_vec.erl).
    command_response(Command(), none, St);
handle_event_3(#mousebutton{}, _St) -> keep;
handle_event_3(#mousemotion{}, _St) -> keep;
handle_event_3(init_opengl, St) ->
    init_opengl(St),
    main_loop_noredraw(St);
handle_event_3(resized, _) -> keep;
handle_event_3(close, _) ->
    delete;
handle_event_3(redraw, St) ->
    redraw(St),
    main_loop_noredraw(St);
handle_event_3(quit, St) ->
    case wings_wm:this() of
	geom -> do_command({file,quit}, none, St);
	_ -> keep
    end;
handle_event_3({new_state,St}, St0) ->
    info_line(),
    save_state(St0, St);
handle_event_3({update_state,St}, _) ->
    main_loop(St);
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
    info_line(),
    keep;
handle_event_3(lost_focus, _) -> keep;
handle_event_3(grab_lost, _) -> keep;
handle_event_3({note,menu_aborted}, St) ->
    main_loop(clear_temp_sel(St));
handle_event_3({note,_}, _) ->
    keep;
handle_event_3({drop,Pos,DropData}, St) ->
    handle_drop(DropData, Pos, St);
handle_event_3(language_changed, _) ->
    This = wings_wm:this(),
    wings_wm:toplevel_title(This, geom_title(This)),
    keep;
handle_event_3({external,no_more_basic_menus}, _St) ->
    wings_help:no_more_basic_menus();
handle_event_3({external,not_possible_to_save_prefs}, _St) ->
    wings_help:not_possible_to_save_prefs();
handle_event_3({external, win32_start_maximized}, _St) ->
    keep;
handle_event_3({external, Fun}, St) 
  when is_function(Fun) ->
    Fun(St);
handle_event_3({external,Op}, St) ->
    wpa:handle_external(Op,St),
    keep;
handle_event_3(ignore, _St) ->
    keep;
handle_event_3({adv_menu_abort, Ev}, _St) ->
    This = wings_wm:actual_focus_window(),
    wings_wm:send(This,Ev);
handle_event_3({camera,Ev,NextEv}, St) ->
    %% used by preview dialogs in (blanket event)
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_camera:event(Ev#mousebutton{x=X,y=Y}, St) of
      next -> NextEv;
      Other -> Other
    end.

handle_popup_event(Ev, Xglobal, Yglobal, St0) ->
    #mousebutton{x=X,y=Y} = Ev,
    #st{sel=Sel} = St0,
    case Sel of
	[] ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} ->
		    wings_io:putback_event(Ev),
		    wings_wm:later({temporary_selection,St});
		_ ->
		    popup_menu(Xglobal, Yglobal, St0)
	    end;
	_ ->
	    popup_menu(Xglobal, Yglobal, St0)
    end.

info_line() ->
    case wings_pref:get_value(tweak_active) of
	false ->
	    Msg1 = wings_msg:button_format(?__(1,"Select")),
	    Msg2 = wings_camera:help(),
	    Msg3 = wings_msg:button_format([], [], ?__(2,"Show menu")),
	    Msg4 = wings_msg:rmb_format(?__(3,"Tweak menu")),
	    Message = wings_msg:join([Msg1,Msg2,Msg3,Msg4]),
	    wings_tweak:tweak_disabled_msg(),
	    wings_wm:message(Message);
	true ->
	    wings_tweak:tweak_info_line(),
	    wings_tweak:tweak_magnet_help()
    end.

do_hotkey(Ev, #st{sel=[]}=St0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
    {add,_,St} ->
        case wings_hotkey:event(Ev, St) of
        next -> next;
        {view,highlight_aim} -> highlight_aim_setup(St0);
        Cmd ->
            case highlight_sel_style(Cmd) of
            none -> {Cmd,St0};
            temporary -> {Cmd,set_temp_sel(St0, St)};
            permanent -> {Cmd,St}
            end
        end;
    _Other -> do_hotkey_1(Ev, St0)
    end;
%%%% Hack to get a basic form of Highlight Aim (see wings_view:highlight_aim/2)
do_hotkey(Ev, St0) ->
    Hs = wings_pref:get_value(hilite_select),
    case wings_hotkey:event(Ev, St0) of
      next -> next;
      {view,highlight_aim} -> highlight_aim_setup(St0);
      {select,{edge_loop,edge_loop}}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,{edge_loop,edge_ring}}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,{oriented_faces,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,{similar_material,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,{similar_area,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,similar}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      {select,all}=Cmd when Hs -> hotkey_select_setup(Cmd,St0);
      Cmd -> {Cmd,St0}
    end.

do_hotkey_1(Ev, St) ->
    case wings_hotkey:event(Ev, St) of
    next -> next;
    {view,highlight_aim} -> {{view,aim}, St};
    Cmd -> {Cmd,St}
    end.

highlight_sel_style({vertex,_}) -> temporary;
highlight_sel_style({edge,_}) -> temporary;
highlight_sel_style({face,_}) -> temporary;
highlight_sel_style({body,_}) -> temporary;
highlight_sel_style({edit,repeat}) -> temporary;
highlight_sel_style({edit,repeat_args}) -> temporary;
highlight_sel_style({edit,repeat_drag}) -> temporary;
highlight_sel_style({select,deselect}) -> none;
highlight_sel_style({select,vertex}) -> none;
highlight_sel_style({select,edge}) -> none;
highlight_sel_style({select,face}) -> none;
highlight_sel_style({select,body}) -> none;
highlight_sel_style({select,{adjacent,_}}) -> none;
highlight_sel_style({select,_}) -> permanent;
highlight_sel_style({tools,{virtual_mirror,_}}) -> temporary;
highlight_sel_style({tools,{align,_}}) -> temporary;
highlight_sel_style({tools,{center,_}}) -> temporary;
highlight_sel_style({tools,put_on_ground}) -> temporary;
highlight_sel_style({tools,{bbox,save_bb}}) -> temporary;
highlight_sel_style({tools,{bbox,{scale_to_bb,_}}}) -> temporary;
highlight_sel_style({tools,{bbox,{scale_to_bb_prop,_}}}) -> temporary;
highlight_sel_style({tools,{bbox,{move_to_bb,_}}}) -> temporary;
highlight_sel_style({tools,{bbox,{move_bb_to_sel,_}}}) -> temporary;
highlight_sel_style({tools,{bbox,{scale_bb_to_sel,_}}}) -> temporary;
highlight_sel_style({view,align_to_selection}) -> temporary;
highlight_sel_style({view,aim}) -> temporary;
highlight_sel_style({view,highlight_aim}) -> temporary;
highlight_sel_style({view,frame}) -> temporary;

highlight_sel_style(_) -> none.

do_command(Cmd, Event, St0) ->
    St = remember_command(Cmd, St0),
    {replace,
     fun(Ev) -> handle_event(Ev, St) end,
     fun() -> raw_command(Cmd, none, Event, St) end}.

raw_command(Cmd, Args, St) ->
    raw_command(Cmd, Args, none, St).

raw_command(Cmd, Args, Event, St) ->
    command_response(raw_command_1(Cmd, Event, St), Args, St).

raw_command_1(Cmd, Event, St0) ->
    case wings_plugin:command(Cmd, St0#st{last_cmd=Cmd}) of
	next ->
	    %% Time the command if command timing is enabled,
	    %% or just execute the command.
	    Execute = fun() ->
			      execute_command(Cmd, Event, St0)
		      end,
	    wings_develop:time_command(Execute, Cmd);
	St0 -> St0;
	#st{}=St -> {save_state,St};
	Other -> Other
    end.

execute_command(Cmd, none, St) ->
    %% The command was obtained directly from a menu.
    %% It it not allowed to fail.
    command(Cmd, St);
execute_command(Cmd, Ev, St) ->
    %% The command was obtained through a hotkey, which for all
    %% we know may be from an ancient version Wings or for a
    %% plug-in that has been disabled.
    try
	wings_menu:check_item(Cmd),
	command(Cmd, St)
    catch
	error:Reason ->
	    io:format("~p:~p: Error: ~p ~p~n", [?MODULE, ?LINE, Reason, erlang:get_stacktrace()]),
	    wings_hotkey:handle_error(Ev, Cmd),
	    St#st{repeatable=ignore}
    end.

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

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
                     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,ask_args=none,drag_args=none};
remember_command(_Cmd, St) -> St.

%% Test if the saved command can be safely repeated, and
%% rewrite it with the current selection mode if needed.
repeatable(Mode, Cmd) ->
%   io:format("Cmd ~p\n",[Cmd]),
    case Cmd of
    {Mode,_} -> Cmd;			%Same mode is always OK.

    %% Commands safe in all modes.
    {_,{move,normal}} when Mode == body -> no;
    {_,{_,region}} when Mode =/= face -> no;
    {_,{move,_}=C} -> {Mode,C};
    {_,{rotate,{_,{_,[normal],_}}}}when Mode == vertex; Mode == body -> no;
    {_,{rotate,{_,{_,[_,normal],_}}}}when Mode == vertex; Mode == body -> no;
    {_,{rotate,_}=C} -> {Mode,C};
    {_,{scale,_}=C} -> {Mode,C};
    {_,{move_planar,_}=C} -> {Mode,C};
    {_,{absolute,_}=C} -> {Mode,C};
    {_,{arc_intersect,_}=C} -> {Mode,C};

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

%% Handle Undo/Redo first.
command({edit,undo_toggle}, St) ->
    wings_u:caption(wings_undo:undo_toggle(St));
command({edit,undo}, St) ->
    wings_u:caption(wings_undo:undo(St));
command({edit,redo}, St) ->
    wings_u:caption(wings_undo:redo(St));
command(Cmd, St) ->
    command_1(Cmd, St#st{last_cmd=Cmd}).

%% Vector and secondary-selection commands.
command_1({shape,Shape}, St0) ->
    case wings_shapes:command(Shape, St0) of
        St0 -> St0;
    #st{}=St -> {save_state,St};
    Other -> Other
    end;
command_1({help,What}, St) ->
    wings_help:command(What, St);

%% Drag & drop.
command_1({drop,What}, St) ->
    drop_command(What, St);

%% File menu.
command_1({file,Command}, St) ->
    wings_file:command(Command, St);

%% Edit menu.
command_1({edit,repeat}, #st{sel=[]}=St) -> St;
command_1({edit,repeat}, #st{selmode=Mode,repeatable=Cmd0}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd when is_tuple(Cmd) -> raw_command(Cmd, none, St)
    end;
command_1({edit,repeat}, St) -> St;
command_1({edit,repeat_args}, #st{sel=[]}=St) -> St;
command_1({edit,repeat_args}, #st{selmode=Mode,repeatable=Cmd0,
				  ask_args=AskArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, none, St)
    end;
command_1({edit,repeat_args}, St) -> St;
command_1({edit,repeat_drag}, #st{sel=[]}=St) -> St;
command_1({edit,repeat_drag}, #st{selmode=Mode,repeatable=Cmd0,
				  ask_args=AskArgs,drag_args=DragArgs}=St) ->
    case repeatable(Mode, Cmd0) of
	no -> keep;
	Cmd1 when is_tuple(Cmd1) ->
	    Cmd = replace_ask(Cmd1, AskArgs),
	    raw_command(Cmd, DragArgs, St)
    end;
command_1({edit,repeat_drag}, St) -> St;
command_1({edit,purge_undo}, St) ->
    purge_undo(St);
command_1({edit,confirmed_purge_undo}, St) ->
    wings_undo:init(St);
command_1({edit,enable_patches}, St) ->
    wings_start:enable_patches(),
    St;
command_1({edit,disable_patches}, St) ->
    wings_start:disable_patches(),
    St;
command_1({edit,{preferences,Pref}}, St) ->
    wings_pref_dlg:command(Pref, St);
command_1({edit,{theme,Theme}}, St) ->
    wings_pref:pref({load,Theme,St}),
    keep;

%% Select menu.
command_1({select,Command}, St) ->
    wings_sel_cmd:command(Command, St);

%% View menu.
command_1({view,Command}, St) ->
    wings_view:command(Command, St);

%% Window menu.
command_1({window,geom_viewer}, St) ->
    new_viewer(St),
    keep;
command_1({window,outliner}, St) ->
    wings_outliner:window(St);
command_1({window,object}, St) ->
    wings_geom_win:window(St);
command_1({window,palette}, St) ->
    wings_palette:window(St);
command_1({window,console}, _St) ->
    wings_console:window(),
    keep;
command_1({window,tweak_palette}, St) ->
    wings_tweak_win:window(tweak_palette, St),
    wings_tweak_win:window(tweak_magnet, St),
    wings_tweak_win:window(axis_constraint, St),
    keep;

%% Body menu.
command_1({body,Cmd}, St) ->
    wings_body:command(Cmd, St);

%% Face menu.
command_1({face,Cmd}, St) ->
    wings_face_cmd:command(Cmd, St);

%% Edge commands.
command_1({edge,Cmd}, St) ->
    wings_edge_cmd:command(Cmd, St);

%% Vertex menu.
command_1({vertex,Cmd}, St) ->
    wings_vertex_cmd:command(Cmd, St);

%% Light menu.
command_1({light,Cmd}, St) ->
    wings_light:command(Cmd, St);

%% Material commands.
command_1({material,Cmd}, St) ->
    wings_material:command(Cmd, St);

%% Tools menu.

command_1({tools,{set_default_axis, Type0}}, St) ->
    Type = case Type0 of
      axis_point -> [axis,point];
      _ -> [Type0]
    end,
    wings:ask({Type,[]}, St,
          fun({Axis,Point}, _) ->
               Default = {Point,Axis},
               wings_pref:set_value(default_axis, Default),
               keep;
             (NewDef, _) ->
               {Point,Axis} = wings_pref:get_value(default_axis),
               Default = case Type0 of
                 axis -> {Point, NewDef};
                 point -> {NewDef, Axis}
               end,
               wings_pref:set_value(default_axis, Default),
               keep
          end);
command_1({tools,{align,Dir}}, St) ->
    {save_state,wings_align:align(Dir, St)};
command_1({tools,{center,Dir}}, St) ->
    {save_state,wings_align:center(Dir, St)};
command_1({tools,{bbox,save_bb}}, St) ->
    wings_align:copy_bb(St);
command_1({tools,{bbox,{scale_to_bb,Dir}}}, St) ->
    {save_state,wings_align:scale_to_bb(Dir, St)};
command_1({tools,{bbox,{scale_to_bb_prop,Dir}}}, St) ->
    {save_state,wings_align:scale_to_bb_prop(Dir, St)};
command_1({tools,{bbox,{move_to_bb,Dir}}}, St) ->
    {save_state,wings_align:move_to_bb(Dir, St)};
command_1({tools,{bbox,{move_bb_to_sel,Dir}}}, St) ->
    {save_state,wings_align:move_bb_to_sel(Dir, St)};
command_1({tools,{bbox,{scale_bb_to_sel,Dir}}}, St) ->
    {save_state,wings_align:scale_bb_to_sel(Dir, St)};
command_1({tools,{virtual_mirror,Cmd}}, St) ->
    wings_view:virtual_mirror(Cmd, St);
command_1({tools, {screenshot,Ask}}, St) ->
    wings_image:screenshot(Ask,St);
command_1({tools, object_info}, St) ->
    object_info(St),
    St;
command_1({tools, put_on_ground}, St) ->
    {save_state,wings_align:put_on_ground(St)};
command_1({tools, unitize}, St) ->
    {save_state,wings_align:unitize(St)};
command_1({tools, tweak_menu}, _St) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    wings_tweak:menu(X, Y);

%% Develop menu.
command_1({develop,Cmd}, St) ->
    wings_develop:command(Cmd, St);

%% wings_job action events.
command_1({wings_job,Command}, St) ->
    wings_job:command(Command, St);

%% Tweak menu
command_1({tweak, Cmd}, St) ->
    wings_tweak:command(Cmd, St);

%% Hotkey setup or delete
command_1({hotkey, Cmd}, St) ->
    wings_hotkey:command(Cmd, St).


popup_menu(X, Y, #st{sel=[]}) ->
    wings_shapes:menu(wings_wm:this_win(), wings_wm:local2screen({X,Y}));
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

edit_menu() ->
    St = #st{},
    UndoInfo = ?__(1,"Delete undo history to reclaim memory"),
    [{?__(3,"Undo/Redo"),undo_toggle,
      ?__(4,"Undo or redo the last command")},
     {?__(5,"Redo"),redo,
      ?__(6,"Redo the last command that was undone")},
     {?__(7,"Undo"),undo,
      ?__(8,"Undo the last command")},
     separator,
     {wings:command_name(?__(9,"Repeat"), St),repeat},
     {wings:command_name(?__(10,"Repeat Args"), St),repeat_args},
     {wings:command_name(?__(11,"Repeat Drag"), St),repeat_drag},
     separator,
     wings_pref_dlg:menu(),
     {?__(12,"Plug-in Preferences"),{plugin_preferences,[]}},
     wings_theme:menu(),
     separator,
     {?__(13,"Purge Undo History"),purge_undo,UndoInfo}|patches()].

tools_menu() ->
    [{?__(8,"Align"),{align,tool_dirs(align)}},
     {?__(9,"Center"),{center,tool_dirs(center)}},
     separator,
     {?__(37,"Bounding Box"),
      {bbox,
       [{?__(10,"Save Bounding Box"),save_bb,
	 ?__(39,"Create bounding box around current selection")},
	{?__(11,"Scale to Saved BB"),{scale_to_bb,tool_dirs(bb)}},
	{?__(12,"Scale to Saved BB Proportionally"),{scale_to_bb_prop,tool_dirs(bb)}},
	{?__(13,"Move to Saved BB"),{move_to_bb,wings_menu_util:all_xyz()}},
	{?__(32,"Move BB to Selection"),{move_bb_to_sel,wings_menu_util:all_xyz()}},
	{?__(33,"Scale BB to Selection"),{scale_bb_to_sel,tool_dirs(bb)}}]},
      ?__(38,"Bounding boxes are useful for scaling or aligning objects")},
     separator,
     {?__(14,"Set Default Axis"),{set_default_axis,
				  [{?__(34,"Set Axis and Point"),axis_point},
				   {?__(35,"Set Axis"),axis},
				   {?__(36,"Set Point"),point}]},
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
     {?__(24,"Screenshot..."), screenshot,
      ?__(25,"Grab an image of the window (export it from the outliner)"),[option]},
     separator,
     {?__(26,"Scene Info"), object_info,
      ?__(27,"Calculate area and volume for each object in the scene")},
     {?__(28,"Put on Ground"), put_on_ground,
      ?__(29,"Put selected objects on the ground plane")},
     {?__(30,"Unitize"), unitize,
      ?__(31,"Scale selected objects to fit inside a unit sphere and move to origin")},
     separator,
     {?__(40,"Tweak"),tweak_menu,?__(41,"Open the Tweak menu")}].

window_menu() ->
    Name = case wings_wm:this() of
	       {_,geom} ->
		   ?__(1,"Geometry Graph");
	       {_,{geom,N}} ->
		   ?__(2,"Geometry Graph #") ++ integer_to_list(N);
	       _ -> ignore
	   end,
    if Name =:= ignore -> [];
       true ->
	    [{?__(3,"Outliner"),outliner,
	      ?__(4,"Open the outliner window (showing materials and objects)")},
	     {Name,object,
	      ?__(5,"Open a Geometry Graph window (showing objects)")},
	     {?__(6,"Palette"), palette,?__(7,"Open the color palette window")},
	     {?__(12,"Tweak Palette"), tweak_palette,
	      ?__(13,"Open palettes from which tweak tools may be selected or bound to modifier keys")},
	     separator,
	     {?__(8,"New Geometry Window"),geom_viewer, ?__(9,"Open a new Geometry window")},
	     {?__(10,"Log Window"),console,?__(11,"Open a log window for information messages")}]
    end.

tool_dirs(Tool) ->
    Help = case Tool of
	       align -> ?__(1,"Align two or more objects along the given axis according to their respective selection centers");
	       center -> ?__(2,"Center the selected objects along the given axis according to their cumulative selection center");
	       bb -> []
	   end,
    [{wings_s:dir(all),all,Help},
     {wings_s:dir(x),x,Help},
     {wings_s:dir(y),y,Help},
     {wings_s:dir(z),z,Help},
     {wings_s:dir(radial_x),radial_x,Help},
     {wings_s:dir(radial_y),radial_y,Help},
     {wings_s:dir(radial_z),radial_z,Help}].

patches() ->
    case wings_start:get_patches() of
	none -> [];
	{enabled,Desc} ->
	    [separator,{?__(1,"Use ")++Desc,disable_patches,[{crossmark, true}]}];
	{disabled,Desc} ->
	    [separator,{?__(1,"Use ")++Desc,enable_patches,[{crossmark, false}]}]
    end.

set_temp_sel(#st{sh=Sh,selmode=Mode}, St) ->
    St#st{temp_sel={Mode,Sh}}.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.


purge_undo(St) ->
    {Un,Rn} = wings_undo:info(St),
    Qs = [{vframe,
	   [{label,?__(1,"Undo states: ") ++ integer_to_list(Un)},
	    {label,?__(2,"Redo states: ")  ++ integer_to_list(Rn)}]}],
    wings_dialog:dialog("Purge Undo", Qs,
			fun(_) -> {edit,confirmed_purge_undo} end).

info(#st{sel=[]}) ->
    [],
    Progs = get(light_shaders),
    NumLights = wings_pref:get_value(number_of_lights),
    ActiveSh = wings_pref:get_value(active_shader),
    UseProg = (Progs /= undefined) and (NumLights == 2),
    case UseProg of
     true ->
	 {_Prog,Name} = element(ActiveSh, Progs),
	 io_lib:format("Shader ~p of ~p: ~s ",[ActiveSh,tuple_size(Progs),Name]);
     false ->
	 []
    end;
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
       [Base|io_lib:format(?__(1,". Position <~s  ~s  ~s>"),
                          [wings_util:nice_float(X),
                           wings_util:nice_float(Y),
                           wings_util:nice_float(Z)])];
    2 ->
        We = gb_trees:get(Id, Shs),
        [Va,Vb] = gb_sets:to_list(Vs),
            {Xa,Ya,Za} = wings_vertex:pos(Va, We),
            {Xb,Yb,Zb} = wings_vertex:pos(Vb, We),
        Dist = e3d_vec:dist({Xa,Ya,Za},{Xb,Yb,Zb}),
       [Base|io_lib:format(?__(5,". Distance ~s  <~s  ~s  ~s>"),
                          [wings_util:nice_float(Dist),
                           wings_util:nice_float(abs(Xb - Xa)),
                           wings_util:nice_float(abs(Yb - Ya)),
                           wings_util:nice_float(abs(Zb - Za))])];
    _ -> Base
    end;
measure(Base, #st{selmode=vertex,sel=[{IdA,VsA},{IdB,VsB}],shapes=Shs}) ->
    case gb_sets:size(VsA) == 1 andalso gb_sets:size(VsB) == 1 of
    true ->
        WeA = gb_trees:get(IdA, Shs),
        WeB = gb_trees:get(IdB, Shs),
        [Va] = gb_sets:to_list(VsA),
        [Vb] = gb_sets:to_list(VsB),
       {Xa,Ya,Za} = wings_vertex:pos(Va, WeA),
       {Xb,Yb,Zb} = wings_vertex:pos(Vb, WeB),
        Dist = e3d_vec:dist(wings_vertex:pos(Va, WeA),
                            wings_vertex:pos(Vb, WeB)),
       [Base|io_lib:format(?__(5,". Distance ~s  <~s  ~s  ~s>"),
               [wings_util:nice_float(Dist),
                wings_util:nice_float(Xb - Xa),
                wings_util:nice_float(Yb - Ya),
                wings_util:nice_float(Zb - Za)])];
    _ -> Base
    end;
measure(Base, #st{selmode=edge,sel=[{Id,Es}],shapes=Shs}) ->
    case gb_sets:size(Es) of
        1 ->
            We = gb_trees:get(Id, Shs),
            [Edge] = gb_sets:to_list(Es),
            #edge{vs=Va,ve=Vb} = array:get(Edge, We#we.es),
            {Xa,Ya,Za} = wings_vertex:pos(Va, We),
            {Xb,Yb,Zb} = wings_vertex:pos(Vb, We),
            Length = e3d_vec:dist({Xa,Ya,Za}, {Xb,Yb,Zb}),
            {X,Y,Z} = e3d_vec:average({Xa,Ya,Za}, {Xb,Yb,Zb}),
            [Base|io_lib:format(?__(3,". Midpoint <~s  ~s  ~s>\nLength ~s") ++
                                "  <~s  ~s  ~s>",
                                [wings_util:nice_float(X),
                                 wings_util:nice_float(Y),
                                 wings_util:nice_float(Z),
                                 wings_util:nice_float(Length),
                                 wings_util:nice_float(abs(Xb - Xa)),
                                 wings_util:nice_float(abs(Yb - Ya)),
                                 wings_util:nice_float(abs(Zb - Za))])];
        2 ->
            We = gb_trees:get(Id, Shs),
            [E0,E1] = gb_sets:to_list(Es),
            #edge{vs=V0s,ve=V0e} = array:get(E0, We#we.es),
            #edge{vs=V1s,ve=V1e} = array:get(E1, We#we.es),
            PosA = {X0s,Y0s,Z0s} = wings_vertex:pos(V0s, We),
            PosB = {X0e,Y0e,Z0e} = wings_vertex:pos(V0e, We),
            PosC = {X1s,Y1s,Z1s} = wings_vertex:pos(V1s, We),
            PosD = {X1e,Y1e,Z1e} = wings_vertex:pos(V1e, We),
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
            enhanced_info([Base|io_lib:format(?__(6,". Angle ~s")++"~c",
                          [wings_util:nice_float(Angle),?DEGREE])],
                          {PosA, PosB, PosC, PosD, E0, E1});
        _ -> 
            Base
    end;

measure(Base, #st{shapes=Shs, selmode=edge, sel=[{Id1,Sel1},{Id2,Sel2}]}) ->
    case gb_sets:size(Sel1) == 1 andalso gb_sets:size(Sel2) == 1 of
    true ->
      WeA = gb_trees:get(Id1, Shs),
      WeB = gb_trees:get(Id2, Shs),
      [Es0] = gb_sets:to_list(Sel1),
      [Es1] = gb_sets:to_list(Sel2),
      #edge{vs=V0s,ve=V0e} = array:get(Es0, WeA#we.es),
      #edge{vs=V1s,ve=V1e} = array:get(Es1, WeB#we.es),
      PosA = {X0s,Y0s,Z0s} = wings_vertex:pos(V0s, WeA),
      PosB = {X0e,Y0e,Z0e} = wings_vertex:pos(V0e, WeA),
      PosC = {X1s,Y1s,Z1s} = wings_vertex:pos(V1s, WeB),
      PosD = {X1e,Y1e,Z1e} = wings_vertex:pos(V1e, WeB),
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
      enhanced_info([Base|io_lib:format(?__(6,". Angle ~s")++"~c",
                    [wings_util:nice_float(Angle),?DEGREE])],
                    {PosA, PosB, PosC, PosD, Es0, Es1, Id1, Id2});
    false->
        Base
    end;
measure(Base, #st{selmode=face,sel=[{Id,Fs}],shapes=Shs}) ->
    case gb_sets:size(Fs) of
    1 ->
        We = gb_trees:get(Id, Shs),
        [Face] = gb_sets:to_list(Fs),
        {X,Y,Z} = wings_face:center(Face, We),
        Mat = wings_facemat:face(Face, We),
        Area = area_info(Face, We),
        [Base|wings_util:format(?__(4,". Midpoint <~s  ~s  ~s> \nMaterial ~s.")
                            ++ Area,
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
      enhanced_info([Base|io_lib:format(?__(6,". Angle ~s")++"~c",
      [wings_util:nice_float(Angle),?DEGREE])],{We,F0,F1});
    _ -> Base
    end;
measure(Base, #st{shapes=Shs, selmode=face, sel=[{Id1,Fs1},{Id2,Fs2}]}) ->
    case gb_sets:size(Fs1) == 1 andalso gb_sets:size(Fs2) == 1 of
    true ->
        WeA = gb_trees:get(Id1, Shs),
        WeB = gb_trees:get(Id2, Shs),
        [F0] = gb_sets:to_list(Fs1),
        [F1] = gb_sets:to_list(Fs2),
          N0 = wings_face:normal(F0, WeA),
          N1 = wings_face:normal(F1, WeB),
        Angle = e3d_vec:degrees(N0,N1),
        enhanced_info([Base|io_lib:format(?__(6,". Angle ~s")++"~c",
                      [wings_util:nice_float(Angle),?DEGREE])],
                      {face,WeA,WeB,F0,F1,Id1,Id2});
    false ->
        Base
    end;

measure(Base, _) -> Base.

item_list(Items, Desc) ->
    item_list(Items, " ", Desc).

item_list([Item|Items], Sep, Desc) ->
    item_list(Items, ", ", [Desc,Sep|integer_to_list(Item)]);
item_list([], _Sep, Desc) -> [Desc|?__(1," selected")].

shape_info(We) when ?IS_LIGHT(We) ->
    wings_light:info(We);
shape_info(#we{id=Id,name=Name,fs=Ftab,es=Etab,vp=Vtab}=We) ->
    Faces = gb_trees:size(Ftab),
    Edges = wings_util:array_entries(Etab),
    Vertices = wings_util:array_entries(Vtab),
    wings_util:format(?__(new_object_info,
			  "Object ~p \"~s\" has ~p polygons, "
			  "~p edges, ~p vertices~s~s."),
              [Id,Name,Faces,Edges,Vertices,
	       vtx_attributes(We),hole_info(We)]).

vtx_attributes(We) ->
    case wings_va:any_attributes(We) of
	false -> "";
	true -> ", " ++ ?__(1,"vertex attributes")
    end.

hole_info(#we{holes=[]}) ->
    "";
hole_info(#we{holes=Holes}) ->
    case length(Holes) of
	1 -> [", 1 ",?__(1,"hole")];
	N -> [", ",integer_to_list(N)," ",?__(2,"holes")]
    end.

shape_info(Objs, Shs) ->
    shape_info(Objs, Shs, 0, 0, 0, 0).

shape_info([{Id,_}|Objs], Shs, On, Vn, En, Fn) ->
    #we{fs=Ftab,es=Etab,vp=Vtab} = gb_trees:get(Id, Shs),
    Faces = gb_trees:size(Ftab),
    Edges = wings_util:array_entries(Etab),
    Vertices = wings_util:array_entries(Vtab),
    shape_info(Objs, Shs, On+1, Vn+Vertices, En+Edges, Fn+Faces);
shape_info([], _Shs, N, Vertices, Edges, Faces) ->
    io_lib:format(?__(2,
             "~p objects, ~p faces, ~p edges, ~p vertices"),
          [N,Faces,Edges,Vertices]).

enhanced_info(Basic, {PosA, PosB, PosC, PosD, E0, E1}) ->
    case wings_pref:get_value(info_enhanced_text) of
        true ->
            Length1 = e3d_vec:dist(PosA,PosB),
            Length2 = e3d_vec:dist(PosC,PosD),
            {Xa,Ya,Za} = e3d_vec:average(PosA,PosB),
            {Xb,Yb,Zb} = e3d_vec:average(PosC,PosD),
               Dist = e3d_vec:dist({Xa,Ya,Za}, {Xb,Yb,Zb}),
               Diff = abs(Length1 - Length2),
            [Basic|io_lib:format(?__(42,"\nDistance ~s")++"  <~s  ~s  ~s>\n"++
                                 ?__(41,"Edge~s ~s")++"  "++?__(41,"Edge~s ~s")
                                 ++"  "++?__(45,"Difference ~s"),
                                 [wings_util:nice_float(Dist),
                                  wings_util:nice_float(abs(Xb - Xa)),
                                  wings_util:nice_float(abs(Yb - Ya)),
                                  wings_util:nice_float(abs(Zb - Za)),
                                  wings_util:stringify(E0),
                                  wings_util:nice_float(Length1),
                                  wings_util:stringify(E1),
                                  wings_util:nice_float(Length2),
                                  wings_util:nice_float(Diff)])];
        false -> 
            Basic
    end;
enhanced_info(Basic, {PosA, PosB, PosC, PosD, Es0, Es1, Id1, Id2}) ->
    case wings_pref:get_value(info_enhanced_text) of
        true ->
            Length1 = e3d_vec:dist(PosA,PosB),
            Length2 = e3d_vec:dist(PosC,PosD),
            {Xa,Ya,Za} = e3d_vec:average(PosA,PosB),
            {Xb,Yb,Zb} = e3d_vec:average(PosC,PosD),
            Dist = e3d_vec:dist({Xa,Ya,Za}, {Xb,Yb,Zb}),
            Diff = abs(Length1 - Length2),
            [Basic|io_lib:format(?__(42,"\nDistance ~s")++"  <~s  ~s  ~s>\n"++
                          ?__(43,"Object~s")++" "++?__(41,"Edge~s ~s")++"  "++
                          ?__(43,"Object~s")++" "++?__(41,"Edge~s ~s")++"  "++
                          ?__(45,"Difference ~s"),
                              [wings_util:nice_float(Dist),
                               wings_util:nice_float(abs(Xb - Xa)),
                               wings_util:nice_float(abs(Yb - Ya)),
                               wings_util:nice_float(abs(Zb - Za)),
                               wings_util:stringify(Id1),
                               wings_util:stringify(Es0),
                               wings_util:nice_float(Length1),
                               wings_util:stringify(Id2),
                               wings_util:stringify(Es1),
                               wings_util:nice_float(Length2),
                               wings_util:nice_float(Diff)])];
        false ->
            Basic
    end;
enhanced_info(Basic,{We,F0,F1}) ->
    case wings_pref:get_value(info_enhanced_text) of
        true ->
            {Xa,Ya,Za} = wings_face:center(F0, We),
            {Xb,Yb,Zb} = wings_face:center(F1, We),
            Dist = e3d_vec:dist({Xa,Ya,Za},{Xb,Yb,Zb}),
            Area0 = area_info(F0, We),
            Area1 = area_info(F1, We),
            [Basic|io_lib:format(?__(42,"\nDistance ~s")++"  <~s  ~s  ~s>\n"
                                 ++ ?__(48,"Face~s") ++ Area0 ++ "  " 
                                 ++ ?__(48,"Face~s") ++ Area1,
                                [wings_util:nice_float(Dist),
                                 wings_util:nice_float(abs(Xb - Xa)),
                                 wings_util:nice_float(abs(Yb - Ya)),
                                 wings_util:nice_float(abs(Zb - Za)),
								 wings_util:stringify(F0),
								 wings_util:stringify(F1)])];
        false ->
            Basic
    end;
enhanced_info(Basic,{face,WeA, WeB, F0, F1,Id1,Id2}) ->
    case wings_pref:get_value(info_enhanced_text) of
        true ->
           {Xa,Ya,Za} = wings_face:center(F0, WeA),
           {Xb,Yb,Zb} = wings_face:center(F1, WeB),
           Area0 = area_info(F0, WeA),
           Area1 = area_info(F1, WeB),
           Dist = e3d_vec:dist({Xa,Ya,Za},{Xb,Yb,Zb}),
           [Basic|io_lib:format(?__(42,"\nDistance ~s")++"  <~s  ~s  ~s>\n"++
                      ?__(43,"Object~s")++" "++?__(48,"Face~s")++Area0++"  "++
                      ?__(43,"Object~s")++" "++?__(48,"Face~s")++Area1,
                      [wings_util:nice_float(Dist),
                       wings_util:nice_float(abs(Xb - Xa)),
                       wings_util:nice_float(abs(Yb - Ya)),
                       wings_util:nice_float(abs(Zb - Za)),
                       wings_util:stringify(Id1),
                       wings_util:stringify(F0),
                       wings_util:stringify(Id2),
                       wings_util:stringify(F1)])];
        false ->
            Basic
    end.

area_info(Face, We) ->
    case wings_face:vertices(Face,We) =< 50 of
      true -> A = wings_face:area(Face, We),
              wings_util:format(?__(40," Area ~s"), [wings_util:nice_float(A)]);
      false -> []
    end.

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
    end;
do_use_command(_, Cmd, _) -> wings_wm:later({action,Cmd}).

crash_logger(Crash) ->
    LogName = wings_u:crash_log(geom, Crash),
    crash_dialog(LogName).

crash_dialog(LogName) ->
    Parent = wings_dialog:get_dialog_parent(),
    Str = ?__(1, "Internal error - log written to") ++ " " ++ LogName,
    Dialog = wxMessageDialog:new(Parent, Str, [{caption,"Internal Error"}]),
    wxMessageDialog:showModal(Dialog),
    wings_dialog:reset_dialog_parent(Dialog).

%%%
%%% Drag & Drop.
%%%

set_drag_filter(Name) ->
    F = fun({material,_}) -> yes;
       (_) -> no
    end,
    wings_wm:set_prop(Name, drag_filter, F).

handle_drop(DropData, ScreenPos, St) ->
    handle_drop_1(DropData, ScreenPos, St).

handle_drop_1(_, Pos, #st{sel=[]}) ->
    wings_menu:popup_menu(wings_wm:this_win(), Pos, drop,
			  [{?__(1,"No Selection"),cancel_drop, ?__(2,"Cancel drop operation")}]);
handle_drop_1({material,Name}, Pos, #st{selmode=face}) ->
    Menu = [{ ?__(3,"Assign material to selected faces"),menu_cmd(assign_to_sel, Name),
          ?__(4,"Assign material \"")++Name++ ?__(5,"\" only to selected faces")},
        { ?__(6,"Assign material to all faces"),
         menu_cmd(assign_to_body, Name),
          ?__(7,"Assign material \"")++Name++
              ?__(8,"\" to all faces in objects having a selection")}],
    wings_menu:popup_menu(wings_wm:this_win(), Pos, drop, Menu);
handle_drop_1({material,Name}, Pos, _) ->
    Menu = [{ ?__(9,"Assign material to all faces"),
          menu_cmd(assign_to_body, Name),
          ?__(10,"Assign material \"")++Name++
          ?__(11,"\" to all faces in objects having a selection")}],
    wings_menu:popup_menu(wings_wm:this_win(), Pos, drop, Menu).
    
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
save_windows_1([{tweak, _}=Tweak|Ns]) ->
    save_window(Tweak, Ns);
save_windows_1([outliner|Ns]) ->
    save_window(outliner, Ns);
save_windows_1([{object,_}=N|Ns]) ->
    save_window(N, Ns);
save_windows_1([geom=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([{geom,_}=N|Ns]) ->
    save_geom_window(N, Ns);
save_windows_1([{plugin,_}=N|Ns]) ->
    save_plugin_window(N, Ns);
save_windows_1([_|T]) -> save_windows_1(T);
save_windows_1([]) -> [].

save_window(Name, Ns) ->
    case wings_wm:is_wxwindow(Name) of
	true  ->
	    Pos = wings_wm:win_ul(Name),
	    Size = wings_wm:win_size(Name),
	    [{Name, Pos, Size, []}|save_windows_1(Ns)];
	false ->
	    save_windows_1(Ns)
    end.

save_geom_window(Name, Ns) ->
    {Pos,Size} = wings_wm:win_rect(Name),
    Ps = save_geom_props(wings_wm:get_props(Name), []),
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

save_plugin_window(Name, Ns) ->
    case wings_plugin:get_win_data(Name) of
      {M, {_,CtmData}} ->
        {Pos,Size} = wings_wm:win_rect(Name),
        WinInfo = {M, {Name, Pos, Size, CtmData}},
        [WinInfo|save_windows_1(Ns)];
      _ ->
        save_windows_1(Ns)
    end.

restore_windows(St) ->
    %% Sort windows using names as keys to make sure we
    %% create the geometry windows before the object windows.
    %% (Because we set up links.)
    Windows0 = wings_pref:get_value(saved_windows, []),
    Windows1 = sort([{element(1, W),W} || W <- Windows0]),
    Windows = [W || {_,W} <- Windows1],
    restore_windows_1(Windows, St).

restore_windows_1([{geom,{_,_}=_Pos0,{_,_}=_Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    %% Pos = geom_pos(Pos0),
    %% wings_wm:move(geom, Pos, Size),
    set_geom_props(Ps, geom),
    wings_wm:set_prop(geom, tweak_draw, true),
    restore_windows_1(Ws, St);
restore_windows_1([{{geom,_}=Name,Pos,Size,Ps0}|Ws], St) ->
    Ps = geom_props(Ps0),
    new_viewer(Name, Pos, Size, initial_properties(), St),
    set_geom_props(Ps, Name),
    restore_windows_1(Ws, St);
restore_windows_1([{Name,Pos,Size}|Ws0], St) -> % OldFormat
    restore_windows_1([{Name,Pos,Size,[]}|Ws0], St);
restore_windows_1([{Module,{{plugin,_}=Name,{_,_}=Pos,{_,_}=Size,CtmData}}|Ws], St) ->
    wings_plugin:restore_window(Module, Name, Pos, Size, CtmData, St),
    restore_windows_1(Ws, St);
restore_windows_1([{{object,_}=Name,{_,_}=Pos,{_,_}=Size,Ps}|Ws], St) ->
    wings_geom_win:window(Name, validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([{outliner,{_,_}=Pos,{_,_}=Size, Ps}|Ws], St) ->
    wings_outliner:window(validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([{console,{_,_}=Pos,{_,_}=Size, Ps}|Ws], St) ->
    wings_console:window(console, validate_pos(Pos), Size, Ps),
    restore_windows_1(Ws, St);
restore_windows_1([{palette,{_,_}=Pos,{_,_}=Size, Ps}|Ws], St) ->
    wings_palette:window(validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([{{tweak, tweak_palette},{_,_}=Pos, Size, Ps}|Ws], St) ->
    wings_tweak_win:window(tweak_palette, validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([{{tweak, tweak_magnet},{_,_}=Pos, Size, Ps}|Ws], St) ->
    wings_tweak_win:window(tweak_magnet, validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([{{tweak, axis_constraint},{_,_}=Pos, Size, Ps}|Ws], St) ->
    wings_tweak_win:window(axis_constraint, validate_pos(Pos), Size, Ps, St),
    restore_windows_1(Ws, St);
restore_windows_1([_|Ws], St) ->
    restore_windows_1(Ws, St);
restore_windows_1([], _) -> ok.

validate_pos({X,Y}=Pos) ->
    case Y < 0 of
      false -> Pos;
      true -> {X,20}
    end.

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
set_geom_props([], Name) ->
    wings_wm:set_prop(Name, tweak_draw, true).

initial_properties() ->
    [{display_data,geom_display_lists}|wings_view:initial_properties()].

mode_restriction(Modes) ->
    Win = wings_wm:this(),
    case Modes of
	none -> wings_wm:erase_prop(Win, mode_restriction);
	_ ->    wings_wm:set_prop(Win, mode_restriction, Modes)
    end.

clear_mode_restriction() ->
    mode_restriction(none).

get_mode_restriction() ->
    Name = wings_wm:this(),
    case wings_wm:lookup_prop(Name, mode_restriction) of
	none -> [edge,vertex,face,body];
	{value,Other} -> Other
    end.

%%%% Get Objects Info and display in a table, one row per object.
object_info(St) ->
    #st{shapes=Shapes} = St,
    HRow = {
      ?__(1,"#"),
      ?__(2,"Name"),
      ?__(3,"Area"),
      ?__(4,"Perimeter"),
      ?__(5,"Volume"),
      ?__(6,"Edges"),
      ?__(7,"Faces"),
      ?__(8,"Verts") },
    case gb_trees:is_empty(Shapes) of
        true -> wings_u:error_msg(?__(9,"No objects in scene"));
        false ->
            Rows = [get_object_info(Id, Shapes) || Id <- gb_trees:keys(Shapes)],
	    Header = list_to_tuple([{H,H}|| H <- tuple_to_list(HRow)]),
            ColumnWidthList = column_widths([Header|Rows]),
            Qs = [{table,[HRow|Rows],
		   [{max_rows, max(length(Rows),20)},
		    {col_widths, list_to_tuple(ColumnWidthList)}]}],
            Ask = fun(_Res) -> ignore end,
            wings_dialog:dialog(?__(10,"Scene Info: "), Qs, Ask)
    end.


get_object_info(Id, Shapes) ->
    #we{id=Id,name=Name,es=Etab0,fs=Ftab0,vp=VPos0} = We0 = gb_trees:get(Id, Shapes),
    Area      =  wings_we:surface_area(We0),
    Volume    =  wings_we:volume(We0),
    Perimeter =  wings_we:perimeter(We0),
    ToString = fun(Item) ->
		       case Item of
			   Item when is_float(Item) ->
			       lists:flatten(io_lib:format("~.4f", [Item]));
			   Item when is_integer(Item) ->
			       integer_to_list(Item);
			   Item when is_list(Item) ->
			       Item
		       end
	       end,
    NEdge   = wings_util:array_entries(Etab0),
    NFace   = gb_trees:size(Ftab0),
    NVertex =  wings_util:array_entries(VPos0),
    list_to_tuple([{X,ToString(X)}||X<-[Id,Name,Area,Perimeter,Volume,NEdge,NFace,NVertex]]).



%% get maximal column widths needed to display each column in characters.
column_widths(Rows) ->
    [Row|_] = Rows,
    NCols = size(Row),
    MyAcc = fun(Col,Acc) ->
		    Temp = [ begin
				 {_,A} = element(Col,RowTuple),
				 length(A)
			     end || RowTuple <- Rows],
		    Mx = lists:max(Temp), % add a bit of padding
		    [Mx|Acc]
	    end,
    lists:foldr(MyAcc,[], lists:seq(1,NCols)).
%%%%

highlight_aim_setup(St0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
	{add,MM,#st{selmode=Selmode,sel=Sel}} ->
	    {{view,{highlight_aim,{add,{Selmode,Sel,MM}}}},St0};
	{delete,MM,#st{selmode=Selmode,sel=Sel}} ->
	    {{view,{highlight_aim,{delete,{Selmode,Sel,MM}}}},St0};
	_Other ->
	    {{view,aim},St0}
    end.

hotkey_select_setup(Cmd,St0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
      {add,_,St} -> {Cmd,St};
      _Other     -> {Cmd,St0}
    end.
