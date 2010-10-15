%%
%%  wings_wm.erl --
%%
%%     Window manager for Wings.
%%
%%  Copyright (c) 2002-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_wm).
-export([toplevel/6,toplevel_title/1,toplevel_title/2,set_knob/3]).
-export([init/0,enter_event_loop/0,dirty/0,dirty_mode/1,pdirty/0,
	 reinit_opengl/0,
	 new/4,delete/1,raise/1,rollup/2,
	 link/2,hide/1,show/1,is_hidden/1,
	 later/1,send/2,psend/2,send_after_redraw/2,
	 set_timer/2,cancel_timer/1,
	 this/0,offset/3,move/2,move/3,resize/2,pos/1,windows/0,is_window/1,
	 window_below/2,resize_windows/2,
	 update_window/2,clear_background/0,
	 callback/1,current_state/1,get_current_state/0,notify/1,
	 local2global/1,local2global/2,global2local/2,local_mouse_state/0,
	 translation_change/0]).

%% Window information.
-export([top_size/0,viewport/0,viewport/1,
	 win_size/0,win_ul/0,win_rect/0,
	 win_size/1,win_ul/1,win_ur/1,win_ll/1,win_lr/1,win_z/1,
	 win_rect/1,win_rollup/1]).

%% Focus management.
-export([grab_focus/0,grab_focus/1,release_focus/0,
	 grabbed_focus_window/0,actual_focus_window/0]).

%% Setting the information bar message.
-export([message/1,message/2,message_right/1]).

%% Menubar management.
-export([menubar/2,get_menubar/1]).

%% Drag & Drop support.
-export([drag/3,drag/4,allow_drag/1]).

%% Window property mangagement.
-export([get_props/1,get_prop/1,get_prop/2,lookup_prop/1,lookup_prop/2,
	 set_prop/2,set_prop/3,erase_prop/1,erase_prop/2,
	 is_prop_defined/2]).

%% Useful sizes
-export([title_height/0,vscroller_width/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,sort/1,keysort/2,reverse/1,foreach/2,member/2]).

-define(Z_LOWEST_DYNAMIC, 10).

-record(win,
	{z,					%Z order.
	 x,y,					%Position.
	 w,h,					%Size.
	 name,					%Name of window.
	 stk,					%Event handler stack.
	 props,					%Window properties.
	 links=[],			  %Windows linked to this one.
	 rollup=false			%Rollup window into Title Bar {controller,Name}
	}).

-record(se,					%Stack entry record.
	{h,					%Handler (fun).
	 msg=[],				%Current message.
	 msg_right=[],				%Right-side message.
	 menubar=[]				%Menubar for this window.
	}).

%%%
%%% Process dictionary usage:
%%%
%%% wm_active		Currently active window (handling current event).
%%% wm_focus		Actual focus window (implicitly or grabbed).
%%% wm_focus_grab	Window name of forced focus window or 'undefined'.
%%% wm_windows		All windows.
%%% wm_dirty		Exists if redraw is needed.
%%% wm_dirty_mode       front|back
%%% wm_top_size         Size of top window.
%%% wm_viewport		Current viewport.
%%% wm_cursor		Default cursor.
%%% {wm_current_state,Displists}
%%%

init() ->
    put(wm_dirty_mode, front),
    put(wm_cursor, arrow),
    {W,H} = TopSize = wings_pref:get_value(window_size),
    put(wm_top_size, TopSize),
    translation_change(),
    put(wm_windows, gb_trees:empty()),
    new(desktop, {0,0,0}, {0,0}, {push,fun desktop_event/1}),
    new(message, {0,0,?Z_LOWEST_DYNAMIC-1}, {0,0},
	{push,fun message_event/1}),
    init_opengl(),
    dirty_mode(back),
    resize_windows(W, H).

desktop_event(_) -> keep.

message(Message) ->
    wings_io:putback_event({wm,{message,get(wm_active),Message}}).

message_right(Right) ->
    wings_io:putback_event({wm,{message_right,get(wm_active),Right}}).

message(Message, Right) ->
    message(Message),
    message_right(Right).

menubar(Name, Menubar) ->
    wings_io:putback_event({wm,{menubar,Name,Menubar}}).

get_menubar(Name) ->
    #win{stk=[#se{menubar=Bar}|_]} = get_window_data(Name),
    Bar.

later(Ev) ->
    wings_io:putback_event({wm,{send_to,this(),Ev}}),
    keep.

send(Name, Ev) ->
    wings_io:putback_event({wm,{send_to,Name,Ev}}),
    keep.

%% Send from another erlang process than the main wings process
psend(Name, Ev) ->
    wings ! {timeout,make_ref(),{event,{wm,{send_to,Name,Ev}}}},
    keep.

send_once(Name, Ev) ->
    wings_io:putback_event_once({wm,{send_to,Name,Ev}}),
    keep.

send_after_redraw(Name, Ev) ->
    wings_io:putback_event({wm,{send_after_redraw,Name,Ev}}).

current_state(St) ->
    DispLists = get_prop(this(), display_lists),
    case put({wm_current_state,DispLists}, St) of
	St -> ok;
	_ ->
	    NewState = {current_state,St},
	    foreach(fun(#win{props=Props,name=Name}) ->
			    case gb_trees:lookup(display_lists, Props) of
				none -> ok;
				{value,DispLists} ->
				    send(Name, NewState);
				{value,_} ->
				    send(Name, {current_state,DispLists,St})
			    end
		    end, gb_trees:values(get(wm_windows)))
    end.

get_current_state() ->
    DispLists = get_prop(this(), display_lists),
    get({wm_current_state,DispLists}).

notify(Note) ->
    Msg = {note,Note},
    foreach(fun(desktop) -> ok;
	       (Name) -> send_once(Name, Msg)
	    end, gb_trees:keys(get(wm_windows))).

dirty_mode(front=Mode) ->
    put(wm_dirty_mode, Mode);
dirty_mode(back=Mode) ->
    case get(wings_os_type) of
	{unix,darwin} -> ok;
	_ ->
	    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	    put(wm_dirty_mode, Mode)
    end.

dirty() ->
    put(wm_dirty, dirty),
    keep.

%% Dirty marker from another erlang process than the main wings process
pdirty() ->
    wings ! {timeout,make_ref(),{event,{wm,dirty}}},
    keep.

clean() ->
    erase(wm_dirty).

callback(Cb) ->
    wings_io:putback_event({wm,{callback,Cb}}).
    
new(Name, {X,Y}, Size, Op) ->
    new(Name, {X,Y,highest}, Size, Op);
new(Name, {X,Y,Z0}, {W,H}, Op) when is_integer(X), is_integer(Y),
				    is_integer(W), is_integer(H) ->
    Z = new_resolve_z(Z0),
    Stk = handle_response(Op, dummy_event, default_stack(Name)),
    Props = gb_trees:from_orddict([{font,wings_pref:get_value(new_system_font)}]),
    Win = #win{x=X,y=Y,z=Z,w=W,h=H,name=Name,stk=Stk,props=Props},
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    dirty().

new_resolve_z(highest) ->
    case highest_z() + 1 of
	Z when Z < ?Z_LOWEST_DYNAMIC -> ?Z_LOWEST_DYNAMIC;
	Z -> Z
    end;
new_resolve_z(Z) when is_integer(Z), Z >= 0-> Z.

delete(Name) ->
    Windows = delete_windows(Name, get(wm_windows)),
    put(wm_windows, Windows),
    case is_window(get(wm_focus_grab)) of
	false -> erase(wm_focus_grab);
	true -> ok
    end,
    case is_window(get(wm_focus)) of
	false -> update_focus(find_active(none));
	true -> ok
    end,
    dirty().

delete_windows(Name, W0) ->
    case gb_trees:lookup(Name, W0) of
	none -> W0;
	{value,#win{links=Links}} ->
	    This = this(),
	    W = gb_trees:delete_any(Name, W0),
	    foldl(fun(L, A) when L =/= This ->
			  delete_windows(L, A);
		     (_, A) -> A
		  end, W, Links)
    end.
raise(Name) ->
    case get_window_data(Name) of
	#win{z=Z} when Z < ?Z_LOWEST_DYNAMIC -> ok;
	#win{z=Z0} ->
	    case highest_z() of
		Z when Z > Z0 ->
		    update_window(Name, [{z,Z+1}]);
		_ -> ok
	    end
    end.

rollup(Action, Name) ->
    case (Action =:= rolldown) =:= win_rollup(Name) of
      true ->
        WinData = gb_trees:values(get(wm_windows)),
        rollup(Action, WinData, Name);
      false ->
        ok
    end.

rollup(Action, [#win{name={closer,Name}}|T], Name) ->
    rollup(Action,T, Name);
rollup(Action, [#win{name={controller,Name}}|T], Name) when Name /= geom ->
    rollup(Action, T, Name);
rollup(rollup, [#win{name=geom}=W|T], geom) ->
    put_window_data(geom, W#win{z=?Z_LOWEST_DYNAMIC, rollup=true}),
    rollup(rollup, T, geom);
rollup(rolldown, [#win{name=geom}=W|T], geom) ->
    put_window_data(geom, W#win{rollup=false}),
    rollup(rolldown, T, geom);
rollup(rollup, [#win{name={object,geom}}|T], geom) ->
    rollup(rollup, T, geom);

rollup(rollup, [#win{z=Z,name={toolbar,Name}=Win}=W|T], Name) when Name /= geom ->
    case Z < -1 of
      true -> ok;
      false -> 
        put_window_data(Win, W#win{z=-1})
    end,
    rollup(rollup, T, Name);
rollup(rolldown, [#win{z=Z,name={toolbar,Name}=Win}=W|T], Name) when Name /= geom ->
    case Z < -1 of
      true -> ok;
      false -> 
        put_window_data(Win, W#win{z=win_z({controller,Name})})
    end,
    rollup(rolldown, T, Name);

rollup(Action, [#win{z=Z,name={toolbar,geom}=Win}=W|T], geom) ->
    case Z < 0 of
     true -> ok;
     false ->
       put_window_data(Win, W#win{z=?Z_LOWEST_DYNAMIC})
    end,
    rollup(Action, T, geom);

rollup(rollup, [#win{name={_,geom}=Win}=W|T], geom) ->
    put_window_data(Win, W#win{z=?Z_LOWEST_DYNAMIC, rollup=true}),
    rollup(rollup, T, geom);
rollup(rolldown, [#win{name={_,geom}}|T], geom) ->
    rollup(rolldown, T, geom);
rollup(rollup, [#win{z=Z,name=Name}=W|T], Name) ->
    put_window_data(Name, W#win{z=-Z, rollup=true}),
    rollup(rollup, T, Name);
rollup(rolldown, [#win{z=Z,name=Name}=W|T], Name) ->
    put_window_data(Name, W#win{z=-Z, rollup=false}),
    rollup(rolldown, T, Name);
rollup(rollup, [#win{name={_,Name}=Win}|T], Name) ->
    hide(Win),
    rollup(rollup, T, Name);
rollup(rolldown, [#win{name={_,Name}=Win}|T], Name) ->
    show(Win),
    rollup(rolldown, T, Name);
rollup(Action, [_|T], Name) ->
    rollup(Action, T, Name);
rollup(_, [], _) ->
    dirty().

highest_z() ->
    highest_z_1(gb_trees:values(get(wm_windows)), 0).

highest_z_1([#win{z=Z}|T], Max) when Z < Max ->
    highest_z_1(T, Max);
highest_z_1([#win{z=Max}|T], _) ->
    highest_z_1(T, Max);
highest_z_1([], Max) -> Max.

link(From, To) ->
    #win{links=Links} = Win = get_window_data(From),
    case member(To, Links) of
	false ->
	    put_window_data(From, Win#win{links=[To|Links]});
	true ->
	    ok
    end.

hide(Name) ->
    case get_window_data(Name) of
	#win{z=Z} when Z < 0 -> ok;
	#win{z=Z}=Win -> put_window_data(Name, Win#win{z=-Z})
    end.

show(Name) ->
    case get_window_data(Name) of
	#win{z=Z} when Z > 0 -> ok;
	#win{z=Z}=Win -> put_window_data(Name, Win#win{z=-Z})
    end.

is_hidden(Name) ->
    case get_window_data(Name) of
	#win{z=Z} when Z < 0 -> true;
	_ -> false
    end.

this() ->
    case get(wm_active) of
	undefined -> none;
	Active -> Active
    end.

windows() ->	    
    gb_trees:keys(get(wm_windows)).

is_window(Name) ->
    gb_trees:is_defined(Name, get(wm_windows)).

offset(Name, Xoffs, Yoffs) ->
    update_window(Name, [{dx,Xoffs},{dy,Yoffs}]).

move(Name, Pos) ->
    update_window(Name, [{pos,Pos}]).

move(Name, Pos, {W,H}) ->
    update_window(Name, [{pos,Pos},{w,W},{h,H}]).

resize(Name, {W,H}) ->
    update_window(Name, [{w,W},{h,H}]).

update_window(Name, Updates) ->
    #win{z=Z0} = Data0 = get_window_data(Name),
    Data = update_window_1(Updates, Data0),
    put_window_data(Name, Data),
    send(Name, resized),
    #win{links=Links} = get_window_data(Name),
    Msg = {window_updated,Name},
    case Data of
	#win{z=Z0} -> ok;
	#win{z=Z} -> update_linked_z(Links, Z-Z0)
    end,
    case Updates of
	[{z,_}] -> ok;
	_ -> foreach(fun(L) -> wings_wm:send(L, Msg) end, Links)
    end,
    dirty().

update_linked_z([N|Ns], Dz) ->
    case lookup_window_data(N) of
	none -> ok;
	#win{z=Hidden} when Hidden < 0 -> ok;
	#win{z=Z}=Data ->
	    put_window_data(N, Data#win{z=Z+Dz}),
	    update_linked_z(Ns, Dz)
    end;
update_linked_z([], _) -> ok.

update_window_1([{dx,Dx}|T], #win{x=X}=Win) ->
    update_window_1(T, Win#win{x=X+Dx});
update_window_1([{dy,Dy}|T], #win{y=Y}=Win) ->
    update_window_1(T, Win#win{y=Y+Dy});
update_window_1([{dw,Dw}|T], #win{w=W}=Win) ->
    update_window_1(T, Win#win{w=W+Dw});
update_window_1([{dh,Dh}|T], #win{h=H}=Win) ->
    update_window_1(T, Win#win{h=H+Dh});
update_window_1([{pos,{X,Y}}|T], Win) ->
    update_window_1(T, Win#win{x=X,y=Y});
update_window_1([{pos,{X,Y,Z}}|T], Win) ->
    update_window_1(T, Win#win{x=X,y=Y,z=Z});
update_window_1([{x,X}|T], Win) ->
    update_window_1(T, Win#win{x=X});
update_window_1([{y,Y}|T], Win) ->
    update_window_1(T, Win#win{y=Y});
update_window_1([{z,Z}|T], Win) ->
    update_window_1(T, Win#win{z=Z});
update_window_1([{w,W}|T], Win) ->
    update_window_1(T, Win#win{w=W});
update_window_1([{h,H}|T], Win) ->
    update_window_1(T, Win#win{h=H});
update_window_1([{rollup,Rollup}|T], Win) ->
    update_window_1(T, Win#win{rollup=Rollup});
update_window_1([], Win) ->
    range_check(Win).

range_check(#win{w=W}=Win) when W < 1 ->
    range_check(Win#win{w=1});
range_check(#win{h=H}=Win) when H < 1 ->
    range_check(Win#win{h=1});
range_check(Win) -> Win.

pos(Name) ->
    #win{x=X,y=Y} = get_window_data(Name),
    {X,Y}.

grab_focus() -> 
    grab_focus(get(wm_active)).
	   
grab_focus(Name) -> 
    case is_window(Name) of
	true -> put(wm_focus_grab, Name);
	false -> erase(wm_focus_grab)
    end.

release_focus() -> 
    erase(wm_focus_grab).

grabbed_focus_window() ->
    get(wm_focus_grab).

actual_focus_window() ->
    get(wm_focus).

top_size() ->
    get(wm_top_size).

set_timer(Time, Event) ->
    Active = get(wm_active),
    wings_io:set_timer(Time, {wm,{send_to,Active,Event}}).

cancel_timer(Ref) ->
    wings_io:cancel_timer(Ref).

viewport() ->
    get(wm_viewport).

viewport(Name) ->
    #win{x=X,y=Y0,w=W,h=H} = get_window_data(Name),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    {X,Y,W,H}.

win_size() ->
    {_,_,W,H} = get(wm_viewport),
    {W,H}.

win_ul() ->
    win_ul(this()).

win_rect() ->
    win_rect(this()).

win_size(Name) ->
    #win{w=W,h=H} = get_window_data(Name),
    {W,H}.

win_ul(Name) ->
    #win{x=X,y=Y} = get_window_data(Name),
    {X,Y}.

win_ur(Name) ->
    #win{x=X,y=Y,w=W} = get_window_data(Name),
    {X+W,Y}.

win_ll(Name) ->
    #win{x=X,y=Y,h=H} = get_window_data(Name),
    {X,Y+H}.

win_lr(Name) ->
    #win{x=X,y=Y,w=W,h=H} = get_window_data(Name),
    {X+W,Y+H}.

win_z(Name) ->
    #win{z=Z} = get_window_data(Name),
    Z.

win_rect(Name) ->
    #win{x=X,y=Y,w=W,h=H} = get_window_data(Name),
    {{X,Y},{W,H}}.

win_rollup({dialog,_}) -> no;
win_rollup(Name) ->
    #win{rollup=Rollup} = get_window_data(Name),
    Rollup.

local2global(#mousebutton{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousebutton{x=X,y=Y};
local2global(#mousemotion{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousemotion{x=X,y=Y};
local2global(Ev) -> Ev.

local2global(X, Y) ->
    {_,TopH} = get(wm_top_size),
    {Xorig,Yorig,_,H} = viewport(),
    {Xorig+X,(TopH-Yorig-H)+Y}.

global2local(X, Y) ->
    {_,TopH} = get(wm_top_size),
    {Xorig,Yorig,_,H} = viewport(),
    {X-Xorig,Y-(TopH-Yorig-H)}.

local_mouse_state() ->
    {B,X0,Y0} = wings_io:get_mouse_state(),
    {X,Y} = global2local(X0, Y0),
    {B,X,Y}.

get_props(Win) ->
    #win{props=Props} = get_window_data(Win),
    gb_trees:to_list(Props).

get_prop(Name) ->
    get_prop(this(), Name).

get_prop(Win, Name) ->
    #win{props=Props} = get_window_data(Win),
    gb_trees:get(Name, Props).
    
lookup_prop(Name) ->
    lookup_prop(this(), Name).

lookup_prop(Win, Name) ->
    #win{props=Props} = get_window_data(Win),
    gb_trees:lookup(Name, Props).

set_prop(Name, Value) ->
    set_prop(this(), Name, Value).

set_prop(Win, Name, Value) ->
    #win{props=Props0} = Data = get_window_data(Win),
    Props = gb_trees:enter(Name, Value, Props0),
    put_window_data(Win, Data#win{props=Props}).

erase_prop(Name) ->
    erase_prop(this(), Name).

erase_prop(Win, Name) ->
    #win{props=Props0} = Data = get_window_data(Win),
    Props = gb_trees:delete_any(Name, Props0),
    put_window_data(Win, Data#win{props=Props}).

is_prop_defined(Win, Name) ->
    #win{props=Props} = get_window_data(Win),
    gb_trees:is_defined(Name, Props).

enter_event_loop() ->
    init_opengl(),
    event_loop().

event_loop() ->
    case get(wm_dirty) of
	undefined -> get_and_dispatch();
	_ -> redraw_all()
    end.

get_and_dispatch() ->
    Event = wings_io:get_event(),
    dispatch_event(Event),
    event_loop().

dispatch_matching(Filter) ->
    Evs0 = wings_io:get_matching_events(Filter),

    %% For historical reasons, because this module
    %% enters all events using wings_io:putback_event_*/1,
    %% we expect the matching events to be in reverse
    %% order. Since wings_io:get_matching_events/1
    %% now returns the events in same order as in the queue,
    %% we must do a reverse here.
    Evs = reverse(Evs0),
    foreach(fun dispatch_event/1, Evs).

dispatch_event(#resize{w=W,h=H}) ->
    ?CHECK_ERROR(),
    %% If the window has become maximized, we don't want
    %% to save the window size, but will keep the previous
    %% size.
    case wings_io:is_maximized() of
	false -> wings_pref:set_value(window_size, {W,H});
	true -> ok
    end,
    put(wm_top_size, {W,H}),
    init_opengl(),
    resize_windows(W, H),
    dirty();
dispatch_event(quit) ->
    foreach(fun(Name) ->
		    send(Name, quit)
	    end, gb_trees:keys(get(wm_windows)));
dispatch_event({wm,WmEvent}) ->
    wm_event(WmEvent);
dispatch_event(Ev = {external,_}) ->
    send(geom,Ev);
dispatch_event(Event) ->
    case find_active(Event) of
	none ->
%	    io:format("~p:~p: Dropped Event ~p~n",[?MODULE,?LINE,Event]),
	    update_focus(none);
	Active ->
	    update_focus(Active),
	    do_dispatch(Active, Event)
    end.

update_focus(none) ->
    case erase(wm_focus) of
	undefined -> ok;
	OldActive ->
	    dirty(),
	    do_dispatch(OldActive, lost_focus)
    end;
update_focus(Active) ->
    case put(wm_focus, Active) of
	undefined ->
	    dirty(),
	    do_dispatch(Active, got_focus);
	Active -> ok;
	OldActive ->
	    dirty(),
	    do_dispatch(OldActive, lost_focus),
	    do_dispatch(Active, got_focus)
    end.

resize_windows(W, H) ->
    Event = #resize{w=W,h=H},

    MsgData0 = get_window_data(message),
    MsgH = ?CHAR_HEIGHT+7,
    MsgY = H-MsgH,
    MsgData1 = MsgData0#win{x=0,y=MsgY,w=W,h=MsgH},
    put_window_data(message, MsgData1),
    MsgData = send_event(MsgData1, Event#resize{w=W}),
    put_window_data(message, MsgData),

    DesktopData0 = get_window_data(desktop),
    DesktopData = DesktopData0#win{x=0,y=0,w=W,h=H-MsgH},
    put_window_data(desktop, DesktopData).

do_dispatch(Active, Ev) ->
    case gb_trees:lookup(Active, get(wm_windows)) of
	none -> 
%	    io:format("~p:~p: Dropped Event ~p~n",[?MODULE,?LINE,Ev]),
	    ok;
	{value,Win0} ->
	    case send_event(Win0, Ev) of
		#win{name=Name,stk=delete} ->
		    delete(Name);
		#win{stk=[]} ->
		    ok;
		Win ->
		    put_window_data(Active, Win)
	    end
    end.

redraw_all() ->
    %% Remove late buffers clear due to problems with ATI cards when AA.
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    Windows = keysort(2, gb_trees:to_list(get(wm_windows))),
    wings_io:foreach(fun({Name,_}) ->
			     dispatch_matching(fun({wm,{send_to,N,_}}) ->
						       N =:= Name;
						  (_) -> false
					       end),
			     do_dispatch(Name, redraw)
		     end, Windows),
    wings_io:swapBuffers(),
    clean(),
    wings_io:set_cursor(get(wm_cursor)),
    event_loop().

clear_background() ->
    Name = this(),

    %% Optimization: Check if we really need to clear the area
    %% occupied by the window. Given a slow OpenGL implementation,
    %% it is a big win.
    case any_window_below(Name) of
	true -> clear_background_1(Name);
	false -> ok
    end.

clear_background_1(Name) ->
    {X,Y,W,H} = viewport(Name),
    gl:scissor(X, Y, W, H),
    gl:enable(?GL_SCISSOR_TEST),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:disable(?GL_SCISSOR_TEST).

any_window_below(Name) ->
    Windows = gb_trees:values(get(wm_windows)),
    #win{x=X,y=Y,z=Z,w=W,h=H} = get_window_data(Name),
    any_window_below_1(Windows, Name, Z, {X,Y,X+W,Y+H}).

any_window_below_1([#win{name=Name}|T], Name, Z, Rect) ->
    any_window_below_1(T, Name, Z, Rect);
any_window_below_1([#win{z=ThisZ}|T], Name, Z, Rect) when Z < ThisZ; ThisZ =< 0 ->
    any_window_below_1(T, Name, Z, Rect);
any_window_below_1([W|T], Name, Z, Rect) ->
    case possible_intersection(W, Rect) of
	true -> true;
	false -> any_window_below_1(T, Name, Z, Rect)
    end;
any_window_below_1([], _, _, _) -> false.

possible_intersection(#win{x=X,y=Y,w=W,h=H}, {Left,Top,Right,Bot}) ->
    if
	Right =< X; Bot =< Y; X+W =< Left; Y+H =< Top -> false;
	true -> true
    end.

reinit_opengl() ->
    wings_io:putback_event({wm,init_opengl}).

init_opengl() ->
    {W,H} = get(wm_top_size),
    wings_io:reset_video_mode_for_gl(W, H),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    wings_io:resize(),
    wings_image:init_opengl(),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1),
    dirty(),
    foreach(fun(Name) ->
		    do_dispatch(Name, init_opengl)
	    end, gb_trees:keys(get(wm_windows))).

send_event(Win, {expose}) ->
    dirty(),
    Win;
send_event(#win{z=Z}=Win, redraw) when Z < 0 ->
    Win;
send_event(#win{name=Name,x=X,y=Y0,w=W,h=H,stk=[Se|_]=Stk0}, Ev0) ->
    OldActive = put(wm_active, Name),
    Ev = translate_event(Ev0, X, Y0),
    {_,TopH} = get(wm_top_size),
    Y = TopH-(Y0+H),
    ViewPort = {X,Y,W,H},
    case put(wm_viewport, ViewPort) of
	ViewPort -> ok;
	_ -> gl:viewport(X, Y, W, H)
    end,
    Stk = handle_event(Se, Ev, Stk0),
    case OldActive of
	undefined -> erase(wm_active);
	_ -> put(wm_active, OldActive)
    end,
    Win = get_window_data(Name),
    Win#win{stk=Stk}.

translate_event(#mousemotion{state=Mask0,x=X,y=Y,mod=Mod0}=M, Ox, Oy) ->
    {Mask,Mod} = translate_bmask(Mask0, Mod0),
    M#mousemotion{state=Mask,x=X-Ox,y=Y-Oy,mod=Mod};
translate_event(#mousebutton{button=B0,x=X,y=Y,mod=Mod0,state=?SDL_PRESSED}=M, Ox, Oy) ->
    {B,Mod} = translate_button(B0, Mod0),
    M#mousebutton{button=B,x=X-Ox,y=Y-Oy,mod=Mod};
translate_event(#mousebutton{button=B0,x=X,y=Y,state=?SDL_RELEASED}=M, Ox, Oy) ->
    case erase({button_up,B0}) of
	undefined ->
	    M#mousebutton{x=X-Ox,y=Y-Oy};
	{B,Mod} ->
	    M#mousebutton{button=B,x=X-Ox,y=Y-Oy,mod=Mod}
    end;
translate_event({drop,{X,Y},DropData}, Ox, Oy) ->
    {drop,{X-Ox,Y-Oy},DropData};
translate_event(Ev, _, _) -> Ev.
    
handle_event(State, Event, Stk) ->
    try
	case State of
	    #se{h=Handler} -> Handler(Event);
	    Fun when is_function(Fun) -> Fun()
	end of
	Res ->
	    handle_response(Res, Event, Stk)
    catch
	throw:{command_error,Error} ->
	    wings_u:message(Error),
	    Stk;
	  exit:normal ->
	    exit(normal);
	  exit:Exit ->
	    [#se{h=CrashHandler}] = pop_all_but_one(Stk),
	    handle_response(CrashHandler({crash,Exit}), Event,
			    default_stack(this()));
	  error:Reason ->
	    [#se{h=CrashHandler}] = pop_all_but_one(Stk),
	    handle_response(CrashHandler({crash,Reason}), Event,
			    default_stack(this()))
    end.

handle_response(Res, Event, Stk0) ->
    case Res of
	keep -> Stk0;
	next -> next_handler(Event, Stk0);
	pop -> pop(Stk0);
	delete -> delete;
	push ->
	    [OldTop|_] = Stk0,
	    [OldTop#se{h=dummy}|Stk0];
	{push,Handler} ->
	    [OldTop|_] = Stk0,
	    [OldTop#se{h=Handler}|Stk0];
	{seq,First,Then} ->
	    Stk = handle_response(First, Event, Stk0),
	    handle_response(Then, Event, Stk);
	{replace,Top} when is_function(Top) ->
	    replace_handler(Top, Stk0);
	{replace,Top,Continue} when is_function(Top), is_function(Continue) ->
	    Stk = replace_handler(Top, Stk0),
	    handle_event(Continue, dummy_event, Stk);
	{pop_handler,_}=PopH -> replace_handler(PopH, Stk0);
	Top when is_function(Top) -> replace_handler(Top, Stk0)
    end.

pop_all_but_one([_]=Stk) -> Stk;
pop_all_but_one(Stk) -> pop_all_but_one(pop(Stk)).
    
pop([_|Stk]) ->
    case Stk of
	[#se{h={pop_handler,Handler0}}|_] ->
	    {replace,Handler} = Handler0(),
	    replace_handler(Handler, Stk);
	_ -> Stk
    end.

replace_handler(Handler, [Top|Stk]) -> [Top#se{h=Handler}|Stk].

next_handler(Event, [_|[Next|_]=Stk]) ->
    handle_event(Next, Event, Stk).

default_stack(Name) ->
    Handler = fun({crash,Crash}) ->
		      StkTrace = erlang:get_stacktrace(),
		      io:format("Window ~p crashed:\n~P\n~P\n",
				[Name,Crash,20,StkTrace,40]),
		      exit({window_crash,Name,Crash,StkTrace});
		 (Other) ->
		      io:format("Window ~p's crash handler got:\n~p\n",
				[Name,Other]),
		      exit({unexpected,Name,Other})
	      end,
    [#se{h=Handler}].

%%%
%%% Handling Wm Events.
%%%

wm_event(dirty) -> 
    dirty();
wm_event({message,Name,Msg}) ->
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{msg=Msg}|_]} -> ok;
	#win{stk=[Top0|Stk]}=Data0 ->
	    Top = Top0#se{msg=Msg},
	    Data = Data0#win{stk=[Top|Stk]},
	    put_window_data(Name, Data),
	    dirty()
    end;
wm_event({message_right,Name,Right0}) ->
    Right = lists:flatten(Right0),
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{msg_right=Right}|_]} -> ok;
	#win{stk=[Top0|Stk]}=Data0 ->
	    Top = Top0#se{msg_right=Right},
	    Data = Data0#win{stk=[Top|Stk]},
	    put_window_data(Name, Data),
	    dirty()
    end;
wm_event({menubar,Name,Menubar}) ->
    case lookup_window_data(Name) of
	none -> ok;
	#win{stk=[#se{menubar=Menubar}|_]} -> ok;
	#win{stk=[Top|Stk]}=Data0 ->
	    Data = Data0#win{stk=[Top#se{menubar=Menubar}|Stk]},
	    put_window_data(Name, Data),
	    dirty()
    end;
wm_event({send_to,Name,Ev}) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({send_after_redraw,Name,Ev}) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({callback,Cb}) ->
    Cb();
wm_event(init_opengl) ->
    init_opengl().
    
%%%
%%% Finding the active window.
%%%

find_active(Ev) ->
    case get(wm_focus_grab) of
 	undefined -> find_active_0(Ev);
 	Focus -> Focus
    end.

find_active_0(Ev) ->
    case Ev of
	#mousebutton{x=X,y=Y} -> ok;
	#mousemotion{x=X,y=Y} -> ok;
	_ -> {_,X,Y} = wings_io:get_mouse_state()
    end,
    window_below(X, Y).

window_below(X, Y) ->
    All = reverse(sort(gb_trees:values(get(wm_windows)))),
    window_below_1(All, X, Y).

window_below_1([#win{x=Wx,y=Wy,w=W,h=H,name=Name,z=Z}|T], X, Y) when Z >= 0 ->
    case {X-Wx,Y-Wy} of
	{Rx,Ry} when 0 =< Rx, Rx < W,0 =< Ry, Ry < H -> Name;
	_ -> window_below_1(T, X, Y)
    end;
window_below_1(_, _, _) -> none.

%%%
%%% Drag and drop support.
%%%
-record(drag,
	{data,					%Drop data.
	 bstate,				%State of mouse buttons.
	 redraw,				%Redraw function.
	 over=none,				%We are over this window.
	 drop_ok=false				%Drop is OK on this window.
	}).

allow_drag(false) -> allow_drag_1(arrow);
allow_drag(true) -> allow_drag_1(pointing_hand).

allow_drag_1(Cursor) ->
    case get(wm_cursor) of
	Cursor -> ok;
	_ ->
	    put(wm_cursor, Cursor),
	    wings_io:set_cursor(Cursor)
    end.

drag(Ev, Rect, DropData) ->
    Redraw = fun() ->
		     gl:pushAttrib(?GL_POLYGON_BIT bor ?GL_LINE_BIT),
		     gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
		     gl:lineStipple(2, 2#0101010101010101),
		     gl:enable(?GL_LINE_STIPPLE),
		     gl:color3b(0, 0, 0),
		     {W,H} = wings_wm:win_size(),
		     gl:rectf(0.5, 0.5, W-1, H-1),
		     gl:popAttrib()
	     end,
    drag(Ev, Rect, Redraw, DropData).

drag(#mousemotion{x=X,y=Y,state=State}, Rect, Redraw, DropData) ->
    drag_1(X, Y, State, Rect, Redraw, DropData);
drag(#mousebutton{x=X,y=Y,button=B}, Rect, Redraw, DropData) ->
    State = 1 bsl (B-1),
    drag_1(X, Y, State, Rect, Redraw, DropData).

drag_1(X0, Y0, State, {W,H}, Redraw, DropData) ->
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    X = X1 - W div 2,
    Y = Y1 - H div 2,
    Drag = #drag{data=DropData,bstate=State,redraw=Redraw},
    Op = {seq,push,get_drag_event(Drag)},
    Name = dragger,
    new(Name, {X,Y,highest}, {W,H}, Op),
    grab_focus(Name),
    dirty(),
    keep.

get_drag_event(Drag) ->
    {replace,fun(Ev) -> drag_event(Ev, Drag) end}.
		     
drag_event(redraw, #drag{redraw=Redraw}) ->
    wings_io:ortho_setup(),
    Redraw(),
    keep;
drag_event(#mousemotion{x=X0,y=Y0}, #drag{over=Over0}=Drag0) ->
    {W,H} = wings_wm:win_size(),
    offset(dragger, X0 - W div 2, Y0 - H div 2),
    {X,Y} = local2global(X0, Y0),
    hide(dragger),
    Over = window_below(X, Y),
    show(dragger),
    case Over of
	Over0 -> keep;
	_ ->
	    Drag = Drag0#drag{over=Over},
	    drag_filter(Drag)
    end;
drag_event(#mousebutton{button=B,state=?SDL_RELEASED},
	   #drag{bstate=State,data=DropData,drop_ok=DropOK}) ->
    if
	((1 bsl (B-1)) band State) =/= 0 ->
	    case DropOK of
		false -> ok;
		true ->
		    {{X,Y},{W,H}} = wings_wm:win_rect(dragger),
		    Ev = {drop,{X + W div 2,Y + H div 2},DropData},
		    wings_io:putback_event(Ev)
	    end,
	    put(wm_cursor, arrow),
	    delete;
	true -> keep
    end;
drag_event(_, _) -> keep.

drag_filter(#drag{over=none}=Drag) ->
    stop_cursor(Drag);
drag_filter(#drag{over=Win,data=DropData}=Drag) ->
    case lookup_prop(Win, drag_filter) of
	{value,Fun} when is_function(Fun) ->
	    case Fun(DropData) of
		yes ->
		    message(""),
		    put(wm_cursor, closed_hand),
		    get_drag_event(Drag#drag{drop_ok=true});
		{yes,Message} ->
		    message(Message),
		    put(wm_cursor, closed_hand),
		    get_drag_event(Drag#drag{drop_ok=true});
		no ->
		    stop_cursor(Drag)
	    end;
	none -> stop_cursor(Drag)
    end.

stop_cursor(Drag) ->
    put(wm_cursor, stop),
    message(""),
    get_drag_event(Drag#drag{drop_ok=false}).

%%%
%%% Utility functions.
%%%

lookup_window_data(Name) ->
    case gb_trees:lookup(Name, get(wm_windows)) of
	none -> none;
	{value,Val} -> Val
    end.

get_window_data(Name) ->
    gb_trees:get(Name, get(wm_windows)).

put_window_data(Name, Data) ->
    put(wm_windows, gb_trees:update(Name, Data, get(wm_windows))).

%%%
%%% Button translation.
%%%

translation_change() ->
    case wings_pref:get_value(num_buttons) of
	3 -> erase(mouse_translation);
	Buttons ->
	    Mode = wings_pref:get_value(camera_mode),
	    put(mouse_translation, {Mode,Buttons})
    end.

translate_bmask(0, Mod) -> {0, Mod};
translate_bmask(Mask, Mod) -> translate_bmask_1(1, Mask, Mod, 0).

translate_bmask_1(4, _, Mod, Acc) -> {Acc,Mod};
translate_bmask_1(B0, Mask, Mod0, Acc) ->
    case 1 bsl (B0-1) of
	Bit when Bit band Mask =:= 0 ->
	    translate_bmask_1(B0+1, Mask, Mod0, Acc);
	_ ->
	    {B,Mod} = translate_button(B0, Mod0),
	    translate_bmask_1(B0+1, Mask, Mod, Acc bor (1 bsl (B-1)))
    end.
    
translate_button(B0, Mod0) ->
    Type = get(mouse_translation),
    case translate_button_1(B0, Type, Mod0) of
	{B0,_}=Res -> Res;
	{_,_}=Res ->
	    put({button_up,B0}, Res),
	    Res
    end.

translate_button_1(1, {blender,2}, Mod) when Mod band ?ALT_BITS =/= 0 ->
    {2,Mod band (bnot ?ALT_BITS)};
translate_button_1(1, {nendo,1}, Mod) when Mod band ?CTRL_BITS =/= 0 ->
    {3,Mod band (bnot ?CTRL_BITS)};
translate_button_1(1, {nendo,1}, Mod) when Mod band ?ALT_BITS =/= 0 ->
    {2,Mod band (bnot ?ALT_BITS)};
translate_button_1(3, {nendo,2}, Mod) when Mod band ?CTRL_BITS =/= 0 ->
    {2,Mod band (bnot ?CTRL_BITS)};
translate_button_1(B, _, Mod) -> {B,Mod}.

%%
%% The message window.
%%

message_event(redraw) ->
    dispatch_matching(fun({wm,{message,_,_}}) -> true;
			 ({wm,{message_right,_,_}}) -> true;
			 (_) -> false
		      end),
    case get(wm_focus) of
	undefined -> keep;
	Active ->
	    #win{stk=[#se{msg=Msg,msg_right=Right}|_]} = get_window_data(Active),
	    message_redraw(Msg, Right)
    end;
message_event({action,_}=Action) ->
    send(geom, Action);
message_event(got_focus) ->
    message(?__(1,"This is the information line")),
    dirty();
message_event(_) -> keep.

message_redraw(Msg, Right) ->
    {W,_} = message_setup(),
    if
	Msg == [] -> ok;
	true -> wings_io:text_at(0, Msg)
    end,
    OsType = get(wings_os_type),
    RMarg0 = case OsType of
		 {unix,darwin} -> 27;
		 _ -> 0
	    end,
    case Right of
	[] -> ok;
	_ ->
	    Cw = wings_text:width(),
	    MsgW = wings_text:width(Msg),
	    RightW = wings_text:width(Right),
	    RMarg = RMarg0 + 2*Cw,
	    if 
		MsgW+RightW < W - RMarg ->
		    Pos = W - RightW - RMarg,
		    wings_io:set_color(e3d_vec:mul(wings_pref:get_value(info_line_bg),0.9)),
		    gl:recti(Pos-Cw, 1-wings_text:height(),
			     Pos+RightW+Cw, 3),
		    wings_io:set_color(wings_pref:get_value(info_line_text)),
		    wings_io:text_at(Pos, Right);
		true -> ok
	    end
    end,
    case OsType of
	{unix,darwin} ->
	    wings_io:draw_icons(fun() ->
					wings_io:draw_icon(W-25, -8, resize)
				end);
	_ -> ok
    end,
    keep.

message_setup() ->
    wings_io:ortho_setup(none),
    {W,H} = win_size(),
    wings_io:gradient_rect(0, 0, W, H, wings_pref:get_value(info_line_bg)),
    wings_io:set_color(wings_pref:get_value(info_line_text)),
    gl:translatef(10, H-5.375, 0),
    {W,H}.

%%%
%%% Toplevel: create a window and a controller window at the same time.
%%% The controller adds a title bar (allowing the window to be moved) and
%%% can optionally add other things such as a scroller.
%%%

toplevel(Name, Title, Pos, Size, Flags, Op) ->
    wings_wm_toplevel:toplevel(Name, Title, Pos, Size, Flags, Op).

toplevel_title(Title) ->
    Win = this(),
    send({controller,Win}, {title,Title}),
    ok.

toplevel_title(Win, Title) ->
    send({controller,Win}, {title,Title}),
    ok.

set_knob(Name, Pos, Proportion) ->
    wings_wm_toplevel:set_knob(Name, Pos, Proportion).

title_height() ->
    wings_wm_toplevel:title_height().

vscroller_width() ->
    wings_wm_toplevel:vscroller_width().
