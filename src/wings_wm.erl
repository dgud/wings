%%
%%  wings_wm.erl --
%%
%%     Window manager for Wings.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_wm).
-export([toplevel/4,toplevel_title/1,toplevel_title/2]).
-export([init/1,enter_event_loop/0,dirty/0,dirty_mode/1,pdirty/0,
	 new/2, new/3, new/4,delete/1,raise/1,
	 link/2,hide/1,show/1,is_hidden/1,
	 later/1,send/2,psend/2, psend/3,send_after_redraw/2,
	 send_once_after_redraw/2,
	 set_timer/2,cancel_timer/1,
	 this/0,this_win/0,
	 offset/3,pos/1,windows/0,is_window/1,
	 is_wxwindow/1,wxwindow/1,
	 window_below/2,geom_below/2,
	 update_window/2,clear_background/0,
	 callback/1,current_state/1,get_current_state/0,notify/1,
	 local2global/1,local2global/2,global2local/2,local_mouse_state/0,
	 local2screen/1, screen2local/1,
	 translation_change/0,is_geom/0]).

%% Window information.
-export([top_size/0,
	 viewport/0, viewport/1,
	 win_size/0, win_size/1,
	 win_rect/0, win_rect/1,
	 win_ul/0, win_ul/1, win_ur/1,win_ll/1,win_lr/1,win_z/1
	]).

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
	 is_prop_defined/2,
	 get_dd/0, get_dd/1, set_dd/2
	]).

%% Useful sizes
-export([title_height/0]).

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
	 links=[],			        %Windows linked to this one.
	 dd=none, 				%Display data cache
	 obj                                    %wx window
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

init(Frame) ->
    put(wm_dirty_mode, front),
    put(wm_cursor, arrow),
    translation_change(),
    put(wm_windows, gb_trees:empty()),
    new(top_frame, Frame, {push, fun wings_frame:forward_event/1}),
    set_dd(top_frame, geom_display_lists), %% Selection mode updates
    StatusBar = wings_status:start(get(top_frame)),
    new(message, StatusBar, {push, fun message_event/1}),

    case wings_pref:get_value(win32_start_maximized) of
	true -> wings_io:maximize();
	_ -> ignore
    end,

    dirty_mode(back).

new(Name, {X,Y}, Size, Op) ->
    new(Name, {X,Y,highest}, Size, Op);
new(Name, {X,Y,Z0}, {W,H}, Op) when is_integer(X), is_integer(Y),
				    is_integer(W), is_integer(H) ->
    Z = new_resolve_z(Z0),
    Stk = handle_response(Op, dummy_event, default_stack(Name)),
    new_props(Name, [{font,system_font}]),
    Win = #win{x=X,y=Y,z=Z,w=W,h=H,name=Name,stk=Stk},
    io:format("~p:~p: ~p ~p~n",[?MODULE, ?LINE, Name, {W,H}]),
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    dirty().

new(Name, Obj) ->
    new(Name, Obj, {push, fun(_Ev) -> keep end}).

new(Name, Obj, Op) when element(1, Obj) =:= wx_ref ->
    Stk = handle_response(Op, dummy_event, default_stack(Name)),
    new_props(Name, [{font,system_font}]),
    Z = highest_z(),
    {W,H} = wxWindow:getClientSize(Obj),
    io:format("~p:~p: ~p ~p~n",[?MODULE, ?LINE, Name, Obj]),
    use_opengl(Obj) andalso init_opengl(),
    Win = #win{x=0,y=0,z=Z,w=W,h=H,name=Name,stk=Stk,obj=Obj},
    put(wm_windows, gb_trees:insert(Name, Win, get(wm_windows))),
    put(Obj, Name),
    dirty().

message(Message) ->
    MsgData0=get_window_data(message),
    put_window_data(message,MsgData0#win{z=highest_z()}),
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
    psend(send_to, Name, Ev).
psend(Type, Name, Ev)
  when Type =:= send_to; Type =:= send_once; Type =:= send_after_redraw ->
    wings ! {wm,{Type,Name,Ev}},
    keep.

send_once(Name, Ev) ->
    wings_io:putback_event_once({wm,{send_to,Name,Ev}}),
    keep.

send_after_redraw(Name, Ev) ->
    wings_io:putback_event({wm,{send_after_redraw,Name,Ev}}),
    keep.

send_once_after_redraw(Name, Ev) ->
    wings_io:putback_event_once({wm,{send_after_redraw,Name,Ev}}),
    keep.

current_state(St) ->
    #win{dd=DispData} = get_window_data(this()),
    case put({wm_current_state,DispData}, St) of
	St -> ok;
	_ ->
	    NewState = {current_state,St},
	    foreach(fun(#win{dd=DD,name=Name}) ->
			    case DD of
				none -> ok;
				DispData -> send(Name, NewState);
				_ -> send(Name, {current_state,DispData,St})
			    end
		    end, gb_trees:values(get(wm_windows)))
    end.

get_current_state() ->
    #win{dd=DispData} = get_window_data(this()),
    get({wm_current_state,DispData}).

notify(Note) ->
    Msg = {note,Note},
    foreach(fun(desktop) -> ok;
	       (Name) -> send_once(Name, Msg)
	    end, gb_trees:keys(get(wm_windows))).

dirty_mode(front=Mode) ->
    put(wm_dirty_mode, Mode);
dirty_mode(back=Mode) ->
    case os:type() of
	{unix,darwin} ->
	    ok;
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
	false -> update_focus(find_active());
	true -> ok
    end,
    dirty().

delete_windows(Name, W0) ->
    case gb_trees:lookup(Name, W0) of
	none when element(1, Name) =:= wx_ref ->
	    case wx2win(Name, W0) of
		false -> W0;
		#win{name=WName} -> delete_windows(WName, W0)
	    end;
	none ->
	    W0;
	{value,#win{links=Links}} ->
	    delete_props(Name),
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
	#win{obj=Obj} when Obj =/= undefined -> show_wx(Obj);
	#win{z=Z} when Z > 0 -> ok;
	#win{z=Z}=Win -> put_window_data(Name, Win#win{z=-Z})
    end.

show_wx(Frame) ->
    case wxFrame:isIconized(Frame) of
	true -> wxFrame:iconize(Frame, [{iconize, false}]);
	false -> ok
    end,
    wxFrame:raise(Frame).

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

this_win() ->
    #win{obj=Obj} = get_window_data(this()),
    Obj.

is_geom() ->
    case this() of
      geom -> true;
      {geom,_} -> true;
      _ -> false
    end.

windows() ->	    
    gb_trees:keys(get(wm_windows)).

is_window(Name) ->
    gb_trees:is_defined(Name, get(wm_windows)).

is_wxwindow(Name) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	true ->
	    #win{obj=Obj} = get_window_data(Name),
	    Obj =/= undefined;
	false ->
	    false
    end.

wxwindow(Name) ->
    #win{obj=Obj} = get_window_data(Name),
    Obj.

%% wx2win(Obj) ->
%%     wx2win(Obj, get(wm_windows)).

wx2win(Obj, W0) ->
    All = gb_trees:values(W0),
    lists:keyfind(Obj, #win.obj, All).

offset(Name, Xoffs, Yoffs) ->
    update_window(Name, [{dx,Xoffs},{dy,Yoffs}]).

update_window(Name, Updates) ->
    #win{z=Z0} = Data0 = get_window_data(Name),
    Data = update_window_1(Updates, Data0),
    put_window_data(Name, Data),
    send(Name, resized),
    #win{links=Links} = get_window_data(Name),
    Msg = {window_updated,Name},
    case Data of
	#win{z=Z0} -> ok;
	#win{z=Z}  -> update_linked_z(Links, Z-Z0)
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
    wxWindow:getClientSize(?GET(top_frame)).

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

win_size(Name) ->
    #win{w=W,h=H} = get_window_data(Name),
    {W,H}.

win_rect() ->
    win_rect(this()).

win_rect(Name) ->
    #win{x=X,y=Y,w=W,h=H} = get_window_data(Name),
    {{X,Y},{W,H}}.

win_ul() ->
    win_ul(this()).

win_ul(Name) ->
    case get_window_data(Name) of
	#win{x=X,y=Y,obj=undefined} ->
	    {X,Y};
	#win{obj=Wx} ->
	    wxWindow:screenToClient(?GET(top_frame), wxWindow:getScreenPosition(Wx))
    end.

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

local2screen(Win, {_,_}=Pos) ->
    wxWindow:clientToScreen(Win, Pos).

local2screen(#mousebutton{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2screen(this_win(), {X0,Y0}),
    Ev#mousebutton{x=X,y=Y};
local2screen(#mousemotion{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2screen(this_win(), {X0,Y0}),
    Ev#mousemotion{x=X,y=Y};
local2screen(Pos) ->
    local2screen(this_win(), Pos).

screen2local(Pos) ->
    wxWindow:screenToClient(this_win(), Pos).

local2global(#mousebutton{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousebutton{x=X,y=Y};
local2global(#mousemotion{x=X0,y=Y0}=Ev) ->
    {X,Y} = local2global(X0, Y0),
    Ev#mousemotion{x=X,y=Y};
local2global(Ev) -> Ev.

local2global(X, Y) ->
    %% {_,TopH} = get(wm_top_size),
    %% {Xorig,Yorig,_,H} = viewport(),
    %% {Xorig+X,(TopH-Yorig-H)+Y}.
    {X,Y}.

global2local(X, Y) ->
    %% {_,TopH} = get(wm_top_size),
    %% {Xorig,Yorig,_,H} = viewport(),
    %% {X-Xorig,Y-(TopH-Yorig-H)}.
    {X,Y}.

local_mouse_state() ->
    {B,X0,Y0} = wings_io:get_mouse_state(),
    {X,Y} = screen2local({X0, Y0}),
    {B,X,Y}.

new_props(Win, Props0) ->
    Props = gb_trees:from_orddict(Props0),
    ?SET({Win, props}, Props).

delete_props(Win) ->
    ?DELETE({Win, props}).

get_props(Win) ->
    Props = ?GET({Win, props}),
    gb_trees:to_list(Props).

get_prop(Name) ->
    get_prop(this(), Name).

get_prop(Win, Name) ->
    Props = ?GET({Win, props}),
    gb_trees:get(Name, Props).

lookup_prop(Name) ->
    lookup_prop(this(), Name).

lookup_prop(Win, Name) ->
    Props = ?GET({Win, props}),
    gb_trees:lookup(Name, Props).

set_prop(Name, Value) ->
    set_prop(this(), Name, Value).

set_prop(Win, Name, Value) ->
    Props0 = ?GET({Win, props}),
    Props = gb_trees:enter(Name, Value, Props0),
    ?SET({Win, props}, Props).

erase_prop(Name) ->
    erase_prop(this(), Name).

erase_prop(Win, Name) ->
    Props0 = ?GET({Win, props}),
    Props = gb_trees:delete_any(Name, Props0),
    ?SET({Win, props}, Props).

is_prop_defined(Win, Name) ->
    Props = ?GET({Win, props}),
    gb_trees:is_defined(Name, Props).

get_dd() ->
    get_dd(this()).
get_dd(Win) ->
    #win{dd=DD} = get_window_data(Win),
    DD.

set_dd(Win, Value) ->
    Data = get_window_data(Win),
    put_window_data(Win, Data#win{dd=Value}).

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

dispatch_event(#mousemotion{which=Obj}=Event) ->
    do_dispatch(get(Obj), Event);
dispatch_event(#mousebutton{which=Obj}=Event) ->
    do_dispatch(get(Obj), Event);
dispatch_event(#keyboard{which=Obj}=Event) ->
    do_dispatch(get(Obj), Event);

dispatch_event(#wx{obj=Obj, event=#wxSize{size={W,H}}}) ->
    ?CHECK_ERROR(),
    #win{name=Name} = Geom0 = get_window_data(Obj),
    Geom = Geom0#win{x=0,y=0,w=W,h=H},
    put_window_data(Name, Geom),
    dirty();
dispatch_event(quit) ->
    foreach(fun(Name) ->
		    send(Name, quit)
	    end, gb_trees:keys(get(wm_windows)));
dispatch_event({wm,WmEvent}) ->
    wm_event(WmEvent);
dispatch_event(Ev = {external,_}) ->
    send(geom,Ev);
dispatch_event({menubar,Ev}) ->
    menubar_event(geom, Ev);
dispatch_event(#wx{event=#wxActivate{active=Active}}) ->
    Active andalso dirty();
dispatch_event(#wx{event=#wxPaint{}}) ->
    dirty();
dispatch_event(#wx{obj=Obj}=Event) ->
    do_dispatch(get(Obj), Event);

dispatch_event(Event) ->
    case find_active() of
	Active when Event#keyboard.which =:= menubar ->
	    menubar_event(Active, Event);
	none ->
	    io:format("~p:~p: Dropped Event ~p~n",[?MODULE,?LINE,Event]),
	    update_focus(none);
	Active ->
	    update_focus(Active),
	    do_dispatch(Active, Event)
    end.

menubar_event(Window, Event) ->
    case get(mb_focus) of
	undefined -> 
	    menubar_focus(Window),
	    menubar_event(Window, Event);
	ignore -> 
	    send(geom, Event);
	Active ->
	    send(Active,Event)
    end.

menubar_focus(Window) ->
    {Send, Focus} = case Window of
			{_, geom} -> {true, geom};
			{_, Geom={geom,_}} -> {true, Geom};
			geom -> {true, geom};
			Geom={geom,_} -> {true, Geom};
			dialog_blanket -> {false, dialog_blanket};
			_ -> {false, ignore}
		    end,
    %% io:format("Set mb focus ~p ~p~n", [Window, Focus]),
    Send andalso send(top_frame, {got_focus, Focus, get_props(Focus)}),
    put(mb_focus, Focus).

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
	    menubar_focus(Active),
	    do_dispatch(Active, got_focus);
	Active -> ok;
	OldActive ->
	    dirty(),
	    do_dispatch(OldActive, lost_focus),
	    menubar_focus(Active),
	    do_dispatch(Active, got_focus)
    end.

do_dispatch(Active, Ev) ->
    case gb_trees:lookup(Active, get(wm_windows)) of
	none ->
	    io:format("~p:~p: Dropped Event ~p~n",[?MODULE,?LINE,Ev]),
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
    Windows = keysort(2, gb_trees:to_list(get(wm_windows))),
    wings_io:foreach(fun redraw_win/1, Windows),
    calc_stats(),
    clean(),
    wings_io:set_cursor(get(wm_cursor)),
    event_loop().

redraw_win({Name, #win{w=W,h=H,obj=Obj}}) ->
    DoSwap = case use_opengl(Obj) of
		 true ->
		     wxGLCanvas:setCurrent(Obj),
		     gl:viewport(0,0,W,H),
		     gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
		     true;
		 false ->
		     false
	     end,
    dispatch_matching(fun({wm,{send_to,N,_Ev}}) ->
			      N =:= Name;
			 (_) -> false
		      end),
    do_dispatch(Name, redraw),
    DoSwap andalso wxGLCanvas:swapBuffers(Obj).

use_opengl(undefined) -> false;
use_opengl(Obj) -> wx:getObjectType(Obj) =:= wxGLCanvas.

-ifndef(DEBUG).
calc_stats() -> ok.
-else.
calc_stats() ->
    Curr = erlang:monotonic_time(),
    case put(time_stamp, Curr) of
	undefined ->
	    put(time_stat, {0, 0, 0, 999999999999999, -1}),
	    ok;
	Prev ->
	    calc_stats(erlang:convert_time_unit(Curr-Prev, native, micro_seconds))
    end.

calc_stats(Diff) when Diff < 80000 ->
    {N, Sum, SumSq, Min, Max} = get(time_stat),
    case N rem 100 of
	0 when N > 50 ->
	    Mean = Sum / N,
	    StdDev = math:sqrt((SumSq - (Sum*Sum/N))/(N - 1)),
	    put(time_stat, {N+1, Sum+Diff, SumSq+Diff*Diff, 999999999999, -1}),
	    io:format("~w ~.1f ~.3f ~w ~w~n", [N, Mean, StdDev, Min, Max]);
	_ ->
	    put(time_stat, {N+1, Sum+Diff, SumSq+Diff*Diff, min(Diff, Min), max(Diff, Max)}),
	    ok
    end;
calc_stats(_) -> %% Do not add user wait times
    ok.
-endif.

clear_background() ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT).

init_opengl() -> % (_Name, _Canvas) ->
    {W,H} = top_size(),
    wings_io:reset_video_mode_for_gl(W, H),
    %% wxGLCanvas:setCurrent(Canvas),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:pixelStorei(?GL_UNPACK_ALIGNMENT, 1),
    wings_io:resize(),
    wings_image:init_opengl(),
    {R,G,B} = wings_pref:get_value(background_color),
    gl:clearColor(R, G, B, 1.0),
    dirty(),
    foreach(fun(Name) ->
		    do_dispatch(Name, init_opengl)
	    end, gb_trees:keys(get(wm_windows))).

send_event(#win{z=Z}=Win, redraw) when Z < 0 ->
    Win;
send_event(#win{name=Name,w=W,h=H,stk=[Se|_]=Stk0,obj=Obj}, Ev0)
  when Obj =/= undefined ->
    ViewPort = {0,0,W,H},
    case use_opengl(Obj) of
	true ->
	    case put(wm_viewport, ViewPort) of
		ViewPort -> ok;
		_ -> gl:viewport(0, 0, W, H)
	    end;
	false -> ignore
    end,
    OldActive = put(wm_active, Name),
    Ev = translate_event(Ev0, 0, 0),
    Stk = handle_event(Se, Ev, Stk0),
    case OldActive of
	undefined -> erase(wm_active);
	_ -> put(wm_active, OldActive)
    end,
    Win = get_window_data(Name),
    Win#win{stk=Stk};
send_event(#win{name=Name,stk=[Se|_]=Stk0}, Ev) ->
    OldActive = put(wm_active, Name),
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
	    Handler when is_function(Handler) -> Handler()
	end of
	Res ->
	    case State of #se{h=H} -> H;
		H when is_function(H) -> H
	    end,
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
	defer -> defer_to_next_handler(Event, Stk0);
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

defer_to_next_handler(Event, [Top|[Next|_]=Stk]) ->
    [Top|handle_event(Next, Event, Stk)].

default_stack(Name) ->
    Handler = fun({crash,Crash}) ->
		      wings_u:win_crash(Crash);
		 (Other) ->
		      io:format("Window ~p's crash handler got:\n~p\n",
				[Name,Other]),
		      delete
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
    %%io:format("~p:~p: ~p ~P~n",[?MODULE,?LINE,Name,Ev,10]),
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({send_after_redraw,Name,Ev}) ->
    case gb_trees:is_defined(Name, get(wm_windows)) of
	false -> ok;
	true -> do_dispatch(Name, Ev)
    end;
wm_event({send_once, Name, Ev}) ->
    wings_io:putback_event_once({wm,{send_to,Name,Ev}}),
    ok;
wm_event({callback,Cb}) ->
    Cb();
wm_event(init_opengl) ->
    init_opengl().
    
%%%
%%% Finding the active window.
%%%

find_active() ->
    case get(wm_focus_grab) of
 	undefined -> window_below(wx_misc:getMousePosition());
 	Focus -> Focus
    end.

window_below(X,Y) ->
    window_below({X,Y}).
window_below(Pos) ->
    Win0 = wx_misc:findWindowAtPoint(Pos),
    case wx:is_null(Win0) of
	true -> none;
	_ ->
	    All = windows(),
	    find_window(All, All, Win0)
    end.

find_window([#win{name=Name, obj=Obj}|Wins], All, Win) ->
    case wings_util:wxequal(Obj, Win) of
	true -> Name;
	false -> find_window(Wins, All, Win)
    end;
find_window([], All, Win) ->
    P = wxWindow:getParent(Win),
    case wx:is_null(P) of
	true -> none;
	false -> find_window(All, All, P)
    end.

geom_below(X, Y) ->
    Windows = windows(),
    Geoms = [geom|[Geom|| {geom,_}=Geom <- Windows]],
    {Window,_} = foldl(fun(Name, {_,Z0}=Acc) ->
        #win{x=Wx,y=Wy,w=W,h=H,z=Z} = get_window_data(Name),
        case {X-Wx,Y-Wy} of
            {Rx,Ry} when 0 =< Rx, Rx < W,0 =< Ry, Ry < H, Z >= Z0 -> {Name,Z};
            _ -> Acc
        end
    end, {none,0}, Geoms),
    Window.


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

get_window_data({wx_ref,_,_,_} = Obj) ->
    gb_trees:get(get(Obj), get(wm_windows));
get_window_data(Name) ->
    gb_trees:get(Name, get(wm_windows)).

put_window_data(Name, #win{name=N1}=Data) ->
    Name = N1, %% Assert
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
	    #win{name=Name, stk=[#se{msg=Msg,msg_right=Right}|_]}
		= get_window_data(Active),
	    wings_status:message(Name, Msg, Right),
	    keep
    end;
message_event({action,_}=Action) ->
    send(geom, Action);
message_event(_) -> keep.

%%%
%%% Toplevel: create a window and a controller window at the same time.
%%% The controller adds a title bar (allowing the window to be moved) and
%%% can optionally add other things such as a scroller.
%%%

toplevel(Name, Window, Props, Op) ->
    new(Name, Window, Op),
    wings_frame:register_win(Window),
    Do = fun({display_data, V}) -> set_dd(Name, V);
	    ({K, V}) -> wings_wm:set_prop(Name, K, V)
	 end,
    [Do(KV) || KV <- Props],
    ok.


toplevel_title(Title) ->
    toplevel_title(this(), Title).

toplevel_title(Win, Title) ->
    wings_frame:set_title(Win, Title),
    ok.

title_height() ->
    wings_frame:title_height().
