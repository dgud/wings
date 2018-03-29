%%
%%  wings_tweak.erl --
%%
%%     A rewrite of wpc_tweak.erl to add Tweak into the Wings core.
%%
%%  Copyright (c) 2009-2011 Richard Jones
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_tweak).

-export([init/0,command/2, help_msg/0]).
-export([tweak_event/2,menu/2,tweak_keys_info/0,tweak_disabled_msg/0,
	 tweak_info_line/0,tweak_magnet_help/0,statusbar/0]).

-export([toggle_draw/1,point_center/3]).
-export([update_dlist/3,draw/4,get_data/3]).

-export([tweak_keys/0, menu/0, tweak_magnet_menu/0, constraints_menu/0]).  %% For wings_tweak_win only

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).

-define(L_ALT, 307).
-define(R_ALT, 308).
-define(MAG_INFO, mag_vertex_info).
-define(MAX_WEIGHT_VALUE, 240.0).

-include("wings.hrl").
-include("e3d.hrl").

%%%
%%% Main Tweak Records
%%%

-record(tweak,
	{mode,      % current tweak tool
	 magnet,    % true|false
	 mag_type,  % magnet type: Type
	 mag_rad,   % magnet influence radius
	 pnt_route=shortest,  % route strategy for paint (shortest, surface)
	 id,        % {Id,Elem} mouse was over when tweak began
	 sym,       % current magnet radius adjustment hotkey
	 ox,oy,     % original X,Y
	 cx,cy,     % current X,Y
	 clk=none,  % click selection/deselection
	 st}).      % wings st record (working)

-record(drag,
	{vs,
	 pos0,      % Original position.
	 pos,       % Current position.
	 pst=none,  % Any data that a specific tweak tool needs stored temporarily
	 mag,       % mag record
	 mm}).      % original|mirror

-record(mag,
	{orig,      % Orig centre of the selection being moved
	 paint=none,% paint record
	 vs,        % [{V,Pos,Distance,Influence}]
	 vtab=[]}). % [{V,Pos}] (latest)


%%%
%%% Set Default Tweak prefs
%%%

init() ->
    set_default_tweak_keys(),
    TweakMagnet = {true,dome,1.0},  %{magnet on, magnet type, magnet radius}
    TweakPaint = shortest,  % distance route
    wings_pref:set_default(tweak_active,false),
    wings_pref:set_default(tweak_magnet, TweakMagnet),
    wings_pref:set_default(tweak_paint, TweakPaint),
    wings_pref:set_default(tweak_xyz, [false,false,false]),
    wings_pref:set_default(tweak_axis, screen),
    wings_pref:set_default(tweak_point, none),
    wings_pref:set_default(tweak_click_speed, 300000),
    wings_pref:set_default(tweak_mag_adj_sensitivity, 0.01),
    wings_pref:set_default(tweak_magnet_color, {0.0, 0.0, 1.0, 0.06}),
    wings_pref:set_default(tweak_geo_point, none),
    wings_pref:set_default(tweak_radial, false),
    wings_pref:set_default(tweak_vector_size, 0.5),
    wings_pref:set_default(tweak_vector_width, 2.0),
    wings_pref:set_default(tweak_vector_color, {1.0,0.5,0.0}),
    wings_pref:set_default(tweak_speed, 0.5), %% control vs speed setting
    wings_pref:set_default(tweak_axis_toggle, []),
    wings_pref:set_default(tweak_magnet_influence, true),

    %% Delete Old Prefs
    wings_pref:delete_value(tweak_draw),
    wings_pref:delete_value(tweak_help),
    wings_pref:delete_value(tweak_magnet_colour),
    wings_pref:delete_value(tweak_sb_clears_constraints),
    wings_pref:delete_value(tweak_single_click),
    true.

%%%
%%% Default Tweak Keys
%%%

set_default_tweak_keys() ->
    Cam = wings_pref:get_value(camera_mode),
    %% Set Default tweak keys according to the camera mode
    case wings_pref:get_value(tweak_prefs) of
	{Cam,Prefs0} ->
	    case check_tweak_prefs(Prefs0) of
		[] -> set_tweak_keys(Cam);
		Prefs ->
		    wings_pref:set_value(tweak_prefs,{Cam,Prefs})
	    end;
	_ ->
	    set_tweak_keys(Cam)
    end.

set_tweak_keys(Cam) ->
    %% Set Default tweak keys according to the camera mode
    TweakKeys = default_tweak_keys(),
    wings_pref:set_value(tweak_prefs,{Cam,TweakKeys}),
    wings_wm:dirty(),
    wings_wm:send({tweak,tweak_palette}, update_palette),
    TweakKeys.

default_tweak_keys() ->
    %% This is the format {{MouseButton, {Crtl, Shift, Alt}}, TweakMode}
    F = false,
    D = [{{1,{F,F,F}}, move}],
    orddict:from_list(D).

tweak_keys() ->
    Cam = wings_pref:get_value(camera_mode),
    case wings_pref:get_value(tweak_prefs) of
	{Cam,Keys} -> Keys;
	_ -> set_tweak_keys(Cam)
    end.

check_tweak_prefs([{{N,{A,B,C}},Mode}=P|Prefs]) ->
    Check1 = is_integer(N),
    Check2 = is_atom(A) andalso is_atom(B) andalso is_atom(C),
    Check3 = lists:member(Mode, [move,move_normal,scale,scale_uniform,relax,slide,paint,erase]),
    case Check1 andalso Check2 andalso Check3 of
	true -> [P|check_tweak_prefs(Prefs)];
	false -> check_tweak_prefs(Prefs)
    end;
check_tweak_prefs([]) -> [].

%%%
%%% Check for Tweak Events
%%%

tweak_event(Ev, St) ->
    case wings_pref:get_value(tweak_active) of
	true -> tweak_event_handler(Ev, St);
	false -> next
    end.

%%% Mouse Buttons
tweak_event_handler(#mousebutton{button=B,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, St)
  when B < 4 ->
    Cam = wings_pref:get_value(camera_mode),
    case wings_pref:get_value(tweak_prefs) of
	{Cam,TweakKeys} ->
	    Ctrl = Mod band ?CTRL_BITS =/= 0,
	    Shift = Mod band ?SHIFT_BITS =/= 0,
	    Alt = Mod band ?ALT_BITS =/= 0,
	    case orddict:find({B,{Ctrl,Shift,Alt}}, TweakKeys) of
		{ok, Mode} ->
		    {Mag,MagType,MagR} = wings_pref:get_value(tweak_magnet),
		    PaintRoute = wings_pref:get_value(tweak_paint),
		    T = #tweak{mode=Mode,ox=X,oy=Y,magnet=Mag,mag_type=MagType,
			       mag_rad=MagR,pnt_route=PaintRoute,st=St},
		    handle_tweak_event_1(T);
		error -> next
	    end;
	_ ->
	    set_tweak_keys(Cam),
	    next
    end;

%%% Keyboard hits
tweak_event_handler(#keyboard{sym=Sym,mod=Mod,state=?SDL_PRESSED}=Ev,St) ->
    {Mag,MagType,MagR} = wings_pref:get_value(tweak_magnet),
    PaintRoute = wings_pref:get_value(tweak_paint),
    case wings_hotkey:event(Ev,St#st{sel=[]}) of
	{tweak,{tweak_magnet,mag_adjust}} when Mag ->
	    T = #tweak{magnet=Mag,mag_type=MagType,mag_rad=MagR,pnt_route=PaintRoute,sym=Sym,st=St},
	    magnet_adjust(T);
	{tweak,{axis_constraint,Axis}} ->
	    Pressed = wings_pref:get_value(tweak_axis_toggle),
	    case lists:keymember(Sym, 1, Pressed) of
		true -> keep;
		false ->
		    ReturnAxis = toggle_data(Axis),
		    wings_pref:set_value(tweak_axis_toggle,[{Sym,ReturnAxis,os:timestamp()}|Pressed]),
		    wings_io:change_event_handler(?SDL_KEYUP, true),
		    toggle_axis(Axis),
		    wings_wm:dirty(),
		    wings_wm:send({tweak,axis_constraint}, update_palette),
		    keep
	    end;
	next when Mag ->
	    case magnet_has_hotkey() of
		true -> next;
		false ->
		    case is_altkey_magnet_event(Sym,Mod) of
			true ->
			    T = #tweak{magnet=Mag,mag_type=MagType,mag_rad=MagR,pnt_route=PaintRoute,sym=Sym,st=St},
			    magnet_adjust(T);
			false -> next
		    end
	    end;
	_ -> next
    end;
tweak_event_handler(#keyboard{sym=Sym,state=?SDL_RELEASED},_St) ->
    Pressed0 = wings_pref:get_value(tweak_axis_toggle),
    case lists:keytake(Sym,1,Pressed0) of
	{value,{Sym,Axis,PressTime},Pressed} ->
	    ClickSpeed = wings_pref:get_value(tweak_click_speed),
	    case timer:now_diff(os:timestamp(), PressTime) > ClickSpeed of
		true ->
		    toggle_axis(Axis),
		    wings_wm:dirty(),
		    wings_wm:send({tweak,axis_constraint}, update_palette);
		false -> ok
	    end,
	    wings_pref:set_value(tweak_axis_toggle,Pressed),
	    case Pressed of
		[] ->
		    wings_io:change_event_handler(?SDL_KEYUP, false),
		    keep;
		_ -> keep
	    end;
	false -> keep
    end;

tweak_event_handler(lost_focus,_) ->
    wings_pref:set_value(tweak_axis_toggle,[]),
    wings_io:change_event_handler(?SDL_KEYUP, false),
    next;
tweak_event_handler(_,_) ->
    next.

%%%
%%% Start Tweak
%%%

handle_tweak_event_1(#tweak{ox=X,oy=Y,mode=Mode,st=St0}=T) when Mode=:=paint; Mode=:=erase ->
    case wings_pick:do_pick(X,Y,St0#st{sel=[]}) of
	{_, What, St} ->
	    Action = if Mode=:=paint -> add; true -> delete end,
	    tweak_handler_setup(Action, What, St, T);
	_ ->
	    next
    end;
handle_tweak_event_1(#tweak{ox=X,oy=Y, st=#st{sel=Sel}=St0}=T) ->
    case wings_pick:do_pick(X,Y,St0) of
	{add, What, St} when Sel =:= [] ->
	    from_element_point(X,Y,St0),
	    tweak_handler_setup(add, What, St, T);
	{add, What, St} ->
	    from_element_point(X,Y,St),
	    tweak_handler_setup(add, What, St, T);
	{delete, What, _} ->
	    from_element_point(X,Y,St0),
	    tweak_handler_setup(delete, What, St0, T);
	none ->
	    next
    end.

tweak_handler_setup(Action, {Id,Elem,_}=What, St, T0) ->
    IdElem = {Id,[Elem]},
    T = T0#tweak{id={Action,IdElem},cx=0,cy=0},
    {seq,push,initiate_tweak_handler(What, St, T)}.


%%%
%%% Initial Event Handler
%%%

%% Basically we want to wait for a mouse button release which sygnifies
%% a Pick Event. If anything else happens go into the actual tweak handler.
initiate_tweak_handler(What, St, T) ->
    {replace,fun(Ev) ->
		     handle_initial_event(Ev, What, St, T) end}.

handle_initial_event(redraw, What, St, T) ->
    wings_draw:refresh_dlists(St),
    wings:redraw(St),
    initiate_tweak_handler(What, St, T);
handle_initial_event(#mousebutton{button=1,state=?SDL_RELEASED}, What, #st{shapes=Shs,sel=Sel0}=St0,
		     #tweak{id={Action,{Id,[Elem]}},clk=none,ox=X,oy=Y}=T) ->
    case wings_io:is_grabbed() of
	false -> ok;
	true -> wings_io:ungrab(X,Y)
    end,
    St = case Action of
	     add -> St0;
	     delete ->
		 We = gb_trees:get(Id, Shs),
		 case orddict:find(Id, Sel0) of
		     _ when ?IS_LIGHT(We) ->
			 Sel = orddict:erase(Id, Sel0),
			 St0#st{sel=Sel};
		     {ok,Sel1} ->
			 case gb_sets:size(Sel1) of
			     1 ->
				 Sel = orddict:erase(Id, Sel0),
				 St0#st{sel=Sel};
			     _ ->
				 Sel2 = gb_sets:delete(Elem, Sel1),
				 Sel = orddict:store(Id, Sel2, Sel0),
				 St0#st{sel=Sel}
			 end;
		     error ->
			 Sel = orddict:store(gb_sets:singleton(Elem), Id, Sel0),
			 St0#st{sel=Sel}
		 end
	 end,
    wings_wm:send({object,wings_wm:this()}, {current_state,St}),
    wings_wm:dirty(),
    initiate_tweak_handler(What, St, T#tweak{clk={one,os:timestamp()}});
handle_initial_event(#mousebutton{button=1,x=X0,y=Y0,state=?SDL_PRESSED}=Ev,
		     _What, St, #tweak{clk={one,Clk},ox=X,oy=Y,st=TweakSt}) ->
    case timer:now_diff(os:timestamp(),Clk) < wings_pref:get_value(tweak_click_speed) of
	true ->
	    wings_pick:paint_pick(X0, Y0, TweakSt);
	false ->
	    Window = wings_wm:this(),
	    wings_wm:send_after_redraw(Window, Ev#mousebutton{x=X,y=Y}),
	    wings_wm:later({new_state,St}),
	    pop
    end;
handle_initial_event({new_state,St}, _, _, _) ->
    %% this is the exiting event from wings_pick after paint_pick/3
    wings_wm:later({new_state,St}),
    pop;
handle_initial_event(#mousemotion{}, _, #st{selmode=body}, #tweak{mode=Mode})
    			when Mode=:=paint; Mode=:=erase ->
    keep;
handle_initial_event(#mousemotion{x=X,y=Y}=Ev, What, St,
		     #tweak{mode=Mode,ox=OX,oy=OY,cx=CX,cy=CY,clk=Clk}=T) ->
    DX = X-OX, %since last move X
    DY = Y-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y
    Total = math:sqrt(DxOrg * DxOrg + DyOrg * DyOrg),
    case lists:member(Mode,[paint, erase]) of
	false -> wings_io:warp(OX,OY);
	true -> ignore
    end,
    case Total > 3 of
	true when Clk =:= none ->
	    enter_tweak_handler(Ev, What, St, T);
	true ->
	    wings_wm:later({new_state,St}),
	    pop;
	false ->
	    initiate_tweak_handler(What, St, T#tweak{cx=DxOrg,cy=DyOrg})
    end;
handle_initial_event(Ev, _, St, #tweak{clk={one,_}}) ->
    wings_wm:send_after_redraw(geom,Ev),
    wings_wm:later({new_state,St}),
    pop;
handle_initial_event(#keyboard{sym=Sym,mod=Mod}=Ev, What, St, #tweak{ox=X,oy=Y}=T)
  when Mod band (?ALT_BITS bor ?SHIFT_BITS bor ?CTRL_BITS) =:= 0 ->
    %% Activate Tweak Camera
    case wings_camera:tweak_camera_event(Sym, X, Y, St) of
	next ->
	    enter_tweak_handler(Ev, What, St, T);
	Other ->
	    case wings_io:is_grabbed() of
		false -> wings_io:grab();
		true -> ok
	    end,
	    Other
    end;
handle_initial_event(Ev, What, St, T) ->
    enter_tweak_handler(Ev, What, St, T).

enter_tweak_handler(Ev, What, St, #tweak{id={Action,_},mode=Mode,st=#st{sel=Sel}=St0}=T) ->
    wings_io:change_event_handler(?SDL_KEYUP, true),
    wings_wm:grab_focus(),
    case wings_io:is_grabbed() of
	true -> ok;
	false -> wings_io:grab()
    end,
    St1 = case wings_pref:get_value(tweak_point) of
	      _ when Sel =:= [] -> St;
	      from_element -> St0;
	      from_cursor -> St0;
	      _other ->
		  case wings_pref:get_value(tweak_axis) of
		      element_normal -> St0;
		      element_normal_edge -> St0;
		      _ when Action =:= delete -> St0;
		      _ -> St
		  end
	  end,
    case Mode of
	paint -> ignore;
	erase -> ignore;
	_ ->
	    begin_drag(What, St1, T),
	    do_tweak_0(0, 0, 0, 0, {move,screen})
    end,
    handle_tweak_drag_event_0(Ev,T).

%%%
%%% Tweak Event Handlers
%%%

update_tweak_handler(T) ->
    case wings_pref:get_value(hide_sel_while_dragging) of
	true -> ok;
	false -> wings_draw:update_sel_dlist()
    end,
    wings_wm:dirty(),
    tweak_drag_no_redraw(T).

tweak_drag_no_redraw(T) ->
    {replace,fun(Ev) -> handle_tweak_drag_event_0(Ev, T) end}.

handle_tweak_drag_event_0(grab_lost, #tweak{mode=Mode}=T) ->
    case lists:member(Mode, [paint, erase]) of
	true -> end_paint(T);
	false -> end_drag(T)
    end;
handle_tweak_drag_event_0(redraw, #tweak{mode=Mode,st=St}=T) ->
    redraw(St),
    tweak_keys_info(),
    info_line(Mode),
    case statusbar() of
	[] -> ok;
	TweakInfo -> wings_io:info(TweakInfo)
    end,
    tweak_drag_no_redraw(T);

%%%
%%% MouseMotion Events
%%%

handle_tweak_drag_event_0(#mousemotion{}=Ev, #tweak{mode={TwkMode,_}}=T0) ->
    %% Tweak Modes that can be modified by xyz constraints
    handle_tweak_drag_event_0(Ev, T0#tweak{mode=TwkMode});
handle_tweak_drag_event_0(#mousemotion{x=X,y=Y,state=?SDL_PRESSED}, #tweak{mode=Mode}=T)
			    when Mode=:=paint; Mode=:=erase ->
    {W,H} = wings_wm:win_size(),
    CX = max(0,min(X,W)),
    CY = max(0,min(Y,H)),
    case do_paint(X, Y, T#tweak{cx=CX,cy=CY}) of
	#tweak{}=T0 ->
	    wings_wm:dirty(),
	    tweak_drag_no_redraw(T0);
	_ -> keep
    end;
handle_tweak_drag_event_0(#mousemotion{x=X,y=Y},
			  #tweak{mode=TweakMode,ox=OX,oy=OY,cx=CX,cy=CY}=T0) ->
    Mode =
	case TweakMode of
	    move -> actual_mode(TweakMode);
	    scale -> actual_mode(TweakMode);
	    move_normal -> actual_mode(TweakMode);
	    scale_uniform -> actual_mode(TweakMode);
	    _ -> TweakMode
	end,
    DX = X-OX, %since last move X
    DY = Y-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y
    wings_io:warp(OX,OY),
    do_tweak_0(DX,DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{mode=Mode,cx=DxOrg,cy=DyOrg},
    update_tweak_handler(T);

%%%
%%% Keyboard Events
%%%

handle_tweak_drag_event_0(#keyboard{sym=Sym,state=?SDL_RELEASED},T) ->
    Pressed0 = wings_pref:get_value(tweak_axis_toggle),
    case lists:keytake(Sym,1,Pressed0) of
	{value,{Sym,Axis,PressTime},Pressed} ->
	    ClickSpeed = wings_pref:get_value(tweak_click_speed),
	    case timer:now_diff(os:timestamp(), PressTime) > ClickSpeed of
		true ->
		    toggle_axis(Axis),
		    wings_wm:send({tweak,axis_constraint}, update_palette);
		false -> ok
	    end,
	    wings_pref:set_value(tweak_axis_toggle,Pressed),
	    update_tweak_handler(T);
	false -> keep
    end;
handle_tweak_drag_event_0(#keyboard{sym=Sym,mod=Mod}=Ev, #tweak{ox=OX,oy=OY,st=St}=T)
  when Mod band (?ALT_BITS bor ?SHIFT_BITS bor ?CTRL_BITS) =:= 0 ->
    %% Activate Tweak Camera
    case wings_camera:tweak_camera_event(Sym, OX, OY, St) of
	next -> handle_tweak_drag_event_1(Ev, T);
	Other -> Other
    end;
handle_tweak_drag_event_0(Ev,T) ->
    handle_tweak_drag_event_1(Ev,T).

handle_tweak_drag_event_1(#keyboard{sym=Sym,mod=Mod}=Ev, #tweak{magnet=Mag,st=St}=T) ->
    case wings_hotkey:event(Ev, St) of
	next ->
	    case  magnet_has_hotkey() of
		true ->
		    is_tweak_combo(T);
		false when Mag->
		    case is_altkey_magnet_event(Sym,Mod) of
			true -> tweak_drag_mag_adjust(T#tweak{sym=Sym});
			false ->
			    is_tweak_combo(T)
		    end;
		false ->
		    is_tweak_combo(T)
	    end;
	{tweak,{tweak_magnet,mag_adjust}} ->
	    if Mag ->
		    tweak_drag_mag_adjust(T#tweak{sym=Sym});
	       true -> keep
	    end;
	Action ->
	    is_tweak_hotkey(Action, T#tweak{sym=Sym})
    end;

handle_tweak_drag_event_1(Ev,T) ->
    handle_tweak_drag_event_2(Ev,T).

%%%
%%% Mouse Button Events
%%%

handle_tweak_drag_event_2(#mousebutton{button=B}=Ev, #tweak{st=St}) when B > 3 ->
    case wings_camera:event(Ev, St) of
	next -> keep;
	Other -> Other
    end;
%% Mouse Button released, so end drag sequence.
handle_tweak_drag_event_2(#mousebutton{button=B,state=?SDL_RELEASED}, #tweak{mode=Mode}=T)
			    when B < 4 ->
    case  wings_io:get_mouse_state() of
	{0,_,_} ->
	    case wings_pref:get_value(tweak_axis_toggle) of
		[] -> wings_io:change_event_handler(?SDL_KEYUP, false);
		_ -> ok
	    end,
	    case lists:member(Mode, [paint, erase]) of
		true -> end_paint(T);
		false -> end_drag(T)
	    end;
	_buttons_still_pressed -> keep
    end;
handle_tweak_drag_event_2(_,_) ->
    keep.

%%%
%%% Adjust Magnet Radius
%%%

magnet_adjust(#tweak{st=#st{selmode=body}}) -> next;
magnet_adjust(#tweak{mode=Mode}) when Mode =:= paint; Mode =:= erase -> next;
magnet_adjust(#tweak{st=St0}=T0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X,Y,St0) of
	{add, What, St} ->
	    magnet_handler_setup(What, X, Y, St, T0);
	{delete, What, _} ->
	    magnet_handler_setup(What, X, Y, St0, T0);
	none -> next
    end.

magnet_handler_setup({Id,Elem,_}=What, X, Y, St, T0) ->
    wings_io:change_event_handler(?SDL_KEYUP, true),
    IdElem = {Id,[Elem]},
    wings_wm:grab_focus(),
    wings_io:grab(),
    begin_magnet_adjustment(What, St),
    tweak_magnet_radius_help(true),
    T = T0#tweak{id=IdElem,ox=X,oy=Y,cx=0,cy=0},
    {seq,push,update_magnet_handler(T)}.

%%%
%%% Magnet Handler
%%%

update_magnet_handler(T) ->
    case wings_pref:get_value(hide_sel_while_dragging) of
	true -> ok;
	false -> wings_draw:update_sel_dlist()
    end,
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_magnet_event(Ev, T)end}.

handle_magnet_event(redraw, #tweak{st=St}=T) ->
    redraw(St),
    draw_magnet(T),
    update_magnet_handler(T);
handle_magnet_event({new_state,St}, T) ->
    end_magnet_event(T#tweak{st=St});
handle_magnet_event(#mousemotion{x=X},#tweak{ox=OX, oy=OY}=T0) ->
    DX = X-OX, %since last move X
    wings_io:warp(OX,OY),
    T = adjust_magnet_radius(DX,T0),
    update_magnet_handler(T);
%% If something is pressed during magnet radius adjustment, save changes
%% and begin new event.
handle_magnet_event(#keyboard{sym=Sym,state=?SDL_RELEASED},#tweak{sym=Sym}=T) ->
    end_magnet_event(T);
handle_magnet_event(#keyboard{sym=Sym},#tweak{sym=Sym}) ->
    keep;
handle_magnet_event(#keyboard{}=Ev,T) ->
    end_magnet_event(Ev,T);
handle_magnet_event(#mousebutton{}=Ev,#tweak{ox=X,oy=Y}=T) ->
    end_magnet_event(Ev#mousebutton{x=X,y=Y},T);
handle_magnet_event(#mousemotion{},T) ->
    end_magnet_event(T);
handle_magnet_event(Ev,T) ->
    end_magnet_event(Ev,T).


%%%
%%% Handeler for In-Drag Magnet Radius Adjustments
%%%

tweak_drag_mag_adjust(#tweak{st=#st{selmode=body}}) -> keep;
tweak_drag_mag_adjust(#tweak{magnet=false}) -> keep;
tweak_drag_mag_adjust(#tweak{mode=Mode, cx=CX, cy=CY, ox=OX, oy=OY}=T0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    DX = X-OX, %since last move X
    DY = Y-OY, %since last move Y
    DxOrg = DX+CX, %total X
    DyOrg = DY+CY, %total Y
    wings_io:warp(OX,OY),
    do_tweak_0(DX,DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{cx=DxOrg,cy=DyOrg},
    update_in_drag_radius_handler(T).

update_in_drag_radius_handler(T) ->
    case wings_pref:get_value(hide_sel_while_dragging) of
	true -> ok;
	false -> wings_draw:update_sel_dlist()
    end,
    wings_wm:dirty(),
    in_drag_radius_no_redraw(T).

in_drag_radius_no_redraw(T) ->
    {replace,fun(Ev) ->
		     handle_in_drag_magnet_ev(Ev, T)end}.

handle_in_drag_magnet_ev(redraw, #tweak{magnet=Mag,st=St}=T) ->
    redraw(St),
    tweak_keys_info(),
    tweak_magnet_radius_help(Mag),
    draw_magnet(T),
    in_drag_radius_no_redraw(T);
handle_in_drag_magnet_ev(#mousemotion{x=X},#tweak{ox=OX, oy=OY}=T0) ->
    DX = X-OX, %since last move X
    wings_io:warp(OX,OY),
    T = in_drag_adjust_magnet_radius(DX,T0),
    update_in_drag_radius_handler(T);
handle_in_drag_magnet_ev(#keyboard{sym=Sym,state=?SDL_RELEASED}, #tweak{sym=Sym}=T) ->
    end_in_drag_mag_event(redraw,T);
handle_in_drag_magnet_ev(#keyboard{sym=Sym}, #tweak{sym=Sym}) ->
    keep;
handle_in_drag_magnet_ev(Ev,T) ->
    end_in_drag_mag_event(Ev, T).

end_in_drag_mag_event(Ev,#tweak{magnet=Mag, mag_type=MagType, mag_rad=MagR}=T) ->
    wings_pref:set_value(tweak_magnet, {Mag, MagType, MagR}),
    handle_tweak_drag_event_0(Ev, T).

%%%
%%% End Magnet Events
%%%

end_magnet_event(#tweak{st=St}=T) ->
    end_magnet_event({new_state,St},T).

end_magnet_event(Ev,#tweak{id=Id}=T) ->
    wings_io:change_event_handler(?SDL_KEYUP, false),
    save_magnet_prefs(T),
    end_magnet_adjust(Id),
    wings_wm:later(Ev),
    pop.


%%%
%%% End of event handlers
%%%


redraw(St) ->
    Render =
	fun() ->
		wings_wm:clear_background(),
		wings_render:render(St)
	end,
    wings_io:batch(Render).

%%%
%%% Magnet Radius Adjustments
%%%

begin_magnet_adjustment(SelElem, St) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
			 begin_magnet_adjustment_fun(D, SelElem)
		 end, []).

begin_magnet_adjustment_fun(#dlo{src_sel={Mode,Els},src_we=We}=D, SelElem) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    case Vs0 of
	[] -> D;
	_ ->
	    Center = wings_vertex:center(Vs0, We),
	    MM = case {We,SelElem} of
		     {#we{id=Id},{Id,_,MM0}} -> MM0;
		     {_,_} -> original
		 end,
	    D#dlo{drag=#drag{pos=Center,mm=MM}}
    end;
begin_magnet_adjustment_fun(D, _) -> D.

adjust_magnet_radius(MouseMovement, #tweak{mag_rad=Falloff0}=T0) ->
    case Falloff0 + MouseMovement * wings_pref:get_value(tweak_mag_adj_sensitivity) of
        Falloff when Falloff > 0 ->
            T0#tweak{mag_rad=Falloff};
        _otherwise -> T0
    end.

in_drag_adjust_magnet_radius(MouseMovement, #tweak{mag_rad=Falloff0}=T) ->
    case Falloff0 + MouseMovement * wings_pref:get_value(tweak_mag_adj_sensitivity) of
        Falloff when Falloff > 0 ->
            setup_magnet(T#tweak{mag_rad=Falloff});
        _otherwise -> T
    end.

end_magnet_adjust({OrigId,El}) ->
    wings_dl:map(fun(#dlo{src_we=#we{id=Id}}=D, _) ->
			 if OrigId =:= Id -> show_cursor(El,D); true -> ok end,
			 D#dlo{vs=none,sel=none,drag=none}
		 end, []).

%%%
%%% Begin Drag
%%%

begin_drag(SelElem, St, T) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
			 begin_drag_fun(D, SelElem, St, T)
		 end, []).

begin_drag_fun(#dlo{src_sel={body,_},src_we=#we{vp=Vtab}=We}=D, _, _, _) ->
    Vs = wings_util:array_keys(Vtab),
    Center = wings_vertex:center(Vs, We),
    Id = e3d_mat:identity(),
    D#dlo{drag={matrix,Center,Id,e3d_mat:expand(Id)}};
begin_drag_fun(#dlo{src_sel={Mode,Els},src_we=#we{id=Id}=We}=D0, SelElem, #st{pst=StPst,sel=Sel}=St, T) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    case Vs0 of
	[] -> D0;
	_ ->
	    %% check #we{} for any painted soft selection and if present we rebuild
	    %% needed magnet data and ignore any "hard" selection on it
	    Painted =
		case gb_trees:lookup(?MODULE, StPst) of
		    {_, MpData} -> gb_sets:is_element(Id,MpData);
		    _ -> false
		end,
	    {Vs,Magnet,VsDyn} =
		case Painted of
		    true ->
			{Vs1,Magnet0,VsDyn0,Center0} = rebuild_magnet(We),
			Match = ordsets:intersection(Vs0,Vs1),
			%% in order to start the tweak action the selected item must
			%% be part of a painted soft selection, otherwise it acts as
			%% the regular tweak command.
			%% Without that Wings3D crashes
			case Match of
			    [] ->
				Center = wings_vertex:center(Vs0, We),
				begin_magnet(T, Vs0, Center, We);
			    _ ->
				Center = Center0,
				{Vs1,Magnet0,VsDyn0}
			end;
		    false ->
			Center = wings_vertex:center(Vs0, We),
			begin_magnet(T, Vs0, Center, We)
		end,
	    #dlo{src_we=We0}= D = wings_draw:split(D0, Vs, St),

	    L = length(Sel) > 1,
	    MM = case {We,SelElem} of
		     {#we{id=Id},{Id,_,_}} when L -> original; %% so at least the shapes
		     %% drag in the same direction.. if the mirrors are pointed the same too.
		     {#we{id=Id},{Id,_,MM0}} -> MM0;
		     {_,_} -> original
		 end,
	    We1 = set_edge_influence(VsDyn,We0),
	    D#dlo{src_we=We1,drag=#drag{vs=Vs0,pos0=Center,pos=Center,mag=Magnet,mm=MM}}
    end;
begin_drag_fun(#dlo{src_we=#we{id=Id}=We}=D0, SelElem, #st{pst=StPst,sel=Sel}=St, _T) ->
    Painted =
	case gb_trees:lookup(?MODULE, StPst) of
	    {_, MpData} -> gb_sets:is_element(Id,MpData);
	    _ -> false
	end,
    case Painted of
	true ->
	    %% This code will run always we have soft selections painted in
	    %% multiple objects and for those without any "hard" selection
	    {Vs,Magnet,VsDyn,Center} = rebuild_magnet(We),
	    #dlo{src_we=We0}= D = wings_draw:split(D0, Vs, St),

	    L = length(Sel) > 1,
	    MM = case {We,SelElem} of
		     {#we{id=Id},{Id,_,_}} when L -> original; %% so at least the shapes
		     %% drag in the same direction.. if the mirrors are pointed the same too.
		     {#we{id=Id},{Id,_,MM0}} -> MM0;
		     {_,_} -> original
		 end,
	    We1 = set_edge_influence(VsDyn,We0),
	    D#dlo{src_we=We1,drag=#drag{vs=Vs,pos0=Center,pos=Center,mag=Magnet,mm=MM}};
	false -> D0
    end.

end_drag(#tweak{mode=Mode,id={_,{OrigId,El}},st=St0}) ->
    St = wings_dl:map(fun (#dlo{src_we=#we{id=Id}}=D, St1) ->
			      if OrigId =:= Id -> show_cursor(El,D); true -> ok end,
			      end_drag(Mode, D, St1)
		      end, St0),
    wings_wm:later({new_state,St}),
    pop.


%%%
%%% End Drag (end tweak event)
%%%

%% paint
end_drag(Mode, #dlo{src_sel=SrcSel,src_we=#we{id=Id,pst=Pst0}=We}=D0, #st{pst=StPst0,shapes=Shs0}=St0) when Mode =:= paint; Mode =:= erase ->
    St =
	case SrcSel of
	    {Mode,Sel} ->
		St0#st{selmode=Mode,sel=[{Id,Sel}]};
	    _ -> St0
	end,
    MpData0 =
	case gb_trees:lookup(?MODULE, StPst0) of
	    {_, MpData1} -> MpData1;
	    _ -> gb_sets:empty()
	end,
    %% this will remove the red dot - cursor position - from the update_dlist routine
    %% and add the #we Id to the #st{pst} data for further use in begin_drag_fun/3
    {MpData,Pst} =
	case gb_trees:lookup(?MODULE, Pst0) of
	    {_, Data} ->
		VsDyn =
		    case gb_trees:get(?MAG_INFO,Data) of
			{_CurPos,VsDyn0} ->
			    gb_trees:to_list(VsDyn0);	% for the latest painted #we{}
			VsDyn0 -> VsDyn0	% for #we{} with magnet influence painted
		    end,
		{gb_sets:add(Id,MpData0),add_pst(VsDyn,Pst0)};
	    _ -> {MpData0,Pst0}
	end,
    Shs = gb_trees:update(Id,We#we{pst=Pst},Shs0),
    StPst = gb_trees:enter(?MODULE,MpData,StPst0),
    {D0#dlo{vs=none,drag=none,sel=none,split=none},St#st{pst=StPst,shapes=Shs}};
%% update
end_drag(update, #dlo{src_sel={Mode,Sel}, src_we=#we{id=Id},drag={matrix,_,Matrix,_}}=D,
	 #st{shapes=Shs0}=St0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    {D,St#st{selmode=Mode,sel=[{Id,Sel}]}};
end_drag(update, #dlo{src_sel={Mode,Sel},src_we=#we{id=Id}}=D0, #st{shapes=Shs0}=St0) ->
    #dlo{src_we=We} = wings_draw:join(D0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    {D0,St#st{selmode=Mode,sel=[{Id,Sel}]}};
%% tweak modes
end_drag(_, #dlo{src_we=#we{id=Id},drag={matrix,_,Matrix,_}}=D,
	 #st{shapes=Shs0}=St0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    D1 = D#dlo{src_we=We},
    D2 = wings_draw:changed_we(D1, D),
    {D2#dlo{vs=none,sel=none,drag=none},St};
end_drag(Mode, #dlo{src_sel={_,_},src_we=#we{id=Id}}=D0, #st{pst=StPst0,shapes=Shs0}=St0) ->
    %% any information about painted solft selection must be removed from pst's field
    StPst =
	case gb_trees:lookup(?MODULE, StPst0) of
	    {_, MpData0} ->
		MpData = gb_sets:delete_any(Id,MpData0),
		case gb_sets:size(MpData) of
		    0 -> gb_trees:delete(?MODULE, StPst0);
		    _ -> gb_trees:enter(?MODULE, MpData, StPst0)
		end;
	    _ -> StPst0
	end,
    case Mode of
	slide ->
	    case wings_io:is_key_pressed(?SDLK_F1) of
		false ->
		    #dlo{src_we=We}=D = wings_draw:join(D0),
		    Shs = gb_trees:update(Id, set_edge_influence([],We), Shs0),
		    St = St0#st{shapes=Shs},
		    {D#dlo{vs=none,sel=none,drag=none},St#st{pst=StPst}};
		true ->
		    #dlo{src_we=We} = D = wings_draw:join(D0),
		    St = case collapse_short_edges(0.0001,We) of
			     {delete, _} ->
				 Shs = gb_trees:delete(Id,Shs0),
				 St0#st{shapes=Shs,sel=[]};
			     {true, We1} ->
				 Shs = gb_trees:update(Id, set_edge_influence([],We1), Shs0),
				 St0#st{shapes=Shs};
			     {false, We1} ->
				 Shs = gb_trees:update(Id, set_edge_influence([],We1), Shs0),
				 St0#st{shapes=Shs, sel=[]}
			 end,
		    {D#dlo{vs=none,sel=none,drag=none},St#st{pst=StPst}}
	    end;
	_ ->
	    #dlo{src_we=We}=D = wings_draw:join(D0),
	    We0=set_edge_influence([],We),
	    Shs = gb_trees:update(Id, We0, Shs0),
	    St = St0#st{shapes=Shs,pst=StPst},
	    {D#dlo{plugins=[],vs=none,sel=none,drag=none,src_we=We0},St}
    end;
end_drag(_, #dlo{src_we=#we{id=Id}}=D0, #st{shapes=Shs0,pst=StPst0}=St0) ->
    case gb_trees:lookup(?MODULE, StPst0) of
	{_, MpData0} ->
	    %% check if #we{} have painted solft selection and clear pst fields
	    case gb_sets:is_element(Id,MpData0) of
		true ->
		    MpData = gb_sets:delete_any(Id,MpData0),
		    StPst =
			case gb_sets:size(MpData) of
			    0 -> gb_trees:delete(?MODULE, StPst0);
			    _ -> gb_trees:enter(?MODULE, MpData, StPst0)
			end,

		    #dlo{src_we=We}=D = wings_draw:join(D0),
		    We0=set_edge_influence([],We),
		    Shs = gb_trees:update(Id, We0, Shs0),
		    St = St0#st{shapes=Shs,pst=StPst},
		    {D#dlo{plugins=[],vs=none,sel=none,drag=none,src_we=We0},St};
		false -> {D0, St0}
	    end;
	_ -> {D0, St0}
    end.

%%%
%%% Do Tweak
%%%

do_tweak_0(DX0, DY0, DxOrg, DyOrg, Mode) ->
    TweakSpeed = wings_pref:get_value(tweak_speed),
    DX = DX0 * TweakSpeed,
    DY = DY0 * TweakSpeed,
    wings_dl:map(fun
		     (#dlo{src_we=We}=D, _) when ?IS_LIGHT(We) ->
			case Mode of
			    {move,Dir} when Dir =:= normal; Dir =:= element_normal;
					    Dir =:= default; Dir =:= element_normal_edge ->
				do_tweak(D, DX, DY, DxOrg, DyOrg, {move,screen});
			    {move,_} ->
				do_tweak(D, DX, DY, DxOrg, DyOrg, Mode);
			    _ ->
				do_tweak(D, DX, DY, DxOrg, DyOrg, {move,screen})
			end;
		     (D, _) ->
			do_tweak(D, DX, DY, DxOrg, DyOrg, Mode)
		end, []).

do_tweak(#dlo{drag={matrix,Pos0,Matrix0,_},src_we=#we{id=Id}}=D0,
	 DX,DY,_,_,Mode) ->
    Matrices = wings_u:get_matrices(Id, original),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    Rad = wings_pref:get_value(tweak_radial),
    Pos = case Mode of
	      {move,x} -> if Rad -> {Px,Ty,Tz}; true -> {Tx,Py,Pz} end;
	      {move,y} -> if Rad -> {Tx,Py,Tz}; true -> {Px,Ty,Pz} end;
	      {move,z} -> if Rad -> {Tx,Ty,Pz}; true -> {Px,Py,Tz} end;
	      {move,xy} -> if Rad -> {Px,Py,Tz}; true -> {Tx,Ty,Pz} end;
	      {move,yz} -> if Rad -> {Tx,Py,Pz}; true -> {Px,Ty,Tz} end;
	      {move,zx} -> if Rad -> {Px,Ty,Pz}; true -> {Tx,Py,Tz} end;
	      _Other -> TweakPos
	  end,
    Move = e3d_vec:sub(Pos, Pos0),
    Matrix = e3d_mat:mul(e3d_mat:translate(Move), Matrix0),
    D0#dlo{drag={matrix,Pos,Matrix,e3d_mat:expand(Matrix)}};

do_tweak(#dlo{drag=#drag{vs=Vs,pos=Pos0,pos0=Orig,pst=none,  %% pst =:= none
			 mag=Mag0,mm=MM}=Drag, src_we=#we{id=Id,mirror=Mir}}=D0,
	 DX, DY, _DxOrg, _DyOrg, {Scale,Type})
  when Scale =:= scale; Scale =:= scale_uniform ->
    %% This is the first time through for Scale ops.
    %% For default Scaling, figure out the axis of scaling by determining the
    %% direction of the user's initial mouse motion. Save this PrimeVector to the
    %% pst field in the #drag record.
    Matrices = case Mir of
		   none -> wings_u:get_matrices(Id, original);
		   _ -> wings_u:get_matrices(Id, MM)
	       end,
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    TweakPointOpType = wings_pref:get_value(tweak_point),
    {{Axis,ENorm,Point},_} = wings_pref:get_value(tweak_geo_point),
    Radial = wings_pref:get_value(tweak_radial),
    {Dir,Pos} = case Type of
		    x -> {axis,{1.0,0.0,0.0}};
		    y -> {axis,{0.0,1.0,0.0}};
		    z -> {axis,{0.0,0.0,1.0}};
		    xy -> {radial,{0.0,0.0,1.0}};
		    yz -> {radial,{1.0,0.0,0.0}};
		    zx -> {radial,{0.0,1.0,0.0}};
		    normal -> {dir, sel_normal_0(Vs,D0)};
		    default_axis ->
			{_,Normal} = wings_pref:get_value(default_axis),
			{axis,Normal};
		    element_normal -> {element_normal, Axis};
		    element_normal_edge -> {element_normal_edge, ENorm};
		    screen ->
			case Radial of
			    true -> {dir, sel_normal_0(Vs,D0)};
			    false when Scale =:= scale ->
				{user,TweakPos}; %% This is for Default Scaling
			    false when Scale =:= scale_uniform ->
				{uniform,TweakPos}
			end
		end,
    {_,MSX,MSY} = wings_wm:local_mouse_state(), %% Mouse Position
    {_,YY0} = wings_wm:win_size(wings_wm:this()), %% Window Size global

    %% Cursor Position according to the model coordinates
    CursorPos = screen_to_obj(Matrices,{float(MSX), float(YY0 - MSY), Zs}),
    %% vector from where the user starts drag to bbox sel center
    V1 = e3d_vec:norm_sub(CursorPos,Orig),

    %% scale axis
    V2 = case e3d_vec:norm_sub(Orig,Pos) of
	     {0.0,0.0,0.0} -> Pos;
	     Other -> Other
	 end,

    %% Flip scale vec depending on the direction
    %% the user is dragging. To or From the selection center.
    Dot = e3d_vec:dot(V1, V2),
    PVec = case  Dot < 0.0 of
	       true -> e3d_vec:neg(V2);
	       false -> V2
	   end,

    PrimeVec = case Dir of
						% user when TweakPointOpType =:= from_element -> Axis;
		   user -> PVec;
		   _other -> Pos
	       end,

    %% Check for active Point ops
    VecData = {DistVec,AxisPoint} = case TweakPointOpType of
					none -> {PVec, {PrimeVec, Orig}};
					from_cursor -> {e3d_vec:neg(PVec), {PrimeVec, CursorPos}};
					from_element ->
					    {e3d_vec:neg(PVec), {PrimeVec, Point}};
					from_default ->
					    {DefPoint,_} = wings_pref:get_value(default_axis),
					    {e3d_vec:neg(PVec), {PrimeVec, DefPoint}}
				    end,

    Dist = dist_along_vector(Orig, TweakPos, DistVec)/2,

    {Vtab,Mag} = case Dir of
		     radial -> tweak_scale_radial(Dist, AxisPoint, Mag0);
		     _ ->
			 case Radial of
			     true -> tweak_scale_radial(Dist, AxisPoint, Mag0);
			     false -> tweak_scale(Dist, AxisPoint, Mag0)
			 end
		 end,

    Pst = {Type,Dir,VecData},
    D = D0#dlo{sel=none,drag=Drag#drag{pos=TweakPos,pst=Pst,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);

do_tweak(#dlo{drag=#drag{pos=Pos0,pos0=Orig,pst={Type,Dir,PrimeVec},
			 mag=Mag0,mm=MM}=Drag, src_we=#we{id=Id,mirror=Mir}}=D0,
	 DX, DY, _DxOrg, _DyOrg, {Scale,Type})
  when Scale =:= scale; Scale =:= scale_uniform ->
    Matrices = case Mir of
		   none -> wings_u:get_matrices(Id, original);
		   _ -> wings_u:get_matrices(Id, MM)
	       end,
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {DistVec,AxisPoint} = PrimeVec,
    Dist = dist_along_vector(Orig, TweakPos, DistVec)/2,
    {Vtab,Mag} = case Dir of
		     uniform -> tweak_scale_uniform(Dist, AxisPoint, Mag0);
		     radial -> tweak_scale_radial(Dist, AxisPoint, Mag0);
		     _ ->
			 case wings_pref:get_value(tweak_radial) of
			     true -> tweak_scale_radial(Dist, AxisPoint, Mag0);
			     false -> tweak_scale(Dist, AxisPoint, Mag0)
			 end
		 end,
    D = D0#dlo{sel=none,drag=Drag#drag{pos=TweakPos,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(#dlo{drag=#drag{vs=Vs,pos=Pos0,mag=#mag{paint=Paint}=Mag0,mm=MM}=Drag,
	      src_we=#we{id=Id,mirror=Mir}}=D0, DX, DY, _DxOrg, _DyOrg,
	 {Move,Type}) when Move =:= move; Move =:= move_normal ->
    Matrices = case Mir of
		   none -> wings_u:get_matrices(Id, original);
		   _ -> wings_u:get_matrices(Id, MM)
	       end,
    Rad = wings_pref:get_value(tweak_radial),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    {Vtab,Mag} = case Type of
		     x ->
			 Pos = if Rad -> {Px,Ty,Tz}; true -> {Tx,Py,Pz} end,
			 magnet_tweak(Mag0, Pos);
		     y ->
			 Pos = if Rad -> {Tx,Py,Tz}; true -> {Px,Ty,Pz} end,
			 magnet_tweak(Mag0, Pos);
		     z ->
			 Pos = if Rad -> {Tx,Ty,Pz}; true -> {Px,Py,Tz} end,
			 magnet_tweak(Mag0, Pos);
		     xy ->
			 Pos = if Rad -> {Px,Py,Tz}; true -> {Tx,Ty,Pz} end,
			 magnet_tweak(Mag0, Pos);
		     yz ->
			 Pos = if Rad -> {Tx,Py,Pz}; true -> {Px,Ty,Tz} end,
			 magnet_tweak(Mag0, Pos);
		     zx ->
			 Pos = if Rad -> {Px,Ty,Pz}; true -> {Tx,Py,Tz} end,
			 magnet_tweak(Mag0, Pos);
		     normal ->
			 Normal =
			     case Paint of
				 Value when is_tuple(Value) -> Value;
				 _ -> sel_normal_0(Vs, D0)
			     end,
			 Pos = tweak_along_axis(Rad, Normal, Pos0, TweakPos),
			 magnet_tweak(Mag0#mag{paint=Normal}, Pos);
		     default_axis ->
			 {_,Axis} = wings_pref:get_value(default_axis),
			 Pos = tweak_along_axis(Rad, Axis, Pos0, TweakPos),
			 magnet_tweak(Mag0, Pos);
		     element_normal ->
			 {{Axis,_,_},_} = wings_pref:get_value(tweak_geo_point),
			 Pos = tweak_along_axis(Rad, Axis, Pos0, TweakPos),
			 magnet_tweak(Mag0, Pos);
		     element_normal_edge ->
			 {{_,Axis,_},_} = wings_pref:get_value(tweak_geo_point),
			 Pos = tweak_along_axis(Rad, Axis, Pos0, TweakPos),
			 magnet_tweak(Mag0, Pos);
		     screen ->
			 if Rad; Move =:= move_normal ->
				 Normal =
				     case Paint of
					 Value when is_tuple(Value) -> Value;
					 _ -> sel_normal_0(Vs, D0)
				     end,
				 Pos = tweak_along_axis(Rad, Normal, Pos0, TweakPos),
				 magnet_tweak(Mag0#mag{paint=Normal}, Pos);
			    true ->
				 Pos = TweakPos,
				 magnet_tweak(Mag0, Pos)
			 end
		 end,
    D = D0#dlo{sel=none,drag=Drag#drag{pos=Pos,pst=none,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(#dlo{drag=#drag{pos=Pos0,pos0=Orig,mag=Mag0,mm=MM}=Drag,
	      src_we=#we{id=Id,mirror=Mir}=We}=D0, DX, DY, DxOrg, _DyOrg, Mode)
  when Mode =:= relax; Mode =:= slide ->
    Matrices = case Mir of
		   none -> wings_u:get_matrices(Id, original);
		   _ -> wings_u:get_matrices(Id, MM)
	       end,
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {Vtab,Mag} =
	case Mode of
	    relax ->
		Len0 = abs(DxOrg) / 600,
		Len = case Len0 > 1 of
			  true -> 1.0;
			  false -> Len0
		      end,
		relax_magnet_tweak_fn(Mag0, We, Len);
	    slide ->
		magnet_tweak_slide_fn(Mag0, We, Orig, TweakPos)
	end,
    D = D0#dlo{sel=none,drag=Drag#drag{pos=TweakPos,pst=none,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(#dlo{drag=#drag{}=Drag}=D, _, _, _, _, _) ->
    D#dlo{drag=Drag#drag{pst=none}};
do_tweak(D, _, _, _, _, _) -> D.

%%%
%%% Tweak Tool Calculations
%%%

%%% Scale
tweak_scale(Dist, {PVec, Point}, #mag{vs=Vs}=Mag) ->
    Vtab = lists:foldl(fun({V, Pos0, Plane, _, Inf}, A) ->
			 D = dist_along_vector(Point, Pos0, PVec),
			 Pos1 = e3d_vec:add_prod(Pos0, PVec, Inf*D*Dist),
			 Pos = mirror_constrain(Plane, Pos1),
			 [{V,Pos}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

tweak_scale_radial(Dist, {Norm,Point}, #mag{vs=Vs}=Mag) ->
    Vtab = lists:foldl(fun({V, Pos0, Plane, _, Inf}, A) ->
			 V1 = e3d_vec:norm_sub(Point, Pos0),
			 V2 = e3d_vec:cross(V1,Norm),
			 Vec = e3d_vec:norm(e3d_vec:cross(V2,Norm)),
			 D = dist_along_vector(Point, Pos0, Vec),
			 Pos1 = e3d_vec:add_prod(Pos0, Vec, Inf*D*Dist),
			 Pos = mirror_constrain(Plane, Pos1),
			 [{V,Pos}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

tweak_scale_uniform(Dist, {_, Point}, #mag{vs=Vs}=Mag) ->
    Vtab = lists:foldl(fun({V, Pos0, Plane, _, Inf}, A) ->
			 Vec = e3d_vec:sub(Point, Pos0),
			 Pos1 = e3d_vec:add_prod(Pos0, Vec, Inf*Dist),
			 Pos = mirror_constrain(Plane, Pos1),
			 [{V,Pos}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

dist_along_vector(PosA,PosB,Vector) ->
    %% Return Distance between PosA and PosB along Vector
    {Xa,Ya,Za} = PosA,
    {Xb,Yb,Zb} = PosB,
    {Vx,Vy,Vz} = e3d_vec:norm(Vector),
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

%%% Relax
relax_magnet_tweak_fn(#mag{vs=Vs}=Mag,We,Weight) ->
    Vtab = lists:foldl(fun({V,P0,Plane,_,1.0}, A) ->
			 P1=relax_vec_fn(V,We,P0,Weight),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A];
		    ({V,P0,Plane,_,Inf}, A) ->
			 P1=relax_vec_fn(V,We,P0,Weight*Inf),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

relax_vec_fn(V, #we{}=We,Pos0,Weight) ->
    Vec = relax_vec(V,We),
    D = e3d_vec:sub(Vec,Pos0),
    e3d_vec:add_prod(Pos0, D, Weight).

relax_vec(V, We) ->
    case collect_neib_verts_coor(V, We) of
	[] ->
	    %% Because of hidden faces there may be no neighbouring vertices,
	    %% so we default to the position of the vertex itself.
	    wings_vertex:pos(V, We);
	Cs0 ->
	    Cs = [C || C <- Cs0, C =/= undefined],
	    if Cs =:= [] -> wings_vertex:pos(V, We);
	       true -> e3d_vec:average([wings_vertex:pos(V, We)|Cs])
	    end
    end.

collect_neib_verts_coor(V,We)->
    VertList = wings_vertex:fold(fun(_,_,ERec,Acc) ->
					 [wings_vertex:other(V,ERec)|Acc]
				 end,[],V,We),
    lists:foldl(fun(Vert,A) -> [wings_vertex:pos(Vert,We)|A] end,[],VertList).

%%% Slide
magnet_tweak_slide_fn(#mag{vs=Vs}=Mag, We,Orig,TweakPos) ->
    Vtab = lists:foldl(fun({V,P0,Plane,_,Inf}, A) ->
			 P1=slide_vec_w(V,P0,Orig,TweakPos,We,Inf,Vs),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

slide_vec_w(V, Vpos, VposS, TweakPosS, We, W,Vs) ->
    Dv = e3d_vec:sub(VposS,Vpos),
    TweakPos = e3d_vec:sub(TweakPosS, Dv),
    Cs0 = collect_neib_verts_coor_vs(V, We, Vs),
    Cs1 = [C || C <- Cs0, C =/= undefined],
    Cs = sub_pos_from_list(Cs1, Vpos),
    TweakPos2=e3d_vec:add(Vpos, e3d_vec:mul(e3d_vec:sub(TweakPos, Vpos), W)),
    slide_one_vec(Vpos, TweakPos2, Cs).

slide_one_vec(Vpos, TweakPos, PosList) ->
    Dpos=e3d_vec:sub(TweakPos,Vpos),
    {Dp,_} = lists:foldl(fun
		       ({0.0,0.0,0.0},VPW) -> VPW;
		       (Vec, {VP,W}) ->
			  Vn = e3d_vec:norm(Vec),
			  Dotp = e3d_vec:dot(Vn,Dpos),
			  if
			      Dotp > W, Dotp > 0 ->
				  Len = e3d_vec:len(Vec),
				  Dotp2 = if
					      Dotp > Len -> Len;
					      true -> Dotp
					  end,
				  {e3d_vec:mul(Vn, Dotp2),Dotp};
			      true -> {VP,W}
			  end
		  end,{{0,0,0},0},PosList),
    e3d_vec:add(Vpos,Dp).

sub_pos_from_list(List,Pos) ->
    lists:foldl(fun
	      (E,B) -> [e3d_vec:sub(E,Pos)|B] end,[],List).

collect_neib_verts_coor_vs(V,We,Vs)->
    VertList = wings_vertex:fold(fun(_,_,ERec,Acc) ->
					 [wings_vertex:other(V,ERec)|Acc]
				 end,[],V,We),
    lists:foldl(fun(E,B) -> [get_orig_pos(E,We,Vs)|B] end,[],VertList).

get_orig_pos(V,We,Vs)->
    Pos=lists:foldl(
	  fun({Vert,Coor,_,_,_},P) ->
		  if V =:= Vert -> Coor; true-> P end
	  end,none,Vs),
    case Pos of
	none -> wings_vertex:pos(V,We);
	_ -> Pos
    end.

%% scanning over the mesh to collapse short edges
collapse_short_edges(Tolerance, #we{es=Etab,vp=Vtab}=We) ->
    Short = array:sparse_foldl(
	      fun(Edge, #edge{vs=Va,ve=Vb}, A) ->
		      case array:get(Va,Vtab) of
			  undefined -> A;
			  VaPos ->
			      case array:get(Vb,Vtab) of
				  undefined -> A;
				  VbPos ->
				      case abs(e3d_vec:dist(VaPos, VbPos)) of
					  Dist when Dist < Tolerance -> [Edge|A];
					  _Dist -> A
				      end
			      end
		      end
	      end, [], Etab),
    NothingCollapsed = Short =:= [],
    case catch wings_collapse:collapse_edges(Short,We) of
        #we{}=We1 ->
            {NothingCollapsed, We1};
        _ ->
            {delete, #we{}}
    end.

%%% Along Average Normal
sel_normal_0(Vs, D) ->
    Normals = sel_normal(Vs,D),
    e3d_vec:norm(e3d_vec:add(Normals)).

sel_normal( _, #dlo{src_we=#we{}=We,src_sel={face,Sel0}}) ->
    Faces = gb_sets:to_list(Sel0),
    face_normals(Faces,We,[]);
sel_normal(Vs,D) ->
    [vertex_normal(V, D) || V <- Vs].

%%% Radial of Default Axis
tweak_along_axis(true, Axis, Pos0, TweakPos) ->
    %% constraining by the plane
    Dot = e3d_vec:dot(Axis, Axis),
    if
	Dot =:= 0.0 -> Pos0;
	true ->
	    T = - e3d_vec:dot(Axis, e3d_vec:sub(TweakPos, Pos0)) / Dot,
	    e3d_vec:add_prod(TweakPos, Axis, T)
    end;

%%% Tweak Along a non-standard Axis
tweak_along_axis(false, Axis, Pos0, TweakPos) ->
    %% Return the point along the normal closest to TweakPos.
    Dot = e3d_vec:dot(Axis, Axis),
    if
	Dot =:= 0.0 -> Pos0;
	true ->
	    T = e3d_vec:dot(Axis, e3d_vec:sub(TweakPos, Pos0)) / Dot,
	    e3d_vec:add_prod(Pos0, Axis, T)
    end.

%%%
%%% Screen to Object Coordinates
%%%

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    wings_gl:project(X, Y, Z, MVM, PM, VP).

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    wings_gl:unProject(Xs, Ys, Zs, MVM, PM, VP).

sel_to_vs(vertex, Vs, _) -> Vs;
sel_to_vs(edge, Es, We) -> wings_vertex:from_edges(Es, We);
sel_to_vs(face, Fs, We) -> wings_face:to_vertices(Fs, We).

%%%
%%% Some Utilities
%%%

%% vertex_normal(Vertex, DLO) -> UnormalizedNormal
%%  Calculate the vertex normal. Will also work for vertices surrounded
%%  by one or more hidden faces.
vertex_normal(V, D) ->
    OrigWe = wings_draw:original_we(D),
    FaceNs = [face_normal(F, D) || F <- wings_face:from_vs([V], OrigWe)],
    e3d_vec:add(FaceNs).

%% face_normal(Face, DLO) -> Normal
%%  Calculate the face normal. Will also work for faces that
%%  are hidden (including the virtual mirror face).
face_normal(Face, #dlo{src_we=#we{vp=Vtab}}=D) ->
    #we{vp=OrigVtab} = OrigWe = wings_draw:original_we(D),
    Vs = wings_face:vertices_ccw(Face, OrigWe),
    VsPos = [vertex_pos(V, Vtab, OrigVtab) || V <- Vs],
    e3d_vec:normal(VsPos).

face_normals([Face|Fs], We, Normals) ->
    N = wings_face:normal(Face, We),
    face_normals(Fs, We, [N|Normals]);
face_normals([], _We, Normals) ->
    Normals.

vertex_pos(V, Vtab, OrigVtab) ->
    case array:get(V, Vtab) of
	undefined -> array:get(V, OrigVtab);
	Pos -> Pos
    end.

magnet_tweak(#mag{orig=Orig,vs=Vs}=Mag, Pos) ->
    Vec = e3d_vec:sub(Pos, Orig),
    Vtab = lists:foldl(fun({V,P0,Plane,_,1.0}, A) ->
			 P1 = e3d_vec:add(P0, Vec),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A];
		    ({V,P0,Plane,_,Inf}, A) ->
			 P1 = e3d_vec:add_prod(P0, Vec, Inf),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

%% Get center point from closest element to cursor and store it for
%% From Element constraints.
from_element_point(X ,Y, #st{shapes=Shs}=St0) ->
    Stp = St0#st{selmode=face,sel=[],sh=true}, % smart highlight mode
    GeomPoint = wings_pick:raw_pick(X,Y,Stp),
    {Selmode, _, {IdP, ElemP}} = GeomPoint,
    We = gb_trees:get(IdP, Shs),
    AxisPoint  = point_center(Selmode, ElemP, We),
    wings_pref:set_value(tweak_geo_point, {AxisPoint,GeomPoint}).

point_center(vertex, V, #we{vp=Vtab,mirror=Mir}=We) ->
    {MirN,Ns} = wings_vertex:fold(fun
				      (_, Face, _, {_,A}) when Face =:= Mir ->
					 {wings_face:normal(Face, We),A};
				      (_, Face, _, {Mb,A}) ->
					 {Mb,[wings_face:normal(Face, We)|A]}
				 end, {none,[]}, V, We),
    Norm = e3d_vec:norm(e3d_vec:add(Ns)),
    Normal = case MirN =:= none of
		 true -> Norm;
		 false ->
		     N = e3d_vec:cross(MirN, Norm),
		     e3d_vec:cross(N,MirN)
	     end,
    Pos = array:get(V, Vtab),
    {Normal,Normal,Pos};
point_center(edge, E, #we{es=Etab,vp=Vtab,mirror=Mir}=We) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = array:get(E, Etab),
    PosA = array:get(Va, Vtab),
    PosB = array:get(Vb, Vtab),
    Pos = e3d_vec:average(PosA, PosB),
    EdgeNorm = e3d_vec:norm_sub(PosA,PosB),
    FaceNormL = wings_face:normal(Lf, We),
    FaceNormR = wings_face:normal(Rf, We),
    Normal = case Mir of
		 Lf -> N = e3d_vec:cross(FaceNormL,FaceNormR),
		       e3d_vec:cross(N,FaceNormL);
		 Rf -> N = e3d_vec:cross(FaceNormR,FaceNormL),
		       e3d_vec:cross(N,FaceNormR);
		 _ -> e3d_vec:average(FaceNormL, FaceNormR)
	     end,
    {Normal,EdgeNorm,Pos};
point_center(face, F, We) ->
    Pos = wings_face:center(F, We),
    Normal = wings_face:normal(F, We),
    {Normal,Normal,Pos}.

%%%
%%% Magnet Calculations
%%%

%% Setup magnet in the middle of a tweak op
setup_magnet(#tweak{mode=TwkMode, cx=X, cy=Y}=T)
  when TwkMode =:= scale;  TwkMode =:= scale_uniform; TwkMode =:= move_normal; TwkMode =:= move ->
    wings_dl:map(fun(D, _) ->
		     setup_magnet_fun(D, T)
		 end, []),
    Mode = actual_mode(TwkMode),
    do_tweak_0(0, 0, X, Y, Mode),
    T;
setup_magnet(#tweak{mode=Mode, cx=X, cy=Y}=T) ->
    wings_dl:map(fun(D, _) ->
		     setup_magnet_fun(D, T)
		 end, []),
    do_tweak_0(0, 0, X, Y, Mode),
    T.

setup_magnet_fun(#dlo{src_sel={_,_},drag=#drag{vs=Vs0,pos0=Center}=Drag}=Dl0,
		 #tweak{st=St}=T) ->
    We = wings_draw:original_we(Dl0),
    {Vs,Mag,VsDyn} = begin_magnet(T, Vs0, Center, We),
    #dlo{src_we=We0} = Dl = wings_draw:split(Dl0, Vs, St),
    W1 = set_edge_influence(VsDyn,We0),
    Dl#dlo{src_we=W1,drag=Drag#drag{mag=Mag}};
setup_magnet_fun(Dl, _) -> Dl.

begin_magnet(#tweak{magnet=false}=T, Vs0, Center, We) ->
    Mirror = mirror_info(We),
    {Vs,Near} = near(Center, Vs0, [], Mirror, T, We),
    Magnet = #mag{orig=Center,vs=Near},
    {Vs,Magnet,[]};
begin_magnet(#tweak{magnet=true}=T, Vs0, Center, #we{vp=Vtab0}=We) ->
    Mirror = mirror_info(We),
    Vtab1 = sofs:from_external(array:sparse_to_orddict(Vtab0), [{vertex,info}]),
    Vtab2 = sofs:drestriction(Vtab1, sofs:set(Vs0, [vertex])),
    Vtab = sofs:to_external(Vtab2),
    {Vs, Near} = near(Center, Vs0, Vtab, Mirror, T, We),
    Magnet = #mag{orig=Center,vs=Near},
    VsDyn = [{Va,Inf} || {Va,_,_,_,Inf} <- Near],
    {Vs,Magnet,VsDyn}.

near(Center, Vs0, MagVs0, Mirror, #tweak{mag_rad=R,mag_type=Type}, We) ->
    RSqr = R*R,
    MagVs = minus_locked_vs(MagVs0, We),
    M = lists:foldl(fun({V,Pos}, {Vs,A}=Acc) ->
		      case e3d_vec:dist_sqr(Pos, Center) of
			  DSqr when DSqr =< RSqr ->
			      D = math:sqrt(DSqr),
			      Inf = magnet_type_calc(Type, D, R),
			      Matrix = mirror_matrix(V, Mirror),
			      {[V|Vs],[{V,Pos,Matrix,D,Inf}|A]};
			  _ -> Acc
		      end;
		 (_, A) -> A
	      end, {[],[]}, MagVs),
    lists:foldl(fun(V, {Vs,A}) ->
		  Matrix = mirror_matrix(V, Mirror),
		  Pos = wpa:vertex_pos(V, We),
		  {[V|Vs],[{V,Pos,Matrix,0.0,1.0}|A]}
          end, M, Vs0).

%%%
%%% Paint Routines
%%%

%%% obs: #we{pst} isn't persistent for regular Tweak commands. If it exists when
%%%      a tweak command starts then there is a soft selection painted.

end_paint(#tweak{mode=Mode,id={_,{OrigId,_}},cx=X,cy=Y,st=St0}=_T) ->
    St = wings_dl:map(fun (#dlo{src_we=#we{id=Id}}=D, St1) ->
			    if OrigId =:= Id ->
				wings_wm:release_focus(),
				wings_io:ungrab(X,Y);
			    true -> ignore
			    end,
			    end_drag(Mode, D, St1)
		      end, St0),
    wings_wm:later({new_state,St}),
    pop.

do_paint(X, Y, #tweak{id={Action,{Id,_}},magnet=true,mag_type=MagType,
		      mag_rad=MagR,pnt_route=PaintRoute,st=#st{shapes=Shs0}=St0}=T) ->
    case wings_pick:raw_pick(X, Y, St0#st{selmode=face,sel=[],sh=false}) of
	{_,MM,{Id,Face}} ->
	    #we{pst=Pst} = We = gb_trees:get(Id,Shs0),
	    {Normal,_,Center} = point_center(face,Face,We),
	    CurPos = scr2d_to_pnt3d(X,Y,Id,MM,Center,Normal), % current vertex (cursor pos)
	    VsDynOld = get_paint_info(Pst),
	    VsDyn = do_paint(Action,{MagType,MagR},{PaintRoute,Face},Center,We,VsDynOld),
	    NewPst = add_pst({CurPos,VsDyn},Pst),
	    Shs = gb_trees:update(Id,We#we{pst=NewPst},Shs0),
	    St = St0#st{sel=[],shapes=Shs},
	    wings_draw:refresh_dlists(St),
	    T#tweak{st=St,ox=X,oy=Y,cx=X,cy=Y};
	_ -> keep
    end;
do_paint(_, _, _) -> keep.

do_paint(Action, {Type,Rad}, {PaintRoute,F}, Center, We, VsDynOld) ->
    Vtab = minus_locked_vs(We),
    {_,VsDynNew0} = Sel = near(Action,{Type,Rad},Center,Vtab),
    VsDynNew =
	case PaintRoute of
	    surface when F=/=none -> validate_surface(F,Sel,We);
	    _ -> VsDynNew0
	end,
    case Action of
	add -> paint(VsDynOld, VsDynNew);
	delete -> erase(VsDynOld, VsDynNew)
    end.

near(Action, {Type,Rad}, Center, Vtab) ->
    RSqr = Rad*Rad,
    array:sparse_foldr(fun(V, Pos, {VAcc,Acc}) ->
			    Dist = e3d_vec:dist_sqr(Pos,Center),
			    if Dist =< RSqr ->
				D0 = math:sqrt(Dist),
				D =
				    case Action of
					add -> D0;
					delete -> Rad-D0
				    end,
				Inf = magnet_type_calc(Type,D,Rad),
				{[V|VAcc],[{V,Inf}|Acc]};
			    true -> {VAcc,Acc}
			    end
		       end,{[],[]},Vtab).

paint(OldVsDyn, NewVsDyn) ->
    lists:foldl(fun({V,Inf}, Acc) ->
		    case gb_trees:lookup(V,Acc) of
			{value, OldInf} ->
			    gb_trees:enter(V,max(Inf, OldInf),Acc);
			_ -> gb_trees:insert(V,Inf,Acc)
		    end
		end, OldVsDyn, NewVsDyn).

erase(OldVsDyn, NewVsDyn) ->
    lists:foldl(fun({V,Inf0}, Acc) ->
		    case gb_trees:lookup(V,Acc) of
			{value, OldInf} ->
			    Inf = Inf0*OldInf,
			    if (Inf =< 0.00001) ->
				gb_trees:delete(V,Acc);
			    true ->
				gb_trees:enter(V,Inf,Acc)
			    end;
			_ -> Acc
		    end
		end,OldVsDyn,NewVsDyn).

validate_surface(F,{Vs,VsDynNew0}, We) ->
    FVs = wings_face:to_vertices([F],We),
    Fs = wings_face:from_vs(Vs,We),
    Fr = wings_sel:face_regions(Fs,We),
    case length(Fr) of
	0 -> VsDynNew0;
	_ ->
	    R = take_valid(FVs,Fr,We,[]),
	    lists:foldl(fun({V,_}=Vinf, Acc) ->
			    case ordsets:is_element(V,R) of
				true -> [Vinf|Acc];
				false -> Acc
			    end
			end, [],VsDynNew0)
    end.

take_valid(_, [], _, Acc) -> Acc;
take_valid(FVs, [R|Rg], We, Acc) ->
    RVs = wings_face:to_vertices(R,We),
    case take_valid_0(FVs,RVs) of
	none -> take_valid(FVs,Rg,We,Acc);
	Vs -> Vs
    end.
take_valid_0([], _) -> none;
take_valid_0([V|FVs], RVs) ->
    case ordsets:is_element(V,RVs) of
	true -> RVs;
	_ -> take_valid_0(FVs, RVs)
    end.

get_paint_info(Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
	{_, Data} ->
	    case gb_trees:get(?MAG_INFO,Data) of
		none -> [];
		{_CurPos,VsDyn} -> VsDyn;
		VsDyn -> gb_trees:from_orddict(VsDyn)
	    end;
	_ -> gb_trees:empty()
    end.

%% It computes the 3d position for a point by projecting 2d screen
%% coordenate on the plane defined by the vertice (V) and its normal (Vn)
%% using the current space transformation. (used by Sculpt Brush code)
scr2d_to_pnt3d(X0, Y0, Id, MM, V, Vn) ->
    {MV,PM,{_,_,W,H}} = wings_u:get_matrices(Id, MM),
    PMi = e3d_mat:invert(e3d_mat:mul(PM, MV)),

    Wc=trunc((W+1)/2),
    Hc=trunc((H+1)/2),
    X=(X0-Wc)/Wc,
    Y=((H-Y0+1)-Hc)/Hc,

    {Xa,Ya,Za,Sa}=e3d_mat:mul(PMi,{X,Y,-1.0,1.0}),
    PosA=e3d_vec:mul({Xa,Ya,Za},1.0/Sa),
    {Xb,Yb,Zb,Sb}=e3d_mat:mul(PMi,{X,Y,0.0,1.0}),
    PosB=e3d_vec:mul({Xb,Yb,Zb},1.0/Sb),
    Dir=e3d_vec:norm(e3d_vec:sub(PosB,PosA)),
    case e3d_vec:dot(Dir,Vn) of
	0.0 ->
	    Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn),
	    e3d_vec:add(PosB, e3d_vec:mul(Vn, Intersection));
	Dot ->
	    Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn) / Dot,
	    e3d_vec:add(PosB, e3d_vec:mul(Dir, Intersection))
    end.

paint_from_selection(#st{selmode=body}=St) -> St;
paint_from_selection(#st{sel=[]}=St) -> St;
paint_from_selection(#st{selmode=Mode,pst=StPst0,shapes=Shs0}=St) ->
    MpData0 =
	case gb_trees:lookup(?MODULE, StPst0) of
	    {_, MpData1} -> MpData1;
	    _ -> gb_sets:empty()
	end,
    {_,MagType,MagR} = wings_pref:get_value(tweak_magnet),
    PaintRoute = wings_pref:get_value(tweak_paint),
    {MpData,Shs} =
    	wings_sel:fold(
	    fun(Elems, #we{id=Id,vp=Vtab,pst=Pst0}=We, {PAcc,SAcc}) ->
		Path0 =
		    case Mode of
			face ->
			    Borders = wings_face:outer_edges(Elems, We),
			    wings_edge:to_vertices(Borders, We);
			edge ->
			    wings_edge:to_vertices(Elems, We);
			_ ->
			    gb_sets:to_list(Elems)
		    end,
		Path = [array:get(V, Vtab) || V <- Path0],
		OldVsDyn = get_paint_info(Pst0),
		VsDyn1 =
		    lists:foldl(fun(V, Acc0)->
				    do_paint(add,{MagType,MagR},{PaintRoute,none},V,We,Acc0)
				end,OldVsDyn,Path),
		Sel = wings_sel:to_vertices(Mode,Elems,We),
		VsDyn0 =
		    lists:foldl(fun(V, Acc1)->
				    gb_trees:enter(V,1.0,Acc1)
			       end,VsDyn1,Sel),
		VsDyn = gb_trees:to_list(VsDyn0),
		Pst = add_pst(VsDyn,Pst0),
		{gb_sets:add(Id,PAcc),gb_trees:update(Id,We#we{pst=Pst},SAcc)}
	    end, {MpData0,Shs0}, St),
    StPst = gb_trees:enter(?MODULE,MpData,StPst0),
    St0 = St#st{pst=StPst,sel=[],shapes=Shs},
    wings_wm:later({new_state,St0}),
    wings_wm:dirty(),
    wings_draw:refresh_dlists(St0),
    keep.

paint_from_weightmap(#st{selmode=Mode,pst=StPst0,shapes=Shs0}=St) ->
    MpData0 =
	case gb_trees:lookup(?MODULE, StPst0) of
	    {_, MpData1} -> MpData1;
	    _ -> gb_sets:empty()
	end,
    {MpData,Shs} =
	wings_sel:fold(
	    fun(Elems, #we{id=Id,pst=Pst0}=We0, {PAcc,SAcc}) ->
		WeightVs =
		    case Mode of
			face ->
			    wings_face:to_vertices(Elems, We0);
			edge ->
			    wings_edge:to_vertices(Elems, We0);
			_ ->
			    Elems
		    end,
		HasUV = wings_va:any_uvs(We0),
		if HasUV ->
		    % applying temporary texture color to vertices
		    We = wings_we:uv_to_color(We0, St),
		    % preparing vertex influence data for painting
		    NewVsDyn =
			lists:foldl(fun(V,Acc) ->
					Va = wings_va:vtx_attrs(V,We),
			    		[{V,color_to_weight(Va)}|Acc]
				    end,[],WeightVs),
		    OldVsDyn = get_paint_info(Pst0),
		    VsDyn0 = paint(OldVsDyn, NewVsDyn),
		    VsDyn = gb_trees:to_list(VsDyn0),
		    Pst = add_pst(VsDyn,Pst0),
		    {gb_sets:add(Id,PAcc),gb_trees:update(Id,We0#we{pst=Pst},SAcc)};
		true ->
		    {PAcc,SAcc}
		end
	    end, {MpData0,Shs0}, St),
    StPst = gb_trees:enter(?MODULE,MpData,StPst0),
    St0 = St#st{pst=StPst,sel=[],shapes=Shs},
    wings_wm:later({new_state,St0}),
    wings_wm:dirty(),
    wings_draw:refresh_dlists(St0),
    keep.

color_to_weight([_Col|none]) -> 0.0;
color_to_weight([Col|_UV]) -> rgb_to_weight(Col).

rgb_to_weight({R,G,B,A}) ->
    rgb_to_weight({R,G,B})*A;
rgb_to_weight({V,V,V}) -> V;
rgb_to_weight({R,0.0,B}) when R > 0.0 ->  % force the range to 0..240 (?MAX_WEIGHT_VALUE)
    rgb_to_weight({0.0,0.0,B});
rgb_to_weight({R,G,B}) ->
    {Hue,_,_}=wings_color:rgb_to_hsv(R,G,B),
    max(0.0,?MAX_WEIGHT_VALUE-Hue)/?MAX_WEIGHT_VALUE.

%%% Used by paint soft selection routine
%%% It rebuilds the magnet data from the dynamic vertices stored in #we{pst}
rebuild_magnet(We) ->
    #we{pst=Pst}=We,
    Mirror = mirror_info(We),
    {_, MgInfo} = gb_trees:lookup(?MODULE, Pst),
    VsDyn = gb_trees:get(?MAG_INFO,MgInfo),
    {Vs, Near} =
	lists:foldr(fun({V,Inf}, {VsAcc,NearAcc}) ->
			Matrix = mirror_matrix(V, Mirror),
			Pos = wpa:vertex_pos(V, We),
			{[V|VsAcc],[{V,Pos,Matrix,1.0,Inf}|NearAcc]}
		    end, {[],[]}, VsDyn),
    Center = wings_vertex:center(Vs, We),
    Magnet = #mag{orig=Center,vs=Near,paint=true},
    {Vs,Magnet,VsDyn,Center}.

set_paint_route(PaintRout) ->
    wings_pref:set_value(tweak_paint,PaintRout).


%%%
%%%  End Paint routines
%%%

%%% Magnet Mask
minus_locked_vs(#we{vp=Vtab,pst=Pst}) ->	% used by paint routine
    Mask = wings_pref:get_value(magnet_mask_on),
    case gb_trees:is_defined(wpc_magnet_mask,Pst) of
	true when Mask ->
	    LockedVs = wpc_magnet_mask:get_locked_vs(Pst),
	    array:sparse_foldl(fun(V,P,Acc)->
			    case gb_sets:is_element(V,LockedVs) of
				true -> Acc;
				false -> array:set(V,P,Acc)
			    end
			 end,array:new(),Vtab);
	_otherwise -> Vtab
    end.
minus_locked_vs(MagVs, #we{pst=Pst}) ->
    Mask = wings_pref:get_value(magnet_mask_on),
    case gb_trees:is_defined(wpc_magnet_mask,Pst) of
	true when Mask ->
	    LockedVs = gb_sets:to_list(wpc_magnet_mask:get_locked_vs(Pst)),
	    remove_masked(LockedVs, MagVs);
	_otherwise ->
	    MagVs
    end.

remove_masked([V|LockedVs],MagVs) ->
    remove_masked(LockedVs,lists:keydelete(V,1,MagVs));
remove_masked([],MagVs) -> MagVs.

magnet_type_calc(dome, D, R) when is_float(R) ->
    math:sin((R-D)/R*math:pi()/2);
magnet_type_calc(straight, D, R) when is_float(R) ->
    (R-D)/R;
magnet_type_calc(spike, D0, R) when is_float(R) ->
    D = (R-D0)/R,
    D*D.

%%%
%%% Draw Magnet
%%%

draw_magnet(#tweak{st=#st{selmode=body}}) -> ok;
draw_magnet(#tweak{magnet=true, mag_rad=R}) ->
    wings_dl:fold(fun(D, _) ->
			  gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
			  gl:disable(?GL_DEPTH_TEST),
			  gl:enable(?GL_BLEND),
			  gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
			  wings_view:load_matrices(false),
			  {CR,CG,CB,CA}=wings_pref:get_value(tweak_magnet_color),
			  wings_io:set_color({CR,CG,CB,CA}),
			  draw_magnet_1(D, R),
			  gl:popAttrib()
		  end, []);
draw_magnet(_) -> ok.

draw_magnet_1(#dlo{src_sel={Mode,Els},src_we=We,mirror=Mtx,drag=#drag{mm=Side}}, R) ->
    case Side of
	mirror -> gl:multMatrixf(Mtx);
	original -> ok
    end,
    Vs = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    {X,Y,Z} = wings_vertex:center(Vs, We),
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:sphere(Obj, R, 40, 40),
    glu:deleteQuadric(Obj);
draw_magnet_1(_, _) -> [].

mirror_info(#we{mirror=none}) -> {[],identity};
mirror_info(#we{mirror=Face}=We) ->
    FaceVs = wings_face:vertices_ccw(Face, We),
    Flatten = wings_we:mirror_projection(We),
    {FaceVs,Flatten}.

mirror_matrix(V, {MirrorVs,Flatten}) ->
    case lists:member(V, MirrorVs) of
	false -> identity;
	true -> Flatten
    end.

mirror_constrain(Matrix, Pos) -> e3d_mat:mul_point(Matrix, Pos).

%%%
%%% Hotkey and Combo Checking
%%%

magnet_has_hotkey() ->
    Hotkeys = wings_hotkey:matching([tweak_magnet,tweak]),
    lists:keymember(mag_adjust,1,Hotkeys).

is_altkey_magnet_event(Sym,Mod) ->
    case Sym of
	?L_ALT -> Mod band (?SHIFT_BITS bor ?CTRL_BITS) =:= 0;
	?R_ALT -> Mod band (?SHIFT_BITS bor ?CTRL_BITS) =:= 0;
	_ -> false
    end.

toggle_data(Axis) ->
    Tp = wings_pref:get_value(tweak_point),
    Ta = wings_pref:get_value(tweak_axis),
    Txyz = wings_pref:get_value(tweak_xyz),
    Which = Ta =:= screen,
    case Axis of
	radial -> radial;
	from_default -> Tp;
	from_cursor -> Tp;
	from_element -> Tp;
	Axis when Axis =:= x; Axis =:= y; Axis =:= z ->
	    if Which -> Axis; true -> Ta end;
	Axis ->
	    if Which -> Txyz; true -> Ta end
    end.

is_tweak_hotkey({tweak,Cmd}, #tweak{magnet=Magnet,sym=Sym,st=#st{pst=StPst,shapes=Shp}=St0}=T0) ->
    case Cmd of
	{axis_constraint, Axis} ->
	    ReturnAxis = toggle_data(Axis),
	    Pressed = wings_pref:get_value(tweak_axis_toggle),
	    case lists:keymember(Sym, 1, Pressed) of
		true -> keep;
		false ->
		    Data = [{Sym,ReturnAxis,os:timestamp()}|Pressed],
		    wings_pref:set_value(tweak_axis_toggle, Data),
		    toggle_axis(Axis),
		    wings_wm:send({tweak,axis_constraint}, update_palette),
		    St = wings_dl:map(fun (D, _) ->
					      update_drag(D, T0)  % used to find mid tweak model data..
				      end, St0),
		    do_tweak_0(0, 0, 0, 0, {move,screen}),
		    update_tweak_handler(T0#tweak{st=St})
	    end;
	{tweak_magnet, toggle_magnet} ->
	    toggle_magnet(),
	    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
	    T = T0#tweak{magnet=Mag, mag_type=MagType,
			 st=St0#st{pst=remove_pst(StPst),shapes=hide_mag_paint(Shp)}},
	    wings_wm:send({tweak,tweak_magnet}, update_palette),
	    tweak_magnet_help(),
	    setup_magnet(T),
	    update_tweak_handler(T);
	{tweak_magnet,reset_radius} when Magnet->
	    Pref = wings_pref:get_value(tweak_magnet),
	    wings_pref:set_value(tweak_magnet,setelement(3,Pref,1.0)),
	    update_tweak_handler(T0#tweak{mag_rad=1.0});
	{tweak_magnet, cycle_magnet} ->
	    NewMag = cycle_magnet(),
	    set_magnet_type(NewMag),
	    T = if NewMag =:= off -> T0#tweak{magnet=false};
		   true -> T0#tweak{magnet=true,mag_type=NewMag}
		end,
	    wings_wm:send({tweak,tweak_magnet}, update_palette),
	    tweak_magnet_help(),
	    setup_magnet(T),
	    update_tweak_handler(T);
	{tweak_magnet, PaintRoute} when PaintRoute =:= shortest; PaintRoute =:= surface ->
	    set_paint_route(PaintRoute),
	    T = T0#tweak{magnet=true, pnt_route=PaintRoute},
	    tweak_magnet_help(),
	    update_tweak_handler(T);
	{tweak_magnet, MagType} ->
	    set_magnet_type(MagType),
	    T = T0#tweak{magnet=true, mag_type=MagType},
	    wings_wm:send({tweak,tweak_magnet}, update_palette),
	    tweak_magnet_help(),
	    setup_magnet(T),
	    update_tweak_handler(T);
	{clear_paint,_} ->
	    update_tweak_handler(T0#tweak{st=St0#st{pst=remove_pst(StPst),shapes=hide_mag_paint(Shp)}});
	{Mode,1} when Mode =:= move; Mode =:= move_normal; Mode =:= slide;
		      Mode =:= scale; Mode =:= scale_uniform; Mode =:= relax;
		      Mode =:= paint; Mode =:= erase ->
	    set_tweak_pref(Mode, 1, {false, false, false}),
	    wings_wm:send({tweak,tweak_palette}, update_palette),
	    is_tweak_combo(T0);
	_ ->
	    keep
    end;

is_tweak_hotkey({view,Cmd}, #tweak{st=St0}) when Cmd =/= quick_preview ->
    St = wings_dl:map(fun (D, St1) ->
			      end_drag(update, D, St1)  % used to find mid tweak model data
		      end, St0),
    wings_view:command(Cmd, St),
    wings_wm:dirty(),
    keep;
is_tweak_hotkey(_, T) ->
    case wings_io:is_key_pressed(?SDLK_SPACE) of
	true -> is_tweak_combo(T);
	false -> keep
    end.

is_tweak_combo(#tweak{st=#st{selmode=body}}) ->
    keep; % Still haven't add Scale
is_tweak_combo(#tweak{mode=Mode,st=St0}=T) ->
    {B,X0,Y0} = wings_io:get_mouse_state(),
    Ctrl = wings_io:is_modkey_pressed(?CTRL_BITS),
    Shift = wings_io:is_modkey_pressed(?SHIFT_BITS),
    Alt = wings_io:is_modkey_pressed(?ALT_BITS),
    {_,TweakKeys} = wings_pref:get_value(tweak_prefs),
    case orddict:find({B,{Ctrl,Shift,Alt}},TweakKeys) of
        {ok, Mode} -> keep;
        {ok, NewMode} when element(1,Mode) =:= NewMode -> keep;
        {ok, NewMode} ->
            St = wings_dl:map(fun (D, _) ->
				      update_drag(D,T)  % used to find mid tweak model data
			      end, St0),
            do_tweak_0(0, 0, 0, 0, {move,screen}),
	    {X,Y} = wings_wm:screen2local({X0,Y0}),
            update_tweak_handler(T#tweak{mode=NewMode,st=St,ox=X,oy=Y,cx=0,cy=0});
        _ -> keep
    end.

update_drag(#dlo{src_sel={Mode,Els},src_we=#we{id=Id},drag=#drag{mm=MM}}=D0,
	    #tweak{st=#st{shapes=Shs0}=St0}=T) ->
    #dlo{src_we=We}=D1 = wings_draw:join(D0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    Center = wings_vertex:center(Vs0, We),
    {Vs,Magnet,VsDyn} = begin_magnet(T#tweak{st=St}, Vs0, Center, We),
    #dlo{src_we=We0}= D = wings_draw:split(D1, Vs, St),
    W1 = set_edge_influence(VsDyn,We0),
    {D#dlo{src_we=W1,drag=#drag{vs=Vs0,pos0=Center,pos=Center,mag=Magnet,mm=MM}},St};
update_drag(D,#tweak{st=St}) -> {D,St}.

%%%
%%% XYZ Tweak Constraints
%%%

actual_mode(Mode) ->
    axis_constraints(Mode).

axis_constraints(Mode) -> %% Mode =:= move; Mode =:= scale
    case wings_pref:get_value(tweak_axis) of
	screen ->
	    TKeys = wings_pref:get_value(tweak_xyz), % Toggled xyz constraints
	    case TKeys of
		[true,false,false] -> {Mode, x};
		[false,true,false] -> {Mode, y};
		[false,false,true] -> {Mode, z};
		[true,true,false]  -> {Mode, xy};
		[false,true,true]  -> {Mode, yz};
		[true,false,true]  -> {Mode, zx};
		[_,_,_] -> {Mode,screen}
	    end;
	Other -> {Mode,Other}
    end.

%%%
%%% Show Cursor
%%%

%% After releasing lmb to conclude drag, unhide the cursor and make sure its
%% inside the window at the centre of the selection if possible.
show_cursor(_, #dlo{src_we=#we{id=Id}, drag={matrix,Pos,_,_}}) ->
    Matrices = wings_u:get_matrices(Id, original),
    {X0,Y0,_} = obj_to_screen(Matrices, Pos),
    show_cursor_1(X0,Y0);
show_cursor(El, #dlo{src_sel={Mode,_},src_we=#we{id=Id}=We,drag=#drag{mm=MM}}) ->
    Vs0 = case catch sel_to_vs(Mode, El, We) of
	      VsList when is_list(VsList) -> VsList;
	      _ -> crash_the_next_check_too
	  end,
    Center = case catch wings_vertex:center(Vs0, We) of
		 {X,Y,Z}=C when C =:= {float(X), float(Y), float(Z)} -> C;
		 _ ->
		     {{_,_,C},_} = wings_pref:get_value(tweak_geo_point),
		     C
	     end,
    Matrices = wings_u:get_matrices(Id, MM),
    {X0,Y0,_} = obj_to_screen(Matrices, Center),
    show_cursor_1(X0,Y0);
show_cursor(_,_) ->
    {{_,_,Center},{_,Mir,{Id,_}}} = wings_pref:get_value(tweak_geo_point),
    Matrices = wings_u:get_matrices(Id, Mir),
    {X0,Y0,_} = obj_to_screen(Matrices, Center),
    show_cursor_1(X0,Y0).

show_cursor_1(X0,Y0) ->
    {W,H} = wings_wm:win_size(),
    {X1,Y1} = {trunc(X0), H - trunc(Y0)},
    X = if X1 < 0 -> 20;
	   X1 > W -> W-20;
	   true -> X1
	end,
    Y = if Y1 < 0 -> 20;
	   Y1 > H -> H-20;
	   true -> Y1
	end,
    wings_wm:release_focus(),
    wings_io:ungrab(X,Y).

%%%
%%% Main Tweak Menu
%%%

menu(X, Y) ->
    Menu = menu(),
    wings_menu:popup_menu(X, Y, tweak, Menu).

menu() ->
    ToggleHelp = ?__(1,"Tweak is a collection of tools for quickly adjusting a mesh."),
    Toggle = case wings_pref:get_value(tweak_active) of
		 false -> ?__(2,"Enable Tweak");
		 true -> ?__(3,"Disable Tweak")
	     end,
    PaintOpt =
	[separator,
	 tweak_menu_item(paint,
			?__(15,"Paint soft selections using the current magnet settings.")),
	 {?__(16,"Paint from selection"), paint_from_sel,
			?__(17,"Paint soft selection from the current selection.")},
	 {?__(18,"Paint from weight map"), paint_from_map,
			?__(19,"Paint soft selection from a weight map from the current selection.")},

	 tweak_menu_item(erase,
			?__(20,"Erase the painted soft selection using the current magnet.")),
	 {?__(21,"Clear painting"), clear_paint,
			?__(22,"Remove the painted soft selection.")}],
    [{Toggle,toggle_tweak,ToggleHelp},
     separator,
     {?__(4,"Magnets"),{tweak_magnet, tweak_magnet_menu()}},
     {?__(5,"Axis Constraints"),{axis_constraint, constraints_menu()}}] ++
    PaintOpt ++
    [separator,
     tweak_menu_item(move,
		     ?__(6,"Move selection relative to screen, or constrained to an axis.")),
     tweak_menu_item(move_normal,
		     ?__(7,"Move selection along average normal.")),
     tweak_menu_item(scale,
		     ?__(8,"Scale selection relative to screen, or constrained to an axis.")),
     tweak_menu_item(scale_uniform,
		     ?__(9,"Scale elements uniformly from the selection center.")),
     tweak_menu_item(relax,
		     ?__(10,"Relax selection toward the average plane of neighbors.")),
     tweak_menu_item(slide,
		     ?__(11,"Slide elements along adjacent edges.")),
     separator,
     {?__(13,"Tweak Preferences"),tweak_preferences,
      ?__(14,"Preferences for Tweak")}].

tweak_menu_item(Mode, Help0) ->
    Set = {bold,?__(2,"Set: ")},
    Swap = {bold,?__(3,"Swap: ")},
    SetHelp = [Set, ?__(4,"Hold modifiers and/or press any mouse button.")],
    SwapHelp = [Swap,?__(5,"Press another tool's key binding.")],
    DelHelp = [{bold, ?__(6,"Unbind: ")}, button(3)],
    Help = wings_msg:join([Help0, SetHelp, SwapHelp, DelHelp]),
    {mode(Mode),tweak_menu_fun(Mode),Help, keys_combo(Mode)}.

mode({move,_}) ->
    ?__(1,"Move");
mode(move) ->
    ?__(1,"Move");
mode({move_normal,_}) ->
    ?__(2,"Move Normal");
mode(move_normal) ->
    ?__(2,"Move Normal");
mode({scale,_}) ->
    ?__(3,"Scale");
mode(scale) ->
    ?__(3,"Scale");
mode({scale_uniform,_}) ->
    ?__(4,"Scale Uniform");
mode(scale_uniform) ->
    ?__(4,"Scale Uniform");
mode(relax) ->
    ?__(5,"Relax");
mode(slide) ->
    ?__(6,"Slide");
mode(paint) ->
    ?__(7,"Paint");
mode(erase) ->
    ?__(8,"Erase");
mode(_) ->
    init().

tweak_menu_fun(Mode) ->
    fun
	(B,_) when B < 4 -> {tweak,{Mode,B}};
	(_,_) -> ignore
		     end.

keys_combo(Key) ->
    TweakKeys = tweak_keys(),
    case lists:keyfind(Key,2,TweakKeys) of
	false -> [];
	{{Button,Modifiers},_} ->
            B = button(Button),
            Mod = modifier(Modifiers),
            [{hotkey, [Mod,B]}]
    end.

%%%
%%% Constraints Menu
%%%

constraints_menu() ->
    [Fx,Fy,Fz] = wings_pref:get_value(tweak_xyz),
    TwAx = wings_pref:get_value(tweak_axis),
    TwPt = wings_pref:get_value(tweak_point),

    N = normal,
    D = default_axis,

    NHelp = ?__(1,"Locks movement to the selection's Normal."),
    DaHelp = ?__(2,"Locks movement to the Default Axis."),
    RHelp = ?__(3,"Locks movement to the Radial of the current Tweak Axis."),
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Help = ?__(5,"Locks movement to the ~s axis."),
    [{X,x,wings_util:format(Help, [X]),crossmark(Fx)},
     {Y,y,wings_util:format(Help, [Y]),crossmark(Fy)},
     {Z,z,wings_util:format(Help, [Z]),crossmark(Fz)},
     separator,
     {wings_s:dir(N), N, NHelp, crossmark(TwAx =:= N)},
     {axis_string(D), D, DaHelp, crossmark(TwAx =:= D)},
     {axis_string(element_normal),element_normal,
      ?__(6,"Constrained movement to normal of element marked by Tweak Vector."),
      crossmark(TwAx =:= element_normal)},
     {axis_string(element_normal_edge),element_normal_edge,
      ?__(6,"Constrained movement to normal of element marked by Tweak Vector."),
      crossmark(TwAx =:= element_normal_edge)},
     separator,
     {axis_string(radial), radial, RHelp, crossmark(wings_pref:get_value(tweak_radial))},
     separator,
     {axis_string(from_cursor),from_cursor,?__(7,"Scale from mouse cursor."),
      crossmark(TwPt =:= from_cursor)},
     {axis_string(from_element),from_element,?__(8,"Scale from element marked by Tweak Vector."),
      crossmark(TwPt =:= from_element)},
     {axis_string(from_default),from_default,?__(9,"Scale from Default Point."),
      crossmark(TwPt =:= from_default)},
     separator,
     {?__(10,"Clear Constraints"),clear,?__(11,"Clear all locked axes.")}].

%%%
%%% Magnet Menu
%%%

tweak_magnet_menu() ->
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    PaintRoute = wings_pref:get_value(tweak_paint),
    Help = ?__(3,"Tweak magnets are similar to 'soft selection'."),
    Toggle = if
		 Mag  -> ?__(1,"Disable Magnet");
		 true -> ?__(2,"Enable Magnet")
	     end,
    MagAdj = {?__(5,"Radius Adjust Key"), mag_adjust,
	      ?__(6,"Press [Insert] to customize hotkey for adjusting the magnet radius. ") ++
		  ?__(7,"Without a hotkey assigned, the magnet radius is adjusted by holding [Alt].")},
    Reset = {?__(8,"Reset Radius"), reset_radius,?__(9,"Reset the magnet radius to 1.0")},
    Cycle = {?__(10,"Next Magnet Type"), cycle_magnet, ?__(11,"Can be hotkeyed to cycle through the magnets.")},
    Dome = {magnet_type(dome), dome, mag_thelp(dome),
            crossmark({dome, MagType})},
    Straight = {magnet_type(straight), straight, mag_thelp(straight),
                crossmark({straight, MagType})},
    Spike = {magnet_type(spike), spike, mag_thelp(spike),
	     crossmark({spike, MagType})},
    Shortest = {mag_route(shortest), shortest, mag_thelp(shortest),
		crossmark({shortest, PaintRoute})},
    Surface = {mag_route(surface), surface, mag_thelp(surface),
	       crossmark({surface, PaintRoute})},
    [{Toggle, toggle_magnet, Help}, separator,
     Dome, Straight, Spike, separator,
     Reset, separator,
     Cycle, MagAdj, separator,
     Shortest, Surface].

magnet_type(dome) -> ?__(1,"Dome");
magnet_type(straight) -> ?__(2,"Straight");
magnet_type(spike) -> ?__(3,"Spike").

mag_thelp(dome) -> ?__(1,"This magnet pulls and pushes geometry with an even and rounded effect.");
mag_thelp(straight) -> ?__(2,"This magnet pulls and pushes geometry with a straight effect.");
mag_thelp(spike) -> ?__(3,"This magnet pulls and pushes geometry out to a sharp point.");
mag_thelp(shortest) -> ?__(4,"The distance route will be computed by the shortest vertex distance (Paint mode only).");
mag_thelp(surface) -> ?__(5,"The distance route will be computed along the surface (Paint mode only).").


mag_route(shortest) -> ?__(1,"Distance route - Shortest");
mag_route(surface) -> ?__(2,"Distance route - Surface").

cycle_magnet() ->
    {MagBool,MagType,_} = wings_pref:get_value(tweak_magnet),
    case MagType of
	dome -> straight;
	straight -> spike;
	spike when MagBool =:= false -> dome;
	spike -> off
    end.

crossmark({MagType, MagType}) -> [{crossmark, true}];
crossmark({_, MagType}) when is_atom(MagType)-> [{crossmark, false}];
crossmark("none") -> [{crossmark, false}];
crossmark(false) -> [{crossmark, false}];
crossmark(_) -> [{crossmark, true}].


%%%
%%% Tweak Commands
%%%

command(toggle_tweak, #st{shapes=Shp0}=St) ->
    Pref = wings_pref:get_value(tweak_active),
    wings_pref:set_value(tweak_active, not Pref),
    wings_wm:send({tweak,tweak_palette}, update_palette),
    case wings_wm:is_geom() of
	true -> wings:info_line();
	false -> ok
    end,
    Shp = hide_mag_paint(Shp0),
    St#st{shapes=Shp};
command({tweak_magnet,toggle_magnet}, #st{pst=StPst,shapes=Shp}=St) ->
    toggle_magnet(),
    case wings_wm:is_geom() of
	true -> wings:info_line();
	false -> ok
    end,
    wings_wm:send({tweak,tweak_magnet}, update_palette),
    St#st{pst=remove_pst(StPst),shapes=hide_mag_paint(Shp)};
command({tweak_magnet,reset_radius}, St) ->
    Pref = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,setelement(3,Pref,1.0)),
    St;
command({tweak_magnet,cycle_magnet}, St) ->
    NewMag = cycle_magnet(),
    set_magnet_type(NewMag),
    wings_wm:send({tweak,tweak_magnet}, update_palette),
    case wings_wm:is_geom() of
	true -> wings:info_line();
	false -> ok
    end,
    St;
command({tweak_magnet,mag_adjust}, St) ->
    St;
command({tweak_magnet,PaintRoute}, St) when PaintRoute =:= shortest; PaintRoute =:= surface ->
    set_paint_route(PaintRoute),
    wings_wm:send({tweak,tweak_magnet},update_palette),
    case wings_wm:is_geom() of
	true -> wings:info_line();
	false -> ok
    end,
    St;
command({tweak_magnet,MagType}, St) ->
    set_magnet_type(MagType),
    wings_wm:send({tweak,tweak_magnet},update_palette),
    case wings_wm:is_geom() of
	true -> wings:info_line();
	false -> ok
    end,
    St;
command(tweak_preferences, St) ->
    tweak_preferences_dialog(St);
command({axis_constraint, Axis}, St) ->
    toggle_axis(Axis),
    wings_wm:send({tweak,axis_constraint},update_palette),
    St;
command({set_tweak_pref,Mode,B,Mods}, St) when B =< 3->
    set_tweak_pref(Mode, B, Mods),
    wings_wm:send({tweak,tweak_palette},update_palette),
    St;
command({tweak_palette, Cmd}, St) ->
    command(Cmd, St);
command({Mode,B}, St) when B =< 3->
    Ctrl = wx_misc:getKeyState(?WXK_CONTROL),
    Shift = wx_misc:getKeyState(?WXK_SHIFT),
    Alt = wx_misc:getKeyState(?WXK_ALT),
    set_tweak_pref(Mode, B, {Ctrl, Shift, Alt}),
    wings_wm:send({tweak,tweak_palette},update_palette),
    St;
command(clear_paint, #st{pst=StPst,shapes=Shp0}=St) ->
    Shp = clear_mag_paint(Shp0),
    St#st{pst=remove_pst(StPst),shapes=Shp};
command(paint_from_sel, St) ->
    {Mag,_,_} = wings_pref:get_value(tweak_magnet),
    if Mag -> paint_from_selection(St);
    true -> St
    end;
command(paint_from_map, St) ->
    {Mag,_,_} = wings_pref:get_value(tweak_magnet),
    if Mag -> paint_from_weightmap(St);
	true -> St
    end;
command(Mode, St) when Mode =:= move; Mode =:= move_normal; Mode =:= scale;
		       Mode =:= scale_uniform; Mode =:= slide; Mode =:= relax;
		       Mode =:= paint; Mode =:= erase ->
    set_tweak_pref(Mode, 1, {false, false, false}),
    wings_wm:send({tweak,tweak_palette},update_palette),
    St;
command(_What,_) ->
    next.

%%%
%%% Process Commands
%%%

%% Delete Tweak bind key
set_tweak_pref(Mode, 3, {false,false,false}) ->
    Cam = wings_pref:get_value(camera_mode),
    TweakKeys = case wings_pref:get_value(tweak_prefs) of
		    {Cam,TweakKeys0} ->
			lists:keydelete(Mode,2,TweakKeys0);
		    _ -> set_tweak_keys(Cam)
		end,
    wings_pref:set_value(tweak_prefs, {Cam,TweakKeys});
%% Set new Tweak bind key or swap functions if the bind keys already exist
set_tweak_pref(Mode, B, Modifiers) ->
    Cam = wings_pref:get_value(camera_mode),
    exceptions(Cam,B,Modifiers),
    Keys1 = tweak_keys(),
    Keys = case lists:keyfind(Mode,2,Keys1) of
	       false ->
		   orddict:store({B,Modifiers},Mode,Keys1);
	       {OldKey,Mode} ->
		   Keys2 = case orddict:find({B,Modifiers},Keys1) of
			       {ok, OldMode} -> orddict:store(OldKey,OldMode,Keys1);
			       error -> lists:keydelete(Mode,2,Keys1)
			   end,
		   orddict:store({B,Modifiers},Mode,Keys2)
	   end,
    wings_pref:set_value(tweak_prefs, {Cam,Keys}).

%% A bind keys that conflict with either menus or camera buttons are listed here
exceptions(wings_cam,2,{false,false,false}) -> cam_conflict();
exceptions(mirai,2,{false,false,false}) -> cam_conflict();
exceptions(nendo,2,{false,false,false}) -> cam_conflict();
exceptions(maya,_,{false,false,true}) -> cam_conflict();
exceptions(tds,2,{false,false,false}) -> cam_conflict();
exceptions(tds,2,{false,false,true}) -> cam_conflict();
exceptions(tds,2,{true,false,true}) -> cam_conflict();
exceptions(blender,2,{false,false,false}) -> cam_conflict();
exceptions(blender,2,{false,true,false}) -> cam_conflict();
exceptions(blender,2,{true,false,false}) -> cam_conflict();
exceptions(mb,1,{true,true,false}) -> cam_conflict();
exceptions(mb,1,{true,false,false}) -> cam_conflict();
exceptions(mb,1,{false,true,false}) -> cam_conflict();
exceptions(sketchup,2,{false,false,false}) -> cam_conflict();
exceptions(sketchup,2,{false,true,false}) -> cam_conflict();
exceptions(_,3,{true,false,false}) -> menu_conflict();
exceptions(_,_,_) -> ok.

-spec menu_conflict() -> no_return().
menu_conflict() ->
    wings_u:error_msg(?__(1,"Key combo was not assigned.\n
    Those keys would conflict with the right click Tweak menu.")).

-spec cam_conflict() -> no_return().
cam_conflict() ->
    wings_u:error_msg(?__(1,"Key combo was not assigned.\n
    That key combination would conflict with the Camera buttons")).

%%%
%%% Toggle Draw Routines
%%%

%% For things like Tweak Vectors. Using props should work per window.
toggle_draw(Bool) ->
				 wings_wm:set_prop(tweak_draw, Bool).

%%%
%%% Toggle Axes
%%%

toggle_axis(Axis) ->
    toggle_axis_1(Axis).

toggle_axis_1([X,Y,Z]) ->
    wings_pref:set_value(tweak_axis, screen),
    wings_pref:set_value(tweak_xyz, [X,Y,Z]);
toggle_axis_1(clear) ->
    wings_pref:set_value(tweak_xyz,[false,false,false]),
    wings_pref:set_value(tweak_point,none),
    wings_pref:set_value(tweak_axis, screen),
    wings_pref:set_value(tweak_radial, false);
toggle_axis_1(P) when P =:= from_cursor; P =:= from_element; P =:= from_default; P =:= none ->
    Pref = case wings_pref:get_value(tweak_point) of
	       P -> none;
	       _ -> P
	   end,
    wings_pref:set_value(tweak_point,Pref);
toggle_axis_1(Axis) when Axis =:= x; Axis =:= y; Axis =:= z->
    case wings_pref:get_value(tweak_xyz) of
	[X,Y,Z] ->
	    NewPref = case Axis of
			  x -> [not X, Y, Z];
			  y -> [X, not Y, Z];
			  z -> [X, Y, not Z]
		      end,
	    wings_pref:set_value(tweak_axis, screen),
	    wings_pref:set_value(tweak_xyz, NewPref);
	_ -> ok
    end;
toggle_axis_1(radial) ->
    case wings_pref:get_value(tweak_radial) of
	true ->
	    wings_pref:set_value(tweak_radial, false);
	false ->
	    wings_pref:set_value(tweak_radial, true),
	    case wings_pref:get_value(tweak_axis) of
		uniform ->
		    wings_pref:set_value(tweak_axis, screen);
		_ -> ok
	    end
    end;
toggle_axis_1(Axis) ->
    wings_pref:set_value(tweak_xyz,[false,false,false]),
    case wings_pref:get_value(tweak_axis) of
	Axis ->
	    wings_pref:set_value(tweak_axis,screen);
	_otherwise ->
	    wings_pref:set_value(tweak_axis, Axis)
    end.

%%%
%%% Toggle Magnets
%%%

toggle_magnet() ->
    {Mag, MagType, MagRad} = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,{not Mag, MagType, MagRad}).

set_magnet_type(off) -> %% for cycling magnets
    {_, MagType, MagRad} = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,{false, MagType, MagRad});
set_magnet_type(MagType) ->
    {_, _, MagRad} = wings_pref:get_value(tweak_magnet),
    wings_pref:set_value(tweak_magnet,{true, MagType, MagRad}).

save_magnet_prefs(#tweak{magnet=Mag, mag_type=MT, mag_rad=MagR}) ->
    wings_pref:set_value(tweak_magnet, {Mag, MT, MagR}).

%%%
%%% Tweak Preference Dialog
%%%

tweak_preferences_dialog(St) ->
    ClkSpd = wings_pref:get_value(tweak_click_speed)/100000,
    MagAdj = wings_pref:get_value(tweak_mag_adj_sensitivity)*100.0,
    VecSize = wings_pref:get_value(tweak_vector_size),
    VecWidth = wings_pref:get_value(tweak_vector_width),
    TweakSpeed = wings_pref:get_value(tweak_speed),
    Menu = [{vframe,
	     [{hframe,[{slider,{text,ClkSpd,[{key,tweak_click_speed},{range,{1.0,5.0}}]}}],
	       [{title,?__(1,"Click Speed for Select/Deselect")}]},

	      {vframe,[{hframe,[{slider,{text,TweakSpeed,[{key,tweak_speed},{range,{0.01,1.0}}]}}]},
		       {label,?__(2,"Lower to increase control or raise to increase speed.")}],
	       [{title,?__(3,"Tweak Speed (Drag Response)")}]},

	      {hframe,[{slider,{text,MagAdj,[{key,tweak_mag_adj_sensitivity},{range,{0.1,2.0}}]}}],
	       [{title,?__(4,"Magnet Radius Adjustment Sensitivity")}]},

	      {label_column,[{color,?__(5,"Magnet Radius Display Color"),tweak_magnet_color}]},
	      {?__(11,"Show Magnet influence"),tweak_magnet_influence},
	      {hframe,
	       [{vframe,[{label,?__(6,"Length")},
			 {label,?__(7,"Width")},
			 {label,?__(8,"Color")}]},
		{vframe,[{text,VecSize,[{key,tweak_vector_size},{range,{0.1,10.0}}]},
			 {text,VecWidth,[{key,tweak_vector_width},{range,{1.0,10.0}}]},
			 {color,tweak_vector_color}]}],
	       [{title,?__(9,"Tweak Vector")}]}
	     ]}],
    PrefQs = [{Lbl, make_query(Ps)} || {Lbl, Ps} <- Menu],
    wings_dialog:dialog(?__(10,"Tweak Preferences"), PrefQs,
			fun(Result) -> set_values(Result), St end).

make_query([_|_]=List)  ->
    [make_query(El) || El <- List];
make_query({[_|_]=Str,Key}) ->
    case wings_pref:get_value(Key) of
	Def when Def =:= true; Def =:= false ->
	    {Str,Def,[{key,Key}]};
	Def ->
	    {Str,{text,Def,[{key,Key}]}}
    end;
make_query({color,Key}) ->
    Def = wings_pref:get_value(Key),
    {color,Def,[{key,Key}]};
make_query({color,[_|_]=Str,Key}) ->
    Def = wings_pref:get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

set_values([{tweak_click_speed = Key, Value}|Result]) ->
    wings_pref:set_value(Key, Value*100000),
    set_values(Result);
set_values([{tweak_mag_adj_sensitivity = Key, Value}|Result]) ->
    wings_pref:set_value(Key, Value/100.0),
    set_values(Result);

set_values([{Key,Value}|Result]) ->
    wings_pref:set_value(Key, Value),
    set_values(Result);
set_values([]) -> ok.

%%%
%%% Tweak Mode Active Info Line
%%%

tweak_info_line() ->
    M1 = ?__(1,"L: Click Select"),
    M2 = ?__(2,"LL: Paint Select"),
    M3 = wings_msg:button_format([], [], ?__(4,"Menu")),
    M4 = wings_msg:rmb_format(?__(3,"Tweak Menu")),
    Message = wings_msg:join([M1,M2,M3,M4]),
    wings_wm:message(Message).

%% Go through list of user defined keys and tweak tool, and build an info line
tweak_keys_info() ->
    This = wings_wm:is_geom(),
    case wings_wm:lookup_prop(tweak_draw) of
	{value,true} when This ->
	    case wings_pref:get_value(tweak_active) of
		true ->
		    Keys = tweak_keys(),
		    Msg = compose_info_line(Keys),
		    draw_tweak_info_line(Msg);
		_ -> ok
	    end;
	_ -> ok
    end.

compose_info_line([{{Button,Modifiers},Mode}|D]) ->
    Mod = modifier(Modifiers),
    B = button(Button) ++ ": ",
    case mode(Mode) of
	true -> compose_info_line(D);
	M ->
	    Message = [Mod,B,M],
	    wings_msg:join([Message,compose_info_line(D)])
    end;
compose_info_line([]) -> [].

button(1) -> wings_s:lmb();
button(2) -> wings_s:mmb();
button(3) -> wings_s:rmb().

modifier({Ctrl,Shift,Alt}) ->
    C = if Ctrl -> [wings_s:key(ctrl),"+"]; true -> [] end,
    S = if Shift -> [wings_s:key(shift),"+"]; true -> [] end,
    A = if Alt -> [wings_s:key(alt),"+"]; true -> [] end,
    [C,S,A].

draw_tweak_info_line(Msg) ->
    {_,H} = wings_wm:win_size(),
    wings_io:info(0, H-?LINE_HEIGHT-3, wings_msg:join([Msg])).

%%%
%%% XYZ Constraints Info line while Tweaking
%%%

statusbar() ->
    Active = wings_pref:get_value(tweak_active),
    Draw = wings_wm:lookup_prop(tweak_draw) =:= {value,true},
    Geom = wings_wm:is_geom(),
    case Active andalso Draw andalso Geom of
	true ->
	    TweakAxis = wings_pref:get_value(tweak_axis),
	    TweakPoint = wings_pref:get_value(tweak_point),
	    RadialAxis = wings_pref:get_value(tweak_radial),
	    Hotkeys = wings_hotkey:matching([axis_constraint,tweak]),
	    TKeys = wings_pref:get_value(tweak_xyz),
	    XYZHelp = xyzkey_help(TKeys, Hotkeys),
	    Normal = toggle_bold(TweakAxis, normal, Hotkeys),
	    DefaultAxis = toggle_bold(TweakAxis, default_axis, Hotkeys),
	    ElementNormal = toggle_bold(TweakAxis, element_normal, Hotkeys),
	    ElementNormalE = toggle_bold(TweakAxis, element_normal_edge, Hotkeys),
	    Radial = toggle_bold(RadialAxis, radial, Hotkeys),
	    FromCursor = toggle_bold(TweakPoint, from_cursor, Hotkeys),
	    FromElement = toggle_bold(TweakPoint, from_element, Hotkeys),
	    DefaultPoint = toggle_bold(TweakPoint, from_default, Hotkeys),
	    Help = wings_msg:join([XYZHelp,Normal,DefaultAxis,ElementNormal,
				   ElementNormalE,Radial,
				   FromCursor,FromElement,DefaultPoint]),
	    wings_msg:join([Help]);
	false -> []
    end.

toggle_bold(Axis0, Axis, Hotkeys) ->
    case matching_hotkey(Axis, Hotkeys) of
	[] -> [];
	{none,Name} when Axis0 =:= from_default andalso Axis0 =:= Axis;
			 Axis0 =:= from_cursor andalso Axis0 =:= Axis;
			 Axis0 =:= from_element andalso Axis0 =:= Axis ->
	    [?__(1,"Active Point: "),{bold,Name}];
	{none,Name} when Axis0 =:= Axis; Axis0 ->
	    [?__(2,"Active Axis: "),{bold,Name}];
	Keys when Axis0 =:= Axis; Axis0 ->
	    [wings_hotkey:format_hotkey(Keys, pretty),": ", {bold,axis_string(Axis)}];
	{none,_} ->
	    [];
	Keys ->
	    [wings_hotkey:format_hotkey(Keys, pretty),": ", axis_string(Axis)]
    end.

xyzkey_help({_,_}, _) -> [];
xyzkey_help([X,Y,Z], Hotkeys) ->
    XStr = axis_string(x),
    YStr = axis_string(y),
    ZStr = axis_string(z),
    StrX = if X -> [{bold,XStr}]; true -> XStr end,
    StrY = if Y -> [{bold,YStr}]; true -> YStr end,
    StrZ = if Z -> [{bold,ZStr}]; true -> ZStr end,
    XKeys = case matching_hotkey(x, Hotkeys) of
		{none,_} when X -> [?__(1,"Active Axis: "),StrX];
		{none,_} -> [];
		Xkey -> ["[",wings_hotkey:format_hotkey(Xkey, pretty),"]: ",StrX]
	    end,
    YKeys = case matching_hotkey(y, Hotkeys) of
		{none,_} when Y -> [?__(1,"Active Axis: "),StrY];
		{none,_} -> [];
		Ykey -> ["[",wings_hotkey:format_hotkey(Ykey, pretty),"]: ",StrY]
	    end,
    ZKeys = case matching_hotkey(z, Hotkeys) of
		{none,_} when Z -> [?__(1,"Active Axis: "),StrZ];
		{none,_} -> [];
		Zkey -> ["[",wings_hotkey:format_hotkey(Zkey, pretty),"]: ",StrZ]
	    end,
    wings_msg:join([XKeys,YKeys,ZKeys]).

matching_hotkey(Match, Hotkeys) ->
    case lists:keyfind(Match, 1, Hotkeys) of
	{_,Keys} -> Keys;
	false -> {none,axis_string(Match)}
    end.

axis_string(Axis) ->
    case Axis of
	default_axis -> ?__(1,"Default Axis");
	element_normal -> ?__(2,"Element Normal");
	element_normal_edge -> ?__(3,"Element Normal (edges)");
	radial -> ?__(4,"Radial");
	from_cursor -> ?__(5,"From Cursor");
	from_element -> ?__(6,"From Element");
	from_default -> ?__(7,"From Default Point");
	Other -> wings_s:dir(Other)
    end.

%%%
%%% Info Line while Tweaking
%%%

info_line(Mode) ->
    M = mode(Mode),
    Cam = camera_msg(),
    Spc = spacebar_msg(),
    Extra = extra_msg(Mode),
    Help = wings_msg:join([M, Cam, Spc, Extra]),
    wings_wm:message(Help).

camera_msg() ->
    ?__(1,"[C]: Tumble camera  [S]: Pan  [D]: Dolly").

spacebar_msg() ->
    ?__(1,"[Space]: Switch to Tweak tool assigned to L").

extra_msg(slide) ->
    "[F1]: " ++ ?__(1,"Slide Collapse");
extra_msg(_) -> [].

%%%
%%% Right Side Info Line including Magnets and Tweak: Disabled
%%%

tweak_disabled_msg() ->
    wings_wm:message_right(?__(1,"Tweak: Disabled")).

tweak_magnet_help() ->
    {Mag, MagType, _} = wings_pref:get_value(tweak_magnet),
    Message = if
		  Mag ->
		      Hotkeys = wings_hotkey:matching([tweak_magnet,tweak]),
		      MKey = case lists:keyfind(mag_adjust, 1, Hotkeys) of
				 {_, Keys} -> "[" ++ wings_hotkey:format_hotkey(Keys, pretty) ++ "]";
				 false -> wings_s:key(alt)
			     end,
		      M1 = [?__(1,"Magnet: "),magnet_type(MagType)],
		      M2 = [MKey, ?__(3,"+Drag: "), ?__(4,"Adjust Radius")],
		      wings_msg:join([M1,M2]);
		  true -> ?__(2,"Magnet: Off")
	      end,
    wings_wm:message_right(Message).
tweak_magnet_radius_help(true) ->
    wings_wm:message(?__(1,"Drag right to increase and left to decrease the magnet radius."));
tweak_magnet_radius_help(false) ->
    wings_wm:message(?__(2,"Magnet is currently off.")).

%%%
%%% Support for highlighting the magnet influence
%%%

%% This function will clean the vertices influence information when the list is empty or
%% it will add the vertices influence information to Pst field of the we#
set_edge_influence([],#we{pst=Pst}=We) ->
    We#we{pst=remove_pst(Pst)};
set_edge_influence(VsDyn,#we{pst=Pst}=We) ->
    case wings_pref:get_value(tweak_magnet_influence) of
	true -> We#we{pst=add_pst(VsDyn,Pst)};
	_ -> We
    end.

%% It adds the plugin functionality
add_pst(InfData,Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
	none ->
	    Data = gb_trees:empty(),
	    NewData = gb_trees:insert(?MAG_INFO,InfData,Data),
	    gb_trees:insert(?MODULE,NewData,Pst);
	{_,Data} ->
	    NewData = gb_trees:enter(?MAG_INFO,InfData,Data),
	    gb_trees:update(?MODULE,NewData,Pst)
    end.

%% It removes the plugin functionality
remove_pst(none) -> none;
remove_pst(Pst) ->
    gb_trees:delete_any(?MODULE,Pst).

hide_mag_paint(Shapes) ->
    TweakActive = wings_pref:get_value(tweak_active),
    {MagActive, _, _} = wings_pref:get_value(tweak_magnet),
    case MagActive and TweakActive of
	false -> clear_mag_paint(Shapes);
	true ->  Shapes
    end.

clear_mag_paint(none) -> none;
clear_mag_paint(Shapes) ->
    gb_trees:map(fun(_, #we{pst=Pst}=We0)->
		    case gb_trees:lookup(?MODULE, Pst) of
			none -> We0;
			_ -> We0#we{pst=remove_pst(Pst)}
		    end
		 end, Shapes).
%%%
%%% Functions to produce the visual effect (inspired by wpc_magnet_mask)
%%%
%% tweak actions - dynamic vertices
update_dlist({?MAG_INFO,PaintData},D, St) when is_list(PaintData) ->
    update_dlist({?MAG_INFO,{none,PaintData}},D, St);
%% Paint mode - cursor position and dynamic vertices
update_dlist({?MAG_INFO,{_,VsDyn}=PaintData},
	     #dlo{plugins=Pdl,src_we=#we{vp=Vtab}}=D, _) when is_list(VsDyn) ->
    Key = ?MODULE,
    case VsDyn of
	[] ->
	    D#dlo{plugins=[{Key,none}|Pdl]};
	_ ->
	    Draw = paint_fun(PaintData, Vtab),
	    D#dlo{plugins=[{Key,Draw}|Pdl]}
    end;
update_dlist({?MAG_INFO,{CurPos,VsDyn}}, D, St) ->
    update_dlist({?MAG_INFO,{CurPos,gb_trees:to_list(VsDyn)}},D, St).

paint_fun({CurPos,DynVs}, Vtab) ->
    {Cx,Cy,Cz} = wings_pref:get_value(selected_color),
    ColFrom = col_to_vec(wings_pref:get_value(edge_color)),
    ColTo = col_to_vec(wings_pref:get_value(tweak_magnet_color)),
    ColRange=e3d_vec:sub(ColTo,ColFrom),

    CurBin =
	case CurPos of
	    {Px,Py,Pz} -> <<Px:?F32,Py:?F32,Pz:?F32,Cx:?F32,Cy:?F32,Cz:?F32>>;
	    none -> <<>>
	end,
    EdBin0 = pump_vertex(DynVs,Vtab,ColFrom,ColRange),
    EdBin = <<EdBin0/binary,CurBin/binary>>,
    ArSize = byte_size(EdBin),
    [Vbo] = gl:genBuffers(1),
    N = ArSize div 24, % [vetex,color] - interlieved array
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, ArSize, EdBin, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    D = fun() ->
	    gl:depthFunc(?GL_LEQUAL),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
	    gl:vertexPointer(3, ?GL_FLOAT, 24, 0),
	    gl:colorPointer(3, ?GL_FLOAT, 24, 12),
	    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
	    gl:enableClientState(?GL_VERTEX_ARRAY),
	    gl:enableClientState(?GL_COLOR_ARRAY),
	    gl:drawArrays(?GL_POINTS, 0, N),
	    gl:disableClientState(?GL_COLOR_ARRAY),
	    gl:disableClientState(?GL_VERTEX_ARRAY),
	    gl:depthFunc(?GL_LESS)
	end,
    {call,D,[{vbo,Vbo}]}.

%% pumping vertex
pump_vertex(DynVs, Vtab, ColFrom, ColRange) ->
    lists:foldl(fun({V,Inf}, Acc) ->
		    case array:get(V, Vtab) of
			undefined -> Acc;
			{X,Y,Z} ->
			    {R,G,B} = color_gradient(ColFrom,ColRange,Inf),
			    <<X:?F32,Y:?F32,Z:?F32,R:?F32,G:?F32,B:?F32,Acc/binary>>
		    end
		end, <<>>, DynVs).

%% It'll will provide the vertices data for 'update_dlist' function
get_data(update_dlist, Data, Acc) ->  % for draw lists
    case gb_trees:lookup(?MAG_INFO, Data) of
        none ->
            {ok, Acc};
        {_,EdgeInfo} ->
            {ok, [{plugin, {?MODULE, {?MAG_INFO, EdgeInfo}}}|Acc]}
    end.

%% It'll use the list prepared by 'update_dlist' function and then draw it (only for plain draw)
draw(plain, EdgeList, _D, SelMode) ->
    gl:lineWidth(edge_width(SelMode)),
    wings_dl:call(EdgeList);
draw(_,_,_,_) -> ok.

edge_width(edge) -> wings_pref:get_value(edge_width);
edge_width(_) -> 1.

col_to_vec({R,G,B}) when is_integer(R) -> {R/255.0,G/255.0,B/255.0};
col_to_vec({_,_,_}=Col) -> Col;
col_to_vec({R,G,B,_}) when is_integer(R) -> col_to_vec({R,G,B});
col_to_vec({R,G,B,_}) -> col_to_vec({R,G,B}).

color_gradient(Cb, Cr, Perc) ->
    e3d_vec:add_prod(Cb, Cr, Perc).


%%%
help_msg() ->
    [help_msg_basic(),
     help_msg_magnet(),
     help_msg_axes(),
     help_msg_keys(),
     help_msg_using_keys(),
     help_msg_hotkeys(),
     help_msg_palette()].

help_msg_basic() ->
    [{bold,?__(1,"--What is Tweak?--")},"\n",
     ?__(2,"Tweak lets you edit a model quickly by clicking and dragging geometry."),"\n",
     ?__(3,"While Tweak is enabled, all the regular Wings commands are still available,"),
     ?__(4," but certain mouse buttons and modifier keys will activate the Tweak tools."),cr()].

help_msg_magnet() ->
    [{bold,?__(1,"--Magnets--")},"\n",
     {bullet,
      [?__(2,"Magnets allow for soft selection."),
       ?__(3,"There are 3 magnet types: Dome, Straight, and Spike."),
       ?__(4,"Adjust the Magnet's Radius by holding [Alt] and moving the mouse."),
       ?__(5,"Commands for switching magnets can be assigned to hotkeys in the Tweak Menu|Magnets."),
       ?__(6,"Magnet options can be set in the Tweak Preferences.")]}].

help_msg_axes() ->
    [{bold,?__(1,"--Axis Constraints--")},"\n",
     {bullet,
      [?__(2,"Axis Constraints only affect Tweak Move and Scale operations."),
       ?__(3,"[F1], [F2], and [F3] can be held to constrain movement to the 3 cardinal axes, X, Y, and Z."),
       ?__(4,"Click axis constraint hotkeys to toggle them."),"\n",
       ?__(5,"Any of the axes can be toggled to stay on via the Tweak Menu or the Tweak Palette."),
       ?__(6,"The From Element and From Point axes activate the Tweak Vector."),
       ?__(7,"Axes can be combined.")]}].

help_msg_keys() ->
    C = ", ",
    TweakTools = "("++mode(move)++C++mode(move_normal)++C++mode(scale)++C++
	mode(scale_uniform)++C++mode(relax)++C++?__(3," and ")++
	mode(slide)++")",
    Str = io_lib:format(?__(2,"Each of the Tweak Tools ~s can be assigned a modifier key combination (Ctrl, Shift, Alt)."),[TweakTools]),
    [{bold,?__(1,"--Assigning Tweak Keys--")},"\n",
     Str,"\n",
     ?__(4,"To assign a key combination to a Tweak Tool:"),"\n",
     ?__(5,"\t1) Open the Tweak Menu."),"\n",
     ?__(6,"\t2) Highlight one of the Tweak Tools."),"\n",
     ?__(7,"\t3) Press and hold the modifier key combination and/or press the activating mouse button."),cr()].

help_msg_using_keys() ->
    [{bold,?__(1,"--Using Tweak--")},"\n",
     ?__(2,"1) Enabled Tweak."),"\n",
     ?__(3,"2) Highlight or select one or more elements of geometry."),"\n",
     ?__(4,"3) Press the Tweak Key combination and associated mouse button to activate Tweak."),"\n",
     ?__(5,"4) Drag geometry to it new position."),"\n",
     ?__(6,"5) Release the mouse button to complete the Tweak."),cr(),
     ?__(7,"Releasing the Tweak Keys will not end the Tweak event."),
     ?__(8,"This allows other hotkeys to be used in mid-tweak to activate another Tweak tool, an axis constraint, aim the camera, or adjust the magnet radius."),"\n",
     ?__(9,"Pressing [C] in mid-tweak tumbles the camera."),"\n",
     ?__(10,"Pressing [S] or the Arrow Keys in mid-tweak pans the camera."),"\n",
     ?__(11,"Pressing [D] in mid-tweak dollies the camera."),"\n",
     ?__(12,"Pressing [Spacebar] in mid-tweak switches to the Tweak Tool assigned to the Lmb."),"\n",
     ?__(13,"Holding [F] while finalizing a Slide operation collapses all newly created short edges."),cr()].

help_msg_hotkeys() ->
    [{bold,?__(1,"--Hotkeyed Tweak Tools--")},"\n",
     ?__(2,"An alternative to usings Tweak Key combinations is to assign hotkeys to the various Tweak Tools usings the [Insert] method."),"\n",
     ?__(3,"Pressing the hotkey assigns that Tweak Tool to Lmb."),cr()].

help_msg_palette() ->
    [{bold,?__(1,"--Tweak Palette (Window|Tweak Palette)--")},"\n",
     ?__(2,"The Tweak Palette is a group of 3 windows that contain the main commands from the Tweak Menu."),"\n",
     ?__(3,"Use the Tweak Palette to switch between Tweak Tools, Magnet Types, or Axis Constraints.")].

cr() -> "\n\n".
