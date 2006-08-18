%%
%%  wings_camera.erl --
%%
%%     This module handles camera moves (rotation, zooming, and panning).
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_camera.erl,v 1.114 2005/01/31 14:34:11 dgud Exp $
%%

-module(wings_camera).
-export([init/0,prefs/0,help/0,event/2,event/3]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2,map/2,foldl/3,sort/1,reverse/1,append/1]).

-define(ZOOM_FACTOR, 20).
-define(CAMDIV, 4).
-define(CAMMAX, 150).  %% Always larger than 300 on my pc

-define(CSEP, 160).				%Short space.

-record(camera,
	{x,y,					%Current mouse position.
	 ox,oy,					%Original mouse position.
	 xt=0,yt=0				%Last warp length.
	}).

-record(state, {st, func}).

init() ->
    wings_pref:set_default(camera_mode, mirai),
    wings_pref:set_default(num_buttons, 3),
    wings_pref:set_default(pan_speed, 25),
    wings_pref:set_default(inverted_wheel_zoom, false),
    case {wings_pref:get_value(num_buttons),wings_pref:get_value(camera_mode)} of
	{3,_} -> ok;
	{_,nendo} -> ok;
	{_,blender} -> ok;
	{_,_} -> wings_pref:set_value(camera_mode, nendo)
    end.
    
prefs() ->
    ZoomFlag0 = wings_pref:get_value(wheel_zooms, true),
    ZoomFactor0 = wings_pref:get_value(wheel_zoom_factor, ?ZOOM_FACTOR),
    PanSpeed0 = wings_pref:get_value(pan_speed),
    InvertZW = wings_pref:get_value(inverted_wheel_zoom),
    Hook = {hook,fun (is_disabled, {_Var,_I,Sto}) ->
			 not gb_trees:get(wheel_zooms, Sto);
		     (_, _) -> void
		 end},
    {vframe,
     [{vframe,[mouse_buttons()],[{title,?__(1,"Mouse Buttons")}]},
      {vframe,[camera_modes()],[{title,?__(2,"Camera Mode")}]},
      {vframe,
       [{hframe,[{slider,{text,PanSpeed0,[{key,pan_speed},{range,{1,100}}]}}]}],
       [{title,?__(3,"Pan Speed")}]},
      {vframe,
       [{?__(4,"Wheel Zooms"),ZoomFlag0,[{key,wheel_zooms}]},
	{vradio,[{?__(5,"Forwards Zooms In"),false},
		 {?__(6,"Forwards Zooms Out"),true}],
	 InvertZW,
	 [{key,inverted_wheel_zoom},Hook]},
	{hframe,
	 [{label,?__(7,"Zoom Factor"),[Hook]},
	  {text,ZoomFactor0,
	   [{key,wheel_zoom_factor},
	    {range,{1,50}},
	    Hook]},
	  {label,"%",[Hook]}]} ],[{title,?__(9,"Scroll Wheel")}] } ]}.

mouse_buttons() ->
    {menu,[{desc(1),1,[{info,info(1)}]},
	   {desc(2),2,[{info,info(2)}]},
	   {desc(3),3,[{info,info(3)}]}],
     wings_pref:get_value(num_buttons),
     [{key,num_buttons},
      {hook,fun (update, {Var,_I,Val,Sto0}) ->
		    Sto = gb_trees:update(Var, Val, Sto0),
		    Mode0 = gb_trees:get(camera_mode, Sto),
		    Mode = case {Val,Mode0} of
			       {1,_} -> nendo;
			       {2,blender} -> blender;
			       {2,_} -> nendo;
			       {3,_} -> Mode0
			   end,
		    {store,gb_trees:update(camera_mode, Mode, Sto)};
		(_, _) -> void
	    end}]}.

camera_modes() ->
    Modes = [mirai,nendo,maya,tds,blender,mb],
    {menu,[{desc(Mode),Mode,[{info,info(Mode)}]} || Mode <- Modes],
     wings_pref:get_value(camera_mode),
     [{key,camera_mode},
      {hook,fun (menu_disabled, {_Var,_I,Sto}) ->
 		    case gb_trees:get(num_buttons, Sto) of
 			1 -> [mirai,maya,tds,blender,mb];
			2 -> [mirai,maya,tds,mb];
 			3 -> []
 		    end;
		(_, _) -> void
	    end}]}.

desc(1) -> ?__(1,"One");
desc(2) -> ?__(2,"Two");
desc(3) -> ?__(3,"Three");
desc(Other) -> wings_s:camera_mode(Other).

info(1) ->
    ?__(1,"Note: Only the Nendo camera mode can be used with a one-button mouse");
info(2) ->
    ?__(2,"Note: Only the Nendo and Blender camera modes can be used with a two-button mouse");
info(3) -> "";
info(nendo) -> "";
info(blender) ->
    [?__(3,"Note: The "),desc(blender),
     ?__(4," camera mode requires at least 2 mouse buttons")];
info(Mode) ->
    [?__(5,"Note: The "),desc(Mode),
     ?__(6," camera mode requires 3 mouse buttons")].

help() ->
    case wings_pref:get_value(camera_mode) of
	blender -> blender_help();
	nendo -> wings_msg:button_format([], ?__(1,"Start camera"));
	mirai -> wings_msg:button_format([], ?__(1,"Start camera"));
	tds -> tds_help();
	maya -> maya_help();
	mb -> mb_help()
    end.

%% Event handler.
event(Ev, St=#st{}) -> 
    event(Ev,St,none).

event(#mousebutton{button=4,state=?SDL_RELEASED}, _, _Redraw) ->
    zoom_step(-1);
event(#mousebutton{button=5,state=?SDL_RELEASED}, _, _Redraw) ->
    zoom_step(1);
event(#mousebutton{button=B}, _, _Redraw) when B==4; B==5 ->
    keep;
event(Ev, St, Redraw) ->
    case wings_pref:get_value(camera_mode) of
	blender -> blender(Ev, #state{st=St, func=Redraw});
	nendo -> nendo(Ev, #state{st=St, func=Redraw});
	mirai -> mirai(Ev, #state{st=St, func=Redraw});
	tds -> tds(Ev, #state{st=St, func=Redraw});
	maya -> maya(Ev, #state{st=St, func=Redraw});
	mb -> mb(Ev, #state{st=St, func=Redraw})
    end.

%%%
%%% Blender style camera.
%%%

blender(#mousebutton{button=2,state=?SDL_PRESSED,x=X0,y=Y0,mod=Mod}, Redraw)
  when Mod band ?ALT_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(blender_help()),
    {seq,push,get_blender_event(Camera, Redraw)};
blender(_, _) -> next.

blender_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _Redraw) ->
    stop_camera(Camera);
blender_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Mod of
	Mod when Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx, Dy);
	Mod when Mod band ?CTRL_BITS =/= 0 ->
	    zoom(Dy);
	_Other ->
	    rotate(Dx, Dy)
    end,
    wings_wm:dirty(),
    get_blender_event(Camera, Redraw);
blender_event(Other, Camera, Redraw) ->
    generic_event(Other, Camera, Redraw).

get_blender_event(Camera, Redraw) ->
    {replace,fun(Ev) -> blender_event(Ev, Camera, Redraw) end}.
	    
blender_help() ->
    TrackDolly = [{?SHIFT_BITS,2,?STR(mode_help,1,"Track")},
		  {?CTRL_BITS,2,?STR(mode_help,2,"Dolly")}],
    case allow_rotation() of
	false -> format(TrackDolly);
	true -> format([{0,2,?STR(mode_help,3,"Tumble")}|TrackDolly])
    end.

%%%
%%% Nendo style camera.
%%%

nendo(#mousebutton{button=2,x=X0,y=Y0,mod=Mod,state=?SDL_RELEASED}, Redraw)
  when Mod band ?CTRL_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    MoveTumbles = allow_rotation(),
    nendo_message(MoveTumbles),
    {seq,push,get_nendo_event(Camera, Redraw, MoveTumbles)};
nendo(#keyboard{sym=Sym}, _Redraw) ->
    nendo_pan(Sym);
nendo(_, _) -> next.

nendo_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _, _) ->
    stop_camera(Camera);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, true) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    rotate(-Dx, -Dy);
	_Other ->				%MMB and/or RMB pressed.
	    zoom(Dy)
    end,
    get_nendo_event(Camera, Redraw, true);
nendo_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, false) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 6 of
	0 ->					%None of MMB/RMB pressed.
	    pan(-Dx, -Dy);
	_Other ->				%MMB and/or RMB pressed.
	    zoom(Dy)
    end,
    get_nendo_event(Camera, Redraw, false);
nendo_event(#keyboard{unicode=$q}, Camera, Redraw, MR0) ->
    MR = MR0 xor allow_rotation(),
    nendo_message(MR),
    get_nendo_event(Camera, Redraw, MR);
nendo_event(#keyboard{sym=Sym}=Event, Camera, Redraw, _) ->
    case nendo_pan(Sym) of
	keep -> keep;
	next -> view_hotkey(Event, Camera, Redraw)
    end;
nendo_event(Event, Camera, Redraw, _) ->
    generic_event(Event, Camera, Redraw).
    
nendo_pan(?SDLK_LEFT) ->
    nendo_pan(0.5, 0.0);
nendo_pan(?SDLK_RIGHT) ->
    nendo_pan(-0.5, 0.0);
nendo_pan(?SDLK_UP) ->
    nendo_pan(0.0, 0.5);
nendo_pan(?SDLK_DOWN) ->
    nendo_pan(0.0, -0.5);
nendo_pan(_) -> next.

nendo_pan(Dx, Dy) ->
    pan(Dx, Dy),
    wings_wm:dirty(),
    keep.
    
get_nendo_event(Camera, Redraw, MouseRotates) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> nendo_event(Ev, Camera, Redraw, MouseRotates) end}.

nendo_message(true) ->
    Help = wings_msg:join([wings_msg:button_format(wings_s:accept(),
						   ?STR(message,2,"Drag to Dolly")),
			   ?STR(message,3,"Move mouse to tumble"),
			   [?STR(message,4,"[Q]"),?CSEP,
			    ?STR(message,5,"Move mouse to track")]]),
    message(Help);
nendo_message(false) ->
    QText = case allow_rotation() of
		false -> [];
		true -> [?STR(message,4,"[Q]"),?CSEP,
			 ?STR(message,3,"Move mouse to tumble")]
	    end,
    Help = wings_msg:join([wings_msg:button_format(wings_s:accept(),
						   ?STR(message,2,"Drag to Dolly")),
			   ?STR(message,7,"Restore view")|QText]),
    message(Help).

%%%
%%% Mirai style camera.
%%%

mirai(#mousebutton{button=2,x=X0,y=Y0,mod=Mod,state=?SDL_RELEASED}, Redraw)
  when Mod band ?CTRL_BITS =:= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    MoveTumbles = allow_rotation(),
    mirai_message(MoveTumbles),
    View = wings_view:current(),
    {seq,push,get_mirai_event(Camera, Redraw, MoveTumbles, View)};
mirai(#keyboard{sym=Sym}, _Redraw) ->
    mirai_pan(Sym);
mirai(_, _) -> next.

mirai_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _, _, _) ->
    stop_camera(Camera);
mirai_event(#mousebutton{button=3,state=?SDL_RELEASED}, Camera, _, _, View) ->
    wings_view:set_current(View),
    stop_camera(Camera);
mirai_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, true, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 2 of
	0 ->					%MMB not pressed.
	    rotate(-Dx, -Dy);
	_Other ->				%MMB pressed.
	    zoom(Dy)
    end,
    get_mirai_event(Camera, Redraw, true, View);
mirai_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw, false, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    case Buttons band 2 of
	0 ->					%MMB pressed.
	    pan(-Dx, -Dy);
	_Other ->				%MMB pressed.
	    zoom(Dy)
    end,
    get_mirai_event(Camera, Redraw, false, View);
mirai_event(#keyboard{unicode=$q}, Camera, Redraw, MR0, View) ->
    MR = MR0 xor allow_rotation(),
    mirai_message(MR),
    get_mirai_event(Camera, Redraw, MR, View);
mirai_event(#keyboard{sym=Sym}=Event, Camera, Redraw, _, _) ->
    case mirai_pan(Sym) of
	keep -> keep;
	next -> view_hotkey(Event, Camera, Redraw)
    end;
mirai_event(Event, Camera, Redraw, _, _) ->
    generic_event(Event, Camera, Redraw).
    
mirai_pan(?SDLK_LEFT) ->
    mirai_pan(0.5, 0.0);
mirai_pan(?SDLK_RIGHT) ->
    mirai_pan(-0.5, 0.0);
mirai_pan(?SDLK_UP) ->
    mirai_pan(0.0, 0.5);
mirai_pan(?SDLK_DOWN) ->
    mirai_pan(0.0, -0.5);
mirai_pan(_) -> next.

mirai_pan(Dx, Dy) ->
    pan(Dx, Dy),
    wings_wm:dirty(),
    keep.
    
get_mirai_event(Camera, Redraw, MouseRotates, View) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> mirai_event(Ev, Camera, Redraw, MouseRotates, View) end}.

mirai_message(true) ->
    Help = wings_msg:join([wings_msg:button_format(wings_s:accept(),
				   ?STR(message,2,"Drag to Dolly"),
				   ?STR(message,6,"Cancel/restore view")),
		     ?STR(message,3,"Move mouse to tumble"),
		     [?STR(message,4,"[Q]"),?CSEP,
		      ?STR(message,5,"Move mouse to track")]]),
    message(Help);
mirai_message(false) ->
    QText = case allow_rotation() of
		false -> [];
		true -> [?STR(message,4,"[Q]"),?CSEP,?STR(message,3,"Move mouse to tumble")]
	    end,
    Help = wings_msg:join([wings_msg:button_format(wings_s:accept(),
						   ?STR(message,2,"Drag to Dolly"),
						   ?STR(message,6,"Cancel/restore view")),
		     ?STR(message,5,"Move mouse to track")|QText]),
    message(Help).

%%%
%%% 3ds max style camera.
%%%

tds(#mousebutton{button=2,x=X0,y=Y0,state=?SDL_PRESSED}, Redraw) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(wings_msg:join(tds_help(), 
			   wings_msg:button_format([], [],
						   ?STR(message,7,"Restore view")))),
    View = wings_view:current(),
    {seq,push,get_tds_event(Camera, Redraw, View)};
tds(_, _) -> next.

tds_event(#mousebutton{button=1,state=?SDL_RELEASED}=Mb, Camera, Redraw, View) ->
    tds_event(Mb#mousebutton{button=2}, Camera, Redraw, View);
tds_event(#mousebutton{button=2,state=?SDL_RELEASED}, Camera, _, _) ->
    stop_camera(Camera);
tds_event(#mousebutton{button=3,state=?SDL_RELEASED}, Camera, _, View) ->
    wings_view:set_current(View),
    stop_camera(Camera);
tds_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw, View) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Mod band ?CTRL_BITS =/= 0, Mod band ?ALT_BITS =/= 0 ->
	    zoom(Dy);
	Mod band ?ALT_BITS =/= 0 ->
	    rotate(Dx, Dy);
	true ->
	    pan(Dx, Dy)
    end,
    get_tds_event(Camera, Redraw, View);
tds_event(Event, Camera, Redraw, _) ->
    generic_event(Event, Camera, Redraw).

get_tds_event(Camera, Redraw, View) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> tds_event(Ev, Camera, Redraw, View) end}.

tds_help() ->
    TrackDolly = [{0,2,?STR(mode_help,1,"Track")},
		  {?CTRL_BITS bor ?ALT_BITS,2,?STR(mode_help,2,"Dolly")}],
    case allow_rotation() of
	false -> format(TrackDolly);
	true -> format([{?ALT_BITS,2,?STR(mode_help,3,"Tumble")}|TrackDolly])
    end.

%%%
%%% Maya style camera.
%%%

maya(#mousebutton{x=X0,y=Y0,mod=Mod,state=?SDL_PRESSED}, Redraw)
  when Mod band ?ALT_BITS =/= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    sdl_events:eventState(?SDL_KEYUP, ?SDL_ENABLE),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(maya_help()),
    {seq,push,get_maya_event(Camera, Redraw)};
maya(_, _) -> next.

maya_event(#keyboard{sym=Alt,state=?SDL_RELEASED},
	   Camera, _Redraw) when Alt == ?SDLK_LALT; Alt == ?SDLK_RALT ->
    maya_stop_camera(Camera);
maya_event(#mousebutton{button=B,state=?SDL_RELEASED}, Camera, _)
  when B < 4 ->
    case sdl_mouse:getMouseState() of
	{0,_,_} ->
	    %% Exit camera mode if all mouse buttons released.
	    maya_stop_camera(Camera);
	_ ->
	    %% Some mouse button still pressed.
	    keep
    end;
maya_event(#mousemotion{x=X,y=Y,state=Buttons}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Buttons band 4 == 4 ->			%RMB
	    zoom(-Dx);
	Buttons band 3 == 3 ->			%LMB+MMB
	    zoom(-Dx);
	Buttons band 1 == 1 ->			%LMB
	    rotate(Dx, Dy);
	Buttons band 2 == 2 ->			%MMB
	    pan(Dx, Dy);
	true -> ok
    end,
    get_maya_event(Camera, Redraw);
maya_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).

get_maya_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> maya_event(Ev, Camera, Redraw) end}.

maya_stop_camera(Camera) ->
    sdl_events:eventState(?SDL_KEYUP, ?SDL_IGNORE),
    stop_camera(Camera).

maya_help() ->
    TrackDolly = [{?ALT_BITS,2,?STR(mode_help,1,"Track")},
		  {?ALT_BITS,3,?STR(mode_help,2,"Dolly")}],
    case allow_rotation() of
	false -> format(TrackDolly);
	true -> format([{?ALT_BITS,1,?STR(mode_help,3,"Tumble")}|TrackDolly])
    end.

%%%
%%% Motionbuilder style camera.
%%%

mb(#mousebutton{button=1,mod=Mod,x=X0,y=Y0,state=?SDL_PRESSED}, Redraw)
  when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Camera = #camera{x=X,y=Y,ox=X,oy=Y},
    grab(),
    message(mb_help()),
    {seq,push,get_mb_event(Camera, Redraw)};
mb(_, _) -> next.

mb_event(#mousebutton{button=1,state=?SDL_RELEASED}, Camera, _) ->
    stop_camera(Camera);
mb_event(#mousemotion{x=X,y=Y,mod=Mod}, Camera0, Redraw) ->
    {Dx,Dy,Camera} = camera_mouse_range(X, Y, Camera0),
    if
	Mod band ?CTRL_BITS =/= 0, Mod band ?SHIFT_BITS =/= 0 ->
	    rotate(Dx, Dy),
	    get_mb_event(Camera, Redraw);
	Mod band ?CTRL_BITS =/= 0 ->
	    zoom(Dy),
	    get_mb_event(Camera, Redraw);
	Mod band ?SHIFT_BITS =/= 0 ->
	    pan(Dx, Dy),
	    get_mb_event(Camera, Redraw);
	true ->
	    stop_camera(Camera)
    end;
mb_event(Event, Camera, Redraw) ->
    generic_event(Event, Camera, Redraw).

get_mb_event(Camera, Redraw) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> mb_event(Ev, Camera, Redraw) end}.

mb_help() ->
    TrackDolly = [{?SHIFT_BITS,1,?STR(mode_help,1,"Track")},
		  {?CTRL_BITS,1,?STR(mode_help,2,"Dolly")}],
    case allow_rotation() of
	false -> format(TrackDolly);
	true -> format([{?SHIFT_BITS bor ?CTRL_BITS,1,?STR(mode_help,3,"Tumble")}|TrackDolly])
    end.
    
%%%
%%% Common utilities.
%%%		     

generic_event(redraw, _Camera, #state{st=St, func=none}) ->
    wings:redraw(St),
    keep;
generic_event(redraw, _Camera, #state{func=Redraw}) when is_function(Redraw) ->
    Redraw(),
    keep;
generic_event(#mousebutton{button=4,state=?SDL_RELEASED}, _Camera, _Redraw) ->
    zoom_step(-1);
generic_event(#mousebutton{button=5,state=?SDL_RELEASED}, _Camera, _Redraw) ->
    zoom_step(1);
generic_event(_, _, _) -> keep.

rotate(Dx, Dy) ->
    case allow_rotation() of
	false -> ok;
	true ->
	    View0= wings_view:current(),
	    #view{azimuth=Az0,elevation=El0} = View0,
	    Az = Az0 + Dx,
	    El = El0 + Dy,
	    View = View0#view{azimuth=Az,elevation=El,along_axis=none},
	    wings_view:set_current(View)
    end.

zoom_step(Dir) ->
    case wings_pref:get_value(wheel_zooms, true) of
	false -> keep;
	true ->
	    wings_wm:dirty(),
	    #view{distance=Dist} = View = wings_view:current(),
	    ZoomPercent0 = wings_pref:get_value(wheel_zoom_factor, ?ZOOM_FACTOR)/100,
	    ZoomPercent = case wings_pref:get_value(inverted_wheel_zoom) of
			      false -> ZoomPercent0;
			      true -> -ZoomPercent0
			  end,
	    Delta = dist_factor(Dist)*Dir*ZoomPercent,
	    wings_view:set_current(View#view{distance=Dist+Delta}),
	    keep
    end.

zoom(Delta0) ->
    #view{distance=Dist} = View = wings_view:current(),
    Delta = dist_factor(Dist)*Delta0/80,
    wings_view:set_current(View#view{distance=Dist+Delta}).

pan(Dx0, Dy0) ->
    #view{pan_x=PanX0,pan_y=PanY0,distance=D} = View = wings_view:current(),
    S = D*(1/20)/(101-wings_pref:get_value(pan_speed)),
    Dx = Dx0*S,
    Dy = Dy0*S,
    PanX = PanX0 + Dx,
    PanY = PanY0 - Dy,
    wings_view:set_current(View#view{pan_x=PanX,pan_y=PanY}).

dist_factor(Dist) ->
    wings_util:max(abs(Dist), 0.2).

stop_camera(#camera{ox=Ox,oy=Oy}) ->
    case wings_io:ungrab(Ox, Oy) of
	still_grabbed ->
	    wings_wm:later(view_changed);
	no_grab ->
	    wings_wm:release_focus(),
	    wings_wm:dirty()
    end,
    update_sel(fun show_sel_fun/2),
    pop.

camera_mouse_range(X0, Y0, #camera{x=OX,y=OY, xt=Xt0, yt=Yt0}=Camera) ->
%%    io:format("Camera Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xt0,Yt0}]),
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    XD0 = (X1 - OX),
    YD0 = (Y1 - OY),
    {XD,YD} = wings_pref:lowpass(XD0 + Xt0, YD0 + Yt0),

    if
	XD0 == 0, YD0 == 0 ->
	    {0.0,0.0,Camera#camera{xt=0,yt=0}};
	true ->
	    wings_io:warp(OX, OY),
	    {XD/?CAMDIV, YD/?CAMDIV, Camera#camera{xt=XD0, yt=YD0}}
    end.

view_hotkey(Ev, Camera, #state{st=St}) ->
    case wings_hotkey:event(Ev,St) of
	next -> keep;
	{view,smooth_preview} -> keep;
	{view,smoothed_preview} -> keep;
	{view,Cmd} -> 
	    wings_view:command(Cmd, St),
	    keep;	
	_Other -> %% Hotkey pressed, Quit camera mode
	    wings_wm:later(Ev),
	    stop_camera(Camera)
    end.

message(Message) ->
    wings_wm:message(Message),
    wings_wm:message_right([]).
    
grab() ->
    wings_io:grab(),
    wings_wm:grab_focus(),
    update_sel(fun hide_sel_fun/2).

hide_sel_fun(#dlo{sel=Sel}=D, _) ->
    D#dlo{sel={call,none,Sel}}.

show_sel_fun(#dlo{sel={call,none,Sel}}=D, _) ->
    D#dlo{sel=Sel};
show_sel_fun(D, _) -> D.

allow_rotation() ->
    wings_wm:get_prop(allow_rotation).

update_sel(Fun) ->
    case wings_pref:get_value(hide_sel_in_camera_moves) of
	false -> ok;
	true -> wings_dl:map(Fun, [])
    end.

format([{Mod,But,Msg}|T]) ->
    wings_msg:join(wings_msg:mod_format(Mod, But, Msg), format(T));
format([]) -> [].
