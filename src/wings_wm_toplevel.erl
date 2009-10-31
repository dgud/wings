%%
%%  wings_wm_toplevel.erl --
%%
%%     Implements toplevel windows.
%%
%%  Copyright (c) 2003-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_wm_toplevel).

%% Don't call any functions in this module directly. Use the supported
%% API in wings_wm.

-export([toplevel/6,set_knob/3,title_height/0,vscroller_width/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,keyfind/3,sort/1,foreach/2]).
-import(erlang, [min/2,max/2]).

toplevel(Name, Title, Pos, Size, Flags, Op) ->
    wings_wm:new(Name, Pos, Size, Op),
    new_controller(Name, Title, Flags).

-record(ctrl,
	{title,					%Title of window.
	 state=idle,			%idle|moving
	 local,
	 prev_focus,			%Previous focus holder.
	 rollup					%rollup function
	}).

new_controller(Client, Title, Flags) ->
    Z = wings_wm:win_z(Client),
    case keyfind(properties, 1, Flags) of
	false -> ok;
	{properties,Props} ->
	    foreach(fun({K,V}) -> wings_wm:set_prop(Client, K, V) end, Props)
    end,
    Controller = {controller,Client},
    ctrl_create_windows(reverse(sort(Flags)), Client),
    Cs = #ctrl{title=Title},
    wings_wm:new(Controller, {0,0,Z}, {1,title_height()},
		 {seq,push,get_ctrl_event(Cs)}),
    wings_wm:link(Client, Controller),
    ctrl_anchor(Client, Flags),
    case keyfind(rollup, 1, Flags) of
      {rollup,true} -> wings_wm:rollup(rollup,Client);
      _ when Client =:= geom -> wings_wm:rollup(rollup,Client);
      _ -> ok
    end,
    keep.

ctrl_create_windows([vscroller|Flags], Client) ->
    {X,Y} = wings_wm:win_ur(Client),
    Z = wings_wm:win_z(Client),
    Name = vscroller(Client, {X,Y,Z}),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([{toolbar,Create}|Flags], Client) ->
    {{X,Y},{W,_}} = wings_wm:win_rect(Client),
    Z = wings_wm:win_z(Client),
    Toolbar = {toolbar,Client},
    Create(Toolbar, {X,Y,Z}, W),
    wings_wm:link(Client, Toolbar),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([resizable|Flags], Client) ->
    Name = ctrl_new_resizer(Client, none),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([sizeable|Flags], Client) ->
    Name = ctrl_new_resizer(Client, none),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([{sizeable,Color}|Flags], Client) ->
    Name = ctrl_new_resizer(Client, Color),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([closable|Flags], Client) ->
    Name = new_closer(Client),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([menubar|Flags], Client) ->
    Name = create_menubar(Client),
    wings_wm:link(Client, Name),
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([_|Flags], Client) ->
    ctrl_create_windows(Flags, Client);
ctrl_create_windows([], _) -> [].

ctrl_anchor(Client, Flags) ->
    case keyfind(anchor, 1, Flags) of
	false ->
	    %% Dummy update to position and size all helper windows properly.
	    wings_wm:update_window(Client, [{dx,0}]);
	{anchor,Anchor} ->
	    {_,Y} = controller_pos(Client),
	    {_,Cy} = wings_wm:win_ul(Client),
	    ctrl_anchor_1(Anchor, Client, Cy-Y),
	    {Lt,Top} = wings_wm:win_ul(Client),
	    CtrlHeight = ?LINE_HEIGHT+3,
	    if
		Lt < 0 -> wings_wm:update_window(Client, [{x,2}]);
		true -> ok
	    end,
	    if
		Top < CtrlHeight ->
		    wings_wm:update_window(Client, [{y,CtrlHeight}]);
		true -> ok
	    end,
	    {W,H} = wings_wm:top_size(),
	    {Rt,Bot} = wings_wm:win_lr(Client),
	    if
		W < Rt -> wings_wm:update_window(Client, [{dx,W-Rt-2}]);
		true -> ok
	    end,
	    if
		H < Bot -> wings_wm:update_window(Client, [{dy,H-Bot-2}]);
		true -> ok
	    end
    end.

ctrl_anchor_1(nw, Client, Th) ->
    wings_wm:update_window(Client, [{dy,Th}]);
ctrl_anchor_1(n, Client, Th) ->
    W = controller_width(Client),
    wings_wm:offset(Client, -W div 2, Th);
ctrl_anchor_1(ne, Client, Th) ->
    W = controller_width(Client),
    wings_wm:offset(Client, -W, Th);
ctrl_anchor_1(sw, Client, Th) ->
    {_,H} = wings_wm:win_size(Client),
    wings_wm:update_window(Client, [{dy,Th-H}]).

get_ctrl_event(Cs) ->
    {replace,fun(Ev) -> ctrl_event(Ev, Cs) end}.
		     
ctrl_event(redraw, Cs) ->
    ctrl_message(),
    ctrl_redraw(Cs);
ctrl_event(#mousebutton{button=1,state=?SDL_PRESSED},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});

ctrl_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Cs) ->
    {_,Client} = Self = wings_wm:this(),
    Rollup = wings_wm:win_rollup(Client),
    Time = if Rollup ->  wings_wm:rollup(rolldown,Client), undefined;
                true -> now()
           end,
    wings_wm:raise(Client),
    Focus = wings_wm:grabbed_focus_window(),
    wings_wm:grab_focus(Self),
    get_ctrl_event(Cs#ctrl{local={X,Y},state=moving,prev_focus=Focus,rollup=Time});

ctrl_event(#mousebutton{button=1,state=?SDL_RELEASED}, #ctrl{prev_focus=Focus,rollup=Time}=Cs) ->
    {_, Client} = wings_wm:this(),
    T = if Time =:= undefined -> 300000; true -> timer:now_diff(now(),Time) end,
    Rollup = wings_wm:win_rollup(Client),
    case  Rollup of
      true when T < 300000 ->
        wings_wm:rollup(rolldown,Client),
        wings_wm:raise(Client);
      false when T < 300000 ->
        wings_wm:rollup(rollup,Client);
      _ -> no_change
    end,
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousebutton{button=2,state=?SDL_RELEASED}, Cs) ->
    case is_resizeable() of
	false -> keep;
	true ->
	    ctrl_command({fit,both}, Cs),
	    keep
    end;
ctrl_event(#mousemotion{state=0},
	   #ctrl{state=moving,prev_focus=Focus}=Cs) ->
    wings_wm:grab_focus(Focus),
    get_ctrl_event(Cs#ctrl{state=idle});
ctrl_event(#mousemotion{x=X0,y=Y0}, #ctrl{state=moving,local={LocX,LocY}}) ->
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = wings_wm:win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {controller,Client} = wings_wm:this(),
    {Dx,Dy} = ctrl_constrain_move(Client, Dx0, Dy0),
    wings_wm:offset(Client, Dx, Dy),
    keep;
ctrl_event(#mousebutton{}=Ev, _) ->
    case is_resizeable() of
	false -> ok;
	true ->
	    {_, Client} = wings_wm:this(),
	    case Client of
		geom -> ok;
		_ ->
	      wings_wm:rollup(rolldown,Client),
	      wings_wm:raise(Client)
	    end,
	    case wings_menu:is_popup_event(Ev) of
		{yes,X,Y,_} -> ctrl_menu(X, Y);
		no -> ok
	    end
    end,
    keep;
ctrl_event(#keyboard{}=Ev, _) ->
    {_,Client} = wings_wm:this(),
    wings_wm:send(Client, Ev);
ctrl_event({window_updated,Client}, _) ->
    W = controller_width(Client),
    H = ?LINE_HEIGHT+3,
    Pos = controller_pos(Client),
    Updates = [{pos,Pos},{w,W},{h,H}],
    Self = {controller,Client},
    wings_wm:update_window(Self, Updates),
    keep;
ctrl_event({action,{titlebar,Action}}, Cs) ->
    ctrl_command(Action, Cs);
ctrl_event({title,Title}, Cs) ->
    get_ctrl_event(Cs#ctrl{title=Title});
ctrl_event(_, _) -> keep.

ctrl_message() ->
    {_,Client} = wings_wm:this(),
    Rollup = case wings_wm:win_rollup(Client) of
        true when Client =:= geom ->
          wings_msg:button_format(?__(6,"Click to bring Geometry Window to Front"));
        false when Client =:= geom ->
          wings_msg:button_format(?__(7,"Click to send Geometry Window to Back"));
        true ->
          wings_msg:button_format(?__(4,"Click to show window"));
        false ->
          wings_msg:button_format(?__(5,"Click to rollup window into titlebar"));
        no -> []
    end,
    M0 = wings_msg:button_format(?__(1,"Drag to Move")),
    M1 = case is_resizeable() of
	     false -> [];
	     true ->
		 wings_msg:button_format([],  
					 ?__(2,"Fit"), 
					 ?__(3,"Show menu"))
	 end,
    M = wings_msg:join([Rollup, M0, M1]),
    wings_wm:message(M),
    wings_wm:dirty().

ctrl_redraw(#ctrl{title=Title}) ->
    wings_io:ortho_setup(none),
    {W,_} = wings_wm:win_size(),
    TitleBarH = title_height(),
    This = wings_wm:this(),
    FocusWindow = wings_wm:actual_focus_window(),
    Pref = case {This,FocusWindow} of
	       {{_,Client},Client} -> title_active_color;
	       % The Geometry Graph {object,geom} naming convention allows it to
	       % match with the Geometry window, so we stop that particular match.
	       {{_,Client},{WinElem,Client}} when WinElem =/= object ->
	           title_active_color;
	       {_,_} -> title_passive_color
	   end,
    wings_io:blend(wings_pref:get_value(Pref),
		   fun(C) ->
			   wings_io:gradient_border(0, 0, W-1, TitleBarH-1, C)
		   end),
    wings_io:set_color(wings_pref:get_value(title_text_color)),
    wings_io:text_at(10, TitleBarH-5, Title),
    keep.

title_height() ->
    ?LINE_HEIGHT+3.

ctrl_constrain_move(Client, Dx0, Dy0) ->
    {{DeskX,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
    {{X0,Y0},{W,_}} = wings_wm:win_rect({controller,Client}),
    Dx = case X0+Dx0-DeskX of
	     X when X < 0 ->
		 DeskX-X0;
	     X when DeskX+DeskW < X+W ->
		 DeskX+DeskW-X0-W;
	     _ ->
		 Dx0
	 end,
    {{_,Cy},{_,Ch}} = wings_wm:win_rect(Client),
    Dy = if 
	     Y0+Dy0 < DeskY ->
		 DeskY-Y0;
	     Cy+Ch+Dy0 >= DeskY+DeskH ->
		 case DeskY+DeskH-Cy-Ch of
		     Dy1 when Y0+Dy1 < DeskY -> Dy0;
		     Dy1 -> Dy1
		 end;
	     true -> Dy0
	 end,
    {Dx,Dy}.

ctrl_menu(X, Y) ->
    Menu = [{ ?__(1,"Fit"),
	     {fit,
	      [{?__(2,"Both"),both,
		?__(3,"Let window use all available space by expanding in all directions")},
	       {?__(4,"Horizontal"),horizontal,
	 	?__(5,"Let window use all available space by expanding it horizontally")},
	       {?__(6,"Vertical"),vertical,
		?__(7,"Let window use all available space by expanding it vertically")}
	      ]}},
	    {?__(8,"Size"),size,?__(9,"Set window size numerically")}|ctrl_menu_toolbar()],
    wings_menu:popup_menu(X, Y, titlebar, Menu).

ctrl_menu_toolbar() ->
    {_,Client} = wings_wm:this(),
    Toolbar = {toolbar,Client},
    case wings_wm:is_window(Toolbar) of
	false -> [];
	true ->
	    case wings_wm:is_hidden(Toolbar) of
		false ->
		    [{?__(1,"Hide Toolbar"),hide_toolbar,?__(2,"Hide the toolbar")}];
		true ->
		    [{?__(3,"Show Toolbar"),show_toolbar,?__(4,"Show the toolbar")}]
	    end
    end.

ctrl_command(hide_toolbar, _) ->
    {_,Client} = wings_wm:this(),
    Toolbar = {toolbar,Client},
    wings_wm:hide(Toolbar),
    {_,H} = wings_wm:win_size(Toolbar),
    wings_wm:update_window(Client, [{dy,-H},{dh,H}]),
    case wings_wm:win_rollup(Client) of
      true when Client /= geom ->
        wings_wm:rollup(rolldown,Client),
        wings_wm:raise(Client);
      _ -> keep
    end,
    wings_wm:dirty();
ctrl_command(show_toolbar, _) ->
    {_,Client} = wings_wm:this(),
    Toolbar = {toolbar,Client},
    wings_wm:show({toolbar,Client}),
    {_,H} = wings_wm:win_size(Toolbar),
    wings_wm:update_window(Client, [{dy,H},{dh,-H}]),
    case wings_wm:win_rollup(Client) of
      true when Client /= geom ->
        wings_wm:rollup(rolldown,Client),
        wings_wm:raise(Client);
      _ -> keep
    end,
    wings_wm:dirty();
ctrl_command({fit,Fit}, _) ->
    ctrl_fit(Fit),
    keep;
ctrl_command(size, _) ->
    {_,Client} = wings_wm:this(),
    {W0,H0} = wings_wm:win_size(Client),
    Qs = [{?__(1,"Width"),W0},
	  {?__(2,"Height"),H0}],
	   wings_ask:ask(?__(3,"Set Window Size"), Qs,
		  fun([W,H]) ->
			  ctrl_resize(Client, W, H),
			  ignore
		  end).

ctrl_resize(Client, W, H) ->
    {TopW,TopH} = wings_wm:top_size(),
    if
	W > TopW; H > TopH ->
	    wings_u:error(?__(1,"Too large size specified"));
	true ->
	    wings_wm:resize(Client, {W,H})
    end.

ctrl_fit(How) ->
    {_,Client} = wings_wm:this(),
    wings_wm:raise(Client),
    {X,Y} = win_center(Client),
    wings_wm:hide(Client),
    Below = wings_wm:window_below(X, Y),
    wings_wm:show(Client),
    if
	Below == none -> ok;
	true -> ctrl_fit_1(How, Client, Below, X, Y)
    end.

win_center(Name) ->
    {TopW,TopH} = wings_wm:top_size(),
    {{X,Y},{W0,H0}} = wings_wm:win_rect(Name),
    W = min(W0, TopW),
    H = min(H0, TopH),
    {X + W div 2,Y + H div 2}.

ctrl_fit_1(both, Client, Below, X, Y) ->
    fit_horizontal(Client, Below, X, Y),
    wings_wm:later({action,{titlebar,{fit,vertical}}});
ctrl_fit_1(horizontal, Client, Below, X, Y) ->
    fit_horizontal(Client, Below, X, Y);
ctrl_fit_1(vertical, Client, Below, X, Y) ->
    fit_vertical(Client, Below, X, Y).

fit_horizontal(Client, Below, X, _) ->
    {Left,_} = wings_wm:win_ul(Below),
    {Right,_} = case wings_wm:is_window(Scroller={vscroller,Below}) of
		    true -> wings_wm:win_ur(Scroller);
		    false -> wings_wm:win_ur(Below)
		end,
    Win0 = fit_filter(wings_wm:windows(), Client, Below),
    {_,Top} = wings_wm:win_ul(),
    {_,ClientBot} = wings_wm:win_ll(Client),
    H = ClientBot-Top,
    Win = [Wi || Wi <- Win0, have_vertical_overlap(Wi, Top, H)],
    fit_horizontal_1(Win, X, Client, Left, Right).

fit_horizontal_1([Client|Ns], X, Client, Min, Max) ->
    fit_horizontal_1(Ns, X, Client, Min, Max);
fit_horizontal_1([N|Ns], X, Client, Min0, Max0) ->
    {{Left,_},{W,_}} = wings_wm:win_rect(N),
    Right = Left+W,
    case N of
	_ when Right < X, Min0 < Right ->
	    fit_horizontal_1(Ns, X, Client, Right, Max0);
	{vscroller,_} ->
	    fit_horizontal_1(Ns, X, Client, Min0, Max0);
	_ when X < Left, Left < Max0 ->
	    fit_horizontal_1(Ns, X, Client, Min0, Left);
	_ ->
	    fit_horizontal_1(Ns, X, Client, Min0, Max0)
    end;
fit_horizontal_1([], _, Client, Left, Right) ->
    {Left0,_} = wings_wm:win_ul(Client),
    W0 = Right-Left,
    W = case wings_wm:is_window({vscroller,Client}) of
	    false -> W0;
	    true -> W0 - vscroller_width()
	end,
    wings_wm:update_window(Client, [{dx,Left-Left0},{w,W}]).

fit_vertical(Client, Below, _, Y) ->
    {_,Top} = case wings_wm:is_window(Ctrl={controller,Below}) of
		  true -> wings_wm:win_ul(Ctrl);
		  false -> wings_wm:win_ul(Below)
	      end,
    {_,Bottom} = wings_wm:win_ll(Below),
    Win0 = fit_filter(wings_wm:windows(), Client, Below),
    {{Left,_},{W,_}} = wings_wm:win_rect(),
    Win = [Wi || Wi <- Win0, have_horizontal_overlap(Wi, Left, W)],
    fit_vert_1(Win, Y, Client, Top, Bottom).

fit_vert_1([N|Ns], Y, Client, Top0, Bottom0) ->
    {{_,Top},{_,H}} = wings_wm:win_rect(N),
    Bottom = Top+H,
    case N of
	_ when Y < Top, Top < Bottom0 ->
	    fit_vert_1(Ns, Y, Client, Top0, Top);
	{controller,_} ->
	    fit_vert_1(Ns, Y, Client, Top0, Bottom0);
	_ when Bottom < Y, Top0 < Bottom ->
	    fit_vert_1(Ns, Y, Client, Bottom, Bottom0);
	_ ->
	    fit_vert_1(Ns, Y, Client, Top0, Bottom0)
    end;
fit_vert_1([], _, Client, Top, Bottom) ->
    {_,CtrlTop} = controller_pos(Client),
    {_,Top0} = wings_wm:win_ul(Client),
    CtrlH = Top0 - CtrlTop,
    wings_wm:update_window(Client, [{dy,Top-Top0+CtrlH},{h,Bottom-Top-CtrlH}]).

fit_filter(Ns, Client, Below) ->
    {{Left,Top},{W,H}} = wings_wm:win_rect(Client),
    Right = Left + W,
    Bottom = Top + H,
    fit_filter(Ns, Client, Below, {Left,Right,Top,Bottom}, []).

fit_filter([{toolbar,_}|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([{menubar,_}|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([{closer,_}|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([{resizer,_}|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([{vscroller,_}|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([Client|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([Below|Ns], Client, Below, Dim, Acc) ->
    fit_filter(Ns, Client, Below, Dim, Acc);
fit_filter([N|Ns], Client, Below, Dim, Acc) ->
    case is_helper_window(N, Client) of
	true -> fit_filter(Ns, Client, Below, Dim, Acc);
	false -> fit_filter(Ns, Client, Below, Dim, [N|Acc])
    end;
fit_filter([], _, _, _, Acc) -> Acc.

is_helper_window({object,_}, _) -> false;
is_helper_window({controller,Client}, Client) -> true;
is_helper_window({vscroller,Client}, Client) -> true;
is_helper_window({resizer,Client}, Client) -> true;
is_helper_window({closer,Client}, Client) -> true;
is_helper_window({toolbar,Client}, Client) -> true;
is_helper_window({menubar,Client}, Client) -> true;
is_helper_window(_, _) -> false.

have_horizontal_overlap(Name, X, W) ->
    {{Ox,_},{Ow,_}} = wings_wm:win_rect(Name),
    (Ox =< X andalso X < Ox+Ow) orelse (X =< Ox andalso Ox < X+W).

have_vertical_overlap(Name, Y, H) ->
    {{_,Oy},{_,Oh}} = wings_wm:win_rect(Name),
    (Oy =< Y andalso Y < Oy+Oh) orelse (Y =< Oy andalso Oy < Y+H).

%%%
%%% Resizer window.
%%%

-record(rsz,
	{state=idle,				%idle|moving
	 aspect=none,				%Aspect ratio to preserve.
	 local,
	 prev_focus,				%Previous focus holder.
	 color=none
	}).

ctrl_new_resizer(Client, Color) ->
    Name = {resizer,Client},
    Rst = #rsz{color=Color},
    Z = wings_wm:win_z(Client),
    {X,Y} = resizer_pos(Client),
    wings_wm:new(Name, {X,Y,Z+1}, {13,13},
		 {seq,push,get_resize_event(Rst)}),
    Name.

get_resize_event(Rst) ->
    {replace,fun(Ev) -> resize_event(Ev, Rst) end}.

resize_event(redraw, #rsz{color=Color}) ->
    wings_io:ortho_setup(none),
    case Color of
	none -> ok;
	_ ->
	    wings_io:set_color(Color),
	    {W,H} = wings_wm:win_size(),
	    gl:recti(0, 0, W, H)
    end,
    wings_io:draw_icons(fun() ->
				gl:enable(?GL_ALPHA_TEST),
				gl:alphaFunc(?GL_GREATER, 0.5),
				wings_io:draw_icon(0, 0, internal_resize),
				gl:disable(?GL_ALPHA_TEST)
			end),
    keep;
resize_event(got_focus, _) ->
    wings_msg:button(?__(1,"Resize"), 
		     ?__(2,"Resize, keeping current aspect ratio")),
    keep;
resize_event(#mousebutton{button=1,state=?SDL_PRESSED},
	     #rsz{state=moving,prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, Rst) ->
    {_,Client} = Self = wings_wm:this(),
    wings_wm:raise(Client),
    Focus = wings_wm:grabbed_focus_window(),
    wings_wm:grab_focus(Self),
    get_resize_event(Rst#rsz{local={X,Y},state=moving,aspect=none,prev_focus=Focus});
resize_event(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED}, Rst) ->
    {_,Client} = wings_wm:this(),
    {W,H} = wings_wm:win_size(Client),
    Focus = wings_wm:grabbed_focus_window(),
    wings_wm:grab_focus(get(wm_active)),
    get_resize_event(Rst#rsz{local={X,Y},state=moving,aspect=W/H,prev_focus=Focus});
resize_event(#mousebutton{button=1,state=?SDL_RELEASED}, #rsz{prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousemotion{state=0}, #rsz{state=moving,prev_focus=Focus}=Rst) ->
    wings_wm:grab_focus(Focus),
    get_resize_event(Rst#rsz{state=idle});
resize_event(#mousemotion{x=X0,y=Y0},
	     #rsz{state=moving,local={LocX,LocY},aspect=Aspect}) ->
    {X1,Y1} = wings_wm:local2global(X0, Y0),
    X = X1 - LocX,
    Y = Y1 - LocY,
    {OldX,OldY} = wings_wm:win_ul(),
    Dx0 = X-OldX,
    Dy0 = Y-OldY,
    {resizer,Client} = wings_wm:this(),
    {Dx,Dy} = resize_constrain(Client, Dx0, Dy0, Aspect),
    wings_wm:update_window(Client, [{dw,Dx},{dh,Dy}]),
    keep;
resize_event({window_updated,Client}, _) ->
    Pos = resizer_pos(Client),
    wings_wm:move(wings_wm:this(), Pos),
    keep;
resize_event(_, _) -> keep.

resize_constrain(Client, Dx0, Dy0, Aspect) ->
    {DeskW,DeskH} = wings_wm:win_size(desktop),
    {{X,Y},{W,H}} = wings_wm:win_rect(),
    Dx = if
	     DeskW =< X+W+Dx0 ->
		 DeskW-X-W;
	     true ->
		 Dx0
	 end,
    Dy = if 
	     DeskH =< Y+H+Dy0 ->
		 DeskH-Y-H;
	     true ->
		 Dy0
	 end,
    resize_constrain_1(Client, Dx, Dy, Aspect).

resize_constrain_1(_, Dx, Dy, none) -> {Dx,Dy};
resize_constrain_1(Client, Dx, Dy, Aspect) ->
    {W,H} = wings_wm:win_size(Client),
    if
	Dx > Dy ->{round(Aspect*(H+Dy)-W),Dy};
	true -> {Dx,round((W+Dx)/Aspect)-H}
    end.

%%%
%%% A vertical scroller.
%%%

-record(ss,
	{knob_pos,				%Position of knob (0-1).
	 knob_prop,				%Proportion of knob (0-1).
	 track_pos=none
	}).

vscroller(Name0, Pos) ->
    Name = {vscroller,Name0},
    Ss = #ss{knob_pos=0.0,knob_prop=1.0},
    wings_wm:new(Name, Pos, {vscroller_width(),1},
		 {seq,push,get_event(Ss)}),
    Name.

set_knob({vscroller,_}=Name, Pos, Proportion) ->
    wings_wm:send(Name, {set_knob,Pos,Proportion});
set_knob(Name, Pos, Proportion) ->
    set_knob({vscroller,Name}, Pos, Proportion).

vscroller_width() ->
    13.

get_event(Ss) ->
    {replace,fun(Ev) -> event(Ev, Ss) end}.

event(redraw, Ss) ->
    redraw(Ss);
event(got_focus, _) ->
    wings_wm:message(""),
    keep;
event({set_knob,Pos0,Prop0}, #ss{knob_pos=OldPos,knob_prop=OldProp}=Ss) ->
    case {max(0.0, min(1.0, Pos0)),max(0.0, min(1.0, Prop0))} of
	{OldPos,OldProp} -> keep;
	{Pos,Prop} ->
	    wings_wm:dirty(),
	    get_event(Ss#ss{knob_pos=Pos,knob_prop=Prop})
    end;
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, Ss) ->
    down(Y, Ss);
event(#mousebutton{button=1,state=?SDL_RELEASED}, Ss) ->
    wings_wm:release_focus(),
    get_event(Ss#ss{track_pos=none});
event(#mousemotion{y=Y,state=?SDL_PRESSED}, #ss{track_pos=Pos}=Ss)
  when Pos =/= none ->
    drag(Y, Ss);
event({window_updated,Client}, _) ->
    UR = wings_wm:win_ur(Client),
    {_,H} = wings_wm:win_size(Client),
    Updates = case wings_wm:is_window({resizer,Client}) of
		  false -> [{h,H}];
		  true -> [{h,H-13}]
	      end,
    wings_wm:update_window({vscroller,Client}, [{pos,UR}|Updates]),
    keep;
event(_, _) -> keep.

down(Y0, #ss{knob_pos=Pos,knob_prop=Prop}=Ss) ->
    {_,H} = wings_wm:win_size(),
    Y = Y0/H,
    {vscroller,Client} = wings_wm:this(),
    if
	Y < Pos ->
	    wings_wm:send(Client, scroll_page_up),
	    keep;
	Y < Pos+Prop ->
	    wings_wm:grab_focus(),
	    get_event(Ss#ss{track_pos=Pos-Y});
	true ->
	    wings_wm:send(Client, scroll_page_down),
	    keep
    end.

drag(Y0, #ss{knob_prop=Prop,track_pos=TrackPos}) ->
    {_,H} = wings_wm:win_size(),
    Y = case Y0/H + TrackPos of
	    Y1 when Y1 < 0 -> 0.0;
	    Y1 when Y1 < 1-Prop -> Y1;
	    _ -> 1-Prop
	end,
    {vscroller,Client} = wings_wm:this(),
    wings_wm:send(Client, {set_knob_pos,Y}),
    keep.

redraw(#ss{knob_pos=Pos,knob_prop=Prop}) ->
    wings_io:ortho_setup(none),
    {W,H} = wings_wm:win_size(),
    wings_io:border(-1, 0, W, H, ?PANE_COLOR),
    X = 0.5, Y = H*Pos+1.5,
    X2 = W-0.5, Y2 = H*(Pos+Prop),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    gl:color3fv(?PANE_COLOR),
    gl:vertex2f(X2, Y),
    gl:vertex2f(X2, Y2),
    gl:color3f(0.7, 0.7, 0.7),
    gl:vertex2f(X, Y2),
    gl:vertex2f(X, Y),
    gl:'end'(),
    gl:shadeModel(?GL_FLAT),
    gl:color3b(0, 0, 0),
    wings_io:border_only(X, Y, X2-X, Y2-Y),
    keep.

%%%
%%% A close box.
%%%

new_closer(Client) ->
    {X,Y} = closer_pos(Client),
    W0 = title_height() - 5,
    W = W0 + W0 rem 2,
    H = W - 1,
    Z = wings_wm:win_z(Client),
    Name = {closer,Client},
    wings_wm:new(Name, {X,Y,Z}, {W,H}, {push,fun close_event/1}),
    Name.

close_event(redraw) ->
    wings_io:ortho_setup(none),
    {W,H} = wings_wm:win_size(),
    wings_io:gradient_border(0, 0, W-1, H-1, ?PANE_COLOR),
    gl:color3b(0, 0, 0),
    if
	H < 12 ->
	    Close = <<
		     2#11001100,
		     2#01111000,
		     2#00110000,
		     2#01111000,
		     2#11001100
		     >>,
	    Half = (W-6) div 2,
	    gl:rasterPos2i(Half+1, H - Half),
	    gl:bitmap(6, 5, 0, 0, 0, 0, Close),
	    keep;
	true ->
	    Close = <<
		     2#11000011,
		     2#01100110,
		     2#00111100,
		     2#00011000,
		     2#00111100,
		     2#01100110,
		     2#11000011
		     >>,
	    Half = (W-8) div 2,
	    gl:rasterPos2i(Half, H - Half),
	    gl:bitmap(8, 7, 0, 0, 0, 0, Close),
	    keep
    end;
close_event(got_focus) ->
    wings_wm:message(?__(1,"Close this window")),
    keep;
close_event(#mousebutton{button=1,state=?SDL_RELEASED}) ->
    {_,Client} = wings_wm:this(),
    wings_wm:send(Client, close),
    keep;
close_event({window_updated,Client}) ->
    Pos = closer_pos(Client),
    wings_wm:move(wings_wm:this(), Pos),
    keep;
close_event(_) -> keep.

%%
%% Menubar for each window.
%%

-define(MENU_MARGIN, 8).
-define(MENU_ITEM_SPACING, 2).
-define(MENU_HEIGHT, (?CHAR_HEIGHT+6)).

-record(mb,
	{sel=none,
	 bar=[],
	 st
	}).

create_menubar(Client) ->
    Name = {menubar,Client},
    Z = wings_wm:win_z(Client),
    wings_wm:new(Name, {1,1,Z}, {1,?MENU_HEIGHT},
		 {seq,push,get_menu_event(#mb{sel=none})}),
    wings_wm:set_prop(Name, display_lists, wings_wm:get_prop(Client, display_lists)),
    Name.

get_menu_event(Mb) ->
    {replace,fun(Ev) -> menubar_event(Ev, Mb) end}.

menubar_event(redraw, Mb) ->
    menubar_redraw(Mb);
menubar_event({action,_}=Action, _) ->
    wings_wm:send(geom, Action);
menubar_event(clear_menu_selection, Mb) ->
    {_,Client} = This = wings_wm:this(),
    wings_wm:update_window(This, [{z,wings_wm:win_z(Client)}]),
    wings_wm:dirty(),
    get_menu_event(Mb#mb{sel=none});
menubar_event({current_state,St}, Mb) ->
    get_menu_event(Mb#mb{st=St});
menubar_event(#mousebutton{button=1,x=X0,state=?SDL_PRESSED},
	      #mb{sel=Sel}=Mb) ->
    case menubar_hit(X0, Mb) of
	none -> keep;
	{_,Sel,_} -> wings_menu:kill_menus();
	{X,Name,Fun} -> menu_open(X, Name, Fun, Mb)
    end;
menubar_event(#mousemotion{x=X0}, #mb{sel=Sel}=Mb) ->
    case menubar_hit(X0, Mb) of
	none -> keep;
	{_,Sel,_} -> keep;
	{X,Name,Fun} when Sel =/= none ->
	    menu_open(X, Name, Fun, Mb);
	_ -> keep
    end;
menubar_event({window_updated,Client}, _) ->
    Pos = menubar_pos(Client),
    Size = {controller_width(Client),?MENU_HEIGHT},
    wings_wm:move(wings_wm:this(), Pos, Size),
    keep;
menubar_event(_, _) -> keep.

menu_open(Xrel, Name, Fun, #mb{st=St}=Mb) ->
    Menu = Fun(St),
    {menubar,Client} = Self = wings_wm:this(),
    {X,Y} = wings_wm:win_ll(Self),
    wings_menu:menu(X+Xrel, Y-1, Client, Name, Menu),
    get_menu_event(Mb#mb{sel=Name}).

menubar_redraw(Mb) ->
    {menubar,Client} = wings_wm:this(),
    wings_io:ortho_setup(none),
    {W,H} = wings_wm:win_size(Client),
    wings_io:border(0, 0, W-1, H-1, wings_pref:get_value(menu_bar_bg)),
    Menubar = wings_wm:get_menubar(Client),
    menubar_redraw_1(Menubar, Mb),
    get_menu_event(Mb#mb{bar=Menubar}).

menubar_redraw_1(Menubar, #mb{sel=Sel}) ->
    menubar_draw(Menubar, ?MENU_MARGIN, Sel).

menubar_draw([{Desc,Name,_}|T], X, Sel) ->
    W = ?CHAR_WIDTH*(?MENU_ITEM_SPACING+length(Desc)),
    if
	Name =:= Sel ->
	    {_,_,_,H} = wings_wm:viewport(),
	    wings_io:gradient_border(X+2-?MENU_MARGIN, 0,
				     W, H-2, wings_pref:get_value(menu_color));
	true -> ok
    end,
    wings_io:set_color(wings_pref:get_value(menubar_text)),
    wings_io:text_at(X, ?CHAR_HEIGHT, Desc),
    menubar_draw(T, X+W, Sel);
menubar_draw([], _, _) -> keep.

menubar_hit(X0, #mb{bar=Bar}=Mb) ->
    case X0-?MENU_MARGIN of
	X when X < 0 -> none;
	X -> menubar_hit_1(Bar, Mb, X, 0)
    end.

menubar_hit_1([{Desc,Name,Fun}|T], Mb, RelX, X) ->
    case ?CHAR_WIDTH*length(Desc) of
	W when RelX < W ->
	    {X+2,Name,Fun};
	W ->
	    Iw = W+?MENU_ITEM_SPACING*?CHAR_WIDTH,
	    menubar_hit_1(T, Mb, RelX-Iw, X+Iw)
    end;
menubar_hit_1([], _, _, _) -> none.

%%%
%%% Common utilities.
%%%

is_resizeable() ->
    {_,Client} = wings_wm:this(),
    wings_wm:is_window({resizer,Client}).

controller_pos(Client) ->
    {_,H} = wings_wm:win_size({controller,Client}),
    {X,Y} = menubar_pos(Client),
    {X,Y-H}.

controller_width(Client) ->
    {W,_} = wings_wm:win_size(Client),
    case wings_wm:is_window({vscroller,Client}) of
	false -> W;
	true -> W + vscroller_width()
    end.

menubar_pos(Client) ->
    UL = toolbar_pos(Client),
    Name = {menubar,Client},
    case wings_wm:is_window({menubar,Client}) of
	false -> UL;
	true ->
	    {_,H} = wings_wm:win_size(Name),
	    {X,Y} = UL,
	    {X,Y-H}
    end.

toolbar_pos(Client) ->
    UL = wings_wm:win_ul(Client),
    Toolbar = {toolbar,Client},
    case wings_wm:is_window(Toolbar) andalso not wings_wm:is_hidden(Toolbar) of
	false ->
	    UL;
	true ->
	    {_,ToolbarH} = wings_wm:win_size(Toolbar),
	    {X,Y} = UL,
	    {X,Y-ToolbarH}
    end.

closer_pos(Client) ->
    {X,_} = wings_wm:win_ul(Client),
    CloserH = title_height() - 6,
    {_,Y0} = menubar_pos(Client),
    Y = Y0 - CloserH - 3,
    W = controller_width(Client),
    {X+W-CloserH-4,Y}.

resizer_pos(Client) ->
    {{X,Y},{W,H}} = wings_wm:win_rect(Client),
    case wings_wm:is_window({vscroller,Client}) of
	false ->  {X+W-13,Y+H-13};
	true -> {X+W,Y+H-13}
    end.
