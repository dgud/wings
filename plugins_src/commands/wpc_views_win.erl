%%
%%  wpc_view_win.erl --
%%
%%     This module implements the Save Views commands in a window.
%%
%%  Copyright (c) 2012 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_views_win).
-export([init/0,menu/2,command/2,win_data/1]).
-export([window/1,window/5]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-define(WIN_NAME, {plugin,saved_views}).
-include("wings.hrl").

%%%
%%% Saved Views window.
%%%
-record(ost,
	{st,					%Current St.
	 n,					    %Number of objects.
	 first,					%First object to show.
	 os,					%All objects.
	 active,				%Number of active object.
	 tracking,				%Number of mouse tracked object.
	 lh					    %Line height.
	}).

init() -> true.

menu({window}, Menu) ->
    Menu++[camera_menu()];
menu(_,Menu) -> 
	Menu.

camera_menu() ->	
	 {?__(1,"Saved Views"), saved_views,
	  ?__(2,"Shows all saved views")}.

command({window,saved_views}, St) ->
    window(St),
    keep;
command(_,_) ->
	next.

%% win_data/1 function allows many plugin windows to be saved.
%% it returns: {Name, {Horiz alignment, Custom_data}}
%% horiz alignment should be either "left" or "right"
%% custom data is used to store windows properties and custom data - it should be parsed in window/5
win_data(?WIN_NAME=Name) ->
    {Name, {right,[{rollup, wings_wm:win_rollup(Name)}]}}.

window(St) ->
    case wings_wm:is_window(?WIN_NAME) of
	true ->
	    wings_wm:raise(?WIN_NAME),
	    keep;
	false ->
	    {{DeskX,DeskY},{_DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 18*?CHAR_WIDTH,
	    Pos = {DeskX+5,DeskY+105},
	    Size = {W,DeskH div 3},
	    window(?WIN_NAME, Pos, Size, [], St),
	    keep
    end.

window(?WIN_NAME, Pos, Size, CtmData, St) ->
	Ps = CtmData,
    Ost = #ost{first=0,lh=18,n=0,active=-1,tracking=-1},
    Current = {current_state,St},
    Op = {seq,push,event(Current, Ost)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(?WIN_NAME, title(), Pos, Size,
		      [{sizeable,?PANE_COLOR},closable,vscroller,{anchor,ne},
		       {properties,Props}|Ps], Op).

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(redraw, Ost) ->
    draw_objects(Ost),
    keep;
event({action,{saved_views,Cmd}}, #ost{st=#st{views={_,Views0}}=St0}=Ost) ->
    case Cmd of
        {save,_} ->
            wings_wm:send(geom, {action, {view, {views, {save,true}}}});
        {rename,_} ->
            wings_wm:send(geom, {action, {view, {views, rename}}});
        {replace,Idx} ->
            {_,Legend} = element(Idx, Views0),
            View = current(),
            Views = setelement(Idx, Views0, {View, Legend}),
            wings_wm:send(geom, {new_state, St0#st{views={Idx,Views}}});
        {delete,_} ->
            wings_wm:send(geom, {action, {view, {views, delete}}});
        {delete_all,_} ->
            wings_wm:send(geom, {action, {view, {views, delete_all}}})
    end,
    get_event(Ost);
event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(close, _) ->
    delete;
event(lost_focus, Ost) ->
    get_event(Ost#ost{tracking=-1});
event(got_focus, _) ->
    Msg = wings_msg:button_format(?__(1,"Select"), [],
				  ?__(2,"Shows saved views menu")),
    wings_wm:message(Msg),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousemotion{y=Y}, #ost{}=Ost) ->
    wings_wm:dirty(),
    get_event(Ost#ost{tracking=active_object(Y,Ost)});
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, #ost{active=Act}=Ost)
  when Act >= 0 ->
    case active_object(Y, Ost) of
	Act ->
	    wings_wm:grab_focus(),
	    get_event(Ost#ost{tracking=-1});
	_ ->
	    ok
    end,
    get_event(Ost);
event(#mousebutton{button=1,y=Y,state=?SDL_RELEASED}, #ost{active=Act0}=Ost) ->
    wings_wm:release_focus(),
    case active_object(Y, Ost) of
	Act0 ->  keep;
	Act ->
	  if Act=/=-1 ->
	      wings_wm:send(geom, {action, {view, {views, {jump,Act+1}}}}),
          wings_wm:dirty(),
          get_event(Ost#ost{active=Act});
      true -> keep
	  end
    end;
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(#mousebutton{}=Ev, Ost) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} -> do_menu(X, Y, Ost)
    end;
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event(Ev, Ost) ->
    case wings_hotkey:event(Ev) of
	{_,_}=Act ->
	    wings_wm:send(geom, {action,Act}),
	    wings_wm:dirty(),
	    get_event(Ost);
	_ -> keep
    end.

%% there are no views
do_menu(X, Y ,#ost{st=#st{views={_,{}}}}=_Ost) ->
    Menu=views_menu({1,new}),
    wings_menu:popup_menu(X, Y, saved_views, Menu);
do_menu(X, Y, #ost{active=Act,st=#st{views={_,Views}},tracking=Trk}=_Ost) ->
    Objs = objs_from_view(Views),
    Id = act_to_key(Objs,Act),
    Menu0 = views_menu({length(Objs)+1,new}),  % Save option
    Menu1 = case Trk of
        Act -> Menu0++views_menu(Id);  % Rename and Delete option
        _ -> Menu0
    end,
    Menu2 = Menu1++views_menu({none,delete}),  % Delete All option
    Menu = if Trk=/=-1 ->
        Id0 = act_to_key(Objs,Trk),
        Menu2++
        views_menu({Trk,Id0}); % Replace option
      true ->
        Menu2
    end,
	wings_menu:popup_menu(X, Y, saved_views, Menu).

views_menu({Idx, new}) ->
	[{?__(1,"Save..."),menu_cmd(save,none), ?__(2,"Save this view at ") ++"["++integer_to_list(Idx)++"]"}];
views_menu({_, delete}) ->
	[separator,
	 {?__(9,"Delete All..."), menu_cmd(delete_all,all), ?__(10,"Delete all saved views")}];
views_menu({_, {Idx,Legend}}) ->
	[{?__(3,"Replace "),menu_cmd(replace,Idx), ?__(4,"Replaces \"")++Legend++"\"["++integer_to_list(Idx)++"]\" settings with the current viewing ones"}];
views_menu({Idx, Legend}) ->
	[separator,
	 {?__(5,"Rename..."), menu_cmd(rename,none), ?__(6,"Rename \"")++Legend++"\"["++integer_to_list(Idx)++"]\""},
	 {?__(7,"Delete"), menu_cmd(delete,none), ?__(8,"Delete \"")++Legend++"\"["++integer_to_list(Idx)++"]\""}].

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.


%%%
%%% Updating the state.
%%%

update_state(#st{views={_,Views}}=St, #ost{active=Act0,first=OldFirst}=Ost0) ->
    Act=case Views of
      {} -> -1;
      _ -> Act0  % undefined
    end,
    #ost{first=First0} = Ost = update_state_1(St, Ost0#ost{active=Act}),
    case clamp(First0, Ost) of
	  OldFirst -> Ost;
	  First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

update_state_1(St, Ost) ->
    update_state_2(St, Ost).

update_state_2(#st{views={CurrentView,Views}}=St, #ost{os=Objs0}=Ost) ->
    Objs = objs_from_view(Views),
    case Objs of
	Objs0 -> ok;
	_ -> wings_wm:dirty()
    end,
    N = length(Objs),
    Act = CurrentView-1,
    Ost#ost{st=St,os=Objs,n=N,active=Act}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:this(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).

objs_from_view(Views) ->
    [{_,Vn}]=lists:foldl(fun({_,Legend}, [{Idx0,Acc}]) ->
        Idx = Idx0+1,
        [{Idx, Acc++[{Idx,Legend}]}]
    end, [{0,[]}], tuple_to_list(Views)),
    lists:flatten(Vn).

act_to_key(_,-1) -> none;
act_to_key(Objs,Act) ->
    lists:nth(Act+1, Objs).

zoom_step(Step, #ost{first=First0}=Ost0) ->
    case clamp(First0+Step, Ost0) of
	First0 -> keep;
	First ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First},
	    update_scroller(Ost),
	    get_event(Ost)
    end.

clamp(F, #ost{n=N}=Ost) ->
    Max = case N-lines(Ost) of
	      Neg when Neg < 0 -> 0;
	      Other -> Other
	  end,
    if
	F < 0 -> 0;
	F > Max -> Max;
	true -> F
    end.
    
lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.

title() ->
    ?__(1,"Saved views").

active_object(Y0, #ost{lh=Lh,first=First,n=N}) ->
    case Y0 - top_of_first_object() of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N ->
		    First+Y;
		_ -> -1
	    end
    end.

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,tracking=Trk,n=N0}=Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),

    Objs = lists:nthtail(First, Objs0),
    Lines = lines(Ost),
    N = case N0-First of
	    N1 when N1 < Lines -> N1;
	    _ -> Lines
	end,
	Y=Lh-2,
    draw_objects_1(N, Objs, Ost, W, Active-First, Y),
    if
      Trk=/=-1 ->
        Y0=Y+(Trk*Lh),
        draw_frame(2,Y0-?CHAR_HEIGHT,W-2,Y0+4);
      true -> ok
    end.

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [{_,Name}|Objs], #ost{lh=Lh}=Ost, R, Active, Y) ->
    B = wings_pref:get_value(bitmap_icons),
    case Active =:= 0 of
    true when B ->
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl)),
      gl:recti(2, Y-?CHAR_HEIGHT, R-2, Y+4),
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl_text));
    true ->
      gl:color3f(0, 0, 0.5),
      gl:recti(2, Y-?CHAR_HEIGHT, R-2, Y+4),
      gl:color3f(1, 1, 1);
    false -> ok
    end,
    wings_io:text_at(4, Y, Name),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Objs, Ost, R, Active-1, Y+Lh).

draw_frame(X1, Y1, X2, Y2) ->
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2i(X1+1, Y1+1),
    gl:vertex2i(X2, Y1),
    gl:vertex2i(X2, Y2),
    gl:vertex2i(X1, Y2),
    gl:vertex2i(X1+1, Y1), % force x1,y1 corner (dot) be drawn
    gl:'end'().

top_of_first_object() ->
    0.

current() ->
    wings_wm:get_prop(geom, current_view).
