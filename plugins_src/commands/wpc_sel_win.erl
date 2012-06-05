%%
%%  wpc_sel_win.erl --
%%
%%     This module implements the selection commands in a window.
%%
%%  Copyright (c) 2012 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_sel_win).
-export([init/0,menu/2,command/2]).
-export([window/1,window/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

%%%
%%% Selection groups window.
%%%
-record(ost,
	{st,					%Current St.
	 n,					    %Number of objects.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh					    %Line height.
	}).

init() -> true.

menu({window}, Menu) ->
    Menu++[sel_group_menu()];
menu(_,Menu) -> 
	Menu.

sel_group_menu() ->	
	 {?__(1,"Selection Groups"), sel_groups,
	  ?__(2,"Shows the selection groups window")}.

command({window,sel_groups}, St) ->
    window(St),
    keep;
command(_,_) ->
    next.

window(St) ->
    case wings_wm:is_window(sel_groups) of
	true ->
	    wings_wm:raise(sel_groups),
	    keep;
	false ->
	    {{_,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 28*?CHAR_WIDTH,
	    Pos = {DeskW-5,DeskY+55},
	    Size = {W,DeskH div 2},
	    window(Pos, Size, [], St),
	    keep
    end.

window(Pos, Size, Ps, St) ->
    Ost = #ost{first=0,lh=18,n=0,active=-1},
    Current = {current_state,St},
    Op = {seq,push,event(Current, Ost)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(sel_groups, title(), Pos, Size,
		      [{sizeable,?PANE_COLOR},closable,vscroller,{anchor,ne},
		       {properties,Props}|Ps], Op).

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event({action,{sel_groups,Cmd}}, Ost) ->
    case Cmd of
    {new_group,_} ->
        wings_wm:send(geom, {action,{select,new_group}});
    _ ->
        wings_wm:send(geom, {action,{select,{ssels,Cmd}}})
    end,
    get_event(Ost);
event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(close, _) ->
    delete;
event(lost_focus, Ost) ->
    get_event(Ost#ost{active=-1});
event(got_focus, _) ->
    Msg = wings_msg:button_format(?__(1,"Select"), [],
				  ?__(2,"Shows selection groups menu")),
    wings_wm:message(Msg),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, #ost{active=Act}=Ost)
  when Act >= 0 ->
    case active_object(Y, Ost) of
	Act ->
	    wings_wm:grab_focus(),
	    get_event(Ost);
	_ ->
	    get_event(Ost)
    end;
event(#mousebutton{button=1,y=Y,state=?SDL_RELEASED}, 
            #ost{active=Act0,st=#st{selmode=SelMode,ssels=Ssels}}=Ost) ->
    wings_wm:release_focus(),
    case active_object(Y, Ost) of
	Act0 -> keep;
	Act ->
	  if Act=/=-1 ->
	      Objs=objs_by_mode(SelMode,gb_trees:keys(Ssels)),
	      Id=act_to_key(Objs,Act),
	      wings_wm:send(geom, {action,{select,{ssels,{select_group,Id}}}});
      true -> ok
	  end,
	  wings_wm:dirty(),
	  get_event(Ost#ost{active=Act})
    end;
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(#mousebutton{y=Y0}=Ev, #ost{active=Act}=Ost) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} -> do_menu(Act, X, Y, Y0, Ost)
    end;
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event(Ev, Ost) ->
    case wings_hotkey:event(Ev) of
	{select,deselect} ->
	    wings_wm:dirty(),
	    get_event(Ost#ost{active=-1});
	_ -> keep
    end.

% none group selected (-1) and there isn't any selection in Geo
do_menu(-1, X, Y, Y0 ,#ost{st=#st{ssels=Ssels,sel=[],selmode=SelMode}}=Ost) -> 
	case gb_trees:is_empty(Ssels) of
	  true -> keep;
	  _ ->
	  	Objs=objs_by_mode(SelMode,gb_trees:keys(Ssels)),
	  	case active_object(Y0, Ost) of % check for item under mouse pointer
	  	  -1 -> keep;
	  	  Act0 -> 
	  	 	Menu=group_del_menu(act_to_key(Objs,Act0)),
	  	 	wings_menu:popup_menu(X, Y, sel_groups, Menu)
	  	end
	end;
% none group selected (-1) and there is a selection in Geo
do_menu(-1, X, Y, Y0, #ost{st=#st{ssels=Ssels,selmode=SelMode}}=Ost) -> 
    Objs=objs_by_mode(SelMode,gb_trees:keys(Ssels)),
    Act0=active_object(Y0, Ost), % check for item under mouse pointer
    Menu1=if
    	Objs =/= [] -> 
    	  Id=act_to_key(Objs,Act0),
    	  group_ins_menu()++group_del_menu(Id)++groups_bool_menu({cur_sel,?__(1,"current selection")},Id);
    	true -> group_ins_menu()
    end,
    Menu2 = groups_basic_menu(act_to_key(Objs,Act0)),
    Menu = Menu1++Menu2, 
	wings_menu:popup_menu(X, Y, sel_groups, Menu);
% there is a group selected (Act)
do_menu(Act, X, Y, Y0, #ost{active=Act0,st=#st{ssels=Ssels,selmode=SelMode}}=Ost) ->
    Objs=objs_by_mode(SelMode,gb_trees:keys(Ssels)),
    Id=act_to_key(Objs,Act),
    Menu0 = group_del_menu(Id),
    Menu1 = case active_object(Y0, Ost) of
        Act0 -> [];
        Act1 -> groups_bool_menu(Id,act_to_key(Objs,Act1))
	end,
    Menu = Menu0++Menu1, 
	wings_menu:popup_menu(X, Y, sel_groups, Menu).

group_ins_menu() ->
	[{?__(1,"New Group..."),menu_cmd(new_group,0),?__(2,"Create a new selection group")}].
group_del_menu(none) -> [];
group_del_menu({_,SrcName}=SrcId) -> 
	[{?__(3,"Delete Group"), menu_cmd(delete_group,SrcId), ?__(4,"Delete group \"")++SrcName++"\""}]++group_cleanup_menu().
groups_basic_menu(none) -> [];
groups_basic_menu({_,SrcName}=SrcId) ->
    [separator,
     {?__(5,"Add to Group"), menu_cmd(add_to_group,SrcId),
          ?__(6,"Add current selection to group \"")++SrcName++"\""},
     {?__(7,"Subtract from Group"), menu_cmd(subtract_from_group,SrcId),
          ?__(8,"Subtract current selection from group \"")++SrcName++"\""}].
group_cleanup_menu() ->
	[separator,
     {?__(9,"Delete Invalid Groups"), menu_cmd(delete_group,invalid),?__(10,"Delete any invalid group - an empty group")}].

groups_bool_menu(none,_) -> [];
groups_bool_menu(_,none) -> [];
groups_bool_menu({_,SrcName},{_,DstName}=DstId) ->
    [separator,
 	 {?__(11,"Union Group"), menu_cmd(union_group,DstId),
	     ?__(12,"Union group \"")++SrcName++?__(13,"\" with \"")++DstName++"\""},
	 {?__(14,"Subtract Group"), menu_cmd(subtract_group,DstId),
	     ?__(15,"Subtract group \"")++DstName++?__(16,"\" from \"")++SrcName++"\""},
	 {?__(17,"Intersect Group"), menu_cmd(intersect_group,DstId),
	     ?__(18,"Intersect group \"")++SrcName++?__(19,"\" with \"")++DstName++"\""}].

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.


%%%
%%% Updating the state.
%%%

update_state(#st{selmode=SelMode}=St, #ost{st=St0,active=Act0,first=OldFirst}=Ost0) ->
    Act=case St0 of
      #st{selmode=SelMode0} ->
        if 
        SelMode=/=SelMode0 -> -1;
        true -> Act0 
        end;
      _ -> Act0
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

update_state_2(#st{ssels=SSels,selmode=SelMode}=St, #ost{os=Objs0,active=Act0}=Ost) ->
    Objs=objs_by_mode(SelMode,gb_trees:keys(SSels)),
    case Objs of
	Objs0 -> ok;
	_ -> wings_wm:dirty()
    end,
    N = length(Objs),
    Act = if
	      Act0 >= N -> N-1;
	      true -> Act0
	  end,
    Ost#ost{st=St,os=Objs,n=N,active=Act}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:this(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).

objs_by_mode(SelMode,Keys) ->
    Keys0=lists:keysort(1,Keys),
    Objs=lists:foldl(fun({SM,_}=Item,Acc) when SM=:=SelMode -> [Acc,Item]; 
                         (_,Acc)-> Acc
                      end, [], Keys0),
    lists:flatten(Objs).

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
    ?__(1,"Selection Groups").

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

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,n=N0}=Ost) ->
    Objs = lists:nthtail(First, Objs0),
    R = right_pos(),
    Lines = lines(Ost),
    N = case N0-First of
	    N1 when N1 < Lines -> N1;
	    _ -> Lines
	end,
    draw_objects_1(N, Objs, Ost, R, Active-First, Lh-2).

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [{SMode,GName}|Objs], #ost{lh=Lh}=Ost, R, Active, Y) ->
    Name = atom_to_list(SMode)++": "++GName,
    case Active =:= 0 of
	true ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(6, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	false -> ok
    end,
    wings_io:text_at(8, Y, Name),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Objs, Ost, R, Active-1, Y+Lh).

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-13.

top_of_first_object() ->
    0.
