%%
%%  wings_geom_win.erl --
%%
%%     Geometry graph window.
%%
%%  Copyright (c) 2016 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_geom_win).
-export([window/1,window/5]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).

-define(FOLDERS, wings_shape).
-define(NO_FLD, no_folder).
-define(NEW_FLD, "new_folder").

-include("wings.hrl").
-import(lists, [foldl/3,keymember/3]).


%%%
%%% Geometry Graph window.
%%%
-record(ost,
	{st,					%Current St.
	 n,					%Number of objects.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh,					%Line height.
	 op					%Latest operation.
	}).

window(St) ->
    Name = {object,wings_wm:this()},
    case wings_wm:is_window(Name) of
	true ->
	    wings_wm:raise(Name),
	    keep;
	false ->
	    {{_,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 28*?CHAR_WIDTH,
	    Pos = {DeskW-5,DeskY+55},
	    Size = {W,DeskH div 2},
	    window(Name, Pos, Size, [], St),
%%	    window2({object2, wings_wm:this()}, Pos, Size, [], St),
	    keep
    end.

window({_,Client}=Name, Pos, Size, Ps, St) ->
    Title = title(Client),
    Ost = #ost{first=0,lh=max(18, ?LINE_HEIGHT),active=-1},
    Current = {current_state,St},
    Op = {seq,push,event(Current, Ost)},
    Props = [{display_data,geom_display_lists}],
    wings_wm:toplevel(Name, Title, Pos, Size,
		      [{sizeable,?PANE_COLOR},closable,vscroller,
		       {anchor,ne},{properties,Props}|Ps], Op).

%% window2(Name, Pos0, Size, Ps, St) ->
%%     Parent = ?GET(top_frame),
%%     Pos = wxWindow:clientToScreen(Parent, Pos0),
%%     Shapes = get_shape_state(St),
%%     Window = wx_object:start_link(?MODULE, [Parent, Pos, Size, Ps, Shapes], []),
%%     wings_wm:new(Name, Window, {push,change_state(Window, Sel)}),
%%     wings_wm:set_dd(Name, geom_display_lists),
%%     wings_frame:register_win(Window),
%%     keep.

title(geom) ->
    ?STR(title,1,"Geometry Graph");
title({geom,N}) ->
    ?STR(title,2,"Geometry Graph #") ++ integer_to_list(N).

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(close, _) ->
    delete;
event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    case wings_pref:get_value(bitmap_icons) of
        false -> wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR);
        true ->
          wings_io:blend(wings_pref:get_value(outliner_geograph_bg),
            fun(Color) ->
              wings_io:border(0, 0, W-1, H-1, Color)
            end)
    end,
    draw_objects(Ost),
    keep;
event({current_state,St0}, Ost0) ->
    Ost = update_state(St0, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousemotion{x=X,y=Y}, Ost) ->
    {Act,Type} = active_object(Y, Ost),
    help(Act, active_field(Type, X)),
    keep;
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(#mousebutton{}=Ev, Ost) ->
    do_action(Ev, Ost);
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event({set_knob_pos,Pos}, #ost{first=First0,n=N}=Ost0) ->
    case round(N*Pos) of
	First0 -> keep;
	First when First < N ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First},
	    update_scroller(Ost),
	    get_event(Ost);
	_ -> keep
    end;
event({action,Action}, #ost{st=#st{sel=Sel}=St0}=Ost) ->
    case Action of
      {objects,{remove_from_folder,Id}} ->
          St = wings_shape:move_to_folder(?NO_FLD, [Id], St0),
          send_client({update_state,St}),
          get_event(Ost);
      {objects,{empty_folder,Folder}} ->
          St = wings_shape:empty_folder(Folder, St0),
          send_client({update_state,St}),
          get_event(Ost);
      {objects,{move_to_folder,Folder}} ->
          St = wings_shape:move_to_folder(Folder, St0),
          send_client({update_state,St}),
          get_event(Ost);
      {objects,{delete_folder,Folder}} ->
          St = wings_shape:delete_folder(Folder, St0),
          send_client({new_state,St}),
          get_event(Ost);
      {objects,{rename_objects,normal}} ->
		Ids=wings_sel:fold(
			fun(_,#we{id=Id},Acc) ->
				Acc++[Id]
			end, [], St0),
		if Ids=/=[] -> command({rename_objects,Ids},Ost);
			true -> get_event(Ost)
		end;
      {objects,{rename_objects,masked}} ->
          rename_filtered_dialog(length(Sel)>1),
          get_event(Ost);
      {objects,Cmd} ->
          command(Cmd, Ost);
      {create_folder,[Folder]} ->
          St = wings_shape:create_folder(Folder, St0),
          send_client({update_state,St}),
          get_event(Ost);
      {rename_folder,[OldName,NewName]} ->
          St = wings_shape:rename_folder(OldName, NewName, St0),
          send_client({update_state,St}),
          get_event(Ost);
      {rename_selected_objects,[Mask]} ->
          St=wings_body:rename_selected(Mask,St0),
          send_client({update_state,St}),
          get_event(Ost);
      {rename_filtered_objects,[Filter,Mask]} ->
          St=wings_body:rename_filtered(Filter,Mask,St0),
          send_client({update_state,St}),
          get_event(Ost)
    end;

event(language_changed, _) ->
    {object,Geom} = This = wings_wm:this(),
    wings_wm:toplevel_title(This, title(Geom)),
    keep;
event(Ev, Ost) ->
    case wings_hotkey:event(Ev) of
	{select,deselect} ->
	    wings_wm:dirty(),
	    get_event(Ost#ost{active=-1});
	_ -> keep
    end.

help(-1, _) -> wings_wm:message("");
help(_, name) ->
    wings_msg:button(?STR(help,1,"Select"), [],?STR(help,2,"Show menu"));
help(_, visibility) ->
    help_1(?STR(help,3,"Toggle visibility of active object"),
	   ?__(13,"Toggle visibility for objects in folder"),
	   ?STR(help,4,"Toggle visibility of all other objects"));
help(_, lock) ->
    help_1(?STR(help,5,"Lock/unlock active object"),
	   ?__(14,"Lock/unlock objects in folder"),
	   ?STR(help,6,"Lock/unlock all objects"));
help(_, wire) ->
    help_1(?STR(help,9,"Toggle shaded/wireframe for active object"),
	   ?__(15,"Toggle shaded/wireframe for objects in folder"),
	   ?STR(help,10,"Toggle shaded/wireframe for all other objects"));
help(_, folder) ->
    help_1(?STR(help,11,"Toggle folder open/closed"),[],
	   ?STR(help,12,"Toggle all folders open/closed"));
help(_, Type) when Type=:=selection; Type=:=?NO_FLD; is_list(Type) ->
    help_1(?STR(help,7,"Toggle selection for active object"),
	   ?__(16,"Toggle selection for folder"),
	   ?STR(help,8,"Toggle selection for all other objects")).

help_1(Msg1, Msg2, Msg3) ->
    wings_msg:button(Msg1, Msg2, Msg3).

command({delete_object,Id}, _) ->
    send_client({action,{body,{delete_object,[Id]}}});
command({duplicate_object,Id}, _) ->
    send_client({action,{body,{duplicate_object,[Id]}}});
command({rename_object,Id}, _) ->
    send_client({action,{body,{rename,[Id]}}});
command({rename_objects,Ids}, _) ->
    send_client({action,{body,{rename,Ids}}});
command(create_folder, _) ->
    create_folder_dialog();
command({rename_folder,OldName}, _) ->
    rename_folder_dialog(OldName);
command({move_to_folder,Folder}, _) ->
    send_client({action,{move_to_folder,Folder}});
command({remove_from_folder,Id}, _) ->
    send_client({action,{remove_from_folder,Id}});
command({empty_folder,Folder}, _) ->
    send_client({action,{empty_folder,Folder}});
command({delete_folder,OldName}, _) ->
    send_client({action,{delete_folder,OldName}});
command(Cmd, _) ->
    io:format("NYI: ~p\n", [Cmd]),
    keep.

update_state(St0, #ost{first=OldFirst}=Ost0) ->
    #ost{st=St,first=First0} = Ost1 = update_state_1(St0, Ost0),
    Alpha = alphabetize_folder_objects(St),
    Ost = Ost1#ost{os=Alpha,n=length(Alpha)},
    case clamp(First0, Ost) of
	OldFirst -> Ost;
	First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

update_state_1(#st{sel=Sel,shapes=Shs}=St, #ost{st=#st{sel=Sel,shapes=Shs}}=Ost) ->
    Ost#ost{st=St};
update_state_1(#st{sel=Sel,shapes=Shs0}=St, #ost{st=#st{shapes=Objs0,sel=Sel}}=Ost) ->
    Shs = gb_trees:values(Shs0),
    Objs = gb_trees:values(Objs0),
    case have_objects_really_changed(Shs, Objs) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel};
update_state_1(#st{sel=Sel}=St, #ost{st=#st{sel=Sel0}}=Ost) ->
    case has_sel_really_changed(Sel, Sel0) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel};
update_state_1(#st{sel=Sel}=St, Ost) ->
    Ost#ost{st=St,sel=Sel}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:this(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).

has_sel_really_changed([{Id,_}|SelA], [{Id,_}|SelB]) ->
    has_sel_really_changed(SelA, SelB);
has_sel_really_changed([], []) -> false;
has_sel_really_changed(_, _) -> true.

have_objects_really_changed([#we{id=Id,name=Name,perm=P}|WesA],
			  [#we{id=Id,name=Name,perm=P}|WesB]) ->
    have_objects_really_changed(WesA, WesB);
have_objects_really_changed([], []) -> false;
have_objects_really_changed(_, _) -> true.

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
    
active_object(Y0, #ost{lh=Lh,first=First,n=N,os=Objs}) ->
    case Y0 of
	Y when Y < 0 -> {-1,[]};
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N ->
		    Act = First+Y,
		    Type = case lists:nth(Act+1, Objs) of
		        {_,#we{pst=Pst}} -> gb_trees:get(?FOLDERS, Pst);
		        _ -> folder
		    end,
		    {Act,Type};
		_ -> {-1,[]}
	    end
    end.

active_field(Type, X) ->
    NamePos = name_pos(Type),
    EyePos =  eye_pos(),
    LockPos = lock_pos(),
    WirePos = wire_pos(),
    if
	X < NamePos -> Type;
	X < EyePos -> name;
	X < LockPos -> visibility;
	X < WirePos -> lock;
	true -> wire
    end.

do_action(#mousebutton{button=B}, _) when B > 3 -> keep;
do_action(#mousebutton{x=X,y=Y,button=B,state=S},
  #ost{st=#st{shapes=Shs,pst=StPst0}=St,os=Objs}=Ost) ->
    {Act,Type} = active_object(Y, Ost),
    case active_field(Type, X) of
	name when (B =:= 1) or (B =:= 3), S =:= ?SDL_PRESSED ->
	    if
		Act =:= -1 -> keep;
		true ->
		    Folders = case lists:nth(Act+1, Objs) of
		      {_,#we{pst=Pst}} ->
		          Folder = gb_trees:get(?FOLDERS, Pst),
		          {_,Fld} = gb_trees:get(?FOLDERS, StPst0),
		          {Folder,Fld};
		      Folder ->
		          {Default,Fld} = gb_trees:get(?FOLDERS, StPst0),
		          case Default=:=Folder of
		              true when B =:= 1 -> {?NO_FLD,Fld};
		              _ -> {Folder,Fld}
		          end
		    end,
		    StPst = gb_trees:enter(?FOLDERS, Folders, StPst0),
		    send_client({update_state,St#st{pst=StPst}}),
		    get_event(Ost#ost{active=Act})
	    end;
	name when B =:= 3, S =:= ?SDL_RELEASED ->
	    {GlobX,GlobY} = wings_wm:local2global(X, Y),
	    do_menu(Act, GlobX, GlobY, Ost);
	_ when B =:= 3, S =:= ?SDL_RELEASED, Act =:= -1 ->
	    {GlobX,GlobY} = wings_wm:local2global(X, Y),
	    do_menu(Act, GlobX, GlobY, Ost);
	Field when S =:= ?SDL_PRESSED ->
	    if
		Act =:= -1 -> keep;
		true ->
		    case lists:nth(Act+1, Objs) of
		      {_,#we{id=Id}} ->
		          We = gb_trees:get(Id, Shs),
		          do_action_1(Field, B, We, Ost);
		      Folder ->
		          do_action_1({Field,Folder}, B, none, Ost)
		    end
	    end;
	_ -> keep
    end.

do_action_1(visibility, 1, We, Ost) -> toggle_visibility(We, Ost);
do_action_1(visibility, 2, We, Ost) -> toggle_visibility_folder(We, Ost);
do_action_1(visibility, 3, We, Ost) -> toggle_visibility_all(We, Ost);
do_action_1(lock, 1, We, Ost) -> toggle_lock(We, Ost);
do_action_1(lock, 2, We, Ost) -> toggle_lock_folder(We, Ost);
do_action_1(lock, 3, We, Ost) -> toggle_lock_all(We, Ost);
do_action_1(wire, 1, We, Ost) -> toggle_wire(We, Ost);
do_action_1(wire, 2, We, Ost) -> toggle_wire_folder(We, Ost);
do_action_1(wire, 3, We, Ost) -> toggle_wire_all(We, Ost);
do_action_1({folder,Folder}, 1, _, Ost) -> toggle_folder(Folder, Ost);
do_action_1({folder,Folder}, 3, _, Ost) -> toggle_folder_all(Folder, Ost);
do_action_1(Type, 1, We, Ost)
  when Type=:=selection; Type=:=?NO_FLD; is_list(Type) ->
    toggle_sel(We, Ost);
do_action_1(Type, 2, We, Ost)
  when Type=:=selection; Type=:=?NO_FLD; is_list(Type) ->
    toggle_sel_folder(We, Ost);
do_action_1(Type, 3, We, Ost)
  when Type=:=selection; Type=:=?NO_FLD; is_list(Type) ->
    toggle_sel_all(We, Ost);
do_action_1(_, _, _, Ost) -> get_event(Ost).

toggle_visibility(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) ->
    {Op,St} = if
		  ?IS_VISIBLE(Perm) -> 
		      {hide,wings_shape:hide_object(Id, St0)};
		  true ->
		      {show,wings_shape:show_object(Id, St0)}
	      end,
    send_client({update_state,St}),
    get_event(Ost#ost{op=Op}).

toggle_visibility_folder(#we{id=Id,pst=WePst}, #ost{st=#st{shapes=Shs,pst=Pst0}=St0}=Ost) ->
    Folder = gb_trees:get(?FOLDERS, WePst),
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids0} = orddict:fetch(Folder, Fld),
    Ids = gb_sets:to_list(Ids0),
    Objs = foldl(fun(Obj, A) ->
        [gb_trees:get(Obj, Shs)|A]
    end, [], Ids),
    St = case are_all_visible(Objs, Id) of
	     false -> wings_shape:show_all_in_folder(Ids, St0);
	     true -> wings_shape:hide_others_in_folder(Id, Ids, St0)
	 end,
    send_client({update_state,St}),
    get_event(Ost#ost{op=none}).

toggle_visibility_all(#we{id=Id}, #ost{st=#st{shapes=Shs}=St0}=Ost) ->
    Objs = gb_trees:values(Shs),
    St = case are_all_visible(Objs, Id) of
	     false -> wings_shape:show_all(St0);
	     true -> wings_shape:hide_others(Id, St0)
	 end,
    send_client({update_state,St}),
    get_event(Ost#ost{op=none}).

are_all_visible([#we{id=Id}|T], Id) ->
    are_all_visible(T, Id);
are_all_visible([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false -> false;
	true -> are_all_visible(T, Id)
    end;
are_all_visible([], _) -> true.

toggle_lock(#we{perm=Perm}, _) when ?IS_NOT_VISIBLE(Perm) -> keep;
toggle_lock(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) when ?IS_SELECTABLE(Perm) ->
    send_client({update_state,wings_shape:lock_object(Id, St0)}),
    get_event(Ost#ost{op=lock});
toggle_lock(#we{id=Id}, #ost{st=St0}=Ost) ->
    send_client({update_state,wings_shape:unlock_object(Id, St0)}),
    get_event(Ost#ost{op=unlock}).

toggle_lock_folder(#we{id=Id,pst=WePst}, #ost{st=#st{shapes=Shs,pst=Pst0}=St0}=Ost) ->
    Folder = gb_trees:get(?FOLDERS, WePst),
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids0} = orddict:fetch(Folder, Fld),
    Ids = gb_sets:to_list(Ids0),
    Objs = foldl(fun(Obj, A) ->
        [gb_trees:get(Obj, Shs)|A]
    end, [], Ids),
    St = case are_all_visible_locked(Objs, Id) of
	     true -> wings_shape:unlock_all_in_folder(Ids, St0);
	     false -> wings_shape:lock_others_in_folder(Id, Ids, St0)
	 end,
    send_client({update_state,St}),
    get_event(Ost#ost{op=none}).

toggle_lock_all(#we{id=Id}, #ost{st=#st{shapes=Shs}=St0}=Ost) ->
    Objs = gb_trees:values(Shs),
    St = case are_all_visible_locked(Objs, Id) of
	     true -> wings_shape:unlock_all(St0);
	     false -> wings_shape:lock_others(Id, St0)
	 end,
    send_client({update_state,St}),
    get_event(Ost#ost{op=none}).

are_all_visible_locked([#we{id=Id}|T], Id) ->
    are_all_visible_locked(T, Id);
are_all_visible_locked([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false ->
	    are_all_visible_locked(T, Id);
	true when ?IS_NOT_SELECTABLE(P) ->
	    are_all_visible_locked(T, Id);
	true ->
	    false
    end;
are_all_visible_locked([], _) -> true.

toggle_sel_folder(#we{id=Id0,pst=WePst}, #ost{st=#st{selmode=Mode,shapes=Shs,
  sel=Sel0,pst=Pst0}=St0}=Ost) ->
    Folder = gb_trees:get(?FOLDERS, WePst),
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids0} = orddict:fetch(Folder, Fld),
    Ids = gb_sets:to_list(Ids0),
    SelIds0 = orddict:fetch_keys(Sel0),
    SelIds1 = Ids -- SelIds0,
    Sel = case SelIds1 =:= [] of
        true ->
            foldl(fun(Id, A) ->
                orddict:erase(Id, A)
            end, Sel0, lists:delete(Id0, Ids));
        false ->
            Sel2 = foldl(fun(Id, A) ->
                #we{perm=P} = gb_trees:get(Id, Shs),
                case ?IS_SELECTABLE(P) of
                    true ->
                        Items = wings_sel:get_all_items(Mode, Id, St0),
                        orddict:store(Id, Items, A);
                    false -> A
                end
            end, Sel0, Ids),
            case Sel0 =:= Sel2 of
                true ->
                    foldl(fun(Id, A) ->
                        orddict:erase(Id, A)
                    end, Sel0, lists:delete(Id0, Ids));
                false -> Sel2
            end
    end,
    St = St0#st{sel=Sel},
    send_client({new_state,St}),
    get_event(Ost#ost{op=none}).

toggle_sel(#we{id=Id,perm=P}, #ost{st=St0,sel=Sel}=Ost) ->
    case keymember(Id, 1, Sel) of
	false when ?IS_SELECTABLE(P) ->
	    St = wings_sel:select_object(Id, St0),
	    send_client({new_state,St}),
	    get_event(Ost#ost{op=select});
	true ->
	    St = wings_sel:deselect_object(Id, St0),
	    send_client({new_state,St}),
	    get_event(Ost#ost{op=deselect});
	false ->
	    get_event(Ost#ost{op=none})
    end.

toggle_sel_all(We, Ost) ->
    toggle_sel_all_1(We, Ost),
    get_event(Ost#ost{op=none}).

toggle_sel_all_1(_, #ost{sel=[],st=St0}) ->
    St = wings_sel_cmd:select_all(St0),
    send_client({new_state,St});
toggle_sel_all_1(#we{id=Id}, #ost{sel=[{Id,_}],st=St0}) ->
    St = wings_sel_cmd:select_all(St0#st{sel=[]}),
    send_client({new_state,St});
toggle_sel_all_1(#we{id=Id,perm=P}, #ost{st=St}) when ?IS_SELECTABLE(P) ->
    send_client({new_state,wings_sel:select_object(Id, St#st{sel=[]})});
toggle_sel_all_1(_, _) -> ok. 

toggle_wire(#we{id=Id}, #ost{st=St}) ->
    {_,Client} = wings_wm:this(),
    W0 = wings_wm:get_prop(Client, wireframed_objects),
    W = case gb_sets:is_member(Id, W0) of
	    false -> gb_sets:insert(Id, W0);
	    true -> gb_sets:delete(Id, W0)
	end,
    wings_wm:set_prop(Client, wireframed_objects, W),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty().

toggle_wire_all(#we{id=Id0}, #ost{st=St}) ->
    All = wings_shape:all_selectable(St),
    {_,Client} = wings_wm:this(),
    W0 = wings_wm:get_prop(Client, wireframed_objects),
    W1 = gb_sets:difference(W0,All), %% Locked WireFrame
    Id = case gb_sets:is_member(Id0,W0) of
      true -> gb_sets:add(Id0,gb_sets:empty());
      false -> gb_sets:empty()
    end,
    W2 = case gb_sets:is_empty(gb_sets:difference(All,W0)) of
      true -> gb_sets:union(W1,Id);
      false -> 
        case gb_sets:is_member(Id0,W0) of
          true -> gb_sets:union(gb_sets:add(Id0,W1),All);
          false -> 
            case gb_sets:is_empty(gb_sets:difference(gb_sets:delete_any(Id0,All),W0)) of
              true -> gb_sets:union(W1,Id);
              false -> gb_sets:delete_any(Id0,gb_sets:union(W1,All))
            end
        end
    end,
    wings_wm:set_prop(Client, wireframed_objects, W2),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty().

toggle_wire_folder(#we{id=Id0,pst=WePst}, #ost{st=#st{pst=Pst0}=St}) ->
    Folder = gb_trees:get(?FOLDERS, WePst),
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids} = orddict:fetch(Folder, Fld),
    All = gb_sets:intersection(wings_shape:all_selectable(St), Ids),
    {_,Client} = wings_wm:this(),
    W0 = wings_wm:get_prop(Client, wireframed_objects),
    W1 = gb_sets:difference(W0,All), %% Locked WireFrame
    Id = case gb_sets:is_member(Id0,W0) of
      true -> gb_sets:add(Id0,gb_sets:empty());
      false -> gb_sets:empty()
    end,
    W2 = case gb_sets:is_empty(gb_sets:difference(All,W0)) of
      true -> gb_sets:union(W1,Id);
      false -> 
        case gb_sets:is_member(Id0,W0) of
          true -> gb_sets:union(gb_sets:add(Id0,W1),All);
          false -> 
            case gb_sets:is_empty(gb_sets:difference(gb_sets:delete_any(Id0,All),W0)) of
              true -> gb_sets:union(W1,Id);
              false -> gb_sets:delete_any(Id0,gb_sets:union(W1,All))
            end
        end
    end,
    wings_wm:set_prop(Client, wireframed_objects, W2),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty().
%%%
%%% Popup menus.
%%%

do_menu(-1, X, Y, _) ->
    Menu =
        [{?__(7,"Create Folder"),menu_cmd(create_folder)},
         {?__(16,"Remove Selected From Folders"),
             menu_cmd(move_to_folder, ?NO_FLD)}],
    wings_menu:popup_menu(X, Y, objects, Menu);
do_menu(Act, X, Y, #ost{os=Objs}) ->
    Menu = case lists:nth(Act+1, Objs) of
        {_,#we{id=Id,pst=Pst}} ->
            RF = case gb_trees:get(?FOLDERS, Pst) of
                ?NO_FLD -> [];
                _ -> [{?__(17,"Remove From Folder"),menu_cmd(remove_from_folder, Id)}]
            end,
            [{?STR(do_menu,1,"Duplicate"),menu_cmd(duplicate_object, Id),
              ?STR(do_menu,2,"Duplicate selected objects")},
             {?STR(do_menu,3,"Delete"),menu_cmd(delete_object, Id),
              ?STR(do_menu,4,"Delete selected objects")},
             {?STR(do_menu,5,"Rename"),rename_menu(Id),
              {?STR(do_menu,6,"Rename selected objects"),
               ?STR(do_menu,14,"Rename all selected objects"),
               ?STR(do_menu,15,"Rename objects using Search and Replace")},[]},
              separator,
             {?__(7,"Create Folder"),menu_cmd(create_folder)}]++RF;
        Folder ->
            [{?__(11,"Move to Folder"),menu_cmd(move_to_folder, Folder),
              ?__(12,"Move selected objects to this folder")},
             {?__(13,"Empty Folder"),menu_cmd(empty_folder, Folder)},
             {?__(8,"Rename Folder"),menu_cmd(rename_folder, Folder)},
             separator,
             {?__(7,"Create Folder"),menu_cmd(create_folder)},
             {?__(9,"Delete Folder"),menu_cmd(delete_folder, Folder),
              ?__(10,"Delete folder and its contents")}
            ]
    end,
    wings_menu:popup_menu(X, Y, objects, Menu).

rename_menu(Id) ->
    fun(1, _Ns) ->
      button_menu_cmd(rename_object, Id);
       (2, _Ns) ->
      button_menu_cmd(rename_objects, normal);
       (3, _Ns) ->
      button_menu_cmd(rename_objects, masked);
       (_, _) -> ignore
    end.

menu_cmd(Cmd) ->
    {'VALUE',Cmd}.

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

button_menu_cmd(Cmd, Id) ->
	{objects,{Cmd,Id}}.

%%%
%%% Draw the Geometry Graph window.
%%%

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,n=N0}=Ost) ->
    Objs = lists:nthtail(First, Objs0),
    R = right_pos(),
    Lines = lines(Ost),
    N = case N0-First of
      N1 when N1 < Lines -> N1;
      _ -> Lines
    end,
    case wings_pref:get_value(bitmap_icons) of
      true ->
        draw_bitmap_icons(N, Objs, Ost, R, Active-First, Lh-2),
        draw_bitmap_objects_1(N, Objs, Ost, R, Active-First, Lh-2);
      false ->
        draw_icons(N, Objs, Ost, R, Active-First, Lh-2),
        draw_objects_1(N, Objs, Ost, R, Active-First, Lh-2)
    end.

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [{_,#we{name=Name,pst=Pst}}|Wes], #ost{lh=Lh}=Ost, R, Active, Y) ->
    Folder = gb_trees:get(?FOLDERS, Pst),
    NamePos = name_pos(Folder),
    if
    Active =:= 0 ->
      gl:color3f(0.0, 0.0, 0.5),
      gl:recti(NamePos-2, Y-?CHAR_HEIGHT+2, R-2, Y+4),
      gl:color3f(1.0, 1.0, 1.0);
    true -> ok
    end,
    wings_io:text_at(NamePos, Y+2, Name),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh);
draw_objects_1(N, [Folder|Wes], #ost{st=St,lh=Lh}=Ost, R, Active, Y) ->
    NamePos = name_pos(folder),
    if
    Active =:= 0 ->
      gl:color3f(0.0, 0.0, 0.5),
      gl:recti(NamePos-2, Y-?CHAR_HEIGHT+2, R-2, Y+4),
      gl:color3f(1.0, 1.0, 1.0);
    true -> ok
    end,
    FolderInfo = wings_shape:folder_info(Folder, St),
    wings_io:text_at(NamePos, Y+2, Folder++FolderInfo),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh).

draw_bitmap_objects_1(0, _, _, _, _, _) -> ok;
draw_bitmap_objects_1(N, [{_,#we{name=Name,pst=Pst}}|Wes], #ost{lh=Lh}=Ost, R, Active, Y) ->
    Folder = gb_trees:get(?FOLDERS, Pst),
    NamePos = name_pos(Folder),
    if
    Active =:= 0 ->
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl)),
      gl:recti(NamePos-2, Y-?CHAR_HEIGHT+2, R-2, Y+4),
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl_text));
    true ->
      gl:color3fv(wings_pref:get_value(outliner_geograph_text))
    end,
    wings_io:text_at(NamePos, Y+2, Name),
    gl:color3b(0, 0, 0),
    draw_bitmap_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh);
draw_bitmap_objects_1(N, [Folder|Wes], #ost{st=St,lh=Lh}=Ost, R, Active, Y) ->
    NamePos = name_pos(folder),
    if
    Active =:= 0 ->
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl)),
      gl:recti(NamePos-2, Y-?CHAR_HEIGHT+2, R-2, Y+4),
      gl:color3fv(wings_pref:get_value(outliner_geograph_hl_text));
    true ->
      gl:color3fv(wings_pref:get_value(outliner_geograph_text))
    end,
    FolderInfo = wings_shape:folder_info(Folder, St),
    wings_io:text_at(NamePos, Y+2, Folder++FolderInfo),
    gl:color3b(0, 0, 0),
    draw_bitmap_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh).

draw_bitmap_icons(N, Objs, Ost, R, I, Y) ->
    {_,Client} = wings_wm:this(),
    Wires = wings_wm:get_prop(Client, wireframed_objects),
    DrawData = {N,Ost,R,I,Y,Wires},
    wings_io:draw_icons(fun() ->
        foldl(fun draw_bitmap_icons/2, DrawData, Objs)
    end).

draw_icons(N, Objs, Ost, R, I, Y) ->
    {_,Client} = wings_wm:this(),
    Wires = wings_wm:get_prop(Client, wireframed_objects),
    DrawData = {N,Ost,R,I,Y,Wires},
    wings_io:draw_icons(fun() ->
        foldl(fun draw_icons_1/2, DrawData, Objs)
    end).

draw_icons_1(_, done) -> done;
draw_icons_1(_, {0,_,_,_,_,_}) -> done;
draw_icons_1({_,#we{id=Id,perm=Perm,pst=Pst}=We},{N,#ost{sel=Sel,lh=Lh}=Ost,
  R,Active,Y,Wires}) ->
    Folder = gb_trees:get(?FOLDERS, Pst),
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    SelPos = sel_pos(Folder),
    WirePos = wire_pos(),
    Center = (Lh-16) div 2 + 4,
    IconY = Y - Center - 8,
    Wire = gb_sets:is_member(Id, Wires),
    if
      Perm =:= 1; Perm =:= 3 ->
        wings_io:draw_icon(LockPos, IconY, small_locked);
      true ->
        wings_io:draw_icon(LockPos, IconY, small_unlocked)
    end,
    if
      ?IS_VISIBLE(Perm) ->
        wings_io:draw_icon(EyePos, IconY, small_eye),
        case Wire of
          false ->
              wings_io:draw_icon(WirePos, IconY, small_object);
          true ->
              wings_io:draw_icon(WirePos, IconY, small_wire)
        end;
      true ->
        wings_io:draw_icon(EyePos, IconY, small_closed_eye)
    end,
    case keymember(Id, 1, Sel) of
      false when ?IS_ANY_LIGHT(We) ->
        wings_io:draw_icon(SelPos, IconY, small_light);
      false ->
        wings_io:draw_icon(SelPos, IconY, small_object);
      true when ?IS_ANY_LIGHT(We) ->
        wings_io:draw_icon(SelPos, IconY, small_sel_light);
      true ->
        wings_io:draw_icon(SelPos, IconY, small_sel)
    end,
    {N-1,Ost,R,Active-1,Y+Lh,Wires};
draw_icons_1(Folder, {N,#ost{st=#st{sel=Sel,pst=Pst}=St,lh=Lh}=Ost,
  R,Active,Y,Wires}) when is_list(Folder)->
    FolderPos = folder_pos(),
    Center = (Lh-16) div 2 + 4,
    IconY = Y - Center - 8,
    {_,Fld} = gb_trees:get(?FOLDERS, Pst),
    {_,Ids} = orddict:fetch(Folder, Fld),
    SelIds = gb_sets:from_list(orddict:fetch_keys(Sel)),
    S = gb_sets:is_empty(gb_sets:intersection(Ids, SelIds)),
    case wings_shape:folder_status(Folder, St) of
      empty ->
        wings_io:draw_icon(FolderPos+1, IconY, empty_folder);
      closed when S ->
        wings_io:draw_icon(FolderPos+1, IconY, open_folder);
      closed ->
        wings_io:draw_icon(FolderPos+1, IconY, open_folder_sel);
      open when S ->
        wings_io:draw_icon(FolderPos+1, IconY, close_folder);
      open ->
        wings_io:draw_icon(FolderPos+1, IconY, close_folder_sel)
    end,
    {N-1,Ost,R,Active-1,Y+Lh,Wires};
draw_icons_1(_, Acc) -> Acc.

draw_bitmap_icons(_, done) -> done;
draw_bitmap_icons(_, {0,_,_,_,_,_}) -> done;
draw_bitmap_icons({_,#we{id=Id,perm=Perm,pst=Pst}=We}, {N,#ost{sel=Sel,lh=Lh}=Ost,
  R,Active,Y0,Wires}) ->
    Center = (Lh-16) div 2-4,
    Y = Y0-Center,
    Folder = gb_trees:get(?FOLDERS, Pst),
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    SelPos = sel_pos(Folder),
    WirePos = wire_pos(),
    Cube = wings_shape:cube_bitmap(),
    Wire = gb_sets:is_member(Id, Wires),
    DisCol = wings_pref:get_value(outliner_geograph_disabled),
    Tx = wings_pref:get_value(outliner_geograph_text),
    TxHl = wings_pref:get_value(outliner_geograph_hl_text),
    SelCol = wings_pref:get_value(selected_color),

    gl:color3fv({0.6,0.6,0.6}),
    EyeBg = wings_shape:eye_bg_bitmap(),
    draw_bitmap_16(EyePos, Y, EyeBg),
    if
        Perm =:= 1; Perm =:= 3 ->
            gl:color3fv(Tx),
            Lock = wings_shape:locked_bitmap();
        true ->
            gl:color3fv(DisCol),
            Lock = wings_shape:unlocked_bitmap()
    end,
    draw_bitmap_16(LockPos, Y, Lock),
    if
        ?IS_VISIBLE(Perm) ->
            Eye = wings_shape:eye_bitmap(),
            case Wire of
                false ->
                    gl:color3fv(Tx),
                    draw_bitmap_16(WirePos, Y, Cube),
                    gl:color3fv(DisCol),
                    ShadeCube = wings_shape:selcube_bitmap(),
                    draw_bitmap_16(WirePos, Y, ShadeCube);
                true ->
                    gl:color3fv(TxHl),
                    draw_bitmap_16(WirePos, Y, Cube)
            end;
        true ->
            Eye = wings_shape:eye_closed_bitmap()
    end,
    gl:color3fv({0.0,0.0,0.0}),
    draw_bitmap_16(EyePos, Y, Eye),
    %% Selection Icon (Light or Cube)
    gl:color3fv(Tx),
    case ?IS_ANY_LIGHT(We) of
        true ->
            Light = wings_shape:light_bitmap_0(),
            draw_bitmap_16(SelPos, Y, Light),
            Object = wings_shape:light_bitmap_1(),
            case keymember(Id, 1, Sel) of
              true -> gl:color3fv(SelCol);
              false -> gl:color3fv({1.0,1.0,0.5})
            end;
        false ->
            draw_bitmap_16(SelPos, Y, Cube),
            Object = wings_shape:selcube_bitmap(),
            case keymember(Id, 1, Sel) of
                true -> gl:color3fv(SelCol);
                false -> gl:color3fv(DisCol)
            end
    end,
    draw_bitmap_16(SelPos, Y, Object),
    gl:color3b(0, 0, 0),
    {N-1,Ost,R,Active-1,Y0+Lh,Wires};
%% Draw Folder Icons
draw_bitmap_icons(Folder, {N,#ost{st=St,lh=Lh}=Ost,
  R,Active,Y0,Wires}) when is_list(Folder)->
    FolderPos = folder_pos(),
    Center = (Lh-16) div 2-4,
    Y = Y0-Center,
    FolderIcon = case wings_shape:folder_status(Folder, St) of
      empty ->
        wings_shape:empty_folder_bitmap();
      closed ->
        wings_shape:open_folder_bitmap();
      open ->
        wings_shape:closed_folder_bitmap()
    end,
    folder_fill(Folder, FolderPos, Y, St),
    gl:color3b(0, 0, 0),
    draw_bitmap_16(FolderPos, Y, FolderIcon),
    {N-1,Ost,R,Active-1,Y0+Lh,Wires};
draw_bitmap_icons(_, Acc) -> Acc.

folder_fill(Folder, FolderPos, Y, #st{sel=Sel,pst=Pst}) ->
    {_,Fld} = gb_trees:get(?FOLDERS, Pst),
    {_,Ids} = orddict:fetch(Folder, Fld),
    SelIds = gb_sets:from_list(orddict:fetch_keys(Sel)),
    Fill = case gb_sets:is_empty(gb_sets:intersection(Ids, SelIds)) of
        true -> {1.0,0.85,0.0};
        _ -> wings_pref:get_value(selected_color)
    end,
    FolderFill = wings_shape:folder_fill_bitmap(),
    gl:color3fv(Fill),
    draw_bitmap_16(FolderPos, Y, FolderFill).

folder_pos() -> 2.

sel_pos(?NO_FLD) -> 2;
sel_pos(Folder) when is_list(Folder) -> 22.

name_pos(?NO_FLD) -> 22;
name_pos(folder) -> 22;
name_pos(Folder) when is_list(Folder) -> 42.

eye_pos() -> right_pos().

lock_pos() -> right_pos()+16+2.

wire_pos() -> right_pos()+32+3.

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-3*(16+2).

lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.

send_client(Message) ->
    {_,Client} = wings_wm:this(),
    wings_wm:send(Client, Message).

draw_bitmap_16(X, Y, Bitmap) ->
    wings_shape:draw_bitmap_16(X,Y,Bitmap).

alphabetize_folder_objects(#st{shapes=Shs,pst=Pst}) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst),
    {NF,Fld} = case Fld0 of
        [{?NO_FLD,D}|T] -> {[{?NO_FLD,?NO_FLD,D}],T};
        _ -> {[],Fld0}
    end,
    Folders0 = [{wings_util:cap(F),F,FData} || {F,FData} <- Fld],
    Folders = lists:keysort(1, Folders0)++NF,
    alphabetize_folder_objects(Folders, Shs).

alphabetize_folder_objects([{_,Folder,{closed,_}}|Folders], Shs) ->
    [Folder]++alphabetize_folder_objects(Folders, Shs);
alphabetize_folder_objects([{_,Folder,{_,Ids}}|Folders], Shs) ->
    Names0 = foldl(fun(Id, Acc) ->
            #we{name=Name}=We = gb_trees:get(Id, Shs),
            [{wings_util:cap(Name),We}|Acc]
    end, [], gb_sets:to_list(Ids)),
    Names = lists:sort(Names0),
    Items = case Folder of
        ?NO_FLD -> Names;
        _ -> [Folder|Names]
    end,
    Items++alphabetize_folder_objects(Folders, Shs);
alphabetize_folder_objects([], _) -> [].

toggle_folder(Folder, #ost{st=#st{pst=Pst0}=St0}=Ost) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    {Status0,Ids} = orddict:fetch(Folder, Fld0),
    Status = case Status0 of
        open -> closed;
        closed -> open
    end,
    Fld = orddict:store(Folder, {Status,Ids}, Fld0),
    Pst = gb_trees:enter(?FOLDERS, {Folder,Fld}, Pst0),
    St = St0#st{pst=Pst},
    send_client({update_state,St}),
    get_event(Ost).

toggle_folder_all(Folder, #ost{st=#st{pst=Pst0}=St0}=Ost) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    {Status,_} = orddict:fetch(Folder, Fld0),
    Fld = case Status of
        closed ->
            foldl(fun
                ({_,{open,_}}, Fld1) -> Fld1;
                ({F,{_,Ids}}, Fld1) ->
                    orddict:store(F, {open,Ids}, Fld1)
            end, Fld0, Fld0);
        open ->
            Stat0 = [S ||{Foldr,{S,_}} <- Fld0, Foldr=/=Folder, Foldr=/=?NO_FLD ],
            case lists:member(open, Stat0) of
                true ->
                    foldl(fun
                        ({_,{closed,_}}, Fld1) -> Fld1;
                        ({F,{_,_}}, Fld1) when F =:= Folder; F =:= ?NO_FLD  -> Fld1;
                        ({F,{_,Ids}}, Fld1) ->
                            orddict:store(F, {closed,Ids}, Fld1)
                    end, Fld0, Fld0);
                false ->
                    foldl(fun
                        ({_,{open,_}}, Fld1) -> Fld1;
                        ({F,{_,Ids}}, Fld1) ->
                        orddict:store(F, {open,Ids}, Fld1)
                    end, Fld0, Fld0)
            end
    end,
    Pst = gb_trees:enter(?FOLDERS, {Folder,Fld}, Pst0),
    St = St0#st{pst=Pst},
    send_client({update_state,St}),
    get_event(Ost).

create_folder_dialog() ->
    Qs = [{hframe,
	   [{label,?__(1,"Choose Folder Name")},
	    {text,?NEW_FLD,[]}]}],
    wings_dialog:dialog(true, ?__(2,"Create Folder"), Qs,
			fun(Res) -> {create_folder,Res} end).

rename_folder_dialog(OldName) ->
    Qs = [{hframe,
          [{label,?__(1,"Choose Folder Name")},
           {text,OldName,[]}]}],
    wings_dialog:dialog(true, ?__(2,"Rename Folder"), Qs,
			fun(Res) -> {rename_folder,[OldName|Res]} end).

rename_filtered_dialog(ManyObjs) ->
    ModeHook=fun(Me, What, Sto) ->
		     case ManyObjs =:= false of
			 true ->
			     wings_dialog:enable(Me, false, Sto);
			 false ->
			     wings_dialog:enable(rn_search, What =:= 1, Sto)
		     end
	     end,
    I4 = {info,?__(4,"Matching objects to be renamed. *'s may be used as wildcards")},
    I5 = {info,?__(5,"New name. Use % to indicate numbered objects and %number% for the start counter")},
    Qs = [{vframe,
	   [{hradio, [{?__(6,"Selected objects"),0},
		      {?__(7,"Search"),1}], 1,
	     [{key,rn_mode},{title, ?__(8,"Apply to")}, {hook, ModeHook}]},
	    {label_column, [{?__(1,"Search"), {text,"",[I4,{key,rn_search}]}},
			    {?__(2,"Choose Name"), {text,"",[I5,{key,rn_name}]}}]}]}],
    wings_dialog:dialog(true, ?__(3,"Replace"), Qs,
			fun([{rn_mode,Mode},{rn_search,Filter},{rn_name,Mask}]=_Res) ->
				case Mode of
				    0 -> {rename_selected_objects,[Mask]};
				    1 -> {rename_filtered_objects,[Filter,Mask]}
				end
			end).

%%%%%%%% GeomGraph Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% change_state(Window, SelSt) ->
%%     fun(Ev) -> forward_event(Ev, Window, SelSt) end.

%% forward_event(redraw, _Window, _St) -> keep;
%% forward_event({current_state, St}, Window, SelSt0) ->
%%     case (SelSt = get_shape_state(St)) =:= SelSt0 of
%% 	true  -> ignore;
%% 	false -> wx_object:cast(Window, {new_state,SelSt})
%%     end,
%%     {replace, change_state(Window, SelSt)};
%% forward_event({apply, ReturnSt, Fun}, Window, St0) ->
%%     %% Apply ops from window in wings process
%%     case ReturnSt of
%% 	true ->
%% 	    St = Fun(St0),
%% 	    {replace, change_state(Window, St)};
%% 	false ->
%% 	    Fun(St0)
%%     end;
%% forward_event({action,Action}, _Window, _St0) ->
%%     io:format("Got Action ~p~n",[Action]),
%%     keep;
%% forward_event(Ev, Window, _) ->
%%     wx_object:cast(Window, Ev),
%%     keep.

%% get_shape_state(#st{sel=Sel, shapes=Shs}) ->
%%     #{sel => [Id || {Id, _} <- Sel],
%%       shs => [#{id=>Id, name=>Name, perm=>Perm} ||
%% 		 #we{id=Id,name=Name,perm=Perm} <- Shs]}.


