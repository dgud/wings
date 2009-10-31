%%
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,window/1,window/5]).
-export([all_selectable/1]).
-export([show_all/1,unlock_all/1,permissions/3]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,
		keymember/3,keyfind/3,sort/1]).

%%%
%%% Exported functions.
%%%

%% new(Name, We, St0) -> St.
%%  Create a new object having the given name,
%%  converting all unknown materials to default.
new(Name, We0, #st{shapes=Shapes0,onext=Oid,mat=Mat}=St) ->
    UsedMat = wings_facemat:used_materials(We0),
    We = 
	case lists:filter(
	       fun (M) -> not gb_trees:is_defined(M, Mat) end, 
	       UsedMat) of
	    [] -> We0;
	    XMat ->
		FMs = lists:filter(fun ({_,M}) -> lists:member(M, XMat) end,
				   wings_facemat:all(We0)),
		wings_facemat:assign(default, [F||{F,_}<-FMs], We0)
	end,
    Shapes = gb_trees:insert(Oid, We#we{name=Name,id=Oid}, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.

%% new(We, Suffix, St0) -> St.
%%  Suffix = cut | clone | copy | extract | sep
%%
%%  Create a new object based on an old object. The name
%%  will be created from the old name (with digits and known
%%  suffixes stripped) with the given Suffix and a number
%%  appended.
insert(#we{name=OldName}=We0, Suffix, #st{shapes=Shapes0,onext=Oid}=St) ->
    Name = new_name(OldName, Suffix, Oid),
    We = We0#we{id=Oid,name=Name},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    St#st{shapes=Shapes,onext=Oid+1}.
    
replace(Id, We0, #st{shapes=Shapes0}=St) ->
    We = We0#we{id=Id},
    Shapes = gb_trees:update(Id, We, Shapes0),
    St#st{shapes=Shapes}.

permissions(We, Visible, Locked) ->
    P0 = case Visible of
	     true -> 0;
	     false -> 2
	 end,
    P = case Locked of
	    true -> P0 bor 1;
	    false -> P0
	end,
    We#we{perm=P}.

show_all(#st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = [{Id,show_we(We)} || #we{id=Id}=We <- Shs1],
    Shs = gb_trees:from_orddict(Shs2),
    Sel = sort(show_all_sel(Shs1, St, Sel0)),
    St#st{shapes=Shs,sel=Sel}.

unlock_all(#st{shapes=Shs0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = [{Id,maybe_unlock(We)} || #we{id=Id}=We <- Shs1],
    Shs = gb_trees:from_orddict(Shs2),
    St#st{shapes=Shs}.

%% all_selectable(St) -> GbSet
%%  Return a GbSet containing IDs for all selectable objects (i.e. not locked).
all_selectable(#st{shapes=Shs}) ->
    all_selectable_1(gb_trees:to_list(Shs), []).

all_selectable_1([{Id,#we{perm=P}}|T], Acc) when ?IS_SELECTABLE(P) ->
    all_selectable_1(T, [Id|Acc]);
all_selectable_1([_|T], Acc) ->
    all_selectable_1(T, Acc);
all_selectable_1([], Acc) -> gb_sets:from_ordset(reverse(Acc)).

%%%
%%% Local functions follow.
%%%

new_name(OldName, Suffix0, Id) ->
    Suffix = suffix(Suffix0),
    Base = base(reverse(OldName)),
    reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

%% Note: Filename suffixes are intentionally not translated.
%% If we are to translate them in the future, base/1 below
%% must be updated to strip suffixes (both for the current language
%% and for English).

suffix(cut) -> "cut";
suffix(clone) -> "clone";
suffix(copy) -> "copy";
suffix(extract) -> "extract";
suffix(mirror) -> "mirror";
suffix(sep) -> "sep".

%% base_1(ReversedName) -> ReversedBaseName
%%  Given an object name, strip digits and known suffixes to
%%  create a base name. Returns the unchanged name if
%%  no known suffix could be stripped.

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1("enolc_"++Base) -> Base;			%"_clone"
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("rorrim_"++Base) -> Base;		%"_mirror"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1(_Base) -> error.

%%%
%%% Object window.
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
	    keep
    end.

window({_,Client}=Name, Pos, Size, Ps, St) ->
    Title = title(Client),
    Ost = #ost{first=0,lh=18,active=-1},
    Current = {current_state,St},
    Op = {seq,push,event(Current, Ost)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(Name, Title, Pos, Size,
		      [{sizeable,?PANE_COLOR},closable,vscroller,
		       {anchor,ne},{properties,Props}|Ps], Op).

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
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousemotion{x=X,y=Y}, Ost) ->
    Act = active_object(Y, Ost),
    help(Act, active_field(X)),
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
event({action,{objects,Cmd}}, Ost) ->
    command(Cmd, Ost);
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
	   ?STR(help,4,"Toggle visibility of all other objects"));
help(_, lock) ->
    help_1(?STR(help,5,"Lock/unlock active object"),
	   ?STR(help,6,"Lock/unlock all objects"));
help(_, selection) ->
    help_1(?STR(help,7,"Toggle selection for active object"),
	   ?STR(help,8,"Toggle selection for all other objects"));
help(_, wire) ->
    help_1(?STR(help,9,"Toggle shaded/wireframe for active object"),
	   ?STR(help,10,"Toggle shaded/wireframe for all other objects")).

help_1(OneMsg, ThreeMsg) ->
    wings_msg:button(OneMsg, [], ThreeMsg).

command({delete_object,Id}, _) ->
    send_client({action,{body,{delete_object,[Id]}}});
command({duplicate_object,Id}, _) ->
    send_client({action,{body,{duplicate_object,[Id]}}});
command({rename_object,Id}, _) ->
    send_client({action,{body,{rename,[Id]}}});
command(Cmd, _) ->
    io:format("NYI: ~p\n", [Cmd]),
    keep.

update_state(St, #ost{first=OldFirst}=Ost0) ->
    #ost{first=First0} = Ost = update_state_1(St, Ost0),
    case clamp(First0, Ost) of
	OldFirst -> Ost;
	First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

update_state_1(#st{sel=Sel,shapes=Shs}=St, #ost{st=#st{sel=Sel,shapes=Shs}}=Ost) ->
    Ost#ost{st=St};
update_state_1(#st{sel=Sel,shapes=Shs0}=St, #ost{st=#st{sel=Sel},os=Objs}=Ost) ->
    Shs = gb_trees:values(Shs0),
    case have_objects_really_changed(Shs, Objs) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel,os=Shs,n=gb_trees:size(Shs0)};
update_state_1(#st{sel=Sel,shapes=Shs}=St, #ost{st=#st{sel=Sel0}}=Ost) ->
    case has_sel_really_changed(Sel, Sel0) of
	false -> ok;
	true -> wings_wm:dirty()
    end,
    Ost#ost{st=St,sel=Sel,os=gb_trees:values(Shs),n=gb_trees:size(Shs)};
update_state_1(#st{sel=Sel,shapes=Shs}=St, Ost) ->
    Ost#ost{st=St,sel=Sel,os=gb_trees:values(Shs),n=gb_trees:size(Shs)}.

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
    
active_object(Y0, #ost{lh=Lh,first=First,n=N}) ->
    case Y0 of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N -> First+Y;
		_ -> -1
	    end
    end.

active_field(X) ->
    NamePos = name_pos(),
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    WirePos = wire_pos(),
    if
	X < NamePos -> selection;
	X < EyePos -> name;
	X < LockPos -> visibility;
	X < WirePos -> lock;
	true -> wire
    end.

do_action(#mousebutton{button=B}, _) when B > 3 -> keep;
do_action(#mousebutton{x=X,y=Y,button=B,state=S}, #ost{active=Act0,os=Objs}=Ost) ->
    Act = active_object(Y, Ost),
    case active_field(X) of
	name when B =:= 1, S =:= ?SDL_PRESSED ->
	    if
		Act =:= Act0 -> keep;
		true ->
		    wings_wm:dirty(),
		    get_event(Ost#ost{active=Act})
	    end;
	name when B =:= 3, S =:= ?SDL_RELEASED ->
	    {GlobX,GlobY} = wings_wm:local2global(X, Y),
	    do_menu(Act, GlobX, GlobY, Ost);
	Field when S =:= ?SDL_PRESSED ->
	    if
		Act =:= -1 -> keep;
		true ->
		    We = lists:nth(Act+1, Objs),
		    do_action_1(Field, B, We, Ost)
	    end;
	_ -> keep
    end.

do_action_1(visibility, 1, We, Ost) -> toggle_visibility(We, Ost);
do_action_1(visibility, 3, We, Ost) -> toggle_visibility_all(We, Ost);
do_action_1(lock, 1, We, Ost) -> toggle_lock(We, Ost);
do_action_1(lock, 3, We, Ost) -> toggle_lock_all(We, Ost);
do_action_1(selection, 1, We, Ost) -> toggle_sel(We, Ost);
do_action_1(selection, 3, We, Ost) -> toggle_sel_all(We, Ost);
do_action_1(wire, 1, We, Ost) -> toggle_wire(We, Ost);
do_action_1(wire, 3, We, Ost) -> toggle_wire_all(We, Ost);
do_action_1(_, _, _, Ost) -> get_event(Ost).

toggle_visibility(#we{id=Id,perm=Perm}, #ost{st=St0}=Ost) ->
    {Op,St} = if
		  ?IS_VISIBLE(Perm) -> 
		      {hide,hide_object(Id, St0)};
		  true ->
		      {show,show_object(Id, St0)}
	      end,
    send_client({new_state,St}),
    get_event(Ost#ost{op=Op}).

toggle_visibility_all(#we{id=Id}, #ost{os=Objs,st=St0}=Ost) ->
    St = case are_all_visible(Objs, Id) of
	     false -> show_all(St0);
	     true -> hide_others(Id, St0)
	 end,
    send_client({new_state,St}),
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
    send_client({new_state,lock_object(Id, St0)}),
    get_event(Ost#ost{op=lock});
toggle_lock(#we{id=Id}, #ost{st=St0}=Ost) ->
    send_client({new_state,unlock_object(Id, St0)}),
    get_event(Ost#ost{op=unlock}).

toggle_lock_all(#we{id=Id}, #ost{st=St0,os=Objs}=Ost) ->
    St = case are_all_visible_locked(Objs, Id) of
	     true -> unlock_all(St0);
	     false -> lock_others(Id, St0)
	 end,
    send_client({new_state,St}),
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
    All = all_selectable(St),
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

do_menu(-1, _, _, _) -> keep;
do_menu(Act, X, Y, #ost{os=Objs}) ->
    Menu = case lists:nth(Act+1, Objs) of
	       #we{id=Id} ->
		   [{?STR(do_menu,1,"Duplicate"),menu_cmd(duplicate_object, Id),
		     ?STR(do_menu,2,"Duplicate selected objects")},
		    {?STR(do_menu,3,"Delete"),menu_cmd(delete_object, Id),
		     ?STR(do_menu,4,"Delete selected objects")},
		    {?STR(do_menu,5,"Rename"),menu_cmd(rename_object, Id),
		     ?STR(do_menu,6,"Rename selected objects")}]
	   end,
    wings_menu:popup_menu(X, Y, objects, Menu).

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

%%%
%%% Draw the object window.
%%%

draw_objects(#ost{os=Objs0,first=First,lh=Lh,active=Active,n=N0}=Ost) ->
    Objs = lists:nthtail(First, Objs0),
    R = right_pos(),
    Lines = lines(Ost),
    N = case N0-First of
	    N1 when N1 < Lines -> N1;
	    _ -> Lines
	end,
    draw_icons(N, Objs, Ost, R, Active-First, Lh-2),
    draw_objects_1(N, Objs, Ost, R, Active-First, Lh-2).

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [#we{name=Name}|Wes],
	       #ost{lh=Lh}=Ost, R, Active, Y) ->
    if
	Active == 0 ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(name_pos()-2, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	true -> ok
    end,
    wings_io:text_at(name_pos(), Y, Name),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Wes, Ost, R, Active-1, Y+Lh).

draw_icons(N, Objs, Ost, R, I, Y) ->
    {_,Client} = wings_wm:this(),
    Wires = wings_wm:get_prop(Client, wireframed_objects),
    DrawData = {N,Ost,R,I,Y,Wires},
    wings_io:draw_icons(fun() ->
				foldl(fun draw_icons_1/2, DrawData, Objs)
			end).

draw_icons_1(_, done) -> done;
draw_icons_1(_, {0,_,_,_,_,_}) -> done;
draw_icons_1(#we{id=Id,perm=Perm}=We, {N,#ost{sel=Sel,lh=Lh}=Ost,
				       R,Active,Y,Wires}) ->
    EyePos = eye_pos(),
    LockPos = lock_pos(),
    SelPos = sel_pos(),
    WirePos = wire_pos(),
    IconY = Y - 14,
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
draw_icons_1(_, Acc) -> Acc.

sel_pos() ->
    2.

name_pos() ->
    22.

eye_pos() ->
    right_pos().

lock_pos() ->
    right_pos()+16+2.

wire_pos() ->
    right_pos()+32+4.

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-3*(16+2).

lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.

%%%
%%% Utilities.
%%%

hide_object(Id, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We = hide_we(We0, St),
    Shs = gb_trees:update(Id, We, Shs0),
    wings_sel:deselect_object(Id, St#st{shapes=Shs}).

hide_we(#we{id=Id,perm=Perm0}=We, St) ->
    Perm = case get_sel(Id, St) of
	       [] -> Perm0 bor 2;
	       Other -> Other
	   end,
    We#we{perm=Perm}.

show_object(Id, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    Sel = update_sel(We0, St),
    We = show_we(We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{sel=Sel,shapes=Shs}.

show_we(#we{perm=Perm0}=We) ->
    Perm = case Perm0 of
	       3 -> 1;
	       1 -> 1;
	       _ -> 0
	   end,
    We#we{perm=Perm}.

lock_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(1, Id, St).

unlock_object(Id, St) ->
    update_permission(0, Id, St).

update_permission(Perm, Id, #st{shapes=Shs0}=St) ->
    We = gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{perm=Perm}, Shs0),
    Sel = update_sel(We, St),
    St#st{sel=Sel,shapes=Shs}.

hide_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id -> {Id,We};
		  (#we{id=Id}=We) ->
		       {Id,hide_we(We, St)}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    St#st{shapes=Shs,sel=Sel}.

show_all_sel([#we{id=Id,perm={Mode,Set}}|T],
		#st{selmode=Mode}=St, Acc) ->
    show_all_sel(T, St, [{Id,Set}|Acc]);
show_all_sel([#we{id=Id,perm={SMode,Set0}}|T],
		#st{selmode=Mode}=St, Acc) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Set0}]},
    #st{sel=[{Id,Set}]} = wings_sel_conv:mode(Mode, StTemp),
    show_all_sel(T, St, [{Id,Set}|Acc]);
show_all_sel([_|T], St, Acc) ->
    show_all_sel(T, St, Acc);
show_all_sel([], _St, Acc) -> Acc.

lock_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id ->
		       {Id,We};
		  (#we{id=Id,perm=P}=We) when ?IS_VISIBLE(P) ->
		       {Id,We#we{perm=1}};
		  (#we{id=Id}=We) ->
		       {Id,We}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    St#st{shapes=Shs,sel=Sel}.

maybe_unlock(#we{perm=P}=We) when ?IS_VISIBLE(P) -> We#we{perm=0};
maybe_unlock(We) -> We.
    
get_sel(Id, #st{selmode=Mode,sel=Sel}) ->
    case keyfind(Id, 1, Sel) of
	false -> [];
	{Id,Set} -> {Mode,Set}
    end.

update_sel(#we{id=Id,perm={Mode,Set}}, #st{selmode=Mode,sel=Sel}) ->
    sort([{Id,Set}|Sel]);
update_sel(#we{id=Id,perm={SMode,Elems0}}, #st{selmode=Mode,sel=Sel}=St) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Elems0}]},
    #st{sel=[{Id,Elems}]} = wings_sel_conv:mode(Mode, StTemp),
    sort([{Id,Elems}|Sel]);
update_sel(_, #st{sel=Sel}) -> Sel.

send_client(Message) ->
    {_,Client} = wings_wm:this(),
    wings_wm:send(Client, Message).
