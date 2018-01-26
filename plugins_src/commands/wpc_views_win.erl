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
-export([init/0,menu/2,command/2,win_data/1,win_name/0]).
-export([window/1,window/5]).

-export([init/1,
	 handle_call/3, handle_cast/2,
	 handle_event/2, handle_sync_event/3,
	 handle_info/2, code_change/3, terminate/2
	]).

-define(NEED_ESDL, true).
-define(WIN_NAME, {plugin,saved_views}).
-include_lib("wings/src/wings.hrl").

%%%
%%% Saved Views window.
%%%
init() -> true.

menu({window}, Menu) ->
    Menu++[camera_menu()];
menu({view}, Menu) ->
    PatchMenu = fun({String, {views, List}}) ->
			{String, {views, List++[separator, camera_menu()]}};
		   (Entry) -> Entry
		end,
    [PatchMenu(Entry) || Entry <- Menu];
menu(_,Menu) ->
    Menu.

camera_menu() ->
	 {?__(1,"Manage Saved Views"), saved_views,
	  ?__(2,"Shows all saved views")}.

command({window,saved_views}, St) ->
    window(St),
    keep;
command({view,{views, saved_views}}, St) ->
    window(St),
    keep;
command(_,_) ->
	next.

%% win_data/1 function allows many plugin windows to be saved.
%% it returns: {Name, {Horiz alignment, Custom_data}}
%% horiz alignment should be either "left" or "right"
%% custom data is used to store windows properties and custom data - it should be parsed in window/5
win_data(?WIN_NAME=Name) ->
    {Name, {right,[]}}.

win_name() ->
    ?WIN_NAME.

window(St) ->
    case wings_wm:is_window(?WIN_NAME) of
	true ->
	    wings_wm:raise(?WIN_NAME),
	    keep;
	false ->
	    {_DeskW,DeskH} = wings_wm:top_size(),
	    W = 18*?CHAR_WIDTH,
	    Pos = {5,105},
	    Size = {W,DeskH div 3},
	    window(?WIN_NAME, Pos, Size, [], St),
	    keep
    end.

window(WinName, Pos, Size, Ps0, St) ->
    View = get_view_state(St),
    {Frame,Ps} = wings_frame:make_win(title(), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Ps, View]),
    Fs = [{display_data, geom_display_lists}|Ps],
    wings_wm:toplevel(WinName, Window, Fs, {push,change_state(Window, St)}),
    keep.

title() ->
    ?__(1,"Saved views").

%%%%%%%% View Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, _,_}, _Window, _St) -> keep;
forward_event({current_state, St}, Window, St0) ->
    case (New = get_view_state(St)) =:= get_view_state(St0) of
	true  -> ignore;
	false -> wx_object:cast(Window, {new_state, New})
    end,
    {replace, change_state(Window, St)};
forward_event({apply, ReturnSt, Fun}, Window, St0) ->
    %% Apply ops from window in wings process
    case ReturnSt of
	true ->
	    St = Fun(St0),
	    {replace, change_state(Window, St)};
	false ->
	    Fun(St0)
    end;
forward_event({action,{view_win,Cmd}}, _Window, #st{views={_,Views0}}=St0) ->
    case Cmd of
        {save,Geom} -> %% New
            wings_wm:send(Geom, {action, {view, {views, {save,true}}}});
        {rename,_} ->
            wings_wm:send(geom, {action, {view, {views, Cmd}}});
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
    keep;
forward_event({cursor, Cursor}, _, _) ->
    wings_io:set_cursor(Cursor),
    keep;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.

get_view_state(#st{views=Views}) -> Views.

%%%%%%%% View Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside window process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {lc, views, drag}).

init([Frame, _Ps, VS]) ->
    #{bg:=BG, text:=FG} = wings_frame:get_colors(),
    Panel = wxPanel:new(Frame),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    Szr = wxBoxSizer:new(?wxVERTICAL),
    Style = ?wxLC_REPORT bor ?wxLC_NO_HEADER bor ?wxLC_EDIT_LABELS bor ?wxLC_SINGLE_SEL,
    LC = wxListCtrl:new(Panel, [{style, Style}]),
    wxListCtrl:setBackgroundColour(LC, BG),
    wxListCtrl:setForegroundColour(LC, FG),
    wxSizer:add(Szr, LC, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxListCtrl:insertColumn(LC, 0, "", [{width, ?wxLIST_AUTOSIZE_USEHEADER}]),
    wxPanel:setSizer(Panel, Szr),
    update_views(VS, undefined, LC),
    Self = self(),
    IgnoreForPopup = fun(Ev,_) ->
			     case wx_misc:getMouseState() of
				 #wxMouseState{rightDown=true} -> ignore;
				 _ -> Self ! Ev
			     end
		     end,
    wxWindow:connect(LC, command_list_item_selected, [{callback, IgnoreForPopup}]),
    wxWindow:connect(LC, command_list_item_activated),
    wxWindow:connect(LC, enter_window, [{userData, {win, Panel}}]),
    wxWindow:connect(LC, right_up),
    case os:type() of %% Mouse right_up does not arrive on items in windows
	{win32,nt} -> wxWindow:connect(LC, command_list_item_right_click);
	_ -> ok
    end,
    wxWindow:connect(LC, command_list_end_label_edit),
    wxWindow:connect(LC, command_list_begin_drag),
    wxWindow:connect(LC, left_up, [{skip, true}]),
    wxWindow:connect(LC, size, [{skip, true}]),
    wxWindow:connect(LC, char, [callback]),
    {Panel, #state{lc=LC, views=VS}}.

handle_sync_event(#wx{obj=LC,event=#wxKey{type=char, keyCode=KC}}, EvObj, #state{lc=LC}) ->
    Indx = wxListCtrl:getNextItem(LC, -1, [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}]),
    case {key_to_op(KC), validate_param(Indx)} of
	{Act,Param} when Act =/= ignore andalso Param =/=ignore ->
	    wings_wm:psend(?WIN_NAME, {action, {view_win, {Act, Param+1}}});
	_ -> wxEvent:skip(EvObj, [{skip, true}])
    end,
    ok.

key_to_op(?WXK_DELETE) -> delete;
key_to_op(?WXK_F2) -> rename;
key_to_op(_) -> ignore.

validate_param(-1) -> ignore;
validate_param(Param) -> Param.

handle_event(#wx{event=#wxList{type=command_list_end_label_edit, itemIndex=Indx}},
	     #state{views={Curr, Views}, lc=LC} = State) ->
    NewName = wxListCtrl:getItemText(LC, Indx),
    if NewName =/= [] ->
	case element(Indx+1, Views) of
	    {_, NewName} -> ignore;
	    {ViewInfo,_} ->
		Rename = fun(#st{}=St0) ->
				 St = St0#st{views={Curr, setelement(Indx+1, Views, {ViewInfo,NewName})}},
				 wings_wm:send(geom, {new_state,St}),
				 St0 %% Intentional so we get updates to window process
			 end,
		wings_wm:psend(?WIN_NAME, {apply, true, Rename})
	end;
    true ->
	{_, Old} = element(Indx+1, Views),
	wxListCtrl:setItemText(LC, Indx, Old)
    end,
    {noreply, State};

handle_event(#wx{event=#wxSize{size={Width,_}}}, #state{lc=LC}=State) ->
    wxListCtrl:setColumnWidth(LC, 0, Width-20),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated, itemIndex=Indx}},
	     #state{views={_, Vs}}=State) ->
    Indx >= 0 andalso wings_wm:psend(geom_focused(), {action, {view, {views, {jump,Indx+1}}}}),
    {noreply, State#state{views={Indx+1, Vs}}};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Indx}},
	     #state{views={_, Vs}}=State) ->
    Indx >= 0 andalso wings_wm:psend(geom_focused(), {action, {view, {views, {jump,Indx+1}}}}),
    {noreply, State#state{views={Indx+1, Vs}}};

handle_event(#wx{event=#wxMouse{type=right_up}}, State) ->
    invoke_menu(State),
    {noreply, State};
handle_event(#wx{event=#wxList{type=command_list_item_right_click}}, State) ->
    invoke_menu(State),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_begin_drag, itemIndex=Indx}}, #state{lc=LC}=State) ->
    wings_io:set_cursor(pointing_hand),
    wxListCtrl:captureMouse(LC),
    {noreply, State#state{drag=Indx}};
handle_event(#wx{event=#wxMouse{type=left_up, x=X,y=Y}},
	     #state{drag=Drag, views={_, Vs0}, lc=LC}=State) ->
    case Drag of
	undefined ->
	    {noreply, State};
	Drag ->
	    Pos = {X,Y},
	    wxListCtrl:releaseMouse(LC),
	    wings_io:set_cursor(arrow),
	    case handle_drop(hitTest(LC,Pos), Drag, Pos, Vs0, LC) of
		false ->
		    {noreply, State#state{drag=undefined}};
		Vs ->
		    Reorder = fun(#st{}=St0) ->
				      St = St0#st{views=Vs},
				      wings_wm:send(geom, {new_state,St}),
				      St0 %% Intentional so we get updates to window process
			      end,
		    wings_wm:psend(?WIN_NAME, {apply, true, Reorder}),
		    {noreply, State#state{drag=undefined}}
	    end
    end;

handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, State) ->
    wings_frame ! Ev,
    {noreply, State};

handle_event(#wx{} = _Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?WIN_NAME,?LINE, _Ev]),
    {noreply, State}.

hitTest(LC, Pos) ->
    try wxListCtrl:hitTest(LC,Pos) of
        {Index, _, _} -> Index
    catch error:undef -> apply(wxListCtrl,hitTest, [LC,Pos, 0])
    end.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(_Req, _From, State) ->
    %% io:format("~p:~p Got unexpected call ~p~n", [?WIN_NAME,?LINE, _Req]),
    {reply, ok, State}.

handle_cast({new_state, Views}, #state{lc=LC, views=Old}=State) ->
    update_views(Views, Old, LC),
    {noreply, State#state{views=Views}};
handle_cast(_Req, State) ->
    %% io:format("~p:~p Got unexpected cast ~p~n", [?WIN_NAME,?LINE, _Req]),
    {noreply, State}.

handle_info(_Msg, State) ->
    %% io:format("~p:~p Got unexpected info ~p~n", [?WIN_NAME,?LINE, _Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, _) ->
    wings ! {wm, {delete, ?WIN_NAME}},
    normal.

%%%%%%%%%%%%%%%%%%%%%%%%%

update_views(Old, Old, _LC) -> Old;
update_views({Indx, Tuple}=New, _Old, LC) ->
    Add = fun({_View,Name}, Id) ->
		  wxListCtrl:insertItem(LC, Id, Name),
		  Id+1
	  end,
    wxListCtrl:deleteAllItems(LC),
    wx:foldl(Add, 0, tuple_to_list(Tuple)),
    wxListCtrl:setItemState(LC, Indx-1, 16#FFFF, ?wxLIST_STATE_SELECTED),
    New.

get_selection(LC) ->
    case wxListCtrl:getSelectedItemCount(LC) of
	0 -> none;
	1 ->
	    Opts = [{geometry,?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}],
	    wxListCtrl:getNextItem(LC, -1, Opts)
    end.

handle_drop(-1, Drop, {X,Y}=Pos, Vs, LC) ->
    {W,_H} = wxListCtrl:getClientSize(LC),
    if 0 > X -> false;
       X > W -> false;
       0 > Y -> handle_drop(0, Drop, Pos, Vs, LC);
       true  -> handle_drop(tuple_size(Vs), Drop, Pos, Vs, LC)
    end;
handle_drop(Drop, Drop, _Pos, _Vs0, _LC) -> false;
handle_drop(Hit, Drop, _Pos, Vs0, _LC) ->
    DroppedItem = element(Drop+1, Vs0),
    {Hit+1, list_to_tuple(insert_to_list(0, Hit, Drop, DroppedItem, tuple_to_list(Vs0)))}.

insert_to_list(Here, Here, Drop, Item, [Next|Vs]) ->
    [Item,Next|insert_to_list(Here+1,Here,Drop,Item,Vs)];
insert_to_list(Drop, Pos, Drop, Item, [_|Vs]) ->
    insert_to_list(Drop+1, Pos, Drop, Item, Vs);
insert_to_list(Indx, Here, Drop, Item, [This|Vs]) ->
    [This|insert_to_list(Indx+1, Here, Drop, Item, Vs)];
insert_to_list(Indx, Here, _Drop, Item, []) ->
    case Here =:= Indx of
	true -> [Item];
	false -> []
    end.

invoke_menu(#state{views=Views, lc=LC}) ->
    Menus = get_menus(get_selection(LC), Views),
    Pos = wx_misc:getMousePosition(),
    Cmd = fun(_) -> wings_menu:popup_menu(LC, Pos, view_win, Menus) end,
    wings_wm:psend(?WIN_NAME, {apply, false, Cmd}).

get_menus(_, {_, {}}) ->
    views_menu({new,geom_focused()});
get_menus(none, _) ->
    views_menu({new,geom_focused()}) ++ [separator|views_menu(delete_all)];
get_menus(Indx, {_,Views}) ->
    {_,Legend} = element(Indx+1, Views),
    views_menu({Indx+1, Legend}) ++ [separator|get_menus(none, undefined)].

views_menu({new,Geom}) ->
    [{?__(1,"Save New..."),menu_cmd(save,Geom), ?__(2,"Create a new saved view")}];
views_menu(delete_all) ->
    [{?__(9,"Delete All..."), menu_cmd(delete_all,all), ?__(10,"Delete all saved views")}];
views_menu({Idx, Legend}) ->
    [{?__(3,"Replace "),menu_cmd(replace,Idx),
      ?__(4,"Replaces \"")++Legend++"\"["++integer_to_list(Idx)++"]\" settings with the current viewing ones"},
     {?__(5,"Rename..."), menu_cmd(rename,Idx),
      ?__(6,"Rename \"")++Legend++"\"["++integer_to_list(Idx)++"]\"",
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_F2,[]},pretty)}]},
     {?__(7,"Delete"), menu_cmd(delete,Idx),
      ?__(8,"Delete \"")++Legend++"\"["++integer_to_list(Idx)++"]\"",
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_DELETE,[]},pretty)}]}].

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

current() ->
    wings_wm:get_prop(geom_focused(), current_view).

geom_focused() -> geom.
