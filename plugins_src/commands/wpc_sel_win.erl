%%
%%  wpc_sel_win.erl --
%%
%%     This module implements the selection commands in a window.
%%
%%  Copyright (c) 2016 Micheus, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_sel_win).
-export([init/0,menu/2,command/2,win_data/1]).
-export([window/1,window/5]).

-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2
	]).

-define(WIN_NAME, {plugin, sel_groups}).
-include_lib("wings/src/wings.hrl").

%%%
%%% Selection groups window.
%%%

init() -> true.

menu({window}, Menu) -> Menu++[sel_group_menu()];
menu({select}, Menu) ->
    PatchMenu = fun({String, {ssels, List}}) ->
			{String, {ssels, List++[separator, sel_group_menu()]}};
		   (Entry) -> Entry
		end,
    [PatchMenu(Entry) || Entry <- Menu];
menu(_,Menu) ->
    Menu.

sel_group_menu() ->
	 {?__(1,"Manage Selection Groups"), sel_groups_win,
	  ?__(2,"Shows the selection groups window")}.

command({window,sel_groups_win}, St) ->
    window(St),
    keep;
command({select, {ssels,sel_groups_win}}, St) ->
    window(St),
    keep;
command({select, {ssels, {rename_group, {Id, Name}}}}, St) ->
    {save_state,rename_group(Id, Name, St)};
command({select, {ssels, {delete_all_groups}}}, St) ->
    {save_state,delete_all_groups(St)};
command(_,_) ->
	next.

%% win_data/1 function allows many plugin windows to be saved.
%% it returns: {Name, {Horiz alignment, Custom_data}}
%% horiz alignment should be either "left" or "right"
%% custom data is used to store windows properties and custom data - it should be parsed in window/5
win_data(?WIN_NAME) ->
    {?WIN_NAME, {right,[]}}.

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
	    window(?WIN_NAME, Pos, Size, [], St)
    end.

window(?WIN_NAME, Pos0, Size, Ps, St) ->
    Parent = ?GET(top_frame),
    Sel = get_sel_state(St),
    Pos = wxWindow:clientToScreen(Parent, Pos0),
    Window = wx_object:start_link(?MODULE, [Parent, Pos, Size, Ps, Sel], []),
    wings_wm:new(?WIN_NAME, Window, {push,change_state(Window, Sel)}),
    wings_wm:set_dd(?WIN_NAME, geom_display_lists),
    wings_frame:register_win(Window),
    keep.

%%%%%%%% Selection Window internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, SelSt) ->
    fun(Ev) -> forward_event(Ev, Window, SelSt) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, St}, Window, SelSt0) ->
    case (SelSt = get_sel_state(St)) =:= SelSt0 of
	true  -> ignore;
	false -> wx_object:cast(Window, {new_state,SelSt})
    end,
    {replace, change_state(Window, SelSt)};
forward_event({apply, false, Fun}, _Window, _SelSt) ->
    %% Apply ops from window in wings process
    Fun();
forward_event({action,{sel_groups,Cmd}}, _Window, _St0) ->
    case Cmd of
	{rename_group,Id} ->
	    rename(Id);
	{delete_groups,Mode} ->
	    delete_groups(Mode);
	{new_group,_} ->
	    wings_wm:send(geom, {action,{select,{ssels,new_group}}});
	_ ->
	    wings_wm:send(geom, {action,{select,{ssels,Cmd}}})
    end,
    keep;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.

get_sel_state(#st{selmode=Mode, sh=Sh, ssels=SavedSels, sel=Sel}) ->
    #{mode=>Mode, sh=>Sh, ssels=>gb_trees:keys(SavedSels), sel=> Sel=/= []}.

rename({_,OldName}=Id) ->
    Qs = [{vframe,
           [{hframe,[
              {label,?__(2,"Current name")++": "},
              {label,OldName}]},
            {hframe,[
              {label,?__(3,"New name")++": "},
              {text,"",[]}]}
           ]}],
    wings_dialog:dialog(?__(1,"Rename"), Qs,
    fun([NewName]) ->
        wings_wm:send(geom, {action,{select,{ssels,{rename_group,{Id,NewName}}}}})
    end).

rename_group({Mode,_}=Key, Name, #st{ssels=Ssels0}=St) ->
    Ssel = gb_trees:get(Key, Ssels0),
    Ssels = gb_trees:insert({Mode,Name}, Ssel, Ssels0),
    St#st{ssels=gb_trees:delete(Key,Ssels)}.

delete_groups(all) ->
    wings_u:yes_no(
      ?__(1,"Are you sure you want to delete all selection groups?"),
      fun() ->
          wings_wm:send(geom,{action,{select,{ssels,{delete_all_groups}}}}),
	      ignore
      end);
delete_groups(invalid) ->
    wings_u:yes_no(
      ?__(2,"Are you sure you want to remove invalid groups?"),
      fun() ->
          wings_wm:send(geom,{action,{select,{ssels,{delete_group,invalid}}}}),
	      ignore
      end).

delete_all_groups(#st{}=St) ->
    St#st{ssels=gb_trees:empty()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {lc, shown, sel, ss}).

init([Parent, Pos, Size, _Ps, SS]) ->
    Frame = wings_frame:make_external_win(Parent, title(), [{size, Size}, {pos, Pos}]),
    Panel = wxPanel:new(Frame),
    Szr = wxBoxSizer:new(?wxVERTICAL),
    Style = ?wxLC_REPORT bor ?wxLC_NO_HEADER bor ?wxLC_EDIT_LABELS,
    LC = wxListCtrl:new(Panel, [{style, Style}]),
    wxSizer:add(Szr, LC, [{proportion,1}, {flag, ?wxEXPAND}]),
    IconImgs = wings_frame:get_icon_images(),
    wxListCtrl:assignImageList(LC, load_icons(IconImgs), ?wxIMAGE_LIST_SMALL),
    wxListCtrl:insertColumn(LC, 0, "", [{width, ?wxLIST_AUTOSIZE_USEHEADER}]),
    Shown = update_sels(maps:get(ssels, SS), [], [], LC),
    wxPanel:setSizer(Panel, Szr),

    Self = self(),
    IgnoreForPopup = fun(Ev,_) ->
			     case wx_misc:getMouseState() of
				 #wxMouseState{rightDown=true} -> ignore;
				 _ -> Self ! Ev
			     end
		     end,
    wxWindow:connect(LC, command_list_item_selected, [{callback, IgnoreForPopup}]),
    wxWindow:connect(LC, command_list_item_activated),
    case os:type() of
	{win32,nt} ->
	    %% list_item_right_click does not work outside of items
	    %% on windows use this instead
	    wxWindow:connect(LC, command_right_click);
	_ ->
	    wxWindow:connect(LC, command_list_item_right_click),
	    ok
    end,
    wxWindow:connect(LC, command_list_end_label_edit),
    wxWindow:connect(LC, size, [{skip, true}]),
    wxWindow:show(Frame),
    {Panel, #state{lc=LC, shown=Shown, ss=SS, sel=none}}.

handle_event(#wx{event=#wxList{type=command_list_item_activated, itemIndex=Indx}},
 	     #state{shown=Shown} = State) ->
    Id = array:get(Indx, Shown),
    wings_wm:psend(geom, {action,{select,{ssels,{select_group,Id}}}}),
    {noreply, State#state{sel=Indx}};

handle_event(#wx{event=#wxList{type=command_list_end_label_edit, itemIndex=Indx}},
	     #state{shown=Shown, lc=LC} = State) ->
    Id = array:get(Indx, Shown),
    NewName = wxListCtrl:getItemText(LC, Indx),
    case Id of
	{_, NewName} -> ignore;
	_ -> wings_wm:psend(geom, {action,{select,{ssels,{rename_group,{Id,NewName}}}}})
    end,
    {noreply, State};

handle_event(#wx{event=#wxSize{size={Width,_}}}, #state{lc=LC}=State) ->
    wxListCtrl:setColumnWidth(LC, 0, Width-20),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Sel}},
	     #state{shown=Shown} = State) ->
    case Sel of
	-1 -> none;
	Indx ->
	    Id = array:get(Indx, Shown),
	    wings_wm:psend(geom, {action,{select,{ssels,{select_group,Id}}}})
    end,
    {noreply, State#state{sel=Sel}};

handle_event(#wx{event=#wxCommand{type=command_right_click}}, State) ->
    invoke_menu(State),
    {noreply, State};
handle_event(#wx{event=#wxList{type=command_list_item_right_click}}, State) ->
    invoke_menu(State),
    {noreply, State};

handle_event(#wx{} = Ev, State) ->
    io:format("~p:~p Got unexpected event ~p~n", [?WIN_NAME,?LINE, Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(Req, _From, State) ->
    io:format("~p:~p Got unexpected call ~p~n", [?WIN_NAME,?LINE, Req]),
    {reply, ok, State}.

handle_cast({new_state, #{ssels:=New} = SS}, #state{lc=LC, ss=Old, shown=OS}=State) ->
    Shown = update_sels(New, Old, OS, LC),
    case SS of
	#{sh:=false, mode:=Mode} ->
	    try array:map(fun(Index, {M, _}) when M =:= Mode ->
				  throw({first, Index});
			     (_, M) -> M
			  end, Shown)
	    catch {first, Index} ->
		    wxListCtrl:ensureVisible(LC, Index)
	    end;
	_ ->
	    ignore
    end,
    {noreply, State#state{lc=LC, shown=Shown, ss=SS}};
handle_cast(_Req, State) ->
    %% io:format("~p:~p Got unexpected cast ~p~n", [?WIN_NAME,?LINE, _Req]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("~p:~p Got unexpected info ~p~n", [?WIN_NAME,?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, _) ->
    io:format("terminate: ~p (~p)~n",[?WIN_NAME, _Reason]),
    wings ! {external, fun(_) -> wings_wm:delete(?WIN_NAME) end},
    normal.

%%%%%%%%%%%%%%%%%%%%%%

update_sels(New, #{ssels:=New}, OS, _LC) ->
    OS;
update_sels(New, _Old, OldShown, LC) ->
    Sorted0 = lists:sort([{image_idx(Type),S}|| {Type,_} = S <- New]),
    Sorted = [S || {_, S} <- Sorted0],
    case array:from_list(Sorted) of
	OldShown ->
	    OldShown;
	Arr ->
	    Add = fun({Image,{_,Name}}, Id) ->
			  wxListCtrl:insertItem(LC, Id, Name, Image),
			  Id+1
		  end,
	    wxListCtrl:deleteAllItems(LC),
	    wx:foldl(Add, 0, Sorted0),
	    Arr
    end.

get_selection(LC) ->
    case wxListCtrl:getSelectedItemCount(LC) of
	0 -> none;
	1 ->
	    Opts = [{geometry,?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}],
	    wxListCtrl:getNextItem(LC, -1, Opts)
    end.

load_icons(Imgs) ->
    IL = wxImageList:new(16,16),
    Add = fun(Name) ->
		  {_, _Sz, Img} = lists:keyfind(Name, 1, Imgs),
		  true = wxImage:ok(Img),
		  Small = wxImage:scale(Img, 16, 16, [{quality, ?wxIMAGE_QUALITY_NORMAL}]),
		  wxImageList:add(IL, wxBitmap:new(Small)),
		  wxImage:destroy(Small)
	  end,
    wx:foreach(Add, [vertex, edge, face, body]),
    IL.

image_idx(vertex) -> 0;
image_idx(edge) -> 1;
image_idx(face) -> 2;
image_idx(body) -> 3.

invoke_menu(#state{sel=Indx, lc=LC, shown=Shown, ss=SS}) ->
    case invoke_menu(Indx, SS, Shown, LC) of
	ignore ->
	    ignore;
	Menus ->
	    Pos = wx_misc:getMousePosition(),
	    Cmd = fun() -> wings_menu:popup_menu(LC, Pos, sel_groups, Menus) end,
	    wings_wm:psend(?WIN_NAME, {apply, false, Cmd})
    end.

invoke_menu(none, #{sel:=false}, Shown, LC) ->
    case array:size(Shown) of
	0 -> ignore;
	_ ->
	    case get_selection(LC) of
		none -> ignore;
		Sel -> group_del_menu(array:get(Sel, Shown))
	    end
    end;
invoke_menu(none, SS, Shown, LC) ->
    case get_selection(LC) of
	none -> group_ins_menu();
	Indx -> invoke_menu(Indx, SS, Shown, LC)
    end;
invoke_menu(Indx, #{sel:=GeomHaveSel}, Shown, LC) ->
    SelIndex = get_selection(LC),
    Current = array:get(Indx, Shown),
    Delete = group_del_menu(Current),
    if SelIndex =:= Indx, GeomHaveSel =:= false ->
	    Delete;
       SelIndex =:= Indx ->
	    group_basic_menu(Current) ++
		[separator|group_ins_menu()] ++
		[separator|Delete];
       SelIndex =:= none, GeomHaveSel ->
	    group_ins_menu();
       SelIndex =:= none ->
	    ignore;
       true ->
	    group_bool_menu(Current, array:get(SelIndex, Shown))
    end.

group_ins_menu() ->
	[{?__(1,"New Group..."),menu_cmd(new_group,0),?__(2,"Create a new selection group")}].
group_del_menu(none) -> [];
group_del_menu({_,SrcName}=SrcId) ->
	[{?__(20,"Rename"), menu_cmd(rename_group,SrcId), ?__(21,"Rename group \"")++SrcName++"\""},
	 {?__(3,"Delete Group"), menu_cmd(delete_group,SrcId), ?__(4,"Delete group \"")++SrcName++"\""},
	 separator,
	 {?__(22,"Delete All"), menu_cmd(delete_groups,all), ?__(23,"Delete all groups")},
	 {?__(24,"Remove Invalid Groups"), menu_cmd(delete_groups,invalid), ?__(25,"Removes all invalid groups")}].
group_basic_menu(none) -> [];
group_basic_menu({_,SrcName}=SrcId) ->
    [{?__(5,"Add to Group"), menu_cmd(add_to_group,SrcId),
      ?__(6,"Add current selection to group \"")++SrcName++"\""},
     {?__(7,"Subtract from Group"), menu_cmd(subtract_from_group,SrcId),
      ?__(8,"Subtract current selection from group \"")++SrcName++"\""}].

group_bool_menu(none,_) -> [];
group_bool_menu(_,none) -> [];
group_bool_menu({_,SrcName},{_,DstName}=DstId) ->
    [	 {?__(11,"Union Group"), menu_cmd(union_group,DstId),
	     ?__(12,"Union group \"")++SrcName++?__(13,"\" with \"")++DstName++"\""},
	 {?__(14,"Subtract Group"), menu_cmd(subtract_group,DstId),
	     ?__(15,"Subtract group \"")++DstName++?__(16,"\" from \"")++SrcName++"\""},
	 {?__(17,"Intersect Group"), menu_cmd(intersect_group,DstId),
	     ?__(18,"Intersect group \"")++SrcName++?__(19,"\" with \"")++DstName++"\""}].

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

title() ->
    ?__(1,"Selection Groups").

