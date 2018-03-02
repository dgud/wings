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

-export([init/1,
	 handle_call/3, handle_cast/2,
	 handle_event/2, handle_sync_event/3,
	 handle_info/2, code_change/3, terminate/2
	]).

-define(FOLDERS, wings_shape).
-define(NO_FLD, no_folder).
-define(NEW_FLD, "new_folder").

-define(NEED_ESDL, true).
-include("wings.hrl").
-import(lists, [all/2,foldl/3,keymember/3,member/2]).

-dialyzer({nowarn_function, gen_event/5}).

%%%
%%% Geometry Graph window.
%%%

window(St) ->
    Name = {object,wings_wm:this()},
    case wings_wm:is_window(Name) of
	true ->
	    wings_wm:raise(Name),
	    keep;
	false ->
	    {DeskW,DeskH} = wings_wm:top_size(),
	    W = 28*?CHAR_WIDTH,
	    Pos  = {DeskW-50, DeskH div 2},
	    Size = {W, DeskH div 2},
	    window({object, wings_wm:this()}, Pos, Size, [], St),
	    keep
    end.

window({_,Client}=Name, Pos, Size, Ps0, St) ->
    Shapes = get_shape_state(Name, St),
    {Frame,Ps} = wings_frame:make_win(title(Client), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Size, Ps, Name, Shapes]),
    Fs = [{display_data, geom_display_lists}|Ps],
    wings_wm:toplevel(Name, Window, Fs, {push,change_state(Window, St)}),
    keep.

title(geom) ->
    ?STR(title,1,"Geometry Graph");
title({geom,N}) ->
    ?STR(title,2,"Geometry Graph #") ++ integer_to_list(N).

send_client(Message) ->
    {_,Client} = wings_wm:this(),
    wings_wm:send_once_after_redraw(Client, Message).


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
			fun([[]]) -> ignore;
			   (Res) ->
				{rename_folder,[OldName|Res]}
			end).

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

visibility(Id, single, St0) ->
    St = case wings_obj:get(Id, St0) of
	     #{perm:=Perm} when ?IS_VISIBLE(Perm) ->
                 wings_obj:hide([Id], St0);
	     #{} ->
                 wings_obj:unhide([Id], St0)
	 end,
    send_client({update_state,St});
visibility(Id, Action, St0) ->
    Objs = case Action of
               folder ->
                   #{folder:=Folder} = wings_obj:get(Id, St0),
                   other_objs_in_folder(Id, Folder, St0);
               all ->
                   all_other_objects(Id, St0)
           end,
    Ids = [I || #{id:=I} <- Objs],
    St = case are_all_visible(Objs) of
             false ->
                 wings_obj:unhide([Id|Ids], St0);
             true ->
                 wings_obj:hide(Ids, St0)
         end,
    send_client({update_state,St}).

are_all_visible(Objs) ->
    VisibleP = fun(#{perm:=P}) -> ?IS_VISIBLE(P) end,
    all(VisibleP, Objs).

selection(Id, single, St0) ->
    #{perm:=Perm} = wings_obj:get(Id, St0),
    case member(Id, wings_sel:selected_ids(St0)) of
	false when ?IS_SELECTABLE(Perm) ->
	    St = wings_sel:select_object(Id, St0),
	    send_client({new_state,St});
	true ->
	    St = wings_sel:deselect_object(Id, St0),
	    send_client({new_state,St});
	false ->
	    keep
    end;
selection(Id0, Action, #st{selmode=Mode}=St0) ->
    Objs = case Action of
               folder ->
                   #{folder:=Folder} = wings_obj:get(Id0, St0),
                   other_objs_in_folder(Id0, Folder, St0);
               all ->
                   all_other_objects(Id0, St0)
           end,
    Ids = ordsets:from_list([I || #{id:=I} <- Objs]),
    SelIds = wings_sel:selected_ids(St0),
    case ordsets:is_subset(Ids, SelIds) of
        true ->
            %% Unselect all other objects in the folder.
            Sel = gb_sets:from_ordset(Ids),
            F = fun(Items, #we{id=Id}) ->
                        case gb_sets:is_member(Id, Sel) of
                            true -> gb_sets:empty();
                            false -> Items
                        end
                end,
            St = wings_sel:update_sel(F, St0),
            send_client({new_state,St});
        false ->
            %% Select all objects in the folder.
            Sel = gb_sets:from_list([Id0|Ids]),
            F = fun(Items, #we{id=Id}=We) ->
                        case gb_sets:is_member(Id, Sel) of
                            true ->
                                wings_sel:get_all_items(Mode, We);
                            false ->
                                Items
                        end
                end,
            St = wings_sel:update_sel_all(F, St0),
            send_client({new_state,St})
    end.

wire(Id, Where, St) ->
    This = {_,Client} = wings_wm:this(),
    W = wire(wings_wm:get_prop(Client, wireframed_objects), Id, Where, St),
    wings_wm:set_prop(Client, wireframed_objects, W),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    wx_object:cast(wings_wm:wxwindow(This), {new_state,get_shape_state(St)}).

wire(W0, Id, single, _St) ->
    case gb_sets:is_member(Id, W0) of
	false -> gb_sets:insert(Id, W0);
	true  -> gb_sets:delete(Id, W0)
    end;
wire(W0, Id, Action, St) ->
    Objs = case Action of
               folder ->
                   #{folder:=Folder} = wings_obj:get(Id, St),
                   other_objs_in_folder(Id, Folder, St);
               all ->
                   all_other_objects(Id, St)
           end,
    Ids0 = gb_sets:from_list([I || #{id:=I} <- Objs]),
    F = fun(#{id:=I,perm:=P}, A) when ?IS_SELECTABLE(P) -> [I|A];
           (#{}, A) -> A
        end,
    Selectable0 = wings_obj:fold(F, [], St),
    Selectable = gb_sets:from_list(Selectable0),
    Ids = gb_sets:intersection(Selectable, Ids0),
    Keep = gb_sets:difference(W0, Ids),
    Single = gb_sets:singleton(Id),
    case gb_sets:is_disjoint(Ids, W0) of
        false ->
            %% Turn off wire for all expcept Id.
            gb_sets:union(Keep, Single);
        true ->
            %% Turn on wire for all.
            gb_sets:union([Keep,Single,Ids])
    end.

lock(Id, single, St0) ->
    case wings_obj:get(Id, St0) of
	#{perm:=Perm} when ?IS_NOT_VISIBLE(Perm) ->
            keep;
	#{perm:=Perm} when ?IS_SELECTABLE(Perm) ->
            St = wings_obj:lock([Id], St0),
	    send_client({update_state,St});
	#{} ->
            St = wings_obj:unlock([Id], St0),
	    send_client({update_state,St})
    end;
lock(Id, Action, St0) ->
    Objs = case Action of
               folder ->
                   #{folder:=Folder} = wings_obj:get(Id, St0),
                   other_objs_in_folder(Id, Folder, St0);
               all ->
                   all_other_objects(Id, St0)
           end,
    Ids = [I || #{id:=I} <- Objs],
    St = case are_all_visible_locked(Objs) of
             true -> wings_obj:unlock([Id|Ids], St0);
             false -> wings_obj:lock(Ids, St0)
         end,
    send_client({update_state,St}).

are_all_visible_locked(Objs) ->
    Pred = fun(#{perm:=P}) ->
                   ?IS_NOT_VISIBLE(P) orelse ?IS_NOT_SELECTABLE(P)
           end,
    all(Pred, Objs).

rename(Id, NewName, St0) ->
    case wings_obj:get(Id, St0) of
        #{name:=NewName} ->
            ignore;
        Obj0 ->
            Obj = Obj0#{name:=NewName},
            St = wings_obj:put(Obj, St0),
            send_client({new_state,St})
    end.

action({objects,Action}, St0) ->
    case Action of
	{remove_from_folder,Id} ->
	    {update_state,move_to_folder(?NO_FLD, [Id], St0)};
	{empty_folder,Folder} ->
	    {update_state,empty_folder(Folder, St0)};
	{move_to_folder,Folder} ->
	    {update_state,move_to_folder(Folder, St0)};
	{delete_folder,Folder} ->
	    {new_state,delete_folder(Folder, St0)};
	{delete_object,Id} ->
	    {action,{body,{delete_object,[Id]}}};
	{duplicate_object,Id} ->
	    {action,{body,{duplicate_object,[Id]}}};
	{rename_objects,normal} ->
            Ids = wings_sel:selected_ids(St0),
	    {action,{body,{rename,Ids}}};
	{rename_object,Id} ->
	    {action,{body,{rename,[Id]}}};
	{rename_objects,Ids} ->
	    {action,{body,{rename,Ids}}}
    end;
action(Action, St0) ->
    case Action of
	{create_folder,[Folder]} ->
	    {update_state,create_folder(Folder, St0)};
	{move_to_folder, Folder, Ids} ->
	    {update_state,move_to_folder(Folder, Ids, St0)};
	{rename_folder,[OldName,NewName]} ->
	    {update_state,rename_folder(OldName, NewName, St0)};
	{rename_selected_objects,[Mask]} ->
	    {update_state,wings_body:rename_selected(Mask, St0)};
	{rename_filtered_objects,[Filter,Mask]} ->
	    {update_state,wings_body:rename_filtered(Filter, Mask, St0)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Folder handling (inside wings process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_folder(Folder, #st{pst=Pst0}=St) ->
    {_,Fs0} = gb_trees:get(?FOLDERS, Pst0),
    Fs = case orddict:is_key(Folder, Fs0) of
             true ->
                 E = ?__(1,"A folder by that name already exists"),
                 wings_u:error_msg(E);
             false ->
                 orddict:store(Folder, {open,gb_sets:new()}, Fs0)
         end,
    Pst = gb_trees:update(?FOLDERS, {Folder,Fs}, Pst0),
    St#st{pst=Pst}.

%% Delete folder and its contents.
delete_folder(Folder, #st{pst=Pst0}=St0) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    Fld = orddict:erase(Folder, Fld0),
    Current = case Fld of
                  [] -> ?NO_FLD;
                  [{Current0,_}|_] -> Current0
              end,
    Pst = gb_trees:update(?FOLDERS, {Current,Fld}, Pst0),
    Ids = ids_in_folder(Folder, St0),
    St = foldl(fun(Id, S) -> wings_obj:delete(Id, S) end, St0, Ids),
    St#st{pst=Pst}.

move_to_folder(Folder, St) ->
    Ids = wings_sel:selected_ids(St),
    move_to_folder(Folder, Ids, St).

move_to_folder(Folder, Ids0, St) ->
    Ids = gb_sets:from_list(Ids0),
    MF = fun(#{id:=Id}=Obj) ->
                 case gb_sets:is_member(Id, Ids) of
                     false -> Obj;
                     true -> Obj#{folder:=Folder}
                 end
         end,
    wings_obj:map(MF, St).

rename_folder(OldName, OldName, St) ->
    St;
rename_folder(OldName, NewName, St0) ->
    St1 = create_folder(NewName, St0),
    Ids = ids_in_folder(OldName, St1),
    St = move_to_folder(NewName, Ids, St1),
    delete_folder(OldName, St).

empty_folder(Folder, St) ->
    Ids = ids_in_folder(Folder, St),
    move_to_folder(?NO_FLD, Ids, St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utilities (inside wings process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ids_in_folder(Folder, St) ->
    F = fun(#{folder:=F,id:=Id}, A) when F =:= Folder ->
                [Id|A];
           (_, A) ->
                A
        end,
    wings_obj:fold(F, [], St).

other_objs_in_folder(Id, Folder, St) ->
    F = fun(#{folder:=F,id:=I}=Obj, A) when F =:= Folder, Id =/= I ->
                [Obj|A];
           (_, A) ->
                A
        end,
    wings_obj:fold(F, [], St).

all_other_objects(Id, St) ->
    F = fun(#{id:=I}=Obj, A) when Id =/= I ->
                [Obj|A];
           (_, A) ->
                A
        end,
    wings_obj:fold(F, [], St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, _, _}, _Window, _St0) -> keep;
forward_event({current_state, St}, Window, St0) ->
    case (SelSt = get_shape_state(St)) =:= get_shape_state(St0) of
	true  -> ignore;
	false -> wx_object:cast(Window, {new_state,SelSt})
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
forward_event({action,{objects,{rename_objects,masked}}}, _Window, #st{sel=Sel}) ->
    rename_filtered_dialog(length(Sel) > 1);
forward_event({action,{objects, create_folder}}, _, _St) ->
    create_folder_dialog();
forward_event({action,{objects, {rename_folder, OldName}}}, _, _St) ->
    rename_folder_dialog(OldName);
forward_event({action,Action}, _Window, St0) ->
    OpSt = action(Action, St0),
    send_client(OpSt),
    keep;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.

get_shape_state(St) ->
    get_shape_state(wings_wm:this(),St).
get_shape_state({_,Client}, #st{pst=Pst}=St) ->
    Folds0 = gb_trees:get(?FOLDERS, Pst),
    {Current, Folds1} = Folds0,
    Folds = [{Folder, State, length(ids_in_folder(Folder, St))}
             || {Folder, {State, _}} <- Folds1],
    Ids = wings_sel:selected_ids(St),
    F = fun(Obj, A) -> [Obj|A] end,
    Shapes = wings_obj:fold(F, [], St),
    #{sel => Ids,
      shs => Shapes,
      wire => wings_wm:get_prop(Client, wireframed_objects),
      folders => {Current, Folds},
      current => Current
     }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {self, name, tw, sp, tc, tree, lc, shown, shapes, drag}).

-define(ICW, column_width()).

init([Frame, {W,_}, _Ps, Name, SS]) ->
    #{bg:=BG, text:=FG} = wings_frame:get_colors(),
    Splitter = wxSplitterWindow:new(Frame, [{style, ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE}]),
    wxSplitterWindow:setFont(Splitter, ?GET(system_font_wx)),
    wxSplitterWindow:setMinimumPaneSize(Splitter, 1),
    wxSplitterWindow:setSashGravity(Splitter, 0.25),
    
    TreeStyle = ?wxTR_EDIT_LABELS bor ?wxTR_NO_BUTTONS bor ?wxTR_NO_LINES,
    TC  = wxTreeCtrl:new(Splitter, [{style, TreeStyle}]),
    wxTreeCtrl:setBackgroundColour(TC, BG),
    wxTreeCtrl:setForegroundColour(TC, FG),
    wxTreeCtrl:assignImageList(TC, load_icons()),

    LCStyle = ?wxLC_REPORT bor ?wxLC_NO_HEADER bor ?wxLC_EDIT_LABELS bor ?wxLC_SINGLE_SEL,
    LC = wxListCtrl:new(Splitter, [{style, LCStyle}]),
    wxListCtrl:setBackgroundColour(LC, BG),
    wxListCtrl:setForegroundColour(LC, FG),
    wxListCtrl:assignImageList(LC, load_icons(), ?wxIMAGE_LIST_SMALL),
    wxListCtrl:insertColumn(LC, 0, "", [{width, ?wxLIST_AUTOSIZE_USEHEADER}]),
    wxListCtrl:insertColumn(LC, 1, "", [{width, column_width()}]),
    wxListCtrl:insertColumn(LC, 2, "", [{width, column_width()}]),
    wxListCtrl:insertColumn(LC, 3, "", [{width, column_width()}]),

    %% Initialize both first and unsplit if needed, make events work on windows
    wxSplitterWindow:splitVertically(Splitter, TC, LC, [{sashPosition, trunc(W*0.25)}]),

    Tree = case show_folders(maps:get(folders, SS)) of
	       false ->	unsplit_window(Splitter, TC), [];
	       true  -> update_folders(maps:get(folders, SS), TC)
	   end,
    Shown = update_shapes(sort_folder(SS), SS, undefined, LC),
    help(Name),
    connect_events(TC, LC),
    {Splitter, #state{self=self(), name=Name,
		      sp=Splitter,
		      tc=TC, tree=Tree,
		      lc=LC, shown=Shown, shapes=SS}}.

handle_event(#wx{event=#wxSize{size={W,_}}}, #state{lc=LC}=State) ->
    %% wxListCtrl:hasScrollbar(LC, ?wxVERTICAL), Always report false
    TextWidth = max(30,W-4*column_width()), %% One extra for potential scrollbar
    wxListCtrl:setColumnWidth(LC, 0, TextWidth),
    {noreply, State#state{tw=TextWidth}};

handle_event(#wx{event=#wxList{type=Type, itemIndex=Indx, col=Col}},
	     #state{lc=LC, name=Name, shown=Shown} = State)
  when (Col =:= 1) orelse (Indx =:= -1) ->
    case Type of
        single -> {noreply, State};
        all    -> {noreply, State};
        folder when Indx =:= -1 ->
            Menus = folder_menu(?NO_FLD),
            #wxMouseState{x=X,y=Y} = wx_misc:getMouseState(),
            Cmd = fun(_) -> wings_menu:popup_menu(LC, {X,Y}, objects, Menus) end,
            wings_wm:psend(Name, {apply, false, Cmd}),
            {noreply, State};
        folder ->
            {shape, Id} = get_id(Indx, Shown),
            Menus = object_menu(Id),
            #wxMouseState{x=X,y=Y} = wx_misc:getMouseState(),
            Cmd = fun(_) -> wings_menu:popup_menu(LC, {X,Y}, objects, Menus) end,
            wings_wm:psend(Name, {apply, false, Cmd}),
            {noreply, State}
    end;
handle_event(#wx{event=#wxList{type=command_list_end_label_edit, itemIndex=Indx}},
	     #state{lc=LC, name=Name, shown=Shown, shapes=SS} = State) ->
    NewName = wxListCtrl:getItemText(LC, Indx),
    {shape, Id} = get_id(Indx, Shown),
    if NewName =/= [] ->
	Apply = fun(St) -> rename(Id, NewName, St), keep end,
	wings_wm:psend(Name, {apply, false, Apply});
    true ->
	Items = maps:get(shs, SS),
	#{name:=Old} = lists:nth(Id, Items),
	wxListCtrl:setItemText(LC, Indx, Old)
    end,
    {noreply, State};

handle_event(#wx{event=#wxTree{type=command_tree_end_label_edit, item=Indx}},
	     #state{name=Name, tree=Tree, tc=TC} = State) ->
    NewName = wxTreeCtrl:getItemText(TC, Indx),
    case lists:keyfind(Indx, 1, Tree) of
        {_, OldName} ->
            if NewName =/= [] ->
                    wings_wm:psend(Name, {action, {rename_folder, [OldName, NewName]}});
               true ->
                    wxTreeCtrl:setItemText(TC, Indx, OldName)
            end;
        false ->
            io:format("~p:~p: Could not find ~p in ~p~n", [?MODULE, ?LINE, Indx, Tree])
    end,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_begin_drag, itemIndex=Indx}},
	     #state{lc=LC}=State) ->
    wings_io:set_cursor(pointing_hand),
    wxListCtrl:captureMouse(LC),
    {noreply, State#state{drag=Indx}};

handle_event(#wx{event=#wxList{type=Op, itemIndex=Indx, col=Col}},
	     #state{name=Name, shown=Shown} = State) ->
    {shape, Id} = get_id(Indx, Shown),
    Action = col_name(Col),
    Apply = fun(St) -> Action(Id, Op, St), keep end,
    wings_wm:psend(Name, {apply, false, Apply}),
    {noreply, State};

handle_event(#wx{event=#wxTree{type=command_tree_sel_changed, item=Indx}},
	     #state{name=Name, tree=Tree} = State) ->
    {_, Folder} = lists:keyfind(Indx, 1, Tree),
    Apply = fun(#st{pst=Pst0} = St) ->
		    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
		    Pst = gb_trees:enter(?FOLDERS, {Folder,Fld}, Pst0),
		    send_client({update_state,St#st{pst=Pst}}),
		    keep
	    end,
    wings_wm:psend(Name, {apply, false, Apply}),
    {noreply, State};

handle_event(#wx{event=#wxTree{type=command_tree_item_menu, item=Indx, pointDrag=Pos0}},
	     #state{name=Name, tree=Tree, tc=TC} = State) ->
    {_, Folder} = lists:keyfind(Indx, 1, Tree),
    Menus = folder_menu(Folder),
    Pos = wxWindow:clientToScreen(TC, Pos0),
    Cmd = fun(_) -> wings_menu:popup_menu(TC, Pos, objects, Menus) end,
    wings_wm:psend(Name, {apply, false, Cmd}),
    {noreply, State};

handle_event(#wx{obj=TC, event=#wxCommand{type=command_right_click}},
             #state{tc=TC, name=Name} = State) ->
    Menus = folder_menu(?NO_FLD),
    #wxMouseState{x=X,y=Y} = wx_misc:getMouseState(),
    Cmd = fun(_) -> wings_menu:popup_menu(TC, {X,Y}, objects, Menus) end,
    wings_wm:psend(Name, {apply, false, Cmd}),
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, #state{sp=Me}=State) ->
    wings_frame ! Ev#wx{userData={win, Me}},
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=left_up, x=X,y=Y}},
	     #state{name=Name, drag=Drag, lc=LC, tc=TC}=State) ->
    case Drag of
	undefined ->
	    {noreply, State};
	Drag ->
	    Pos0 = {X,Y},
	    Pos1 = wxWindow:clientToScreen(LC, Pos0),
	    Pos  = wxWindow:screenToClient(TC, Pos1),
	    wxListCtrl:releaseMouse(LC),
	    wings_io:set_cursor(arrow),
	    case handle_drop(wxTreeCtrl:hitTest(TC, Pos), State) of
		false ->
		    {noreply, State#state{drag=undefined}};
		Op ->
		    wings_wm:psend(Name, {action, Op}),
		    {noreply, State#state{drag=undefined}}
	    end
    end;

handle_event(#wx{} = _Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, _Ev]),
    {noreply, State}.

handle_drop({0, _}, _St) -> false;
handle_drop({Indx, _}, #state{tree=Tree, drag=Drag, shown=Shown}) ->
    {_, Folder} = lists:keyfind(Indx, 1, Tree),
    {shape, Id} = get_id(Drag, Shown),
    {move_to_folder, Folder, [Id]}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(_Req, _From, State) ->
    %% io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, _Req]),
    {reply, ok, State}.

handle_cast({new_state, New}, #state{shapes=New}=State) ->
    {noreply, State};
handle_cast({new_state, New}, #state{sp=SP, lc=LC, tc=TC, shapes=Old, tree=OldT}=State) ->
    OldF = maps:get(folders,Old),
    Tree =
	case maps:get(folders,New) of
	    OldF -> OldT;
	    NewF ->
		case show_folders(NewF) of
		    true ->
			split_window(SP, TC, LC),
			update_folders(NewF, TC);
		    false ->
			unsplit_window(SP, TC),
			[]
		end
	end,
    Shown = update_shapes(sort_folder(New), New, Old, LC),
    {noreply, State#state{lc=LC, shown=Shown, tree=Tree, shapes=New}};

handle_cast(_Req, State) ->
    %% io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, _Req]),
    {noreply, State}.

handle_info(_Msg, State) ->
    %% io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, _Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{name=Name}) ->
    %% io:format("terminate: ~p:~p (~p)~n",[?MODULE, Name, _Reason]),
    wings ! {wm, {delete, Name}},
    normal.

%%%%%%%%%%%%%%%%%%%%%%

show_folders({?NO_FLD, List}) ->  
    case List of
	[] -> false;
	[_] -> false;
	_ -> true
    end;
show_folders(_) -> true.

split_window(Splitter, TC, LC) ->
    case wxSplitterWindow:isSplit(Splitter) of
	true -> ok;
	false -> wxSplitterWindow:splitVertically(Splitter, TC, LC)
    end.

unsplit_window(Splitter, TC) ->
    case wxSplitterWindow:isSplit(Splitter) of
	false -> ok;
	true  -> wxSplitterWindow:unsplit(Splitter, [{toRemove, TC}])
    end.

update_folders({Curr, Fld0}, TC) ->
    Do = fun() ->
		 wxTreeCtrl:deleteAllItems(TC),
		 [{no_folder,_,S0}|Fld] = Fld0,
		 Caption0 = io_lib:format("~ts (~p)",[?__(1, "Objects"), S0]),
		 Root = wxTreeCtrl:addRoot(TC, Caption0, []),

		 Sorted = lists:sort([{wings_util:cap(F),F,S} || {F,_,S} <- Fld]),
		 Add = fun({_, Name, Qtd}) ->
			    Caption = io_lib:format("~ts (~p)",[Name, Qtd]),
			    {wxTreeCtrl:appendItem(TC, Root, Caption, []), Name}
		       end,
		 Leaves = lists:map(Add, Sorted),
		 wxTreeCtrl:expand(TC, Root),
		 All = [{Root,?NO_FLD}|Leaves],
		 case lists:keyfind(Curr, 2, All) of
		     {Node,_} ->
			 wxTreeCtrl:selectItem(TC, Node),
			 wxTreeCtrl:ensureVisible(TC, Node);
		     _ ->
                         io:format("~p:~p: Unexpected folder error.\nCurr: ~p\nAll: ~p\n\n",
                                   [?MODULE, ?LINE, Curr,All])
		 end,
		 All
	 end,
    wx:batch(Do).

update_shapes(_Sorted, Prev, Prev, _LC) ->
    Prev;
update_shapes(Sorted, #{sel:=Sel, wire:=Wire}, _, LC) ->
    Indx = get_selection(LC),
    Update = wxListCtrl:getItemCount(LC) == length(Sorted),
    Update orelse wxListCtrl:deleteAllItems(LC),
    Add = fun({_, #{id:=Id,name:=Name,perm:=Perm}=Obj}, J) ->
                  Light = maps:is_key(light, Obj),
		  case Update of
		      true  ->
			  Img = [{imageId, bm_sel(Id, Sel, Light)}],
			  wxListCtrl:setItem(LC, J, 0, Name, Img);
		      false ->
			  Img = bm_sel(Id, Sel, Light),
			  wxListCtrl:insertItem(LC, J, Name, Img)
		  end,
		  wxListCtrl:setItemColumnImage(LC, J, 1, bm_visibilty(Perm)),
		  wxListCtrl:setItemColumnImage(LC, J, 2, bm_lock(Perm)),
		  wxListCtrl:setItemColumnImage(LC, J, 3, bm_wire(Id, Wire)),
		  J+1
	  end,
    wx:foldl(Add, 0, Sorted),
    Indx =/= none andalso
	wxListCtrl:setItemState(LC, Indx, 16#FFFF, ?wxLIST_STATE_SELECTED),
    Sorted.

sort_folder(#{shs:=Shs,current:=Current}) ->
    Ns = [{wings_util:cap(Name),Obj} ||
             #{folder:=Folder,name:=Name}=Obj <- Shs,
             Folder =:= Current],
    lists:sort(Ns).

get_selection(LC) ->
    case wxListCtrl:getSelectedItemCount(LC) of
	0 -> none;
	1 ->
	    Opts = [{geometry,?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}],
	    wxListCtrl:getNextItem(LC, -1, Opts)
    end.

bm_sel(Id, Sel, Light) ->
    IsSel = lists:member(Id, Sel),
    if Light, IsSel -> image_idx(sel_light);
       Light -> image_idx(light);
       IsSel -> image_idx(sel);
       true -> image_idx(object)
    end.

bm_lock(Perm) ->
    if Perm =:= 1; Perm =:= 3 -> image_idx(locked);
       true -> image_idx(unlocked)
    end.

bm_visibilty(Perm) ->
    case ?IS_VISIBLE(Perm) of
	true  -> image_idx(eye);
	false -> image_idx(closed)
    end.

bm_wire(Id, Wire) ->
    case gb_sets:is_member(Id, Wire) of
	true  -> image_idx(wire);
	false -> image_idx(object)
    end.

%image_idx(no_image) -> -1;
image_idx(object) -> 0;
image_idx(sel) -> 1;
image_idx(eye) -> 2;
image_idx(closed) -> 3;
image_idx(unlocked) -> 4;
image_idx(locked) -> 5;
image_idx(light) -> 6;
image_idx(sel_light) -> 7;
image_idx(wire) -> 8.
%image_idx(folder) -> 9.

col_name(0) -> fun selection/3;
%col_name(1) -> fun name/3;
col_name(2) -> fun visibility/3;
col_name(3) -> fun lock/3;
col_name(4) -> fun wire/3.

load_icons() ->
    Imgs = wings_frame:get_icon_images(),
    IL = wxImageList:new(16,16),
    %% io:format("~p~n",[Imgs]),
    Add = fun(Name) ->
		  {_, Sz, Img} = lists:keyfind(Name, 1, Imgs),
		  true = wxImage:ok(Img),
		  Q = [{quality,?wxIMAGE_QUALITY_NORMAL}],
		  Small = case Sz of
			      {16,16} -> wxImage:copy(Img);
			      _ -> wxImage:scale(Img,16,16,Q)
			  end,
		  wxImageList:add(IL, wxBitmap:new(Small)),
		  wxImage:destroy(Small)
	  end,
    wx:foreach(Add, [perspective, body,
		     small_eye, small_closed_eye,
		     small_unlocked, small_locked,
		     small_light, small_sel_light,
		     edge
		    ]),
    IL.

get_id(Indx, SHS) ->
    case lists:nth(Indx+1, SHS) of
	{_, #{id:=Id}} -> {shape, Id};
	Folder -> {folder, Folder}
    end.

connect_events(TC, LC) ->
    wxWindow:connect(TC, enter_window),
    wxWindow:connect(TC, command_tree_begin_label_edit, [callback]),
    wxWindow:connect(TC, command_tree_end_label_edit),
    wxWindow:connect(TC, command_tree_sel_changed),
    wxWindow:connect(TC, command_tree_item_menu, [{skip, true}]),
    wxWindow:connect(TC, char, [callback]),
    %% wxWindow:connect(TC, left_up, [{skip,true}]),

    wxWindow:connect(LC, enter_window),
    wxWindow:connect(LC, command_list_begin_drag),
    wxWindow:connect(LC, command_list_end_label_edit),
    wxWindow:connect(LC, size, [{skip, true}]),
    wxWindow:connect(LC, char, [callback]),
    %% See handle_sync_event below for the following callbacks
    wxWindow:connect(LC, left_up, [callback]),
    wxWindow:connect(LC, right_up, [callback]),
    case os:type() of
    	{win32,_} ->
            %% list_item_right_click does not work outside of items
            %% on windows catched by right|left_up above
            wxWindow:connect(TC, command_right_click, []), %% Menu in empty tree area
            wxWindow:connect(LC, command_list_item_right_click, [callback]),
            wxWindow:connect(LC, command_left_click, [callback]);
    	_ ->
    	    ok
    end,
    ok.

handle_sync_event(#wx{event=#wxMouse{}} = Ev, _EvObj, #state{drag=Drag, self=Pid})
  when Drag =/= undefined ->
    Pid ! Ev,
    ok;
%% Calc item and column our selves send a generated event
%% a bit tricky to get it working on all OS's
handle_sync_event(#wx{obj=TC, event=#wxTree{type=command_tree_begin_label_edit, item=Indx}},
		  From, #state{tree=Tree}) ->
    case wxTreeCtrl:getItemParent(TC, Indx) of
	0 -> wxTreeEvent:veto(From);  % 0 is returned for the root item: 'Objects'
	_ ->
	    case lists:keyfind(Indx, 1, Tree) of
		{_, Name} -> wxTreeCtrl:setItemText(TC, Indx, Name);
		_ -> ignore
	    end
    end,
    ok;
handle_sync_event(#wx{obj=Obj,event=#wxKey{type=char, keyCode=KC}}, EvObj,
		  #state{tc=TC, lc=LC, name=Name, shown=Shown, tree=Tree}) ->
    ItemParam =
	case Obj of
	    TC ->
		Indx = wxTreeCtrl:getSelection(TC),
		{_, Folder} = lists:keyfind(Indx, 1, Tree),
		{"folder", Folder};
	    LC ->
		Opts = [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}],
		Indx = wxListCtrl:getNextItem(LC, -1, Opts),
		Id =
		    if Indx >= 0 ->
			{shape, Id0} = get_id(Indx, Shown),
			Id0;
		    true ->
			Indx
		    end,
		{"object", Id}
	end,
    case {key_to_op(KC),validate_item_param(ItemParam)} of
	{Act, {Elm, Param}} when Act =/= ignore ->
	    Cmd = list_to_atom(Act++"_"++Elm),
	    wings_wm:psend(Name, {action, {objects, {Cmd, Param}}});
	_ -> wxEvent:skip(EvObj, [{skip, true}])
    end,
    ok;
handle_sync_event(#wx{obj=LC, event=Event}=Ev, EvObj, #state{lc=LC, tw=TW, self=Pid}) ->
    try
	{ok, Which, Pos} = event_info(Event, LC),
	%% io:format("~p => ~p ~p~n", [Event, Which, Pos]),
	Where = calc_position(Pos, TW, LC),
	Shift = wx_misc:getKeyState(?WXK_SHIFT),
	gen_event(Which, Shift, Where, Pid, Ev),
	case Event of
	    #wxMouse{} -> wxEvent:skip(EvObj);
	    _ -> ok
	end
    catch _:_Reason ->
	    %% Happens when we close the window ignore
	    %% io:format("~p: ~p ~p~n",[?LINE, _Reason, erlang:get_stacktrace()]),
	    ok
    end.

key_to_op(?WXK_DELETE) -> "delete";
key_to_op(?WXK_F2) -> "rename";
key_to_op(_) -> ignore.

validate_item_param({_, no_folder}) -> ignore;
validate_item_param({_, -1}) -> ignore;
validate_item_param({_, _}=ItemParam) -> ItemParam.

event_info(#wxCommand{type=command_left_click}, LC) ->
    #wxMouseState{x=SX,y=SY} = wx_misc:getMouseState(),
    Pos = wxListCtrl:screenToClient(LC, {SX,SY}),
    {ok, single, Pos};
event_info(#wxList{type=command_list_item_right_click}, LC) ->
    #wxMouseState{x=SX,y=SY} = wx_misc:getMouseState(),
    Pos = wxListCtrl:screenToClient(LC, {SX,SY}),
    {ok, folder, Pos};
event_info(#wxMouse{type=left_up, x=X,y=Y}, _) ->
    {ok, single, {X,Y}};
event_info(#wxMouse{type=right_up, x=X,y=Y}, _LC) ->
    {ok, folder, {X,Y}}.

gen_event(_Which, true, {ItemIndex, Col}, Pid, WX) ->
    Pid ! WX#wx{event=#wxList{type=all, itemIndex=ItemIndex, col=Col}},
    ok;
gen_event(Which, false, {ItemIndex, Col}, Pid, WX) ->
    Pid ! WX#wx{event=#wxList{type=Which, itemIndex=ItemIndex, col=Col}},
    ok.

calc_position(Pos, TW, LC) ->
    ItemIndex = try wxListCtrl:hitTest(LC, Pos) of
                    %% SI only works on some windows versions
                    %% so we calc column index ourselves
                    {Index, _Flags, _SI} -> Index
                catch  error:undef ->  %% Erlang-18 and earlier
                        apply(wxListCtrl, hitTest, [LC, Pos, 0])
                end,
    case ItemIndex >= 0 of
        true  -> {ItemIndex, column_index(Pos, TW)};
        false -> {-1, -1}
    end.

column_index({X, _}, TW) ->
    C0 = column_width()+2,
    C1 = TW,
    C2 = C1 + column_width(),
    C3 = C2 + column_width(),
    C4 = C3 + column_width(),
    %% io:format("~p ~p ~n", [X, [C0,C1,C2,C3,C4]]),
    if X < 0 -> -1;
       X < C0 -> 0;
       X < C1 -> 1;
       X < C2 -> 2;
       X < C3 -> 3;
       X < C4 -> 4;
       true -> -1
    end.

column_width() ->
    case os:type() of
	{unix, _} -> 24;
	_ -> 18
    end.

folder_menu(?NO_FLD) ->
    [{?__(7,"Create Folder"),menu_cmd(create_folder)}];
folder_menu(Folder) ->
    [{?__(11,"Move to Folder"),menu_cmd(move_to_folder, Folder),
      ?__(12,"Move selected objects to this folder")},
     {?__(13,"Empty Folder"),menu_cmd(empty_folder, Folder)},
     {?__(8,"Rename Folder"),menu_cmd(rename_folder, Folder),"",
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_F2,[]},pretty)}]},
     separator,
     {?__(7,"Create Folder"),menu_cmd(create_folder)},
     {?__(9,"Delete Folder"),menu_cmd(delete_folder, Folder),
      ?__(10,"Delete folder and its contents"),
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_DELETE,[]},pretty)}]}
    ].

object_menu(Id) ->
    [{?STR(do_menu,1,"Duplicate"),menu_cmd(duplicate_object, Id),
      ?STR(do_menu,2,"Duplicate selected objects")},
     {?STR(do_menu,3,"Delete"),menu_cmd(delete_object, Id),
      ?STR(do_menu,4,"Delete selected objects"),
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_DELETE,[]},pretty)}]},
     {?STR(do_menu,5,"Rename"),rename_menu(Id),
      {?STR(do_menu,6,"Rename selected objects"),
       ?STR(do_menu,14,"Rename all selected objects"),
       ?STR(do_menu,15,"Rename objects using Search and Replace")},
      [{hotkey,wings_hotkey:format_hotkey({?SDLK_F2,[]},pretty)}]},
     separator,
     {?__(7,"Create Folder"),menu_cmd(create_folder)},
     {?__(17,"Remove From Folder"),menu_cmd(remove_from_folder, Id)}].

help(Name) ->
    Msg = [?__(1, "Toggle operation"),
	   wings_msg:mod_format(0, 3, ?__(2, "Toogle all visible objects or show menu")),
	   wings_msg:mod_format(?SHIFT_BITS, 3, ?__(3, "Toggle objects in all folders"))],
    wings_status:message(Name, wings_msg:join(Msg)).

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
