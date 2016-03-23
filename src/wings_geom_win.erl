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
-import(lists, [foldl/3,keymember/3]).


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
	    {{_,DeskY},{DeskW,DeskH}} = wings_wm:win_rect(desktop),
	    W = 28*?CHAR_WIDTH,
	    Pos = {DeskW-5,DeskY+55},
	    Size = {W,DeskH div 2},
	    window({object, wings_wm:this()}, Pos, Size, [], St),
	    keep
    end.

window(Name, Pos0, Size, Ps, St) ->
    Parent = ?GET(top_frame),
    Pos = wxWindow:clientToScreen(Parent, Pos0),
    Shapes = get_shape_state(Name, St),
    Window = wx_object:start_link(?MODULE, [Parent, Pos, Size, Ps, Name, Shapes], []),
    wings_wm:new(Name, Window, {push,change_state(Window, St)}),
    wings_wm:set_dd(Name, geom_display_lists),
    wings_frame:register_win(Window),
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

visibility(Id, single, #st{shapes=Shs}=St0) ->
    St = case gb_trees:get(Id, Shs) of
	     #we{perm=Perm} when ?IS_VISIBLE(Perm) ->
		 wings_shape:hide_object(Id, St0);
	     _ -> wings_shape:show_object(Id, St0)
	 end,
    send_client({update_state,St});
visibility(Id, folder, #st{shapes=Shs, pst=Pst0}=St0) ->
    #we{pst=WePst} = gb_trees:get(Id, Shs),
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
    send_client({update_state,St});
visibility(Id, all, #st{shapes=Shs}=St0) ->
    Objs = gb_trees:values(Shs),
    St = case are_all_visible(Objs, Id) of
	     false -> wings_shape:show_all(St0);
	     true -> wings_shape:hide_others(Id, St0)
	 end,
    send_client({update_state,St}).

selection(Id, single, #st{sel=Sel, shapes=Shs}=St0) ->
    #we{perm=Perm} = gb_trees:get(Id, Shs),
    case keymember(Id, 1, Sel) of
	false when ?IS_SELECTABLE(Perm) ->
	    St = wings_sel:select_object(Id, St0),
	    send_client({new_state,St});
	true ->
	    St = wings_sel:deselect_object(Id, St0),
	    send_client({new_state,St});
	false ->
	    ok
    end;
selection(Id0, folder, #st{shapes=Shs, pst=Pst0, selmode=Mode, sel=Sel0}=St0) ->
    #we{pst=WePst} = gb_trees:get(Id0, Shs),
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
    send_client({new_state,St});
selection(_Id, all, #st{sel=[]}=St0) ->
    St = wings_sel_cmd:select_all(St0),
    send_client({new_state,St});
selection(Id, all, #st{sel=[{Id,_}]}=St0) ->
    St = wings_sel_cmd:select_all(St0#st{sel=[]}),
    send_client({new_state,St});
selection(Id, all, #st{shapes=Shs}=St) ->
    case gb_trees:get(Id, Shs) of
	#we{id=Id,perm=P} when ?IS_SELECTABLE(P) ->
	    send_client({new_state,wings_sel:select_object(Id, St#st{sel=[]})});
	_ ->
	    ok
    end.

wire(Id, Where, St) ->
    This = {_,Client} = wings_wm:this(),
    W = wire(wings_wm:get_prop(Client, wireframed_objects), Id, Where, St),
    wings_wm:set_prop(Client, wireframed_objects, W),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    wx_object:cast(wings_wm:wxwindow(This), {new_state,  get_shape_state(St)}).

wire(W0, Id, single, _St) ->
    case gb_sets:is_member(Id, W0) of
	false -> gb_sets:insert(Id, W0);
	true  -> gb_sets:delete(Id, W0)
    end;
wire(W0, Id0, folder, #st{shapes=Shs, pst=Pst0}=St0) ->
    #we{pst=WePst} = gb_trees:get(Id0, Shs),
    Folder = gb_trees:get(?FOLDERS, WePst),
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids} = orddict:fetch(Folder, Fld),
    All = gb_sets:intersection(wings_shape:all_selectable(St0), Ids),
    {_,Client} = wings_wm:this(),
    W0 = wings_wm:get_prop(Client, wireframed_objects),
    W1 = gb_sets:difference(W0,All), %% Locked WireFrame
    Id = case gb_sets:is_member(Id0,W0) of
	     true -> gb_sets:add(Id0,gb_sets:empty());
	     false -> gb_sets:empty()
	 end,
    case gb_sets:is_empty(gb_sets:difference(All,W0)) of
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
    end;
wire(W0, Id0, all, St) ->
    All = wings_shape:all_selectable(St),
    W1 = gb_sets:difference(W0,All), %% Locked WireFrame
    Id = case gb_sets:is_member(Id0,W0) of
	     true -> gb_sets:add(Id0,gb_sets:empty());
	     false -> gb_sets:empty()
	 end,
    case gb_sets:is_empty(gb_sets:difference(All,W0)) of
	true -> gb_sets:union(W1,Id);
	false ->
	    case gb_sets:is_member(Id0,W0) of
		true -> gb_sets:union(gb_sets:add(Id0,W1),All);
		false ->
		    Set = gb_sets:difference(gb_sets:delete_any(Id0,All),W0),
		    case gb_sets:is_empty(Set) of
			true -> gb_sets:union(W1,Id);
			false -> gb_sets:delete_any(Id0,gb_sets:union(W1,All))
		    end
	    end
    end.

lock(Id, single, #st{shapes=Shs}=St0) ->
    case gb_trees:get(Id, Shs) of
	#we{perm=Perm}  when ?IS_NOT_VISIBLE(Perm) -> keep;
	#we{id=Id,perm=Perm} when ?IS_SELECTABLE(Perm) ->
	    send_client({update_state,wings_shape:lock_object(Id, St0)});
	_ ->
	    send_client({update_state,wings_shape:unlock_object(Id, St0)})
    end;
lock(Id, folder, #st{shapes=Shs, pst=Pst0}=St0) ->
    #we{pst=WePst} = gb_trees:get(Id, Shs),
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
    send_client({update_state,St});
lock(Id, all, #st{shapes=Shs}=St0) ->
    Objs = gb_trees:values(Shs),
    St = case are_all_visible_locked(Objs, Id) of
	     true -> wings_shape:unlock_all(St0);
	     false -> wings_shape:lock_others(Id, St0)
	 end,
    send_client({update_state,St}).

rename(Id, NewName, #st{shapes=Shs}=St) ->
    case gb_trees:get(Id, Shs) of
	#we{name=NewName} -> ignore;
	We0 ->
	    We = We0#we{name=NewName},
	    Shapes = gb_trees:update(Id, We, Shs),
	    send_client({new_state, St#st{shapes=Shapes}})
    end.

action({objects,Action}, St0) ->
    case Action of
	{remove_from_folder,Id} ->
	    {update_state, wings_shape:move_to_folder(?NO_FLD, [Id], St0)};
	{empty_folder,Folder} ->
	    {update_state, wings_shape:empty_folder(Folder, St0)};
	{move_to_folder,Folder} ->
	    {update_state, wings_shape:move_to_folder(Folder, St0)};
	{delete_folder,Folder} ->
	    {new_state,wings_shape:delete_folder(Folder, St0)};
	{delete_object,Id} ->
	    {action,{body,{delete_object,[Id]}}};
	{duplicate_object,Id} ->
	    {action,{body,{duplicate_object,[Id]}}};
	{rename_objects,normal} ->
	    Ids=wings_sel:fold(
		  fun(_,#we{id=Id},Acc) ->
			  Acc++[Id]
		  end, [], St0),
	    {action,{body,{rename,Ids}}};
	{rename_object,Id} ->
	    {action,{body,{rename,[Id]}}};
	{rename_objects,Ids} ->
	    {action,{body,{rename,Ids}}}
    end;
action(Action, St0) ->
    case Action of
	{create_folder,[Folder]} ->
	    {update_state,wings_shape:create_folder(Folder, St0)};
	{move_to_folder, Folder, Ids} ->
	    {update_state, wings_shape:move_to_folder(Folder, Ids, St0)};
	{rename_folder,[OldName,NewName]} ->
	    {update_state, wings_shape:rename_folder(OldName, NewName, St0)};
	{rename_selected_objects,[Mask]} ->
	    {update_state, wings_body:rename_selected(Mask,St0)};
	{rename_filtered_objects,[Filter,Mask]} ->
	    {update_state,wings_body:rename_filtered(Filter,Mask,St0)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
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
    rename_filtered_dialog(length(Sel)>1);
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
get_shape_state({_,Client}, #st{sel=Sel, shapes=Shs, pst=Pst}) ->
    Folds = gb_trees:get(?FOLDERS, Pst),
    #{sel => [Id || {Id, _} <- Sel],
      shs => [#{id=>Id, name=>Name, perm=>Perm, light=>?IS_ANY_LIGHT(We)} ||
		 #we{id=Id,name=Name,perm=Perm} = We <- gb_trees:values(Shs)],
      wire => wings_wm:get_prop(Client, wireframed_objects),
      folders => Folds
     }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {self, name, tw, sp, tc, tree, lc, shown, shapes, drag}).

-define(ICW, column_width()).

init([Parent, Pos, {W,_}=Size, _Ps, {_,Client}=Name, SS]) ->
    Frame = wings_frame:make_external_win(Parent, title(Client), [{size, Size}, {pos, Pos}]),
    #{bg:=BG, text:=FG} = wings_frame:get_colors(),
    Splitter = wxSplitterWindow:new(Frame, [{style, ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE}]),
    wxSplitterWindow:setMinimumPaneSize(Splitter, 1),
    wxSplitterWindow:setSashGravity(Splitter, 0.25),
    wxSplitterWindow:connect(Splitter, enter_window),
    
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
    
    connect_events(TC, LC),
    wxWindow:show(Frame),
    {Splitter, #state{self=self(), name=Name,
		      sp=Splitter,
		      tc=TC, tree=Tree,
		      lc=LC, shown=Shown, shapes=SS}}.

handle_event(#wx{event=#wxSize{size={W,_}}}, #state{lc=LC}=State) ->
    TextWidth = max(30,W-3*column_width()-5),
    wxListCtrl:setColumnWidth(LC, 0, TextWidth),
    {noreply, State#state{tw=TextWidth}};

handle_event(#wx{event=#wxList{type=single, col=1}}, State) ->
    {noreply, State}; %% Ignore Mouse 1
handle_event(#wx{event=#wxList{itemIndex=Indx, col=Col}},
	     #state{lc=LC, name=Name, shown=Shown} = State)
  when (Col =:= 1) orelse (Indx =:= -1) ->
    {shape, Id} = get_id(Indx, Shown),
    Menus = object_menu(Id),
    #wxMouseState{x=X,y=Y} = wx_misc:getMouseState(),
    Cmd = fun(_) -> wings_menu:popup_menu(LC, {X,Y}, objects, Menus) end,
    wings_wm:psend(Name, {apply, false, Cmd}),
    {noreply, State};
handle_event(#wx{event=#wxList{type=command_list_end_label_edit, itemIndex=Indx}},
	     #state{lc=LC, name=Name, shown=Shown} = State) ->
    NewName = wxListCtrl:getItemText(LC, Indx),
    {shape, Id} = get_id(Indx, Shown),
    Apply = fun(St) -> rename(Id, NewName, St), keep end,
    wings_wm:psend(Name, {apply, false, Apply}),
    {noreply, State};

handle_event(#wx{event=#wxTree{type=command_tree_end_label_edit, item=Indx}},
	     #state{name=Name, tree=Tree, tc=TC} = State) ->
    NewName = wxTreeCtrl:getItemText(TC, Indx),
    {_, OldName} = lists:keyfind(Indx, 1, Tree),
    wings_wm:psend(Name, {action, {rename_folder, [OldName, NewName]}}),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_begin_drag, itemIndex=Indx}},
	     #state{lc=LC}=State) ->
    wxWindow:setCursor(?GET(top_frame), wxCursor:new(?wxCURSOR_HAND)),
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

handle_event(#wx{event=#wxMouse{type=enter_window}}, State) ->
    help(),
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
	    wxWindow:setCursor(?GET(top_frame), ?wxNullCursor),
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
    wings ! {external, fun(_) -> wings_wm:delete(Name) end},
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
		 %% Img = {image, image_idx(folder)},
		 Fld = case Fld0 of
			   [{?NO_FLD,_}|T] -> T;
			   T -> T
		       end,
		 Root = wxTreeCtrl:addRoot(TC, ?__(1, "Objects"), []),
		 Sorted = lists:sort([{wings_util:cap(F),F} || {F,_} <- Fld]),
		 Add = fun({_, Name}) -> {wxTreeCtrl:appendItem(TC, Root, Name, []), Name} end,
		 Leaves = lists:map(Add, Sorted),
		 wxTreeCtrl:expand(TC, Root),
		 All = [{Root,?NO_FLD}|Leaves],
		 {Node,_} = lists:keyfind(Curr, 2, All),
		 wxTreeCtrl:selectItem(TC, Node),
		 wxTreeCtrl:ensureVisible(TC, Node),
		 All
	 end,
    wx:batch(Do).

update_shapes(_Sorted, Prev, Prev, _LC) ->
    Prev;
update_shapes(Sorted, #{sel:=Sel, wire:=Wire}, _, LC) ->
    Indx = get_selection(LC),
    Update = wxListCtrl:getItemCount(LC) == length(Sorted),
    Update orelse wxListCtrl:deleteAllItems(LC),
    Add = fun({_, #{id:=Id, name:=Name, perm:=Perm, light:=Light}}, J) ->
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

sort_folder(#{shs:=Shs, folders:={Current, Fld0}}) ->
    case lists:keyfind(Current, 1, Fld0) of
	false -> [];
	{_,{_,Ids}} -> sort_folder(Ids, Shs)
    end.

sort_folder(Ids, Shs) ->
    Names0 = foldl(fun(Id, Acc) ->
			   #{name:=Name}=We = wings_util:mapsfind(Id, id, Shs),
			   [{wings_util:cap(Name),We}|Acc]
		   end, [], gb_sets:to_list(Ids)),
    lists:sort(Names0).

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
    wx:foreach(Add, [perspective, %small_object,
		     body, %, small_sel,
		     small_eye, small_closed_eye,
		     small_unlocked, small_locked,
		     small_light, small_sel_light,
		     edge, %small_wire,
		     close_folder
		    ]),
    IL.

get_id(Indx, SHS) ->
    case lists:nth(Indx+1, SHS) of
	{_, #{id:=Id}} -> {shape, Id};
	Folder -> {folder, Folder}
    end.

connect_events(TC, LC) ->
    wxWindow:connect(TC, command_tree_end_label_edit),
    wxWindow:connect(TC, command_tree_sel_changed),
    wxWindow:connect(TC, command_tree_item_menu, [{skip, true}]),
    %% wxWindow:connect(TC, left_up, [{skip,true}]),

    wxWindow:connect(LC, command_list_begin_drag),
    wxWindow:connect(LC, command_list_end_label_edit),
    wxWindow:connect(LC, size, [{skip, true}]),
    %% See handle_sync_event below for the following callbacks
    wxWindow:connect(LC, left_up, [callback]),
    case os:type() of
    	{win32,_} ->
    	    %% list_item_right_click does not work outside of items
    	    %% on windows use this instead
	    wxWindow:connect(LC, command_left_click, [callback]),
    	    wxWindow:connect(LC, command_right_click, [callback]);
    	_ ->
	    wxWindow:connect(LC, right_up, [callback]),
    	    ok
    end,
    ok.

handle_sync_event(#wx{event=#wxMouse{}} = Ev, _EvObj, #state{drag=Drag, self=Pid})
  when Drag =/= undefined ->
    Pid ! Ev,
    ok;
%% Calc item and column our selves send a generated event
%% a bit tricky to get it working on all OS's
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

event_info(#wxCommand{type=command_left_click}, LC) ->
    #wxMouseState{x=SX,y=SY} = wx_misc:getMouseState(),
    Pos = wxListCtrl:screenToClient(LC, {SX,SY}),
    {ok, single, Pos};
event_info(#wxCommand{type=command_right_click}, LC) ->
    #wxMouseState{x=SX,y=SY} = wx_misc:getMouseState(),
    Pos = wxListCtrl:screenToClient(LC, {SX,SY}),
    {ok, folder, Pos};
event_info(#wxMouse{type=left_up, x=X,y=Y}, _) ->
    {ok, single, {X,Y}};
event_info(#wxMouse{type=right_up, x=X,y=Y}, _LC) ->
    {ok, folder, {X,Y}}.

gen_event(_which, _, {_, -1}, _, _) ->
    ok;
gen_event(_Which, true, {ItemIndex, Col}, Pid, WX) ->
    Pid ! WX#wx{event=#wxList{type=all, itemIndex=ItemIndex, col=Col}},
    ok;
gen_event(Which, false, {ItemIndex, Col}, Pid, WX) ->
    Pid ! WX#wx{event=#wxList{type=Which, itemIndex=ItemIndex, col=Col}},
    ok.


calc_position({X,_}=Pos, TW, LC) ->
    ItemIndex = wxListCtrl:hitTest(LC, Pos, 0),
    Col = case ItemIndex >= 0 of
	      false -> -1;
	      true  ->
		  C0 = column_width()+2, C1 = TW, C2 = C1 + column_width(),
		  C3 = C2 + column_width(), C4 = C3 + column_width(),
		  %% io:format("~p ~p ~n", [X, [C0,C1,C2,C3,C4]]),
		  if X < 0 -> -1;
		     X < C0 -> 0;
		     X < C1 -> 1;
		     X < C2 -> 2;
		     X < C3 -> 3;
		     X < C4 -> 4;
		     true -> -1
		  end
	  end,
    {ItemIndex, Col}.

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
     {?__(8,"Rename Folder"),menu_cmd(rename_folder, Folder)},
     separator,
     {?__(7,"Create Folder"),menu_cmd(create_folder)},
     {?__(9,"Delete Folder"),menu_cmd(delete_folder, Folder),
      ?__(10,"Delete folder and its contents")}
    ].

object_menu(Id) ->
    [{?STR(do_menu,1,"Duplicate"),menu_cmd(duplicate_object, Id),
      ?STR(do_menu,2,"Duplicate selected objects")},
     {?STR(do_menu,3,"Delete"),menu_cmd(delete_object, Id),
      ?STR(do_menu,4,"Delete selected objects")},
     {?STR(do_menu,5,"Rename"),rename_menu(Id),
      {?STR(do_menu,6,"Rename selected objects"),
       ?STR(do_menu,14,"Rename all selected objects"),
       ?STR(do_menu,15,"Rename objects using Search and Replace")},[]},
     separator,
     {?__(7,"Create Folder"),menu_cmd(create_folder)},
     {?__(17,"Remove From Folder"),menu_cmd(remove_from_folder, Id)}].


help() ->
    Msg = [?__(1, "Toggle operation"),
	   wings_msg:mod_format(0, 3, ?__(2, "Toogle all visible objects or show menu")),
	   wings_msg:mod_format(?SHIFT_BITS, 3, ?__(3, "Toggle objects in all folders"))],
    wings_status:message(?MODULE, wings_msg:join(Msg)).
    


are_all_visible([#we{id=Id}|T], Id) ->
    are_all_visible(T, Id);
are_all_visible([#we{perm=P}|T], Id) ->
    case ?IS_VISIBLE(P) of
	false -> false;
	true -> are_all_visible(T, Id)
    end;
are_all_visible([], _) -> true.

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
