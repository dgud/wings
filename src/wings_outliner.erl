%%
%%  wings_outliner.erl --
%%
%%     Maintains the outliner window.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_outliner).
-export([window/1,window/4]).

-export([init/1,
	 handle_call/3, handle_cast/2,
	 handle_event/2, handle_sync_event/3,
	 handle_info/2, code_change/3, terminate/2
	]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [keydelete/3]).

%%%
%%% Outliner window.
%%%

window(St) ->
    case wings_wm:is_window(?MODULE) of
	true ->
	    wings_wm:raise(?MODULE),
	    keep;
	false ->
	    {DeskW, DeskH} = wings_wm:top_size(),
	    W = 28*?CHAR_WIDTH,
	    Pos  = {DeskW-50, 0},
	    Size = {W,DeskH div 2},
	    window(Pos, Size, [], St),
	    keep
    end.

window(Pos, Size, Ps0, St) ->
    Shapes = get_state(St),
    {Frame,Ps} = wings_frame:make_win(title(), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wx_object:start_link(?MODULE, [Frame, Ps, Shapes], []),
    Fs = [{display_data, geom_display_lists}|Ps],
    wings_wm:toplevel(?MODULE, Window, Fs, {push, change_state(Window, St)}),
    keep.

command({edit_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{edit,Name}}});
command({assign_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{assign,Name}}});
command({select_material,Parameters}, _Ost) ->
    wings_wm:send(geom, {action,{material,{select,Parameters}}});
command({duplicate_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{duplicate,[Name]}}});
command({delete_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{delete,[Name]}}});
command({rename_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{rename,[Name]}}});
command({assign_texture,Type,Id,Name0}, #st{mat=Mtab}) ->
    Name = list_to_atom(Name0),
    Mat0 = gb_trees:get(Name, Mtab),
    {Maps0,Mat1} = prop_get_delete(maps, Mat0),
    Maps = [{Type,Id}|keydelete(Type, 1, Maps0)],
    Mat  = [{maps,Maps}|Mat1],
    case Type of
	normal -> wings_image:is_normalmap(Id);
	_ -> ignore
    end,
    wings_wm:send(geom, {action,{material,{update,Name,Mat}}});
command({duplicate_object,Id}, _) ->
    wings_wm:send(geom, {action,{body,{duplicate_object,[Id]}}});
command({delete_object,Id}, _) ->
    wings_wm:send(geom, {action,{body,{delete_object,[Id]}}});
command({rename_object,Id}, _) ->
    wings_wm:send(geom, {action,{body,{rename,[Id]}}});
command({edit_light,Id}, _) ->
    wings_wm:send(geom, {action,{light,{edit,Id}}});
command({show_image,Id}, _) ->
    wings_image:window(Id),
    keep;
command({refresh_image,Id}, _) ->
    refresh_image(Id);
command({duplicate_image,Id}, _) ->
    duplicate_image(Id);
command({delete_image,Id}, Ost) ->
    delete_image(Id, Ost);
command({rename_image,Id}, _) ->
    rename_image(Id);
command({make_internal,Id}, _) ->
    make_internal(Id);
command({make_external,Id}, _) ->
    make_external(Id);
command({export_image,Id}, _) ->
    export_image(Id);
command(Cmd, _) ->
    io:format(?__(1,"NYI: ~p\n"), [Cmd]),
    keep.

prop_get_delete(Key, List) ->
    Val = proplists:get_value(Key, List),
    {Val,keydelete(Key, 1, List)}.

duplicate_image(Id) ->
    #e3d_image{name=Name0} = Im = wings_image:info(Id),
    Name = copy_of(Name0),
    wings_image:new(Name, Im),
    wings_wm:send(geom, need_save),
    keep.

delete_image(Id, St) ->
    Used = wings_material:used_images(St),
    case gb_sets:is_member(Id, Used) of
	true ->
	    wings_u:message(?__(1,"The image is used by a material.")),
	    keep;
	false ->
	    wings_u:yes_no(?__(2,"Are you sure you want to delete the image (NOT undoable)?"),
			   fun() ->
				   wings_image:delete(Id),
				   wings_wm:send(geom, need_save),
				   ignore
			   end, ignore)
    end.

copy_of("Copy of "++_=Name) -> Name;
copy_of(Name) -> "Copy of "++Name.

rename_image(Id) ->
    #e3d_image{name=Name0} = wings_image:info(Id),
    wings_dialog:ask(?__(1,"Rename Image"),
		     [{Name0,Name0,[]}],
		     fun([Name]) when Name =/= Name0 ->
			     wings_image:rename(Id, Name),
			     wings_wm:send(geom, need_save),
			     ignore;
			(_) -> ignore
		     end).

make_external(Id) ->
    Save = fun(Name) ->
		   Image = wings_image:info(Id),
		   Ps = [{image,Image},{filename,Name}],
		   case wings_image:image_write(Ps) of
		       ok ->
			   wings_image:update_filename(Id, Name),
			   rename(Id, Name),
			   keep;
		       {_,Error0} ->
			   Error = Name ++ ": " ++ file:format_error(Error0),
			   wings_u:message(Error)
		   end
	   end,
    #e3d_image{name=ImageName} = wings_image:info(Id),
    Ps = [{extensions,wings_image:image_formats()},
	  {title,?__(1,"Make External")},
	  {default_filename,ImageName}],
    wings_file:export_filename(Ps, Save).

refresh_image(Id) ->
    #e3d_image{filename=Filename} = wings_image:info(Id),
    Props = [{filename,Filename},{alignment,1}],
    case wings_image:image_read(Props) of
	#e3d_image{}=Image ->
	    wings_image:update(Id, Image),
	    keep;
	{error,R} ->
	    Msg = e3d_image:format_error(R),
	    wings_u:message(?__(1,"Failed to refresh \"")
			       ++ Filename ++ "\": " ++ Msg)
    end.

make_internal(Id) ->
    wings_image:update_filename(Id, none),
    keep.

export_image(Id) ->
    Save = fun(Name) ->
		   Image = wings_image:info(Id),
		   Ps = [{image,Image},{filename,Name}],
		   case wings_image:image_write(Ps) of
		       ok ->
			   rename(Id, Name),
			   keep;
		       {_,Error0} ->
			   Error = Name ++ ": " ++ file:format_error(Error0),
			   wings_u:message(Error)
		   end
	   end,
    #e3d_image{name=ImageName} = wings_image:info(Id),
    Ps = [{extensions,wings_image:image_formats()},
	  {default_filename,ImageName}],
    wings_file:export_filename(Ps, Save).

%%%
%%% Drag and drop.
%%%

%% drag_and_drop(Ev, What) ->
%%     DropData = drop_data(What),
%%     {W,_} = wings_wm:win_size(),
%%     wings_wm:drag(Ev, {W-4,?LINE_HEIGHT}, DropData).

%% drop_data({object,Id,_}) -> {object,Id};
%% drop_data({light,Id,_}) -> {light,Id};
%% drop_data({image,Id,Info}) -> {image,Id,Info};
%% drop_data({material,Name,_,_}) -> {material,Name}.

title() ->
    ?__(1,"Outliner").

rename(Id, File) ->
    Name = filename:basename(File),
    wings_image:rename(Id, Name).

rename_obj(Id, NewName, #st{shapes=Shs}=St) ->
    case gb_trees:get(Id, Shs) of
	#we{name=NewName} -> ignore;
	We0 ->
	    We = We0#we{name=NewName},
	    Shapes = gb_trees:update(Id, We, Shs),
	    wings_wm:send(geom, {new_state, St#st{shapes=Shapes}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, _, _}, _Window, _St0) -> keep;
forward_event({current_state, St}, Window, St0) ->
    case (SelSt = get_state(St)) =:= get_state(St0) of
	true  -> ignore;
	false ->
	    wx_object:cast(Window, {new_state,SelSt})
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
forward_event({action,{?MODULE,Cmd}}, _Window, St) ->
    command(Cmd, St);
forward_event({action,{shape,_}}=Act, _, _) ->
    wings_wm:send(geom, Act);
forward_event({note, image_change}, Window, St) ->
    SelSt = get_state(St),
    wx_object:cast(Window, {new_state,SelSt}),
    keep;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.

get_state(#st{mat=Mat,shapes=Shs0}) ->
    Lights = [light_info(We) || We <- gb_trees:values(Shs0),
				?IS_ANY_LIGHT(We)],
    Materials = [mat_info(M) || M <- gb_trees:to_list(Mat)],
    Images = [image_info(Im) || Im <- wings_image:images()],
    Lights ++ Materials ++ Images.

light_info(#we{id=Id,name=Name}) ->
    #{type=>light,id=>Id,name=>Name}.

mat_info({Name,Mp}) ->
    OpenGL = proplists:get_value(opengl, Mp),
    Maps = proplists:get_value(maps, Mp, []),
    {R,G,B,_} = proplists:get_value(diffuse, OpenGL),
    Col = {trunc(R*16),trunc(G*16),trunc(B*16)},
    #{type=>mat, name=>atom_to_list(Name), color=>Col, maps=>Maps}.

image_info({Id, #e3d_image{name=Name}=Im}) ->
    #{type=>image, name=>Name, id=>Id, image=>Im}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {top, szr, tc, os, il, imap, shown, drag}).

init([Frame,  _Ps, Os]) ->
    {IL, IMap0} = load_icons(),
    Panel = wxPanel:new(Frame),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    #{bg:=BG} = Cs = wings_frame:get_colors(),
    Szr = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setBackgroundColour(Panel, BG),
    TC = make_tree(Panel, Cs, IL),
    wxSizer:add(Szr, TC, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Szr),
    {Shown,IMap} = update_object(Os, TC, IL, IMap0),
    wxWindow:connect(TC, enter_window, [{userData, {win, Panel}}]),
    Msg = wings_msg:button_format(?__(1,"Select"), [],
				  ?__(2,"Show outliner menu (if selection)"
				      " or creation menu (if no selection)")),
    wings_status:message(?MODULE, Msg),
    {Panel, #state{top=Panel, szr=Szr, tc=TC, os=Os, shown=Shown, il=IL, imap=IMap}}.

handle_sync_event(#wx{event=#wxTree{type=command_tree_begin_label_edit, item=Indx}}, From,
		  #state{shown=Tree}) ->
    case lists:keyfind(Indx, 1, Tree) of
	{_, #{type:=mat, name:="default"}} -> wxTreeEvent:veto(From);
	false -> wxTreeEvent:veto(From);  % false is returned for the root items: 'Lights', 'Materials', 'Images'
	_ -> ignore
    end,
    ok;
handle_sync_event(#wx{event=#wxTree{item=Indx}}, Drag,
		  #state{tc=TC, shown=Shown}) ->
    case lists:keyfind(Indx, 1, Shown) of
	{_, #{type:=image}=Obj} ->
	    wings_io:set_cursor(pointing_hand),
	    wxTreeEvent:allow(Drag),
	    wx_object:get_pid(TC) ! {drag, Obj};
	_ -> ignore %% for now
    end,
    ok.

handle_event(#wx{event=#wxMouse{type=right_up, x=X, y=Y}}, #state{tc=TC} = State) ->
    {Indx, _} = wxTreeCtrl:hitTest(TC, {X,Y}),
    make_menus(Indx, wxWindow:clientToScreen(TC, {X,Y}), State),
    {noreply, State};
handle_event(#wx{event=#wxTree{type=command_tree_item_menu, item=Indx, pointDrag=Pos}},
	     #state{shown=Tree, tc=TC} = State) ->
    case lists:keyfind(Indx, 1, Tree) of
	false -> ignore;
	_ -> make_menus(Indx, wxWindow:clientToScreen(TC, Pos), State)
    end,
    {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_right_click}}, State) ->
    #wxMouseState{x=X,y=Y} = wx_misc:getMouseState(),
    make_menus(0, {X,Y}, State),
    {noreply, State};
handle_event(#wx{event=#wxTree{type=command_tree_end_label_edit, item=Indx}},
	     #state{shown=Tree, tc=TC} = State) ->
    NewName = wxTreeCtrl:getItemText(TC, Indx),
    if NewName =/= [] ->
	case lists:keyfind(Indx, 1, Tree) of
	    {_, #{type:=mat, name:=Old}} ->
		wings_wm:psend(geom, {action,{material,{rename, Old, NewName}}});
	    {_, #{type:=image, id:=Id}} ->
		Apply = fun(_) ->
				wings_image:rename(Id, NewName),
				wings_wm:psend(geom, need_save),
				keep
			end,
		wings_wm:psend(?MODULE, {apply, false, Apply});
	    {_, #{id:=Id}} ->
		Apply = fun(St) -> rename_obj(Id, NewName, St), keep end,
		wings_wm:psend(?MODULE, {apply, false, Apply})
	end;
    true ->
	case lists:keyfind(Indx, 1, Tree) of
	    {_, #{name:=Old}} ->
		wxTreeCtrl:setItemText(TC, Indx, Old);
	    _ ->
		ignore
	end
    end,
    {noreply, State};

handle_event(#wx{event=#wxTree{type=command_tree_end_drag, item=Indx, pointDrag=Pos0}},
	     #state{shown=Tree, drag=Drag, tc=TC} = State) ->
    wings_io:set_cursor(arrow),
    case lists:keyfind(Indx, 1, Tree) of
	{_, #{type:=mat} = Mat} when Drag =/= undefined ->
	    Menu = handle_drop(Drag, Mat),
	    Pos = wxWindow:clientToScreen(TC, Pos0),
	    Cmd = fun(_) -> wings_menu:popup_menu(TC, Pos, ?MODULE, Menu) end,
	    wings_wm:psend(?MODULE, {apply, false, Cmd});
	_ ->
	    ignore
    end,
    {noreply, State#state{drag=undefined}};

handle_event(#wx{event=#wxTree{type=command_tree_item_activated, item=Indx}},
	     #state{shown=Tree, tc=_TC} = State) ->
    case lists:keyfind(Indx, 1, Tree) of
        false ->
            io:format("~p:~p Unknown Item activated ~p~n", [?MODULE,?LINE, Indx]);
        {_, #{type:=mat, name:=Name}} ->
            wings_wm:psend(?MODULE, {action, {?MODULE, {edit_material, Name}}});
        {_, #{type:=image, id:=Id}} ->
            wings_wm:psend(?MODULE, {action, {?MODULE, {show_image, Id}}});
        {_, #{type:=light, id:=Id}} ->
            wings_wm:psend(?MODULE, {action, {?MODULE, {edit_light, Id}}});
        {_, What} ->
            io:format("~p:~p Item activated ~p~n", [?MODULE,?LINE, What])
    end,
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, State) ->
    wings_frame ! Ev,
    {noreply, State};

handle_event(#wx{} = _Ev, State) ->
    %%io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, _Ev]),
    {noreply, State}.

handle_call(_Req, _From, State) ->
    %%io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, _Req]),
    {reply, ok, State}.

handle_cast({new_state, Os}, #state{tc=TC, il=IL, imap=IMap0} = State) ->
    {Shown,IMap} = update_object(Os, TC, IL, IMap0),
    {noreply, State#state{os=Os, shown=Shown, imap=IMap}};
handle_cast(quit, State) ->
    {noreply, State};
handle_cast(_Req, State) ->
    io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, _Req]),
    {noreply, State}.

handle_info(parent_changed,
	    #state{top=Top, szr=Szr, tc=TC0, os=Os, il=IL, imap=IMap} = State) ->
    case os:type() of
	{win32, _} ->
	    %% Windows or wxWidgets somehow messes up the icons when reparented,
	    %% Recreating the tree ctrl solves it
	    TC = make_tree(Top, wings_frame:get_colors(), IL),
	    wxSizer:replace(Szr, TC0, TC),
	    wxSizer:recalcSizes(Szr),
	    wxWindow:destroy(TC0),
	    wxWindow:connect(TC, enter_window, [{userData, {win, Top}}]),
	    {Shown,_} = update_object(Os, TC, IL, IMap),
	    {noreply, State#state{tc=TC, shown=Shown}};
	_ ->
	    {noreply, State}
    end;
handle_info({drag, Obj}, State) ->
    {noreply, State#state{drag=Obj}};
handle_info(_Msg, State) ->
    %% io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, _Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{}) ->
    wings ! {wm, {delete, ?MODULE}},
    normal.

%%%%%%%%%%%%%%%%%%%%%%

make_tree(Parent, #{bg:=BG, text:=FG}, IL) ->
    TreeStyle = ?wxTR_EDIT_LABELS bor ?wxTR_HIDE_ROOT bor ?wxTR_HAS_BUTTONS
	bor ?wxTR_LINES_AT_ROOT bor ?wxTR_NO_LINES,
    TC = set_pid(wxTreeCtrl:new(Parent, [{style, TreeStyle}]), self()),
    wxTreeCtrl:setBackgroundColour(TC, BG),
    wxTreeCtrl:setForegroundColour(TC, FG),
    wxTreeCtrl:setImageList(TC, IL),
    wxWindow:connect(TC, command_tree_end_label_edit),
    wxWindow:connect(TC, command_tree_begin_label_edit, [callback]),
    wxWindow:connect(TC, command_tree_begin_drag, [callback]),
    wxWindow:connect(TC, command_tree_end_drag, []),
    wxWindow:connect(TC, command_tree_item_activated, []),
    case os:type() of
	{win32, _} ->
	    wxWindow:connect(TC, command_tree_item_menu, [{skip, false}]),
	    wxWindow:connect(TC, command_right_click, []);
	_ ->
	    wxWindow:connect(TC, right_up, [{skip, true}])
    end,
    TC.

update_object(Os, TC, IL, Imap0) ->
    Sorted = [{{order(T), wings_util:cap(N)},O} || #{type:=T,name:=N} = O <- Os],
    wxTreeCtrl:deleteAllItems(TC),
    Root = wxTreeCtrl:addRoot(TC, []),
    Lights = wxTreeCtrl:appendItem(TC, Root, root_name(light)),
    Materials = wxTreeCtrl:appendItem(TC, Root, root_name(mat)),
    Images = wxTreeCtrl:appendItem(TC, Root, root_name(image)),
    Do = fun({_, #{type:=Type, name:=Name}=O}, {Acc0,Imap00}) ->
		 {Indx, Imap} = image_index(O, IL, Imap00),
		 Item =
		     case Type of
			 light -> wxTreeCtrl:appendItem(TC, Lights, Name, [{image, Indx}]);
			 mat -> wxTreeCtrl:appendItem(TC, Materials, Name, [{image, Indx}]);
			 image -> wxTreeCtrl:appendItem(TC, Images, Name, [{image, Indx}])
		     end,
		 Acc = [{Item, O}|Acc0],
		 if Type =:= mat ->
		     case maps:get(maps, O, []) of
			 [] -> {Acc, Imap};
			 Maps ->
			     add_maps(Maps, TC, Item, IL, Imap, Os, Acc)
		     end;
		 true -> {Acc, Imap}
		 end
		 %% {Node,_} = lists:keyfind(Curr, 2, All),
		 %% wxTreeCtrl:selectItem(TC, Node),
		 %% wxTreeCtrl:ensureVisible(TC, Node),
	 end,
    Res = wx:foldl(Do, {[],Imap0}, Sorted),
    wxTreeCtrl:expand(TC, Lights),
    wxTreeCtrl:setItemBold(TC, Lights),
    wxTreeCtrl:expand(TC, Materials),
    wxTreeCtrl:setItemBold(TC, Materials),
    wxTreeCtrl:expand(TC, Images),
    wxTreeCtrl:setItemBold(TC, Images),
    wxTreeCtrl:selectItem(TC, Lights),
    Res.

add_maps([{MType,Mid}|Rest], TC, Dir, IL, Imap0, Os,Acc) ->
    case [O || #{type:=image, id:=Id} = O <- Os, Id =:= Mid] of
	[O = #{name:=MName}] ->
	    Indx = image_maps_index(MType),
	    Item = wxTreeCtrl:appendItem(TC, Dir, MName, [{image, Indx}]),
	    add_maps(Rest, TC, Dir, IL, Imap0, Os, [{Item, O}|Acc]);
	[] ->
	    add_maps(Rest, TC, Dir, IL, Imap0, Os, Acc)
    end;
add_maps([], _TC, _Dir, _, Imap, _, Acc) ->
    %% wxTreeCtrl:expand(TC,Dir),
    {Acc, Imap}.

order(light) -> 1;
order(mat)   -> 2;
order(image) -> 3.

root_name(light) -> ?__(1, "Lights");
root_name(mat) -> ?__(2, "Materials");
root_name(image) -> ?__(3, "Images").

image_index(#{type:=light}, _IL, Map) ->
    #{light:=Index} = Map,
    {Index, Map};
image_index(#{type:=image, image:=#e3d_image{filename=FName}}, _IL, Map) ->
    Index = case FName of
		none -> maps:get(internal_image, Map);
		_ ->    maps:get(image, Map)
	    end,
    {Index, Map};
image_index(#{type:=mat, color:=Col}, IL, Map) ->
    case maps:get(Col, Map, undefined) of
	undefined ->
	    Indx = wxImageList:getImageCount(IL),
	    wxImageList:add(IL, mat_bitmap(Col)),
	    {Indx, Map#{Col=>Indx}};
	Indx ->
	    {Indx, Map}
    end.

image_maps_index(Type) ->
    case Type of
    	diffuse -> 4;
	gloss -> 5;
	bump -> 6;
	normal -> 7;
	material -> 8;
	_ -> undefined
    end.

load_icons() ->
    Imgs = wings_frame:get_icon_images(),
    IL = wxImageList:new(16,16),
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
    wx:foreach(Add, [
		     small_image,small_image2,perspective, %small_object,
		     small_light,
		     small_diffuse,small_gloss,small_bump,small_normal,
		     material
		    ]),
    {IL, #{image=>0, internal_image=>1, object=>2, light=>3, mat=>8}}.

mat_bitmap(Col) ->
    {_, _, Orig} = lists:keyfind(material, 1, wings_frame:get_icon_images()),
    Image = wxImage:copy(Orig),
    RGB = wxImage:getData(Image),
    Alpha = wxImage:getAlpha(Image),
    RGBNew = mat_bitmap_masked(RGB, Col),
    wxImage:setData(Image, RGBNew),
    wxImage:setAlpha(Image, Alpha),
    BM = wxBitmap:new(Image),
    wxImage:destroy(Image),
    BM.

mat_bitmap_masked(RGB, Col) ->
    mat_bitmap_masked(RGB, Col, <<>>).

mat_bitmap_masked(<<R:8,_G:8,_B:8,RGB/binary>>, {Rc,Gc,Bc}=Col, Acc) ->
    Mul = R/255,
    R0 = min(trunc(Mul*Rc*16), 255),
    G0 = min(trunc(Mul*Gc*16), 255),
    B0 = min(trunc(Mul*Bc*16), 255),
    mat_bitmap_masked(RGB, Col, <<Acc/binary,R0:8,G0:8,B0:8>>);
mat_bitmap_masked(<<>>, _, Acc) -> Acc.

%% missing wx_object function
set_pid({wx_ref, Ref, Type, []}, Pid) when is_pid(Pid) ->
    {wx_ref, Ref, Type, Pid}.

%%%%%%%%%%%
make_menus(0, Pos, #state{tc=TC}) ->
    wings_wm:psend(?MODULE, {apply, false, fun(_) -> wings_shapes:menu(TC,Pos) end});
make_menus(Indx, Pos, #state{tc=TC, shown=Tree}) ->
    case lists:keyfind(Indx, 1, Tree) of
        {_, Obj} ->
            Menus = do_menu(Obj),
            Cmd = fun(_) -> wings_menu:popup_menu(TC, Pos, ?MODULE, Menus) end,
            wings_wm:psend(?MODULE, {apply, false, Cmd});
        false ->  %% Material or light or images r-clicked
            ok
    end.

do_menu(#{type:=mat, name:=Name}) ->
    [{?__(1,"Edit Material..."),menu_cmd(edit_material, Name),
      ?__(2,"Edit material properties")},
     {?__(3,"Assign to Selection"),menu_cmd(assign_material, Name),
      ?__(4,"Assign the material to the selected faces or bodies")},
     separator,
     {?__(5,"Select"),select_menu(Name),
      {?__(6,"Select all elements that have this material"),
       ?__(27,"Add all elements that have this material to selection"),
       ?__(28,"Remove all elements that have this material from selection")},[]},
     separator,
     {?__(7,"Duplicate"),menu_cmd(duplicate_material, Name),
      ?__(8,"Duplicate this material")},
     {?__(9,"Delete"),menu_cmd(delete_material, Name),
      ?__(10,"Delete this material")},
     {?__(11,"Rename"),menu_cmd(rename_material, Name),
      ?__(12,"Rename this material")}];
do_menu(#{type:=object, id:=Id}) ->
    [{?__(13,"Duplicate"),menu_cmd(duplicate_object, Id),
      ?__(14,"Duplicate this object")},
     {?__(15,"Delete"),menu_cmd(delete_object, Id),
      ?__(16,"Delete this object")},
     {?__(17,"Rename"),menu_cmd(rename_object, Id),
      ?__(18,"Rename this object")}];
do_menu(#{type:=light, id:=Id}) ->
    [{?__(19,"Edit Light..."),menu_cmd(edit_light, Id),
      ?__(20,"Edit light properties")},
     separator,
     {?__(21,"Duplicate"),menu_cmd(duplicate_object, Id),
      ?__(22,"Duplicate this light")},
     {?__(23,"Delete"),menu_cmd(delete_object, Id),
      ?__(24,"Delete this light")},
     {?__(25,"Rename"),menu_cmd(rename_object, Id),
      ?__(26,"Rename this light")}];
do_menu(#{type:=image, id:=Id, image:=Im}) ->
    image_menu(Id, Im).

image_menu(Id, Im) ->
    [{?__(1,"Show"),menu_cmd(show_image, Id),
      ?__(2,"Show the image in a window")}|image_menu_1(Id, Im)].

%% Currently disabled.
image_menu_1(Id, #e3d_image{filename=none}) ->
    [{?__(1,"Make External..."),menu_cmd(make_external, Id)}|common_image_menu(Id)];
image_menu_1(Id, _) ->
    [{?__(2,"Refresh"),menu_cmd(refresh_image, Id),?__(11,"Update image to the contents of the saved file")},
     {?__(3,"Make Internal"),menu_cmd(make_internal, Id)}|common_image_menu(Id)].

common_image_menu(Id) ->
    [separator,
     {?__(1,"Export..."),menu_cmd(export_image, Id),
      ?__(2,"Export the image")},
     separator,
     {?__(3,"Duplicate"),menu_cmd(duplicate_image, Id),
      ?__(4,"Duplicate selected image")},
     {?__(5,"Delete"),menu_cmd(delete_image, Id),
      ?__(6,"Delete selected image")},
     {?__(7,"Rename"),menu_cmd(rename_image, Id),
      ?__(8,"Rename selected image")}
    ].

handle_drop(#{type:=image, id:=Id}, #{type:=mat, name:=Name}) ->
    [{?__(1,"Texture Type"),ignore},
     separator,
     {?__(2,"Diffuse"),tx_cmd(diffuse, Id, Name)},
     {?__(3,"Gloss"),tx_cmd(gloss, Id, Name)},
     {?__(4,"Bump (HeightMap)"),tx_cmd(bump, Id, Name)},
     {?__(5,"Bump (NormalMap)"),tx_cmd(normal, Id, Name)}].

tx_cmd(Type, Id, Mat) ->
    {'VALUE',{assign_texture,Type,Id,Mat}}.

select_menu(Name) ->
    fun(1, _Ns) ->
	    button_menu_cmd(select_material, [Name,select]);
       (2, _Ns) ->
	    button_menu_cmd(select_material, [Name,sel_add]);
       (3, _Ns) ->
	    button_menu_cmd(select_material, [Name,sel_rem]);
       (_, _) -> ignore
    end.

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

button_menu_cmd(Cmd, Id) ->
    {?MODULE,{Cmd,Id}}.
