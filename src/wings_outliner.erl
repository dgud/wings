%%
%%  wings_outliner.erl --
%%
%%     Maintains the outliner window.
%%
%%  Copyright (c) 2003-2008 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_outliner).
-export([window/1,window/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [foldl/3,reverse/1,keydelete/3]).

%%%
%%% Outliner window.
%%%
-record(ost,
	{st,					%Current St.
	 n,					%Number of objects.
	 first,					%First object to show.
	 sel,					%Current selection.
	 os,					%All objects.
	 active,				%Number of active object.
	 lh,					%Line height.
	 drag_ok=false,				%Drag is possible.
	 save_drag=none                         %Dragged object
	}).

window(St) ->
    case wings_wm:is_window(outliner) of
	true ->
	    wings_wm:raise(outliner),
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
    Ost = #ost{first=0,lh=18,active=-1},
    Current = {current_state,St},
    Op = {seq,push,event(Current, Ost)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(outliner, title(), Pos, Size,
		      [{sizeable,?PANE_COLOR},closable,vscroller,{anchor,ne},
		       {properties,Props}|Ps], Op),
    F = fun({image,_,_}) -> yes;
	   (_) -> no
	end,
    wings_wm:set_prop(outliner, drag_filter, F).

get_event(Ost) ->
    {replace,fun(Ev) -> event(Ev, Ost) end}.

event(redraw, Ost) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objects(Ost),
    keep;
event(resized, Ost) ->
    update_scroller(Ost),
    keep;
event(close, _) ->
    delete;
event(got_focus, _) ->
    Msg = wings_msg:button_format(?__(1,"Select"), [],
				  ?__(2,"Show outliner menu (if selection) or creation menu (if no selection)")),
    wings_wm:message(Msg),
    keep;
event({current_state,St}, Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event({note,image_change}, #ost{st=St}=Ost0) ->
    Ost = update_state(St, Ost0),
    update_scroller(Ost),
    get_event(Ost);
event(#mousemotion{state=Bst,y=Y}, #ost{active=Act}=Ost)
  when Act >= 0, Bst == 0 ->
    wings_wm:allow_drag(Act =:= active_object(Y, Ost)),
    keep;
event(#mousemotion{state=Bst,y=Y}=Ev, #ost{active=Act,os=Objs,drag_ok=true}=Ost)
  when Act >= 0, Bst band ?SDL_BUTTON_LMASK =/= 0 ->
    case active_object(Y, Ost) of
	Act -> keep;
	_ ->
	    drag_and_drop(Ev, lists:nth(Act+1, Objs)),
	    keep
    end;
event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, #ost{active=Act}=Ost)
  when Act >= 0 ->
    case active_object(Y, Ost) of
	Act ->
	    wings_wm:grab_focus(),
	    get_event(Ost#ost{drag_ok=true});
	_ ->
	    get_event(Ost#ost{drag_ok=false})
    end;
event(#mousebutton{button=1,y=Y,state=?SDL_RELEASED}, #ost{active=Act0}=Ost) ->
    wings_wm:release_focus(),
    case active_object(Y, Ost) of
	Act0 -> keep;
	Act ->
	    wings_wm:allow_drag(true),
	    wings_wm:dirty(),
	    get_event(Ost#ost{active=Act})
    end;
event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-1*lines(Ost) div 4, Ost);
event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(lines(Ost) div 4, Ost);
event(#mousebutton{}=Ev, #ost{st=St,active=Act}=Ost) ->
    case wings_menu:is_popup_event(Ev) of
	no -> keep;
	{yes,X,Y,_} ->
	    if
		Act =:= -1 -> wings_shapes:menu(X, Y, St);
		true -> do_menu(Act, X, Y, Ost)
	    end
    end;
event(scroll_page_up, Ost) ->
    zoom_step(-lines(Ost), Ost);
event(scroll_page_down, Ost) ->
    zoom_step(lines(Ost), Ost);
event({set_knob_pos,Pos}, #ost{first=First0,n=N}=Ost0) ->
    case round(N*Pos) of
	First0 -> keep;
	First when First < N ->
	    wings_wm:dirty(),
	    Ost = Ost0#ost{first=First,active=-1},
	    update_scroller(Ost),
	    get_event(Ost);
	_ -> keep
    end;
event({action,{outliner,Cmd}}, Ost) ->
    command(Cmd, Ost);
event({action,{shape,_}}=Act, _) ->
    wings_wm:send(geom, Act);
event(lost_focus, _) ->
    wings_wm:allow_drag(false),
    keep;
event({drop,{_,Y}=Pos,DropData}, #ost{os=Objs}=Ost) ->
    case active_object(Y, Ost) of
	-1 -> keep;
	Act ->
	    Obj = lists:nth(Act+1, Objs),
	    handle_drop(DropData, Obj, Pos, Ost)
    end;
event(language_changed, _) ->
    wings_wm:toplevel_title(title()),
    keep;
event(Ev, Ost) ->
    case wings_hotkey:event(Ev) of
	{select,deselect} ->
	    wings_wm:dirty(),
	    get_event(Ost#ost{active=-1});
	_ -> keep
    end.

handle_drop({image,Id,_}, {material,Name,_,_}, {X0,Y0}, _Ost) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Menu = [{?__(1,"Texture Type"),ignore},
	    separator,
	    {?__(2,"Diffuse"),tx_cmd(diffuse, Id, Name)},
	    {?__(3,"Gloss"),tx_cmd(gloss, Id, Name)},
	    {?__(4,"Bump (HeightMap)"),tx_cmd(bump, Id, Name)},
	    {?__(5,"Bump (NormalMap)"),tx_cmd(normal, Id, Name)}],
    wings_menu:popup_menu(X, Y, outliner, Menu);
handle_drop(_Drop, _On, _, _) -> 
    keep.

tx_cmd(Type, Id, Mat) ->
    {'VALUE',{assign_texture,Type,Id,Mat}}.

do_menu(-1, _, _, _) -> keep;
do_menu(Act, X, Y, #ost{os=Objs}) ->
    Menu = case lists:nth(Act+1, Objs) of
	       Mat = {material,Name,_,_} ->
		   [{?__(1,"Edit Material..."),menu_cmd(edit_material, Name),
		     ?__(2,"Edit material properties")},
		    {?__(3,"Assign to Selection"),menu_cmd(assign_material, Name),
		     ?__(4,"Assign the material to the selected faces or bodies")},
		    separator,
		    {?__(5,"Select"),menu_cmd(select_material, Name),
		     ?__(6,"Select all faces that have this material")},
		    separator,
		    {?__(7,"Duplicate"),menu_cmd(duplicate_material, Name),
		     ?__(8,"Duplicate this material")},
		    {?__(9,"Delete"),menu_cmd(delete_material, Name),
		     ?__(10,"Delete this material")},
		    {?__(11,"Rename"),menu_cmd(rename_material, Name),
		     ?__(12,"Rename this material")},
		    separator,
		    {?__(121,"Drop picked object"),
		     menu_cmd(drop_object,{{X,Y},Mat}),
		     ?__(122,"Drop a previously picked object on this material")}];
	       {object,Id,_} ->
		   [{?__(13,"Duplicate"),menu_cmd(duplicate_object, Id),
		     ?__(14,"Duplicate this object")},
		    {?__(15,"Delete"),menu_cmd(delete_object, Id),
		     ?__(16,"Delete this object")},
		    {?__(17,"Rename"),menu_cmd(rename_object, Id),
		     ?__(18,"Rename this object")}];
	       {light,Id,_} ->
		   [{?__(19,"Edit Light..."),menu_cmd(edit_light, Id),
		     ?__(20,"Edit light properties")},
		    separator,
		    {?__(21,"Duplicate"),menu_cmd(duplicate_object, Id),
		     ?__(22,"Duplicate this light")},
		    {?__(23,"Delete"),menu_cmd(delete_object, Id),
		     ?__(24,"Delete this light")},
		    {?__(25,"Rename"),menu_cmd(rename_object, Id),
		     ?__(26,"Rename this light")}];
	       {image,Id,Im} ->
		   image_menu(Id, Im);
	       ignore -> none;
	       {image_preview,_} -> none
	   end,
    if
	Menu == none -> keep;
	true -> wings_menu:popup_menu(X, Y, outliner, Menu)
    end.

image_menu(Id, Im) ->
    [{?__(1,"Show"),menu_cmd(show_image, Id),
      ?__(2,"Show the image in a window")}|image_menu_1(Id, Im)].

%% Currently disabled.
image_menu_1(Id, #e3d_image{filename=none}) ->
    [{?__(1,"Make External..."),menu_cmd(make_external, Id)}|common_image_menu(Id)];
image_menu_1(Id, _) ->
    [{?__(2,"Refresh"),menu_cmd(refresh_image, Id)},
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
      ?__(8,"Rename selected image")},
     separator, 
     {?__(9,"Pick up Image"), menu_cmd(pick_image, Id), 
      ?__(10,"Pick up the Image to it put on a material as a texture")}
    ].

menu_cmd(Cmd, Id) ->
    {'VALUE',{Cmd,Id}}.

command({edit_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{edit,Name}}});
command({assign_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{assign,Name}}});
command({select_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{select,[Name]}}});
command({duplicate_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{duplicate,[Name]}}});
command({delete_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{delete,[Name]}}});
command({rename_material,Name}, _Ost) ->
    wings_wm:send(geom, {action,{material,{rename,[Name]}}});
command({drop_object, _}, #ost{save_drag=none}) ->
    keep;
command({drop_object, {Pos, OnObject}}, Ost = #ost{save_drag=Saved}) ->
    handle_drop(Saved, OnObject, Pos, Ost);
command({assign_texture,Type,Id,Name0}, #ost{st=#st{mat=Mtab}}) ->
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
command({pick_image, Id}, Ost1) ->
    Ost=Ost1#ost{save_drag={image,Id,dummy}},
    get_event(Ost);
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

delete_image(Id, #ost{st=St}) ->
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
    wings_ask:ask(?__(1,"Rename Image"),
		  [{Name0,Name0}],
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
		       ok -> keep;
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

drag_and_drop(Ev, What) ->
    DropData = drop_data(What),
    {W,_} = wings_wm:win_size(),
    wings_wm:drag(Ev, {W-4,?LINE_HEIGHT}, DropData).
    
drop_data({object,Id,_}) -> {object,Id};
drop_data({light,Id,_}) -> {light,Id};
drop_data({image,Id,Info}) -> {image,Id,Info};
drop_data({material,Name,_,_}) -> {material,Name}.

%%%
%%% Updating the state.
%%%

update_state(St, #ost{first=OldFirst}=Ost0) ->
    #ost{first=First0} = Ost = update_state_1(St, Ost0),
    case clamp(First0, Ost) of
	OldFirst -> Ost;
	First ->
	    wings_wm:dirty(),
	    Ost#ost{first=First}
    end.

% update_state_1(#st{shapes=Shs,mat=Mat}=St, #ost{st=#st{shapes=Shs,mat=Mat}}=Ost) ->
%     Ost#ost{st=St};
update_state_1(St, Ost) ->
    update_state_2(St, Ost).

update_state_2(#st{mat=Mat,shapes=Shs0}=St, #ost{os=Objs0,active=Act0}=Ost) ->
    Objects =
      case wings_pref:get_value(objects_in_outliner) of
        true -> [{object,Id,Name} || #we{id=Id,name=Name}=We <- gb_trees:values(Shs0),
				     not ?IS_ANY_LIGHT(We)];
        _ -> []
      end,
    Lights = [{light,Id,Name} || #we{id=Id,name=Name}=We <- gb_trees:values(Shs0),
			         ?IS_ANY_LIGHT(We)],
    Materials = [make_mat(M) || M <- gb_trees:to_list(Mat)],
    Objs = Objects ++ Lights ++ Materials ++ update_images(),
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

update_images() ->
    Ims = foldl(fun({Id,Im}, A) ->
			[ignore,{image_preview,Id},{image,Id,Im}|A]
		end, [], wings_image:images()),
    reverse(Ims).

make_mat({Name,Mp}) ->
    OpenGL = proplists:get_value(opengl, Mp),
    {R,G,B,_} = Color = proplists:get_value(diffuse, OpenGL),
    TextColor = case lists:max([R,G,B]) of
		    V when V < 0.5 -> {1,1,1};
		    _ -> {0,0,0}
		end,
    {material,atom_to_list(Name),Color,TextColor}.

update_scroller(#ost{n=0}) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, 0.0, 1.0);
update_scroller(#ost{first=First,n=N}=Ost) ->
    Name = wings_wm:this(),
    Lines = lines(Ost),
    wings_wm:set_knob(Name, First/N, Lines/N).

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
    
active_object(Y0, #ost{lh=Lh,first=First,n=N,os=Os}) ->
    case Y0 - top_of_first_object() of
	Y when Y < 0 -> -1;
	Y1 ->
	    case Y1 div Lh of
		Y when First+Y < N ->
		    Act = First+Y,
		    %% Ugly hack until we rewrite different element
		    %% type to have different height.
		    case lists:nth(Act+1, Os) of
			{image_preview,_} -> Act-1;
			ignore -> Act-2;
			_ -> Act
		    end;
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
    draw_icons(N, Objs, Ost, Lh-2),
    draw_objects_1(N, Objs, Ost, R, Active-First, Lh-2).

draw_objects_1(0, _, _, _, _, _) -> ok;
draw_objects_1(N, [O|Objs], #ost{lh=Lh}=Ost, R, Active, Y) ->
    case O of
	{material,Name,Color,TextColor} ->
	    wings_io:border(2, Y-10, 12, 12, Color),
	    gl:color3fv(TextColor),
	    gl:rasterPos2f(5.5, Y),
	    wings_io:draw_char(m_bitmap()),
	    gl:color3b(0, 0, 0);
	{image,_,#e3d_image{name=Name}} -> ok;
	{_,_,Name} -> ok;
	{image_preview,_} -> Name = [];
	ignore -> Name = []
    end,
    if
	Active == 0 ->
	    gl:color3f(0, 0, 0.5),
	    gl:recti(name_pos()-2, Y-?CHAR_HEIGHT, R-2, Y+4),
	    gl:color3f(1, 1, 1);
	true -> ok
    end,
    wings_io:text_at(name_pos(), Y, Name),
    gl:color3b(0, 0, 0),
    draw_objects_1(N-1, Objs, Ost, R, Active-1, Y+Lh).

draw_icons(N, Objs, Ost, Y) ->
    wings_io:draw_icons(fun() -> draw_icons_1(N, Objs, Ost, Y-14) end),
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    draw_previews(N, Objs, Ost, Y-14),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:disable(?GL_TEXTURE_2D).
    
draw_icons_1(0, _, _, _) -> ok;
draw_icons_1(N, [ignore|Objs], #ost{lh=Lh}=Ost, Y) ->
    draw_icons_1(N-1, Objs, Ost, Y+Lh);
draw_icons_1(N, [O|Objs], #ost{lh=Lh}=Ost, Y) ->
    X = 2,
    Type = element(1, O),
    case Type of
	object ->
	    wings_io:draw_icon(X, Y, small_object);
	light ->
	    wings_io:draw_icon(X, Y, small_light);
	image ->
	    case O of
		{_,_,#e3d_image{filename=none}} ->
		    wings_io:draw_icon(X, Y, small_image);
		_ ->
		    wings_io:draw_icon(X, Y, small_image2)
	    end;
	image_preview -> ok;
	material -> ok
    end,
    draw_icons_1(N-1, Objs, Ost, Y+Lh).

draw_previews(0, _, _, _) -> ok;
draw_previews(N, [{image_preview,Im}|Objs], #ost{lh=Lh}=Ost, Y) ->
    W = H = 2*Lh,
    wings_image:draw_preview(name_pos(), Y, W, H, Im),
    draw_previews(N-1, Objs, Ost, Y+Lh);
draw_previews(N, [_|Objs], #ost{lh=Lh}=Ost, Y) ->
    draw_previews(N-1, Objs, Ost, Y+Lh).

m_bitmap() ->
    {7,8,0,0,8,0,
     <<2#10000010,
       2#10000010,
       2#10000010,
       2#10010010,
       2#10111010,
       2#10101010,
       2#11000110,
       2#10000010>>}.

top_of_first_object() ->
    0.

right_pos() ->
    {W,_} = wings_wm:win_size(),
    W-13.

name_pos() ->
    22.

lines(#ost{lh=Lh}) ->
    {_,_,_,H} = wings_wm:viewport(),
    H div Lh.

title() ->
    ?__(1,"Outliner").
