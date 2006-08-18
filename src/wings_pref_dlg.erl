%%                     -*- mode:erlang; coding:iso-latin-1-unix -*-
%%
%%  wings_pref_dlg.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pref_dlg.erl,v 1.15 2006/07/28 18:54:27 antoneos Exp $
%%

-module(wings_pref_dlg).
-export([menu/1,command/2]).

-include("wings.hrl").
-import(wings_pref, [get_value/1,set_value/2]).

-import(lists, [foreach/2,keysearch/3,map/2,reverse/1,sort/1,last/1]).

menu(_St) ->
    {?__(1,"Preferences..."),
      fun(_, _) ->
	      {edit,{preferences,prefs}}
      end,?__(2,"Edit the preferences for Wings"),[]}.

command(prefs, St) ->
    PrefQs0 = [{?__(1,"General"),gen_prefs()},
	       {?__(2,"Camera"),{'VALUE',wings_camera:prefs()}},
	       {?__(3,"Advanced"),advanced_prefs()},
	       {?__(4,"User Interface"),ui_prefs()},
	       {?__(5,"Misc"),misc_prefs()}],
    PrefQs = [{Lbl,make_query(Ps)} || {Lbl,Ps} <- PrefQs0],
    Qs = [{oframe,PrefQs,1,[{style,buttons}]}],
	wings_ask:dialog(?__(6,"Preferences"), Qs,
		     fun([_|Res]) ->
			     Dl = wings_wm:get_prop(geom, display_lists),
			     wings_wm:set_prop(wings_wm:this(), display_lists, Dl),
			     set_values(Res, St)
		     end).

set_values([{Key,Val}|Kvs], St) ->
    smart_set_value(Key, Val, St),
    set_values(Kvs, St);
set_values([], _) -> ignore.


gen_prefs() ->
    {vframe,
     [{hframe,
       [{vframe,
	 [{label_column,
	   [{?__(1,"Unselected Size"),vertex_size,
	     [{info,?__(2,"Size in pixels of unselected vertices (0.0 means not shown)")}]},
	    {?__(3,"Selected Size"),selected_vertex_size,
	     [{info,?__(4,"Size in pixels of selected vertices")}]}]}],
	 [{title,?__(5,"Vertex Display")}]},
	{vframe,
	 [{label_column,
	   [{?__(6,"Unselected Width"),edge_width,
	     [{info,?__(7,"Width in pixels of unselected edges")}]},
	    {?__(8,"Selected Width"),selected_edge_width,
	     [{info,?__(9,"Width in pixels of selected edges")}]},
	    {color,?__(10,"Soft Edges"),edge_color,[{info,?__(11,"Color of soft edges")}]},
	    {color,?__(12,"Hard Edges"),hard_edge_color,[{info,?__(13,"Color of hard edges")}]}]}],
	 [{title,?__(14,"Edge Display")}]}]},
      {hframe,
       [{vframe,
	 [{hframe,
	   [{?__(15,"Vertices"),vertex_hilite},
	    {?__(16,"Edges"),edge_hilite},
	    {?__(17,"Faces"),face_hilite},
	    {?__(18,"Objects"),body_hilite}]},
	  {hframe,
	   [{label,?__(19,"Unselected")},{color,unselected_hlite},
	    {label,?__(20,"Selected")},{color,selected_hlite}]},
	  {?__(21,"Smart Highlighting"),smart_highlighting}],
	 [{title,?__(22,"Highlighting")}]},
	{vframe,
	 [{vradio,[{?__(23,"Solid Face Selections"),solid},
		   {?__(24,"Stippled Face Selections"),stippled}],
	   selection_style},
	  {hframe,[{label,?__(25,"Selection Color")},{color,selected_color}]} ],
	 [{title,?__(26,"Selection")}]}]},
      {hframe,
       [{vframe,
        [{label_column,
	  [{color,?__(27,"Text"),info_color,[{info,?__(28,"Color of information text")}]},
	   {color,?__(29,"Background"),info_background_color,
	    [{info,?__(30,"Color of background for information text (including transparency)")}]}]}],
        [{title,?__(31,"Information text")}]},
       {vframe,
        [{label_column,
 	 [{?__(44,"Length"),normal_vector_size,
	    [{info,?__(45,"Length of normals")},{range,{0.1,10.0}}]},
	   {color,?__(46,"Color"),normal_vector_color,
	    [{info,?__(47,"Color of normals")}]}]}],
	 [{title,?__(48,"Normals Display")}]}]},
      {hframe,
       [{label,?__(32,"Color")},{color,grid_color},
	{?__(33,"Force Axis-Aligned Grid"),force_show_along_grid,
	 [{info,?__(34,"Always show the grid when the view is aligned along one of the major axes")}]},
	{?__(aa_ortho1,"Force Axis-Aligned Ortho"),
	 force_ortho_along_axis,
	[{info, ?__(aa_ortho2,"Always go into orthogonal mode when the view is aligned along one of the major axes")}]}],
      [{title,?__(35,"Grid")}]},
      {hframe,
       [{vframe,
	 [{?__(36,"Show Axis Letters"),show_axis_letters},
	  {?__(49,"Constrain Axes to Grid"),constrain_axes},
	  {hframe,
	   [{label_column,
	     [{color,?__(37,"+X Color"),x_color},
	      {color,?__(38,"+Y Color"),y_color},
	      {color,?__(39,"+Z Color"),z_color}]},
	    {label_column,
	     [{color,?__(40,"-X Color"),neg_x_color},
	      {color,?__(41,"-Y Color"),neg_y_color},
	      {color,?__(42,"-Z Color"),neg_z_color}]}]}],
	 [{title,?__(43,"Axes")}]}
       ]}]}.

advanced_prefs() ->
    DisableHook = fun (is_disabled, {_Var,_I,Store}) ->
			  not gb_trees:get(advanced_menus, Store);
		      (_, _) ->	void
		  end,
    SuperDisable = fun (is_disabled, {_Var,_I,Store}) ->
			   not gb_trees:get(advanced_menus, Store) orelse
			       not gb_trees:get(use_temp_sel, Store);
		       (_, _) ->	void
		  end,
    Flags = [{hook,DisableHook}],

    {vframe,
     [{?__(1,"Default Commands"),default_commands,
       [{info,?__(2,"Allow defining commands that can be invoked by Ctrl+L or Ctrl+M")}]},
      {?__(3,"Use Highlight as Temporary Selection"),use_temp_sel,
       [{info,?__(4,"If there is no selection, ")++
	 ?__(5,"allow commands to act on the highlighted element")}]},
      {?__(6,"Hide Selection While Dragging"),hide_sel_while_dragging,
       [{info,?__(7,"Don't show the selection in any interactive command")}]},
      {?__(8,"Hide Selection While Moving Camera"),hide_sel_in_camera_moves,
       [{info,?__(9,"Don't show the selection when the camera is being moved")}]},
      panel,
      {?__(10,"Advanced Menus"),advanced_menus,
       [{info,?__(11,"More commands and more options, such as magnets")}]},
      {?__(21,"Use Mirror for Selections"),use_mirror_for_sels,
       [{info,?__(22,"Default to using the virtual mirror for secondary selections")}]},
      {?__(12,"Power-user temporary selections"),use_super_temp_sel,
       [{info,?__(13,"In the secondary selection mode, RMB-clicking always add to the selection")},{hook,SuperDisable}]},
      {vframe,
       [{label_column,
	 [{?__(14,"Length"),active_vector_size,
	   [{info,?__(15,"Length of vector in secondary selections")},{range,{0.1,10.0}}|Flags]},
	   {?__(16,"Width"),active_vector_width,
	   [{info,?__(17,"Width of vector (in pixels)")},{range,{1.0,10.0}}|Flags]},
	  {color,?__(18,"Color"),active_vector_color,
	   [{info,?__(19,"Color of vector")}|Flags]}]}],
	[{title,?__(20,"Vector Display")}]}
     ]}.

ui_prefs() ->
    Fonts = wings_text:fonts(),
    Langs0 = wings_lang:available_languages(),
    InterfaceIcons = [{"Classic", classic}, {"Blue Cube", bluecube}],
    Langs = [{language_name(L),L} || L <- Langs0],
    {vframe,
     [{hframe,
       [{vframe,
	 [{vframe,
	   [{menu,Fonts,new_system_font}],
	   [{title,?__(1,"System Font")}]},
	  {hframe,
	   [{vframe,
	     [{label,?__(2,"Desktop/Geometry Background")},
	      {label,?__(3,"Menu Text")},
	      {label,?__(4,"Menu Highlight")},
	      {label,?__(5,"Menu Highlighted Text")},
	      {label,?__(6,"Menu Background")},
	      {label,?__(7,"Dialog Text")},
	      {label,?__(8,"Dialog (Disabled) Text")},
	      {label,?__(9,"Dialog Background")},
	      {label,?__(10,"Title Text")},
	      {label,?__(11,"Title (Passive) Background")},
	      {label,?__(12,"Title (Active) Background")}]},
	    {vframe,
	     [{color,background_color},
	      {color,menu_text},
	      {color,menu_hilite},
	      {color,menu_hilited_text},
	      {color,menu_color},
	      {color,dialog_text},
	      {color,dialog_disabled},
	      {color,dialog_color},
	      {color,title_text_color},
	      {color,title_passive_color},
	      {color,title_active_color}]}],
	   [{title,?__(13,"Colors")}]}
	 ]},
	{vframe,
	 [{vframe,
	   [{menu,Langs,language}],
	   [{title,?__(23,"Language")}]},
	  {vframe,
	   [{menu,Fonts,new_console_font}],
	   [{title,?__(15,"Console Font")}]},
	  {hframe,[{vframe,[{label,?__(16,"Width")},
			    {label,?__(17,"Height")},
			    {label,?__(18,"Save Lines")},
			    {label,?__(19,"Background")},
			    {label,?__(20,"Text")},
			    {label,?__(21,"Cursor")}]},
		   {vframe,[{text,console_width,[{range,{12,120}}]},
			    {text,console_height,[{range,{3,72}}]},
			    {text,console_save_lines,[{range,{0,10000}}]},
			    {color,console_color},
			    {color,console_text_color},
			    {color,console_cursor_color}]}],
	   [{title,?__(22,"Console")}]},
	  {vframe,
	   [{menu,InterfaceIcons,interface_icons}],
	   [{title,?__(50,"Interface Icons")}]}]}
       ]},
      {?__(14,"No Progress Bar"),no_progress_bar},
      {?__(24,"Objects in Outliner"),objects_in_outliner},
      {hframe,[{?__(25,"View image after rendering"),render_load_image},
	       {?__(26,"Load image after rendering"),render_iload_image}]},
      {oframe,
       [{atom_to_list(Format),viewer_prefs(Format)}
	|| {Format,_,_} <- wings_job:render_formats()],
       viewer_frame,
       [{style,buttons}]}
     ]}.

viewer_prefs(Format) ->
    {hframe,
     [{vframe,
       [{label,?__(1,"Viewer")},
	{label,?__(2,"Options")}]},
      {vframe,
       [{button,{text,{viewer,Format},[wings_job:browse_props()]}},
	{hframe,
	 [{text,{viewer_preopts,Format},[{width,10}]},
	  {label,?__(3,"..filename..")},
	  {text,{viewer_postopts,Format},[{width,10}]}]}]}]}.



language_name("cs") ->
    %% "Czech" in Czech as Unicode:
    [16#10C|"eský"];
language_name("en") -> "English";
language_name("de") -> "Deutsch";
language_name("es") -> "Español";
language_name("fi") -> "Suomi";
language_name("fr") -> "Français";
language_name("it") -> "Italiano";
language_name("pl") -> "Polski";
language_name("pt") -> "Português";
language_name("ru") ->
    %% "Russian" in Russian as Unicode:
    [1056,1091,1089,1089,1082,1080,1081];
language_name("sv") -> "Svenska";
language_name(Other) -> Other.

misc_prefs() ->
    Flags = case wings_gl:is_ext({1,2}, 'GL_ARB_imaging') of
		true -> [];
		false ->
		    [{hook,fun(is_disabled, _) -> true;
			      (_, _) -> void
			   end},
		     {info,?__(1,"Opacity settings not supported using this version of OpenGL")}]
	    end,
    AutoFun = fun(is_disabled, {_Var,_I,Store}) ->
		      not gb_trees:get(autosave, Store);
		 (_, _) -> void
	      end,
    {vframe,
     [{hframe,[{?__(2,"Save automatically every"),autosave},
	       {text,autosave_time,[{hook,AutoFun},{range,{1,1440}}]},
	       {label,?__(3,"minutes")}]},
      {hframe,[{label,?__(4,"Undo levels")},
	       {text,num_undo_levels,[{range,{10,128}}]}]},
      {vframe,
	 [{label_column,
	   [{?__(5,"Angle"),auto_rotate_angle},
	    {?__(6,"Delay (ms)"),auto_rotate_delay}]}],
	 [{title,?__(7,"Auto Rotate")}]},
      {vframe,
       [{vframe,
	 [{menu,[{?__(8,"Cage"),cage},
		 {?__(9,"Some Edges"),some},
		 {?__(10,"All Edges"),all}],
	   proxy_shaded_edge_style}],
	 [{title,?__(11,"Shaded Mode Edge Style")}]},
	{hframe,
	 [{vframe,
	   [{label,?__(12,"Stationary Opacity")},
	    {label,?__(13,"Moving Opacity")}]},
	  {vframe,
	   [{slider,{text,proxy_static_opacity,[{range,{0.0,1.0}}|Flags]}},
	    {slider,{text,proxy_moving_opacity,[{range,{0.0,1.0}}|Flags]}}]}]}],
       [{title,?__(14,"Proxy Mode")}]},
      {vframe,
       [{hframe,
	 %% Note: text_display_lists is specially handled
	 %% in make_query/1 and smart_set_value/3 to have its value inverted
	 %% to keep preference files backward compatible.
	 workaround([{text_display_lists,
		      ?__(15,"Text in menus and dialogs disappear"),
		      ?__(16,"Problem occurs on some Matrox cards")},
		     {dummy_axis_letter,
		      ?__(17,"Wings crashes if axes are turned off"),
		      ?__(18,"Problem occurs on some Matrox cards")},
		     {jumpy_camera,
		     ?__(19,"Camera moves and interactive commands are jumpy"),
		      ?__(20,"Problem occurs on Mac OS X 10.3 (Panther)")}
		    ])},
	separator,
	{hframe,[{label,?__(23,"Edge offsets:")},
		 {text,polygon_offset_f,[{range,{1.0,100.0}}]},
		 {text,polygon_offset_r,[{range,{1.0,100.0}}]}],
	 [{title,?__(22,"Edge display problems?")}]}],
       [{title,?__(21,"Workarounds")}]}
     ]}.

workaround(L) ->
    workaround_1(L, [], []).

workaround_1([{Key,Str,BadGuy}|T], A0, B0) ->
    Bl = " ",
    Info = [{info,BadGuy}],
    A = [{label,Str,Info}|A0],
    B = [{Bl,Key,Info}|B0],
    workaround_1(T, A, B);
workaround_1([], A, B) ->
    [{vframe,[{label,?__(1,"Problem")},separator|reverse(A)]},
     {vframe,[{label,?__(2,"Use Workaround?")},separator|reverse(B)]}].

smart_set_value(text_display_lists=Key, Val, St) ->
    %% Reverse sense to keep backwards comptibility of preferences.
    smart_set_value_1(Key, not Val, St);
smart_set_value(Key, Val, St) ->
    smart_set_value_1(Key, Val, St).

smart_set_value_1(Key, Val, St) ->
    case ets:lookup(wings_state, Key) of
	[] -> set_value(Key, Val);
	[{Key,Val}] -> ok;
	[{Key,OldVal}] ->
	    set_value(Key, Val),
	    case Key of
		vertex_size ->
		    clear_vertex_dlist();
		background_color ->
		    {R,G,B} = Val,
		    gl:clearColor(R, G, B, 1.0);
		autosave_time ->
		    wings_file:init_autosave();
		proxy_shaded_edge_style ->
		    clear_proxy_edges(St);
		new_system_font ->
		    delayed_set_value(Key, OldVal, Val),
		    wings_u:message(?__(1,"The change to the system font will take effect the next time Wings 3D is started."));
		interface_icons ->
		    delayed_set_value(Key, OldVal, Val),
		    wings_u:message(?__(2,"The change to the interface icons will take\neffect the next time Wings 3D is started."));
		new_console_font ->
		    wings_console:window();
		camera_mode ->
		    wings_wm:translation_change();
		num_buttons ->
		    wings_wm:translation_change();
		no_progress_bar ->
		    wings_pb:init();
 		language ->
		    wings_lang:load_language(Val);
		polygon_offset_f -> 
		    erase(polygon_offset);
		polygon_offset_r -> 
		    erase(polygon_offset);
		normal_vector_size ->
		    update_normal_dlist(St);
		normal_vector_color ->
		    update_normal_dlist(St);
        	_Other -> ok
	    end
    end.

delayed_set_value(Key, OldVal, NewVal) ->
    set_value(Key, OldVal),
    ets:insert(wings_delayed_update, {Key,NewVal}).

update_normal_dlist(St) ->
    wings_dl:map(fun(D, _) -> D#dlo{normals=none} end, []),
    wings_draw:refresh_dlists(St).

clear_vertex_dlist() ->
    wings_dl:map(fun clear_vertex_dlist/2, []).

clear_vertex_dlist(D, _) -> D#dlo{vs=none}.

clear_proxy_edges(St) ->
    wings_dl:map(fun(D, _) -> clear_proxy_edges(D, St) end, []).

clear_proxy_edges(D, St) ->
    wings_proxy:update(D#dlo{proxy_edges=none}, St).

make_query({'VALUE',Val}) ->
    Val;
make_query([_|_]=List)  ->
    [make_query(El) || El <- List];
make_query({color,Key}) ->
    Def = get_value(Key),
    {color,Def,[{key,Key}]};
make_query({color,[_|_]=Str,Key}) ->
    Def = get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query({color,[_|_]=Str,Key,Flags}) ->
    Def = get_value(Key),
    {Str,{color,Def,[{key,Key}|Flags]}};
make_query({[_|_]=Str,text_display_lists=Key}) ->
    %% Reverse sense to keep backwards comptibility of preferences.
    {Str,not get_value(Key),[{key,Key}]};
make_query({[_|_]=Str,text_display_lists=Key,Flags}) ->
    %% Reverse sense to keep backwards comptibility of preferences.
    {Str,not get_value(Key),[{key,Key}|Flags]};
make_query({[_|_]=Str,Key}) ->
    case get_value(Key) of
	Def when Def == true; Def == false ->
	    {Str,Def,[{key,Key}]};
	Def ->
	    {Str,{text,Def,[{key,Key}]}}
    end;
make_query({[_|_]=Str,Key,Flags}) ->
    case get_value(Key) of
	Def when Def == true; Def == false ->
	    {Str,Def,[{key,Key}|Flags]};
	Def ->
	    {Str,{text,Def,[{key,Key}|Flags]}}
    end;
make_query({alt,Key,Label,Val}) ->
    Def = get_value(Key),
    {key_alt,{Key,Def},Label,Val};
make_query({menu,List,Key}) ->
    Def = get_value(Key),
    {menu,List,Def,[{key,Key}]};
make_query({vradio,List,Key}) ->
    Def = get_value(Key),
    {vradio,List,Def,[{key,Key}]};
make_query({slider,Slider}) ->
    {slider,make_query(Slider)};
make_query({text,Key,Flags}) ->
    Def = get_value(Key),
    {text,Def,[{key,Key}|Flags]};
make_query({text,Key}) ->
    Def = get_value(Key),
    {text,Def,[{key,Key}]};
make_query({oframe,Frames,Key,Flags}) ->
    Def = get_value(Key),
    {oframe,
     [{Tag,make_query(Field)} || {Tag,Field} <- Frames],
     Def,
     [{key,Key}|Flags]};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

