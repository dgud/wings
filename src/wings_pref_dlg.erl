%%
%%  wings_pref_dlg.erl --
%%
%%     Preference management.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_pref_dlg).
-export([menu/0,command/2]).
-export([set_values/2]).

-include("wings.hrl").
-import(wings_pref, [get_value/1,set_value/2]).

-import(lists, [reverse/1,foreach/2]).
-define(NONZERO,1.0e-6).

menu() ->
    {?__(1,"Preferences..."),
     fun(_, _) ->
	     {edit,{preferences,prefs}}
     end,?__(2,"Edit the preferences for Wings"),[]}.

command(prefs, St) ->
    PrefQs0 = [{?__(1,"General"),gen_prefs()},
	       {?__(2,"Camera"),{'VALUE',wings_camera:prefs()}},
	       {?__(3,"Advanced"),advanced_prefs()},
	       {?__(31,"Constraints"),constraint_prefs()},
	       {?__(4,"User Interface"),ui_prefs()},
	       {?__(5,"Misc"),misc_prefs()}],
    PrefQs = [{Lbl,make_query(Ps)} || {Lbl,Ps} <- PrefQs0],
    Qs = [{oframe,PrefQs,1,[{style,buttons}]}],
    wings_dialog:dialog(?__(6,"Preferences"), Qs, fun(Res) -> set_values(Res, St) end).

set_values([{Key,Val}|Kvs], St) ->
    smart_set_value(Key, Val, St),
    set_values(Kvs, St);
set_values([], _) -> ignore.


gen_prefs() ->
    {vframe,
     [{hframe,
       [{label_column,
	 [{?__(1,"Unselected Size"),vertex_size,
	   [{info,?__(2,"Size in pixels of unselected vertices (0.0 means not shown)")}]},
	  {?__(3,"Selected Size"),selected_vertex_size,
	   [{info,?__(4,"Size in pixels of selected vertices")}]},
	  {?__(65,"Magnet Mask Vertex Size"),masked_vertex_size,
	   [{info,?__(66,"Size in pixels of magnet mask vertices")}]},
	  {color,?__(67,"Unselected Vertices"),vertex_color,
	   [{info,?__(68,"Color of unselected vertices")}]},
	  {color,?__(69,"Magnet Mask"),masked_vertex_color,
	   [{info,?__(70,"Color of magnet mask vertices")}]}],
	 [{title,?__(5,"Vertex Display")}]},
	{vframe,
	 [{label_column,
	   [{?__(6,"Unselected Width"),edge_width,
	     [{info,?__(7,"Width in pixels of unselected edges")}]},
	    {?__(8,"Selected Width"),selected_edge_width,
	     [{info,?__(9,"Width in pixels of selected edges")}]},
	    {?__(57,"Hard Edge Width"),hard_edge_width,
	     [{info,?__(58,"Width in pixels of hard edges")}]},
	    {color,?__(10,"Soft Edges"),edge_color,
	     [{info,?__(11,"Color of soft edges")}]},
	    {color,?__(12,"Hard Edges"),hard_edge_color,
	     [{info,?__(13,"Color of hard edges")}]}]},
	  {?__(50,"Draw Anti-Aliased Edges"),aa_edges}],
	 [{title,?__(14,"Edge Display")}]}]},

      {hframe,
       [{vframe,
	 [{hframe,
	   [{?__(18,"Objects"),body_hilite}]},
	  {hframe,
	   [{label,?__(19,"Unselected")},{color,unselected_hlite},
	    {label,?__(20,"Selected")},{color,selected_hlite}]},
	  {?__(71,"Conditional Deselection"), conditional_deselect,
	   [{info,?__(72,"Deselecting keeps you in the same Selection Mode, unless there is no selection")}]}],
	 [{title,?__(22,"Highlighting")}]},

	{hframe,
	 [{label,?__(25,"Selection Color")},{color,selected_color}],
	 [{title,?__(26,"Selection")}]}]},

      {hframe,
       [{vframe,
	 [{label_column,
	   [{color,?__(27,"Text"),info_color,
	     [{info,?__(28,"Color of information text")}]},
	    {color,?__(29,"Background"),info_background_color,
	     [{info,?__(30,"Color of background for information text (including transparency)")}]}]},
	  {hframe,
	   [{?__(61,"Verbose"),info_enhanced_text,
	     [{info,?__(62,"Show additional information about selected elements")}]}]}],
	 [{title,?__(31,"Information Text")}]},
	{label_column,
	 [{?__(44,"Length"),normal_vector_size,
	   [{info,?__(45,"Length of normals")},{range,{0.1,10.0}}]},
	  {?__(59,"Width"),normal_vector_width,
	   [{info,?__(60,"Width of normals")}]},
	  {color,?__(46,"Color"),normal_vector_color,
	   [{info,?__(47,"Color of normals")}]}],
	 [{title,?__(48,"Normals Display")}]},
	{vframe,
	 [{label_column,
	   [{color,?__(51,"Default"),material_default,
	     [{info,?__(52,"Sets the colour of the default material")}]}]}],
	 [{title,?__(55,"Materials")}]}]},
      {hframe,
       [{vframe,
	 [{?__(49,"Constrain Axes to Grid"),constrain_axes},
	  {?__(56,"Show Mini Axis"),mini_axis}]},
	{hframe,
	 [{label_column,
	   [{color,?__(37,"+X Color"),x_color},
	    {color,?__(38,"+Y Color"),y_color},
	    {color,?__(39,"+Z Color"),z_color}]},
	  {label_column,
	   [{color,?__(40,"-X Color"),neg_x_color},
	    {color,?__(41,"-Y Color"),neg_y_color},
	    {color,?__(42,"-Z Color"),neg_z_color}]}]}],
       [{title,?__(43,"Axes")}]},
      {vframe,
       [{hframe,
	 [{label_column,
	   [{color,?__(32,"Color"),grid_color}]},
	  {?__(33,"Force Axis-Aligned Grid"),force_show_along_grid,
	   [{info,?__(34,"Always show the grid when the view is aligned along one of the major axes")}]},
	  {?__(aa_ortho1,"Force Axis-Aligned Ortho"),force_ortho_along_axis,
	   [{info, ?__(aa_ortho2,"Always go into orthogonal mode when the view is aligned along one of the major axes")}]}]}],
       [{title,?__(35,"Grid")}]},
      {hframe,
       [{?__(75,"Enable Multi-Sampling"),multisample,
	 [{info,?__(74,"Anti-Alias the whole interface, "
	   "including polygons.")}]}],
       [{title,?__(73,"Anti-Aliasing")}]}]}.

advanced_prefs() ->
    Disable = fun (_, Enable, Store) ->
		      Keys = [drag_speed_abs, drag_speed_relative, drag_speed_rotate],
		      wings_dialog:enable(Keys, Enable, Store)
	      end,
    {hframe,
    [{vframe,
     [{?__(1,"Default Commands"),default_commands,
       [{info,?__(2,"Allow defining commands that can be invoked by Ctrl+L or Ctrl+M")}]},
      {?__(6,"Hide Selection While Dragging"),hide_sel_while_dragging,
       [{info,?__(7,"Don't show the selection in any interactive command")}]},
      {?__(8,"Hide Selection While Moving Camera"),hide_sel_in_camera_moves,
       [{info,?__(9,"Don't show the selection when the camera is being moved")}]},
      {?__(54,"Extend selection via hotkey on temporary highlight"),hilite_select,
	   [{info,?__(55,"Affects: Select All, Edge Loop, Edge Ring, Select Similar, Similar Normals, Similar Materials, and Similar Area.")}]},
     {vframe,
       [{label_column,
     [{?__(14,"Length"),active_vector_size,
       [{info,?__(15,"Length of vector in secondary selections")},{range,{0.1,10.0}}]},
      {?__(16,"Width"),active_vector_width,
       [{info,?__(17,"Width of vector (in pixels)")},{range,{1.0,10.0}}]},
      {color,?__(18,"Color"),active_vector_color,
       [{info,?__(19,"Color of vector")}]}]}],
       [{title,?__(20,"Vector Display")}]},
      {vframe,
       [{label_column,
     [{?__(23,"Size"),clip_plane_size,
       [{info,?__(24,"Size of user-defined clipping indicator")},{range,{0.1,100.0}}]},
      {color,?__(25,"Color"),clip_plane_color,
       [{info,?__(26,"Color of user-defined clipping indicator")}]}]}],
       [{title,?__(27,"Clipping Plane Indicator")}]}]},
     {vframe,
      [{vframe,
       [{?__(38,"Customize Drag Response"),drag_custom,
         [{info,?__(39,"Customize the drag to mouse motion ratio. Unchecked uses default settings.")},
	  {hook, Disable}
	 ]}]},
        {vframe,
          [{hframe,
              [{slider,{text,drag_speed_abs,[{range,{1.0,10.0}}]}}]}],
          [{title,?__(50,"Mouse Speed for Dragging")}]},
         {vframe,
          [{hframe,
              [{slider,{text,drag_speed_relative,[{range,{1.0,10.0}}]}}]}],
           [{title,?__(51,"Mouse Speed for Scaling")}]},
         {vframe,
          [{hframe,
           [{slider,{text,drag_speed_rotate,[{range,{1.0,10.0}}]}}]}],
       [{title,?__(52,"Mouse Speed for Rotating")}]}],
    [{title,?__(49,"Drag Preferences")}]}]}.

constraint_prefs() ->
    A = wings_s:modkey(alt),
    S = wings_s:modkey(shift),
    C = wings_s:modkey(ctrl),
    P = "+",
    CS = C++P++S,
    CA = C++P++A,
    SA = S++P++A,
    CSA = C++P++S++P++A,

    Set_Con_Default = [{A,alt},
                       {S,shift},
                       {C,ctrl},
                       {CS,ctrl_shift},
                       {CA,ctrl_alt},
                       {SA,shift_alt},
                       {CSA,ctrl_shift_alt}],

    {hframe,
     [{vframe,
       [{hframe,
	 [{vframe,
	   [{label_column,
	     [{A,con_dist_alt,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[A])},{range,{?NONZERO,1000.0,10.0}}]},
	      {S,con_dist_shift,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[S])},{range,{?NONZERO,1000.0,1.0}}]},
	      {C,con_dist_ctrl,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[C])},{range,{?NONZERO,1000.0,0.1}}]},
	      {CS,con_dist_ctrl_shift,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[CS])},{range,{?NONZERO,1000.0,0.01}}]},
	      {CA,con_dist_ctrl_alt,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[CA])},{range,{?NONZERO,1000.0,0.001}}]},
	      {SA,con_dist_shift_alt,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[SA])},{range,{?NONZERO,1000.0,0.25}}]},
	      {CSA,con_dist_ctrl_shift_alt,
	       [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					[CSA])},{range,{?NONZERO,1000.0,0.025}}]}],
	     [{title,?__(22,"Distance Constraints")}]},

	    {label_column,
	     [{A,con_scale_alt,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[A]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 10.0}}]},
	      {S,con_scale_shift,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[S]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 1.0}}]},
	      {C,con_scale_ctrl,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[C]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 0.1}}]},
	      {CS,con_scale_ctrl_shift,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[CS]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 0.01}}]},
	      {CA,con_scale_ctrl_alt,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[CA]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 0.5}}]},
	      {SA,con_scale_shift_alt,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[SA]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 0.25}}]},
	      {CSA,con_scale_ctrl_shift_alt,
	       [{info,wings_util:format(?__(14,"Constraint bound to ~s for scale operations."),
					[CSA]) ++ ?__(15," Alternate constrains to reciprocal (1.0/Value).")},
		{range,{?NONZERO, 1000.0, 0.2}}]}],
	     [{title,?__(25,"Scale Factors")}]}]},

	  {hframe,
	   [{vframe,
	     [{label_column,
	       [{A,con_dist_a_alt,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [A])++wings_util:format(?__(17,"(default 1/~s)"),["64"])},{range,{?NONZERO, 1000.0, 1/64}}]},
		{S,con_dist_a_shift,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [S])++wings_util:format(?__(17,"(default 1/~s)"),["4"])},{range,{?NONZERO, 1000.0, 1/4}}]},
		{C,con_dist_a_ctrl,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [C])++wings_util:format(?__(17,"(default 1/~s)"),["8"])},{range,{?NONZERO, 1000.0, 1/8}}]},
		{CS,con_dist_a_ctrl_shift,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [CS])++wings_util:format(?__(17,"(default 1/~s)"),["32"])},{range,{?NONZERO, 1000.0, 1/32}}]},
		{CA,con_dist_a_ctrl_alt,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [CA])++?__(18,"(default 5/8)")},{range,{?NONZERO, 1000.0, 5/8}}]},
		{SA,con_dist_a_shift_alt,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [SA])++wings_util:format(?__(17,"(default 1/~s)"),["16"])},{range,{?NONZERO, 1000.0, 1/16}}]},
		{CSA,con_dist_a_ctrl_shift_alt,
		 [{info,wings_util:format(?__(12,"Constraint bound to ~s for distance operations."),
					  [CSA])++?__(19,"(default 3/64)")},{range,{?NONZERO, 1000.0, 3/64}}]}],
	       [{title,?__(29,"Alternate Distances")}]},
	      {label_column,
	       [{A,con_rot_alt,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [A]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 180.0}}]},
		{S,con_rot_shift,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [S]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 15.0}}]},
		{C,con_rot_ctrl,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [C]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 1.0}}]},
		{CS,con_rot_ctrl_shift,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [CS]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 0.1}}]},
		{CA,con_rot_ctrl_alt,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [CA]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 22.5}}]},
		{SA,con_rot_shift_alt,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [SA]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 135.0}}]},
		{CSA,con_rot_ctrl_shift_alt,
		 [{info,wings_util:format(?__(13,"Constraint bound to ~s for rotations."),
					  [CSA]) ++ ?__(16," Alternate constrains to supplementary (180.0 minus angle).")},
		  {range,{?NONZERO, 360.0, 144.0}}]}],
	       [{title,?__(23,"Rotation Constraints")}]}]}]}]}
       ,{label,?__(30,"Deleting value or entering '0' resets constraint to Wings default")}
       ],
       [{title,?__(32,"Default Constraint Values")}]},
      {vframe,
       [{vframe,
	 [{label_column,
	   [{?__(1,"Distance"),{menu,Set_Con_Default,con_dist_default}},
	    {?__(2,"Rotation"),{menu,Set_Con_Default,con_rot_default}},
	    {?__(3,"Scale"),{menu,Set_Con_Default,con_scale_default}}]}],
	 [{title,?__(4,"Default Modifier Keys")}]},
	{vframe,
	 [{vframe,
	   [{vradio,[{?__(9,"Distance Constraints"),con_dist_},
		     %% ends with _ to allow atom_to_string completion
		     {?__(10,"Alternate Distances"),con_dist_a_}], con_dist_set}],
	   [{title,?__(11,"Save Constraints to...")}]},
	  {?__(33,"Use Alternate Constraints"),con_alternate,
	   [{info,?__(34,"Checked uses alternate distances, reciprocal scale factors, and supplementary angles.")
	     ++?__(35," Switch on the fly pressing [Shift]+[Tab] during a drag operation.")}]}]}],
       [{title,?__(31,"Set Constraint Preferences")}]}]}.

ui_prefs() ->
    Langs0 = wings_lang:available_languages(),
    InterfaceIcons = [{?__(51,"Classic"), classic},
		      {?__(52,"Blue Cube"), bluecube},
		      {?__(53,"Purple Tube"), purpletube}],
    Langs = [{language_name(L),L} || L <- Langs0],

    {hframe,
     [{vframe,
       [{label_column,
	 [{?__(12,"Title (Active) Background"), {color,title_active_color}},
	  {?__(11,"Title (Passive) Background"),{color,title_passive_color}},
	  {?__(10,"Title Text"),		  {color,title_text_color}},
	  separator,
	  {?__(2,"Desktop/Geometry Background"),{color,background_color}},
	  separator,
	  {?__(28,"Menubar Background"),{color,menu_bar_bg}},
	  {?__(31,"Menubar Text"),	  {color,menubar_text}},
	  separator,
	  {?__(6,"Menu Background"),       {color,menu_color}},
	  {?__(3,"Menu Text"),	     {color,menu_text}},
	  {?__(4,"Menu Highlight"),	     {color,menu_hilite}},
	  {?__(5,"Menu Highlighted Text"), {color,menu_hilited_text}},
	  separator,
	  %% {?__(9,"Dialog Background"),     {color,dialog_color}},
	  %% {?__(7,"Dialog Text"),	     {color,dialog_text}},
	  %% {?__(8,"Dialog (Disabled) Text"),{color,dialog_disabled}},
	  %% separator,
	  {?__(29,"Info Line Background"), {color,info_line_bg}},
	  {?__(30,"Info Line Text"),	 {color,info_line_text}},
          separator,
          {?__(35,"Window Background"),      {color, outliner_geograph_bg}},
          {?__(36,"Window Text"),		{color, outliner_geograph_text}},
          {?__(37,"Window Highlighter"),	{color, outliner_geograph_hl}},
          {?__(38,"Window Highlighted Text"),{color, outliner_geograph_hl_text}},
          {?__(39,"Window Disabled"),	{color, outliner_geograph_disabled}}
         ]},
	separator,
	{?__(24,"Flat Panels"),	         flat_color_panels},
        {menu,InterfaceIcons,interface_icons},
        {?__(47,"Show Toolbar"), show_toolbar},
        {?__(27,"Extended Toolbar Icons"),extended_toolbar}
       ],
       [{title,?__(13,"Colors")}]},
      {vframe,
       [{hframe,
	 [{vframe,
	   [{vframe,
	     [{fontpicker,system_font}],
	     [{title,?__(1,"System Font")}]},
	    {vframe,
	     [{menu,Langs,language}],
	     [{title,?__(23,"Language")}]},
	    {vframe,
	     [{fontpicker,console_font}],
	     [{title,?__(15,"Console Font")}]},
	    {label_column,
	     [{?__(16,"Width"),     {text,console_width,[{range,{12,120}}]}},
	      {?__(17,"Height"),    {text,console_height,[{range,{3,72}}]}},
	      {?__(18,"Save Lines"),{text,console_save_lines,[{range,{0,10000}}]}},
	      {?__(19,"Background"),{color,console_color}},
	      {?__(20,"Text"),      {color,console_text_color}}],
	     [{title,?__(22,"Console")}]}
	   ]}]},
	{hframe,
	 [{vframe,
	   [{?__(25, "Use the OS native color dialog"), color_dialog_native},
	    {?__(42,"View render with external viewer"),render_load_image}]}]},
	{oframe,
	 [{atom_to_list(Format),viewer_prefs(Format)}
	  || {Format,_,_} <- wings_job:render_formats()],
	 viewer_frame,
	 [{style,buttons}]}]}
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

%% Localised languages in their local language with unicode integers
language_name("cs") -> [268,"esk",253]; %Czech 
language_name("en") -> "English";
language_name("de") -> "Deutsch";
language_name("es") -> ["Espa",241,"ol"];
language_name("es-ES") -> ["Espa",241,"ol"];
language_name("fi") -> "Suomi";
language_name("fr") -> ["Fran",231,"ais"];
language_name("hu") -> "Hungarian";
language_name("it") -> "Italiano";
language_name("pl") -> "Polski";
language_name("pt") -> ["Portugu",234,"s"]; 
language_name("tr") -> ["T",252,"rk",231,"e"]; %Turkish
language_name("ru") -> [1056,1091,1089,1089,1082,1080,1081]; %Russian
language_name("sv") -> "Svenska";
language_name("ko") -> "Korean";
language_name("zh-cn") -> "Simplified Chinese";
language_name("zh-tw") -> "Traditional Chinese";
language_name("jp") -> "Japanese";
language_name(Other) -> Other.

misc_prefs() ->
    Flags = case wings_gl:is_ext({1,2}, 'GL_ARB_imaging') of
		true -> [];
		false ->
		    [disable,
		     {info,?__(1,"Opacity settings not supported using this version of OpenGL")}]
	    end,
    AutoFun = fun(_, Enable, Store) ->
		      wings_dialog:enable(autosave_time, Enable, Store)
	      end,
    OpenCL = case wings_cl:is_available() of
		 true -> [{info,?__(40,"A value of 0 will use the standard implementation, " 
				    "a larger value takes more memory "
				    "(experimental feature requires good OpenCL drivers)")}];
		 false ->
		     [disable,{info,?__(41,"OpenCL drivers is required ")}]
	     end,
    {vframe,
     [{hframe,[{?__(2,"Save automatically every"),autosave, [{hook,AutoFun}]},
	       {text,autosave_time,[{range,{1,1440}}]},
	       {label,?__(3,"minutes")}]},
      {hframe,[{label,?__(4,"Undo levels")},
	       {text,num_undo_levels,[{range,{1,128}}]}]},
      {vframe,
       [{label_column,
	 [{?__(5,"Angle per second"),auto_rotate_angle}]}],
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
	    {label,?__(13,"Moving Opacity")},
	    {label, ?__(42, "OpenCL proxy level")}]},
	  {vframe,
	   [{slider,{text,proxy_static_opacity,[{range,{0.0,1.0}}|Flags]}},
	    {slider,{text,proxy_moving_opacity,[{range,{0.0,1.0}}|Flags]}},
	    {slider,{text,proxy_opencl_level,[{range,{0,5}}|OpenCL]}}
	   ]}]}
       ],
       [{title,?__(14,"Proxy Mode")}]},
      {vframe,
       [{hframe,
	 workaround([
		     {jumpy_camera,
		      ?__(19,"Camera moves and interactive commands are jumpy"),
		      ?__(20,"Problem occurs on Mac OS X 10.3 (Panther)")},
		     {ungrab_bug,
		      ?__(26,"Camera moves steals focus"),
		      ?__(27,"Problem occurs on linux")}
		    ])},
	separator,
	{hframe,[{label,?__(23,"Edge offsets:")},
		 {text,polygon_offset_f,[{range,{1.0,100.0}}]},
		 {text,polygon_offset_r,[{range,{1.0,100.0}}]}],
	 [{title,?__(22,"Edge display problems?")}]}],
       [{title,?__(21,"Workarounds")}]},
      {vframe,
       [{hframe,[{?__(24,"Show Develop menu"),show_develop_menu,
		  [{info,?__(25,"Show a menu with tools for the Wings developers")}]}]},
	{vframe,
	  [{label_column,
	    [{?__(28,"Maximum menu height in pixels"),max_menu_height,
	     [{info,?__(29,"Menus are clipped and continue in 'More...' submenu.")
	       ++" "++?__(30,"Less than 1 sets menu clipping to auto.")}]}]}]}]}
       
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

smart_set_value(Font, Val0, St) when Font =:= system_font; Font =:= console_font ->
    Val = wings_text:get_font_info(Val0),
    smart_set_value_1(Font, Val, St);
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
		outliner_geograph_bg ->
		    wings_frame:update_theme();
		autosave ->
		    wings_file:init_autosave();
		autosave_time ->
		    wings_file:init_autosave();
		proxy_shaded_edge_style ->
		    clear_proxy_edges(St);
		proxy_opencl_level ->
		    clear_proxy(St);
		system_font ->
		    delayed_set_value(Key, OldVal, Val),
		    wings_text:reload_font(Key, Val),
		    Str = ?__(1,"The change to the system font will take\n"
			      "effect the next time Wings 3D is started."),
		    wings_u:message(Str);
		interface_icons ->
		    delayed_set_value(Key, OldVal, Val),
		    Str=?__(2,"The change to the interface icons will take\n"
			    "effect the next time Wings 3D is started."),
		    wings_u:message(Str);
                show_toolbar ->
                    delayed_set_value(Key, OldVal, Val),
                    wings_frame:show_toolbar(Val);
		extended_toolbar ->
		    delayed_set_value(Key, OldVal, Val),
		    Str = ?__(2,"The change to the interface icons will take\n"
			      "effect the next time Wings 3D is started."),
		    wings_u:message(Str);
		console_font ->
		    wings_text:reload_font(Key, Val),
		    delayed_set_value(Key, OldVal, Val),
		    Str = ?__(4,"The change to the console font will take\n"
			      "effect the next time Wings 3D is started."),
		    wings_u:message(Str);
		console_color ->
		    set_console();
		console_text_color ->
		    set_console();
		console_cursor_color ->
		    set_console();
		camera_mode ->
		    wings_wm:translation_change();
		num_buttons ->
		    wings_wm:translation_change();
		language ->
		    case Val of
			V when V =:= "zh-cn"; V =:= "zh-tw"; V =:= "ko" ->
			    delayed_set_value(Key, OldVal, Val),
			    Str = ?__(5,"The language change will take effect\n"
				      "the next time Wings 3D is started."),
		          wings_u:message(Str);
		      _ ->
		          wings_lang:load_language(Val)
		    end;
		polygon_offset_f ->
		    erase(polygon_offset);
		polygon_offset_r ->
		    erase(polygon_offset);
		normal_vector_size ->
		    update_normal_dlist(St);
		normal_vector_color ->
		    update_normal_dlist(St);
		material_default ->
		    delayed_set_value(Key, OldVal, Val),
		    Str = ?__(3,"The change to the default material color will take\n"
			      "effect the next time Wings 3D is started."),
		    wings_u:message(Str);
		show_develop_menu ->
		    foreach(fun(W) ->
				    wings_wm:send(W, language_changed) end,
			    wings_wm:windows());
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

clear_proxy(St) ->
    wings_dl:map(fun(D, _) -> clear_proxy(D, St) end, []).

clear_proxy(#dlo{proxy_data=Data}=D, St) ->
    PD = wings_proxy:invalidate(Data, all),
    wings_proxy:update(D#dlo{proxy_data=PD}, St).

clear_proxy_edges(St) ->
    wings_dl:map(fun(D, _) -> clear_proxy_edges(D, St) end, []).

clear_proxy_edges(#dlo{proxy_data=Data}=D, St) ->
    PD = wings_proxy:invalidate(Data, edges),
    wings_proxy:update(D#dlo{proxy_data=PD}, St).

make_query({'VALUE',Val}) ->
    Val;
make_query([_|_]=List)	->
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
make_query({fontpicker,Key}) ->
    Def = get_value(Key),
    {fontpicker,Def,[{key,Key}]};
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
make_query({label_column, List}) ->
    make_query({label_column, List, []});
make_query({label_column, List, Opts}) ->
    MQ = fun({Str=[_|_],What}) when not(is_atom(What)) ->
		 {Str, make_query(What)};
	    (Other) ->
		 make_query(Other)
	 end,
    {label_column, [MQ(H) || H <- List], Opts};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

set_console() ->
    case wings_wm:is_window(console) of
	true  -> wings_console:window();
	false -> ok
    end.
