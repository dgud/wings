%%
%% HLines -- Hidden Line Renderer for Wings 0.98.17a and higher.
%%
%% Based on "HLines" hidden line elimination program from "Programming
%% Principles in Computer Graphics" and "Computer Graphics for Java
%% Programmers" by Leen Ammeraal, http://home.planet.nl/~ammeraal/ .
%% The source code listed in the books can be found here:
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/older/ppcgraph.zip
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/grjava.zip
%%
%% Copyright (c) 2003-2015  Dmitry Efremov <defremov@aha.ru>
%% wpc_hlines.erl,v 1.46 2006/02/26 21:40:13
%% Support for eps/svg format exporting ,Colored faces rendering ,Processing speed up
%%
%% Modified Version
%% wpc_hlines.erl,v 2.01 2016/03/03
%% wpc_hlines.erl,v 2.0  2015/08/22
%% wpc_hlines.erl,v 1.5  2015/07/22
%% Show elements,Colored lines rendering,Line style,SVG options and Optimize etc.
%% Copyright (c) 2015 Tukubado(tkbd)
%%
%% BUGS:
%%  Near clipping will not work correctly if occurs in the viewport
%%  Duplicate and zero length line segments are generated in some cases
%%
%% $Id$
%%

-module(wpc_hlines).
-author('Dmitry Efremov <defremov@aha.ru>').

-export([init/0, menu/2, command/2]).
-import(lists, [
		foldl/3,
		foreach/2,
		keymember/3,
		keysearch/3,
		keysort/2,
		keyreplace/4
	       ]).
-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-define(EPS, 1.0e-6).
-define(EPS1, 1.0e-5).
-define(EPS2, 1.0e-4).
-define(BIG, 1.0e30).
-define(MAXDEPTH, 10).

-define(DEF_FILE_TYPE, file_eps).
-define(DEF_WIDTH, 288).
-define(DEF_HEIGHT, 216).
-define(DEF_EDGE_MODE, hard_edges).
-define(DEF_EDGE_WIDTH_OUTLINE, 1.0).
-define(DEF_EDGE_WIDTH_HARD, 1.0).
-define(DEF_EDGE_WIDTH_CREASE, 1.0).
-define(DEF_EDGE_WIDTH_MATERIAL, 1.0).
-define(DEF_EDGE_WIDTH_REGULAR, 1.0).
-define(DEF_EDGE_WIDTH_LUCENT, 0.5).
-define(DEF_EDGE_ONE_WIDTH_FOR_ALL, true).
-define(DEF_CREASE_ANGLE, 0).
-define(DEF_LINE_CAP, 1).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_OPTIMIZE, false).
-define(DEF_COLL_ANGLE, 0.5).
-define(DEF_COLL_DIST, 0.25).
-define(DEF_DRAW_MODE, both).
-define(DEF_RESPONSIVE, false).

-define(DEF_VAR_STROKE, false).
-define(DEF_SVG_ATTB_TYPE, attribute).
-define(DEF_SVG_RENDER_TYPE, smooth_fill ).

-define(DEF_DIVISION_PRIORITY, scene).
-define(DEF_Z_DIST_OFFSET,2).

-define(DEF_SVG_ART_FILL_TYPE, 1 ).
-define(DEF_SVG_ART_LINE_TYPE, 1 ).

-define(DEF_FILL_SHADE_TYPE, 0 ).
-define(DEF_LIGHT_POS, {-480,-512,500,0.00096} ).

-define(DEF_OUTL_COLOR, {0.0, 0.0, 0.0, 1.0}).
-define(DEF_HARD_COLOR, {0.0, 0.0, 0.0, 1.0}).
-define(DEF_CREA_COLOR, {0.0, 0.0, 0.0, 1.0}).
-define(DEF_MATL_COLOR, {0.0, 0.0, 0.0, 1.0}).
-define(DEF_LCNT_COLOR, {0.0, 0.0, 0.0, 1.0}).
-define(DEF_REGL_COLOR, {0.0, 0.0, 0.0, 1.0}).


-define(DEF_OUTL_PATTERN, "0" ).
-define(DEF_HARD_PATTERN, "0" ).
-define(DEF_CREA_PATTERN, "0" ).
-define(DEF_MATL_PATTERN, "0" ).
-define(DEF_REGL_PATTERN, "0" ).
-define(DEF_LCNT_PATTERN, "2 2" ).

-define(DEF_SCALE_PROP, false).
-define(DEF_SVG_COLOR_INT, 0).


init() ->
    %% that force the wrong preferences stored before the fix to be updated
    case get_pref( light_pos, ?DEF_LIGHT_POS) of
	[] -> wpa:pref_delete(?MODULE, light_pos);
	_ -> ignore
    end,
    true.

menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{"Cartoon edges (.eps, .svg)...", eps, [option]}].

command({file, {export, {eps, Arg}}}, St) ->
    export(Arg, export, St);
command({file, {export_selected, {eps, Arg}}}, St) ->
    export(Arg, export_selected, St);
command(_, _) -> next.

export(Arg, Op, _) when is_atom(Arg) ->
    wpa:dialog(Arg, ?__(1,"Cartoon edges Render Options (EPS/SVG)"), dialog(),
	       fun(Res) -> {file, {Op, {eps, Res}}} end);
export(Arg, Op, St0) when is_list(Arg) ->
    set_pref(Arg),
    Camera_info = wpa:camera_info([aim, distance_to_aim,
				   azimuth, elevation, tracking,
				   fov, hither, yon, pos_dir_up]),
    File_type = get_pref(file_type, ?DEF_FILE_TYPE),
    Props = [{title, "Export"},
	     {ext, file_type(File_type, suffix)},
	     {ext_desc, file_type(File_type, desc)},
	     {camera_info, Camera_info},
	     {subdivisions, get_pref(subdivisions, ?DEF_SUBDIVISIONS)},
	     {win_size, wings_wm:win_size()},
	     {ortho_view, wings_wm:get_prop(orthogonal_view)}],

    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id, wpa:vm_freeze(We)} || {Id, We} <- Shapes0],
    St = St0#st{shapes = gb_trees:from_orddict(Shapes)},

    case Op of
        export ->
            ?SLOW(wpa:export(Props, fun_export(Props), St))
		;
        export_selected ->
            ?SLOW(wpa:export_selected(Props, fun_export(Props), St))
    end.



dialog() ->
    IntlPT = ?__(1,"pt"),
    File_type = get_pref(file_type, ?DEF_FILE_TYPE),
    BB_width = get_pref(bb_width, ?DEF_WIDTH),
    BB_height = get_pref(bb_height, ?DEF_HEIGHT),
    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
    Edge_width_outline = get_pref(edge_width_outline,
				  ?DEF_EDGE_WIDTH_OUTLINE),
    Edge_width_hard = get_pref(edge_width_hard, ?DEF_EDGE_WIDTH_HARD),
    Edge_width_crease = get_pref(edge_width_crease, ?DEF_EDGE_WIDTH_CREASE),
    Edge_width_material = get_pref(edge_width_material, ?DEF_EDGE_WIDTH_MATERIAL),
    Edge_width_regular = get_pref(edge_width_regular,
				  ?DEF_EDGE_WIDTH_REGULAR),
    Edge_width_lucent = get_pref(edge_width_lucent, ?DEF_EDGE_WIDTH_LUCENT),

    Edge_one_width_for_all = get_pref(edge_one_width_for_all,
				      ?DEF_EDGE_ONE_WIDTH_FOR_ALL),
    Crease_angle = get_pref(crease_angle, ?DEF_CREASE_ANGLE),
    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),
    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    Optimize = get_pref(optimize, ?DEF_OPTIMIZE),
    Coll_angle = get_pref(coll_angle, ?DEF_COLL_ANGLE),
    Coll_dist = get_pref(coll_dist, ?DEF_COLL_DIST),
    Draw_mode = get_pref(draw_mode, ?DEF_DRAW_MODE),
    Responsive = get_pref(responsive, ?DEF_RESPONSIVE),
    Var_stroke = get_pref(var_stroke, ?DEF_VAR_STROKE),
    Svg_attb_type = get_pref(svg_attb_type, ?DEF_SVG_ATTB_TYPE),
    Division_priority = get_pref( division_priority, ?DEF_DIVISION_PRIORITY),
    Z_dist_offset = get_pref( z_dist_offset, ?DEF_Z_DIST_OFFSET),

    Svg_art_fill_type = get_pref(svg_art_fill_type, ?DEF_SVG_ART_FILL_TYPE),
    Svg_art_line_type = get_pref(svg_art_line_type, ?DEF_SVG_ART_LINE_TYPE),

    Fill_shade_type = get_pref(fill_shade_type, ?DEF_FILL_SHADE_TYPE),
    Light_pos = get_pref( light_pos, ?DEF_LIGHT_POS),

    Outl_color = get_pref( outl_color, ?DEF_OUTL_COLOR),
    Hard_color = get_pref( hard_color, ?DEF_HARD_COLOR),
    Crea_color = get_pref( crea_color, ?DEF_CREA_COLOR),
    Matl_color = get_pref( matl_color, ?DEF_MATL_COLOR),
    Regl_color = get_pref( regl_color, ?DEF_REGL_COLOR),
    Lcnt_color = get_pref( lcnt_color, ?DEF_LCNT_COLOR),

    Outl_pattern = get_pref( outl_pattern, ?DEF_OUTL_PATTERN),
    Hard_pattern = get_pref( hard_pattern, ?DEF_HARD_PATTERN),
    Crea_pattern = get_pref( crea_pattern, ?DEF_CREA_PATTERN),
    Matl_pattern = get_pref( matl_pattern, ?DEF_MATL_PATTERN),
    Regl_pattern = get_pref( regl_pattern, ?DEF_REGL_PATTERN),
    Lcnt_pattern = get_pref( lcnt_pattern, ?DEF_LCNT_PATTERN),

    Scale_prop = get_pref( scale_prop, ?DEF_SCALE_PROP),
    Svg_color_int = get_pref( svg_color_int, ?DEF_SVG_COLOR_INT),


    Svg_render_type = get_pref( svg_render_type, ?DEF_SVG_RENDER_TYPE),
    [
     {hframe, [
	       {label, ?__(200,"File type") },
	       {menu, [
		       {file_type(file_eps), file_eps},
		       {file_type(file_svg), file_svg},
		       {file_type(file_test_svg), file_test_svg}
		      ], File_type, [{key, file_type}]}
	      ]},

     {vframe, [
	       {hframe,[
			{label, ?__(100,"Width")},
			{text, BB_width, [{key, bb_width}]},
			{label, ?__(101,"Height")},
			{text, BB_height, [{key, bb_height}]},
			{label, IntlPT},
			{label, ?__(102,"Proportional Effect")},
			{ ?__(103,"Enable"), Scale_prop, [{key, scale_prop}
							 ]}
		       ]}
	      ], [{title, ?__(201,"Bounding box") }]},

     {vframe, [
	       {hframe, [
			 {label,?__(104,"Subdiv Steps")},
			 {text,SubDiv,[{key,subdivisions},{range,0,4}]},
			 {label,?__(105,"Overlap push")},
			 {text,Z_dist_offset,[{key,z_dist_offset},{range,0,10}]}
			]}

	      ], [{title, ?__(106,"Pre-rendering") }]},


     {hframe, [
	       { menu, [
			{ ?__(107,"Scene (made many sliced,and file size larger)")     , scene},
			{ ?__(108,"Path  (divided nicely,but messed if overlap areas)"), path }
		       ], Division_priority, [{key, division_priority }]}
	      ]
     , [{title, ?__(109,"Surface division Priorities") }]},

     {hframe, [
	       {vradio, [
			 {?__(110,"Both"), both },
			 {?__(111,"Edge"), edge_only },
			 {?__(112,"Face"), face_only }
			], Draw_mode, [{key, draw_mode}, {title, ?__(113 ,"Export") }]},


	       {vframe, [
			 {hframe, [ {label, ?__(114,"CSS:") },
				    {menu, [
					    {?__(115,"Use Attribute"), attribute},
					    {?__(116,"Use ClassName"), css_class},
					    {?__(117,"Use AttrCompd"), compound }
					   ], Svg_attb_type, [{key, svg_attb_type}]},
				    {label, ?__(118,"Shape-rendering") },
				    {menu, [
					    { ?__(119,"Auto")      , none  },
					    { ?__(120,"Smooth")    , smooth_fill },
					    { ?__(121,"Jaggy line"), jaggy_line  }
					   ], Svg_render_type, [{key, svg_render_type}]}
				  ]},
			 {vframe, [
				   {hframe, [ {label, ?__(122,"NPR:Fill") }, {menu, define_svg_filter(ui, 0), Svg_art_fill_type, [{key, svg_art_fill_type}]},  {?__(123,"Responsive"), Responsive, [{key, responsive}]}  ]},
				   {hframe, [ {label, ?__(124,"NPR:Line") }, {menu, define_svg_filter(ui, 0), Svg_art_line_type, [{key, svg_art_line_type}]},  {?__(125,"VarStroke"), Var_stroke, [{key, var_stroke}]}   ]},
				   {hframe, [ {label, ?__(126,"Shading ") }, {menu, define_fill_type() , Fill_shade_type,        [{key, fill_shade_type}]}  ,
					      {label, ?__(127,"Lighting") }, {menu, define_light_pos() , Light_pos,              [{key, light_pos}]}  ]},
				   {hframe, [ {label, ?__(128,"Color interpolation")},  { menu, [{ "sRGB", 0 }, { "linearRGB", 1 }, { "auto", 2 } ], Svg_color_int, [{key, svg_color_int} ]}  ]}
				  ]}
			], [{title, ?__(202,"SVG Export Options") }]}

	      ]},
     separator,
     {vframe, [  {label, ?__(129,"Edge display Options")} ]},

     {hframe, [
	       {hradio, [
			 {?__(130,"All ")  , all_edges},
			 {?__(131,"Hard")  , hard_edges},
			 {?__(132,"Others"), no_edges}
			], Edge_mode, [{key, edge_mode},
				       {title, ?__(133,"Show edges") }]},

	       {hframe, [
			 {slider,
			  {text, Crease_angle, [
						{key, crease_angle}, {range, {0, 180}}
					       ]}}
			], [{title, ?__(134,"Crease angle(Exclude if exceed an angle.)") }]}

	      ]},

     {vframe, [
	       {hframe,  [ {label, ?__(135, "Outline ") }, {text, Edge_width_outline,  [{key, edge_width_outline}, {range, {0.0, ?BIG}}]}, {label, IntlPT},
			   {color,  Outl_color, [{key, outl_color}] },  { menu, define_dot_styles(), Outl_pattern, [{key, outl_pattern }]}
			 ]},separator,

	       {hframe, [
			 {vframe, [
				   {label, ?__(136,"Hard") },
				   {label, ?__(137,"Crease") },
				   {label, ?__(138,"Material") },
				   {label, ?__(139,"Lucent") }
				  ]},
			 {vframe, [
				   {text, Edge_width_hard,  [{key, edge_width_hard}, {range, {0.0, ?BIG}}]},
				   {text, Edge_width_crease, [{key, edge_width_crease}, {range, {0.0, ?BIG}}]},
				   {text, Edge_width_material, [{key, edge_width_material}, {range, {0.0, ?BIG}}]},
				   {text, Edge_width_lucent, [{key, edge_width_lucent}, {range, {0.0, ?BIG}}]}
				  ]},
			 {vframe, [
				   {hframe,  [      {label, IntlPT}, {color,  Hard_color, [{key, hard_color}] }  ]},
				   {hframe,  [      {label, IntlPT}, {color,  Crea_color, [{key, crea_color}] }  ]},
				   {hframe,  [      {label, IntlPT}, {color,  Matl_color, [{key, matl_color}] }  ]},
				   {hframe,  [      {label, IntlPT}, {color,  Lcnt_color, [{key, lcnt_color}] }  ]}
				  ]},
			 {vframe, [
				   {hframe,  [    { menu, define_dot_styles(), Hard_pattern, [{key, hard_pattern }]} ]},
				   {hframe,  [    { menu, define_dot_styles(), Crea_pattern, [{key, crea_pattern }]} ]},
				   {hframe,  [    { menu, define_dot_styles(), Matl_pattern, [{key, matl_pattern }]} ]},
				   {hframe,  [    { menu, define_dot_styles(), Lcnt_pattern, [{key, lcnt_pattern }]} ]}
				  ]}
			]},
	       separator,
	       {hframe,  [   {label, ?__(140,"Regular ") } ,  {text, Edge_width_regular, [{key, edge_width_regular}, {range, {0.0, ?BIG}} ]},
			     {label, IntlPT}, {color, Regl_color, [{key, regl_color}] } ,
			     { menu, define_dot_styles(), Regl_pattern, [{key, regl_pattern }]} ,
			     {?__(141,"All") , Edge_one_width_for_all, [{key, edge_one_width_for_all}]}
			 ]}

	      ], [{title, ?__(142,"Edge width,Color,dashed style") }]},


     {hframe, [
	       {vradio, [
			 {?__(143,"Butt")  , 0},
			 {?__(144,"Round") , 1},
			 {?__(145,"Square"), 2}
			], Line_cap, [{key, line_cap}, {title, ?__(146,"Line caps") }]},

	       {vframe, [
			 { ?__(147,"Merge"), Optimize, [{key, optimize}]},
			 {hframe, [
				   {vframe, [
					     {label, ?__(148,"Angle") },
					     {label, ?__(149,"Distance") }
					    ]},
				   {vframe, [
					     {text, Coll_angle, [
								 {key, coll_angle}, {range, {0.0, 90.0}}
								]},
					     {text, Coll_dist, [
								{key, coll_dist}, {range, {0.0, ?BIG}}
							       ]}
					    ]},
				   {vframe, [
					     {label, [176]},
					     {label, IntlPT}
					    ]}
				  ]}
			], [{title, ?__(150,"Collinear lines") }]}

	      ]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

fun_export(Props) ->
    fun (File_name, Scene) -> do_export(Props, File_name, Scene) end.

file_type(file_eps, desc) -> "Encapsulated Postscript";
file_type(file_eps, suffix) -> ".eps";
file_type(file_svg, desc) ->       "Scalable Vector Graphics";
file_type(file_svg, suffix) -> ".svg";
file_type(file_test_svg, desc) -> "SVG style setting Help(Non 3D)";
file_type(file_test_svg, suffix) -> ".svg".



file_type(File_type) ->
    lists:flatten([file_type(File_type, desc), " (*",
		   file_type(File_type, suffix), ")"]).

file_type_funs(file_eps) ->
    {fun write_eps_header/5, fun write_eps_line_group/9,
     fun write_eps_polygons/5, fun write_eps_footer/1};
file_type_funs(file_svg) ->
    {fun write_svg_header/5, fun write_svg_line_group/9,
     fun write_svg_polygons/5, fun write_svg_footer/1};
file_type_funs(file_test_svg) ->
    {fun write_svg_header/5, fun write_svg_line_group/9,
     fun write_svg_polygons/5, fun write_svg_footer/1}.

%%
%%  Main
%%

%%  Style Catalog Mode (for SVG Style preview,don't save the 3D Model Data)
do_export(Props, File_name, #e3d_file{objs=Objs, mat=Mats}) ->

    Is_style_catalog = (file_test_svg =:= get_pref(file_type, ?DEF_FILE_TYPE)),
    case Is_style_catalog of
	true ->

	    Start_time = os:timestamp(),


	    Wbb = get_max(get_pref(bb_width, ?DEF_WIDTH), 1500),
	    Hbb = get_max(get_pref(bb_height, ?DEF_HEIGHT), 750),



	    Mats_dict = materials(Mats),

	    wings_pb:start(""),
	    io:format("~n", []),
	    LSpos = {get_pref(fill_shade_type, ?DEF_FILL_SHADE_TYPE), get_pref( light_pos, ?DEF_LIGHT_POS) },
	    BB_size = {Wbb, Hbb},
	    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),

	    {Write_header, _Write_line_group, _Write_polygons, Write_footer} = file_type_funs(get_pref(file_type, ?DEF_FILE_TYPE)),
	    wings_pb:update(0.5, "All the SVG styles are exporting"),
	    wings_pb:pause(),
	    {ok, F} = file:open(File_name, [write]),

	    Write_header(F, BB_size, Line_cap,Mats_dict,LSpos),
	    Write_footer(F),
	    ok = file:close(F),

	    wings_pb:update(1.0, "done"),
	    wings_pb:done(),
	    io:format(" "++"done in ~.1f sec"++"~n", [timer:now_diff(os:timestamp(), Start_time) / 1.0e6]);


	%% Export Mode (3D Model export to SVG/EPS)
	false ->

	    Start_time = os:timestamp(),

	    [Aim, Distance, Azimuth, Elevation, {TrackX, TrackY}, Fov, Hither, Yonder, _Pos_dir_up] =
		proplists:get_value(camera_info, Props),

	    Taim = e3d_mat:translate(Aim),
	    Ry = e3d_mat:rotate(Azimuth, {0.0, 1.0, 0.0}),
	    Rx = e3d_mat:rotate(Elevation, {1.0, 0.0, 0.0}),
	    Tdist = e3d_mat:translate({TrackX, TrackY, -Distance}),
	    Veye = e3d_mat:mul(Tdist, e3d_mat:mul(e3d_mat:mul(Rx, Ry), Taim)),



	    %% io:format("EyePos ~p   ~n",[Pos_eye]),

	    LSpos = {get_pref(fill_shade_type, ?DEF_FILL_SHADE_TYPE), get_pref( light_pos, ?DEF_LIGHT_POS) },


	    Wbb = get_max(get_pref(bb_width, ?DEF_WIDTH), 1),
	    Hbb = get_max(get_pref(bb_height, ?DEF_HEIGHT), 1),
	    ARbb = Wbb / Hbb,
	    {Wwin, Hwin} = proplists:get_value(win_size, Props),
	    ARwin = Wwin / Hwin,

	    {ARw, ARh, Eps} = if
				  ARbb > ARwin -> {ARbb, 1.0, 1.0 / Hbb * 0.01};
				  true -> {ARwin, ARwin / ARbb, ARwin / Wbb * 0.01}
			      end,

	    Epseps = Eps * Eps,

	    Flen = 0.5 / math:tan(Fov * math:pi() / 180.0 / 2.0),
	    Zf = -Flen,

	    Is_ortho = proplists:get_value(ortho_view, Props),

	    Proj = if Is_ortho ->
	    		   fun({X, Y, _Z}) -> {X, Y};
	    		      ({{X1, Y1, _Z1}, {X2, Y2, _Z2}}) ->
	    			   {{X1, Y1}, {X2, Y2}}
	    		   end;
	    	      true ->
	    		   fun({X, Y, Z}) ->
	    			   Rz = case catch Zf / Z of
	    				    R when is_float(R) -> R; _ -> ?BIG end,
	    			   {X * Rz, Y * Rz};
	    		      ({{X1, Y1, Z1}, {X2, Y2, Z2}}) ->
	    			   Rz_1 = case catch Zf / Z1 of
	    				      R1 when is_float(R1) -> R1; _ -> ?BIG end,
	    			   Rz_2 = case catch Zf / Z2 of
	    				      R2 when is_float(R2) -> R2; _ -> ?BIG end,
	    			   {{X1 * Rz_1, Y1 * Rz_1}, {X2 * Rz_2, Y2 * Rz_2}}
	    		   end
	    	   end,
	    Front_face = if Is_ortho -> fun ortho_ff/1;
	    		    true -> fun persp_ff/1
	    		 end,
	    Side_face = if Is_ortho -> fun ortho_sf/1;
	    		   true -> fun persp_sf/1
	    		end,
	    Wvp = if Is_ortho -> Distance * ARw / Flen;
	    	     true -> ARw
	    	  end,
	    Hvp = if Is_ortho -> Distance * ARh / Flen;
	    	     true -> ARh
	    	  end,

	    View_port = {{-Wvp / 2.0, -Hvp / 2.0, -Yonder},
			 {Wvp / 2.0, Hvp / 2.0, -Hither}},

	    Frustum = if
			  Is_ortho -> View_port;
			  true -> {Wvp / 2.0, Hvp / 2.0, -Yonder, -Hither, Zf}
		      end,

	    Frustum_planes = frustum_planes(Frustum),

	    VCs = foldl(fun(#e3d_object{obj=Obj}, VCs_acc) ->
				VCs_acc ++ Obj#e3d_mesh.vs
			end, [], Objs),

	    EyeMesh = e3d_mesh:transform(#e3d_mesh{vs = VCs}, Veye),

	    VC_tree0 = ctree__from_list(Proj, EyeMesh#e3d_mesh.vs),

						%io:format("Mats=~w~n~n", [Mats]),
	    Mats_dict = materials(Mats),
						%io:format("Mats_dict=~w~n~n", [Mats_dict]),
	    Crease_angle = get_pref(crease_angle, ?DEF_CREASE_ANGLE),
	    Thresh_cos = math:cos((180.0 - Crease_angle) * math:pi() / 180.0) + ?EPS,
	    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
	    Edge_type_fun =
		case get_pref(edge_one_width_for_all, ?DEF_EDGE_ONE_WIDTH_FOR_ALL) of
		    false ->
			fun edge_type_group/6
			    ;
		    true ->
			fun edge_type_copy/6
		end,



	    %% Process Start

	    wings_pb:start(""),
	    io:format("~ts~n", ["Start"]),

	    Objs_total = length(Objs),
	    Draw_mode = get_pref(draw_mode, ?DEF_DRAW_MODE),

	    {VC_tree, Edge_dict, Face_tree, _VI_incr, _Obj_count}
		= foldl(fun(#e3d_object{name=Name, obj=Mesh},
			    {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0,
			     VI_incr, Obj_count0}) ->
				Obj_count = Obj_count0 + 1,

				Percent = Obj_count0 / Objs_total,
				wings_pb:update(Percent * 0.28 + 0.01,
						?__(1,"reading objects") ++ " " ++
						    integer_to_list(round(Percent * 100.0)) ++ "%"),
				wings_pb:pause(),
				io:format(?__(2,"Reading object ~B of") ++" ~B \"~s\"...",
					  [Obj_count, Objs_total, Name]),

				#e3d_mesh{vs=MVCs, fs=MFs, he=MHEs}=Mesh,
				Is_open = is_open(Mesh) orelse has_transp_faces(Mesh, Mats_dict),



				case Draw_mode =:= both orelse  Draw_mode =:= face_only of
				    false ->

					#e3d_mesh{fs=TMFs} = e3d_mesh:triangulate(Mesh),
					{VC_tree_acc, ME_dict, Face_tree_acc} =
					    add_trimesh(incr(TMFs, VI_incr),
							Is_open, Frustum, Frustum_planes, Front_face, Side_face,
							VC_tree_acc0, dict:new(), Face_tree_acc0)
					    ;
				    true ->


%%%%% Add convex and flatness tests
					{VC_tree_acc, ME_dict, Face_tree_acc} =
					    add_mesh(incr(MFs, VI_incr), VCs, Epseps,
						     Is_open, Frustum, Frustum_planes, Front_face, Side_face,
						     VC_tree_acc0, dict:new(), Face_tree_acc0 )

%%%%%
				end,


%%%%  Edge draw
				{HardEdge_set, Edge_set} = edge_sets(MHEs, MFs, Edge_mode, VI_incr),
				Edge_dict_acc = group_edges(Thresh_cos, HardEdge_set, Edge_set,
							    Edge_type_fun, ME_dict, Edge_dict_acc0,Mats_dict),

				io:format(" "++?__(3,"done")++"~n", []),

				{VC_tree_acc, Edge_dict_acc, Face_tree_acc,
				 VI_incr + length(MVCs), Obj_count}
			end, {VC_tree0, dict__new(), tree__empty(), 0, 0}, Objs),



	    %%  add_Mesh / add_trimesh  Is_open/transparent
						%
	    %% EDGE_DICT:
	    %% [{crease,[{3,7},{6,7},{0,2},{0,4},{5,7},{0,1}]},
	    %%  {outline,[{2,3},{4,6},{1,3},{1,5},{4,5},{2,6}]}]
						%

	    %% io:format("EDGE_DICT:~n ~p ~n",[Edge_dict]),


	    wings_pb:update(0.3, ?__(4,"reading objects")++" 100%"),
	    wings_pb:pause(),
	    io:format("Exporting"),

%%% for debug function (Warnings are get,but don't delete these functions)
	    %%   tree__to_list/1
	    %%   tree__from_list/1
	    %%   project/2

	    %%io:format("*\tVC_tree=~w~n", [VC_tree]),
	    %%    VCs1 = tree__to_list(VC_tree),
	    %%io:format("*\tVCs=~w~n", [VCs1]),
	    %%    V2C_tree = tree__from_list(project(Proj, VCs1)),
	    %%io:format("*\tV2C_tree=~w~n", [V2C_tree]),

						%io:format("*\tFace_tree=~w~n", [Face_tree]),


	    {Face_bspt, VC_tree1} = tree__fold(fun(FI, {FVIs, _FP, _Mat} = Face, {Face_bspt_acc0, VC_tree_acc0}) ->
						       FVCs = ctree__get(FVIs, VC_tree_acc0),
						       FZt = face_zlim(FVCs),
						       bspt__insert(FI, Face, FZt, Face_bspt_acc0, VC_tree_acc0)
					       end, {bspt__empty(), VC_tree}, Face_tree),


						%io:format("*\tFace_bspt=~w~n", [Face_bspt]),

	    O_qtree = tree__fold(fun(_FI, {FVIs, FP, Mat} = _Face, O_qtree_acc0) ->
					 {Is_transparent, _RGBA} = hd(dict__fetch(hd(Mat), Mats_dict)),
					 case Is_transparent of
					     true ->
						 O_qtree_acc0
						     ;
					     false ->
						 FVCs = ctree__get(FVIs, VC_tree),
						 Fzmax = face_zmax(FVCs),
						 FV2Cs = ctree__get2d(FVIs, VC_tree),
						 FBB = bbox(FV2Cs),
						 qtree__insert({{FVIs, FP}, FBB, Fzmax}, O_qtree_acc0)
					 end
				 end, qtree__new(bbox_2d(View_port)), Face_tree),


	    %% Line Group structure by Line Code
	    %% Edge dict + Style(Line width,color etc) = Line Group
	    %% [{ "Outline", %Line_Code
	    %%      [
	    %%       Width,
	    %%       { x0, y0 },
	    %%       { x1, y1 },
	    %%            :
	    %%            :
	    %%        { x  ,y },
	    %%        {R,G,B},
	    %%        "Pattern" ]

	    {Line_groups, Edges_total} =
		foldl(fun({Line_code, { Line_width ,Edges, Line_color,Line_pattern} }, {Dict_acc, Edge_count}) ->
			      {dict__append_list(Line_code, { Line_width, Edges, Line_color,Line_pattern } , Dict_acc),
			       Edge_count + length(Edges)}
		      end,
		      {dict__new(), 0 },
		      [{Line_code, {  Line_width,  Edges, Line_color, Line_pattern } } ||
			  {Edge_type1, Line_code, Line_width, Line_color, Line_pattern } <- [
											     {outline,  "Outline", get_pref(edge_width_outline,  ?DEF_EDGE_WIDTH_OUTLINE) , get_pref( outl_color, ?DEF_OUTL_COLOR), get_pref( outl_pattern, ?DEF_OUTL_PATTERN) },
											     {hard,     "Hadedge", get_pref(edge_width_hard,     ?DEF_EDGE_WIDTH_HARD)    , get_pref( hard_color, ?DEF_HARD_COLOR), get_pref( hard_pattern, ?DEF_HARD_PATTERN) },
											     {crease,   "Crease" , get_pref(edge_width_crease,   ?DEF_EDGE_WIDTH_CREASE)  , get_pref( crea_color, ?DEF_CREA_COLOR), get_pref( crea_pattern, ?DEF_CREA_PATTERN) },
											     {material, "Materia", get_pref(edge_width_material, ?DEF_EDGE_WIDTH_MATERIAL), get_pref( matl_color, ?DEF_MATL_COLOR), get_pref( matl_pattern, ?DEF_MATL_PATTERN) },
											     {regular,  "Regular", get_pref(edge_width_regular,  ?DEF_EDGE_WIDTH_REGULAR) , get_pref( regl_color, ?DEF_REGL_COLOR), get_pref( regl_pattern, ?DEF_REGL_PATTERN) },
											     {transp, "Trans_hide", get_pref(edge_width_lucent,  ?DEF_EDGE_WIDTH_LUCENT)  , get_pref( lcnt_color, ?DEF_LCNT_COLOR), get_pref( lcnt_pattern, ?DEF_LCNT_PATTERN) }

											    ],% Add Line Group code (Aboid the bug when same width size setting in dictonary)
			  {Edge_type2, Edges  } <- Edge_dict,
			  Edge_type1 =:= Edge_type2]),

	    %% io:format("LGP: ~p ~n",[Line_groups]),


	    Prog_step = get_max(Edges_total div 20, 250),



	    %%
	    %%  Header
	    %%


	    BB_size = {Wbb, Hbb},
	    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),

	    File_type_g = get_pref(file_type, ?DEF_FILE_TYPE),
	    {Write_header, Write_line_group, Write_polygons, Write_footer}
		= file_type_funs(File_type_g),
	    {ok, F} = file:open(File_name, [write]),
	    Write_header(F, BB_size, Line_cap,Mats_dict,LSpos),

	    Offset = divide(bbox_size(bbox_2d(View_port)), 2.0),



	    %%
	    %%  Face Draw
	    %%

	    case Draw_mode == both orelse  Draw_mode == face_only of
		true ->

		    %% Begin Draw Face

		    Write_polygons(F, BB_size,
				   [{transvc(bbox_2d(View_port), Offset, Wbb / Wvp,  ctree__get2d(get_fis(FI, FVIs, Face_tree), VC_tree1) ), FP,  Mat}
				    || {FI, {FVIs, FP, Mat}} <- bspt__to_list(Face_bspt)], Mats_dict,LSpos);

		%% End Draw Face

		false -> ok
	    end,

	    %%
	    %%  Edge Draw
	    %%

	    case Draw_mode == both orelse  Draw_mode == edge_only of
		true ->

		    case File_type_g of
			file_svg ->
			    Stype =  get_pref(svg_art_line_type, ?DEF_SVG_ART_LINE_TYPE),
			    io:put_chars(F,"\n<g id=\"edges\" inkscape:label=\"edges\" inkscape:groupmode=\"layer\" "++
					     add_svg_filter(line,Stype) ++
					     " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\">\n");
			_ -> ok
		    end,

		    %% Begin Draw Edge

		    foldl(fun({ Line_code, {Line_width ,Edges, Line_color,Line_pattern} }, {Group_count, Edge_count0}) ->
				  %% Line code is only acces dictionary
				  {Ls0, Edge_count} = foldl(fun(EVIt, {Ls_acc0, Edge_count_acc0}) ->
								    EVCt = vct(EVIt, VC_tree),
								    {Ls_acc, Edge_count_acc} =
									case clip_z(EVCt, View_port) of
									    nil ->
										{Ls_acc0, Edge_count_acc0}
										    ;
									    LVCt ->
										LV2Ct = Proj(LVCt),
										{Ls_acc0 ++ line_segment(EVIt, LVCt, LV2Ct,
													 qtree__get_objs(bbox(LV2Ct), O_qtree),
													 VC_tree, Proj, Is_ortho, Epseps),
										 Edge_count_acc0 + 1}
									end,
								    if
									Edge_count_acc rem Prog_step =:= 0 ->
									    Percent = Edge_count_acc / Edges_total,
									    wings_pb:update(Percent * 0.69 + 0.3,
											    integer_to_list(round(Percent * 100.0)) ++ "%"),
									    wings_pb:pause()
										;
									true ->
									    ok
								    end,
								    {Ls_acc, Edge_count_acc}
							    end, {[], Edge_count0}, Edges),
				  Ls1 = translate(bbox_2d(View_port), Offset, Wbb / Wvp, Ls0),
				  Ls = case get_pref(optimize, ?DEF_OPTIMIZE) of
					   true ->
					       Athr_deg = get_pref(coll_angle, ?DEF_COLL_ANGLE),
					       Athr = math:sin(Athr_deg * math:pi() / 180.0),
					       Dthr = get_pref(coll_dist, ?DEF_COLL_DIST),
					       lstree_to_list(lists:foldl(fun(L, Ls_acc) ->
										  lstree_insert(L, Ls_acc, Athr, Dthr)
									  end, lstree_empty(), Ls1))
						   ;
					   false ->
					       Ls1
				       end,
				  Write_line_group(F, BB_size, Ls, Line_code, Line_width, Line_color, Line_pattern, Group_count,Line_cap),
				  {Group_count + 1, Edge_count}
			  end, {0, 0}, Line_groups),

		    case File_type_g of
			file_svg ->  io:put_chars(F,"</g>\n");
			_ -> ok
		    end;

		%% End Draw Edge

		false -> ok
	    end,


	    %%
	    %% Footer
	    %%

	    Write_footer(F),

	    ok = file:close(F),
	    wings_pb:update(1.0, ?__(6,"done") ),
	    wings_pb:done(),
	    io:format(" "++ ?__(7,"done in ~.1f sec")++"~n", [timer:now_diff(os:timestamp(), Start_time) / 1.0e6])

	    %% since case Style_atalog
    end.



%%
%%  Make Edges
%%

edge_sets(Hard_edges, Faces, Edge_mode, Incr) when Edge_mode =:= all_edges ->
    {gb_sets:from_list(incr(Hard_edges, Incr)),
     gb_sets:from_list(edges(incr(Faces, Incr)))};
edge_sets(Hard_edges, _Faces, Edge_mode, Incr) when Edge_mode =:= hard_edges ->
    {gb_sets:from_list(incr(Hard_edges, Incr)), gb_sets:empty()};
edge_sets(_Hard_edges, _Faces, _Edge_mode, _Incr) ->
    {gb_sets:empty(), gb_sets:empty()}.


incr({A, B, C}, Incr) -> {A + Incr, B + Incr, C + Incr};
incr({A, B}, Incr) -> {A + Incr, B + Incr};
incr(#e3d_face{vs = VIs, mat = Mat}, Incr) ->
    #e3d_face{vs = incr(VIs, Incr), mat = Mat};
incr([], _Incr)  -> [];
incr([A | T], Incr) -> [incr(A, Incr) | incr(T, Incr)];
incr(A, Incr) -> A + Incr.

%%
%%  [9, 2, 5, 3] -> [{9, 2}, {2, 5}, {5, 3}, {3, 9}]
%%
pair(L) when is_list(L) -> pair(hd(L), L).
%pair(_, []) -> [];
pair(H, [E]) -> [{E, H}];
pair(H, [E | T]) -> [{E, hd(T)} | pair(H, T)].

normalize({P, Q}) when P > Q -> {Q, P};
normalize({P, Q})            -> {P, Q}.

%%
%%  [9, 2, 5, 3] -> [{2, 9}, {2, 5}, {3, 5}, {3, 9}]
%%
npair(L) when is_list(L) -> npair(hd(L), L).
%npair(_, []) -> [];
npair(H, [E]) -> [normalize({E, H})];
npair(H, [E | T]) -> [normalize({E, hd(T)}) | npair(H, T)].

cull([], _Frustum, Sum) -> Sum;
cull([FVC | T], Frustum, Sum) ->
    cull(T, Frustum, outcode(FVC, Frustum) band Sum).

cull([FVC | T], Frustum) ->
    cull(T, Frustum, outcode(FVC, Frustum)) =:= 0;
cull({LVC1, LVC2}, Frustum) ->
    outcode(LVC1, Frustum) band outcode(LVC2, Frustum) =:= 0;
cull({TVC1, TVC2, TVC3}, Frustum) ->
    outcode(TVC1, Frustum)
        band outcode(TVC2, Frustum)
        band outcode(TVC3, Frustum) =:= 0.

flip({N, D}) -> {neg(N), -D}.

is_open(#e3d_mesh{fs = Fs}) ->
    EFs_dict = foldl(fun(#e3d_face{vs = FVIs}, ME_acc) ->
			     foldl(fun(EVIt, FE_acc) ->
					   case dict:find(EVIt, FE_acc) of
					       {ok, Count} ->
						   dict:store(EVIt, Count + 1, FE_acc)
						       ;
					       error ->
						   dict:store(EVIt, 1, FE_acc)
					   end
				   end, ME_acc, npair(FVIs))
		     end, dict:new(), Fs),
    [] /= [C || {_, C} <- dict:to_list(EFs_dict), C < 2].

has_transp_faces(#e3d_mesh{fs = Fs}, Mats_dict) ->
    has_transp_faces(Fs, Mats_dict);
has_transp_faces([], _) -> false;
has_transp_faces([#e3d_face{mat = Mat} | T], Mats_dict) ->
    case hd(dict__fetch(hd(Mat), Mats_dict)) of
        {true, _RGBA} ->
            true
		;
        _ ->
            has_transp_faces(T, Mats_dict)
    end.

vct({TVI1, TVI2, TVI3}, VC_tree) ->
    {ctree__get(TVI1, VC_tree), ctree__get(TVI2, VC_tree), ctree__get(TVI3, VC_tree)};
vct({EVI1, EVI2}, VC_tree) -> {ctree__get(EVI1, VC_tree), ctree__get(EVI2, VC_tree)}.

face_zmax([] = _FVCs, Zmax) -> Zmax;
face_zmax([{_, _, Z} | T] = _FVCs, Zmax) when Z > Zmax -> face_zmax(T, Z);
face_zmax([_VC | T] = _FVCs, Zmax) -> face_zmax(T, Zmax).

face_zmax([{_, _, Z} | T]) -> face_zmax(T, Z).


face_zlim([] = _FVCs, Zlim) -> Zlim;
face_zlim([{_, _, Z} | T] = _FVCs, {Zmin, Zmax} = _Zlim) when Z > Zmax ->
    face_zlim(T, {Zmin, Z});
face_zlim([{_, _, Z} | T] = _FVCs, {Zmin, Zmax} = _Zlim) when Z < Zmin ->
    face_zlim(T, {Z, Zmax});
face_zlim([_VC | T] = _FVCs, Zlim) -> face_zlim(T, Zlim).

face_zlim([{_, _, Z} | T]) -> face_zlim(T, {Z, Z}).


face_type(_Is_FF, Is_SF) when Is_SF -> side;
face_type(Is_FF, _Is_SF) when Is_FF -> front;
face_type(_Is_FF, _Is_SF) -> back.

edges(#e3d_face{vs = FVIs}) -> npair(FVIs);
edges([]) -> [];
edges([F | T]) -> edges(F) ++ edges(T).

add_edges(FVIs, FVCs, Frustum, FN, Is_FF, Is_SF, Mat, Edge_dict0) ->
    foldl(fun({EVIt, EVCt}, Edge_dict_acc0) ->
		  case cull(EVCt, Frustum) of
		      true ->
			  Face_type = face_type(Is_FF, Is_SF),
			  dict:append(EVIt, {Face_type, FN, Mat}, Edge_dict_acc0)
			      ;
		      false ->
			  Edge_dict_acc0
		  end
	  end, Edge_dict0, lists:zip(npair(FVIs), npair(FVCs))).


add_poly(FVIs0, FP, Mat, Frustum, Frustum_planes, VC_tree0, Face_tree0) ->
    FVCs = ctree__get(FVIs0, VC_tree0),
    case inside(FVCs, Frustum) of
        true ->
            Face_tree = tree__insert({FVIs0, FP, Mat}, Face_tree0),
            {VC_tree0, Face_tree}
		;
        false ->
            case splitpoly__clip(Frustum_planes, FVIs0, VC_tree0) of
                {FVIs, VC_tree} ->
                    Face_tree = tree__insert({FVIs, FP, Mat}, Face_tree0),
                    {VC_tree, Face_tree}
			;
                _ ->
                    {VC_tree0, Face_tree0}
            end
    end.

add_face(FVIs, FVCs, {FN, _FD} = FP, Mat, Is_open, Frustum, Frustum_planes,
	 Front_face, Side_face,
	 VC_tree0, Edges_dict0, Face_tree0) ->
    Is_FF = Front_face(FP),
    case Is_open of
        true ->
            Is_SF = Side_face(FP),
            Edges_dict = add_edges(FVIs, FVCs, Frustum, FN,
				   Is_FF, Is_SF, Mat, Edges_dict0),
            {VC_tree, Face_tree} = if
				       Is_SF ->
					   {VC_tree0, Face_tree0}
					       ;
				       true ->
					   case Is_FF of
					       true ->
						   add_poly(FVIs, FP, Mat, Frustum, Frustum_planes,
							    VC_tree0, Face_tree0)
						       ;
					       false ->
						   add_poly(lists:reverse(FVIs), flip(FP), Mat,
							    Frustum, Frustum_planes, VC_tree0, Face_tree0)
					   end
				   end,
            {VC_tree, Edges_dict, Face_tree}
		;
        false ->
            case Is_FF of
                true ->
                    Edges_dict = add_edges(FVIs, FVCs, Frustum, FN,
					   Is_FF, false, Mat, Edges_dict0),
                    {VC_tree, Face_tree} = add_poly(FVIs, FP, Mat, Frustum,
						    Frustum_planes, VC_tree0, Face_tree0),
                    {VC_tree, Edges_dict, Face_tree}
			;
                false ->
                    {VC_tree0, Edges_dict0, Face_tree0}
            end
    end.

add_trimesh(TMFs, Is_open, Frustum, Frustum_planes, Front_face, Side_face,
	    VC_tree0, Edges_dict0, Face_tree0) ->
    foldl(fun(#e3d_face{vs = TVIs, mat = Mat},
	      {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0}) ->
		  TVCs = ctree__get(TVIs, VC_tree0),
		  case cull(TVCs, Frustum) of
		      true ->
			  TP = plane(list_to_tuple(TVCs)),
			  add_face(TVIs, TVCs, TP, Mat, Is_open,
				   Frustum, Frustum_planes, Front_face, Side_face,
				   VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0)
			      ;
		      false ->
			  {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0}
		  end
	  end, {VC_tree0, Edges_dict0, Face_tree0}, TMFs).

add_mesh(MFs, VCs, _Eps, % need VCs for e3d_mesh:triangulate_face
	 Is_open, Frustum, Frustum_planes, Front_face, Side_face,
	 VC_tree0, Edges_dict0, Face_tree0) ->

    DP = get_pref(division_priority, ?DEF_DIVISION_PRIORITY),
    DP_factor =  case DP of scene -> 1; path -> -1; _ -> 1 end,

    Z_dist_offset =  get_pref(z_dist_offset, ?DEF_Z_DIST_OFFSET),

    foldl(fun(#e3d_face{vs=FVIs, mat=Mat}=Face,
	      {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0}) ->

		  FVCs = ctree__get(FVIs, VC_tree0),
		  case cull(FVCs, Frustum) of
		      true ->

			  %% need a nice looking triangle (slow):
			  TFs = e3d_mesh:triangulate_face(Face, VCs),

			  #e3d_face{vs=T0VIs} = hd(TFs),

			  T0VCs = ctree__get(T0VIs, VC_tree0),

			  FP = plane(list_to_tuple(T0VCs)),
			  FV2Cs = ctree__get2d(FVIs, VC_tree0),
			  case length(FVIs) =:= 3 orelse
			      is_convex(FV2Cs, Front_face(FP)) andalso %% Front_face shouldn't be called from here
			      is_flat(FP, FVCs) of
			      true ->
				  {FN, FD} = FP ,{_X,Y,_Z}= FN,
				  Z_offset = if  Y< 0.1;Y < -0.1 -> -Z_dist_offset*0.028;true-> Z_dist_offset end, % same  position by eye level then bug?

				  FP0 ={ FN, FD-FD*0.001*(Z_offset*DP_factor) }, % Overlap faces offset : Overlap factor of Front and Back polygon fighting (push -  2   + pull  )
				  add_face(FVIs, FVCs, FP0, Mat, Is_open,
					   Frustum, Frustum_planes, Front_face, Side_face,
					   VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0)
				      ;
			      false ->
				  lists:foldl(fun(#e3d_face{vs=TVIs},
						  {VC_tree_acc1, Edge_dict_acc1, Face_tree_acc1} ) ->
						      %%  'Edge_dict_acc0' shadowed in 'fun' if this to fix ,then  triangle polygon not display(For example: Torus knot)
						      {FN, FD} = FP ,
						      FP0 ={ FN, FD+FD*0.001*(Z_dist_offset*DP_factor) }, %  Overlap faces offset : experiment  mul(FN, 0.001*Z_dist_offset)
						      add_face(TVIs, ctree__get(TVIs, VC_tree0), %%%%% ctree_get in args
							       FP0, Mat, Is_open,
							       Frustum, Frustum_planes, Front_face, Side_face,
							       VC_tree_acc1, Edge_dict_acc1, Face_tree_acc1)
					      end,
					      {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0}, TFs)
			  end

			      ;
		      false ->
			  {VC_tree_acc0, Edge_dict_acc0, Face_tree_acc0}
		  end
	  end, {VC_tree0, Edges_dict0, Face_tree0}, MFs).



materials(Mats) when is_list(Mats) ->
    materials(Mats, []).


materials([], Mats_dict) -> Mats_dict;
materials([{Name, Props} | T], Mats_dict) ->
    OpenGL = proplists:get_value(opengl, Props),
    Is_transparent = foldl(fun(_, true) -> true;
			      ({emission,_}, _) -> false;
			      ({_,{_,_,_,1.0}}, _) -> false;
			      ({_,{_,_,_,_}}, _) -> true;
			      (_, _) -> false
			   end, false, OpenGL),
    Color = proplists:get_value(diffuse, OpenGL),
    materials(T, dict__store(Name, {Is_transparent, Color}, Mats_dict)).


is_visible(AFs) when length(AFs) < 2 -> true;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
  when FT1 =:= side, FT2 =:= side -> false;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
  when FT1 =:= side, FT2 =:= back -> false;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
  when FT1 =:= back, FT2 =:= side -> false;
is_visible(_AFs) -> true.

is_outline(AFs) when length(AFs) < 2 -> true;
is_outline([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
  when (FT1 =:= front) xor (FT2 =:= front) -> true;
is_outline(_AFs) -> false.

is_crease(_EVIt, AFs, _ThreshCosA) when length(AFs) < 2 -> true;
is_crease(_EVIt, [{_FT1, TN1, _Mat1}, {_FT2, TN2, _Mat2}], ThreshCosA) ->
    dot(TN1, TN2) =< ThreshCosA;
is_crease(_, _, _) -> false.

is_material(AFs) when length(AFs) < 2 -> true;
is_material([{_FT1, _TN1, Mat1}, {_FT2, _TN2, Mat2}])
  when Mat1 /= Mat2 -> true;
is_material(_) -> false.


%% Mat1 == Mat2  is edge, Mat /= Mat2 not edge
%% EVIt ga naito dame
is_material_transp(_EVIt,AFs,_ThreshCosA, _Mats_dict) when length(AFs) < 2 -> true;
is_material_transp(_EVIt,[{FT1, TN1, Mat1}, {FT2, TN2, Mat2}] ,ThreshCosA, Mats_dict)
  when (FT1 =:= back) and (FT2 =:= back);  Mat1 /= Mat2  ->
    Trans = is_transparent(Mat1,Mats_dict),
    %% io:format("MT ~p ~n",[Trans]),
    %% io:format("D  ~p . ~p ~p ~p . ~p ~p ~p ~n", [EVIt,   FT1, TN1, Mat1, FT2, TN2, Mat2 ] ),
    case (dot(TN1, TN2) =< ThreshCosA) of
	true ->  Trans; false -> false end;
is_material_transp(_,_,_,_) -> false.


%%
%% From src/wings_material.erl
%%
is_transparent(Mat_name ,Mats_dict) ->
    Data =  case  hd(dict__fetch( hd(Mat_name), Mats_dict))  of
		{false, _RGBA} ->
		    false
			;
		_ ->
		    true
	    end,
    Data.



%% Non hard edge draw (for crease  material outline )

edge_type_group(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set,Mats_dict) ->
    case is_visible(AFs) of
        true ->
            case is_outline(AFs) of
                true ->
                    outline
			;
                false ->
		    case gb_sets:is_member(EVIt, HardEdge_set) of
			true ->
			    hard
				;
			false ->
			    case is_material_transp(EVIt,AFs, ThreshCosA, Mats_dict) of
				true ->
				    transp
					;
				false ->
				    case is_crease(EVIt, AFs, ThreshCosA) of
					true ->
					    crease
						;
					false ->
					    case is_material(AFs) of
						true ->
						    material
							;
						false ->
						    case gb_sets:is_member(EVIt, Edge_set) of
							true ->
							    regular
								;
							false ->
							    none
						    end
					    end
				    end
			    end
		    end
            end
		;
        false ->
            none
    end.

%% Regular = All edge draw by same style

edge_type_copy(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set,Mats_dict) ->
    case is_visible(AFs)
        andalso (is_outline(AFs)
		 orelse is_material(AFs)
		 orelse is_crease(EVIt, AFs, ThreshCosA)
		 orelse is_material_transp(EVIt,AFs,ThreshCosA,Mats_dict)
		 orelse gb_sets:is_member(EVIt, HardEdge_set)
		 orelse gb_sets:is_member(EVIt, Edge_set))
    of
        true ->
            regular
		;
        false ->
            none
    end.

group_edges(ThreshCosA, HardEdge_set, Edge_set, Edge_type_fun,
	    Edge_dict, Edges_dict_acc0,Mats_dict) ->
    dict:fold(fun(EVIt, AFs, Es_dict_acc0) ->
		      case Edge_type_fun(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set,Mats_dict) of
			  none ->
			      Es_dict_acc0
				  ;
			  Edge_type ->
			      dict__append(Edge_type, EVIt, Es_dict_acc0)
		      end
	      end, Edges_dict_acc0, Edge_dict).

%%
%% Utilities
%%


get_min(A, B) when A < B -> A;
get_min(_A, B) -> B.

get_max(A, B) when A > B -> A;
get_max(_A, B) -> B.

add({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 + X2, Y1 + Y2, Z1 + Z2};
add({X1, Y1}, {X2, Y2}) -> {X1 + X2, Y1 + Y2}.

sub({X1, Y1, Z1}, {X2, Y2, Z2}) -> {X1 - X2, Y1 - Y2, Z1 - Z2};
sub({X1, Y1}, {X2, Y2}) -> {X1 - X2, Y1 - Y2}.

neg({X, Y, Z}) -> {-X, -Y, -Z};
neg({X, Y}) -> {-X, -Y}.

mul({X, Y, Z}, S) -> {X * S, Y * S, Z * S};
mul({X, Y}, S) -> {X * S, Y * S}.

%divide({X, Y, Z}, S) -> {X / S, Y / S, Z / S};
divide({X, Y}, S) -> {X / S, Y / S}.

cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {Y1 * Z2 - Y2 * Z1, X2 * Z1 - X1 * Z2, X1 * Y2 - X2 * Y1};
cross({X1, Y1}, {X2, Y2}) -> X1 * Y2 - X2 * Y1.

dot({X1, Y1, Z1}, {X2, Y2, Z2}) -> X1 * X2 + Y1 * Y2 + Z1 * Z2;
dot({X1, Y1}, {X2, Y2}) -> X1 * X2 + Y1 * Y2.

area(V1, V2, V3) -> cross(sub(V2, V1), sub(V3, V1)).

delta({X1, Y1, Z1} = LVC1, {X2, Y2, Z2}) ->
    {LVC1, {X2 - X1, Y2 - Y1, Z2 - Z1}};
delta({X1, Y1} = LVC1, {X2, Y2}) -> {LVC1, {X2 - X1, Y2 - Y1}}.

delta({{X1, Y1, Z1} = LVC1, {X2, Y2, Z2}}) ->
    {LVC1, {X2 - X1, Y2 - Y1, Z2 - Z1}};
delta({{X1, Y1} = LVC1, {X2, Y2}}) -> {LVC1, {X2 - X1, Y2 - Y1}}.

param({LVC1, DL}, U) -> add(LVC1, mul(DL, U)).

section({LVC1, DL}, {TN, TD}) ->
    case catch (TD - dot(TN, LVC1)) / dot(TN, DL) of
        R when is_float(R)-> R;
        _ -> ?BIG
    end.

ip(Delta, P) -> param(Delta, section(Delta, P)).

ip(VC1, VC2, P) -> ip(delta(VC1, VC2), P).

plane({VC1, VC2, VC3}) ->
    N = e3d_vec:normal(VC1, VC2, VC3),
    {N, dot(N, VC1)}.

persp_ff({_, D}) -> D < -0.001.

ortho_ff({{_, _, Z}, _}) -> Z > 0.001.

persp_sf({_, D}) -> abs(D) =< 0.001.

ortho_sf({{_, _, Z}, _}) -> abs(Z) =< 0.001.

%%
%% Cohen-Sutherland
%%
outcode({X, Y, Z}, {{Xmin, Ymin, Zmin}, {Xmax, Ymax, Zmax}}) ->
    C0 = if X < Xmin -> 1; true -> 0 end,
    C1 = if X > Xmax -> 2; true -> 0 end,
    C2 = if Y < Ymin -> 4; true -> 0 end,
    C3 = if Y > Ymax -> 8; true -> 0 end,
    C4 = if Z < Zmin -> 16; true -> 0 end,
    C5 = if Z > Zmax -> 32; true -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5;
outcode({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    C0 = if X < Xmin -> 1; true -> 0 end,
    C1 = if X > Xmax -> 2; true -> 0 end,
    C2 = if Y < Ymin -> 4; true -> 0 end,
    C3 = if Y > Ymax -> 8; true -> 0 end,
    C0 bor C1 bor C2 bor C3;
outcode({X, Y, Z}, {HSx, HSy, Zmin, Zmax, Zf}) ->
    R =  Z / Zf,
    Rx = HSx * R,
    Ry = HSy * R,
    {C0, C1, C2, C3} = if
			   Z > 0.0 ->
			       {if X < -Rx -> 0; true -> 1 end,
				if X > Rx -> 0; true -> 2 end,
				if Y < -Ry -> 0; true -> 4 end,
				if Y > Ry -> 0; true -> 8 end}
				   ;
			   true ->
			       {if X < -Rx -> 1; true -> 0 end,
				if X > Rx -> 2; true -> 0 end,
				if Y < -Ry -> 4; true -> 0 end,
				if Y > Ry -> 8; true -> 0 end}
		       end,
    C4 = if Z < Zmin -> 16; true -> 0 end,
    C5 = if Z > Zmax -> 32; true -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5.

%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {{Xmin, _, _}, _}) when X < Xmin ->
%%     {Xmin, Y + DY * (Xmin - X) / DX, Z + DZ * (Xmin - X) / DX};
%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {Xmax, _, _}}) when X > Xmax ->
%%     {Xmax, Y + DY * (Xmax - X) / DX, Z + DZ * (Xmax - X) / DX};
%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {{_, Ymin, _}, _}) when Y < Ymin ->
%%     {X + DX * (Ymin - Y) / DY, Ymin, Z + DZ * (Ymin - Y) / DY};
%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {_, Ymax, _}}) when Y > Ymax ->
%%     {X + DX * (Ymax - Y) / DY, Ymax, Z + DZ * (Ymax - Y) / DY};
%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {{_, _, Zmin}, _}) when Z < Zmin ->
%%     {X + DX * (Zmin - Z) / DZ, Y + DY * (Zmin - Z) / DZ, Zmin};
%% to_boundary({X, Y, Z}, {DX, DY, DZ}, {_, {_, _, Zmax}}) when Z > Zmax ->
%%     {X + DX * (Zmax - Z) / DZ, Y + DY * (Zmax - Z) / DZ, Zmax};
to_boundary({X, Y, Z}, {DX, DY, DZ}, Zlim) when is_float(Zlim) ->
    {X + DX * (Zlim - Z) / DZ, Y + DY * (Zlim - Z) / DZ, Zlim};
to_boundary({X, Y}, {DX, DY}, {{Xmin, _}, _}) when X < Xmin ->
    {Xmin, Y + DY * (Xmin - X) / DX};
to_boundary({X, Y}, {DX, DY}, {_, {Xmax, _}}) when X > Xmax ->
    {Xmax, Y + DY * (Xmax - X) / DX};
to_boundary({X, Y}, {DX, DY}, {{_, Ymin}, _}) when Y < Ymin ->
    {X + DX * (Ymin - Y) / DY, Ymin};
to_boundary({X, Y}, {DX, DY}, {_, {_, Ymax}}) when Y > Ymax ->
    {X + DX * (Ymax - Y) / DY, Ymax}.

clip({LC1, LC2}, Box) ->
    C1 = outcode(LC1, Box),
    C2 = outcode(LC2, Box),
    if
        C1 band C2 /= 0 ->
            nil
		;
        C1 bor C2 /= 0 ->
            D = sub(LC2, LC1),
            case C1 /= 0 of
                true  ->
                    clip({to_boundary(LC1, D, Box), LC2}, Box)
			;
                false ->
                    clip({LC1, to_boundary(LC2, D, Box)}, Box)
            end
		;
        true ->
            {LC1, LC2}
    end.

clip_z({LC1, LC2}, View_port) when size(View_port) =:= 2 ->
    {{_, _, Z1}, {_, _, Z2}} = {LC1, LC2},
    {{_, _, Zvp_min}, {_, _, Zvp_max}} = View_port,
    if
        Z1 >= Zvp_min, Z1 =< Zvp_max, Z2 >= Zvp_min, Z2 =< Zvp_max ->
            {LC1, LC2}
		;
        ((Z1 < Zvp_min) and (Z2 < Zvp_min))
	or ((Z1 > Zvp_max) and (Z2 > Zvp_max)) ->
            nil
		;
        true ->
            {if
		 Z1 < Zvp_min ->
		     to_boundary(LC1, sub(LC2, LC1), Zvp_min)
			 ;
		 Z1 > Zvp_max ->
		     to_boundary(LC1, sub(LC2, LC1), Zvp_max)
			 ;
		 true ->
		     LC1
	     end,
	     if
		 Z2 < Zvp_min ->
		     to_boundary(LC2, sub(LC2, LC1), Zvp_min)
			 ;
		 Z2 > Zvp_max ->
		     to_boundary(LC2, sub(LC2, LC1), Zvp_max)
			 ;
		 true ->
		     LC2
	     end}
    end.


outside_bb({{Xmin1, Ymin1}, {Xmax1, Ymax1}},
	   {{Xmin2, Ymin2}, {Xmax2, Ymax2}}) ->
    Xmax1 =< Xmin2 orelse Xmax2 =< Xmin1
        orelse Ymax1 =< Ymin2 orelse Ymax2 =< Ymin1.


%%% Should it be a BB test?
nearer({{_, _, LZ1}, {_, _, LZ2}}, FVCs) when is_list(FVCs) ->
    LZ_min = get_min(LZ1, LZ2),
    lists:all(fun({_, _, FZ} = _FVC) -> LZ_min >= FZ end, FVCs).


edge_of_poly(LVIt, FVIs) ->
%%% LVIt must be normalized
    lists:any(fun(FVIt) -> FVIt =:= LVIt end, npair(FVIs)).


outside_poly({LVC1, LVC2}, FVCs, Eps) ->
    lists:any(fun({EVC1, EVC2}) ->
		      (area(LVC1, EVC1, EVC2) < Eps) andalso (area(LVC2, EVC1, EVC2) < Eps)
	      end, pair(FVCs)).


inside_poly(C, FVCs, Eps) ->
    lists:all(fun({EVC1, EVC2}) -> area(C, EVC2, EVC1) < Eps end, pair(FVCs)).


outside_line(FVCs, {LVC1, LVC2}, Eps) ->
%%% No need to calculate all areas, the last three should be enough
%%% to determine if the poly is a tangent
    As = lists:map(fun(FVC) -> area(FVC, LVC1, LVC2) end, FVCs),
    lists:all(fun(A) -> A < Eps end, As)
        orelse lists:all(fun(A) -> A > -Eps end, As).

line_segment(_LVIt, nil, _Fs, _VC_tree,
	     _Proj, _Is_ortho, _Eps) -> [];
line_segment(LVIt, LVCt, Fs, VC_tree, Proj, Is_ortho, Eps) ->
    line_segment(LVIt, LVCt, Proj(LVCt), Fs, VC_tree,
		 Proj, Is_ortho, Eps).

line_segment(_, _, LV2Ct, [], _, _, _, _) -> [LV2Ct];
line_segment(LVIt, {LVC1, LVC2} = LVCt, {LV2C1, LV2C2} = LV2Ct,
	     [{{FVIs, FP}, FBB} | T], VC_tree, Proj, Is_ortho, Eps) ->
    FVCs = ctree__get(FVIs, VC_tree),
    FV2Cs = ctree__get2d(FVIs, VC_tree),
    case

        outside_bb(bbox(LV2Ct), FBB)
	orelse edge_of_poly(LVIt, FVCs)
	orelse nearer(LVCt, FVCs)
    of
        true  ->
            line_segment(LVIt, LVCt, LV2Ct, T, VC_tree,
			 Proj, Is_ortho, Eps)
		;
        false ->
            {FN, FD} = FP,
            L1D = dot(LVC1, FN),
            L2D = dot(LVC2, FN),
            EpsD = ?EPS1 * abs(FD),
            FDm = FD - EpsD,
            case
                (L1D >= FDm) andalso (L2D >= FDm)
		orelse outside_poly(LV2Ct, FV2Cs, Eps)
		orelse outside_line(FV2Cs, LV2Ct, Eps)
            of
                true  ->
                    line_segment(LVIt, LVCt, LV2Ct, T, VC_tree,
				 Proj, Is_ortho, Eps)
			;
                false ->
                    FDp = FD + EpsD,
                    L1_behind = L1D =< FDp,
                    L2_behind = L2D =< FDp,
                    if
                        L1_behind; L2_behind ->
                            L1_in = inside_poly(LV2C1, FV2Cs, Eps),
                            L2_in = inside_poly(LV2C2, FV2Cs, Eps),
                            if
                                L1_behind, L2_behind, L1_in, L2_in ->
                                    []
					;
                                true ->
                                    case
                                        divide_segment(LVCt, LV2Ct,
						       FV2Cs, FP, FDp,
						       L1_behind, L2_behind,
						       L1_in, L2_in, Is_ortho)
                                    of
                                        {LA, LA} ->
                                            line_segment(LVIt, LA, T,
							 VC_tree,
							 Proj, Is_ortho, Eps)
						;
                                        {LA, LB} ->
                                            line_segment(LVIt, LA, T,
							 VC_tree,
							 Proj, Is_ortho, Eps)
                                                ++ line_segment(LVIt, LB, T,
								VC_tree,
								Proj, Is_ortho, Eps)
                                    end
                            end
				;
                        true ->
                            line_segment(LVIt, LVCt, LV2Ct, T,
					 VC_tree,
					 Proj, Is_ortho, Eps)
                    end
            end
    end.

divide_segment({{_X1, _Y1, Z1} = LVC1, {_X2, _Y2, Z2} = LVC2} = LVCt,
	       LV2Ct, FV2Cs, {FN, _FD} = FP, FDp,
	       L1_behind, L2_behind, L1_in, L2_in, Is_ortho) when L1_in /= L2_in ->
    Del = delta(LVCt),
    Del2 = delta(LV2Ct),
    {_, S} = find_section(true, Del2, FV2Cs),
    L = param(Del, if
		       Is_ortho -> S;
		       true -> R = Z1 * S / Z2, R / (1.0 + R - S)
		   end),
    L_behind = dot(L, FN) =< FDp,
    if
        L_behind ->
            {if
		 L2_in, S > 0.0 + ?EPS2 ->
		     {LVC1, L}
			 ;
		 true ->
		     if
			 L1_behind -> nil;
			 true -> {LVC1, ip(Del, FP)}
		     end
	     end,
	     if
		 L1_in, S < 1.0 - ?EPS2 ->
		     {L, LVC2}
			 ;
		 true ->
		     if
			 L2_behind -> nil;
			 true -> {ip(Del, FP), LVC2}
		     end
	     end}
		;
        true ->
            {if
		 L2_in, S > 0.0 + ?EPS2 ->
		     if
			 L2_behind -> {LVC1, ip(Del, FP)};
			 true -> {LVC1, LVC2}
		     end
			 ;
		 true ->
		     nil
	     end,
	     if
		 L1_in, S < 1.0 - ?EPS2 ->
		     if
			 L1_behind -> {ip(Del, FP), LVC2};
			 true -> {LVC1, LVC2}
		     end
			 ;
		 true ->
		     nil
	     end}
    end;
divide_segment({LVC1, LVC2} = LVCt, _LV2Ct,
	       _FV2Cs, FP, _FDp,
	       L1_behind, L2_behind, L1_in, L2_in, _Is_ortho) when L1_in, L2_in ->
    Del = delta(LVCt),
    {if
	 L1_behind -> nil;
	 true -> {LVC1, ip(Del, FP)}
     end,
     if
	 L2_behind -> nil;
	 true -> {ip(Del, FP), LVC2}
     end};
divide_segment({{_X1, _Y1, Z1} = LVC1, {_X2, _Y2, Z2} = LVC2} = LVCt,
	       LV2Ct, FV2Cs, {FN, _FD} = FP, FDp,
	       _L1_behind, _L2_behind, _L1_in, _L2_in, Is_ortho) ->
    Del = delta(LVCt),
    Del2 = delta(LV2Ct),
    {Smin, Smax} = find_section(false, Del2, FV2Cs),
    {Lmin, Lmax} = if
		       Is_ortho ->
			   {param(Del, Smin), param(Del, Smax)}
			       ;
		       true ->
			   Rz = Z1 / Z2,
			   Rmin = Rz * Smin,
			   Rmax = Rz * Smax,
			   {param(Del, Rmin / (1.0 + Rmin - Smin)),
			    param(Del, Rmax / (1.0 + Rmax - Smax))}
		   end,
    {Lmin_behind, Lmax_behind} = {dot(Lmin, FN) =< FDp, dot(Lmax, FN) =< FDp},
    if
        Lmin_behind; Lmax_behind ->
            {if
		 Lmin_behind ->
		     if
			 Smin > 0.0 + ?EPS2 -> {LVC1, Lmin};
			 true -> nil
		     end
			 ;
		 true ->
		     {LVC1, ip(Del, FP)}
	     end,
	     if
		 Lmax_behind ->
		     if
			 Smax < 1.0 - ?EPS2 -> {Lmax, LVC2};
			 true -> nil
		     end
			 ;
		 true ->
		     {ip(Del, FP), LVC2}
	     end}
		;
        true ->
            {LVCt, LVCt}
    end.

find_section(Find_one, Del, FV2Cs) ->
    find_section(1.0, 0.0, Find_one, Del, pair(FV2Cs)).

find_section(Smin, Smax, _, _, []) -> {Smin, Smax};
find_section(Smin, Smax, Find_one, {LVC1, DL} = Del, [{EVC1, EVC2} | T]) ->
    D = sub(LVC1, EVC1),
    DE = sub(EVC2, EVC1),
    A = cross(DE, DL),
    case catch cross(D, DL) / A of
        R when is_float(R), R > 0.0 - ?EPS2, R < 1.0 + ?EPS2 ->
            case cross(D, DE) / A of
                S when S > 0.0 - ?EPS2, S < 1.0 + ?EPS2 ->
                    if
                        Find_one ->
                            if
                                S > 0.0 + ?EPS2, S < 1.0 - ?EPS2 ->
                                    {S, S}
					;
                                true ->
                                    find_section(get_min(S, Smin), get_max(S, Smax),
						 Find_one, Del, T)
                            end
				;
                        true ->
                            find_section(get_min(S, Smin), get_max(S, Smax),
					 Find_one, Del, T)
                    end
			;
                _ ->
                    find_section(Smin, Smax, Find_one, Del, T)
            end
		;
        _ ->
            find_section(Smin, Smax, Find_one, Del, T)
    end.



%%
%% See http://www.tulrich.com/geekstuff/partitioning.html
%%

qtree__new(Box) when size(Box) =:= 2 -> qtree__new(qtree__cons(Box));
qtree__new(Cons) -> {[], Cons, nil, nil, nil, nil}.

qtree__insert({_Obj, OBB, _Ozmax} = QE, Q) ->
    qtree__insert(QE, mid(OBB), Q, 0).

qtree__insert(false, _QE, _QEmid, _Cons, Q, _Depth) -> Q;
qtree__insert(true, QE, QEmid, Cons, nil, Depth) ->
    qtree__insert(QE, QEmid, qtree__new(Cons), Depth + 1);
qtree__insert(true, QE, QEmid, _Cons, Q, Depth) ->
    qtree__insert(QE, QEmid, Q, Depth + 1).

qtree__insert(QE, QEmid, {QEs, Cons, Q1, Q2, Q3, Q4}, Depth) ->
    C1 = qtree__cons(if Q1 =:= nil -> qtree__quad1(Cons); true -> Q1 end),
    C2 = qtree__cons(if Q2 =:= nil -> qtree__quad2(Cons); true -> Q2 end),
    C3 = qtree__cons(if Q3 =:= nil -> qtree__quad3(Cons); true -> Q3 end),
    C4 = qtree__cons(if Q4 =:= nil -> qtree__quad4(Cons); true -> Q4 end),
    F1 = qtree__fits(QE, QEmid, C1),
    F2 = qtree__fits(QE, QEmid, C2),
    F3 = qtree__fits(QE, QEmid, C3),
    F4 = qtree__fits(QE, QEmid, C4),
						%io:format("~n"),
						%io:format("QE=~w~n", [QE]),
						%io:format("QEmid=~w~n", [QEmid]),
						%io:format("F1=~w F2=~w F3=~w F4=~w~n", [F1, F2, F3, F4]),
						%io:format("C1=~w C2=~w C3=~w C4=~w~n", [C1, C2, C3, C4]),
    if
        Depth > ?MAXDEPTH - 1; not (F1 or F2 or F3 or F4) ->
            {[QE | QEs], Cons, Q1, Q2, Q3, Q4}
		;
        true ->
            N1 = qtree__insert(F1, QE, QEmid, C1, Q1, Depth),
            N2 = qtree__insert(F2, QE, QEmid, C2, Q2, Depth),
            N3 = qtree__insert(F3, QE, QEmid, C3, Q3, Depth),
            N4 = qtree__insert(F4, QE, QEmid, C4, Q4, Depth),
            {QEs, Cons, N1, N2, N3, N4}
    end.

qtree__cons({{Xmin, Ymin}, {Xmax, Ymax}}) ->
    Dx = (Xmax - Xmin) / 2.0,
    Dy = (Ymax - Ymin) / 2.0,
    {{{Xmin, Ymin}, {Xmax, Ymax}},
     {{Xmin - Dx, Ymin - Dy}, {Xmax + Dx, Ymax + Dy}},
     {Xmin + Dx, Ymin + Dy}};
qtree__cons({_QEs, Cons, _Q1, _Q2, _Q3, _Q4}) -> Cons.

qtree__quad1({{_, {Xmax, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymid}, {Xmax, Ymax}}.
qtree__quad2({{{Xmin, _}, {_, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymid}, {Xmid, Ymax}}.
qtree__quad3({{{Xmin, Ymin}, _}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymin}, {Xmid, Ymid}}.
qtree__quad4({{{_, Ymin}, {Xmax, _}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymin}, {Xmax, Ymid}}.

qtree__fits({_Obj, {OBBmin, OBBmax}, _Ozmax}, QEmid, {CBB, Clim, _Cmid}) ->
    inside(OBBmin, Clim) andalso inside(OBBmax, Clim)
        andalso inside(QEmid, CBB).

qtree__get_quad_objs(_LBB, []) -> [];
qtree__get_quad_objs(LBB, [{_Obj, OBB, _Ozmax} = QE | T]) ->
    case outside_bb(LBB, OBB) of
        true ->
            qtree__get_quad_objs(LBB, T)
		;
        false ->
            [QE | qtree__get_quad_objs(LBB, T)]
    end.

qtree__do_get_objs(_LBB,  nil) -> [];
qtree__do_get_objs(LBB, {QEs, {_CBB, Clim, _Cmid}, Q1, Q2, Q3, Q4}) ->
    case outside_bb(LBB, Clim) of
        true ->
            []
		;
        false ->
            qtree__get_quad_objs(LBB, QEs)
                ++ qtree__do_get_objs(LBB, Q1)
                ++ qtree__do_get_objs(LBB, Q2)
                ++ qtree__do_get_objs(LBB, Q3)
                ++ qtree__do_get_objs(LBB, Q4)
    end.

qtree__get_objs(LS, Q) ->
    [{Obj, OBB} || {Obj, OBB, _} <- keysort(3, qtree__do_get_objs(LS, Q))].

inside([], _Frustum, Flag) -> Flag;
inside([FVC | T], Frustum, true) ->
    inside(T, Frustum, inside(FVC, Frustum));
inside(_FVCs, _Frustum, Flag) -> Flag.

inside([FVC | T], Frustum) ->
    inside(T, Frustum, inside(FVC, Frustum));
inside({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    (X >= Xmin) andalso (Y >= Ymin) andalso (X =< Xmax) andalso (Y =< Ymax);
inside({X, Y, Z}, {{Xmin, Ymin, Zmin}, {Xmax, Ymax, Zmax}}) ->
    (X >= Xmin) andalso (Y >= Ymin) andalso (Z >= Zmin)
        andalso (X =< Xmax) andalso (Y =< Ymax) andalso (Z =< Zmax);
inside({X, Y, Z}, {HSx, HSy, Zmin, Zmax, Zf}) ->
    R =  Z / Zf,
    Rx = HSx * R,
    Ry = HSy * R,
    (if
	 Z >= 0.0 ->
	     (X >= Rx) andalso (Y >= Ry) andalso (X =< -Rx) andalso (Y =< -Ry)
		 ;
	 true ->
	     (X >= -Rx) andalso (Y >= -Ry) andalso (X =< Rx) andalso (Y =< Ry)
     end) andalso (Z >= Zmin) andalso (Z =< Zmax).

mid({{Xmin, Ymin}, {Xmax, Ymax}}) -> {(Xmin + Xmax) / 2.0, (Ymin + Ymax) / 2.0}.

bbox_size({{Xmin, Ymin}, {Xmax, Ymax}}) -> {Xmax - Xmin, Ymax - Ymin}.

bbox([VC1, VC2]) -> bbox({VC1, VC2});
bbox([VC1, VC2 | T]) -> bbox(T, bbox({VC1, VC2}));
bbox({{X1, Y1}, {X2, Y2}}) ->
    Xmin = get_min(X1, X2),
    Ymin = get_min(Y1, Y2),
    Xmax = get_max(X1, X2),
    Ymax = get_max(Y1, Y2),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox([VC], BB) -> bbox(VC, BB);
bbox([VC | T], BB) -> bbox(T, bbox(VC, BB));
bbox({{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}},
     {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}}) ->
    Xmin = get_min(Xmin_1, Xmin_2),
    Ymin = get_min(Ymin_1, Ymin_2),
    Xmax = get_max(Xmax_1, Xmax_2),
    Ymax = get_max(Ymax_1, Ymax_2),
    {{Xmin, Ymin}, {Xmax, Ymax}};
bbox({X, Y}, {{Xmin0, Ymin0}, {Xmax0, Ymax0}}) ->
    Xmin = get_min(X, Xmin0),
    Ymin = get_min(Y, Ymin0),
    Xmax = get_max(X, Xmax0),
    Ymax = get_max(Y, Ymax0),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox_2d({{Xmin, Ymin, _}, {Xmax, Ymax, _}}) -> {{Xmin, Ymin}, {Xmax, Ymax}}.


translate(_, _, _, []) -> [];
translate(BB, Offset, Scale, [LVCt0 | T]) ->
    case clip(LVCt0, BB) of
        nil -> translate(BB, Offset, Scale, T);
        {LVC1, LVC2} ->
            [{mul(add(LVC1, Offset), Scale), mul(add(LVC2, Offset), Scale)}
	     | translate(BB, Offset, Scale, T)]
    end.


transvc(_, _, _, []) -> [];
transvc(BB, Offset, Scale, [VCt | T]) ->
    [mul(add(VCt, Offset), Scale) | transvc(BB, Offset, Scale, T)].

%% A function for debug
%% project(_, []) -> [];
%% project(Proj, [C | T]) -> [Proj(C) | project(Proj, T)].


merge(LS1, _BB1, [], _Dthr) -> [LS1];
merge(LS1, {{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}} = BB1, [LS2 | T], Dthr) ->
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}} = BB2 = bbox(LS2),
    {{Xmin, Ymin}, {Xmax, Ymax}} = BB = bbox(BB1, BB2),
    case
        (get_max(Xmin_1, Xmin_2) =< get_min(Xmax_1, Xmax_2) + Dthr)
	andalso (get_max(Ymin_1, Ymin_2) =< get_min(Ymax_1, Ymax_2) + Dthr)
    of
        true ->
            {LS11, _LS12} = LS1,
            LS = if
		     LS11 =:= {Xmin_1, Ymin_1}; LS11 =:= {Xmax_1, Ymax_1} ->
			 {{Xmin, Ymin}, {Xmax, Ymax}}
			     ;
		     true ->
			 {{Xmin, Ymax}, {Xmax, Ymin}}
		 end,
            merge(LS, BB, T, Dthr)
		;
        false ->
            [LS2 | merge(LS1, BB1, T, Dthr)]
    end.

merge(LS, [], _Dthr) -> [LS];
merge(LS, LSs, Dthr) -> merge(LS, bbox(LS), LSs, Dthr).

lstree_empty() -> nil.

lstree_insert(LS, LS_tree, Athr, Dthr) ->
    lstree_insert(unit_2d(LS, Dthr), LS, LS_tree, Athr, Dthr).

lstree_insert(nil, _LS0, LS_tree, _Athr, _Dthr) -> LS_tree;
lstree_insert(U0, LS0, nil, _Athr, Dthr)  ->
    {lstree_insert1(U0, mid(LS0), LS0, nil, Dthr), nil, nil};
lstree_insert(U0, LS0, {{U, _, _, _, _} = LS_tree, Small, Big}, Athr, Dthr) ->
    Sin = cross(U0, U),
    Cos = dot(U0, U),
    if
        Sin < -Athr, Cos > Athr; Sin > Athr, Cos < -Athr ->
            {LS_tree, lstree_insert(U0, LS0, Small, Athr, Dthr), Big}
		;
        Sin > Athr, Cos >= -Athr; Sin < -Athr, Cos =< Athr->
            {LS_tree, Small, lstree_insert(U0, LS0, Big, Athr, Dthr)}
		;
        true ->
            {lstree_insert1(U0, mid(LS0), LS0, LS_tree, Dthr), Small, Big}
    end.

lstree_insert1(U0, Mid_LS0, LS0, nil, _Dthr)  ->
    {U0, Mid_LS0, [LS0], nil, nil};
lstree_insert1(U0, Mid_LS0, LS0, {U, Mid_LS, LSs, Small, Big}, Dthr) ->
    V = sub(Mid_LS0, Mid_LS),
    D1 = cross(U, V),
    D2 = cross(U0, neg(V)),
    D0 = get_min(abs(D1), abs(D2)),
    D = if D1 >= 0.0 -> D0 ; true -> -D0 end,
    if
        D < -Dthr ->
            {U, Mid_LS, LSs,
	     lstree_insert1(U0, Mid_LS0, LS0, Small, Dthr), Big}
		;
        D > Dthr ->
            {U, Mid_LS, LSs,
	     Small, lstree_insert1(U0, Mid_LS0, LS0, Big, Dthr)}
		;
        true ->
	    {U, Mid_LS, merge(LS0, LSs, Dthr), Small, Big}
    end.

lstree_to_list(nil, List) -> List;
lstree_to_list({LS_tree, Small, Big}, List) ->
    LSs = lstree_to_list(LS_tree),
    lstree_to_list(Small, LSs ++ lstree_to_list(Big, List));
lstree_to_list({_U, _Mid_LS, LSs, Small, Big}, List) ->
    lstree_to_list(Small, LSs ++ lstree_to_list(Big, List)).

lstree_to_list(LS_tree) -> lstree_to_list(LS_tree, []).

unit_2d({{X1, Y1}, {X2, Y2}}, Dthr) ->
    UX0 = X2 - X1,
    UY0 = Y2 - Y1,
    if
        (abs(UX0) < Dthr) and (abs(UY0) < Dthr) ->
            nil
		;
        true ->
            D = math:sqrt(UX0 * UX0 + UY0 * UY0),
            {UX0 / D, UY0 / D}
    end.


get_mat_class(F,Mats_dict) ->
    io:put_chars(F, "<style type=\"text/css\">\n"),
    io:put_chars(F, "<![CDATA[\n"),
    foldl(fun( { _MatName, [{ _, { R,G,B,A } }] } , Cnt0 ) ->
		  Cnt = Cnt0 + 1,
		  Ts = (if A < 1.0 -> "shape-rendering:auto;";true->"" end),
		  Dr = trunc(R*255), Dg = trunc(G*255), Db = trunc(B*255),
		  FixName = "m",
		  io:fwrite(F, "."++"~ts~p { fill:~ts;opacity:~.2f;stroke:none;~ts }~n", [ FixName, Cnt, set_fill_hex_color(Dr,Dg, Db), A, Ts]),
		  Cnt
	  end, 0 , Mats_dict),
    io:put_chars(F, "]]>\n</style>\n").




%%
%%  EPS
%%

write_eps_header(F, {Wbb, Hbb}, Line_cap,_Mats_dict,_LSpos) ->
    io:put_chars(F, "%!PS-Adobe-2.0 EPSF-2.0\n"),
    io:fwrite(F, "%%BoundingBox: 0 0 ~w ~w~n", [round(Wbb), round(Hbb)]),
    io:put_chars(F, "/s {newpath moveto lineto stroke} bind def\n"),

    io:put_chars(F, "/m {gsave newpath moveto} bind def\n"),
    io:put_chars(F, "/l {lineto } bind def\n"),
    io:put_chars(F, "/f {setrgbcolor fill grestore} bind def\n" ),
    case Line_cap of
        0 -> ok;
        _ -> io:fwrite(F, "~w setlinecap~n", [Line_cap])
    end.

write_eps_line_group(_F, _BB_size, [], _Line_code, _Line_width, _Line_color, _Line_pattern, _Group_count,_Line_cap) -> ok;
write_eps_line_group(F, _BB_size, Ls, _Line_code, Line_width, Line_color, Line_pattern, Group_count,_Line_cap)
  when Group_count > 0; Line_width /= 1.0 ->

    %% Proportional Effect
    Line_width1 = get_max(Line_width, 0.0) * get_scale_factor(1.168),

    if Line_pattern =/= "0" ->
	    Dx = Line_pattern,
	    Dash ="[" ++ Dx ++ "] 0 setdash\n";
       true ->
	    Dash ="[] 0 setdash\n"
    end,

    {R,G,B,_A} = Line_color,
    io:fwrite(F, "~.1f setlinewidth~n~ts", [get_max(Line_width1, 0.0),Dash]),
    io:fwrite(F, "~.2f ~.2f ~.2f setrgbcolor~n", [R, G, B]),

    write_eps_line_group(F, Ls);
write_eps_line_group(F, _BB_size, Ls, _Line_code,_Line_width, _Line_color, _Line_pattern, _Group_count,_Line_cap) ->
    write_eps_line_group(F, Ls).

write_eps_line_group(F, Ls) ->
    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
		    io:fwrite(F, "~.1f ~.1f ~.1f ~.1f s~n", [X2, Y2, X1, Y1])
	    end, Ls).

write_eps_footer(F) -> io:put_chars(F, "%%EOF\n\n").


write_eps_polygons(_F, _BB_size, [],  _Mats_dict, _LSpos) -> ok;
write_eps_polygons(F, {_Wbb, _Hbb} = _BB_size, Fs, Mats_dict,{Shade,Lpos} ) ->
    Shade = get_pref(fill_shade_type, ?DEF_FILL_SHADE_TYPE),
    Lpos  = get_pref(light_pos, ?DEF_LIGHT_POS),
    foldl(fun({FV2Cs, FP, Mat},Cnt0) ->
		  {_Is_transparent, {R, G, B, _A}} = hd(dict__fetch(hd(Mat), Mats_dict)),
		  {SR, SG, SB} = set_face_shade(eps, FP, {R,G,B},Shade,Lpos), % shading(pseudo)
		  [{Hx,Hy} | FV2CsT] = FV2Cs,
		  io:fwrite(F, "~.2f ~.2f m ", [Hx,Hy]),
		  foreach(fun({X, Y}) ->
				  io:fwrite(F, "~.2f ~.2f l ", [X, Y])
			  end, FV2CsT),
		  io:fwrite(F, "~.2f ~.2f ~.2f f~n", [SR, SG, SB]),
		  poly_pb(Cnt0, length(Fs)),
		  Cnt0 + 1
	  end, 0, Fs).



%%
%%  Dot lines / SVG NPR Setting
%%


get_scale_factor(Factor) ->
    Scale_flag = get_pref( scale_prop, ?DEF_SCALE_PROP),
    Wbb = get_max(get_pref(bb_width, ?DEF_WIDTH), 800),
    Hbb = get_max(get_pref(bb_height, ?DEF_HEIGHT), 600),
    if  Scale_flag  =:= true -> abs(((Wbb + Hbb) * 0.75) / 1400)*Factor;true -> 1  end.
get_scale_factor()->
    Scale_flag = get_pref( scale_prop, ?DEF_SCALE_PROP),
    Wbb = get_max(get_pref(bb_width, ?DEF_WIDTH), 800),
    Hbb = get_max(get_pref(bb_height, ?DEF_HEIGHT), 600),
    if  Scale_flag  =:= true -> abs(((Wbb + Hbb) * 0.75) / 1400);true -> 1  end.


s_r(Val)->
    io_lib:format("~.2f",[Val]).

%% RGB=( 0, 255 ,0 ) -> #00FF00  ~F.P.Pad (F=2 P=16 PadChar=0)
set_fill_hex_color(R,G,B)->io_lib:format("#~2.16.0B~2.16.0B~2.16.0B",[get_min(255,R), get_min(255,G), get_min(255,B)]).
%% set_fill_rgb_color(R,G,B)-> io_lib:format("~B,~B,~B",[R,G,B]).

%% Shade 0: None
set_face_shade( Mode, _FP, {R,G,B}, Shade, _Lpos ) when Shade == 0  ->
    case Mode of
	eps -> {R,G,B};
	svg_class -> "";
	svg_attrb -> "fill:"++ set_fill_hex_color(round(R*255),round(G*255),round(B*255)) ++";"
    end;
%% Shade 1: Normal
set_face_shade( Mode, {FN, _FD}, {R,G,B}, Shade ,_Lpos ) when Shade == 1  ->
    {_Nx, _Ny, Nz0} = FN, Nz = abs(Nz0),
    %% io:format("RGB: ~p,~p,~p~n",[round(R*Nz*255),round(G*Nz*255),round(B*Nz*255)]),
    case Mode of
	eps -> {R*Nz,G*Nz,B*Nz};
	svg_class -> " style=\"fill:"++ set_fill_hex_color(round(R*Nz*255),round(G*Nz*255),round(B*Nz*255)) ++"\"";
	svg_attrb -> "fill:"++ set_fill_hex_color(round(R*Nz*255),round(G*Nz*255),round(B*Nz*255)) ++";"
    end;

%% Shade 6: PatID= [1,2,3,4]    [5,6,7,8]
set_face_shade( Mode, {FN, _FD}, {_R,_G,_B}, Shade,Lpos ) when Shade >= 6 andalso Mode =/= eps ->
    {Lx,Ly,Lz,Fact}=Lpos, PatId = (Shade - 6) * 4 + 1,
    Nz0 = abs(1-(dot(FN, {Lx,Ly,Lz})*Fact)),
						%io:format("dot:~p  FN ~p ~n",[Nz0,FN]),
    Nz = case Shade >= 6  of
	     true when Nz0*0.5 < 0.2 -> io_lib:format("url(#pt~p)",[PatId+3]); % dark
	     true when Nz0*0.5 < 0.4 -> io_lib:format("url(#pt~p)",[PatId+2]); % middle
	     true when Nz0*0.5 < 0.6 -> io_lib:format("url(#pt~p)",[PatId+1]); % light
	     true when Nz0*0.5 < 0.8 -> io_lib:format("url(#pt~p)",[PatId]);
	     true when Nz0*0.5 < 0.95 -> "#FFFFFF";
	     _ -> "#FFFFFF"
	 end,

    case Mode of
	svg_class -> io_lib:format(" style=\"fill:~ts\"",[Nz]);
	svg_attrb -> io_lib:format("fill:~ts;",[Nz])
    end;




%% Shade 2: Shade
set_face_shade( Mode, {FN, _FD}, {R,G,B}, Shade,Lpos ) ->
    {Lx,Ly,Lz,Fact}=Lpos,
    Nz0 = abs(1-(dot(FN, {Lx,Ly,Lz})*Fact)),
						%io:format("dot:~p  FN ~p ~n",[Nz0,FN]),
    Nz = case Shade of
	     2 -> Nz0;
	     3 when Nz0 < 0.88 -> 0.8;
	     4 when Nz0 < 0.3  -> 0.3; % dark
	     4 when Nz0 < 0.6  -> 0.6; % middle
	     4 when Nz0 < 0.88 -> 0.8; % light
	     5 when Nz0 < 0.88 -> 0.25;
	     _ when Nz0 > 0.9 -> 1;
	     _ -> Nz0
	 end,

    case Mode of
	eps -> {R*Nz,G*Nz,B*Nz};
	svg_attrb -> "fill:"++ set_fill_hex_color(round(R*Nz*255),round(G*Nz*255),round(B*Nz*255)) ++";";
	_ -> " style=\"fill:"++ set_fill_hex_color(round(R*Nz*255),round(G*Nz*255),round(B*Nz*255)) ++"\""
    end.

%% Shade
define_fill_type() ->
    [
     { ?__(1,"None")    ,0},
     { ?__(2,"Normal")  ,1},
     { ?__(3,"Shade")   ,2},
     { ?__(4,"2-tone")  ,3},
     { ?__(5,"3-tone")  ,4},
     { ?__(6,"Contrast"),5},
     { ?__(7,"HalfTone"),6},
     { ?__(8,"Hatching"),7}
    ].

%% Light_positon            Name {Lx, Ly, Lz, Illuminence Factor}
define_light_pos() ->
    [
      { ?__(1,"Up to Down") , {-480,-512, 500,0.00096} },
      { ?__(2,"Down to Up") , {-480, 512, 500,0.00096} },
      { ?__(3,"Down to Up2"), {   1, 512, 256,0.00108} },
      { ?__(4,"Contrast")   , {-480,-512, 512,0.00108} },
      { ?__(5,"Overlight")  , {   1,-999, 300,0.00186} },
      { ?__(6,"From Right") , {-700,-400, 600,0.0005} },
      { ?__(7,"From Left")  , { 600,-400, 700,0.0005} },
      { ?__(8,"Center")     , {-400,-400,-400,0.0001} }

    ].

%% Line style  ( dotted pattern )
define_dot_styles() ->
    [
     { "None"  , "0"},
     { "Dash-1", "1 1"},
     { "Dash-2", "2 2"},
     { "Dash-4", "4 4"},
     { "Dash-8", "8 8"},
     { "Dot-2" , "0.1 2"},
     { "Dot-4" , "0.1 4"},
     { "Dot-6" , "0.1 6"},
     { "_-__"  , "6 3 1 3"},
     { "_--_"  , "15 5 1 3 1 5"},
     { "_.__"  , "6 3 0.1 3"},
     { "_.._"  , "16 2 0.1 2 0.1 2"},
     { "_._x2" , "20 5 0.1 5"},
     { "_..x2" , "20 5 0.1 5 0.1 5"},
     { "Fibonacci", "1 1 2 3 5 8 13"},
     { "Irregular", "13 3 1 1 3 5"},
     { "Irregular2", "0 6 1 0 3 7"},
     { "Irregular3", "0 7 80 0 18 0"}
    ].

define_svg_filter( Mode, Type) ->
    %% ui: return Name for UI build,  getp_aram: return Other Paramaters
    %%  Freq , Octave, Scale, Blur  ,  Morph, fact,  Opt,  Name     , Type#   Opt -1 turbulance
    FList0 =[
	     { "0",          0,     0,    0,   0,     0,    0, "Not apply"},        %%  1
	     { "0.013",      2,     5,    0,   0,     0,    0, "Drawn" },           %%  2
	     { "0.089",      4,     3,    0,   0,     0,    0, "Pencil"},           %%  3
	     { "0.030",     10,    14,    0,   0,     0,    0, "Brush" },           %%  4
	     { "0.15 0.007", 1,     6,    0,   0,     0,   -1, "Paint" },           %%  5
	     { "0.0053 0.03",10,    6,    0,   0,     0,   -1, "Doodle"},           %%  6
	     { "0.03 0.018", 3,    12,    0,   0,     0,    1, "Twice Draw" },      %%  7
	     { "0.15 0.038", 3,     7,    0,   0,     0,    2, "Twice Draw2"},      %%  8
	     { "0.05 0.018", 2,     9,  1.0,   0,     0,    1, "Twice (Wet)"},      %%  9
	     { "-0.17320",   6,     2,    0,   0,     0,    0, "Grunge Brush" },    %% 10
	     { "0.9",        2,     3,    0,   0,     0,    0, "Chalk" },           %% 11
	     { "0.9",        2,    10,    0,   0,     0,    0, "Chalk(Rough)" },    %% 12
	     { "0.5",        5,     7,  3.0,   0,     0,    2, "Crayon" },          %% 13
	     { "0.9",        2,    20,    0,   0,     0,    0, "Sand Art"},         %% 14
	     { "0.019",     14,    18,    0,   0,     0,    0, "Distortion" },      %% 15
	     { "0.01 0.008", 3,    12,    0,  "D","0.9",    3, "Shadow Blur"},      %% 16
	     { "0.002 0.002",5,    12,    0,  "D","0.9",    3, "Shadow Blur2"},     %% 17
	     { "0.01 0.008" ,5,    12,    0,  "D",  "2",    3, "Shadow Blur3"},     %% 18
	     { "0.9 0.9"   , 5,    12,    0,  "D",  "2",    3, "Shadow Blur4"},     %% 19
	     { "0.05 0.05", 2,     9,   0.5,   0,     0,    6, "Triple Lines"},     %% 20
	     { "0.005 0.0045",8,   12,    0,   0,     0,    5, "Wet Blur"},         %% 21
	     { "0.0015 0.0045",8,  12,    0,   0,     0,    5, "Wet Blur2"},        %% 22
	     { "0.005 0.42" ,5,    12,    0,   0,     0,    5, "Graffiti(H)"},      %% 23
	     { "0.42 0.005" ,5,    12,    0,   0,     0,    5, "Graffiti(V)"},      %% 24
	     { "0.0006 0.0078",7,   6,  1.0, "E",   "0.1",  5, "Wind Blur(H)" },    %% 25
	     { "0.0078 0.0006",7,   6,  1.0, "E",   "0.1",  5, "Wind Blur(V)" },    %% 26
	     { "0.53 0.3",   4,    12,    0, "E",   "0.1",  5, "DrawingPaper" },    %% 27
	     { "0.4 0.95",   7,    12,    0, "E",   "0.1",  5, "DrawingPaper2" },   %% 28
	     { "0.9 0.9",    5,     8,    0, "E",   "2",    5, "DrawingPaper3"},    %% 29
	     {   "1",        1,     1,    0,   0,     0,    4, "Silhouette"},       %% 30
	     { "0",          0,     0, 0.25,   0,     0,    0, "Blur 0.25"  },      %% 31
	     { "0",          0,     0,  1.0,   0,     0,    0, "Blur   1"   },      %% 32
	     { "0",          0,     0,  2.5,   0,     0,    0, "Blur 2.5"   },      %% 33
	     { "0",          0,     0,  5.0,   0,     0,    0, "Blur   5"   },      %% 34
	     { "0",          0,     0, 10.0,   0,     0,    0, "Blur  10"   },      %% 35
	     { "0.089",      1,     3,    0,   0,     0,    0, "OverSketch"}        %% 36
	    ],

    case Mode of
	ui ->{ AList, _Total } = foldl(fun(X,  { Acc0 ,Cnt0 } ) ->
					       { _, _, _, _, _,_,_, Name } = X,
					       Cnt = Cnt0 + 1,
					       {Acc0 ++ [{ Name, Cnt }] , Cnt }
				       end, {[],0} , FList0 ),
	     AList;
        get_param -> { Freq, Octave, Scale, Blur, Morph, Mfactor, Option ,Name }  = lists:nth( Type , FList0),
                     { Freq, Octave, Scale, Blur, Morph, Mfactor, Option ,Name };
	get_length -> length(FList0)
    end.



add_svg_filter(Mode,Type) when Type > 1 ->
    case Mode of
	fill -> " filter=\"url(#fill_style"++ integer_to_list(Type)++")\" ";
	line -> " filter=\"url(#line_style"++ integer_to_list(Type)++")\" "
    end;
add_svg_filter(_, _) -> "".



turbulence_type(Flag) when Flag == -1  ->"turbulence";
turbulence_type(_)  ->"fractalNoise".


get_svg_color_int()->
    case get_pref(svg_color_int, ?DEF_SVG_COLOR_INT) of
	0 -> "sRGB";
	1 -> "linearRGB";
	2 -> "auto";
	_ -> "sRGB"
    end.



add_svg_filter_tag(F, Mode, Type) when Type > 1 ->
    Id_name = case Mode of
                  fill -> "fill";
                  line -> "line"
	      end,
    { Freq, Octave, Scale, Blur, Morph0 ,Mfactor0 ,Option, Name } = define_svg_filter(get_param, Type),

    { Morph,Mfactor} = case Morph0    of
			   "E" -> {"erode" ,[Mfactor0] };
			   "D" -> {"dilate",[Mfactor0] };
			   _ -> {"dilate","0.9 0.25" }
		       end,

    %% Color interpolution filters
    Svg_color_int = get_svg_color_int(),

    %% Proportional Effect
    Scf =  get_scale_factor(),

    %%  Set color space to sRGB for Blur, because only Safari use linearRGB default
    io:fwrite(F, "<!-- Wings3D SVG Style library ~p: ~ts -->~n",[Type ,Name ]),
    io:fwrite(F, "<filter id=\"~ts" ++ "_style~p\" >~n",[Id_name,Type]),

    if Freq =/= "0" -> io:fwrite(F, "\t<feTurbulence type=\"~ts\" baseFrequency=\"~ts\"  numOctaves=\"~p\" result=\"filpr1\" filterRes=\"720 720\" />~n", [ turbulence_type(Option), Freq, Octave]); true -> ok  end,
    if Scale > 0 -> io:fwrite(F, "\t<feDisplacementMap xChannelSelector=\"R\" yChannelSelector=\"G\" in=\"SourceGraphic\" in2=\"filpr1\"  result=\"filpr2\"  scale=\"~ts\" />~n",[s_r(Scale*Scf*1.0)]); true -> ok  end,
    if Blur > 0 -> io:fwrite(F, "\t<feGaussianBlur color-interpolation-filters=\"~ts\"  stdDeviation=\"~ts\"/>~n", [Svg_color_int, s_r(Blur*Scf*1.0)] ); true -> ok  end,

    case Option of
	0 -> ok;
	1 -> % TwiceDrawn
	    io:fwrite(F, "\t<feTurbulence baseFrequency=\"0.0735\" type=\"fractalNoise\" result=\"filpr3\" numOctaves=\"4\"/>\n"
		      "\t<feDisplacementMap scale=\"6\" xChannelSelector=\"R\" in2=\"filpr3\" yChannelSelector=\"G\" result=\"filpr4\" in=\"SourceGraphic\"/>\n"
		      "\t<feComposite operator=\"over\" in2=\"filpr4\" color-interpolation-filters=\"~ts\" result=\"filpr5\" in=\"filpr2\"/>\n",
		      [Svg_color_int]);
	2 -> % TwiceDraw(strong)
	    io:put_chars(F, "\t<feTurbulence baseFrequency=\"0.02 0.14\" type=\"fractalNoise\" result=\"filpr3\" numOctaves=\"2\"/>\n"
			 "\t<feDisplacementMap scale=\"15\" xChannelSelector=\"R\" in2=\"filpr3\" yChannelSelector=\"G\" result=\"filpr4\" in=\"SourceGraphic\"/>\n"
			 "\t<feBlend in2=\"filpr4\" mode=\"lighten\" result=\"filpr5\" in=\"filpr2\"/>\n"
			);
	3 -> %color
	    io:put_chars(F, "\t<feDisplacementMap scale=\"8\" xChannelSelector=\"R\" in2=\"filpr2\" yChannelSelector=\"G\" result=\"filpr3\" in=\"SourceGraphic\"/>\n" ),
	    io:fwrite(F,    "\t<feMorphology operator=\"~ts\"  in=\"filpr2\" result=\"filpr6\" radius=\"~ts\" />\n", [Morph, Mfactor ] ),
	    io:fwrite(F,   "\t<feGaussianBlur in=\"filpr6\" stdDeviation=\"~ts\"  result=\"filpr6\" color-interpolation-filters=\"~ts\" />\n", [ Mfactor, Svg_color_int] ),
	    io:put_chars(F, "\t<feColorMatrix type=\"matrix\" values=\"0 0 1 0 0, 0 0 1 0 0, 0 0 1 0 0, 0 0 0 0.67 0.0\" in2=\"filpr6\" result=\"filpr7\"   /> \n"
			 "\t<feBlend mode=\"screen\"  in=\"filpr7\" in2=\"filpr2\"/>\n"
			);
	4 -> % Silhouette
	    io:put_chars(F, "\t<feBlend in2=\"filpr2\" mode=\"over\" result=\"filpr5\" in=\"SourceAlpha\"/>\n"
			);

	5 -> % WetPaint
	    io:put_chars(F,
			 "\t<feDisplacementMap scale=\"8\" xChannelSelector=\"R\" in2=\"filpr2\" yChannelSelector=\"G\" result=\"filpr3\" in=\"SourceGraphic\"/>\n"),
	    io:fwrite(F, "\t<feMorphology operator=\"~ts\"  in=\"filpr2\" result=\"filpr6\" radius=\"~ts\" />\n" ,[Morph,Mfactor]),
	    io:fwrite(F,"\t<feTurbulence type=\"turbulence\" baseFrequency=\"~ts\" in=\"SourceGraphic\" numOctaves=\"~p\" result=\"filpr9\" />~n",[Freq,Octave]),
	    io:put_chars(F, "\t<feColorMatrix type=\"matrix\" values=\".2 .2 .2 0 0, .2 .2 .2 0 0, .4 .4 .2 0 0, 0 0 0 0.2 0\" in=\"filpr6\" result=\"filpr7\" />\n"
			 "\t<feComposite operator=\"xor\" in=\"filpr6\" in2=\"filpr9\" result=\"filpr10\" />\n"
			 "\t<feBlend mode=\"darken\"  in=\"filpr10\" in2=\"filpr7\" result=\"filpr11\"/>\n"
			 "\t<feComposite operator=\"out\" in=\"filpr2\" in2=\"filpr11\"  />"
			);

	6 -> % Triple Line
	    io:put_chars(F,
			 "\t<feTurbulence type=\"fractalNoise\" baseFrequency=\"0.05\"  numOctaves=\"5\" result=\"filpr2\" filterRes=\"720 720\" />\n"
			 "\t<feDisplacementMap xChannelSelector=\"R\" yChannelSelector=\"G\" in=\"SourceGraphic\" in2=\"filpr2\"  result=\"filpr2\"  scale=\"7\" />\n"
			 "\t<feColorMatrix type=\"matrix\" values=\"0 0 1 0 0, 0 0 1 0.7 0, 0 0 1 0 0, 0 0 0 0.65 0.0\" in=\"filpr2\" result=\"filpr3\"   /> \n"
			 "\t<feTurbulence type=\"fractalNoise\" baseFrequency=\"0.030\"  numOctaves=\"5\" result=\"filpr11\" filterRes=\"720 720\" />\n"
			 "\t<feDisplacementMap scale=\"7\" xChannelSelector=\"R\"  yChannelSelector=\"G\" result=\"filpr12\" in2=\"filpr11\" in=\"SourceGraphic\"/>\n"
			 "\t<feColorMatrix type=\"matrix\" values=\"0 0 1 0.7 0, 0 0 1 0 0, 0 0 1 0 0, 0 0 0 0.45 0.0\" in=\"filpr12\" result=\"filpr13\"   />\n"
			 "\t<feTurbulence type=\"fractalNoise\" baseFrequency=\"0.03 0.08\"  numOctaves=\"5\" result=\"filpr21\" filterRes=\"720 720\" />\n"
			 "\t<feColorMatrix type=\"matrix\" values=\"0 0 1 0 0, 0 0 1 0 0, 0 0 1 0.5 0, 0 0 0 0.55 0.0\" in=\"filpr21\" result=\"filpr22\"   />\n"
			 "\t<feDisplacementMap scale=\"5\" xChannelSelector=\"R\"  yChannelSelector=\"G\" result=\"filpr23\"  in2=\"filpr22\" in=\"SourceGraphic\"/>\n"
			 "\t<feComposite operator=\"in\" in=\"filpr1\" in2=\"filpr2\" result=\"filpr103\" />\n"
			 "\t<feMerge>\n"
			 "\t\t<feMergeNode in=\"filpr3\"/>\n"
			 "\t\t<feMergeNode in=\"filpr13\"/>\n"
			 "\t\t<feMergeNode in=\"filpr23\"/>\n"
			 "\t\t<feMergeNode in=\"filpr103\"/>\n"
			 "\t</feMerge>\n"
			 "\t<feComposite operator=\"out\" in2=\"filpr11\"  />\n"
			);

	_ -> ok
    end,
    io:put_chars(F, "</filter>\n" );
add_svg_filter_tag(_, _, _) -> " ".


add_svg_pattern(F,Shade)->
    {R,G,B,_A}= get_pref( regl_color, ?DEF_REGL_COLOR),
    Color = set_fill_hex_color(round(R*255),round(G*255),round(B*255)),
    Scf=get_scale_factor(),

    case Shade of
	6 ->
	    io:put_chars(F, "<pattern id=\"pt1\" viewBox=\"-30 -30 30 30\" x=\"0\" y=\"8\" width=\"8\" height=\"8\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.8*Scf) ++") rotate(35)\"><rect x=\"-30\" y=\"-30\" width=\"35\" height=\"35\" stroke=\"none\" fill=\"white\" /><circle stroke=\"none\" cx=\"-10\" cy=\"-10\" r=\"3\" fill=\""++ Color ++"\" /></pattern>\n"
			 "<pattern id=\"pt2\" viewBox=\"-30 -30 30 30\" x=\"0\" y=\"8\" width=\"8\" height=\"8\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.5*Scf) ++") rotate(35)\"><rect x=\"-30\" y=\"-30\" width=\"35\" height=\"35\" stroke=\"none\" fill=\"white\" /><circle stroke=\"none\" cx=\"-10\" cy=\"-10\" r=\"5\" fill=\""++ Color ++"\" /></pattern>\n"
			 "<pattern id=\"pt3\" viewBox=\"-30 -30 30 30\" x=\"0\" y=\"10\" width=\"10\" height=\"10\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.5*Scf) ++") rotate(35)\"><rect x=\"-30\" y=\"-30\" width=\"35\" height=\"35\" stroke=\"none\" fill=\"white\" /><circle stroke=\"none\" cx=\"-10\" cy=\"-10\" r=\"10\" fill=\""++ Color ++"\" /></pattern>\n"
			 "<pattern id=\"pt4\" viewBox=\"0 0 40 40\" x=\"10\" y=\"10\" width=\"20\" height=\"20\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.3*Scf) ++") rotate(35)\"><rect x=\"0\" y=\"0\" width=\"41\" height=\"41\" stroke=\"none\" fill=\"white\" /><circle stroke=\"none\" cx=\"18\" cy=\"18\" r=\"20\" fill=\""++ Color ++"\" /></pattern>\n");
	7 ->
	    io:put_chars(F,
			 "<pattern id=\"pt5\" width=\"20\" height=\"20\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.6*Scf) ++") rotate(-15)\"><rect x=\"0\" y=\"0\" width=\"40\" height=\"40\" stroke=\"none\" fill=\"white\" /><g stroke=\""++ Color ++"\" stroke-width=\"0.3\"  ><path d=\"M0,0 40,40z\"/></g></pattern>\n"
			 "<pattern id=\"pt6\" width=\"20\" height=\"20\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.6*Scf) ++") rotate(15)\"><rect x=\"0\" y=\"0\" width=\"20\" height=\"20\" stroke=\"none\" fill=\"white\" /><g stroke=\""++ Color ++"\" stroke-width=\"0.5\"  ><path d=\"M0,0 20,20z\"/></g></pattern>\n"
			 "<pattern id=\"pt7\" width=\"10\" height=\"10\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.6*Scf) ++") rotate(15)\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\" stroke=\"none\" fill=\"white\" /><g stroke=\""++ Color ++"\" stroke-width=\"1\"  ><path d=\"M0,0 20,20z\"/></g></pattern>\n"
			 "<pattern id=\"pt8\" width=\"10\" height=\"10\" patternUnits=\"userSpaceOnUse\" patternTransform=\"scale("++ s_r(0.6*Scf) ++") rotate(15)\"><rect x=\"0\" y=\"0\" width=\"10\" height=\"10\" stroke=\"none\" fill=\"white\" /><g stroke=\""++ Color ++"\" stroke-width=\"1.5\"  ><path d=\"M0,0 10,10M 0,10 10,0z\"/></g></pattern>\n");
	_ ->ok
    end.


add_svg_marker(F)->
    io:put_chars(F, "<marker id=\"mkr1\" viewBox=\"0 0 380 60\" refX=\"82\" refY=\"10\"\n"
		 "\tmarkerUnits=\"strokeWidth\" orient=\"auto\"\n"
		 "\tmarkerWidth=\"60\" markerHeight=\"32\">\n"
		 "\t<path fill=\"none\" stroke=\"#000\" stroke-width=\"0.65\" d=\"M14.899,16.505c0,0,63.636-14.141,92.929,2.021\"/>\n"
		 "</marker>\n").


get_svg_prefs()->
    { get_pref(svg_art_fill_type, ?DEF_SVG_ART_FILL_TYPE),
      get_pref(svg_art_line_type, ?DEF_SVG_ART_LINE_TYPE),
      define_svg_filter( get_length, 0) }.

%% SVG Filter export
add_svg_filters(F,Is_style_catalog,Fill_type, Line_type) ->

    %% Make for Style Catalog (This mode doesn't export 3D data)
    if  Is_style_catalog =:= true ->
	    lists:foldl(fun(_A,Cnt) -> add_svg_filter_tag( F, line,Cnt) ,  Cnt+1 end, 1,  define_svg_filter(ui, 0)),   % Export All Filter
	    add_svg_pattern(F,7);
	true ->
	    add_svg_filter_tag( F, fill,  Fill_type ),
	    add_svg_filter_tag( F, line,  Line_type )
    end.

%% For Catalog  mode (All styles are display, Non 3D)
add_svg_catalog_base(F) ->

    Svg_color_int = get_svg_color_int(),

    io:put_chars(F, "<g id=\"portfolio\" transform=\"matrix(1 0 0 1 15 15)\" >\n"),
    io:fwrite(   F, "<text transform=\"matrix(1 0 0 1 2 25)\" stroke=\"none\" font-family=\"Arial Black\" font-size=\"16\">Wings3D<tspan font-family=\"Arial\" font-size=\"8\" >" ++ wpa:version() ++ "</tspan> SVG Setting Help <tspan font-family=\"Arial\" x=\"0\" dy=\"20\">The Style/Filter preset catalog.(~ts)</tspan></text>\n",[Svg_color_int]),
    io:put_chars(F,"\t<text transform=\"matrix(1 0 0 1 220 70)\" stroke=\"none\" font-family=\"Arial Black\" font-size=\"9\">SVG Artistic styles<tspan x=\"0\" dy=\"10\">for a fill and a line</tspan></text>\n"),

    %% Filter Style
    lists:foldl(fun( A, { Col0, Row0, Cnt }) ->
			{Col, Row} = if Cnt rem 7 == 0 -> {Col0 + 1, 0}; true -> {Col0, Row0 + 1} end,
			{Name, _No} = A, add_svg_style_catalog(F,Name, Row, Col, Cnt),
			{Col, Row, Cnt + 1 }
		end, {0, 0, 1}, define_svg_filter(ui, 0)),  % Export All style

    %% Dot Line Style
    io:put_chars(F,"\t<text transform=\"matrix(1 0 0 1 100 70)\" stroke=\"none\" font-family=\"Arial Black\" font-size=\"9\">The line styles <tspan x=\"0\" dy=\"10\">in the lucent Material</tspan></text>\n"),
    lists:foldl(fun({Name, Dot_style},Cnt0 ) ->
			io:put_chars(F,
				     "\t<line transform=\"matrix(1 0 0 1 105 "++ integer_to_list( 88 + 16 * Cnt0 ) ++")\" fill=\"none\" stroke=\"#000\" stroke-width=\"1\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-dasharray=\""++ Dot_style ++"\" x1=\"0\" y1=\"0\" x2=\"40\" y2=\"0\"/>\n"
				     "\t<text transform=\"matrix(1 0 0 1 150 "++ integer_to_list( 88 + 2 + 16 * Cnt0 ) ++")\" stroke=\"none\" font-weight=\"lighter\"  font-size=\"9\">"++ Name ++"</text>\n"
				    ),
			Cnt0 + 1
		end, 0, define_dot_styles()),

    %% Line width
    Pos = 70, % length(define_dot_styles()) + 1,
    lists:foldl(
      fun( _Width, Cnt0 ) when Cnt0 == 0 ->
	      io:put_chars(F, "\t<text transform=\"matrix(1 0 0 1 5 "++ integer_to_list(  Pos + 2 + 18 * Cnt0 ) ++")\" stroke=\"none\" font-family=\"Arial Black\" font-size=\"9\">Line width(pt)</text>\n"),
	      Cnt0 + 1;
	 ( Width, Cnt0 ) ->
	      io:put_chars(F, "\t<line transform=\"matrix(1 0 0 1 5 "++ integer_to_list( Pos + 18 * Cnt0 ) ++")\" fill=\"none\" stroke=\"#000\" stroke-width="),
	      io:fwrite(   F, "\"~.1fpt\"", [Width]),
	      io:put_chars(F, " stroke-linecap=\"round\" stroke-linejoin=\"round\"  x1=\"0\" y1=\"0\" x2=\"40\" y2=\"0\"/>\n"
			   "\t<text transform=\"matrix(1 0 0 1 55 "++ integer_to_list(  Pos + 2 + 18 * Cnt0 ) ++")\" stroke=\"none\" font-size=\"9\">"),
	      io:fwrite(   F, "~.1fpt", [Width]),
	      io:put_chars(F, "</text>\n"),
	      Cnt0 + 1
      end, 0, [0.0,0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.35, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0,10.0]),
    io:put_chars(F,"\t<text transform=\"matrix(1 0 0 1 0 370)\" stroke=\"none\" font-size=\"9\">Note:"
		 "\t\t<tspan x=\"0\" dy=\"12\">*To using lightweight data for Website.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">Results of the 3D rendering is actually</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">dividing many polygons.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">(In particular face polygons)</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">If you want to use the result for web,</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">you should be more reduce Data size and </tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">merge polygons to same colors. </tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">(Ex.Illustrator: Use PathFinder &gt; Merge)</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"20\">*Note2:</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">Advise for Illustrator CS,LibreOffice Draw User:</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">In these software,SVG importing is very slowly.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">If SVG file size is over than 2MB,avoid reading</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">by these.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">There is no problem to read in Inkscape.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"20\">*SVG CSS Property:</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">To UseClasName to make smaller file size.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">Use Attribute_Compound option,Adobe Illustrator SVG importing speed is better than other type.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">this option saving file size too, but Shade/Transparent display is not good.</tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"20\">File size Comparison:  </tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"15\">Attribute: 707Kb (--- Standard size --) </tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">ClassName: 518Kb (smaller than 27-30% ) </tspan>\n"
		 "\t\t<tspan x=\"0\" dy=\"12\">Attrcompd: 564Kb (smaller than 21-42% ) </tspan>\n"
		 "\t</text>\n"),
    io:put_chars(F, "</g>\n").

add_svg_style_catalog(F, Name, Col, Row, Cnt)->
    io:put_chars(F,
		 "<g transform=\"matrix(1 0 0 1 "++integer_to_list( 220 + 140 * Row )++" "++integer_to_list( 8 + 78 * Col )++")\">\n"
		 "\t<rect x=\"0\" y=\"0\" stroke-width=\"0.5\"  stroke=\"#abc\"  fill=\"none\" width=\"115\" height=\"72\"/>\n" % frame
		 "\t<rect x=\"3\" y=\"27\"  stroke=\"none\"  fill=\"#dee\" width=\"68\" height=\"32\"/>\n"
		 "\t<g id=\"sample_fill"++ integer_to_list(Cnt) ++"\" "++ add_svg_filter(line,Cnt) ++" >\n"
		 "\t\t<rect x=\"36\" y=\"30\"  stroke=\"none\"  fill=\"#E72243\" width=\"26\" height=\"26\"/><rect x=\"36\" y=\"4\" stroke=\"none\" fill=\"#43A722\" width=\"26\" height=\"26\"/>\n" %% fill red
		 "\t</g>\n"
		 "\t<g id=\"sample_line"++ integer_to_list(Cnt) ++"\" "++ add_svg_filter(line,Cnt) ++" >\n"
		 "\t\t<rect x=\"7\" y=\"7\" fill=\"none\" stroke=\"#000\" stroke-width=\"2\" stroke-miterlimit=\"10\" width=\"26\" height=\"26\"/>\n"  % outline
		 "\t\t<rect x=\"20\" y=\"20\" fill=\"url(#pt8)\" stroke=\"#000\" stroke-miterlimit=\"10\" width=\"26\" height=\"26\"/>\n"
		 "\t\t<line fill=\"none\" stroke=\"#000\" stroke-width=\"2\" stroke-miterlimit=\"10\" x1=\"70\" y1=\"16.5\" x2=\"110\" y2=\"16\"/>\n"
		 "\t\t<line fill=\"none\" stroke=\"#000\" stroke-width=\"1\" stroke-miterlimit=\"10\" x1=\"70\" y1=\"29\" x2=\"110\" y2=\"29\"/>\n"
		 "\t\t<line fill=\"none\" stroke=\"#000\" stroke-width=\"1\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-dasharray=\"6,6\" x1=\"70\" y1=\"40\" x2=\"110\" y2=\"40\"/>\n"
		 "\t</g>\n"
		 "\t<text transform=\"matrix(1 0 0 1 5 68)\" stroke=\"none\" font-weight=\"lighter\"  font-size=\"9\">"++ Name ++"</text>\n"
		 "</g>\n"
		).



%%
%%  SVG
%%


write_svg_header(F, {Wbb, Hbb}, Line_cap, Mats_dict, {Shade,_Lpos}) ->
    io:put_chars(F, "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
		 "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" "
		 "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"),
    io:put_chars(F, "<!-- Generated by Wings3D " ++ wpa:version() ++ " Hidden Lines Renderer -->\n"),
    io:put_chars(F, "<svg version=\"1.2\" baseProfile=\"tiny\"   id=\"Layer1\" \n  x=\"0px\" y=\"0px\" "),

    case get_pref( svg_render_type, ?DEF_SVG_RENDER_TYPE) of jaggy_line -> io:put_chars(F,"shape-rendering=\"crispEdges\""); _ -> ok end,
    case get_pref(responsive, ?DEF_RESPONSIVE) of true -> ok;false -> io:fwrite(F, " width=\"~w" ++ "px\" height=\"~w" ++ "px\" " , [ round(Wbb),round(Hbb)]) end,

    io:fwrite(F, " viewBox=\"~w ~w ~w ~w\" style=\"enable-background:new 0 0 ~w ~w;\"~n" , [0, 0, round(Wbb), round(Hbb), round(Wbb), round(Hbb)]),
    io:put_chars(F, " stroke=\"black\""), % storoke = "currentColor" cant work LibreOfficeDraw
    case Line_cap of
	0 -> ok;
	_ ->
	    io:put_chars(F, lists:flatten([" stroke-linecap=\"",
					   case Line_cap of 1 -> "round"; 2 -> "square" end, "\""]))
    end,
    io:put_chars(F, " xmlns=\"http://www.w3.org/2000/svg\">\n"),

    Is_style_catalog = (file_test_svg =:= get_pref(file_type, ?DEF_FILE_TYPE)),
    { Fill_type, Line_type, Style_length } =get_svg_prefs(),

    io:put_chars(F, "\n" ++ "<defs>\n" ),
    add_svg_filters(F,Is_style_catalog,Fill_type, Line_type ),
    if Line_type == Style_length -> add_svg_marker(F);true ->ok end,
    if Shade >= 6 -> add_svg_pattern(F,Shade);true->ok end,
    io:put_chars(F,  "</defs>\n" ),
    if  Is_style_catalog =:= true -> add_svg_catalog_base(F);true -> ok end,

    case get_pref(file_type, ?DEF_FILE_TYPE) == file_svg andalso get_pref(svg_attb_type, ?DEF_SVG_ATTB_TYPE) of
        css_class -> get_mat_class(F,Mats_dict);
        attribute -> ok;
        compound -> ok;
        _ -> ok
    end.

svg_line_cap(Line_cap) when Line_cap > 0 ->
    Type = case Line_cap of 1 -> "round"; 2 -> "square"; _-> "butt" end,
    "stroke-linejoin=\""++ Type ++"\" ";
svg_line_cap(_)->"".


write_svg_line_group(_F, _BB_size, [], _Line_code, _Line_width, _Line_color, _Line_pattern, _Group_count,_Line_cap) -> ok;
write_svg_line_group(F, BB_size, Ls, Line_code, Line_width, Line_color, Line_pattern, Group_count,Line_cap)
  when Group_count > -1  -> %% Line_width /= 1.0
    { _Fill_type, Line_type, Style_length } =get_svg_prefs(),

    %% Proportional Effect
    Line_width1 = get_max(Line_width, 0.0) * get_scale_factor(1.168),
    if Line_pattern =/= "0" ->
	    Dx = Line_pattern,
	    Dash ="stroke-dasharray=\"" ++ Dx ++ "\"";
       true ->
	    Dash =""
    end,

    Overline = if Line_type == Style_length andalso Line_code=:="Outline"  -> "marker-start=\"url(#mkr1)\" marker-end=\"url(#mkr1)\"";true ->"" end,

    FixName = trunc(Line_width1 * 10),
    {R,G,B,A} = Line_color,
    {SR, SG, SB, SA} = {round(R * 255.0), round(G * 255.0), round(B * 255.0),A },
    Line_color_hex = set_fill_hex_color(SR,SG,SB),
    io:fwrite(F,
	      "\t<g  id=\"Line~p_~p\" inkscape:label=\"Line~p_~p\" inkscape:groupmode=\"layer\" stroke=\"~ts\" opacity=\"~.1f\"  stroke-width=\"~.1f\" ~ts "
	      "xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"  fill=\"none\" ~ts ~ts>\n",
	      [  FixName, Group_count, FixName, Group_count, set_fill_hex_color(SR,SG,SB), SA, Line_width1, Dash, Overline,svg_line_cap(Line_cap)]
	     ),
    Var_stroke = get_pref(var_stroke, ?DEF_VAR_STROKE),
    write_svg_line_group(F, BB_size, Ls, Var_stroke, Line_color_hex),
    io:put_chars(F, "\t</g>\n");

write_svg_line_group(F, BB_size, Ls, _Line_code, _Line_width, _Line_color, _Line_pattern, _Group_count,_Line_cap) ->
    io:put_chars(F, "\t<g id=\"Line-base\" inkscape:label=\"Line-base\" inkscape:groupmode=\"layer\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" fill=\"none\">\n"),
    Var_stroke = get_pref(var_stroke, ?DEF_VAR_STROKE),
    write_svg_line_group(F, BB_size, Ls, Var_stroke, "#000000"),
    io:put_chars(F, "\t</g>\n").



%% Use Var strole
write_svg_line_group(F, {_Wbb, Hbb}, Ls ,Var_stroke, Line_color_hex) when Var_stroke =:= true ->
    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
		    {Sx,Sy} = sub({X1, Y1}, {X2, Y2}),
		    Dist = math:sqrt(Sx*Sx+Sy*Sy),
		    Qx= X1-Sx*0.07+8, Qy= Y1-Sy*0.25,

		    %% io:format("Dist ~p,~p ~n", [Sx,Sy]),

		    if Dist < 11 ->
			    io:fwrite(F, "\t<path"
				      " d=\"M~.1f,~.1fL~.1f,~.1f\"/>\n",
				      [X2, Hbb - Y2, X1, Hbb - Y1]);
		       true->
			    io:fwrite(F, "\t<path stroke=\"none\" fill=\"~ts\""
				      " d=\"M~.1f,~.1f Q~.1f,~.1f ~.1f,~.1f s~f,~f ~.1f,~.1f z\"/>\n",
				      [Line_color_hex,X2, Hbb - Y2, Qx, Hbb- Qy, X1, Hbb - Y1, Sx*0.1, Sy*0, Sx*-0.15, Sy*0.15])
		    end
	    end, Ls);

%% Use Path element
write_svg_line_group(F, {_Wbb, Hbb}, Ls,_Var_stroke,_Line_color_hex) ->
    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
		    io:fwrite(F, "\t<path"
			      " d=\"M~.1f,~.1fL~.1f,~.1f\"/>\n",
			      [X2, Hbb - Y2, X1, Hbb - Y1])
	    end, Ls).

%% Use Line element
%% write_svg_line_group(F, {_Wbb, Hbb}, Ls) ->
%%    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
%%        io:fwrite(F, "\t<line"
%%            " x1=\"~.1f\" y1=\"~.1f\" x2=\"~.1f\" y2=\"~.1f\"/>\n",
%%            [X2, Hbb - Y2, X1, Hbb - Y1])
%%    end, Ls).


write_svg_footer(F) -> io:put_chars(F, "</svg>\n").


%% {FV2Cs,FP,Mat} = Fs
write_svg_polygons(_F, _BB_size, [],  _Mats_dict,_LSpos) -> ok;
write_svg_polygons(F, {_Wbb, Hbb} = _BB_size, Fs,  Mats_dict, {Shade,Lpos}) ->

    Svg_render_style = case get_pref( svg_render_type, ?DEF_SVG_RENDER_TYPE) of
			   smooth_fill -> " shape-rendering=\"crispEdges\" ";
			   _ -> ""
		       end,
    Stype = get_pref(svg_art_fill_type, ?DEF_SVG_ART_FILL_TYPE),
    io:put_chars(F,"\n<g id=\"faces\" inkscape:label=\"faces\" inkscape:groupmode=\"layer\" stroke=\"none\" " ++ Svg_render_style ++ add_svg_filter(fill,Stype) ++ " xmlns:inkscape=\"http://"++"www.inkscape.org/namespaces/inkscape\">\n"),

    case get_pref(svg_attb_type, ?DEF_SVG_ATTB_TYPE) of
	css_class ->
	    {Class_dict, _Cnt } = foldl(fun( { MatName, T_RGBA }, { Acc_list0, Cnt0 } ) ->
						Cnt = Cnt0 + 1,
						FixName =  "m"++integer_to_list(Cnt),
						Acc_list  = Acc_list0 ++ [{ MatName, FixName, T_RGBA }],
						{Acc_list, Cnt }
					end, {[],0}, Mats_dict),

	    foldl(fun({FV2Cs, FP, Mat},Cnt0) ->
			  Ref_Name = hd(Mat),
			  {  _value, { _Ref, Class_Name ,[{ _bool ,{R,G,B,_A}}] }} = lists:keysearch( Ref_Name, 1, Class_dict ),
			  Option_style = set_face_shade(svg_class, FP, {R,G,B}, Shade,Lpos),
						%io:fwrite(F, "\t<polygon class=\"~ts\"~ts points=\"", [Class_Name,Option_style]),

			  io:fwrite(F, "\t<path class=\"~ts\"~ts d=\"M", [Class_Name,Option_style]),
			  foldl(fun({X, Y},VCnt) ->
					Zend = if VCnt >= length(FV2Cs) - 1 -> "z";true ->" " end,
					io:fwrite(F, "~.2f,~.2f~ts", [X, Hbb - Y, Zend]),
					VCnt + 1
				end, 0, FV2Cs),
			  io:put_chars(F, "\"/>\n"),
			  poly_pb(Cnt0, length(Fs)),
			  Cnt0+1
		  end, 0, Fs);

	compound ->

	    foldl(fun({FV2Cs, FP, Mat0},{PrevMat,PrevNorm,Cnt0}) ->
			  {Norm,_FD}=FP,
			  Mat=hd(Mat0),
			  {_Is_transparent, {R, G, B, A}} = hd(dict__fetch(Mat, Mats_dict)),
			  Colors = set_face_shade(svg_attrb, FP, {R,G,B}, Shade,Lpos),  % shading(pseudo)
			  Ts = (if A < 1.0 -> "shape-rendering:auto;";true->"" end),

			  if Mat =/= PrevMat;Norm =/= PrevNorm ->
				  if Cnt0 > 0 -> io:put_chars(F, "\"/>\n"); true->ok end,
				  if A < 1.0 -> io:fwrite(F, "\t<path style=\"~tsopacity:~.2f;stroke:none;~ts\" d=\"", [Colors, A, Ts]);
				     true;Shade == 0-> io:fwrite(F, "\t<path style=\"~tsstroke:none;\" d=\"", [Colors])
				  end;
			     true ->
				  if A < 1.0 -> io:fwrite(F, "\"/>\n\t<path style=\"~tsopacity:~.2f;stroke:none;~ts\" d=\"", [Colors, A, Ts]); true->"" end
			  end,

			  {_VCnt,Vtxs} = foldl(fun({X, Y},{VCnt,Acc0}) ->
						       Mov = if VCnt == 0 -> "M";true ->"" end,
						       Zend = if VCnt >= length(FV2Cs) - 1 -> "z";true ->"L" end,
						       {VCnt + 1, Acc0 ++ [io_lib:format("~ts~.2f,~.2f~ts", [Mov,X, Hbb - Y,Zend])] }
					       end, {0,[]}, FV2Cs),
			  io:put_chars(F, Vtxs),
			  if Cnt0 >= length(Fs)-1 -> io:put_chars(F, "\"/>\n"); true->ok end,
			  %% io:format("~p,~p ~n",[ Mat,PrevMat]),
			  poly_pb(Cnt0, length(Fs)),
			  CurrentMat = Mat, CurrentNorm = Norm, Cnt = Cnt0+1,
			  {CurrentMat,CurrentNorm,Cnt}
		  end, {"_",{0,0,0},0}, Fs);

	attribute ->
	    foldl(fun({FV2Cs, FP, Mat},Cnt0) ->

			  {_Is_transparent, {R, G, B, A}} = hd(dict__fetch(hd(Mat), Mats_dict)),
			  Colors = set_face_shade(svg_attrb, FP, {R,G,B}, Shade,Lpos),  % shading(pseudo)
			  Ts = (if A < 1.0 -> "shape-rendering:auto;";true->"" end),
			  Opacity = if A < 1.0 -> io_lib:format("opacity:~.2f;",[A]);true->" " end,
			  io:fwrite(F, "\t<path style=\"~ts"++ Opacity ++"stroke:none;~ts\" d=\"M", [Colors, Ts]),

			  foldl(fun({X, Y},VCnt) ->
					Zend = if VCnt >= length(FV2Cs) - 1 -> "z";true ->" " end,
					io:fwrite(F, "~.2f,~.2f~ts", [X, Hbb - Y, Zend]),
					VCnt + 1
				end, 0, FV2Cs),
			  io:put_chars(F, "\"/>\n"),
			  poly_pb(Cnt0, length(Fs)),
			  Cnt0 + 1
		  end,0, Fs);

        _ -> ok
    end,
    io:put_chars(F,"</g>\n").



%% Progress bar
poly_pb(Cnt, Total) when round(Total - Cnt + 1 / 100) rem 100 == 0 ->
    Percent = (Total - (Cnt + 1)) ,
    Percent2 = integer_to_list(round(Percent)),
    Total2 = integer_to_list(Total),
    wings_pb:update(0.5 + Percent * 0.01 * 0.5, "Left " ++ Percent2 ++ " of " ++ Total2 ++" objects"),
    wings_pb:pause(),
    ok;
poly_pb(_, _)->ok.



dict__new() -> [].

dict__append_list(Key0, Values0, KVs) ->
    case keysearch(Key0, 1, KVs) of
        {value, {_Key, Values}} ->
            keyreplace(Key0, 1, KVs, {Key0, Values ++ Values0})
		;
        false ->
            [{Key0, Values0} | KVs]
    end.

dict__append(Key0, Value, KVs) ->
    case keysearch(Key0, 1, KVs) of
        {value, {_Key, Values}} ->
            keyreplace(Key0, 1, KVs, {Key0, [Value | Values]})
		;
        false ->
            [{Key0, [Value]} | KVs]
    end.

dict__store(Key0, Value, KVs) ->
    case keymember(Key0, 1, KVs) of
        true ->
            keyreplace(Key0, 1, KVs, {Key0, [Value]})
		;
        false ->
            [{Key0, [Value]} | KVs]
    end.

dict__fetch(Key, KVs) ->
    {value, {_Key, Values}} = keysearch(Key, 1, KVs),
    Values.

tree__empty() -> {0, gb_trees:empty()}.

%% tree__insert([], Tree_acc) -> Tree_acc;
%% tree__insert([Value | T], Tree_acc) ->
%%     tree__insert(T, tree__insert(Value, Tree_acc));
tree__insert(Value, {Next_key, GBtree} = _Tree) ->
    {Next_key + 1, gb_trees:insert(Next_key, Value, GBtree)}.

tree__get([], _Tree, Values_acc) -> lists:reverse(Values_acc);
tree__get([Key | T], Tree, Values_acc) ->
    tree__get(T, Tree, [tree__get(Key, Tree) | Values_acc]).

tree__get(Keys, Tree) when is_list(Keys) -> tree__get(Keys, Tree, []);
tree__get(Key, {_Next_key, GBtree} = _Tree) -> gb_trees:get(Key, GBtree).

%% tree__last_key({Next_key, _GBtree} = _Tree) -> Next_key - 1.

tree__next_key({Next_key, _GBtree} = _Tree) -> Next_key.

						%tree__from_list(Values) when is_list(Values) ->
%%    tree__insert(Values, tree__empty()).

%% tree__to_list({_Next_key, GBtree} = _Tree) -> gb_trees:values(GBtree).

%% tree__do_map(_Fun, none, GBtree_acc0) -> GBtree_acc0;
%% tree__do_map(Fun, {Key, Value, GBtree_iter0}, GBtree_acc0) ->
%%    GBtree_acc = gb_trees:insert(Key, Fun(Value), GBtree_acc0),
%%    tree__do_map(Fun, gb_trees:next(GBtree_iter0), GBtree_acc).

						%tree__map(Fun, {Next_key, GBtree} = _Tree) ->
%%    Iter = gb_trees:iterator(GBtree),
%%    {Next_key, tree__do_map(Fun, gb_trees:next(Iter), gb_trees:empty())}.

tree__do_fold(_Fun, Acc0, none) -> Acc0;
tree__do_fold(Fun, Acc0, {Key, Value, GBtree_iter0}) ->
    tree__do_fold(Fun, Fun(Key, Value, Acc0), gb_trees:next(GBtree_iter0)).

tree__fold(Fun, Acc, {_Next_key, GBtree} = _Tree) ->
    Iter = gb_trees:iterator(GBtree),
    tree__do_fold(Fun, Acc, gb_trees:next(Iter)).

frustum_planes({{Xmin, Ymin, Zmin}, {Xmax, Ymax, Zmax}}) ->
    Left = {{1.0, 0.0, 0.0}, Xmin},
    Bottom = {{0.0, 1.0, 0.0}, Ymin},
    Far = {{0.0, 0.0, 1.0}, Zmin},
    Right = {{-1.0, 0.0, 0.0}, -Xmax},
    Top = {{0.0, -1.0, 0.0}, -Ymax},
    Near = {{0.0, 0.0, -1.0}, -Zmax},
    [Left, Bottom, Far, Right, Top, Near];
frustum_planes({HSx, HSy, Zfar, Znear, Zf}) ->
    Zf2 = Zf * Zf,
    Dx = math:sqrt(HSx * HSx + Zf2),
    Xrl = Zf / Dx,
    Zrl = -HSx / Dx,
    Dy = math:sqrt(HSy * HSy + Zf2),
    Ytb = Zf / Dy,
    Ztb = -HSy / Dy,
    Left = {{-Xrl, 0.0, Zrl}, 0.0},
    Bottom = {{0.0, -Ytb, Ztb}, 0.0},
    Far = {{0.0, 0.0, 1.0}, Zfar},
    Right = {{Xrl, 0.0, Zrl}, 0.0},
    Top = {{0.0, Ytb, Ztb}, 0.0},
    Near = {{0.0, 0.0, -1.0}, -Znear},
    [Left, Bottom, Far, Right, Top, Near].

splitpoly__position(VD_prev, VD_cur, VD_next, {PDm, _PDp})
  when VD_cur >= PDm, VD_prev >= VD_cur, VD_next >= VD_cur -> front;
splitpoly__position(VD_prev, VD_cur, VD_next, {_PDm, PDp})
  when VD_cur =< PDp, VD_prev =< VD_cur, VD_next =< VD_cur -> back;
splitpoly__position(_VD_prev, VD_cur, VD_next, {PDm, PDp})
  when VD_cur >= PDp, VD_next =< PDm -> {point, front};
splitpoly__position(_VD_prev, VD_cur, VD_next, {PDm, PDp})
  when VD_cur =< PDm, VD_next >= PDp -> {point, back};
splitpoly__position(VD_prev, VD_cur, VD_next, {PDm, PDp})
  when VD_cur >= PDm, VD_cur =< PDp ->
    {vertex,
     if
	 VD_prev >= VD_cur; VD_next =< VD_cur -> front;
	 true -> back
     end};
splitpoly__position(_VD_prev, _VD_cur, _VD_next, _PDt) -> undef.

splitpoly__get_vc(VC, _VC_tree) when is_tuple(VC) -> VC;
splitpoly__get_vc(VI, VC_tree) -> ctree__get(VI, VC_tree).

splitpoly__insert_vc(Vs, VC_tree0) when is_list(Vs) ->
    {VIs, VC_tree} = lists:foldl(fun(V, {VIs_acc0, VC_tree_acc0}) ->
					 {VI, VC_tree_acc} = splitpoly__insert_vc(V, VC_tree_acc0),
					 {[VI | VIs_acc0], VC_tree_acc}
				 end, {[], VC_tree0}, Vs),
    {lists:reverse(VIs), VC_tree};
splitpoly__insert_vc(VC, VC_tree0) when is_tuple(VC)->
    {ctree__next_key(VC_tree0), ctree__insert(VC, VC_tree0)};
splitpoly__insert_vc(VI, VC_tree0)->
    {VI, VC_tree0}.

splitpoly__do_find(_P, _PDt, [V_last], SVs, VDt, _VC_tree) ->
    {V_last, SVs, VDt};
splitpoly__do_find({PN, _PD} = P, PDt,
		   [V_cur | T] = _Vs, SVs0, {VD_prev, VD_cur} = _VDt0, VC_tree0) ->
    V_next = hd(T),
    VC_next = splitpoly__get_vc(V_next, VC_tree0),
    VD_next = dot(VC_next, PN),
    VDt = {VD_cur, VD_next},
						%io:format("\tV_cur=~w~n\tV_next=~w~n\tT=~w~n\tSVs0=~w~n\tVDp=~w~n\tVDc=~w~n\tVDn=~w~n\tPDt=~w~n~n", [V_cur, V_next, T, SVs0, VD_prev, VD_cur, VD_next, PDt]),
    case splitpoly__position(VD_prev, VD_cur, VD_next, PDt) of
        undef ->
            splitpoly__do_find(P, PDt, T, [V_cur | SVs0], VDt, VC_tree0)
		;
        {vertex, Pos} ->
            {Pos, T, V_cur, SVs0, VDt}
		;
        {point, Pos} ->
            VC_cur = splitpoly__get_vc(V_cur, VC_tree0),
            VC_new = ip(VC_cur, VC_next, P),
            {Pos, T, VC_new, [V_cur | SVs0], VDt}
		;
        Pos -> Pos
    end.

splitpoly__find(P, PDt, Hs, Vs0, VDt0, VC_tree) ->
    case splitpoly__do_find(P, PDt, Vs0, [], VDt0, VC_tree) of
        {V_last, SVs, VDt} ->
            splitpoly__do_find(P, PDt, [V_last | Hs], SVs, VDt, VC_tree)
		;
        R -> R
    end.

splitpoly__combine([], SVs2, Vs2) ->
    {Vs2, lists:reverse(SVs2)};
splitpoly__combine(SVs1, SVs2, Vs2) ->
    {Vs2 ++ tl(lists:reverse(SVs1)), lists:reverse(SVs2)}.

splitpoly__combine(SVs1, SVs2, [_V1, V2] = Vs2, [_H1, H2] = _Hs)
  when V2 =:= H2 ->
    splitpoly__combine(SVs1, SVs2, Vs2);
splitpoly__combine(SVs1, SVs2, [V1] = Vs2, [_H1, H2] = _Hs) when V1 =:= H2 ->
    splitpoly__combine(SVs1, SVs2, Vs2);
splitpoly__combine(SVs1, SVs2, Vs2, Hs) ->
    splitpoly__combine(SVs1, SVs2, Vs2 ++ Hs).

splitpoly__combine(front, P1, SVs1, back, P2, SVs2, Vs2, Hs) ->
    {FFVs, BFVs} = splitpoly__combine(SVs1, SVs2, Vs2, Hs),
    {P2, P1, FFVs, BFVs};
splitpoly__combine(_Pos1, P1, SVs1, _Pos2, P2, SVs2, Vs2, Hs) ->
    {FFVs, BFVs} = splitpoly__combine(SVs1, SVs2, Vs2, Hs),
    {P1, P2, BFVs, FFVs}.

splitpoly__do_split({PN, PD} = P, [V1, V2 | _] = Vs0, VC_tree) ->
    EpsD = ?EPS1 * (1.0 + abs(PD)), % ?????????????????????????????????????????????????????????????
    PDt = {PD - EpsD, PD + EpsD},
    VC1 = splitpoly__get_vc(V1, VC_tree),
    VD1 = dot(VC1, PN),
    VC2 = splitpoly__get_vc(V2, VC_tree),
    VD2 = dot(VC2, PN),
    Hs = [V1, V2],
    case splitpoly__find(P, PDt, Hs, tl(Vs0), {VD1, VD2}, VC_tree) of
        {Pos1, Vs1, IP1, SVs1, VDt1} ->
						%io:format("Pos1=~w~nHs=~w~nVs1=~w~nIP1=~w~nSVs1=~w~nVDt1=~w~n~n", [Pos1, Hs, Vs1, IP1, SVs1, VDt1]),
            case splitpoly__find(P, PDt, Hs, Vs1, VDt1, VC_tree) of
                {Pos2, Vs2, IP2, SVs2, _VDt2} ->
						%io:format("Pos2=~w~nHs=~w~nVs2=~w~nIP2=~w~nSVs2=~w~n~n~n", [Pos2, Hs, Vs2, IP2, SVs2]),
                    splitpoly__combine(Pos1, IP1, SVs1, Pos2, IP2, SVs2, Vs2, Hs)
			;
                Pos2 ->
						%io:format("miss2=~w~n~n", [Pos2]),
                    Pos2
            end
		;
        Pos1 ->
						%io:format("miss1=~w~n~n", [Pos1]),
            Pos1
    end.

splitpoly__split(P, VIs0, VC_tree0) ->
    case splitpoly__do_split(P, VIs0, VC_tree0) of
        {IP1, IP2, FFVs, BFVs} ->
            {IPI1, VC_tree1} = splitpoly__insert_vc(IP1, VC_tree0),
            {IPI2, VC_tree} = splitpoly__insert_vc(IP2, VC_tree1),
            {[IPI1 | FFVs] ++ [IPI2], [IPI2 | BFVs] ++ [IPI1], VC_tree}
		;
        R -> R
    end.

splitpoly__do_clip([] = _BPs, Vs0, _VC_tree) -> Vs0;
splitpoly__do_clip([BP | T] = _BPs, Vs0, VC_tree) ->
						%io:format("BP=~w~n~n", [BP]),
						%io:format("Vs0=~w~n~n", [Vs0]),
    case splitpoly__do_clip(BP, Vs0, VC_tree) of
        Vs when is_list(Vs) -> splitpoly__do_clip(T, Vs, VC_tree);
        R -> R
    end;
splitpoly__do_clip(BP, Vs0, VC_tree) ->
    case splitpoly__do_split(BP, Vs0, VC_tree) of
        {IP1, IP2, FFVs, _BFVs} -> [IP1 | FFVs] ++ [IP2];
        front -> Vs0;
        _ -> nil
    end.

splitpoly__clip(BPs, VIs0, VC_tree0) ->
    case splitpoly__do_clip(BPs, VIs0, VC_tree0) of
        Vs when is_list(Vs) -> splitpoly__insert_vc(Vs, VC_tree0);
        R -> R
    end.

%% ztree__empty() -> nil.

						%ztree__insert(Obj, Obj_zlim, nil) -> {[Obj], Obj_zlim, nil, nil};
						%ztree__insert(Obj, {Obj_zmin, Obj_zmax} = Obj_zlim,
%%    {Objs, {Node_zmin, Node_zmax} = Node_zlim, Near, Far}) ->
%%    if
%%        Obj_zmin >= Node_zmax ->
%%            {Objs, Node_zlim, ztree__insert(Obj, Obj_zlim, Near), Far}
%%            ;
%%        Obj_zmax =< Node_zmin ->
%%            {Objs, Node_zlim, Near, ztree__insert(Obj, Obj_zlim, Far)}
%%            ;
%%        true ->
%%% min max
%%            {[Obj | Objs],
%%                Node_zlim,
%%                {get_min(Obj_zmin, Node_zmin), get_max(Obj_zmax, Node_zmax)},
%%                Near, Far}
%%    end.

						%ztree__to_list(nil, List) -> List;
						%ztree__to_list({Objs, _Node_zlim, Near, Far}, List) ->
%%    ztree__to_list(Far, Objs ++ ztree__to_list(Near, List)).
						%ztree__to_list(Ztree) -> ztree__to_list(Ztree, []).


ctree__empty(Proj) -> {Proj, tree__empty()}.

ctree__insert(VCs, {Proj, Tree0}=_VC_tree0) when is_list(VCs) ->
    {Proj, lists:foldl(fun(VC, Tree_acc0) ->
			       tree__insert({VC, Proj(VC)}, Tree_acc0)
		       end, Tree0, VCs)};
ctree__insert(VC, {Proj, Tree0}=_VC_tree0) ->
    {Proj, tree__insert({VC, Proj(VC)}, Tree0)}.

ctree__get(VIs, {_Proj, Tree}=_VC_tree) when is_list(VIs) ->
    [VC || {VC, _V2C} <- tree__get(VIs, Tree)];
ctree__get(VI, {_Proj, Tree}=_VC_tree) ->
    {VC, _V2C} = tree__get(VI, Tree), VC.

ctree__get2d(VIs, {_Proj, Tree}=_VC_tree) when is_list(VIs) ->
    [V2C || {_VC, V2C} <- tree__get(VIs, Tree)];
ctree__get2d(VI, {_Proj, Tree}=_VC_tree) ->
    {_VC, V2C} = tree__get(VI, Tree), V2C.

%% ctree__last_key({_Proj, Tree}=_VC_tree) -> tree__last_key(Tree).

ctree__next_key({_Proj, Tree}=_VC_tree) -> tree__next_key(Tree).

ctree__from_list(Proj, Values) when is_list(Values) ->
    ctree__insert(Values, ctree__empty(Proj)).



%%  This function return true / false
%%  AVCs && BVCs  =  [ true or false ]   AVCs is Front etc...
intersect(AVCs, BVCs, Eps) ->
    lists:all(fun({AVC1, AVC2}) ->
		      DA = sub(AVC2, AVC1),
		      lists:any(fun(BVC) -> cross(DA, sub(BVC, AVC2)) >= Eps end, BVCs)
	      end, pair(AVCs)) andalso
	lists:all(fun({BVC1, BVC2}) ->
			  DB = sub(BVC2, BVC1),
			  lists:any(fun(AVC) -> cross(DB, sub(AVC, BVC2)) =< Eps end, AVCs)
		  end, pair(BVCs)).





bspt__empty() -> nil.

bspt__insert(FI0, F0, FZt0, nil, VC_tree0) -> {{FI0, F0, FZt0, nil, nil}, VC_tree0};
bspt__insert(FI0, {FVIs0, FP0, Mat0} = F0, FZt0,
	     {FI, {FVIs, FP, _Mat} = F, FZt, Back_bspt0, Front_bspt0}, VC_tree0) ->
    case splitpoly__split(FP, FVIs0, VC_tree0) of
        {Front_VIs, Back_VIs, VC_tree} ->
            FV2Cs0 = ctree__get2d(FVIs0, VC_tree),
            FV2Cs = ctree__get2d(FVIs, VC_tree),





	    %% Output Priority
	    %% Scene  (Output for scene mainly made many sliced polygons,and file size larger.)
	    %%   intersect() true  false -> BAD Z order(for complex compotision ), but circular object OK  Default

	    %% Path (Divide nice shapes and small size,but messed if it has overlapped objects.)
	    %%    intersect() false true -> Good Z order, but circular object NG

	    Result = intersect(FV2Cs0, FV2Cs, ?EPS2 * ?EPS2),

	    Division_priority = get_pref(division_priority, ?DEF_DIVISION_PRIORITY),
	    Overlapped =  case Division_priority of
			      scene -> not Result;
			      path -> Result;
			      _ -> false
			  end,

	    %%  io:format("Overlap: ~p ~n",[Overlap_priority]),

	    %% Front_VCs = ctree__get(Front_VIs, VC_tree),
	    %% Back_VCs = ctree__get(Back_VIs, VC_tree),
	    %% {Front_zmin, Front_zmax} = Front_Zt = face_zlim(Front_VCs),
	    %% {Back_zmin, Back_zmax} = Back_Zt = face_zlim(Back_VCs),

	    %% {FZmin0, FZmax0} = FZt0,
	    %% {FZmin, FZmax} = FZt,

            case Overlapped of
                true ->

		    {Back_bspt, VC_tree1} =
			bspt__insert(nil, {Back_VIs, FP0, Mat0}, {0.0, 0.0}, Back_bspt0, VC_tree),
		    {Front_bspt, VC_tree2} =
			bspt__insert(nil, {Front_VIs, FP0, Mat0}, {0.0, 0.0}, Front_bspt0, VC_tree1),
		    {{FI, F, FZt, Back_bspt, Front_bspt}, VC_tree2}
			;
                false ->
                    {Back_bspt, VC_tree1} =
                        bspt__insert(FI0, {Back_VIs, FP0, Mat0}, {0.0, 0.0}, Back_bspt0, VC_tree),
                    {Front_bspt, VC_tree2} =
                        bspt__insert(FI0, {Front_VIs, FP0, Mat0}, {0.0, 0.0}, Front_bspt0, VC_tree1),
                    {{FI, F, FZt, Back_bspt, Front_bspt}, VC_tree2}
            end

		;
        back ->
            {Back_bspt, VC_tree} =
                bspt__insert(FI0, F0, FZt0, Back_bspt0, VC_tree0),
            {{FI, F, FZt, Back_bspt, Front_bspt0}, VC_tree}
		;
        front ->
            {Front_bspt, VC_tree} =
                bspt__insert(FI0, F0, FZt0, Front_bspt0, VC_tree0),
            {{FI, F, FZt, Back_bspt0, Front_bspt}, VC_tree}
    end.

get_fis(nil, FVIs,_Face_tree) -> FVIs;
get_fis(FI, _FVIs, Face_tree) ->
    {FVIs, _FP, _Mat} = tree__get(FI, Face_tree), FVIs.

bspt__to_list(nil, List) -> List;
bspt__to_list({FI, F, _FZt, Back_bspt, Front_bspt}, List) ->
    bspt__to_list(Back_bspt, [{FI, F} | bspt__to_list(Front_bspt, List)]).

bspt__to_list(Bspt) ->
    FEs = bspt__to_list(Bspt, []),
    {FEs_rev, _Dup_set} =
	lists:foldl(fun
			({nil, _F} = FE, {FEs_acc0, Dup_set_acc0}) ->
			   {[FE | FEs_acc0], Dup_set_acc0};
			({FI, _F} = FE, {FEs_acc0, Dup_set_acc0}) ->
			   case gb_sets:is_member(FI, Dup_set_acc0) of
			       true ->
						%io:format("Removed: FI=~w~n", [FI]),
				   {FEs_acc0, Dup_set_acc0}
				       ;
			       false ->
				   Dup_set_acc = gb_sets:insert(FI, Dup_set_acc0),
				   {[FE | FEs_acc0], Dup_set_acc}
			   end
		   end, {[], gb_sets:empty()}, FEs),
    lists:reverse(FEs_rev).


sign(V) when V > 0.0 -> 1.0;
sign(V) when V < 0.0 -> -1.0;
sign(V) when V =:= 0.0 -> 0.0.

is_convex(FVCs, _Is_FF) when length(FVCs) =:= 3 -> true;
is_convex([FVC1 | T], Is_FF) ->
    FVC2 = hd(T),

    Dir = case Is_FF of true -> 1.0; false -> -1.0 end,
    is_convex(sub(FVC2, FVC1), T ++ [FVC1, FVC2], Dir).

is_convex(_V12, [_FVC2], _Dir0) -> true;
is_convex(V12, [FVC2 | T], Dir0) ->
    V23 = sub(hd(T), FVC2),
    Dir = sign(cross(V12, V23)),

    if
        Dir =/= Dir0 ->
            false
		;
        true ->
            is_convex(V23, T, Dir)
    end.

is_flat({FN, FD}, FVCs) ->
    EpsD = ?EPS1 * abs(FD),
    lists:all(fun(FVC) -> abs(FD - dot(FN, FVC)) < EpsD end, FVCs).
