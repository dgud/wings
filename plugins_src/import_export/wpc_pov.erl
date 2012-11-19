%%  wpc_pov.erl --
%%
%%     POV-Ray Plugin User Interface.
%%
%%  Copyright (c) 2007-2011 Chris Hegarty
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_pov).

-export([init/0,menu/2,command/2,dialog/2]).

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").

-define(TAG, povray36).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, povray36_render).

-define(DEF_RENDERER, "c:/program files/POV-Ray for Windows v3.6/bin/pvengine.exe").
-define(DEF_LINUX_RENDERER, "povray").
-define(DEF_DIALOGS, auto).
-define(DEF_RENDER_FORMAT, png).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_RENDERARGS, "/NR /EXIT").
-define(DEF_LINUX_RENDERARGS, " ").

-define(MAX_VERTEX, 10).

-record(camera_info, {pos,dir,up,fov}).

key(Key) -> {key, ?KEY(Key)}.
exkey(Key) ->{key, Key}.

colorlevel(M) ->key({colorlevel, M}).
colorvalue(M) ->key({colorvalue, M}).
colordelete(M) ->key({colordelete, M}).
normallevel(M) ->key({normallevel, M}).
normalvalue(M) ->key({normalvalue, M}).
normaldelete(M) ->key({normaldelete, M}).

range(T) -> {range,T}.

%Gets if os is win32
is_windows()->
	T = os:type(),
	L = tuple_to_list(T),
	lists:member(win32, L).
	
export_matrix()->
	e3d_mat:scale(-1.0, 1.0, 1.0).
	
export_transform(Contents) ->
    e3d_file:transform(Contents, export_matrix()).
	
export_transform_pos(Pos)->
    e3d_mat:mul_point(export_matrix(), Pos).
	
export_transform_vec(Vec)->
    e3d_mat:mul_vector(export_matrix(), Vec).

%initialize plugin with plugin preferences
init() ->
    init_pref(),
    set_var(rendering, false),
    true.

%retrieve plugin preferences from prefernce file and store as global variables
init_pref() ->
	case is_windows() of
		true-> LocalRenderer = ?DEF_RENDERER;
		false-> LocalRenderer = ?DEF_LINUX_RENDERER
	end,
	
    Renderer = get_pref(renderer, LocalRenderer),
    RendererPath = case Renderer of
    	[] -> false;
	_ -> Renderer
    end,
    case get_pref(dialogs, ?DEF_DIALOGS) of
	auto ->
	    set_var(renderer, RendererPath),
	    set_var(dialogs, case RendererPath of 
				 false -> false; 
				 _ -> true 
			     end);
	enabled ->
	    set_var(renderer, RendererPath),
	    set_var(dialogs, true);
	disabled ->
	    set_var(renderer, false),
	    set_var(dialogs, false)
    end,
    set_var(use_emit_ambient, get_pref(use_emit_ambient, true)),
	
	case is_windows() of
		true-> LocalArgs = ?DEF_RENDERARGS;
		false-> LocalArgs = ?DEF_LINUX_RENDERARGS
	end,
	set_var(renderargs, get_pref(renderargs, LocalArgs)),
	set_var(limit_vertices, get_pref(limit_vertices, true)),
	set_var(use_model_dim, get_pref(use_model_dim, false)),
    ok.
    
%insert menu items into export, export selected, render, and plugin preferences
menu({file,export}, Menu) ->
    case get_var(dialogs) of
    	false -> Menu;
	_ -> povray_menu(Menu)
    end;
menu({file,export_selected}, Menu) ->
    case get_var(dialogs) of
    	false -> Menu;
	_ -> povray_menu(Menu)
    end;
menu({file,render}, Menu) ->
    case {get_var(renderer),Menu} of
	{_,[plugin_manager_category]} -> povray_menu(Menu);
    	{false,_} -> Menu;
	{_,_} -> povray_menu(Menu)
    end;
menu({edit, plugin_preferences}, Menu) ->
	Menu++[{?__(1,"POV-Ray"),?TAG}];
menu(_, Menu) ->
    Menu.

povray_menu(Menu) ->
    Menu ++ [{?__(1,"POV-Ray (.pov)"),?TAG,[option]}].

%dialog and file type properties    
props(render, Attr) ->
    RenderFormat = proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    {value, {RenderFormat, Ext, Desc}} = lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
    Title = case os:type() of
        {win32,_} -> "Render";
        _Other    -> ?__(1,"Render")
	end,
    [{title,Title}, {ext,Ext}, {ext_desc,Desc}];
props(export, _Attr) ->
    {Title,File} = case os:type() of
        {win32,_} -> {"Export","POV-Ray File"};
        _Other    -> {?__(2,"Export"),?__(3,"POV-Ray File")}
	end,
    [{title,Title},{ext,".pov"},{ext_desc,File}];
props(export_selected, _Attr) ->
    {Title,File} = case os:type() of
        {win32,_} -> {"Export","POV-Ray File"};
        _Other    -> {?__(4,"Export Selected"),?__(3,"POV-Ray File")}
	end,
    [{title,Title},{ext,".pov"},{ext_desc,File}].

%menu commands
command({file,{export,{?TAG,A}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(A, export, Exporter, St);
command({file,{export_selected,{?TAG,A}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(A, export_selected, Exporter, St);
command({file,{render,{?TAG,A}}}, St) ->
     Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(A, render, Exporter, St);
command({edit,{plugin_preferences,?TAG}}, St) ->
    pref_dialog(St);
command(_Spec, _St) ->
    next.

%Material / Light Dialogs
dialog({material_editor_setup, Name, Mat}, Dialog) ->
	case get_var(dialogs) of
		false-> Dialog;
		_ -> Dialog++material_dialog(Name, Mat)
	end;
dialog({material_editor_result, Name, Mat}, Res) ->
    case get_var(dialogs) of
		false -> {Mat,Res};
		_ -> material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup, Name, Ps}, Dialog) ->
	case get_var(dialogs) of
		false-> Dialog;
		_ -> Dialog++light_dialog(Name, Ps)
    end;
dialog({light_editor_result, Name, Ps}, Res) ->
    case get_var(dialogs) of
		false -> {Ps, Res};
		_ -> light_result(Name, Ps, Res)
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.


%Create Preferences dialog
pref_dialog(St) ->
	case is_windows() of
		true-> LocalRenderer = ?DEF_RENDERER,
			LocalArgs = ?DEF_RENDERARGS;
		false-> LocalRenderer = ?DEF_LINUX_RENDERER,
			LocalArgs = ?DEF_LINUX_RENDERARGS
	end,
	
	[{dialogs, Dialogs}, {renderer, Renderer}, {use_emit_ambient, UseEmitAmbient}, 
	{renderargs, RenderArgs}, {limit_vertices, LimitVertices}, {use_model_dim, UseModelDim}] = 
		get_user_prefs([{dialogs,?DEF_DIALOGS},{renderer, LocalRenderer}, {use_emit_ambient,true},
		{renderargs, LocalArgs}, {limit_vertices, true}, {use_model_dim, false}]),

    Dialog =
		[{vframe, [
			{hframe, [
				{menu,[{?__(1,"Disabled Dialogs"),disabled},
					{?__(2,"Automatic Dialogs"),auto},
					{?__(3,"Enabled Dialogs"),enabled}
				], Dialogs, [{key, dialogs}]}
			]},
			{hframe,[ {label,?__(4,"Executable")}, {button, {text, Renderer, [{key,renderer}, wings_job:browse_props()]}}]},
			{hframe, [ {label, ?__(5,"Arguments")}, {text, RenderArgs, [{key, renderargs}]}]},
			{?__(6,"Fix camera dimensions to model view dimensions"), UseModelDim, [{key, use_model_dim}]},
			{?__(7,"Use material Emit for POV-Ray Ambient"), UseEmitAmbient, [{key, use_emit_ambient}]},
			{?__(8,"Limit number of vertices, indices per line"), LimitVertices, [{key, limit_vertices}]}
		]}],
    wpa:dialog(?__(9,"POV-Ray Options"), Dialog, fun (Attr) -> pref_result(Attr,St) end).
pref_result(Attr, St) ->
    set_user_prefs(Attr),
    init_pref(),
    St.

%Add necessary properties to lists, setup export function
do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"POV-Ray Export Options"), export_dialog(Op),
	       fun(Res) ->
		       {file, {Op, {?TAG, Res}}}
	       end);
do_export(Attr, Op, Exporter, St) when is_list(Attr) ->
    set_prefs(Attr),
	
	%Basic additional settings
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Tesselation = proplists:get_value(tesselation, Attr, none),
    ExportUV = proplists:get_value(include_uvs, Attr, true),
    ExportVC = proplists:get_value(include_colors, Attr, true),
    Ps = [{tesselation,Tesselation},{subdivisions,SubDivs},
	  {include_uvs,ExportUV},{include_colors,ExportVC}|props(Op, Attr)],
	%Add Camera, lights to list
	[{Pos, Dir, Up}, Fov] = wpa:camera_info([pos_dir_up, fov]),
	Pos1 = export_transform_pos(Pos),
	Dir1 = export_transform_vec(Dir),
	Up1 = export_transform_vec(Up),
	CameraInfo = #camera_info{pos=Pos1, dir=Dir1, up=Up1, fov=Fov},
	CL_Attr = [CameraInfo, {lights, wpa:lights(St)}, {operation, Op} | Attr],
	
	case Op of
		render -> R_Attr = [{?TAG_RENDER, true} | CL_Attr];
		_ -> R_Attr = CL_Attr
	end,
	
    Exporter(Ps, export_fun(R_Attr)).
	
export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export(Filename, Contents, Attr)
    end.

%Open files, do the actual export, check for render operation and launch render if necessary	
export(Filename, Contents, Attr) ->
 	wpa:popup_console(),
    ExportTS = erlang:now(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
	RenderFormat = proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    ExportDir = filename:dirname(Filename),
	
	ContentsXForm = export_transform(Contents),
	#e3d_file{objs=Objs, mat=Mats, creator=Creator} = ContentsXForm,
	
	case Render of
		false -> ExportFile = Filename;
		true ->ExportFile = filename:rootname(Filename)++?__(1,"_export.pov")
	end,
	
	case get_var(use_model_dim) of
		false ->
			Width = proplists:get_value(width, Attr, 320),
			Height = proplists:get_value(height, Attr, 240);
		_ ->
			%get the focus window.  If it isn't a sub geom window, use the primary geom window
			case wings_wm:actual_focus_window() of
				{geom, N}-> {Width, Height} = wings_wm:win_size({geom, N});
				_ -> {Width, Height} = wings_wm:win_size(geom)
			end
	end,
	
	#camera_info{fov=Fov} = proplists:lookup(camera_info, Attr),
	Depth = (float(Height) / 2.0) / math:tan((Fov / 2.0) * math:pi() / 180.0),
	CorrectedFOV = 2.0 * math:atan((float(Width) / 2.0) / Depth) * 180.0 / math:pi(), 
	
	{ok, F} = file:open(ExportFile, [write]),
	
	io:format(F, ?__(2,"// ~s: Exported from ~s \n\n"), [filename:basename(ExportFile), Creator]),
	io:put_chars(F, "#include \"rad_def.inc\"\n\n"),
	
	io:put_chars(F, "global_settings{\n"),
	export_global(F, Attr),
	io:put_chars(F, "}\n"),
	{Br, Bg, Bb} = proplists:get_value(background, Attr, {0.0, 0.0, 0.0}),
	io:format(F, "background { rgb <~f, ~f, ~f> }\n", [Br, Bg, Bb]),
	
	export_interior(F, Attr),
	export_camera(F, Attr, CorrectedFOV, Width, Height),
	
	Lights = proplists:get_value(lights, Attr, []),
	export_lights(F, Lights, 0),
	
	export_materials(F, Mats, Attr, ExportDir),
	export_objects(F, Objs, Mats, Attr, 0),
	export_lights_objects(F, Lights, 0),
			
	file:close(F),
	
	
    case {get_var(renderer), Render} of
		{_, false} ->
			wings_job:export_done(ExportTS),
			io:nl();
		{false, true} ->
			io:put_chars(?__(3,"PovRay executable path not set\n")),
			wings_job:export_done(ExportTS),
			io:nl();
		{Renderer, true} ->
			ArgStr = wings_job:quote(filename:basename(ExportFile))++" +W"++wings_job:quote(integer_to_list(Width))++
				" +H"++wings_job:quote(integer_to_list(Height))++
				" +FN +o"++wings_job:quote(filename:basename(Filename))++
				case proplists:get_value(antialias, Attr, false) of
					false->[];
					true->" +A"++ wings_util:nice_float(proplists:get_value(aa_threshold, Attr, 0.3))++
						" +R"++integer_to_list(proplists:get_value(aa_depth, Attr, 1))++
						case proplists:get_value(aa_jitter, Attr, false) of
							false->[];
							true -> " +J"
						end
				end,
				
			RA = get_var(renderargs),
				
			PortOpts = [{cd,filename:dirname(ExportFile)}],
			Handler =
				fun (Status) ->
					set_var(rendering, false),
					case Status of
						ok -> {RenderFormat, Filename};
						_  -> Status
					end
				end,
				file:delete(Filename),
				set_var(rendering, true),
				wings_job:render(ExportTS, Renderer, ArgStr++" "++RA, PortOpts, Handler)
    end.

export_global(F, Attr)->
	io:format(F, "\t assumed_gamma ~f\n", [2.2 / proplists:get_value(assumed_gamma, Attr, 1.0)]),
	io:format(F, "\t max_trace_level ~p\n", [proplists:get_value(max_trace_level, Attr, 5)]),
	{Ar, Ag, Ab} = proplists:get_value(ambient, Attr, {0.0, 0.0, 0.0}),
	Ap = proplists:get_value(ambient_power, Attr, 1.0),
	io:format(F, "\t ambient_light rgb <~f, ~f, ~f>\n", [Ar * Ap, Ag * Ap, Ab * Ap]),
	Radiosity = proplists:get_value(radiosity, Attr, none),
	case Radiosity of
		none ->ok;
		_ -> io:format(F, "\t radiosity { Rad_Settings(~s, off, off) }\n", [Radiosity])
	end,
	case proplists:get_value(photons, Attr, false) of
		false -> ok;
		true ->
			io:put_chars(F, "\t photons {\n"),
			io:format(F, "\t\t count ~p\n", [proplists:get_value(photon_count, Attr, 10000)]),
			case proplists:get_value(media_photons, Attr, false) of
				false -> ok;
				true -> io:format(F, "\t\t media ~p, 2\n", [proplists:get_value(media_photons_count, Attr, 100)])
			end,
			io:put_chars(F, "\t }\n")
	end.
export_interior(F, Attr)->
	Fog = proplists:get_value(fog, Attr, false),
	Media = proplists:get_value(media, Attr, false),
	
	case Fog of
		false ->ok;
		true ->io:put_chars(F, "fog {\n"),
			io:format(F, "\t fog_type ~p\n", [proplists:get_value(fog_type, Attr, 1)]),
			io:format(F, "\t distance ~f\n", [proplists:get_value(fog_distance, Attr, 0.0)]),
			{Fr, Fg, Fb, Fa} = proplists:get_value(fog_color, Attr, {0.0, 0.0, 0.0, 0.0}),
			io:format(F, "\t rgbf <~f, ~f, ~f, ~f>\n",[Fr, Fg, Fb, 1.0 - Fa]),
			case proplists:get_value(fog_type, Attr, 1) of
				1 -> ok;
				2 -> io:format(F, "\t fog_offset ~f\n", [proplists:get_value(fog_offset, Attr, 0.0)]),
					io:format(F, "\t fog_alt ~f\n", [proplists:get_value(fog_alt, Attr, 0.0)])
			end,
			io:put_chars(F, "}\n")
	end,
	
	case Media of
		false -> ok;
		true -> io:put_chars(F, "media {\n"),
			io:format(F, "\t method ~p\n", [proplists:get_value(media_method, Attr, 1)]),
			io:format(F, "\t intervals ~p\n", [proplists:get_value(media_intervals, Attr, 1)]),
			io:format(F, "\t samples ~p\n", [proplists:get_value(media_samples, Attr, 1)]),
			{Ar, Ag, Ab} = proplists:get_value(absorbtion, Attr, {0.0, 0.0, 0.0}),
			io:format(F, "\t absorption rgb <~f, ~f, ~f>\n", [Ar, Ag, Ab]),
			{Er, Eg, Eb} = proplists:get_value(emission, Attr, {0.0, 0.0, 0.0}),
			io:format(F, "\t emission rgb <~f, ~f, ~f>\n", [Er, Eg, Eb]),
			{Sr, Sg, Sb} = proplists:get_value(scattering, Attr, {0.0, 0.0, 0.0}),
			io:format(F, "\t scattering { ~p rgb <~f, ~f, ~f> extinction 0.0 }\n", [proplists:get_value(scattering_type, Attr, 1), Sr, Sg, Sb]),
			io:put_chars(F, "}\n")
	end.
			
export_camera(F, Attr, CorrectedFOV, Width, Height)->
	
	#camera_info{pos=Pos,dir=Dir,up=Up} = proplists:lookup(camera_info, Attr),
	{Dx, Dy, Dz} = Dir,
	{Px, Py, Pz} = Pos,
	
	%%FOV, Width and Height information passed in to allow dimensions to be forced
	%%to geometry screen size
	Fov = CorrectedFOV,
	%Width = proplists:get_value(width, Attr, 320),
	%Height = proplists:get_value(height, Attr, 240),
	
	io:format(F, "#declare camera_location = <~f, ~f, ~f>;\n", [Px, Py, Pz]),
	io:put_chars(F, "camera{\n"),
	
	io:format(F, "\t ~s\n", [atom_to_list(proplists:get_value(camera_type, Attr, perspective))]),
	io:put_chars(F, "\t location camera_location\n"),
	
	io:format(F, "\t right (~p / ~p) * x\n",[Width, Height]),
	io:put_chars(F, "\t up y\n"),
	io:format(F, "\t angle ~f\n", [Fov]),
	{Ux, Uy, Uz} = Up,
	io:format(F, "\t sky <~f, ~f, ~f>\n", [Ux, Uy, Uz]),
	case proplists:get_value(aperture, Attr, 0.0) of
		0.0 -> ok;
		_ ->
			io:format(F, "\t aperture ~f\n", [proplists:get_value(aperture, Attr, 0.0)]),
			io:format(F, "\t blur_samples ~p\n", [proplists:get_value(blur_samples, Attr, 1)]),
			io:format(F, "\t focal_point <~f, ~f, ~f>\n", [Px + Dx, Py + Dy, Pz + Dz])
	end, 
	io:format(F, "\t look_at <~f, ~f, ~f>\n", [Px + Dx, Py + Dy, Pz + Dz]),
	
	io:put_chars(F, "}\n").
	
export_lights(_F, [], _I)->
	ok;
export_lights(F, [Light | Lights],Index)->
	{Name, Ps} = Light,
	export_light(F, Name, Ps, Index),
	export_lights(F, Lights, Index+1).
	
export_light(F, Name, Ps, Index) ->
    case proplists:get_value(visible, Ps, true) of
		true ->
			OpenGL = proplists:get_value(opengl, Ps, []),
			PovRay = proplists:get_value(?TAG, Ps, []),
			Type = proplists:get_value(type, OpenGL, []),
			case Type of
				ambient->ok;
				_ ->
                    io:format(F, "#declare ~s = light_source {\n", [clean_name("wl_"++integer_to_list(Index)++"_"++Name)]),
					export_light_basics(F, OpenGL, PovRay),
					export_light(F, Name, Type, OpenGL, PovRay),
					io:put_chars(F, "}\n")
			end;
		_ ->ok
    end.
export_light_basics(F, OpenGL, PovRay)->
	{Pxo, Pyo, Pzo} = proplists:get_value(position, OpenGL, {0.0, 0.0, 0.0}),
	{Px, Py, Pz} = export_transform_pos({Pxo, Pyo, Pzo}),
	
	io:format(F, "\t <~f, ~f, ~f>\n", [Px, Py, Pz]),
	Power = proplists:get_value(light_power, PovRay, 1.0),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
	io:format(F, "\t color rgb <~f, ~f, ~f>\n", [Dr * Power, Dg * Power, Db * Power]),
	io:format(F, "\t fade_distance ~f\n", [proplists:get_value(fade_distance, PovRay, 1.0)]),
	io:format(F, "\t fade_power ~f\n", [proplists:get_value(fade_power, PovRay, 0.0)]),
	case proplists:get_value(media_interaction, PovRay, true) of
		true -> io:put_chars(F, "\t media_interaction on\n");
		false ->io:put_chars(F, "\t media_interaction off\n")
	end,
	case proplists:get_value(media_attenuation, PovRay, false) of
		true -> io:put_chars(F, "\t media_attenuation on\n");
		false ->io:put_chars(F, "\t media_attenuation off\n")
	end,
	case proplists:get_value(shadows, PovRay, true) of
		true -> ok;
		false ->io:put_chars(F, "\t shadowless\n")
	end,
	case proplists:get_value(photons, PovRay, false) of
		false ->ok;
		true -> io:put_chars(F, "\t photons {\n"),
			case proplists:get_value(refraction, PovRay, true) of
				true -> io:put_chars(F, "\t\t refraction on\n");
				false ->io:put_chars(F, "\t\t refraction off\n")
			end,
			case proplists:get_value(reflection, PovRay, true) of
				true -> io:put_chars(F, "\t\t reflection on\n");
				false ->io:put_chars(F, "\t\t reflection off\n")
			end,
			io:put_chars(F, "\t }\n")
	end.
	
export_light(F, _Name, spot, OpenGL, PovRay)->
	case proplists:get_value(cylinder, PovRay, false) of
		false -> io:put_chars(F, "\t spotlight\n");
		_ -> io:put_chars(F, "\t cylinder\n")
	end,
	Radius = proplists:get_value(cone_angle, OpenGL, 30.0),
	io:format(F, "\t radius ~f\n", [Radius]),
	case proplists:get_value(spot_exponent,OpenGL, 0.0) > Radius of
		true ->io:format(F, "\t falloff ~f\n", [proplists:get_value(spot_exponent,OpenGL, 0.0)]);
		false ->io:format(F, "\t falloff ~f\n", [Radius])
	end,
	io:format(F, "\t tightness ~f\n", [proplists:get_value(tightness, PovRay, 0.0)]),
	{Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
	{Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
	io:format(F, "\t point_at <~f, ~f, ~f>\n", [Dx, Dy, Dz]);
	
export_light(F, _Name, area, OpenGL, PovRay)->
	#e3d_mesh{vs=Vs} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
	{{V1xo, V1yo, V1zo}, {V2xo, V2yo, V2zo}, {V3xo, V3yo, V3zo}, _V4} = VsT,
	{V1x, V1y, V1z} = export_transform_pos({V1xo, V1yo, V1zo}),
	{V2x, V2y, V2z} = export_transform_pos({V2xo, V2yo, V2zo}),
	{V3x, V3y, V3z} = export_transform_pos({V3xo, V3yo, V3zo}),
	
	io:put_chars(F, "\t area_light\n"),
	io:format(F, "\t <~f, ~f, ~f>, <~f, ~f, ~f>, ~p, ~p\n", [V1x - V2x, V1y - V2y, V1z - V2z,
		V3x - V2x, V3y - V2y, V3z - V2z, proplists:get_value(size_1, PovRay, 2), proplists:get_value(size_2, PovRay, 2)]),
	case proplists:get_value(adaptive, PovRay, none) of
		none ->ok;
		Adaptive ->
			io:format(F, "\t adaptive ~p\n", [Adaptive])
	end,
	case proplists:get_value(jitter, PovRay, false) of
		true -> io:put_chars(F, "\t jitter\n");
		false -> ok
	end,
	case proplists:get_value(orient, PovRay, false) of
		true -> io:put_chars(F, "\t orient\n");
		false -> ok
	end,
	case proplists:get_value(circular, PovRay, false) of
		true -> io:put_chars(F, "\t circular\n");
		false -> ok
	end;
	
export_light(F, _Name, infinite, OpenGL, _PovRay)->
	io:put_chars(F, "\t parallel\n"),
	{Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
	{Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
	io:format(F, "\t point_at <~f, ~f, ~f>\n", [Dx, Dy, Dz]);
export_light(_F, _Name, _Type, _OpenGL, _PovRay)->
	ok.

export_lights_objects(_F,[],_I) ->
	ok;
export_lights_objects(F,[Light|Lights],Index0) ->
	{Name,Ps} = Light,
    OpenGL = proplists:get_value(opengl, Ps, []),
    Index = case proplists:get_value(type, OpenGL, []) of
        ambient ->
            Index0;
        _ ->
            export_light_def(F, clean_name("wl_"++integer_to_list(Index0)++"_"++Name)),
            Index0+1
    end,
	export_lights_objects(F,Lights,Index).

export_light_def(F,Name) ->
	io:format(F, "object{ ~s\n", [Name]),
	io:put_chars(F, "}\n").


export_materials(_F, [], _Attr, _ExportDir)->
	ok;
export_materials(F, [{Name, Mat} | Mats], Attr, ExportDir)->
	Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    PovRay = proplists:get_value(?TAG, Mat, []),
	
	Ghost = proplists:get_value(ghost_material, PovRay, false),
	
	case Ghost of 
		true->ok;
		false->
		
	io:format(F, "#declare ~s = texture{\n", [clean_name("wm_"++atom_to_list(Name))]),
	
	%export system image maps
	MapList = export_maps(Maps, ExportDir),
	
	%pigment 
	Pigment = proplists:get_value(pigment_pattern, PovRay, color),
	case Pigment of
		image -> io:put_chars(F, "\t uv_mapping\n");
		_ -> ok
	end,
	io:put_chars(F, "\t pigment{\n"),
	export_pigment(F, Pigment, PovRay, OpenGL, MapList, Attr, ExportDir),
	case proplists:get_value(pigment_modifiers, PovRay, false) of
		false ->ok;
		true ->export_pigment_modifiers(F, Pigment, PovRay)
	end,
	case proplists:get_value(pigment_colormap, PovRay, false) of
		false ->ok;
		true ->	export_colormap(F, Pigment, PovRay)
	end,
	io:put_chars(F, "\t }\n"),
	
	%normal
	Normal = proplists:get_value(normal_pattern, PovRay, none),
	case Normal of
		none-> Skip=true;
		average->
			case proplists:get_value(normal_normalmap, PovRay, false) of
				false->Skip=true;
				true->
					case proplists:get_value(normalmap_list, PovRay, []) of
						[]->Skip=true;
						_ ->Skip=false
					end
			end;
		_ ->Skip=false
	end,
	case Skip of
		true -> ok;
		_ ->
			io:put_chars(F, "\t normal{\n"),
			export_normal(F, Normal, PovRay, OpenGL, MapList, Attr, ExportDir),
			case proplists:get_value(normal_modifiers, PovRay, false) of
				false ->ok;
				true ->export_normal_modifiers(F, Normal, PovRay)
			end,
			case proplists:get_value(normal_normalmap, PovRay, false) of
				false ->ok;
				true ->	export_normalmap(F, Normal, PovRay)
			end,
			io:put_chars(F, "\t }\n")
	end,
	io:put_chars(F, "\t finish {\n"),
	export_finish(F, OpenGL, PovRay),
	io:put_chars(F, "\t }\n"),
	%io:put_chars(F, "#end\n"),
	io:put_chars(F, "}\n")
	
	end,
	
	export_materials(F, Mats, Attr, ExportDir),
	ok.

export_finish(F, OpenGL, PovRay)->
	case get_var(use_emit_ambient) of
		true -> {Ar, Ag, Ab, _Aa} = proplists:get_value(emission, OpenGL, {0.0, 0.0, 0.0, 1.0}),
			io:format(F, "\t\t ambient rgb <~f, ~f, ~f>\n", [Ar, Ag, Ab]);
		false -> {Ar, Ag, Ab, _Aa} = proplists:get_value(ambient, OpenGL, {0.0, 0.0, 0.0, 1.0}),
			io:format(F, "\t\t ambient rgb <~f, ~f, ~f>\n", [Ar, Ag, Ab])
	end,
	io:format(F, "\t\t diffuse ~f\n", [proplists:get_value(finish_diffuse, PovRay, 0.7)]),
	io:format(F, "\t\t brilliance ~f\n", [proplists:get_value(finish_brilliance, PovRay, 1.0)]),
	io:format(F, "\t\t metallic ~f\n", [proplists:get_value(finish_metallic, PovRay, 0.0)]),
	{Sr, Sg, Sb, _} = proplists:get_value(specular, OpenGL, 1.0),
	{_H,_S,V} = wings_ask:rgb_to_hsv({Sr, Sg, Sb}),
	io:format(F, "\t\t specular ~f\n", [V]),
	io:format(F, "\t\t roughness ~f\n", [1.01 - proplists:get_value(shininess, OpenGL, 1.0)]),
	case proplists:get_value(reflection_enabled, PovRay, false) of
		false ->ok;
		true -> io:put_chars(F, "\t\t reflection {\n"),
			{Mir, Mig, Mib} = proplists:get_value(reflection_minimum, PovRay, {0.0, 0.0, 0.0}),
			{Mar, Mag, Mab} = proplists:get_value(reflection_maximum, PovRay, {0.0, 0.0, 0.0}),
			io:format(F, "\t\t\t color rgb <~f, ~f, ~f> ", [Mir, Mig, Mib]),
			case proplists:get_value(reflection_variable, PovRay, false) of
				false -> io:put_chars(F, "\n");
				true -> io:format(F, "color <~f, ~f, ~f>\n", [Mar, Mag, Mab])
			end,
			case proplists:get_value(reflection_fresnel, PovRay, false) of
				false-> ok;
				true -> io:put_chars(F, "\t\t\t fresnel\n")
			end,
			io:format(F, "\t\t\t falloff ~f\n", [proplists:get_value(reflection_falloff, PovRay, 1.0)]),
			io:format(F, "\t\t\t exponent ~f\n", [proplists:get_value(reflection_exponent, PovRay, 1.0)]),
			io:format(F, "\t\t\t metallic ~f\n", [proplists:get_value(reflection_metallic, PovRay, 0.0)]),
			io:put_chars(F, "\t\t }\n")
	end.
	
export_maps([], _ED)->
	[];
export_maps([{MapID, Map} | Maps], ExportDir) ->
	#e3d_image{name=ImageName,filename=FileName} = Map,
	case FileName of
	none ->
		MapFile = case get_map_type(ImageName) of
		sys -> ImageName++".png";
		_ -> ImageName
		end,
		e3d_image:save(Map, filename:join(ExportDir, MapFile));
	_ -> ok
	end,
	[{MapID, ImageName} | export_maps(Maps, ExportDir)].

get_map_type(Filepath) ->
	Ext = filename:extension(Filepath),
	case Ext of
		".jpg" -> jpeg;
		".png" -> png;
		".bmp" -> sys;
		".gif" -> gif;
		".iff" -> iff;
		".tiff" ->tiff;
		_ -> sys
	end.
export_pigment_modifiers(F, Type, PovRay)->
	case Type of
		color ->ok;
		image ->ok;
		_ ->
			io:format(F, "\t\t translate <~f, ~f, ~f>\n", [proplists:get_value(pigment_off_x, PovRay, 0.0),
				proplists:get_value(pigment_off_y, PovRay, 0.0), proplists:get_value(pigment_off_z, PovRay, 0.0)]),
			io:format(F, "\t\t rotate <~f, ~f, ~f>\n", [proplists:get_value(pigment_rot_x, PovRay, 0.0),
				proplists:get_value(pigment_rot_y, PovRay, 0.0), proplists:get_value(pigment_rot_z, PovRay, 0.0)]),
			io:format(F, "\t\t turbulence ~f\n", [proplists:get_value(pigment_turbulence, PovRay, 0.0)]),
			io:format(F, "\t\t scale ~f\n", [proplists:get_value(pigment_scale, PovRay, 1.0)]),
			io:format(F, "\t\t frequency ~f\n", [proplists:get_value(pigment_frequency, PovRay, 1.0)])
	end.
export_pigment(F, Pigment, PovRay, OpenGL, Maps, Attr, ExportDir) ->
	case proplists:get_value(finish_transparency, PovRay, filter) of
		filter -> Filter = "rgbf";
		_-> Filter = "rgbt"
	end,
	
	%retrieve the kind of pigment map, if any
	case proplists:is_defined(proplists:get_value(image_type, PovRay, user), Maps) of
		true -> PigmentImage = proplists:get_value(image_type, PovRay, user);
		_ -> PigmentImage = user
	end,
	
	case Pigment of
		color -> {Cr, Cg, Cb, Ca} = proplists:get_value(diffuse, OpenGL, {1.0, 1.0, 1.0, 1.0}),
			io:format(F, "\t\t color ~s <~f, ~f, ~f, ~f>\n", [Filter, Cr, Cg, Cb, 1.0 - Ca]);
		agate -> io:format(F, "\t\t agate\n\t\t agate_turb ~f\n", [proplists:get_value(agate_turbulence, PovRay, 1.0)]);
		brick ->{Br, Bg, Bb, Ba} = proplists:get_value(brick_color, PovRay, {1.0, 0.0, 0.0, 1.0}),
			{Mr, Mg, Mb, Ma} = proplists:get_value(mortar_color, PovRay, {1.0, 1.0, 1.0, 1.0}),
			io:format(F, "\t\t brick\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n", [Filter, Mr, Mg, Mb, 1.0 - Ma, Filter, Br, Bg, Bb, 1.0 - Ba]),
			io:format(F, "\t\t brick_size <~f, ~f, ~f>\n\t\t mortar ~f\n", [proplists:get_value(brick_size_x, PovRay, 7.5), 
				proplists:get_value(brick_size_y, PovRay, 2.5), proplists:get_value(brick_size_z, PovRay, 4.0), proplists:get_value(mortar_size, PovRay, 0.5)]);
		checker ->{C1r, C1g, C1b, C1a} = proplists:get_value(checker_color1, PovRay, {1.0, 1.0, 1.0, 1.0}),
			{C2r, C2g, C2b, C2a} = proplists:get_value(checker_color2, PovRay, {0.0, 1.0, 0.0, 1.0}),
			io:format(F, "\t\t checker\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n", [Filter, C1r, C1g, C1b, 1.0 - C1a, Filter, C2r, C2g, C2b, 1.0 - C2a]);
		crackle -> io:format(F, "\t crackle\n\t metric ~p\n\toffset ~f\n", [proplists:get_value(crackle_metric, PovRay, 2), 
			proplists:get_value(crackle_offset, PovRay, 0.0)]),
			case proplists:get_value(crackle_solid, PovRay, false) of
				false ->ok;
				true ->io:put_chars(F, "\t\t solid\n")
			end;
		gradient -> io:format(F, "\t\t gradient ~s\n", [atom_to_list(proplists:get_value(gradient_axis, PovRay, x))]);
		hexagon ->{H1r, H1g, H1b, H1a} = proplists:get_value(hexagon_color1, PovRay, {0.0, 0.0, 1.0, 1.0}),
			{H2r, H2g, H2b, H2a} = proplists:get_value(hexagon_color2, PovRay, {0.0, 1.0, 0.0, 1.0}),
			{H3r, H3g, H3b, H3a} = proplists:get_value(hexagon_color3, PovRay, {1.0, 0.0, 0.0, 1.0}),
			io:format(F, "\t\t hexagon\n\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n", 
				[Filter, H1r, H1g, H1b, 1.0 - H1a, Filter, H2r, H2g, H2b, 1.0 - H2a, Filter, H3r, H3g, H3b, 1.0 - H3a]);
		quilted -> io:format(F, "\t\t quilted\n\t\t control0 ~f\n\t\t control1 ~f\n", [proplists:get_value(quilted_value1, PovRay, 1.0), proplists:get_value(quilted_value2, PovRay, 1.0)]);
		spiral1 -> io:format(F, "\t\t spiral1 ~p\n", [proplists:get_value(spiral_arms, PovRay, 5)]);
		spiral2 -> io:format(F, "\t\t spiral2 ~p\n", [proplists:get_value(spiral_arms, PovRay, 5)]);
		slope -> 
			#camera_info{dir=Dir} = proplists:lookup(camera_info, Attr),
			{Dx, Dy, Dz} = Dir,
			io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0}\n", [Dx, Dy, Dz]);
		image -> io:put_chars(F, "\t\t image_map {\n"),
			case PigmentImage of
				user -> Filepath = proplists:get_value(image_user_file, PovRay, []),
					Filepath0=wings_u:relative_path_name(ExportDir, Filepath),
					io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath)), Filepath0]);
				PI -> io:format(F, "\t\t png \"~s.png\"\n", [proplists:get_value(PI, Maps, [])])
			end,
			io:put_chars(F, "\t\t }\n");
		Pattern -> io:format(F, "\t\t ~s\n", [Pattern])
	end.

export_colormap(F, Type, PovRay)->
	case proplists:get_value(finish_transparency, PovRay, filter) of
		filter -> Filter = "rgbf";
		_-> Filter = "rgbt"
	end,
	
	case Type of
		color ->ok;
		image ->ok;
		checker ->ok;
		brick ->ok;
		hexagon ->ok;
		_ ->ColorMap = proplists:get_value(colormap_list, PovRay, []),
			io:put_chars(F, "\t\t color_map {\n"),
			export_color_entry(F, ColorMap, Filter),
			io:put_chars(F, "\t\t }\n")
	end.

export_color_entry(_F, [], _Filter)->
	ok;
export_color_entry(F, [{Mag, Color} | Entries], Filter)->
	{Cr, Cg, Cb, Ca} = Color,
	io:format(F, "\t\t\t [~f ~s <~f, ~f, ~f, ~f>]\n", [Mag, Filter, Cr, Cg, Cb, 1.0 - Ca]),
	export_color_entry(F, Entries, Filter).

export_normal_modifiers(F, Type, PovRay)->
	case Type of
		none ->ok;
		image ->ok;
		_ ->
			io:format(F, "\t\t translate <~f, ~f, ~f>\n", [proplists:get_value(normal_off_x, PovRay, 0.0),
				proplists:get_value(normal_off_y, PovRay, 0.0), proplists:get_value(normal_off_z, PovRay, 0.0)]),
			io:format(F, "\t\t rotate <~f, ~f, ~f>\n", [proplists:get_value(normal_rot_x, PovRay, 0.0),
				proplists:get_value(normal_rot_y, PovRay, 0.0), proplists:get_value(normal_rot_z, PovRay, 0.0)]),
			io:format(F, "\t\t turbulence ~f\n", [proplists:get_value(normal_turbulence, PovRay, 0.0)]),
			io:format(F, "\t\t scale ~f\n", [proplists:get_value(normal_scale, PovRay, 1.0)]),
			io:format(F, "\t\t frequency ~f\n", [proplists:get_value(normal_frequency, PovRay, 1.0)])
	end.
export_normal(F, Normal, PovRay, _OpenGL, Maps, Attr, ExportDir) ->
	%retrieve the kind of pigment map, if any
	case proplists:is_defined(proplists:get_value(n_image_type, PovRay, user), Maps) of
		true -> NormalImage = proplists:get_value(n_image_type, PovRay, user);
		_ -> NormalImage = user
	end,
	
	Mag = proplists:get_value(normal_magnitude, PovRay, 0.5),
	NComp = proplists:get_value(normal_components, PovRay, false),
	
	case proplists:get_value(normal_normalmap, PovRay, false) of
		false->NMap=false;
		true->
			case proplists:get_value(normalmap_list, PovRay, []) of
				[]->NMap=false;
				_ ->NMap=true
			end
	end,
	
	case Normal of
		none -> ok;
		agate -> 
			case NMap of
				false -> io:format(F, "\t\t agate ~f\n", [Mag]);
				true -> io:put_chars(F, "\t\t agate\n")
			end,
			io:format(F, "\t\t agate_turb ~f\n", [proplists:get_value(n_agate_turbulence, PovRay, 1.0)]);
		brick ->
			case NComp of
				false -> io:format(F, "\t\t brick ~f\n", [Mag]);
				true ->
					io:put_chars(F, "\t\t brick\n"),
					io:format(F, "\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n", 
						[atom_to_list(proplists:get_value(mortar_normal, PovRay, agate)), Mag, atom_to_list(proplists:get_value(brick_normal, PovRay, agate)), Mag])
			end,
			io:format(F, "\t\t brick_size <~f, ~f, ~f>\n\t\t mortar ~f\n", [proplists:get_value(n_brick_size_x, PovRay, 7.5), 
				proplists:get_value(n_brick_size_y, PovRay, 2.5), proplists:get_value(n_brick_size_z, PovRay, 4.0), proplists:get_value(n_mortar_size, PovRay, 0.5)]);
		checker ->
			case NComp of
				false -> io:format(F, "\t\t checker ~f\n", [Mag]);
				true ->
					io:put_chars(F, "\t\t checker\n"),
					io:format(F, "\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n", 
						[atom_to_list(proplists:get_value(checker_normal1, PovRay, agate)), Mag, atom_to_list(proplists:get_value(checker_normal2, PovRay, agate)), Mag])
			end;
		crackle -> 
			case NMap of
				false -> io:format(F, "\t\t crackle ~f\n", [Mag]);
				true -> io:put_chars(F, "\t\t crackle\n")
			end,
			io:format(F, "\t\t metric ~p\n\t\t offset ~f\n", [proplists:get_value(n_crackle_metric, PovRay, 2), 
				proplists:get_value(n_crackle_offset, PovRay, 0.0)]),
			case proplists:get_value(n_crackle_solid, PovRay, false) of
				false ->ok;
				true ->io:put_chars(F, "\t\t solid\n")
			end;
		gradient -> 
			case NMap of
				false -> io:format(F, "\t\t gradient ~s, ~f \n", [atom_to_list(proplists:get_value(n_gradient_axis, PovRay, x)), Mag]);
				true -> io:format(F, "\t\t gradient ~s\n", [atom_to_list(proplists:get_value(n_gradient_axis, PovRay, x))])
			end;
		hexagon ->
			case NComp of
				false -> io:format(F, "\t\t hexagon ~f\n", [Mag]);
				true ->
					H1 = proplists:get_value(hexagon_normal1, PovRay, agate),
					H2 = proplists:get_value(hexagon_normal2, PovRay, agate),
					H3 = proplists:get_value(hexagon_normal3, PovRay, agate),
					io:put_chars(F, "\t\t hexagon\n"),
					io:format(F, "\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n", [atom_to_list(H1),Mag, atom_to_list(H2),Mag, atom_to_list(H3),Mag])
			end;
		quilted -> 
			case NMap of
				false -> io:format(F, "\t\t quilted ~f\n", [Mag]);
				true -> io:put_chars(F, "\t\t quilted\n")
			end,
			io:format(F, "\t\t control0 ~f\n\t\t control1 ~f\n", [proplists:get_value(n_quilted_value1, PovRay, 1.0), proplists:get_value(n_quilted_value2, PovRay, 1.0)]);
		spiral1 ->
			case NMap of
				false -> io:format(F, "\t\t spiral1 ~p, ~f\n", [proplists:get_value(n_spiral_arms, PovRay, 5), Mag]);
				true -> io:format(F, "\t\t spiral1 ~p\n", [proplists:get_value(n_spiral_arms, PovRay, 5) ])
			end;
		spiral2 ->
			case NMap of
				false -> io:format(F, "\t\t spiral2 ~p, ~f\n", [proplists:get_value(n_spiral_arms, PovRay, 5), Mag]);
				true -> io:format(F, "\t\t spiral2 ~p\n", [proplists:get_value(n_spiral_arms, PovRay, 5)])
			end;
		slope -> 
			#camera_info{dir=Dir} = proplists:lookup(camera_info, Attr),
			{Dx, Dy, Dz} = Dir,
			case NMap of
				false -> io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0} ~f\n", [Dx, Dy, Dz, Mag]);
				true -> io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0}\n", [Dx, Dy, Dz])
			end;
		image -> io:put_chars(F, "\t\t uv_mapping bump_map {\n"),
			case NormalImage of
				user -> Filepath = proplists:get_value(n_image_user_file, PovRay, []),
					Filepath0=wings_u:relative_path_name(ExportDir, Filepath),
					io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath)), Filepath0]);
				PI -> io:format(F, "\t\t png \"~s.png\"\n", [proplists:get_value(PI, Maps, [])])
			end,
			io:format(F, "\t\t bump_size ~f\n", [Mag]),
			io:put_chars(F, "\t\t }\n");
		facets -> 
			io:format(F, "\t\t facets ~f\n", [Mag]);
		Pattern -> 
			case NMap of
				false -> io:format(F, "\t\t ~s ~f\n", [Pattern, Mag]);
				true -> io:format(F, "\t\t ~s\n", [Pattern])
			end
	end.
export_normalmap(F, Type, PovRay)->

	Mag = proplists:get_value(normal_magnitude, PovRay, 0.5),

	case Type of
		none ->ok;
		image ->ok;
		checker ->ok;
		brick ->ok;
		hexagon ->ok;
		facets->ok;
		_ ->NormalMap = proplists:get_value(normalmap_list, PovRay, []),
			io:put_chars(F, "\t\t normal_map {\n"),
			export_normal_entry(F, NormalMap, Mag),
			io:put_chars(F, "\t\t }\n")
	end.

export_normal_entry(_F, [], _M)->
	ok;
export_normal_entry(F, [{Mag, Normal} | Entries], NormalMag)->
	io:format(F, "\t\t\t [~f ~s ~f]\n", [Mag, atom_to_list(Normal), NormalMag]),
	export_normal_entry(F, Entries, NormalMag).

export_objects(_F, [], _M, _A, _I)->
	ok;
export_objects(F, [EObj | Objs], AllMats, Attr, Index)->
	#e3d_object{name=Name, obj=Obj} = EObj,
	
	ObjMesh = e3d_mesh:triangulate(Obj),
	#e3d_mesh{fs=Fs0,ns=NTab,vs=VTab,tx=UVTab} = e3d_mesh:vertex_normals(ObjMesh),
	Mat = e3d_mesh:used_materials(ObjMesh),
	
	LimitVertex = get_var(limit_vertices),
	
	VLen = float(length(VTab)),
	io:format(F, "#declare ~s = mesh2{\n", [clean_name("wo_"++ integer_to_list(Index)++"_"++Name)]),
	io:format(F, "\t vertex_vectors { ~p", [length(VTab)]),
	case LimitVertex of
		true->{Sx, Sy, Sz} = export_vectors(F, VTab, 0);
		false->{Sx, Sy, Sz} = export_vectors(F, VTab)
	end,
	io:put_chars(F, "}\n"),
	io:format(F, "\t //#local average_center = <~f, ~f, ~f>;\n", [Sx / VLen, Sy / VLen, Sz / VLen]),
	
	
	case proplists:get_value(export_normals, Attr, true) of
		false ->ok;
		true -> io:format(F, "\t normal_vectors { ~p", [length(NTab)]),
			case LimitVertex of
				true ->	export_vectors(F, NTab, 0);
				false -> export_vectors(F, NTab)
			end,
			io:put_chars(F, "}\n")
	end,
	case proplists:get_value(include_uvs, Attr, true) of
		false ->ok;
		true ->io:format(F, "\t uv_vectors { ~p", [length(UVTab)]),
			case LimitVertex of
				true-> export_vectors2D(F, UVTab, 0);
				false-> export_vectors2D(F, UVTab)
			end,
			io:put_chars(F, "}\n")
	end,
	
	io:format(F, "\t texture_list { ~p", [length(Mat)]),
	export_texture_names(F, Mat, AllMats),
	io:put_chars(F, "}\n"),
	
	MatList = arrange_texture_list(Mat, 0),
	
	io:format(F, "\t face_indices { ~p", [length(Fs0)]),
	case LimitVertex of
		true->  MatIndices = export_faces(F, Fs0, MatList, [], 0);
		false-> MatIndices = export_faces(F, Fs0, MatList, [])
	end,
	io:put_chars(F, "}\n"),
	
	case proplists:get_value(export_normals, Attr, true) of
		false ->ok;
		true ->
			io:format(F, "\t normal_indices { ~p", [length(Fs0)]),
			case LimitVertex of
				true-> export_normal_indices(F, Fs0, 0);
				false-> export_normal_indices(F, Fs0)
			end,
			io:put_chars(F, "}\n")
	end,
	
	case proplists:get_value(include_uvs, Attr, true) of
		false ->ok;
		true ->io:format(F, "\t uv_indices { ~p", [length(Fs0)]),
			case LimitVertex of
				true-> export_uv_indices(F, Fs0, 0);
				false-> export_uv_indices(F, Fs0)
			end,
			io:put_chars(F, "}\n")
	end,

	io:put_chars(F, "}\n"),
	
	%using the material on the most number of faces for interior and photons
	{Mi, _MC} = greatest_index(MatIndices, {0, 0}),
	InteriorMatName = name_of_index(Mat, Mi, 0),
	InteriorMat = material_of_name(InteriorMatName, AllMats),
	export_object_def(F, clean_name("wo_"++integer_to_list(Index)++"_"++Name), InteriorMat),
	
	export_objects(F, Objs, AllMats, Attr, Index + 1).
	
	
greatest_index([], {Mi, MC})->
	{Mi, MC};
greatest_index([{Index, Count} | List], {Mi, MC})->
	case Count>MC of
		true -> greatest_index(List, {Index, Count});
		false -> greatest_index(List, {Mi, MC})
	end.
name_of_index([], _I, _C)->
	[];
name_of_index([ Mat | Mats], Index, Current)->
	case Index==Current of
		true -> Mat;
		false -> name_of_index(Mats, Index, Current + 1)
	end.
material_of_name(_I, [])->
	{[],[]};
material_of_name(InteriorMatName, [{MatName, Mat} | AllMats])->
	case MatName==InteriorMatName of
		true->{MatName, Mat};
		false -> material_of_name(InteriorMatName, AllMats)
	end.
	
arrange_texture_list([], _I)->
	[];
arrange_texture_list([Name | Mats], Index)->
	[{Name, Index} | arrange_texture_list(Mats, Index + 1)].


export_vectors(_F, [])->
	{0.0, 0.0, 0.0};
export_vectors(F, [{X, Y, Z} | List])->
	io:format(F, ", <~f, ~f, ~f>", [X, Y, Z]),
	{Sx, Sy, Sz} = export_vectors(F, List),
	{Sx + X, Sy + Y, Sz + Z}.
	
export_vectors(_F, [], _C)->
	{0.0, 0.0, 0.0};
export_vectors(F, [{X, Y, Z} | List], Count)->
	io:put_chars(F, ", "),
	case Count of
		?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
			Next = 0;
		_ -> Next = Count + 1
	end,
	
	io:format(F, "<~f, ~f, ~f>", [X, Y, Z]),
	{Sx, Sy, Sz} = export_vectors(F, List, Next),
	{Sx + X, Sy + Y, Sz + Z}.


export_vectors2D(_F, [])->
	ok;
export_vectors2D(F, [{X, Y} | List])->
	io:format(F, ", <~f, ~f>", [X, Y]),
	export_vectors2D(F, List).
	
export_vectors2D(_F, [], _C)->
	ok;
export_vectors2D(F, [{X, Y} | List], Count)->
	io:put_chars(F, ", "),
	case Count of
		?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
			Next = 0;
		_ -> Next = Count + 1
	end,
	io:format(F, "<~f, ~f>", [X, Y]),
	export_vectors2D(F, List, Next).
	
export_texture_names(_F, [], _AM)->
	ok;
export_texture_names(F, [Name | Mats], AllMats)->
	%io:format(F, ", texture{~s(camera_location)}", [clean_name("wm_"++atom_to_list(Name))]),
	
	%find out if ghost material, don't decorate name if so
	{_N, Mat} = material_of_name(Name, AllMats),
	PovRay = proplists:get_value(?TAG, Mat, []),
	Ghost = proplists:get_value(ghost_material, PovRay, false),
	
	case Ghost of
		false->	io:format(F, ", texture{~s}", [clean_name("wm_"++atom_to_list(Name))]);
		true-> io:format(F, ", texture{~s}", [atom_to_list(Name)])
	end,
	export_texture_names(F, Mats, AllMats).
	
export_faces(_F, [], _ML, MI)->
	MI;
export_faces(F, [#e3d_face{vs=Vs, mat=MatNames} | Faces], MatList, MatIndices)->
	{X, Y, Z} = list_to_tuple(Vs),
	{M} = list_to_tuple(MatNames),
	
	%pull the appropriate index based on the tex name
	Index = proplists:get_value(M, MatList, 1),
	
	%increment the index count for a given material
	Count = proplists:get_value(Index, MatIndices, 0),
	DelMi = proplists:delete(Index, MatIndices),
	NewMI = [{Index, Count + 1} | DelMi],
	
	io:format(F, ", <~p, ~p, ~p>, ~p", [X, Y, Z, Index]),
	export_faces(F, Faces, MatList, NewMI).	
	
export_faces(_F, [], _ML, MI, _C)->
	MI;
export_faces(F, [#e3d_face{vs=Vs, mat=MatNames} | Faces], MatList, MatIndices, PerLine)->
	{X, Y, Z} = list_to_tuple(Vs),
	{M} = list_to_tuple(MatNames),
	
	%pull the appropriate index based on the tex name
	Index = proplists:get_value(M, MatList, 1),
	
	%increment the index count for a given material
	Count = proplists:get_value(Index, MatIndices, 0),
	DelMi = proplists:delete(Index, MatIndices),
	NewMI = [{Index, Count + 1} | DelMi],
	
	io:put_chars(F, ", "),
	case PerLine of
		?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
			Next = 0;
		_ -> Next = PerLine + 1
	end,
	
	io:format(F, "<~p, ~p, ~p>, ~p", [X, Y, Z, Index]),
	export_faces(F, Faces, MatList, NewMI, Next).
	
export_normal_indices(_F, [])->
	ok;
export_normal_indices(F, [#e3d_face{ns=Ns} | Faces])->
	{X, Y, Z} = list_to_tuple(Ns),
	
	io:format(F, ", <~p, ~p, ~p>", [X, Y, Z]),
	export_normal_indices(F, Faces).
	
export_normal_indices(_F, [], _P)->
	ok;
export_normal_indices(F, [#e3d_face{ns=Ns} | Faces], PerLine)->
	{X, Y, Z} = list_to_tuple(Ns),
	
	io:put_chars(F, ", "),
	case PerLine of
		?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
			Next = 0;
		_ -> Next = PerLine + 1
	end,
	
	io:format(F, "<~p, ~p, ~p>", [X, Y, Z]),
	export_normal_indices(F, Faces, Next).

export_uv_indices(_F, [])->
	ok;
export_uv_indices(F, [#e3d_face{tx=Txs} | Faces])->
	case Txs of
		[]-> io:put_chars(F, ", <0, 0, 0>");
		_ ->
			{X, Y, Z} = list_to_tuple(Txs),
			io:format(F, ", <~p, ~p, ~p>", [X, Y, Z])
	end,
	export_uv_indices(F, Faces).
	
export_uv_indices(_F, [], _P)->
	ok;
export_uv_indices(F, [#e3d_face{tx=Txs} | Faces], PerLine)->

	io:put_chars(F, ", "),
	case PerLine of
		?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
			Next = 0;
		_ -> Next = PerLine + 1
	end,
	
	case Txs of
		[]-> io:put_chars(F, "<0, 0, 0>");
		_ ->
			{X, Y, Z} = list_to_tuple(Txs),
			io:format(F, "<~p, ~p, ~p>", [X, Y, Z])
	end,
	export_uv_indices(F, Faces, Next).

export_object_def(F, Name, {_MatName, InteriorMat})->
	PovRay = proplists:get_value(?TAG, InteriorMat, []),
	
	io:format(F, "object{ ~s\n", [Name]),
	io:put_chars(F, "\t interior {\n"),
	io:format(F, "\t\t ior ~f\n", [proplists:get_value(interior_ior, PovRay, 1.0)]),
	case proplists:get_value(interior_extended, PovRay, false) of
		false -> ok;
		true ->
			io:format(F, "\t\t caustics ~f\n", [proplists:get_value(interior_caustic, PovRay, 0.0)]),
			io:format(F, "\t\t dispersion ~f\n", [proplists:get_value(interior_dispersion, PovRay, 1.0)]),
			io:format(F, "\t\t dispersion_samples ~p\n", [proplists:get_value(interior_dispersion_samples, PovRay, 7)]),
			io:format(F, "\t\t fade_distance ~p\n", [proplists:get_value(interior_fade_distance, PovRay, 0.0)]),
			io:format(F, "\t\t fade_power ~p\n", [proplists:get_value(interior_fade_power, PovRay, 0.0)]),
			{Fr, Fg, Fb} = proplists:get_value(interior_fade_color, PovRay, {0.0, 0.0, 0.0}),
			io:format(F, "\t\t fade_color rgb <~f, ~f, ~f>\n", [Fr, Fg, Fb])
	end,
	io:put_chars(F, "\t }\n"),
	case proplists:get_value(photons_enabled, PovRay, false) of
		false -> ok;
		true ->
			io:put_chars(F, "\t photons {\n"),
			case proplists:get_value(photons_target, PovRay, true) of
				false ->ok;
				true -> io:put_chars(F, "\t\t target\n")
			end,
			case proplists:get_value(photons_collect, PovRay, true) of
				false ->io:put_chars(F, "\t\t collect off\n");
				true -> io:put_chars(F, "\t\t collect on\n")
			end,
			case proplists:get_value(photons_reflect, PovRay, true) of
				false ->io:put_chars(F, "\t\t reflection off\n");
				true -> io:put_chars(F, "\t\t reflection on\n")
			end,
			case proplists:get_value(photons_refract, PovRay, true) of
				false ->io:put_chars(F, "\t\t refraction off\n");
				true -> io:put_chars(F, "\t\t refraction on\n")
			end,
			io:put_chars(F, "\t }\n")
	end,
	io:put_chars(F, "}\n").
	
%Construct dialog with export options, use get_pref to load options
export_dialog(Op)->
	case Op of
		render ->Render = true;
		_ -> Render =false
	end,
	case get_var(use_model_dim) of
		true -> SizeCam = false;
		_ -> SizeCam = true
	end,
	
	[
		{vframe, [
			{?__(1,"Export UVs"), get_pref(include_uvs, true), [exkey(include_uvs)]},
			{?__(2,"Export Normals"), get_pref(export_normals, true), [exkey(export_normals)]},
			{hframe, [{label, ?__(3,"Subdivisions")}, {slider, {text, get_pref(subdivisions, 0), [range({0, 10}), exkey(subdivisions)]}}]}
		]},
		separator,
		{hframe, [
			{vframe, [{label, ?__(4,"Gamma")}, {label, ?__(5,"Max Trace")}, {label, ?__(6,"Background")}, {label, ?__(7,"Emissive Filter")}, {label, ?__(8,"Emissive Power")}]},
			{vframe, [{text, get_pref(assumed_gamma, 1.0), [exkey(assumed_gamma)]}, {text, get_pref(max_trace_level, 5), [exkey(max_trace_level)]},
				{slider, {color, get_pref(background, {0.0, 0.0, 0.0}), [exkey(background)]}},
				{slider, {color, get_pref(ambient, {0.0, 0.0, 0.0}), [exkey(ambient)]}},
				{slider, {text, get_pref(ambient_power, 1.0), [exkey(ambient_power), range({0.0, 50.0})]}}
			]}
		]},
		{vframe, [
			{vframe, [
				{hframe, [{?__(9,"AntiAlias"), get_pref(antialias, false), [exkey(antialias)]}, 
					{?__(10,"Jitter"), get_pref(aa_jitter, false), [exkey(aa_jitter), hook(enable, [member, antialias, true])]}
				]},
				{hframe, [{label, ?__(11,"AA Threshold")}, {text, get_pref(aa_threshold, 0.3), [exkey(aa_threshold)]}, {label, ?__(12,"AA Depth")}, 
					{slider, {text, get_pref(aa_depth, 1), [range({1, 9}), exkey(aa_depth)]}}
				], [hook(enable, [member, antialias, true])]}
			], [ {hook, fun(is_disabled, _D)->not Render;(_, _)->false end}]},
			separator,
			{hframe, [{label, ?__(13,"Width")}, {text, get_pref(width, 320), [exkey(width)]}, 
				{label, ?__(14,"Height")}, {text, get_pref(height, 240), [exkey(height)]}], 
				[ {hook, fun(is_disabled, _D)->not SizeCam;(_, _)->false end}]},
				
			{hframe, [{label, ?__(15,"Camera")}, {menu, [{?__(16,"Perspective"), perspective}, {?__(17,"Orthographic"), orthographic}, {?__(18,"Fisheye"), fisheye}, {?__(19,"Ultra Wide"), ultra_wide_angle},
				{?__(20,"Omnimax"), omnimax}, {?__(21,"Panoramic"), panoramic}, {?__(22,"Spherical"), spherical}], get_pref(camera_type, perspective), [exkey(camera_type)]}
			]},
			{hframe, [{label, ?__(23,"Aperture")}, {text, get_pref(aperture, 0.0), [exkey(aperture)]}, 
				{label, ?__(24,"Blur Samples")}, {text, get_pref(blur_samples, 1), [range({1, 50}), exkey(blur_samples), hook(enable, ['not', [member, aperture, 0.0]])]}
			]}
		], [{title, ?__(15,"Camera")}]},
		{vframe, [
			{hframe, [{?__(25,"Photons"), get_pref(photons, false), [exkey(photons)]}, 
				{hframe, [{label, ?__(26,"Count")}, {text, get_pref(photon_count, 10000), [exkey(photon_count)]}, {?__(27,"Media Photons"), get_pref(media_photons, false), [exkey(media_photons)]},
					{label, ?__(26,"Count")}, {text, get_pref(media_photons_count, 100), [exkey(media_photons_count), hook(enable, [member, media_photons, true])]}
				], [hook(enable, [member, photons, true])]}
			]}
		]},
		{vframe, [
			{hframe, [{?__(28,"Fog"), get_pref(fog, false), [exkey(fog)]}, {label, ?__(29,"Type")}, 
				{menu, [{?__(30,"Constant"), 1}, {?__(31,"Ground"), 2}], get_pref(fog_type, 1), [exkey(fog_type), hook(enable, [member, fog, true])]}
			]},
			
			{hframe, [
				{vframe, [{label, ?__(32,"Fog Height")}, {label, ?__(33,"Fog Fade")}]},
				{vframe, [{text, get_pref(fog_offset, 0.0), [exkey(fog_offset)]}, {text, get_pref(fog_alt, 0.0), [exkey(fog_alt)]}], [hook(enable, [member, fog_type, 2])]},
				{vframe, [{label, ?__(34,"Distance")}, {label, ?__(35,"Color")}]},
				{vframe, [{text, get_pref(fog_distance, 0.0), [exkey(fog_distance)]}, {color, get_pref(fog_color, {0.0, 0.0, 0.0, 0.0}), [exkey(fog_color)]}]}
			], [hook(enable, [member, fog, true])]},
			
			{vframe, [{?__(36,"Media"), get_pref(media, false), [exkey(media)]},
				{hframe, [{label, ?__(37,"Method")}, {menu, [{?__(38,"Variance"), 1}, {?__(39,"Even"), 2}, {?__(40,"Adaptive"), 3}], get_pref(media_method, 1), [exkey(media_method)]},
					{label, ?__(41,"Intervals")}, {text, get_pref(media_intervals, 1), [exkey(media_intervals)]}, 
					{label, ?__(42,"Samples")}, {text, get_pref(media_samples, 1), [exkey(media_samples)]}
				], [hook(enable, [member, media, true])]},
				{hframe, [
					{vframe, [{label, ?__(43,"Scattering")}, {label, ?__(44,"Absorption")}, {label, ?__(45,"Emission")}]},
					{vframe, [{slider, {color, get_pref(scattering, {0.0, 0.0, 0.0}), [exkey(scattering)]}}, 
						{slider, {color, get_pref(absorbtion, {0.0, 0.0, 0.0}), [exkey(absorbtion)]}},
						{slider, {color, get_pref(emission, {0.0, 0.0, 0.0}), [exkey(emission)]}}
					]},
					{vframe, [{label, ?__(29,"Type")}]},
					{vframe, [{menu, [{?__(46,"Isotropic"), 1}, {?__(47,"Mie Haze"), 2}, {?__(48,"Mie Murky"), 3}, {?__(49,"Rayleigh"), 4}, {?__(50,"Henyey-Greenstein"), 5}], get_pref(scattering_type, 1), [exkey(scattering_type)]}]}
				], [hook(enable, [member, media, true])]}
			]}
			
		], [{title, ?__(51,"Atmosphere")}]},
		
		{hframe, [{label, ?__(52,"Radiosity")}, {menu, [{?__(53,"None"), none}, {?__(54,"Default"), "Radiosity_Default"}, {?__(55,"Debug"), "Radiosity_Debug"}, 
			{?__(56,"Fast"), "Radiosity_Fast"}, {?__(57,"Normal"), "Radiosity_Normal"}, {?__(58,"2Bounce"), "Radiosity_2Bounce"}, {?__(59,"Final"), "Radiosity_Final"}, 
			{?__(60,"OutdoorLQ"), "Radiosity_OutdoorLQ"}, {?__(61,"OutdoorHQ"), "Radiosity_OutdoorHQ"}, {?__(62,"OutdoorLight"), "Radiosity_OutdoorLight"}, 
			{?__(63,"IndoorLQ"), "Radiosity_IndoorLQ"}, {?__(64,"IndoorHQ"), "Radiosity_IndoorHQ"}], get_pref(radiosity, none), [exkey(radiosity)]}
		]}
	].
	
%Construct material dialog, return list of controls
material_dialog(_Name, Mat)->
	Maps = proplists:get_value(maps, Mat, []),
    %OpenGL = proplists:get_value(opengl, Mat),
    PovRay = proplists:get_value(?TAG, Mat, []),
	
	BrowseProps = [ {dialog_type,open_dialog},
		   {extensions,[{".jpg","JPEG"}, {".png","PNG"}, {".bmp", "Bitmap"}, {".gif", "GIF"}, {".iff", "IFF"}, {".tiff", "TIFF"}]}],
		   
	case proplists:is_defined(proplists:get_value(image_type, PovRay, user), Maps) of
		true -> PigmentImage = proplists:get_value(image_type, PovRay, user);
		_ -> PigmentImage = user
	end,
	
	case proplists:is_defined(proplists:get_value(n_image_type, PovRay, user), Maps) of
		true -> NormalImage = proplists:get_value(n_image_type, PovRay, user);
		_ -> NormalImage = user
	end,
	
	[{vframe, [

		{?__(1,"Exclude Material Definition (requires external definition)"), proplists:get_value(ghost_material, PovRay, false), [key(ghost_material), layout]},
		{vframe, [ 
			%%Finish
			{hframe, [
				{vframe, [{label, ?__(2,"Diffuse")}, {label, ?__(3,"Brilliance")}, {label, ?__(4,"Metallic")}, {label, ?__(5,"Transparency")}] },
				{vframe, [{slider, {text, proplists:get_value(finish_diffuse, PovRay, 0.7), [range({0.0, 1.0}), key(finish_diffuse)]}},
					{slider, {text, proplists:get_value(finish_brilliance, PovRay, 1.0), [range({0.0, 10.0}), key(finish_brilliance)]}},
					{slider, {text, proplists:get_value(finish_metallic, PovRay, 0.0), [range({0.0, 10.0}), key(finish_metallic)]}},
					{hradio, [{?__(6,"Transmit"), transmit},{?__(7,"Filter"), filter}], proplists:get_value(finish_transparency, PovRay, filter), [key(finish_transparency)]}
				]}
			], [{title, ?__(8,"Finish")}, {minimized, proplists:get_value(finish_minimized, PovRay, true)}, key(finish_minimized)]},
			%%Reflection
			{vframe, [
				{hframe, [{?__(9,"Enabled"), proplists:get_value(reflection_enabled, PovRay, false), [key(reflection_enabled)]},
					{?__(10,"Variable"), proplists:get_value(reflection_variable, PovRay, false), [key(reflection_variable), layout, hook(enable, [member, ?KEY(reflection_enabled), true])]},
					{?__(11,"Fresnel"), proplists:get_value(reflection_fresnel, PovRay, false), [key(reflection_fresnel), hook(enable, [member, ?KEY(reflection_enabled), true])]}
				]},
				{hframe, [
					{vframe, [
						{label, ?__(12,"Minimum")}, {label, ?__(13,"Maximum"), [hook(open, [member, ?KEY(reflection_variable), true])]}, {label, ?__(14,"Falloff")}, {label, ?__(15,"Exponent")}, {label, ?__(4,"Metallic")}
					]},
					{vframe, [{slider, {color, proplists:get_value(reflection_minimum, PovRay, {0.0, 0.0, 0.0}), [key(reflection_minimum)]}},
						{slider, {color, proplists:get_value(reflection_maximum, PovRay, {0.0, 0.0, 0.0}), [key(reflection_maximum),hook(open, [member, ?KEY(reflection_variable), true])]}},
						{slider, {text, proplists:get_value(reflection_falloff, PovRay, 1.0), [range({0.0, 10.0}), key(reflection_falloff)]}},
						{slider, {text, proplists:get_value(reflection_exponent, PovRay, 1.0), [range({0.0, 10.0}), key(reflection_exponent)]}},
						{slider, {text, proplists:get_value(reflection_metallic, PovRay, 0.0), [range({0.0, 10.0}), key(reflection_metallic)]}}
					]}
				], [hook(enable, [member, ?KEY(reflection_enabled), true])]}
			], [{title, ?__(16,"Reflection")}, {minimized, proplists:get_value(reflection_minimized, PovRay, true)}, key(reflection_minimized)]},
			%%Interior
			{hframe, [
				{vframe, [{label, ?__(17,"IOR")},{label, ?__(18,"Fake Caustics")}, {label, ?__(19,"Dispersion")}, {label, ?__(20,"Disp. Samples")}, {label, ?__(21,"Fade Dist.")}, {label, ?__(22,"Fade Power")}, {label, ?__(23,"Fade Color")}]},
				{vframe, [{slider, {text, proplists:get_value(interior_ior, PovRay, 1.0), [range({0.0, 3.0}), key(interior_ior)]}},
					{vframe, [{slider, {text, proplists:get_value(interior_caustic, PovRay, 0.0), [range({0.0, 1.0}), key(interior_caustic)]}},
						{slider, {text, proplists:get_value(interior_dispersion, PovRay, 1.0), [range({0.0, 2.0}), key(interior_dispersion)]}},
						{slider, {text, proplists:get_value(interior_dispersion_samples, PovRay, 7), [range({0, 50}), key(interior_dispersion_samples)]}},
						{slider, {text, proplists:get_value(interior_fade_distance, PovRay, 0.0), [range({0.0, 10.0}), key(interior_fade_distance)]}},
						{slider, {text, proplists:get_value(interior_fade_power, PovRay, 0.0), [range({0.0, 10.0}), key(interior_fade_power)]}},
						{slider, {color, proplists:get_value(interior_fade_color, PovRay, {0.0, 0.0, 0.0}), [key(interior_fade_color)]}}
					], [hook(enable, [member, ?KEY(interior_extended), true])]}
				]},
				{vframe, [{?__(24,"Extended Interior"), proplists:get_value(interior_extended, PovRay, false), [key(interior_extended)]}]}
			], [{title, ?__(25,"Interior")}, {minimized, proplists:get_value(interior_minimized, PovRay, true)}, key(interior_minimized)]},
			%%Photons
			{hframe, [
				{?__(9,"Enabled"), proplists:get_value(photons_enabled, PovRay, false), [key(photons_enabled)]},
				{hframe, [
					{?__(26,"Target"), proplists:get_value(photons_target, PovRay, true), [key(photons_target)]},
					{?__(27,"Collect"), proplists:get_value(photons_collect, PovRay, true), [key(photons_collect)]},
					{?__(28,"Reflect"), proplists:get_value(photons_reflect, PovRay, true), [key(photons_reflect)]},
					{?__(29,"Refract"), proplists:get_value(photons_refract, PovRay, true), [key(photons_refract)]}
				], [hook(enable, [member, ?KEY(photons_enabled), true])]}
			], [{title, ?__(30,"Photons")}, {minimized, proplists:get_value(photons_minimized, PovRay, true)}, key(photons_minimized)]},
			%%Texture
			{vframe, [
				{vframe, [
					{hframe, [
						{label, ?__(31,"Pattern")}, {menu, [
							{?__(32,"Color"), color}, {?__(33,"Image"), image}, {?__(34,"Agate"), agate}, {?__(35,"Boxed"), boxed}, {?__(36,"Bozo"), bozo}, {?__(37,"Brick"), brick},
							{?__(38,"Bumps"), bumps}, {?__(39,"Cells"), cells}, {?__(40,"Checker"), checker}, {?__(41,"Crackle"), crackle}, {?__(42,"Cylindrical"), cylindrical},
							{?__(43,"Dents"), dents}, {?__(44,"Gradient"), gradient}, {?__(45,"Granite"), granite}, {?__(46,"Hexagon"), hexagon}, {?__(47,"Leopard"), leopard},
							{?__(48,"Marble"), marble}, {?__(49,"Onion"), onion}, {?__(50,"Planar"), planar}, {?__(51,"Quilted"), quilted}, {?__(52,"Radial"), radial},
							{?__(53,"Ripples"), ripples}, {?__(54,"Slope"), slope}, {?__(55,"Spherical"), spherical}, {?__(56,"Spiral1"), spiral1}, {?__(57,"Spiral2"), spiral2}, {?__(58,"Waves"), waves},
							{?__(59,"Wood"), wood}, {?__(60,"Wrinkles"), wrinkles}], proplists:get_value(pigment_pattern, PovRay, color), [key(pigment_pattern), layout]},
						{hframe, [
							{?__(61,"Color Map"), proplists:get_value(pigment_colormap, PovRay, false), [key(pigment_colormap), layout]},
							{button, ?__(62,"New Entry"), done, [key(new_colormap), hook(enable, [member, ?KEY(pigment_colormap), true])]}
						], [hook(enable, ['not', [member, ?KEY(pigment_pattern), color, image, brick, checker, hexagon]])]}
					]},
					{vframe, [
						{?__(63,"Modifiers"), proplists:get_value(pigment_modifiers, PovRay, false), [key(pigment_modifiers), layout]},
						{hframe, [
							{vframe, [{label, ?__(64,"Offset X")}, {label, ?__(65,"Rotate X")}, {label, ?__(66,"Turbulence")}]},
							{vframe, [{text, proplists:get_value(pigment_off_x, PovRay, 0.0), [key(pigment_off_x)]}, 
								{text, proplists:get_value(pigment_rot_x, PovRay, 0.0), [key(pigment_rot_x)]},
								{text, proplists:get_value(pigment_turbulence, PovRay, 0.0), [key(pigment_turbulence)]}]},
							{vframe, [{label, ?__(67,"Y")}, {label, ?__(67,"Y")}, {label, ?__(68,"Freq.")}]},
							{vframe, [{text, proplists:get_value(pigment_off_y, PovRay, 0.0), [key(pigment_off_y)]}, 
								{text, proplists:get_value(pigment_rot_y, PovRay, 0.0), [key(pigment_rot_y)]},
								{text, proplists:get_value(pigment_frequency, PovRay, 1.0), [key(pigment_frequency)]}]},
							{vframe, [{label, ?__(69,"Z")}, {label, ?__(69,"Z")}, {label, ?__(70,"Scale")}]},
							{vframe, [{text, proplists:get_value(pigment_off_z, PovRay, 0.0), [key(pigment_off_z)]}, 
								{text, proplists:get_value(pigment_rot_z, PovRay, 0.0), [key(pigment_rot_z)]},
								{text, proplists:get_value(pigment_scale, PovRay, 1.0), [key(pigment_scale)]}]}
						], [hook(open, [member, ?KEY(pigment_modifiers), true])]}
					], [hook(open, ['not', [member, ?KEY(pigment_pattern), color, image]])]},
					separator,
					%pattern specific values
					%agate
					{hframe, [{label, ?__(71,"Agate Turbulence")}, {text, proplists:get_value(agate_turbulence, PovRay, 1.0), [key(agate_turbulence)]}], [hook(open, [member, ?KEY(pigment_pattern), agate])]},
					%brick
					{hframe, [
						{vframe, [{label, ?__(37,"Brick")}, {label, ?__(72,"Mortar")}]},
						{vframe, [{color, proplists:get_value(brick_color, PovRay, {1.0, 0.0, 0.0, 1.0}), [key(brick_color)]}, {color, proplists:get_value(mortar_color, PovRay, {1.0, 1.0, 1.0, 1.0}), [key(mortar_color)]}]},
						{vframe, [{label, ?__(73,"Size X")}, {label, ?__(74,"Size")}]},
						{vframe, [{text, proplists:get_value(brick_size_x, PovRay,7.5), [key(brick_size_x)]}, {text, proplists:get_value(mortar_size, PovRay,0.5), [key(mortar_size)]}]},
						{vframe, [{label, ?__(67,"Y")}]},
						{vframe, [{text, proplists:get_value(brick_size_y, PovRay,2.5), [key(brick_size_y)]}]},
						{vframe, [{label, ?__(69,"Z")}]},
						{vframe, [{text, proplists:get_value(brick_size_z, PovRay,4.0), [key(brick_size_z)]}]}
					], [hook(open, [member, ?KEY(pigment_pattern), brick])]},
					%checker
					{hframe, [
						{label, ?__(75,"Checker 1")}, {color, proplists:get_value(checker_color1, PovRay, {1.0, 1.0, 1.0, 1.0}), [key(checker_color1)]},
						{label, ?__(76,"Checker 2")}, {color, proplists:get_value(checker_color2, PovRay, {0.0, 1.0, 0.0, 1.0}), [key(checker_color2)]}
					], [hook(open, [member, ?KEY(pigment_pattern), checker])]},
					%crackle
					{hframe, [
						{vframe, [{label, ?__(77,"Crackle Metric")}, {label, ?__(78,"Crackle Offset")}, {?__(79,"Crackle Solid"), proplists:get_value(crackle_solid, PovRay, false), [key(crackle_solid)]}]},
						{vframe, [{slider, {text, proplists:get_value(crackle_metric, PovRay, 2), [range({0, 10}), key(crackle_metric)]}},
							{slider, {text, proplists:get_value(crackle_offset, PovRay, 0.0), [range({0.0, 10.0}), key(crackle_offset)]}}]}
					], [hook(open, [member, ?KEY(pigment_pattern), crackle])]},
					%gradient
					{hframe, [{label, ?__(80,"Gradient Axis")}, {menu, [{?__(81,"X"), x}, {?__(67,"Y"), y}, {?__(69,"Z"), z}], proplists:get_value(gradient_axis, PovRay, x), [key(gradient_axis)]}
						], [hook(open, [member, ?KEY(pigment_pattern), gradient])]},
					%hexagon
					{hframe, [
						{label, ?__(82,"Hex. 1")}, {color, proplists:get_value(hexagon_color1, PovRay, {0.0, 0.0, 1.0, 1.0}), [key(hexagon_color1)]},
						{label, ?__(83,"Hex. 2")}, {color, proplists:get_value(hexagon_color2, PovRay, {0.0, 1.0, 0.0, 1.0}), [key(hexagon_color2)]},
						{label, ?__(84,"Hex. 3")}, {color, proplists:get_value(hexagon_color3, PovRay, {1.0, 0.0, 0.0, 1.0}), [key(hexagon_color3)]}
					], [hook(open, [member, ?KEY(pigment_pattern), hexagon])]},
					%quilted
					{hframe, [
						{label, ?__(85,"Quilted 1")}, {text, proplists:get_value(quilted_value1, PovRay, 1.0), [key(quilted_value1)]},
						{label, ?__(86,"Quilted 2")}, {text, proplists:get_value(quilted_value2, PovRay, 1.0), [key(quilted_value2)]}
					], [hook(open, [member, ?KEY(pigment_pattern), quilted])]},
					%Spiral
					{hframe, [
						{label, ?__(87,"Spiral Arms")}, {text, proplists:get_value(spiral_arms, PovRay, 5), [key(spiral_arms)]}
					], [hook(open, [member, ?KEY(pigment_pattern), spiral1, spiral2])]},
					%Image
					{vframe, [
						{hframe, [{hradio, [{?__(88,"user"), user} | enumerate_image_maps(Maps)], PigmentImage, [key(image_type)] }]},
						{hframe, [{label, ?__(89,"Filename")}, 
							{button, {text, proplists:get_value(image_user_file, PovRay, []), [key(image_user_file), {props, BrowseProps}] }}
						], [hook(enable, [member, ?KEY(image_type), user])]}
					], [hook(open, [member, ?KEY(pigment_pattern), image])]},
					%colormap
					{vframe, enumerate_colormap_controls(proplists:get_value(colormap_list, PovRay, []), 0), [hook(open, ['and', [member, ?KEY(pigment_colormap), true], 
							['not', [member, ?KEY(pigment_pattern), color,  image, checker, brick, hexagon]]])]}
							
				], [{title, ?__(90,"Pigment")}, {minimized, proplists:get_value(pigment_minimized, PovRay, true)}, key(pigment_minimized)]},
				{vframe, [
					{hframe, [
						{label, ?__(31,"Pattern")}, {menu, [
							{?__(91,"None"), none}, {?__(33,"Image"), image}, {?__(34,"Agate"), agate}, {?__(35,"Boxed"), boxed}, {?__(36,"Bozo"), bozo}, {?__(37,"Brick"), brick},
							{?__(38,"Bumps"), bumps}, {?__(39,"Cells"), cells}, {?__(40,"Checker"), checker}, {?__(41,"Crackle"), crackle}, {?__(42,"Cylindrical"), cylindrical},
							{?__(43,"Dents"), dents}, {?__(44,"Gradient"), gradient}, {?__(45,"Granite"), granite}, {?__(46,"Hexagon"), hexagon}, {?__(47,"Leopard"), leopard},
							{?__(48,"Marble"), marble}, {?__(49,"Onion"), onion}, {?__(50,"Planar"), planar}, {?__(51,"Quilted"), quilted}, {?__(52,"Radial"), radial},
							{?__(53,"Ripples"), ripples}, {?__(54,"Slope"), slope}, {?__(55,"Spherical"), spherical}, {?__(56,"Spiral1"), spiral1}, {?__(57,"Spiral2"), spiral2}, {?__(58,"Waves"), waves},
							{?__(59,"Wood"), wood}, {?__(60,"Wrinkles"), wrinkles}], proplists:get_value(normal_pattern, PovRay, none), [key(normal_pattern), layout]},
						{hframe, [
							{?__(92,"Normal Map"), proplists:get_value(normal_normalmap, PovRay, false), [key(normal_normalmap), layout]},
							{button, ?__(93,"New Entry"), done, [key(new_normalmap), hook(enable, [member, ?KEY(normal_normalmap), true])]}
						], [hook(open, ['not', [member, ?KEY(normal_pattern), none, image, hexagon, checker, brick, facets]])]},
						{hframe, [
							{?__(94,"Normal Components"), proplists:get_value(normal_components, PovRay, false), [key(normal_components)]}
						], [hook(open, [member, ?KEY(normal_pattern), hexagon, checker, brick])]}
					]},
					{hframe, [{label, ?__(95,"Magnitude")}, {text, proplists:get_value(normal_magnitude, PovRay, 0.5), [key(normal_magnitude)]}], [hook(enable, ['not', [member, ?KEY(normal_pattern), none]])]},
					{vframe, [
						{?__(63,"Modifiers"), proplists:get_value(normal_modifiers, PovRay, false), [key(normal_modifiers), layout]},
						{hframe, [
							{vframe, [{label, ?__(64,"Offset X")}, {label, ?__(65,"Rotate X")}, {label, ?__(66,"Turbulence")}]},
							{vframe, [{text, proplists:get_value(normal_off_x, PovRay, 0.0), [key(normal_off_x)]}, 
								{text, proplists:get_value(normal_rot_x, PovRay, 0.0), [key(normal_rot_x)]},
								{text, proplists:get_value(normal_turbulence, PovRay, 0.0), [key(normal_turbulence)]}]},
							{vframe, [{label, ?__(67,"Y")}, {label, ?__(67,"Y")}, {label, ?__(68,"Freq.")}]},
							{vframe, [{text, proplists:get_value(normal_off_y, PovRay, 0.0), [key(normal_off_y)]}, 
								{text, proplists:get_value(normal_rot_y, PovRay, 0.0), [key(normal_rot_y)]},
								{text, proplists:get_value(normal_frequency, PovRay, 1.0), [key(normal_frequency)]}]},
							{vframe, [{label, ?__(69,"Z")}, {label, ?__(69,"Z")}, {label, ?__(70,"Scale")}]},
							{vframe, [{text, proplists:get_value(normal_off_z, PovRay, 0.0), [key(normal_off_z)]}, 
								{text, proplists:get_value(normal_rot_z, PovRay, 0.0), [key(normal_rot_z)]},
								{text, proplists:get_value(normal_scale, PovRay, 1.0), [key(normal_scale)]}]}
						], [hook(open, [member, ?KEY(normal_modifiers), true])]}
					], [hook(open, ['not', [member, ?KEY(normal_pattern), none, image]])]},
					separator,
					%pattern specific values
					%agate
					{hframe, [{label, ?__(71,"Agate Turbulence")}, {text, proplists:get_value(n_agate_turbulence, PovRay, 1.0), [key(n_agate_turbulence)]}], [hook(open, [member, ?KEY(normal_pattern), agate])]},
					%brick
					{hframe, [
						{vframe, [{label, ?__(37,"Brick")}, {label, ?__(72,"Mortar")}]},
						{vframe, [{menu, normalmap_menu(), proplists:get_value(brick_normal, PovRay, agate), [key(brick_normal)]}, 
							{menu, normalmap_menu(), proplists:get_value(mortar_normal, PovRay, agate), [key(mortar_normal)]}
						], [hook(enable, [member, ?KEY(normal_components), true])]},
						{vframe, [{label, ?__(73,"Size X")}, {label, ?__(74,"Size")}]},
						{vframe, [{text, proplists:get_value(n_brick_size_x, PovRay,7.5), [key(n_brick_size_x)]}, {text, proplists:get_value(n_mortar_size, PovRay,0.5), [key(n_mortar_size)]}]},
						{vframe, [{label, "Y"}]},
						{vframe, [{text, proplists:get_value(n_brick_size_y, PovRay,2.5), [key(n_brick_size_y)]}]},
						{vframe, [{label, "Z"}]},
						{vframe, [{text, proplists:get_value(n_brick_size_z, PovRay,4.0), [key(n_brick_size_z)]}]}
					], [hook(open, [member, ?KEY(normal_pattern), brick])]},
					%checker
					{hframe, [
						{label, ?__(75,"Checker 1")}, {menu, normalmap_menu(), proplists:get_value(checker_normal1, PovRay, agate), [key(checker_normal1), hook(enable, [member, ?KEY(normal_components), true])]},
						{label, ?__(76,"Checker 2")}, {menu, normalmap_menu(), proplists:get_value(checker_normal2, PovRay, agate), [key(checker_normal2), hook(enable, [member, ?KEY(normal_components), true])]}
					], [hook(open, [member, ?KEY(normal_pattern), checker])]},
					%crackle
					{hframe, [
						{vframe, [{label, ?__(77,"Crackle Metric")}, {label, ?__(78,"Crackle Offset")}, {?__(79,"Crackle Solid"), proplists:get_value(n_crackle_solid, PovRay, false), [key(n_crackle_solid)]}]},
						{vframe, [{slider, {text, proplists:get_value(n_crackle_metric, PovRay, 2), [range({0, 10}), key(n_crackle_metric)]}},
							{slider, {text, proplists:get_value(n_crackle_offset, PovRay, 0.0), [range({0.0, 10.0}), key(n_crackle_offset)]}}]}
					], [hook(open, [member, ?KEY(normal_pattern), crackle])]},
					%gradient
					{hframe, [{label, ?__(80,"Gradient Axis")}, {menu, [{?__(81,"X"), x}, {?__(67,"Y"), y}, {?__(69,"Z"), z}], proplists:get_value(n_gradient_axis, PovRay, x), [key(n_gradient_axis)]}
						], [hook(open, [member, ?KEY(normal_pattern), gradient])]},
					%hexagon
					{hframe, [
						{label, ?__(82,"Hex. 1")}, {menu, normalmap_menu(), proplists:get_value(hexagon_normal1, PovRay, agate), [key(hexagon_normal1), hook(enable, [member, ?KEY(normal_components), true])]},
						{label, ?__(83,"Hex. 2")}, {menu, normalmap_menu(), proplists:get_value(hexagon_normal2, PovRay, agate), [key(hexagon_normal2), hook(enable, [member, ?KEY(normal_components), true])]},
						{label, ?__(84,"Hex. 3")}, {menu, normalmap_menu(), proplists:get_value(hexagon_normal3, PovRay, agate), [key(hexagon_normal3), hook(enable, [member, ?KEY(normal_components), true])]}
					], [hook(open, [member, ?KEY(normal_pattern), hexagon])]},
					%quilted
					{hframe, [
						{label, ?__(85,"Quilted 1")}, {text, proplists:get_value(n_quilted_value1, PovRay, 1.0), [key(n_quilted_value1)]},
						{label, ?__(86,"Quilted 2")}, {text, proplists:get_value(n_quilted_value2, PovRay, 1.0), [key(n_quilted_value2)]}
					], [hook(open, [member, ?KEY(normal_pattern), quilted])]},
					%Spiral
					{hframe, [
						{label, ?__(87,"Spiral Arms")}, {text, proplists:get_value(n_spiral_arms, PovRay, 5), [key(n_spiral_arms)]}
					], [hook(open, [member, ?KEY(normal_pattern), spiral1, spiral2])]},
					%Image
					{vframe, [
						{hframe, [{hradio, [{?__(88,"user"), user} | enumerate_image_maps(Maps)], NormalImage, [key(n_image_type)] }]},
						{hframe, [{label, ?__(89,"Filename")}, 
							{button, {text, proplists:get_value(n_image_user_file, PovRay, []), [key(n_image_user_file), {props, BrowseProps}] }}
						], [hook(enable, [member, ?KEY(n_image_type), user])]}
					], [hook(open, [member, ?KEY(normal_pattern), image])]},
					%normalmap
					{vframe, enumerate_normalmap_controls(proplists:get_value(normalmap_list, PovRay, []), 0), [hook(open, ['and', [member, ?KEY(normal_normalmap), true], 
							['not', [member, ?KEY(normal_pattern), none,  image, checker, brick, hexagon, facets]]])]}
				], [{title, ?__(96,"Normal")}, {minimized, proplists:get_value(normal_minimized, PovRay, true)}, key(normal_minimized)]}
			], [{title, ?__(97,"Texture")}, {minimized, proplists:get_value(texture_minimized, PovRay, true)}, key(texture_minimized)]}
		], [hook(open, [member, ?KEY(ghost_material), false])]}
		
		], [{title, ?__(98,"POV-Ray Options")}, {minimized, proplists:get_value(minimized, PovRay, true)}, key(minimized)]}
	].

enumerate_image_maps([])->
	[];
enumerate_image_maps([{MapType, _I} | Maps])->
	[{atom_to_list(MapType), MapType} | enumerate_image_maps(Maps)].
enumerate_colormap_controls([], _M)->
	[];
enumerate_colormap_controls([{ColorLevel, ColorValue} | ColorMaps], M)->
	Controls = [{hframe, [{slider, {text, ColorLevel, [colorlevel(M), range({0.0, 1.0})]}}, {color, ColorValue, [colorvalue(M)]}, {button, ?__(1,"Delete"), done, [colordelete(M)]}]}],
	List = Controls++enumerate_colormap_controls(ColorMaps, M + 1),
	List.
	
enumerate_normalmap_controls([], _M)->
	[];
enumerate_normalmap_controls([{NormalLevel, NormalValue} | NormalMaps], M)->
	Controls = [{hframe, [{slider, {text, NormalLevel, [normallevel(M), range({0.0, 1.0})]}}, {menu, normalmap_menu(), NormalValue, [normalvalue(M)]}, {button, ?__(1,"Delete"), done, [normaldelete(M)]}]}],
	List = Controls++enumerate_normalmap_controls(NormalMaps, M + 1),
	List.
	
normalmap_menu()->
	[{?__(1,"Agate"), agate}, {?__(2,"Boxed"), boxed}, {?__(3,"Bozo"), bozo}, {?__(4,"Brick"), brick},
	{?__(5,"Bumps"), bumps}, {?__(6,"Cells"), cells}, {?__(7,"Checker"), checker}, {?__(8,"Crackle"), crackle}, {?__(9,"Cylindrical"), cylindrical},
	{?__(10,"Dents"), dents}, {?__(11,"Facets"), facets}, {?__(12,"Gradient"), gradient}, {?__(13,"Granite"), granite}, {?__(14,"Hexagon"), hexagon}, {?__(15,"Leopard"), leopard},
	{?__(16,"Marble"), marble}, {?__(17,"Onion"), onion}, {?__(18,"Planar"), planar}, {?__(19,"Quilted"), quilted}, {?__(20,"Radial"), radial},
	{?__(21,"Ripples"), ripples}, {?__(22,"Slope"), slope}, {?__(23,"Spherical"), spherical}, {?__(24,"Spiral1"), spiral1}, {?__(25,"Spiral2"), spiral2}, {?__(26,"Waves"), waves}].

material_result(_Name, Mat, Res)->
	%Rip out all povray material properties
	{Found, Remaining} = rip_all(?TAG, Res),
	
	%retrieve colormap entries
    NewColorMap = proplists:get_value(new_colormap, Found, false),
	{ColorMapNC, RemainingCD} = extract_map(Found, NewColorMap, {colorlevel, colorvalue, colordelete},
		fun()->[{0.0, {0.0, 0.0, 0.0, 1.0}}] end),
	%retrieve normalmap entries
	NewNormalMap = proplists:get_value(new_normalmap, Found, false),
	{NormalMapNC, RemainingND} = extract_map(RemainingCD, NewNormalMap, {normallevel, normalvalue, normaldelete},
		fun()->[{0.0, agate}] end),
	
	FoundCM = [{colormap_list, ColorMapNC}, {normalmap_list, NormalMapNC} | RemainingND],
	
	NewMat = [{?TAG, FoundCM} | lists:keydelete(?TAG, 1, Mat)],
    {NewMat, Remaining}.

extract_map(Found, New, {Level, Value, Delete}, NewFunc)->
	{Levels, RemainingL} = rip_all(Level, Found),
	{Values, RemainingV} = rip_all(Value, RemainingL),
	{Deletes, RemainingD} = rip_all(Delete, RemainingV),
	Lsort = sort(Levels, 0),
	Vsort = sort(Values, 0),
	Dsort = sort(Deletes, 0),
	Map = compose_map(Lsort, Vsort, Dsort),
	case New of
		false -> MapNew = Map;
		true -> MapNew = Map++NewFunc()
	end,
	{MapNew, RemainingD}.
	
compose_map([], _V, _D)->
	[];
compose_map([{_, CL} | Levels], [{_, CV} | Values], [{_, CD} | Deletes])->
	case CD of
		true-> compose_map(Levels, Values, Deletes);
		false->
			[{CL, CV} | compose_map(Levels, Values, Deletes)]
	end.

sort([], _Inc)->
	[];
sort([{M, Val} | List], Increment)->
	case M of
		Increment -> [{M, Val} | sort(List, Increment + 1)];
		_ -> sort(List ++ [{M, Val}], Increment)
	end.
%construct light dialog
light_dialog(Name, Light)->
	OpenGL = proplists:get_value(opengl, Light),
    PovRay = proplists:get_value(?TAG, Light, []),
	Type = proplists:get_value(type, OpenGL, []),
	
	case Type of
		ambient ->[];
		_ ->
			[
				{vframe, [ 
					{hframe, [{label, ?__(1,"Power")}, {text, proplists:get_value(light_power, PovRay, 1.0), [key(light_power)]},
						{?__(2,"Shadows"), proplists:get_value(shadows, PovRay, true), [key(shadows)]}]},
					{hframe, [{label, ?__(3,"Fade Power")}, {text, proplists:get_value(fade_power, PovRay, 0.0), [key(fade_power)]}, 
						{label, ?__(4,"Fade Distance")}, {text, proplists:get_value(fade_distance, PovRay, 1.0), [key(fade_distance)]}]},
					{hframe, [{?__(5,"Media Interaction"), proplists:get_value(media_interaction, PovRay, true), [key(media_interaction)]},
						{?__(6,"Media Attenuation"), proplists:get_value(media_attenuation, PovRay, false), [key(media_attenuation)]}
					]},
					{hframe, [{?__(7,"Photons"), proplists:get_value(photons, PovRay, false), [key(photons)]},
						{hframe, [{?__(8,"Refraction"), proplists:get_value(refraction, PovRay, true), [key(refraction)]}, 
							{?__(9,"Reflection"), proplists:get_value(reflection, PovRay, true), [key(reflection)]}], [hook(enable, [member, ?KEY(photons), true])]}
					]},
					separator,
					{vframe, light_dialog(Name, Type, PovRay)}
				], [{title, ?__(10,"POV-Ray Options")}, {minimized, proplists:get_value(minimized, PovRay, true)}, key(minimized)]}
			]
	end.
light_dialog(_Name, spot, PovRay)->
	[
		{hframe, [{label, ?__(11,"Tightness")}, {slider, {text, proplists:get_value(tightness, PovRay, 0.0), [range({0.0, 100.0}), key(tightness)]}}]},
		{?__(12,"Cylinder"), proplists:get_value(cylinder, PovRay, false), [key(cylinder)]}
	];
light_dialog(_Name, area, PovRay)->
	[
		{hframe, [{label, ?__(13,"Size 1")}, {text, proplists:get_value(size_1, PovRay, 2), 
			[key(size_1)]}, {label, ?__(14,"Size 2")}, {text, proplists:get_value(size_2, PovRay, 2), [key(size_2)]}]},
		{hframe, [
			{menu, [{?__(15,"No Adaptive"), none},{?__(16,"Adaptive 1"), 1}, {?__(17,"Adaptive 2"), 2}, {?__(18,"Adaptive 3"), 3}, {?__(19,"Adaptive 4"), 4}], 
				proplists:get_value(adaptive, PovRay, none), [key(adaptive)]},
			{?__(20,"Jitter"), proplists:get_value(jitter, PovRay, false), [key(jitter)]}, {?__(21,"Circular"), proplists:get_value(circular, PovRay, false), [key(circular)]},
			{?__(22,"Orient"), proplists:get_value(orient, PovRay, false), [key(orient)]}
		]}
	];
light_dialog(_Name, _Type, _Povray)->
	[].
light_result(_Name, Light, Res)->
	OpenGL = proplists:get_value(opengl, Light),
	Type = proplists:get_value(type, OpenGL, []),
	case Type of
		ambient -> {Light, Res};
		_ ->
			{Found, Remaining} = rip_all(?TAG, Res),
			 NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Light)],
			{NewLight, Remaining}
	end.

clean_name([]) ->
	[];
clean_name([L | Name])->
	case L of
		32 -> [95 | clean_name(Name)]; %space to underscore
		45 -> [95 | clean_name(Name)]; %dash to underscore
		%92 -> [47 | clean_name(Name)]; %backslash to forward slash
		Val -> 
			case Val<48 of
				true -> [95 | clean_name(Name)];
				false ->
					case Val>57 of
						false -> [Val | clean_name(Name)];
						true ->
							case Val<65 of
								true -> [95 | clean_name(Name)];
								false -> 
									case Val>90 of
										false-> [Val | clean_name(Name)];
										true->
											case Val<97 of
												true -> [95 | clean_name(Name)];
												false ->
													case Val>122 of
														false-> [Val | clean_name(Name)];
														true->[95 | clean_name(Name)]
													end
											end
									end
							end
					end
			end
	end.

%pulls out all the values stored as {{KeyTag, SubKey}, Value}
%returns {ListOfFound, ListRemaining}
%ListOfFound is a list of {SubKey, Value}
rip_all(KeyTag, List)->
	Keys = proplists:get_keys(List),
	rip_all(KeyTag, Keys, List).
rip_all(KeyTag, [Key | Keys], List)->
	case rip_keytag(KeyTag, Key) of
		true ->
			{_SetTag, SubTag} = Key,
			Value = proplists:get_value(Key, List),
			ListNext = proplists:delete(Key, List),
			{Found, Remaining} = rip_all(KeyTag, Keys, ListNext),
			{[{SubTag, Value} | Found], Remaining};
		false ->
			rip_all(KeyTag, Keys, List)
	end;
rip_all(_K, _KL, List)->
	{[], List}.
	
rip_keytag(KeyTag, {SetTag, _})->
	case KeyTag of
		SetTag -> true;
		_ -> false
	end;
rip_keytag(_KT, _ST)->
	false.
    
%% Set and get preference variables saved in the .wings file for this module
set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

set_user_prefs(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    [{Key,Val}] = get_prefs([{Key,Def}]),
    Val.

get_prefs(KeyDefs) when is_list(KeyDefs) ->
    get_prefs_1(KeyDefs, make_ref()).

get_prefs_1([], _Undefined) ->
    [];
get_prefs_1([{Key,Def}|KeyDefs], Undefined) ->
    [{Key,case wpa:scene_pref_get(?MODULE, Key, Undefined) of
	      Undefined ->
		  wpa:pref_get(?MODULE, Key, Def);
	      Val ->
		  Val
	  end}|get_prefs_1(KeyDefs, Undefined)].

get_user_prefs(KeyDefs) when is_list(KeyDefs) ->
    [{Key,wpa:pref_get(?MODULE, Key, Def)} || {Key,Def} <- KeyDefs].

%% Set and get global variables (in the process dictionary) 
%% per wings session for this module.

set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    put({?MODULE,Name}, Value).

get_var(Name) ->
    get({?MODULE,Name}).

erase_var(Name) ->
    erase({?MODULE,Name}).
	
%% General purpose hook handling is_minimized and is_disabled.
%% Does lookup in Store for combinations of values.
%%
hook(Props) when is_list(Props) ->
    {hook,
     fun (is_minimized, {_Var,I,Store}) ->
	     case proplists:lookup(open, Props) of
		 {_,Expr} ->
		     not hook_eval(Expr, I, Store);
		 none -> void
	     end;
	 (is_disabled, {_Var,I,Store}) ->
	     case proplists:lookup(enable, Props) of
		 {_,Expr} ->
		     not hook_eval(Expr, I, Store);
		 none -> void
	     end;
	 (_, _) -> void
     end}.

hook(Op, Expr) -> hook([{Op,Expr}]).
    
hook_eval(['not',Expr], I, Store) ->
    not hook_eval(Expr, I, Store);
hook_eval(['and'|Exprs], I, Store) ->
    hook_and(Exprs, I, Store);
hook_eval(['or'|Exprs], I, Store) ->
    hook_or(Exprs, I, Store);
hook_eval([member,Expr|Keys], I, Store) ->
    lists:member(hook_eval(Expr, I, Store), Keys);
hook_eval([key,Key], I, Store) ->
    hook_key(Key, I, Store);
hook_eval(Key, I, Store) when not is_list(Key) ->
    hook_key(Key, I, Store).

hook_key(Key, I, Store) when is_integer(Key) ->
    gb_trees:get(I+Key, Store);
hook_key(Key, _I, Store) ->
    gb_trees:get(Key, Store).

hook_and([Expr], I, Store) -> 
    hook_eval(Expr, I, Store);
hook_and([Expr|Exprs], I, Store) ->
    hook_eval(Expr, I, Store) andalso hook_and(Exprs, I, Store).

hook_or([Expr], I, Store) -> 
    hook_eval(Expr, I, Store);
hook_or([Expr|Exprs], I, Store) ->
    hook_eval(Expr, I, Store) orelse hook_or(Exprs, I, Store).

