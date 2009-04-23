%%
%%  wpc_kerky.erl
%%
%%     Kerkythea Plugin User Interface.
%%
%%  Copyright (c) 2007 Chris Hegarty
%%
%%     $Id$
%%

-module(wpc_kerky).

-export([init/0,menu/2,command/2,dialog/2]).

-include("e3d.hrl").
-include("e3d_image.hrl").

-define(TAG, kerkythea).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, kerkythea_render).

-define(DEF_RENDERER, "C:/Program Files/Kerkythea Rendering System/Kerkythea.exe").
-define(DEF_LINUX_RENDERER, "kerkythea").
-define(DEF_DIALOGS, auto).
-define(DEF_RENDER_FORMAT, png).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_RENDERARGS, " ").
-define(DEF_LINUX_RENDERARGS, " ").

-record(camera_info, {pos,dir,up,fov, az, el, track, dist}).

key(Key) -> {key, ?KEY(Key)}.
exkey(Key) ->{key, Key}.

range(T) -> {range,T}.


%Gets if os is win32
is_windows()->
	T = os:type(),
	L = tuple_to_list(T),
	lists:member(win32, L).
	
export_matrix()->
	%e3d_mat:scale(-1.0, 1.0, 1.0).
	AxisXForm = {-1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0},
	Rotate = e3d_mat:rotate(180.0, {0.0, 1.0, 0.0}),
	e3d_mat:mul(AxisXForm, Rotate).
	
export_transform(Contents) ->
    e3d_file:transform(Contents, export_matrix()).
	
export_transform_pos(Pos)->
    e3d_mat:mul_point(export_matrix(), Pos).
	
export_transform_vec(Vec)->
    e3d_mat:mul_vector(export_matrix(), Vec).
    
export_matrix_cam()->
	AxisXForm = {-1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0},
	AxisXForm.
	
export_transform_cam_vec(Vec)->
    e3d_mat:mul_vector(export_matrix_cam(), Vec).

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
	
	case is_windows() of
		true-> LocalArgs = ?DEF_RENDERARGS;
		false-> LocalArgs = ?DEF_LINUX_RENDERARGS
	end,
	set_var(renderargs, get_pref(renderargs, LocalArgs)),
	set_var(overwrite_exportfile, get_pref(overwrite_exportfile, true)),
    ok.
    
%insert menu items into export, export selected, render, and plugin preferences
menu({file,export}, Menu) ->
    case get_var(dialogs) of
    	false -> Menu;
	_ -> Menu++[{"Kerkythea (.xml)",?TAG,[option]}]
    end;
menu({file,export_selected}, Menu) ->
    case get_var(dialogs) of
    	false -> Menu;
	_ -> Menu++[{"Kerkythea (.xml)",?TAG,[option]}]
    end;
menu({file,render}, Menu) ->
    case get_var(renderer) of
    	false -> Menu;
	_ -> Menu++[{"Kerkythea",?TAG,[option]}]
    end;
menu({edit, plugin_preferences}, Menu) ->
	Menu++[{"Kerkythea",?TAG}];
menu(_, Menu) ->
    Menu.

%dialog and file type properties    
props(render, _Attr) ->
    [{title,"Render"}, {ext,".png"}, {ext_desc,"Kerkythea Image"}];
props(export, _Attr) ->
    [{title,"Export"},{ext,".xml"},{ext_desc,"Kerkythea File"}];
props(export_selected, _Attr) ->
    [{title,"Export Selected"},{ext,".xml"},{ext_desc,"Kerkythea File"}].

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
	
	[{dialogs, Dialogs}, {renderer, Renderer},
	{renderargs, RenderArgs}, {overwrite_exportfile, OverwriteExport}] = 
		get_user_prefs([{dialogs,?DEF_DIALOGS},{renderer, LocalRenderer},
		{renderargs, LocalArgs}, {overwrite_exportfile, true}]),

    Dialog =
		[{vframe, [
			{hframe, [
				{menu,[{"Disabled Dialogs",disabled},
					{"Automatic Dialogs",auto},
					{"Enabled Dialogs",enabled}
				], Dialogs, [{key, dialogs}]}
			]},
			{hframe,[ {label,"Executable"}, {button, {text, Renderer, [{key,renderer}, wings_job:browse_props()]}}]},
			{hframe, [ {label, "Arguments"}, {text, RenderArgs, [{key, renderargs}]}]},
			{hframe, [ {"Overwrite Export File on Render", OverwriteExport, [{key, overwrite_exportfile}]}]}
		]}],
    wpa:dialog("Kerkythea Options", Dialog, fun (Attr) -> pref_result(Attr,St) end).
pref_result(Attr, St) ->
    set_user_prefs(Attr),
    init_pref(),
    St.

%Add necessary properties to lists, setup export function
do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Kerkythea Export Options", export_dialog(Op),
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
	[{Pos, Dir, Up}, Fov, Azimuth, Elevation, Track, Dist] = wpa:camera_info([pos_dir_up, fov, azimuth, elevation, tracking, distance_to_aim]),
	Pos1 = export_transform_pos(Pos),
	Dir1 = export_transform_vec(Dir),
	Up1 = export_transform_vec(Up),
	CameraInfo = #camera_info{pos=Pos1, dir=Dir1, up=Up1, fov=Fov, az=Azimuth, el=Elevation, track=Track, dist=Dist},
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
    
unique_filename(Name, Names, Count)->
	case lists:member(Name, Names) of
		false->Name;
		_ ->
			unique_filename(filename:rootname(Name)++integer_to_list(Count)++filename:extension(Name), Names, Count + 1)
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
	
	OverwriteRenderEx = get_var(overwrite_exportfile),
	
	case Render of
		false -> ExportFile = Filename;
		true ->
			case OverwriteRenderEx of
				true -> ExportFile = filename:rootname(Filename)++"_export.xml";
				_ -> 
					case file:list_dir(ExportDir) of
						{ok, Filenames} -> 
							ExportFile = filename:absname_join(ExportDir,
								unique_filename(filename:basename(filename:rootname(Filename))++"_export.xml", Filenames, 0));
						_ -> ExportFile = filename:rootname(Filename)++"_export.xml"
					end
			end
	end,
	
	case proplists:get_value(fix_win_dim, Attr, false) of
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

	{ok, F} = file:open(ExportFile, [write]),
	
	io:format(F, "<!-- ~s: Exported from ~s -->\n\n", [filename:basename(ExportFile), Creator]),
	
	%base node
	io:put_chars(F, "<Root Label=\"Default Kernel\" Name=\"\" Type=\"Kernel\">\n"),
	

	RenderType = proplists:get_value(render_preset, Attr, pm_medium),
	AA = proplists:get_value(antialias_type, Attr, none),
	AAThreshold = proplists:get_value(antialias_threshold, Attr, 0.3),
	Translucence = proplists:get_value(translucence, Attr, normal),
	Caustics = proplists:get_value(caustics_type, Attr, none),
	
	case RenderType of
		raytrace -> case Caustics of
				normal -> BaseOpts = {low, Translucence, pseudo};
				_ -> BaseOpts = {low, Translucence, Caustics}
			end;
		raytrace_high -> case Caustics of
				normal -> BaseOpts = {high, Translucence, pseudo};
				_ -> BaseOpts = {high, Translucence, Caustics}
			end;
		pm_quick -> BaseOpts = {low, Translucence, Caustics};
		pm_medium -> BaseOpts = {medium, Translucence, Caustics};
		pm_high -> BaseOpts = {high, Translucence, Caustics};
		pt_quick -> BaseOpts = {low, Translucence, Caustics};
		pt_medium -> BaseOpts = {medium, Translucence, Caustics};
		pt_high -> BaseOpts = {high, Translucence, Caustics};
		pt_progressive -> BaseOpts = {high, Translucence, Caustics};
		_ -> BaseOpts = {high, normal, normal}
	end,
	
	ThreadCount = proplists:get_value(render_thread_count, Attr, 1),
	
	case RenderType of
		raytrace ->  case ThreadCount of
				1 -> export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, false, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, false, BaseOpts})
			end,
			export_directlight_estimator(F, BaseOpts);
		raytrace_high ->case ThreadCount of
				1 -> export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, false, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, false, BaseOpts})
			end,
			export_directlight_estimator(F, BaseOpts);
		pm_quick ->case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, false, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, false, BaseOpts})
			end,
			export_interreflection_estimator(F, BaseOpts),
			export_density_estimator(F, BaseOpts),
			export_directlight_estimator(F, BaseOpts);
		pm_medium -> case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, false, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, false, BaseOpts})
			end,
			export_interreflection_estimator(F, BaseOpts),
			export_density_estimator(F, BaseOpts),
			export_directlight_estimator(F, BaseOpts);
		pm_high -> case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, false, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, false, BaseOpts})
			end,
			export_interreflection_estimator(F, BaseOpts),
			export_density_estimator(F, BaseOpts),
			export_directlight_estimator(F, BaseOpts);
		pt_quick ->case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, true, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, true, BaseOpts})
			end,
			export_directlight_estimator(F, BaseOpts);
		pt_medium -> case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, true, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, true, BaseOpts})
			end,
			export_directlight_estimator(F, BaseOpts);
		pt_high ->case ThreadCount of
				1 ->export_standard_raytracer(F, "Standard Ray Tracer", {AA, AAThreshold, true, BaseOpts});
				_ ->multithreadedRayTracer(F, ThreadCount,  {AA, AAThreshold, true, BaseOpts})
			end,
			export_directlight_estimator(F, BaseOpts);
		pt_progressive->case ThreadCount of
				1 ->export_progressive_raytracer(F, "Standard Ray Tracer");
				_ ->multiPTP(F, ThreadCount)
			end,
			export_directlight_estimator(F, BaseOpts);
		pt_bpt->
			case ThreadCount of
				1 ->export_bidirectional_raytracer(F, "Bidirectional Path Tracer");
				_ ->multiBIPT(F, ThreadCount)
			end;
		mlt ->case ThreadCount of
				1 ->export_mlt_raytracer(F, "Metropolis Light Transport", {0, "temp_mlt.jpg"});
				_ ->multiMLT(F, ThreadCount,  {0, "temp_mlt.jpg"})
			end;
		mlt_bpt ->case ThreadCount of
				1 ->export_mlt_raytracer(F, "Metropolis Light Transport", {1, "temp_bmlt.jpg"});
				_ ->multiMLT(F, ThreadCount,  {1, "temp_bmlt.jpg"})
			end
	end,
	
	expOCTTREE(F),
	expTONEMAPPING(F),
	
	io:put_chars(F, "<Object Identifier=\"./Scenes/My Scene\" Label=\"Default Scene\" Name=\"My Scene\" Type=\"Scene\">\n"),
	%we need to find the first "on" ambient light, if there is one
	Lights = proplists:get_value(lights, Attr, []),
	Ambient = get_ambient_light(Lights),
	export_global(F, Attr, Ambient),
	
	%objects
	export_objects(F, Objs, Mats, Attr, 0, ExportDir),
	
	%Lights only include point (as omni light) and spot light
	export_lights(F, Lights),
	
	%camera
	#camera_info{fov=Fov} = proplists:lookup(camera_info, Attr),
	export_camera(F, Attr, Fov, Width, Height),
	
	io:put_chars(F, "\t<Parameter Name=\"./Cameras/Active\" Type=\"String\" Value=\"Wings View\"/>\n"),
	io:put_chars(F, "</Object>\n"),
	
	case ThreadCount of
		1-> 
			case RenderType of
				pt_bpt->RayTraceName = "Bidirectional Path Tracer";
				mlt->RayTraceName = "Metropolis Light Transport";
				mlt_bpt->RayTraceName = "Metropolis Light Transport";
				_ -> RayTraceName = "Standard Ray Tracer"
			end;
		_-> RayTraceName = "Threaded Ray Tracer"
	end,
	
	case RenderType of
	
		raytrace -> expSCENE(F, true, RayTraceName);
		raytrace_high -> expSCENE(F, true, RayTraceName);
		pt_quick -> expSCENE(F, true, RayTraceName);
		pt_medium -> expSCENE(F, true, RayTraceName);
		pt_high -> expSCENE(F, true, RayTraceName);
		pt_progressive -> expSCENE(F, true, RayTraceName);
		pt_bpt -> expBIPT_SCENE(F, RayTraceName);
		mlt ->expMLT_SCENE(F, RayTraceName);
		mlt_bpt -> expMLT_SCENE(F, RayTraceName);
		_ -> expSCENE(F, false, RayTraceName)
	end,
	
	io:put_chars(F, "<Parameter Name=\"./Scenes/Active\" Type=\"String\" Value=\"My Scene\"/>\n"),
	io:put_chars(F, "<Parameter Name=\"./Libraries/Active\" Type=\"String\" Value=\"Material Librarian\"/>\n"),
	io:put_chars(F, "</Root>\n"),

	file:close(F),
	
	
    case {get_var(renderer), Render} of
		{_, false} ->
			wings_job:export_done(ExportTS),
			io:nl();
		{false, true} ->
			io:put_chars("Kerkythea executable path not set\n"),
			wings_job:export_done(ExportTS),
			io:nl();
		{Renderer, true} ->
			ArgStr = wings_job:quote(filename:basename(ExportFile)),
			RA = get_var(renderargs),
				
			PortOpts = [{cd,filename:dirname(ExportFile)}],
			Handler =
				fun (Status) ->
					set_var(rendering, false),
					case Status of
						ok -> {RenderFormat, Filename}; %filename:dirname(ExportFile)++"/output.png"};
						_  -> Status
					end
				end,
				%file:delete(Filename),
				set_var(rendering, true),
				wings_job:render(ExportTS, Renderer, ArgStr++" -o "++filename:basename(Filename)++" "++RA, PortOpts, Handler)
    end.
    
multithreadedRayTracer(F, ThreadCount, Options) ->
	threadedRayTracer(F, 0, ThreadCount, Options),
	io:put_chars(F, "<Object Identifier=\"./Ray Tracers/Threaded Ray Tracer\" Label=\"Threaded Ray Tracer\" Name=\"Threaded Ray Tracer\" Type=\"Ray Tracer\">\n"),
	threadedNames(F, 0, ThreadCount),
	io:put_chars(F, "</Object>\n").
	
threadedRayTracer(F, ThreadNumber, MaxCount, Options) ->
	case ThreadNumber<MaxCount of
		true -> Name = "#"++io_lib:write(ThreadNumber),
			export_standard_raytracer(F, Name, Options),
			threadedRayTracer(F, ThreadNumber + 1, MaxCount, Options);
		false ->ok
	end.
	
threadedNames(F, ThreadNumber, MaxCount)->
	case ThreadNumber<MaxCount of
		true -> io:format(F, "\t<Parameter Name=\"Thread #~p\" Type=\"String\" Value=\"#~p\"/>\n", [ThreadNumber, ThreadNumber]),
			threadedNames(F, ThreadNumber + 1, MaxCount);
		false -> ok
	end.
	
multiPTP(F, ThreadCount) ->
	threadedPTP(F, 0, ThreadCount),
	io:put_chars(F, "<Object Identifier=\"./Ray Tracers/Threaded Ray Tracer\" Label=\"Threaded Ray Tracer\" Name=\"Threaded Ray Tracer\" Type=\"Ray Tracer\">\n"),
	threadedNames(F, 0, ThreadCount),
	io:put_chars(F, "</Object>\n").

threadedPTP(F, ThreadNumber, MaxCount)->
	case ThreadNumber<MaxCount of
		true -> Name = "#"++io_lib:write(ThreadNumber),
			export_progressive_raytracer(F, Name),
			threadedPTP(F, ThreadNumber + 1, MaxCount);
		false ->ok
	end.
	
multiBIPT(F, ThreadCount) ->
	threadedBIPT(F, 0, ThreadCount),
	io:put_chars(F, "<Object Identifier=\"./Ray Tracers/Threaded Ray Tracer\" Label=\"Threaded Ray Tracer\" Name=\"Threaded Ray Tracer\" Type=\"Ray Tracer\">\n"),
	threadedNames(F, 0, ThreadCount),
	io:put_chars(F, "</Object>\n").

threadedBIPT(F, ThreadNumber, MaxCount)->
	case ThreadNumber<MaxCount of
		true -> Name = "#"++io_lib:write(ThreadNumber),
			export_bidirectional_raytracer(F, Name),
			threadedBIPT(F, ThreadNumber + 1, MaxCount);
		false ->ok
	end.
	
multiMLT(F, ThreadCount, Options) ->
	threadedMLT(F, 0, ThreadCount, Options),
	io:put_chars(F, "<Object Identifier=\"./Ray Tracers/Threaded Ray Tracer\" Label=\"Threaded Ray Tracer\" Name=\"Threaded Ray Tracer\" Type=\"Ray Tracer\">\n"),
	threadedNames(F, 0, ThreadCount),
	io:put_chars(F, "</Object>\n").

threadedMLT(F, ThreadNumber, MaxCount, Options)->
	case ThreadNumber<MaxCount of
		true -> Name = "#"++io_lib:write(ThreadNumber),
			export_mlt_raytracer(F, Name, Options),
			threadedMLT(F, ThreadNumber + 1, MaxCount, Options);
		false ->ok
	end.


get_ambient_light([])->
	void;
get_ambient_light([Light | Lights])->
	{_Name, Ps} = Light,
	OpenGL = proplists:get_value(opengl, Ps, []),
	Type = proplists:get_value(type, OpenGL, []),
	case Type of
		ambient -> Light;
		_ -> get_ambient_light(Lights)
	end.

export_global(F, Attr, Ambient)->

	%Global settings node
	io:put_chars(F, "\t<Object Identifier=\"Default Global Settings\" Label=\"Default Global Settings\" Name=\"\" Type=\"Global Settings\">\n"),
	
	case Ambient of
		void ->
			io:format(F, "\t\t<Parameter Name=\"Background Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [0.0, 0.0, 0.0]),
			io:put_chars(F, "\t\t<Parameter Name=\"Background Type\" Type=\"String\" Value=\"Background Color\"/>\n");
		{_Name, Ps} ->
			OpenGL = proplists:get_value(opengl, Ps, []),
			KT = proplists:get_value(?TAG, Ps, []),
			{Ar, Ag, Ab, _Aa} = proplists:get_value(ambient, OpenGL, {0.0, 0.0, 0.0, 0.0}),
			Type = proplists:get_value(sky, KT, background_color),
			
			Intensity = proplists:get_value(sky_intensity, KT, 1.0),
			Turbidity = proplists:get_value(sky_turbidity, KT, 2.0),
			Image = proplists:get_value(sky_file, KT, []),
			
			io:format(F, "\t\t<Parameter Name=\"Background Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Ar, Ag, Ab]),
			io:format(F, "\t\t<Parameter Name=\"Sky Turbidity\" Type=\"Real\" Value=\"~f\"/>\n", [Turbidity]),
			io:format(F, "\t\t<Parameter Name=\"Sky Intensity\" Type=\"Real\" Value=\"~f\"/>\n", [Intensity]),
			case Type of
				background_color->ok;
				sky_color -> ok;
				physical -> ok;
				_ ->
					io:format(F, "\t\t<Parameter Name=\"./Background Image/Filename\" Type=\"String\" Value=\"~s\"/>\n", [Image])
			end,
			
			io:format(F, "\t\t<Parameter Name=\"Background Type\" Type=\"String\" Value=\"~s\"/>\n", [
				case Type of
					background_color -> "Background Color";
					sky_color -> "Sky Color";
					centered_image -> "Background Centered Image";
					tiled_image -> "Background Tiled Image";
					fit_image -> "Background Fit Image";
					hemisphere -> "Hemispherical Sky";
					spherical -> "Spherical Sky";
					physical -> "Physical Sky"
				end
			])
	end,
	
	Volume = proplists:get_value(volume_light, Attr, false),
	case Volume of
		true -> io:put_chars(F, "\t\t<Parameter Name=\"Compute Volume Transfer\" Type=\"Boolean\" Value=\"1\"/>\n"),
				io:put_chars(F, "\t\t<Parameter Name=\"Transfer Recursion Depth\" Type=\"Integer\" Value=\"1\"/>\n");
		_ -> ok
	end,
	io:put_chars(F, "\t</Object>\n").
			
export_camera(F, Attr, CorrectedFOV, Width, Height)->
	
	#camera_info{pos=Pos,dir=_Dir,up=_Up, az=Az, el=El, track=_Track, dist=Dist} = proplists:lookup(camera_info, Attr),
	{Px, Py, Pz} = Pos,
	
	%%FOV, Width and Height information passed in to allow dimensions to be forced
	%%to geometry screen size
	Fov = CorrectedFOV,
	FilmHeight = wings_pref:get_value(negative_height),
	
	%The standard axis arrangement for a KT camera is a little.... odd.
	M0 = {-1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0},
	M1 = e3d_mat:mul(M0, e3d_mat:rotate(El,export_transform_cam_vec( {1.0,0.0,0.0}))),
	%The use of cam transform and the addition of 180 done to compensate for the 180 degree rotation around the vertical
	%added with KT 2008 release
	M2 = e3d_mat:mul(M1, e3d_mat:rotate(Az+180.0, export_transform_cam_vec({0.0,1.0,0.0}))),
	M5 = e3d_mat:expand(M2),
	{Ai,Bi,Ci,_Si,Dj,Ej,Fj,_Sj,Gk,Hk,Ik,_Sk,_Tx,_Ty,_Tz,_Ts} = M5,
	
	io:put_chars(F, "\t<Object Identifier=\"./Cameras/Wings View\" Label=\"Pinhole Camera\" Name=\"Wings View\" Type=\"Camera\">\n"),
	
	LensLength = 
	case catch 0.5 / math:tan(Fov*math:pi()/360.0) of
	    {'EXIT',_} -> 0.0;
	    L when is_float(L) -> L
	end,
	
	%KT 2007 format
	%io:format(F, "\t\t<Parameter Name=\"Focal Length\" Type=\"Real\" Value=\"~f\"/>\n", [LensLength]),
	%KT 2008 Format
	io:format(F, "\t\t<Parameter Name=\"Focal Length (mm)\" Type=\"Real\" Value=\"~f\"/>\n", [LensLength * FilmHeight]),
	io:format(F, "\t\t<Parameter Name=\"Film Height (mm)\" Type=\"Real\" Value=\"~f\"/>\n", [FilmHeight * 1.0]),
	
	io:format(F, "\t\t<Parameter Name=\"Resolution\" Type=\"String\" Value=\"~px~p\"/>\n", [Width, Height]),
	io:format(F, "\t\t<Parameter Name=\"Frame\" Type=\"Transform\" Value=\"~f ~f ~f ~f\n\t\t\t~f ~f ~f ~f\n\t\t\t~f ~f ~f ~f\"/>\n", [Ai,Bi,Ci,Px,Dj,Ej,Fj,Py,Gk,Hk,Ik,Pz]),
	
	io:format(F, "\t\t<Parameter Name=\"Focus Distance\" Type=\"Real\" Value=\"~f\"/>\n",[Dist]),
	%KT 2007 format
	%io:format(F, "\t\t<Parameter Name=\"Lens Radius\" Type=\"Real\" Value=\"~f\"/>\n", [proplists:get_value(aperture, Attr, 0.0)]),
	%KT 2008 format
	io:format(F, "\t\t<Parameter Name=\"f-number\" Type=\"String\" Value=\"~s\"/>\n", [proplists:get_value(fnumber, Attr, "Pinhole")]),

	io:format(F, "\t\t<Parameter Name=\"Lens Samples\" Type=\"Integer\" Value=\"~p\"/>\n", [proplists:get_value(blur_samples, Attr, 3)]),
	case proplists:get_value(camera_type, Attr, perspective) of
		spherical -> io:put_chars(F, "\t\t<Parameter Name=\"Projection\" Type=\"String\" Value=\"Spherical\"/>\n");
		cylindrical -> io:put_chars(F, "\t\t<Parameter Name=\"Projection\" Type=\"String\" Value=\"Cylindrical\"/>\n");
		parallel -> io:put_chars(F, "\t\t<Parameter Name=\"Projection\" Type=\"String\" Value=\"Parallel\"/>\n");
		_ -> io:put_chars(F, "\t\t<Parameter Name=\"Projection\" Type=\"String\" Value=\"Planar\"/>\n")
	end,
	io:put_chars(F, "\t</Object>\n").
	
export_lights(_F, [])->
	ok;
export_lights(F, [Light | Lights])->
	{Name, Ps} = Light,
	export_light(F, Name, Ps),
	export_lights(F, Lights).
	
export_light(F, Name, Ps) ->
	OpenGL = proplists:get_value(opengl, Ps, []),
	KT = proplists:get_value(?TAG, Ps, []),
	Type = proplists:get_value(type, OpenGL, []),
	case Type of
		ambient->ok;
		_ ->
			export_light(F, Name, Type, OpenGL, KT, Ps)
	end.
export_light_basics(F, _OpenGL, KT, Ps)->

	case proplists:get_value(visible, Ps, true) of
		true -> io:put_chars(F, "\t\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"1\"/>\n");
		false -> io:put_chars(F, "\t\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"0\"/>\n")
	end,

	case proplists:get_value(shadows, KT, true) of
		true -> io:put_chars(F, "\t\t<Parameter Name=\"Shadow\" Type=\"Boolean\" Value=\"1\"/>\n");
		false ->io:put_chars(F, "\t\t<Parameter Name=\"Shadow\" Type=\"Boolean\" Value=\"0\"/>\n")
	end,
	case proplists:get_value(soft_shadows, KT, false) of
		true -> io:put_chars(F, "\t\t<Parameter Name=\"Soft Shadow\" Type=\"Boolean\" Value=\"1\"/>\n");
		false ->io:put_chars(F, "\t\t<Parameter Name=\"Soft Shadow\" Type=\"Boolean\" Value=\"0\"/>\n")
	end,
	case proplists:get_value(neg_light, KT, false) of
		true -> io:put_chars(F, "\t\t<Parameter Name=\"Negative Light\" Type=\"Boolean\" Value=\"1\"/>\n");
		false ->io:put_chars(F, "\t\t<Parameter Name=\"Negative Light\" Type=\"Boolean\" Value=\"0\"/>\n")
	end,
	
	Radius = proplists:get_value(light_radius, KT, 0.2),
	{Sr, Sg, Sb} = proplists:get_value(shadowcolor, KT, {0.0, 0.0, 0.0}),
	
	io:format(F, "\t\t<Parameter Name=\"Radius\" Type=\"Real\" Value=\"~f\"/>\n", [Radius]),
	io:format(F, "\t\t<Parameter Name=\"Shadow Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Sr, Sg, Sb]),
	
	io:put_chars(F, "\t\t<Parameter Name=\"Global Photons\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Caustic Photons\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Multiplier\" Type=\"Real\" Value=\"3.14159\"/>\n").
	
export_light(F, Name, spot, OpenGL, KT, Ps)->

	io:format(F, "\t<Object Identifier=\"./Lights/~s\" Label=\"Default Light\" Name=\"~s\" Type=\"Light\">\n", [Name, Name]),
	io:put_chars(F, "\t\t<Object Identifier=\"Spot Light\" Label=\"Spot Light\" Name=\"\" Type=\"Emittance\">\n"),
	
	%Color block
	Power = proplists:get_value(light_power, KT, 1.0),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
	export_constant_texture(F, "./Radiance/", {Dr * Power, Dg * Power, Db * Power}, 3),
	
	
	Attenuation = proplists:get_value(attenuation, KT, none),
	case Attenuation of
		inverse -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse\"/>\n");
		inverse_square -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse Square\"/>\n");
		_ -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"None\"/>\n")
	end,
	
	%Spotlight block
	Radius = proplists:get_value(cone_angle, OpenGL, 30.0),
	case proplists:get_value(spot_exponent,OpenGL, 0.0) > Radius of
		true ->io:format(F, "\t\t\t<Parameter Name=\"Fall Off\" Type=\"Real\" Value=\"~f\"/>\n", [proplists:get_value(spot_exponent,OpenGL, 0.0)]);
		false ->io:format(F, "\t\t\t<Parameter Name=\"Fall Off\" Type=\"Real\" Value=\"~f\"/>\n", [Radius])
	end,
	io:format(F, "\t\t\t<Parameter Name=\"Hot Spot\" Type=\"Real\" Value=\"~f\"/>\n", [Radius]),
	io:put_chars(F, "\t\t</Object>\n"),
	
	{Pxo, Pyo, Pzo} = proplists:get_value(position, OpenGL, {0.0, 0.0, 0.0}),
	{Px, Py, Pz} = export_transform_pos({Pxo, Pyo, Pzo}),
	{Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
	{Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
	
	ZAxis = e3d_vec:norm({Dx - Px, Dy - Py, Dz - Pz}),
	case abs(e3d_vec:dot(ZAxis, {0.0, 0.0, 1.0})) < 0.9999 of
		false -> %ZAxis almost straight up
			YAxis = {0.0, 1.0, 0.0},
			XAxis = {1.0, 0.0, 0.0};
		true ->
			XAxis = e3d_vec:cross(ZAxis, {0.0, 0.0, 1.0}),
			YAxis = e3d_vec:cross(ZAxis, XAxis)
	end,
	
	%We need to transpose the vectors
	%{Ai, Bi, Ci} = XAxis,
	%{Dj, Ej, Fj} = YAxis,
	%{Gk, Hk, Ik} = ZAxis,
	{Ai, Dj, Gk} = XAxis,
	{Bi, Ej, Hk} = YAxis,
	{Ci, Fj, Ik} = ZAxis,
	
	io:format(F, "\t\t<Parameter Name=\"Focus Distance\" Type=\"Real\" Value=\"~f\"/>\n", [e3d_vec:len({Dx - Px, Dy - Py, Dz - Pz})]),
	io:format(F, "\t\t<Parameter Name=\"Frame\" Type=\"Transform\" Value=\"~f ~f ~f ~f\n\t\t\t~f ~f ~f ~f\n\t\t\t~f ~f ~f ~f\"/>\n", [Ai,Bi,Ci,Px,Dj,Ej,Fj,Py,Gk,Hk,Ik,Pz]),
	
	export_light_basics(F, OpenGL, KT, Ps),
	io:put_chars(F, "\t</Object>\n");
	
export_light(F, Name, point, OpenGL, KT, Ps)->
	io:format(F, "\t<Object Identifier=\"./Lights/~s\" Label=\"Default Light\" Name=\"~s\" Type=\"Light\">\n", [Name, Name]),
	io:put_chars(F, "\t\t<Object Identifier=\"Omni Light\" Label=\"Omni Light\" Name=\"\" Type=\"Emittance\">\n"),
	
	%Color block
	Power = proplists:get_value(light_power, KT, 1.0),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
	export_constant_texture(F, "./Radiance/", {Dr * Power, Dg * Power, Db * Power}, 3),
	
	%point block
	Attenuation = proplists:get_value(attenuation, KT, none),
	case Attenuation of
		inverse -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse\"/>\n");
		inverse_square -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse Square\"/>\n");
		_ -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"None\"/>\n")
	end,
	io:put_chars(F, "\t\t</Object>\n"),
	
	{Pxo, Pyo, Pzo} = proplists:get_value(position, OpenGL, {0.0, 0.0, 0.0}),
	{Px, Py, Pz} = export_transform_pos({Pxo, Pyo, Pzo}),
	
	io:put_chars(F, "\t\t<Parameter Name=\"Focus Distance\" Type=\"Real\" Value=\"4\"/>\n"),
	io:format(F, "\t\t<Parameter Name=\"Frame\" Type=\"Transform\" Value=\"1 0 0 ~f\n\t\t\t0 1 0 ~f\n\t\t\t0 0 1 ~f\"/>\n", [Px, Py, Pz]),
	
	export_light_basics(F, OpenGL, KT, Ps),
	io:put_chars(F, "\t</Object>\n");
	
export_light(F, _Name, area, OpenGL, KT, _Ps)->
	Mesh = proplists:get_value(mesh, OpenGL),
	
	ObjMesh = e3d_mesh:triangulate(Mesh),
	ObjMeshTx = e3d_mesh:transform(ObjMesh,export_matrix()), 
	#e3d_mesh{fs=Fs0,ns=NTab,vs=VTab} = e3d_mesh:vertex_normals(ObjMeshTx),
	
	%start model
	io:format(F, "\t<Object Identifier=\"./Models/~s\" Label=\"Default Model\" Name=\"~s\" Type=\"Model\">\n", ["AreaLight", "AreaLight"]),
	export_model(F, VTab, NTab, Fs0, true),
	
	io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["AreaLight_mat", "AreaLight_mat"]),
	export_constant_texture(F, "./Diffuse/", {0.0, 0.0, 0.0}, 3),
	io:put_chars(F, "\t\t</Object>\n"),
	
	
	Power = proplists:get_value(light_power, KT, 1.0),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
	export_emit(F, {Dr * Power, Dg * Power, Db * Power}, 2),
	
	AreaVisible = proplists:get_value(area_visible, KT, true),
	%Follow params, turn off shadow, caustics
	case AreaVisible of
		false -> io:put_chars(F, "\t\t<Parameter Name=\"Visible\" Type=\"Boolean\" Value=\"0\"/>\n");
		_ -> io:put_chars(F, "\t\t<Parameter Name=\"Visible\" Type=\"Boolean\" Value=\"1\"/>\n")
	end,
	io:put_chars(F, "\t\t<Parameter Name=\"Shadow Caster\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Shadow Receiver\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Caustics Transmitter\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Caustics Receiver\" Type=\"Boolean\" Value=\"0\"/>\n"),
	%close model
	io:put_chars(F, "\t</Object>\n"),
	
	ok;
	
export_light(F, Name, infinite, OpenGL, KT, Ps)->
	io:format(F, "\t<Object Identifier=\"./Lights/~s\" Label=\"Default Light\" Name=\"~s\" Type=\"Light\">\n", [Name, Name]),
	io:put_chars(F, "\t\t<Object Identifier=\"Omni Light\" Label=\"Omni Light\" Name=\"\" Type=\"Emittance\">\n"),
	
	%Color block
	Power = proplists:get_value(light_power, KT, 1.0),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
	export_constant_texture(F, "./Radiance/", {Dr * Power, Dg * Power, Db * Power}, 3),
	
	%point block
	Attenuation = proplists:get_value(attenuation, KT, none),
	case Attenuation of
		inverse -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse\"/>\n");
		inverse_square -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse Square\"/>\n");
		_ -> io:put_chars(F, "\t\t\t<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"None\"/>\n")
	end,
	io:put_chars(F, "\t\t</Object>\n"),
	
	{Pxo, Pyo, Pzo} = proplists:get_value(position, OpenGL, {0.0, 0.0, 0.0}),
	{Px, Py, Pz} = export_transform_pos({Pxo, Pyo, Pzo}),
	
	{Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
	{Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
	
	{Zx, Zy, Zz} = e3d_vec:norm({Px - Dx, Py - Dy, Pz - Dz}),
	%Move the position off in the Zaxis for 100 units
	Pxt = Px + (Zx * 100.0),
	Pyt = Py + (Zy * 100.0),
	Pzt = Pz + (Zz * 100.0),
	
	io:put_chars(F, "\t\t<Parameter Name=\"Focus Distance\" Type=\"Real\" Value=\"4\"/>\n"),
	io:format(F, "\t\t<Parameter Name=\"Frame\" Type=\"Transform\" Value=\"1 0 0 ~f\n\t\t\t0 1 0 ~f\n\t\t\t0 0 1 ~f\"/>\n", [Pxt, Pyt, Pzt]),
	
	export_light_basics(F, OpenGL, KT, Ps),
	io:put_chars(F, "\t</Object>\n");
	
export_light(_F, _Name, _Type, _OpenGL, _KT, _Ps)->
	ok.
	
export_objects(_F, [], _M, _A, _I, _E)->
	ok;
export_objects(F, [EObj | Objs], AllMats, Attr, Index, ExportDir)->
	#e3d_object{name=Name, obj=Obj} = EObj,
	
	SplitMeshes = e3d_mesh:split_by_material(Obj),
	
	export_objects_split(F, SplitMeshes, AllMats, Attr, 0, Name, ExportDir),
	export_objects(F, Objs, AllMats, Attr, Index + 1, ExportDir).
	
export_objects_split(_F, [], _AllMats, _Attr, _Index, _RootName, _ExportDir)->
	ok;
export_objects_split(F, [EObj | Objs], AllMats, Attr, Index, RootName, ExportDir)->
	ObjMesh = e3d_mesh:triangulate(EObj),
	%ObjMesh = e3d_mesh:merge_vertices(CMesh),
	%RMesh = e3d_mesh:vertex_normals(ObjMesh),
	#e3d_mesh{fs=Fs0,ns=NTab,vs=VTab,tx=UVTab} = e3d_mesh:vertex_normals(ObjMesh),%e3d_mesh:renumber(RMesh),
	[MatName | _ ] = e3d_mesh:used_materials(ObjMesh),
	
	Mat = material_of_name(MatName, AllMats),
	
	FixedMatName = proplists:get_value(fixed_material_name, Attr, false),
	
	case FixedMatName of
		true -> ObjName = clean_name("wo_"++atom_to_list(MatName));
		_ -> ObjName = clean_name("wo_"++RootName++"_"++atom_to_list(MatName))
	end,
	
	%start model
	io:format(F, "\t<Object Identifier=\"./Models/~s\" Label=\"Default Model\" Name=\"~s\" Type=\"Model\">\n", [ObjName, ObjName]),
	export_model(F, VTab, NTab, Fs0, proplists:get_value(export_normals, Attr, true)),
	
	%material
	FullMatName = ObjName,
	export_material(F, Mat, FullMatName, ExportDir),
	
	%emissive
	{_MName, MProps} = Mat,
	OpenGL = proplists:get_value(opengl, MProps, []),
	{Er, Eg, Eb, _Ea} = proplists:get_value(emission, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case all_zero({Er, Eg, Eb}) of
		true -> ok;
		_ -> export_emit(F, {Er, Eg, Eb}, 2)
	end,
	
	%UV
	case length(UVTab) of
		0 ->
			io:put_chars(F, "\t\t<Parameter Name=\"Map Channel\" Type=\"Point2D List\" Value=\"0\">\n"),
			io:put_chars(F, "\t\t</Parameter>\n");
		_ ->
			case proplists:get_value(include_uvs, Attr, true) of
				false ->
					io:put_chars(F, "\t\t<Parameter Name=\"Map Channel\" Type=\"Point2D List\" Value=\"0\">\n"),
					io:put_chars(F, "\t\t</Parameter>\n");
				_ ->
					io:format(F, "\t\t<Parameter Name=\"Map Channel\" Type=\"Point2D List\" Value=\"~p\">\n",[length(Fs0) * 3]),
					%export_vectors2f(F, indexed_to_raw(UVTab, get_uv_indices(Fs0))),
					
					export_uv_by_tree(F, UVTab, Fs0),
					io:put_chars(F, "\t\t</Parameter>\n")
			end
	end,
	
	%Followup params
	io:put_chars(F, "\t\t<Parameter Name=\"Visible\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Shadow Caster\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Shadow Receiver\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Caustics Transmitter\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t<Parameter Name=\"Caustics Receiver\" Type=\"Boolean\" Value=\"1\"/>\n"),
	
	%close model
	io:put_chars(F, "\t</Object>\n"),
	
	export_objects_split(F, Objs, AllMats, Attr, Index + 1, RootName, ExportDir).
	
	
export_model(F, VTab, NTab, Fs0, DoNormals)->
	
	io:put_chars(F, "\t\t<Object Identifier=\"Triangular Mesh\" Label=\"Triangular Mesh\" Name=\"\" Type=\"Surface\">\n"),
	
	io:format(F, "\t\t\t<Parameter Name=\"Vertex List\" Type=\"Point3D List\" Value=\"~p\">\n",[length(VTab)]),
	export_vectors(F, VTab),
	io:put_chars(F, "\t\t\t</Parameter>\n"),
	
	case DoNormals of
		false ->ok;
		_ ->
			io:format(F, "\t\t\t<Parameter Name=\"Normal List\" Type=\"Point3D List\" Value=\"~p\">\n",[length(Fs0) * 3]),
			%export_vectors(F, indexed_to_raw(NTab, get_normal_indices(Fs0))),
			
			export_normals_by_tree(F, NTab, Fs0),
			io:put_chars(F, "\t\t\t</Parameter>\n")
	end,
	
	io:format(F, "\t\t\t<Parameter Name=\"Index List\" Type=\"Triangle Index List\" Value=\"~p\">\n",[length(Fs0)]),
	export_faces(F, Fs0),
	io:put_chars(F, "\t\t\t</Parameter>\n"),
	io:put_chars(F, "\t\t\t<Parameter Name=\"Smooth\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t\t\t<Parameter Name=\"AA Tolerance\" Type=\"Real\" Value=\"15\"/>\n"),
	
	io:put_chars(F, "\t\t</Object>\n").
	
	
material_of_name(_I, [])->
	{[],[]};
material_of_name(InteriorMatName, [{MatName, Mat} | AllMats])->
	case MatName==InteriorMatName of
		true->{MatName, Mat};
		false -> material_of_name(InteriorMatName, AllMats)
	end.
	
%indexed_to_raw(_S, []) -> % replace indices with values
%	[];
%indexed_to_raw([], _I) -> % replace indices with values
%	[];
%indexed_to_raw(Source, Indices) -> % replace indices with values
%	[lists:nth(Index + 1, Source) || Index <- Indices].
	
export_vectors(_F, [])->
	ok;
export_vectors(F, [{X, Y, Z} | List])->
	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [X, Y, Z]),
	export_vectors(F, List).
	
%export_vectors2f(_F, [])->
%	ok;
%export_vectors2f(F, [{X, Y} | List])->
%	io:format(F, "\t\t\t\t<P xy=\"~f ~f\"/>\n", [X, 1.0 - Y]),
%	export_vectors2f(F, List).
	
export_faces(_F, [])->
	ok;
export_faces(F, [#e3d_face{vs=Vs} | Faces])->
	{X, Y, Z} = list_to_tuple(Vs),
	io:format(F, "\t\t\t\t<F ijk=\"~p ~p ~p\"/>\n", [X, Y, Z]),
	export_faces(F, Faces).	
	
%export_normals(_F, _N, [])->
%	ok;
%export_normals(F, NTab, [#e3d_face{ns=NS} | Faces])->
%	{X, Y, Z} = list_to_tuple(NS),  %indices
%	{NX1, NY1, NZ1} = lists:nth(X+1, NTab),
%	{NX2, NY2, NZ2} = lists:nth(Y+1, NTab),
%	{NX3, NY3, NZ3} = lists:nth(Z+1, NTab),
%	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX1, NY1, NZ1]),
%	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX2, NY2, NZ2]),
%	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX3, NY3, NZ3]),
%	export_normals(F, NTab, Faces).
	
	
iterate_normals(_F, _N, [])->
	ok;
iterate_normals(F, NTree, [#e3d_face{ns=NS} | Faces])->
	{X, Y, Z} = list_to_tuple(NS),  %indices
	{NX1, NY1, NZ1} = gb_trees:get(X+1, NTree),
	{NX2, NY2, NZ2} = gb_trees:get(Y+1, NTree),
	{NX3, NY3, NZ3} = gb_trees:get(Z+1, NTree),
	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX1, NY1, NZ1]),
	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX2, NY2, NZ2]),
	io:format(F, "\t\t\t\t<P xyz=\"~f ~f ~f\"/>\n", [NX3, NY3, NZ3]),
	iterate_normals(F, NTree, Faces).
	
export_normals_by_tree(F, NTab, Faces)->
	Numbers = lists:seq(1, length(NTab)),  %create a list of normal indices
	Zippy = lists:zip(Numbers, NTab), %zip with normal list to create list of tuples {indexkey, normal}
	Tree = gb_trees:from_orddict(Zippy), %Make a tree
	iterate_normals(F, Tree, Faces).
	
%get_normal_indices([])->
%	[];
%get_normal_indices([#e3d_face{ns=NS} | Faces])->
%	{X, Y, Z} = list_to_tuple(NS),
%	[X, Y , Z | get_normal_indices(Faces)].
	
%get_uv_indices([])->
%	[];
%get_uv_indices([#e3d_face{tx=Txs} | Faces])->
%	case Txs of
%		[] -> {X, Y, Z} = {0, 0, 0};
%		_ -> {X, Y, Z} = list_to_tuple(Txs)
%	end,
%	[X, Y, Z | get_uv_indices(Faces)].
	
iterate_uv(_F, _Tx, [])->
	ok;
iterate_uv(F, TxTree, [#e3d_face{tx=Txs} | Faces])->
	{X, Y, Z} = case Txs of %indices
		[] -> {0, 0, 0};
		_ -> list_to_tuple(Txs)
	end,
	{NX1, NY1} = gb_trees:get(X+1, TxTree),
	{NX2, NY2} = gb_trees:get(Y+1, TxTree),
	{NX3, NY3} = gb_trees:get(Z+1, TxTree),
	io:format(F, "\t\t\t\t<P xy=\"~f ~f\"/>\n", [NX1, 1.0 - NY1]),
	io:format(F, "\t\t\t\t<P xy=\"~f ~f\"/>\n", [NX2, 1.0 - NY2]),
	io:format(F, "\t\t\t\t<P xy=\"~f ~f\"/>\n", [NX3, 1.0 - NY3]),
	iterate_uv(F, TxTree, Faces).
	
export_uv_by_tree(F, TxTab, Faces)->
	Numbers = lists:seq(1, length(TxTab)),  %create a list of uv indices
	Zippy = lists:zip(Numbers, TxTab), %zip with UV list to create list of tuples {indexkey, UV}
	Tree = gb_trees:from_orddict(Zippy), %Make a tree
	iterate_uv(F, Tree, Faces).
	
export_material(F, Mat, FullMatName, ExportDir) ->
	{_Mname, MatProps} = Mat,
	KT = proplists:get_value(?TAG, MatProps, []),
	Maps = proplists:get_value(maps, MatProps, []),
	
	export_maps(Maps, ExportDir),
	
	Type = proplists:get_value(material_type, KT, matte),
	case Type of
		plastic ->
			io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
			
			%hand specular map as an additional layer with bitmap weighting
			SpecularMap = proplists:get_value(gloss, Maps, none),
			case SpecularMap of
				none -> export_plastic(F, Mat, false, 3);
				#e3d_image{name=SpcImageName, filename=SpcFileName} ->
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
					export_plastic(F, Mat, false, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
					export_diffuse(F, MatProps, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					export_bitmap_weight(F, "Weight #0", SpcImageName, SpcFileName, 3, false),
					export_bitmap_weight(F, "Weight #1", SpcImageName, SpcFileName, 3, true)
			end,
			io:put_chars(F, "\t\t</Object>\n");
		di_glass -> export_dielectric(F, Mat, FullMatName, 2);
		fresnel_glass -> 
			io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
			
			%hand specular map as an additional layer with bitmap weighting
			SpecularMap = proplists:get_value(gloss, Maps, none),
			case SpecularMap of
				none -> export_fresnel_glass(F, Mat, 3);
				#e3d_image{name=SpcImageName, filename=SpcFileName} ->
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
					export_fresnel_glass(F, Mat, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
					export_diffuse(F, MatProps, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					export_bitmap_weight(F, "Weight #0", SpcImageName, SpcFileName, 3, false),
					export_bitmap_weight(F, "Weight #1", SpcImageName, SpcFileName, 3, true)
			end,
			io:put_chars(F, "\t\t</Object>\n");
		frosted_glass ->
			io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
			
			%hand specular map as an additional layer with bitmap weighting
			SpecularMap = proplists:get_value(gloss, Maps, none),
			case SpecularMap of
				none -> export_frosted_glass(F, Mat, 3);
				#e3d_image{name=SpcImageName, filename=SpcFileName} ->
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
					export_frosted_glass(F, Mat, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
					export_diffuse(F, MatProps, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					export_bitmap_weight(F, "Weight #0", SpcImageName, SpcFileName, 3, false),
					export_bitmap_weight(F, "Weight #1", SpcImageName, SpcFileName, 3, true)
			end,
			io:put_chars(F, "\t\t</Object>\n");
		thin_glass ->
			export_thin_glass(F, Mat, FullMatName, 2);
		matte_metal ->
			export_matte_metal(F, Mat, FullMatName, 2);
		ashikhmin_metal ->
			export_ashikhmin_metal(F, Mat, FullMatName, 2);
		sss ->
			io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
			
			%hand specular map as an additional layer with bitmap weighting
			SpecularMap = proplists:get_value(gloss, Maps, none),
			case SpecularMap of
				none -> export_plastic(F, Mat, true, 3);
				#e3d_image{name=SpcImageName, filename=SpcFileName} ->
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Layered Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
					export_plastic(F, Mat, true, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					io:format(F, "\t\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
					export_diffuse(F, MatProps, 4),
					io:put_chars(F, "\t\t\t</Object>\n"),
					export_bitmap_weight(F, "Weight #0", SpcImageName, SpcFileName, 3, false),
					export_bitmap_weight(F, "Weight #1", SpcImageName, SpcFileName, 3, true)
			end,
			io:put_chars(F, "\t\t</Object>\n"),
			export_absorption(F, MatProps, 2);
		_ ->
			io:format(F, "\t\t<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
			export_flat_mat(F, Mat),
			io:put_chars(F, "\t\t</Object>\n")
	end,
	
	export_bump_map(F, Mat),
	export_normal_map(F, Mat).
	
export_flat_mat(F, {_Name, Mat})->
	Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	
	%%ambient color
	{Ar, Ag, Ab, _Aa} = proplists:get_value(ambient, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case all_zero({Ar, Ag, Ab}) of
		true -> ok;
		_ -> export_constant_texture(F, "./Ambient/", {Ar, Ag, Ab}, 3)
	end,
	
	%diffuse color, diffuse map
	export_diffuse(F, Mat, 3),
	
	%specular color
	SpeculareMap = proplists:get_value(gloss, Maps, none),
	{Spr, Spg, Spb, _Spa} = proplists:get_value(specular, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	
	case SpeculareMap of
		none ->
			case all_zero({Spr, Spg, Spb}) of
				true -> ok;
				_ -> export_constant_texture(F, "./Specular/", {Spr, Spg, Spb}, 3)
			end;
		#e3d_image{name=SpcImageName, filename=SpcFileName} ->
			export_bitmap_tex(F, "./Specular/", SpcImageName, SpcFileName, 3)
	end,
	
	Shiny = proplists:get_value(shininess, OpenGL, 1.0),
	ShinyEx = Shiny * 128.0,
	io:format(F, "\t\t\t<Parameter Name=\"Shininess\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	IOR = proplists:get_value(ior, KT, 1.0),
	io:format(F, "\t\t\t<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]).
	
export_plastic(F, {_Name, Mat}, Translucence, TabCount) ->
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
	
	%diffuse color, diffuse map
	export_diffuse(F, Mat, TabCount + 1),
	
	case Translucence of
		true -> 
			{Tr, Tg, Tb} = proplists:get_value(translucence, KT, {0.0, 0.0, 0.0}),
			export_constant_texture(F, "./Translucent/", {Tr, Tg, Tb}, TabCount + 1);
		_ -> ok
	end,
	
	IOR = proplists:get_value(ior, KT, 1.0),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
	%specular color
	{Sr, Sg, Sb, _Sa} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
	%export_constant_texture(F, "./Specular/", {1.0, 1.0, 1.0}, TabCount + 1),
	export_constant_texture(F, "./Specular/", {Sr, Sg, Sb}, TabCount + 1),
	
	Shiny = proplists:get_value(shininess, OpenGL, 1.0),
	ShinyEx = math:exp(Shiny * 10.0) * 2.0,
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Shininess\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Specular Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Specular Attenuation\" Type=\"String\" Value=\"None\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	%weights
	export_fresnel_weight(F, "Weight #0", "Weight #0", {1.0, 1.0, 1.0}, {0.0, 0.0, 0.0}, IOR, false, TabCount),
	export_fresnel_weight(F, "Weight #1", "Weight #1", {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, IOR, false, TabCount).
	
export_dielectric(F, {_Name, Mat}, FullMatName, TabCount) ->
    KT = proplists:get_value(?TAG, Mat, []),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Snell Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
	export_refraction(F, Mat, TabCount + 1),
	export_reflection(F, Mat, TabCount + 1),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Fresnel\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Dispersion\" Type=\"Boolean\" Value=\"0\"/>\n"),
	
	IOR = proplists:get_value(ior, KT, 1.0),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"./Index of Refraction/Index\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_thin_glass(F, {_Name, Mat}, FullMatName, TabCount)->
	Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	IOR = proplists:get_value(ior, KT, 1.0),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Thin Glass Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
	
	%for some reason, thin glass is listed under reflectance, but the values are more appropriate to refractance
	
	%diffuse color, diffuse map
	DiffuseMap = proplists:get_value(diffuse, Maps, none),
	{Dr, Dg, Db, Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case DiffuseMap of
		none ->
			Ra = 1.0 - Da,
			export_constant_texture(F, "./Reflectance/", {Dr * Ra, Dg * Ra, Db * Ra}, TabCount + 1);
		#e3d_image{name=ImageName, filename=FileName} ->
			export_bitmap_tex(F, "./Reflectance/", ImageName, FileName, TabCount + 1)
	end,
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_fresnel_glass(F, {_Name, Mat}, TabCount) ->
    KT = proplists:get_value(?TAG, Mat, []),
	OpenGL = proplists:get_value(opengl, Mat),
	 
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
	
	%diffuse color, diffuse map
	export_refraction(F, Mat, TabCount + 1),
	IOR = proplists:get_value(ior, KT, 1.0),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
	%specular color
	{Sr, Sg, Sb, _Sa} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
	%export_constant_texture(F, "./Reflection/", {1.0, 1.0, 1.0}, TabCount + 1),
	export_constant_texture(F, "./Reflection/", {Sr, Sg, Sb}, TabCount + 1),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	%weights
	export_fresnel_weight(F, "Weight #0", "Weight #0", {1.0, 1.0, 1.0}, {0.0, 0.0, 0.0}, IOR, true, TabCount),
	export_fresnel_weight(F, "Weight #1", "Weight #1", {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, IOR, true, TabCount).
	
export_frosted_glass(F, {_Name, Mat}, TabCount) ->
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#0", "#0"]),
	
	%diffuse color, diffuse map
	export_transmitted(F, Mat, TabCount + 1),
	
	IOR = proplists:get_value(ior, KT, 1.0),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	
	Shiny = proplists:get_value(shininess, OpenGL, 1.0),
	ShinyEx = math:exp(Shiny * 10.0) * 2.0,
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Transmitted Shininess\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Transmitted Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Transmitted Attenuation\" Type=\"String\" Value=\"None\"/>\n"),
	
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", ["#1", "#1"]),
	%specular color
	{Sr, Sg, Sb, _Sa} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
	%export_constant_texture(F, "./Reflection/", {1.0, 1.0, 1.0}, TabCount + 1),
	export_constant_texture(F, "./Reflection/", {Sr, Sg, Sb}, TabCount + 1),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n"),
	
	%weights
	export_fresnel_weight(F, "Weight #0", "Weight #0", {1.0, 1.0, 1.0}, {0.0, 0.0, 0.0}, IOR, true, TabCount),
	export_fresnel_weight(F, "Weight #1", "Weight #1", {0.0, 0.0, 0.0}, {1.0, 1.0, 1.0}, IOR, true, TabCount).
	
export_matte_metal(F, {_Name, Mat}, FullMatName, TabCount)->
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	IOR = proplists:get_value(ior, KT, 1.0),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Whitted Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	{Sr, Sg, Sb, _Sa} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
	export_fresnel_weight(F, "./Specular/Fresnel Ramp Texture", "", {Dr, Dg, Db}, {Sr, Sg, Sb}, IOR, false, TabCount + 1),
	
	Shiny = proplists:get_value(shininess, OpenGL, 1.0),
	ShinyEx = math:exp(Shiny * 10.0) * 2.0,
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Shininess\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Specular Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Specular Attenuation\" Type=\"String\" Value=\"None\"/>\n"),

	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_ashikhmin_metal(F, {_Name, Mat}, FullMatName, TabCount)->
    OpenGL = proplists:get_value(opengl, Mat),
    KT = proplists:get_value(?TAG, Mat, []),
	IOR = proplists:get_value(ior, KT, 1.0),
	
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Ashikhmin Material\" Name=\"~s\" Type=\"Material\">\n", [FullMatName, FullMatName]),
	{Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	{Sr, Sg, Sb, _Sa} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
	export_fresnel_weight(F, "./Specular/Fresnel Ramp Texture", "", {Dr, Dg, Db}, {Sr, Sg, Sb}, IOR, false, TabCount + 1),
	
	Shiny = proplists:get_value(shininess, OpenGL, 1.0),
	ShinyEx = math:exp(Shiny * 10.0) * 2.0,
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Shininess X\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Shininess Y\" Type=\"Real\" Value=\"~f\"/>\n", [ShinyEx]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Specular Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"None\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Rotation\" Type=\"Real\" Value=\"0\"/>\n"),

	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_bump_map(F, {_Name, Mat}) ->
	Maps = proplists:get_value(maps, Mat, []),
	BumpMap = proplists:get_value(bump, Maps, none),
	case BumpMap of
		none -> ok;
		#e3d_image{name=ImageName, filename=FileName} ->
			io:put_chars(F, "\t\t<Object Identifier=\"Bump Mapping\" Label=\"Bump Mapping\" Name=\"Bump Mapping\" Type=\"Intersection Modifier\">\n"),
			
			io:put_chars(F, "\t\t\t<Object Identifier=\"./Texture/Weighted Texture\" Label=\"Weighted Texture\" Name=\"\" Type=\"Texture\">\n"),
			export_bitmap_tex(F, "", ImageName, FileName, 4),
			io:put_chars(F, "\t\t\t\t<Parameter Name=\"Bitmap Texture:Weight\" Type=\"Real\" Value=\"1\"/>\n"),
			io:put_chars(F, "\t\t\t</Object>\n"),
			
			io:put_chars(F, "\t\t\t<Parameter Name=\"Strength\" Type=\"Real\" Value=\"1\"/>\n"),
			io:put_chars(F, "\t\t</Object>\n")
	end.
export_normal_map(F, {_Name, Mat})->
	Maps = proplists:get_value(maps, Mat, []),
	BumpMap = proplists:get_value(normal, Maps, none),
	case BumpMap of
		none -> ok;
		#e3d_image{name=ImageName, filename=FileName} ->
			io:put_chars(F, "\t\t<Object Identifier=\"Normal Mapping\" Label=\"Normal Mapping\" Name=\"Normal Mapping\" Type=\"Intersection Modifier\">\n"),
			
			io:put_chars(F, "\t\t\t<Object Identifier=\"./Texture/Weighted Texture\" Label=\"Weighted Texture\" Name=\"\" Type=\"Texture\">\n"),
			export_bitmap_tex(F, "", ImageName, FileName, 4),
			io:put_chars(F, "\t\t\t\t<Parameter Name=\"Bitmap Texture:Weight\" Type=\"Real\" Value=\"1\"/>\n"),
			io:put_chars(F, "\t\t\t</Object>\n"),
			io:put_chars(F, "\t\t</Object>\n")
	end.
	
export_diffuse(F, MatProps, TabCount)->
	Maps = proplists:get_value(maps, MatProps, []),
    OpenGL = proplists:get_value(opengl, MatProps),
	
	%diffuse color, diffuse map
	DiffuseMap = proplists:get_value(diffuse, Maps, none),
	{Dr, Dg, Db, Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case DiffuseMap of
		none ->
			export_constant_texture(F, "./Diffuse/", {Dr * Da, Dg * Da, Db * Da}, TabCount),
			case Da of
				1.0 -> ok;
				_ ->
					Ra = 1.0 - Da,
					export_constant_texture(F, "./Refraction/", {Dr * Ra, Dg * Ra, Db * Ra}, TabCount)
			end;
		#e3d_image{name=ImageName, filename=FileName} ->
			export_bitmap_tex(F, "./Diffuse/", ImageName, FileName, TabCount)
	end.
	
export_refraction(F, MatProps, TabCount)->
	Maps = proplists:get_value(maps, MatProps, []),
    OpenGL = proplists:get_value(opengl, MatProps),
	
	%diffuse color, diffuse map
	DiffuseMap = proplists:get_value(diffuse, Maps, none),
	{Dr, Dg, Db, Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case DiffuseMap of
		none ->
			Ra = 1.0 - Da,
			export_constant_texture(F, "./Refraction/", {Dr * Ra, Dg * Ra, Db * Ra}, TabCount);
		#e3d_image{name=ImageName, filename=FileName} ->
			export_bitmap_tex(F, "./Refraction/", ImageName, FileName, TabCount)
	end.
	
export_reflection(F, MatProps, TabCount)->
	Maps = proplists:get_value(maps, MatProps, []),
    OpenGL = proplists:get_value(opengl, MatProps),
	
	%specular color, specular map
	SpecularMap = proplists:get_value(gloss, Maps, none),
	{Dr, Dg, Db, _Da} = proplists:get_value(specular, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case SpecularMap of
		none ->
			export_constant_texture(F, "./Reflection/", {Dr, Dg, Db}, TabCount);
		#e3d_image{name=ImageName, filename=FileName} ->
			export_bitmap_tex(F, "./Reflection/", ImageName, FileName, TabCount)
	end.
	
export_transmitted(F, MatProps, TabCount)->
	Maps = proplists:get_value(maps, MatProps, []),
    OpenGL = proplists:get_value(opengl, MatProps),
	
	%diffuse color, diffuse map
	DiffuseMap = proplists:get_value(diffuse, Maps, none),
	{Dr, Dg, Db, Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 1.0}),
	case DiffuseMap of
		none ->
			Ra = 1.0 - Da,
			export_constant_texture(F, "./Transmitted/", {Dr * Ra, Dg * Ra, Db * Ra}, TabCount);
		#e3d_image{name=ImageName, filename=FileName} ->
			export_bitmap_tex(F, "./Transmitted/", ImageName, FileName, TabCount)
	end.
	
export_constant_texture(F, Type, Color, TabCount)->
	{Cr, Cg, Cb} = Color,
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~sConstant Texture\" Label=\"Constant Texture\" Name=\"\" Type=\"Texture\">\n",[Type]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Cr, Cg, Cb]),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_emit(F, Color, TabCount) ->
	export_tabs(F, TabCount),
	io:put_chars(F, "<Object Identifier=\"Diffuse Light\" Label=\"Diffuse Light\" Name=\"\" Type=\"Emittance\">\n"),
	export_constant_texture(F, "./Radiance/", Color, TabCount + 1),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Attenuation\" Type=\"String\" Value=\"Inverse Square\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Emitter\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Front Side\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Back Side\" Type=\"Boolean\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Power\" Type=\"Real\" Value=\"50\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Unit\" Type=\"String\" Value=\"Watts\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_bitmap_tex(F, Type, ImageName, FileName, TabCount)->
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~sBitmap Texture\" Label=\"Bitmap Texture\" Name=\"\" Type=\"Texture\">\n", [Type]),
	export_bitmap_data(F, ImageName, FileName, TabCount + 1, false),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_bitmap_weight(F, Type, ImageName, FileName, TabCount, Inverted)->
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Bitmap Texture\" Name=\"~s\" Type=\"Texture\">\n", [Type, Type]),
	export_bitmap_data(F, ImageName, FileName, TabCount + 1, Inverted),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_bitmap_data(F, ImageName, FileName, TabCount, Inverted)->
	case FileName of 
		none ->
			export_tabs(F, TabCount),
			io:format(F, "<Parameter Name=\"Filename\" Type=\"String\" Value=\"~s\"/>\n", [ImageName++".png"]);
		_ ->
			export_tabs(F, TabCount),
			io:format(F, "<Parameter Name=\"Filename\" Type=\"String\" Value=\"~s\"/>\n", [FileName])
	end,
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Projection\" Type=\"String\" Value=\"UV\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Offset X\" Type=\"Real\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Offset Y\" Type=\"Real\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Scale X\" Type=\"Real\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Scale Y\" Type=\"Real\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Rotation\" Type=\"Real\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Parameter Name=\"Smooth\" Type=\"Boolean\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount),
	case Inverted of
		true -> io:put_chars(F, "<Parameter Name=\"Inverted\" Type=\"Boolean\" Value=\"1\"/>\n");
		_ -> io:put_chars(F, "<Parameter Name=\"Inverted\" Type=\"Boolean\" Value=\"0\"/>\n")
	end.
	
export_fresnel_weight(F, Type, Name, {Lr, Lg, Lb}, {Hr, Hg, Hb}, IOR, ExitAttenuation, TabCount)->
	export_tabs(F, TabCount),
	io:format(F, "<Object Identifier=\"~s\" Label=\"Fresnel Ramp Texture\" Name=\"~s\" Type=\"Texture\">\n", [Type, Name]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Low Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Lr, Lg, Lb]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"High Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Hr, Hg, Hb]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Index of Refraction\" Type=\"Real\" Value=\"~f\"/>\n",[IOR]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Inverted Attenuation\" Type=\"Boolean\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount + 1),
	case ExitAttenuation of
		true -> io:put_chars(F, "<Parameter Name=\"Exit Attenuation\" Type=\"Boolean\" Value=\"1\"/>\n");
		_ -> io:put_chars(F, "<Parameter Name=\"Exit Attenuation\" Type=\"Boolean\" Value=\"0\"/>\n")
	end, 
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_absorption(F, MatProps, TabCount) ->
	KT = proplists:get_value(?TAG, MatProps, []),
	export_tabs(F, TabCount),
	io:put_chars(F, "<Object Identifier=\"Simple Fog Interior\" Label=\"Simple Fog Interior\" Name=\"Simple Fog Interior\" Type=\"Interior\">\n"),
	{Ar, Ag, Ab} = proplists:get_value(absorption_color, KT, {0.0, 0.0, 0.0}),
	{Sr, Sg, Sb} = proplists:get_value(scatter_color, KT, {0.0, 0.0, 0.0}),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Absorption Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Ar, Ag, Ab]),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Absorption Intensity\" Type=\"Real\" Value=\"~f\"/>\n", [1.0]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Emittance Color\" Type=\"RGB\" Value=\"1 1 1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Emittance Intensity\" Type=\"Real\" Value=\"0\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:format(F, "<Parameter Name=\"Scatter Color\" Type=\"RGB\" Value=\"~f ~f ~f\"/>\n", [Sr, Sg, Sb]),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Scatter Intensity\" Type=\"Real\" Value=\"1\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Scatter Density\" Type=\"Real\" Value=\"0.001\"/>\n"),
	export_tabs(F, TabCount + 1),
	io:put_chars(F, "<Parameter Name=\"Absorption Bias\" Type=\"RGB\" Value=\"0 0 0\"/>\n"),
	export_tabs(F, TabCount),
	io:put_chars(F, "</Object>\n").
	
export_tabs(F, TabCount) ->
	case TabCount of
		0 -> ok;
		_ -> io:put_chars(F, "\t"),
			export_tabs(F, TabCount - 1)
	end.
	
export_maps([], _ED)->
	[];
export_maps([{MapID, Map} | Maps], ExportDir) ->
	#e3d_image{name=ImageName, filename=FileName} = Map,
	case FileName of
		none ->
			MapFile = ImageName++".png",
			e3d_image:save(Map, filename:join(ExportDir, MapFile));
		_ -> ok
	end,
	[{MapID, ImageName} | export_maps(Maps, ExportDir)].
	
%Construct dialog with export options, use get_pref to load options
export_dialog(_Op)->
	
	[
		{vframe, [
			{"Export UVs", get_pref(include_uvs, true), [exkey(include_uvs)]},
			{"Export Normals", get_pref(export_normals, true), [exkey(export_normals)]},
			{hframe, [{label, "Subdivisions"}, {slider, {text, get_pref(subdivisions, 0), [range({0, 10}), exkey(subdivisions)]}}]},
			{"Use material names only", get_pref(fixed_material_name, false), [exkey(fixed_material_name)]}
		]},
		separator,
		{vframe, [
			{hframe, [{label, "Render: "}, {menu, 
				[{"Ray Tracing", raytrace}, {"Ray Tracing High", raytrace_high},
				{"Photon Map Quick", pm_quick}, {"Photon Map Medium", pm_medium},
				{"Photon Map High", pm_high}, {"Path Trace Quick", pt_quick},
				{"Path Trace Medium", pt_medium}, {"Path Trace High", pt_high},
				{"Progressive Path Trace", pt_progressive},
				{"Bidirectional Path Trace", pt_bpt},
				{"MLT", mlt}, {"MLT BPT", mlt_bpt}],
				get_pref(render_preset, pm_medium), [exkey(render_preset)]}
			
			%[{"Medium PhotonMap", medium_pm}, {"High PhotonMap", high_pm},
			%	{"Low PathTrace", low_pt}, {"High PathTrace", high_pt},
			%	{"Progressive PathTrace", prog_pt}, {"BiPT", bipt},
			%	{"MLT", mlt}], 
			%	get_pref(render_type, medium_pm), [exkey(render_type)]}
			]},
			{vframe, [
				{hframe, [
					{vframe, [{label, "Translucencies"}, {label, "Caustics"}, {label, "AntiAlias"}, {label, "AA Threshold"}]},
					{vframe, [
						{menu, [{"None", none}, {"Pseudo", pseudo}, {"Normal", normal}], get_pref(translucence, normal), [exkey(translucence), hook(enable, [member, render_preset, raytrace, raytrace_high, pm_quick, pm_medium, pm_high, pt_quick, pt_medium, pt_high, pt_progressive])] },
						{menu, [{"None", none}, {"Pseudo", pseudo}, {"Normal", normal}], get_pref(caustics_type, none), [exkey(caustics_type), hook(enable, [member, render_preset, raytrace, raytrace_high, pm_quick, pm_medium, pm_high, pt_quick, pt_medium, pt_high, pt_progressive])] },
						{menu, [{"None", none}, {"Extra Pass", pass}, {"Iterative", iterative}, {"Production", production}], get_pref(antialias_type, none), 
							[exkey(antialias_type), hook(enable, [member, render_preset, raytrace, raytrace_high, pm_quick, pm_medium, pm_high, pt_quick, pt_medium, pt_high])] 
						},
						{slider, {text, get_pref(antialias_threshold, 0.3), [range({0.05, 0.5}), exkey(antialias_threshold), 
							hook(enable, [member, antialias_type, pass, iterative, production])]}}
					]}
				]},
				{"Volume Lighting", get_pref(volume_light, false), [exkey(volume_light)]}
			]}
		]},
		{vframe, [
			{"Fix camera dimensions to model view dimensions", get_pref(fix_win_dim, false), [exkey(fix_win_dim)]},
			{hframe, [{label, "Width"}, {text, get_pref(width, 320), [exkey(width)]}, 
				{label, "Height"}, {text, get_pref(height, 240), [exkey(height)]}], 
				[ hook(enable, [member, fix_win_dim, false])]},
				
			{hframe, [{label, "Camera"}, {menu, [{"Perspective", perspective}, {"Cylindrical", cylindrical}, 
				{"Spherical", spherical}, {"Parallel", parallel}], get_pref(camera_type, perspective), [exkey(camera_type)]}
			]},
			%{hframe, [{label, "Lens Radius (mm)"}, {text, get_pref(aperture, 0.0), [exkey(aperture)]}, 
			%	{label, "Lens Samples"}, {text, get_pref(blur_samples, 1), [range({1, 50}), exkey(blur_samples), hook(enable, ['not', [member, aperture, 0.0]])]}
			%KT 2008 f-stop
			{hframe, [{label, "F-Number"}, {menu, [{"Pinhole", "Pinhole"}, {"1", "1"}, {"1.4", "1.4"},
				{"2", "2"}, {"2.8", "2.8"}], get_pref(fnumber, "Pinhole"), [exkey(fnumber)]},
				{label, "Lens Samples"}, {text, get_pref(blur_samples, 3), [range({1, 50}), exkey(blur_samples), hook(enable, ['not', [member, fnumber, "Pinhole"]])]}
			]}
		], [{title, "Camera"}]},
		separator,
		{hframe, [ {label, "Render Threads"}, {slider, {text, get_pref(render_thread_count, 1), [range({1, 4}), exkey(render_thread_count)]}}]}
	].
	
	%construct light dialog
light_dialog(_Name, Light)->
	OpenGL = proplists:get_value(opengl, Light),
    KT = proplists:get_value(?TAG, Light, []),
	Type = proplists:get_value(type, OpenGL, []),
	
	BrowseProps = [ {dialog_type, open_dialog},
		   {extensions, [{".jpg","JPEG"}, {".png","PNG"}, {".bmp", "Bitmap"}, {".gif", "GIF"}, {".hdr", "HDR"}]}],
	
	case Type of
		ambient ->
			[
				{hframe, [ 
					{vframe, [
						{label, "Sky"},
						{label, "Intensity"},
						{label, "Turbidity"},
						{label, "Image"}
					]},
					{vframe, [
						{menu, [{"Background Color", background_color}, {"Sky Color", sky_color},
							{"Centered Image", centered_image}, {"Tiled Image", tiled_image},
							{"Fit Image", fit_image}, {"Hemisphere Image", hemisphere},
							{"Spherical", spherical}, {"Physical", physical}], proplists:get_value(sky, KT, background_color), [key(sky)]},
						{slider, {text, proplists:get_value(sky_intensity, KT, 1.0), [range({0.0, 10.0}), key(sky_intensity)]}},
						{slider, {text, proplists:get_value(sky_turbidity, KT, 2.0), [range({0.0, 50.0}), key(sky_turbidity),
							hook(enable, [member, ?KEY(sky), physical])]
						}},
						{button, {text, proplists:get_value(sky_file, KT, []), [key(sky_file), {props, BrowseProps},
							hook(enable, [member, ?KEY(sky), centered_image, tiled_image, fit_image, hemisphere, spherical])]
						}}
					]}
				], [{title, "Kerkythea Options"}, {minimized, proplists:get_value(minimized, KT, true)}, key(minimized)]}
			];
		area ->
			[
				{vframe, [ 
					{hframe, [
						{label, "Power"},
						{slider, {text, proplists:get_value(light_power, KT, 1.0), [range({0.0, 100.0}), key(light_power)]}}
					]},
					{"Visible", proplists:get_value(area_visible, KT, true), [key(area_visible)]}
				], [{title, "Kerkythea Options"}, {minimized, proplists:get_value(minimized, KT, true)}, key(minimized)]}
			];
		_ ->
			[
				{vframe, [ 
					{hframe, [
						{vframe, [ {label, "Power"}, {label, "Light Radius"}, {label, "Shadow Color"}, {label, "Attenuation"}] },
						{vframe, [ {slider, {text, proplists:get_value(light_power, KT, 1.0), [range({0.0, 100.0}), key(light_power)]}},
							{slider, {text, proplists:get_value(light_radius, KT, 0.2), [range({0.0, 100.0}), key(light_radius)]}},
							{slider, {color, proplists:get_value(shadowcolor, KT, {0.0, 0.0, 0.0}), [key(shadowcolor)]}},
							{menu, [{"None", none}, {"Inverse", inverse}, {"Inverse Square", inverse_square}], 
							proplists:get_value(attenuation, KT, none), [key(attenuation)]}
						]}
					]},
					{hframe, [
						{"Shadows", proplists:get_value(shadows, KT, true), [key(shadows)]},
						{"Soft Shadows", proplists:get_value(soft_shadows, KT, false), [key(soft_shadows)]},
						{"Negative Light", proplists:get_value(neg_light, KT, false), [key(neg_light)]}
					]}
				], [{title, "Kerkythea Options"}, {minimized, proplists:get_value(minimized, KT, true)}, key(minimized)]}
			]
	end.
light_result(_Name, Light, Res)->

	{Found, Remaining} = rip_all(?TAG, Res),
	 NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Light)],
	{NewLight, Remaining}.
	
	%construct material dialog
material_dialog(_Name, Mat)->
    KT = proplists:get_value(?TAG, Mat, []),
	
	[ {vframe, [ 
		{hframe, [
			{label, "IOR"},
						{slider, {text, proplists:get_value(ior, KT, 1.0), [range({0.0, 3.0}), key(ior)]}}
		]},
		{hframe, [
			{label, "Type"}, {menu, [{"Matte", matte}, {"Plastic", plastic}, {"Dielectric Glass", di_glass},
				{"Fresnel Glass", fresnel_glass}, {"Frosted Glass", frosted_glass}, {"Thin Glass", thin_glass},
				{"Matte Metal", matte_metal}, {"Ashikhmin Metal", ashikhmin_metal}, {"SSS", sss}], 
				proplists:get_value(material_type, KT, matte), [key(material_type), layout]},
				{hframe, [ panel, help_button({material_dialog, matte}) ], [hook(open, [member, ?KEY(material_type), matte])]},
				{hframe, [ panel, help_button({material_dialog, plastic}) ], [hook(open, [member, ?KEY(material_type), plastic])]},
				{hframe, [ panel, help_button({material_dialog, di_glass}) ], [hook(open, [member, ?KEY(material_type), di_glass])]},
				{hframe, [ panel, help_button({material_dialog, fresnel_glass}) ], [hook(open, [member, ?KEY(material_type), fresnel_glass])]},
				{hframe, [ panel, help_button({material_dialog, frosted_glass}) ], [hook(open, [member, ?KEY(material_type), frosted_glass])]},
				{hframe, [ panel, help_button({material_dialog, thin_glass}) ], [hook(open, [member, ?KEY(material_type), thin_glass])]},
				{hframe, [ panel, help_button({material_dialog, matte_metal}) ], [hook(open, [member, ?KEY(material_type), matte_metal])]},
				{hframe, [ panel, help_button({material_dialog, ashikhmin_metal}) ], [hook(open, [member, ?KEY(material_type), ashikhmin_metal])]},
				{hframe, [ panel, help_button({material_dialog, sss}) ], [hook(open, [member, ?KEY(material_type), sss])]}
		]},
		{hframe, [
			{vframe, [{label, "Translucency"}, {label, "Absorption"}, {label, "Scatter"}]},
			{vframe, [
				{slider, {color, proplists:get_value(translucence, KT, {0.0, 0.0, 0.0}), [key(translucence)]}},
				{slider, {color, proplists:get_value(absorption_color, KT, {0.0, 0.0, 0.0}), [key(absorption_color)]}},
				{slider, {color, proplists:get_value(scatter_color, KT, {0.0, 0.0, 0.0}), [key(scatter_color)]}}
			]}
		], [hook(open, [member, ?KEY(material_type), sss])]}
	], [{title, "Kerkythea Options"}, {minimized, proplists:get_value(minimized, KT, true)}, key(minimized)]}
	].
	
material_result(_Name, Mat, Res)->
	{Found, Remaining} = rip_all(?TAG, Res),
	 NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Mat)],
	{NewLight, Remaining}.
	
all_zero(InTuple) when is_tuple(InTuple)->
	InList = tuple_to_list(InTuple),
	all_zero(InList);
all_zero([])->
	true;
all_zero([InVal | InList]) when is_list(InList)->
	case InVal of
		0.0 -> all_zero(InList);
		0 -> all_zero(InList);
		_ -> false
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
     end};
hook(Prop) -> hook([Prop]).

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
	
help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.
	
help(title, {material_dialog, matte}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, matte}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Matte",
	"   This is the most basic setting.  It is NOT a photorealistic setting, but still has uses.",
	"    KT Diffuse    -> diffuse * opacity",
	"    KT Specular   -> specular",
	"    KT Refraction -> diffuse * (1 - opacity)",
	"    KT Shininess  -> shininess (linear range 0 - 128)",
	"    KT Ambient    -> ambient",
	"    KT IOR        -> IOR",
	"    A diffuse map will be used in place of diffuse color.  No opacity is applied, therefore there is no refraction.",
	"    A gloss map will be used in place of specular color.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, plastic}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, plastic}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Plastic",
	"   This is the most useful setting.  It is a layered material using the fresnel procedural to provide reflection based on IOR.",
	"    KT Diffuse    -> diffuse * opacity",
	"    KT Specular   -> specular; Intensity (V in HSV) should ALWAYS be 1.0",
	"    KT Refraction -> diffuse * (1 - opacity)",
	"    KT Shininess  -> shininess (exp range 1 - ~44,000)",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    KT IOR        -> IOR",
	"    A diffuse map will be used in place of diffuse color.  No opacity is applied, therefore there is no refraction.",
	"    A gloss map creates a layered material around the plastic, mapping between the specular material and a flat diffuse version.",
	"    Specular sampling is enabled.  This will result in fuzzy reflection based on the shininess value.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, di_glass}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, di_glass}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Dielectric Glass",
	"   This is a simple glass setting.  It is a single layer material.",
	"    KT Reflection   -> specular",
	"    KT Refraction -> diffuse * (1 - opacity)",
	"    KT IOR        -> IOR",
	"    KT Shininess  -> shininess is ignored.",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    A diffuse map will be used in place of refraction color.  The values of the map determine the opacity.",
	"    A gloss map overrides reflection, the values of the map determining the reflection.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, fresnel_glass}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, fresnel_glass}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Fresnel Glass",
	"   This is similar to plastic and frosted glass.  Fresnel procedurals govern the balance between refractive and reflective layers.",
	"    KT Reflection   -> specular; Intensity (V in HSV) should ALWAYS be 1.0",
	"    KT Refraction -> diffuse * (1 - opacity)",
	"    KT IOR        -> IOR",
	"    KT Shininess  -> shininess is ignored.",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    A diffuse map will be used in place of refraction color.  The values of the map determine the opacity.",
	"    A gloss map is ignored.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, frosted_glass}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, frosted_glass}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Frosted Glass",
	"   This is similar to plastic and fresnel glass.  Fresnel procedurals govern the balance between transmitted and reflective layers.",
	"    KT Reflection   -> specular; Intensity (V in HSV) should ALWAYS be 1.0",
	"    KT Transmission -> diffuse * (1 - opacity)",
	"    KT IOR        -> IOR",
	"    KT Transmitted Shininess  -> shininess (exp range 1 - ~44,000).",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    A diffuse map will be used in place of transmitted color.  The values of the map determine the opacity.",
	"    A gloss map is ignored.",
	"    Transmission sampling is enabled.  This will result in fuzzy refraction based on the shininess value.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, thin_glass}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, thin_glass}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Thin Glass",
	"   This is a simple glass imitating a thin glass layer.",
	"    KT Refraction   -> diffuse * (1 - opacity)",
	"    KT IOR        -> IOR",
	"    KT Specular   -> specular is ignored",
	"    KT Shininess  -> shininess is ignored",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    A diffuse map will be used in place of refraction color.  The values of the map determine the opacity.",
	"    A gloss map is ignored.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, matte_metal}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, matte_metal}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Matte Metal",
	"   This is a simple single layer material for reflective metal.",
	"    KT Diffuse   -> always black",
	"    KT Specular   -> always white",
	"    KT IOR        -> IOR",
	"    KT Fresnel Low Color -> diffuse",
	"    KT Fresnel High Color -> specular",
	"    KT Shininess  -> shininess (exp range 1 - ~44,000)",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    Opacity is ignored",
	"    A diffuse map is ignored.",
	"    A gloss map is ignored.",
	"    Specular sampling is enabled.  This will result in fuzzy reflection based on the shininess value.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, ashikhmin_metal}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, ashikhmin_metal}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"Ashikhmin Metal",
	"   This is a simple single layer material using Ashikhmin for reflective metal.",
	"    KT Diffuse   -> always black",
	"    KT Specular   -> always white",
	"    KT IOR        -> IOR",
	"    KT Fresnel Low Color -> diffuse",
	"    KT Fresnel High Color -> specular",
	"    KT Shininess  -> shininess (exp range 1 - ~44,000)",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    Opacity is ignored",
	"    A diffuse map is ignored.",
	"    A gloss map is ignored.",
	"    Ashikhmin sampling is enabled.  This will result in fuzzy reflection based on the shininess value.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ];
help(title, {material_dialog, sss}) ->
    "Kerkythea Material Properties";
help(text, {material_dialog, sss}) ->
    ["Kerkythea focuses on photorealism.  The OpenGL settings available in Wings3D do not always correspond to realistic results."
	"  Instead of providing ALL the controls available for KT, presets are available for the most common, photorealistic settings.",
	"SubSurface Scattering",
	"   Essentially Plastic, SSS adds translucency and absorption to show light scattering within the depth of the material.",
	"   For an accurate SSS effect, opacity should be 1, and for dark diffuse colors, use a light translucency of a similar hue, and vice versa",
	"   Volume Lighting MUST be enabled in the render / export dialog.",
	"    KT Diffuse    -> diffuse * opacity",
	"    KT Specular   -> specular; Intensity (V in HSV) should ALWAYS be 1.0",
	"    KT Refraction -> diffuse * (1 - opacity)",
	"    KT Shininess  -> shininess (exp range 1 - ~44,000)",
	"    KT Ambient    -> ambient setting is completely ignored.",
	"    KT IOR        -> IOR",
	"    KT Translucency -> translucency",
	"    KT Absorption -> white * absorption",
	"    A diffuse map will be used in place of diffuse color.  No opacity is applied, therefore there is no refraction.",
	"    A gloss map creates a layered material around the plastic, mapping between the specular material and a flat diffuse version.",
	"    Specular sampling is enabled.  This will result in fuzzy reflection based on the shininess value.",
	"  Emission will cause the material to emit light.  Height and normal maps are applied as Bump and Normal maps."
     ].	
     
     
export_standard_raytracer(F, Name, Options)->
	{AA, AAThreshold, PathTrace, SubOpts} = Options,
	{Quality, Translucence, _Caustics} = SubOpts,
	
	case AA of
		production -> AAName = "Production AA",
			TextureFilter = 1;
		iterative -> AAName = "Iterative Pass 3x3",
			TextureFilter = 1;
		pass -> AAName = "Extra Pass 3x3",
			TextureFilter = 1;
		_ -> AAName = "None",
			TextureFilter = 0
	end,
	
	case Quality of
		medium -> DiffuseSamples = 64,
			SpecularSamples = 8;
		high -> DiffuseSamples = 256,
			SpecularSamples = 16;
		_ -> DiffuseSamples = 16,
			SpecularSamples = 4
	end,
	
	case PathTrace of
		true -> 
			TraceDiffusers = 1,
			case Quality of
				medium->Brightness = 0.001,
					MaxTracingDepth = 6;
				high ->Brightness = 0.0005,
					MaxTracingDepth = 6;
				_ -> Brightness = 0.002,
					MaxTracingDepth = 5
			end;
		_ ->
			Brightness = 0.002,
			TraceDiffusers = 0,
			MaxTracingDepth = 5
	end,
	
	case Translucence of
		normal -> Translucency = 1;
		_ -> Translucency = 0
	end,
			
	
	export_standard_raytracer_ind(F, Name, {AAName, AAThreshold, TextureFilter, DiffuseSamples, SpecularSamples, Translucency, Brightness, TraceDiffusers, MaxTracingDepth}).

export_standard_raytracer_ind(F, Name, ExpandedOptions)->
	{AAName, AAThreshold, TextureFilter, DiffuseSamples, SpecularSamples, Translucency, Brightness, TraceDiffusers, MaxTracingDepth} = ExpandedOptions,
	io:format(F, "<Object Identifier=\"./Ray Tracers/~s\" Label=\"Standard Ray Tracer\" Name=\"~s\" Type=\"Ray Tracer\">\n", [Name, Name]),
	io:put_chars(F, "\t<Parameter Name=\"Rasterization\" Type=\"String\" Value=\"Auto\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"Antialiasing\" Type=\"String\" Value=\"~s\"/>\n", [AAName]),
	io:format(F, "\t<Parameter Name=\"Texture Filtering\" Type=\"Boolean\" Value=\"~p\"/>\n", [TextureFilter]),
	
	io:put_chars(F, "\t<Parameter Name=\"Antialiasing Filter\" Type=\"String\" Value=\"Mitchell-Netravali 0.5 0.8\"/>\n"),
	io:format(F, "\t<Parameter Name=\"Antialiasing Threshold\" Type=\"Real\" Value=\"~f\"/>\n", [AAThreshold]),
	io:format(F, "\t<Parameter Name=\"Brightness Threshold\" Type=\"Real\" Value=\"~f\"/>\n", [Brightness]),
	io:format(F, "\t<Parameter Name=\"Max Ray Tracing Depth\" Type=\"Integer\" Value=\"~p\"/>\n", [MaxTracingDepth]),
	io:put_chars(F, "\t<Parameter Name=\"Irradiance Scale\" Type=\"RGB\" Value=\"1 1 1\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"./Sampling Criteria/Diffuse Samples\" Type=\"Integer\" Value=\"~p\"/>\n", [DiffuseSamples]),
	io:format(F, "\t<Parameter Name=\"./Sampling Criteria/Specular Samples\" Type=\"Integer\" Value=\"~p\"/>\n", [SpecularSamples]),
	
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Dispersion Samples\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:format(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Diffusers\" Type=\"Boolean\" Value=\"~p\"/>\n", [TraceDiffusers]),
	io:format(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Translucencies\" Type=\"Boolean\" Value=\"~p\"/>\n", [Translucency]),
	
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Fuzzy Reflections\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Fuzzy Refractions\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Reflections\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Refractions\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Random Generator\" Type=\"String\" Value=\"Pure\"/>\n"),
	io:put_chars(F, "</Object>\n").
	
export_progressive_raytracer(F, Name)->
	io:format(F, "<Object Identifier=\"./Ray Tracers/~s\" Label=\"Standard Ray Tracer\" Name=\"~s\" Type=\"Ray Tracer\">\n", [Name, Name]),
	io:put_chars(F, "\t<Parameter Name=\"Rasterization\" Type=\"String\" Value=\"Progressive\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Antialiasing\" Type=\"String\" Value=\"None\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Texture Filtering\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Antialiasing Threshold\" Type=\"Real\" Value=\"0.3\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Irradiance Scale\" Type=\"RGB\" Value=\"1 1 1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Iterations\" Type=\"Integer\" Value=\"10000\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Diffuse Samples\" Type=\"Integer\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Specular Samples\" Type=\"Integer\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Brightness Threshold\" Type=\"Real\" Value=\"0.001\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Ray Tracing Depth\" Type=\"Integer\" Value=\"6\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Image Filename\" Type=\"String\" Value=\"temp_ppt.jpg\"/>\n"), 
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Dispersion Samples\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Diffusers\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Translucencies\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Fuzzy Reflections\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Fuzzy Refractions\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Reflections\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Refractions\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Random Generator\" Type=\"String\" Value=\"Pure\"/>\n"),
	io:put_chars(F, "</Object>\n").
	
export_bidirectional_raytracer(F, Name)->
	io:format(F, "<Object Identifier=\"./Ray Tracers/~s\" Label=\"Bidirectional Path Tracer\" Name=\"~s\" Type=\"Ray Tracer\">\n", [Name, Name]),
	io:put_chars(F, "\t<Parameter Name=\"Max Ray Tracing Depth\" Type=\"Integer\" Value=\"100\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Iterations\" Type=\"Integer\" Value=\"10000\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Image Filename\" Type=\"String\" Value=\"temp_bipt.jpg\"/>\n"), 
	io:put_chars(F, "</Object>\n").
	
export_mlt_raytracer(F, Name, Options)->
	{Bidirectional, Filename} = Options,
	io:format(F, "<Object Identifier=\"./Ray Tracers/~s\" Label=\"Metropolis Light Transport\" Name=\"~s\" Type=\"Ray Tracer\">\n", [Name, Name]),
	io:put_chars(F, "\t<Parameter Name=\"Max Ray Tracing Depth\" Type=\"Integer\" Value=\"100\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Iterations\" Type=\"Integer\" Value=\"10000\"/>\n"),
	io:format(F, "\t<Parameter Name=\"Image Filename\" Type=\"String\" Value=\"~s\"/>\n", [Filename]),
	io:put_chars(F, "\t<Parameter Name=\"Linear Lightflow\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Random Seed\" Type=\"String\" Value=\"Automatic\"/>\n"), 
	io:put_chars(F, "\t<Parameter Name=\"Seed Paths\" Type=\"Integer\" Value=\"50000\" /> \n"), 
	io:put_chars(F, "\t<Parameter Name=\"Large Step Probability\" Type=\"Real\" Value=\"0.2\" /> \n"), 
	io:put_chars(F, "\t<Parameter Name=\"Max Mutation Distance\" Type=\"Real\" Value=\"0.02\" /> \n"), 
	io:put_chars(F, "\t<Parameter Name=\"Live Probability\" Type=\"Real\" Value=\"0.7\" /> \n"), 
	io:put_chars(F, "\t<Parameter Name=\"Max Consecutive Rejections\" Type=\"Integer\" Value=\"200\" /> \n"), 
	io:format(F, "\t<Parameter Name=\"Bidirectional\" Type=\"Boolean\" Value=\"~p\" /> \n", [Bidirectional]), 
	io:put_chars(F, "</Object>\n").

export_directlight_estimator(F, Options)->
	{Quality, Translucency, Caustics} = Options,
	
	case Quality of
		medium -> AAQuality = "Low";
		high -> AAQuality = "Medium";
		_ -> AAQuality = "Lower"
	end,
	
	case Translucency of
		pseudo -> PseudoXL = 1;
		_ -> PseudoXL = 0
	end,
	
	case Caustics of
		pseudo -> PseudoCS = 1;
		_ -> PseudoCS = 0
	end,
		
	
	export_directlight_estimator_ind(F, {AAQuality, PseudoXL, PseudoCS}).
	
export_directlight_estimator_ind(F, Options)->
	{AAQuality, PseudoXL, PseudoCS} = Options,
	io:put_chars(F, "<Object Identifier=\"./Direct Light Estimators/Refraction Enhanced\" Label=\"Refraction Enhanced\" Name=\"Refraction Enhanced\" Type=\"Direct Light Estimator\">\n"),
	io:put_chars(F, "\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"1\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"PseudoCaustics\" Type=\"Boolean\" Value=\"~p\"/>\n", [PseudoCS]),
	io:format(F, "\t<Parameter Name=\"PseudoTranslucencies\" Type=\"Boolean\" Value=\"~p\"/>\n", [PseudoXL]),
	io:format(F, "\t<Parameter Name=\"Antialiasing\" Type=\"String\" Value=\"~s\"/>\n", [AAQuality]),

	io:put_chars(F, "\t<Parameter Name=\"Optimized Area Lights\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Accurate Soft Shadows\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t</Object>\n").

export_interreflection_estimator(F, Options)->
	{Quality, _Translucency, _Caustics} = Options,
	
	case Quality of
		medium -> FGRays = 400,
			Accuracy = 0.25;
		high -> FGRays = 900,
			Accuracy = 0.15;
		_ -> FGRays = 150,
			Accuracy = 0.25
	end,
	
	export_interreflection_estimator_ind(F, {FGRays, Accuracy}).
	
export_interreflection_estimator_ind(F, Options)->
	{FGRays, Accuracy} = Options,
	io:put_chars(F, "<Object Identifier=\"./Irradiance Estimators/Diffuse Interreflection\" Label=\"Diffuse Interreflection\" Name=\"Diffuse Interreflection\" Type=\"Irradiance Estimator\">\n"),
	io:put_chars(F, "\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Recursion Depth\" Type=\"Integer\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Ray Tracing Depth\" Type=\"Integer\" Value=\"5\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"Final Gathering Rays\" Type=\"Integer\" Value=\"~p\"/>\n",[FGRays]),
	io:format(F, "\t<Parameter Name=\"Accuracy\" Type=\"Real\" Value=\"~f\"/>\n", [Accuracy]),
	
	io:put_chars(F, "\t<Parameter Name=\"Light Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Ambient Lighting\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Minimum Pixel Reuse\" Type=\"Real\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Maximum Pixel Reuse\" Type=\"Real\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Radiance Limit\" Type=\"Real\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Secondary Estimator\" Type=\"String\" Value=\"Density Estimation\"/>\n"),
	io:put_chars(F, "</Object>\n").

export_density_estimator(F, Options)->
	{Quality, _Translucency, Caustics} = Options,
	
	case Quality of
		medium -> MaxPhotonDepth = 6,
			FGAccuracy = 0.25,
			SamplesPerLight = 100000,
			CausticCount = 512;
		high -> MaxPhotonDepth = 7,
			FGAccuracy = 0.15,
			SamplesPerLight = 1000000,
			CausticCount = 1024;
		_ -> MaxPhotonDepth = 5,
			FGAccuracy = 0.25,
			SamplesPerLight = 20000,
			CausticCount = 256
	end,
	
	case Caustics of
		normal -> CausticSamples = 1,
			DispersionSamples = 1;
		_ -> CausticSamples = 0,
			DispersionSamples = 0
	end,
	
	export_density_estimator_ind(F, {MaxPhotonDepth, FGAccuracy, SamplesPerLight, DispersionSamples, CausticSamples, CausticCount}).
	
export_density_estimator_ind(F, Options)->
	{MaxPhotonDepth, FGAccuracy, SamplesPerLight, DispersionSamples, CausticSamples, CausticCount} = Options,
	io:put_chars(F, "<Object Identifier=\"./Irradiance Estimators/Density Estimation\" Label=\"Density Estimation\" Name=\"Density Estimation\" Type=\"Irradiance Estimator\">\n"),
	io:put_chars(F, "\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Direct Lighting\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Final Gathering\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Mesh Subdivision\" Type=\"Boolean\" Value=\"1\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"Max Photon Tracing Depth\" Type=\"Integer\" Value=\"~p\"/>\n", [MaxPhotonDepth]),
	io:format(F, "\t<Parameter Name=\"./Final Gathering/Accuracy\" Type=\"Real\" Value=\"~f\"/>\n", [FGAccuracy]),
	io:format(F, "\t<Parameter Name=\"Samples per Light\" Type=\"Integer\" Value=\"~p\"/>\n", [SamplesPerLight]),
	
	io:put_chars(F, "\t<Parameter Name=\"Terminating Brightness\" Type=\"Real\" Value=\"0.01\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Sample Sky\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Diffuse Samples\" Type=\"Integer\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Specular Samples\" Type=\"Integer\" Value=\"0\"/>\n"),
	
	io:format(F, "\t<Parameter Name=\"./Sampling Criteria/Dispersion Samples\" Type=\"Boolean\" Value=\"~p\"/>\n", [DispersionSamples]),
	io:format(F, "\t<Parameter Name=\"./Final Gathering/Caustics\" Type=\"Boolean\" Value=\"~p\"/>\n", [CausticSamples]),
	io:format(F, "\t<Parameter Name=\"./Final Gathering/Caustic Count\" Type=\"Integer\" Value=\"~p\"/>\n", [CausticCount]),
			
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Reflections\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Trace Refractions\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Importance Sampling\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Sampling Criteria/Russian Roulette\" Type=\"Boolean\" Value=\"0\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Final Gathering/Hardware Rendering\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Final Gathering/Final Gathering Rays\" Type=\"Integer\" Value=\"900\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Final Gathering/Crop Intensity (HR)\" Type=\"Real\" Value=\"3\"/>\n"),
	io:put_chars(F, "</Object>\n").
	
expOCTTREE(F)->
	io:put_chars(F, "<Object Identifier=\"./Environments/Octree Environment\" Label=\"Octree Environment\" Name=\"Octree Environment\" Type=\"Environment\">\n"),
	io:put_chars(F, "\t<Parameter Name=\"Max Objects per Cell\" Type=\"Integer\" Value=\"20\"/>\n"),
	io:put_chars(F, "</Object>\n").

expTONEMAPPING(F)->
	io:put_chars(F, "<Object Identifier=\"./Filters/Simple Tone Mapping\" Label=\"Simple Tone Mapping\" Name=\"\" Type=\"Filter\">\n"),
	io:put_chars(F, "\t<Parameter Name=\"Enabled\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Method\" Type=\"String\" Value=\"Simple\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Exposure\" Type=\"Real\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Gamma Correction\" Type=\"Real\" Value=\"2.2\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Dark Multiplier\" Type=\"Real\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Bright Multiplier\" Type=\"Real\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"Reverse Correction\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "</Object>\n").
	
expSCENE(F, NullIrradEst, Name)->
	io:put_chars(F, "\t<Parameter Name=\"Mip Mapping\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Interfaces/Active\" Type=\"String\" Value=\"Null Interface\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Modellers/Active\" Type=\"String\" Value=\"XML Modeller\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Image Handlers/Active\" Type=\"String\" Value=\"Free Image Support\"/>\n"),
	io:format(F, "\t<Parameter Name=\"./Ray Tracers/Active\" Type=\"String\" Value=\"~s\"/>\n", [Name]),
	case NullIrradEst of
		true ->io:put_chars(F, "\t<Parameter Name=\"./Irradiance Estimators/Active\" Type=\"String\" Value=\"Null Irradiance Estimator\"/>\n");
		_ ->io:put_chars(F, "\t<Parameter Name=\"./Irradiance Estimators/Active\" Type=\"String\" Value=\"Diffuse Interreflection\"/>\n")
	end,
	io:put_chars(F, "\t<Parameter Name=\"./Direct Light Estimators/Active\" Type=\"String\" Value=\"Refraction Enhanced\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Environments/Active\" Type=\"String\" Value=\"Octree Environment\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Filters/Active\" Type=\"String\" Value=\"Simple Tone Mapping\"/>\n").

expBIPT_SCENE(F, Name)->
	io:put_chars(F, "\t<Parameter Name=\"Mip Mapping\" Type=\"Boolean\" Value=\"1\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Interfaces/Active\" Type=\"String\" Value=\"Null Interface\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Modellers/Active\" Type=\"String\" Value=\"XML Modeller\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Image Handlers/Active\" Type=\"String\" Value=\"Free Image Support\"/>\n"),
	io:format(F, "\t<Parameter Name=\"./Ray Tracers/Active\" Type=\"String\" Value=\"~s\"/>\n", [Name]),
	io:put_chars(F, "\t<Parameter Name=\"./Irradiance Estimators/Active\" Type=\"String\" Value=\"Null Irradiance Estimator\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Direct Light Estimators/Active\" Type=\"String\" Value=\"Null Direct Light Estimator\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Environments/Active\" Type=\"String\" Value=\"Octree Environment\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Filters/Active\" Type=\"String\" Value=\"Simple Tone Mapping\"/>\n").

expMLT_SCENE(F, Name)->
	io:format(F, "\t<Parameter Name=\"./Ray Tracers/Active\" Type=\"String\" Value=\"~s\"/>\n", [Name]),
	io:put_chars(F, "\t<Parameter Name=\"./Irradiance Estimators/Active\" Type=\"String\" Value=\"Null Irradiance Estimator\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Direct Light Estimators/Active\" Type=\"String\" Value=\"Null Direct Light Estimator\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Environments/Active\" Type=\"String\" Value=\"Octree Environment\"/>\n"),
	io:put_chars(F, "\t<Parameter Name=\"./Filters/Active\" Type=\"String\" Value=\"Simple Tone Mapping\"/>\n").
