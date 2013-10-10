%%
%%  wpc_yafray.erl
%%
%%     YafRay Plugin User Interface.
%%
%%  Copyright (c) 2003-2011 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_yafray).
-export([init/0,menu/2,dialog/2,command/2]).

%% Debug exports
%% -export([now_diff_1/1]).

-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keydelete/3,
		foreach/2,foldl/3,foldr/3]).

-define(TAG, yafray).
-define(KEY(K), {?TAG,(K)}).
-define(TAG_RENDER, yafray_render).

key(Key) -> {key,?KEY(Key)}.

-define(NONZERO, 1.0e-10).

%%% Default values
-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "yafray").
-define(DEF_OPTIONS, "").
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_KEEP_XML, false).
-define(DEF_SAVE_ALPHA, false).
-define(DEF_GAMMA, 1.0).
-define(DEF_EXPOSURE, 1.41421356237).
-define(DEF_FOG_DENSITY, 0.0).
-define(DEF_FOG_COLOR, {1.0,1.0,1.0,1.0}).
-define(DEF_RENDER_FORMAT, tga).
-define(DEF_EXR_FLAG_COMPRESSION, compression_zip).

%% Shader
-define(DEF_SHADER_TYPE, generic).
-define(DEF_CAUS, false).
-define(DEF_TIR, false).
-define(DEF_IOR, 1.5).
-define(DEF_MIN_REFLE, 0.0).
-define(DEF_USE_HARDNESS, false).
-define(DEF_AUTOSMOOTH, true).
-define(DEF_AUTOSMOOTH_ANGLE, 60.0).
-define(DEF_SHADOW,true).
-define(DEF_EMIT_RAD,true).
-define(DEF_RECV_RAD,true).
-define(DEF_FAST_FRESNEL,false).
-define(DEF_ABSORPTION_COLOR, {1.0,1.0,1.0}).
-define(DEF_ABSORPTION_DIST, 1.0).
-define(DEF_DISPERSION_POWER, 0.0).
-define(DEF_DISPERSION_SAMPLES, 10).
-define(DEF_DISPERSION_JITTER, false).
%% Arealight
-define(DEF_AREALIGHT, false).
-define(DEF_AREALIGHT_SAMPLES, 50).
-define(DEF_AREALIGHT_PSAMPLES, 0).
-define(DEF_DUMMY, false).
-define(DEF_QMC_METHOD, 0).
-define(DEF_AREALIGHT_RADIUS, 1.0).

%% Render
-define(DEF_AA_PASSES, 0).
-define(DEF_AA_MINSAMPLES, 5).
-define(DEF_AA_PIXELWIDTH, 1.0).
-define(DEF_AA_THRESHOLD, 0.125).
-define(DEF_AA_JITTERFIRST, false).
-define(DEF_CLAMP_RGB, false).
-define(DEF_RAYDEPTH, 12).
-define(DEF_BIAS, 0.001).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).
-define(DEF_ORTHO, false).
-define(DEF_APERTURE, 0.0).
-define(DEF_BOKEH_TYPE, disk1).
-define(DEF_BOKEH_BIAS, uniform).
-define(DEF_BOKEH_ROTATION, 0.0).

%% Light
-define(DEF_ATTN_POWER, 10.0).
-define(DEF_POINT_TYPE, pointlight).
-define(DEF_CAST_SHADOWS, true).
-define(DEF_USE_QMC, false).
-define(DEF_GLOW_INTENSITY, 0.0).
-define(DEF_GLOW_OFFSET, 0.0).
-define(DEF_GLOW_TYPE, 0).
%% Spotlight
-define(DEF_SPOT_TYPE, spotlight).
-define(DEF_CONE_ANGLE, 45.0).
-define(DEF_SPOT_EXPONENT, 2.0).
-define(DEF_BLEND, 5.0).
-define(DEF_HALO, false).
-define(DEF_HALO_RES, 512).
-define(DEF_HALO_BLUR, 0.0).
-define(DEF_HALO_SHADOW_BLUR, 0.0).
-define(DEF_HALO_FOG_DENSITY, 0.0).
-define(DEF_HALO_FOG_COLOR, {0.0,0.0,0.0}).

%% Photonlight
-define(DEF_MODE,caustic).
-define(DEF_PHOTONS,5000000).
-define(DEF_SEARCH,64).
-define(DEF_DEPTH,3).
-define(DEF_CAUS_DEPTH,4).
-define(DEF_DIRECT,false).
-define(DEF_MINDEPTH,1).
-define(DEF_FIXEDRADIUS,0.08).
-define(DEF_CLUSTER,0.01).
%% Softlight
-define(DEF_RES, 100).
-define(DEF_RADIUS, 1).
%% Sunlight
-define(DEF_POWER, 1.0).
-define(DEF_BACKGROUND, undefined).
-define(DEF_BACKGROUND_COLOR, {0.0,0.0,0.0}).
-define(DEF_TURBIDITY, 4.0).
-define(DEF_SUNSKY_VAR, 1.0).
%% Hemilight and Pathlight
-define(DEF_AMBIENT_TYPE, hemilight).
-define(DEF_USE_MAXDISTANCE, false).
-define(DEF_MAXDISTANCE, 1.0).
-define(DEF_BACKGROUND_FILENAME, "").
-define(DEF_BACKGROUND_EXPOSURE_ADJUST, 0).
-define(DEF_BACKGROUND_MAPPING, probe).
-define(DEF_BACKGROUND_POWER, 1.0).
-define(DEF_BACKGROUND_PREFILTER, true).
-define(DEF_SAMPLES, 256).
%% Pathlight
-define(DEF_PATHLIGHT_MODE, undefined).
-define(DEF_CACHE, false).
-define(DEF_CACHE_SIZE, 0.01).
-define(DEF_ANGLE_THRESHOLD, 0.2).
-define(DEF_SHADOW_THRESHOLD, 0.3).
-define(DEF_GRADIENT, false).
-define(DEF_SHOW_SAMPLES, false).
%% Global Photonlight
-define(DEF_GLOBALPHOTONLIGHT_PHOTONS, 50000).
-define(DEF_GLOBALPHOTONLIGHT_RADIUS, 1.0).
-define(DEF_GLOBALPHOTONLIGHT_DEPTH, 2).
-define(DEF_GLOBALPHOTONLIGHT_SEARCH, 200).

%% Modulator
-define(DEF_MOD_ENABLED, true).
-define(DEF_MOD_MODE, mix).
-define(DEF_MOD_SIZE, 1.0).
-define(DEF_MOD_SIZE_X, 1.0).
-define(DEF_MOD_SIZE_Y, 1.0).
-define(DEF_MOD_SIZE_Z, 1.0).
-define(DEF_MOD_OPACITY, 1.0).
-define(DEF_MOD_DIFFUSE, 0.0).
-define(DEF_MOD_SPECULAR, 0.0).
-define(DEF_MOD_AMBIENT, 0.0).
-define(DEF_MOD_SHININESS, 0.0).
-define(DEF_MOD_NORMAL, 0.0).
-define(DEF_MOD_TYPE, image).
-define(DEF_MOD_FILENAME, "").
-define(DEF_MOD_COLOR1, {0.0,0.0,0.0}).
-define(DEF_MOD_COLOR2, {1.0,1.0,1.0}).
-define(DEF_MOD_DEPTH, 2).
-define(DEF_MOD_HARD, false).
-define(DEF_MOD_TURBULENCE, 1.0).
-define(DEF_MOD_SHARPNESS, 1.0).
-define(DEF_MOD_RINGSCALE_X, 1.0).
-define(DEF_MOD_RINGSCALE_Z, 1.0).

%% Block shaders
-define(DEF_SHININESS, 1.0).
-define(DEF_DIFFUSE, {1.0,1.0,1.0}).
-define(DEF_BS, {block_shader,[]}).
-define(DEF_BS_TOP, {phong,[]}).
-define(DEF_BS_COLOR, {rgb, []}).
-define(DEF_BSCOL(Color), {rgb,[{color,Color}]}).
-define(DEF_BS_H, 1.0).
-define(DEF_BS_S, 1.0).
-define(DEF_BS_V, 1.0).
-define(DEF_BS_FLOAT, {coords, []}).
-define(DEF_BS_COORD, 'Y').
-define(DEF_BS_MUL_VALUE, 1.0).
-define(DEF_BS_MIX_MODE, add).
-define(DEF_BS_CONE_MODE, reflect).
-define(DEF_BS_CONE_SAMPLES, 1).
-define(DEF_BS_CONE_ANGLE, 0.0).
-define(DEF_BS_CONE_COLOR, {1.0,1.0,1.0}).
-define(DEF_BS_GOBO_HARD, true).
-define(DEF_BS_GOBO_EDGE, 0.5).
-define(DEF_BS_SSS_COLOR, {0.0,0.0,0.0}).
-define(DEF_BS_SSS_RADIUS, 0.1).
-define(DEF_BS_SSS_SAMPLES, 32).

range(T) -> {range,range_1(T)}.

%% Material ranges
range_1(autosmooth_angle)	-> {0.0,180.0};
range_1(ior)			-> {0.0,infinity};
range_1(min_refle)		-> {0.0,1.0};
range_1(size)			-> {0.0,infinity};
range_1(modulation)		-> {0.0,1.0};
range_1(turbulence)		-> {?NONZERO,infinity};
range_1(scale)			-> {?NONZERO,infinity};
range_1(sharpness)		-> {1.0,infinity};
range_1(noise_depth)		-> {1,infinity};
range_1(absorption_dist)	-> {?NONZERO,infinity};
range_1(dispersion_power)	-> {0.0,1.0};
range_1(dispersion_samples)	-> {1,infinity};
%% Light ranges
range_1(power)			-> {0.0,infinity};
range_1(bias)			-> {0.0,1.0};
range_1(res)			-> {0,infinity};
range_1(radius)			-> {0,infinity};
range_1(blur)			-> {0.0,1.0};
range_1(samples)		-> {1,infinity};
range_1(halo_fog_density)	-> {0.0,infinity};
range_1(glow_intensity)		-> {0.0,1.0};
range_1(glow_offset)		-> {0.0,infinity};
range_1(blend)			-> {0.0,infinity};
range_1(photons)		-> {0,infinity};
range_1(depth)			-> {0,infinity};
range_1(fixedradius)		-> {0.0,infinity};
range_1(search)			-> {0,infinity};
range_1(cluster)		-> {0.0,infinity};
range_1(turbidity)		-> {0.0,infinity};
range_1(angle_threshold)	-> {0.0,1.0};
range_1(raydepth)		-> {1,infinity};
range_1(cache_size)		-> {0.0,infinity};
range_1(shadow_threshold)	-> {0.0,infinity};
range_1(cache_search)		-> {3,infinity};
range_1(exposure_adjust)	-> {-128,127};
range_1(psamples)		-> {0,infinity};
range_1(arealight_radius)	-> {0.0,infinity};
range_1(maxdistance)		-> {0.0,infinity};
%% Render ranges
range_1(subdivisions)		-> {0,infinity};
range_1(aa_pixelwidth)		-> {1.0,2.0};
range_1(aa_passes)		-> {0,infinity};
range_1(aa_threshold)		-> {0.0,1.0};
range_1(aa_minsamples)		-> {1,infinity};
range_1(gamma)			-> {0.0,infinity};
range_1(exposure)		-> {0.0,infinity};
range_1(pixels)			-> {1,infinity};
range_1(fog_density)		-> {0.0,infinity};
range_1(aperture)		-> {0.0,infinity};
range_1(bokeh_rotation)		-> {-180.0,180.0};
%% Block Shader ranges
range_1(shininess)		-> {0.0,1.0};
range_1(hsv)			-> {0.0,1.0};
range_1(cone_angle)		-> {0.0,90.0};
range_1(sss_radius)             -> {?NONZERO,infinity};
range_1(sss_samples)            -> {1,infinity}.



%% Exported plugin callback functions
%%

init() ->
    init_pref(),
    set_var(rendering, false),
    true.

menu({file,export}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,export_selected}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,render}, Menu) ->
    maybe_append(render, Menu, menu_entry(render));
menu({edit,plugin_preferences}, Menu) ->
    Menu++menu_entry(pref);
menu(_, Menu) ->
    Menu.

command({file,{export,{?TAG,A}}}, St) ->
    command_file(export, A, St);
command({file,{export_selected,{?TAG,A}}}, St) ->
    command_file(export_selected, A, St);
command({file,{render,{?TAG,A}}}, St) ->
    command_file(render, A, St);
command({edit,{plugin_preferences,?TAG}}, St) ->
    pref_dialog(St);
command(_Spec, _St) ->
%    erlang:display({?MODULE,?LINE,Spec}),
    next.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    maybe_append(edit, Dialog, material_dialog(Name, Mat));
dialog({material_editor_result,Name,Mat}, Res) ->
    case is_plugin_active(edit) of
	false ->
	    {Mat,Res};
	_ ->
	    material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    maybe_append(edit, Dialog, light_dialog(Name, Ps));
dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
	false ->
	    {Ps0,Res};
	_ ->
	    light_result(Name, Ps0, Res)
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.

%%
%% End of exported plugin callback functions



init_pref() ->
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    RendererPath =
	case filename:pathtype(Renderer) of
	    absolute -> 
		Renderer;
	    _ -> 
		case wings_job:find_executable(Renderer) of
		    false ->
			false;
		    Path -> 
			Path
		end
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
    ok.

maybe_append(Condition, Menu, PluginMenu) ->
    case {is_plugin_active(Condition),Menu} of
	{_,[plugin_manager_category]} ->
	    Menu++PluginMenu;
	{false,_} ->
	    Menu;
	{_,_} ->
	    Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
	export ->
	    get_var(dialogs);
	edit ->
	    get_var(dialogs);
	render ->
	    get_var(renderer)
    end.

menu_entry(render) ->
    [{"YafRay",?TAG,[option]}];
menu_entry(export) ->
    [{"YafRay (.xml)",?TAG,[option]}];
menu_entry(pref) ->
    [{"YafRay",?TAG}].



command_file(render, Attr, St) when is_list(Attr) ->
    set_prefs(Attr),
    case get_var(rendering) of
	false ->
	    do_export(export, 
		      props(render, Attr), 
		      [{?TAG_RENDER,true}|Attr], St);
	true ->
	    wpa:error_msg(?__(1,"Already rendering."))
    end;
command_file(render=Op, Ask, _St) when is_atom(Ask) ->
    export_dialog(Op, Ask, ?__(2,"YafRay Render Options"),
		  fun(Attr) -> {file,{Op,{?TAG,Attr}}} end);
command_file(Op, Attr, St) when is_list(Attr) ->
    %% when Op =:= export; Op =:= export_selected
    set_prefs(Attr),
    do_export(Op, props(Op, Attr), Attr, St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    export_dialog(Op, Ask, ?__(3,"YafRay Export Options"),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

-record(camera_info, {pos,dir,up,fov}).

do_export(Op, Props0, Attr0, St) ->
    SubDiv = proplists:get_value(subdivisions, Attr0, ?DEF_SUBDIVISIONS),
    Props = [{subdivisions,SubDiv}|Props0],
    [{Pos,Dir,Up},Fov] = wpa:camera_info([pos_dir_up,fov]),
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov},
    Attr = [CameraInfo,{lights,wpa:lights(St)}|Attr0],
    ExportFun = 
	fun (Filename, Contents) ->
		case catch export(Attr, Filename, Contents) of
		    ok ->
			ok;
		    Error ->
			io:format(?__(1,"ERROR: Failed to export")++":~n~p~n", [Error]),
			{error,?__(2,"Failed to export")}
		end
	end,
    wpa:Op(Props, ExportFun, St).

props(render, Attr) ->
    RenderFormat = 
	proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    {value,{RenderFormat,Ext,Desc}} =
	lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
    Title = case os:type() of
        {win32,_} -> "Render";
        _Other    -> ?__(1,"Render")
	end,
    [{title,Title},{ext,Ext},{ext_desc,Desc}];
props(export, _Attr) ->
    {Title,File} = case os:type() of
        {win32,_} -> {"Export","YafRay File"};
        _Other    -> {?__(2,"Export"),?__(5,"YafRay File")}
	end,
    [{title,Title},{ext,".xml"},{ext_desc,File}];
props(export_selected, _Attr) ->
    {Title,File} = case os:type() of
        {win32,_} -> {"Export Selected","YafRay File"};
        _Other    -> {?__(4,"Export Selected"),?__(5,"YafRay File")}
	end,
    [{title,Title},{ext,".xml"},{ext_desc,File}].



%%% Dialogues and results
%%%

material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    DefReflected = alpha(proplists:get_value(specular, OpenGL)),
    DefTransmitted = def_transmitted(proplists:get_value(diffuse, OpenGL)),
    YafRay = proplists:get_value(?TAG, Mat, []),
    Minimized = proplists:get_value(minimized, YafRay, true),
    ObjectMinimized = proplists:get_value(object_minimized, YafRay, true),
    ShaderType = proplists:get_value(shader_type, YafRay, ?DEF_SHADER_TYPE),
    BlockShader = proplists:get_value(block_shader, YafRay, ?DEF_BS),
    Caus = proplists:get_value(caus, YafRay, ?DEF_CAUS),
    Shadow = proplists:get_value(shadow, YafRay, ?DEF_SHADOW),
    EmitRad = proplists:get_value(emit_rad, YafRay, ?DEF_EMIT_RAD),
    RecvRad = proplists:get_value(recv_rad, YafRay, ?DEF_RECV_RAD),
    UseHardness = proplists:get_value(use_hardness, YafRay, ?DEF_USE_HARDNESS),
    TIR = proplists:get_value(tir, YafRay, ?DEF_TIR),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafRay, 
				     if AutosmoothAngle == 0.0 -> false;
					true -> ?DEF_AUTOSMOOTH end),
    FresnelMinimized = proplists:get_value(fresnel_minimized, YafRay, true),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    FastFresnel = proplists:get_value(fast_fresnel, YafRay, ?DEF_FAST_FRESNEL),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    Reflected = proplists:get_value(reflected, YafRay, DefReflected),
    Transmitted = 
	proplists:get_value(transmitted, YafRay, DefTransmitted),
    Fresnel2 = proplists:get_value(fresnel2, YafRay, false),
    Reflected2 = proplists:get_value(reflected2, YafRay, DefReflected),
    Transmitted2 = 
	proplists:get_value(transmitted2, YafRay, DefTransmitted),
    AbsorptionColor = 
	proplists:get_value(absorption_color, YafRay, ?DEF_ABSORPTION_COLOR),
    AbsorptionDist =
	proplists:get_value(absorption_dist, YafRay, ?DEF_ABSORPTION_DIST),
    DispersionPower =
	proplists:get_value(dispersion_power, YafRay, ?DEF_DISPERSION_POWER),
    DispersionSamples =
	proplists:get_value(dispersion_samples, YafRay, 
			    ?DEF_DISPERSION_SAMPLES),
    DispersionJitter = 
	proplists:get_value(dispersion_jitter, YafRay, 
			    ?DEF_DISPERSION_JITTER),
    Modulators = proplists:get_value(modulators, YafRay, def_modulators(Maps)),
    ObjectFrame = 
	{vframe,
	 [{hframe,[{?__(1,"Cast Shadow"),Shadow,[key(shadow)]},
		   {?__(2,"Emit Rad"),EmitRad,[key(emit_rad)]},
		   {?__(3,"Recv Rad"),RecvRad,[key(recv_rad)]},
		   panel,
		   help_button({material_dialog,object})]},
	  {hframe,[{?__(4,"Use Edge Hardness"),UseHardness,
		    [key(use_hardness)]},
		   {?__(5,"Caustic"),Caus,[key(caus)]}]},
	  {hframe,[{?__(6,"Autosmooth"),Autosmooth,[key(autosmooth)]},
		   {label,?__(7,"Angle")},
		   {slider,{text,AutosmoothAngle,
			    [range(autosmooth_angle),{width,5},
			     key(autosmooth_angle),
			     hook(enable, ?KEY(autosmooth))]}}]}],
	 [{title,?__(8,"Object Parameters")},{minimized,ObjectMinimized},
	  key(object_minimized)]},
    BlockShaderFlags = 
	[key(block_shader),
	 {hook,
	  fun (update, {Var,_I,{block_shader,_}=Val,Sto}) ->
%%% 		  io:format(?MODULE_STRING":~w ~p~n", 
%%% 			    [?LINE,Val]),
		  {store,gb_trees:update(Var, Val, Sto)};
	      (_, _) -> void
	  end}],
    ShaderFrame =
	{hframe,
	 [{menu,
	   [{?__(9,"Generic Shader"),generic},{?__(10,"Block Shader"),block}],
	   ShaderType,
	   [key(shader_type),layout]},
	  {value,BlockShader,BlockShaderFlags},
	  {button,?__(11,"Edit"),keep,
	   [block_shader_hook(?KEY(shader_type), 
			      ?KEY(block_shader),
			      Mat),
	    {drop_flags, [{index,-1}|BlockShaderFlags]}]}]},
    FresnelFrame =
	{vframe,
	 [{hframe,[{label,?__(12,"Index Of Refraction")},
		   {text,IOR,[range(ior),key(ior)]},
		   panel,
		   help_button({material_dialog,fresnel})]},
	  {hframe,
	   [{?__(13,"Fast Fresnel"),FastFresnel,[key(fast_fresnel)]},
	    {?__(14,"Total Internal Reflection"),TIR,[key(tir)]}],
	   [hook(enable, [member,?KEY(shader_type),generic])]},
	  {hframe,[{label,?__(15,"Minimum Reflection")},
		   {slider,{text,MinRefle,[range(min_refle),{width,5},
					   key(min_refle)]}}]},
	  {hframe,[{vframe,[{label,?__(16,"Reflected")},
			    {label,?__(17,"Transmitted")}]},
		   {vframe,[{slider,{color,Reflected,
				     [key(reflected)]}},
			    {slider,{color,Transmitted,
				     [key(transmitted)]}}]},
		   {vframe,[panel,
			    {button,?__(18,"Set Default"),keep,
			     [transmitted_hook(?KEY(transmitted))]}]}]},
	  {vframe,
	   [{?__(19,"Grazing Angle Colors"),Fresnel2,[key(fresnel2),layout]},
	    {hframe,[{vframe,[{label,?__(20,"Reflected")},
			      {label,?__(21,"Transmitted")}]},
		     {vframe,[{slider,{color,Reflected2,
				       [key(reflected2)]}},
			      {slider,{color,Transmitted2,
				       [key(transmitted2)]}}]},
		     {vframe,[panel,
			      {button,?__(22,"Set Default"),keep,
			       [transmitted_hook(?KEY(transmitted2))]}]}],
	     [hook(open, ?KEY(fresnel2))]},
	    {hframe,[{label,?__(23,"Absorption:")},
		     {color,AbsorptionColor,[key(absorption_color)]},
		     {label,"@"},
		     {text,AbsorptionDist,
		      [key(absorption_dist),
		       range(absorption_dist),
		       hook(enable, ['not',[member,?KEY(absorption_color),
					    ?DEF_ABSORPTION_COLOR]])]}]},
	    {vframe,[{hframe,[{label,?__(24,"Dispersion: Power")},
			      {slider,{text,DispersionPower,
				       [key(dispersion_power),
					range(dispersion_power)]}}]},
		     {hframe,[{label," "++?__(25,"Samples")},
			      {text,DispersionSamples,
			       [key(dispersion_samples),
				range(dispersion_samples)]},
			      {?__(26,"Jitter"),DispersionJitter,
			       [key(dispersion_jitter)]}],
		      [hook(enable, 
			    ['not',[member,?KEY(dispersion_power),0.0]])]}]}],
	   [hook(enable, [member,?KEY(shader_type),generic])]}],
	 [{title,?__(27,"Fresnel Parameters")},{minimized,FresnelMinimized},
	  key(fresnel_minimized)]},
    %%
    [{vframe,
      [ObjectFrame,
       ShaderFrame,
       FresnelFrame,
       {vframe,
	modulator_dialogs(Modulators, Maps),
	[hook(open, ['not',[member,?KEY(shader_type),block]])]}],
      [{title,?__(28,"YafRay Options")},{minimized,Minimized},key(minimized)]}].

alpha({R,G,B,A}) -> {R*A,G*A,B*A}.

def_transmitted({Dr,Dg,Db,Da}) ->
    Dt = 1-Da,
    {Dr*Dt,Dg*Dt,Db*Dt}.

transmitted_hook(Tag) ->
    {hook,fun (update, {_Var,_I,_Val,Sto}) ->
		  {Dr,Dg,Db} = gb_trees:get(diffuse, Sto),
		  Da = gb_trees:get(opacity, Sto),
		  Transmitted = def_transmitted({Dr,Dg,Db,Da}),
		  {store,gb_trees:update(Tag, Transmitted, Sto)};
	      (_, _) -> void end}.

def_modulators([]) ->
    [];
def_modulators([{diffuse,_}|Maps]) ->
    [{modulator,[{type,{map,diffuse}},{diffuse,1.0}]}
     |def_modulators(Maps)];
def_modulators([{ambient,_}|Maps]) ->
    [{modulator,[{type,{map,ambient}},{ambient,1.0}]}
     |def_modulators(Maps)];
def_modulators([{bump,_}|Maps]) ->
    [{modulator,[{type,{map,bump}},{normal,1.0}]}
     |def_modulators(Maps)];
def_modulators([{gloss,_}|Maps]) ->
    [{modulator,[{type,{map,gloss}},{shininess,1.0}]}
     |def_modulators(Maps)];
def_modulators([_|Maps]) ->
    def_modulators(Maps).

material_result(_Name, Mat0, [{?KEY(minimized),_}|_]=Res0) ->
    {Ps1,Res1} = split_list(Res0, 26),
    Ps2 = [{Key,Val} || {?KEY(Key),Val} <- Ps1],
    {Ps,Res} = modulator_result(Ps2, Res1),
    Mat = [?KEY(Ps)|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

modulator_dialogs(Modulators, Maps) ->
    modulator_dialogs(Modulators, Maps, 1).

modulator_dialogs([], _Maps, M) ->
    [{hframe,
      [{button,?__(1,"New Modulator"),done,[key(new_modulator)]},
       panel|
       if M =:= 1 -> [{button,?__(2,"Default Modulators"),done}];
	  true -> [] end]}];
modulator_dialogs([Modulator|Modulators], Maps, M) ->
    modulator_dialog(Modulator, Maps, M)++
	modulator_dialogs(Modulators, Maps, M+1).

modulator_dialog({modulator,Ps}, Maps, M) when is_list(Ps) ->
%    erlang:display({?MODULE,?LINE,[Ps,M,Maps]}),
    {Enabled,Mode,Type} = mod_enabled_mode_type(Ps, Maps),
    Minimized = proplists:get_value(minimized, Ps, true),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type,open_dialog},
		   {extensions,[{".jpg",?__(3,"JPEG compressed image")},
				{".tga",?__(4,"Targa bitmap")}]}],
%    erlang:display({?MODULE,?LINE,[Filename,AbsnameX,BrowseProps]}),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    RingscaleX = proplists:get_value(ringscale_x, Ps, ?DEF_MOD_RINGSCALE_X),
    RingscaleZ = proplists:get_value(ringscale_z, Ps, ?DEF_MOD_RINGSCALE_Z),
    TypeTag = {?TAG,type,M},
    MapsFrame = [{hradio,[{atom_to_list(Map),{map,Map}} || {Map,_} <- Maps],
		  Type,[{key,TypeTag},layout]}],
    [{vframe,
      [{hframe,[{?__(5,"Enabled"),Enabled,[{key,{?TAG,enabled,M}}]},
		{menu,[{?__(6,"Mix"),mix},{?__(7,"Mul"),mul},{?__(8,"Add"),add}],Mode,
		 [hook(enable, {?TAG,enabled,M})]},
		{button,?__(9,"Delete"),done}]},
       {vframe, % hook(enable, {?TAG,enabled,M})
	[{hframe,[{label,?__(10,"SizeX")},{text,SizeX,[range(size)]},
		  {label,?__(11,"SizeY")},{text,SizeY,[range(size)]},
		  {label,?__(12,"SizeZ")},{text,SizeZ,[range(size)]}]},
	 {hframe,[{vframe,[{label,?__(13,"Diffuse")++" "},
			   {label,?__(14,"Specular")},
			   {label,?__(15,"Ambient")},
			   {label,?__(16,"Shininess")},
			   {label,?__(17,"Normal")}]},
		  {vframe,[{slider,{text,Diffuse,[range(modulation)]}},
			   {slider,{text,Specular,[range(modulation)]}},
			   {slider,{text,Ambient,[range(modulation)]}},
			   {slider,{text,Shininess,[range(modulation)]}},
			   {slider,{text,Normal,[range(modulation)]}}]}]}]
	++MapsFrame++
	[{hradio,[{?__(18,"Image"),image},{?__(19,"Clouds"),clouds},
		  {?__(20,"Marble"),marble},{?__(21,"Wood"),wood}],
	  Type,[{key,TypeTag},layout]},
	 {vframe,
	  [{hframe,
	    [{hframe,
	      [{label,?__(22,"Filename")},
	       {button,{text,Filename,[{props,BrowseProps}]}}],
	      [hook(open, [member,{?TAG,type,M},image])]},
	     {hframe,
	      [{label,?__(23,"Color 1")},{color,Color1},
	       {label,?__(24,"Color 2")},{color,Color2},
	       {label,?__(25,"Depth")},{text,Depth,[range(noise_depth)]},
	       {?__(26,"Hard Noise"),Hard,
		[hook(open, [member,{?TAG,type,M},marble,wood])]}],
	      [hook(open, [member,{?TAG,type,M},clouds,marble,wood])]}]},
	   {hframe,
	    [{label,?__(27,"Turbulence")},{text,Turbulence,[range(turbulence)]},
	     {hframe,
	      [{label,?__(28,"Sharpness")},{text,Sharpness,[range(sharpness)]}],
	      [hook(open, [member,{?TAG,type,M},marble])]}],
	    [hook(open, [member,{?TAG,type,M},marble,wood])]},
	   {hframe,
	    [{label,?__(29,"Ringscale X")},{text,RingscaleX,[range(scale)]},
	     {label,?__(30,"Ringscale Z")},{text,RingscaleZ,[range(scale)]}],
	    [hook(open, [member,{?TAG,type,M},wood])]}]}],
	[hook(enable, {?TAG,enabled,M})]}],
      [{title,
	?__(31,"Modulator")++" "++integer_to_list(M)++mod_legend(Enabled, Mode, Type)},
       {minimized,Minimized}]}];
modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

mod_enabled_mode_type(Ps, Maps) ->
    {Enabled,Mode} = 
	case proplists:get_value(mode, Ps, ?DEF_MOD_MODE) of
	    off -> {false,?DEF_MOD_MODE};
	    Mode1 -> {proplists:get_value(enabled, Ps, ?DEF_MOD_ENABLED),Mode1}
	end,
    Type = proplists:get_value(type, Ps, ?DEF_MOD_TYPE),
    case Type of
	{map,Map} ->
	    case lists:keymember(Map, 1, Maps) of
		true -> {Enabled,Mode,Type};
		false -> {false,Mode,?DEF_MOD_TYPE}
	    end;
	_ -> {Enabled,Mode,Type}
    end.

mod_legend(Enabled, Mode, {map,Map}) ->
    mod_legend(Enabled, Mode, atom_to_list(Map));
mod_legend(Enabled, Mode, Type) when is_atom(Mode) ->
    mod_legend(Enabled, wings_util:cap(Mode), Type);
mod_legend(Enabled, Mode, Type) when is_atom(Type) ->
    mod_legend(Enabled, Mode, wings_util:cap(Type));
mod_legend(Enabled, Mode, Type) when is_list(Mode), is_list(Type) ->
    case Enabled of
	true -> " ("++?__(1,"enabled")++", ";
	false -> " ("++?__(2,"disabled")++", "
    end++Mode++", "++Type++")".


modulator_result(Ps, Res) ->
    modulator_result(Ps, Res, 1, []).

modulator_result(Ps, [], _, Modulators) ->
    %% Should not happen
    {[{modulators,reverse(Modulators)}|Ps], []};
modulator_result(Ps, [{?KEY(new_modulator),false},false|Res], 1, []) ->
    {[{modulators,[]}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),false},true|Res], 1, []) ->
    %% Default Modulators
    {Ps,Res};
modulator_result(Ps, [{?KEY(new_modulator),true},_|Res], 1, []) ->
    {[{modulators,[{modulator,[]}]}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),false}|Res], _, Modulators) ->
    {[{modulators,reverse(Modulators)}|Ps],Res};
modulator_result(Ps, [{?KEY(new_modulator),true}|Res], _, Modulators) ->
    {[{modulators,reverse(Modulators, [{modulator,[]}])}|Ps],Res};
modulator_result(Ps, [_Minimized,{{?TAG,enabled,M},_},_Mode,true|Res0], 
		 M, Modulators) -> 
    %% Delete
    {_,Res} = split_list(Res0, 18),
    modulator_result(Ps, Res, M+1, Modulators);
modulator_result(Ps, [Minimized,{{?TAG,enabled,M},Enabled},Mode,false|Res0], 
		 M, Modulators) ->
    {Modulator,Res} = modulator(Minimized, Enabled, Mode, Res0, M),
    modulator_result(Ps, Res, M+1, [Modulator|Modulators]).

modulator(Minimized, Enabled, Mode, Res0, M) ->
    {Res1,Res} = split_list(Res0, 18),
    TypeTag = {?TAG,type,M},
    {value,{TypeTag,Type}} = lists:keysearch(TypeTag, 1, Res1),
    [SizeX,SizeY,SizeZ,
     Diffuse,Specular,Ambient,Shininess,Normal,
     Filename,
     Color1,Color2,Depth,Hard,
     Turbulence,Sharpness,
     RingscaleX,RingscaleZ] %% 17 values = 18-1
	= lists:keydelete(TypeTag, 1, Res1),
    Ps = [{minimized,Minimized},{enabled,Enabled},{mode,Mode},
	  {size_x,SizeX},{size_y,SizeY},{size_z,SizeZ},
	  {diffuse,Diffuse},{specular,Specular},{ambient,Ambient},
	  {shininess,Shininess},{normal,Normal},
	  {type,Type},
	  {filename,Filename},{color1,Color1},{color2,Color2},{depth,Depth},
	  {hard,Hard},{turbulence,Turbulence},{sharpness,Sharpness},
	  {ringscale_x,RingscaleX},{ringscale_z,RingscaleZ}],
    {{modulator,Ps},Res}.



%%
%% Block shaders framework
%%

%% Hook for the edit button in the enclosing dialog
%%
block_shader_hook(TypeVar, DropVar, Mat) ->
    {hook,
     fun (update, {_Var,_I,_Val,Sto}) ->
	     Block = gb_trees:get(DropVar, Sto),
%%% 	     io:format(?MODULE_STRING":~w Enter~n~p~n", [?LINE,Block]),
	     Qs = bs_qs(Block, Mat, Sto),
	     Fun = bs_fun(Block, Mat, Sto, wings_wm:this()),
	     wings_ask:dialog(?__(1,"Block Shader"), Qs, Fun);
	 (is_minimized, {_Var,_I,Sto}) ->
	     gb_trees:get(TypeVar, Sto) =/= block;
	 (_, _) -> void
    end}.

-record(bs_dialog, {clipboard,parent_store,material,ps=[]}).
-record(bs_result, {clipboard,parent_store,result,ps=[]}).
-record(bs_export, {material,export_dir="",
		    ps=[],dest,base_name="",name="",n=0}).

%% Returns the dialog query term for the block shader editor
%%
bs_qs(Block, Mat, ParentStore) ->
    BsQs = case catch bs(#bs_dialog{parent_store=ParentStore,
				    clipboard=undefined,
				    material=Mat},
			 [Block,?__(1,"Block Shader"),[no_clipboard]]) of
	       {'EXIT',_} = Exit ->
		   io:format(?MODULE_STRING":~w 'EXIT'~n~p~n~p~n", 
			     [?LINE,Block,Exit]),
		   panel;
	       BQ -> BQ
	   end,
    {hframe,[BsQs,{vframe,[{button,?__(2,"OK"),done,[ok]},
			   {button,cancel,[cancel]}]}]}.

%% Returns the dialog return fun for the block shader editor
%%
bs_fun(Block, Mat, ParentStore, Parent) ->
    fun (Result) ->
%%% 	    io:format(?MODULE_STRING":~w ~p~n", 
%%% 		      [?LINE,Result]),
	    case catch bs(#bs_result{result=Result,parent_store=ParentStore},
			    [bs_fun]) of
		#bs_result{ps=[{bs_fun,NewBlock}],
			   result=[true]} -> % OK
%%% 		    io:format(?MODULE_STRING":~w OK~n~p~n~p~n", 
%%% 			      [?LINE,Block,NewBlock]),
		    wpa:drop(Parent, NewBlock);
		#bs_result{ps=[{bs_fun,NewBlock}],
			   result=[false]} -> % Loop
%%% 		    io:format(?MODULE_STRING":~w Loop~n~p~n~p~n", 
%%% 			      [?LINE,Block,NewBlock]),
		    {dialog,
		     bs_qs(NewBlock, Mat, ParentStore),
		     bs_fun(NewBlock, Mat, ParentStore, Parent)};
		Other ->
		    io:format(?MODULE_STRING":~w Other~n~p~n~p~n", 
			      [?LINE,Block,Other]),
		    {dialog,
		     bs_qs(Block, Mat, ParentStore),
		     bs_fun(Block, Mat, ParentStore, Parent)}
	    end
    end.

%% Entry point for exporting a block shader tree
%%
export_block_shader(F, {Type,Ps}, Name, Mat, ExportDir) ->
    bs_dispatch(Type,
		#bs_export{material=Mat,ps=Ps,dest=F,export_dir=ExportDir,
			   base_name=Name,name=Name,n=0}).


%% Convenient shorthand for calling bs/1,2 with a block shader
%% from a property list
%%
bs_prop(Op, PropSpec) ->
    bs_prop(Op, PropSpec, []).
%%
bs_prop(Op, {Tag,Ps}, Args) ->
    bs(Op, [proplists:get_value(Tag, Ps)|Args]);
bs_prop(Op, {Tag,Ps,Def}, Args) ->
    bs(Op, [proplists:get_value(Tag, Ps, Def)|Args]).

%%
%% Block Shader Dialog
%%
bs(#bs_dialog{clipboard=undefined}=Op, Args) ->
    bs(Op#bs_dialog{clipboard={undefined,[]}}, Args);
bs(#bs_dialog{}=Op, [undefined|Args]) ->
    bs(Op, [{undefined,[]}|Args]);
bs(#bs_dialog{clipboard={TypeCB,_}}=Op, [{Type,Ps}=Default,Title0,Spec]) ->
    Menu0 =
	case proplists:get_bool(no_clipboard, Spec) of
	    true -> [];
	    false -> [{?__(1,"[STO]"),store},{?__(2,"[RCL]"),recall},{?__(3,"[XCHG]"),exchange}]
	end,
    Menu1 =
	case proplists:get_bool(float, Spec) of
	    true -> bs_menu(float, Menu0);
	    false -> Menu0
	end,
    Menu2 =
	case proplists:get_bool(color, Spec) of
	    true -> bs_menu(color, Menu1);
	    false -> Menu1
	end,
    Menu3 =
	case proplists:get_bool(top, Spec) of
	    true -> bs_menu(top, Menu2);
	    false -> Menu2
	end,
    Menu =
	case proplists:get_bool(undefined, Spec) of
	    true -> [{?__(4,"void"),undefined}|Menu3];
	    false -> Menu3
	end,
    Title = case Title0 of
		"" ->
		    {value,{T,Type}} = lists:keysearch(Type, 2, Menu),
		    T;
		_ -> Title0
	    end,
    case lists:delete(no_clipboard, Spec) of
	[] ->
	    {hframe,[{value,false},
		     {value,Default},
		     {value,Type},
		     bs_dispatch(Type, Op#bs_dialog{ps=Ps})]};
	[top] ->
	    Minimized = proplists:get_value(minimized, Ps, false),
	    {hframe,[{value,Default},
		     {value,Type},
		     bs_dispatch(Type, Op#bs_dialog{ps=Ps})],
	     [{minimized,Minimized},{title,Title}]};
	_ ->
	    Compatible = lists:keymember(TypeCB, 2, Menu),
	    Minimized = proplists:get_value(minimized, Ps, true),
	    HookFun =
		fun (update, {Var,_I,Val,Sto}) ->
			{done,gb_trees:update(Var, Val, Sto)};
		    (menu_disabled, {_Var,_I,_Sto}) when not Compatible ->
			[recall,exchange];
		    (_, _) -> void
		end,
	    {hframe,[{value,Default},
		     {menu,Menu,Type,[{hook,HookFun}]},
		     bs_dispatch(Type, Op#bs_dialog{ps=Ps})],
	     [{minimized,Minimized},{title,Title}]}
    end;
%%
%% Block Shader Result
%%
bs(#bs_result{clipboard=undefined}=Op, Args) ->
    bs(Op#bs_result{clipboard={undefined,[]}}, Args);
bs(#bs_result{result=[Minimized,{OldType,_},NewType|R],ps=ParentPs}=Op0,
   [Tag|DefaultPs]) ->
    #bs_result{result=Rest,clipboard={TypeCB,PsCB}=CB,ps=Ps} = Op =
	bs_dispatch(OldType, Op0#bs_result{result=R,ps=[]}),
    case NewType of
	store ->
	    NewBlock = {OldType,[{minimized,Minimized}|Ps]},
	    NewCB = 
		{OldType,
		 [{minimized,proplists:get_value(minimized, PsCB, true)}|Ps]},
%%% 	    io:format(?MODULE_STRING":~w store~n~p~n~p~n~p~n", 
%%% 		      [?LINE,CB,NewBlock,NewCB]),
	    Op#bs_result{ps=[{Tag,NewBlock}|ParentPs],
			     result=Rest,
			     clipboard=NewCB};
	recall ->
	    NewBlock = {TypeCB,[{minimized,Minimized}
				|proplists:delete(minimized, PsCB)]},
	    Op#bs_result{ps=[{Tag,NewBlock}|ParentPs],
			     result=Rest,
			     clipboard=CB};
	exchange ->
	    NewBlock = {TypeCB,[{minimized,Minimized}
				|proplists:delete(minimized, PsCB)]},
	    NewCB = 
		{OldType,
		 [{minimized,proplists:get_value(minimized, PsCB, true)}|Ps]},
	    Op#bs_result{ps=[{Tag,NewBlock}|ParentPs],
			     result=Rest,
			     clipboard=NewCB};
	OldType -> % Content change
	    NewBlock = {OldType,[{minimized,Minimized}|Ps]},
	    Op#bs_result{ps=[{Tag,NewBlock}|ParentPs],
			     result=Rest,
			     clipboard=CB};
	_ -> % New type - should maybe convert old Ps to new block type
	    NewBlock = {NewType,[{minimized,Minimized}|DefaultPs]},
	    Op#bs_result{ps=[{Tag,NewBlock}|ParentPs],
			     result=Rest,
			     clipboard=CB}
    end;
%%
%% Block Shader Export
%%
bs(#bs_export{}=Op, [undefined]) ->
    bs(Op, [{undefined,[]}]);
bs(#bs_export{base_name=BaseName,n=M}=Op, [{Type,Ps}]) ->
    N = M+1,
    bs_dispatch(Type, 
		Op#bs_export{ps=Ps,
			     name=BaseName++"_"++integer_to_list(N),
			     n=N}).

bs_dispatch(Type, Op) ->
    case Type of
	%% Internal virtual block shaders
	undefined    -> bs_undefined(Op);
	block_shader -> bs_block_shader(Op);
	%% Top level block shader
	phong        -> bs_phong(Op);
	%% Color output block shaders
	float2color  -> bs_float2color(Op);
	rgb          -> bs_rgb(Op);
	sss          -> bs_sss(Op);
	hsv          -> bs_hsv(Op);
	image        -> bs_image(Op);
	mix          -> bs_mix(Op);
	fresnel      -> bs_fresnel(Op);
	conetrace    -> bs_conetrace(Op);
	gobo         -> bs_gobo(Op);
	colorband    -> bs_colorband(Op);
	clouds       -> bs_clouds(Op);
	marble       -> bs_marble(Op);
	wood         -> bs_wood(Op);
	%% Float output block shaders
	color2float  -> bs_color2float(Op);
	coords       -> bs_coords(Op);
	mul          -> bs_mul(Op);
 	sin          -> bs_sin(Op)
    end.

bs_menu(top, L) ->
    [{?__(1,"Phong"),phong}|L];
bs_menu(color, L) ->
    [{?__(2,"(color)"),float2color},
     {?__(3,"RGB"),rgb},
     {?__(4,"HSV"),hsv},
     {?__(5,"SSS"),sss},
     {?__(6,"Image"),image},
     {?__(7,"Mix"),mix},
     {?__(8,"Fresnel"),fresnel},
     {?__(9,"ConeTr"),conetrace},
     {?__(10,"Gobo"),gobo},
     {?__(11,"ColBand"),colorband},
     {?__(12,"Clouds"),clouds},
     {?__(13,"Marble"),marble},
     {?__(14,"Wood"),wood}|L];
bs_menu(float, L) ->
    [{?__(15,"(float)"),color2float},
     {?__(16,"Coords"),coords},
     {?__(17,"Mul"),mul},
     {?__(18,"Sin"),sin}|L].

bs_print_tag(_F, _Tag, undefined) -> ok;
bs_print_tag(F, Tag, Value) ->
    print(F, "~s=\"\~s\" ", [format(Tag),Value]).

bs_print_attr(_F, _Attr, undefined) -> ok;
bs_print_attr(F, Attr, Value) ->
    println(F, "        <~s value=\"\~s\"/>", [format(Attr),Value]).

%%%
%%% Block shaders
%%%

%% Internal virtual block shaders
%%

bs_undefined(#bs_dialog{}) ->
    {vframe,[]};
bs_undefined(#bs_result{}=Op) ->
    Op;
bs_undefined(#bs_export{}=Op) ->
    Op#bs_export{name=undefined}.


%% The toplevel block shader. Does dirty tricks with the clipboard.
%%
bs_block_shader(#bs_dialog{ps=Ps}=Op) ->
    CB = proplists:get_value(clipboard, Ps),
    {vframe,[{value,CB},
	     bs_prop(Op#bs_dialog{clipboard=CB}, 
		     {shader,Ps,?DEF_BS_TOP}, ["",[top,no_clipboard]]),
	     {vframe,
	      [bs(Op#bs_dialog{clipboard=CB}, 
		  [CB,?__(1,"[MEM]"),[float,color,undefined,no_clipboard]])],
	      [{hook,fun (is_disabled, {_Var,_I,_Sto}) -> true;
			 (_, _) -> void end}]}]};
bs_block_shader(#bs_result{result=[CB0|R]}=Op0) ->
    Op1 = #bs_result{ps=Ps,clipboard=CB} =
	bs(Op0#bs_result{clipboard=CB0,result=R}, [shader]),
    Op2 = #bs_result{result=Rest} = bs(Op1, [clipboard]),
%%%     io:format(?MODULE_STRING":~w result~n~p~n~p~n", 
%%% 	      [?LINE,CB0,CB]),
    Op2#bs_result{ps=[{clipboard,CB}|Ps],result=Rest};
bs_block_shader(#bs_export{ps=Ps}=Op0) ->
    Op = bs_prop(Op0, {shader,Ps,?DEF_BS_TOP}),
    Op#bs_export{name=undefined}.

%% Top level block shaders
%%

bs_phong(#bs_dialog{ps=Ps,parent_store=Sto}=Op) ->
%%%     io:format(?MODULE_STRING":~w bs_phong::dialog~n~p~n", 
%%% 	      [?LINE,Ps]),
    DefDiffuse = gb_trees:get(diffuse, Sto),
    DefAmbient = gb_trees:get(ambient, Sto),
    DefSpecular = gb_trees:get(specular, Sto),
    {vframe,[bs_prop(Op, {diffuse,Ps,?DEF_BSCOL(DefDiffuse)},
		     [?__(1,"Diffuse"),[color]]),
	     bs_prop(Op, {ambient,Ps,?DEF_BSCOL(DefAmbient)},
		     [?__(2,"Ambient"),[color]]),
	     bs_prop(Op, {specular,Ps,?DEF_BSCOL(DefSpecular)},
		     [?__(3,"Specular"),[color]])]};
bs_phong(#bs_result{parent_store=Sto}=Op0) ->
    DefDiffuse = gb_trees:get(diffuse, Sto),
    DefAmbient = gb_trees:get(ambient, Sto),
    DefSpecular = gb_trees:get(specular, Sto),
    Op1 = bs(Op0, [diffuse,{color,DefDiffuse}]),
    Op2 = bs(Op1, [ambient,{color,DefAmbient}]),
    bs(Op2, [specular,{color,DefSpecular}]);
bs_phong(#bs_export{ps=Ps,dest=F,base_name=Name,material=Mat}=Op0) ->
    OpenGL = proplists:get_value(opengl, Mat, []),
    YafRay = proplists:get_value(?TAG, Mat, []),
    DefDiffuse = alpha(proplists:get_value(diffuse, OpenGL)),
    DefAmbient = alpha(proplists:get_value(ambient, OpenGL)),
    DefSpecular = alpha(proplists:get_value(specular, OpenGL)),
    Op1 = #bs_export{name=Diffuse} = 
	bs_prop(Op0, {diffuse,Ps,?DEF_BSCOL(DefDiffuse)}),
    Op2 = #bs_export{name=Specular} = 
	bs_prop(Op1, {specular,Ps,?DEF_BSCOL(DefSpecular)}),
    Op = #bs_export{name=Ambient} = 
	bs_prop(Op2, {ambient,Ps,?DEF_BSCOL(DefAmbient)}),
    Shininess = proplists:get_value(shininess, OpenGL),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    println(F, "<shader type=\"phong\" name=\"~s\">~n"++
	    "    <attributes>", [Name]),
    println(F, "        <color value=\"~s\"/>", [Diffuse]),
    println(F, "        <specular value=\"~s\"/>", [Specular]),
    println(F, "        <environment value=\"~s\"/>", [Ambient]),
    println(F, "        <hard value=\"~.10f\"/>", [Shininess*128.0]),
    println(F, "        <IOR value=\"~.10f\"/>", [IOR]),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

%% Color output block shaders
%%

bs_float2color(#bs_dialog{ps=Ps}=Op) ->
    bs_prop(Op, {input,Ps,?DEF_BS_FLOAT}, [?__(1,"Input"),[float]]);
bs_float2color(#bs_result{}=Op) ->
    bs(Op, [input]);
bs_float2color(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op = #bs_export{name=Input} = bs_prop(Op0, {input,Ps,?DEF_BS_FLOAT}),
    println(F, "<shader type=\"float2color\" name=\"~s\" input=\"~s\">~n"++
 	    "    <attributes>", [Name,Input]),
    println(F, "    </attributes>~n"++
 	    "</shader>~n", []),
    Op#bs_export{name=Name}.

bs_rgb(#bs_dialog{ps=Ps}=Op) ->
%%%     io:format(?MODULE_STRING":~w bs_rgb::dialog~n~p~n", 
%%% 	      [?LINE,Ps]),
    Color = proplists:get_value(color, Ps, ?DEF_DIFFUSE),
    {vframe,
     [bs_prop(Op, {input_r,Ps}, ["R",[float,undefined]]),
      bs_prop(Op, {input_g,Ps}, ["G",[float,undefined]]),
      bs_prop(Op, {input_b,Ps}, ["B",[float,undefined]]),
      {hframe,[{label,?__(1,"Default Color")},{color,Color}]}]};
bs_rgb(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_r]),
    Op2 = bs(Op1, [input_g]),
    Op3 = #bs_result{result=[Color|Rest],ps=Ps} = bs(Op2, [input_b]),
    Op3#bs_result{ps=[{color,Color}|Ps],result=Rest};
bs_rgb(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op1 = #bs_export{name=InputR} = bs_prop(Op0, {input_r,Ps}),
    Op2 = #bs_export{name=InputG} = bs_prop(Op1, {input_g,Ps}),
    Op = #bs_export{name=InputB} = bs_prop(Op2, {input_b,Ps}),
    Color = proplists:get_value(color, Ps, ?DEF_DIFFUSE),
    print(F, "<shader type=\"RGB\" name=\"~s\"", [Name]),
    if InputR =:= undefined, InputG =:= undefined, InputB =:= undefined ->
	    println(F, ">");
       true ->
	    print(F, "~n        ", []),
	    bs_print_tag(F, inputred, InputR),
	    bs_print_tag(F, inputgreen, InputG),
	    bs_print_tag(F, inputblue, InputB),
	    println(F, ">")
    end,
    println(F, "    <attributes>"),
    export_rgb(F, color, Color),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

bs_sss(#bs_dialog{ps=Ps}) ->
%%%     io:format(?MODULE_STRING":~w bs_sss::dialog~n~p~n", 
%%% 	      [?LINE,Ps]),
    Color = proplists:get_value(color, Ps, ?DEF_BS_SSS_COLOR),
    Radius = proplists:get_value(sss_radius, Ps, ?DEF_BS_SSS_RADIUS),
    Samples = proplists:get_value(sss_samples, Ps, ?DEF_BS_SSS_SAMPLES),
    {vframe,
     [{label_column,
       [{?__(1,"Color"),{color,Color}},
	{?__(2,"Radius"),{text,Radius,[range(sss_radius)]}},
	{?__(3,"Samples"),{text,Samples,[range(sss_samples)]}}]}]};
bs_sss(#bs_result{result=[Color,Radius,Samples|Rest],ps=Ps}=Op) ->
    Op#bs_result{
      result=Rest,
      ps=[{color,Color},{sss_radius,Radius},{sss_samples,Samples}|Ps]};
bs_sss(#bs_export{ps=Ps,dest=F,name=Name}=Op) ->
    Color = proplists:get_value(color, Ps, ?DEF_BS_SSS_COLOR),
    Radius = proplists:get_value(sss_radius, Ps, ?DEF_BS_SSS_RADIUS),
    Samples = proplists:get_value(sss_samples, Ps, ?DEF_BS_SSS_SAMPLES),
    println(F, 
	    "<shader type=\"sss\" name=\"~s\">~n"
	    "    <attributes>", [Name]),
    export_rgb(F, color, Color),
    println(F, 
	    "        <radius value=\"~.10f\"/>~n"
	    "        <samples value=\"~w\"/>~n"
	    "    </attributes>~n"
	    "</shader>~n", [Radius,Samples]),
    Op#bs_export{name=Name}.

bs_hsv(#bs_dialog{ps=Ps}=Op) ->
    Color = proplists:get_value(color, Ps, ?DEF_DIFFUSE),
    {vframe,
     [bs_prop(Op, {input_h,Ps}, ["H",[float,undefined]]),
      bs_prop(Op, {input_s,Ps}, ["S",[float,undefined]]),
      bs_prop(Op, {input_v,Ps}, ["V",[float,undefined]]),
      {hframe,[{label,?__(1,"Default Color")},{color,Color}]}]};
bs_hsv(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_h]),
    Op2 = bs(Op1, [input_s]),
    Op3 = #bs_result{result=[Color|Rest],ps=Ps} = bs(Op2, [input_v]),
    Op3#bs_result{ps=[{color,Color}|Ps],result=Rest};
bs_hsv(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op1 = #bs_export{name=InputH} = bs_prop(Op0, {input_h,Ps}),
    Op2 = #bs_export{name=InputS} = bs_prop(Op1, {input_s,Ps}),
    Op = #bs_export{name=InputV} = bs_prop(Op2, {input_v,Ps}),
    Color = proplists:get_value(color, Ps, ?DEF_DIFFUSE),
    {H,S,V} = wings_ask:rgb_to_hsv(Color),
    println(F, "<shader type=\"HSV\" name=\"~s\"", [Name]),
    if InputH =:= undefined, InputS =:= undefined, InputV =:= undefined ->
	    ok;
       true ->
	    print(F, "        "),
	    bs_print_tag(F, inputhue, InputH),
	    bs_print_tag(F, inputsaturation, InputS),
	    bs_print_tag(F, inputvalue, InputV),
	    println(F)
    end,
    println(F, "        hue=\"~s\" saturation=\"~s\" value=\"~s\">~n"
	    "    <attributes>~n"
	    "    </attributes>~n"
	    "</shader>~n",
	    [format(H/360),format(S),format(V)]),
    Op#bs_export{name=Name}.

bs_image(#bs_dialog{ps=Ps,material=Mat}) ->
    Types = [image|[{map,M1} || {M1,_} <- proplists:get_value(maps, Mat, [])]],
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type,open_dialog},
		   {extensions,[{".jpg",?__(1,"JPEG compressed image")},
				{".tga",?__(2,"Targa bitmap")}]}],
    NofTypes = length(Types),
    TypesFrame =
	case Types of
	    [image] -> [{value,image}];
	    _ ->
		T1 = proplists:get_value(type, Ps, image), 
		Type = case lists:member(T1, Types) of
			   true -> T1;
			   false -> image
		       end,
		[{hradio,
		  [case T2 of
		       {map,M2} -> {atom_to_list(M2),T2};
		       _ -> {wings_util:cap(T2),T2}
		   end || T2 <- Types],
		  Type}]
	end,
    {vframe,
     TypesFrame++
     [{hframe,
       [{label,?__(3,"Filename")},
	{button,{text,Filename,[{width,15},{props,BrowseProps}]}}],
       [hook(enable, [member,-NofTypes,image])]}]};
bs_image(#bs_result{result=[Type,Filename|Rest],ps=Ps}=Op) ->
    Op#bs_result{result=Rest,ps=[{type,Type},{filename,Filename}|Ps]};
bs_image(#bs_export{ps=Ps,dest=F,name=Name,
		    material=Mat,export_dir=ExportDir}=Op) ->
    Maps = proplists:get_value(maps, Mat, []),
    Filename =
	case proplists:get_value(type, Ps, image) of
	    image -> 
		proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME);
	    {map,Map} ->
		case proplists:get_value(Map, Maps, undefined) of
		    undefined ->
			exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
		    #e3d_image{name=ImageName}=Image ->
			MapFile = ImageName++".tga",
			ok = e3d_image:save(Image, 
					    filename:join(ExportDir, MapFile)),
			MapFile
		end
	end,
    println(F, "<shader type=\"image\" name=\"~s\">~n"++
	    "    <attributes>~n"
	    "        <filename value=\"~s\"/>~n"
	    "    </attributes>~n"
	    "</shader>~n", [Name,Filename]),
    Op.

bs_mix(#bs_dialog{ps=Ps}=Op) ->
    Mode = proplists:get_value(mode, Ps, ?DEF_BS_MIX_MODE),
    {vframe,
     [{menu,
       [{wings_util:cap(Tag),Tag} ||
	   Tag <- [add,average,colorburn,colordodge,darken,
		   difference,exclusion,freeze,hardlight,lighten,
		   multiply,negation,overlay,reflect,screen,
		   softlight,stamp,subtract]],
       Mode},
      bs_prop(Op, {input_1,Ps,?DEF_BS_COLOR}, [?__(1,"Input 1"),[color]]),
      bs_prop(Op, {input_2,Ps,?DEF_BS_COLOR}, [?__(2,"Input 2"),[color]])]};
bs_mix(#bs_result{result=[Mode|Rest],ps=Ps}=Op0) ->
    Op1 = bs(Op0#bs_result{result=Rest,ps=[{mode,Mode}|Ps]}, [input_1]),
    bs(Op1, [input_2]);
bs_mix(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op1 = #bs_export{name=Input1} = bs_prop(Op0, {input_1,Ps,?DEF_BS_COLOR}), 
    Op2 = #bs_export{name=Input2} = bs_prop(Op1, {input_2,Ps,?DEF_BS_COLOR}), 
    Mode = proplists:get_value(mode, Ps, ?DEF_BS_MIX_MODE),
    println(F, "<shader type=\"mix\" name=\"~s\"~n"
	    "        input1=\"~s\" input2=\"~s\" mode=\"~s\" >~n"++
	    "    <attributes>", [Name,Input1,Input2,format(Mode)]),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op2#bs_export{name=Name}.

bs_fresnel(#bs_dialog{ps=Ps,parent_store=Sto}=Op) ->
    DefReflected = gb_trees:get(?KEY(reflected), Sto),
    DefTransmitted = gb_trees:get(?KEY(transmitted), Sto),
    {vframe,
     [bs_prop(Op, {reflected,Ps,
		   {conetrace,[{mode,reflect},{color,DefReflected}]}}, 
	      [?__(1,"Reflected"),[color]]),
      bs_prop(Op, {transmitted,Ps,
		   {conetrace,[{mode,refract},{color,DefTransmitted}]}}, 
	      [?__(2,"Transmitted"),[color]])]};
bs_fresnel(#bs_result{parent_store=Sto}=Op0) ->
    DefReflected = gb_trees:get(?KEY(reflected), Sto),
    DefTransmitted = gb_trees:get(?KEY(transmitted), Sto),
    Op1 = bs(Op0, [reflected,{mode,reflect},{color,DefReflected}]),
    bs(Op1, [transmitted,{mode,refract},{color,DefTransmitted}]);
bs_fresnel(#bs_export{ps=Ps,dest=F,name=Name,material=Mat}=Op0) ->
    OpenGL = proplists:get_value(opengl, Mat),
    DefReflected0 = alpha(proplists:get_value(specular, OpenGL)),
    DefTransmitted0 = def_transmitted(proplists:get_value(diffuse, OpenGL)),
    YafRay = proplists:get_value(?TAG, Mat, []),
    DefReflected = proplists:get_value(reflected, YafRay, DefReflected0),
    DefTransmitted = proplists:get_value(transmitted, YafRay, DefTransmitted0),
    Op1 = #bs_export{name=Reflected} = 
	bs_prop(Op0, {reflected,Ps,
		      {conetrace,[{mode,reflect},
				  {color,DefReflected}]}}),
    Op = #bs_export{name=Transmitted} = 
	bs_prop(Op1, {transmitted,Ps,
		      {conetrace,[{mode,refract},
				  {color,DefTransmitted}]}}),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    println(F, "<shader type=\"fresnel\" name=\"~s\"~n"
	    "        reflected=\"~s\" transmitted=\"~s\"~n"
	    "        IOR=\"\~.10f\" min_refle=\"~.10f\" >~n"++
	    "    <attributes>", [Name,Reflected,Transmitted,IOR,MinRefle]),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

bs_conetrace(#bs_dialog{ps=Ps}) ->
    Mode = proplists:get_value(mode, Ps, ?DEF_BS_CONE_MODE),
    Angle = proplists:get_value(angle, Ps, ?DEF_BS_CONE_ANGLE),
    Samples = proplists:get_value(samples, Ps, ?DEF_BS_CONE_SAMPLES),
    Color = proplists:get_value(color, Ps, ?DEF_BS_CONE_COLOR),
    {vframe,
     [{menu,[{?__(1,"Reflect"),reflect},{?__(2,"Refract"),refract}],Mode},
      {hframe,
       [{vframe,[{label,?__(3,"Angle")},
		 {label,?__(4,"Samples")}]},
	{vframe,[{text,Angle,[range(cone_angle)]},
		 {text,Samples,[range(samples)]}]},
	{vframe,[{slider,[range(cone_angle),{key,-3}]},
		 {hframe,[{label,?__(5,"Color")},{color,Color}]}]}]}]};
bs_conetrace(#bs_result{result=[Mode,Angle,Samples,Color|Rest],ps=Ps}=Op) ->
    Op#bs_result{result=Rest,
		 ps=[{mode,Mode},{angle,Angle},
		     {samples,Samples},{color,Color}|Ps]};
bs_conetrace(#bs_export{ps=Ps,dest=F,name=Name,material=Mat}=Op) ->
    Reflect = (proplists:get_value(mode, Ps, ?DEF_BS_CONE_MODE) =:= reflect),
    println(F, "<shader type=\"conetrace\" name=\"~s\" reflect=\"~s\"",
	    [Name,format(Reflect)]),
    Angle = proplists:get_value(angle, Ps, ?DEF_BS_CONE_ANGLE),
    Samples = proplists:get_value(samples, Ps, ?DEF_BS_CONE_SAMPLES),
    print(F, "angle=\"\~.3f\" samples=\"~w\"", [Angle,Samples]),
    OpenGL = proplists:get_value(opengl, Mat),
    YafRay = proplists:get_value(?TAG, Mat, []),
    DefColor =
	if Reflect ->
		println(F, ">"),
		DefReflected = alpha(proplists:get_value(specular, OpenGL)),
		proplists:get_value(reflected, YafRay, DefReflected);
	   true ->
		IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
		println(F, " IOR=\"~.10f\">", [IOR]),
		DefTransmitted = def_transmitted(
				   proplists:get_value(diffuse, OpenGL)),
		proplists:get_value(transmitted, YafRay, DefTransmitted)
	end,
    println(F, "    <attributes>"),
    export_rgb(F, color, proplists:get_value(color, Ps, DefColor)),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op.

bs_gobo(#bs_dialog{ps=Ps}=Op) ->
    Hardedge = proplists:get_value(hardedge, Ps, ?DEF_BS_GOBO_HARD),
    Edgeval = proplists:get_value(edgeval, Ps, ?DEF_BS_GOBO_EDGE),
    {vframe,
     [bs_prop(Op, {input_1,Ps,?DEF_BS_COLOR}, [?__(1,"Input 1"),[color]]),
      bs_prop(Op, {input_2,Ps,?DEF_BS_COLOR}, [?__(2,"Input 2"),[color]]),
      bs_prop(Op, {gobo,Ps,{image,[]}}, [?__(3,"Gobo"),[color,float]]),
      {hframe,[{?__(4,"Hard Edge Val"),Hardedge},
	       {text,Edgeval,[hook(enable, -1)]}]}]};
bs_gobo(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_1]),
    Op2 = bs(Op1, [input_2]),
    Op = #bs_result{result=[Hardedge,Edgeval|Rest],ps=Ps} = bs(Op2, [gobo]),
    Op#bs_result{result=Rest,ps=[{hardedge,Hardedge},{edgeval,Edgeval}|Ps]};
bs_gobo(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    {GoboType,_} = GoboBlock = proplists:get_value(gobo, Ps, {image,[]}),
    Op1 = #bs_export{name=Input1} = bs_prop(Op0, {input_1,Ps,?DEF_BS_COLOR}),
    Op2 = #bs_export{name=Input2} = bs_prop(Op1, {input_2,Ps,?DEF_BS_COLOR}),
    Op = #bs_export{name=Gobo} = bs(Op2, [GoboBlock]),
    Hardedge = proplists:get_value(hardedge, Ps, ?DEF_BS_GOBO_HARD),
    Edgeval = proplists:get_value(edgeval, Ps, ?DEF_BS_GOBO_EDGE),
    println(F, "<shader type=\"gobo\" name=\"~s\">~n"
	    "    <attributes>~n"
	    "        <input1 value=\"~s\"/>~n"
	    "        <input2 value=\"~s\"/>",
	    [Name,Input1,Input2]),
    case lists:keysearch(GoboType, 2, bs_menu(float, [])) of
	{value,_} ->
	    println(F, "        <goboFloat value=\"~s\"/>", [Gobo]);
	false ->
	    println(F, "        <goboColor value=\"~s\"/>", [Gobo])    end,
    println(F, "        <hardedge value=\"~s\"/>", [format(Hardedge)]),
    if Hardedge ->
	    println(F, "        <edgeval value=\"~.10f\"/>", [Edgeval]);
       true -> ok
    end,
    println(F, "    </attributes>~n"
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

bs_colorband(#bs_dialog{ps=Ps}=Op) ->
    Bands = proplists:get_value(bands, Ps, 
				[[{value,0.0},{color,?DEF_DIFFUSE}]]),
    {vframe,
     [bs_prop(Op, {input,Ps,?DEF_BS_FLOAT}, [?__(1,"Input"),[float]]),
      {hframe,bs_colorband_dialog(Bands)}]};
bs_colorband(#bs_result{}=Op0) ->
    Op = #bs_result{result=Result,ps=Ps}= bs(Op0, [input]),
    case bs_colorband_result(Result) of
	{[true|Rest],Bands} -> % Sort
	    Op#bs_result{result=Rest,ps=[{bands,lists:sort(Bands)}|Ps]};
	{[false|Rest],Bands} ->
	    Op#bs_result{result=Rest,ps=[{bands,Bands}|Ps]}
    end;
bs_colorband(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op = #bs_export{name=Input} = bs_prop(Op0, {input,Ps,?DEF_BS_FLOAT}), 
    Bands = proplists:get_value(bands, Ps, 
				[[{value,0.0},{color,?DEF_DIFFUSE}]]),
    println(F, "<shader type=\"colorband\" name=\"~s\">~n"
	    "    <attributes>~n"
	    "        <input value=\"~s\"/>~n"
	    "    </attributes>", [Name,Input]),
    lists:foreach(
      fun (Band) ->
	      Value = proplists:get_value(value, Band),
	      Color = proplists:get_value(color, Band),
	      println(F, "    <modulator value=\"~.10f\">", [Value]),
	      export_rgb(F, color, Color),
	      println(F, "    </modulator>")
      end,
      lists:sort(Bands)),
    println(F, "</shader>~n", []),
    Op#bs_export{name=Name}.


bs_colorband_dialog([_]=Bands) ->
    DelButtonOpts =
	[{hook,
	  fun (is_disabled, _) -> true;
	      (_, _) -> void
	  end}],
    bs_colorband_dialog_1(Bands, DelButtonOpts);
bs_colorband_dialog(Bands) ->
    bs_colorband_dialog_1(Bands, []).

bs_colorband_dialog_1([Band|Bands], DelButtonOpts) ->
    Value = proplists:get_value(value, Band, 0.0),
    Color = proplists:get_value(color, Band, ?DEF_DIFFUSE),
    [{vframe,[{text,Value,[{width,4}]},{color,Color},
	      {button,?__(1,"Del"),done,DelButtonOpts}]}|
     case Bands of
	 [] ->
	     [{vframe,[{button,?__(2,"New"),done},{button,?__(3,"Sort"),done}]}];
	 _ ->
	     bs_colorband_dialog_1(Bands, DelButtonOpts)
     end].

bs_colorband_result([_Value,_Color,true,false|Rest]) -> % Del
    {Rest,[]};
bs_colorband_result([Value,Color,false|Rest0]) ->
    Band = [{value,Value},{color,Color}],
    case Rest0 of
	[true|Rest] -> % New
	    {Rest,[Band,Band]};
	[false|Rest] ->
	    {Rest,[Band]};
	_ ->
	    {Rest,Bands} = bs_colorband_result(Rest0),
	    {Rest,[Band|Bands]}
    end;
bs_colorband_result([_Value,_Color,true|Rest]) -> % Del
    bs_colorband_result(Rest).

bs_clouds(#bs_dialog{ps=Ps}=Op) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    {vframe,
     [bs_prop(Op, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}, 
	      [?__(1,"Input 1"),[color,undefined]]),
      bs_prop(Op, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}, 
	      [?__(2,"Input 2"),[color,undefined]]),
      {hframe,[{label,?__(3,"Size")},{text,Size,[range(size)]},
	       {label,?__(4,"Depth")},{text,Depth,[range(noise_depth)]}]}]};
bs_clouds(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_1,{color,?DEF_MOD_COLOR1}]),
    Op2 = #bs_result{result=[Size,Depth|Rest],ps=Ps} =
	bs(Op1, [input_2,{color,?DEF_MOD_COLOR2}]),
    Op2#bs_result{result=Rest,ps=[{size,Size},{depth,Depth}|Ps]};
bs_clouds(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Op1 = #bs_export{name=Input1} = 
	bs_prop(Op0, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}), 
    Op2 = #bs_export{name=Input2} = 
	bs_prop(Op1, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}), 
    println(F, "<shader type=\"clouds\" name=\"~s\"~n"
	    "        size=\"~.3f\" depth=\"~w\" >~n"++
	    "    <attributes>", [Name,Size,Depth]),
    %% Input swap to compensate for inconsistency in YafRay
    %% shader block versus procedural texture
    bs_print_attr(F, input2, Input1),
    bs_print_attr(F, input1, Input2),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op2#bs_export{name=Name}.

bs_marble(#bs_dialog{ps=Ps}=Op) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    {vframe,
     [bs_prop(Op, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}, 
	      [?__(1,"Input 1"),[color,undefined]]),
      bs_prop(Op, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}, 
	      [?__(2,"Input 2"),[color,undefined]]),
      {hframe,[{label,?__(3,"Size")},{text,Size,[range(size)]},
	       {label,?__(4,"Depth")},{text,Depth,[range(noise_depth)]},
	       {?__(5,"Hard Noise"),Hard}]},
      {hframe,[{label,?__(6,"Turbulence")},{text,Turbulence,[range(turbulence)]},
	       {label,?__(7,"Sharpness")},{text,Sharpness,[range(sharpness)]}]}]};
bs_marble(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_1,{color,?DEF_MOD_COLOR1}]),
    Op2 = #bs_result{result=[Size,Depth,Hard,Turbulence,Sharpness|Rest],
		     ps=Ps} =
	bs(Op1, [input_2,{color,?DEF_MOD_COLOR2}]),
    Op2#bs_result{result=Rest,
		  ps=[{size,Size},{depth,Depth},{hard,Hard},
		      {turbulence,Turbulence},{sharpness,Sharpness}|Ps]};
bs_marble(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    Op1 = #bs_export{name=Input1} = 
	bs_prop(Op0, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}), 
    Op2 = #bs_export{name=Input2} = 
	bs_prop(Op1, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}), 
    println(F, "<shader type=\"marble\" name=\"~s\"~n"
	    "        size=\"~.3f\" depth=\"~w\" hard=\"~s\"~n"
	    "        turbulence=\"~.6f\" sharpness=\"~.6f\" >~n"
	    "    <attributes>", 
	    [Name,Size,Depth,format(Hard),Turbulence,Sharpness]),
    %% Input swap to compensate for inconsistency in YafRay
    %% shader block versus procedural texture
    bs_print_attr(F, input2, Input1),
    bs_print_attr(F, input1, Input2),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op2#bs_export{name=Name}.

bs_wood(#bs_dialog{ps=Ps}=Op) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    RingscaleX = proplists:get_value(ringscale_x, Ps, ?DEF_MOD_RINGSCALE_X),
    RingscaleZ = proplists:get_value(ringscale_z, Ps, ?DEF_MOD_RINGSCALE_Z),
    {vframe,
     [bs_prop(Op, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}, 
	      [?__(1,"Input 1"),[color,undefined]]),
      bs_prop(Op, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}, 
	      [?__(2,"Input 2"),[color,undefined]]),
      {hframe,[{vframe,[{label,?__(3,"Size")},
			{label,?__(4,"Turbulence")},
			{label,?__(5,"Ringscale X")}]},
	       {vframe,[{text,Size,[range(size)]},
			{text,Turbulence,[range(turbulence)]},
			{text,RingscaleX,[range(scale)]}]},
	       {vframe,[{label,?__(6,"Depth")},
			{?__(7,"Hard Noise"),Hard},
			{label,?__(8,"Ringscale Z")}]},
	       {vframe,[{text,Depth,[range(noise_depth)]},
			panel,
			{text,RingscaleZ,[range(scale)]}]}]}]};
bs_wood(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_1,{color,?DEF_MOD_COLOR1}]),
    Op2 = #bs_result{result=[Size,Turbulence,RingscaleX,
			     Hard,Depth,RingscaleZ|Rest],
		     ps=Ps} =
	bs(Op1, [input_2,{color,?DEF_MOD_COLOR2}]),
    Op2#bs_result{result=Rest,
		  ps=[{size,Size},{depth,Depth},
		      {turbulence,Turbulence},{hard,Hard},
		      {ringscale_x,RingscaleX},{ringscale_z,RingscaleZ}|Ps]};
bs_wood(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Size = proplists:get_value(size, Ps, ?DEF_MOD_SIZE),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    RingscaleX = proplists:get_value(ringscale_x, Ps, ?DEF_MOD_RINGSCALE_X),
    RingscaleZ = proplists:get_value(ringscale_z, Ps, ?DEF_MOD_RINGSCALE_Z),
    Op1 = #bs_export{name=Input1} = 
	bs_prop(Op0, {input_1,Ps,?DEF_BSCOL(?DEF_MOD_COLOR1)}), 
    Op2 = #bs_export{name=Input2} = 
	bs_prop(Op1, {input_2,Ps,?DEF_BSCOL(?DEF_MOD_COLOR2)}), 
    %% Coordinate rotation see export_pos/3
    println(F, "<shader type=\"wood\" name=\"~s\"~n"
	    "        size=\"~.3f\" depth=\"~w\"~n"
	    "        turbulence=\"~.6f\"  hard=\"~s\"~n"
	    "        ringscale_x=\"~.6f\" ringscale_y=\"~.6f\">~n"
	    "    <attributes>", 
	    [Name,Size,Depth,Turbulence,format(Hard),RingscaleZ,RingscaleX]),
    %% Input swap to compensate for inconsistency in YafRay
    %% shader block versus procedural texture
    bs_print_attr(F, input2, Input1),
    bs_print_attr(F, input1, Input2),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op2#bs_export{name=Name}.

%% Float output block shaders
%%

bs_color2float(#bs_dialog{ps=Ps}=Op) ->
    bs_prop(Op, {input,Ps,?DEF_BS_COLOR}, [?__(1,"Input"),[color]]);
bs_color2float(#bs_result{}=Op) ->
    bs(Op, [input]);
bs_color2float(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op = #bs_export{name=Input} = 
	bs_prop(Op0, {input,Ps,?DEF_BS_COLOR}),
    println(F, "<shader type=\"color2float\" name=\"~s\" input=\"~s\">~n"++
	    "    <attributes>", [Name,Input]),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

bs_coords(#bs_dialog{ps=Ps}) ->
    %% Coordinate rotation see export_pos/3
    Coord = proplists:get_value(coord, Ps, ?DEF_BS_COORD),
    {menu,[{"X",'Y'},{"Y",'Z'},{"Z",'X'}],Coord};
bs_coords(#bs_result{result=[Coord|Rest],ps=Ps}=Op) ->
    Op#bs_result{result=Rest,ps=[{coord,Coord}|Ps]};
bs_coords(#bs_export{ps=Ps,dest=F,name=Name}=Op) ->
    Coord = proplists:get_value(coord, Ps, ?DEF_BS_COORD),
    println(F, "<shader type=\"coords\" name=\"~s\" coord=\"~s\">~n"++
	    "    <attributes>~n"
	    "    </attributes>~n"
	    "</shader>~n", [Name,format(Coord)]),
    Op.

bs_mul(#bs_dialog{ps=Ps}=Op) ->
    Value = proplists:get_value(value, Ps, ?DEF_BS_MUL_VALUE),
    {vframe,
     [bs_prop(Op, {input_1,Ps}, [?__(1,"Input 1"),[float,undefined]]),
      bs_prop(Op, {input_2,Ps}, [?__(2,"Input 2"),[float,undefined]]),
      {hframe,[{label,?__(3,"Value")},{text,Value}]}]};
bs_mul(#bs_result{}=Op0) ->
    Op1 = bs(Op0, [input_1]),
    Op = #bs_result{result=[Value|Rest],ps=Ps} = bs(Op1, [input_2]),
    Op#bs_result{result=Rest,ps=[{value,Value}|Ps]};
bs_mul(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op1 = #bs_export{name=Input1} = bs_prop(Op0, {input_1,Ps}),
    Op = #bs_export{name=Input2} = bs_prop(Op1, {input_2,Ps}),
    Value = proplists:get_value(value, Ps, ?DEF_BS_MUL_VALUE),
    print(F, "<shader type=\"mul\" name=\"~s\"~n        ", [Name]),
    bs_print_tag(F, input1, Input1),
    bs_print_tag(F, input2, Input2),
    println(F, "value=\"~.10f\">~n"
	    "    <attributes>~n"
	    "    </attributes>~n"
	    "</shader>~n", [Value]),
    Op#bs_export{name=Name}.

bs_sin(#bs_dialog{ps=Ps}=Op) ->
    bs_prop(Op, {input,Ps,?DEF_BS_FLOAT}, [?__(1,"Input"),[float]]);
bs_sin(#bs_result{}=Op) ->
    bs(Op, [input]);
bs_sin(#bs_export{ps=Ps,dest=F,name=Name}=Op0) ->
    Op = #bs_export{name=Input} = bs_prop(Op0, {input,Ps,?DEF_BS_FLOAT}),
    println(F, "<shader type=\"sin\" name=\"~s\" input=\"~s\">~n"++
	    "    <attributes>", [Name,Input]),
    println(F, "    </attributes>~n"++
	    "</shader>~n", []),
    Op#bs_export{name=Name}.

%%%
%%% End of Block Shaders	   
%%%


light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = case Type of
		   point -> ?DEF_ATTN_POWER;
		   spot -> ?DEF_ATTN_POWER;
		   area -> ?DEF_ATTN_POWER;
		   _ -> ?DEF_POWER
	       end,
    Minimized = proplists:get_value(minimized, YafRay, true),
    Power = proplists:get_value(power, YafRay, DefPower),
    [{vframe,
      [{hframe,[{vframe, [{label,?__(1,"Power")}]},
		{vframe,[{text,Power,[range(power),key(power)]}]},
		panel,
		help_button(light_dialog)]}|
       light_dialog(Name, Type, YafRay)],
      [{title,?__(2,"YafRay Options")},key(minimized),{minimized,Minimized}]}].

light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bias = proplists:get_value(bias, Ps, ?DEF_BIAS),
    Res = proplists:get_value(res, Ps, ?DEF_RES),
    Radius = proplists:get_value(radius, Ps, ?DEF_RADIUS),
    ArealightRadius = proplists:get_value(arealight_radius, Ps, 
					   ?DEF_AREALIGHT_RADIUS),
    ArealightSamples = proplists:get_value(arealight_samples, Ps, 
					   ?DEF_AREALIGHT_SAMPLES),
    ArealightPsamples = proplists:get_value(arealight_psamples, Ps, 
					    ?DEF_AREALIGHT_PSAMPLES),
    QmcMethod = proplists:get_value(qmc_method, Ps, ?DEF_QMC_METHOD),
    Dummy = proplists:get_value(dummy, Ps, ?DEF_DUMMY),
    MinimizedGlow = proplists:get_value(minimized_glow, Ps, true),
    GlowIntensity =
	proplists:get_value(glow_intensity, Ps, ?DEF_GLOW_INTENSITY),
    GlowIntensityOpts = [key(glow_intensity),range(glow_intensity)],
    GlowOffset = proplists:get_value(glow_offset, Ps, ?DEF_GLOW_OFFSET),
    GlowType = proplists:get_value(glow_type, Ps, ?DEF_GLOW_TYPE),
    [{vframe,
      [{hradio,[{?__(3,"Pointlight"),pointlight},
		{?__(4,"Softlight"),softlight},
		{?__(5,"Spherelight"),spherelight}],Type,[key(type),layout]},
       {hframe,
	[{vframe,[{label,?__(6,"Intensity")},
		  {label,?__(7,"Offset")}]},
	 {vframe,[{text,GlowIntensity,GlowIntensityOpts},
		  {text,GlowOffset,
		   [key(glow_offset),
		    range(glow_offset),
		    hook(enable, 
			 ['not',[member,?KEY(glow_intensity),0.0]])]}]},
	 {vframe,[{slider,GlowIntensityOpts},
		  {menu,[{?__(8,"Ad-hoc type"),0},
			 {?__(9,"Han-Wen Nienhuys"),1}],GlowType,
		   [key(glow_type),
		    hook(enable, 
			 ['not',[member,?KEY(glow_intensity),0.0]])]}]}],
	[{title,?__(10,"Glow")},key(minimized_glow),{minimized,MinimizedGlow}]},
       {?__(11,"Cast Shadows"),CastShadows,
	[key(cast_shadows),
	 hook(open, [member,?KEY(type),pointlight])]},
       {hframe,
	[{label,?__(12,"Bias")},{text,Bias,[range(bias),key(bias)]},
	 {label,?__(13,"Res")},{text,Res,[range(res),key(res)]},
	 {label,?__(14,"Radius")},{text,Radius,[range(radius),key(radius)]}],
	[hook(open, [member,?KEY(type),softlight])]},
       {vframe,
	[{hframe,[{label,?__(15,"Radius")},
		  {text,ArealightRadius,[range(arealight_radius),
					 key(arealight_radius)]},
		  {?__(16,"Global Photonlight Dummy"),Dummy,[key(dummy)]}]},
	 {hframe,[{label,?__(17,"Samples")},
		  {text,ArealightSamples,[range(samples),
					  key(arealight_samples)]},
		  {label,?__(18,"Penumbra Samples")},
		  {text,ArealightPsamples,[range(psamples),
					   key(arealight_psamples)]},
		  {menu,[{?__(19,"QMC 0"),0},{?__(20,"QMC 1"),1}],QmcMethod,
		   [key(qmc_method)]}],
	  [hook(enable, ['not',?KEY(dummy)])]}],
	[hook(open, [member,?KEY(type),spherelight])]}]}];
light_dialog(_Name, spot, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_SPOT_TYPE),
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Blend = proplists:get_value(blend, Ps, ?DEF_BLEND),
    Mode = proplists:get_value(mode, Ps, ?DEF_MODE),
    Photons = proplists:get_value(photons, Ps, ?DEF_PHOTONS),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    Fixedradius = proplists:get_value(fixedradius, Ps, ?DEF_FIXEDRADIUS),
    Search = proplists:get_value(search, Ps, ?DEF_SEARCH),
    Mindepth = proplists:get_value(mindepth, Ps, ?DEF_MINDEPTH),
    Cluster = proplists:get_value(cluster, Ps, ?DEF_CLUSTER),
    UseQMC = proplists:get_value(use_QMC, Ps, ?DEF_USE_QMC),
    MinimizedHalo = proplists:get_value(minimized_halo, Ps, true),
    Halo = proplists:get_value(halo, Ps, ?DEF_HALO),
    HaloRes = proplists:get_value(halo_res, Ps, ?DEF_HALO_RES),
    HaloSamples = proplists:get_value(halo_samples, Ps, HaloRes),
    HaloShadowSamples = 
	proplists:get_value(halo_shadow_samples, Ps, HaloRes),
    HaloBlur = proplists:get_value(halo_blur, Ps, ?DEF_HALO_BLUR),
    HaloShadowBlur = 
	proplists:get_value(halo_shadow_blur, Ps, ?DEF_HALO_SHADOW_BLUR),
    HaloFogDensity = 
	proplists:get_value(halo_fog_density, Ps, ?DEF_HALO_FOG_DENSITY),
    HaloFogColor = 
	proplists:get_value(halo_fog_color, Ps, ?DEF_HALO_FOG_COLOR),
    BlurRange = range(blur),
    HaloFrame =
	{hframe,
	 [{vframe,[{label,?__(21,"Res")},
		   {label,?__(22,"Samples")},
		   {label,?__(23,"Shadow Samples")},
		   {label,?__(24,"Blur")},
		   {label,?__(25,"Shadow Blur")},
		   {label,?__(26,"Fog Density")},
		   {label,?__(27,"Fog Color")}]},
	  {vframe,[{text,HaloRes,
		    [range(res),key(halo_res),
		     {hook,
		      fun (update, {Key,_I,Val,Sto0}) ->
			      Sto1 = gb_trees:update
				       (?KEY(halo_samples), Val, Sto0),
			      Sto = gb_trees:update
				      (?KEY(halo_shadow_samples), Val, Sto1),
			      {store,gb_trees:update(Key, Val, Sto)};
			  (_, _) -> void
		      end}]},
		   {text,HaloSamples,
		    [range(samples),key(halo_samples)]},
		   {text,HaloShadowSamples,
		    [range(samples),key(halo_shadow_samples)]},
		   {text,HaloBlur,[BlurRange,key(halo_blur)]},
		   {text,HaloShadowBlur,
		    [BlurRange,key(halo_shadow_blur)]},
		   {text,HaloFogDensity,
		    [range(halo_fog_density),key(halo_fog_density)]},
		   {color,HaloFogColor,[key(halo_fog_color)]}]},
	  {vframe,[panel,
		   panel,
		   panel,
		   {slider,[BlurRange,key(halo_blur)]},
		   {slider,[BlurRange,key(halo_shadow_blur)]},
		   panel]}],
	 [{title,?__(28,"Halo")},
	  key(minimized_halo),{minimized,MinimizedHalo},
	  hook(enable, ?KEY(halo))]},
    %%
    [{hframe,
      [{hradio,[{?__(29,"Spotlight"),spotlight},
		{?__(30,"Photonlight"),photonlight}],Type,[layout,key(type)]},
       {menu,[{?__(31,"Diffuse"),diffuse},{?__(32,"Caustic"),caustic}],Mode,
	[key(mode),hook(open, [member,?KEY(type),photonlight])]},
       {?__(33,"Use QMC"),UseQMC,[key(use_QMC),
			  hook(open, [member,?KEY(type), photonlight])]}]},
     {vframe,
      [{hframe,[{?__(34,"Cast Shadows"),CastShadows,[key(cast_shadows)]},
		{label,?__(35,"Blend")},
		{text,Blend,[range(blend),key(blend)]}]},
       {hframe,[{"",Halo,[key(halo)]},
		HaloFrame]}],
      [hook(open, [member,?KEY(type), spotlight])]},
     {hframe,[{vframe,[{label,?__(36,"Photons")},
		       {label,?__(37,"Depth")},
		       {label,?__(38,"Fixedradius")}]},
	      {vframe,[{text,Photons,[range(photons),key(photons)]},
		       {text,Depth,[range(depth),key(depth)]},
		       {text,Fixedradius,[range(fixedradius),
					  key(fixedradius)]}]},
	      {vframe,[{label,?__(39,"Search")},
		       {label,?__(40,"Mindepth")},
		       {label,?__(41,"Cluster")}]},
	      {vframe,[{text,Search,[range(search),key(search)]},
		       {text,Mindepth,[range(depth),key(mindepth)]},
		       {text,Cluster,[range(cluster),key(cluster)]}]}],
      [hook(open, [member,?KEY(type), photonlight])]}];
light_dialog(_Name, infinite, Ps) ->
    CastShadows = proplists:get_value(cast_shadows, Ps, ?DEF_CAST_SHADOWS),
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    %%
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    %%
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    A_var = proplists:get_value(a_var, Ps, ?DEF_SUNSKY_VAR),
    B_var = proplists:get_value(b_var, Ps, ?DEF_SUNSKY_VAR),
    C_var = proplists:get_value(c_var, Ps, ?DEF_SUNSKY_VAR),
    D_var = proplists:get_value(d_var, Ps, ?DEF_SUNSKY_VAR),
    E_var = proplists:get_value(e_var, Ps, ?DEF_SUNSKY_VAR),
    %%
    [{?__(42,"Cast Shadows"),CastShadows,[key(cast_shadows)]},
     {vframe,
      [{hradio,[{?__(43,"Constant"),constant},
		{?__(44,"Sunsky"),sunsky},
		{?__(45,"None"), undefined}],Bg,[layout,key(background)]},
       {hframe,[{label,?__(46,"Color")},
		{color,BgColor,[key(background_color)]}],
	[hook(open, [member,?KEY(background),constant])]},
       {vframe,
	[{hframe,[]},
	 {hframe,
	  [{vframe,[{label,?__(47,"Turbidity")},
		    {label,"a: "++?__(48,"Horizon Brightness")},
		    {label,"b: "++?__(49,"Horizon Spread")},
		    {label,"c: "++?__(50,"Sun Brightness")},
		    {label,"d: "++?__(51,"Sun Contraction")},
		    {label,"e: "++?__(52,"Sun Backscatter")}]},
	   {vframe,[{text,Turbidity,[range(turbidity),key(turbidity)]},
		    {text,A_var,[key(a_var)]},
		    {text,B_var,[key(b_var)]},
		    {text,C_var,[key(c_var)]},
		    {text,D_var,[key(d_var)]},
		    {text,E_var,[key(e_var)]}]}]}],
	[hook(open, [member,?KEY(background),sunsky])]}],
      [{title,?__(53,"Background")}]}];
light_dialog(_Name, ambient, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND),
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    BgFnameImage = proplists:get_value(background_filename_image, Ps, 
				       ?DEF_BACKGROUND_FILENAME),
    BrowsePropsImage = [{dialog_type,open_dialog},
			{extensions,[{".jpg",?__(54,"JPEG compressed image")},
				     {".tga",?__(55,"Targa bitmap")}]}],
    BgFnameHDRI = proplists:get_value(background_filename_HDRI, Ps, 
				      ?DEF_BACKGROUND_FILENAME),
    BrowsePropsHDRI = [{dialog_type,open_dialog},
		       {extensions,[{".hdr",?__(56,"High Dynamic Range image")},
				    {".exr",?__(95,"OpenEXR image")}]}],
    BgExpAdj = proplists:get_value(background_exposure_adjust, Ps, 
				   ?DEF_BACKGROUND_EXPOSURE_ADJUST),
    BgMapping = proplists:get_value(background_mapping, Ps, 
				    ?DEF_BACKGROUND_MAPPING),
    BgPower = proplists:get_value(background_power, Ps, 
				  ?DEF_BACKGROUND_POWER),
    BgPrefilter = proplists:get_value(background_prefilter, Ps,
				  ?DEF_BACKGROUND_PREFILTER),
    BgEnlight = proplists:get_value(background_enlight, Ps, false),
    %%
    Type = proplists:get_value(type, Ps, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, Ps, ?DEF_SAMPLES),
    Depth = proplists:get_value(depth, Ps, ?DEF_DEPTH),
    CausDepth = proplists:get_value(caus_depth, Ps, ?DEF_CAUS_DEPTH),
    Direct = proplists:get_value(direct, Ps, ?DEF_DIRECT),
    UseQMC = proplists:get_value(use_QMC, Ps, ?DEF_USE_QMC),
    %%
    PathlightMode = 
	proplists:get_value(pathlight_mode, Ps, ?DEF_PATHLIGHT_MODE),
    UseMaxdistance = 
	proplists:get_value(use_maxdistance, Ps, ?DEF_USE_MAXDISTANCE),
    Maxdistance = 
	proplists:get_value(maxdistance, Ps, ?DEF_MAXDISTANCE),
    %%
    CacheMinimized = proplists:get_value(cache_minimized, Ps, true),
    Cache = proplists:get_value(cache, Ps, ?DEF_CACHE),
    CacheSize = proplists:get_value(cache_size, Ps, ?DEF_CACHE_SIZE),
    AngleThreshold = proplists:get_value(angle_threshold, Ps, 
					 ?DEF_ANGLE_THRESHOLD),
    AngleKey = ?KEY(angle_threshold),
    AngleRange = range(angle_threshold),
    ShadowThreshold = proplists:get_value(shadow_threshold, Ps, 
					  ?DEF_SHADOW_THRESHOLD),
    Gradient = proplists:get_value(gradient, Ps, ?DEF_GRADIENT),
    ShowSamples = proplists:get_value(show_samples, Ps, ?DEF_SHOW_SAMPLES),
    Search = proplists:get_value(search, Ps, ?DEF_SEARCH),
    %%
    GplPhotons = proplists:get_value(globalphotonlight_photons, Ps,
				     ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
    GplRadius = proplists:get_value(globalphotonlight_radius, Ps,
				    ?DEF_GLOBALPHOTONLIGHT_RADIUS),
    GplDepth = proplists:get_value(globalphotonlight_depth, Ps,
				   ?DEF_GLOBALPHOTONLIGHT_DEPTH),
    GplSearch = proplists:get_value(globalphotonlight_search, Ps,
				    ?DEF_GLOBALPHOTONLIGHT_SEARCH),
    [{hradio,[{?__(57,"Hemilight"),hemilight},
	      {?__(58,"Pathlight"),pathlight},
	      {?__(59,"Global Photonlight"),globalphotonlight}],
      Type,[layout,key(type)]},
     %% Hemilight and Pathlight
     {hframe,
      [{vframe,[{?__(60,"Use QMC"),UseQMC,[key(use_QMC)]},
		{?__(61,"Direct"),Direct,[key(direct),
				  hook(open, [member,?KEY(type),
					      pathlight])]}]},
       {vframe,[{label,?__(62,"Samples")},
		{label,?__(63,"Depth"),[hook(open, [member,?KEY(type),pathlight])]}]},
       {vframe,[{text,Samples,[range(samples),key(samples)]},
		{text,Depth,[range(raydepth),key(depth),
			     hook(open, [member,?KEY(type),pathlight])]}]},
       {vframe,[panel,
		{label,?__(64,"Caus Depth")}],
	[hook(open, [member,?KEY(type),pathlight])]},
       {vframe,[panel,
		{text,CausDepth,[range(raydepth),key(caus_depth)]}],
	[hook(open, [member,?KEY(type),pathlight])]}],
      [hook(open, [member,?KEY(type),hemilight,pathlight])]},
     {menu,[{?__(65,"default mode"),undefined},{?__(66,"Occlusion mode"),occlusion}],
      PathlightMode,
      [key(pathlight_mode),hook(open, [member,?KEY(type),pathlight])]},
     {hframe,
      [{?__(67,"Maxdistance"),UseMaxdistance,[key(use_maxdistance)]},
       {text,Maxdistance,[key(maxdistance),
			  range(maxdistance),
			  hook(enable, ?KEY(use_maxdistance))]}],
      [hook(open, [member,?KEY(type),hemilight,pathlight])]},
     %% Pathlight
     {vframe,
      [{hframe,
	[{"",Cache,[key(cache)]},
	 {hframe,
	  [{vframe,
	    [{label,?__(68,"Size")},
	     {label,?__(69,"Angle Threshold")},
	     panel,
	     {label,?__(70,"Shadow Threshold")},
	     {?__(71,"Gradient"),Gradient,[key(gradient)]},
	     {label,?__(72,"Search")}]},
	   {vframe,
	    [{text,CacheSize,[key(cache_size),range(cache_size)]},
	     {text,AngleThreshold,[{key,AngleKey},AngleRange]},
	     {slider,[{key,AngleKey},AngleRange]},
	     {text,ShadowThreshold,[key(shadow_threshold),
				    range(shadow_threshold)]},
	     {?__(73,"Show Samples"),ShowSamples,[key(show_samples)]},
	     {text,Search,[key(search),range(cache_search)]}]}],
	  [{title,?__(74,"Irradiance Cache")},
	   {minimized,CacheMinimized},key(cache_minimized),
	   hook(enable, ?KEY(cache))]}],
	[hook(enable, ['not',?KEY(direct)])]}],
      [hook(open, [member,?KEY(type),pathlight])]},
     %% Global Photonlight
     {hframe,[{vframe,[{label,?__(75,"Photons")},
		       {label,?__(76,"Depth")}]},
	      {vframe,[{text,GplPhotons,
			[range(photons),key(globalphotonlight_photons)]},
		       {text,GplDepth,
			[range(raydepth),
			 key(globalphotonlight_depth)]}]},
	      {vframe,[{label,?__(77,"Radius")},
		       {label,?__(78,"Search")}]},
	      {vframe,[{text,GplRadius,
			[range(fixedradius),key(globalphotonlight_radius)]},
		       {text,GplSearch,
			[range(search),key(globalphotonlight_search)]}]}],
      [hook(open, [member,?KEY(type),globalphotonlight])]},
     %% Backgrounds
     {vframe,
      [{hradio,[{?__(79,"HDRI"),'HDRI'},
		{?__(80,"Image"),image},
		{?__(81,"Constant"),constant},
		{?__(82,"None"), undefined}],Bg,[layout,key(background)]},
       {hframe,[{label,?__(83,"Filename")},
		{button,{text,BgFnameHDRI,
			 [key(background_filename_HDRI),
			  {props,BrowsePropsHDRI}]}}],
	[hook(open, [member,?KEY(background),'HDRI'])]},
       {hframe,[{label,?__(84,"Filename")},
		{button,{text,BgFnameImage,
			 [key(background_filename_image),
			  {props,BrowsePropsImage}]}}],
	[hook(open, [member,?KEY(background),image])]},
       {hframe,
	[{hframe,[{label,?__(85,"Exposure Adjust")},
		  {text,BgExpAdj,[key(background_exposure_adjust),
				  range(exposure_adjust)]},
		  {menu,[{?__(86,"Light Probe (Angular)"),probe},{?__(87,"Spherical (Lat-Long)"),spherical}],
		   BgMapping,[key(background_mapping)]}],
	  [hook(open, [member,?KEY(background),'HDRI'])]},
	 {hframe,[{label,?__(88,"Power")},
		  {text,BgPower,[key(background_power),range(power)]}],
	  [hook(open, [member,?KEY(background),image])]},
	 {?__(89,"Enlight"),BgEnlight,[key(background_enlight)]},
	 {?__(96,"Prefilter"),BgPrefilter,[key(background_prefilter)]}],
	[hook(open, [member,?KEY(background),'HDRI',image])]},
       {hframe,[{label,?__(90,"Color")},
		{color,BgColor,[key(background_color)]}],
	[hook(open, [member,?KEY(background),constant])]}],
      [{title,?__(91,"Background")}]}];
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, 
					   ?DEF_AREALIGHT_SAMPLES),
    ArealightPsamples = proplists:get_value(arealight_psamples, Ps, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, Ps, ?DEF_DUMMY),
    [{?__(92,"Global Photonlight Dummy"),Dummy,[key(dummy)]},
     {hframe,[{label,?__(93,"Samples")},
	      {text,ArealightSamples,[range(samples),key(arealight_samples)]},
	      {label,?__(94,"Penumbra Samples")},
	      {text,ArealightPsamples,[range(psamples),
				       key(arealight_psamples)]}],
      [hook(enable, ['not',?KEY(dummy)])]}];
light_dialog(_Name, _Type, _Ps) ->
%%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Ps0, 
	     [{?KEY(minimized),Minimized},{?KEY(power),Power}|Res0]) ->
    {LightPs0,Res1} = light_result(Res0),
    LightPs = [{Key,Val} || {?KEY(Key),Val} <- LightPs0],
    Ps = [{?TAG,[{minimized,Minimized},{power,Power}|LightPs]}
	  |keydelete(?TAG, 1, Ps0)],
%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res1}.

%% Point
light_result([{?KEY(type),pointlight}|_]=Ps) ->
    split_list(Ps, 14);
light_result([{?KEY(type),softlight}|_]=Ps) ->
    split_list(Ps, 14);
light_result([{?KEY(type),spherelight}|_]=Ps) ->
    split_list(Ps, 14);
%% Spot
light_result([{?KEY(type),spotlight}|_]=Ps) ->
    split_list(Ps, 20);
light_result([{?KEY(type),photonlight}|_]=Ps) ->
    split_list(Ps, 20);
%% Infinite
light_result([_,{?KEY(background),_}|_]=Ps) ->
    split_list(Ps, 9);
%% Area
light_result([_,{?KEY(arealight_samples),_}|_]=Ps) ->
    split_list(Ps, 3);
%% Ambient
light_result([{?KEY(type),hemilight}|_]=Ps) ->
    split_list(Ps, 30);
light_result([{?KEY(type),pathlight}|_]=Ps) ->
    split_list(Ps, 30);
light_result([{?KEY(type),globalphotonlight}|_]=Ps) ->
    split_list(Ps, 30);
light_result(Ps) ->
%    erlang:display({?MODULE,?LINE,Ps}),
    {[],Ps}.


pref_dialog(St) ->
    [{dialogs,Dialogs},{renderer,Renderer},
     {options,Options}] = 
	get_user_prefs([{dialogs,?DEF_DIALOGS},{renderer,?DEF_RENDERER},
			{options,?DEF_OPTIONS}]),
    Dialog =
	[{vframe,
	  [{hframe,
	    [{menu,[{?__(1,"Disabled Dialogs"),disabled},
		    {?__(2,"Automatic Dialogs"),auto},
		    {?__(3,"Enabled Dialogs"),enabled}],
	      Dialogs,[{key,dialogs}]},
	     panel,
	     help_button(pref_dialog)]},
	   {hframe,
	    [{vframe,
	      [{label,?__(4,"Executable")},
	       {label,?__(5,"Options")}]},
	     {vframe,
	      [{button,{text,Renderer,[{key,renderer},
				       wings_job:browse_props()]}},
	       {text,Options,[{key,options}]}]}]}]}],
    wpa:dialog(?__(6,"YafRay Options"), Dialog, 
	       fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_user_prefs(Attr),
    init_pref(),
    St.



export_dialog(Op, Ask, Title, Fun) ->
    Keep = {Op,Fun},
    wpa:dialog(Ask, Title, 
	       export_dialog_qs(Op, get_prefs(export_prefs())
				++[save,load,reset]),
	       export_dialog_fun(Keep)).

export_dialog_fun(Keep) ->
    fun (Attr) -> export_dialog_loop(Keep, Attr) end.

export_prefs() ->
    [{subdivisions,?DEF_SUBDIVISIONS},
     {keep_xml,?DEF_KEEP_XML},
     {raydepth,?DEF_RAYDEPTH},
     {gamma,?DEF_GAMMA},
     {bias,?DEF_BIAS},
     {exposure,?DEF_EXPOSURE},
     {render_format,?DEF_RENDER_FORMAT},
     {exr_flag_float,false},
     {exr_flag_zbuf,false},
     {exr_flag_compression,?DEF_EXR_FLAG_COMPRESSION},
     {aa_passes,?DEF_AA_PASSES},
     {aa_minsamples,?DEF_AA_MINSAMPLES},
     {aa_jitterfirst,?DEF_AA_JITTERFIRST},
     {aa_threshold,?DEF_AA_THRESHOLD},
     {aa_pixelwidth,?DEF_AA_PIXELWIDTH},
     {clamp_rgb,?DEF_CLAMP_RGB},
     {background_color,?DEF_BACKGROUND_COLOR},
     {save_alpha,?DEF_SAVE_ALPHA},
     {ortho,?DEF_ORTHO},
     {bokeh_use_QMC,?DEF_USE_QMC},
     {width,?DEF_WIDTH},
     {aperture,?DEF_APERTURE},
     {bokeh_type,?DEF_BOKEH_TYPE},
     {height,?DEF_HEIGHT},
     {aperture,?DEF_APERTURE},
     {bokeh_bias,?DEF_BOKEH_BIAS},
     {bokeh_rotation,?DEF_BOKEH_ROTATION},
     {fog_density,?DEF_FOG_DENSITY},
     {fog_color,?DEF_FOG_COLOR}].

export_dialog_qs(Op,
		 [{subdivisions,SubDiv},
		  {keep_xml,KeepXML},
		  {raydepth,Raydepth},
		  {gamma,Gamma},
		  {bias,Bias},
		  {exposure,Exposure},
		  {render_format,RenderFormat},
		  {exr_flag_float,ExrFlagFloat},
		  {exr_flag_zbuf,ExrFlagZbuf},
		  {exr_flag_compression,ExrFlagCompression},
		  {aa_passes,AA_passes},
		  {aa_minsamples,AA_minsamples},
		  {aa_jitterfirst,AA_jitterfirst},
		  {aa_threshold,AA_threshold},
		  {aa_pixelwidth,AA_pixelwidth},
		  {clamp_rgb,ClampRGB},
		  {background_color,BgColor},
		  {save_alpha,SaveAlpha},
		  {ortho,Ortho},
		  {bokeh_use_QMC,BokehUseQMC},
		  {width,Width},
		  {aperture,Aperture},
		  {bokeh_type,BokehType},
		  {height,Height},
		  {aperture,_Aperture},
		  {bokeh_bias,BokehBias},
		  {bokeh_rotation,BokehRotation},
		  {fog_density,FogDensity},
		  {fog_color,FogColor},
		  _Save,_Load,_Reset]) ->
    AA_thresholdFlags = [range(aa_threshold),{key,aa_threshold}],
    AA_pixelwidthFlags = [range(aa_pixelwidth),{key,aa_pixelwidth}],
    BiasFlags = [range(bias),{key,bias}],
    [{hframe,[{label,?__(1,"Sub-division Steps")},
	      {text,SubDiv,[{key,subdivisions},range(subdivisions)]},
	      case Op of
		  render ->
		      {?__(2,"Write and keep .xml file"),KeepXML,[{key,keep_xml}]};
		  _ ->
		      {value,KeepXML,[{key,keep_xml}]}
	      end],
      [{title,?__(3,"Pre-rendering")}]},
     {hframe,
      [{vframe,
	[{label,?__(4,"Raydepth")},
	 {label,?__(5,"Gamma")}]},
       {vframe,
	[{text,Raydepth,[range(raydepth),{key,raydepth}]},
	 {text,Gamma,[range(gamma),{key,gamma}]}]},
       {vframe,
	[{label,?__(6,"Bias")},
	 {label,?__(7,"Exposure")}]},
       {vframe,
	[{text,Bias,BiasFlags},
	 {text,Exposure,[range(exposure),{key,exposure}]}]},
       {vframe,
	[{slider,BiasFlags},
	 panel]}],
      [{title,?__(8,"Render")}]},
     {hframe,
	[{menu,[{Ext++" ("++Desc++")",Format}
		|| {Format,Ext,Desc} <- wings_job:render_formats(),
		   (Format == tga) or (Format == hdr) or (Format == exr)],
	  RenderFormat,
	  [{key,render_format},layout]},
	 {hframe,
	  [{?__(9,"Float"),ExrFlagFloat,[{key,exr_flag_float}]},
	   {?__(10,"Zbuf"),ExrFlagZbuf,[{key,exr_flag_zbuf}]},
	   {label," "++?__(11,"Compression:")},
	   {menu,
	    [{?__(12,"none"),compression_none},
	     {"piz",compression_piz},
	     {"rle",compression_rle},
	     {"pxr24",compression_pxr24},
	     {"zip",compression_zip}],
	    ExrFlagCompression,
	    [{key,exr_flag_compression}]}],
	  [hook(open, [member,render_format,exr])]}],
      [{title,?__(13,"Output")}]},
     {hframe,
      [{vframe,
	[{hframe,
	  [{vframe,
	    [{label,?__(14,"AA_passes")},
	     {label,?__(15,"AA_minsamples")}]},
	   {vframe,
	    [{text,AA_passes,[range(aa_passes),{key,aa_passes}]},
	     {text,AA_minsamples,[range(aa_minsamples),
				  {key,aa_minsamples}]}]}]},
	 {?__(16,"AA_jitterfirst"),AA_jitterfirst,[{key,aa_jitterfirst}]}]},
       {vframe,
	[{hframe,
	  [{vframe,
	    [{label,?__(17,"AA_threshold")},
	     {label,?__(18,"AA_pixelwidth")}]},
	   {vframe,
	    [{text,AA_threshold,AA_thresholdFlags},
	     {text,AA_pixelwidth,AA_pixelwidthFlags}]}]},
	 {?__(19,"Clamp RGB"),ClampRGB,[{key,clamp_rgb}]}]},
       {vframe,
	[{slider,AA_thresholdFlags},
	 {slider,AA_pixelwidthFlags}]}],
      [{title,?__(20,"Anti-Aliasing")}]},
     {hframe,
      [{label,?__(21,"Default Color")},
       {color,BgColor,[{key,background_color}]},
       {label,?__(22,"Alpha Channel:")},
       {menu,[{?__(23,"Off"),false},{?__(61,"On"),true},
	      {?__(24,"Premultiply"),premultiply},{?__(25,"Backgroundmask"),backgroundmask}],
	SaveAlpha,
	[{key,save_alpha}]}],
      [{title,?__(26,"Background")}]},
     {hframe,
      [{vframe,
	[{menu,[{?__(27,"Perspective"),false},
		{?__(28,"Orthographic"),true},
		{?__(29,"Spherical"),spherical},
		{?__(30,"Lightprobe"),lightprobe}],Ortho,[{key,ortho}]},
	 {label,?__(31,"Depth Of Field:")},
	 {hframe,[panel,{?__(32,"Use QMC"),BokehUseQMC,
			 [{key,bokeh_use_QMC},
			  hook(enable, 
			       ['not',[member,aperture,0.0]])]}]},
	 panel]},
       {vframe,
	[{label,?__(33,"Width")},
	 {label,?__(34,"Aperture")},
	 {label,?__(35,"Type")},
	 {label,?__(36,"Rotation")}]},
       {vframe,
	[{hframe,
	  [{vframe,[{text,Width,[range(pixels),{key,width},{width,6}]},
		    {text,Aperture,[range(aperture),{key,aperture},{width,6}]},
		    {menu,[{?__(37,"Disk1"),disk1},{?__(38,"Disk2"),disk2},
			   {?__(39,"Triangle"),triangle},
			   {?__(40,"Square"),square},{?__(41,"Pentagon"),pentagon},
			   {?__(42,"Hexagon"),hexagon},{?__(43,"Ring"),ring}],
		     BokehType,[{key,bokeh_type},
				hook(enable, 
				     ['not',[member,aperture,0.0]])]}]},
	   {vframe,[{label,?__(44,"Height")},
		    {label,?__(45,"f-stop")},
		    {label,?__(46,"Bias")}]},
	   {vframe,[{text,Height,[range(pixels),{key,height},{width,6}]},
		    {menu,[{F,math:sqrt(A)}
			   || {F,A} <- [{"1.0",1/1},{"1.4",1/2},{"2",1/4},
					{"2.8",1/8},{"4",1/16},{"5.6",1/32},
					{"8",1/64},{"11",1/128},{"16",256},
					{"22",1/512},{"32",1/1024},
					{?__(47,"pinhole"),0.0}]],
		     Aperture,[{key,aperture}]},
		    {menu,[{?__(48,"Uniform"),uniform},{?__(49,"Center"),center},
			   {?__(50,"Edge"),edge}],
		     BokehBias,[{key,bokeh_bias},
				hook(enable, 
				     ['not',[member,aperture,0.0]])]}]}]},
	 {slider,{text,BokehRotation,
		  [range(bokeh_rotation),
		   {key,bokeh_rotation},
		   hook(enable, 
			['not',[member,aperture,0.0]])]}}]}],
      [{title,?__(51,"Camera")}]},
     {hframe,
      [{label,?__(52,"Density")},
       {text,FogDensity,[range(fog_density),{key,fog_density}]},
       {label,?__(53,"Color")},
       {color,FogColor,[{key,fog_color}]}],
      [{title,?__(54,"Fog")}]},
     {hframe,[{button,?__(55,"Save"),done,[{info,?__(56,"Save to user preferences")}]},
	      {button,?__(57,"Load"),done,[{info,?__(58,"Load from user preferences")}]},
	      {button,?__(59,"Reset"),done,[{info,?__(60,"Reset to default values")}]}]}].

export_dialog_loop({Op,Fun}=Keep, Attr) ->
    {Prefs,Buttons} = split_list(Attr, 29),
    case Buttons of
	[true,false,false] -> % Save
	    set_user_prefs(Prefs),
	    {dialog,
	     export_dialog_qs(Op, Attr),
	     export_dialog_fun(Keep)};
	[false,true,false] -> % Load
	    {dialog,
	     export_dialog_qs(Op, 
			      get_user_prefs(export_prefs())
			      ++[save,load,reset]),
	     export_dialog_fun(Keep)};
	[false,false,true] -> % Reset
	    {dialog,
	     export_dialog_qs(Op,
			      export_prefs()++[save,load,reset]),
	     export_dialog_fun(Keep)};
	[false,false,false] -> % Ok
	    Fun(Prefs)
    end.

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



%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs,mat=Mats,creator=Creator}) ->
    wpa:popup_console(),
    ExportTS = erlang:now(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    KeepXML = proplists:get_value(keep_xml, Attr, ?DEF_KEEP_XML),
    RenderFormat = 
	proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    ExportDir = filename:dirname(Filename),
    {ExportFile,RenderFile} =
	case {Render,KeepXML} of
	    {true,true} ->
		{filename:rootname(Filename)++".xml",
		 Filename};
	    {true,false} ->
		{filename:join(ExportDir, 
			       ?MODULE_STRING++"-"
			       ++wings_job:uniqstr()++".xml"),
		 Filename};
	    {false,_} ->
		{value,{RenderFormat,Ext,_}} =
		    lists:keysearch(RenderFormat, 1, 
				    wings_job:render_formats()),
		{Filename,filename:rootname(Filename)++Ext}
	end,
    F = open(ExportFile, export),
    io:format(?__(1,"Exporting  to:")++" ~s~n"++
	      ?__(2,"for render to:")++" ~s~n", [ExportFile,RenderFile]),
    CameraName = "x_Camera",
    ConstBgName = "x_ConstBackground",
    Lights = proplists:get_value(lights, Attr, []),
    %%
    println(F, "<!-- ~s: Exported from ~s -->~n"++
	    "~n"++
	    "<scene>", [filename:basename(ExportFile), Creator]),
    %%
    section(F, "Shaders"),
    MatsGb =
	foldl(fun ({Name,Mat}, Gb) -> 
		      export_shader(F, "w_"++format(Name), Mat, ExportDir),
		      println(F),
		      gb_trees:insert(Name, Mat, Gb)
	      end, gb_trees:empty(), Mats),
    %%
    section(F, "Objects"),
    foreach(fun (#e3d_object{name=Name,obj=Mesh}) ->
		    export_object(F, "w_"++format(Name), Mesh, MatsGb),
		    println(F)
	    end, Objs),
    %%
    section(F, "Lights"),
    BgLights = 
	reverse(
	  foldl(fun ({Name,Ps}=Light, Bgs) -> 
			Bg = export_light(F, "w_"++format(Name), Ps),
			println(F),
			case Bg of
			    undefined -> Bgs;
			    _ -> [Light|Bgs]
			end
		end, [], Lights)),
    %%
    section(F, "Background, Camera, Filter and Render"),
    warn_multiple_backgrounds(BgLights),
    BgName = 
	case BgLights of
	    [] ->
		BgColor = proplists:get_value(background_color, Attr,
					      ?DEF_BACKGROUND_COLOR),
		Ps = [{?TAG,[{background,constant},
			     {background_color,BgColor}]}],
		export_background(F, ConstBgName, Ps),
		ConstBgName;
	    [{Name,Ps}|_] ->
		N = "w_"++format(Name),
		export_background(F, N, Ps),
		N
	end,
    println(F),
    export_camera(F, CameraName, Attr),
    println(F),
    export_render(F, CameraName, BgName, filename:basename(RenderFile), Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),
    %%
    [{options,Options}] =
	get_user_prefs([{options,?DEF_OPTIONS}]),
    case {get_var(renderer),Render} of
	{_,false} ->
	    wings_job:export_done(ExportTS),
	    io:nl();
	{false,true} ->
	    %% Should not happen since the file->render dialog
	    %% must have been disabled
	    if KeepXML -> ok; true -> file:delete(ExportFile) end,
	    no_renderer;
	{_,true} when ExportFile == RenderFile ->
	    export_file_is_render_file;
	{Renderer,true} ->
	    ArgStr = Options++case Options of
				  [] -> [];
				  _ -> " "
			      end
		++wings_job:quote(filename:basename(ExportFile)),
	    PortOpts = [{cd,filename:dirname(ExportFile)}],
	    Handler =
		fun (Status) ->
			if KeepXML -> ok; true -> file:delete(ExportFile) end,
			set_var(rendering, false),
			case Status of
			    ok -> {RenderFormat,RenderFile};
			    _  -> Status
			end
		end,
	    file:delete(RenderFile),
	    set_var(rendering, true),
	    wings_job:render(ExportTS, Renderer, ArgStr, PortOpts, Handler)
    end.

warn_multiple_backgrounds([]) ->
    ok;
warn_multiple_backgrounds([_]) ->
    ok;
warn_multiple_backgrounds(BgLights) ->
    io:format(?__(1,"WARNING: Multiple backgrounds")++" - ", []),
    foreach(fun ({Name,_}) ->
		    io:put_chars([format(Name), $ ])
	    end, BgLights),
    io:nl(),
    ok.



% template(F, Fun_0) ->
%     println(F, "<!-- Begin Template"),
%     Fun_0(),
%     println(F, "End Template -->").

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).


export_shader(F, Name, Mat, ExportDir) ->
    YafRay = proplists:get_value(?TAG, Mat, []),
    ShaderType = proplists:get_value(shader_type, YafRay, ?DEF_SHADER_TYPE),
    case ShaderType of
	block ->
	    BlockShader = proplists:get_value(block_shader, YafRay, ?DEF_BS),
	    export_block_shader(F, BlockShader, Name, Mat, ExportDir);
	generic ->
	    export_generic_shader(F, Name, Mat, ExportDir, YafRay)
    end.

export_generic_shader(F, Name, Mat, ExportDir, YafRay) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, YafRay, def_modulators(Maps)),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
		  case export_texture(F, [Name,$_,format(N)], 
				      Maps, ExportDir, M) of
		      off -> N+1;
		      ok ->
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "<shader type=\"generic\" name=\"~s\">~n"++ 
	    "    <attributes>", [Name]),
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Color = alpha(DiffuseA),
    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),
    export_rgb(F, color, Color),
    export_rgb(F, specular, Specular),
    %% XXX Wings scaling of shininess is weird. Commonly this value
    %% is the cosine power and as such in the range 0..infinity.
    %% OpenGL limits this to 0..128 which mostly is sufficient.
    println(F, "        <hard value=\"~.10f\"/>", 
		   [proplists:get_value(shininess, OpenGL)*128.0]),
    export_rgb(F, reflected, 
	       proplists:get_value(reflected, YafRay, DefReflected)),
    export_rgb(F, transmitted, 
	       proplists:get_value(transmitted, YafRay, DefTransmitted)),
    case proplists:get_value(fresnel2, YafRay, false) of
	true ->
	    export_rgb(F, reflected2, 
		       proplists:get_value(reflected2, YafRay, DefReflected)),
	    export_rgb(F, transmitted2, 
		       proplists:get_value(transmitted2, YafRay, 
					   DefTransmitted));
	false -> ok
    end,
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    TIR = proplists:get_value(tir, YafRay, ?DEF_TIR),
    FastFresnel = proplists:get_value(fast_fresnel, YafRay, ?DEF_FAST_FRESNEL),
    MinRefle = proplists:get_value(min_refle, YafRay, ?DEF_MIN_REFLE),
    AbsorptionColor = 
	proplists:get_value(absorption_color, YafRay, ?DEF_ABSORPTION_COLOR),
    case AbsorptionColor of
	?DEF_ABSORPTION_COLOR -> ok;
	{AbsR,AbsG,AbsB} ->
	    AbsD =
		proplists:get_value(absorption_dist, YafRay, 
				    ?DEF_ABSORPTION_DIST),
	    export_rgb(F, absorption, {-math:log(max(AbsR, ?NONZERO))/AbsD,
				       -math:log(max(AbsG, ?NONZERO))/AbsD,
				       -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,
    DispersionPower =
	proplists:get_value(dispersion_power, YafRay, ?DEF_DISPERSION_POWER),
    case DispersionPower of
	0.0 -> ok;
	_   ->
	    DispersionSamples =
		proplists:get_value(dispersion_samples, YafRay, 
				    ?DEF_DISPERSION_SAMPLES),
	    DispersionJitter = 
		proplists:get_value(dispersion_jitter, YafRay, 
			    ?DEF_DISPERSION_JITTER),
	    println(F, "        <dispersion_power value=\"~.10f\"/>~n"
		    "        <dispersion_samples value=\"~w\"/>~n"
		    "        <dispersion_jitter value=\"~s\"/>",
		    [DispersionPower,DispersionSamples,
		     format(DispersionJitter)])
	    
	    
    end,
    println(F, "        <IOR value=\"~.10f\"/>~n"
	    "        <tir value=\"~s\"/>~n"
	    "        <fast_fresnel value=\"~s\"/>~n"
	    "        <min_refle value=\"~.10f\"/>~n"
	    "    </attributes>", 
	    [IOR,format(TIR),format(FastFresnel),MinRefle]),
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
		  case export_modulator(F, [Name,$_,format(N)], 
					Maps, M, Opacity) of
		      off -> N+1;
		      ok ->
			  println(F),
			  N+1
		  end;
	      (_, N) ->
		  N % Ignore old modulators
	  end, 1, Modulators),
    println(F, "</shader>").

export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
	{false,_,_} ->
	    off;
	{true,_,image} ->
	    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
	    export_texture(F, Name, image, Filename);
	{true,_,jpeg} -> %% Old tag
	    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
	    export_texture(F, Name, image, Filename);
	{true,_,{map,Map}} ->
	    case proplists:get_value(Map, Maps, undefined) of
		undefined ->
		    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
		#e3d_image{name=ImageName}=Image ->
		    MapFile = ImageName++".tga",
		    ok = e3d_image:save(Image, 
					filename:join(ExportDir, MapFile)),
		    export_texture(F, Name, image, MapFile)
	    end;
	{true,_,Type} ->
	    export_texture(F, Name, Type, Ps)
    end.

export_texture(F, Name, image, Filename) ->
    println(F, "<texture type=\"image\" name=\"~s\">~n"++
	    "    <filename value=\"~s\"/>~n"++
	    "</texture>", [Name,Filename]);
export_texture(F, Name, Type, Ps) ->
    println(F, "<texture type=\"~s\" name=\"~s\">", [format(Type),Name]),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    export_rgb(F, color1, Color1),
    export_rgb(F, color2, Color2),
    println(F, "    <depth value=\"~w\"/>", [Depth]),
    if Type =/= clouds ->
	    Turbulence = proplists:get_value(turbulence, Ps, 
					     ?DEF_MOD_TURBULENCE),
	    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
	    println(F, "    <turbulence value=\"~.6f\"/>~n"++
		    "    <hard value=\"~s\"/>", [Turbulence,format(Hard)]);
       true ->
	    ok
    end,
    case Type of
	marble ->
	    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
	    println(F, "    <sharpness value=\"~.6f\"/>", [Sharpness]);
	wood ->
	    RingscaleX = proplists:get_value(ringscale_x, Ps, 
					     ?DEF_MOD_RINGSCALE_X),
	    RingscaleZ = proplists:get_value(ringscale_z, Ps, 
					     ?DEF_MOD_RINGSCALE_Z),
	    %% Coordinate rotation, see export_pos/3.
	    println(F, "    <ringscale_x value=\"~.6f\"/>~n"++
		    "    <ringscale_y value=\"~.6f\"/>",
		    [RingscaleZ,RingscaleX]);
	clouds ->
	    ok
    end,
    println(F, "</texture>").

export_modulator(F, Texname, Maps, {modulator,Ps}, Opacity) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
	{false,_,_} ->
	    off;
	{true,Mode,Type} ->
	    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
	    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
	    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
	    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
	    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
	    Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
	    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
	    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
	    Color = Diffuse * Opacity,
	    HardValue = Shininess,
	    Transmission = Diffuse * (1.0 - Opacity),
	    Reflection = Ambient,
	    TexCo = 
		case Type of
		    image -> "texco=\"uv\" ";
		    jpeg -> "texco=\"uv\" ";
		    {map,_} -> "texco=\"uv\" ";
		    marble -> "texco=\"global\" ";
		    wood -> "texco=\"global\" ";
		    clouds -> "texco=\"global\" ";
		    _ -> ""
		end,
	    println(F, "        <modulator texname=\"~s\" mode=\"~s\"~n"++
		    "         "++TexCo++"clipping=\"repeat\"~n"++
		    "         sizex=\"~.3f\" sizey=\"~.3f\" sizez=\"~.3f\">~n"++
		    "            <color value=\"~.3f\"/>~n"++
		    "            <specular value=\"~.3f\"/>~n"++
		    "            <hard value=\"~.3f\"/>~n"++
		    "            <transmission value=\"~.3f\"/>~n"++
		    "            <reflection value=\"~.3f\"/>~n"++
		    "            <normal value=\"~.3f\"/>~n"++
		    "        </modulator>", 
		    [Texname,format(Mode),SizeX,SizeY,SizeZ,
		     Color,Specular,HardValue,Transmission,Reflection,Normal])
    end.



export_rgb(F, Type, {R,G,B,_}) ->
    export_rgb(F, Type, {R,G,B});
export_rgb(F, Type, {R,G,B}) ->
    println(F, ["        <",format(Type)," r=\"",format(R),
		"\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).



%% Return object with arealight faces only
%%
export_object(F, NameStr, Mesh=#e3d_mesh{fs=Fs}, MatsGb) ->
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    MatPs = gb_trees:get(DefaultMaterial, MatsGb),
    export_object_1(F, NameStr, Mesh, DefaultMaterial, MatPs).

%% Count the number of subsequent equal elements in the list.
%% Returns list of {Count,Element}.
%%
count_equal([H|T]) ->
    count_equal(T, 1, H, []).
%%
count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).

export_object_1(F, NameStr, Mesh0=#e3d_mesh{he=He0}, DefaultMaterial, MatPs) ->
    OpenGL = proplists:get_value(opengl, MatPs),
    YafRay = proplists:get_value(?TAG, MatPs, []),
    UseHardness = proplists:get_value(use_hardness, YafRay, ?DEF_USE_HARDNESS),
    Caus = proplists:get_value(caus, YafRay, ?DEF_CAUS),
    IOR = proplists:get_value(ior, YafRay, ?DEF_IOR),
    AutosmoothAngle = 
	proplists:get_value(autosmooth_angle, YafRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafRay, 
				     if AutosmoothAngle == 0.0 -> false;
					true -> ?DEF_AUTOSMOOTH end),
    Shadow = proplists:get_value(shadow, YafRay, ?DEF_SHADOW),
    EmitRad = proplists:get_value(emit_rad, YafRay, ?DEF_EMIT_RAD),
    RecvRad = proplists:get_value(recv_rad, YafRay, ?DEF_RECV_RAD),
    %% Pre-process mesh
    Mesh1 = #e3d_mesh{} = 
	case {He0,UseHardness} of
	    {[_|_],true} ->
		io:format(?__(1,"Mesh ~s: slitting hard edges..."), [NameStr]),
		M1 = e3d_mesh:slit_hard_edges(Mesh0, [slit_end_vertices]),
		io:format(?__(2,"done")++"~n"),
		M1;
	    _ -> Mesh0
	end,
    io:format(?__(3,"Mesh ~s: triangulating..."), [NameStr]),
    #e3d_mesh{fs=Fs,vs=Vs,vc=Vc,tx=Tx} = e3d_mesh:triangulate(Mesh1),
    io:format(?__(4,"done")++"~n"),
    io:format(?__(5,"Mesh ~s: exporting..."), [NameStr]),
    %%
    println(F, "<object name=\"~s\" shader_name=\"~s\" shadow=\"~s\"~n"++
	    "        "++
	    case Caus of true -> "caus_IOR=\"~.10f\" ";
		false -> ""
	    end++"emit_rad=\"~s\" recv_rad=\"~s\">",
	    [NameStr,"w_"++format(DefaultMaterial),format(Shadow)|
	     case Caus of true -> [IOR];
		 false -> []
	     end]++[format(EmitRad),format(RecvRad)]),
    println(F, "    <attributes>"),
    case Caus of true ->
	    DefReflected = alpha(proplists:get_value(specular, OpenGL)),
	    Reflected = proplists:get_value(reflected, YafRay, DefReflected),
	    DefTransmitted = 
		def_transmitted(proplists:get_value(diffuse, OpenGL)),
	    Transmitted = 
		proplists:get_value(transmitted, YafRay, DefTransmitted),
	    export_rgb(F, caus_rcolor, Reflected),
	    export_rgb(F, caus_tcolor, Transmitted);
	false -> ok
    end,
    println(F, "    </attributes>"),
    case Autosmooth of
	false ->
	    println(F, "    <mesh>");
	true ->
	    println(F, "    <mesh autosmooth=\"~.3f\">", [AutosmoothAngle])
    end,
    println(F, "        <points>"),
    export_vertices(F, Vs),
    println(F, "        </points>~n"++
	    "        <faces>", []),
    export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc)),
    println(F, "        </faces>~n"++
	    "    </mesh>~n"++
	    "</object>", []),
    io:format(?__(6,"done")++"~n").



export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).



%% The coordinate system is rotated to make sunsky background
%% and environment images work as expected. 
%% It assumes X=South Y=East Z=Up in YafRay coordinates.
%% Hence Z=South, X=East, Y=Up in Wings coordinates.
%%
export_pos(F, Type, {X,Y,Z}) ->
    println(F, ["        <",format(Type)," x=\"",format(Z),
		"\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).



export_faces(_F, [], _DefMat, _TxT, _VColT) ->
    ok;
export_faces(F, [#e3d_face{vs=[A,B,C],vc=VCols,tx=Tx,mat=[Mat|_]}|T], 
	     DefaultMaterial, TxT, VColT) ->
    Shader =
	case Mat of
	    DefaultMaterial -> "";
	    _ -> [" shader_name=\"w_",format(Mat),"\""]
	end,
    UV = case {TxT,Tx} of
	     {{},[]} -> "";
	     {{},_} ->
		 io:format(?__(1,"WARNING! Face refers to non-existing "
			   "texture coordinates")++"~n"),
		 "";
	     {_,[]} ->
		 %%io:format("WARNING! Face missing texture coordinates")++"~n",
		 "";
	     {_,[Ta,Tb,Tc]} ->
		 {Ua,Va} = element(1+Ta, TxT),
		 {Ub,Vb} = element(1+Tb, TxT),
		 {Uc,Vc} = element(1+Tc, TxT),
		 [io_lib:nl(),"           u_a=\"",format(Ua),
		  "\" v_a=\"",format(-Va),"\"",
		  io_lib:nl(),"           u_b=\"",format(Ub),
		  "\" v_b=\"",format(-Vb),"\"",
		  io_lib:nl(),"           u_c=\"",format(Uc),
		  "\" v_c=\"",format(-Vc),"\""];
	     _ ->
		 io:format(?__(2,"WARNING! Face has ~w =/= 3 texture coordinates")++"~n",
			    [length(Tx)]),
		 ""
	 end,
    VCol = case {VColT,VCols} of
	       {{},[]} -> "";
	       {{},_} ->
		   io:format(?__(3,"WARNING! Face refers to non-existing "
			     "vertex colors")++"~n"),
		   "";
	       {_,[]} ->
		   %%io:format("WARNING! Face missing vertex colors~n"),
		   "";
	       {_,[VcA,VcB,VcC]} ->
		   {VcAr,VcAg,VcAb} = element(1+VcA, VColT),
		   {VcBr,VcBg,VcBb} = element(1+VcB, VColT),
		   {VcCr,VcCg,VcCb} = element(1+VcC, VColT),
		   [io_lib:nl(),"           vcol_a_r=\"",format(VcAr),
		    "\" vcol_a_g=\"",format(VcAg),
		    "\" vcol_a_b=\"",format(VcAb),"\"",
		    io_lib:nl(),"           vcol_b_r=\"",format(VcBr),
		    "\" vcol_b_g=\"",format(VcBg),
		    "\" vcol_b_b=\"",format(VcBb),"\"",
		    io_lib:nl(),"           vcol_c_r=\"",format(VcCr),
		    "\" vcol_c_g=\"",format(VcCg),
		    "\" vcol_c_b=\"",format(VcCb),"\""];
	       _ ->
		   io:format(?__(4,"WARNING! Face has ~w =/= 3 vertex colors")++"~n",
			     [length(VCols)]),
		   ""
	   end,
    println(F, ["        <f a=\"",format(A),
		"\" b=\"",format(B),"\" c=\"",format(C),"\"",
		Shader,UV,VCol,"/>"]),
    export_faces(F, T, DefaultMaterial, TxT, VColT).



export_light(F, Name, Ps) ->
    case proplists:get_value(visible, Ps, true) of
	true ->
	    OpenGL = proplists:get_value(opengl, Ps, []),
	    YafRay = proplists:get_value(?TAG, Ps, []),
	    Type = proplists:get_value(type, OpenGL, []),
	    export_light(F, Name, Type, OpenGL, YafRay);
	_ ->
	    undefined
    end.

export_light(F, Name, point, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafRay, ?DEF_POINT_TYPE),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" ", 
	    [Type,Name,Power]),
    case Type of
	pointlight ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    println(F,"       cast_shadows=\"~s\">", [format(CastShadows)]);
	softlight ->
	    Bias = proplists:get_value(bias, YafRay, ?DEF_BIAS),
	    Res = proplists:get_value(res, YafRay, ?DEF_RES),
	    Radius = proplists:get_value(radius, YafRay, ?DEF_RADIUS),
	    println(F,"       bias=\"~.6f\" res=\"~w\" radius=\"~w\">", 
		    [Bias,Res,Radius]);
	spherelight ->
	    ArealightRadius = 
		proplists:get_value(arealight_radius, YafRay, 
				    ?DEF_AREALIGHT_RADIUS),
	    ArealightSamples = 
		proplists:get_value(arealight_samples, YafRay, 
				    ?DEF_AREALIGHT_SAMPLES),
	    ArealightPsamples = 
		proplists:get_value(arealight_psamples, YafRay, 
				    ?DEF_AREALIGHT_PSAMPLES),
	    QmcMethod = 
		proplists:get_value(qmc_method, YafRay, ?DEF_QMC_METHOD),
	    Dummy = proplists:get_value(dummy, YafRay, ?DEF_DUMMY),
	    println(F,"       radius=\"~.10f\" dummy=\"~s\""++
		    if Dummy -> "";
		       true ->
			    "~n       samples=\"~w\" psamples=\"~w\" "
				"qmc_method=\"~w\""
		    end++">", 
		    [ArealightRadius,format(Dummy)]++
		    if Dummy -> [];
		       true -> [ArealightSamples,ArealightPsamples,
				QmcMethod]
		    end)
    end,
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),
    GlowIntensity = 
	proplists:get_value(glow_intensity, YafRay, ?DEF_GLOW_INTENSITY),
    case GlowIntensity of
	0.0 -> ok;
	_ ->
	    GlowOffset = 
		proplists:get_value(glow_offset, YafRay, ?DEF_GLOW_OFFSET),
	    GlowType = 
		proplists:get_value(glow_type, YafRay, ?DEF_GLOW_TYPE),
	    println(F, "    <glow_intensity value=\"~.10f\"/>~n"
		    "    <glow_offset value=\"~.10f\"/>~n"
		    "    <glow_type value=\"~w\"/>",
		    [GlowIntensity,GlowOffset,GlowType])
    end,
    println(F, "</light>"),
    undefined;
export_light(F, Name, infinite, OpenGL, YafRay) ->
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
%%%     case Bg of 
%%% 	sunsky ->
%%% 	    Bg;
%%% 	_ ->
    begin
	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    if Power > 0.0 ->
		    CastShadows = proplists:get_value(cast_shadows, YafRay, 
						      ?DEF_CAST_SHADOWS),
		    Position = proplists:get_value(position, OpenGL, 
						   {0.0,0.0,0.0}),
		    Diffuse = proplists:get_value(diffuse, OpenGL, 
						  {1.0,1.0,1.0,1.0}),
		    println(F,"<light type=\"sunlight\" name=\"~s\" "++
			    "power=\"~.3f\" cast_shadows=\"~s\">", 
			    [Name, Power,format(CastShadows)]),
		    export_pos(F, from, Position),
		    export_rgb(F, color, Diffuse),
		    println(F, "</light>");
	       true -> ok
	    end,
	    Bg
    end;
export_light(F, Name, spot, OpenGL, YafRay) ->
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafRay, ?DEF_SPOT_TYPE),
    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\" ", 
	    [Type,Name,Power]),
    case Type of
	spotlight ->
	    CastShadows = 
		proplists:get_value(cast_shadows, YafRay, ?DEF_CAST_SHADOWS),
	    SpotExponent = 
		proplists:get_value(spot_exponent, OpenGL, ?DEF_SPOT_EXPONENT),
	    Blend = proplists:get_value(blend, YafRay, ?DEF_BLEND),
	    print(F, "       cast_shadows=\"~s\" size=\"~.3f\"~n"++
		  "       beam_falloff=\"~.10f\" blend=\"~.3f\"", 
		  [format(CastShadows), ConeAngle, SpotExponent, Blend]),
	    case proplists:get_value(halo, YafRay, ?DEF_HALO) of
		false -> 
		    println(F, ">");
		true ->
		    HaloRes = 
			proplists:get_value(halo_res, YafRay, ?DEF_HALO_RES),
		    HaloSamples = 
			proplists:get_value(halo_samples, YafRay, HaloRes),
		    HaloShadowSamples = 
			proplists:get_value(halo_shadow_samples, YafRay, 
					    HaloRes),
		    HaloBlur = 
			proplists:get_value(halo_blur, YafRay, ?DEF_HALO_BLUR),
		    HaloShadowBlur = 
			proplists:get_value(halo_shadow_blur, YafRay, 
					    ?DEF_HALO_SHADOW_BLUR),
		    HaloFogDensity = 
			proplists:get_value(halo_fog_density, YafRay, 
					    ?DEF_HALO_FOG_DENSITY),
		    HaloFogColor = 
			proplists:get_value(halo_fog_color, YafRay, 
					    ?DEF_HALO_FOG_COLOR),
		    println(F, "~n       halo=\"~s\" res=\"~w\" "
			    "samples=\"~w\" shadow_samples=\"~w\"~n"
			    "       halo_blur=\"~.10f\" "
			    "shadow_blur=\"~.10f\"~n"
			    "       fog_density=\"~.10f\">",
			  [format(true),HaloRes,HaloSamples,HaloShadowSamples,
			   HaloBlur,HaloShadowBlur,HaloFogDensity]),
		    export_rgb(F, fog, HaloFogColor)
	    end;
	photonlight ->
	    Mode = proplists:get_value(mode, YafRay, ?DEF_MODE),
	    Photons = proplists:get_value(photons, YafRay, ?DEF_PHOTONS),
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    Fixedradius = 
		proplists:get_value(fixedradius, YafRay, ?DEF_FIXEDRADIUS),
	    Search = proplists:get_value(search, YafRay, ?DEF_SEARCH),
	    Mindepth = proplists:get_value(mindepth, YafRay, ?DEF_MINDEPTH),
	    Cluster = proplists:get_value(cluster, YafRay, ?DEF_CLUSTER),
	    UseQMC = proplists:get_value(use_QMC, YafRay, ?DEF_USE_QMC),
	    case Mode of
		diffuse ->
		    println(F, "       mode=\"diffuse\"");
		_ ->
		    ok
	    end,
	    println(F, "       angle=\"~.3f\" photons=\"~w\" depth=\"\~w\"~n"++
		    "       fixedradius=\"~.10f\" search=\"~w\"~n"++
		    "       mindepth=\"~w\" cluster=\"~.10f\" use_QMC=\"~s\">",
		    [ConeAngle,Photons,Depth,
		     Fixedradius,Search,Mindepth,Cluster,format(UseQMC)])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>"),
    undefined;
export_light(F, Name, ambient, OpenGL, YafRay) ->
    Type = proplists:get_value(type, YafRay, ?DEF_AMBIENT_TYPE),
    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
    case Type of
	hemilight when Power > 0.0 ->
	    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\"", 
		    [Type,Name,Power]),
	    UseQMC = proplists:get_value(use_QMC, YafRay, 
					 ?DEF_USE_QMC),
	    Ambient = proplists:get_value(ambient, OpenGL, 
					  ?DEF_BACKGROUND_COLOR),
	    Samples = proplists:get_value(samples, YafRay, 
					  ?DEF_SAMPLES),
	    BgEnlight = proplists:get_value(background_enlight, YafRay, false),
	    println(F, "       use_QMC=\"~s\" samples=\"~w\">", 
		    [format(UseQMC),Samples]),
	    case proplists:get_value(use_maxdistance, YafRay, 
				     ?DEF_USE_MAXDISTANCE) of
		true ->
		    Maxdistance = proplists:get_value(maxdistance, YafRay,
						      ?DEF_MAXDISTANCE),
		    println(F, "    <maxdistance value=\"~.10f\"/>",
			    [Maxdistance]);
		false -> ok
	    end,
	    if BgEnlight, Bg == 'HDRI';
	       BgEnlight, Bg == image -> ok; % Skip color tag
	       true -> export_rgb(F, color, Ambient)
	    end,
	    println(F, "</light>"),
	    Bg;
	hemilight -> Bg;
	pathlight when Power > 0.0 ->
	    println(F,"<light type=\"~w\" name=\"~s\" power=\"~.3f\"", 
		    [Type,Name,Power]),
	    UseQMC = proplists:get_value(use_QMC, YafRay, 
					 ?DEF_USE_QMC),
	    Depth = proplists:get_value(depth, YafRay, ?DEF_DEPTH),
	    CausDepth = proplists:get_value(caus_depth, YafRay, ?DEF_CAUS_DEPTH),
	    Direct = proplists:get_value(direct, YafRay, ?DEF_DIRECT),
	    Samples = proplists:get_value(samples, YafRay, 
					  ?DEF_SAMPLES),
	    print(F, "       use_QMC=\"~s\" samples=\"~w\" "
		  "depth=\"~w\" caus_depth=\"~w\"", 
		  [format(UseQMC),Samples,Depth,CausDepth]),
	    case Direct of
		true ->
		    print(F, " direct=\"on\"");
		false ->
		    case proplists:get_value(cache, YafRay, ?DEF_CACHE) of
			true ->
			    CacheSize = 
				proplists:get_value(cache_size, YafRay, 
						    ?DEF_CACHE_SIZE),
			    AngleThreshold = 
				proplists:get_value(angle_threshold, YafRay, 
						    ?DEF_ANGLE_THRESHOLD),
			    ShadowThreshold = 
				proplists:get_value(shadow_threshold, YafRay, 
						    ?DEF_SHADOW_THRESHOLD),
			    Gradient = 
				proplists:get_value(gradient, YafRay, 
						    ?DEF_GRADIENT),
			    ShowSamples = 
				proplists:get_value(show_samples, YafRay, 
						    ?DEF_SHOW_SAMPLES),
			    Search = 
				proplists:get_value(search, YafRay, ?DEF_SEARCH),
			    print(F, " cache=\"on\"~n"
				  "       cache_size=\"~.10f\" "
				  "angle_threshold=\"~.10f\"~n"
				  "       shadow_threshold=\"~.10f\" "
				  "gradient=\"~s\"~n"
				  "       show_samples=\"~s\" search=\"~w\"",
				  [CacheSize,AngleThreshold,
				   ShadowThreshold,format(Gradient),
				   format(ShowSamples),Search]);
			false -> ok
		    end
	    end,
	    println(F, ">"),
	    PathlightMode = proplists:get_value(pathlight_mode, YafRay,
						?DEF_PATHLIGHT_MODE),
	    case PathlightMode of
		undefined ->
		    ok;
		_ ->
		    println(F, "    <mode value=\"~s\"/>", 
			    [format(PathlightMode)])
	    end,
	    case proplists:get_value(use_maxdistance, YafRay, 
				     ?DEF_USE_MAXDISTANCE) of
		true ->
		    Maxdistance = proplists:get_value(maxdistance, YafRay,
						      ?DEF_MAXDISTANCE),
		    println(F, "    <maxdistance value=\"~.10f\"/>",
			    [Maxdistance]);
		false -> ok
	    end,
	    println(F, "</light>"),
	    Bg;
	pathlight -> Bg;
	globalphotonlight ->
	    println(F,"<light type=\"~w\" name=\"~s\"", [Type,Name]),
	    GplPhotons = proplists:get_value(
			   globalphotonlight_photons, YafRay,
			   ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
	    GplRadius = proplists:get_value(
			  globalphotonlight_radius, YafRay,
			  ?DEF_GLOBALPHOTONLIGHT_RADIUS),
	    GplDepth = proplists:get_value(
			 globalphotonlight_depth, YafRay,
			 ?DEF_GLOBALPHOTONLIGHT_DEPTH),
	    GplSearch = proplists:get_value(
			  globalphotonlight_search, YafRay,
			  ?DEF_GLOBALPHOTONLIGHT_SEARCH),
	    println(F,"       photons=\"~w\" radius=\"~.3f\" "
		    "depth=\"~w\" search=\"~w\">", 
		    [GplPhotons,GplRadius,GplDepth,GplSearch]),
	    println(F, "</light>"),
	    Bg
    end;
export_light(F, Name, area, OpenGL, YafRay) ->
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    Power = proplists:get_value(power, YafRay, ?DEF_ATTN_POWER),
    Samples = proplists:get_value(arealight_samples, YafRay, 
					   ?DEF_AREALIGHT_SAMPLES),
    Psamples = proplists:get_value(arealight_psamples, YafRay, 
					    ?DEF_AREALIGHT_PSAMPLES),
    Dummy = proplists:get_value(dummy, YafRay, ?DEF_DUMMY),
    Fs = foldr(fun (Face, Acc) -> 
			e3d_mesh:quadrangulate_face(Face, Vs)++Acc
		end, [], Fs0),
    As = e3d_mesh:face_areas(Fs, Vs),
    Area = foldl(fun (A, Acc) -> A+Acc end, 0.0, As),
    AFs = zip_lists(As, Fs),
    foldl(
      fun ({Af,#e3d_face{vs=VsF}}, I) ->
	      case catch Power*Af/Area of
		  {'EXIT',{badarith,_}} -> I;
		  Pwr ->
		      NameI = Name++"_"++integer_to_list(I),
		      [A,B,C,D] = quadrangle_vertices(VsF, VsT),
		      println(F, "<light type=\"arealight\" "
			      "name=\"~s\" power=\"~.3f\"~n"
			      "       dummy=\"~s\""++
			      if Dummy -> "";
				 true ->
				      " samples=\"~w\" psamples=\"~w\""
			      end++">", 
			      [NameI,Pwr,format(Dummy)]++
			      if Dummy -> [];
				 true -> [Samples,Psamples]
			      end),
		      export_rgb(F, color, Color),
		      export_pos(F, a, A),
		      export_pos(F, b, B),
		      export_pos(F, c, C),
		      export_pos(F, d, D),
		      println(F, "</light>"),
		      I+1
	      end
      end, 1, AFs),
    undefined;
export_light(_F, Name, Type, _OpenGL, _YafRay) ->
    io:format(?__(1,"WARNING: Ignoring unknown light \"~s\" type: ~p")++"~n", 
	      [Name, format(Type)]),
    undefined.

%% Cut the longest edge of a triangle in half to make it a quad.
%% Lookup vertex positions.
%%
quadrangle_vertices([V1,V2,V3], VsT) -> 
    P1 = element(V1+1, VsT),
    P2 = element(V2+1, VsT),
    P3 = element(V3+1, VsT),
    [L12,L23,L31] = 
	[e3d_vec:dot(L, L) || 
	    L <- [e3d_vec:sub(P1, P2),e3d_vec:sub(P2, P3),
		  e3d_vec:sub(P3, P1)]],
    if L23 > L31 ->
	    if L12 > L23 -> [P1,e3d_vec:average(P1, P2),P2,P3];
	       true -> [P1,P2,e3d_vec:average(P2, P3),P3]
	    end;
       true -> [P1,P2,P3,e3d_vec:average(P3, P1)]
    end;
quadrangle_vertices([V1,V2,V3,V4], VsT) -> 
    [element(V1+1, VsT),element(V2+1, VsT),
     element(V3+1, VsT),element(V4+1, VsT)].



export_camera(F, Name, Attr) ->
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} = 
	proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Ortho = proplists:get_value(ortho, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% YafRay focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5*Ro)),
    Aperture = proplists:get_value(aperture, Attr),
    println(F, "<camera name=\"~s\" "++
	    "resx=\"~w\" resy=\"~w\" focal=\"~.10f\""++
	    if Aperture > 0.0 ->
		    "~n        dof_distance=\"~.10f\" aperture=\"~.10f\""
			"~n        use_qmc=\"~s\" bokeh_type=\"~s\""
			"~n        bokeh_bias=\"~s\" bokeh_rotation=\"~.10f\"";
	       true -> ""
	    end++
	    case Ortho of
		false -> ">";
		true  -> "~n        type=\"ortho\">";
		_     -> "~n        type=\"~s\">"
	    end,
	    [Name,Width,Height,FocalDist]++
	    if Aperture > 0.0 ->
		    [e3d_vec:len(Dir),
		     Aperture,
		     format(proplists:get_value(bokeh_use_QMC, Attr)),
		     format(proplists:get_value(bokeh_type, Attr)),
		     format(proplists:get_value(bokeh_bias, Attr)),
		     proplists:get_value(bokeh_rotation, Attr)];
	       true -> []
	    end++
	    case Ortho of
	       false -> [];
	       true  -> [];
	       _     -> [format(Ortho)]
	    end),
    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.



export_background(F, Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafRay = proplists:get_value(?TAG, Ps, []),
    Bg = proplists:get_value(background, YafRay, ?DEF_BACKGROUND),
    case Bg of
	constant ->

	    print(F, "<background type=\"~s\" name=\"~s\"",
		  [format(Bg),Name]),

	    println(F, ">"),
	    BgColor = proplists:get_value(background_color, YafRay, 
					  ?DEF_BACKGROUND_COLOR),
	    export_rgb(F, color, BgColor);
	sunsky ->
%%% 	    Power = proplists:get_value(power, YafRay, ?DEF_POWER),
	    Turbidity = proplists:get_value(turbidity, YafRay, ?DEF_TURBIDITY),
	    A_var = proplists:get_value(a_var, YafRay, ?DEF_SUNSKY_VAR),
	    B_var = proplists:get_value(b_var, YafRay, ?DEF_SUNSKY_VAR),
	    C_var = proplists:get_value(c_var, YafRay, ?DEF_SUNSKY_VAR),
	    D_var = proplists:get_value(d_var, YafRay, ?DEF_SUNSKY_VAR),
	    E_var = proplists:get_value(e_var, YafRay, ?DEF_SUNSKY_VAR),
	    Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

	    print(F, "<background type=\"~s\" name=\"~s\"",
		  [format(Bg),Name]),

	    println(F, "~n            turbidity=\"~.3f\" a_var=\"~.3f\"~n"
		    "            b_var=\"~.3f\" c_var=\"~.3f\"~n"
		    "            d_var=\"~.3f\" e_var=\"~.3f\" "
		    "add_sun=\"off\">",
		    [Turbidity,A_var,B_var,C_var,D_var,E_var]),
%%% 	    if Power > 0.0 ->
%%% 		    println(F, "~n            add_sun=\"on\" "
%%% 			    "sun_power=\"~.3f\">",
%%% 			    [Power]);
%%% 	       true ->
%%% 		    println(F, ">")
%%% 	    end,
	    export_pos(F, from, Position);
	'HDRI' ->
	    BgFname = proplists:get_value(background_filename_HDRI, YafRay, 
					  ?DEF_BACKGROUND_FILENAME),
	    BgExpAdj = proplists:get_value(background_exposure_adjust, YafRay, 
					   ?DEF_BACKGROUND_EXPOSURE_ADJUST),
	    BgMapping = proplists:get_value(background_mapping, YafRay, 
					    ?DEF_BACKGROUND_MAPPING),
	    BgPrefilter = proplists:get_value(background_prefilter, YafRay,
					    ?DEF_BACKGROUND_PREFILTER),
	    print(F, "<background type=\"image\" name=\"~s\"",
	  [Name]),

	    println(F, "~n            exposure_adjust=\"~w\" mapping=\"~s\""
		    " prefilter=\"~s\">",
		    [BgExpAdj,format(BgMapping),format(BgPrefilter)]),
	    println(F, "    <filename value=\"~s\" />", [BgFname]);
	image ->
	    BgFname = proplists:get_value(background_filename_image, YafRay,
					  ?DEF_BACKGROUND_FILENAME),
	    BgPower = proplists:get_value(background_power, YafRay, 
					   ?DEF_BACKGROUND_POWER),
	    BgPrefilter = proplists:get_value(background_prefilter, YafRay,
					    ?DEF_BACKGROUND_PREFILTER),
	    print(F, "<background type=\"~s\" name=\"~s\"",
		  [format(Bg),Name]),

	    println(F, " power=\"~.3f\" prefilter=\"~s\">", [BgPower,format(BgPrefilter)]),
	    println(F, "    <filename value=\"~s\" />", [BgFname])
    end,
    println(F, "</background>").



export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    AA_passes = proplists:get_value(aa_passes, Attr),
    AA_minsamples = proplists:get_value(aa_minsamples, Attr),
    AA_pixelwidth = proplists:get_value(aa_pixelwidth, Attr),
    AA_threshold = proplists:get_value(aa_threshold, Attr),
    AA_jitterfirst = proplists:get_value(aa_jitterfirst, Attr),
    ClampRGB = proplists:get_value(clamp_rgb, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    Bias = proplists:get_value(bias, Attr),
    SaveAlpha = proplists:get_value(save_alpha, Attr),
    Gamma = proplists:get_value(gamma, Attr),
    Exposure = proplists:get_value(exposure, Attr),
    FogColor = proplists:get_value(fog_color, Attr),
    FogDensity = proplists:get_value(fog_density, Attr),
    RenderFormat = proplists:get_value(render_format, Attr),
    ExrFlagFloat = proplists:get_value(exr_flag_float, Attr),
    ExrFlagZbuf = proplists:get_value(exr_flag_zbuf, Attr),
    ExrFlagCompression = proplists:get_value(exr_flag_compression, Attr),
    ExrFlags =
	case RenderFormat of
	    exr ->
		[if ExrFlagFloat -> "float "; true -> "" end,
		 if ExrFlagZbuf -> "zbuf "; true -> "" end,
		 format(ExrFlagCompression)];
	    _ -> ""
	end,
    println(F, "<render camera_name=\"~s\" "
	    "AA_passes=\"~w\" raydepth=\"~w\"~n"
	    "        bias=\"~.10f\" AA_threshold=\"~.10f\"~n"
	    "        AA_minsamples=\"~w\" AA_pixelwidth=\"~.10f\"~n"++
	    case SaveAlpha of
		premultiply -> 
		    "        alpha_premultiply=\"on\"~n";
		backgroundmask -> 
		    "        alpha_backgroundmask=\"on\"~n";
		_ -> ""
	    end++
	    "        AA_jitterfirst=\"~s\" clamp_rgb=\"~s\">~n"
	    "    <background_name value=\"~s\"/>~n"++
	    case RenderFormat of
		tga -> "";
		_   -> "    <output_type value=\"~s\"/>~n"
	    end++
	    case RenderFormat of
		exr -> "    <exr_flags value=\"~s\"/>~n";
		_   -> ""
	    end++
	    "    <outfile value=\"~s\"/>~n"
	    "    <indirect_samples value=\"0\"/>~n"
	    "    <indirect_power value=\"1.0\"/>~n"
	    "    <exposure value=\"~.10f\"/>~n"++
	    case SaveAlpha of
		false -> "";
		_ ->
		    "    <save_alpha value=\"on\"/>~n"
	    end++
	    "    <gamma value=\"~.10f\"/>~n"
	    "    <fog_density value=\"~.10f\"/>",
	    [CameraName,AA_passes,Raydepth,Bias,AA_threshold,
	     AA_minsamples,AA_pixelwidth,format(AA_jitterfirst),
	     format(ClampRGB),BackgroundName]++
	    case RenderFormat of
		tga -> [];
		_   -> [format(RenderFormat)]
	    end++
	    case RenderFormat of
		exr -> [ExrFlags];
		_   -> []
	    end++
	    [Outfile,Exposure,Gamma,FogDensity]),
    export_rgb(F, fog_color, FogColor),
    println(F, "</render>").



%%% Noisy file output functions. Fail if anything goes wrong.
%%%

open(Filename, export) ->
    case file:open(Filename, [write,raw,delayed_write]) of
	{ok, F} ->
	    F;
	Error ->
	    error(Error, [Filename, export])
    end.

println(F) ->
    println(F, "").

print(F, DeepString) ->
    case file:write(F, DeepString) of
	ok ->
	    ok;
	Error ->
	    error(Error, [F,DeepString])
    end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
	ok ->
	    ok;
	Error ->
	    error(Error, [F,DeepString])
    end.

print(F, Format, Args) ->
    case file:write(F, io_lib:format(Format, Args)) of
	ok ->
	    ok;
	Error ->
	    error(Error, [F,Format,Args])
    end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
	ok ->
	    ok;
	Error ->
	    error(Error, [F,Format,Args])
    end.

close(F) ->
    case file:close(F) of
	ok ->
	    ok;
	Error ->
	    error(Error, [F])
    end.



%% Convert certain terms to printable strings in a
%% hopefully efficient way.

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->
	    [$-,integer_to_list(I)|format_decimals(D)];
       true ->
	    [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->
    integer_to_list(I);
format(true) ->
    "on";
format(false) ->
    "off";
format(A) when is_atom(A) ->
    atom_to_list(A);
format(L) when is_list(L) ->
    L.

format_decimals(F) when is_float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(0.0) ->
    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(0.0) ->
    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
	    [$0,(I+$0)|format_decimals_3(D)];
       true ->
	    [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(0.0) ->
    [];
format_decimals_3(F) when is_float(F) ->
    G = 1000.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
	    [$0,$0,(I+$0)|format_decimals_4(D)];
       I < 100 ->
	    [$0,integer_to_list(I)|format_decimals_4(D)];
       true ->
	    [integer_to_list(I)|format_decimals_4(D)]
    end.

format_decimals_4(0.0) ->
    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
	    if I < 10 ->
		    [$0,$0,$0,(I+$0)];
	       true ->
		    [$0,$0|integer_to_list(I)]
	    end;
       true ->
	    if I < 1000 ->
		    [$0|integer_to_list(I)];
	       true ->
		    integer_to_list(I)
	    end
    end.

    

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



%% Split a list into a list of length Pos, and the tail
%%
split_list(List, Pos) when is_list(List), is_integer(Pos), Pos >= 0 ->
    case split_list1(List, Pos, []) of
	{_,_}=Result ->
	    Result;
	Error ->
	    error(Error, [List, Pos])
    end.
%%
split_list1(List, 0, Head) ->
    {lists:reverse(Head),List};
split_list1([], _Pos, _) ->
    badarg;
split_list1([H|T], Pos, Head) ->
    split_list1(T, Pos-1, [H|Head]).

%% Zip lists together into a list of tuples
%%
zip_lists([], []) -> [];
zip_lists([H1|T1], [H2|T2]) -> [{H1,H2}|zip_lists(T1, T2)].



%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end, List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%% 	true -> filter2_1(Pred, T, [H|True], False);
%%% 	false -> filter2_1(Pred, T, True, [H|False])
%%%     end.


-ifdef(print_mesh_1).
print_mesh(#e3d_mesh{type=T,vs=Vs,vc=Vc,tx=Tx,ns=Ns,fs=Fs,he=He,matrix=M}) ->
    io:format("#e3d_mesh{type=~p,~nvs=~p,~nvc=~p,~ntx=~p,~nns=~p,~nfs=~p,~n"
	      "he=~p,~nmatrix=~p}.~n",
	      [T,Vs,Vc,Tx,Ns,Fs,He,M]).
-endif.

help_button(Subject) ->
    Title = help(title, Subject),
    TextFun = fun () -> help(text, Subject) end,
    {help,Title,TextFun}.

help(title, {material_dialog,object}) ->
    ?__(6,"YafRay Material Properties: Object Parameters");
help(text, {material_dialog,object}) ->
    [?__(7,"Object Parameters are applied to whole objects, namely those "
      "that have this material on a majority of their faces."),
     ?__(8,"Mapping to YafRay object parameters:"),
     ?__(9,"Cast Shadow -> 'shadow'."),
     ?__(10,"Emit Rad -> 'emit_rad' -> Emit Radiosity."),
     ?__(11,"Recv Rad -> 'recv_rad' -> Receive Radiosity."),
     ?__(12,"Use Edge Hardness -> Emulate hard edges by "
      "slitting the object mesh along hard edges."),
     ?__(13,"Autosmooth Angle -> 'autosmooth'."),
     ?__(14,"A Photon Light must be present for Emit Rad and Recv Rad "
     "to have an affect. Set Fresnel Parameters to add Caustics.")];
help(title, {material_dialog,fresnel}) ->
    ?__(15,"YafRay Material Properties: Fresnel Parameters");
help(text, {material_dialog,fresnel}) ->
    [?__(16,"Fresnel Parameters affect how rays reflect off and refract in "
      "glass-like materials. This is a different light model than the "
      "OpenGL (Diffuse,Specular,Shininess) model and they do not often "
      "go well together. "
      "A Photon Light must be present to produce Caustics."),
     ?__(17,"Mapping to YafRay shader parameters:"),
     ?__(18,"Index Of Refraction -> 'ior' -> 1.5 for Glass/Caustics."),
     ?__(19,"Total Internal Reflection -> 'tir' -> Enable for Glass."),
     ?__(20,"Minimum Reflection -> 'min_refle' -> 1.0 for Metal."),
     ?__(21,"Reflected -> 'reflected' -> Reflective Caustics."),
     ?__(22,"Transmitted -> 'transmitted' -> Glass/Refractive Caustics."),
     ?__(23,"Set Default -> Sets 'transmitted' to Diffuse * (1 - Opacity). "
      "This makes a semi-transparent object in OpenGL look the same in "
      "YafRay provided that Index Of Refraction is 1.1 minimum."),
     ?__(24,"Grazing Angle Colors -> Use the secondary Reflected and Transmitted "
      "colors following that show from grazing angles of the material. "
      "For a glass with green edges set Transmitted to white and "
      "Grazing Angle Transmitted to green."),
     ?__(25,"Absorption -> Sets the desired color for white light travelling "
      "the given distance through the material.")];
%%
help(title, light_dialog) ->
    ?__(26,"YafRay Light Properties");
help(text, light_dialog) ->
    [?__(27,"OpenGL properties that map to YafRay light parameters are:"),
     ?__(28,"Diffuse -> 'color'"),
     ?__(29,"All other OpenGl properties are ignored, particulary the "
      "Attenuation properties."),
     ?__(30,"Spotlight set to Photonlight is used to produce Caustics or Radiosity. "
      "Photonlight set to Caustic for Caustics. "
      "Photonlight set to Diffuse for Radiosity. "),
     ?__(31,"The Enlight checkbox in a Hemilight with an image background "
      "activates the background image as ambient light source instead of "
      "the defined ambient color by excluding the 'color' tag "
      "from the Hemilight."),
     ?__(32,"Note: For a YafRay Global Photon Light (one of the Ambient lights) - "
      "the Power parameter is ignored")];
help(title, pref_dialog) ->
    ?__(33,"YafRay Options");
help(text, pref_dialog) ->
    [?__(34,"These are user preferences for the YafRay exporter plugin"),
     ?__(35,"Automatic Dialogs: ")
     ++wings_help:cmd([?__(36,"File"),?__(37,"Export"),?__(38,"YafRay")])++", "
     ++wings_help:cmd([?__(39,"File"),?__(40,"Export Selected"),?__(41,"YafRay")])++" "++?__(42,"and")++" "
     ++wings_help:cmd([?__(43,"File"),?__(44,"Render"),?__(45,"YafRay")])++" "++
     ?__(46,"are enabled if the rendering executable is found (in the path), "
     "or if the rendering executable is specified with an absolute path."),
     %%
     ?__(47,"Disabled Dialogs:")++" "
     ++wings_help:cmd([?__(48,"File"),?__(49,"Export"),?__(50,"YafRay")])++", "
     ++wings_help:cmd([?__(51,"File"),?__(52,"Export Selected"),?__(53,"YafRay")])++" "++?__(54,"and")++" "
     ++wings_help:cmd([?__(55,"File"),?__(56,"Render"),?__(57,"YafRay")])++" "++
     ?__(58,"are disabled."),
     %%
     ?__(59,"Enabled Dialogs:")++" "
     ++wings_help:cmd([?__(60,"File"),?__(61,"Export"),?__(62,"YafRay")])++" "++?__(63,"and")++" "
     ++wings_help:cmd([?__(64,"File"),?__(65,"Export Selected"),?__(66,"YafRay")])++" "++
     ?__(67,"are always enabled, but")++" "
     ++wings_help:cmd([?__(68,"File"),?__(69,"Render"),?__(70,"YafRay")])++" "++
     ?__(71,"is still as for \"Automatic Dialogs\"."),
     %%
     ?__(72,"Executable: The rendering command for the YafRay "
      "raytrace renderer (normally 'yafray') that is supposed to be found "
      "in the executables search path; "
      "or, the absolute path of that executable."),
     ?__(73,"Options: Rendering command line options to be inserted "
      "between the executable and the .xml filename.")].
