%%
%%  wpc_toxic.erl
%%
%%     Toxic Plugin User Interface and Exporter.
%%
%%  Copyright (c) 2004 Dan Gudmundsson, Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_toxic.erl,v 1.21 2006/01/08 18:02:00 giniu Exp $
%%

-module(wpc_toxic).

-export([init/0,menu/2,dialog/2,command/2]).

-include_lib("kernel/include/file.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,keydelete/3,
		foreach/2,foldl/3,foldr/3]).



-define(TAG, toxic).
-define(TAG_RENDER, toxic_render).
-define(PI, 3.1415926536).
%%% Default values

-define(DEF_DIALOGS, auto).
-define(DEF_RENDERER, "toxic").
-define(DEF_OPTIONS, "").
-define(DEF_EXPORT_MESH, true).

-define(DEF_SUBDIVISIONS, 0).
-define(DEF_GAMMA, 2.0).

%% Render Opts
-define(DEF_DIRECT_LIGHT, true).
-define(DEF_DIRECT_LIGHT_MIN, true).
-define(DEF_INDIRECT_LIGHT, false).
-define(DEF_INDIRECT_LIGHT_MIN, true).
-define(DEF_SPECULAR_REFL, false).
-define(DEF_SPECULAR_REFL_MIN, true).
-define(DEF_CAUSTICS, false).
-define(DEF_CAUSTICS_MIN, true).
-define(DEF_WIDTH, 100).
-define(DEF_HEIGHT, 100).

-define(DEF_SAMPLING, whitted).
-define(DEF_SSTYPE,   stratified).
-define(DEF_SSRANDOM, 8).
-define(DEF_SSWIDTH,  8).
-define(DEF_SSHEIGHT, 8).
-define(DEF_WACONTRAST,  0.025).
-define(DEF_WAMAX, 3).

-define(DEF_IDLNOPHOTONS, 1000000).
-define(DEF_IDLMAXPHOTONS,500).
-define(DEF_IDLMAXDIST,   1000000).

-define(DEF_RADPRE_COMP, true).
-define(DEF_RADPRE_SPACING, 4).
-define(DEF_RADPRE_SEARCH,  0.1).

-define(DEF_RADPRIMARY, false).
-define(DEF_RADSECONDARY, false).
-define(DEF_RADSECDIST, 0.01).

-define(DEF_SPECDEPTH,  3).

%% Shader
-define(DEF_BDF, lambertian).
-define(DEF_EDF, false).
-define(DEF_EDF_RADIANCE, 5).

%% Arealight
-define(DEF_AREALIGHT, false).
-define(DEF_AREALIGHT_SAMPLES, 50).
-define(DEF_AREALIGHT_PSAMPLES, 0).
-define(DEF_DUMMY, false).

%% Light
-define(DEF_POWER, 10.0).
-define(DEF_ATTN_POWER, 10.0).

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
    pref_edit(St);
command(_Spec, _St) ->
    %%    erlang:display({?MODULE,?LINE,Spec}),
    next.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    maybe_append(edit, Dialog, material_dialog(Name, Mat));
dialog({material_editor_result,Name,Mat}, Res) ->
    case is_plugin_active(edit) of
	false ->    {Mat,Res};
	_ ->     material_result(Name, Mat, Res)
    end;
dialog({light_editor_setup,Name,Ps}, Dialog) ->
    maybe_append(edit, Dialog, light_dialog(Name, Ps));
dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
	false ->   {Ps0,Res};
	_ ->     light_result(Name, Ps0, Res)
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
	    absolute -> Renderer;
	    _ ->
		case wings_job:find_executable(Renderer) of
		    false -> false;
		    Path ->  Path
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
    case is_plugin_active(Condition) of
	false ->    Menu;
	_ ->     Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
	export ->   get_var(dialogs);
	edit ->     get_var(dialogs);
	render ->   get_var(renderer);
	_ ->     false
    end.

menu_entry(render) ->
    {value,{png,Ext,_}} = lists:keysearch(png, 1, wings_job:render_formats()),
    [{"Toxic ("++Ext++")",?TAG,[option]}];
menu_entry(export) ->    [{"Toxic (.xml)",?TAG,[option]}];
menu_entry(pref) ->      [{"Toxic",?TAG}].

command_file(render, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    case get_var(rendering) of
	false ->
	    do_export(export, props(render), [{?TAG_RENDER,true}|Attr], St);
	true ->
	    wpa:error(?__(1,"Already rendering."))
    end;
command_file(render, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(2,"Toxic Render Options"), export_dialog(render),
	       fun(Attr) -> {file,{render,{?TAG,Attr}}} end);
command_file(Op, Attr, St) when is_list(Attr) ->
    set_pref(Attr),
    do_export(Op, props(Op), Attr, St);
command_file(Op, Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(3,"Toxic Export Options"), export_dialog(Op),
	       fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

props(render) ->
    {value,{png,Ext,Desc}} =
	lists:keysearch(png, 1, wings_job:render_formats()),
    [{title,?__(1,"Render")},{ext,Ext},{ext_desc,Desc}];
props(export) ->
    [{title,?__(2,"Export")},{ext,".xml"},{ext_desc,?__(3,"Toxic File")}];
props(export_selected) ->
    [{title,?__(4,"Export Selected")},{ext,".xml"},{ext_desc,?__(5,"Toxic File")}].

-record(camera_info, {pos,dir,up,fov,origin,distance,azimuth,
		      elevation,pan_x,pan_y}).

attr(St, Attr) ->
    [{Pos,Dir,Up},Fov,Origin,Dist,Az,El,{PanX,PanY}] = 
	wpa:camera_info([pos_dir_up,fov,aim,distance_to_aim,
			 azimuth,elevation,tracking]),
%%    CurrSz = wings_wm:win_size(),
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov,
			      origin=Origin,distance=Dist,azimuth=Az,
			      elevation=El,pan_x=PanX,pan_y=PanY},
    [CameraInfo,{lights,wpa:lights(St)}|Attr].

do_export(Op, Props0, Attr, St0) ->    
    case pget(export_mesh,Attr) of
	true -> 
	    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
	    Props = [{subdivisions,SubDiv}|Props0],
	    %% Freeze virtual mirrors.
	    Shapes0 = gb_trees:to_list(St0#st.shapes),
	    Shapes = [{Id, wpa:vm_freeze(We)} || {Id, We} <- Shapes0],
	    St = St0#st{shapes = gb_trees:from_orddict(Shapes)},
	    wpa:Op(Props, fun_export_2(attr(St, Attr)), St);
	false ->
	    wpa:Op(Props0, fun_export_2(attr(St0, Attr)), St0)
    end.

fun_export_2(Attr) ->
    fun(Filename, Contents) ->
	    case catch export(Attr, Filename, Contents) of
		ok ->
		    ok;
		Error ->
		    io:format(?__(1,"ERROR: Failed to export")++":~n~p~n", [Error]),
		    {error,?__(2,"Failed to export")}
	    end
    end.



%%% Dialogues and results
%%%

material_dialog(_Name, Mat) ->
    Toxic   = proplists:get_value(?TAG, Mat, []),
    Min     = pget(shader_minimized, Toxic, true),
    BDF     = pget(bdf, Toxic, ?DEF_BDF),
%    EDF     = pget(edf, Toxic, ?DEF_EDF),
%    Radiant = pget(radiant, Toxic, if EDF == false -> 0;
%				      true -> ?DEF_EDF_RADIANCE end),
    [{vframe,
      [{hframe,
	[{label, ?__(1,"BRDF")},
	 {hradio,[{?__(2,"Lambertian"),lambertian},
		  {?__(3,"Perfect Specular"),perfectspecular}],
	  BDF,[layout,{key,{?TAG,bdf}}]},
	 panel,
	 help_button({material_dialog,shaders})]}
%        {hframe,
% 	[{"Enable Emission", EDF, [{key, {?TAG,edf}}]},
% 	 {label, "Radiant Exitance"},
% 	 {text, Radiant, [{range, {0, 500000}}, {key, {?TAG, radiant}},
% 			  enable_hook({?TAG, edf})]}]}
      ],
      [{title,?__(4,"Toxic Options")},{minimized,Min},
       {key,{?TAG,shader_minimized}}]}].

rgba2rgb({R,G,B,_}) -> {R,G,B}.

material_result(_Name, Mat0, [{{?TAG,shader_minimized},_}|_]=Res0) ->
    {Ps,Res} = split_list(Res0, 4),
    Mat = [{?TAG,Ps}|keydelete(?TAG, 1, Mat0)],
    {Mat,Res};
material_result(Name, Mat, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Name,Mat,Res]}}).

light_dialog(_Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Toxic = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = case Type of
		   point -> ?DEF_ATTN_POWER;
		   spot -> ?DEF_ATTN_POWER;
		   area -> ?DEF_ATTN_POWER;
		   _ -> ?DEF_POWER
	       end,
    Minimized = pget(minimized, Toxic, true),
    Power = pget(power, Toxic, DefPower),
    CastShadows = pget(cast_shadows, Toxic, true),
    case Type of
	area ->
 	    [{vframe,
	      [{hframe,[{label,?__(1,"Power")},
			{text,Power,[{range,{0.0,10000.0}},{key,{?TAG,power}}]},
			panel,
			help_button(light_dialog)]}],
	      [{title,?__(2,"Toxic Options")},{key,{?TAG,minimized}},{minimized,Minimized}]}];
	ambient -> [];
	_ ->
	    [{vframe,
	      [{hframe,[{label,?__(3,"Power")},
			{text,Power,[{range,{0.0,10000.0}},{key,{?TAG,power}}]},
			panel,
			help_button(light_dialog)]},
	       {?__(4,"Cast Shadows"),CastShadows,[{key,{?TAG,cast_shadows}}]}],
	      [{title,?__(5,"Toxic Options")},{key,{?TAG,minimized}},{minimized,Minimized}]}]
    end.

max_hook(Key, Values) when list(Values) ->
    {hook,
     fun (is_minimized, {_Var,I,Sto}) when is_integer(Key) ->
	     not lists:member(gb_trees:get(I+Key, Sto), Values);
	 (is_minimized, {_Var,_I,Sto}) ->
%%	     io:format("~p ~p~n", [Key, gb_trees:get(Key, Sto)]),
	     not lists:member(gb_trees:get(Key, Sto), Values);
	 (_, _) -> void
     end};
max_hook(Key, Value) -> max_hook(Key, [Value]).

enable_hook(Key) ->
    {hook,
     fun (is_disabled, {_Var,_I,Store}) ->
	     not enable_hook_eval(Key, Store);
	 (_, _) -> void
     end}.

enable_hook_eval(['not',Key], Store) ->
    not enable_hook_eval(Key, Store);
enable_hook_eval(['and'|Keys], Store) ->
    enable_hook_and(Keys, Store);
enable_hook_eval(['or'|Keys], Store) ->
    enable_hook_or(Keys, Store);
enable_hook_eval(Key, Store) when not is_list(Key) ->
    gb_trees:get(Key, Store).

enable_hook_and([Key], Store) ->
    enable_hook_eval(Key, Store);
enable_hook_and([Key|Keys], Store) ->
    enable_hook_eval(Key, Store) andalso enable_hook_and(Keys, Store).

enable_hook_or([Key], Store) ->
    enable_hook_eval(Key, Store);
enable_hook_or([Key|Keys], Store) ->
    enable_hook_eval(Key, Store) orelse enable_hook_or(Keys, Store).

light_result(_Name, Ps0, Res0) ->
    {LightPs,Res} = split_list(Res0,3),
    Ps = [{?TAG,LightPs} | keydelete(?TAG, 1, Ps0)],
    %%    erlang:display({?MODULE,?LINE,[Ps,Res1]}),
    {Ps,Res}.

pref_edit(St) ->
    Dialogs = get_pref(dialogs, ?DEF_DIALOGS),
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    Dialog =
	[{vframe,[{menu,[{?__(1,"Disabled Dialogs"),disabled},
			 {?__(2,"Automatic Dialogs"),auto},
			 {?__(3,"Enabled Dialogs"),enabled}],
		   Dialogs,[{key,dialogs}]},
		  {label,?__(4,"Rendering Command:")},
		  {button,{text,Renderer,
			   [{key,renderer},wings_job:browse_props()]}}]}],
    wpa:dialog(?__(5,"Toxic Options"), Dialog,
	       fun (Attr) -> pref_result(Attr,St) end).

pref_result(Attr, St) ->
    set_pref(Attr),
    init_pref(),
    St.

export_dialog(Operation) ->
    SubDiv     = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    GScale     = get_pref(globalscale,  1.0),
    
    Camera     = get_pref(camera,  pinhole),
    Fstop      = get_pref(fstop,        1.4),   %??
    Focallen   = get_pref(focallen,     0.125), %??
    FocalDist  = get_pref(focaldist,    2.0),   %??
    AutoFocus  = get_pref(autofocus,    true),
    AutoPosX   = get_pref(autoposx,     0.0),
    AutoPosY   = get_pref(autoposy,     0.0),

    Gamma      = get_pref(gamma, ?DEF_GAMMA),
    Width      = get_pref(width, ?DEF_WIDTH),
    Height     = get_pref(height, ?DEF_HEIGHT),
    BG         = get_pref(background, {0.0,0.0,0.0}),

    ExportMesh = get_pref(export_mesh, ?DEF_EXPORT_MESH),
    %% Lighting
    DirectLight      = get_pref(direct_light, ?DEF_DIRECT_LIGHT),
    DirectLightMin   = get_pref(direct_light_min, ?DEF_DIRECT_LIGHT_MIN),
    InDirectLight    = get_pref(indirect_light, ?DEF_INDIRECT_LIGHT),
    InDirectLightMin = get_pref(indirect_light_min,?DEF_INDIRECT_LIGHT_MIN),
    SpecularRefl     = get_pref(specular_refl, ?DEF_SPECULAR_REFL),
    SpecularReflMin  = get_pref(specular_refl_min, ?DEF_SPECULAR_REFL_MIN),
    Caustics         = get_pref(caustics, ?DEF_CAUSTICS),
    CausticsMin      = get_pref(caustics_min, ?DEF_CAUSTICS_MIN),

    %% Sampling
    Sampling  = get_pref(sampling,  ?DEF_SAMPLING),
    SStype    = get_pref({pixel,sstype},    ?DEF_SSTYPE),
    SSrandom  = get_pref({pixel,ssrandom},  ?DEF_SSRANDOM),
    SSWidth   = get_pref({pixel,sswidth},   ?DEF_SSWIDTH),
    SSHeight  = get_pref({pixel,ssheight},  ?DEF_SSHEIGHT),
    WAContrast= get_pref(wacontrast,?DEF_WACONTRAST),
    WAMax     = get_pref(wamax,     ?DEF_WAMAX),
    %% Direct Light (Area parameters)
    DLtype    = get_pref({direct_light,sstype},    ?DEF_SSTYPE),
    DLrandom  = get_pref({direct_light,ssrandom},  ?DEF_SSRANDOM),
    DLWidth   = get_pref({direct_light,sswidth},   ?DEF_SSWIDTH),
    DLHeight  = get_pref({direct_light,ssheight},  ?DEF_SSHEIGHT),
    %% InDirect Light
    IDLNoPhotons = get_pref(idlnoPhotons,  ?DEF_IDLNOPHOTONS),
    IDLMaxPhotons= get_pref(idlmaxPhotons, ?DEF_IDLMAXPHOTONS),
    IDLMaxDist   = get_pref(idlmaxDist,    ?DEF_IDLMAXDIST),

    RadPreComp    = get_pref(radprecomp,     ?DEF_RADPRE_COMP),
    RadPreSpacing = get_pref({radprecomp,spacing},  ?DEF_RADPRE_SPACING),
    RadPreSearch  = get_pref({radprecomp,search},   ?DEF_RADPRE_SEARCH),

    RadPrimary = get_pref(radprimary,    ?DEF_RADPRIMARY),
    IDLPtype    = get_pref({radprimary,sstype}, ?DEF_SSTYPE),
    IDLPrandom  = get_pref({radprimary,ssrandom},?DEF_SSRANDOM),
    IDLPWidth   = get_pref({radprimary,sswidth}, ?DEF_SSWIDTH*2),
    IDLPHeight  = get_pref({radprimary,ssheight},?DEF_SSHEIGHT div 2),
    
    RadSecondary = get_pref(radsecondary,    ?DEF_RADSECONDARY),
    RadSecDist   = get_pref(radsecdist,      ?DEF_RADSECDIST),
    IDLStype    = get_pref({radsecondary,sstype}, ?DEF_SSTYPE),
    IDLSrandom  = get_pref({radsecondary,ssrandom},?DEF_SSRANDOM),
    IDLSWidth   = get_pref({radsecondary,sswidth}, ?DEF_SSWIDTH*2),
    IDLSHeight  = get_pref({radsecondary,ssheight},?DEF_SSHEIGHT div 2),
    %% Specular
    SpecDepth    = get_pref(specdepth,       ?DEF_SPECDEPTH),

    %% Caustics
    CAUNoPhotons = get_pref(caunoPhotons,  ?DEF_IDLNOPHOTONS),
    CAUMaxPhotons= get_pref(caumaxPhotons, ?DEF_IDLMAXPHOTONS),
    CAUMaxDist   = get_pref(caumaxDist,    ?DEF_IDLMAXDIST),

    Options      = get_pref(options,   []),

    [{vframe,
      [{hframe,
	[{hframe,[{?__(1,"Export Mesh(es)"), ExportMesh, [{key, {?TAG,export_mesh}}]},
		  {label,?__(2,"Sub-division Steps")},
		  {text,SubDiv,[{key,{?TAG,subdivisions}},{range,{0,10}},
				enable_hook({?TAG, export_mesh})]}],
	  [{title,?__(3,"Pre-rendering")}]},
	 panel,
	 help_button(export_dialog)]},
       {hframe, [{label, ?__(4,"Scale")},
		 {text, GScale, [{range,{0.0,1.0E200}},
				 {key, {?TAG,globalscale}}]}],
	[{title,?__(5,"Global Scale")}]},
       {vframe, [{hframe, [{hradio,[{?__(6,"PinHole Camera"),pinhole},
				    {?__(7,"Thin Lens Camera"),thinlens}],
			    Camera,[layout,{key,{?TAG,camera}}]}]},
		 {hframe, [{label, ?__(8,"Fstop")},
			   {text, Fstop, [{key, {?TAG,fstop}}]},
			   {label, ?__(9,"Focal length")},
			   {text, Focallen, [{key, {?TAG,focallen}}]}
			  ], [max_hook({?TAG,camera},[thinlens])]},
		 {vframe, [{hframe, [{?__(10,"AutoFocus"), AutoFocus,
				      [{key, {?TAG,autofocus}},layout]},
				     {hframe, [{label, "X"},
					       {text,AutoPosX,
						[{key,{?TAG,autoposx}},
						 {range,{-0.5,0.5}}]},
					       {label, "Y"},
					       {text,AutoPosY,
						[{key,{?TAG,autoposy}},
						 {range,{-0.5,0.5}}]}],
				      [max_hook({?TAG, autofocus},[true])]},
				     {hframe, [{label, ?__(11,"Focal Distance")},
					       {text, FocalDist, 
						[{key, {?TAG,focaldist}}]}],
				      [max_hook({?TAG, autofocus},[false])]}]
			   }],[max_hook({?TAG,camera},[thinlens])]}
		], [{title, ?__(12,"Camera")}]},
       {vframe,
	[{hradio,[{?__(13,"Super Sampling"),super},
		  {?__(14,"Whitted Adaptive"),whitted}],
	  Sampling,[layout,{key,{?TAG,sampling}}]},
	 {vframe,
	  pixelsampling(pixel,[SStype, SSrandom,SSWidth,SSHeight]),
	  [max_hook({?TAG, sampling},[super])]},
	 {hframe,
	  [{vframe, [{label, ?__(15,"Contrast Threshold")},
		     {label, ?__(16,"Max Depth")}]},
	   {vframe, [{text, WAContrast, [{key, {?TAG,wacontrast}}]},
		     {text, WAMax, [{range, {0, 500}}, {key, {?TAG, wamax}}]}]}],
	  [max_hook({?TAG, sampling},[whitted])]}],
	[{title,?__(17,"Pixel Sampling")}]},
       {vframe,
	[{hframe,
	  [{"", DirectLight, [{key, {?TAG,direct_light}}]},
	   {vframe,
	    pixelsampling(direct_light,[DLtype, DLrandom, DLWidth,DLHeight]),
	    [{title,?__(18,"Enable Direct Lighting")},
	     {minimized,DirectLightMin},{key,{?TAG,direct_light_min}},
	     enable_hook({?TAG,direct_light})]}]},
	 {hframe,
	  [{"", InDirectLight, [{key, {?TAG,indirect_light}}]},
	   {vframe,
	    [{hframe,
	      [{label, ?__(19,"Emit Photons")},
	       {text,
		IDLNoPhotons,[{range,{10000,50000000}},{key,{?TAG,idlnoPhotons}}]},
	       {label, ?__(20,"Max Photons")},
	       {text,
		IDLMaxPhotons,[{range,{10,50000}},{key,{?TAG,idlmaxPhotons}}]},
	       {label, ?__(21,"Max Distance")},
	       {text, IDLMaxDist, [{key, {?TAG, idlmaxDist}}]}]},
	     {hframe,
	      [{"", RadPreComp, [{key, {?TAG, radprecomp}}]},
	       {label, ?__(22,"Radiance Precomputation")},
	       {hframe,
		[
		 {label, ?__(23,"Spacing")},
		 {text, RadPreSpacing,[{range,{1,10000}},
				       {key,{?TAG,{radprecomp,spacing}}}]},
		 {label, ?__(24,"Search Dist")},
		 {text, RadPreSearch, [{range,{0.1,100.0}},
				       {key, {?TAG, {radprecomp, search}}}]}],
		[enable_hook({?TAG, radprecomp})]}]},
	     {hframe,
	      [{"", RadPrimary, [{key, {?TAG, radprimary}}]},
	       {vframe,
		[{label, ?__(25,"Primary Final Gathering")} |
		 pixelsampling(radprimary, [IDLPtype, IDLPrandom,
					    IDLPWidth,IDLPHeight])],
		[enable_hook({?TAG, radprimary})]}]},
	     {hframe,
	      [{"", RadSecondary, [{key, {?TAG, radsecondary}}]},
	       {vframe, 
		[{hframe, 
		  [{label, ?__(26,"Secondary Final Gathering")},
		   {text, RadSecDist, 
		    [{range, {0.0000001, 1000.0}},
		     {key, {?TAG, {radsecondary, dist}}}]}]} |
		 pixelsampling(radsecondary, [IDLStype, IDLSrandom,
					      IDLSWidth,IDLSHeight])],
		[enable_hook({?TAG,radsecondary})]}]}],
	    [{title,?__(27,"Enable Indirect Lighting")},
	     {minimized,InDirectLightMin},
	     {key,{?TAG,indirect_light_min}},
	   enable_hook({?TAG,indirect_light})]}]},
	 {hframe,
	  [{"", SpecularRefl, [{key, {?TAG,specular_refl}}]},
	   {hframe,
	    [{label, ?__(28,"Reflection Depth")},
	     {text, SpecDepth, [{range, {0, 1000000}},
				{key, {?TAG, specdepth}}]}],
	    [{title,?__(29,"Enable Specular Reflections")},
	     {minimized,SpecularReflMin},{key,{?TAG,specular_refl_min}},
	     enable_hook({?TAG,specular_refl})]}]},
	 {hframe,
	  [{"", Caustics, [{key, {?TAG,caustics}}]},
	   {hframe,
	    [{label, ?__(30,"Emit Photons")},
	     {text, CAUNoPhotons,
	      [{range,{10000,50000000}},{key,{?TAG,caunoPhotons}}]},
	     {label, ?__(31,"Max Photons")},
	     {text, CAUMaxPhotons,
	      [{range,{10,50000}},{key,{?TAG,caumaxPhotons}}]},
	     {label, ?__(32,"Max Distance")},
	     {text, CAUMaxDist, [{key, {?TAG, caumaxDist}}]}],
	    [{title,?__(33,"Enable Caustics")},
	     {minimized,CausticsMin},{key,{?TAG,caustics_min}},
	     enable_hook({?TAG,caustics})]}]}],
	[{title,?__(34,"Render")}]},
       {hframe,
	[{vframe,[{label,?__(35,"Width")}]},
	 {vframe,[{text,Width,[{range,{1,10000}},{key,{?TAG,width}}]}]},
	 {vframe,[{label,?__(36,"Height")}]},
	 {vframe,[{text,Height,[{range,{1,10000}},{key,{?TAG,height}}]}]},
	 {vframe,[{label,?__(37,"Gamma")}]},
	 {vframe,[{text,Gamma,[{range,{0.0,10.0}},{key,{?TAG,gamma}}]}]},
	 {vframe,[{label,?__(38,"BackGround")}]},
	 {vframe,[{color,BG,[{key,{?TAG,background}}]}]}],
	[{title,?__(39,"Output")}]}]}
     |
     case Operation of
	 render ->
	     [{hframe,[{label,?__(40,"Options")},
		       {text,Options,[{key,{?TAG,options}}]}
%%		       {"Load Image",LoadImage,[{key,load_image}]}
		      ],
	       [{title,?__(41,"Rendering Job")}]}];
	 export ->
	     [];
	 export_selected ->
	     []
     end].

pixelsampling(PType, [Sample, Random, Width,Height]) ->
    [{hframe, [{hradio, [{?__(1,"Random"), random},
			 {?__(2,"Regular"), regular},
			 {?__(3,"Stratified"), stratified}],
		Sample, [layout, {key,{?TAG,{PType,sstype}}}]},
	       {hframe,
		[{label, ?__(4,"Samples")},
		 {text, Random, [{range,{1,128}},
				 {key,{?TAG,{PType,samples}}}]}],
		[max_hook({?TAG,{PType,sstype}},[random])]},
	       {hframe,
		[{label, ?__(5,"Width")},
		 {text, Width, [{range,{1,128}},{key,{?TAG,{PType,sswidth}}}]},
		 {label, ?__(6,"Height")},
		 {text, Height,[{range,{1,128}},{key,{?TAG,{PType,ssheight}}}]}
		],[max_hook({?TAG,{PType,sstype}},[regular, stratified])]}
	      ]}].

%% Boolean hook
%%
%% Used to enable/disable or minimize/maximize a field depending
%% on a boolean control field.
%%
% bhook(Type, Tag) ->
%     {hook,fun (is_disabled, {_Var,_I,Sto}) ->
% 		  case Type of
% 		      enabled -> not gb_trees:get(Tag, Sto);
% 		      disabled -> gb_trees:get(Tag, Sto);
% 		      _ -> void
% 		  end;
% 	      (is_minimized, {_Var,_I,Sto}) ->
% 		  case Type of
% 		      maximized -> not gb_trees:get(Tag, Sto);
% 		      minimized -> gb_trees:get(Tag, Sto);
% 		      _ -> void
% 		  end;
% 	      (_, _) -> void end}.

%%% Export and rendering functions
%%%

-record(files, {dir, image, scene, settings, objects}).

files(Render, Filename) ->
    RootName  = filename:rootname(Filename),
    SceneName = filename:rootname(filename:basename(Filename)),
    ExportDir0 = filename:dirname(Filename),
    ExportDir  = case Render of
		     false -> ExportDir0;
		     true ->
			 Dir = RootName ++ "_toxic",
			 case file:make_dir(Dir) of
			     ok -> Dir;
			     {error, eexist} -> Dir;
			     Err  -> exit(Err)
			 end
		 end,
    {value,{png,Ext,_}} =
	lists:keysearch(png, 1, wings_job:render_formats()),
    Image = case Render of
		true ->  Filename;
		false -> RootName ++ Ext
	    end,
    Scene    = filename:join(ExportDir, SceneName ++ ".xml"),
    Settings = filename:join(ExportDir, SceneName ++ ".settings.xml"),
    Objects  = filename:join(ExportDir, SceneName ++ ".objects.obj"),
    #files{dir=ExportDir, image=Image, scene=Scene,
	   settings=Settings, objects=Objects}.

export(Attr, Filename, E3DExport0) ->
    wpa:popup_console(),
    ExportTS = erlang:now(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    #files{dir=Dir, image=Image, scene=Scene} = Files = 
	files(Render, Filename),
%    E3DExport = obj_per_mat(E3DExport0),
    E3DExport = obj_per_file(E3DExport0),
    case pget(export_mesh, Attr) of
	true ->
	    export_objs(Files, E3DExport);
	false ->
	    ok
    end,

    export_settings(Files, Attr),
    export_scene(Files, E3DExport,Attr),
    
    case {get_var(renderer),Render} of
	{_,false} ->
	    wings_job:export_done(ExportTS),
	    io:nl();
	{false,true} ->
	    %% Should not happen since the file->render dialog
	    %% must have been disabled
	    no_renderer;
	{Renderer,true} ->
	    Options = pget(options,Attr,?DEF_OPTIONS),
%%	    LoadImage = pget(load_image,Attr,?DEF_LOAD_IMAGE),
	    Width = pget(width,Attr),
	    Height = pget(height,Attr),
	    ArgStr =wings_job:quote(filename:basename(Scene))
		++" -o "++wings_job:quote(Image)
		++" -w "++format(Width)
		++" -h "++format(Height)
		++case Options of
		      [] -> [];
		      _ -> " "
		  end++Options,
	    PortOpts = [{cd,Dir}],
	    Handler =
		fun (Status) ->
			set_var(rendering, false),
			case Status of
			    ok  -> {png,Files#files.image};
			    _   -> Status
			end
		end,
	    set_var(rendering, true),
	    wings_job:render(ExportTS, Renderer, ArgStr, PortOpts, Handler)
    end.

export_objs(#files{}, []) -> ok;
export_objs(F=#files{dir=Dir}, [E3F|R]) ->
    #e3d_file{objs=[#e3d_object{name=File}]} = E3F,
    e3d_obj:export(filename:join(Dir,File++".obj"), E3F, 
		   [{include_normals,true}]),
    export_objs(F,R);
export_objs(#files{objects=File}, E3DExport) ->
    e3d_obj:export(File, E3DExport).

% obj_per_mat(EF = #e3d_file{objs=Objs0}) ->
%     Objs = split_objs(Objs0, []),
%     EF#e3d_file{objs=Objs}.

obj_per_file(EF = #e3d_file{objs=Objs0, mat=Mtab}) ->
    Objs = split_objs(Objs0, []),
    lists:map(fun(Obj = #e3d_object{mat=[Mat0|_], obj=Mesh = #e3d_mesh{fs=Fs0}}) ->
		      Mat = fixName(Mat0),
		      Fs = [F#e3d_face{mat=[Mat]} || F <- Fs0],
		      {value, {_,MatDef}} = lists:keysearch(Mat0,1,Mtab),
		      EF#e3d_file{objs=[Obj#e3d_object{mat=[Mat], 
						       obj=Mesh#e3d_mesh{fs=Fs}}],
				  mat=[{Mat,MatDef}]}
	      end, Objs).

split_objs([Ok=#e3d_object{mat=[_OneMat]}|R], Acc) ->
    split_objs(R, [Ok|Acc]);
split_objs([Obj0=#e3d_object{obj=Mesh0=#e3d_mesh{fs=FaceL0}}|R],Acc) ->
    case lists:keysort(#e3d_face.mat, FaceL0) of
	[] -> % hmm no faces..
	    split_objs(R, Acc);
	[First|SortedFL] ->
	    Objs = [mkObj(Obj0,Mesh0, SplitFs) 
		    ||SplitFs <- split_fs(SortedFL, First#e3d_face.mat, [[First]])],
	    split_objs(R, Objs ++ Acc)
    end;
split_objs([], Acc) -> Acc.

mkObj(Obj=#e3d_object{name=Name},Mesh,{Mat,FaceL}) ->
    Obj#e3d_object{obj=Mesh#e3d_mesh{fs=FaceL},
		   mat=Mat,
		   name=mkName(Name,Mat)}.

fixName(Name) when is_atom(Name) ->
    list_to_atom(fixName2(atom_to_list(Name)));
fixName(Name) ->
    fixName2(Name).

fixName2([$_|R]) ->
    [$_,$_|fixName2(R)];
fixName2([$ |R]) ->
    [$_|fixName2(R)];
fixName2([$/|R]) ->
    [$_|fixName2(R)];
fixName2([92|R]) -> %% \
    [$_|fixName2(R)];
fixName2([$.|R]) ->
    [$_|fixName2(R)];
fixName2([$*|R]) ->
    [$_|fixName2(R)];
fixName2([A|R]) ->
    [A|fixName2(R)];
fixName2([]) -> [].

mkName(Name, [Mat0]) when list(Name) ->
    Mat = to_list(Mat0),
    fixName(Name) ++ "_" ++ fixName(Mat);
mkName(Name, Mat) ->
    erlang:fault({strange_names, Name,Mat}).

to_list(Mat) when atom(Mat) ->
    atom_to_list(Mat);
to_list(Mat) when list(Mat) ->
    Mat;
to_list(Mat) ->
    exit({strange_matname, Mat}).

split_fs([F=#e3d_face{mat=Mat}|R], Mat, [Curr|Acc]) ->
    split_fs(R, Mat, [[F|Curr]|Acc]);
split_fs([F=#e3d_face{mat=Mat}|R], PrevMat, [Curr|Acc]) ->
    split_fs(R, Mat, [[F],{PrevMat,Curr}|Acc]);
split_fs([], Mat, [Curr|Acc]) ->
    [{Mat,Curr}|Acc].

export_settings(#files{settings=File}, Attr) ->
    F = open(File, export),
    io:format(?__(1,"Exporting to")++" ~s~n", [File]),
    println(F, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
    println(F, "<!-- ~s: Exported from Wings3d -->",
	    [filename:basename(File)]),
    println(F, "<!-- Exported to toxic see: http://toxicengine.sourceforge.net -->"),
    println(F, "<ToxicSceneSettings xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
	    "xsi:noNamespaceSchemaLocation=\"toxicscene.settings.xsd\">"),
    println(F, " <Rendering>"),
    println(F, " <PixelSampling>"),
    case pget(sampling, Attr) of
	whitted ->
	    println(F,"  <WhittedAdaptiveSampling contrastthreshold=\"~s\" "
		    "maxdepth=\"~s\" />",
		    [format(pget(wacontrast, Attr)),
		     format(pget(wamax, Attr))]);
	super ->
	    println(F,"  <Supersampling>"),
	    export_sampling(F,pixel, Attr),
	    println(F,"  </Supersampling>")
    end,

    println(F, " </PixelSampling>"),
    println(F, " <Components>"),
    case pget(direct_light, Attr) of
	true ->
	    println(F,"  <DirectLighting>~n   <ArealightSampling>", []),
	    export_sampling(F,direct_light, Attr),
	    println(F,"   </ArealightSampling>~n  </DirectLighting>",[]);
	false -> ok
    end,
    case pget(indirect_light, Attr) of
	true ->
	    println(F, " <IndirectLighting>"),
	    println(F, "  <PhotonTracing photons=\"~s\" />",
		    [format(pget(idlnoPhotons,Attr))]),
	    println(F, "  <RadianceEstimate maxphotons=\"~s\" maxdistance=\"~s\"/>",
		    [format(pget(idlmaxPhotons,Attr)),
		     format(pget(idlmaxDist,Attr))]),
	    case pget(radprecomp,Attr) of
		true ->
		    println(F, "<RadiancePrecomputation spacing=\"~s\" "
			    "maxsearchdistance=\"~s\" />",
			    [format(pget({radprecomp,spacing},Attr)),
			     format(pget({radprecomp,search},Attr))]);
		false -> ok
	    end,
	    case pget(radprimary, Attr) of
		true ->
		    println(F, " <PrimaryFinalGathering>"),
		    export_sampling(F,radprimary, Attr),
		    println(F, " </PrimaryFinalGathering>");
		false -> ok
	    end,
	    case pget(radsecondary, Attr) of
		true ->
		    println(F, " <SecondaryFinalGathering distancethreshold=\"~s\">",
			    [format(pget({radsecondary,dist},Attr))]),
		    export_sampling(F,radsecondary, Attr),
		    println(F, " </SecondaryFinalGathering>");
		false -> ok
	    end,
	    println(F," </IndirectLighting>");
	false -> ok
    end,
    case pget(specular_refl, Attr) of
	true ->
	    println(F, " <SpecularReflections maxdepth=\"~s\" />",
		    [format(pget(specdepth,Attr))]);
	false -> ok
    end,
    case pget(caustics, Attr) of
	true ->
	    println(F, " <Caustics>"),
	    println(F, "  <PhotonTracing photons=\"~s\" />",
		    [format(pget(caunoPhotons,Attr))]),
	    println(F, "  <RadianceEstimate maxphotons=\"~s\" maxdistance=\"~s\"/>",
		    [format(pget(caumaxPhotons,Attr)),
		     format(pget(caumaxDist,Attr))]),
	    println(F, " </Caustics>");
	false -> ok
    end,
    println(F, " </Components>"),
    println(F, " </Rendering>"),
    println(F, " <Output>"),
    println(F, "  <GammaCorrection targetgamma=\"~s\" />",
	    [format(pget(gamma, Attr))]),
    println(F, " </Output>"),
    println(F, "</ToxicSceneSettings>"),
    close(F),
    ok.

export_sampling(F,Type,Attr) ->
    Samples = pget({Type,samples},Attr),
    W = pget({Type,sswidth},Attr),
    H = pget({Type,ssheight},Attr),
    case pget({Type,sstype},Attr) of
	random ->
	    println(F, "<RandomSampling samples=\"~s\" />",
		    [format(Samples)]);
	stratified ->
	    println(F, "<StratifiedSampling width=\"~s\" height=\"~s\" />",
		    [format(W),format(H)]);
	regular ->
	    println(F, "<RegularSampling width=\"~s\" height=\"~s\" />",
		    [format(W),format(H)])
    end.

export_scene(#files{scene=File,dir=Dir, objects=Wavefront}, Objs, Attrs) ->
    F = open(File, export),
    io:format(?__(1,"Exporting to")++" ~s~n", [File]),
    println(F, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
    println(F, "<!-- ~s: Exported from Wings3d -->",
	    [filename:basename(File)]),
    println(F, "<!-- Exported to toxic see: http://toxicengine.sourceforge.net -->"),
    println(F, "<ToxicScene xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
	    "xsi:noNamespaceSchemaLocation=\"../../schemas/toxicscene.xsd\">"),
    println(F, " <Frame>"),
    println(F, "  <Parameter name=\"backgroundcolor\" value=\"~s ~s ~s\"/>",
	    vector_to_list(pget(background,Attrs))),

    Mat = case Objs of  %% Hack
	      #e3d_file{mat=Mtab} -> 
		  Mtab;
	      _ -> 
		  lists:usort([OMat || #e3d_file{mat=[OMat]} <- Objs])
	  end,

    export_shaders(F, Mat, Dir),
    Scale = pget(globalscale, Attrs),
    export_obj_refs(F, Objs, Scale, Wavefront),
    Lights = proplists:get_value(lights, Attrs, []),
    export_lights(F, Lights, Scale),
    export_camera(F, Scale, Attrs),
    println(F, " </Frame>"),
    println(F, "</ToxicScene>"),
    close(F),
    ok.

export_shaders(F, [{Name,Mat}|Ms], ExportDir) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Toxic = proplists:get_value(?TAG, Mat, []),
    println(F, "  <SurfaceShader name=\"~s\">", [to_list(Name)]),
    println(F, "   <BDF type=\"~s\"/>", [to_list(pget(bdf, Toxic, lambertian))]),
    println(F, "   <Reflectance>"),
    case proplists:get_value(diffuse, Maps, none) of
	none -> 
	    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL)),
	    println(F,"     <ConstantTexture value=\"~s ~s ~s\"/>",
		    [format(DR),format(DG),format(DB)]);
	#e3d_image{name=ImageName}=Image ->
	    MapFile = ImageName++".png",
	    ok = e3d_image:save(Image, filename:join(ExportDir, MapFile)),
	    println(F,"     <ImageTexture href=\"~s\"/>",[MapFile])
    end,
    println(F, "   </Reflectance>"),
    case pget(edf, Toxic) of
	true -> 
	    {ER,EG,EB} = rgba2rgb(proplists:get_value(emission, OpenGL)),
	    Rad = pget(radiant, Toxic),
	    println(F, "   <EDF type=\"lambertian\"/>"),
	    println(F, "   <RadiantExitance value=\"~s ~s ~s\"/>",
		    [format(ER*Rad), format(EB*Rad), format(EG*Rad)]),
	    ok;
	_ -> ok
    end,
    println(F, "  </SurfaceShader>"),
    export_shaders(F, Ms, ExportDir);
export_shaders(_, [], _) -> ok.
export_obj_refs(F, [Obj|Os], Scale, File) ->
    {Name, MatName} = 
	case Obj of 
	    #e3d_object{name=N,mat=[M]} -> {N,M};
	    #e3d_file{objs=[#e3d_object{name=N,mat=[M]}]} -> {N,M}
	end,

    println(F,"  <Object type=\"mesh\">"),
    println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
	    ["href", filename:basename(Name++".obj")]),
%% If one file
%     println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
% 	    ["href", filename:basename(File)]),
%     println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
% 	    ["include", Name]),
    println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
	    ["surfaceshader", MatName]),
    println(F,"     <Transform> <Scale value=\"~s\"/> </Transform>", [format(Scale)]),
    println(F,"  </Object>"),
    export_obj_refs(F, Os, Scale, File);
export_obj_refs(_, [],_,_) ->
    ok.

export_lights(F, [{Name,L}|Ls], Scale) ->
    case proplists:get_value(visible,L,true) of
	true ->
	    OpenGL = proplists:get_value(opengl, L, []),
	    Toxic  = proplists:get_value(?TAG, L, []),
	    Type   = proplists:get_value(type, OpenGL, []),
	    export_light(F, Name, Type, Scale, OpenGL, Toxic);
	_ ->
	    undefined
    end,
    export_lights(F, Ls,Scale);
export_lights(_, [],_) -> ok.

export_light(F, Name, area, Scale, OpenGL, Toxic) -> 
    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0})),
    #e3d_mesh{vs=Vs0,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs0),
    Fs = foldr(fun (Face, Acc) -> 
		       e3d_mesh:quadrangulate_face(Face, Vs0)++Acc
	       end, [], Fs0),
    As = e3d_mesh:face_areas(Fs, Vs0),
    Power = pget(power, Toxic, ?DEF_ATTN_POWER) / length(As),
    println(F, "  <SurfaceShader name=\"~s\">", [to_list(Name)]),
    println(F, "   <Reflectance>"),
    println(F,"     <ConstantTexture value=\"~s ~s ~s\"/>",
	    [format(DR),format(DG),format(DB)]),
    println(F, "   </Reflectance>"),
    println(F, "   <EDF type=\"lambertian\"/>"),
    println(F, "   <RadiantExitance value=\"~s ~s ~s\"/>",
	    [format(DR*Power), format(DB*Power), format(DG*Power)]),
    println(F, "  </SurfaceShader>"),

    foldl(
      fun(#e3d_face{vs=VsF}, I) ->
	      Vs = [A,B,C|_] = quadrangle_vertices(VsF, VsT),
	      %% Since Toxic currently only handles squares
	      %% I intentionally make them square
	      Width  = Scale * e3d_vec:dist(A,B),
	      Height = Scale * e3d_vec:dist(B,C),
	      Pos = e3d_vec:average(Vs),
	      Normal = e3d_vec:normal(Vs),
	      println(F,"  <Object type=\"square\">"),
	      println(F,"     <Parameter name=\"~s\" value=\"~s\"/>", 
		      ["surfaceshader", Name]),
	      println(F,"     <Transform>"),
	      println(F,"      <Scale value=\"~s ~s ~s\"/>", 
		      [format(Width), format(Scale), format(Height)]),
	      %% Rotate plane to get correct lie
	      Mat = e3d_mat:rotate_s_to_t(Normal,{0.0,1.0,0.0}),
	      print(F,"      <Matrix4>~n        ",[]),
	      lists:foreach(fun(E) -> print(F,"~s ", [format(E)]) end,
			    tuple_to_list(e3d_mat:expand(Mat))),
	      println(F,"      ~n       </Matrix4>",[]),
	      %% Rotate arealight around normal, 
	      %% to get the corners in the correct position
	      [MyCorner|_] = lists:sort([e3d_mat:mul_point(Mat,{Cx*Width,0.0,Cz*Height}) 
					 || {Cx,Cz} <- [{-0.5,-0.5},{-0.5,0.5},
							{0.5,0.5},{-0.5,0.5}]]),
	      [Corner|_] = lists:sort(Vs),
	      V1 = e3d_vec:norm(e3d_vec:sub(Corner,Pos)),
	      V2 = e3d_vec:norm(MyCorner),
	      ACos = case e3d_vec:cross(V1,V2) < 0.0 of
			 true -> - e3d_vec:dot(V1,V2);
			 false -> e3d_vec:dot(V1,V2)
		     end,
	      println(F,"      <Rotation angle=\"~s\" axis=\"~s ~s ~s\"/>", 
		      [format(acos(ACos)*180/?PI)|vector_to_list(Normal)]),
	      println(F,"      <Translation value=\"~s ~s ~s\"/>",
		      vector_to_list(Pos)),
	      println(F,"     </Transform>"),
	      println(F,"  </Object>"),
	      I+1
      end, 1, Fs);
export_light(F, _, ambient, _, OpenGL, _) -> 
    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0})),
    println(F,"  <Parameter name=\"ambientillum\" value=\"~s ~s ~s\"/>", 
	    [format(DR),format(DG),format(DB)]); 
export_light(F, _, _, Scale, OpenGL, Toxic) ->
    {X,Y,Z} = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    {DR,DG,DB} = rgba2rgb(proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0})),
    Power = pget(power, Toxic, ?DEF_ATTN_POWER),
    CastShadows = pget(cast_shadows, Toxic, true),
    println(F,"  <Object type=\"pointlight\">"),
    println(F,"  <Parameter name=\"power\" value=\"~s ~s ~s\"/>", 
	    [format(DR*Power),format(DG*Power),format(DB*Power)]),
    println(F,"  <Parameter name=\"castsshadow\" value=\"~s\"/>", 
	    [to_list(CastShadows)]),
    println(F,"    <Transform>"),
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
	    [format(X),format(Y),format(Z)]),
    println(F,"      <Scale value=\"~s\"/>", [format(Scale)]),
    println(F,"    </Transform>"),
    println(F,"  </Object>").


acos(V) when abs(V) < 1.0 ->
    math:acos(V);
acos(V) when abs(V) < 1.1 ->
    case V > 0.0 of
	true -> 0.0;
	false -> ?PI
    end.

export_camera(F, Scale, Attr) ->
    #camera_info{ %pos=_Pos,dir=Dir,up=Up,
	       fov=Fov, origin=Origin,distance=Dist,azimuth=Az,
	       elevation=El,pan_x=PanX,pan_y=PanY} = 
	proplists:lookup(camera_info, Attr),
    case pget(camera, Attr) of
	pinhole ->
	    println(F,"  <Object type=\"pinholecamera\">");
	thinlens ->
	    println(F,"  <Object type=\"thinlenscamera\">"),
	    println(F,"    <Parameter name=\"fstop\" value=\"~s\"/>", 
		    [format(pget(fstop,Attr))]),
	    println(F,"    <Parameter name=\"focallength\" value=\"~s\"/>", 
		    [format(pget(focallen,Attr))]),
	    case pget(autofocus, Attr) of
		true ->
		    println(F,"    <Parameter name=\"autofocus\" value=\"~s ~s\"/>", 
			    [format(pget(autoposx,Attr)),format(pget(autoposy,Attr))]);
		false ->
		    println(F,"    <Parameter name=\"focaldistance\" value=\"~s\"/>", 
			    [format(pget(focaldist,Attr))])
	    end
    end,
    W = pget(width, Attr),    H = pget(height, Attr),
    println(F,"  <Parameter name=\"hfov\" value=\"~s\"/>", 
	    [format(2*math:atan(2*math:tan(0.5*Fov*?PI/180)*W/H)*180/?PI)]),
    println(F,"    <Transform>"),

%%%%%%%%%%% Almost Same as wings %%%%%%%%%%%%
%%% After trial and error, It looks like this works..
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
 	    [format(-PanX*Scale),format(-PanY*Scale),format(Dist*Scale)]),
    println(F,"      <Rotation angle=\"~s\" axis=\"1.0 0.0 0.0\"/>", 
 	    [format(-El)]),
    println(F,"      <Rotation angle=\"~s\" axis=\"0.0 1.0 0.0\"/>", 
 	    [format(-Az)]),
    println(F,"      <Translation value=\"~s ~s ~s\"/>", 
 	    vector_to_list(Origin,-Scale)),

    println(F,"    </Transform>"),
    println(F,"  </Object>"),
    ok.

vector_to_list({X,Y,Z}) ->
    [format(X),format(Y),format(Z)].
vector_to_list({X,Y,Z},Scale) ->
    [format(X*Scale),format(Y*Scale),format(Z*Scale)].



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
	    if L12 > L23 -> [P1,e3d_vec:average([P1,P2]),P2,P3];
	       true -> [P1,P2,e3d_vec:average([P2,P3]),P3]
	    end;
       true -> [P1,P2,P3,e3d_vec:average([P3,P1])]
    end;
quadrangle_vertices([V1,V2,V3,V4], VsT) -> 
    [element(V1+1, VsT),element(V2+1, VsT),
     element(V3+1, VsT),element(V4+1, VsT)].


% %% Zip lists together into a list of tuples
% %%
% zip([], []) -> [];
% zip([H1|T1], [H2|T2]) -> [{H1,H2}|zip(T1, T2)].



%%% Noisy file output functions. Fail if anything goes wrong.
%%%

open(Filename, export) ->
    case file:open(Filename, [write,raw,delayed_write]) of
	{ok, F} ->
	    F;
	Error ->
	    erlang:fault(Error, [Filename, export])
    end.

%% println(F) ->
%%     println(F, "").

% print(F, DeepString) ->
%     case file:write(F, DeepString) of
% 	ok ->
% 	    ok;
% 	Error ->
% 	    erlang:fault(Error, [F,DeepString])
%     end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F,DeepString])
    end.

print(F, Format, Args) ->
    case file:write(F, io_lib:format(Format, Args)) of
 	ok ->    ok;
 	Error -> erlang:fault(Error, [F,Format,Args])
    end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F,Format,Args])
    end.

close(F) ->
    case file:close(F) of
	ok ->    ok;
	Error -> erlang:fault(Error, [F])
    end.

%% Convert certain terms to printable strings in a
%% hopefully efficient way.

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->   [$-,integer_to_list(I)|format_decimals(D)];
       true ->    [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->    integer_to_list(I);
format(true) ->     "true";
format(false) ->    "false";
format(A) when is_atom(A) ->    atom_to_list(A);
format(L) when is_list(L) ->    L.

format_decimals(F) when float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(0.0) ->    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(0.0) ->    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->   [$0,(I+$0)|format_decimals_3(D)];
       true ->     [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(0.0) ->    [];
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

format_decimals_4(0.0) ->    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
	    if I < 10 ->   [$0,$0,$0,(I+$0)];
	       true ->     [$0,$0|integer_to_list(I)]
	    end;
       true ->
	    if I < 1000 -> [$0|integer_to_list(I)];
	       true ->     integer_to_list(I)
	    end
    end.

%% Set and get preference variables saved in the .wings file for this module

set_pref(Attr) ->  % Both in scene and ~/.wings file
    wpa:scene_pref_set(?MODULE, Attr),
    wpa:pref_set(?MODULE, Attr).
get_pref(Key, Def) ->
    Wings = case wpa:pref_get(?MODULE, {?TAG,Key}) of
		undefined ->
		    wpa:pref_get(?MODULE, Key, Def);
		Else ->
		    Else
	    end,
    wpa:scene_pref_get(?MODULE, {?TAG,Key}, Wings).

pget(Key,Attr) ->
    proplists:get_value({?TAG,Key}, Attr).
pget(Key,Attr,Def) ->
    proplists:get_value({?TAG,Key}, Attr, Def).

%% Set and get global variables (in the process dictionary)
%% per wings session for this module.

set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    put({?MODULE,{?TAG,Name}}, Value).
get_var(Name) ->
    get({?MODULE,{?TAG,Name}}).
erase_var(Name) ->
    erase({?MODULE,{?TAG,Name}}).

%% Split a list into a list of length Pos, and the tail
%%

split_list(List,_) ->
    split_list1(List, []).

split_list1([T={TAG, _}|R], Toxic) when element(1, TAG) == ?TAG ->
    split_list1(R, [T|Toxic]);
split_list1(R,Toxic) ->
    {lists:reverse(Toxic),R}.
 
%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end,List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%%  true -> filter2_1(Pred, T, [H|True], False);
%%%  false -> filter2_1(Pred, T, True, [H|False])
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

help(title, export_dialog) ->
    ?__(1,"Toxic Export Properties");
help(text, export_dialog) ->
    [?__(2,"Toxic: see http://toxicengine.sourceforge.net"),

     ?__(3,"Toxic export uses several files: YourScene.xml consists of "
      "the shaders (e.g. materials) and objects (with references to the "
      "actual meshes). Lights and the camera are also defined as scene objects. "
      "Render settings are defined in YourScene.setting.xml "
      "(i.e. most of the settings in this dialog). "
      "The actual meshes are stored in YourScene.objects.obj "
      "file; that means that you don't have to export the the meshes for every "
      "rendering, i.e. if you only change something in the materials or in this "
      "dialog you don't have to export the meshes again."),

     ?__(4,"Global Scale lets you specify a scale parameter from wings units to meters. "
      "This is necessary get correct lighting in your scene. So if one wings unit "
      "represents one centimeter on your model set Global Scale to 0.01, or if "
      "one wings unit is two meters, set Global Scale to 2."),

     ?__(5,"Camera lets you choose between two different type of cameras."
      "The pinhole camera is the simplest one. Since it has infinite "
      "depth of field, objects can't be out of focus and thus never appear blurred. "
      "The thin lens camera is slightly more complex (and much more plausible) "
      "than the pinhole camera. It will produce depth of field according to the "
      "fstop, focaldistance and focallength  parameters. Out of focus objects "
      "will appear blurred.\n"
      "Fstop: this is the aperture number or f-stop number.\n"
      "     Lens diameter is computed as follow: \n"
      "     lensdiameter = focallength / fstop.\n "      
      "Focal length: focal length of the camera, expressed in meters.\n"
      
      "Additionally, one of the two following parameters must be defined: "
      "focaldistance or autofocus. Focal distance of the camera, expressed in meters. "
      "The focal distance is the distance along the view direction at which "
      "objects will be in focus." 
      "Autofocus is a point, in the image plane, which will be in focus. "
      "Bottom left corner of the image plane is at (-0.5, -0.5), and "
      "upper right corner is at (0.5, 0.5), regardless of the image resolution. "),
     ?__(6,"Pixel Sampling specifies how antialising is done"),
     ?__(7,"Rendering specifies which kind of lighting method should be used "
      "There are four types lighting methods: direct lighting, indirect lighting, "
      "specular reflections and caustics. These methods form the global " 
      "illumination (GI) and each method can be individually enabled or disabled. "),
     ?__(8,"Output specifies the rendered image width, height and gamma."),

     ?__(9,"Rendering Job lets you specify additional command line arguments for "
      "toxic")
    ];

help(title, {material_dialog,shaders}) ->
    ?__(10,"Toxic Material Properties")++": ";
help(text, {material_dialog,shaders}) ->
    [?__(11,"The BRDF characterizes how light is reflected by the surface."),
     ?__(12," Lambertian is a perfectly diffuse surface and currently the only "
      "other option is a perfectly specular surface"),
     ?__(13," The diffuse color is used in toxic as the reflectance color"
      " for all different types of BRDF."
      " If the material has an diffuse texture it will replace the reflectance color")];
%     <<"If emission is enabled, EDF will be set to lambertian, "
%       "Radiant Exitance will be multiplied with the emission color, and"
%       " it is expressed in W.m^-2">>];
%%
help(title, light_dialog) ->
    ?__(14,"Toxic Light Properties");
help(text, light_dialog) ->
    [?__(15,"Toxic only has one light type, pointlight, it produces true hard shadows."
      " The diffuse color is multiplied with the power value, "
      "to get the emission power in Watts."),
     ?__(16,"All Wings3D lights (except area lights) uses the Toxic point light.")].



