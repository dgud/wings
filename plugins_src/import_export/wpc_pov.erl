%%
%%  wpc_pov.erl --
%%
%%     POV-Ray Plugin User Interface.
%%
%%  Copyright (c) 2007-2011 Chris Hegarty
%%                2015 Micheus (porting to use wx dialogs)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_pov).

-export([init/0, menu/2, command/2, dialog/2]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-define(TAG, povray36).
-define(KEY(K), {?TAG, (K)}).
-define(TAG_RENDER, povray36_render).
-define(LOCAL_MODULE, ?MODULE).

-define(DEF_RENDERER, "c:/program files/POV-Ray for Windows v3.6/bin/pvengine.exe").
-define(DEF_LINUX_RENDERER, "povray").
-define(DEF_DIALOGS, auto).
-define(DEF_RENDER_FORMAT, png).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_RENDERARGS, "/NR /EXIT").
-define(DEF_LINUX_RENDERARGS, " ").

-define(MAX_VERTEX, 10).

%% Environment light
-define(DEF_AMBIENT_TYPE, none).
-define(DEF_BACKGROUND_AMBIENT, undefined).
-define(DEF_HORIZON_COLOR, {1.0,1.0,1.0}).
-define(DEF_HORIZON_ELEVATION, 30.0).
-define(DEF_ZENITH_COLOR, {0.4,0.5,1.0}).
-define(DEF_ZENITH_ELEVATION, 120.0).
-define(DEF_BACKGROUND_ROTATION, 0.0).
-define(DEF_BACKGROUND_FILENAME, "").
-define(DEF_BACKGROUND_POWER, 1.0).
-define(DEF_SKYBOX_SCALE, 1.0).
-define(DEF_TRANSPARENT_BACKGROUND, false).
-define(DEF_GAMMA_CORRECTION, 1.0).
-define(DEF_HDRI_GAMMA, 1.2).
-define(DEF_IMAGE_GAMMA, 2.2).

-record(camera_info, {pos, dir, up, fov}).

key(Key) -> {key, ?KEY(Key)}.
exkey(Key) -> {key, Key}.

colorlevel(M) -> key({colorlevel, M}).
colorvalue(M) -> key({colorvalue, M}).
%colordelete(M) -> key({colordelete, M}).
normallevel(M) -> key({normallevel, M}).
normalvalue(M) -> key({normalvalue, M}).
%normaldelete(M) -> key({normaldelete, M}).

range(T) -> {range, T}.

%%% Gets if os is win32
is_windows() ->
    T = os:type(),
    L = tuple_to_list(T),
    lists:member(win32, L).

export_matrix() ->
    e3d_mat:scale(-1.0, 1.0, 1.0).

export_transform(Contents) ->
    e3d_file:transform(Contents, export_matrix()).

export_transform_pos(Pos) ->
    e3d_mat:mul_point(export_matrix(), Pos).

export_transform_vec(Vec) ->
    e3d_mat:mul_vector(export_matrix(), Vec).

%%% initialize plugin with plugin preferences
init() ->
    ets:new(?LOCAL_MODULE, [named_table,public,ordered_set]),
    init_pref(),
    set_var(rendering, false),
    true.

%%% retrieve plugin preferences from preference file and store as global variables
init_pref() ->
    case is_windows() of
        true -> LocalRenderer = ?DEF_RENDERER;
        false -> LocalRenderer = ?DEF_LINUX_RENDERER
    end,

    Renderer = get_pref(renderer, LocalRenderer),
    RendererPath =
        case filename:pathtype(Renderer) of
            absolute ->
                case filelib:is_file(Renderer)of
                    false -> false;
                       _ -> Renderer
                end;
            _ ->
                case wings_job:find_executable(Renderer) of
                    false -> false;
                    Path -> Path
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
    set_var(use_emit_ambient, get_pref(use_emit_ambient, true)),

    case is_windows() of
        true -> LocalArgs = ?DEF_RENDERARGS;
        false -> LocalArgs = ?DEF_LINUX_RENDERARGS
    end,
    set_var(render_format, get_pref(render_format, ?DEF_RENDER_FORMAT)),
    set_var(renderargs, get_pref(renderargs, LocalArgs)),
    set_var(limit_vertices, get_pref(limit_vertices, true)),
    set_var(use_model_dim, get_pref(use_model_dim, false)),
    ok.

maybe_append(Condition, Menu, PluginMenu) ->
    case {is_plugin_active(Condition),Menu} of
        {_,[plugin_manager_category]} -> Menu++PluginMenu;
        {false,_} -> Menu;
        {_,_} -> Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
        export -> get_var(dialogs);
        render -> get_var(renderer)
    end.

%%% insert menu items into export, export selected, render, and plugin preferences
menu({file,export}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,export_selected}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,render}, Menu) ->
    maybe_append(render, Menu, menu_entry(render));
menu({edit,plugin_preferences}, Menu) ->
    Menu ++ menu_entry(pref);
menu(_, Menu) ->
    Menu.

menu_entry(export) ->
    [{?__(1, "POV-Ray")++"...", ?TAG}];
menu_entry(_) ->
    [{?__(1, "POV-Ray")++" (.pov)...", ?TAG}].

%%% dialog and file type properties
%%% POV-Ray output file formats: http://www.povray.org/documentation/view/3.6.0/219/
props(render, _Attr) ->
    RenderFormat = get_var(render_format),
    ExtsInfo = wings_job:render_formats(),
    {value, {_, Ext, Desc}} = lists:keysearch(RenderFormat, 1, ExtsInfo),
    Title = case os:type() of
                {win32, _} -> "Render";
                _Other -> ?__(1, "Render")
            end,
    Exts = get_ext_info(ExtsInfo),
    [{title, Title}, {ext, Ext}, {ext_desc, Desc}, {extensions, Exts}];
props(export, _Attr) ->
    {Title, File} = case os:type() of
                        {win32, _} -> {"Export", "POV-Ray File"};
                        _Other -> {?__(2, "Export"), ?__(3, "POV-Ray File")}
                    end,
    [{title, Title}, {ext, ".pov"}, {ext_desc, File}];
props(export_selected, _Attr) ->
    {Title, File} = case os:type() of
                        {win32, _} -> {"Export", "POV-Ray File"};
                        _Other -> {?__(4, "Export Selected"), ?__(3, "POV-Ray File")}
                    end,
    [{title, Title}, {ext, ".pov"}, {ext_desc, File}].

%%% menu commands
command({file, {Option, ?TAG}}, St) ->
    do_export(Option, St);
command({file, {export, {?TAG, A}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(A, export, Exporter, St);
command({file, {export_selected, {?TAG, A}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(A, export_selected, Exporter, St);
command({file, {render, {?TAG, A}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(A, render, Exporter, St);
command({edit, {plugin_preferences, ?TAG}}, St) ->
    pref_dialog(St);
command(_Spec, _St) ->
    next.

%%% Material / Light Dialogs
dialog({material_editor_setup, Name, Mat}, Dialog) ->
    case get_var(dialogs) of
        false-> Dialog;
        _ -> Dialog++[{?__(1,"POV-Ray"), material_dialog(Name, Mat)}]
    end;
dialog({light_editor_setup, Name, Ps}, Dialog) ->
    case get_var(dialogs) of
        false -> Dialog;
        _ ->
            POVDlg = light_dialog(Name, Ps),
            Dialog ++ [{?__(1,"POV-Ray"), {vframe, POVDlg}}]
    end;
dialog({material_editor_result, Name, Mat}, Res) ->
    case get_var(dialogs) of
        false -> {Mat, Res};
        _ -> material_result(Name, Mat, Res)
    end;
dialog({light_editor_result, Name, Ps}, Res) ->
    case get_var(dialogs) of
        false -> {Ps, Res};
        _ -> light_result(Name, Ps, Res)
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X, Dialog}]),
    Dialog.


%%% Create Preferences dialog
pref_dialog(St) ->
    case is_windows() of
        true -> LocalRenderer = ?DEF_RENDERER,
            LocalArgs = ?DEF_RENDERARGS;
        false -> LocalRenderer = ?DEF_LINUX_RENDERER,
            LocalArgs = ?DEF_LINUX_RENDERARGS
    end,

    [{dialogs, Dialogs}, {renderer, Renderer}, {use_emit_ambient, UseEmitAmbient},
        {renderargs, RenderArgs}, {limit_vertices, LimitVertices}, {use_model_dim, UseModelDim}] =
        get_user_prefs([{dialogs, ?DEF_DIALOGS}, {renderer, LocalRenderer}, {use_emit_ambient, true},
            {renderargs, LocalArgs}, {limit_vertices, true}, {use_model_dim, false}]),

    Dialog =
        [{vframe, [
            {hframe, [
                {menu, [
                    {?__(1, "Disabled Dialogs"), disabled},
                    {?__(2, "Automatic Dialogs"), auto},
                    {?__(3, "Enabled Dialogs"), enabled}
                ], Dialogs, [{key, dialogs}]}
            ]},
            {label_column, [
                {?__(4, "Executable"),
                    {button, {text, Renderer, [{key, renderer}, {width,35}, wings_job:browse_props()]}}},
                {?__(5, "Arguments"),{text, RenderArgs, [{key, renderargs}]}}]},
            {vframe, [
                {?__(6, "Fix camera dimensions to model view dimensions"), UseModelDim, [{key, use_model_dim}]},
                {?__(7, "Use material Emit for POV-Ray Ambient"), UseEmitAmbient, [{key, use_emit_ambient}]},
                {?__(8, "Limit number of vertices, indices per line"), LimitVertices, [{key, limit_vertices}]}
            ]}
        ], [{title,""}]}],
    wpa:dialog(?__(9, "POV-Ray Options"), Dialog, fun(Attr) -> pref_result(Attr, St) end).

pref_result(Attr, St) ->
    set_user_prefs(Attr),
    OldVal = get_var(renderer),
    init_pref(),
    case get_var(renderer) of
        OldVal -> ok;
        false ->
            wings_menu:update_menu(file, {render, ?TAG}, delete);
        _ ->
            [{Label, _}] = menu_entry(render),
            wings_menu:update_menu(file, {render, ?TAG}, {append, -1, Label})
    end,
    St.

%%% Add necessary properties to lists, setup export function
do_export(Op, _St) ->
    wpa:dialog(true, ?__(1, "POV-Ray Export Options"), export_dialog(Op),
        fun(Res) ->
            {file, {Op, {?TAG, Res}}}
        end).
do_export(Attr, Op, Exporter, St) when is_list(Attr) ->
    set_prefs(Attr),

    %% Basic additional settings
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Tesselation = proplists:get_value(tesselation, Attr, none),
    ExportUV = proplists:get_value(include_uvs, Attr, true),
    ExportVC = proplists:get_value(include_colors, Attr, true),
    Ps = [{tesselation, Tesselation}, {subdivisions, SubDivs},
        {include_uvs, ExportUV}, {include_colors, ExportVC} | props(Op, Attr)],
    %% Add Camera, lights to list
    [{Pos, Dir, Up}, Fov] = wpa:camera_info([pos_dir_up, fov]),
    Pos1 = export_transform_pos(Pos),
    Dir1 = export_transform_vec(Dir),
    Up1 = export_transform_vec(Up),
    CameraInfo = #camera_info{pos = Pos1, dir = Dir1, up = Up1, fov = Fov},
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

%%% Open files, do the actual export, check for render operation and launch render if necessary
export(Filename, Contents, Attr) ->
    wpa:popup_console(),
    ExportTS = os:timestamp(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    RenderFormat = get_var(render_format),
    ExportDir = filename:dirname(Filename),

    ContentsXForm = export_transform(Contents),
    #e3d_file{objs = Objs, mat = Mats, creator = Creator} = ContentsXForm,

    case Render of
        false -> ExportFile = Filename;
        true -> ExportFile = filename:rootname(Filename) ++ ?__(1, "_export")++".pov"
    end,

    Width = proplists:get_value(width, Attr, 320),
    Height = proplists:get_value(height, Attr, 240),

    #camera_info{fov = Fov} = proplists:lookup(camera_info, Attr),
    Depth = (float(Height) / 2.0) / math:tan((Fov / 2.0) * math:pi() / 180.0),
    CorrectedFOV = 2.0 * math:atan((float(Width) / 2.0) / Depth) * 180.0 / math:pi(),

    {ok, F} = file:open(ExportFile, [write]),

    io:format(F, ?__(2, "// ~s: Exported from ~s \n\n"), [filename:basename(ExportFile), Creator]),
    io:put_chars(F, "#include \"rad_def.inc\"\n\n"),

    io:put_chars(F, "global_settings{\n"),
    export_global(F, Attr),
    io:put_chars(F, "}\n"),
    {Br, Bg, Bb} = proplists:get_value(background, Attr, {0.0, 0.0, 0.0}),
    case proplists:get_value(transp_bkg, Attr, ?DEF_TRANSPARENT_BACKGROUND) of
        false -> io:format(F, "background { rgb <~f, ~f, ~f> }\n", [Br, Bg, Bb]);
        true -> io:format(F, "background { rgbt <~f, ~f, ~f, 1.0> }\n", [Br, Bg, Bb])
    end,

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
            io:put_chars(?__(3, "PovRay executable path not set\n")),
            wings_job:export_done(ExportTS),
            io:nl();
        {Renderer, true} ->
            Threads = proplists:get_value(threads_number, Attr, 4),
            {RenderFormatId, RenderFormatStr} = get_output_param(Filename),
            set_var(render_format, RenderFormatId),
            set_user_prefs([{render_format, RenderFormatId}]),  % it ensures global preference is updated
            ArgStr = wings_job:quote(filename:basename(ExportFile)) ++ " +W" ++ wings_job:quote(integer_to_list(Width)) ++
                case proplists:get_value(render_quality, Attr, none) of
                    none -> [];
                    quality1 -> " +Q1";
                    quality3 -> " +Q3";
                    quality4 -> " +Q4";
                    quality5 -> " +Q5";
                    quality7 -> " +Q7";
                    quality8 -> " +Q8";
                    quality11 -> " +Q11"
                end ++
                " +H" ++ wings_job:quote(integer_to_list(Height)) ++
                case proplists:get_value(transp_bkg, Attr, ?DEF_TRANSPARENT_BACKGROUND) of
                    true -> " +UA";
                    false -> []
                end ++
                " " ++ RenderFormatStr ++
                " +o" ++ wings_job:quote(filename:basename(Filename)) ++
                case proplists:get_value(antialias, Attr, false) of
                    false -> [];
                    true ->
                        " +A" ++ wings_util:nice_float(proplists:get_value(aa_threshold, Attr, 0.3)) ++
                        " +R" ++ integer_to_list(proplists:get_value(aa_depth, Attr, 3)) ++
                        case proplists:get_value(aa_jitter, Attr, false) of
                            false -> [];
                            true -> " +J"
                        end ++
                        case proplists:get_value(aa_method, Attr, false) of
                            aa_type1 -> []; % default
                            aa_type2 -> " +AM2"
                        end
                end ++
                case proplists:get_value(threads_auto, Attr, true) of
                    true -> [];
                    false -> " +WT" ++ integer_to_list(Threads)
                end,

            RA = get_var(renderargs),

            PortOpts = [{cd, filename:dirname(ExportFile)}],
            Handler =
                fun(Status) ->
                    set_var(rendering, false),
                    case Status of
                        ok -> {RenderFormat, Filename};
                        _ -> Status
                    end
                end,
            file:delete(Filename),
            set_var(rendering, true),
            wings_job:render(ExportTS, Renderer, ArgStr ++ " " ++ RA, PortOpts, Handler)
    end.

export_global(F, Attr) ->
    io:format(F, "\t assumed_gamma ~f\n", [2.2 / proplists:get_value(assumed_gamma, Attr, 1.0)]),
    io:format(F, "\t max_trace_level ~p\n", [proplists:get_value(max_trace_level, Attr, 5)]),
    {Ar, Ag, Ab} = proplists:get_value(ambient, Attr, {0.0, 0.0, 0.0}),
    Ap = proplists:get_value(ambient_power, Attr, 1.0),
    io:format(F, "\t ambient_light rgb <~f, ~f, ~f>\n", [Ar * Ap, Ag * Ap, Ab * Ap]),
    Radiosity = proplists:get_value(radiosity, Attr, none),
    case Radiosity of
        none -> ok;
        _ -> io:format(F, "\t radiosity { Rad_Settings(~s, off, off) }\n", [Radiosity])
    end,
    case proplists:get_value(sslt, Attr, false) of
        false -> ok;
        true ->
            io:put_chars(F, "\t subsurface {\n"),
            io:format(F, "\t\t samples ~p, ~p\n", [
                        proplists:get_value(sslt_diff_scatt, Attr, 50),
                        proplists:get_value(sslt_single_scatt, Attr, 50)]),
            case proplists:get_value(sslt_radiosity, Attr, false) of
                true -> io:put_chars(F, "\t\t radiosity true\n");
                false -> ok
            end,
            io:put_chars(F, "\t }\n")
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
export_interior(F, Attr) ->
    Fog = proplists:get_value(fog, Attr, false),
    Media = proplists:get_value(media, Attr, false),

    case Fog of
        false -> ok;
        true -> io:put_chars(F, "fog {\n"),
            io:format(F, "\t fog_type ~p\n", [proplists:get_value(fog_type, Attr, 1)]),
            io:format(F, "\t distance ~f\n", [proplists:get_value(fog_distance, Attr, 0.0)]),
            {Fr, Fg, Fb, Fa} = proplists:get_value(fog_color, Attr, {0.0, 0.0, 0.0, 0.0}),
            io:format(F, "\t rgbf <~f, ~f, ~f, ~f>\n", [Fr, Fg, Fb, 1.0 - Fa]),
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

export_camera(F, Attr, CorrectedFOV, Width, Height) ->
    #camera_info{pos = Pos, dir = Dir, up = Up} = proplists:lookup(camera_info, Attr),
    {Px, Py, Pz} = Pos,
    {Dx, Dy, Dz} = Dir,
    {Ux, Uy, Uz} = Up,

    CamType = proplists:get_value(camera_type, Attr, perspective),
    %% FOV, Width and Height information passed in to allow dimensions to be forced
    %% to geometry screen size
    Fov =
        case is_member(CamType, [fisheye, ultra_wide_angle, spherical]) of
            true -> proplists:get_value(cam_angle, Attr, 180.0);
            _ -> CorrectedFOV
        end,

    io:format(F,    "#declare camera_location = <~f, ~f, ~f>;\n", [Px, Py, Pz]),
    io:put_chars(F, "camera{\n"),

    io:format(F,    "\t ~s\n", [atom_to_list(CamType)]),
    io:put_chars(F, "\t location camera_location\n"),

    io:format(F,    "\t right (~p / ~p) * x\n", [Width, Height]),
    io:put_chars(F, "\t up y\n"),
    io:format(F,    "\t angle ~f\n", [Fov]),
    io:format(F,    "\t sky <~f, ~f, ~f>\n", [Ux, Uy, Uz]),
    io:format(F,    "\t look_at <~f, ~f, ~f>\n", [Px + Dx, Py + Dy, Pz + Dz]),
    case proplists:get_value(aperture, Attr, 0.0) of
        0.0 -> ok;
        _ ->
            io:format(F, "\t aperture ~f\n", [proplists:get_value(aperture, Attr, 0.0)]),
            io:format(F, "\t blur_samples ~p\n", [proplists:get_value(blur_samples, Attr, 1)]),
            io:format(F, "\t focal_point <~f, ~f, ~f>\n", [Px + Dx, Py + Dy, Pz + Dz]),
            io:format(F, "\t variance ~f\n", [proplists:get_value(variance, Attr, 1.0/128)])
    end,

    io:put_chars(F, "}\n").

export_lights(_F, [], _I) ->
    ok;
export_lights(F, [Light | Lights], Index) ->
    set_var(gamma_macro, false),
    {Name, Ps} = Light,
    export_light(F, Name, Ps, Index),
    export_lights(F, Lights, Index + 1).

export_light(F, Name, Ps, Index) ->
    case proplists:get_value(visible, Ps, true) of
        true ->
            OpenGL = proplists:get_value(opengl, Ps, []),
            PovRay = proplists:get_value(?TAG, Ps, []),
            Type = proplists:get_value(type, OpenGL, []),
            case Type of
                ambient ->
                    export_light(F, Name, Type, OpenGL, PovRay);
                _ ->
                    io:format(F, "#declare ~s = light_source {\n", [clean_name("wl_" ++ integer_to_list(Index) ++ "_" ++ Name)]),
                    export_light_basics(F, OpenGL, PovRay),
                    export_light(F, Name, Type, OpenGL, PovRay),
                    io:put_chars(F, "}\n")
            end;
        _ -> ok
    end.
export_light_basics(F, OpenGL, PovRay) ->
    {Pxo, Pyo, Pzo} = get_light_pos(OpenGL),
    {Px, Py, Pz} = export_transform_pos({Pxo, Pyo, Pzo}),

    io:format(F, "\t <~f, ~f, ~f>\n", [Px, Py, Pz]),
    Power = proplists:get_value(light_power, PovRay, 1.0),
    {Dr, Dg, Db, _Da} = proplists:get_value(diffuse, OpenGL, {0.0, 0.0, 0.0, 0.0}),
    io:format(F, "\t color rgb <~f, ~f, ~f>\n", [Dr * Power, Dg * Power, Db * Power]),
    io:format(F, "\t fade_distance ~f\n", [proplists:get_value(fade_distance, PovRay, 1.0)]),
    io:format(F, "\t fade_power ~f\n", [proplists:get_value(fade_power, PovRay, 0.0)]),
    case proplists:get_value(media_interaction, PovRay, true) of
        true -> io:put_chars(F, "\t media_interaction on\n");
        false -> io:put_chars(F, "\t media_interaction off\n")
    end,
    case proplists:get_value(media_attenuation, PovRay, false) of
        true -> io:put_chars(F, "\t media_attenuation on\n");
        false -> io:put_chars(F, "\t media_attenuation off\n")
    end,
    case proplists:get_value(shadows, PovRay, true) of
        true -> ok;
        false -> io:put_chars(F, "\t shadowless\n")
    end,
    case proplists:get_value(photons, PovRay, false) of
        false -> ok;
        true -> io:put_chars(F, "\t photons {\n"),
            case proplists:get_value(refraction, PovRay, true) of
                true -> io:put_chars(F, "\t\t refraction on\n");
                false -> io:put_chars(F, "\t\t refraction off\n")
            end,
            case proplists:get_value(reflection, PovRay, true) of
                true -> io:put_chars(F, "\t\t reflection on\n");
                false -> io:put_chars(F, "\t\t reflection off\n")
            end,
            io:put_chars(F, "\t }\n")
    end.

export_light(F, _Name, spot, OpenGL, PovRay) ->
    case proplists:get_value(cylinder, PovRay, false) of
        false -> io:put_chars(F, "\t spotlight\n");
        _ -> io:put_chars(F, "\t cylinder\n")
    end,
    Radius = proplists:get_value(cone_angle, OpenGL, 30.0),
    io:format(F, "\t radius ~f\n", [Radius]),
    case proplists:get_value(spot_exponent, OpenGL, 0.0) > Radius of
        true -> io:format(F, "\t falloff ~f\n", [proplists:get_value(spot_exponent, OpenGL, 0.0)]);
        false -> io:format(F, "\t falloff ~f\n", [Radius])
    end,
    io:format(F, "\t tightness ~f\n", [proplists:get_value(tightness, PovRay, 0.0)]),
    {Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
    {Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
    io:format(F, "\t point_at <~f, ~f, ~f>\n", [Dx, Dy, Dz]);

export_light(F, _Name, area, OpenGL, PovRay) ->
    io:put_chars(F, "\t area_light\n"),
    case is_looks_like_light(OpenGL) of
        true -> export_light_1(F, looks_like, OpenGL, PovRay);
        _ -> export_light_1(F, area, OpenGL, PovRay)
    end,
    case proplists:get_value(adaptive, PovRay, none) of
        none -> ok;
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

export_light(F, _Name, infinite, OpenGL, _PovRay) ->
    io:put_chars(F, "\t parallel\n"),
    {Dxo, Dyo, Dzo} = proplists:get_value(aim_point, OpenGL, {0.0, 0.0, 0.0}),
    {Dx, Dy, Dz} = export_transform_pos({Dxo, Dyo, Dzo}),
    io:format(F, "\t point_at <~f, ~f, ~f>\n", [Dx, Dy, Dz]);
export_light(F, _Name, ambient, _OpenGL, PovRay) ->
    case proplists:get_value(bg_type, PovRay, undefined) of
        undefined -> ok;
        Type -> export_light_2(F, Type, PovRay)
    end;
export_light(_F, _Name, _Type, _OpenGL, _PovRay) ->
    ok.

export_light_1(F, area, OpenGL, PovRay) ->
    #e3d_mesh{vs = Vs} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    {{V1xo, V1yo, V1zo}, {V2xo, V2yo, V2zo}, {V3xo, V3yo, V3zo}, _V4} = VsT,
    {V1x, V1y, V1z} = export_transform_pos({V1xo, V1yo, V1zo}),
    {V2x, V2y, V2z} = export_transform_pos({V2xo, V2yo, V2zo}),
    {V3x, V3y, V3z} = export_transform_pos({V3xo, V3yo, V3zo}),
    io:format(F, "\t <~f, ~f, ~f>, <~f, ~f, ~f>, ~p, ~p\n", [V1x - V2x, V1y - V2y, V1z - V2z,
        V3x - V2x, V3y - V2y, V3z - V2z, proplists:get_value(size_1, PovRay, 2), proplists:get_value(size_2, PovRay, 2)]);

export_light_1(F, looks_like, OpenGL, PovRay) ->
    Mesh = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    LightMesh = e3d_mesh:triangulate(Mesh),
    #e3d_mesh{fs = Fs0, ns = NTab, vs = VTab0} = e3d_mesh:vertex_normals(LightMesh),
    LimitVertex = get_var(limit_vertices),

    %Aim = proplists:get_value(aim_point, OpenGL, {0.0,0.0,0.0}),
    %% Aim should be used for orient the area plane and produce right shadows when "orient" is checked
    %% Due the current beharvior of Area light object, we are not able to pick the aim on the screen
    %% so, for now, we are just setting the area plane [oriented top-down]

    {{V1xo, _, V1zo}, {V3xo, _, V3zo}} = BB = e3d_bv:box(VTab0),
    {_, Vyc, _} = BBc = e3d_bv:center(BB),
    {V1x, V1y, V1z} = export_transform_pos({V1xo, Vyc, V1zo}),
    {V2x, V2y, V2z} = export_transform_pos({V3xo, Vyc, V1zo}),
    {V3x, V3y, V3z} = export_transform_pos({V3xo, Vyc, V3zo}),
    io:format(F, "\t <~f, ~f, ~f>, <~f, ~f, ~f>, ~p, ~p\n", [V1x - V2x, V1y - V2y, V1z - V2z,
        V3x - V2x, V3y - V2y, V3z - V2z, proplists:get_value(size_1, PovRay, 2), proplists:get_value(size_2, PovRay, 2)]),
    VTab = export_transform_vs(VTab0, BBc),
    VLen = float(length(VTab)),
    io:format(F, "\t looks_like { mesh2 {\n", []),
    io:format(F, "\t\t vertex_vectors { ~p", [length(VTab)]),
    case LimitVertex of
        true -> {Sx, Sy, Sz} = export_vectors(F, VTab, 0);
        false -> {Sx, Sy, Sz} = export_vectors(F, VTab)
    end,
    io:put_chars(F, "\t}\n"),

    case get_pref(export_normals, true) of
        false -> ok;
        true -> io:format(F, "\t\t normal_vectors { ~p", [length(NTab)]),
            case LimitVertex of
                true -> export_vectors(F, NTab, 0);
                false -> export_vectors(F, NTab)
            end,
            io:put_chars(F, "} \n")
    end,

    Am = ambient_multiplier(),
    io:format(F, "\t\t texture_list { 1, texture {\n", []),
    io:put_chars(F, "\t\t\t pigment{\n\t\t"),
    export_pigment(F, color, PovRay, OpenGL, [], [], ""),
    io:put_chars(F, "\t\t\t }\n"),
    io:put_chars(F, "\t\t\t finish {\n"),
    {Vr, Vg, Vb, _} = proplists:get_value(ambient, OpenGL, {1.0, 1.0, 1.0, 1.0}),
    io:format(F, "\t\t\t\t ambient rgb <~f, ~f, ~f>  *~f\n", [Vr, Vg, Vb, Am]),
    {Dr, Dg, Db, _} = proplists:get_value(diffuse, OpenGL, {1.0, 1.0, 1.0, 1.0}),
    {_Hd, _Sd, Vd} = wings_color:rgb_to_hsv(Dr, Dg, Db),
    io:format(F, "\t\t\t\t diffuse ~f\n", [Vd]),
    {Sr, Sg, Sb, _} = proplists:get_value(specular, OpenGL, {1.0, 1.0, 1.0, 1.0}),
    {_Hs, _Ss, Vs} = wings_color:rgb_to_hsv(Sr, Sg, Sb),
    io:format(F, "\t\t\t\t specular ~f\n", [Vs]),
    io:format(F, "\t\t\t }\n\t\t } }\n", []),

    Mat = e3d_mesh:used_materials(LightMesh),
    MatList = arrange_texture_list(Mat, 0),
    io:format(F, "\t\t face_indices { ~p", [length(Fs0)]),
    case LimitVertex of
        true -> export_faces(F, Fs0, MatList, [], 0);
        false -> export_faces(F, Fs0, MatList, [])
    end,

    case get_pref(export_normals, true) of
        false -> ok;
        true ->
            io:format(F, "}\n\t\t normal_indices { ~p", [length(Fs0)]),
            case LimitVertex of
                true -> export_normal_indices(F, Fs0, 0);
                false -> export_normal_indices(F, Fs0)
            end
    end,

    io:format(F, "\n\t\t //#local average_center = <~f, ~f, ~f>;\n\t\t }\n", [Sx / VLen, Sy / VLen, Sz / VLen]),
    io:put_chars(F, "\t } }\n").

export_light_2(F, gradient, PovRay) ->
    {Hr, Hg, Hb} = proplists:get_value(bg_horizon_color, PovRay, ?DEF_HORIZON_COLOR),
    {Zr, Zg, Zb} = proplists:get_value(bg_zenith_color, PovRay, ?DEF_ZENITH_COLOR),
    HorizonElev = proplists:get_value(bg_horizon_elev, PovRay, ?DEF_HORIZON_COLOR),
    ZenithElev = proplists:get_value(bg_zenith_elev, PovRay, ?DEF_ZENITH_ELEVATION),
    io:put_chars(F, "sky_sphere{\n"),
    io:put_chars(F, "\tpigment {\n"),
    io:put_chars(F, "\t\tgradient y\n"),
    io:put_chars(F, "\t\tcolor_map {\n"),
    io:format(F,    "\t\t\t[(1-cos(radians(~p)))/2 color <~p,~p,~p>]  // horizon\n", [HorizonElev, Hr, Hg, Hb]),
    io:format(F,    "\t\t\t[(1-cos(radians(~p)))/2 color <~p,~p,~p>]  // zenith\n", [ZenithElev, Zr, Zg, Zb]),
    io:put_chars(F, "\t\t}\n"),
    io:put_chars(F, "\t\tscale 2\n"),
    io:put_chars(F, "\t\ttranslate -1\n"),
    io:put_chars(F, "\t}\n"),
    io:put_chars(F, "}\n\n");
export_light_2(F, skybox, PovRay) ->
    FileName =proplists:get_value(bg_filename_image, PovRay, ?DEF_BACKGROUND_FILENAME),
    ImgType = get_map_type(FileName),
    BgRotation = proplists:get_value(bg_rotation, PovRay, ?DEF_BACKGROUND_ROTATION),
    BgScale = proplists:get_value(bg_box_scale, PovRay, ?DEF_SKYBOX_SCALE),
    Gamma = proplists:get_value(bg_gamma_correction, PovRay, ?DEF_GAMMA_CORRECTION),
    Power = proplists:get_value(bg_power, PovRay, ?DEF_BACKGROUND_POWER),
    TranspBg = get_pref(transp_bkg, ?DEF_TRANSPARENT_BACKGROUND),
    export_gamma_macro(F, get_var(gamma_macro)),
    io:put_chars(F, "box{ <-1, -1, -1>,< 1, 1, 1>\n"),
    io:put_chars(F, "\ttexture{ uv_mapping\n"),
    io:put_chars(F, "\t\tCorrect_Pigment_Gamma( // gamma correction (needed)\n"),
    io:put_chars(F, "\t\t\tpigment{\n"),
    io:format(F,    "\t\t\t\timage_map{ ~p \"~ts\"\n", [ImgType, FileName]),
    io:put_chars(F, "\t\t\t\t\tmap_type 0     //  planar\n"),
    io:put_chars(F, "\t\t\t\t\tinterpolate 2  //  bilinear\n"),
    io:put_chars(F, "\t\t\t\t\tonce\n"),
    io:put_chars(F, "\t\t\t\t}\n"),
    io:put_chars(F, "\t\t\t}\n"),
    io:format(F,    "\t\t, ~p) // New_Gamma\n",[Gamma]),
    io:format(F,    "\t\tfinish { ambient 0 diffuse 1 emission ~p}\n", [Power]),
    io:put_chars(F, "\t}\n"),
    io:format(F,    "\trotate<0,~p,0>\n",[BgRotation]),
    io:format(F,    "\tscale 10000 * ~p\n",[BgScale]),
    case TranspBg of
        true ->
            io:put_chars(F, "\thollow\n"),
            io:put_chars(F, "\tno_image\n");
        _ -> ok
    end,
    io:put_chars(F, "}\n\n");
export_light_2(F, skydome, PovRay) ->
    FileName =proplists:get_value(bg_filename_image, PovRay, ?DEF_BACKGROUND_FILENAME),
    export_light_2_0(FileName, F, PovRay);
export_light_2(F, hdri, PovRay) ->
    FileName =proplists:get_value(bg_filename_hdri, PovRay, ?DEF_BACKGROUND_FILENAME),
    export_light_2_0(FileName, F, PovRay).

export_light_2_0(FileName, F, PovRay) ->
    ImgType = get_map_type(FileName),
    BgRotation = proplists:get_value(bg_rotation, PovRay, ?DEF_BACKGROUND_ROTATION),
    BgScale = proplists:get_value(bg_box_scale, PovRay, ?DEF_SKYBOX_SCALE),
    Gamma = proplists:get_value(bg_gamma_correction, PovRay, ?DEF_GAMMA_CORRECTION),
    Power = proplists:get_value(bg_power, PovRay, ?DEF_BACKGROUND_POWER),
    TranspBg = get_pref(transp_bkg, ?DEF_TRANSPARENT_BACKGROUND),
    export_gamma_macro(F, get_var(gamma_macro)),
    io:put_chars(F, "sphere{0,1\n"),
    io:put_chars(F, "\tCorrect_Pigment_Gamma( // gamma correction (needed)\n"),
    io:put_chars(F, "\t\tpigment{\n"),
    io:format(F,    "\t\t\timage_map{ ~p \"~ts\"\n", [ImgType, FileName]),
    io:put_chars(F, "\t\t\t\tmap_type 1    //  spherical\n"),
    io:put_chars(F, "\t\t\t\tinterpolate 2 //  bilinear\n"),
    io:put_chars(F, "\t\t\t\tonce\n"),
    io:put_chars(F, "\t\t\t}\n"),
    io:put_chars(F, "\t\tscale<1,1.02,1>\n"),
    io:format(F,    "\t\trotate<0,~p,0>\n",[BgRotation]),
    io:put_chars(F, "\t}\n"),
    io:format(F,    "\t, ~p) // New_Gamma\n",[Gamma]),
    io:format(F,    "\tfinish { ambient 0 diffuse 1 emission ~p}\n", [Power]),
    io:format(F,    "\tscale 10000 * ~p\n",[BgScale]),
    case TranspBg of
        true ->
            io:put_chars(F, "\thollow\n"),
            io:put_chars(F, "\tno_image\n");
        _ -> ok
    end,
    io:put_chars(F, "}\n\n").

export_gamma_macro(_F, true) -> ok;
export_gamma_macro(F, false) ->
    io:put_chars(F, "//------------------------------------------------\n"),
    io:put_chars(F, "// Macro for the adjustment of images\n"),
    io:put_chars(F, "// for image_map with assumed_gamma = 1.0\n"),
    io:put_chars(F, "// Reference: http://www.f-lohmueller.de/pov_tut/backgrnd/p_sky9.htm\n"),
    io:put_chars(F, "// by Friedrich A. Lohmüller\n"),
    io:put_chars(F, "//\n"),
    io:put_chars(F, "#macro Correct_Pigment_Gamma(Orig_Pig, New_G)\n"),
    io:put_chars(F, "\t#local Correct_Pig_fn = function{ pigment {Orig_Pig} }\n"),
    io:put_chars(F, "\tpigment{\n"),
    io:put_chars(F, "\t\taverage pigment_map{\n"),
    io:put_chars(F, "\t\t[function{ pow(Correct_Pig_fn(x,y,z).x, New_G)}\n"),
    io:put_chars(F, "\t\t color_map{[0 rgb 0][1 rgb<3,0,0>]}]\n"),
    io:put_chars(F, "\t\t[function{ pow(Correct_Pig_fn(x,y,z).y, New_G)}\n"),
    io:put_chars(F, "\t\t color_map{[0 rgb 0][1 rgb<0,3,0>]}]\n"),
    io:put_chars(F, "\t\t[function{ pow(Correct_Pig_fn(x,y,z).z, New_G)}\n"),
    io:put_chars(F, "\t\t color_map{[0 rgb 0][1 rgb<0,0,3>]}]\n"),
    io:put_chars(F, "\t\t}\n"),
    io:put_chars(F, "\t}\n"),
    io:put_chars(F, "#end //\n"),
    io:put_chars(F, "//------------------------------------------------\n\n"),
    set_var(gamma_macro, true).

%%% try to find out the ambient light multiplier in order to make the object glowing like we see in Wings3d.
%%% By experiments, the multiplier should have the integer part with the same length for the minor
%%% value present at one of the components of RGB color (from global ambient color) shifted right.
%%% e.g. 0.001 requires a multiplier 1000; 0.01 requires 100
ambient_multiplier() ->
    {Ar, Ag, Ab} = get_pref(ambient, {0.0, 0.0, 0.0}),
    Ap0 = get_pref(ambient_power, 1.0),
    case Ar * Ag * Ab * Ap0 of
        0.0 -> 1.0;
        _ ->
            {Apr, Apg, Apb} = {Ar * Ap0, Ag * Ap0, Ab * Ap0},
            Ap1 = min(Apr, min(Apg, Apb)),
            if (Ap1 < 1.0) ->
                case trunc(1.0 / Ap1) of
                    0 -> 1.0;
                    H -> (H / list_to_float("0." ++ integer_to_list(H))) / 10
                end;
                true -> 1.0
            end
    end.

export_transform_vs(Vs, To) ->
    lists:foldl(fun(Pos, A) ->
        A ++ [export_transform_pos(e3d_vec:sub(Pos, To))]
    end, [], Vs).


get_light_pos(OpenGL) ->
    #e3d_mesh{vs = Vs} = Mesh = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    case is_looks_like_light(Mesh) of
        true ->
            e3d_bv:center(e3d_bv:box(Vs));
        _ ->
            proplists:get_value(position, OpenGL, {0.0, 0.0, 0.0})
    end.

is_looks_like_light(#e3d_mesh{vs = Vs}) -> length(Vs) > 4;
is_looks_like_light(OpenGL) ->
    is_looks_like_light(proplists:get_value(mesh, OpenGL, #e3d_mesh{})).

export_lights_objects(_F, [], _I) ->
    ok;
export_lights_objects(F, [Light | Lights], Index) ->
    {Name, Ps} = Light,
    export_light_object(F, Name, Ps, Index),
    export_lights_objects(F, Lights, Index + 1).

export_light_object(F, Name, Ps, Index) ->
    case proplists:get_value(visible, Ps, true) of
        true ->
            OpenGL = proplists:get_value(opengl, Ps, []),
            Type = proplists:get_value(type, OpenGL, []),
            case Type of
                ambient -> ok;
                _ ->
                    io:format(F, "object{ ~s\n", [clean_name("wl_" ++ integer_to_list(Index) ++ "_" ++ Name)]),
                    io:put_chars(F, "}\n")
            end;
        _ -> ok
    end.

export_materials(_F, [], _Attr, _ExportDir) ->
    ok;
export_materials(F, [{Name, Mat} | Mats], Attr, ExportDir) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    PovRay = proplists:get_value(?TAG, Mat, []),

    Ghost = proplists:get_value(ghost_material, PovRay, false),

    case Ghost of
        true -> ok;
        false ->

            io:format(F, "#declare ~s = texture{\n", [clean_name("wm_" ++ atom_to_list(Name))]),

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
                false -> ok;
                true -> export_pigment_modifiers(F, Pigment, PovRay)
            end,
            case proplists:get_value(pigment_colormap, PovRay, false) of
                false -> ok;
                true -> export_colormap(F, Pigment, PovRay)
            end,
            io:put_chars(F, "\t }\n"),

            %normal
            Normal = proplists:get_value(normal_pattern, PovRay, none),
            case Normal of
                none -> Skip = true;
                average ->
                    case proplists:get_value(normal_normalmap, PovRay, false) of
                        false -> Skip = true;
                        true ->
                            case proplists:get_value(normalmap_list, PovRay, []) of
                                [] -> Skip = true;
                                _ -> Skip = false
                            end
                    end;
                _ -> Skip = false
            end,
            case Skip of
                true -> ok;
                _ ->
                    io:put_chars(F, "\t normal{\n"),
                    export_normal(F, Normal, PovRay, OpenGL, MapList, Attr, ExportDir),
                    case proplists:get_value(normal_modifiers, PovRay, false) of
                        false -> ok;
                        true -> export_normal_modifiers(F, Normal, PovRay)
                    end,
                    case proplists:get_value(normal_normalmap, PovRay, false) of
                        false -> ok;
                        true -> export_normalmap(F, Normal, PovRay)
                    end,
                    io:put_chars(F, "\t }\n")
            end,
            io:put_chars(F, "\t finish {\n"),
            export_finish(F, OpenGL, PovRay),
            io:put_chars(F, "\t }\n"),
            io:put_chars(F, "}\n")

    end,

    export_materials(F, Mats, Attr, ExportDir),
    ok.

export_finish(F, OpenGL, PovRay) ->
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
    {_H, _S, V} = wings_color:rgb_to_hsv(Sr, Sg, Sb),
    io:format(F, "\t\t specular ~f\n", [V]),
    io:format(F, "\t\t roughness ~f\n", [1.00001 - proplists:get_value(shininess, OpenGL, 1.0)]),
    case proplists:get_value(finish_phong, PovRay, 0.0) of
        0.0 -> ok;
        Phong ->
            io:format(F, "\t\t phong ~f\n", [Phong]),
            io:format(F, "\t\t phong_size ~f\n", [proplists:get_value(finish_phong_size, PovRay, 40.0)])
    end,
    case proplists:get_value(finish_irid_amount, PovRay, 0.0) of
        0.0 -> ok;
        Amount ->
            io:format(F, "\t\t irid { ~f\n", [Amount]),
            io:format(F, "\t\t\t thickness ~f\n", [proplists:get_value(finish_irid_thickness, PovRay, 0.2)]),
            io:format(F, "\t\t\t turbulence ~f\n", [proplists:get_value(finish_irid_turbulence, PovRay, 0.15)]),
            io:put_chars(F, "\t\t }\n")
    end,
    SSLTMul = proplists:get_value(finish_sslt_multiplier, PovRay, 0.0),
    case {get_pref(sslt, false), SSLTMul} of
        {_, 0.0} -> ok;
        {true, SSLTMul} ->
            {Tr, Tg, Tb} = proplists:get_value(finish_sslt_color, PovRay, {1.0, 1.0, 1.0}),
            io:put_chars(F, "\t\t subsurface { \n"),
            io:format(F,    "\t\t\t translucency <~f, ~f, ~f> * ~f\n", [max(Tr,0.001), max(Tg,0.001), max(Tb,0.001), SSLTMul]),
            io:put_chars(F, "\t\t }\n");
        _ -> ok
    end,
    case proplists:get_value(reflection_enabled, PovRay, false) of
        false -> ok;
        true -> io:put_chars(F, "\t\t reflection {\n"),
            {Mir, Mig, Mib} = proplists:get_value(reflection_minimum, PovRay, {0.0, 0.0, 0.0}),
            {Mar, Mag, Mab} = proplists:get_value(reflection_maximum, PovRay, {0.0, 0.0, 0.0}),
            io:format(F, "\t\t\t color rgb <~f, ~f, ~f> ", [Mir, Mig, Mib]),
            case proplists:get_value(reflection_variable, PovRay, false) of
                false -> io:put_chars(F, "\n");
                true -> io:format(F, "color <~f, ~f, ~f>\n", [Mar, Mag, Mab])
            end,
            case proplists:get_value(reflection_fresnel, PovRay, false) of
                false -> ok;
                true -> io:put_chars(F, "\t\t\t fresnel\n")
            end,
            io:format(F, "\t\t\t falloff ~f\n", [proplists:get_value(reflection_falloff, PovRay, 1.0)]),
            io:format(F, "\t\t\t exponent ~f\n", [proplists:get_value(reflection_exponent, PovRay, 1.0)]),
            io:format(F, "\t\t\t metallic ~f\n", [proplists:get_value(reflection_metallic, PovRay, 0.0)]),
            io:put_chars(F, "\t\t }\n")
    end.

export_maps([], _ED) ->
    [];
export_maps([{MapID, Map} | Maps], ExportDir) ->
    #e3d_image{name = ImageName, filename = FileName} = Map,
    case FileName of
        none ->  % Internal image
            MapFile = case get_map_type(ImageName) of
                          sys -> ImageName ++ ".png";
                          _ -> ImageName
                      end,
            Filepath0 = filename:join(ExportDir, MapFile),
            case e3d_image:save(Map, Filepath0) of
                {error, _} -> % file type not supported by Wings3d
                    Filepath = filename:join(ExportDir, set_map_type(ImageName,".png")),
                    e3d_image:save(Map, Filepath);
                _ -> Filepath = Filepath0
            end;
        _ -> Filepath = FileName
    end,
    [{MapID, Filepath} | export_maps(Maps, ExportDir)].

get_map_type(Filepath) ->
    Ext = filename:extension(Filepath),
    case Ext of
        ".jpg" -> jpeg;
        ".png" -> png;
        ".bmp" -> sys;
        ".gif" -> gif;
        ".iff" -> iff;
        ".tiff" -> tiff;
        ".tga" -> tga;
        ".exr" -> exr;
        ".hdr" -> hdr;
        _ -> sys
    end.

%%% Ext parameter must include the "." - ex. ".jpg"
set_map_type(Filepath0,Ext) ->
    Filepath = filename:rootname(Filepath0),
    Filepath ++ Ext.

export_pigment_modifiers(F, Type, PovRay) ->
    case Type of
        color -> ok;
        image -> ok;
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
        _ -> Filter = "rgbt"
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
        brick -> {Br, Bg, Bb, Ba} = proplists:get_value(brick_color, PovRay, {1.0, 0.0, 0.0, 1.0}),
            {Mr, Mg, Mb, Ma} = proplists:get_value(mortar_color, PovRay, {1.0, 1.0, 1.0, 1.0}),
            io:format(F, "\t\t brick\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n", [Filter, Mr, Mg, Mb, 1.0 - Ma, Filter, Br, Bg, Bb, 1.0 - Ba]),
            io:format(F, "\t\t brick_size <~f, ~f, ~f>\n\t\t mortar ~f\n", [proplists:get_value(brick_size_x, PovRay, 7.5),
                proplists:get_value(brick_size_y, PovRay, 2.5), proplists:get_value(brick_size_z, PovRay, 4.0), proplists:get_value(mortar_size, PovRay, 0.5)]);
        checker -> {C1r, C1g, C1b, C1a} = proplists:get_value(checker_color1, PovRay, {1.0, 1.0, 1.0, 1.0}),
            {C2r, C2g, C2b, C2a} = proplists:get_value(checker_color2, PovRay, {0.0, 1.0, 0.0, 1.0}),
            io:format(F, "\t\t checker\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n", [Filter, C1r, C1g, C1b, 1.0 - C1a, Filter, C2r, C2g, C2b, 1.0 - C2a]);
        crackle ->
            io:format(F, "\t crackle\n\t metric ~p\n\toffset ~f\n", [proplists:get_value(crackle_metric, PovRay, 2),
                proplists:get_value(crackle_offset, PovRay, 0.0)]),
            case proplists:get_value(crackle_solid, PovRay, false) of
                false -> ok;
                true -> io:put_chars(F, "\t\t solid\n")
            end;
        gradient -> io:format(F, "\t\t gradient ~s\n", [atom_to_list(proplists:get_value(gradient_axis, PovRay, x))]);
        hexagon -> {H1r, H1g, H1b, H1a} = proplists:get_value(hexagon_color1, PovRay, {0.0, 0.0, 1.0, 1.0}),
            {H2r, H2g, H2b, H2a} = proplists:get_value(hexagon_color2, PovRay, {0.0, 1.0, 0.0, 1.0}),
            {H3r, H3g, H3b, H3a} = proplists:get_value(hexagon_color3, PovRay, {1.0, 0.0, 0.0, 1.0}),
            io:format(F, "\t\t hexagon\n\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n\t\t color ~s <~f, ~f, ~f, ~f>\n",
                [Filter, H1r, H1g, H1b, 1.0 - H1a, Filter, H2r, H2g, H2b, 1.0 - H2a, Filter, H3r, H3g, H3b, 1.0 - H3a]);
        quilted ->
            io:format(F, "\t\t quilted\n\t\t control0 ~f\n\t\t control1 ~f\n", [proplists:get_value(quilted_value1, PovRay, 1.0), proplists:get_value(quilted_value2, PovRay, 1.0)]);
        spiral1 -> io:format(F, "\t\t spiral1 ~p\n", [proplists:get_value(spiral_arms, PovRay, 5)]);
        spiral2 -> io:format(F, "\t\t spiral2 ~p\n", [proplists:get_value(spiral_arms, PovRay, 5)]);
        slope ->
            #camera_info{dir = Dir} = proplists:lookup(camera_info, Attr),
            {Dx, Dy, Dz} = Dir,
            io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0}\n", [Dx, Dy, Dz]);
        image -> io:put_chars(F, "\t\t image_map {\n"),
            case PigmentImage of
                user ->
                    Filepath = proplists:get_value(image_user_file, PovRay, []),
                    Filepath0 = wings_u:relative_path_name(ExportDir, Filepath),
                    io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath)), Filepath0]);
                PI ->
                    Filepath = proplists:get_value(PI, Maps, []),
                    Filepath0 = wings_u:relative_path_name(ExportDir, Filepath),
                    io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath0)), Filepath0])
            end,
            io:put_chars(F, "\t\t }\n");
        Pattern -> io:format(F, "\t\t ~s\n", [Pattern])
    end.

export_colormap(F, Type, PovRay) ->
    case proplists:get_value(finish_transparency, PovRay, filter) of
        filter -> Filter = "rgbf";
        _ -> Filter = "rgbt"
    end,

    case Type of
        color -> ok;
        image -> ok;
        checker -> ok;
        brick -> ok;
        hexagon -> ok;
        _ -> ColorMap = proplists:get_value(colormap_list, PovRay, []),
            io:put_chars(F, "\t\t color_map {\n"),
            export_color_entry(F, ColorMap, Filter),
            io:put_chars(F, "\t\t }\n")
    end.

export_color_entry(_F, [], _Filter) ->
    ok;
export_color_entry(F, [{Mag, Color} | Entries], Filter) ->
    {Cr, Cg, Cb, Ca} = Color,
    io:format(F, "\t\t\t [~f ~s <~f, ~f, ~f, ~f>]\n", [Mag, Filter, Cr, Cg, Cb, 1.0 - Ca]),
    export_color_entry(F, Entries, Filter).

export_normal_modifiers(F, Type, PovRay) ->
    case Type of
        none -> ok;
        image -> ok;
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
    %% retrieve the kind of pigment map, if any
    case proplists:is_defined(proplists:get_value(n_image_type, PovRay, user), Maps) of
        true -> NormalImage = proplists:get_value(n_image_type, PovRay, user);
        _ -> NormalImage = user
    end,

    Mag = proplists:get_value(normal_magnitude, PovRay, 0.5),
    NComp = proplists:get_value(normal_components, PovRay, false),

    case proplists:get_value(normal_normalmap, PovRay, false) of
        false -> NMap = false;
        true ->
            case proplists:get_value(normalmap_list, PovRay, []) of
                [] -> NMap = false;
                _ -> NMap = true
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
                false -> ok;
                true -> io:put_chars(F, "\t\t solid\n")
            end;
        gradient ->
            case NMap of
                false ->
                    io:format(F, "\t\t gradient ~s, ~f \n", [atom_to_list(proplists:get_value(n_gradient_axis, PovRay, x)), Mag]);
                true ->
                    io:format(F, "\t\t gradient ~s\n", [atom_to_list(proplists:get_value(n_gradient_axis, PovRay, x))])
            end;
        hexagon ->
            case NComp of
                false -> io:format(F, "\t\t hexagon ~f\n", [Mag]);
                true ->
                    H1 = proplists:get_value(hexagon_normal1, PovRay, agate),
                    H2 = proplists:get_value(hexagon_normal2, PovRay, agate),
                    H3 = proplists:get_value(hexagon_normal3, PovRay, agate),
                    io:put_chars(F, "\t\t hexagon\n"),
                    io:format(F, "\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n\t\t normal {~s ~f}\n", [atom_to_list(H1), Mag, atom_to_list(H2), Mag, atom_to_list(H3), Mag])
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
                true -> io:format(F, "\t\t spiral1 ~p\n", [proplists:get_value(n_spiral_arms, PovRay, 5)])
            end;
        spiral2 ->
            case NMap of
                false -> io:format(F, "\t\t spiral2 ~p, ~f\n", [proplists:get_value(n_spiral_arms, PovRay, 5), Mag]);
                true -> io:format(F, "\t\t spiral2 ~p\n", [proplists:get_value(n_spiral_arms, PovRay, 5)])
            end;
        slope ->
            #camera_info{dir = Dir} = proplists:lookup(camera_info, Attr),
            {Dx, Dy, Dz} = Dir,
            case NMap of
                false -> io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0} ~f\n", [Dx, Dy, Dz, Mag]);
                true -> io:format(F, "\t\t slope {<~f, ~f, ~f>, 0.5, 1.0}\n", [Dx, Dy, Dz])
            end;
        image -> io:put_chars(F, "\t\t uv_mapping bump_map {\n"),
            case NormalImage of
                user ->
                    Filepath = proplists:get_value(n_image_user_file, PovRay, []),
                    Filepath0 = wings_u:relative_path_name(ExportDir, Filepath),
                    io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath)), Filepath0]);
                PI ->
                    Filepath = proplists:get_value(PI, Maps, []),
                    Filepath0 = wings_u:relative_path_name(ExportDir, Filepath),
                    io:format(F, "\t\t ~s \"~s\"\n", [atom_to_list(get_map_type(Filepath0)), Filepath0])
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
export_normalmap(F, Type, PovRay) ->

    Mag = proplists:get_value(normal_magnitude, PovRay, 0.5),

    case Type of
        none -> ok;
        image -> ok;
        checker -> ok;
        brick -> ok;
        hexagon -> ok;
        facets -> ok;
        _ -> NormalMap = proplists:get_value(normalmap_list, PovRay, []),
            io:put_chars(F, "\t\t normal_map {\n"),
            export_normal_entry(F, NormalMap, Mag),
            io:put_chars(F, "\t\t }\n")
    end.

export_normal_entry(_F, [], _M) ->
    ok;
export_normal_entry(F, [{Mag, Normal} | Entries], NormalMag) ->
    io:format(F, "\t\t\t [~f ~s ~f]\n", [Mag, atom_to_list(Normal), NormalMag]),
    export_normal_entry(F, Entries, NormalMag).

export_objects(_F, [], _M, _A, _I) ->
    ok;
export_objects(F, [EObj | Objs], AllMats, Attr, Index) ->
    #e3d_object{name = Name, obj = Obj} = EObj,

    ObjMesh = e3d_mesh:triangulate(Obj),
    #e3d_mesh{fs = Fs0, ns = NTab, vs = VTab, tx = UVTab} = e3d_mesh:vertex_normals(ObjMesh),
    Mat = e3d_mesh:used_materials(ObjMesh),

    LimitVertex = get_var(limit_vertices),

    VLen = float(length(VTab)),
    io:format(F, "#declare ~s = mesh2{\n", [clean_name("wo_" ++ integer_to_list(Index) ++ "_" ++ Name)]),
    io:format(F, "\t vertex_vectors { ~p", [length(VTab)]),
    case LimitVertex of
        true -> {Sx, Sy, Sz} = export_vectors(F, VTab, 0);
        false -> {Sx, Sy, Sz} = export_vectors(F, VTab)
    end,
    io:put_chars(F, "}\n"),
    io:format(F, "\t //#local average_center = <~f, ~f, ~f>;\n", [Sx / VLen, Sy / VLen, Sz / VLen]),


    case proplists:get_value(export_normals, Attr, true) of
        false -> ok;
        true -> io:format(F, "\t normal_vectors { ~p", [length(NTab)]),
            case LimitVertex of
                true -> export_vectors(F, NTab, 0);
                false -> export_vectors(F, NTab)
            end,
            io:put_chars(F, "}\n")
    end,
    case proplists:get_value(include_uvs, Attr, true) of
        false -> ok;
        true -> io:format(F, "\t uv_vectors { ~p", [length(UVTab)]),
            case LimitVertex of
                true -> export_vectors2D(F, UVTab, 0);
                false -> export_vectors2D(F, UVTab)
            end,
            io:put_chars(F, "}\n")
    end,

    io:format(F, "\t texture_list { ~p", [length(Mat)]),
    export_texture_names(F, Mat, AllMats),
    io:put_chars(F, "}\n"),

    MatList = arrange_texture_list(Mat, 0),

    io:format(F, "\t face_indices { ~p", [length(Fs0)]),
    case LimitVertex of
        true -> MatIndices = export_faces(F, Fs0, MatList, [], 0);
        false -> MatIndices = export_faces(F, Fs0, MatList, [])
    end,
    io:put_chars(F, "}\n"),

    case proplists:get_value(export_normals, Attr, true) of
        false -> ok;
        true ->
            io:format(F, "\t normal_indices { ~p", [length(Fs0)]),
            case LimitVertex of
                true -> export_normal_indices(F, Fs0, 0);
                false -> export_normal_indices(F, Fs0)
            end,
            io:put_chars(F, "}\n")
    end,

    case proplists:get_value(include_uvs, Attr, true) of
        false -> ok;
        true -> io:format(F, "\t uv_indices { ~p", [length(Fs0)]),
            case LimitVertex of
                true -> export_uv_indices(F, Fs0, 0);
                false -> export_uv_indices(F, Fs0)
            end,
            io:put_chars(F, "}\n")
    end,

    io:put_chars(F, "}\n"),

    %% using the material on the most number of faces for interior and photons
    {Mi, _MC} = greatest_index(MatIndices, {0, 0}),
    InteriorMatName = name_of_index(Mat, Mi, 0),
    InteriorMat = material_of_name(InteriorMatName, AllMats),
    export_object_def(F, clean_name("wo_" ++ integer_to_list(Index) ++ "_" ++ Name), InteriorMat),

    export_objects(F, Objs, AllMats, Attr, Index + 1).


greatest_index([], {Mi, MC}) ->
    {Mi, MC};
greatest_index([{Index, Count} | List], {Mi, MC}) ->
    case Count > MC of
        true -> greatest_index(List, {Index, Count});
        false -> greatest_index(List, {Mi, MC})
    end.
name_of_index([], _I, _C) ->
    [];
name_of_index([Mat | Mats], Index, Current) ->
    case Index == Current of
        true -> Mat;
        false -> name_of_index(Mats, Index, Current + 1)
    end.
material_of_name(_I, []) ->
    {[], []};
material_of_name(InteriorMatName, [{MatName, Mat} | AllMats]) ->
    case MatName == InteriorMatName of
        true -> {MatName, Mat};
        false -> material_of_name(InteriorMatName, AllMats)
    end.

arrange_texture_list([], _I) ->
    [];
arrange_texture_list([Name | Mats], Index) ->
    [{Name, Index} | arrange_texture_list(Mats, Index + 1)].


export_vectors(_F, []) ->
    {0.0, 0.0, 0.0};
export_vectors(F, [{X, Y, Z} | List]) ->
    io:format(F, ", <~f, ~f, ~f>", [X, Y, Z]),
    {Sx, Sy, Sz} = export_vectors(F, List),
    {Sx + X, Sy + Y, Sz + Z}.

export_vectors(_F, [], _C) ->
    {0.0, 0.0, 0.0};
export_vectors(F, [{X, Y, Z} | List], Count) ->
    io:put_chars(F, ", "),
    case Count of
        ?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
            Next = 0;
        _ -> Next = Count + 1
    end,

    io:format(F, "<~f, ~f, ~f>", [X, Y, Z]),
    {Sx, Sy, Sz} = export_vectors(F, List, Next),
    {Sx + X, Sy + Y, Sz + Z}.


export_vectors2D(_F, []) ->
    ok;
export_vectors2D(F, [{X, Y} | List]) ->
    io:format(F, ", <~f, ~f>", [X, Y]),
    export_vectors2D(F, List).

export_vectors2D(_F, [], _C) ->
    ok;
export_vectors2D(F, [{X, Y} | List], Count) ->
    io:put_chars(F, ", "),
    case Count of
        ?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
            Next = 0;
        _ -> Next = Count + 1
    end,
    io:format(F, "<~f, ~f>", [X, Y]),
    export_vectors2D(F, List, Next).

export_texture_names(_F, [], _AM) ->
    ok;
export_texture_names(F, [Name | Mats], AllMats) ->
    %% find out if ghost material, don't decorate name if so
    {_N, Mat} = material_of_name(Name, AllMats),
    PovRay = proplists:get_value(?TAG, Mat, []),
    Ghost = proplists:get_value(ghost_material, PovRay, false),

    case Ghost of
        false -> io:format(F, ", texture{~s}", [clean_name("wm_" ++ atom_to_list(Name))]);
        true -> io:format(F, ", texture{~s}", [atom_to_list(Name)])
    end,
    export_texture_names(F, Mats, AllMats).

export_faces(_F, [], _ML, MI) ->
    MI;
export_faces(F, [#e3d_face{vs = Vs, mat = MatNames} | Faces], MatList, MatIndices) ->
    {X, Y, Z} = list_to_tuple(Vs),
    {M} = list_to_tuple(MatNames),

    %% pull the appropriate index based on the tex name
    Index = proplists:get_value(M, MatList, 1),

    %% increment the index count for a given material
    Count = proplists:get_value(Index, MatIndices, 0),
    DelMi = proplists:delete(Index, MatIndices),
    NewMI = [{Index, Count + 1} | DelMi],

    io:format(F, ", <~p, ~p, ~p>, ~p", [X, Y, Z, Index]),
    export_faces(F, Faces, MatList, NewMI).

export_faces(_F, [], _ML, MI, _C) ->
    MI;
export_faces(F, [#e3d_face{vs = Vs, mat = MatNames} | Faces], MatList, MatIndices, PerLine) ->
    {X, Y, Z} = list_to_tuple(Vs),
    {M} = list_to_tuple(MatNames),

    %% pull the appropriate index based on the tex name
    Index = proplists:get_value(M, MatList, 1),

    %% increment the index count for a given material
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

export_normal_indices(_F, []) ->
    ok;
export_normal_indices(F, [#e3d_face{ns = Ns} | Faces]) ->
    {X, Y, Z} = list_to_tuple(Ns),

    io:format(F, ", <~p, ~p, ~p>", [X, Y, Z]),
    export_normal_indices(F, Faces).

export_normal_indices(_F, [], _P) ->
    ok;
export_normal_indices(F, [#e3d_face{ns = Ns} | Faces], PerLine) ->
    {X, Y, Z} = list_to_tuple(Ns),

    io:put_chars(F, ", "),
    case PerLine of
        ?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
            Next = 0;
        _ -> Next = PerLine + 1
    end,

    io:format(F, "<~p, ~p, ~p>", [X, Y, Z]),
    export_normal_indices(F, Faces, Next).

export_uv_indices(_F, []) ->
    ok;
export_uv_indices(F, [#e3d_face{tx = Txs} | Faces]) ->
    case Txs of
        [] -> io:put_chars(F, ", <0, 0, 0>");
        _ ->
            {X, Y, Z} = list_to_tuple(Txs),
            io:format(F, ", <~p, ~p, ~p>", [X, Y, Z])
    end,
    export_uv_indices(F, Faces).

export_uv_indices(_F, [], _P) ->
    ok;
export_uv_indices(F, [#e3d_face{tx = Txs} | Faces], PerLine) ->

    io:put_chars(F, ", "),
    case PerLine of
        ?MAX_VERTEX -> io:put_chars(F, "\n\t\t\t"),
            Next = 0;
        _ -> Next = PerLine + 1
    end,

    case Txs of
        [] -> io:put_chars(F, "<0, 0, 0>");
        _ ->
            {X, Y, Z} = list_to_tuple(Txs),
            io:format(F, "<~p, ~p, ~p>", [X, Y, Z])
    end,
    export_uv_indices(F, Faces, Next).

export_object_def(F, Name, {_MatName, InteriorMat}) ->
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
                false -> ok;
                true -> io:put_chars(F, "\t\t target\n")
            end,
            case proplists:get_value(photons_collect, PovRay, true) of
                false -> io:put_chars(F, "\t\t collect off\n");
                true -> io:put_chars(F, "\t\t collect on\n")
            end,
            case proplists:get_value(photons_reflect, PovRay, true) of
                false -> io:put_chars(F, "\t\t reflection off\n");
                true -> io:put_chars(F, "\t\t reflection on\n")
            end,
            case proplists:get_value(photons_refract, PovRay, true) of
                false -> io:put_chars(F, "\t\t refraction off\n");
                true -> io:put_chars(F, "\t\t refraction on\n")
            end,
            io:put_chars(F, "\t }\n")
    end,
    io:put_chars(F, "}\n").

%%% Construct dialog with export options, use get_pref to load options
export_dialog(Op) ->
    Render =
        case Op of
            render -> true;
            _ -> false
        end,

    %get the focus window.  If it isn't a sub geom window, use the primary geom window
    {ViewW, ViewH} =
        case wings_wm:actual_focus_window() of
            {geom, N} -> wings_wm:win_size({geom, N});
            _ -> wings_wm:win_size(geom)
        end,
    VpDim = io_lib:format(" (~px~p)",[ViewW, ViewH]),

    {Width, Height} =
        case get_var(use_model_dim) of
            true -> {ViewW, ViewH};
            _ -> {get_pref(width, 320), get_pref(height, 240)}
        end,

    DimPreset =
        case get_pref(dim_preset, undefined) of
            undefined ->
                case get_var(use_model_dim) of
                    true -> viewport;
                    _ -> custom
                end;
            DimPreset0 -> DimPreset0
        end,

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            threads_auto ->
                wings_dialog:enable(threads_number, Value =:= false, Store);
            photons ->
                wings_dialog:enable(pnl_photons, Value =:= true, Store),
                wings_dialog:enable(media_photons, Value =:= true, Store),
                wings_dialog:enable(pnl_media_photons, Value and (wings_dialog:get_value(media_photons, Store) =:= true), Store);
            media_photons ->
                wings_dialog:enable(pnl_media_photons, Value =:= true, Store);
            fog ->
                wings_dialog:enable(pnl_fog_type, Value =:= true, Store),
                wings_dialog:enable(pnl_fog_params, Value =:= true, Store),
                wings_dialog:enable(pnl_fog, Value and (wings_dialog:get_value(fog_type, Store) =:= 2), Store);
            fog_type ->
                wings_dialog:enable(pnl_fog, Value =:= 2, Store);
            media ->
                wings_dialog:enable(pnl_media, Value =:= true, Store);
            sslt ->
                wings_dialog:enable(pnl_sslt, Value =:= true, Store);
            antialias ->
                wings_dialog:enable(aa_jitter, Value =:= true, Store),
                wings_dialog:enable(pnl_aa_params, Value =:= true, Store);
            camera_type ->
                wings_dialog:enable(pnl_angle, is_member(Value, [fisheye, ultra_wide_angle, spherical]), Store);
            dim_preset ->
		wings_dialog:show(pnl_ar, Value =:= aspect, Store),
	    	wings_dialog:enable(pnl_w, is_member(Value, [custom, aspect]), Store),
                wings_dialog:enable(pnl_h, Value =:= custom, Store),
                case Value of
                    aspect ->
                        W = wings_dialog:get_value(width, Store),
                        ARPreset = wings_dialog:get_value(ar_preset, Store),
                        {W,H} = get_aspect_dim(ARPreset, W),
                        wings_dialog:set_value(height, H, Store),
                        wings_dialog:update(pnl_h, Store);
                    custom -> ok;
                    viewport ->
                        wings_dialog:set_value(width, ViewW, Store),
                        wings_dialog:set_value(height, ViewH, Store);
                    _ ->
                        {W,H} = get_default_dim(Value),
                        wings_dialog:set_value(width, W, Store),
                        wings_dialog:set_value(height, H, Store)
                end;
            ar_preset ->
                W = wings_dialog:get_value(width, Store),
                {W,H} = get_aspect_dim(Value, W),
                wings_dialog:set_value(height, H, Store),
                wings_dialog:update(pnl_h, Store);
            width ->
                DP = wings_dialog:get_value(dim_preset, Store),
                case DP of
                    aspect ->
                        ARPreset = wings_dialog:get_value(ar_preset, Store),
                        {Value,H} = get_aspect_dim(ARPreset, Value),
                        wings_dialog:set_value(height, H, Store),
                        wings_dialog:update(pnl_h, Store);
                    _ -> ok
                end;
            aperture ->
                wings_dialog:enable(pnl_blur, Value =/= 0.0, Store);
            _ -> ok
        end
    end,

    GeneralOpt =
        {?__(65, "General option"),
            {vframe, [
                {vframe, [
                    {label_column, [
                        {?__(3, "Subdivisions"),{slider, {text, get_pref(subdivisions, 0),
                                    [range({0, 10}), exkey(subdivisions)]}}}
                    ]},
                    {hframe, [
                        {?__(1, "Export UVs"), get_pref(include_uvs, true), [exkey(include_uvs)]},
                        panel,
                        {?__(2, "Export Normals"), get_pref(export_normals, true), [exkey(export_normals)]},
                        panel,
                        {hframe, [
                            {label, "Threads"++" "},
                            {text, get_pref(threads_number,4), [range({1,512}),exkey(threads_number)]},
                            {label, " "},
                            {?__(67, "Auto"), get_pref(threads_auto,true),
                             [exkey(threads_auto),{hook,Hook_Enable}]}
                        ]}
                    ]}
                ], [{title, ?__(66, "Pre-rendering")},{margin,false}]},
                {vframe, [
                    {hframe, [
                        {label_column, [
                            {?__(4, "Gamma"),{text, get_pref(assumed_gamma, 1.0),
                                        [exkey(assumed_gamma), range({0.1, 10.0})]}}
                        ]},
                        panel,
                        {label_column, [
                            {?__(5, "Max Trace"),{text, get_pref(max_trace_level, 5),
                                        [exkey(max_trace_level), range({1, 256})]}}
                        ]},
                        panel,
                        {label_column, [
                            {?__(73, "Transparent Background"),{"", get_pref(transp_bkg, ?DEF_TRANSPARENT_BACKGROUND),
                                                  [exkey(transp_bkg)]}}
                        ]}
                    ], [{margin,false}]},
                    {label_column, [
                        {?__(6, "Background"),{slider, {color, get_pref(background, {0.0, 0.0, 0.0}),
                                        [exkey(background)]}}},
                        {?__(7, "Emissive Filter"),{slider, {color, get_pref(ambient, {0.0, 0.0, 0.0}),
                                        [exkey(ambient)]}}},
                        {?__(8, "Emissive Power"),{slider, {text, get_pref(ambient_power, 1.0),
                                        [exkey(ambient_power), range({0.0, 50.0})]}}},
                        {?__(75, "Quality Settings"), {menu, [
                                {?__(76, "Default"), none},
                                {?__(77, "Just show quick colors (Use full ambient lighting only)"), quality1},
                                {?__(78, "Show specified diffuse and ambient light"), quality3},
                                {?__(79, "Render shadows, but no extended lights"), quality4},
                                {?__(80, "Render shadows, including extended lights"), quality5},
                                {?__(81, "Compute texture patterns, compute photons"), quality7},
                                {?__(82, "Compute reflected, refracted, and transmitted rays"), quality8},
                                {?__(83, "Compute media and radiosity"), quality11}
                            ], get_pref(render_quality,none), [exkey(render_quality)]}}
                    ]}
                ], [{title, ?__(68, "Render")},{margin,false}]},
                {vframe, [
                    {hframe, [
                        {?__(84, "Enabled"), get_pref(antialias, false), [exkey(antialias), {hook, Hook_Enable}]},
                        panel,
                        {?__(10, "Jitter"), get_pref(aa_jitter, false), [exkey(aa_jitter)]}
                    ]},
                    {hframe, [
                        {label_column, [
                            {?__(70, "Method"),
                            {menu, [
                                {?__(71, "Adaptive, non-recursive, super-sampling"), aa_type1},
                                {?__(72, "Adaptive and recursive super-sampling"), aa_type2}
                            ], get_pref(aa_method, aa_type1), [exkey(aa_method)]}}]},
                        {label_column, [
                            {?__(11, "Threshold"),
                             {text, get_pref(aa_threshold, 0.3), [range({0.0, 3.0}), exkey(aa_threshold)]}}]},
                        {label_column, [
                            {?__(12, "Depth"),
                             {text, get_pref(aa_depth, 3), [range({1, 9}), exkey(aa_depth)]}}]}
                    ], [exkey(pnl_aa_params)]}
                ], [{title, ?__(9, "Anti-Aliasing")}, {enabled, Render},{margin,false}]}
            ]}
        },

    Lighting =
        {?__(74, "Lighting"),
            {vframe, [
                {hframe, [
                    {hframe, [
                        {?__(84, "Enabled"), get_pref(photons, false), [exkey(photons), {hook,Hook_Enable}]}
                    ]},
                    {hframe, [
                        {label, " " ++ ?__(26, "Count")},
                        {text, get_pref(photon_count, 10000), [exkey(photon_count)]}
                    ], [exkey(pnl_photons)]},
                    panel,
                    {hframe, [
                        {?__(27, "Media Photons"), get_pref(media_photons, false), [exkey(media_photons), {hook, Hook_Enable}]}
                    ]},
                    {hframe, [
                        {label, ?__(26, "Count")},
                        {text, get_pref(media_photons_count, 100), [exkey(media_photons_count)]}
                    ], [exkey(pnl_media_photons)]}
                ], [{title,?__(25, "Photons")}]},
                {vframe, [
                    {hframe, [
                        {?__(84, "Enabled"), get_pref(sslt, false), [exkey(sslt), {hook,Hook_Enable}]}
                    ]},
                    {vframe, [
                        {hframe, [
                            {?__(89, "Use radiosity based diffuse illumination"),
                             get_pref(sslt_radiosity, false), [exkey(sslt_radiosity)]}
                        ]},
                        {hframe, [
                            {label, ?__(87, "Diffuse scattering")},
                            {text, get_pref(sslt_diff_scatt, 50), [exkey(sslt_diff_scatt)]},
                            panel,
                            {label, ?__(88, "Single-scattering Approximation")},
                            {text, get_pref(sslt_single_scatt, 50), [exkey(sslt_single_scatt)]}
                        ],[{title, ?__(90, "Sampling")}]}
                    ],[exkey(pnl_sslt),{margin,false}]}
                ], [{title, ?__(86, "Subsurface Light Transport")}]},
                {hframe, [
                    {label, ?__(85, "Preset")},
                    {menu, [
                        {?__(53, "None"), none},
                        {?__(54, "Default"), "Radiosity_Default"},
                        {?__(55, "Debug"), "Radiosity_Debug"},
                        {?__(56, "Fast"), "Radiosity_Fast"},
                        {?__(57, "Normal"), "Radiosity_Normal"},
                        {?__(58, "2Bounce"), "Radiosity_2Bounce"},
                        {?__(59, "Final"), "Radiosity_Final"},
                        {?__(60, "OutdoorLQ"), "Radiosity_OutdoorLQ"},
                        {?__(61, "OutdoorHQ"), "Radiosity_OutdoorHQ"},
                        {?__(62, "OutdoorLight"), "Radiosity_OutdoorLight"},
                        {?__(63, "IndoorLQ"), "Radiosity_IndoorLQ"},
                        {?__(64, "IndoorHQ"), "Radiosity_IndoorHQ"}
                    ], get_pref(radiosity,none),[exkey(radiosity)]}
                ], [{title,?__(52, "Radiosity")}]}
            ], [{margin,false}]}
        },

    Atmosphere =
        {?__(51, "Atmosphere"),
            {vframe, [
                {vframe, [
                    {hframe, [
                        {vframe, [
                            {?__(84, "Enabled"), get_pref(fog, false), [exkey(fog), {hook, Hook_Enable}]}
                        ]},
                        {hframe, [
                            panel,
                            {label, ?__(29, "Type")},
                            {menu, [
                                {?__(30, "Constant"), 1},
                                {?__(31, "Ground"), 2}
                            ], get_pref(fog_type, 1),[exkey(fog_type),{hook, Hook_Enable}]}
                        ],[exkey(pnl_fog_type)]}
                    ]},
                    {hframe, [
                        {label_column, [
                            {?__(32, "Fog Height"),{text, get_pref(fog_offset, 0.0), [exkey(fog_offset)]}},
                            {?__(33, "Fog Fade"),{text, get_pref(fog_alt, 0.0), [exkey(fog_alt)]}}
                        ],[exkey(pnl_fog),{margin,false}]},
                        {label_column, [
                            {?__(34, "Distance"),{text, get_pref(fog_distance, 0.0), [exkey(fog_distance)]}},
                            {?__(35, "Color"),{color, get_pref(fog_color, {0.0, 0.0, 0.0, 0.0}), [exkey(fog_color)]}}
                        ]}
                    ], [exkey(pnl_fog_params)]}
                ], [{title,?__(28, "Fog")}, {margin,false}]},
                {vframe, [
                    {vframe, [
                        {hframe, [
                            {?__(84, "Enabled"), get_pref(media, false), [exkey(media), {hook,Hook_Enable}]},
                            panel,
                            {label, ?__(37, "Method")},
                            {menu, [
                                {?__(38, "Variance"), 1},
                                {?__(39, "Even"), 2},
                                {?__(40, "Adaptive"), 3}
                            ], get_pref(media_method, 1),[exkey(media_method)]}
                        ]},
                        {vframe, [
                            {hframe, [
                                {hframe, [
                                    {label, ?__(41, "Intervals")},
                                    {text, get_pref(media_intervals, 1), [exkey(media_intervals)]}
                                ]},
                                {hframe, [
                                    {label, ?__(42, "Samples")},
                                    {text, get_pref(media_samples, 1), [exkey(media_samples)]}
                                ]}
                            ]},
                            {label_column, [
                                {?__(45, "Emission"),{slider, {color, get_pref(emission, {0.0, 0.0, 0.0}), [exkey(emission)]}}},
                                {?__(44, "Absorption"),{slider, {color, get_pref(absorbtion, {0.0, 0.0, 0.0}), [exkey(absorbtion)]}}},
                                {?__(43, "Scattering"),{slider, {color, get_pref(scattering, {0.0, 0.0, 0.0}), [exkey(scattering)]}}}
                            ],[{margin,false}]},
                            {hframe, [
                                {label, ?__(29, "Type")},
                                panel,
                                {menu, [
                                    {?__(46, "Isotropic"), 1},
                                    {?__(47, "Mie Haze"), 2},
                                    {?__(48, "Mie Murky"), 3},
                                    {?__(49, "Rayleigh"), 4},
                                    {?__(50, "Henyey-Greenstein"), 5}
                                ], get_pref(scattering_type, 1),[exkey(scattering_type)]}
                            ]}
                        ], [exkey(pnl_media),{margin,false}]}
                    ]}
                ], [{title,?__(36, "Media")}, {margin,false}]}
            ]}
        },

    Camera =
        {?__(15, "Camera"),
            {vframe, [
		{hframe, [
		    {label, ?__(15, "Camera")},
		    {menu, [
			{?__(16, "Perspective"), perspective},
			{?__(17, "Orthographic"), orthographic},
			{?__(18, "Fisheye"), fisheye},
			{?__(19, "Ultra Wide"), ultra_wide_angle},
			{?__(20, "Omnimax"), omnimax},
			{?__(21, "Panoramic"), panoramic},
			{?__(22, "Spherical"), spherical}
		    ], get_pref(camera_type, perspective),[exkey(camera_type), {hook, Hook_Enable}]},
                    panel,
                    {label_column, [
                        {?__(112, "Angle"),
                        {slider, {text, get_pref(cam_angle, 180.0), [exkey(cam_angle), range({0.0, 360.0})]}}}
                    ],[exkey(pnl_angle)]}
		]},

                {vframe, [
		    {hframe, [
			{label_column, [
			    {?__(92, "Preset"),
			    {menu, [
				{?__(93,  "Custom"), custom},
                                {?__(94,  "Viewport")++VpDim, viewport},
				{?__(95,  "Set by Aspect Ratio"), aspect},
				{?__(96,  "Squared (320x320) [1:1]"), res_square},
				{?__(97,  "VGA (640x480) [4:3]"), res_vga},
				{?__(98,  "SVGA (800x600) [4:3]"), res_svga},
				{?__(99,  "DV (720x576) [16:9]"), res_dv},
				{?__(100,  "HD (1280x720) [16:9]"), res_hd},
				{?__(101, "FullHD (1920x1080) [16:9]"), res_fhd}
			    ], DimPreset, [exkey(dim_preset), {hook, Hook_Enable}]}}
			]},
			{label_column, [
			    {?__(102, "Aspect Ratio"),
			     {menu, [
				 {?__(103, "Squared [1:1]"), ar11},
				 {?__(104, "Classic Film [3:2]"), ar32},
				 {?__(105, "Standard Monitor [4:3]"), ar43},
				 {?__(106, "Photo [5:4]"), ar54},
				 {?__(107, "Panoramic [12:6]"), ar126},
				 {?__(108, "HD Video [16:9]"), ar169},
				 {?__(109, "Cinemascope [21:9]"), ar219}
			     ], get_pref(ar_preset, ar43),[exkey(ar_preset), {hook, Hook_Enable}]}}
			], [exkey(pnl_ar)]}
		    ], [exkey(pnl_preset)]},
		    {hframe, [
			{label_column, [
			    {?__(13, "Width"),
			    {text, Width, [exkey(width), {hook, Hook_Enable}]}}
			],[exkey(pnl_w)]},
			panel,
			{label_column, [
			    {?__(14, "Height"),
			    {text, Height, [exkey(height)]}}
			],[exkey(pnl_h)]}
		    ]}
                ],[{title,?__(91,"Dimension")},{margin,false}]},

                {hframe, [
                    {label_column, [
                        {?__(23, "Aperture"),
                        {text, get_pref(aperture, 0.0), [range({0.0,infinity}), exkey(aperture), {hook, Hook_Enable}]}}
                    ]},
                    {hframe, [
                        {label_column, [
                            {?__(24, "Blur Samples"),
                            {text, get_pref(blur_samples, 1), [range({1,50}), exkey(blur_samples)]}}
                        ]},
                        {label_column, [
                            {?__(111, "Variance"),
                            {slider, {text, get_pref(variance, 1.0/128), [range({1.0/1000000, 0.99999}),
                                                                          exkey(variance), {hook, Hook_Enable}]}}}
                        ]}
                    ],[exkey(pnl_blur),{margin,false}]}
                ],[{title,?__(110,"Focal Blur")},{margin,false}]}
            ]}
        },

    [
        {oframe, [
            GeneralOpt,
            Lighting,
            Atmosphere,
            Camera
        ], 1, [{style, buttons}]}
    ].

%%% Construct material dialog, return list of controls
material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    PovRay = proplists:get_value(?TAG, Mat, []),

    BrowseProps = [{dialog_type, open_dialog},
		   {extensions, [{".jpg", "JPEG"}, {".png", "PNG"}, {".bmp", "Bitmap"}, {".gif", "GIF"}, {".iff", "IFF"}, {".tiff", "TIFF"}]}],

    case proplists:is_defined(proplists:get_value(image_type, PovRay, user), Maps) of
        true -> PigmentImage = proplists:get_value(image_type, PovRay, user);
        _ -> PigmentImage = user
    end,

    case proplists:is_defined(proplists:get_value(n_image_type, PovRay, user), Maps) of
        true -> NormalImage = proplists:get_value(n_image_type, PovRay, user);
        _ -> NormalImage = user
    end,

    Hook_Enable = fun(Key, Value, Store) ->
			  case Key of
			      %% Texture->Pigment fields
			      ?KEY(reflection_enabled) ->
				  wings_dialog:enable(?KEY(reflection_variable), Value =:= true, Store),
				  wings_dialog:enable(?KEY(reflection_fresnel), Value =:= true, Store),
				  wings_dialog:enable(?KEY(pnl_reflection), Value =:= true, Store),
				  Value0 = wings_dialog:get_value(?KEY(reflection_variable),Store),
				  wings_dialog:enable(?KEY(refl_max_row), Value0 =:= true, Store);
			      ?KEY(reflection_variable) ->
				  wings_dialog:enable(?KEY(refl_max_row), Value =:= true, Store);
			      ?KEY(interior_extended) ->
				  wings_dialog:enable(?KEY(interior_caustic), Value =:= true, Store),
				  wings_dialog:enable(?KEY(interior_dispersion), Value =:= true, Store),
				  wings_dialog:enable(?KEY(interior_dispersion_samples), Value =:= true, Store),
				  wings_dialog:enable(?KEY(interior_fade_distance), Value =:= true, Store),
				  wings_dialog:enable(?KEY(interior_fade_power), Value =:= true, Store),
				  wings_dialog:enable(?KEY(interior_fade_color), Value =:= true, Store);
			      ?KEY(photons_enabled) ->
				  wings_dialog:enable(?KEY(pnl_photons), Value =:= true, Store);
			      ?KEY(pigment_colormap) ->
				  wings_dialog:enable(?KEY(new_colormap), Value =:= true, Store),
				  wings_dialog:enable(?KEY(pnl_colormap), Value =:= true, Store);
			      ?KEY(image_type) ->
				  wings_dialog:enable(?KEY(pnl_pigment_file), Value =:= user, Store);
			      ?KEY(pigment_modifiers) ->
				  wings_dialog:enable(?KEY(pnl_modif_props), Value =:= true, Store);
			      %% Texture->Normal fields
			      ?KEY(normal_normalmap) ->
				  wings_dialog:enable(?KEY(new_normalmap), Value =:= true, Store);
			      ?KEY(normal_components) ->
				  wings_dialog:enable(?KEY(pnl_n_checker), Value =:= true, Store),
				  wings_dialog:enable(?KEY(pnl_n_hexagon), Value =:= true, Store),
				  wings_dialog:enable(?KEY(pnl_n_brick_props), Value =:= true, Store);
			      ?KEY(n_image_type) ->
				  wings_dialog:enable(?KEY(pnl_normal_file), Value =:= user, Store);
			      ?KEY(normal_modifiers) ->
				  wings_dialog:enable(?KEY(pnl_n_modif_props), Value =:= true, Store);
			      _ -> ok
			  end
		  end,
    Hook_Show = fun(Key, Value, Store) ->
			case Key of
			    ?KEY(ghost_material) ->
				wings_dialog:show(?KEY(pnl_material), Value =:= false, Store);
			    ?KEY(pigment_pattern) ->
				InGroup0 = is_member(Value, [color, image]),
				InGroup1 = is_member(Value, [color, image, brick, checker, hexagon]),
				wings_dialog:show(?KEY(pnl_modifiers), not InGroup0, Store),
				Hook_Enable(?KEY(pigment_modifiers), wings_dialog:get_value(?KEY(pigment_modifiers),Store), Store),
				wings_dialog:show(?KEY(pnl_agate), Value =:= agate, Store),
				wings_dialog:show(?KEY(pnl_brick), Value =:= brick, Store),
				wings_dialog:show(?KEY(pnl_checker), Value =:= checker, Store),
				wings_dialog:show(?KEY(pnl_crackle), Value =:= crackle, Store),
				wings_dialog:show(?KEY(pnl_gradient), Value =:= gradient, Store),
				wings_dialog:show(?KEY(pnl_hexagon), Value =:= hexagon, Store),
				wings_dialog:show(?KEY(pnl_quilted), Value =:= quilted, Store),
				wings_dialog:show(?KEY(pnl_spiral), is_member(Value, [spiral1,spiral2]), Store),
				wings_dialog:show(?KEY(pnl_image), Value =:= image, Store),
				wings_dialog:show(?KEY(pnl_colormap), not InGroup1, Store),
				wings_dialog:update(?KEY(pnl_tex_pigment), Store);
			    ?KEY(normal_pattern) ->
				InGroup0 = is_member(Value, [none, image, hexagon, checker, brick, facets]),
				InGroup1 = is_member(Value, [hexagon, checker, brick]),
				Hook_Enable(?KEY(normal_normalmap), wings_dialog:get_value(?KEY(normal_normalmap),Store), Store),
				wings_dialog:show(?KEY(normal_components), InGroup1, Store),
				wings_dialog:show(?KEY(pnl_normal_comp), InGroup1, Store),
				wings_dialog:show(?KEY(pnl_magnitude), Value =/= none, Store),
				wings_dialog:show(?KEY(pnl_n_modifiers), not is_member(Value, [none, image]), Store),
				Hook_Enable(?KEY(normal_modifiers), wings_dialog:get_value(?KEY(normal_modifiers),Store), Store),
				wings_dialog:show(?KEY(pnl_n_agate), Value =:= agate, Store),
				wings_dialog:show(?KEY(pnl_n_brick), Value =:= brick, Store),
				wings_dialog:show(?KEY(pnl_n_checker), Value =:= checker, Store),
				wings_dialog:show(?KEY(pnl_n_crackle), Value =:= crackle, Store),
				wings_dialog:show(?KEY(pnl_n_gradient), Value =:= gradient, Store),
				wings_dialog:show(?KEY(pnl_n_hexagon), Value =:= hexagon, Store),
				wings_dialog:show(?KEY(pnl_n_quilted), Value =:= quilted, Store),
				wings_dialog:show(?KEY(pnl_n_spiral), is_member(Value, [spiral1,spiral2]), Store),
				wings_dialog:show(?KEY(pnl_n_image), Value =:= image, Store),
				wings_dialog:show(?KEY(pnl_n_colormap), not InGroup0, Store),
				wings_dialog:update(?KEY(pnl_tex_normal), Store)
			end
		end,

    QsFinish =
        {?__(8, "Finish"),
	 {vframe, [
		   {label_column,
		    [{?__(2, "Diffuse"),
		      {slider, {text, proplists:get_value(finish_diffuse, PovRay, 0.7),
				[range({0.0, 1.0}), key(finish_diffuse)]}}},
                     {?__(3, "Brilliance"),
		      {slider, {text, proplists:get_value(finish_brilliance, PovRay, 1.0),
				[range({0.0, 10.0}), key(finish_brilliance)]}}},
		     {?__(4, "Metallic"),
		      {slider, {text, proplists:get_value(finish_metallic, PovRay, 0.0),
				[range({0.0, 10.0}), key(finish_metallic)]}}},
		     {?__(5, "Transparency"),
		      {hradio, [{?__(6, "Transmit"), transmit},
				{?__(7, "Filter"), filter}],
		       proplists:get_value(finish_transparency, PovRay, filter),
		       [key(finish_transparency)]}}
		    ]},
                   {label_column, [
                       {?__(104, "Amount"),
                        {slider, {text, proplists:get_value(finish_phong, PovRay, 0.0),
                                  [range({0.0, 10.0}), key(finish_phong)]}}},
                       {?__(74, "Size"),
                        {slider, {text, proplists:get_value(finish_phong_size, PovRay, 40.0),
                                  [range({0.0, 250.0}), key(finish_phong_size)]}}}
                       ],[{title, ?__(101, "Phong")}]},
                   {label_column, [
                       {?__(104, "Amount"),
                        {slider, {text, proplists:get_value(finish_irid_amount, PovRay, 0.0),
                                  [range({0.0, 1.0}), key(finish_irid_amount)]}}},
                       {?__(105, "Thickness"),
                        {slider, {text, proplists:get_value(finish_irid_thickness, PovRay, 0.2),
                                  [range({0.0, 100.0}), key(finish_irid_thickness)]}}},
                       {?__(66, "Turbulence"),
                        {slider, {text, proplists:get_value(finish_irid_turbulence, PovRay, 0.15),
                                  [range({0.0, 100.0}), key(finish_irid_turbulence)]}}}
                   ], [{title, ?__(103, "Iridescense")}]},
                   {label_column, [
                       {?__(107, "Translucency"),
                        {slider, {color, proplists:get_value(finish_sslt_color, PovRay, {1.0, 1.0, 1.0}),
                                  [key(finish_sslt_color)]}}},
                       {?__(108, "Multiplier"),
                        {slider, {text, proplists:get_value(finish_sslt_multiplier, PovRay, 0.0),
                                  [range({0.0, 100.0}), key(finish_sslt_multiplier)]}}}
                   ], [{title,?__(106, "Subsurface")}]}

         ], [{key,pnl_finished}]}
        },
    QsReflection =
        {?__(16, "Reflection"),
	 {vframe, [
             {hframe, [
                 {?__(9, "Enabled"), proplists:get_value(reflection_enabled, PovRay, false),
                   [key(reflection_enabled), {hook, Hook_Enable}]}
             ],[{border,3}]},
             {vframe,[
                 {hframe, [
                     {?__(10, "Variable"), proplists:get_value(reflection_variable, PovRay, false),
                      [key(reflection_variable), {hook, Hook_Enable}]},
                     panel,
                     {?__(11, "Fresnel"), proplists:get_value(reflection_fresnel, PovRay, false),
                      [key(reflection_fresnel)]}
                  ],[{border,3}]},
                 {label_column,
                    [{?__(12, "Minimum"),
                      {slider, {color, proplists:get_value(reflection_minimum, PovRay, {0.0, 0.0, 0.0}),
                                [key(reflection_minimum)]}}},
                     {?__(13, "Maximum"),
                      {slider, {color, proplists:get_value(reflection_maximum, PovRay, {0.0, 0.0, 0.0}),
                                [key(reflection_maximum)]}}, [key(refl_max_row)]},
                     {?__(14, "Falloff"),
                      {slider, {text, proplists:get_value(reflection_falloff, PovRay, 1.0),
                                [range({0.0, 10.0}), key(reflection_falloff)]}}},
                     {?__(15, "Exponent"),
                      {slider, {text, proplists:get_value(reflection_exponent, PovRay, 1.0),
                                [range({0.0, 10.0}), key(reflection_exponent)]}}},
                     {?__(4, "Metallic"),
                      {slider, {text, proplists:get_value(reflection_metallic, PovRay, 0.0),
                                [range({0.0, 10.0}), key(reflection_metallic)]}}}
                 ],[key(pnl_reflection)]}
             ], [{title,""}]}
         ]}
        },

    QsInterior =
        {?__(25, "Interior"),
            {vframe, [
                {hframe, [
                    {?__(24, "Extended Interior"), proplists:get_value(interior_extended, PovRay, false),
                        [key(interior_extended),{hook,Hook_Enable}]}],[{border,3}]},
                {label_column, [
                    {?__(17, "IOR"),
                        {slider, {text, proplists:get_value(interior_ior, PovRay, 1.0),
                                [range({0.0, 3.0}), key(interior_ior)]}}},
                    {?__(18, "Fake Caustics"),
                        {slider, {text, proplists:get_value(interior_caustic, PovRay, 0.0),
                                [range({0.0, 1.0}), key(interior_caustic)]}}},
                    {?__(19, "Dispersion"),
                        {slider, {text, proplists:get_value(interior_dispersion, PovRay, 1.0),
                                [range({0.0, 2.0}), key(interior_dispersion)]}}},
                    {?__(20, "Disp. Samples"),
                        {slider, {text, proplists:get_value(interior_dispersion_samples, PovRay, 7),
                                [range({0, 50}), key(interior_dispersion_samples)]}}},
                    {?__(21, "Fade Dist."),
                        {slider, {text, proplists:get_value(interior_fade_distance, PovRay, 0.0),
                                [range({0.0, 10.0}), key(interior_fade_distance)]}}},
                    {?__(22, "Fade Power"),
                        {slider, {text, proplists:get_value(interior_fade_power, PovRay, 0.0),
                                [range({0.0, 10.0}), key(interior_fade_power)]}}},
                    {?__(23, "Fade Color"),
                        {slider, {color, proplists:get_value(interior_fade_color, PovRay, {0.0, 0.0, 0.0}),
                                [key(interior_fade_color)]}}}
                ], [{title,""}]}
            ]}
        },

    QsPhotons =
        {?__(30, "Photons"),
            {vframe, [
	        {hframe, [
                    {?__(9, "Enabled"), proplists:get_value(photons_enabled, PovRay, false),
                        [key(photons_enabled), {hook,Hook_Enable}]}
                ],[{border,3}]},
                {hframe, [
                    {?__(26, "Target"), proplists:get_value(photons_target, PovRay, true), [key(photons_target)]},
                    panel,
                    {?__(27, "Collect"), proplists:get_value(photons_collect, PovRay, true), [key(photons_collect)]},
                    panel,
                    {?__(28, "Reflect"), proplists:get_value(photons_reflect, PovRay, true), [key(photons_reflect)]},
                    panel,
                    {?__(29, "Refract"), proplists:get_value(photons_refract, PovRay, true), [key(photons_refract)]}
                ],[key(pnl_photons),{border,3}]}
            ]}
        },

    TxtPigment =
        {?__(90, "Pigment"),
	 {vframe, [
		   {vframe, [
			     {hframe, [
				       {label, ?__(31, "Pattern")},
				       {menu, [
					       {?__(32, "Color"), color},
					       {?__(33, "Image"), image},
					       {?__(34, "Agate"), agate},
					       {?__(35, "Boxed"), boxed},
					       {?__(36, "Bozo"), bozo},
					       {?__(37, "Brick"), brick},
					       {?__(38, "Bumps"), bumps},
					       {?__(39, "Cells"), cells},
					       {?__(40, "Checker"), checker},
					       {?__(41, "Crackle"), crackle},
					       {?__(42, "Cylindrical"), cylindrical},
					       {?__(43, "Dents"), dents},
					       {?__(44, "Gradient"), gradient},
					       {?__(45, "Granite"), granite},
					       {?__(46, "Hexagon"), hexagon},
					       {?__(47, "Leopard"), leopard},
					       {?__(48, "Marble"), marble},
					       {?__(49, "Onion"), onion},
					       {?__(50, "Planar"), planar},
					       {?__(51, "Quilted"), quilted},
					       {?__(52, "Radial"), radial},
					       {?__(53, "Ripples"), ripples},
					       {?__(54, "Slope"), slope},
					       {?__(55, "Spherical"), spherical},
					       {?__(56, "Spiral1"), spiral1},
					       {?__(57, "Spiral2"), spiral2},
					       {?__(58, "Waves"), waves},
					       {?__(59, "Wood"), wood},
					       {?__(60, "Wrinkles"), wrinkles}
					      ], proplists:get_value(pigment_pattern, PovRay, color), [key(pigment_pattern),{hook,Hook_Show}]},
				       panel,
				       {hframe, [
						 {?__(61, "Color Map"), proplists:get_value(pigment_colormap, PovRay, false), [key(pigment_colormap), {hook, Hook_Enable}]},
						 {button, ?__(62, "New Entry"), done, [key(new_colormap)]}
						],[key(pnl_color_map),{show,false},{margin,false}]}
				       %%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
				       %%                        ],[key(pnl_color_map),{margin,false}]}
				      ]},

			     %% pattern specific values
			     %% agate
			     {hframe, [
				       {hframe, [
						 {label, ?__(71, "Agate Turbulence")},
						 {text, proplists:get_value(agate_turbulence, PovRay, 1.0), [key(agate_turbulence)]}
						]}
				      ], [key(pnl_agate),{show, false},{margin,false}]},
			     %% brick
			     {hframe, [
				       {label_column, [
						 {?__(37, "Brick"),{color, proplists:get_value(brick_color, PovRay, {1.0, 0.0, 0.0, 1.0}), [key(brick_color)]}},
						 {?__(72, "Mortar"),{color, proplists:get_value(mortar_color, PovRay, {1.0, 1.0, 1.0, 1.0}), [key(mortar_color)]}}
						]},
				       {label_column, [
						 {?__(73, "Size X"),{text, proplists:get_value(brick_size_x, PovRay, 7.5), [key(brick_size_x)]}},
						 {?__(74, "Size"),{text, proplists:get_value(mortar_size, PovRay, 0.5), [key(mortar_size)]}}
						]},
				       {label_column, [
						 {?__(67, "Y"),{text, proplists:get_value(brick_size_y, PovRay, 2.5), [key(brick_size_y)]}}
						]},
				       {label_column, [
						 {?__(69, "Z"),{text, proplists:get_value(brick_size_z, PovRay, 4.0), [key(brick_size_z)]}}
						]}
				      ], [key(pnl_brick),{show, false},{margin,false}]},
			     %% checker
			     {hframe, [
                                       {hframe, [
                                                    {label, ?__(75, "Checker 1")},
                                                    {color, proplists:get_value(checker_color1, PovRay, {1.0, 1.0, 1.0, 1.0}), [key(checker_color1)]}
                                                ]},
				       panel,
                                       {hframe, [
                                                    {label, ?__(76, "Checker 2")},
                                                    {color, proplists:get_value(checker_color2, PovRay, {0.0, 1.0, 0.0, 1.0}), [key(checker_color2)]}
                                                ]}
				      ], [key(pnl_checker),{show, false},{margin,false}]},
			     %% crackle
			     {vframe, [
                                       {label_column, [
                                                   {?__(77, "Crackle Metric"),{slider, {text, proplists:get_value(crackle_metric, PovRay, 2), [range({0, 10}), key(crackle_metric)]}}},
                                                   {?__(78, "Crackle Offset"),{slider, {text, proplists:get_value(crackle_offset, PovRay, 0.0), [range({0.0, 10.0}), key(crackle_offset)]}}}
                                                ]},
                                       {hframe, [
                                                   {?__(79, "Crackle Solid"), proplists:get_value(crackle_solid, PovRay, false), [key(crackle_solid)]}
                                                ]}
				      ], [key(pnl_crackle),{show, true},{margin,false}]},
			     %% gradient
			     {hframe, [
				       {label, ?__(80, "Gradient Axis")},
				       {menu, [
					       {?__(81, "X"), x},
					       {?__(67, "Y"), y},
					       {?__(69, "Z"), z}
					      ], proplists:get_value(gradient_axis, PovRay, x), [key(gradient_axis)]}
				      ], [key(pnl_gradient),{show, false}]},
			     %% hexagon
			     {hframe, [
				       {label, ?__(82, "Hex. 1")},
				       {color, proplists:get_value(hexagon_color1, PovRay, {0.0, 0.0, 1.0, 1.0}), [key(hexagon_color1)]},
				       panel,
				       {label, ?__(83, "Hex. 2")},
				       {color, proplists:get_value(hexagon_color2, PovRay, {0.0, 1.0, 0.0, 1.0}), [key(hexagon_color2)]},
				       panel,
				       {label, ?__(84, "Hex. 3")},
				       {color, proplists:get_value(hexagon_color3, PovRay, {1.0, 0.0, 0.0, 1.0}), [key(hexagon_color3)]}
				      ], [key(pnl_hexagon),{show, false}]},
			     %% quilted
			     {hframe, [
				       {hframe, [
						 {label, ?__(85, "Quilted 1")},
						 {text, proplists:get_value(quilted_value1, PovRay, 1.0), [key(quilted_value1)]}
						]},
				       panel,
				       {hframe, [
						 {label, ?__(86, "Quilted 2")},
						 {text, proplists:get_value(quilted_value2, PovRay, 1.0), [key(quilted_value2)]}
						]}
				      ], [key(pnl_quilted),{show, false},{margin,false}]},
			     %% Spiral
			     {hframe, [
				       {hframe, [
						 {label, ?__(87, "Spiral Arms")},
						 {text, proplists:get_value(spiral_arms, PovRay, 5), [key(spiral_arms)]}
						]}
				      ], [key(pnl_spiral),{show, false},{margin,false}]},
			     %% Image
			     {vframe, [
				       {hframe, [
						 {hradio, [
							   {?__(88, "user"), user} | enumerate_image_maps(Maps)
							  ], PigmentImage, [key(image_type),{hook,Hook_Enable}]}]},
				       {hframe, [
						 {label, ?__(89, "Filename")},
						 {button, {text, proplists:get_value(image_user_file, PovRay, []), [key(image_user_file), {width,35}, {props, BrowseProps}]}}
						], [key(pnl_pigment_file)]}
				      ], [key(pnl_image),{show, false},{margin,false}]},
			     %% colormap
			     {vframe,
			        enumerate_colormap_controls(proplists:get_value(colormap_list, PovRay, []), 0),
			        [key(pnl_colormap),{show, false}]
			     },
			     %% Modifiers
			     {vframe, [
				       {?__(63, "Modifiers"), proplists:get_value(pigment_modifiers, PovRay, false), [key(pigment_modifiers), {hook, Hook_Enable}]},
				       {hframe, [
						 {label_column, [
                                                           {?__(64, "Offset X"),{text, proplists:get_value(pigment_off_x, PovRay, 0.0), [key(pigment_off_x)]}},
                                                           {?__(65, "Rotate X"),{text, proplists:get_value(pigment_rot_x, PovRay, 0.0), [key(pigment_rot_x)]}},
                                                           {?__(66, "Turbulence"),{text, proplists:get_value(pigment_turbulence, PovRay, 0.0), [key(pigment_turbulence)]}}
                                                        ]},
						 {label_column, [
							   {?__(67, "Y"),{text, proplists:get_value(pigment_off_y, PovRay, 0.0), [key(pigment_off_y)]}},
							   {?__(67, "Y"),{text, proplists:get_value(pigment_rot_y, PovRay, 0.0), [key(pigment_rot_y)]}},
							   {?__(68, "Freq."),{text, proplists:get_value(pigment_frequency, PovRay, 1.0), [key(pigment_frequency)]}}
                                                        ]},
						 {label_column, [
							   {?__(69, "Z"),{text, proplists:get_value(pigment_off_z, PovRay, 0.0), [key(pigment_off_z)]}},
							   {?__(69, "Z"),{text, proplists:get_value(pigment_rot_z, PovRay, 0.0), [key(pigment_rot_z)]}},
							   {?__(70, "Scale"),{text, proplists:get_value(pigment_scale, PovRay, 1.0), [key(pigment_scale)]}}
                                                        ]}
						], [key(pnl_modif_props),{margin,false}]}
				      ], [key(pnl_modifiers)]}
			    ], [key(pnl_tex_pigment)]}
		  ]}
        },

    TxtNormal =
        {?__(96, "Normal"),
	 {vframe, [
		   {vframe, [
			     {hframe, [
				       {label, ?__(31, "Pattern")},
				       {menu, [
					       {?__(91, "None"), none},
					       {?__(33, "Image"), image},
					       {?__(34, "Agate"), agate},
					       {?__(35, "Boxed"), boxed},
					       {?__(36, "Bozo"), bozo},
					       {?__(37, "Brick"), brick},
					       {?__(38, "Bumps"), bumps},
					       {?__(39, "Cells"), cells},
					       {?__(40, "Checker"), checker},
					       {?__(41, "Crackle"), crackle},
					       {?__(42, "Cylindrical"), cylindrical},
					       {?__(43, "Dents"), dents},
					       {?__(44, "Gradient"), gradient},
					       {?__(45, "Granite"), granite},
					       {?__(46, "Hexagon"), hexagon},
					       {?__(47, "Leopard"), leopard},
					       {?__(48, "Marble"), marble},
					       {?__(49, "Onion"), onion},
					       {?__(50, "Planar"), planar},
					       {?__(51, "Quilted"), quilted},
					       {?__(52, "Radial"), radial},
					       {?__(53, "Ripples"), ripples},
					       {?__(54, "Slope"), slope},
					       {?__(55, "Spherical"), spherical},
					       {?__(56, "Spiral1"), spiral1},
					       {?__(57, "Spiral2"), spiral2},
					       {?__(58, "Waves"), waves},
					       {?__(59, "Wood"), wood},
					       {?__(60, "Wrinkles"), wrinkles}
					      ], proplists:get_value(normal_pattern, PovRay, none), [key(normal_pattern),{hook,Hook_Show}]},
				       panel,
				       {hframe, [
						 {?__(92, "Normal Map"), proplists:get_value(normal_normalmap, PovRay, false), [key(normal_normalmap), {hook, Hook_Enable}]},
						 {button, ?__(93, "New Entry"), done, [key(new_normalmap)]}
						], [key(pnl_normal_map),{show,false},{margin,false}]},
				       %%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
				       %%                        ], [key(pnl_normal_map),{margin,false}]},
				       {hframe, [
						 {?__(94, "Normal Components"), proplists:get_value(normal_components, PovRay, false), [key(normal_components), {hook, Hook_Enable}]}
						], [key(pnl_normal_comp),{margin,false}]}
				      ]},
			     {hframe, [
				       {hframe, [
						 {label, ?__(95, "Magnitude")},
						 {text, proplists:get_value(normal_magnitude, PovRay, 0.5), [key(normal_magnitude)]}
						], [key(pnl_magnitude),{margin,false}]}
				      ]},

			     %% pattern specific values
			     %% agate
			     {hframe, [
				       {hframe, [
						 {label, ?__(71, "Agate Turbulence")},
						 {text, proplists:get_value(n_agate_turbulence, PovRay, 1.0), [key(n_agate_turbulence)]}
						]}
				      ], [key(pnl_n_agate), {show, false},{margin,false}]},
			     %% brick
			     {hframe, [
				       {label_column, [
						 {?__(37, "Brick"),{menu,
                                                     normalmap_menu(),
                                                     proplists:get_value(brick_normal, PovRay, agate), [key(brick_normal)]}},
						 {?__(72, "Mortar"),{menu,
                                                     normalmap_menu(),
                                                     proplists:get_value(mortar_normal, PovRay, agate), [key(mortar_normal)]}}
						], [key(pnl_n_brick_props)]},
				       {label_column, [
						 {?__(73, "Size X"),{text, proplists:get_value(n_brick_size_x, PovRay, 7.5), [key(n_brick_size_x)]}},
						 {?__(74, "Size"),{text, proplists:get_value(n_mortar_size, PovRay, 0.5), [key(n_mortar_size)]}}
						]},
				       {label_column, [
						 {"Y",{text, proplists:get_value(n_brick_size_y, PovRay, 2.5), [key(n_brick_size_y)]}}
						]},
				       {label_column, [
						 {"Z",{text, proplists:get_value(n_brick_size_z, PovRay, 4.0), [key(n_brick_size_z)]}}
						]}
				      ], [key(pnl_n_brick), {show, false},{margin,false}]},
			     %% checker
			     {hframe, [
				       {hframe, [
						 {label, ?__(75, "Checker 1")},
						 {menu, normalmap_menu(), proplists:get_value(checker_normal1, PovRay, agate), [key(checker_normal1)]}
						]},
				       {hframe, [
						 {label, ?__(76, "Checker 2")},
						 {menu, normalmap_menu(), proplists:get_value(checker_normal2, PovRay, agate), [key(checker_normal2)]}
						]}
				      ], [key(pnl_n_checker), {show, false},{margin,false}]},
			     %% crackle
			     {vframe, [
                                       {label_column, [
                                                    {?__(77, "Crackle Metric"),{slider, {text, proplists:get_value(n_crackle_metric, PovRay, 2), [range({0, 10}), key(n_crackle_metric)]}}},
                                                    {?__(78, "Crackle Offset"),{slider, {text, proplists:get_value(n_crackle_offset, PovRay, 0.0), [range({0.0, 10.0}), key(n_crackle_offset)]}}}
                                                ]},
                                       {hframe, [
                                                    {?__(79, "Crackle Solid"), proplists:get_value(n_crackle_solid, PovRay, false), [key(n_crackle_solid)]}
                                                ]}
				      ], [key(pnl_n_crackle),{show, true},{margin,false}]},
			     %% gradient
			     {hframe, [
				       {label, ?__(80, "Gradient Axis")},
				       {menu, [
					       {?__(81, "X"), x},
					       {?__(67, "Y"), y},
					       {?__(69, "Z"), z}
					      ], proplists:get_value(n_gradient_axis, PovRay, x), [key(n_gradient_axis)]}
				      ], [key(pnl_n_gradient),{show, false}]},
			     %% hexagon
			     {hframe, [
				       {label, ?__(82, "Hex. 1")},
				       {menu, normalmap_menu(),
                                            proplists:get_value(hexagon_normal1, PovRay, agate), [key(hexagon_normal1)]},
				       panel,
				       {label, ?__(83, "Hex. 2")},
				       {menu, normalmap_menu(),
                                            proplists:get_value(hexagon_normal2, PovRay, agate), [key(hexagon_normal2)]},
				       panel,
				       {label, ?__(84, "Hex. 3")},
				       {menu, normalmap_menu(),
                                            proplists:get_value(hexagon_normal3, PovRay, agate), [key(hexagon_normal3)]}
				      ], [key(pnl_n_hexagon),{show, false}]},
			     %% quilted
			     {hframe, [
				       {hframe, [
						 {label, ?__(85, "Quilted 1")},
						 {text, proplists:get_value(n_quilted_value1, PovRay, 1.0), [key(n_quilted_value1)]}
						]},
                                       panel,
				       {hframe, [
						 {label, ?__(86, "Quilted 2")},
						 {text, proplists:get_value(n_quilted_value2, PovRay, 1.0), [key(n_quilted_value2)]}
						]}
				      ], [key(pnl_n_quilted),{show, false},{margin,false}]},
			     %% Spiral
			     {hframe, [
				       {hframe, [
						 {label, ?__(87, "Spiral Arms")},
						 {text, proplists:get_value(n_spiral_arms, PovRay, 5), [key(n_spiral_arms)]}
						]}
				      ], [key(pnl_n_spiral),{show, false},{margin,false}]},
			     %% Image
			     {vframe, [
				       {hframe, [
						 {hradio, [
							   {?__(88, "user"), user} | enumerate_image_maps(Maps)
							  ], NormalImage, [key(n_image_type),{hook,Hook_Enable}]}]},
				       {hframe, [
						 {label, ?__(89, "Filename")},
						 {button, {text, proplists:get_value(n_image_user_file, PovRay, []), [key(n_image_user_file), {width,35}, {props, BrowseProps}]}}
						], [key(pnl_normal_file)]}
				      ], [key(pnl_n_image),{show, false},{margin,false}]},
			     %% normalmap
			     {vframe,
			        enumerate_normalmap_controls(proplists:get_value(normalmap_list, PovRay, []), 0),
			        [key(pnl_n_colormap),{show, false}]
			     },

			     %% Modifiers
			     {vframe, [
				       {?__(63, "Modifiers"), proplists:get_value(normal_modifiers, PovRay, false), [key(normal_modifiers), {hook, Hook_Enable}]},
				       {hframe, [
						 {label_column, [
							   {?__(64, "Offset X"),{text, proplists:get_value(normal_off_x, PovRay, 0.0), [key(normal_off_x)]}},
							   {?__(65, "Rotate X"),{text, proplists:get_value(normal_rot_x, PovRay, 0.0), [key(normal_rot_x)]}},
							   {?__(66, "Turbulence"),{text, proplists:get_value(normal_turbulence, PovRay, 0.0), [key(normal_turbulence)]}}
                                                        ]},
						 {label_column, [
							   {?__(67, "Y"),{text, proplists:get_value(normal_off_y, PovRay, 0.0), [key(normal_off_y)]}},
							   {?__(67, "Y"),{text, proplists:get_value(normal_rot_y, PovRay, 0.0), [key(normal_rot_y)]}},
							   {?__(68, "Freq."),{text, proplists:get_value(normal_frequency, PovRay, 1.0), [key(normal_frequency)]}}
                                                        ]},
						 {label_column, [
							   {?__(69, "Z"),{text, proplists:get_value(normal_off_z, PovRay, 0.0), [key(normal_off_z)]}},
							   {?__(69, "Z"),{text, proplists:get_value(normal_rot_z, PovRay, 0.0), [key(normal_rot_z)]}},
							   {?__(70, "Scale"),{text, proplists:get_value(normal_scale, PovRay, 1.0), [key(normal_scale)]}}
                                                        ]}
						], [key(pnl_n_modif_props),{margin,false}]}
				      ], [key(pnl_n_modifiers),{margin,false}]}
			    ], [key(pnl_tex_normal)]}
		  ]} %,[{margin,false}]
        },

    %% Texture
    QsTexture =
        {?__(97, "Texture"),
	 {vframe, [
		   {oframe, [
			     TxtPigment,
			     TxtNormal], 1, [{style, buttons}]}
		  ], [{title, ?__(97, "Texture")},key(texture_minimized)]}
        },

    {vframe, [
          {?__(1, "Exclude Material Definition (requires external definition)"),
           proplists:get_value(ghost_material, PovRay, false), [key(ghost_material),{hook,Hook_Show}]},
          {vframe, [
                    {oframe, [
                              QsFinish,
                              QsReflection,
                              QsInterior,
                              QsPhotons,
                              QsTexture], 1, [{style, buttons}]}
          ], [{title, ?__(98, "POV-Ray Options")}, key(pnl_material), {show,true}]}
    ]}.
enumerate_image_maps([]) ->
    [];
enumerate_image_maps([{MapType, _I} | Maps]) ->
    [{atom_to_list(MapType), MapType} | enumerate_image_maps(Maps)].
enumerate_colormap_controls([], _M) ->
    [];
enumerate_colormap_controls([{ColorLevel, ColorValue} | ColorMaps], M) ->
    Controls = [
        {hframe, [
            {slider, {text, ColorLevel, [colorlevel(M), range({0.0, 1.0})]}},
            {color, ColorValue, [colorvalue(M)]}
%%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%            {button, ?__(1, "Delete"), done, [colordelete(M)]}
        ]}
    ],
    List = Controls ++ enumerate_colormap_controls(ColorMaps, M + 1),
    List.

enumerate_normalmap_controls([], _M) ->
    [];
enumerate_normalmap_controls([{NormalLevel, NormalValue} | NormalMaps], M) ->
    Controls = [
        {hframe, [
            {slider, {text, NormalLevel, [normallevel(M), range({0.0, 1.0})]}},
            {menu,
                normalmap_menu(),
                NormalValue, [normalvalue(M)]}
%%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%            {button, ?__(1, "Delete"), done, [normaldelete(M)]}
        ]}
    ],
    List = Controls ++ enumerate_normalmap_controls(NormalMaps, M + 1),
    List.

normalmap_menu() ->
    [{?__(1, "Agate"), agate}, {?__(2, "Boxed"), boxed},
        {?__(3, "Bozo"), bozo}, {?__(4, "Brick"), brick},
        {?__(5, "Bumps"), bumps}, {?__(6, "Cells"), cells},
        {?__(7, "Checker"), checker}, {?__(8, "Crackle"), crackle},
        {?__(9, "Cylindrical"), cylindrical}, {?__(10, "Dents"), dents},
        {?__(11, "Facets"), facets}, {?__(12, "Gradient"), gradient},
        {?__(13, "Granite"), granite}, {?__(14, "Hexagon"), hexagon},
        {?__(15, "Leopard"), leopard}, {?__(16, "Marble"), marble},
        {?__(17, "Onion"), onion}, {?__(18, "Planar"), planar},
        {?__(19, "Quilted"), quilted}, {?__(20, "Radial"), radial},
        {?__(21, "Ripples"), ripples}, {?__(22, "Slope"), slope},
        {?__(23, "Spherical"), spherical}, {?__(24, "Spiral1"), spiral1},
        {?__(25, "Spiral2"), spiral2}, {?__(26, "Waves"), waves}].

material_result(_Name, Mat, Res) ->
    %% Rip out all povray material properties
    {Found, Remaining} = rip_all(?TAG, Res),

    %% retrieve colormap entries
    NewColorMap = proplists:get_value(new_colormap, Found, false),
    {ColorMapNC, RemainingCD} = extract_map(Found, NewColorMap, {colorlevel, colorvalue, colordelete},
        fun() -> [{0.0, {0.0, 0.0, 0.0, 1.0}}] end),
    %% retrieve normalmap entries
    NewNormalMap = proplists:get_value(new_normalmap, Found, false),
    {NormalMapNC, RemainingND} = extract_map(RemainingCD, NewNormalMap, {normallevel, normalvalue, normaldelete},
        fun() -> [{0.0, agate}] end),

    FoundCM = [{colormap_list, ColorMapNC}, {normalmap_list, NormalMapNC} | RemainingND],

    NewMat = [{?TAG, FoundCM} | lists:keydelete(?TAG, 1, Mat)],
    {NewMat, Remaining}.

extract_map(Found, New, {Level, Value, Delete}, NewFunc) ->
    {Levels, RemainingL} = rip_all(Level, Found),
    {Values, RemainingV} = rip_all(Value, RemainingL),
    {Deletes, RemainingD} = rip_all(Delete, RemainingV),
    Lsort = sort(Levels, 0),
    Vsort = sort(Values, 0),
    Dsort = sort(Deletes, 0),
    Map = compose_map(Lsort, Vsort, Dsort),
    case New of
        false -> MapNew = Map;
        true -> MapNew = Map ++ NewFunc()
    end,
    {MapNew, RemainingD}.

compose_map([], _V, _D) ->
    [];
compose_map([{_, CL} | Levels], [{_, CV} | Values], [{_, CD} | Deletes]) ->
    case CD of
        true -> compose_map(Levels, Values, Deletes);
        false ->
            [{CL, CV} | compose_map(Levels, Values, Deletes)]
    end.

sort([], _Inc) ->
    [];
sort([{M, Val} | List], Increment) ->
    case M of
        Increment -> [{M, Val} | sort(List, Increment + 1)];
        _ -> sort(List ++ [{M, Val}], Increment)
    end.
%%% construct light dialog
light_dialog(Name, Light) ->
    OpenGL = proplists:get_value(opengl, Light),
    PovRay = proplists:get_value(?TAG, Light, []),
    Type = proplists:get_value(type, OpenGL, []),
    HookLight = fun(Key, Value, Store) ->
        case Key of
            ?KEY(photons) ->
                wings_dialog:enable(?KEY(pnl_light), Value =:= true, Store);
            _ -> ok
        end
    end,
    case Type of
        ambient -> % [];
            [{vframe, light_dialog(Name, Type, PovRay)}];
        _ ->
            LightBase = [
                {hframe, [
                    {hframe, [
                        {label, ?__(1, "Power")},
                        {text, proplists:get_value(light_power, PovRay, 1.0), [key(light_power)]}
                    ]},
                    panel,
                    {hframe, [
                        {?__(2, "Shadows"), proplists:get_value(shadows, PovRay, true), [key(shadows)]}
                    ]}
                ]},
                {hframe, [
                    {hframe, [
                        {label, ?__(3, "Fade Power")},
                        {text, proplists:get_value(fade_power, PovRay, 0.0), [key(fade_power)]}
                    ]},
                    panel,
                    {hframe, [
                        {label, ?__(4, "Fade Distance")},
                        {text, proplists:get_value(fade_distance, PovRay, 1.0), [key(fade_distance)]}
                    ]}
                ]},
                {hframe, [
                    {?__(5, "Media Interaction"), proplists:get_value(media_interaction, PovRay, true), [key(media_interaction)]},
                    panel,
                    {?__(6, "Media Attenuation"), proplists:get_value(media_attenuation, PovRay, false), [key(media_attenuation)]}
                ]},
                {hframe, [
                    {?__(7, "Photons"), proplists:get_value(photons, PovRay, false), [key(photons),{hook,HookLight}]},
                    {hframe, [
                        panel,
                        {?__(8, "Refraction"), proplists:get_value(refraction, PovRay, true), [key(refraction)]},
                        panel,
                        {?__(9, "Reflection"), proplists:get_value(reflection, PovRay, true), [key(reflection)]}
                    ], [key(pnl_light)]}
                ]}],
            LightExt =
                case light_dialog(Name, Type, PovRay) of
                    [] -> [];
                    LightExt0 -> [separator, {vframe, LightExt0}]
                end,
            LightBase++LightExt
    end.
light_dialog(_Name, spot, PovRay) ->
    [
        {hframe, [
            {label, ?__(11, "Tightness")},
            {slider, {text, proplists:get_value(tightness, PovRay, 0.0), [range({0.0, 100.0}), key(tightness)]}}
        ]},
        {?__(12, "Cylinder"), proplists:get_value(cylinder, PovRay, false), [key(cylinder)]}
    ];
light_dialog(_Name, area, PovRay) ->
    [
        {hframe, [
            {hframe, [
                {label, ?__(13, "Size 1")},
                {text, proplists:get_value(size_1, PovRay, 2), [key(size_1)]}
            ]},
            panel,
            {hframe, [
                {label, ?__(14, "Size 2")},
                {text, proplists:get_value(size_2, PovRay, 2), [key(size_2)]}
            ]}
        ]},
        {hframe, [
            {menu, [
                {?__(15, "No Adaptive"), none},
                {?__(16, "Adaptive 1"), 1},
                {?__(17, "Adaptive 2"), 2},
                {?__(18, "Adaptive 3"), 3},
                {?__(19, "Adaptive 4"), 4}
            ], proplists:get_value(adaptive, PovRay, none), [key(adaptive)]},
            panel,
            {hframe, [
                {?__(20, "Jitter"), proplists:get_value(jitter, PovRay, false), [key(jitter)]},
                panel,
                {?__(21, "Circular"), proplists:get_value(circular, PovRay, false), [key(circular)]},
                panel,
                {?__(22, "Orient"), proplists:get_value(orient, PovRay, false), [key(orient)]}
            ]}
        ]}
    ];
light_dialog(_Name, ambient, PovRay) ->
    Bg = proplists:get_value(bg_type, PovRay, ?DEF_BACKGROUND_AMBIENT),
    %%
    BgFnameImage = proplists:get_value(bg_filename_image, PovRay, ?DEF_BACKGROUND_FILENAME),
    ImageFormats = images_format(),
    BrowsePropsImage = [{dialog_type,open_dialog},
			{extensions,ImageFormats}],
    GammaIMG = io_lib:format("=~p",[?DEF_IMAGE_GAMMA]),
    %%
    BgFnameHDRI = proplists:get_value(bg_filename_hdri, PovRay, ?DEF_BACKGROUND_FILENAME),
    BrowsePropsHDRI = [{dialog_type,open_dialog},
                       {extensions,images_format_filter([hdr,exr])}],
    GammaHDRI = io_lib:format("=~p",[?DEF_HDRI_GAMMA]),
    %%
    Gamma = proplists:get_value(bg_gamma_correction, PovRay, ?DEF_GAMMA_CORRECTION),
    BgRotation = proplists:get_value(bg_rotation, PovRay, ?DEF_BACKGROUND_ROTATION),
    BgScale = proplists:get_value(bg_box_scale, PovRay, ?DEF_SKYBOX_SCALE),
    BgPower = proplists:get_value(bg_power, PovRay, ?DEF_BACKGROUND_POWER),
    %%
    HorizonColor = proplists:get_value(bg_horizon_color, PovRay, ?DEF_HORIZON_COLOR),
    HorizonElev = proplists:get_value(bg_horizon_elev, PovRay, ?DEF_HORIZON_ELEVATION),
    ZenithColor = proplists:get_value(bg_zenith_color, PovRay, ?DEF_ZENITH_COLOR),
    ZenithElev = proplists:get_value(bg_zenith_elev, PovRay, ?DEF_ZENITH_ELEVATION),

    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(bg_type) ->
                    wings_dialog:show(?KEY(pnl_file), is_member(Value, [hdri,skybox,skydome]), Store),
                    wings_dialog:show(?KEY(pnl_img_hdri), Value =:= hdri, Store),
                    wings_dialog:show(?KEY(pnl_img_bkg), is_member(Value, [skybox,skydome]), Store),
                    wings_dialog:show(?KEY(pnl_box_scale), is_member(Value, [hdri,skybox,skydome]), Store),
                    wings_dialog:show(?KEY(pnl_gradient), Value =:= gradient, Store),
                    wings_dialog:show(?KEY(pnl_background), Value =/= undefined, Store),
                    wings_dialog:update(?KEY(pnl_background), Store)
            end
        end,
    [
       %% Environment lights
        {vframe, [
            {hframe, [
                {label,?__(30,"Background Light/Environment")++" "},
                {menu, [
                    {?__(31,"None"), undefined},
                    {?__(32,"HDRI"),hdri},
                    {?__(33,"SkyBox"),skybox},
                    {?__(34,"SkyDome"),skydome},
                    {?__(35,"Gradient"),gradient}
                ], Bg, [key(bg_type),{hook,Hook_Show}]}
            ]},

            {vframe, [
                %% HDRI/SkyBox/SkyDome Background
                {vframe, [
                    {hframe, [
                        {label, ?__(36,"Filename") ++ "  "},
                        {hframe, [
                            {button,{text,BgFnameImage,[key(bg_filename_image),{width,35},{props,BrowsePropsImage}]}}
                        ],[key(pnl_img_bkg),{margin,false},{show,false}]},
                        {hframe, [
                            {button,{text,BgFnameHDRI,[key(bg_filename_hdri),{width,35},{props,BrowsePropsHDRI}]}}
                        ],[key(pnl_img_hdri),{margin,false}]}
                    ]},
                    {hframe, [
                        {label, ?__(37,"Gamma Correction")},
                        {text,Gamma,[key(bg_gamma_correction)]},
                        {label, " " ++
                                ?__(38,"* Suggested values: ") ++
                                ?__(32,"HDRI") ++ GammaHDRI ++ "; " ++
                                ?__(33,"SkyBox") ++ "/" ++
                                ?__(34,"SkyDome") ++ GammaIMG ++"."}
                    ]},
                    {label_column, [
                        {?__(39,"Rotation"),
                         {slider,{text,BgRotation,[key(bg_rotation),range({-360.0, 360.0})]}}
                        }
                    ],[{margin,false}]},
                    {hframe, [
                        {label_column, [
                            {?__(45,"Power"),
                                    {text,BgPower,[key(bg_power),range({0.0, 100.0})]}
                            }
                        ],[{margin,false}]},
                        {label_column, [
                            {?__(40,"Scale") ++" ",
                            {text, BgScale, [range({0.0, 100.0}),key(bg_box_scale)]}}
                        ], [key(pnl_box_scale),{margin,false}]}
                    ],[{margin,false}]}
                ],[key(pnl_file),{margin,false}]},
                %% Gradient Background
                {vframe,[
		    {hframe, [
			{label, ?__(42,"Zenith Color") ++ " "},
			{color,ZenithColor,[key(bg_zenith_color)]},
			panel,
			{label, ?__(43,"Elevation") ++ " "},
			{slider,{text,ZenithElev,[key(bg_zenith_elev),range({0.0, 180.0})]}}
		    ]},
		    {hframe, [
			{label, ?__(44,"Horizon Color") ++ " "},
			{color,HorizonColor,[key(bg_horizon_color)]},
			panel,
			{label, ?__(43,"Elevation") ++ " "},
			{slider,{text,HorizonElev,[key(bg_horizon_elev),range({0.0, 180.0})]}}
		    ]}
                ],[key(pnl_gradient),{margin,false},{show,false}]}
            ],[key(pnl_background),{margin,false}]}
        ]}];


light_dialog(_Name, _Type, _Povray) ->
    [].

light_result(_Name, Light, Res) ->
%%    OpenGL = proplists:get_value(opengl, Light),
%%    Type = proplists:get_value(type, OpenGL, []),
%%    case Type of
%%        ambient -> {Light, Res};
%%        _ ->
%%            {Found, Remaining} = rip_all(?TAG, Res),
%%            NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Light)],
%%            {NewLight, Remaining}
%%    end.
    {Found, Remaining} = rip_all(?TAG, Res),
    NewLight = [{?TAG, Found} | lists:keydelete(?TAG, 1, Light)],
    {NewLight, Remaining}.

clean_name([]) ->
    [];
clean_name([L | Name]) ->
    case L of
        32 -> [95 | clean_name(Name)]; %space to underscore
        45 -> [95 | clean_name(Name)]; %dash to underscore
    %92 -> [47 | clean_name(Name)]; %backslash to forward slash
        Val ->
            case Val < 48 of
                true -> [95 | clean_name(Name)];
                false ->
                    case Val > 57 of
                        false -> [Val | clean_name(Name)];
                        true ->
                            case Val < 65 of
                                true -> [95 | clean_name(Name)];
                                false ->
                                    case Val > 90 of
                                        false -> [Val | clean_name(Name)];
                                        true ->
                                            case Val < 97 of
                                                true -> [95 | clean_name(Name)];
                                                false ->
                                                    case Val > 122 of
                                                        false -> [Val | clean_name(Name)];
                                                        true -> [95 | clean_name(Name)]
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

images_format_filter(FmtList) ->
    ImgInfo = wings_job:render_formats(),
    lists:foldr(fun(Type,Acc) ->
        case lists:keyfind(Type,1,ImgInfo) of
            {_,Ext,Desc} -> Acc ++[{Ext,Desc}];
            _ -> Acc
        end
    end, [], FmtList).

images_format() ->
    images_format_filter([tga,jpg,png,hdr,exr]) ++
    [{".tiff","Tagged Image File Format"},
     {".gif","Graphics Interchange Format"}].

get_default_dim(res_square) -> {320,320};
get_default_dim(res_vga) -> {640,480};
get_default_dim(res_svga) -> {800,600};
get_default_dim(res_dv) -> {720,576};
get_default_dim(res_hd) -> {1280,720};
get_default_dim(res_fhd) -> {1920,1080}.

get_aspect_dim(AspRatio, W) ->
    AR = get_asp_ratio(AspRatio),
    {W,trunc(W*AR)}.

get_asp_ratio(ar11) -> 1.0;
get_asp_ratio(ar32) -> 2.0/3.0;
get_asp_ratio(ar43) -> 3.0/4.0;
get_asp_ratio(ar54) -> 4.0/5.0;
get_asp_ratio(ar126) -> 6.0/12.0;
get_asp_ratio(ar169) -> 9.0/16.0;
get_asp_ratio(ar219) -> 9.0/21.0.

%%%
%%% functions to manage the output file type
%%%
%%% returns a list of {atom, param string} for the output file type supported by POV-Ray
pov_output_exts() ->
    wings_job:render_formats(),
    OSDep = case os:type() of
                {win32, _} -> [{bmp,"+FS"}];
                _ -> []
            end,
    OSDep ++ [{png,"+FN"}, {tga,"+FT"}, {exr,"+FE"}, {hdr,"+FH"}, {jpg,"+FJ"}].

%%% returns the file extension and description of the file type
get_ext_info(ExtInfo) ->
    Exts = pov_output_exts(),
    lists:foldl(fun({Key, Ext, Dsc}, Acc) ->
        case lists:keysearch(Key, 1, Exts) of
            {value, _} -> Acc ++ [{Ext, Dsc}];
            _ -> Acc
        end
    end, [], ExtInfo).

%%% ensure to return a valid pair {atom, param string} for output file supported by POV-Ray
get_output_param(Filename) ->
    ExtsInfo = wings_job:render_formats(),
    PovExts = pov_output_exts(),
    {value, {_, DefParam}} = lists:keysearch(?DEF_RENDER_FORMAT, 1, PovExts),
    Ext = filename:extension(Filename),
    case lists:keysearch(Ext, 2, ExtsInfo) of
        {value, {RenderFormat, _, _}} ->
            case lists:keysearch(RenderFormat, 1, PovExts) of
                {value, {_, Param}} -> {RenderFormat, Param};
                _ -> {?DEF_RENDER_FORMAT, DefParam}
            end;
        _ -> {?DEF_RENDER_FORMAT, DefParam}
    end.

%%% pulls out all the values stored as {{KeyTag, SubKey}, Value}
%%% returns {ListOfFound, ListRemaining}
%%% ListOfFound is a list of {SubKey, Value}
rip_all(KeyTag, List) ->
    Keys = proplists:get_keys(List),
    rip_all(KeyTag, Keys, List).
rip_all(KeyTag, [Key | Keys], List) ->
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
rip_all(_K, _KL, List) ->
    {[], List}.

rip_keytag(KeyTag, {SetTag, _}) ->
    case KeyTag of
        SetTag -> true;
        _ -> false
    end;
rip_keytag(_KT, _ST) ->
    false.

%%% Set and get preference variables saved in the .wings file for this module
set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

set_user_prefs(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, Def) ->
    [{Key, Val}] = get_prefs([{Key, Def}]),
    Val.

get_prefs(KeyDefs) when is_list(KeyDefs) ->
    get_prefs_1(KeyDefs, make_ref()).

get_prefs_1([], _Undefined) ->
    [];
get_prefs_1([{Key, Def} | KeyDefs], Undefined) ->
    [{Key, case wpa:scene_pref_get(?MODULE, Key, Undefined) of
               Undefined ->
                   wpa:pref_get(?MODULE, Key, Def);
               Val ->
                   Val
           end} | get_prefs_1(KeyDefs, Undefined)].

get_user_prefs(KeyDefs) when is_list(KeyDefs) ->
    [{Key, wpa:pref_get(?MODULE, Key, Def)} || {Key, Def} <- KeyDefs].

%%% Set and get global variables (in the process dictionary)
%%% per wings session for this module.
set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    ets:insert(?LOCAL_MODULE, {Name,Value}).

get_var(Name) ->
    case ets:lookup(?LOCAL_MODULE, Name) of
        [] -> undefined;
        [{Name,Val}] -> Val
    end.

erase_var(Name) ->
    ets:delete(?LOCAL_MODULE, Name).

%%% General purpose function used by Enable/Show hooks.
%%%
is_member(Value, Members) ->
    lists:member(Value,Members).
