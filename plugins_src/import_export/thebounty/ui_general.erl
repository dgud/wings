%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%  See AUTHORS.txt for a complete list of authors.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

export_dialog(Op, Title) ->
    wpa:dialog(true, Title,
                export_dialog_qs(Op, get_prefs(export_prefs())),
               fun(Attr) -> {file,{Op,{?TAG,Attr}}} end, fun () -> help_export() end).

%% Export Render Options Dialog Settings
export_prefs() ->
    [
        {subdivisions,0},
        {keep_xml,false},
        {gui_mode, false},
        {raydepth,3},
        {gamma,2.2},
        {transparent_shadows,false},
        {shadow_depth,3},
        {threads_number, -1},
        {threads_auto, true},
        {verbosity_level, mute},
        {draw_params, false},
        {clay_pass, false},
        {z_pass, false},
        {render_format,png},
        {save_alpha, false},
        {clamp_rgb,false},
        {exr_float, false},
        {exr_flag_compression, exr_none},
        {aa_filter_type,?DEF_AA_FILTER_TYPE},
        {aa_pixelwidth,1.5},{aa_passes,1},{aa_samples,2},
        {aa_threshold,0.05},{aa_moresamples,1},
        {show_pixels, true},
        % lighting
        {lighting_method,?DEF_LIGHTING_METHOD},
        {use_caustics,?DEF_USE_CAUSTICS},
        {caustic_photons,?DEF_CAUSTIC_PHOTONS},
        {caustic_depth,?DEF_CAUSTIC_DEPTH},
        {caustic_mix,?DEF_CAUSTIC_MIX},
        {caustic_radius,?DEF_CAUSTIC_RADIUS},
        {pm_diffuse_photons,?DEF_PM_DIFFUSE_PHOTONS},
        {pm_bounces,?DEF_PM_BOUNCES},
        {pm_search,?DEF_PM_SEARCH},
        {pm_diffuse_radius,?DEF_PM_DIFFUSE_RADIUS},
        {pt_diffuse_photons,?DEF_PT_DIFFUSE_PHOTONS},
        {pt_bounces,?DEF_PT_BOUNCES},
        {sppm_photons,?DEF_SPPM_PHOTONS},
        {sppm_bounces,?DEF_SPPM_BOUNCES},
        {sppm_search,?DEF_SPPM_SEARCH},
        {sppm_radius,?DEF_SPPM_RADIUS},
        {do_ao,?DEF_DO_AO},
        {ao_distance,?DEF_AO_DISTANCE},
        {ao_samples,?DEF_AO_SAMPLES},
        {ao_color,?DEF_AO_COLOR},
        {pm_caustic_photons,?DEF_PM_CAUSTIC_PHOTONS},
        {pm_caustic_radius,?DEF_PM_CAUSTIC_RADIUS},
        {pm_caustic_mix,?DEF_PM_CAUSTIC_MIX},
        {pt_caustic_type,?DEF_PT_CAUSTIC_TYPE},
        {pt_caustic_radius,?DEF_PT_CAUSTIC_RADIUS},
        {pt_caustic_mix,?DEF_PT_CAUSTIC_MIX},
        {pt_caustic_depth,?DEF_PT_CAUSTIC_DEPTH},
        {sppm_times,?DEF_SPPM_TIMES},
        {sppm_passes,?DEF_SPPM_PASSES},
        {sppm_ire, ?DEF_SPPM_IRE},
        {pm_use_fg,?DEF_PM_USE_FG},
        {pm_fg_bounces,?DEF_PM_FG_BOUNCES},
        {pm_fg_samples,?DEF_PM_FG_SAMPLES},
        {pm_fg_show_map,?DEF_PM_FG_SHOW_MAP},
        {pt_samples,?DEF_PT_SAMPLES},
        {use_sss,?DEF_USE_SSS},
        {sss_photons,?DEF_SSS_PHOTONS},
        {sss_depth,?DEF_SSS_DEPTH},
        {sss_scale,?DEF_SSS_SCALE},
        {scatter_samples,32},
        {volintegr_type,?DEF_VOLINTEGR_TYPE},
        {volintegr_adaptive,?DEF_VOLINTEGR_ADAPTIVE},
        {volintegr_stepsize,?DEF_VOLINTEGR_STEPSIZE},
        {volintegr_optimize,?DEF_VOLINTEGR_OPTIMIZE},
        % camera
        {lens_type,?DEF_LENS_TYPE},
        {width,640},
        {height,480},
        {lens_scale,?DEF_LENS_ORTHO_SCALE},
        {lens_circular,?DEF_LENS_ANGULAR_CIRCULAR},
        {lens_max_angle,?DEF_LENS_ANGULAR_MAX_ANGLE},
        {lens_mirrored,?DEF_LENS_ANGULAR_MIRRORED},
        {lens_angle,?DEF_LENS_ANGULAR_ANGLE},
        {aperture,?DEF_APERTURE}, %% ??
        {bokeh_type,?DEF_BOKEH_TYPE},
        {bokeh_bias,?DEF_BOKEH_BIAS},
        {bokeh_rotation,?DEF_BOKEH_ROTATION},
        {dof_distance,0.0},
        % world environment
        {enviroment, constant}, {turbidity, 2.0},
        {a_var, 1.0}, {b_var, 1.0}, {c_var, 1.0}, {d_var, 1.0}, {e_var, 1.0},
        {add_sun, false}, {sky_light, false}, {sun_power, 1.0}, {sun_samples, 1},
        {background_power, 1.0}, {background_samples, 8}, {altitude, 1.0},
        {exposure, 1.0}, {night, false}, {bright, 1.0},
        {back_filename, ""}, {ibl_rotation, 180.0}, {ibl_mapping, spherical},
        {background_color, {0.8,0.8,0.8}}, {horizon_color, ?DEF_HORIZON_COLOR},
        {zenith_color, ?DEF_ZENITH_COLOR},
        {use_ibl, false}, {ibl_samples, 16}, {ibl_power, 1.0},
        {to_diffuse, false},{to_caustic, false}
        ].

f_stop_str(Value) when is_float(Value) ->
    %% we must use the same number of decimals used in the aperture edit box
    float_to_list(Value,[{decimals,6}]);
f_stop_str(Value) -> Value.

f_stop_find(Value,List) ->
    case lists:keyfind(f_stop_str(Value),2,List) of
        false -> {f_stop_str(-1.0),-1.0};
        {_,Idx,Val} -> {Idx,Val}
    end.

export_dialog_qs(Op, Attr) ->
    Aperture = get_pref(aperture,Attr),
    Custom = f_stop_str(-1.0),
    ApertureList = [
        {F, f_stop_str(math:sqrt(A)),math:sqrt(A)}
        || {F, A} <- [{"1.0", 1 / 1},{"1.4", 1 / 2},
                      {"2",   1 / 4},{"2.8", 1 / 8},
                      {"4",   1 / 16},{"5.6", 1 / 32},
                      {"8",   1 / 64},{"11",  1 / 128},
                      {"16",  1 / 256},{"22",  1 / 512},
                      {"32",  1 / 1024},{?__(47, "pinhole"), 0.0}]],

    {ApertureIdx,_} = f_stop_find(f_stop_str(Aperture),ApertureList),

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            threads_auto ->
                wings_dialog:enable(?KEY(pnl_threads), Value =:= false, Store);
            aa_passes ->
                wings_dialog:enable(?KEY(pnl_moresamples), Value > 1, Store),
                wings_dialog:enable(?KEY(pnl_pixels), Value > 1, Store);
            use_caustics ->
                wings_dialog:enable(?KEY(pnl_dl1), Value =:= true, Store);
            do_ao ->
                wings_dialog:enable(?KEY(pnl_use_ao), Value =:= true, Store);
            pm_use_fg ->
                wings_dialog:enable(?KEY(pnl_use_fg), Value =:= true, Store);
            use_sss ->
                wings_dialog:enable(?KEY(pnl_sss_opt), Value =:= true, Store);
            transparent_shadows ->
                wings_dialog:enable(?KEY(pnl_transp_shadow), Value =:= true, Store);
            render_format ->
                wings_dialog:enable(?KEY(pnl_exr_option), Value =:= exr, Store);
            aperture ->
                {Value0,_} = f_stop_find(Value,ApertureList),
                wings_dialog:set_value(aperture_idx, Value0, Store),
                wings_dialog:enable(?KEY(pnl_dof_type), Value =/= 0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_rotate), Value =/= 0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_distance), Value =/= 0.0, Store);
            aperture_idx ->
                if ((Value =/= "") and (Value =/= Custom)) ->
                        {_,Value0} = f_stop_find(Value,ApertureList),
                        wings_dialog:set_value(aperture, Value0, Store);
                    true -> ok
                end,
                Enabled = wings_dialog:get_value(aperture, Store) =/= 0.0,
                wings_dialog:enable(?KEY(pnl_dof_type), Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_rotate), Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_distance), Enabled, Store);
            _ -> ok
        end
    end,
    Hook_Show = fun(Key, Value, Store) ->
        case Key of
            lighting_method ->
                %% 1st collumn of panels
                %% Direct Light
                wings_dialog:enable(?KEY(pnl_dl1), wings_dialog:get_value(use_caustics, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_caustics), Value =:= directlighting, Store),
                %% Photon Mapping
                wings_dialog:show(?KEY(pnl_pm1), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt1), Value =:= pathtracing, Store),
                %% SPPM - GI
                wings_dialog:show(?KEY(pnl_sppm1), Value =:= sppm, Store),

                %% 2rd collumn of panels
                %% Direct Light
                wings_dialog:enable(?KEY(pnl_use_ao), wings_dialog:get_value(do_ao, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_ao), Value =:= directlighting, Store),
                %% Photon Mapping
                wings_dialog:show(?KEY(pnl_pm2), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt2), Value =:= pathtracing, Store),
                %% SPPM - GI
                wings_dialog:show(?KEY(pnl_sppm2), Value =:= sppm, Store),

                %% 3rd collumn of panels
                %% Photon Mapping
                wings_dialog:enable(?KEY(pnl_use_fg), wings_dialog:get_value(pm_use_fg, Store) =:= true, Store),
                wings_dialog:show(?KEY(pnl_pm3), Value =:= photonmapping, Store),
                %% Path Tracing - GI
                wings_dialog:show(?KEY(pnl_pt3), Value =:= pathtracing, Store),

                wings_dialog:enable(?KEY(pnl_sss), is_member(Value,[directlighting,photonmapping,pathtracing]), Store),
                wings_dialog:enable(?KEY(pnl_sss_opt), wings_dialog:get_value(use_sss, Store) =:= true, Store),

                wings_dialog:update(?KEY(pnl_light), Store);
            volintegr_type ->
                wings_dialog:enable(?KEY(pnl_volumetric), Value =:= singlescatterintegrator, Store);
            lens_type ->
                wings_dialog:show(?KEY(pnl_lens_scale), Value =:= orthographic, Store),
                wings_dialog:show(?KEY(pnl_lens_angle), Value =:= angular, Store),
                wings_dialog:update(?KEY(pnl_camera), Store);
            _ -> ok
        end
    end,
    %
    BrowsePropsHDRI = [{dialog_type,open_dialog},
                       {extensions,[
                            {".hdr",?__(398,"High Dynamic Range image")},
                            {".exr",?__(399,"OpenEXR image")}]
                        }],
    %
    GeneralOpt =
        {?__(100, "General options"),
            {vframe, [
                %!----------------------------------------
                %! Pre-Render group 100-105
                %!----------------------------------------
                {hframe, [
                    {hframe, [
                        {label, ?__(101, "Subdivisions")++" "},
                        {slider, {text, get_pref(subdivisions,Attr),[{key,subdivisions}, range(subdivisions)]}}
                    ]}, panel, panel,
                    {hframe, [
                        case Op of
                            render ->
                                {hframe,[
                                    {?__(102, "Write .xml file"), get_pref(keep_xml,Attr),[{key,keep_xml}]},
                                    panel,
                                    {?__(103, "GUI mode"), get_pref(gui_mode, Attr),[{key, gui_mode}]}
                                ]};
                            _ ->
                                %{value, get_pref(keep_xml,Attr), [{key,keep_xml}]},
                                {value, get_pref(gui_mode,Attr), [{key,gui_mode}]}
                        end
                    ],[{margin,false}]}
                ],[{title, ?__(104, "Pre-rendering")},{margin,false}]
                },
                %----------------------------
                % Render group 105-> 125
                %----------------------------
                {hframe, [
                    {label_column, [
                        %{?__(105, "Raydepth "),
                        %    {text, get_pref(raydepth,Attr),[range(raydepth),{key,raydepth},{width,5}]}},
                        {?__(106, "Gamma Out"),
                            {text, get_pref(gamma,Attr),[range(gamma),{key,gamma},{width,5}]}}
                    ]},
                    {vframe, [
                        {vframe, [
                            {?__(107, "Transp. Shadows"), get_pref(transparent_shadows,Attr),
                                [{key,transparent_shadows},{hook,Hook_Enable}]}
                        ]},
                        {hframe, [
                            {label, ?__(108, "Shadows Depth")},
                            {text, get_pref(shadow_depth,Attr), [range(shadow_depth),{key,shadow_depth}]}
                        ], [key(pnl_transp_shadow), {enabled,false},{margin,false}]}
                    ]},
                    {vframe, [
                        {hframe, [
                            {label, "Threads "},
                            {text, get_pref(threads_number,Attr), [range(threads_number),{key,threads_number}]}
                        ],[key(pnl_threads)]},
                        {hframe,[
                            {?__(109, "Auto Threads"), get_pref(threads_auto,Attr),[{key,threads_auto},{hook,Hook_Enable}]}
                        ]}
                    ]},
                    {vframe, [
                        {hframe,[
                            {label,?__(110, "Console ")},
                            {menu, [
                                {?__(111, "Mute"), mute},
                                {"Info", info},
                                {"Warning", warning},
                                {"Error", error}
                            ], get_pref(verbosity_level,Attr), [{key,verbosity_level}]}
                        ]},
                        {hframe,[
                            {?__(115, "Draw Parameters"), get_pref(draw_params,Attr), [{key,draw_params},{width,5}]}
                        ]}
                    ]},
                    {vframe,[
                        {hframe, [{?__(116, "Clay Pass"), get_pref(clay_pass,Attr), [{key,clay_pass}]}]},
                        {hframe, [{?__(117, "Z-Depth"), get_pref(z_pass,Attr), [{key,z_pass}]}]}
                    ]}
                ],[{title, ?__(125, "General settings")},{margin,false}]},

                %----------------------------
                % Output group 130 ->
                %----------------------------
                {hframe, [
                    {vframe,[
                        {hframe, [
                            {label, ?__(130, "Format ")},
                            {menu, [
                                {Ext ++ " (" ++ Desc ++ ")", Format}
                                || {Format, Ext, Desc} <- wings_job:render_formats(),
                                (Format == jpg) or (Format == tga) or (Format == tiff) or
                                (Format == png) or (Format == hdr) or (Format == exr)
                            ], get_pref(render_format,Attr), [{key,render_format},{hook,Hook_Enable}]}
                        ]},
                        {hframe, [
                            {?__(131, "Save Alpha"), get_pref(save_alpha,Attr), [{key,save_alpha}]},
                            {?__(132, "Clamp RGB"), get_pref(clamp_rgb,Attr),[{key,clamp_rgb}]}
                        ]}
                    ]},
                    {vframe, [
                        {hframe, [
                            {?__(133, "Float"), get_pref(exr_float,Attr), [{key,exr_float}]},
                            panel,
                            {label, ?__(134, "Compression ")},
                            {menu,[
                                {?__(135,"none"),exr_none},
                                {"run length",exr_rle},
                                {"scan line",exr_zlin},
                                {"scan block",exr_zblo},
                                {"piz based",exr_piz},
                                {"lossy 24b",exr_pxr24},
                                {"lossy 44",exr_b44},
                                {"lossy 44a",exr_b44a}
                            ], get_pref(exr_flag_compression,Attr), [{key,exr_flag_compression}]}
                        ], [key(pnl_exr_option),{enabled,false},{margin,false}]}
                    ]}
                ],[{title, ?__(136, "Image Output")},{margin,false}]},
                %!---------------------
                %% Antialising group
                %!---------------------
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label, ?__(137, "Method ")},
                            {menu, [
                                {?__(138, "Box"), box},
                                {?__(139, "Gaussian"), gauss},
                                {?__(140, "Mitchell"), mitchell},
                                {?__(141, "Lanczos"), lanczos}
                            ], get_pref(aa_filter_type,Attr), [{key,aa_filter_type}]},
                            panel,
                            {hframe, [
                                {label,?__(142, "Pixelwidth")},{text, get_pref(aa_pixelwidth,Attr),[range(aa_pixelwidth),{key,aa_pixelwidth}]}
                            ],[{margin,false}]}
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(143, "Passes"),
                                    {text, get_pref(aa_passes,Attr),[range(aa_passes),{key,aa_passes},{hook,Hook_Enable}]}},
                                {?__(144, "Samples"),{text, get_pref(aa_samples,Attr),[range(aa_samples),{key,aa_samples}]}}
                            ]},
                            {label_column, [
                                {?__(145, "Threshold"),   {text, get_pref(aa_threshold,Attr),[range(zero_one),{key,aa_threshold}]}},
                                {?__(146, "Add. Samples"),{text, get_pref(aa_moresamples,Attr),[range(samples),{key,aa_moresamples}]}}
                            ],[key(pnl_moresamples)]},
                            {vframe, [
                                {?__(147, "Show pixels"), get_pref(show_pixels,Attr),[{key,show_pixels}]},
                                {label, ?__(148, "Test ")}
                            ],[key(pnl_pixels)]}
                        ],[{margin,false}]}
                    ],[{margin,false}]}
                ],[{title, ?__(150, "Anti-Aliasing")},{margin,false}]}
            ]}
        },

    %!----------------------------------------
    %! Lighting group tab 200 ->
    %!----------------------------------------
    Lighting =
    {?__(200, "Lighting"),
        {vframe, [
            {vframe, [
                {hframe,[
                    {menu, [
                        {?__(201, "Direct Light"), directlighting},
                        {?__(202, "Photon Mapping"), photonmapping},
                        {?__(203, "Path Tracing"), pathtracing},
                        {?__(204, "Bidirectional Path Tracing"), bidirectional},
                        {?__(205, "SPPM"),sppm}
                    ], get_pref(lighting_method,Attr), [{key,lighting_method}, {hook,Hook_Show}]}
                ]},
                {hframe,[{hframe,[
                    {label, "Recursive Raytracing"},
                    {slider,{text, get_pref(raydepth,Attr),[range(raydepth),{key,raydepth}]}}
                ]}],[{margin,false}]},
                    %
                %% Start Direct Lighting Menu Section
                {hframe,[
                    % 1st collunm of panels
                    {vframe, [
                        {hframe,[
                            {"Caustics", get_pref(use_caustics,Attr), [{key,use_caustics},{hook,Hook_Enable}]}
                        ]},
                        {hframe, [
                            {vframe, [{label, "Photons "},{label,"Bounces" },{label,"Mixed"},{label,"Radius"}]},
                            {vframe, [
                                {text, get_pref(caustic_photons,Attr),[range(photons),{key,caustic_photons},{width,6}]},
                                {text, get_pref(caustic_depth,Attr),[range(caustic_depth),{key,caustic_depth},{width,6}]},
                                {text, get_pref(caustic_mix,Attr), [range(caustic_mix),{key,caustic_mix},{width,6}]},
                                {text, get_pref(caustic_radius,Attr),[range(caustic_radius),{key,caustic_radius},{width,6}]}
                            ]}
                        ],[key(pnl_dl1)]}
                    ],[key(pnl_caustics),{show,false},{magin,false}]},

                    {vframe, [
                        {hframe,[
                            {vframe, [{label, "Photons"},{label, "Bounces"},{label, "Search"},{label, "Radius"}]},
                            {vframe, [
                                {text, get_pref(pm_diffuse_photons,Attr), [range(photons),{key,pm_diffuse_photons}]},
                                {text, get_pref(pm_bounces,Attr), [range(pm_bounces),{key,pm_bounces}]},
                                {text, get_pref(pm_search,Attr), [range(pm_search),{key,pm_search}]},
                                {text, get_pref(pm_diffuse_radius,Attr), [range(pm_diffuse_radius),{key,pm_diffuse_radius}]}
                            ]}
                        ]}
                    ], [key(pnl_pm1)]},
                    {vframe, [
                        {hframe,[
                            {vframe, [{label,"Photons"},{label, "Bounces"}]},
                            {vframe, [
                                {text, get_pref(pt_diffuse_photons,Attr), [range(photons),{key,pt_diffuse_photons}]},
                                {text, get_pref(pt_bounces,Attr), [range(pt_bounces),{key,pt_bounces}]}
                            ]}
                        ]}
                    ], [key(pnl_pt1),{show,false}]
                    },
                    {vframe, [
                        {hframe, [{label, "Photons"},{text,get_pref(sppm_photons,Attr),[range(photons),{key,sppm_photons}]}]},
                        {hframe, [{label, "Bounces"},{text,get_pref(sppm_bounces,Attr),[range(sppm_bounces),{key,sppm_bounces}]}]},
                        {hframe, [{label, "Search "},{text,get_pref(sppm_search,Attr),[range(sppm_search),{key,sppm_search}]}]},
                        {hframe, [{label, "Radius "},{text,get_pref(sppm_radius,Attr),[range(sppm_radius),{key,sppm_radius}]}]}
                    ], [key(pnl_sppm1),{show,false}]},

                    %% 2nd collumn of panels
                    {vframe, [
                        {hframe, [{"Ambient Occlusion", get_pref(do_ao,Attr), [{key,do_ao},{hook,Hook_Enable}]}]},
                        {label_column, [
                            {?__(97, "AO Distance"),{text, get_pref(ao_distance,Attr), [range(ao_distance),{key,ao_distance}]}},
                            {?__(98, "AO Samples"),{text, get_pref(ao_samples,Attr), [range(ao_samples),{key,ao_samples}]}},
                            {?__(99, "AO Color"),{color, get_pref(ao_color,Attr), [{key,ao_color}]}}
                        ], [key(pnl_use_ao)]}
                    ], [key(pnl_ao),{show,false},{magin,false}]
                    },
                    {label_column, [
                        {?__(225, "Caustic Photons"),{text, get_pref(pm_caustic_photons,Attr), [range(photons),{key,pm_caustic_photons}]}},
                        {?__(226, "Caustic Radius"),{text, get_pref(pm_caustic_radius,Attr), [range(pm_caustic_radius),{key,pm_caustic_radius}]}},
                        {?__(227, "Caustic Mix"),{text, get_pref(pm_caustic_mix,Attr), [range(pm_caustic_mix),{key,pm_caustic_mix}]}}
                    ],[key(pnl_pm2)]
                    },
                    {vframe,[
                        {hframe,[
                             {label, ?__(245, "Caustic Type ")},
                             {menu, [
                                {?__(253, "path"), path},
                                {?__(254, "photons"), photons},
                                {?__(255, "both"), both},
                                {?__(256, "none"), none}
                            ], get_pref(pt_caustic_type,Attr), [{key,pt_caustic_type}]}
                        ]},
                        {label_column, [
                            {?__(257, "Caustic Radius"),{text, get_pref(pt_caustic_radius,Attr), [{key,pt_caustic_radius},range(pt_caustic_radius)]}},
                            {?__(258, "Caustic Mix"),{text, get_pref(pt_caustic_mix,Attr), [{key,pt_caustic_mix},range(pt_caustic_mix)]}},
                            {?__(259, "Caustic Depth"),{text, get_pref(pt_caustic_depth,Attr), [{key,pt_caustic_depth},range(pt_caustic_depth)]}}
                        ],[{margin,false}]}
                    ],[key(pnl_pt2),{show,false}]
                    },
                    {vframe,[
                        {label_column, [
                            {?__(260, "Times"),{text,get_pref(sppm_times,Attr),[{key,sppm_times},range(sppm_times),{width,6}]}},
                            {?__(261, "Passes"),{text,get_pref(sppm_passes,Attr),[{key,sppm_passes}, range(sppm_passes)]}}
                        ]},
                        {hframe,[
                            {?__(262,"IRE"),get_pref(sppm_ire,Attr),[{key,sppm_ire}]}
                        ]}
                    ],[key(pnl_sppm2),{show,false},{margin,false}]},

                    %% 3rd collumn of panels
                    {vframe, [
                        {vframe, [
                            {vframe,[
                                {?__(263, "Final Gather"), get_pref(pm_use_fg,Attr), [{key,pm_use_fg},{hook,Hook_Enable}]}
                            ],[{border,1}]},
                            {vframe, [
                                {label_column, [
                                    {?__(264, "FG Bounces"),{text, get_pref(pm_fg_bounces,Attr), [{key,pm_fg_bounces},range(pm_fg_bounces)]}},
                                    {?__(265, "FG Samples"),{text, get_pref(pm_fg_samples,Attr), [{key,pm_fg_samples},range(pm_fg_samples)]}}
                                ],[{margin,false}]
                                },
                                {vframe,[
                                    {?__(266, "Show Map"), get_pref(pm_fg_show_map,Attr), [{key,pm_fg_show_map}]}
                                ],[{border,1}]}
                            ], [key(pnl_use_fg),[{margin,false}]]
                            }
                        ], [key(pnl_pm3)]},
                        {label_column, [
                            {?__(267, "Path Samples"),{text, get_pref(pt_samples,Attr), [{key,pt_samples},range(pt_samples)]}}
                        ], [key(pnl_pt3)]}
                    ], [{margin,false}]}
                ],[key(pnl_light)]},

                {vframe, [
                    {vframe, [
                        {hframe, [{"Enabled", get_pref(use_sss,Attr), [{key,use_sss},{hook,Hook_Enable}]}]},
                        {hframe, [
                            {label_column, [
                                {?__(269, "Photons"),{text, get_pref(sss_photons,Attr), [{key,sss_photons},range(sss_photons)]}},
                                {?__(270, "Depth"),{text, get_pref(sss_depth,Attr), [{key,sss_depth},range(sss_depth)]}}
                            ]},
                            {label_column, [
                                {?__(271, "Scale"),{text, get_pref(sss_scale,Attr), [{key,sss_scale},range(sss_scale)]}},
                                {?__(272, "Scatter Samples"),{text, get_pref(scatter_samples,Attr), [{key,scatter_samples},range(scatter_samples)]}}
                            ]}
                        ],[key(pnl_sss_opt),{margin,false}]}
                    ],[{title, ?__(273, "SubSurface Scattering")},{margin,false}]}
                ],[key(pnl_sss)]}
            ],[{title, ""}]},

            %% Volumetrics group
            {hframe, [
                {menu, [
                    {?__(280, "None"), none},
                    {?__(281, "SingleScatter"), singlescatterintegrator}
                ], get_pref(volintegr_type,Attr), [{key,volintegr_type},{hook,Hook_Show}]},
                panel,
                {hframe, [
                    {"Adaptive", get_pref(volintegr_adaptive,Attr), [{key,volintegr_adaptive}]},
                    panel, {label, "StepSize"},
                    {text, get_pref(volintegr_stepsize,Attr), [{key,volintegr_stepsize},range(volintegr_stepsize)]},
                    panel, {"Optimize", get_pref(volintegr_optimize,Attr),[{key,volintegr_optimize}]}
                ], [key(pnl_volumetric)]}
            ],[{title, ?__(285, "Volumetrics")}]}
        ],[{margin,false}]}
    },

    Camera =
        {?__(300, "Camera") ,
            {vframe, [
                {vframe, [
                    {menu, [
                        {?__(301, "Perspective"), perspective},
                        {?__(302, "Orthographic"), orthographic},
                        {?__(303, "Architect"), architect},
                        {?__(304, "Fish Eye (Angular)"), angular}
                    ], get_pref(lens_type,Attr), [{key,lens_type},{hook,Hook_Show}]},
                    {hframe, [
                        {hframe, [
                            {label, ?__(305, "Width ")},
                            {text, get_pref(width,Attr), [range(pixels),{key,width},{width,5}]}
                        ]}, panel,
                        {hframe, [
                            {label, ?__(306, "Height ")},
                            {text, get_pref(height,Attr), [range(pixels),{key,height},{width,5}]}
                        ]}, panel,
                        {hframe, [
                            {label, ?__(307, "Scale ")},
                            {text, get_pref(lens_scale,Attr), [range(lens_scale),{key,lens_scale},{width,5}]}
                        ],[key(pnl_lens_scale)]}
                    ],[{margin,false}]},
                    {vframe, [
                        {hframe, [
                            {?__(308, "Circular"), get_pref(lens_circular,Attr),[{key,lens_circular},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(309, "Max Angle"),{text, get_pref(lens_max_angle,Attr),[range(lens_max_angle),{key,lens_max_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]},
                        {hframe, [
                            {?__(310, "Mirrored"), get_pref(lens_mirrored,Attr),[{key,lens_mirrored},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(311, "Frame/Angle"), {text, get_pref(lens_angle,Attr),[range(lens_angle),{key,lens_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]}
                    ],[key(pnl_lens_angle),{margin,false}]},

                    {hframe, [
                        {hframe, [
                            {label, ?__(312, "Aperture ")},
                            {text, Aperture, [range(aperture),{key,aperture},{hook,Hook_Enable}]}
                        ]},
                        panel,
                        {label, ?__(313, "f-stop ")},
                        {menu, [{F,A} || {F,A,_} <- ApertureList]++[{"Custom",Custom}],
                            ApertureIdx, [{key,aperture_idx},{hook,Hook_Enable}]
                        }],[{margin,false}]},

                    {hframe, [
                        {label, ?__(315, "DOF Type ")},
                        {menu, [
                            {?__(316, "Disk1"), disk1},
                            {?__(317, "Disk2"), disk2},
                            {?__(318, "Triangle"), triangle},
                            {?__(319, "Square"), square},
                            {?__(320, "Pentagon"), pentagon},
                            {?__(321, "Hexagon"), hexagon},
                            {?__(322, "Ring"), ring}
                        ], get_pref(bokeh_type,Attr), [{key,bokeh_type}]},
                        panel,
                        {label, ?__(323, "Bias ")},
                        {menu, [
                            {?__(324, "Uniform"), uniform},
                            {?__(325, "Center"), center},
                            {?__(326, "Edge"), edge}
                        ], get_pref(bokeh_bias,Attr), [{key,bokeh_bias}]}
                    ],[key(pnl_dof_type)]},

                    {hframe, [
                        {hframe, [
                            {label, ?__(327, "DOF Rotation ")},
                            {slider, {text, get_pref(bokeh_rotation,Attr), [range(bokeh_rotation),{key,bokeh_rotation}]}}
                        ]}
                    ],[key(pnl_dof_rotate),{margin,false}]},
                    {hframe, [
                        {hframe, [
                            {label, ?__(328, "DOF Distance ")},
                            {slider, {text, get_pref(dof_distance,Attr), [range(dof_distance),{key,dof_distance}]}}
                        ]}
                    ],[key(pnl_dof_distance),{margin,false}]}
                ],[key(pnl_camera)]}
            ],[{title,""}]}
        },
    %!---------------------------------------------------
    %! World enviroment logic values for UI
    %!---------------------------------------------------
    WHook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                add_sun ->
                    wings_dialog:enable(?KEY(pnl_sun_power), Value =/= false, Store),
                    wings_dialog:enable(?KEY(pnl_sun_samples), Value =/= false, Store);

                sky_light ->
                    wings_dialog:enable(?KEY(pnl_enlight_photons), Value =/= false, Store),
                    wings_dialog:enable(?KEY(pnl_sky_power), Value =/= false, Store),
                    wings_dialog:enable(?KEY(pnl_sky_samples), Value =/= false, Store);

                use_ibl ->
                    wings_dialog:enable(?KEY(pnl_ibl_power), Value =/= false, Store),
                    wings_dialog:enable(?KEY(pnl_ibl_samples), Value =/= false, Store),
                    wings_dialog:enable(?KEY(pnl_enlight_photons), Value =/= false, Store)
            end
        end,
    WHook_Show =
        fun(Key, Value, Store) ->
            case Key of
                enviroment ->
                    wings_dialog:show(?KEY(pnl_bkg_power), is_member(Value,[darksky,sunsky]), Store),

                    wings_dialog:show(?KEY(pnl_add_sun), is_member(Value,[darksky,sunsky]), Store),

                    wings_dialog:show(?KEY(pnl_alt_night), Value =:= darksky, Store),

                    wings_dialog:show(?KEY(pnl_bright_night), Value =:= darksky, Store),

                    wings_dialog:show(?KEY(pnl_sky), is_member(Value,[darksky,sunsky]), Store),

                    wings_dialog:show(?KEY(pnl_const), Value =:= constant, Store),

                    wings_dialog:show(?KEY(pnl_gradient), Value =:= gradientback, Store),

                    wings_dialog:show(?KEY(panel_ibl), not is_member(Value,[darksky,sunsky]), Store),

                    wings_dialog:show(?KEY(pnl_file), Value =:= textureback, Store),

                    wings_dialog:show(?KEY(pnl_enlight_photons), is_member(Value,[textureback,darksky]), Store),

                    wings_dialog:update(?KEY(pnl_background), Store)
            end
        end,
    World =
    {?__(400, "World") ,
        {vframe,[
            {menu, [
                {?__(401,"Constant Color"),constant},
                {?__(402,"Gradient Color"),gradientback},
                {?__(403,"Textured background"),textureback},
                {?__(404,"Sunsky"),sunsky},
                {?__(405,"DarkSky"),darksky}
            ], get_pref(enviroment,Attr), [{key,enviroment},{hook,WHook_Show}]},
            panel,
            %!------------------------------------
            %! DarkSky
            %!------------------------------------
            {vframe, [
                {hframe, [
                    {vframe, [{label,"Turbidity"},{label,"Hor. Bright"},{label,"Hor. Spread"}]},
                    {vframe, [
                        {slider,{text,get_pref(turbidity,Attr),[range(zero_five),{key,turbidity}]}},
                        {slider,{text,get_pref(a_var,Attr),[range(zero_five),{key,a_var}]}},
                        {slider,{text,get_pref(b_var,Attr),[range(zero_five),{key,b_var}]}}]},
                    {vframe, [{label,"Sun Bright"},{label,"Sun Distance"},{label,"Back Light"}]},
                    {vframe, [
                        {slider, {text,get_pref(c_var,Attr),[range(zero_five),{key,c_var}]}},
                        {slider, {text,get_pref(d_var,Attr),[range(zero_five),{key,d_var}]}},
                        {slider, {text,get_pref(e_var,Attr),[range(zero_five),{key,e_var}]}}
                    ]}
                ],[key(pnl_sky),{margin,false}]},
                % altitude and night
                {hframe,[
                    {vframe, [{label, "Altitude"}]},
                    {vframe, [{slider, {text,get_pref(altitude,Attr),[range(zero_five),{key,altitude}]}}]},panel,
                    {vframe, [{label, "Exposure"}]},
                    {vframe, [{slider, {text,get_pref(exposure,Attr),[range(zero_five),{key,exposure}]}}]}
                ],[key(pnl_alt_night),{margin,false}]},
                {hframe,[
                    {hframe, [
                        {"Night",get_pref(night, Attr),[{key,night}]}, panel,
                        {label, "Sky brightness"},
                        {slider, {text,get_pref(bright,Attr),[range(zero_to_ten),{key,bright},{width,5}]}}
                    ]}
                ],[key(pnl_bright_night),{margin,false}]},

                {hframe,[
                    {hframe, [{"Real Sun ",get_pref(add_sun, Attr),[{key,add_sun},{hook,WHook_Enabled}]}]},
                    {hframe, [
                        {label, "Power"},
                        {slider, {text, get_pref(sun_power, Attr),[range(zero_to_ten),{key,sun_power},{width,5}]}}
                    ],[key(pnl_sun_power),{margin,false}]},
                    panel,
                    {hframe,[
                        {label,"Samples"},{slider, {text,get_pref(sun_samples, Attr),[range(samples),{key,sun_samples}]}}
                    ],[key(pnl_sun_samples),{margin,false}]}
                ],[key(pnl_add_sun),{margin,true}]},
                %panel,
                {hframe,[
                    {hframe, [{"Sky Light", get_pref(sky_light, Attr),[{key,sky_light},{hook,WHook_Enabled}]}]},
                    {hframe, [
                        {label," Power"},
                        {slider, {text,get_pref(background_power, Attr),[range(zero_ten),{key,background_power},{width,5}]}}
                    ],[key(pnl_sky_power),{margin,false}]},
                    panel,
                    {hframe,[
                        {label,"Samples"},
                        {slider, {text,get_pref(background_samples, Attr),[range(samples),{key,background_samples}]}}
                    ],[key(pnl_sky_samples),{margin,false}]
                    }
                ],[key(pnl_bkg_power),{margin,false}]},
                %!----------------------
                %! texture background
                %!----------------------
                {vframe, [
                    {hframe, [
                        {label,"HDRI File"},
                        {button,{text,get_pref(back_filename, Attr),[{key,back_filename},{props,BrowsePropsHDRI},{width, 25}]}},
                        panel
                    ],[{margin,false}]},
                    panel,
                    {hframe,[
                        {label,"Rotation"},
                        {slider, {text,get_pref(ibl_rotation, Attr),[range(ibl_rotation),{key,ibl_rotation}]}},
                        panel,
                        {menu, [
                            {?__(425,"Mapping Angular"),angular},
                            {?__(426,"Mapping Spherical"),spherical}
                        ], get_pref(ibl_mapping, Attr), [{key,ibl_mapping}]},
                        panel
                    ],[{margin,false}]}
                ],[key(pnl_file),{margin,false}]},
                %!------------------------
                %! Constant Background
                %!------------------------
                {hframe, [
                    {label,"Color"},{color,get_pref(background_color, Attr),[{key,background_color}]}
                ],[key(pnl_const),{margin,false}]},
                %!------------------------
                %! Gradient Background
                %!------------------------
                {hframe,[
                    {label,"Horizon Color"},{color,get_pref(horizon_color, Attr),[{key,horizon_color}]},
                    panel,
                    {label,"Zenith Color"}, {color,get_pref(zenith_color, Attr),[{key,zenith_color}]}
                ],[key(pnl_gradient),{margin,false}]},
                %% Common parameters
                {hframe,[
                    {hframe,[
                        {"Use IBL",get_pref(use_ibl, Attr),[{key,use_ibl},{hook,WHook_Enabled}]}
                    ],[{margin,false}]},% panel,
                    {hframe,[
                        {label,"IBL Power"},{slider, {text,get_pref(ibl_power, Attr),[range(zero_one),{key,ibl_power}]}}
                    ],[key(pnl_ibl_power),{margin,false}]},
                    {hframe,[
                        panel,
                        {label,"Samples"},{slider, {text,get_pref(ibl_samples, Attr),[range(samples),{key,ibl_samples}]}},
                        panel
                    ],[key(pnl_ibl_samples),{margin,false}]}
                ],[key(panel_ibl),{margin,false}]},
                %! influence of background light
                {hframe, [
                    {"Affect Diffuse Photons",get_pref(to_diffuse, Attr),[{key,to_diffuse}]},
                    panel,
                    {"Affect Caustic Photons",get_pref(to_caustic, Attr),[{key,to_caustic}]}
                ],[key(pnl_enlight_photons),{margin,false}]}

            ],[key(pnl_background),{margin,false}]}

        ],[{title, ?__(450, "Environment")},{margin,false}]}
    },

    [{oframe, [ GeneralOpt, Lighting, Camera, World ], 1, [{style, buttons}]}].

%% Used to lookup in Store for combinations of values.
is_member(Value, Members) ->
    lists:member(Value,Members).
