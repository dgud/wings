%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the GNU General Public License for more details.
%%

export_dialog(Op, Title) ->
    wpa:dialog(true, Title,
               export_dialog_qs(Op, get_prefs(export_prefs())),
               fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

%% Export Render Options Dialog Settings
export_prefs() ->
    [
        {subdivisions,?DEF_SUBDIVISIONS},
        {keep_xml,?DEF_KEEP_XML},
        {threads_number,?DEF_THREADS_NUMBER},
        {threads_auto,?DEF_THREADS_AUTO},
        {lighting_method,?DEF_LIGHTING_METHOD},
        {use_caustics,?DEF_USE_CAUSTICS},
        {caustic_photons,?DEF_CAUSTIC_PHOTONS},
        {caustic_depth,?DEF_CAUSTIC_DEPTH},
        {caustic_mix,?DEF_CAUSTIC_MIX},
        {caustic_radius,?DEF_CAUSTIC_RADIUS},
        {do_ao,?DEF_DO_AO},
        {ao_distance,?DEF_AO_DISTANCE},
        {ao_samples,?DEF_AO_SAMPLES},
        {ao_color,?DEF_AO_COLOR},
        {pm_diffuse_photons,?DEF_PM_DIFFUSE_PHOTONS},
        {pm_bounces,?DEF_PM_BOUNCES},
        {pm_search,?DEF_PM_SEARCH},
        {pm_diffuse_radius,?DEF_PM_DIFFUSE_RADIUS},
        {pm_caustic_photons,?DEF_PM_CAUSTIC_PHOTONS},
        {pm_caustic_radius,?DEF_PM_CAUSTIC_RADIUS},
        {pm_caustic_mix,?DEF_PM_CAUSTIC_MIX},
        {pm_use_fg,?DEF_PM_USE_FG},
        {pm_fg_bounces,?DEF_PM_FG_BOUNCES},
        {pm_fg_samples,?DEF_PM_FG_SAMPLES},
        {pm_fg_show_map,?DEF_PM_FG_SHOW_MAP},
        {pt_diffuse_photons,?DEF_PT_DIFFUSE_PHOTONS},
        {pt_bounces,?DEF_PT_BOUNCES},
        {pt_caustic_type,?DEF_PT_CAUSTIC_TYPE},
        {pt_caustic_radius,?DEF_PT_CAUSTIC_RADIUS},
        {pt_caustic_mix,?DEF_PT_CAUSTIC_MIX},
        {pt_caustic_depth,?DEF_PT_CAUSTIC_DEPTH},
        {pt_samples,?DEF_PT_SAMPLES},
        {sppm_photons,?DEF_SPPM_PHOTONS},
        {sppm_bounces,?DEF_SPPM_BOUNCES},
        {sppm_search,?DEF_SPPM_SEARCH},
        {sppm_radius,?DEF_SPPM_RADIUS},
        {sppm_times,?DEF_SPPM_TIMES},
        {sppm_passes,?DEF_SPPM_PASSES},
        {sppm_ire, ?DEF_SPPM_IRE},
        {volintegr_type,?DEF_VOLINTEGR_TYPE},
        {volintegr_adaptive,?DEF_VOLINTEGR_ADAPTIVE},
        {volintegr_optimize,?DEF_VOLINTEGR_OPTIMIZE},
        {volintegr_stepsize,?DEF_VOLINTEGR_STEPSIZE},
        {use_sss,?DEF_USE_SSS},
        {sss_photons,?DEF_SSS_PHOTONS},
        {sss_depth,?DEF_SSS_DEPTH},
        {sss_scale,?DEF_SSS_SCALE},
        {sss_scatter_samples,?DEF_SSS_SINGLESCATTER_SAMPLES},
        {raydepth,?DEF_RAYDEPTH},
        {gamma,?DEF_GAMMA},
        {transparent_shadows,?DEF_TRANSPARENT_SHADOWS},
        {shadow_depth,?DEF_SHADOW_DEPTH},
        %
        {clay_pass, false},
        {z_pass, false},
        %
        {render_format,?DEF_RENDER_FORMAT},
        {exr_float,false},
        {exr_flag_compression,?DEF_EXR_FLAG_COMPRESSION},
        {aa_passes,?DEF_AA_PASSES},
        %add
        {verbosity_level, mute},
        {draw_params, false},
        {aa_moresamples,?DEF_AA_PASSES },
        {aa_samples,?DEF_AA_MINSAMPLES},
        {aa_threshold,?DEF_AA_THRESHOLD},
        {aa_pixelwidth,?DEF_AA_PIXELWIDTH},
        {clamp_rgb,?DEF_CLAMP_RGB},
        {aa_filter_type,?DEF_AA_FILTER_TYPE},
        % camera
        {lens_type,?DEF_LENS_TYPE},
        {lens_ortho_scale,?DEF_LENS_ORTHO_SCALE},
        {lens_angular_circular,?DEF_LENS_ANGULAR_CIRCULAR},
        {lens_angular_mirrored,?DEF_LENS_ANGULAR_MIRRORED},
        {lens_angular_max_angle,?DEF_LENS_ANGULAR_MAX_ANGLE},
        {lens_angular_angle,?DEF_LENS_ANGULAR_ANGLE},
        {bokeh_use_QMC,?DEF_USE_QMC},
        {width,?DEF_WIDTH},
        {aperture,?DEF_APERTURE},
        {bokeh_type,?DEF_BOKEH_TYPE},
        {height,?DEF_HEIGHT},
        {bokeh_bias,?DEF_BOKEH_BIAS},
        {bokeh_rotation,?DEF_BOKEH_ROTATION},
        {dof_distance,?DEF_DOF_DISTANCE},
        %{save_alpha,?DEF_SAVE_ALPHA},
        {environment, constant},
        {turbidity, 2.0},{a_var, 1.0},{b_var, 1.0},{c_var, 1.0},{d_var, 1.0},{e_var, 1.0},
        {altitude, 1.0},{add_sun, false},{sun_power, 1.0},{background_light, false},
        {background_power, 1.0},{background_samples, 8},{to_diffuse_photons, false},
        {to_caustics_photons, false},{night, false},
        % start---------------------------
        {background_color, {0.25,0.25,0.50}}, {horizon_color, ?DEF_HORIZON_COLOR},{zenith_color, ?DEF_ZENITH_COLOR},
        {back_filename, ""}, {background_mapping, probe},{use_ibl, false},
        {ambient_diffusephotons, false}, {ambient_causticphotons, false}, {background_rotation, 0.0},
        {samples, 8}
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
                wings_dialog:enable(bokeh_use_QMC, Value =/= 0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_type), Value =/= 0.0, Store),
                wings_dialog:enable(?KEY(pnl_dof_sliders), Value =/= 0.0, Store);
            aperture_idx ->
                if ((Value =/= "") and (Value =/= Custom)) ->
                        {_,Value0} = f_stop_find(Value,ApertureList),
                        wings_dialog:set_value(aperture, Value0, Store);
                    true -> ok
                end,
                Enabled = wings_dialog:get_value(aperture, Store) =/= 0.0,
                wings_dialog:enable(bokeh_use_QMC, Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_type), Enabled, Store),
                wings_dialog:enable(?KEY(pnl_dof_sliders), Enabled, Store);
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

%%     % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%     ButtonsHook = fun(Key,_,_Store) ->
%%         io:format("Button ~p pressed...\n",[Key])
%%     end,
%!----------------------------------------
%!----------------------------------------
    WHook_Enabled =
    fun(Key, Value, Store) ->
        case Key of
            ?KEY(add_sun) ->
                wings_dialog:enable(?KEY(pnl_add_sun), Value =/= ?DEF_SUN_REAL, Store);
            ?KEY(background_light) ->
                wings_dialog:enable(?KEY(pnl_bkg_power), Value =/= ?DEF_SKY_BACKGROUND_LIGHT, Store),
                wings_dialog:enable(?KEY(pnl_bkg_photons), Value =/=?DEF_SKY_BACKGROUND_LIGHT, Store);
			?KEY(use_ibl) ->
				wings_dialog:enable(?KEY(pnl_ibl_samples), Value =/= false, Store)%,
                %wings_dialog:enable(?KEY(pnl_enlight_photons), Value =/= false, Store)
        end
    end,
    WHook_Show =
    fun(Key, Value, Store) ->
        case Key of
            ?KEY(environment) ->
                wings_dialog:show(?KEY(pnl_sky), Value =:= darksky, Store), %=/= undefined, Store),
				wings_dialog:update(?KEY(pnl_sky), Store),
				wings_dialog:show(?KEY(pnl_background), Value =:= constant, Store),
                wings_dialog:update(?KEY(pnl_background), Store)
        end
    end,
    %!----------------------------------------
    %! Environment background 150 ->
    %!----------------------------------------
	BrowsePropsHDRI = [{dialog_type,open_dialog},
                       {extensions,[
                            {".hdr",?__(398,"High Dynamic Range image")},
                            {".exr",?__(399,"OpenEXR image")}]
                        }],
    World =
    {?__(400, "World") ,
        {vframe,[
            {menu, [
                {?__(401,"Constant"),constant},
                {?__(402,"Gradient"),gradientback},
                {?__(403,"Texture"),texture},
                {?__(404,"Sunsky"),sunsky},
                {?__(405,"DarkSky"),darksky}
            ], get_pref(environment,Attr), [key(environment), {hook,WHook_Show}]},
            %!------------------------------------
            %! DarkSky
            %!------------------------------------
            {vframe, [
                {hframe, [
                    {label_column, [
                        {?__(407,"Turbidity"),{text,get_pref(turbidity,Attr),[range(turbidity),key(turbidity),{width, 5}]}},
                        {?__(408,"Horizon Brightness"),{text,get_pref(a_var,Attr),[key(a_var),{width, 5}]}},
                        {?__(409,"Horizon Spread"),{text,get_pref(b_var,Attr),[key(zero_to_ten),{width, 5}]}}
                    ],[{margin,false}]},
                    {label_column, [
                        {?__(410,"Sun Brightness"),{text,get_pref(c_var,Attr),[key(c_var),{width, 5}]}},
                        {?__(411,"Sun Distance"),{text,get_pref(d_var,Attr),[key(d_var),{width, 5}]}},
                        {?__(412,"Backscattered Light"),{text,get_pref(e_var,Attr),[key(e_var),{width, 5}]}}
                    ],[{margin,false}]}
                ],[{margin,false}]},
            
                {hframe,[
                    {hframe,[
                        {label,?__(413,"Altitude")},
                        {text,get_pref(altitude,Attr),[range(altitude),key(altitude)]}
                    ],[key(pnl_darksky_altitude),{margin,false}]
                    }
                ]},

                {hframe,[
                    {?__(414,"Add Sun"),get_pref(add_sun, Attr),[key(add_sun),{hook,WHook_Enabled}]},
                    panel, panel,
                    {?__(416,"Add Skylight"),get_pref(background_light, Attr),[key(background_light),{hook,WHook_Enabled}]}%,
                    %panel,
                    %{hframe,[
                    %    {label,?__(415,"Sun Power")},
                    %    {text, get_pref(sun_power, Attr),[range(sun_power),key(sun_power),{width,4}]}
                    %    ],[key(pnl_add_sun),{margin,false}]
                    %}
                ]},

                {vframe,[
                    {hframe, [
                    %{?__(416,"Skylight"),get_pref(background_light, Attr),[key(background_light),{hook,WHook_Enabled}]},
                        {hframe,[
                            {label,?__(415,"Sun Power")},
                            {text, get_pref(sun_power, Attr),[range(sun_power),key(sun_power),{width,5}]}
                            ],[key(pnl_add_sun),{margin,false}]
                        },
                        %panel,
                        {hframe, [
                            {label,?__(417," Sky Power")},
                            {text,get_pref(background_power, Attr),[range(zero_to_ten),key(sky_background_power),{width,5}]},
                            {label,?__(418,"Samples")},
                            {text,get_pref(background_samples, Attr),[range(background_samples),key(background_samples),{width,5}]}
                        ], [key(pnl_bkg_power),{margin,false}]}
                    ]},
                    {hframe, [
                        {?__(419,"Diffuse Photons"),get_pref(to_diffuse_photons,Attr),[key(to_diffuse_photons)]},
                        panel,
                        {?__(420,"Caustic Photons"),get_pref(to_caustics_photons, Attr),[key(to_caustics_photons)]}
                    ],[key(pnl_bkg_photons)]}
                ],[{margin,false}]
                },
                {vframe,[
                    {?__(421,"Night"),get_pref(night, Attr),[key(night)]}
                ]}
            ],[key(pnl_sky),{margin,false}]
            },
			% end darksky
			{vframe, [
                %% HDRI Background
                {vframe, [
                    {label_column, [
                        {?__(425,"Filename"),
                            {vframe, [
                                {hframe, [
                                    {button,{text,get_pref(back_filename, Attr),[key(back_filename),{width,35},{props,BrowsePropsHDRI}]}},
                                    {menu, [
                                        {?__(426,"Light Probe"),probe},
                                        {?__(427,"Spherical"),spherical}
                                    ], get_pref(background_mapping, Attr), [key(background_mapping)]}
                                ],[key(pnl_img_hdri),{margin,false}]}
                            ],[{margin,false}]}},
                        {?__(428,"Rotation"),
                            {text,get_pref(background_rotation, Attr),[range(background_rotation),key(background_rotation)]}
                        }
                    ],[{margin,false}]
                }],
                [key(pnl_file),{margin,false}]
                },
				%!------------------------
                %! Constant Background
				%!------------------------
                {hframe, [
                    {label,?__(429,"Color")},
                    {color,get_pref(background_color, Attr),[key(background_color)]}
                ],[key(pnl_const),{show,false}]},
                %% Gradient Background
                {hframe,[
                    {label,?__(430,"Horizon Color")},
                    {color,get_pref(horizon_color, Attr),[key(horizon_color)]},
                    panel,
                    {label,?__(431,"Zenith Color")},
                    {color,get_pref(zenith_color, Attr),[key(zenith_color)]}
                ],[key(pnl_gradient),{show,false}]},
                %% Common parameters
                {vframe,[
                    {hframe,[
                        {hframe,[
                            {?__(432,"Use IBL"),get_pref(use_ibl, Attr),[key(use_ibl),{hook,WHook_Enabled}]}
                        ]},
                        panel,
                        {hframe,[
                            {label,?__(433,"Samples")},
                            {text,get_pref(samples, Attr),[range(samples),key(samples)]}
                        ],[key(pnl_ibl_samples),{margin,false}]}
                    ],[{margin,false}]},
                    {hframe, [
                        {?__(94,"Diffuse Photons"),get_pref(ambient_diffusephotons, Attr),[key(ambient_diffusephotons)]},
                        panel,
                        {?__(96,"Caustic Photons"),get_pref(ambient_causticphotons, Attr),[key(ambient_causticphotons)]}
                    ],[key(pnl_enlight_photons)]}
                ]}
            ],[key(pnl_background),{margin,false}]}
            % end import code
        ],[{title, ?__(450, "Environment")},{margin,false}]}
    },
    %!-------------------------
    %!
    %!-------------------------
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
                                    panel
                                ]};
                            _ ->
                                {value, get_pref(keep_xml,Attr), [{key,keep_xml}]}
                        end
                    ],[{margin,false}]}
                ],[{title, ?__(104, "Pre-rendering")},{margin,false}]
                },
                %----------------------------
                % Render group 105-> 125
                %----------------------------
                {hframe, [
                    {label_column, [
                        {?__(105, "Raydepth "),
                            {text, get_pref(raydepth,Attr),[range(raydepth),{key,raydepth},{width,5}]}},
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
                            {?__(115, "Draw Params"), get_pref(draw_params,Attr), [{key,draw_params},{width,5}]}
                        ]}
                    ]},
                    {vframe,[
                        {hframe,[
                            {?__(116, "Clay Pass"), get_pref(clay_pass,Attr), [{key,clay_pass}]}%,{width,5}]}
                        ]},
                        {hframe,[
                            {?__(117, "Z-Depth"), get_pref(z_pass,Attr), [{key,z_pass}]}%,{width,5}]}
                        ]}
                    ]}
                ],[{title, ?__(125, "General settings")},{margin,false}]},

                %----------------------------
                % Output group 130 ->
                %----------------------------
                {hframe, [
                    {hframe, [
                        {label, ?__(130, "Format ")},
                        {menu, [
                            {Ext ++ " (" ++ Desc ++ ")", Format}
                            || {Format, Ext, Desc} <- wings_job:render_formats(),
                            (Format == tga) or (Format == tif) or (Format == png) or
                            (Format == hdr) or (Format == exr)
                        ], get_pref(render_format,Attr), [{key,render_format},{hook,Hook_Enable}]}
                    ]},
                    panel,
                    {hframe, [
                        {?__(131, "Float"), get_pref(exr_float,Attr), [{key,exr_float}]},
                        panel,
                        {label, ?__(133, "Compression ")},
                        %------------------------------------
                        {menu,[
                            {?__(134,"none"),exr_none},
                            {"run length",exr_rle},
                            {"scan line",exr_zlin},
                            {"scan block",exr_zblo},
                            {"piz based",exr_piz},
                            {"lossy 24b",exr_pxr24},
                            {"lossy 44",exr_b44},
                            {"lossy 44a",exr_b44a}
                        ], get_pref(exr_flag_compression,Attr), [{key,exr_flag_compression}]}
                    ], [key(pnl_exr_option),{enabled,false},{margin,false}]}
                ],[{title, ?__(135, "Image Output")},{margin,false}]},
                %!---------------------
                %% Antialising group
                %!---------------------
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label, ?__(136, "Method ")},
                            {menu, [
                                {?__(137, "Box Filter"), box},
                                {?__(138, "Gaussian Filter"), gauss},
                                {?__(139, "Mitchell-Netravali"), mitchell},
                                {?__(140, "Lanczos Filter"), lanczos}
                            ], get_pref(aa_filter_type,Attr), [{key,aa_filter_type}]},
                            panel,
                            {hframe, [
                                {label, ?__(141, "Add. Samples")},{text, get_pref(aa_moresamples,Attr), [{key,aa_moresamples}]}
                            ]}, panel,
                            {hframe, [
                                {?__(142, "Clamp RGB"), get_pref(clamp_rgb,Attr),[{key,clamp_rgb}]}
                            ]}
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(143, "Passes"),{text, get_pref(aa_passes,Attr),[range(aa_passes),{key,aa_passes}]}},
                                {?__(144, "Samples"),{text, get_pref(aa_samples,Attr),[range(aa_samples),{key,aa_samples}]}}
                            ]},
                            {label_column, [
                                {?__(145, "Threshold"),
                                    {text, get_pref(aa_threshold,Attr),[range(aa_threshold),{key,aa_threshold}]}},
                                {?__(146, "Pixelwidth"),
                                    {text, get_pref(aa_pixelwidth,Attr),[range(aa_pixelwidth),{key,aa_pixelwidth}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{margin,false}]}
                ],[{title, ?__(147, "Anti-Aliasing")},{margin,false}]}%,

                % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
                %{hframe, [
                %{button, ?__(55, "Save"), save, [{info, ?__(56, "Save to user preferences")},{hook,ButtonsHook}]},
                %{button, ?__(57, "Load"), load, [{info, ?__(58, "Load from user preferences")},{hook,ButtonsHook}]},
                %{button, ?__(59, "Reset"), reset, [{info, ?__(60, "Reset to default values")},{hook,ButtonsHook}]}
                %]}
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
                            {?__(202, "Photon Mapping - Global Illumination"), photonmapping},
                            {?__(203, "Path Tracing - Global Illumination"), pathtracing},
                            {?__(204, "Bidirectional Path Tracing - Global Illumination"), bidirectional},
                            {?__(205, "SPPM - Global Illumination"),sppm}
                        ], get_pref(lighting_method,Attr), [{key,lighting_method}, {hook,Hook_Show}]}
                    ]},
                    %% Start Direct Lighting Menu Section
                    {hframe,[
                        %% 1st collunm of panels
                        {vframe, [
                            {hframe,[
                                {?__(206, "Caustics"), get_pref(use_caustics,Attr), [{key,use_caustics},{hook,Hook_Enable},{show,false}]}
                            ]},
                            {label_column, [
                                {?__(207, "Photons"),{text, get_pref(caustic_photons,Attr), [range(caustic_photons),{key,caustic_photons}]}},
                                {?__(208, "Depth"),{text, get_pref(caustic_depth,Attr), [range(caustic_depth),{key,caustic_depth}]}},
                                {?__(209, "Mix"),{text, get_pref(caustic_mix,Attr), [range(caustic_mix),{key,caustic_mix}]}},
                                {?__(210, "Radius"),{text, get_pref(caustic_radius,Attr), [range(caustic_radius),{key,caustic_radius}]}}
                            ], [key(pnl_dl1)]}
                        ], [key(pnl_caustics),{show,false},{magin,false}]},
                        {label_column, [
                            {?__( 211, "Photons"),{text, get_pref(pm_diffuse_photons,Attr), [range(pm_diffuse_photons),{key,pm_diffuse_photons}]}},
                            {?__(212, "Bounces"),{text, get_pref(pm_bounces,Attr), [range(pm_bounces),{key,pm_bounces}]}},
                            {?__(213, "Search"),{text, get_pref(pm_search,Attr), [range(pm_search),{key,pm_search}]}},
                            {?__(214, "Diffuse Radius"),{text, get_pref(pm_diffuse_radius,Attr), [range(pm_diffuse_radius),{key,pm_diffuse_radius}]}}
                        ], [key(pnl_pm1)]},
                        {label_column, [
                            {?__( 215, "Photons"),{text, get_pref(pt_diffuse_photons,Attr), [range(pt_diffuse_photons),{key,pt_diffuse_photons}]}},
                            {?__(216, "Bounces"),{text, get_pref(pt_bounces,Attr), [range(pt_bounces),{key,pt_bounces}]}}
                        ], [key(pnl_pt1),{show,false}]},
                        {label_column, [
                            {?__( 217, "Photons"),{text,get_pref(sppm_photons,Attr),[range(sppm_photons),{key,sppm_photons}]}},
                            {?__(218, "Bounces"),{text,get_pref(sppm_bounces,Attr),[range(sppm_bounces),{key,sppm_bounces}]}},
                            {?__(219, "Search"),{text,get_pref(sppm_search,Attr),[range(sppm_search),{key,sppm_search}]}},
                            {?__( 220, "Radius"),{text,get_pref(sppm_radius,Attr),[range(sppm_radius),{key,sppm_radius}]}}
                        ], [key(pnl_sppm1),{show,false}]},

                        %% 2nd collumn of panels
%                        {vframe, [
                            {vframe, [
                                {hframe, [
                                    {?__(95, "Ambient Occlusion"), get_pref(do_ao,Attr), [{key,do_ao},{hook,Hook_Enable}]}
                                ]},
                                {label_column, [
                                    {?__(97, "AO Distance"),{text, get_pref(ao_distance,Attr), [range(ao_distance),{key,ao_distance}]}},
                                    {?__(98, "AO Samples"),{text, get_pref(ao_samples,Attr), [range(ao_samples),{key,ao_samples}]}},
                                    {?__(99, "AO Color"),{color, get_pref(ao_color,Attr), [{key,ao_color}]}}
                                ], [key(pnl_use_ao)]}
                            ], [key(pnl_ao),{show,false},{magin,false}]},
                            {label_column, [
                                {?__(225, "Caustic Photons"),{text, get_pref(pm_caustic_photons,Attr), [range(pm_caustic_photons),{key,pm_caustic_photons}]}},
                                {?__(226, "Caustic Radius"),{text, get_pref(pm_caustic_radius,Attr), [range(pm_caustic_radius),{key,pm_caustic_radius}]}},
                                {?__(227, "Caustic Mix"),{text, get_pref(pm_caustic_mix,Attr), [range(pm_caustic_mix),{key,pm_caustic_mix}]}}
                            ],[key(pnl_pm2)]},
                            {vframe,[
                                {hframe,[
                                    {label, ?__(245, "Caustic Type")++" "},
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
                            ],[key(pnl_pt2),{show,false}]},
                            {vframe,[
                                {label_column, [
                                    {?__(260, "Times"),{text,get_pref(sppm_times,Attr),[{key,sppm_times},range(sppm_times)]}},
                                    {?__(261, "Passes"),{text,get_pref(sppm_passes,Attr),[{key,sppm_passes}, range(sppm_passes)]}}
                                ]},
                                {hframe,[
                                    {?__(262,"IRE"),get_pref(sppm_ire,Attr),[{key,sppm_ire}]}
                                ]}
                            ],[key(pnl_sppm2),{show,false},{margin,false}]},
%                        ], [{margin,false}]},

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
                                    ],[{margin,false}]},
                                    {vframe,[
                                        {?__(266, "Show Map"), get_pref(pm_fg_show_map,Attr), [{key,pm_fg_show_map}]}
                                    ],[{border,1}]}
                                ], [key(pnl_use_fg),[{margin,false}]]}
                            ], [key(pnl_pm3)]},
                            {label_column, [
                                {?__(267, "Path Samples"),{text, get_pref(pt_samples,Attr), [{key,pt_samples},range(pt_samples)]}}
                            ], [key(pnl_pt3)]}
                        ], [{margin,false}]}
                    ],[key(pnl_light)]},

                    {vframe, [
                        {vframe, [
                            {hframe,[
                                {?__(268, "Enabled"), get_pref(use_sss,Attr), [{key,use_sss},{hook,Hook_Enable}]}
                            ]},
                            {hframe, [
                                {label_column, [
                                    {?__(269, "Photons"),{text, get_pref(sss_photons,Attr), [{key,sss_photons},range(sss_photons)]}},
                                    {?__(270, "Depth"),{text, get_pref(sss_depth,Attr), [{key,sss_depth},range(sss_depth)]}}
                                ]},
                                {label_column, [
                                    {?__(271, "Scale"),{text, get_pref(sss_scale,Attr), [{key,sss_scale},range(sss_scale)]}},
                                    {?__(272, "SingleScatter Samples"),{text, get_pref(sss_scatter_samples,Attr), [{key,sss_scatter_samples},range(sss_scatter_samples)]}}
                                ]}
                            ],[key(pnl_sss_opt), {margin,false}]}
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
                        {?__(282, "Adaptive"), get_pref(volintegr_adaptive,Attr), [{key,volintegr_adaptive}]},
                        panel,
                        {label, ?__(283, "StepSize")},
                        panel,
                        {text, get_pref(volintegr_stepsize,Attr), [{key,volintegr_stepsize},range(volintegr_stepsize)]},
                        panel,
                        {?__(284, "Optimize"), get_pref(volintegr_optimize,Attr),[{key,volintegr_optimize}]}
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
                            {text, get_pref(width,Attr), [range(pixels),{key,width}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(306, "Height ")},
                            {text, get_pref(height,Attr), [range(pixels),{key,height}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(307, "Scale ")},
                            {text, get_pref(lens_ortho_scale,Attr), [range(lens_ortho_scale),{key,lens_ortho_scale}]}
                        ],[key(pnl_lens_scale)]}
                    ],[{margin,false}]},
                    {vframe, [
                        {hframe, [
                            {?__(308, "Circular"), get_pref(lens_angular_circular,Attr),
                                        [{key,lens_angular_circular},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(309, "Circle/Max Angle"), {text, get_pref(lens_angular_max_angle,Attr),
                                            [range(lens_angular_max_angle),{key,lens_angular_max_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]},
                        {hframe, [
                            {?__(310, "Mirrored"), get_pref(lens_angular_mirrored,Attr),
                                        [{key,lens_angular_mirrored},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(311, "Frame/Angle"), {text, get_pref(lens_angular_angle,Attr),
                                            [range(lens_angular_angle),{key,lens_angular_angle}]}}
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
                            ApertureIdx, [{key,aperture_idx},{hook,Hook_Enable}]},
                        panel,
                        {?__(314, "Use QMC"), get_pref(bokeh_use_QMC,Attr),[{key,bokeh_use_QMC}]}
                    ],[{margin,false}]},

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
                    {vframe, [
                        {hframe, [
                            {label, ?__(327, "DOF Rotation ")},
                            {slider, {text, get_pref(bokeh_rotation,Attr), [range(bokeh_rotation),{key,bokeh_rotation}]}}
                        ]},
                        {hframe, [
                            {label, ?__(328, "DOF Distance ")},
                            {slider, {text, get_pref(dof_distance,Attr), [range(dof_distance),{key,dof_distance}]}}
                        ]}
                    ],[key(pnl_dof_sliders),{margin,false}]}
                ],[key(pnl_camera)]}
            ],[{title,""}]}
        },
    % world--

    [
        {oframe, [
            GeneralOpt,
            Lighting,
            Camera,
            World
        ], 1, [{style, buttons}]}
    ].

%% Used to lookup in Store for combinations of values.
is_member(Value, Members) ->
    lists:member(Value,Members).
