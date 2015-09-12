%
%
%
%
export_dialog(Op, Title) ->
    wpa:dialog(true, Title,
               export_dialog_qs(Op, get_prefs(export_prefs())),
                fun(Attr) -> {file,{Op,{?TAG,Attr}}} end).

%% Export Render Options Dialog Settings
export_prefs() ->
    [{subdivisions,?DEF_SUBDIVISIONS},
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
        {sss_singlescatter_samples,?DEF_SSS_SINGLESCATTER_SAMPLES},
        {raydepth,?DEF_RAYDEPTH},
        {gamma,?DEF_GAMMA},
        {bias,?DEF_BIAS},
        {exposure,?DEF_EXPOSURE},
        {transparent_shadows,?DEF_TRANSPARENT_SHADOWS},
        {shadow_depth,?DEF_SHADOW_DEPTH},
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
        {aa_filter_type,?DEF_AA_FILTER_TYPE},
        {background_color,?DEF_BACKGROUND_COLOR},
        {save_alpha,?DEF_SAVE_ALPHA},
        {background_transp_refract,?DEF_BACKGROUND_TRANSP_REFRACT},
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
        %{aperture,?DEF_APERTURE},% povman: double definition?
        {bokeh_bias,?DEF_BOKEH_BIAS},
        {bokeh_rotation,?DEF_BOKEH_ROTATION},
        {dof_distance,?DEF_DOF_DISTANCE}].

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
        || {F, A} <- [{"1.0", 1 / 1},
                        {"1.4", 1 / 2},
                        {"2", 1 / 4},
                        {"2.8", 1 / 8},
                        {"4", 1 / 16},
                        {"5.6", 1 / 32},
                        {"8", 1 / 64},
                        {"11", 1 / 128},
                        {"16", 1 / 256},
                        {"22", 1 / 512},
                        {"32", 1 / 1024},
                        {?__(47, "pinhole"), 0.0}]],
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

    GeneralOpt =
        {?__(0, "General options"),
            {vframe, [
                %% Pre-Render group
                {vframe, [
                    {hframe, [
                        {label, ?__(1, "Subdivisions")++" "},
                        {slider, {text, get_pref(subdivisions,Attr),[{key,subdivisions}, range(subdivisions)]}}
                    ]},
                    {hframe, [
                        case Op of
                            render ->
                                {hframe,[
                                    {?__(2, "Write .xml file"), get_pref(keep_xml,Attr),[{key,keep_xml}]},
                                    panel
                                ]};
                            _ ->
                                {value, get_pref(keep_xml,Attr), [{key,keep_xml}]}
                        end,
                        {hframe, [
                            {label, "Threads"++" "},
                            {text, get_pref(threads_number,Attr), [range(threads_number),{key,threads_number}]}
                        ],[key(pnl_threads)]},
                        {hframe,[
                            {?__(160, "Auto"), get_pref(threads_auto,Attr),
                                                    [{key,threads_auto},{hook,Hook_Enable}]}
                        ]}
                    ],[{margin,false}]}
                ],[{title, ?__(3, "Pre-rendering")},{margin,false}]},

                %% Render group
                {hframe, [
                    {label_column, [
                        {?__(4, "Raydepth"),{text, get_pref(raydepth,Attr), [range(raydepth),{key,raydepth}]}},
                        {?__(5, "Gamma"),{text, get_pref(gamma,Attr), [range(gamma),{key,gamma}]}}
                    ]},
                    {label_column, [
                        {?__(6, "Bias"),{text, get_pref(bias,Attr), [range(bias),{key,bias}]}},
                        {?__(7, "Exposure"),{text, get_pref(exposure,Attr), [range(exposure),{key,exposure}]}}
                    ]},
                    {vframe, [
                        {vframe, [
                            {menu, [
                                {?__(133, "Transp Shadows Off"), false},
                                {?__(134, "Transp Shadows On"), true}
                            ], get_pref(transparent_shadows,Attr), [{key,transparent_shadows},{hook,Hook_Enable}]}
                        ]},
                        {hframe, [
                            {label, ?__(135, "Depth")},
                            {text, get_pref(shadow_depth,Attr), [range(shadow_depth),{key,shadow_depth}]}
                        ], [key(pnl_transp_shadow), {enabled,false},{margin,false}]}
                    ]}
                ],[{title, ?__(8, "Render")},{margin,false}]},

                %% Output group
                {hframe, [
                    {hframe, [
                        {label, ?__(13, "Output")++" "},
                        {menu, [
                            {Ext ++ " (" ++ Desc ++ ")", Format}
                            || {Format, Ext, Desc} <- wings_job:render_formats(),
                            (Format == tga) or (Format == tif) or (Format == png) or
                            (Format == hdr) or (Format == exr)
                        ], get_pref(render_format,Attr), [{key,render_format},{hook,Hook_Enable}]}
                    ]},
                    panel,
                    {hframe, [
                        {?__(9, "Float"), get_pref(exr_flag_float,Attr), [{key,exr_flag_float}]},
                        panel,
                        {?__(10, "Zbuf"), get_pref(exr_flag_zbuf,Attr), [{key,exr_flag_zbuf}]},
                        panel,
                        {label, ?__(11, "Compression")++" "},
                        {menu, [
                            {?__(12, "none"), compression_none},
                            {"piz", compression_piz},
                            {"rle", compression_rle},
                            {"pxr24", compression_pxr24},
                            {"zip", compression_zip}
                        ], get_pref(exr_flag_compression,Attr), [{key,exr_flag_compression}]}
                    ], [key(pnl_exr_option),{enabled,false},{margin,false}]}
                ],[{margin,false}]},

                %% Antialising group
                {hframe, [
                    {vframe, [
                        {hframe, [
                            {label, ?__(165, "Method")++" "},
                            {menu, [
                                {?__(136, "Box Filter"), box},
                                {?__(137, "Gaussian Filter"), gauss},
                                {?__(138, "Mitchell-Netravali Filter"), mitchell},
                                {?__(139, "Lanczos Filter"), lanczos}
                            ], get_pref(aa_filter_type,Attr), [{key,aa_filter_type}]},
                            panel,
                            {hframe, [
                                {?__(16, "Jitter First"), get_pref(aa_jitterfirst,Attr),[{key,aa_jitterfirst}]}
                            ]},
                            panel,
                            {hframe, [
                                {?__(19, "Clamp RGB"), get_pref(clamp_rgb,Attr),[{key,clamp_rgb}]}
                            ]}
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(14, "Passes"),{text, get_pref(aa_passes,Attr),
                                                        [range(aa_passes),{key,aa_passes}]}},
                                {?__(15, "Min Samples"),{text, get_pref(aa_minsamples,Attr),
                                                        [range(aa_minsamples),{key,aa_minsamples}]}}
                            ]},
                            {label_column, [
                                {?__(17, "Threshold"),{text, get_pref(aa_threshold,Attr),
                                    [range(aa_threshold),{key,aa_threshold}]}},
                                {?__(18, "Pixelwidth"),{text, get_pref(aa_pixelwidth,Attr),
                                    [range(aa_pixelwidth),{key,aa_pixelwidth}]}}
                            ]}
                        ],[{margin,false}]}
                    ],[{margin,false}]}
                ],[{title, ?__(20, "Anti-Aliasing")},{margin,false}]},

                {hframe, [
                    {label, ?__(21, "Default Color")++" "},
                    {color, get_pref(background_color,Attr), [{key,background_color}]},
                    panel,
                    {label, ?__(22, "Alpha Channel")++" "},
                    {menu, [
                        {?__(23, "Off"), false},
                        {?__(61, "On"), true},
                        {?__(24, "Premultiply"), premultiply},
                        {?__(25, "Backgroundmask"), backgroundmask}
                    ], get_pref(save_alpha,Attr), [{key,save_alpha}]},
                    panel,
                    {?__(159, "Transp Refraction"),get_pref(background_transp_refract,Attr),
                                                    [{key,background_transp_refract}]}
                ],[{title, ?__(26, "Background")},{margin,false}]}

%%                 % TO DO: we need changes to wings_dialog code in order to enable this kind of use for buttons
%%                 {hframe, [
%%                     {button, ?__(55, "Save"), save, [{info, ?__(56, "Save to user preferences")},{hook,ButtonsHook}]},
%%                     {button, ?__(57, "Load"), load, [{info, ?__(58, "Load from user preferences")},{hook,ButtonsHook}]},
%%                     {button, ?__(59, "Reset"), reset, [{info, ?__(60, "Reset to default values")},{hook,ButtonsHook}]}
%%                 ]}
            ]}
        },

    %% Lighting group
    Lighting =
        {?__(113, "Lighting"),
            {vframe, [
                {vframe, [
                    {hframe,[
                        {menu, [
                            {?__(114, "Direct Light"), directlighting},
                            {?__(115, "Photon Mapping - Global Illumination"), photonmapping},
                            {?__(140, "Path Tracing - Global Illumination"), pathtracing},
                            {?__(116, "Bidirectional Path Tracing - Global Illumination"), bidirectional},
                            {?__(157, "SPPM - Global Illumination"),sppm}
                        ], get_pref(lighting_method,Attr), [{key,lighting_method}, {hook,Hook_Show}]}
                    ]},
                    %% Start Direct Lighting Menu Section
                    {hframe,[
                        %% 1st collunm of panels
                        {vframe, [
                            {hframe,[
                                {?__(82, "Caustics"), get_pref(use_caustics,Attr), [{key,use_caustics},{hook,Hook_Enable},{show,false}]}
                            ]},
                            {label_column, [
                                {?__(84, "Photons"),{text, get_pref(caustic_photons,Attr), [range(caustic_photons),{key,caustic_photons}]}},
                                {?__(85, "Depth"),{text, get_pref(caustic_depth,Attr), [range(caustic_depth),{key,caustic_depth}]}},
                                {?__(86, "Mix"),{text, get_pref(caustic_mix,Attr), [range(caustic_mix),{key,caustic_mix}]}},
                                {?__(87, "Radius"),{text, get_pref(caustic_radius,Attr), [range(caustic_radius),{key,caustic_radius}]}}
                            ], [key(pnl_dl1)]}
                        ], [key(pnl_caustics),{show,false},{magin,false}]},
                        {label_column, [
                            {?__( 84, "Photons"),{text, get_pref(pm_diffuse_photons,Attr), [range(pm_diffuse_photons),{key,pm_diffuse_photons}]}},
                            {?__(122, "Bounces"),{text, get_pref(pm_bounces,Attr), [range(pm_bounces),{key,pm_bounces}]}},
                            {?__(123, "Search"),{text, get_pref(pm_search,Attr), [range(pm_search),{key,pm_search}]}},
                            {?__(124, "Diffuse Radius"),{text, get_pref(pm_diffuse_radius,Attr), [range(pm_diffuse_radius),{key,pm_diffuse_radius}]}}
                        ], [key(pnl_pm1)]},
                        {label_column, [
                            {?__( 84, "Photons"),{text, get_pref(pt_diffuse_photons,Attr), [range(pt_diffuse_photons),{key,pt_diffuse_photons}]}},
                            {?__(122, "Bounces"),{text, get_pref(pt_bounces,Attr), [range(pt_bounces),{key,pt_bounces}]}}
                        ], [key(pnl_pt1),{show,false}]},
                        {label_column, [
                            {?__( 84, "Photons"),{text,get_pref(sppm_photons,Attr),[range(sppm_photons),{key,sppm_photons}]}},
                            {?__(122, "Bounces"),{text,get_pref(sppm_bounces,Attr),[range(sppm_bounces),{key,sppm_bounces}]}},
                            {?__(123, "Search"),{text,get_pref(sppm_search,Attr),[range(sppm_search),{key,sppm_search}]}},
                            {?__( 87, "Radius"),{text,get_pref(sppm_radius,Attr),[range(sppm_radius),{key,sppm_radius}]}}
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
                                {?__(125, "Caustic Photons"),{text, get_pref(pm_caustic_photons,Attr), [range(pm_caustic_photons),{key,pm_caustic_photons}]}},
                                {?__(126, "Caustic Radius"),{text, get_pref(pm_caustic_radius,Attr), [range(pm_caustic_radius),{key,pm_caustic_radius}]}},
                                {?__(127, "Caustic Mix"),{text, get_pref(pm_caustic_mix,Attr), [range(pm_caustic_mix),{key,pm_caustic_mix}]}}
                            ],[key(pnl_pm2)]},
                            {vframe,[
                                {hframe,[
                                    {label, ?__(145, "Caustic Type")++" "},
                                    {menu, [
                                        {?__(153, "path"), path},
                                        {?__(154, "photons"), photons},
                                        {?__(155, "both"), both},
                                        {?__(156, "none"), none}
                                    ], get_pref(pt_caustic_type,Attr), [{key,pt_caustic_type}]}
                                ]},
                                {label_column, [
                                    {?__(126, "Caustic Radius"),{text, get_pref(pt_caustic_radius,Attr), [{key,pt_caustic_radius},range(pt_caustic_radius)]}},
                                    {?__(127, "Caustic Mix"),{text, get_pref(pt_caustic_mix,Attr), [{key,pt_caustic_mix},range(pt_caustic_mix)]}},
                                    {?__(148, "Caustic Depth"),{text, get_pref(pt_caustic_depth,Attr), [{key,pt_caustic_depth},range(pt_caustic_depth)]}}
                                ],[{margin,false}]}
                            ],[key(pnl_pt2),{show,false}]},
                            {vframe,[
                                {label_column, [
                                    {?__(146, "Times"),{text,get_pref(sppm_times,Attr),[{key,sppm_times},range(sppm_times)]}},
                                    {?__(147, "Passes"),{text,get_pref(sppm_passes,Attr),[{key,sppm_passes}, range(sppm_passes)]}}
                                ]},
                                {hframe,[
                                    {?__(166,"IRE"),get_pref(sppm_ire,Attr),[{key,sppm_ire}]}
                                ]}
                            ],[key(pnl_sppm2),{show,false},{margin,false}]},
%                        ], [{margin,false}]},

                        %% 3rd collumn of panels
                        {vframe, [
                            {vframe, [
                                {vframe,[
                                    {?__(128, "Final Gather"), get_pref(pm_use_fg,Attr), [{key,pm_use_fg},{hook,Hook_Enable}]}
                                ],[{border,1}]},
                                {vframe, [
                                    {label_column, [
                                        {?__(130, "FG Bounces"),{text, get_pref(pm_fg_bounces,Attr), [{key,pm_fg_bounces},range(pm_fg_bounces)]}},
                                        {?__(131, "FG Samples"),{text, get_pref(pm_fg_samples,Attr), [{key,pm_fg_samples},range(pm_fg_samples)]}}
                                    ],[{margin,false}]},
                                    {vframe,[
                                        {?__(132, "Show Map"), get_pref(pm_fg_show_map,Attr), [{key,pm_fg_show_map}]}
                                    ],[{border,1}]}
                                ], [key(pnl_use_fg),[{margin,false}]]}
                            ], [key(pnl_pm3)]},
                            {label_column, [
                                {?__(152, "Path Samples"),{text, get_pref(pt_samples,Attr), [{key,pt_samples},range(pt_samples)]}}
                            ], [key(pnl_pt3)]}
                        ], [{margin,false}]}
                    ],[key(pnl_light)]},

                    {vframe, [
                        {vframe, [
                            {hframe,[
                                {?__(76, "Enabled"), get_pref(use_sss,Attr), [{key,use_sss},{hook,Hook_Enable}]}
                            ]},
                            {hframe, [
                                {label_column, [
                                    {?__(77, "Photons"),{text, get_pref(sss_photons,Attr), [{key,sss_photons},range(sss_photons)]}},
                                    {?__(78, "Depth"),{text, get_pref(sss_depth,Attr), [{key,sss_depth},range(sss_depth)]}}
                                ]},
                                {label_column, [
                                    {?__(79, "Scale"),{text, get_pref(sss_scale,Attr), [{key,sss_scale},range(sss_scale)]}},
                                    {?__(80, "SingleScatter Samples"),{text, get_pref(sss_singlescatter_samples,Attr), [{key,sss_singlescatter_samples},range(sss_singlescatter_samples)]}}
                                ]}
                            ],[key(pnl_sss_opt), {margin,false}]}
                        ],[{title, ?__(74, "SubSurface Scattering")},{margin,false}]}
                    ],[key(pnl_sss)]}
                ],[{title, ""}]},

                %% Volumetrics group
                {hframe, [
                    {menu, [
                        {?__(89, "None"), none},
                        {?__(90, "SingleScatter"), singlescatterintegrator}
                    ], get_pref(volintegr_type,Attr), [{key,volintegr_type},{hook,Hook_Show}]},
                    panel,
                    {hframe, [
                        {?__(91, "Adaptive"), get_pref(volintegr_adaptive,Attr), [{key,volintegr_adaptive}]},
                        panel,
                        {label, ?__(93, "StepSize")},
                        panel,
                        {text, get_pref(volintegr_stepsize,Attr), [{key,volintegr_stepsize},range(volintegr_stepsize)]},
                        panel,
                        {?__(92, "Optimize"), get_pref(volintegr_optimize,Attr),[{key,volintegr_optimize}]}
                    ], [key(pnl_volumetric)]}
                ],[{title, ?__(88, "Volumetrics")}]}
            ],[{margin,false}]}
        },


    Camera =
        {?__(51, "Camera"),
            {vframe, [
                {vframe, [
                    {menu, [
                        {?__(102, "Perspective"), perspective},
                        {?__(103, "Orthographic"), orthographic},
                        {?__(104, "Architect"), architect},
                        {?__(105, "Fish Eye (Angular)"), angular}
                    ], get_pref(lens_type,Attr), [{key,lens_type},{hook,Hook_Show}]},
                    {hframe, [
                        {hframe, [
                            {label, ?__(33, "Width")++" "},
                            {text, get_pref(width,Attr), [range(pixels),{key,width}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(44, "Height")++" "},
                            {text, get_pref(height,Attr), [range(pixels),{key,height}]}
                        ]},
                        panel,
                        {hframe, [
                            {label, ?__(108, "Scale")++" "},
                            {text, get_pref(lens_ortho_scale,Attr), [range(lens_ortho_scale),{key,lens_ortho_scale}]}
                        ],[key(pnl_lens_scale)]}
                    ],[{margin,false}]},
                    {vframe, [
                        {hframe, [
                            {?__(109, "Circular"), get_pref(lens_angular_circular,Attr),
                                        [{key,lens_angular_circular},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(111, "Circle/Max Angle"), {text, get_pref(lens_angular_max_angle,Attr),
                                            [range(lens_angular_max_angle),{key,lens_angular_max_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]},
                        {hframe, [
                            {?__(110, "Mirrored"), get_pref(lens_angular_mirrored,Attr),
                                        [{key,lens_angular_mirrored},{hook,Hook_Enable}]},
                            panel,
                            {label_column, [
                                {?__(112, "Frame/Angle"), {text, get_pref(lens_angular_angle,Attr),
                                            [range(lens_angular_angle),{key,lens_angular_angle}]}}
                            ],[{margin,false}]}
                        ],[{margin,false}]}
                    ],[key(pnl_lens_angle),{margin,false}]},

                    {hframe, [
                        {hframe, [
                            {label, ?__(34, "Aperture")++" "},
                            {text, Aperture, [range(aperture),{key,aperture},{hook,Hook_Enable}]}
                        ]},
                        panel,
                        {label, ?__(45, "f-stop")++" "},
                        {menu, [{F,A} || {F,A,_} <- ApertureList]++[{"Custom",Custom}],
                            ApertureIdx, [{key,aperture_idx},{hook,Hook_Enable}]},
                        panel,
                        {?__(32, "Use QMC"), get_pref(bokeh_use_QMC,Attr),[{key,bokeh_use_QMC}]}
                    ],[{margin,false}]},

                    {hframe, [
                        {label, ?__(35, "DOF Type")++" "},
                        {menu, [
                            {?__(37, "Disk1"), disk1}, {?__(38, "Disk2"), disk2},
                            {?__(39, "Triangle"), triangle},
                            {?__(40, "Square"), square}, {?__(41, "Pentagon"), pentagon},
                            {?__(42, "Hexagon"), hexagon}, {?__(43, "Ring"), ring}
                        ], get_pref(bokeh_type,Attr), [{key,bokeh_type}]},
                        panel,
                        {label, ?__(46, "Bias")++" "},
                        {menu, [
                            {?__(48, "Uniform"), uniform},
                            {?__(49, "Center"), center},
                            {?__(50, "Edge"), edge}
                        ], get_pref(bokeh_bias,Attr), [{key,bokeh_bias}]}
                    ],[key(pnl_dof_type)]},
                    {vframe, [
                        {hframe, [
                            {label, ?__(36, "DOF Rotation")++" "},
                            {slider, {text, get_pref(bokeh_rotation,Attr), [range(bokeh_rotation),{key,bokeh_rotation}]}}
                        ]},
                        {hframe, [
                            {label, ?__(100, "DOF Distance")++" "},
                            {slider, {text, get_pref(dof_distance,Attr), [range(dof_distance),{key,dof_distance}]}}
                        ]}
                    ],[key(pnl_dof_sliders),{margin,false}]}
                ],[key(pnl_camera)]}
            ],[{title,""}]}
        },

    [
        {oframe, [
            GeneralOpt,
            Lighting,
            Camera
        ], 1, [{style, buttons}]}
    ].
	
%% Used to lookup in Store for combinations of values.
is_member(Value, Members) ->
    lists:member(Value,Members).
