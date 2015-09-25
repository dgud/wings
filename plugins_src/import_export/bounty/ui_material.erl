%%% Object Specific Material Properties
%%%
material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    DefReflected = alpha(proplists:get_value(specular, OpenGL)),
    DefTransmitted = def_transmitted(proplists:get_value(diffuse, OpenGL)),
    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),
    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),
    YafaRay = proplists:get_value(?TAG, Mat, []),
	%MatAttr = proplists:get_value(?TAG, Mat, []),
    %DefShaderType = get_pref(shader_type, YafaRay),
	%DefMatType = get_pref(shader_type, YafaRay),
    MaterialType = proplists:get_value(material_type, YafaRay, ?DEF_MATERIAL_TYPE),
    Object_Type = proplists:get_value(object_type, YafaRay, ?DEF_OBJECT_TYPE),
    Volume_Type = proplists:get_value(volume_type, YafaRay, ?DEF_VOLUME_TYPE),
    Volume_Sigma_a = proplists:get_value(volume_sigma_a, YafaRay, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, YafaRay, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, YafaRay, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, YafaRay, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, YafaRay, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Sharpness = proplists:get_value(volume_sharpness, YafaRay, ?DEF_VOLUME_SHARPNESS),
    Volume_Cover = proplists:get_value(volume_cover, YafaRay, ?DEF_VOLUME_COVER),
    Volume_Density = proplists:get_value(volume_density, YafaRay, ?DEF_VOLUME_DENSITY),
    Volume_Minmax_X = proplists:get_value(volume_minmax_x, YafaRay, ?DEF_VOLUME_MINMAX_X),
    Volume_Minmax_Y = proplists:get_value(volume_minmax_y, YafaRay, ?DEF_VOLUME_MINMAX_Y),
    Volume_Minmax_Z = proplists:get_value(volume_minmax_z, YafaRay, ?DEF_VOLUME_MINMAX_Z),
    Lightportal_Power = proplists:get_value(lightportal_power, YafaRay, ?DEF_LIGHTPORTAL_POWER),
    Lightportal_Samples = proplists:get_value(lightportal_samples, YafaRay, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Diffusephotons = proplists:get_value(lightportal_diffusephotons, YafaRay, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, YafaRay, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, YafaRay, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    Meshlight_Power = proplists:get_value(meshlight_power, YafaRay, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, YafaRay, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, YafaRay, ?DEF_MESHLIGHT_COLOR),
    Meshlight_Double_Sided = proplists:get_value(meshlight_double_sided, YafaRay, ?DEF_MESHLIGHT_DOUBLE_SIDED),
    TIR = proplists:get_value(tir, YafaRay, ?DEF_TIR),
    AutosmoothAngle = proplists:get_value(autosmooth_angle, YafaRay, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, YafaRay,
                                     if AutosmoothAngle == 0.0 -> false;
                                        true -> ?DEF_AUTOSMOOTH end),

    %% IOR Material Property for all Materials except Glossy
    %%
    IOR = proplists:get_value(ior, YafaRay, ?DEF_IOR),

    %% Color Properties Transmitted = Diffuse and Refracted
    %% Color Properties Reflected = Glossy and Reflected
    %%
    Reflected = proplists:get_value(reflected, YafaRay, DefReflected),
    Transmitted = proplists:get_value(transmitted, YafaRay, DefTransmitted),

    %% Glass Properties
    %%
    AbsorptionColor = proplists:get_value(absorption_color, YafaRay, DefAbsorptionColor),
    AbsorptionDist = proplists:get_value(absorption_dist, YafaRay, ?DEF_ABSORPTION_DIST),
    DispersionPower = proplists:get_value(dispersion_power, YafaRay, ?DEF_DISPERSION_POWER),
    DispersionSamples = proplists:get_value(dispersion_samples, YafaRay, ?DEF_DISPERSION_SAMPLES),
    FakeShadows = proplists:get_value(fake_shadows, YafaRay, ?DEF_FAKE_SHADOWS),
    Roughness = proplists:get_value(roughness, YafaRay, ?DEF_ROUGHNESS),
    Glass_IR_Depth = proplists:get_value(glass_ir_depth, YafaRay, ?DEF_GLASS_IR_DEPTH),

    %% Shiny Diffuse Properties
    %% Transmit Filter also for Glass and Rough Glass
    Transparency = proplists:get_value(transparency, YafaRay, ?DEF_TRANSPARENCY),
    TransmitFilter = proplists:get_value(transmit_filter, YafaRay, ?DEF_TRANSMIT_FILTER),
    Translucency = proplists:get_value(translucency, YafaRay, ?DEF_TRANSLUCENCY),
    SpecularReflect = proplists:get_value(specular_reflect, YafaRay, ?DEF_SPECULAR_REFLECT),
    Emit = proplists:get_value(emit, YafaRay, ?DEF_EMIT),

    %% Translucency (SSS) Properties
    %%
    SSS_AbsorptionColor = proplists:get_value(sss_absorption_color, YafaRay, ?DEF_SSS_ABSORPTION_COLOR),
    ScatterColor = proplists:get_value(scatter_color, YafaRay, ?DEF_SCATTER_COLOR),
    SigmaSfactor = proplists:get_value(sigmas_factor, YafaRay, ?DEF_SIGMAS_FACTOR),
    SSS_Translucency = proplists:get_value(sss_translucency, YafaRay, ?DEF_SSS_TRANSLUCENCY),
    SSS_Specular_Color = proplists:get_value(sss_specular_color, YafaRay, ?DEF_SSS_SPECULAR_COLOR),

    %% Shiny Diffuse, Glossy, Coated Glossy Properties
    %%
    DiffuseReflect = proplists:get_value(diffuse_reflect, YafaRay, ?DEF_DIFFUSE_REFLECT),
    OrenNayar = proplists:get_value(oren_nayar, YafaRay, ?DEF_OREN_NAYAR),
    OrenNayar_Sigma = proplists:get_value(sigma, YafaRay, ?DEF_OREN_NAYAR_SIGMA),

    %% Glossy and Coated Glossy Properties
    %%
    GlossyReflect = proplists:get_value(glossy_reflect, YafaRay, ?DEF_GLOSSY_REFLECT),
    Exponent = proplists:get_value(exponent, YafaRay, ?DEF_EXPONENT),
    Anisotropic = proplists:get_value(anisotropic, YafaRay, ?DEF_ANISOTROPIC),
    Anisotropic_U = proplists:get_value(anisotropic_u, YafaRay, ?DEF_ANISOTROPIC_U),
    Anisotropic_V = proplists:get_value(anisotropic_v, YafaRay, ?DEF_ANISOTROPIC_V),

    %% Light Material Properties
    %%
    Lightmat_Color = proplists:get_value(lightmat_color, YafaRay, DefLightmatColor),
    Lightmat_Power = proplists:get_value(lightmat_power, YafaRay, ?DEF_LIGHTMAT_POWER),

    %% Blend Material Properties
    %%
    Blend_Mat1 = proplists:get_value(blend_mat1, YafaRay, ?DEF_BLEND_MAT1),
    Blend_Mat2 = proplists:get_value(blend_mat2, YafaRay, ?DEF_BLEND_MAT2),
    Blend_Value = proplists:get_value(blend_value, YafaRay, ?DEF_BLEND_VALUE),

    Hook_Enable = fun(Key, Value, Store) ->
		case Key of
            ?KEY(oren_nayar) ->
                wings_dialog:enable(?KEY(pnl_sigma_shiny), Value =/= ?DEF_OREN_NAYAR, Store);
            ?KEY(autosmooth) ->
                wings_dialog:enable(?KEY(pnl_autosmooth), Value =/= ?DEF_OREN_NAYAR, Store);
            ?KEY(anisotropic) ->
                wings_dialog:enable(?KEY(pnl_exp_coated), Value =/= ?DEF_ANISOTROPIC, Store);
            ?KEY(dispersion_power) ->
                wings_dialog:enable(?KEY(pnl_dsp_sam), Value > 0.0, Store)
		end
    end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(object_type) ->
                    Value0 = wings_dialog:get_value(?KEY(volume_type),Store),
                    wings_dialog:show(?KEY(pnl_desnity_volume), Value0 =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value0 =:= noisevolume, Store),

                    wings_dialog:show(?KEY(pnl_volume), Value =:= volume, Store),
                    wings_dialog:show(?KEY(pnl_mesh_light), Value =:= meshlight, Store),
                    wings_dialog:show(?KEY(pnl_lightportal), Value =:= lightportal, Store),
                    wings_dialog:update(?KEY(pnl_obj_params), Store);
                ?KEY(volume_type) ->
                    wings_dialog:show(?KEY(pnl_desnity_volume), Value =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value =:= noisevolume, Store),
                    wings_dialog:update(?KEY(pnl_volume_type), Store);
                ?KEY(material_type) ->
					%% IOR
					wings_dialog:show(?KEY(pnl_ior), not is_member(Value, [glossy,lightmat,blend_mat]), Store),
					%% Internal Reflection
					wings_dialog:show(?KEY(pnl_ir), Value =:= glass, Store),
					%% Glossy Color
					Gc = is_member(Value, [glossy,coatedglossy,translucent]),
					Rl = is_member(Value, [glass,rough_glass]),
					Rc = Value =:= shinydiffuse,
					wings_dialog:show(?KEY(pnl_gc), Gc, Store),
					%% Reflected Light
					wings_dialog:show(?KEY(pnl_rl), Rl, Store),
					%% Reflected Color
					wings_dialog:show(?KEY(pnl_rc), Rc, Store),
					wings_dialog:show(?KEY(pnl_rf), is_member(Value, [shinydiffuse,glass,rough_glass,glossy,coatedglossy,translucent]), Store),
					%% Diffuse Color
					wings_dialog:show(?KEY(pnl_dc_l), is_member(Value, [shinydiffuse,glossy,coatedglossy,translucent]), Store),
					%% Filtered Light
					wings_dialog:show(?KEY(pnl_fl_l), is_member(Value, [glass,rough_glass]), Store),
                    wings_dialog:show(?KEY(pnl_fl), is_member(Value, [shinydiffuse,glass,rough_glass,glossy,coatedglossy,translucent]), Store),
					%% Transparency
					wings_dialog:show(?KEY(pnl_transp), Value =:= shinydiffuse, Store),
					%% Specular Color
					wings_dialog:show(?KEY(pnl_sc), Value =:= translucent, Store),
					%% Absorption Color & Absorption Distance
					wings_dialog:show(?KEY(pnl_abs_reg), is_member(Value, [glass,rough_glass]), Store),
					wings_dialog:show(?KEY(pnl_abs_sss), Value =:= translucent, Store),
					wings_dialog:show(?KEY(pnl_abs), is_member(Value, [glass,rough_glass,translucent]), Store),
					%% Transmit Filter
					wings_dialog:show(?KEY(pnl_tf), is_member(Value, [shinydiffuse,glass,rough_glass]), Store),
					%% Translucency
					wings_dialog:show(?KEY(pnl_transl), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_transl_sss), Value =:= translucent, Store),
					%% Scatter Color & SigmaS Factor
					wings_dialog:show(?KEY(pnl_sct), Value =:= translucent, Store),
					%% Diffuse Reflection
					wings_dialog:show(?KEY(pnl_dr), is_member(Value, [shinydiffuse,glossy,coatedglossy,translucent]), Store),
					%% Mirror Reflection & Emit Light
					wings_dialog:show(?KEY(pnl_mr), Value =:= shinydiffuse, Store),
					%% Glossy Reflection & Exponent
					wings_dialog:show(?KEY(pnl_gr), is_member(Value, [glossy,coatedglossy,translucent]), Store),
					%% Anisotropic
					wings_dialog:show(?KEY(pnl_an), Value =:= coatedglossy, Store),
					%% Roughness
					wings_dialog:show(?KEY(pnl_rg), Value =:= rough_glass, Store),
					%% Dispersion Power & Dispersion Samples
					wings_dialog:show(?KEY(pnl_dsp), is_member(Value, [glass,rough_glass]), Store),
					%% nayar
					wings_dialog:show(?KEY(pnl_on), is_member(Value, [shinydiffuse,glossy,coatedglossy]), Store),
					%% Fresnel Effect
					wings_dialog:show(?KEY(pnl_fe), Value =:= shinydiffuse, Store),
					%% Ligth Material: Color & Power
					wings_dialog:show(?KEY(pnl_lm), Value =:= lightmat, Store),
					%% Blend: Material 1, Material 2 & Blend Mix
					wings_dialog:show(?KEY(pnl_bl), Value =:= blend_mat, Store),
					%wings_dialog:update(?KEY(pnl_shader_l), Store),
                    wings_dialog:update(?KEY(pnl_shader), Store);
                _ -> ok
            end
        end,
		
		%% Object Specific Material Properties Dialog
    %%
    Modulators = proplists:get_value(modulators, YafaRay, def_modulators(Maps)),
    ObjectFrame =
        {vframe, [
            {hframe, [
                {?__(6,"Autosmooth"),Autosmooth,[key(autosmooth),{hook,Hook_Enable}]},
                panel,
                {hframe, [
                    {label,?__(7,"Angle")},
                    {slider,{text,AutosmoothAngle,[range(autosmooth_angle),key(autosmooth_angle)]}},
                    help_button({material_dialog,object})
                ],[key(pnl_autosmooth),{margin,false}]}
            ]},

            %% Start Object Type Menu
            {vframe, [
                {hframe, [
                    {label, ?__(113,"Object Type")},
                    {menu,[
                        {?__(31,"Mesh"),mesh},
                        {?__(32,"Volume"),volume},
                        {?__(33,"Mesh Light"),meshlight},
                        {?__(34,"Light Portal"),lightportal}
                    ],Object_Type,[key(object_type),{hook,Hook_Show}]}
                ]},
                {hframe, [
                    {hframe, [
                        {vframe, [
                            {menu,[
                                {?__(82,"Uniform"),uniformvolume},
                                {?__(83,"ExpDensity"),expdensityvolume},
                                {?__(126,"Noise"),noisevolume}
                            ],Volume_Type,[key(volume_type),{hook,Hook_Show}]},
                            panel,
                            panel
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(84,"Absorption"), {text,Volume_Sigma_a,[range(volume_sigma_a),key(volume_sigma_a)]}},
                                {?__(85,"Scatter"), {text,Volume_Sigma_s,[range(volume_sigma_s),key(volume_sigma_s)]}},
                                {?__(86,"AttgridScale"), {text,Volume_Attgridscale,[range(volume_attgridscale),key(volume_attgridscale)]}}
                            ],[{margin,false}]},
                            %% Start ExpDensity Volume - ONLY
                            {label_column, [
                                {?__(90,"Height"), {text,Volume_Height,[range(volume_height),key(volume_height)]}},
                                {?__(91,"Steepness"), {text,Volume_Steepness,[range(volume_steepness),key(volume_steepness)]}},
                                {" ", panel}
                            ], [key(pnl_desnity_volume),{show,false},{margin,false}]},
                            %% End ExpDensity Volume - ONLY

                            %% Start Noise Volume - ONLY
                            {label_column, [
                                {?__(130,"Sharpness"), {text,Volume_Sharpness,[range(volume_sharpness),key(volume_sharpness)]}},
                                {?__(131,"Cover"), {text,Volume_Cover,[range(volume_cover),key(volume_cover)]}},
                                {?__(132,"Density"), {text,Volume_Density,[range(volume_density),key(volume_density)]}}
                            ], [key(pnl_noise_volume),{margin,false}]},
                            %% End Noise Volume - ONLY
                            {label_column, [
                                {?__(133,"Min/Max X"), {text,Volume_Minmax_X,[range(volume_minmax_x),key(volume_minmax_x)]}},
                                {?__(134,"Min/Max Y"), {text,Volume_Minmax_Y,[range(volume_minmax_y),key(volume_minmax_y)]}},
                                {?__(135,"Min/Max Z"), {text,Volume_Minmax_Z,[range(volume_minmax_z),key(volume_minmax_z)]}}
                            ],[{margin,false}]}
                        ],[key(pnl_volume_type),{margin,false}]}
                    ], [{title,"params"},key(pnl_volume),{margin,false}]},

                    {hframe, [
                        {label_column, [
                            {?__(121,"Power"), {text,Meshlight_Power,[range(meshlight_power),key(meshlight_power)]}},
                            {?__(122,"Samples"), {text,Meshlight_Samples,[range(meshlight_samples),key(meshlight_samples)]}}
                        ]},
                        {label_column, [
                            {?__(123,"Color"), {slider, {color, Meshlight_Color, [key(meshlight_color)]}}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(124,"Double Sided"),Meshlight_Double_Sided,[key(meshlight_double_sided)]},
                            panel
                        ]}
                    ], [key(pnl_mesh_light),{show,false}]},
                    {hframe, [
                        {label_column, [
                            {?__(121,"Power"), {text,Lightportal_Power,[key(lightportal_power),range(lightportal_power)]}},
                            {?__(122,"Samples"), {text,Lightportal_Samples,[key(lightportal_samples),range(lightportal_samples)]}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(142,"Diffuse Photons"),Lightportal_Diffusephotons,[key(lightportal_diffusephotons)]},
                            {?__(143,"Caustic Photons"),Lightportal_Causticphotons,[key(lightportal_causticphotons)]},
                            {?__(144,"Photon Only"),Lightportal_Photon_Only,[key(lightportal_photon_only)]}
                        ]}
                    ], [key(pnl_lightportal),{show,false}]}
                ],[{margin,false}]}
            ], [key(pnl_obj_params)]}
        %% End Object Type Menu
        ]},

    %% Shader Specific Material Properties Dialog
    %%
    ShaderFrame =
        {vframe, [
            {menu,menu_shader(),MaterialType,[key(material_type),{hook,Hook_Show}]},

            %% label column
            {vframe, [
                %% 1st row
                {label_column, [
                    {"IOR", {slider, {text,IOR,[range(ior),key(ior)]}}}
                ],[key(pnl_ior),{margin,false}]},
                %% 2nd row
                {label_column, [
                    {"Internal Reflection", {slider, {text,Glass_IR_Depth,[range(glass_ir_depth),key(glass_ir_depth)]}}}
                ],[key(pnl_ir),{show,false},{margin,false}]},
                %% 3rd row
                {hframe, [
                    {hframe, [
                        {label, "Reflected Color"}
                    ],[key(pnl_rc),{show,false},{margin,false}]},
                    {hframe, [
                        {label, "Reflected Light"}
                    ],[key(pnl_rl),{show,false},{margin,false}]},
                    {hframe, [
                        {label, "Glossy Color"}
                    ],[key(pnl_gc),{margin,false}]},
                    {hframe, [
                        {slider, {color,Reflected,[key(reflected)]}}
                    ],[{margin,false}]}
                ],[key(pnl_rf)]},
                %% 4th row
                {hframe, [
                    {vframe, [
                        {label, "Diffuse Color"}
                    ],[key(pnl_dc_l),{margin,false}]},
                    {vframe, [
                        {label, "Filtered Light"}
                    ],[key(pnl_fl_l),{show,false},{margin,false}]},
                    {vframe, [
                        {slider, {color,Transmitted,[key(transmitted)]}}
                    ],[{margin,false}]}
                ],[key(pnl_fl)]},
                %% 5th row
                {label_column, [
                    {"Transparency", {slider, {text,Transparency,[range(transparency),key(transparency)]}}}
                ],[key(pnl_transp),{show,false},{margin,false}]},
                %% 6th row
                {label_column, [
                    {"Specular Color", {slider, {color,SSS_Specular_Color, [key(sss_specular_color)]}}}
                ],[key(pnl_sc),{margin,false}]},
                %% 7th row
                {vframe, [
                    {label_column, [
                        {"Absorption Color", {slider, {color,SSS_AbsorptionColor,[key(sss_absorption_color)]}}}
                    ],[key(pnl_abs_sss),{margin,false},{show,false}]},
                    {label_column, [
                        {"Absorption Color", {slider, {color,AbsorptionColor,[key(absorption_color)]}}}
                    ],[key(pnl_abs_reg),{margin,false}]},
                    {label_column, [
                        {"Absorption Distance", {slider, {text,AbsorptionDist,[range(absorption_dist),key(absorption_dist)]}}}
                    ],[key(pnl_abs_v),{margin,false}]}
                ],[key(pnl_abs),{margin,false}]},
                %% 8th row
                {label_column, [
                    {"Transmit Filter", {slider, {text,TransmitFilter,[range(transmit_filter),key(transmit_filter)]}}}
                ],[key(pnl_tf),{show,false},{margin,false}]},
                %% 9th row
                {label_column, [
                    {"Translucency", {slider, {text,Translucency,[range(translucency),key(translucency)]}}}
                ],[key(pnl_transl),{margin,false},{show,false}]},
                {label_column, [
                    {"Translucency", {slider, {text,SSS_Translucency,[range(sss_translucency),key(sss_translucency)]}}}
                ],[key(pnl_transl_sss),{margin,false}]},
                %% 10th row
                {label_column, [
                    {"Scatter Color", {slider, {color,ScatterColor,[key(scatter_color)]}}},
                    {"SigmaS Factor", {slider, {text,SigmaSfactor,[range(sigmas_factor),key(sigmas_factor)]}}}
                ],[key(pnl_sct),{margin,false}]},
                %% 11th row
                {label_column, [
                    {"Diffuse Reflection", {slider, {text,DiffuseReflect,[range(diffuse_reflect),key(diffuse_reflect)]}}}
                ],[key(pnl_dr),{margin,false}]},
                %% 12th row
                {label_column, [
                    {"Mirror Reflection", {slider, {text,SpecularReflect,[range(specular_reflect),key(specular_reflect)]}}},
                    {"Emit Light", {slider, {text,Emit,[range(emit),key(emit)]}}}
                ],[key(pnl_mr),{show,false},{margin,false}]},
                %% 13th row
                {label_column, [
                    {"Glossy Reflection", {slider, {text,GlossyReflect,[range(glossy_reflect),key(glossy_reflect)]}}},
                    {"Exponent", {slider, {text,Exponent,[range(exponent),key(exponent)]}}}
                ],[key(pnl_gr),{margin,false}]},
                %% 14th row
                {hframe, [
                    {hframe, [
                        {"Anisotropic",Anisotropic,[key(anisotropic),{hook,Hook_Enable}]}
                    ],[{margin,false}]},
                    {hframe,[
                        {label, "Exp U"},
                        {text,Anisotropic_U,[range(anisotropic_u),key(anisotropic_u) ]},
                        panel,
                        {label, "Exp V"},
                        {text,Anisotropic_V,[range(anisotropic_v),key(anisotropic_v) ]}
                    ],[key(pnl_exp_coated),{margin,false}]}
                ],[key(pnl_an),{show,false},{margin,false}]},
                %% 15th row
                {label_column, [
                    {"Roughness", {slider, {text,Roughness,[range(roughness),key(roughness)]}}}
                ],[key(pnl_rg),{show,false},{margin,false}]},
                %% 16th row
                {vframe, [
                    {label_column, [
                        {"Dispersion Power", {slider, {text,DispersionPower,[range(dispersion_power),key(dispersion_power),{hook,Hook_Enable}]}}}
                    ],[{margin,false}]},
                    {label_column, [
                        {"Dispersion Samples", {slider, {text, DispersionSamples,[range(dispersion_samples),key(dispersion_samples)]}}}
                    ],[key(pnl_dsp_sam),{margin,false}]},
                    {"Fake Shadows",FakeShadows,[key(fake_shadows)]}
                ],[key(pnl_dsp),{show,false},{margin,false}]},
                %% 17th row
                {hframe, [
                    {"Oren-Nayar",OrenNayar,[key(oren_nayar),{hook,Hook_Enable}]},
                    {label_column,[
                        {"Sigma", {text,OrenNayar_Sigma,[range(sigma),key(sigma)]}}
                    ],[key(pnl_sigma_shiny),{margin,false}]}
                ],[key(pnl_on),{show,false},{margin,false}]},
                %% 18th row
                {hframe, [
                    {"Fresnel Effect",TIR,[key(tir)]}
                ],[key(pnl_fe),{show,false},{margin,false}]},
                %% 19th row
                {label_column, [
                    {"Color", {slider, {color,Lightmat_Color,[key(lightmat_color)]}}},
                    {"Power", {slider, {text,Lightmat_Power,[range(lightmat_power),key(lightmat_power)]}}}
                ],[key(pnl_lm),{show,false},{margin,false}]},
                %% 20th row
                {label_column, [
                    {"Material 1", {text,Blend_Mat1,[key(blend_mat1)]}},
                    {"Material 2", {text,Blend_Mat2,[key(blend_mat2)]}},
                    {"Blend Mix", {slider, {text,Blend_Value,[range(blend_value),key(blend_value)]}}}
                ],[key(pnl_bl),{show,false},{margin,false}]}

	    ],[key(pnl_shader), {margin,false}]}
        ]},
    %% End of Material Dialogs

    Modulator_Frame =
        {vframe,
            modulator_dialogs(Modulators, Maps)
        },

    [{
        ?__(1,"TheBounty Material"),
        {vframe, [
            {oframe, [
                {"Material", ShaderFrame},
                {"Textures", Modulator_Frame},
                {?__(8,"Object Parameters"), ObjectFrame}
                ], 1, [{style, buttons}]
            }
        ]}
    }].

%%% povman: need review 
alpha({R,G,B,A}) -> {R*A,G*A,B*A}.

%%% Define Lightmat Color
def_lightmat_color({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.

%%% Define Absorption Color
def_absorption_color({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.

%%% Grab OpenGL Transmitted Default Button
def_transmitted({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.
	
%% test move modulator def from wpc_bounty
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
