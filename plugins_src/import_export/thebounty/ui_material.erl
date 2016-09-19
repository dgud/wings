%%% Object Specific Material Properties
%%%

material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    OpenGL = proplists:get_value(opengl, Mat),
    _DefReflected = alpha(proplists:get_value(specular, OpenGL)),
    _DefTransmitted = def_transmitted(proplists:get_value(diffuse, OpenGL)),
    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),
    Attr = proplists:get_value(?TAG, Mat, []),
    %MatAttr = proplists:get_value(?TAG, Mat, []),
    MaterialType = proplists:get_value(material_type, Attr, ?DEF_MATERIAL_TYPE),

    Object_Type = proplists:get_value(object_type, Attr, ?DEF_OBJECT_TYPE),
    Volume_Type = proplists:get_value(volume_type, Attr, ?DEF_VOLUME_TYPE),
    Volume_Sigma_a = proplists:get_value(volume_sigma_a, Attr, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, Attr, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, Attr, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, Attr, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, Attr, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Sharpness = proplists:get_value(volume_sharpness, Attr, ?DEF_VOLUME_SHARPNESS),
    Volume_Cover = proplists:get_value(volume_cover, Attr, ?DEF_VOLUME_COVER),
    Volume_Density = proplists:get_value(volume_density, Attr, ?DEF_VOLUME_DENSITY),
    Volume_Minmax_X = proplists:get_value(volume_minmax_x, Attr, ?DEF_VOLUME_MINMAX_X),
    Volume_Minmax_Y = proplists:get_value(volume_minmax_y, Attr, ?DEF_VOLUME_MINMAX_Y),
    Volume_Minmax_Z = proplists:get_value(volume_minmax_z, Attr, ?DEF_VOLUME_MINMAX_Z),
    Lightportal_Power = proplists:get_value(lightportal_power, Attr, ?DEF_LIGHTPORTAL_POWER),
    Lightportal_Samples = proplists:get_value(lightportal_samples, Attr, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Diffusephotons = proplists:get_value(lightportal_diffusephotons, Attr, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, Attr, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, Attr, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    % TODO:  sync mesh light and portal light color
    %% Light Material Properties
    %%
    Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),
    Lightmat_Power = proplists:get_value(lightmat_power, Attr, ?DEF_LIGHTMAT_POWER),

    Meshlight_Power = proplists:get_value(meshlight_power, Attr, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, Attr, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, Attr, ?DEF_MESHLIGHT_COLOR),
    Meshlight_Double_Sided = proplists:get_value(meshlight_double_sided, Attr, ?DEF_MESHLIGHT_DOUBLE_SIDED),

    AutosmoothAngle = proplists:get_value(autosmooth_angle, Attr, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, Attr,
                                     if AutosmoothAngle == 0.0 -> false;
                                        true -> ?DEF_AUTOSMOOTH end),

    %!---------------------------
    % shiny diffuse properties
    %!---------------------------
    DiffuseColor =   proplists:get_value(diffuse_color, Attr, {0.7,0.7,0.7}),
    DiffuseReflect = proplists:get_value(diffuse_reflect, Attr, ?DEF_DIFFUSE_REFLECT),
    MirrorColor =    proplists:get_value(mirror_color, Attr, {0.7,0.7,0.7}),
    MirrorReflect =  proplists:get_value(mirror_reflect, Attr, ?DEF_SPECULAR_REFLECT),
    Fresnel =        proplists:get_value(fresnel, Attr, ?DEF_FRESNEL),
    MirrorIOR =      proplists:get_value(mirror_ior, Attr, 1.0),

    Emit =           proplists:get_value(emit, Attr, 0.0),

    Transparency =  proplists:get_value(transparency, Attr, ?DEF_TRANSPARENCY),
    Translucency =  proplists:get_value(translucency, Attr, ?DEF_TRANSLUCENCY),
    Transmittance = proplists:get_value(transmittance, Attr, ?DEF_TRANSMIT_FILTER),
    IOR =           proplists:get_value(ior, Attr, ?DEF_IOR),
    ReflectMode =   proplists:get_value(reflect_mode, Attr, lambert),
    OrenNayar =     proplists:get_value(oren_nayar, Attr, ?DEF_OREN_NAYAR),
    Sigma =         proplists:get_value(sigma, Attr, ?DEF_OREN_NAYAR_SIGMA),

    %!------------------------------------------------------------------
    %! Glossy and Coated Glossy Properties.
    %!   -difuse, mirror (color and amount) are declared on shiny panel
    %!------------------------------------------------------------------
    Coated = proplists:get_value(coated, Attr, false),
    GlossyColor =   proplists:get_value(glossy_color, Attr, {0.9,0.9,0.9}),
    GlossyReflect = proplists:get_value(glossy_reflect, Attr, 0.0),
    Anisotropic = proplists:get_value(anisotropic, Attr, ?DEF_ANISOTROPIC),
    Exponent =    proplists:get_value(exponent, Attr, ?DEF_EXPONENT),
    Exponent_U =  proplists:get_value(anisotropic_u, Attr, 50.0),
    Exponent_V =  proplists:get_value(anisotropic_v, Attr, 500.0),
    _AsDiffuse  =  proplists:get_value(as_diffuse, Attr, false),

    %!--------------------------------------
    %! Glass and Roughness glass properties
    %---------------------------------------
    AbsorptionColor = proplists:get_value(absorption_color, Attr, {0.99,0.99,0.99}),
    AbsorptionDist = proplists:get_value(absorption_dist, Attr, 1.0),
    DispersionPower = proplists:get_value(dispersion_power, Attr, ?DEF_DISPERSION_POWER),
    DispersionSamples = proplists:get_value(dispersion_samples, Attr, ?DEF_DISPERSION_SAMPLES),
    FakeShadows = proplists:get_value(fake_shadows, Attr, ?DEF_FAKE_SHADOWS),
    Roughness = proplists:get_value(roughness, Attr, ?DEF_ROUGHNESS),

    %!---------------------------------
    %! Translucent material properties
    %!---------------------------------
    SSS_AbsorptionColor = proplists:get_value(sss_absorption_color, Attr, ?DEF_SSS_ABSORPTION_COLOR),
    ScatterColor = proplists:get_value(scatter_color, Attr, ?DEF_SCATTER_COLOR),
    SigmaSfactor = proplists:get_value(sigmas_factor, Attr, ?DEF_SIGMAS_FACTOR),
    SSS_Translucency = proplists:get_value(sss_translucency, Attr, ?DEF_SSS_TRANSLUCENCY),
    SSS_Specular_Color = proplists:get_value(sss_specular_color, Attr, ?DEF_SSS_SPECULAR_COLOR),


    %% Light Material Properties
    %%
    %Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),
    %Lightmat_Power = proplists:get_value(lightmat_power, Attr, ?DEF_LIGHTMAT_POWER),

    %% Blend Material Properties
    %%
    Blend_Mat1 = proplists:get_value(blend_mat1, Attr, "blendone"),
    Blend_Mat2 = proplists:get_value(blend_mat2, Attr, "blendtwo"),
    Blend_Value = proplists:get_value(blend_value, Attr, 0.5),

    OHook_Enable = fun(Key, Value, Store) ->
        case Key of
            ?KEY(autosmooth) ->
                wings_dialog:enable(?KEY(pnl_autosmooth), Value =/= ?DEF_OREN_NAYAR, Store)
        end
    end,
    OHook_Show =
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
                _ -> ok
            end
        end,

    %!--------------------------------------------
    %! Object Specific Material Properties Dialog
    %!--------------------------------------------
    % moved down..
    %Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    ObjectFrame =
        {vframe, [
            {hframe, [
                {?__(6,"Autosmooth"),Autosmooth,[key(autosmooth),{hook,OHook_Enable}]},
                panel,
                {hframe, [
                    {label,?__(7,"Angle")},
                    {slider,{text,AutosmoothAngle,[range(autosmooth_angle),key(autosmooth_angle)]}}%,
                    %help_button({material_dialog,object})
                ],[key(pnl_autosmooth),{margin,false}]}
            ]},

            %% Start Object Type Menu
            {vframe, [
                {hframe, [
                    {label, ?__(30,"Object Type")},
                    {menu,[
                        {?__(31,"Mesh"),mesh},
                        {?__(32,"Volume"),volume},
                        {?__(33,"Mesh Light"),meshlight},
                        {?__(34,"Light Portal"),lightportal}
                    ],Object_Type,[key(object_type),{hook,OHook_Show}]}
                ]},
                {hframe, [
                    {hframe, [
                        {vframe, [
                            {menu,[
                                {?__(35,"Uniform"),uniformvolume},
                                {?__(36,"ExpDensity"),expdensityvolume},
                                {?__(37,"Noise"),noisevolume}
                            ],Volume_Type,[key(volume_type),{hook,OHook_Show}]},
                            panel,
                            panel
                        ],[{margin,false}]},
                        {hframe, [
                            {label_column, [
                                {?__(38,"Absorption"), {text,Volume_Sigma_a,[range(volume_sigma_a),key(volume_sigma_a)]}},
                                {?__(39,"Scatter"), {text,Volume_Sigma_s,[range(volume_sigma_s),key(volume_sigma_s)]}},
                                {?__(40,"AttgridScale"), {text,Volume_Attgridscale,[range(volume_attgridscale),key(volume_attgridscale)]}}
                            ],[{margin,false}]},
                            %% Start ExpDensity Volume - ONLY
                            {label_column, [
                                {?__(41,"Height"), {text,Volume_Height,[range(volume_height),key(volume_height)]}},
                                {?__(42,"Steepness"), {text,Volume_Steepness,[range(volume_steepness),key(volume_steepness)]}},
                                {" ", panel}
                            ], [key(pnl_desnity_volume),{show,false},{margin,false}]},
                            %% End ExpDensity Volume - ONLY

                            %% Start Noise Volume - ONLY
                            {label_column, [
                                {?__(43,"Sharpness"), {text,Volume_Sharpness,[range(volume_sharpness),key(volume_sharpness)]}},
                                {?__(44,"Cover"), {text,Volume_Cover,[range(volume_cover),key(volume_cover)]}},
                                {?__(45,"Density"), {text,Volume_Density,[range(volume_density),key(volume_density)]}}
                            ], [key(pnl_noise_volume),{margin,false}]},
                            %% End Noise Volume - ONLY
                            {label_column, [
                                {?__(46,"Min/Max X"), {text,Volume_Minmax_X,[range(volume_minmax_x),key(volume_minmax_x)]}},
                                {?__(47,"Min/Max Y"), {text,Volume_Minmax_Y,[range(volume_minmax_y),key(volume_minmax_y)]}},
                                {?__(48,"Min/Max Z"), {text,Volume_Minmax_Z,[range(volume_minmax_z),key(volume_minmax_z)]}}
                            ],[{margin,false}]}
                        ],[key(pnl_volume_type),{margin,false}]}
                    ], [{title,"params"},key(pnl_volume),{margin,false}]},

                    {hframe, [
                        {label_column, [
                            {?__(49,"Power"), {text,Meshlight_Power,[range(meshlight_power),key(meshlight_power)]}},
                            {?__(50,"Samples"), {text,Meshlight_Samples,[range(meshlight_samples),key(meshlight_samples)]}}
                        ]},
                        {label_column, [
                            {?__(51,"Color"), {slider, {color, Meshlight_Color, [key(meshlight_color)]}}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(52,"Double Sided"),Meshlight_Double_Sided,[key(meshlight_double_sided)]},
                            panel
                        ]}
                    ], [key(pnl_mesh_light),{show,false}]},
                    {hframe, [
                        {label_column, [
                            {?__(53,"Power"), {text,Lightportal_Power,[key(lightportal_power),range(lightportal_power)]}},
                            {?__(54,"Samples"), {text,Lightportal_Samples,[key(lightportal_samples),range(lightportal_samples)]}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(55,"Diffuse Photons"),Lightportal_Diffusephotons,[key(lightportal_diffusephotons)]},
                            {?__(56,"Caustic Photons"),Lightportal_Causticphotons,[key(lightportal_causticphotons)]},
                            {?__(57,"Photon Only"),Lightportal_Photon_Only,[key(lightportal_photon_only)]}
                        ]}
                    ], [key(pnl_lightportal),{show,false}]}
                ],[{margin,false}]}
            ], [key(pnl_obj_params)]}
        %% End Object Type Menu
        ]},

    %!-------------------------------
    %! Material Properties Dialog
    %!-------------------------------
    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            ?KEY(fresnel) ->
                wings_dialog:enable(?KEY(pnl_ior_fresnel), Value =:= true, Store);
            ?KEY(reflect_mode) ->
                wings_dialog:enable(?KEY(pnl_sigma_shiny), Value =/= lambert, Store);
            ?KEY(anisotropic) ->
                wings_dialog:enable(?KEY(pnl_exp_uv), Value =:= true, Store),
                wings_dialog:enable(?KEY(pnl_exponent), Value =:= false, Store);
            ?KEY(dispersion_power) ->
                wings_dialog:enable(?KEY(pnl_dsp_sam), Value > 0.0, Store);
            ?KEY(coated) ->
                wings_dialog:enable(?KEY(pnl_mircol), Value =:= true, Store),
                wings_dialog:enable(?KEY(pnl_ior), Value =:= true, Store)
        end
    end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(material_type) ->
                    wings_dialog:show(?KEY(pnl_diffuse), is_member(Value, [shinydiffuse, glossy, translucent]), Store),
                    wings_dialog:show(?KEY(pnl_transp), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_emitt), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_mirror), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_mircol), is_member(Value, [shinydiffuse,glossy]), Store),
                    %% IOR
                    wings_dialog:show(?KEY(pnl_coated), Value =:= glossy, Store),
                    wings_dialog:show(?KEY(pnl_ior), Value =:= glossy, Store),
                    wings_dialog:show(?KEY(pnl_oren), is_member(Value, [shinydiffuse,glossy]), Store),
                    wings_dialog:show(?KEY(pnl_fresnel), Value =:= shinydiffuse, Store),

                    wings_dialog:show(?KEY(pnl_fl_l), is_member(Value, [glass,rough_glass]), Store),
                    wings_dialog:show(?KEY(pnl_fl), not is_member(Value, [blend_mat,lightmat]), Store),
                    %% Transparency
                    %wings_dialog:show(?KEY(pnl_transp), Value =:= shinydiffuse, Store),
                    %% Specular Color
                    wings_dialog:show(?KEY(pnl_spec_color), Value =:= translucent, Store),
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
                    %% Mirror Reflection & Emit Light
                    wings_dialog:show(?KEY(pnl_mr), Value =:= shinydiffuse, Store),
                    %% Glossy Reflection & Exponent
                    wings_dialog:show(?KEY(pnl_gr), Value =:= translucent, Store),
                    
                    wings_dialog:show(?KEY(pnl_glossy), is_member(Value, [glossy, translucent]), Store),
                    wings_dialog:show(?KEY(pnl_gloss_aniso), Value =:= glossy, Store),
                    %% Roughness
                    wings_dialog:show(?KEY(pnl_rg), Value =:= rough_glass, Store),
                    %% Dispersion Power & Dispersion Samples
                    wings_dialog:show(?KEY(pnl_dsp), Value =:= glass, Store),
                    %% nayar
                    wings_dialog:show(?KEY(pnl_on), is_member(Value, [shinydiffuse,glossy]), Store),
                    wings_dialog:show(?KEY(pnl_lm), Value =:= lightmat, Store),
                    %% Blend: Material 1, Material 2 & Blend Mix
                    wings_dialog:show(?KEY(pnl_bl), Value =:= blend_mat, Store),
                    %
                    wings_dialog:update(?KEY(pnl_shader), Store);
                _ -> ok
            end
        end,
    ShaderFrame =
        {vframe, [
            {menu,menu_shader(),MaterialType,[key(material_type),{hook,Hook_Show}]},
            {vframe, [
                %!-----------------------
                %! shiny material panels
                %!-----------------------
                {hframe, [
                    {hframe, [
                        {label, "Diffuse Color "}, {slider,{color,DiffuseColor,[key(diffuse_color)]}}, panel,
                        {label, "Diffuse Reflect"},{slider,{text,DiffuseReflect,[range(zero_to_one),key(diffuse_reflect)]}}
                    ]}
                ],[key(pnl_diffuse),{show,false},{margin,false}]
                },
                {hframe,[
                    {hframe, [
                        {label, "Reflectance model: "},
                        {menu,[
                            {?__(80,"Lambert"),lambert},
                            {?__(81,"Oren-Nayar"),oren_nayar}
                        ],ReflectMode,[key(reflect_mode),{hook,Hook_Enable}]}
                        ]}, panel,
                    {hframe,[
                        {label,"Sigma"}, {slider,{text,Sigma,[range(sigma),key(sigma),{width,4}]}}
                    ],[key(pnl_sigma_shiny),{margin, false}]}
                ],[key(pnl_oren)]},
                %!----------------------------------
                %! Trans/parency/lucency/mittance..
                %!----------------------------------
                {vframe,[
                    {hframe,[
                        {label,"Emittance"},{slider, {text,Emit,[range(emit),key(emit)]}}, panel
                    ]},
                    {hframe,[
                        {label,"Transparency"},{slider, {text,Transparency,[range(transparency),key(transparency)]}}, panel
                    ]},
                    {hframe,[
                        {label,"Translucency"}, {slider, {text,Translucency,[range(translucency),key(translucency)]}}, panel
                    ]},
                    {hframe,[
                        {label,"Transmittance"}, {slider, {text,Transmittance,[range(zero_to_one),key(transmittance)]}}, panel
                    ]}
                ],[key(pnl_transp),{show,false},{margin,false}]},
                %!---------------------
                %! glossy material.
                %!---------------------
                {vframe, [
                    {hframe, [
                        {label, "Glossy Color"}, {slider, {color,GlossyColor,[key(glossy_color)]}},
                        panel,
                        {label, "Glossy Reflect"},{slider,{text,GlossyReflect,[range(zero_to_one),key(glossy_reflect)]}},
                        panel
                    ]},
                    {hframe,[
                        {label, "Exponent"}, {slider, {text,Exponent,[range(exponent),key(exponent)]}},
                        panel
                    ],[key(pnl_exponent)]}
                ],[key(pnl_glossy),{show,false},{margin,false}]},
                {vframe, [
                    {hframe, [
                        {"Anisotropic",Anisotropic,[key(anisotropic),{hook,Hook_Enable}]}
                    ]},
                    {hframe,[
                        {label, "Exponent U"}, {slider, {text,Exponent_U,[range(exponent),key(anisotropic_u)]}}, panel,
                        {label, "Exponent V"}, {slider, {text,Exponent_V,[range(exponent),key(anisotropic_v),{width,3}]}}, panel
                    ],[key(pnl_exp_uv)]}
                    
                ],[key(pnl_gloss_aniso),{show,false},{margin,false}]},
                % fresnel
                {hframe,[
                    {"Fresnel Effect",Fresnel,[key(fresnel),{hook,Hook_Enable}]}, panel,
                    {hframe, [
                        {label, "IOR"}, {slider, {text,IOR,[range(ior),key(ior),{width,4}]}}
                    ],[key(pnl_ior_fresnel),{margin, false}]}
                ],[key(pnl_fresnel),{margin,false}]},
                %!----------------------
                %! Specular and fresnel
                %!----------------------
                {hframe, [
                    {"Coated Glossy",Coated,[key(coated),{hook,Hook_Enable}]}
                ],[key(pnl_coated),{margin,false}]},
                {hframe,[
                    {hframe, [
                        {label, "Mirror Color"},{color,MirrorColor, [key(mirror_color)]}
                    ],[key(pnl_mircol),{margin,false}]},
                    panel,
                    {hframe, [
                        {label, "IOR"}, {slider, {text,MirrorIOR,[range(zero_to_five),key(mirror_ior),{width,4}]}}
                    ],[key(pnl_ior),{margin,false}]},
                    {hframe,[
                        {label, "Mirror Reflect"},
                        {slider, {text,MirrorReflect,[range(zero_to_one),key(mirror_reflect),{width,2}]}}, panel
                    ],[key(pnl_mirror),{margin,false}]}
                ]},
                %----------------------------------------------<
                {hframe, [
                    {vframe, [
                        {label, "Filtered Light"}
                    ],[key(pnl_fl_l),{show,false},{margin,false}]}
                ],[key(pnl_fl)]},
                %% 6th row
                {label_column, [
                    {"Specular Color", {slider, {color,SSS_Specular_Color, [key(sss_specular_color)]}}}
                ],[key(pnl_spec_color),{margin,false}]},
                %% 7th row
                {vframe, [
                    {label_column, [
                        {"Absorption Color", {slider, {color,AbsorptionColor,[key(absorption_color)]}}}
                    ],[key(pnl_abs_reg),{margin,false}]},
                    {label_column, [
                        {"Absorption Distance", {slider, {text,AbsorptionDist,[range(absorption_dist),key(absorption_dist)]}}}
                    ],[key(pnl_abs_v),{margin,false}]}
                ],[key(pnl_abs),{margin,false}]},
                %% 8th row
                {label_column, [
                    {"Translucency", {slider, {text,SSS_Translucency,[range(sss_translucency),key(sss_translucency)]}}}
                ],[key(pnl_transl_sss),{margin,false}]},
                %% 10th row
                {label_column, [
                    {"Scatter Color", {slider, {color,ScatterColor,[key(scatter_color)]}}},
                    {"SigmaS Factor", {slider, {text,SigmaSfactor,[range(sigmas_factor),key(sigmas_factor)]}}}
                ],[key(pnl_sct),{margin,false}]},
                {label_column, [
                    {"Glossy Reflection", {slider, {text,GlossyReflect,[range(glossy_reflect),key(glossy_reflect)]}}}
                ],[key(pnl_gr),{margin,false}]
                },
                %% 15th row
                {label_column, [
                    {"Roughness", {slider, {text,Roughness,[range(roughness),key(roughness)]}}}
                ],[key(pnl_rg),{show,false},{margin,false}]},
                %% 16th row
                {vframe, [
                    {label_column, [
                        {"Dispersion Power", {slider,
                            {text,DispersionPower,[range(dispersion_power),key(dispersion_power),{hook,Hook_Enable}]}}}
                    ],[{margin,false}]},
                    {label_column, [
                        {"Dispersion Samples",
                            {slider, {text, DispersionSamples,[range(dispersion_samples),key(dispersion_samples)]}}}
                    ],[key(pnl_dsp_sam),{margin,false}]},
                    {"Fake Shadows",FakeShadows,[key(fake_shadows)]}
                ],[key(pnl_dsp),{show,false},{margin,false}]
                },
                {label_column, [
                    {"Color", {slider, {color,Lightmat_Color,[key(lightmat_color)]}}},
                    {"Power", {slider, {text,Lightmat_Power,[range(lightmat_power),key(lightmat_power)]}}}
                ],[key(pnl_lm),{show,false},{margin,false}]},
                
                %% 20th row
                {vframe, [
                    {hframe,[
                        {label, "Material 1"}, {text,Blend_Mat1,[key(blend_mat1)]}, panel
                    ]},
                    {hframe,[
                        {label, "Material 2"}, {text,Blend_Mat2,[key(blend_mat2)]}, panel
                    ]},
                    {hframe, [
                        {label, "Blend Mix"}, {slider, {text,Blend_Value,[range(blend_value),key(blend_value)]}}, panel
                    ]}
                ],[key(pnl_bl),{show,false},{margin,false}]}

        ],[{title,"Material"},key(pnl_shader), {margin,false}]}
        ]},
    % Modulators moved here..
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    Modulator_Frame = {vframe, modulator_dialogs(Modulators, Maps, MaterialType) },

    [{
        ?__(1,"TheBounty"),
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

%%% Grab OpenGL Transmitted Default Button
def_transmitted({Dr,Dg,Db,_Da}) ->
    Dt = 1-0,
    {Dr*Dt,Dg*Dt,Db*Dt}.

%% test move modulator here from wpc_thebounty
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
