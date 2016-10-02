%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%  See AUTHORS.txt for a complete list of authors.
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the GNU General Public License for more details.
%%

%!-------------------------------------
%! Object and Material UI dialogs
%! Use camel case for atomic variables
%! and down slash '_' for all others
%!-------------------------------------

material_dialog(_Name, Mat) ->
    Maps = proplists:get_value(maps, Mat, []),
    _OpenGL = proplists:get_value(opengl, Mat),
    %DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),
    Attr = proplists:get_value(?TAG, Mat, []),
    MaterialType = proplists:get_value(material_type, Attr, ?DEF_MATERIAL_TYPE),

    Object_Type = proplists:get_value(object_type, Attr, mesh),
    Volume_Type = proplists:get_value(volume_type, Attr, uniformvolume),
    VolumeSigmaA = proplists:get_value(volume_sigma_a, Attr, ?DEF_VOLUME_SIGMA_A),
    VolumeSigmaS = proplists:get_value(volume_sigma_s, Attr, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, Attr, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, Attr, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, Attr, ?DEF_VOLUME_ATTGRIDSCALE),
    VolumeSharpness = proplists:get_value(volume_sharpness, Attr, 2.0),
    VolumeCover = proplists:get_value(volume_cover, Attr, ?DEF_VOLUME_COVER),
    VolumeDensity = proplists:get_value(volume_density, Attr, ?DEF_VOLUME_DENSITY),
    VolumePosX = proplists:get_value(volume_x, Attr, 1.0),
    VolumePosY = proplists:get_value(volume_y, Attr, 1.0),
    VolumePosZ = proplists:get_value(volume_z, Attr, 1.0),
    VolumeRegionSize = proplists:get_value(volume_region_size, Attr, 2.0),
    PortalPower = proplists:get_value(portal_power, Attr, ?DEF_LIGHTPORTAL_POWER),
    PortalSamples = proplists:get_value(portal_samples, Attr, ?DEF_LIGHTPORTAL_SAMPLES),
    PortalDiffusePhotons = proplists:get_value(portal_diffusephotons, Attr, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, Attr, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, Attr, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    % TODO:  sync mesh light and portal light color
    %% Light Material Properties
    %%
    %Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),
    %Lightmat_Power = proplists:get_value(lightmat_power, Attr, 0.9),

    Meshlight_Power = proplists:get_value(meshlight_power, Attr, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, Attr, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, Attr, ?DEF_MESHLIGHT_COLOR),
    MeshLightDoubleSided = proplists:get_value(meshlight_double_sided, Attr, ?DEF_MESHLIGHT_DOUBLE_SIDED),

    AutosmoothAngle = proplists:get_value(autosmooth_angle, Attr, ?DEF_AUTOSMOOTH_ANGLE),
    Autosmooth = proplists:get_value(autosmooth, Attr, AutosmoothAngle =/= 0.0), 
                                     %if AutosmoothAngle == 0.0 -> false;
                                     %  true -> ?DEF_AUTOSMOOTH end),
    VolumeFile = proplists:get_value(volume_file, Attr, ""),
    BrowseProps = [{dialog_type, open_dialog},
                    {extensions, [{".df3", "DF3 Volume"}]}],

    OHook_Enable = fun(Key, Value, Store) ->
        case Key of
            ?KEY(autosmooth) ->
                wings_dialog:enable(?KEY(pnl_autosmooth), Value =/= false, Store)
        end
    end,
    OHook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(object_type) ->
                    Value0 = wings_dialog:get_value(?KEY(volume_type),Store),
                    wings_dialog:show(?KEY(pnl_density_volume), Value0 =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value0 =:= noisevolume, Store),

                    wings_dialog:show(?KEY(pnl_volume), Value =:= volume, Store),
                    wings_dialog:show(?KEY(pnl_mesh_light), Value =:= meshlight, Store),
                    wings_dialog:show(?KEY(pnl_lightportal), Value =:= lightportal, Store),
                    wings_dialog:update(?KEY(pnl_obj_params), Store);
                ?KEY(volume_type) ->
                    wings_dialog:show(?KEY(pnl_density_volume), Value =:= expdensityvolume, Store),
                    wings_dialog:show(?KEY(pnl_noise_volume), Value =:= noisevolume, Store),
                    wings_dialog:show(?KEY(pnl_vol), Value =:= gridvolume, Store),
                    wings_dialog:update(?KEY(pnl_volume_type), Store);
                _ -> ok
            end
        end,

    %!----------------------------------------
    %! Object Specific properties Dialog
    %!----------------------------------------
    ObjectFrame =
        {vframe, [
            {hframe, [
                {?__(6,"Autosmooth"),Autosmooth,[key(autosmooth),{hook,OHook_Enable}]},
                panel,
                {hframe, [
                    {label,?__(7,"Angle")},
                    {slider,{text,AutosmoothAngle,[range(autosmooth_angle),key(autosmooth_angle)]}}                    
                ],[key(pnl_autosmooth),{margin,false}]},
                panel, help_button({material_dialog,object})
            ]},

            %% Start Object Type Menu
            {vframe, [
                {hframe, [
                    {label, ?__(20,"Object Type")},
                    {menu,[
                        {?__(21,"Mesh"),mesh},
                        {?__(22,"Volume"),volume},
                        {?__(23,"Mesh Light"),meshlight},
                        {?__(24,"Light Portal"),lightportal}
                    ],Object_Type,[key(object_type),{hook,OHook_Show}]}
                ]},
                {hframe, [
                    {vframe, [
                        {vframe, [
                            {hframe, [
                                {label, ?__(25,"Volume type")},
                                {menu,[
                                    {?__(26,"Uniform"),uniformvolume},
                                    {?__(27,"ExpDensity"),expdensityvolume},
                                    {?__(28,"Noise"),noisevolume},
                                    {?__(29,"GridVolume"),gridvolume}
                                ],Volume_Type,[key(volume_type),{hook,OHook_Show}]}%,
                            %panel,
                            %panel
                            ],[{margin,false}]}, panel,
                            {hframe, [
                                {label, ?__(30,"Volume File")},
                                {button,{text,VolumeFile,[key(volume_file),{width,30},{props,BrowseProps}]}}
                            ],[key(pnl_vol), {margin,false}]},
                            {hframe, [
                                {label, ?__(31,"Absorption")},{text,VolumeSigmaA,[range(volume_sigma_a),key(volume_sigma_a),{width,5}]},
                                panel,{label, ?__(32,"Scatter")},{text,VolumeSigmaS,[range(volume_sigma_s),key(volume_sigma_s),{width,5}]},
                                panel,{label, ?__(33,"AttgridScale")},
                                {text,Volume_Attgridscale,[range(volume_attgridscale),key(volume_attgridscale),{width,5}]}
                            ],[{margin,false}]},
                            %% Start ExpDensity Volume - ONLY
                            {hframe, [
                                {label, ?__(34,"Height")}, {text,Volume_Height,[range(volume_height),key(volume_height)]},
                                {label, ?__(35,"Steepness")}, {text,Volume_Steepness,[range(volume_steepness),key(volume_steepness)]}
                            ], [key(pnl_density_volume),{show,false},{margin,false}]},
                            
                            %% Start Noise Volume - ONLY
                            {hframe, [
                                {label, ?__(36,"Sharpness")},{text,VolumeSharpness,[range(volume_sharpness),key(volume_sharpness),{width,5}]},
                                panel,{label, ?__(37,"Cover")}, {text,VolumeCover,[range(volume_cover),key(volume_cover),{width,5}]},
                                panel,{label, ?__(38,"Density")}, {text,VolumeDensity,[range(volume_density),key(volume_density),{width,5}]}
                            ], [key(pnl_noise_volume),{margin,false}]},                            
                            %% End Noise Volume - ONLY
                            
                        
                            {hframe, [
                                {label,?__(39,"Position X")},
                                    {text,VolumePosX,[range(volume_region),key(volume_x),{width,5}]},
                                panel,{label,?__(40,"Position Y")},
                                    {text,VolumePosY,[range(volume_region),key(volume_y),{width,5}]},
                                panel,{label,?__(41,"Position Z")},
                                    {text,VolumePosZ,[range(volume_region),key(volume_z),{width,5}]}
                            ],[{margin,false}]},
                            {hframe, [
                                {label,?__(42,"Region Size")},
                                    {text,VolumeRegionSize,[range(volume_region),key(volume_region_size),{width,5}]}
                            ],[{margin,false}]}
                        ],[key(pnl_volume_type),{margin,false}]}
                    ], [{title,"params"},key(pnl_volume),{margin,false}]},
                    %!--------------------
                    %! meshlight
                    %!--------------------
                    {hframe, [
                        {label_column, [
                            {?__(45,"Power"), {text,Meshlight_Power,[range(meshlight_power),key(meshlight_power)]}},
                            {?__(46,"Samples"), {text,Meshlight_Samples,[range(meshlight_samples),key(meshlight_samples)]}}
                        ]},
                        {label_column, [
                            {?__(47,"Color"), {slider, {color, Meshlight_Color, [key(meshlight_color)]}}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(48,"Double Sided"),MeshLightDoubleSided,[key(meshlight_double_sided)]},
                            panel
                        ]}
                    ], [key(pnl_mesh_light),{show,false}]},
                    {hframe, [
                        {label_column, [
                            {?__(49,"Power"), {text,PortalPower,[key(lightportal_power),range(portal_power)]}},
                            {?__(50,"Samples"), {text,PortalSamples,[key(portal_samples),range(samples)]}},
                            {" ", panel}
                        ]},
                        {vframe, [
                            {?__(51,"Diffuse Photons"),PortalDiffusePhotons,[key(portal_diffusephotons)]},
                            {?__(52,"Caustic Photons"),Lightportal_Causticphotons,[key(lightportal_causticphotons)]},
                            {?__(53,"Photon Only"),Lightportal_Photon_Only,[key(lightportal_photon_only)]}
                        ]}
                    ], [key(pnl_lightportal),{show,false}]}
                ],[{margin,false}]}
            ], [key(pnl_obj_params)]}
        %% End Object Type Menu
        ]},

    %!-------------------------------
    %! Material Properties Dialog
    %!-------------------------------
    DiffuseColor =   proplists:get_value(diffuse_color, Attr, {0.7,0.7,0.7}),
    DiffuseReflect = proplists:get_value(diffuse_reflect, Attr, 1.0),
    MirrorColor =    proplists:get_value(mirror_color, Attr, {0.7,0.7,0.7}),
    MirrorReflect =  proplists:get_value(mirror_reflect, Attr, 0.0),
    Fresnel =        proplists:get_value(fresnel, Attr, false),
    MirrorIOR =      proplists:get_value(mirror_ior, Attr, 1.0),

    Emit =           proplists:get_value(emit, Attr, 0.0),

    Transparency =  proplists:get_value(transparency, Attr, 0.0),
    Translucency =  proplists:get_value(translucency, Attr, 0.0),
    Transmittance = proplists:get_value(transmittance, Attr, 1.0),
    IOR =           proplists:get_value(ior, Attr, 1.4),
    ReflectMode =   proplists:get_value(reflect_mode, Attr, lambert),
    Sigma =         proplists:get_value(sigma, Attr, 0.1),
    %!------------------------------------------------------------------
    %! Glossy and Coated Glossy Properties.
    %!   -difuse, mirror (color and amount) are declared on shiny panel
    %!------------------------------------------------------------------
    Coated = proplists:get_value(coated, Attr, false),
    GlossyColor =   proplists:get_value(glossy_color, Attr, {0.9,0.9,0.9}),
    GlossyReflect = proplists:get_value(glossy_reflect, Attr, 0.0),
    Anisotropic = proplists:get_value(anisotropic, Attr, false),
    Exponent =    proplists:get_value(exponent, Attr, 50.0),
    Exponent_U =  proplists:get_value(anisotropic_u, Attr, 50.0),
    Exponent_V =  proplists:get_value(anisotropic_v, Attr, 500.0),
    GlossyMirrorColor = proplists:get_value(glossy_mirror_color, Attr, {0.7,0.7,0.7}),
    AsDiffuse  =  proplists:get_value(as_diffuse, Attr, false),
    %!--------------------------------------
    %! Glass and Roughness glass properties
    %---------------------------------------
    GlassIOR =  proplists:get_value(glass_ior, Attr, 1.52),
    Rough = proplists:get_value(rough, Attr, false),
    AbsorptionColor = proplists:get_value(absorption_color, Attr, {0.99,0.99,0.99}),
    AbsorptionDist = proplists:get_value(absorption_dist, Attr, 1.0),
    DispersionPower = proplists:get_value(dispersion_power, Attr, ?DEF_DISPERSION_POWER),
    TransmitFilter = proplists:get_value(transmit_filter, Attr, 1.0),
    FakeShadows = proplists:get_value(fake_shadows, Attr, false),
    Roughness = proplists:get_value(roughness, Attr, 0.2),
    FilterColor = proplists:get_value(filter_color, Attr, {0.99,0.99,0.99}),
    ReflectColor = proplists:get_value(reflect_color, Attr, {0.99,0.99,0.99}),
    %!---------------------------------
    %! Translucent material properties
    %!---------------------------------
    ScatterColor = proplists:get_value(scatter_color, Attr, {0.738, 0.547, 0.315}),
    SigmaAColor = proplists:get_value(sigmaA_color, Attr, {0.0002, 0.0028, 0.0163}),
    SigmaSfactor = proplists:get_value(sigmas_factor, Attr, 1.0),
    ScatterTransmit = proplists:get_value(scatter_transmit, Attr, 1.0),
    SssSpecularColor = proplists:get_value(sss_specular_color, Attr, ?DEF_SSS_SPECULAR_COLOR),
    SpecularFactor = proplists:get_value(specular_factor, Attr, 50.0),
    SSSior = proplists:get_value(sss_ior, Attr, 1.3),
    SSSPhase = proplists:get_value(sss_phase, Attr, 0.8),

    %% Light Material Properties
    %%
    %Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),
    %Lightmat_Power = proplists:get_value(lightmat_power, Attr, 0.9),

    %% Blend Material Properties
    %%
    Blend_Mat1 = proplists:get_value(blend_mat1, Attr, "blendone"),
    Blend_Mat2 = proplists:get_value(blend_mat2, Attr, "blendtwo"),
    Blend_Value = proplists:get_value(blend_value, Attr, 0.5),

    %!----------------------------
    %% logic UI panels code
    %!----------------------------
    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            ?KEY(fresnel) ->
                wings_dialog:enable(?KEY(pnl_ior_fresnel), Value =:= true, Store);
            ?KEY(reflect_mode) ->
                wings_dialog:enable(?KEY(pnl_sig),   Value =/= lambert, Store),
                wings_dialog:enable(?KEY(pnl_sigma), Value =/= lambert, Store);
            ?KEY(anisotropic) ->
                wings_dialog:enable(?KEY(pnl_exp_uv), Value =:= true, Store),
                %wings_dialog:enable(?KEY(pnl_expv), Value =:= true, Store),
                wings_dialog:enable(?KEY(pnl_exp), Value =:= false, Store),
                wings_dialog:enable(?KEY(pnl_exp1), Value =:= false, Store);
            ?KEY(coated) ->
                wings_dialog:enable(?KEY(pnl_ior), Value =:= true, Store);
            ?KEY(rough) ->
                wings_dialog:enable(?KEY(pnl_roughness), Value =:= true, Store)
        end
    end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(material_type) ->
                    wings_dialog:show(?KEY(pnl_diffuse),is_member(Value, [shinydiffuse, glossy, translucent]), Store),
                    wings_dialog:show(?KEY(pnl_transp), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_emitt),  Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_mirror), Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_fresnel),Value =:= shinydiffuse, Store),
                    wings_dialog:show(?KEY(pnl_transl), Value =:= shinydiffuse, Store),
                    %
                    wings_dialog:show(?KEY(pnl_oren),is_member(Value, [shinydiffuse, glossy]), Store),
                    wings_dialog:show(?KEY(pnl_sigma),is_member(Value, [shinydiffuse, glossy]), Store),

                    wings_dialog:show(?KEY(pnl_exp_uv),   Value =:= glossy, Store),
                    %wings_dialog:show(?KEY(pnl_expv),   Value =:= glossy, Store),
                    wings_dialog:show(?KEY(pnl_coated), Value =:= glossy, Store),
                    wings_dialog:show(?KEY(pnl_ior),    Value =:= glossy, Store),
                    wings_dialog:show(?KEY(pnl_gloss_aniso), Value =:= glossy, Store),

                    wings_dialog:show(?KEY(pnl_fl), not is_member(Value, [blend_mat,lightmat]), Store),
                    %% Specular Color
                    wings_dialog:show(?KEY(pnl_glossy), is_member(Value, [glossy,translucent]), Store),
                    %wings_dialog:show(?KEY(pnl_exp),    Value =:= glossy, Store),
                    %wings_dialog:show(?KEY(pnl_exp1),   Value =:= glossy, Store),
                    %% Absorption Color & Absorption Distance
                    wings_dialog:show(?KEY(pnl_abs), Value =:= glass, Store),
                    %% Translucency
                    wings_dialog:show(?KEY(pnl_abs_sss),    Value =:= translucent, Store),
                    wings_dialog:show(?KEY(pnl_spec_color), Value =:= translucent, Store),
                    wings_dialog:show(?KEY(pnl_transl_sss), Value =:= translucent, Store),
                    wings_dialog:show(?KEY(pnl_phase),      Value =:= translucent, Store),
                    %% Scatter Color & SigmaS Factor
                    wings_dialog:show(?KEY(pnl_sct),        Value =:= translucent, Store),
                    wings_dialog:show(?KEY(pnl_scatter),    Value =:= translucent, Store),

                    %% Roughness
                    wings_dialog:show(?KEY(pnl_rough),     Value =:= glass, Store),
                    wings_dialog:show(?KEY(pnl_dsp),       Value =:= glass, Store),
                    wings_dialog:show(?KEY(pnl_filter),    Value =:= glass, Store),
                    wings_dialog:show(?KEY(pnl_fake),      Value =:= glass, Store),
                    wings_dialog:show(?KEY(pnl_fakelabel), Value =:= glass, Store),
                    %% nayar
                    wings_dialog:show(?KEY(pnl_lm), Value =:= lightmat, Store),
                    %% Blend: Material 1, Material 2 & Blend Mix
                    wings_dialog:show(?KEY(pnl_bl), Value =:= blend_mat, Store),
                    wings_dialog:show(?KEY(pnl_mix), Value =:= blend_mat, Store),
                    %
                    wings_dialog:update(?KEY(pnl_shader), Store)
            end
        end,
    % test

    MaterialFrame =
        {vframe, [
            {menu,menu_shader(),MaterialType,[key(material_type),{hook,Hook_Show}]},
            {vframe,[
                %
                %! shiny diffuse UI options, some items are use also in other materials, like diffuse or mirror
                %
                {hframe, [
                    {vframe, [{label, "Diffuse Color "}]},
                    {vframe, [{slider,{color,DiffuseColor,[key(diffuse_color)]}}]},
                    {vframe, [{label, "Diffuse Reflect"}]},
                    {vframe, [{slider,{text,DiffuseReflect,[range(zero_one),key(diffuse_reflect)]}}]}
                ],[key(pnl_diffuse),{margin,false}]},
                {hframe, [
                    {vframe, [{label, "Reflectance model:"}]},
                    {vframe, [
                        {menu,[
                            {?__(80,"Lambert"),lambert},
                            {?__(81,"Oren-Nayar"),oren_nayar}
                        ],ReflectMode,[key(reflect_mode),{hook,Hook_Enable}]}
                    ]}, panel,
                    {vframe, [{label,"Sigma"}],[key(pnl_sig)]},
                    {vframe, [{slider,{text,Sigma,[range(sigma),key(sigma)]}}],[key(pnl_sigma),{margin,false}]}
                ],[key(pnl_oren),{margin,false}]},
                {hframe, [
                    {vframe, [{label,"Emittance"}, {label,"Transparency"}]},
                    {vframe, [
                        {slider, {text,Emit,[range(emit),key(emit)]}},
                        {slider, {text,Transparency,[range(zero_one),key(transparency)]}}
                    ]},panel,
                    {vframe,[{label,"Translucency"}, {label,"Transmittance"}]},
                    {vframe,[
                        {slider, {text,Translucency,[range(zero_one),key(translucency)]}},
                        {slider, {text,Transmittance,[range(zero_one),key(transmittance)]}}
                    ]}
                ],[key(pnl_transp),{margin,false}]},
                {hframe,[
                    {vframe, [{label, "Mirror Color"}]},
                    {vframe, [{slider,{color,MirrorColor, [key(mirror_color)]}}]},panel,
                    {vframe, [{label, "Mirror Reflect"}]},
                    {vframe, [{slider, {text,MirrorReflect,[range(zero_one),key(mirror_reflect)]}}]}
                ],[key(pnl_mirror),{margin,false}]},
                {hframe,[
                    {vframe, [{"Fresnel Effect",Fresnel,[key(fresnel),{hook,Hook_Enable}]}]}, panel,
                    {hframe, [
                        {vframe, [{label, "IOR"}]},
                        {vframe, [{slider, {text,IOR,[range(ior),key(ior),{width,4}]}}]}
                    ],[key(pnl_ior_fresnel)]}
                ],[key(pnl_fresnel),{margin,false}]},
                %
                %! glossy materials options. Some items like 'glossy color' are use in translucent
                %
                {hframe, [
                    {vframe, [{label, "Glossy  Color"}]},
                    {vframe, [{slider,{color,GlossyColor,[key(glossy_color)]}}]},
                    {vframe, [{label, "Glossy Reflect"}]},
                    {vframe, [{slider,{text,GlossyReflect,[range(zero_one),key(glossy_reflect)]}}]}
                ],[key(pnl_glossy),{margin,false},{show,false}]},
                
                {hframe, [
                    {vframe, [{label, "Exp. U"}]},
                    {vframe, [{slider, {text,Exponent_U,[range(exponent),key(anisotropic_u)]}}]},
                    {vframe, [{label, "Exp. V"}]},
                    {vframe,[{slider, {text,Exponent_V,[range(exponent),key(anisotropic_v)]}}]}
                ],[key(pnl_exp_uv),{margin,false},{show,false}]},
                
                {hframe, [
                    {vframe, [{"Anisotropic",Anisotropic,[key(anisotropic),{hook,Hook_Enable}]}]},panel,
                    {vframe, [{label, "Exponent"}],[key(pnl_exp)]},
                    {vframe, [{slider, {text,Exponent,[range(exponent),key(exponent)]}}],[key(pnl_exp1)]},
                    panel,
                    {vframe, [{"As Diffuse",AsDiffuse,[key(as_diffuse)]}]}
                ],[key(pnl_gloss_aniso),{margin,false},{show,false}]},
                
                {hframe, [
                    {"Coated Glossy",Coated,[key(coated),{hook,Hook_Enable}]}, panel,
                    {hframe, [
                        {vframe, [{label, "Mirror Color"}]},
                        {vframe, [{slider,{color,GlossyMirrorColor, [key(glossy_mirror_color)]}}]}, panel,
                        {vframe, [{label, "IOR"}]},
                        {vframe, [{slider, {text,MirrorIOR,[range(zero_five),key(mirror_ior)]}}]}
                    ],[key(pnl_ior),{margin,false}]}
                ],[key(pnl_coated),{margin,false},{show,false}]},
                %
                %! glass material options
                %
                {hframe,[
                    {vframe,[{label, "Absorption"}, {label,"Abs. Distance"}]},
                    {vframe,[
                        {slider, {color,AbsorptionColor,[key(absorption_color)]}},
                        {slider, {text,AbsorptionDist,[range(zero_ten),key(absorption_dist)]}}
                    ]},
                    {vframe, [{label," Dispersion"}, {label, " IOR"}]},
                    {vframe, [
                        {slider, {text,DispersionPower,[range(zero_ten),key(dispersion_power)]}},
                        {slider, {text,GlassIOR,[range(zero_five),key(glass_ior)]}}
                    ]}
                ],[key(pnl_abs),{margin,false},{show,false}]},
                {hframe, [
                    {vframe, [{"Rough Glass",Rough,[key(rough),{hook,Hook_Enable}]}]}, panel,
                    {hframe,[
                        {vframe, [{label,"Roughness"}]},
                        {vframe, [{slider, {text,Roughness,[range(roughness),key(roughness)]}}]}
                    ],[key(pnl_roughness),{margin,false}]}
                ],[key(pnl_rough),{margin,false},{show,false}]},
                {hframe, [
                    {vframe, [{label, "Filter Color"}]},
                    {vframe, [{slider, {color,FilterColor, [key(filter_color)]}}]}, panel,
                    {vframe, [{label, "Reflect Color"}]},
                    {vframe, [{slider, {color,ReflectColor,[key(reflect_color)]}}]}
                ],[key(pnl_fake),{margin,false},{show,false}]},
                {hframe, [
                    {vframe, [{label, "Transmitt filter"}]},
                    {vframe, [{slider, {text,TransmitFilter,[range(zero_one),key(transmit_filter)]}}]}, panel,
                    {vframe, [{"Fake Shadows",FakeShadows,[key(fake_shadows)]}]}
                ],[key(pnl_filter),{margin,false},{show,false}]},
                %
                %! translucent material options
                %!
                {hframe,[
                    {vframe, [{label,"Specular"},{label,"Scatter"}]},
                    {vframe, [
                        {slider, {color,SssSpecularColor, [key(sss_specular_color)]}},
                        {slider, {color,ScatterColor,[key(scatter_color)]}}]},
                    {vframe, [{label,"Exponent"},{label,"Factor"}]},
                    {vframe, [
                        {slider, {text,SpecularFactor,[range(exponent),key(specular_factor)]}},
                        {slider, {text,SigmaSfactor,[range(zero_twenty),key(sigmas_factor)]}}]}
                ],[key(pnl_spec_color),{show,false},{margin,false}]},

                {hframe, [
                    {vframe, [{label,  "Absorption"}]},
                    {vframe, [{slider, {color,SigmaAColor,[key(sigmaA_color)]}}]},
                    {vframe, [{label,  "Translucency"}]},
                    {vframe, [{slider, {text,ScatterTransmit,[range(zero_one),key(scatter_transmit)]}}]}
                ],[key(pnl_scatter),{margin,false},{show,false}]},
                {hframe, [
                    {vframe, [{label,  "Phase"}]},
                    {vframe, [{slider, {text,SSSPhase,[range(sss_phase),key(sss_phase)]}}]},panel,
                    {vframe, [{label,  "IOR"}]},
                    {vframe, [{slider, {text,SSSior,[range(ior),key(sss_ior)]}}]}
                ],[key(pnl_phase),{margin,false},{show,false}]},
                
                {hframe,[
                    {vframe, [{label, "Material 1"}]},
                    {vframe, [{text,Blend_Mat1,[key(blend_mat1)]}]}, panel,
                    {vframe, [{label, "Material 2"}]},
                    {vframe, [{text,Blend_Mat2,[key(blend_mat2)]}]}
                ],[key(pnl_bl),{margin,false},{show,false}]},
                {hframe, [
                    {vframe, [{label, "Blend Mix"}]},
                    {vframe, [{slider,{text,Blend_Value,[range(zero_one),key(blend_value)]}}]}
                ],[key(pnl_mix),{margin,false},{show,false}]},
                help_button({material_dialog,fresnel})
            %
            ],[key(pnl_shader),{margin,false}]}
        ],[{margin,false}]},

    % Modulators moved here..
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    ModulatorFrame = {vframe, modulator_dialogs(Modulators, Maps, MaterialType) },

    [{
        ?__(1,"TheBounty"),
        {vframe, [
            {oframe, [
                {"Material", MaterialFrame},
                {"Textures", ModulatorFrame},
                {?__(8,"Object Parameters"), ObjectFrame}
                ], 1, [{style, buttons}]
            }
        ]}
    }].

%%% povman: need review
alpha({R,G,B,A}) -> {R*A,G*A,B*A}.

%%% Define Lightmat Color
%def_lightmat_color({Dr,Dg,Db,_Da}) ->
%    Dt = 1-0,
%    {Dr*Dt,Dg*Dt,Db*Dt}.

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
