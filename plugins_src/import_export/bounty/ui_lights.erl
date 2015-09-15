%
%
%
%
%%%
light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafaRay = proplists:get_value(?TAG, Ps, []),
    Type = proplists:get_value(type, OpenGL, []),
    DefPower = 
        case Type of
            point -> ?DEF_ATTN_POWER;
            spot -> ?DEF_ATTN_POWER;
            area -> ?DEF_ATTN_POWER;
            _ -> ?DEF_POWER
        end,
    Power = proplists:get_value(power, YafaRay, DefPower),
    PowerStr =
        [{hframe, [
            {hframe, [
                {label,?__(1,"Power")},
                {text,Power,[range(power),key(power)]}
            ]},
            panel, panel, panel, panel, % povman test
            help_button({light_dialog,Type})
        ]}],
    {vframe, PowerStr ++ light_dialog(Name, Type, YafaRay)}.

%%% Point Light Dialog
light_dialog(_Name, point, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    ArealightRadius = proplists:get_value(arealight_radius, Ps, ?DEF_AREALIGHT_RADIUS),
    ArealightSamples = proplists:get_value(arealight_samples, Ps, ?DEF_AREALIGHT_SAMPLES),

    Hook_Show =
    fun(Key, Value, Store) ->
        case Key of
        ?KEY(type) ->
            wings_dialog:show(?KEY(pnl_sphere), Value =:= spherelight, Store)
        end
    end,

    [
    {hradio,[
            {?__(3,"Pointlight"),pointlight},
        {?__(5,"Spherelight"),spherelight}
        ],Type,[key(type),{hook,Hook_Show}]},
        {hframe, [
            {hframe, [
        {label,?__(15,"Radius")},
        {text,ArealightRadius,[range(arealight_radius),key(arealight_radius)]}
            ]},
            panel,
            {hframe, [
        {label,?__(17,"Samples")},
        {text,ArealightSamples,[range(samples),key(arealight_samples)]}
        ]}
        ],[key(pnl_sphere),{margin,false}]}
    ];

%%% Spot Light Dialog
light_dialog(_Name, spot, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_SPOT_TYPE),
    SpotPhotonOnly = proplists:get_value(spot_photon_only, Ps, ?DEF_SPOT_PHOTON_ONLY),
    SpotSoftShadows = proplists:get_value(spot_soft_shadows, Ps, ?DEF_SPOT_SOFT_SHADOWS),
    SpotBlend = proplists:get_value(spot_blend, Ps, ?DEF_SPOT_BLEND),
    SpotFuzzyness = proplists:get_value(spot_fuzzyness, Ps, ?DEF_SPOT_FUZZYNESS),
    SpotIESFilename = proplists:get_value(spot_ies_filename, Ps, ?DEF_SPOT_IES_FILENAME),
    SpotIESSamples = proplists:get_value(spot_ies_samples, Ps, ?DEF_SPOT_IES_SAMPLES),
    BrowsePropsIES = [{dialog_type,open_dialog},{extensions,[{".ies",?__(30,"IES")}]}],

    Hook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(spot_soft_shadows) ->
                    wings_dialog:enable(?KEY(spot_ies_samples), Value=/=?DEF_SPOT_SOFT_SHADOWS, Store),
                    wings_dialog:enable(?KEY(spot_fuzzyness), Value=/=?DEF_SPOT_SOFT_SHADOWS, Store)
            end
        end,

    Hook_Show =
    fun(Key, Value, Store) ->
        case Key of
        ?KEY(type) ->
            wings_dialog:show(?KEY(pnl_ies), Value =:= spot_ies, Store),
            wings_dialog:show(?KEY(pnl_spt_photon), Value =:= spotlight, Store),
                    wings_dialog:update(?KEY(pnl_spot_light), Store)
        end
    end,

    [
    {hradio,[
            {?__(29,"Spotlight"),spotlight},
        {?__(30,"IES"),spot_ies}
        ],Type,[key(type),{hook,Hook_Show}]},
        {vframe, [
        {hframe, [
        {label,?__(100,"Filename")},
        {button,{text,SpotIESFilename,[key(spot_ies_filename),{width,35},{props,BrowsePropsIES}]}}
        ],[key(pnl_ies)]},
            {vframe, [
        {hframe, [
                    {label,?__(101,"Blend")},
                    {text,SpotBlend,[key(spot_blend),range(spot_blend)]},
                    panel,
            {?__(97,"Photon Only"),SpotPhotonOnly,[key(spot_photon_only)]},
                    panel
        ],[key(pnl_spt_photon)]},
            {hframe, [
            {?__(38,"Soft Shadows"),SpotSoftShadows,[key(spot_soft_shadows),{hook,Hook_Enabled}]},
            panel,
            {label,?__(35,"Samples")},
            {text,SpotIESSamples,[range(spot_ies_samples),key(spot_ies_samples)]},
                    panel,
                    {label,?__(102,"Fuzzyness")},
                    {text,SpotFuzzyness,[range(spot_fuzzyness),key(spot_fuzzyness)]}
        ]}
            ],[{margin,false}]}
        ],[key(pnl_spot_light),[{margin,false}]]}
    ];

%%% Infinite Light Dialog
light_dialog(_Name, infinite, Ps) ->
    Type = proplists:get_value(type, Ps, ?DEF_INFINITE_TYPE),
    SunSamples = proplists:get_value(sun_samples, Ps, ?DEF_SUN_SAMPLES),
    SunAngle = proplists:get_value(sun_angle, Ps, ?DEF_SUN_ANGLE),
    SkyBackgroundLight = proplists:get_value(sky_background_light, Ps, ?DEF_SKY_BACKGROUND_LIGHT),
    SkyBackgroundPower = proplists:get_value(sky_background_power, Ps, ?DEF_SKY_BACKGROUND_POWER),
    SkyBackgroundSamples = proplists:get_value(sky_background_samples, Ps, ?DEF_SKY_BACKGROUND_SAMPLES),
    InfiniteTrue = proplists:get_value(infinite_true, Ps, ?DEF_INFINITE_TRUE),
    InfiniteRadius = proplists:get_value(infinite_radius, Ps, ?DEF_INFINITE_RADIUS),
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND_INFINITE),
    %%
    Turbidity = proplists:get_value(turbidity, Ps, ?DEF_TURBIDITY),
    A_var = proplists:get_value(a_var, Ps, ?DEF_SUNSKY_VAR),
    B_var = proplists:get_value(b_var, Ps, ?DEF_SUNSKY_VAR),
    C_var = proplists:get_value(c_var, Ps, ?DEF_SUNSKY_VAR),
    D_var = proplists:get_value(d_var, Ps, ?DEF_SUNSKY_VAR),
    E_var = proplists:get_value(e_var, Ps, ?DEF_SUNSKY_VAR),
    %%
    DarkskyAltitude = proplists:get_value(darksky_altitude, Ps, ?DEF_DARKSKY_ALTITUDE),
    SunReal = proplists:get_value(sun_real, Ps, ?DEF_SUN_REAL),
    SunRealPower = proplists:get_value(sun_real_power, Ps, ?DEF_SUN_REAL_POWER),

    DarkskyNight = proplists:get_value(darksky_night, Ps, ?DEF_DARKSKY_NIGHT),
    DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, Ps, ?DEF_DARKSKY_DIFFUSEPHOTONS),
    DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, Ps, ?DEF_DARKSKY_CAUSTICPHOTONS),

    Hook_Enabled =
    fun(Key, Value, Store) ->
        case Key of
                ?KEY(infinite_true) ->
                    wings_dialog:enable(?KEY(pnl_inf_radius), Value =:= false, Store);
                ?KEY(sun_real) ->
                    wings_dialog:enable(?KEY(pnl_sun_real), Value =/= ?DEF_SUN_REAL, Store);
        ?KEY(sky_background_light) ->
            wings_dialog:enable(?KEY(pnl_bkg_power), Value =/= ?DEF_SKY_BACKGROUND_LIGHT, Store),
                    wings_dialog:enable(?KEY(pnl_bkg_photons), Value =/=?DEF_SKY_BACKGROUND_LIGHT, Store)
        end
    end,
    Hook_Show =
    fun(Key, Value, Store) ->
        case Key of
        ?KEY(type) ->
            wings_dialog:show(?KEY(pnl_sunlight), Value =:= sunlight, Store),
            wings_dialog:show(?KEY(pnl_directional), Value =:= directional, Store),
                    wings_dialog:update(?KEY(pnl_base1),Store);
                ?KEY(background) ->
                    wings_dialog:show(?KEY(pnl_darksky_altitude), Value =:= darksky, Store),
                    wings_dialog:show(?KEY(darksky_night), Value =:= darksky, Store),
                    wings_dialog:show(?KEY(pnl_sky), Value =/= undefined, Store),
                    wings_dialog:update(?KEY(pnl_sky), Store)
        end
    end,
    [
        {vframe, [
            {vframe, [
                {hradio, [
                    {?__(110,"Sunlight"),sunlight},
                    {?__(111,"Directional"),directional}
                ],Type,[key(type),{hook,Hook_Show}]},
                %% Sunlight Settings Start
                {hframe, [
                    {hframe, [
                        {label,?__(114,"Samples")++" "},
                        {text,SunSamples,[key(sun_samples),range(sun_samples)]}
                    ]},
                    panel,
                    {hframe, [
                        {label,?__(115,"Angle")++" "},
                        {text,SunAngle,[key(sun_angle),range(sun_angle)]}
                    ]}
                ],[key(pnl_sunlight), {margin,false}]},
                %% Sunlight Settings End

                %% Directional Semi-infinite Radius Settings Start
                {hframe, [
                    {?__(112,"Infinite"),InfiniteTrue,[key(infinite_true),{hook,Hook_Enabled}]},
                    panel,
                    %% Directional Semi-infinite Radius
                    {hframe, [
                        {label,?__(113,"Semi-infinite Radius")},
                        {text,InfiniteRadius,[range(infinite_radius),key(infinite_radius)]}
                    ],[key(pnl_inf_radius), {margin,false}]}
                ],[key(pnl_directional),{show,false}]}
                %% End Directional Semi-infinite Radius
            ],[key(pnl_base1),{margin,false}]},

            {vframe, [
                %% Sunsky Background
                {hradio, [
                    {?__(44,"Sunsky"),sunsky},
                    {?__(46,"Darktide Sunsky"),darksky},
                    {?__(45,"None"), undefined}
                ],Bg,[key(background),{hook,Hook_Show}]},
                {vframe, [
                    {hframe, [
                        {label_column, [
                            {?__(47,"Turbidity"),{text,Turbidity,[range(turbidity),key(turbidity)]}},
                            {"a: "++?__(48,"Horizon Brightness"),{text,A_var,[key(a_var)]}},
                            {"b: "++?__(49,"Horizon Spread"),{text,B_var,[key(b_var)]}}
                        ],[{margin,false}]},
                        {label_column, [
                            {"c: "++?__(50,"Sun Brightness"),{text,C_var,[key(c_var)]}},
                            {"d: "++?__(51,"Sun Distance"),{text,D_var,[key(d_var)]}},
                            {"e: "++?__(52,"Backscattered Light"),{text,E_var,[key(e_var)]}}
                        ],[{margin,false}]}
                    ],[{margin,false}]},

                    {hframe,[
                        {hframe,[
                            {label,?__(122,"Altitude Adjustment")},
                            {text,DarkskyAltitude,[range(darksky_altitude),key(darksky_altitude)]}
                        ],[key(pnl_darksky_altitude),{margin,false}]}
                    ]},

                    %% Start Sun Real Settings
                    {hframe,[
                        {?__(120,"Real Sun"),SunReal,[key(sun_real),{hook,Hook_Enabled}]},
                        panel,
                        {hframe,[
                            {label,?__(121,"Sun Power")},
                            {text,SunRealPower,[range(sun_real_power),key(sun_real_power)]}
                        ],[key(pnl_sun_real),{margin,false}]}
                    ]},

                    %% Start Skylight Settings
                    {vframe,[
                        {hframe, [
                            {?__(116,"Skylight"),SkyBackgroundLight,[key(sky_background_light),{hook,Hook_Enabled}]},
                            panel,
                            {hframe, [
                                {label,?__(117,"Power")},
                                {text,SkyBackgroundPower,[range(sky_background_power),key(sky_background_power)]},
                                panel,
                                {label,?__(118,"Samples")},
                                {text,SkyBackgroundSamples,[range(sky_background_samples),key(sky_background_samples)]}
                            ], [key(pnl_bkg_power),{margin,false}]}
                        ]},
                        {hframe, [
                            {?__(123,"Diffuse Photons"),DarkskyDiffusePhotons,[key(darksky_diffusephotons)]},
                            panel,
                            {?__(124,"Caustic Photons"),DarkskyCausticPhotons,[key(darksky_causticphotons)]}
                        ],[key(pnl_bkg_photons)]}
                    ],[{margin,false}]},
                    {vframe,[
                        {?__(119,"Night"),DarkskyNight,[key(darksky_night)]}
                    ]}
                %% End Skylight Settings
                ],[key(pnl_sky),{margin,false}]}
            ],[{title,?__(53,"Sky")},key(pnl_base2),{margin,false}]}
        ]}
    ];

%%% Ambient Light Dialog
light_dialog(_Name, ambient, Ps) ->
    Bg = proplists:get_value(background, Ps, ?DEF_BACKGROUND_AMBIENT),
    BgColor = proplists:get_value(background_color, Ps, ?DEF_BACKGROUND_COLOR),
    %%
    HorizonColor = proplists:get_value(horizon_color, Ps, ?DEF_HORIZON_COLOR),
    ZenithColor = proplists:get_value(zenith_color, Ps, ?DEF_ZENITH_COLOR),
    %%
    BgFnameImage = proplists:get_value(background_filename_image, Ps, ?DEF_BACKGROUND_FILENAME),
    BrowsePropsImage = [{dialog_type,open_dialog},
                        {extensions,[{".jpg",?__(54,"JPEG compressed image")},
                                     {".tga",?__(55,"Targa bitmap")}]}],
    BgFnameHDRI = proplists:get_value(background_filename_HDRI, Ps,
                                      ?DEF_BACKGROUND_FILENAME),
    BrowsePropsHDRI = [{dialog_type,open_dialog},
                       {extensions,[{".hdr",?__(56,"High Dynamic Range image")},
                                    {".exr",?__(95,"OpenEXR image")}]}],
    BgMapping = proplists:get_value(background_mapping, Ps, ?DEF_BACKGROUND_MAPPING),
    BgEnlight = proplists:get_value(background_enlight, Ps, ?DEF_BACKGROUND_ENLIGHT),
    AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, Ps, ?DEF_AMBIENT_DIFFUSEPHOTONS),
    AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, Ps, ?DEF_AMBIENT_CAUSTICPHOTONS),
    BgRotation = proplists:get_value(background_rotation, Ps, ?DEF_BACKGROUND_ROTATION),
    %%
    Type = proplists:get_value(type, Ps, ?DEF_AMBIENT_TYPE),
    Samples = proplists:get_value(samples, Ps, ?DEF_SAMPLES),

    Hook_Enabled =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(background_enlight) ->
                    wings_dialog:enable(?KEY(pnl_enlight_samples), Value, Store),
                    wings_dialog:enable(?KEY(pnl_enlight_photons), Value, Store)
            end
        end,
    Hook_Show =
        fun(Key, Value, Store) ->
            case Key of
                ?KEY(background) ->
                    wings_dialog:show(?KEY(pnl_file), is_member(Value, ['HDRI',image]), Store),
                    wings_dialog:show(?KEY(pnl_img_hdri), Value =:= 'HDRI', Store),
                    wings_dialog:show(?KEY(pnl_img_bkg), Value =:= image, Store),
                    wings_dialog:show(?KEY(pnl_const), Value =:= constant, Store),
                    wings_dialog:show(?KEY(pnl_gradient), Value =:= gradientback, Store),
                    wings_dialog:show(?KEY(pnl_background), Value =/= undefined, Store),
                    wings_dialog:update(?KEY(pnl_background), Store)
            end
        end,

    [
        %% Global Photonlight
        %% Backgrounds
        {vframe, [
            {value,Type,[key(type)]},
            {hframe, [
                {label,?__(91,"Background Light/Environment")++" "},
                {menu, [
                    {?__(79,"HDRI"),'HDRI'},
                    {?__(80,"Image"),image},
                    {?__(81,"Constant"),constant},
                    {?__(105,"Gradient"),gradientback},
                    {?__(82,"None"), undefined}
                ], Bg, [key(background),{hook,Hook_Show}]}
            ]},
            {vframe, [
                %% HDRI Background
                {vframe, [
                    {label_column, [
                        {?__(83,"Filename"),
                            {vframe, [
                                {hframe, [
                                    {button,{text,BgFnameImage,[key(background_filename_image),{width,35},{props,BrowsePropsImage}]}}
                                ],[key(pnl_img_bkg),{margin,false},{show,false}]},
                                {hframe, [
                                    {button,{text,BgFnameHDRI,[key(background_filename_HDRI),{width,35},{props,BrowsePropsHDRI}]}},
                                    {menu, [
                                        {?__(86,"Light Probe"),probe},
                                        {?__(87,"Spherical"),spherical}
                                    ], BgMapping, [key(background_mapping)]}
                                ],[key(pnl_img_hdri),{margin,false}]}
                            ],[{margin,false}]}},
                        {?__(108,"Rotation"),
                            {text,BgRotation,[range(background_rotation),key(background_rotation)]}
                        }
                    ],[{margin,false}]
                }],
                [key(pnl_file),{margin,false}]
                },
                %% Constant Background
                {hframe, [
                    {label,?__(90,"Color")},
                    {color,BgColor,[key(background_color)]}
                ],[key(pnl_const),{show,false}]},
                %% Gradient Background
                {hframe,[
                    {label,?__(106,"Horizon Color")},
                    {color,HorizonColor,[key(horizon_color)]},
                    panel,
                    {label,?__(107,"Zenith Color")},
                    {color,ZenithColor,[key(zenith_color)]}
                ],[key(pnl_gradient),{show,false}]},
                %% Common parameters
                {vframe,[
                    {hframe,[
                        {hframe,[
                            {?__(89,"Enlight"),BgEnlight,[key(background_enlight),{hook,Hook_Enabled}]}
                        ]},
                        panel,
                        {hframe,[
                            {label,?__(60,"Samples")},
                            {text,Samples,[range(samples),key(samples)]}
                        ],[key(pnl_enlight_samples),{margin,false}]}
                    ],[{margin,false}]},
                    {hframe, [
                        {?__(94,"Diffuse Photons"),AmbientDiffusePhotons,[key(ambient_diffusephotons)]},
                        panel,
                        {?__(96,"Caustic Photons"),AmbientCausticPhotons,[key(ambient_causticphotons)]}
                    ],[key(pnl_enlight_photons)]}
                ]}
            ],[key(pnl_background),{margin,false}]}
        ]}];

%%% Area Light Dialog
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, ?DEF_AREALIGHT_SAMPLES),

    [
        {label_column,[
            {?__(93,"Samples"), {text,ArealightSamples,[range(samples),key(arealight_samples)]}}
        ]}
    ];

light_dialog(_Name, _Type, _Ps) ->
%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Ps0, [{?KEY(power),Power}|Res0]) ->
    {LightPs0,Res1} = light_result(Res0),
    LightPs = [{Key,Val} || {?KEY(Key),Val} <- LightPs0],
    Ps = [{?TAG,[{power,Power}|LightPs]}
          |keydelete(?TAG, 1, Ps0)],
    {Ps,Res1}.

%%% Point
light_result([{?KEY(type),pointlight}|_]=Ps) ->
    split_list(Ps, 3);
light_result([{?KEY(type),spherelight}|_]=Ps) ->
    split_list(Ps, 3);
%%% Spot
light_result([{?KEY(type),spotlight}|_]=Ps) ->
    split_list(Ps, 7);
light_result([{?KEY(type),spot_ies}|_]=Ps) ->
    split_list(Ps, 7);
%%% Infinite
light_result([{?KEY(type),sunlight}|_]=Ps) ->
    split_list(Ps, 21);
light_result([{?KEY(type),directional}|_]=Ps) ->
    split_list(Ps, 21);
%%% Area
light_result([{?KEY(arealight_samples),_}|_]=Ps) ->
    split_list(Ps, 1);
%%% Ambient
light_result([{?KEY(background),_}|_]=Ps) ->
    split_list(Ps, 10);
light_result([{?KEY(type),hemilight}|_]=Ps) ->
    split_list(Ps, 13);
light_result(Ps) ->
%%    erlang:display({?MODULE,?LINE,Ps}),
    {[],Ps}.
