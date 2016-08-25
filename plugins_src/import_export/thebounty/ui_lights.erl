%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 - 2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the include GNU General Public License file for more details.
%%

%
%
%

light_dialog(Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Attr = proplists:get_value(?TAG, Ps, []),
    LightType = proplists:get_value(type, OpenGL, []),
    DefPower = 
        case LightType of
            point -> ?DEF_ATTN_POWER;
            spot -> ?DEF_ATTN_POWER;
            area -> ?DEF_ATTN_POWER;
            _ -> ?DEF_POWER
        end,
    Power = proplists:get_value(power, Attr, DefPower),
    PowerStr =
        [{hframe, [
            {hframe, [
                {label,?__(1,"Power")},
                {text,Power,[range(power),key(power)]}
            ]},
            panel, panel, panel, panel, help_button({light_dialog,LightType})
        ]}],
    {vframe, PowerStr ++ light_dialog(Name, LightType, Attr)}.

%%% Point Light Dialog
light_dialog(_Name, point, Ps) ->
    PointLightType = proplists:get_value(type, Ps, ?DEF_POINT_TYPE),
    ArealightRadius = proplists:get_value(arealight_radius, Ps, ?DEF_AREALIGHT_RADIUS),
    ArealightSamples = proplists:get_value(arealight_samples, Ps, ?DEF_AREALIGHT_SAMPLES),

    Hook_Enable =
    fun(Key, Value, Store) ->
        case Key of
        ?KEY(type) ->
            wings_dialog:enable(?KEY(pnl_sphere), Value =:= spherelight, Store)
        end
    end,

    [
        {hradio,[
            {?__(3,"Pointlight"),pointlight},
            {?__(5,"Spherelight"),spherelight}
            ],PointLightType,[key(type),{hook,Hook_Enable}]
        },
        {hframe,[
            {hframe,[
                {label,?__(15,"Radius")},{text,ArealightRadius,[range(arealight_radius),key(arealight_radius)]}
            ]},
            panel,
            {hframe,[
                {label,?__(17,"Samples")}, {text,ArealightSamples,[range(samples),key(arealight_samples)]}
            ]}
        ],[key(pnl_sphere),{margin,false}]
        }
    ];

%!-----------------------------
%! Spot Light Dialog
%!-----------------------------
light_dialog(_Name, spot, Ps) ->
    SpotType = proplists:get_value(spot_type, Ps, ?DEF_SPOT_TYPE),
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
                ?KEY(spot_type) ->
                    wings_dialog:show(?KEY(pnl_ies), Value =:= spot_ies, Store),
                    wings_dialog:show(?KEY(pnl_spt_photon), Value =:= spotlight, Store),
                    wings_dialog:update(?KEY(pnl_spot_light), Store)
            end
        end,

    [
    {hradio,[
        {?__(100,"Spotlight"),spotlight},
        {?__(101,"IES"),spot_ies}
        ],SpotType,[key(spot_type),{hook,Hook_Show}]
    },
    {vframe, [
        {hframe, [
            {label,?__(102,"Filename")},
            {button,{text,SpotIESFilename,[key(spot_ies_filename),{width,35},{props,BrowsePropsIES}]}}
            ],[key(pnl_ies)]
        },
        {vframe, [
            {hframe, [
                {label,?__(103,"Blend")},
                {text,SpotBlend,[key(spot_blend),range(spot_blend)]},
                panel,
                {?__(104,"Photon Only"),SpotPhotonOnly,[key(spot_photon_only)]},
                panel
            ],[key(pnl_spt_photon)]},
            {hframe, [
                {?__(105,"Soft Shadows"),SpotSoftShadows,[key(spot_soft_shadows),{hook,Hook_Enabled}]},
                panel,
                {label,?__(106,"Samples")},
                {text,SpotIESSamples,[range(spot_ies_samples),key(spot_ies_samples)]},
                panel,
                {label,?__(107,"Fuzzyness")},
                {text,SpotFuzzyness,[range(spot_fuzzyness),key(spot_fuzzyness)]}
            ]}
        ],[{margin,false}]
        }
    ],[key(pnl_spot_light),[{margin,false}]]
    }];

%!----------------------------------
%! Infinite Light Dialog
%!----------------------------------
light_dialog(_Name, infinite, Ps) ->
    InfiniteType = proplists:get_value(type, Ps, ?DEF_INFINITE_TYPE),
    SunSamples = proplists:get_value(sun_samples, Ps, ?DEF_SUN_SAMPLES),
    SunAngle = proplists:get_value(sun_angle, Ps, ?DEF_SUN_ANGLE),
    InfiniteTrue = proplists:get_value(infinite_true, Ps, ?DEF_INFINITE_TRUE),
    InfiniteRadius = proplists:get_value(infinite_radius, Ps, ?DEF_INFINITE_RADIUS),

    Hook_Enabled =
    fun(Key, Value, Store) ->
        case Key of 
            ?KEY(infinite_true) ->
                wings_dialog:enable(?KEY(pnl_inf_radius), Value =:= false, Store)
        end
    end,
    Hook_Show =
    fun(Key, Value, Store) ->
        case Key of
            ?KEY(type) ->
                wings_dialog:show(?KEY(pnl_sunlight), Value =:= sunlight, Store),
                wings_dialog:show(?KEY(pnl_directional), Value =:= directional, Store),
                wings_dialog:update(?KEY(pnl_base1),Store)
        end
    end,
    [
        {vframe, [
            {vframe, [
                %% se queda ------------------------------------------------------->
                {hradio, [
                    {?__(110,"Sunlight"),sunlight},
                    {?__(111,"Directional"),directional}
                ],InfiniteType,[key(type),{hook,Hook_Show}]
                },
                {hframe, [
                    {hframe, [
                        {label,?__(112,"Samples ")},{text,SunSamples,[key(sun_samples),range(sun_samples)]}
                    ]},
                    panel,
                    {hframe, [
                        {label,?__(113,"Angle ")},{text,SunAngle,[key(sun_angle),range(sun_angle)]}
                    ]}
                ],[key(pnl_sunlight), {margin,false}]
                },
                %% Directional Semi-infinite Radius Settings Start
                {hframe, [
                    {?__(114,"Infinite"),InfiniteTrue,[key(infinite_true),{hook,Hook_Enabled}]},
                    panel,
                    {hframe, [
                        {label,?__(115,"Semi-infinite Radius")},
                        {text,InfiniteRadius,[range(infinite_radius),key(infinite_radius)]}
                    ],[key(pnl_inf_radius), {margin,false}]}
                ],[key(pnl_directional),{show,false}]}
            ],[key(pnl_base1),{margin,false}]}
        ]}
    ];


%!------------------------------
%! Area Light Dialog
%!------------------------------
light_dialog(_Name, area, Ps) ->
    ArealightSamples = proplists:get_value(arealight_samples, Ps, 16),
    [{label_column,[
        {?__(93,"Samples"), {text,ArealightSamples,[range(samples),key(arealight_samples)]}}
    ]}];

light_dialog(_Name, _Type, _Ps) ->
%%    erlang:display({?MODULE,?LINE,{_Name,_Type,_Ps}}),
    [].

light_result(_Name, Ps0, [{?KEY(power),Power}|Res0]) ->
    {LightPs0,Res1} = light_result(Res0),
    LightPs = [{Key,Val} || {?KEY(Key),Val} <- LightPs0],
    Ps = [{?TAG,[{power,Power}|LightPs]}
          |keydelete(?TAG, 1, Ps0)],
    {Ps,Res1}.

%!-----------------------------
%! Point and sphere
%!-----------------------------
light_result([{?KEY(type),pointlight}|_]=Ps) ->
    split_list(Ps, 3);
light_result([{?KEY(type),spherelight}|_]=Ps) ->
    split_list(Ps, 3);

%!-------------------
%! Spot
%!-------------------
light_result([{?KEY(spot_type),spotlight}|_]=Ps) ->
    split_list(Ps, 7);
    
light_result([{?KEY(spot_type),spot_ies}|_]=Ps) ->
    split_list(Ps, 7);


%!----------------------------------
%! Infinite and sun
%!----------------------------------
light_result([{?KEY(type),sunlight}|_]=Ps) ->
    split_list(Ps, 5); %21);

light_result([{?KEY(type),directional}|_]=Ps) ->
    split_list(Ps, 5); %21);

%!------------------------------------
%! Area
%!------------------------------------
light_result([{?KEY(arealight_samples),_}|_]=Ps) ->
    split_list(Ps, 1);

%!------------------------------------
%! Ambient
%!------------------------------------
%light_result([{?KEY(background),_}|_]=Ps) ->
%    split_list(Ps, 10);

%light_result([{?KEY(type),hemilight}|_]=Ps) ->
%    split_list(Ps, 11); %13);

light_result(Ps) ->
%%    erlang:display({?MODULE,?LINE,Ps}),
    {[],Ps}.
