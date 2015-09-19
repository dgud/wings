%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the include GNU General Public License file for more details.
%%

%!
%! Export background environment
%! TO DO: make only one 'textured' mode
%!

export_background(F, BgName, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafaRay = proplists:get_value(?TAG, Ps, []),

    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_AMBIENT),
    SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
    % test

    case Bg of
        %% Constant Background Export
        constant ->
            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            BgColor = proplists:get_value(background_color, YafaRay, ?DEF_BACKGROUND_COLOR),
            export_rgb(F, color, BgColor),
            println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(power, YafaRay, ?DEF_POWER)]);

        gradientback ->
            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            HorizonColor = proplists:get_value(horizon_color, YafaRay, ?DEF_HORIZON_COLOR),
            export_rgb(F, horizon_color, HorizonColor),

            ZenithColor = proplists:get_value(zenith_color, YafaRay, ?DEF_ZENITH_COLOR),
            export_rgb(F, zenith_color, ZenithColor),

            println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(power, YafaRay, ?DEF_POWER)]);

%% Sunsky Background Export
        sunsky ->
            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),

            AddSun = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            println(F, "\t<turbidity fval=\"~.3f\"/>",[proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY)]),
            println(F, "\t<a_var fval=\"~.3f\"/>",[proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<b_var fval=\"~.3f\"/>",[proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<c_var fval=\"~.3f\"/>",[proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<d_var fval=\"~.3f\"/>",[proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<e_var fval=\"~.3f\"/>",[proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR)]),

            %% Add Sun
            case AddSun of
                true ->
                    println(F,
                        "\t<add_sun bval=\"~s\"/>",[format(AddSun)]),
                    println(F,
                        "\t<sun_power fval=\"~.3f\"/>", [proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER)]);

                false -> ok
            end,

            %% Add Skylight
            println(F, "\t<background_light bval=\"~s\"/>",[format(SkyBackgroundLight)]),

            case SkyBackgroundLight of
                true ->
                    println(F,
                        "\t<power fval=\"~.3f\"/>",
                            [proplists:get_value(sky_background_power, YafaRay, ?DEF_SKY_BACKGROUND_POWER)]),
                    println(F,
                        "\t<light_samples ival=\"~w\"/>",
                            [proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES)]);

                false -> ok
            end,
%% Add Skylight End

            export_pos(F, from, Position);

%% Darksky Background Export
        darksky ->

            DarkskyAltitude = proplists:get_value(darksky_altitude, YafaRay, ?DEF_DARKSKY_ALTITUDE),
            %SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
            %SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES),

            SunReal = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            %SunRealPower = proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER),

            %DarkskyNight = proplists:get_value(darksky_night, YafaRay, ?DEF_DARKSKY_NIGHT),
            DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, YafaRay, ?DEF_DARKSKY_DIFFUSEPHOTONS),
            DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, YafaRay, ?DEF_DARKSKY_CAUSTICPHOTONS),
            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),
            %
            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            println(F, "\t<turbidity fval=\"~.3f\"/>",[proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY)]),
            println(F, "\t<a_var fval=\"~.3f\"/>",[proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<b_var fval=\"~.3f\"/>",[proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<c_var fval=\"~.3f\"/>",[proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<d_var fval=\"~.3f\"/>",[proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<e_var fval=\"~.3f\"/>",[proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR)]),
            println(F, "\t<altitude fval=\"~.3f\"/>",[DarkskyAltitude]),

            %% Add Sun Real Start
            case SunReal of
                true ->
                    println(F,
                        "\t<add_sun bval=\"~s\"/>",[format(SunReal)]),

                    println(F,
                        "\t<sun_power fval=\"~.3f\"/>",
                            [proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER)]);

                false -> ok
            end,

            %% Add Skylight Start
            println(F, "\t<background_light bval=\"~s\"/>",[format(SkyBackgroundLight)]),
            %
            case SkyBackgroundLight of
                true ->
                    println(F,
                        "\t<power fval=\"~.3f\"/>",
                            [proplists:get_value(sky_background_power, YafaRay, ?DEF_SKY_BACKGROUND_POWER)]),
                    println(F,
                        "\t<light_samples ival=\"~w\"/>",
                            [proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES)]);

                false -> ok
            end,

            %% Add Darksky Photons

            println(F, "\t<with_diffuse bval=\"~s\"/>",[format(DarkskyDiffusePhotons)]),

            println(F, "\t<with_caustic bval=\"~s\"/>",[format(DarkskyCausticPhotons)]),

            println(F, "\t<night bval=\"~s\"/>",[format(proplists:get_value(darksky_night, YafaRay, ?DEF_DARKSKY_NIGHT))]),

            export_pos(F, from, Position);


%% HDRI Background Export
        'HDRI' ->
            BgFname =
                proplists:get_value(background_filename_HDRI, YafaRay,  ?DEF_BACKGROUND_FILENAME),
            BgExpAdj =
                proplists:get_value(power, YafaRay, ?DEF_POWER),
            BgMapping =
                proplists:get_value(background_mapping, YafaRay, ?DEF_BACKGROUND_MAPPING),
            BgRotation =
                proplists:get_value(background_rotation, YafaRay, ?DEF_BACKGROUND_ROTATION),

            %Samples = proplists:get_value(samples, YafaRay, ?DEF_SAMPLES),

            println(F, "<texture name=\"world_texture\">"),
            println(F, "\t<filename sval=\"~s\"/>",[BgFname]),
            println(F, "\t<interpolate sval=\"bilinear\"/>"),
            println(F, "\t<type sval=\"image\"/>"),
            println(F, "</texture>"),

            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"textureback\"/>"),
            println(F, "\t<power fval=\"~w\"/>",[BgExpAdj]),
            println(F, "\t<mapping sval=\"~s\"/>",[format(BgMapping)]),
            println(F, "\t<rotation fval=\"~.3f\"/>",[BgRotation]),

            println(F, "\t<texture sval=\"world_texture\"/>");

%% Image Background Export
        image ->
            BgFname = proplists:get_value(background_filename_image, YafaRay,  ?DEF_BACKGROUND_FILENAME),
            BgPower = proplists:get_value(power, YafaRay,   ?DEF_POWER),
            BgRotation = proplists:get_value(background_rotation, YafaRay, ?DEF_BACKGROUND_ROTATION),
            %Samples = proplists:get_value(samples, YafaRay, ?DEF_SAMPLES),
            
            % Create texture before background definition
            println(F, "<texture name=\"world_texture\">"),
            println(F, "\t<filename sval=\"~s\"/>",[BgFname]),
            println(F, "\t<interpolate sval=\"bilinear\"/>"),
            println(F, "\t<type sval=\"image\"/>"),
            println(F, "</texture>"),

            % Now, background
            println(F, "<background name=\"~s\">",[BgName]),
            println(F, "\t<type sval=\"textureback\"/>"),
            println(F, "\t<texture sval=\"world_texture\"/>"),
            println(F, "\t<power fval=\"~.3f\"/>",[BgPower]),
            println(F, "\t<rotation fval=\"~.3f\"/>",[BgRotation]);
        _  -> ""
    end,

    %% Add Enlight Image Background for all suport modes
    AllowIBL = 
        case  Bg of
            constant -> true;
            gradientback -> true;
            textureback -> true;
            _ -> false
        end,
    case AllowIBL of
        true ->
            Enlight = proplists:get_value(background_enlight, YafaRay, ?DEF_BACKGROUND_ENLIGHT),
            println(F, "\t<ibl bval=\"~s\"/>",[Enlight]),
            case Enlight of
                true ->
                    println(F,
                        "\t<ibl_samples ival=\"~w\"/>",[proplists:get_value(samples, YafaRay, ?DEF_SAMPLES)]),
                    println(F,
                        "\t<with_diffuse bval=\"~s\"/>",
                            [proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS)]),
                    println(F,
                        "\t<with_caustic bval=\"~s\"/>",
                            [proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS)]);
                false -> ok
            end;
        _ -> ""
    end,
    println(F, "</background>").


