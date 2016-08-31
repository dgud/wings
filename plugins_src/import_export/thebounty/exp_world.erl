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

%!
%! Export background environment
%! TO DO: make only one 'textured' mode
%!

export_background(F, Attr) ->
    OpenGL = proplists:get_value(opengl,Attr,[]), %Ps, []),
    %Attr = proplists:get_value(?TAG, Att, []),
  
    Bg = proplists:get_value(background, Attr, constant),
    SkyBackgroundLight = proplists:get_value(background_light, Attr, ?DEF_SKY_BACKGROUND_LIGHT),
    % test
    case Bg of
        %% Constant Background Export
        constant ->
            println(F, "<background name=\"worldbackground\">"),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            BgColor = proplists:get_value(background_color, Attr, ?DEF_BACKGROUND_COLOR),
            export_rgb(F, color, BgColor),
            println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(power, Attr, ?DEF_POWER)]);

        gradientback ->
            println(F, "<background name=\"worldbackground\">"),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            HorizonColor = proplists:get_value(horizon_color, Attr, ?DEF_HORIZON_COLOR),
            export_rgb(F, horizon_color, HorizonColor),
            export_rgb(F, horizon_ground_color,{0.8,0.8,0.8}),
            ZenithColor = proplists:get_value(zenith_color, Attr, ?DEF_ZENITH_COLOR),
            export_rgb(F, zenith_color, ZenithColor),
            export_rgb(F, zenith_ground_color,{0.3,0.3,0.3}),

            println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(power, Attr, ?DEF_POWER)]);

%% Sunsky Background Export
        sunsky ->
            println(F, "<background name=\"worldbackground\">"),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),

            AddSun = proplists:get_value(add_sun, Attr, false),
            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            println(F, "\t<turbidity fval=\"~.3f\"/>",[proplists:get_value(turbidity, Attr, 2.0)]),
            println(F, "\t<a_var fval=\"~.3f\"/>",[proplists:get_value(a_var, Attr, 1.0)]),
            println(F, "\t<b_var fval=\"~.3f\"/>",[proplists:get_value(b_var, Attr, 1.0)]),
            println(F, "\t<c_var fval=\"~.3f\"/>",[proplists:get_value(c_var, Attr, 1.0)]),
            println(F, "\t<d_var fval=\"~.3f\"/>",[proplists:get_value(d_var, Attr, 1.0)]),
            println(F, "\t<e_var fval=\"~.3f\"/>",[proplists:get_value(e_var, Attr, 1.0)]),

            %% Add Sun
            case AddSun of
                true ->
                    println(F,
                        "\t<add_sun bval=\"~s\"/>",[format(AddSun)]),
                    println(F,
                        "\t<sun_power fval=\"~.3f\"/>", [proplists:get_value(sun_power, Attr, 1.0)]);

                false -> ok
            end,

            %% Add Skylight
            println(F, "\t<background_light bval=\"~s\"/>",[format(SkyBackgroundLight)]),

            case SkyBackgroundLight of
                true ->
                    println(F,
                        "\t<power fval=\"~.3f\"/>",
                            [proplists:get_value(sky_background_power, Attr, ?DEF_SKY_BACKGROUND_POWER)]),
                    println(F,
                        "\t<light_samples ival=\"~w\"/>",
                            [proplists:get_value(background_samples, Attr, ?DEF_SKY_BACKGROUND_SAMPLES)]);

                false -> ok
            end,
%% Add Skylight End

            export_pos(F, from, Position);

%% Darksky Background Export
        darksky ->

            SunReal = proplists:get_value(sun_real, Attr, ?DEF_SUN_REAL),
            DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, Attr, ?DEF_DARKSKY_DIFFUSEPHOTONS),
            DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, Attr, ?DEF_DARKSKY_CAUSTICPHOTONS),
            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),
            %
            println(F, "<background name=\"worldbackground\">"),
            println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
            println(F, "\t<turbidity fval=\"~.3f\"/>",[proplists:get_value(turbidity, Attr, 3.0)]),
            println(F, "\t<a_var fval=\"~.3f\"/>",[proplists:get_value(a_var, Attr, 1.0)]),
            println(F, "\t<b_var fval=\"~.3f\"/>",[proplists:get_value(b_var, Attr, 1.0)]),
            println(F, "\t<c_var fval=\"~.3f\"/>",[proplists:get_value(c_var, Attr, 1.0)]),
            println(F, "\t<d_var fval=\"~.3f\"/>",[proplists:get_value(d_var, Attr, 1.0)]),
            println(F, "\t<e_var fval=\"~.3f\"/>",[proplists:get_value(e_var, Attr, 1.0)]),
            println(F, "\t<altitude fval=\"~.3f\"/>",[proplists:get_value(altitude, Attr, 0.0)]),

            %% Add Sun Real Start
            case SunReal of
                true ->
                    println(F,
                        "\t<add_sun bval=\"~s\"/>",[format(SunReal)]),

                    println(F,
                        "\t<sun_power fval=\"~.3f\"/>",
                            [proplists:get_value(sun_real_power, Attr, ?DEF_SUN_REAL_POWER)]);

                false -> ok
            end,

            %% Add Skylight Start
            println(F, "\t<background_light bval=\"~s\"/>",[format(SkyBackgroundLight)]),
            %
            case SkyBackgroundLight of
                true ->
                    println(F,
                        "\t<power fval=\"~.3f\"/>",
                            [proplists:get_value(sky_background_power, Attr, ?DEF_SKY_BACKGROUND_POWER)]),
                    println(F,
                        "\t<light_samples ival=\"~w\"/>",
                            [proplists:get_value(sky_background_samples, Attr, ?DEF_SKY_BACKGROUND_SAMPLES)]);

                false -> ok
            end,

            %% Add Darksky Photons

            println(F, "\t<with_diffuse bval=\"~s\"/>",[format(DarkskyDiffusePhotons)]),

            println(F, "\t<with_caustic bval=\"~s\"/>",[format(DarkskyCausticPhotons)]),

            println(F, "\t<night bval=\"false\"/>"), %[format(proplists:get_value(darksky_night, Attr, ?DEF_DARKSKY_NIGHT))]),

            export_pos(F, from, Position);


%% HDRI Background Export
        texture ->
            BgFname =  proplists:get_value(back_filename, Attr,  ?DEF_BACKGROUND_FILENAME),
            BgExpAdj =  proplists:get_value(power, Attr, ?DEF_POWER),
            BgMapping = proplists:get_value(background_mapping, Attr, ?DEF_BACKGROUND_MAPPING),
            BgRotation = proplists:get_value(background_rotation, Attr, ?DEF_BACKGROUND_ROTATION),

            %Samples = proplists:get_value(samples, Attr, ?DEF_SAMPLES),
            println(F, "<texture name=\"world_texture\">"),
            println(F, "\t<filename sval=\"~s\"/>",[BgFname]),
            println(F, "\t<interpolate sval=\"bilinear\"/>"),
            println(F, "\t<type sval=\"image\"/>"),
            println(F, "</texture>"),

            println(F, "<background name=\"worldbackground\">"),
            println(F, "\t<type sval=\"textureback\"/>"),
            println(F, "\t<power fval=\"~w\"/>",[BgExpAdj]),
            println(F, "\t<mapping sval=\"~s\"/>",[format(BgMapping)]),
            println(F, "\t<rotation fval=\"~.3f\"/>",[BgRotation]),

            println(F, "\t<texture sval=\"world_texture\"/>");

%% Image Background Export
        image ->
            BgFname = proplists:get_value(background_filename_image, Attr,  ?DEF_BACKGROUND_FILENAME),
            BgPower = proplists:get_value(power, Attr,   ?DEF_POWER),
            BgRotation = proplists:get_value(background_rotation, Attr, ?DEF_BACKGROUND_ROTATION),
            %Samples = proplists:get_value(samples, Attr, ?DEF_SAMPLES),
            
            % Create texture before background definition
            println(F, "<texture name=\"world_texture\">"),
            println(F, "\t<filename sval=\"~s\"/>",[BgFname]),
            println(F, "\t<interpolate sval=\"bilinear\"/>"),
            println(F, "\t<type sval=\"image\"/>"),
            println(F, "</texture>"),

            % Now, background
            println(F, "<background name=\"worldbackground\">"),
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
            UseIBL = proplists:get_value(use_ibl, Attr, false),
            println(F, "\t<ibl bval=\"~s\"/>",[UseIBL]),
            case UseIBL of
                true ->
                    println(F,
                        "\t<ibl_samples ival=\"~w\"/>",[proplists:get_value(samples, Attr, 16)]),
                    println(F,
                        "\t<with_diffuse bval=\"~s\"/>",
                            [proplists:get_value(ambient_diffusephotons, Attr, false)]),
                    println(F,
                        "\t<with_caustic bval=\"~s\"/>",
                            [proplists:get_value(ambient_causticphotons, Attr, false)]);
                false -> ok
            end;
        _ -> ""
    end,
    println(F, "</background>").


