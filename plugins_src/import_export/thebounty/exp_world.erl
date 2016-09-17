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
%! Note: Small 'case' parts to more extend code
%! Not more big parts of code inside 'cases'.
%!

export_background(F, Attr) ->
    OpenGL = proplists:get_value(opengl,Attr,[]),
    Bg = proplists:get_value(enviroment, Attr, sunsky),
    BgName = "world_background",
    case Bg of
        constant ->
            export_constant_background(F, Attr);

        gradientback ->
            export_gradient_background(F, Attr);

        textureback ->
            export_texture_background(F, Attr);
        _ ->
            export_sunsky_backgrounds(F, Bg, OpenGL, Attr)
    end,
    %% Add Enlight Image Background for all suport modes
    AllowIBL =
        case Bg of
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
                        "\t<ibl_samples ival=\"~w\"/>",[proplists:get_value(ibl_samples, Attr, 16)]),
                    println(F,
                        "\t<power fval=\"~.4f\"/>",[proplists:get_value(ibl_power, Attr)]),
                    println(F,
                        "\t<with_diffuse bval=\"~s\"/>",[proplists:get_value(to_diffuse, Attr, false)]),
                    println(F,
                        "\t<with_caustic bval=\"~s\"/>",[proplists:get_value(to_caustic, Attr, false)]);
                false -> ok
            end;
        _ -> ok
    end,
    println(F, "</background>").

export_constant_background(F, Attr) ->
    %
    println(F, "<background name=\"world_background\">"),
    println(F, "\t<type sval=\"constant\"/>"),
    BgColor = proplists:get_value(background_color, Attr, {0.8,0.8,0.8}),
    export_rgb(F, color, proplists:get_value(background_color, Attr, {0.8,0.8,0.8})),
    println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(background_power, Attr, 1.0)]).

export_gradient_background(F, Attr)->
    %gradientback
    println(F, "<background name=\"world_background\">"),
    println(F, "\t<type sval=\"gradientback\"/>"),
    HorizonColor = proplists:get_value(horizon_color, Attr, ?DEF_HORIZON_COLOR),
    export_rgb(F, horizon_color, HorizonColor),
    export_rgb(F, horizon_ground_color,{0.8,0.8,0.8}),
    ZenithColor = proplists:get_value(zenith_color, Attr, ?DEF_ZENITH_COLOR),
    export_rgb(F, zenith_color, ZenithColor),
    export_rgb(F, zenith_ground_color,{0.3,0.3,0.3}),
    println(F, "\t<power fval=\"~w\"/>", [proplists:get_value(background_power, Attr, 1.0)]).

export_texture_background(F, Attr) ->
    %!-----------------------------------------------
    %! write texture out of background declaration
    %!-----------------------------------------------
    println(F, "<texture name=\"world_texture\">"),
    println(F, "\t<filename sval=\"~s\"/>",[proplists:get_value(back_filename, Attr, "")]),
    println(F, "\t<interpolate sval=\"bilinear\"/>"), % TODO: create UI option
    println(F, "\t<type sval=\"image\"/>"),
    println(F, "</texture>"),

    % now, background values
    println(F, "<background name=\"world_background\">"),
    println(F, "\t<type sval=\"textureback\"/>"),
    println(F,
        "\t<mapping sval=\"~s\"/>",[format(proplists:get_value(ibl_mapping, Attr))]),
    println(F,
        "\t<rotation fval=\"~.3f\"/>",[proplists:get_value(ibl_rotation, Attr, 180.0)]),
    println(F,
        "\t<texture sval=\"world_texture\"/>").


export_sunsky_backgrounds(F, Bg, OpenGL, Attr) ->
    %!---------------------------
    %! common sunsky's values
    %!---------------------------
    BgLight = proplists:get_value(sky_light, Attr, false),

    println(F, "<background name=\"world_background\">"),
    println(F, "\t<type sval=\"~s\"/>",[format(Bg)]),
    %Position = proplists:get_value(position, OpenGL, {0.0,2.0,0.0}),
    export_pos(F, from, {0.0,2.0,0.0}),
    println(F,
        "\t<turbidity fval=\"~.3f\"/>",[proplists:get_value(turbidity, Attr, 2.0)]),
    println(F,
        "\t<a_var fval=\"~.3f\"/>",[proplists:get_value(a_var, Attr, 1.0)]),
    println(F,
        "\t<b_var fval=\"~.3f\"/>",[proplists:get_value(b_var, Attr, 1.0)]),
    println(F,
        "\t<c_var fval=\"~.3f\"/>",[proplists:get_value(c_var, Attr, 1.0)]),
    println(F,
        "\t<d_var fval=\"~.3f\"/>",[proplists:get_value(d_var, Attr, 1.0)]),
    println(F,
        "\t<e_var fval=\"~.3f\"/>",[proplists:get_value(e_var, Attr, 1.0)]),
    println(F,
        "\t<add_sun bval=\"~s\"/>",[format(proplists:get_value(add_sun, Attr, false))]),
    println(F,
        "\t<sun_power fval=\"~.3f\"/>",[proplists:get_value(sun_power, Attr, 1.0)]),
    println(F,
        "\t<background_light bval=\"~s\"/>",[format(BgLight)]),
    println(F,
        "\t<power fval=\"~.3f\"/>", [proplists:get_value(background_power, Attr, 1.0)]),
    println(F,
        "\t<light_samples ival=\"~w\"/>", [proplists:get_value(background_samples, Attr, 16)]),
    %!-------------------------
    %! specific daksky values
    %!-------------------------
    case Bg of
        darksky ->
            % "night", "altitude", "bright", "exposure", "color_space",
            println(F, "\t<night bval=\"~s\"/>",[format(proplists:get_value(night, Attr, false))]),
            println(F, "\t<altitude fval=\"~.3f\"/>",[proplists:get_value(altitude, Attr, 0.0)]),
            println(F, "\t<bright fval=\"~.3f\"/>",[1.0]),
            println(F, "\t<exposure fval=\"~.3f\"/>",[0.0]),
            println(F, "\t<color_space sval=\"~s\"/>",["sRGB (D65)"]), % TODO: create option in UI
            case BgLight of
                true ->
                    println(F,
                        "\t<with_diffuse bval=\"~s\"/>", [proplists:get_value(to_diffuse, Attr, false)]),
                    println(F,
                        "\t<with_caustic bval=\"~s\"/>",[proplists:get_value(to_caustic, Attr, false)]);
                _ -> ok
            end;
        _ -> ok
    end.




