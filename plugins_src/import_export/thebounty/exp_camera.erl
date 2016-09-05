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

export_camera(F, Attr) ->
    %
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} = proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    LensType = proplists:get_value(lens_type, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% TheBounty focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5 * Ro)),
    Aperture = proplists:get_value(aperture, Attr),

    println(F, "<camera name=\"camera\">"),
    println(F, "\t<type sval=\"~s\"/>",[LensType]),
    println(F, "\t<resx ival=\"~w\"/>",[Width]),
    println(F, "\t<resy ival=\"~w\"/>",[Height]),
    println(F, "\t<focal fval=\"~.10f\"/>",[FocalDist]),
    case Aperture of
        0.0 ->
            ok;
        _ ->
            println(F,
                "\t<aperture fval=\"~.10f\"/>",[Aperture]),
            println(F,
                "\t<bokeh_type sval=\"~s\"/>",[format(proplists:get_value(bokeh_type, Attr))]),
            println(F,
                "\t<bokeh_bias sval=\"~s\"/>",[format(proplists:get_value(bokeh_bias, Attr))]),
            println(F,
                "\t<bokeh_rotation fval=\"~.10f\"/>",[proplists:get_value(bokeh_rotation, Attr)]),
            println(F,
                "\t<dof_distance fval=\"~.10f\"/>",[proplists:get_value(dof_distance, Attr)])
    end,
    %
    case LensType of
        %
        orthographic ->
            println(F,
                "\t<scale fval=\"~.10f\"/>",[proplists:get_value(lens_ortho_scale, Attr)]);
        angular ->
            println(F,
                "\t<circular bval=\"~s\"/>",[proplists:get_value(lens_circular, Attr)]),
            println(F,
                "\t<mirrored bval=\"~s\"/>",[proplists:get_value(lens_mirrored, Attr)]),
            println(F,
                "\t<max_angle fval=\"~.10f\"/>",[proplists:get_value(lens_max_angle, Attr)]),
            println(F,
                "\t<angle fval=\"~.10f\"/>",[proplists:get_value(lens_angle, Attr)]);
        _ -> ok
    end,

    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.
