%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the GNU General Public License for more details.
%%

%! Export render parameters to scene file.
%! TO DO: Is need make some code for detecting when no material SSS in scene.
%! Using SSS integrator without SSS material, cause to render freeze.


export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    %
    RenderFormat = proplists:get_value(render_format, Attr),
    %ExrFlagFloat = proplists:get_value(exr_float, Attr),
    %ExrFlagZbuf = proplists:get_value(exr_zbuf, Attr),
    %ExrFlagCompression = proplists:get_value(exr_flag_compression, Attr),

    %
    % from old code
    %! Compress mode for EXR files
    ExrCompress =
        case proplists:get_value(exr_flag_compression, Attr, exr_none) of
            exr_none -> 0;   exr_rle -> 1;    exr_zlin -> 2;
            exr_zblo -> 3;   exr_piz -> 4;    exr_pxr24 -> 5;
            exr_b44 -> 6;    exr_b44a -> 7;   _ -> 0
        end,
    
    %--------------------------------------------------------------

    println(F, "<render>"),
    println(F, "\t<camera_name sval=\"~s\"/>",[CameraName]),
    println(F,
        "\t<filter_type sval=\"~s\"/>",[proplists:get_value(aa_filter_type, Attr)]),
    println(F,
        "\t<AA_passes ival=\"~w\"/>",[proplists:get_value(aa_passes, Attr)]),
    println(F,
        "\t<AA_threshold fval=\"~.10f\"/>",[proplists:get_value(aa_threshold, Attr)]),
    println(F,
        "\t<AA_minsamples ival=\"~w\"/>",[proplists:get_value(aa_samples, Attr)]),
    println(F,
        "\t<AA_pixelwidth fval=\"~.10f\"/>",[proplists:get_value(aa_pixelwidth, Attr)]),
    %
    SaveAlpha = proplists:get_value(save_alpha, Attr),
    println(F,
        "\t<save_alpha bval=\"~s\"/>",[format(SaveAlpha)]),
    case SaveAlpha of
        premultiply ->
            println(F, "\t<premult bval=\"false\"/>");
        %backgroundmask ->
        %    println(F, "\t<alpha_backgroundmask bval=\"true\"/>"); % povman: deprecated
        _ -> ""
    end,
    println(F,
        "\t<clamp_rgb bval=\"~s\"/>",[format(proplists:get_value(clamp_rgb, Attr))]),
    println(F,
        "\t<z_channel bval=\"false\"/>"), % povman. review.. [proplists:get_value(flag_zbuf, Attr)]),
    %println(F,
    %    "\t<bg_transp_refract bval=\"~s\"/>",[format(proplists:get_value(background_transp_refract, Attr))]),
    println(F,
        "\t<background_name sval=\"~s\"/>",[BackgroundName]),
    %
    println(F, "\t<output_type sval=\"~s\"/>",[RenderFormat]),
    %%------------------------------------
    case RenderFormat of
        exr ->
            PixelType =
                case proplists:get_value(exr_float , Attr) of
                    true -> 2; %! 32 bits float
                    _ -> 1     %! 16 bits
                end,
            %println(F, "\t<exr_flags sval=\"~s\"/>",[ExrFlags]),
            println(F,
                "\t<pixel_type ival=\"~w\"/>",[PixelType]),
            println(F,
                "\t<compression ival=\"~w\"/>",[ExrCompress]);
        _ -> ""
    end,

    println(F, "\t<width ival=\"~w\"/>",[proplists:get_value(width, Attr)]),
    println(F, "\t<height ival=\"~w\"/>",[proplists:get_value(height, Attr)]),
    println(F, "\t<outfile sval=\"~s\"/>",[Outfile]),
    %println(F, "\t<indirect_samples sval=\"0\"/>"), % TO DO: review..
    %println(F, "\t<indirect_power sval=\"1.0\"/>"), %
    %println(F,
    %    "\t<exposure fval=\"~.10f\"/>",[proplists:get_value(exposure, Attr)]),
    println(F,
        "\t<gamma fval=\"~.10f\"/>",[proplists:get_value(gamma, Attr)]),
    println(F,
        "\t<integrator_name sval=\"default\"/>"),
    %
    ThreadsAuto = proplists:get_value(threads_auto, Attr),
    NThreads = case ThreadsAuto of
        true -> -1;
        false -> [proplists:get_value(threads_number, Attr)]
    end,
    println(F, "\t<threads ival=\"~w\"/>",[NThreads]),

    println(F, "\t<volintegrator_name sval=\"volintegr\"/>"),
    println(F, "</render>").
