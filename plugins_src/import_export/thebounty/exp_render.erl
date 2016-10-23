%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

%! Export render parameters to scene file.
%! TO DO: Is need make some code for checking when exists any material SSS in scene.
%! Using SSS integrator without SSS material, cause to render freeze.

export_render(F, Outfile, Attr) ->
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
    println(F, "\t<camera_name sval=\"camera\"/>"),
    println(F,
        "\t<filter_type sval=\"~s\"/>",[proplists:get_value(aa_filter_type, Attr)]),
    println(F,
        "\t<AA_passes ival=\"~w\"/>",[proplists:get_value(aa_passes, Attr)]),
    println(F,
        "\t<AA_threshold fval=\"~.10f\"/>",[proplists:get_value(aa_threshold, Attr)]),
    println(F,
        "\t<AA_inc_samples ival=\"~w\"/>",[proplists:get_value(aa_moresamples, Attr)]),
    println(F,
        "\t<AA_minsamples ival=\"~w\"/>",[proplists:get_value(aa_samples, Attr)]),
    println(F,
        "\t<AA_pixelwidth fval=\"~.10f\"/>",[proplists:get_value(aa_pixelwidth, Attr)]),
    %
    println(F,
        "\t<save_alpha bval=\"~s\"/>",[format(proplists:get_value(save_alpha, Attr))]),
    println(F,
        "\t<premult bval=\"false\"/>"),

    println(F,
        "\t<clamp_rgb bval=\"~s\"/>",[format(proplists:get_value(clamp_rgb, Attr))]),
    println(F,
        "\t<z_channel bval=\"~s\"/>",[format(proplists:get_value(z_pass, Attr))]),
    %println(F,
    %    "\t<bg_transp_refract bval=\"~s\"/>",[format(proplists:get_value(background_transp_refract, Attr))]),
    println(F,
        "\t<background_name sval=\"world_background\"/>"),
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
    println(F,
        "\t<gamma fval=\"~.4f\"/>",[proplists:get_value(gamma, Attr)]),
    println(F,
        "\t<integrator_name sval=\"default\"/>"),
    %
    ThreadsAuto = proplists:get_value(threads_auto, Attr),
    NThreads =
        case ThreadsAuto of
            true -> -1;
            false -> [proplists:get_value(threads_number, Attr)]
        end,
    println(F,
        "\t<drawParams bval=\"~s\"/>",[proplists:get_value(draw_params, Attr)]),
    println(F,
        "\t<threads ival=\"~w\"/>",[NThreads]),
    println(F,
        "\t<tile_size ival=\"64\"/>"),
    println(F,
        "\t<tiles_order sval=\"random\"/>"),
    println(F,
        "\t<show_sam_pix bval=\"~s\"/>",[proplists:get_value(show_pixels, Attr)]),
    println(F,
        "\t<volintegrator_name sval=\"volintegr\"/>"),

    println(F, "</render>").
