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

% Export lighting integrator parameters to scene file
% TO DO: review some cases like when FG is activate

export_integrator(F, Attr) ->
    %
    Lighting_Method = proplists:get_value(lighting_method, Attr),

    println(F, "<integrator name=\"default\">"),
    println(F, "\t<raydepth ival=\"~w\"/>",    [proplists:get_value(raydepth, Attr)]),
    println(F, "\t<transpShad bval=\"~s\"/>",  [format(proplists:get_value(transparent_shadows, Attr))]),
    println(F, "\t<shadowDepth ival=\"~w\"/>", [proplists:get_value(shadow_depth, Attr)]),
    % TODO: create option in UI
    println(F, "\t<bg_transp bval=\"false\"/>"),
    println(F, "\t<bg_transp_refract bval=\"false\"/>"),

    case Lighting_Method of
        directlighting ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            UseCaustics = proplists:get_value(use_caustics, Attr),
            println(F, "\t<caustics bval=\"~s\"/>",[UseCaustics]),
            case UseCaustics of
                true ->
                    println(F, "\t<photons ival=\"~w\"/>",[proplists:get_value(caustic_photons, Attr)]),
                    println(F, "\t<caustic_depth ival=\"~w\"/>",[proplists:get_value(caustic_depth, Attr)]),
                    println(F, "\t<caustic_mix ival=\"~w\"/>",[proplists:get_value(caustic_mix, Attr)]),
                    println(F, "\t<caustic_radius fval=\"~.10f\"/>",[proplists:get_value(caustic_radius, Attr)]);

                false -> ok
            end,

            Do_AO = proplists:get_value(do_ao, Attr, false),
            println(F, "\t<do_AO bval=\"~s\"/>",[Do_AO]),
            case Do_AO of
                true ->
                    println(F, "\t<AO_distance fval=\"~.10f\"/>",[proplists:get_value(ao_distance, Attr, 1.0)]),
                    println(F, "\t<AO_samples fval=\"~.10f\"/>",[proplists:get_value(ao_samples, Attr, 16.0)]),
                    export_rgb(F, "AO_color", proplists:get_value(ao_color, Attr));
                false -> ok
            end;

        photonmapping ->
            %
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            println(F,
                "\t<photons ival=\"~w\"/>",[proplists:get_value(pm_diffuse_photons, Attr)]),
            println(F,
                "\t<bounces ival=\"~w\"/>",[proplists:get_value(pm_bounces, Attr)]),
            println(F,
                "\t<search ival=\"~w\"/>",[proplists:get_value(pm_search, Attr)]),
            println(F,
                "\t<diffuseRadius fval=\"~.10f\"/>",[proplists:get_value(pm_diffuse_radius, Attr)]),
            println(F,
                "\t<cPhotons ival=\"~w\"/>",[proplists:get_value(pm_caustic_photons, Attr)]),
            println(F,
                "\t<causticRadius fval=\"~.10f\"/>",[proplists:get_value(pm_caustic_radius, Attr)]),
            println(F,
                "\t<caustic_mix ival=\"~w\"/>",[proplists:get_value(pm_caustic_mix, Attr)]),
            %
            println(F,
                "\t<finalGather bval=\"~s\"/>",[proplists:get_value(pm_use_fg, Attr)]),
            println(F,
                "\t<fg_bounces ival=\"~w\"/>",[proplists:get_value(pm_fg_bounces, Attr)]),
            println(F,
                "\t<fg_samples ival=\"~w\"/>",[proplists:get_value(pm_fg_samples, Attr)]),
            println(F,
                "\t<show_map bval=\"~s\"/>",[proplists:get_value(pm_fg_show_map, Attr)]);

        pathtracing ->
            %
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            println(F,
                "\t<photons ival=\"~w\"/>",[proplists:get_value(pt_diffuse_photons, Attr)]),
            %% TO DO: review case when not caustic is active
            println(F,
                "\t<bounces ival=\"~w\"/>",[proplists:get_value(pt_bounces, Attr)]),
            println(F,
                "\t<caustic_type sval=\"~s\"/>",[proplists:get_value(pt_caustic_type, Attr)]),
            println(F,
                "\t<caustic_radius fval=\"~.10f\"/>",[proplists:get_value(pt_caustic_radius, Attr)]),
            println(F,
                "\t<caustic_mix ival=\"~w\"/>",[proplists:get_value(pt_caustic_mix, Attr)]),
            println(F,
                "\t<caustic_depth ival=\"~w\"/>",[proplists:get_value(pt_caustic_depth, Attr)]),
            println(F,
                "\t<path_samples ival=\"~w\"/>",[proplists:get_value(pt_samples, Attr)]);

        bidirectional ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]);

        sppm ->
            println(F, "\t<type sval=\"SPPM\"/>"),
            println(F,
                "\t<photons ival=\"~w\"/>",[proplists:get_value(sppm_photons, Attr)]),
            println(F,
                "\t<bounces ival=\"~w\"/>",[proplists:get_value(sppm_bounces, Attr)]),
            println(F,
                "\t<searchNum ival=\"~w\"/>",[proplists:get_value(sppm_search, Attr)]),
            println(F,
                "\t<photonRadius fval=\"~.10f\"/>",[proplists:get_value(sppm_radius, Attr)]),
            println(F,
                "\t<times fval=\"~.10f\"/>",[proplists:get_value(sppm_times, Attr)]),
            println(F,
                "\t<passNums ival=\"~w\"/>",[proplists:get_value(sppm_passes, Attr)]),
            println(F,
                "\t<pmIRE bval=\"~s\"/>",[format(proplists:get_value(sppm_ire, Attr))])
    end,
    %
    UseSSS = proplists:get_value(use_sss, Attr),
    AllowSSS =
        case Lighting_Method of
            bidirectional -> false;
            sppm -> false;
            _ -> true
        end,
    case AllowSSS of
        true ->
            case UseSSS of
                true ->
                    println(F, "\t<useSSS bval=\"true\"/>"),
                    println(F,
                        "\t<sssPhotons ival=\"~w\"/>",[proplists:get_value(sss_photons, Attr)]),
                    println(F,
                        "\t<sssDepth ival=\"~w\"/>",[proplists:get_value(sss_depth, Attr)]),
                    println(F,
                        "\t<sssScale fval=\"~.10f\"/>",[proplists:get_value(sss_scale, Attr)]),
                    println(F,
                        "\t<singleScatterSamples ival=\"~w\"/>",[proplists:get_value(sss_scatter_samples, Attr)]);
                false -> ok
            end;
        _ -> ok
    end,

    %% volume integrator
    Volintegr_Type = proplists:get_value(volintegr_type, Attr),
    println(F, "</integrator>\n"),
    println(F, "<integrator name=\"volintegr\">"),
    %
    case Volintegr_Type of
        singlescatterintegrator ->
            println(F, "\t<type sval=\"SingleScatterIntegrator\"/>"),
            println(F,
                "\t<adaptive bval=\"~s\"/>",[format(proplists:get_value(volintegr_adaptive, Attr))]),
            println(F,
                "\t<optimize bval=\"~s\"/>",[format(proplists:get_value(volintegr_optimize, Attr))]),
            println(F,
                "\t<stepSize fval=\"~.10f\"/>",[proplists:get_value(volintegr_stepsize, Attr)]);
        _ ->
            println(F, "\t<type sval=\"none\"/>")
    end,
    println(F, "</integrator>\n").
