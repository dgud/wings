%
%
%
%

export_render(F, CameraName, BackgroundName, Outfile, Attr) ->
    AA_passes = proplists:get_value(aa_passes, Attr),
    AA_minsamples = proplists:get_value(aa_minsamples, Attr),
    AA_pixelwidth = proplists:get_value(aa_pixelwidth, Attr),
    AA_threshold = proplists:get_value(aa_threshold, Attr),
    ClampRGB = proplists:get_value(clamp_rgb, Attr),
    BackgroundTranspRefract = proplists:get_value(background_transp_refract, Attr),
    AA_Filter_Type = proplists:get_value(aa_filter_type, Attr),
    SaveAlpha = proplists:get_value(save_alpha, Attr),
    Raydepth = proplists:get_value(raydepth, Attr),
    TransparentShadows = proplists:get_value(transparent_shadows, Attr),
    ShadowDepth = proplists:get_value(shadow_depth, Attr),
    Gamma = proplists:get_value(gamma, Attr),
    Exposure = proplists:get_value(exposure, Attr),
    RenderFormat = proplists:get_value(render_format, Attr),
    ExrFlagFloat = proplists:get_value(exr_flag_float, Attr),
    ExrFlagZbuf = proplists:get_value(exr_flag_zbuf, Attr),
    ExrFlagCompression = proplists:get_value(exr_flag_compression, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    UseSSS = proplists:get_value(use_sss, Attr),
    SSS_Photons = proplists:get_value(sss_photons, Attr),
    SSS_Depth = proplists:get_value(sss_depth, Attr),
    SSS_Scale = proplists:get_value(sss_scale, Attr),
    SSS_SingleScatter_Samples = proplists:get_value(sss_singlescatter_samples, Attr),
    UseCaustics = proplists:get_value(use_caustics, Attr),
    Caustic_Photons = proplists:get_value(caustic_photons, Attr),
    Caustic_Depth = proplists:get_value(caustic_depth, Attr),
    Caustic_Mix = proplists:get_value(caustic_mix, Attr),
    Caustic_Radius = proplists:get_value(caustic_radius, Attr),
    Do_AO = proplists:get_value(do_ao, Attr),
    AO_Distance = proplists:get_value(ao_distance, Attr),
    AO_Samples = proplists:get_value(ao_samples, Attr),
    AO_Color =  proplists:get_value(ao_color,Attr),
    Lighting_Method = proplists:get_value(lighting_method, Attr),
    PM_Diffuse_Photons = proplists:get_value(pm_diffuse_photons, Attr),
    PM_Bounces = proplists:get_value(pm_bounces, Attr),
    PM_Search = proplists:get_value(pm_search, Attr),
    PM_Diffuse_Radius = proplists:get_value(pm_diffuse_radius, Attr),
    PM_Caustic_Photons = proplists:get_value(pm_caustic_photons, Attr),
    PM_Caustic_Radius =  proplists:get_value(pm_caustic_radius, Attr),
    PM_Caustic_Mix = proplists:get_value(pm_caustic_mix, Attr),
    PM_Use_FG = proplists:get_value(pm_use_fg, Attr),
    PM_FG_Bounces = proplists:get_value(pm_fg_bounces, Attr),
    PM_FG_Samples = proplists:get_value(pm_fg_samples, Attr),
    PM_FG_Show_Map = proplists:get_value(pm_fg_show_map, Attr),
    PT_Diffuse_Photons = proplists:get_value(pt_diffuse_photons, Attr),
    PT_Bounces = proplists:get_value(pt_bounces, Attr),
    PT_Caustic_Type = proplists:get_value(pt_caustic_type, Attr),
    PT_Caustic_Radius =  proplists:get_value(pt_caustic_radius, Attr),
    PT_Caustic_Mix = proplists:get_value(pt_caustic_mix, Attr),
    PT_Caustic_Depth = proplists:get_value(pt_caustic_depth, Attr),
    PT_Samples = proplists:get_value(pt_samples, Attr),
    SPPM_Photons = proplists:get_value(sppm_photons, Attr),
    SPPM_Bounces = proplists:get_value(sppm_bounces, Attr),
    SPPM_Search = proplists:get_value(sppm_search, Attr),
    SPPM_Radius = proplists:get_value(sppm_radius, Attr),
    SPPM_Times = proplists:get_value(sppm_times, Attr),
    SPPM_Passes = proplists:get_value(sppm_passes, Attr),
    SPPM_Ire = proplists:get_value(sppm_ire, Attr),
    Volintegr_Type = proplists:get_value(volintegr_type, Attr),
    Volintegr_Adaptive = proplists:get_value(volintegr_adaptive, Attr),
    Volintegr_Optimize = proplists:get_value(volintegr_optimize, Attr),
    Volintegr_Stepsize = proplists:get_value(volintegr_stepsize, Attr),
    ThreadsAuto = proplists:get_value(threads_auto, Attr),
    ThreadsNumber = proplists:get_value(threads_number, Attr),
    
    
    println(F, "<integrator name=\"default\">"),
    println(F, "\t<raydepth ival=\"~w\"/>",[Raydepth]),
    println(F, "\t<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
    println(F, "\t<shadowDepth ival=\"~w\"/>",[ShadowDepth]),

    case Lighting_Method of
        directlighting ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "\t<caustics bval=\"~s\"/>",[UseCaustics]),
            case UseCaustics of
                    true ->            
                        println(F, "\t<photons ival=\"~w\"/>",[Caustic_Photons]),
                        println(F, "\t<caustic_depth ival=\"~w\"/>",[Caustic_Depth]),
                        println(F, "\t<caustic_mix ival=\"~w\"/>",[Caustic_Mix]),
                        println(F, "\t<caustic_radius fval=\"~.10f\"/>",[Caustic_Radius]);

                    false -> ""
            end,
            println(F, "\t<do_AO bval=\"~s\"/>",[Do_AO]),
            case Do_AO of
                    true ->
                        println(F, "\t<AO_distance fval=\"~.10f\"/>",[AO_Distance]),
                        println(F, "\t<AO_samples fval=\"~.10f\"/>",[AO_Samples]),
                        export_rgb(F, "AO_color",AO_Color);
                    false -> ""
            end;

        photonmapping ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "\t<photons ival=\"~w\"/>",[PM_Diffuse_Photons]),
            println(F, "\t<bounces ival=\"~w\"/>",[PM_Bounces]),
            println(F, "\t<search ival=\"~w\"/>",[PM_Search]),
            println(F, "\t<diffuseRadius fval=\"~.10f\"/>",[PM_Diffuse_Radius]),
            println(F, "\t<cPhotons ival=\"~w\"/>",[PM_Caustic_Photons]),
            println(F, "\t<causticRadius fval=\"~.10f\"/>",[PM_Caustic_Radius]),
            println(F, "\t<caustic_mix ival=\"~w\"/>",[PM_Caustic_Mix]),
            println(F, "\t<finalGather bval=\"~s\"/>",[PM_Use_FG]),
            println(F, "\t<fg_bounces ival=\"~w\"/>",[PM_FG_Bounces]),
            println(F, "\t<fg_samples ival=\"~w\"/>",[PM_FG_Samples]),
            println(F, "\t<show_map bval=\"~s\"/>",[PM_FG_Show_Map]);

        pathtracing ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "\t<photons ival=\"~w\"/>",[PT_Diffuse_Photons]),
            %% TO DO: review case when not caustic is active
            println(F, "\t<bounces ival=\"~w\"/>",[PT_Bounces]),
            println(F, "\t<caustic_type sval=\"~s\"/>",[PT_Caustic_Type]),
            println(F, "\t<caustic_radius fval=\"~.10f\"/>",[PT_Caustic_Radius]),
            println(F, "\t<caustic_mix ival=\"~w\"/>",[PT_Caustic_Mix]),
            println(F, "\t<caustic_depth ival=\"~w\"/>",[PT_Caustic_Depth]),
            println(F, "\t<path_samples ival=\"~w\"/>",[PT_Samples]);

        bidirectional ->
            println(F, "\t<type sval=\"~s\"/>",[Lighting_Method]);

        sppm ->
            println(F, "\t<type sval=\"SPPM\"/>"),
            println(F, "\t<photons ival=\"~w\"/>",[SPPM_Photons]),
            println(F, "\t<bounces ival=\"~w\"/>",[SPPM_Bounces]),
            println(F, "\t<searchNum ival=\"~w\"/>",[SPPM_Search]),
            println(F, "\t<photonRadius fval=\"~.10f\"/>",[SPPM_Radius]),
            println(F, "\t<times fval=\"~.10f\"/>",[SPPM_Times]),
            println(F, "\t<passNums ival=\"~w\"/>",[SPPM_Passes]),
            println(F, "\t<pmIRE bval=\"~s\"/>",[format(SPPM_Ire)])

    end,

    case Lighting_Method of
        {pathtracing,photonmapping,directlighting} ->
            case  UseSSS of
                true ->
                    println(F, "\t<useSSS bval=\"true\"/>"),
                    println(F, "\t<sssPhotons ival=\"~w\"/>",[SSS_Photons]),
                    println(F, "\t<sssDepth ival=\"~w\"/>",[SSS_Depth]),
                    println(F, "\t<sssScale fval=\"~.10f\"/>",[SSS_Scale]),
                    println(F, "\t<singleScatterSamples ival=\"~w\"/>",[SSS_SingleScatter_Samples]);

                false -> ""
            end;
        _ -> ""
    end,

    println(F, "</integrator>\n"),
    % volume integrator
    println(F, "<integrator name=\"volintegr\">"),
    case Volintegr_Type of
        singlescatterintegrator ->
            println(F, "\t<type sval=\"SingleScatterIntegrator\"/>"),
            println(F, "\t<adaptive bval=\"~s\"/>",[format(Volintegr_Adaptive)]),
            println(F, "\t<optimize bval=\"~s\"/>",[format(Volintegr_Optimize)]),
            println(F, "\t<stepSize fval=\"~.10f\"/>",[Volintegr_Stepsize]);
        _ -> 
            println(F, "\t<type sval=\"none\"/>")
    end,
    println(F, "</integrator>\n"),

    ExrFlags =
        case RenderFormat of
            exr ->
                [if ExrFlagFloat -> "float "; true -> "" end,
                 if ExrFlagZbuf -> "zbuf "; true -> "" end,
                 format(ExrFlagCompression)];
            _ -> ""
        end,
    %
    println(F, "<render>"),
    println(F, "\t<camera_name sval=\"~s\"/>",[CameraName]),
    println(F, "\t<filter_type sval=\"~s\"/>",[AA_Filter_Type]),
    println(F, "\t<AA_passes ival=\"~w\"/>",[AA_passes]),
    println(F, "\t<AA_threshold fval=\"~.10f\"/>",[AA_threshold]),
    println(F, "\t<AA_minsamples ival=\"~w\"/>",[AA_minsamples]),
    println(F, "\t<AA_pixelwidth fval=\"~.10f\"/>",[AA_pixelwidth]),
    %
    case SaveAlpha of
        premultiply ->
            println(F, "\t<tpremult bval=\"true\"/>");
        backgroundmask ->
                println(F, "\t<alpha_backgroundmask bval=\"true\"/>"); % povman: deprecated
            _ -> ""
        end,
        println(F, "\t<clamp_rgb bval=\"~s\"/>",[format(ClampRGB)]),
        println(F, "\t<bg_transp_refract bval=\"~s\"/>",[format(BackgroundTranspRefract)]),
        println(F, "\t<background_name sval=\"~s\"/>",[BackgroundName]),
        %
        println(F, "\t<output_type sval=\"~s\"/>",[RenderFormat]),
        case RenderFormat of
            exr -> println(F, "\t<exr_flags sval=\"~s\"/>",[ExrFlags]);
            _   -> ""
        end,

        println(F, "\t<width ival=\"~w\"/>",[Width]), 
        println(F, "\t<height ival=\"~w\"/>",[Height]),
        println(F, "\t<outfile sval=\"~s\"/>",[Outfile]),
        %println(F, "\t<indirect_samples sval=\"0\"/>"), % TO DO: review..
        %println(F, "\t<indirect_power sval=\"1.0\"/>"), %
        println(F, "\t<exposure fval=\"~.10f\"/>",[Exposure]),
        println(F, "\t<save_alpha bval=\"~s\"/>",[format(SaveAlpha)]),
        println(F, "\t<gamma fval=\"~.10f\"/>",[Gamma]),
        

    println(F, "\t<integrator_name sval=\"default\"/>"),

    NThreads = case ThreadsAuto of
        true -> -1;
        false -> [ThreadsNumber]
    end,
    println(F, "\t<threads ival=\"~w\"/>",[NThreads]),


    println(F, "\t<volintegrator_name sval=\"volintegr\"/>"),
    println(F, "</render>").