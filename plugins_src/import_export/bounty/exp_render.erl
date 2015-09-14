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
    println(F, "<raydepth ival=\"~w\"/>",[Raydepth]),
    println(F, "<transpShad bval=\"~s\"/>",[format(TransparentShadows)]),
    println(F, "<shadowDepth ival=\"~w\"/>",[ShadowDepth]),

    case Lighting_Method of
        directlighting ->
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<caustics bval=\"~s\"/>",[UseCaustics]),
            case UseCaustics of
                    true ->            
                        println(F, "<photons ival=\"~w\"/>",[Caustic_Photons]),
                        println(F, "<caustic_depth ival=\"~w\"/>",[Caustic_Depth]),
                        println(F, "<caustic_mix ival=\"~w\"/>",[Caustic_Mix]),
                        println(F, "<caustic_radius fval=\"~.10f\"/>",[Caustic_Radius]);

                    false -> println(F,"")
            end,
            println(F, "<do_AO bval=\"~s\"/>",[Do_AO]),
            case Do_AO of
                    true ->
                        println(F, "\t<AO_distance fval=\"~.10f\"/>",[AO_Distance]),
                        println(F, "<AO_samples fval=\"~.10f\"/>",[AO_Samples]),
                        export_rgb(F, "AO_color",AO_Color);
                    false -> println(F, "")
            end;

        photonmapping ->
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<photons ival=\"~w\"/>",[PM_Diffuse_Photons]),
            println(F, "<bounces ival=\"~w\"/>",[PM_Bounces]),
            println(F, "<search ival=\"~w\"/>",[PM_Search]),
            println(F, "<diffuseRadius fval=\"~.10f\"/>",[PM_Diffuse_Radius]),
            println(F, "<cPhotons ival=\"~w\"/>",[PM_Caustic_Photons]),
            println(F, "<causticRadius fval=\"~.10f\"/>",[PM_Caustic_Radius]),
            println(F, "<caustic_mix ival=\"~w\"/>",[PM_Caustic_Mix]),
            println(F, "<finalGather bval=\"~s\"/>",[PM_Use_FG]),
            println(F, "<fg_bounces ival=\"~w\"/>",[PM_FG_Bounces]),
            println(F, "<fg_samples ival=\"~w\"/>",[PM_FG_Samples]),
            println(F, "<show_map bval=\"~s\"/>",[PM_FG_Show_Map]);
            %println(F," ");

        pathtracing ->
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]),
            println(F, "<photons ival=\"~w\"/>",[PT_Diffuse_Photons]),
            %% TO DO: review case when not caustic is active
            println(F, "<bounces ival=\"~w\"/>",[PT_Bounces]),
            println(F, "<caustic_type sval=\"~s\"/>",[PT_Caustic_Type]),
            println(F, "<caustic_radius fval=\"~.10f\"/>",[PT_Caustic_Radius]),
            println(F, "<caustic_mix ival=\"~w\"/>",[PT_Caustic_Mix]),
            println(F, "<caustic_depth ival=\"~w\"/>",[PT_Caustic_Depth]),
            println(F, "<path_samples ival=\"~w\"/>",[PT_Samples]);

        bidirectional ->
            println(F, "<type sval=\"~s\"/>",[Lighting_Method]);

        sppm ->
            println(F, "<type sval=\"SPPM\"/>"),
            println(F, "<photons ival=\"~w\"/>",[SPPM_Photons]),
            println(F, "<bounces ival=\"~w\"/>",[SPPM_Bounces]),
            println(F, "<searchNum ival=\"~w\"/>",[SPPM_Search]),
            println(F, "<photonRadius fval=\"~.10f\"/>",[SPPM_Radius]),
            println(F, "<times fval=\"~.10f\"/>",[SPPM_Times]),
            println(F, "<passNums ival=\"~w\"/>",[SPPM_Passes]),
            println(F, "<pmIRE bval=\"~s\"/>",[format(SPPM_Ire)])

    end,

    case Lighting_Method of
        {pathtracing,photonmapping,directlighting} ->
            case  UseSSS of
                true ->
                    println(F, "<useSSS bval=\"true\"/>"),
                    println(F, "<sssPhotons ival=\"~w\"/>",[SSS_Photons]),
                    println(F, "<sssDepth ival=\"~w\"/>",[SSS_Depth]),
                    println(F, "<sssScale fval=\"~.10f\"/>",[SSS_Scale]),
                    println(F, "<singleScatterSamples ival=\"~w\"/>",[SSS_SingleScatter_Samples]);

                false -> ""
            end;
        _ -> ""
    end,

    println(F, "</integrator>"),

    case Volintegr_Type of
        none ->
            println(F," "),
            println(F, "<integrator name=\"volintegr\">"),
            println(F, "<type sval=\"~s\"/>",[Volintegr_Type]),
            println(F, "</integrator>"),
            println(F," ");

        singlescatterintegrator ->
            println(F," "),
            println(F, "<integrator name=\"volintegr\">"),
            println(F, "<type sval=\"SingleScatterIntegrator\"/>"),
            println(F, "<adaptive bval=\"~s\"/>",[format(Volintegr_Adaptive)]),
            println(F, "<optimize bval=\"~s\"/>",[format(Volintegr_Optimize)]),
            println(F, "<stepSize fval=\"~.10f\"/>",[Volintegr_Stepsize]),
            println(F, "</integrator>"),
            println(F," ")
    end,

    ExrFlags =
        case RenderFormat of
            exr ->
                [if ExrFlagFloat -> "float "; true -> "" end,
                 if ExrFlagZbuf -> "zbuf "; true -> "" end,
                 format(ExrFlagCompression)];
            _ -> ""
        end,
    println(F, "<render>"),
    println(F, "\t<camera_name sval=\"~s\"/>",[CameraName]),
    println(F, "\t<filter_type sval=\"~s\"/>",[AA_Filter_Type]),
    println(F, "\t<AA_passes ival=\"~w\"/>",[AA_passes]),
    println(F, "\t<AA_threshold fval=\"~.10f\"/>",[AA_threshold]),
    println(F, "\t<AA_minsamples ival=\"~w\"/>",[AA_minsamples]),
    println(F, "\t<AA_pixelwidth fval=\"~.10f\"/>",[AA_pixelwidth]),
    case SaveAlpha of
        premultiply ->
            println(F, "\t<tpremult bval=\"true\"/>");
        backgroundmask ->
                println(F, "\talpha_backgroundmask=\"on\"/~n");
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
        %println(F, "\t<indirect_samples sval=\"0\"/>"
        %println(F, "\t<indirect_power sval=\"1.0\"/>"
        println(F, "\t<exposure fval=\"~.10f\"/>",[Exposure]),
        println(F, "\t<save_alpha bval=\"~s\"/>",[format(SaveAlpha)]),
        %end,
        println(F, "\t<gamma fval=\"~.10f\"/>",[Gamma]),
        
        %case RenderFormat of
        %        tga -> [];
        %        _   -> [format(RenderFormat)]
        %    end,
        %    case RenderFormat of
        %        exr -> [ExrFlags];
        %        _   -> []
        %    end,

    println(F, "<integrator_name sval=\"default\"/>"),

    case ThreadsAuto of
        true -> println(F, "<threads ival=\"-1\"/>");

        false -> println(F, "<threads ival=\"~w\"/>",[ThreadsNumber])

    end,

    println(F, "<volintegrator_name sval=\"volintegr\"/>"),
    println(F, "</render>").