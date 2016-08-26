%
%
%
%
export_shader(F, Name, Mat, ExportDir) ->

    Attr = proplists:get_value(?TAG, Mat, []),

    DefShaderType = get_pref(shader_type, Attr),

    ShaderType = proplists:get_value(shader_type, Attr, DefShaderType),

    case ShaderType of

        shinydiffuse ->
            export_shinydiffuse_shader(F, Name, Mat, ExportDir, Attr);
        glossy ->
            export_glossy_shader(F, Name, Mat, ExportDir, Attr);
        coatedglossy ->
            export_glossy_shader(F, Name, Mat, ExportDir, Attr);
        translucent ->
            export_translucent_shader(F, Name, Mat, ExportDir, Attr);
        glass ->
            export_glass_shader(F, Name, Mat, ExportDir, Attr);
        lightmat ->
            export_lightmat_shader(F, Name, Mat, ExportDir, Attr);
        rough_glass ->
            export_rough_glass_shader(F, Name, Mat, ExportDir, Attr);

        blend_mat ->
            ok

    end.
%!-----------------------%
%! Shiny Diffuse Material
%!-----------------------%
export_shinydiffuse_shader(F, Name, Mat, ExportDir, Attr) ->

    OpenGL = proplists:get_value(opengl, Mat),

    Maps = proplists:get_value(maps, Mat, []),

    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_texture(F, [Name,$_,format(N)], Maps, ExportDir, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F,
        "<material name=\"~s\">~n"
        "\t<type sval=\"shinydiffusemat\"/>", [Name]),

    % DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    DiffuseA = proplists:get_value(diffuse, OpenGL),    
    DefReflected = alpha(proplists:get_value(specular, OpenGL)),
    DefTransmitted = def_transmitted(DiffuseA),
    
    export_rgb(F, mirror_color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, color, proplists:get_value(transmitted, Attr, DefTransmitted)),        

    OrenNayar = proplists:get_value(oren_nayar, Attr, ?DEF_OREN_NAYAR),

    case OrenNayar of
        false -> ok;
        _ ->
            println(F,
                "\t<diffuse_brdf sval=\"oren_nayar\"/>\n"
                "\t<sigma fval=\"~.6f\"/>",[proplists:get_value(oren_nayar_sigma, Attr, 0.1)])
    end,

    Fresnel =  proplists:get_value(tir, Attr, ?DEF_TIR),
    println(F, "\t<fresnel_effect bval=\"~s\"/>",[format(Fresnel)]),
    case Fresnel of
        true ->
            println(F,
                "\t<IOR fval=\"~.6f\"/>", [proplists:get_value(ior, Attr, 1.0)]);
        false -> ok
    end,

    println(F,
        "\t<transmit_filter fval=\"~.6f\"/>",[proplists:get_value(transmit_filter, Attr, 1.0)]),
    println(F,
        "\t<translucency fval=\"~.6f\"/>",[proplists:get_value(translucency, Attr, 0.0)]),
    println(F,
        "\t<transparency fval=\"~.6f\"/>",[proplists:get_value(transparency, Attr, 0.0)]),
    println(F,
        "\t<diffuse_reflect fval=\"~.10f\"/>",[proplists:get_value(diffuse_reflect, Attr, 1.0)]),
    println(F,
        "\t<specular_reflect fval=\"~.10f\"/>",[proplists:get_value(specular_reflect, Attr, 0.0)]),
    println(F,
        "\t<emit fval=\"~.10f\"/>",[proplists:get_value(emit, Attr, 0.0)]),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
            case export_modulator(F, [Name,$_,format(N)], Maps, M) of
                    off -> N+1;
                    ok ->
                        println(F),
                        N+1
                end;
            (_, N) ->
                N % Ignore old modulators
        end, 1, Modulators),

    println(F, "</material>").

%!-------------------------------------%
%! Glossy material ( normal or coated )
%!-------------------------------------%
export_glossy_shader(F, Name, Mat, ExportDir, Attr) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_texture(F, [Name,$_,format(N)],Maps, ExportDir, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),
    
    ShaderType = proplists:get_value(shader_type, Attr, ?DEF_SHADER_TYPE),

    println(F,
        "<material name=\"~s\">\n",[Name]),
    println(F,
        "\t<type sval=\"~s\"/>", [ShaderType]),

    % povman TODO: review this part --->
    % DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    DiffuseA =  proplists:get_value(diffuse, OpenGL),

    DefReflected = alpha(proplists:get_value(specular, OpenGL)),

    DefTransmitted = def_transmitted(DiffuseA),

    export_rgb(F, color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, diffuse_color, proplists:get_value(transmitted, Attr, DefTransmitted)),

    % coated..
    ShaderType = proplists:get_value(shader_type, Attr, ?DEF_SHADER_TYPE),
    %
    case ShaderType of
        coatedglossy ->
            export_rgb(F, mirror_color, proplists:get_value(mirror, Attr, ?DEF_MIRROR_COLOR)),

            println(F, "\t<IOR fval=\"~.6f\"/>\n",[proplists:get_value(ior, Attr, 1.0)]);
        _ -> ok
    end,

    println(F,
        "\t<diffuse_reflect fval=\"~.6f\"/>",[proplists:get_value(diffuse_reflect, Attr, 1.0)]),
    println(F,
        "\t<glossy_reflect fval=\"~.6f\"/>",[proplists:get_value(glossy_reflect, Attr, 0.1)]),

    Anisotropic = proplists:get_value(anisotropic, Attr, false),

    println(F, "\t<anisotropic bval=\"~s\"/>", [Anisotropic]),

    case Anisotropic of
        true ->
            println(F, 
                "\t<exp_u fval=\"~.6f\"/>",[proplists:get_value(anisotropic_u, Attr, 50.0)]),
            println(F, 
                "\t<exp_v fval=\"~.6f\"/>",[proplists:get_value(anisotropic_v, Attr, 50.0)]);
        _ ->
            println(F,
                "\t<exponent fval=\"~.6f\"/>",[proplists:get_value(exponent, Attr, ?DEF_EXPONENT)])
    end,

    OrenNayar = proplists:get_value(oren_nayar, Attr, ?DEF_OREN_NAYAR),

    case OrenNayar of
        false -> ok;
        _ ->
            println(F,
                "\t<diffuse_brdf sval=\"oren_nayar\"/>\n"
                "\t<sigma fval=\"~.6f\"/>",[proplists:get_value(oren_nayar_sigma, Attr, 0.1)])
    end,
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_modulator(F, [Name,$_,format(N)], Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F, "</material>").

%!-------------------%
%! Translucent shader
%!-------------------%
export_translucent_shader(F, Name, Mat, ExportDir, Attr) ->

    OpenGL = proplists:get_value(opengl, Mat),

    Maps = proplists:get_value(maps, Mat, []),

    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_texture(F, [Name,$_,format(N)], Maps, ExportDir, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),
    
    println(F,
        "<material name=\"~s\">\n"++
        "\t<type sval=\"translucent\"/>", [Name]),

    DiffuseA = proplists:get_value(diffuse, OpenGL),

    Specular = alpha(proplists:get_value(specular, OpenGL)),

    DefReflected = Specular,

    DefTransmitted = def_transmitted(DiffuseA),

    SSS_AbsorptionColor = proplists:get_value(sss_absorption_color, Attr, ?DEF_SSS_ABSORPTION_COLOR),

    ScatterColor = proplists:get_value(scatter_color, Attr, ?DEF_SCATTER_COLOR),

    SSS_Specular_Color = proplists:get_value(sss_specular_color, Attr, ?DEF_SSS_SPECULAR_COLOR),

    export_rgb(F, glossy_color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, color, proplists:get_value(transmitted, Attr, DefTransmitted)),

    export_rgb(F, specular_color, proplists:get_value(sss_specular_color, Attr, SSS_Specular_Color)),

    case SSS_AbsorptionColor of
        [ ] -> ok;
        {AbsR,AbsG,AbsB} ->
            AbsD = proplists:get_value(absorption_dist, Attr, ?DEF_ABSORPTION_DIST),

            export_rgb(F, sigmaA, {-math:log(max(AbsR, ?NONZERO))/AbsD,
                                   -math:log(max(AbsG, ?NONZERO))/AbsD,
                                   -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,

    export_rgb(F, sigmaS, proplists:get_value(scatter_color, Attr, ScatterColor)),

    println(F,
        "\t<IOR fval=\"~.10f\"/>",[proplists:get_value(ior, Attr, ?DEF_IOR)]),
    println(F,
        "\t<sigmaS_factor fval=\"~.10f\"/>",[proplists:get_value(sigmas_factor, Attr, ?DEF_SIGMAS_FACTOR)]),
    println(F,
        "\t<diffuse_reflect fval=\"~.10f\"/>",[proplists:get_value(diffuse_reflect, Attr, ?DEF_DIFFUSE_REFLECT)]),
    println(F,
        "\t<glossy_reflect fval=\"~.10f\"/>",[proplists:get_value(glossy_reflect, Attr, ?DEF_GLOSSY_REFLECT)]),
    println(F,
        "\t<sss_transmit fval=\"~.10f\"/>",[proplists:get_value(sss_translucency, Attr, ?DEF_SSS_TRANSLUCENCY)]),
    println(F,
        "\t<exponent fval=\"~.10f\"/>",[proplists:get_value(exponent, Attr, ?DEF_EXPONENT)]),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_modulator(F, [Name,$_,format(N)], Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F, "</material>").

%!---------------%
%! Glass Material
%!---------------%
export_glass_shader(F, Name, Mat, ExportDir, Attr) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    %%
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_texture(F, [Name,$_,format(N)],
                            Maps, ExportDir, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F,
        "<material name=\"~s\">~n"
        "\t<type sval=\"glass\"/>",
        [Name]),

    % DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    DiffuseA = proplists:get_value(diffuse, OpenGL),

    DefReflected = alpha(proplists:get_value(specular, OpenGL)),

    DefTransmitted = def_transmitted(DiffuseA),

    export_rgb(F, mirror_color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, filter_color, proplists:get_value(transmitted, Attr, DefTransmitted)),

    Glass_IR_Depth = proplists:get_value(glass_ir_depth, Attr, 3),

    TransmitFilter = proplists:get_value(transmit_filter, Attr, ?DEF_TRANSMIT_FILTER),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor = proplists:get_value(absorption_color, Attr, DefAbsorptionColor),

    case AbsorptionColor of
        [ ] -> ok;
        {_AbsR,_AbsG,_AbsB} ->
            AbsD = proplists:get_value(absorption_dist, Attr, ?DEF_ABSORPTION_DIST),
            %%
            export_rgb(F, absorption, proplists:get_value(absorption_color, Attr, AbsorptionColor)),

            println(F,
                "\t<absorption_dist fval=\"~.10f\"/>",[AbsD]),
            println(F,
                "\t<transmit_filter fval=\"~.10f\"/>",[TransmitFilter])
    end,
    DispersionPower = proplists:get_value(dispersion_power, Attr, 1.0),
    %%
    case DispersionPower of
        0.0 -> ok;
        _ ->
            println(F,
                "\t<dispersion_power fval=\"~.6f\"/>",[DispersionPower]),
            println(F,
                "\t<dispersion_samples ival=\"~w\"/>",[proplists:get_value(dispersion_samples, Attr, 8)])
    end,

    FakeShadows = proplists:get_value(fake_shadows, Attr, ?DEF_FAKE_SHADOWS),
    
    println(F,
        "\t<IOR fval=\"~.6f\"/>",[proplists:get_value(ior, Attr, 1.0)]),
    println(F,
        "\t<glass_internal_reflect_depth ival=\"~w\"/>\n"
        "\t<fake_shadows bval=\"~s\"/>",
        [Glass_IR_Depth,format(FakeShadows)]),

    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_modulator(F, [Name,$_,format(N)],Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F, "</material>").
    
%!----------------------------%
%! Export Rough Glass Material
%!----------------------------%
export_rough_glass_shader(F, Name, Mat, ExportDir, Attr) ->

    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
            case export_texture(F, [Name,$_,format(N)], Maps, ExportDir, M) of
                    off -> N+1;
                    ok ->
                        println(F),
                        N+1
                end;
            (_, N) ->
                N % Ignore old modulators
        end, 1, Modulators),

    println(F,
        "<material name=\"~s\">\n"
        "\t<type sval=\"rough_glass\"/>",
        [Name]),

    % DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    DiffuseA =  proplists:get_value(diffuse, OpenGL),

    DefReflected = alpha(proplists:get_value(specular, OpenGL)),

    DefTransmitted = def_transmitted(DiffuseA),

    export_rgb(F, mirror_color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, filter_color, proplists:get_value(transmitted, Attr, DefTransmitted)),

    DefAbsorptionColor = def_absorption_color(proplists:get_value(diffuse, OpenGL)),

    AbsorptionColor = proplists:get_value(absorption_color, Attr, DefAbsorptionColor),

    case AbsorptionColor of
        [ ] -> ok;
        {_AbsR,_AbsG,_AbsB} ->
            
            export_rgb(F, absorption, proplists:get_value(absorption_color, Attr, AbsorptionColor)),

            println(F,
                "<absorption_dist fval=\"~.6f\"/>",[proplists:get_value(absorption_dist, Attr, 3.0)]),
            println(F,
                "\t<transmit_filter fval=\"~.6f\"/>",[proplists:get_value(transmit_filter, Attr, 1.0)]),
            println(F,
                "\t<roughness fval=\"~.6f\"/>",[proplists:get_value(roughness, Attr, ?DEF_ROUGHNESS)])
    end,

    DispersionPower = proplists:get_value(dispersion_power, Attr, ?DEF_DISPERSION_POWER),
    case DispersionPower of
        0.0 -> ok;
        _ ->
            println(F,
                "\t<dispersion_power fval=\"~.6f\"/>",[DispersionPower]),
            println(F,
                "\t<dispersion_samples ival=\"~w\"/>",[proplists:get_value(dispersion_samples, Attr, 8)])
    end,

    println(F,
        "\t<IOR fval=\"~.10f\"/>", [proplists:get_value(ior, Attr, ?DEF_IOR)]),
    println(F,
        "\t<fake_shadows bval=\"~s\"/>",
        [format(proplists:get_value(fake_shadows, Attr, false))]),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_modulator(F, [Name,$_,format(N)], Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),
    println(F, "</material>").

%!----------------------%
%! Export Light Material
%!----------------------%
export_lightmat_shader(F, Name, Mat, ExportDir, Attr) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_texture(F, [Name,$_,format(N)], Maps, ExportDir, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),
    println(F,
        "<material name=\"~s\">\n"
        "\t<type sval=\"light_mat\"/>",[Name]),

    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),

    Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),

    export_rgb(F, color, proplists:get_value(lightmat_color, Attr, Lightmat_Color)),

    println(F,
        "\t<power fval=\"~.6f\"/>", [proplists:get_value(lightmat_power, Attr, 1.0)]),
    println(F,
        "\t<double_sided bval=\"true\"/>"),
    %
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        % fix for Opacity crash
        case export_modulator(F, [Name,$_,format(N)], Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F, "</material>").

%!-----------------------%
%! Blend Materials Export
%!-----------------------%

export_shaderblend(F, Name, Mat, ExportDir) ->

    Attr = proplists:get_value(?TAG, Mat, []),

    DefShaderType = get_pref(shader_type, Attr),

    ShaderType = proplists:get_value(shader_type, Attr, DefShaderType),

    case ShaderType of
        blend_mat ->
            export_blend_mat_shader(F, Name, Mat, ExportDir, Attr);
        _ -> ok
    end.

%%% Export Blend Material

export_blend_mat_shader(F, Name, Mat, ExportDir, Attr) ->
    %OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    %%
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
            case export_texture(F, [Name,$_,format(N)], Maps, ExportDir, M) of
                off -> N+1;
                ok ->
                    println(F),
                    N+1
                end;
            (_, N) ->
                N % Ignore old modulators
        end, 1, Modulators),

    println(F,
        "<material name=\"~s\">\n"
        "\t<type sval=\"blend_mat\"/>",[Name]),

    println(F,
        "\t<material1 sval=\"""w_""\~s\"/>",[proplists:get_value(blend_mat1, Attr, ?DEF_BLEND_MAT1)]),
    println(F,
        "\t<material2 sval=\"""w_""\~s\"/>",[proplists:get_value(blend_mat2, Attr, ?DEF_BLEND_MAT2)]),
    println(F,
        "\t<blend_value fval=\"~.10f\"/>",[proplists:get_value(blend_value, Attr, 0.5)]),

    % povman : not have modulators for blendmaterial. only an 'special' case for texture mask
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
        case export_modulator(F, [Name,$_,format(N)], Maps, M) of
            off -> N+1;
            ok ->
                println(F),
                N+1
        end;
        (_, N) ->
            N % Ignore old modulators
    end, 1, Modulators),

    println(F, "</material>").
