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

export_shader(F, Name, Mat, ExportDir) ->
    %!---------------------------------------------------
    %! Write materials
    %! TODO: review blend material part. ATM don't work
    %!---------------------------------------------------
    MatAttr = proplists:get_value(?TAG, Mat, []),

    MatType = proplists:get_value(material_type, MatAttr, shinydiffuse),

    case MatType of
        shinydiffuse ->
            export_shinydiffuse_shader(F, Name, Mat, ExportDir, MatAttr);

        glossy ->
            export_glossy_shaders(F, Name, Mat, ExportDir, MatAttr);

        translucent ->
            export_translucent_shader(F, Name, Mat, ExportDir, MatAttr);

        glass ->
            export_glass_shaders(F, Name, Mat, ExportDir, MatAttr);

        %lightmat ->
        %    export_lightmat_shader(F, Name, Mat, ExportDir, MatAttr);

        blend_mat ->
            ok
    end.

%
write_material_layers(F, Name, Maps, Attr, Modulators)->
    %!
    %! write each active layer on material modulators
    %!
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
                case export_modulator(F, [Name,$_,format(N)], Maps, M, Attr) of
                    off -> N+1;
                    ok ->  println(F), N+1
                end;
            (_, N) -> N % Ignore old modulators
        end, 1, Modulators).

write_material_textures(F, Name, Maps, ExportDir, Modulators)->
    %!
    %! write each texture
    %!
    foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
          case export_texture(F, [Name,$_,format(N)],Maps, ExportDir, M) of
                      off -> N+1;
                      ok ->  println(F), N+1
                  end;
              (_, N) -> N % Ignore old modulators
          end, 1, Modulators).


export_shinydiffuse_shader(F, Name, Mat, ExportDir, Attr) ->
    %!--------------------------------
    %! Shiny Diffuse Material
    %!--------------------------------
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    %! write all textures associated with this material
    write_material_textures(F, Name, Maps, ExportDir, Modulators),

    println(F, "<material name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"shinydiffusemat\"/>"),

    % for sync viewer object colors (from wings3d color)
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Specular = alpha(proplists:get_value(specular, OpenGL)),

    export_rgb(F, color, proplists:get_value(diffuse_color, Attr, DiffuseA)),
    println(F,
        "\t<diffuse_reflect fval=\"~.10f\"/>",[proplists:get_value(diffuse_reflect, Attr, 1.0)]),

    export_rgb(F, mirror_color, proplists:get_value(mirror_color, Attr, Specular)),
    println(F,
        "\t<specular_reflect fval=\"~.10f\"/>",[proplists:get_value(mirror_reflect, Attr, 0.0)]),

    case proplists:get_value(reflect_mode, Attr, lambert) of
        lambert -> 
            println(F, "\t<diffuse_brdf sval=\"lambert\"/>");
        _ ->
            println(F, "\t<diffuse_brdf sval=\"Oren-Nayar\"/>"),
            println(F, "\t<sigma fval=\"~.10f\"/>",[proplists:get_value(sigma, Attr, 0.1)])
    end,

    println(F,
        "\t<IOR fval=\"~.10f\"/>",[proplists:get_value(ior, Attr, ?DEF_IOR)]), % 1.0 ??
    println(F,
        "\t<fresnel_effect bval=\"~s\"/>",[format(proplists:get_value(fresnel, Attr, false))]),
    println(F,
        "\t<transmit_filter fval=\"~.10f\"/>",[proplists:get_value(transmit_filter, Attr, 1.0)]),
    println(F,
        "\t<translucency fval=\"~.10f\"/>",[proplists:get_value(translucency, Attr, 0.0)]),
    println(F,
        "\t<transparency fval=\"~.10f\"/>",[proplists:get_value(transparency, Attr, 0.0)]),
    println(F,
        "\t<emit fval=\"~.10f\"/>",[proplists:get_value(emit, Attr, 0.0)]),

    %! write each material layer
    write_material_layers(F, Name, Maps, Attr, Modulators),

    println(F, "</material>").


export_glossy_shaders(F, Name, Mat, ExportDir, Attr) ->
    %!------------------------------------
    %! Export Coated Glossy Material
    %!------------------------------------
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    write_material_textures(F, Name, Maps, ExportDir, Modulators),

    GlossType = case proplists:get_value(coated, Attr, false) of
        true -> coated_glossy;
        _ -> glossy
    end,
    println(F, "<material name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~s\"/>", [GlossType]),

    % keep this code for now. Need review
    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Specular = alpha(proplists:get_value(specular, OpenGL)),

    export_rgb(F, color, proplists:get_value(glossy_color, Attr, Specular)),

    export_rgb(F, diffuse_color, proplists:get_value(diffuse_color, Attr, DiffuseA)),
    %
    case GlossType of
        coated_glossy ->
            export_rgb(F, mirror_color, proplists:get_value(glossy_mirror_color, Attr, {0.7, 0.7, 0.7})),
            println(F,
                "\t<IOR fval=\"~.10f\"/>",[proplists:get_value(mirror_ior, Attr, ?DEF_IOR)]);
        _ -> ok
    end,
    case proplists:get_value(reflect_mode, Attr, lambert) of
        lambert ->
            println(F, "\t<diffuse_brdf sval=\"lambert\"/>");
        _ ->
            println(F, "\t<diffuse_brdf sval=\"Oren-Nayar\"/>"), % TODO: check the right value are exporter
            println(F, "\t<sigma fval=\"~.10f\"/>",[proplists:get_value(sigma, Attr, 0.1)])
    end,
    println(F,
        "\t<diffuse_reflect fval=\"~.10f\"/>",[proplists:get_value(diffuse_reflect, Attr, 1.0)]),
    println(F,
        "\t<glossy_reflect fval=\"~.10f\"/>",[proplists:get_value(glossy_reflect, Attr, 0.0)]),
    println(F,
        "\t<anisotropic bval=\"~s\"/>",[proplists:get_value(anisotropic, Attr, false)]),

    case proplists:get_value(anisotropic, Attr, false) of
        true ->
            println(F,
                "\t<exp_u fval=\"~.10f\"/>",[proplists:get_value(anisotropic_u, Attr, 50.0)]),
            println(F,
                "\t<exp_v fval=\"~.10f\"/>",[proplists:get_value(anisotropic_v, Attr, 50.0)]);
        _ ->
            println(F,
                "\t<exponent fval=\"~.10f\"/>",[proplists:get_value(exponent, Attr, ?DEF_EXPONENT)])
    end,

    write_material_layers(F, Name, Maps, Attr, Modulators),

    println(F, "</material>").


export_translucent_shader(F, Name, Mat, ExportDir, Attr) ->
    %!---------------------------------------
    %! Export Translucent (SSS) Material
    %!---------------------------------------
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    write_material_textures(F, Name, Maps, ExportDir, Modulators),

    println(F, "<material name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"translucent\"/>"),

    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefTransmitted = def_transmitted(DiffuseA),

    % SigmaA
    %export_rgb(F, sigmaA, proplists:get_value(sigmaA_color, Attr, ?DEF_SSS_ABSORPTION_COLOR)),    

    export_rgb(F, glossy_color, proplists:get_value(glossy_color, Attr, Specular)),

    export_rgb(F, color, proplists:get_value(diffuse_color, Attr, DefTransmitted)),

    export_rgb(F, specular_color, proplists:get_value(sss_specular_color, Attr, ?DEF_SSS_SPECULAR_COLOR)),
    % need review..
    SSS_AbsorptionColor = proplists:get_value(sigmaA_color, Attr, ?DEF_SSS_ABSORPTION_COLOR),
    case SSS_AbsorptionColor of
        [ ] -> ok;
        {AbsR,AbsG,AbsB} ->
            AbsD = proplists:get_value(absorption_dist, Attr, ?DEF_ABSORPTION_DIST),

            export_rgb(F, sigmaA, {-math:log(max(AbsR, ?NONZERO))/AbsD,
                                   -math:log(max(AbsG, ?NONZERO))/AbsD,
                                   -math:log(max(AbsB, ?NONZERO))/AbsD})
    end,

    export_rgb(F, sigmaS, proplists:get_value(scatter_color, Attr, ?DEF_SCATTER_COLOR)),

    %DispersionPower =  proplists:get_value(dispersion_power, Attr, 1.0),
    %case DispersionPower of
    %    0.0 -> ok;
    %    _   ->
    %        println(F,
    %            "\t<dispersion_samples ival=\"~w\"/>",[proplists:get_value(dispersion_samples, Attr, 16)]),
    %        println(F,
    %            "\t<dispersion_jitter bval=\"~s\"/>", [format(proplists:get_value(dispersion_jitter, Attr, false))])
    %end,
    
    println(F, "\t<IOR fval=\"~.10f\"/>",[proplists:get_value(sss_ior, Attr, 1.3)]),
    println(F,
        "\t<sigmaS_factor fval=\"~.10f\"/>",[proplists:get_value(sigmas_factor, Attr, 1.0)]),
    println(F,
        "\t<diffuse_reflect fval=\"~.10f\"/>",[proplists:get_value(diffuse_reflect, Attr, 1.0)]),
    println(F,
        "\t<glossy_reflect fval=\"~.10f\"/>",[proplists:get_value(glossy_reflect, Attr, 0.0)]),
    println(F,
        "\t<sss_transmit fval=\"~.10f\"/>",[proplists:get_value(scatter_transmit, Attr, 0.75)]),
    println(F,
        "\t<exponent fval=\"~.10f\"/>",[proplists:get_value(specular_factor, Attr, 500)]),
    println(F,
        "\t<g fval=\"~.10f\"/>",[proplists:get_value(sss_phase, Attr, 0.0)]),

    write_material_layers(F, Name, Maps, Attr, Modulators),

    println(F, "</material>").


export_glass_shaders(F, Name, Mat, ExportDir, Attr) ->
    %!----------------------------------
    %! glass and rough glass materials
    %!----------------------------------
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    write_material_textures(F, Name, Maps, ExportDir, Modulators),
    
    GlassType = case proplists:get_value(rough, Attr, false) of
        true -> rough_glass;
        _ -> glass
    end,
    println(F, "<material name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~s\"/>", [GlassType]),

    DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Specular = alpha(proplists:get_value(specular, OpenGL)),
    DefReflected = Specular,
    DefTransmitted = def_transmitted(DiffuseA),

    export_rgb(F, mirror_color, proplists:get_value(reflected, Attr, DefReflected)),

    export_rgb(F, filter_color, proplists:get_value(transmitted, Attr, DefTransmitted)),

    export_rgb(F, absorption, proplists:get_value(absorption_color, Attr, {0.9, 0.9, 0.9})),

    println(F, "\t<absorption_dist fval=\"~.10f\"/>",[proplists:get_value(absorption_dist, Attr, 1.0)]),
    println(F, "\t<transmit_filter fval=\"~.10f\"/>",[proplists:get_value(transmit_filter, Attr, 1.0)]),

    case GlassType of
        rough_glass ->
            println(F, "\t<alpha fval=\"~.10f\"/>",[proplists:get_value(roughness, Attr, 0.20)]);
        _ -> ok
    end,

    println(F,
        "\t<dispersion_power fval=\"~.10f\"/>",[proplists:get_value(dispersion_power, Attr, 0.0)]),
    println(F,
        "\t<IOR fval=\"~.10f\"/>",[proplists:get_value(glass_ior, Attr, ?DEF_IOR)]),
    println(F,
        "\t<fake_shadows bval=\"~s\"/>",[format(proplists:get_value(fake_shadows, Attr, false))]),

    % loop for export modulators / layers
    write_material_layers(F, Name, Maps, Attr, Modulators),

    println(F, "</material>").

%%% Export Light Material%%%

export_lightmat_shader(F, Name, Mat, ExportDir, Attr) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat, []),
    Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),

    % unsure if this material allow textures..
    write_material_textures(F, Name, Maps, ExportDir, Modulators),

    println(F, "<material name=\"~s\">",[Name]),
    println(F, "<type sval=\"light_mat\"/>"),

    _DiffuseA = {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),

    DefLightmatColor = def_lightmat_color(proplists:get_value(diffuse, OpenGL)),

    Lightmat_Color = proplists:get_value(lightmat_color, Attr, DefLightmatColor),

    export_rgb(F, color, proplists:get_value(lightmat_color, Attr, Lightmat_Color)),

    println(F, "\t<power fval=\"~.10f\"/>",[proplists:get_value(lightmat_power, Attr, 1.0)]),

    % unsure about if this material allow layers..
    write_material_layers(F, Name, Maps, Attr, Modulators),

    println(F, "</material>").

%%% Start Blend Materials Export

export_shaderblend(F, Name, Mat, ExportDir) ->
    Attr = proplists:get_value(?TAG, Mat, []),

    DefaultMaterialType = get_pref(default_material_type, Attr),

    MatType = proplists:get_value(material_type, Attr, DefaultMaterialType),

    case MatType of
        blend_mat ->
            export_blend_mat_shader(F, Name, Mat, ExportDir, Attr);

        _ -> ok
    end.

%%% Export Blend Material
%% need reviewing, render crash.

export_blend_mat_shader(F, Name, Mat, ExportDir, Attr) ->
    %OpenGL = proplists:get_value(opengl, Mat),
    %Maps = proplists:get_value(maps, Mat, []),
    %Modulators = proplists:get_value(modulators, Attr, def_modulators(Maps)),
    %foldl(fun ({modulator,Ps}=M, N) when is_list(Ps) ->
    %              case export_texture(F, [Name,$_,format(N)],
    %                                  Maps, ExportDir, M) of
    %                  off -> N+1;
    %                  ok ->
    %                      println(F),
    %                      N+1
    %              end;
    %          (_, N) ->
    %              N % Ignore old modulators
    %      end, 1, Modulators),

    println(F, "<material name=\"~s\">",[Name]),

    println(F, "\t<type sval=\"blend_mat\"/>"),

    println(F,
        "\t<material1 sval=\"""w_""\~s\"/>",[proplists:get_value(blend_mat1, Attr, "blendone")]),
    println(F,
        "\t<material2 sval=\"""w_""\~s\"/>",[proplists:get_value(blend_mat2, Attr, "blendtwo")]),
    println(F,
        "\t<blend_value fval=\"~.4f\"/>",[proplists:get_value(blend_value, Attr, 0.5)]),

    println(F, "</material>").
