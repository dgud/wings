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

%
%  Export material shader modulators
%  TO DO:
%  Finish some parts related with Stencil modes.

%  Done!!
%  Review texture projection like XYZ, or XZY.
%  Fix blend material texture mask layer.

export_modulator(F, Texname, Maps, {modulator,Ps}, Attr) when is_list(Ps) ->
    %
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,_BlendMode,_TexType} ->
            %!-----------------------------------
            %! TODO: add the rest of layers
            %!-----------------------------------
            DiffuseLayer = proplists:get_value(diffuse_layer, Ps, false),
            MirrorLayer = proplists:get_value(mirror, Ps, false),
            MirrorColorLayer = proplists:get_value(mirror_color_layer, Ps, false),
            TransparentLayer = proplists:get_value(transparency, Ps, false),
            TranslucentLayer = proplists:get_value(translucency, Ps, false),
            GlossyLayer = proplists:get_value(glossy, Ps, false),
            GlossyReflectLayer = proplists:get_value(glossy_reflect, Ps, false),
            BumpLayer = proplists:get_value(bump, Ps, false),
            BlendLayer = proplists:get_value(blend_mask, Ps, false),
    
            % cases...
            case DiffuseLayer of
                true  -> export_modulators(F, Texname, Maps, diffuse_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case MirrorLayer of
                true  -> export_modulators(F, Texname, Maps, mirror_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case MirrorColorLayer of
                true  -> export_modulators(F, Texname, Maps, mirror_color_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case TransparentLayer of
                true  -> export_modulators(F, Texname, Maps, transparency_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case TranslucentLayer of
                true  -> export_modulators(F, Texname, Maps, translucency_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case GlossyLayer  of
                true  -> export_modulators(F, Texname, Maps, glossy_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case GlossyReflectLayer of
                true  -> export_modulators(F, Texname, Maps, glossy_reflect_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case BumpLayer of 
                true  -> export_modulators(F, Texname, Maps, bump_shader, {modulator,Ps}, Attr);
                false -> ok
            end,
            case BlendLayer of 
                true  -> export_modulators(F, Texname, Maps, mask, {modulator,Ps}, Attr);
                false -> ok
            end,

            % when all layers are written , then mapping
            mapping_textures(F, Texname, Maps, BumpLayer, {modulator,Ps}, Attr)
    end.

% 
export_modulators(F, Texname, Maps, LayerType, {modulator,Ps}, Attr) ->
    %
    {_Enable,BlendMode,TexType} = mod_enabled_mode_type(Ps, Maps),

    DoColor =
        case LayerType of
            diffuse_shader ->  true;
            glossy_shader ->  true;
            mirror_color_shader -> true;
            _ -> false
        end,
    % for blend mat: <mask sval="mask_layer0"/>
    println(F, "\t<~s sval=\"~s_layer\"/>",[LayerType,LayerType]),

    %!----------------------------------------------------
    %! entry to element shader list
    %!----------------------------------------------------
    println(F,"\t<list_element>"),

    %! layer factor amount controlled with 'Factor Modulator' slider in UI
    %! TODO: lack some 'cases', for translucent SSS 
    Factor =
        case LayerType of
            diffuse_shader ->        proplists:get_value(diffuse_factor, Ps, 1.0);
                
            mirror_shader ->         proplists:get_value(mirror_factor, Ps, 1.0);
                
            mirror_color_shader ->   proplists:get_value(mirror_color_factor, Ps, 1.0);
                
            transparency_shader ->   proplists:get_value(transparent_factor, Ps, 1.0);
                
            translucency_shader ->   proplists:get_value(translucent_factor, Ps, 1.0);
            
            glossy_shader ->         proplists:get_value(glossy_factor, Ps, 1.0);
            
            glossy_reflect_shader -> proplists:get_value(glossy_reflect_factor, Ps, 1.0);
                
            bump_shader ->           proplists:get_value(bump_factor, Ps, 1.0);
           
            mask ->                  proplists:get_value(blend_factor, Ps, 1.0);
                
            _ -> proplists:get_value(def_value, Ps, 1.0)
        end,
    %% Try use value or color
    FactorType =
        case DoColor of
            true -> "colfac";
            _ ->    "valfac"
        end,
    %
    println(F,"\t\t<~s fval=\"~w\"/>",[FactorType,Factor]),
    %
    CellType = proplists:get_value(cell_type, Ps, intensity),
    %%--------------------------->
    IsColor = % revise
        case TexType of
            image -> true;
            {map,_} -> true;
            voronoi ->
                case CellType of
                    intensity -> true;
                    _ -> false % maybe this case.. fail
                end;
            _ -> false
        end,

    println(F,"\t\t<color_input bval=\"~s\"/>",[IsColor]),

    % default color for 'blending'
    {DefaultR, DefaultG, DefaultB} = proplists:get_value(def_color, Ps, {1.0,0.0,1.0}),

    println(F,"\t\t<def_col r=\"~w\" g=\"~w\" b=\"~w\" a=\"1\"/>",[DefaultR, DefaultG, DefaultB]),
    %
    println(F,"\t\t<def_val fval=\"~w\"/>",[proplists:get_value(def_value, Ps, 1.0)]),

    % swich to use texture values
    %
    println(F,"\t\t<do_color bval=\"~s\"/>",[DoColor]),
    %
    println(F,"\t\t<do_scalar bval=\"~s\"/>",[not DoColor]),
    %
    println(F,"\t\t<element sval=\"shader_node\"/>"),
    %
    println(F,"\t\t<input sval=\"~s_map\"/>",[Texname]),
    %
    ModeNumber =
        case BlendMode of
            %mix -> "0"; 
            add -> 1; mul -> 2; sub -> 3; scr -> 4;
            divide -> 5; dif -> 6; dar -> 7; lig -> 8;
            _ -> 0
        end,
    println(F, "\t\t<mode ival=\"~w\"/>",[ModeNumber]),
    %
    println(F, "\t\t<name sval=\"~s_layer\"/>",[LayerType]), % use the layer type name
    %
    println(F, "\t\t<negative bval=\"~s\"/>",[proplists:get_value(negative, Ps, false)]),
    %
    println(F, "\t\t<noRGB bval=\"~s\"/>",[proplists:get_value(no_rgb, Ps, false)]),

    Stencil = proplists:get_value(stencil, Ps, false),

    println(F, "\t\t<stencil bval=\"~s\"/>",[Stencil]),
    % TODO: create code for stencil modes

    %!----------------------
    %! Set upper color, WIP
    %!----------------------
    {UpR, UpG, UpB} =
        case LayerType of
            diffuse ->
                proplists:get_value(diffuse_color, Attr, {0.0,0.0,0.0});
            mirror_color ->
                proplists:get_value(mirror_color, Attr, {0.0,0.0,0.0});
            _ -> {0.0,0.0,0.0}
        end,
    % TODO: create code for stencil and blending modes
    Ulayer = "",
    %
    case Ulayer of
        "" ->
            case DoColor of
                true ->
                    println(F,
                        "\t\t<upper_color r=\"~.10f\" g=\"~.10f\" b=\"~.10f\" a=\"1\"/>\n"
                        "\t\t<upper_value fval=\"0\"/>",[UpR, UpG, UpB]);
                false ->
                    println(F,
                        "\t\t<upper_color r=\"0\" g=\"0\" b=\"0\" a=\"1\"/>\n"
                        "\t\t<upper_value fval=\"~w\"/>",[Factor])
            end;
        _ ->
            println(F,"\t\t<upper_layer sval=\"~s\"/>",[Ulayer])
    end,

    println(F, "\t\t<type sval=\"layer\"/>"),

    % TODO: Atm, use IsColor value, but need review
    println(F, "\t\t<use_alpha bval=\"~s\"/>",[IsColor]),

    % close list element shader
    println(F, "\t</list_element>").


mapping_textures(F, Texname, _Maps, BumpLayer, {modulator,Ps}, _Attr)->
    %!-----------------------------
    %! mapping texture layers
    %!-----------------------------
    println(F, "\t<list_element>"),
    println(F, "\t\t<element sval=\"shader_node\"/>"),

    % projection mapping coordinates
    println(F,
        "\t\t<mapping sval=\"~s\"/>",[proplists:get_value(projection, Ps, plain)]),
    println(F,
        "\t\t<name sval=\"~s_map\"/>",[Texname]), % same name of 'input' in 'list_elements'
    %
    OffsetX = proplists:get_value(offset_x, Ps, 0.0),
    OffsetY = proplists:get_value(offset_y, Ps, 0.0),
    OffsetZ = proplists:get_value(offset_z, Ps, 0.0),
    %
    println(F, "\t\t<offset x=\"~w\" y=\"~w\" z=\"~w\"/>",[OffsetX, OffsetY, OffsetZ]),
    % 
    Direction = proplists:get_value(direction, Ps, xyz),
    ProjectionX = case Direction of
        xyz -> 1; xzy -> 1; yxz -> 2; zxy -> 2; _ -> 3 end,
    ProjectionY = case Direction of
        yxz -> 1; yzx -> 1; xyz -> 2; zyx -> 2; _ -> 3 end,
    ProjectionZ = case Direction of
        zxy -> 1; zyx -> 1; xzy -> 2; yzx -> 2; _ -> 3 end,
        
    println(F, "\t\t<proj_x ival=\"~w\"/>",[ProjectionX]),
    println(F, "\t\t<proj_y ival=\"~w\"/>",[ProjectionY]),
    println(F, "\t\t<proj_z ival=\"~w\"/>",[ProjectionZ]),

    % for scale texture mapping
    SizeX = proplists:get_value(size_x, Ps, 1.0),
    SizeY = proplists:get_value(size_y, Ps, 1.0),
    SizeZ = proplists:get_value(size_z, Ps, 1.0),
    %
    println(F, "\t\t<scale x=\"~w\" y=\"~w\" z=\"~w\"/>",[SizeX, SizeY, SizeZ]),

    % texture coordinates type
    println(F, "\t\t<texco sval=\"~s\"/>",[proplists:get_value(coordinates, Ps, global)]),
    println(F, "\t\t<texture sval=\"~s\"/>",[Texname]),
    println(F, "\t\t<type sval=\"texture_mapper\"/>"),
    % if bump map..
    case BumpLayer of
        true ->
            Factor = proplists:get_value(bump_factor, Ps, 1.0),
            println(F, "\t\t<bump_strength fval=\"~w\"/>",[Factor*5]);
        _ -> ok
    end,
    % close mapper element
    println(F, "\t</list_element>").

