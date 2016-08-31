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
%! UI modulators options
%! work in progress.
%!


menu_blend_mode() ->
    [{?__(105,"Mix"),mix},
     {?__(106,"Add"),add},
     {?__(107,"Multiply"),mul},
     {?__(109,"Subtract"),sub},
     {?__(110,"Screen"),scr},
     {?__(111,"Divide"),divide},
     {?__(112,"Difference"),dif},
     {?__(113,"Darken"),dar},
     {?__(114,"Lighten"),lig}].

menu_distortion_type() ->
   [{?__(87,"Blender-Distort"),blender},
    {?__(88,"Cellnoise"),cellnoise},
    {?__(89,"New Perlin"),newperlin},
    {?__(90,"Perlin"),stdperlin},
    {?__(91,"Voronoi Crackle"),voronoi_crackle},
    {?__(92,"Voronoi F1"),voronoi_f1},
    {?__(93,"Voronoi F2"),voronoi_f2},
    {?__(94,"Voronoi F3"),voronoi_f3},
    {?__(95,"Voronoi F4"),voronoi_f4},
    {?__(96,"Voronoi F1F2"),voronoi_f2f1}].

% TODO: change this part of code for generate only the
% allowed modulators for each material
%
modulator_dialogs(Modulators0, Maps) ->
    ModCount = length(Modulators0),
    Modulators =
        if (ModCount < 4) ->
            Modulators0 ++ modulator_add(4-ModCount);
        true -> Modulators0
        end,
    [{oframe, modulator_dialogs(Modulators, Maps, 1), 1, [{style, buttons}]}].

modulator_dialogs([], _Maps, _M) -> [];
modulator_dialogs([Modulator|Modulators], Maps, M) ->
    modulator_dialog(Modulator, Maps, M)++
    modulator_dialogs(Modulators, Maps, M+1).

modulator_dialog({modulator,Ps}, Maps, M) when is_list(Ps) ->
    {Enabled,BlendMode,TextureType} = mod_enabled_mode_type(Ps, Maps),
    %AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),
    %ShaderType = proplists:get_value(shader_type, Ps, ?DEF_SHADER_TYPE), % only use for logical UI
    ShaderType = proplists:get_value(material_type, Ps, diffuse),

    Stencil = proplists:get_value(stencil, Ps, false),
    Negative = proplists:get_value(negative, Ps, false),
    NoRGB = proplists:get_value(no_rgb, Ps, false),
    DefColor = proplists:get_value(def_color, Ps, ?DEF_MOD_DEFCOLOR),
    DefValue = proplists:get_value(def_value, Ps, 1.0),

    Coordinates = proplists:get_value(coordinates, Ps, global),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Projection = proplists:get_value(projection, Ps, plain),
    OffsetX = proplists:get_value(offset_x, Ps, 0.0),
    OffsetY = proplists:get_value(offset_y, Ps, 0.0),
    OffsetZ = proplists:get_value(offset_z, Ps, 0.0),

    % povman: Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
    DiffuseLayer = proplists:get_value(diffuse, Ps, false),
    DiffuseFactor = proplists:get_value(diffuse_factor, Ps, 1.0),
    MirrorLayer = proplists:get_value(mirror, Ps, false),
    MirrorFactor = proplists:get_value(mirror_factor, Ps, 1.0),
    MirrorColorLayer = proplists:get_value(mirror_color, Ps, false),
    MirrorColorFactor = proplists:get_value(mirror_color_factor, Ps, 1.0),
    TransparentLayer = proplists:get_value(transparency, Ps, false),
    TransparentFactor = proplists:get_value(transparent_factor, Ps, 1.0),
    TranslucentLayer = proplists:get_value(translucency, Ps, false),
    TranslucentFactor = proplists:get_value(translucent_factor, Ps, 1.0),
    BumpLayer = proplists:get_value(bump, Ps, false),
    BumpFactor = proplists:get_value(bump_factor, Ps, 1.0),
    %%    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    %Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    %Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
    Filename = proplists:get_value(image_filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type, open_dialog},
                    {extensions, [{".jpg", "JPEG"}, {".png", "PNG"},{".bmp", "Bitmap"},
                                {".gif", "GIF"}, {".exr", "EXR"}, {".tiff", "TIFF"}]}],
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
    NoiseSize = proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE),
    NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    Turbulence = proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE),
    Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),
    WoodType = proplists:get_value(wood_type, Ps, ?DEF_MOD_WOODTYPE),
    Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),
    CellType = proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE),
    CellShape = proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE),
    CellSize = proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE),
    Intensity = proplists:get_value(intensity, Ps, ?DEF_MOD_INTENSITY),
    CellWeight1 = proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1),
    CellWeight2 = proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2),
    CellWeight3 = proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3),
    CellWeight4 = proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4),
    MusgraveType = proplists:get_value(musgrave_type, Ps, ?DEF_MOD_MUSGRAVE_TYPE),

    MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),
    MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),
    MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, ?DEF_MOD_MUSGRAVE_CONTRAST),
    MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),
    MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),
    DistortionType = proplists:get_value(distortion_type, Ps, ?DEF_MOD_DISTORTION_TYPE),

    DistortionIntensity = proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY),
    DistortionNoiseSize = proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE),

    MapsItems = [{atom_to_list(Map),{map,Map}} || {Map,_} <- Maps],

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            {?TAG,enabled,M} ->
                wings_dialog:enable(?KEY({pnl_mode,M}), Value =:= true, Store),
                wings_dialog:enable(?KEY({pnl_mod,M}), Value =:= true, Store);
            _ -> ok
        end
    end,

    Hook_Show = fun(Key, Value, Store) ->
        case Key of
            {?TAG,texture_type,M} ->
                wings_dialog:show(?KEY({pnl_image,M}), Value =:= image, Store),
                wings_dialog:show(?KEY({pnl_base1,M}), is_member(Value,[clouds,marble,wood,musgrave,distorted_noise]), Store),
                wings_dialog:show(?KEY({pnl_base2,M}), is_member(Value,[clouds,marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_base3,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_sharpness,M}), Value =:= marble, Store),
                wings_dialog:show(?KEY({pnl_turb,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_wood,M}), Value =:= wood, Store),
                wings_dialog:show(?KEY({pnl_voronoi,M}), Value =:= voronoi, Store),
                wings_dialog:show(?KEY({pnl_musgrave,M}), Value =:= musgrave, Store),
                wings_dialog:show(?KEY({pnl_dist_noise,M}), Value =:= distorted_noise, Store),
                wings_dialog:update(?KEY({pnl_mod,M}), Store);
            {?TAG, material_type, M} ->
                wings_dialog:show(?KEY({pnl_layer,M}), Value =:= glossy, Store),
                wings_dialog:update(?KEY({pnl_mod,M}), Store)
        end
    end,


    ModFrame =
        {vframe, [ %% vertical frame que engloba todo el panel
            {hframe, [ % horiz. para el boton Enable
                {?__(6,"Enabled"),Enabled,[{key,{?TAG,enabled,M}},{hook,Hook_Enable}]}%,
            ]},
            {vframe,[
                {vframe,[ % este panel es del key, pnl_mode
                    {hframe,[
                        {label,?__(7,"Blend Mode" )},
                        {menu,menu_blend_mode(), BlendMode,[]},
                        panel,
                        {?__(8," Stencil "), Stencil}, panel,
                        {?__(9," Negative "), Negative},panel,
                        {?__(10," No RGB "), NoRGB}
                    ]},
                    {hframe,[
                        {hframe,[ %test
                        {label,?__(11," Def. Color: ")},{color,DefColor},
                        {label,?__(12," Def. Value:")},{slider,{text,DefValue,[range(zero_to_one)]}}
                        ]} % test
                    ]}
                ],[key({pnl_mode,M}),{title,"Stencil"},{margin,false},{hook,Hook_Show}]}
            ]},
            {vframe, [
                %!---------------------
                %! Texture mapping
                %!---------------------
                {vframe,[
                    {hframe,[
                        {label,?__(15,"Coordinates:")},
                        {menu,[
                            {?__(16,"UV"),uv},
                            {?__(17,"Orco"),orco},
                            {?__(18,"Global"),global}
                        ],Coordinates,[]
                        }, panel,
                        {label,?__(19,"Size:    X ")},{text,SizeX,[range(size),{width,5}]},
                        {label,?__(20," Y ")},{text,SizeY,[range(size),{width,5}]},
                        {label,?__(21," Z ")},{text,SizeZ,[range(size),{width,5}]}
                    ]},
                    {hframe,[
                        {label,?__(22,"Projection:   ")},
                        {menu,[
                            {?__(23,"Flat"),plain},
                            {?__(24,"Cube"),cube},
                            {?__(25,"Tube"),tube},
                            {?__(26,"Sphere"),sphere}
                        ],Projection,[]
                        }, panel,
                        {label,?__(27,"Offset: X")},{text,OffsetX,[range(size),{width,5}]},
                        {label,?__(28," Y ")},{text,OffsetY,[range(size),{width,5}]},
                        {label,?__(29," Z ")},{text,OffsetZ,[range(size),{width,5}]}
                    ]}, % place here new frame's for fill to the end
                    {hframe,[

                    ]}
                ],[{title,"Texture Mapping"},{margin,false}]},
                %!-----------------------------------------
                %! layers to mapping texture
                %!-----------------------------------------
                %{menu,menu_modulators(),ShaderType,[]},
                {vframe,[
                    {hframe,[
                        {hframe,[
                            {?__(35,"Diffuse Color"),DiffuseLayer},
                            {slider,{text,DiffuseFactor,[range(zero_to_one),{width,5}]}}]}
                    ],[key({pnl_layer,M}), {margin,false},{hook,Hook_Show}]},
                    {hframe,[
                        {hframe,[
                            {?__(36,"Mirror Amount"),MirrorLayer},
                            {slider,{text,MirrorFactor,[range(zero_to_one),{width,5}]}}]}
                    ]},
                    {hframe,[
                        {hframe,[
                            {?__(37,"Mirror  Color"),MirrorColorLayer},
                            {slider,{text,MirrorColorFactor,[range(zero_to_one),{width,5}]}}]}
                    ]},
                    {hframe,[
                        {hframe,[
                            {?__(38,"Transparency "),TransparentLayer},
                            {slider,{text,TransparentFactor,[range(zero_to_one),{width,5}]}}]}
                    ]},
                    {hframe,[
                        {hframe,[{?__(39,"Translucency "),TranslucentLayer},
                        {slider,{text,TranslucentFactor,[range(neg_one_to_one),{width,5}]}}]}
                    ]},
                    {hframe,[
                        {hframe,[{?__(40,"Bump  Mapping"),BumpLayer},
                        {slider,{text,BumpFactor,[range(neg_one_to_one),{width,5}]}}]}
                    ]}
                ],[{title,"Influence"},{margin,false}]},
                %!-----------------------
                %! textures
                %!-----------------------
                {menu, MapsItems++[
                    {?__(50,"Image"),image},
                    {?__(51,"Clouds"),clouds},
                    {?__(52,"Marble"),marble},
                    {?__(53,"Wood"),wood},
                    {?__(54,"Voronoi"),voronoi},
                    {?__(55,"Musgrave"),musgrave},
                    {?__(56,"Distorted Noise"),distorted_noise}
                ],TextureType,[{key,{?TAG,texture_type,M}}, {hook,Hook_Show}]},
                {vframe, [
                    {hframe, [
                        {label,?__(57,"Filename")},
                        {button,{text,Filename,[{width,35},{props,BrowseProps}]}}
                    ],[key({pnl_image,M}), {show,false}]},
                    %% Clouds,Marble,Wood Specific Procedurals Line 1
                    {hframe, [
                        {label,?__(58,"Texture")},{color,Color1},
                        panel,
                        {label,?__(59,"Base")},{color,Color2},
                        panel,
                        {?__(60,"Hard Noise"),Hard},
                        %% Start Noise Basis Select
                        {menu,menu_distortion_type(),NoiseBasis,[]}
                        %% End Noise Basis Select
                    ],[key({pnl_base1,M})]},

                    %% Clouds,Marble,Wood Specific Procedurals Line 2
                    {hframe, [
                        {hframe, [
                            {label,?__(61,"Noise Size")},
                            {text,NoiseSize,[range(noise_size)]}
                        ]},
                        {hframe, [
                            {label,?__(62,"Noise Depth")},
                            {text,Depth,[range(noise_depth)]}
                        ]}
                    ],[key({pnl_base2,M}),{margin,false},{show,false}]},

                    %% Marble Specific Procedurals
                    {hframe, [
                        {hframe, [
                            {label,?__(63,"Sharpness")},
                            {text,Sharpness,[range(sharpness)]}
                        ],[key({pnl_sharpness,M})]},
                        %],[hook(open, [member,{?TAG,type,M},marble])]},

                        %% Marble,Wood Specific Procedurals
                        {hframe, [
                            {hframe, [
                                {label,?__(64,"Turbulence")},
                                {text,Turbulence,[range(turbulence)]}
                            ]},
                            %% Start Shape Select
                            {menu,[
                                {?__(65,"sin"),"sin"},
                                {?__(66,"saw"),saw},
                                {?__(67,"tri"),tri}
                            ],Shape,[]}
                            %% End Shape Select
                        ],[key({pnl_turb,M}),{margin,false}]},

                        %% Wood Specific Procedurals
                        {hframe, [
                            %% Start Wood Type Select
                            {menu,[
                                {?__(68,"Rings"),rings},
                                {?__(69,"Bands"),bands}
                            ],WoodType,[]}
                            %% End Wood Type Select
                        ],[key({pnl_wood,M})]}
                    ],[key({pnl_base3,M}),{margin,false},{show,false}]},

                    %% Voronoi Specific Procedurals
                    {vframe, [
                        %% Start Voronoi Line 1
                        {hframe, [
                            %% Start Voronoi Cell Type Select
                            {menu,[
                                {?__(70,"Intensity"),intensity},
                                {?__(71,"Color"),col1},
                                {?__(72,"Color+Outline"),col2},
                                {?__(73,"Color+Outline+Intensity"),col3}
                            ],CellType,[]},
                            %% End Voronoi Cell Type Select
                            panel,
                            %% Start Voronoi Cell Shape Select
                            {menu,[{?__(74,"Actual Distance"),actual},
                                   {?__(75,"Distance Squared"),squared},
                                   {?__(76,"Manhattan"),manhattan},
                                   {?__(77,"Chebychev"),chebychev},
                                   {?__(78,"Minkovsky"),minkovsky}],
                                CellShape,[]}
                            %% End Voronoi Cell Shape Select
                        ],[{margin,false}]},
                        %% End Voronoi Line 1

                        %% Start Voronoi Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(79,"Cell Size")},{text,CellSize,[range(cell_size)]}
                            ]},
                            {hframe, [
                                {label,?__(80,"Intensity")},{text,Intensity,[range(intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Voronoi Line 2

                        %% Start Voronoi Line 3
                        {hframe, [
                            {hframe, [
                                {label,?__(81,"W1")},{text,CellWeight1,[range(cell_weight1)]}
                            ]},
                            {hframe, [
                                {label,?__(82,"W2")},{text,CellWeight2,[range(cell_weight2)]}
                            ]},
                            {hframe, [
                                {label,?__(83,"W3")},{text,CellWeight3,[range(cell_weight3)]}
                            ]},
                            {hframe, [
                                {label,?__(84,"W4")},{text,CellWeight4,[range(cell_weight4)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Voronoi Line 3
                    ],[key({pnl_voronoi,M}),{margin,false},{show,false}]},

                    %% Start Musgrave Specific Procedurals
                    {vframe,[
                        {hframe,[
                            %% Start Musgrave Type Select
                            {menu,[
                                {?__(85,"Multifractal"),multifractal},
                                {?__(86,"Ridged"),ridgedmf},
                                {?__(87,"Hybrid"),hybridmf},
                                {?__(88,"FBM"),fBm}
                            ],MusgraveType,[]},
                            panel,
                            {hframe,[
                                {label,?__(89,"Noise Size")},
                                {text,MusgraveNoiseSize,[range(musgrave_noisesize)]}
                            ]},
                            {hframe,[
                                {label,?__(90,"Intensity")},
                                {text,MusgraveIntensity,[range(musgrave_intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Musgrave Line 1

                        %% Start Musgrave Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(91,"Contrast (H)")},
                                {text,MusgraveContrast,[range(musgrave_contrast)]}
                            ]},
                            {hframe, [
                                {label,?__(92,"Lacunarity")},
                                {text,MusgraveLacunarity,[range(musgrave_lacunarity)]}
                            ]},
                            {hframe, [
                                {label,?__(93,"Octaves")},
                                {text,MusgraveOctaves,[range(musgrave_octaves)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Musgrave Line 2
                    ],[key({pnl_musgrave,M})]},

                    %% Start Distorted Noise Specific Procedurals
                    {vframe, [
                        {hframe, [
                            %% Start Distorted Noise Type Select
                            {menu,menu_distortion_type(),DistortionType,[]},
                            %% End Distorted Noise Type Select
                            {label,?__(94,"Noise Size")},{text,DistortionNoiseSize,[range(distortion_noisesize)]},
                            {label,?__(95,"Distortion")},{text,DistortionIntensity,[range(distortion_intensity)]}
                        ],[{margin,false}]}
                    ],[key({pnl_dist_noise,M}),{show,false}]}
                ],[key({pnl_type,M})]}
            ],[key({pnl_mod,M})]}
        ]},
    [{?__(100,"Shader")++" "++integer_to_list(M)++mod_legend(Enabled, BlendMode, TextureType), ModFrame}];

modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

mod_enabled_mode_type(Ps, Maps) ->
    {Enabled,Mode} =
        case proplists:get_value(mode, Ps, ?DEF_MOD_MODE) of
            off -> {false,?DEF_MOD_MODE};
            Mode1 -> {proplists:get_value(enabled, Ps, ?DEF_MOD_ENABLED),Mode1}
        end,
    TextureType = proplists:get_value(texture_type, Ps, ?DEF_MOD_TEXTURETYPE),
    case TextureType of
        {map,Map} ->
            case lists:keymember(Map, 1, Maps) of
                true -> {Enabled,Mode,TextureType};
                false -> {false,Mode,?DEF_MOD_TEXTURETYPE}
            end;
        _ -> {Enabled,Mode,TextureType}
    end.

mod_legend(Enabled, Mode, {map,Map}) ->
    mod_legend(Enabled, Mode, atom_to_list(Map));

mod_legend(Enabled, Mode, TextureType) when is_atom(Mode) ->
    mod_legend(Enabled, wings_util:cap(Mode), TextureType);

mod_legend(Enabled, Mode, TextureType) when is_atom(TextureType) ->
    mod_legend(Enabled, Mode, wings_util:cap(TextureType));

mod_legend(Enabled, Mode, TextureType) when is_list(Mode), is_list(TextureType) ->
    case Enabled of
        true -> " ("++?__(1,"On")++", ";
        false -> " ("++?__(2,"Off")++", "
    end++Mode++", "++TextureType++")".


modulator_result(Ps, [{{?TAG,enabled,_},_}|_]=Res) ->
    modulator_result(Ps, Res, 1, []);

modulator_result(Ps, Res) ->
    exit({invalid_tag,{?MODULE,?LINE,[Ps, Res]}}).

modulator_result(Ps, [], _, Modulators) ->  % Should not happen
    {[{modulators,reverse(Modulators)}|Ps], []};

modulator_result(Ps, [{{?TAG,autosmooth},_}|_]=Res, _, Modulators) ->
    {[{modulators,reverse(Modulators)}|Ps], Res};

modulator_result(Ps, Res0, M, Modulators) ->
    {Modulator,Res} = modulator(Res0, M),
    modulator_result(Ps, Res, M+1, [Modulator|Modulators]).


%%% Increase split_list # +1 per line if add Modulator to Dialog
modulator(Res0, M) ->
    {Res1,Res} = split_list(Res0,
        fun (A) ->
            Next = M+1,
            case A of
                {{?TAG,enabled,Next},_} -> true;    % The end block is the next start one
                {{?TAG,autosmooth},_} -> true;      % The last block was already read
                _ -> false
            end
        end),
    EnabledTag = {?TAG,enabled,M},
    TypeTag = {?TAG,texture_type,M},
    {EnabledTag,Enabled} = lists:keyfind(EnabledTag, 1, Res1),
    {TypeTag,TextureType} = lists:keyfind(TypeTag, 1, Res1),
    Res2 = lists:keydelete(EnabledTag, 1, lists:keydelete(TypeTag, 1, Res1)),
    %!------------------------------------------------------------------
    %! !!! IMPORTANTE !!!:
    %! los elementos de la siguiente lista deben guardar el mismo
    %! orden en el que aparecen o son usados en el UI.
    %!-------------------------------------------------------------------
    [Mode,
     Stencil,Negative,NoRGB,DefColor, DefValue,
     Coordinates, SizeX, SizeY, SizeZ,
     Projection, OffsetX, OffsetY, OffsetZ,
     %ShaderType,
     DiffuseLayer,DiffuseFactor,MirrorLayer,MirrorFactor,
     MirrorColorLayer,MirrorColorFactor,TransparentLayer,TransparentFactor,
     TranslucentLayer,TranslucentFactor,BumpLayer,BumpFactor,
     Filename,
     Color1,Color2,Hard,NoiseBasis,NoiseSize,Depth,
     Sharpness,Turbulence,Shape,WoodType,
     CellType,CellShape,CellSize,Intensity,CellWeight1,CellWeight2,CellWeight3,CellWeight4,
     MusgraveType,MusgraveNoiseSize,MusgraveIntensity,MusgraveContrast,
     MusgraveLacunarity,MusgraveOctaves,DistortionType,
     DistortionNoiseSize,DistortionIntensity] =
        case Res2 of
            [Minimized|Rest] when is_boolean(Minimized) -> Rest;  % for keep compatibility with previous W3D version
            Rest -> Rest
        end,
    %!--------------------------------------------------------
    %! Aqui mantendremos el mismo orden de uso del UI
    %!---------------------------------------------------------
    Ps = [{enabled,Enabled},{mode,Mode},
          {stencil,Stencil},{negative,Negative},{no_rgb,NoRGB},{def_color,DefColor},
          {def_value, DefValue},
          %{shader_type,ShaderType}, % only for logical UI, don't is need activate here ?
          {coordinates, Coordinates},
          {size_x,SizeX},{size_y,SizeY},{size_z,SizeZ},
          {projection, Projection},
          {offset_x,OffsetX},{offset_y,OffsetY},{offset_z,OffsetZ},
          {diffuse, DiffuseLayer},{diffuse_factor,DiffuseFactor},
          {mirror, MirrorLayer},{mirror_factor, MirrorFactor},
          {mirror_color, MirrorColorLayer},{mirror_color_factor, MirrorColorFactor},
          {transparency, TransparentLayer}, {transparent_factor,TransparentFactor},
          {translucency,TranslucentLayer},{translucent_factor, TranslucentFactor},
          {bump,BumpLayer},{bump_factor, BumpFactor},
          {texture_type,TextureType},
          {image_filename,Filename},{color1,Color1},{color2,Color2},{hard,Hard},
          {noise_basis,NoiseBasis},{noise_size,NoiseSize},{depth,Depth},
          {sharpness,Sharpness},{turbulence,Turbulence},{shape,Shape},
          {wood_type,WoodType},{cell_type,CellType},{cell_shape,CellShape},
          {cell_size,CellSize},{intensity,Intensity},{cell_weight1,CellWeight1},
          {cell_weight2,CellWeight2},{cell_weight3,CellWeight3},{cell_weight4,CellWeight4},
          {musgrave_type,MusgraveType},
          {musgrave_noisesize,MusgraveNoiseSize},{musgrave_intensity,MusgraveIntensity},
          {musgrave_contrast,MusgraveContrast},{musgrave_lacunarity,MusgraveLacunarity},
          {musgrave_octaves,MusgraveOctaves},{distortion_type,DistortionType},
          {distortion_noisesize,DistortionNoiseSize},
          {distortion_intensity,DistortionIntensity}
         ],
    {{modulator,Ps},Res}.

%%% Creates new Modulator
modulator_add(M) ->
    modulator_add(M,[]).
modulator_add(0,Acc) ->
    Acc;
modulator_add(M,Acc) ->
    modulator_add(M-1, Acc++[modulator_init(?DEF_MOD_MODE)]).

modulator_init(Mode) ->
    Ps = [{enabled,false},
        {mode,Mode},
        {stencil, false},
        {negative, false},
        {no_rgb, false},
        {def_color,?DEF_MOD_DEFCOLOR},
        {def_value, 1.0},
        %{shader_type,?DEF_SHADER_TYPE},
        {coordinates, global},
        {size_x,1.0},{size_y,1.0},{size_z,1.0},
        {projection, plain},
        {offset_x, 0.0},{offset_y, 0.0},{offset_z, 0.0},
        {diffuse, false},{diffuse_factor, 1.0},
        {mirror, false},{mirror_factor, 1.0},
        {mirror_color, false},{mirror_color_factor, 1.0},
        {transparency, false},{transparent_factor, 1.0},
        {translucency, false},{translucent_factor, 1.0},
        {bump,false},{bump_factor, 1.0},
        {texture_type,?DEF_MOD_TEXTURETYPE},
        {image_filename,?DEF_MOD_FILENAME},
        {color1,?DEF_MOD_COLOR1},
        {color2,?DEF_MOD_COLOR2},
        {hard,?DEF_MOD_HARD},
        {noise_basis,?DEF_MOD_NOISEBASIS},
        {noise_size,?DEF_MOD_NOISESIZE},
        {depth,?DEF_MOD_DEPTH},
        {sharpness,?DEF_MOD_SHARPNESS},
        {turbulence,?DEF_MOD_TURBULENCE},
        {shape,?DEF_MOD_SHAPE},
        {wood_type,?DEF_MOD_WOODTYPE},
        {cell_type,?DEF_MOD_CELLTYPE},
        {cell_shape,?DEF_MOD_CELLSHAPE},
        {cell_size,?DEF_MOD_CELLSIZE},
        {intensity,?DEF_MOD_INTENSITY},
        {cell_weight1,?DEF_MOD_CELL_WEIGHT1},
        {cell_weight2,?DEF_MOD_CELL_WEIGHT2},
        {cell_weight3,?DEF_MOD_CELL_WEIGHT3},
        {cell_weight4,?DEF_MOD_CELL_WEIGHT4},
        {musgrave_type,?DEF_MOD_MUSGRAVE_TYPE},
        {musgrave_noisesize,?DEF_MOD_MUSGRAVE_NOISESIZE},
        {musgrave_intensity,?DEF_MOD_MUSGRAVE_INTENSITY},
        {musgrave_contrast,?DEF_MOD_MUSGRAVE_CONTRAST},
        {musgrave_lacunarity,?DEF_MOD_MUSGRAVE_LACUNARITY},
        {musgrave_octaves,?DEF_MOD_MUSGRAVE_OCTAVES},
        {distortion_type,?DEF_MOD_DISTORTION_TYPE},
        {distortion_noisesize,?DEF_MOD_DISTORTION_NOISESIZE},
        {distortion_intensity,?DEF_MOD_DISTORTION_INTENSITY}
    ],
    {modulator,Ps}.
