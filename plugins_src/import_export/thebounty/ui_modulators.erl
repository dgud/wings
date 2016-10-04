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
    [{?__(120,"Mix"),mix},
     {?__(121,"Add"),add},
     {?__(122,"Multiply"),mul},
     {?__(123,"Subtract"),sub},
     {?__(124,"Screen"),scr},
     {?__(125,"Divide"),divide},
     {?__(126,"Difference"),dif},
     {?__(127,"Darken"),dar},
     {?__(128,"Lighten"),lig}].

menu_distortion_type() ->
   [{?__(129,"Blender-Distort"),blender},
    {?__(130,"Cellnoise"),cellnoise},
    {?__(131,"New Perlin"),newperlin},
    {?__(132,"Perlin"),stdperlin},
    {?__(133,"Voronoi Crackle"),voronoi_crackle},
    {?__(134,"Voronoi F1"),voronoi_f1},
    {?__(135,"Voronoi F2"),voronoi_f2},
    {?__(136,"Voronoi F3"),voronoi_f3},
    {?__(137,"Voronoi F4"),voronoi_f4},
    {?__(138,"Voronoi F1F2"),voronoi_f2f1}].

menu_extension_mode() ->
    [{?__(139,"Ext: Checked"),checker},
     {?__(140,"Ext: Repeat"),repeat},
     {?__(141,"Ext: Clip"),clip},
     {?__(142,"Ext: Clipcube"),clipcube},
     {?__(143,"Ext: Extended"),extend}].

menu_interpolate_mode() ->
    [{?__(144,"Interpolation: None"),none},
     {?__(145,"Interpolation: Bilinear"),bilinear},
     {?__(146,"Interpolation: Bicubic"),bicubic}].

% TODO: change this part of code for generate only the
% allowed modulators for each material
%
modulator_dialogs(Modulators0, Maps, MaterialType) ->
    ModCount = length(Modulators0),
    Modulators =
        if (ModCount < ?MAX_MODULATORS) ->
            Modulators0 ++ modulator_add(?MAX_MODULATORS-ModCount);
        true -> Modulators0
        end,
    [{oframe, modulator_dialogs(Modulators, Maps, MaterialType, 1), 1, [{style, buttons}]}].

modulator_dialogs([], _Maps, _MaterialType, _M) -> [];
modulator_dialogs([Modulator|Modulators], Maps, MaterialType, M) ->
    modulator_dialog(Modulator, Maps, MaterialType, M)++
    modulator_dialogs(Modulators, Maps, MaterialType, M+1).

modulator_dialog({modulator,Ps}, Maps, MaterialType, M) when is_list(Ps) ->
    %
    {Enabled,BlendMode,TextureType} = mod_enabled_mode_type(Ps, Maps),

    Stencil = proplists:get_value(stencil, Ps, false),
    Negative = proplists:get_value(negative, Ps, false),
    NoRGB = proplists:get_value(no_rgb, Ps, false),
    DefColor = proplists:get_value(def_color, Ps, ?DEF_MOD_DEFCOLOR),
    DefValue = proplists:get_value(def_value, Ps, 1.0),

    Coordinates = proplists:get_value(coordinates, Ps, global),
    SizeX = proplists:get_value(size_x, Ps, 1.0),
    SizeY = proplists:get_value(size_y, Ps, 1.0),
    SizeZ = proplists:get_value(size_z, Ps, 1.0),
    Projection = proplists:get_value(projection, Ps, plain),
    OffsetX = proplists:get_value(offset_x, Ps, 0.0),
    OffsetY = proplists:get_value(offset_y, Ps, 0.0),
    OffsetZ = proplists:get_value(offset_z, Ps, 0.0),
    Direction =  proplists:get_value(direction, Ps, xyz),

    DiffuseLayer = proplists:get_value(diffuse_layer, Ps, true),
    DiffuseFactor = proplists:get_value(diffuse_factor, Ps, 1.0),

    MirrorLayer = proplists:get_value(mirror, Ps, false),
    MirrorFactor = proplists:get_value(mirror_factor, Ps, 1.0),

    MirrorColorLayer = proplists:get_value(mirror_color, Ps, false),
    MirrorColorFactor = proplists:get_value(mirror_color_factor, Ps, 1.0),

    TransparentLayer = proplists:get_value(transparency, Ps, false),
    TransparentFactor = proplists:get_value(transparent_factor, Ps, 1.0),

    TranslucentLayer = proplists:get_value(translucency, Ps, false),
    TranslucentFactor = proplists:get_value(translucent_factor, Ps, 1.0),

    GlossyLayer = proplists:get_value(glossy, Ps, false),
    GlossyFactor = proplists:get_value(glossy_factor, Ps, 1.0),

    GlossyReflectLayer = proplists:get_value(glossy_reflect, Ps, false),
    GlossyReflectFactor = proplists:get_value(glossy_reflect_factor, Ps, 1.0),

    BumpLayer = proplists:get_value(bump, Ps, false),
    BumpFactor = proplists:get_value(bump_factor, Ps, 1.0),

    BlendLayer = proplists:get_value(blend_mask, Ps, false),
    BlendFactor = proplists:get_value(blend_factor, Ps, 1.0),

    Filename = proplists:get_value(image_filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type, open_dialog},
                    {extensions, [{".jpg", "JPEG"}, {".png", "PNG"},{".bmp", "Bitmap"},
                                {".gif", "GIF"}, {".exr", "EXR"}, {".tiff", "TIFF"}]}],
    Extension = proplists:get_value(extension,Ps,repeat),

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

    %MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),
    MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),
    %MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, 0.1),
    %MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),
    %MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),
    DistortionType = proplists:get_value(distortion_type, Ps, ?DEF_MOD_DISTORTION_TYPE),

    DistortionIntensity = proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY),
    DistortionNoiseSize = proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE),

    MapsItems = [{atom_to_list(Map),{map,Map}} || {Map,_} <- Maps],

    Hook_Enable = fun(Key, Value, Store) ->
        case Key of
            {?TAG,{M,enabled}} ->
                wings_dialog:enable(?KEY({pnl_blend_mode,M}), Value =:= true, Store),
                wings_dialog:enable(?KEY({pnl_modulator,M}), Value =:= true, Store);
            _ -> ok
        end
    end,

    Hook_Show = fun(Key, Value, Store) ->
        case Key of
            {?TAG,{M, texture_type}} ->
                wings_dialog:show(?KEY({pnl_image,M}), Value =:= image, Store),
                wings_dialog:show(?KEY({pnl_base1,M}), not is_member(Value,[image, voronoi]), Store),
                wings_dialog:show(?KEY({pnl_base2,M}), is_member(Value,[clouds,marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_base3,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_sharpness,M}), Value =:= marble, Store),
                wings_dialog:show(?KEY({pnl_turb,M}), is_member(Value,[marble,wood]), Store),
                wings_dialog:show(?KEY({pnl_wood,M}), Value =:= wood, Store),
                wings_dialog:show(?KEY({pnl_voronoi,M}), Value =:= voronoi, Store),
                wings_dialog:show(?KEY({pnl_musgrave,M}), Value =:= musgrave, Store),
                wings_dialog:show(?KEY({pnl_dist_noise,M}), Value =:= distorted_noise, Store),
                wings_dialog:update(?KEY({pnl_modulator,M}), Store);
            {?TAG,{M, extension}} ->
                wings_dialog:show(?KEY({pnl_repeat,M}), Value =:= repeat, Store),
                wings_dialog:show(?KEY({pnl_checked,M}), Value =:= checker, Store),
                wings_dialog:update(?KEY({pnl_modulator,M}), Store)
        end
    end,
    %
    InfluenceSlot =
        case MaterialType of
           shinydiffuse ->
                {hframe, [
                    {vframe,[
                        {?__(37,"Diffuse     "),DiffuseLayer,     [key({M,diffuse_layer})]},
                        {?__(38,"Mirror      "),MirrorLayer,      [key({M,mirror})]},
                        {?__(39,"Mirror Color"),MirrorColorLayer, [key({M,mirror_color})]}
                    ]},
                    {vframe,[
                        {slider,{text,DiffuseFactor,    [key({M,diffuse_factor}),range(zero_one)]}},
                        {slider,{text,MirrorFactor,     [key({M,mirror_factor}),range(zero_one)]}},
                        {slider,{text,MirrorColorFactor,[key({M,mirror_color_factor}),range(zero_one)]}}
                    ]},
                    {vframe,[
                        {?__(40,"Transparency"),TransparentLayer,[key({M,transparency})]},
                        {?__(41,"Translucency"),TranslucentLayer,[key({M,translucency})]},
                        {?__(42,"Bumpmap"),BumpLayer,[key({M,bump})]}
                    ]},
                    {vframe,[
                        {slider,{text,TransparentFactor,[key({M,transparent_factor}),range(zero_one)]}},
                        {slider,{text,TranslucentFactor,[key({M,translucent_factor}),range(neg_one_to_one)]}},
                        {slider,{text,BumpFactor,[key({M,bump_factor}),range(neg_one_to_one)]}}
                    ]}
                ],[{title,"Influence"},{margin,false}]};
            translucent ->
                {hframe, [
                    {vframe,[
                        {?__(43,"Diffuse"),DiffuseLayer, [key({M,diffuse_layer})]},
                        {?__(44,"Glossy"),GlossyLayer, [key({M,glossy})]},
                        {?__(45,"Transparency "),TransparentLayer,[key({M,transparency})]}
                    ]},
                    {vframe,[
                        {slider,{text,DiffuseFactor,[key({M,diffuse_factor}),range(zero_one)]}},
                        {slider,{text,GlossyFactor,[key({M,glossy_factor}),range(zero_one)]}},
                        {slider,{text,TransparentFactor,[key({M,transparent_factor}),range(zero_one)]}}
                    ]},
                    {vframe,[
                        {?__(46,"Translucency "),TranslucentLayer,[key({M,translucency})]},
                        {?__(47,"Bumpmap"),BumpLayer,[key({M,bump})]}
                    ]},
                    {vframe,[
                        {slider,{text,TranslucentFactor,[key({M,translucent_factor}),range(neg_one_to_one)]}},
                        {slider,{text,BumpFactor,[key({M,bump_factor}),range(neg_one_to_one)]}}
                    ]}
                ],[{title,"Influence"},{margin,false}]};
            glossy ->
                {hframe, [
                    {vframe,[
                        {?__(48,"Diffuse"),DiffuseLayer, [key({M,diffuse_layer})]},
                        {?__(49,"Glossy"),GlossyLayer, [key({M,glossy})]}
                    ]},
                    {vframe,[
                        {slider,{text,DiffuseFactor,[key({M,diffuse_factor}),range(zero_one)]}},
                        {slider,{text,GlossyFactor,[key({M,glossy_factor}),range(zero_one)]}}
                    ]},
                    {vframe,[
                        {?__(50,"Glossy Reflect"),GlossyReflectLayer,[key({M,glossy_reflect})]},
                        {?__(51,"Bumpmap"),BumpLayer,[key({M,bump})]}
                    ]},
                    {vframe,[
                        {slider,{text,GlossyReflectFactor,[key({M,glossy_reflect_factor}),range(zero_one)]}},
                        {slider,{text,BumpFactor,[key({M,bump_factor}),range(neg_one_to_one)]}}
                    ]}
                ],[{title,"Influence"},{margin,false}]};
            glass ->
                {hframe, [
                    {vframe,[
                        {?__(52,"Mirror Color"),MirrorColorLayer, [key({M,mirror_color})]},
                        {?__(53,"Bumpmap"),BumpLayer,[key({M,bump})]}
                    ]},
                    {vframe,[
                        {slider,{text,MirrorFactor,[key({M,mirror_factor}),range(zero_one)]}},
                        {slider,{text,BumpFactor,[key({M,bump_factor}),range(neg_one_to_one)]}}
                    ]}
                ],[{title,"Influence"},{margin,false}]};

            blend_mat ->
                {hframe, [
                    {vframe,[
                        {?__(54,"Blend Mask"),BlendLayer,[key({M,blend_mask})]}
                    ]},
                    {vframe,[
                        {slider,{text,BlendFactor,[key({M,blend_factor}),range(zero_one)]}}
                    ]}
                ],[{title,"Influence"},{margin,false}]}
        end,
    %
    ModFrame =
        {vframe, [
            {hframe, [
                {?__(5,"Enabled"),Enabled,[key({M,enabled}),{hook,Hook_Enable}]}
            ]},
            {vframe,[
                {vframe,[
                    {hframe,[
                        {label,?__(7,"Blend Mode" )},
                        {menu,menu_blend_mode(), BlendMode,[key({M,mode})]},
                        panel,
                        {?__(8," Stencil "), Stencil, [key({M,stencil})]}, panel,
                        {?__(9," Negative "), Negative, [key({M,negative})]},panel,
                        {?__(10," No RGB "), NoRGB, [key({M,no_rgb})]}
                    ],[{margin, false}]},
                    {hframe,[
                        {hframe,[
                            {label,?__(11,"Def. Color: ")},{color,DefColor, [key({M,def_color})]},
                            {label,?__(12," Def. Value:")},
                            {slider,{text,DefValue,[key({M,def_value}),range(zero_one),{width,4}]}}
                        ]}
                    ]}
                ],[key({pnl_blend_mode,M}),{title,"Stencil"},{margin,false},{hook,Hook_Show}]}
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
                        ],Coordinates,[key({M,coordinates})]
                        }, panel,
                        {label,?__(19,"Size:    X ")},{text,SizeX,[key({M,size_x}),range(size),{width,5}]},
                        {label,?__(20," Y ")},{text,SizeY,[key({M,size_y}),range(size),{width,5}]},
                        {label,?__(21," Z ")},{text,SizeZ,[key({M,size_z}),range(size),{width,5}]},
                        {label,?__(22,"Direction: ")},
                        {menu,[
                            {?__(23,"X Y Z"),xyz},{?__(24,"X Z Y"),xzy},
                            {?__(25,"Y X Z"),yxz},{?__(26,"Y Z X"),yzx},
                            {?__(27,"Z X Y"),zxy},{?__(28,"Z Y X"),zyx}
                        ],Direction,[key({M,direction})]}
                    ],[{margin,false}]},
                    {hframe,[
                        {label,?__(29,"Projection:   ")},
                        {menu,[
                            {?__(30,"Flat"),plain},
                            {?__(31,"Cube"),cube},
                            {?__(32,"Tube"),tube},
                            {?__(33,"Sphere"),sphere}
                        ],Projection,[key({M,projection})]
                        }, panel,
                        {label,?__(34,"Offset: X")},{text,OffsetX,[key({M,offset_x}),range(size),{width,5}]},
                        {label,?__(35," Y ")},{text,OffsetY,[key({M,offset_x}),range(size),{width,5}]},
                        {label,?__(36," Z ")},{text,OffsetZ,[key({M,offset_x}),range(size),{width,5}]}
                    ],[{margin,false}]}
                ],[{title,"Texture Mapping"},{margin,false}]},
                InfluenceSlot,

                %!-----------------------
                %! textures
                %!-----------------------
                {hframe, [
                    {label,?__(60,"Texture type")},
                    {menu, MapsItems++[
                        {?__(61,"Image"),image},
                        {?__(62,"Clouds"),clouds},
                        {?__(63,"Marble"),marble},
                        {?__(64,"Wood"),wood},
                        {?__(65,"Voronoi"),voronoi},
                        {?__(66,"Musgrave"),musgrave},
                        {?__(67,"Distorted Noise"),distorted_noise}
                    ],TextureType,[key({M,texture_type}), {hook,Hook_Show}]}
                ]},
                {vframe, [
                    {vframe, [
                        {hframe, [
                            {hframe, [ {label,?__(68,"File")},
                                {button,{text,Filename,[key({M,image_filename}),{width,27},{props,BrowseProps}]}},
                                {label,?__(69,"Gamma Inp.")},
                                {slider,{text,proplists:get_value(gamma_input,Ps,1.0),[key({M,gamma_input}),range(gamma),{width,5}]}}
                            ]}
                        ]},
                        {hframe, [
                            {?__(70,"Use Alpha"),proplists:get_value(use_alpha,Ps,false),[key({M,use_alpha})]},panel,
                            {?__(71,"Calc. Alpha"),proplists:get_value(calc_alpha,Ps,false),[key({M,calc_alpha})]},panel,
                            {?__(72,"Flip Axis"),proplists:get_value(flip_axis,Ps,false),[key({M,flip_axis})]},
                            {menu,menu_interpolate_mode(),proplists:get_value(interpolate,Ps,none),[key({M,interpolate})]}
                        ]},
                        {hframe, [
                            {menu,menu_extension_mode(),Extension,[key({M,extension}),{hook,Hook_Show}]},panel,
                            {hframe, [
                                {label,?__(73,"Repeat X")},
                                {text,proplists:get_value(repeat_x,Ps,1),[key({M,repeat_x}),range(ione_twenty),{width,5}]},
                                panel,{label,?__(74,"Repeat Y")},
                                {text,proplists:get_value(repeat_y,Ps,1),[key({M,repeat_y}),range(ione_twenty),{width,5}]}
                            ],[key({pnl_repeat,M}),{margin,false}]},
                            {hframe, [
                                {?__(75,"Even "), proplists:get_value(even, Ps, false),[key({M,even})]},
                                {?__(76,"Odd "), proplists:get_value(odd, Ps, false),[key({M,odd})]},
                                {label,?__(77,"Distance")},
                                {text,proplists:get_value(distance,Ps,0.0),[key({M,distance}),range(zero_ten),{width,4}]},
                                {label,?__(78," Crop Min. XY")},
                                {text,proplists:get_value(crop_minx,Ps,0.0),[key({M,crop_minx}),range(zero_ten),{width,4}]},
                                {text,proplists:get_value(crop_miny,Ps,0.0),[key({M,crop_miny}),range(zero_ten),{width,4}]},
                                {label,?__(79," Crop Max. XY")},
                                {text,proplists:get_value(crop_maxx,Ps,0.0),[key({M,crop_maxx}),range(zero_ten),{width,4}]},
                                {text,proplists:get_value(crop_maxy,Ps,0.0),[key({M,crop_maxy}),range(zero_ten),{width,4}]}
                            ],[key({pnl_checked,M}),{show,false}]}
                        ]}
                    ],[key({pnl_image,M}), {margin,false}]},
                    %% Clouds,Marble,Wood Specific Procedurals Line 1
                    {hframe, [
                        {label,?__(80,"Texture")},{color,Color1,[key({M,color1})]}, panel,
                        {label,?__(81,"Base")},{color,Color2,[key({M,color2})]}, panel,
                        {?__(82,"Hard Noise"),Hard,[key({M,hard})]},
                        {menu,menu_distortion_type(),NoiseBasis,[key({M,noise_basis})]}
                    ],[key({pnl_base1,M}),{show,false}]},

                    %% Clouds,Marble,Wood Specific Procedurals Line 2
                    {hframe, [
                        {hframe, [
                            {label,?__(83,"Noise Size")},{text,NoiseSize,[key({M,noise_size}),range(noise_size),{width,4}]}
                        ]},
                        {hframe, [
                            {label,?__(84,"Noise Depth")},
                            {text,Depth,[key({M,depth}),range(noise_depth)]}
                        ]}
                    ],[key({pnl_base2,M}),{margin,false},{show,false}]},

                    %% Marble Specific Procedurals
                    {hframe, [
                        {hframe, [
                            {label,?__(85,"Sharpness")},
                            {text,Sharpness,[key({M,sharpness}),range(sharpness),{width,4}]}
                        ],[key({pnl_sharpness,M})]},
                        %],[hook(open, [member,{?TAG,type,M},marble])]},

                        %% Marble,Wood Specific Procedurals
                        {hframe, [
                            {hframe, [
                                {label,?__(86,"Turbulence")},
                                {text,Turbulence,[key({M,turbulence}),range(turbulence)]}
                            ]},
                            %% Start Shape Select
                            {menu,[
                                {?__(87,"sin"),"sin"},
                                {?__(88,"saw"),saw},
                                {?__(89,"tri"),tri}
                            ],Shape,[key({M,shape})]}
                        ],[key({pnl_turb,M}),{margin,false}]},

                        %% Wood Specific Procedurals
                        {hframe, [
                            {menu,[
                                {?__(90,"Rings"),rings},
                                {?__(91,"Bands"),bands}
                            ],WoodType,[key({M,wood_type})]}
                        ],[key({pnl_wood,M})]}
                    ],[key({pnl_base3,M}),{margin,false},{show,false}]},

                    %% Voronoi Specific Procedurals
                    {vframe, [
                        %% Start Voronoi Line 1
                        {hframe, [
                            %% Voronoi Cell Type Select
                            {menu,[
                                {?__(92,"Intensity"),intensity},
                                {?__(93,"Color"),col1},
                                {?__(94,"Color+Outline"),col2},
                                {?__(95,"Color+Outline+Intensity"),col3}
                            ],CellType,[key({M,cell_type})]},
                            panel,
                            %% Voronoi Cell Shape Select
                            {menu,[
                                {?__(96,"Actual Distance"),actual},
                                {?__(97,"Distance Squared"),squared},
                                {?__(98,"Manhattan"),manhattan},
                                {?__(99,"Chebychev"),chebychev},
                                {?__(100,"Minkovsky"),minkovsky}
                            ],CellShape,[key({M,cell_shape})]}
                        ],[{margin,false}]},

                        %% Start Voronoi Line 2
                        {hframe, [
                            {hframe, [ {label,?__(101,"Cell Size")},
                                {text,CellSize,[key({M,cell_size}),range(cell_size),{width,5}]}
                            ]},
                            {hframe, [ {label,?__(102,"Intensity")},
                                {text,Intensity,[key({M,intensity}),range(intensity),{width,5}]}
                            ]}
                        ],[{margin,false}]},

                        %% Start Voronoi Line 3
                        {hframe, [
                            {hframe, [ {label,?__(103,"W1")},
                                {text,CellWeight1,[key({M,cell_weight1}),range(neg_two_two),{width,5}]}
                            ]},
                            {hframe, [ {label,?__(104,"W2")},
                                {text,CellWeight2,[key({M,cell_weight2}),range(neg_two_two),{width,5}]}
                            ]},
                            {hframe, [ {label,?__(405,"W3")},
                                {text,CellWeight3,[key({M,cell_weight3}),range(neg_two_two),{width,5}]}
                            ]},
                            {hframe, [ {label,?__(106,"W4")},
                                {text,CellWeight4,[key({M,cell_weight4}),range(neg_two_two),{width,5}]}
                            ]}
                        ],[{margin,false}]}
                        %% End Voronoi Line 3
                    ],[key({pnl_voronoi,M}),{margin,false},{show,false}]},

                    %% Start Musgrave Specific Procedurals
                    {vframe,[
                        {hframe,[
                            %% Start Musgrave Type Select
                            {menu,[
                                {?__(107,"Multifractal"),multifractal},
                                {?__(108,"Ridged"),ridgedmf},
                                {?__(109,"Hybrid"),hybridmf},
                                {?__(110,"FBM"),fBm}
                            ],MusgraveType,[key({M,musgrave_type})]},
                            panel,
                            {hframe,[ {label,?__(111,"Noise Size")},
                                {text,proplists:get_value(musgrave_noisesize, Ps, 0.5),[key({M,musgrave_noisesize}),range(musgrave_noisesize)]}
                            ]},
                            {hframe,[ {label,?__(112,"Intensity")},
                                {text,MusgraveIntensity,[key({M,musgrave_intensity}),range(zero_to_ten)]}
                            ]}
                        ],[{margin,false}]},

                        %% Start Musgrave Line 2
                        {hframe, [
                            {hframe, [ {label,?__(113,"Contrast (H)")},
                                {text,proplists:get_value(musgrave_contrast, Ps, 0.1),[key({M,musgrave_contrast}),range(musgrave_contrast)]}
                            ]},
                            {hframe, [ {label,?__(114,"Lacunarity")},
                                {text,proplists:get_value(musgrave_lacunarity, Ps, 2.0),[key({M,musgrave_lacunarity}),range(zero_six)]}
                            ]},
                            {hframe, [ {label,?__(115,"Octaves")},
                                {text,proplists:get_value(musgrave_octaves, Ps, 8.0),[key({M,musgrave_octaves}),range(zero_eight)]}
                            ]}
                        ],[{margin,false}]}
                    ],[key({pnl_musgrave,M}),{show,false}]},

                    %% Start Distorted Noise Specific Procedurals
                    {vframe, [
                        {hframe, [
                            {menu,menu_distortion_type(),DistortionType,[key({M,distortion_type})]},
                            {label,?__(116,"Noise Size")},{text,DistortionNoiseSize,[key({M,distortion_noisesize}),range(distortion_noisesize)]},
                            {label,?__(117,"Distortion")},{text,DistortionIntensity,[key({M,distortion_intensity}),range(zero_to_ten)]}
                        ],[{margin,false}]}
                    ],[key({pnl_dist_noise,M}),{show,false}]}
                ],[key({pnl_type,M})]}
            ],[key({pnl_modulator,M})]}
        ]},
    [{?__(119,"Shader")++" "++integer_to_list(M)++mod_legend(Enabled, BlendMode, TextureType), ModFrame}];

modulator_dialog(_Modulator, _Maps, _MaterialType, _) ->
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


process_modulator(Ps) ->
    {Modulators,Remaning} =
    lists:foldr(fun(Id, {Mod0,Ps0})->
        {Mod1, Ps1} = modulator_result_find(Ps0, Id, {[],[]}),
        {[{modulator, lists:sort(Mod1)}]++Mod0,Ps1}
                end, {[],Ps}, lists:seq(1,?MAX_MODULATORS)),
    {[{modulators, Modulators}], Remaning}.

modulator_result_find([], _, Acc) -> Acc;

modulator_result_find([{{Id,Field},Value}|Ps], Id, {Mod, Remaining}) ->
    modulator_result_find(Ps, Id, {Mod ++[{Field,Value}], Remaining});

modulator_result_find([Item|Ps], Id, {Mod, Remaining}) ->
    modulator_result_find(Ps, Id, {Mod, Remaining ++ [Item]}).


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
        {coordinates, global},
        {size_x,1.0},{size_y,1.0},{size_z,1.0},
        {projection, plain},
        {offset_x, 0.0},{offset_y, 0.0},{offset_z, 0.0},
        {direction, xyz},
        {diffuse_layer, false},{diffuse_factor, 1.0},
        {mirror, false},{mirror_factor, 1.0},
        {mirror_color, false},{mirror_color_factor, 1.0},
        {glossy, false},{glossy_factor, 1.0},
        {transparency, false},{transparent_factor, 1.0},
        {translucency, false},{translucent_factor, 1.0},
        {bump,false},{bump_factor, 1.0},
        {blend_mask, false},{blend_factor, 0.5},
        {texture_type, clouds},
        {image_filename, ""},
        {gamma_input, 2.2},
        {use_alpha, false},
        {calc_alpha, false},
        {flip_axis, false},
        {extension, repeat},{repeat_x,1},{repeat_y, 1},{even, false},{odd, false},
        {distance,0.0},{crop_minx, 0.0},{crop_miny, 0.0},{crop_maxx, 0.0},{crop_maxy, 0.0},
        %{normal_map, false},
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
        {musgrave_noisesize,0.5},
        {musgrave_intensity,?DEF_MOD_MUSGRAVE_INTENSITY},
        {musgrave_contrast,0.1},
        {musgrave_lacunarity,?DEF_MOD_MUSGRAVE_LACUNARITY},
        {musgrave_octaves,?DEF_MOD_MUSGRAVE_OCTAVES},
        {distortion_type,?DEF_MOD_DISTORTION_TYPE},
        {distortion_noisesize,?DEF_MOD_DISTORTION_NOISESIZE},
        {distortion_intensity,?DEF_MOD_DISTORTION_INTENSITY}
    ],
    {modulator,Ps}.
