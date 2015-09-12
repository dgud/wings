%
%
%
%
modulator_dialogs(Modulators0, Maps) ->
    ModCount = length(Modulators0),
    Modulators =
        if (ModCount < 5) ->
            Modulators0 ++ modulator_add(5-ModCount);
        true -> Modulators0
        end,
    [{oframe, modulator_dialogs(Modulators, Maps, 1), 1, [{style, buttons}]}].

modulator_dialogs([], _Maps, _M) -> [];
modulator_dialogs([Modulator|Modulators], Maps, M) ->
    modulator_dialog(Modulator, Maps, M)++
    modulator_dialogs(Modulators, Maps, M+1).

modulator_dialog({modulator,Ps}, Maps, M) when is_list(Ps) ->
    {Enabled,Mode,Type} = mod_enabled_mode_type(Ps, Maps),
    AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),
    TextureType = proplists:get_value(texture_type, Ps, ?DEF_TEXTURE_TYPE),
    SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
    SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
    SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
    Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
%%    Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
    Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
    Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
    Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
    BrowseProps = [{dialog_type,open_dialog},
                   {extensions,[{".jpg",?__(3,"JPEG compressed image")},
                                {".tga",?__(4,"Targa bitmap")}]}],
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
            {?TAG,type,M} ->
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
                wings_dialog:update(?KEY({pnl_mod,M}), Store)
        end
    end,

    ModFrame =
        {vframe, [
            {hframe, [
                {?__(5,"Enabled"),Enabled,[{key,{?TAG,enabled,M}},{hook,Hook_Enable}]},
                panel,
                {hframe, [
                    {menu,[
                        {?__(6,"Mix"),mix},
                        {?__(7,"Add"),add},
                        {?__(8,"Multiply"),mul},
                        {?__(109,"Subtract"),sub},
                        {?__(110,"Screen"),scr},
                        {?__(111,"Divide"),divide},
                        {?__(112,"Difference"),dif},
                        {?__(113,"Darken"),dar},
                        {?__(114,"Lighten"),lig}
                    ],Mode,[]},
                    panel,
                    {menu,[
                        {?__(115,"Alpha Off"),off},
                        {?__(116,"Alpha Transparency"),transparency},
                        {?__(117,"Diffuse+Alpha Transparency"),diffusealphatransparency},
                        {?__(118,"Alpha Translucency"),translucency},
                        {?__(119,"Specularity"),specularity},
                        {?__(120,"Stencil"),stencil}
                    ],AlphaIntensity,[]},
                    panel,
                    {menu,[
                        {?__(121,"Diffuse (Shiny Diffuse, Glossy, SSS)"),diffusetexture},
                        {?__(122,"Mirror Color (Shiny Diffuse, Glass)"),mirrorcolortexture},
                        {?__(123,"Mirror (Shiny Diffuse)"),mirrortexture},
                        {?__(124,"Glossy (Glossy)"),glossytexture},
                        {?__(125,"Glossy Reflect (Glossy)"),glossyreflecttexture},
                        {?__(126,"Transparency (Shiny Diffuse)"),transparencytexture},
                        {?__(127,"Translucency (Shiny Diffuse)"),translucencytexture},
                        {?__(128,"Bump (All)"),bumptexture}
                    ],TextureType,[]}
                ],[key({pnl_mode,M}),{margin,false},{hook,Hook_Show}]}
            ]},
            {vframe, [
                {hframe,[
                    {hframe,[
                        {label,?__(10,"Size X")},{text,SizeX,[range(size)]}
                    ]},
                    {hframe,[
                        {label,?__(11,"Y")},{text,SizeY,[range(size)]}
                    ]},
                    {hframe,[
                        {label,?__(12,"Z")},{text,SizeZ,[range(size)]}
                    ]}
                ],[{margin,false}]},
                {hframe,[
                    {vframe,[
                        {label,?__(13,"Color Factor")},
                        {label,?__(16,"Value Factor")},
                        {label,?__(17,"Normal")}
                    ]},
                    {vframe,[
                        {slider,{text,fit_range(Diffuse,modulation),[range(modulation)]}},
                        {slider,{text,Shininess,[range(modulation)]}},
                        {slider,{text,Normal,[range(modulation)]}}
                    ]}
                ],[{margin,false}]},
                {menu, MapsItems++[
                    {?__(18,"Image"),image},
                    {?__(19,"Clouds"),clouds},
                    {?__(20,"Marble"),marble},
                    {?__(21,"Wood"),wood},
                    {?__(46,"Voronoi"),voronoi},
                    {?__(62,"Musgrave"),musgrave},
                    {?__(82,"Distorted Noise"),distorted_noise}
                ],Type,[{key,{?TAG,type,M}}, {hook,Hook_Show}]},
                {vframe, [
                    {hframe, [
                        {label,?__(22,"Filename")},
                        {button,{text,Filename,[{width,35},{props,BrowseProps}]}}
                    ],[key({pnl_image,M}), {show,false}]},
                    %% Clouds,Marble,Wood Specific Procedurals Line 1
                    {hframe, [
                        {label,?__(23,"Texture")},{color,Color1},
                        panel,
                        {label,?__(24,"Base")},{color,Color2},
                        panel,
                        {?__(25,"Hard Noise"),Hard},
                        %% Start Noise Basis Select
                        {menu,[{?__(36,"Blender-Basis"),blender},
                            {?__(37,"Cellnoise"),cellnoise},
                            {?__(38,"New Perlin"),newperlin},
                            {?__(39,"Perlin"),stdperlin},
                            {?__(40,"Voronoi Crackle"),voronoi_crackle},
                            {?__(41,"Voronoi F1"),voronoi_f1},
                            {?__(42,"Voronoi F2"),voronoi_f2},
                            {?__(43,"Voronoi F3"),voronoi_f3},
                            {?__(44,"Voronoi F4"),voronoi_f4},
                            {?__(45,"Voronoi F1F2"),voronoi_f2f1}],
                            NoiseBasis,[]}
                        %% End Noise Basis Select
                    ],[key({pnl_base1,M})]},

                    %% Clouds,Marble,Wood Specific Procedurals Line 2
                    {hframe, [
                        {hframe, [
                            {label,?__(26,"Noise Size")},
                            {text,NoiseSize,[range(noise_size)]}
                        ]},
                        {hframe, [
                            {label,?__(27,"Noise Depth")},
                            {text,Depth,[range(noise_depth)]}
                        ]}
                    ],[key({pnl_base2,M}),{margin,false},{show,false}]},

                    %% Marble Specific Procedurals
                    {hframe, [
                        {hframe, [
                            {label,?__(28,"Sharpness")},
                            {text,Sharpness,[range(sharpness)]}
                        ],[key({pnl_sharpness,M})]},
                        %],[hook(open, [member,{?TAG,type,M},marble])]},

                        %% Marble,Wood Specific Procedurals
                        {hframe, [
                            {hframe, [
                                {label,?__(29,"Turbulence")},
                                {text,Turbulence,[range(turbulence)]}
                            ]},
                            %% Start Shape Select
                            {menu,[
                                {?__(30,"sin"),"sin"},
                                {?__(31,"saw"),saw},
                                {?__(32,"tri"),tri}
                            ],Shape,[]}
                            %% End Shape Select
                        ],[key({pnl_turb,M}),{margin,false}]},

                        %% Wood Specific Procedurals
                        {hframe, [
                            %% Start Wood Type Select
                            {menu,[
                                {?__(33,"Rings"),rings},
                                {?__(34,"Bands"),bands}
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
                                {?__(47,"Intensity"),intensity},
                                {?__(48,"Color"),col1},
                                {?__(49,"Color+Outline"),col2},
                                {?__(50,"Color+Outline+Intensity"),col3}
                            ],CellType,[]},
                            %% End Voronoi Cell Type Select
                            panel,
                            %% Start Voronoi Cell Shape Select
                            {menu,[{?__(51,"Actual Distance"),actual},
                                   {?__(52,"Distance Squared"),squared},
                                   {?__(53,"Manhattan"),manhattan},
                                   {?__(54,"Chebychev"),chebychev},
                                   {?__(55,"Minkovsky"),minkovsky}],
                                CellShape,[]}
                            %% End Voronoi Cell Shape Select
                        ],[{margin,false}]},
                        %% End Voronoi Line 1

                        %% Start Voronoi Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(56,"Cell Size")},
                                {text,CellSize,[range(cell_size)]}
                            ]},
                            {hframe, [
                                {label,?__(57,"Intensity")},
                                {text,Intensity,[range(intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Voronoi Line 2

                        %% Start Voronoi Line 3
                        {hframe, [
                            {hframe, [
                                {label,?__(58,"W1")},
                                {text,CellWeight1,[range(cell_weight1)]}
                            ]},
                            {hframe, [
                                {label,?__(59,"W2")},
                                {text,CellWeight2,[range(cell_weight2)]}
                            ]},
                            {hframe, [
                                {label,?__(60,"W3")},
                                {text,CellWeight3,[range(cell_weight3)]}
                            ]},
                            {hframe, [
                                {label,?__(61,"W4")},
                                {text,CellWeight4,[range(cell_weight4)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Voronoi Line 3
                    ],[key({pnl_voronoi,M}),{margin,false},{show,false}]},

                    %% Start Musgrave Specific Procedurals
                    {vframe,[
                        {hframe,[
                            %% Start Musgrave Type Select
                            {menu,[
                                {?__(63,"Multifractal"),multifractal},
                                {?__(64,"Ridged"),ridgedmf},
                                {?__(65,"Hybrid"),hybridmf},
                                {?__(66,"FBM"),fBm}
                            ],MusgraveType,[]},
                            panel,
                            {hframe,[
                                {label,?__(77,"Noise Size")},
                                {text,MusgraveNoiseSize,[range(musgrave_noisesize)]}
                            ]},
                            {hframe,[
                                {label,?__(78,"Intensity")},
                                {text,MusgraveIntensity,[range(musgrave_intensity)]}
                            ]}
                        ],[{margin,false}]},
                        %% End Musgrave Line 1

                        %% Start Musgrave Line 2
                        {hframe, [
                            {hframe, [
                                {label,?__(79,"Contrast (H)")},
                                {text,MusgraveContrast,[range(musgrave_contrast)]}
                            ]},
                            {hframe, [
                                {label,?__(80,"Lacunarity")},
                                {text,MusgraveLacunarity,[range(musgrave_lacunarity)]}
                            ]},
                            {hframe, [
                                {label,?__(81,"Octaves")},
                                {text,MusgraveOctaves,[range(musgrave_octaves)]}
                            ]}
                        ],[{margin,false}]}
                        %% End Musgrave Line 2
                    ],[key({pnl_musgrave,M})]},

                    %% Start Distorted Noise Specific Procedurals
                    {vframe, [
                        {hframe, [
                            %% Start Distorted Noise Type Select
                            {menu,[
                                {?__(87,"Blender-Distort"),blender},
                                {?__(88,"Cellnoise"),cellnoise},
                                {?__(89,"New Perlin"),newperlin},
                                {?__(90,"Perlin"),stdperlin},
                                {?__(91,"Voronoi Crackle"),voronoi_crackle},
                                {?__(92,"Voronoi F1"),voronoi_f1},
                                {?__(93,"Voronoi F2"),voronoi_f2},
                                {?__(94,"Voronoi F3"),voronoi_f3},
                                {?__(95,"Voronoi F4"),voronoi_f4},
                                {?__(96,"Voronoi F1F2"),voronoi_f2f1}
                            ],DistortionType,[]},
                            %% End Distorted Noise Type Select
                            {label,?__(107,"Noise Size")},{text,DistortionNoiseSize,[range(distortion_noisesize)]},
                            {label,?__(108,"Distortion")},{text,DistortionIntensity,[range(distortion_intensity)]}
                        ],[{margin,false}]}
                    ],[key({pnl_dist_noise,M}),{show,false}]}
                ],[key({pnl_type,M})]}
            ],[key({pnl_mod,M})]}
        ]},
    [{?__(35,"Texture")++" "++integer_to_list(M)++mod_legend(Enabled, Mode, Type), ModFrame}];

modulator_dialog(_Modulator, _Maps, _) ->
    []. % Discard old modulators that anyone may have

mod_enabled_mode_type(Ps, Maps) ->
    {Enabled,Mode} =
        case proplists:get_value(mode, Ps, ?DEF_MOD_MODE) of
            off -> {false,?DEF_MOD_MODE};
            Mode1 -> {proplists:get_value(enabled, Ps, ?DEF_MOD_ENABLED),Mode1}
        end,
    Type = proplists:get_value(type, Ps, ?DEF_MOD_TYPE),
    case Type of
        {map,Map} ->
            case lists:keymember(Map, 1, Maps) of
                true -> {Enabled,Mode,Type};
                false -> {false,Mode,?DEF_MOD_TYPE}
            end;
        _ -> {Enabled,Mode,Type}
    end.

mod_legend(Enabled, Mode, {map,Map}) ->
    mod_legend(Enabled, Mode, atom_to_list(Map));
mod_legend(Enabled, Mode, Type) when is_atom(Mode) ->
    mod_legend(Enabled, wings_util:cap(Mode), Type);
mod_legend(Enabled, Mode, Type) when is_atom(Type) ->
    mod_legend(Enabled, Mode, wings_util:cap(Type));
mod_legend(Enabled, Mode, Type) when is_list(Mode), is_list(Type) ->
    case Enabled of
        true -> " ("++?__(1,"enabled")++", ";
        false -> " ("++?__(2,"disabled")++", "
    end++Mode++", "++Type++")".


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
    TypeTag = {?TAG,type,M},
    {EnabledTag,Enabled} = lists:keyfind(EnabledTag, 1, Res1),
    {TypeTag,Type} = lists:keyfind(TypeTag, 1, Res1),
    Res2 = lists:keydelete(EnabledTag, 1, lists:keydelete(TypeTag, 1, Res1)),
    [Mode,AlphaIntensity,TextureType,SizeX,SizeY,SizeZ,
     Diffuse,Shininess,Normal,
     Filename,
     Color1,Color2,Hard,NoiseBasis,NoiseSize,Depth,
     Sharpness,Turbulence,Shape,
     WoodType,CellType,CellShape,CellSize,Intensity,CellWeight1,CellWeight2,CellWeight3,CellWeight4,
     MusgraveType,MusgraveNoiseSize,MusgraveIntensity,MusgraveContrast,
     MusgraveLacunarity,MusgraveOctaves,DistortionType,
     DistortionNoiseSize,DistortionIntensity] =
        case Res2 of
            [Minimized|Rest] when is_boolean(Minimized) -> Rest;  % for keep compatibility with previous W3D version
            Rest -> Rest
        end,
    Ps = [{enabled,Enabled},{mode,Mode},{alpha_intensity,AlphaIntensity},
          {texture_type,TextureType},
          {size_x,SizeX},{size_y,SizeY},{size_z,SizeZ},
          {diffuse,Diffuse},
          {shininess,Shininess},{normal,Normal},
          {type,Type},
          {filename,Filename},{color1,Color1},{color2,Color2},{hard,Hard},
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
        {alpha_intensity,?DEF_MOD_ALPHA_INTENSITY},
        {size_x,?DEF_MOD_SIZE_X},
        {size_y,?DEF_MOD_SIZE_Y},
        {size_z,?DEF_MOD_SIZE_Z},
        {diffuse,?DEF_MOD_DIFFUSE},
%        {specular,?DEF_MOD_SPECULAR},
        {shininess,?DEF_MOD_SHININESS},
        {normal,?DEF_MOD_NORMAL},
        {type,?DEF_MOD_TYPE},
        {filename,?DEF_MOD_FILENAME},
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
