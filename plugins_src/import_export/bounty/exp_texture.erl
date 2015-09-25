%
%
%


export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,_,image} ->
            Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
            export_texture(F, Name, image, Filename);
        {true,_,jpeg} -> %% Old tag
            Filename = proplists:get_value(filename, Ps, ?DEF_MOD_FILENAME),
            export_texture(F, Name, image, Filename);
        {true,_,{map,Map}} ->
            case proplists:get_value(Map, Maps, undefined) of
                undefined ->
                    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
                #e3d_image{name=ImageName}=Image ->
                    MapFile = ImageName++".tga",
                    ok = e3d_image:save(Image,
                                        filename:join(ExportDir, MapFile)),
                    export_texture(F, Name, image, MapFile)
            end;
        {true,_,Type} ->
            export_texture(F, Name, Type, Ps)
    end.

export_texture(F, TexName, image, Filename) ->
    println(F, "<texture name=\"~s\">",[TexName]),
    println(F, "\t<filename sval=\"~s\"/>",[Filename]),
    println(F, "\t<type sval=\"image\"/>"),
    println(F, "\t</texture>");

export_texture(F, Name, Type, Ps) ->
    %
    %TextureNameChg = re:replace(Name,"w_TEmytex_1","TEmytex",[global]),
    println(F, "<texture name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~s\"/>", [format(Type)]),
    %
    export_rgb(F,
        color1, proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1)),
    export_rgb(F,
        color2, proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2)),
    println(F,
        "\t<hard bval=\"~s\"/>",[format(proplists:get_value(hard, Ps, ?DEF_MOD_HARD))]),
    println(F,
        "\t<noise_type sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS)]),
    println(F,
        "\t<size fval=\"~.6f\"/>",[proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE)]),

    case Type of

        clouds ->
            println(F, "\t<depth ival=\"~w\"/>", [proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH)]);

        marble ->
            println(F,
                "\t<depth ival=\"~w\"/>",[proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH)]),
            println(F,
                "\t<turbulence fval=\"~.6f\"/>",[proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE)]),
            println(F,
                "\t<sharpness fval=\"~.6f\"/>",[proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS)]),
            println(F,
                "\t<shape sval=\"~s\"/>",[proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE)]);

        wood ->
            %% Coordinate rotation, see export_pos/3.
            println(F,
                "\t<wood_type sval=\"~s\"/>",[proplists:get_value(wood_type, Ps, ?DEF_MOD_WOODTYPE)]),
            println(F,
                "\t<turbulence fval=\"~.6f\"/>",[proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE)]),
            println(F,
                "\t<shape sval=\"~s\"/>",[proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE)]);

        voronoi ->
            %% Coordinate rotation, see export_pos/3.
            println(F,
                "\t<color_type sval=\"~s\"/>",[proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE)]),
            println(F,
                "\t<distance_metric sval=\"~s\"/>",[proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE)]),
            println(F,
                "\t<intensity fval=\"~.6f\"/>",[proplists:get_value(intensity, Ps,  ?DEF_MOD_INTENSITY)]),
            println(F,
                "\t<weight1 fval=\"~.6f\"/>",[proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1)]),
            println(F,
                "\t<weight2 fval=\"~.6f\"/>",[proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2)]),
            println(F,
                "\t<weight3 fval=\"~.6f\"/>",[proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3)]),
            println(F,
                "\t<weight4 fval=\"~.6f\"/>",[proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4)]);

        musgrave ->
            MusgraveType = proplists:get_value(musgrave_type, Ps, ?DEF_MOD_MUSGRAVE_TYPE),
            NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
            MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),
            MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),
            MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, ?DEF_MOD_MUSGRAVE_CONTRAST),
            MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),
            MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),

            %% Coordinate rotation, see export_pos/3.
            println(F, "\t<musgrave_type sval=\"~s\"/>\n"
                       "\t<noise_type sval=\"~s\"/>~n"
                       "\t<size fval=\"~.6f\"/>\n"
                       "\t<intensity fval=\"~.6f\"/>~n"
                       "\t<H fval=\"~.6f\"/>\n"
                       "\t<lacunarity fval=\"~.6f\"/>~n"
                       "\t<octaves fval=\"~.6f\"/>",
                    [MusgraveType,NoiseBasis,MusgraveNoiseSize,
                     MusgraveIntensity,MusgraveContrast,MusgraveLacunarity,MusgraveOctaves]);

        distorted_noise ->

            %% Coordinate rotation, see export_pos/3.
            println(F,
                "\t<noise_type1 sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS)]),
            println(F,
                "\t<noise_type2 sval=\"~s\"/>",[proplists:get_value(distortion_type, Ps, ?DEF_MOD_DISTORTION_TYPE)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE)]),
            println(F,
                "\t<distort fval=\"~.6f\"/>",[proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY)]);
        _ ->
            ok
    end,
    println(F, "</texture>").

