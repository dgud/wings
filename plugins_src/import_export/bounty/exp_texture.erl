%
%
%
%
%%%

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

export_texture(F, Name, image, Filename) ->
    println(F, "<texture name=\"~s\">~n"++
                "    <filename sval=\"~s\"/>~n"++
                "<type sval=\"image\"/>~n" ++
                "</texture>", [Name,Filename]);
export_texture(F, Name, Type, Ps) ->

%%% Start Work-Around for YafaRay Texture Name TEmytex Requirement for Noise Volume

    TextureNameChg = re:replace(Name,"w_TEmytex_1","TEmytex",[global]),
    println(F, "<texture name=\"~s\"> <type sval=\"~s\"/>", [TextureNameChg,format(Type)]),

%%% End Work-Around for YafaRay Texture Name TEmytex Requirement for Noise Volume

    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    Hard = proplists:get_value(hard, Ps, ?DEF_MOD_HARD),
    NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),
    NoiseSize = proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE),
    export_rgb(F, color1, Color1),
    export_rgb(F, color2, Color2),
    println(F, "    <hard bval=\"~s\"/>" " <noise_type sval=\"~s\"/>"  " <size fval=\"~.6f\"/>", [format(Hard),NoiseBasis,NoiseSize]),

    case Type of

        clouds ->
            Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),
            println(F, "  <depth ival=\"~w\"/>" , [Depth]);

        marble ->
            Depth = proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH),

            Turbulence = proplists:get_value(turbulence, Ps,
                                             ?DEF_MOD_TURBULENCE),

            Sharpness = proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS),

            Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),

            println(F, "  <depth ival=\"~w\"/>" "      <turbulence fval=\"~.6f\"/>~n" "  <sharpness fval=\"~.6f\"/>" " <shape sval=\"~s\"/>", [Depth,Turbulence,Sharpness,Shape]);
        wood ->
            WoodType = proplists:get_value(wood_type, Ps,
                                           ?DEF_MOD_WOODTYPE),


            Turbulence = proplists:get_value(turbulence, Ps,
                                             ?DEF_MOD_TURBULENCE),

            Shape = proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE),

            %% Coordinate rotation, see export_pos/3.
            println(F, " <wood_type sval=\"~s\"/>"++
                        "    <turbulence fval=\"~.6f\"/>~n" " <shape sval=\"~s\"/>",
                    [WoodType,Turbulence,Shape]);

        voronoi ->
            CellType = proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE),

            CellShape = proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE),

            CellSize = proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE),

            Intensity = proplists:get_value(intensity, Ps,  ?DEF_MOD_INTENSITY),

            CellWeight1 = proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1),

            CellWeight2 = proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2),

            CellWeight3 = proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3),

            CellWeight4 = proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4),

            %% Coordinate rotation, see export_pos/3.
            println(F, " <color_type sval=\"~s\"/>"++
                        "   <distance_metric sval=\"~s\"/>~n"
                    "    <size fval=\"~.6f\"/>"++
                        "    <intensity fval=\"~.6f\"/>~n"
                    "    <weight1 fval=\"~.6f\"/>"++
                        "    <weight2 fval=\"~.6f\"/>~n"
                    "    <weight3 fval=\"~.6f\"/>"++
                        "    <weight4 fval=\"~.6f\"/>",
                    [CellType,CellShape,CellSize,Intensity,CellWeight1,CellWeight2,CellWeight3,CellWeight4]);

        musgrave ->
            MusgraveType = proplists:get_value(musgrave_type, Ps,
                                               ?DEF_MOD_MUSGRAVE_TYPE),

            NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),

            MusgraveNoiseSize = proplists:get_value(musgrave_noisesize, Ps, ?DEF_MOD_MUSGRAVE_NOISESIZE),

            MusgraveIntensity = proplists:get_value(musgrave_intensity, Ps, ?DEF_MOD_MUSGRAVE_INTENSITY),

            MusgraveContrast = proplists:get_value(musgrave_contrast, Ps, ?DEF_MOD_MUSGRAVE_CONTRAST),

            MusgraveLacunarity = proplists:get_value(musgrave_lacunarity, Ps, ?DEF_MOD_MUSGRAVE_LACUNARITY),

            MusgraveOctaves = proplists:get_value(musgrave_octaves, Ps, ?DEF_MOD_MUSGRAVE_OCTAVES),

            %% Coordinate rotation, see export_pos/3.
            println(F, " <musgrave_type sval=\"~s\"/>"++
                        " <noise_type sval=\"~s\"/>~n"
                    "    <size fval=\"~.6f\"/>"++
                        "    <intensity fval=\"~.6f\"/>~n"
                    "    <H fval=\"~.6f\"/>"++
                        "    <lacunarity fval=\"~.6f\"/>~n"
                    "    <octaves fval=\"~.6f\"/>",
                    [MusgraveType,NoiseBasis,MusgraveNoiseSize,
                     MusgraveIntensity,MusgraveContrast,MusgraveLacunarity,MusgraveOctaves]);

        distorted_noise ->

            NoiseBasis = proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS),

            DistortionType = proplists:get_value(distortion_type, Ps,
                                                 ?DEF_MOD_DISTORTION_TYPE),

            DistortionNoiseSize = proplists:get_value(distortion_noisesize, Ps, ?DEF_MOD_DISTORTION_NOISESIZE),

            DistortionIntensity = proplists:get_value(distortion_intensity, Ps, ?DEF_MOD_DISTORTION_INTENSITY),


            %% Coordinate rotation, see export_pos/3.
            println(F, " <noise_type1 sval=\"~s\"/>"++
                        " <noise_type2 sval=\"~s\"/>~n"
                    "    <size fval=\"~.6f\"/>"++
                        "    <distort fval=\"~.6f\"/>~n",
                    [NoiseBasis,DistortionType,DistortionNoiseSize,
                     DistortionIntensity]);


        _ ->
            ok
    end,
    println(F, "</texture>").

