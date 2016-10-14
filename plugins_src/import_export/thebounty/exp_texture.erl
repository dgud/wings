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

export_texture(F, Name, Maps, ExportDir, {modulator,Ps}) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,_,image} ->
            Filename = proplists:get_value(image_filename, Ps, ?DEF_MOD_FILENAME),
            export_image_texture(F, Name, Filename, Ps);
        {true,_,jpeg} -> %% Old tag
            Filename = proplists:get_value(image_filename, Ps, ?DEF_MOD_FILENAME),
            export_image_texture(F, Name, Filename, Ps);
        {true,_,{map,Map}} ->
            case proplists:get_value(Map, Maps, undefined) of
                undefined ->
                    exit({unknown_texture_map,{?MODULE,?LINE,[Name,Map]}});
                #e3d_image{name=ImageName, filename = FileName}=Image ->
                    case FileName of
                        none ->
                            MapFile =
                                case get_map_type(ImageName) of
                                    sys -> ImageName++".png";
                                    _ -> ImageName
                                end,
                            Filepath0 = filename:join(ExportDir, MapFile),
                            case e3d_image:save(Image, Filepath0) of
                                {error, _} -> % file type not supported by Wings3d
                                    Filepath = filename:join(ExportDir, set_map_type(ImageName,".png")),
                                    e3d_image:save(Image, Filepath);
                                _ -> Filepath = Filepath0
                            end;
                        _ -> Filepath = FileName
                    end,
                    export_image_texture(F, Name, Filepath, Ps)
            end;
        {true,_,Type} ->
            export_texture(F, Name, Type, Ps)
    end.

export_image_texture(F, TexName, Filename, Ps) ->
    println(F, "<texture name=\"~s\">",[TexName]),
    println(F, "\t<filename sval=\"~s\"/>",[Filename]),
    println(F, "\t<type sval=\"image\"/>"),
    println(F, "\t<use_alpha bval=\"~s\"/>",[format(proplists:get_value(use_alpha, Ps, false))]),
    println(F, "\t<calc_alpha bval=\"~s\"/>",[format(proplists:get_value(calc_alpha, Ps, false))]),
    println(F, "\t<clipping sval=\"~s\"/>",[proplists:get_value(clipping, Ps, repeat)]),
    println(F, "\t<xrepeat ival=\"~w\"/>",[proplists:get_value(repeat_x, Ps, 1)]),
    println(F, "\t<yrepeat ival=\"~w\"/>",[proplists:get_value(repeat_y, Ps, 0.0)]),
    println(F, "\t<cropmax_x fval=\"~.2f\"/>",[proplists:get_value(crop_maxx, Ps, 0.0)]),
    println(F, "\t<cropmax_y fval=\"~.2f\"/>",[proplists:get_value(crop_maxy, Ps, 0.0)]),
    println(F, "\t<cropmin_x fval=\"~.2f\"/>",[proplists:get_value(crop_minx, Ps, 0.0)]),
    println(F, "\t<cropmin_y fval=\"~.2f\"/>",[proplists:get_value(crop_miny, Ps, 0.0)]),
    println(F, "\t<even_tiles bval=\"~s\"/>",[format(proplists:get_value(even_alpha, Ps, false))]),
    println(F, "\t<odd_tiles bval=\"~s\"/>",[format(proplists:get_value(odd, Ps, false))]),
    println(F, "\t<checker_dist fval=\"~.2f\"/>",[proplists:get_value(distance, Ps, 0.0)]),
    println(F, "\t<interpolate sval=\"~s\"/>",[proplists:get_value(interpolate, Ps, none)]),
    println(F, "\t<rot90 bval=\"~s\"/>",[proplists:get_value(flip_axis, Ps, false)]),
    println(F, "\t<normalmap bval=\"false\"/>"),
    println(F, "\t<gamma fval=\"~.2f\"/>",[proplists:get_value(gamma_input, Ps, 2.2)]),
    println(F, "\t</texture>").

export_texture(F, Name, Type, Ps) ->
    %
    println(F, "<texture name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~s\"/>", [format(Type)]),
    Color1 = proplists:get_value(color1, Ps, ?DEF_MOD_COLOR1),
    Color2 = proplists:get_value(color2, Ps, ?DEF_MOD_COLOR2),
    case Type of
        blend -> ok;
        _ ->
        export_rgb(F, color1, Color1),
        export_rgb(F, color2, Color2)
    end,

    case Type of

        clouds ->
            %% <depth, hard, noise_type, size
            println(F,
                "\t<depth ival=\"~w\"/>", [proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH)]),
            println(F,
                "\t<hard bval=\"~s\"/>",[format(proplists:get_value(hard, Ps, false))]),
            println(F,
                "\t<noise_type sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE)]);

        marble ->
            %% <depth, hard, noise_type, shape, sharpness, size, turbulence
            println(F,
                "\t<depth ival=\"~w\"/>",[proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH)]),
            println(F,
                "\t<hard bval=\"~s\"/>",[format(proplists:get_value(hard, Ps, false))]),
            println(F,
                "\t<noise_type sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS)]),
            println(F,
                "\t<shape sval=\"~s\"/>",[proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE)]),
            println(F,
                "\t<sharpness fval=\"~.6f\"/>",[proplists:get_value(sharpness, Ps, ?DEF_MOD_SHARPNESS)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE)]),
            println(F,
                "\t<turbulence fval=\"~.6f\"/>",[proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE)]);

        wood ->
            %% <depth, hard, noise_type, shape, size, turbulence, wood_type
            println(F,
                "\t<depth ival=\"~w\"/>",[proplists:get_value(depth, Ps, ?DEF_MOD_DEPTH)]),
            println(F,
                "\t<hard bval=\"~s\"/>",[format(proplists:get_value(hard, Ps, false))]),
            println(F,
                "\t<noise_type sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, ?DEF_MOD_NOISEBASIS)]),
            println(F,
                "\t<shape sval=\"~s\"/>",[proplists:get_value(shape, Ps, ?DEF_MOD_SHAPE)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(noise_size, Ps, ?DEF_MOD_NOISESIZE)]),
            println(F,
                "\t<turbulence fval=\"~.6f\"/>",[proplists:get_value(turbulence, Ps, ?DEF_MOD_TURBULENCE)]),
            println(F,
                "\t<wood_type sval=\"~s\"/>",[proplists:get_value(wood_type, Ps, ?DEF_MOD_WOODTYPE)]);

        voronoi ->
            %% color_type, distance_metric, intensity, mk_exponent, size, weight1, weight2, weight3, weight4
            println(F,
                "\t<color_type sval=\"~s\"/>",[proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE)]),
            println(F,
                "\t<distance_metric sval=\"~s\"/>",[proplists:get_value(cell_shape, Ps, ?DEF_MOD_CELLSHAPE)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(cell_size, Ps, ?DEF_MOD_CELLSIZE)]),
            println(F,
                "\t<intensity fval=\"~.6f\"/>",[proplists:get_value(intensity, Ps,  ?DEF_MOD_INTENSITY)]),
            println(F,
                "\t<mk_exponent fval=\"~.6f\"/>",[proplists:get_value(mk_exponent, Ps,  2.5)]),
            println(F,
                "\t<weight1 fval=\"~.6f\"/>",[proplists:get_value(cell_weight1, Ps, ?DEF_MOD_CELL_WEIGHT1)]),
            println(F,
                "\t<weight2 fval=\"~.6f\"/>",[proplists:get_value(cell_weight2, Ps, ?DEF_MOD_CELL_WEIGHT2)]),
            println(F,
                "\t<weight3 fval=\"~.6f\"/>",[proplists:get_value(cell_weight3, Ps, ?DEF_MOD_CELL_WEIGHT3)]),
            println(F,
                "\t<weight4 fval=\"~.6f\"/>",[proplists:get_value(cell_weight4, Ps, ?DEF_MOD_CELL_WEIGHT4)]);

        musgrave ->
            %% <H, gain, intensity, lacunarity, musgrave_type, noise_type, octaves, offset, size
            println(F,
                "\t<H fval=\"~.6f\"/>",[proplists:get_value(musgrave_contrast, Ps, 0.1)]),
            println(F,
                "\t<gain fval=\"~.6f\"/>",[proplists:get_value(gain, Ps, 1.0)]),
            println(F,
                "\t<intensity fval=\"~.6f\"/>",[proplists:get_value(musgrave_intensity, Ps, 2.0)]),
            println(F,
                "\t<lacunarity fval=\"~.6f\"/>",[proplists:get_value(musgrave_lacunarity, Ps, 2.0)]),
            println(F,
                "\t<musgrave_type sval=\"~s\"/>",[proplists:get_value(musgrave_type, Ps, multifractal)]),
            println(F,
                "\t<noise_type sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, blender)]),
            println(F,
                "\t<octaves fval=\"~.6f\"/>",[proplists:get_value(musgrave_octaves, Ps, 8.0)]),
            println(F,
                "\t<offset fval=\"~.6f\"/>",[proplists:get_value(offset, Ps, 1.0)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(musgrave_noisesize, Ps, 0.5)]);

        distorted_noise ->

            %% distort, noise_type1, noise_type2, size
            println(F,
                "\t<distort fval=\"~.6f\"/>",[proplists:get_value(distortion_intensity, Ps, 10.0)]),
            println(F,
                "\t<noise_type1 sval=\"~s\"/>",[proplists:get_value(noise_basis, Ps, blender)]),
            println(F,
                "\t<noise_type2 sval=\"~s\"/>",[proplists:get_value(distortion_type, Ps, blender)]),
            println(F,
                "\t<size fval=\"~.6f\"/>",[proplists:get_value(distortion_noisesize, Ps, 1.0)]);
        blend ->
            println(F,
                "\t<stype sval=\"~s\"/>",[proplists:get_value(progression, Ps, lin)]);
        _ ->
            ok
    end,
    println(F, "</texture>").

% add from micheus
get_map_type(Filepath) ->
    Ext = filename:extension(Filepath),
    case Ext of
        ".tga" -> tga;
        ".jpg" -> jpg;
        ".png" -> png;
        ".tiff" -> tiff;
        ".hdr" -> hdr;
        ".exr" -> exr;
        _ -> sys
    end.

%images_format() ->
%    ImgInfo = wings_job:render_formats(),
%    lists:foldr(fun(Type,Acc) ->
%        case lists:keyfind(Type,1,ImgInfo) of
%            {_,Ext,Desc} -> Acc ++[{Ext,Desc}];
%            _ -> Acc
%        end
%   end, [{".tiff","Tagged Image File Format"}], [tga,jpg,png,hdr,exr]).

%%% Ext parameter must include the "." - ex. ".jpg"
%%% that will replace the extension in case the file name already includes it.
set_map_type(Filepath0,Ext) ->
    Filepath = filename:rootname(Filepath0),
    Filepath ++ Ext.

