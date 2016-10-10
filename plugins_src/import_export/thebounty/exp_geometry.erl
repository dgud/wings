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


%% Return object with arealight faces only.. ¿what?
export_object(F, NameStr, Mesh=#e3d_mesh{fs=Fs}, MatsGb, Id) ->
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    MatPs = gb_trees:get(DefaultMaterial, MatsGb),
    export_geometry(F, NameStr, Mesh, DefaultMaterial, MatPs, Id).

%% Count the number of subsequent equal elements in the list.
%% Returns list of {Count,Element}.
%%
count_equal([H|T]) ->
    count_equal(T, 1, H, []).
%%
count_equal([], C, H, R) ->
    [{C,H}|R];
count_equal([H|T], C, H, R) ->
    count_equal(T, C+1, H, R);
count_equal([H|T], C, K, R) ->
    count_equal(T, 1, H, [{C,K}|R]).

export_geometry(F, NameStr, Mesh0=#e3d_mesh{he=He0}, DefaultMaterial, MatPs, Id) ->
    Attr = proplists:get_value(?TAG, MatPs, []),
    _OpenGL = proplists:get_value(opengl, MatPs),
    UseHardness = proplists:get_value(use_hardness, Attr, false),
    ObjectType = proplists:get_value(object_type, Attr, mesh),
    AutosmoothAngle = proplists:get_value(autosmooth_angle, Attr, 90.0),
    Autosmooth = proplists:get_value(autosmooth, Attr, AutosmoothAngle =/= 0.0),

    %% Pre-process mesh
    Mesh1 = #e3d_mesh{} =
        case {He0,UseHardness} of
            {[_|_],true} ->
                io:format(?__(1,"Mesh ~s: slitting hard edges..."), [NameStr]),
                M1 = e3d_mesh:slit_hard_edges(Mesh0, [slit_end_vertices]),
                io:format(?__(2,"done")++"~n"),
                M1;
            _ -> Mesh0
        end,

    io:format(?__(3,"Mesh ~s: triangulating..."), [NameStr]),
    #e3d_mesh{fs=Fs,vs=Vs,vc=Vc,tx=Tx} = e3d_mesh:triangulate(Mesh1),
    io:format(?__(4,"done")++"~n"),
    io:format(?__(5,"Mesh ~s: exporting..."), [NameStr]),

    %% Add export object name
    %% println(F, "<!-- Object Name ~s, Object # ~w -->", [NameStr,Id]),

    HasUV =  case Tx of []-> false; _ -> true end,

    case ObjectType of
        mesh ->
            println(F,
                "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"0\">",[Id,length(Vs),length(Fs),HasUV]);

        volume ->
            println(F, "<volumeregion name=\"~s\">",[NameStr]),

            case proplists:get_value(volume_type, Attr,  uniformvolume) of
                uniformvolume ->
                    println(F, "\t<type sval=\"UniformVolume\"/>");

                expdensityvolume ->
                    println(F, "\t<type sval=\"ExpDensityVolume\"/>"),
                    println(F, "\t<a fval=\"~.10f\"/>",[proplists:get_value(volume_height, Attr, 0.5)]),
                    println(F, "\t<b fval=\"~.10f\"/>",[proplists:get_value(volume_steepness, Attr, 1.0)]);

                noisevolume ->
                    println(F, "\t<type sval=\"NoiseVolume\"/>"),
                    println(F, "\t<sharpness fval=\"~.10f\"/>",[proplists:get_value(volume_sharpness, Attr, 2.0)]),
                    println(F, "\t<cover fval=\"~.10f\"/>",    [proplists:get_value(volume_cover, Attr, 0.05)]),
                    println(F, "\t<density fval=\"~.10f\"/>",  [proplists:get_value(volume_density, Attr, 1.0)]),
                    %! small hard coded for test :). 
                    %! This code asume that the noise texture are in the first slot.
                    println(F, "\t<texture sval=\"w_~s\"/>",[format(NameStr)++"_1"]);
                    % TODO:  texture noise don't work atm..

                gridvolume ->
                    println(F, "\t<type sval=\"GridVolume\"/>"),
                    println(F, "\t<density_file sval=\"~s\"/>",[format(proplists:get_value(volume_file, Attr, ""))])

            end, %volume type

            %! TODO: temporaly solution for volume domain.
            %! Is need improve this code using down/left <> up/right corner coordinates.
            VolumeX = proplists:get_value(volume_x, Attr, 1.0),
            VolumeY = proplists:get_value(volume_y, Attr, 1.0),
            VolumeZ = proplists:get_value(volume_z, Attr, 1.0),

            Size = proplists:get_value(volume_region_size, Attr, 1.0),
            MinX = VolumeX - (Size * 0.5), MaxX = VolumeX + (Size * 0.5),
            MinY = VolumeY - (Size * 0.5), MaxY = VolumeY + (Size * 0.5),
            MinZ = VolumeZ - (Size * 0.5), MaxZ = VolumeZ + (Size * 0.5),

            println(F, "\t<maxX fval=\"~.10f\"/>",[MaxZ]),
            println(F, "\t<maxY fval=\"~.10f\"/>",[MaxX]),
            println(F, "\t<maxZ fval=\"~.10f\"/>",[MaxY]),
            println(F, "\t<minX fval=\"~.10f\"/>",[MinZ]),
            println(F, "\t<minY fval=\"~.10f\"/>",[MinX]),
            println(F, "\t<minZ fval=\"~.10f\"/>",[MinY]),
            println(F, "\t<attgridScale ival=\"~w\"/>",[proplists:get_value(volume_attgridscale, Attr, 1)]),
            println(F, "\t<sigma_a fval=\"~.10f\"/>",[proplists:get_value(volume_sigma_a, Attr, 0.4)]),
            println(F, "\t<sigma_s fval=\"~.10f\"/>",[proplists:get_value(volume_sigma_s, Attr, 0.05)]);

        meshlight ->
            % material..
            println(F, "<material name=\"w_~s\">",[NameStr]),
            println(F, "\t<type sval=\"light_mat\"/>"),
            export_rgb(F, color, proplists:get_value(lightmat_color, Attr, {0.9,0.9,0.9})),
            println(F,
                "\t<double_sided bval=\"~s\"/>",[format(proplists:get_value(lightmat_double_sided, Attr, false))]),
            println(F,
                "\t<power fval=\"~.10f\"/>",[proplists:get_value(lightmat_power, Attr, 1.0)]),
            println(F, "</material>\n"),
            % light..
            println(F, "<light name=\"~s_ml\">",[NameStr]),
            export_rgb(F, color, proplists:get_value(lightmat_color, Attr, {0.9,0.9,0.9})),
            println(F,
                "\t<double_sided bval=\"~s\"/>",[format(proplists:get_value(lightmat_double_sided, Attr, false))]),
            println(F, "\t<object ival= \"~w\"/>",[Id]),
            println(F,
                "\t<power fval=\"~.10f\"/>",[proplists:get_value(lightmat_power, Attr, 1.0)]),
            println(F,
                "\t<samples ival=\"~w\"/>",[proplists:get_value(lightmat_samples, Attr, 16)]),
            println(F, "\t<type sval=\"meshlight\"/>"),
            println(F, "</light>\n"),
            % extra!! implementing option for hide mesh. You only need set 'type' value to 256
            Hide = proplists:get_value(hide, Attr, false),
            Typ = case Hide of true -> 256; _ -> 0 end,
            % mesh object..
            println(F,
                "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"~w\">",[Id,length(Vs),length(Fs),HasUV,Typ]);

        lightportal ->
            % light...
            println(F, "\n<light name=\"~s\">",[NameStr]),
            println(F, "\t<type sval=\"bgPortalLight\"/>"),
            println(F, "\t<object ival= \"~w\"/>",[Id]),
            println(F,
                "\t<photon_only bval=\"~s\"/>",[format(proplists:get_value(portal_photon_only, Attr, false))]),
            println(F,
                "\t<power fval=\"~.10f\"/>",[proplists:get_value(portal_power, Attr, 1.0)]),
            println(F,
                "\t<samples ival=\"~w\"/>",[proplists:get_value(portal_samples, Attr, 16)]),
            println(F,
                "\t<with_caustic bval=\"~s\"/>",[format(proplists:get_value(portal_causticphotons, Attr, false))]),
            println(F,
                "\t<with_diffuse bval=\"~s\"/>",[format(proplists:get_value(portal_diffusephotons, Attr, false))]),
            println(F, "</light>\n"),
            % mesh object..
            println(F,
                "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"256\">",[Id,length(Vs),length(Fs),HasUV])
    end,

    %%
    case ObjectType of
        volume ->
            println(F, "</volumeregion>\n");
        _ ->
            export_vertices(F, Vs),
            export_faces(F, Fs, ObjectType, NameStr, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc)),
            case HasUV of
                false -> ok;
                true -> export_vectors2D(F, Tx)
            end,

            println(F, "</mesh>\n"),

            case Autosmooth of
                true ->
                    println(F, "<smooth ID=\"~w\" angle=\"~.3f\"/>", [Id,AutosmoothAngle]);
                _ -> ok
            end
        end,

    io:format(?__(6,"done")++"~n").

export_vertices(_F, []) ->
    ok;
export_vertices(F, [Pos|T]) ->
    export_pos(F, p, Pos),
    export_vertices(F, T).


%% The coordinate system is rotated to make sunsky background
%% and environment images work as expected.
%% It assumes `South Y=East Z=Up in TheBounty coordinates.
%% Hence Z=South, X=East, Y=Up in Wings coordinates.
%%
export_pos(F, Type, {X,Y,Z}) ->
    println(F, ["\t<",format(Type)," x=\"",format(Z),"\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).

%%Add Export UV_Vectors Part 2 Start

export_vectors2D(_F, [])->
    ok;

export_vectors2D(F, [{X, Y} | List])->
    println(F, "\t<uv u=\"~f\" v=\"~f\"/>", [X, Y]),
    export_vectors2D(F, List).


export_faces(_F, [], _Object, _Name, _DefMat, _TxT, _VColT) ->
    ok;

export_faces(F, [#e3d_face{mat=[Mat|_],tx=Tx,vs=[A,B,C],vc=_VCols}|T], ObjectType, NameStr, DefaultMaterial, TxT, VColT) ->


    Material =
        case ObjectType of
            meshlight -> NameStr;
            _ -> Mat
        end,
    UVIndices =
        case Tx of
            []-> "/>";
            _ ->
                {U, V, W} = list_to_tuple(Tx),
                (io_lib:format(" uv_a=\"~w\" uv_b=\"~w\" uv_c=\"~w\"/>", [U, V, W]))
        end,

    println(F, "\t<set_material sval=\"w_~s\"/>",[format(Material)]),
    println(F, "\t<f a=\"~w\" b=\"~w\" c=\"~w\"~s",[A,B,C,UVIndices]),

    export_faces(F, T, ObjectType, NameStr, DefaultMaterial, TxT, VColT).

