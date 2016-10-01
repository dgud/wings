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


%% Return object with arealight faces only
%%
export_object(F, NameStr, Mesh=#e3d_mesh{fs=Fs}, MatsGb, Id) ->
    %% Find the default material
    MM = sort(foldl(fun (#e3d_face{mat=[M|_]}, Ms) -> [M|Ms] end, [], Fs)),
    [{_Count,DefaultMaterial}|_] = reverse(sort(count_equal(MM))),
    MatPs = gb_trees:get(DefaultMaterial, MatsGb),
    export_object_1(F, NameStr, Mesh, DefaultMaterial, MatPs, Id).

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

export_object_1(F, NameStr, Mesh0=#e3d_mesh{he=He0}, DefaultMaterial, MatPs, Id) ->
    Attr = proplists:get_value(?TAG, MatPs, []),
    OpenGL = proplists:get_value(opengl, MatPs),
    UseHardness = proplists:get_value(use_hardness, Attr, false),
    Object_Type = proplists:get_value(object_type, Attr, mesh),

    Volume_Sigma_a = proplists:get_value(volume_sigma_a, Attr, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, Attr, ?DEF_VOLUME_SIGMA_S),
    %Volume_Height = proplists:get_value(volume_height, Attr, ?DEF_VOLUME_HEIGHT),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, Attr, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Cover = proplists:get_value(volume_cover, Attr, ?DEF_VOLUME_COVER),
    VolumeX = proplists:get_value(volume_x, Attr, 1.0),
    VolumeY = proplists:get_value(volume_y, Attr, 1.0),
    VolumeZ = proplists:get_value(volume_z, Attr, 1.0),
    VolumeFile = proplists:get_value(volume_file, Attr, ""),

    PortalPower = proplists:get_value(portal_power, Attr, ?DEF_LIGHTPORTAL_POWER),
    PortalSamples = proplists:get_value(portal_samples, Attr, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, Attr, false),
    Meshlight_Color = proplists:get_value(meshlight_color, Attr, ?DEF_MESHLIGHT_COLOR),

    MeshLightDoubleSided = proplists:get_value(meshlight_double_sided, Attr, false),
    AutosmoothAngle =
        proplists:get_value(autosmooth_angle, Attr, ?DEF_AUTOSMOOTH_ANGLE),

    Autosmooth = proplists:get_value(autosmooth, Attr,
                                     if AutosmoothAngle == 0.0 -> false;
                                        true -> ?DEF_AUTOSMOOTH end),

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
    %%

    %% Add Export Object Name Start

    println(F, "<!-- Object Name ~s, Object # ~w -->", [NameStr,Id]),

    %% Add Export Object Name End

    HasUV =  case Tx of
            []-> false;
            _ -> true
        end,

    case Object_Type of
        mesh ->
            println(F," "),
            println(F, "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"0\">",[Id,length(Vs),length(Fs),HasUV]),
            export_vertices(F, Vs);

        volume ->
            println(F, "<volumeregion name=\"~s\">",[NameStr]),

            case proplists:get_value(volume_type, Attr,  uniformvolume) of
                uniformvolume ->
                    println(F, "<type sval=\"UniformVolume\"/>");

                expdensityvolume ->
                    println(F, "\t<type sval=\"ExpDensityVolume\"/>"),
                    println(F, "\t<a fval=\"~.10f\"/>",[proplists:get_value(volume_height, Attr, ?DEF_VOLUME_HEIGHT)]),
                    println(F, "\t<b fval=\"~.10f\"/>",[proplists:get_value(volume_steepness, Attr, 1.0)]);

                noisevolume ->
                    println(F, "\t<type sval=\"NoiseVolume\"/>"),
                    println(F, "\t<sharpness fval=\"~.10f\"/>",[proplists:get_value(volume_sharpness, Attr, 2.0)]),
                    println(F, "\t<cover fval=\"~.10f\"/>",[Volume_Cover]),
                    println(F, "\t<density fval=\"~.10f\"/>",[proplists:get_value(volume_density, Attr, 1.0)]),
                    println(F, "\t<texture sval=\"TEmytex\"/>");

                gridvolume ->
                    println(F, "\t<type sval=\"GridVolume\"/>"),
                    println(F, "\t<density_file sval=\"~s\"/>",[VolumeFile])

            end, %volume type

            Size = proplists:get_value(volume_region_size, Attr, 1.0),
            MinX = VolumeX - (Size * 0.5), MaxX = VolumeX + (Size * 0.5),
            MinY = VolumeY - (Size * 0.5), MaxY = VolumeY + (Size * 0.5),
            MinZ = VolumeZ - (Size * 0.5), MaxZ = VolumeZ + (Size * 0.5),

            println(F, "\t<attgridScale ival=\"~w\"/>",[Volume_Attgridscale]),
            println(F, "\t<maxX fval=\"~.10f\"/>",[MaxZ]),
            println(F, "\t<maxY fval=\"~.10f\"/>",[MaxX]),
            println(F, "\t<maxZ fval=\"~.10f\"/>",[MaxY]),
            println(F, "\t<minX fval=\"~.10f\"/>",[MinZ]),
            println(F, "\t<minY fval=\"~.10f\"/>",[MinX]),
            println(F, "\t<minZ fval=\"~.10f\"/>",[MinY]),
            println(F, "\t<sigma_a fval=\"~.10f\"/>",[Volume_Sigma_a]),
            println(F, "\t<sigma_s fval=\"~.10f\"/>",[Volume_Sigma_s]);

        meshlight ->
            println(F, "<light name=\"~s\">",[NameStr]),

            export_rgb(F, color, proplists:get_value(meshlight_color, Attr, {0.9,0.9,0.9})),

            println(F,
                "\t<double_sided bval=\"~s\"/>",[format(proplists:get_value(meshlight_double_sided, Attr, false))]),
            println(F, "\t<object ival= \"~w\"/>",[Id]),
            println(F,
                "\t<power fval=\"~.10f\"/>",[proplists:get_value(meshlight_power, Attr, 1.0)]),
            println(F,
                "\t<samples ival=\"~w\"/>",[proplists:get_value(meshlight_samples, Attr, 16)]),
            println(F, "\t<type sval=\"~s\"/>",[Object_Type]),
            println(F, "</light>\n"),
            println(F,
                "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"0\">",[Id,length(Vs),length(Fs),HasUV]);

        lightportal ->
            println(F, "\n<light name=\"~s\">",[NameStr]),
            println(F, "\t<object ival= \"~w\"/>",[Id]),
            println(F,
                "\t<photon_only bval=\"~s\"/>",[format(proplists:get_value(lightportal_photon_only, Attr, false))]),
            println(F, "\t<power fval=\"~.10f\"/>",[PortalPower]),
            println(F, "\t<samples ival=\"~w\"/>",[PortalSamples]),
            println(F, "\t<type sval=\"bgPortalLight\"/>"),
            println(F,
                "\t<with_caustic bval=\"~s\"/>",[format(proplists:get_value(lightportal_causticphotons, Attr, false))]),
            println(F,
                "\t<with_diffuse bval=\"~s\"/>",[format(proplists:get_value(portal_diffusephotons, Attr, false))]),
            println(F, "</light>"),
            println(F,
                "\n<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"256\">",[Id,length(Vs),length(Fs),HasUV])
    end,

    %% Add Export UV_Vectors Part 1 Start
    case Object_Type of
        volume ->
            println(F, "</volumeregion>\n");
        _ ->
            export_vertices(F, Vs),
            case HasUV of
                false -> ok;
                true -> println(F, "\t<!--uv_vectors Quantity=\"~w\" -->\n",[length(Tx)]),
                        export_vectors2D(F, Tx)
            end,
            %% Add Export UV_Vectors Part 1 End

            export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc)),
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
    println(F, ["\t<",format(Type)," x=\"",format(Z),
                "\" y=\"",format(X),"\" z=\"",format(Y),"\"/>"]).

%%Add Export UV_Vectors Part 2 Start

export_vectors2D(_F, [])->
    ok;

export_vectors2D(F, [{X, Y} | List])->
    println(F, "<uv u=\"~f\" v=\"~f\"/>", [X, Y]),
    export_vectors2D(F, List).

%%Add Export UV_Vectors Part 2 End

export_faces(_F, [], _DefMat, _TxT, _VColT) ->
    ok;
export_faces(F, [#e3d_face{mat=[Mat|_],tx=Tx,vs=[A,B,C],vc=VCols}|T],

             DefaultMaterial, TxT, VColT) ->

    Shader =
        case Mat of
            DefaultMaterial -> ["\t<set_material sval=\"w_",format(Mat),"\"/>"];
            _ -> ["\t<set_material sval=\"w_",format(Mat),"\"/>"]
        end,

    UVIndices = case Tx of
                    []-> " uv_a=\"0\" uv_b=\"0\" uv_c=\"0\"/>";
                    _ ->
                        {U, V, W} = list_to_tuple(Tx),
                        (io_lib:format(" uv_a=\"~w\" uv_b=\"~w\" uv_c=\"~w\"/>", [U, V, W]))
                end,

    VCol = case {VColT,VCols} of
               {{},[]} -> "";
               {{},_} ->
                   io:format(?__(3,"WARNING! Face refers to non-existing "
                                 "vertex colors\n")),
                   "";
               {_,[]} ->
                   %%io:format("WARNING! Face missing vertex colors~n"),
                   "";
               {_,[VcA,VcB,VcC]} ->
                   {VcAr,VcAg,VcAb} = element(1+VcA, VColT),
                   {VcBr,VcBg,VcBb} = element(1+VcB, VColT),
                   {VcCr,VcCg,VcCb} = element(1+VcC, VColT);
                   %[io_lib:nl(),"           vcol_a_r=\"",format(VcAr),
                   % "\" vcol_a_g=\"",format(VcAg),
                   % "\" vcol_a_b=\"",format(VcAb),"\"",
                    %io_lib:nl(),"           vcol_b_r=\"",format(VcBr),
                    %"\" vcol_b_g=\"",format(VcBg),
                    %"\" vcol_b_b=\"",format(VcBb),"\"",
                    %io_lib:nl(),"           vcol_c_r=\"",format(VcCr),
                    %"\" vcol_c_g=\"",format(VcCg),
                    %"\" vcol_c_b=\"",format(VcCb),"\""];
               _ ->
                   io:format(?__(4,"WARNING! Face has ~w =/= 3 vertex colors")++"~n",
                             [length(VCols)]),
                   ""
           end,
    println(F, [Shader, "\<f a=\"",format(A),
                "\" b=\"",format(B),"\" c=\"",format(C),"\"", UVIndices,
                VCol]),


    export_faces(F, T, DefaultMaterial, TxT, VColT).

