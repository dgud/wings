%%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 - 2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the include GNU General Public License file for more details.
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
    _OpenGL = proplists:get_value(opengl, MatPs),
    UseHardness = proplists:get_value(use_hardness, Attr, ?DEF_USE_HARDNESS),
    Object_Type = proplists:get_value(object_type, Attr, ?DEF_OBJECT_TYPE),

    Volume_Sigma_a = proplists:get_value(volume_sigma_a, Attr, ?DEF_VOLUME_SIGMA_A),
    Volume_Sigma_s = proplists:get_value(volume_sigma_s, Attr, ?DEF_VOLUME_SIGMA_S),
    Volume_Height = proplists:get_value(volume_height, Attr, ?DEF_VOLUME_HEIGHT),
    Volume_Steepness = proplists:get_value(volume_steepness, Attr, ?DEF_VOLUME_STEEPNESS),
    Volume_Attgridscale = proplists:get_value(volume_attgridscale, Attr, ?DEF_VOLUME_ATTGRIDSCALE),
    Volume_Sharpness = proplists:get_value(volume_sharpness, Attr, ?DEF_VOLUME_SHARPNESS),
    Volume_Cover = proplists:get_value(volume_cover, Attr, ?DEF_VOLUME_COVER),
    Volume_Density = proplists:get_value(volume_density, Attr, ?DEF_VOLUME_DENSITY),
    Volume_Minmax_X = proplists:get_value(volume_minmax_x, Attr, ?DEF_VOLUME_MINMAX_X),
    Volume_Minmax_Y = proplists:get_value(volume_minmax_y, Attr, ?DEF_VOLUME_MINMAX_Y),
    Volume_Minmax_Z = proplists:get_value(volume_minmax_z, Attr, ?DEF_VOLUME_MINMAX_Z),
    Lightportal_Power = proplists:get_value(lightportal_power, Attr, ?DEF_LIGHTPORTAL_POWER),
    Lightportal_Samples = proplists:get_value(lightportal_samples, Attr, ?DEF_LIGHTPORTAL_SAMPLES),
    Lightportal_Diffusephotons = proplists:get_value(lightportal_diffusephotons, Attr, ?DEF_LIGHTPORTAL_DIFFUSEPHOTONS),
    Lightportal_Causticphotons = proplists:get_value(lightportal_causticphotons, Attr, ?DEF_LIGHTPORTAL_CAUSTICPHOTONS),
    Lightportal_Photon_Only = proplists:get_value(lightportal_photon_only, Attr, ?DEF_LIGHTPORTAL_PHOTON_ONLY),
    Meshlight_Power = proplists:get_value(meshlight_power, Attr, ?DEF_MESHLIGHT_POWER),
    Meshlight_Samples = proplists:get_value(meshlight_samples, Attr, ?DEF_MESHLIGHT_SAMPLES),
    Meshlight_Color = proplists:get_value(meshlight_color, Attr, ?DEF_MESHLIGHT_COLOR),

    Meshlight_Double_Sided = proplists:get_value(meshlight_double_sided, Attr, ?DEF_MESHLIGHT_DOUBLE_SIDED),
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

    println(F, "<!--Object Name ~s, Object # ~w-->", [NameStr,Id]),

    %% Add Export Object Name End

    HasUV = case Tx of
                []-> "false";
                _ ->
                    "true"

            end,

    case Object_Type of
        mesh ->
            println(F," "),
            println(F, "<mesh id=\"~w\" vertices=\"~w\" faces=\"~w\" has_uv=\"~s\" type=\"0\">",[Id,length(Vs),length(Fs),HasUV]),
            println(F," ");

        volume ->
            println(F," "),
            println(F, "<volumeregion name=\"volumename\">"),

            case proplists:get_value(volume_type, Attr,
                                     ?DEF_VOLUME_TYPE) of
                uniformvolume ->
                    println(F, "<type sval=\"UniformVolume\"/>");


                expdensityvolume ->
                    println(F, "<type sval=\"ExpDensityVolume\"/>"),
                    println(F, "<a fval=\"~.10f\"/>",[Volume_Height]),
                    println(F, "<b fval=\"~.10f\"/>",[Volume_Steepness]);


                noisevolume ->
                    println(F, "<type sval=\"NoiseVolume\"/>"),
                    println(F, "<sharpness fval=\"~.10f\"/>",[Volume_Sharpness]),
                    println(F, "<cover fval=\"~.10f\"/>",[Volume_Cover]),
                    println(F, "<density fval=\"~.10f\"/>",[Volume_Density]),
                    println(F, "<texture sval=\"TEmytex\"/>")

            end,


            println(F, "<attgridScale ival=\"~w\"/>",[Volume_Attgridscale]),
            println(F, "<maxX fval=\"~.10f\"/>",[Volume_Minmax_Z]),
            println(F, "<maxY fval=\"~.10f\"/>",[Volume_Minmax_X]),
            println(F, "<maxZ fval=\"~.10f\"/>",[Volume_Minmax_Y]),
            println(F, "<minX fval=\"-\~.10f\"/>",[Volume_Minmax_Z]),
            println(F, "<minY fval=\"-\~.10f\"/>",[Volume_Minmax_X]),
            println(F, "<minZ fval=\"-\~.10f\"/>",[Volume_Minmax_Y]),
            println(F, "<sigma_a fval=\"~.10f\"/>",[Volume_Sigma_a]),
            println(F, "<sigma_s fval=\"~.10f\"/>",[Volume_Sigma_s]),
            println(F," ");


        meshlight ->
            println(F," "),
            println(F, "<light name=\"~s\">",[NameStr]),

            export_rgb(F, color, proplists:get_value(meshlight_color, Attr, Meshlight_Color)),

            println(F, "<object ival= \"~w\"/>",[Id]),
            println(F, "<power fval=\"~.10f\"/>",[Meshlight_Power]),
            println(F, "<samples ival=\"~w\"/>",[Meshlight_Samples]),
            println(F, "<double_sided bval=\"~s\"/>",[Meshlight_Double_Sided]),
            println(F, "<type sval=\"~s\"/>",[Object_Type]),
            println(F, "</light>"),
            println(F," "),
            println(F, "<mesh id=\"~w\" type=\"0\">",[Id]),
            println(F," ");

        lightportal ->
            println(F," "),
            println(F, "<light name=\"~s\">",[NameStr]),
            println(F, "<type sval=\"bgPortalLight\"/>"),
            println(F, "<power fval=\"~.10f\"/>",[Lightportal_Power]),
            println(F, "<samples ival=\"~w\"/>",[Lightportal_Samples]),
            println(F, "<object ival= \"~w\"/>",[Id]),
            println(F, "<with_diffuse bval=\"~s\"/>",[Lightportal_Diffusephotons]),
            println(F, "<with_caustic bval=\"~s\"/>",[Lightportal_Causticphotons]),
            println(F, "<photon_only bval=\"~s\"/>",[Lightportal_Photon_Only]),
            println(F," ")

    end,

    export_vertices(F, Vs),

    %% Add Export UV_Vectors Part 1 Start
    case HasUV of
        "false" -> ok;
        "true" -> println(F, "        "),
                  println(F, "<!--uv_vectors Quantity=\"~w\" -->",[length(Tx)]),
                  println(F, "        "),
                  export_vectors2D(F, Tx)
    end,
    %% Add Export UV_Vectors Part 1 End

    export_faces(F, Fs, DefaultMaterial, list_to_tuple(Tx), list_to_tuple(Vc)),

    case Object_Type of
        mesh ->
            println(F," "),
            println(F, "</mesh>"),
            println(F," ");

        volume ->
            println(F," "),
            println(F, "</volumeregion>"),
            println(F," ");

        meshlight ->
            println(F," "),
            println(F, "</mesh>"),
            println(F," ");

        lightportal ->
            println(F," "),
            println(F, "</light>"),
            println(F," ")
    end,

    case Autosmooth of
        false ->
            println(F, "");
        true ->
            println(F, "    <smooth ID=\"~w\" angle=\"~.3f\"/>", [Id,AutosmoothAngle])
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
            DefaultMaterial -> ["  <set_material sval=\"w_",format(Mat),"\"/>"];
            _ -> ["  <set_material sval=\"w_",format(Mat),"\"/>"]
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
                                 "vertex colors")++"~n"),
                   "";
               {_,[]} ->
                   %%io:format("WARNING! Face missing vertex colors~n"),
                   "";
               {_,[VcA,VcB,VcC]} ->
                   {VcAr,VcAg,VcAb} = element(1+VcA, VColT),
                   {VcBr,VcBg,VcBb} = element(1+VcB, VColT),
                   {VcCr,VcCg,VcCb} = element(1+VcC, VColT),
                   [io_lib:nl(),"           vcol_a_r=\"",format(VcAr),
                    "\" vcol_a_g=\"",format(VcAg),
                    "\" vcol_a_b=\"",format(VcAb),"\"",
                    io_lib:nl(),"           vcol_b_r=\"",format(VcBr),
                    "\" vcol_b_g=\"",format(VcBg),
                    "\" vcol_b_b=\"",format(VcBb),"\"",
                    io_lib:nl(),"           vcol_c_r=\"",format(VcCr),
                    "\" vcol_c_g=\"",format(VcCg),
                    "\" vcol_c_b=\"",format(VcCb),"\""];
               _ ->
                   io:format(?__(4,"WARNING! Face has ~w =/= 3 vertex colors")++"~n",
                             [length(VCols)]),
                   ""
           end,
    println(F, [Shader, "        <f a=\"",format(A),
                "\" b=\"",format(B),"\" c=\"",format(C),"\"", UVIndices,
                VCol]),


    export_faces(F, T, DefaultMaterial, TxT, VColT).

