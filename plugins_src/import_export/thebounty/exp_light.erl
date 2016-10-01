%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2015 - 2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the include GNU General Public License file for more details.
%%

%
% lights..
%
export_light(F, Name, Ps) ->
    case proplists:get_value(visible, Ps, true) of
        true ->
            OpenGL = proplists:get_value(opengl, Ps, []),
            Attr = proplists:get_value(?TAG, Ps, []),
            LightType = proplists:get_value(type, OpenGL, []),
            export_light(F, Name, LightType, OpenGL, Attr);
        _ ->
            undefined
    end.

%!--------------------
%! Export Point Light
%!--------------------

export_light(F, Name, point, OpenGL, Attr) ->
    Power = proplists:get_value(power, Attr, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,2.0,0.0}),
    LightColor = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    PointType = proplists:get_value(type, Attr, ?DEF_POINT_TYPE),

    println(F, "<light name=\"~s\">",[Name]),
    println(F, "\t<power fval=\"~.3f\"/>",[Power]),
    println(F, "\t<type sval=\"~s\"/>",[PointType]),

    case PointType of
        spherelight ->
            println(F,
                "\t<radius fval=\"~.10f\"/>",[proplists:get_value(arealight_radius, Attr, 1.0)]),
            println(F,
                "\t<samples ival=\"~w\"/>",[proplists:get_value(arealight_samples, Attr, 16)]);
        _ -> ok
    end,
    export_pos(F, from, Position),
    export_rgb(F, color, LightColor),

    println(F, "</light>");
    %undefined;

%!-------------------------------------------
%! Export Infinite Light Sun and Directional
%!-------------------------------------------
export_light(F, Name, infinite, OpenGL, Attr) ->
    Type = proplists:get_value(type, Attr, ?DEF_INFINITE_TYPE),
    IsInfinite = proplists:get_value(infinite_true, Attr, ?DEF_INFINITE_TRUE),
    Power = proplists:get_value(power, Attr, ?DEF_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {0.9,0.9,0.9,0.9}),

    println(F, "<light name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~w\"/>",[Type]),
    println(F, "\t<power fval=\"~.10f\"/>",[Power]),
    case Type of
        directional ->
            println(F, "\t<infinite bval=\"~s\"/>",[IsInfinite]),
            case IsInfinite of
                false ->
                    println(F,
                        "\t<radius fval=\"~.10f\"/>",[proplists:get_value(infinite_radius, Attr, 10.0)]);
                true -> ok
            end,
            export_pos(F, direction, Position),
            export_rgb(F, color, Diffuse);

        % sun light case
        sunlight ->
            println(F, "\t<samples ival=\"~w\"/>",[proplists:get_value(samples, Attr, 8)]),
            println(F, "\t<angle fval=\"~.3f\"/>",[proplists:get_value(sun_angle, Attr, ?DEF_SUN_ANGLE)])
    end,
    export_pos(F, direction, Position),
    export_rgb(F, color, Diffuse),
    println(F, "</light>");


%!----------------------
%! Export Spot Light
%!----------------------
export_light(F, Name, spot, OpenGL, Attr) ->
    Power = proplists:get_value(power, Attr, 1.0),
    Position = proplists:get_value(position, OpenGL, {0.0,1.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    SpotType = proplists:get_value(spot_type, Attr, spotlight),
    SpotSoftShadows = proplists:get_value(spot_soft_shadows, Attr, ?DEF_SPOT_SOFT_SHADOWS),
    SpotIESSamples = proplists:get_value(spot_ies_samples, Attr,  16),

    println(F, "<light name=\"~s\">",[Name]),
    println(F, "\t<power fval=\"~.3f\"/>",[Power]),
    case SpotType of
        spotlight ->
            println(F, "\t<type sval=\"spotlight\"/>"),
            println(F,
                "\t<photon_only bval=\"~s\"/>",[proplists:get_value(spot_photon_only, Attr, false)]),
            println(F,
                "\t<cone_angle fval=\"~.3f\"/>",[ConeAngle]),
            println(F,
                "\t<blend fval=\"~.3f\"/>",[proplists:get_value(spot_blend, Attr, 0.5)]),
            println(F, "\t<soft_shadows bval=\"~s\"/>",[SpotSoftShadows]),
            println(F,
                "\t<shadowFuzzyness fval=\"~.3f\"/>",[proplists:get_value(spot_fuzzyness, Attr, 0.5)]),
            println(F, "\t<samples ival=\"~w\"/>",[SpotIESSamples]);

        spot_ies ->
            println(F, "\t<type sval=\"ieslight\"/>"),
            println(F, "\t<cone_angle fval=\"~.3f\"/>",[ConeAngle]),
            println(F, "\t<soft_shadows bval=\"~s\"/>",[SpotSoftShadows]),
            println(F, "\t<samples ival=\"~w\"/>",[SpotIESSamples]),
            println(F, "\t<file sval=\"~s\"/>",[proplists:get_value(spot_ies_filename, Attr, "")])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>");

%% Export Ambient Light -- moved
%!------------------------
%! Export Area Light
%!------------------------

export_light(F, Name, area, OpenGL, Attr) ->
    %
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),

    Power = proplists:get_value(power, Attr, ?DEF_ATTN_POWER),
    Samples = proplists:get_value(arealight_samples, Attr, 16),
    %Dummy = proplists:get_value(dummy, Attr, ?DEF_DUMMY),

    Fs = foldr(fun (Face, Acc) ->
                    e3d_mesh:quadrangulate_face(Face, Vs)++Acc
               end, [], Fs0),
    %
    As = e3d_mesh:face_areas(Fs, Vs),
    Area = foldl(fun (A, Acc) -> A+Acc end, 0.0, As),
    AFs = zip_lists(As, Fs),
    foldl(
      fun ({Af,#e3d_face{vs=VsF}}, I) ->
              case catch Power*Af/Area of
                  {'EXIT',{badarith,_}} -> I;
                  Pwr ->
                      NameI = Name++"_"++integer_to_list(I),
                      [A,B,C,D] = quadrangle_vertices(VsF, VsT),
                      println(F, "<light name=\"~s\">",[NameI]),
                      println(F, "\t<type sval=\"arealight\"/>"),
                      println(F, "\t<power fval=\"~.3f\"/>",[Pwr]),
                      println(F, "\t<samples ival=\"~w\"/>",[Samples]),
                      export_rgb(F, color, Color),
                      export_pos(F, corner, A),
                      export_pos(F, from, B),
                      export_pos(F, point1, C),
                      export_pos(F, point2, D),
                      println(F, "</light>"),
                      I+1
              end
      end, 1, AFs);
    %undefined;

 % no supported light
export_light(_F, Name, Type, _OpenGL, Attr) ->
    io:format(?__(1,"WARNING: Ignoring unknown light \"~s\" type: ~p")++"~n",
              [Name, format(Type)]).%,
    %undefined.

%% Cut the longest edge of a triangle in half to make it a quad.
%% Lookup vertex positions.
%%
quadrangle_vertices([V1,V2,V3], VsT) ->
    P1 = element(V1+1, VsT),
    P2 = element(V2+1, VsT),
    P3 = element(V3+1, VsT),
    [L12,L23,L31] =
        [e3d_vec:dot(L, L) ||
            L <- [e3d_vec:sub(P1, P2),e3d_vec:sub(P2, P3),
                  e3d_vec:sub(P3, P1)]],
    if L23 > L31 ->
            if L12 > L23 -> [P1,e3d_vec:average([P1,P2]),P2,P3];
               true -> [P1,P2,e3d_vec:average([P2,P3]),P3]
            end;
       true -> [P1,P2,P3,e3d_vec:average([P3,P1])]
    end;
quadrangle_vertices([V1,V2,V3,V4], VsT) ->
    [element(V1+1, VsT),element(V2+1, VsT),
     element(V3+1, VsT),element(V4+1, VsT)].
