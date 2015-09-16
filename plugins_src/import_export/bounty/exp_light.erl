%
%
% lights..
%
%
export_light(F, Name, Ps) ->
    case proplists:get_value(visible, Ps, true) of
        true ->
            OpenGL = proplists:get_value(opengl, Ps, []),
            YafaRay = proplists:get_value(?TAG, Ps, []),
            Type = proplists:get_value(type, OpenGL, []),
            export_light(F, Name, Type, OpenGL, YafaRay);
        _ ->
            undefined
    end.

%% Export Point Light

export_light(F, Name, point, OpenGL, Attr) ->
    Power = proplists:get_value(power, Attr, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    LightType = proplists:get_value(type, Attr, ?DEF_POINT_TYPE),

    println(F, "<light name=\"~s\">",[Name]),
    println(F, "\t<power fval=\"~.3f\"/>",[Power]),
    case LightType of
        spherelight ->
            println(F,
                "\t<radius fval=\"~.10f\"/>",
                    [proplists:get_value(arealight_radius, Attr, ?DEF_AREALIGHT_RADIUS)]),
            println(F,
                "\t<samples ival=\"~w\"/>",
                    [proplists:get_value(arealight_samples, Attr, ?DEF_AREALIGHT_SAMPLES)]),
            println(F,
                "\t<type sval=\"spherelight\"/>");
        _ ->
            println(F,
                "\t<type sval=\"pointlight\"/>")
    end,
    export_pos(F, from, Position),
    export_rgb(F, color, Diffuse),

    println(F, "</light>"),
    undefined;

%%% Export Infinite Light Sun and Directional
export_light(F, Name, infinite, OpenGL, YafaRay) ->
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_INFINITE),
    Type = proplists:get_value(type, YafaRay, ?DEF_INFINITE_TYPE),
    InfiniteTrue = proplists:get_value(infinite_true, YafaRay, ?DEF_INFINITE_TRUE),
    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    SunSamples = proplists:get_value(sun_samples, YafaRay, ?DEF_SUN_SAMPLES),
    SunAngle = proplists:get_value(sun_angle, YafaRay, ?DEF_SUN_ANGLE),

    %% Directional Infinite Light Start
    case Type of
        directional when Power > 0.0 ->
            println(F, "<light name=\"~s\">",[Name]),
            println(F, "\t<type sval=\"~w\"/>",[Type]),
            println(F, "\t<power fval=\"~.3f\"/>",[Power]),

            %% Add Semi-infinite Start

            case proplists:get_value(infinite_true, YafaRay,
                                     ?DEF_INFINITE_TRUE) of
                false ->
                    InfiniteRadius = proplists:get_value(infinite_radius, YafaRay, ?DEF_INFINITE_RADIUS),
                    println(F, "<infinite bval=\"~s\"/>",[format(InfiniteTrue)]),
                    println(F, "<radius fval=\"~.10f\"/>",[InfiniteRadius]),
                    export_pos(F, from, Position);
                true -> ok
            end,
            %% Add Semi-infinite End

            export_pos(F, direction, Position),
            export_rgb(F, color, Diffuse),
            println(F, "</light>"),
            Bg;


        directional -> Bg;

        %% Directional Infinite Light End
        %% Sunlight Infinite Light Start
        sunlight when Power > 0.0 ->
            println(F, "<light name=\"~s\">",[Name]),
            println(F, "\t<type sval=\"~w\"/>",[Type]),
            println(F, "\t<power fval=\"~.10f\"/>",[Power]),
            println(F, "\t<samples ival=\"~w\"/>",[SunSamples]),
            println(F, "\t<angle fval=\"~.3f\"/>",[SunAngle]),

            export_pos(F, direction, Position),
            export_rgb(F, color, Diffuse),
            println(F, "</light>"),
            Bg;

        sunlight -> Bg

                    %% Sunlight Infinite Light End
    end;


%%% Export Spot Light
export_light(F, Name, spot, OpenGL, YafaRay) ->
    Power = proplists:get_value(power, YafaRay, ?DEF_ATTN_POWER),
    Position = proplists:get_value(position, OpenGL, {0.0,0.0,0.0}),
    AimPoint = proplists:get_value(aim_point, OpenGL, {0.0,0.0,1.0}),
    ConeAngle = proplists:get_value(cone_angle, OpenGL, ?DEF_CONE_ANGLE),
    Diffuse = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    Type = proplists:get_value(type, YafaRay, ?DEF_SPOT_TYPE),

    println(F, "<light name=\"~s\">",[Name]),
    println(F, "\t<power fval=\"~.3f\"/>",[Power]),
    case Type of
        spotlight ->

            SpotPhotonOnly =    proplists:get_value(spot_photon_only, YafaRay, ?DEF_SPOT_PHOTON_ONLY),
            SpotSoftShadows =   proplists:get_value(spot_soft_shadows, YafaRay, ?DEF_SPOT_SOFT_SHADOWS),
            SpotIESSamples =    proplists:get_value(spot_ies_samples, YafaRay,  ?DEF_SPOT_IES_SAMPLES),
            CastShadows =       proplists:get_value(cast_shadows, YafaRay, ?DEF_CAST_SHADOWS),
            SpotExponent =      proplists:get_value(spot_exponent, OpenGL, ?DEF_SPOT_EXPONENT),

            SpotBlend = proplists:get_value(spot_blend, YafaRay, ?DEF_SPOT_BLEND),

            SpotFuzzyness = proplists:get_value(spot_fuzzyness, YafaRay, ?DEF_SPOT_FUZZYNESS),

            println(F, "\t<type sval=\"spotlight\"/>"),
            println(F, "\t<cast_shadows bval=\"~s\"/>",[format(CastShadows)]),
            println(F, "\t<photon_only bval=\"~s\"/>",[SpotPhotonOnly]),
            println(F, "\t<cone_angle fval=\"~.3f\"/>",[ConeAngle]),
            println(F, "\t<beam_falloff fval=\"~.10f\"/>",[SpotExponent]),
            println(F, "\t<blend fval=\"~.3f\"/>",[SpotBlend]),
            println(F, "\t<soft_shadows bval=\"~s\"/>",[SpotSoftShadows]),
            println(F, "\t<shadowFuzzyness fval=\"~.3f\"/>",[SpotFuzzyness]),
            println(F, "\t<samples ival=\"~w\"/>",[SpotIESSamples]);

        spot_ies ->

            SpotSoftShadows = proplists:get_value(spot_soft_shadows, YafaRay, ?DEF_SPOT_SOFT_SHADOWS),

            SpotIESFilename = proplists:get_value(spot_ies_filename, YafaRay, ?DEF_SPOT_IES_FILENAME),

            SpotIESSamples = proplists:get_value(spot_ies_samples, YafaRay,  ?DEF_SPOT_IES_SAMPLES),

        println(F, "\t<type sval=\"ieslight\"/>"),
        println(F, "\t<cone_angle fval=\"~.3f\"/>",[ConeAngle]),
        println(F, "\t<soft_shadows bval=\"~s\"/>",[SpotSoftShadows]),
        println(F, "\t<samples ival=\"~w\"/>",[SpotIESSamples]),
        println(F, "\t<file sval=\"~s\"/>",[SpotIESFilename])
    end,
    export_pos(F, from, Position),
    export_pos(F, to, AimPoint),
    export_rgb(F, color, Diffuse),
    println(F, "</light>"),
    undefined;

%% Export Ambient Light

export_light(F, Name, ambient, _OpenGL, YafaRay) ->
    Type = proplists:get_value(type, YafaRay, ?DEF_AMBIENT_TYPE),
    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_AMBIENT),
    case Type of
        hemilight when Power > 0.0 ->
            println(F,"",
                    []),
            println(F, "",
                    []),
            case proplists:get_value(use_maxdistance, YafaRay,
                                     ?DEF_USE_MAXDISTANCE) of
                true ->
                    Maxdistance = proplists:get_value(maxdistance, YafaRay, ?DEF_MAXDISTANCE),
                    println(F, "\t<maxdistance fval=\"~.10f\"/>",[Maxdistance]);
                
                false -> ok
            end,

            println(F, ""),
            Bg;
        hemilight -> Bg;
        %
        pathlight when Power > 0.0 ->
            println(F,"<light type sval=\"~w\" name sval=\"~s\" power fval=\"~.3f\"",
                    [Type,Name,Power]),
            UseQMC = proplists:get_value(use_QMC, YafaRay,?DEF_USE_QMC),
            Depth = proplists:get_value(depth, YafaRay, ?DEF_DEPTH),
            CausDepth = proplists:get_value(caus_depth, YafaRay, ?DEF_CAUS_DEPTH),
            Direct = proplists:get_value(direct, YafaRay, ?DEF_DIRECT),
            Samples = proplists:get_value(samples, YafaRay, ?DEF_SAMPLES),
            print(F, "       use_QMC=\"~s\" samples=\"~w\" "
                  "depth=\"~w\" caus_depth=\"~w\"",
                  [format(UseQMC),Samples,Depth,CausDepth]),
            case Direct of
                true ->
                    print(F, " direct=\"on\"");
                false ->
                    case proplists:get_value(cache, YafaRay, ?DEF_CACHE) of
                        true ->
                            CacheSize =
                                proplists:get_value(cache_size, YafaRay,?DEF_CACHE_SIZE),
                            AngleThreshold =
                                proplists:get_value(angle_threshold, YafaRay,?DEF_ANGLE_THRESHOLD),
                            ShadowThreshold =
                                proplists:get_value(shadow_threshold, YafaRay, ?DEF_SHADOW_THRESHOLD),
                            Gradient =
                                proplists:get_value(gradient, YafaRay, ?DEF_GRADIENT),
                            ShowSamples =
                                proplists:get_value(show_samples, YafaRay, ?DEF_SHOW_SAMPLES),
                            Search =
                                proplists:get_value(search, YafaRay, ?DEF_SEARCH),
                            print(F, " cache=\"on\"~n"
                                  "       cache_size=\"~.10f\" "
                                  "angle_threshold=\"~.10f\"~n"
                                  "       shadow_threshold=\"~.10f\" "
                                  "gradient=\"~s\"~n"
                                  "       show_samples=\"~s\" search=\"~w\"",
                                  [CacheSize,AngleThreshold, ShadowThreshold,
                                  format(Gradient), format(ShowSamples),Search]);
                        false -> ok
                    end
            end,
            println(F, ">"),
            PathlightMode = 
                proplists:get_value(pathlight_mode, YafaRay, ?DEF_PATHLIGHT_MODE),
            case PathlightMode of
                undefined ->
                    ok;
                _ ->
                    println(F, "<mode sval=\"~s\"/>",
                            [format(PathlightMode)])
            end,
            case proplists:get_value(use_maxdistance, YafaRay,
                                     ?DEF_USE_MAXDISTANCE) of
                true ->
                    Maxdistance = proplists:get_value(maxdistance, YafaRay, ?DEF_MAXDISTANCE),
                    println(F, "<maxdistance fval=\"~.10f\"/>", [Maxdistance]);
                false -> ok
            end,
            println(F, "</light>"),
            Bg;
        pathlight -> Bg;
        globalphotonlight ->
            println(F,"<light type sval=\"~w\" name sval=\"~s\"", [Type,Name]),
            GplPhotons = 
                proplists:get_value(globalphotonlight_photons, YafaRay, ?DEF_GLOBALPHOTONLIGHT_PHOTONS),
            GplRadius = 
                proplists:get_value(globalphotonlight_radius, YafaRay,?DEF_GLOBALPHOTONLIGHT_RADIUS),
            GplDepth = 
                proplists:get_value(globalphotonlight_depth, YafaRay,?DEF_GLOBALPHOTONLIGHT_DEPTH),
            GplSearch = 
                proplists:get_value(globalphotonlight_search, YafaRay,?DEF_GLOBALPHOTONLIGHT_SEARCH),
            println(F,"       photons ival=\"~w\" radius=\"~.3f\" "
                    "depth=\"~w\" search=\"~w\">",
                    [GplPhotons,GplRadius,GplDepth,GplSearch]),
            println(F, "</light>"),
            Bg
    end;

%% Export Area Light

export_light(F, Name, area, OpenGL, YafaRay) ->
    Color = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0,1.0}),
    #e3d_mesh{vs=Vs,fs=Fs0} = proplists:get_value(mesh, OpenGL, #e3d_mesh{}),
    VsT = list_to_tuple(Vs),
    
    Power = proplists:get_value(power, YafaRay, ?DEF_ATTN_POWER),
    Samples = proplists:get_value(arealight_samples, YafaRay, ?DEF_AREALIGHT_SAMPLES),
    Dummy = proplists:get_value(dummy, YafaRay, ?DEF_DUMMY),
    
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
                      println(F, "<light name=\"~s\"> <type sval=\"arealight\"/>"
                              "<power fval=\"~.3f\"/>~n"
                              "<samples ival=\"~w\"/>"++
                                  if Dummy -> "";
                                     true ->
                                          ""
                                  end++"",
                              [NameI,Pwr,Samples]++
                                  if Dummy -> [];
                                     true -> []
                                  end),
                      export_rgb(F, color, Color),
                      export_pos(F, corner, A),
                      export_pos(F, from, B),
                      export_pos(F, point1, C),
                      export_pos(F, point2, D),
                      println(F, "</light>"),
                      I+1
              end
      end, 1, AFs),
    undefined;
export_light(_F, Name, Type, _OpenGL, _YafaRay) ->
    io:format(?__(1,"WARNING: Ignoring unknown light \"~s\" type: ~p")++"~n",
              [Name, format(Type)]),
    undefined.

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
