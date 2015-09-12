
export_background(F, Name, Ps) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    YafaRay = proplists:get_value(?TAG, Ps, []),
    Bg = proplists:get_value(background, YafaRay, ?DEF_BACKGROUND_AMBIENT),
    case Bg of
%% Constant Background Export
        constant ->

            Samples = proplists:get_value(samples, YafaRay, ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),

            print(F, "<background name=\"~s\">", [Name]),

            println(F, "<type sval=\"~s\"/>",[format(Bg)]),
            BgColor = proplists:get_value(background_color, YafaRay, ?DEF_BACKGROUND_COLOR),
            export_rgb(F, color, BgColor),
            ConstantBackPower = proplists:get_value(power, YafaRay,  ?DEF_POWER),


%% Add Enlight Constant Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Constant Background End

            println(F, "<power fval=\"~w\"/>~n",
                [ConstantBackPower]);


%% Gradient Background Export
        gradientback ->

            Samples = proplists:get_value(samples, YafaRay, ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),


            print(F, "<background name=\"~s\">", [Name]),

            println(F, "<type sval=\"~s\"/>", [format(Bg)]),
			
            HorizonColor = proplists:get_value(horizon_color, YafaRay,
                ?DEF_HORIZON_COLOR),
            export_rgb(F, horizon_color, HorizonColor),
            ZenithColor = proplists:get_value(zenith_color, YafaRay, ?DEF_ZENITH_COLOR),
            export_rgb(F, zenith_color, ZenithColor),
            GradientBackPower = proplists:get_value(power, YafaRay, ?DEF_POWER),

%% Add Enlight Gradient Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Gradient Background End


            println(F, "<power fval=\"~w\"/>~n",
                [GradientBackPower]);

%% Sunsky Background Export
        sunsky ->
%%% 	    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
            Turbidity = proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY),
            A_var = proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR),
            B_var = proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR),
            C_var = proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR),
            D_var = proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR),
            E_var = proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR),

            SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
            SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES),

            SunReal = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            SunRealPower = proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER),


            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            print(F, "<background name=\"~s\"> <type sval=\"~s\"/>",
                [Name,format(Bg)]),

            println(F, "~n            <turbidity fval=\"~.3f\"/> <a_var fval=\"~.3f\"/>~n"
            "            <b_var fval=\"~.3f\"/> <c_var fval=\"~.3f\"/>~n"
            "            <d_var fval=\"~.3f\"/> <e_var fval=\"~.3f\"/> "
                ,
                [Turbidity,A_var,B_var,C_var,D_var,E_var]),


%% Add Sun Real Start
            case proplists:get_value(sun_real, YafaRay,
                ?DEF_SUN_REAL) of
                true ->
                    SunRealPower = proplists:get_value(sun_real_power, YafaRay,
                        ?DEF_SUN_REAL_POWER),
                    println(F, " <add_sun bval=\"~s\"/><sun_power fval=\"~.3f\"/>",
                        [format(SunReal),SunRealPower]);

                false -> ok
            end,


%% Add Sun Real End

%% Add Skylight Start

            case proplists:get_value(sky_background_light, YafaRay,
                ?DEF_SKY_BACKGROUND_LIGHT) of
                true ->
                    SkyBackgroundPower = proplists:get_value(sky_background_power, YafaRay,
                        ?DEF_SKY_BACKGROUND_POWER),
                    SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay,
                        ?DEF_SKY_BACKGROUND_SAMPLES),
                    println(F, " <background_light bval=\"~s\"/><power fval=\"~.3f\"/><light_samples ival=\"~w\"/>",
                        [format(SkyBackgroundLight),SkyBackgroundPower,SkyBackgroundSamples]);

                false -> ok
            end,
%% Add Skylight End

            export_pos(F, from, Position);

%% Darksky Background Export
        darksky ->
%%% 	    Power = proplists:get_value(power, YafaRay, ?DEF_POWER),
            Turbidity = proplists:get_value(turbidity, YafaRay, ?DEF_TURBIDITY),
            A_var = proplists:get_value(a_var, YafaRay, ?DEF_SUNSKY_VAR),
            B_var = proplists:get_value(b_var, YafaRay, ?DEF_SUNSKY_VAR),
            C_var = proplists:get_value(c_var, YafaRay, ?DEF_SUNSKY_VAR),
            D_var = proplists:get_value(d_var, YafaRay, ?DEF_SUNSKY_VAR),
            E_var = proplists:get_value(e_var, YafaRay, ?DEF_SUNSKY_VAR),

            DarkskyAltitude = proplists:get_value(darksky_altitude, YafaRay, ?DEF_DARKSKY_ALTITUDE),
            SkyBackgroundLight = proplists:get_value(sky_background_light, YafaRay, ?DEF_SKY_BACKGROUND_LIGHT),
            SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay, ?DEF_SKY_BACKGROUND_SAMPLES),

            SunReal = proplists:get_value(sun_real, YafaRay, ?DEF_SUN_REAL),
            SunRealPower = proplists:get_value(sun_real_power, YafaRay, ?DEF_SUN_REAL_POWER),

            DarkskyNight = proplists:get_value(darksky_night, YafaRay, ?DEF_DARKSKY_NIGHT),
            DarkskyDiffusePhotons = proplists:get_value(darksky_diffusephotons, YafaRay, ?DEF_DARKSKY_DIFFUSEPHOTONS),
            DarkskyCausticPhotons = proplists:get_value(darksky_causticphotons, YafaRay, ?DEF_DARKSKY_CAUSTICPHOTONS),

            Position = proplists:get_value(position, OpenGL, {1.0,1.0,1.0}),

            print(F, "<background name=\"~s\"> <type sval=\"~s\"/>",
                [Name,format(Bg)]),

            println(F, "~n            <turbidity fval=\"~.3f\"/> <a_var fval=\"~.3f\"/>~n"
            "            <b_var fval=\"~.3f\"/> <c_var fval=\"~.3f\"/>~n"
            "            <d_var fval=\"~.3f\"/> <e_var fval=\"~.3f\"/>~n"
            "            <altitude fval=\"~.3f\"/>"
                ,
                [Turbidity,A_var,B_var,C_var,D_var,E_var,DarkskyAltitude]),

%% Add Sun Real Start
            case proplists:get_value(sun_real, YafaRay,
                ?DEF_SUN_REAL) of
                true ->
                    SunRealPower = proplists:get_value(sun_real_power, YafaRay,
                        ?DEF_SUN_REAL_POWER),
                    println(F, " <add_sun bval=\"~s\"/><sun_power fval=\"~.3f\"/>",
                        [format(SunReal),SunRealPower]);

                false -> ok
            end,

%% Add Skylight Start

            case proplists:get_value(sky_background_light, YafaRay,
                ?DEF_SKY_BACKGROUND_LIGHT) of
                true ->
                    SkyBackgroundPower = proplists:get_value(sky_background_power, YafaRay,
                        ?DEF_SKY_BACKGROUND_POWER),
                    SkyBackgroundSamples = proplists:get_value(sky_background_samples, YafaRay,
                        ?DEF_SKY_BACKGROUND_SAMPLES),
                    println(F, " <background_light bval=\"~s\"/><power fval=\"~.3f\"/><light_samples ival=\"~w\"/>",
                        [format(SkyBackgroundLight),SkyBackgroundPower,SkyBackgroundSamples]);

                false -> ok
            end,

%% Add Darksky Photons

            case proplists:get_value(darksky_diffusephotons, YafaRay,
                ?DEF_DARKSKY_DIFFUSEPHOTONS) of
                true ->

                    println(F, " <with_diffuse bval=\"~s\"/>", [format(DarkskyDiffusePhotons)]);

                false -> ok
            end,

            case proplists:get_value(darksky_causticphotons, YafaRay,
                ?DEF_DARKSKY_CAUSTICPHOTONS) of
                true ->

                    println(F, " <with_caustic bval=\"~s\"/>",
                        [format(DarkskyCausticPhotons)]);

                false -> ok
            end,

%% Add Darksky Night

            case proplists:get_value(darksky_night, YafaRay,
                ?DEF_DARKSKY_NIGHT) of
                true ->

                    println(F, " <night bval=\"~s\"/>",
                        [format(DarkskyNight)]);

                false -> ok
            end,
%% Add Darksky Night End


            export_pos(F, from, Position);


%% HDRI Background Export
        'HDRI' ->
            BgFname = proplists:get_value(background_filename_HDRI, YafaRay,
                ?DEF_BACKGROUND_FILENAME),
            BgExpAdj = proplists:get_value(power, YafaRay,
                ?DEF_POWER),
            BgMapping = proplists:get_value(background_mapping, YafaRay,
                ?DEF_BACKGROUND_MAPPING),

            BgRotation = proplists:get_value(background_rotation, YafaRay,
                ?DEF_BACKGROUND_ROTATION),

            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),


            print(F, "<texture name=\"world_texture\">~n"
            "        <filename sval=\"~s\"/>~n"
            "        <interpolate sval=\"bilinear\"/>~n"
            "        <type sval=\"image\"/>~n"
            "        </texture>",
                [BgFname]),
            println(F, "~n <background name=\"~s\"> <type sval=\"textureback\"/> ",
                [Name]),
            println(F, "<power fval=\"~w\"/>~n"
            "<mapping sval=\"~s\"/>~n"
            "<rotation fval=\"~.3f\"/>"
                ,
                [BgExpAdj,format(BgMapping),BgRotation]),

%% Add Enlight HDRI Background Start

            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,
%% Add Enlight HDRI Background End

            print(F, "<texture sval=\"world_texture\"/>");

%% Image Background Export
        image ->
            BgFname = proplists:get_value(background_filename_image, YafaRay,
                ?DEF_BACKGROUND_FILENAME),
            BgPower = proplists:get_value(power, YafaRay,
                ?DEF_POWER),
            Samples = proplists:get_value(samples, YafaRay,
                ?DEF_SAMPLES),
            BgRotation = proplists:get_value(background_rotation, YafaRay,
                ?DEF_BACKGROUND_ROTATION),

            AmbientDiffusePhotons = proplists:get_value(ambient_diffusephotons, YafaRay, ?DEF_AMBIENT_DIFFUSEPHOTONS),

            AmbientCausticPhotons = proplists:get_value(ambient_causticphotons, YafaRay, ?DEF_AMBIENT_CAUSTICPHOTONS),



            print(F, "<texture name=\"world_texture\">~n"
            "        <filename sval=\"~s\"/>~n"
            "        <interpolate sval=\"bilinear\"/>~n"
            "        <type sval=\"image\"/>~n"
            "        </texture>",
                [BgFname]),
            println(F, "~n <background name=\"~s\"> <type sval=\"textureback\"/> ",
                [Name]),

            println(F, " <power fval=\"~.3f\"/>~n"
            "<rotation fval=\"~.3f\"/>"
                , [BgPower,BgRotation]),

%% Add Enlight Image Background Start
            case proplists:get_value(background_enlight, YafaRay,
                ?DEF_BACKGROUND_ENLIGHT) of
                true ->
                    println(F, "<ibl bval=\"true\"/>"),
                    println(F, "<ibl_samples ival=\"~w\"/>",[Samples]),
                    println(F, "<with_diffuse bval=\"~s\"/>",[AmbientDiffusePhotons]),
                    println(F, "<with_caustic bval=\"~s\"/>",[AmbientCausticPhotons]);
                false ->
                    println(F, "<ibl bval=\"false\"/>")
            end,

%% Add Enlight Image Background End


            println(F, "    <texture sval=\"world_texture\" />")
    end,
    println(F, "</background>").


