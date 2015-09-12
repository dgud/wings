%
%
%
%

export_camera(F, Name, Attr) ->
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} =
        proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Lens_Type = proplists:get_value(lens_type, Attr),
    Lens_Ortho_Scale = proplists:get_value(lens_ortho_scale, Attr),
    Lens_Angular_Circular = proplists:get_value(lens_angular_circular, Attr),
    Lens_Angular_Mirrored = proplists:get_value(lens_angular_mirrored, Attr),
    Lens_Angular_Max_Angle = proplists:get_value(lens_angular_max_angle, Attr),
    Lens_Angular_Angle = proplists:get_value(lens_angular_angle, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% YafaRay focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5*Ro)),
    Aperture = proplists:get_value(aperture, Attr),
    println(F, "<camera name=\"~s\"> "++
                "<resx ival=\"~w\"/> <resy ival=\"~w\"/> <focal fval=\"~.10f\"/>"++
                if Aperture > 0.0 ->
                        "~n        <dof_distance fval=\"~.10f\"/> <aperture fval=\"~.10f\"/>"
                            "~n        <use_qmc bval=\"~s\"/> <bokeh_type sval=\"~s\"/>"
                            "~n        <bokeh_bias sval=\"~s\"/> <bokeh_rotation fval=\"~.10f\"/> <dof_distance fval=\"~.10f\"/>";
                   true -> ""
                end++


                case Lens_Type of
                    perspective   ->
                        "~n <type sval=\"~s\"/>";

                    orthographic  ->
                        "~n <type sval=\"~s\"/>"
                            "<scale fval=\"~.10f\"/>";

                    architect     ->
                        "~n <type sval=\"~s\"/>";

                    angular           ->
                        "~n <type sval=\"~s\"/>"
                            "<circular bval=\"~s\"/>"
                            "<mirrored bval=\"~s\"/>"
                            "~n <max_angle fval=\"~.10f\"/>"
                            "<angle fval=\"~.10f\"/>"

                end,


            [Name,Width,Height,FocalDist]++
                if Aperture > 0.0 ->
                        [e3d_vec:len(Dir),
                         Aperture,
                         format(proplists:get_value(bokeh_use_QMC, Attr)),
                         format(proplists:get_value(bokeh_type, Attr)),
                         format(proplists:get_value(bokeh_bias, Attr)),
                         proplists:get_value(bokeh_rotation, Attr),
                         proplists:get_value(dof_distance, Attr)];
                   true -> []
                end++


                case Lens_Type of
                    perspective -> [format(Lens_Type)];
                    orthographic  -> [format(Lens_Type),Lens_Ortho_Scale];

                    architect    -> [format(Lens_Type)];
                    angular    -> [format(Lens_Type),
                                   format(Lens_Angular_Circular),
                                   format(Lens_Angular_Mirrored),
                                   Lens_Angular_Max_Angle,
                                   Lens_Angular_Angle]

                end

           ),
    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.