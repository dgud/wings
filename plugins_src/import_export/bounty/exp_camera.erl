%
%
%
%

export_camera(F, Name, Attr) ->
    #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov} = proplists:lookup(camera_info, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Lens_Type = proplists:get_value(lens_type, Attr),
    Ro = math:pi()/180.0,
    %% Fov is vertical angle from lower to upper border.
    %% TheBounty focal plane is 1 unit wide.
    FocalDist = 0.5 / ((Width/Height) * math:tan(limit_fov(Fov)*0.5*Ro)),
    Aperture = proplists:get_value(aperture, Attr),
    
    println(F, "<camera name=\"~s\">",[Name]),
    println(F, "\t<type sval=\"~s\"/>",[Lens_Type]),
    println(F, "\t<resx ival=\"~w\"/>",[Width]),
    println(F, "\t<resy ival=\"~w\"/>",[Height]),
    println(F, "\t<focal fval=\"~.10f\"/>",[FocalDist]),
    case Aperture of
        0.0 ->
            ok;
        _ ->
            println(F, "\t<aperture fval=\"~.10f\"/>",[Aperture]),
            %println(F, "\t<use_qmc bval=\"~s\"/>",[format(proplists:get_value(bokeh_use_QMC, Attr))]),
            println(F, "\t<bokeh_type sval=\"~s\"/>",[format(proplists:get_value(bokeh_type, Attr))]),
            println(F, "\t<bokeh_bias sval=\"~s\"/>",[format(proplists:get_value(bokeh_bias, Attr))]),
            println(F, "\t<bokeh_rotation fval=\"~.10f\"/>",[proplists:get_value(bokeh_rotation, Attr)]),
            println(F, "\t<dof_distance fval=\"~.10f\"/>",[proplists:get_value(dof_distance, Attr)])
    end,
    Lens_Type = proplists:get_value(lens_type, Attr),
    case Lens_Type of
        %
        orthographic ->
            println(F,
                "\t<scale fval=\"~.10f\"/>",[proplists:get_value(lens_ortho_scale, Attr)]);
        angular ->
            println(F,
                "\t<circular bval=\"~s\"/>",[proplists:get_value(lens_angular_circular, Attr)]),
            println(F,
                "\t<mirrored bval=\"~s\"/>",[proplists:get_value(lens_angular_mirrored, Attr)]),
            println(F,
                "\t<max_angle fval=\"~.10f\"/>",[proplists:get_value(lens_angular_max_angle, Attr)]),
            println(F,
                "\t<angle fval=\"~.10f\"/>",[proplists:get_value(lens_angular_angle, Attr)]);
        _ -> ok
    end,
    
    export_pos(F, from, Pos),
    export_pos(F, to, e3d_vec:add(Pos, Dir)),
    export_pos(F, up, e3d_vec:add(Pos, Up)),
    println(F, "</camera>").

limit_fov(Fov) when Fov < 1.0 -> 1.0;
limit_fov(Fov) when Fov > 179.0 -> 179.0;
limit_fov(Fov) -> Fov.