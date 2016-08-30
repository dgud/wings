%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU GPL as published by the FSF;
%%  either version 2 of the License, or (at your option) any later version.
%%  See the GNU General Public License for more details.
%%

%
%  Export material shaders modulators
%  TO DO:
%  Split mapping code part and make an function
%  for mapping multiple modulators ( layers )
%
% org. export_modulator(F, Texname, Maps, {modulator,Ps}, _Opacity) when is_list(Ps) ->
export_modulator(F, Texname, Maps, {modulator,Ps}, Attr) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,BlendMode,TexType} ->
            %             
            ShaderType = proplists:get_value(shader_type, Ps),
            DoColor = 
                case ShaderType of 
                    diffuse ->  true;
                    mirror_color -> true;
                    glossy_reflect ->  true;
                    _ -> false
                end,
                
            println(F, "\t<~s_shader sval=\"~s\"/>",[ShaderType,Texname]),

            %!----------------------------------------------------
            %! entry to element shader list  D:\apps\wings3d205\lib\wings-2.0.5\plugins\import_export\wpc_thebounty.beam
            %!----------------------------------------------------
            println(F,"\t<list_element>"),
            
            %% shader factor amount controled with 'Factor Modulator' slider in UI
            Factor = proplists:get_value(diffuse_factor, Ps, 1.0),
            %% Try use value or color
            FactorType =
                case DoColor of
                    true -> "colfac";
                    _ ->    "valfac"
                end,
            %
            println(F,"\t\t<~s fval=\"~w\"/>",[FactorType,Factor]),
            %
            CellType = proplists:get_value(cell_type, Ps, ?DEF_MOD_CELLTYPE),
            %%--------------------------->
            IsColor = % revise
                case TexType of
                    image -> true;
                    {map,_} -> true;
                    voronoi ->
                        case CellType of
                            intensity -> true;
                            _ -> false % maybe this case.. fail
                        end;
                    _ -> false
                end,

            println(F,"\t\t<color_input bval=\"~s\"/>",[IsColor]),

            % def color for 'blending' by default
            {DefaultR, DefaultG, DefaultB} = proplists:get_value(def_color, Ps, ?DEF_MOD_DEFCOLOR),

            println(F,"\t\t<def_col r=\"~w\" g=\"~w\" b=\"~w\" a=\"1\"/>",[DefaultR, DefaultG, DefaultB]),
            %
            println(F,"\t\t<def_val fval=\"~w\"/>",[proplists:get_value(def_value, Ps, 1.0)]),

            % swich to use texture values
            %
            println(F,"\t\t<do_color bval=\"~s\"/>",[DoColor]),
            %
            println(F,"\t\t<do_scalar bval=\"~s\"/>",[not DoColor]),
            %
            println(F,"\t\t<element sval=\"shader_node\"/>"),
            %
            println(F,"\t\t<input sval=\"~s_mod\"/>",[Texname]),
            %
            ModeNumber =
                case BlendMode of
                    mix -> "0"; add -> "1"; mul -> "2"; sub -> "3"; scr -> "4";
                    divide -> "5"; dif -> "6"; dar -> "7"; lig -> "8";
                    _ -> ""
                end,
            println(F, "\t\t<mode ival=\"~s\"/>",[ModeNumber]),
            %
            println(F, "\t\t<name sval=\"~s\"/>",[Texname]),
            %
            println(F, "\t\t<negative bval=\"~s\"/>",[proplists:get_value(negative, Ps, false)]),
            %
            println(F, "\t\t<noRGB bval=\"~s\"/>",[proplists:get_value(no_rgb, Ps, false)]),

            Stencil = proplists:get_value(stencil, Ps, false),

            println(F, "\t\t<stencil bval=\"~s\"/>",[Stencil]),
            % TODO: create code for stencil modes

            %!------------------
            %! Set upper color
            %!------------------
            {UpR, UpG, UpB} =
                case ShaderType of
                    diffuse ->
                        proplists:get_value(diffuse_color, Attr, {0.0,0.0,0.0});
                    mirror_color ->
                        proplists:get_value(mirror_color, Attr, {0.0,0.0,0.0});

                    _ -> {0.0,0.0,0.0}
                end,
            Ulayer = "",
            %
            case Ulayer of
                "" ->
                    case DoColor of
                        true ->
                            println(F,
                                "\t\t<upper_color r=\"~.10f\" g=\"~.10f\" b=\"~.10f\" a=\"1\"/>\n"
                                "\t\t<upper_value fval=\"0\"/>",[UpR, UpG, UpB]);
                        false ->
                            println(F,
                                "\t\t<upper_color r=\"0\" g=\"0\" b=\"0\" a=\"1\"/>\n"
                                "\t\t<upper_value fval=\"~w\"/>",[Factor])
                    end;
                _ ->
                    println(F,"\t\t<upper_layer sval=\"~s\"/>",[Ulayer])
            end,

            println(F, "\t\t<type sval=\"layer\"/>"),

            % TODO: Atm, use IsColor value, but need review
            println(F, "\t\t<use_alpha bval=\"~s\"/>",[IsColor]),

            % close list element shader
            println(F, "\t</list_element>"),
            %%%-------------------------------->
            % open list element for mapper
            %
            println(F, "\t<list_element>\n"
                "\t\t<element sval=\"shader_node\"/>"),

            % projection mapping coordinates type
            % TODO: create UI option
            MappingType = proplists:get_value(projection, Ps, plain),

            println(F, "\t\t<mapping sval=\"~s\"/>",[MappingType]),
            println(F, "\t\t<name sval=\"~s_mod\"/>",[Texname]),
            %
            OffX = proplists:get_value(offset_x, Ps, 0.0),
            OffY = proplists:get_value(offset_y, Ps, 0.0),
            OffZ = proplists:get_value(offset_z, Ps, 0.0),
            %
            println(F,
                "\t\t<offset x=\"~w\" y=\"~w\" z=\"~w\"/>",
                [OffX, OffY, OffZ]),
            % TODO: create option in UI panel
            println(F,
                "\t\t<proj_x ival=\"1\"/>\n"
                "\t\t<proj_y ival=\"2\"/>\n"
                "\t\t<proj_z ival=\"3\"/>"),

            % for scale texture mapping
            SizeX = proplists:get_value(size_x, Ps, 1.0),
            SizeY = proplists:get_value(size_y, Ps, 1.0),
            SizeZ = proplists:get_value(size_z, Ps, 1.0),
            %
            println(F, "\t\t<scale x=\"~w\" y=\"~w\" z=\"~w\"/>",
                    [SizeX, SizeY, SizeZ]),

            % texture coordinates type
            println(F, "\t\t<texco sval=\"~s\"/>",[proplists:get_value(coordinates, Ps, global)]),
            println(F, "\t\t<texture sval=\"~s\"/>",[Texname]),
            println(F, "\t\t<type sval=\"texture_mapper\"/>"),
            % if bumpmap..
            case ShaderType of
                bump ->
                    println(F, "\t\t<bump_strength fval=\"~w\"/>",[Factor*5]);
                _ -> ok
            end,
            % close mapper element
            println(F, "\t</list_element>")

            %end

    end.