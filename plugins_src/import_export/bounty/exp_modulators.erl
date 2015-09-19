%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2015 Pedro Alcaide, aka povmaniac.
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
%  for mapping multiple modulators ( layer )
%

export_modulator(F, Texname, Maps, {modulator,Ps}, _Opacity) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,BlendMode,TexType} ->

            %AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),
            %UpperLayerName =  case AlphaIntensity of
            %        stencil -> re:replace(Texname,"_2","_1",[global]);
            %        _-> re:replace(Texname,"_1","_2",[global])
            %    end,
            %StencilInputName =
            %    case AlphaIntensity of
            %        stencil -> re:replace(Texname,"_2","_3",[global]);
            %        _-> ""
            %    end,
            %StencilUpperLayerName2 =
            %    case AlphaIntensity of
            %        stencil -> re:replace(Texname,"_1","_2",[global]);
            %        _-> ""
            %    end,
            %-----------------------------------------------------------------------
            % povman: change TextureType variable to most clarified ShaderType
            % TextureType = proplists:get_value(texture_type, Ps, ?DEF_TEXTURE_TYPE),
            %-----------------------------------------------------------------------

            ShaderType = proplists:get_value(texture_type, Ps, ?DEF_SHADER_TYPE),
			%{marble, wood,clouds} -> "global";
            TexCo =
                case TexType of
                    diffuse -> "uv";
					image -> "uv";
					jpeg -> "uv";
					map -> "uv"; %"<texco sval=\"uv\"/>";
					_ -> "global"
                end,

            %UpperLayer =
            %    case {Num,BlendMode,AlphaIntensity} of
            %        {"1",mix,_} ->  "";
            %        {"1",_,_} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
            %        {"2",_,stencil} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
            %        _ -> ""
            %    end,
            %% End Identify Modulator #
            %UseAlpha =
            %    case {Num,AlphaIntensity} of
            %        {"1",off} ->  "";
            %        {_,transparency} -> "<do_scalar bval=\"true\"/>";
            %        {_,diffusealphatransparency} -> "<use_alpha bval=\"true\"/>";
            %        {_,translucency} -> "<do_scalar bval=\"true\"/>";
            %        {_,specularity} -> "<do_scalar bval=\"true\"/>";
            %        {_,stencil} -> "<use_alpha bval=\"true\"/>";
            %        _ -> ""
            %    end,
            % TextureShaderType

            %ShaderName =
            %    case {Num,BlendMode} of
            %        {"1",_} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
            %        {_,mix} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
            %        _ -> ""
            %    end,

            %%------------------------------------------------------------------------------------->

            %% Identify Modulator # (w_default_Name_1 or w_default_Name_2)
            %Split=re:split(Texname,"_",[{return, list}]),
            %Num=lists:last(Split),

            DoColor = case ShaderType of
                    diffuse -> true;
                    mirror_color -> true;
                    glossy_reflect -> true;
                    glossy -> true;
                    _ -> false
                    %{mirror,transparency,translucency,bump}-> false
                end,
            %
            %_ShaderName =
            %    case {Num, BlendMode} of
            %            {"1",_} ->   "  "++ShaderType++" sval=\""++Texname++"\"/>";
            %            {_,mix} ->   "  "++ShaderType++" sval=\""++Texname++"\"/>";
            %            _ -> ""
            %    end,

            %% write shader type. TODO: search better way..
            %println(F, ShaderType),
            println(F, "\t<~s_shader sval=\"~s\"/>",[ShaderType,Texname]),

            %% entry to element shader list
            println(F,"\t<list_element>"),

            %% shader factor amount controled with 'Factor Modulator' slider in UI
            Factor = proplists:get_value(mod_colorfactor, Ps, ?DEF_MOD_COLORFACTOR),
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

            % def color for 'blended' by default
            println(F,"\t\t<def_col r=\"0.81\" g=\"0.8\" b=\"0.81\" a=\"1\"/>"),
            %
            println(F,"\t\t<def_val fval=\"1\"/>"),

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
            erlang:display("BlendMode for Modenumber "++format(BlendMode)),
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
            println(F, "\t\t<negative bval=\"false\"/>"),
            %
            println(F, "\t\t<noRGB bval=\"false\"/>"),

            % _StencilMode = proplists:get_value(stencil_mode, Ps, ?DEF_MOD_STENCIL),
            % erlang:display("Modulator. Stencil value is: "++StencilMode),


            println(F, "\t\t<stencil bval=\"false\"/>"),

            % for layers, is need 'stencil' param and more review code.. :)
            %%
            %_UpperColor =
            %    case Num of
            %        "1" ->  "<upper_color r=\"1\" g=\"1\" b=\"1\" a=\"1\"/>";
            %        _ -> ""
            %    end,

            %%% Change Number from Texname for UpperLayer
            %_ULayerName =
            %    case ModShaderType of % TODO: change for stencil mode
            %        stencil ->
            %            re:replace(Texname,"_2","_1",[global]);
            %        _->
            %            re:replace(Texname,"_1","_2",[global])
            %    end,
            % UpperLayer =
            %    case {Num, BlendMode, ModShaderType} of
            %            {"1",mix,_} ->  "";
            %            {"1",_,_} ->  UlayerName;
            %            {"2",_,stencil} -> UlayerName;
            %            _ -> ""
            %    end,

            Ulayer = "",
            %
            case Ulayer of
                "" ->
                    case DoColor of
                        true ->
                            println(F,
                                "\t\t<upper_color r=\"1\" g=\"1\" b=\"1\" a=\"1\"/>\n"
                                "\t\t<upper_value fval=\"0\"/>");
                        false ->
                            println(F,
                                "\t\t<upper_color r=\"0\" g=\"0\" b=\"0\" a=\"1\"/>\n" % alpha = 1 ??
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
            MappingType = "plain", %proplists:get_value(projection, Ps, ?DEF_PROJECTION),

            println(F, "\t\t<mapping sval=\"~s\"/>",[MappingType]),
            println(F, "\t\t<name sval=\"~s_mod\"/>",[Texname]),
            %
            OffX = proplists:get_value(offsetx, Ps, 0.0),
            OffY = proplists:get_value(offsety, Ps, 0.0),
            OffZ = proplists:get_value(offsetz, Ps, 0.0),
            %
            println(F,
                "\t\t<offset x=\"~w\" y=\"~w\" z=\"~w\"/>",
                [OffX, OffY, OffZ]),
            %
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
            % povman comment: Coordinates = proplists:get_value(coordinates, Ps, ?DEF_COORDINATES),
            %
            println(F, "\t\t<texco sval=\"~s\"/>",[TexCo]), %Coordinates]),
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