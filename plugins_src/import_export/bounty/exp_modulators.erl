%%%%

export_modulator(F, Texname, Maps, {modulator,Ps}, _Opacity) when is_list(Ps) ->
    case mod_enabled_mode_type(Ps, Maps) of
        {false,_,_} ->
            off;
        {true,Mode,Type} ->

            AlphaIntensity = proplists:get_value(alpha_intensity, Ps, ?DEF_MOD_ALPHA_INTENSITY),

            %%% Start Change Number from Texname for UpperLayer

            UpperLayerName =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_2","_1",[global]);
                    _-> re:replace(Texname,"_1","_2",[global])
                end,

            %%% Start Change Number from Texname for Stencil Input

            StencilInputName =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_2","_3",[global]);
                    _-> ""
                end,

%%% Start Change Number from Texname for Stencil UpperLayer Name 2

            StencilUpperLayerName2 =
                case AlphaIntensity of
                    stencil -> re:replace(Texname,"_1","_2",[global]);
                    _-> ""
                end,


%            _SizeX = proplists:get_value(size_x, Ps, ?DEF_MOD_SIZE_X),
%            _SizeY = proplists:get_value(size_y, Ps, ?DEF_MOD_SIZE_Y),
%            _SizeZ = proplists:get_value(size_z, Ps, ?DEF_MOD_SIZE_Z),
            TextureType = proplists:get_value(texture_type, Ps, ?DEF_TEXTURE_TYPE),
            Diffuse = proplists:get_value(diffuse, Ps, ?DEF_MOD_DIFFUSE),
%%      _Specular = proplists:get_value(specular, Ps, ?DEF_MOD_SPECULAR),
%%      Ambient = proplists:get_value(ambient, Ps, ?DEF_MOD_AMBIENT),
            Shininess = proplists:get_value(shininess, Ps, ?DEF_MOD_SHININESS),
            Normal = proplists:get_value(normal, Ps, ?DEF_MOD_NORMAL),
%%            _Color = Diffuse * Opacity,
%%            _HardValue = Shininess,
%%            _Transmission = Diffuse * (1.0 - Opacity),
%%      _Reflection = Ambient,
            TexCo =
                case Type of
                    {image,jpeg,map,_}-> "<texco sval=\"uv\"/>";
                    {marble, wood,clouds} -> "<texco sval=\"global\"/>";
                    _ -> ""
                end,

            ModeNumber =
                case Mode of
                    %mix -> "0"; % test..
                    add -> "1";
                    mul -> "2";
                    sub -> "3";
                    scr -> "4";
                    divide -> "5";
                    dif -> "6";
                    dar -> "7";
                    lig -> "8";
                    _ -> "0"
                end,

%% Start Identify Modulator # (w_default_Name_1 or w_default_Name_2)
            Split=re:split(Texname,"_",[{return, list}]),
            Num=lists:last(Split),
            UpperLayer =
                case {Num,Mode,AlphaIntensity} of
                    {"1",mix,_} ->  "";
                    {"1",_,_} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
                    {"2",_,stencil} ->  "<upper_layer sval=\""++UpperLayerName++"\"/>";
                    _ -> ""
                end,
%% End Identify Modulator #

            UpperColor =
                case Num of
                    "1" ->  "<upper_color r=\"1\" g=\"1\" b=\"1\" a=\"1\"/>";
                    _ -> ""
                end,

            UseAlpha =
                case {Num,AlphaIntensity} of
                    {"1",off} ->  "";
                    {_,transparency} -> "<do_scalar bval=\"true\"/>";
                    {_,diffusealphatransparency} -> "<use_alpha bval=\"true\"/>";
                    {_,translucency} -> "<do_scalar bval=\"true\"/>";
                    {_,specularity} -> "<do_scalar bval=\"true\"/>";
                    {_,stencil} -> "<use_alpha bval=\"true\"/>";
                    _ -> ""
                end,



            TextureShaderType =
                case {Normal,TextureType,AlphaIntensity} of
                    {0.0,diffusetexture,off} -> "<diffuse_shader";
                    {0.0,mirrorcolortexture,off} -> "<mirror_color_shader";
                    {0.0,mirrortexture,off} -> "<mirror_shader";
                    {0.0,glossytexture,off} -> "<glossy_shader";
                    {0.0,glossyreflecttexture,off} -> "<diffuse_reflect_shader";
                    {0.0,transparencytexture,off} -> "<transparency_shader";
                    {0.0,translucencytexture,off} -> "<translucency_shader";
                    {0.0,bumptexture,off} -> "<bump_shader";

                    {0.0,diffusetexture,transparency} -> "<transparency_shader";
                    {0.0,mirrorcolortexture,transparency} -> "<transparency_shader";
                    {0.0,mirrortexture,transparency} -> "<transparency_shader";
                    {0.0,glossytexture,transparency} -> "<transparency_shader";
                    {0.0,glossyreflecttexture,transparency} -> "<transparency_shader";
                    {0.0,transparencytexture,transparency} -> "<transparency_shader";

                    {0.0,diffusetexture,diffusealphatransparency} -> "<diffuse_shader";
                    {0.0,transparencytexture,diffusealphatransparency} -> "<diffuse_shader";

                    {0.0,diffusetexture,translucency} -> "<translucency_shader";
                    {0.0,glossytexture,translucency} -> "<translucency_shader";
                    {0.0,translucencytexture,translucency} -> "<translucency_shader";

                    {0.0,diffusetexture,specularity} -> "<mirror_shader";
                    {0.0,mirrorcolortexture,specularity} -> "<mirror_shader";
                    {0.0,mirrortexture,specularity} -> "<mirror_shader";
                    {0.0,glossytexture,specularity} -> "<mirror_shader";
                    {0.0,glossyreflecttexture,specularity} -> "<mirror_shader";

                    {0.0,diffusetexture,stencil} -> "<diffuse_shader";
                    _ -> "<bump_shader"
                end,

            ShaderName =
                case {Num,Mode} of
                    {"1",_} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
                    {_,mix} ->   "  "++TextureShaderType++" sval=\""++Texname++"\"/>";
                    _ -> ""
                end,


            case AlphaIntensity of
                stencil ->
%%Stencil Export Start
                    println(F, " <!--Start Stencil Section Here-->

                <list_element>
                <element sval=\"shader_node\"/>
                <name sval=\"~s\"/>
                <input sval=\"~s_mod\"/>

                <noRGB bval=\"true\"/>
                <stencil bval=\"true\"/>
                "++UpperLayer++"

                <type sval=\"layer\"/>
                <mode ival=\""++ModeNumber++"\"/>
                </list_element>

                <list_element>
                <element sval=\"shader_node\"/>
                <name sval=\"~s_mod\"/>
                "++TexCo++"
                <mapping sval=\"plain\"/>
                <texture sval=\"~s\"/>
                <type sval=\"texture_mapper\"/>
                <bump_strength fval=\"~.3f\"/>
                </list_element>

                <diffuse_shader sval=\"diff_layer2\"/>
                                <list_element>
                                <element sval=\"shader_node\"/>
                                <name sval=\"diff_layer2\"/>
                <input sval=\""++StencilInputName++"_mod\"/>
                <upper_layer sval=\""++StencilUpperLayerName2++"\"/>
                                <type sval=\"layer\"/>
                <mode ival=\""++ModeNumber++"\"/>
                                </list_element>

    <!--End Stencil Section Here-->",
                        [Texname,Texname,Texname,Texname,Normal
                        ]);
%%Stencil Export End
                _ ->

                    println(F, "  "++ShaderName++"
                <list_element>
                <element sval=\"shader_node\"/>
                <name sval=\"~s\"/>
                <input sval=\"~s_mod\"/>
                                "++UpperLayer++"
                "++UpperColor++"
                "++UseAlpha++"
                <type sval=\"layer\"/>
                <mode ival=\""++ModeNumber++"\"/>
                <colfac fval=\"~.3f\"/>
                <valfac fval=\"~.3f\"/>
                </list_element>
                <list_element>
                <element sval=\"shader_node\"/>
                <name sval=\"~s_mod\"/>
                "++TexCo++"
                <mapping sval=\"plain\"/>
                <texture sval=\"~s\"/>
                <type sval=\"texture_mapper\"/>

                <bump_strength fval=\"~.3f\"/>
                </list_element>",
                        [Texname,Texname,Diffuse,Shininess,Texname,Texname,Normal
                        ])

            end

    end.