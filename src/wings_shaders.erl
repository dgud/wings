%%
%%  wings_shaders.erl --
%%
%%     Support for vertex & fragment shaders (for cards with OpenGL 2.0)
%%
%%  Copyright (c) 2001-2006 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id:$
%%

-module(wings_shaders).
-export([init/0, branding/0]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").

init() ->
    case wings_gl:support_shaders() of
	true ->
	    try
		%% Hemi
		Sh1 = wings_gl:compile(vertex, light_shader_src()),
		Prog1 = wings_gl:link_prog([Sh1]),
		gl:useProgram(Prog1),
		wings_pref:set_default(hl_lightpos,  {3.0,10.0,1.0}),
		wings_pref:set_default(hl_skycol,    {0.95,0.95,0.90}),
		wings_pref:set_default(hl_groundcol, {0.026,0.024,0.021}),
		wings_gl:set_uloc(Prog1, "LightPosition",
				  wings_pref:get_value(hl_lightpos)),
		wings_gl:set_uloc(Prog1, "SkyColor",
				  wings_pref:get_value(hl_skycol)),
		wings_gl:set_uloc(Prog1, "GroundColor",
				  wings_pref:get_value(hl_groundcol)),

		Prog2 = make_prog("hemilight"),
		Prog3 = make_prog("gooch"),
		Prog4 = make_prog("toon"),
		Prog5 = env_map_prog(),
		Prog6 = make_prog("brick"),

		ShVC = wings_gl:compile(vertex, read_shader("vertex_color.vs")),
		%% Vertex Color
		ProgV = wings_gl:link_prog([ShVC]),
		gl:useProgram(ProgV),
		wings_gl:set_uloc(ProgV, "Flag", 0),
		%% Normal Color
		ProgN = wings_gl:link_prog([ShVC]),
		gl:useProgram(ProgN),
		wings_gl:set_uloc(ProgN, "Flag", 1),

		%% Depth Mask
		ShD = wings_gl:compile(vertex, read_shader("spherical_ao.vs")),
		%ShD = wings_gl:compile(vertex, read_shader("depth.vs")),
		ProgD = wings_gl:link_prog([ShD]),
		gl:useProgram(ProgD),

		ShHDR = wings_gl:compile(vertex, read_shader("harmonics.vs")),

		%% HDRI 1
		ProgH1 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH1),
		wings_gl:set_uloc(ProgH1, "Type", 1),

		%% HDRI 2
		ProgH2 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH2),
		wings_gl:set_uloc(ProgH2, "Type", 2),

		%% HDRI 3
		ProgH3 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH3),
		wings_gl:set_uloc(ProgH3, "Type", 3),

		%% HDRI 4
		ProgH4 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH4),
		wings_gl:set_uloc(ProgH4, "Type", 4),

		%% HDRI 5
		ProgH5 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH5),
		wings_gl:set_uloc(ProgH5, "Type", 5),

		%% HDRI 6
		ProgH6 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH6),
		wings_gl:set_uloc(ProgH6, "Type", 6),

		%% HDRI 7
		ProgH7= wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH7),
		wings_gl:set_uloc(ProgH7, "Type", 7),

		%% HDRI 8
		ProgH8 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH8),
		wings_gl:set_uloc(ProgH8, "Type", 8),

		%% HDRI 9
		ProgH9 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH9),
		wings_gl:set_uloc(ProgH9, "Type", 9),

		%% HDRI 10
		ProgH10 = wings_gl:link_prog([ShHDR]),
		gl:useProgram(ProgH10),
		wings_gl:set_uloc(ProgH10, "Type", 10),

		?CHECK_ERROR(),
		gl:useProgram(0),
		put(light_shaders, {Prog1,Prog2,Prog3,Prog4,Prog5,Prog6,
				    ProgV,ProgN,ProgD}),
		io:format("Using GPU shaders.\n"),
		ok
	    catch _:_Err -> ok
	    end;
	false ->
	    ok
    end.

light_shader_src() ->
    <<"
       uniform vec3 LightPosition;
       uniform vec3 SkyColor;
       uniform vec3 GroundColor;

       void main()
       {
	   vec3 ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);
	   vec3 tnorm	   = normalize(gl_NormalMatrix * gl_Normal);
	   vec3 lightVec   = normalize(LightPosition - ecPosition);
	   float costheta  = dot(tnorm, lightVec);
	   float a	   = 0.5 + 0.5 * costheta;
			     // ATI needs this for vcolors to work
	   vec4 color	   = gl_FrontMaterial.diffuse * gl_Color;
	   gl_FrontColor   = color * vec4(mix(GroundColor, SkyColor, a), 1.0);
	   gl_TexCoord[0]  = gl_MultiTexCoord0;
	   gl_Position	   = ftransform();
       }
       ">>.

read_texture(FileName) ->
    Path = filename:join(wings_util:lib_dir(wings), "textures"),
    NewFileName = filename:join(Path, FileName),
    ImgRec = e3d_image:load(NewFileName, [{order,lower_left}]),
    ImgRec.

read_shader(FileName) ->
    Path = filename:join(wings_util:lib_dir(wings), "shaders"),
    NewFileName = filename:join(Path, FileName),
    {ok,Bin} = file:read_file(NewFileName),
    %Files = filelib:wildcard("*.?s", Path),
    %io:fwrite("~p\n", [Files]),
    Bin.

branding() ->
    wings_io:ortho_setup(),
    {WinW,WinH} = wings_wm:win_size(),
    ImgRec = read_texture("brand.png"),
    #e3d_image{width=ImgW,height=ImgH,image=ImgData} = ImgRec,
    Pad = 2,
    gl:rasterPos2i(WinW-ImgW-Pad, WinH-Pad),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:drawPixels(ImgW, ImgH, ?GL_RGBA, ?GL_UNSIGNED_BYTE, ImgData),
    %gl:disable(?GL_BLEND), %% Causes think lines on border ?
    ok.

env_map_prog() ->
    Shv = wings_gl:compile(vertex,   read_shader("envmap.vs")),
    Shf = wings_gl:compile(fragment, read_shader("envmap.fs")),
    Prog = wings_gl:link_prog([Shv,Shf]),
    gl:useProgram(Prog),

    %EnvImgRec = read_texture("cabin.png"),
    %EnvImgRec = read_texture("island.png"),
    %EnvImgRec = read_texture("hills.png"),
    %EnvImgRec = read_texture("nvlobby.png"),
    EnvImgRec = read_texture("grandcanyon.png"),
    %EnvImgRec = read_texture("opensea.png"),

    #e3d_image{width=ImgW,height=ImgH,image=ImgData} = EnvImgRec,
    TxId = 0, %[TxId] = gl:genTextures(1), % ?
    %TxId = wings_image:new("", EnvImgRec),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, ImgW, ImgH, 0, ?GL_RGB,
		  ?GL_UNSIGNED_BYTE, ImgData),
    gl:activeTexture(?GL_TEXTURE0),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    wings_gl:set_uloc(Prog, "EnvMap", 0),
    Prog.

make_prog(Name) ->
    Shv = wings_gl:compile(vertex, read_shader(Name ++ ".vs")),
    Shf = wings_gl:compile(fragment, read_shader(Name ++ ".fs")),
    Prog = wings_gl:link_prog([Shv,Shf]),
    gl:useProgram(Prog),
    Prog.
