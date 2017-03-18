%%
%%  wings_shaders.erl --
%%
%%     Support for vertex & fragment shaders (for cards with OpenGL 2.0).
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_shaders).
-export([init/0, use_prog/2, read_texture/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").

init() ->
    wings_pref:set_default(hl_lightpos, {3000.0, 10000.0, 1000.0}),
    wings_pref:set_default(hl_skycol, {0.95,0.95,0.90}),
    wings_pref:set_default(hl_groundcol, {0.026,0.024,0.021}),
    HL = [{"LightPosition", wings_pref:get_value(hl_lightpos)},
	  {"SkyColor", wings_pref:get_value(hl_skycol)},
	  {"GroundColor", wings_pref:get_value(hl_groundcol)}],
    Programs =
        [{1, make_prog(camera_light, "Two Camera Lights")},
         {2, make_prog(hemilight, HL, "Hemispherical Lighting")},
         {ambient_light, make_prog(ambient_light, "")},
         {infinite_light, make_prog(infinite_light, "")},
         {point_light, make_prog(point_light, "")},
         {spot_light, make_prog(spot_light, "")},
         {envmap, make_prog(envmap, "Environment Mapping")}
        ],
    ?CHECK_ERROR(),
    gl:useProgram(0),
    ?SET(light_shaders, maps:from_list(Programs)),
    %io:format("Using GPU shaders.\n"),
    %io:format("~p\n",[Programs]),
    ok.

use_prog(0, RS) ->
    wings_gl:use_prog(0),
    RS#{shader=>0};
use_prog(Name, RS) ->
    Active = maps:get(shader, RS, 0),
    case ?GET(light_shaders) of
        #{Name:=Active} -> RS;
        #{Name:=Shader} ->
            #{prog:=Prog} = Shader,
            wings_gl:use_prog(Prog),
            RS#{shader=>Shader}
    end.

read_texture(FileName) ->
    Path = filename:join(wings_util:lib_dir(wings), "textures"),
    NewFileName = filename:join(Path, FileName),
    ImgRec = e3d_image:load(NewFileName, [{order,lower_left}]),
    ImgRec.

read_shader(FileName, Ext) ->
    read_shader(FileName, undefined, Ext).

read_shader(FileName, Orig, Ext) ->
    Path = filename:join(wings_util:lib_dir(wings), "shaders"),
    NewFileName = filename:join(Path, FileName),
    case file:read_file(NewFileName++Ext) of
        {ok, Bin} -> Bin;
        {error, _} when Orig =:= undefined ->
            read_shader("standard", FileName, Ext);
        {error, ER} ->
            io:format("ERROR: failed reading ~s or ~s ~s~n",[FileName, Orig, Ext]),
            error(ER)
    end.

make_prog(Name, Desc) ->
    make_prog(Name, [], Desc).
make_prog(Name, Vars, Desc) ->
    File = atom_to_list(Name),
    Shv = wings_gl:compile(vertex, read_shader(File, ".vs")),
    Shf = wings_gl:compile(fragment, read_shader(File, ".fs")),
    Prog = wings_gl:link_prog([Shv,Shf],[{?TANGENT_ATTR, "wings_tangent"}]),
    gl:useProgram(Prog),
    N = gl:getProgramiv(Prog, ?GL_ACTIVE_UNIFORMS),
    StrSize = gl:getProgramiv(Prog, ?GL_ACTIVE_UNIFORM_MAX_LENGTH),
    Uniforms = fetch_uniforms(0, N, StrSize+1, Prog),
    %% io:format("Prog: ~p ~s~n", [Prog, Name]),
    %% [io:format("~5w ~s ~n",[Loc, Str]) || {Str, Loc} <- Uniforms],
    envmap(Name),
    Res = maps:from_list([{name,Name},{prog,Prog},{desc,Desc}|Uniforms]),
    wings_gl:set_uloc(Res, "DiffuseMap", ?DIFFUSE_MAP_UNIT),
    wings_gl:set_uloc(Res, "NormalMap",  ?NORMAL_MAP_UNIT),
    wings_gl:set_uloc(Res, "EnvMap", ?ENV_MAP_UNIT),
    [wings_gl:set_uloc(Res, Var, Val) || {Var,Val} <- Vars],
    Res.

fetch_uniforms(N, Max, StrSize, Prog) when N < Max ->
    {_, _, Name} = gl:getActiveUniform(Prog, N, StrSize),
    %%io:format("  ~s: ~p~n",[Name, wings_gl:uloc(Prog, Name)]),
    case wings_gl:uloc(Prog, Name) of
        -1 -> fetch_uniforms(N+1, Max, StrSize, Prog); %% Builtin
        Loc -> [{Name, Loc} | fetch_uniforms(N+1, Max, StrSize, Prog)]
    end;
fetch_uniforms(_N, _Max, _StrSize, _Prog) -> [].

envmap(envmap) ->
    FileName = "grandcanyon.png",
    EnvImgRec = read_texture(FileName),
    #e3d_image{width=ImgW,height=ImgH,image=ImgData} = EnvImgRec,
    [TxId] = gl:genTextures(1),
    gl:activeTexture(?GL_TEXTURE0 + ?ENV_MAP_UNIT),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGB, ImgW, ImgH, 0, ?GL_RGB,
		  ?GL_UNSIGNED_BYTE, ImgData),
    gl:activeTexture(?GL_TEXTURE0);
envmap(_) -> ok.
