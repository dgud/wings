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
-export([init/0, compile_all/0, use_prog/2,
         set_uloc/3, change_uloc/3,
         set_state/3, get_state/2, clear_state/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-define(cl_lightpos, {2.5, 2.5, 0.0}).
-define(hl_lightpos, {3000.0, 10000.0, 1000.0}).

init() ->
    wings_pref:set_default(hl_skycol, {0.95,0.95,0.90}),
    wings_pref:set_default(hl_groundcol, {0.026,0.024,0.021}),
    compile_all().

compile_all() ->
    HL = [{'LightPosition', wings_pref:get_value(hl_lightpos)},
	  {'SkyColor', wings_pref:get_value(hl_skycol)},
	  {'GroundColor', wings_pref:get_value(hl_groundcol)}],
    Programs0 = [{1, camera_light, [], "One Camera Lights"},
                 {2, hemilight, HL, "Hemispherical Lighting"},
                 {ambient_light, ambient_light, [], ""},
                 {infinite_light, infinite_light, [], ""},
                 {point_light, point_light, [], ""},
                 {spot_light, spot_light, [], ""},
                 {area_light, area_light, [], ""},
                 {light_light, light_light, [], ""}
                ],
    Make = fun({Id, Name, Uniforms, Desc}, Acc) ->
                   case make_prog(Name, Uniforms, Desc) of
                       {error, _Reason} -> Acc;
                       Prog -> [{Id, Prog}|Acc]
                   end
           end,
    Programs = lists:foldl(Make, [], Programs0),
    ?CHECK_ERROR(),
    gl:useProgram(0),
    ?SET(light_shaders, maps:from_list(Programs)),
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
            RS1 = set_uloc(ws_matrix, e3d_mat:identity(), RS#{shader=>Shader}),
            RS2 = set_uloc(ws_eyepoint, maps:get(ws_eyepoint, RS1), RS1),
            case Name of
                1 ->
                    WorldFromView = e3d_transform:inv_matrix(maps:get(view_from_world, RS2)),
                    LPos = e3d_mat:mul_point(WorldFromView, ?cl_lightpos),
                    set_uloc('ws_lightpos', LPos, RS2);
                2 ->
                    WorldFromView = e3d_transform:inv_matrix(maps:get(view_from_world, RS2)),
                    LPos = e3d_mat:mul_point(WorldFromView, ?hl_lightpos),
                    set_uloc('ws_lightpos', LPos, RS2);
                _ ->
                    RS2
            end
    end.

set_uloc(Id, To, Rs0) ->
    case maps:get(shader, Rs0) of
        0 ->
            Rs0;
        #{name:=Name} = Shader ->
            case maps:get({Name, Id}, Rs0, undefined) of
                To ->
                    Rs0;
                _ ->
                    wings_gl:set_uloc(Shader, Id, To),
                    Rs0#{{Name, Id} => To}
            end
    end.

change_uloc(Id, To, Rs0) ->
    case maps:get(shader, Rs0) of
        0 -> {false, Rs0};
        #{name:=Name} = Shader ->
            case maps:get({Name, Id}, Rs0, undefined) of
                To -> {false, Rs0};
                _ ->
                    wings_gl:set_uloc(Shader, Id, To),
                    {true, Rs0#{{Name, Id} => To}}
            end
    end.

get_state(Id, Rs0) ->
    case maps:get(shader, Rs0) of
        0 -> undefined;
        #{name:=Name} -> maps:get({Name, Id}, Rs0, undefined)
    end.

set_state(Id, To, Rs0) ->
    case maps:get(shader, Rs0) of
        0 -> {false, Rs0};
        #{name:=Name} ->
            case maps:get({Name, Id}, Rs0, undefined) of
                To -> {false, Rs0};
                _ ->
                    {true, Rs0#{{Name, Id} => To}}
            end
    end.

clear_state(Id, Rs0) ->
    case maps:get(shader, Rs0) of
        0 -> {false, Rs0};
        #{name:=Name} ->
            maps:remove({Name, Id}, Rs0)
    end.

make_prog(Name, Vars, Desc) ->
    File = atom_to_list(Name),
    Vs = try read_shader(File, ".vs")
         catch throw:E0 -> io:format("Error: vertex shader: ~s ~p~n", [File, E0]) end,
    Fs = try read_shader(File, ".fs")
         catch throw:E1 -> io:format("Error: fragment shader: ~s ~p~n", [File, E1]) end,
    Compile = fun(VsBin, FsBin) ->
                      Shv = wings_gl:compile(vertex, VsBin),
                      Shf = wings_gl:compile(fragment, FsBin),
                      wings_gl:link_prog([Shv,Shf],[{?TANGENT_ATTR, "wings_tangent"}])
              end,
    case {Vs, Fs} of
        {{VsFile,VsBin}, {FsFile,FsBin}} ->
            try Compile(VsBin,FsBin) of
                Prog ->
                    Res = setup_uniforms(Prog, Vars, Name, Desc),
                    %% io:format("Deleting cache: ~p~n",[VsFile]),
                    %% io:format("Deleting cache: ~p~n",[FsFile]),
                    file:delete(VsFile),
                    file:delete(FsFile),
                    Res
            catch
                throw:{vertex, Str} ->
                    io:format("~s in see: ~p~n~n", [Str, VsFile]),
                    {error, Str};
                throw:{fragment, Str} ->
                    io:format("~s in see: ~p~n~n", [Str, FsFile]),
                    {error, Str};
                _:Reason ->
                    io:format("~p in see: ~p~n~n", [Reason, FsFile]),
                    gl:useProgram(0),
                    {error, Reason}
            end;
        _ ->
            {error, enoent}
    end.

setup_uniforms(Prog, Vars, Name, Desc) ->
    gl:useProgram(Prog),
    N = gl:getProgramiv(Prog, ?GL_ACTIVE_UNIFORMS),
    StrSize = gl:getProgramiv(Prog, ?GL_ACTIVE_UNIFORM_MAX_LENGTH),
    Uniforms = fetch_uniforms(0, N, StrSize+1, Prog),
    %% io:format("Prog: ~p ~s~n", [Prog, Name]),
    %% [io:format("~5w ~s ~n",[Loc, Str]) || {Str, Loc} <- Uniforms],
    Res = maps:from_list([{name,Name},{prog,Prog},{desc,Desc}|Uniforms]),
    wings_gl:set_uloc(Res, 'DiffuseMap',  ?DIFFUSE_MAP_UNIT),
    wings_gl:set_uloc(Res, 'NormalMap',   ?NORMAL_MAP_UNIT),
    wings_gl:set_uloc(Res, 'PBRMap',      ?PBR_MAP_UNIT),
    wings_gl:set_uloc(Res, 'EmissionMap', ?EMISSION_MAP_UNIT),
    %% Lights
    wings_gl:set_uloc(Res, 'EnvBrdfMap',  ?ENV_BRDF_MAP_UNIT),
    wings_gl:set_uloc(Res, 'EnvSpecMap',  ?ENV_SPEC_MAP_UNIT),
    wings_gl:set_uloc(Res, 'EnvDiffMap',  ?ENV_DIFF_MAP_UNIT),
    wings_gl:set_uloc(Res, 'AreaLTCMap',  ?AREA_LTC_MAT_UNIT),

    [wings_gl:set_uloc(Res, Var, Val) || {Var,Val} <- Vars],
    Res.

fetch_uniforms(N, Max, StrSize, Prog) when N < Max ->
    {_, _, Name} = gl:getActiveUniform(Prog, N, StrSize),
    case wings_gl:uloc(Prog, Name) of
        -1 -> fetch_uniforms(N+1, Max, StrSize, Prog); %% Builtin
        Loc ->
            {TruncName,_} = string:take(Name, "[", true),
            [{list_to_atom(TruncName), Loc}
             | fetch_uniforms(N+1, Max, StrSize, Prog)]
    end;
fetch_uniforms(_N, _Max, _StrSize, _Prog) -> [].

%%
%% Read and parse (to fix includes) shaders

read_shader(FileName, Ext) ->
    {File,Bin} =
        try read_shader_1(FileName++Ext, false)
        catch _:{file,enoent} ->
                read_shader_1("standard"++Ext, true)
        end,
    CacheDir = filename:basedir(user_cache, "wings3d"),
    CacheFile = filename:join(CacheDir, filename:basename(File)),
    ok = filelib:ensure_dir(CacheFile),
    ok = file:write_file(CacheFile, Bin),
    {CacheFile, Bin}.

read_shader_1(FileName, Log) ->
    Path = filename:join(wings_util:lib_dir(wings), "shaders"),
    NewFileName = filename:join(Path, FileName),
    case file:read_file(NewFileName) of
        {ok, Bin} ->
            {NewFileName, iolist_to_binary(replace_include(Bin))};
        {error, ER} ->
            Log andalso io:format("ERROR: ~p failed reading ~s~n",[?MODULE, NewFileName]),
            throw({file, ER})
    end.

replace_include(Bin) ->
    case binary:split(Bin, <<"#include">>, []) of
        [Pre, Rest] ->
            {IncFile0, Post} = string:take(Rest, [$\n,"\r\n"], true),
            IncFile = string:trim(string:trim(IncFile0), both, "\""),
            {_,IncBin} = try read_shader_1(IncFile, false)
                         catch throw:{file,enoent} ->
                                 io:format("Could not read include file: '~s'~n",[IncFile]),
                                 throw({file,enoent})
                         end,
            IncStr = ["\n// included from: ", IncFile, "\n"],
            [Pre, IncStr, IncBin, IncStr | replace_include(Post)];
        Completed ->
            Completed
    end.
