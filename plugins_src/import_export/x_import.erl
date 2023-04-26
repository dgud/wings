%%
%%  x_import.erl
%%
%%     Import X files
%%
%%  Copyright 2022-2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(x_import).
-export([do_import/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

%%%
%%%

-record(x_mat, {
    diffuse  :: {float(), float(), float(), float()},
    power    :: float(),
    specular :: {float(), float(), float(), float()},
    emissive :: {float(), float(), float(), float()},
    txfile   :: [string()]
}).

-record(x_mesh, {
    vslist,
    fslist,
    normals = none,
    texcoords = none,
    matlist = none
}).

-record(x_frame, {
    name :: string(),
    mesh :: #x_mesh{} | none
}).

%%%
%%% Import.
%%%

-define(WPCX, wpc_x).

props() ->
    [{ext,".x"},{ext_desc,?__(1,"DirectX .x File")}].
    
dialog_import() ->
    [wpa:dialog_template(?WPCX, import, [include_colors])].

do_import(Ask, _St) when is_atom(Ask) ->
    Dialog = [
    ] ++ dialog_import(),
    wpa:dialog(Ask, ?__(1,"DirectX .x Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{x,Res}}}
           end);
do_import(Attr, St) ->
    wpa:import(props(), import_fun(Attr), St).

set_pref(KeyVals) ->
    wpa:pref_set(?WPCX, KeyVals).

import_transform(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

import_fun(Attr) ->
    fun(Filename) ->
        set_pref(Attr),
        FilenameExt = filename:extension(Filename),
        _ObjName = filename:basename(Filename,FilenameExt),
        case import_x_file(Filename) of
            {ok, {Meshes, Mats_0}} ->
                Objs = [import_mesh(MName,M) || {MName,M} <- Meshes],
                Mats = [import_mat(MName,M) || {MName,M} <- Mats_0],
                E3dfile = #e3d_file{objs=Objs,mat=Mats},
                {ok, import_transform(E3dfile, Attr)};
            {error, _}=Err ->
                Err
        end
    end.

import_mesh(ObjName, #x_mesh{vslist=Vs_0,fslist=Fs_0,normals=_Ns,texcoords=TxC_0,matlist=ML}=_) ->
    Vs = [Coord || Coord <- Vs_0],
    case TxC_0 of
        none -> TxC = [];
        _    -> TxC = [ Coord || Coord <- TxC_0]
    end,
    TxA = array:from_list(TxC),
    case ML of
        none -> Efs = [ to_w_face(L,[],TxA) || {face,L} <- Fs_0 ];
        _    -> Efs = [ to_w_face(L,[M],TxA) || {{face,L},M} <- lists:zip(Fs_0, ML) ]
    end,
    HEs = all_edges([X || #e3d_face{vs=X} <- Efs]),
    Mesh = #e3d_mesh{
        type=polygon,
        vs=Vs,
        fs=Efs,
        tx=TxC,
        ns=[],
        he=HEs
    },
    Obj = #e3d_object{name=ObjName,obj=Mesh},
    Obj.
    
to_w_face(L,[],_) ->
    #e3d_face{
        vs=L,
        mat=[]
    };
to_w_face(L,Mat,TxA) ->
    #e3d_face{
        vs=L,
        tx=[case array:get(Idx+1, TxA) of
                undefined -> 0;
                _ -> Idx
            end || Idx <- L],
        mat=Mat
    }.


import_mat(MatName, #x_mat{diffuse=DiffuseCol,power=Power,specular=SpecularCol,
                           emissive=EmissiveCol,txfile=TxL}=_) ->
    case TxL of
        [] ->
            Maps_L = [];
        [F | _] ->
            case import_mat_image(F) of
                {error, _} ->
                    io:format("NOTE: Image not found: ~p~n", [F]),
                    Maps_L = [];
                #e3d_image{}=E3dImg ->
                    Maps_L = [{diffuse, E3dImg}]
            end
    end,
    Maps = {maps, Maps_L},
    OpenGL = {opengl,
             [{ambient,{0.0,0.0,0.0,0.0}},
              {specular,SpecularCol},
              {shininess,Power},
              {diffuse,DiffuseCol},
              {emission,EmissiveCol},
              {metallic,0.1},
              {roughness,0.8},
              {vertex_colors, set}]},
    {MatName, [Maps, OpenGL]}.
    
import_mat_image(FilePath) ->
    {ok, E3dImage} = get_bitmap_by_ext(FilePath),
    E3dImage.
    
get_bitmap_by_ext(FilePath) ->
    case string:to_lower(filename:extension(FilePath)) of
        ".jpeg" -> Ext = ".jpg";
        Ext_0   -> Ext = Ext_0
    end,
    case Ext of
        ".png" ->
            F = fun read_png/1;
        ".jpg" ->
            F = fun read_jpeg/1;
        _ ->
            F = none
    end,
    case F of
        none ->
            {error, none};
        _ ->
            F(FilePath)
    end.

read_jpeg(FileName) ->
    BlockWxMsgs = wxLogNull:new(),
    Ret = read_jpeg_1(FileName),
    wxLogNull:destroy(BlockWxMsgs),
    Ret.
read_jpeg_1(FileName) ->
    Image = wxImage:new(),
    case wxImage:loadFile(Image, FileName) of
        true ->
            E3d = wings_image:wxImage_to_e3d(Image),
            wxImage:destroy(Image),
            {ok, e3d_image:fix_outtype(FileName, E3d, [])};
        false ->
            {error, none}
    end.
read_png(FileName) ->
    E3D = e3d__png:load(FileName),
    {ok, E3D}.


%%%
%%% Read .x file
%%%

import_x_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Cont} ->
            {ok, O} = xf(Cont, [], []),
            
            ets:new(?MODULE, [named_table,public,ordered_set]),
            Return = try 
                {ok, O_1} = parse(O),
                {ok, {Meshes, Materials}} = split_mesh_mtls(O_1),
                {ok, {Meshes, Materials}}
            catch _:Err:ST ->
                io:format(".x Import Error: ~P in"++" ~p~n", [Err,30,ST]),
                {error, lists:flatten(".x Import Error")}
            end,
            ets:delete(?MODULE),
            Return
    end.

xf(<<C, R/binary>>, [], O) when C =:= ${ ->
    xf(R, [], [open|O]);
xf(<<C, R/binary>>, [], O) when C =:= $} ->
    xf(R, [], [close|O]);
xf(<<";;", R/binary>>, [], O) ->
    xf(R, [], [d4|O]);
xf(<<";,", R/binary>>, [], O) ->
    xf(R, [], [d3|O]);
xf(<<";", R/binary>>, [], O) ->
    xf(R, [], [d2|O]);
xf(<<",", R/binary>>, [], O) ->
    xf(R, [], [d1|O]);

xf(<<C, R/binary>>, [], O) when C =:= 32; C =:= 9; C =:= 10; C =:= 13 ->
    xf(R, [], O);

xf(<<C, R/binary>>, [], O) when C =:= 34 ->
    {ok, Str, R_1} = x_str(R),
    xf(R_1, [], [{str, Str}|O]);

xf(<<C, R/binary>>, [], O) when C =:= $# ->
    {ok, R_1} = x_comment(R),
    xf(R_1, [], O);
xf(<<C, C, R/binary>>, [], O) when C =:= $/ ->
    {ok, R_1} = x_comment(R),
    xf(R_1, [], O);

xf(<<C, _/binary>>=R, S, O)
    when length(S) > 0, (
        C =:= 34 orelse C =:= 32 orelse C =:= 9 orelse 
        C =:= 10 orelse C =:= 13 orelse C =:= ${ orelse C =:= $} orelse
        C =:= $; orelse C =:= $, ) orelse C =:= $# orelse $/ ->
    xf(R, [], [{word, lists:reverse(S)}|O]);
    
xf(<<C, R/binary>>, S, O) ->
    xf(R, [C|S], O);

xf(<<>>, [], O) ->
    {ok, lists:reverse(O)};
xf(<<>>, S, O) when length(S) > 0 ->
    xf(<<>>, [], [lists:reverse(S)|O]).

x_str(S) ->
    x_str(S, []).
x_str(<<34, R/binary>>, O) ->
    {ok, lists:reverse(O), R};
x_str(<<C, R/binary>>, O) ->
    x_str(R, [C | O]).

x_comment(<<C, R/binary>>) when C =:= 10; C =:= 13 ->
    {ok, R};
x_comment(<<_, R/binary>>) ->
    x_comment(R).

parse(L) ->
    {ok, L_1} = parse_header(L),
    parse_1(L_1, []).
parse_1([_|_]=L, O) ->
    {ok, Elem, L_1} = parse_element(L),
    parse_1(L_1, [Elem | O]);
parse_1([], O) ->
    {ok, lists:reverse(O)}.

parse_header([{word, "xof"}, {word, _B}, {word, _C}|L]) ->
    {ok, L}.

parse_element([{word, "Material"}, {word, Name}, open | L]) ->
    {ok, Num4A, L_1} = get_4_numbers(L),
    {ok, Num4B, L_2} = get_4_numbers(L_1),
    {ok, Num3C, L_3} = get_3_numbers(L_2),
    {ok, NewMat, L_4} = parse_material_1(L_3, [{diffuse, Num4A}, {power_specular, Num4B}, {emissive, Num3C}]),
    case L_4 of
        [close | L_5] ->
            NameAtm = material_atom(Name),
            {ok, {with_mtl_spec, {NameAtm, NewMat}}, L_5}
    end;
parse_element([{word, "Frame"}, {word, Name}, open | L]) ->
    {ok, Mesh, L_1} = parse_frame_1_l(L),
    case L_1 of
        [close | L_2] ->
            {ok, #x_frame{name=Name,mesh=Mesh}, L_2}
    end;
parse_element([{word, "template"}, {word, Name}, open | L]) ->
    ets:insert(?MODULE, {{template, Name}, true}),
    L_1 = parse_template(L),
    case L_1 of
        [close | L_2] ->
            {ok, unused, L_2}
    end;
parse_element([{word, "AnimationSet"}, {word, _Name}, open | L]) ->
    L_1 = parse_animation_set(L),
    case L_1 of
        [close | L_2] ->
            {ok, unused, L_2}
    end;
parse_element([close | L]) ->
    %% An unexpected closing bracket when we are already at the top level,
    %% we'll ignore it.
    parse_element(L);
parse_element([{word, Unknown}, open | L]) ->
    parse_element_unknown(Unknown, none, L);
parse_element([{word, Unknown}, {word, Name}, open | L]) ->
    parse_element_unknown(Unknown, Name, L).

parse_element_unknown(TemplateName, _Name, L) ->
    case ets:lookup(?MODULE, {template, TemplateName}) of
        [] ->
            error(unexpected);
        [{_, _TODO}] ->
            L_1 = parse_unused_section(L),
            case L_1 of
                [close | L_2] ->
                    {ok, unused, L_2}
            end
    end.

parse_template([open, {word, _}, close | L]) ->
    parse_template(L);
parse_template([{word, _}, open | L]) ->
    [close | L_1] = parse_template(L),
    parse_template(L_1);
parse_template([{word, _}, _, open | L]) ->
    [close | L_1] = parse_template(L),
    parse_template(L_1);
parse_template([close | _]=L) ->
    L;
parse_template([_Unused | L]) ->
    parse_template(L).


parse_animation_set([open, {word, _}, close | L]) ->
    parse_animation_set(L);
parse_animation_set([{word, _}, open | L]) ->
    [close | L_1] = parse_animation_set(L),
    parse_animation_set(L_1);
parse_animation_set([{word, _}, _, open | L]) ->
    [close | L_1] = parse_animation_set(L),
    parse_animation_set(L_1);
parse_animation_set([close | _]=L) ->
    L;
parse_animation_set([_Unused | L]) ->
    parse_animation_set(L).


parse_material_1(L, Props) ->
    parse_material_1(L, Props, []).
parse_material_1([{word, "TextureFilename"}, open | L], Props, TxL) ->
    case get_string(L) of
        {ok, Str, [close | L_1]} ->
            parse_material_1(L_1, Props, [Str | TxL])
    end;
parse_material_1([close | _]=L, Props, TxL) ->
    {RD, GD, BD, AD} = proplists:get_value(diffuse, Props, [0.7, 0.7, 0.7, 1.0]),
    {Power, RS, GS, BS} = proplists:get_value(power_specular, Props, [0.0, 0.0, 0.0, 0.0]),
    {RE, GE, BE} = proplists:get_value(emissive, Props, [0.0, 0.0, 0.0]),
    FileNames = lists:reverse(TxL),
    DiffuseCol = {RD, GD, BD, AD},
    SpecularCol = {RS, GS, BS, 1.0},
    EmissiveCol = {RE, GE, BE, 1.0},
    NewMat = #x_mat{diffuse=DiffuseCol,power=Power,
        specular=SpecularCol,emissive=EmissiveCol,txfile=FileNames},
    {ok, NewMat, L}.

get_string([{str, Str}, d2 | L]) ->
    {ok, Str, L};
get_string([{str, Str} | [close | _]=L]) ->
    {ok, Str, L}.

get_4_numbers(L) ->
    {ok, N1, L_1} = get_number_d2(L),
    {ok, N2, L_2} = get_number_d2(L_1),
    {ok, N3, L_3} = get_number_d2(L_2),
    {ok, N4, L_4} = get_number_d4(L_3),
    {ok, {N1,N2,N3,N4}, L_4}.
get_3_numbers(L) ->
    {ok, N1, L_1} = get_number_d2(L),
    {ok, N2, L_2} = get_number_d2(L_1),
    {ok, N3, L_3} = get_number_d4(L_2),
    {ok, {N1,N2,N3}, L_3}.


get_number_d1_or_d2([{word, Num_S}, d1 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L};
get_number_d1_or_d2([{word, Num_S}, d2 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.

get_number_d1([{word, Num_S}, d1 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.
get_number_d2([{word, Num_S}, d2 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.
get_number_d3([{word, Num_S}, d3 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.
get_number_d4([{word, Num_S}, d4 | L]) ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.
    
parse_num(Num_S) ->
    case string:to_float(Num_S) of
        {Num, _} when is_float(Num) ->
            {ok, Num};
        {error, no_float} ->
            case string:to_integer(Num_S) of
                {Num, _} when is_integer(Num) ->
                    {ok, Num}
            end
    end.
    
get_numbers_d1d1d1d1(L) ->
    {ok, N1, L_1} = get_number_d1(L),
    {ok, N2, L_2} = get_number_d1(L_1),
    {ok, N3, L_3} = get_number_d1(L_2),
    {ok, N4, L_4} = get_number_d1(L_3),
    {ok, N1, N2, N3, N4, L_4}.
    
get_numbers_d1d1d1d4(L) ->
    {ok, N1, L_1} = get_number_d1(L),
    {ok, N2, L_2} = get_number_d1(L_1),
    {ok, N3, L_3} = get_number_d1(L_2),
    {ok, N4, L_4} = get_number_d4(L_3),
    {ok, N1, N2, N3, N4, L_4}.

parse_frame_1_l(L) ->
    parse_frame_1_l(L, []).
parse_frame_1_l(L, O) ->
    case L of
        [close | _] ->
            {ok, proplists:get_value(mesh, O, none), L};
        _ ->
            {ok, AL, L_1} = parse_frame_1(L),
            parse_frame_1_l(L_1, [AL | O])
    end.
    
parse_frame_1([{word, "FrameTransformMatrix"}, open | L]) ->
    parse_frametransformmtx(L);
parse_frame_1([{word, "FrameTransformMatrix"}, _, open | L]) ->
    parse_frametransformmtx(L);
parse_frame_1([{word, "Mesh"}, open | L]) ->
    parse_frame_1_mesh(L);
parse_frame_1([{word, "Mesh"}, {word, _MeshName}, open | L]) ->
    parse_frame_1_mesh(L);
parse_frame_1([{word, "Frame"}, {word, _Name}, open | L]) ->
    {ok, Mesh, L_1} = parse_frame_1_l(L),
    case L_1 of
        [close | L_2] ->
            {ok, {mesh, Mesh}, L_2}
    end;
parse_frame_1([{word, Unknown}, open | L]) ->
    parse_element_unknown(Unknown, none, L);
parse_frame_1([{word, Unknown}, {word, Name}, open | L]) ->
    parse_element_unknown(Unknown, Name, L).



parse_frame_1_mesh(L) ->
    {ok, VsList, L_1} = get_vs_list(L),
    {ok, FsList, L_2} = get_face_list(L_1),
    parse_frame_1_mesh_1(#x_mesh{vslist=VsList,fslist=FsList}, L_2).

parse_frame_1_mesh_1(Mesh, [{word, "MeshNormals"}, open|_]=L) ->
    parse_frame_1_mesh_1_normals(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, "MeshNormals"}, _, open|_]=L) ->
    parse_frame_1_mesh_1_normals(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, "MeshTextureCoords"}, open|_]=L) ->
    parse_frame_1_mesh_1_meshtexcoords(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, "MeshTextureCoords"}, _, open|_]=L) ->
    parse_frame_1_mesh_1_meshtexcoords(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, "MeshMaterialList"}, open|_]=L) ->
    parse_frame_1_mesh_1_meshmtllist(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, "MeshMaterialList"}, _, open|_]=L) ->
    parse_frame_1_mesh_1_meshmtllist(Mesh, L);
parse_frame_1_mesh_1(Mesh, [{word, _UnusedTemplate}, open | L]) ->
    [close | L_1] = parse_unused_section(L),
    parse_frame_1_mesh_1(Mesh, L_1);
parse_frame_1_mesh_1(Mesh, [{word, _UnusedTemplate}, _, open | L]) ->
    [close | L_1] = parse_unused_section(L),
    parse_frame_1_mesh_1(Mesh, L_1);
    
parse_frame_1_mesh_1(Mesh,[close | L]) ->
    {ok, {mesh, Mesh}, L}.


parse_frame_1_mesh_1_normals(Mesh, L) ->
    {ok, MeshNormals, L_1} = parse_mesh_normals(L),
    parse_frame_1_mesh_1(Mesh#x_mesh{normals=MeshNormals}, L_1).

parse_frame_1_mesh_1_meshtexcoords(Mesh, L) ->
    {ok, MeshTextureCoords, L_1} = parse_mesh_texture_coords(L),
    parse_frame_1_mesh_1(Mesh#x_mesh{texcoords=MeshTextureCoords}, L_1).


parse_frame_1_mesh_1_meshmtllist(Mesh, L) ->
    {ok, MeshMaterialList, L_1} = parse_mesh_material_list(L),
    parse_frame_1_mesh_1(Mesh#x_mesh{matlist=MeshMaterialList}, L_1).

parse_frametransformmtx(L) ->
    {ok, N11, N21, N31, N41, L_1} = get_numbers_d1d1d1d1(L),
    {ok, N12, N22, N32, N42, L_2} = get_numbers_d1d1d1d1(L_1),
    {ok, N13, N23, N33, N43, L_3} = get_numbers_d1d1d1d1(L_2),
    {ok, N14, N24, N34, N44, L_4} = get_numbers_d1d1d1d4(L_3),
    case L_4 of
        [close | L_5] ->
            {ok, {matrix, N11,N21,N31,N41,
                          N12,N22,N32,N42,
                          N13,N23,N33,N43,
                          N14,N24,N34,N44},
                 L_5}
    end.

parse_unused_section([open, {word, _}, close | L]) ->
    parse_unused_section(L);
parse_unused_section([{word, _}, open | L]) ->
    [close | L_1] = parse_unused_section(L),
    parse_unused_section(L_1);
parse_unused_section([{word, _}, {word, _}, open | L]) ->
    [close | L_1] = parse_unused_section(L),
    parse_unused_section(L_1);
parse_unused_section([close | _]=L) ->
    L;
parse_unused_section([_Unused | L]) ->
    parse_unused_section(L).



get_vs_list(L) ->
    {ok, VsCount, L_1} = get_number_d2(L),
    {ok, VsList, L_2} = get_vs_list_1(VsCount, L_1, []),
    {ok, VsList, L_2}.
get_vs_list_1(Count, L, O) when Count > 1 ->
    {ok, Num1, L_1} = get_number_d1_or_d2(L),
    {ok, Num2, L_2} = get_number_d1_or_d2(L_1),
    {ok, Num3, L_3} = get_number_d3(L_2),
    O_1 = [{Num1,Num2,Num3} | O],
    get_vs_list_1(Count-1, L_3, O_1);
get_vs_list_1(1, L, O) ->
    {ok, Num1, L_1} = get_number_d1_or_d2(L),
    {ok, Num2, L_2} = get_number_d1_or_d2(L_1),
    {ok, Num3, L_3} = get_number_d4(L_2),
    O_1 = [{Num1,Num2,Num3} | O],
    {ok, lists:reverse(O_1), L_3}.


get_face_list(L) ->
    {ok, FsCount, L_1} = get_number_d2(L),
    {ok, FsList, L_2} = get_face_list_1(FsCount, L_1, []),
    {ok, FsList, L_2}.
get_face_list_1(Count, [{word,IdxCount_S}, d2 | L], O)
  when Count > 0 ->
    {ok, IdxCount} = parse_num(IdxCount_S),
    {ok, List, L_1} = get_face_list_2(IdxCount, L, []),
    get_face_list_1(Count-1, L_1, [{face, List} | O]);
get_face_list_1(0, L, O) ->
    {ok, lists:reverse(O), L}.
get_face_list_2(1, [{word,Idx_S}, D | L], O)
  when D =:= d3 orelse D =:= d4 ->
    {ok, Idx} = parse_num(Idx_S),
    {ok, lists:reverse([Idx | O]), L};
get_face_list_2(Count, [{word,Idx_S}, d1 | L], O)
  when Count > 1 ->
    {ok, Idx} = parse_num(Idx_S),
    O_1 = [Idx | O],
    get_face_list_2(Count-1, L, O_1).

parse_mesh_normals([{word, "MeshNormals"}, open | L]) ->
    parse_mesh_normals_1(L);
parse_mesh_normals([{word, "MeshNormals"}, _, open | L]) ->
    parse_mesh_normals_1(L).

parse_mesh_normals_1(L) ->
    {ok, VsList, L_1} = get_vs_list(L),
    {ok, FsList, L_2} = get_face_list(L_1),
    case L_2 of
        [close | L_3] ->
            {ok, {VsList, FsList}, L_3}
    end.

parse_mesh_texture_coords([{word, "MeshTextureCoords"}, open | L]) ->
    parse_mesh_texture_coords_1(L);
parse_mesh_texture_coords([{word, "MeshTextureCoords"}, _, open | L]) ->
    parse_mesh_texture_coords_1(L).

parse_mesh_texture_coords_1(L) ->
    {ok, List, L_1} = get_uv_list(L),
    case L_1 of
        [close | L_2] ->
            {ok, List, L_2}
    end.


get_uv_list(L) ->
    {ok, VsCount, L_1} = get_number_d2(L),
    {ok, VsList, L_2} = get_uv_list_1(VsCount, L_1, []),
    {ok, VsList, L_2}.
get_uv_list_1(Count, L, O) when Count > 1 ->
    {ok, Num1, L_1} = get_number_d1_or_d2(L),
    {ok, Num2, L_2} = get_number_d3(L_1),
    O_1 = [{Num1,Num2} | O],
    get_uv_list_1(Count-1, L_2, O_1);
get_uv_list_1(1, L, O) ->
    {ok, Num1, L_1} = get_number_d1_or_d2(L),
    {ok, Num2, L_2} = get_number_d4(L_1),
    O_1 = [{Num1,Num2} | O],
    {ok, lists:reverse(O_1), L_2}.



parse_mesh_material_list([close | _]=L) ->
    {ok, none, L};
parse_mesh_material_list([{word, "MeshMaterialList"}, open | L]) ->
    {ok, _MtlCount, L_1} = get_number_d2(L),
    {ok, MtlIdxList, L_2} = get_idx_list(L_1),
    case L_2 of
        [d2 | L_3] -> L_3;  %% Empty semicolon
        L_3 -> L_3
    end,
    {ok, MtlNames, L_4} = get_material_in_list(L_3),
    case L_4 of
        [close | L_5] ->
            {ok, make_mtl_list(MtlIdxList, MtlNames), L_5}
    end.

make_mtl_list(Indices, MtlNames) ->
    ML_0 = [make_mtl_list_1(M) || M <- MtlNames],
    ML_1 = lists:filter(fun (Tuple) -> Tuple =/= unused end, ML_0),
    make_mtl_list(Indices, MtlNames, [], ML_1).
make_mtl_list([Idx|R], MtlNames, OL, ML) ->
    case lists:nth(Idx+1, MtlNames) of
        {with_mtl_spec, {Name, _}} -> ok;
        {name_only, Name} -> ok
    end,
    make_mtl_list(R, MtlNames, [Name|OL], ML);
make_mtl_list([], _MtlNames, OL, ML) ->
    {lists:reverse(OL), lists:reverse(ML)}.
make_mtl_list_1({with_mtl_spec, Tuple}) ->
    Tuple;
make_mtl_list_1({name_only, _}) ->
    unused.
    


get_material_in_list(L) ->
    get_material_in_list(L, []).
get_material_in_list([{word, "Material"}, _, open | _] = L, OL) ->
    {ok, Mat, L_1} = parse_element(L),
    get_material_in_list(L_1, [Mat|OL]);
get_material_in_list([open, {word, Name}, close | L], OL) ->
    NameAtm = material_atom(Name),
    get_material_in_list(L, [{name_only, NameAtm}|OL]);
get_material_in_list([close | _] = L, OL) ->
    {ok, lists:reverse(OL), L}.
    


get_idx_list(L) ->
    {ok, IdxCount, L_1} = get_number_d2(L),
    {ok, IdxList, L_2} = get_idx_list_1(IdxCount, L_1, []),
    {ok, IdxList, L_2}.
get_idx_list_1(Count, L, O) when Count > 1 ->
    {ok, Num, L_1} = get_number_d1_or_d2(L),
    get_idx_list_1(Count-1, L_1, [Num | O]);
get_idx_list_1(1, L, O) ->
    {ok, Num, L_1} = get_number_d1_or_d2(L),
    {ok, lists:reverse([Num | O]), L_1}.
    
%% Create wings3d material name from .x file material name
%%
material_atom(Name) ->
    list_to_atom(Name).


%% Split meshes from material definitions
%%
split_mesh_mtls(L) ->
    split_mesh_mtls(L, [], []).
split_mesh_mtls([unused|L], MeshL, MatL) ->
    split_mesh_mtls(L, MeshL, MatL);
split_mesh_mtls([#x_frame{mesh=Mesh_0}=_|L], MeshL, MatL)
  when Mesh_0 =/= none ->
    {Mesh_1, MtlTups} = split_mesh_mtls_1(Mesh_0),
    NewName = "obj_" ++ integer_to_list(abs(erlang:unique_integer())),
    MeshL_1 = [{NewName,Mesh_1} | MeshL],
    MatL_1 = lists:reverse(MtlTups) ++ MatL,
    split_mesh_mtls(L, MeshL_1, MatL_1);
split_mesh_mtls([{with_mtl_spec, MtlTup}=_|L], MeshL, MatL) ->
    MatL_1 = [MtlTup | MatL],
    split_mesh_mtls(L, MeshL, MatL_1);
split_mesh_mtls([#x_frame{mesh=none}=_|L], MeshL, MatL) ->
    split_mesh_mtls(L, MeshL, MatL);
split_mesh_mtls([], MeshL, MatL) ->
    {ok, {lists:reverse(MeshL), lists:reverse(MatL)}}.

split_mesh_mtls_1(#x_mesh{matlist=none}=Mesh) ->
    {Mesh, []};
split_mesh_mtls_1(#x_mesh{matlist={MatList, MSpecs}}=Mesh) ->
    {Mesh#x_mesh{matlist=MatList}, MSpecs}.


edge_pairs([E|_]=Fs) ->
    edge_pairs(Fs, E, []).
edge_pairs([E1|[E2|_]=Fs], E0, OL) ->
    edge_pairs(Fs, E0, [{E1,E2}|OL]);
edge_pairs([E1], E0, OL) ->
    lists:reverse([{E1,E0}|OL]).

all_edges(FL) ->
    lists:append([edge_pairs(F) || F <- FL]).

