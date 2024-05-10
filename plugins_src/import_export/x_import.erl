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
    txfile   :: [{atom(), string()}]
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
    DefHandedness = wpa:pref_get(?WPCX, handedness, lhand),
    Dialog = [
    ] ++ dialog_import() ++ [
        {hradio,[{?__(3,"Left-handed"),lhand},{?__(4,"Right-handed"),rhand}],DefHandedness,
            [{key,handedness},{title,?__(2,"Handedness")}]}
    ],
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
        Handedness = proplists:get_value(handedness, Attr, lhand),
        set_pref(Attr),
        FilenameExt = filename:extension(Filename),
        DirName = filename:dirname(Filename),
        _ObjName = filename:basename(Filename,FilenameExt),
        case import_x_file(Filename, Handedness) of
            {ok, {Meshes, Mats_0}} ->
                Objs = [import_mesh(MName,M) || {MName,M} <- Meshes],
                Mats = [import_mat(MName,M,DirName) || {MName,M} <- Mats_0],
                E3dfile = #e3d_file{objs=Objs,mat=Mats},
                {ok, import_transform(E3dfile, Attr)};
            {error, _}=Err ->
                Err
        end
    end.

import_mesh(ObjName, #x_mesh{vslist=Vs_0,fslist=Fs_0,normals=Ns,texcoords=TxC_0,matlist=ML}=_) ->
    Vs = [Coord || Coord <- Vs_0],
    case TxC_0 of
        none -> TxC = [];
        _    -> TxC = [ UV || UV <- TxC_0]
    end,
    TxA = array:from_list(TxC),
    case ML of
        none ->
            {Efs, NsC} = to_efs(Fs_0, Ns, TxA);
        _ when is_list(ML) ->
            {Efs, NsC} = to_efs_mtl(Fs_0, Ns, TxA, ML)
    end,
    Mesh = #e3d_mesh{
        type=polygon,
        vs=Vs,
        fs=Efs,
        tx=TxC,
        ns=NsC
    },
    Obj = #e3d_object{name=ObjName,obj=Mesh},
    Obj.

to_efs(Fs_0, {Ns_0,Fs_N0}, TxA) ->
    {NsC, Fs_1} = add_normals(Ns_0, Fs_N0, Fs_0),
    Efs = [ to_w_face(L,[],TxA,N) || {{face,L},{face,N}} <- Fs_1 ],
    {Efs, NsC};
to_efs(Fs_0, none, TxA) ->
    Efs = [ to_w_face(L,[],TxA) || {face,L} <- Fs_0 ],
    {Efs, []}.

to_efs_mtl(Fs_0, {Ns_0,Fs_N0}, TxA, ML) ->
    {NsC, Fs_1} = add_normals(Ns_0, Fs_N0, Fs_0),
    ML_1 = fill_in(ML, length(Fs_1)),
    Efs = [ to_w_face(L,[M],TxA,N) || {{{face,L},{face,N}},M} <- lists:zip(Fs_1, ML_1) ],
    {Efs, NsC};
to_efs_mtl(Fs_0, none, TxA, ML) ->
    ML_1 = fill_in(ML, length(Fs_0)),
    Efs = [ to_w_face(L,[M],TxA) || {{face,L},M} <- lists:zip(Fs_0, ML_1) ],
    {Efs, []}.

add_normals(Ns_0, Fs_N0, Fs_0) ->
    case Ns_0 of
        none -> NsC = [];
        _ ->
            NsC = [Coord || Coord <- Ns_0]
    end,
    Fs_N = fill_in(Fs_N0, length(Fs_0)),
    Fs_1 = lists:zip(Fs_0, Fs_N),
    {NsC, Fs_1}.
    

%% With normals
%%
to_w_face(L,[],_,N) ->
    #e3d_face{
        vs=L,
        ns=N,
        mat=[]
    };
to_w_face(L,Mat,TxA,N) ->
    #e3d_face{
        vs=L,
        tx=[case array:get(Idx, TxA) of
                undefined -> 0;
                _ -> Idx
            end || Idx <- L],
        ns=N,
        mat=Mat
    }.

%% Without normals
%%
to_w_face(L,[],_) ->
    #e3d_face{
        vs=L,
        mat=[]
    };
to_w_face(L,Mat,TxA) ->
    #e3d_face{
        vs=L,
        tx=[case array:get(Idx, TxA) of
                undefined -> 0;
                _ -> Idx
            end || Idx <- L],
        mat=Mat
    }.


fill_in(L, Amt) ->
    fill_in(L, Amt, []).
fill_in([A], Amt, OL) when Amt > 1 ->
    fill_in([A], Amt-1, [A|OL]);
fill_in([A|[_|_]=L], Amt, OL) when Amt > 1 ->
    fill_in(L, Amt-1, [A|OL]);
fill_in([A], 1, OL) ->
    fill_in([], 0, [A|OL]);
fill_in([], 0, OL) ->
    lists:reverse(OL).


import_mat(MatName, #x_mat{diffuse=DiffuseCol,power=Power,specular=SpecularCol,
                           emissive=EmissiveCol,txfile=TxL}=_, DirName) ->
    %% TODO: Add: [... || {bumpmap, _} <- TxL]
    case [A || {diffuse, A} <- TxL, A =/= ""] of
        [] ->
            Maps_L = [];
        [F | _] ->
            case import_mat_image(F, DirName) of
                {error, _} ->
                    io:format(?__(1,"NOTE: Image not found:") ++ " ~p~n", [F]),
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
    
import_mat_image(FilePath_0, DirName) ->
    FilePath_1 = filename:join(DirName, FilePath_0),
    case file:read_file_info(FilePath_1) of
        {ok, _} ->
            %% Something was found at the path specified
            FilePath_2 = FilePath_1;
        _ ->
            %% A file was not found, the file path might be an absolute
            %% path that does not exist on this machine, try with just the
            %% base name of of the file path relative to the .x file.
            %%
            FilePath_2 = filename:join(DirName, filename:basename(FilePath_0))
    end,
    case get_bitmap_by_ext(FilePath_2) of
        {ok, E3dImage} ->
            E3dImage;
        {error, _}=Error ->
            Error
    end.
    
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
            F = fun read_e3d_image/1
    end,
    F(FilePath).

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
read_e3d_image(FileName) ->
    E3D = e3d_image:load(FileName),
    {ok, E3D}.


%%%
%%% Read .x file
%%%

import_x_file(Filename, Handedness) ->
    case read_xof(Filename) of
        {ok, Cont} ->
            {ok, O} = xf(Cont, [], []),
            
            ets:new(?MODULE, [named_table,public,ordered_set]),
            Return = try 
                ets:insert(?MODULE, {handedness, Handedness}),
                {ok, O_1} = parse(O),
                {ok, {Meshes, Materials}} = split_mesh_mtls(O_1),
                {ok, {Meshes, Materials}}
            catch _:Err:ST ->
                io:format(?__(2,".x Import Error:") ++ " ~P~nstack trace: ~p~n", [Err,ST]),
                {error, ?__(1,".x Import Error")}
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
    parse_1(L, []).
parse_1([_|_]=L, O) ->
    {ok, Elem, L_1} = parse_element(L),
    parse_1(L_1, [Elem | O]);
parse_1([], O) ->
    {ok, lists:reverse(O)}.
    
-define(UNUSED_ELEMENT(A), (
    A =:= "Header" orelse
    A =:= "AnimTicksPerSecond"
)).

parse_element([{word, "Material"}, {word, Name}, open | L]) ->
    {ok, NewMat, L_4} = parse_element_material(L),
    case L_4 of
        [close | L_5] ->
            NameAtm = material_atom(Name),
            {ok, {with_mtl_spec, {NameAtm, NewMat}}, L_5}
    end;
parse_element([{word, "Material"}, open | L]) ->
    {ok, NewMat, L_4} = parse_element_material(L),
    case L_4 of
        [close | L_5] ->
            Name = "material" ++ integer_to_list(abs(erlang:unique_integer())),
            NameAtm = material_atom(Name),
            {ok, {with_mtl_spec, {NameAtm, NewMat}}, L_5}
    end;
parse_element([{word, "Frame"}, {word, Name}, open | L]) ->
    {ok, Mesh, L_1} = parse_frame_1_l(L),
    case L_1 of
        [close | L_2] ->
            {ok, #x_frame{name=Name,mesh=Mesh}, L_2}
    end;
parse_element([{word, "Frame"}, open | L]) ->
    {ok, Mesh, L_1} = parse_frame_1_l(L),
    case L_1 of
        [close | L_2] ->
            Name = "obj" ++ integer_to_list(abs(erlang:unique_integer())),
            {ok, #x_frame{name=Name,mesh=Mesh}, L_2}
    end;
parse_element([{word, "Mesh"}, open | L]) ->
    {ok, {mesh, Mesh}, L_1} = parse_frame_1_mesh(L),
    Name = "obj" ++ integer_to_list(abs(erlang:unique_integer())),
    {ok, #x_frame{name=Name,mesh=Mesh}, L_1};
parse_element([{word, "Mesh"}, {word, Name}, open | L]) ->
    {ok, {mesh, Mesh}, L_1} = parse_frame_1_mesh(L),
    {ok, #x_frame{name=Name,mesh=Mesh}, L_1};
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
parse_element([{word, Unused}, open | L])
  when ?UNUSED_ELEMENT(Unused) ->
    parse_unused_element(L);
parse_element([{word, Unused}, {word, _}, open | L])
  when ?UNUSED_ELEMENT(Unused) ->
    parse_unused_element(L);
parse_element([{word, Unknown}, open | L]) ->
    parse_element_unknown(Unknown, none, L);
parse_element([{word, Unknown}, {word, Name}, open | L]) ->
    parse_element_unknown(Unknown, Name, L).


parse_element_material(L) ->
    {ok, Num4A, L_1} = get_4_numbers(L),
    {ok, Num4B, L_2} = get_4_numbers(L_1),
    {ok, Num3C, L_3} = get_3_numbers(L_2),
    parse_material_1(L_3, [{diffuse, Num4A}, {power_specular, Num4B}, {emissive, Num3C}]).


parse_unused_element(L) ->
    L_1 = parse_unused_section(L),
    case L_1 of
        [close | L_2] ->
            {ok, unused, L_2}
    end.

parse_element_unknown(TemplateName, Name, L) ->
    case ets:lookup(?MODULE, {template, TemplateName}) of
        [] ->
            error({unexpected, {TemplateName, Name}});
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


-define(TEXTURE_ELEMENT_NAME(A), (
    TxSection =:= "TextureFilename" orelse
    TxSection =:= "TextureFileName"
)).

parse_material_1(L, Props) ->
    parse_material_1(L, Props, []).
parse_material_1([{word, TxSection}, open | L], Props, TxL)
  when ?TEXTURE_ELEMENT_NAME(TxSection) ->
    case get_string(L) of
        {ok, Str, [close | L_1]} ->
            parse_material_1(L_1, Props, [{diffuse, Str} | TxL])
    end;
parse_material_1([{word, TxSection}, {word, _MapType}, open | L], Props, TxL)
  when ?TEXTURE_ELEMENT_NAME(TxSection) ->
    %% Note: _MapType=:="Diffuse" seen in a .x file
    case get_string(L) of
        {ok, Str, [close | L_1]} ->
            parse_material_1(L_1, Props, [{diffuse, Str} | TxL])
    end;
parse_material_1([{word, "BumpMapFilename"}, open | L], Props, TxL) ->
    case get_string(L) of
        {ok, Str, [close | L_1]} ->
            parse_material_1(L_1, Props, [{bumpmap, Str} | TxL])
    end;
parse_material_1([close | _]=L, Props, TxL) ->
    {RD, GD, BD, AD} = proplists:get_value(diffuse, Props, [0.7, 0.7, 0.7, 1.0]),
    {Power, RS, GS, BS} = proplists:get_value(power_specular, Props, [0.0, 0.0, 0.0, 0.0]),
    {RE, GE, BE} = proplists:get_value(emissive, Props, [0.0, 0.0, 0.0]),
    FileNames = lists:reverse(TxL),
    DiffuseCol = {float(RD), float(GD), float(BD), float(AD)},
    SpecularCol = {float(RS), float(GS), float(BS), 1.0},
    EmissiveCol = {float(RE), float(GE), float(BE), 1.0},
    NewMat = #x_mat{diffuse=DiffuseCol,power=float(Power),
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


get_number_d1_or_d2_or_d4([{word, Num_S}, Delimiter | L])
  when Delimiter =:= d1; Delimiter =:= d2; Delimiter =:= d4 ->
    {ok, Num} = parse_num(Num_S),
    {ok, Num, L}.


get_number_d1_or_d2([{word, Num_S}, Delimiter | L])
  when Delimiter =:= d1; Delimiter =:= d2->
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
    
parse_frame_1([open, {word, _Name}, close | L]) ->
    {ok, unused, L};
parse_frame_1([{word, "FrameTransformMatrix"}, open | L]) ->
    parse_frametransformmtx(L);
parse_frame_1([{word, "FrameTransformMatrix"}, _, open | L]) ->
    parse_frametransformmtx(L);
parse_frame_1([{word, "Mesh"}, open | L]) ->
    parse_frame_1_mesh(L);
parse_frame_1([{word, "Mesh"}, {word, _MeshName}, open | L]) ->
    parse_frame_1_mesh(L);
parse_frame_1([{word, "Frame"}, {word, _Name}, open | L]) ->
    parse_frame_1_frame(L);
parse_frame_1([{word, "Frame"}, open | L]) ->
    parse_frame_1_frame(L);
parse_frame_1([{word, Unknown}, open | L]) ->
    parse_element_unknown(Unknown, none, L);
parse_frame_1([{word, Unknown}, {word, Name}, open | L]) ->
    parse_element_unknown(Unknown, Name, L).


parse_frame_1_frame(L) ->
    case parse_frame_1_l(L) of
        {ok, #x_mesh{}=Mesh, L_1} ->
            Mesh_1 = {mesh, Mesh};
        {ok, _, L_1} ->
            Mesh_1 = none
    end,
    case L_1 of
        [close | L_2] ->
            {ok, Mesh_1, L_2}
    end.


import_handedness() ->
    case ets:lookup(?MODULE, handedness) of
        [] ->
            lhand;
        [{_, Handedness}] ->
            Handedness
    end.


%% Set to right handed for wings
%%
set_handedness(lhand, VsList, FsList) ->
    VsList_1 = lists:map(fun ({X,Y,Z}) -> {float(X),float(Y),-float(Z)} end, VsList),
    FsList_1 = [{face, lists:reverse(F)} || {face, F} <- FsList],
    {VsList_1, FsList_1};
set_handedness(rhand, VsList, FsList) ->
    VsList_1 = lists:map(fun ({X,Y,Z}) -> {float(X),float(Y),float(Z)} end, VsList),
    FsList_1 = FsList,
    {VsList_1, FsList_1}.

set_handedness_uv(lhand, List_0) ->
    lists:map(fun({U,V}) -> {float(U), 1.0 - float(V)} end, List_0);
set_handedness_uv(rhand, List_0) ->
    lists:map(fun({U,V}) -> {float(U), 1.0 - float(V)} end, List_0).


parse_frame_1_mesh(L) ->
    {ok, VsList, L_1} = get_vs_list(L),
    {ok, FsList, L_2} = get_face_list(L_1),
    Handedness = import_handedness(),
    {VsList_1, FsList_1} = set_handedness(Handedness, VsList, FsList),
    parse_frame_1_mesh_1(#x_mesh{vslist=VsList_1,fslist=FsList_1}, L_2).

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
get_face_list_1(Count, [{word,IdxCount_S}, D | L], O)
  when Count > 0, (D =:= d2 orelse D =:= d1) ->
    %% Usually d2 but d1 seen in a file
    {ok, IdxCount} = parse_num(IdxCount_S),
    {ok, List, L_1} = get_face_list_2(IdxCount, L, []),
    get_face_list_1(Count-1, L_1, [{face, List} | O]);
get_face_list_1(0, L, O) ->
    {ok, lists:reverse(O), L}.
get_face_list_2(1, [{word,Idx_S}, D | L], O)
  when D =:= d3 orelse D =:= d4 ->
    {ok, Idx} = parse_num(Idx_S),
    {ok, lists:reverse([Idx | O]), L};
get_face_list_2(Count, [{word,Idx_S}, D | L], O)
  when Count > 1, D =:= d1 orelse D =:= d2 -> %% d1 is usual, d2 is more rare
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
    Handedness = import_handedness(),
    {VsList_1, FsList_1} = set_handedness(Handedness, VsList, FsList),
    case L_2 of
        [close | L_3] ->
            {ok, {VsList_1, FsList_1}, L_3}
    end.

parse_mesh_texture_coords([{word, "MeshTextureCoords"}, open | L]) ->
    parse_mesh_texture_coords_1(L);
parse_mesh_texture_coords([{word, "MeshTextureCoords"}, _, open | L]) ->
    parse_mesh_texture_coords_1(L).

parse_mesh_texture_coords_1(L) ->
    {ok, List_0, L_1} = get_uv_list(L),
    %% Invert V
    Handedness = import_handedness(),
    List = set_handedness_uv(Handedness, List_0),
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
    parse_mesh_material_list_1(L);
parse_mesh_material_list([{word, "MeshMaterialList"}, {word, _}, open | L]) ->
    parse_mesh_material_list_1(L).

parse_mesh_material_list_1(L) ->
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
get_material_in_list([{word, "Material"}, {word, _}, open | _] = L, OL) ->
    {ok, Mat, L_1} = parse_element(L),
    get_material_in_list(L_1, [Mat|OL]);
get_material_in_list([{word, "Material"}, open | _] = L, OL) ->
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
    %% A lot of variation for the last delimiter.
    {ok, Num, L_1} = get_number_d1_or_d2_or_d4(L),
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


%% Read the .x file, and return the content to the tokenizer
%%
read_xof(Filename) ->
    case file:read_file(Filename) of
        {ok, <<"xof ", _Ver:4/binary-unit:8, Sig2:8/binary-unit:8, R/binary>>} ->
            xof(R, Sig2);
        Error ->
            Error
    end.


%% Determine if the file is ascii or binary, and if it is compressed or
%% uncompressed
%%
xof(R, <<"bzip", SigHB:4/binary-unit:8>>) ->
    {ok, bintok(mz(R), binhdr_from_sig(SigHB))};
xof(R, <<"bin ", SigHB:4/binary-unit:8>>) ->
    {ok, bintok(R, binhdr_from_sig(SigHB))};
xof(R, <<"tzip", _:4/binary-unit:8>>) ->
    {ok, mz(R)};
xof(R, <<"txt ", _:4/binary-unit:8>>) ->
    {ok, R}.


%% Binary and compressed x files
%%

-record(delstate, {
    mode = {none,0,0,0},
    stack = []
}).

-record(binhdr, {
    floatsize = 4,  %% 4 or 8
    delstate = #delstate{}
}).


%% Only used when the file is binary
%%    
binhdr_from_sig(<<"0032">>) ->
    #binhdr{floatsize=4};
binhdr_from_sig(<<"0064">>) ->
    #binhdr{floatsize=8}.

-define(UINT, little-unsigned-integer).

-define(B_NAME, 1).
-define(B_STRING, 2).
-define(B_INTEGER, 3).
-define(B_GUID, 5).
-define(B_INTEGER_LIST, 6).
-define(B_FLOAT_LIST, 7).
-define(B_OBRACE, 10).
-define(B_CBRACE, 11).
-define(B_OPAREN, 12).
-define(B_CPAREN, 13).
-define(B_OBRACKET, 14).
-define(B_CBRACKET, 15).
-define(B_OANGLE, 16).
-define(B_CANGLE, 17).
-define(B_DOT, 18).
-define(B_COMMA, 19).
-define(B_SEMICOLON, 20).

-define(B_TMPL_KW_TEMPLATE, 31).
-define(B_TMPL_KW_WORD, 40).
-define(B_TMPL_KW_DWORD, 41).
-define(B_TMPL_KW_FLOAT, 42).
-define(B_TMPL_KW_DOUBLE, 43).
-define(B_TMPL_KW_CHAR, 44).
-define(B_TMPL_KW_UCHAR, 45).
-define(B_TMPL_KW_SWORD, 46).
-define(B_TMPL_KW_SDWORD, 47).
-define(B_TMPL_KW_VOID, 48).
-define(B_TMPL_KW_LPSTR, 49).
-define(B_TMPL_KW_UNICODE, 50).
-define(B_TMPL_KW_CSTRING, 51).
-define(B_TMPL_KW_ARRAY, 52).

bintok(Bin, BinHdr) ->
    bintok(Bin, BinHdr, []).

bintok(<<?B_NAME:16/?UINT, Count:32/?UINT, R1/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    Name = binary:part(R1, {0, Count}),
    {DStt1, _} = bintok_d(DStt, {name, Name}),
    Rest = binary:part(R1, {Count, byte_size(R1)-Count}),
    String1 = iolist_to_binary(io_lib:format("~s ", [Name])),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [String1 | OL]);
bintok(<<?B_STRING:16/?UINT, Count:32/?UINT, R1/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    String = binary:part(R1, {0, Count}),
    {DStt1, _} = bintok_d(DStt, {string, String}),
    <<Term:16/?UINT, Rest/binary>> = binary:part(R1, {Count, byte_size(R1)-Count}),
    String1 = iolist_to_binary(io_lib:format("\"~s\"", [String])),
    case Term of
        ?B_COMMA ->
            TermTok = <<", ">>;
        ?B_SEMICOLON ->
            TermTok = <<"; ">>
    end,
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [TermTok, String1 | OL]);
bintok(<<?B_INTEGER:16/?UINT, Value:32/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    ValB = iolist_to_binary(io_lib:format("~p ", [Value])),
    {DStt1, _} = bintok_d(DStt, ValB),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [ValB | OL]);
bintok(<<?B_GUID:16/?UINT,
         G1:4/binary-unit:8,
         G2:2/binary-unit:8,
         G3:2/binary-unit:8,
         G4:8/binary-unit:8,
         Rest/binary>>, BinHdr, OL) ->
    GUID = guid_string(G1,G2,G3,G4),
    bintok(Rest, BinHdr, [GUID | OL]);

bintok(<<?B_INTEGER_LIST:16/?UINT, Count:32/?UINT, R1/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {Vals, DStt1, Rest} = bintok_int(R1, DStt, Count, []),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [Vals|OL]);
bintok(<<?B_FLOAT_LIST:16/?UINT, Count:32/?UINT, R1/binary>>, #binhdr{floatsize=FLSize,delstate=DStt}=BinHdr, OL) ->
    case FLSize of
        4 ->
            {Vals, DStt1, Rest} = bintok_fl4(R1, DStt, Count, []),
            bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [Vals|OL]);
        8 ->
            {Vals, DStt1, Rest} = bintok_fl8(R1, DStt, Count, []),
            bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [Vals|OL])
    end;
bintok(<<?B_OBRACE:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, obrace),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<"{">>|OL]);
bintok(<<?B_CBRACE:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, cbrace),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<"}">>|OL]);
bintok(<<?B_OPAREN:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, oparen),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<"(">>|OL]);
bintok(<<?B_CPAREN:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, cparen),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<")">>|OL]);
bintok(<<?B_OBRACKET:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, obracket),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<"[">>|OL]);
bintok(<<?B_CBRACKET:16/?UINT, Rest/binary>>, #binhdr{delstate=DStt}=BinHdr, OL) ->
    {DStt1, _} = bintok_d(DStt, cbracket),
    bintok(Rest, BinHdr#binhdr{delstate=DStt1}, [<<"]">>|OL]);
bintok(<<?B_OANGLE:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"<">>|OL]);
bintok(<<?B_CANGLE:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<">">>|OL]);
bintok(<<?B_DOT:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<".">>|OL]);
bintok(<<?B_COMMA:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<",">>|OL]);
bintok(<<?B_SEMICOLON:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<";">>|OL]);

bintok(<<?B_TMPL_KW_TEMPLATE:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"template ">>|OL]);
bintok(<<?B_TMPL_KW_WORD:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"WORD ">>|OL]);
bintok(<<?B_TMPL_KW_DWORD:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"DWORD ">>|OL]);
bintok(<<?B_TMPL_KW_FLOAT:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"FLOAT ">>|OL]);
bintok(<<?B_TMPL_KW_DOUBLE:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"DOUBLE ">>|OL]);
bintok(<<?B_TMPL_KW_CHAR:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"CHAR ">>|OL]);
bintok(<<?B_TMPL_KW_UCHAR:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"UCHAR ">>|OL]);
bintok(<<?B_TMPL_KW_SWORD:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"SWORD ">>|OL]);
bintok(<<?B_TMPL_KW_SDWORD:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"SDWORD ">>|OL]);
bintok(<<?B_TMPL_KW_VOID:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"VOID ">>|OL]);
bintok(<<?B_TMPL_KW_LPSTR:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"LPSTR ">>|OL]);
bintok(<<?B_TMPL_KW_UNICODE:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"UNICODE ">>|OL]);
bintok(<<?B_TMPL_KW_CSTRING:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"CSTRING ">>|OL]);
bintok(<<?B_TMPL_KW_ARRAY:16/?UINT, Rest/binary>>, BinHdr, OL) ->
    bintok(Rest, BinHdr, [<<"ARRAY ">>|OL]);
bintok(<<>>, _BinHdr, OL) ->
    iolist_to_binary(lists:reverse(OL)).

bintok_int(<<Val:32/?UINT, R/binary>>, DStt, Count, OL)
  when Count > 0 ->
    {DStt1, DT} = bintok_d(DStt, Val),
    ValB = iolist_to_binary(io_lib:format("~p~s", [Val, DT])),
    bintok_int(R, DStt1, Count-1, [ValB|OL]);
bintok_int(Rest, DStt1, 0, OL) ->
    {lists:reverse( OL ), DStt1, Rest}.

bintok_fl4(<<Val:32/little-float, R/binary>>, DStt, Count, OL)
  when Count > 0 ->
    {DStt1, DT} = bintok_d(DStt, Val),
    ValB = iolist_to_binary(io_lib:format("~p~s", [Val, DT])),
    bintok_fl4(R, DStt1, Count-1, [ValB|OL]);
bintok_fl4(Rest, DStt1, 0, OL) ->
    {lists:reverse( OL ), DStt1, Rest}.

bintok_fl8(<<Val:64/little-float, R/binary>>, DStt, Count, OL)
  when Count > 0 ->
    {DStt1, DT} = bintok_d(DStt, Val),
    ValB = iolist_to_binary(io_lib:format("~p~s", [Val, DT])),
    bintok_fl8(R, DStt1, Count-1, [ValB|OL]);
bintok_fl8(Rest, DStt1, 0, OL) ->
    {lists:reverse( OL ), DStt1, Rest}.

guid_string(G1,G2,G3,<<G4:2/binary-unit:8,G5:6/binary-unit:8>>) ->
    iolist_to_binary(io_lib:format("<~s-~s-~s-~s-~s> ", [
        to_hex_intl(G1),
        to_hex_intl(G2),
        to_hex_intl(G3),
        to_hex_bin(G4),
        to_hex_bin(G5)])).

to_hex_intl(<<A1,A2,A3,A4>>) ->
    [to_hex(A4),
     to_hex(A3),
     to_hex(A2),
     to_hex(A1)];
to_hex_intl(<<A1,A2>>) ->
    [to_hex(A2),
     to_hex(A1)].
to_hex_bin(AB) ->
    AB1 = binary_to_list(AB),
    [to_hex(A) || A <- AB1].

to_hex(A) ->
    A1 = (A band 16#f0) bsr 4,
    A2 =  A band 16#0f,
    [to_hex_1(A1), to_hex_1(A2)].
to_hex_1(A)
  when A >= 0 andalso A =< 9 ->
    A + $0;
to_hex_1(A)
  when A >= 10 ->
    A - 10 + $a.



%% Try to determine the right delimiters to use next
%%
bintok_d(DStt, {name, <<"Mesh">>}) ->
    {DStt#delstate{mode={mesh,0}}, ";"};
bintok_d(DStt, {name, <<"MeshNormals">>}) ->
    {DStt#delstate{mode={mesh,0}}, ";"};
bintok_d(DStt, {name, <<"MeshTextureCoords">>}) ->
    {DStt#delstate{mode={meshtxc,0}}, ";"};
bintok_d(DStt, {name, <<"Material">>}) ->
    {DStt#delstate{mode={mtl,{1,0}}}, ";"};
bintok_d(DStt, {name, <<"MeshMaterialList">>}) ->
    {DStt#delstate{mode={meshmtl,0}}, ";"};
bintok_d(DStt, {name, <<"FrameTransformMatrix">>}) ->
    {DStt#delstate{mode={frmtmtx,0}}, ";"};

%% Mesh, MeshNormals
%%
bintok_d(#delstate{mode={mesh,0}}=DStt, Count)
  when is_integer(Count) ->
    {DStt#delstate{mode={mesh,{1,Count,0}}}, ";"};
bintok_d(#delstate{mode={mesh,{1,Count,A}}}=DStt, Coord)
  when is_float(Coord) orelse is_integer(Coord), A < 2, Count > 0 ->
    {DStt#delstate{mode={mesh,{1,Count,A+1}}}, ";"};
bintok_d(#delstate{mode={mesh,{1,Count,A}}}=DStt, Coord)
  when is_float(Coord) orelse is_integer(Coord), A =:= 2, Count > 1 ->
    {DStt#delstate{mode={mesh,{1,Count-1,0}}}, ";,"};
bintok_d(#delstate{mode={mesh,{1,Count,A}}}=DStt, Coord)
  when is_float(Coord) orelse is_integer(Coord), A =:= 2, Count =:= 1 ->
    {DStt#delstate{mode={mesh,{1,Count-1,0}}}, ";;"};

bintok_d(#delstate{mode={mesh,{1,0,0}}}=DStt, Count)
  when is_integer(Count) ->
    {DStt#delstate{mode={mesh,{2,Count,-1}}}, ";"};
bintok_d(#delstate{mode={mesh,{2,1,1}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={mesh,{2,1,1}}}, ";;"};
bintok_d(#delstate{mode={mesh,{2,Count,-1}}}=DStt, Count2)
  when is_integer(Count2) ->
    {DStt#delstate{mode={mesh,{2,Count,Count2}}}, ";"};
bintok_d(#delstate{mode={mesh,{2,Count,1}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={mesh,{2,Count-1,-1}}}, ";,"};
bintok_d(#delstate{mode={mesh,{2,Count,Count2}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={mesh,{2,Count,Count2-1}}}, ","};

bintok_d(#delstate{mode={mesh,{2,Count,0}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={mesh,{2,Count,0}}}, ";"};

%% MeshTextureCoords
%% 
bintok_d(#delstate{mode={meshtxc,0}}=DStt, Count)
  when is_integer(Count) ->
    {DStt#delstate{mode={meshtxc,{1,Count,0}}}, ";"};

bintok_d(#delstate{mode={meshtxc,{1,Count,A}}}=DStt, Coord)
  when is_float(Coord), A < 1, Count > 0 ->
    {DStt#delstate{mode={meshtxc,{1,Count,A+1}}}, ";"};

bintok_d(#delstate{mode={meshtxc,{1,1,1}}}=DStt, Coord)
  when is_float(Coord) ->
    {DStt#delstate{mode={meshtxc,{1,1,0}}}, ";;"};
bintok_d(#delstate{mode={meshtxc,{1,Count,1}}}=DStt, Coord)
  when is_float(Coord), Count > 0 ->
    {DStt#delstate{mode={meshtxc,{1,Count-1,0}}}, ";,"};

%% Material
%% 
bintok_d(#delstate{mode={mtl,{1,A}}}=DStt, Coord)
  when is_float(Coord), A < 3 ->
    {DStt#delstate{mode={mtl,{1,A+1}}}, ";"};
bintok_d(#delstate{mode={mtl,{1,A}}}=DStt, Coord)
  when is_float(Coord), A =:= 3 ->
    {DStt#delstate{mode={mtl,{2,0}}}, ";;"};

bintok_d(#delstate{mode={mtl,{2,A}}}=DStt, Coord)
  when is_float(Coord), A < 3 ->
    {DStt#delstate{mode={mtl,{2,A+1}}}, ";"};
bintok_d(#delstate{mode={mtl,{2,A}}}=DStt, Coord)
  when is_float(Coord), A =:= 3 ->
    {DStt#delstate{mode={mtl,{3,0}}}, ";;"};

bintok_d(#delstate{mode={mtl,{3,A}}}=DStt, Coord)
  when is_float(Coord), A < 2 ->
    {DStt#delstate{mode={mtl,{3,A+1}}}, ";"};
bintok_d(#delstate{mode={mtl,{3,A}}}=DStt, Coord)
  when is_float(Coord), A =:= 2 ->
    {DStt#delstate{mode={mtl,{3,0}}}, ";;"};

%% MeshMaterialList
%% 
bintok_d(#delstate{mode={meshmtl,0}}=DStt, Count1)
  when is_integer(Count1) ->
    {DStt#delstate{mode={meshmtl,{1,Count1,-1}}}, ";"};
bintok_d(#delstate{mode={meshmtl,{1,Count1,-1}}}=DStt, Count2)
  when is_integer(Count2) ->
    {DStt#delstate{mode={meshmtl,{1,Count1,Count2}}}, ";"};
bintok_d(#delstate{mode={meshmtl,{1,Count1,1}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={meshmtl,{1,Count1-1,-1}}}, ";"};
bintok_d(#delstate{mode={meshmtl,{1,Count1,Count2}}}=DStt, Val)
  when is_integer(Val) ->
    {DStt#delstate{mode={meshmtl,{1,Count1,Count2-1}}}, ","};

%% FrameTransformMatrix
%% 
bintok_d(#delstate{mode={frmtmtx,15}}=DStt, Val)
  when is_float(Val) orelse is_integer(Val) ->
    {DStt#delstate{mode={frmtmtx,15}}, ";;"};
bintok_d(#delstate{mode={frmtmtx,Count}}=DStt, Val)
  when is_float(Val) orelse is_integer(Val), Count < 15 ->
    {DStt#delstate{mode={frmtmtx,Count+1}}, ","};

%% Default for other sections
%% 
bintok_d(DStt, _) ->
    {DStt, ";"}.


%% "mszip" compression information:
%% http://justsolve.archiveteam.org/wiki/MSZIP
%% https://stackoverflow.com/questions/39390314/deflating-data-from-mszip-format
%%
mz(<<TotalSize:32/?UINT, Rest/binary>>) ->
    Z = zlib:open(),
    zlib:inflateInit(Z, -15),
    Ret = mz_b(Rest, Z, []),
    ok = zlib:inflateEnd(Z),
    zlib:close(Z),
    BinCont = iolist_to_binary(Ret),
    ActualTotalSize = byte_size(BinCont) + 16,
    case ActualTotalSize =:= TotalSize of
        true ->
            ok;
        false ->
            %% Something might be wrong with this file, let the user know.
            io:format("~p: " ++
                ?__(1, "NOTE: Unexpected difference:~n"
                       "Given total uncompressed size: ~p~nActual total size: ~p~n~n"),
                    [?MODULE, TotalSize, ActualTotalSize])
    end,
    BinCont.
mz_b(<<>>, _, OL) ->
    lists:reverse(OL);
mz_b(<<UncSize:16/?UINT, FlateSize:16/?UINT, "CK", XComp/binary>>, Z, OL) ->
    FS = FlateSize - 2,
    Block = binary:part(XComp, {0, FS}),
    Rest = binary:part(XComp, {FS, byte_size(XComp)-FS}),
    IOList = zlib:inflate(Z, Block),
    Data = iolist_to_binary(IOList),
    ActualSize = byte_size(Data),
    case UncSize =:= ActualSize of
        true ->
            mz_b_2(Rest, Z, [Data|OL]);
        false ->
            %% Something might be wrong with this file, let the user know.
            io:format("~p: " ++
                ?__(1,"NOTE: Unexpected difference between the given uncompressed size: ~p~n"
                      "and actual decoded block size: ~p~n~n"),
                    [?MODULE, UncSize, ActualSize]),
            mz_b_2(Rest, Z, [Data|OL])
    end.

%% After completing a compressed block, the zlib inflater needs
%% to be reset and given the previous uncompressed block to set
%% the dictionary.
%%     
mz_b_2(<<>>, _, OL) ->
    lists:reverse(OL);
mz_b_2(Rest, Z, [Prev|_]=OL) ->
    ok = zlib:inflateReset(Z),
    ok = zlib:inflateSetDictionary(Z, Prev),
    mz_b(Rest, Z, OL).


