%%
%%  fbx_p_import.erl --
%%
%%     FBX file import part of the wpc_fbx_p plugin.
%%
%%  Copyright (c) 2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(fbx_p_import).
-export([do_import/2]).
-export([t_bin/0]).
-export([trav/1]).
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

-define(WPC_FBX, wpc_fbx_p).

%%%
%%% Import.
%%%

do_import(Ask, _St) when is_atom(Ask) ->
    Dialog = [
    ] ++ common_mesh_options(import),
    wpa:dialog(Ask, ?__(1,"FBX Import Options"), Dialog,
           fun(Res) ->
               {file,{import,{fbx_model,Res}}}
           end);
do_import(Attr, St) ->
    wpa:import(props(), import_fun(Attr), St).

set_pref(KeyVals) ->
    wpa:pref_set(?WPC_FBX, KeyVals).

props() ->
    [{ext,".fbx"},{ext_desc,?__(1,"FBX Model File")}].

common_mesh_options(Type) ->
    [wpa:dialog_template(?WPC_FBX, Type, [include_colors])].

import_transform(E3dFile, KeyVals) ->
    Mat = wpa:import_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).


import_fun(Attr) ->
    fun(Filename) ->
        set_pref(Attr),
        FilenameExt = filename:extension(Filename),
        FBXPath = filename:dirname(Filename),
        _ObjName = filename:basename(Filename,FilenameExt),
        
        {Models,MatL} = read_fbx_to_models(Filename),
        Objs = lists:append([from_fbx_model_to_obj(M) || M <- Models]),
        Mats = [from_fbx_material(Mt, FBXPath) || Mt <- MatL],
        
        {ok, import_transform(#e3d_file{objs=Objs,mat=Mats}, Attr)}
    end.
    
%% FBX models seem to be normally 0 ... 100.0
-define(FBX_SCALE, 0.01).

rescale_model_import(Vertices0, L) ->
    case proplists:get_value(prop, L, []) of
        Props ->
            {lclscaling,XScl,YScl,ZScl} =
                proplists:get_value(<<"Lcl Scaling">>, Props,
                    {lclscaling,100.0,100.0,100.0}),
            Vertices = 
                [{X*XScl*?FBX_SCALE,Y*YScl*?FBX_SCALE,Z*ZScl*?FBX_SCALE}
                  || {X,Y,Z} <- Vertices0],
            Vertices
    end.

reposition_model_import(Vertices0, L) ->
    case proplists:get_value(prop, L, []) of
        Props ->
            {lcltranslation,XTrl,YTrl,ZTrl} =
                proplists:get_value(<<"Lcl Translation">>, Props,
                    {lcltranslation,0.0,0.0,0.0}),
            Vertices = 
                [{float(X+XTrl),float(Y+YTrl),float(Z+ZTrl)}
                  || {X,Y,Z} <- Vertices0],
            Vertices
    end.

rotate_model_import(Vertices0, L) ->
    case proplists:get_value(prop, L, []) of
        Props ->
            {lclrotation,XAng,YAng,ZAng} =
                proplists:get_value(<<"Lcl Rotation">>, Props,
                    {lclrotation,0.0,0.0,0.0}),
            Mat = e3d_mat:mul(
                e3d_mat:mul(
                    rotate_mat(1.0, 0.0, 0.0, XAng),
                    rotate_mat(0.0, 1.0, 0.0, YAng)),
                rotate_mat(0.0, 0.0, 1.0, ZAng)),
            Vertices = 
                [e3d_mat:mul_point(Mat, VPos)
                  || VPos <- Vertices0],
            Vertices
    end.

rotate_mat(XA, YA, ZA, Ang) ->
    e3d_mat:rotate(float(Ang), {float(XA), float(YA), float(ZA)}).


from_fbx_model_to_obj({{model,ObjAtomName},L}) ->
    MatNames = lists:reverse([Mt || {{mat,Mt},mat} <- L]), %% Order of materials are reversed
    case proplists:get_value(geom, L, none) of
        none ->
            [];
        L1 ->
            Vertices_0 = proplists:get_value(vertices, L1, []),
            Vertices_1 = rescale_model_import(Vertices_0, L),
            Vertices_2 = reposition_model_import(Vertices_1, L),
            Vertices = rotate_model_import(Vertices_2, L),
            VtxList = proplists:get_value(vtxindex, L1, []),
            Efs_0 = [
                    #e3d_face{ vs=IdxL, mat=[default] }
                    || IdxL <- VtxList ],
            {Efs_1, Tx} = set_uv_fs_list(Efs_0, Vertices, L1),
            {Efs_2, Ns} = set_ns_fs_list(Efs_1, Vertices, L1),
            HEs = set_he_list(VtxList, L1),
            Efs = set_mtl_fs_list(Efs_2, L1, MatNames),
            Mesh = #e3d_mesh{
                type=polygon,
                vs=Vertices,
                fs=Efs,
                tx=Tx,
                ns=Ns,
                he=HEs
            },
            [#e3d_object{name=atom_to_list(ObjAtomName),obj=Mesh}]
    end;
from_fbx_model_to_obj(_) ->
    [].

%% Set material to e3d_face
%%
set_mtl_fs_list(Efs_0, _L1, [])
  when is_list(Efs_0) ->
    Efs_0;
set_mtl_fs_list(Efs_0, L1, MatNames)
  when is_list(Efs_0), is_list(L1), is_list(MatNames) ->
    case proplists:get_value(layerelementmaterial, L1, none) of
        none ->
            set_mtl_fs_list_1(Efs_0, [0], MatNames);
        [] ->
            set_mtl_fs_list_1(Efs_0, [0], MatNames);
        LEM when is_list(LEM) ->
            case proplists:get_value(refinfotype, LEM, none) of
                <<"IndexToDirect">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByPolygon">> ->
                            ML = proplists:get_value(materials, LEM, []),
                            set_mtl_fs_list_1(Efs_0, ML, MatNames);
                        <<"AllSame">> -> %% The same function can be used.
                            ML = proplists:get_value(materials, LEM, []),
                            set_mtl_fs_list_1(Efs_0, ML, MatNames)
                    end
            end
    end.

set_mtl_fs_list_1(L1, [], _MatNames) ->
    L1;
set_mtl_fs_list_1(L1, ML, MatNames) ->
    set_mtl_fs_list_1(L1, ML, MatNames, []).
set_mtl_fs_list_1([#e3d_face{}=F|R], [MLI]=ML, MatNames, OL) ->
    MName = lists:nth(MLI+1, MatNames),
    set_mtl_fs_list_1(R, ML, MatNames, [F#e3d_face{mat=[MName]}|OL]);
set_mtl_fs_list_1([#e3d_face{}=F|R], [MLI|ML], MatNames, OL) ->
    MName = lists:nth(MLI+1, MatNames),
    set_mtl_fs_list_1(R, ML, MatNames, [F#e3d_face{mat=[MName]}|OL]);
set_mtl_fs_list_1([], [_], _MatNames, OL) ->
    lists:reverse(OL).


%% Set UV to e3d_face
%%
set_uv_fs_list(Efs_0, Vertices, L1)
  when is_list(Efs_0), is_list(Vertices), is_list(L1) ->
    case proplists:get_value(layerelementuv, L1, none) of
        none ->
            {Efs_0, []};
        [] ->
            {Efs_0, []};
        LEM when is_list(LEM) ->
            case proplists:get_value(refinfotype, LEM, none) of
                <<"IndexToDirect">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByPolygonVertex">> ->
                            TxTup = proplists:get_value(uv, LEM, []),
                            ML = proplists:get_value(uvindex, LEM, []),
                            {set_uv_fs_list_1(Efs_0, ML), set_uv_float(TxTup)}
                    end;
                <<"Direct">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByPolygonVertex">> ->
                            TxTup = proplists:get_value(uv, LEM, []),
                            ML = lists:seq(0,length(TxTup)-1),
                            {set_uv_fs_list_1(Efs_0, ML), set_uv_float(TxTup)};
                        <<"ByVertice">> ->
                            TxTup = proplists:get_value(uv, LEM, []),
                            set_uv_fs_list_byv(Efs_0, set_uv_float(TxTup), Vertices)
                    end
            end
    end.
set_uv_float(LTup) ->
    lists:map(fun({U,V}) -> {float(U),float(V)} end, LTup).

set_uv_fs_list_1(Efs_0, []) ->
    Efs_0;
set_uv_fs_list_1(Efs_0, UVL) ->
    set_uv_fs_list_1(Efs_0, UVL, []).
set_uv_fs_list_1([#e3d_face{}=F|R], UVL, OL) ->
    {TxI1, UVL_1} = set_uv_fs_list_2(F, UVL),
    set_uv_fs_list_1(R, UVL_1, [F#e3d_face{tx=TxI1}|OL]);
set_uv_fs_list_1([], [], OL) ->
    lists:reverse(OL).

set_uv_fs_list_2(#e3d_face{vs=Vs}, UVL) ->
    set_uv_fs_list_2(Vs, UVL, []).
set_uv_fs_list_2([_|R], [Idx|UVL], OL) ->
    set_uv_fs_list_2(R, UVL, [Idx|OL]);
set_uv_fs_list_2([], UVL, OL) ->
    {lists:reverse(OL), UVL}.

%% ByVertice
set_uv_fs_list_byv(Efs_0, [], _) ->
    {Efs_0, []};
set_uv_fs_list_byv(Efs_0, L1, L2)
  when length(L1) < length(L2) ->
    set_uv_fs_list_byv(Efs_0,
        L1 ++ [{0.0,0.0} || _ <- lists:seq(1, length(L2)-length(L1))],
        L2);
set_uv_fs_list_byv(Efs_0, L1, L2)
  when length(L1) > length(L2) ->
    set_uv_fs_list_byv(Efs_0, lists:sublist(L1,length(L2)), L2);
set_uv_fs_list_byv(Efs_0, L1, _) ->
    {set_uv_fs_list_byv_1(Efs_0, []), L1}.
set_uv_fs_list_byv_1([#e3d_face{vs=Vs}=F|R], OL) ->
    set_uv_fs_list_byv_1(R, [F#e3d_face{tx=Vs}|OL]);
set_uv_fs_list_byv_1([], OL) ->
    lists:reverse(OL).


%% Set Normals to e3d_face
%%
set_ns_fs_list(Efs_0, Vertices, L1)
  when is_list(Efs_0), is_list(Vertices), is_list(L1) ->
    case proplists:get_value(layerelementnormal, L1, none) of
        none ->
            {Efs_0, []};
        [] ->
            {Efs_0, []};
        LEM when is_list(LEM) ->
            case proplists:get_value(refinfotype, LEM, none) of
                <<"IndexToDirect">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByPolygonVertex">> ->
                            TxTup = proplists:get_value(normals, LEM, []),
                            ML = proplists:get_value(normalsindex, LEM, []),
                            {set_ns_fs_list_1(Efs_0, ML), set_ns_float(TxTup)};
                        _ ->
                            {Efs_0, []}
                    end;
                <<"Direct">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByPolygonVertex">> ->
                            TxTup = proplists:get_value(normals, LEM, []),
                            ML = lists:seq(0,length(TxTup)-1),
                            {set_ns_fs_list_1(Efs_0, ML), set_ns_float(TxTup)};
                        <<"ByVertice">> ->
                            TxTup = proplists:get_value(normals, LEM, []),
                            set_ns_fs_list_byv(Efs_0, set_ns_float(TxTup), Vertices);
                        _ ->
                            {Efs_0, []}
                    end;
                _ ->
                    {Efs_0, []}
            end
    end.
set_ns_float(LTup) ->
    lists:map(fun ({X,Y,Z}) -> {float(X),float(Y),float(Z)} end, LTup).


set_ns_fs_list_1(Efs_0, []) ->
    Efs_0;
set_ns_fs_list_1(Efs_0, NsL) ->
    set_ns_fs_list_1(Efs_0, NsL, []).
set_ns_fs_list_1([#e3d_face{}=F|R], NsL, OL) ->
    {TxI1, NsL_1} = set_ns_fs_list_2(F, NsL),
    set_ns_fs_list_1(R, NsL_1, [F#e3d_face{ns=TxI1}|OL]);
set_ns_fs_list_1([], [], OL) ->
    lists:reverse(OL).

set_ns_fs_list_2(#e3d_face{vs=Vs}, NsL) ->
    set_ns_fs_list_2(Vs, NsL, []).
set_ns_fs_list_2([_|R], [Idx|NsL], OL) ->
    set_ns_fs_list_2(R, NsL, [Idx|OL]);
set_ns_fs_list_2([], NsL, OL) ->
    {lists:reverse(OL), NsL}.

%% ByVertice
set_ns_fs_list_byv(Efs_0, [], _) ->
    {Efs_0, []};
set_ns_fs_list_byv(Efs_0, L1, L2)
  when length(L1) < length(L2) ->
    set_ns_fs_list_byv(Efs_0,
        L1 ++ [{0.0,0.0,0.0} || _ <- lists:seq(1,length(L2)-length(L1))],
        L2);
set_ns_fs_list_byv(Efs_0, L1, L2)
  when length(L1) > length(L2) ->
    set_ns_fs_list_byv(Efs_0, lists:sublist(L1,length(L2)), L2);
set_ns_fs_list_byv(Efs_0, L1, _) ->
    {set_ns_fs_list_byv_1(Efs_0, []), L1}.
set_ns_fs_list_byv_1([#e3d_face{vs=Vs}=F|R], OL) ->
    set_ns_fs_list_byv_1(R, [F#e3d_face{ns=Vs}|OL]);
set_ns_fs_list_byv_1([], OL) ->
    lists:reverse(OL).



%% Set hard edges to e3d_face
%%
set_he_list(VtxList, L1)
  when is_list(VtxList), is_list(L1) ->
    case proplists:get_value(layerelementsmoothing, L1, none) of
        none ->
            [];
        [] ->
            [];
        LEM when is_list(LEM) ->
            case proplists:get_value(refinfotype, LEM, none) of
                <<"Direct">> ->
                    case proplists:get_value(mappinginfotype, LEM, none) of
                        <<"ByEdge">> ->
                            EdgeL = proplists:get_value(edges, L1, none),
                            EdgeS = proplists:get_value(smoothing, LEM, []),
                            set_he_list_1(VtxList, zip_edge_list(EdgeL, EdgeS));
                        <<"ByPolygon">> ->
                            PlyS = proplists:get_value(smoothing, LEM, []),
                            set_he_list_ply(VtxList, PlyS);
                        _ ->
                            []
                    end;
                _ ->
                    []
            end
    end.
zip_edge_list(_, []) ->
    [];
zip_edge_list(none, B) ->
    {none, B};
zip_edge_list(A, B) ->
    zip_edge_list(A, B, []).
zip_edge_list([A|EdgeL], [B], OL) ->
    zip_edge_list(EdgeL, [B], [{A,B}|OL]);
zip_edge_list([A|EdgeL], [B|EdgeS], OL) ->
    zip_edge_list(EdgeL, EdgeS, [{A,B}|OL]);
zip_edge_list([], [_], OL) ->
    lists:reverse(OL).
uidx_to_edge(Idx,UE) ->
    case gb_trees:lookup(Idx, UE) of
        {value, E} -> E
    end.
set_he_list_1(VtxList, {none, EL}) ->
    UE = [U || {_,U} <- set_he_uedges(VtxList)],
    [E || {E,On} <- lists:zip(UE,EL), On =:= 0];
set_he_list_1(VtxList, EL) ->
    UE = gb_trees:from_orddict(orddict:from_list(set_he_uedges(VtxList))),
    [uidx_to_edge(VN,UE) || {VN,On} <- EL, On =:= 0].
%% List of unique edges
set_he_uedges(PolygonVIdx) ->
    set_he_uedges(PolygonVIdx, []).
set_he_uedges([[C|[B2|_]=L]|R], OL) ->
    OL_1 = set_he_uedges_1(C, L, [{C,B2}|OL]),
    set_he_uedges(R, OL_1);
set_he_uedges([], OL) ->
    set_he_uedges_2(lists:reverse(OL)).
set_he_uedges_1(F, [C|[B2|_]=L], OL) ->
    set_he_uedges_1(F, L, [{C,B2}|OL]);
set_he_uedges_1(F, [C], OL) ->
    [{C,F}|OL].
set_he_uedges_2(Edges) ->
    set_he_uedges_2(Edges, 0, #{}, []).
set_he_uedges_2([{A1,A2}|R], Idx, Seen, OL) ->
    {OL_1, Seen_1} =
        case maps:is_key({A1,A2},Seen) orelse
             maps:is_key({A2,A1},Seen)
        of
            true ->
                {OL, Seen};
            false ->
                {[{Idx,{A1,A2}}|OL],Seen#{ {A1,A2} => 1 }}
        end,
    set_he_uedges_2(R, Idx+1, Seen_1, OL_1);
set_he_uedges_2([], _Idx, _Seen, OL) ->
    lists:reverse(OL).

%% ByPolygon smoothing seems to be smoothing groups
set_he_list_ply(VtxList, SP) ->
    set_he_list_ply(VtxList, SP, [], orddict:new()).
set_he_list_ply([F|R], [0|LL], HPly, SGPly) ->
    %% A value of 0 is assumed to be hard edges all around.
    set_he_list_ply(R, LL, [set_he_list_ply_edges(F)|HPly], SGPly);
set_he_list_ply([F|R], [SG|LL], HPly, SGPly)
  when SG > 0 ->
    set_he_list_ply(R, LL, HPly,
        orddict:append_list(SG, set_he_list_ply_edges(F), SGPly));
set_he_list_ply([], [], HPly, SGPly) ->
    set_he_list_ply_uedges(
        lists:append(HPly) ++
        lists:append([set_he_list_ply_ra(SEL) || {_,SEL} <- SGPly])).
set_he_list_ply_edges([A|[B|_]=C]) -> set_he_list_ply_edges(A, C, [{A,B}]).
set_he_list_ply_edges(Pt,[A],OL) -> lists:reverse([{A,Pt}|OL]);
set_he_list_ply_edges(Pt,[A|[B|_]=C],OL) -> set_he_list_ply_edges(Pt, C, [{A,B}|OL]).
set_he_list_ply_ra(L) ->
    AdjE = lists:foldl(fun (A,B) -> gb_sets:insert(A,B) end, gb_sets:empty(), L),
    set_he_list_ply_ra(L, AdjE, []).
set_he_list_ply_ra([{A,B}=Tup|L], AdjE, OL) ->
    case gb_sets:is_member({B,A}, AdjE) of
        true ->
            set_he_list_ply_ra(L, AdjE, OL);
        false ->
            set_he_list_ply_ra(L, AdjE, [Tup|OL])
    end;
set_he_list_ply_ra([], _, OL) ->
    lists:reverse(OL).
set_he_list_ply_uedges(L) ->
    set_he_list_ply_uedges(L, gb_sets:empty()).
set_he_list_ply_uedges([{A,B}=Tup|L], UE) ->
    case gb_sets:is_member({B,A},UE) of
        true ->
            set_he_list_ply_uedges(L, UE);
        false ->
            set_he_list_ply_uedges(L, gb_sets:insert(Tup, UE))
    end;
set_he_list_ply_uedges([], UE) ->
    gb_sets:to_list(UE).



from_fbx_material_prop([], _, {number, I0}) ->
    I0;
from_fbx_material_prop([Name|RL], Prop, {number, _}=Default) ->
    case proplists:get_value(Name, Prop, none) of
        {Atm, I} when is_atom(Atm),is_number(I) -> float(I);
        none -> from_fbx_material_prop(RL, Prop, Default)
    end;
from_fbx_material_prop([], _, {color, R0,G0,B0}) ->
    {R0,G0,B0};
from_fbx_material_prop([Name|RL], Prop, {color, _,_,_}=Default) ->
    case proplists:get_value(Name, Prop, none) of
        {Atm,R,G,B}
          when is_atom(Atm),is_number(R),is_number(G),is_number(B) ->
            {float(R),float(G),float(B)};
        {Atm,R,G,B,_}
          when is_atom(Atm),is_number(R),is_number(G),is_number(B) ->
            {float(R),float(G),float(B)};
        none -> from_fbx_material_prop(RL, Prop, Default)
    end.

from_fbx_material({mat,MatName,Cont}, FBXPath)
  when is_atom(MatName) -> %% 'Material_default'
    Maps = from_fbx_tex(Cont,FBXPath),
    
    Prop = proplists:get_value(prop, Cont, []),
    EmCol = from_fbx_material_prop([<<"Emissive">>,<<"EmissiveColor">>], Prop, {color,0.0,0.0,0.0}),
    EmIntens = from_fbx_material_prop([<<"EmissiveFactor">>], Prop, {number,1.0}),
    AmCol = from_fbx_material_prop([<<"Ambient">>,<<"AmbientColor">>], Prop, {color,0.0,0.0,0.0}),
    DifCol = from_fbx_material_prop([<<"Diffuse">>,<<"DiffuseColor">>], Prop, {color,0.5,0.5,0.5}),
    {TC_R,TC_G,TC_B} = from_fbx_material_prop([<<"TransparentColor">>], Prop, {color,0.0,0.0,0.0}),
    DifA_0 = 1.0 - from_fbx_material_prop([<<"TransparencyFactor">>], Prop, {number,0.0}),
    DifA_1 = from_fbx_material_prop([<<"Opacity">>], Prop, {number,1.0}),
    SpecCol = from_fbx_material_prop([<<"Specular">>,<<"SpecularColor">>], Prop, {color,1.0,1.0,1.0}),
    SpecIntens = from_fbx_material_prop([<<"SpecularFactor">>], Prop, {number,1.0}),
    Shine = from_fbx_material_prop([<<"Shininess">>], Prop, {number,9.0}),
    RefleIntens = from_fbx_material_prop([<<"ReflectionFactor">>], Prop, {number,0.1}),
    DifA_2 = 1.0 - ((TC_R+TC_G+TC_B)/3.0),
    
    %% If both values of DifA_0 and DifA_2 are somewhere
    %% below 1.0, there will be some transparency.
    DifA = min(max(DifA_0, DifA_2), DifA_1), %% Choose either Transparent or Opacity as the lowest.

    OpenGL = {opengl, [
        {ambient, rgb_to_rgba(AmCol)},
        {specular, rgb_to_rgba(SpecCol, SpecIntens)},
        {shininess, float(Shine)},
        {diffuse, rgb_to_rgba(DifCol, DifA)},
        {emission, rgb_to_rgba(EmCol, EmIntens)},
        {metallic, RefleIntens},
        {roughness, 0.8},
        {vertex_colors, set}
    ]},
    {MatName, [OpenGL] ++ if length(Maps) > 0 -> [{maps, Maps}]; true -> [] end }.

rgb_to_rgba({R,G,B}) ->
    rgb_to_rgba({R,G,B}, 1.0).
rgb_to_rgba({R,G,B}, Alpha) ->
    {float(R),float(G),float(B),float(Alpha)}.


from_fbx_tex(L, FBXPath) ->
    from_fbx_tex(L, FBXPath, []).
from_fbx_tex([{{tex, {for, Which, TexName}}, Cont}|L], FBXPath, OL) ->
    {_UseMipMap, FileName, RelFileName} = from_fbx_tex_img(Cont),
    MapAtom = case Which of
        <<"DiffuseColor">> -> diffuse;
        <<"NormalMap">> -> normal;
        <<"Bump">> -> bump;
        <<"ReflectionFactor">> -> metallic;
        <<"ShininessExponent">> -> roughness;
        <<"EmissiveColor">> -> emission;
        _ ->
            io:format("~p: Unknown Texture Type: ~p~n", [?MODULE, Which]),
            unknown
    end,
    case proplists:get_value(type, Cont, []) of
        <<"TextureVideoClip">> ->
            FileNameTex = proplists:get_value(filename, Cont, none),
            RelFileNameTex = proplists:get_value(relfilename, Cont, none),
            case load_image(find_image_file(FBXPath, [RelFileName, RelFileNameTex, FileNameTex, FileName])) of
                E3DIm=#e3d_image{} ->
                    from_fbx_tex(L, FBXPath, [{MapAtom, E3DIm#e3d_image{name=atom_to_list(TexName)}}|OL]);
                {error, Err} ->
                    io:format("~p: NOTE: Texture not loaded: ~p~n", [?MODULE,Err]),
                    from_fbx_tex(L, FBXPath, OL)
            end
    end;
from_fbx_tex([_|L], FBXPath, OL) ->
    from_fbx_tex(L, FBXPath, OL);
from_fbx_tex([], _, OL) ->
    lists:reverse(OL).

load_image({error, Err}) ->
    {error, Err};
load_image(Filename)
  when is_list(Filename) ->
    get_bitmap_by_ext(Filename).

get_bitmap_by_ext(FilePath) ->
    Ext = case string:to_lower(filename:extension(FilePath)) of
        ".jpeg" -> ".jpg";
        Ext_0   -> Ext_0
    end,
    F = case Ext of
        ".png" ->
            fun read_png/1;
        ".jpg" ->
            fun read_jpeg/1;
        _ ->
            fun read_default/1
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
            e3d_image:fix_outtype(FileName, E3d, []);
        false ->
            {error, {unsupported_by_wxImage, FileName}}
    end.
read_png(FileName) ->
    case e3d__png:load(FileName) of
        E3D=#e3d_image{} ->
            E3D;
        {error, Err} ->
            {error, Err}
    end.
read_default(FileName) ->
    case e3d_image:load(FileName) of
        E3DImage=#e3d_image{} ->
            E3DImage;
        {error, Err} ->
            {error, Err}
    end.



find_image_file(FBXPath, [none|R]) ->
    find_image_file(FBXPath, R);
find_image_file(FBXPath, L0) ->
    L = [utf8b_to_list(A) || A <- L0, is_binary(A)],
    [FullPath|_] = L,
    Filename = filename:basename(FullPath),
    find_image_file(FBXPath, Filename, L).
find_image_file(FBXPath, Filename, [FilePath|R]) ->
    APath = filename:absname(FilePath, FBXPath),
    case file:read_file_info(APath) of
        {ok, _} ->
            APath;
        _ ->
            find_image_file(FBXPath, Filename, R)
    end;
find_image_file(FBXPath, Filename, []) ->
    APath = filename:absname(Filename, FBXPath),
    case file:read_file_info(APath) of
        {ok, _} ->
            APath;
        _ ->
            {error, {not_found, Filename}}
    end.

from_fbx_tex_img([{{video, _VidName}, Cont} |_]) ->
    case proplists:get_value(type, Cont, []) of
        <<"Clip">> ->
            UseMipMap = proplists:get_value(usemipmap, Cont, 0),
            FileName = proplists:get_value(filename, Cont, none),
            RelFileName = proplists:get_value(relfilename, Cont, none),
            {UseMipMap, FileName, RelFileName}
    end;
from_fbx_tex_img([_|L]) ->
    from_fbx_tex_img(L).



%%% The source for the importer is roughly organized as:
%%%
%%% * Wings3D plugin interface code near the beginning of the source
%%% * Turn obj and mat list to #e3d_*{}
%%% * Build obj and mat list from #fbt{}
%%% * Traversal of initial FBX structure into #fbt{}
%%% * Binary reading of the file to initial FBX structure
%%% * ASCII reading of the file to initial FBX structure
%%%
%%% 

%%
%% Read FBX File
%%

read_fbx_to_models(Fln) ->
    {Models,MatL} = rd_fbx(Fln),
    {Models,MatL}.



%%% The tuple of objects and materials should look similar
%%% to these tuple trees.
%%% 
%%% {Objs, Mats}
%%% 
%%% Objs=
%%%  [{{model,'Model_Cube'},
%%%    [{{mat,'Material_default'},mat},
%%%     {geom,[...,
%%%            {vertices,[{1.0,1.0,-1.0},
%%%                       {1.0,-1.0,-1.0},
%%%                       ...
%%%                       ]},
%%%            {vtxindex,[[0,1,2,3],
%%%                       [4,7,6,5],
%%%                       ...]},
%%%            {edges,[0,1,2,3,4,5,6,7,8,10,14,18]},
%%%            {layerelementnormal,[...,
%%%                                 {mappinginfotype,<<"ByPolygonVertex">>},
%%%                                 {refinfotype,<<"Direct">>},
%%%                                 {normals,[{0.0,0.0,-1.0},
%%%                                           ...]}]},
%%%            {layerelementmaterial,[...,
%%%                                   {mappinginfotype,<<"AllSame">>},
%%%                                   {refinfotype,<<"IndexToDirect">>},
%%%                                   {materials,[0]}]}]},
%%%     {prop,[{<<"Lcl Rotation">>, {lclrotation,0.0,0.0,0.0}},
%%%            {<<"Lcl Scaling">>,  {lclscaling,100.0,100.0,100.0}},
%%%            ...]},
%%%     ...]}]
%%%
%%% Mats=
%%%  [{mat,'Material_default',
%%%        [...,
%%%         {shadingmodel,<<"Phong">>},
%%%         {prop,[{<<"EmissiveColor">>,  {color,0.8,0.8,0.9}},
%%%                {<<"EmissiveFactor">>, {number,0.0}},
%%%                {<<"AmbientColor">>,   {color,0.0,0.0,0.0}},
%%%                {<<"DiffuseColor">>,   {color,0.8,0.8,0.8}},
%%%                {<<"DiffuseFactor">>,  {number,0.8}},
%%%                {<<"TransparentColor">>,{color,1.0,1.0,1.0}},
%%%                {<<"SpecularColor">>,  {color,1.0,1.0,1.0}},
%%%                {<<"SpecularFactor">>, {number,0.25}},
%%%                {<<"Shininess">>,      {number,9.0}},
%%%                {<<"ShininessExponent">>,{number,9.0}},
%%%                {<<"ReflectionColor">>,  {color,1.0,1.0,1.0}},
%%%                {<<"ReflectionFactor">>, {number,0.0}}]}]}]
%%% 


%%% Traverse nested models from the root node
%%%

tmod({_,_,RootList}, MatL) ->
    {[],Models} = tmod_1(RootList),
    {Models, MatL}.
tmod_1(List) ->
    tmod_1(List, [], []).
tmod_1([{{model,ID},M}|L], OL, O2) ->
    {M_1,L_1} = tmod_1(M),
    A = { {model,ID},
          move_to_geom(M_1) },
    tmod_1(L, L_1 ++ [A|OL], O2);
tmod_1([{{model,ID,MName,MType},M}|L], OL, O2) ->
    {M_1,L_1} = tmod_1(M),
    A = { {model,ID,MName,MType},
          move_to_geom(M_1) },
    tmod_1(L, L_1 ++ [A|OL], O2);
tmod_1([A|L], OL, O2) ->
    tmod_1(L, OL, [A|O2]);
tmod_1([], OL, O2) ->
    {lists:reverse(O2), lists:reverse(OL)}.


%% Move some tuples to a geom tuple if they are found in 
%% the model tuple list.
%%
move_to_geom(L) ->
    move_to_geom(L,[],[]).
move_to_geom([{Atm,_}=V|R], O1, O2)
  when Atm =:= vertices; Atm =:= vtxindex; Atm =:= geomver;
       Atm =:= layerelementnormal;
       Atm =:= layerelementcolor; Atm =:= layerelementsmoothing;
       Atm =:= layerelementuv; Atm =:= layerelementmaterial ->
    move_to_geom(R, O1, [V|O2]);
move_to_geom([E|R], O1, O2) ->
    move_to_geom(R, [E|O1], O2);
move_to_geom([], O1, []) ->
    lists:reverse(O1);
move_to_geom([], O1, O2)
  when length(O2) > 0 ->
    lists:reverse([{geom, lists:reverse(O2)}|O1]).


%%% Connect objects to materials and geometry for importing
%%%

-record(fbt, {
    header,
    global,
    documents,
    references,
    objects,
    connections
}).


%% Use the connection information to merge the different
%% FBX entities (model, mesh, etc.) into a nested tree.
%% Geometry will merge into models, materials will get
%% a reference merged into models, and so on.
%%
connectobj(#fbt{objects=ObjL,connections=CL}) ->
    RootNode = do_connect_obj(sort_connects(CL), to_id_map(ObjL)),
    RootNode.

split_name_pair(R) ->
    split_name_pair(R, []).
split_name_pair(<<0,_,R/binary>>, OL) ->
    {iolist_to_binary(lists:reverse(OL)), R};
split_name_pair(<<C,R/binary>>, OL) when C =/= 0, C =/= 1 ->
    split_name_pair(R, [C|OL]);
split_name_pair(<<>>, OL) ->
    {iolist_to_binary(lists:reverse(OL)), <<>>}.

get_atom_from_pair(<<>>) ->
    NewName = io_lib:format("o~w", [abs(erlang:unique_integer())]),
    list_to_atom(lists:flatten(NewName));
get_atom_from_pair(NamePair) when is_binary(NamePair) ->
    case split_name_pair(NamePair) of
        {<<"Material::",Name/binary>>, <<>>} ->
            list_to_atom(utf8b_to_list(Name));
        {<<"Model::",Name/binary>>, <<>>} ->
            list_to_atom(utf8b_to_list(Name));
        {<<"Texture::",Name/binary>>, <<>>} ->
            list_to_atom(utf8b_to_list(Name));
        {Name, <<>>} ->
            list_to_atom(utf8b_to_list(Name));
        {Name, <<"Material">>} ->
            list_to_atom(utf8b_to_list(Name));
        {Name, <<"Model">>} ->
            list_to_atom(utf8b_to_list(Name));
        {Name, <<"Texture">>} ->
            list_to_atom(utf8b_to_list(Name));
        {Name, Kind} ->
            list_to_atom(utf8b_to_list(Kind) ++ "_" ++ utf8b_to_list(Name))
    end.

utf8b_to_list(A) when is_binary(A) ->
    unicode:characters_to_list(A, utf8).

%% Turn the list of objects into a map data type where the
%% key is either the numeric ID or the binary name pair.
%%
to_id_map(ObjL) ->
    to_id_map(ObjL, []).
to_id_map([{_,{ID,NamePair,_},_}=O|ObjL], OL)
  when is_number(ID) ->
    Atom = get_atom_from_pair(NamePair),
    to_id_map(ObjL, [
        {ID,{Atom,O}}
        |OL]);
to_id_map([{_,{none,NamePair,_},_}=O|ObjL], OL)
  when is_binary(NamePair) ->
    Atom = get_atom_from_pair(NamePair),
    to_id_map(ObjL, [
        {NamePair,{Atom,O}}
        |OL]);
to_id_map([], OL) ->
    OL_1 = [
        {0,{none,{rootnode,{0,false,false},[]}}}
        |OL],
    maps:from_list(OL_1).

%% When ID numbers are used.
do_connect_obj([{c,From,To,ConnectType}|CL],IDMap)
  when is_number(From),
       is_number(To) ->
    case maps:is_key(From, IDMap) andalso
         maps:is_key(To, IDMap)
    of
        true ->
            do_connect_obj_1(From,To,ConnectType,CL,IDMap);
        false ->
            do_connect_obj(CL,IDMap)
    end;

%% When ID numbers are not used.
do_connect_obj([{c,From,To_0,ConnectType}|CL],IDMap)
  when is_binary(From),
       is_binary(To_0) ->
    To = if To_0 =:= <<"Scene",0,1,"Model">> -> 0;
            To_0 =:= <<"Model::Scene">> -> 0;
            true -> To_0 end,
    case maps:is_key(From, IDMap) andalso
         maps:is_key(To, IDMap)
    of
        true ->
            do_connect_obj_1(From,To,ConnectType,CL,IDMap);
        false ->
            do_connect_obj(CL,IDMap)
    end;

do_connect_obj([],IDMap) ->
    case maps:is_key(0, IDMap) of
        true ->
            ML = c_materials_list(IDMap),
            {_, RootNode} = maps:get(0, IDMap),
            {RootNode, ML};
        false ->
            error("Could not find root node")
    end.

do_connect_obj_1(From,To,ConnectType,CL,IDMap) ->
    case maps:get(From, IDMap) of
        {MAtomName, {mat,{_MergeID,_MergeName,_MergeType},_}} ->
            {AtomName, {Kind,Idents,Obj_0}} = maps:get(To, IDMap),
            %% Just refer to the material
            Obj_1 = [{{mat,MAtomName},mat}|Obj_0],
            IDMap_1 = maps:update(To, {AtomName, {Kind,Idents,Obj_1}}, IDMap),
            do_connect_obj(CL,IDMap_1);
        {MAtomName, {MergeKind,{_MergeID,_MergeName,_MergeType},MergeO}} ->
            {AtomName, {Kind,Idents,Obj_0}} = maps:get(To, IDMap),
            Obj_1 = case {MergeKind,Kind} of
                {geom,model} ->
                    %% We can probably assume there's one geometry
                    %% for a model
                    [{MergeKind,MergeO}|Obj_0];
                _ ->
                    MAtomName_1 = case ConnectType of
                        oo ->
                            MAtomName;
                        {op, PType} ->
                            {for, PType, MAtomName}
                    end,
                    [{{MergeKind,MAtomName_1},MergeO}|Obj_0]
            end,
            IDMap_1 = maps:update(To, {AtomName, {Kind,Idents,Obj_1}}, IDMap),
            do_connect_obj(CL,IDMap_1)
    end.


%% Make a list of all the materials so they can
%% be refered to from the models.
%%
c_materials_list(IDMap) ->
    c_materials_list(maps:to_list(IDMap),[]).
c_materials_list([{_,{MName,{mat,_,Mat}}}|L],ML) ->
    c_materials_list(L,[{mat,MName,Mat}|ML]);
c_materials_list([_|L],ML) ->
    c_materials_list(L,ML);
c_materials_list([],ML) ->
    lists:reverse(ML).



%% Sort the connections so "from" nodes
%% are fully merged before they are merged
%% to the next node.
%%
sort_connects(L) ->
    sort_connects(L,[]).
sort_connects([{c,A,_,_}=Cn|L],OL) ->
    case nodes_before(A,L) of
        {[],L_1} ->
            sort_connects(L_1,[Cn|OL]);
        {BefL,L_1} ->
            sort_connects(BefL ++ [Cn|L_1],OL)
    end;
sort_connects([],OL) ->
    lists:reverse(OL).
nodes_before(For,L) ->
    nodes_before(For,L,[],[]).
nodes_before(For,[{c,_,B,_}=Cn|L],BefL,O2)
  when For =:= B ->
    nodes_before(For,L,[Cn|BefL],O2);
nodes_before(For,[Cn|L],BefL,O2) ->
    nodes_before(For,L,BefL,[Cn|O2]);
nodes_before(_For,[],BefL,O2) ->
    {lists:reverse(BefL),lists:reverse(O2)}.

    





%%% FBX import traversal after parsing
%%%

%% The following functions take patterns of FBX elements
%% and turn them into a particular data type for a parent
%% element.
%%

%% Take a list of numbers for an element.
%%
trav_numlist(Atm,List,Etc,OL) ->
    trav_numlist(Atm,List,Etc,OL, fun(A) -> A end).
trav_numlist(Atm,[[]],_,OL,_AfterFun) ->
    [{Atm,[]}|OL];
trav_numlist(Atm,[[Num|_]=Numbers],_,OL,AfterFun)
  when is_float(Num); is_integer(Num) ->
    [{Atm,AfterFun(Numbers)}|OL];
trav_numlist(Atm,[{double,Num}|_]=L1,_,OL,AfterFun)
  when is_float(Num); is_integer(Num) ->
    Numbers = [float(N) || {double,N} <- L1],
    [{Atm,AfterFun(Numbers)}|OL];
trav_numlist(Atm,[{int,Num}|_]=L1,_,OL,AfterFun)
  when is_float(Num); is_integer(Num) ->
    Numbers = [N || {int,N} <- L1],
    [{Atm,AfterFun(Numbers)}|OL];
trav_numlist(Atm,[{starnum,_Count}|_],[{<<"a">>,L1,_}|_]=_List,OL,AfterFun) ->
    Numbers = [N || {number,N} <- L1],
    [{Atm,AfterFun(Numbers)}|OL];
trav_numlist(Atm,[{number,Num}|_]=L1,_,OL,AfterFun)
  when is_float(Num) ->
    Numbers = [float(N) || {number,N} <- L1],
    [{Atm,AfterFun(Numbers)}|OL];
trav_numlist(Atm,[{number,_}|_]=L1,_,OL,AfterFun) ->
    Numbers = [N || {number,N} <- L1],
    [{Atm,AfterFun(Numbers)}|OL].


%% Take a boolean for an element
%%
trav_bool(Atm,[{bool,Num}|_],_,OL) ->
    [{Atm,Num}|OL];
trav_bool(Atm,[{T,Num}|_],_,OL)
  when T =:= int orelse
       (T =:= number andalso is_integer(Num)) ->
    [{Atm, Num > 0 }|OL].


%% Take an integer for an element.
%%
trav_int(Atm,[{int,Num}|_],_,OL) ->
    [{Atm,Num}|OL];
trav_int(Atm,[{number,Num}|_],_,OL) when is_integer(Num) ->
    [{Atm,Num}|OL].


%% Take a string for an element
%%
trav_str(Atm,[Str|_],_,OL) when is_binary(Str) ->
    [{Atm,Str}|OL].


%% Take a pair of numbers for an element
%%
trav_num2(Atm,[{int,Num1},{int,Num2}|_],_,OL) ->
    [{Atm,{Num1,Num2}}|OL];
trav_num2(Atm,[{FT1,Num1},{FT2,Num2}|_],_,OL)
  when FT1 =:= float orelse FT1 =:= double,
       FT2 =:= float orelse FT2 =:= double ->
    [{Atm,{float(Num1),float(Num2)}}|OL];
trav_num2(Atm,[{number,Num1},{number,Num2}|_],_,OL) ->
    [{Atm,{Num1,Num2}}|OL].


%% Take a quad of numbers for an element.
%%
trav_num4(Atm,[{int,Num1},{int,Num2},{int,Num3},{int,Num4}|_],_,OL) ->
    [{Atm,{Num1,Num2,Num3,Num4}}|OL];
trav_num4(Atm,[{float,Num1},{float,Num2},{float,Num3},{float,Num4}|_],_,OL) ->
    [{Atm,{Num1,Num2,Num3,Num4}}|OL];
trav_num4(Atm,[{number,Num1},{number,Num2},{number,Num3},{number,Num4}|_],_,OL) ->
    [{Atm,{Num1,Num2,Num3,Num4}}|OL].


%% The following function is where the FBX is initially
%% traversed and a #fbt{} record is built.
%%

%% Traverse the top sections of the FBX structure.
%%
trav(S) when is_list(S) ->
    trav_1(S,#fbt{}).
trav_1([], FBT) ->
    FBT;
trav_1([{Name,_Prop,List}|L], FBT) ->
    FBT_1 = case Name of
        <<"FBXHeaderExtension">> -> FBT#fbt{header=trav_header(List)};
        <<"GlobalSettings">> -> FBT#fbt{global=trav_global(List)};
        <<"Documents">> -> FBT#fbt{documents=trav_documents(List)};
        <<"References">> -> FBT#fbt{references=trav_references(List)};
        <<"Definitions">> -> FBT;
        <<"Objects">> -> FBT#fbt{objects=trav_objects(List)};
        <<"Connections">> -> FBT#fbt{connections=trav_connections(List)};
        _Unknown ->
            FBT
    end,
    trav_1(L, FBT_1).



trav_header(_L) ->
    [].

trav_global(_L) ->
    [].

trav_documents(_L) ->
    [].

trav_references(_L) ->
    [].


%% Traverse the objects top part of the FBX structure.
%%
trav_objects(List) ->
    trav_objects(List,[]).
trav_objects([], FT) ->
    lists:reverse(FT);
trav_objects([{Name,Prop,List}|L], FT) ->
    FT_1 = case Name of
        <<"Geometry">> ->
            trav_obj_geom(Prop, List, FT);
        <<"Model">> ->
            trav_obj_model(Prop, List, FT);
        <<"Material">> ->
            trav_obj_material(Prop, List, FT);
        <<"Video">> ->
            trav_obj_video(Prop, List, FT);
        <<"Texture">> ->
            trav_obj_texture(Prop, List, FT);

        _ ->
            FT
    end,
    trav_objects(L, FT_1).


%% Get the different identifiers that are in the parameters
%% of models, geometry and material nodes.
%%
idents_from_prop([{Atm, ID}, NamePair, Type])
  when is_binary(NamePair),
       is_binary(Type),
       (Atm =:= number orelse Atm =:= int) ->
    {ID, NamePair, Type};
idents_from_prop([NamePair, Type])
  when is_binary(NamePair),
       is_binary(Type) ->
    {none, NamePair, Type}.



%% Turn Vertices list to list of tuples
%%
numbers_to_tuplelist(List) ->
    numbers_to_tuplelist(List, []).
numbers_to_tuplelist([X,Y,Z|L], OL) ->
    numbers_to_tuplelist(L, [{X,Y,Z}|OL]);
numbers_to_tuplelist([], OL) ->
    lists:reverse(OL).
    

%% Turn UV list to list of tuples
%%
numbers_to_tuplelist2(List) ->
    numbers_to_tuplelist2(List, []).
numbers_to_tuplelist2([X,Y|L], OL) ->
    numbers_to_tuplelist2(L, [{X,Y}|OL]);
numbers_to_tuplelist2([], OL) ->
    lists:reverse(OL).
    

%% Turn PolygonVertexIndex list to list of lists.
%%
polyvtxi_to_lists(List) ->
    polyvtxi_to_lists(List, [], []).
polyvtxi_to_lists([A|L], O1, OL) when A >= 0 ->
    polyvtxi_to_lists(L, [A|O1], OL);
polyvtxi_to_lists([A|L], O1, OL) when A < 0 ->
    B = lists:reverse([((-A)-1)|O1]),
    polyvtxi_to_lists(L, [], [B|OL]);
polyvtxi_to_lists([], [], OL) ->
    lists:reverse(OL).


%% Traverse the initial FBX structure for geometry
%% information.
%%
trav_obj_geom(Prop, List, FT) ->
    List_1 = trav_obj_geom_1(List, []),
    Idents = idents_from_prop(Prop),
    [{geom, Idents, List_1}|FT].
trav_obj_geom_1([], OL) ->
    lists:reverse(OL);
trav_obj_geom_1([{Name,Prop,List}|L], OL) ->
    OL_1 = case Name of
        <<"Properties70">> -> trav_prop(Prop,List,OL);
        <<"Properties60">> -> trav_propb(Prop,List,OL);
        
        %% These can be either in a geometry or a model object
        <<"Vertices">> -> trav_numlist(vertices,Prop,List,OL, fun numbers_to_tuplelist/1);
        <<"PolygonVertexIndex">> -> trav_numlist(vtxindex,Prop,List,OL, fun polyvtxi_to_lists/1);
        <<"Edges">> -> trav_numlist(edges,Prop,List,OL);
        <<"GeometryVersion">> -> trav_int(geomver,Prop,List,OL);
        <<"LayerElementNormal">> -> trav_obj_geom_layerelementnormal(Prop,List,OL);
        <<"LayerElementBinormal">> -> OL; % unused
        <<"LayerElementTangent">> -> OL; % unused
        <<"LayerElementSmoothing">> -> trav_obj_geom_layerelementsmoothing(Prop,List,OL);
        <<"LayerElementColor">> -> trav_obj_geom_layerelementcolor(Prop,List,OL);
        <<"LayerElementUV">> -> trav_obj_geom_layerelementuv(Prop,List,OL);
        <<"LayerElementMaterial">> -> trav_obj_geom_layerelementmaterial(Prop,List,OL);
        
        <<"Layer">> -> OL; % unused
    
        _ ->
            OL
    end,
    trav_obj_geom_1(L, OL_1).
    

trav_obj_geom_layerelementnormal(_Prop,List,FT) ->
    List_1 = trav_obj_geom_layerelementnormal_1(List,[]),
    [{layerelementnormal,List_1}|FT].
trav_obj_geom_layerelementnormal_1([],OL) ->
    lists:reverse(OL);
trav_obj_geom_layerelementnormal_1([{Name,Prop,List}|L],OL) ->
    OL_1 = case Name of
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Name">> -> trav_str(name,Prop,List,OL);
        <<"MappingInformationType">> -> trav_str(mappinginfotype,Prop,List,OL);  % <<"ByVertice">>
        <<"ReferenceInformationType">> -> trav_str(refinfotype,Prop,List,OL);  % <<"Direct">>
        <<"Normals">> -> trav_numlist(normals,Prop,List,OL,fun numbers_to_tuplelist/1);
        <<"NormalsIndex">> -> trav_numlist(normalsindex,Prop,List,OL);
        <<"NormalsW">> -> trav_numlist(normals_w,Prop,List,OL);
        _ ->
            OL
    end,
    trav_obj_geom_layerelementnormal_1(L,OL_1).


trav_obj_geom_layerelementcolor(_Prop,List,FT) ->
    List_1 = trav_obj_geom_layerelementcolor_1(List,[]),
    [{layerelementcolor,List_1}|FT].
trav_obj_geom_layerelementcolor_1([],OL) ->
    lists:reverse(OL);
trav_obj_geom_layerelementcolor_1([{Name,Prop,List}|L],OL) ->
    OL_1 = case Name of
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Name">> -> trav_str(name,Prop,List,OL);
        <<"MappingInformationType">> -> trav_str(mappinginfotype,Prop,List,OL);  % <<"ByVertice">>
        <<"ReferenceInformationType">> -> trav_str(refinfotype,Prop,List,OL);  % <<"IndexToDirect">>

        <<"Colors">> -> trav_numlist(colors,Prop,List,OL);
        _ ->
            OL
    end,
    trav_obj_geom_layerelementcolor_1(L,OL_1).


trav_obj_geom_layerelementuv(_Prop,List,FT) ->
    List_1 = trav_obj_geom_layerelementuv_1(List,[]),
    [{layerelementuv,List_1}|FT].
trav_obj_geom_layerelementuv_1([],OL) ->
    lists:reverse(OL);
trav_obj_geom_layerelementuv_1([{Name,Prop,List}|L],OL) ->
    OL_1 = case Name of
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Name">> -> trav_str(name,Prop,List,OL);
        <<"MappingInformationType">> -> trav_str(mappinginfotype,Prop,List,OL);  % <<"ByVertice">>
        <<"ReferenceInformationType">> -> trav_str(refinfotype,Prop,List,OL);  % <<"Direct">>

        <<"UV">> -> trav_numlist(uv,Prop,List,OL, fun numbers_to_tuplelist2/1);
        <<"UVIndex">> -> trav_numlist(uvindex,Prop,List,OL);
        
        _ ->
            OL
    end,
    trav_obj_geom_layerelementuv_1(L,OL_1).


trav_obj_geom_layerelementmaterial(_Prop,List,FT) ->
    List_1 = trav_obj_geom_layerelementmaterial_1(List,[]),
    [{layerelementmaterial,List_1}|FT].
trav_obj_geom_layerelementmaterial_1([],OL) ->
    lists:reverse(OL);
trav_obj_geom_layerelementmaterial_1([{Name,Prop,List}|L],OL) ->
    OL_1 = case Name of
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Name">> -> trav_str(name,Prop,List,OL);
        <<"MappingInformationType">> -> trav_str(mappinginfotype,Prop,List,OL);  % <<"AllSame">>
        <<"ReferenceInformationType">> -> trav_str(refinfotype,Prop,List,OL);  % <<"IndexToDirect">>
        <<"Materials">> -> trav_numlist(materials,Prop,List,OL);
        
        _ ->
            OL
    end,
    trav_obj_geom_layerelementmaterial_1(L,OL_1).
    

trav_obj_geom_layerelementsmoothing(_Prop,List,FT) ->
    List_1 = trav_obj_geom_layerelementsmoothing_1(List,[]),
    [{layerelementsmoothing,List_1}|FT].
trav_obj_geom_layerelementsmoothing_1([],OL) ->
    lists:reverse(OL);
trav_obj_geom_layerelementsmoothing_1([{Name,Prop,List}|L],OL) ->
    OL_1 = case Name of
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Name">> -> trav_str(name,Prop,List,OL);
        <<"MappingInformationType">> -> trav_str(mappinginfotype,Prop,List,OL);
        <<"ReferenceInformationType">> -> trav_str(refinfotype,Prop,List,OL);
        <<"Smoothing">> -> trav_numlist(smoothing,Prop,List,OL);
        
        _ ->
            OL
    end,
    trav_obj_geom_layerelementsmoothing_1(L,OL_1).


%% Traverse the initial FBX structure for models
%%
trav_obj_model(Prop, List, FT) ->
    List_1 = trav_obj_model_1(List, []),
    Idents = idents_from_prop(Prop),
    [{model, Idents, List_1}|FT].
trav_obj_model_1([], OL) ->
    lists:reverse(OL);
trav_obj_model_1([{Name,Prop,List}|L], OL) ->
    OL_1 = case Name of
        <<"Properties70">> -> trav_prop(Prop,List,OL);
        <<"Properties60">> -> trav_propb(Prop,List,OL);
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"Shading">> -> trav_bool(shading,Prop,List,OL);
        <<"Culling">> -> trav_str(culling,Prop,List,OL);

        %% These can be either in a geometry or a model object,
        %% older FBXs seem to have them in models.
        %%
        <<"Vertices">> -> trav_numlist(vertices,Prop,List,OL, fun numbers_to_tuplelist/1);
        <<"PolygonVertexIndex">> -> trav_numlist(vtxindex,Prop,List,OL, fun polyvtxi_to_lists/1);
        <<"GeometryVersion">> -> trav_int(geomver,Prop,List,OL);
        <<"LayerElementNormal">> -> trav_obj_geom_layerelementnormal(Prop,List,OL);
        <<"LayerElementBinormal">> -> OL; %% unused
        <<"LayerElementTangent">> -> OL; %% unused
        <<"LayerElementSmoothing">> -> trav_obj_geom_layerelementsmoothing(Prop,List,OL);
        <<"LayerElementColor">> -> trav_obj_geom_layerelementcolor(Prop,List,OL);
        <<"LayerElementUV">> -> trav_obj_geom_layerelementuv(Prop,List,OL);
        <<"LayerElementMaterial">> -> trav_obj_geom_layerelementmaterial(Prop,List,OL);
        
        _ ->
            OL
    end,
    trav_obj_model_1(L, OL_1).


%% Traverse the initial FBX structure for materials
%%
trav_obj_material(Prop, List, FT) ->
    Idents = idents_from_prop(Prop),
    List_1 = trav_obj_material_1(List, []),
    [{mat, Idents, List_1}|FT].
trav_obj_material_1([], OL) ->
    lists:reverse(OL);
trav_obj_material_1([{Name,Prop,List}|L], OL) ->
    OL_1 = case Name of
        <<"Properties70">> -> trav_prop(Prop,List,OL);
        <<"Properties60">> -> trav_propb(Prop,List,OL);
        <<"Version">> -> trav_int(version,Prop,List,OL);
        <<"ShadingModel">> -> trav_str(shadingmodel,Prop,List,OL);
        <<"MultiLayer">> -> trav_int(multilayer,Prop,List,OL);

        _ ->
            OL
    end,
    trav_obj_material_1(L, OL_1).




%% Traverse the initial FBX structure for image references,
%% they are refered to as videos but also are used for images.
%%
trav_obj_video(Prop, List, FT) ->
    Idents = idents_from_prop(Prop),
    List_1 = trav_obj_video_1(List, []),
    [{video, Idents, List_1}|FT].
trav_obj_video_1([], OL) ->
    lists:reverse(OL);
trav_obj_video_1([{Name,Prop,List}|L], OL) ->
    OL_1 = case Name of
        <<"Properties70">> -> trav_prop(Prop,List,OL);
        <<"Properties60">> -> trav_propb(Prop,List,OL);
        <<"Version">> -> trav_int(version,Prop,List,OL);
        
        <<"Type">> -> trav_str(type,Prop,List,OL);
        <<"UseMipMap">> -> trav_int(usemipmap,Prop,List,OL);
        <<"Filename">> -> trav_str(filename,Prop,List,OL);
        <<"RelativeFilename">> -> trav_str(relfilename,Prop,List,OL);

        _ ->
            OL
    end,
    trav_obj_video_1(L, OL_1).


%% Traverse the initial FBX structure for textures.
%%
trav_obj_texture(Prop, List, FT) ->
    Idents = idents_from_prop(Prop),
    List_1 = trav_obj_texture_1(List, []),
    [{tex, Idents, List_1}|FT].
trav_obj_texture_1([], OL) ->
    lists:reverse(OL);
trav_obj_texture_1([{Name,Prop,List}|L], OL) ->
    OL_1 = case Name of
        <<"Properties70">> -> trav_prop(Prop,List,OL);
        <<"Properties60">> -> trav_propb(Prop,List,OL);
        <<"Version">> -> trav_int(version,Prop,List,OL);
        
        <<"Type">> -> trav_str(type,Prop,List,OL);
        <<"TextureName">> -> trav_str(texname,Prop,List,OL);
        <<"Media">> -> trav_str(media,Prop,List,OL);
        <<"FileName">> -> trav_str(filename,Prop,List,OL);
        <<"RelativeFilename">> -> trav_str(relfilename,Prop,List,OL);
        <<"ModelUVTranslation">> -> trav_num2(modeluvtranslation,Prop,List,OL);
        <<"ModelUVScaling">> -> trav_num2(modeluvscaling,Prop,List,OL);
        <<"Texture_Alpha_Source">> -> trav_str(alphasrc,Prop,List,OL);
        <<"Cropping">> -> trav_num4(cropping,Prop,List,OL);

        _ ->
            OL
    end,
    trav_obj_texture_1(L, OL_1).


%% Traverse the initial FBX structure for connection
%% information.
%%
trav_connections(L) ->
    trav_connections_1(L, []).
trav_connections_1([], OL) ->
    lists:reverse(OL);
trav_connections_1([{W,[<<"OO">>,{Tp1,Num1},{Tp2,Num2}],[]}|L], OL)
  when (W =:= <<"C">> orelse W =:= <<"Connect">>),
       (Tp1 =:= number orelse Tp1 =:= int),
       (Tp2 =:= number orelse Tp2 =:= int) ->
    trav_connections_1(L, [{c,Num1,Num2,oo}|OL]);
trav_connections_1([{W,[<<"OO">>,Str1,Str2],[]}|L], OL)
  when (W =:= <<"C">> orelse W =:= <<"Connect">>),
       is_binary(Str1), is_binary(Str2) ->
    trav_connections_1(L, [{c,Str1,Str2,oo}|OL]);
trav_connections_1([{W,[<<"OP">>,{Tp1,Num1},{Tp2,Num2},Kind],[]}|L], OL)
  when (W =:= <<"C">> orelse W =:= <<"Connect">>),
       (Tp1 =:= number orelse Tp1 =:= int),
       (Tp2 =:= number orelse Tp2 =:= int),
       is_binary(Kind) ->
    trav_connections_1(L, [{c,Num1,Num2,{op,Kind}}|OL]);
trav_connections_1([{W,[<<"OP">>,Str1,Str2,Kind],[]}|L], OL)
  when (W =:= <<"C">> orelse W =:= <<"Connect">>),
       is_binary(Str1), is_binary(Str2), is_binary(Kind) ->
    trav_connections_1(L, [{c,Str1,Str2,{op,Kind}}|OL]).


-define(PROP(A), A).
-define(T_FLOAT(A),
    (Tp =:= double orelse
     Tp =:= float orelse
     Tp =:= number)).
-define(T_INT(A,Num),
    (Tp =:= int orelse
     (Tp =:= number andalso is_integer(Num)))).
     

-define(LISTPROP(Name,Other,L),
    [{<<"P">>,[Name|Other],_}|L]
).


%% Forms of properties
%%
trav_prop(_Prop, List, PL) ->
    List_1 = trav_prop_1(List, []),
    [{prop, List_1}|PL].
trav_prop_1([], OL) ->
    lists:reverse(OL);
trav_prop_1(?LISTPROP(Name,[
                <<"Color">>,<<>>,<<"A">>,
                {Tp,R},{Tp,G},{Tp,B}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{color,float(R),float(G),float(B)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"ColorRGB">>,<<"Color">>,<<>>,
                {Tp,R},{Tp,G},{Tp,B}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{color_rgb,float(R),float(G),float(B)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Number">>,<<>>,<<"A">>,
                {Tp,Num}
            ],L), OL)
  when ?T_FLOAT(Tp) orelse Tp =:= int ->
    trav_prop_1(L, [{Name,{number,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Vector3D">>,<<"Vector">>,<<>>,
                {Tp,X},{Tp,Y},{Tp,Z}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{vec3d,float(X),float(Y),float(Z)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"double">>,<<"Number">>,<<>>,
                {Tp,Num}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{double,float(Num)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"int">>,<<"Integer">>,<<>>,{Tp,Num}
            ],L), OL)
  when ?T_INT(Tp,Num) ->
    trav_prop_1(L, [{Name,{int,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"bool">>,<<>>,<<>>,{Tp,Num}
            ],L), OL)
  when ?T_INT(Tp,Num) ->
    trav_prop_1(L, [{Name,{bool,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"enum">>,<<>>,<<>>,{Tp,Num}
            ],L), OL)
  when ?T_INT(Tp, Num) ->
    trav_prop_1(L, [{Name,{enum,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Lcl Translation">>,<<>>,<<"A",_/binary>>,{Tp,X},{Tp,Y},{Tp,Z}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{lcltranslation,float(X),float(Y),float(Z)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Lcl Scaling">>,<<>>,<<"A",_/binary>>,{Tp,X},{Tp,Y},{Tp,Z}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{lclscaling,float(X),float(Y),float(Z)}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Lcl Rotation">>,<<>>,<<"A",_/binary>>,{Tp,X},{Tp,Y},{Tp,Z}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{lclrotation,float(X),float(Y),float(Z)}}|OL]);

trav_prop_1(?LISTPROP(Name,[
                <<"KString">>,<<>>,<<"U">>,Str
            ],L), OL)
  when is_binary(Str) ->
    trav_prop_1(L, [{Name,{kstring,Str}}|OL]);

trav_prop_1(?LISTPROP(Name,[
                <<"KString">>,<<>>,<<>>,Str
            ],L), OL)
  when is_binary(Str) ->
    trav_prop_1(L, [{Name,{kstring,Str}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"KString">>,_,<<>>,Str
            ],L), OL)
  when is_binary(Str) ->
    trav_prop_1(L, [{Name,{kstring,Str}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Bool">>,<<>>,<<"A">>,{Tp,Num}
            ],L), OL)
  when ?T_INT(Tp,Num) ->
    trav_prop_1(L, [{Name,{bool_a,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Integer">>,<<>>,<<"A">>,{Tp,Num}
            ],L), OL)
  when ?T_INT(Tp,Num) ->
    trav_prop_1(L, [{Name,{integer_a,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"Float">>,<<>>,<<"A">>,
                {Tp,Num}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{float_a,Num}}|OL]);
trav_prop_1(?LISTPROP(Name,[
                <<"ColorAndAlpha">>,<<>>,<<"A">>,
                {Tp,R},{Tp,G},{Tp,B},{Tp,Alph}
            ],L), OL)
  when ?T_FLOAT(Tp) ->
    trav_prop_1(L, [{Name,{color_alpha,float(R),float(G),float(B),float(Alph)}}|OL]);

trav_prop_1(?LISTPROP(Name,[<<"Reference">>,<<>>,<<"A">>],L), OL) ->
    trav_prop_1(L, [{Name,{reference}}|OL]);
trav_prop_1(?LISTPROP(Name,[<<"Compound">>,<<>>,<<>>],L), OL) ->
    trav_prop_1(L, [{Name,{compound}}|OL]);

trav_prop_1([{_Name,_,_}|L], OL) ->
    %% Unknown property kind
    trav_prop_1(L, OL).



%% Older forms of properties (Properties60)
%%

trav_propb(_Prop, List, PL) ->
    List_1 = trav_propb_1(List, []),
    [{prop, List_1}|PL].
trav_propb_1([], OL) ->
    lists:reverse(OL);
trav_propb_1([{<<"Property">>,[Name,<<"ColorRGB">>,<<>>,{Tp,R},{Tp,G},{Tp,B}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{color_rgb,float(R),float(G),float(B)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Color">>,<<"A+">>,{Tp,R},{Tp,G},{Tp,B}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{color,float(R),float(G),float(B)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Vector3D">>,<<>>,{Tp,X},{Tp,Y},{Tp,Z}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{vec3d,float(X),float(Y),float(Z)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Number">>,<<"A+">>,{Tp,Num}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{double,float(Num)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"double">>,<<>>,{Tp,Num}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{double,float(Num)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"bool">>,<<>>,{Tp,Num}],_}|L], OL)
  when ?T_INT(Tp,Num) ->
    trav_propb_1(L, [{Name,{bool,Num}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Lcl Translation">>,<<"A+">>,{Tp,X},{Tp,Y},{Tp,Z}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{lcltranslation,float(X),float(Y),float(Z)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Lcl Scaling">>,<<"A+">>,{Tp,X},{Tp,Y},{Tp,Z}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{lclscaling,float(X),float(Y),float(Z)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"Lcl Rotation">>,<<"A+">>,{Tp,X},{Tp,Y},{Tp,Z}],_}|L], OL)
  when ?T_FLOAT(Tp) ->
    trav_propb_1(L, [{Name,{lclrotation,float(X),float(Y),float(Z)}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"enum">>,<<>>,{Tp,Num}],_}|L], OL)
  when ?T_INT(Tp, Num) ->
    trav_propb_1(L, [{Name,{enum,Num}}|OL]);
trav_propb_1([{<<"Property">>,[Name,<<"charptr">>,_,Str],_}|L], OL)
  when is_binary(Str) ->
    trav_propb_1(L, [{Name,{kstring,Str}}|OL]);
trav_propb_1([{_Name,_,_}|L], OL) ->
    trav_propb_1(L, OL).


%%% Read binary FBX
%%%

-define(UINT, little-unsigned-integer).
-define(SINT, little-signed-integer).
-define(FLT, little-float).

%% Binary with position to track offsets
-record(binp, {
    c :: binary(),
    p :: integer()
}).

%% With a filename, read the file contents and either read
%% it as a binary file or call read_fbxa_content/1 if it
%% is an ASCII file.
%%
rd_fbx(Filename) ->
    case file:read_file(Filename) of
        {ok, BinCont} ->
            Cont = #binp{c=BinCont,p=0},
            case read_header(Cont) of
                {bin32, Cont_1} ->
                    rd_fbx_1(bin32, Cont_1, []);
                {bin64, Cont_1} ->
                    rd_fbx_1(bin64, Cont_1, []);
                {txt, #binp{c=AscCont}=_} ->
                    %% Read as ASCII
                    read_fbxa_content(AscCont)
            end
    end.
rd_fbx_1(BW, Cont, OL) ->
    case read_record(BW, Cont) of
        {eorecords, Cont_1} ->
            rd_fbx_2(Cont_1, lists:reverse(OL));
        {Rec, Cont_1} ->
            rd_fbx_1(BW, Cont_1, [Rec|OL])
    end.
rd_fbx_2(_Cont, OL) ->
    {RootNode, ML} = connectobj(trav(OL)),
    tmod(RootNode, ML).


%% Read the header to determine if it is a binary or text file
%%
read_header(#binp{c= <<"Kaydara FBX Binary  ",0,16#1A,0,Ver:32/?UINT, R/binary>>,p=Pos}) ->
    io:format("~p: File ver: ~p~n", [?MODULE, Ver]),
    if
        Ver < 7500 ->
            {bin32, #binp{c=R,p=Pos+27}};
        true ->
            {bin64, #binp{c=R,p=Pos+27}}
    end;
read_header(#binp{c= <<"Kaydara FBX", _/binary>>=Bin}) ->
    %% This couldn't be parsed as text if this is encountered
    %% and wasn't matched as a binary header, so this file might
    %% not be a recognizable FBX file, return an error.
    %%
    io:format("~p: Unknown header: ~s~w~n", [
        ?MODULE,
        binary:part(Bin, {0, 20}),
        binary:part(Bin, {20, 7})]),
    {error, unknown};
read_header(BP) ->
    {txt, BP}.


inflate(Bin) ->
    Z = zlib:open(),
    ok = zlib:inflateInit(Z, 15),
    Bin_1 = zlib:inflate(Z, Bin),
    ok = zlib:inflateEnd(Z),
    zlib:close(Z),
    iolist_to_binary(Bin_1).


rd_float_list(BP, Len, Enc, _Cln)
  when Enc =:= 0 ->
    rd_float_list_1(BP, Len);
rd_float_list(BP, Len, Enc, Cln)
  when Enc =:= 1 ->
    {Comp, BP_1} = rd_bytes(BP, Cln),
    Decomp = inflate(Comp),
    {FloatList, _} = rd_float_list_1(#binp{c=Decomp,p=0}, Len),
    {FloatList, BP_1}.
rd_float_list_1(BP, Len) ->
    rd_float_list_1(BP, Len, []).
rd_float_list_1(BP, 0, OL) ->
    {lists:reverse(OL), BP};
rd_float_list_1(#binp{c= <<Data:32/?FLT, R/binary>>,p=Pos}, Len, OL)
  when Len > 0 ->
    rd_float_list_1(#binp{c=R,p=Pos+4}, Len-1, [Data|OL]).


rd_double_list(BP, Len, Enc, _Cln)
  when Enc =:= 0 ->
    rd_double_list_1(BP, Len);
rd_double_list(BP, Len, Enc, Cln)
  when Enc =:= 1 ->
    {Comp, BP_1} = rd_bytes(BP, Cln),
    Decomp = inflate(Comp),
    {DoubleList, _} = rd_double_list_1(#binp{c=Decomp,p=0}, Len),
    {DoubleList, BP_1}.
rd_double_list_1(BP, Len) ->
    rd_double_list_1(BP, Len, []).
rd_double_list_1(BP, 0, OL) ->
    {lists:reverse(OL), BP};
rd_double_list_1(#binp{c= <<Data:64/?FLT, R/binary>>,p=Pos}, Len, OL)
  when Len > 0 ->
    rd_double_list_1(#binp{c=R,p=Pos+8}, Len-1, [Data|OL]).

rd_longlong_list(BP, Len, Enc, _Cln)
  when Enc =:= 0 ->
    rd_longlong_list_1(BP, Len);
rd_longlong_list(BP, Len, Enc, Cln)
  when Enc =:= 1 ->
    {Comp, BP_1} = rd_bytes(BP, Cln),
    Decomp = inflate(Comp),
    {LongLongList, _} = rd_longlong_list_1(#binp{c=Decomp,p=0}, Len),
    {LongLongList, BP_1}.
rd_longlong_list_1(BP, Len) ->
    rd_longlong_list_1(BP, Len, []).
rd_longlong_list_1(BP, 0, OL) ->
    {lists:reverse(OL), BP};
rd_longlong_list_1(#binp{c= <<Data1:64/?SINT, R/binary>>,p=Pos}, Len, OL)
  when Len > 0 ->
    rd_longlong_list_1(#binp{c=R,p=Pos+8}, Len-1, [Data1|OL]).


rd_int_list(BP, Len, Enc, Cln)
  when Enc =:= 0 ->
    if Len =:= Cln ->
        rd_int_list_1_b(BP, Len);
    true ->
        rd_int_list_1(BP, Len)
    end;
rd_int_list(BP, Len, Enc, Cln)
  when Enc =:= 1 ->
    {Comp, BP_1} = rd_bytes(BP, Cln),
    Decomp = inflate(Comp),
    {IntList, _} =
        if  byte_size(Decomp) =:= Len -> % Per byte
                rd_int_list_1_b(#binp{c=Decomp,p=0}, Len);
            true ->
                rd_int_list_1(#binp{c=Decomp,p=0}, Len)
        end,
    {IntList, BP_1}.
rd_int_list_1(BP, Len) ->
    rd_int_list_1(BP, Len, []).
rd_int_list_1(BP, 0, OL) ->
    {lists:reverse(OL), BP};
rd_int_list_1(#binp{c= <<Data:32/?SINT, R/binary>>,p=Pos}, Len, OL)
  when Len > 0 ->
    rd_int_list_1(#binp{c=R,p=Pos+4}, Len-1, [Data|OL]).

rd_int_list_1_b(BP, Len) ->
    rd_int_list_1_b(BP, Len, []).
rd_int_list_1_b(BP, 0, OL) ->
    {lists:reverse(OL), BP};
rd_int_list_1_b(#binp{c= <<Data:8/?SINT, R/binary>>,p=Pos}, Len, OL)
  when Len > 0 ->
    rd_int_list_1_b(#binp{c=R,p=Pos+1}, Len-1, [Data|OL]).



rd_bytes(#binp{c=R,p=Pos}, Len) ->
    { binary:part(R, {0, Len}),
      #binp{c=binary:part(R, {Len, byte_size(R) - Len}),p=Pos+Len}}.


rd_bytes_enc(BP, Cln, 0) ->
    rd_bytes(BP, Cln);
rd_bytes_enc(BP, Cln, 1) ->
    {Comp, BP_1} = rd_bytes(BP, Cln),
    Decomp = inflate(Comp),
    {Decomp, BP_1}.


read_record_prop(#binp{c= <<Type, R/binary>>,p=Pos_0}) ->
    Pos = Pos_0+1,
    case Type of
        $v ->
            <<Prim_s:16/?SINT, R_1/binary>> = R,
            { {short, Prim_s},
              #binp{c=R_1,p=Pos+2} };
        $C ->
            <<Prim_b/?SINT, R_1/binary>> = R,
            { {int, Prim_b},
              #binp{c=R_1,p=Pos+1} };
        $I ->
            <<Prim_i:32/?SINT, R_1/binary>> = R,
            { {int, Prim_i},
              #binp{c=R_1,p=Pos+4} };
        $F ->
            <<Prim_f:32/?FLT, R_1/binary>> = R,
            { {float, Prim_f},
              #binp{c=R_1,p=Pos+4} };
        $D ->
            <<Prim_d:64/?FLT, R_1/binary>> = R,
            { {double, Prim_d},
              #binp{c=R_1,p=Pos+8} };
        $L ->
            <<Prim_i:64/?SINT, R_1/binary>> = R,
            { {int, Prim_i},
              #binp{c=R_1,p=Pos+8} };
        
        $f ->
            <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT, R_1/binary>> = R,
            {FloatList, BP_2} = rd_float_list(#binp{c=R_1,p=Pos+12}, Len, Enc, Cln),
            {FloatList, BP_2};
        $d ->
            <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT, R_1/binary>> = R,
            {DoubleList, BP_2} = rd_double_list(#binp{c=R_1,p=Pos+12}, Len, Enc, Cln),
            {DoubleList, BP_2};
        $l ->
            <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT, R_1/binary>> = R,
            {LongLongList, BP_2} = rd_longlong_list(#binp{c=R_1,p=Pos+12}, Len, Enc, Cln),
            {LongLongList, BP_2};
        $i ->
            <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT, R_1/binary>> = R,
            {IntList, BP_2} = rd_int_list(#binp{c=R_1,p=Pos+12}, Len, Enc, Cln),
            {IntList, BP_2};
        $b ->
            <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT, R_1/binary>> = R, %% Bool
            {BoolList, BP_2} = rd_int_list(#binp{c=R_1,p=Pos+12}, Len, Enc, Cln),
            {BoolList, BP_2};
    
        $S ->
            <<Len:32/?UINT, R_1/binary>> = R,
            {String, BP_2} = rd_bytes(#binp{c=R_1,p=Pos+4}, Len),
            {String, BP_2};
        $R ->
            <<Len:32/?UINT, R_1/binary>> = R,
            {Data, BP_2} = rd_bytes(#binp{c=R_1,p=Pos+4}, Len),
            {Data, BP_2};

        $c ->
            <<_Len:32/?UINT,Enc:32/?UINT,Cln:32/?UINT, R_1/binary>> = R,
            {String, BP_2} = rd_bytes_enc(#binp{c=R_1,p=Pos+12}, Cln, Enc),
            {String, BP_2};

        _ ->
            io:format(
                "~p: Unknown: '~c' num=~w at byte position ~w R=~w\n",
                [?MODULE, Type, Type, Pos, binary:part(R, {0, 50})]),
            error(1)
    end.


%% 32-bit offsets for version < 7500
%%
read_record(bin32, #binp{c= <<EndOffset:32/?UINT,NumP:32/?UINT,PropLen:32/?UINT,NameLen,R/binary>>,p=Pos})
  when EndOffset > Pos; EndOffset =:= 0 ->
    read_record_1(bin32, #binp{c=R,p=Pos+4+4+4+1}, EndOffset,NumP,PropLen,NameLen);

%% 64-bit offsets for version >= 7500
%%
read_record(bin64,#binp{c= <<EndOffset:64/?UINT,NumP:64/?UINT,PropLen:64/?UINT,NameLen,R/binary>>,p=Pos})
  when EndOffset > Pos; EndOffset =:= 0 ->
    read_record_1(bin64, #binp{c=R,p=Pos+8+8+8+1}, EndOffset,NumP,PropLen,NameLen).

read_record_1(_, BP, EndOffset,_NumP,_PropLen,_NameLen)
  when EndOffset =:= 0 ->
    %% This is a null padding, return and exit loop
    {eorecords, BP};
read_record_1(BW, BP, EndOffset,NumP,PropLen,NameLen)
  when NameLen > 0 ->
    {Name, BP_1} = rd_bytes(BP, NameLen),
    read_record_2(BW, BP_1, EndOffset,NumP,PropLen,Name);
read_record_1(BW, BP, EndOffset,NumP,PropLen,NameLen)
  when NameLen =:= 0 ->
    read_record_2(BW, BP, EndOffset,NumP,PropLen,<<>>).

read_record_2(BW, BP, EndOffset,NumP,PropLen,Name) ->
    read_record_2(BW, BP, EndOffset,NumP,PropLen,Name,[]).
read_record_2(BW, BP, EndOffset,NumP,PropLen,Name,Props)
  when NumP > 0 ->
    {P, BP_1} = read_record_prop(BP),
    read_record_2(BW, BP_1, EndOffset,NumP-1,PropLen,Name,[P|Props]);
read_record_2(BW, BP, EndOffset,0,PropLen,Name,Props) ->
    read_record_3(BW, BP, EndOffset,lists:reverse(Props),PropLen,Name).

read_record_3(BW, #binp{p=Pos}=BP, EndOffset,Props,_PropLen,Name)
  when Pos < EndOffset ->
    {Records, BP_1} = read_record_3_r(BW, BP, []),
    {{Name, Props, Records}, BP_1};
read_record_3(_, #binp{p=Pos}=BP, EndOffset,Props,_PropLen,Name)
  when Pos =:= EndOffset ->
    {{Name, Props, []}, BP}.

read_record_3_r(BW, BP, OL) ->
    case read_record(BW, BP) of
        {eorecords, BP_1} ->
            {lists:reverse(OL), BP_1};
        {Rec, BP_1} ->
            read_record_3_r(BW, BP_1, [Rec|OL])
    end.



%%% Read ASCII FBX
%%%

%% Read the binary contents as an ASCII representation of
%% the FBX file.
%%
read_fbxa_content(Cont) ->
    {ok, Tokens} = tok(fbxa_strip_comments(Cont)),
    {ok, P} = parse(Tokens),
    {RootNode, ML} = connectobj(trav(P)),
    tmod(RootNode, ML).

%% Remove comments from FBX text
fbxa_strip_comments(Content) ->
    fbxa_strip_comments(Content, []).
fbxa_strip_comments(<<>>, AL) -> iolist_to_binary(lists:reverse(AL));
fbxa_strip_comments(Content, AL) ->
    case binary:split(Content, <<34>>) of
        [BeforeString, InString] -> 
            case fbxa_strip_comments_outside_string(BeforeString) of
                {no_comment, BeforeString_1} ->
                    {InString_2, Rest_2} = fbxa_strip_comments_inside_string(InString),
                    fbxa_strip_comments(Rest_2, [<<34>>,InString_2,<<34>>,BeforeString_1|AL]);
                {had_comment, BeforeString_1} ->
                    %% If the previous part of the source has a comment
                    %% then the double quote inside the comment doesn't carry to the next line.
                    case binary:split(InString, <<10>>) of
                        [_] -> error;
                        [_CommentedString, NextLine] ->
                            fbxa_strip_comments(NextLine, [BeforeString_1|AL])
                    end
            end;
        [_NoString] ->
            {_, Rest_1} = fbxa_strip_comments_outside_string(Content),
            iolist_to_binary(lists:reverse([Rest_1 | AL]))
    end.
fbxa_strip_comments_inside_string(Content) ->
    fbxa_strip_comments_inside_string(Content, []).

fbxa_strip_comments_inside_string(<<>>, AL) ->
    {iolist_to_binary(lists:reverse(AL)), <<>>};
fbxa_strip_comments_inside_string(<<DQ:8, Rest/binary>>, AL) when DQ =:= 34 ->
    {iolist_to_binary(lists:reverse(AL)), Rest};
fbxa_strip_comments_inside_string(<<Char:8, Rest/binary>>, AL) ->
    fbxa_strip_comments_inside_string(Rest, [Char|AL]).

fbxa_strip_comments_outside_string(Content) ->
    fbxa_strip_comments_outside_string(Content, []).
fbxa_strip_comments_outside_string(Content, AL) ->
    case binary:split(Content, <<10>>) of
        [ThisLine, R] ->
            case binary:split(ThisLine, <<";">>) of
                [Keep, _] -> fbxa_strip_comments_outside_string(R, [Keep | AL]);
                [Keep]    -> fbxa_strip_comments_outside_string(R, [Keep | AL])
            end;
        [LastLine] ->
            %% Only the last line matters whether a comment character appears
            %% which overrides the string afterwards.
            case binary:split(LastLine, <<";">>) of
                [Keep, _] -> {had_comment, iolist_to_binary(lists:reverse([Keep|AL]))};
                [Keep]    -> {no_comment,  iolist_to_binary(lists:reverse([Keep|AL]))}
            end
    end.



tok(A) ->
    tok(A, [], []).
tok(<<>>, [], Toks) ->
    {ok, lists:reverse(Toks)};
tok(<<>>, Current, Toks) ->
    tok(<<>>, [], [tok_w(Current)|Toks]);
tok(<<WhiteSpace:8, Rest/binary>>, Current, Toks)
    when WhiteSpace =:= 32;
         WhiteSpace =:= 10; WhiteSpace =:= 13;
         WhiteSpace =:= 9 ->
    case Current of
        [] -> tok(Rest, Current, Toks);
        _  -> tok(Rest, [], [tok_w(Current)|Toks])
    end;
tok(<<DQ:8, Rest_0/binary>>, Current, Toks)
    when DQ =:= 34 ->
    {InsideString, Rest_1} = tok_inside_string(Rest_0),
    case Current of
        [] -> tok(Rest_1, Current, [InsideString | Toks]);
        _  -> tok(Rest_1, [], [InsideString, tok_w(Current)|Toks])
    end;
tok(<<SC:8, Rest/binary>>, Current, Toks)
    when SC =:= ${; SC =:= $};
         SC =:= $,; SC =:= $:; SC =:= $*  ->
    case Current of
        [] -> tok(Rest, Current, [fbxa_tok(SC)|Toks]);
        _  -> tok(Rest, [], [fbxa_tok(SC),tok_w(Current)|Toks])
    end;
tok(<<Char:8, Rest/binary>>, Current, Toks) ->
    tok(Rest, [Char|Current], Toks).

tok_inside_string(A) -> tok_inside_string(A, []).
tok_inside_string(<<>>, AL) -> {iolist_to_binary(lists:reverse(AL)), <<>>};
tok_inside_string(<<DQ:8, Rest/binary>>, AL) when DQ =:= 34 ->
    {iolist_to_binary(lists:reverse(AL)), Rest};
tok_inside_string(<<Char:8, Rest/binary>>, AL) ->
    tok_inside_string(Rest, [Char|AL]).

tok_w(A) ->
    fbxa_word_or_number(iolist_to_binary(lists:reverse(A))).
    
fbxa_word_or_number(<<B:8,_/binary>>=A)
  when B >= $0, B =< $9; B =:= $.; B =:= $- ->
    {number, parsenumber(A)};
fbxa_word_or_number(A) -> {word, A}.

parsenumber(A) ->
    parsenumber_1(binary_to_list(A)).
parsenumber_1(Num_S) ->
    parsenumber_2(Num_S).
parsenumber_2([$.|R]) -> parsenumber_2([$0,$.|R]);
parsenumber_2(Num_S) ->
    case string:split(Num_S, "e") of
        [LExpNum_S, RExpNum_S] ->
            LExpNum = case string:to_float(LExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(LExpNum_S) of
                        {error, _} -> 0;
                        {Num_1, _} -> Num_1
                    end;
                {Num_1, _} -> Num_1
            end,
            RExpNum = case string:to_float(RExpNum_S) of
                {error, no_float} -> 
                    case string:to_integer(RExpNum_S) of
                        {error, _} -> 0;
                        {Num_2, _} -> Num_2
                    end;
                {Num_2, _} -> Num_2
            end,
            LExpNum * math:pow(10, RExpNum);
        [_NoExponent] ->
            case string:to_float(Num_S) of
                {error, no_float} -> 
                    case string:to_integer(Num_S) of
                        {error, _} -> 0;
                        {Num_1, _} -> Num_1
                    end;
                {Num_1, _} -> Num_1
            end
    end.

fbxa_tok($,) -> comma;
fbxa_tok($*) -> star; %% e.g. as *24
fbxa_tok($:) -> colon;
fbxa_tok(${) -> open_curly;
fbxa_tok($}) -> close_curly.

parse(T) ->
    parse(T, []).
parse(T, Cont) ->
    case T of
        [close_curly|_] -> {ok, lists:reverse(Cont), T};
        [] -> {ok, lists:reverse(Cont)};
        [{word, Word}, colon|Rest0] ->
            {ok, Params, Content, Rest1} = parse_record(Rest0),
            parse(Rest1, [{Word, Params, Content} | Cont])
    end.


parse_record(T) ->
    {ok, Params, T_1} = parse_params(T),
    case T_1 of
        [open_curly|T_2] ->
            {ok, List, T_3} = parse(T_2),
            case T_3 of
                [close_curly|T_4] ->
                    {ok, Params, List, T_4}
            end;
        _ ->
            {ok, Params, [], T_1}
    end.


parse_params(T) ->
    parse_params(T, []).
parse_params(T, OL) ->
    case T of
        [close_curly|_] ->
            {ok, lists:reverse(OL), T};
        [open_curly|_] ->
            {ok, lists:reverse(OL), T};
        [star,{number,Number},comma|T_2] ->
            parse_params(T_2, [{starnum, Number}|OL]);
        [{number,Number},comma,{word,<<"L">>}|T_2] -> %% Used in "Takes"
            parse_params(T_2, [{number, Number}|OL]);
        [{number,Number},comma|T_2] ->
            parse_params(T_2, [{number, Number}|OL]);
        [Str,comma|T_2] when is_binary(Str) ->
            parse_params(T_2, [Str|OL]);
        [{word, BoolS},comma|T_2]
          when BoolS =:= <<"T">>;
               BoolS =:= <<"F">>;
               BoolS =:= <<"Y">>;
               BoolS =:= <<"N">>;
               BoolS =:= <<"W">> ->
            parse_params(T_2, [{bool, fbxa_bool_str(BoolS)}|OL]);
        [star,{number,Number}|T_2] ->
            parse_params(T_2, [{starnum, Number}|OL]);
        [{number,Number}|T_2] ->
            parse_params(T_2, [{number, Number}|OL]);
        [Str|T_2] when is_binary(Str) ->
            parse_params(T_2, [Str|OL]);
        [{word,BoolS}|T_2]
          when BoolS =:= <<"T">>;
               BoolS =:= <<"F">>;
               BoolS =:= <<"Y">>;
               BoolS =:= <<"N">>;
               BoolS =:= <<"W">> ->
            parse_params(T_2, [{bool, fbxa_bool_str(BoolS)}|OL]);
        [{word, _}|_] ->
            {ok, lists:reverse(OL), T}
    end.

fbxa_bool_str(<<"T">>) ->
    true;
fbxa_bool_str(<<"F">>) ->
    false;
fbxa_bool_str(<<"Y">>) ->
    true;
fbxa_bool_str(<<"N">>) ->
    false;
fbxa_bool_str(<<"W">>) ->
    true.

%%%
%%%


t_bin() ->
    {ok,Fo}=file:open("debug.out",[write]),
    {Models,Mats}=read_fbx_to_models("box.fbx"),io:format(Fo,"Models=~p~nMats=~p~n", [Models,Mats]),file:close(Fo).

