%%
%%  wpc_ncube.erl --
%%
%%     N-Cube and N-Gon Plugin
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_ncube).
-export([init/0,menu/2,command/2]).
-import(math, [cos/1,sin/1,pi/0]).
-include_lib("wings/src/wings.hrl").

init() -> true.

menu({shape}, []) ->
    menu();
menu({shape}, Menu) ->
    [Cube|Ngon] = menu(),
    [Cube] ++ Menu ++ [separator|Ngon];
menu(_, Menu) -> Menu.

menu() ->
    [{cube_str(),ncube,?__(1,"Create a cube"),[option]},
     {?__(2,"N-Gon"),ngon,[option]}].

command({shape,{ncube, Arg}}, St) -> make_ncube(Arg, St);
command({shape,{ngon, Arg}}, St) -> make_ngon(Arg, St);
command(_, _) -> next.

cube_str() ->
    ?__(1,"Cube").

%%% The rest are local functions.

% =============
% === Ncube ===
% =============
make_ncube(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,ncube}, Arg, ?__(1,"Cube Options"),
				ncube_dialog(), St);
make_ncube(Arg, _) ->
    % set_pref(Arg),	% don't save
    ArgDict = dict:from_list(Arg),
    Xres = dict:fetch(xres, ArgDict),
    Yres = dict:fetch(yres, ArgDict),
    Zres = dict:fetch(zres, ArgDict),
    X = dict:fetch(xcube, ArgDict)/2,
    Y = dict:fetch(ycube, ArgDict)/2,
    Z = dict:fetch(zcube, ArgDict)/2,
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    SpherizeFlag = dict:fetch(spherizeflag, ArgDict),
    CreateUV = dict:fetch(create_uv, ArgDict),
    Verts = ncube_verts(Xres+1, Yres+1, Zres+1),
    Faces = ncube_faces(Xres+1, Yres+1, Zres+1),
    {Vs0, Fs} = clean_indexed_mesh(Verts, Faces),
    Vs1 = transform_mesh(SpherizeFlag, {X,Y,Z}, Vs0),
    FsUv = ncube_uv_faces(CreateUV, SpherizeFlag, Vs0, Fs, {X,Y,Z}),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    {new_shape,cube_str(),FsUv,Vs}.

ncube_dialog() ->
    Xres = get_pref(xres, 1),
    Yres = get_pref(yres, 1),
    Zres = get_pref(zres, 1),
    SpherizeFlag = get_pref(spherizeflag, false),
    [{vframe, [
        {hframe,[
            {label_column, [
                {?__(5,"Size"),
                    {label_column, [
                        {wings_s:dir(x), {text,2.0,[{key,xcube},{range,{+0.0,infinity}}]}},
                        {wings_s:dir(y), {text,2.0,[{key,ycube},{range,{+0.0,infinity}}]}},
                        {wings_s:dir(z), {text,2.0,[{key,zcube},{range,{+0.0,infinity}}]}}
                    ]}}
            ]},
            {label_column, [
                {?__(1,"Number of Cuts"),
                    {label_column, [
                        {wings_s:dir(x), {text,Xres,[{key,xres},{range,{1,infinity}},{width,4}]}},
                        {wings_s:dir(y), {text,Yres,[{key,yres},{range,{1,infinity}},{width,4}]}},
                        {wings_s:dir(z), {text,Zres,[{key,zres},{range,{1,infinity}},{width,4}]}}
                    ]}}
            ]}
        ],[{title,""},{margin,false}]},
        {hradio,[{?__(2,"Yes"), true},
                 {?__(3,"No"), false}],
                SpherizeFlag, [{key,spherizeflag}, {title, ?__(4,"Spherize")}]},
        wings_shapes_uv:create_uv_dlg(),
        wings_shapes:transform_obj_dlg()
    ],[{margin,false}]}].

ncube_verts(Xres, Yres, Zres) ->
    S = 1.0,
    Xverts = plane_verts(Yres, Zres),
    Yverts = plane_verts(Xres, Zres),
    Zverts = plane_verts(Xres, Yres),
    Tverts = [calc_vs({X, S, Z},{1.0,S,1.0}) || {X,Z} <- Yverts],
    Bverts = [calc_vs({X, S, Z},{1.0,-S,-1.0}) || {X,Z} <- Yverts],
    Fverts = [calc_vs({X, Z, S},{1.0,-1.0,S}) || {X,Z} <- Zverts],
    Kverts = [calc_vs({X, Z, S},{-1.0,-1.0,-S}) || {X,Z} <- Zverts],
    Rverts = [calc_vs({S, X, Z},{S,-1.0,1.0}) || {X,Z} <- Xverts],
    Lverts = [calc_vs({S, X, Z},{-S,1.0,1.0}) || {X,Z} <- Xverts],
    VertsWithDups = Tverts ++ Bverts ++ Fverts ++ Kverts ++ Rverts ++ Lverts,
    VertsWithDups.

calc_vs({X,Y,Z},{Xm,Ym,Zm}) ->
    {safe_multiply(X,Xm),safe_multiply(Y,Ym),safe_multiply(Z,Zm)}.

safe_multiply(N, M) ->
    case abs(N) < ?EPSILON of
        true -> +0.0;
        false -> N*M
    end.

transform_mesh(false, Box, Vs) ->
    [transform(Box,V) || V <- Vs];
transform_mesh(true, Box, Vs) ->
    [transform(Box,e3d_vec:norm(V)) || V <- Vs].

transform({Xs,Ys,Zs}, {Xp,Yp,Zp}) ->
    {Xp*Xs, Yp*Ys, Zp*Zs}.

ncube_faces(Xres, Yres, Zres) ->
    Tcount = Xres * Zres,
    Fcount = Xres * Yres,
    Rcount = Yres * Zres,

    Tfaces = plane_faces(0, Xres, Zres),
    Bfaces = side_faces(Tcount, Tfaces),

    Ffaces = plane_faces((Tcount*2),Xres, Yres),
    Kfaces = side_faces(Fcount, Ffaces),

    Rfaces = plane_faces((Tcount+Fcount)*2, Yres, Zres),
    Lfaces = side_faces(Rcount, Rfaces),

    Faces = Tfaces ++ Bfaces ++ Ffaces ++ Kfaces ++ Rfaces ++ Lfaces,
    Faces.

side_faces(Offset, Faces) ->
    AddOffset = fun([A,B,C,D]) -> [A+Offset,B+Offset,C+Offset,D+Offset] end,
    lists:map(AddOffset, Faces).

plane_verts(Vres, Ures) ->
    [{dtc_round((V/(Vres-1)*2-1)), dtc_round((U/(Ures-1)*2-1))}
      || V <- lists:seq(0, Vres-1), U <- lists:seq(0, Ures-1)].

plane_faces(Offset, Vres, Ures) ->
    [ [(V*Ures)+U+Offset, (V*Ures)+U+Offset+1, ((V+1)*Ures)+U+Offset+1, ((V+1)*Ures)+U+Offset]
        || U <- lists:seq(0, Ures-2), V <- lists:seq(0, Vres-2) ].

dtc_round(Float) ->
    dtc_round(Float, 6).

dtc_round(Float, Decimals) -> % Accurately rounds decimals - www.digithings.com
    (round(Float * math:pow(10, Decimals))) / math:pow(10, Decimals).

% =============
% === N-Gon ===
% =============
make_ngon(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,ngon}, Arg, ?__(1,"N-Gon Options"), ngon_dialog(), St);
make_ngon(Arg, _) ->
    ArgDict = dict:from_list(Arg),
    NumVerts = dict:fetch(numverts, ArgDict),
    Radius0 = dict:fetch(radius, ArgDict),
    DistType = dict:fetch(dist_type, ArgDict),
    CreateUV = dict:fetch(create_uv, ArgDict),
    Radius =
        case DistType of
            radius -> Radius0;
            apothem ->
                Degree = math:pi()/NumVerts,
                Radius0/math:cos(Degree);
            side_len ->
                Degree = math:pi()/NumVerts,
                (Radius0/2)/math:sin(Degree)
        end,
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    Vs1 = ngon_verts(NumVerts, Radius),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    Fs = ngon_uv_faces(CreateUV, Vs1, ngon_faces(NumVerts)),
    {new_shape,?__(2,"N-Gon"),Fs,Vs}.

ngon_dialog() ->
    NumVerts = get_pref(numverts, 5),
    Radius = get_pref(radius, 1.0),
    DistType = get_pref(dist_type, radius),
    [{vframe, [
        {label_column, [
            {?__(3,"Number of Verts"), {slider, {text, NumVerts,
                                                 [{key, numverts}, {range, {3, 100}}]}}}]
        },
        {vframe, [
            {hradio, [
                {?__(6,"Radius"),radius},
                {?__(7,"Apothem"),apothem},
                {?__(8,"Side Length"),side_len}],
                DistType, [{key,dist_type}]},
            {label_column, [
                {?__(4,"Value"), {slider, {text, Radius, [{key, radius}, {range, {0.1, 50.0}}]}}}]
            }
        ],[{title,?__(5,"Perimeter Computed by")},{margin,false}]},
         wings_shapes_uv:create_uv_dlg(),
         wings_shapes:transform_obj_dlg()]
    }].

ngon_verts(NumVerts, Radius) ->
    Nres = NumVerts,
    Delta = 2*pi()/Nres,
    [{Radius*cos(I*Delta),
      +0.0,
      Radius*sin(I*Delta)} || I <- lists:seq(0, Nres-1)].

ngon_faces(NumVerts) ->
    Nres = NumVerts,
    BotFaces = lists:seq(0, Nres-1),
    TopFaces = lists:reverse(BotFaces),
    Faces = [TopFaces, BotFaces],
    Faces.

%% UV layout: the sides are unfolded like an opened box with the real
%% proportions of the box, from the positions of the cube before the spherize
%% and the sizes. With the spherize each side is an island of the azimuthal
%% projection around its center, and the six islands are packed.
ncube_uv_faces(false, _, _, Fs, _) -> Fs;
ncube_uv_faces(true, false, Vs, Fs, Sizes) ->
    Vt = list_to_tuple(Vs),
    Scale = wings_shapes_uv:cross_scale(Sizes),
    CalcUV =
        fun(Ps) ->
            [wings_shapes_uv:cross(cube_side(Ps), P, Sizes, Scale) || P <- Ps]
        end,
    [{default,F,CalcUV(Ps),none,none} || F <- Fs, Ps <- [[element(V+1, Vt) || V <- F]]];
ncube_uv_faces(true, true, Vs, Fs, Sizes) ->
    Vt = list_to_tuple(Vs),
    Vp = list_to_tuple(transform_mesh(true, Sizes, Vs)),
    Side = fun(F) -> cube_side([element(V+1, Vt) || V <- F]) end,
    Patch = fun(S) -> [F || F <- Fs, Side(F) =:= S] end,
    Sides = [right, left, top, bottom, front, back],
    Islands = [wings_shapes_uv:azimuthal(Patch(S), Vp) || S <- Sides],
    wings_shapes_uv:pack(Fs, Islands).

cube_side(Ps) ->
    Xs = [X || {X,_,_} <- Ps],
    Ys = [Y || {_,Y,_} <- Ps],
    Zs = [Z || {_,_,Z} <- Ps],
    case {cube_flat(Xs), cube_flat(Ys), cube_flat(Zs)} of
        {true,_,_} when hd(Xs) > 0 -> right;
        {true,_,_} -> left;
        {_,true,_} when hd(Ys) > 0 -> top;
        {_,true,_} -> bottom;
        {_,_,true} when hd(Zs) > 0 -> front;
        {_,_,true} -> back
    end.

cube_flat(Cs) ->
    Min = lists:min(Cs),
    Max = lists:max(Cs),
    ((Max-Min) < ?EPSILON) andalso (abs(Max) > (1.0-?EPSILON)).

%% UV layout of the N-Gon: the top and the bottom side by side with the
%% real size.
ngon_uv_faces(false, _, Fs) -> Fs;
ngon_uv_faces(true, Vs, [Top,Bot]) ->
    Vt = list_to_tuple(Vs),
    Rs = [max(abs(X), abs(Z)) || {X,_,Z} <- Vs],
    R = lists:max(Rs),
    Gap = 0.08 * R,
    Scale = 1.0 / (4*R + Gap),
    TopUv = [{(X+R)*Scale, (R-Z)*Scale} || V <- Top, {X,_,Z} <- [element(V+1, Vt)]],
    BotUv = [{(X+3*R+Gap)*Scale, (Z+R)*Scale} || V <- Bot, {X,_,Z} <- [element(V+1, Vt)]],
    [{default,Top,TopUv,none,none}, {default,Bot,BotUv,none,none}].

clean_indexed_mesh(Verts, Faces) ->
    Raw = e3d_util:indexed_to_raw(Verts, Faces),
    e3d_util:raw_to_indexed(Raw).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
