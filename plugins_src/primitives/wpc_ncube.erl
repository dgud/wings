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
    Nres = dict:fetch(nres, ArgDict),
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
    Verts = ncube_verts(Nres+1),
    Faces = ncube_faces(Nres+1, Nres+1),
    {Vs0, Fs} = clean_indexed_mesh(Verts, Faces),
    Vs1 = transform_mesh(SpherizeFlag, {X,Y,Z}, Vs0),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    {new_shape,cube_str(),Fs,Vs}.

ncube_dialog() ->
    Nres = get_pref(nres, 1),
    SpherizeFlag = get_pref(spherizeflag, false),
    [{hframe,
      [{slider, {text, Nres,
		 [{key,nres},{range,{1,20}}]}}],[{title, ?__(1,"Number of Cuts")}]},
        {hframe,[
            {label_column, [
                {wings_s:dir(x), {text,2.0,[{key,xcube},{range,{0.0,infinity}}]}},
                {wings_s:dir(y), {text,2.0,[{key,ycube},{range,{0.0,infinity}}]}},
                {wings_s:dir(z), {text,2.0,[{key,zcube},{range,{0.0,infinity}}]}}
            ]}
        ],[{margin,false}]},
        {hradio,[{?__(2,"Yes"), true},
                 {?__(3,"No"), false}],
                SpherizeFlag, [{key,spherizeflag}, {title, ?__(4,"Spherize")}]},
        wings_shapes:transform_obj_dlg()
    ].

ncube_verts(Nres) ->
    S = 1.0,
    Nverts = plane_verts(Nres),
    Tverts = [{X, S, Z} || {X,Z} <- Nverts],
    Bverts = [{X, -S, -Z} || {X,Z} <- Nverts],
    Fverts = [{X, -Z, S} || {X,Z} <- Nverts],
    Kverts = [{-X, -Z, -S} || {X,Z} <- Nverts],
    Rverts = [{ S, -X, Z} || {X,Z} <- Nverts],
    Lverts = [{-S,  X, Z} || {X,Z} <- Nverts],
    VertsWithDups = Tverts ++ Bverts ++ Fverts ++ Kverts ++ Rverts ++ Lverts,
    VertsWithDups.

transform_mesh(false, Box, Vs) ->
    [transform(Box,V) || V <- Vs];
transform_mesh(true, Box, Vs) ->
    [transform(Box,e3d_vec:norm(V)) || V <- Vs].

transform({Xs,Ys,Zs}, {Xp,Yp,Zp}) ->
    {Xp*Xs, Yp*Ys, Zp*Zs}.

ncube_faces(Nres, Nres) ->
    Nsq = Nres*Nres,
    Tfaces = plane_faces(Nres, Nres),
    Bfaces = side_faces(Nsq*1, Tfaces),
    Ffaces = side_faces(Nsq*2, Tfaces),
    Kfaces = side_faces(Nsq*3, Tfaces),
    Rfaces = side_faces(Nsq*4, Tfaces),
    Lfaces = side_faces(Nsq*5, Tfaces),
    Faces = Tfaces ++ Bfaces ++ Ffaces ++ Kfaces ++ Rfaces ++ Lfaces,
    Faces.

side_faces(Offset, Faces) ->
    AddOffset = fun([A,B,C,D]) -> [A+Offset,B+Offset,C+Offset,D+Offset] end,
    lists:map(AddOffset, Faces).

plane_verts(Nres) ->
    [{dtc_round((I/(Nres-1)*2-1)), dtc_round((J/(Nres-1)*2-1))}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

plane_faces(Ures, Vres) ->
    [[I*Vres+J, I*Vres+J+1, (I+1)*Vres+J+1, (I+1)*Vres+J]
      || I <- lists:seq(0, Vres-2), J <- lists:seq(0, Ures-2)].

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
    Radius = dict:fetch(radius, ArgDict),
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),

    Vs1 = ngon_verts(NumVerts, Radius),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Vs1),
    Fs = ngon_faces(NumVerts),
    {new_shape,?__(2,"N-Gon"),Fs,Vs}.

ngon_dialog() ->
    NumVerts = get_pref(numverts, 5),
    Radius = get_pref(radius, 1.0),
    [{vframe, [
        {label_column, [
            {?__(3,"Number of Verts"), {slider, {text, NumVerts,
                                                 [{key, numverts}, {range, {3, 20}}]}}},
            {?__(4,"Radius"), {slider, {text, Radius, [{key, radius}, {range, {0.1, 20.0}}]}}}]
        },
        wings_shapes:transform_obj_dlg()]
    }].

ngon_verts(NumVerts, Radius) ->
    Nres = NumVerts,
    Delta = 2*pi()/Nres,
    [{Radius*cos(I*Delta),
      0.0,
      Radius*sin(I*Delta)} || I <- lists:seq(0, Nres-1)].

ngon_faces(NumVerts) ->
    Nres = NumVerts,
    BotFaces = lists:seq(0, Nres-1),
    TopFaces = lists:reverse(BotFaces),
    Faces = [TopFaces, BotFaces],
    Faces.

clean_indexed_mesh(Verts, Faces) ->
    Raw = e3d_util:indexed_to_raw(Verts, Faces),
    e3d_util:raw_to_indexed(Raw).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
