%%
%%  wpc_ncube.erl --
%%
%%     N-Cube and N-Gon Plugin
%%
%%  Copyright (c) 2003-2006 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_ncube.erl,v 1.2 2006/08/05 21:14:58 antoneos Exp $
%%

-module(wpc_ncube).
-export([init/0,menu/2,command/2]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0,pow/2,exp/1]).
-include("e3d.hrl").

init() -> true.

menu({shape,more}, []) ->
    ncube_menu();
menu({shape,more}, Menu) ->
    Menu ++ [separator|ncube_menu()];
menu(_, Menu) -> Menu.

ncube_menu() ->
    [{"N-Cube",ncube,[option]},
     {"N-Gon",ngon,[option]}].

command({shape,{more,{ncube, Arg}}},_St) -> make_ncube(Arg);
command({shape,{more,{ngon, Arg}}},_St) -> make_ngon(Arg);
command(_, _) -> next.

%%% The rest are local functions.

% =============
% === Ncube ===
% =============
make_ncube(Arg) when is_atom(Arg) ->
    wpa:dialog(Arg, "N-Cube Options", ncube_dialog(),
	fun(Res) -> {shape,{more,{ncube,Res}}} end);
make_ncube(Arg) ->
    % set_pref(Arg),	% don't save
    ArgDict = dict:from_list(Arg),
    Nres = dict:fetch(nres, ArgDict),
    SpherizeFlag = dict:fetch(spherizeflag, ArgDict),
    Verts = ncube_verts(Nres+1),
    Faces = ncube_faces(Nres+1, Nres+1),
    case SpherizeFlag of
	true -> Verts2 = lists:map({e3d_vec, norm}, Verts);
	false ->  Verts2 = Verts
    end,
    {Vs, Fs} = clean_indexed_mesh(Verts2, Faces),
    {new_shape,"N-Cube",Fs,Vs}.

ncube_dialog() ->
    Nres = get_pref(nres, 5),
    SpherizeFlag = get_pref(spherizeflag, false),
    [{hframe, [{label, "Number of Cuts"},
	       {slider, {text, Nres,
	       [{key, nres}, {range, {2, 20}}]}}]},
     {vradio, [{"Yes", true},
	       {"No", false}],
	      SpherizeFlag,
	      [{key,spherizeflag}, {title, "Spherize"}]}
    ].

ncube_verts(Nres) ->
    S = 1.0,
    Nverts = plane_verts(Nres),
    Tverts = [{ X, S, Z} || {X,Z} <- Nverts],
    Bverts = [{ X,-S,-Z} || {X,Z} <- Nverts],
    Fverts = [{ X,-Z, S} || {X,Z} <- Nverts],
    Kverts = [{-X,-Z,-S} || {X,Z} <- Nverts],
    Rverts = [{ S,-X, Z} || {X,Z} <- Nverts],
    Lverts = [{-S, X, Z} || {X,Z} <- Nverts],
    VertsWithDups = Tverts ++ Bverts ++ Fverts ++ Kverts ++ Rverts ++ Lverts,
    VertsWithDups.

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
make_ngon(Arg) when is_atom(Arg) ->
    wpa:dialog(Arg, "N-Gon Options", ngon_dialog(),
	fun(Res) -> {shape,{more,{ngon,Res}}} end);
make_ngon(Arg) ->
    ArgDict = dict:from_list(Arg),
    NumVerts = dict:fetch(numverts, ArgDict),
    Radius = dict:fetch(radius, ArgDict),
    Vs = ngon_verts(NumVerts, Radius),
    Fs = ngon_faces(NumVerts),
    {new_shape,"N-Gon",Fs,Vs}.

ngon_dialog() ->
    NumVerts = get_pref(numverts, 5),
    Radius = get_pref(radius, 1.0),
    [{hframe, [{label, "Number of Verts"},
	       {slider, {text, NumVerts,
	       [{key, numverts}, {range, {2, 20}}]}}]},
     {hframe, [{label, "Radius"},
	       {slider, {text, Radius,
	       [{key, radius}, {range, {0.1, 20.0}}]}}]}].

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

% set_pref(KeyVals) ->
%     wpa:pref_set(?MODULE, KeyVals).

