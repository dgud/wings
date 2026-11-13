%%
%%  wpc_knot.erl --
%%
%%     Torus Knot Plugin
%%
%%  Copyright (c) 2001-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_knot).
-export([init/0,menu/2,command/2]).

-include_lib("wings/intl_tools/wings_intl.hrl").

init() -> true.

menu({shape}, []) ->
    knot_menu();
menu({shape}, Menu) ->
    Menu ++ [separator|knot_menu()];
menu(_, Menu) -> Menu.

knot_menu() ->
    [{?__(1,"Torus Knot"), knot, [option]}].

command({shape,{knot, Arg}}, St) -> make_knot(Arg, St);
command(_, _) -> next.

%%% The rest are local functions.

make_knot(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,knot}, Arg, "Torus Knot Options", dialog(), St);
make_knot(Arg, _) ->
    ArgDict = dict:from_list(Arg),
    TypeFlag = dict:fetch(typeflag, ArgDict),
    Resolution = dict:fetch(resolution, ArgDict),
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),
    CreateUV = dict:fetch(create_uv, ArgDict),

    case TypeFlag of
	knot2 -> Knot_Func = fun knot2/1;
	knot3 -> Knot_Func = fun knot3/1;
	knot4 -> Knot_Func = fun knot4/1;
	_ -> Knot_Func = fun knot1/1
    end,
    Ures = Resolution,
    Vres = Ures div 10,
    Verts = make_verts(Ures, Vres, Knot_Func),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Verts),
    Faces = uv_faces(CreateUV, make_faces(Ures, Vres), Ures, Vres, Vs),
    {new_shape,"knot",Faces,Vs}.

dialog() ->
    TypeFlag = get_pref(typeflag, knot1),
    Resolution = get_pref(resolution, 80),
    [{label_column, [
	{"Resolution", {slider, {text, Resolution, [{key, resolution}, {range, {30, 300}}]}}}
     ]},
     {hradio, [{"Type 1", knot1},
	       {"Type 2", knot2},
	       {"Type 3", knot3},
	       {"Type 4", knot4}],
	       TypeFlag, [{key,typeflag}, {title, "Knot Type"}]},
     prm_uv_unfold:create_uv_dlg(),
     wings_shapes:transform_obj_dlg()
    ].

make_verts(Ures, Vres, Knot_Func) ->
    Radius = 0.25,
    Pi2 = math:pi()*2,
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Process_Us = fun(U) ->
	T1 = (U+0) * Pi2/Ures,
	T2 = (U+1) * Pi2/Ures,
	A = Knot_Func(T1),		% curr point
	B = Knot_Func(T2),		% next point
	E = e3d_vec:sub(A,B),
	F = e3d_vec:add(A,B),
	G = e3d_vec:norm(e3d_vec:cross(E,F)),
	H = e3d_vec:norm(e3d_vec:cross(E,G)),
	Process_Vs = fun(V) ->
	    K = V * Pi2/Vres,
	    L = {math:cos(K),0.0,math:sin(K)},
	    M = e3d_vec:mul(L,Radius),
	    {X,_,Z} = M,
	    N = e3d_vec:mul(H,X),
	    O = e3d_vec:mul(G,Z),
	    P = e3d_vec:add(N,O),
	    Q = e3d_vec:add(A,P),
	    Q
	end,
	lists:map(Process_Vs, Vs)
    end,
    Verts = lists:map(Process_Us, Us),
    lists:flatten(Verts).

make_faces(Ures, Vres) ->
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Make_Face = fun(I,J) ->
	Idx1 = (J+1) rem Vres + I*Vres,
	Idx2 = (J+1) rem Vres + ((I+1) rem Ures)*Vres,
	Idx3 = J + ((I+1) rem Ures) * Vres,
	Idx4 = J + I*Vres,
	[Idx1,Idx2,Idx3,Idx4]
    end,
    [Make_Face(I,J) || I <- Us, J <- Vs].

uv_faces(false, Fs, _, _, _) -> Fs;
uv_faces(true, Fs, Ures, Vres, Vs) ->
    Vt = list_to_tuple(Vs),
    Pos = fun(K, N) ->
              element((N rem Ures)*Vres + (K rem Vres) + 1, Vt)
          end,
    {Cum, Lv} = prm_uv_unfold:lengths(Pos, Vres, Ures, true),
    Total = element(Ures+1, Cum),
    Mid = Total / 2,
    Cuts = [{abs(element(I+1, Cum) - Mid), I} || I <- lists:seq(1, Ures-1)],
    {_, Half} = lists:min(Cuts),
    LenA = element(Half+1, Cum),
    LenB = Total - LenA,
    Gap = 0.01,
    Scale = min(1.0 / max(LenA, LenB), (1.0 - Gap) / (2*Lv)),
    Make_UV =
        fun(I,J) ->
            {Start, Len, Base} =
                case I < Half of
                    true -> {0.0, LenA, 0.0};
                    false -> {LenA, LenB, Lv*Scale + Gap}
                end,
            U0 = (Len - (element(I+1, Cum) - Start)) * Scale,
            U1 = (Len - (element(I+2, Cum) - Start)) * Scale,
            V0 = Base + (J/Vres) * Lv * Scale,
            V1 = Base + ((J+1)/Vres) * Lv * Scale,
            [{U0,V1},{U1,V1},{U1,V0},{U0,V0}]
        end,
    UVs = [Make_UV(I,J) || I <- lists:seq(0, Ures-1), J <- lists:seq(0, Vres-1)],
    [{default,F,UV,none,none} || {F,UV} <- lists:zip(Fs, UVs)].

knot1(T) ->
    X = math:cos(T) - 2*math:cos(2*T),
    Y = math:sin(3*T),
    Z = math:sin(T) + 2*math:sin(2*T),
    e3d_vec:mul({X,Y,Z}, 0.5).

knot2(T) ->
    X = 10 * (math:cos(T) + math:cos(3*T)) + math:cos(2*T) + math:cos(4*T),
    Y = 4 * math:sin(3*T) * math:sin(5*T/2) + 4*math:sin(4*T) - 2*math:sin(6*T),
    Z = 6 * math:sin(T) + 10 * math:sin(3*T),
    e3d_vec:mul({X,Y,Z}, 0.1).

knot3(T) ->
    Pi = math:pi(),
    X = 2.5*math:cos(T+Pi)/3 + 2*math:cos(3*T),
    Y = 1.5*math:sin(4*T) + math:sin(2*T)/3,
    Z = 2.5*math:sin(T)/3 + 2*math:sin(3*T),
    e3d_vec:mul({X,Y,Z}, 0.5).

knot4(T) ->
    P = 2,
    Q = 5,
    OUT = 1.2*2,
    IN = 0.48*3,
    H = 0.8*2,
    R = OUT + IN*math:cos(P*T),
    Theta = Q*T,
    X = R*math:cos(Theta),
    Y = H*math:sin(P*T),
    Z = R*math:sin(Theta),
    e3d_vec:mul({X,Y,Z}, 0.5).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
