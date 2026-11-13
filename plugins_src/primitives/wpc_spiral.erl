%%
%%  wpc_spiral.erl --
%%
%%     A semi-simple semi-automatic UV-mapping semi-plugin.
%%
%%  Copyright (c) 2001-2011 Jakob Cederlund, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_spiral).
-export([init/0,menu/2,command/2]).

-import(math, [cos/1,sin/1,pi/0]).
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({shape}, []) ->
    spiral_menu();
menu({shape}, Menu) ->
    Menu ++ [separator|spiral_menu()];
menu(_, Menu) -> Menu.

spiral_menu() ->
    [{?__(1,"Spiral"),spiral,[option]},
     {?__(2,"Spring"),spring,[option]}].

command({shape,{spiral,Ask}}, St) -> make_spiral(Ask, St);
command({shape,{spring,Ask}}, St) -> make_spring(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

make_spiral(Ask, St) when is_atom(Ask) ->
    Qs = [
        {label_column, [
            {?__(2,"Loops"), {text, 2,[{range,{1,32}}]}},
            {?__(3,"Segments"), {text, 16,[{range,{3,128}}]}},
            {?__(4,"Sections"), {text, 8,[{range,{2,64}}]}}
        ]},
	wings_shapes:transform_obj_dlg()],
    wings_dialog:dialog_preview({shape,spiral}, Ask, ?__(1,"Create Spiral"), Qs, St);
make_spiral([L,Ns,Nl|Transf], _) ->
    Vs0 = spiral_vertices(Ns, Nl, L),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = spiral_faces(Ns, Nl, L, Vs),
    {new_shape,"spiral",Fs,Vs}.

make_spring(Ask, St) when is_atom(Ask) ->
    Qs = [
        {label_column, [
            {?__(2,"Loops"), {text, 2,[{range,{1,32}}]}},
            {?__(3,"Segments"), {text, 16,[{range,{3,128}}]}},
            {?__(4,"Sections"), {text, 8,[{range,{2,64}}]}}
        ]},
	wings_shapes:transform_obj_dlg()],
    wings_dialog:dialog_preview({shape,spring}, Ask, ?__(1,"Create Spring"), Qs, St);
make_spring([L,Ns,Nl|Transf], _) ->
    Vs0 = spiral_vertices2(Ns, Nl, L),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = spiral_faces(Ns, Nl, L, Vs),
    {new_shape,"spring",Fs,Vs}.

spiral_faces(Ns0, Nl, L, Vs) ->
    Nl2= Nl*2,
    Ns = Ns0*L,
    {UvFun, Scale} = spiral_uv_fun(Vs, Ns, Nl2),
    Vt = list_to_tuple(Vs),
    Factor = Scale*Scale,
    Slices= [ [ {default,
                 [(I+1) rem Ns + J*Ns, I + J*Ns,
                  I+ ((J+1) rem Nl2) *Ns, (I+1) rem Ns + ((J+1) rem Nl2)*Ns],
                 UvFun(I, J), none, none}
		|| I <- lists:seq(0, Ns - 2)]
	      || J <- lists:seq(0, Nl2 - 1)],
    F0 = lists:append(Slices),
    F1 = [spiral_cap(lists:seq((Nl2-1)*Ns, 0, -Ns), {0.125,0.125}, Vt, Factor) | F0],
    F = [spiral_cap(lists:seq(Ns-1, Ns*Nl2-1, Ns), {0.375,0.125}, Vt, Factor) | F1],
    F.

%% UV layout: the body is cut near the middle of its length into two strips
%% stacked in the upper three quarters, with the proportions of the real
%% lengths; the two end caps are polygons in the lower quarter, sized to
%% keep the texture scale of the body.
spiral_uv_fun(Vs, Ns, Nl2) ->
    Vt = list_to_tuple(Vs),
    Pos = fun(K, N) ->
              element(N + (K rem Nl2)*Ns + 1, Vt)
          end,
    {Cum, Lv} = prm_uv_unfold:lengths(Pos, Nl2, Ns, false),
    Total = element(Ns, Cum),
    Mid = Total / 2,
    Cuts = [{abs(element(I+1, Cum) - Mid), I} || I <- lists:seq(1, Ns-2)],
    {_, Half} = lists:min(Cuts),
    LenA = element(Half+1, Cum),
    LenB = Total - LenA,
    Gap = 0.01,
    Scale = min(1.0 / max(LenA, LenB), (0.75 - Gap) / (2*Lv)),
    UvFun =
        fun(I, J) ->
            {Start, Len, Base} =
                case I < Half of
                    true -> {0.0, LenA, 0.25};
                    false -> {LenA, LenB, 0.25 + Lv*Scale + Gap}
                end,
            U0 = (Len - (element(I+1, Cum) - Start)) * Scale,
            U1 = (Len - (element(I+2, Cum) - Start)) * Scale,
            V0 = Base + (J/Nl2) * Lv * Scale,
            V1 = Base + ((J+1)/Nl2) * Lv * Scale,
            [{U1,V0},{U0,V0},{U0,V1},{U1,V1}]
        end,
    {UvFun, Scale}.

spiral_cap(Idx, Center, Vt, Factor) ->
    Ps = [element(V+1, Vt) || V <- Idx],
    R = min(0.12, prm_uv_unfold:cap_radius(Ps, Factor)),
    prm_uv_unfold:cap_face(Idx, Center, R).

spiral_circle(N, Y, R, D, L) ->
    Delta= pi()*2 / N,
    [{(R+I*D)*cos(I*Delta), Y, (R+I*D)*sin(I*Delta)} ||
	I <- lists:seq(0, L*N-1)].

spiral_vertices(Ns, Nl, L) ->
    Nl2 = Nl*2,
    Delta= 2*pi() / Nl2,
    PosAndRads= [{0.25*sin(I*Delta), 0.75 + 0.25*cos(I*Delta)} ||
		    I <- lists:seq(0, Nl2 - 1)],
    Circles= [spiral_circle(Ns, Pos, Rad, 0.05, L) ||
		 {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

spiral_circle2(N, Y, R, D, L) ->
    Delta= pi()*2 / N,
    [{R*cos(I*Delta), Y+I*D, R*sin(I*Delta)} || I <- lists:seq(0, L*N-1)].

spiral_vertices2(Ns, Nl, L) ->
    Nl2 = Nl*2,
    Delta= 2*pi() / Nl2,
    PosAndRads= [{0.25*sin(I*Delta), 0.75 + 0.25*cos(I*Delta)} ||
		    I <- lists:seq(0, Nl2 - 1)],
    Circles= [spiral_circle2(Ns, Pos, Rad, 0.05, L) ||
		 {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).
