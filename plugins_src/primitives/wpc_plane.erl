%%
%%  wpc_plane.erl --
%%
%%     Plane Plugin (generates four classic types of planes)
%%
%%  Copyright (c) 2002-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_plane).
-export([init/0,menu/2,command/2]).
-import(math, [sqrt/1,cos/1,pi/0,pow/2,exp/1]).
-include_lib("wings/intl_tools/wings_intl.hrl").

init() -> true.

menu({shape}, Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [plane_menu()|NewMenu];
parse([A = {_,grid,_,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,plane_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

plane_menu() ->
    {?__(1,"Various Planes"),plane,?__(7,"Create a plane"),[option]}.

command({shape,{plane,Ask}}, St) -> make_plane(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Various Planes
%%%

plane_dialog() ->
    Hook = fun(Var, Val, Sto) ->
        case Var of
            plane_type ->
                wings_dialog:enable(waves, Val=/=regular, Sto),
                wings_dialog:enable(height, (Val=/=regular) and (Val=/=lumpy), Sto),
                wings_dialog:enable(falloff, Val=:=sombrero, Sto);
            _ -> ok
        end
    end,
    [{vframe, [
        {label_column,[
            {?__(1,"Resolution"), {text,40,[{key,resolution},{range,{3,infinity}}]}},
            {?__(2,"Size"), {text,2.0,[{key,plane_size},{range,{0.0,infinity}}]}},
            {?__(3,"Thickness"), {text,0.2,[{key,thickness},{range,{0.0,infinity}}]}}]
        },
        {hradio,
           [{?__(4,"Regular"),regular},
            {?__(5,"Lumpy"),lumpy},
            {?__(6,"Wavy"),wavy},
            {?__(7,"Sombrero"),sombrero}],
            regular, [{key,plane_type},{hook,Hook},{title,?__(8,"Plane Type")}]},
        {label_column,[
            {?__(9,"Waves"), {text,4,[{key,waves},{range,{1,infinity}}]}},
            {?__(10,"Height"), {text,0.4,[{key,height},{range,{0.0,infinity}}]}},
            {?__(11,"Falloff"), {text,0.4,[{key,falloff},{range,{0.0,infinity}}]}}]
        },
        wings_shapes:transform_obj_dlg()]
     }].

make_plane(Ask, St) when is_atom(Ask) ->
    Qs = plane_dialog(),
    wings_dialog:dialog_preview({shape,plane}, Ask, ?__(1,"Plane Options"), Qs, St);
make_plane([{_,Nres},{_,Size},{_,Thickness},{_,regular},_,_,_|Transf], _) ->
    Vs0 = regular_plane_verts(Nres, Size, +Thickness/2) ++
	 regular_plane_verts(Nres, Size, -Thickness/2),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Regular Plane",Fs,Vs};
make_plane([{_,Nres},{_,Size},{_,Thickness},{_,lumpy},{_,Lumps},_,_,
            Rot_X, Rot_Y, Rot_Z, Mov_X ,Mov_Y ,Mov_Z ,Ground], _) ->
    Vs0 = lumpy_plane_verts(Nres, Size, Lumps, +Thickness/2) ++
	 lumpy_plane_verts(Nres, Size, Lumps, -Thickness/2),
    Vs = wings_shapes:transform_obj([Rot_X,Rot_Y,Rot_Z,Mov_X,Mov_Y,Mov_Z,Ground], Vs0),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Lumpy Plane",Fs,Vs};
make_plane([{_,Nres},{_,Size},{_,Thickness},{_,wavy},{_,Waves},{_,Height},_|Transf], _) ->
    Vs0 = wavy_plane_verts(Nres, Size, Waves, Height, +Thickness/2) ++
	 wavy_plane_verts(Nres, Size, Waves, Height, -Thickness/2),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Wavy Plane",Fs,Vs};
make_plane([{_,Nres},{_,Size},{_,Thickness},{_,sombrero},
            {_,Waves},{_,Height},{_,Falloff}|Transf], _) ->
    Vs0 = sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, +Thickness/2) ++
	 sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, -Thickness/2),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = plane_faces(Nres, Nres),
    {new_shape,"Sombrero Plane",Fs,Vs}.

%%%
%%% Vertex distrobutions
%%%

regular_plane_verts(Nres, Size, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

lumpy_plane_verts(Nres, Size, Lumps, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(cos(Lumps*(I/(Nres-1.0)*2-1)*pi())*
	     cos(Lumps*(J/(Nres-1.0)*2-1)*pi())/(Lumps*3)))+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

wavy_plane_verts(Nres, Size, Waves, Height, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(1.0/Waves *
	  cos(2*pi()*sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			+ pow((Waves*(J/(Nres-1.0)*2-1)), 2))))*Height)+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

sombrero_plane_verts(Nres, Size, Waves, Falloff, Height, Thickness) ->
    [{Size*(I/(Nres-1.0)*2-1),
      (Size*(1.0/Waves *
	  cos(2*pi()*sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			 +pow((Waves*(J/(Nres-1.0)*2-1)), 2)))
	      * exp(-sqrt(pow((Waves*(I/(Nres-1.0)*2-1)), 2)
			 +pow((Waves*(J/(Nres-1.0)*2-1)), 2))*Falloff))*Height)+Thickness,
      Size*(J/(Nres-1.0)*2-1)}
      || I <- lists:seq(0, Nres-1), J <- lists:seq(0, Nres-1)].

plane_faces(Nres, Nres) ->
    plane_faces_top(Nres, Nres) ++
    plane_faces_bot(Nres, Nres) ++
    plane_faces_left(Nres, Nres) ++
    plane_faces_right(Nres, Nres) ++
    plane_faces_front(Nres, Nres) ++
    plane_faces_back(Nres, Nres).

plane_faces_top(Ures, Vres) ->
    [[I*Vres+J,I*Vres+J+1,(I+1)*Vres+J+1,(I+1)*Vres+J]
      || I <- lists:seq(0, Vres-2), J <- lists:seq(0, Ures-2)].

plane_faces_bot(Ures, Vres) ->
    Offset = Ures*Vres,
    [[Offset + ((I+1)*Vres+J),
      Offset + ((I+1)*Vres+J+1),
      Offset + (I*Vres+J+1),
      Offset + (I*Vres+J)]
      || I <- lists:seq(0, Vres-2), J <- lists:seq(0, Ures-2)].

plane_faces_left(Ures, Vres) ->
    A = lists:seq(0, Vres-1),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_right(Ures, Vres) ->
    A = lists:seq(Ures*Vres-1, Ures*Vres-Vres, -1),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_front(Ures, Vres) ->
    A = lists:seq(Ures*Vres-Vres, 0, -Vres),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].

plane_faces_back(Ures, Vres) ->
    A = lists:seq(Vres-1, Ures*Vres-1, Vres),
    B = lists:map(fun(X) -> (Ures*Vres)+X end, A),
    [lists:reverse(A) ++ B].
