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
        modify_dlg()],
    wings_dialog:dialog_preview({shape,spiral}, Ask, ?__(1,"Create Spiral"), Qs, St);
make_spiral([L,Ns,Nl,
             {_,Rot_X},{_,Rot_Y},{_,Rot_Z},{_,Mov_X},{_,Mov_Y},{_,Mov_Z},{_,Ground}], _) ->
    Vs1 = spiral_vertices(Ns, Nl, L),
    Vs0 = rotate({Rot_X, Rot_Y, Rot_Z}, Vs1),
    Vs = move({Mov_X, Mov_Y, Mov_Z}, Ground, Vs0),
    Fs = spiral_faces(Ns, Nl, L),
    {new_shape,"spiral",Fs,Vs}.
 
make_spring(Ask, St) when is_atom(Ask) ->
    Qs = [
        {label_column, [
            {?__(2,"Loops"), {text, 2,[{range,{1,32}}]}},
            {?__(3,"Segments"), {text, 16,[{range,{3,128}}]}},
            {?__(4,"Sections"), {text, 8,[{range,{2,64}}]}}
        ]},
        modify_dlg()],
    wings_dialog:dialog_preview({shape,spring}, Ask, ?__(1,"Create Spring"), Qs, St);
make_spring([L,Ns,Nl,
             {_,Rot_X},{_,Rot_Y},{_,Rot_Z},{_,Mov_X},{_,Mov_Y},{_,Mov_Z},{_,Ground}], _) ->
    Vs1 = spiral_vertices2(Ns, Nl, L),
    Vs0 = rotate({Rot_X, Rot_Y, Rot_Z}, Vs1),
    Vs = move({Mov_X, Mov_Y, Mov_Z}, Ground, Vs0),
    Fs = spiral_faces(Ns, Nl, L),
    {new_shape,"spring",Fs,Vs}.

spiral_faces(Ns0, Nl, L) ->
    Nl2= Nl*2,
    Ns = Ns0*L,
    Slices= [ [ [(I+1) rem Ns + J*Ns, I + J*Ns,
		 I+ ((J+1) rem Nl2) *Ns, (I+1) rem Ns + ((J+1) rem Nl2)*Ns]
		|| I <- lists:seq(0, Ns - 2)]
	      || J <- lists:seq(0, Nl2 - 1)],
    F0 = lists:append(Slices),
    F1 = [lists:seq((Nl2-1)*Ns, 0, -Ns) | F0],
    F = [lists:seq(Ns-1, Ns*Nl2-1, Ns) | F1],
    F. 

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

modify_dlg() ->
    Hook = fun(Var, Val, Sto) ->
        case Var of
            ground ->
                wings_dialog:enable(mov_y, Val=:=false, Sto);
            _ -> ok
        end
    end,
    {vframe,[
        {hframe,[
            {label_column,
             [{wings_util:stringify(rotate),
               {label_column, [
                   {wings_util:stringify(x),{text, 0.0,[{key,rot_x},{range,{-360.0,360.0}}]}},
                   {wings_util:stringify(y),{text, 0.0,[{key,rot_y},{range,{-360.0,360.0}}]}},
                   {wings_util:stringify(z),{text, 0.0,[{key,rot_z},{range,{-360.0,360.0}}]}}
               ]}
              }
             ]},
            {label_column,
             [{wings_util:stringify(move),
               {label_column, [
                   {wings_util:stringify(x),{text, 0.0,[{key,mov_x},{range,{-360.0,360.0}}]}},
                   {wings_util:stringify(y),{text, 0.0,[{key,mov_y},{range,{-360.0,360.0}}]}},
                   {wings_util:stringify(z),{text, 0.0,[{key,mov_z},{range,{-360.0,360.0}}]}}
               ]}
              }]}
        ],[{margin,false}]},
        {wings_util:stringify(put_on_ground), false, [{key,ground},{hook, Hook}]}
    ],[{title,""},{margin,false}]}.

rotate({0.0,0.0,0.0}, Vs) -> Vs;
rotate({X,Y,Z}, Vs) ->
    MrX = e3d_mat:rotate(X, {1.0,0.0,0.0}),
    MrY = e3d_mat:rotate(Y, {0.0,1.0,0.0}),
    MrZ = e3d_mat:rotate(Z, {0.0,0.0,1.0}),
    Mr = e3d_mat:mul(MrZ, e3d_mat:mul(MrY, MrX)),
    [e3d_mat:mul_point(Mr, V) || V <- Vs].

move({0.0,0.0,0.0}, false, Vs) -> Vs;
move({X,Y,Z}, Ground, Vs0) ->
    Mt = e3d_mat:translate(X,Y,Z),
    Vs = [e3d_mat:mul_point(Mt, V) || V <- Vs0],
    case Ground of
        true ->
            {{_,Y1,_},_} = e3d_bv:box(Vs),
            Mt0= e3d_mat:translate(0.0,-Y1,0.0),
            [e3d_mat:mul_point(Mt0, V) || V <- Vs];
        _ -> Vs
    end.
