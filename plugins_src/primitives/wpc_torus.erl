%%
%%  wpc_torus.erl --
%%
%%     Torus Plugin plus two other types of tori borrowed from LightFlow.
%%
%%  Copyright (c) 2002-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% The U resolution is the number of faces that will be generated
%% along the path of the major radius; the V resolution, along the path
%% of the minor radius.
%%
%% To generate square faces, rather than long and thin rectangles,
%% maintain the following ratio: URes/VRes = MajorRad/MinorRad
%% Or you can enter a zero for either U or V and it will automatically adjust.

-module(wpc_torus).
-export([init/0,menu/2,command/2]).

-import(math, [cos/1,sin/1,pi/0]).
-include_lib("wings/intl_tools/wings_intl.hrl").

init() -> true.

menu({shape}, []) ->
    [torus_menu()];
menu({shape}, Menu0) ->
    {P1, P2} = lists:splitwith(fun({_, cone, _, _}) -> false; (_) -> true end, Menu0),
    P1 ++ [torus_menu()|P2];
menu(_, Menu) -> Menu.

torus_menu() ->
    {?__(1,"Torus"),torus,?__(2,"Create a torus"),[option]}.

command({shape,{torus,Ask}}, St) -> make_torus(Ask, St);

command(_, _) -> next.

%%% The rest are local functions.

torus_dialog() ->
    Hook = fun(Var, Val, Sto) ->
	case Var of
	    torus_type ->
		wings_dialog:enable(torus_nodes, Val=/=smooth, Sto),
	    	wings_dialog:enable(node_height, Val=/=smooth, Sto);
	    _ -> ok
	end
    end,
    [
     {label_column,
	[{?__(1,"Sections"), {text,16,[{key,sections},{range,{3,infinity}}]}},
	 {?__(2,"Slices"), {text,8,[{key,slices},{range,{3,infinity}}]}},
	 {?__(3,"Major X Radius"), {text,1.0,[{key,major_x},{range,{0.0,infinity}}]}},
	 {?__(4,"Major Z Radius"), {text,1.0,[{key,major_z},{range,{0.0,infinity}}]}},
	 {?__(5,"Minor Radius"), {text,0.25,[{key,minor_rad},{range,{0.0,infinity}}]}}
	]},
     {hradio,
	[{?__(6,"Smooth"),smooth},
	 {?__(7,"Lumpy"),lumpy},
	 {?__(8,"Spiral"),spiral}],
	 smooth, [{key,torus_type},{hook,Hook},{title,?__(9,"Torus Type")}
	]},
     {label_column,
	[{?__(10,"Nodes"), {text,8,[{key,torus_nodes},{range,{1,infinity}}]}},
	 {?__(11,"Node Height"), {text,0.25,[{key,node_height},{range,{0.0,infinity}}]}}
	]},
     wings_shapes:transform_obj_dlg()].

make_torus(Ask, St) when is_atom(Ask) ->
    Qs = torus_dialog(),
    wings_dialog:dialog_preview({shape,torus}, Ask, ?__(1,"Torus Options"), Qs, St);
make_torus([{_,Ures},{_,Vres},{_,MajXR},{_,MajZR},{_,MinR},{_,smooth},_,_|Transf], _) ->
    Ures0=min_uv_torus_res(Ures),
    Vres0=min_uv_torus_res(Vres),
    Vs0 = make_verts(Ures0, Vres0, MajXR, MajZR, MinR, none, none, 1),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = make_faces(Ures0, Vres0),
    {new_shape,"Torus",Fs,Vs};
make_torus([{_,Ures},{_,Vres},{_,MajXR},{_,MajZR},{_,MinR},{_,lumpy},
            {_,Loops},{_,LoopRad}|Transf], _) ->
    Vs0 = make_verts(Ures, Vres, MajXR, MajZR, MinR, Loops, LoopRad, 2),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = make_faces(Ures, Vres),
    {new_shape,"Lumpy Torus",Fs,Vs};
make_torus([{_,Ures},{_,Vres},{_,MajXR},{_,MajZR},{_,MinR},{_,spiral},
            {_,Loops},{_,LoopRad}|Transf], _) ->
    Vs0 = make_verts(Ures, Vres, MajXR, MajZR, MinR, Loops, LoopRad, 3),
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    Fs = make_faces(Ures, Vres),
    {new_shape,"Spiral Torus",Fs,Vs}.

min_uv_torus_res(Res) when Res =< 3 -> 3;
min_uv_torus_res(Res) -> Res.

%%%
%%% Build Shapes
%%%

make_verts(Ures, Vres, MajXR, MajZR, MinR, Loops, LoopRad, Type) ->
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Du = 2*pi()/Ures,
    Dv = 2*pi()/Vres,
    Make_Vert = case Type of
	1->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		X = (MajXR + MinR*A) * C,
		Y =	  -(MinR*B),
		Z = (MajZR + MinR*A) * D,
		{X,Y,Z}
	    end;
	2 ->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		N = 1+cos(I*Du*Loops)*LoopRad,
		X = (MajXR + MinR*A*N) * C,
		Y =	  -(MinR*B*N),
		Z = (MajZR + MinR*A*N) * D,
		{X,Y,Z}
	    end;
	3 ->
	    fun(I,J) ->
		{A,B,C,D} = {cos(J*Dv), sin(J*Dv), cos(I*Du), sin(I*Du)},
		N = sin(I*Du*Loops)*LoopRad,
		O = cos(I*Du*Loops)*LoopRad,
		X = (MajXR + MinR*A + N) * C,
		Y =	  -(MinR*B + O),
		Z = (MajZR + MinR*A + N) * D,
		{X,Y,Z}
	    end
	end,
    [Make_Vert(I,J) || I <- Us, J <- Vs].

make_faces(Ures, Vres) ->
    Us = lists:seq(0, Ures-1),
    Vs = lists:seq(0, Vres-1),
    Make_Face = fun(I,J) ->
	Idx1 = (J+1) rem Vres + I*Vres,
	Idx2 = (J+1) rem Vres + ((I+1) rem Ures)*Vres,
	Idx3 = J + ((I+1) rem Ures) * Vres,
	Idx4 = J + I*Vres,
	[Idx4,Idx3,Idx2,Idx1]
    end,
    [Make_Face(I,J) || I <- Us, J <- Vs].
