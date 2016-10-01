%%
%%  wings_shapes.erl --
%%
%%     This module contains definition of all primitive
%%     shapes that can be created, such as Cube, Sphere,
%%     and Grid.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_shapes).
-export([menu/2,command/2]).
-export([transform_obj_dlg/0, transform_obj/4, transform_obj/2]).
-include("wings.hrl").

-import(lists, [map/2,seq/2,seq/3]).
-import(math, [sqrt/1,cos/1,sin/1,pi/0]).

menu(Parent, Pos) ->
    wings_menu:popup_menu(Parent, Pos, shape, menu()).

menu() ->
    Opt = [option],
    Menu0 = [
	     {sphere,Opt},
	     {cone,Opt},
	     separator,
	     {tetrahedron,Opt},
	     {octahedron,Opt},
	     {octotoad, Opt},
	     {dodecahedron,Opt},
	     {icosahedron,Opt},
	     separator,
	     {grid,Opt},
	     separator,
	     {prim_name(light),{light,wings_light:light_types()},
	      prim_help(light)},
	     material,
	     image],
    [prim_trans(Item) || Item <- Menu0].


prim_trans(separator) ->
    separator;
prim_trans({K,Opts}) when is_atom(K) ->
    {prim_name(K),K,prim_help(K),Opts};
prim_trans(K) when is_atom(K) ->
    {prim_name(K),K,prim_help(K)};
prim_trans(Other) -> Other.

prim_name(tetrahedron) ->  ?STR(prim_name,tetrahedron,"Tetrahedron");
prim_name(octahedron) ->   ?STR(prim_name,octahedron,"Octahedron");
prim_name(octotoad) ->     ?STR(prim_name,octotoad,"Octotoad");
prim_name(dodecahedron) -> ?STR(prim_name,dodecahedron,"Dodecahedron");
prim_name(icosahedron) ->  ?STR(prim_name,icosahedron,"Icosahedron");
prim_name(cone) ->         ?STR(prim_name,cone,"Cone");
prim_name(sphere) ->       ?STR(prim_name,sphere,"Sphere");
prim_name(grid) ->         ?STR(prim_name,grid,"Grid");
prim_name(light) ->        ?STR(prim_name,light,"Light");
prim_name(material) ->     ?STR(prim_name,material,"Material...");
prim_name(image) ->        ?STR(prim_name,image,"Image...").

prim_help(tetrahedron) ->
    ?STR(prim_help,tetrahedron,"Create a tetrahedron");
prim_help(octahedron) ->
    ?STR(prim_help,octahedron,"Create an octahedron");
prim_help(octotoad) ->
    ?STR(prim_help,octotoad,"Create an octotoad");
prim_help(dodecahedron) ->
    ?STR(prim_help,dodecahedron,"Create a dodecahedron");
prim_help(icosahedron) ->
    ?STR(prim_help,icosahedron,"Create an icosahedron");
prim_help(cone) ->
    ?STR(prim_help,cone,"Create a cone");
prim_help(sphere) ->
    ?STR(prim_help,sphere,"Create a sphere");
prim_help(grid) ->
    ?STR(prim_help,grid,"Create a grid");
prim_help(light) ->
    ?STR(prim_help,light,"Create a light");
prim_help(material) ->
    ?STR(prim_help,material,"Create a material...");
prim_help(image) ->
    ?STR(prim_help,image,"Create an image...").


command({tetrahedron,Ask}, St) -> tetrahedron(Ask, St);
command({octahedron,Ask}, St) -> octahedron(Ask, St);
command({octotoad,Ask}, St) -> octotoad(Ask, St);
command({dodecahedron,Ask}, St) -> dodecahedron(Ask, St);
command({icosahedron,Ask}, St) -> icosahedron(Ask, St);
command({cone,Ask}, St) -> cone(Ask, St);
command({sphere,Ask}, St) -> sphere(Ask, St);
command({grid,Ask}, St) -> grid(Ask, St);
command({light,Type}, St) -> wings_light:create(Type, St);
command(material, St) -> wings_material:new(St);
command(image, St) -> wings_image:create(St).

build_shape(Prefix, Fs, Vs, #st{onext=Oid}=St) ->
    We = wings_we:build(Fs, Vs),
    Name = Prefix++integer_to_list(Oid),
    wings_shape:new(Name, We, St).

tetrahedron(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(tetrahedron,1,"Edge Length"), {text, 2.0,[{range,{0.0,infinity}}]}}]
	 },
	 transform_obj_dlg()],
  ask(tetrahedron, Ask, Q, St);
tetrahedron([L|Transf], St) ->
    Xi = L/2.0,
    Hp = sqrt(3.0),
    Li = Xi*Hp,
    Zi = Xi/Hp,
    Yi = L * sqrt(2.0/3.0),
    Yf = Yi / 3.0,

    Fs = [[2,1,0],[1,2,3],[1,3,0],[3,2,0]],
    Vs0 = [{ 0.0,  Yi-Yf,  0.0},
	   { 0.0, -Yf,   Li-Zi},
	   { -Xi, -Yf,     -Zi},
	   {  Xi, -Yf,     -Zi}],
    %% The string below is intentionally not translated.
    Vs = transform_obj(Transf, Vs0),
    build_shape("tetrahedron", Fs, Vs, St).

octahedron(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	{?STR(octahedron,1,"Height"), {text, 2.0,[{range,{0.0,infinity}}]}}]
	 },
	 transform_obj_dlg()],
    ask(octahedron, Ask, Q, St);
octahedron([L|Transf], St) ->
    Fs = [[2,4,0],[4,2,1],[4,3,0],[3,4,1],[5,2,0],[2,5,1],[3,5,0],[5,3,1]],
    Vs0 = [{L,0.0,0.0},{-L,0.0,0.0},{0.0,L,0.0},
	   {0.0,-L,0.0},{0.0,0.0,L},{0.0,0.0,-L}],
    Vs = transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    build_shape("octahedron", Fs, Vs, St).

octotoad(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	{?STR(octotoad,1,"Height"), {text, 2.0,[{range,{0.0,infinity}}]}}]
	 },
	 transform_obj_dlg()],
    ask(octotoad, Ask, Q, St);
octotoad([L|Transf], St) ->
    Fs = [[2,3,1,0],[7,6,4,5],[9,8,0,1],[10,11,3,2],
	  [12,0,8],[12,14,2,0],[13,9,1],[14,10,2],
	  [15,3,11],[15,13,1,3],[16,17,5,4],[16,20,12,8],
	  [17,16,8,9],[18,19,11,10],[19,18,6,7],[19,23,15,11],
	  [20,16,4],[20,22,14,12],[21,5,17],[21,17,9,13],
	  [21,23,7,5],[22,6,18],[22,18,10,14],[22,20,4,6],
	  [23,19,7],[23,21,13,15]],
    Vs1 = [{1.668,0.556,0.556},{1.668,0.556,-0.556},
	   {1.668,-0.556,0.556},{1.668,-0.556,-0.556},
	   {-1.668,0.556,0.556},{-1.668,0.556,-0.556},
	   {-1.668,-0.556,0.556},{-1.668,-0.556,-0.556},
	   {0.556,1.668,0.556},{0.556,1.668,-0.556},
	   {0.556,-1.668,0.556},{0.556,-1.668,-0.556},
	   {0.556,0.556,1.668},{0.556,0.556,-1.668},
	   {0.556,-0.556,1.668},{0.556,-0.556,-1.668},
	   {-0.556,1.668,0.556},{-0.556,1.668,-0.556},
	   {-0.556,-1.668,0.556},{-0.556,-1.668,-0.556},
	   {-0.556,0.556,1.668},{-0.556,0.556,-1.668},
	   {-0.556,-0.556,1.668},{-0.556,-0.556,-1.668}],
    Scale = 1.668*(L/2),
    Vs0 = [{X/Scale,Y/Scale,Z/Scale} || {X,Y,Z} <- Vs1],
    Vs = transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    build_shape("octotoad", Fs, Vs, St).

dodecahedron(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(dodecahedron,1,"Edge Length"), {text, 1.0,[{range,{0.0,infinity}}]}}
	 ]},
	 transform_obj_dlg()],
    ask(dodecahedron, Ask, Q, St);
dodecahedron([L|Transf], St) ->
	 Pn = sqrt(5.0),
	 Phi = (1.0 + Pn)/2.0,
	 Li = L/2.0 * Phi,
	 Ap = sqrt(2.0 / (3.0 + Pn)),
	 Alpha = Li*Ap,
    Beta = Li*(1.0 + sqrt(6.0 / (3.0 + Pn) - 2.0 + 2.0 * Ap)),
    Fs = [[0,1,9,16,5],[1,0,3,18,7],[1,7,11,10,9],[11,7,18,19,6],
	  [8,17,16,9,10],[2,14,15,6,19],[2,13,12,4,14],[2,19,18,3,13],
	  [3,0,5,12,13],[6,15,8,10,11],[4,17,8,15,14],[4,12,5,16,17]],
    Vs0 = [{-Alpha,0.0,Beta},{Alpha,0.0,Beta},{-Li,-Li,-Li},{-Li,-Li,Li},
	   {-Li,Li,-Li},{-Li,Li,Li},{Li,-Li,-Li},
	   {Li,-Li,Li},{Li,Li,-Li},
	   {Li,Li,Li},{Beta,Alpha,0.0},{Beta,-Alpha,0.0},{-Beta,Alpha,0.0},
	   {-Beta,-Alpha,0.0},{-Alpha,0.0,-Beta},{Alpha,0.0,-Beta},
	   {0.0,Beta,Alpha},{0.0,Beta,-Alpha},{0.0,-Beta,Alpha},
	   {0.0,-Beta,-Alpha}],
    Vs = transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    build_shape("dodecahedron", Fs, Vs, St).

icosahedron(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(icosahedron,1,"Edge Length"), {text,2.0,[{range,{0.0,infinity}}]}}
	 ]},
	 transform_obj_dlg()],
    ask(icosahedron, Ask, Q, St);
icosahedron([S|Transf], St) ->
    T2 = pi()/10.0,
    T4 = 2.0 * T2,
    CT4 = cos(T4),
    R  = (S/2.0) / sin(T4),
    H  = CT4 * R,
    Cx = R * cos(T2),
    Cy = R * sin(T2),
    H1 = sqrt(S * S - R * R),
    H2 = abs(R) * sqrt(1.0 + 2.0*CT4),
    Z2 = (H2 - H1) / 2.0,
    Z1 = Z2 + H1,

    Fs = [[0,2,1],[0,3,2],[0,4,3],[0,5,4],[0,1,5],
	  [4,7,6],[2,9,1],[5,8,7],[3,10,2],[9,8,1],
	  [4,6,3],[10,9,2],[7,4,5],[6,10,3],[1,8,5],
	  [11,10,6],[11,6,7],[11,7,8],[11,8,9],[11,9,10]],

    Vs0 =[{0.0,  Z1, 0.0},
      {R,    Z2, 0.0	},
      {Cy,   Z2, Cx	},
      {-H,   Z2, S/2.0	},
      {-H,   Z2, -S/2.0	},
      {Cy,   Z2, -Cx 	},
      {-R,  -Z2, 0.0	},
      {-Cy, -Z2, -Cx	},
      { H,  -Z2, -S/2.0	},
      { H,  -Z2, S/2.0	},
      {-Cy, -Z2, Cx	},
      {0.0, -Z1, 0.0	}],
    Vs = transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    build_shape("icosahedron", Fs, Vs, St).

circle(N, Y, R) ->
    Delta = pi()*2 / N,
    [{R*cos(I*Delta), Y, R*sin(I*Delta)} || I <- lists:seq(0, N-1)].

ellipse(N, Y, {R1, R2}) ->
    Delta = pi()*2 / N,
    [{R1*cos(I*Delta), Y, R2*sin(I*Delta)} || I <- lists:seq(0, N-1)].

cone(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(cone,1,"Sections"), {text,16,[{range,{3,infinity}}]}},
	    {?STR(cone,2,"Height"), {text,2.0,[{range,{0.0,infinity}}]}},
	    {?STR(cone,3,"X Diameter"), {text,2.0,[{range,{0.0,infinity}}]}},
	    {?STR(cone,5,"Z Diameter"), {text,2.0,[{range,{0.0,infinity}}]}}
	 ]},
	 transform_obj_dlg()],
    ask(cone, Ask, Q, St);
cone([N,H,Dx,Dy|Transf], St) ->
    Hi = H/2.0,
	 Di = Dx/2.0,
	 Dj = Dy/2.0,
	 Ns = lists:seq(0, N-1),
    Lower = lists:seq(0, N-1),
    C = ellipse(N, -Hi, {Di, Dj}),
    Vs0 = C ++ [{0.0,Hi,0.0}],
    Vs = transform_obj(Transf, Vs0),
    Sides = [[N, (I+1) rem N, I] || I <- Ns],
    Fs = [Lower | Sides],
    %% The string below is intentionally not translated.
    build_shape("cone", Fs, Vs, St).
    
sphere_circles(Ns, Nl, Xi, Yi) ->
    Delta = pi() / Nl,
    PosAndRads= [{cos(I*Delta), sin(I*Delta)} || I <- lists:seq(1, Nl-1)],
    Circles = [circle(Ns, Pos*Xi, Rad*Yi) || {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

sphere_faces(Ns, Nl) ->
    Lasti= (Nl-1)*Ns,
    Topi= Lasti,
    Boti= Topi+1,
    Topf= [[Topi, (I+1) rem Ns, I]
	   || I <- lists:seq(0, Ns-1)],
    Botf= [[Boti, Lasti - Ns + I, Lasti - Ns + (I+1) rem Ns]
	   || I <- lists:seq(0, Ns-1)],
    Slices= [ [ [(I+1) rem Ns  +J*Ns,
		 (I+1) rem Ns  +J*Ns +Ns,
		 I             +J*Ns +Ns,
		 I             +J*Ns]
		|| I <- lists:seq(0, Ns-1)]
	      || J <- lists:seq(0, Nl-3)],
    Topf ++ Botf ++ lists:append(Slices).

sphere(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(sphere,1,"Sections"), {text,16,[{range,{3,infinity}}]}},
	    {?STR(sphere,2,"Slices"), {text,8,[{range,{3,infinity}}]}},
	    {?STR(sphere,3,"X Radial"), {text,2.0,[{range,{0.0,infinity}}]}},
	    {?STR(sphere,4,"Y Radial"), {text,2.0,[{range,{0.0,infinity}}]}}
    	 ]},
	 transform_obj_dlg()],
    ask(sphere, Ask, Q, St);
sphere([Ns,Nl,Xr,Yr|Transf], St) ->
    Xi = Xr/2.0,
    Yi = Yr/2.0,
    Fs = sphere_faces(Ns, Nl),
    Vs0 = sphere_circles(Ns, Nl, Xi, Yi) ++ [{0.0, Xi, 0.0}, {0.0, -Xi, 0.0}],
    Vs = transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    build_shape("sphere", Fs, Vs, St).

grid(Ask, St) when is_atom(Ask) ->
    Q = [{label_column, [
	    {?STR(grid,1,"Rows/Cols"), {text,10,[{range,{1,infinity}}]}},
	    {?STR(grid,2,"X Spacing"), {text,0.5,[{range,{0.0,infinity}}]}},
	    {?STR(grid,3,"Z Spacing"), {text,0.5,[{range,{0.0,infinity}}]}},
	    {?STR(grid,4,"Height"), {text,0.1,[{range,{0.0,infinity}}]}}
    	 ]},
	 transform_obj_dlg()],
    ask(grid, Ask, Q, St);
grid([Rc,Sp,Zp,Ht|Transf], St) ->
    Vs0 = grid_vertices(Rc,Sp,Zp,Ht),
    Vs = transform_obj(Transf, Vs0),
    Fs = grid_faces(Rc),
    %% The string below is intentionally not translated.
    build_shape("grid", Fs, Vs, St).

grid_vertices(Rc,Sp,Zp,Ht) ->
    {Low,High} = case Rc rem 2 of
		     0 -> {-Rc,Rc};
		     1 -> {-Rc,Rc}
		 end,
    TopSeq = seq(Low, High, 2),
    BotSeq = [Low,High],
    VsBot = [{I*Sp/2,-Ht,J*Zp/2} || J <- BotSeq, I <- BotSeq],
    [{I*Sp/2,Ht,J*Zp/2} || J <- TopSeq, I <- TopSeq] ++ VsBot.

grid_faces(Rc) ->
    TopSeq = seq(0, Rc-1),
    Rsz = Rc+1,
    TopFs = [grid_face(I, J, Rsz) || I <- TopSeq, J <- TopSeq],
    ULv = Rsz*Rsz,
    Fs0 = [[ULv,ULv+1,ULv+3,ULv+2]|TopFs],	%Add bottom.
    Fs1 = [[ULv+1,ULv|seq(0, Rsz-1)]|Fs0],	%North
    Fs2 = [[ULv,ULv+2|seq(Rsz*Rsz-Rsz, 0, -Rsz)]|Fs1], %West
    Fs = [[ULv+2,ULv+3|seq(Rsz*Rsz-1, Rsz*Rsz-Rsz, -1)]|Fs2], % South.
    [[ULv+3,ULv+1|seq(Rsz-1, Rsz*Rsz-1, Rsz)]|Fs]. %East

grid_face(I, J, Rsz) ->
    [Rsz*J+I+1,   Rsz*J+I,
     Rsz*(J+1)+I, Rsz*(J+1)+I+1].

ask(Shape, Bool, Qs, St) ->
    Title = prim_help(Shape),
    wings_dialog:dialog_preview({shape,Shape}, Bool, Title, Qs, St).

transform_obj_dlg() ->
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

transform_obj([{_,Rot_X},{_,Rot_Y},{_,Rot_Z},{_,Mov_X},{_,Mov_Y},{_,Mov_Z},{_,Ground}], Vs) ->
    transform_obj({Rot_X,Rot_Y,Rot_Z}, {Mov_X,Mov_Y,Mov_Z}, Ground, Vs).
transform_obj({0.0,0.0,0.0},{0.0,0.0,0.0},false, Vs) -> Vs;
transform_obj({Rot_X,Rot_Y,Rot_Z}, {Mov_X,Mov_Y,Mov_Z}, Ground, Vs) ->
    MrX = e3d_mat:rotate(Rot_X, {1.0,0.0,0.0}),
    MrY = e3d_mat:rotate(Rot_Y, {0.0,1.0,0.0}),
    MrZ = e3d_mat:rotate(Rot_Z, {0.0,0.0,1.0}),
    Mr = e3d_mat:mul(MrZ, e3d_mat:mul(MrY, MrX)),
    Y = case  Ground of
	    true ->
		{{_,Y1,_},{_,Y2,_}} = e3d_bv:box(Vs),
		min(Y1,Y2)*-1.0;
	    false -> Mov_Y
	end,
    Mt = e3d_mat:translate(Mov_X,Y,Mov_Z),
    Mat = e3d_mat:mul(Mt,Mr),
    [e3d_mat:mul_point(Mat, V) || V <- Vs].
