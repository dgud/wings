%%
%%  wings_move.erl --
%%
%%     This module implements the Move command.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_move).
-export([setup/2,setup_we/4,plus_minus/3,magnet_move_fun/3]).

-include("wings.hrl").
-import(lists, [foldl/3,sort/1]).

setup({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun setup/2);
setup({Vec,Magnet}, St) ->
    setup(Vec, Magnet, St);
setup(Vec, St) ->
    setup(Vec, none, St).

setup(Type, _Magnet, #st{selmode=body,sel=Sel}=St) ->
    Vec = make_vector(Type),
    Fun = translate_fun(Vec),
    Ids = [{Id,Fun} || {Id,_} <- Sel],
    wings_drag:setup({matrix,Ids}, unit(Type), flags(Type), St);
setup(Vec0, Magnet, #st{selmode=Mode}=St) ->
    Vec = make_vector(Vec0),
    Tvs = wings_sel:fold(
	    fun(Items, We, Acc) ->
		    Tv = setup_we(Mode, Vec, Items, We),
		    magnet_move(Tv, Vec, Magnet, We, Acc)
	    end, [], St),
    Flags = wings_magnet:flags(Magnet, flags(Vec0)),
    wings_drag:setup(Tvs, unit(Vec0, magnet_unit(Magnet)), Flags, St).

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

plus_minus({'ASK',Ask}, Tvs, St0) ->
    wings:ask(Ask, St0, fun(Type, St) -> plus_minus(Type, Tvs, St) end);
plus_minus(Type, Tvs0, #st{selmode=Mode}=St) ->
    Vec = make_vector(Type),
    Tvs = plus_minus_2(Mode, Vec, Tvs0, []),
    Flags = [flags(Type),{initial,[0.0,0.0,1.0]}],
    wings_drag:setup(Tvs, unit(Type, [{percent,{0.0,infinity}}]), Flags, St).

plus_minus_2(Mode, Vec, [{Items,NewVs,Forbidden,We}|T], Acc0) ->
    Tv = setup_we(Mode, Vec, Items, We),
    Acc = plus_minus_3(Tv, NewVs, Forbidden, We, Acc0),
    plus_minus_2(Mode, Vec, T, Acc);
plus_minus_2(_Mode, _Vec, [], Acc) -> Acc.

plus_minus_3(Tv0, NewVs, Forbidden, #we{id=Id}=We, Acc) ->
    Affected0 = affected(Tv0),
    Vecs = move_vectors(NewVs, Forbidden, gb_sets:from_list(Affected0), We, []),
    Affected = [V || {V,_,_} <- Vecs],
    VsPos = move_away(1.0, Vecs, []),
    MoveAway = {Affected,move_away_fun(Vecs, VsPos)},
    [{Id,Tv0},{Id,MoveAway}|Acc].

setup_we(Mode, Vec, _, We) when ?IS_LIGHT(We) ->
    setup_we_1(Mode, Vec, wings_sel:get_all_items(Mode, We), We);
setup_we(Mode, Vec, Items, We) ->
    setup_we_1(Mode, Vec, Items, We).

setup_we_1(Mode, Vec, Items, We) when not is_list(Items) ->
    setup_we_1(Mode, Vec, gb_sets:to_list(Items), We);
setup_we_1(vertex, Vec, Items, We) ->
    vertices_to_vertices(Items, We, Vec);
setup_we_1(edge, Vec, Items, We) ->
    edges_to_vertices(Items, We, Vec);
setup_we_1(face, Vec, Items, We) ->
    faces_to_vertices(Items, We, Vec).

unit(Type) ->
    unit(Type, []).

unit(free, T) -> [dx,dy,dz|T];
unit(free_2d, T) -> [dx,dy|T];
unit(intrude, T) -> [{distance,{0.0,9.9E307}}|T];
unit(_, T) -> [distance,skip|T].

flags(free) -> [screen_relative];
flags(free_2d) -> [screen_relative];
flags(_) -> [].

move_away_fun(Tv, VsPos0) ->
    fun(view_changed, _Acc) ->
	    move_away_fun(Tv, VsPos0);
       ([_,_,Percent|_], A) ->
	    move_away(Percent, Tv, A)
    end.

move_away(R0, Tv, Acc) ->
    R = R0-1.0,
    foldl(fun({V,Vec,{X,Y,Z}}, A) -> 
		  {Xt,Yt,Zt} = e3d_vec:mul(Vec, R),
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Pos}|A]
	  end, Acc, Tv).
    
move_vectors([V|Vs], Forbidden, VsSet, #we{vp=Vtab}=We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(Edge, _, Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, VsSet) andalso
			not gb_sets:is_member(Edge, Forbidden) of
			false -> A;
			true ->
			    Pa = array:get(OtherV, Vtab),
			    Pb = array:get(V, Vtab),
			    Vec = e3d_vec:sub(Pb, Pa),
			    [{V,Vec,Pb}|A]
		    end
	    end, Acc0, V, We),
    move_vectors(Vs, Forbidden, VsSet, We, Acc);
move_vectors([], _, _, _, Acc) -> Acc.

%%
%% Conversion of vertex selection to vertices. :-)
%% Not entirely pointless, as we'll need to add vectors for
%% the points (vertices).
%%

vertices_to_vertices(Vs, We, normal) -> vertex_normals(We, Vs);
vertices_to_vertices(Vs, We, Vec) -> make_tvs(Vs, Vec, We).

vertex_normals(We, Vs) ->
    foldl(fun(V, Acc) ->
		  Vec = wings_vertex:normal(V, We),
		  [{Vec,[V]}|Acc]
	  end, [], Vs).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Es, We, normal) ->
    #we{es=Etab,vp=Vtab} = We,
    Vs = foldl(fun(Edge, D0) ->
		       #edge{vs=Va,ve=Vb,lf=FaceL,rf=FaceR} =
			   array:get(Edge, Etab),
		       VaPos = array:get(Va, Vtab),
		       VbPos = array:get(Vb, Vtab),
		       EdgeDir = e3d_vec:norm_sub(VbPos, VaPos),
 		       NL = wings_face:normal(FaceL, We),
 		       NR = wings_face:normal(FaceR, We),
		       Normal = e3d_vec:norm(e3d_vec:add(NL, NR)),
		       [{Va,{Normal,VaPos,EdgeDir}},
			{Vb,{Normal,VbPos,e3d_vec:neg(EdgeDir)}}|D0]
	       end, [], Es),
    average(Vs);
edges_to_vertices(Es, We, Vec) ->
    make_tvs(wings_edge:to_vertices(Es, We), Vec, We).

average(Vs) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun average/2, [], sofs:to_external(F)).

average({V,Info}, Acc) ->
    Normal = average_normals(Info),
    [{Normal,[V]}|Acc].

average_normals([{Normal,_,_}]) -> Normal;
average_normals([{Na,Orig,Da}|[{Nb,_,Db}|_]=T]) ->
    %% This code is probably obvious. :-)
    Oa = e3d_vec:add(Orig, Na),
    Ob = e3d_vec:add(Orig, Nb),
    Diff = e3d_vec:sub(Oa, Ob),
    A = e3d_vec:dot(Da, Da),
    B = -e3d_vec:dot(Da, Db),
    C = e3d_vec:dot(Db, Db),
    D = e3d_vec:dot(Da, Diff),
    Det = A*C-B*D,
    if
	Det*Det >= 1.0E-9*abs(A*B) ->
	    E = -e3d_vec:dot(Db, Diff),
	    S = (B*E-C*D)/Det,
	    e3d_vec:add_prod(Na, Da, S);
	true ->					%Parallel edges
	    average_normals(T)
    end.

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Faces, #we{vp=Vtab}=We, normal) ->
    Vs = foldl(fun(Face, Acc0) ->
		       Vs = wings_face:vertices_cw(Face, We),
		       face_normal(Vs, Vtab, Acc0)
	       end, [], Faces),
    face_average(Vs, Vtab);
faces_to_vertices(Faces, We, Vec) ->
    make_tvs(wings_face:to_vertices(Faces, We), Vec, We).

face_normal(Vs, Vtab, Acc) ->
    Normal = wings_face:face_normal_cw(Vs, Vtab),
    foldl(fun(V, A) -> [{V,Normal}|A] end, Acc, Vs).

face_average(Vs, Vtab) ->
    R = sofs:relation(Vs),
    F = sofs:relation_to_family(R),
    foldl(fun({V,Ns0}, Acc) ->
		  Ns = filter_normals(Ns0),
		  N = face_average_normals(V, Ns, Vtab),
		  [{N,[V]}|Acc]
	  end, [], sofs:to_external(F)).

face_average_normals(_V, [Na], _Vtab) -> Na;
face_average_normals(_V, [Na,Nb], _Vtab) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    case e3d_vec:dot(N, Na) of
	Dot when abs(Dot) < 1.0E-6 ->
	    e3d_vec:add(Na, Nb);
	Dot ->
	    e3d_vec:divide(N, Dot)
    end;
face_average_normals(V, [Na,Nb,Nc], Vtab) ->
    %% The caller assures that the normals are not co-linear.
    Vpos = wings_vertex:pos(V, Vtab),
    Nao = e3d_vec:add(Vpos, Na),
    Nbo = e3d_vec:add(Vpos, Nb),
    Nco = e3d_vec:add(Vpos, Nc),
    {A,D,G} = Na,
    {B,E,H} = Nb,
    {C,F,I} = Nc,
    J = e3d_vec:dot(Nao, Na),
    K = e3d_vec:dot(Nbo, Nb),
    L = e3d_vec:dot(Nco, Nc),

    %% Calculate intersection of three planes using Cramer's rule.
    if
	is_float(A), is_float(B), is_float(C),
	is_float(D), is_float(E), is_float(F),
	is_float(G), is_float(H), is_float(I),
	is_float(J), is_float(K), is_float(L) ->
	    EiMinusHf = E*I - H*F,
	    GFMinusDI = G*F - D*I,
	    DHMinusEG = D*H - E*G,
	    JCMinusAL = J*C - A*L,
	    AKMinusJB = A*K - J*B,
	    BLMinusKC = B*L - K*C,
	    case A*EiMinusHf + B*GFMinusDI + C*DHMinusEG of
		M when abs(M) < 0.0001 ->
		    %% Should not happen, but just in case...
		    face_average_normals(V, [Na,Nb], Vtab);
		M ->
		    X = (J*EiMinusHf + K*GFMinusDI + L*DHMinusEG)/M,
		    Y = (I*AKMinusJB + H*JCMinusAL + G*BLMinusKC)/M,
		    Z = -(F*AKMinusJB + E*JCMinusAL + D*BLMinusKC)/M,
		    e3d_vec:sub({X,Y,Z}, Vpos)
	    end
    end.

%% Filter out normals that are too close to each other.
filter_normals([_]=Ns) -> Ns;
filter_normals([_,_]=Ns) -> Ns;
filter_normals([Na|[_|_]=Ns]) ->
    %% Three or more normals. We want three normals that point
    %% in as different directions as possible, or just two normals
    %% if we can't find three different enough.
    Nb = largest_angle(Ns, Na),
    N1 = e3d_vec:cross(Na, Nb),
    case smallest_angle(Ns, N1) of
	{_Nc,Dot} when Dot < 0.01 ->
	    %% The third normal is not usable. It is too close to
	    %% the plane of the other two.
	    [Na,Nb];
	{Nc,_Dot} ->
	    %% The third normal is OK.
	    [Na,Nb,Nc]
    end.

%% Find the normal with the greatest angle from Na.
largest_angle([N|Ns], Na) ->
    Dot = abs(e3d_vec:dot(Na, N)),
    largest_angle(Ns, Na, Dot, N).
    
largest_angle([N|Ns], Na, OldDot, OldN) ->
    case abs(e3d_vec:dot(Na, N)) of
	Dot when Dot < OldDot ->
	    largest_angle(Ns, Na, Dot, N);
	_Dot ->
	    largest_angle(Ns, Na, OldDot, OldN)
    end;
largest_angle([], _Na, _Dot, N) -> N.

%% Find the normal with the smallest angle from Na.
smallest_angle([N|Ns], Na) ->
    Dot = abs(e3d_vec:dot(Na, N)),
    smallest_angle(Ns, Na, Dot, N).
    
smallest_angle([N|Ns], Na, OldDot, OldN) ->
    case abs(e3d_vec:dot(Na, N)) of
	Dot when Dot > OldDot ->
	    smallest_angle(Ns, Na, Dot, N);
	_Dot ->
	    smallest_angle(Ns, Na, OldDot, OldN)
    end;
smallest_angle([], _Na, Dot, N) -> {N,Dot}.

%%
%% Conversion of body selections (entire objects) to vertices.
%%

translate_fun(Free) when Free == free; Free == free_2d ->
    fun(Matrix0, [Dx,Dy,Dz|_]) ->
	    #view{azimuth=Az,elevation=El} = wings_view:current(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
	    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
	    {Xt,Yt,Zt} = e3d_mat:mul_point(M1, {Dx,Dy,-Dz}),
	    e3d_mat:translate(Xt, Yt, Zt);
       (Matrix0, [Dx,Dy|_]) ->
	    #view{azimuth=Az,elevation=El} = wings_view:current(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
	    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
	    {Xt,Yt,Zt} = e3d_mat:mul_point(M1, {Dx,Dy,0.0}),
	    e3d_mat:translate(Xt, Yt, Zt)
    end;
translate_fun({Xt0,Yt0,Zt0}) ->
    fun(_Matrix0, [Dx|_]) when is_float(Dx) ->
	    Xt = Xt0*Dx,
	    Yt = Yt0*Dx,
	    Zt = Zt0*Dx,
	    e3d_mat:translate(Xt, Yt, Zt)
    end.

%%%
%%% Magnet move.
%%%

magnet_move(Tv, _Vec, none, #we{id=Id}, Acc) -> [{Id,Tv}|Acc];
magnet_move(Tv, Vec0, Magnet0, #we{id=Id}=We, Acc) ->
    Vs = affected(Tv),
    {VsInf,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs, We),
    Vec = magnet_vec(Vec0, Affected, We),
    [{Id,{Affected,magnet_move_fun(Vec, VsInf, Magnet)}}|Acc].

magnet_vec(normal, Vs, We) ->
    VsVec = [{V,Vec} || {Vec,[V]} <- vertex_normals(We, Vs)],
    gb_trees:from_orddict(sort(VsVec));
magnet_vec(Vec, _, _) -> Vec.

magnet_move_fun({Xt0,Yt0,Zt0}=Vec, VsInf0, {_,R}=Magnet0) ->
    fun(new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_move_fun(Vec, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_move_fun(Vec, VsInf, Magnet);
       ([Dx0|_], A0) ->
	    foldl(fun({V,{Px,Py,Pz},_,Inf}, A) ->
			  Dx = Dx0*Inf,
			  Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
			  Pos = wings_util:share(Px+Xt, Py+Yt, Pz+Zt),
			  [{V,Pos}|A]
		  end, A0, VsInf0)
    end;
magnet_move_fun(free, VsInf0, {_,R}=Magnet0) ->
    fun(view_changed, We) ->
	    VsInf = wings_util:update_vpos(VsInf0, We),
	    magnet_move_fun(free, VsInf, Magnet0);
       (new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_move_fun(free, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_move_fun(free, VsInf, Magnet);
       ([Dx,Dy,Dz|_], Acc) ->
	    M = view_matrix(),
	    foldl(
	      fun({V,{Px,Py,Pz},_,Inf}, A) ->
		      {Xt,Yt,Zt} = e3d_mat:mul_point(M, {Dx*Inf,Dy*Inf,-Dz*Inf}),
		      Pos = wings_util:share(Px+Xt, Py+Yt, Pz+Zt),
		      [{V,Pos}|A]
	      end, Acc, VsInf0)
    end;
magnet_move_fun(free_2d, VsInf0, {_,R}=Magnet0) ->
    fun(view_changed, We) ->
	    VsInf = wings_util:update_vpos(VsInf0, We),
	    magnet_move_fun(free_2d, VsInf, Magnet0);
       (new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_move_fun(free_2d, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_move_fun(free_2d, VsInf, Magnet);
       ([Dx,Dy,_|_], Acc) ->
	    M = view_matrix(),
	    foldl(
	      fun({V,{Px,Py,Pz},_,Inf}, A) ->
		      {Xt,Yt,_} = e3d_mat:mul_point(M, {Dx*Inf,Dy*Inf,0.0}),
		      Pos = wings_util:share(Px+Xt, Py+Yt, Pz),
		      [{V,Pos}|A]
	      end, Acc, VsInf0)
    end;
magnet_move_fun(VsVec, VsInf0, {_,R}=Magnet0) ->
    %% Move each element along its own normal.
    fun(new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_move_fun(VsVec, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_move_fun(VsVec, VsInf, Magnet);
       ([Dx0|_], A0) ->
	    foldl(fun({V,{Px,Py,Pz},_,Inf}, A) ->
			  {Xt0,Yt0,Zt0} = gb_trees:get(V, VsVec),
			  Dx = Dx0*Inf,
			  Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
			  Pos = wings_util:share(Px+Xt, Py+Yt, Pz+Zt),
			  [{V,Pos}|A]
		  end, A0, VsInf0)
    end.

%%%
%%% Utilities.
%%%

affected([_|_]=Tv) -> lists:append([Vs || {_,Vs} <- Tv]);
affected({Vs,Fun}) when is_function(Fun) -> Vs.
    
make_tvs(Vs, free_2d, We) ->
    make_tvs(Vs, free, We);
make_tvs(Vs, free, We) ->
    VsPos = wings_util:add_vpos(Vs, We),
    {Vs,move_fun(VsPos, view_matrix())};
make_tvs(Vs, Vec, _We) -> [{Vec,Vs}].

move_fun(VsPos, ViewMatrix) ->
    fun(view_changed, NewWe) ->
	    move_fun(wings_util:update_vpos(VsPos, NewWe), view_matrix());
       ([Dx,Dy,Dz|_], Acc) ->
	    {Xt,Yt,Zt} = e3d_mat:mul_point(ViewMatrix, {Dx,Dy,-Dz}),
	    foldl(fun({V,{X,Y,Z}}, A) -> 
			  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
			  [{V,Pos}|A]
		  end, Acc, VsPos);
       ([Dx,Dy|_], Acc) ->
	    {Xt,Yt,_} = e3d_mat:mul_point(ViewMatrix, {Dx,Dy,0.0}),
	    foldl(fun({V,{X,Y,Z}}, A) -> 
			  Pos = wings_util:share(X+Xt, Y+Yt, Z),
			  [{V,Pos}|A]
		  end, Acc, VsPos)
    end.

view_matrix() ->
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    e3d_mat:mul(M, e3d_mat:rotate(-El, {1.0,0.0,0.0})).

make_vector(free_2d) -> free_2d;
make_vector(Vec) -> wings_util:make_vector(Vec).
