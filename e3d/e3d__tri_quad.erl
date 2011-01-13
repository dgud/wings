%%
%%  e3d__tri_quad.erl --
%%
%%     Triangulates and quadrangulates meshes.
%%
%%  Copyright (c) 2001-2002 Howard Trickey
%%	          2003-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d__tri_quad.erl,v 1.17 2005/03/13 18:23:41 bjorng Exp $
%%

-module(e3d__tri_quad).
-export([triangulate/1,triangulate_face/2,triangulate_face/3,
	 triangulate_face_with_holes/3,
	 quadrangulate/1,quadrangulate_face/2,quadrangulate_face_with_holes/3]).

%-define(TESTING, true).  % qqq

-ifdef(TESTING).
-export([test_tri/1,test_quad/1]).
-endif.

-include("e3d.hrl").

-import(lists, [reverse/1,map/2,seq/2,sort/2,foldl/3,
		sublist/3,delete/2,nth/2]).

-define(ANGFAC, 1.0).
-define(DEGFAC, 10.0).
-define(GTHRESH, 75).
-define(TOL, 0.0000001).

						% Triangulate an entire mesh.
triangulate(#e3d_mesh{type=triangle}=Mesh) -> Mesh;
triangulate(#e3d_mesh{type=polygon,fs=Fs0,vs=Vs}=Mesh) ->
    Fs = triangulate(Fs0, list_to_tuple(Vs), []),
    Mesh#e3d_mesh{type=triangle,fs=Fs,ns=[]}.

triangulate([#e3d_face{vs=[_,_,_]}=FaceRec|Ps], Vtab, Acc) ->
    triangulate(Ps, Vtab, [FaceRec|Acc]);
triangulate([#e3d_face{vs=Vs0,vc=VCol0,tx=Tx0}=FaceRec0|Ps], Vtab, Acc0) ->
    Vs = seq(0, length(Vs0)-1),
    TempVtab = [element(V+1, Vtab) || V <- Vs0],
    FaceRec = FaceRec0#e3d_face{vs=Vs},
    Tris = triangulate_face(FaceRec, TempVtab),
    Acc = renumber_result(Tris, list_to_tuple(Vs0), list_to_tuple(VCol0),
			  list_to_tuple(Tx0), Acc0),
    triangulate(Ps, Vtab, Acc);
triangulate([], _, Acc) -> reverse(Acc).

renumber_result([#e3d_face{vs=[Va,Vb,Vc]}=Rec|Tris], OrigNum, OrigVCol, OrigTx, 
		Acc) ->
    Vs = renumber_one(Va, Vb, Vc, OrigNum),
    VCol = renumber_one(Va, Vb, Vc, OrigVCol),
    Tx = renumber_one(Va, Vb, Vc, OrigTx),
    renumber_result(Tris, OrigNum, OrigVCol, OrigTx, 
		    [Rec#e3d_face{vs=Vs,vc=VCol,tx=Tx}|Acc]);
renumber_result([], _, _, _, Acc) -> Acc.

renumber_one(_Va, _Vb, _Vc, {}) -> [];
renumber_one(Va, Vb, Vc, Orig) ->
    [element(Va+1, Orig),element(Vb+1, Orig),element(Vc+1, Orig)].

%% Quadrangulate an entire mesh. (Not optimized yet; slow on large meshes.)
quadrangulate(#e3d_mesh{type=quad}=Mesh) -> Mesh;
quadrangulate(#e3d_mesh{fs=Fs0,vs=Vs}=Mesh) ->
    Fs = quadrangulate_1(Fs0, Vs, []),
    Mesh#e3d_mesh{type=quad,fs=Fs,ns=[]}.

quadrangulate_1([FaceRec|Ps], Vtab, Acc) ->
    Faces = quadrangulate_face(FaceRec, Vtab),
    quadrangulate_1(Ps, Vtab, Faces++Acc);
quadrangulate_1([], _, Acc) -> reverse(Acc).

%% Vcoords is list of vertex coordinates.
%% Returns list of (Triangular) faces to replace Face.
triangulate_face(#e3d_face{vs=Vs}=Face, Vcoords) ->
    Vtab = rotate_normal_to_z(Vs, Vcoords),
    Tris = triface(Vs, Vtab),
    Bord = border_edges([Vs]),
    Triscdt = cdt(Tris, Bord, Vtab),
    to_faces_new(Triscdt, Bord, Face).

triangulate_face(#e3d_face{vs=Vs}=Face, N, Vcoords) ->
    Vtab = rot_normal_to_z(N, Vcoords),
    Tris = triface(Vs, Vtab),
    Bord = border_edges([Vs]),
    Triscdt = cdt(Tris, Bord, Vtab),
    to_faces_new(Triscdt, Bord, Face).

%% Like triangulate, but Holes is list of e3d_faces
%% containing Clockwise-oriented holes inside CCW-oriented Face.
triangulate_face_with_holes(#e3d_face{vs=Vs}=Face, Holes, Vcoords) ->
    Vtab = rotate_normal_to_z(Vs, Vcoords),
    Holes1 = map(fun (H) -> sortface(H, Vtab) end, Holes),
    #e3d_face{vs=Vsjoined} = joinislands(Face, Holes1, Vtab),
    Tris = triface(Vsjoined, Vtab),
    Bord = border_edges([Vs | map(fun (#e3d_face{vs=Hs}) -> Hs end, Holes)]),
    Triscdt = cdt(Tris, Bord, Vtab),
    to_faces_new(Triscdt, Bord, Face).

quadrangulate_face(#e3d_face{vs=Vs}=Face, Vcoords) ->
    Vtab = rotate_normal_to_z(Vs, Vcoords),
    Tris = triface(Vs, Vtab),
    Bord = border_edges([Vs]),
    Triscdt = cdt(Tris, Bord, Vtab),
    Qs = quadrangulate(Triscdt, Bord, Vtab),
    to_faces_new(Qs, Bord, Face).

quadrangulate_face_with_holes(#e3d_face{vs=Vs}=Face, Holes, Vcoords) ->
    Vtab = rotate_normal_to_z(Vs, Vcoords),
    Holes1 = map(fun (H) -> sortface(H, Vtab) end, Holes),
    #e3d_face{vs=Vsjoined} = joinislands(Face, Holes1, Vtab),
    Tris = triface(Vsjoined, Vtab),
    Bord = border_edges([Vs | map(fun (#e3d_face{vs=Hs}) -> Hs end, Holes)]),
    Triscdt = cdt(Tris, Bord, Vtab),
    Qs = quadrangulate(Triscdt, Bord, Vtab),
    to_faces_new(Qs, Bord, Face).

%% Fl is list of tuples (should be 3-tuples, but might be smaller
%% if original face was smaller).
%% Bord is gb_sets set of border edges (2-tuples).
%% Mat is material of original face.
%% Return list e3d_faces.
%% Assume original border had all visible edges.
%% Note: Texture coordinates and vertex colors are handled in
%% another place in this file.
to_faces_new(Fl, Bord, Face) ->
    [to_face(Ftup, Bord, Face) || Ftup <- Fl].

to_face(Ftup, Bord, #e3d_face{ns=Ns0,mat=Mat}=Face) ->
    Vis = case Ftup of
	      {A,B,C} ->
		  vismask(A, B, Bord, 4) bor
		      vismask(B, C, Bord, 2) bor
		      vismask(C, A, Bord, 1);
	      _ -> -1
	  end,
    Ns = kill_ns(Ns0),
    Face#e3d_face{vs=tuple_to_list(Ftup),tx=[],vc=[],
		  ns=Ns,mat=Mat,vis=Vis}.

%% The 3ds format stores the smoothing group bits here. Preserve them.
%% Kill any normals.
kill_ns(SmoothGroup) when is_integer(SmoothGroup) -> SmoothGroup;
kill_ns(_) -> [].
        
vismask(A, B, Bord, Bit) ->
    case gb_sets:is_member({A,B}, Bord) of
	true -> Bit;
	_ -> 0
    end.

triface(Fl,Vtab) -> 
    %% We should have a qriteria for the start postion
    %% to get a uniform triangulation, if we triangulates
    %% a grid of squares for example.
    %% I have choosen the least vertex pos in 3d space of the face.

    Start = get_least_index(Fl ,Vtab),
%    Start = 1, % qqq
    Res = triface(Fl,Vtab,Start,1,[]),
%    erlang:display(Res), %% qqq
    Res.

get_least_index([F|Fl], Vtab) ->
    Best = element(F+1,Vtab),
    get_least_index(Fl, Vtab, Best, 1, 2).

get_least_index([], _, _, I, _) -> I;    
get_least_index([H|R],Vtab, Curr,I,N) -> 
    case element(H+1, Vtab) of
	Pos when Pos < Curr ->
	    get_least_index(R, Vtab, Pos, N, N+1);
	_ ->
	    get_least_index(R, Vtab, Curr,I, N+1)
    end.

triface(Fl,Vtab,Start,Incr,Acc) ->
    F = list_to_tuple(Fl),
    N = size(F),
    if
	N =< 3 -> [F | Acc];
	true ->
	    I = findear(F, N, Start, Incr, Vtab),
	    Im1 = windex(I-1,N),
	    I1 = windex(I+1,N),
	    Vm1 = element(Im1,F),
	    V0 = element(I,F),
	    V1 = element(I1,F),
%	    io:format("new tri: ~p ~p ~p~n", [Vm1,V0,V1]), %qqq
	    Fl1 = chopear(F, N, I),
	    Incr1 = -Incr,
	    Start1 =
		case Incr1 of
		    1 -> windex(I, N-1);
		    -1 -> windex(I-1, N-1)
		end,
	    triface(Fl1, Vtab, Start1, Incr1, [{Vm1,V0,V1} | Acc])
    end.

%% Make list copy of tuple F, omitting I
chopear(F, N, I) -> chopear(1, F, N, I, []).

chopear(J, _F, N, _I, Acc) when J > N -> reverse(Acc);
chopear(J, F, N, I, Acc) when J == I -> chopear(J+1, F, N, I, Acc);
chopear(J, F, N, I, Acc) -> chopear(J+1, F, N, I, [element(J,F) | Acc]).

%% An ear if a polygon consists of three consecutive vertices
%% v(-1), v0, v1 such that v(-1) can connect to v1 without intersecting
%% the polygon.
%% F is tuple of size N of indices into Vtab.  Assume N > 3.
%% Tries finding an ear starting at index Start and moving
%% in direction Incr.  (We attempt to alternate directions, to find
%% "nice" triangulations for simple convex polygons.)
%% Returns index into F of V0 (will always find one, because uses
%% desperation mode if fails to find one according to above rule).
findear(F, N, Start, Incr, Vtab) ->
    Angk = classifyangles(F, N, Vtab),
    findearmodeloop(F, N, Start, Incr, Angk, Vtab, 0).

findearmodeloop(F, N, Start, Incr, Angk, Vtab, Mode) ->
    case tryfindear(F, N, Start, Incr, Start, Angk, Vtab, Mode) of
	{ear,I} -> I;
	_ -> findearmodeloop(F, N, Start, Incr, Angk, Vtab, Mode+1)
    end.

tryfindear(_, N, I, _, _, _, _, _) when I > N -> none;
tryfindear(F, N, I, Incr, Start, Angk, Vtab, Mode) ->
    case isear(F, I, N, Angk, Vtab, Mode) of
	true -> {ear, I};
	false ->
	    I1 = windex(I+Incr, N),
	    if
		I1 == Start -> none;
		true -> tryfindear(F, N, I1, Incr, Start, Angk, Vtab, Mode)
	    end
    end.

%% Return true, false depending on ear status of {F(I-1),F(I),F(I+1)}.
%% Mode is amount of desperation: 0 is normal mode,
%% Mode 1 allows degenerate triangles (with repeated vertices)
%% Mode 2 allows local self crossing (folded) ears
%% Mode 3 allows any convex vertex (should always be one)
%% Mode 4 allows anything (just to be sure loop terminates!)
isear(F, I, N, Angk, Vtab, Mode) ->
    K = element(I, Angk),
    Vm2 = welement(I-2, N, F),
    Vm1 = welement(I-1, N, F),
    V0 = element(I, F),
    V1 = welement(I+1, N, F),
    V2 = welement(I+2, N, F),
    if
	Vm1 == V0; V0 == V1 ->
	    (Mode > 0);
	true ->
	    B = (K == ang_convex orelse K == ang_tangential orelse K == ang_0),
	    C = incone(Vm1, V0, V1, V2, welement(I+1,N,Angk), Vtab)
		andalso incone(V1, Vm2, Vm1, V0, welement(I-1,N,Angk), Vtab),
	    case (B and C) of
		true -> earloop(F, 1, N, Angk, Vm1, V0, V1, Vtab);
		_ ->
		    case Mode of
			0 -> false;
			1 -> false;
			2 -> segsintersect(Vm2, Vm1, V0, V1, Vtab);
			3 -> B;
			_ -> true
		    end
	    end
    end.

earloop(_, J, N, _, _, _, _, _) when J > N -> true;
earloop(F, J, N, Angk, Vm1, V0, V1, Vtab) ->
    Fv = element(J, F),
    K = element(J, Angk),
    B = (K == ang_reflex orelse K == ang_360)
	andalso not(Fv == Vm1 orelse Fv == V0 orelse Fv == V1),
    case B of
	true ->
	    %% is Fv inside closure of triangle (Vm1,V0,V1)?
	    C = not(ccw(V0,Vm1,Fv,Vtab)
		    orelse ccw(Vm1,V1,Fv,Vtab)
		    orelse ccw(V1,V0,Fv,Vtab)),
	    %% PROBLEM: Fv could be all the way on the other side.
	    %% PARTIAL FIX: check seg intersections
	    %% BETTER FIX (TODO): preperturb coords so no crossings
	    Fvm1 = welement(J-1, N, F),
	    Fv1 = welement(J+1, N, F),
	    D = segsintersect(Fvm1, Fv, Vm1, V0, Vtab) orelse
		segsintersect(Fvm1, Fv, V0, V1, Vtab) orelse
		segsintersect(Fv, Fv1, Vm1, V0, Vtab) orelse
		segsintersect(Fv, Fv1, V0, V1, Vtab),
	    case C or D of
		true -> false;
		false -> earloop(F, J+1, N, Angk, Vm1, V0, V1, Vtab)
	    end;
	false -> earloop(F, J+1, N, Angk, Vm1, V0, V1, Vtab)
    end.

%% Return true if point with index Vtest is in Cone of points with
%% indices A, B, C, where angle ABC has AngleKind Bkind.
%% The Cone is the set of points "inside" the left face defined by
%% segments ab and bc, disregarding all other segments of polygon for
%% purposes of "inside" test.
incone(Vtest, A, B, C, Bkind, Vtab) ->
    if
	Bkind == ang_reflex; Bkind == ang_360 ->
	    case incone(Vtest, C, B, A, ang_convex, Vtab) of
		true -> false;
		false ->
		    not((not(ccw(B,A,Vtest,Vtab))
			 andalso not(ccw(B,Vtest,A,Vtab))
			 andalso ccw(B,A,Vtest,Vtab))
			orelse
			(not(ccw(B,C,Vtest,Vtab))
			 andalso not(ccw(B,Vtest,C,Vtab))
			 andalso ccw(B,A,Vtest,Vtab)))
	    end;
	true ->
	    ccw(A,B,Vtest,Vtab) andalso ccw(B,C,Vtest,Vtab)
    end.

joinislands(Face, [], _) -> Face;
joinislands(Face, Holes, Vtab) ->
    Hole = leftmostface(Holes, Vtab),
    Hrest = delete(Hole, Holes),
    Face1 = joinisland(Face, Hole, Vtab),
    joinislands(Face1, Hrest, Vtab).

joinisland(#e3d_face{vs=Vs}=Face, #e3d_face{vs=[Hv|_]=Hvs}, Vtab) ->
    F = list_to_tuple(Vs),
    N = size(F),
    D = finddiag(F,N,Hv,Vtab),
    Newvs = sublist(Vs, 1, D) ++ Hvs ++ [Hv] ++ sublist(Vs, D, N-D+1),
    Face#e3d_face{vs=Newvs};
joinisland(Face, _, _) -> Face.

finddiag(F,N,Hv,Vtab) ->
    finddiagmodeloop(F,N,Hv,Vtab,0).

finddiagmodeloop(F,N,Hv,Vtab,Mode) ->
    case tryfinddiag(F, 1, N, Hv, Vtab, Mode, 0, 1.0e30) of
	I when I > 0 -> I;
	_ -> finddiagmodeloop(F, N, Hv, Vtab, Mode+1)
    end.

tryfinddiag(_, I, N, _, _, _, Best, _) when I > N -> Best;
tryfinddiag(F, I, N, Hv, Vtab, Mode, Best, Bestdist) ->
    %% Should be able to find a diagonal that connects a vertex of F
    %% left of Hv to Hv without crossing F, but try two
    %% more desperation passes after that to get SOME diagonal, even if
    %% it might cross some edge somewhere.
    %% First desperation pass (Mode == 1): allow points right of Hv.
    %% Second desperation pass (Mode == 2): allow crossing boundary poly
    V = element(I, F),
    {Best1,Bestdist1} =
	case {Mode, vless(Hv,V,Vtab)} of
	    {0, true} ->
		{Best,Bestdist};
	    _ ->
		Diff = sub2(coords2(V,Vtab),coords2(Hv,Vtab)),
		Dist = dot2(Diff,Diff),
		if
		    (Best > 0) and (Bestdist =< Dist) ->
			{Best,Bestdist};
		    true ->
			if
			    Mode == 2 -> {I,Dist};
			    true ->
				case isdiag(I, V, Hv, F, N, Vtab) of
				    true -> {I,Dist};
				    false -> {Best,Bestdist}
				end
			end
		end
	end,
    tryfinddiag(F,I+1,N,Hv,Vtab,Mode,Best1,Bestdist1).

%% Return true if segment (V,Hv) is a diagonal of face F:
%% Hv is in the cone of the angle at index I of F (== vertex V)
%% and no segment in F intersects (V,Hv).
isdiag(I,V,Hv,F,N,Vtab) ->
    Vm1 = welement(I-1, N, F),
    V1 = welement(I+1, N, F),
    K = anglekind(Vm1,V,V1,Vtab),
    case incone(Hv, Vm1, V, V1, K, Vtab) of
	true ->
	    isdiagloop(1, F, N, V, Hv, Vtab);
	false -> false
    end.

isdiagloop(J, _, N, _, _, _) when J > N -> true;
isdiagloop(J, F, N, V, Hv, Vtab) ->
    Vj = element(J, F),
    Vj1 = welement(J+1, N, F),
    case segsintersect(V, Hv, Vj, Vj1, Vtab) of
	true -> false;
	false -> isdiagloop(J+1, F, N, V, Hv, Vtab)
    end.

%% Return Hole (an e3d_face) with leftmost leftmost vertex.
leftmostface([H|Hrest], Vtab) ->
    foldl(fun (X, BestH) ->
		  case fless(X, BestH, Vtab) of
		      true -> X;
		      _ -> BestH
		  end
	  end, H, Hrest).

%% FF is a list of Faces, where each Face is a list of vertices
%% (with an implied wraparound).
%% Return gb_sets set of {U,V} such that {U,V} is an edge of some Face.
border_edges(FF) -> gb_sets:union(map(fun bedges/1, FF)).

bedges(F) -> bedges(F,F,gb_sets:empty()).

bedges([], _, S) -> S;
bedges([A],[B|_],S) -> gb_sets:add({A,B}, S);
bedges([A,B|T],F,S) -> bedges([B|T], F, gb_sets:add({A,B}, S)).

%% Tris is a list of triangles ({A,B,C}, CCW-oriented indices into Vtab)
%% Bord is a gb_sets set of border edges {U,V}, oriented so that Tris
%% is a triangulation of the left face of the border(s).
%% Make the triangulation "Constrained Delaunay" by flipping "reversed"
%% quadrangulaterals until can flip no more.
%% Return list of triangles in new triangulation.
cdt(Tris, Bord, Vtab) ->
    TD = tridict(Tris),
    RE = reversededges(Tris, TD, Bord, Vtab),
    TS = gb_sets:from_list(Tris),
    TS2 = cdtloop(RE, TS, TD, Bord, Vtab),
    gb_sets:to_list(TS2).

cdtloop([], TS, _, _, _) -> TS;
cdtloop([E={A,B} | Rest], TS, TD, Bord, Vtab) ->
    case isreversed(E, TD, Bord, Vtab) of
	true ->
	    %% Rotate E in quad ADBC to get other diagonal
	    Er={B,A},
	    case {gb_trees:lookup(E,TD), gb_trees:lookup(Er,TD)} of
		{{value,Tl},{value,Tr}} ->
		    C = othervert(Tl,A,B),
		    D = othervert(Tr,A,B),
		    NewT1 = {C,D,B},
		    NewT2 = {C,A,D},
		    NewE = {C,D},
		    NewEr = {D,C},
		    TD1 = gb_trees:delete(E, TD),
		    TD2 = gb_trees:delete(Er, TD1),
		    TD3 = gb_trees:insert(NewE, NewT1, TD2),
		    TD4 = gb_trees:insert(NewEr, NewT2, TD3),
		    TD5 = gb_trees:update({B,C}, NewT1, TD4),
		    TD6 = gb_trees:update({C,A}, NewT2, TD5),
		    TD7 = gb_trees:update({A,D}, NewT2, TD6),
		    TD8 = gb_trees:update({D,B}, NewT1, TD7),
		    TS1 = gb_sets:delete(Tl, TS),
		    TS2 = gb_sets:delete(Tr, TS1),
		    TS3 = gb_sets:insert(NewT1, TS2),
		    TS4 = gb_sets:insert(NewT2, TS3),
		    E2 = [{D,B},{B,C},{C,A},{A,D} | Rest],
		    cdtloop(E2, TS4, TD8, Bord, Vtab);
		_ ->
		    %% shouldn't happen, but...
		    io:format("couldn't find tri!~n",[]),
		    cdtloop(Rest, TS, TD, Bord, Vtab)
	    end;
	_ ->
	    cdtloop(Rest, TS, TD, Bord, Vtab)
    end.

%% Return a gb_trees dictionary mapping all edges of triangles Tris to
%% the containing triangle.  (Assume there is only one containing triangle,
%% if pay attention to the orientation of the edge.)
tridict(Tris) -> foldl(fun tridict1/2, gb_trees:empty(), Tris).

%% Use insert (which assume edge not already in Dict), because
%% whole algorithm assumes that a given edge (with direction) appears
%% in exactly one CCW-oriented triangle of the triangulation.
tridict1(T={A,B,C}, Dict) ->
    D1 = gb_trees:insert({A,B},T,Dict),
    D2 = gb_trees:insert({B,C},T,D1),
    gb_trees:insert({C,A},T,D2).

%% Return list of reversed edges in Tris.
%% Only want edges not in Bord, and only need one representative
%% of {U,V} / {V,U}, so choose the one with U < V.
%% TD is used to find left and right triangles of edges.
reversededges(Tris, TD, Bord, Vtab) -> reversededges(Tris, TD, Bord, Vtab, []).

reversededges([], _, _, _, Acc) -> Acc;
reversededges([{A,B,C}|T], TD, Bord, Vtab, Acc) ->
    Acc1 = revecheck({A,B}, TD, Bord, Vtab, Acc),
    Acc2 = revecheck({B,C}, TD, Bord, Vtab, Acc1),
    Acc3 = revecheck({C,A}, TD, Bord, Vtab, Acc2),
    reversededges(T, TD, Bord, Vtab, Acc3).

revecheck(E={A,B}, TD, Bord, Vtab, Acc) ->
    if
	A > B -> Acc;
	true ->
	    case isreversed(E, TD, Bord, Vtab) of
		true -> [E | Acc];
		false -> Acc
	    end
    end.

%% If E is a non-border edge, with left-face triangle Tl and
%% right-face triangle Tr, then it is "reversed" if the circle through
%% A, B, and (say) the other vertex of Tl containts the other vertex of Tr.
isreversed(E={A,B}, TD, Bord, Vtab) ->
    case gb_sets:is_member(E, Bord) of
	true -> false;
	_ ->
	    Er={B,A},
	    case {gb_trees:lookup(E,TD), gb_trees:lookup(Er,TD)} of
		{{value,Tl},{value,Tr}} ->
		    C = othervert(Tl,A,B),
		    D = othervert(Tr,A,B),
		    incircle(A,B,C,D,Vtab);
		_ -> false
	    end
    end.

%% Assume two out of three vertices of triangle are A,B.  Return the other.
othervert({A,B,V},A,B) -> V;
othervert({B,A,V},A,B) -> V;
othervert({A,V,B},A,B) -> V;
othervert({B,V,A},A,B) -> V;
othervert({V,A,B},A,B) -> V;
othervert({V,B,A},A,B) -> V.

%% Tris is list of triangles, forming a triangulation of region whose
%% border edges are in gb_sets set Bord.
%% Combine adjacent triangles to make quads, trying for "good" quads where possible.
%% Some triangles will probably remain uncombined.
quadrangulate(Tris, Bord, Vtab) ->
    ER = ergraph(Tris, Bord, Vtab),
    N = length(ER),
    if
	N == 0 -> Tris;
	true ->
	    Match = if N > ?GTHRESH -> greedymatch(ER);
		       true -> maxmatch(ER) end,
	    removeedges(Tris, Match)
    end.

%% Return list of {Weight,E,Tl,Tr} where edge E={A,B} is non-border edge
%% with left face Tl and right face Tr (each a triple {I,J,K}), where removing
%% the edge would form an "OK" quad (no concave angles), with weight representing
%% the desirability of removing the edge (high values -> more desirable)
ergraph(Tris, Bord, Vtab) ->
    TD = tridict(Tris),
    DD = degreedict(Tris),
    Tabs = {Bord, TD, DD, Vtab},
    ergraphloop(Tris, Tabs, []).

ergraphloop([], _, Acc) -> Acc;
ergraphloop([{A,B,C}=Tri | Rest], Tabs, Acc) ->
    Acc1 = ergraphe({A, B}, Tri, Tabs, Acc),
    Acc2 = ergraphe({B, C}, Tri, Tabs, Acc1),
    Acc3 = ergraphe({C, A}, Tri, Tabs, Acc2),
    ergraphloop(Rest, Tabs, Acc3).

ergraphe({A, B}, _, _, Acc) when A > B -> Acc;
ergraphe(E={A, B}, Tl, {Bord,TD,DD,Vtab}, Acc) ->
    case gb_sets:is_member(E, Bord) of
	true -> Acc;
	_ ->
	    Er = {B, A},
	    case gb_trees:lookup(Er, TD) of
		{value,Tr} ->
		    C = othervert(Tl,A,B),
		    D = othervert(Tr,A,B),

		    %% find angmax, max of two angles formed inside
		    A1 = angle(C,A,B,Vtab) + angle(D,A,B,Vtab),
		    A2 = angle(C,B,A,Vtab) + angle(D,B,A,Vtab),
		    Amax = case A1 > A2 of true -> A1; _ -> A2 end,
		    if
			Amax > 180.0 -> Acc;
			true ->
			    DegA = gb_trees:get(A,DD),
			    DegB = gb_trees:get(B,DD),
						% 1 : 10 weighting is heuristic
			    Weight = ?ANGFAC*(180.0 - Amax)  + ?DEGFAC*(DegA + DegB),
			    [{Weight,E,Tl,Tr} | Acc]
		    end;
		_ -> Acc
	    end
    end.

%% ER is list of {Weight,E,Tl,Tr}.
%% Find maximal set so that each triangle appears in at most one member of set.
greedymatch(ER) ->
    ER1 = sort(fun ({W1,_,_,_},{W2,_,_,_}) -> W1 > W2 end, ER),
    gmloop(ER1, gb_sets:empty(), []).

gmloop([], _, Acc) -> Acc;
gmloop([{_,_,Tl,Tr}=Q | Rest], M, Acc) ->
    case gb_sets:is_member(Tl, M) orelse gb_sets:is_member(Tr, M) of
	true -> gmloop(Rest, M, Acc);
	false ->
	    M1 = gb_sets:insert(Tl, gb_sets:insert(Tr, M)),
	    gmloop(Rest, M1, [Q | Acc])
    end.

%% Like greedymatch, but use divide and conquer to find best possible set.
maxmatch(ER) ->
    {Ans, _} = dcmatch(ER),
    Ans.

dcmatch([]) -> {[], 0.0};
dcmatch(ER=[{W,_,_,_}]) -> {ER, W};
dcmatch(ER) ->
    {greedymatch(ER), 1.0}.	 % TODO: put in DC code

%% Tris is list of triangles.  ER is as returned from maxmatch or greedymatch.
%% Return list of {A,D,B,C} resulting from deleting edge {A,B} causing a merge
%% of two triangles; append to that list the remaining unmatched triangles.
removeedges(Tris, ER) -> redges(gb_sets:from_list(Tris), ER, []).

redges(Tris, [], Acc) -> Acc ++ gb_sets:to_list(Tris);
redges(Tris, [{_,{A,B},Tl,Tr} | Rest], Acc) ->
    Tris1 = gb_sets:delete(Tl, Tris),
    Tris2 = gb_sets:delete(Tr, Tris1),
    C = othervert(Tl,A,B),
    D = othervert(Tr,A,B),
    redges(Tris2, Rest, [{A,D,B,C} | Acc]).


%% Make a dictionary taking vertex numbers to their degree (minus 1)
%% by adding together the number of triangles in which they appear.
degreedict(Tris) ->
    foldl(fun ({A,B,C}, DD) -> incd(A, incd(B, incd(C, DD))) end,
	  gb_trees:empty(), Tris).

incd(A, DD) ->
    case gb_trees:lookup(A, DD) of
	{value, V} -> gb_trees:update(A, V+1, DD);
	_ -> gb_trees:insert(A, 1, DD)
    end.


%% rotate vertex list so that leftmost vert is first
sortface(#e3d_face{vs=Vs}=Face, Vtab) ->
    N = length(Vs),
    if
	N < 2 -> Face;
	true ->
	    [V|T] = Vs,
	    I = leftmostv(T, 2, Vtab, 1, V),
	    Vs1 = case I of
		      1 -> Vs;
		      _ -> sublist(Vs, I, N-I+1) ++ sublist(Vs, 1, I-1)
		  end,
	    Face#e3d_face{vs=Vs1}
    end.

leftmostv([], _, _, BestI, _) -> BestI;
leftmostv([V|T], I, Vtab, BestI, BestV) ->
    case vless(V,BestV,Vtab) of
	true -> leftmostv(T, I+1, Vtab, I, V);
	_ -> leftmostv(T, I+1, Vtab, BestI, BestV)
    end.

vless(A,B,Vtab) ->
    {Ax,Ay} = coords2(A,Vtab),
    {Bx,By} = coords2(B,Vtab),
    (Ax < Bx) or ((Ax == Bx) and (Ay < By)).

%% Assume face is ordered with leftmost vertex first.
fless(#e3d_face{vs=[Va|_]}, #e3d_face{vs=[Vb|_]}, Vtab) ->
    vless(Va, Vb, Vtab);
fless(_,_,_) -> true.

%% Return tuple of anglekind of angles of F (a tuple of vertices)
classifyangles(F, N, Vtab) ->
    list_to_tuple(map(fun (I) ->
			      anglekind(welement(I-1,N,F), element(I,F),
					welement(I+1,N,F), Vtab) end,
		      seq(1,N))).

%% Classify angle formed by vertices A,B,C with respect to
%% its left side as one of:
%% ang_convex, ang_reflex, ang_tangential, ang_0, ang_360
anglekind(A,B,C,Vtab) ->
    case ccw(A,B,C,Vtab) of
	true -> ang_convex;
	false ->
	    case ccw(A,C,B,Vtab) of
		true -> ang_reflex;
		false ->
		    Vb = coords2(B,Vtab),
		    Udotv = dot2(sub2(Vb,coords2(A,Vtab)),sub2(coords2(C,Vtab),Vb)),
		    if
			Udotv > 0.0 -> ang_tangential;
			true -> ang_0	% to fix: return ang_360 if an "inside" spur
		    end
	    end
    end.

%% Return true if ABC is counter-clockwise oriented triangle.
%% Returns false if not (could be colinear, within 1e-7 tolerance)
ccw(A,B,C,Vtab) ->
    {Ax,Ay} = coords2(A,Vtab),
    {Bx,By} = coords2(B,Vtab),
    {Cx,Cy} = coords2(C,Vtab),
    D = Ax*By-Bx*Ay -Ax*Cy+Cx*Ay +Bx*Cy-Cx*By,
    if
	D > ?TOL -> true;
	true -> false
    end.

%% Return true if circle through points with indices A, B, C
%% contains point with index D,
%% except that if A, B, C forms a clockwise oriented triangle
%% the test is reversed: return true if d is outside the circle.
%% (Will get false if cocircular, within tolerance)
%%   | xa ya xa^2+ya^2 1 |
%%   | xb yb xb^2+yb^2 1 | > 0
%%   | xc yc xc^2+yc^2 1 |
%%   | xd yd xd^2+yd^2 1 |
incircle(A,B,C,D,Vtab) ->
    {Xa,Ya,Za} = icc(A,Vtab),
    {Xb,Yb,Zb} = icc(B,Vtab),
    {Xc,Yc,Zc} = icc(C,Vtab),
    {Xd,Yd,Zd} = icc(D,Vtab),
    Det = Xa*(Yb*Zc-Yc*Zb -Yb*Zd+Yd*Zb + Yc*Zd-Yd*Zc)
	-Xb*(Ya*Zc-Yc*Za -Ya*Zd+Yd*Za + Yc*Zd-Yd*Zc)
	+Xc*(Ya*Zb-Yb*Za -Ya*Zd+Yd*Za + Yb*Zd-Yd*Zb)
	-Xd*(Ya*Zb-Yb*Za -Ya*Zc+Yc*Za + Yb*Zc-Yc*Zb),
    Det > ?TOL .

icc(A,Vtab) ->
    {X,Y} = coords2(A,Vtab),
    {X,Y,X*X+Y*Y}.

%% Return tuple version of Vcoords (list of coords) rotated
%% so that normal of polygon with indices in list Vs
%% gets rotated to (0,0,1).
rotate_normal_to_z(Vs, Vcoords) ->
    Fnorm = polygon_plane(Vs, Vcoords),
    rot_normal_to_z(Fnorm, Vcoords).

rot_normal_to_z(N, Vcoords) ->
    Rotm = e3d_mat:rotate_to_z(N),
    Vrot = [begin {X,Y,_} = e3d_mat:mul_point(Rotm, P), {X,Y} end || P <- Vcoords],
    list_to_tuple(Vrot).

polygon_plane(Vs, Vcoords) ->
    case length(Vs) of
	Nvs when Nvs < 3 ->
	    {0.0,0.0,1.0};			% so doesn't crash
	_ ->
	    Vtab = list_to_tuple(Vcoords),
	    Ps = [element(V+1, Vtab) || V <- Vs],
	    case e3d_vec:normal(Ps) of
		{0.0,0.0,0.0} -> {0.0,0.0,1.0};
		N -> N
	    end
    end.

coords2(A, Vtab) ->
    element(A+1, Vtab).

dot2({Ax,Ay}, {Bx,By}) -> Ax*Bx + Ay*By.

perp2({Ax,Ay}, {Bx,By}) -> Ax*By - Ay*Bx.

sub2({Ax,Ay}, {Bx,By}) -> {Ax-Bx,Ay-By}.

norm2({X,Y}) -> math:sqrt(X*X + Y*Y).

%% return angle ABC in degrees, in range [0,180)
angle(A,B,C,Vtab) ->
    U = sub2(coords2(C,Vtab), coords2(B,Vtab)),
    V = sub2(coords2(A,Vtab), coords2(B,Vtab)),
    N1 = norm2(U),
    N2 = norm2(V),
    case (N1 == 0.0 orelse N2 == 0.0) of
	true -> 0.0;
	false ->
	    Costheta = dot2(U,V)/(N1*N2),
	    C1 = if Costheta > 1.0 -> 1.0; true -> Costheta end,
	    C2 = if C1 < -1.0 -> -1.0; true -> C1 end,
	    math:acos(C2) * 180.0 / math:pi()
    end.

%% does segment AB intersect CD?  Just touching -> false.
segsintersect(IA, IB, IC, ID, Vtab) ->
    A = coords2(IA,Vtab),
    B = coords2(IB,Vtab),
    C = coords2(IC,Vtab),
    D = coords2(ID,Vtab),
    U = sub2(B,A),
    V = sub2(D,C),
    W = sub2(A,C),
    PP = perp2(U,V),
    case (-1.0e-7 < PP) andalso (PP < 1.0e-7) of
	false ->
	    SI = perp2(V,W) / PP,
	    TI = perp2(U,W) / PP,
	    (SI > 0.0 andalso SI < 1.0
	     andalso TI > 0.0 andalso TI < 1.0);
	true ->
	    %% parallel or overlapping
	    case {dot2(U, U),dot2(V, V)} of
		{0.0,_} -> false;
		{_,0.0} -> false;
		{_,_} ->
		    %% At this point, we know that none of the
		    %% segments are points.
		    Z = sub2(B, C),
		    {Vx,Vy}=V, {Wx,Wy}=W, {Zx,Zy}=Z,
		    {T0,T1} = case Vx of
				  0.0 ->
				      {Wy/Vy, Zy/Vy};
				  _ ->
				      {Wx/Vx, Zx/Vx}
			      end,
		    (0.0 < T0) andalso (T0 < 1.0) andalso
		    (0.0 < T1) andalso (T1 < 1.0)
	    end
    end.


%% element(I,T), but wrap I if necessary to stay in range 1..N
welement(I, N, T) -> element(windex(I, N), T).

windex(I, N) when I < 1 -> windex(I+N, N);
windex(I, N) when I =< N -> I;
windex(I, N) -> windex(I-N, N).



-ifdef(TESTING).

						% TESTING
test_data(T) ->
						% Points in pattern:
						% 4     3
						%
						% 
						%    2
						% 0     1
    Vs1 = [{0.0,0.0,0.0},
	   {1.0,0.0,0.0},
	   {0.5,0.25,0.0},
	   {1.0,1.0,0.0},
	   {0.0,1.0,0.0}],
						% Points in pattern
						% 0                   1
						%    2 3        4  5
						%         6  7
						%    8 9        10 10
						% 12      13 14       15
    Vs2 = [{0.0,1.0,0.0}, {1.75,1.0,0.0},
	   {0.25,0.75,0.0}, {0.5,0.75,0.0}, {1.25,0.75,0.0}, {1.5,0.75,0.0},
	   {0.75,0.5,0.0}, {1.0,0.5,0.0},
	   {0.25,0.25,0.0}, {0.5,0.25,0.0}, {1.25,0.25,0.0}, {1.5,0.25,0.0},
	   {0.0,0.0,0.0}, {0.75,0.0,0.0}, {1.0,0.0,0.0}, {1.75,0.0,0.0}],
						% 16 points in circle
    Vs3 = [{1.00000,0.0,0.0},
	   {0.923880,0.0,0.382683},
	   {0.707107,0.0,0.707107},
	   {0.382683,0.0,0.923880},
	   {2.67949e-8,0.0,1.000000},
	   {-0.382683,0.0,0.923880},
	   {-0.707107,0.0,0.707107},
	   {-0.923880,0.0,0.382683},
	   {-1.000000,0.0,5.35898e-8},
	   {-0.923880,0.0,-0.382683},
	   {-0.707107,0.0,-0.707107},
	   {-0.382684,0.0,-0.923880},
	   {-8.03847e-8,0.0,-1.000000},
	   {0.382683,0.0,-0.923880},
	   {0.707107,0.0,-0.707107},
	   {0.923879,0.0,-0.382684}],
						% Points for lowercase Arial m
    Vsm =[{0.131836,0.0,0.0},	
	  {0.307617,0.0,0.0},
	  {0.307617,0.538086,0.0},
	  {0.335938,0.754883,0.0},
	  {0.427246,0.869141,0.0},
	  {0.564453,0.908203,0.0},
	  {0.705078,0.849609,0.0},
	  {0.748047,0.673828,0.0},
	  {0.748047,0.0,0.0},		
	  {0.923828,0.0,0.0},		
	  {0.923828,0.602539,0.0},
	  {0.996094,0.835449,0.0},
	  {1.17773,0.908203,0.0},		
	  {1.28320,0.879883,0.0},		
	  {1.34521,0.805176,0.0},
	  {1.36230,0.653320,0.0},		
	  {1.36230,0.0,0.0},		
	  {1.53711,0.0,0.0},
	  {1.53711,0.711914,0.0},		
	  {1.45410,0.975098,0.0},		
	  {1.21680,1.06055,0.0},		
	  {0.896484,0.878906,0.0},
	  {0.792480,1.01270,0.0},		
	  {0.603516,1.06055,0.0},		
	  {0.418945,1.01416,0.0},		
	  {0.289063,0.891602,0.0},
	  {0.289063,1.03711,0.0},		
	  {0.131836,1.03711,0.0}],
    Mfront = lists:seq(0,27,1),
    Mback = lists:seq(27,0,-1),
    case T of
	tri -> {#e3d_face{vs=[0,1,2]}, [], Vs1};
	square -> {#e3d_face{vs=[0,1,3,4]}, [], Vs1};
	circle -> {#e3d_face{vs=lists:seq(0,15)}, [], Vs3};
	concave -> {#e3d_face{vs=[0,2,1,3,4]}, [], Vs1};
	crosses -> {#e3d_face{vs=[0,1,4,3]}, [], Vs1};
	twoholes -> {#e3d_face{vs=[0,12,13,6,7,14,15,1]},
		     [#e3d_face{vs=[2,3,9,8]}, #e3d_face{vs=[5,11,10,4]}], Vs2};
	mf -> {#e3d_face{vs=Mfront}, [], Vsm};
	mb -> {#e3d_face{vs=Mback}, [], Vsm};
	_ -> {#e3d_face{vs=[0,1,2]}, Vs1}
    end.

test_tri(T) ->
    {F,H,V} = test_data(T),
    case H of
	[] -> triangulate_face(F, V);
	_ -> triangulate_face_with_holes(F, H, V)
    end.

test_quad(T) ->
    {F,H,V} = test_data(T),
    case H of
	[] -> quadrangulate_face(F, V);
	_ -> quadrangulate_face_with_holes(F, H, V)
    end.
-endif.

