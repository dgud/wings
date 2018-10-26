%%
%%  wings_tesselation.erl --
%%
%%     Tesselation/subdivision commands.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_tesselation).
-export([submenu/0,command/2]).
-export([triangulate/1,triangulate/2,quadrangulate/1,quadrangulate/2]).
-export([is_good_triangulation/5]).

-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-import(lists, [reverse/1]).

submenu() ->
    [{?STR(submenu,1,"Triangulate"),triangulate},
     {?STR(submenu,2,"Quadrangulate"),quadrangulate}].

command(triangulate, St0) ->
    Action = fun triangulate/2,
    St = wings_sel:map_update_sel(
           fun(Fs, We) ->
                   do_faces(Action, Fs, We)
           end, St0),
    {save_state,St};
command(quadrangulate, St0) ->
    Action = fun quadrangulate/2,
    St = wings_sel:map_update_sel(
           fun(Fs, We) ->
                   do_faces(Action, Fs, We)
           end, St0),
    {save_state,St}.

triangulate(#we{fs=Ftab}=We) ->
    triangulate(gb_sets:from_ordset(gb_trees:keys(Ftab)), We).

triangulate(Faces, We) when is_list(Faces) ->
    triangulate(gb_sets:from_list(Faces), We);
triangulate(Faces, We) ->
    tri_faces(Faces, We).

quadrangulate(#we{fs=Ftab}=We) ->
    quadrangulate(gb_trees:keys(Ftab), We).

quadrangulate(Faces, We) when is_list(Faces) ->
    tess_faces(Faces, We);
quadrangulate(Faces, We) ->
    quadrangulate(gb_sets:to_list(Faces), We).

%% is_good_triangulation(Normal, PointA, PointB, PointC, PointD) -> true|false
%%  The points PointA through PointD are assumed to be the vertices of
%%  quadrilateral in counterclockwise order, and Normal should be the
%%  averaged normal for the quad.
%%
%%  This function will determine whether a triangulation created by
%%  joining PointA to PointC is a good triangulation (thus creating
%%  the two triangles PointA-PointB-PointC and PointA-PointC-PointD).
%%  This function returns 'true' if none of the two triangles is degenerated
%%  and the diagonal PointA-PointC is inside the original quad (if the
%%  quad is concave, one of the "diagonals" will be inside the quad).
%%
%%  This function returns 'false' if the PointA-PointC triangulation is
%%  bad. Except for pathoglogical quads (e.g. non-planar or warped), the other
%%  triangulation using the PointB-PointD triangulation should be OK.
%%
is_good_triangulation({Nx,Ny,Nz}, {Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz})
  when is_float(Ax), is_float(Ay), is_float(Az) ->
    %% Construct the normals for the two triangles by calculating the
    %% cross product of two edges in the correct order:
    %%
    %%    NormalTri1 = (PointC-PointA) x (PointA-PointB)
    %%    NormalTri2 = (PointD-PointA) x (PointA-PointC)
    %%
    %% The normals should point in about the same direction as the
    %% normal for the quad. We certainly expect the angle between a
    %% triangle normal and the quad normal to be less than 90
    %% degrees. That can be verified by taking the dot product:
    %%
    %%    Dot1 = QuadNormal . NormalTri1
    %%    Dot2 = QuadNormal . NormalTri2
    %%
    %% Both dot products should be greater than zero. A zero dot product either
    %% means that the triangle normal was not defined (a degenerate triangle) or
    %% that the angle is exactly 90 degrees. A negative dot product means that
    %% the angle is greater than 90 degress, which implies that the PointA-PointC
    %% line is outside the quad.
    %%
    CAx = Cx-Ax, CAy = Cy-Ay, CAz = Cz-Az,
    ABx = Ax-Bx, ABy = Ay-By, ABz = Az-Bz,
    DAx = Dx-Ax, DAy = Dy-Ay, DAz = Dz-Az,
    D1 = Nx*(CAy*ABz-CAz*ABy) + Ny*(CAz*ABx-CAx*ABz) + Nz*(CAx*ABy-CAy*ABx),
    D2 = Nx*(DAz*CAy-DAy*CAz) + Ny*(DAx*CAz-DAz*CAx) + Nz*(DAy*CAx-DAx*CAy),
    is_good_triangulation_1(D1, D2).

%%%
%%% Internal functions.
%%%
%% Check that D1 and D2 is larger than "Epsilon" to avoid triangles that are
%% 3 points on a line
is_good_triangulation_1(D1, D2) when D1 > 0.000001, D2 > 0.000001 -> true;
is_good_triangulation_1(_, _) -> false.

do_faces(Action, Faces, We0) ->
    We = Action(Faces, We0),
    Sel = gb_sets:union(wings_we:new_items_as_gbset(face, We0, We), Faces),
    {We,Sel}.

tess_faces([], We) -> We;
tess_faces([F|T], We) -> tess_faces(T, doface(F, We)).

doface(Face, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    case length(Vs) of
	Len when Len =< 3 -> We;
	Len when Len =< 4 -> We;
	Len -> doface_1(Face,Len, Vs, We, true)
    end.

tri_faces(Fs0,We0) ->
    tri_faces([],Fs0,gb_sets:empty(), We0).   

tri_faces([], Fs0, TriV0, We0) ->
    case gb_sets:is_empty(Fs0) of
	true -> We0;
	false ->
	    {Face, Fs1} = gb_sets:take_smallest(Fs0),
	    {Pref, Fs, TriV, We} = triface(Face,Fs1,TriV0,We0),
	    tri_faces(Pref, Fs,TriV,We)
    end;
tri_faces([Face|R],Fs0,TriV0,We0) ->
    {Pref, Fs, TriV,We} = triface(Face,Fs0,TriV0,We0),
    tri_faces(Pref ++ R,Fs,TriV,We).

triface(Face, Fs, TriV,We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    case length(Vs) of
	3 -> {[], Fs, TriV, We};
	4 ->
	    triangulate_quad(Face, Vs, TriV, Fs, We);
	Len -> {[], Fs, TriV, doface_1(Face,Len, Vs, We, false)}
    end.

%%  Triangulates a quad, tries to make the triangulation so nice
%%  patterns emerges, or otherwise along the shortest diagonal, Then
%%  checking that normals for the triangles are consistent with the
%%  normal for the quad. Falls back to the general triangulator if
%%  normals are inconsistent (= concave or otherwise strange quad).

triangulate_quad(F, Vs, TriV0, FsSet0, #we{vp=Vtab}=We0) ->
    VsPos = [array:get(V, Vtab) || V <- Vs],
    try 
	{V1,V2,TriV,We} = triangulate_quad_1(VsPos, Vs, F, TriV0, We0),
	{Fs1, FsSet1} = get_pref_faces(V1,FsSet0,We),
	{Fs2, FsSet} = get_pref_faces(V2,FsSet1,We),
	{Fs1++Fs2,FsSet,TriV,We}
    catch throw:_Problematic ->
	    {[],FsSet0,TriV0, doface_1(F,4, Vs, We0, false)};
	Type:Err ->
	    io:format("~p:~p: ~p ~p ~p~n", 
		      [?MODULE,?LINE, Type, Err, erlang:get_stacktrace()])
    end.

get_pref_faces(V,Fs0,We) ->
    wings_vertex:fold(fun(_,F,_,{Acc,FsSet}) ->
			      case gb_sets:is_member(F,FsSet) of
				  true -> {[F|Acc], gb_sets:delete(F,FsSet)};
				  false -> {Acc,FsSet}
			      end
		      end, {[],Fs0}, V, We).

triangulate_quad_1(VsPos=[A,B,C,D], Vi=[Ai,Bi,Ci,Di], F, TriV, We) ->
    N = e3d_vec:normal(VsPos),
    ACgood = gb_sets:is_member(Ai,TriV) orelse 
	gb_sets:is_member(Ci,TriV),
    BDgood = gb_sets:is_member(Bi,TriV) orelse 
	gb_sets:is_member(Di,TriV),
    [V1,V2] = 
	if ACgood, (not BDgood) ->
		assert_quad2tris(N,A,B,C,D,F),
		[Ai,Ci];
	   BDgood, (not ACgood) ->
		assert_quad2tris(N,B,C,D,A,F),
		[Bi,Di];
	   true ->
		select_newedge(VsPos,Vi,N,F)
	end,
    {NewWe,_NewFace} = wings_vertex:force_connect(V1,V2,F,We),
    {V1,V2,gb_sets:add(V2,gb_sets:add(V1,TriV)), NewWe}.

select_newedge(_L = [A,B,C,D],[Ai,Bi,Ci,Di],N,F) ->
    AC = e3d_vec:dist(A, C),
    BD = e3d_vec:dist(B, D),
    Epsilon = 0.15,  %% 1/6 diffs Is rougly equal 
    case AC < BD of
	true when ((BD-AC) / BD)  > Epsilon ->
	    assert_quad2tris(N,A,B,C,D,F),
	    [Ai,Ci];
	_ ->
	    assert_quad2tris(N,B,C,D,A,F),
	    [Bi,Di]
    end.

%% Good enough triangles
-define(TRI_AREA, 0.70).  
%% This allows pretty big area diff, but avoid areas close to 0.
assert_quad2tris(N,A,B,C,D,F) ->
    try 
	case is_good_triangulation(N, A, B, C, D) of
	    true ->
		T1 = e3d_vec:area(A,B,C),
		T2 = e3d_vec:area(C,D,A),
		case (abs(T1-T2) / (T1+T2)) < 0.80 of
		    true -> ok;
		    _ -> 
			throw(F)
		end;
	    false ->
		throw(F)
	end
    catch error:_ ->
	    throw(F)
    end.

doface_1(Face,Len,Vs,#we{vp=Vtab}=We, Q) ->
    FaceVs = lists:seq(0, Len-1),
    Vcoords = [array:get(V, Vtab) || V <- Vs],
    E3dface = #e3d_face{vs=FaceVs},
    T3dfaces = case Q of
		   true -> e3d_mesh:quadrangulate_face(E3dface, Vcoords);
		   false -> e3d_mesh:triangulate_face(E3dface, Vcoords)
	       end,
    VsTuple = list_to_tuple(Vs),
    Tfaces = [renumber(FVs, VsTuple) || #e3d_face{vs=FVs} <- T3dfaces],
    Bord = bedges(Vs),
    Diags = diags(Tfaces, Bord),
    connect_diags(Diags, [{Vs,Face}], Q, We).

renumber(L, Vtab) ->
    renumber(L, Vtab, []).
renumber([V|Vs], Vtab, Acc) ->
    renumber(Vs, Vtab, [element(V+1, Vtab)|Acc]);
renumber([], _, Acc) -> reverse(Acc).

%% This simple code only works because we assume that each
%% vertex can appear only once in a face.
connect_diags([], _Faces, _Q, We) -> We;
connect_diags([{A,B}|T], Faces, Q, We0) ->
    case find_face(A,B,Faces) of
	none -> %% Hmm
	    connect_diags(T, Faces, Q, We0);
	Face -> 
	    {We,NewFace} = wings_vertex:force_connect(A,B,Face,We0),
	    Vs = wings_face:vertices_ccw(NewFace, We),
	    connect_diags(T,[{Vs,NewFace}|Faces],Q,We)
    end.

find_face(_,_,[]) -> none;
find_face(A,B,[{Vs, Face}|Fs]) ->
    case lists:member(A,Vs) andalso lists:member(B,Vs) of
	true ->  Face;
	false -> find_face(A,B,Fs)
    end.

%% Return GbSet of {A,B} where AB is an edge in face F
bedges(F) -> bedges(F, F, []).

bedges([A], [B|_], S) -> gb_sets:from_list([{A,B}|S]);
bedges([A|[B|_]=T], F, S) when A < B -> 
    bedges(T, F, [{A,B}|S]);
bedges([A|[B|_]=T], F, S) -> 
    bedges(T, F, [{B,A}|S]).

diags(Fl, Bord) -> diags1(Fl, {Bord, []}).

diags1([], {_, S}) -> reverse(S);
diags1([Vs|T], Acc) -> diags1(T, diagsf(Vs, Vs, Acc)).

diagsf([A], [First|_], Acc) ->
    trydiag(A, First, Acc);
diagsf([A|[B|_]=T], Vs, Acc) ->
    diagsf(T,Vs,trydiag(A, B, Acc)).

trydiag(A, B, Acc) when A > B ->
    %% only want one representative of diag
    Acc;
trydiag(A, B, Old={Bord, S}) ->
    E = {A,B},
    case gb_sets:is_member(E, Bord) of
	true -> Old;
	false -> {gb_sets:add(E,Bord),[E|S]}
    end.
