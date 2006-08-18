%%
%%  wings_tesselation.erl --
%%
%%     Tesselation/subdivision commands.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_tesselation.erl,v 1.13 2005/03/14 12:34:44 dgud Exp $
%%

-module(wings_tesselation).
-export([submenu/0,command/2]).
-export([triangulate/1,triangulate/2,quadrangulate/1,quadrangulate/2]).

-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-import(lists, [map/2,reverse/1]).

submenu() ->
    [{?STR(submenu,1,"Triangulate"),triangulate},
     {?STR(submenu,2,"Quadrangulate"),quadrangulate}].

command(triangulate, St) ->
    Action = fun triangulate/2,
    {St1,Sel} = wings_sel:mapfold(fun(Fs, We, A) ->
					  do_faces(Action, Fs, We, A) end,
				  [], St),
    {save_state,St1#st{sel=reverse(Sel)}};
command(quadrangulate, St) ->
    Action = fun quadrangulate/2,
    {St1,Sel} = wings_sel:mapfold(fun(Fs, We, A) ->
					  do_faces(Action, Fs, We, A) end,
				  [], St),
    {save_state,St1#st{sel=reverse(Sel)}}.

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

%%%
%%% Internal functions.
%%%

do_faces(Action, Faces, #we{id=Id}=We0, Acc) ->
    We = Action(Faces, We0),
    Sel = gb_sets:union(wings_we:new_items_as_gbset(face, We0, We), Faces),
    {We,[{Id,Sel}|Acc]}.

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
    VsPos = [gb_trees:get(V, Vtab) || V <- Vs],
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
	case wings_draw_util:good_triangulation(N,A,B,C,D) of
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
    Vcoords = [gb_trees:get(V, Vtab) || V <- Vs],
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
