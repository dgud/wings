%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude (edge), Bevel (face/edge) and
%%     Bump commands. (All based on edge extrusion.)
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_extrude_edge).
-export([bump/1,bevel/1,bevel_faces/1,extrude/2,crease/1,extrude_edges/3]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

-define(DEFAULT_EXTRUDE_DIST, 0.2).
-define(BEVEL_EXTRUDE_DIST_KLUDGE, 0.0001).

-type edge_num() :: wings_edge:edge_num().

%%%
%%% The Bump command.
%%%

bump(St0) ->
    Dist = calc_bump_dist(St0),
    MF = fun(Fs, We) -> bump(Fs, Dist, We) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp=PlusMinus}) -> PlusMinus end,
    wings_move:plus_minus(normal, DF, St).

calc_bump_dist(St) ->
    Map = fun calc_bump_dist_1/2,
    Combine = fun min/2,
    wings_sel:dfold(Map, Combine, infinity, St) / 2.

calc_bump_dist_1(Faces, We) ->
    Edges = gb_sets:to_list(wings_edge:from_faces(Faces, We)),
    min_dist_from_edges(Edges, We).

bump(Faces, Dist, We0) ->
    Edges = gb_sets:from_list(wings_face:outer_edges(Faces, We0)),
    {We,_,_,_} = extrude_edges(Edges, Faces, Dist, We0),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    We#we{temp={Faces,NewVs,gb_sets:empty()}}.

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    St = wings_sel:map_update_sel(fun bevel_edges/2, face, St0),
    bevel_drag(St).

bevel_edges(Edges, #we{mirror=MirrorFace}=We0) ->
    Dist = ?BEVEL_EXTRUDE_DIST_KLUDGE,
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, Dist, We0#we{mirror=none}),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2, Forbidden),
    Tv = scale_tv(Tv0, Dist),
    We3 = wings_collapse:collapse_vertices(OrigVs, We2),
    Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, We3#we.vp),
    We4 = We3#we{vp=Vtab,mirror=MirrorFace},
    Limit = bevel_limit(Tv, We4),
    We = We4#we{temp={Limit,Tv}},
    Sel = case gb_sets:is_empty(Forbidden) of
	      true ->
                  wings_we:new_items_as_gbset(face, We0, We);
	      false ->
                  gb_sets:empty()
	  end,
    {We,Sel}.

%%
%% The Bevel command (for faces).
%%

bevel_faces(St0) ->
    St = wings_sel:map(fun bevel_faces/2, St0),
    bevel_drag(St).

bevel_faces(Faces, #we{mirror=MirrorFace}=We0) ->
    Dist = ?BEVEL_EXTRUDE_DIST_KLUDGE,
    Edges = wings_edge:from_faces(Faces, We0),
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, Dist, We0#we{mirror=none}),
    case {wings_util:array_entries(We0#we.es),wings_util:array_entries(We1#we.es)} of
	{Same,Same} ->
	    wings_u:error_msg(?__(1,"Object is too small to bevel."));
	{_,_} ->
	    We2 = wings_edge:dissolve_edges(Edges, We1),
	    Tv0 = bevel_tv(OrigVs, We2, Forbidden),
	    Tv = scale_tv(Tv0, Dist),
	    #we{vp=Vtab0} = We3 = wings_collapse:collapse_vertices(OrigVs, We2),
	    Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, Vtab0),
	    We = We3#we{vp=Vtab,mirror=MirrorFace},
	    Limit = bevel_limit(Tv, We),
	    We#we{temp={Limit,Tv}}
    end.

%%
%% Common bevel utilities.
%%

bevel_drag(St) ->
    MF = fun(_, #we{temp={Limit,_}}) -> Limit end,
    RF = fun min/2,
    Limit = wings_sel:dfold(MF, RF, infinite, St),
    DF = fun(_, #we{temp={_,Tv}}=We) ->
                 wings_drag:translate_fun(Tv, We)
         end,
    wings_drag:fold(DF, [{distance,{0.0,Limit}}], [], St).

bevel_tv(Vs, We, Forbidden) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, Forbidden, A) end, [], Vs).

bevel_tv_1(V, We, Forbidden, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Tv0) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Tv0;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      Pos = wings_vertex:pos(OtherV, We),
		      Vec = e3d_vec:sub(Pos, Center),
		      [{Vec,[OtherV]}|Tv0]
	      end
      end, Acc, V, We).

bevel_reset_pos(Vs, We, Forbidden, Vtab) ->
    foldl(fun(V, A) -> bevel_reset_pos_1(V, We, Forbidden, A) end, Vtab, Vs).

bevel_reset_pos_1(V, We, Forbidden, Vtab) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Vt) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Vt;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      array:set(OtherV, Center, Vt)
	      end
      end, Vtab, V, We).

bevel_limit(Tv, We) ->
    L0 = foldl(fun({Vec,[V]}, A) ->
		       bevel_limit_1(V, Vec, We, A)
	       end, [], Tv),
    L = wings_util:rel2fam(L0),
    try
	bevel_min_limit(L, We, infinite)
    catch
	error:badarith ->
	    extrude_problem()
    end.

bevel_limit_1(V, Vec, #we{vp=Vtab}=We, Acc) ->
    Pos = array:get(V, Vtab),
    Data = {Pos,Vec},
    wings_vertex:fold(fun(_, _, Rec, A) ->
			      OtherV = wings_vertex:other(V, Rec),
			      [{edge_name(V, OtherV),Data}|A]
		      end, Acc, V, We).

edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
edge_name(Va, Vb) -> {Vb,Va}.

bevel_min_limit([{_Edge,[{O1,D1},{O2,D2}]}|Tail], We, Min0) ->
    %% Find intersection between lines.
    O2MinusO1 = e3d_vec:sub(O2, O1),
    D1CrossD2 = e3d_vec:cross(D1, D2),
    LenD1CrossD2 = e3d_vec:len(D1CrossD2),
    case LenD1CrossD2*LenD1CrossD2 of
	Z when Z < (0.01*?DEFAULT_EXTRUDE_DIST*?DEFAULT_EXTRUDE_DIST) ->
	    %% There is no intersection between the lines.
	    case e3d_vec:len(O2MinusO1) of
		Odist when Odist < 0.000001 ->
		    %% As the vertices are already near each other,
		    %% we will assume that they will be moving apart.
		    bevel_min_limit(Tail, We, Min0);
		Odist ->
		    D1Len = e3d_vec:len(D1),
		    case e3d_vec:dist(e3d_vec:mul(D1, Odist/D1Len), O2MinusO1) of
			Dist when Dist < 0.000001 ->
			    %% The vertices will be moved directly towards each
			    %% others.
			    S = Odist / (2*D1Len),
			    bevel_min_limit(Tail, We, min(Min0, S));
			_ ->
			    %% The vertices will not meet each other.
			    bevel_min_limit(Tail, We, Min0)
		    end
	    end;
	SqrLen ->
	    %% There is an intersection. Calculate its parameters.
	    S = e3d_vec:dot(e3d_vec:cross(O2MinusO1, D2), D1CrossD2)/SqrLen,
	    T = e3d_vec:dot(e3d_vec:cross(O2MinusO1, D1), D1CrossD2)/SqrLen,
	    if
		S > 0, T > 0 ->
		    Min = min(S, min(T, Min0)),
		    bevel_min_limit(Tail, We, Min);
		true ->
		    %% No intersection in the forward direction.
		    bevel_min_limit(Tail, We, Min0)
	    end
    end;
bevel_min_limit([{{Va,Vb},[{_,D1}]}|Tail], #we{vp=Vtab}=We, Min0) ->
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    case e3d_vec:len(D1) of
	DLen when DLen < 0.000001 ->
	    bevel_min_limit(Tail, We, 0.0);
	DLen ->
	    case e3d_vec:len(e3d_vec:sub(VaPos, VbPos)) / DLen of
		Min when Min < Min0 ->
		    bevel_min_limit(Tail, We, Min);
		_ ->
		    bevel_min_limit(Tail, We, Min0)
	    end
    end;
bevel_min_limit([], _, Min) -> Min.

-spec extrude_problem() -> no_return().
extrude_problem() ->
    M = ?__(1,"Can't extrude/bevel; two or more vertices are "
	    "probably too near to each other.\n"
	    "Try the Cleanup command."),
    wings_u:error_msg(M).

%%
%% Crease command
%%

crease(St0) ->
    Dist = calc_extrude_dist(St0),
    MF = fun(Edges, We0) ->
        We1 = extrude_1(Edges, Dist, We0),
        #we{temp={_,NewVs0,F}} = We1,
        ValidCaps = find_cap_vs(Edges, We0, []),
        #we{vp=Vtab}=We = foldl(fun(V, We2) ->
            EndCap = wings_vertex:fold(fun
              (Edge, _, #edge{lf=Lf,rf=Rf}=E, EndCapAcc) ->
                case wings_face:vertices(Lf, We2) of
                    3 ->
                      case wings_face:vertices(Rf, We2) of
                          3 ->
                              Other = wings_vertex:other(V, E),
                              case lists:member(Other, ValidCaps) of
                                  true -> [{d,Edge}|EndCapAcc];
                                  false -> EndCapAcc
                              end;
                          _ -> EndCapAcc
                      end;
                    N1 when N1 > 4 ->
                      case wings_face:vertices(Rf, We2) of
                          N2 when N2 > 4 ->
                            Other = wings_vertex:other(V, E),
                            [{c,Edge,Other}|EndCapAcc];
                          _ -> EndCapAcc
                      end;
                    _ -> EndCapAcc
                end
            end, [], V, We2), % vertex fold
            case lists:sort(EndCap) of
                [{d,E1},{c,E2,V1}] ->
                    We3 = wings_edge:dissolve_edge(E1, We2),
                    Pos = wings_vertex:pos(V1, We3),
                    #we{vp=Vtab0}=We4 = wings_collapse:collapse_edge(E2, V1, We3),
                    Vtab = array:set(V1, Pos, Vtab0),
                    We4#we{vp=Vtab};
                [{d,E1}|_] ->
                    #we{vp=Vtab0}=We3 = wings_edge:dissolve_edge(E1, We2),
                    RVs = wings_vertex:fold(fun(_,_,Erec,RVs0) ->
                        [wings_vertex:other(V, Erec)|RVs0]
                    end, [], V, We3),
                    Center = wings_vertex:center(RVs, We3),
                    Vtab = array:set(V, Center, Vtab0),
                    We3#we{vp=Vtab};
                _ -> We2
            end
        end, We1, NewVs0), % list foldl
        AllVs = orddict:fetch_keys(array:sparse_to_orddict(Vtab)),
        NewVs =  ordsets:intersection(lists:sort(NewVs0), AllVs),
        We#we{temp={Edges,NewVs,F}}
    end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp=PlusMinus}) -> PlusMinus end,
    wings_move:plus_minus(normal, DF, St).

find_cap_vs(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
      true ->
          CapVs = find_cap_vs(lists:sort(Acc), []),
          valid_caps(CapVs, We);
      false ->
          {Edge,Edges} = gb_sets:take_smallest(Edges0),
          #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
          find_cap_vs(Edges, We, [Va,Vb|Acc])
    end.

find_cap_vs([V,V|Vs0], CapVs) ->
    Vs = rem_v(V, Vs0),
    find_cap_vs(Vs, CapVs);
find_cap_vs([V|Vs], CapVs) ->
    find_cap_vs(Vs, [V|CapVs]);
find_cap_vs([], CapVs) ->
    CapVs.

rem_v(V, [V|Vs]) ->
    rem_v(V, Vs);
rem_v(_, Vs) ->
    Vs.

valid_caps(CapVs, We) ->
    foldl(fun(V, Acc) ->
        Count = wings_vertex:fold(fun(_, _, _, Cnt) ->
            Cnt+1
        end, 0, V, We),
        case Count rem 2 of
            0 -> [V|Acc];
            _ -> Acc
        end
    end, [], CapVs).

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    Dist = calc_extrude_dist(St0),
    MF = fun(Edges, We) -> extrude_1(Edges, Dist, We) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp=PlusMinus}) -> PlusMinus end,
    wings_move:plus_minus(Type, DF, St).

calc_extrude_dist(St) ->
    Map = fun(Edges0, We) ->
		  Edges = gb_sets:to_list(Edges0),
		  min_dist_from_edges(Edges, We)
	  end,
    Combine = fun min/2,
    wings_sel:dfold(Map, Combine, 3.0*?DEFAULT_EXTRUDE_DIST, St) / 3.0.

extrude_1(Edges, ExtrudeDist, We0) ->
    {We1,_,New,Forbidden} = extrude_edges(Edges, ExtrudeDist, We0),
    Ns = orig_normals(Edges, We1),
    We = straighten(Ns, New, We1),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    We#we{temp={Edges,NewVs,Forbidden}}.

orig_normals(Es0, #we{es=Etab,vp=Vtab}) ->
    VsVec0 = gb_sets:fold(
	       fun(E, A) ->
		       #edge{vs=Va,ve=Vb} = array:get(E, Etab),
		       Vec = e3d_vec:norm_sub(array:get(Va, Vtab),
					      array:get(Vb, Vtab)),
		       [{Va,{Vec,Vb}},{Vb,{Vec,Va}}|A]
	       end, [], Es0),
    VsVec1 = sofs:relation(VsVec0, [{vertex,info}]),
    VsVec2 = sofs:relation_to_family(VsVec1),
    VsVec = sofs:to_external(VsVec2),
    orig_normals_1(VsVec, gb_trees:from_orddict(VsVec), []).

orig_normals_1([{V,[{VecA,_},{VecB,_}]}|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
orig_normals_1([{V,[{VecA,OtherV}]}|T], VsVec, Acc) ->
    OtherRec = gb_trees:get(OtherV, VsVec),
    case [Vec || {Vec,Vertex} <- OtherRec, Vertex =/= V] of
	[VecB] ->
	    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
	_ ->
	    orig_normals_1(T, VsVec, Acc)
    end;
orig_normals_1([_|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, Acc);
orig_normals_1([], _, Acc) -> reverse(Acc).

straighten([{V,N0}|Ns], New, #we{vp=Vtab0}=We0) ->
    Pos = wings_vertex:pos(V, We0),
    Vtab = wings_vertex:fold(
	     fun(_, _, R, Vt0) ->
		     OtherV = wings_vertex:other(V, R),
		     case gb_sets:is_member(OtherV, New) of
			 false -> Vt0;
			 true ->
			     OPos0 = array:get(OtherV, Vt0),
			     Vec = e3d_vec:norm_sub(Pos, OPos0),
			     case e3d_vec:dot(N0, Vec) of
				 Dot when abs(Dot) < 0.87 ->
				     Vt0;
				 Dot when Dot < 0 ->
				     N = e3d_vec:neg(N0),
				     straighten_1(Vec, N, Pos, OtherV, OPos0, Vt0);
				 _ ->
				     straighten_1(Vec, N0, Pos, OtherV, OPos0, Vt0)
			     end
		     end
	     end, Vtab0, V, We0),
    We = We0#we{vp=Vtab},
    straighten(Ns, New, We);
straighten([], _, We) -> We.

straighten_1(Vec, N, {Cx,Cy,Cz}, OtherV, OPos0, Vt) ->
    case catch e3d_mat:rotate_s_to_t(Vec, N) of
	{'EXIT',_} -> Vt;
        Rot ->
	    M0 = e3d_mat:translate(Cx, Cy, Cz),
	    M1 = e3d_mat:mul(M0, Rot),
	    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
	    OPos = e3d_mat:mul_point(M, OPos0),
	    array:set(OtherV, OPos, Vt)
    end.

%% 
%% Common help function for actually extruding edges.
%%    
extrude_edges(Edges, ExtrudeDist, We) ->
    NoForbiddenFaces = gb_sets:empty(),
    extrude_edges(Edges, NoForbiddenFaces, ExtrudeDist, We).

extrude_edges(Edges, ForbiddenFaces, ExtrudeDist,
	      #we{next_id=Wid,es=Etab,vc=Vct0}=We0) ->
    G = digraph:new(),

    %% We update the 'vc' table here to handle the case that a
    %% a vertex's incident edge points to a completely unrelated
    %% face (i.e. a vertex is shared by two faces that have no
    %% common edge).
    Vct = gb_sets:fold(
	    fun(Edge, A) ->
		    #edge{vs=Va,ve=Vb} = Rec = array:get(Edge, Etab),
		    digraph_edge(G, ForbiddenFaces, Rec),
		    array:set(Vb, Edge, array:set(Va, Edge, A))
	    end, Vct0, Edges),
    Vs0 = digraph:vertices(G),
    Vs = sofs:to_external(sofs:domain(sofs:relation(Vs0))),
    {We1,Forbidden} =
	foldl(fun(V, A) ->
		      new_vertex(V, G, Edges, ForbiddenFaces, ExtrudeDist, A)
	      end, {We0#we{vc=Vct},[]}, Vs),
    NewVs = wings_we:new_items_as_gbset(vertex, We0, We1),
    We = connect(G, ExtrudeDist, Wid, We1),
    digraph:delete(G),
    {We,Vs,NewVs,gb_sets:from_list(Forbidden)}.

new_vertex(V, G, Edges, ForbiddenFaces, ExtrudeDist, {We0,F0}=Acc) ->
    case wings_vertex:fold(fun(E, F, R, A) -> [{E,F,R}|A] end, [], V, We0) of
	[_,_]=Es ->
	    case filter_edges(Es, Edges, ForbiddenFaces) of
		[] -> Acc;
		[{Edge,_,#edge{lf=Lf,rf=Rf}}] ->
		    New = {new,V},
		    digraph_insert(G, New, V, Lf),
		    digraph_insert(G, V, New, Lf),
		    digraph_insert(G, V, New, Rf),
		    digraph_insert(G, New, V, Rf),
		    {We0,[Edge|F0]}
	    end;
	Es0 ->
	    Es = filter_edges(Es0, Edges, ForbiddenFaces),
	    Center = wings_vertex:pos(V, We0),
	    We = foldl(fun({Edge,_,_}, W0) ->
			       new_vertex_1(V, G, Edge, Center, ExtrudeDist, W0)
		       end, We0, Es),
	    {We,F0}
    end.

new_vertex_1(V, G, Edge, Center, ExtrudeDist, #we{es=Etab}=We0) ->
    OtherPos = wings_vertex:other_pos(V, array:get(Edge, Etab), We0),
    Dir = e3d_vec:norm_sub(OtherPos, Center),
    Pos = e3d_vec:add_prod(Center, Dir, ExtrudeDist),
    {We,NewV=NewE} = wings_edge:fast_cut(Edge, Pos, We0),
    Rec = get_edge_rec(V, NewV, Edge, NewE, We),
    digraph_edge(G, Rec),
    We.

get_edge_rec(Va, Vb, EdgeA, EdgeB, #we{es=Etab}) ->
    case array:get(EdgeA, Etab) of
	#edge{vs=Va,ve=Vb}=Rec -> Rec;
	#edge{vs=Vb,ve=Va}=Rec -> Rec;
	_Other -> array:get(EdgeB, Etab)
    end.

filter_edges(Es, EdgeSet, FaceSet) ->
    foldl(fun({Edge,Face,_}=E, A) ->
		  case gb_sets:is_member(Edge, EdgeSet) orelse
		      gb_sets:is_member(Face, FaceSet) of
		      true -> A;
		      false -> [E|A]
		  end
	  end, [], Es).

digraph_edge(G, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    digraph_insert(G, Va, Vb, Lf),
    digraph_insert(G, Vb, Va, Rf).

digraph_edge(G, ForbiddenFaces, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    case gb_sets:is_member(Lf, ForbiddenFaces) of
	false -> digraph_insert(G, Va, Vb, Lf);
	true -> ok
    end,
    case gb_sets:is_member(Rf, ForbiddenFaces) of
	false -> digraph_insert(G, Vb, Va, Rf);
	true -> ok
    end.

digraph_insert(G, Va0, Vb0, Face) ->
    Va = {Va0,Face},
    Vb = {Vb0,Face},
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb).

connect(G, ExtrudeDist, Wid, We) ->
    Cs = digraph_utils:components(G),
    connect(G, Cs, ExtrudeDist, Wid, We, []).

connect(G, [C|Cs], ExtrudeDist, Wid, #we{mirror=Mirror}=We0, Closed) ->
    case [VF || {V,_}=VF <- C, V >= Wid] of
	[] ->
	    case C of
		[{_,Mirror}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, Closed);
		[{_,Face}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, [Face|Closed])
	    end;
	[_] ->
	    extrude_problem();
	[Va0,Vb0] ->
	    case digraph_get_path(G, Va0, Vb0) of
		[{_,Mirror}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, Closed);
		[{Va,Face}|Path0] ->
		    Path = [V || {V,_} <- Path0],
		    N = wings_face:normal(Face, We0),
		    We = connect_inner(Va, Path, N, Face, ExtrudeDist, We0),
		    connect(G, Cs, ExtrudeDist, Wid, We, Closed)
	    end
    end;
connect(_, [], ExtrudeDist, _, We0, Closed) ->
    We = wings_extrude_face:faces(Closed, We0),
    move_vertices(Closed, ExtrudeDist, We).

digraph_get_path(G, Va, Vb) ->
    case digraph:get_path(G, Va, Vb) of
	false -> digraph:get_path(G, Vb, Va);
	Path -> Path
    end.

connect_inner({new,Va}, [Va,Vb,{new,Vb}], N, Face, ExtrudeDist, We0) ->
    [{EdgeThrough,_,_}] = wings_vertex:edge_through(Va, Vb, We0),
    {We1,TempE} = wings_edge:fast_cut(EdgeThrough, default, We0),
    {We2,Edge} = wings_vertex:force_connect(Vb, Va, Face, We1),
    #we{vp=Vtab} = We2,
    APos = array:get(Va, Vtab),
    BPos = array:get(Vb, Vtab),
    Vec = e3d_vec:sub(APos, BPos),
    Pos1 = e3d_vec:add_prod(BPos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We3,NewE} = wings_edge:fast_cut(Edge, Pos1, We2),
    Pos2 = e3d_vec:add_prod(APos, e3d_vec:cross(Vec, N), ExtrudeDist),
    We4 = wings_edge:dissolve_edge(TempE, We3),
    {We,_} = wings_edge:fast_cut(NewE, Pos2, We4),
    wings_we_util:validate(We),
    We;
connect_inner({new,V}, [V|[B,C,_|_]=Next], N, Face, ExtrudeDist, We0) ->
    {We1,Current} = connect_one_inner(V, V, B, C, N, Face, ExtrudeDist, We0),
    #we{vp=Vtab} = We2 = connect_inner(Current, Next, N, Face, ExtrudeDist, We1),
    Edge = wings_vertex:fold(
	     fun(E, _, R, A) ->
		     case wings_vertex:other(V, R) of
			 Current -> E;
			 _ -> A
		     end
	     end, none, V, We2),
    VPos = array:get(V, Vtab),
    BPos = array:get(B, Vtab),
    Vec = e3d_vec:sub(VPos, BPos),
    Pos = e3d_vec:add_prod(VPos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We2),
    We;
connect_inner({new,_}, [A|[B,C]], _, Face, _, We0) ->
    {We1,Edge} = wings_vertex:force_connect(C, A, Face, We0),
    #we{vp=Vtab} = We1,
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    CPos = array:get(C, Vtab),
    Pos = e3d_vec:add(APos, e3d_vec:sub(CPos, BPos)),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
connect_inner(C, [B|[A,{new,_}]], N, Face, ExtrudeDist, We0) ->
    {We1,Edge} = wings_vertex:force_connect(A, C, Face, We0),
    #we{vp=Vtab} = We1,
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    Vec = e3d_vec:sub(BPos, APos),
    Pos = e3d_vec:add_prod(APos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
connect_inner(Current0, [A|[B,C,_|_]=Next], N, Face, ExtrudeDist, We0) ->
    {We,Current} = connect_one_inner(Current0, A, B, C, N, Face, ExtrudeDist, We0),
    connect_inner(Current, Next, N, Face, ExtrudeDist, We);
connect_inner(Current, [_|[_,_]=Next], N, Face, ExtrudeDist, We) ->
    connect_inner(Current, Next, N, Face, ExtrudeDist, We);
connect_inner(Current, [_,Last], _, Face, _, We0) ->
    {We,_} = wings_vertex:force_connect(Last, Current, Face, We0),
    We.

connect_one_inner(Current, A, B, C, N, Face, ExtrudeDist, We0) ->
    {We1,Edge} = wings_vertex:force_connect(B, Current, Face, We0),
    #we{vp=Vtab} = We1,
    Pos = new_vertex_pos(A, B, C, N, ExtrudeDist, Vtab),
    wings_edge:fast_cut(Edge, Pos, We1).

move_vertices([Face|Fs], ExtrudeDist, #we{vp=Vtab0}=We0) ->
    N = wings_face:normal(Face, We0),
    Vs = wings_face:vertices_ccw(Face, We0),
    Vtab = move_vertices(Vs, Vs, N, ExtrudeDist, Vtab0, Vtab0),
    We = We0#we{vp=Vtab},
    move_vertices(Fs, ExtrudeDist, We);
move_vertices([], _, We) -> We.

move_vertices([Va|[Vb,Vc|_]=Vs], First, N, ExtrudeDist, OldVtab, Vtab0) ->
    Pos = new_vertex_pos(Va, Vb, Vc, N, ExtrudeDist, OldVtab),
    Vtab = array:set(Vb, wings_util:share(Pos), Vtab0),
    move_vertices(Vs, First, N, ExtrudeDist, OldVtab, Vtab);
move_vertices([Va,Vb], [Vc,Vd|_], N, ExtrudeDist, OldVtab, Vtab) ->
    move_vertices([Va,Vb,Vc,Vd], [], N, ExtrudeDist, OldVtab, Vtab);
move_vertices([_,_], [], _, _, _, Vtab) -> Vtab.

new_vertex_pos(A, B, C, N, ExtrudeDist, Vtab) ->
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    CPos = array:get(C, Vtab),
    VecA0 = e3d_vec:norm_sub(APos, BPos),
    VecB0 = e3d_vec:norm_sub(BPos, CPos),
    VecA = e3d_vec:norm(e3d_vec:cross(VecA0, N)),
    VecB = e3d_vec:norm(e3d_vec:cross(VecB0, N)),
    Vec = average(VecA, VecB),
    e3d_vec:add_prod(BPos, Vec, ExtrudeDist).

average(Na, Nb) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    case e3d_vec:dot(N, Na) of
	Dot when abs(Dot) < 1.0E-6 ->
	    e3d_vec:add(Na, Nb);
	Dot ->
	    e3d_vec:divide(N, Dot)
    end.

scale_tv(Tv, ExtrudeDist) ->
    S = 1.0 / ExtrudeDist,
    scale_tv_1(Tv, S, []).

scale_tv_1([{Vec,Vs}|T], S, Acc) ->
    scale_tv_1(T, S, [{e3d_vec:mul(Vec, S),Vs}|Acc]);
scale_tv_1([], _, Acc) -> Acc.

-spec min_dist_from_edges(Edges, #we{}) -> float() when
      Edges :: nonempty_list(edge_num()).

min_dist_from_edges(Edges0, We) ->
    Vs = wings_vertex:from_edges(Edges0, We),
    Edges = wings_edge:from_vs(Vs, We),
    min_dist_from_edges_1(Edges, We, infinite).

min_dist_from_edges_1([E|Es], #we{es=Etab,vp=Vtab}=We, D0) ->
    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
    D = e3d_vec:dist(array:get(Va, Vtab), array:get(Vb, Vtab)),
    min_dist_from_edges_1(Es, We, min(D0, D));
min_dist_from_edges_1([], _, D) -> D.
