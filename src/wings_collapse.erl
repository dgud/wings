%%
%%  wings_collapse.erl --
%%
%%     This module contains the Collapse commands
%%     (for vertices, edges, and faces).
%%
%%  Copyright (c) 2001 Jakob Cederlund
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_collapse).
-export([collapse/1,collapse_edge/2,collapse_edge/3,
	 collapse_edges/2,fast_collapse_edge/2,collapse_vertices/2,
         collapse_faces/2]).
-export([uniform_collapse/1,clean_uniform_collapse/1]).
-include("wings.hrl").
-import(lists, [foldl/3,reverse/1,sort/1]).

%%%
%%% API.
%%%

collapse(#st{selmode=face}=St) ->
    wings_sel:map_update_sel(fun collapse_faces/2, vertex, St);
collapse(#st{selmode=vertex}=St0) ->
    St1 = wings_sel:map_update_sel(fun collapse_vertices_cmd/2, face, St0),
    case St1 of
	#st{sel=[]}=St -> St#st{selmode=vertex};
	St -> St
    end.

collapse_edges([E|Es], #we{es=Etab}=We0) ->
    We1 = fast_collapse_edge(E, We0),
    #edge{lf=Lf,rf=Rf} = array:get(E, Etab),
    We = wings_face:delete_bad_faces([Lf,Rf], We1),
    collapse_edges(Es,We);
collapse_edges([],We) -> We.

collapse_edge(Edge, #we{es=Etab}=We)->
    case array:get(Edge, Etab) of
	#edge{vs=Vkeep}=Rec ->
	    collapse_edge_1(Edge, none, Vkeep, Rec, We);
	undefined -> We
    end.

collapse_edge(Edge, Vkeep, #we{es=Etab}=We)->
    case array:get(Edge, Etab) of
	undefined -> We;
	Rec -> collapse_edge_1(Edge, none, Vkeep, Rec, We)
    end.

fast_collapse_edge(Edge, #we{es=Etab}=We)->
    #edge{vs=Vkeep,ve=Vremove} = Rec = array:get(Edge, Etab),
    internal_collapse_edge(Edge, none, Vkeep, Vremove, Rec, We).

%%% Collapse contiguous edge groups to central point
uniform_collapse(#st{selmode=edge}=St) ->
    wings_sel:map_update_sel(fun uniform_collapse_edges/2, vertex, St).

%%% Collapse contiguous edge groups to central point and cleanup any newly
%%% created isolated vertices.
clean_uniform_collapse(#st{selmode=edge}=St) ->
    wings_sel:map_update_sel(fun clean_uniform_edge_collapse/2, vertex, St).

uniform_collapse([Edge|Es],Center,#we{es=Etab}=We0) ->
    We = case array:get(Edge, Etab) of
      #edge{vs=Vkeep}=Rec ->
        collapse_edge_1(Edge, Center, Vkeep, Rec, We0);
      undefined -> We0
    end,
    uniform_collapse(Es,Center,We);
uniform_collapse([],_,We) -> We.

%% collapse_vertices(Vs, We) -> We'
%%  Remove vertices, replacing them with faces.
collapse_vertices(Vs, We0) ->
    {We,_} = do_collapse_vertices(Vs, We0),
    We.

collapse_faces(Faces, #we{fs=Ftab}=We0)->
    F1 = gb_sets:size(Faces),
    F2 = gb_trees:size(Ftab),
    case F1 =:= F2 of
	false ->
	    We1 = gb_sets:fold(fun collapse_face/2, We0, Faces),
	    We2 = wings_facemat:gc(We1),
	    check_consistency(We2),
            We = remove_bad_holes(We2),
	    Sel = wings_we:new_items_as_gbset(vertex, We0, We),
	    {We,Sel};
	true ->
	    {#we{},gb_trees:empty()}
    end.

%%%
%%% Internal functions (for collapsing faces).
%%%

collapse_face(Face, #we{fs=Ftab}=We) ->
    %% This face could have have been removed earlier because it
    %% had only two edges left.
    case gb_trees:is_defined(Face, Ftab) of
	true -> collapse_face_1(Face, We);
	false -> We
    end.

collapse_face_1(Face, We0) ->
    Vertices = wings_face:vertices_ccw(Face, We0),
    check_face_vertices(Vertices, We0),

    %% Allocate an Id for the new center vertex.
    {NewV,We1}= wings_we:new_id(We0),
    #we{es=Es0,he=He0,fs=Fs0,vc=Vct0,vp=Vs0}= We1,

    %% Delete edges and vertices.
    {Es1,Vct1,Vs1,Fs1,He1} =
	wings_face:fold(
	  fun(V, Edge, _OldRec, A) ->
		  delete_edges(V, Edge, Face, A)
	  end, {Es0,Vct0,Vs0,Fs0,He0}, Face, We1),

    %% Delete face.
    Fs2 = gb_trees:delete(Face, Fs1),

    %% Patch vertices references in edges surrounding the deleted vertices.
    {AnEdge,Es2} = foldl(fun(V, A) ->
				 patch_vtx_refs(V, NewV, We0, A)
			 end, {none,Es1}, Vertices),

    %% Insert the new vertex, if there are any edges left
    %% to connect it to.
    if
	AnEdge =:= none -> We0;
	true ->
	    Pos = wings_vertex:center(Vertices, We1),
	    Vct = array:set(NewV, AnEdge, Vct1),
	    Vs = array:set(NewV, Pos, Vs1),
	    We2 = We1#we{vc=Vct,vp=Vs,es=Es2,fs=Fs2,he=He1},
	    We = wings_vertex:fold(
		   fun(_, _, _, bad_edge) -> bad_edge;
		      (_, F, _, W) -> delete_if_bad(F, W)
		   end, We2, NewV, We2),

	    %% If no edges left, return the original object.
	    case We == bad_edge orelse wings_util:array_is_empty(We#we.es) of
		true -> We0;
		false -> We
	    end
    end.

check_face_vertices([V|Vs], We) ->
    Vlist = wings_vertex:fold(
	      fun(_, _, Rec, Acc0) ->
		      OtherV = wings_vertex:other(V, Rec),
		      [OtherV|Acc0]
	      end, [], V, We),
    check_vertices(Vlist),
    check_face_vertices(Vs, We);
check_face_vertices([], _) -> ok.

delete_edges(V, Edge, Face, {Etab0,Vct0,Vtab0,Ftab0,Htab0}) ->
    Rec = array:get(Edge, Etab0),

    %% Patch all predecessors and successor of
    %% the edge we will remove.
    Etab2 = case Rec of
		#edge{lf=Face,rf=Rf,rtpr=RP,rtsu=RS} ->
		    Etab1 = wings_edge:patch_edge(RP, RS, Rf, Edge, Etab0),
		    wings_edge:patch_edge(RS, RP, Rf, Edge, Etab1);
		#edge{rf=Face,lf=Lf,ltpr=LP,ltsu=LS} ->
		    Etab1 = wings_edge:patch_edge(LP, LS, Lf, Edge, Etab0),
		    wings_edge:patch_edge(LS, LP, Lf, Edge, Etab1)
	    end,

    %% Delete edge and vertex.
    Etab = array:reset(Edge, Etab2),
    Vct = array:reset(V, Vct0),
    Vtab = array:reset(V, Vtab0),

    %% Patch the face entry for the remaining face.
    Ftab = case Rec of
	       #edge{lf=Face,rf=AFace,rtpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0);
	       #edge{rf=Face,lf=AFace,ltpr=AnEdge} ->
		   wings_face:patch_face(AFace, Edge, AnEdge, Ftab0)
	   end,
    Htab = wings_edge:hardness(Edge, soft, Htab0),
    {Etab,Vct,Vtab,Ftab,Htab}.

%%%
%%% Internal functions (edge collapsing).
%%%

uniform_collapse_edges(Edges, We)->
    edge_collapse(fun do_uniform_collapse_edges/2, Edges, We).

clean_uniform_edge_collapse(Edges, We)->
    edge_collapse(fun do_clean_uniform_edge_collapse/2, Edges, We).

edge_collapse(Post, Edges, #we{es=Etab}=We0) ->
    N = gb_sets:size(Edges),
    case wings_util:array_entries(Etab) of
	N ->
	    %% All edges are collapsed.
	    {#we{},gb_sets:empty()};
	_ ->
	    EdgeSets = wings_edge_loop:partition_edges(Edges, We0),

	    %% Collapse.
	    We1 = foldl(fun(Es, WeAcc) ->
				Vs = wings_edge:to_vertices(Es, We0),
				Center = wings_vertex:center(Vs, We0),
				uniform_collapse(Es, Center, WeAcc)
			end, We0, EdgeSets),

	    check_consistency(We1),
	    We2 = Post(We0, We1),

            %% Remove holes that now refer to non-existing faces.
            We = remove_bad_holes(We2),

	    %% Create selection.
	    Sel0 = wings_edge:to_vertices(Edges, We0),
	    Sel1 = [V || V <- Sel0, wings_vertex:pos(V, We) =/= undefined],
	    Sel = gb_sets:from_list(Sel1),
	    {We,Sel}
    end.

do_uniform_collapse_edges(_We0, We1) ->
    We1.

do_clean_uniform_edge_collapse(#we{es=Etab0}=We0, We1) ->
    IsolatedVs1 = lists:umerge(wings_vertex:isolated(We0), vertices_w_two_edges(Etab0)),
    #we{es=Etab} = We1,
    IsolatedVs2 = vertices_w_two_edges(Etab),
    IssVs = IsolatedVs2 -- IsolatedVs1,
    We2 = collapse_vertices(IssVs, We1),
    wings_edge:dissolve_isolated_vs(wings_vertex:isolated(We2), We2).

vertices_w_two_edges(Etab) ->
    array:foldl(fun
      (undefined,_,Acc) -> Acc;
      (_,#edge{vs=V,ltsu=E1,rtpr=E1},Acc) -> [V|Acc];
      (_,#edge{ve=V,ltpr=E2,rtsu=E2},Acc) -> [V|Acc];
      (_,_,Acc) -> Acc
    end,[],Etab).

collapse_edge_1(Edge, Center, Vkeep, Rec, We0) ->
    Faces = case Rec of
		#edge{vs=Vkeep,ve=Vremove,lf=Lf,rf=Rf} -> [Lf,Rf];
		#edge{ve=Vkeep,vs=Vremove,lf=Lf,rf=Rf} -> [Lf,Rf]
    end,
    case is_waist(Vkeep, Vremove, We0) of
	true -> We0;
	false ->
	    We1 = internal_collapse_edge(Edge, Center, Vkeep, Vremove, Rec, We0),
	    We2 = delete_bad_faces(Faces, We1),
	    case We2 of
		bad_edge -> We0;
		_ ->
		  SurrFaces = wings_face:from_vs([Vkeep], We2),
		  We = delete_bad_faces(SurrFaces, We2),
		  case We of
			bad_edge -> We0;
			_ -> We
		  end
	    end
    end.

internal_collapse_edge(Edge, Center, Vkeep, Vremove, Rec,
		       #we{es=Etab0,he=Htab0,fs=Ftab0,
			   vc=Vct0,vp=Vtab0}=We)->
    Etab1 = slim_patch_vtx_refs(Vremove, Vkeep, We, Etab0),
    Etab2 = array:reset(Edge, Etab1),
    Htab = gb_sets:delete_any(Edge, Htab0),
    Vct1 = array:reset(Vremove, Vct0),

    #edge{lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    Vct = array:set(Vkeep, RP, Vct1),

    Vtab1 = if
      Center==none ->
        %% Move kept vertex. Delete the other one.
        PosKeep = array:get(Vkeep, Vtab0),
        case array:get(Vremove, Vtab0) of
          PosKeep -> Vtab0;
          PosRemove ->
            Pos = e3d_vec:average(PosKeep, PosRemove),
            array:set(Vkeep, Pos, Vtab0)
        end;
      true -> array:set(Vkeep, Center, Vtab0)
    end,
    Vtab = array:reset(Vremove, Vtab1),

    %% Patch all predecessors and successors of
    %% the edge we will remove.
    Etab3 = wings_edge:patch_edge(LP, LS, LF, Edge, Etab2),
    Etab4 = wings_edge:patch_edge(LS, LP, LF, Edge, Etab3),
    Etab5 = wings_edge:patch_edge(RP, RS, RF, Edge, Etab4),
    Etab = wings_edge:patch_edge(RS, RP, RF, Edge, Etab5),

    %% Patch the face entries for the surrounding faces.
    Ftab1= wings_face:patch_face(LF, Edge, LP, Ftab0),
    Ftab = wings_face:patch_face(RF, Edge, RP, Ftab1),

    We#we{vc=Vct,vp=Vtab,he=Htab,fs=Ftab,es=Etab}.

%%
%% The Collapse command on vertices.
%%
collapse_vertices_cmd(Vs, #we{vp=Vtab}=We0) ->
    V1 = gb_sets:size(Vs),
    V2 = wings_util:array_entries(Vtab),
    case V1 =:= V2 of
	false ->
	    {We1,Sel0} = do_collapse_vertices(gb_sets:to_list(Vs), We0),
	    check_consistency(We1),
            We = remove_bad_holes(We1),
	    AllItems = wings_sel:get_all_items(face, We),
	    Sel = gb_sets:intersection(Sel0, AllItems),
	    {We,Sel};
	true ->
	    {#we{},gb_sets:empty()}
    end.

do_collapse_vertices(Vs, We) ->
    do_collapse_vertices(Vs, We, gb_sets:empty(), [], []).

do_collapse_vertices([V|Vs], #we{vp=Vtab}=We0, Sel0, IsoAcc, Acc) ->
    case array:get(V, Vtab) of
	undefined ->
	    do_collapse_vertices(Vs, We0, Sel0, IsoAcc, Acc);
	_ ->
	    case collapse_vertex_1(V, We0, Sel0) of
		isolated ->
		    do_collapse_vertices(Vs, We0, Sel0, [V|IsoAcc], Acc);
		{We,Sel} ->
		    do_collapse_vertices(Vs, We, Sel, IsoAcc, [V|Acc])
	    end
    end;
do_collapse_vertices([], We, Sel, [], []) ->
    {We,Sel};
do_collapse_vertices([], We0, Sel, Isolated, Vs) ->
    We = wings_vertex:dissolve_isolated(Isolated, We0),

    %% Note that a vertex may be connected to two faces that
    %% have no edge in common. In that case, the vertex might
    %% still be there (it will not have been removed by
    %% wings_vertex:dissolve_isolated/2 if it is not isolated
    %% in all faces it occurs in).
    %%
    do_collapse_vertices(Isolated++Vs, We, Sel, [], []).

collapse_vertex_1(Vremove, We0, Sel0) ->
    VsEs = wings_vertex:fold(
	     fun(E, _, Rec, Acc0) ->
		     OtherV = wings_vertex:other(Vremove, Rec),
		     [{OtherV,E}|Acc0]
	     end, [], Vremove, We0),
    case VsEs of
	[_,_] ->
	    %% For performance reasons, we will remove all
	    %% isolated vertices at once.
	    isolated;
	_ ->
	    Vlist = [V || {V,_} <- VsEs],

	    %% Check for duplicated vertices.
	    check_vertices(Vlist),

	    %% Connect vertices.
	    Pairs = make_pairs(VsEs),
	    We1 = foldl(fun(Pair, W) ->
				collapse_connect(Pair, W)
			end, We0, Pairs),

	    %% Remove all original edges.
	    Edges = [E || {_,E} <- VsEs],
	    We = wings_edge:dissolve_edges(Edges, We1),

	    Faces = collapse_vtx_faces(Vlist, We, []),
	    Sel = collapse_vtx_sel(Faces, ordsets:from_list(Vlist), We, Sel0),
	    {We,Sel}
    end.

collapse_connect([{Va,EdgeA},{Vb,EdgeB}], We) ->
    Pair = [Va,Vb],
    FaceVs = wings_vertex:per_face(Pair, We),
    Edges = {EdgeA,EdgeB},
    foldl(fun({Face,Vs}, Acc) -> collapse_connect_1(Face, Vs, Edges, Acc)
	  end, We, FaceVs).

collapse_connect_1(Face, [Va,Vb], {EdgeA,EdgeB}, #we{es=Etab}=We0) ->
    case wings_vertex:edge_through(Va, Vb, Face, We0) of
	none ->
	    %% Here we will create one of the new edges that will
	    %% surround the new face that will replace the vertex
	    %% being collapsed.
	    %% 
	    %% Choose the vertex order so that the existing face
	    %% will be placed on the outside (i.e. the vertex being
	    %% collapsed will not be part of it), and the new face
	    %% inside (two original edges, soon to be dissolved, and
	    %% the new edge will surround the new face). This is
	    %% particularily important when the existing face is a
	    %% hole (the hole face would dissappear or shrink to a
	    %% triangle if it is placed inside).
	    %%
	    case array:get(EdgeA, Etab) of
		#edge{vs=Va,lf=Face,ltpr=EdgeB} ->
		    {We,_} = wings_vertex:force_connect(Vb, Va, Face, We0),
		    We;
		#edge{ve=Va,rf=Face,rtpr=EdgeB} ->
		    {We,_} = wings_vertex:force_connect(Vb, Va, Face, We0),
		    We;
		#edge{vs=Vb,rf=Face,rtsu=EdgeB} ->
		    {We,_} = wings_vertex:force_connect(Vb, Va, Face, We0),
		    We;
		#edge{ve=Vb,lf=Face,ltsu=EdgeB} ->
		    {We,_} = wings_vertex:force_connect(Vb, Va, Face, We0),
		    We;
		_ ->
		    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
		    We
	    end;
	_ -> We0
    end;
collapse_connect_1(_, _, _, We) -> We.

collapse_vtx_faces([V|Vs], We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(_, Face, _, A) ->
		    [Face|A]
	    end, Acc0, V, We),
    collapse_vtx_faces(Vs, We, Acc);
collapse_vtx_faces([], _, Acc) ->
    ordsets:from_list(Acc).

collapse_vtx_sel([Face|Fs], NewVs, We, Sel) ->
    case ordsets:from_list(wings_face:vertices_ccw(Face, We)) of
	NewVs -> gb_sets:add(Face, Sel);
	_ -> collapse_vtx_sel(Fs, NewVs, We, Sel)
    end;
collapse_vtx_sel([], _, _, Sel) -> Sel.

make_pairs([H|_]=L) ->
    make_pairs(L, H, []).

make_pairs([A], F, Acc) -> [[A,F]|Acc];
make_pairs([A|[B|_]=T], F, Acc) -> make_pairs(T, F, [[A,B]|Acc]).

check_vertices(Vs0) ->
    check_vertices_1(sort(Vs0)).

check_vertices_1([V,V|_]) ->
    wings_u:error_msg(?STR(check_vertices_1,1,"Non-collapsible vertex") ++ " ("
		     ++ integer_to_list(V) ++ ") - "
		     ++?STR(check_vertices_1,2,"would leave waist.\n"));
check_vertices_1([_|Vs]) ->
    check_vertices(Vs);
check_vertices_1([]) -> ok.

is_waist(Va, Vb, We) ->
    N = wings_vertex:fold(
	  fun(_, _, Rec, N) ->
		  case wings_vertex:other(Va, Rec) of
		      Vb -> N+1;
		      _Other -> N
		  end
	  end, 0, Va, We),
    N =/= 1.

slim_patch_vtx_refs(OldV, NewV, We, Acc) ->
    wings_vertex:fold(
      fun(Edge, _, Rec, Tab) ->
	      case Rec of
		  #edge{vs=OldV} ->
		      array:set(Edge, Rec#edge{vs=NewV}, Tab);
		  #edge{ve=OldV} ->
		      array:set(Edge, Rec#edge{ve=NewV}, Tab)
	      end
      end, Acc, OldV, We).

patch_vtx_refs(OldV, NewV, We, {_,_}=Acc) ->
    wings_vertex:fold(
      fun(Edge, _, _, {_,Tab}=A) ->
	      case array:get(Edge, Tab) of
		  #edge{vs=OldV}=Rec ->
		      {Edge,array:set(Edge, Rec#edge{vs=NewV}, Tab)};
		  #edge{ve=OldV}=Rec ->
		      {Edge,array:set(Edge, Rec#edge{ve=NewV}, Tab)};
		  undefined -> A		%An deleted edge.
	      end
      end, Acc, OldV, We).

check_consistency(We) ->
    case wings_we:is_consistent(We) of
	true -> ok;
	false ->
	    Msg = ?STR(check_consistency,1,"Collapsing would cause an inconsistent object structure."),
	    wings_u:error_msg(Msg)
    end.


delete_bad_faces(_, bad_edge) -> bad_edge;
delete_bad_faces([F|Fs], We0) ->
    We = delete_if_bad(F, We0),
    delete_bad_faces(Fs, We);
delete_bad_faces([], We) -> We.

%% Delete a face if it only has two edges.

delete_if_bad(Face, #we{fs=Ftab,es=Etab}=We) ->
    case gb_trees:lookup(Face, Ftab) of
	{value,Edge} ->
	    case array:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		    bad_edge;
		#edge{ltpr=Same,ltsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		#edge{rtpr=Same,rtsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		_ -> We
	    end;
	none -> We
    end.

%% Remove holes refering to faces that no longer exist.

remove_bad_holes(#we{fs=Ftab,holes=Holes0}=We) ->
    Holes = ordsets:intersection(gb_trees:keys(Ftab), Holes0),
    We#we{holes=Holes}.
