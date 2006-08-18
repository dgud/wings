%%
%%  wings_collapse.erl --
%%
%%     This module contains the Collapse commands
%%     (for vertices, edges, and faces).
%%
%%  Copyright (c) 2001 Jakob Cederlund
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_collapse.erl,v 1.47 2005/01/23 07:35:59 bjorng Exp $
%%

-module(wings_collapse).
-export([collapse/1,collapse_edge/2,collapse_edge/3,
	 collapse_edges/2,fast_collapse_edge/2,collapse_vertices/2]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,sort/1,keymember/3,member/2]).

%%%
%%% API.
%%%

collapse(#st{selmode=face}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_faces/3, [], St0),
    wings_sel:set(vertex, Sel, St);
collapse(#st{selmode=edge}=St0) ->
    {St,Sel} = wings_sel:mapfold(fun collapse_edges/3, [], St0),
    wings_sel:valid_sel(wings_sel:set(vertex, Sel, St));
collapse(#st{selmode=vertex}=St0) ->
    {St1,Sel} = wings_sel:mapfold(fun collapse_vertices_cmd/3, [], St0),
    case wings_sel:valid_sel(wings_sel:set(face, Sel, St1)) of
	#st{sel=[]}=St -> St#st{selmode=vertex};
	St -> St
    end.

collapse_edges(Es, We0) ->
    We1 = collapse_edges_1(Es, We0),
    Faces = wings_face:from_edges(Es, We0),
    case wings_face:delete_bad_faces(Faces, We1) of
	bad_edge -> We0;
	We -> We
    end.

collapse_edge(Edge, #we{es=Etab}=We)->
    case gb_trees:lookup(Edge, Etab) of
	{value,#edge{vs=Vkeep}=Rec} -> 
	    collapse_edge_1(Edge, Vkeep, Rec, We);
	none -> We
    end.

collapse_edge(Edge, Vkeep, #we{es=Etab}=We)->
    case gb_trees:lookup(Edge, Etab) of
	{value,Rec} -> 
	    collapse_edge_1(Edge, Vkeep, Rec, We);
	none -> We
    end.

fast_collapse_edge(Edge, #we{es=Etab}=We)->
    #edge{vs=Vkeep,ve=Vremove} = Rec = gb_trees:get(Edge, Etab),
    internal_collapse_edge(Edge, Vkeep, Vremove, Rec, We).

%% collapse_vertices(Vs, We) -> We'
%%  Remove vertices, replacing them with faces.
collapse_vertices(Vs, We0) ->
    {We,_} = do_collapse_vertices(Vs, We0),
    We.

%%%
%%% Internal functions (for collapsing faces).
%%%

collapse_faces(Faces, #we{id=Id}=We0, SelAcc)->
    We1 = foldl(fun collapse_face/2, We0, gb_sets:to_list(Faces)),
    We = wings_facemat:gc(We1),
    check_consistency(We),
    Sel = wings_we:new_items_as_gbset(vertex, We0, We),
    {We,[{Id,Sel}|SelAcc]}.

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
	    Vct = gb_trees:insert(NewV, AnEdge, Vct1),
	    Vs = gb_trees:insert(NewV, Pos, Vs1),
	    We2 = We1#we{vc=Vct,vp=Vs,es=Es2,fs=Fs2,he=He1},
	    We = wings_vertex:fold(
		   fun(_, _, _, bad_edge) -> bad_edge;
		      (_, F, _, W) -> delete_if_bad(F, W)
		   end, We2, NewV, We2),

	    %% If no edges left, return the original object.
	    case We == bad_edge orelse gb_trees:is_empty(We#we.es) of
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
    Rec = gb_trees:get(Edge, Etab0),

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
    Etab = gb_trees:delete(Edge, Etab2),
    Vct = gb_trees:delete(V, Vct0),
    Vtab = gb_trees:delete(V, Vtab0),

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

collapse_edges_1([E|Es], We0) ->
    We = fast_collapse_edge(E, We0),
    collapse_edges_1(Es, We);
collapse_edges_1([], We) -> We.
	    
collapse_edges(Edges0, #we{id=Id,es=Etab}=We0, SelAcc)->
    Edges = gb_sets:to_list(Edges0),
    We = foldl(fun collapse_edge/2, We0, Edges),
    check_consistency(We),
    Sel = foldl(fun(Edge, A) ->
			#edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
			gb_sets:add(Va, gb_sets:add(Vb, A))
		end, gb_sets:empty(), Edges),
    {We,[{Id,Sel}|SelAcc]}.

collapse_edge_1(Edge, Vkeep, Rec, We0) ->
    Faces = case Rec of
		#edge{vs=Vkeep,ve=Vremove,lf=Lf,rf=Rf} -> [Lf,Rf];
		#edge{ve=Vkeep,vs=Vremove,lf=Lf,rf=Rf} -> [Lf,Rf]
    end,
    case is_waist(Vkeep, Vremove, We0) of
	true -> We0;
	false ->
	    We1 = internal_collapse_edge(Edge, Vkeep, Vremove, Rec, We0),
	    We2 = delete_bad_faces(Faces, We1),
	    SurrFaces = wings_face:from_vs([Vkeep], We2),
	    We = delete_bad_faces(SurrFaces, We2),
	    case We of
		bad_edge -> We0;
		_ -> We
	    end
    end.


internal_collapse_edge(Edge, Vkeep, Vremove, Rec,
		       #we{es=Etab0,he=Htab0,fs=Ftab0,
			   vc=Vct0,vp=Vtab0}=We)->
    Etab1 = slim_patch_vtx_refs(Vremove, Vkeep, We, Etab0),
    Etab2 = gb_trees:delete(Edge, Etab1),
    Htab = gb_sets:delete_any(Edge, Htab0),
    Vct1 = gb_trees:delete(Vremove, Vct0),
	    
    #edge{lf=LF,rf=RF,ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    Vct = gb_trees:update(Vkeep, RP, Vct1),

    %% Move kept vertex. Delete the other one.
    PosKeep = gb_trees:get(Vkeep, Vtab0),
    Vtab1 = case gb_trees:get(Vremove, Vtab0) of
		PosKeep -> Vtab0;
		PosRemove ->
		    Pos = e3d_vec:average(PosKeep, PosRemove),
		    gb_trees:update(Vkeep, Pos, Vtab0)
	    end,
    Vtab = gb_trees:delete(Vremove, Vtab1),

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
collapse_vertices_cmd(Vs, #we{id=Id}=We0, SelAcc) ->
    {We,Sel} = do_collapse_vertices(gb_sets:to_list(Vs), We0),
    check_consistency(We),
    {We,[{Id,Sel}|SelAcc]}.

do_collapse_vertices(Vs, We) ->
    do_collapse_vertices(Vs, We, gb_sets:empty(), [], []).

do_collapse_vertices([V|Vs], #we{vp=Vtab}=We0, Sel0, IsoAcc, Acc) ->
    case gb_trees:is_defined(V, Vtab) of
	false ->
	    do_collapse_vertices(Vs, We0, Sel0, IsoAcc, Acc);
	true ->
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
	    Vlist = reverse([V || {V,_} <- VsEs]),
	    check_vertices(Vlist),

	    %% Connect vertices.
	    Pairs = make_pairs(Vlist),
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

collapse_connect(Pair, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Pair, We),
    foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
	     ({Face,Vs}, Acc) -> collapse_connect_1(Face, Vs, Acc)
	  end, We, FaceVs).

collapse_connect_1(Face, [Va,Vb], We0) ->
    case wings_vertex:edge_through(Va, Vb, Face, We0) of
	none ->
	    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
	    We;
	_ -> We0
    end;
collapse_connect_1(_, _, We) -> We.

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
    wings_u:error(?STR(check_vertices_1,1,"Non-collapsible vertex") ++ " ("
		     ++ integer_to_list(V) ++ ") -"
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
		      gb_trees:update(Edge, Rec#edge{vs=NewV}, Tab);
		  #edge{ve=OldV} ->
		      gb_trees:update(Edge, Rec#edge{ve=NewV}, Tab)
	      end
      end, Acc, OldV, We).

patch_vtx_refs(OldV, NewV, We, {_,_}=Acc) ->
    wings_vertex:fold(
      fun(Edge, _, _, {_,Tab}=A) ->
	      case gb_trees:lookup(Edge, Tab) of
		  {value,#edge{vs=OldV}=Rec} ->
		      {Edge,gb_trees:update(Edge, Rec#edge{vs=NewV}, Tab)};
		  {value,#edge{ve=OldV}=Rec} ->
		      {Edge,gb_trees:update(Edge, Rec#edge{ve=NewV}, Tab)};
		  none -> A		%An deleted edge.
	      end
      end, Acc, OldV, We).

check_consistency(We) ->
    case wings_we:is_consistent(We) of
	true -> ok;
	false ->
	    Msg = ?STR(check_consistency,1,"Collapsing would cause an inconsistent object structure."),
	    wings_u:error(Msg)
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
	    case gb_trees:get(Edge, Etab) of
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
