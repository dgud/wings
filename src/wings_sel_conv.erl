%%
%%  wings_sel_conv.erl --
%%
%%     Conversion of selections.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_sel_conv.erl,v 1.5 2005/01/09 09:30:38 bjorng Exp $
%%

-module(wings_sel_conv).
-export([mode/2,more/1,less/1]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

mode(Mode, #st{sel=[]}=St) ->
    St#st{selmode=Mode,sh=false};
mode(Mode, St) ->
    mode_1(Mode, St#st{sh=false}).

mode_1(vertex, St) -> vertex_selection(St);
mode_1(edge, St) -> edge_selection(St);
mode_1(face, St) -> face_selection(St);
mode_1(body, St) -> body_selection(St).

more(#st{selmode=vertex}=St) ->
    vertex_more(St);
more(#st{selmode=edge}=St) ->
    edge_more(St);
more(#st{selmode=face}=St) ->
    face_more(St);
more(St) -> St.

less(#st{selmode=vertex}=St) ->
    vertex_less(St);
less(#st{selmode=edge}=St) ->
    edge_less(St);
less(#st{selmode=face}=St) ->
    face_less(St);
less(St) -> St.

%%
%% Convert the current selection to a vertex selection.
%%

vertex_selection(#st{selmode=body}=St) ->
    conv_sel(
      fun(_, We) ->
	      gb_sets:from_list(wings_we:visible_vs(We))
      end, vertex, St);
vertex_selection(#st{selmode=face}=St) ->
    conv_sel(
      fun(Fs, We) ->
	      gb_sets:from_ordset(wings_vertex:from_faces(Fs, We))
      end, vertex, St);
vertex_selection(#st{selmode=edge}=St) ->
    conv_sel(
      fun(Es, We) ->
	      gb_sets:from_ordset(wings_vertex:from_edges(Es, We))
      end, vertex, St);
vertex_selection(#st{selmode=vertex}=St) ->
    vertex_more(St).

vertex_more(St) ->
    conv_sel(
      fun(Vs, We) ->
	      gb_sets:fold(
		fun(V, S0) ->
			wings_vertex:fold(
			  fun(_, F, Rec, S) when F >= 0 ->
				  Other = wings_vertex:other(V, Rec),
				  gb_sets:add(Other, S);
			     (_, _, Rec, S) ->
				  Other = wings_vertex:other(V, Rec),
				  case vertex_visible(Other, We) of
				      true -> gb_sets:add(Other, S);
				      false -> S
				  end
			  end, S0, V, We)
		end, Vs, Vs)
      end, vertex, St).

vertex_less(St) ->
    conv_sel(
      fun(Vs, We) ->
	      gb_sets:fold(
		fun(V, A) ->
			Set = wings_vertex:fold(
				fun(_, _, Rec, S) ->
					Other = wings_vertex:other(V, Rec),
					gb_sets:add(Other, S)
				end, gb_sets:empty(), V, We),
			case gb_sets:is_subset(Set, Vs) of
			    true -> gb_sets:add(V, A);
			    false -> A
			end
		end, gb_sets:empty(), Vs)
      end, vertex, St).

vertex_visible(V, We) ->
    wings_vertex:fold(
      fun(_, F, _, _) when F >= 0 -> true;
	 (_, _, _, A) -> A
      end, false, V, We).

%%
%% Convert the current selection to an edge selection.
%%

edge_selection(#st{selmode=body}=St) ->
    conv_sel(fun(_, We) ->
		     gb_sets:from_ordset(wings_we:visible_edges(We))
	     end, edge, St);
edge_selection(#st{selmode=face}=St) ->
    conv_sel(
      fun(Faces, We) ->
	      wings_edge:from_faces(Faces, We)
      end, edge, St);
edge_selection(#st{selmode=edge}=St) ->
    conv_sel(
      fun(Edges, We) ->
	      edge_extend_sel(Edges, We)
      end, edge, St);
edge_selection(#st{selmode=vertex}=St) ->
    conv_sel(fun(Vs, We) -> wings_edge:from_vs(Vs, We) end, edge, St).

edge_more(St) ->
    conv_sel(fun edge_more/2, edge, St).

edge_more(Edges, We) ->
    Vs = wings_edge:to_vertices(Edges, We),
    Es = adjacent_edges(Vs, We, Edges),
    wings_we:visible_edges(Es, We).

edge_less(St) ->
    conv_sel(fun(Edges, #we{es=Etab}=We) ->
		     Vs0 = edge_less_1(Edges, Etab),
		     Vs = ordsets:from_list(Vs0),
		     AdjEdges = adjacent_edges(Vs, We),
		     gb_sets:subtract(Edges, AdjEdges)
	     end, edge, St).

edge_less_1(Edges, Etab) ->
    foldl(fun(Edge, A0) ->
		  Rec = gb_trees:get(Edge, Etab),
		  #edge{vs=Va,ve=Vb,
			ltpr=LP,ltsu=LS,
			rtpr=RP,rtsu=RS} = Rec,
		  A = case gb_sets:is_member(LS, Edges) andalso
			  gb_sets:is_member(RP, Edges) of
			  true -> A0;
			  false -> [Va|A0]
		      end,
		  case gb_sets:is_member(LP, Edges) andalso
		      gb_sets:is_member(RS, Edges) of
		      true -> A;
		      false -> [Vb|A]
		  end
	  end, [], gb_sets:to_list(Edges)).

adjacent_edges(Vs, We) ->
    adjacent_edges(Vs, We, gb_sets:empty()).

adjacent_edges(Vs, We, Acc) ->
    foldl(fun(V, A) ->
		  wings_vertex:fold(
		    fun(Edge, _, _, AA) ->
			    gb_sets:add(Edge, AA)
		    end, A, V, We)
	  end, Acc, Vs).

edge_extend_sel(Es0, #we{es=Etab}=We) ->
    Es = foldl(fun(Edge, S) ->
		       #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} =
			   gb_trees:get(Edge, Etab),
		       gb_sets:union(S, gb_sets:from_list([LP,LS,RP,RS]))
	       end, Es0, gb_sets:to_list(Es0)),
    wings_we:visible_edges(Es, We).

%%
%% Convert the current selection to a face selection.
%%

face_selection(#st{selmode=body}=St) ->
    conv_sel(
      fun(_, We) ->
	      wings_sel:get_all_items(face, We)
      end, face, St);
face_selection(#st{selmode=face}=St) ->
    conv_sel(
      fun(Sel0, We) ->
	      Sel = wings_face:extend_border(Sel0, We),
	      remove_invisible_faces(Sel, We)
      end, face, St);
face_selection(#st{selmode=edge}=St) ->
    conv_sel(fun(Es, We) ->
		     Fs = wings_face:from_edges(Es, We),
		     remove_invisible_faces(Fs, We)
	     end, face, St);
face_selection(#st{selmode=vertex}=St) ->
    conv_sel(fun(Vs, We) ->
		     Fs = wings_we:visible(wings_face:from_vs(Vs, We), We),
		     gb_sets:from_ordset(Fs)
	     end, face, St).

face_more(St) ->
    conv_sel(fun face_more/2, face, St).

face_more(Fs0, We) ->
    Fs = foldl(fun(Face, A) ->
		       do_face_more(Face, We, A)
	       end, Fs0, gb_sets:to_list(Fs0)),
    remove_invisible_faces(Fs, We).

do_face_more(Face, We, Acc) ->
    foldl(fun(V, A0) ->
		  wings_vertex:fold(
		    fun(_, AFace, _, A1) ->
			    gb_sets:add(AFace, A1)
		    end, A0, V, We)
	  end, Acc, wings_face:vertices_ccw(Face, We)).

face_less(St) ->
    conv_sel(
      fun(Faces0, We) ->
	      Faces1 = gb_sets:to_list(Faces0),
	      Faces = sofs:from_external(Faces1, [face]),
	      VsFs0 = vs_faces(Faces1, We),
	      VsFs = sofs:relation(VsFs0, [{vertex,face}]),
	      Vs = sofs:to_external(sofs:domain(VsFs)),
	      BorderVs0 = vs_bordering(Vs, Faces0, We),
	      BorderVs = sofs:from_external(BorderVs0, [vertex]),
	      BorderFs = sofs:restriction(VsFs, BorderVs),
	      Border = sofs:range(BorderFs),
	      Sel = sofs:difference(Faces, Border),
	      gb_sets:from_ordset(sofs:to_external(Sel))
      end, face, St).

vs_faces(Faces, We) ->
    wings_face:fold_faces(fun(Face, V, _, _, A) ->
				  [{V,Face}|A]
			  end, [], Faces, We).

vs_bordering(Vs, FaceSet, We) ->
    B = foldl(fun(V, A) ->
		      case vtx_bordering(V, FaceSet, We) of
			  true -> [V|A];
			  false -> A
		      end
	      end, [], Vs),
    ordsets:from_list(B).

vtx_bordering(V, FaceSet, We) ->
    wings_vertex:fold(
      fun(_, _, _, true) -> true;
	 (_, Face, _, false) ->
	      not gb_sets:is_member(Face, FaceSet)
      end, false, V, We).

remove_invisible_faces(Fs, #we{mirror=none}) ->
    remove_invisible_faces_1(Fs);
remove_invisible_faces(Fs, #we{mirror=Face}) ->
    remove_invisible_faces_1(gb_sets:delete_any(Face, Fs)).

remove_invisible_faces_1(Fs) ->
    case gb_sets:is_empty(Fs) of
	true -> Fs;
	false ->
	    case gb_sets:smallest(Fs) of
		F when F < 0 -> remove_invisible_faces_2(Fs);
		_ -> Fs
	    end
    end.
				    
remove_invisible_faces_2(Fs0) ->
    case gb_sets:is_empty(Fs0) of
	true -> Fs0;
	false ->
	    case gb_sets:take_smallest(Fs0) of
		{F,Fs} when F < 0 ->
		    remove_invisible_faces_2(Fs);
		{_,_} ->
		    gb_sets:balance(Fs0)
	    end
    end.

%%
%% Convert the current selection to a body selection.
%%

body_selection(#st{sel=Sel0}=St) ->
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || {Id,_} <- Sel0],
    wings_sel:set(body, Sel, St).

%%%
%%% Utilities.
%%%

conv_sel(F, NewMode, St) ->
    Sel = wings_sel:fold(
	    fun(Items0, #we{id=Id}=We, Acc) ->
		    Items = F(Items0, We),
		    case gb_sets:is_empty(Items) of
			true -> Acc;
			false -> [{Id,Items}|Acc]
		    end
	    end, [], St),
    St#st{selmode=NewMode,sel=reverse(Sel)}.
