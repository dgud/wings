%%
%%  wings_sel_conv.erl --
%%
%%     Conversion of selections.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_sel_conv).
-export([mode/2,mode/3,more/1,more/3,less/1,less/3]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

-spec mode(ToMode, #st{}) -> #st{} when
      ToMode :: wings_sel:mode().

mode(Mode, #st{sel=[]}=St) ->
    St#st{selmode=Mode,sh=false};
mode(Mode, St) ->
    mode_1(Mode, St#st{sh=false}).

-spec mode(ToMode, {FromMode,InItems}, #we{}) -> OutItems when
      ToMode :: wings_sel:mode(),
      FromMode :: wings_sel:mode(),
      InItems :: wings_sel:item_set(),
      OutItems :: wings_sel:item_set().

mode(vertex, {FromMode,Items}, We) ->
    vertex_selection(FromMode, Items, We);
mode(edge, {FromMode,Items}, We) ->
    edge_selection(FromMode, Items, We);
mode(face, {FromMode,Items}, We) ->
    face_selection(FromMode, Items, We);
mode(body, {FromMode,_}, #we{}) when is_atom(FromMode) ->
    gb_sets:singleton(0).

-spec more(#st{}) -> #st{}.

more(#st{selmode=vertex}=St) ->
    vertex_more(St);
more(#st{selmode=edge}=St) ->
    edge_more(St);
more(#st{selmode=face}=St) ->
    face_more(St);
more(St) -> St.

-spec less(#st{}) -> #st{}.

less(#st{selmode=vertex}=St) ->
    vertex_less(St);
less(#st{selmode=edge}=St) ->
    edge_less(St);
less(#st{selmode=face}=St) ->
    face_less(St);
less(St) -> St.

-type some_modes() :: 'vertex' | 'edge' | 'face'.

-spec more(Mode, ItemsIn, #we{}) -> ItemsOut when
      Mode :: some_modes(),
      ItemsIn :: wings_sel:item_set(),
      ItemsOut :: wings_sel:item_set().

more(vertex, Vs, We) ->
    vertex_more(Vs, We);
more(edge, Es, We) ->
    edge_more(Es, We);
more(face, Fs, We) ->
    face_more(Fs, We).

-spec less(Mode, ItemsIn, #we{}) -> ItemsOut when
      Mode :: some_modes(),
      ItemsIn :: wings_sel:item_set(),
      ItemsOut :: wings_sel:item_set().

less(vertex, Vs, We) ->
    vertex_less(Vs, We);
less(edge, Es, We) ->
    edge_less(Es, We);
less(face, Fs, We) ->
    face_less(Fs, We).

%%%
%%% Local functions.
%%%

mode_1(vertex, St) -> vertex_selection(St);
mode_1(edge, St) -> edge_selection(St);
mode_1(face, St) -> face_selection(St);
mode_1(body, St) -> body_selection(St).

%%
%% Convert the current selection to a vertex selection.
%%

vertex_selection(#st{selmode=Mode}=St) ->
    wings_sel:update_sel(
      fun(Items, We) ->
	      vertex_selection(Mode, Items, We)
      end, vertex, St).

vertex_selection(body, _, We) ->
    gb_sets:from_list(wings_we:visible_vs(We));
vertex_selection(face, Fs, We) ->
    gb_sets:from_ordset(wings_vertex:from_faces(Fs, We));
vertex_selection(edge, Es, We) ->
    gb_sets:from_ordset(wings_vertex:from_edges(Es, We));
vertex_selection(vertex, Vs, We) ->
    vertex_more(Vs, We).

vertex_more(St) ->
    wings_sel:update_sel(fun vertex_more/2, St).

vertex_more(Vs, We) ->
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
      end, Vs, Vs).

vertex_less(St) ->
    wings_sel:update_sel(fun vertex_less/2, St).

vertex_less(Vs, We) ->
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
      end, gb_sets:empty(), Vs).

vertex_visible(V, We) ->
    wings_vertex:fold(
      fun(_, F, _, _) when F >= 0 -> true;
	 (_, _, _, A) -> A
      end, false, V, We).

%%
%% Convert the current selection to an edge selection.
%%

edge_selection(#st{selmode=Mode}=St) ->
    wings_sel:update_sel(
      fun(Items, We) ->
              edge_selection(Mode, Items, We)
      end, edge, St).

edge_selection(body, _, We) ->
    gb_sets:from_ordset(wings_we:visible_edges(We));
edge_selection(face, Faces, We) ->
    wings_edge:from_faces(Faces, We);
edge_selection(edge, Edges, We) ->
    edge_extend_sel(Edges, We);
edge_selection(vertex, Vs, We) ->
    wings_edge:from_vs(Vs, We).

edge_more(St) ->
    wings_sel:update_sel(fun edge_more/2, edge, St).

edge_more(Edges, We) ->
    Vs = wings_edge:to_vertices(Edges, We),
    Es = adjacent_edges(Vs, We, Edges),
    wings_we:visible_edges(Es, We).

edge_less(St) ->
    wings_sel:update_sel(fun edge_less/2, St).

edge_less(Edges, #we{es=Etab}=We) ->
    Vs0 = edge_less_1(Edges, Etab),
    Vs = ordsets:from_list(Vs0),
    AdjEdges = adjacent_edges(Vs, We),
    gb_sets:subtract(Edges, AdjEdges).

edge_less_1(Edges, Etab) ->
    gb_sets:fold(
      fun(Edge, A0) ->
	      Rec = array:get(Edge, Etab),
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
      end, [], Edges).

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
    Es = gb_sets:fold(
	   fun(Edge, S) ->
		   #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} =
		       array:get(Edge, Etab),
		   gb_sets:union(S, gb_sets:from_list([LP,LS,RP,RS]))
	   end, Es0, Es0),
    wings_we:visible_edges(Es, We).

%%
%% Convert the current selection to a face selection.
%%

face_selection(#st{selmode=Mode}=St) ->
    wings_sel:update_sel(
      fun(Items, We) ->
	      face_selection(Mode, Items, We)
      end, face, St).

face_selection(body, _, We) ->
    wings_sel:get_all_items(face, We);
face_selection(face, Fs0, We) ->
    Fs = wings_face:extend_border(Fs0, We),
    remove_invisible_faces(Fs);
face_selection(edge, Es, We) ->
    Fs = wings_face:from_edges(Es, We),
    remove_invisible_faces(Fs);
face_selection(vertex, Vs, We) ->
    Fs = wings_we:visible(wings_face:from_vs(Vs, We), We),
    gb_sets:from_ordset(Fs).

face_more(St) ->
    wings_sel:update_sel(fun face_more/2, face, St).

face_more(Fs0, We) ->
    Fs = gb_sets:fold(fun(Face, A) ->
			      do_face_more(Face, We, A)
		      end, Fs0, Fs0),
    remove_invisible_faces(Fs).

do_face_more(Face, We, Acc) ->
    foldl(fun(V, A0) ->
		  wings_vertex:fold(
		    fun(_, AFace, _, A1) ->
			    gb_sets:add(AFace, A1)
		    end, A0, V, We)
	  end, Acc, wings_face:vertices_ccw(Face, We)).

face_less(St) ->
    wings_sel:update_sel(fun face_less/2, St).

face_less(Faces0, #we{mirror=Mirror}=We) ->
    Es0 = wings_face:outer_edges(Faces0, We),
    MirEs = case Mirror of
                none -> [];
                _ -> wings_face:to_edges([Mirror], We)
            end,
    Es = Es0 -- MirEs,
    Faces = wings_face:from_edges(Es, We),
    gb_sets:difference(Faces0, Faces).

remove_invisible_faces(Fs) ->
    case gb_sets:is_empty(Fs) of
	true -> Fs;
	false ->
	    case gb_sets:smallest(Fs) of
		F when F < 0 -> remove_invisible_faces_1(Fs);
		_ -> Fs
	    end
    end.

remove_invisible_faces_1(Fs0) ->
    case gb_sets:is_empty(Fs0) of
	true -> Fs0;
	false ->
	    case gb_sets:take_smallest(Fs0) of
		{F,Fs} when F < 0 ->
		    remove_invisible_faces_1(Fs);
		{_,_} ->
		    gb_sets:balance(Fs0)
	    end
    end.

%%
%% Convert the current selection to a body selection.
%%

body_selection(St) ->
    Zero = gb_sets:singleton(0),
    wings_sel:update_sel(fun(_, _) -> Zero end, body, St).
