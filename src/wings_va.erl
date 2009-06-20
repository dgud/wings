%%
%%  wings_va.erl --
%%
%%     This module implements vertex attributes.
%%
%%  Copyright (c) 2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_va).
-export([set_vertex_color/3,set_edge_color/3,set_face_color/3,set_body_color/2,
	 info/2,face_attr/3,face_attr/4]).

-include("wings.hrl").

-import(lists, [any/2]).

set_vertex_color(Vs, Color, We) ->
    gb_sets:fold(fun(V, W) ->
			 set_vertex_color_1(V, Color, W)
		 end, We#we{mode=vertex}, Vs).

set_edge_color(Es, Color, We) ->
    gb_sets:fold(fun(E, W) ->
			 set_edge_color_1(E, Color, W)
		 end, We#we{mode=vertex}, Es).

set_face_color(Fs, Color, We) ->
    gb_sets:fold(fun(F, W) ->
			 set_face_color_1(F, Color, W)
		 end, We#we{mode=vertex}, Fs).

set_body_color(Color, #we{es=Etab0}=We) ->
    Etab = array:sparse_map(fun(_, Rec) ->
				    Rec#edge{a=Color,b=Color}
			    end, Etab0),
    We#we{es=Etab,mode=vertex}.

%% info(We, St) -> [color|uv]
%%  Return a list of the available vertex attributes for the We.
%%
info(#we{mode=vertex}, _St) ->
    [color];
info(#we{mode=material}=We, #st{mat=Mtab}) ->
    Used = wings_facemat:used_materials(We),
    AnyTexture = any(fun(Mat) ->
			     wings_material:has_texture(Mat, Mtab)
		     end, Used),
    case AnyTexture of
	false -> [];
	true -> [uv]
    end.

face_attr(What, Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    face_attr(What, Face, Edge, We).

%% face_attr(What, Face, Edge, We) -> [Attribute]
%%     What = uv | color.
%%     Attribute = {_,_,_} | {_,_} | none
%%  Return vertex attributes for the all vertices in the face.
%%
face_attr(uv, Face, Edge, #we{es=Etab}) ->
    face_attr(Edge, Etab, Face, Edge, []);
face_attr(color, Face, Edge, #we{es=Etab}) ->
    face_attr(Edge, Etab, Face, Edge, []).

%%%
%%% Local functions.
%%%

set_vertex_color_1(V, Color, #we{es=Etab0}=We) ->
    Etab = wings_vertex:fold(
	     fun(Edge, _Face, Rec0, Es) ->
		     Rec = case Rec0 of
			       #edge{vs=V} -> Rec0#edge{a=Color};
			       #edge{ve=V} -> Rec0#edge{b=Color}
			   end,
		     array:set(Edge, Rec, Es)
	     end, Etab0, V, We),
    We#we{es=Etab}.

set_edge_color_1(E, Color, #we{es=Etab0}=We) ->
    Rec0 = #edge{vs=Va,ve=Vb,rtpr=Rp,ltpr=Lp} = array:get(E, Etab0),
    Rec = Rec0#edge{a=Color,b=Color},
    Etab1 = array:set(E, Rec, Etab0),
    Etab2 = set_edge_color_2(Rp, Va, Color, Etab1),
    Etab = set_edge_color_2(Lp, Vb, Color, Etab2),
    We#we{es=Etab}.

set_edge_color_2(E, V, Color, Etab) ->
    Rec = case array:get(E, Etab) of
	      #edge{vs=V}=Rec0 -> Rec0#edge{a=Color};
	      #edge{ve=V}=Rec0 -> Rec0#edge{b=Color}
	  end,
    array:set(E, Rec, Etab).

set_face_color_1(F, Color, #we{es=Etab0}=We) ->
    Etab = wings_face:fold(
	     fun(_V, Edge, Rec0, Es) ->
		     Rec = case Rec0 of
			       #edge{lf=F} -> Rec0#edge{a=Color};
			       #edge{rf=F} -> Rec0#edge{b=Color}
			   end,
		     array:set(Edge, Rec, Es)
	     end, Etab0, F, We),
    We#we{es=Etab}.

face_attr(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_attr(Edge, Etab, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{a=Info,lf=Face,ltsu=NextEdge} ->
	    face_attr(NextEdge, Etab, Face, LastEdge, [Info|Acc]);
	#edge{b=Info,rf=Face,rtsu=NextEdge} ->
	    face_attr(NextEdge, Etab, Face, LastEdge, [Info|Acc])
    end.
