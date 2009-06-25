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
	 info/2,any_attributes/1,
	 face_attr/3,face_attr/4,face_pos_attr/4,fold/5,set_face_attrs/3,
	 face_mixed_attrs/2,
	 all/2,edge_attrs/3,edge_attrs/4,set_edge_attrs/4,
	 set_both_edge_attrs/4,set_edge_uvs/2,set_edge_colors/2,del_edge_attrs/2,
	 set_edge_color/4,
	 vtx_attrs/2,vtx_attrs/3,attr/2,new_attr/2,average_attrs/2]).

-include("wings.hrl").

-import(lists, [any/2]).

-opaque all_attributes() :: {float(),float(),float()} | {float(),float()} | 'none'.
-type side() :: 'left'|'right'|face_num().

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

%% any_attributes(We) -> true|false.
%%  Find out whether We has any attributes at all.
%%
-spec any_attributes(#we{}) -> boolean().
any_attributes(_) ->
    true.

%% face_attr(What, Face, We) -> [Attribute]
%%  Return vertex attributes for the all vertices in the face.
%%
face_attr(What, Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    face_attr(What, Face, Edge, We).

%% face_attr(What, Face, Edge, We) -> [Attribute]
%%     What = uv | color | [vertex|uv] | [vertex|color]
%%     Attribute = {_,_,_} | {_,_} | none
%%  Return vertex attributes for the all vertices in the face.
%%
face_attr(uv, Face, Edge, #we{es=Etab}) ->
    face_attr(Edge, Etab, Face, Edge, []);
face_attr(color, Face, Edge, #we{es=Etab}) ->
    face_attr(Edge, Etab, Face, Edge, []);
face_attr([vertex|uv], Face, Edge, #we{es=Etab}) ->
    face_vtx_attr(Edge, Etab, Face, Edge, []);
face_attr([vertex|color], Face, Edge, #we{es=Etab}) ->
    face_vtx_attr(Edge, Etab, Face, Edge, []).

%% Returns {[VsPos],[Info]}
face_pos_attr(uv, Face, Edge, #we{es=Etab,vp=Vtab}) ->
    face_pos_attr_1(Edge, Etab, Vtab, Face, Edge, [], []);
face_pos_attr(color, Face, Edge, #we{es=Etab,vp=Vtab}) ->
    face_pos_attr_1(Edge, Etab, Vtab, Face, Edge, [], []).

fold(W, F, Acc, Face, #we{es=Etab,fs=Ftab}) when W =:= uv; W =:= color ->
    Edge = gb_trees:get(Face, Ftab),
    fold_1(F, Acc, Face, Edge, Edge, Etab, not_done).

%% face_mixed_attrs(Face, We) -> Attrs
%%       Attr = opaque representation of attributes
%%  Returns the averaged attributes for all vertices in the face.
%%
face_mixed_attrs(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    face_mixed_attrs(Face, Edge, We).

face_mixed_attrs(Face, Edge, #we{es=Etab}) ->
    Attrs = face_attr(Edge, Etab, Face, Edge, []),
    wings_color:average(Attrs).

%% set_face_attrs(Face, Attrs, We0) -> We
%%       Attrs = opaque representation of attributes
%%  Assign the attributes Attrs to all vertices in
%%  face Face.
%%
-spec set_face_attrs(face_num(), all_attributes(), #we{}) -> #we{}.
set_face_attrs(Face, Attr, We) ->
    set_face_color_1(Face, Attr, We).

%% all(uv|color, We) -> OrderedSet.
%%  Return an ordered set containing all UV coordinates or
%%  vertex colors.
%%
all(uv, #we{}=We) -> all_1(2, We);
all(color, #we{}=We) -> all_1(3, We).

%% edge_attrs(Edge, Side, We) -> Attr
%%       Side = left | right | Face | {other,Face}
%%       Attr = opaque representation of attributes
%%  Return the attributes for the edge on the given side.
%%  If Side is 'left', the attributes for the 'va' vertex
%%  will be returned, and if Side is 'right', the attributes
%%  for the 've' vertex will be returned.
%%
-spec edge_attrs(edge_num(), side()|{other,face_num()}, #we{}) ->
    all_attributes().
edge_attrs(Edge, left, #we{es=Etab}) ->
    #edge{a=La} = array:get(Edge, Etab),
    La;
edge_attrs(Edge, right, #we{es=Etab}) ->
    #edge{b=Ra} = array:get(Edge, Etab),
    Ra;
edge_attrs(Edge, {other,Face}, #we{es=Etab}) ->
    case array:get(Edge, Etab) of
	#edge{a=La,rf=Face} -> La;
	#edge{b=Ra,lf=Face} -> Ra
    end;
edge_attrs(Edge, Face, #we{es=Etab}) ->
    case array:get(Edge, Etab) of
	#edge{a=La,lf=Face} -> La;
	#edge{b=Ra,rf=Face} -> Ra
    end.

%% edge_attrs(Edge, Side, Weight, We) -> Attr
%%       Side = left | right | Face
%%       Attr = opaque representation of attributes
%%  Return the mixed attributes for the vertices according
%%  to the following formula:
%%
%%     Attr = AttrForVe*Weight + AttrForVs*(1-Weight)
%%
-spec edge_attrs(edge_num(), side(), float(), #we{}) ->
    all_attributes().
edge_attrs(Edge, left, 0.0, #we{es=Etab}) ->
    #edge{a=La} = array:get(Edge, Etab),
    La;
edge_attrs(Edge, left, W, #we{es=Etab}) ->
    #edge{a=La,lf=Lf,ltpr=Ltpr} = array:get(Edge, Etab),
    case array:get(Ltpr, Etab) of
	#edge{lf=Lf,a=Lb} -> wings_color:mix(W, Lb, La);
	#edge{rf=Lf,b=Lb} -> wings_color:mix(W, Lb, La)
    end;
edge_attrs(Edge, right, 1.0, #we{es=Etab}) ->
    #edge{b=Ra} = array:get(Edge, Etab),
    Ra;
edge_attrs(Edge, right, W, #we{es=Etab}) ->
    #edge{b=Ra,rf=Rf,rtpr=Rtpr} = array:get(Edge, Etab),
    case array:get(Rtpr, Etab) of
	#edge{lf=Rf,a=Rb} -> wings_color:mix(W, Ra, Rb);
	#edge{rf=Rf,b=Rb} -> wings_color:mix(W, Ra, Rb)
    end;
edge_attrs(Edge, Face, W, #we{es=Etab}) ->
    case array:get(Edge, Etab) of
	#edge{a=La,lf=Face} when W =:= 0.0 ->
	    La;
	#edge{a=La,lf=Face,ltpr=Ltpr} ->
	    case array:get(Ltpr, Etab) of
		#edge{lf=Face,a=Lb} -> wings_color:mix(W, Lb, La);
		#edge{rf=Face,b=Lb} -> wings_color:mix(W, Lb, La)
	    end;
	#edge{b=Ra,rf=Face} when W =:= 1.0 ->
	    Ra;
	#edge{b=Ra,rf=Face,rtpr=Rtpr} ->
	    case array:get(Rtpr, Etab) of
		#edge{lf=Face,a=Rb} -> wings_color:mix(W, Ra, Rb);
		#edge{rf=Face,b=Rb} -> wings_color:mix(W, Ra, Rb)
	    end
    end.

%% set_edge_attrs(Edge, Side, Attr, We0) -> We
%%       Side = left | right | Face
%%       Attr = opaque representation of attributes
%%              (for instance obtained by calling edge_attrs/4)
%%  Set attributes for the 'vs' vertex for this edge if side
%%  is 'left' or for the 've' vertex for this edge if side
%%  is 'right'.
%%
-spec set_edge_attrs(edge_num(), side(), all_attributes(), #we{}) ->
    #we{}.
set_edge_attrs(Edge, left, Attr, #we{es=Etab}=We) ->
    Rec = array:get(Edge, Etab),
    We#we{es=array:set(Edge, Rec#edge{a=Attr}, Etab)};
set_edge_attrs(Edge, right, Attr, #we{es=Etab}=We) ->
    Rec = array:get(Edge, Etab),
    We#we{es=array:set(Edge, Rec#edge{b=Attr}, Etab)};
set_edge_attrs(Edge, Face, Attr, #we{es=Etab}=We) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face}=Rec ->
	    We#we{es=array:set(Edge, Rec#edge{a=Attr}, Etab)};
	#edge{rf=Face}=Rec ->
	    We#we{es=array:set(Edge, Rec#edge{b=Attr}, Etab)}
    end.

%% set_both_edge_attrs(Edge, LeftAttr, RightAttr, We0) -> We
%%       LeftAttr, RightAttr = opaque representation of attributes
%%              (for instance obtained by calling edge_attrs/4)
%%  Set the attributes for both side of this edge.
%%
-spec set_both_edge_attrs(edge_num(), all_attributes(), all_attributes(), #we{}) ->
    #we{}.
set_both_edge_attrs(Edge, LeftAttr, RightAttr, #we{es=Etab}=We) ->
    Rec = array:get(Edge, Etab),
    We#we{es=array:set(Edge, Rec#edge{a=LeftAttr,b=RightAttr}, Etab)}.

%% set_edge_uvs([{Edge,LeftUV,RightUV}, We0) -> We
%%  Assign UV coordinates to the edges in the list.
%%
-type uv_coords() :: {float(),float()}.
-spec set_edge_uvs([{edge_num(),uv_coords(),uv_coords()}], #we{}) -> #we{}.
set_edge_uvs(List, #we{es=Etab}=We) ->
    We#we{es=set_edge_uvs_1(List, Etab)}.

%% set_edge_colors([{Edge,LeftColor,RightColor}, We0) -> We
%%  Assign vertex colors to the edges in the list.
%%
-type vertex_color() :: {float(),float(),float()}.
-spec set_edge_colors([{edge_num(),vertex_color(),vertex_color()}], #we{}) -> #we{}.
set_edge_colors(List, #we{es=Etab}=We) ->
    We#we{es=set_edge_uvs_1(List, Etab)}.

%% set_edge_color(Edge, LeftColor, RightColor, We0) -> We
%%  Assign vertex colors to the edge Edge.
set_edge_color(Edge, LeftCol, RightCol, #we{es=Etab}=We) ->
    Rec0 = array:get(Edge, Etab),
    Rec = Rec0#edge{a=LeftCol,b=RightCol},
    We#we{es=array:set(Edge, Rec, Etab)}.

%% del_edge_attrs(Edge, We0) -> We.
%%  Delete all vertex attributes for the given edge.
%%  Should only be use when an edge has been removed.
%%
del_edge_attrs(_Edge, We) ->
    %% A dummy for now, since attributes are stored in
    %% the edge table and the edge has already been removed.
    We.

%% vtx_attrs(Vertex, We) -> Attr.
%%       Attr = opaque representation of attributes
%%  Return the averaged attributes for vertex Vertex.
%%
-spec vtx_attrs(vertex_num(), #we{}) -> all_attributes().
vtx_attrs(V, We) ->
    Fun = fun(_, _, E, Acc) ->
		  case E of
		      #edge{vs=V,a=Attr} -> [Attr|Acc];
		      #edge{ve=V,b=Attr} -> [Attr|Acc]
		  end
	  end,
    wings_color:average(wings_vertex:fold(Fun, [], V, We)).

%% vtx_attrs(Vertex, Face, We) -> Attr.
%%       Attr = opaque representation of attributes
%%  Return the attributes for vertex Vertex in face Face.
%%
-spec vtx_attrs(vertex_num(), face_num(), #we{}) ->
    all_attributes().
vtx_attrs(V, Face, We) ->
    Fun = fun(_, _, E, NotDone) ->
		  case E of
		      #edge{lf=Face,vs=V,a=Attr} -> Attr;
		      #edge{rf=Face,ve=V,b=Attr} -> Attr;
		      _ -> NotDone
		  end
	  end,
    wings_vertex:until(Fun, not_done, V, We).

%% attr(What, Attrs) -> [Attribute]
%%     What = uv | color | [vertex|uv] | [vertex|color]
%%     Attrs = opaque representation of attributes
%%   Given Attrs, an opaque collection of attributes,
%%   retrive the attribute given by What.
%%
-spec attr('uv' | 'color', all_attributes()) -> term().
attr(uv, Attrs) -> Attrs;
attr(color, Attrs) -> Attrs.

%% new_attr(Color, UV) -> Attrs
%%     Attrs = opaque representation of attributes
%%   Create an opaque collection of attributes given
%%   vertex color and UV coordinates.
%%
new_attr(Color, none) -> Color;
new_attr(_, UV) -> UV.

%% new_attr(AttrA, AttrB) -> Attr
%%     AttrA, AttrB, Attr = opaque representation of attributes
%%   Average the attributes AttrA and AttrB
%%
average_attrs(AttrA, AttrB) ->
    wings_color:mix(0.5, AttrA, AttrB).

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

face_vtx_attr(LastEdge, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_vtx_attr(Edge, Etab, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,a=Info,lf=Face,ltsu=NextEdge} ->
	    face_vtx_attr(NextEdge, Etab, Face, LastEdge, [[V|Info]|Acc]);
	#edge{ve=V,b=Info,rf=Face,rtsu=NextEdge} ->
	    face_vtx_attr(NextEdge, Etab, Face, LastEdge, [[V|Info]|Acc])
    end.

face_pos_attr_1(LastEdge, _, _, _, LastEdge, Vs, Info)
  when Vs =/= [] -> {Vs,Info};
face_pos_attr_1(Edge, Etab, Vtab, Face, LastEdge, Vs, Info) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,a=Col,lf=Face,ltsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    face_pos_attr_1(NextEdge, Etab, Vtab, Face, LastEdge,
			    [Pos|Vs], [Col|Info]);
	#edge{ve=V,b=Col,rtsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    face_pos_attr_1(NextEdge, Etab, Vtab, Face, LastEdge,
			    [Pos|Vs], [Col|Info])
    end.

fold_1(_F, Acc, _Face, LastEdge, LastEdge, _Etab, done) -> Acc;
fold_1(F, Acc0, Face, Edge, LastEdge, Etab, _) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,a=VInfo,lf=Face,ltsu=NextEdge} ->
		  F(V, VInfo, Acc0);
	      #edge{ve=V,b=VInfo,rf=Face,rtsu=NextEdge} ->
		  F(V, VInfo, Acc0)
	  end,
    fold_1(F, Acc, Face, NextEdge, LastEdge, Etab, done).

all_1(Sz, #we{es=Etab}) ->
    Cuvs0 = array:sparse_foldl(fun(_, #edge{a=A,b=B}, Acc) ->
				       [A,B|Acc]
			       end, [], Etab),
    Cuvs = [E || E <- Cuvs0, tuple_size(E) =:= Sz],
    ordsets:from_list(Cuvs).

set_edge_uvs_1([{Edge,LeftUV,RightUV}|T], Etab0) ->
    Rec = array:get(Edge, Etab0),
    Etab = array:set(Edge, Rec#edge{a=LeftUV,b=RightUV}, Etab0),
    set_edge_uvs_1(T, Etab);
set_edge_uvs_1([], Etab) -> Etab.
