%%
%%  wings_va.erl --
%%
%%     This module implements vertex attributes.
%%
%%  Copyright (c) 2009-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_va).
-export([set_vertex_color/3,set_edge_color/3,set_face_color/3,set_body_color/2,
	 any_attributes/1,any_colors/1,any_uvs/1,
	 face_attr/3,face_attr/4,face_pos_attr/4,fold/5,set_face_attrs/3,
	 face_mixed_attrs/2,
	 all/2,edge_attrs/3,edge_attrs/4,set_edge_attrs/4,set_both_edge_attrs/4,
	 set_edge_attrs/2,set_edge_uvs/2,set_edge_colors/2,del_edge_attrs/2,
	 set_edge_color/4,blend_edge_color/4,
	 vtx_attrs/2,vtx_attrs/3,attr/2,new_attr/2,average_attrs/1,average_attrs/2,
	 set_vtx_face_uvs/4,
         remove/2,remove/3,renumber/2,merge/2,gc/1,any_update/2,
         uv_move_data/2,uv_move/3,corners/2,uv_wedge/3,uv_wedge_delta/2,uv_class/3,uv_class_color/1,uv_set/3,
         wall_uv/2,wall_uv_ring/2,wall_uv_bridge/3,wall_uv_strips/3,flat_wall_uv/2,
    seam_strips/2,seam_strip_uv/2,lift_wall_uv/3,restore_corners/4,
    follow_uvs/1]).

-include("wings.hrl").

-import(lists, [any/2,member/2,sort/1,reverse/1]).
-export_type([all_attributes/0]).

-type uv_coords() :: {float(),float()}.
-type vertex_color() :: {float(),float(),float()}.
-type all_attributes() :: 'none' |
                          {float(),float(),float()} | {float(),float()} |
                          nonempty_improper_list(vertex_color() | 'none',
                                                 uv_coords() | 'none').
-type side() :: 'left'|'right'|face_num().

-type edge_num() :: wings_edge:edge_num().
-type face_num() :: wings_face:face_num().
-type vertex_num() :: wings_vertex:vertex_num().

set_vertex_color(Vs, Color, We) ->
    gb_sets:fold(fun(V, W) ->
			 set_vertex_color_1(V, Color, W)
		 end, We, Vs).

set_edge_color(Es, Color, We) ->
    gb_sets:fold(fun(E, W) ->
			 set_edge_color_1(E, Color, W)
		 end, We, Es).

set_face_color(Fs, Color, We) ->
    gb_sets:fold(fun(F, W) ->
			 set_face_color_1(F, Color, W)
		 end, We, Fs).

set_body_color(Color, #we{es=Etab,lv=Lva0,rv=Rva0}=We) ->
    Update = fun(E, _, Tab) ->
		     set_color(E, Color, Tab)
	     end,
    Lva = array:sparse_foldl(Update, Lva0, Etab),
    Rva = array:sparse_foldl(Update, Rva0, Etab),
    We#we{lv=Lva,rv=Rva}.

%% any_attributes(We) -> true|false.
%%  Find out whether We has any attributes at all.
%%
-spec any_attributes(#we{}) -> boolean().
any_attributes(#we{lv=none,rv=none}) -> false;
any_attributes(_) -> true.

%% any_colors(We) -> true|false.
%%  Find out whether We has any vertex colors.
%%
-spec any_colors(#we{}) -> boolean().
any_colors(#we{lv=none,rv=none}) -> false;
any_colors(#we{lv=Lva,rv=Rva}) ->
    any_colors_1(Lva) orelse any_colors_1(Rva).

%% any_uvs(We | St) -> true|false.
%%  Find out whether We (or a selected object of St) has any UVs.
%%
-spec any_uvs(#we{} | #st{}) -> boolean().
any_uvs(#st{}=St) ->
    wings_sel:fold(fun(_, We, Any) -> Any orelse any_uvs(We) end, false, St);
any_uvs(#we{lv=none,rv=none}) -> false;
any_uvs(#we{lv=Lva,rv=Rva}) ->
    any_uvs_1(Lva) orelse any_uvs_1(Rva).

%% follow_uvs(We) -> true|false.
%%  Find out whether the UVs of We should follow the changes of the geometry,
%%  i.e. the preference update_uv is on and We has UVs.
%%
-spec follow_uvs(#we{}) -> boolean().
follow_uvs(We) ->
    wings_pref:get_value(update_uv) andalso any_uvs(We).

%% face_attr(What, Face, We) -> [Attribute]
%%  Return vertex attributes for all vertices in the face.
%%
face_attr(What, Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    face_attr(What, Face, Edge, We).

%% face_attr(What, Face, Edge, We) -> [Attribute]
%%     What = uv | color | [vertex|uv] | [vertex|color]
%%     Attribute = {_,_,_} | {_,_} | none
%%  Return vertex attributes for the all vertices in the face.
%%
face_attr(uv, Face, Edge, #we{es=Etab,lv=Lva,rv=Rva}) ->
    face_attr_uv(Edge, Etab, Lva, Rva, Face, Edge, []);
face_attr(color, Face, Edge, #we{es=Etab,lv=Lva,rv=Rva}) ->
    face_attr_color(Edge, Etab, Lva, Rva, Face, Edge, []);
face_attr([color|uv], Face, Edge, #we{es=Etab,lv=Lva,rv=Rva}) ->
    face_attr_color_uv(Edge, Etab, Lva, Rva, Face, Edge, []);
face_attr([vertex|uv], Face, Edge, #we{es=Etab,lv=Lva,rv=Rva}) ->
    face_vtx_attr_uv(Edge, Etab, Face, {Lva,Rva}, Edge, []);
face_attr([vertex|color], Face, Edge, #we{es=Etab,lv=Lva,rv=Rva}) ->
    face_vtx_attr_color(Edge, Etab, Face, {Lva,Rva}, Edge, []).

%% Returns {[VsPos],[Info]}
face_pos_attr(uv, Face, Edge, #we{es=Etab,vp=Vtab,lv=Lva,rv=Rva}) ->
    face_pos_attr_uv(Edge, Etab, {Vtab,Lva,Rva}, Face, Edge, [], []);
face_pos_attr(color, Face, Edge, #we{es=Etab,vp=Vtab,lv=Lva,rv=Rva}) ->
    face_pos_attr_color(Edge, Etab, {Vtab,Lva,Rva}, Face, Edge, [], []);
face_pos_attr([color|uv], Face, Edge, #we{es=Etab,vp=Vtab,lv=Lva,rv=Rva}) ->
    face_pos_attr_col_uv(Edge, Etab, {Vtab,Lva,Rva}, Face, Edge, [], []).

fold(uv, F, Acc, Face, #we{es=Etab,fs=Ftab,lv=Lva,rv=Rva}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold_uv(F, Acc, Face, Edge, Edge, {Etab,Lva,Rva}, not_done);
fold(color, F, Acc, Face, #we{es=Etab,fs=Ftab,lv=Lva,rv=Rva}) ->
    Edge = gb_trees:get(Face, Ftab),
    fold_color(F, Acc, Face, Edge, Edge, {Etab,Lva,Rva}, not_done).

%% face_mixed_attrs(Face, We) -> Attrs
%%       Attr = opaque representation of attributes
%%  Returns the averaged attributes for all vertices in the face.
%%
face_mixed_attrs(_, #we{lv=none,rv=none}) ->
    none;
face_mixed_attrs(Face, #we{fs=Ftab,es=Etab,lv=Lva,rv=Rva}) ->
    Edge = gb_trees:get(Face, Ftab),
    Attrs = face_attr(Edge, Etab, Lva, Rva, Face, Edge, []),
    average(Attrs).

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
edge_attrs(Edge, left, #we{lv=Lva}) ->
    aget(Edge, Lva);
edge_attrs(Edge, right, #we{rv=Rva}) ->
    aget(Edge, Rva);
edge_attrs(Edge, {other,Face}, #we{es=Etab,lv=Lva,rv=Rva}) ->
    case array:get(Edge, Etab) of
	#edge{rf=Face} ->
	    aget(Edge, Lva);
	#edge{lf=Face} ->
	    aget(Edge, Rva)
    end;
edge_attrs(Edge, Face, #we{es=Etab,lv=Lva,rv=Rva}) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face} ->
	    aget(Edge, Lva);
	#edge{rf=Face} ->
	    aget(Edge, Rva)
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
edge_attrs(_Edge, left, _W, #we{lv=none,rv=none}) -> none;
edge_attrs(Edge, left, W, #we{es=Etab,lv=Lva,rv=Rva}) ->
    #edge{lf=Lf,ltpr=Ltpr} = array:get(Edge, Etab),
    La = aget(Edge, Lva),
    case array:get(Ltpr, Etab) of
	#edge{lf=Lf} ->
	    Lb = aget(Ltpr, Lva),
	    mix(W, Lb, La);
	#edge{rf=Lf} ->
	    Lb = aget(Ltpr, Rva),
	    mix(W, Lb, La)
    end;
edge_attrs(_Edge, right, _W, #we{lv=none,rv=none}) -> none;
edge_attrs(Edge, right, W, #we{es=Etab,lv=Lva,rv=Rva}) ->
    #edge{rf=Rf,rtpr=Rtpr} = array:get(Edge, Etab),
    Ra = aget(Edge, Rva),
    case array:get(Rtpr, Etab) of
	#edge{lf=Rf} ->
	    Rb = aget(Rtpr, Lva),
	    mix(W, Ra, Rb);
	#edge{rf=Rf} ->
	    Rb = aget(Rtpr, Rva),
	    mix(W, Ra, Rb)
    end;
edge_attrs(_Edge, _, _W, #we{lv=none,rv=none}) -> none;
edge_attrs(Edge, Face, W, #we{es=Etab,lv=Lva,rv=Rva}) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face} when W =:= +0.0 ->
	    aget(Edge, Lva);
	#edge{lf=Face,ltpr=Ltpr} ->
	    La = aget(Edge, Lva),
	    case array:get(Ltpr, Etab) of
		#edge{lf=Face} ->
		    Lb = aget(Ltpr, Lva),
		    mix(W, Lb, La);
		#edge{rf=Face} ->
		    Lb = aget(Ltpr, Rva),
		    mix(W, Lb, La)
	    end;
	#edge{rf=Face} when W =:= 1.0 ->
	    aget(Edge, Rva);
	#edge{rf=Face,rtpr=Rtpr} ->
	    Ra = aget(Edge, Rva),
	    case array:get(Rtpr, Etab) of
		#edge{lf=Face} ->
		    Rb = aget(Rtpr, Lva),
		    mix(W, Ra, Rb);
		#edge{rf=Face} ->
		    Rb = aget(Rtpr, Rva),
		    mix(W, Ra, Rb)
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
set_edge_attrs(Edge, left, Attr, #we{lv=Lva}=We) ->
    We#we{lv=aset(Edge, Attr, Lva)};
set_edge_attrs(Edge, right, Attr, #we{rv=Rva}=We) ->
    We#we{rv=aset(Edge, Attr, Rva)};
set_edge_attrs(Edge, Face, Attr, #we{es=Etab,lv=Lva,rv=Rva}=We) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face} ->
	    We#we{lv=aset(Edge, Attr, Lva)};
	#edge{rf=Face} ->
	    We#we{rv=aset(Edge, Attr, Rva)}
    end.

%% set_both_edge_attrs(Edge, LeftAttr, RightAttr, We0) -> We
%%       LeftAttr, RightAttr = opaque representation of attributes
%%              (for instance obtained by calling edge_attrs/4)
%%  Set the attributes for both side of this edge.
%%
-spec set_both_edge_attrs(edge_num(), all_attributes(), all_attributes(), #we{}) ->
    #we{}.
set_both_edge_attrs(Edge, LeftAttr, RightAttr, #we{lv=Lva,rv=Rva}=We) ->
    We#we{lv=aset(Edge, LeftAttr, Lva),
	  rv=aset(Edge, RightAttr, Rva)}.

%% set_both_edge_attrs([{Edge,LeftUV,RightUV,LeftVC,RightVC}, We0) -> We
%%  Assign UV coordinates and vertex's color to the edges in the list.
%%
-spec set_edge_attrs([{edge_num(),all_attributes(),all_attributes(),
			    all_attributes(),all_attributes()}], #we{}) -> #we{}.
set_edge_attrs(List, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = set_edge_attrs_1(List, Lva0, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% set_edge_uvs([{Edge,LeftUV,RightUV}, We0) -> We
%%  Assign UV coordinates to the edges in the list.
%%
-spec set_edge_uvs([{edge_num(),uv_coords(),uv_coords()}], #we{}) -> #we{}.
set_edge_uvs(List, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = set_edge_uvs_1(List, Lva0, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% set_edge_colors([{Edge,LeftColor,RightColor}, We0) -> We
%%  Assign vertex colors to the edges in the list.
%%
-spec set_edge_colors([{edge_num(),vertex_color(),vertex_color()}], #we{}) -> #we{}.
set_edge_colors(List, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = set_edge_colors_1(List, Lva0, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% set_edge_color(Edge, LeftColor, RightColor, We0) -> We
%%  Assign vertex colors to the edge Edge.
set_edge_color(Edge, LeftCol, RightCol, #we{lv=Lva0,rv=Rva0}=We) ->
    Lva = set_color(Edge, LeftCol, Lva0),
    Rva = set_color(Edge, RightCol, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% blend_edge_color(Edge, LeftColor, RightColor, We0) -> We
%%  Blend the current vertex colors and the new color to the edge.
blend_edge_color(Edge, LeftCol, RightCol, #we{lv=Lva0,rv=Rva0}=We) ->
    Lva = blend_color(Edge, LeftCol, Lva0),
    Rva = blend_color(Edge, RightCol, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% del_edge_attrs(Edge, We0) -> We.
%%  Delete all vertex attributes for the given edge.
%%  Should only be use when an edge has been removed.
%%
del_edge_attrs(Edge, #we{lv=Lva,rv=Rva}=We) ->
    We#we{lv=areset(Edge, Lva),
	  rv=areset(Edge, Rva)}.

%% vtx_attrs(Vertex, We) -> Attr.
%%       Attr = opaque representation of attributes
%%  Return the averaged attributes for vertex Vertex.
%%
-spec vtx_attrs(vertex_num(), #we{}) -> all_attributes().
vtx_attrs(V, #we{lv=Lva,rv=Rva}=We) ->
    Fun = fun(E, _, Rec, Acc) ->
		  case Rec of
		      #edge{vs=V} ->
			  [aget(E, Lva)|Acc];
		      #edge{ve=V} ->
			  [aget(E, Rva)|Acc]
		  end
	  end,
    average(wings_vertex:fold(Fun, [], V, We)).

%% vtx_attrs(Vertex, Face, We) -> Attr.
%%       Attr = opaque representation of attributes
%%  Return the attributes for vertex Vertex in face Face.
%%
-spec vtx_attrs(vertex_num(), face_num(), #we{}) ->
    all_attributes().
vtx_attrs(V, Face, #we{lv=Lva,rv=Rva}=We) ->
    Fun = fun(E, _, Rec, NotDone) ->
		  case Rec of
		      #edge{lf=Face,vs=V} ->
			  aget(E, Lva);
		      #edge{rf=Face,ve=V} ->
			  aget(E, Rva);
		      _ -> NotDone
		  end
	  end,
    wings_vertex:until(Fun, none, V, We).

%% attr(What, Attrs) -> [Attribute]
%%     What = uv | color | [vertex|uv] | [vertex|color]
%%     Attrs = opaque representation of attributes
%%   Given Attrs, an opaque collection of attributes,
%%   retrieve the attribute given by What.
%%
-spec attr('uv' | 'color', all_attributes()) -> term().
attr(uv, [_|UV]) -> UV;
attr(color, [Color|_]) -> Color;
attr(uv, none) -> none;
attr(color, none) -> none.

%% new_attr(Color, UV) -> Attrs
%%     Attrs = opaque representation of attributes
%%   Create an opaque collection of attributes given
%%   vertex color and UV coordinates.
%%
new_attr(none, none) -> none;
new_attr(Color, UV) -> [Color|UV].

%% new_attr(AttrA, AttrB) -> Attr
%%     AttrA, AttrB, Attr = opaque representation of attributes
%%   Average the attributes AttrA and AttrB
%%
-spec average_attrs(all_attributes(), all_attributes()) -> all_attributes().
average_attrs(AttrA, AttrB) ->
    mix(0.5, AttrA, AttrB).
average_attrs(List) ->
    average(List).

%% set_vtx_face_uvs(Vertex, [Face], UV, We0) -> We
%%  Set the UV coordinates for vertex Vertex in each face
%%  contained in the list of faces.
%%
set_vtx_face_uvs(V, Fs, UV, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} =
	wings_vertex:fold(
	  fun(Edge, _, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, {Lv,Rv}=Acc) ->
		  case V of
		      Va ->
			  case member(Lf, Fs)  of
			      true ->
				  case aget(Edge, Lv) of
				      none ->
					  {aset(Edge, [none|UV], Lv),Rv};
				      [_|UV] ->
					  Acc;
				      [Cl|_] ->
					  {aset(Edge, [Cl|UV], Lv),Rv}
				  end;
			      false -> Acc
			  end;
		      Vb ->
			  case member(Rf, Fs) of
			      true ->
				  case aget(Edge, Rv) of
				      none ->
					  {Lv,aset(Edge, [none|UV], Rv)};
				      [_|UV] ->
					  Acc;
				      [Cr|_] ->
					  {Lv,aset(Edge, [Cr|UV], Rv)}
				  end;
			      false -> Acc
			  end;
		      _ -> Acc
		  end
	  end, {Lva0,Rva0}, V, We),
    We#we{lv=Lva,rv=Rva}.

%% remove(What, We0) -> We
%%       What = all | color | uv
%%  Remove the specified kind of attributes from the object.
%%
-spec remove('color'|'uv'|'all', #we{}) -> #we{}.
remove(all, We) ->
    We#we{lv=none,rv=none};
remove(color, We) ->
    remove_1(fun(_, [_|none]) -> none;
		(_, [_|UV]) -> [none|UV]
	     end, We);
remove(uv, We) ->
    remove_1(fun(_, [none|_]) -> none;
		(_, [Color|_]) -> [Color|none]
	     end, We).

%% remove(What, We0) -> We
%%       What = all
%%  Remove the specified kind of attributes from the object.
%%
-spec remove('color'|'uv'|'all', [face_num()], #we{}) -> #we{}.
remove(_, _, #we{lv=none,rv=none}=We) ->
    We;
remove(all, Faces, #we{lv=Lva0,rv=Rva0}=We) ->
    F = fun(Face, _, E, Rec, {Lv,Rv}) ->
		case Rec of
		    #edge{lf=Face} ->
			{areset(E, Lv),Rv};
		    #edge{rf=Face} ->
			{Lv,areset(E, Rv)}
		end
	end,
    {Lva,Rva} = wings_face:fold_faces(F, {Lva0,Rva0}, Faces, We),
    We#we{lv=Lva,rv=Rva}.

%% renumber(GbTreesEdgeMap, We0) -> We
%%  Renumbers vertex attributes using EdgeMap, a gb_tree
%%  containing a mapping from the old edge numbers to the
%%  new edge numbers.
%%
renumber(Emap, #we{lv=Lva0,rv=Rva0}=We) ->
    Update = fun(Edge0, Attr, Acc) ->
		     %% We may have vertex attributes left for
		     %% edges that have been deleted, so we must
		     %% be prepared to handle that.
		     case gb_trees:lookup(Edge0, Emap) of
			 none -> Acc;
			 {value,Edge} -> [{Edge,Attr}|Acc]
		     end
	     end,
    Lva = renumber_1(Update, Lva0),
    Rva = renumber_1(Update, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% merge([We], We0) -> We
%%  Merge the vertex attributes from all We records in the first
%%  argument, storing the result into We0.
merge(Wes, We) ->
    {Lva,Rva} = merge_1(Wes, [], []),
    We#we{lv=Lva,rv=Rva}.

%% gc(We0) -> We
%%  Remove attributes for edges that no longer exists.
%%
gc(#we{lv=Lva0,rv=Rva0,es=Etab}=We) ->
    Lva = gc_1(Lva0, Etab),
    Rva = gc_1(Rva0, Etab),
    We#we{lv=Lva,rv=Rva}.

%% any_update(We0, We1) -> true | false.
%%  Check whether there has been any change to vertex attributes.
%%  (Optimized for speed; may return false positives.)
%%
any_update(#we{lv=Lva,rv=Rva}, #we{lv=Lva,rv=Rva}) -> false;
any_update(_, _) -> true.

%%%
%%% Local functions.
%%%

set_vertex_color_1(V, Color, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = wings_vertex:fold(
		  fun(Edge, _Face, Rec0, {Lv,Rv}) ->
			  case Rec0 of
			      #edge{vs=V} ->
				  {set_color(Edge, Color, Lv),Rv};
			      #edge{ve=V} ->
				  {Lv,set_color(Edge, Color, Rv)}
			  end
		  end, {Lva0,Rva0}, V, We),
    We#we{lv=Lva,rv=Rva}.

set_edge_color_1(E, Color, #we{es=Etab,lv=Lva0,rv=Rva0}=We) ->
    Lva1 = set_color(E, Color, Lva0),
    Rva1 = set_color(E, Color, Rva0),
    #edge{vs=Va,ve=Vb,rtpr=Rp,ltpr=Lp} = array:get(E, Etab),
    {Lva2,Rva2} = set_edge_color_2(Rp, Va, Color, Etab, Lva1, Rva1),
    {Lva,Rva} = set_edge_color_2(Lp, Vb, Color, Etab, Lva2, Rva2),
    We#we{lv=Lva,rv=Rva}.

set_edge_color_2(E, V, Color, Etab, Lva, Rva) ->
    case array:get(E, Etab) of
	#edge{vs=V} ->
	    {set_color(E, Color, Lva),Rva};
	#edge{ve=V} ->
	    {Lva,set_color(E, Color, Rva)}
    end.

set_face_color_1(F, Color, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = wings_face:fold(
		  fun(_V, Edge, Rec, {Lv0,Rv0}) ->
			  case Rec of
			      #edge{lf=F} ->
				  Lv = set_color(Edge, Color, Lv0),
				  {Lv,Rv0};
			      #edge{rf=F} ->
				  Rv = set_color(Edge, Color, Rv0),
				  {Lv0,Rv}
			  end
		  end, {Lva0,Rva0}, F, We),
    We#we{lv=Lva,rv=Rva}.

any_uvs_1(none) -> false;
any_uvs_1(VaTab) ->
    try
	array:sparse_foldl(fun(_, [_|none], _) -> false;
			      (_, [_|_], _) -> throw(true)
			   end, [], VaTab)
    catch
	throw:Res ->
	    Res
    end.

any_colors_1(none) -> false;
any_colors_1(VaTab) ->
    try
	array:sparse_foldl(fun(_, [none|_], _) -> false;
			      (_, [_|_], _) -> throw(true)
			   end, [], VaTab)
    catch
	throw:Res ->
	    Res
    end.

face_attr(LastEdge, _, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_attr(Edge, Etab, Lva, Rva, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    Info = aget(Edge, Lva),
	    face_attr(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc]);
	#edge{rf=Face,rtsu=NextEdge} ->
	    Info = aget(Edge, Rva),
	    face_attr(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc])
    end.

face_attr_uv(LastEdge, _, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_attr_uv(Edge, Etab, Lva, Rva, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_attr_uv(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc]);
	#edge{rf=Face,rtsu=NextEdge} ->
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_attr_uv(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc])
    end.

face_attr_color_uv(LastEdge, _, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_attr_color_uv(Edge, Etab, Lva, Rva, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    Info = case aget(Edge, Lva) of
		       none -> [none|none];
		       [_|_]=ColUV -> ColUV
		   end,
	    face_attr_color_uv(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc]);
	#edge{rf=Face,rtsu=NextEdge} ->
	    Info = case aget(Edge, Rva) of
		       none -> [none|none];
		       [_|_]=ColUV -> ColUV
		   end,
	    face_attr_color_uv(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc])
    end.

face_attr_color(LastEdge, _, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_attr_color(Edge, Etab, Lva, Rva, Face, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge} ->
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [Color|_] -> Color
		   end,
	    face_attr_color(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc]);
	#edge{rf=Face,rtsu=NextEdge} ->
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [Color|_] -> Color
		   end,
	    face_attr_color(NextEdge, Etab, Lva, Rva, Face, LastEdge, [Info|Acc])
    end.

face_vtx_attr_uv(LastEdge, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_vtx_attr_uv(Edge, Etab, Face, {Lva,Rva}=Tabs, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_vtx_attr_uv(NextEdge, Etab, Face, Tabs, LastEdge, [[V|Info]|Acc]);
	#edge{ve=V,rf=Face,rtsu=NextEdge} ->
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_vtx_attr_uv(NextEdge, Etab, Face, Tabs, LastEdge, [[V|Info]|Acc])
    end.

face_vtx_attr_color(LastEdge, _, _, _, LastEdge, Acc) when Acc =/= [] -> Acc;
face_vtx_attr_color(Edge, Etab, Face, {Lva,Rva}=Tabs, LastEdge, Acc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [Color|_] -> Color
		   end,
	    face_vtx_attr_color(NextEdge, Etab, Face, Tabs,
				LastEdge, [[V|Info]|Acc]);
	#edge{ve=V,rf=Face,rtsu=NextEdge} ->
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [Color|_] -> Color
		   end,
	    face_vtx_attr_color(NextEdge, Etab, Face, Tabs,
				LastEdge, [[V|Info]|Acc])
    end.

face_pos_attr_uv(LastEdge, _, _, _, LastEdge, Vs, Info)
  when Vs =/= [] -> {Vs,Info};
face_pos_attr_uv(Edge, Etab, {Vtab,Lva,Rva}=Tabs, Face, LastEdge, Vs, InfoAcc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_pos_attr_uv(NextEdge, Etab, Tabs, Face, LastEdge,
			     [Pos|Vs], [Info|InfoAcc]);
	#edge{ve=V,rtsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [_|UV] -> UV
		   end,
	    face_pos_attr_uv(NextEdge, Etab, Tabs, Face, LastEdge,
			     [Pos|Vs], [Info|InfoAcc])
    end.

face_pos_attr_color(LastEdge, _, _, _, LastEdge, Vs, Info)
  when Vs =/= [] -> {Vs,Info};
face_pos_attr_color(Edge, Etab, {Vtab,Lva,Rva}=Tabs, Face, LastEdge, Vs, InfoAcc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Lva) of
		       none -> none;
		       [Col|_] -> Col
		   end,
	    face_pos_attr_color(NextEdge, Etab, Tabs, Face, LastEdge,
				[Pos|Vs], [Info|InfoAcc]);
	#edge{ve=V,rtsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Rva) of
		       none -> none;
		       [Col|_] -> Col
		   end,
	    face_pos_attr_color(NextEdge, Etab, Tabs, Face, LastEdge,
				[Pos|Vs], [Info|InfoAcc])
    end.

face_pos_attr_col_uv(LastEdge, _, _, _, LastEdge, Vs, Info)
  when Vs =/= [] -> {Vs,Info};
face_pos_attr_col_uv(Edge, Etab, {Vtab,Lva,Rva}=Tabs, Face,
		     LastEdge, Vs, InfoAcc) ->
    case array:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Lva) of
		       none -> [none|none];
		       [_|_]=Attr -> Attr
		   end,
	    face_pos_attr_col_uv(NextEdge, Etab, Tabs, Face, LastEdge,
				[Pos|Vs], [Info|InfoAcc]);
	#edge{ve=V,rtsu=NextEdge} ->
	    Pos = array:get(V, Vtab),
	    Info = case aget(Edge, Rva) of
		       none -> [none|none];
		       [_|_]=Attr -> Attr
		   end,
	    face_pos_attr_col_uv(NextEdge, Etab, Tabs, Face, LastEdge,
				 [Pos|Vs], [Info|InfoAcc])
    end.

fold_uv(_F, Acc, _Face, LastEdge, LastEdge, _Tabs, done) -> Acc;
fold_uv(F, Acc0, Face, Edge, LastEdge, {Etab,Lva,Rva}=Tabs, _) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,ltsu=NextEdge} ->
		  Info = case aget(Edge, Lva) of
			     none -> none;
			     [_|UV] -> UV
			 end,
		  F(V, Info, Acc0);
	      #edge{ve=V,rf=Face,rtsu=NextEdge} ->
		  Info = case aget(Edge, Rva) of
			     none -> none;
			     [_|UV] -> UV
			 end,
		  F(V, Info, Acc0)
	  end,
    fold_uv(F, Acc, Face, NextEdge, LastEdge, Tabs, done).

fold_color(_F, Acc, _Face, LastEdge, LastEdge, _Tabs, done) -> Acc;
fold_color(F, Acc0, Face, Edge, LastEdge, {Etab,Lva,Rva}=Tabs, _) ->
    Acc = case array:get(Edge, Etab) of
	      #edge{vs=V,lf=Face,ltsu=NextEdge} ->
		  Info = case aget(Edge, Lva) of
			     none -> none;
			     [Col|_] -> Col
			 end,
		  F(V, Info, Acc0);
	      #edge{ve=V,rf=Face,rtsu=NextEdge} ->
		  Info = case aget(Edge, Rva) of
			     none -> none;
			     [Col|_] -> Col
			 end,
		  F(V, Info, Acc0)
	  end,
    fold_color(F, Acc, Face, NextEdge, LastEdge, Tabs, done).

all_1(Sz, #we{lv=Lva,rv=Rva}) ->
    Get = fun(_, [C|UV], Acc) -> [C,UV|Acc] end,
    Cuvs0 = afoldl(Get, [], Lva),
    Cuvs1 = afoldl(Get, Cuvs0, Rva),
    Cuvs = [E || E <- Cuvs1, tuple_size(E) =:= Sz],
    ordsets:from_list(Cuvs).

set_edge_uvs_1([{Edge,LeftUV,RightUV}|T], Lva0, Rva0) ->
    Lva = set_uv(Edge, LeftUV, Lva0),
    Rva = set_uv(Edge, RightUV, Rva0),
    set_edge_uvs_1(T, Lva, Rva);
set_edge_uvs_1([], Lva, Rva) -> {Lva,Rva}.

set_edge_colors_1([{Edge,LeftUV,RightUV}|T], Lva0, Rva0) ->
    Lva = set_color(Edge, LeftUV, Lva0),
    Rva = set_color(Edge, RightUV, Rva0),
    set_edge_colors_1(T, Lva, Rva);
set_edge_colors_1([], Lva, Rva) -> {Lva,Rva}.

set_edge_attrs_1([{Edge,LeftUV,RightUV,LeftVC,RightVC}|T], Lva0, Rva0) ->
    Lva1 = set_uv(Edge, LeftUV, Lva0),
    Lva = set_color(Edge, LeftVC, Lva1),
    Rva1 = set_uv(Edge, RightUV, Rva0),
    Rva = set_color(Edge, RightVC, Rva1),
    set_edge_attrs_1(T, Lva, Rva);
set_edge_attrs_1([], Lva, Rva) -> {Lva,Rva}.

set_color(Edge, Color, Tab) ->
    case aget(Edge, Tab) of
	none -> aset(Edge, [Color|none], Tab);
	[Color|_] -> Tab;
	[_|UV] -> aset(Edge, [Color|UV], Tab)
    end.

set_uv(Edge, UV, Tab) ->
    Attr = case aget(Edge, Tab) of
	       none -> [none|UV];
	       [Color|_] -> [Color|UV]
	   end,
    aset(Edge, Attr, Tab).

blend_color(Edge, Color, Tab) ->
    case aget(Edge, Tab) of
        none -> aset(Edge, [Color|none], Tab);
        [Color|_] -> Tab;
        [OldColor|UV] -> aset(Edge, [wings_color:blend(OldColor,Color)|UV], Tab)
    end.

mix(_, none, _) -> none;
mix(_, [_|_], none) -> none;
mix(W, [Col1|UV1], [Col2|UV2]) ->
    [wings_color:mix(W, Col1, Col2)|wings_color:mix(W, UV1, UV2)].

average(L) ->
    {A0,B0} = average_1(L, [], []),
    case {wings_color:average(A0),wings_color:average(B0)} of
	{none,none} -> none;
	{A,B} -> [A|B]
    end.

average_1([none|T], _, _) ->
    average_1(T, [none], [none]);
average_1([[Col|UV]|T], A, B) ->
    average_1(T, [Col|A], [UV|B]);
average_1([], A, B) -> {A,B}.

remove_1(F, #we{lv=Lva0,rv=Rva0,es=Etab}=We) when is_function(F, 2) ->
    Lva1 = amap(F, Lva0),
    Lva = gc_1(Lva1, Etab),
    Rva1 = amap(F, Rva0),
    Rva = gc_1(Rva1, Etab),
    We#we{lv=Lva,rv=Rva}.

renumber_1(_, none) -> none;
renumber_1(Update, VaTab0) ->
    VaTab = array:sparse_foldl(Update, [], VaTab0),
    array:from_orddict(sort(VaTab), none).

merge_1([#we{lv=Lva,rv=Rva}|Wes], LvaAcc0, RvaAcc0) ->
    LvaAcc = merge_2(Lva, LvaAcc0),
    RvaAcc = merge_2(Rva, RvaAcc0),
    merge_1(Wes, LvaAcc, RvaAcc);
merge_1([], LvaAcc, RvaAcc) ->
    {merge_3(LvaAcc),merge_3(RvaAcc)}.

merge_2(none, Acc) -> Acc;
merge_2(VaTab, Acc) -> [array:sparse_to_orddict(VaTab)|Acc].

merge_3(Lists) ->
    case lists:merge(Lists) of
	[] -> none;
	VaTab -> array:from_orddict(VaTab, none)
    end.

gc_1(none, _Etab) ->
    none;
gc_1(VaTab0, Etab) ->
    gc_2(array:sparse_to_orddict(VaTab0), Etab, []).

gc_2([{E,_}=H|T], Etab, Acc) ->
    case array:get(E, Etab) of
	undefined -> gc_2(T, Etab, Acc);
	_ -> gc_2(T, Etab, [H|Acc])
    end;
gc_2([], _, []) ->
    none;
gc_2([], _, Acc) ->
    array:from_orddict(reverse(Acc), none).

aset(_, none, none) -> none;
aset(K, V, none) -> array:set(K, V, array:new({default,none}));
aset(K, V, A) -> array:set(K, V, A).

areset(_, none) -> none;
areset(K, A) -> array:reset(K, A).

aget(_, none) -> none;
aget(K, A) -> array:get(K, A).

afoldl(_, Acc, none) -> Acc;
afoldl(Fun, Acc, VaTab) -> array:sparse_foldl(Fun, Acc, VaTab).

amap(_Fun, none) -> none;
amap(Fun, VaTab) -> array:sparse_map(Fun, VaTab).

%%%
%%% Keeping the UV coordinates when vertices move.
%%%
%%% When a vertex V moves by DP, each of its corners gets the UV that the
%%% original mapping of a face at V gives to the new position:
%%% dUV = A*DU1 + B*DU2, where DP = A*E1 + B*E2 (least squares) and E1, E2
%%% are the vectors to the two neighbours in the face. Corners with the
%%% same UV (no seam) move together.
%%%
%% uv_move_data(Vs, We) -> MoveData
%%  MoveData = [{V,[Class]}]
%%  Class = [Corner]
%%  Corner = {Edge,Side,Color,UV,Wedge}
%%  Wedge = none | {E1,E2,Ka,Kb,DU1,DU2}
-spec uv_move_data([vertex_num()], #we{}) -> list().
uv_move_data(Vs, We) ->
    [{V,uv_move_classes(V, We)} || V <- Vs].

%% uv_move(MoveData, DPFun, We) -> We
%%  DPFun(V) -> {Dx,Dy,Dz}
%%  Absolute: depends only on MoveData and DPFun (not on the current UVs
%%  of We), so it can be called repeatedly while dragging.
-spec uv_move(list(), fun((vertex_num()) -> {float(),float(),float()}),
              #we{}) -> #we{}.
uv_move(MoveData, DPFun, We) ->
    lists:foldl(fun({V,Classes}, W) ->
                        DP = DPFun(V),
                        lists:foldl(fun(Class, W0) ->
                                            uv_move_class(Class, DP, W0)
                                    end, W, Classes)
                end, We, MoveData).

uv_move_classes(V, #we{es=Etab,vp=Vtab}=We) ->
    Pos = array:get(V, Vtab),
    %% The corner of V in a face is stored on the edge of that face that
    %% starts at V (left face) or ends at V (right face). Note that the face
    %% given by wings_vertex:fold/4 is not necessarily that face.
    Fun = fun(Edge, _, Rec, Acc) ->
                  {Side,OtherEdge,Face} =
                      case Rec of
                          #edge{vs=V,ltsu=LS,lf=LF} -> {left,LS,LF};
                          #edge{ve=V,rtsu=RS,rf=RF} -> {right,RS,RF}
                      end,
                  case edge_attrs(Edge, Side, We) of
                      [Col|{_,_}=UV] ->
                          Na = wings_vertex:other(V, Rec),
                          Nb = wings_vertex:other(V, array:get(OtherEdge, Etab)),
                          Wedge = uv_move_wedge(Pos, UV, Na, Nb, Face, We),
                          [{Edge,Side,Col,UV,Wedge}|Acc];
                      _ ->
                          Acc
                  end
          end,
    uv_move_group(wings_vertex:fold(Fun, [], V, We), []).

%% At a straight corner (a vertex that cuts an edge in two) the neighbours
%% are collinear with V, so the other vertices of Face are used instead:
%% the mapping of the face is the same.
uv_move_wedge(Pos, UV0, Na, Nb, Face, #we{vp=Vtab}=We) ->
    case uv_move_wedge_1(Pos, UV0, Na, Nb, Face, We) of
        none ->
            Others = [C || C <- wings_face:vertices_ccw(Face, We),
                           C =/= Na, C =/= Nb, array:get(C, Vtab) =/= Pos],
            WedgeFun = fun(C) -> uv_move_wedge_1(Pos, UV0, Na, C, Face, We) end,
            Wedges = [W || W <- lists:map(WedgeFun, Others), W =/= none],
            case Wedges of
                [] -> none;
                _ -> element(2, lists:max([{uv_move_det(W),W} || W <- Wedges]))
            end;
        Wedge ->
            Wedge
    end.

uv_move_det({E1,E2,_,_,_,_}) ->
    e3d_vec:len(e3d_vec:cross(E1, E2)).

uv_move_wedge_1(Pos, {U0,V0}, Na, Nb, Face, #we{vp=Vtab}=We) ->
    case {attr(uv, vtx_attrs(Na, Face, We)),
          attr(uv, vtx_attrs(Nb, Face, We))} of
        {{Ua,Va},{Ub,Vb}} ->
            E1 = e3d_vec:sub(array:get(Na, Vtab), Pos),
            E2 = e3d_vec:sub(array:get(Nb, Vtab), Pos),
            G11 = e3d_vec:dot(E1, E1), G22 = e3d_vec:dot(E2, E2),
            G12 = e3d_vec:dot(E1, E2),
            Det = G11*G22 - G12*G12,
            if
                Det =< 1.0e-9*G11*G22 ->        %Degenerate (collinear) corner.
                    none;
                true ->
                    Ka = e3d_vec:mul(e3d_vec:sub(e3d_vec:mul(E1, G22),
                                                 e3d_vec:mul(E2, G12)), 1/Det),
                    Kb = e3d_vec:mul(e3d_vec:sub(e3d_vec:mul(E2, G11),
                                                 e3d_vec:mul(E1, G12)), 1/Det),
                    {E1,E2,Ka,Kb,{Ua-U0,Va-V0},{Ub-U0,Vb-V0}}
            end;
        _ ->
            none
    end.

%% Group the corners that have the same UV.
uv_move_group([{_,_,_,UV,_}=Corner|Cs], Classes) ->
    {Same,Other} = lists:partition(fun([{_,_,_,UV0,_}|_]) ->
                                           uv_move_same(UV, UV0)
                                   end, Classes),
    uv_move_group(Cs, [[Corner|lists:append(Same)]|Other]);
uv_move_group([], Classes) -> Classes.

uv_move_same({U1,V1}, {U2,V2}) ->
    abs(U1-U2) < 1.0e-6 andalso abs(V1-V2) < 1.0e-6.

uv_move_class(Class, DP, We) ->
    {DU,DV} = uv_move_delta(Class, DP),
    lists:foldl(fun({Edge,Side,Col,{U,V},_}, W) ->
                        set_edge_attrs(Edge, Side, [Col|{U+DU,V+DV}], W)
                end, We, Class).

%% Use the corner whose face fits DP best: smallest distance from DP to
%% the plane of the face, preferring faces that contain DP in their wedge.
uv_move_delta(Class, DP) ->
    Limit = 1.0e-3*e3d_vec:len(DP),
    Cands = [uv_move_cand(Wedge, DP, Limit) ||
                {_,_,_,_,Wedge} <- Class, Wedge =/= none],
    case Cands of
        [] -> {0.0,0.0};
        _ -> element(2, lists:min(Cands))
    end.

uv_move_cand({E1,E2,Ka,Kb,{U1,V1},{U2,V2}}, DP, Limit) ->
    A = e3d_vec:dot(DP, Ka),
    B = e3d_vec:dot(DP, Kb),
    Res = e3d_vec:len(e3d_vec:sub(DP, e3d_vec:add(e3d_vec:mul(E1, A),
                                                  e3d_vec:mul(E2, B)))),
    Score = if
                A >= -1.0e-6, B >= -1.0e-6 -> Res;
                true -> Res + Limit
            end,
    {Score,{A*U1+B*U2,A*V1+B*V2}}.

%% corners(V, We) -> [{Edge,Side,Face,Attr}]
%%  All the corners of vertex V, also those without attributes. A corner is
%%  stored on the edge of its face that starts at V (Side = left, the face
%%  is the left face of Edge) or ends at V (Side = right, the face is the
%%  right face of Edge). Attr is what edge_attrs/3 returns for Edge and Side.
-spec corners(vertex_num(), #we{}) ->
          [{edge_num(),'left'|'right',face_num(),all_attributes()|list()}].
corners(V, We) ->
    Fun = fun(Edge, _, Rec, Acc) ->
                  {Side,Face} =
                      case Rec of
                          #edge{vs=V,lf=LF} -> {left,LF};
                          #edge{ve=V,rf=RF} -> {right,RF}
                      end,
                  [{Edge,Side,Face,edge_attrs(Edge, Side, We)}|Acc]
          end,
    wings_vertex:fold(Fun, [], V, We).

%% uv_wedge(V, Face, We) -> Wedge | none
%%  The wedge (see uv_move_data/2) of the corner of vertex V in Face.
%%  It is none if the corner has no UV coordinates or is degenerate.
-spec uv_wedge(vertex_num(), face_num(), #we{}) -> tuple() | 'none'.
uv_wedge(V, Face, #we{es=Etab,vp=Vtab}=We) ->
    Pos = array:get(V, Vtab),
    Fun = fun(Edge, _, Rec, Acc) ->
                  {Side,OtherEdge,F} =
                      case Rec of
                          #edge{vs=V,ltsu=LS,lf=LF} -> {left,LS,LF};
                          #edge{ve=V,rtsu=RS,rf=RF} -> {right,RS,RF}
                      end,
                  case F =:= Face of
                      false ->
                          Acc;
                      true ->
                          case edge_attrs(Edge, Side, We) of
                              [_|{_,_}=UV] ->
                                  Na = wings_vertex:other(V, Rec),
                                  Nb = wings_vertex:other(V, array:get(OtherEdge, Etab)),
                                  uv_move_wedge(Pos, UV, Na, Nb, Face, We);
                              _ ->
                                  Acc
                          end
                  end
          end,
    wings_vertex:fold(Fun, none, V, We).

%% uv_wedge_delta(Wedge, DP) -> {DU,DV}
%%  The change of UV for a displacement DP of the vertex of the wedge.
-spec uv_wedge_delta(tuple() | 'none', {float(),float(),float()}) ->
          {float(),float()}.
uv_wedge_delta(none, _) ->
    {0.0,0.0};
uv_wedge_delta({_,_,Ka,Kb,{U1,V1},{U2,V2}}, DP) ->
    A = e3d_vec:dot(DP, Ka),
    B = e3d_vec:dot(DP, Kb),
    {A*U1+B*U2,A*V1+B*V2}.

%% uv_class(V, Face, We) -> {UV,[{Edge,Side,Color}]} | none
%%  The corner of vertex V in Face and the other corners of V that have
%%  the same UV coordinates, i.e. the corners that follow the corner of
%%  Face when the texture is moved. It is none if the corner of Face
%%  has no UV coordinates.
-spec uv_class(vertex_num(), face_num(), #we{}) ->
                      {{float(),float()},[{edge_num(),side(),term()}]} | 'none'.
uv_class(V, Face, We) ->
    Cs = corners(V, We),
    case lists:keyfind(Face, 3, Cs) of
        {_,_,_,[_|{U,W}]} ->
            SameUV = fun({_,_,_,[_|{U1,W1}]}) ->
                             abs(U1-U) < 1.0e-6 andalso abs(W1-W) < 1.0e-6;
                        (_) ->
                             false
                     end,
            Same = lists:filter(SameUV, Cs),
            {{U,W},[{Edge,Side,uv_class_color(Attr)} ||
                       {Edge,Side,_,Attr} <- Same]};
        _ ->
            none
    end.

uv_class_color([Col|_]) -> Col;
uv_class_color(_) -> none.

%% uv_set(Corners, UV, We) -> We
%%  Sets the UV coordinates of corners given as in uv_class/3.
-spec uv_set([{edge_num(),side(),term()}], {float(),float()}, #we{}) -> #we{}.
uv_set(Corners, UV, We) ->
    lists:foldl(fun({Edge,Side,Col}, W) ->
                        set_edge_attrs(Edge, Side, [Col|UV], W)
                end, We, Corners).

%%%
%%% Walls: side faces of an extrusion.
%%%

%% AutoUV makes a new chart where the UV of the two sides of an edge differ
%% (exact comparison). The walls are shifted by this amount so that they are
%% charts of their own and not part of the chart of the face they stand on.
-define(WALL_SEPARATION, 1.0e-5).

%% A wall whose normal is this close (cosine, about 30 degrees) to the one of
%% the face it stands on continues that face: it keeps the UV of the shared
%% edge (no separation), so it stays in the same chart.
-define(WALL_SIMILAR, 0.866).

%% wall_uv(Sel, We) -> We
%%  The side faces of the extruded faces Sel that are not in the plane of
%%  the extruded face (the walls) are laid flat (unfolded about their base
%%  edge) into the plane of the face they stand on and get its mapping, so
%%  the texture keeps its scale on them. They form a seam with the top face.
wall_uv(Sel, We) ->
    case any_uvs(We) of
        false ->
            We;
        true ->
            WFs = wall_faces(Sel, We),
            Walls = [W || {W,_} <- WFs],
            lists:foldl(fun({W,F}, Acc) -> wall_uv_1(W, F, Sel, Walls, Acc) end,
                        We, WFs)
    end.

%% wall_faces(Sel, We) -> [{Wall,Face}]
%%  The faces next to the faces of Sel (and not in Sel), each with the face
%%  of Sel that it touches.
wall_faces(Sel, We) ->
    NotSel = fun(W) -> not gb_sets:is_member(W, Sel) end,
    WallsOf = fun(F) ->
                      [{W,F} || W <- lists:filter(NotSel, wall_neighbors(F, We))]
              end,
    lists:usort(lists:append([WallsOf(F) || F <- gb_sets:to_list(Sel)])).

%% Two walls that share their base edge (the extruded faces were next to
%% each other, or the face they stand on has no UV) have no mapping to
%% unfold into: the one of the top face is used instead.
wall_uv_1(W, F, Sel, Walls, We) ->
    Vs = wings_face:vertices_ccw(W, We),
    TopVs = wings_face:to_vertices(gb_sets:to_list(Sel), We),
    Bases = [V || V <- Vs, not lists:member(V, TopVs)],
    case {Vs,Bases} of
        {[_,_,_,_],[Va,Vb]} ->
            case wings_vertex:edge_through(Va, Vb, W, We) of
                none ->
                    We;
                Edge ->
                    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
                    B = case Lf of W -> Rf; _ -> Lf end,
                    wall_uv_on(W, F, B, Sel, Walls, Va, Vb, Vs -- [Va,Vb], We)
            end;
        {[_,_,_],[_]} ->
            wall_uv_tri(W, F, Sel, Walls, We);
        _ ->
            We
    end.

wall_uv_on(W, F, B, Sel, Walls, Va, Vb, Tops, We) ->
    NoBase = lists:member(B, Walls) orelse
        gb_sets:is_member(B, Sel) orelse
        wall_uv_map(B, We) =:= none,
    case NoBase of
        true -> wall_uv_2(W, F, F, true, Va, Vb, Tops, We);
        false -> wall_uv_2(W, F, B, false, Va, Vb, Tops, We)
    end.

%% A triangular wall (Lift: the face was lifted around a hinge) stands on the
%% edge that it shares with a face that is neither lifted nor a wall. If that
%% face has no UV, the wall continues the lifted face from the edge they share.
wall_uv_tri(W, F, Sel, Walls, We) ->
    Free = fun(Face) ->
                   not (gb_sets:is_member(Face, Sel) orelse
                        lists:member(Face, Walls))
           end,
    Neighbor = fun(_, _, #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, A) ->
                       [{case Lf of W -> Rf; _ -> Lf end,Va,Vb}|A]
               end,
    Edges = wings_face:fold(Neighbor, [], W, We),
    Bases = [E || {B,_,_}=E <- Edges, Free(B), wall_uv_map(B, We) =/= none],
    Shared = [E || {B,_,_}=E <- Edges, B =:= F],
    Found = Bases ++ Shared,
    case Found of
        [{B,Va,Vb}|_] ->
            Tops = wings_face:vertices_ccw(W, We) -- [Va,Vb],
            wall_uv_2(W, F, B, false, Va, Vb, Tops, We);
        [] ->
            We
    end.

%% restore_corners(Face, Vs, From, We) -> We
%%  The corners of Face at the vertices Vs get the attributes that they have
%%  in From (collapsing the edges of the hinge of Lift mixes them up).
-spec restore_corners(face_num(), [vertex_num()], #we{}, #we{}) -> #we{}.
restore_corners(Face, Vs, From, We) ->
    case follow_uvs(We) of
        false ->
            We;
        true ->
            Restore = fun(V, Acc) ->
                              {_,_,_,Attr} = lists:keyfind(Face, 3, corners(V, From)),
                              {Edge,Side,_,_} = lists:keyfind(Face, 3, corners(V, Acc)),
                              set_edge_attrs(Edge, Side, Attr, Acc)
                      end,
            lists:foldl(Restore, We, Vs)
    end.

%% lift_wall_uv(Walls, Sel, We) -> We
%%  As wall_uv/2 and flat_wall_uv/2 for the faces Walls only, that can be
%%  triangles. The faces next to Sel at the hinge are not walls.
-spec lift_wall_uv([face_num()], gb_sets:set(face_num()), #we{}) -> #we{}.
lift_wall_uv(Walls, Sel, We) ->
    case any_uvs(We) of
        false ->
            We;
        true ->
            IsWall = fun({W,_}) -> lists:member(W, Walls) end,
            WFs = lists:filter(IsWall, wall_faces(Sel, We)),
            Unfold = fun({W,F}, Acc) -> wall_uv_1(W, F, Sel, Walls, Acc) end,
            Flat = fun({W,F}, Acc) -> flat_wall_uv_1(W, F, Acc) end,
            lists:foldl(Flat, lists:foldl(Unfold, We, WFs), WFs)
    end.

wall_uv_2(W, F, B, Swap, Va, Vb, Tops, We) ->
    Dot = e3d_vec:dot(wings_face:normal(W, We), wings_face:normal(B, We)),
    Similar = (not Swap) andalso Dot > ?WALL_SIMILAR,
    Shift = case Similar of
                true -> 0.0;
                false -> ?WALL_SEPARATION
            end,
    wall_uv_3(W, F, B, Swap, Va, Vb, Tops, Shift, We).

%% The UV of V in Face when it has one (exactly, as AutoUV compares them to
%% find the charts), otherwise the given one.
wall_exact_uv(V, Face, UV0, We) ->
    case attr(uv, vtx_attrs(V, Face, We)) of
        {_,_}=UV -> UV;
        _ -> UV0
    end.

wall_uv_3(W, F, B, Swap, Va, Vb, Tops, Shift, We) ->
    Coords = wall_uv_coords(W, F, B, Swap, Va, Vb, Tops, We),
    SetCorner = fun({V,UV}, Acc) -> wall_set_corner(V, W, UV, Shift, Acc) end,
    lists:foldl(SetCorner, We, Coords).

%% wall_uv_coords(W, F, B, Swap, Va, Vb, Tops, We) -> [{V,UV}]
wall_uv_coords(W, F, B, Swap, Va, Vb, Tops, We) ->
    Flat = e3d_vec:dot(wings_face:normal(W, We), wings_face:normal(F, We)),
    case wall_uv_map(B, We) of
        none ->
            [];
        _ when Flat > 0.999999 ->
            [];
        {Map,Center0} ->
            Pa = wings_vertex:pos(Va, We),
            Pb = wings_vertex:pos(Vb, We),
            Center = case Swap of
                         true -> e3d_vec:sub(e3d_vec:add(Pa, Pb), Center0);
                         false -> Center0
                     end,
            E = e3d_vec:sub(Pb, Pa),
            U = e3d_vec:norm(E),
            DC = e3d_vec:sub(Center, Pa),
            In0 = e3d_vec:norm(e3d_vec:sub(DC, e3d_vec:mul(U, e3d_vec:dot(DC, U)))),
            In = case Swap of
                     true ->
                         %% The centre of the top face is not in the plane
                         %% of its mapping when it was lifted.
                         N = wings_face:normal(B, We),
                         e3d_vec:norm(e3d_vec:sub(In0, e3d_vec:mul(N, e3d_vec:dot(In0, N))));
                     false ->
                         In0
                 end,
            Flatten = fun(V) ->
                              D = e3d_vec:sub(wings_vertex:pos(V, We), Pa),
                              S = e3d_vec:dot(D, U),
                              Perp = e3d_vec:sub(D, e3d_vec:mul(U, S)),
                              e3d_vec:sub(e3d_vec:add(Pa, e3d_vec:mul(U, S)),
                                          e3d_vec:mul(In, e3d_vec:len(Perp)))
                      end,
            Exact = fun(V) -> wall_exact_uv(V, B, Map(wings_vertex:pos(V, We)), We) end,
            [{Va,Exact(Va)},{Vb,Exact(Vb)}|[{V,Map(Flatten(V))} || V <- Tops]]
    end.

%% seam_strips(Faces, We) -> {Strips,Corners}
%%  Faces are the new faces made by Bevel. Strips: the quads between two
%%  faces whose UV coordinates do not join at the edge that was beveled, or
%%  that have UV on one side only. Corners: [{Face,Base}], the triangles and
%%  the faces that have only new faces around them (at the vertices), each
%%  with a new face next to it (Base, see seam_corner/3).
seam_strips(Faces, We) ->
    Strips = lists:filter(fun(F) -> seam_strip(F, We) end, Faces),
    IsCorner = fun(F) ->
                       length(wings_face:vertices_ccw(F, We)) =:= 3 orelse
                           lists:all(fun(N) -> lists:member(N, Faces) end,
                                     wall_neighbors(F, We))
               end,
    {Vertices,Others} = lists:partition(IsCorner, Faces),
    Base = fun(F) -> {F,seam_corner(F, Others, We)} end,
    Found = lists:map(Base, Vertices -- Strips),
    {Strips,[C || {_,B}=C <- Found, B =/= none]}.

seam_strip(Face, We) ->
    case strip_sides(Face, We) of
        {A,{A1,_},B,{B1,_}} when is_integer(A), is_integer(B) ->
            UvA = attr(uv, vtx_attrs(A1, A, We)),
            UvB = attr(uv, vtx_attrs(B1, B, We)),
            seam_strip_apart(UvA, UvB);
        _ ->
            false
    end.

seam_strip_apart({U1,V1}, {U2,V2}) -> abs(U1-U2) + abs(V1-V2) > 1.0e-3;
seam_strip_apart({_,_}, _) -> true;
seam_strip_apart(_, {_,_}) -> true;
seam_strip_apart(_, _) -> false.

%% seam_strip_uv({Strips,Corners}, We) -> We
%%  The strips are unfolded (about the edge shared with one of the faces)
%%  into the mapping of that face. The corners that are far from where they
%%  would be if unfolded from their strip are unfolded from it too.
seam_strip_uv({Strips,Corners}, We) ->
    UnfoldCorner = fun({F,Base}, W) -> seam_corner_uv(F, Base, W) end,
    We1 = lists:foldl(fun seam_strip_uv_1/2, We, Strips),
    lists:foldl(UnfoldCorner, We1, Corners).

seam_strip_uv_1(Face, We) ->
    case strip_sides(Face, We) of
        {A,{A1,A2},B,{B1,B2}} when is_integer(A), is_integer(B) ->
            case wall_uv_map(A, We) of
                none -> wall_uv_3(Face, B, B, false, B1, B2, [A1,A2], 0.0, We);
                _ -> wall_uv_3(Face, A, A, false, A1, A2, [B1,B2], 0.0, We)
            end;
        _ ->
            We
    end.

%% seam_corner(Face, Others, We) -> none | {Other,Va,Vb,Tops}
%%  Tops are the vertices of Face that are not on the edge (Va,Vb) it
%%  shares with the face Other.
seam_corner(Face, Others, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    Pairs = lists:zip(Vs, tl(Vs) ++ [hd(Vs)]),
    Found = [{O,Va,Vb,Vs -- [Va,Vb]} || {Va,Vb} <- Pairs,
                                        O <- [strip_neighbor(Va, Vb, Face, We)],
                                        lists:member(O, Others)],
    case Found of
        [Base|_] -> Base;
        [] -> none
    end.

seam_corner_uv(Face, {O,Va,Vb,Tops}, We) ->
    Coords = wall_uv_coords(Face, O, O, false, Va, Vb, Tops, We),
    Dist = fun({V,UV}) -> seam_uv_dist(UV, attr(uv, vtx_attrs(V, Face, We))) end,
    case Coords of
        [{_,UvA},{_,UvB}|_] ->
            Far = lists:max(lists:map(Dist, Coords)) > seam_uv_dist(UvA, UvB) / 2,
            seam_corner_set(Far, Face, {O,Va,Vb,Tops}, We);
        _ ->
            We
    end.

%% Unfolded only when the corners of the face are far from the unfolded ones.
seam_corner_set(true, Face, {O,Va,Vb,Tops}, We) ->
    wall_uv_3(Face, O, O, false, Va, Vb, Tops, 0.0, We);
seam_corner_set(false, _, _, We) ->
    We.

seam_uv_dist({U1,V1}, {U2,V2}) -> abs(U1-U2) + abs(V1-V2);
seam_uv_dist(_, _) -> 0.0.

%% strip_sides(Face, We) -> none | {FaceA,{A1,A2},FaceB,{B1,B2}}
%%  The long sides of a quad, the faces on the other side of them, and
%%  the vertices so that A1-B1 and A2-B2 are the short sides.
strip_sides(Face, We) ->
    case wings_face:vertices_ccw(Face, We) of
        [V0,V1,V2,V3] ->
            Len = fun(A, B) ->
                          e3d_vec:dist(wings_vertex:pos(A, We),
                                       wings_vertex:pos(B, We))
                  end,
            Along = Len(V0, V1) + Len(V2, V3) >= Len(V1, V2) + Len(V3, V0),
            {{A1,A2},{B1,B2}} = case Along of
                                    true -> {{V0,V1},{V3,V2}};
                                    false -> {{V1,V2},{V0,V3}}
                                end,
            {strip_neighbor(A1, A2, Face, We),{A1,A2},
             strip_neighbor(B1, B2, Face, We),{B1,B2}};
        _ ->
            none
    end.

strip_neighbor(Va, Vb, Face, We) ->
    case wings_vertex:edge_through(Va, Vb, Face, We) of
        none ->
            none;
        Edge ->
            #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
            case Lf of
                Face -> Rf;
                _ -> Lf
            end
    end.

%% flat_wall_uv(Sel, We) -> We
%%  The side faces of Sel that are in the plane of the face they touch (the
%%  extruded face was moved sideways) get the mapping of that face.
flat_wall_uv(Sel, We) ->
    case any_uvs(We) of
        false ->
            We;
        true ->
            WFs = wall_faces(Sel, We),
            lists:foldl(fun({W,F}, Acc) -> flat_wall_uv_1(W, F, Acc) end, We, WFs)
    end.

flat_wall_uv_1(W, F, We) ->
    flat_wall_uv_2(W, F, 0.0, We).

flat_wall_uv_2(W, F, Shift, We) ->
    Dot = e3d_vec:dot(wings_face:normal(W, We), wings_face:normal(F, We)),
    case wall_uv_map(F, We) of
        {Map,_} when abs(Dot) > 0.999999 ->
            lists:foldl(fun(V, Acc) ->
                                UV = Map(wings_vertex:pos(V, Acc)),
                                Exact = wall_exact_uv(V, F, UV, Acc),
                                wall_set_corner(V, W, Exact, Shift, Acc)
                        end, We, wings_face:vertices_ccw(W, We));
        _ ->
            We
    end.

%% wall_uv_ring(Sel, We) -> We
%%  The walls that Intrude creates between the hole of the outer shell and
%%  the faces Sel (the inner shell that is moved) form rings of quads. Each
%%  ring is opened in an edge and unfolded as a strip: the first wall about
%%  its base edge into the face it stands on (as wall_uv/2) and each of the
%%  next walls about the edge that it shares with the previous one. The
%%  strip is a chart of its own (see ?WALL_SEPARATION).
-spec wall_uv_ring(gb_sets:set(face_num()), #we{}) -> #we{}.
wall_uv_ring(Sel, We) ->
    case any_uvs(We) of
        false ->
            We;
        true ->
            Walls = [W || {W,_} <- wall_faces(Sel, We)],
            TopVs = wings_face:to_vertices(gb_sets:to_list(Sel), We),
            wall_rings(gb_sets:from_list(Walls), TopVs, We)
    end.

%% wall_uv_bridge(Walls, Vs, We) -> We
%%  The walls that Bridge creates between two faces. They form a ring that is
%%  unfolded as in wall_uv_ring/2, from the face around the first of the two
%%  holes. Vs are the vertices of the other face: the walls stay a chart of
%%  their own.
-spec wall_uv_bridge([face_num()], [vertex_num()], #we{}) -> #we{}.
wall_uv_bridge(Walls, Vs, We) ->
    case any_uvs(We) of
        false -> We;
        true -> wall_rings(gb_sets:from_list(Walls), Vs, We)
    end.

wall_neighbors(Face, We) ->
    wings_face:fold(fun(_, _, #edge{lf=Lf,rf=Rf}, A) ->
                            [case Lf of Face -> Rf; _ -> Lf end|A]
                    end, [], Face, We).

wall_rings(Todo0, TopVs, We0) ->
    case gb_sets:is_empty(Todo0) of
        true ->
            We0;
        false ->
            {W0,Todo1} = gb_sets:take_smallest(Todo0),
            We1 = wall_ring_first(W0, TopVs, We0),
            {Todo,We} = wall_ring_chain([W0], Todo1, We1),
            wall_rings(Todo, TopVs, We)
    end.

wall_ring_first(W, TopVs, We) ->
    Vs = wings_face:vertices_ccw(W, We),
    Bases = [V || V <- Vs, not lists:member(V, TopVs)],
    case Bases of
        [Va,Vb] when length(Vs) =:= 4 ->
            case wings_vertex:edge_through(Va, Vb, W, We) of
                none ->
                    We;
                Edge ->
                    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
                    B = case Lf of W -> Rf; _ -> Lf end,
                    wall_unfold(W, B, Va, Vb, Vs -- [Va,Vb], ?WALL_SEPARATION, We)
            end;
        _ ->
            We
    end.

wall_ring_chain([], Todo, We) ->
    {Todo,We};
wall_ring_chain([P|Queue], Todo0, We0) ->
    Neighbors = lists:usort(wall_neighbors(P, We0)),
    Next = [W || W <- Neighbors, gb_sets:is_member(W, Todo0)],
    Todo = gb_sets:subtract(Todo0, gb_sets:from_list(Next)),
    We = lists:foldl(fun(W, A) -> wall_ring_next(W, P, A) end, We0, Next),
    wall_ring_chain(Queue ++ Next, Todo, We).

wall_ring_next(W, Prev, We) ->
    Vs = wings_face:vertices_ccw(W, We),
    PrevVs = wings_face:vertices_ccw(Prev, We),
    Shared = [V || V <- Vs, lists:member(V, PrevVs)],
    case Shared of
        [Va,Vb] when length(Vs) =:= 4 ->
            We1 = wall_unfold(W, Prev, Va, Vb, Vs -- [Va,Vb], 0.0, We),
            %% The same UV, exactly, on both sides of the shared edge.
            lists:foldl(fun(V, A) ->
                                UV = attr(uv, vtx_attrs(V, Prev, A)),
                                wall_set_corner(V, W, UV, 0.0, A)
                        end, We1, [Va,Vb]);
        _ ->
            We
    end.

%% Unfolds W about the edge Va-Vb into the plane of B (flat if W is already
%% in that plane) using the mapping of B.
wall_unfold(W, B, Va, Vb, Tops, Shift, We0) ->
    We = wall_uv_3(W, B, B, false, Va, Vb, Tops, Shift, We0),
    Dot = e3d_vec:dot(wings_face:normal(W, We), wings_face:normal(B, We)),
    case Dot > 0.999999 of
        true -> flat_wall_uv_2(W, B, Shift, We);
        false -> We
    end.

%% wall_uv_strips(Tops, Bots, We) -> We
%%  As wall_uv/2 for walls known by their vertices: the quads with two
%%  vertices in the set Tops (the moved ones) and two in the list Bots
%%  (the base edge); also triangles with one vertex in Tops. Used by Bump,
%%  Crease and Extrude.
wall_uv_strips(Tops, Bots, We) ->
    VertexFaces = fun(V) ->
                          wings_vertex:fold(fun(_, F, _, A) -> [F|A] end, [], V, We)
                  end,
    Faces = lists:usort(lists:append([VertexFaces(V) || V <- gb_sets:to_list(Tops)])),
    lists:foldl(fun(W, Acc) -> wall_strip(W, Tops, Bots, Acc) end, We, Faces).

wall_strip(W, Tops, Bots, We) ->
    Vs = wings_face:vertices_ccw(W, We),
    Ts = [V || V <- Vs, gb_sets:is_member(V, Tops)],
    Bases = [V || V <- Vs, lists:member(V, Bots)],
    case {Vs,Ts,Bases} of
        {[_,_,_|_],[_|_],[Va,Vb]} when length(Vs) =:= length(Ts)+2 ->
            case wings_vertex:edge_through(Va, Vb, W, We) of
                none ->
                    We;
                Edge ->
                    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
                    B = case Lf of W -> Rf; _ -> Lf end,
                    We1 = wall_uv_2(W, B, B, false, Va, Vb, Ts, We),
                    Dot = e3d_vec:dot(wings_face:normal(W, We1),
                                      wings_face:normal(B, We1)),
                    case Dot > 0.999999 of
                        true -> flat_wall_uv_1(W, B, We1);
                        false -> We1
                    end
            end;
        _ ->
            We
    end.

wall_set_corner(V, W, {U,V0}, Shift, We) ->
    {Edge,Side,_,Attr} = lists:keyfind(W, 3, corners(V, We)),
    Col = case Attr of
              [C|_] -> C;
              _ -> none
          end,
    set_edge_attrs(Edge, Side, [Col|{U+Shift,V0}], We).

%% wall_uv_map(Face, We) -> {Map,Center} | none
%%  Map(Pos) -> {U,V}: the mapping of Face, taken from three of its corners.
wall_uv_map(Face, We) ->
    Pts = wall_uv_points(Face, We),
    case Pts of
        [{P0,UV0},{P1,UV1}|Rest] when Rest =/= [] ->
            E1 = e3d_vec:sub(P1, P0),
            CrossLen = fun({P,UV}) ->
                               Cross = e3d_vec:cross(E1, e3d_vec:sub(P, P0)),
                               {e3d_vec:len(Cross),P,UV}
                       end,
            {Area,P2,UV2} = lists:max(lists:map(CrossLen, Rest)),
            case Area > 1.0e-9 of
                false ->
                    none;
                true ->
                    E2 = e3d_vec:sub(P2, P0),
                    A = e3d_vec:dot(E1, E1),
                    Bb = e3d_vec:dot(E1, E2),
                    C = e3d_vec:dot(E2, E2),
                    Det = A*C - Bb*Bb,
                    {U0,V0} = UV0,
                    {U1,V1} = UV1,
                    {U2,V2} = UV2,
                    Map = fun(Q) ->
                                  D = e3d_vec:sub(Q, P0),
                                  D1 = e3d_vec:dot(D, E1),
                                  D2 = e3d_vec:dot(D, E2),
                                  Sc = (C*D1 - Bb*D2)/Det,
                                  Tc = (A*D2 - Bb*D1)/Det,
                                  {U0 + Sc*(U1-U0) + Tc*(U2-U0),
                                   V0 + Sc*(V1-V0) + Tc*(V2-V0)}
                          end,
                    {Map,wings_face:center(Face, We)}
            end;
        _ ->
            none
    end.

%% The corners of Face that have UV: [{Pos,UV}].
wall_uv_points(Face, We) ->
    CornerUV = fun(V) -> attr(uv, vtx_attrs(V, Face, We)) end,
    WithUV = [{V,UV} || V <- wings_face:vertices_ccw(Face, We),
                        {_,_}=UV <- [CornerUV(V)]],
    [{wings_vertex:pos(V, We),UV} || {V,UV} <- WithUV].
