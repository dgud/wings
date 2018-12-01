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
	 set_edge_color/4,
	 vtx_attrs/2,vtx_attrs/3,attr/2,new_attr/2,average_attrs/1,average_attrs/2,
	 set_vtx_face_uvs/4,
	 remove/2,remove/3,renumber/2,merge/2,gc/1,any_update/2]).

-include("wings.hrl").

-import(lists, [any/2,member/2,sort/1,reverse/1]).
-export_type([all_attributes/0]).

-opaque all_attributes() :: {float(),float(),float()} | {float(),float()} | 'none'.
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

%% any_uvs(We) -> true|false.
%%  Find out whether We has any vertex colors.
%%
-spec any_uvs(#we{}) -> boolean().
any_uvs(#we{lv=none,rv=none}) -> false;
any_uvs(#we{lv=Lva,rv=Rva}) ->
    any_uvs_1(Lva) orelse any_uvs_1(Rva).


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
	#edge{lf=Face} when W =:= 0.0 ->
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
-type uv_coords() :: {float(),float()}.
-spec set_edge_uvs([{edge_num(),uv_coords(),uv_coords()}], #we{}) -> #we{}.
set_edge_uvs(List, #we{lv=Lva0,rv=Rva0}=We) ->
    {Lva,Rva} = set_edge_uvs_1(List, Lva0, Rva0),
    We#we{lv=Lva,rv=Rva}.

%% set_edge_colors([{Edge,LeftColor,RightColor}, We0) -> We
%%  Assign vertex colors to the edges in the list.
%%
-type vertex_color() :: {float(),float(),float()}.
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
%%   retrive the attribute given by What.
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
