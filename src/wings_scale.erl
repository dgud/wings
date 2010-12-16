%%
%%  wings_scale.erl --
%%
%%     This module implements the Scale command plus
%%     the interactive part of the Bevel (face).
%%
%%     -- Inset removed at revision 388 and replaced by wpc_contour.erl --
%%
%%  Copyright (c) 2001-2010 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_scale).
-export([setup/2]).

-include("wings.hrl").
-import(lists, [foldl/3]).
-define(HUGE, 1.0E307).

setup({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun setup/2);
setup({X,Y,Z}=Point, St) when is_float(X), is_float(Y), is_float(Z) ->
    setup(uniform, Point, none, St);
setup({Vec,Point,Magnet}, St) ->
    setup(Vec, Point, Magnet, St);
setup({Point,Magnet}, St) when element(1, Magnet) == magnet ->
    setup(uniform, Point, Magnet, St);
setup({Vec,Point}, St) ->
    setup(Vec, Point, none, St).

setup(Axis, Point, Magnet, St) ->
    case special_axis(Axis) of
	no ->
	    setup_2(Axis, Point, Magnet, St);
	AxisVal ->
	    setup_1(AxisVal, Point, Magnet, St)
    end.

setup_1({radial,{Point,Dir}}, center, Magnet, St) ->
    setup_2({radial,Dir}, Point, Magnet, St);
setup_1({radial,{_,Dir}}, Point, Magnet, St) ->
    setup_2({radial,Dir}, Point, Magnet, St);
setup_1({Point,Dir}, center, Magnet, St) ->
    setup_2(Dir, Point, Magnet, St);
setup_1({_,Dir}, Point, Magnet, St) ->
    setup_2(Dir, Point, Magnet, St).

setup_2(Vec, Point, Magnet, #st{selmode=vertex}=St) ->
    Tvs = scale_vertices(Vec, Point, Magnet, St),
    init_drag(Tvs, Magnet, St);
setup_2(Vec, Point, Magnet, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Point, Magnet, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, Magnet, St);
setup_2(Vec, Point, Magnet, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Point, Magnet, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, Magnet, St);
setup_2(Vec, Point, _Magnet, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_to_vertices(Vec, Point, We)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, none, [rescale_normals], St).

special_axis(last_axis=A) ->
    wings_pref:get_value(A);
special_axis(default_axis=A) ->
    wings_pref:get_value(A);
special_axis({radial,A}) ->
    case special_axis(A) of
	no -> no;
	Val -> {radial,Val}
    end;
special_axis(_) -> no.

init_drag(Tvs, Magnet, St) ->
    init_drag(Tvs, Magnet, [], St).

init_drag(Tvs, Magnet, Flags, St) ->
    wings_drag:setup(Tvs, [percent|magnet_unit(Magnet)],
		     wings_magnet:flags(Magnet, [{initial,[1.0]}|Flags]), St).

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

%%
%% Scaling of vertices.
%%

scale_vertices(Vec, center, Magnet, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 [{gb_sets:to_list(Vs),We}|Acc]
			 end, [], St),
    VsPs = foldl(fun({Vs,#we{vp=Vtab0}}, Acc) ->
			 Vtab1 = sofs:from_external(array:sparse_to_orddict(Vtab0),
						    [{vertex,pos}]),
			 Restr = sofs:set(Vs, [vertex]),
			 Vtab2 = sofs:restriction(Vtab1, Restr),
			 Vtab3 = sofs:range(Vtab2),
			 Vtab = sofs:to_external(Vtab3),
			 [Vtab|Acc]
		 end, [], Tvs),
    Center = e3d_vec:average(lists:append(VsPs)),
    foldl(fun({Vs,We}, Acc) ->
		  scale(Vec, Center, Magnet, Vs, We, Acc)
	  end, [], Tvs);
scale_vertices(Vec, Center, Magnet, St) ->
    wings_sel:fold(
      fun(Vs0, We, Acc) ->
	      Vs = gb_sets:to_list(Vs0),
	      scale(Vec, Center, Magnet, Vs, We, Acc)
      end, [], St).

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Vec, center, none, Edges0, We, Acc) ->
    foldl(fun(Edges, A) ->
		  edges_to_vertices_1(Vec, center, none, Edges, We, A)
	  end, Acc, wings_sel:edge_regions(Edges0, We));
edges_to_vertices(Vec, center, Magnet, Edges0, We, Acc) ->
    case wings_sel:edge_regions(Edges0, We) of
	[Edges] ->
	    edges_to_vertices_1(Vec, center, Magnet, Edges, We, Acc);
	_Other ->
	    wings_u:error_msg(?__(1,"Magnet scale on multiple edge regions requires an explicit scale origin."))
    end;
edges_to_vertices(Vec, Point, Magnet, Edges, We, Acc) ->
    edges_to_vertices_1(Vec, Point, Magnet, Edges, We, Acc).

edges_to_vertices_1(Vec, Point, Magnet, Edges, We, Acc) ->
    Vs = wings_edge:to_vertices(Edges, We),
    scale(Vec, Point, Magnet, Vs, We, Acc).

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, center, none, Faces0, We, Acc) ->
    foldl(fun(Faces, A) ->
		  faces_to_vertices_1(Vec, center, none, Faces, We, A)
	  end, Acc, wings_sel:strict_face_regions(Faces0, We));
faces_to_vertices(Vec, center, Magnet, Faces0, We, Acc) ->
    case wings_sel:strict_face_regions(Faces0, We) of
	[Faces] ->
	    faces_to_vertices_1(Vec, center, Magnet, Faces, We, Acc);
	_Other ->
	    wings_u:error_msg(?__(1,"Magnet scale on multiple face regions requires an explicit scale origin."))
    end;
faces_to_vertices(Vec, Point, Magnet, Faces, We, Acc) ->
    faces_to_vertices_1(Vec, Point, Magnet, Faces, We, Acc).

faces_to_vertices_1(Vec, Point, Magnet, Faces, We, Acc) ->
    Vs = wings_face:to_vertices(Faces, We),
    scale(Vec, Point, Magnet, Vs, We, Acc).

%%
%% Conversion of body selection to vertices.
%%

body_to_vertices(Vec, center, We) ->
    Center = wings_vertex:center(We),
    body_to_vertices_1(Vec, Center);
body_to_vertices(Vec, Point, _We) ->
    body_to_vertices_1(Vec, Point).

body_to_vertices_1(Vec0, Center) ->
    Vec = make_vector(Vec0),
    fun(_Matrix0, [Dx]) when is_float(Dx) ->
	    make_matrix(Dx, Vec, Center)
    end.

%%%
%%% Utilities.
%%%

scale(Vec, center, Magnet, Vs, We, A) ->
    Center = wings_vertex:center(Vs, We),
    scale(Vec, Center, Magnet, Vs, We, A);
scale(Vec0, Center, Magnet, Vs, #we{id=Id}=We, Acc) ->
    Vec = make_vector(Vec0),
    {Pre,Post} = make_matrices(Vec, Center),
    VsPos = mul(Vs, Post, We),
    Fun = fun([Dx], A0) ->
		  {Sx,Sy,Sz} = make_scale(Dx, {1.0,1.0,1.0}, Vec),
		  Matrix = e3d_mat:mul(Pre, e3d_mat:scale(Sx, Sy, Sz)),
		  foldl(fun({V,Pos0}, A) ->
				Pos = e3d_mat:mul_point(Matrix, Pos0),
				[{V,Pos}|A]
			end, A0, VsPos)
	  end,
    magnet(Vec, Magnet, Pre, Post, Vs, We, {Id,{Vs,Fun}}, Acc).

magnet(_Vec, none, _Pre, _Post, _Vs, _We, Tv, Acc) -> [Tv|Acc];
magnet(Vec, Magnet0, Pre, Post, Vs0, #we{id=Id}=We, _, Acc) ->
    {VsInf0,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs0, We),
    VsInf = pre_transform(Post, VsInf0),
    [{Id,{Affected,magnet_scale_fun(Vec, Pre, VsInf, Magnet)}}|Acc].

pre_transform(Matrix, VsInf) ->
    wings_magnet:transform(fun(Pos) ->
				   e3d_mat:mul_point(Matrix, Pos)
			   end, VsInf).
    
magnet_scale_fun(Vec, Pre, VsInf0, {_,R}=Magnet0) ->
    fun(new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_scale_fun(Vec, Pre, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_scale_fun(Vec, Pre, VsInf, Magnet);
       ([Dx|_], A0) ->
	    foldl(fun({V,Pos0,_,Inf}, A) ->
			  Pos1 = make_scale(1.0+(Dx-1.0)*Inf, Pos0, Vec),
			  Pos = e3d_mat:mul_point(Pre, Pos1),
			  [{V,Pos}|A]
		  end, A0, VsInf0)
    end.

make_vector({radial,{_,_,_}}=Vec) -> Vec;
make_vector({radial,x}) -> {radial,{1.0,0.0,0.0}};
make_vector({radial,y}) -> {radial,{0.0,1.0,0.0}};
make_vector({radial,z}) -> {radial,{0.0,0.0,1.0}};
make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(uniform) -> uniform.

make_matrix(Dx, Vec, Center) ->
    {Pre,Post} = make_matrices(Vec, Center),
    {Sx,Sy,Sz} = make_scale(Dx, {1.0,1.0,1.0}, Vec),
    e3d_mat:mul(e3d_mat:mul(Pre, e3d_mat:scale(Sx, Sy, Sz)), Post).

make_scale(Dx, {X,Y,Z}, uniform) when is_float(Dx) -> {Dx*X,Dx*Y,Dx*Z};
make_scale(Dx, {X,Y,Z}, {radial,_}) when is_float(Dx) -> {Dx*X,Dx*Y,Z};
make_scale(Dx, {X,Y,Z}, _) when is_float(Dx) -> {X,Y,Dx*Z}.

make_matrices(uniform, Center) ->
    Pre = e3d_mat:translate(Center),
    Post = e3d_mat:translate(e3d_vec:neg(Center)),
    {Pre,Post};
make_matrices({radial,Vec}, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Pre0 = e3d_mat:translate(Center),
    Pre = e3d_mat:mul(Pre0, Rot),
    Post = e3d_mat:mul(RotBack, e3d_mat:translate(e3d_vec:neg(Center))),
    {Pre,Post};
make_matrices(Vec, Center) ->
    RotBack = e3d_mat:rotate_to_z(Vec),
    Rot = e3d_mat:transpose(RotBack),
    Pre0 = e3d_mat:translate(Center),
    Pre = e3d_mat:mul(Pre0, Rot),
    Post = e3d_mat:mul(RotBack, e3d_mat:translate(e3d_vec:neg(Center))),
    {Pre,Post}.

mul(Vs, Matrix, #we{vp=Vtab}) ->
    foldl(fun(V, A) ->
		  Pos0 = array:get(V, Vtab),
		  Pos = e3d_mat:mul_point(Matrix, Pos0),
		  [{V,Pos}|A]
	  end, [], Vs).
