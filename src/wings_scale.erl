%%
%%  wings_scale.erl --
%%
%%     This module implements the Scale command plus
%%     the interactive part of the Bevel (face).
%%
%%     -- Inset removed at revision 388 and replaced by wpc_contour.erl --
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_scale).
-export([setup/2]).

-include("wings.hrl").
-import(lists, [foldl/3,map/2]).

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
    Center = vertices_center(Point, St),
    F = fun(Vs0, We) ->
                Vs = gb_sets:to_list(Vs0),
                scale(Vec, Center, Magnet, Vs, We)
        end,
    init_drag(F, Magnet, St);
setup_2(Vec, Point, Magnet, #st{selmode=edge}=St) ->
    F = fun(Edges, We) ->
                edges_to_vertices(Vec, Point, Magnet, Edges, We)
        end,
    init_drag(F, Magnet, St);
setup_2(Vec, Point, Magnet, #st{selmode=face}=St) ->
    F = fun(Faces, We) ->
                faces_to_vertices(Vec, Point, Magnet, Faces, We)
        end,
    init_drag(F, Magnet, St);
setup_2(Vec, Point, _Magnet, #st{selmode=body}=St) ->
    Flags = [{initial,[1.0]},rescale_normals],
    wings_drag:matrix(fun(We) ->
                              body_to_vertices(Vec, Point, We)
                      end, [percent], Flags, St).

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

init_drag(Tvs, Magnet, Flags0, St) ->
    Unit = [percent|magnet_unit(Magnet)],
    Flags = wings_magnet:flags(Magnet, [{initial,[1.0]}|Flags0]),
    wings_drag:fold(Tvs, Unit, Flags, St).

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

%%
%% Scaling of vertices.
%%

vertices_center(center, St) ->
    wings_sel:center_vs(St);
vertices_center(Center, _) -> Center.

%%
%% Conversion of edge selections to vertices.
%%

edges_to_vertices(Vec, center, none, Edges0, We) ->
    Ts = [edges_to_vertices_1(Vec, center, none, Edges, We) ||
             Edges <- wings_sel:edge_regions(Edges0, We)],
    wings_drag:compose(Ts);
edges_to_vertices(Vec, center, Magnet, Edges0, We) ->
    case wings_sel:edge_regions(Edges0, We) of
	[Edges] ->
	    edges_to_vertices_1(Vec, center, Magnet, Edges, We);
	_Other ->
	    wings_u:error_msg(?__(1,"Magnet scale on multiple edge regions requires an explicit scale origin."))
    end;
edges_to_vertices(Vec, Point, Magnet, Edges, We) ->
    edges_to_vertices_1(Vec, Point, Magnet, Edges, We).

edges_to_vertices_1(Vec, Point, Magnet, Edges, We) ->
    Vs = wings_edge:to_vertices(Edges, We),
    scale(Vec, Point, Magnet, Vs, We).

%%
%% Conversion of face selections to vertices.
%%

faces_to_vertices(Vec, center, none, Faces0, We) ->
    Ts = [faces_to_vertices_1(Vec, center, none, Faces, We) ||
             Faces <- wings_sel:strict_face_regions(Faces0, We)],
    wings_drag:compose(Ts);
faces_to_vertices(Vec, center, Magnet, Faces0, We) ->
    case wings_sel:strict_face_regions(Faces0, We) of
	[Faces] ->
	    faces_to_vertices_1(Vec, center, Magnet, Faces, We);
	_Other ->
	    wings_u:error_msg(?__(1,"Magnet scale on multiple face regions requires an explicit scale origin."))
    end;
faces_to_vertices(Vec, Point, Magnet, Faces, We) ->
    faces_to_vertices_1(Vec, Point, Magnet, Faces, We).

faces_to_vertices_1(Vec, Point, Magnet, Faces, We) ->
    Vs = wings_face:to_vertices(Faces, We),
    scale(Vec, Point, Magnet, Vs, We).

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

scale(Vec, center, Magnet, Vs, We) ->
    Center = wings_vertex:center(Vs, We),
    scale(Vec, Center, Magnet, Vs, We);
scale(Vec0, Center, Magnet, Vs, We) ->
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
    magnet(Vec, Magnet, Pre, Post, Vs, We, {Vs,Fun}).

magnet(_Vec, none, _Pre, _Post, _Vs, _We, Tv) ->
    Tv;
magnet(Vec, Magnet0, Pre, Post, Vs0, We, _Tv) ->
    {VsInf0,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs0, We),
    VsInf = pre_transform(Post, VsInf0),
    {Affected,magnet_scale_fun(Vec, Pre, VsInf, Magnet)}.

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
