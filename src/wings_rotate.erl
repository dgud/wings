%%
%%  wings_rotate.erl --
%%
%%     This module implements the Rotate command.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_rotate.erl,v 1.41 2005/12/18 14:04:31 dgud Exp $
%%

-module(wings_rotate).
-export([setup/2,rotate/5]).
-include("wings.hrl").

-import(lists, [foldl/3]).

setup({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun setup/2);
setup({Vec,Center,Magnet}, St) ->
    setup_1(Vec, Center, Magnet, St);
setup({Vec,Center}, St) ->
    setup_1(Vec, Center, none, St).

setup_1(Axis, center, Magnet, St)
  when Axis == last_axis; Axis == default_axis ->
    {Point,Vec} = wings_pref:get_value(Axis),
    setup_2(Vec, Point, Magnet, St);
setup_1(Axis, Point, Magnet, St)
  when Axis == last_axis; Axis == default_axis ->
    {_,Vec} = wings_pref:get_value(Axis),
    setup_2(Vec, Point, Magnet, St);
setup_1(Vec, Center, Magnet, St) ->
    setup_2(wings_util:make_vector(Vec), Center, Magnet, St).

setup_2(Vec, center, Magnet, #st{selmode=vertex}=St) ->
    VsPs = wings_sel:fold(
	     fun(Vs, #we{vp=Vtab}, Acc) ->
		     Pos = [gb_trees:get(V,Vtab)||V <- gb_sets:to_list(Vs)],
		     [Pos|Acc]
	     end, [], St),
    Center = e3d_vec:average(lists:append(VsPs)),
    setup_2(Vec, Center, Magnet, St);
setup_2(Vec, Center, Magnet, #st{selmode=vertex}=St) ->
    Tvs = wings_sel:fold(
	    fun(Vs, We, Acc) ->
		    rotate(Vec, Center, Magnet, gb_sets:to_list(Vs), We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, Magnet, St);
setup_2(Vec, Center, Magnet, #st{selmode=edge}=St) ->
    Tvs = wings_sel:fold(
	    fun(Edges, We, Acc) ->
		    edges_to_vertices(Vec, Center, Magnet, Edges, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, Magnet, St);
setup_2(Vec, Center, Magnet, #st{selmode=face}=St) ->
    Tvs = wings_sel:fold(
	    fun(Faces, We, Acc) ->
		    faces_to_vertices(Vec, Center, Magnet, Faces, We, Acc)
	    end, [], St),
    init_drag(Tvs, Vec, Magnet, St);
setup_2(Vec, Center, _Magnet, #st{selmode=body}=St) ->
    Tvs = wings_sel:fold(
	    fun(_, #we{id=Id}=We, Acc) ->
		    [{Id,body_rotate(Vec, Center, We)}|Acc]
	    end, [], St),
    init_drag({matrix,Tvs}, Vec, none, St).

init_drag(Tvs, Vec, Magnet, St) ->
    Flags = wings_magnet:flags(Magnet, flags(Vec)),
    wings_drag:setup(Tvs, [angle|magnet_unit(Magnet)], Flags, St).

flags(free) -> [screen_relative];
flags(_) -> [].

magnet_unit(none) -> [];
magnet_unit(_) -> [falloff].

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
	    wings_u:error(?__(1,
			      "Magnet Rotate on multiple edge regions "
			      "requires an explicit rotate origin."))
    end;
edges_to_vertices(Vec, Center, Magnet, Edges, We, Acc) ->
    edges_to_vertices_1(Vec, Center, Magnet, Edges, We, Acc).

edges_to_vertices_1(normal, Center, Magnet, Es, #we{es=Etab}=We, Acc) ->
    Ns = foldl(fun(Edge, A) ->
		       #edge{lf=Lf,rf=Rf} = gb_trees:get(Edge, Etab),
		       [wings_face:normal(Lf, We),
			wings_face:normal(Rf, We)|A]
	       end, [], gb_sets:to_list(Es)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    edges_to_vertices_1(Vec, Center, Magnet, Es, We, Acc);
edges_to_vertices_1(Vec, Center, Magnet, Es, We, Acc) ->
    Vs = wings_edge:to_vertices(Es, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc).
 
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
	    wings_u:error(?__(1,"Magnet Rotate on multiple face regions requires an explicit rotate origin."))
    end;
faces_to_vertices(Vec, Center, Magnet, Faces, We, Acc) ->
    faces_to_vertices_1(Vec, Center, Magnet, Faces, We, Acc).

faces_to_vertices_1(normal, Center, Magnet, Faces, We, Acc) ->
    Ns = foldl(fun(Face, N0) ->
		       [wings_face:normal(Face, We)|N0]
	       end, [], gb_sets:to_list(Faces)),
    Vec = e3d_vec:norm(e3d_vec:add(Ns)),
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc);
faces_to_vertices_1(Vec, Center, Magnet, Faces, We, Acc) ->
    Vs = wings_face:to_vertices(Faces, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc).

%%
%% Conversion of body selections (entire objects) to vertices.
%%

body_rotate(Vec, center, We) ->
    body_rotate(Vec, wings_vertex:center(We), Vec);
body_rotate(free, {Cx,Cy,Cz}, _We) ->
    fun(Matrix0, [Angle]) when is_float(Angle) ->
	    Vec = view_vector(),
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end;
body_rotate(Vec, {Cx,Cy,Cz}, _We) ->
    fun(Matrix0, [Angle]) when is_float(Angle) ->
	    M0 = e3d_mat:mul(Matrix0, e3d_mat:translate(Cx, Cy, Cz)),
	    M = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Vec)),
	    e3d_mat:mul(M, e3d_mat:translate(-Cx, -Cy, -Cz))
    end.

%%%
%%% Setup rotation.
%%%

rotate(Vec, Center, Vs, We, Acc) ->
    rotate(Vec, Center, none, Vs, We, Acc).

rotate(Vec, center, Magnet, Vs, We, Acc) ->
    Center = wings_vertex:center(Vs, We),
    rotate(Vec, Center, Magnet, Vs, We, Acc);
rotate(free, Center, Magnet, Vs, We, Acc) ->
    rotate(view_vector(), Center, Magnet, Vs, We, Acc);
rotate(Vec, Center, Magnet, Vs, #we{id=Id}=We, Acc) ->
    VsPos = wings_util:add_vpos(Vs, We),
    Fun = rotate_fun(Center, VsPos, Vec),
    magnet(Vec, Center, Magnet, Vs, We, {Id,{Vs,Fun}}, Acc).

rotate_fun(Center, VsPos, Axis) ->
    fun(view_changed, NewWe) ->
	    NewAxis = view_vector(),
	    NewVsPos = wings_util:update_vpos(VsPos, NewWe),
	    rotate_fun(Center, NewVsPos, NewAxis);
       ([Dx], A) -> do_rotate(Center, Axis, Dx, VsPos, A)
    end.

do_rotate({Cx,Cy,Cz}, Axis, Angle, VsPos, Acc0) ->
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,Pos0}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Pos}|Acc]
	  end, Acc0, VsPos).

view_vector() ->
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul_point(M, {0.0,0.0,1.0}).

magnet(_Vec, _Center, none, _Vs, _We, Tv, Acc) -> [Tv|Acc];
magnet(Vec, Center, Magnet0, Vs, #we{id=Id}=We, _, Acc) ->
    {VsInf0,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs, We),
    VsInf = pre_transform(Center, VsInf0),
    [{Id,{Affected,magnet_rotate_fun(Vec, Center, VsInf, Magnet)}}|Acc].

pre_transform(Center, VsInf) ->
    wings_magnet:transform(fun(Pos) ->
				   e3d_vec:sub(Pos, Center)
			   end, VsInf).

magnet_rotate_fun(Axis0, Center, VsInf0, {_,R}=Magnet0) ->
    fun(new_falloff, Falloff) ->
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet0),
	    magnet_rotate_fun(Axis0, Center, VsInf, Magnet0);
       (new_mode_data, {Type,Falloff}) ->
	    Magnet = {Type,R},
	    VsInf = wings_magnet:recalc(Falloff, VsInf0, Magnet),
	    magnet_rotate_fun(Axis0, Center, VsInf, Magnet);
       (view_changed, NewWe) ->
	    Axis = view_vector(),
	    VsInf1 = wings_util:update_vpos(VsInf0, NewWe),
	    VsInf = pre_transform(Center, VsInf1),
	    magnet_rotate_fun(Axis, Center, VsInf, Magnet0);
       ([Dx|_], A) ->
	    magnet_rotate(Axis0, Center, Dx, VsInf0, A)
    end.
    
magnet_rotate(Axis, {Cx,Cy,Cz}, Angle, VsPos, Acc0) ->
    foldl(fun({V,Pos0,_,Inf}, Acc) ->
		  {X,Y,Z} = e3d_mat:mul_point(e3d_mat:rotate(Inf*Angle, Axis), Pos0),
		  Pos = wings_util:share(X+Cx, Y+Cy, Z+Cz),
		  [{V,Pos}|Acc]
	  end, Acc0, VsPos).
