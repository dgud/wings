%%
%%  wpc_region.erl --
%%
%%     Plug-in with region and edge-loop commands.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_region).

-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").

-import(lists, [append/1,foldl/3,unzip/1]).
-define(HUGE, 1.0E307).

init() ->
    true.

menu({face,move}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(1,"Region"),region,
	       ?__(2,"Move region along the normal of its edge loop.")}];
menu({face,scale}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(3,"Region"),region,
	       ?__(4,"Scale region from the plane of its edge loop.")}];
menu({face,rotate}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(5,"Region"),region,
	       ?__(6,"Rotate region around the normal of its edge loop.")}];
menu({face,flatten}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(7,"Region"),region,
	       ?__(8,"Flatten region to the normal of its edge loop.")}];
menu(_, Menu) -> Menu.

command({face,{move,region}}, St) ->
    wings_drag:fold(
      fun(Faces, We) ->
              move_region(Faces, We)
      end, [distance], St);
command({face,{scale,region}}, St) ->
    wings_drag:fold(
      fun(Faces, We) ->
              scale_region(Faces, We)
      end, [{percent,{0.0,?HUGE}}], [{initial,[1.0]}], St);
command({face,{rotate,region}}, St) ->
    wings_drag:fold(
      fun(Faces, We) ->
              rotate_region(Faces, We)
      end, [angle], St);
command({face,{flatten,region}}, St) ->
    wpa:sel_map(
      fun(Faces, We) ->
	      flatten_region(Faces, We)
      end, St);
command(_, _) -> next.

%%%
%%% Move Region.
%%%

move_region(Faces, We) ->
    move_region(wpa:sel_strict_face_regions(Faces, We), We, []).

move_region([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices_ccw(Fs, We) of
	      error ->
		  region_error();
	      OuterVs when is_list(OuterVs) ->
		  move_region(OuterVs, Fs, We, Acc0)
	  end,
    move_region(Regs, We, Acc);
move_region([], We, Acc) ->
    wings_drag:translate_fun(Acc, We).

move_region(OuterVs, Faces, We, Acc) ->
    PlaneNormal = wings_face:face_normal_cw(OuterVs, We),
    [{PlaneNormal,wings_face:to_vertices(Faces, We)}|Acc].

%%%
%%% Scale Region.
%%%

scale_region(Faces, We) ->
    scale_region_1(wpa:sel_strict_face_regions(Faces, We), We, []).

scale_region_1([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices_ccw(Fs, We) of
	      error ->
		  region_error();
	      OuterVs when is_list(OuterVs) ->
		  scale_region_2(OuterVs, Fs, We, Acc0)
	  end,
    scale_region_1(Regs, We, Acc);
scale_region_1([], _, Tv) ->
    Vs = [V || {V,_,_} <- Tv],
    F = fun([Dx0], A0) ->
            Dx = Dx0-1.0,
            foldl(fun({V,Pos0,Vec}, A) ->
                          Pos = e3d_vec:add(Pos0, e3d_vec:mul(Vec, Dx)),
                          [{V,Pos}|A]
                  end, A0, Tv)
        end,
    {Vs,F}.

scale_region_2(OuterVs, Faces, We, Acc) ->
    PlaneNormal = wings_face:face_normal_cw(OuterVs, We),
    WeTemp = wpa:vertex_flatten(OuterVs, PlaneNormal, We),
    Center = wings_vertex:center(OuterVs, WeTemp),
    Vs = wings_face:to_vertices(Faces, We),
    foldl(fun(V, A) ->
                  Pos = wpa:vertex_pos(V, We),
                  Vec = e3d_vec:sub(Pos, Center),
                  [{V,Pos,Vec}|A]
          end, Acc, Vs).

%%%
%%% Rotate Region.
%%%

rotate_region(Faces, We) ->
    rotate_region_1(wpa:sel_strict_face_regions(Faces, We), We, []).

rotate_region_1([Fs|Regs], We, Acc0) ->
    Acc = case wpa:face_outer_vertices_ccw(Fs, We) of
	      error ->
		  region_error();
	      OuterVs when is_list(OuterVs) ->
		  rotate_region(OuterVs, Fs, We, Acc0)
	  end,
    rotate_region_1(Regs, We, Acc);
rotate_region_1([], _, Acc) ->
    wings_drag:compose(Acc).

rotate_region(OuterVs, Faces, We, Acc) ->
    PlaneNormal = wings_face:face_normal_cw(OuterVs, We),
    Vs = wings_face:to_vertices(Faces, We),
    Center = wings_vertex:center(OuterVs, We),
    VsPos = wings_util:add_vpos(Vs, We),
    [{Vs,rotate_fun(Center, VsPos, PlaneNormal)}|Acc].

rotate_fun(Center, VsPos, Axis) ->
    fun([Angle], A) ->
	    rotate(Center, Axis, Angle, VsPos, A)
    end.

rotate({Cx,Cy,Cz}, Axis, Angle, VsPos, Acc0) ->
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(Angle, Axis)),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    foldl(fun({V,Pos0}, Acc) ->
		  Pos = e3d_mat:mul_point(M, Pos0),
		  [{V,Pos}|Acc]
	  end, Acc0, VsPos).

%%%
%%% Flatten Region.
%%%

flatten_region(Faces, We) ->
    flatten_region_1(wpa:sel_strict_face_regions(Faces, We), We).

flatten_region_1([Fs|Regs], We0) ->
    We = case wpa:face_outer_vertices_ccw(Fs, We0) of
	     error -> region_error();
	     OuterVs when is_list(OuterVs) -> flatten_region_2(OuterVs, We0)
	 end,
    flatten_region_1(Regs, We);
flatten_region_1([], We) -> We.

flatten_region_2(Vs, We) ->
    PlaneNormal = wings_face:face_normal_ccw(Vs, We),
    wpa:vertex_flatten(Vs, PlaneNormal, We).

%%%
%%% Utilities.
%%%

-spec region_error() -> no_return().
region_error() ->
    wpa:error_msg(?__(1,"Each region must have exactly one edge loop.")).
