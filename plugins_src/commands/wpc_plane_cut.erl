%%
%%  wpc_plane_cut.erl --
%%
%%    Plane Cut plugin for Vertex, Face, and Body mode.
%%    Optimized for Standard axis cuts and Object selections.
%%    Multiple plane cuts will time out if the operation is taking too long.
%%    There are Loop Cut options in Body mode.
%%    Slice options in Body and Face mode pressing [Alt].
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_plane_cut).
-export([init/0,menu/2,command/2, plane_cut/3]).
-include("wings.hrl").


-import(lists, [foldl/3,usort/1,reverse/1]).

-define(NON_ZERO, 1.0E-4).

init() ->
    true.

%%%
%%% Insert menu heading
%%%

menu({Mode},Menu) when Mode =:= vertex; Mode =:= face; Mode =:= body ->
    reverse(parse(Menu, [], Mode, false));
menu(_,Menu) ->
    Menu.

parse([{_,{flatten,_}}=A|Rest], NewMenu, vertex, false) ->
    parse(Rest, [menu_heading(vertex),A|NewMenu], vertex, true);
parse([{_,weld,_,_}=A|Rest], NewMenu, body, false) ->
    parse(Rest, [menu_heading(body),A|NewMenu], body, true);
parse([{_,bridge,_}=A|Rest], NewMenu, face, false) ->
    parse(Rest, [menu_heading(face),A|NewMenu], face, true);
parse([Elem|Rest], NewMenu, Mode,  Found) ->
    parse(Rest, [Elem|NewMenu], Mode, Found);
parse([], NewMenu, _, true) ->
    NewMenu;
parse([], NewMenu, Mode, false) ->
    [menu_heading(Mode)|NewMenu].

menu_heading(body) ->
    [{?__(1,"Plane Cut"),{plane_cut,object_menu(plane_cut)}},
     {?__(2,"Slice"),{slice,object_menu(pre_slice)}}];
menu_heading(Mode) ->
    [{?__(1,"Plane Cut"),{plane_cut,plane_cut_menu(Mode)}}].

object_menu(Cmd) ->
    fun(help,_) when Cmd =:= pre_slice ->
           {?__(1,"Slice objects into equal parts along standard axis"),
            ?__(2,"Pick axis and Loop Cut the result"),
            ?__(3,"Pick axis")};
       (help,_) when Cmd =:= plane_cut ->
           {?__(4,"Cut around standard axis"),
            ?__(5,"Pick plane and Loop Cut the result"),
            ?__(6,"Pick plane")};
       (1,_) -> standard_axes(Cmd, body);
       (2,_) -> {body,{Cmd,{loop_cut,{'ASK',[axis]}}}};
       (3,_) -> {body,{Cmd,{'ASK',[axis]}}};
       (_,_) -> ignore
    end.

plane_cut_menu(face) ->
    fun(help,_) ->
           {?__(1,"Cut around standard axis"),
            ?__(2,"Slice selection into equal parts; pick plane"),
            ?__(3,"Pick plane")};
       (1,_) -> standard_axes(plane_cut, face);
       (2,_) -> {face,{pre_slice,{'ASK',[axis]}}};
       (3,_) -> {face,{plane_cut,{'ASK',[axis]}}};
       (_,_) -> ignore
    end;
plane_cut_menu(vertex) ->
    fun(help,_) ->
           {?__(4,"Cut around standard axis at each selected vertex"),[],
            ?__(3,"Pick plane")};
       (1,_) -> standard_axes(plane_cut, vertex);
       (3,_) -> {vertex,{plane_cut,{'ASK',[axis]}}};
       (_,_) -> ignore
    end.

standard_axes(Cmd, Mode) ->
     [axis_menu(Cmd, Mode, x),
      axis_menu(Cmd, Mode, y),
      axis_menu(Cmd, Mode, z),
      separator,
      axis_menu(Cmd, Mode, last_axis),
      axis_menu(Cmd, Mode, default_axis)].

axis_menu(Cmd, body, Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Fun = fun
      (help,_) when Cmd =:= pre_slice ->
        Help0 = ?__(1,"Slice into equal parts along the ~s axis"),
        Help = wings_util:format(Help0, [AxisStr]),
        {Help, [], Help ++ ?__(2," and Loop Cut the result")};
      (help,_) when Cmd =:= plane_cut ->
        Help0 = ?__(3,"Cut object around the ~s axis"),
        Help = wings_util:format(Help0, [AxisStr]),
        {Help, [], Help ++ ?__(2," and Loop Cut the result")};
      (1,_) -> {body,{Cmd,Axis}};
      (3,_) -> {body,{Cmd,{loop_cut,Axis}}};
      (_,_) -> ignore
    end,
    {AxisStr,{Cmd,Fun}};
axis_menu(_, vertex, Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Help0 = ?__(4,"Cut objects or faces around the ~s axis at each selected vertex"),
    Help = wings_util:format(Help0, [AxisStr]),
    {AxisStr,Axis,Help};
axis_menu(_, face, Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Fun = fun
      (help,_) ->
        Help0 = ?__(5,"Cut faces around the ~s axis"),
        Help = wings_util:format(Help0, [AxisStr]),
        SHelp0 = ?__(6,"Slice selection into equal parts along the ~s axis"),
        SHelp = wings_util:format(SHelp0, [AxisStr]),
        {Help,[],SHelp};
      (1,_) -> {face,{plane_cut,Axis}};
      (3,_) -> {face,{pre_slice,Axis}};
      (_,_) -> ignore
    end,
    {AxisStr,{plane_cut,Fun}}.

slices_dialog(Mode,Cmd) ->
    wings_ask:dialog(?__(1,"Slice into Equal Parts"),
      [{vframe,[{hframe,[{slider,{text,2,[{range,{2,100}}]}}]}]}],
      fun([Parts]) ->
        {Mode,{slice,{Parts,Cmd}}}
      end).

%%%
%%% Commands
%%%

command({Mode,{pre_slice,Cmd}}, _) ->
    slices_dialog(Mode,Cmd);

command({body,{plane_cut,{loop_cut,{'ASK',_}}}}, St) ->
    wings:ask({[axis,point],[]}, St, fun (Res,St0) ->
      ?SLOW(plane_cut(true, Res, St0))
    end);
command({Mode,{plane_cut,{'ASK',_}}}, St) when Mode =:= face; Mode =:= body ->
    wings:ask({[axis,point],[]}, St, fun (Res,St0) ->
      ?SLOW(plane_cut(false, Res, St0))
    end);
command({body,{plane_cut,{loop_cut,Axis}}}, St) ->
    wings:ask({[point],[]}, St, fun (Point,St0) ->
      ?SLOW(plane_cut(true, {Axis,Point}, St0))
    end);
command({Mode,{plane_cut,Axis}}, St) when Mode =:= face; Mode =:= body ->
    wings:ask({[point],[]}, St, fun (Point,St0) ->
      ?SLOW(plane_cut(false, {Axis,Point}, St0))
    end);

command({body,{slice,{N,{loop_cut,{'ASK',Ask}}}}}, St) ->
    wings:ask({Ask,[]}, St, fun (Axis,St0) ->
      ?SLOW(plane_cut(true, {Axis,N}, St0))
    end);
command({Mode,{slice,{N,{'ASK',Ask}}}}, St) when Mode =:= face; Mode =:= body ->
    wings:ask({Ask,[]}, St, fun (Axis,St0) ->
      ?SLOW(plane_cut(false, {Axis,N}, St0))
    end);
command({body,{slice,{N,{loop_cut,Axis}}}}, St) ->
      ?SLOW(plane_cut(true, {Axis,N}, St));
command({Mode,{slice,{N,Axis}}}, St) when Mode =:= face; Mode =:= body ->
      ?SLOW(plane_cut(false, {Axis,N}, St));

command({vertex,{plane_cut,{'ASK',Ask}}}, St) ->
    wings:ask(secondary_sel_ask(), St, fun (SelSt,St0) ->
        wings:ask({Ask,[]}, St0, fun (Plane,St1) ->
        ?SLOW(plane_cut(false, {Plane,SelSt}, St1))
      end)
    end);
command({vertex,{plane_cut,Axis}}, St) ->
    wings:ask(secondary_sel_ask(), St, fun (SelSt,St0) ->
        ?SLOW(plane_cut(false, {Axis,SelSt}, St0))
    end);

command(_, _) -> next.

secondary_sel_ask() ->
    Desc = ?__(2,"Select the objects or faces to cut"),
    Fun = sel_fun(),
    {[{Fun,Desc}],[],[],[face,body]}.
check_selection(#st{sel=[]}) -> {none,?__(1,"Nothing selected")};
check_selection(_) -> {none,[]}.

sel_fun() ->
    fun
      (check, St) ->
        check_selection(St);
      (exit, {_,_,St}) ->
        case check_selection(St) of
          {_,[]} ->
            {result,St};
          {_,_} -> error
        end;
      (exit,_) -> error
    end.

%%%
%%% Process commands
%%%

plane_cut(Cut, {Axis0,N}, #st{selmode=SelMode}=St0) when is_integer(N) ->
    T1 = now(),
    Axis = axis_conv(Axis0),
    Points = slice_points(Axis, N, St0),

    {St1,Sel} = case SelMode of
      face ->
        wings_sel:mapfold(fun(Faces, #we{id=Id}=We0, Acc) ->
            {NewEdges,_,_,We} = foldl(fun
              (Point,{Es,Fs0,Seen0,We1}) ->
                T2 = timer:now_diff(now(), T1),
                if T2 > 20000000 -> planecut_error();
                  true ->
                    {NewEs,We2} = intersects(Fs0, Axis, Point, We1),
                    SeenVs = wings_edge:to_vertices(NewEs,We2),
                    Seen = ordsets:union(SeenVs,Seen0),
                    NewFaces = wings_we:new_items_as_gbset(face, We1, We2),
                    Fs = gb_sets:union(NewFaces,Fs0),
                    {gb_sets:union(NewEs, Es),Fs,Seen,We2}
                end
              end, {gb_sets:new(),Faces,[],We0}, Points),
            {We,[{Id,NewEdges}|Acc]}
        end,[],St0);
      body ->
        wings_sel:mapfold(fun(_, #we{id=Id}=We0, Acc) ->
            {NewEdges,_,We} = foldl(fun(Point,{Es,Seen0,We1}) ->
                T2 = timer:now_diff(now(), T1),
                if T2 > 20000000 -> planecut_error();
                  true ->
                    EdgePos2Cut = intersects(Axis, Point, We1),
                    {NewEs,We2} = cut_intersecting_edges(false, Axis, EdgePos2Cut, We1),
                    SeenVs = wings_edge:to_vertices(NewEs,We2),
                    Seen = ordsets:union(SeenVs,Seen0),
                    {gb_sets:union(NewEs, Es),Seen,We2}
                end
              end, {gb_sets:new(),[],We0},  Points),
            {We,[{Id,NewEdges}|Acc]}
        end,[],St0)
    end,
    St = wings_sel:set(edge, Sel, St1),
    case Cut of
      true ->
        {save_state,loop_cut(Axis, all, wings_sel:valid_sel(St))};
      false ->
        {save_state,wings_sel:valid_sel(St)}
    end;

plane_cut(_, {Axis0,Point}, #st{selmode=face}=St0) ->
    Axis = axis_conv(Axis0),
    {St1,Sel} = wings_sel:mapfold(fun(Faces, #we{id=Id}=We0, Acc) ->
            {NewEdges,We} = intersects(Faces, Axis, Point, We0),
            {We,[{Id,NewEdges}|Acc]}
        end,[],St0),
    St = wings_sel:set(edge, Sel, St1),
    {save_state,wings_sel:valid_sel(St)};

%% Vertex mode
plane_cut(_, {Axis0,#st{selmode=SelMode}=StA},
  #st{selmode=vertex,repeatable=R,drag_args=Da}=StB) ->
    Axis = axis_conv(Axis0),
    T1 = now(),
    Points = wings_sel:fold(fun(Vs0,#we{vp=Vtab},Acc0) ->
            Vs = gb_sets:to_list(Vs0),
            foldl(fun(V, Acc1) ->
                    [{V,array:get(V, Vtab)}|Acc1]
                  end, Acc0, Vs)
          end, [], StB),
    {St1,Sel} = case SelMode of
      face ->
        wings_sel:mapfold(fun(Faces, #we{id=Id}=We0, Acc) ->
            {NewEdges,_,_,We} = foldl(fun
              ({V,Point},{Es,Fs0,Seen0,We1}=A) ->
                T2 = timer:now_diff(now(), T1),
                if T2 > 20000000 -> planecut_error();
                true ->
                  case ordsets:is_element(V,Seen0) of
                    true -> A;
                    false ->
                      {NewEs,We2} = intersects(Fs0, Axis, Point, We1),
                      SeenVs = wings_edge:to_vertices(NewEs,We2),
                      Seen = ordsets:union(SeenVs,Seen0),
                      NewFaces = wings_we:new_items_as_gbset(face, We1, We2),
                      Fs = gb_sets:union(NewFaces,Fs0),
                      {gb_sets:union(NewEs, Es),Fs,Seen,We2}
                  end
                end
              end, {gb_sets:new(),Faces,[],We0}, Points),
            {We,[{Id,NewEdges}|Acc]}
        end,[],StA);
      body ->
        wings_sel:mapfold(fun(_, #we{id=Id}=We0, Acc) ->
            {NewEdges,_,We} = foldl(fun({V,Point},{Es,Seen0,We1}=A) ->
                T2 = timer:now_diff(now(), T1),
                if T2 > 20000000 -> planecut_error();
                true ->
                  case ordsets:is_element(V,Seen0) of
                    true -> A;
                    false ->
                      EdgePos2Cut = intersects(Axis, Point, We1),
                      {NewEs,We2} = cut_intersecting_edges(false, Axis, EdgePos2Cut, We1),
                      SeenVs = wings_edge:to_vertices(NewEs,We2),
                      Seen = ordsets:union(SeenVs,Seen0),
                      {gb_sets:union(NewEs, Es),Seen,We2}
                  end
                end
              end, {gb_sets:new(),[],We0},  Points),
            {We,[{Id,NewEdges}|Acc]}
        end,[],StA)
    end,
    St = wings_sel:set(edge, Sel, St1),
    {save_state,wings_sel:valid_sel(St#st{repeatable=R,ask_args=Axis,drag_args=Da})};

plane_cut(Cut, {Axis0,Point}, St0) ->
    Axis = axis_conv(Axis0),
    {St1,Sel} = wings_sel:mapfold(fun(_, #we{id=Id}=We0, Acc) ->
            EdgePos2Cut = intersects(Axis, Point, We0),
            {NewEdges,We} = cut_intersecting_edges(Cut, Axis, EdgePos2Cut, We0),
            {We,[{Id,NewEdges}|Acc]}
        end,[],St0),
    St = wings_sel:set(edge, Sel, St1),
    case Cut of
      true ->
        {save_state,loop_cut(Axis, Point, wings_sel:valid_sel(St))};
      false ->
        {save_state,wings_sel:valid_sel(St)}
    end.

planecut_error() ->
    wings_u:error_msg(?__(1,"This is taking too long.\nTry again with a smaller selection.")).

%%%
%%% Calculate slice points distributed equally along the given axis
%%%

slice_points(Axis, N, #st{selmode=face}=St) ->
    Zero = e3d_vec:zero(),
    [{_,PosA},{_,PosB}] =
    wings_sel:fold(fun(Faces,#we{vp=Vtab}=We,Acc) ->
        Vs = wings_face:to_vertices(Faces,We),
        foldl(fun
          (V,none) ->
            Pos = array:get(V, Vtab),
            D = dist_along_vector(Pos, Zero, Axis),
            [{D,Pos},{D,Pos}];
          (V,[{MinD,_}=Min,{MaxD,_}=Max]) ->
            Pos = array:get(V, Vtab),
            D = dist_along_vector(Pos, Zero, Axis),
            if D < MinD -> [{D,Pos},Max];
               D > MaxD -> [Min,{D,Pos}];
               true -> [Min,Max]
            end
        end, Acc, Vs)
    end, none, St),
    Dist = abs(dist_along_vector(PosA, PosB, Axis)) / N,
    get_point_positions(PosA, Dist, Axis, N-1);
slice_points(Axis, N, #st{selmode=body}=St) ->
    Zero = e3d_vec:zero(),
    [{_,PosA},{_,PosB}] =
    wings_sel:fold(fun(_,#we{vp=Vtab},Acc) ->
        array:sparse_foldl(fun
          (_, Pos, none) ->
            D = dist_along_vector(Pos, Zero, Axis),
            [{D,Pos},{D,Pos}];
          (_, Pos, [{MinD,_}=Min,{MaxD,_}=Max]) ->
            D = dist_along_vector(Pos, Zero, Axis),
            if D < MinD -> [{D,Pos},Max];
               D > MaxD -> [Min,{D,Pos}];
               true -> [Min,Max]
            end
        end, Acc, Vtab)
    end, none, St),
    Dist = abs(dist_along_vector(PosA, PosB, Axis)) / N,
    get_point_positions(PosA, Dist, Axis, N-1).


dist_along_vector({Xa,Ya,Za},{Xb,Yb,Zb},{Vx,Vy,Vz}) ->
%% Return Distance between PosA and PosB along Normalized Vector
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

get_point_positions(_, _, _, 0) -> [];
get_point_positions(Pos0, Dist, Axis, N) ->
    Pos = e3d_vec:add(Pos0, e3d_vec:mul(Axis, Dist)),
    [Pos|get_point_positions(Pos, Dist, Axis, N-1)].


%% There are optimizations for Standard Axes (see opposite_sides).
%% Body mode is faster than face mode for wholly selected objects since face
%% mode has to cenvert the selection to edges as part of processing the
%% selection; body mode just folds over the Etab.

%%%
%%% Body mode
%%%

intersects({1.0,0.0,0.0}=Plane, {X,_,_}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {Xa,_,_} = PosA = array:get(Va, Vtab),
        {Xb,_,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Xa, Xb, X) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects({0.0,1.0,0.0}=Plane, {_,Y,_}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {_,Ya,_} = PosA = array:get(Va, Vtab),
        {_,Yb,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Ya, Yb, Y) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects({0.0,0.0,1.0}=Plane, {_,_,Z}=CutPoint, #we{es=Etab,vp=Vtab}) ->
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {_,_,Za} = PosA = array:get(Va, Vtab),
        {_,_,Zb} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Za, Zb, Z) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab);
intersects(Plane, CutPoint, #we{es=Etab,vp=Vtab}) ->
    SideArray = assign_side(Vtab, Plane, CutPoint),
    array:sparse_foldl(fun
      (Edge, #edge{vs=Va,ve=Vb}, EdgesToCut1) ->
        {SideA,PosA} = array:get(Va, SideArray),
        {SideB,PosB} = array:get(Vb, SideArray),
        case oposite_sides(SideA, SideB) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Etab).

%%%
%%% Face mode
%%%

intersects(Faces, {1.0,0.0,0.0}=Plane, {X,_,_}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {Xa,_,_} = PosA = array:get(Va, Vtab),
        {Xb,_,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Xa, Xb, X) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, {0.0,1.0,0.0}=Plane, {_,Y,_}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {_,Ya,_} = PosA = array:get(Va, Vtab),
        {_,Yb,_} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Ya, Yb, Y) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, {0.0,0.0,1.0}=Plane, {_,_,Z}=CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        {_,_,Za} = PosA = array:get(Va, Vtab),
        {_,_,Zb} = PosB = array:get(Vb, Vtab),
        case oposite_sides(Za, Zb, Z) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0);
intersects(Faces, Plane, CutPoint, #we{es=Etab,vp=Vtab}=We0) ->
    Edges = wings_face:to_edges(Faces, We0),
    EdgePos2Cut = foldl(fun(Edge, EdgesToCut1) ->
        #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
        PosA = array:get(Va, Vtab),
        PosB = array:get(Vb, Vtab),
        case oposite_sides(PosA, PosB, CutPoint, Plane) of
          true ->
            EdgeVec = e3d_vec:norm_sub(PosA, PosB),
             case abs(e3d_vec:dot(Plane, EdgeVec)) < ?NON_ZERO of
               true -> EdgesToCut1;
               false ->
                 Point = intersect_vec_plane(CutPoint, PosA, Plane, EdgeVec),
                 [{Edge,Point}|EdgesToCut1]
             end;
          false -> EdgesToCut1
        end
    end, [], Edges),
    cut_intersecting_edges(Faces, Plane, EdgePos2Cut, We0).

%%%
%%% Cut edges and connect the vertices
%%%

cut_intersecting_edges(Type, Plane, EdgePos, We0) ->
    {Vs0,We1} = cut_edges(EdgePos, We0),
    Vs = ordsets:from_list(Vs0),
    {GoodVs,We} = connect(Type, Plane, Vs, We1),
    Edges = vs_to_edges(GoodVs, We, []),
    {Edges,We}.

connect(true, Plane, Vs0, We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,Vs}, {VsA,NewWe}) ->
      FaceNorm = wings_face:normal(Face, We),
      case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
        true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
        false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
      end
    end, {Vs0,We}, FaceVs);
connect(false, Plane, Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun
           ({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
           ({Face,Vs}, {VsA,NewWe}) ->
              FaceNorm = wings_face:normal(Face, We),
              case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
                true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
                false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
              end
    end, {Vs0,We}, FaceVs);
connect(Faces, Plane, Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun
           ({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
           ({Face,Vs}, {VsA,NewWe}=Acc) ->
             case gb_sets:is_member(Face, Faces) of
               true ->
                 FaceNorm = wings_face:normal(Face, We),
                 case abs(e3d_vec:dot(Plane, FaceNorm)) > 0.999 of
                   true -> {wings_face:to_vertices([Face],We)++VsA,NewWe};
                   false -> {VsA,wings_vertex:connect(Face, Vs, NewWe)}
                 end;
               false -> Acc
             end
      end, {Vs0,We}, FaceVs).

vs_to_edges([Va|Vs], We, Acc0) ->
%% finds the edges between any two listed vertices and returns a gb_set fo edges
    Edges = wings_vertex:fold(fun(Edge, _, EdgeRec, Acc) ->
         Vb = wings_vertex:other(Va, EdgeRec),
         case ordsets:is_element(Vb,Vs) of
           true -> [Edge|Acc];
           _ -> Acc
         end
     end, Acc0, Va, We),
    vs_to_edges(Vs, We, Edges);
vs_to_edges([], _, Edges) ->
    gb_sets:from_list(usort(Edges)).

cut_edges(Es, We0) ->
%% Fold over the list Es of which each entry is a tuple with the Edge id and the
%% proposed cut position. If the proposed cut position is VERY close to one of
%% the edge's vertices, then don't cut that, but return that Vertex id.
%% If the edge is cut, then return the new vertex id. In a later function, all
%% of the collected vertex id will be connected with new edges making the final
%% edge loop.
    lists:mapfoldl(fun
        ({Edge,Pos}, #we{es=Etab,vp=Vtab}=We1) ->
            #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
            PosA = array:get(Va, Vtab),
            case e3d_vec:dist(PosA, Pos) < ?NON_ZERO of
              true -> {Va,We1};
              false ->
                PosB = array:get(Vb, Vtab),
                case e3d_vec:dist(PosB, Pos) < ?NON_ZERO of
                  true -> {Vb,We1};
                  false ->
                    {We,V} = wings_edge:fast_cut(Edge, Pos, We1),
                    {V,We}
                end
            end
         end, We0, Es).

%%%
%%% Plane Cut utilities
%%%

intersect_vec_plane(PosA, PosB, Plane, EdgeVec) ->
%% Return point where Vector through PosA intersects with plane at PosB
    case e3d_vec:dot(EdgeVec,Plane) of
      0.0 ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane),
        e3d_vec:add(PosB, e3d_vec:mul(Plane, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosA, PosB), Plane) / Dot,
        e3d_vec:add(PosB, e3d_vec:mul(EdgeVec, Intersection))
    end.

%% Tests whether the 2 vertices of an edge are on opposite sides of the Plane.
%% The opposite_sides function that is used depends on the Plane and the
%% selection mode.
oposite_sides(A, B) when A =:= on_vertex; B =:= on_vertex -> true;
oposite_sides(Side, Side) -> false;
oposite_sides(_, _) -> true.

oposite_sides(_, B, B) -> true;
oposite_sides(A, _, A) -> true;
oposite_sides(A, B, C) ->
    SideA = A < C,
    SideB = B < C,
    SideA =/= SideB.

oposite_sides(PosA, PosB, CutPoint, Plane) ->
    VecA = e3d_vec:norm_sub(PosA, CutPoint),
    VecB = e3d_vec:norm_sub(PosB, CutPoint),
    Zero = e3d_vec:zero(),
    case e3d_vec:dot(VecA, Plane) =< 0 of
      _ when VecA =:= Zero; VecB =:= Zero ->
        true;
      true ->
        e3d_vec:dot(VecB, Plane) >= 0;
      false ->
        e3d_vec:dot(VecB, Plane) =< 0
    end.

assign_side(Vtab, Plane, CutPoint) ->
%% Create an array using each vertex id as the index and {Dot < 0, Pos} as the
%% value. The Dot boolean is later used to determine the side of the plane on
%% which the vertex resides. The position is stored just so is doesn't have to
%% be looked up again.
    array:sparse_foldl(fun
      (V, Pos, Array) ->
        Vec = e3d_vec:norm_sub(Pos, CutPoint),
        Dot = e3d_vec:dot(Vec, Plane),
        case Vec =:= e3d_vec:zero() of
          true ->
            array:set(V, {on_vertex ,Pos}, Array);
          false ->
            array:set(V, {Dot =< 0 ,Pos}, Array)
        end
    end, Vtab, Vtab).

axis_conv(Axis) ->
%% Converts an atom axis to a tuple axis.
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      last_axis ->
        {_, Dir} = wings_pref:get_value(last_axis),
        Dir;
      default_axis ->
        {_, Dir} = wings_pref:get_value(default_axis),
        Dir;
      {X,Y,Z} -> {X,Y,Z};
      {LastAxis,_} -> LastAxis
    end.

%%%
%%% Loop Cut modified from wings_edge_cmd.erl
%%%

loop_cut(Axis, Point, St0) ->
    {Sel,St} = wings_sel:fold(fun(Edges, #we{id=Id,fs=Ftab}=We0, {Sel0,St1}) ->
        AdjFaces = wings_face:from_edges(Edges, We0),
        case loop_cut_partition(AdjFaces, Edges, We0, []) of
          [_] ->
            {Sel0,St1}; 
          [_|Parts0] ->
            Parts = [gb_sets:to_list(P) || P <- Parts0],
            FirstComplement = ordsets:union(Parts),
            First = ordsets:subtract(gb_trees:keys(Ftab), FirstComplement),
            We = wings_dissolve:complement(First, We0),
            #st{shapes=Shapes} = St1,
            St = St1#st{shapes=gb_trees:update(Id, We, Shapes)},
            Sel = select_one_side(Axis, Point, Id, Sel0, We),
            loop_cut_make_copies(Parts, Axis, Point, We0, Sel, St)
        end
    end, {[],St0}, St0),
    wings_sel:set(body, Sel, St).

loop_cut_make_copies([P|Parts], Axis, Point, We0, Sel0, #st{onext=Id}=St0) ->
    We = wings_dissolve:complement(P, We0),
    Sel = select_one_side(Axis, Point, Id, Sel0, We),
    St = wings_shape:insert(We, cut, St0),
    loop_cut_make_copies(Parts, Axis, Point, We0, Sel, St);
loop_cut_make_copies([], _, _, _, Sel, St) -> {Sel,St}.

loop_cut_partition(Faces0, Edges, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
      true -> Acc;
      false ->
        {AFace,Faces1} = gb_sets:take_smallest(Faces0),
        Reachable = collect_faces(AFace, Edges, We),
        Faces = gb_sets:difference(Faces1, Reachable),
        loop_cut_partition(Faces, Edges, We, [Reachable|Acc])
    end.

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
      true -> Acc0;
      false ->
        {Face,Work1} = gb_sets:take_smallest(Work0),
        Acc = gb_sets:insert(Face, Acc0),
        Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
        collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(_, Edge, Rec, A) ->
          case gb_sets:is_member(Edge, Edges) of
            true -> A;
            false ->
              Of = wings_face:other(Face, Rec),
              case gb_sets:is_member(Of, Res) of
                true -> A;
                false -> gb_sets:add(Of, A)
              end
          end
      end, Work, Face, We).

select_one_side(_, all, Id, Sel, _) ->
    [{Id,gb_sets:singleton(0)}|Sel];
select_one_side(Plane, Point, Id, Sel, #we{fs=Fs}=We) ->
    {Face,_} = gb_trees:smallest(Fs),
    Center = wings_face:center(Face, We),
    Vec = e3d_vec:sub(Point, Center),
    case e3d_vec:dot(Plane, Vec) < 0.0 of
      true -> [{Id,gb_sets:singleton(0)}|Sel];
      false -> Sel
    end.
