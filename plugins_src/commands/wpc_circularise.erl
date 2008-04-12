%%
%%  wpc_circularise.erl --
%%
%%    Plugin to flatten, equalise, and inflate an edge loop to its maximum or
%%    user specified diameter.
%%
%%  Copyright (c) 2008 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_circularise).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.

%%%% Menu
menu({edge},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [circularise(), separator|NewMenu];
parse([A = {_,loop_cut,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,circularise()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

circularise() ->
    {?__(1,"Circularise"),circularise_options(),
      {?__(2,"Flatten, equalise, and inflate selected edge loop making it circular"),[],
       ?__(3,"Pick Center, Plane, and Vertex marking Stable Ray from center")},[]}.

circularise_options() ->
    fun
      (1,_Ns) -> {edge,circularise};
      (3,_Ns) -> {edge,{circularise,{'ASK',[center,plane]}}};
      (_,_) -> ignore
    end.

%%%% Commands
command({edge,circularise},St) ->
    circle_setup(St);
command({edge,{circularise,{'ASK',Ask}}},#st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
      case wings_edge_loop:edge_loop_vertices(Edges, We) of
        [Vs] ->
            wings:ask(selection_ask(Ask), St, fun({Center,Plane},St0) ->
            second_ask(Center,Plane,Vs,We,St0)
            end);
        _ -> sel_error()
      end;
command({edge,{circularise,{'ASK',_Ask}}},_St) ->
    sel_error();
command({_,{circularise,_}},St) ->
    command({edge,{circularise,{'ASK',[center,plane]}}},St);
command(_,_) -> next.


%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([center|Rest],Ask) ->
    Desc = ?__(1,"Pick Center"),
    selection_ask(Rest,[{point,Desc}|Ask]);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(2,"Pick Plane"),
    selection_ask(Rest,[{axis,Desc}|Ask]).

second_ask(Center,Plane,Vs,We,St) ->
    wings:ask(secondary_sel_ask(Vs), St, fun(RayVs0,St0) ->
    [{_,{_,{RayVs,_,_}}}] = RayVs0,
    circle_pick_all_setup(Vs,RayVs,Center,Plane,We,St0)
    end).

secondary_sel_ask(Vs) ->
    Desc = ?__(1,"Select single vertex from edge loop to mark the stable ray from center"),
    Data = fun(check, St) -> check_selection(St,Vs);
             (exit, {_,_,#st{sel=RayVs}=St}) ->
               case check_selection(St,Vs) of
                 {_,[]} -> {[],[RayVs]};
                 {_,_} -> error
               end
           end,
    {[{Data,Desc}],[],[],[vertex]}.

check_selection(#st{sel=[]},_Vs) ->
    {none,?__(1,"Nothing selected")};
check_selection(#st{sel=[{_Id,{1,{Vert,_,_}}}]}, Vs) ->
    case lists:member(Vert,Vs) of
      true -> {none,[]};
      false -> {none,?__(2,"Vertex must be from the original edge loop")}
    end;
check_selection(_St, _Vs) ->
    {none,?__(3,"Selection can only be a single vertex from the original edge loop")}.

%%%% Setup RMB
circle_pick_all_setup(Vs,RayVs,Center,Axis,We,St) ->
    Deg = (360.0/length(Vs)),
    {Pos,Num} = get_radius_rv(Vs,RayVs,Center,Axis,We,{0.0,0.0,0.0},0,0),
    VertDegList = degrees_from_static_vert(Vs,Deg,Num,1,[]),
    Ray = e3d_vec:norm(e3d_vec:sub(Pos,Center)),
    Tvs = make_circular_1(Vs,We,{Center,Ray,Axis,Pos,VertDegList},[]),
    Flags = [{mode,{mode(),{none,none,normal}}}],
    wings_drag:setup(Tvs,[percent|[falloff]],Flags,St).

%%%% Setup LMB
circle_setup(St) ->
    Tvs = wings_sel:fold(fun(Edges,#we{vp=Vtab}=We,Acc) ->
      case wings_edge_loop:edge_loop_vertices(Edges, We) of
        [Vs] -> Center = wings_vertex:center(Vs,We),
          Deg = (360.0/length(Vs)),
          Axis = e3d_vec:norm(wings_face:face_normal_ccw(Vs, Vtab)),
          {Pos,Index} = get_radius(Vs,Center,Axis,We,0,{0.0,0.0,0.0},0,0),
          VertDegList = degrees_from_static_vert(Vs,Deg,Index,1,[]),
          Ray = e3d_vec:norm(e3d_vec:sub(Pos,Center)),
          make_circular_1(Vs,We,{Center,Ray,Axis,Pos,VertDegList},Acc);
        _ -> sel_error()
      end
    end,[],St),
    Flags = [{mode,{mode(),{none,none,none}}}],
    wings_drag:setup(Tvs,[percent|[falloff]],Flags,St).

%%%% Return the Index and position of the Vertex chosen to represent the stable
%%%% ray from the chosen Center point on the chosen Plane
get_radius_rv([], _, _, _, _, Pos,_,Index) ->
    {Pos,Index};
get_radius_rv([Vert|Vs], RayVs, Center, Plane, We, Pos0, AtIndex, Index) ->
    case Vert == RayVs of
      true  ->
          #we{vp=Vtab} = We,
          Pos = gb_trees:get(Vert,Vtab),
          PosAtCntrPln = intersect_vec_plane(Pos,Center,Plane),
          get_radius_rv(Vs, RayVs, Center, Plane, We, PosAtCntrPln, AtIndex+1, AtIndex+1);
      false ->
          get_radius_rv(Vs, RayVs, Center, Plane, We, Pos0, AtIndex+1, Index)
    end.

%%%% Return the Index and Postion of the Vertex furthest from the Center
%%%% relative to the Plane
get_radius([], _, _, _, _, Pos,_,Index) ->
    {Pos,Index};
get_radius([Vert|Vs], Center, Plane, We, MaxDist, Pos0, AtIndex, Index) ->
    #we{vp=Vtab} = We,
    Pos = gb_trees:get(Vert,Vtab),
    PosAtCntrPln = intersect_vec_plane(Pos,Center,Plane),
    Dist = abs(e3d_vec:dist(PosAtCntrPln,Center)),
    case Dist > MaxDist of
      true  -> get_radius(Vs, Center, Plane, We, Dist, PosAtCntrPln, AtIndex+1, AtIndex+1);
      false -> get_radius(Vs, Center, Plane, We, MaxDist, Pos0, AtIndex+1, Index)
    end.

%%%% Return a tuple list [{Vertex, Degrees_From_Stable_Ray_Vertex}] of all the
%%%% vertices in the edge loop in ccw order with the the number of degrees it
%%%% will be rotated around the center point from the stable ray vertex.
degrees_from_static_vert([],_,_,_,DegList) ->
    DegList;
degrees_from_static_vert([Vert|Vs],Deg,Num,At,DegList) ->
    degrees_from_static_vert(Vs,Deg,Num,At+1,[{Vert,Deg*(At-Num)}|DegList]).

%%%%
make_circular_1(Vs,#we{id=Id}=We,Data,Acc) ->
    VsPos = wings_util:add_vpos(Vs,We),
    [{Id,{Vs,make_circular_fun(Data,VsPos)}}|Acc].

%%%% Modes
mode() ->
    fun
      (help, State) -> mode_help(State);
      ({key,$1},{none,none,normal}) -> {none,none,reverse};
      ({key,$1},{none,none,reverse}) -> {none,none,normal};
      (_,_) -> none
    end.

mode_help(State) ->
    [?__(1,"[+] or [-] Adjust Diameter"),
     mode_help_1(State)].
mode_help_1({none,none,normal}) ->
    ?__(1,"  [1] Reverse Plane Normal");
mode_help_1({none,none,reverse}) ->
    ?__(2,"  [1] Original Plane Normal");
mode_help_1({none,none,none}) ->
    [].

%%%% Mode, Diameter, and Percentage changes
make_circular_fun(Data,VsPos) ->
    fun
      (new_falloff,Diameter) ->
          {Center,Ray,Axis,_Pos,VertDegList} = Data,
          NewPos = e3d_vec:add(Center,e3d_vec:mul(Ray,Diameter/2.0)),
          make_circular_fun({Center,Ray,Axis,NewPos,VertDegList},VsPos);
      (new_mode_data,{_N,_}) ->
          {Center,Ray,Axis0,Pos,VertDegList} = Data,
          Axis = e3d_vec:neg(Axis0),
          make_circular_fun({Center,Ray,Axis,Pos,VertDegList},VsPos);
      ([Dist|_], A) ->
          {Center,_Ray,Axis,Pos,VertDegList} = Data,
          make_circular_2(Center,Axis,Pos,VertDegList,VsPos,Dist,A)
    end.

%%%% Find the Degrees the Vertex is to be rotated in the VertDegList
make_circular_2(Center,Axis,Pos,VertDegList,VsPos,Dist,A) ->
    lists:foldl(fun({Vertex,Vpos}, VsAcc) ->
      {_,{_,Degrees}} = lists:keysearch(Vertex, 1, VertDegList),
      [{Vertex,make_circular(Center,Axis,Pos,Degrees,Vpos,Dist)}|VsAcc]
    end, A, VsPos).

%%%% Main Function
make_circular(_Center,_Axis,_Pos,_Deg,Vpos,0.0) ->
    Vpos;

make_circular(Center,Axis0,Pos,Deg,Vpos,Dist) ->
    Axis =e3d_vec:norm(Axis0),
    NewPos = rotate(Pos,Axis,Center,Deg),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos,Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Dist * Factor)).

rotate(Vpos,Norm,{Cx,Cy,Cz},Angle) ->
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Norm)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).

%%%% Helper Functions
intersect_vec_plane(PosA,PosB,_Vector) when PosA == PosB ->
    PosA;
intersect_vec_plane(PosA,PosB,Vector) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    PlaneNorm = e3d_vec:norm(Vector),
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    case DotProduct of
      0.0 ->
        wings_u:error(?__(1,"Edge loop cannot self intersect at center point"));
      _ ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
        e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection))
    end.

%%%% Selection error
sel_error() ->
    wings_u:error(?__(1,"Selection must form a single closed edge loop")).
