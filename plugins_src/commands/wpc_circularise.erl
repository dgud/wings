%%
%%  wpc_circularise.erl --
%%
%%    Plugin to flatten, equalise, and inflate open or closed edge loops
%%    makaing them circular
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
    [circular_arc_menu(), separator|NewMenu];
parse([A = {_,loop_cut,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,circular_arc_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

circular_arc_menu() ->
    {?__(1,"Circularise"), circular_arc_options(),
      {?__(2,"Flatten, equalise, and inflate selected edge loops making them circular"),[],
       ?__(4,"Circularise a single open or closed loop using secondary selections")},[]}.

circular_arc_options() ->
    fun
      (1,_Ns) -> {edge,{circularise,{'ASK',[plane]}}};
      (3,_Ns) -> {edge,circularise_center};
      (_,_) -> ignore
    end.

%%%% Commands
command({edge,{circularise,{'ASK',Ask}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] ->
        case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
          true ->
            circle_setup(St);
          false ->
            wings:ask(selection_ask(Ask), St, fun(Plane,St0) ->
            arc_setup(Plane,Vs,We,St0)
            end)
        end;
      _ -> circle_setup(St)
    end;
command({edge,{circularise,Plane}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] ->
        case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
          true ->
            circle_setup(St);
          false ->
            arc_setup(Plane,Vs,We,St)
        end;
      _ -> circle_setup(St)
    end;

command({edge,{circularise,_}}, St) ->
    circle_setup(St);

%%%%
command({edge, circularise_center}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            wings:ask(selection_ask([center,plane]), St, fun({Center,Plane},St0) ->
            second_ask(Center,Plane,Edges,Vs0,We,St0)
            end);
          _other ->
            wings:ask(selection_ask([plane,arc_center]), St, fun({Plane,Center},St0) ->
            arc_center_setup(Plane,Center,Vs,We,St0)
            end)
        end;
      _ ->
        circ_sel_error()
    end;

command({edge,{circularise_center,{Plane0,Center0}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            wings:ask(selection_ask([center,plane]), St, fun({Center,Plane},St0) ->
            second_ask(Center,Plane,Edges,Vs0,We,St0)
            end);
          _other ->
            arc_center_setup(Plane0,Center0,Vs,We,St)
        end;
      _ ->
        circ_sel_error()
    end;

command({edge,{circularise_center,_}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            wings:ask(selection_ask([center,plane]), St, fun({Center,Plane},St0) ->
            second_ask(Center,Plane,Edges,Vs0,We,St0)
            end);
          _other ->
            wings:ask(selection_ask([plane,arc_center]), St, fun({Plane,Center},St0) ->
            arc_center_setup(Plane,Center,Vs,We,St0)
            end)
        end;
      _ ->
        circ_sel_error()
    end;

command({edge,{circularise_center,_}}, _) ->
    circ_sel_error();

command(_, _) ->
    next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Pick plane"),
    selection_ask(Rest,[{axis,Desc}|Ask]);

selection_ask([center|Rest],Ask) ->
    Desc = ?__(2,"Pick center point"),
    selection_ask(Rest,[{point,Desc}|Ask]);

selection_ask([arc_center|Rest],Ask) ->
    Desc = ?__(3,"Pick point from which the center will be calculated relative to chosen plane and ends of edge selection"),
    selection_ask(Rest,[{point,Desc}|Ask]).

%%%% Secondary Selection for Circularise Rmb
second_ask(Center,Plane,Edges,Vs,We,St) ->
    wings:ask(secondary_sel_ask(Edges,Vs), St, fun(RayPos,St0) ->
    circle_pick_all_setup(Vs,RayPos,Center,Plane,We,St0)
    end).

secondary_sel_ask(Edges,Vs) ->
    Desc = ?__(1,"Select a single edge or vertex from the original edge loop marking the stable ray from the center point"),
    Data = fun
             (check, #st{selmode=vertex}=St) ->
               check_selection(St,Vs);
             (check, #st{selmode=edge}=St) ->
               check_selection(St,Edges);
             (exit, {_,_,#st{shapes=Shs,selmode=vertex,sel=[{Id,Sel}]}=St}) ->
               case check_selection(St,Vs) of
                 {_,[]} ->
                   We = gb_trees:get(Id, Shs),
                   {_,{RayVs,_,_}} = Sel,
                   RayPos = wings_vertex:pos(RayVs, We),
                   {[],[RayPos]};
                 {_,_} -> error
                end;
             (exit, {_,_,#st{shapes=Shs,selmode=edge,sel=[{Id,Sel}]}=St}) ->
               case check_selection(St,Edges) of
                 {_,[]} ->
                   We = gb_trees:get(Id, Shs),
                   {_,{RayEdge,_,_}} = Sel,
                   #edge{vs=V0s,ve=V0e} = gb_trees:get(RayEdge, We#we.es),
                   VPos1 = wings_vertex:pos(V0s, We),
                   VPos2 = wings_vertex:pos(V0e, We),
                   RayPos = e3d_vec:average(VPos1, VPos2),
                   {[],[RayPos]};
                 {_,_} -> error
               end;
             (exit,_) -> error
           end,
    {[{Data,Desc}],[],[],[vertex,edge]}.

check_selection(#st{sel=[]},_Vs) ->
    {none,?__(1,"Nothing selected")};

check_selection(#st{selmode=vertex,sel=[{_Id,{1,{Vert,_,_}}}]}, Vs) ->
    case lists:member(Vert,Vs) of
      true -> {none,[]};
      false -> {none,?__(2,"Vertex must be from the original edge loop")}
    end;

check_selection(#st{selmode=edge, sel=[{_Id,{1,{Edge,_,_}}}]}, Edges) ->
    case lists:member(Edge,Edges) of
      true -> {none,[]};
      false -> {none,?__(3,"Edge must be from the original edge loop")}
    end;

check_selection(_, _) ->
    {none,?__(4,"Only a single edge or vertex may be selected")}.


% % % % % % % % % % %
%    Data Setup     %
% % % % % % % % % % %

%%%% Arc Setup LMB
arc_setup(Plane,Vs,#we{id=Id}=We,St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,none,none},
    {VPos1,_,Cross,Hinge,DegVertList,NumVs,Vertices,VsPos} = arc_data(Plane,Vs,We),
    Opp = e3d_vec:dist(Hinge, VPos1),
    Data = {e3d_vec:norm(Cross), Opp, Plane, VPos1, Hinge, DegVertList, NumVs},
    Tvs = [{Id,{Vertices, make_arc_fun(Data,State,VsPos)}}],
    %% 'skip' is simply a place holder between 'angle' and 'percent'.
    %% Using 'skip' allows the two drag totals to be calculated separately when
    %% only two amounts are required.
    Flags = [{mode, {arc_modes(),State}},{initial,[0.0,0.0,1.0]}],
    Units = [angle, skip, percent],
    wings_drag:setup(Tvs, Units, Flags, St).

%%%% Arc Setup RMB
arc_center_setup(Plane,Center,Vs,#we{id=Id}=We,St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,normal,acute},
    {VPos1,VPos2,Cross,Hinge,DegVertList,NumVs,Vertices,VsPos} = arc_data(Plane,Vs,We),
    CenterPoint = intersect_vec_plane(Hinge,Center,Cross),

    %% get angle
    Vec1 = e3d_vec:sub(CenterPoint,VPos1),
    Vec2 = e3d_vec:sub(CenterPoint,VPos2),
    Angle = e3d_vec:degrees(Vec1,Vec2),

    Data = {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},
    Tvs = [{Id,{Vertices,make_arc_center_fun(Data,State,VsPos)}}],
    Flags = [{mode,{arc_modes(),State}},{initial,[1.0]}],
    Units = [percent],
    wings_drag:setup(Tvs, Units, Flags, St).

arc_data(Plane,Vs,We) ->
    %% StartVs and EndVs are in 3rd of First and 2nd of Last
    [{_,_,StartVs}|_] = Vs,
    {_,EndVs,_} = lists:last(Vs),
    VPos1 = wings_vertex:pos(StartVs, We),
    VPos2 = wings_vertex:pos(EndVs, We),
    Hinge = e3d_vec:average(VPos1, VPos2),
    Chord = e3d_vec:sub(VPos1, VPos2),
    Cross = e3d_vec:cross(Chord,Plane),
    {Vertices,DegVertList} = make_degree_vert_list(Vs,[],0,[]),
    NumVs = length(DegVertList) + 1,
    VsPos = wings_util:add_vpos(Vertices,We),
    {VPos1, VPos2, Cross,  Hinge, DegVertList, NumVs, Vertices, VsPos}.

%%%% Index vertices for open edge loop (Arc)
make_degree_vert_list([],Vertices,_,DegVertList) ->
    {Vertices,DegVertList};

make_degree_vert_list([_|Vs], Vertices, 0, DegVertList) ->
    %% This is the first time through the list.  Since the first Vertex (VPos1)
    %% doesn't move, remove it from the list so it isn't processed later in
    %% arc/5. (The last vertex (VPos2) is already not in the list).
    make_degree_vert_list(Vs, Vertices, 1, DegVertList);

make_degree_vert_list([{_,_,Vert}|Vs], Vertices, Index, DegVertList) ->
    make_degree_vert_list(Vs, [Vert|Vertices], Index+1, [{Vert, Index}|DegVertList]).

%%%% Circularise Setup RMB
circle_pick_all_setup(Vs,RayPos,Center,Axis0,#we{vp=Vtab,id=Id}=We,St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,normal,none},
    Axis = e3d_vec:norm(Axis0),
    Deg = (360.0/length(Vs)),
    {Pos,Num} = get_radius_rmb(Vs,RayPos,Center,Axis,Vtab,{0.0,0.0,0.0},lastpos,firstpos,0.0,0.0),
    VertDegList = degrees_from_static_ray(Vs,Deg,Num,1,[]),
    Ray = e3d_vec:norm(e3d_vec:sub(Pos,Center)),
    VsPos = wings_util:add_vpos(Vs,We),
    Data = {Center,Ray,Axis,Pos,VertDegList},
    Tvs = [{Id,{Vs,make_circular_fun(Data,State,VsPos)}}],
    Flags = [{mode,{circ_mode(),State}}],
    wings_drag:setup(Tvs,[percent|[falloff]],Flags,St).

%%%% Circularise Setup LMB
circle_setup(St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,none,none},
    Tvs = wings_sel:fold(fun(Edges,We,Acc) ->
      Groups = wings_edge_loop:edge_loop_vertices(Edges, We),
      case Groups =/= none of
        true ->
          TotalVs = length(wings_edge:to_vertices(Edges,We)),
          SumCheck = [length(SubGroup) || SubGroup <- Groups],
          Sum = lists:sum(SumCheck),
          case TotalVs == Sum of
            true -> circle_setup_1(Groups,We,State,Acc);
            false -> circ_sel_error_1()
          end;
        false ->
          circ_sel_error_2()
      end
    end,[],St),
    Flags = [{mode,{circ_mode(),State}}],
    wings_drag:setup(Tvs,[percent|[falloff]],Flags,St).

circle_setup_1([],_,_,Acc) ->
    Acc;
circle_setup_1([Vs|Groups],#we{vp=Vtab,id=Id}=We,State,Acc) ->
    Center = wings_vertex:center(Vs,We),
    Total = length(Vs),
    Deg = 360.0/Total,
    Axis = e3d_vec:norm(wings_face:face_normal_ccw(Vs, Vtab)),
    {MinRadius,Pos,Index} = get_radius(Vs, Center, Axis, Vtab, 0.0, minradius, lastpos, firstpos, 0.0, index),
    VertDegList = degrees_from_static_ray(Vs,Deg,Index,1.0,[]),
    Ray = e3d_vec:norm(e3d_vec:sub(Pos,Center)),
    MinRadiusPos = e3d_vec:add(Center,e3d_vec:mul(Ray,MinRadius)),
    VsPos = wings_util:add_vpos(Vs,We),
    Data = {Center,Ray,Axis,MinRadiusPos,VertDegList},
    circle_setup_1(Groups,We,State,[{Id,{Vs,make_circular_fun(Data,State,VsPos)}}|Acc]).

%%%% Return the Index and position of the Point chosen to represent the stable
%%%% ray from the chosen Center point on the chosen Plane.
get_radius_rmb([], RayPos, Center, Plane, _, Pos,LastPos,FirstPos,AtIndex,Index) ->
    HalfPos = e3d_vec:average(LastPos,FirstPos),
    case HalfPos == RayPos of
      true ->
        PosOnPlane = intersect_vec_plane(HalfPos,Center,Plane),
        {PosOnPlane,AtIndex+0.5};
      false ->
        {Pos,Index}
    end;

get_radius_rmb([Vert|Vs], RayPos, Center, Plane, Vtab, Pos0, lastpos, firstpos, 0.0, Index) ->
    Pos = gb_trees:get(Vert,Vtab),
    case Pos == RayPos of
      true  ->
        PosOnPlane = intersect_vec_plane(Pos,Center,Plane),
        get_radius_rmb(Vs, RayPos, Center, Plane, Vtab, PosOnPlane, Pos, Pos, 1.0, 1.0);
      false ->
        get_radius_rmb(Vs, RayPos, Center, Plane, Vtab, Pos0, Pos, Pos, 1.0, Index)
    end;

get_radius_rmb([Vert|Vs], RayPos, Center, Plane, Vtab, Pos0, LastPos, FirstPos, AtIndex, Index) ->
    Pos = gb_trees:get(Vert,Vtab),
    case Pos == RayPos of
      true  ->
        PosOnPlane = intersect_vec_plane(Pos,Center,Plane),
        get_radius_rmb(Vs, RayPos, Center, Plane, Vtab, PosOnPlane, Pos, FirstPos, AtIndex+1.0, AtIndex+1.0);
      false ->
        HalfPos = e3d_vec:average(Pos,LastPos),
        case HalfPos == RayPos of
          true ->
            PosOnPlane = intersect_vec_plane(HalfPos,Center,Plane),
            get_radius_rmb(Vs, RayPos, Center, Plane, Vtab, PosOnPlane, Pos, FirstPos, AtIndex+1.0, AtIndex+0.5);
        false ->
            get_radius_rmb(Vs, RayPos, Center, Plane, Vtab, Pos0, Pos, FirstPos, AtIndex+1.0, Index)
      end
    end.

%%%% Return the Index and Postion of the Vertex or midpoint between adjacent
%%%% vertices closeest to the Center. Distance calculation is made after the
%%%% point in question is flattened to the relevant Plane.
get_radius([],Center,_,_,MinDist,Pos,LastPos,FirstPos,AtIndex,Index) ->
    HalfPos = e3d_vec:average(LastPos,FirstPos),
    DistHalf = abs(e3d_vec:dist(HalfPos,Center)),
    case DistHalf < MinDist of
      true -> {DistHalf, HalfPos, AtIndex+0.5};
      false -> {MinDist, Pos, Index}
    end;

get_radius([Vert|Vs], Center, Plane, Vtab, 0.0, _Pos, _LastPos, _FirstPos, AtIndex, _Index) ->
    Pos = gb_trees:get(Vert,Vtab),
    PosOnPlane = intersect_vec_plane(Pos,Center,Plane),
    Dist = abs(e3d_vec:dist(PosOnPlane,Center)),
    get_radius(Vs, Center, Plane, Vtab, Dist, PosOnPlane, Pos, Pos, AtIndex+1.0, AtIndex+1.0);

get_radius([Vert|Vs], Center, Plane, Vtab, MinDist, Pos0, LastPos, FirstPos, AtIndex, Index) ->
    Pos = gb_trees:get(Vert,Vtab),
    PosOnPlane = intersect_vec_plane(Pos,Center,Plane),
    HalfPos = e3d_vec:average(PosOnPlane,LastPos),
    DistFull = abs(e3d_vec:dist(PosOnPlane,Center)),
    DistHalf = abs(e3d_vec:dist(HalfPos,Center)),
    case DistFull < DistHalf of
      true ->
        case ((DistFull < MinDist) andalso (DistFull > 0.0)) of
          true  -> get_radius(Vs, Center, Plane, Vtab, DistFull, PosOnPlane, PosOnPlane, FirstPos, AtIndex+1.0, AtIndex+1.0);
          false -> get_radius(Vs, Center, Plane, Vtab, MinDist, Pos0, PosOnPlane, FirstPos, AtIndex+1.0, Index)
        end;
      false ->
        case ((DistHalf < MinDist) andalso (DistHalf > 0.0)) of
          true  -> get_radius(Vs, Center, Plane, Vtab, DistHalf, HalfPos, PosOnPlane, FirstPos, AtIndex+1.0, AtIndex+0.5);
          false -> get_radius(Vs, Center, Plane, Vtab, MinDist, Pos0, PosOnPlane, FirstPos, AtIndex+1.0, Index)
        end
    end.

%%%% Return a tuple list [{Vert, Degrees}] of all the vertices
%%%% in the edge loop in ccw order and the number of degrees it
%%%% will be rotated around the center point from the stable ray.
degrees_from_static_ray([],_,_,_,DegList) ->
    DegList;
degrees_from_static_ray([Vert|Vs],Deg,Num,At,DegList) ->
    Degrees = Deg * (At-Num),
    degrees_from_static_ray(Vs, Deg, Num, At+1.0, [{Vert, Degrees}|DegList]).

%%%% Arc Modes
arc_modes() ->
    fun(help, State) -> arc_mode_help(State);
      ({key,$1},{true,_normal,_angle})       -> {false,_normal,_angle};
      ({key,$1},{false,_normal,_angle})      -> {true,_normal,_angle};
      ({key,$2},{_flatten,normal,_angle})  -> {_flatten,reverse,_angle};
      ({key,$2},{_flatten,reverse,_angle}) -> {_flatten,normal,_angle};
      ({key,$3},{_flatten,_normal,acute})    -> {_flatten,_normal,obtuse};
      ({key,$3},{_flatten,_normal,obtuse})   -> {_flatten,_normal,acute};
      (done,{Flatten,_,_}) -> wings_pref:set_value(circularise_flatten, Flatten);
      (_,_) -> none
    end.

%%%% Mode Help
arc_mode_help({Flatten,Normal,Angle}) ->
    [flatten_help(Flatten),
     norm_help(Normal),
     angle_help(Angle)].

flatten_help(true)  -> ?__(1,"[1] Don't Flatten");
flatten_help(false) -> ?__(2,"[1] Flatten").

norm_help(normal)   -> ?__(1,"  [2] Reverse Arc Normal");
norm_help(reverse)  -> ?__(2,"  [2] Original Arc Normal");
norm_help(none)     -> [].

angle_help(acute)   -> ?__(1,"  [3] Use Obtuse Angle");
angle_help(obtuse)  -> ?__(2,"  [3] Use Acute Angle");
angle_help(none)    -> [].

%%%% Circularise Modes
circ_mode() ->
    fun
      (help, State) -> circ_mode_help(State);
      ({key,$1},{true,_normal,none})     -> {false,_normal,none};
      ({key,$1},{false,_normal,none})    -> {true,_normal,none};
      ({key,$2},{_flatten,normal,none})  -> {_flatten,reverse,none};
      ({key,$2},{_flatten,reverse,none}) -> {_flatten,normal,none};
      (done,{Flatten,_,_}) -> wings_pref:set_value(circularise_flatten, Flatten);
      (_,_) -> none
    end.

circ_mode_help({Flatten,Normal,_}) ->
    [?__(1,"[+] or [-] Adjust Diameter  "),
     flatten_help(Flatten),
     circ_norm_help(Normal)].

circ_norm_help(normal)  -> ?__(1,"  [2] Reverse Plane Normal");
circ_norm_help(reverse) -> ?__(2,"  [2] Original Plane Normal");
circ_norm_help(none)  -> [].

%%%% Arc drag data
make_arc_fun(Data,State,VsPos) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_fun(Data,NewState,VsPos);
      ([Angle, _, Percent|_], A) ->
        make_arc_2(Data, State, VsPos, Percent, Angle, A)
    end.

make_arc_2(Data,State,VsPos,Percent,Angle,A) ->
    lists:foldl(fun({Vertex,Vpos}, VsAcc) ->
      [{Vertex,arc(Vertex,Vpos,Data,State,Percent,Angle)}|VsAcc]
    end,A,VsPos).

%%%% Arc Center drag data
make_arc_center_fun(Data,State,VsPos) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_center_fun(Data,NewState,VsPos);
      ([Percent|_], A) ->
        make_arc_center_2(Data,State,VsPos,Percent,A)
    end.

make_arc_center_2(Data,State,VsPos,Percent,A) ->
    lists:foldl(fun({Vertex,Vpos}, VsAcc) ->
      [{Vertex,arc_center(Vertex,Vpos,Data,State,Percent)}|VsAcc]
    end,A,VsPos).

%%%% Circularise Mode, Diameter, and Percentage changes
make_circular_fun(Data,State,VsPos) ->
    fun
      (new_falloff,Diameter) ->
          {Center,Ray,Axis,_Pos,VertDegList} = Data,
          NewPos = e3d_vec:add(Center,e3d_vec:mul(Ray,Diameter/2.0)),
          make_circular_fun({Center,Ray,Axis,NewPos,VertDegList},State,VsPos);
      (new_mode_data,{NewState,_}) ->
          {_,NewNormal,_} = NewState,
          {_,Normal,_} = State,
          case Normal == NewNormal of
            true ->
              make_circular_fun(Data,NewState,VsPos);
            false ->
              {Center,Ray,Axis0,Pos,VertDegList} = Data,
              Axis = e3d_vec:neg(Axis0),
              make_circular_fun({Center,Ray,Axis,Pos,VertDegList},NewState,VsPos)
          end;
      ([Percent|_], A) ->
          {Center,_Ray,Axis,Pos,VertDegList} = Data,
          make_circular_2(Center,Axis,Pos,VertDegList,VsPos,State,Percent,A)
    end.

%%%% Find the number of Degrees the Vertex is to be rotated in the VertDegList
make_circular_2(Center,Axis,Pos,VertDegList,VsPos,{Flatten,_,_},Percent,A) ->
    lists:foldl(fun({Vertex,Vpos}, VsAcc) ->
      {_,{_,Degrees}} = lists:keysearch(Vertex, 1, VertDegList),
      [{Vertex,make_circular(Center,Axis,Pos,Degrees,Vpos,Flatten,Percent)}|VsAcc]
    end, A, VsPos).

%%%% Arc Main Functions
arc(_Vertex,Vpos,_Data,_State,0.0,0.0) ->
    Vpos;

arc(Vertex, Vpos, {_, Opp, _, VPos1, Hinge, DegVertList, NumVs}, {true,_,_}, Percent, 0.0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Segment = (Opp * 2) / NumVs,
    Norm = e3d_vec:norm(e3d_vec:sub(Hinge, VPos1)),
    NewPos = e3d_vec:add(VPos1, e3d_vec:mul(Norm, Segment * Index)),
    NewNorm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos, Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(NewNorm, Percent * Factor));

arc(Vertex, Vpos, {_, Opp, Plane, VPos1, Hinge, DegVertList, NumVs}, {false,_,_}, Percent, 0.0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Segment = (Opp * 2) / NumVs,
    Norm = e3d_vec:norm(e3d_vec:sub(Hinge, VPos1)),
    NewPos0 = e3d_vec:add(VPos1, e3d_vec:mul(Norm, Segment * Index)),
    NewPos = intersect_vec_plane(NewPos0, Vpos, Plane),
    NewNorm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos, Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(NewNorm, Percent * Factor));

arc(Vertex, Vpos, {_, _, Plane, VPos1, Hinge, DegVertList, NumVs}, {Flatten,_,_}, Percent, 180.0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = 180.0 / NumVs,
    RotationAmount = Deg * Index,
    make_circular(Hinge, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent);

arc(Vertex, Vpos, {Cross, Opp, Plane, VPos1, Hinge, DegVertList, NumVs}, {Flatten,_,_}, Percent, Angle0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Angle =  90.0 - (Angle0/2.0),
    Deg = (180.0 - (Angle * 2)) / NumVs,
    Radians = (math:pi()/(180.0/Angle)),
    %% Erlang trigonomic inputs have to be converted from Degrees to Radians
    Adj = math:tan(Radians) * Opp,
    RotationAmount = Deg * Index,
    RotPoint = e3d_vec:add(Hinge, e3d_vec:mul(Cross, Adj)),
    make_circular(RotPoint, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent).

    % % % % % % % % % % % % % % % % % %
    %                                 %
    %                 .               %
    %          *             *        %
    %      *_ _ _ _ _ _ _ _ _ _ _*    %
    %                 |_| OPP   /     %
    %                 |        /      %
    %                 |       /       %
    %               A |      /        %
    %               D |     /         %
    %               J |    /          %
    %                 |   /           %
    %                 |  /            %
    %                 | /             %
    %                 |/  ANGLE       %
    %   ROTATION POINT                %
    %                                 %
    %                                 %
    % % % % % % % % % % % % % % % % % %

arc_center(_,Vpos,_,_,0.0) ->
    Vpos;

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{Flatten,normal,acute}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = Angle / NumVs,
    RotationAmount = -Deg * Index,
    make_circular(CenterPoint, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent);

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{Flatten,normal,obtuse}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = (360 - Angle) / NumVs,
    RotationAmount = Deg * Index,
    make_circular(CenterPoint, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent);

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{Flatten,reverse,acute}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = Angle / NumVs,
    RotationAmount = Deg * Index,
    make_circular(CenterPoint, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent);

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{Flatten,reverse,obtuse}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = (360.0 - Angle) / NumVs,
    RotationAmount = -Deg * Index,
    make_circular(CenterPoint, Plane, VPos1, RotationAmount, Vpos, Flatten, Percent).

%%%% Main Function. Calculate the final position of each vertex (NewPos).
%%%% Measure the distance between NewPos and the Center (Factor). Move the
%%%% vertex towards the NewPos by a distance of the drag Dist * Factor.
make_circular(_Center,_Axis,_Pos,_Deg,Vpos,_Flatten, 0.0) ->
    Vpos;

make_circular(Center, Plane, Pos, Deg, Vpos, true, Percent) ->
    NewPos = rotate(Pos,Plane,Center,Deg),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos,Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor));

make_circular(Center, Plane, Pos, Deg, Vpos, false, Percent) ->
    NewPos0 = rotate(Pos,Plane,Center,Deg),
    NewPos = intersect_vec_plane(NewPos0, Vpos, Plane),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos,Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor)).

%%%% Utilities
rotate(Vpos,Axis,{Cx,Cy,Cz},Angle) ->
    %% return new position as {x,y,z}
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).

intersect_vec_plane(PosA,PosB,_) when PosA == PosB ->
    PosA;
intersect_vec_plane(PosA,PosB,PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    case DotProduct of
      0.0 ->
        PosA;
      _ ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
        e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection))
    end.

%%%% Selection errors
circ_sel_error() ->
    wings_u:error(?__(1,"Selection must either be a single open or closed edge loop")).
circ_sel_error_1() ->
    wings_u:error(?__(1,"Selected edge loops may not share vertices")).
circ_sel_error_2() ->
    wings_u:error(?__(1,"Selection must consist of closed edge loops\nor a single open edge loop")).
