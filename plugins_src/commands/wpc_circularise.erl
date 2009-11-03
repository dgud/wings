%%
%%  wpc_circularise.erl --
%%
%%    Plugin to flatten, equalise, and inflate open or closed edge loops
%%    making them circular.
%%
%%  Copyright (c) 2008-2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
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
      {?__(2,"Flatten, equalise, and inflate selected edge loops making them circular"),
       ?__(4,"Circularise a single open or closed loop using secondary selections"),
       ?__(5,"Choose common plane to which loops will be flattened")},[]}.

circular_arc_options() ->
    fun
      (1,_Ns) -> {edge,circularise};
      (2,_Ns) -> {edge,circularise_center};
      (3,_Ns) -> {edge,{circularise,{'ASK',[plane]}}}
    end.

%%%% Commands
command({edge,circularise},St) ->
    VsData = wings_sel:fold(fun(Edges, We, Acc) ->
        EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
        VsList = wings_edge_loop:edge_links(Edges,We),
        case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
          true ->
            [circle|Acc];
          false ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                [{VsList,We}|Acc];
              not_mixed ->
                circ_sel_error_4();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error_4()
            end
        end
    end, [], St),
    case lists:member(circle,VsData) of
      true ->
        circle_setup(find_plane,St);
      false ->
        arc_setup(find_plane,VsData,St)
    end;

command({edge,{circularise,{'ASK',Ask}}},St) ->
    VsData = wings_sel:fold(fun(Edges, We, Acc) ->
        EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
        VsList = wings_edge_loop:edge_links(Edges,We),
        case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
          true ->
            [circle|Acc];
          false ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                [{VsList,We}|Acc];
              not_mixed ->
                circ_sel_error_4();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error_4()
            end
        end
    end, [], St),
    case lists:member(circle,VsData) of
      true ->
        wings:ask(selection_ask(Ask), St, fun(Plane,St0) ->
        circle_setup(Plane,St0)
        end);
      false ->
        wings:ask(selection_ask(Ask), St, fun(Plane,St0) ->
        arc_setup(Plane,VsData,St0)
        end)
    end;

command({edge,{circularise,Plane}}, St) ->
    VsData = wings_sel:fold(fun(Edges, We, Acc) ->
        EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
        VsList = wings_edge_loop:edge_links(Edges,We),
        case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
          true ->
            [circle|Acc];
          false ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                [{VsList,We}|Acc];
              not_mixed ->
                circ_sel_error_4();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error_4()
            end
        end
    end, [], St),
    case lists:member(circle,VsData) of
      true -> circle_setup(Plane,St);
      false -> arc_setup(Plane,VsData,St)
    end;

%%%%
command({edge, circularise_center}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
    VsList = wings_edge_loop:edge_links(Edges,We),
    case VsList of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            second_ask(Edges,Vs0,We,St);
          _other ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                wings:ask(selection_ask([plane,arc_center]), St, fun({Plane,Center},St0) ->
                arc_center_setup(Plane,Center,Vs,We,St0)
                end);
              not_mixed ->
                circ_sel_error();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error()
            end
        end;
      _ ->
        circ_sel_error()
    end;
command({edge, circularise_center}, _) ->
    circ_sel_error();
command({edge,{circularise_center,{Plane0,Center0}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
    VsList = wings_edge_loop:edge_links(Edges,We),
    case VsList of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            second_ask(Edges,Vs0,We,St);
          _other ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                 arc_center_setup(Plane0,Center0,Vs,We,St);
              not_mixed ->
                circ_sel_error();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error()
            end
        end;
      _ ->
        circ_sel_error()
    end;

command({edge,{circularise_center,_}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
    VsList = wings_edge_loop:edge_links(Edges,We),
    case VsList of
      [Vs] ->
        case wings_edge_loop:edge_loop_vertices(Edges,We) of
          [Vs0] ->
            second_ask(Edges,Vs0,We,St);
          _other ->
            case check_if_partial_and_full_loops_are_mixed(VsList,We) of
              not_mixed when length(EdgeGroups)==length(VsList) ->
                wings:ask(selection_ask([plane,arc_center]), St, fun({Plane,Center},St0) ->
                arc_center_setup(Plane,Center,Vs,We,St0)
                end);
              not_mixed ->
                circ_sel_error();
              single_edge ->
                circ_sel_error_3();
              mixed ->
                circ_sel_error()
            end
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

%%%% Secondary Selection for Circularise Mmb
second_ask(Edges,Vs,We,St) ->
    wings:ask(secondary_sel_ask(Edges,Vs), St, fun({Center,Plane,RayPos},St0) ->
    circle_pick_all_setup(Vs,RayPos,Center,Plane,We,St0)
    end).

secondary_sel_ask(Edges,Vs) ->
    Desc1 = ?__(1,"Select a single edge or vertex from the original edge loop marking the stable ray from the center point"),
    Desc2 = ?__(2,"Pick center point"),
    Desc3 = ?__(3,"Pick plane"),
    Fun = fun
             (check, #st{selmode=vertex}=St) ->
               check_selection(St,Vs);
             (check, #st{selmode=edge}=St) ->
               check_selection(St,Edges);
             (check, St) ->
               check_selection(St,[]);
             (exit, {_,_,#st{selmode=vertex,sel=[{_,Sel}]}=St}) ->
               case check_selection(St,Vs) of
                 {_,[]} ->
                   RayV = gb_sets:smallest(Sel),
                   {result,RayV};
                 {_,_} -> error
               end;
             (exit, {_,_,#st{shapes=Shs,selmode=edge,sel=[{Id,Sel}]}=St}) ->
               case check_selection(St,Edges) of
                 {_,[]} ->
                   #we{es=Etab} = gb_trees:get(Id, Shs),
                   E = gb_sets:smallest(Sel),
                   #edge{vs=Va,ve=Vb} = array:get(E, Etab),
                   {result,{Va,Vb}};
                 {_,_} -> error
               end;
             (exit,_) -> error
           end,
    {[{point,Desc2},{axis,Desc3},{Fun,Desc1}],[],[],[vertex,edge,face]}.

check_selection(#st{sel=[]},_Vs) ->
    {none,?__(1,"Nothing selected")};

check_selection(#st{selmode=Mode,sel=[{_,Sel}]}, OrigSel) when Mode==edge; Mode==vertex ->
    case gb_sets:size(Sel) of
      1 ->
        Elem = gb_sets:smallest(Sel),
        case lists:member(Elem,OrigSel) of
          true -> {none,[]};
          false -> {none,?__(2,"Vertex must be from the original edge loop")}
        end;
      _ ->
        {none,?__(2,"Vertex must be from the original edge loop")}
    end;
check_selection(_, _) ->
    {none,?__(4,"Only a single edge or vertex may be selected")}.


% % % % % % % % % % %
%    Data Setup     %
% % % % % % % % % % %

%%%% Arc Setup LMB - find plane for each arc
%%%% Arc Setup RMB - arc to common plane
arc_setup(Plane,VsData,St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,normal,none},
    Tvs = lists:foldl(fun({VertList,We},Acc) ->
            arc_setup(State,Plane,VertList,We,Acc)
            end, [], VsData),
    Flags = [{mode, {arc_modes(),State}},{initial,[0.0,0.0,0.0,1.0]}],
    Units = [angle, skip, skip, percent],
    wings_drag:setup(Tvs, Units, Flags, St).

arc_setup(State,Plane0,[VsList|Loops],#we{id=Id,vp=Vtab}=We,Acc) ->
    {Vs0,Edges} = arc_vs(VsList,[],[]),
    CwNorm = wings_face:face_normal_cw(Vs0,Vtab),
    case e3d_vec:is_zero(CwNorm) of
        true ->
          Plane = check_plane(Vs0,We),
          {[NewVsList],TempVtab} = adjust_vertex_order(Plane0, Edges, Vs0, Plane, We),
          {Vs,_} = arc_vs(NewVsList,[],[]),
          CwNorm1 = wings_face:face_normal_ccw(Vs,TempVtab),
          {Vlist,DegVertList} = make_degree_vert_list(Vs,TempVtab,0,[],[]),
          Norm = CwNorm1;
        false when Plane0==find_plane ->
          Vs = Vs0,
          {Vlist,DegVertList} = make_degree_vert_list(Vs0,Vtab,0,[],[]),
          Norm = e3d_vec:neg(CwNorm);
        false ->
          Plane = check_for_user_plane(Plane0, CwNorm),
          Vs = check_vertex_order(Vs0,Plane,CwNorm),
          {Vlist,DegVertList} = make_degree_vert_list(Vs,Vtab,0,[],[]),
          [V0|_] = Vs,
          V1 = array:get(V0,Vtab),
          V2 = array:get(lists:last(Vs),Vtab),
          Vec = e3d_vec:norm_sub(V1,V2),
          Cr1 = e3d_vec:norm(e3d_vec:cross(Vec,Plane)),
          Cr2 = e3d_vec:neg(Cr1),
          Norm = if Cr1 > Cr2 -> e3d_vec:neg(Plane);
                    true -> Plane
                 end
    end,
    NumVs = length(DegVertList) + 1,
    [StartVs|_] = Vs,
    EndVs = lists:last(Vs),
    SPos = array:get(StartVs, Vtab),
    EPos = array:get(EndVs, Vtab),
    Hinge = e3d_vec:average(SPos, EPos),
    Chord = e3d_vec:sub(Hinge, SPos),
    Cross = e3d_vec:norm(e3d_vec:cross(Norm,Chord)),
    Opp = e3d_vec:len(Chord),
    Data = {{CwNorm, Cross, Opp, Norm, SPos, Hinge, NumVs}, DegVertList},
    arc_setup(State,Plane0,Loops,We,[{Id,{Vlist, make_arc_fun(Data,State)}}|Acc]);
arc_setup(_,_,[],_,Acc) -> Acc.

%%%% Arc Setup MMB
arc_center_setup(Plane,Center,VsList,#we{id=Id,vp=Vtab},St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    {Vs0,_} = arc_vs(VsList,[],[]),
    Vs = check_vertex_order(Vs0,Plane,wings_face:face_normal_cw(Vs0,Vtab)),
    {Vlist,DegVertList} = make_degree_vert_list(Vs,Vtab,0,[],[]),
    NumVs = length(DegVertList) + 1,
    [StartVs|_] = Vs,
    EndVs = lists:last(Vs),
    SPos = array:get(StartVs, Vtab),
    EPos = array:get(EndVs, Vtab),
    Hinge = e3d_vec:average(SPos, EPos),
    Chord = e3d_vec:sub(Hinge, SPos),
    Cross = e3d_vec:cross(Chord,Plane),
    CenterPoint = intersect_vec_plane(Hinge,Center,Cross),
    %% get angle
    Vec1 = e3d_vec:sub(CenterPoint,SPos),
    Vec2 = e3d_vec:sub(CenterPoint,EPos),
    Angle = e3d_vec:degrees(Vec1,Vec2),

    {ObtuseAcute,Axis} = axis_orientation(Plane,Cross,Vec1),
    State = {Flatten,normal,ObtuseAcute},

    Data = {Angle, CenterPoint, Axis, SPos, DegVertList, NumVs},
    Tvs = [{Id,{Vlist,make_arc_center_fun(Data,State)}}],
    Flags = [{mode,{arc_modes(),State}},{initial,[1.0]}],
    wings_drag:setup(Tvs, [percent], Flags, St).

%% StartVs and EndVs are in 3rd of First and 2nd of Last
arc_vs([{E,LastV,V}|[]], VAcc, EAcc) ->
    {[LastV,V|VAcc], [E|EAcc]};
arc_vs([{E,_,V}|VsList], VAcc, EAcc) ->
    arc_vs(VsList, [V|VAcc], [E|EAcc]).

%%%% Index vertices for open edge loop (Arc)
%% The first and last vertices in the list don't move, so we skip them.
make_degree_vert_list([_|[]],_,_,Vlist,DegVertList) ->
    {Vlist, DegVertList};
make_degree_vert_list([_|Vs], Vtab, 0, Vlist, DegVertList) ->
    make_degree_vert_list(Vs, Vtab, 1, Vlist, DegVertList);
make_degree_vert_list([V|Vs], Vtab, Index, Vlist, DegVertList) ->
    Vpos = array:get(V,Vtab),
    make_degree_vert_list(Vs, Vtab, Index+1, [V|Vlist], [{V,{Vpos, Index}}|DegVertList]).

check_if_partial_and_full_loops_are_mixed([Group|Vs],We) when length(Group) > 1 ->
    {Edges,Bool} = edges_in_group(Group,[],[],[],false,false),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
      true -> mixed;
      false when Bool -> mixed;
      false -> check_if_partial_and_full_loops_are_mixed(Vs,We)
    end;
check_if_partial_and_full_loops_are_mixed([],_) -> not_mixed;
check_if_partial_and_full_loops_are_mixed(_,_) -> single_edge.

edges_in_group([{Edge,V,V2}|VsList],EAcc,VAcc,V2Acc,false,false) ->
    Bool = lists:member(V,VAcc),
    Bool2 = lists:member(V2,V2Acc),
    edges_in_group(VsList,[Edge|EAcc],[V|VAcc],[V2|V2Acc],Bool,Bool2);
edges_in_group(_,_,_,_,true,_) ->
    {[],true};
edges_in_group(_,_,_,_,_,true) ->
    {[],true};
edges_in_group([],Edges,_,_,_,_) -> {Edges,false}.

%%%% Circularise Setup LMB RMB
circle_setup(Plane,St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    DragMode = wings_pref:get_value(circularise_drag, relative),
    State = {Flatten,none,DragMode},
    Tvs = wings_sel:fold(fun(Edges,We,Acc) ->
      Groups = wings_edge_loop:edge_loop_vertices(Edges, We),
      case Groups =/= none of
        true ->
          TotalVs = length(wings_edge:to_vertices(Edges,We)),
          SumCheck = [length(SubGroup) || SubGroup <- Groups],
          Sum = lists:sum(SumCheck),
          case TotalVs == Sum of
            true -> circle_setup_1(Groups,We,Plane,State,Acc);
            false -> circ_sel_error_1()
          end;
        false ->
          circ_sel_error_4()
      end
    end,[],St),
    Flags = [{mode,{circ_mode(),State}},{initial,[1.0,0.0,0.0,1.0]}],
    wings_drag:setup(Tvs,circularise_units(State),Flags,St).

%%%% Circularise Setup MMB
circle_pick_all_setup(Vs0,RayV,Center,Axis0,#we{vp=Vtab,id=Id},St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    DragMode = wings_pref:get_value(circularise_drag, relative),
    State = {Flatten,normal,DragMode},
    Axis = e3d_vec:norm(Axis0),
    Vs = check_vertex_order(Vs0,Axis,wings_face:face_normal_cw(Vs0,Vtab)),
    Deg = (360.0/length(Vs)),
    {Pos,Index} = find_stable_point(Vs,RayV,Vtab,0.0),
    Ray0 = e3d_vec:sub(intersect_vec_plane(Pos,Center,Axis),Center),
    Len = e3d_vec:len(Ray0),
    Ray = e3d_vec:norm(Ray0),
    VertDegList = degrees_from_static_ray(Vs,Vtab,Deg,Index,1,[]),
    Data = {Center,Ray,Len,Axis,VertDegList},
    Tvs = [{Id,{Vs,make_circular_fun(Data,State)}}],
    Flags = [{mode,{circ_mode(),State}},{initial,[1.0,0.0,0.0,1.0]}],
    wings_drag:setup(Tvs,circularise_units(State),Flags,St).

circle_setup_1([],_,_,_,Acc) -> Acc;
circle_setup_1([Vs0|Groups],#we{vp=Vtab,id=Id}=We,Plane,State,Acc) ->
    CwNorm = wings_face:face_normal_cw(Vs0,Vtab),
    Axis = circle_plane(Plane,CwNorm),
    Vs = check_vertex_order(Vs0,Axis,CwNorm),
    Center = wings_vertex:center(Vs,We),
    Deg = 360.0/length(Vs),
    {Pos, NearestVpos, Index} = get_radius(Vs, Center, Axis, Vtab, 0.0, 0.0, raypos, lastpos, firstpos, 0.0, index),
    VertDegList = degrees_from_static_ray(Vs,Vtab,Deg,Index,1.0,[]),
    Ray = e3d_vec:norm_sub(Pos, Center),
    Data = {Center, Ray, NearestVpos, Axis, VertDegList},
    circle_setup_1(Groups,We,Plane,State,[{Id,{Vs,make_circular_fun(Data,State)}}|Acc]).

%% Check whether the UserAxis is opposite to the cw normal of the vert list and
%% if so, reverse the vertex list. This check reduces the probablility of the
%% user having to use the Reverse Normal option.
adjust_vertex_order(find_plane,Edges, [_,V2|_],Norm,#we{vp=Vtab}=We) ->
    V2pos = array:get(V2,Vtab),
    TempV2pos = e3d_vec:add(V2pos,Norm),
    TempVtab = array:set(V2,TempV2pos,Vtab),
    We1 = We#we{vp=TempVtab},
    {wings_edge_loop:edge_links(Edges,We1),TempVtab};
adjust_vertex_order(Plane,Edges, [_,V2|_]=Vs,Norm,#we{vp=Vtab}=We) ->
    LastV = lists:last(Vs),
    V2pos = array:get(V2,Vtab),
    Lpos = array:get(LastV,Vtab),
    Chord = e3d_vec:norm_sub(V2pos,Lpos),

    Cr1 = e3d_vec:cross(Plane,Chord),
    Cr2 = e3d_vec:neg(Cr1),
    D1 = e3d_vec:dot(Cr1,Norm),
    D2 = e3d_vec:dot(Cr2,Norm),
    Vec = if D1+D2==0.0 ->
                 if Cr1 > Cr2 -> Cr1;
                    true -> Cr2
                 end;
             D1 > D2 -> Cr1;
             D1 < D2 -> Cr2
          end,
    TempV2pos = e3d_vec:add(V2pos,Vec),
    TempVtab = array:set(V2,TempV2pos,Vtab),
    We1 = We#we{vp=TempVtab},
    {wings_edge_loop:edge_links(Edges,We1),TempVtab}.

check_vertex_order(Vs,Axis1,Axis2) ->
    Dot = e3d_vec:dot(Axis1,Axis2),
    if Dot < 0.0 -> Vs;
       true -> lists:reverse(Vs)
    end.

axis_orientation(Plane,Vec,Chord) ->
    Dot = e3d_vec:dot(Vec,Chord),
    if Dot < 0.0 -> {acute,Plane};
       true -> {obtuse,e3d_vec:neg(Plane)}
    end.

%% Differenciate between Lmb and Rmb Arc commands
check_plane(Vs,We) ->
    Normals = normals_for_surrounding_faces(Vs,We,[]),
    e3d_vec:average(Normals).

check_for_user_plane(find_plane,CwNorm) -> CwNorm;
check_for_user_plane(Plane,_) -> Plane.

normals_for_surrounding_faces([V|Vs],We,Acc) ->
    Normal = wings_vertex:normal(V,We),
    normals_for_surrounding_faces(Vs,We,[Normal|Acc]);
normals_for_surrounding_faces([],_,Acc) -> Acc.

circle_plane(find_plane,CwNorm) ->
    e3d_vec:neg(CwNorm);
circle_plane(Plane,_) -> Plane.

%%%% Return the Pos and Index of the stable point chosen by the user
find_stable_point([Va|_], {Va,Vb}, Vtab, Index) ->
    VposA = array:get(Va,Vtab),
    VposB = array:get(Vb,Vtab),
    Pos = e3d_vec:average(VposA,VposB),
    {Pos,Index+1.5};
find_stable_point([Vb|_], {Va,Vb}, Vtab, Index) ->
    VposA = array:get(Va,Vtab),
    VposB = array:get(Vb,Vtab),
    Pos = e3d_vec:average(VposA,VposB),
    {Pos,Index+1.5};
find_stable_point([_|Vs], {Va,Vb}, Vtab, Index) ->
    find_stable_point(Vs, {Va,Vb}, Vtab, Index+1);

find_stable_point([RayV|_], RayV, Vtab, Index) ->
    Pos = array:get(RayV,Vtab),
    {Pos,Index+1};
find_stable_point([_|Vs], RayV, Vtab, Index) ->
    find_stable_point(Vs, RayV, Vtab, Index+1).


%%%% Return the Index and Postion of the Vertex or midpoint between adjacent
%%%% vertices closeest to the Center. Distance calculation is made after the
%%%% point in question is flattened to the relevant Plane.
get_radius([],Center,_,_,RayLen0,NearestVert,Pos,LastPos,FirstPos,AtIndex,Index) ->
    HalfPos = e3d_vec:average(LastPos,FirstPos),
    HalfDist = len_sqrt(e3d_vec:sub(HalfPos,Center)),
    case HalfDist < RayLen0 of
      true -> {HalfPos, math:sqrt(NearestVert), AtIndex+0.5};
      false -> {Pos, math:sqrt(NearestVert), Index}
    end;

get_radius([Vert|Vs], Center, Plane, Vtab, 0.0, 0.0, _Pos, _LastPos, _FirstPos, AtIndex, _Index) ->
    Pos = array:get(Vert,Vtab),
    RayPos = intersect_vec_plane(Pos,Center,Plane),
    Dist = len_sqrt(e3d_vec:sub(RayPos,Center)),
    get_radius(Vs, Center, Plane, Vtab, Dist, Dist, RayPos, Pos, Pos, AtIndex+1.0, AtIndex+1.0);

get_radius([Vert|Vs], Center, Plane, Vtab, RayLen0, NearestVert0, RayPos0, LastPos0, FirstPos, AtIndex0, Index0) ->
    Pos = array:get(Vert,Vtab),
    LastPos = intersect_vec_plane(Pos,Center,Plane),
    HalfPos = e3d_vec:average(LastPos,LastPos0),
    FullDist = len_sqrt(e3d_vec:sub(LastPos,Center)),
    HalfDist = len_sqrt(e3d_vec:sub(HalfPos,Center)),
    AtIndex = AtIndex0+1.0,
    case FullDist < HalfDist of
      true ->
        case ((FullDist < RayLen0) andalso (FullDist > 0.0)) of
          true ->
            RayLen = FullDist,
            NearestVert = FullDist,
            RayPos = LastPos,
            Index = AtIndex;
          false ->
            RayLen = RayLen0,
            NearestVert = NearestVert0,
            RayPos = RayPos0,
            Index = Index0
        end;
      false ->
        case ((HalfDist < RayLen0) andalso (HalfDist > 0.0)) of
          true ->
            RayLen = HalfDist,
            NearestVert = case FullDist < NearestVert0 of
              true -> FullDist;
              false -> NearestVert0
            end,
            RayPos = HalfPos,
            Index = AtIndex0+0.5;
          false ->
            RayLen = RayLen0,
            NearestVert = case FullDist < NearestVert0 of
              true -> FullDist;
              false -> NearestVert0
            end,
            RayPos = RayPos0,
            Index = Index0
        end
    end,
    get_radius(Vs, Center, Plane, Vtab, RayLen, NearestVert, RayPos, LastPos, FirstPos, AtIndex, Index).

len_sqrt({X,Y,Z}) ->
    X*X+Y*Y+Z*Z.

%%%% Return a tuple list [{Vert, Degrees}] of all the vertices
%%%% in the edge loop in ccw order and the number of degrees it
%%%% will be rotated around the center point from the stable ray.
degrees_from_static_ray([],_,_,_,_,DegList) ->
    DegList;
degrees_from_static_ray([Vert|Vs],Vtab,Deg,Index,At,DegList) ->
    Degrees = Deg * (At-Index),
    Vpos = array:get(Vert,Vtab),
    degrees_from_static_ray(Vs, Vtab, Deg, Index, At+1.0, [{Vert,{Vpos, Degrees}}|DegList]).

circularise_units({_,_,relative}) ->
    [diametric_factor,skip,skip,percent];
circularise_units({_,_,absolute}) ->
    [absolute_diameter,skip,skip,percent].

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
      ({key,$1},{true,_normal,_dragmode})         -> {false,_normal,_dragmode};
      ({key,$1},{false,_normal,_dragmode})        -> {true,_normal,_dragmode};
      ({key,$2},{_flatten,normal,_dragmode})      -> {_flatten,reverse,_dragmode};
      ({key,$2},{_flatten,reverse,_dragmode})     -> {_flatten,normal,_dragmode};
      ({key,$2},{_flatten,none,_dragmode})     -> {_flatten,none,_dragmode};
      ({key,$3},{_flatten,_normal,relative}) -> {_flatten,_normal,absolute};
      ({key,$3},{_flatten,_normal,absolute}) -> {_flatten,_normal,relative};
      (units,State) -> circularise_units(State);
      (done,{Flatten,_,DragMode}) ->
          wings_pref:set_value(circularise_flatten, Flatten),
          wings_pref:set_value(circularise_drag, DragMode);
      (_,_) -> none
    end.

circ_mode_help({Flatten,Normal,DragMode}) ->
    [flatten_help(Flatten),
     circ_norm_help(Normal),
     radius_help(DragMode)].

circ_norm_help(normal)  -> ?__(1,"  [2] Reverse Plane Normal");
circ_norm_help(reverse) -> ?__(2,"  [2] Original Plane Normal");
circ_norm_help(none)  -> [].

radius_help(relative) -> ?__(1,"  [3] Use Absolute Diameter");
radius_help(absolute) -> ?__(2,"  [3] Use Relative Diameter").

%%%% Arc drag data LMB/RMB
make_arc_fun(Data0,State) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_fun(Data0, NewState);
      ([Angle, _, _, Percent|_], A) ->
        {Data,VertDistList} = Data0,
        lists:foldl(fun({V,{Vpos,Index}}, VsAcc) ->
          [{V, arc(Vpos,Index,Data,State,Percent,Angle)}|VsAcc]
        end, A, VertDistList)
    end.

%%%% Arc Center drag data MMB
make_arc_center_fun(Data,State) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_center_fun(Data,NewState);
      ([Percent|_], A) ->
        {Angle, Center, Plane, Pos, DegVertList, NumVs} = Data,
        lists:foldl(fun({V, {Vpos, Index}}, VsAcc) ->
          [{V,arc_center(Vpos,Angle,Index,NumVs,Pos,Center,Plane,State,Percent)}|VsAcc]
        end, A, DegVertList)
    end.

%%%% Circularise Mode, Diameter, and Percentage changes LMB/RMB
make_circular_fun(Data,State) ->
    fun
      (new_mode_data,{NewState,_}) ->
          {_,NewNormal,_} = NewState,
          {_,Normal,_} = State,
          case Normal == NewNormal of
            true ->
              make_circular_fun(Data,NewState);
            false ->
              {Center,Ray,Nearest,Axis0,VertDegList} = Data,
              Axis = e3d_vec:neg(Axis0),
              make_circular_fun({Center,Ray,Nearest,Axis,VertDegList},NewState)
          end;
      ([Dia,_,_,Percent|_], A) ->
          {Center,Ray,Nearest,Axis,VertDegList} = Data,
          lists:foldl(fun({V,{Vpos,Degrees}}, VsAcc) ->
            [{V,make_circular(Center,Ray,Nearest,Axis,Degrees,Vpos,State,Percent,Dia)}|VsAcc]
          end, A, VertDegList)
    end.

%%%% Arc Main Functions
arc(Vpos, _Index, _Data, _State, 0.0, 0.0) -> Vpos;
arc(Vpos, Index, {CwNorm, _, Opp, Plane0, Pos, Hinge, NumVs},
        {Flatten,Orientation,_}, Percent, 0.0) ->
    Segment = (Opp * 2) / NumVs,
    ChordNorm = e3d_vec:norm(e3d_vec:sub(Hinge, Pos)),
    Plane = reverse_norm(CwNorm,Plane0,Orientation),
    Pos1 = e3d_vec:add(Pos, e3d_vec:mul(ChordNorm, Segment * Index)),
    Pos2 = flatten(Flatten,Pos1,Vpos,Plane),
    Vec = e3d_vec:sub(Pos2, Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Vec, Percent));

arc(Vpos, Index, {CwNorm, _, _, Plane0, Pos, Hinge, NumVs},
        {Flatten,Orientation,_}, Percent, 180.0) ->
    Plane = reverse_norm(CwNorm,Plane0,Orientation),
    Deg = 180.0 / NumVs,
    RotationAmount = Deg * Index,
    Pos1 = rotate(Pos,Plane,Hinge,RotationAmount),
    Pos2 = flatten(Flatten,Pos1,Vpos,Plane),
    Norm = e3d_vec:sub(Pos2,Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent));

arc(Vpos, Index, {CwNorm, Cross0, Opp, Plane0, Pos, Hinge, NumVs},
        {Flatten,Orientation,_}, Percent, Angle) ->
    Plane = reverse_norm(CwNorm,Plane0,Orientation),
    Cross = reverse_norm(CwNorm,Cross0,Orientation),
    {Deg, RotPoint} = angle_and_point(Angle, Opp, Index, NumVs, Hinge, Cross),
    Pos1 = rotate(Pos,Plane,RotPoint,Deg),
    Pos2 = flatten(Flatten,Pos1,Vpos,Plane),
    Norm = e3d_vec:sub(Pos2,Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent)).

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

arc_center(Vpos,_,_,_,_,_,_,_,0.0) -> Vpos;
arc_center(Vpos,Angle,Index,NumVs,Pos,Center,Plane,{Flatten,AxisMode,AngleMode},Percent) ->
    DegIncrement = acute_obtuse(AngleMode, Angle, NumVs),
    RotationAmount = rotation_amount(AxisMode,DegIncrement,Index),
    Pos1 = rotate(Pos,Plane,Center,RotationAmount),
    Pos2 = flatten(Flatten,Pos1,Vpos,Plane),
    Norm = e3d_vec:sub(Pos2,Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent)).

acute_obtuse(acute, Angle, NumVs) -> Angle / NumVs;
acute_obtuse(obtuse, Angle, NumVs) -> - (360 - Angle) / NumVs.

rotation_amount(normal,Deg,Index) -> Deg * Index;
rotation_amount(reverse,Deg,Index) -> -Deg * Index.

%%%% Closed Loop. Calculate the final position of each vertex (NewPos).
%%%% Measure the distance between NewPos and the Center (Factor). Move the
%%%% vertex towards the NewPos by a distance of the drag Dist * Factor.
make_circular(_Center,_Ray,_Nearest,_Axis,_Deg,Vpos,_State, 0.0, 0.0) -> Vpos;
make_circular(Center, Ray, Nearest, Plane, Deg, Vpos, {Flatten,_,Mode}, Percent, Dia) ->
    Pos0 = static_pos(Mode,Center,Ray,Nearest,Dia),
    Pos1 = rotate(Pos0,Plane,Center,Deg),
    Pos2 = flatten(Flatten,Pos1,Vpos,Plane),
    Norm = e3d_vec:sub(Pos2,Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent)).

%%%% Utilities
angle_and_point(Angle0, Opp, Index, NumVs, Hinge, Cross) ->
    Angle = 90.0 - (Angle0/2.0),
    %% Erlang trigonomic inputs have to be converted from Degrees to Radians
    Radians = (math:pi()/(180.0/Angle)),
    Adj = math:tan(Radians) * Opp,
    Deg = (180.0 - (Angle * 2)) / NumVs,
    RotationAmount = Deg * Index,
    RotPoint = e3d_vec:add(Hinge, e3d_vec:mul(Cross, Adj)),
    {RotationAmount, RotPoint}.

static_pos(relative,Center,Ray,Nearest,Dia) ->
    e3d_vec:add(Center, e3d_vec:mul(Ray,Nearest*Dia));
static_pos(absolute,Center,Ray,_Nearest,Dia) ->
    e3d_vec:add(Center, e3d_vec:mul(Ray,Dia/2)).

flatten(true,Pos,_Vpos,_Plane) -> Pos;
flatten(false,Pos,Vpos,Plane) -> intersect_vec_plane(Pos, Vpos, Plane).

rotate(Vpos,Axis,{Cx,Cy,Cz},Angle) ->
    %% return new position as {x,y,z}
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).

intersect_vec_plane(PosA,PosA,_) -> PosA;
intersect_vec_plane(PosA,PosB,PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm),
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

reverse_norm({0.0,0.0,0.0},Norm,reverse) -> e3d_vec:neg(Norm);
reverse_norm(_,Norm,_) -> Norm.

%%%% Selection errors
circ_sel_error() ->
    wings_u:error(?__(2,"Selection must consist of either a single closed or open edge loop")).
circ_sel_error_1() ->
    wings_u:error(?__(1,"Selected edge loops may not share vertices")).
circ_sel_error_3() ->
    wings_u:error(?__(2,"Selections including single edges cannot be processed")).
circ_sel_error_4() ->
    wings_u:error(?__(2,"Selected edge loops must be non-intersecting, and be either all open or all closed.")).
