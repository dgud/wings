%%
%%  wpc_circularise.erl --
%%
%%    Plugin to flatten, equalise, and inflate open or closed edge loops
%%    making them circular.
%%
%%  Copyright (c) 2008-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_circularise).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-import(lists, [member/2]).

init() ->
    true.

%%%% Menu
menu({Mode}, Menu) when Mode =:= edge; Mode =:= {auv,edge} ->
    lists:reverse(parse(Menu, [], Mode, false));
menu(_,Menu) ->
    Menu.

parse([], NewMenu, _, true) ->
    NewMenu;
parse([], NewMenu, Mode, false) ->
    [circular_arc_menu(Mode), separator|NewMenu];
parse([A = {_,loop_cut,_}|Rest], NewMenu, Mode, false) ->
    parse(Rest, [A,circular_arc_menu(Mode)|NewMenu], Mode, true);
parse([Elem|Rest], NewMenu, Mode, Found) ->
    parse(Rest, [Elem|NewMenu], Mode, Found).

circular_arc_menu(edge) ->
    Name = ?__(1,"Circularise"),
    Help = {?__(2,"Flatten, equalise, and inflate selected edge loops making them circular"),
       ?__(6,"Specify using secondary selections"),
       ?__(5,"Choose common plane to which loops will be flattened")},
    F = fun
      (1,_Ns) -> {edge,circularise};
      (2,_Ns) -> {edge,circularise_center};
      (3,_Ns) -> {edge,{circularise,{'ASK',[plane]}}}
    end,
    {Name, {circular, F}, Help, []};
circular_arc_menu({auv,edge}) ->
    {?__(1,"Circularise"),circularise,
     ?__(2,"Flatten, equalise, and inflate selected edge loops making them circular")}.

%%%% Commands
command({Mode,circularise}, St) when Mode =:= edge; Mode =:= {auv,edge} ->
    process_circ_cmd(find_plane, St);
command({edge,{circularise,Plane}}, St) ->
    process_circ_cmd(Plane, St);
command({edge, circularise_center}, St) ->
    process_cc_cmd(none, St);
command({edge,{circularise_center,Data}}, St) ->
    process_cc_cmd(Data, St);
command(_, _) ->
    next.

process_circ_cmd(Plane, St0) ->
    MF = fun(Edges, We) ->
                 case wings_edge_loop:edge_loop_vertices(Edges, We) of
                     [_|_] ->
                         true;
                     none ->
                         Gs = wings_edge_loop:partition_edges(Edges, We),
                         VsList = wings_edge_loop:edge_links(Edges, We),
                         case check_partial_and_full_loops(VsList, We) of
                             not_mixed when length(Gs) =:= length(VsList) ->
                                 false;
                             not_mixed ->
                                 circ_sel_error_4();
                             single_edge ->
                                 circ_sel_error_3();
                             mixed ->
                                 circ_sel_error_4()
                         end
                 end
         end,
    RF = fun erlang:'or'/2,
    IsCircle = wings_sel:dfold(MF, RF, false, St0),
    Setup = case IsCircle of
                true -> fun circle_setup/2;
                false -> fun arc_setup/2
            end,
    case Plane of
        {'ASK',Ask} ->
            wings:ask(selection_ask(Ask), St0, Setup);
        _ ->
            Setup(Plane, St0)
    end.

process_cc_cmd(Data, #st{sel=[_]}=St0) ->
    MF = fun(Edges, #we{id=Id}=We) ->
                 case wings_edge_loop:edge_loop_vertices(Edges, We) of
                     [Vs] ->
                         {Id,gb_sets:to_list(Edges),Vs};
                     _ ->
                         none
                 end
         end,
    RF = fun(Res, none) -> Res end,
    case wings_sel:dfold(MF, RF, none, St0) of
        none ->
            process_cc_cmd_1(Data, St0);
        {_,_,_}=OrigSel ->
            %% Single closed loop, MMB.
            F = fun({Center,Plane,RayPos}, St) ->
                        circle_pick_all_setup(RayPos, Center, Plane, St)
                end,
            wings:ask(secondary_sel_ask(OrigSel), St0, F)
    end;
process_cc_cmd(Data, St) ->
    process_cc_cmd_1(Data, St).

process_cc_cmd_1(Data, St0) ->
    MF = fun(Es, We) ->
                 Edges = gb_sets:to_list(Es),
                 Vs = wings_edge_loop:edge_links(Edges, We),
                 EdgeGroups = wings_edge_loop:partition_edges(Edges, We),
                 case check_partial_and_full_loops(Vs, We) of
                     not_mixed when length(EdgeGroups) =:= length(Vs) ->
                         ok;
                     not_mixed ->
                         circ_sel_error();
                     single_edge ->
                         circ_sel_error_3();
                     mixed ->
                         circ_sel_error()
                 end
         end,
    RF = fun(ok, ok) -> ok end,
    ok = wings_sel:dfold(MF, RF, ok, St0),
    case Data of
      {Plane,Center} ->
            arc_center_setup(Plane, Center, St0);
      _ ->
            wings:ask(selection_ask([plane,arc_center]), St0,
                      fun({Plane,Center}, St) ->
                              arc_center_setup(Plane, Center, St)
                      end)
    end.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks, []),
    {Ask,[],[],[vertex,edge,face]}.

selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Pick plane"),
    selection_ask(Rest, [{axis,Desc}|Ask]);

selection_ask([center|Rest], Ask) ->
    Desc = ?__(2,"Pick center point"),
    selection_ask(Rest, [{point,Desc}|Ask]);

selection_ask([arc_center|Rest], Ask) ->
    Desc = ?__(3,"Pick point from which the center will be calculated relative to chosen plane and ends of edge selection"),
    selection_ask(Rest, [{point,Desc}|Ask]).

secondary_sel_ask(OrigSel) ->
    Desc1 = ?__(1,"Select a single edge or vertex from the original edge loop marking the stable ray from the center point"),
    Desc2 = ?__(2,"Pick center point"),
    Desc3 = ?__(3,"Pick plane"),
    Fun = fun(check, St) ->
                  case check_selection(OrigSel, St) of
                      {_,Point,Msg} ->
                          {Point,Msg};
                      Msg ->
                          {none,Msg}
                  end;
             (exit, {_,_,St}) ->
                  case check_selection(OrigSel, St) of
                      {Result,_,_} ->
                          {result,Result};
                      _ ->
                          error
                  end
           end,
    {[{point,Desc2},{axis,Desc3},{Fun,Desc1}],[],[],[vertex,edge,face]}.

check_selection({OrigId,Edges,Vs}, #st{selmode=Mode}=St) ->
    Sel = case Mode of
              vertex -> Vs;
              edge -> Edges;
              face -> []
          end,
    MF = fun(Items, #we{id=Id}=We) when Id =:= OrigId ->
                 case gb_sets:size(Items) of
                     1 ->
                         [Item] = gb_sets:to_list(Items),
                         case member(Item, Sel) of
                             false ->
                                 [wrong];
                             true ->
                                 [get_point(Mode, Item, We)]
                         end;
                     _ ->
                         [multiple]
                 end;
            (_, _) ->
                 [wrong]
         end,
    RF = fun erlang:'++'/2,
    case wings_sel:dfold(MF, RF, [], St) of
        [] ->
            ?__(1,"Nothing selected");
        [{Result,Point}] ->
            {Result,Point,?__(2,"Point saved as stable ray location.")};
        [wrong] ->
            ?__(3,"Only a vertex or an edge from the original "
                "edge loop may be selected.");
        [_|_] ->
            ?__(4,"Only a single edge or vertex may be selected.")
    end.

get_point(vertex, V, We) ->
    {V,wings_vertex:pos(V, We)};
get_point(edge, E, #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
    Pos = e3d_vec:average(wings_vertex:pos(Va, Vtab),
                          wings_vertex:pos(Vb, Vtab)),
    {{Va,Vb},Pos}.

% % % % % % % % % % %
%    Data Setup     %
% % % % % % % % % % %

%%%% Arc Setup LMB - find plane for each arc
%%%% Arc Setup RMB - arc to common plane
arc_setup(Plane, St0) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,normal,none},
    F = fun(Es, We, TentVec0) ->
                VsList = wings_edge_loop:edge_links(Es, We),
                {TentVec,Tv} = arc_setup(State, Plane, VsList, We, TentVec0),
                {We#we{temp=Tv},TentVec}
        end,
    {St,_} = wings_sel:mapfold(F, tent_vec, St0),
    Flags = [{mode,{arc_modes(),State}},{initial,[0.0,0.0,1.0]}],
    Units = [angle,skip,percent],
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, Units, Flags, St).

arc_setup(State, Plane, VsData, We, TentVec) ->
    arc_setup(State, Plane, VsData, We, TentVec, []).

arc_setup(State, Plane0, [VsList|Loops], #we{vp=Vtab}=We, TentVec0, Acc0) ->
    {Vs0,Edges} = arc_vs(VsList, [], []),
    CwNorm = wings_face:face_normal_cw(Vs0, Vtab),
    case e3d_vec:is_zero(CwNorm) of
        true when Plane0 =:= find_plane -> %% LMB
          SurfaceNorm = check_plane(Vs0, We),
          {[NewVsList], TempVtab} = tent_arc(Edges, Vs0, SurfaceNorm, We),
          {Vs,_} = arc_vs(NewVsList, [], []),
          CwNorm1 = wings_face:face_normal_ccw(Vs, TempVtab),
          {Vlist,DegVertList} = make_degree_vert_list(Vs, TempVtab, 0, [], []),
          TentVec = TentVec0,
          Norm = CwNorm1;
        true -> %% RMB
          TentVec = case TentVec0 of
            tent_vec ->
                SurfaceNorm = check_plane(Vs0, We),
                establish_tent_vec(Plane0, Vs0, SurfaceNorm, We);
            _ -> TentVec0
          end,
          {[NewVsList], TempVtab} = tent_arc(Edges, Vs0, TentVec, We),
          {Vs,_} = arc_vs(NewVsList, [], []),
          CwNorm1 = wings_face:face_normal_ccw(Vs, TempVtab),
          {Vlist,DegVertList} = make_degree_vert_list(Vs, TempVtab, 0, [], []),
          Norm = CwNorm1;
        false when Plane0 =:= find_plane -> %% LMB
          Vs = Vs0,
          {Vlist,DegVertList} = make_degree_vert_list(Vs0, Vtab, 0, [], []),
          TentVec = TentVec0,
          Norm = e3d_vec:neg(CwNorm);
        false -> %% RMB
          Plane = check_for_user_plane(Plane0, CwNorm),
          Vs = check_vertex_order(Vs0, Plane, CwNorm),
          {Vlist,DegVertList} = make_degree_vert_list(Vs, Vtab, 0, [], []),
          [V0|_] = Vs,
          V1 = array:get(V0, Vtab),
          V2 = array:get(lists:last(Vs), Vtab),
          Vec = e3d_vec:norm_sub(V1, V2),
          Cr1 = e3d_vec:norm(e3d_vec:cross(Vec, Plane)),
          Cr2 = e3d_vec:neg(Cr1),
          TentVec = TentVec0,
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
    Cross = e3d_vec:norm(e3d_vec:cross(Norm, Chord)),
    Opp = e3d_vec:len(Chord),
    Data = {{CwNorm, Cross, Opp, Norm, SPos, Hinge, NumVs}, DegVertList},
    Acc = [{Vlist, make_arc_fun(Data, State)}|Acc0],
    arc_setup(State, Plane0, Loops, We, TentVec, Acc);
arc_setup(_, _, [], _, TentVec, Acc) ->
    {TentVec,wings_drag:compose(Acc)}.

%%%% Arc Setup MMB
arc_center_setup(Plane, Center, St0) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    State = {Flatten,normal,acute},
    F = fun(Edges, #we{vp=Vtab}=We) ->
                ArcVs = wings_edge_loop:edge_links(Edges, We),
                {Vlist,Data} = arc_center_setup_1(ArcVs, Plane, Center, Vtab),
                Tv = {Vlist,make_arc_center_fun(Data, State)},
                We#we{temp=Tv}
        end,
    St = wings_sel:map(F, St0),
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    Flags = [{mode,{arc_modes(),State}},{initial,[1.0]}],
    wings_drag:fold(DF, [percent], Flags, St).

arc_center_setup_1(VData, Plane, Center, Vtab) ->
    lists:foldl(fun(ArcVs, {VlistAcc,DataAcc}) ->
        {Vs0,_} = arc_vs(ArcVs, [], []),
        Vs = check_vertex_order(Vs0,Plane,wings_face:face_normal_cw(Vs0,Vtab)),
        {Vlist,DegVertList} = make_degree_vert_list(Vs,Vtab,0,[],[]),
        NumVs = length(DegVertList) + 1,
        [StartV|_] = Vs,
        EndV = lists:last(Vs),
        SPos = array:get(StartV, Vtab),
        EPos = array:get(EndV, Vtab),
        Hinge = e3d_vec:average(SPos, EPos),
        Chord = e3d_vec:sub(Hinge, SPos),
        Cross = e3d_vec:cross(Chord,Plane),
        CenterPoint = intersect_vec_plane(Hinge,Center,e3d_vec:norm(Cross)),
        %% get angle
        Vec1 = e3d_vec:sub(CenterPoint,SPos),
        Vec2 = e3d_vec:sub(CenterPoint,EPos),
        Angle = e3d_vec:degrees(Vec1,Vec2),
        Axis0 = e3d_vec:normal([SPos,EPos,CenterPoint]),
        Axis = case Axis0 =:= e3d_vec:zero() of
            true -> Plane;
            false -> Axis0
        end,
        Data = {Angle, CenterPoint, Axis, SPos, DegVertList, NumVs},
        {Vlist++VlistAcc,[Data|DataAcc]}
    end, {[],[]}, VData).

%% StartVs and EndVs are in 3rd of First and 2nd of Last
arc_vs([{E,LastV,V}|[]], VAcc, EAcc) ->
    {[LastV,V|VAcc], [E|EAcc]};
arc_vs([{E,_,V}|VsList], VAcc, EAcc) ->
    arc_vs(VsList, [V|VAcc], [E|EAcc]).

%%%% Index vertices for open edge loop (Arc)
%% The first and last vertices in the list don't move, so we skip them.
make_degree_vert_list([_|[]], _, _, Vlist, DegVertList) ->
    {Vlist,DegVertList};
make_degree_vert_list([_|Vs], Vtab, 0, Vlist, DegVertList) ->
    make_degree_vert_list(Vs, Vtab, 1, Vlist, DegVertList);
make_degree_vert_list([V|Vs], Vtab, Index, Vlist, DegVertList) ->
    Vpos = array:get(V, Vtab),
    make_degree_vert_list(Vs, Vtab, Index+1, [V|Vlist], [{V,{Vpos, Index}}|DegVertList]).

check_partial_and_full_loops([Group|Vs], We) when length(Group) > 1 ->
    {Edges,Bool} = edges_in_group(Group, [], [], [], false, false),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges, We)) of
      true -> mixed;
      false when Bool -> mixed;
      false -> check_partial_and_full_loops(Vs, We)
    end;
check_partial_and_full_loops([], _) -> not_mixed;
check_partial_and_full_loops(_, _) -> single_edge.

edges_in_group([{Edge,V,V2}|VsList], EAcc, VAcc, V2Acc, false, false) ->
    Bool = lists:member(V, VAcc),
    Bool2 = lists:member(V2, V2Acc),
    edges_in_group(VsList, [Edge|EAcc], [V|VAcc], [V2|V2Acc], Bool, Bool2);
edges_in_group(_, _, _, _, true, _) ->
    {[],true};
edges_in_group(_, _, _, _, _, true) ->
    {[],true};
edges_in_group([], Edges, _, _, _, _) -> {Edges,false}.

%%%% Circularise Setup LMB RMB
circle_setup(Plane, St) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    DragMode = wings_pref:get_value(circularise_drag, relative),
    State = {Flatten,none,DragMode},
    DF = fun(Edges, We) ->
                 case wings_edge_loop:edge_loop_vertices(Edges, We) of
                     none ->
                         circ_sel_error_4();
                     Groups ->
                         TotalVs = length(wings_edge:to_vertices(Edges, We)),
                         SumCheck = [length(SubGroup) || SubGroup <- Groups],
                         Sum = lists:sum(SumCheck),
                         case TotalVs  =:=  Sum of
                             true ->
                                 circle_setup_1(Groups, We, Plane, State, []);
                             false ->
                                 circ_sel_error_1()
                         end
                 end
         end,
    Flags = [{mode,{circ_mode(),State}},{initial,[1.0,0.0,1.0]}],
    wings_drag:fold(DF, circularise_units(State), Flags, St).


%%%% Circularise Setup MMB
circle_pick_all_setup(RayV, Center, Axis0, St0) ->
    Flatten = wings_pref:get_value(circularise_flatten, true),
    DragMode = wings_pref:get_value(circularise_drag, relative),
    State = {Flatten,normal,DragMode},
    Axis = e3d_vec:norm(Axis0),
    MF = fun(Es, We) ->
                 circle_pick_all_setup_1(Es, We, State, RayV, Center, Axis)
         end,
    St = wings_sel:map(MF, St0),
    Flags = [{mode,{circ_mode(),State}},{initial,[1.0,0.0,1.0]}],
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, circularise_units(State), Flags, St).

circle_pick_all_setup_1(Edges, #we{vp=Vtab}=We, State, RayV, Center, Axis) ->
    [Vs0] = wings_edge_loop:edge_loop_vertices(Edges, We),
    Vs = check_vertex_order(Vs0, Axis, wings_face:face_normal_cw(Vs0, Vtab)),
    Deg = (360.0/length(Vs)),
    {Pos,Index} = find_stable_point(Vs, RayV, Vtab, 0.0),
    Ray0 = e3d_vec:sub(intersect_vec_plane(Pos, Center, Axis), Center),
    Len = e3d_vec:len(Ray0),
    Ray = e3d_vec:norm(Ray0),
    VertDegList = degrees_from_static_ray(Vs, Vtab, Deg, Index, 1, []),
    Data = {Center,Ray,Len,Axis,VertDegList},
    Tv = {Vs,make_circular_fun(Data, State)},
    We#we{temp=Tv}.

circle_setup_1([], _, _, _, Acc) ->
    wings_drag:compose(Acc);
circle_setup_1([Vs0|Groups], #we{vp=Vtab}=We, Plane, State, Acc0) ->
    CwNorm = wings_face:face_normal_cw(Vs0, Vtab),
    Axis = circle_plane(Plane, CwNorm),
    Vs = check_vertex_order(Vs0, Axis, CwNorm),
    Center = wings_vertex:center(Vs, We),
    Deg = 360.0/length(Vs),
    {Pos,NearestVpos,Index} = get_radius(Vs, Center, Axis, Vtab, 0.0, 0.0, raypos, lastpos, firstpos, 0.0, index),
    VertDegList = degrees_from_static_ray(Vs, Vtab, Deg, Index, 1.0, []),
    Ray = e3d_vec:norm_sub(Pos, Center),
    Data = {Center,Ray,NearestVpos,Axis,VertDegList},
    Acc = [{Vs,make_circular_fun(Data, State)}|Acc0],
    circle_setup_1(Groups, We, Plane, State, Acc).

%% Tent arc for open edge loops that have a ccw normal of {0,0,0}
tent_arc(Edges, [_,V2|_], Norm, #we{vp=Vtab}=We) ->
    V2pos = array:get(V2, Vtab),
    TempV2pos = e3d_vec:add(V2pos, Norm),
    TempVtab = array:set(V2, TempV2pos, Vtab),
    We1 = We#we{vp=TempVtab},
    {wings_edge_loop:edge_links(Edges, We1), TempVtab}.

%% Tent vec when user plane is in effect and ccw norm is {0,0,0}
establish_tent_vec(Plane, [_,V2|_]=Vs, Norm, #we{vp=Vtab}) ->
    LastV = lists:last(Vs),
    V2pos = array:get(V2, Vtab),
    Lpos = array:get(LastV, Vtab),
    Chord = e3d_vec:norm_sub(V2pos, Lpos),
    Cr1 = e3d_vec:cross(Plane, Chord),
    Cr2 = e3d_vec:neg(Cr1),
    D1 = e3d_vec:dot(Cr1, Norm),
    D2 = e3d_vec:dot(Cr2, Norm),
    TentVec = if
             D1 > D2 -> Cr1;
             true -> Cr2
          end,
    TentVec.

%% Check whether the UserAxis is opposite to the cw normal of the vert list and
%% if so, reverse the vertex list. This check reduces the probablility of the
%% user having to use the Reverse Normal option.
check_vertex_order(Vs, Axis1, Axis2) ->
    Dot = e3d_vec:dot(Axis1, Axis2),
    if Dot < 0.0 -> Vs;
       true -> lists:reverse(Vs)
    end.

%% Differenciate between Lmb and Rmb Arc commands
check_plane(Vs, We) ->
    Normals = normals_for_surrounding_faces(Vs, We, []),
    e3d_vec:average(Normals).

check_for_user_plane(find_plane, CwNorm) -> CwNorm;
check_for_user_plane(Plane, _) -> Plane.

normals_for_surrounding_faces([V|Vs], We, Acc) ->
    Normal = wings_vertex:normal(V, We),
    normals_for_surrounding_faces(Vs, We, [Normal|Acc]);
normals_for_surrounding_faces([], _, Acc) -> Acc.

circle_plane(find_plane, CwNorm) ->
    e3d_vec:neg(CwNorm);
circle_plane(Plane, _) -> Plane.

%%%% Return the Pos and Index of the stable point chosen by the user
find_stable_point([Va|_], {Va,Vb}, Vtab, Index) ->
    VposA = array:get(Va, Vtab),
    VposB = array:get(Vb, Vtab),
    Pos = e3d_vec:average(VposA, VposB),
    {Pos,Index+1.5};
find_stable_point([Vb|_], {Va,Vb}, Vtab, Index) ->
    VposA = array:get(Va, Vtab),
    VposB = array:get(Vb, Vtab),
    Pos = e3d_vec:average(VposA, VposB),
    {Pos,Index+1.5};
find_stable_point([_|Vs], {Va,Vb}, Vtab, Index) ->
    find_stable_point(Vs, {Va,Vb}, Vtab, Index+1);

find_stable_point([RayV|_], RayV, Vtab, Index) ->
    Pos = array:get(RayV, Vtab),
    {Pos,Index+1};
find_stable_point([_|Vs], RayV, Vtab, Index) ->
    find_stable_point(Vs, RayV, Vtab, Index+1).


%%%% Return the Index and Postion of the Vertex or midpoint between adjacent
%%%% vertices closeest to the Center. Distance calculation is made after the
%%%% point in question is flattened to the relevant Plane.
get_radius([], Center, _, _, RayLen0, NearestVert, Pos, LastPos, FirstPos, AtIndex, Index) ->
    HalfPos = e3d_vec:average(LastPos, FirstPos),
    HalfDist = len_sqrt(e3d_vec:sub(HalfPos, Center)),
    case HalfDist < RayLen0 of
      true -> {HalfPos, math:sqrt(NearestVert), AtIndex+0.5};
      false -> {Pos, math:sqrt(NearestVert), Index}
    end;

get_radius([Vert|Vs], Center, Plane, Vtab, 0.0, 0.0, _Pos, _LastPos, _FirstPos, AtIndex, _Index) ->
    Pos = array:get(Vert, Vtab),
    RayPos = intersect_vec_plane(Pos, Center, Plane),
    Dist = len_sqrt(e3d_vec:sub(RayPos, Center)),
    get_radius(Vs, Center, Plane, Vtab, Dist, Dist, RayPos, Pos, Pos, AtIndex+1.0, AtIndex+1.0);

get_radius([Vert|Vs], Center, Plane, Vtab, RayLen0, NearestVert0, RayPos0, LastPos0, FirstPos, AtIndex0, Index0) ->
    Pos = array:get(Vert, Vtab),
    LastPos = intersect_vec_plane(Pos, Center, Plane),
    HalfPos = e3d_vec:average(LastPos, LastPos0),
    FullDist = len_sqrt(e3d_vec:sub(LastPos, Center)),
    HalfDist = len_sqrt(e3d_vec:sub(HalfPos, Center)),
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
degrees_from_static_ray([], _, _, _, _, DegList) ->
    DegList;
degrees_from_static_ray([Vert|Vs], Vtab, Deg, Index, At, DegList) ->
    Degrees = Deg * (At-Index),
    Vpos = array:get(Vert, Vtab),
    degrees_from_static_ray(Vs, Vtab, Deg, Index, At+1.0, [{Vert,{Vpos,Degrees}}|DegList]).

circularise_units({_, _, relative}) ->
    [diametric_factor,skip,percent];
circularise_units({_, _, absolute}) ->
    [absolute_diameter,skip,percent].

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
arc_mode_help({Flatten, Normal, Angle}) ->
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

circ_mode_help({Flatten, Normal, DragMode}) ->
    [flatten_help(Flatten),
     circ_norm_help(Normal),
     radius_help(DragMode)].

circ_norm_help(normal)  -> ?__(1,"  [2] Reverse Plane Normal");
circ_norm_help(reverse) -> ?__(2,"  [2] Original Plane Normal");
circ_norm_help(none)  -> [].

radius_help(relative) -> ?__(1,"  [3] Use Absolute Diameter");
radius_help(absolute) -> ?__(2,"  [3] Use Relative Diameter").

%%%% Arc drag data LMB/RMB
make_arc_fun(Data0, State) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_fun(Data0, NewState);
      ([Angle,_,Percent|_], A) ->
        {Data,VertDistList} = Data0,
        lists:foldl(fun({V,{Vpos,Index}}, VsAcc) ->
          [{V, arc(Vpos, Index, Data, State, Percent, Angle)}|VsAcc]
        end, A, VertDistList)
    end.

%%%% Arc Center drag data MMB
make_arc_center_fun(Data, State) ->
    fun
      (new_mode_data,{NewState,_}) ->
        make_arc_center_fun(Data, NewState);
      ([Percent|_], A) ->
        lists:foldl(fun(D, Acc) ->
            {Angle, Center, Plane, Pos, DegVertList, NumVs} = D,
            lists:foldl(fun({V,{Vpos,Index}}, VsAcc) ->
                [{V,arc_center(Vpos, Angle, Index, NumVs, Pos, Center, Plane, State, Percent)}|VsAcc]
             end, Acc, DegVertList)
         end, A, Data)
    end.

%%%% Circularise Mode, Diameter, and Percentage changes LMB/RMB
make_circular_fun(Data, State) ->
    fun
      (new_mode_data,{NewState,_}) ->
          {_,NewNormal,_} = NewState,
          {_,Normal,_} = State,
          case Normal  =:=  NewNormal of
            true ->
              make_circular_fun(Data, NewState);
            false ->
              {Center,Ray,Nearest,Axis0,VertDegList} = Data,
              Axis = e3d_vec:neg(Axis0),
              make_circular_fun({Center,Ray,Nearest,Axis,VertDegList}, NewState)
          end;
      ([Dia,_,Percent|_], A) ->
          {Center,Ray,Nearest,Axis,VertDegList} = Data,
          lists:foldl(fun({V,{Vpos,Degrees}}, VsAcc) ->
            [{V,make_circular(Center, Ray, Nearest, Axis, Degrees, Vpos, State, Percent, Dia)}|VsAcc]
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
    Pos2 = flatten(Flatten, Pos1, Vpos, Plane),
    Vec = e3d_vec:sub(Pos2, Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Vec, Percent));

arc(Vpos, Index, {CwNorm, _, _, Plane0, Pos, Hinge, NumVs},
        {Flatten,Orientation,_}, Percent, 180.0) ->
    Plane = reverse_norm(CwNorm, Plane0, Orientation),
    Deg = 180.0 / NumVs,
    RotationAmount = Deg * Index,
    Pos1 = rotate(Pos, Plane, Hinge, RotationAmount),
    Pos2 = flatten(Flatten, Pos1, Vpos, Plane),
    Norm = e3d_vec:sub(Pos2, Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent));

arc(Vpos, Index, {CwNorm, Cross0, Opp, Plane0, Pos, Hinge, NumVs},
        {Flatten,Orientation,_}, Percent, Angle) ->
    Plane = reverse_norm(CwNorm, Plane0, Orientation),
    Cross = reverse_norm(CwNorm, Cross0, Orientation),
    {Deg, RotPoint} = angle_and_point(Angle, Opp, Index, NumVs, Hinge, Cross),
    Pos1 = rotate(Pos, Plane, RotPoint, Deg),
    Pos2 = flatten(Flatten, Pos1, Vpos, Plane),
    Norm = e3d_vec:sub(Pos2, Vpos),
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

arc_center(Vpos, _, _, _, _, _, _, _, 0.0) -> Vpos;
arc_center(Vpos, Angle, Index, NumVs, Pos, Center, Plane, {Flatten,AxisMode,AngleMode}, Percent) ->
    DegIncrement = acute_obtuse(AngleMode, Angle, NumVs),
    RotationAmount = rotation_amount(AxisMode, DegIncrement, Index),
    Pos1 = rotate(Pos, Plane, Center, RotationAmount),
    Pos2 = flatten(Flatten, Pos1, Vpos, Plane),
    Norm = e3d_vec:sub(Pos2, Vpos),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent)).

acute_obtuse(acute, Angle, NumVs) -> Angle / NumVs;
acute_obtuse(obtuse, Angle, NumVs) -> - (360 - Angle) / NumVs.

rotation_amount(normal, Deg, Index) -> Deg * Index;
rotation_amount(reverse, Deg, Index) -> -Deg * Index.

%%%% Closed Loop. Calculate the final position of each vertex (NewPos).
%%%% Measure the distance between NewPos and the Center (Factor). Move the
%%%% vertex towards the NewPos by a distance of the drag Dist * Factor.
make_circular(_Center, _Ray, _Nearest, _Axis, _Deg, Vpos, _State, 0.0, 0.0) -> Vpos;
make_circular(Center, Ray, Nearest, Plane, Deg, Vpos, {Flatten,_,Mode}, Percent, Dia) ->
    Pos0 = static_pos(Mode, Center, Ray, Nearest, Dia),
    Pos1 = rotate(Pos0, Plane, Center, Deg),
    Pos2 = flatten(Flatten, Pos1, Vpos, Plane),
    Norm = e3d_vec:sub(Pos2, Vpos),
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

static_pos(relative, Center, Ray, Nearest, Dia) ->
    e3d_vec:add(Center, e3d_vec:mul(Ray, Nearest*Dia));
static_pos(absolute, Center, Ray, _Nearest, Dia) ->
    e3d_vec:add(Center, e3d_vec:mul(Ray, Dia/2)).

flatten(true, Pos, _Vpos, _Plane) -> Pos;
flatten(false, Pos, Vpos, Plane) -> intersect_vec_plane(Pos, Vpos, Plane).

rotate(Vpos, Axis, {Cx,Cy,Cz}, Angle) ->
    %% return new position as {x,y,z}
    A0 = e3d_mat:translate(Cx, Cy, Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    e3d_mat:mul_point(A2, Vpos).

intersect_vec_plane(PosA, PosA, _) -> PosA;
intersect_vec_plane(PosA, PosB, PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), PlaneNorm),
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

reverse_norm({0.0,0.0,0.0}, Norm, reverse) -> e3d_vec:neg(Norm);
reverse_norm(_, Norm, _) -> Norm.

%%%% Selection errors
-spec circ_sel_error() -> no_return().
circ_sel_error() ->
    wings_u:error_msg(?__(3,"Selection must consist of one or more open edge loops\nor a single closed loop")).
-spec circ_sel_error_1() -> no_return().
circ_sel_error_1() ->
    wings_u:error_msg(?__(1,"Selected edge loops may not share vertices")).
-spec circ_sel_error_3() -> no_return().
circ_sel_error_3() ->
    wings_u:error_msg(?__(2,"Selections including single edges cannot be processed")).
-spec circ_sel_error_4() -> no_return().
circ_sel_error_4() ->
    wings_u:error_msg(?__(2,"Selected edge loops must be non-intersecting, and be either all open or all closed.")).
