%%
%%  wpc_arc.erl --
%%
%%    Plugin to flatten, equalise, and inflate a partial edge loop into an arc
%%
%%  Copyright (c) 2008 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_arc).
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
    [arc_menu(), separator|NewMenu];
parse([A = {_,loop_cut,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,arc_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

arc_menu() ->
    {?__(1,"Arc"), arc_options(),
      {?__(2,"Arc partial edge loop"),[],
       ?__(3,"Arc partial edge loop around defined centre")},[]}.

arc_options() ->
    fun
      (1,_Ns) -> {edge,{arc,{'ASK',[plane]}}};
      (3,_Ns) -> {edge,{arc_center,{'ASK',[plane,center]}}};
      (_,_) -> ignore
    end.

%%%% Commands
%% Check for multiple segments -> error
%% Check for full loop -> error
command({edge,{arc,{'ASK',Ask}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    Vs = check_multiple_segments(Edges,We),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
      true ->
        sel_error();
      false ->
        wings:ask(selection_ask(Ask), St, fun(Plane,St0) ->
        arc_setup(Plane,Vs,We,St0)
        end)
    end;

command({edge,{arc,Plane}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    Vs = check_multiple_segments(Edges,We),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
      true ->
        sel_error();
      false ->
        arc_setup(Plane,Vs,We,St)
    end;

command({edge,{arc,_}}, _) ->
    sel_error();

command({edge,{arc_center,{'ASK',Ask}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    Vs = check_multiple_segments(Edges,We),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
      true ->
        sel_error();
      false ->
        wings:ask(selection_ask(Ask), St, fun({Plane,Center},St0) ->
        arc_center_setup(Plane,Center,Vs,We,St0)
        end)
    end;

command({edge,{arc_center,{Plane,Center}}}, #st{shapes=Shs,sel=[{Id,Sel}]}=St) ->
    We = gb_trees:get(Id,Shs),
    Edges = gb_sets:to_list(Sel),
    Vs = check_multiple_segments(Edges,We),
    case is_list(wings_edge_loop:edge_loop_vertices(Edges,We)) of
      true ->
        sel_error();
      false ->
        arc_center_setup(Plane,Center,Vs,We,St)
    end;

command({edge,{arc_center,_}}, _) ->
    sel_error();

command(_, _) ->
    next.

check_multiple_segments(Edges,We) ->
    case wings_edge_loop:edge_links(Edges,We) of
      [Vs] -> Vs;
      _ -> sel_error()
    end.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Pick plane"),
    selection_ask(Rest,[{axis,Desc}|Ask]);

selection_ask([center|Rest],Ask) ->
    Desc = ?__(2,"Pick point from which the center will be calculated relative to chosen plane and ends of edge selection"),
    selection_ask(Rest,[{point,Desc}|Ask]).

sel_error() ->
    wings_u:error(?__(1,"Selection must be a single partial edge loop")).

%%%% Data Setup
arc_setup(Plane,Vs,#we{id=Id}=We,St) ->
    %% StartVs and EndVs are in 3rd of First and 2nd of Last
    [{_,_,StartVs}|_] = Vs,
    {_,EndVs,_} = lists:last(Vs),
    VPos1 = wings_vertex:pos(StartVs, We),
    VPos2 = wings_vertex:pos(EndVs, We),
    Hinge = e3d_vec:average(VPos1, VPos2),
    Opp = e3d_vec:dist(Hinge, VPos1),
    Chord = e3d_vec:sub(VPos1, VPos2),
    Cross = e3d_vec:norm(e3d_vec:cross(Chord,Plane)),
    {Vertices,DegVertList} = make_degree_vert_list(Vs,[],0,[]),
    NumVs = length(DegVertList) + 1,
    VsPos = wings_util:add_vpos(Vertices,We),
    Data = {Cross, Opp, e3d_vec:norm(Plane), VPos1, Hinge, DegVertList, NumVs},
    Tvs = [{Id,{Vertices,make_arc_fun(Data,VsPos)}}],
    Units = [angle, skip, percent], %% 'skip' is simply a place holder so that
    %% the moused drag between 'angle' and 'percent' are separated.
    wings_drag:setup(Tvs, Units, [{initial,[0.0,0.0,1.0]}], St).

arc_center_setup(Plane,Center,Vs,#we{id=Id}=We,St) ->
    %% StartVs and EndVs are in 3rd of First and 2nd of Last
    [{_,_,StartVs}|_] = Vs,
    {_,EndVs,_} = lists:last(Vs),
    VPos1 = wings_vertex:pos(StartVs, We),
    VPos2 = wings_vertex:pos(EndVs, We),
    Hinge = e3d_vec:average(VPos1, VPos2),

    Chord = e3d_vec:sub(VPos1, Hinge),
    Cross = e3d_vec:cross(Chord,Plane),
    CenterPoint = intersect_vec_plane(Hinge,Center,Cross),

    %% get angle
    Vec1 = e3d_vec:sub(CenterPoint,VPos1),
    Vec2 = e3d_vec:sub(CenterPoint,VPos2),
    Angle = e3d_vec:degrees(Vec1,Vec2),

    {Vertices,DegVertList} = make_degree_vert_list(Vs,[],0,[]),
    NumVs = length(DegVertList) + 1,
    VsPos = wings_util:add_vpos(Vertices,We),
    Data = {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},

    State = {none,normal,acute},
    Tvs = [{Id,{Vertices,make_arc_center_fun(Data,State,VsPos)}}],
    Flags = [{mode,{center_modes(),State}},{initial,[1.0]}],
    Units = [percent],
    wings_drag:setup(Tvs, Units, Flags, St).

intersect_vec_plane(PosA,PosB,_PlaneNorm) when (PosA == PosB) ->
    PosA;
intersect_vec_plane(PosA,PosB,PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

%%%% Center Modes
center_modes() ->
    fun(help, State) -> mode_help(State);
      ({key,$1},{none,_norm,acute}) -> {none,_norm,obtuse};
      ({key,$1},{none,_norm,obtuse}) -> {none,_norm,acute};
      ({key,$2},{none,normal,_angle}) -> {none,reverse,_angle};
      ({key,$2},{none,reverse,_angle}) -> {none,normal,_angle};
      (_,_) -> none
    end.

%%%% Mode Help
mode_help({_,Norm,Angle}) ->
    [angle_help(Angle),
     norm_help(Norm)].

angle_help(acute) -> ?__(1,"[1] Use Obtuse Angle");
angle_help(obtuse) -> ?__(2,"[1] Use Acute Angle").
norm_help(normal) -> ?__(1,"[2] Flip Arc Normal");
norm_help(reverse) -> ?__(2,"[2] Flip Arc Normal Back").

%%%% Index vertices
make_degree_vert_list([],Vertices,_,DegVertList) ->
    {Vertices,DegVertList};

make_degree_vert_list([_|Vs], Vertices, 0, DegVertList) ->
    %% This is the first time through the list.  Since the first Vertex (VPos1)
    %% doesn't move, remove it from the list so it isn't processed later in
    %% arc/5. (The last vertex (VPos2) is already not in the list).
    make_degree_vert_list(Vs, Vertices, 1, DegVertList);

make_degree_vert_list([{_,_,Vert}|Vs], Vertices, Index, DegVertList) ->
    make_degree_vert_list(Vs, [Vert|Vertices], Index+1, [{Vert, Index}|DegVertList]).

%%%% Drag Data
make_arc_fun(Data,VsPos) ->
    fun
      ([Angle, _, Percent|_], A) ->
        make_arc_2(Data, VsPos, Percent, Angle, A)
    end.

make_arc_2(Data,VsPos,Percent,Angle,A) ->
    lists:foldl(fun({Vertex,Vpos}, VsAcc) ->
      [{Vertex,arc(Vertex,Vpos,Data,Percent,Angle)}|VsAcc]
    end,A,VsPos).

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


%%%% Main Functions
arc(_Vertex,Vpos,_Data,0.0,0.0) ->
    Vpos;

arc(Vertex, Vpos, {_, Opp, _, VPos1, Hinge, DegVertList, NumVs}, Percent, 0.0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Segment = (Opp * 2) / NumVs,
    Norm = e3d_vec:norm(e3d_vec:sub(Hinge, VPos1)),
    NewPos = e3d_vec:add(VPos1, e3d_vec:mul(Norm, Segment * Index)),
    NewNorm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos, Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(NewNorm, Percent * Factor));

arc(Vertex, Vpos, {_, _, Plane, VPos1, Hinge, DegVertList, NumVs}, Percent, 180.0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = 180.0 / NumVs,
    RotationAmount = Deg * Index,
    NewPos = rotate(VPos1, Plane, Hinge, RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos, Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor));

arc(Vertex, Vpos, {Cross, Opp, Plane, VPos1, Hinge, DegVertList, NumVs}, Percent, Angle0) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Angle =  90.0 - (Angle0/2.0),
    Deg = (180.0 - (Angle * 2)) / NumVs,
    Radians = (math:pi()/(180.0/Angle)),
    %% Erlang trigonomic inputs have to be converted from Degrees to Radians
    Adj = math:tan(Radians) * Opp,
    RotationAmount = Deg * Index,
    RotPoint = e3d_vec:add(Hinge, e3d_vec:mul(Cross, Adj)),
    NewPos = rotate(VPos1,Plane,RotPoint,RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor)).

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

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{_,normal,acute}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = Angle / NumVs,
    RotationAmount = -Deg * Index,
    NewPos = rotate(VPos1,Plane,CenterPoint,RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor));

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{_,normal,obtuse}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = (360 - Angle) / NumVs,
    RotationAmount = Deg * Index,
    NewPos = rotate(VPos1,Plane,CenterPoint,RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor));

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{_,reverse,acute}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = Angle / NumVs,
    RotationAmount = Deg * Index,
    NewPos = rotate(VPos1,Plane,CenterPoint,RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor));

arc_center(Vertex, Vpos, {Angle, CenterPoint, Plane, VPos1, DegVertList, NumVs},{_,reverse,obtuse}, Percent) ->
    {_,{_,Index}} = lists:keysearch(Vertex, 1, DegVertList),
    Deg = (360.0 - Angle) / NumVs,
    RotationAmount = -Deg * Index,
    NewPos = rotate(VPos1,Plane,CenterPoint,RotationAmount),
    Norm = e3d_vec:norm(e3d_vec:sub(NewPos, Vpos)),
    Factor = abs(e3d_vec:dist(NewPos,Vpos)),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Percent * Factor)).

rotate(Vpos,Axis,{Cx,Cy,Cz},Angle) ->
    %% return new position as {x,y,z}
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Axis)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).
