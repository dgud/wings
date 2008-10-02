%%
%%  wpc_contour.erl --
%%
%%    Contour creates edges around or inside a selected face region and allows
%%    you to move them parallel to the original edges. Works best for square
%%    geometry. Interface includes switching between Average, Normalise, and
%%    Stay on Line which provide 3 possible solutions to keeping the edges
%%    parallel. Hold down the rmb while dragging to bump the selection up or
%%    down acccording to either the face normals, or the region's normal.
%%
%%  Copyright (c) 2008 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_contour).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.
menu({face},Menu) ->
    lists:reverse(parse(Menu, [], false));

menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [contour_menu()|NewMenu];
parse([A = {_,intrude,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,contour_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

%%%% Menus
contour_menu() ->
    Title = contour_title(),
    HelpL = contour_lmb_help(),
    HelpM = contour_mmb_help(),
    HelpR = contour_rmb_help(),
    case wings_pref:get_value(advanced_menus) of
        true -> {Title, contour_fun(), {HelpL,HelpM,HelpR},[]};
        false -> {Title,{contour,
                  [{?__(1,"Inset Region"),inset_region,HelpL},
                   {?__(2,"Inset Faces"),inset_faces,HelpM},
                   {?__(3,"Offset Region"),offset_region,HelpR}]}}
    end.

contour_title() ->
    ?__(1,"Contour").
contour_lmb_help() ->
    ?__(1,"Create edges inside selection perimeter").
contour_mmb_help() ->
    ?__(1,"Create edges inside individual faces").
contour_rmb_help() ->
    ?__(1,"Create edges outside selection perimeter").

contour_fun() ->
    fun
      (1,_Ns) -> {face,{contour,inset_region}};
      (2,_Ns) -> {face,{contour,inset_faces}};
      (3,_Ns) -> {face,{contour,offset_region}};
      (_, _)  -> ignore
    end.

%%%% Commands
command({face,{contour,inset_region}}, St) ->
    contour_setup(inset_region, St);
command({face,{contour,inset_faces}}, St) ->
    contour_setup(inset_faces, St);
command({face, {contour,offset_region}}, St) ->
    contour_setup(offset_region, St);
command(_,_) ->
    next.

%%%% Setup
contour_setup(inset_faces,St) ->
    contour_setup_1(inset_faces,extrude_faces(St));
contour_setup(Type, St0) ->
    St = wings_sel:map(fun extrude_region_0/2, St0),
    contour_setup_1(Type, St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrude %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                              %%
extrude_faces(St) ->                                                          %%
    wings_sel:map(fun(Faces, We) ->                                           %%
        wings_extrude_face:faces(Faces, We)                                   %%
    end, St).                                                                 %%
                                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrrude Region (from wings_face_cmd.erl) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                              %%
extrude_region_0(Faces0, We0) ->                                              %%
    %% We KNOW that a gb_set with fewer elements sorts before                 %%
    %% a gb_set with more elements.                                           %%
    Rs = lists:sort(wings_sel:face_regions(Faces0, We0)),                     %%
    We = extrude_region_1(Rs, We0, []),                                       %%
    extrude_region_vmirror(We0, We).                                          %%
                                                                              %%
extrude_region_1([Faces0|Rs0]=Rs, We0, Acc) ->                                %%
    case gb_sets:size(Faces0) of                                              %%
    1 ->                                                                      %%
        [Face] = gb_sets:to_list(Faces0),                                     %%
        extrude_region_1(Rs0, We0, [Face|Acc]);                               %%
    _Other ->                                                                 %%
        We = wings_extrude_face:faces(Acc, We0),                              %%
        wings_extrude_face:regions(Rs, We)                                    %%
    end;                                                                      %%
extrude_region_1([], We, Faces) ->                                            %%
    wings_extrude_face:faces(Faces, We).                                      %%
                                                                              %%
extrude_region_vmirror(_, #we{mirror=none}=We) -> We;                         %%
extrude_region_vmirror(OldWe, #we{mirror=Face0}=We0) ->                       %%
  %% Merge the mirror face and any newly created faces to one new mirror face %%
  %% and flatten it.                                                          %%
    FaceSet = gb_sets:singleton(Face0),                                       %%
    Bordering = wings_face:extend_border(FaceSet, We0),                       %%
    NewFaces = wings_we:new_items_as_gbset(face, OldWe, We0),                 %%
    Dissolve0 = gb_sets:intersection(Bordering, NewFaces),                    %%
    case gb_sets:is_empty(Dissolve0) of                                       %%
    true -> We0;                                                              %%
    false ->                                                                  %%
        Dissolve = gb_sets:insert(Face0, Dissolve0),                          %%
        We1 = wings_dissolve:faces(Dissolve, We0),                            %%
        [Face] = NewFace = wings_we:new_items_as_ordset(face, We0, We1),      %%
        We = wings_facemat:assign('_hole_', NewFace, We1),                    %%
        wings_we:mirror_flatten(OldWe, We#we{mirror=Face})                    %%
    end.                                                                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

contour_setup_1(Type, St0) ->
    DefaultState = {average,absolute,continue,loop},
    {Mode,AbRel,Stop,Norm} = wings_pref:get_value(contour, DefaultState),
    State = {Type,Mode,AbRel,Stop,Norm},
    St1 = type(Type,St0),
    Tvs = wings_sel:fold(fun(Faces, #we{id=Id}=We, Acc) ->
            AllTvs = wings_face:to_vertices(Faces, We),
            FaceRegions = wings_sel:strict_face_regions(Faces,We),
            Dict0 = orddict:new(),
            Dict = contour_data(FaceRegions, We, Dict0, Dict0, Dict0, Dict0),
            VsPos = wings_util:add_vpos(AllTvs, We),
            [{Id, {AllTvs, contour_fun(VsPos, Dict, State)}}|Acc]
            end, [], St1),
    Flags = [{mode, {modes(),State}}],
    wings_drag:setup(Tvs, drag_units(Type,AbRel,Stop), Flags, St0).

drag_units(inset_faces,absolute,continue) -> [distance,skip,skip,bump];
drag_units(inset_faces,relative,continue) -> [percent,skip,skip,bump];
drag_units(inset_faces,absolute,stop) -> [{distance,{0.0,infinity}},skip,skip,bump];
drag_units(inset_faces,relative,stop) -> [{percent,{0.0,1.0}},skip,skip,bump];
drag_units(_,_,continue) -> [distance,skip,skip,bump];
drag_units(_,_,stop) -> [{distance,{0.0,infinity}},skip,skip,bump].

type(offset_region, St0) -> wings_sel_conv:more(St0);
type(_inset, St) -> St.

contour_data([], _, EDict, FDict, LDict, SDict) ->
    {EDict, FDict, LDict, SDict};
contour_data([Faces|FaceRegions], We, EDict, FDict, LDict, SDict) ->
    Fs = gb_sets:to_list(Faces),
    Vs = wings_face:to_vertices(Fs,We),
    Edges = wings_face:outer_edges(Faces, We),
    Center = wings_vertex:center(Vs, We),
    ScaleDict = to_dict(Vs,Center,SDict),
    LoopNorm = loop_norm(Edges, We),
    LoopDict = to_dict(Vs, LoopNorm, LDict),
    FaceDict = face_norms(Fs, We, FDict),
    EdgeDict = vs_directions(Edges, We, Edges, EDict),
    contour_data(FaceRegions, We, EdgeDict, FaceDict, LoopDict, ScaleDict).

loop_norm(Edges,We) ->
%%%% Return average normal of multiple loops in a single face region
    Loops = wings_edge_loop:edge_loop_vertices(Edges, We),
    loop_norm_1(Loops, We, []).

loop_norm_1([Vs|Loops], We, Normals) ->
    Norm = wings_face:face_normal_ccw(Vs, We),
    loop_norm_1(Loops, We, [Norm|Normals]);
loop_norm_1([], _, [First|Normals]) ->
    e3d_vec:norm(e3d_vec:average([e3d_vec:neg(First)]++Normals)).

face_norms([F|Faces],We,Dict0) ->
    Norm = wings_face:normal(F, We),
    Vs = wings_face:to_vertices([F], We),
    Dict = to_dict(Vs,Norm,Dict0),
    face_norms(Faces,We,Dict);
face_norms([],_,Dict) ->
    orddict:map(fun(_,Normals) ->
        [e3d_vec:norm(e3d_vec:average(Normals))]
        end,Dict).

to_dict([V|Vs],Norm,Dict0) ->
    Dict = case orddict:find(V,Dict0) of
      {ok, _} ->
        orddict:append(V,Norm,Dict0);
      _Otherwise ->
        orddict:store(V,[Norm],Dict0)
    end,
    to_dict(Vs,Norm,Dict);
to_dict([],_,Dict) -> Dict.

vs_directions([], _, _, Dict) -> Dict;
vs_directions([Edge|Edges], #we{es=Etab,vp=Vtab}=We, OrigEs, Dict0) ->
    #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    VaPos = gb_trees:get(Va, Vtab),
    VbPos = gb_trees:get(Vb, Vtab),
    Ln = e3d_vec:norm(wings_face:normal(Lf, We)),
    Rn = e3d_vec:norm(wings_face:normal(Rf, We)),
    Normal = e3d_vec:norm(e3d_vec:add(Ln, Rn)),
    Fa = wings_face:center(Lf, We),
    Fb = wings_face:center(Rf, We),
    EdgeCenter = e3d_vec:average([VaPos,VbPos]),
    Da = e3d_vec:dist(Fa, EdgeCenter),
    Db = e3d_vec:dist(Fb, EdgeCenter),
    Vec = vector(Da, Db, VaPos, VbPos),
    VsDir = e3d_vec:norm(e3d_vec:cross(Normal, Vec)),
    PreDirA = pre_existing_edges(Va, VaPos, OrigEs, We),
    PreDirB = pre_existing_edges(Vb, VbPos, OrigEs, We),
    Dict = add_to_dict([{Va,PreDirA},{Vb,PreDirB}], VsDir, Normal, Dict0),
    vs_directions(Edges, We, OrigEs, Dict).

add_to_dict([{Vs,PreDir}|VsList], VsDir, Normal, Dict0) ->
    Dict1 = case orddict:find(Vs,Dict0) of
      {ok, [_VsDirA]} ->
        orddict:append(Vs,{PreDir,VsDir,Normal},Dict0);
      {ok, _Error} ->
          wings_u:error(?__(1,"Creating edges around a region requires\nadjacent faces to share at least one edge"));
      _Otherwise ->
        orddict:store(Vs,[{PreDir,VsDir,Normal}],Dict0)
    end,
    add_to_dict(VsList, VsDir, Normal, Dict1);
add_to_dict([], _, _, Dict) -> Dict.

pre_existing_edges(V, Pos, OrigEs, We) ->
%% Pre existing edges define the vector for the new vs. If none returns [].
%% If multiple edges, returns the average vector.
    LinkedVs0 = adjacent(V, OrigEs, We),
    average_vec(LinkedVs0, Pos, We, []).

adjacent(V, OrigEs, We) ->
    wings_vertex:fold(
      fun(Edge, _, Rec, A) ->
          OtherV = wings_vertex:other(V, Rec),
          case lists:member(Edge,OrigEs) of
            true -> A;
            false -> [OtherV|A]
          end
      end, [], V, We).

average_vec([Vs|LinkedVs], Pos, #we{vp=Vtab}=We, []) ->
    VPos = gb_trees:get(Vs, Vtab),
    case e3d_vec:dist(Pos, VPos) < 1.0e-6 of
      true ->
        average_vec(LinkedVs, Pos, We, []);
      false ->
        Vec = e3d_vec:sub(VPos, Pos),
        average_vec(LinkedVs, Pos, We, Vec)
    end;
average_vec([Vs|LinkedVs], Pos, #we{vp=Vtab}=We, Vec0) ->
    VPos = gb_trees:get(Vs, Vtab),
    case e3d_vec:dist(Pos, VPos) < 1.0e-6 of
      true ->
        average_vec(LinkedVs, Pos, We, Vec0);
      false ->
        Vec1 = e3d_vec:sub(VPos, Pos),
        Vec = e3d_vec:add(Vec0, Vec1),
        average_vec(LinkedVs, Pos, We, Vec)
    end;
average_vec([], _, _, []) -> [];
average_vec([], _, _, Vec) -> e3d_vec:norm(Vec).

vector(Da, Db, _VaPos, _VbPos) when Da =:= Db ->
    wings_u:error(?__(1,"Edges are too close togther.\nUse Cleanup command before trying again."));
vector(Da, Db, VaPos, VbPos) when Da < Db->
    e3d_vec:norm(e3d_vec:sub(VaPos, VbPos));
vector(_, _, VaPos, VbPos) ->
    e3d_vec:norm(e3d_vec:sub(VbPos, VaPos)).

modes() ->
    fun
      (help,State) -> mode_help(State);

      ({key,$4}, {Type,Mode,AbRel,continue,Norm}) ->
          {Type,Mode,AbRel,stop,Norm};
      ({key,$4}, {Type,Mode,AbRel,stop,Norm})     ->
          {Type,Mode,AbRel,continue,Norm};

      ({key,$5}, {inset_faces,_,_,_,_}) ->
          none;
      ({key,$5}, {Type,Mode,AbRel,Stop,loop}) ->
          {Type,Mode,AbRel,Stop,faces};
      ({key,$5}, {Type,Mode,AbRel,Stop,faces}) ->
          {Type,Mode,AbRel,Stop,loop};

      ({key,$6}, {inset_faces,Mode,absolute,Stop,Norm}) ->
          {inset_faces,Mode,relative,Stop,Norm};
      ({key,$6}, {inset_faces,Mode,relative,Stop,Norm}) ->
          {inset_faces,Mode,absolute,Stop,Norm};

      ({key,Key}, {Type,_,AbRel,Stop,Norm}) ->
          key_press(Key,Type,AbRel,Stop,Norm);
      (units, {Type,_,AbRel,Stop,_}) ->
          drag_units(Type,AbRel,Stop);
      (done,{_,Mode,AbRel,Stop,Norm}) ->
          wings_pref:set_value(contour, {Mode,AbRel,Stop,Norm});
      (_,_) -> none
    end.

key_press($1,Type,AbRel,Stop,Norm) ->  {Type,planar,AbRel,Stop,Norm};
key_press($2,Type,AbRel,Stop,Norm) ->  {Type,average,AbRel,Stop,Norm};
key_press($3,Type,AbRel,Stop,Norm) ->  {Type,equal,AbRel,Stop,Norm};
key_press(_,_,_,_,_) -> none.

mode_help({Type,Mode,AbRel,Stop,Norm}) ->
    Help = solution(Mode,[{"[1] ",planar},{"  [2] ",average},{"  [3] ",equal}]),
    Divider = [{bold," | "}],
    AbRelHelp = abs_rel_help(Type,AbRel),
    StopEdge = "  [4] " ++ stop_edge_help(Stop),
    Extrude = extrude_norm_help0(Type,Norm),
    Help ++ Divider  ++ StopEdge  ++ Extrude ++ AbRelHelp.

solution(Mode, [{Num,Mode}|Rest]) ->
    Num ++ [{bold,string(Mode)}] ++ solution(Mode, Rest);
solution(Mode, [{Num,ModeTag}|Rest]) ->
    Num ++ string(ModeTag) ++ solution(Mode, Rest);
solution(_,[]) ->[].

string(planar) -> ?__(1,"Normalise");
string(equal) -> ?__(2,"Along Edges");
string(average) -> ?__(3,"Average").

abs_rel_help(inset_faces, relative) -> "  [6] " ++ ?__(1,"Distance");
abs_rel_help(inset_faces, absolute) -> "  [6] " ++ ?__(2,"Percent");
abs_rel_help(_, _) -> [].

stop_edge_help(continue) -> ?__(1,"Stop at edges");
stop_edge_help(stop) -> ?__(2,"Continue past edges").

extrude_norm_help0(inset_faces,_) -> [];
extrude_norm_help0(_,Norm) ->
    "  [5] " ++ extrude_norm_help1(Norm).
extrude_norm_help1(loop) -> ?__(1,"Bump: Face Normal");
extrude_norm_help1(faces) -> ?__(2,"Bump: Region Normal").

%%%% Contour Fun
contour_fun(VsPos, Dict, State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          contour_fun(VsPos, Dict, NewState);
      ([Dist, _, _, Bump|_], A) ->
        {Type,Mode,AbRel,_,Norm} = State,
        lists:foldl(fun({V,Vpos0}, VsAcc) ->
        {EDict,FDict,LDict,SDict} = Dict,
        Vpos1 =  contour_absolute(V, Vpos0, EDict, FDict, LDict, Type,Mode,Norm, Dist, Bump),
        Vpos2 =  contour_relative(V, Vpos1, Vpos0, EDict, SDict, Type, AbRel, Dist),
        Vpos3 = bump(V, Vpos2, Type, Norm, FDict, LDict, Bump),
        [{V, Vpos3}|VsAcc]
        end, A, VsPos)
    end.

%%%% Main Functions
bump(_, Vpos, _, _, _, _, 0.0) -> Vpos;
bump(_, Vpos, offset_region, _, _, _, _) -> Vpos;

bump(V, Vpos, inset_faces, _, FDict, _, Bump) ->
    [Normal] = orddict:fetch(V,FDict),
    e3d_vec:add(Vpos, e3d_vec:mul(Normal, Bump));
bump(V, Vpos, _, loop, _, LDict, Bump) ->
    [Normal] = orddict:fetch(V,LDict),
    e3d_vec:add(Vpos, e3d_vec:mul(Normal, Bump));
bump(V, Vpos, _, faces, FDict, _, Bump) ->
    [Normal] = orddict:fetch(V,FDict),
    e3d_vec:add(Vpos, e3d_vec:mul(Normal, Bump)).

contour_relative(V, Vpos1, Vpos0, EDict, SDict, inset_faces, relative, Percent) ->
    case orddict:find(V, EDict) of
        {ok, [{_,V1,_},{_,V2,_}]} ->
            [Center] = orddict:fetch(V,SDict),
            Normal0 = e3d_vec:norm_sub(Vpos1,Vpos0),
            Point1 = intersect_vec_plane(Vpos0, Center, V1, Normal0),
            Point2 = intersect_vec_plane(Vpos0, Center, V2, Normal0),
            Normal1 = e3d_vec:norm_sub(Point1,Vpos0),
            Normal2 = e3d_vec:norm_sub(Point2,Vpos0),
            Dist1 = e3d_vec:dist(Vpos0, Point1),
            Dist2 = e3d_vec:dist(Vpos0, Point2),
            {Normal,Dist} = case Dist1 > Dist2 of
                true ->  {Normal2,Dist2};
                false -> {Normal1, Dist1}
            end,
            e3d_vec:add(Vpos0, e3d_vec:mul(Normal, Dist * Percent));
        _Otherwise -> Vpos0
    end;
contour_relative(_, Vpos1, _, _, _, _, _, _) -> Vpos1.

contour_absolute(_, Vpos,_,_,_,_,_,_, 0.0, 0.0) -> Vpos;
contour_absolute(V, Vpos, EDict,FDict,LDict, Type,Mode,Norm, Dist, Bump) ->
    case orddict:find(V, EDict) of
        {ok, [{_,V1,N1},{_,V2,N2}]} when Mode =:= planar ->
            PosA = e3d_vec:add(Vpos, e3d_vec:mul(V1, Dist)),
            PosB = e3d_vec:add(Vpos, e3d_vec:mul(V2, Dist)),
            case PosA =:= PosB of
                true -> PosA;
                false ->
                  V3 = e3d_vec:norm(e3d_vec:add(V1,V2)),
                  N = e3d_vec:norm(e3d_vec:cross(N1,N2)),
                  Na = e3d_vec:norm(e3d_vec:cross(N,N2)),
                  Nb = e3d_vec:norm(e3d_vec:cross(N,N1)),
                  P0 = intersect_vec_plane(Vpos,PosB,V2,V3),
                  P1 = intersect_vec_plane(P0,Vpos,N2,N2),
                  P2 = intersect_vec_plane(P1,Vpos,N1,N1),
                  P3 = intersect_vec_plane(P2,Vpos,N2,N2),
                  P4 = intersect_vec_plane(P3,Vpos,Na,Na),
                  intersect_vec_plane(P4,Vpos,Nb,Nb)
            end;
        {ok, [{_,V1,N1},{_,V2,N2}]} when Mode =:= average ->
            PosA = e3d_vec:add(Vpos, e3d_vec:mul(V1, Dist)),
            PosB = e3d_vec:add(Vpos, e3d_vec:mul(V2, Dist)),
            case PosA =:= PosB of
                true -> PosA;
                false ->
                  V3 = e3d_vec:norm(e3d_vec:add(V1,V2)),
                  N = e3d_vec:norm(e3d_vec:cross(N1,N2)),
                  NN = e3d_vec:norm(e3d_vec:cross(N,V3)),
                  P0 = intersect_vec_plane(Vpos,PosB,V2,V3),
                  intersect_vec_plane(P0,Vpos,NN,NN)
            end;
        {ok, [{[],V1,N1},{[],V2,N2}]} when Mode =:= equal ->
            V3 = e3d_vec:norm(e3d_vec:add(V1,V2)),
            Pos = e3d_vec:add(Vpos, e3d_vec:mul(V3, Dist)),
            PosA = intersect_vec_plane(Pos,Vpos,N1,N1),
            intersect_vec_plane(PosA,Vpos,N2,N2);
        {ok, [{E1,_,_},{_,_,_}]}  when Mode =:= equal ->
            e3d_vec:add(Vpos, e3d_vec:mul(E1, Dist));
        _Otherwise ->
            case {Type, Norm} of
                {offset_region, loop} ->
                    [Normal] = orddict:fetch(V,LDict),
                    e3d_vec:add(Vpos, e3d_vec:mul(Normal, Bump));
                {offset_region, faces} ->
                    [Normal] = orddict:fetch(V,FDict),
                    e3d_vec:add(Vpos, e3d_vec:mul(Normal, Bump));
                {_, _} -> Vpos
            end
    end.

intersect_vec_plane(PosA,PosB,Vector,Plane) ->
    %% Return point where Vector through PosA intersects with Plane at PosB
    DotProduct = e3d_vec:dot(Vector,Plane),
    case DotProduct of
      0.0 ->
        PosA;
      _Otherwise ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),Vector)/DotProduct,
        e3d_vec:add(PosA, e3d_vec:mul(Plane, Intersection))
    end.
