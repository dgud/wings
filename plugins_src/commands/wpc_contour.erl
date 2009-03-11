%%
%%  wpc_contour.erl --
%%
%%  Contour shows up in the face menu as Inset and includes the commands: Inset,
%%  Inset Region, and Offset Region.
%%
%%  Copyright (c) (2008-2009) Richard Jones.
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
    Title = title(),
    HelpL = lmb_help(),
    HelpM = mmb_help(),
    HelpR = rmb_help(),
    {Title, contour_fun(), {HelpL,HelpM,HelpR},[]}.

title() ->
    ?__(1,"Inset").
lmb_help() ->
    ?__(1,"Inset a face inside each selected face").
mmb_help() ->
    ?__(1,"Offset Region creating new edges around each face group selection").
rmb_help() ->
    ?__(1,"Inset Region creating new edges inside each face group selection").

contour_fun() ->
    fun
      (1,_Ns) -> {face,{contour,insetfaces}};
      (2,_Ns) -> {face,{contour,offsetregion}};
      (3,_Ns) -> {face,{contour,insetregion}};
      (_, _)  -> ignore
    end.

%%%% Commands
command({face,{contour,insetregion}}, St) ->
    ?SLOW(contour_setup(inset_region, St));
command({face,{contour,insetfaces}}, St) ->
    ?SLOW(contour_setup(inset_faces, St));
command({face, {contour,offsetregion}}, St) ->
    ?SLOW(contour_setup(offset_region, St));
command(_,_) ->
    next.

%%%% Setup
contour_setup(inset_faces,St) ->
    inset_faces_setup(inset_faces,extrude_faces(St));
contour_setup(inset_region, St0) ->
    St = wings_sel:map(fun extrude_region_0/2, St0),
    inset_regions_setup(inset_region, St);
contour_setup(offset_region, St0) ->
    St = wings_sel:map(fun extrude_region_0/2, St0),
    offset_regions_setup(offset_region, St).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrude %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                              %%
extrude_faces(St) ->                                                          %%
    wings_sel:map(fun(Faces, We) ->                                           %%
        wings_extrude_face:faces(Faces, We)                                   %%
    end, St).                                                                 %%
                                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrude Region (from wings_face_cmd.erl) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

offset_regions_setup(offset_region, St) ->
    State = drag_mode(inset_region),
    SetupSt = wings_sel_conv:more(St),
    Tvs = wings_sel:fold(fun(Faces, #we{id=Id}=We, Acc) ->
            FaceRegions = wings_sel:face_regions(Faces,We),
            {AllVs0,VsData} = collect_offset_regions_data(FaceRegions,We,[],[]),
            test_selection(AllVs0),
            AllVs = ordsets:from_list(AllVs0),
            [{Id, {AllVs, offset_regions_fun(VsData, State)}}|Acc]
            end, [], SetupSt),
    Flags = [{mode,{modes(),State}}],
    wings_drag:setup(Tvs, drag_units(State), Flags, St).

inset_regions_setup(inset_region, St) ->
    State = drag_mode(inset_region),
    Tvs = wings_sel:fold(fun(Faces, #we{id=Id}=We, Acc) ->
            FaceRegions = wings_sel:face_regions(Faces,We),
            {AllVs0,VsData} = collect_inset_regions_data(FaceRegions,We,[],[]),
            AllVs = ordsets:from_list(AllVs0),
            [{Id, {AllVs, inset_regions_fun(VsData, State)}}|Acc]
            end, [], St),
    Flags = [{mode,{modes(),State}}],
    wings_drag:setup(Tvs, drag_units(State), Flags, St).

inset_faces_setup(inset_faces, St) ->
    State = drag_mode(inset_faces),
    Tvs = wings_sel:fold(fun(Faces, #we{id=Id}=We, Acc) ->
            {AllVs0,VData} = collect_inset_face_data(Faces,We,[],[],none),
            AllVs = ordsets:from_list(AllVs0),
            [{Id, {AllVs, inset_faces_fun(VData, State)}}|Acc]
            end, [], St),
    Flags = [{mode,{modes(),State}}],
    wings_drag:setup(Tvs, drag_units(State), Flags, St).

test_selection(AllVs) ->
    case length(AllVs) == length(lists:usort(AllVs)) of
      true -> ok;
      false -> loop_error_2()
    end.

drag_mode(inset_faces) ->
    Prefs = wings_pref:get_value(inset_faces,{average,relative,stop,per_obj}),
    {Mode,Drag,Stop,Smallest} = Prefs,
    {inset_faces,Mode,Drag,Stop,Smallest};
drag_mode(Type) ->
    {Mode,Norm} = wings_pref:get_value(inset_region,{average,loop}),
    {Type,Mode,Norm}.

drag_units({inset_faces,_,absolute,_,_}) -> [distance,skip,skip,bump];
drag_units({inset_faces,_,relative,continue,_}) -> [percent,skip,skip,bump];
drag_units({inset_faces,_,relative,stop,_}) -> [{percent,{0.0,1.0}},skip,skip,bump];
drag_units({_,_,_}) -> [distance,skip,skip,bump].

modes() ->
    fun
      (help,State) -> mode_help(State);

      ({key,$3}, {inset_faces,Mode,relative,continue,Smallest}) ->
          {inset_faces,Mode,relative,stop,Smallest};
      ({key,$3}, {inset_faces,Mode,relative,stop,Smallest})     ->
          {inset_faces,Mode,relative,continue,Smallest};

      ({key,$4}, {inset_faces,_,_,_,_}) ->
          none;
      ({key,$4}, {Type,Mode,loop}) ->
          {Type,Mode,faces};
      ({key,$4}, {Type,Mode,faces}) ->
          {Type,Mode,loop};

      ({key,$5}, {inset_faces,Mode,absolute,Stop,Smallest}) ->
          {inset_faces,Mode,relative,Stop,Smallest};
      ({key,$5}, {inset_faces,Mode,relative,Stop,Smallest}) ->
          {inset_faces,Mode,absolute,Stop,Smallest};

      ({key,Key}, {inset_faces,Mode,AbRel,Stop,Smallest}) ->
          key_press(Key,inset_faces,Mode,AbRel,Stop,Smallest);
      ({key,Key}, {Type,Mode,Norm}) ->
          key_press(Key,Type,Mode,Norm);
      (units, State) ->
          drag_units(State);
      (done,{inset_faces,Mode,AbRel,Stop,Smallest}) ->
          wings_pref:set_value(inset_faces,{Mode,AbRel,Stop,Smallest});
      (done,{_,Mode,Norm}) ->
          wings_pref:set_value(inset_region, {Mode,Norm});
      (_,_) -> none
    end.

key_press($1,inset_faces,Mode,relative,Stop,_) ->
    {inset_faces,Mode,relative,Stop,per_obj};
key_press($2,inset_faces,Mode,relative,Stop,_) ->
    {inset_faces,Mode,relative,Stop,per_face};
key_press($1,inset_faces,_,absolute,Stop,Smallest) ->
    {inset_faces,average,absolute,Stop,per_obj};
key_press($2,inset_faces,_,absolute,Stop,Smallest) ->
    {inset_faces,along_edges,absolute,Stop,per_face};
key_press(_,_,_,_,_,_) -> none.

key_press($1,Type,_,Norm) ->  {Type,average,Norm};
key_press($2,Type,_,Norm) ->  {Type,along_edges,Norm};
key_press(_,_,_,_) -> none.

mode_help({inset_faces,Mode,absolute,_,_}) ->
    Help = solution_type(Mode,[{"[1] ",average},{"  [2] ",along_edges}]),
    Divider = [{bold," | "}],
    AbRelHelp = abs_rel_help(inset_faces,absolute),
    Help ++ Divider  ++ AbRelHelp;

mode_help({inset_faces,_,relative,Stop,Smallest}) ->
    Help = solution_type(Smallest,[{"[1] ",per_obj},{"  [2] ",per_face}]),
    Divider = [{bold," | "}],
    StopEdge = "  [3] " ++ stop_edge_help(Stop),
    AbRelHelp = abs_rel_help(inset_faces,relative),
    ?__(1,"Scale by Shortest Vector per: ") ++ Help ++ Divider ++ StopEdge ++ AbRelHelp;


mode_help({Type,Mode,Norm}) ->
    Help = solution_type(Mode,[{"[1] ",average},{"  [2] ",along_edges}]),
    Divider = [{bold," | "}],
    Extrude = extrude_norm_help0(Type,Norm),
    Help ++ Divider ++ Extrude.

solution_type(Mode, [{Num,Mode}|Rest]) ->
    Num ++ [{bold,string(Mode)}] ++ solution_type(Mode, Rest);
solution_type(Mode, [{Num,ModeTag}|Rest]) ->
    Num ++ string(ModeTag) ++ solution_type(Mode, Rest);
solution_type(_,[]) -> [].

string(along_edges) -> ?__(2,"Along Edges");
string(average) -> ?__(3,"Average");
string(per_obj) -> ?__(4,"Object");
string(per_face) -> ?__(5,"Face").

abs_rel_help(inset_faces, relative) -> "  [5] " ++ ?__(1,"Distance");
abs_rel_help(inset_faces, absolute) -> "  [5] " ++ ?__(2,"Percent");
abs_rel_help(_, _) -> [].

stop_edge_help(continue) -> ?__(1,"Stop at edges");
stop_edge_help(stop) -> ?__(2,"Continue past edges").

extrude_norm_help0(inset_faces,_) -> [];
extrude_norm_help0(_,Norm) ->
    "  [4] " ++ extrude_norm_help1(Norm).
extrude_norm_help1(loop) -> ?__(1,"Bump: Face Normal");
extrude_norm_help1(faces) -> ?__(2,"Bump: Region Normal").

%%%% OffSet Regions
collect_offset_regions_data([Faces|Regions],We,AllVs,VsData) ->
    {FaceNormTab,OuterEdges,RegVs} = faces_data_0(Faces,We,[],[],[]),
    {LoopNorm,LoopVsData,LoopVs} = offset_regions_loop_data(OuterEdges,Faces,We),
    test_selection(LoopVs),
    Vs = RegVs -- LoopVs,
    RegVsData = vertex_normals(Vs,FaceNormTab,We,LoopVsData),
    collect_offset_regions_data(Regions,We,RegVs++AllVs,[{LoopNorm,RegVsData}|VsData]);
collect_offset_regions_data([],_,AllVs,VsData) ->
    {AllVs,VsData}.

%%%% Inset Regions
collect_inset_regions_data([Faces|Regions],We,AllVs,VsData) ->
    {FaceNormTab,OuterEdges,RegVs} = faces_data_0(Faces,We,[],[],[]),
    {LoopNorm,LoopVsData,LoopVs} = inset_regions_loop_data(OuterEdges,FaceNormTab,We),
    Vs = RegVs -- LoopVs,
    RegVsData = vertex_normals(Vs,FaceNormTab,We,LoopVsData),
    collect_inset_regions_data(Regions,We,RegVs++AllVs,[{LoopNorm,RegVsData}|VsData]);
collect_inset_regions_data([],_,AllVs,VsData) ->
    {AllVs,VsData}.

%%%% Inset and Offset regional face data
faces_data_0(Faces0,#we{es=Etab,vp=Vtab,fs=Ftab}=We,FaceNorms0,EAcc0,Vs0) ->
    case gb_sets:is_empty(Faces0) of
      false ->
        {Face,Faces1} = gb_sets:take_smallest(Faces0),
        Edge = gb_trees:get(Face, Ftab),
        {FNorm,EAcc,Vs} = faces_data_1(Edge,Face,Etab,Vtab,EAcc0,Vs0),
        faces_data_0(Faces1,We,[FNorm|FaceNorms0],EAcc,Vs);
      true ->
        FaceNormTab = gb_trees:from_orddict(lists:sort(FaceNorms0)),
        OuterEdges = outer_edges_1(lists:sort(EAcc0),[]),
        {FaceNormTab,OuterEdges,lists:usort(Vs0)}
    end.

faces_data_1(Edge,Face,Etab,Vtab,EAcc,Vs) ->
    case gb_trees:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposA = gb_trees:get(Va,Vtab),
        VposB = gb_trees:get(Vb,Vtab),
        faces_data_2(NextEdge,Face,Edge,Etab,Vtab,[VposB,VposA],[Edge|EAcc],[Vb|Vs]);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposA = gb_trees:get(Va,Vtab),
        VposB = gb_trees:get(Vb,Vtab),
        faces_data_2(NextEdge,Face,Edge,Etab,Vtab,[VposA,VposB],[Edge|EAcc],[Va|Vs])
    end.

faces_data_2(LastEdge,Face,LastEdge,_,_,Vp,EAcc,Vs) ->
    {{Face,e3d_vec:normal(Vp)},EAcc,Vs};

faces_data_2(Edge,Face,LastEdge,Etab,Vtab,Vp,EAcc,Vs) ->
    case gb_trees:get(Edge,Etab) of
      #edge{ve=V,lf=Face,ltpr=NextEdge} ->
        Vpos = gb_trees:get(V,Vtab),
        faces_data_2(NextEdge,Face,LastEdge,Etab,Vtab,[Vpos|Vp],[Edge|EAcc],[V|Vs]);
      #edge{vs=V,rf=Face,rtpr=NextEdge} ->
        Vpos = gb_trees:get(V,Vtab),
        faces_data_2(NextEdge,Face,LastEdge,Etab,Vtab,[Vpos|Vp],[Edge|EAcc],[V|Vs])
    end.

outer_edges_1([E,E|T],Out) ->
    outer_edges_1(T,Out);
outer_edges_1([E|T],Out) ->
    outer_edges_1(T,[E|Out]);
outer_edges_1([],Out) -> Out.

%%%% Get vertex normals from FaceNormal gb_tree after listing all the faces
%%%% surrounding a vertex
vertex_normals([V|Vs],FaceNormTab,#we{vp=Vtab}=We,Acc) ->
    FaceNorms = wings_vertex:fold(fun(_,Face,_,A) ->
        [gb_trees:get(Face,FaceNormTab)|A]
    end,[],V,We),
    VNorm = e3d_vec:norm(e3d_vec:add(FaceNorms)),
    Vpos = gb_trees:get(V,Vtab),
    vertex_normals(Vs,FaceNormTab,We,[{V,{Vpos,VNorm}}|Acc]);
vertex_normals([],_,_,Acc) -> Acc.

%%%% Return data for Inset Region cmd, including the average normal for one or
%%%% multiple eloops in a face region and collect vector data for the eloop
%%%% vertices.
inset_regions_loop_data([], _, _) ->
    loop_error_1();
inset_regions_loop_data(Edges,FNtab,We) ->
    EdgeSet = gb_sets:from_list(Edges),
    loop_vertices_data_0(EdgeSet,FNtab,We,[],[],[]).

loop_vertices_data_0(EdgeSet0,FNtab,#we{es=Etab,vp=Vtab}=We,LNorms,VData0,Vs0) ->
    case gb_sets:is_empty(EdgeSet0) of
      false ->
        {Edge,EdgeSet1} = gb_sets:take_smallest(EdgeSet0),
        {EdgeSet,VData,Links,LoopNorm,Vs} = loop_vertices_data_1(Edge,EdgeSet1,FNtab,Etab,Vtab,VData0,Vs0),
        loop_vertices_data_0(EdgeSet,FNtab,We,[{Links,LoopNorm}|LNorms],VData,Vs);
      true ->
        AvgLoopNorm = average_loop_norm(LNorms),
        {AvgLoopNorm,VData0,Vs0}
    end.

loop_vertices_data_1(Edge,EdgeSet,FNtab,Etab,Vtab,VData,Vs) ->
    #edge{vs=Va,ve=Vb,rf=Rf,rtpr=NextEdge} = gb_trees:get(Edge, Etab),
    VposA = gb_trees:get(Va,Vtab),
    VposB = gb_trees:get(Vb,Vtab),
    FNorm = gb_trees:get(Rf,FNtab),
    VDir = e3d_vec:sub(VposB,VposA),
    EdgeData = gb_trees:get(NextEdge,Etab),
    loop_vertices_data_2(NextEdge,EdgeData,Va,VposA,Rf,Edge,FNtab,Etab,Vtab,EdgeSet,VDir,[],[FNorm],VData,[],Vs,0).

loop_vertices_data_2(LastE,#edge{vs=Va,ve=Vb,rf=PrevFace},
      Vb,VposB,PrevFace,LastE,_,_,Vtab,EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    VposA = gb_trees:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    VNormal = e3d_vec:norm(e3d_vec:add(VNorms)),
    EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
    VData = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0],
    LoopNorm = e3d_vec:normal([VposB|VPs]),
    Vs = [Vb|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

loop_vertices_data_2(CurE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevFace,ltpr=NextEdge,rtpr=IfNoFaceEdge},
      Vb,VposB,PrevFace,LastE,FNtab,Etab,Vtab,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposA = gb_trees:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    case  gb_trees:lookup(Face,FNtab) of
      none ->
        EdgeSet = gb_sets:delete(CurE,EdgeSet0),
        VNormal = e3d_vec:norm(e3d_vec:add(VNorms0)),
        EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
        VData = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0],
        NextVDir = e3d_vec:neg(Dir),
        EdgeData = gb_trees:get(IfNoFaceEdge,Etab),
        [FNorm|_] = VNorms0,
        VPs = [VposB|VPs0],
        Vs = [Vb|Vs0],
        loop_vertices_data_2(IfNoFaceEdge,EdgeData,Va,VposA,PrevFace,LastE,FNtab,Etab,Vtab,EdgeSet,NextVDir,[],[FNorm],VData,VPs,Vs,Links+1);
      {value,FNorm} ->
        EdgeData = gb_trees:get(NextEdge,Etab),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        loop_vertices_data_2(NextEdge,EdgeData,Vb,VposB,Face,LastE,FNtab,Etab,Vtab,EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end;

loop_vertices_data_2(_CurE,#edge{vs=Va,ve=Vb,lf=PrevFace,rf=Face,rtpr=NextEdge},
      Va,VposA,PrevFace,LastE,FNtab,Etab,Vtab,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposB = gb_trees:get(Vb,Vtab),
    Dir = e3d_vec:sub(VposB,VposA),
    FNorm = gb_trees:get(Face,FNtab),
    EdgeData = gb_trees:get(NextEdge,Etab),
    EDirs = [Dir|EDir0],
    VNorms = [FNorm|VNorms0],
    loop_vertices_data_2(NextEdge,EdgeData,Va,VposA,Face,LastE,FNtab,Etab,Vtab,EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links).

%%%% Return data for Offset Region cmd, including the average normal for one or
%%%% multiple eloops in a face region and collect vector data for the eloop
%%%% vertices
offset_regions_loop_data([],_,_) ->
    loop_error_1();
offset_regions_loop_data(Edges,Faces,We) ->
    EdgeSet = gb_sets:from_list(Edges),
    offset_loop_data_0(EdgeSet,Faces,We,[],[],[]).

offset_loop_data_0(EdgeSet0,Faces,We,LNorms,VData0,Vs0) ->
    case gb_sets:is_empty(EdgeSet0) of
      false ->
        {Edge,EdgeSet1} = gb_sets:take_smallest(EdgeSet0),
        {EdgeSet,VData,Links,LoopNorm,Vs} = offset_loop_data_1(Edge,EdgeSet1,Faces,We,VData0,Vs0),
        offset_loop_data_0(EdgeSet,Faces,We,[{Links,LoopNorm}|LNorms],VData,Vs);
      true ->
        AvgLoopNorm = average_loop_norm(LNorms),
        {AvgLoopNorm,VData0,Vs0}
    end.

offset_loop_data_1(Edge,EdgeSet,Faces,#we{es=Etab,vp=Vtab}=We,VData,Vs) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltsu=NextLeft,rtsu=NextRight} = gb_trees:get(Edge,Etab),
    VposA = gb_trees:get(Va,Vtab),
    VposB = gb_trees:get(Vb,Vtab),
    case gb_sets:is_member(Rf,Faces) of
      true ->
        VDir = e3d_vec:sub(VposB,VposA),
        FNorm = wings_face:normal(Lf,We),
        EdgeData = gb_trees:get(NextLeft,Etab),
        offset_loop_data_2(NextLeft,EdgeData,Va,VposA,Lf,Edge,We,EdgeSet,VDir,[],[FNorm],VData,[],Vs,0);
      false ->
        VDir = e3d_vec:sub(VposA,VposB),
        FNorm = wings_face:normal(Rf,We),
        EdgeData = gb_trees:get(NextRight,Etab),
        offset_loop_data_2(NextRight,EdgeData,Vb,VposB,Rf,Edge,We,EdgeSet,VDir,[],[FNorm],VData,[],Vs,0)
    end.

offset_loop_data_2(LastE,#edge{vs=Va,ve=Vb,lf=PrevFace},
        Vb,VposB,PrevFace,LastE,#we{vp=Vtab},EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    VposA = gb_trees:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    VNormal = e3d_vec:norm(e3d_vec:add(VNorms)),
    EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
    VData = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),EDir}}|VData0],
    LoopNorm = e3d_vec:normal([VposB|VPs]),
    Vs = [Vb|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

offset_loop_data_2(LastE,#edge{vs=Va,ve=Vb,rf=PrevFace},
        Va,VposA,PrevFace,LastE,#we{vp=Vtab},EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    VposB = gb_trees:get(Vb,Vtab),
    Dir = e3d_vec:sub(VposB,VposA),
    VNormal = e3d_vec:norm(e3d_vec:add(VNorms)),
    EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
    VData = [{Va,{VposA,evaluate_vdata(VDir,Dir,VNormal),EDir}}|VData0],
    LoopNorm = e3d_vec:normal([VposA|VPs]),
    Vs = [Va|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

offset_loop_data_2(CurE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevFace,ltsu=NextEdge,rtsu=IfCurIsMember},
        Va,VposA,PrevFace,LastE,#we{es=Etab,vp=Vtab}=We,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposB = gb_trees:get(Vb,Vtab),
    Dir = e3d_vec:sub(VposB,VposA),
    case gb_sets:is_member(CurE,EdgeSet0) of
      true ->
        EdgeSet = gb_sets:delete(CurE,EdgeSet0),
        VNormal = e3d_vec:norm(e3d_vec:add(VNorms0)),
        EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
        VData = [{Va,{VposA,evaluate_vdata(VDir,Dir,VNormal),EDir}}|VData0],
        NextVDir = e3d_vec:neg(Dir),
        EdgeData = gb_trees:get(IfCurIsMember,Etab),
        [FNorm|_] = VNorms0,
        VPs = [VposA|VPs0],
        Vs = [Va|Vs0],
        offset_loop_data_2(IfCurIsMember,EdgeData,Vb,VposB,PrevFace,LastE,We,EdgeSet,NextVDir,[],[FNorm],VData,VPs,Vs,Links+1);
      false ->
        FNorm = wings_face:normal(Face,We),
        EdgeData = gb_trees:get(NextEdge,Etab),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        offset_loop_data_2(NextEdge,EdgeData,Va,VposA,Face,LastE,We,EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end;

offset_loop_data_2(CurE,#edge{vs=Va,ve=Vb,lf=PrevFace,rf=Face,rtsu=NextEdge,ltsu=IfCurIsMember},
        Vb,VposB,PrevFace,LastE,#we{es=Etab,vp=Vtab}=We,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposA = gb_trees:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    case gb_sets:is_member(CurE,EdgeSet0) of
      true ->
        EdgeSet = gb_sets:delete(CurE,EdgeSet0),
        VNormal = e3d_vec:norm(e3d_vec:add(VNorms0)),
        EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
        VData = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),EDir}}|VData0],
        NextVDir = e3d_vec:neg(Dir),
        EdgeData = gb_trees:get(IfCurIsMember,Etab),
        [FNorm|_] = VNorms0,
        VPs = [VposB|VPs0],
        Vs = [Vb|Vs0],
        offset_loop_data_2(IfCurIsMember,EdgeData,Va,VposA,PrevFace,LastE,We,EdgeSet,NextVDir,[],[FNorm],VData,VPs,Vs,Links+1);
      false ->
        FNorm = wings_face:normal(Face,We),
        EdgeData = gb_trees:get(NextEdge,Etab),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        offset_loop_data_2(NextEdge,EdgeData,Vb,VposB,Face,LastE,We,EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end.

average_loop_norm([{_,LNorms}]) ->
    e3d_vec:norm(LNorms);
average_loop_norm([{LinksA,LNormA},{LinksB,LNormB}]) ->
    case LinksA < LinksB of
      true -> e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormA),LNormB));
      false -> e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormB),LNormA))
    end;
average_loop_norm(LNorms) ->
    LoopNorms = [Norm||{_,Norm}<-LNorms],
    e3d_vec:norm(e3d_vec:neg(e3d_vec:add(LoopNorms))).

average_edge_dir(VNormal,DirA,DirB,[]) ->
    Vec1 = e3d_vec:norm(e3d_vec:cross(VNormal,DirA)),
    Vec2 = e3d_vec:norm(e3d_vec:cross(DirB,VNormal)),
    e3d_vec:norm(e3d_vec:add(Vec1,Vec2));
average_edge_dir(_,_,_,[EDir]) -> e3d_vec:norm(EDir);
average_edge_dir(_,_,_,EDirs) -> e3d_vec:norm(e3d_vec:add(EDirs)).

evaluate_vdata(DirA,DirB,VNorm) ->
    A = e3d_vec:norm(e3d_vec:cross(VNorm,DirA)),
    B = e3d_vec:norm(e3d_vec:cross(DirB,VNorm)),
    Dot = e3d_vec:dot(A,B)+1,
    e3d_vec:divide(e3d_vec:add(A,B),Dot).

%%%% Inset Faces
collect_inset_face_data(Faces0,#we{es=Etab,vp=Vtab,fs=Ftab}=We,AllVs0,VData0,SmallestDist) ->
    case gb_sets:is_empty(Faces0) of
      false ->
        {Face,Faces1} = gb_sets:take_smallest(Faces0),
        Edge = gb_trees:get(Face, Ftab),
        {AllVs,VData1} = traverse_1(Edge,Face,Etab,Vtab,AllVs0),
        {VData2,Dist} = evaluate_data_1(VData1),
        NewSmallestDist = case Dist < SmallestDist of
            true -> Dist;
            false -> SmallestDist
        end,
        VData = [VData2|VData0],
        collect_inset_face_data(Faces1,We,AllVs,VData,NewSmallestDist);
      true -> {AllVs0,{SmallestDist,VData0}} %result
    end.

traverse_1(Edge,Face,Etab,Vtab,AllVs) ->
    case gb_trees:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposA = gb_trees:get(Va,Vtab),
        VposB = gb_trees:get(Vb,Vtab),
        VDir = e3d_vec:norm_sub(VposB,VposA),
        traverse_2(NextEdge,Face,VDir,VposB,Edge,Etab,Vtab,{Va,{VposA,VDir}},[],AllVs,[]);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposB = gb_trees:get(Vb,Vtab),
        VposA = gb_trees:get(Va,Vtab),
        VDir = e3d_vec:norm_sub(VposA,VposB),
        traverse_2(NextEdge,Face,VDir,VposA,Edge,Etab,Vtab,{Vb,{VposB,VDir}},[],AllVs,[])
    end.

traverse_2(LastEdge,_,PrevVDir,Vpos,LastEdge,_,_,{Vb,{Vpos,VDirA}},VPositions0,AllVs0,Acc) ->
    VDirB = e3d_vec:neg(PrevVDir),
    VData = [{Vb,{Vpos,VDirA,VDirB}}|Acc],
    AllVs = [Vb|AllVs0],
    VPositions = lists:reverse([Vpos|VPositions0]),
    FNorm = e3d_vec:normal(VPositions),
    FCntr = e3d_vec:average(VPositions),
    {AllVs,{FNorm,FCntr,VData}};

traverse_2(Edge,Face,PrevVDir,Vpos,LastEdge,Etab,Vtab,LastVert,VPositions0,AllVs0,Acc) ->
    case gb_trees:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposB = gb_trees:get(Vb, Vtab),
        VDirB = e3d_vec:norm_sub(Vpos,VposB),
        VDirA = e3d_vec:neg(PrevVDir),
        VData = [{Va,{Vpos,VDirB,VDirA}}|Acc],
        VPositions = [Vpos|VPositions0],
        AllVs = [Va|AllVs0],
        traverse_2(NextEdge,Face,VDirB,VposB,LastEdge,Etab,Vtab,LastVert,VPositions,AllVs,VData);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposA = gb_trees:get(Va, Vtab),
        VDirA = e3d_vec:norm_sub(VposA,Vpos),
        VDirB = e3d_vec:neg(PrevVDir),
        VData = [{Vb,{Vpos,VDirA,VDirB}}|Acc],
        VPositions = [Vpos|VPositions0],
        AllVs = [Vb|AllVs0],
        traverse_2(NextEdge,Face,VDirA,VposA,LastEdge,Etab,Vtab,LastVert,VPositions,AllVs,VData)
    end.

sqr_length({X,Y,Z}) ->
    X*X+Y*Y+Z*Z.

evaluate_data_1({FNorm,FCntr,VertexData}) ->
    evaluate_data_2(FNorm,FCntr,VertexData,[],none).

evaluate_data_2(FNorm,_,[],Acc,SmallestDist0) ->
    SmallestDist = math:sqrt(SmallestDist0),
    {{FNorm,SmallestDist,Acc},SmallestDist};

evaluate_data_2(FNorm,FCntr,[{V,Data}|VertexData],Acc,SmallestDist) ->
    {Vpos,Dir,Dist} = evaluate_data_3(FNorm,FCntr,Data),
    NewSmallest = if
      Dist < SmallestDist -> Dist;
      true -> SmallestDist
    end,
    evaluate_data_2(FNorm,FCntr,VertexData,[{V,{Vpos,Dir}}|Acc],NewSmallest).

evaluate_data_3(FNorm,FCntr,{Vpos,VDirA,VDirB}) ->
    ToCntr = e3d_vec:sub(FCntr,Vpos),
    CDB = e3d_vec:cross(VDirB,FNorm),
    CDA = e3d_vec:cross(FNorm,VDirA),
    IntersectionB = e3d_vec:dot(ToCntr,CDB),
    VectorB = e3d_vec:mul(CDB, IntersectionB),
    DistB = sqr_length(VectorB),
    Dot = e3d_vec:dot(CDA,CDB)+1,
    if
        Dot == 2 -> {Vpos,CDB,DistB};
        true ->
            IntersectionA = e3d_vec:dot(ToCntr,CDA),
            VectorA = e3d_vec:mul(CDA, IntersectionA),
            DistA = sqr_length(VectorA),
            Dir = e3d_vec:divide(e3d_vec:add(CDB,CDA),Dot),  % scale vector
            Dist = if
                DistA < DistB -> DistA;
                true -> DistB
                end,
            {Vpos,Dir,Dist}
    end.

offset_regions_fun(InsetData,State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          offset_regions_fun(InsetData, NewState);
      ([Dist, _, _, Bump|_], A) ->
        lists:foldl(fun({LoopNormal,VsData},VsAcc0) ->
            lists:foldl(fun
              ({V,{Vpos0,VNorm}},VsAcc) ->
                Vpos = bump_regions(Vpos0,LoopNormal,VNorm,State,-Bump),
                [{V,Vpos}|VsAcc];
              ({V,{Vpos0,Dir,EDir}},VsAcc) ->
                Vpos = inset_regions(Vpos0,Dir,EDir,State,Dist),
                [{V,Vpos}|VsAcc]
            end,VsAcc0,VsData)
        end,A,InsetData)
    end.

inset_regions_fun(InsetData,State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          inset_regions_fun(InsetData, NewState);
      ([Dist, _, _, Bump|_], A) ->
        lists:foldl(fun({LoopNormal,VsData},VsAcc0) ->
            lists:foldl(fun
              ({V,{Vpos0,VNorm}},VsAcc) ->
                Vpos = bump_regions(Vpos0,LoopNormal,VNorm,State,-Bump),
                [{V,Vpos}|VsAcc];
              ({V,{Vpos0,Dir,VNorm,EDir}},VsAcc) ->
                Vpos1 = inset_regions(Vpos0,Dir,EDir,State,Dist),
                Vpos = bump_regions(Vpos1,LoopNormal,VNorm,State,-Bump),
                [{V,Vpos}|VsAcc]
            end,VsAcc0,VsData)
        end,A,InsetData)
    end.

inset_faces_fun(InsetData,State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          inset_faces_fun(InsetData, NewState);
      ([Amount, _, _, Bump|_], A) ->
        {SmallestDistObj,VData} = InsetData,
        lists:foldl(fun({FNorm,SmallestDistF,VertexData},VsAcc0) ->
            lists:foldl(fun({Vs,Data},VsAcc) ->
                Vpos0 = inset_faces(SmallestDistObj,SmallestDistF,Data,State,Amount),
                Vpos = bump(Vpos0,FNorm,Bump),
                [{Vs,Vpos}|VsAcc]
            end,VsAcc0,VertexData)
        end,A,VData)
    end.

inset_regions(Vpos,_,_,_,0.0) ->
    Vpos;
inset_regions(Vpos,_Dir,EDir,{_,along_edges,_},Dist) ->
    e3d_vec:add(Vpos,e3d_vec:mul(EDir,Dist));
inset_regions(Vpos,Dir,_EDir,{_,average,_},Dist) ->
    e3d_vec:add(Vpos,e3d_vec:mul(Dir,Dist)).

bump_regions(Vpos,_,_,_,0.0) ->
    Vpos;
bump_regions(Vpos,LoopNormal,_,{_,_,loop},Bump) ->
    e3d_vec:add(Vpos,e3d_vec:mul(LoopNormal,Bump));
bump_regions(Vpos,_,VNorm,{_,_,faces},Bump) ->
    e3d_vec:add(Vpos,e3d_vec:mul(VNorm,Bump)).

inset_faces(_,_,{Vpos,_},_,0.0) ->
    Vpos;
inset_faces(SDist,_,{Vpos,Dir},{_,_,relative,_,per_obj},Percent) ->
    e3d_vec:add(Vpos, e3d_vec:mul(Dir, SDist * Percent));
inset_faces(_,SFDist,{Vpos,Dir},{_,_,relative,_,per_face},Percent) ->
    e3d_vec:add(Vpos, e3d_vec:mul(Dir, SFDist * Percent));
inset_faces(_,_,{Vpos,Dir},{_,along_edges,absolute,_,_},Dist) ->
    e3d_vec:add(Vpos, e3d_vec:mul(e3d_vec:norm(Dir), Dist));
inset_faces(_,_,{Vpos,Dir},{_,average,absolute,_,_},Dist) ->
    e3d_vec:add(Vpos, e3d_vec:mul(Dir, Dist)).

bump(Vpos,_,0.0) ->
    Vpos;
bump(Vpos,FNorm,Bump)->
    e3d_vec:add(Vpos, e3d_vec:mul(FNorm,Bump)).

loop_error_1() ->
    wings_u:error(?__(1,"Inset/Offset Region doesn't work for wholly selected objects")).
loop_error_2() ->
    wings_u:error(?__(1,"Offset Region requires that neighbouring faces\nshare at least one edge")).
