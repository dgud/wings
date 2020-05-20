%%
%%  wpc_inset.erl --
%%
%%  Face|Inset includes the commands: Inset, Inset Region, and Offset Region.
%%
%%  Copyright (c) 2008-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_inset).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

init() ->
    true.
menu({face},Menu) ->
    lists:reverse(parse(Menu, [], false));

menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [inset_menu()|NewMenu];
parse([A = {_,intrude,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,inset_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

%%%% Menus
inset_menu() ->
    Title = title(),
    HelpL = lmb_help(),
    HelpM = mmb_help(),
    HelpR = rmb_help(),
    Fun = fun
      (1,_Ns) -> {face,inset_faces};
      (2,_Ns) -> {face,offset_region};
      (3,_Ns) -> {face,inset_region};
      (_, _)  -> ignore
    end,
    {Title,{inset, Fun},{HelpL,HelpM,HelpR},[]}.

title() ->
    ?__(1,"Inset").
lmb_help() ->
    ?__(1,"Inset a face inside each selected face").
mmb_help() ->
    ?__(1,"Offset Region creating new edges around each face group selection").
rmb_help() ->
    ?__(1,"Inset Region creating new edges inside each face group selection").

%%%% Commands
command({face,Type}, St)
  when Type =:= inset_faces; Type =:= inset_region; Type =:= offset_region ->
    ?SLOW(inset_setup(Type, St));
command(_,_) ->
    next.

%%%% Setup
%% Start by Extruding the selection
inset_setup(inset_faces,St0) ->
    St = wings_face_cmd:extrude_faces(St0),
    inset_faces_setup(inset_faces, St);
inset_setup(inset_region, St0) ->
    St = wings_face_cmd:extrude_region(St0),
    inset_regions_setup(inset_region, St);
inset_setup(offset_region, St0) ->
    St = wings_face_cmd:extrude_region(St0),
    offset_regions_setup(offset_region, St).

offset_regions_setup(offset_region, St0) ->
    State = drag_mode(offset_region),
    St = wings_sel:map(
           fun(Faces0, We) ->
                   Faces = wings_sel_conv:more(face, Faces0, We),
                   FaceRegions = wings_sel:face_regions(Faces, We),
                   {AllVs0,VsData} =
                       collect_offset_regions_data(FaceRegions, We, [], []),
                   test_selection(AllVs0),
                   AllVs = ordsets:from_list(AllVs0),
                   Tv = {AllVs,offset_regions_fun(VsData, State)},
                   We#we{temp=Tv}
           end, St0),
    Flags = [{mode,{modes(),State}}],
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, drag_units(State), Flags, St).

inset_regions_setup(inset_region, St) ->
    State = drag_mode(inset_region),
    Flags = [{mode,{modes(),State}}],
    wings_drag:fold(
      fun(Faces, We) ->
              inset_regions_setup_1(Faces, We, State)
      end, drag_units(State), Flags, St).

inset_regions_setup_1(Faces, We, State) ->
    FaceRegions = wings_sel:face_regions(Faces, We),
    {AllVs0,VsData} = collect_inset_regions_data(FaceRegions, We, [], []),
    AllVs = ordsets:from_list(AllVs0),
    {AllVs,inset_regions_fun(VsData, State)}.

inset_faces_setup(inset_faces, St) ->
    State = drag_mode(inset_faces),
    Flags = [{mode,{modes(),State}}],
    wings_drag:fold(
      fun(Faces, We) ->
              inset_faces_setup_1(Faces, We, State)
      end, drag_units(State), Flags, St).

inset_faces_setup_1(Faces, We, State) ->
    {AllVs0,VData} = collect_inset_face_data(Faces, We, [], [], none),
    AllVs = ordsets:from_list(AllVs0),
    {AllVs,inset_faces_fun(VData, State)}.

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
    {Mode,Norm} = wings_pref:get_value(Type,{average,loop}),
    {Type,Mode,Norm}.

drag_units({inset_faces,_,absolute,_,_}) -> [distance,skip,bump];
drag_units({inset_faces,_,relative,continue,_}) -> [percent,skip,bump];
drag_units({inset_faces,_,relative,stop,_}) -> [{percent,{0.0,1.0}},skip,bump];
drag_units({_,_,_}) -> [distance,skip,bump].

modes() ->
    fun
      (help,State) -> mode_help(State);

      ({key,$3}, {inset_faces,Mode,relative,continue,Smallest}) ->
          {inset_faces,Mode,relative,stop,Smallest};
      ({key,$3}, {inset_faces,Mode,relative,stop,Smallest}) ->
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
      (done,{inset_region,Mode,Norm}) ->
          wings_pref:set_value(inset_region,{Mode,Norm});
      (done,{offset_region,Mode,Norm}) ->
          wings_pref:set_value(offset_region,{Mode,Norm});
      (_,_) -> none
    end.

key_press($1,inset_faces,Mode,relative,Stop,_) ->
    {inset_faces,Mode,relative,Stop,per_obj};
key_press($2,inset_faces,Mode,relative,Stop,_) ->
    {inset_faces,Mode,relative,Stop,per_face};
key_press($1,inset_faces,_,absolute,Stop,_) ->
    {inset_faces,average,absolute,Stop,per_obj};
key_press($2,inset_faces,_,absolute,Stop,_) ->
    {inset_faces,along_edges,absolute,Stop,per_face};
key_press(_,_,_,_,_,_) -> none.

key_press($1,Type,_,Norm) ->  {Type,average,Norm};
key_press($2,Type,_,Norm) ->  {Type,along_edges,Norm};
key_press($3,offset_region,_,Norm) -> {offset_region,scaled,Norm};
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


mode_help({offset_region,Mode,Norm}) ->
    Help = solution_type(Mode,[{"[1] ",average},{"  [2] ",along_edges},
            {"  [3] ",scaled}]),
    Divider = [{bold," | "}],
    Extrude = extrude_norm_help0(offset_region,Norm),
    Help ++ Divider ++ Extrude;

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
string(scaled) -> ?__(6,"Avg Along Edges");
string(per_obj) -> ?__(4,"Object");
string(per_face) -> ?__(5,"Face").

abs_rel_help(inset_faces, relative) -> "  [5] " ++ ?__(1,"Distance");
abs_rel_help(inset_faces, absolute) -> "  [5] " ++ ?__(2,"Percent").

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
    {LoopNorm,LoopVsData,LoopVs} = offset_regions_loop_data(OuterEdges,Faces,We,FaceNormTab),
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
    case array:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposA = array:get(Va,Vtab),
        VposB = array:get(Vb,Vtab),
        faces_data_2(NextEdge,Face,Edge,Etab,Vtab,[VposB,VposA],[Edge|EAcc],[Vb|Vs]);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposA = array:get(Va,Vtab),
        VposB = array:get(Vb,Vtab),
        faces_data_2(NextEdge,Face,Edge,Etab,Vtab,[VposA,VposB],[Edge|EAcc],[Va|Vs])
    end.

faces_data_2(LastEdge,Face,LastEdge,_,_,Vp,EAcc,Vs) ->
    {{Face,e3d_vec:normal(Vp)},EAcc,Vs};

faces_data_2(Edge,Face,LastEdge,Etab,Vtab,Vp,EAcc,Vs) ->
    case array:get(Edge,Etab) of
      #edge{ve=V,lf=Face,ltpr=NextEdge} ->
        Vpos = array:get(V,Vtab),
        faces_data_2(NextEdge,Face,LastEdge,Etab,Vtab,[Vpos|Vp],[Edge|EAcc],[V|Vs]);
      #edge{vs=V,rf=Face,rtpr=NextEdge} ->
        Vpos = array:get(V,Vtab),
        faces_data_2(NextEdge,Face,LastEdge,Etab,Vtab,[Vpos|Vp],[Edge|EAcc],[V|Vs])
    end.

outer_edges_1([E,E|T],Out) ->
    outer_edges_1(T,Out);
outer_edges_1([E|T],Out) ->
    outer_edges_1(T,[E|Out]);
outer_edges_1([],Out) -> Out.

%%%% Return data for Inset Region cmd, including the average normal for one or
%%%% multiple eloops in a face region and collect vector data for the eloop
%%%% vertices.
inset_regions_loop_data([], _, _) ->
    loop_error_1();
inset_regions_loop_data(Edges,FNtab,We) ->
    EdgeSet = gb_sets:from_list(Edges),
    loop_vertices_data_0(EdgeSet,FNtab,We,[],[],[]).

loop_vertices_data_0(EdgeSet0,FNtab,#we{es=Etab,vp=Vtab,mirror=M}=We,LNorms,VData0,Vs0) ->
    case gb_sets:is_empty(EdgeSet0) of
      false ->
        {Edge,EdgeSet1} = gb_sets:take_smallest(EdgeSet0),
        {EdgeSet,VData,Links,LoopNorm,Vs} = loop_vertices_data_1(Edge,EdgeSet1,FNtab,Etab,Vtab,M,VData0,Vs0),
        loop_vertices_data_0(EdgeSet,FNtab,We,[{Links,LoopNorm}|LNorms],VData,Vs);
      true ->
        AvgLoopNorm = average_loop_norm(LNorms),
        {AvgLoopNorm,VData0,Vs0}
    end.

loop_vertices_data_1(Edge,EdgeSet,FNtab,Etab,Vtab,M,VData,Vs) ->
    #edge{vs=Va,ve=Vb,rf=Rf,rtpr=NextEdge} = array:get(Edge, Etab),
    VposA = array:get(Va,Vtab),
    VposB = array:get(Vb,Vtab),
    FNorm = gb_trees:get(Rf,FNtab),
    VDir = e3d_vec:sub(VposB,VposA),
    EdgeData = array:get(NextEdge,Etab),
    loop_vertices_data_2(NextEdge,EdgeData,Va,VposA,Rf,Edge,FNtab,Etab,Vtab,M,
        EdgeSet,VDir,[],[FNorm],VData,[],Vs,0).

loop_vertices_data_2(LastE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevFace},
      Vb,VposB,PrevFace,LastE,_,_,Vtab,M,EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    VposA = array:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    VNormal = e3d_vec:norm(e3d_vec:add(VNorms)),
    VData = case M of
      none ->
        EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
        [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0];
      Face ->
        Vec = e3d_vec:norm(Dir),
        [{Vb,{VposB,evaluate_mirror_vdata(Dir,VDir),VNormal,Vec}}|VData0];
      {Face,_} ->
        Vec = average_edge_dir(VNormal,VDir,Dir,[]),
        [{Vb,{VposB,Vec}}|VData0];
      {_,Vb} ->
        Vec = e3d_vec:norm(VDir),
        [{Vb,{VposB,evaluate_mirror_vdata(VDir,Dir),VNormal,Vec}}|VData0];
      _Otherwise ->
        EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
        [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0]
      end,
    LoopNorm = e3d_vec:normal([VposB|VPs]),
    Vs = [Vb|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

loop_vertices_data_2(CurE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevFace,ltpr=NextEdge,rtpr=IfNoFaceEdge},
      Vb,VposB,PrevFace,LastE,FNtab,Etab,Vtab,M,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposA = array:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    case  gb_trees:lookup(Face,FNtab) of
      none ->
        EdgeSet = gb_sets:delete(CurE,EdgeSet0),
        VNormal = e3d_vec:norm(e3d_vec:add(VNorms0)),
        {VData,Mir} = case M of
          none ->
            EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
            VData1 = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0],
            {VData1,M};
          Face ->
            Vec = e3d_vec:norm(Dir),
            FC = case array:get(NextEdge,Etab) of
              #edge{vs=Vb,rf=F} -> F;
              #edge{ve=Vb,lf=F} -> F
            end,
            case gb_trees:is_defined(FC,FNtab) of
              true ->
                VData1 = [{Vb,{VposB,Vec}}|VData0],
                {VData1,{M,Va}};
              false ->
                VData1 = [{Vb,{VposB,evaluate_mirror_vdata(Dir,VDir),VNormal,Vec}}|VData0],
                {VData1,{M,Va}}
            end;
          {Face,_} ->
            Vec = average_edge_dir(VNormal,VDir,Dir,[]),
            VData1 = [{Vb,{VposB,Vec}}|VData0],
            {VData1,{Face,Va}};
          {Mirror,Vb} ->
            Vec = e3d_vec:norm(VDir),
            VData1 = [{Vb,{VposB,evaluate_mirror_vdata(VDir,Dir),VNormal,Vec}}|VData0],
            {VData1,Mirror};
          _Otherwise ->
            EDir = average_edge_dir(VNormal,VDir,Dir,EDir0),
            VData1 = [{Vb,{VposB,evaluate_vdata(VDir,Dir,VNormal),VNormal,EDir}}|VData0],
            {VData1,M}
        end,
        NextVDir = e3d_vec:neg(Dir),
        EdgeData = array:get(IfNoFaceEdge,Etab),
        [FNorm|_] = VNorms0,
        VPs = [VposB|VPs0],
        Vs = [Vb|Vs0],
        loop_vertices_data_2(IfNoFaceEdge,EdgeData,Va,VposA,PrevFace,LastE,FNtab,Etab,Vtab,Mir,
            EdgeSet,NextVDir,[],[FNorm],VData,VPs,Vs,Links+1);
      {value,FNorm} ->
        EdgeData = array:get(NextEdge,Etab),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        loop_vertices_data_2(NextEdge,EdgeData,Vb,VposB,Face,LastE,FNtab,Etab,Vtab,M,
            EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end;

loop_vertices_data_2(_CurE,#edge{vs=Va,ve=Vb,lf=PrevFace,rf=Face,rtpr=NextEdge},
      Va,VposA,PrevFace,LastE,FNtab,Etab,Vtab,M,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    VposB = array:get(Vb,Vtab),
    Dir = e3d_vec:sub(VposB,VposA),
    FNorm = gb_trees:get(Face,FNtab),
    EdgeData = array:get(NextEdge,Etab),
    EDirs = [Dir|EDir0],
    VNorms = [FNorm|VNorms0],
    loop_vertices_data_2(NextEdge,EdgeData,Va,VposA,Face,LastE,FNtab,Etab,Vtab,M,
        EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links).

%%%% Return data for Offset Region cmd, including the average normal for one or
%%%% multiple eloops in a face region and collect vector data for the eloop
%%%% vertices
offset_regions_loop_data([],_,_,_) ->
    loop_error_1();
offset_regions_loop_data(Edges,Faces,We,FNtab) ->
    EdgeSet = gb_sets:from_list(Edges),
    offset_loop_data_0(EdgeSet,Faces,We,FNtab,[],[],[]).

offset_loop_data_0(EdgeSet0,Faces,We,FNtab,LNorms,VData0,Vs0) ->
    case gb_sets:is_empty(EdgeSet0) of
      false ->
        {Edge,EdgeSet1} = gb_sets:take_smallest(EdgeSet0),
        {EdgeSet,VData,Links,LoopNorm,Vs} = offset_loop_data_1(Edge,EdgeSet1,Faces,We,FNtab,VData0,Vs0),
        offset_loop_data_0(EdgeSet,Faces,We,FNtab,[{Links,LoopNorm}|LNorms],VData,Vs);
      true ->
        AvgLoopNorm = average_loop_norm(LNorms),
        {AvgLoopNorm,VData0,Vs0}
    end.

offset_loop_data_1(Edge,EdgeSet,Faces,#we{es=Etab,vp=Vtab}=We,FNtab,VData,Vs) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltsu=NextLeft,rtsu=NextRight} = array:get(Edge,Etab),
    VposA = array:get(Va,Vtab),
    VposB = array:get(Vb,Vtab),
    case gb_sets:is_member(Rf,Faces) of
      true ->
        VDir = e3d_vec:sub(VposB,VposA),
        FNorm = wings_face:normal(Lf,We),
        EdgeData = array:get(NextLeft,Etab),
        offset_loop_data_2(NextLeft,EdgeData,Va,VposA,Lf,Edge,We,FNtab,
            EdgeSet,VDir,[],[FNorm],VData,[],Vs,0);
      false ->
        VDir = e3d_vec:sub(VposA,VposB),
        FNorm = wings_face:normal(Rf,We),
        EdgeData = array:get(NextRight,Etab),
        offset_loop_data_2(NextRight,EdgeData,Vb,VposB,Rf,Edge,We,FNtab,
            EdgeSet,VDir,[],[FNorm],VData,[],Vs,0)
    end.

offset_loop_data_2(LastE,#edge{vs=Va,ve=Vb,lf=PrevFace,rtsu=NextEdge},
        Vb,VposB,PrevFace,LastE,We,FNtab,EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    #we{vp=Vtab,mirror=M} = We,
    Mirror = M == PrevFace,
    VposA = array:get(Va,Vtab),
    Dir = e3d_vec:sub(VposA,VposB),
    VData = offset_loop_data_3(Mirror,Vb,VposB,VNorms,NextEdge,VDir,Dir,EDir0,FNtab,We,VData0),
    LoopNorm = e3d_vec:normal([VposB|VPs]),
    Vs = [Vb|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

offset_loop_data_2(LastE,#edge{vs=Va,ve=Vb,rf=PrevFace,ltsu=NextEdge},
        Va,VposA,PrevFace,LastE,We,FNtab,EdgeSet,VDir,EDir0,VNorms,VData0,VPs,Vs0,Links) ->
    #we{vp=Vtab,mirror=M} = We,
    Mirror = M == PrevFace,
    VposB = array:get(Vb,Vtab),
    Dir = e3d_vec:sub(VposB,VposA),
    VData = offset_loop_data_3(Mirror,Va,VposA,VNorms,NextEdge,VDir,Dir,EDir0,FNtab,We,VData0),
    LoopNorm = e3d_vec:normal([VposA|VPs]),
    Vs = [Va|Vs0],
    {EdgeSet,VData,Links+1,LoopNorm,Vs};

offset_loop_data_2(CurE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevFace,ltsu=NextEdge,rtsu=IfCurIsMember},
        Va,VposA,PrevFace,LastE,#we{mirror=M}=We,FNtab,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    case gb_sets:is_member(CurE,EdgeSet0) of
      true ->
        Mirror = M == PrevFace,
        offset_loop_is_member(Mirror,Va,Vb,VposA,CurE,IfCurIsMember,VNorms0,NextEdge,
            EdgeSet0,VDir,EDir0,FNtab,PrevFace,LastE,We,VData0,VPs0,Vs0,Links);
      false ->
        #we{es=Etab,vp=Vtab} = We,
        EdgeData0 = array:get(NextEdge,Etab),
        VposB = array:get(Vb,Vtab),
        Dir = e3d_vec:sub(VposB,VposA),
        FNorm = wings_face:normal(Face,We),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        offset_loop_data_2(NextEdge,EdgeData0,Va,VposA,Face,LastE,We,FNtab,
            EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end;

offset_loop_data_2(CurE,#edge{vs=Va,ve=Vb,lf=PrevFace,rf=Face,rtsu=NextEdge,ltsu=IfCurIsMember},
        Vb,VposB,PrevFace,LastE,#we{mirror=M}=We,FNtab,EdgeSet0,VDir,EDir0,VNorms0,VData0,VPs0,Vs0,Links) ->
    case gb_sets:is_member(CurE,EdgeSet0) of
      true ->
        Mirror = M == PrevFace,
        offset_loop_is_member(Mirror,Vb,Va,VposB,CurE,IfCurIsMember,VNorms0,NextEdge,
            EdgeSet0,VDir,EDir0,FNtab,PrevFace,LastE,We,VData0,VPs0,Vs0,Links);
      false ->
        #we{es=Etab,vp=Vtab} = We,
        EdgeData0 = array:get(NextEdge,Etab),
        VposA = array:get(Va,Vtab),
        Dir = e3d_vec:sub(VposA,VposB),
        FNorm = wings_face:normal(Face,We),
        EDirs = [Dir|EDir0],
        VNorms = [FNorm|VNorms0],
        offset_loop_data_2(NextEdge,EdgeData0,Vb,VposB,Face,LastE,We,FNtab,
            EdgeSet0,VDir,EDirs,VNorms,VData0,VPs0,Vs0,Links)
    end.

offset_loop_data_3(false,V,Vpos,VNorms0,NextEdge,VDir,Dir,EDir0,FNtab,We,VData0) ->
    #we{es=Etab} = We,
    VNorm = e3d_vec:norm(e3d_vec:add(VNorms0)),
    NV = wings_vertex:other(V,array:get(NextEdge,Etab)),
    ANorm = vertex_normal(NV,FNtab,We),
    EDir = average_edge_dir(VNorm,VDir,Dir,EDir0),
    AvgDir = evaluate_vdata(VDir,Dir,VNorm),
    ScaledDir = along_edge_scale_factor(VDir,Dir,EDir,ANorm),
    [{V,{Vpos,AvgDir,EDir,ScaledDir}}|VData0];

offset_loop_data_3(true,V,Vpos,VNorms0,NE,VDir,Dir,EDir0,FNtab,#we{es=Etab},VData0) ->
    Face = case array:get(NE,Etab) of
      #edge{vs=V,lf=IsFaceInc} -> IsFaceInc;
      #edge{ve=V,rf=IsFaceInc} -> IsFaceInc
    end,
    case gb_trees:is_defined(Face,FNtab) of
    true ->
      VNorm = e3d_vec:norm(e3d_vec:add(VNorms0)),
      Vec = average_edge_dir(VNorm,VDir,Dir,[]),
      [{V,{Vpos,Vec}}|VData0];
    false ->
      VNorm = e3d_vec:norm(e3d_vec:add(VNorms0)),
      EDir = average_edge_dir(VNorm,VDir,Dir,EDir0),
      AvgDir = evaluate_vdata(VDir,Dir,VNorm),
      ScaledDir = along_edge_scale_factor(VDir,Dir,EDir,VNorms0),
      [{V,{Vpos,AvgDir,EDir,ScaledDir}}|VData0]
    end.

offset_loop_is_member(Mirror,V1,V2,Vpos1,CurE,NextE,VNorms0,NEdge,EdgeSet0,VDir,
        EDir0,FNtab,PFace,LastE,We,VData0,VPs0,Vs0,Links) ->
    #we{es=Etab,vp=Vtab} = We,
    Vpos2 = array:get(V2,Vtab),
    Dir = e3d_vec:sub(Vpos2,Vpos1),
    NextVDir = e3d_vec:neg(Dir),
    EdgeSet = gb_sets:delete(CurE,EdgeSet0),
    EdgeData = array:get(NextE,Etab),
    [FNorm|_] = VNorms0,
    VData = offset_loop_data_3(Mirror,V1,Vpos1,VNorms0,NEdge,VDir,Dir,EDir0,
            FNtab,We,VData0),
    VPs = [Vpos1|VPs0],
    Vs = [V1|Vs0],
    offset_loop_data_2(NextE,EdgeData,V2,Vpos2,PFace,LastE,We,FNtab,
        EdgeSet,NextVDir,[],[FNorm],VData,VPs,Vs,Links+1).

%%%%
along_edge_scale_factor(DirA,DirB,EDir,[ANorm|Ns]=Norms) ->
    Dot = abs(e3d_vec:dot(ANorm,EDir)),
    along_edge_sf(DirA,DirB,EDir,Ns,Dot,Norms).

along_edge_sf(DirA,DirB,EDir,[ANorm|Ns],Dot0,Norms) ->
    Dot = abs(e3d_vec:dot(ANorm,EDir)),
    case Dot > Dot0 of
      true -> along_edge_sf(DirA,DirB,EDir,Ns,Dot,Norms);
      false -> along_edge_sf(DirA,DirB,EDir,Ns,Dot0,Norms)
    end;
along_edge_sf(DirA,DirB,EDir,[],Dot,[N|Norms]) ->
    case Dot < 1.0E-12 of
        true ->
            Dot0 = along_edge_sf_2(DirA,DirB,EDir,N),
            along_edge_sf_3(DirA,DirB,EDir,Norms,Dot0);
        false -> e3d_vec:divide(EDir,Dot)
    end.

along_edge_sf_2(DirA,DirB,EDir,N) ->
    A = e3d_vec:norm(e3d_vec:cross(N,DirA)),
    B = e3d_vec:norm(e3d_vec:cross(DirB,N)),
    DotA = abs(e3d_vec:dot(A,EDir)),
    DotB = abs(e3d_vec:dot(B,EDir)),
    case DotA > DotB of
      true -> DotA;
      false -> DotB
    end.

along_edge_sf_3(DirA,DirB,EDir,[N|Norms],Dot0) ->
    Dot = along_edge_sf_2(DirA,DirB,EDir,N),
    case Dot > Dot0 of
      true -> along_edge_sf_3(DirA,DirB,EDir,Norms,Dot);
      false -> along_edge_sf_3(DirA,DirB,EDir,Norms,Dot0)
    end;

along_edge_sf_3(_,_,EDir,[],Dot) ->
    case Dot < 1.0E-12 of
        true -> EDir;
        false -> e3d_vec:divide(EDir,Dot)
    end.

%%%%
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

%%%% Get vertex normals from FaceNormal gb_tree after listing all the faces
%%%% surrounding a vertex
vertex_normals([V|Vs],FaceNormTab,#we{vp=Vtab,mirror=M}=We,Acc) ->
    FaceNorms = wings_vertex:fold(fun
        (_,Face,_,A) when Face == M -> [e3d_vec:neg(wings_face:normal(M,We))|A];
        (_,Face,_,A) -> [gb_trees:get(Face,FaceNormTab)|A]
    end,[],V,We),
    VNorm = e3d_vec:norm(e3d_vec:add(FaceNorms)),
    Vpos = array:get(V,Vtab),
    vertex_normals(Vs,FaceNormTab,We,[{V,{Vpos,VNorm}}|Acc]);
vertex_normals([],_,_,Acc) -> Acc.

vertex_normal(V,FaceNormTab,#we{mirror=M}=We) ->
    wings_vertex:fold(fun
        (_,Face,_,A) when Face == M ->
            [e3d_vec:neg(wings_face:normal(Face,We))|A];
        (_,Face,_,A) ->
            N = gb_trees:get(Face,FaceNormTab),
            case e3d_vec:is_zero(N) of
              true -> A;
              false -> [N|A]
            end
    end,[],V,We).

%%%%
evaluate_vdata(DirA,DirB,VNorm) ->
    A = e3d_vec:norm(e3d_vec:cross(VNorm,DirA)),
    B = e3d_vec:norm(e3d_vec:cross(DirB,VNorm)),
    Dot = e3d_vec:dot(A,B)+1,
    Vec = e3d_vec:add(A,B),
    case Dot < 1.0E-12 of
      true -> Vec;
      false -> e3d_vec:divide(Vec,Dot)
    end.

%%%%
evaluate_mirror_vdata(VDir,Dir) ->
    Dot = e3d_vec:dot(Dir,VDir)+1,
    case Dot < 1.0E-12 of
      false -> e3d_vec:divide(e3d_vec:norm(VDir),Dot);
      true -> e3d_vec:norm(VDir)
    end.

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
    case array:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposA = array:get(Va,Vtab),
        VposB = array:get(Vb,Vtab),
        VDir = e3d_vec:norm_sub(VposB,VposA),
        traverse_2(NextEdge,Face,VDir,VposB,Edge,Etab,Vtab,{Va,{VposA,VDir}},[],AllVs,[]);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposB = array:get(Vb,Vtab),
        VposA = array:get(Va,Vtab),
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
    case array:get(Edge,Etab) of
      #edge{vs=Va,ve=Vb,lf=Face,ltpr=NextEdge} ->
        VposB = array:get(Vb, Vtab),
        VDirB = e3d_vec:norm_sub(Vpos,VposB),
        VDirA = e3d_vec:neg(PrevVDir),
        VData = [{Va,{Vpos,VDirB,VDirA}}|Acc],
        VPositions = [Vpos|VPositions0],
        AllVs = [Va|AllVs0],
        traverse_2(NextEdge,Face,VDirB,VposB,LastEdge,Etab,Vtab,LastVert,VPositions,AllVs,VData);
      #edge{vs=Va,ve=Vb,rf=Face,rtpr=NextEdge} ->
        VposA = array:get(Va, Vtab),
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
        Dot < 1.0E-12 -> {Vpos,e3d_vec:add(CDB,CDA),DistB};
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

offset_regions_fun(OffsetData,{_,Solution,_}=State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          offset_regions_fun(OffsetData, NewState);
      ([Dist, _, Bump|_], A) ->
        lists:foldl(fun({LoopNormal,VsData},VsAcc0) ->
            lists:foldl(fun
              ({V,{Vpos0,VNorm}},VsAcc) ->
                Vpos = bump_regions(Vpos0,LoopNormal,VNorm,State,-Bump),
                [{V,Vpos}|VsAcc];
              ({V,{Vpos0,Dir,EDir,ScaledEDir}},VsAcc) ->
                Vec = case Solution of
                  average -> Dir;
                  along_edges -> EDir;
                  scaled -> ScaledEDir
                end,
                Vpos = offset_regions(Vpos0,Vec,Dist),
                [{V,Vpos}|VsAcc]
            end,VsAcc0,VsData)
        end,A,OffsetData)
    end.

inset_regions_fun(InsetData,State) ->
    fun
      (new_mode_data, {NewState,_}) ->
          inset_regions_fun(InsetData, NewState);
      ([Dist, _, Bump|_], A) ->
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
      ([Amount, _, Bump|_], A) ->
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

offset_regions(Vpos,_,0.0) ->
    Vpos;
offset_regions(Vpos,Vec,Dist) ->
    e3d_vec:add(Vpos,e3d_vec:mul(Vec,Dist)).

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

-spec loop_error_1() -> no_return().
loop_error_1() ->
    wings_u:error_msg(?__(1,"Inset/Offset Region doesn't work for wholly selected objects")).

-spec loop_error_2() -> no_return().
loop_error_2() ->
    wings_u:error_msg(?__(1,"Offset Region requires that neighbouring faces\nshare at least one edge")).
