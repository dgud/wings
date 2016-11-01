%%%-------------------------------------------------------------------
%%% File    : auv_segment.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Different segmentation algorithms.
%%%               Segments Model into set of charts containg faces.
%%% Created :  3 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2011 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id$

-module(auv_segment).

-export([create/2,segment_by_material/1,cut_model/3,
	 normalize_charts/3,map_vertex/2,map_edge/2,
	 fv_to_uv_map/2,uv_to_charts/3]).

-ifdef(DEBUG).
-export([degrees/0, find_features/3, build_seeds/2]). %% Debugging
-endif.

-include_lib("wings/src/wings.hrl").
-include("auv.hrl").

-import(lists, [reverse/1,mapfoldl/3,sort/1,foldl/3]).

%% Returns segments=[Charts={Id,[Faces]}] and Bounds=gb_sets([Edges])
create(Mode, We0) ->
    case Mode of 
	feature ->
	    Tot = wings_util:array_entries(We0#we.es),
	    {_Distances,Charts0,Cuts0,_Feats} = 
		segment_by_feature(We0, edge_sharpness(), Tot div 50),
	    {Charts0, Cuts0};
	autouvmap ->
	    Charts0 = segment_by_direction(We0),
	    {Charts0, gb_sets:empty()}
    end.

edge_sharpness() ->
    %% Fool dialyzer to think this is not hard coded
    case is_process_alive(self()) of
        true -> 60;
        false -> rand:uniform(100)
    end.

%%%%%% Feature detection Algorithm %%%%%%%%%%%%
%% Algorithms based on the paper, now probably totally ruined by me...
%% 'Least Square Conformal Maps for Automatic Texture Generation Atlas'
%% by Bruno Levy, Sylvain Petitjean, Nicolas Ray, Jerome Mailot.
%% Presented on Siggraph 2002.

-define(MAX_STRING_LENGTH, 5).
-define(MIN_SHARPNESS(MAX), (1 - MAX) * (?MAX_STRING_LENGTH -1)).

-record(restr, {sharp, featl}).

%% Debug func %%
-ifdef(DEBUG).
degrees() ->
    Test = fun(D) ->
		   X = (math:cos(D * math:pi() / 180) + 1) / 2, 
		   Y = math:sin(D *  math:pi() / 180) /2,    
		   Dir = math:sqrt(X*X+Y*Y),
		   io:format("~.3w "++?__(1,"deg")++" -> ~w ~w~n", [D, Dir, 1 - Dir])
	   end,
    lists:foreach(Test, lists:seq(0,360,15)).
-endif.

degrees(Deg) ->
    X = (math:cos(Deg * math:pi() / 180) + 1) / 2, 
    Y = math:sin(Deg *  math:pi() / 180) /2,    
    math:sqrt(X*X+Y*Y).

segment_by_feature(We, SharpEdgeDeg, MinFeatureLen) ->
    {Features,VEG,EWs} = find_features(We, SharpEdgeDeg, MinFeatureLen),
    {LocalMaxs,Extra} = build_seeds(Features, We),
    {Distances,Charts0,Cuts} = build_charts(LocalMaxs, Extra, VEG, EWs, We),    
    Charts = solve_map_problem(Charts0, We),
    {Distances,Charts,Cuts,Features}.


solve_map_problem(Charts0, _We) when length(Charts0) =< 9 ->
    Charts0;

solve_map_problem(Charts0, We) ->
    Charts = auv_util:number(Charts0),
    FamC2F = sofs:family(Charts),
    RelC2F = sofs:family_to_relation(FamC2F),
    FaceChart0 = sofs:to_external(sofs:converse(RelC2F)),
    Face2Chart  = gb_trees:from_orddict(lists:sort(FaceChart0)),
    EdgesInCharts = 
	lists:foldl(fun({Id, Faces}, Acc) -> 
			    [{Id, wings_face:outer_edges(Faces, We)} | Acc]
		    end, [], Charts),
    Neighbours0 = neighbour_charts(EdgesInCharts, Face2Chart, We#we.es, []),
    Map = color_map(Neighbours0, gb_trees:empty(), gb_sets:empty(), 0),
    MapC2Col = sofs:relation(Map),
    T0 = sofs:range(sofs:relative_product([MapC2Col, FamC2F])),
    T1 = sofs:range(sofs:family_union(sofs:relation_to_family(T0))),
    T2 = sofs:to_external(T1),
    ?DBG("Map painting solved from ~p to ~p colors~n", [length(Charts0), length(T2)]), 
    T2.
    

color_map([{_, Id, NBeds}|Rest], Map0, Cols, NextCol) ->
    NBcols = lists:foldl(fun(Neigh, Acc) ->
				 case gb_trees:lookup(Neigh, Map0) of
				     none -> Acc;
				     {value, Col} ->
					 gb_sets:add(Col, Acc)
				 end
			 end, gb_sets:empty(), NBeds),
    Avail = gb_sets:difference(Cols, NBcols),
    case gb_sets:is_empty(Avail) of
	true ->
	    color_map(Rest, gb_trees:insert(Id, NextCol, Map0), 
		      gb_sets:insert(NextCol,Cols), NextCol +1);
	false ->
	    {First,_} = gb_sets:take_smallest(Avail),
	    color_map(Rest, gb_trees:insert(Id, First, Map0), Cols, NextCol)
    end;
color_map([], Map, _, _) ->
    gb_trees:to_list(Map).

neighbour_charts([{Id, Edges}|Rest], Face2Chart, Etab, Acc) ->
    Neighb0 = foldl(fun(Edge, Acc0) ->
			    #edge{lf=LF, rf=RF} = array:get(Edge, Etab),
			   case gb_trees:get(RF, Face2Chart) of
			       Id ->
				   [gb_trees:get(LF, Face2Chart)|Acc0];
			       Chart ->
				   [Chart|Acc0]
			   end
		    end, [], Edges),
    Neighb = lists:usort(Neighb0),
    neighbour_charts(Rest, Face2Chart, Etab, [{length(Neighb), Id, Neighb}|Acc]);
neighbour_charts([], _, _, Acc) ->
    reverse(sort(Acc)).

%%%   SharpEdge should be degrees value is: (180 +/- SharpEdge) 
%%%   MinFeatureLen is the number of edges a feature must contain
find_features(We, SharpEdge, MinFeatureLen) when MinFeatureLen > 20 ->
    find_features(We, SharpEdge, 20);
find_features(We, SharpEdge, MinFeatureLen) when MinFeatureLen < 2 ->
    find_features(We, SharpEdge, 2);
find_features(We, SharpEdge, MinFeatureLen) when SharpEdge > 90 ->
    find_features(We, 90, MinFeatureLen);
find_features(We, SharpEdge, MinFeatureLen) when SharpEdge < 2 ->
    find_features(We, 10, MinFeatureLen);    
find_features(We0, SharpEdgeDeg, MinFeatureLen) ->
    Restrs = #restr{sharp = ?MIN_SHARPNESS(degrees(SharpEdgeDeg)), 
		    featl=MinFeatureLen},
    {Sorted, N, _FNormals} = sort_edges_by_weight(We0),
    %% split list normals may diff with +-60 deg and 20% of the edges
    {Best, _Rest} = pick_features(Sorted, 0, degrees(SharpEdgeDeg), N, []),
%%    ?DBG("Best ~p ~p ~n", [Best, _Rest]),
    EVGraph = build_vertex_graph(Sorted, We0, gb_trees:empty()),
    EWs    = gb_trees:from_orddict(lists:sort(Sorted)),

    Features0 = expand_features(Best, EVGraph, EWs, We0, Restrs),
    {Features0, EVGraph, EWs}.

sort_edges_by_weight(We0 = #we{fs = Ftab, es = Etab}) ->
    Faces = gb_trees:keys(Ftab),
    FaceNormals0 = lists:map(fun(Face) ->
				     {Face,wings_face:normal(Face, We0)}
			     end, Faces),
    FaceNormals = gb_trees:from_orddict(FaceNormals0),
    Edges  = wings_util:array_keys(Etab),
    WEdges = lists:map(fun(Edge) ->
			       Val = calc_normal_value(Edge, FaceNormals, We0),
			       {Edge, Val}
		       end, Edges),
    {lists:keysort(2,WEdges), length(WEdges), FaceNormals}.

calc_normal_value(Edge, FaceData, We) ->
    #edge{lf = LF, rf = RF} = array:get(Edge, We#we.es),
    calc_dir_vector(gb_trees:get(LF,FaceData), gb_trees:get(RF,FaceData)).

pick_features([Edge = {_,Value}|Rest], N, Constraint, Max, Acc) 
  when Value < Constraint, N < Max -> 
    pick_features(Rest, N+1, Constraint, Max, [Edge|Acc]);
pick_features(Rest, _,_,_, Acc) ->
    {lists:reverse(Acc), Rest}.

-record(fd, {graph, vals, neigh, feat}).
-define(fdcreate(VerG,Vals), #fd{graph=VerG, vals=Vals, neigh=gb_sets:empty(), feat=[]}).
-define(fdmember(Edge,Fd), gb_sets:is_member(Edge, Fd#fd.neigh)).
-define(fdisneigh(Edge,Fd),  gb_sets:is_member(Edge, Fd#fd.neigh)).
-define(fdgetfeat(Fd), Fd#fd.feat).
-define(fdgetsurrneigh(EdgVer, Fd), gb_trees:get(EdgVer,Fd#fd.graph)).
-define(fdgetvalue(Edge, Fd), gb_trees:get(Edge,Fd#fd.vals)).
-define(fdmarkused(Edge, Fd), Fd#fd{neigh=gb_sets:add(Edge, Fd#fd.neigh)}).
fdaddneighandfeat(Feat, We, FdXX) ->
    FindXX = fun(EdgeXX, AccXX) ->			 
		     #edge{vs=Vs,ve=Ve} = array:get(EdgeXX, We#we.es),
		     Neigh1 = ?fdgetsurrneigh({EdgeXX,Vs}, FdXX),
		     Neigh2 = ?fdgetsurrneigh({EdgeXX,Ve}, FdXX),
		     (Neigh1 ++ Neigh2) ++ AccXX
	     end,		    
    PossNeighXX = gb_sets:from_list(lists:foldl(FindXX,[], Feat)),
    NewNBXX = gb_sets:union(PossNeighXX, FdXX#fd.neigh),
    FdXX#fd{neigh = NewNBXX, feat = Feat ++ FdXX#fd.feat}.

expand_features(Possible, EVGr, EdgeCost, We, Restrs) ->
    expand_features(Possible, ?fdcreate(EVGr, EdgeCost), We, Restrs).

expand_features([First = {Edge, _}|Rest], Fd = #fd{}, We, Restrs) ->
    case ?fdmember(Edge,Fd) of
	true -> %% Already used ignore
	    expand_features(Rest, Fd, We, Restrs);
	false ->
	    {Feature, Fd1} = expand_feature_curve(First, Fd, We, Restrs),
%%	    ?DBG("Expand ~p ~p ~p ~p~n", [First, Restrs#restr.featl, length(Feature), Feature]),
	    if 
		length(Feature) < Restrs#restr.featl ->
		    expand_features(Rest, Fd, We,Restrs);
		true -> %% Accepted feature
		    %% Add neighbors
		    Fd2 = fdaddneighandfeat(Feature,We,Fd1),
		    expand_features(Rest, Fd2, We, Restrs)
	    end
    end;
expand_features([], Fd, _We, _Restrs) ->
%    ?DBG("Expand done~n"),
    ?fdgetfeat(Fd).

expand_feature_curve({Edge, _Sharpness}, Fd0, We, Rs) ->
    #edge{vs = Vs, ve = Ve} = array:get(Edge, We#we.es),
    Dir1 = get_vector(Vs,Ve,We),
    {Edges1,_}   = get_edges(Vs, Edge, Dir1, We, Fd0),
    {Feat0, Fd2}=depth_traverse_tree([Edges1],0,0,Dir1,Fd0,We,Rs,[Edge]), 
    Dir2 = get_vector(Ve,Vs,We),
    {Edges2,_} = get_edges(Ve, Edge, Dir2, We, Fd2),
    depth_traverse_tree([Edges2],0,0,Dir2,Fd2,We,Rs,Feat0).

depth_traverse_tree(Tree=[[{Val,_,_}|_]|_],Sharp,Depth,_Dir1,Fd0,We,Rs,Feat) 
  when Sharp + Val > Rs#restr.sharp ->
    %% Found suiteable edge -> add to feat
    [[{Val0,{Edge,#edge{vs=VaN,ve=VbN}},V0}|_]|Found] = lists:reverse(Tree),
%%    ?DBG("Found suiteable edge ~p ~p ~p ~n", [Edge,Sharp,Depth]),
    Fd1 = ?fdmarkused(Edge, Fd0),
    case Found of 
	[] -> %% Oops first level hit, restart (special case)
%	    ?DBG("Special case ~p ~n", [Tree]),
	    NextV = if V0 == VaN -> VbN; V0 == VbN -> VaN end,
	    Dir2 = get_vector(VaN,VbN,V0,We),
	    {Edges1,_} = get_edges(NextV, Edge, Dir2, We, Fd1),
	    depth_traverse_tree([Edges1], 0, 0, Dir2, Fd1, We, Rs, 
				[Edge|Feat]);
	[[{_,{_,#edge{vs=Va,ve=Vb}},V}|_]|_] ->
	    Dir2 = get_vector(Va,Vb,V,We),
	    depth_traverse_tree(lists:reverse(Found), Sharp - Val0, 
				Depth -1,Dir2,Fd1,We,Rs,[Edge|Feat])
    end;
 
depth_traverse_tree([[]|[[{Value,_,_}|Alt]|Tree]],Sharp,
		    Depth,Dir,Fd0,We,Rs,Feat) ->
    %% Last tested in that branch -> search other branches
%    ?DBG("Last tested in that branch -> search other branches ~p~n", [Depth]),
    depth_traverse_tree([Alt|Tree], Sharp -Value, 
			Depth -1,Dir,Fd0, We,Rs,Feat);
 
depth_traverse_tree([_Miss|[[Root|Alt]|Tree]], Sharp, Depth, 
		    Dir,Fd0, We,Rs,Feat) 
  when Depth >= ?MAX_STRING_LENGTH ->
    %% To deep -> look in other branch
%    ?DBG("To deep -> look in other branch ~n",[]),
    {Value, _, _} = Root,
    depth_traverse_tree([Alt|Tree], Sharp - Value, Depth -1, 
			Dir,Fd0, We,Rs, Feat);
depth_traverse_tree([[]], _Sharp, _Depth, _Dir,Fd0,_We,_Rs,Feat) ->
    %% done -> tree search complete
%    ?DBG("done[[]]~n",[]),
    {Feat, Fd0};
depth_traverse_tree(Tree=[[{Val,Leaf,Vertex}|_]|_],Sharp,Depth,
		    Dir,Fd0,We,Rs,Feat) ->
    %% No success yet -> search deeper
%%    ?DBG("No success yet -> search deeper ~p (~p) ~p ~n",[Sharp+Val, ?MIN_SHARPNESS, Depth]),
    Next = case Leaf of 
	       {Id, #edge{vs = Vertex, ve = NextV}} ->
		   NextV;
	       {Id, #edge{ve = Vertex, vs = NextV}} ->
		   NextV
	   end,
    case get_edges(Next, Id, Dir, We, Fd0) of
	{[],Poss} -> %% Fixing last edge in edgeloop
	    {NewSharp,NewTree, NewFeat} = 
		patch_tree(Poss, Sharp+Val, Tree, Rs,Feat),
	    depth_traverse_tree(NewTree,NewSharp,Depth+1,Dir,Fd0,
				We,Rs,NewFeat);
        {Edges,_} ->
	    depth_traverse_tree([Edges|Tree],Sharp+Val,Depth+1,Dir,Fd0,
				We,Rs,Feat)
    end.

patch_tree([{Val,{Edge,_ER},_Vs}|_],Sharp,Tree,Rs,Feat) ->
    case lists:member(Edge, Feat) of
	true when (Val + Sharp) > Rs#restr.sharp ->
%%%	    ?DBG("Patched OK ~p ~n",[Edge]),
	    NewFeats = lists:map(fun([{_,{Element,_},_}|_]) -> Element end,
				 Tree),
	    {Val + Sharp, [[]], NewFeats ++ Feat};
	_Mem ->
%%%	    ?DBG("patch miss ~p ~p ~p ~p ~n",[Edge, _Mem, Val, Sharp]),
	    {Sharp, [[]|Tree], Feat}
    end;
patch_tree([],Sharp,Tree, _,Feat) ->
    {Sharp, [[]|Tree], Feat}.

get_edges(V, Current, CurrVect, We, Fd) ->
    Surr = ?fdgetsurrneigh({Current,V}, Fd),
    Add = fun(Edge, Acc = {Acc1,Acc2}) ->
		  case ?fdisneigh(Edge, Fd) of
		      true ->
			  ER = array:get(Edge, We#we.es),
			  Val = ?fdgetvalue(Edge,Fd),
			  {Acc1,[{1-Val,{Edge,ER},V}|Acc2]};
		      false ->			  
			  ER = #edge{vs = Va,ve = Vb} = array:get(Edge, We#we.es),
			  ThisVect = get_vector(Va,Vb,V,We),
			  Dir = calc_dir_vector(CurrVect, ThisVect),
			  if Dir < 0.707106  -> %% 90deg
				  Acc;
			     true ->
				  Val = ?fdgetvalue(Edge,Fd),
				  {[{1 - Val, {Edge, ER}, V}|Acc1],Acc2}
			  end
		  end
	  end,
    {New,Bup} = lists:foldl(Add, {[],[]}, Surr),
    {lists:reverse(lists:sort(New)), lists:reverse(lists:sort(Bup))}.


calc_dir_vector(Vec1, Vec2) ->
    Vec = e3d_vec:add(Vec1,Vec2),
    e3d_vec:len(e3d_vec:divide(Vec, 2.0)).

get_vector(A,B,A,We) ->
    get_vector(B,A,We);
get_vector(A,B,B,We) ->
    get_vector(A,B,We).

get_vector(A, B, #we{vp=Vs}) ->
    e3d_vec:norm(e3d_vec:sub(array:get(A, Vs), array:get(B, Vs))).

find_extremities(#we{vc=Vct,vp=Vs0,es=Es}) ->
    Center = e3d_vec:average(array:sparse_to_list(Vs0)),
    Vs = array:sparse_to_orddict(Vs0),
    AllC = lists:map(fun({Id,Pos}) ->
			     Dist = e3d_vec:dist(Pos, Center),
			     {Dist,Id,Pos}
		    end, Vs),
    [{_,V1,V1Pos}|_] = lists:reverse(lists:sort(AllC)),
    AllV1 = lists:map(fun({Id,Pos}) ->
			      Dist = e3d_vec:dist(Pos, V1Pos),
			      {Dist,Id,Pos}
		      end, Vs),
    [{_,V2,_}|_] = lists:reverse(lists:sort(AllV1)),
    E1 = array:get(V1, Vct),
    E2 = array:get(V2, Vct),
    F1 = (array:get(E1,Es))#edge.lf,
    case (array:get(E2,Es))#edge.lf of
	F1 ->
	    [F1, (array:get(E2,Es))#edge.rf];
	F2 ->
	    [F1, F2]
    end.

build_extreme([], _, _, Acc0) ->
    ReNumber = fun({_Group, Fs}, {No, Acc}) ->
		       {No+1,[{No, Fs}|Acc]}
	       end,
    {_,Acc} = lists:foldl(ReNumber, {0,[]}, Acc0),
    F = sofs:family(Acc),    
    Rel = sofs:family_to_relation(F),
    lists:reverse(sofs:to_external(Rel));
build_extreme(Fs,Max,FaceG,Acc) ->    
    Find = fun(Face, {FG, New0, Fs0}) ->
           case gb_trees:lookup(Face,FG) of
               {value, New1} ->
               FG1 = gb_trees:delete(Face,FG),
               {FG1, New1 ++ New0, [Face|Fs0]};
               none ->
               {FG, New0, Fs0}
           end
       end,
    {FG1,New, Fs1} = lists:foldl(Find, {FaceG, [], []}, Fs),
    build_extreme(New, Max-1, FG1, [{Max,Fs1}|Acc]).
    
build_seeds(Features0, #we{fs=Ftab}=We) ->
    FaceGraph = build_face_graph(gb_trees:keys(Ftab), We, 
				 gb_trees:empty()),
    case Features0 of
	[] ->
	    Fs = [F1,F2] = find_extremities(We),        
	    StartNo = gb_trees:size(Ftab),
	    Dists = [{Max, _}|_] = build_extreme(Fs,StartNo,FaceGraph,[]),
	    LMaxs = [{Max, F1}, {Max, F2}],
	    DTree = lists:foldl(fun({Dist, Face}, Tree) ->
					gb_trees:insert(Face, Dist, Tree)
				end, gb_trees:empty(), Dists),
	    {LMaxs,{DTree,Max,Dists}};
	_ ->
	    Distances = [{Max, _}|_] = 
		calc_distance(Features0, FaceGraph, We),
	    DTree = lists:foldl(fun({Dist, Face}, Tree) ->
					gb_trees:insert(Face, Dist, Tree)
				end, gb_trees:empty(), Distances),
	    LocalMaxs = find_local_max(Distances, DTree,Features0,
				       FaceGraph, We),
	    {LocalMaxs,{DTree,Max,Distances}}
    end.

build_charts(LocalMaxs, {DistTree,Max,Distances}, VEG, EWs, We) ->
    {Charts0,Bounds} = expand_charts(LocalMaxs, Max + 1, DistTree, VEG,EWs, We),
    Charts1 = sofs:from_external(gb_trees:to_list(Charts0), [{atom,atom}]),
    Charts2 = sofs:converse(Charts1),
    Charts3 = sofs:relation_to_family(Charts2),
    Charts4 = sofs:to_external(sofs:range(Charts3)),
    {Charts,Cuts} = normalize_charts(Charts4, Bounds, We),
    {Distances,Charts,Cuts}.

add_face_edges_to_heap(Face, FaceDist, ChartBds, Heap, EWs, We) ->
    Find = fun(_, Edge, #edge{lf=LF,rf=RF}, Heap0) ->
		   case gb_sets:is_member(Edge, ChartBds) of
		       true ->
			   Fopp = if LF == Face -> RF; RF == Face ->LF end,
			   EdgeW = 1 - gb_trees:get(Edge, EWs),
			   gb_trees:enter({FaceDist,EdgeW,Edge},{Face,Fopp}, Heap0);
		       false ->
			   Heap0
		   end
	   end,
    wings_face:fold(Find, Heap, Face, We).

delete_inner_edges(Face, VEG, We, Boundaries) ->
    wings_face:fold(fun(_, Edge, _, A) ->
			    case is_extremity(Edge, Boundaries, VEG, We) of
				false -> A;
				true -> gb_sets:delete_any(Edge, A)
			    end
		    end, Boundaries, Face, We).
			    
expand_charts(LocalMaxs, Max, Dt, VEG,EWs, We0) ->
    ChartsE = gb_trees:empty(),
    HeapE = gb_trees:empty(),
    ChartBds = gb_sets:from_ordset(wings_util:array_keys(We0#we.es)),
    Init = fun({LMax, Face}, {Chart0, Heap0}) ->
		   Chart1 = gb_trees:insert(Face, Face, Chart0),
		   Heap1  = add_face_edges_to_heap(Face,Max-LMax,ChartBds,Heap0,EWs,We0),
		   {Chart1, Heap1}
	   end,
    {ChartI, HeapI} = lists:foldl(Init, {ChartsE, HeapE}, LocalMaxs),
    expand_charts(HeapI, ChartI, ChartBds, Max, Dt, VEG,EWs, We0).
    
expand_charts(Heap0, Charts0, ChartBds0, Max, Dt, VEG,EWs, We) ->
    case gb_trees:is_empty(Heap0) of
	true ->
	    {Charts0,ChartBds0};
	false ->	    
	    {{_Dist,_,Edge},{Face,Fopp},Heap1} = gb_trees:take_smallest(Heap0),
	    ChartNum = gb_trees:get(Face, Charts0),
	    case gb_trees:lookup(Fopp, Charts0) of
		none ->
		    Charts = gb_trees:insert(Fopp, ChartNum, Charts0),
		    ChartBds1 = gb_sets:delete(Edge, ChartBds0),
		    ChartBds = delete_inner_edges(Fopp, VEG, We, ChartBds1),
		    DistFopp = gb_trees:get(Fopp, Dt),
		    Heap = add_face_edges_to_heap(Fopp, Max-DistFopp,
						  ChartBds, Heap1, EWs, We),
		    expand_charts(Heap, Charts, ChartBds, Max, Dt, VEG,EWs, We);
		{value,ChartNum} ->  %% Fopp and Face are in same chart
		    ChartBds = case is_extremity(Edge, ChartBds0, VEG, We) of
				   false -> ChartBds0;
				   true -> gb_sets:delete_any(Edge, ChartBds0)
			       end,
 		    expand_charts(Heap1, Charts0, ChartBds, Max, Dt, VEG,EWs, We);
		{value,OtherChartNum} ->
		    MaxDistChartFace = gb_trees:get(ChartNum, Dt),
		    MaxDistChartFopp = gb_trees:get(OtherChartNum, Dt),
		    DistFace = gb_trees:get(Face, Dt),
		    DistFopp = gb_trees:get(Fopp, Dt),
		    Const = Max/4,
		    if ((MaxDistChartFace - DistFace) < Const) and
		       ((MaxDistChartFopp - DistFopp) < Const) ->
			    {Charts,ChartBds} = 
				merge_charts(ChartNum, OtherChartNum,
					     Charts0, Dt, VEG, ChartBds0, We),
			    expand_charts(Heap1, Charts, ChartBds, Max,
					  Dt, VEG,EWs, We);
		       true ->
			    expand_charts(Heap1, Charts0, ChartBds0, Max, Dt, VEG,EWs, We)
		    end
	    end
    end.

is_extremity(Edge, ChartBds, VEG, #we{es=Etab}) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    not (is_connected(gb_trees:get({Edge,Va}, VEG), ChartBds) andalso
	 is_connected(gb_trees:get({Edge,Vb}, VEG), ChartBds)).

is_connected([E|Es], ChartBds) ->
    gb_sets:is_member(E, ChartBds) orelse is_connected(Es, ChartBds);
is_connected([], _) -> false.

merge_charts(Ch1,Ch2, Charts0, Dt, VEG,ChartBds0,We) ->    
    {C1,C2} =  
	case gb_trees:get(Ch1, Dt) > gb_trees:get(Ch2, Dt) of
	    true ->
		{Ch1, Ch2};
	    false->
		{Ch2, Ch1}
	end,
    List = gb_trees:to_list(Charts0),
    {Merged, ChartBds1} = 
	mapfoldl(fun({Face, Chart}, ChB) when Chart == C2 ->	      
			 %% Removed Merged Charts borders from
			 %% BorderEdges.
			 DelCommonEdge = 
			     fun(_V,Edge,#edge{lf=LF,rf=RF},Acc) ->
				     Test = if LF==Face -> RF; true -> LF end,
				     case gb_trees:lookup(Test, Charts0) of
					 {value, C1} ->
					     case is_extremity(Edge, Acc, VEG, We) of
						 true ->
						     gb_sets:delete_any(Edge,Acc);
						 false ->
						     Acc
					     end;
					 _ ->
					     Acc
				     end
			     end,
			 {{Face, C1}, wings_face:fold(DelCommonEdge, ChB, Face, We)};
		    (Else,ChB) -> 
			 {Else,ChB} end, 
		 ChartBds0, List),
    {gb_trees:from_orddict(Merged), ChartBds1}.   

find_local_max(Distances, DTree, Features, FaceGraph, #we{es = Es}) ->
    %% Remove the features from FaceGraph, edges which are a feature
    %%  shouldn't connect to opposite faces
    FG1 = lists:foldl(fun(Edge, Tree0) ->
			      #edge{lf=LF,rf=RF} = array:get(Edge, Es),
			      LFL = gb_trees:get(LF, Tree0),
			      LFL1 = lists:delete(RF, LFL),
			      Tree1 = gb_trees:update(LF, LFL1, Tree0),
			      RFL = gb_trees:get(RF, Tree1),
			      RFL1 = lists:delete(LF, RFL),
			      gb_trees:update(RF, RFL1, Tree1)
		      end, FaceGraph, Features),    
    find_local_maximum(Distances, DTree, FG1, []).

find_local_maximum([This = {Dist, Face}|Dists], Dtree0, FG, Maxs) ->
    case gb_trees:delete_any(Face, Dtree0) of
	Dtree0 ->
	    find_local_maximum(Dists, Dtree0, FG, Maxs);
	Dtree1 ->
	    Dtree2 = delete_less([{gb_trees:get(Face, FG), Dist}], Dtree1, FG),
	    find_local_maximum(Dists, Dtree2, FG, [This|Maxs])
    end;
find_local_maximum([], _, _FG, Maxs) ->
    lists:reverse(Maxs).

delete_less([{[Face|Rest1], Dist}|Rest2], Dtree0, FG) ->
    case gb_trees:lookup(Face, Dtree0) of
	none ->
	    delete_less([{Rest1, Dist}|Rest2], Dtree0, FG);
	{value, Val} when Val > Dist ->
	    delete_less([{Rest1, Dist}|Rest2], Dtree0, FG);
	{value, Val} ->
	    Dtree1 = gb_trees:delete(Face, Dtree0),
	    New = {gb_trees:get(Face, FG), Val},
	    delete_less([{Rest1,Dist},New|Rest2], Dtree1, FG)
    end;
delete_less([{[],_}|Rest], Dt, Fg) ->
    delete_less(Rest, Dt,Fg);
delete_less([],Dt,_Fg) ->
    Dt.

build_face_graph([Face|Faces], We, Tree0) ->
    Find = fun(_V, _Edge, #edge{lf=LF,rf=RF}, Acc) ->
		   New = if LF == Face -> RF; RF == Face ->LF end,
		   [New|Acc]
	   end,
    Surround = wings_face:fold(Find, [], Face, We),
    build_face_graph(Faces, We, gb_trees:insert(Face, Surround, Tree0));
build_face_graph([],_, Tree) ->
    Tree.

build_vertex_graph([{Edge, _Value}|R], We, Tree0) ->
    #edge{vs = Vs, ve = Ve} = array:get(Edge,We#we.es),
    Find = fun(Id, _Face, _, Acc) when Id == Edge ->
		   Acc;
	      (Id, _Face, _, Acc) ->
		   [Id|Acc]
	   end,
    Surround0 = wings_vertex:fold(Find, [], Vs, We),
    Tree1 = gb_trees:insert({Edge,Vs}, Surround0, Tree0),
    Surround1 = wings_vertex:fold(Find, [], Ve, We),
    build_vertex_graph(R, We, gb_trees:insert({Edge,Ve}, Surround1, Tree1));
build_vertex_graph([],_, Tree) ->
    Tree.

calc_distance(Features, FG, We = #we{fs = FsOrig}) ->
    Faces = fun(Edge, {Dist0,Next0,Fs0}) ->
		    #edge{lf = LF, rf = RF} = array:get(Edge, We#we.es),
		    {Dist1,Next1,Fs1} = find_delete(LF,0,Dist0,Next0,Fs0,FG),
		    find_delete(RF,0,Dist1,Next1,Fs1,FG)
	    end,   
    {Dist2,Next2,Fs2} = lists:foldl(Faces, {[],[],FsOrig}, Features),
    calc_distance(Next2, 1, Dist2, [], Fs2,FG).

calc_distance([Head|Rest], Level, Dist0, Next0, Fs0,Fg) ->
    {Dist2, Next2, Fs2} = find_delete(Head, Level, Dist0, Next0, Fs0,Fg),
    calc_distance(Rest, Level, Dist2, Next2, Fs2,Fg);
calc_distance([], _Level, Dist, [], _Fs, _) ->
    lists:reverse(lists:sort(Dist));
calc_distance([], Level, Dist, Next, Fs, Fg) ->
    calc_distance(Next, Level+1, Dist, [], Fs, Fg).

find_delete(Face,Level,Dist0,Next0,Fs0,Fg) ->
    case gb_trees:delete_any(Face, Fs0) of
	Fs0 -> {Dist0,Next0,Fs0};
	Fs1 -> 
	    Next = gb_trees:get(Face,Fg),
	    {[{Level,Face}|Dist0],Next ++ Next0,Fs1}
    end.
%%%%%%%%%%% End Feature detection

%%
%% The original autouv algorithm...
%%

segment_by_direction(We) ->
    Rel = [begin
	       N = wings_face:normal(Face, We),
	       {seg_dir(N),Face}
	   end || Face <- wings_we:visible(We)],
    segment_by_cluster(Rel, We).

seg_dir({X,Y,Z}) ->
    Max = lists:max([abs(X),abs(Y),abs(Z)]),
    if
	X =:= Max -> x;
	Y =:= Max -> y;
	Z =:= Max -> z;
	-X =:= Max -> '-x';
	-Y =:= Max -> '-y';
	-Z =:= Max -> '-z'
    end.

segment_by_material(We) ->
    Rel = [{Name,Face} || {Face,Name} <- wings_facemat:all(We)],
    segment_by_cluster(Rel, We).

%% segment_by_cluster([{Key,Face}], We)
%%  Group all faces by Key.
segment_by_cluster(Rel0, #we{mirror=Mirror}=We) ->
    Rel1 = lists:keydelete(Mirror, 2, Rel0),
    Rel = sofs:relation(Rel1),
    Clustered = sofs:relation_to_family(Rel),
    Groups0 = sofs:range(Clustered),
    Groups = sofs:to_external(Groups0),
    Neigh = [wings_sel:face_regions(Group, We) || Group <- Groups],
    foldl(fun(List, Acc) ->
		  [gb_sets:to_list(L) || L <- List]++Acc
	  end, [], Neigh).

%%%
%%% Map back to the original vertex.
%%%

map_vertex(V0, Vmap) ->
    case gb_trees:lookup(V0, Vmap) of
	none -> V0;
	{value,V} -> V
    end.

%%%
%%% Map back to the original edge.
%%%

map_edge(E0, Emap) ->
    case gb_trees:lookup(E0, Emap) of
	none -> E0;
	{value,E} -> E
    end.

%%%
%%% Cutting along hard edges.
%%%

cut_model(Charts, Cuts0, We0) ->
    We = wings_va:remove(all, We0),
    Cuts = gb_sets:to_list(Cuts0),
    cut_model_1(Charts, Cuts, We#we{mirror=none}, length(Charts), []).

cut_model_1([Fs|Cs], Cuts, OrigWe, Id, Acc) ->
    We = cut_one_chart(Fs, Cuts, OrigWe#we{id=Id}),
    cut_model_1(Cs, Cuts, OrigWe, Id-1, [We|Acc]);
cut_model_1([], _, _, _, Acc) -> Acc.

cut_one_chart(Keep0, Cuts, We0) ->
    {InnerEdges,OuterEdges} = wings_face:inner_outer_edges(Keep0, We0),
    Keep = gb_sets:from_list(Keep0),
    Map0 = gb_trees:empty(),
    {We1,Map1} = cut_shared_vertices(Keep, OuterEdges, We0, Map0),
    {We2,Vmap} = cut_edges(Keep0, InnerEdges, Cuts, We1, Map1),
    %% Dissolve unneeded faces and also hide them.
    #we{fs=Ftab} = We3 = wpa:face_dissolve_complement(Keep0, We2),
    Hidden = ordsets:subtract(gb_trees:keys(Ftab), Keep0),
    We4 = wings_we:hide_faces(Hidden, We3),
    %% Create edge map and finish We.
    Me = wings_we:new_items_as_ordset(edge, We1, We4),
    Emap = make_emap(Me, Vmap, We0, We4, []),
    We4#we{name=#ch{vmap=Vmap,me=Me,emap=Emap}}.

make_emap([ME|T], Vmap, We0, #we{es=Etab}=We, Acc) ->
    case array:get(ME, Etab) of
	undefined ->
	    make_emap(T, Vmap, We0, We, Acc);
	#edge{vs=Va0,ve=Vb0} ->
	    Va = map_vertex(Va0, Vmap),
	    case map_vertex(Vb0, Vmap) of
		Va ->
		    make_emap(T, Vmap, We0, We, Acc);
		Vb ->
		    E = wings_vertex:until(
			  fun(E, _, Rec, A) ->
				  case Rec of
				      #edge{vs=Va,ve=Vb} -> E;
				      #edge{vs=Vb,ve=Va} -> E;
				      _ -> A
				  end
			  end, none, Va, We0),
		    case E of
			none -> make_emap(T, Vmap, We0, We, Acc);
			_ -> make_emap(T, Vmap, We0, We, [{ME,E}|Acc])
		    end
	    end
    end;
make_emap([], _, _, _, Acc) -> gb_trees:from_orddict(sort(Acc)).

cut_shared_vertices(Faces, Es, #we{es=Etab}=We0, InvVmap0) ->
    VsEs0 = foldl(fun(E, A) ->
			  #edge{vs=Va,ve=Vb} = array:get(E, Etab),
			  [{Va,E},{Vb,E}|A]
		  end, [], Es),
    VsEs = sofs:relation(VsEs0),
    F = sofs:relation_to_family(VsEs),
    Shared0 = sofs:specification({external,fun({_,L}) ->
						   length(L) > 2
					   end}, F),
    Shared = sofs:to_external(sofs:domain(Shared0)),
    foldl(fun(V, A) ->
		  do_cut_shared(V, Faces, A)
	  end, {We0,InvVmap0}, Shared).

do_cut_shared(V, Faces, {#we{next_id=Wid}=We0,Ivmap}) ->
    Fs = wings_vertex:fold(
	   fun(_, Face, _, A) ->
		   case gb_sets:is_member(Face, Faces) of
		       false -> A;
		       true -> [Face|A]
		   end
	   end, [], V, We0),
    We = wings_vertex_cmd:bevel_vertex(V, We0),
    do_cut_shared_1(Fs, V, Wid, We, Ivmap).

do_cut_shared_1([F|Fs], V, Wid, We0, Map0) ->
    {We,Map} = wings_face:fold(
		 fun(_, E, #edge{vs=Va}, {W0,M}) when E >= Wid ->
			 W = wings_collapse:collapse_edge(E, Va, W0),
			 {W,add_new_vs(V, [Va], M)};
		    (_, _, _, A) -> A
		 end, {We0,Map0}, F, We0),
    do_cut_shared_1(Fs, V, Wid, We, Map);
do_cut_shared_1([], _, _, We, Map) -> {We,Map}.

cut_edges(Faces, Inner, Cuts0, We, Map) ->
    case ordsets:intersection(Inner, Cuts0) of
	[] -> {We,Map};
	Cuts -> cut_edges_1(Faces, Cuts, We, Map)
    end.

cut_edges_1(Faces, Cuts, We0, Map0) ->
    Vs = wings_edge:to_vertices(Cuts, We0),
    {We1,Map1} = bevel_cut_vs(Vs, We0, Map0),
    CutEdges = edges_to_cut(Cuts, We1),
    {We2,Map} = cut_new_edges(CutEdges, We1, Map1),
    MaybeRem = wings_we:new_items_as_ordset(edge, We0, We2),
    We3 = connect_edges(Cuts, We2),
    We = cut_cleanup(Faces, MaybeRem, We3),
    {We,Map}.

bevel_cut_vs([V|Vs], We0, Map0) ->
    We = wings_vertex_cmd:bevel_vertex(V, We0),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    Map = add_new_vs(V, NewVs, Map0),
    bevel_cut_vs(Vs, We, Map);
bevel_cut_vs([], We, Map) -> {We,Map}.

edges_to_cut(Es, #we{es=Etab}) ->
    edges_to_cut_1(Es, Etab, []).

edges_to_cut_1([E|Es], Etab, Acc) ->
    #edge{ltpr=Lp,ltsu=Lu,rtpr=Rp,rtsu=Ru} = array:get(E, Etab),
    edges_to_cut_1(Es, Etab, [Lp,Lu,Rp,Ru|Acc]);
edges_to_cut_1([], _, Es) ->
    edges_to_cut_2(sort(Es), []).

edges_to_cut_2([E,E|Es], Acc) ->
    edges_to_cut_2(Es, [E|Acc]);
edges_to_cut_2([_|Es], Acc) ->
    edges_to_cut_2(Es, Acc);
edges_to_cut_2([], Acc) ->
    ordsets:from_list(Acc).

cut_new_edges([Edge|Es], #we{es=Etab}=We0, Map0) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    Pos = wpa:vertex_pos(Va, We0),
    {We,NewV} = wings_edge:screaming_cut(Edge, Pos, We0),
    Map = add_new_vs(Vb, [NewV], add_new_vs(Va, [NewV], Map0)),
    cut_new_edges(Es, We, Map);
cut_new_edges([], We, Map) -> {We,Map}.

connect_edges([E|Es], #we{es=Etab}=We0) ->
    #edge{vs=Va,ve=Vb,lf=Lf,ltpr=Lp,ltsu=Lu,
	  rf=Rf,rtpr=Rp,rtsu=Ru} = array:get(E, Etab),
    LpV = other_vertex(Vb, Lp, Etab),
    LuV = other_vertex(Va, Lu, Etab),
    RpV = other_vertex(Va, Rp, Etab),
    RuV = other_vertex(Vb, Ru, Etab),
    {We1,_} = wings_vertex:force_connect(LpV, LuV, Lf, We0),
    {We,_} = wings_vertex:force_connect(RpV, RuV, Rf, We1),
    connect_edges(Es, We);
connect_edges([], We) -> We.

other_vertex(V, Edge, Etab) ->
    wings_vertex:other(V, array:get(Edge, Etab)).

cut_cleanup(Faces, MaybeRemove, We) ->
    Es = ordsets:intersection(MaybeRemove,
			      wings_face:to_edges(Faces, We)),
    foldl(fun(E, W) ->
		  wings_collapse:fast_collapse_edge(E, W)
	  end, We, Es).

add_new_vs(OldV, NewVs, Map) ->
    foldl(fun(NewV, M) -> add_new_vs_1(OldV, NewV, M) end, Map, NewVs).

add_new_vs_1(To, From, Map) ->
    case gb_trees:lookup(To, Map) of
	none -> gb_trees:enter(From, To, Map);
	{value,NewTo} -> add_new_vs_1(NewTo, From, Map)
    end.

%% normalize_charts(Charts, Cuts, We) -> {Charts',Cuts'}
%%  Possibly split charts that are divided into several non-connected
%%  components by edges in Cuts. Remove any edges in Cuts that goes
%%  along the boundary between two charts.
normalize_charts(Charts, Cuts, We) ->
    normalize_charts(Charts, Cuts, We, []).

normalize_charts([C0|Cs], Cuts, We, Acc0) ->
    C = gb_sets:from_list(C0),
    Acc = normalize_chart(C, Cuts, We, Acc0),
    normalize_charts(Cs, Cuts, We, Acc);
normalize_charts([], Cuts0, We, Charts) ->
    AllInner0 = lists:concat([wings_face:inner_edges(C, We) || C <- Charts]),
    AllInner = gb_sets:from_list(AllInner0),
    Cuts = gb_sets:intersection(Cuts0, AllInner),
    {Charts,Cuts}.

normalize_chart(Faces, Cuts, We, Acc) ->
    find_reachable(Faces, We, collect_fun(Cuts), Acc).

find_reachable(Faces0, We, Coll, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = [Face],
	    {Reg,Faces} = collect_reachable(Ws, We, Coll, [], Faces1),
	    find_reachable(Faces, We, Coll, [Reg|Acc])
    end.

collect_reachable([_|_]=Ws0, We, Coll, Reg0, Faces0) ->
    Reg = Ws0++Reg0,
    {Ws,Faces} = wings_face:fold_faces(Coll, {[],Faces0}, Ws0, We),
    collect_reachable(Ws, We, Coll, Reg, Faces);
collect_reachable([], _, _, Reg, Faces) ->
    {sort(Reg),Faces}.

collect_fun(Cuts) ->
    fun(Face, _, Edge, Rec, {Ws,Faces}=A) ->
	    Of = case Rec of
		     #edge{lf=Face,rf=Of0} -> Of0;
		     #edge{rf=Face,lf=Of0} -> Of0
		 end,
	    case gb_sets:is_member(Of, Faces) andalso
		not gb_sets:is_member(Edge, Cuts) of
		true -> {[Of|Ws],gb_sets:delete(Of, Faces)};
		false -> A
	    end
    end.

%%%
%%% Build a map [F|V] => UV.
%%%

fv_to_uv_map(object,#we{fs=Ftab,holes=Holes0}=We) ->
    AllFsEs = sofs:relation(gb_trees:to_list(Ftab)),
    Holes = sofs:set(Holes0),
    FsEs = sofs:drestriction(AllFsEs, Holes),
    fvuvmap_1(sofs:to_external(FsEs), We, [], []);
fv_to_uv_map(Faces0,#we{fs=Ftab}=We) ->
    AllFsEs = sofs:relation(gb_trees:to_list(Ftab)),
    Fs = sofs:set(gb_sets:to_list(Faces0)),
    FsEs = sofs:restriction(AllFsEs,Fs),
    fvuvmap_1(sofs:to_external(FsEs), We, [], []).

fvuvmap_1([{F,E}|FsEs], We, FaceAcc, Acc) ->
    case uv_info(F, E, We) of
	error -> fvuvmap_1(FsEs, We, FaceAcc, Acc);
	Info -> fvuvmap_1(FsEs, We, [F|FaceAcc], Info++Acc)
    end;
fvuvmap_1([], _, FaceAcc, Acc) ->
    {FaceAcc,gb_trees:from_orddict(sort(Acc))}.
	
uv_info(F, E, We) ->
    uv_info_1(wings_va:face_attr([vertex|uv], F, E, We), F, []).

uv_info_1([[V|{_,_}=UV]|T], F, Acc) ->
    uv_info_1(T, F, [{[F|V],UV}|Acc]);
uv_info_1([[V|_]|T], F, Acc) ->
    %% No UV coordinates for this vertex.
    uv_info_1(T, F, [{[F|V],none}|Acc]);
uv_info_1([_|_], _, _) -> error;
uv_info_1([], _, Acc) -> Acc.

%%%
%%% Given a model having UV coordinates, partition it into charts.
%%%

uv_to_charts(Faces0, Dict, We) ->
    Faces = gb_sets:from_list(Faces0),
    Coll = collect_chart_fun(Dict),
    Cs = uv_to_charts_1(Faces, We, Coll, []),
    Cuts = chart_cuts(Cs, We, Dict, []),
    {Cs,Cuts}.

uv_to_charts_1(Faces0, We, Coll, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {Face,Faces1} = gb_sets:take_smallest(Faces0),
	    Ws = [Face],
	    {Reg,Faces} = collect_chart(Ws, We, Coll, [], Faces1),
	    uv_to_charts_1(Faces, We, Coll, [Reg|Acc])
    end.

collect_chart([_|_]=Ws0, We, Coll, Reg0, Faces0) ->
    Reg = Ws0++Reg0,
    {Ws,Faces} = wings_face:fold_faces(Coll, {[],Faces0}, Ws0, We),
    collect_chart(Ws, We, Coll, Reg, Faces);
collect_chart([], _, _, Reg, Faces) ->
    {sort(Reg),Faces}.

collect_chart_fun(Dict) ->
    fun(Face, _, _, Rec, {Ws,Faces}=A) ->
	    Of = case Rec of
		     #edge{lf=Face,rf=Of0} -> Of0;
		     #edge{rf=Face,lf=Of0} -> Of0
		 end,
	    case gb_sets:is_member(Of, Faces) andalso
		not is_cutting_edge(Rec, Dict) of
		true -> {[Of|Ws],gb_sets:delete(Of, Faces)};
		false -> A
	    end
    end.

chart_cuts([C|Cs], #we{es=Etab}=We, D, Acc0) ->
    Inner = wings_face:inner_edges(C, We),
    Acc = chart_cuts_1(Inner, Etab, D, Acc0),
    chart_cuts(Cs, We, D, Acc);
chart_cuts([], _, _, Acc) -> gb_sets:from_list(Acc).

chart_cuts_1([E|Es], Etab, D, Acc) ->
    case is_cutting_edge(array:get(E, Etab), D) of
	false -> chart_cuts_1(Es, Etab, D, Acc);
	true -> chart_cuts_1(Es, Etab, D, [E|Acc])
    end;
chart_cuts_1([], _, _, Acc) -> Acc.

is_cutting_edge(#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}, D) ->
    gb_trees:get([Lf|Va], D) =/= gb_trees:get([Rf|Va], D) orelse
	gb_trees:get([Lf|Vb], D) =/= gb_trees:get([Rf|Vb], D).
