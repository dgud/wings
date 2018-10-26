%%
%%  wpc_untriangulate.erl --
%%
%%    Plug-in to untriangulate (make quads from a triangle soup).
%%
%%  Copyright (c) 2005-2011 Dave Rodgers
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_untriangulate).

-export([init/0,menu/2,command/2]).
-import(lists, [foldl/3,seq/2]).
-include_lib("wings/src/wings.hrl").

%% Uncomment the following line turn on debug printouts.
%%-define(DEBUG_UNTRI, 1).

-define(MAXIMUM_ALGO_LEVEL, 3).
-define(DEFAULT_ALGO_LEVEL, 1).
-define(DEFAULT_EDGE_ANGLE, 60.0).
-define(DEFAULT_UV_TOLERANCE, 1.0E-3).


%% utopts - Untriangulate Options
-record(utopts, {angle = ?DEFAULT_EDGE_ANGLE,
                 usehard = true,
                 usemat = true,
                 useuvs = true,
                 convex = true,
                 algolvl = 2
                }).

%% edege record
-record(erec, {nbs,    % gb_sets of neighbours
               geoval  % weight based on rectangular geometry
              }).
              

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Exported functions
%%

init() ->
    init_pref(),
    true.

menu({face,tesselate}, Menu) ->
    Menu ++ [{?__(1,"Untriangulate"), untriangulate,
              ?__(2,"Convert triangle sets into quads"),[option]}];
menu({edit,plugin_preferences}, Menu) ->
    Menu ++ [{?__(3,"Untriangulate"),untriangulate}];
menu(_,Menu) -> Menu.

command({face, {tesselate, {untriangulate, Ask}}}, St) ->
    untriangulate(Ask,St);
command({edit,{plugin_preferences,untriangulate}}, St) ->
    pref_edit(St);
command(_,_) -> next.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Menus and Input Processing
%%

untriangulate(Ask, _) when is_atom(Ask) ->
    init_pref(),
    Qs = get_ask_list(),
    wings_dialog:dialog(Ask, ?__(1,"Untriangulate (v0.2)"), [{vframe, Qs}],
			fun(Res) ->
				{face, {tesselate, {untriangulate, Res}}}
			end);

untriangulate([AlgoLevelAtom, Angle,
               {usemat, Mat},
               {useuvs, UVs},
               {usehard, Hard},
               {convex, Convex}], St) ->
    [_H|AlgoNum] = atom_to_list(AlgoLevelAtom),
    AlgoLevel = list_to_integer(AlgoNum),
    CosAngle = math:cos(Angle * (math:pi() / 180.0)),
    Options = #utopts{angle=CosAngle,
                      usehard=Hard,
                      usemat=Mat,
                      useuvs=UVs,
                      convex=Convex,
                      algolvl=AlgoLevel},
    ?SLOW(untriangulate_objs(Options, St)).  %% ?SLOW() for the hourglass

untriangulate_objs(Options, St) ->
    EdgesSt = wings_sel_conv:mode(edge, St),
    SelFun = wings_sel:fold(
               fun(Edges, #we{id=Id}=We, Acc) ->
                 Eset = process_edges(Options, Edges, We, St),
                 [{Id,Eset}|Acc]
               end, [], EdgesSt),
    Targets = wings_sel:set(edge, SelFun, St),
    %%
    %%  Dissolve the edges
    %%
    Result = wings_sel:map(fun(Es, We) ->
                         wings_edge:dissolve_edges(Es, We)
                       end, Targets),
    wings_sel:clear(Result).

get_ask_list() ->
    Levels = seq(1, ?MAXIMUM_ALGO_LEVEL),
    IntToAtom = fun(X) ->
                  list_to_atom("l" ++ integer_to_list(X))
                end,
    Qs = [{hradio, 
            [{integer_to_list(N) ++ " ", IntToAtom(N)} || N <- Levels],
            IntToAtom(get_param(algolvl)),
            [{title, ?__(1,"Algorithm Strength")},
             {info,  ?__(2,"Higher algorithm strengths are slower, "
                     "but may produce better results")}
            ]},
          {hframe,
            [{label, ?__(3,"Max Edge Angle")},
             {text, get_param(angle), [{range, {0.0, 180.0}},
               {info, ?__(4,"Maximum angle of edges that are dissolved")}]}
            ]},
          separator,
          {?__(5,"Check Materials"), get_param(usemat), [{key, usemat},
            {info,?__(6,"Do not dissolve edges along material boundaries")}]},
          {?__(7,"Check UV Coords"), get_param(useuvs), [{key, useuvs},
            {info,?__(8,"Do not dissolve edges along disjoint "
                  "texture coordinate boundaries")}]},
          {?__(9,"Check Hard Edges"), get_param(usehard), [{key, usehard},
            {info,?__(10,"Do not dissolve hard edges")}]},
          {?__(11,"Check Concavity"), get_param(convex), [{key, convex},
            {info,?__(12,"Do not produce concave polygons")}]}
         ],
    Qs.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Compare 2 edge records
%%
%%    Returns true if Erec0 is a better pick then Erec1, otherwise false
%%

compare_erecs(Erec0, Erec1, AlgoLevel) ->
    Nbs0 = gb_sets:size(Erec0#erec.nbs),
    Nbs1 = gb_sets:size(Erec1#erec.nbs),
    case AlgoLevel of
      1 -> 
        %%
        %% straight geoval comparison
        %%
        if
          (Erec0#erec.geoval > Erec1#erec.geoval) -> true;
          true -> false
        end;
      2 ->
        %%
        %% select pinched quads first
        %%
        case {Nbs0,Nbs1} of
          {0,_} -> true;
          {_,0} -> false;
          {1,_} -> true;  
          {_,1} -> false;
          _ ->
            if
              (Erec0#erec.geoval > Erec1#erec.geoval) -> true;
              true -> false
            end
        end;
      _ ->
        %%
        %% select peripheral quads before centered quads
        %%
        case {Nbs0,Nbs1} of
          {0,_} -> true;
          {_,0} -> false;
          {1,_} -> true;
          {_,1} -> false;
          {2,4} -> true;
          {3,4} -> true;
          _ ->
            if
              (Erec0#erec.geoval > Erec1#erec.geoval) -> true;
              true -> false
            end
        end
      end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Process the edges
%%
%%    Returns a gb_sets of edges to be deleted
%%

process_edges(Options, SelSet, We, St) ->
    AlgoLevel = Options#utopts.algolvl,
    wings_pb:start(?__(1,"Untriangulate")),
    wings_pb:update(0.01, ?__(2,"Picking Target Edges")),
    Tlist = find_target_edges(Options,SelSet,We,St),
    wings_pb:update(0.02, ?__(3,"Making Selection Tree")),
    ETree = make_erec_tree(AlgoLevel, Tlist, We),
    KillList = pick_edges(ETree, AlgoLevel),
    wings_pb:done(),
    gb_sets:from_list(KillList).

pick_edges(ETree, AlgoLevel) ->
    Size = gb_trees:size(ETree),
    pick_edges(Size, Size, ETree, AlgoLevel, []).
pick_edges(_, 0, _, _, List) ->
    List;
pick_edges(FullCount, Count, ETree, AlgoLevel, List) ->
    update_progress_bar(Count, FullCount),
    Elist = gb_trees:to_list(ETree),
    BestEdge = pick_best_edge(Elist, AlgoLevel),
    ET0 = remove_edge(BestEdge, ETree),
    List0 = [BestEdge|List],
    pick_edges(FullCount, gb_trees:size(ET0), ET0, AlgoLevel, List0).

pick_best_edge([{Edge,Erec}|T], AlgoLevel) ->
    pick_best_edge({Edge,Erec}, T, AlgoLevel).
pick_best_edge({Edge,_Erec}, [], _) -> Edge;
pick_best_edge({Edge,Erec}, [{NextEdge, NextErec}|T], AlgoLevel) ->
    case compare_erecs(Erec, NextErec, AlgoLevel) of
      true -> pick_best_edge({Edge,Erec}, T, AlgoLevel);
      _ -> pick_best_edge({NextEdge,NextErec}, T, AlgoLevel)
    end.
      
remove_edge(Edge, ETree) ->
    Erec = gb_trees:get(Edge, ETree),
    NbSet = Erec#erec.nbs,
    NbList = gb_sets:to_list(NbSet),
    ET0 = unreference_edges(Edge, NbList, NbSet, ETree),
    ET1 = gb_trees:delete(Edge, ET0),
    ET2 = foldl(fun(Nb, ET) ->
                  gb_trees:delete(Nb, ET)
                end, ET1, NbList),
    ET2.

unreference_edges(OrigEdge, NbList, NbSet, ETree) ->
    Tset0 = foldl(fun(E, Acc) ->
                    Erec = gb_trees:get(E,ETree),
                    gb_sets:union(Erec#erec.nbs, Acc)
                  end, gb_sets:empty(), NbList),
    CoreSet = gb_sets:add(OrigEdge, NbSet),
    Tset2 = gb_sets:difference(Tset0, CoreSet),
    Nb2List = gb_sets:to_list(Tset2),
    ET0 = foldl(fun(E, ET) ->
                  Erec0 = gb_trees:get(E, ET),
                  NewNbs = gb_sets:difference(Erec0#erec.nbs, NbSet),
                  Erec1 = Erec0#erec{nbs=NewNbs},
                  gb_trees:update(E, Erec1, ET)
                end, ETree, Nb2List),
    ET0.

update_progress_bar(Count, FullCount) ->
    Fraction = 1.0 - (Count / FullCount),
    F = if
          Fraction < 0.03 -> 0.03;
          Fraction > 1.00 -> 1.00;
          true -> Fraction
        end,
    wings_pb:update(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Initialize the ETree
%%

make_erec_tree(_AlgoLevel, [], _) -> gb_trees:empty();
make_erec_tree(AlgoLevel, Elist, We) ->
    Dict = [{E, make_new_erec(AlgoLevel,E,We)} || E <- Elist ],
    ET0 = gb_trees:from_orddict(lists:sort(Dict)),
    ET1 = prune_first_etree(ET0),
    maybe_print_etree(ET1),
    ET1.

make_new_erec(_AlgoLevel, Edge, We) ->
    #edge{ltpr=Ltpr,ltsu=Ltsu,
          rtpr=Rtpr,rtsu=Rtsu} = array:get(Edge, We#we.es),
    {Geo,_Convex} = geometry_weight(Edge, We),
    Nbs = gb_sets:from_list([Ltpr,Ltsu,Rtpr,Rtsu]),
    #erec{geoval=Geo,nbs=Nbs}.

prune_first_etree(ETree) ->
    Elist = gb_trees:to_list(ETree),
    prune_first_etree(Elist, ETree).
prune_first_etree([], ETree) -> ETree;
prune_first_etree([{Edge,Erec}|T], ETree) ->
    NL0 = gb_sets:to_list(Erec#erec.nbs),
    NL1 = foldl(fun(E, Acc) ->
                  case gb_trees:lookup(E, ETree) of
                    none -> Acc;
                    _ -> [E|Acc]
                  end
                end, [], NL0),
    Nset = gb_sets:from_list(NL1),
    Erec0 = Erec#erec{nbs=Nset},
    ETree0 = gb_trees:update(Edge, Erec0, ETree),
    prune_first_etree(T, ETree0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Geometry Factor
%%
%%    A perfect score is 4. It is based on the corner angles
%%    (concave corners will substract from the overall score).
%%
%%    Returns {Val, Convex}, where Convex is 'true' or 'false'
%%

geometry_weight(E, We) ->
    #edge{ltpr=Ltpr,ltsu=Ltsu,
          rtpr=Rtpr,rtsu=Rtsu,lf=Lf,rf=Rf} = array:get(E, We#we.es),
    N0 = edge_unit_vector(Ltpr, Lf, We),
    N1 = edge_unit_vector(Ltsu, Lf, We),
    N2 = edge_unit_vector(Rtpr, Rf, We),
    N3 = edge_unit_vector(Rtsu, Rf, We),
    C0 = e3d_vec:cross(N0, N1),
    C1 = e3d_vec:cross(N1, N2),
    C2 = e3d_vec:cross(N2, N3),
    C3 = e3d_vec:cross(N3, N0),
    N = e3d_vec:norm(e3d_vec:add([C0,C1,C2,C3])),  % the combined normal
    W0 = e3d_vec:dot(N, C0),
    W1 = e3d_vec:dot(N, C1),
    W2 = e3d_vec:dot(N, C2),
    W3 = e3d_vec:dot(N, C3),
    Val= W0 + W1 + W2 + W3,
    Convex = if
               (W0 < 0.0) -> false;
               (W1 < 0.0) -> false;
               (W2 < 0.0) -> false;
               (W3 < 0.0) -> false;
               true -> true
             end,
    {Val, Convex}.

edge_unit_vector(Edge, Face, We) ->
    {Vs1,Ve1} = case array:get(Edge, We#we.es) of
                  #edge{lf=Face,vs=Vs0,ve=Ve0} -> {Vs0,Ve0};
                  #edge{rf=Face,vs=Vs0,ve=Ve0} -> {Ve0,Vs0};
                  _  -> error
                end,
    Vs = array:get(Vs1, We#we.vp),
    Ve = array:get(Ve1, We#we.vp),
    V = e3d_vec:sub(Ve, Vs),
    N = e3d_vec:norm(V),
    N.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Find edges that could potentially be deleted
%%
%%    Returns a list of edge indices
%%

find_target_edges(Options, SelSet, We, _St) ->
    Elist = gb_sets:to_list(SelSet),
    foldl(fun(E, Acc) ->
            case check_edge(Options, E, We, SelSet) of
              true -> [E|Acc];
              _ -> Acc
            end
          end, [], Elist).
          
check_edge(Opts, Edge, We, SelSet) ->
    check_triangles(Edge,We,SelSet)
      andalso  check_angle(Edge, We, Opts#utopts.angle)
      andalso  ((not Opts#utopts.usehard)  orelse  check_hard_edge(Edge,We))
      andalso  ((not Opts#utopts.usemat)   orelse  check_materials(Edge,We))
      andalso  ((not Opts#utopts.useuvs)   orelse  check_uv_coords(Edge,We))
      andalso  ((not Opts#utopts.convex)   orelse  check_concavity(Edge,We)).

check_triangles(Edge, We, SelSet) ->
    #edge{ltpr=Ltpr,ltsu=Ltsu,
          rtpr=Rtpr,rtsu=Rtsu} = array:get(Edge, We#we.es),
    #edge{ltsu=LLtsu,rtsu=LRtsu} = array:get(Ltsu, We#we.es),
    #edge{ltsu=RLtsu,rtsu=RRtsu} = array:get(Rtsu, We#we.es),
    (((Ltpr == LLtsu) orelse (Ltpr == LRtsu)) andalso
     ((Rtpr == RRtsu) orelse (Rtpr == RLtsu)))         %% triangles?
      andalso gb_sets:is_member(Rtpr,SelSet)
      andalso gb_sets:is_member(Rtsu,SelSet)
      andalso gb_sets:is_member(Ltpr,SelSet)
      andalso gb_sets:is_member(Ltsu,SelSet).          %% all selected?

check_angle(Edge, We, CosAngle) ->
    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
    NL = e3d_vec:norm(wings_face:normal(Lf,We)),
    NR = e3d_vec:norm(wings_face:normal(Rf,We)),
    Dot = e3d_vec:dot(NL,NR),
    (Dot >= CosAngle).

check_materials(Edge, We) ->
    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
    wings_facemat:face(Lf, We) =:= wings_facemat:face(Rf, We).

check_hard_edge(Edge, We) ->
    not gb_sets:is_member(Edge, We#we.he).

check_uv_coords(Edge, We) ->
    Luv = wings_va:attr(uv, wings_va:edge_attrs(Edge, left, We)),
    Ruv = wings_va:attr(uv, wings_va:edge_attrs(Edge, right, We)),
    #edge{ltpr=Ltpr, rtpr=Rtpr, lf=Lf, rf=Rf} =
	array:get(Edge, We#we.es),
    Luv2 = matching_uv(Lf, Ltpr, We),
    Ruv2 = matching_uv(Rf, Rtpr, We),
    compare_uvs(Luv, Ruv2) andalso compare_uvs(Ruv, Luv2).

matching_uv(Face, Edge, We) ->
    wings_va:attr(uv, wings_va:edge_attrs(Edge, Face, We)).

compare_uvs({U0,V0},{U1,V1}) ->
    %% allows for repeating textures
    Du = abs(U0 - U1),
    Dv = abs(V0 - V1),
    ((Du =< ?DEFAULT_UV_TOLERANCE) andalso (Dv =< ?DEFAULT_UV_TOLERANCE));
compare_uvs({_,_}, _) -> false;
compare_uvs(_, {_,_}) -> false;
compare_uvs(_, _) -> true.

check_concavity(Edge, We) ->          %% FIXME - kinda wasteful
    {_Val,Convex} = geometry_weight(Edge, We),
    Convex.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Preferences
%%

init_pref() ->
    Angle = get_pref(angle, ?DEFAULT_EDGE_ANGLE),
    UseHard = get_pref(usehard, true),
    UseMat = get_pref(usemat, true),
    UseUVs = get_pref(useuvs, true),
    Convex = get_pref(convex, true),
    AlgoLevel = get_pref(algolvl, ?DEFAULT_ALGO_LEVEL),
    put({?MODULE, angle},   Angle),
    put({?MODULE, usehard}, UseHard),
    put({?MODULE, usemat},  UseMat),
    put({?MODULE, useuvs},  UseUVs),
    put({?MODULE, convex},  Convex),
    put({?MODULE, algolvl}, AlgoLevel),
    ok.

pref_edit(St) ->
    Qs = get_ask_list(),
    wpa:dialog(?__(1,"Untriangulate Preferences (v0.2)"), [{vframe, Qs}],
                fun(Attrs) -> pref_result(Attrs, St) end).

pref_result([AlgoLevelAtom, Angle, 
             {usemat,UseMat},
             {useuvs,UseUVs},
             {usehard, UseHard},
             {convex, Convex}], St) ->
    [_H|AlgoNum] = atom_to_list(AlgoLevelAtom),
    AlgoLevel = list_to_integer(AlgoNum),
    save_pref(angle, Angle),
    save_pref(usemat, UseMat),
    save_pref(useuvs, UseUVs),
    save_pref(usehard, UseHard),
    save_pref(convex, Convex),
    save_pref(algolvl, AlgoLevel),
    St.

get_param(Param) ->
    get({?MODULE,Param}).

get_pref(Key, Val) ->
    wpa:pref_get(?MODULE, Key, Val).

save_pref(Key, Val) ->
    put({?MODULE,Key},Val),
    wpa:pref_set(?MODULE, Key, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Utilities
%%

-ifndef(DEBUG_UNTRI).
maybe_print_etree(_) -> ok.
-else.
maybe_print_etree(ETree) ->
    io:format("ETree: size: ~p\n", [gb_trees:size(ETree)]),
    case ETree of
      nil -> ok;
      _ ->
        foldl(fun({E,_},_) ->
                io:format("  ~p -> ", [E]),
                Erec = gb_trees:get(E, ETree),
                print_erec(Erec)
              end, [], gb_trees:to_list(ETree)),
        ok
    end.

print_erec(Erec) ->
    io:format("geo: ~p  nbs:", [Erec#erec.geoval]),
    foldl(fun(E,_) ->
            io:format(" ~p", [E])
          end, [], gb_sets:to_list(Erec#erec.nbs)),
    io:format("\n", []).
-endif.
