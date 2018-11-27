%%
%%  wings_bool.erl --
%%
%%     This module implements boolean commands for Wings objects.
%%
%%  Copyright (c) 2016 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_bool).
-export([add/1,isect/1,sub/1]).
-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-define(EPSILON, 1.0e-8).  %% used without SQRT() => 1.0e-4

%-define(DEBUG,true).
-ifdef(DEBUG).
%% Lots of debug stuff in here, maybe it can tell a thing or two of
%% the problems I have had with making this, sigh.
-define(DBG_TRY(Do,WE1,WE2),
        try Do
        catch error:__R ->
                ?dbg("ERROR: ~w:~n ~P~n", [__R, erlang:get_stacktrace(), 20]),
                #{we=>WE1,delete=>none, el=>[], sel_es=>[], error=>WE2};
              exit:_ ->
                #{we=>WE1,delete=>none, el=>[], sel_es=>[], error=>WE2};
              throw:{command_error,Message} ->
                io:format("***ERROR**: ~p~n", [Message]),
                #{we=>WE1,delete=>none, el=>[], sel_es=>[], error=>WE2}
        end).
-define(PUT(Id,We),put(Id,We)).
-define(TEST, true).
-define(D(F,A), ?dbg(F,A)).
-else.
-define(DBG_TRY(Do,We1,We2), Do).
-define(PUT(Id,We),ok).
-define(TEST, false).
-define(D(F,A), ok).
-endif.


add(St) ->
    Map = fun(_, We) -> init_isect(We, add) end,
    do_bool(St, Map).

isect(St) ->
    Map = fun(_, We) -> init_isect(We, isect) end,
    do_bool(St, Map).

sub(#st{sel=OrigSel0}=St0) ->
    case length(wings_sel:selected_ids(St0)) =:= 1 orelse ?TEST of
        true ->
            Do = fun(Subtract, St1) ->
                         St = ?SLOW(wings_sel:valid_sel(sub(Subtract, St1))),
                         {save_state, wings_obj:recreate_folder_system(St)}
                 end,
            OrigSel = gb_sets:from_list([Id || {Id, _} <- OrigSel0]),
            wings:ask(sub_ask(OrigSel), St0, Do);
        false ->
            wings_u:error_msg(?__(1, "Select the object to subtract from"))
    end.


sub(Subtract, St0) ->
    Map = fun(_, We) -> init_isect(We, add) end,
    Reduce = fun(Bvh, Acc) -> find_intersect(Bvh, sub, Acc) end,
    {Subs, Merged} = wings_sel:dfold(Map, Reduce, {Subtract, []}, St0),
    case Subs of
        Subtract -> St0;
        _ -> repeat(Subs, Merged, Map, Reduce, St0)
    end.

do_bool(St, Map) ->
    case length(wings_sel:selected_ids(St)) =:= 2 orelse ?TEST of
        true ->
            Reduce = fun(Bvh, Acc) -> find_intersect(Bvh, add, Acc) end,
            {Bvhs, Merged} = wings_sel:dfold(Map, Reduce, {[], []}, St),
            repeat(Bvhs, Merged, Map, Reduce, St);
        false ->
            wings_u:error_msg(?__(1, "Select two objects"))
    end.

repeat(Bvhs, [], _, _, St) ->
    finish(Bvhs, St);
repeat([], Merged, _, _, St) ->
    finish(Merged, St);
repeat(Bvhs0, Merged0, Map, Reduce, #st{shapes=Sh0}=St) ->
    Redo = fun(#{we:=We, sel_es:=Es0}, Acc) ->
                   Reduce(Map(gb_sets:empty(), We#we{temp={?MODULE,Es0}}),Acc)
           end,
    {Bvhs, Merged} = lists:foldl(Redo, {Bvhs0, []}, Merged0),
    Sh = lists:foldl(fun(#{delete:=Del}, Sh) ->
                             gb_trees:delete_any(Del, Sh)
                     end, Sh0, Merged0),
    case Bvhs0 =:= Bvhs of
        true ->
            finish(Bvhs++Merged, St);
        false ->
            repeat(Bvhs, Merged, Map, Reduce, St#st{shapes=Sh})
    end.

finish(Merged, #st{shapes=Sh0}=St0) ->
    Upd = fun(#{we:=#we{id=Id}=We}=MI, Sh1) ->
                  Sh2 = case maps:get(delete, MI, undefined) of
                            undefined -> Sh1;
                            Del -> gb_trees:delete_any(Del, Sh1)
                        end,
                  Sh3 = gb_trees:update(Id, We#we{temp=[]}, Sh2),
                  case maps:get(error, MI, undefined) of
                      undefined -> Sh3;
                      #we{id=IdDbg}=WeDbg ->
                          gb_trees:update(IdDbg, WeDbg, Sh3)
                  end
	  end,
    Sh = lists:foldl(Upd, Sh0, Merged),
    Sel = [{Id,gb_sets:from_list(Es)} || #{we:=#we{id=Id}, sel_es:=Es} <- Merged],
    wings_sel:set(edge, Sel, St0#st{shapes=Sh}).

sub_ask(OrigSel) ->
    Desc  = ?__(1,"Pick body to subtract from original selection"),
    Desc2 = ?__(2,"Select one body that is not original selection"),
    Fun = fun(check, St) ->
                  Sel = wings_sel:selected_ids(St),
                  case sub_is_valid_sel(Sel, OrigSel) of
                      false -> {none,Desc2};
                      true -> {none,[]}
		  end;
             (exit, {_, _, St}) ->
                  Sel = wings_sel:selected_ids(St),
                  case sub_is_valid_sel(Sel, OrigSel) of
                      false -> error;
                      true ->
                          Map = fun(_, We) -> init_isect(We, sub) end,
                          Reduce = fun(D,Acc) -> [D|Acc] end,
                          Sub = wings_sel:dfold(Map, Reduce, [], St),
                          {result, Sub}
                  end
	  end,
    {[{Fun,Desc}],[],[],[body]}.

sub_is_valid_sel([], _OrigSel) ->
    false;
sub_is_valid_sel(Sel, OrigSel) ->
    Set = gb_sets:from_list(Sel),
    case gb_sets:is_empty(gb_sets:intersection(Set, OrigSel)) of
        true when length(Sel) =:= 1 orelse ?TEST -> true;
        _ -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_intersect(Bvh, Op, {Bvhs0, Merged}) ->
    case find_intersect_1(Bvh, Bvhs0, []) of
        none when Op =:= sub -> {Bvhs0, [Bvh|Merged]};
        none -> {[Bvh|Bvhs0], Merged};
        {We, Bvhs} -> {Bvhs, [We|Merged]}
    end.

find_intersect_1(#{bvh:=B1}=Head, [#{bvh:=B2}=H1|Rest], Tested) ->
    case e3d_bvh:intersect(B1, B2) of
	[] ->  find_intersect_1(Head,Rest,[H1|Tested]);
	EdgeInfo ->
            Merged = merge_0(Head, H1, EdgeInfo),
            {Merged, Rest ++ Tested}
    end;
find_intersect_1(_Head, [], _) ->
    none.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_0(#{we:=We1}=I1, #{we:=We2}=I2, EdgeInfo0) ->
    ?D("~p STARTING~n~n",[?FUNCTION_NAME]),
    EdgeInfo = [remap(Edge, I1, I2) || Edge <- EdgeInfo0],
    ?PUT(we1,We1), ?PUT(we2,We2), %% Debug
    case [{MF1,MF2} || {coplanar, MF1, MF2} <- EdgeInfo] of
        [] -> ?DBG_TRY(merge_1(I1, I2, EdgeInfo, We1, We2), get(we1),get(we2));
        Coplanar ->
            %% ?D("~p coplanar tesselate and restart ~w ~n",[?FUNCTION_NAME, Coplanar]),
            tesselate_and_restart(Coplanar, I1, I2)
    end.

merge_1(#{we:=We10,el:=_EL10,temp_es:=TEs1}=I10,
        #{we:=We20,el:=_EL20, temp_es:=TEs2}=I20,
        EdgeInfo0, We1, We2) ->
    ?D("~p~n",[?FUNCTION_NAME]),
    {Vmap, EdgeInfo} = make_vmap(EdgeInfo0, We10, We20),  %% Make vertex id => pos and update edges
    %?D("Vmap: ~p~n",[array:to_orddict(Vmap)]),
    Loops0 = build_vtx_loops(EdgeInfo), %% Figure out edge loops
    L10 = [split_loop(Loop, Vmap, {We10,We20}) || Loop <- Loops0], % Split loops per We and precalc
    L20 = [split_loop(Loop, Vmap, {We20,We10}) || Loop <- Loops0], % some data
    %% Remove vertexes on triangulated edges
    Loops1 = [filter_tri_edges(Loop,We10,We20) || Loop <- lists:zip(L10,L20)],
    Loops = sort_largest(Loops1, Vmap),
    %% Create vertices on the edge-loops
    {Res, I11, I21} = make_verts(Loops, Vmap, TEs1, TEs2, We10, We20),
    merge_2(Res, maps:merge(I10, I11), maps:merge(I20, I21), We1, We2).

%% Continuing: multiple edge loops have hit the same face. It was
%% really hard to handle that in one pass, since faces are split and
%% moved.  Solved it by doing the intersection test again for the new
%% faces and start over
merge_2(cont, #{we:=We11, fs:=Fs1}=I10, #{we:=We21, fs:=Fs2}=I20, We1,We2) ->
    ?D("~p cont~n",[?FUNCTION_NAME]),
    {We12, Vmap1, Es1, B1} = remake_bvh(Fs1, We1, We11),
    {We22, Vmap2, Es2, B2} = remake_bvh(Fs2, We2, We21),
    EI0 = e3d_bvh:intersect(B1, B2),
    I11 = maps:update_with(temp_es,fun(EL) -> gb_sets:union(Es1,EL) end, I10#{we=>We12,map=>Vmap1}),
    I21 = maps:update_with(temp_es,fun(EL) -> gb_sets:union(Es2,EL) end, I20#{we=>We22,map=>Vmap2}),
    EI = [remap(Edge, I11, I21) || Edge <- EI0],
    %% We should crash if we have coplanar faces in this step
    ?DBG_TRY(merge_1(I11,I21,EI,We1,We2), We12,We22);
    %% #{we=>We11,delete=>none, el=>[], sel_es=>[], error=>We21};

%% All edge loops are in place, dissolve faces inside edge loops and
%% merge the two we's
merge_2(done, #{we:=_We1} = I1, #{we:=_We2} = I2, #we{id=Id1}, #we{id=Id2}) ->
    ?D("~p ~p ~p done~n",[?FUNCTION_NAME, Id1, Id2]),

    {DRes1,DRes2} = build_parts(I1, I2),
    Weld = fun() ->
                   {We,Es} = weld(DRes1, DRes2),
                   [Del] = lists:delete(We#we.id, [Id1,Id2]),
                   ok = wings_we_util:validate(We),
                   #{sel_es=>Es, we=>We, delete=>Del}
           end,
    ?DBG_TRY(Weld(), element(3, DRes1), element(3, DRes2)).
    %% ?DBG_TRY(Weld(), _We1, _We2).

sort_largest(Loops, Vmap) ->
    OnV = fun(#{e:=on_vertex}) -> true; (_) -> false end,
    Early = fun({L1,L2}) ->
                    case lists:all(OnV,L1) andalso lists:all(OnV,L2) of
                        true -> 0;
                        false -> 1000000
                    end
            end,
    Length = fun Lenght([#{v:=V1}|[#{v:=V2}|_]=R], Sum) ->
                     Lenght(R, Sum + e3d_vec:dist_sqr(vmap_pos(V1, Vmap),vmap_pos(V2, Vmap)));
                 Lenght([_], Sum) -> Sum
             end,
    Ls0 = [{Length([lists:last(L1)|L1], 0)+Early(Loop), Loop} || {L1,_} = Loop <- Loops],
    [L || {_, L} <- lists:sort(Ls0)].

remake_bvh(Fs0, We0, We1) ->
    Fs1 = gb_sets:union(Fs0,wings_we:new_items_as_gbset(face,We0,We1)),
    We = wings_tesselation:quadrangulate(Fs1, We1),
    Fs = gb_sets:union(Fs1,wings_we:new_items_as_gbset(face,We1,We)),
    Es = wings_we:new_items_as_gbset(edge,We1,We),
    %% ?D("Tess ~w: ~w Es ~w~n", [We0#we.id, gb_sets:to_list(Fs1), gb_sets:to_list(Es)]),
    {Vmap, Bvh} = make_bvh(gb_sets:to_list(Fs), We),
    {We, Vmap, Es, Bvh}.

%% Coplanar faces are often caused by bad triangulations
tesselate_and_restart(Coplanar, #{we:=#we{id=Id1}=We1, op:=Op1},
                      #{we:=#we{id=Id2}=We2,op:=Op2}) ->
    {L1,L2} = lists:unzip(Coplanar),
    Tess = sofs:to_external(sofs:relation_to_family(sofs:relation(L1++L2))),
    We10 = tesselate_faces(proplists:get_value(Id1,Tess), We1),
    We20 = tesselate_faces(proplists:get_value(Id2,Tess), We2),
    {Vmap1, B1} = make_bvh(We10),
    {Vmap2, B2} = make_bvh(We20),
    EI0 = e3d_bvh:intersect(B1, B2),
    I11 = #{we=>We10,map=>Vmap1,el=>[],op=>Op1, bvh=>B1, temp_es=>gb_sets:empty()},
    I21 = #{we=>We20,map=>Vmap2,el=>[],op=>Op2, bvh=>B2, temp_es=>gb_sets:empty()},
    EI = [remap(Edge, I11, I21) || Edge <- EI0],
    merge_1(I11,I21,EI,We1,We2). %% We should crash if we have coplanar faces in this step

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_parts(#{we:=#we{id=WeId1,temp=PSel1}=We10, temp_es:=TEs1, op:=Op1, bvh:=B1}=I1,
            #{we:=#we{id=WeId2,temp=PSel2}=We20, temp_es:=TEs2, op:=Op2, bvh:=B2}=I2) ->
    %% We must use the original bvh's, the recursive ones don't have all faces
    %% OBS we test faces against the other bvh
    LM0 = #{{we,WeId1}=>We10, {temp_es, WeId1}=>gb_sets:to_list(TEs1),
            {we,WeId2}=>We20, {temp_es, WeId2}=>gb_sets:to_list(TEs2)},
    PartsAndLoops0 = build_parts_0(I2, B1, {LM0, array:new(), array:new()}),
    PartsAndLoops  = build_parts_0(I1, B2, PartsAndLoops0),

    {LM, Parts, LoopMap} = dissolve_ps(WeId1, Op1, WeId2, Op2, PartsAndLoops),

    %% ?D("VLoops: ~n",[]), [io:format(" ~W~n",[E,20]) || E <- array:to_orddict(LoopMap)],
    %% ?D("Parts: ~n",[]), [io:format(" ~W~n",[P,20]) || P <- array:to_orddict(Parts)],

    Dict = array:to_orddict(Parts),
    FindFace = fun({LId, #{ps:=Ps}}, WeId, Acc) ->
                       Fs = [Face || {PId, #{we:=We, intsect:=done, ls:=[Id], fs:=[Face]}} <- Dict,
                                     LId =:= Id, We =:= WeId, lists:member(PId, Ps)],
                       case Fs of
                           [Face] -> [{LId,Face}|Acc];
                           [] -> Acc
                       end
               end,
    Fs1 = lists:foldl(fun(L,Acc) -> FindFace(L, WeId1, Acc) end,
                      [], array:to_orddict(LoopMap)),
    Fs2 = lists:foldl(fun(L,Acc) -> FindFace(L, WeId2, Acc) end,
                      [], array:to_orddict(LoopMap)),

    We1 = maps:get({we, WeId1}, LM),
    We2 = maps:get({we, WeId2}, LM),

    ?D("Merge:  ~w:~w~n\t\t\t~w:~w~n",[WeId1,Fs1,WeId2,Fs2]),
    {
     {[F||{_,F} <- Fs1], maps:get({temp_es, WeId1}, LM), invert_normals(Op1, We1#we{temp=PSel1})},
     {[F||{_,F} <- Fs2], maps:get({temp_es, WeId2}, LM), invert_normals(Op2, We2#we{temp=PSel2})}
    }.

invert_normals(sub, We) -> wings_we:invert_normals(We);
invert_normals(_, We) -> We.

build_parts_0(#{el:=EFLs, we:=We, vmap:=Vmap0}, OBvh, PaL) ->
    %% ?D("Build Graph: ~w~n",[We#we.id]),
    Vmap1 = lists:sort([{B,A} || {A,B} <- array:to_orddict(Vmap0), is_integer(B)]),
    Vmap1 = ordsets:from_list(Vmap1), %% Assert duplicates
    Vmap  = array:from_orddict(Vmap1),

    Es = gb_sets:from_list([E || Es <- EFLs, E <- Es]),
    Parts0 = wings_edge_cmd:loop_cut_partition(Es, We),
    Rest = gb_sets:difference(tree_to_set(We#we.fs), gb_sets:union(Parts0)),
    Parts = case gb_sets:is_empty(Rest) of
                true -> Parts0;
                false -> [Rest|Parts0]
            end,
    SetupParts = fun(P, Acc) -> setup_part(P, OBvh, Vmap, We, Acc) end,
    lists:foldl(SetupParts, PaL, Parts).

setup_part(PGBS, OBvh, Vmap, #we{id=WeId}=We, {LT, Parts, LArr0}) ->
    [Face|_] = P = gb_sets:to_list(PGBS),
    PId = array:size(Parts),
    Outer = wings_face:outer_edges(P, We),
    Loops0 = wings_edge_loop:edge_loop_vertices(Outer, We),
    SetupLoop = fun(VL0,LArr) ->
                        VL = [array:get(V, Vmap) || V <- VL0],
                        Loop = #{search=>ordsets:from_list(VL),
                                 orig=>{WeId,VL0}},
                        setup_loop(Loop, PId, none, LArr)
                end,
    {Loops1, LArr} = lists:mapfoldl(SetupLoop, LArr0, Loops0),
    {Loops,Partial} = lists:unzip(Loops1),
    Pos = case ordsets:subtract(wings_face:to_edges([Face], We), Outer) of
              [] -> wings_face:center(Face,We);
              [Edge|_] ->
                  [A,B] = wings_edge:to_vertices([Edge], We),
                  e3d_vec:average(wings_vertex:pos(A,We),wings_vertex:pos(B,We))
          end,
    ISect = e3d_bvh:is_inside(Pos, OBvh),
    %% ?D("Test ~w:~w => ~w ~n",[WeId, Face, ISect]),
    Part = #{we=>WeId, fs=>P, ls=>Loops, nb=>lists:append(Partial), intsect=>ISect},
    {LT#{{vmap,WeId}=>Vmap}, array:set(PId,Part,Parts), LArr}.

setup_loop(#{search:=Search}=Loop, PId, Old, Arr) ->
    Find = fun(Id, #{search:=L}=LD, Acc) ->
                   if L =:= Search -> throw({found,Id, LD});
                      true -> case ordsets:intersection(Search, L) of
                                  [_,_,_|_] when Old =:= none -> [Id|Acc];
                                  _Part -> Acc
                              end
                   end
           end,
    try array:foldl(Find, [], Arr) of
        [] when Old =:= none ->
            Next = array:size(Arr),
            {{Next,[]}, array:set(Next, Loop#{ps=>[PId]}, Arr)};
        Ids when Old =:= none ->
            Next = array:size(Arr),
            {{Next, Ids}, array:set(Next, Loop#{ps=>[PId]}, Arr)}
    catch throw:{found, Id, #{ps:=Parts}=L} ->
            Ps = lists:reverse([PId|lists:delete(Old,Parts)]),
            {{Id, []}, array:set(Id, L#{ps:=Ps}, Arr)}
    end.

dissolve_ps(Id1, Op1, Id2, Op2, {_,Parts0, _} = PALs0) ->
    Compl = fun(_, P, {C1,C2}) -> {compl_ps(Id1, P, C1),compl_ps(Id2, P, C2)} end,
    {ComplL10,ComplL2} = array:foldl(Compl, {false,false}, Parts0),
    ComplL1 = case {Op1, Op2} of
                  {add,sub} when ComplL2 =:= true -> true;
                  _ -> ComplL10
              end,

    Split = fun(Id, P, {L1,L2}) ->
                    {pick_ps(Id, P, Id1, Op1 =/= add, ComplL1, L1),
                     pick_ps(Id, P, Id2, Op2 =/= add, ComplL2, L2)}
            end,
    {L1,L2} = array:foldl(Split, {[], []}, Parts0),
    %% ?D("L1 ~w:compl ~w:sub ~w: ~w~n",  [Id1, ComplL1,Op1 =/= add,L1]),
    %% ?D("L2 ~w:compl ~w:sub ~w: ~w~n~n",[Id2, ComplL2,Op2 =/= add,L2]),
    case {ComplL1, ComplL2} of
        {_, false} ->
            {ok, PALs1} = dissolve_ps(L2, ComplL2, PALs0, ok),
            merge_wes(dissolve_ps(L1, ComplL1, PALs1, []));
        {false, true} ->
            {ok, PALs1} = dissolve_ps(L1, ComplL1, PALs0, ok),
            merge_wes(dissolve_ps(L2, ComplL2, PALs1, []));
        {true, true} when Op2 =:= sub ->
            PALs1 = merge_wes(dissolve_ps(L1, ComplL1, PALs0, [])),
            merge_wes(dissolve_ps(L2, ComplL2, PALs1, []))
    end.

compl_ps(We, #{we:=We,nb:=[_|_]}, _) -> true;
compl_ps(_, _, Keep) -> Keep.

pick_ps(Id, #{intsect:=I, we:=We}, We, Inv, Compl, Acc)
  when (Inv xor I) =:= (not Compl) ->
    [Id|Acc];
pick_ps(_, _, _, _, _, Acc) ->
    Acc.

dissolve_ps([PId0|Ids], false, {Lookup, Parts0, LMap0}, Acc) ->
    PId = pick_part(PId0, false, Parts0, LMap0),
    #{we:=Id, fs:=IsectFs} = array:get(PId, Parts0),
    We0 = maps:get({we,Id}, Lookup),
    Vmap = maps:get({vmap,Id}, Lookup),
    We = wings_dissolve:faces(IsectFs, We0),
    Fs = wings_we:new_items_as_ordset(face, We0, We),
    {Parts, LMap,_} = update_loops(Fs, PId, Parts0, LMap0, Vmap, We, []),
    dissolve_ps(Ids, false, {Lookup#{{we,Id}:=We}, Parts, LMap}, Acc);
dissolve_ps([PId0|Ids], true, {Lookup, Parts0, LMap0}, Acc) ->
    PId = pick_part(PId0, true, Parts0, LMap0),
    case PId of
        ignore ->
            dissolve_ps(Ids, true, {Lookup, Parts0, LMap0}, Acc);
        _ ->
            #{we:=Id, fs:=IsectFs} = array:get(PId, Parts0),
            We0 = maps:get({we,Id}, Lookup),
            Vmap = maps:get({vmap,Id}, Lookup),
            We = wings_dissolve:complement(IsectFs, We0),
            Fs = wings_we:new_items_as_ordset(face, We0, We),
            {Parts, LMap, PF} = update_loops(Fs, PId, Parts0, LMap0, Vmap, We, []),
            dissolve_ps(Ids, true, {Lookup, Parts, LMap}, [{PF,We}|Acc])
    end;
dissolve_ps([], _, PALs, Acc) ->
    {Acc, PALs}.

merge_wes({[], PALs}) ->
    PALs;
merge_wes({Wes, {Lookup, Parts0, LMap}}) ->
    [{_, #we{id=Id}}|_] = Wes,
    TEs0 = maps:get({temp_es, Id}, Lookup),
    MakeRoot = fun({PFs,#we{es=Etab}=We}) ->
                       Es = ordsets:intersection(TEs0, wings_util:array_keys(Etab)),
                       Fs = [{face, [Face], Pid} || {Pid, Face} <- PFs],
                       {We,[{edge,Es,temp}|Fs]}
               end,
    WithRs = lists:map(MakeRoot, Wes),
    {We,Rs} = wings_we:merge_root_set(WithRs),
    Upd = fun({face, [Face], Pid}, Parts) ->
                  Part = array:get(Pid, Parts),
                  array:set(Pid, Part#{fs:=[Face]}, Parts);
             ({edge,_,_}, Parts) -> Parts
          end,
    Parts = lists:foldl(Upd, Parts0, Rs),
    TEs = [Edge || {edge,Es,temp} <- Rs, Edge <- Es],
    {Lookup#{{we,Id}:=We, {temp_es,Id}:=TEs}, Parts, LMap}.

pick_part(PId, false, _Parts, _LMap) ->
    PId;
pick_part(PId, true, Parts, _LMap) ->
    case array:get(PId, Parts) of
        #{intsect:=false} -> PId;
        #{intsect:=true} -> PId;
        #{intsect:=ignore} -> ignore
    end.

update_loops([Face|Fs], PartId, Parts0, LMap0, VMap, We, Acc) ->
    Search = [array:get(V, VMap) || V <- wings_face:to_vertices([Face], We)],
    NewId = array:size(Parts0),
    {{LId,[]},LMap} = setup_loop(#{search=>ordsets:from_list(Search)}, NewId, none, LMap0),
    Part = array:get(PartId, Parts0),
    Parts = array:set(NewId, Part#{fs:=[Face], ls:=[LId], intsect:=done}, Parts0),
    %% ?D("Upd: ~w => ~w: ~w~n",[PartId,NewId, Part#{fs:=[Face], ls:=[LId], intsect:=done}]),
    update_loops(Fs, PartId, Parts, LMap, VMap, We, [{NewId,Face}|Acc]);
update_loops([], PartId, Parts0, LMap, _, _, Acc) ->
    Part = array:get(PartId, Parts0),
    {array:set(PartId, Part#{intsect:=ignore}, Parts0), LMap, Acc}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Weld
%% Merge the two We's and bridge corresponding face-pairs
weld({Fs01,TEs01,#we{temp={?MODULE,Es01}}=We01}, {Fs02,TEs02,#we{temp={?MODULE, Es02}}=We02}) ->
    TEs1 = ordsets:intersection(TEs01, wings_util:array_keys(We01#we.es)),
    TEs2 = ordsets:intersection(TEs02, wings_util:array_keys(We02#we.es)),
    WeRs = [{We01, [{face,Fs01,weld}, {edge,Es01,border}, {edge,TEs1,temp}]},
            {We02, [{face,Fs02,weld}, {edge,Es02,border}, {edge,TEs2,temp}]}],
    {We0,Rs} = wings_we:merge_root_set(WeRs),
    [Fs1,Fs2] = [Fs || {face,Fs,weld} <- Rs],
    FacePairs = lists:zip(Fs1,Fs2),
    SelEs = lists:append([Es || {edge,Es,border} <- Rs]),
    TempEs = lists:append([Es || {edge,Es,temp} <- Rs]),
    ?PUT(we1,We01),
    ?PUT(we2,undefined),
    Weld = fun({F1,F2}, WeAcc) -> do_weld(F1,F2,WeAcc) end,
    {We1, BorderEs0} = lists:foldl(Weld, {We0,[]}, FacePairs),
    BorderEs1 = ordsets:from_list(BorderEs0),

    %% Cleanup temp edges (and it's faces)
    {CFs0, We2} = cleanup_temp_edges(TempEs, We1),
    %% dissolve rebuilds we need to update
    BorderEs2 = ordsets:intersection(BorderEs1, wings_util:array_keys(We2#we.es)),
    %% We really need to keep track of edges (and its vertices) for the second
    %% pass if temp edges is left and bordering faces get quadrangalute it leaves
    %% new edges..
    CFS1 = ordsets:intersection(CFs0, gb_trees:keys(We2#we.fs)),
    CFS2 = [Face || Face <- CFS1, wings_face:vertices(Face, We2) > 5],
    We3 = wings_tesselation:quadrangulate(CFS2, We2),
    {_, We4} = cleanup_temp_edges(TempEs, We3),

    %% Calculate border edges and quadrangulate border faces
    Es = wings_util:array_keys(We4#we.es),
    Borders = ordsets:intersection(ordsets:from_list(BorderEs2++SelEs), Es),
    BorderFs = gb_sets:to_list(wings_face:from_edges(Borders, We4)),
    Tess1 = [Face || Face <- BorderFs, wings_face:vertices(Face, We4) > 5],
    We = wings_tesselation:quadrangulate(Tess1, We4),
    ok = wings_we_util:validate(We),
    {wings_facemat:gc(We), Borders}.

do_weld(Fa, Fb, {We0, Acc}) ->
    [Va|_] = wings_face:vertices_ccw(Fa, We0),
    Pos = wings_vertex:pos(Va, We0),
    Find = fun(Vb, _, _, Vs) ->
                   case e3d_vec:dist_sqr(wings_vertex:pos(Vb, We0), Pos) < ?EPSILON of
                       true -> [Vb|Vs];
                       false -> Vs
                   end
           end,
    [Vb] = wings_face:fold(Find, [], Fb, We0),
    %% Bridge and collapse new edges
    We1 = wings_face_cmd:force_bridge(Fa, Va, Fb, Vb, We0),
    Es = wings_we:new_items_as_ordset(edge, We0, We1),
    We = lists:foldl(fun(E, W) -> wings_collapse:collapse_edge(E, W) end, We1, Es),
    %% Find selection
    BorderEdges = wings_face:to_edges([Fa,Fb], We0),
    {We, BorderEdges ++ Acc}.

cleanup_temp_edges(DelEs0, #we{es=Etab}=We0) ->
    Es = wings_util:array_keys(Etab),
    case ordsets:intersection(ordsets:from_list(DelEs0), Es) of
        [] -> {[], We0};
        DelEs ->
            %% ?D("Dissolve Es: ~w: ~w~n", [We0#we.id, DelEs]),
            Fs = gb_sets:to_list(wings_face:from_edges(DelEs, We0)),
            {We1,_Bad} = wings_edge:dissolve_edges(DelEs, Fs, We0),
            ?D("Bad faces: ~w~n",[_Bad]),
            Vs = wings_edge:to_vertices(DelEs, We0),
            {Fs, wings_edge:dissolve_isolated_vs(Vs, We1)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_verts(Loops, Vmap, TEs1, TEs2, We1, We2) ->
    Empty = gb_sets:empty(),
    %% ?D("Temp ~p:~w~n",[We1#we.id, gb_sets:to_list(TEs1)]),
    %% ?D("Temp ~p:~w~n",[We2#we.id, gb_sets:to_list(TEs2)]),
    make_verts(Loops, Vmap, Empty, We1, Vmap, Empty, We2, TEs1, TEs2, [], []).

make_verts([{L1,L2}=L12|Ls], Vm10, Fs10, We10, Vm20, Fs20, We20, TEs10, TEs20, Acc, Cont) ->
    case check_if_used(L1,Fs10) orelse check_if_used(L2,Fs20) of
	true ->
	    make_verts(Ls, Vm10, Fs10, We10, Vm20, Fs20, We20, TEs10, TEs20, Acc, [L12|Cont]);
	false ->
	    {Es1, TEs1, Vm1, We1} = make_verts_per_we(L1, TEs10, Vm10, We10),
	    {Es2, TEs2, Vm2, We2} = make_verts_per_we(L2, TEs20, Vm20, We20),
            EL = [{Es1,Es2}|Acc],
            case We1 =:= We10 andalso We2 =:= We20 of
                true ->
                    make_verts(Ls, Vm1, Fs10, We10, Vm2, Fs20, We20, TEs10, TEs20, EL, Cont);
                false ->
                    Fs1 = gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L1]), Fs10),
                    Fs2 = gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L2]), Fs20),
                    ?PUT(we1,We1), ?PUT(we2,We2),
                    make_verts(Ls, Vm1, Fs1, We1, Vm2, Fs2, We2, TEs1,TEs2, EL, Cont)
            end
    end;
make_verts([], Vmap1, Fs10, We1, Vmap2, Fs20, We2, TEs1, TEs2, Acc, Cont) ->
    {Es1, Es2} = lists:unzip(Acc),
    %% ?D("Temp ~p:~w~n",[We1#we.id, gb_sets:to_list(TEs1)]),
    %% ?D("Temp ~p:~w~n",[We2#we.id, gb_sets:to_list(TEs2)]),
    I1 = #{we=>We1, el=>Es1, vmap=>Vmap1, temp_es=>TEs1},
    I2 = #{we=>We2, el=>Es2, vmap=>Vmap2, temp_es=>TEs2},
    case Cont of
	[] ->
	    {done, I1, I2};
	_ ->
	    Add = fun({L1,L2}, {F1,F2}) ->
			  {gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L1]),F1),
			   gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L2]),F2)}
		  end,
	    {Fs11,Fs21} = lists:foldl(Add, {Fs10,Fs20}, Cont),
            Fs1 = gb_sets:intersection(Fs11, tree_to_set(We1#we.fs)),
            Fs2 = gb_sets:intersection(Fs21, tree_to_set(We2#we.fs)),
            {cont, I1#{fs=>Fs1}, I2#{fs=>Fs2}}
    end.

check_if_used(Loop, Fs) ->
    case gb_sets:is_empty(Fs) of
	true -> false;
	false ->
	    Next = [F || #{f:=F} <- Loop],
	    Int = gb_sets:intersection(gb_sets:from_list(Next), Fs),
	    not gb_sets:is_empty(Int)
    end.

make_verts_per_we(Loop, TEs0, Vmap0, We0) ->
    %%?D("We ~w Make verts:~n",[We0#we.id]),[io:format(" ~w~n", [E]) || E <- Loop],
    {TEs, Vmap, We1} = cut_edges(Loop, TEs0, Vmap0, We0),
    make_edge_loop(Loop, TEs, Vmap, [], We1).

cut_edges(SE, TEs0, Vmap, We0) ->
    WiEs = [{E,Vn} || #{op:=split_edge, e:=E, v:=Vn} <- SE],
    ECuts = sofs:to_external(sofs:relation_to_family(sofs:relation(WiEs, [{edge,vn}]))),
    lists:foldl(fun cut_edge/2, {TEs0, Vmap, We0}, ECuts).

cut_edge({on_vertex, Vs}, {TEs0, Vmap, #we{id=Id}=We}) ->
    {TEs0,
     lists:foldl(fun(V, VM) ->
                         case array:get(V, Vmap) of
                             {Where, _Pos} ->
                                 Vi = proplists:get_value(Id, Where),
                                 array:set(V,Vi,VM);
                             Vi when is_integer(Vi) ->
                                 VM
                         end
                 end,
                 Vmap,Vs),
     We};
cut_edge({Edge, [V]}, {TEs, Vmap, #we{id=Id}=We0}) ->
    {Where,Pos} = array:get(V, Vmap),
    case proplists:get_value(Id, Where) of
        undefined ->
            {We, New} = wings_edge:fast_cut(Edge, Pos, We0),
            {temp_edge(Edge, New, TEs), array:set(V, New, Vmap), We};
        Vi ->
            {TEs, array:set(V, Vi, Vmap), We0}
    end;
cut_edge({Edge, Vs}, {TEs0, Vmap0, #we{es=Etab}=We0}) ->
    #edge{vs=VS} = array:get(Edge, Etab),
    P1 = wings_vertex:pos(VS,We0),
    C = fun(V) ->
                Pos = vmap_pos(V, Vmap0),
                Dist2 = e3d_vec:dist_sqr(Pos, P1),
                {Dist2, V, Pos}
        end,
    VsPos = lists:sort([C(V) || V <- Vs]),
    Cut = fun({_,V,Pos}, {WE, E, Temp, Vm}) ->
                  {We, New} = wings_edge:fast_cut(E, Pos, WE),
                  {We, New, temp_edge(E, New, Temp), array:set(V, New, Vm)}
          end,
    {We,_,TEs, Vmap} = lists:foldl(Cut, {We0, Edge, TEs0, Vmap0}, VsPos),
    {TEs, Vmap, We}.

%% If cut edge is in Temp Edges Set add the new one
temp_edge(Edge, New, Set) ->
    case gb_sets:is_member(Edge, Set) of
        true -> gb_sets:add_element(New, Set);
        false -> Set
    end.

make_edge_loop([#{op:=split_edge}=F|_]=Loop, TEs0, Vmap0, EL0, We0) ->
    make_edge_loop_1(Loop, F, TEs0, Vmap0, EL0, We0);
make_edge_loop(Loop, TEs, Vmap, EL, We) ->
    %% Start with split_edge
    case lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Loop) of
        {FSs, []} -> %% No edges intersect, make a face inside the intersecting face
            inset_face(FSs, TEs, Vmap, We);
        {FSs, Edges} -> %% Connect edges and create new verts
            make_edge_loop(Edges++FSs, TEs, Vmap, EL, We)
    end.

make_edge_loop_1([V1], V1, TEs, Vmap, EL, We) ->
    {EL, TEs, Vmap, We};
make_edge_loop_1([#{op:=split_edge}=V1],#{op:=split_edge}=V2,
                 TEs, Vmap, EL, We0) ->
    {We, New} = connect_verts(V1,V2,Vmap,We0),
    {[New|EL], TEs, Vmap, We};
make_edge_loop_1([#{op:=split_edge}=V1|[#{op:=split_edge}=V2|_]=Rest],
                 Last, TEs, Vmap, EL, We0) ->
    {We, New} = connect_verts(V1,V2,Vmap, We0),
    make_edge_loop_1(Rest, Last, TEs, Vmap, [New|EL], We);
make_edge_loop_1([#{op:=split_edge}=V1|Splits], Last, TEs, Vmap, EL0, We0) ->
    {FSs, [V2|_]=Rest} =
        case lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Splits) of
            {FS, []} -> {FS, [Last]};
            {FS, Rs} -> {FS, Rs}
        end,
    case edge_exists(V1,V2,Vmap,We0) of
        [] -> %% Standard case
            {We1, Edge} = connect_verts(V1,V2,Vmap,We0),
            {EL1,Vmap1,We} = make_face_vs(FSs, V1, Edge, Vmap, We1),
            make_edge_loop_1(Rest, Last, TEs, Vmap1, EL1++EL0, We);
        [{Edge,_F1,_F2}] ->
            {EL1,Vmap1,We} = half_inset_face(V1,V2,FSs,Edge,Vmap,We0),
            make_edge_loop_1(Rest, Last, TEs, Vmap1, EL1++EL0, We)
    end.

edge_exists(#{v:=V10},#{v:=V20},Vmap,We) ->
    wings_vertex:edge_through(array:get(V10,Vmap),array:get(V20,Vmap),We).

connect_verts(V1, V2, Vmap, We) ->
    {WeV1,WeV2,Face,OtherN} = pick_face(V1,V2, Vmap, We),
    connect_verts_1(WeV1, WeV2, Face, OtherN, We).

connect_verts_1(WeV1, WeV2, Face, CrossDir, #we{vp=Vtab}=We) ->
    %% ?D("~w: ~w ~w in ~w ~s~n",[We#we.id, WeV1, WeV2, Face, vf(CrossDir)]),
    V1 = array:get(WeV1,Vtab),
    V2 = array:get(WeV2,Vtab),
    case wings_vertex:edge_through(WeV1,WeV2,Face,We) of
        none ->
            N = wings_face:normal(Face, We),
            Dir = e3d_vec:cross(N,e3d_vec:norm_sub(V1,V2)),
            %% ?D("Swap: ~.3f (~s) ~n", [e3d_vec:dot(CrossDir, Dir), vf(Dir)]),
            Inside = 0 >= e3d_vec:dot(CrossDir, Dir),
            case Inside of
                true  -> wings_vertex:force_connect(WeV1,WeV2,Face,We);
                false -> wings_vertex:force_connect(WeV2,WeV1,Face,We)
            end;
        Edge ->
            {We, Edge}
    end.

pick_face(#{v:=V1,o_n:=N1}, #{v:=V2,o_n:=N2}, Vmap, We) ->
    WeV1 = array:get(V1, Vmap),
    WeV2 = array:get(V2, Vmap),
    true = is_integer(WeV1), true = is_integer(WeV2), %% Assert
    OtherN = e3d_vec:norm(e3d_vec:average(N1,N2)),
    [Face|_] = [Face || {Face, [_,_]} <- wings_vertex:per_face([WeV1,WeV2],We)],
    {WeV1,WeV2,Face,OtherN}.

pick_face_2(Wanted, Fs, Edge, #we{id=_Id,es=Etab}) ->
    #edge{lf=LF, rf=RF} = array:get(Edge, Etab),
    case Fs of
        {Wanted,_} -> LF;
        {_,Wanted} -> RF
    end.

pick_ref_face([#{f:=F}|Ss], undefined) ->
    pick_ref_face(Ss, F);
pick_ref_face([#{f:=F}|Ss], F) ->
    pick_ref_face(Ss, F);
pick_ref_face([], F) -> F.

half_inset_face(#{v:=EV1,o_n:=ON, fs:=CFs}=R0, #{v:=EV2}=R1, [#{v:=NV}=R2|FSs],
                RefEdge, Vmap0, We0) ->
    E1 = array:get(EV1, Vmap0),
    E2 = array:get(EV2, Vmap0),
    Wanted = pick_ref_face([R2|FSs], undefined),
    Face = pick_face_2(Wanted, CFs, RefEdge, We0),
    % ?D("Half inset: ~p(~p) ~p(~p) in ~p~n",[EV1,E1,EV2,E2,Face]),
    FVs0 = wings_face:vertices_ccw(Face, We0),
    FVs = order_vertex_list(E1, E2, FVs0),
    [E1,_Skip,E3|_FVs] = FVs,
    {We1,Edge0} = connect_verts_1(E1,E3,Face,ON,We0),
    Pos = vmap_pos(NV,Vmap0),
    {We2,Edge1} = wings_edge:fast_cut(Edge0,Pos,We1),
    V0 = Edge1,
    {Keep,Remove} = case is_first(E1,Edge0,We2) of
                        true -> {Edge0, Edge1};
                        false -> {Edge1, Edge0}
                    end,
    Dir2 = e3d_vec:norm(e3d_vec:average([N || #{o_n:=N} <- [R0,R1,R2|FSs]])),
    %% Make verts on the new edge
    Other = wings_face:other(Face, array:get(Keep, We2#we.es)),

    try connect_verts_1(V0,E2,Face,Dir2,We2) of
        {We3,Edge2} ->
            {EL, Vmap, We4} = make_face_vs_0(FSs, V0, Edge2, array:set(NV,V0,Vmap0), We3),
            We = wings_edge:dissolve_edge(Remove,We4),
            {[Keep|EL], Vmap, We}
    catch _:{badmatch,false} ->
            {We3,Edge2} = connect_verts_1(V0,E2,Other,Dir2,We2),
            {EL, Vmap, We4} = make_face_vs_0(FSs, V0, Edge2, array:set(NV,V0,Vmap0), We3),
            We = wings_edge:dissolve_edge(Remove,We4),
            {[Keep|EL], Vmap, We}
    end.

order_vertex_list(First, Last, FVs0) ->
    %%?D("~w ~w in ~w~n",[First, Last, FVs0]),
    {VL1,VL2} = lists:splitwith(fun(V) when V =:= First -> false; (_) -> true end, FVs0),
    case VL2++VL1 of
        [First|[Last|_]=R] -> [First|lists:reverse(R)];
        L -> Last = lists:last(VL1), L
    end.

is_first(V, Edge, #we{es=Etab}) ->
    #edge{vs=Vs} = array:get(Edge, Etab),
    V =:= Vs.

make_face_vs([_]=Ss, _Vs, Edge, Vmap, We) ->
    make_face_vs_1(Ss, Edge, Vmap, [Edge], We);
make_face_vs(Ss, #{v:=Vs0}, Edge, Vmap, We) ->
    make_face_vs_0(Ss, array:get(Vs0, Vmap), Edge, Vmap, We).

make_face_vs_0(Ss, Vs, Edge, Vmap, We) ->
    case is_first(Vs, Edge, We) of
        true ->
            make_face_vs_1(Ss, Edge, Vmap, [Edge], We);
        false ->
            {EL1,VM1,WE1} = make_face_vs_1(lists:reverse(Ss), Edge, Vmap, [Edge], We),
            {lists:reverse(EL1),VM1,WE1}
    end.

make_face_vs_1([#{op:=split_face,v:=V}|Ss], Edge, Vmap, EL, We0) ->
    Pos = vmap_pos(V, Vmap),
    {We, New} = wings_edge:fast_cut(Edge, Pos, We0),
    make_face_vs_1(Ss, New, array:set(V, New, Vmap), [New|EL], We);
make_face_vs_1([], _, Vmap, EL, We) ->
    {EL, Vmap, We}.

inset_face(Loop0, TEs0, Vmap0,  #we{fs=Ftab0, es=Etab0, vc=Vct0, vp=Vpt0}=We0) ->
    Face = pick_ref_face(Loop0, undefined),
    NumberOfNew = length(Loop0),
    true = NumberOfNew > 2, %% Otherwise something is wrong

    {IdStart, We1} = wings_we:new_ids(NumberOfNew+2, We0),
    IdEnd = IdStart+NumberOfNew-1,
    Ids = lists:seq(IdStart, IdEnd),
    F1 = IdStart, %% F2 = IdStart+1,
    E1 = IdEnd+1, %% E2 = IdEnd+2,

    Pos0 = [vmap_pos(V,Vmap0) || #{v:=V} <- Loop0],
    FaceN = wings_face:normal(Face, We0),
    LoopN = e3d_vec:normal(Pos0),
    {VsId,Pos} = case e3d_vec:dot(FaceN,LoopN) > 0.0 of
                     true -> {[V || #{v:=V} <- Loop0], Pos0};
                     false -> {lists:reverse([V || #{v:=V} <- Loop0]),lists:reverse(Pos0)}
                 end,

    TestEdges = [{{lists:nth(V1, Pos),array:get(V2, Vpt0)},{V1+IdStart-1, V2}}
                 || V1 <- lists:seq(1, NumberOfNew),
                    V2 <- wings_face:vertices_ccw(Face, We0)],
    %% ?D("~p: ~p ~p~n",[We0#we.id, Face, TestEdges]),
    {{VI1, VO1},{VI2,VO2}} = pick_vs_pairs(TestEdges, FaceN),
    %% ?D("Connect ~w ~w~n", [{VO1, VI1},{VO2,VI2}]),
    E1R = #edge{vs=VO1, ve=VI1, lf=F1+1, rf=F1},
    E2R = #edge{vs=VO2, ve=VI2, lf=F1, rf=F1+1},
    Etab1 = array:set(E1, E1R, Etab0),
    Etab2 = array:set(E1+1, E2R, Etab1),
    %% Update old edges with new face
    Etab3 = update_inset_edges(VO1,VO2,Face,E1,F1,Etab2,We0),
    %% MkEdges
    MkEdge = fun(Id, Acc) -> make_inset_edges(Id, IdStart, IdEnd, VI1, VI2, Face, F1, Acc) end,
    Etab = lists:foldl(MkEdge, Etab3, Ids),
    %% Update the Ftab, Vct and VP
    Mat = wings_facemat:face(Face, We0),
    Ftab1 = gb_trees:insert(F1, E1, Ftab0),
    Ftab2 = gb_trees:insert(F1+1, E1+1, Ftab1),
    Ftab  = gb_trees:update(Face, IdStart, Ftab2),
    Vct = lists:foldl(fun(Id, Acc) -> array:set(Id,Id,Acc) end, Vct0, Ids),
    Vpt = lists:foldl(fun({Id, Point}, Vtab) -> array:set(Id, Point, Vtab) end,
                      Vpt0, lists:zip(Ids, Pos)),
    WeR = wings_facemat:assign(Mat, [F1, F1+1], We1#we{fs=Ftab,es=Etab,vc=Vct,vp=Vpt}),
    TEs = gb_sets:union(TEs0, gb_sets:from_ordset([E1,E1+1])),
    EL = wings_face:to_edges([Face], WeR),
    Upd = fun({V,New}, Acc) -> array:set(V,New,Acc) end,
    Vmap = lists:foldl(Upd, Vmap0, lists:zip(VsId, Ids)),
    {EL, TEs, Vmap, WeR}.

make_inset_edges(Id, First, Last, V1, V2, Face, SFace, Etab0) ->
    Prev = if Id =:= First -> Last;
              true -> Id-1
           end,
    Next = if Id =:= Last -> First;
              true -> Id+1
           end,
    OutF = case V1 < V2 of
               true ->
                   if Id < V1 -> SFace+1;
                      Id < V2 -> SFace;
                      true -> SFace+1
                   end;
               false ->
                   if Id < V2 -> SFace;
                      Id < V1 -> SFace+1;
                      true -> SFace
                   end
           end,
    Rtsu = if Next =:= V1 -> Last+1;
              Next =:= V2 -> Last+2;
              true -> Next
           end,
    Rtpr = if Id =:= V1 -> Last+1;
              Id =:= V2 -> Last+2;
              true -> Prev
           end,

    %% Make and insert face edge
    ERec = #edge{vs=Id,ve=Next,lf=Face,rf=OutF,
                 ltsu=Prev,ltpr=Next,
                 rtsu=Rtsu,rtpr=Rtpr},
    Etab1 = array:set(Id, ERec, Etab0),
    %% Update connected edge if this vertes should be connected to
    %% outer loop
    if Rtsu =:= Next ->
            Etab1;
       true ->
            E1 = array:get(Rtsu, Etab1),
            %% Assert
            if Next =:= V1 -> #edge{ve=V1, rf=SFace} = E1;
               Next =:= V2 -> #edge{ve=V2, lf=SFace} = E1
            end,
            %% ?D("~w: ~p~n", [Rtsu, E1#edge{ltpr=Id, rtsu=Next}]),
            array:set(Rtsu, E1#edge{ltpr=Id, rtsu=Next}, Etab1)
    end.

update_inset_edges(V1,V2,Face,E1,F1,Etab0,We) ->
    StartEdge = wings_vertex:until(
                  fun(Edge, _F, #edge{vs=VS, lf=F}, _)
                        when VS =:= V1, F =:= Face -> Edge;
                     (Edge, _F, #edge{ve=VE, rf=F}, _)
                        when VE =:= V1, F =:= Face -> Edge;
                     (_E,_F,_ER,A) -> A
                  end,
                  false, V1, We),
    %% ?D("Searching ~p in ~p => Startedge ~w ~n", [V1,Face,StartEdge]),
    All = fun(_V, E, ER, Acc) -> [{E, ER} | Acc] end,
    ERecs0 = wings_face:fold(All, [], Face, StartEdge, We),
    [First0|Recs] = lists:reverse(ERecs0),
    case First0 of
        {Id, #edge{ve=V1, rf=Face, rtsu=Next}=Rec} ->
            #edge{vs=V1, rf=F1} = C1 = array:get(E1, Etab0),
            Etab1 = array:set(E1, C1#edge{ltsu=Next,rtpr=Id}, Etab0),
            First = Rec#edge{rf=F1, rtsu=E1},
            Etab  = array:set(Id, First, Etab1),
            update_inset_es2(Recs++[{Id,First}],Id,[V1,V2,done],Face,E1,F1+1,Etab);
        {Id, #edge{vs=V1, lf=Face, ltsu=Next}=Rec} ->
            #edge{vs=V1, rf=F1} = C1 = array:get(E1, Etab0),
            Etab1 = array:set(E1, C1#edge{ltsu=Next,rtpr=Id}, Etab0),
            First = Rec#edge{lf=F1, ltsu=E1},
            Etab  = array:set(Id, First, Etab1),
            update_inset_es2(Recs++[{Id,First}],Id,[V1,V2,done],Face,E1,F1+1,Etab)
    end.

update_inset_es2([{Id, Rec0}|Recs], Prev, [V1|NextV], Face, E1, F1, Etab) ->
    %% ?D("Want V:~w in F:~w new(~w) ~n",[V1, Face, F1]),
    case Rec0 of
        #edge{lf=F, ve=V1, ltpr=Prev} when F =:= Face; F =:= F1 ->
            Rec = Rec0#edge{ltpr=E1},
            %% ?D("U: ~w: ~p~n",[Id, Rec]),
            update_inset_es1([{Id, Rec}|Recs], NextV, Face, E1+1, F1, Etab);
        #edge{rf=F, vs=V1, rtpr=Prev} when F =:= Face; F =:= F1 ->
            Rec = Rec0#edge{rtpr=E1},
            %% ?D("U: ~w: ~p~n",[Id, Rec]),
            update_inset_es1([{Id, Rec}|Recs], NextV, Face, E1+1, F1, Etab)
    end.

update_inset_es1([{Id, Rec0}|Recs], [V2|_]=VC, Face, E1, F1, Etab0) ->
    case Rec0 of
        #edge{rf=Face, ve=V2, rtsu=Next}=Rec ->
            #edge{vs=V2, rf=F1} = C1 = array:get(E1, Etab0),
            Etab1 = array:set(E1, C1#edge{ltsu=Next,rtpr=Id}, Etab0),
            Etab  = array:set(Id, Rec#edge{rf=F1, rtsu=E1}, Etab1),
            update_inset_es2(Recs,Id,VC,Face,E1,F1-1,Etab);
        #edge{vs=V2, lf=Face, ltsu=Next}=Rec ->
            #edge{vs=V2, rf=F1} = C1 = array:get(E1, Etab0),
            Etab1 = array:set(E1, C1#edge{ltsu=Next,rtpr=Id}, Etab0),
            Etab  = array:set(Id, Rec#edge{lf=F1, ltsu=E1}, Etab1),
            update_inset_es2(Recs,Id,VC,Face,E1,F1-1,Etab);
        #edge{rf=Face}=Rec ->
            Etab = array:set(Id, Rec#edge{rf=F1}, Etab0),
            update_inset_es1(Recs, VC, Face, E1, F1, Etab);
        #edge{lf=Face}=Rec ->
            Etab = array:set(Id, Rec#edge{lf=F1}, Etab0),
            update_inset_es1(Recs, VC, Face, E1, F1, Etab);
        #edge{} when Recs =:= [] ->
            %% First edge already updated now
            array:set(Id, Rec0, Etab0)
    end.

pick_vs_pairs(Edges, N) ->
    WithDist = [{e3d_vec:dist_sqr(P1,P2),Edge} ||
                   {{P1,P2},_}=Edge <- Edges],
    [{_,First}|Rest] = lists:sort(WithDist),
    pick_vs_pairs(Rest, First, N).

pick_vs_pairs([{_, {{P3,P4}, {A,B}=E1}}|Rest], {{P1,P2},{C,D}=E0}=Edge, N)
  when A =/= C, B=/= D ->
    case e3d_vec:line_line_intersect(P3,P4,P1,P2,N) of
        true -> pick_vs_pairs(Rest, Edge, N);
        false -> {E0,E1}
    end;
pick_vs_pairs([_|Rest], E0, Dir) ->
    pick_vs_pairs(Rest, E0, Dir).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter_tri_edges({L1,L2}, We1,We2) ->
    Loop = lists:zip(L1,L2),
    Res = filter_tri_edges_1(Loop,We1,We2),
    lists:unzip(Res).

filter_tri_edges_1([{#{v:=V}=V1,U1}, {#{v:=V}=V2, U2}|Vs],We1,We2) ->
    %% Remove edges to it self (loops)
    filter_tri_edges_1([{filter_edge(V1,V2),filter_edge(U1,U2)}|Vs],We1,We2);

filter_tri_edges_1([{#{op:=split_edge,e:=none},#{op:=split_edge, e:=none}}|Vs],We1,We2) ->
    filter_tri_edges_1(Vs,We1,We2);
filter_tri_edges_1([{#{op:=split_edge,e:=none,f:=F}=V1,#{op:=Op}=V2}|Vs],We1,We2) ->
    case Op of
        split_face -> filter_tri_edges_1(Vs,We1,We2);
        split_edge ->
            case skip_tess_edge(wings_face:normal(F,We1), V2, We2) of
                true -> filter_tri_edges_1(Vs,We1,We2);
                false -> [{edge_to_face(V1), V2}|filter_tri_edges_1(Vs,We1,We2)]
            end
    end;
filter_tri_edges_1([{#{op:=Op}=V1,#{op:=split_edge,e:=none,f:=F}=V2}|Vs],We1,We2) ->
    case Op of
        split_face -> filter_tri_edges_1(Vs,We1,We2);
        split_edge ->
            case skip_tess_edge(wings_face:normal(F,We2), V1, We1) of
                true -> filter_tri_edges_1(Vs,We1,We2);
                false -> [{V1,edge_to_face(V2)}|filter_tri_edges_1(Vs,We1,We2)]
            end
    end;
filter_tri_edges_1([V|Vs],We1,We2) ->
    [V|filter_tri_edges_1(Vs,We1,We2)];
filter_tri_edges_1([],_We1,_We2) -> [].

filter_edge(_, #{op:=split_edge, e:=Edge}=V2) when Edge =/= none -> V2;
filter_edge(V1,_) -> V1.

skip_tess_edge(_, #{e:=on_vertex}, _We) -> false;
skip_tess_edge(N, #{e:=Edge}=_EC, #we{es=Etab}=We) ->
    #edge{vs=VS,ve=VE} = array:get(Edge,Etab),
    Dir = e3d_vec:sub(wings_vertex:pos(VS, We),wings_vertex:pos(VE,We)),
    abs(e3d_vec:dot(N, Dir)) < 0.1.

edge_to_face(#{op:=split_edge}=Orig) ->
    Orig#{op=>split_face}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We need to build the cycle our selfves since the edges may not be directed
%% in the correct direction. Also the V is new Vs on edges and there maybe
%% several new V on the same wings edge.

build_vtx_loops(Edges0) ->
    G = digraph:new(),
    %% Vertices with "low" numbers are available in We1 and We2
    %% and new vertices are created afterwards
    %% Sort edges so that we get exiting vertices first and thus those edges
    %% will be picked first when there are several options below.
    %% This so we get a consistent ordering when we do multipass.
    Edges = lists:sort(fun(#{p1:={_,P1},p2:={_,P2}}, #{p1:={_,P3},p2:={_,P4}}) ->
                               max(P1,P2) =< max(P3,P4)
                       end, Edges0),
    Es = lists:foldl(fun(EI, Acc) -> add_edge2(EI,G, Acc) end, [], Edges),
    Cs = make_directed(gb_trees:from_orddict(lists:sort(Es)), G, []),
    [] = digraph:edges(G),
    digraph:delete(G),
    Cs.

add_edge2(#{p1:={_,V},p2:={_,V}}=_EI, _G, Acc) ->
    Acc;  %% Loop ignore
add_edge2(#{p1:={_,P1},p2:={_,P2}}=EI, G, Acc) ->
    digraph:add_vertex(G, P1),
    digraph:add_vertex(G, P2),
    case edge_exists(G, P1, P2) of
        true ->
            Acc;
        false ->
            E1 = digraph:add_edge(G, P1, P2, EI),
            E2 = digraph:add_edge(G, P2, P1, swap(EI)),
            [{E1,E2},{E2,E1}|Acc]
    end.

make_directed(All0, G, Cs) ->
    case gb_trees:is_empty(All0) of
        true -> Cs;
        false ->
            {E1,E2,All1} = gb_trees:take_smallest(All0),
            All2 = gb_trees:delete(E2,All1),
            {_, In, Out, Edge} = digraph:edge(G,E1),
            digraph:del_edge(G, E1),
            digraph:del_edge(G, E2),
            %% io:format("Start: ~p ~p => ~p (~p)~n",[E1,_In,Out,E2]),
            Es = digraph:out_edges(G,Out),
            {All, Comp} = make_directed(Es, Out, In, All2, G, [Edge]),
            make_directed(All, G, [Comp|Cs])
    end.

make_directed([E1|Es], Prev, Start, All0, G, Acc) when Prev =/= Start ->
    case gb_trees:lookup(E1, All0) of
        none ->
            make_directed(Es, Prev, Start, All0, G, Acc);
        {value, E2} ->
            All = gb_trees:delete(E1,gb_trees:delete(E2,All0)),
            case digraph:edge(G,E1) of
                {_, Prev, Next, Edge} ->
                    digraph:del_edge(G, E1),
                    digraph:del_edge(G, E2),
                    make_directed(digraph:out_edges(G,Next), Next, Start, All, G, [Edge|Acc]);
                {_, Next, Prev, Edge} ->
                    digraph:del_edge(G, E1),
                    digraph:del_edge(G, E2),
                    make_directed(digraph:out_edges(G,Next), Next, Start, All, G, [Edge|Acc])
            end
    end;
make_directed(_, Start, Start, All, _G, Es) ->
    [#{p2:={_, Last}}|_] = Es,
    [#{p1:={_, First}}|_] = _EL = lists:reverse(Es),
    if First =:= Last -> ok;
       true -> ?D("~n ~w~n ~w~n",[hd(Es),hd(_EL)]), error(no_loop)
    end,
    First = Last, %% Assert loop
    {All, Es}.

swap(#{mf1:=MF1,mf2:=MF2,p1:=P1,p2:=P2,other:=O}) ->
    #{mf1=>MF2,mf2=>MF1,p1=>P2,p2=>P1,other=>O}.

edge_exists(G,V1,V2) ->
    lists:member(V2, digraph:out_neighbours(G, V1)) orelse
        lists:member(V1, digraph:out_neighbours(G, V2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_loop(Loop, Vmap, We) ->
    split_loop(Loop, Vmap, We, []).

split_loop([E|Loop], Vmap, We, Acc) ->
    Vertex = vertex_info(E, Vmap, We),
    split_loop(Loop, Vmap, We, [Vertex|Acc]);
split_loop([], _Vmap, _We, Acc) ->
    lists:reverse(Acc).

vertex_info(#{mf1:={O1,F1}, mf2:={O2,F2}, other:={O3,F3}, p2:={{_, {A,B}=Edge0},V0}},
            Vmap, {#we{id=Id}=We,OWe}) ->
    if O2 =:= Id ->
            ON = other_normal(O1,Id,F3,F1,OWe),
            Edge = wings_vertex:edge_through(A,B,F2,We),
            Fs = edge_faces(Edge,F2,We),
            SF = #{op=>split_edge, o=>Id, f=>F2, e=>Edge, v=>V0, vs=>Edge0, fs=>Fs, o_n=>ON},
            on_vertex(SF, Vmap);
       O1 =:= Id ->
            ON = wings_face:normal(F2, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F1, v=>V0, o_n=>ON, other=>{O2,F2}}, Vmap, We);
       O3 =:= Id ->
            ON = wings_face:normal(F2, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F3, v=>V0, o_n=>ON, other=>{O2,F2}}, Vmap, We)
    end.

other_normal(Id,Id,F1,_F2,We) ->
    wings_face:normal(F1, We);
other_normal(_,_,_,F2,We) ->
    wings_face:normal(F2, We).

edge_faces(none,F1, _We) ->
    {F1,F1};
edge_faces(Edge,_F1, #we{es=Etab}) ->
    #edge{lf=LF,rf=RF} = array:get(Edge, Etab),
    {LF,RF}.

check_if_edge(#{f:=F, v:=V}=SF, Vmap, #we{id=Id, vp=Vtab, es=Etab}=We) ->
    {Where, Pos} = array:get(V, Vmap),
    Find = fun(_,Edge,#edge{vs=V1,ve=V2},Acc) ->
                   V1P = array:get(V1, Vtab),
                   V2P = array:get(V2, Vtab),
                   case e3d_vec:line_dist_sqr(Pos, V1P, V2P) < ?EPSILON of
                       true -> [{{V1,V2}, Edge}|Acc];
                       false -> Acc
                   end
           end,
    Es = wings_face:fold(Find, [], F, We),
    case {proplists:get_value(Id, Where), Es} of
        {undefined, []} -> SF;
        {undefined, [{Vs,Edge}]} ->
            #edge{lf=LF,rf=RF} = array:get(Edge, Etab),
            SF#{op:=split_edge, e=>Edge, vs=>Vs, fs=>{LF,RF}};
        {WeV, [{_,Edge}|_]} ->
            #edge{lf=LF,rf=RF} = array:get(Edge, Etab),
            SF#{op:=split_edge, e=>on_vertex, fs=>{LF,RF}, vs=>WeV}
    end.

on_vertex(#{o:=Id, v:=V}=SF, Vmap) ->
    {Where, _} = array:get(V, Vmap),
    case proplists:get_value(Id, Where) of
	undefined -> SF;
	WeV -> SF#{e=>on_vertex, vs=>WeV}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_isect(#we{id=Id, temp={?MODULE,Es}}=We0, Op) ->
    {Ts, Bvh} = make_bvh(We0),
    #{id=>Id,map=>Ts,bvh=>Bvh,sel_es=>Es,el=>[],we=>We0, op=>Op, temp_es=>gb_sets:empty()};
init_isect(We, Op) ->
    init_isect(We#we{temp={?MODULE, []}}, Op).

make_bvh(#we{fs=Fs0}=We) ->
    make_bvh(gb_trees:keys(Fs0), We).

make_bvh(Fs, #we{id=Id}=We) ->
    {Vtab,Ts} = triangles(Fs, We),
    Get = fun({verts, Face}) -> element(1, array:get(Face, Ts));
	     (verts) -> Vtab;
	     (meshId) -> Id
	  end,
    Bvh = e3d_bvh:init([{array:size(Ts), Get}]),
    {Ts, Bvh}.

make_vmap(ReEI0, #we{id=Id1, vp=Vtab1}, #we{id=Id2, vp=Vtab2}) ->
    {I0,L1} = lists:foldl(fun({N, Pos}, {I, Acc}) ->
                                  {I+1, [{{I, [{Id1,N}]}, Pos}|Acc]}
                          end, {0, []}, array:sparse_to_orddict(Vtab1)),
    Tree0 = e3d_kd3:from_list(L1),
    {I1,Tree1} = add_vtab(Vtab2, I0, Id2, Tree0),
    make_vmap(ReEI0, Tree1, I1, []).

add_vtab(Vtab, I0, Id, Tree) ->
    Add = fun(N, Pos, {I,Acc}) ->
                  {{IF,V1},P1} = Obj = e3d_kd3:nearest(Pos, Acc),
                  New = {Id,N},
                  case e3d_vec:dist_sqr(Pos, P1) < ?EPSILON of
                      true  -> {I, e3d_kd3:update(Obj, {IF,[New|V1]}, Acc)};
                      false -> {I+1, e3d_kd3:enter(Pos, {I, [New]}, Acc)}
                  end
          end,
    array:sparse_foldl(Add, {I0,Tree}, Vtab).

make_vmap([#{p1:=P10, p2:=P20}=E|R], T0, N0, Acc) ->
    {P1, N1, T1} = vmap(P10, N0, T0),
    {P2, N2, T2} = vmap(P20, N1, T1),
    make_vmap(R, T2, N2, [E#{p1:=P1,p2:=P2}|Acc]);
make_vmap([], T, _, Acc) ->
    OrdD = [{N,{Where,Pos}} || {{N, Where}, Pos} <- lists:sort(e3d_kd3:to_list(T))],
    {array:from_orddict(OrdD), Acc};
make_vmap([{coplanar, _,_}=_CP|_R], _T0, _N0, _Acc) ->
    wings_u:error_msg(?__(1, "Coplanar faces can not be handled currently")).


vmap({Where, Pos}, N, Tree) ->
    {{I, _V1}, P1} = e3d_kd3:nearest(Pos, Tree),
    case e3d_vec:dist_sqr(Pos, P1) < ?EPSILON of
        true  -> {{Where, I}, N, Tree};
        false -> {{Where, N}, N+1, e3d_kd3:enter(Pos, {N, []}, Tree)}
    end.

vmap_pos(N, Vmap) ->
    {_Where, Pos} = array:get(N, Vmap),
    Pos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remap(#{mf1:=MF10,mf2:=MF20,p1:={Pos1,E11,E12}, p2:= {Pos2, E21, E22}, other:=Other},
      #{we:=#we{id=Id1},map:=M1}, #{we:=#we{id=Id2},map:=M2}) ->
    MF1 = remap_1(MF10, Id1, M1, Id2, M2),
    MF2 = remap_1(MF20, Id1, M1, Id2, M2),
    Oth = remap_1(Other, Id1, M1, Id2, M2),
    EId1 = {element(1,MF1), order(E11,E12)},
    EId2 = {element(1,MF2), order(E21,E22)},
    case Id1 < Id2 of
        true  -> #{mf1=>MF1, mf2=>MF2, p1=>{EId1, Pos1}, p2=>{EId2, Pos2}, other=>Oth};
        false -> #{mf1=>MF2, mf2=>MF1, p1=>{EId2, Pos2}, p2=>{EId1, Pos1}, other=>Oth}
    end;
remap({coplanar, MF10, MF20}, #{we:=#we{id=Id1},map:=M1}, #{we:=#we{id=Id2},map:=M2}) ->
    MF1 = remap_1(MF10, Id1, M1, Id2, M2),
    MF2 = remap_1(MF20, Id1, M1, Id2, M2),
    {coplanar, MF1, MF2}.

remap_1({Id, TriFace}, Id, M1, _Id2, _M2) ->
    {_, Face} = array:get(TriFace, M1),
    {Id, Face};
remap_1({Id, TriFace}, _Id, _M1, Id, M2) ->
    {_, Face} = array:get(TriFace, M2),
    {Id, Face}.

order(V1, V2) when V1 < V2 ->  {V1,V2};
order(V2, V1) -> {V1,V2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

triangles(Fs, #we{vp=Vtab}=We) ->
    Ts = lists:foldl(fun(Face, Acc) -> triangle(Face,We,Acc) end, [], Fs),
    {Vtab, array:from_list(Ts)}.

triangle(Face, We, Acc) ->
    Vs = wings_face:vertices_ccw(Face, We),
    case length(Vs) of
	3 -> [{list_to_tuple(Vs), Face}|Acc];
	4 -> tri_quad(Vs, We, Face, Acc);
	_ -> tri_poly(Vs, We, Face, Acc)
    end.

tri_quad([Ai,Bi,Ci,Di] = Vs, #we{vp=Vtab}, Face, Acc) ->
    [A,B,C,D] = VsPos = [array:get(V, Vtab) || V <- Vs],
    N = e3d_vec:normal(VsPos),
    case wings_tesselation:is_good_triangulation(N, A, B, C, D) of
	true  -> [{{Ai,Bi,Ci}, Face}, {{Ai,Ci,Di}, Face}|Acc];
	false -> [{{Ai,Bi,Di}, Face}, {{Bi,Ci,Di}, Face}|Acc]
    end.

tri_poly(Vs, #we{vp=Vtab}, Face, Acc0) ->
    VsPos = [array:get(V, Vtab) || V <- Vs],
    FaceVs = lists:seq(0, length(Vs)-1),
    Tris = e3d_mesh:triangulate_face(#e3d_face{vs=FaceVs}, VsPos),
    VsT = list_to_tuple(Vs),
    Tri = fun(#e3d_face{vs=[A,B,C]}, Acc) ->
                  [{{element(A+1,VsT), element(B+1,VsT), element(C+1,VsT)}, Face}|Acc]
          end,
    lists:foldl(Tri, Acc0, Tris).

tesselate_faces(Fs, We0) ->
    Pick = fun(Face, {Tris, Other}) ->
                   case wings_face:vertices(Face,We0) of
                       3 -> {[Face|Tris],Other};
                       _ -> {Tris, [Face|Other]}
                   end
           end,
    {Tris,Ngons} = lists:foldl(Pick, {[],[]}, Fs),
    {We1,_} = wings_face_cmd:subdiv(Tris, We0),
    TessN = fun(Face, We00) ->
                    %% Ugly workaround for inset vertex in face
                    We01 = wings_extrude_face:faces([Face], We00),
                    {We02,_} = wings_collapse:collapse_faces(gb_sets:singleton(Face),We01),
                    We02
            end,
    lists:foldl(TessN, We1, Ngons).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS:

tree_to_set(GbTree) ->
    gb_sets:from_ordset(gb_trees:keys(GbTree)).
