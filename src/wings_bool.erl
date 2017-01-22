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
-define(DBG_TRY(Do,Err),
        try Do
        catch error:__R ->
                ?dbg("ERROR: ~p:~n ~P~n", [__R, erlang:get_stacktrace(), 20]),
                Err;
              exit:_ ->
                Err
        end).
-define(PUT(Id,We),put(Id,We)).
-define(TEST, true).
-define(D(F,A), ?dbg(F,A)).
-else.
-define(DBG_TRY(Do,Err), Do).
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
	EdgeInfo -> {merge_0(EdgeInfo, Head, H1), Rest ++ Tested}
    end;
find_intersect_1(_Head, [], _) ->
    none.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_0(EdgeInfo0, I1, I2) ->
    EdgeInfo = [remap(Edge, I1, I2) || Edge <- EdgeInfo0],
    ?PUT(we1,maps:get(we,I1)), ?PUT(we2,maps:get(we,I2)),
    case [{MF1,MF2} || {coplanar, MF1, MF2} <- EdgeInfo] of
        [] -> ?DBG_TRY(merge_1(EdgeInfo, I1, I2), #{we=>get(we1),delete=>none, sel_es=>[], error=>get(we2)});
        Coplanar -> tesselate_and_restart(Coplanar, I1, I2)
    end.

merge_1(EdgeInfo0, #{we:=We10,el:=EL10,op:=Op1}, #{we:=We20,el:=EL20,op:=Op2}) ->
    ?D("~p~n",[?FUNCTION_NAME]),
    {Vmap, EdgeInfo} = make_vmap(EdgeInfo0, We10, We20),  %% Make vertex id => pos and update edges
    %?D("Vmap: ~p~n",[array:to_orddict(Vmap)]),
    Loops0 = build_vtx_loops(EdgeInfo, []), %% Figure out edge loops
    L10 = [split_loop(Loop, Vmap, {We10,We20}) || Loop <- Loops0], % Split loops per We and precalc
    L20 = [split_loop(Loop, Vmap, {We20,We10}) || Loop <- Loops0], % some data
    %% Remove vertexes on triangulated edges
    Loops1 = [filter_tri_edges(Loop,We10,We20) || Loop <- lists:zip(L10,L20)],
    Loops = sort_largest(Loops1),
    %% Create vertices on the edge-loops
    #{el1:=EL11, el2:=EL21} = R0 = make_verts(Loops, Vmap, We10, We20),
    merge_2(R0#{el1:=EL11++EL10, el2:=EL21++EL20, op1=>Op1, op2=>Op2},We10,We20).

%% Continuing: multiple edge loops have hit the same face. It was
%% really hard to handle that in one pass, since faces are split and
%% moved.  Solved it by doing the intersection test again for the new
%% faces and start over
merge_2(#{res:=cont,we1:=We11, el1:=EL1, fs1:=Fs1, op1:=Op1, op2:=Op2,
          we2:=We21, el2:=EL2, fs2:=Fs2},We10,We20) ->
    ?D("~p cont~n",[?FUNCTION_NAME]),
    {We1, Vmap1, B1} = remake_bvh(Fs1, We10, We11),
    {We2, Vmap2, B2} = remake_bvh(Fs2, We20, We21),
    EI0 = e3d_bvh:intersect(B1, B2),
    I11 = #{we=>We1,map=>Vmap1,el=>EL1, op=>Op1},
    I21 = #{we=>We2,map=>Vmap2,el=>EL2, op=>Op2},
    EI = [remap(Edge, I11, I21) || Edge <- EI0],
    %% We should crash if we have coplanar faces in this step
    ?DBG_TRY(merge_1(EI,I11,I21), #{we=>We1,delete=>none, el=>[], sel_es=>[], error=>We2});
%% All edge loops are in place, dissolve faces inside edge loops and
%% merge the two we's
merge_2(#{res:=done, we1:=We1, el1:=EL1, we2:=We2, el2:=EL2, op1:=Op1, op2:=Op2},
        #we{id=Id1}, #we{id=Id2}) ->
    ?D("~p ~p ~p done~n",[?FUNCTION_NAME, We1#we.id, We2#we.id]),
    %% ?D("Dissolve: ~p: ~w~n",[Id1,gb_sets:to_list(faces_in_region(EL1, We1))]),
    %% ?D("~w ~n",[EL2]),
    %% ?D("Dissolve: ~p: ~w~n",[Id2,gb_sets:to_list(faces_in_region(EL2, We2))]),
    DRes1 = dissolve_faces_in_edgeloops(EL1, Op1, We1),
    DRes2 = dissolve_faces_in_edgeloops(EL2, Op2, We2),
    Weld = fun() ->
                   {We,Es} = weld([DRes1, DRes2]),
                   [Del] = lists:delete(We#we.id, [Id1,Id2]),
                   ok = wings_we_util:validate(We),
                   #{sel_es=>Es, we=>We, delete=>Del}
           end,
    ?DBG_TRY(Weld(), #{we=>element(2, DRes1),delete=>none, sel_es=>[], error=>element(2, DRes2)}).
    %?DBG_TRY(Weld(), #{we=>We1,delete=>none, sel_es=>[], error=>We2}).

sort_largest(Loops) ->
    OnV = fun(#{e:=on_vertex}) -> true; (_) -> false end,
    Filter = fun({L1,L2}) -> lists:all(OnV,L1) andalso lists:all(OnV,L2) end,
    Ls0 = [{length(L1), Loop} || {L1,_} = Loop <- Loops],
    [L || {_, L} <- lists:sort(Ls0), not Filter(L)].

remake_bvh(Fs0, We0, We1) ->
    Fs1 = gb_sets:union(Fs0,wings_we:new_items_as_gbset(face,We0,We1)),
%    ?D("Tess ~w ~n", [gb_sets:to_list(Fs1)]),
    We = wings_tesselation:quadrangulate(Fs1, We1),
    Fs = gb_sets:union(Fs1,wings_we:new_items_as_gbset(face,We1,We)),
    {Vmap, Bvh} = make_bvh(gb_sets:to_list(Fs), We),
    {We, Vmap, Bvh}.

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
    I11 = #{we=>We10,map=>Vmap1,el=>[],op=>Op1},
    I21 = #{we=>We20,map=>Vmap2,el=>[],op=>Op2},
    EI = [remap(Edge, I11, I21) || Edge <- EI0],
    merge_1(EI,I11,I21). %% We should crash if we have coplanar faces in this step

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dissolve_faces_in_edgeloops(ELs, Op, #we{fs=Ftab} = We0) ->
    Fs0 = faces_in_region(ELs, We0),
    We = case Op of
             add -> wings_dissolve:faces(Fs0, We0);
             isect ->
                 Fs = gb_sets:difference(gb_sets:from_ordset(gb_trees:keys(Ftab)), Fs0),
                 wings_dissolve:faces(Fs, We0);
             sub ->
                 Fs = gb_sets:difference(gb_sets:from_ordset(gb_trees:keys(Ftab)), Fs0),
                 We1 = wings_dissolve:faces(Fs, We0),
                 wings_we:invert_normals(We1)
         end,
    Faces = wings_we:new_items_as_ordset(face, We0, We),
    {order_loops(Faces, ELs, We),We}.

order_loops([_]=Face, _ELs, _We) ->
    Face;
order_loops(Fs, ELs0, We) ->
    {OrderEs,_} = lists:mapfoldl(fun(Edge, N) -> {{Edge,N},N+1} end,
                                 0, [hd(lists:sort(EL)) || {EL,_} <- ELs0]),
    CFs0 = [{hd(lists:sort(wings_face:to_edges([Face],We))), Face} || Face <- Fs],
    CFs = [{proplists:get_value(Edge, OrderEs), Face} || {Edge, Face} <- CFs0],
    [Face || {_, Face} <- lists:sort(CFs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wings_edge:select_region() does not work as I want with several loops
%% we know the faces though.
faces_in_region(ELs, #we{fs=All}=We) ->
    Es  = gb_sets:from_list([E || {Es,_} <- ELs, E <- Es]),
    Fs0 = gb_sets:from_list([F || {_,Fs} <- ELs, F <- Fs]),
    Fs  = gb_sets:intersection(gb_sets:from_ordset(gb_trees:keys(All)), Fs0),
    %% ?D("~p~n",[gb_sets:to_list(Fs)]),
    case gb_sets:is_empty(Fs) of
        true -> wings_edge:select_region(Es, We);
        false -> wings_edge:reachable_faces(Fs, Es, We)
    end.

%% Weld
%% Merge the two We's and bridge corresponding face-pairs
weld(FsWes) ->
    WeRs = [{We,[{face, Fs, unused},{edge, Es, unused}]} || {Fs,#we{temp=Es}=We} <- FsWes],
    {We0,Rs} = wings_we:merge_root_set(WeRs),
    [Fs1,Fs2] = [Fs || {face,Fs,_} <- Rs],
    FacePairs = lists:zip(Fs1,Fs2),
    SelEs = lists:append([Es || {edge,Es,_} <- Rs]),
    %?D("After ~p: ~w~n",[We0#we.id,gb_trees:keys(We0#we.fs)]),
    Weld = fun({F1,F2}, WeAcc) -> do_weld(F1,F2,WeAcc) end,
    {#we{es=Etab} = We1, Es} = lists:foldl(Weld, {We0,[]}, FacePairs),
    Borders = ordsets:intersection(ordsets:from_list(Es++SelEs),
                                   wings_util:array_keys(Etab)),
    BorderFs = gb_sets:to_list(wings_face:from_edges(Borders, We1)),
    Fs = [Face || Face <- BorderFs, wings_face:vertices(Face, We1) > 5],
    We = wings_tesselation:quadrangulate(Fs, We1),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_verts(Loops, Vmap, We10, We20) ->
    Empty = gb_sets:empty(),
    make_verts(Loops, Vmap, Empty, We10, Vmap, Empty, We20, [], []).

make_verts([{L1,L2}=L12|Ls], Vm10, Fs10, We10, Vm20, Fs20, We20, Acc, Cont) ->
    case check_if_used(L1,Fs10) orelse check_if_used(L2,Fs20) of
	true ->
	    make_verts(Ls, Vm10, Fs10, We10, Vm20, Fs20, We20, Acc, [L12|Cont]);
	false ->
	    {Es1, Fs11, Vm1, We1} = make_verts_per_we(L1, Vm10, We10),
            ?PUT(we1,We1),
	    {Es2, Fs21, Vm2, We2} = make_verts_per_we(L2, Vm20, We20),
            ?PUT(we2,We2),
	    Fs12 = gb_sets:union(gb_sets:from_list(Fs11), Fs10),
            Fs1 = gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L1]), Fs12),
	    Fs22 = gb_sets:union(gb_sets:from_list(Fs21), Fs20),
            Fs2 = gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L2]), Fs22),
            make_verts(Ls, Vm1, Fs1, We1, Vm2, Fs2, We2,[{{Es1,Fs11},{Es2,Fs21}}|Acc], Cont)
    end;
make_verts([], _, Fs10, We1, _, Fs20, We2, Acc, Cont) ->
    {Es1, Es2} = lists:unzip(Acc),
    case Cont of
	[] ->
	    #{res=>done, we1=>We1, el1=>Es1, we2=>We2, el2=>Es2};
	_ ->
	    Add = fun({L1,L2}, {F1,F2}) ->
			  {gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L1]),F1),
			   gb_sets:union(gb_sets:from_list([F || #{f:=F} <- L2]),F2)}
		  end,
	    {Fs11,Fs21} = lists:foldl(Add, {Fs10,Fs20}, Cont),
            Fs1 = gb_sets:intersection(Fs11, gb_sets:from_ordset(gb_trees:keys(We1#we.fs))),
            Fs2 = gb_sets:intersection(Fs21, gb_sets:from_ordset(gb_trees:keys(We2#we.fs))),
            #{res=>cont,we1=>We1, el1=>Es1, fs1=>Fs1, we2=>We2, el2=>Es2, fs2=>Fs2}
    end.

check_if_used(Loop, Fs) ->
    case gb_sets:is_empty(Fs) of
	true -> false;
	false ->
	    Next = [F || #{f:=F} <- Loop],
	    Int = gb_sets:intersection(gb_sets:from_list(Next), Fs),
	    not gb_sets:is_empty(Int)
    end.

make_verts_per_we(Loop, Vmap0, We0) ->
    % ?D("We ~w Make verts:~n",[We0#we.id]),[io:format(" ~w~n", [E]) || E <- Loop],
    {Vmap, We1} = cut_edges(Loop, Vmap0, We0),
    make_edge_loop(Loop, Vmap, [], [], We1).

cut_edges(SE, Vmap, We0) ->
    WiEs = [{E,Vn} || #{op:=split_edge, e:=E, v:=Vn} <- SE],
    ECuts = sofs:to_external(sofs:relation_to_family(sofs:relation(WiEs, [{edge,vn}]))),
    lists:foldl(fun cut_edge/2, {Vmap, We0}, ECuts).

cut_edge({on_vertex, Vs}, {Vmap, #we{id=Id}=We}) ->
    {lists:foldl(fun(V, VM) ->
                         {Where, _Pos} = array:get(V, Vmap),
                         Vi = proplists:get_value(Id, Where),
                         array:set(V,Vi,VM)
                 end,
                 Vmap,Vs),
     We};
cut_edge({Edge, [V]}, {Vmap, #we{id=Id}=We0}) ->
    {Where,Pos} = array:get(V, Vmap),
    case proplists:get_value(Id, Where) of
        undefined ->
            {We, NewV} = wings_edge:fast_cut(Edge, Pos, We0),
            {array:set(V, NewV, Vmap), We};
        Vi ->
            {array:set(V, Vi, Vmap), We0}
    end;
cut_edge({Edge, Vs}, {Vmap0, #we{es=Etab}=We0}) ->
    #edge{vs=VS} = array:get(Edge, Etab),
    P1 = wings_vertex:pos(VS,We0),
    C = fun(V) ->
                Pos = vmap_pos(V, Vmap0),
                Dist2 = e3d_vec:dist_sqr(Pos, P1),
                {Dist2, V, Pos}
        end,
    VsPos = lists:sort([C(V) || V <- Vs]),
    {We,_,Vmap} = lists:foldl(fun({_,V,Pos}, {WE, E, Vm}) ->
                                      {We, New} = wings_edge:fast_cut(E, Pos, WE),
                                      {We, New, array:set(V, New, Vm)}
                              end, {We0, Edge, Vmap0}, VsPos),
    {Vmap, We}.

make_edge_loop([#{op:=split_edge}=F|_]=Loop, Vmap, EL, IFs, We) ->
    make_edge_loop_1(Loop, F, Vmap, EL, IFs, We);
make_edge_loop(Loop, Vmap, EL, IFs, We) ->
    %% Start with split_edge
    case lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Loop) of
        {FSs, []} -> %% No edges intersect, make a face inside the intersecting face
            inset_face(FSs, Vmap, EL, We);
        {FSs, Edges} -> %% Connect edges and create new verts
            make_edge_loop(Edges++FSs, Vmap, EL, IFs, We)
    end.

make_edge_loop_1([V1], V1, Vmap, EL, IFs, We) ->
    {EL, IFs, Vmap, We};
make_edge_loop_1([#{op:=split_edge}=V1],#{op:=split_edge}=V2, Vmap, EL, IFs, We0) ->
    {{We, New}, Face} = connect_verts(V1,V2,Vmap, We0),
    {[New|EL], inside(Face,IFs), Vmap, We};
make_edge_loop_1([#{op:=split_edge}=V1|[#{op:=split_edge}=V2|_]=Rest], Last, Vmap, EL, IFs, We0) ->
    {{We, New}, Face} = connect_verts(V1,V2,Vmap, We0),
    make_edge_loop_1(Rest, Last, Vmap, [New|EL], inside(Face,IFs), We);
make_edge_loop_1([#{op:=split_edge}=V1|Splits], Last, Vmap, EL0, IFs, We0) ->
    {FSs, [V2|_]=Rest} =
        case lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Splits) of
            {FS, []} -> {FS, [Last]};
            {FS, Rs} -> {FS, Rs}
        end,
    case edge_exists(V1,V2,Vmap,We0) of
        [] -> %% Standard case
            %% ?D("Connect: ~w[~w] ~w[~w]~n",
            %%      [maps:get(v,V1), array:get(maps:get(v,V1),Vmap),
            %%       maps:get(v,V2), array:get(maps:get(v,V2), Vmap)]),
            {{We1, Edge}, Face} = connect_verts(V1,V2,FSs,Vmap,We0),
            ok = wings_we_util:validate(We1),
%            ?D("~w: new edge ~w face ~w~n",[We0#we.id, Edge, Face]),
            {EL1,Vmap1,We} = make_face_vs(FSs, V1, Edge, Vmap, We1),
            %% ?D("New: ~w~n",[EL1]),
            %% ?D("Old: ~w~n",[EL0]),
            make_edge_loop_1(Rest, Last, Vmap1, EL1++EL0, inside(Face,IFs), We);
        [{Edge,_F1,_F2}] ->
            %% ?D("Recreate: ~w: ~w ~w edge ~p in Fs ~w ~w~n",
            %%      [We0#we.id, array:get(maps:get(v,V1),Vmap),array:get(maps:get(v,V2), Vmap),Edge,_F1,_F2]),
            {EL1,Face,Vmap1,We} = half_inset_face(V1,V2,FSs,Edge,Vmap,We0),
%            ?D("~w: new face ~w~n",[We0#we.id, Face]),
            make_edge_loop_1(Rest, Last, Vmap1, EL1++EL0, inside(Face,IFs), We)
    end.

inside(none, Fs) -> Fs;
inside({delete, Face}, Fs) -> [F || F <- Fs, F =/= Face];
inside(Face, Fs) when is_integer(Face) -> [Face|Fs].

edge_exists(#{v:=V10},#{v:=V20},Vmap,We) ->
    wings_vertex:edge_through(array:get(V10,Vmap),array:get(V20,Vmap),We).

connect_verts(V1, V2, Vmap, We) ->
    {WeV1,WeV2,Face,OtherN} = pick_face(V1,V2, [], Vmap, We),
    connect_verts_1(WeV1, WeV2, Face, OtherN, We).
connect_verts(V1, V2, Refs, Vmap, We) ->
    {WeV1,WeV2,Face,OtherN} = pick_face(V1,V2, Refs, Vmap, We),
    connect_verts_1(WeV1, WeV2, Face, OtherN, We).

connect_verts_1(WeV1, WeV2, Face, CrossDir, #we{vp=Vtab}=We) ->
    case wings_vertex:edge_through(WeV1,WeV2,Face,We) of
        none ->
%            ?D("~w: ~w ~w in ~w ~s~n",[We#we.id, WeV1, WeV2, Face, e3d_vec:format(CrossDir)]),
            N = wings_face:normal(Face, We),
            Dir = e3d_vec:cross(N,e3d_vec:norm_sub(array:get(WeV1,Vtab),array:get(WeV2,Vtab))),
%            ?D("Swap: ~.3f~n", [e3d_vec:dot(CrossDir, Dir)]),
            case 0 >= e3d_vec:dot(CrossDir, Dir) of
                true  -> {wings_vertex:force_connect(WeV1,WeV2,Face,We), Face};
                false -> {wings_vertex:force_connect(WeV2,WeV1,Face,We), Face}
            end;
        Edge ->
%            ?D("Skip ~p ~p~n",[Edge,Face]),
            {{We, Edge}, none}
    end.

pick_face(#{v:=V1,o_n:=N1}, #{v:=V2,o_n:=N2}, [], Vmap, We) ->
    WeV1 = array:get(V1, Vmap),
    WeV2 = array:get(V2, Vmap),
    true = is_integer(WeV1), true = is_integer(WeV2), %% Assert
    OtherN = e3d_vec:norm(e3d_vec:average(N1,N2)),
    case [Face || {Face, [_,_]} <- wings_vertex:per_face([WeV1,WeV2],We)] of
        [Face] ->
            {WeV1,WeV2,Face,OtherN};
        [Face|_] = _Fs ->
            {WeV1,WeV2,Face,OtherN}
    end;
pick_face(#{v:=V1,fs:=_Fs}=R0, #{v:=V2}=R1, Refs, Vmap, #we{es=_Etab, vp=_Vtab}=We) ->
    N = e3d_vec:norm(e3d_vec:average([N || #{o_n:=N} <- [R0,R1|Refs]])),
    WeV1 = array:get(V1, Vmap),
    WeV2 = array:get(V2, Vmap),
    All = wings_vertex:per_face([WeV1,WeV2],We),
    % ?D("~p in ~w => ~w ~n",[pick_ref_face(Refs, undefined),_Fs, [Face || {Face, [_,_]} <- All]]),
    case [Face || {Face, [_,_]} <- All] of
        [Face] -> {WeV1,WeV2,Face,N}
    end.

pick_face_2(Wanted, Fs, Edge, #we{id=_Id,es=Etab}) ->
    % ?D("id:~p Wanted ~w ~w Fs: ~p~n", [_Id, Wanted, Fs, array:get(Edge, Etab)]),
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

half_inset_face(#{v:=EV1,o_n:=ON, fs:=CFs}=R0, #{v:=EV2}=R1, [#{v:=NV}=R2|FSs], RefEdge, Vmap0, We0) ->
    E1 = array:get(EV1, Vmap0),
    E2 = array:get(EV2, Vmap0),
    Wanted = pick_ref_face([R2|FSs], undefined),
    Face = pick_face_2(Wanted, CFs, RefEdge, We0),
    % ?D("Half inset: ~p(~p) ~p(~p) in ~p~n",[EV1,E1,EV2,E2,Face]),
    FVs0 = wings_face:vertices_ccw(Face, We0),
    FVs = order_vertex_list(E1, E2, FVs0),
    [E1,_Skip,E3|_FVs] = FVs,
    {{We1,Edge0},_} = connect_verts_1(E1,E3,Face,ON,We0),
    Pos = vmap_pos(NV,Vmap0),
    {We2,Edge1} = wings_edge:fast_cut(Edge0,Pos,We1),
    V0 = Edge1,
    {Keep,Remove} = case is_first(E1,Edge0,We2) of
                        true -> {Edge0, Edge1};
                        false -> {Edge1, Edge0}
                    end,
    Dir2 = e3d_vec:norm(e3d_vec:average([N || #{o_n:=N} <- [R0,R1,R2|FSs]])),
    %% Make verts on the new edge
    try connect_verts_1(V0,E2,Face,Dir2,We2) of
        {{We3,Edge2},_} ->
            {EL, Vmap, We} = make_face_vs_0(FSs, V0, Edge2, array:set(NV,V0,Vmap0), We3),
            %% Dissolve edge here??
            {[Keep|EL], Face, Vmap, wings_edge:dissolve_edge(Remove,We)}
    catch _:{badmatch,false} ->
            #we{es=Etab}=We2,
            Other = wings_face:other(Face, array:get(Edge1, Etab)),
            {{We3,Edge2},_} = connect_verts_1(V0,E2,Other,Dir2,We2),
            {EL, Vmap, We} = make_face_vs_0(FSs, V0, Edge2, array:set(NV,V0,Vmap0), We3),
            %% Dissolve edge here??
            {[Keep|EL], {delete,Face}, Vmap, wings_edge:dissolve_edge(Remove,We)}
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

inset_face(Fs, Vmap, EL, We0) ->
    Face = pick_ref_face(Fs, undefined),
    NumberOfNew = length(Fs),
    true = NumberOfNew > 2, %% Otherwise something is wrong
    We1 = wings_extrude_face:faces([Face], We0),
    FVs = wings_face:vertices_ccw(Face, We1),
    FPos = wings_face:vertex_positions(Face, We1),
    NumberOfOld = length(FVs),
    if
	NumberOfOld =:= NumberOfNew ->
            inset_face_equal(Face, FVs, FPos, Fs, Vmap, EL,We1);
	NumberOfOld > NumberOfNew ->
            inset_face_less(Face, FVs, FPos, Fs, Vmap, EL,We1);
	true ->
            inset_face_more(Face, FVs, FPos, Fs, Vmap, EL,We1)
    end.

inset_face_equal(Face, FVs, [P1,P2|_] = FPos, [#{v:=V1},#{v:=V2}|_]=Fs, Vmap, EL, We) ->
    KD3 = e3d_kd3:from_list(lists:zip(FVs, FPos)),
    P3 = vmap_pos(V1, Vmap),
    P4 = vmap_pos(V2, Vmap),
    Center = e3d_vec:average(FPos),
    D1 = e3d_vec:normal(P1,P2,Center),
    D2 = e3d_vec:normal(P3,P4,Center),
    Ordered = case e3d_vec:dot(D1,D2) > 0 of
                  true -> FVs;
                  false -> lists:reverse(FVs)
              end,
    {{First,_}, _} = e3d_kd3:take_nearest(P3, KD3),
    {VL1,VL2} = lists:splitwith(fun(V) when V =:= First -> false; (_) -> true end,
                                Ordered),
    PosL = [vmap_pos(Vi, Vmap)|| #{v:=Vi} <- Fs],
    Vs = lists:zip(VL2++VL1, PosL),
    Vtab = lists:foldl(fun({V,Pos}, Vtab) -> array:set(V, Pos, Vtab) end,
                       We#we.vp, Vs),
    cleanup_edges(FVs, [V||{V,_}<-Vs], Face, EL, Vmap, We#we{vp=Vtab}).

inset_face_less(Face, FVs, FPos, Fs, Vmap, EL, We) ->
    KD3 = e3d_kd3:from_list(lists:zip(FVs, FPos)),
    {Vs,_} = lists:mapfoldl(fun(#{v:=Vi}, Tree0) ->
                                    Pos = vmap_pos(Vi, Vmap),
                                    {{V,_}, Tree} = e3d_kd3:take_nearest(Pos, Tree0),
                                    {{V,Pos},Tree}
                            end, KD3, Fs),
    Vtab = lists:foldl(fun({V,Pos}, Vtab) -> array:set(V, Pos, Vtab) end,
                       We#we.vp, Vs),
    cleanup_edges(FVs, [V||{V,_}<-Vs], Face, EL, Vmap, We#we{vp=Vtab}).

inset_face_more(Face, FVs, FPos, Fs, Vmap, EL, We) ->
    KD3 = e3d_kd3:from_list([{FS, vmap_pos(Vi, Vmap)} || #{v:=Vi}=FS <- Fs]),
    {Vs,_} = lists:mapfoldl(fun({V, Old}, Tree0) ->
                                    {{FS,Pos}, Tree} = e3d_kd3:take_nearest(Old, Tree0),
                                    {{V,Pos,FS},Tree}
                            end, KD3, lists:zip(FVs, FPos)),
    Vtab = lists:foldl(fun({V, Pos, _}, Vtab) -> array:set(V, Pos, Vtab) end,
                       We#we.vp, Vs),
    Vmap1 = lists:foldl(fun({V, _, #{v:=Vi}}, Map) -> array:set(Vi, V, Map) end,
                        Vmap, Vs),
    Fs1 = lists:map(fun(FS) -> case lists:keyfind(FS, 3, Vs) of
                                   false -> FS;
                                   {_,_,_} -> FS#{op:=split_edge}
                               end
                    end, Fs),
    {Fs2,Fs3} = lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Fs1),
    inset_face_more(Fs3++Fs2++[hd(Fs3)], EL, Vmap1, We#we{vp=Vtab}, Face).

inset_face_more([_], EL, Vmap, We, Face) -> {EL, [Face], Vmap, We};
inset_face_more([SE|Rest], EL0, Vmap0, We0, Face) ->
    case lists:splitwith(fun(#{op:=Op}) -> Op =:= split_face end, Rest) of
        {[], Fs1} ->
            inset_face_more(Fs1, EL0, Vmap0, We0, Face);
        {Fs0,[Next|_]=Fs1} ->
            [{Edge,_,_}] = edge_exists(SE,Next,Vmap0,We0),
            {EL, Vmap, We} = make_face_vs(Fs0, SE, Edge, Vmap0, We0),
            inset_face_more(Fs1, EL++EL0, Vmap, We,Face)
    end.

cleanup_edges(FVs, Used, Face, EL0, Vmap, We) ->
    %% Start with a used vertex
    {Vs1,Vs0} = lists:splitwith(fun(V) -> not lists:member(V, Used) end, FVs),
    {EL,Fs,WeR} = cleanup_edges(Vs0++Vs1, false, hd(Vs0), [], Used, Face, EL0, We),
    {EL,Fs,Vmap,WeR}.

cleanup_edges([V1|[V2|Vs]=Vs0], Connect, Last, Drop, Used, Face, EL, We0) ->
    case lists:member(V2, Used) of
        true when Connect ->
            {We, New} = wings_vertex:force_connect(V2,V1,Face,We0),
            cleanup_edges(Vs0, false, Last, Drop, Used, Face, [New|EL], We);
        true ->
            Edge = wings_vertex:edge_through(V1,V2,Face,We0),
            cleanup_edges(Vs0, false, Last, Drop, Used, Face, [Edge|EL], We0);
        false ->
            cleanup_edges([V1|Vs], true, Last, [V2|Drop], Used, Face, EL, We0)
    end;
cleanup_edges([V1], Connect, Last, Drop, _Used, Face, EL0, We0) ->
    {EL,We2} = case Connect of
                   true ->
                       {We1, Edge} = wings_vertex:force_connect(Last,V1,Face,We0),
                       {[Edge|EL0],We1};
                   false ->
                       Edge = wings_vertex:edge_through(V1,Last,Face,We0),
                       {[Edge|EL0],We0}
               end,
    Es = wings_edge:from_vs(Drop, We2),
    We3 = wings_edge:dissolve_edges(Es, We2),
    ok = wings_we_util:validate(We3),
    {EL, [Face], We3}.

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

build_vtx_loops(Edges, _Acc) ->
    G = make_lookup_table(Edges),
    Comps = digraph_utils:components(G),
    %% ?D("Cs: ~w~n",[Comps]),
    Res = [build_vtx_loop(C, G) || C <- Comps],
    [] = digraph:edges(G), %% Assert that we have completed all edges
    digraph:delete(G),
    Res.

make_lookup_table(Edges) ->
    G = digraph:new(),
    Add = fun(#{p1:={_,V},p2:={_,V}}) ->
                  ignore; %% Loops
             (#{p1:={_,P1},p2:={_,P2}}=EI) ->
                  digraph:add_vertex(G, P1),
                  digraph:add_vertex(G, P2),
                  case edge_exists(G,P1,P2) of
                      false -> digraph:add_edge(G, P1, P2, EI);
                      true -> ok
                  end
          end,
    _ = [Add(EI) || EI <- Edges],
    G.

build_vtx_loop([V|_Vs], G) ->
    case build_vtx_loop(V, G, []) of
        {V, Acc} -> Acc;
        {_V, _Acc} ->
            ?dbg("V ~p => ~p~n",[V,_Acc]),
            ?dbg("Last ~p~n",[_V]),
            error(incomplete_edge_loop)
    end.

build_vtx_loop(V0, G, Acc) ->
    case [digraph:edge(G, E) || E <- digraph:edges(G, V0)] of
        [] -> {V0, Acc};
        Es ->
            {Edge, Next, Ei} = pick_edge(Es, V0, undefined),
            %?D("~p in ~P~n => ~p ~n",[V0, Es, 10, Next]),
            digraph:del_edge(G, Edge),
            build_vtx_loop(Next, G, [Ei,V0|Acc])
    end.

edge_exists(G,V1,V2) ->
    lists:member(V2, digraph:out_neighbours(G, V1)) orelse
        lists:member(V1, digraph:out_neighbours(G, V2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pick_edge([{E,V,V,Ei}|_], V, _Best) ->
    {E, V, Ei}; %% Self cyclic pick first
pick_edge([{E,V,N,Ei}|R], V, _Best) ->
    pick_edge(R, V, {E,N,Ei});
pick_edge([{E,N,V,Ei}|R], V, _Best) ->
    pick_edge(R, V, {E,N,Ei});
pick_edge([], _, Best) -> Best.

split_loop([Last|Loop], Vmap, We) ->
    split_loop(Loop, Last, Vmap, We, []).

split_loop([V1,E|Loop], Last, Vmap, We, Acc) when is_integer(V1) ->
%    ?D("~p: ~p in ~p~n",[(element(1,We))#we.id,V1,E]),
    Vertex = vertex_info(E, V1, Vmap, We),
    split_loop(Loop, Last, Vmap, We, [Vertex|Acc]);
split_loop([V1], E, Vmap, We, Acc) ->
%    ?D("~p: ~p in ~p~n",[(element(1,We))#we.id,V1,E]),
    Vertex = vertex_info(E, V1, Vmap, We),
    lists:reverse([Vertex|Acc]).

vertex_info(#{mf1:={O1,F1}, mf2:={O2,F2}, other:={O3,F3},
              p1:={{_, {A1,B1}=Edge1},V0},
              p2:={{_, {A2,B2}=Edge2},V0}},
            V0, Vmap, {#we{id=Id}=We,OWe}) ->
    if O1 =:= Id ->
            ON = other_normal(O2,Id,F3,F2,OWe),
            Edge = wings_vertex:edge_through(A1,B1,F1,We),
            Fs = edge_faces(Edge,F1,We),
            SF=#{op=>split_edge, o=>Id, f=>F1, e=>Edge, v=>V0, vs=>Edge1, fs=>Fs, o_n=>ON},
	    on_vertex(SF, Vmap);
       O2 =:= Id ->
            ON = other_normal(O1,Id,F3,F1,OWe),
            Edge = wings_vertex:edge_through(A2,B2,F2,We),
            Fs = edge_faces(Edge,F1,We),
            SF=#{op=>split_edge, o=>Id, f=>F2, e=>Edge, v=>V0, vs=>Edge2, fs=>Fs, o_n=>ON},
	    on_vertex(SF, Vmap);
       O3 =:= Id ->
            ON = wings_face:normal(F1, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F3, v=>V0, o_n=>ON}, Vmap, We)
    end;
vertex_info(#{mf1:={O1,F1}, mf2:={O2,F2}, other:={O3,F3}, p1:={{_, {A,B}=Edge0},V0}}, V0,
            Vmap, {#we{id=Id}=We,OWe}) ->
    if O1 =:= Id ->
            ON = other_normal(O2,Id,F3,F2,OWe),
            Edge = wings_vertex:edge_through(A,B,F1,We),
            Fs = edge_faces(Edge,F1,We),
            SF=#{op=>split_edge, o=>Id, f=>F1, e=>Edge, v=>V0, vs=>Edge0, fs=>Fs, o_n=>ON},
	    on_vertex(SF, Vmap);
       O2 =:= Id ->
            ON = wings_face:normal(F1, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F2, v=>V0, o_n=>ON}, Vmap, We);
       O3 =:= Id ->
            ON = wings_face:normal(F1, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F3, v=>V0, o_n=>ON}, Vmap, We)
    end;
vertex_info(#{mf2:={O1,F1}, mf1:={O2,F2}, other:={O3,F3}, p2:={{_, {A,B}=Edge0},V0}}, V0,
            Vmap, {#we{id=Id}=We,OWe}) ->
    if O1 =:= Id ->
            ON = other_normal(O2,Id,F3,F2,OWe),
            Edge = wings_vertex:edge_through(A,B,F1,We),
            Fs = edge_faces(Edge,F1,We),
            SF = #{op=>split_edge, o=>Id, f=>F1, e=>Edge, v=>V0, vs=>Edge0, fs=>Fs, o_n=>ON},
	    on_vertex(SF, Vmap);
       O2 =:= Id ->
            ON = wings_face:normal(F1, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F2, v=>V0, o_n=>ON}, Vmap, We);
       O3 =:= Id ->
            ON = wings_face:normal(F1, OWe),
            check_if_edge(#{op=>split_face, o=>Id, f=>F3, v=>V0, o_n=>ON}, Vmap, We)
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
    {array:from_orddict(OrdD), Acc}.

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

