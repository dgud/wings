%%
%%  wpc_flow_connect.erl --
%%
%%    Connect edges with respect to surrounding edge angles.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_flow_connect).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-define(ANGLE, 90.0).

init() ->
    true.

%%% Menu
menu({edge},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [flow_connect(),separator|NewMenu];
parse([A = {_,bevel,_}|Rest], NewMenu, false) ->
    parse(Rest, [flow_connect(),A|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

flow_connect() ->
    {?__(1,"Flow Connect"),{flow_connect, flow_connect_fun()},
     {?__(2,"Connect edges with respect to the surrounding geometry"),[],
      ?__(3,"Flow Connect and move edges into place")},[]}.

flow_connect_fun() ->
    fun
        (1, _) -> {edge,flow_connect};
        (3, _) -> {edge,flow_connect_drag};
        (_, _) -> ignore
    end.

%%% Commands
command({edge,flow_connect}, St) ->
    flow_connect(St);
command({edge,flow_connect_drag}, St) ->
    flow_connect_drag(St);
command(_, _) ->
    next.

%%% Functions
flow_connect(St0) ->
    F = fun calculate_cuts/2,
    St1 = wings_sel:map_update_sel(F, St0),
    St = wings_sel:valid_sel(St1),
    {save_state,St}.

flow_connect_drag(St0) ->
    F = fun(Edges0, We0) ->
                {Edges,Vs,Data,We} = calculate_cuts_data(Edges0, We0),
                Tv = {Vs,flow_connect_tension_fun(Data, false)},
                {We#we{temp=Tv},Edges}
        end,
    St1 = wings_sel:map_update_sel(F, St0),
    St = wings_sel:valid_sel(St1),
    Flags = [{mode,{mode(),false}}],
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, [percent], Flags, St).

mode() ->
    fun
      (help, State) -> help(State);
      ({key,$1}, true) -> false;
      ({key,$1}, false) -> true;
      (_,_) -> none
    end.

help(false) -> "[1] " ++ ?__(1,"Move in direction of face normals");
help(true) -> "[1] " ++ ?__(2,"Move in direction of geometry flow").

calculate_cuts_data(Edges, We) ->
    calculate_cuts_data(Edges, Edges, We, []).

calculate_cuts_data(Edges0, Es, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
        true ->
            cut_edges(Acc, We, [], []);
        false ->
            {Edge,Edges} = gb_sets:take_smallest(Edges0),
            CutData = edge_link_vectors(Edge, Es, We),
            calculate_cuts_data(Edges, Es, We, [{Edge,CutData}|Acc])
    end.

calculate_cuts(Edges, We) ->
    calculate_cuts(Edges, Edges, We, []).

calculate_cuts(Edges0, Es, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
        true ->
            cut_edges(Acc, We, []);
        false ->
            {Edge,Edges} = gb_sets:take_smallest(Edges0),
            {Mid,Vec,_,Opp} = edge_link_vectors(Edge, Es, We),
            Pos = e3d_vec:add(Mid, e3d_vec:mul(Vec, Opp)),
            calculate_cuts(Edges, Es, We, [{Edge,Pos}|Acc])
    end.

edge_link_vectors(Edge, Es, #we{mirror=Mir,es=Etab,vp=Vtab}=We) ->
    #edge{vs=Va,ve=Vb,ltpr=Lp,rtpr=Rp,lf=Lf,rf=Rf} = array:get(Edge, Etab),
    FNorm = edge_normal(Lf, Rf, We),
    PosA = array:get(Va, Vtab),
    PosB = array:get(Vb, Vtab),
    Mid = e3d_vec:average(PosA, PosB),
    EvecA = e3d_vec:sub(PosA, Mid),
    EvecB = e3d_vec:sub(PosB, Mid),
    ENormA = e3d_vec:norm(EvecA),
    ENormB = e3d_vec:norm(EvecB),
    Len = e3d_vec:len(EvecA),
    OrgMir = orig_on_mirror(Mir, Lf, Rf),
    {OppR,RVec,Nr} = vec_acc(Va, Rp, PosA, PosB, Edge, ENormA, Es, Len, OrgMir, We, []),
    {OppL,LVec,Nl} = vec_acc(Vb, Lp, PosB, PosA, Edge, ENormB, Es, Len, OrgMir, We, []),
    %% Favour poles of 4 (a vertex connecting four edges)
    case OppR < OppL of
        true when Nr=:=4 ->
            cut_point_data(PosA, Mid, ENormA, RVec, FNorm, OppR);
        true when is_atom(OppL) ->
            cut_point_data(PosA, Mid, ENormA, RVec, FNorm, OppR);
        true when Nl=:=4 ->
            cut_point_data(PosB, Mid, ENormB, LVec, FNorm, OppL);
        true ->
            cut_point_data(PosA, Mid, ENormA, RVec, FNorm, OppR);
        false when is_atom(OppR) andalso is_atom(OppL) ->
            {Mid,e3d_vec:zero(),e3d_vec:zero(),0.0};
        false when Nl=:=4 ->
            cut_point_data(PosB, Mid, ENormB, LVec, FNorm, OppL);
        false when Nr=:=4 andalso not is_atom(OppR) ->
            cut_point_data(PosA, Mid, ENormA, RVec, FNorm, OppR);
        false ->
            cut_point_data(PosB, Mid, ENormB, LVec, FNorm, OppL)
    end.

edge_normal(Fa, Fb, We) ->
    FaNorm = wings_face:normal(Fa, We),
    FbNorm = wings_face:normal(Fb, We),
    e3d_vec:norm(e3d_vec:add(FaNorm, FbNorm)).

%% Vector in Mirror
mirrored_vector(Vec, Mir, We) ->
    MirNorm = wings_face:normal(Mir, We),
    U = e3d_vec:mul(MirNorm, e3d_vec:dot(MirNorm, Vec)),
    InMir = e3d_vec:sub(Vec, e3d_vec:mul(U, 2.0)),
    e3d_vec:norm(e3d_vec:add(Vec, InMir)).

orig_on_mirror(Mir, Mir, _) -> true;
orig_on_mirror(Mir, _, Mir) -> true;
orig_on_mirror(_, _, _) -> false.

vec_acc(V, Edge, PosA, PosB, OrigE, EVec, Es, Len, OrgMir, We, Acc) ->
    case adjacent_vector(V, Edge, PosA, PosB, OrigE, OrgMir, We) of
        {VecE,OrigE} ->
            get_best_vec(EVec, Es, Len, [VecE|Acc]);
        {VecE,NextE} ->
            vec_acc(V, NextE, PosA, PosB, OrigE, EVec, Es, Len, OrgMir, We, [VecE|Acc]);
        {VecE,MirVec,OrigE} ->
            get_best_vec(EVec, Es, Len, [VecE,MirVec|Acc]);
        {VecE,MirVec,NextE} ->
            vec_acc(V, NextE, PosA, PosB, OrigE, EVec, Es, Len, OrgMir, We, [VecE,MirVec|Acc])
    end.

adjacent_vector(Va, Edge, PosA, PosB, OrigE, OrgMir, #we{mirror=Mir,es=Etab,vp=Vtab}=We) ->
    case array:get(Edge, Etab) of
        #edge{vs=V,ve=Va,ltpr=NextE,rf=Rf,lf=Lf} when Rf=:=Mir; Lf=:=Mir ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                true ->
                    {{VecE,Edge},NextE};
                false ->
                    Vec = e3d_vec:norm_sub(PosA, PosB),
                    MirVec = mirrored_vector(Vec, Mir, We),
                    {{VecE,Edge},{MirVec,OrigE},NextE}
            end;
        #edge{vs=Va,ve=V,rtpr=NextE,rf=Rf,lf=Lf} when Rf=:=Mir; Lf=:=Mir ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                true ->
                    {{VecE,Edge},NextE};
                false ->
                    Vec = e3d_vec:norm_sub(PosA, PosB),
                    MirVec = mirrored_vector(Vec, Mir, We),
                    {{VecE,Edge},{MirVec,OrigE},NextE}
            end;
        #edge{vs=V,ve=Va,ltpr=NextE} ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                false ->
                    {{VecE,Edge},NextE};
                true ->
                    Vec = e3d_vec:norm_sub(PosA, PosS),
                    MirVec = mirrored_vector(Vec, Mir, We),
                    {{VecE,Edge},{MirVec,Edge},NextE}
            end;
        #edge{vs=Va,ve=V,rtpr=NextE} ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                false ->
                    {{VecE,Edge},NextE};
                true ->
                    Vec = e3d_vec:norm_sub(PosA, PosS),
                    MirVec = mirrored_vector(Vec, Mir, We),
                    {{VecE,Edge},{MirVec,Edge},NextE}
            end
    end.

get_best_vec(EVec, Es, Len, Acc0) ->
    Acc = lists:usort(Acc0),
    N = length(Acc)+1,
    New = {not_allowed,none,?ANGLE,[]},
    {Opp0,Vec0,Deg0,VecAcc} = get_best_vec_1(EVec, Es, Len, New, Acc),
    case N rem 2 of
        0 -> {Opp0,Vec0,N};
        1 ->
            AvgVec = e3d_vec:average(VecAcc),
            Check1 = {Opp0,Vec0,Deg0,[]},
            Check2 = [{AvgVec,gb_sets:smallest(Es)}],
            {Opp,Vec,_,_} = get_best_vec_1(EVec, Es, Len, Check1, Check2),
            case round_float(Opp) < round_float(Opp0) of
                true -> {Opp,Vec,N};
                false -> {Opp0,Vec0,N}
            end
    end.

get_best_vec_1(EVec, Es, Len, New, Acc) ->
    lists:foldl(fun({Vec,Edge}, {Opp0,Vec0,Deg0,VecAcc}) ->
        Deg = round_float(e3d_vec:degrees(EVec, Vec)),
        case get_result(Deg, Len, gb_sets:is_element(Edge, Es)) of
            Opp when Deg > Deg0 ->
                {Opp,Vec,Deg,[Vec|VecAcc]};
            _Opp ->
                {Opp0,Vec0,Deg0,[Vec|VecAcc]}
        end
    end, New, Acc).

cut_point_data(PosA, Mid, Evec, Vec0, FNorm, FinalOpp) ->
    Pos = intersect_vec_plane(PosA, Mid, e3d_vec:norm(Evec), Vec0),
    Vec = e3d_vec:norm_sub(Pos, Mid),
    case e3d_vec:dot(Vec, FNorm) < 0 of
        true -> {Mid,Vec,e3d_vec:neg(Vec),FinalOpp};
        false -> {Mid,Vec,Vec,FinalOpp}
    end.

get_result(Deg, _, _) when Deg =< ?ANGLE ->
    not_allowed;
get_result(Deg, Len, true) ->
    opposite(Deg, Len, 4);
get_result(Deg, Len, false) ->
    opposite(Deg, Len, 3).

opposite(Deg, Adjacent, Divider) ->
    D0 = (180 - Deg)/Divider,
    case D0 of
        0.0 -> 0.0;
        _ ->
            Radians = math:pi()/(180/D0),
            Adjacent * math:tan(Radians)
    end.

intersect_vec_plane(PosA, PosB, Plane, Vec) ->
%% Return point where Vec through PosA intersects with Plane at PosB
    case e3d_vec:dot(Vec,Plane) of
      0.0 ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane),
        e3d_vec:add(PosB, e3d_vec:mul(Plane, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane) / Dot,
        e3d_vec:add(PosA, e3d_vec:mul(Vec, Intersection))
    end.

%% Drag option cut
cut_edges([{Edge,{Mid,Vec,FVec,Opp}}|Edges], We0, Acc, TvsAcc) ->
    {We,V} = wings_edge:fast_cut(Edge, Mid, We0),
    cut_edges(Edges, We, [V|Acc], [{V,Mid,Vec,FVec,Opp}|TvsAcc]);
cut_edges([], We0, Vs, TvsAcc) ->
    We = wings_vertex_cmd:connect(Vs, We0),
    NewEdges = wings_we:new_items_as_gbset(edge, We0, We),
    {NewEdges,Vs,TvsAcc,We}.

%% Just cut, no drag
cut_edges([{Edge,Pos}|Edges], We0, Acc) ->
    {We,V} = wings_edge:fast_cut(Edge, Pos, We0),
    cut_edges(Edges, We, [V|Acc]);
cut_edges([], We0, Vs) ->
    We = wings_vertex_cmd:connect(Vs, We0),
    NewEdges = wings_we:new_items_as_gbset(edge, We0, We),
    {We,NewEdges}.

round_float(Float) when is_float(Float) ->
    round(10000*Float)/10000;
round_float(Other) -> Other.

flow_connect_tension_fun(Data, Bool) ->
    fun
        (new_mode_data, {NewBool,_}) ->
            flow_connect_tension_fun(Data, NewBool);
        ([Percent], A) ->
            lists:foldl(fun({V,Mid,Vec,FVec,Opp}, VpAcc) ->
                Factor = Opp*Percent,
                Vector = if Bool -> FVec; true -> Vec end,
                [{V,e3d_vec:add(Mid, e3d_vec:mul(Vector, Factor))}|VpAcc]
            end, A, Data)
    end.
