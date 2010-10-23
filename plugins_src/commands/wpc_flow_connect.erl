%%
%%  wpc_flow_connect.erl --
%%
%%    Connect edges with respect to surrounding edge angles.
%%
%%  Copyright (c) 2010 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_flow_connect).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

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
    {?__(1,"Flow Connect"),flow_connect_fun(),
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
    {St1,Sel} = wings_sel:mapfold(fun(Edges, #we{id=Id}=We0, Acc) ->
        {NewEdges,We} = calculate_cuts(Edges, Edges, We0, []),
        {We,[{Id,NewEdges}|Acc]}
    end, [], St0),
    St = wings_sel:set(edge, Sel, St1),
    {save_state,wings_sel:valid_sel(St)}.

flow_connect_drag(St0) ->
    {St1,{Tvs,Sel}} = wings_sel:mapfold(fun(Edges, #we{id=Id}=We0, {Tvs0,SelAcc}) ->
        {NewEdges,Vs,Data,We} = calculate_cuts_data(Edges, Edges, We0, []),
        TvsData = [{Id,{Vs,fc_tension_fun(Data)}}|Tvs0],
        {We,{TvsData,[{Id,NewEdges}|SelAcc]}}
    end, {[],[]}, St0),
    St = wings_sel:set(edge, Sel, St1),
    wings_drag:setup(Tvs, [percent], wings_sel:valid_sel(St)).

calculate_cuts_data(Edges0, Es, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
        true ->
            cut_edges(Acc, We, [], []);
        false ->
            {Edge,Edges} = gb_sets:take_smallest(Edges0),
            CutData = edge_link_vectors(Edge, Es, We),
            calculate_cuts_data(Edges, Es, We, [{Edge,CutData}|Acc])
    end.

calculate_cuts(Edges0, Es, We, Acc) ->
    case gb_sets:is_empty(Edges0) of
        true ->
            cut_edges(Acc, We, []);
        false ->
            {Edge,Edges} = gb_sets:take_smallest(Edges0),
            {Mid,Vec,Opp} = edge_link_vectors(Edge, Es, We),
            Pos = e3d_vec:add(Mid, e3d_vec:mul(Vec, Opp)),
            calculate_cuts(Edges, Es, We, [{Edge,Pos}|Acc])
    end.

edge_link_vectors(Edge, Es, #we{mirror=Mir,es=Etab,vp=Vtab}=We) ->
    #edge{vs=Va,ve=Vb,ltpr=Lp,rtpr=Rp,lf=Lf,rf=Rf} = array:get(Edge, Etab),
    PosA = array:get(Va, Vtab),
    PosB = array:get(Vb, Vtab),
    Mid = e3d_vec:average(PosA, PosB),
    EvecA = e3d_vec:sub(PosA, Mid),
    EvecB = e3d_vec:sub(PosB, Mid),
    ENormA = e3d_vec:norm(EvecA),
    ENormB = e3d_vec:norm(EvecB),
    Len = e3d_vec:len(EvecA),
    EdgeNorm = edge_norm(Lf, Rf, We),
    OrgMir = orig_on_mirror(Mir, Lf, Rf),
    {OppR,RVec,Nr} = vec_acc(Va, Rp, PosA, PosB, Edge, ENormA, Es, Len, EdgeNorm, OrgMir, We, []),
    {OppL,LVec,Nl} = vec_acc(Vb, Lp, PosB, PosA, Edge, ENormB, Es, Len, EdgeNorm, OrgMir, We, []),
    %% Favour poles of 4 (a vertex connecting four edges)
    case OppR < OppL of
        true when Nr=:=4 ->
            cut_point_data(PosA, Mid, ENormA, RVec, OppR);
        true when is_atom(OppL) ->
            cut_point_data(PosA, Mid, ENormA, RVec, OppR);
        true when Nl=:=4 ->
            cut_point_data(PosB, Mid, ENormB, LVec, OppL);
        true ->
            cut_point_data(PosA, Mid, ENormA, RVec, OppR);
        false when is_atom(OppR) andalso is_atom(OppL) ->
            {Mid,e3d_vec:zero(),0.0};
        false when Nl=:=4 ->
            cut_point_data(PosB, Mid, ENormB, LVec, OppL);
        false when Nr=:=4 andalso not is_atom(OppR) ->
            cut_point_data(PosA, Mid, ENormA, RVec, OppR);
        false ->
            cut_point_data(PosB, Mid, ENormB, LVec, OppL)
    end.

edge_norm(Mir, Face, #we{mirror=Mir}=We) ->
    MirNorm = wings_face:normal(Mir, We),
    FNorm = wings_face:normal(Face, We),
    U = e3d_vec:mul(MirNorm, e3d_vec:dot(MirNorm, FNorm)),
    InMir = e3d_vec:sub(FNorm, e3d_vec:mul(U, 2.0)),
    e3d_vec:norm(e3d_vec:add(FNorm, InMir));
edge_norm(Face, Mir, #we{mirror=Mir}=We) ->
    MirNorm = wings_face:normal(Mir, We),
    FNorm = wings_face:normal(Face, We),
    U = e3d_vec:mul(MirNorm, e3d_vec:dot(MirNorm, FNorm)),
    InMir = e3d_vec:sub(FNorm, e3d_vec:mul(U, 2.0)),
    e3d_vec:norm(e3d_vec:add(FNorm, InMir));
edge_norm(Lf, Rf, We) ->
    Ln = wings_face:normal(Lf, We),
    Rn = wings_face:normal(Rf, We),
    e3d_vec:norm(e3d_vec:add(Ln, Rn)).

orig_on_mirror(Mir, Mir, _) -> true;
orig_on_mirror(Mir, _, Mir) -> true;
orig_on_mirror(_, _, _) -> false.

vec_acc(V, Edge, PosA, PosB, OrigE, EVec, Es, Len, EdgeNorm, OrgMir, We, Acc) ->
    case adjacent_vector(V, Edge, PosA, PosB, OrigE, OrgMir, We) of
        {VecE,OrigE} ->
            get_best_vec(EVec, Es, Len, EdgeNorm, [VecE|Acc]);
        {VecE,NextE} ->
            vec_acc(V, NextE, PosA, PosB, OrigE, EVec, Es, Len, EdgeNorm, OrgMir, We, [VecE|Acc]);
        {VecE,MirVec,OrigE} ->
            get_best_vec(EVec, Es, Len, EdgeNorm, [VecE,MirVec|Acc]);
        {VecE,MirVec,NextE} ->
            vec_acc(V, NextE, PosA, PosB, OrigE, EVec, Es, Len, EdgeNorm, OrgMir, We, [VecE,MirVec|Acc])
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
                    MirPlane = wings_face:normal(Mir, We),
                    MirCenter = wings_face:center(Mir, We),
                    PosAtMir = intersect_vec_plane(PosB, MirCenter, MirPlane, MirPlane),
                    Vec = e3d_vec:sub(PosAtMir, PosB),
                    PosInMir = e3d_vec:add(PosAtMir, Vec),
                    MirVec = e3d_vec:norm_sub(PosA, PosInMir),
                    {{VecE,Edge},{MirVec,OrigE},NextE}
            end;
        #edge{vs=Va,ve=V,rtpr=NextE,rf=Rf,lf=Lf} when Rf=:=Mir; Lf=:=Mir ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                true ->
                    {{VecE,Edge},NextE};
                false ->
                    MirPlane = wings_face:normal(Mir, We),
                    MirCenter = wings_face:center(Mir, We),
                    PosAtMir = intersect_vec_plane(PosB, MirCenter, MirPlane, MirPlane),
                    Vec = e3d_vec:sub(PosAtMir, PosB),
                    PosInMir = e3d_vec:add(PosAtMir, Vec),
                    MirVec = e3d_vec:norm_sub(PosA, PosInMir),
                    {{VecE,Edge},{MirVec,OrigE},NextE}
            end;
        #edge{vs=V,ve=Va,ltpr=NextE} ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                false ->
                    {{VecE,Edge},NextE};
                true ->
                    MirPlane = wings_face:normal(Mir, We),
                    MirCenter = wings_face:center(Mir, We),
                    PosAtMir = intersect_vec_plane(PosS, MirCenter, MirPlane, MirPlane),
                    Vec = e3d_vec:sub(PosAtMir, PosS),
                    PosInMir = e3d_vec:add(PosAtMir, Vec),
                    MirVec = e3d_vec:norm_sub(PosA, PosInMir),
                    {{VecE,Edge},{MirVec,Edge},NextE}
            end;
        #edge{vs=Va,ve=V,rtpr=NextE} ->
            PosS = array:get(V, Vtab),
            VecE = e3d_vec:norm_sub(PosA, PosS),
            case OrgMir of
                false ->
                    {{VecE,Edge},NextE};
                true ->
                    MirPlane = wings_face:normal(Mir, We),
                    MirCenter = wings_face:center(Mir, We),
                    PosAtMir = intersect_vec_plane(PosS, MirCenter, MirPlane, MirPlane),
                    Vec = e3d_vec:sub(PosAtMir, PosS),
                    PosInMir = e3d_vec:add(PosAtMir, Vec),
                    MirVec = e3d_vec:norm_sub(PosA, PosInMir),
                    {{VecE,Edge},{MirVec,Edge},NextE}
            end
    end.

get_best_vec(EVec, Es, Len, EdgeNorm, Acc0) ->
    Acc = lists:usort(Acc0),
    N = length(Acc)+1,
    New = {not_allowed,none,0.0,[]},
    {Opp0,Vec0,DN0,VecAcc} = get_best_vec_1(EVec, Es, Len, EdgeNorm, New, Acc),
    case N rem 2 of
        0 ->{Opp0,Vec0,N};
        1 ->
            AvgVec = e3d_vec:average(VecAcc),
            Check1 = {Opp0,Vec0,DN0,[]},
            Check2 = [{AvgVec,gb_sets:smallest(Es)}],
            {Opp,Vec,_,_} = get_best_vec_1(EVec, Es, Len, EdgeNorm, Check1, Check2),
            case round_float(Opp) < round_float(Opp0) of
                true -> {Opp,Vec,N};
                false -> {Opp0,Vec0,N}
            end
    end.

get_best_vec_1(EVec, Es, Len, EdgeNorm, New, Acc) ->
    lists:foldl(fun({Vec,Edge}, {Opp0,Vec0,DN0,VecAcc}) ->
        Deg = round_float(e3d_vec:degrees(EVec, Vec)),
        Cross = e3d_vec:norm(e3d_vec:cross(EVec, Vec)),
        Dot0 = e3d_vec:dot(EdgeNorm, Cross),
        Dot = abs(Dot0),
        DN = if
            Dot >= 1.0 -> 180.0;
            true -> round_float(math:acos(Dot) * (180.0 / math:pi()))
        end,
        case DN >= DN0 of
            true ->
                case get_result(Deg, Len, gb_sets:is_element(Edge, Es)) of
                    not_allowed ->
                        {Opp0,Vec0,DN0,[Vec|VecAcc]};
                    Opp when Opp < Opp0 andalso DN =:= DN0 ->
                        {Opp,Vec,DN,[Vec|VecAcc]};
                    _Opp when DN =:= DN0 ->
                        {Opp0,Vec0,DN0,[Vec|VecAcc]};
                    Opp ->
                        {Opp,Vec,DN,[Vec|VecAcc]}
                end;
            false ->
                {Opp0,Vec0,DN0,[Vec|VecAcc]}
        end
    end, New, Acc).

cut_point_data(PosA, Mid, Evec, Vec0, FinalOpp) ->
    Pos = intersect_vec_plane(PosA, Mid, e3d_vec:norm(Evec), Vec0),
    Vec = e3d_vec:norm_sub(Pos, Mid),
    {Mid,Vec,FinalOpp}.

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
cut_edges([{Edge,{Mid,Vec,Opp}}|Edges], We0, Acc, TvsAcc) ->
    {We,V} = wings_edge:fast_cut(Edge, Mid, We0),
    cut_edges(Edges, We, [V|Acc], [{V,Mid,Vec,Opp}|TvsAcc]);
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
    {NewEdges,We}.

round_float(Float) when is_float(Float) ->
    round(10000*Float)/10000;
round_float(Other) -> Other.

fc_tension_fun(Data) ->
    fun
        ([Percent], A) ->
            lists:foldl(fun({V,Mid,Vec,Opp}, VpAcc) ->
                [{V,e3d_vec:add(Mid, e3d_vec:mul(Vec, Opp*Percent))}|VpAcc]
            end, A, Data)
    end.