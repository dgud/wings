%%
%%  wpc_corner.erl --
%%
%%    Add corners to edges bordered by a tri and a pentagon or a combination
%%    of such faces with any number of quads in between.
%%
%%  Copyright (c) 2010-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_corner).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-import(lists, [foldl/3]).

init() ->
    true.

%% Menu
menu({edge}, Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [corner_menu(), separator|NewMenu];
parse([A = {_,connect,_}|Rest], NewMenu, false) ->
    parse(Rest, [A,corner_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

corner_menu() ->
    {?__(1,"Corner"),corner,
     ?__(2,"Make corners from edge rings capped by a 3 and 5 sided face")}.

%% Command
command({edge,corner}, St) ->
    corner(St);
command(_, _) ->
    next.

%% Process Command
corner(St0) ->
    ErrorMsg = ?__(1,"You can only add corners to edges that have a 3 sided\n"
                   "face on one end, and a 5 sided face on the other."),
    F = fun(Edges, We0) ->
                CornerEs = corner_edges(Edges, Edges, We0, []),
                if
                    CornerEs =:= [] ->
                        wings_u:error_msg(ErrorMsg);
                    true ->
                        ok
                end,
                {Vs,VD,We,EdgeSel} = corner_1(Edges, CornerEs, We0),
                EsVs = wings_edge:to_vertices(Edges, We) -- Vs,
                EsVpos = wings_util:add_vpos(EsVs, We),
                AllVs = Vs ++ EsVs,
                Tv = {AllVs,corner_tension_fun(VD ++ EsVpos)},
                {We#we{temp=Tv},EdgeSel}
        end,
    St = wings_sel:map_update_sel(F, St0),
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, [angle], St).

corner_1(Edges, CornerEdges, We0) ->
    foldl(
      fun([H|_]=D, Acc) when is_list(D) ->
              MD = lists:max(D),
              Data = if MD =:= H -> D;
                        true -> lists:reverse(D)
                     end,
              complex_corner_0(Data, Acc);
         ({Edge,Face,CV,Va}, {VList,VD0,We1,EsAcc}) ->
              {We2,NewVertex} = wings_edge:cut(Edge, 2, We1),
              [NewEdge] = wings_we:new_items_as_ordset(edge, We1, We2),
              {VData,We3} = connect_five_sided(NewVertex, CV, Va, Face, We2),
              NewEs = gb_sets:add(NewEdge, EsAcc),
              {[NewVertex|VList],[VData|VD0],We3,NewEs}
      end, {[],[],We0,Edges}, CornerEdges).

corner_edges(Edges0, AllEs, #we{es=Etab}=We, Acc0) ->
    case gb_sets:is_empty(Edges0) of
      true ->
        Acc0;
      false ->
        {Edge,Edges} = gb_sets:take_smallest(Edges0),
        #edge{vs=Va,lf=Lf,rf=Rf} = array:get(Edge, Etab),
        LeftVs = wings_face:vertices_ccw(Lf, Edge, We),
        RightVs = wings_face:vertices_ccw(Rf, Edge, We),
        case length(LeftVs) of
          3 ->
            case length(RightVs) of
              5 ->
              % io:format("~p\n",[okay1]),
                CV = lists:nth(3, RightVs),
                corner_edges(Edges, AllEs, We, [{Edge,Rf,CV,Va}|Acc0]);
              4 ->
              % io:format("~p\n",[okay2]),
                find(5, Edge, Rf, [{Edge,Rf,Va}], Edges, AllEs, We, Acc0);
              _ -> corner_edges(Edges, AllEs, We, Acc0)
            end;
          5 ->
            case length(RightVs) of
              3 ->
              % io:format("~p\n",[okay3]),
                CV = lists:nth(3, LeftVs),
                corner_edges(Edges, AllEs, We, [{Edge,Lf,CV,Va}|Acc0]);
              4 ->
              % io:format("~p\n",[okay4]),
                CV = lists:nth(3, LeftVs),
                find(3, Edge, Rf, [{Edge,Lf,CV,Va}], Edges, AllEs, We, Acc0);
              _ -> corner_edges(Edges, AllEs, We, Acc0)
            end;
          4 ->
            case length(RightVs) of
              3 ->
              % io:format("~p\n",[okay5]),
                find(5, Edge, Lf, [{Edge,Lf,Va}], Edges, AllEs, We, Acc0);
              5 ->
              % io:format("~p\n",[okay6]),
                CV = lists:nth(3, RightVs),
                find(3, Edge, Lf, [{Edge,Rf,CV,Va}], Edges, AllEs, We, Acc0);
              _ -> corner_edges(Edges, AllEs, We, Acc0)
            end;
          _ ->
              corner_edges(Edges, AllEs, We, Acc0)
        end
    end.

find(N, Edge, Face, E2Cut, Edges0, AllEs, #we{es=Etab}=We, Acc0) ->
    Itr0 = wings_face:iterator(Face, We),
    Itr1 = wings_face:skip_to_edge(Edge, Itr0),
    {_,_,_,Itr2} = wings_face:next_ccw(Itr1),
    {_,_,_,Itr3} = wings_face:next_ccw(Itr2),
    {_,E,_,_} = wings_face:next_ccw(Itr3),
    case gb_sets:is_member(E, AllEs) of
      true ->
        #edge{vs=Va,lf=Lf,rf=Rf} = array:get(E, Etab),
        case Face of
          Lf ->
            RightVs = wings_face:vertices_ccw(Rf, E, We),
            case length(RightVs) of
              5 when N =:= 5 ->
              % io:format("~p\n",[okay7]),
                CV = lists:nth(3, RightVs),
                Edges = gb_sets:delete(E, Edges0),
                corner_edges(Edges, AllEs, We, [[{E,Rf,CV,Va}|E2Cut]|Acc0]);
              3 when N =:= 3 ->
              % io:format("~p\n",[okay8]),
                Edges = gb_sets:delete(E, Edges0),
                corner_edges(Edges, AllEs, We, [[{E,Lf,Va}|E2Cut]|Acc0]);
              4 when N =:= 5 ->
              % io:format("~p\n",[okay9]),
                find(N, E, Rf, [{E,Rf,Va}|E2Cut], Edges0, AllEs, We, Acc0);
              4 when N =:= 3 ->
              % io:format("~p\n",[okay10]),
                find(N, E, Rf, [{E,Lf,Va}|E2Cut], Edges0, AllEs, We, Acc0);
              _ ->
                corner_edges(Edges0, AllEs, We, Acc0)
            end;
          Rf ->
            LeftVs = wings_face:vertices_ccw(Lf, E, We),
            case length(LeftVs) of
              5 when N =:= 5 ->
              % io:format("~p\n",[okay11]),
                CV = lists:nth(3, LeftVs),
                Edges = gb_sets:delete(E, Edges0),
                corner_edges(Edges, AllEs, We, [[{E,Lf,CV,Va}|E2Cut]|Acc0]);
              3 when N =:= 3 ->
              % io:format("~p\n",[okay12]),
                Edges = gb_sets:delete(E, Edges0),
                corner_edges(Edges, AllEs, We, [[{E,Rf,Va}|E2Cut]|Acc0]);
              4 when N =:= 5 ->
              % io:format("~p\n",[okay13]),
                find(N, E, Lf, [{E,Lf,Va}|E2Cut], Edges0, AllEs, We, Acc0);
              4 when N =:= 3 ->
              % io:format("~p\n",[okay14]),
                find(N, E, Lf, [{E,Rf,Va}|E2Cut], Edges0, AllEs, We, Acc0);
              _ ->
                corner_edges(Edges0, AllEs, We, Acc0)
            end
        end;
      false ->
        corner_edges(Edges0, AllEs, We, Acc0)
    end.

complex_corner_0([{Edge,Face,CV,Va}|Data], {VList,VD,We0,NewEs0}) ->
    {We1,NewVertex} = wings_edge:cut(Edge, 2, We0),
    [NewEdge] = wings_we:new_items_as_ordset(edge, We0, We1),
    {VData,We} = connect_five_sided(NewVertex, CV, Va, Face, We1),
    NewEs = gb_sets:add(NewEdge, NewEs0),
    complex_corner_1(Data, NewVertex, {[NewVertex|VList],[VData|VD],We,NewEs}).

complex_corner_1([{Edge,Face,Va}|Data], Vb, {VList,VD,We0,NewEs0}) ->
    {We1,NewVertex} = wings_edge:cut(Edge, 2, We0),
    [NewEdge] = wings_we:new_items_as_ordset(edge, We0, We1),
    {VData,We} = connect_five_sided(NewVertex, Vb, Va, Face, We1),
    NewEs = gb_sets:add(NewEdge, NewEs0),
    complex_corner_1(Data, NewVertex, {[NewVertex|VList],[VData|VD],We,NewEs});
complex_corner_1([], _, Acc) ->
    Acc.

connect_five_sided(NewVertex, CV, Va, Face, We0) ->
    Vs = wings_face:to_vertices([Face], We0),
    Vsp = ordsets:from_list([Va,NewVertex,CV]),
    case ordsets:is_subset(Vsp, ordsets:from_list(Vs)) of
      true ->
        case catch wings_vertex:force_connect(NewVertex, CV, Face, We0) of
          {#we{vp=Vtab}=We,_} ->
              P0 = array:get(NewVertex, Vtab),
              P1 = array:get(CV, Vtab),
              P2 = array:get(Va, Vtab),
              Rad = e3d_vec:dist(P0, P2),
              Vec = e3d_vec:norm_sub(P1, P0),
              {{NewVertex,P0,Rad,Vec},We};
          _Error ->
              corner_error()
        end;
      false ->
        corner_error()
    end.

-spec corner_error() -> no_return().
corner_error() ->
    wings_u:error_msg(?__(1,"Sorry, that selection goes beyond the scope of this tool.")).

corner_tension_fun(VD) ->
    fun
        ([0.0], A) ->
            lists:foldl(fun
                ({V,Vpos,_Rad,_Vec}, VsAcc) ->
                    [{V,Vpos}|VsAcc];
                (VPos, VsAcc) ->
                    [VPos|VsAcc]
            end, A, VD);
        ([Angle], A) ->
            lists:foldl(fun
                ({V,Vpos,Rad,Vec}, VsAcc) ->
                    %% Erlang trigonomic inputs have to be converted from Degrees to Radians
                    Radians = (math:pi()/(360.0/Angle)),
                    Dist = math:tan(Radians) * Rad,
                    Pos = e3d_vec:add_prod(Vpos, Vec, Dist),
                    [{V,Pos}|VsAcc];
                (VPos, VsAcc) ->
                    [VPos|VsAcc]
            end, A, VD)
    end.
