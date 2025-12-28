
%%
%%  wpc_sel_fill.erl --
%%
%%     Fill face selection commands
%%
%%  Copyright (c) 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_sel_fill).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-export([init/0,menu/2,command/2]).


init() ->
    true.

menu({select},Menu) ->
    fill_submenu(Menu);
menu({select,fill},Menu) ->
    sel_menu_fill(fill) ++ Menu;
menu({select,grow},Menu) ->
    sel_menu_fill(grow) ++ Menu;
menu(_,Menu) ->
    Menu.

fill_submenu([{_,{fill,_}}|_]=Menu) ->
    Menu;
fill_submenu([{_,all,_}=All|Menu]) ->
    [All,
     {?__(1,"Fill"), {fill, []}},
     {?__(2,"Grow Selection"), {grow, []}}
    | Menu];
fill_submenu([A|Menu]) ->
    [A|fill_submenu(Menu)];
fill_submenu([]) ->
    [{?__(1,"Fill"), {fill, []}},
     {?__(2,"Grow Selection"), {grow, []}}].

sel_menu_fill(FillOrGrow) ->
    GrowStr = string_grow(FillOrGrow),
    [
        {?__(1,"Smoothing Group"),sel_fill_smoothing_group,
         ?__(2,"Fill face selection inside hard edges") ++ GrowStr},
        {?__(3,"Inside Sharp Edges"),sel_fill_crease_angle,
         ?__(4,"Fill face selection inside sharp edges") ++ GrowStr},
        separator,
        {?__(5,"Color Range"),sel_fill_color_range,
         ?__(6,"Fill face selection inside faces within color range") ++ GrowStr},
        {?__(7,"Connected Color Edges"),sel_fill_connected_color_edges,
         ?__(8,"Fill face selection inside connected color edges") ++ GrowStr},
        {?__(9,"Unassigned Color"),sel_fill_unassigned_color,
         ?__(10,"Fill face selection inside faces without colors") ++ GrowStr},
        separator,
        {?__(13,"Connected UV Edges"),sel_fill_connected_uv_edges,
         ?__(14,"Fill face selection connected UV edges") ++ GrowStr},
        {?__(11,"Unassigned UV"),sel_fill_unassigned_uv,
         ?__(12,"Fill face selection unassigned UV") ++ GrowStr},
        separator,
        {?__(17,"Adjacent Triangles"),sel_fill_adjacent_tri,
         ?__(18,"Fill face selection adjacent triangles") ++ GrowStr},
        {?__(19,"Adjacent Quads"),sel_fill_adjacent_quads,
         ?__(20,"Fill face selection adjacent quads") ++ GrowStr},
        {?__(21,"Adjacent NGons"),sel_fill_adjacent_ngon,
         ?__(22,"Fill face selection adjacent ngon faces") ++ GrowStr},
        {?__(15,"Adjacent Faces with Same Number of Edges"),sel_fill_same_number_edge,
         ?__(16,"Fill face selection adjacent faces with same number of edges") ++ GrowStr}
    ].

string_grow(grow) ->
    ?__(1," (grow selection)");
string_grow(_) ->
    "".

%%
%%

-define(FILL_OR_GROW(A), ((A =:= fill) orelse (A =:= grow))).

command({select,{FillOrGrow,sel_fill_smoothing_group}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_smoothing_group(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_crease_angle}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_crease_angle(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_color_range}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_color_range(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_connected_color_edges}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_connected_color_edges(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_unassigned_color}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_unassigned_color(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_unassigned_uv}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_unassigned_uv(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_connected_uv_edges}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_connected_uv_edges(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_same_number_edge}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_same_number_edge(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_adjacent_tri}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_adjacent_tri(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_adjacent_quads}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_adjacent_quads(St, FillOrGrow);
command({select,{FillOrGrow,sel_fill_adjacent_ngon}}, St)
  when ?FILL_OR_GROW(FillOrGrow) ->
    sel_fill_adjacent_ngon(St, FillOrGrow);
command(_, _) ->
    next.


%%
%% Fill smoothing group
%%
sel_fill_smoothing_group(#st{selmode=face}=St, FillOrGrow) ->
    %% Select the rest of the faces enclosed in hard edges
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab,he=HE}=We0) ->
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, HE, We0), Fs0, HE, We0)
        end, St);
sel_fill_smoothing_group(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_smoothing_group(St1, FillOrGrow).

%%
%% Fill in sharp edges by crease angle (fill within edges higher than crease angle)
%%
sel_fill_crease_angle(St, FillOrGrow) ->
    Frame = [{vframe,[
        {hframe,[{label,?__(11,"Angle:")},{text,120.0,[{key,angle}]}]}
        ]}],
    wings_dialog:dialog(?__(1,"Fill Inside Sharp Edges"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,sel_fill_crease_angle_1(Args, St, FillOrGrow)};
            (cancel) ->
                St;
            (Args) ->
                {commit,St,sel_fill_crease_angle_1(Args, St, FillOrGrow)}
        end).
sel_fill_crease_angle_1(Args, #st{selmode=face}=St, FillOrGrow) ->
    Ang = proplists:get_value(angle, Args),
    %% Select the rest of the faces enclosed in sharp edges
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Edges = get_sharp_edges(Ang*math:pi()/360.0, We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_crease_angle_1(Args, #st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_crease_angle_1(Args, St1, FillOrGrow).
get_sharp_edges(Ang_0, #we{fs=Fs0}=We) ->
    Fs = gb_trees:keys(Fs0),
    Edges = wings_edge:from_faces(Fs, We),
    Ns = gb_trees:from_orddict(orddict:from_list([{F,wings_face:normal(F, We)} || F <- Fs])),
    Ang = math:cos(Ang_0),
    Edges1 = gb_sets:fold(fun (E, Acc) -> get_sharp_edges_1(E, Ns, Ang, We, Acc) end, [], Edges),
    gb_sets:from_list(Edges1).
%% Used wings_body:auto_smooth/5 as reference for crease angle.
get_sharp_edges_1(E, Ns, Ang, #we{es=Etab}=_We, Acc) ->
    #edge{lf=F1,rf=F2} = array:get(E, Etab),
    Nrm1 = gb_trees:get(F1,Ns),
    Nrm2 = gb_trees:get(F2,Ns),
    case e3d_vec:is_zero(Nrm1) orelse e3d_vec:is_zero(Nrm2) of
        true ->
            Acc;
        _ ->
            case e3d_vec:dot(Nrm1, Nrm2) of
                DAng when DAng < Ang ->
                    %% Sharp Edge
                    [E|Acc];
                _ ->
                    Acc
            end
    end.


%%
%% Fill Inside Color Range
%%
sel_fill_color_range(St, FillOrGrow) ->
    Frame = [{vframe,[
        {hframe,[{label,?__(11,"Tolerance (%):")},{text,1.0,[{key,tolerance}]}]}
        ]}],
    wings_dialog:dialog(?__(1,"Fill Inside Color Range"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,sel_fill_color_range_1(Args, St, FillOrGrow)};
            (cancel) ->
                St;
            (Args) ->
                {commit,St,sel_fill_color_range_1(Args, St, FillOrGrow)}
        end).
sel_fill_color_range_1(Args, #st{selmode=face}=St, FillOrGrow) ->
    Tolerance = max(?EPSILON, proplists:get_value(tolerance, Args) / 100.0),
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            SelColors = get_colors(Fs0, We0),
            Edges = same_color_edges(SelColors, Tolerance, We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_color_range_1(Args, #st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_color_range_1(Args, St1, FillOrGrow).

get_colors(Fs0, We) ->
    gb_sets:fold(
        fun (F, Acc) ->
            case color_avg([C || C <- wings_va:face_attr(color, F, We), C =/= none]) of
                none -> Acc;
                Color ->
                    case lists:any(fun (C) -> same_color(C, Color) end, Acc) of
                        true -> Acc;
                        false -> [Color|Acc]
                    end
            end
        end, [], Fs0).

color_avg([]) ->
    none;
color_avg(List) ->
    Len = length(List),
    R1 = [element(1,C) || C <- List],
    G1 = [element(2,C) || C <- List],
    B1 = [element(3,C) || C <- List],
    {lists:sum(R1) / Len, lists:sum(G1) / Len, lists:sum(B1) / Len}.

same_color(Col1, Col2) when is_tuple(Col1), is_tuple(Col2) ->
    same_color(Col1, Col2, ?EPSILON).
same_color(Col1, Col2, Tolerance) when is_tuple(Col1), is_tuple(Col2) ->
    R1 = element(1, Col1),
    G1 = element(2, Col1),
    B1 = element(3, Col1),
    R2 = element(1, Col2),
    G2 = element(2, Col2),
    B2 = element(3, Col2),
    (abs(R1 - R2) < Tolerance) andalso
    (abs(G1 - G2) < Tolerance) andalso
    (abs(B1 - B2) < Tolerance);
same_color(_, _, _) ->
    false.

same_color_edges(SelColors, Tolerance, #we{fs=Fs0,vp=_Vtab}=We) ->
    Fs = gb_trees:keys(Fs0),
    Edges = wings_edge:from_faces(Fs, We),
    DissimilarBoundaries = gb_sets:fold(
        fun (E, Acc) ->
            Col1 = wings_va:attr(color, wings_va:edge_attrs(E, left, 0.5, We)),
            Col2 = wings_va:attr(color, wings_va:edge_attrs(E, right, 0.5, We)),
            case same_color_edges_1(Col1, Tolerance, SelColors) andalso
                 same_color_edges_1(Col2, Tolerance, SelColors)
            of
                true -> Acc;
                _ -> [E|Acc]
            end
        end, [], Edges),
    gb_sets:from_list(DissimilarBoundaries).
same_color_edges_1(Color, Tolerance, SelColors) ->
    case lists:any(fun (C) -> same_color(C, Color, Tolerance) end, SelColors) of
        true -> true;
        false -> false
    end.


%%
%% Fill Inside Connected Color Edges
%%
sel_fill_connected_color_edges(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Edges = get_edges_va_connected(color,We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_connected_color_edges(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_connected_color_edges(St1, FillOrGrow).

%%
%% Fill Inside Unassigned Color
%%
sel_fill_unassigned_color(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Fs1 = faces_va_assigned(Fs0, color, We0),
            Edges = get_edges_va_assigned(color,We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs1, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_unassigned_color(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_unassigned_color(St1, FillOrGrow).


%%
%% Fill Unassigned UV
%%
sel_fill_unassigned_uv(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Fs1 = faces_va_assigned(Fs0, uv, We0),
            Edges = get_edges_va_assigned(uv,We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs1, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_unassigned_uv(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_unassigned_uv(St1, FillOrGrow).

%%
%% Fill Connected UV edges
%%
sel_fill_connected_uv_edges(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Edges = get_edges_va_connected(uv,We0),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_connected_uv_edges(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_connected_uv_edges(St1, FillOrGrow).


get_edges_va_connected(What,#we{fs=Fs0,vp=_Vtab,es=Es}=We) ->
    Fs = gb_trees:keys(Fs0),
    Edges = wings_edge:from_faces(Fs, We),
    DissimilarBoundaries = gb_sets:fold(
        fun (E, Acc) ->
            #edge{lf=F1,rf=F2} = array:get(E, Es),
            F1C = wings_va:attr(What, wings_va:edge_attrs(E, F1, We)),
            F2C = wings_va:attr(What, wings_va:edge_attrs(E, F2, We)),
            if
                F1C =/= F2C -> [E|Acc];
                true -> Acc
            end
        end, [], Edges),
    gb_sets:from_list(DissimilarBoundaries).


faces_va_assigned(Fs0, What, We) ->
    gb_sets:from_list(gb_sets:fold(
        fun (F, Acc) ->
            case lists:all(fun (V) -> V =:= none end, wings_va:face_attr(What, F, We)) of
                true -> [F|Acc];
                false -> Acc
            end
        end, [], Fs0)).

get_edges_va_assigned(What,#we{fs=Fs0,vp=_Vtab,es=Es}=We) ->
    Fs = gb_trees:keys(Fs0),
    Edges = wings_edge:from_faces(Fs, We),
    ColorAttr_0 = [{F,lists:all(fun (V) -> V =:= none end, wings_va:face_attr(What, F, We))} || F <- Fs],
    ColorAttr = gb_trees:from_orddict(orddict:from_list(ColorAttr_0)),
    DissimilarBoundaries = gb_sets:fold(
        fun (E, Acc) ->
            #edge{lf=F1,rf=F2} = array:get(E, Es),
            F1C = gb_trees:get(F1, ColorAttr),
            F2C = gb_trees:get(F2, ColorAttr),
            if
                F1C =/= F2C -> [E|Acc];
                true -> Acc
            end
        end, [], Edges),
    gb_sets:from_list(DissimilarBoundaries).


%%
%% Same Number of Edges
%%
sel_fill_same_number_edge(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Edges = get_edges_face_edge_number(We0, false),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs0, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_same_number_edge(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_same_number_edge(St1, FillOrGrow).


%%
%% Fill Adjacent Triangles
%%
sel_fill_adjacent_tri(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Fs1 = faces_edge_number(Fs0, fun (Number) -> Number =:= 3 end, We0),
            Edges = get_edges_face_edge_number(We0, false),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs1, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_adjacent_tri(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_adjacent_tri(St1, FillOrGrow).


%%
%% Fill Adjacent Quads
%%
sel_fill_adjacent_quads(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Fs1 = faces_edge_number(Fs0, fun (Number) -> Number =:= 4 end, We0),
            Edges = get_edges_face_edge_number(We0, false),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs1, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_adjacent_quads(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_adjacent_quads(St1, FillOrGrow).

%%
%% Fill Adjacent NGons
%%
sel_fill_adjacent_ngon(#st{selmode=face}=St, FillOrGrow) ->
    wings_sel:update_sel(
        fun (Fs0, #we{vp=_Vtab}=We0) ->
            Fs1 = faces_edge_number(Fs0, fun (Number) -> Number >= 5 end, We0),
            Edges = get_edges_face_edge_number(We0, true),
            fill_method(FillOrGrow, wings_edge:reachable_faces(Fs1, Edges, We0), Fs0, Edges, We0)
        end, St);
sel_fill_adjacent_ngon(#st{selmode=_Mode}=St0, FillOrGrow) ->
    St1=wings_sel_conv:mode(face, St0),
    sel_fill_adjacent_ngon(St1, FillOrGrow).

faces_edge_number(Fs0, Fun, We) ->
    gb_sets:from_list(gb_sets:fold(
        fun (F, Acc) ->
            case Fun(length(wings_face:to_edges([F], We))) of
                true -> [F|Acc];
                false -> Acc
            end
        end, [], Fs0)).

get_edges_face_edge_number(#we{fs=Fs0,vp=_Vtab,es=Es}=We, Ngons) ->
    Fs = gb_trees:keys(Fs0),
    Edges = wings_edge:from_faces(Fs, We),
    EdgeNum_0 = [{F,length(wings_face:to_edges([F], We))} || F <- Fs],
    EdgeNum = gb_trees:from_orddict(orddict:from_list(EdgeNum_0)),
    DissimilarBoundaries = gb_sets:fold(
        fun (E, Acc) ->
            #edge{lf=F1,rf=F2} = array:get(E, Es),
            F1C = gb_trees:get(F1, EdgeNum),
            F2C = gb_trees:get(F2, EdgeNum),
            if
                F1C > 4, F2C > 4, Ngons =:= true, F1C =/= F2C -> Acc;
                F1C =/= F2C -> [E|Acc];
                true -> Acc
            end
        end, [], Edges),
    gb_sets:from_list(DissimilarBoundaries).


%% Fill method
%%
fill_method(grow, Fs1, Fs0, Edges, We) ->
    Regions = wings_sel:face_regions(Fs0, We),
    lists:foldl(
        fun (Fs0_0, Acc) ->
            Fs0_1 = gb_sets:from_list(wings_face:from_vs(wings_vertex:from_faces(Fs0_0, We), We)),
            Fs0_2 = gb_sets:intersection(Fs1, wings_edge:reachable_faces(Fs0_0, Edges, We)),
            Fs2 = gb_sets:intersection(Fs0_2, Fs0_1),
            gb_sets:union(Fs2, Acc)
        end, gb_sets:empty(), Regions);
fill_method(_, Fs1, _Fs0, _Edges, _We) ->
    Fs1.


