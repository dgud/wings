%%
%%  auv_pack_utils.erl
%%
%%      Shared functions for the UV island packing algorithms
%%      (auv_pack_maxrects and auv_pack_poly).
%%
%%  Copyright (c) 2026 Micheus Vieira (micheus)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Option validation, rotation candidates, convex hull and minimum-area OBB,
%% chart bounds, chart transforms, outline extraction and critical charts.

-module(auv_pack_utils).

-include_lib("wings/src/wings.hrl").

-export([validate_options/1,
         rotation_angles/2, rotated_bounds/3, unique_orientations/1,
         convex_hull_2d/1, min_area_obb_2d/1,
         chart_obb/1, chart_bbox/1,
         is_movable/2, critical_unpacked/4,
         transform_chart/3, place_chart/3,
         extract_polygons/1, fit_scale/4]).

-define(EPS, 1.0e-9).
-define(SCALE_MIN, 0.25).
-define(SCALE_MAX, 4.0).
-define(SCALE_TRIES, 6).

%%%
%%% Options
%%%

%% -> {Gap, Margin, RotationStep, Profile, Normalize, Origin} | {error, invalid_options}
%% Gap and Margin come back as fractions of the tile side (input is percent).
validate_options(Options) ->
    Gap = maps:get(gap, Options, 0),
    Margin = maps:get(margin, Options, 0),
    RotationStep = maps:get(rotation_step, Options, 0),
    Profile = maps:get(profile, Options, balanced),
    Normalize = maps:get(normalize, Options, false),
    Origin = maps:get(origin, Options, {+0.0, +0.0}),
    ValidateAll = valid_percent(Gap) andalso valid_percent(Margin) andalso
         valid_rotation_step(RotationStep) andalso valid_profile(Profile) andalso
         is_boolean(Normalize) andalso valid_origin(Origin),
    case ValidateAll of
        true ->
            {Gap/100.0, Margin/100.0, RotationStep, Profile, Normalize,
             Origin};
        false ->
            {error, invalid_options}
    end.

valid_rotation_step(0) -> true;
valid_rotation_step(7) -> true;
valid_rotation_step(15) -> true;
valid_rotation_step(30) -> true;
valid_rotation_step(45) -> true;
valid_rotation_step(90) -> true;
valid_rotation_step(_) -> false.

valid_profile(fast) -> true;
valid_profile(balanced) -> true;
valid_profile(quality) -> true;
valid_profile(max) -> true;
valid_profile(_) -> false.

valid_percent(Value) when is_number(Value), Value >= 0 ->
    true;
valid_percent(_) ->
    false.

valid_origin({U, V}) ->
    is_number(U) andalso is_number(V);
valid_origin(_) ->
    false.

%%%
%%% Rotation candidates
%%%

%% Angles in [0,180) every Step degrees, reduced to at most MaxSearch evenly
%% spaced values.
rotation_angles(0, _MaxSearch) ->
    [0.0];
rotation_angles(Step, MaxSearch) ->
    All = [float(A) || A <- lists:seq(0, 179, Step)],
    Count = length(All),
    case Count =< MaxSearch of
        true ->
            All;
        false when MaxSearch =< 1 ->
            [hd(All)];
        false ->
            Indices =
                lists:usort(
                  [1 + round(I * (Count - 1) / (MaxSearch - 1))
                   || I <- lists:seq(0, MaxSearch - 1)]),
            [lists:nth(I, All) || I <- Indices]
    end.

%% Bounding box of a W x H rectangle rotated Angle degrees.
rotated_bounds(W, H, +0.0) ->
    {W, H};
rotated_bounds(W, H, Angle) ->
    Rad = Angle * math:pi() / 180.0,
    C = abs(math:cos(Rad)),
    S = abs(math:sin(Rad)),
    {C * W + S * H, S * W + C * H}.

%% One candidate {Angle, ...} per angle; the first one wins.
unique_orientations(Candidates) ->
    lists:ukeysort(1, Candidates).

%%%
%%% Convex hull and OBB
%%%

%% 2D convex hull (monotone chain) of {X,Y} or {X,Y,Z} points, in CCW order.
convex_hull_2d(Points) ->
    Sorted = lists:usort([xy(P) || P <- Points]),
    case Sorted of
        [] ->
            [];
        [_] ->
            Sorted;
        _ ->
            Lower = hull_chain(Sorted),
            Upper = hull_chain(lists:reverse(Sorted)),
            lists:droplast(Lower) ++ lists:droplast(Upper)
    end.

xy({X, Y}) -> {X, Y};
xy({X, Y, _}) -> {X, Y}.

hull_chain(Points) ->
    Lp = lists:foldl(fun(Point, Acc) ->
                        hull_chain_add(Point, Acc)
                    end, [], Points),
    lists:reverse(Lp).

hull_chain_add(Point, [B, A | Rest] = Acc) ->
    case cross_2d(A, B, Point) =< 0.0 of
        true ->
            hull_chain_add(Point, [A | Rest]);
        false ->
            [Point | Acc]
    end;
hull_chain_add(Point, Acc) ->
    [Point | Acc].

cross_2d({X1, Y1}, {X2, Y2}, {X3, Y3}) ->
    (X2 - X1) * (Y3 - Y1) - (Y2 - Y1) * (X3 - X1).

%% Minimum-area oriented bounding box -> {Angle, Area}: Angle is the hull edge
%% direction in degrees; rotate the chart by -Angle to align the OBB.
min_area_obb_2d(Hull) when length(Hull) < 2 ->
    {0.0, 0.0};
min_area_obb_2d(Hull) ->
    Edges = hull_edges(Hull),
    {Angle, Area} =
        lists:foldl(
          fun({A, B}, {BestAngle, BestArea}) ->
              {DX, DY} = sub_2d(B, A),
              AngleRad = math:atan2(DY, DX),
              {{MinX, MinY}, {MaxX, MaxY}} =
                  project_hull(Hull, AngleRad),
              Area0 = (MaxX - MinX) * (MaxY - MinY),
              case Area0 < BestArea of
                  true ->
                      {AngleRad, Area0};
                  false ->
                      {BestAngle, BestArea}
              end
          end,
          {0.0, 1.0e308},
          Edges),
    {Angle * 180.0 / math:pi(), Area}.

hull_edges([_]) ->
    [];
hull_edges(Hull) ->
    lists:zip(Hull, tl(Hull) ++ [hd(Hull)]).

%% Bounds of the hull in the system rotated by -Angle (radians).
project_hull(Hull, Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    Rotated = [{X * Cos + Y * Sin, -X * Sin + Y * Cos}
               || {X, Y} <- Hull],
    Xs = [X || {X, _Y} <- Rotated],
    Ys = [Y || {_X, Y} <- Rotated],
    {{lists:min(Xs), lists:min(Ys)},
     {lists:max(Xs), lists:max(Ys)}}.

sub_2d({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

%% -> none | {Rotation, W, H}: rotation (degrees) that aligns the chart's
%% minimum-area OBB with the axes, and the OBB size.
chart_obb(#we{vp=Vs}) ->
    Points = [{X, Y} || {X, Y, _} <- array:sparse_to_list(Vs)],
    case length(lists:usort(Points)) of
        N when N < 3 ->
            none;
        _ ->
            Hull = convex_hull_2d(Points),
            {Angle, _Area} = min_area_obb_2d(Hull),
            {{MinX, MinY}, {MaxX, MaxY}} =
                project_hull(Hull, Angle * math:pi() / 180.0),
            W = MaxX - MinX,
            H = MaxY - MinY,
            case W < ?EPS orelse H < ?EPS of
                true -> none;
                false -> {-Angle, W, H}
            end
    end.

%%%
%%% Chart bounds
%%%

%% {X0, Y0, W, H} of the chart, or 'degenerate' (no vertices or no extent).
chart_bbox(#we{vp=Vs}) ->
    case array:sparse_to_list(Vs) of
        [] ->
            degenerate;
        Positions ->
            {{X0,Y0,_}, {X1,Y1,_}} = e3d_bv:box(Positions),
            W = X1 - X0,
            H = Y1 - Y0,
            case W < ?EPS orelse H < ?EPS of
                true -> degenerate;
                false -> {X0, Y0, W, H}
            end
    end.

chart_bounds(#we{vp=Vs}) ->
    {{X0,Y0,_}, {X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
    {X0,Y0,X1-X0,Y1-Y0}.

bounds_overlap({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    X1 < (X2 + W2) andalso (X1 + W1) > X2 andalso
    Y1 < (Y2 + H2) andalso (Y1 + H1) > Y2.

is_movable(_Id, all) ->
    true;
is_movable(Id, Ids) when is_list(Ids) ->
    lists:member(Id, Ids).

%% Unpacked charts whose bbox still overlaps an obstacle (fixed or placed).
critical_unpacked(_Charts, _Placed, [], _FixedIds) ->
    [];
critical_unpacked(Charts, Placed, Unpacked, FixedIds) ->
    FixedSet = gb_sets:from_list(FixedIds),
    Candidates = [Id || Id <- Unpacked,
                        not gb_sets:is_member(Id, FixedSet)],
    FixedBounds = [chart_bounds(We)
                   || We <- Charts,
                      gb_sets:is_member(We#we.id, FixedSet)],
    PlacedBounds = [Bounds || {_Id, We} <- Placed,
                              Bounds <- [chart_bounds(We)]],
    ObstacleBounds = FixedBounds ++ PlacedBounds,
    [Id || Id <- Candidates,
           case lists:keyfind(Id, #we.id, Charts) of
               false ->
                   false;
               We ->
                   Bounds = chart_bounds(We),
                   lists:any(fun(Other) ->
                                     bounds_overlap(Bounds, Other)
                             end, ObstacleBounds)
           end].

%%%
%%% Chart transforms
%%%

%% Rotates Angle degrees (counterclockwise) around the bbox center.
rotate_chart(#we{vp=Vs}=We, Angle) ->
    case abs(Angle) < ?EPS of
        true ->
            We;
        false ->
            {{X0,Y0,_},{X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
            C = {(X0 + X1) / 2.0, (Y0 + Y1) / 2.0, +0.0},
            ToOrigin = e3d_mat:translate(e3d_vec:neg(C)),
            Rotate = e3d_mat:rotate(float(Angle), {+0.0,+0.0,1.0}),
            M = e3d_mat:mul(e3d_mat:translate(C),
                            e3d_mat:mul(Rotate, ToOrigin)),
            wings_we:transform_vs(M, We)
    end.

%% Rotates around the bbox center, then translates by {Dx,Dy}.
transform_chart(We, Angle, {Dx, Dy}) ->
    We1 = rotate_chart(We, Angle),
    wings_we:transform_vs(e3d_mat:translate({Dx, Dy, +0.0}), We1).

%% Rotates, then moves the center of the rotated bbox to the center of {X,Y,W,H}.
place_chart(We, {X, Y, W, H}, Angle) ->
    #we{vp=Vs} = We1 = rotate_chart(We, Angle),
    {{X0,Y0,_},{X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
    Dx = X + W / 2.0 - (X0 + X1) / 2.0,
    Dy = Y + H / 2.0 - (Y0 + Y1) / 2.0,
    wings_we:transform_vs(e3d_mat:translate({Dx, Dy, +0.0}), We1).

%%%
%%% Scale to fit
%%%

%% fit_scale(PackFun, Charts, Movable, Options) -> PackFun's result
%% With option scale => true and every chart movable, searches the largest
%% uniform scale (SCALE_MIN..SCALE_MAX) at which all charts still fit, calling
%% PackFun(Charts) with scaled copies of the charts. The result is the one of
%% the best scale, with 'scale' added; when nothing fits it is the unscaled one.
fit_scale(PackFun, Charts, Movable, Options) ->
    Run = case maps:get(scale, Options, false) andalso
              all_movable(Charts, Movable) of
              true -> validate_options(Options);
              false -> none
          end,
    case Run of
        {_Gap, Margin, _Step, _Profile, _Normalize, {_U, V}} ->
            scale_search(PackFun, Charts, 1.0 - 2.0 * Margin, V + Margin);
        _ ->
            PackFun(Charts)
    end.

all_movable(_Charts, all) ->
    true;
all_movable(Charts, Ids) ->
    lists:all(fun(#we{id=Id}) -> lists:member(Id, Ids) end, Charts).

scale_search(PackFun, Charts, Side, Y0) ->
    Run = fun(S) -> scaled_run(PackFun, Charts, S, Y0) end,
    case Run(1.0) of
        {error, _}=Error ->
            Error;
        {_, Fits, _, _, _}=Base ->
            {Lo, Hi} = case Fits of
                           true -> {Base, none};
                           false -> {none, Base}
                       end,
            scale_loop(Run, Side, Base, Lo, Hi, ?SCALE_TRIES)
    end.

%% -> {Scale, Fits, Result, UsedHeight, PackedFraction} | {error, Reason}
scaled_run(PackFun, Charts, S, Y0) ->
    Cs = case S =:= 1.0 of
             true -> Charts;
             false -> [scale_chart(We, S) || We <- Charts]
         end,
    case PackFun(Cs) of
        {error, _}=Error ->
            Error;
        #{placed := Placed, unpacked := Unpacked, degenerate := Degenerate,
          stats := Stats}=R ->
            Top = lists:max([Y0 | [Y + H || {_, We} <- Placed,
                                            {_, Y, _, H} <- [chart_bbox(We)]]]),
            Total = maps:get(movable_area, Stats),
            Frac = case Total > ?EPS of
                       true -> maps:get(packed_area, Stats) / Total;
                       false -> 1.0
                   end,
            {S, Unpacked -- Degenerate =:= [], R#{scale => S}, Top - Y0, Frac}
    end.

scale_loop(_Run, _Side, Base, Lo, _Hi, 0) ->
    scale_result(Lo, Base);
scale_loop(Run, Side, Base, Lo, Hi, Tries) ->
    case next_scale(Lo, Hi, Side) of
        stop ->
            scale_result(Lo, Base);
        S ->
            case Run(S) of
                {error, _}=Error ->
                    Error;
                {_, true, _, _, _}=Res ->
                    scale_loop(Run, Side, Base, Res, Hi, Tries - 1);
                {_, false, _, _, _}=Res ->
                    scale_loop(Run, Side, Base, Lo, Res, Tries - 1)
            end
    end.

next_scale(none, {Sh, _, _, _, Frac}, _Side) ->
    case Sh =< ?SCALE_MIN + ?EPS of
        true ->
            stop;
        false ->
            C = max(?SCALE_MIN, Sh * math:sqrt(Frac) * 0.95),
            case C >= Sh * 0.99 of
                true -> max(?SCALE_MIN, Sh * 0.9);
                false -> C
            end
    end;
next_scale({Sl, _, _, Used, _}, none, Side) ->
    case Used > ?EPS of
        false ->
            stop;
        true ->
            C = min(?SCALE_MAX, Sl * math:sqrt(Side / Used) * 0.99),
            case C < Sl * 1.01 of
                true -> stop;
                false -> C
            end
    end;
next_scale({Sl, _, _, _, _}, {Sh, _, _, _, _}, _Side) ->
    case Sh / Sl < 1.02 of
        true -> stop;
        false -> math:sqrt(Sl * Sh)
    end.

scale_result(none, {_, _, R, _, _}) -> R;
scale_result({_, _, R, _, _}, _Base) -> R.

%% Uniform scale around the bbox center.
scale_chart(#we{vp=Vs}=We, S) ->
    {{X0,Y0,_},{X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
    C = {(X0 + X1) / 2.0, (Y0 + Y1) / 2.0, +0.0},
    M = e3d_mat:mul(e3d_mat:translate(C),
                    e3d_mat:mul(e3d_mat:scale(S, S, 1.0),
                                e3d_mat:translate(e3d_vec:neg(C)))),
    wings_we:transform_vs(M, We).

%%%
%%% Outline extraction
%%%

%% -> [{Id, none | {{Perim, Vs}, [{Perim, Vs}]}}]
%% The loop with the largest area is the outline, the others are holes;
%% 'none' for charts without hole faces.
extract_polygons(WeList) ->
    [extract_chart(We) || We <- WeList].

extract_chart(#we{id=Id, fs=Ftab, vp=Vtab}=We) ->
    Loops = case [F || F <- gb_trees:keys(Ftab), F < 0] of
                [] -> [];
                HoleFs ->
                    Edges = wings_face:outer_edges(HoleFs, We),
                    wings_edge_loop:edge_loop_vertices(Edges, We)
            end,
    Items = [{loop_area(L, Vtab), loop_length(L, Vtab), L}
             || L <- Loops, length(L) >= 3],
    case lists:reverse(lists:keysort(1, Items)) of
        [] ->
            {Id, none};
        [{_, Len, Outer} | Inner] ->
            {Id, {{Len, Outer}, [{PL, L} || {_, PL, L} <- Inner]}}
    end.

%% Closed pairs of consecutive vertices.
loop_pairs(L) -> lists:zip(L, tl(L) ++ [hd(L)]).

loop_length(L, Vtab) ->
    lists:sum([e3d_vec:dist(array:get(A, Vtab), array:get(B, Vtab))
               || {A, B} <- loop_pairs(L)]).

loop_area(L, Vtab) ->
    S = lists:sum([begin
                       {X1, Y1, _} = array:get(A, Vtab),
                       {X2, Y2, _} = array:get(B, Vtab),
                       X1 * Y2 - X2 * Y1
                   end || {A, B} <- loop_pairs(L)]),
    abs(S) / 2.0.
