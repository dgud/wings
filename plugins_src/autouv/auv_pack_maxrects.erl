%%
%%  auv_pack_maxrects.erl
%%
%%      UV island packing core (MaxRects, Best Short Side Fit).
%%
%%  Copyright (c) 2026 Micheus Vieira (micheus)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Packs the charts of one tile by their bounding boxes (MaxRects, Best
%% Short Side Fit). pack_charts/3 is the adapter used by wpc_autouv; options
%% are validated by auv_pack_utils:validate_options/1 (gap and margin are
%% percent of the tile side, the tile has its own origin).
%%
%% pack_tile/2 gets [#{id, bounds := {X, Y, W, H}, fixed => boolean(), obb =>
%% {Angle, W, H}}] (fixed charts are obstacles) and returns
%% #{placements := [#{id, bounds, rotation}], unpacked, stats} | {error, Reason}.
%% A placement is applied with auv_pack_utils:place_chart/3.

-module(auv_pack_maxrects).

-include_lib("wings/src/wings.hrl").

-export([pack_charts/3]).

-define(EPS, 1.0e-9).

-record(rect, {x, y, w, h, obb = none}).

%%%
%%% Packing
%%%

pack_tile(Charts, Options) when is_list(Charts), is_map(Options) ->
    case auv_pack_utils:validate_options(Options) of
        {Gap, Margin, RotationStep, Profile, _Normalize, Origin} ->
            case validate_charts(Charts) of
                {error, _}=Error ->
                    Error;
                {Fixed, Movable} ->
                    pack_validated(Fixed, Movable, Gap, Margin,
                                   RotationStep, Profile, Origin)
            end;
        Error ->
            Error
    end;
pack_tile(_, _) ->
    {error, invalid_input}.

%%%
%%% Validation
%%%

valid_bounds({X, Y, W, H}) ->
    is_number(X) andalso is_number(Y) andalso
    is_number(W) andalso is_number(H) andalso W > 0 andalso H > 0;
valid_bounds(_) ->
    false.

validate_charts(Charts) ->
    validate_charts(Charts, #{}, [], []).

validate_charts([], _Seen, Fixed, Movable) ->
    {lists:reverse(Fixed), lists:reverse(Movable)};
validate_charts([#{id := Id, bounds := Bounds} = Chart | Rest],
                Seen, Fixed, Movable) ->
    IsFixed = maps:get(fixed, Chart, false),
    OBB = maps:get(obb, Chart, none),
    case {is_boolean(IsFixed), maps:is_key(Id, Seen),
          valid_bounds(Bounds), valid_obb(OBB)} of
        {true, false, true, true} ->
            {X, Y, W, H} = Bounds,
            Item = {Id, #rect{x = X, y = Y, w = W, h = H,
                              obb = OBB}},
            Seen1 = Seen#{Id => true},
            case IsFixed of
                true ->
                    validate_charts(Rest, Seen1, [Item | Fixed], Movable);
                false ->
                    validate_charts(Rest, Seen1, Fixed, [Item | Movable])
            end;
        {false, _, _, _} ->
            {error, invalid_chart};
        {_, true, _, _} ->
            {error, duplicate_id};
        {_, _, false, _} ->
            {error, invalid_chart_bounds};
        {_, _, _, false} ->
            {error, invalid_chart}
    end;
validate_charts(_, _Seen, _Fixed, _Movable) ->
    {error, invalid_chart}.

valid_obb(none) ->
    true;
valid_obb({Angle, W, H}) ->
    is_number(Angle) andalso is_number(W) andalso is_number(H) andalso
    W > 0 andalso H > 0;
valid_obb(_) ->
    false.

%%%
%%% Placement
%%%

pack_validated(Fixed, Movable, Gap, Margin, RotationStep, Profile,
               {OU, OV}) ->
    HalfGap = Gap / 2.0,
    Side = 1.0 - 2.0 * Margin,
    case Side > ?EPS of
        false ->
            {error, margin_exceeds_tile};
        true ->
            Area = #rect{x = OU + Margin - HalfGap,
                         y = OV + Margin - HalfGap,
                         w = Side + Gap,
                         h = Side + Gap},
            Free = lists:foldl(
                     fun({_Id, Rect}, Acc) ->
                             prune_free(
                               subtract_from_free(Acc, expand(Rect, HalfGap)))
                     end,
                     [Area],
                     Fixed),
            {Placements, Unpacked} =
                place_all(sort_movable(Movable), Free, Gap, HalfGap,
                          RotationStep, Profile, [], []),
            make_result(Placements, Unpacked, Movable, Fixed)
    end.

%% Largest area first; the id makes the order deterministic.
sort_movable(Movable) ->
    Keyed = [{-(R#rect.w * R#rect.h), Id, R} || {Id, R} <- Movable],
    [{Id, R} || {_Key, Id, R} <- lists:sort(Keyed)].

%% Rotation candidates evaluated per chart.
profile_params(fast) ->
    #{max_search => 1};
profile_params(balanced) ->
    #{max_search => 4};
profile_params(quality) ->
    #{max_search => 8};
profile_params(max) ->
    #{max_search => 16}.

place_all([], _Free, _Gap, _HalfGap, _RotationStep, _Profile,
          Placements, Unpacked) ->
    {lists:reverse(Placements), lists:reverse(Unpacked)};
place_all([{Id, Rect} | Rest], Free, Gap, HalfGap,
          RotationStep, Profile, Placements, Unpacked) ->
    #{max_search := MaxSearch} = profile_params(Profile),
    case best_position(Rect, Free, Gap, RotationStep, MaxSearch) of
        none ->
            %% Stays in place and keeps its box reserved.
            Original = expand(Rect, HalfGap),
            NewFree = prune_free(subtract_from_free(Free, Original)),
            place_all(Rest, NewFree, Gap, HalfGap, RotationStep, Profile,
                      Placements, [Id | Unpacked]);
        {X, Y, ReservedW, ReservedH, Angle} ->
            Used = #rect{x = X, y = Y, w = ReservedW, h = ReservedH},
            Placement = #{id => Id,
                          bounds => {X + HalfGap, Y + HalfGap,
                                     max(0.0, ReservedW - Gap),
                                     max(0.0, ReservedH - Gap)},
                          rotation => Angle},
            NewFree = prune_free(subtract_from_free(Free, Used)),
            place_all(Rest, NewFree, Gap, HalfGap, RotationStep, Profile,
                      [Placement | Placements], Unpacked)
    end.

best_position(Rect, Free, Gap, RotationStep, MaxSearch) ->
    RegularCandidates =
        [{Angle, W, H}
         || Angle <- auv_pack_utils:rotation_angles(RotationStep, MaxSearch),
            {W, H} <- [auv_pack_utils:rotated_bounds(Rect#rect.w,
                                                     Rect#rect.h, Angle)]],
    %% The OBB goes first: ukeysort keeps the first tuple of each angle.
    Candidates0 =
        case Rect#rect.obb of
            none ->
                RegularCandidates;
            {OBBAngle, OBBW, OBBH} ->
                [{OBBAngle, OBBW, OBBH} | RegularCandidates]
        end,
    Candidates =
        lists:append(
          [candidate_positions(W + Gap, H + Gap, Free, Angle)
           || {Angle, W, H}
                  <- auv_pack_utils:unique_orientations(Candidates0)]),
    case Candidates of
        [] ->
            none;
        _ ->
            unpack_candidate(lists:min(Candidates))
    end.

%% Best Short Side Fit: smallest short-side leftover; then long side, y, x, angle.
candidate_positions(W, H, Free, Angle) ->
    lists:filtermap(
      fun(F) ->
              case W =< F#rect.w + ?EPS andalso H =< F#rect.h + ?EPS of
                  true ->
                      LeftoverW = abs(F#rect.w - W),
                      LeftoverH = abs(F#rect.h - H),
                      Score = {min(LeftoverW, LeftoverH),
                               max(LeftoverW, LeftoverH),
                               F#rect.y, F#rect.x, Angle},
                      {true, {Score, F#rect.x, F#rect.y, W, H, Angle}};
                  false ->
                      false
              end
      end,
      Free).

unpack_candidate({_Score, X, Y, W, H, Angle}) ->
    {X, Y, W, H, Angle}.

%%%
%%% Free rectangle management
%%%

subtract_from_free(Free, Used) ->
    lists:append([split_free(F, Used) || F <- Free]).

%% Maximal-rectangles split; the four resulting rectangles overlap on purpose.
split_free(Free, Used) ->
    case intersects(Free, Used) of
        false ->
            [Free];
        true ->
            X0 = Free#rect.x,
            Y0 = Free#rect.y,
            X1 = X0 + Free#rect.w,
            Y1 = Y0 + Free#rect.h,

            IX0 = max(X0, Used#rect.x),
            IY0 = max(Y0, Used#rect.y),
            IX1 = min(X1, Used#rect.x + Used#rect.w),
            IY1 = min(Y1, Used#rect.y + Used#rect.h),

            valid_free_rects([
                #rect{x = X0, y = Y0,
                      w = Free#rect.w, h = IY0 - Y0},
                #rect{x = X0, y = IY1,
                      w = Free#rect.w, h = Y1 - IY1},
                #rect{x = X0, y = IY0,
                      w = IX0 - X0, h = IY1 - IY0},
                #rect{x = IX1, y = IY0,
                      w = X1 - IX1, h = IY1 - IY0}
            ])
    end.

valid_free_rects(Rects) ->
    [R || R <- Rects, R#rect.w > ?EPS, R#rect.h > ?EPS].

%% Drops rectangles contained in another one.
prune_free(Free) ->
    prune_free(Free, []).

prune_free([], Acc) ->
    lists:reverse(Acc);
prune_free([Rect | Rest], Acc) ->
    case lists:any(fun(Other) -> contains(Other, Rect) end, Rest ++ Acc) of
        true -> prune_free(Rest, Acc);
        false -> prune_free(Rest, [Rect | Acc])
    end.

%%%
%%% Geometry helpers
%%%

expand(Rect, Amount) ->
    #rect{x = Rect#rect.x - Amount,
          y = Rect#rect.y - Amount,
          w = Rect#rect.w + 2.0 * Amount,
          h = Rect#rect.h + 2.0 * Amount}.

contains(Outer, Inner) ->
    Inner#rect.x >= Outer#rect.x - ?EPS andalso
    Inner#rect.y >= Outer#rect.y - ?EPS andalso
    Inner#rect.x + Inner#rect.w =< Outer#rect.x + Outer#rect.w + ?EPS andalso
    Inner#rect.y + Inner#rect.h =< Outer#rect.y + Outer#rect.h + ?EPS.

%% Touching edges do not count.
intersects(A, B) ->
    A#rect.x < B#rect.x + B#rect.w - ?EPS andalso
    A#rect.x + A#rect.w > B#rect.x + ?EPS andalso
    A#rect.y < B#rect.y + B#rect.h - ?EPS andalso
    A#rect.y + A#rect.h > B#rect.y + ?EPS.

%%%
%%% Result
%%%

make_result(Placements, Unpacked, Movable, Fixed) ->
    MovableArea = lists:sum([R#rect.w * R#rect.h || {_Id, R} <- Movable]),
    PackedArea = lists:sum(
                   [begin
                        {_X, _Y, W, H} = maps:get(bounds, Placement),
                        W * H
                    end || Placement <- Placements]),
    #{placements => Placements,
      unpacked => Unpacked,
      stats => #{movable => length(Movable),
                 fixed => length(Fixed),
                 packed => length(Placements),
                 unpacked => length(Unpacked),
                 movable_area => MovableArea,
                 packed_area => PackedArea}}.

%%%
%%%  Wings3D adapter
%%%

%% pack_charts(Charts, Movable, Options) -> Result | {error, Reason}
%% Charts: #we{} of one tile; Movable: all | [Id] (the others are obstacles).
%% Result = #{placed := [{Id, #we{}}], unpacked := [Id], degenerate := [Id],
%%           stats := #{}}; degenerate charts are also listed in unpacked.
%% With normalize, each movable chart gets its OBB as a rotation candidate;
%% with scale, charts are enlarged or reduced to fill the tile.
pack_charts(Charts, Movable, Options) ->
    auv_pack_utils:fit_scale(
      fun(Cs) -> pack_charts_1(Cs, Movable, Options) end,
      Charts, Movable, Options).

pack_charts_1(Charts, Movable, Options) ->
    Normalize = maps:get(normalize, Options, false),
    {Descs, Degenerate} =
        chart_descs(Charts, Movable, Normalize, [], []),
    case pack_tile(Descs, Options) of
        {error, _}=Error ->
            Error;
        #{placements := Placements,
          unpacked := Unpacked0,
          stats := Stats0} ->
            Wes = maps:from_list([{We#we.id, We} || We <- Charts]),
            Placed =
                [{Id, auv_pack_utils:place_chart(maps:get(Id, Wes), New,
                                                 Angle)}
                 || #{id := Id, bounds := New, rotation := Angle}
                    <- Placements],
            Unpacked = Unpacked0 ++ Degenerate,
            Stats = Stats0#{unpacked => length(Unpacked)},
            #{placed => Placed,
              unpacked => Unpacked,
              degenerate => Degenerate,
              stats => Stats}
    end.

chart_descs([We | Rest], Movable, Normalize, Descs, Degenerate) ->
    case chart_desc(We, Movable, Normalize) of
        degenerate ->
            case auv_pack_utils:is_movable(We#we.id, Movable) of
                true ->
                    chart_descs(Rest, Movable, Normalize, Descs,
                                [We#we.id | Degenerate]);
                false ->
                    chart_descs(Rest, Movable, Normalize, Descs, Degenerate)
            end;
        Desc ->
            chart_descs(Rest, Movable, Normalize, [Desc | Descs],
                        Degenerate)
    end;
chart_descs([], _Movable, _Normalize, Descs, Degenerate) ->
    {lists:reverse(Descs), lists:reverse(Degenerate)}.

chart_desc(#we{id=Id}=We, Movable, Normalize) ->
    case auv_pack_utils:chart_bbox(We) of
        degenerate ->
            degenerate;
        {X0, Y0, W, H} ->
            Fixed = not auv_pack_utils:is_movable(Id, Movable),
            Base = #{id => Id,
                     bounds => {X0, Y0, W, H},
                     fixed => Fixed},
            case Normalize andalso not Fixed of
                true ->
                    case auv_pack_utils:chart_obb(We) of
                        none -> Base;
                        OBB -> Base#{obb => OBB}
                    end;
                false ->
                    Base
            end
    end.
