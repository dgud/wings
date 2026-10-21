%%
%%  auv_packing.erl
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

%% Pure computation: no Wings3D state is read or modified here. The caller
%% (AutoUV integration) builds the input from the charts of ONE tile and
%% applies the returned plan to the UVs.
%%
%% Tiles are always 1x1, but each one lives at its own origin in the UV
%% space (e.g. [0,1]x[0,1], [0,1]x[1,2]). The origin is passed in the
%% 'origin' option; chart bounds are expressed in that same global UV space.
%%
%% Charts:
%%   [#{id := Id, bounds := {X, Y, W, H}, fixed => boolean()}]
%%   fixed = true  -> obstacle (never moved)
%%   fixed = false -> movable (default)
%%
%% Options (all optional):
%%   gap            => Percent of the tile side (default 0). Minimum distance
%%                     between the bounding boxes of any two charts.
%%   margin         => Percent of the tile side (default 0). Distance kept
%%                     between movable charts and the tile borders.
%%   rotation_step  => 0 | 15 | 30 | 45 | 90 (default 0). Angle step in
%%                     degrees for rotation candidates.
%%   profile        => fast | balanced | quality (default balanced).
%%                     Predefined parameter sets; explicit options override.
%%   normalize      => boolean() (default false). Pre-rotate each chart to
%%                     its minimum-area oriented bounding box before packing.
%%   origin         => {U, V} (default {0.0, 0.0}). Lower-left corner of the tile.
%%
%% Result of pack_tile/2:
%%   #{placements := [#{id, bounds, rotation}],
%%     unpacked   := [Id],
%%     stats      := #{...}}
%%   {error, Reason}
%%
%% Result of pack_tiles/2:
%%   [{TileId, Result}]  (Result as in pack_tile/2)
%%   {error, {tile, TileId, Reason}}
%%
%% 'bounds' in a placement is the final bounding box {X, Y, W, H}, already
%% with the dimensions of the oriented bounding box when rotated. 'rotation'
%% is the rotation angle in degrees applied to the chart. To apply it to the
%% UVs: rotate the vertices counterclockwise by `rotation` degrees around the
%% center of the ORIGINAL bounding box, then translate so that the center of
%% the original box lands on the center of the placement bounds.

-module(auv_packing).

-include_lib("wings/src/wings.hrl").

-export([pack_tiles/2, pack_tile/2, pack_charts/3,
         normalize_chart_obb/1, critical_unpacked/4]).

-define(EPS, 1.0e-9).

-record(rect, {x, y, w, h, obb = none}).

%%%
%%% Public API
%%%

%% Tiles: [#{id := TileId, charts := Charts}]
%% The same options are applied independently to each tile. Per-tile origin
%% may be given with an optional 'origin' key in the tile map, overriding the
%% one in Options.
pack_tiles(Tiles, Options) when is_list(Tiles), is_map(Options) ->
    pack_tiles_1(Tiles, Options, []);
pack_tiles(_, _) ->
    {error, invalid_input}.

pack_tiles_1([], _Options, Acc) ->
    lists:reverse(Acc);
pack_tiles_1([#{id := TileId, charts := Charts} = Tile | Rest], Options, Acc) ->
    TileOptions = case maps:find(origin, Tile) of
                      error -> Options;
                      {ok, Origin} -> Options#{origin => Origin}
                  end,
    case pack_tile(Charts, TileOptions) of
        {error, Reason} ->
            {error, {tile, TileId, Reason}};
        Result ->
            pack_tiles_1(Rest, Options, [{TileId, Result} | Acc])
    end;
pack_tiles_1([_ | _], _Options, _Acc) ->
    {error, invalid_tile}.

pack_tile(Charts, Options) when is_list(Charts), is_map(Options) ->
    case validate_options(Options) of
        {Gap, Margin, RotationStep, Profile, Normalize, Origin} ->
            case validate_charts(Charts) of
                {error, _}=Error ->
                    Error;
                {Fixed, Movable} ->
                    pack_validated(Fixed, Movable, Gap, Margin,
                                   RotationStep, Profile, Normalize, Origin)
            end;
        Error ->
            Error
    end;
pack_tile(_, _) ->
    {error, invalid_input}.

%% Rotate chart to its minimum-area oriented bounding box.
%% Uses rotating calipers on the convex hull of UV vertices.
normalize_chart_obb(#we{vp=Vs}=We) ->
    Points = [Pos || {_V, Pos} <- array:sparse_to_orddict(Vs)],
    case length(Points) of
        N when N < 3 ->
            We;  %% Degenerate: keep as-is
        _ ->
            Hull = convex_hull_2d(Points),
            {Angle, _Area} = min_area_obb_2d(Hull),
            case abs(Angle) < ?EPS of
                true ->
                    We;
                false ->
                    center_obb(We, Angle)
            end
    end.

%% 2D convex hull (monotone chain). Returns vertices in CCW order.
convex_hull_2d(Points) ->
    Sorted = lists:usort([{X, Y} || {X, Y, _} <- Points]),
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
           case find_chart(Id, Charts) of
               none ->
                   false;
               {ok, We} ->
                   Bounds = chart_bounds(We),
                   lists:any(fun(Other) ->
                                     bounds_overlap(Bounds, Other)
                             end, ObstacleBounds)
           end].

find_chart(Id, Charts) ->
    case lists:keyfind(Id, #we.id, Charts) of
        false -> none;
        We -> {ok, We}
    end.

chart_bounds(#we{vp=Vs}) ->
    {{X0,Y0,_}, {X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
    {X0,Y0,X1-X0,Y1-Y0}.

bounds_overlap({X1,Y1,W1,H1}, {X2,Y2,W2,H2}) ->
    X1 < (X2 + W2) andalso (X1 + W1) > X2 andalso
    Y1 < (Y2 + H2) andalso (Y1 + H1) > Y2.


%%%
%%% Validation
%%%

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
%%% Packing
%%%

pack_validated(Fixed, Movable, Gap, Margin, RotationStep, Profile,
               Normalize, {OU, OV}) ->
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
            MovableNorm = apply_normalize(Movable, Normalize),
            TotalAngles =
                case RotationStep of
                    0 -> 1;
                    Step -> trunc(round(180/Step)) +1
                end,
            {Placements, Unpacked} =
                place_all(sort_movable(MovableNorm), Free, Gap, HalfGap,
                        RotationStep, Profile, TotalAngles, [], []),
            make_result(Placements, Unpacked, Movable, Fixed)
    end.

%% Largest area first; the id makes the order deterministic.
sort_movable(Movable) ->
    Keyed = [{-(R#rect.w * R#rect.h), Id, R} || {Id, R} <- Movable],
    [{Id, R} || {_Key, Id, R} <- lists:sort(Keyed)].

%% Profile definitions: {max_search}
%% These are coarse-tuning knobs, not algorithmic changes. They limit
%% the number of rotation-angle candidates evaluated per chart. Placeholder
%% values; tune after benchmarking.
profile_params(fast) ->
    #{max_search => 1};
profile_params(balanced) ->
    #{max_search => balanced};
profile_params(quality) ->
    #{max_search => quality};
profile_params(max) ->
    #{max_search => infinity}.

place_all([], _Free, _Gap, _HalfGap, _RotationStep, _Profile, _TotalAngles,
          Placements, Unpacked) ->
    {lists:reverse(Placements), lists:reverse(Unpacked)};
place_all([{Id, Rect} | Rest], Free, Gap, HalfGap,
          RotationStep, Profile, TotalAngles, Placements, Unpacked) ->
    #{max_search := MaxSearch0} = profile_params(Profile),
    MaxSearch = resolve_max_search(MaxSearch0, TotalAngles),
    case best_position(Rect, Free, Gap, RotationStep, MaxSearch) of
        none ->
            Original = expand(Rect, HalfGap),
            NewFree = prune_free(subtract_from_free(Free, Original)),
            place_all(Rest, NewFree, Gap, HalfGap, RotationStep, Profile,
                      TotalAngles, Placements, [Id | Unpacked]);
        {X, Y, ReservedW, ReservedH, Angle} ->
            Used = #rect{x = X, y = Y, w = ReservedW, h = ReservedH},
            Placement = #{id => Id,
                          bounds => {X + HalfGap, Y + HalfGap,
                                     max(0.0, ReservedW - Gap),
                                     max(0.0, ReservedH - Gap)},
                          rotation => Angle},
            NewFree = prune_free(subtract_from_free(Free, Used)),
            place_all(Rest, NewFree, Gap, HalfGap, RotationStep, Profile,
                      TotalAngles, [Placement | Placements], Unpacked)
    end.

resolve_max_search(infinity, Total) ->
    Total;
resolve_max_search(balanced, Total) ->
    max(1, round(2 * Total / 5));
resolve_max_search(quality, Total) ->
    max(1, round(4 * Total / 5));
resolve_max_search(N, _Total) when is_integer(N) ->
    N.

best_position(Rect, Free, Gap, RotationStep, MaxSearch) ->
    RegularCandidates =
        [{Angle, W, H}
         || Angle <- rotation_angles(RotationStep, MaxSearch),
            {W, H} <- [rotated_bounds(Rect#rect.w, Rect#rect.h, Angle)]],
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
           || {Angle, W, H} <- unique_orientations(Candidates0)]),
    case Candidates of
        [] ->
            none;
        _ ->
            unpack_candidate(lists:min(Candidates))
    end.

unique_orientations(Candidates) ->
    lists:ukeysort(1, Candidates).

rotation_angles(0, _MaxSearch) ->
    [0.0];
rotation_angles(Step, MaxSearch) ->
    All = [float(A) || A <- lists:seq(0, 180 - Step, Step)],
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

%rotation_angles(0) ->
%    [+0.0];
%rotation_angles(Step) ->
%    [float(A) || A <- lists:seq(0, 180 - Step, Step)].

rotated_bounds(W, H, +0.0) ->
    {W, H};
rotated_bounds(W, H, Angle) ->
    Rad = Angle * math:pi() / 180.0,
    C = abs(math:cos(Rad)),
    S = abs(math:sin(Rad)),
    {C * W + S * H, S * W + C * H}.

%% Best Short Side Fit: smallest leftover on the short side wins; the long
%% side leftover, then position and angle, break ties.
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
%%% Normalization (minimum-area oriented bounding box via rotating calipers)
%%%

apply_normalize(Movable, true) ->
    [{Id, oriented_bounding_box(Rect)} || {Id, Rect} <- Movable];
apply_normalize(Movable, _) ->
    Movable.

%% The rectangle is assumed to be the axis-aligned bounding box of a set of
%% points (the chart vertices). We approximate the oriented bounding box by
%% computing the convex hull's rotating calipers. Since we only have the AABB
%% here, we return it as-is, delegating the real OBB computation to the
%% caller (pack_charts). The caller will pre-rotate the chart and supply
%% updated bounds. The normalized Movable list will have Rect already
%% representing the OBB dimensions and the chart vertices will be rotated
%% accordingly by the adapter.
%%
%% For the core algorithm, an OBB is just a rectangle; the caller guarantees
%% that the bounds passed in are the OBB when normalize is true.
oriented_bounding_box(Rect) ->
    Rect.

%%%
%%% Free rectangle management
%%%

subtract_from_free(Free, Used) ->
    lists:append([split_free(F, Used) || F <- Free]).

%% Maximal-rectangles split: the four resulting rectangles intentionally
%% overlap (top/bottom span the full width, left/right the full height), so
%% that every maximal free area stays available to later placements.
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

%% Drops every rectangle contained in another one (duplicates included).
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

%% Touching edges (within EPS) do not count as intersection.
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
%%   Charts  = [#we{}] of ONE tile (UV islands)
%%   Movable = all | [Id]   (charts not listed are obstacles)
%%   Result  = #{placed   := [{Id, #we{}}],   %% already transformed
%%               unpacked := [Id],
%%               stats    := #{...}}
%% Charts with a degenerate bounding box are ignored.
%% When Options includes normalize => true, each chart is pre-rotated to
%% its minimum-area oriented bounding box and the bounds passed to the core
%% represent that OBB.
pack_charts(Charts, Movable, Options) ->
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
                [{Id, place_chart(maps:get(Id, Wes), New, Angle)}
                 || #{id := Id, bounds := New, rotation := Angle}
                    <- Placements],
            Unpacked = Unpacked0 ++ Degenerate,
            Stats = Stats0#{unpacked => length(Unpacked)},
            #{placed => Placed,
              unpacked => Unpacked,
              stats => Stats}
    end.

chart_descs([We | Rest], Movable, Normalize, Descs, Degenerate) ->
    case chart_desc(We, Movable, Normalize) of
        degenerate ->
            case is_movable(We#we.id, Movable) of
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

chart_desc(#we{id=Id, vp=Vs}, Movable, Normalize) ->
    case array:sparse_to_list(Vs) of
        [] ->
            degenerate;
        Positions ->
            {{X0,Y0,_}, {X1,Y1,_}} = e3d_bv:box(Positions),
            W = X1 - X0,
            H = Y1 - Y0,
            case W < ?EPS orelse H < ?EPS of
                true ->
                    degenerate;
                false ->
                    Fixed = not is_movable(Id, Movable),
                    Base = #{id => Id,
                             bounds => {X0, Y0, W, H},
                             fixed => Fixed},
                    case Normalize andalso not Fixed of
                        true ->
                            case chart_obb(Positions) of
                                none ->
                                    Base;
                                OBB ->
                                    Base#{obb => OBB}
                            end;
                        false ->
                            Base
                    end
            end
    end.

chart_obb(Positions) ->
    Points = [{X, Y} || {_V, {X, Y, _}} <- Positions],
    case length(lists:usort(Points)) of
        N when N < 3 ->
            none;
        _ ->
            Hull = convex_hull_2d(Points),
            {Angle, _Area} = min_area_obb_2d(Hull),
            {OBBW, OBBH} = projected_hull_size(Hull, -Angle * math:pi() / 180.0),
            case OBBW < ?EPS orelse OBBH < ?EPS of
                true -> none;
                false -> {-Angle, OBBW, OBBH}
            end
    end.

projected_hull_size(Hull, Angle) ->
    Cos = math:cos(Angle),
    Sin = math:sin(Angle),
    Rotated = [{X * Cos - Y * Sin, X * Sin + Y * Cos}
               || {X, Y} <- Hull],
    Xs = [X || {X, _Y} <- Rotated],
    Ys = [Y || {_X, Y} <- Rotated],
    {lists:max(Xs) - lists:min(Xs),
     lists:max(Ys) - lists:min(Ys)}.

is_movable(_Id, all) ->
    true;
is_movable(Id, Ids) when is_list(Ids) ->
    lists:member(Id, Ids).

%% Rotate counterclockwise around the original bounding-box center, then
%% translate that center to the center of the placement bounds.
place_chart(#we{vp=Vs}=We, {X,Y,W,H}, Angle) ->
    {{X0,Y0,_},{X1,Y1,_}} = e3d_bv:box(array:sparse_to_list(Vs)),
    From = {(X0 + X1) / 2.0, (Y0 + Y1) / 2.0, +0.0},
    To = {X + W / 2.0, Y + H / 2.0, +0.0},
    ToOrigin = e3d_mat:translate(e3d_vec:neg(From)),
    Rotate = e3d_mat:rotate(Angle, {+0.0,+0.0,1.0}),
    M0 = e3d_mat:mul(Rotate, ToOrigin),
    T = e3d_mat:mul(e3d_mat:translate(To), M0),
    wings_we:transform_vs(T, We).


%% Minimum-area oriented bounding box via rotating calipers.
%% Returns {angle_radians, area} of the best OBB.
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

%% Rotate chart vertices by Angle degrees around their center.
center_obb(#we{vp=Vs}=We, Angle) ->
    {{X0, Y0, _}, {X1, Y1, _}} = e3d_bv:box(array:sparse_to_list(Vs)),
    Center = {(X0 + X1) / 2.0, (Y0 + Y1) / 2.0, 0.0},
    ToOrigin = e3d_mat:translate(e3d_vec:neg(Center)),
    MRot = e3d_mat:rotate(Angle, {0.0,0.0,1.0}),
    M0 = e3d_mat:mul(MRot, ToOrigin),
    T = e3d_mat:mul(e3d_mat:translate(Center), M0),
    wings_we:transform_vs(T, We).
