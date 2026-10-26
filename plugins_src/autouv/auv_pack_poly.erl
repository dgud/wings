%%
%%  auv_pack_poly.erl
%%
%%      UV island packing by polygonal outlines.
%%
%%  Copyright (c) 2026 Micheus Vieira (micheus)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Packs the charts of one tile using their outlines (holes included), so
%% small charts can sit inside the holes of bigger ones. pack/2 is pure;
%% pack_charts/3 is the adapter used by wpc_autouv, with the same contract
%% as auv_pack_maxrects:pack_charts/3.
%%
%% pack/2 gets Items = [#{id, poly, fixed => boolean()}] (poly from
%% auv_poly:new/2; fixed items are obstacles) and the options of
%% auv_pack_maxrects plus 'order' (area | maxdim | bbox). It returns
%% #{placements, unpacked, critical, stats} | {error, Reason}.
%% A placement is #{id, rotation, delta}: rotation in degrees around the
%% center of the bounding box, then translation by delta
%% (see auv_pack_utils:transform_chart/3).
%%
%% Search: bottom-left fill on a conservative occupancy grid, verified with
%% the exact test, followed by a short exact compaction.
%% Charts that do not fit stay in place; 'critical' lists those still
%% overlapping another chart.

-module(auv_pack_poly).

-include_lib("wings/src/wings.hrl").

-export([pack_charts/3]).

-define(EPS, 1.0e-9).
-define(CEPS, 1.0e-9).       %% tolerance, in cell units
-define(MAX_SLIDE, 3.0).     %% max slide per move, in cells
-define(MAX_VERIFY, 32).     %% exact-test failures tolerated per rotation

%%%
%%% Public API
%%%

pack(Items, Options) when is_list(Items), is_map(Options) ->
    Order = maps:get(order, Options, maxdim),
    case auv_pack_utils:validate_options(Options) of
        {error, _} = Error1 ->
            Error1;
        _ when Order =/= area, Order =/= maxdim, Order =/= bbox ->
            {error, invalid_options};
        {Gap, Margin, RotationStep, Profile, Normalize, {OU, OV}} ->
            Side = 1.0 - 2.0 * Margin,
            case Side > ?EPS of
                false ->
                    {error, margin_exceeds_tile};
                true ->
                    case validate_items(Items) of
                        {error, _} = Error2 ->
                            Error2;
                        {Fixed, Movable} ->
                            X0 = OU + Margin,
                            Y0 = OV + Margin,
                            #{max_search := MaxSearch, grid := N, compact := Rounds} =
                                profile_params(Profile),
                            C = Side / N,
                            Ctx = #{rect => {X0, Y0, X0 + Side, Y0 + Side},
                                    gap => Gap,
                                    step => RotationStep,
                                    max_search => MaxSearch,
                                    normalize => Normalize,
                                    n => N,
                                    c => C,
                                    compact => Rounds,
                                    g => max(0, ceil(Gap / C - ?CEPS))},
                            Obstacles = [Poly || {_Id, Poly} <- Fixed],
                            Occ0 = lists:foldl(
                                     fun(P, Acc) -> occ_add_global(P, Acc, Ctx) end,
                                     erlang:make_tuple(N, 0), Obstacles),
                            {Placements, Unpacked, Critical} =
                                place_all(sort_movable(Movable, Order), Obstacles,
                                          Occ0, Ctx, [], [], []),
                            Final = compact(Placements, Movable, Fixed,
                                            Unpacked, Ctx),
                            Used = used_height(Final, Movable, Fixed, Y0),
                            make_result(Final, Unpacked, Critical,
                                        Movable, Fixed, Used, Side)
                    end
            end
    end;
pack(_, _) ->
    {error, invalid_input}.

%% Contours (auv_pack_utils:extract_polygons/1) -> {Items, Degenerate}.
%% Degenerate: movable charts without a usable outline.
to_items(Contours, Vtabs, Movable) ->
    lists:foldr(
      fun({Id, _} = Contour, {Items, Bad}) ->
              case contour_to_poly(Contour, maps:get(Id, Vtabs, undefined)) of
                  #{} = Poly ->
                      IsFixed = not auv_pack_utils:is_movable(Id, Movable),
                      {[#{id => Id, poly => Poly, fixed => IsFixed} | Items],
                       Bad};
                  {error, _} ->
                      case auv_pack_utils:is_movable(Id, Movable) of
                          true -> {Items, [Id | Bad]};
                          false -> {Items, Bad}
                      end
              end
      end, {[], []}, Contours).

%% Vertex ids -> {U,V} positions -> polygon.
contour_to_poly({_Id, none}, _Vtab) ->
    {error, no_contour};
contour_to_poly({_Id, {{_Perim, Outer}, Inners}}, Vtab) ->
    Pts = fun(Vs) ->
                  [begin {U, V, _} = array:get(V0, Vtab), {U, V} end
                   || V0 <- Vs]
          end,
    auv_poly:new(Pts(Outer), [Pts(L) || {_Perim1, L} <- Inners]).

%% Rotation (degrees) aligning the minimum-area OBB with the axes.
obb_rotation(#{outer := Outer}) ->
    case auv_pack_utils:convex_hull_2d(Outer) of
        Hull when length(Hull) >= 3 ->
            {Angle, _Area} = auv_pack_utils:min_area_obb_2d(Hull),
            -Angle + 0.0;
        _ ->
            none
    end.

%%%
%%% Validation
%%%

validate_items(Items) ->
    validate_items(Items, #{}, [], []).

validate_items([], _Seen, Fixed, Movable) ->
    {lists:reverse(Fixed), lists:reverse(Movable)};
validate_items([#{id := Id, poly := #{outer := _, holes := _, area := _,
                                       bbox := _} = Poly} = Item | Rest],
               Seen, Fixed, Movable) ->
    IsFixed = maps:get(fixed, Item, false),
    case {is_boolean(IsFixed), maps:is_key(Id, Seen)} of
        {false, _} ->
            {error, invalid_item};
        {_, true} ->
            {error, duplicate_id};
        {true, false} ->
            Seen1 = Seen#{Id => true},
            case IsFixed of
                true ->
                    validate_items(Rest, Seen1, [{Id, Poly} | Fixed], Movable);
                false ->
                    validate_items(Rest, Seen1, Fixed, [{Id, Poly} | Movable])
            end
    end;
validate_items(_, _Seen, _Fixed, _Movable) ->
    {error, invalid_item}.

%%%
%%% Packing
%%%

%% Grid cells per side, rotation candidates and compaction rounds.
profile_params(fast) ->
    #{max_search => 1, grid => 128, compact => 0};
profile_params(balanced) ->
    #{max_search => 4, grid => 192, compact => 1};
profile_params(quality) ->
    #{max_search => 8, grid => 256, compact => 2};
profile_params(max) ->
    #{max_search => 16, grid => 384, compact => 3}.

%% Placement order: area, longest bbox side (default) or bbox area.
sort_movable(Movable, Order) ->
    Keyed = [{-sort_key(Order, Poly), Id, Poly} || {Id, Poly} <- Movable],
    [{Id, Poly} || {_Key, Id, Poly} <- lists:sort(Keyed)].

sort_key(area, Poly) ->
    maps:get(area, Poly);
sort_key(Order, #{bbox := {X0, Y0, X1, Y1}}) ->
    case Order of
        maxdim -> max(X1 - X0, Y1 - Y0);
        bbox -> (X1 - X0) * (Y1 - Y0)
    end.

%% Top of the highest polygon, measured from the bottom of the usable area.
used_height(Placements, Movable, Fixed, Y0) ->
    MovMap = maps:from_list(Movable),
    Tops = [begin {_, _, _, Y1} = maps:get(bbox, placed_poly(maps:get(Id, MovMap),
                                                            Rot, Delta)),
                  Y1
            end || #{id := Id, rotation := Rot, delta := Delta} <- Placements]
        ++ [begin {_, _, _, Y1} = maps:get(bbox, P), Y1 end || {_, P} <- Fixed],
    case Tops of
        [] -> 0.0;
        _ -> max(0.0, lists:max(Tops) - Y0)
    end.

place_all([], _Obstacles, _Occ, _Ctx, Placements, Unpacked, Critical) ->
    {lists:reverse(Placements), lists:reverse(Unpacked),
     lists:reverse(Critical)};
place_all([{Id, Poly} | Rest], Obstacles, Occ, Ctx, Placements, Unpacked,
          Critical) ->
    Angles = rotation_candidates(Poly, Ctx),
    case best_placement(Poly, Angles, Obstacles, Occ, Ctx) of
        none ->
            %% Stays in place; 'critical' when it overlaps what is already placed.
            Critical1 = case overlaps_any(Poly, Obstacles) of
                            true -> [Id | Critical];
                            false -> Critical
                        end,
            Occ1 = occ_add_global(Poly, Occ, Ctx),
            place_all(Rest, [Poly | Obstacles], Occ1, Ctx, Placements,
                      [Id | Unpacked], Critical1);
        {Angle, Delta, Placed, X, Y, Rows} ->
            Placement = #{id => Id, rotation => Angle, delta => Delta},
            Occ1 = occ_add(Rows, X, Y, Occ, maps:get(n, Ctx)),
            place_all(Rest, [Placed | Obstacles], Occ1, Ctx,
                      [Placement | Placements], Unpacked, Critical)
    end.

overlaps_any(Poly, Obstacles) ->
    lists:any(fun(O) -> auv_poly:collide(Poly, O, 0.0) end, Obstacles).

rotation_candidates(Poly, #{step := Step, max_search := MaxSearch,
                            normalize := Normalize}) ->
    Regular = auv_pack_utils:rotation_angles(Step, MaxSearch),
    %% OBB angle first.
    All = case Normalize andalso obb_rotation(Poly) of
              false -> Regular;
              none -> Regular;
              Angle -> [Angle | Regular]
          end,
    dedup_angles(All).

dedup_angles(Angles) ->
    lists:reverse(
      lists:foldl(
        fun(A, Acc) ->
                case lists:any(fun(B) -> abs(A - B) < 1.0e-6 end, Acc) of
                    true -> Acc;
                    false -> [A | Acc]
                end
        end, [], Angles)).

%% -> none | {Angle, {Dx, Dy}, PlacedPoly, X, Y, Rows}
best_placement(Poly, Angles, Obstacles, Occ,
               #{rect := {RX0, RY0, _, _}, c := C} = Ctx) ->
    %% After a fit, later angles only search up to the best top found.
    Best =
        lists:foldl(
          fun(Angle, Acc) ->
                  Rot = auv_poly:rotate(Poly, Angle),
                  {BX0, BY0, BX1, BY1} = maps:get(bbox, Rot),
                  W = BX1 - BX0,
                  H = BY1 - BY0,
                  P0 = auv_poly:translate(Rot, -BX0, -BY0),
                  TopLimit = case Acc of
                                 none -> infinity;
                                 {{T, _, _}, _, _, _, _, _} -> T
                             end,
                  case grid_search(P0, W, H, Obstacles, Occ, Ctx, TopLimit) of
                      none ->
                          Acc;
                      {X, Y, Rows} ->
                          LX = RX0 + X * C,
                          LY = RY0 + Y * C,
                          Cand = {{LY + H, LX, Angle},
                                  {LX - BX0, LY - BY0}, Rot, X, Y, Rows},
                          case Acc of
                              none -> Cand;
                              _ -> erlang:min(Acc, Cand)
                          end
                  end
          end, none, Angles),
    case Best of
        none ->
            none;
        {{_Top, _X, Angle1}, {Dx, Dy}, Rot1, X1, Y1, Rows1} ->
            {Angle1, {Dx, Dy}, auv_poly:translate(Rot1, Dx, Dy),
             X1, Y1, Rows1}
    end.

%%%
%%% Compaction
%%%

%% Slides each placed chart down, then left, while the exact test allows it.
%% Fixed and unpacked charts do not move.
compact(Placements, _Movable, _Fixed, _Unpacked, #{compact := 0}) ->
    Placements;
compact([], _Movable, _Fixed, _Unpacked, _Ctx) ->
    [];
compact(Placements, Movable, Fixed, Unpacked,
        #{compact := Rounds} = Ctx) ->
    MovMap = maps:from_list(Movable),
    Static = [P || {_Id, P} <- Fixed] ++ [maps:get(Id, MovMap) || Id <- Unpacked],
    Cur0 = maps:from_list(
             [{Id, {Delta, placed_poly(maps:get(Id, MovMap), Rot, Delta)}}
              || #{id := Id, rotation := Rot, delta := Delta} <- Placements]),
    Order = [Id || {_Y, _X, Id} <-
                       lists:sort([begin {X0, Y0, _, _} = maps:get(bbox, P),
                                         {Y0, X0, Id}
                                   end || {Id, {_, P}} <- maps:to_list(Cur0)])],
    Cur = compact_rounds(Rounds, Order, Static, Cur0, Ctx),
    [begin {Delta, _} = maps:get(Id, Cur), Pl#{delta => Delta} end
     || #{id := Id} = Pl <- Placements].

placed_poly(Poly, Rot, {Dx, Dy}) ->
    auv_poly:translate(auv_poly:rotate(Poly, Rot), Dx, Dy).

compact_rounds(0, _Order, _Static, Cur, _Ctx) ->
    Cur;
compact_rounds(R, Order, Static, Cur, Ctx) ->
    {Cur1, Moved} =
        lists:foldl(
          fun(Id, {C, Mv}) ->
                  {{Dx, Dy}, P} = maps:get(Id, C),
                  Others = Static ++ [Q || {I, {_, Q}} <- maps:to_list(C),
                                           I =/= Id],
                  {P1, TY} = slide(P, down, Others, Ctx),
                  {P2, TX} = slide(P1, left, Others, Ctx),
                  case TX + TY > 0.0 of
                      true -> {C#{Id => {{Dx - TX, Dy - TY}, P2}}, Mv + TX + TY};
                      false -> {C, Mv}
                  end
          end, {Cur, 0.0}, Order),
    case Moved > 1.0e-9 of
        true -> compact_rounds(R - 1, Order, Static, Cur1, Ctx);
        false -> Cur1
    end.

%% -> {MovedPoly, Distance}. Tries the whole run, then steps that halve on failure.
slide(P, Dir, Others, #{rect := {RX0, RY0, _, _}, c := C, gap := Gap}) ->
    {BX0, BY0, BX1, BY1} = maps:get(bbox, P),
    Smin = C / 8.0,
    {Tmax, Swept, Sx, Sy} =
        case Dir of
            down -> T1 = min(BY0 - RY0, ?MAX_SLIDE * C),
                    {T1, {BX0, BY0 - T1, BX1, BY1}, 0.0, -1.0};
            left -> T2 = min(BX0 - RX0, ?MAX_SLIDE * C),
                    {T2, {BX0 - T2, BY0, BX1, BY1}, -1.0, 0.0}
        end,
    case Tmax > Smin of
        false ->
            {P, 0.0};
        true ->
            Near = [O || O <- Others, box_near(Swept, maps:get(bbox, O), Gap)],
            Valid = fun(T) ->
                            M = auv_poly:translate(P, Sx * T, Sy * T),
                            not lists:any(
                                  fun(O) -> auv_poly:collide(M, O, Gap) end,
                                  Near)
                    end,
            T = case Valid(Tmax) of
                    true -> Tmax;
                    false -> gallop(0.0, min(8.0 * C, Tmax), Smin, Tmax, Valid)
                end,
            case T > 0.0 of
                true -> {auv_poly:translate(P, Sx * T, Sy * T), T};
                false -> {P, 0.0}
            end
    end.

gallop(T, S, Smin, _Tmax, _Valid) when S < Smin ->
    T;
gallop(T, S, Smin, Tmax, Valid) ->
    T2 = T + S,
    case T2 =< Tmax andalso Valid(T2) of
        true -> gallop(T2, S, Smin, Tmax, Valid);
        false -> gallop(T, S / 2.0, Smin, Tmax, Valid)
    end.

%%%
%%% Occupancy grid
%%%

%% P0 has its bbox corner at the origin. -> none | {X, Y, Rows}
grid_search(P0, W, H, Obstacles, Occ,
            #{rect := {RX0, RY0, RX1, RY1}, n := N, c := C, g := G,
              gap := Gap}, TopLimit) ->
    case W =< RX1 - RX0 + ?EPS andalso H =< RY1 - RY0 + ?EPS of
        false ->
            none;
        true ->
            case raster(P0, 0.0, 0.0, C, N) of
                [] ->
                    none;
                Rows0 ->
                    Rows = dilate(Rows0, G),
                    Xmax = max(0, floor((RX1 - RX0 - W) / C + ?CEPS)),
                    Ymax0 = max(0, floor((RY1 - RY0 - H) / C + ?CEPS)),
                    Ymax = case TopLimit of
                               infinity -> Ymax0;
                               _ -> min(Ymax0,
                                        floor((TopLimit - H - RY0) / C + ?CEPS))
                           end,
                    Verify = fun(X, Y) ->
                                     fits(P0, RX0 + X * C, RY0 + Y * C,
                                          W, H, Obstacles, Gap)
                             end,
                    S = #{xmax => Xmax, ymax => Ymax,
                          key => key_rows(Rows), all => order_rows(Rows),
                          occ => Occ, g => G, n => N, verify => Verify},
                    case scan(0, S, ?MAX_VERIFY) of
                        none -> none;
                        {X, Y} -> {X, Y, Rows0}
                    end
            end
    end.

%% Bottom-up scan; key rows prefilter X, survivors get the full row test.
scan(Y, #{ymax := Ymax}, _Tries) when Y > Ymax ->
    none;
scan(Y, #{xmax := Xmax, key := Key, occ := Occ, g := G, n := N} = S,
     Tries) ->
    Cand = filter_rows(Key, Y, (1 bsl (Xmax + 1)) - 1, Occ, G, N),
    case try_cands(Cand, 0, Y, S, Tries) of
        {ok, X} -> {X, Y};
        {more, Tries1} -> scan(Y + 1, S, Tries1);
        stop -> none
    end.

try_cands(0, _X, _Y, _S, Tries) ->
    {more, Tries};
try_cands(Cand, X, Y,
          #{all := All, occ := Occ, g := G, n := N, verify := Verify} = S,
          Tries) ->
    case Cand band 1 of
        0 ->
            try_cands(Cand bsr 1, X + 1, Y, S, Tries);
        1 ->
            case free_at(All, X, Y, G, Occ, N) of
                false ->
                    try_cands(Cand bsr 1, X + 1, Y, S, Tries);
                true ->
                    case Verify(X, Y) of
                        true -> {ok, X};
                        false when Tries > 1 ->
                            try_cands(Cand bsr 1, X + 1, Y, S, Tries - 1);
                        false -> stop
                    end
            end
    end.

%% Removes from Cand the X values blocked on the key rows.
filter_rows(_Key, _Y, 0, _Occ, _G, _N) ->
    0;
filter_rows([], _Y, Cand, _Occ, _G, _N) ->
    Cand;
filter_rows([{R, _M, Runs} | Rest], Y, Cand, Occ, G, N) ->
    Yy = Y + R - G,
    case Yy < 0 orelse Yy >= N of
        true ->
            filter_rows(Rest, Y, Cand, Occ, G, N);
        false ->
            case element(Yy + 1, Occ) of
                0 ->
                    filter_rows(Rest, Y, Cand, Occ, G, N);
                O ->
                    Cand1 = lists:foldl(
                              fun({B0, Len}, C0) ->
                                      Blocked = shift(window_or(O, Len), G - B0),
                                      C0 band (bnot Blocked)
                              end, Cand, Runs),
                    filter_rows(Rest, Y, Cand1, Occ, G, N)
            end
    end.

%% Full grid test at cell shift {X, Y}.
free_at([], _X, _Y, _G, _Occ, _N) ->
    true;
free_at([{R, M} | Rest], X, Y, G, Occ, N) ->
    Yy = Y + R - G,
    case Yy < 0 orelse Yy >= N of
        true ->
            free_at(Rest, X, Y, G, Occ, N);
        false ->
            case element(Yy + 1, Occ) of
                0 ->
                    free_at(Rest, X, Y, G, Occ, N);
                O ->
                    case shift(M, X - G) band O of
                        0 -> free_at(Rest, X, Y, G, Occ, N);
                        _ -> false
                    end
            end
    end.

%% Bit x set when M has a set bit in [x, x+W-1].
window_or(M, W) ->
    window_or(M, 1, W).

window_or(R, Len, W) when Len * 2 =< W ->
    window_or(R bor (R bsr Len), Len * 2, W);
window_or(R, Len, W) when Len < W ->
    R bor (R bsr (W - Len));
window_or(R, _Len, _W) ->
    R.

%% Bit x set when M has a set bit in [x-W+1, x].
spread(M, W) ->
    spread(M, 1, W).

spread(R, Len, W) when Len * 2 =< W ->
    spread(R bor (R bsl Len), Len * 2, W);
spread(R, Len, W) when Len < W ->
    R bor (R bsl (W - Len));
spread(R, _Len, _W) ->
    R.

shift(M, S) when S >= 0 -> M bsl S;
shift(M, S) -> M bsr (-S).

%% Runs of set bits: [{Start, Length}].
runs(M) ->
    runs(M, 0, none, []).

runs(0, _I, none, Acc) ->
    lists:reverse(Acc);
runs(0, I, Start, Acc) ->
    lists:reverse([{Start, I - Start} | Acc]);
runs(M, I, Start, Acc) ->
    case {M band 1, Start} of
        {1, none} -> runs(M bsr 1, I + 1, I, Acc);
        {1, _} -> runs(M bsr 1, I + 1, Start, Acc);
        {0, none} -> runs(M bsr 1, I + 1, none, Acc);
        {0, _} -> runs(M bsr 1, I + 1, none, [{Start, I - Start} | Acc])
    end.

%% Up to five evenly spread rows with their runs.
key_rows(Rows) ->
    L = length(Rows),
    Sel = case L =< 5 of
              true ->
                  Rows;
              false ->
                  Idx = lists:usort([1 + (I * (L - 1)) div 4
                                     || I <- lists:seq(0, 4)]),
                  [lists:nth(K, Rows) || K <- Idx]
          end,
    [{R, M, runs(M)} || {R, M} <- Sel].

%% Rows most likely to collide first.
order_rows(Rows) when length(Rows) =< 3 ->
    Rows;
order_rows(Rows) ->
    First = hd(Rows),
    Last = lists:last(Rows),
    Mid = lists:nth(length(Rows) div 2 + 1, Rows),
    [Mid, First, Last | (Rows -- [Mid, First, Last])].

%% Dilation by G cells; the result is offset by G in rows and bits.
dilate(Rows, 0) ->
    Rows;
dilate(Rows, G) ->
    W = 2 * G + 1,
    Hmap = maps:from_list([{R, spread(M, W)} || {R, M} <- Rows]),
    {R0, _} = hd(Rows),
    {R1, _} = lists:last(Rows),
    lists:filtermap(
      fun(Rp) ->
              M = lists:foldl(
                    fun(R, Acc) -> Acc bor maps:get(R, Hmap, 0) end,
                    0, lists:seq(max(R0, Rp - 2 * G), min(R1, Rp))),
              case M of
                  0 -> false;
                  _ -> {true, {Rp, M}}
              end
      end, lists:seq(R0, R1 + 2 * G)).

%% Marks Rows of a chart placed at cell shift {X, Y}.
occ_add(Rows, X, Y, Occ, N) ->
    Full = (1 bsl N) - 1,
    lists:foldl(
      fun({R, M}, Acc) ->
              Yy = Y + R,
              case Yy >= 0 andalso Yy < N of
                  true ->
                      setelement(Yy + 1, Acc,
                                 element(Yy + 1, Acc) bor ((M bsl X) band Full));
                  false ->
                      Acc
              end
      end, Occ, Rows).

%% Marks a polygon at its tile position.
occ_add_global(Poly, Occ, #{rect := {RX0, RY0, _, _}, c := C, n := N}) ->
    occ_add(raster(Poly, RX0, RY0, C, N), 0, 0, Occ, N).

%%%
%%% Rasterization
%%%

%% Conservative raster of the region on cells of side C from {OX, OY}:
%% [{Row, Mask}] clipped to the N x N grid; holes stay free.
raster(#{outer := Outer, holes := Holes}, OX, OY, C, N) ->
    Segs = lists:append([loop_segments(L) || L <- [Outer | Holes]]),
    {Bd, Cr} = lists:foldl(
                 fun(S, Acc) -> raster_edge(S, OX, OY, C, N, Acc) end,
                 {#{}, #{}}, Segs),
    Rows = lists:usort(maps:keys(Bd) ++ maps:keys(Cr)),
    lists:filtermap(
      fun(J) ->
              M0 = maps:get(J, Bd, 0),
              M = span_mask(lists:sort(maps:get(J, Cr, [])), OX, C, N, M0),
              case M of
                  0 -> false;
                  _ -> {true, {J, M}}
              end
      end, Rows).

loop_segments(Pts) ->
    lists:zip(Pts, tl(Pts) ++ [hd(Pts)]).

cell_lo(V) -> floor(V + ?CEPS).
cell_hi(V) -> ceil(V - ?CEPS) - 1.

range_mask(I0, I1) ->
    ((1 bsl (I1 - I0 + 1)) - 1) bsl I0.

span_mask([A, B | Rest], OX, C, N, Mask) ->
    I0 = max(0, cell_lo((A - OX) / C)),
    I1 = min(N - 1, cell_hi((B - OX) / C)),
    Mask1 = case I0 =< I1 of
                true -> Mask bor range_mask(I0, I1);
                false -> Mask
            end,
    span_mask(Rest, OX, C, N, Mask1);
span_mask(_, _OX, _C, _N, Mask) ->
    Mask.

raster_edge({{X1, Y1}, {X2, Y2}}, OX, OY, C, N, {Bd, Cr}) ->
    YMin = min(Y1, Y2),
    YMax = max(Y1, Y2),
    J0 = max(0, cell_lo((YMin - OY) / C)),
    J1 = min(N - 1, cell_hi((YMax - OY) / C)),
    raster_rows(J0, J1, {X1, Y1, X2, Y2}, {YMin, YMax}, {OX, OY, C, N},
                Bd, Cr).

raster_rows(J, J1, _E, _YR, _G, Bd, Cr) when J > J1 ->
    {Bd, Cr};
raster_rows(J, J1, {X1, Y1, X2, Y2} = E, {YMin, YMax} = YR,
            {OX, OY, C, N} = G, Bd, Cr) ->
    YA = max(YMin, OY + J * C),
    YB = min(YMax, OY + (J + 1) * C),
    {XA, XB} = case Y1 == Y2 of
                   true -> {X1, X2};
                   false -> {x_at(YA, X1, Y1, X2, Y2),
                             x_at(YB, X1, Y1, X2, Y2)}
               end,
    I0 = max(0, cell_lo((min(XA, XB) - OX) / C)),
    I1 = min(N - 1, cell_hi((max(XA, XB) - OX) / C)),
    Bd1 = case I0 =< I1 of
              true -> Bd#{J => maps:get(J, Bd, 0) bor range_mask(I0, I1)};
              false -> Bd
          end,
    YC = OY + (J + 0.5) * C,
    Cr1 = case (Y1 > YC) =/= (Y2 > YC) of
              true -> Cr#{J => [x_at(YC, X1, Y1, X2, Y2)
                                | maps:get(J, Cr, [])]};
              false -> Cr
          end,
    raster_rows(J + 1, J1, E, YR, G, Bd1, Cr1).

x_at(Y, X1, Y1, X2, Y2) ->
    X1 + (Y - Y1) * (X2 - X1) / (Y2 - Y1).

%%%
%%% Exact verification
%%%

%% Exact test against the obstacles whose bbox is near.
fits(P0, X, Y, W, H, Obstacles, Gap) ->
    Box = {X, Y, X + W, Y + H},
    case [O || O <- Obstacles, box_near(Box, maps:get(bbox, O), Gap)] of
        [] ->
            true;
        Near ->
            Placed = auv_poly:translate(P0, X, Y),
            not lists:any(fun(O) -> auv_poly:collide(Placed, O, Gap) end, Near)
    end.

%% Same bbox filter as auv_poly:collide/3.
box_near({AX0, AY0, AX1, AY1}, {BX0, BY0, BX1, BY1}, Gap) ->
    AX0 < BX1 + Gap andalso AX1 > BX0 - Gap andalso
    AY0 < BY1 + Gap andalso AY1 > BY0 - Gap.

%%%
%%% Result
%%%

make_result(Placements, Unpacked, Critical, Movable, Fixed, Used, Side) ->
    AreaOf = fun(Id) ->
                     {_, Poly} = lists:keyfind(Id, 1, Movable),
                     maps:get(area, Poly)
             end,
    PackedArea = lists:sum([AreaOf(maps:get(id, P)) || P <- Placements]),
    #{placements => Placements,
      unpacked => Unpacked,
      critical => Critical,
      stats => #{movable => length(Movable),
                 fixed => length(Fixed),
                 packed => length(Placements),
                 unpacked => length(Unpacked),
                 movable_area => lists:sum([maps:get(area, P)
                                            || {_, P} <- Movable]),
                 packed_area => PackedArea,
                 used_height => Used,
                 %% packed area / (full-width strip up to the highest top)
                 efficiency => case Used > ?EPS of
                                   true -> PackedArea / (Used * Side);
                                   false -> 0.0
                               end}}.

%%%
%%%  Wings3D adapter
%%%

%% pack_charts(Charts, Movable, Options) -> Result | {error, Reason}
%% Result = #{placed, unpacked, critical, stats}; charts come out transformed.
%% Fixed charts without outline block their bounding box.
%% With scale, charts are enlarged or reduced to fill the tile.
pack_charts(Charts, Movable, Options) ->
    auv_pack_utils:fit_scale(
      fun(Cs) -> pack_charts_1(Cs, Movable, Options) end,
      Charts, Movable, Options).

pack_charts_1(Charts, Movable, Options) ->
    Contours = auv_pack_utils:extract_polygons(Charts),
    Vtabs = maps:from_list([{We#we.id, We#we.vp} || We <- Charts]),
    {Items0, Degenerate} = to_items(Contours, Vtabs, Movable),
    Items = Items0 ++ bbox_obstacles(Charts, Movable, Items0),
    case pack(Items, Options) of
        {error, _} = Error ->
            Error;
        #{placements := Placements, unpacked := Unpacked0,
          critical := Critical, stats := Stats0} ->
            Wes = maps:from_list([{We#we.id, We} || We <- Charts]),
            Placed = [{Id, auv_pack_utils:transform_chart(
                             maps:get(Id, Wes), Rotation, Delta)}
                      || #{id := Id, rotation := Rotation, delta := Delta}
                             <- Placements],
            Unpacked = Unpacked0 ++ Degenerate,
            #{placed => Placed,
              unpacked => Unpacked,
              critical => Critical,
              degenerate => Degenerate,
              stats => Stats0#{unpacked => length(Unpacked)}}
    end.

bbox_obstacles(Charts, Movable, Items) ->
    Have = [Id || #{id := Id} <- Items],
    lists:filtermap(
      fun(#we{id = Id} = We) ->
              case auv_pack_utils:is_movable(Id, Movable)
                  orelse lists:member(Id, Have) of
                  true ->
                      false;
                  false ->
                      case auv_pack_utils:chart_bbox(We) of
                          degenerate ->
                              false;
                          {X, Y, W, H} ->
                              Box = [{X, Y}, {X + W, Y},
                                     {X + W, Y + H}, {X, Y + H}],
                              case auv_poly:new(Box, []) of
                                  #{} = Poly ->
                                      {true, #{id => Id, poly => Poly,
                                               fixed => true}};
                                  {error, _} ->
                                      false
                              end
                      end
              end
      end, Charts).
