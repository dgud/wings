%%
%%  auv_poly.erl
%%
%%      Polygon geometry for UV island packing.
%%
%%  Copyright (c) 2026 Micheus Vieira (micheus)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

%% Pure geometry. A polygon is #{outer := [{U,V}], holes := [[{U,V}]]};
%% new/2 normalizes it (outer CCW, holes CW, no repeated end point) and adds
%% area and bbox := {X0,Y0,X1,Y1}.
%%
%% collide/3: with Gap =< EPS touching is allowed and only overlap counts;
%% with Gap > EPS a minimum outline distance below Gap collides. A region
%% inside another (and not in a hole) always collides.

-module(auv_poly).

-export([new/2, rotate/2, translate/3, collide/3]).

-define(BOXPAD, 1.0e-6).
-define(EPS, 1.0e-9).
-define(PROBE, 1.0e-7).

%%%
%%% Construction
%%%

%% new(Outer, Holes) -> Poly | {error, Reason}
new(Outer0, Holes0) ->
    Outer1 = clean(Outer0),
    Holes1 = [H || H0 <- Holes0, H <- [clean(H0)], H =/= []],
    case Outer1 of
        [] ->
            {error, degenerate_outer};
        _ ->
            Outer = orient(Outer1, ccw),
            Holes = [orient(H, cw) || H <- Holes1],
            Poly = #{outer => Outer, holes => Holes},
            Poly#{area => area(Poly), bbox => bbox(Poly)}
    end.

%% Drops repeated points; loops with fewer than 3 points or no area are dropped.
clean(Pts0) ->
    Pts1 = dedup(Pts0),
    Pts = case Pts1 of
              [F | _] = L when length(L) > 1 ->
                  case near(F, lists:last(L)) of
                      true -> lists:droplast(L);
                      false -> L
                  end;
              L -> L
          end,
    case length(Pts) >= 3 andalso abs(signed_area(Pts)) > ?EPS of
        true -> Pts;
        false -> []
    end.

dedup([A, B | R]) ->
    case near(A, B) of
        true -> dedup([A | R]);
        false -> [A | dedup([B | R])]
    end;
dedup(L) -> L.

near({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) =< ?EPS andalso abs(Y1 - Y2) =< ?EPS.

orient(Pts, Want) ->
    Is = case signed_area(Pts) > 0.0 of true -> ccw; false -> cw end,
    case Is =:= Want of
        true -> Pts;
        false -> lists:reverse(Pts)
    end.

%% Shoelace; positive = CCW.
signed_area(Pts) ->
    Next = tl(Pts) ++ [hd(Pts)],
    lists:sum([X1 * Y2 - X2 * Y1
               || {{X1, Y1}, {X2, Y2}} <- lists:zip(Pts, Next)]) / 2.0.

%% Region area (outer minus holes).
area(#{outer := O, holes := Hs}) ->
    signed_area(O) + lists:sum([signed_area(H) || H <- Hs]).

bbox(#{outer := O}) ->
    Xs = [X || {X, _} <- O],
    Ys = [Y || {_, Y} <- O],
    {lists:min(Xs), lists:min(Ys), lists:max(Xs), lists:max(Ys)}.

%%%
%%% Transformations
%%%

%% Counterclockwise rotation (degrees) around the bbox center.
rotate(#{bbox := {X0, Y0, X1, Y1}} = P, Deg) ->
    transform(P, Deg, {(X0 + X1) / 2.0, (Y0 + Y1) / 2.0}).

%% Rotation by Deg degrees around Center.
transform(P, +0.0, _) -> P;
transform(#{outer := O, holes := Hs}, Deg, {CX, CY}) ->
    R = Deg * math:pi() / 180.0,
    C = math:cos(R), S = math:sin(R),
    F = fun({X, Y}) ->
                DX = X - CX, DY = Y - CY,
                {CX + DX * C - DY * S, CY + DX * S + DY * C}
        end,
    rebuild([F(P) || P <- O], [[F(Q) || Q <- H] || H <- Hs]).

translate(#{outer := O, holes := Hs}, Dx, Dy) ->
    F = fun({X, Y}) -> {X + Dx, Y + Dy} end,
    rebuild([F(P) || P <- O], [[F(Q) || Q <- H] || H <- Hs]).

%% Orientation is preserved; only area and bbox are recomputed.
rebuild(O, Hs) ->
    P = #{outer => O, holes => Hs},
    P#{area => area(P), bbox => bbox(P)}.

%%%
%%% Collision
%%%

collide(#{bbox := BA} = A, #{bbox := BB} = B, Gap) ->
    G = max(Gap, 0.0),
    case bbox_overlap(BA, BB, G) of
        false -> false;
        true ->
            %% Only segments near the other bbox matter.
            M = G + ?BOXPAD,
            SA = in_box(segments(A), BB, M),
            SB = in_box(segments(B), BA, M),
            case G > ?EPS of
                true -> close_segments(SA, SB, G - ?EPS)
                            orelse probe_inside(SA, B) orelse probe_inside(SB, A);
                false -> cross_segments(SA, SB)
                             orelse probe_inside(SA, B) orelse probe_inside(SB, A)
            end
    end.

bbox_overlap({AX0, AY0, AX1, AY1}, {BX0, BY0, BX1, BY1}, G) ->
    AX0 < BX1 + G andalso AX1 > BX0 - G andalso
    AY0 < BY1 + G andalso AY1 > BY0 - G.

in_box(Segs, {X0, Y0, X1, Y1}, M) ->
    [S || {{AX, AY}, {BX, BY}} = S <- Segs,
          max(AX, BX) >= X0 - M, min(AX, BX) =< X1 + M,
          max(AY, BY) >= Y0 - M, min(AY, BY) =< Y1 + M].

%% Directed edges of all loops.
segments(#{outer := O, holes := Hs}) ->
    lists:append([loop_segments(L) || L <- [O | Hs]]).

loop_segments(Pts) ->
    lists:zip(Pts, tl(Pts) ++ [hd(Pts)]).

seg_box({{AX, AY}, {BX, BY}}) ->
    {min(AX, BX), min(AY, BY), max(AX, BX), max(AY, BY)}.

%% True when a segment pair is closer than Dist (boxes prefilter).
close_segments(SA, SB, Dist) ->
    Boxed = [begin {X0, Y0, X1, Y1} = seg_box(S),
                   {X0 - Dist, Y0 - Dist, X1 + Dist, Y1 + Dist, S}
             end || S <- SB],
    lists:any(fun(S1) ->
                      {X0, Y0, X1, Y1} = seg_box(S1),
                      lists:any(
                        fun({BX0, BY0, BX1, BY1, S2}) ->
                                X0 =< BX1 andalso X1 >= BX0 andalso
                                Y0 =< BY1 andalso Y1 >= BY0 andalso
                                seg_dist(S1, S2) < Dist
                        end, Boxed)
              end, SA).

cross_segments(SA, SB) ->
    Boxed = [begin {X0, Y0, X1, Y1} = seg_box(S), {X0, Y0, X1, Y1, S} end
             || S <- SB],
    lists:any(fun(S1) ->
                      {X0, Y0, X1, Y1} = seg_box(S1),
                      lists:any(
                        fun({BX0, BY0, BX1, BY1, S2}) ->
                                X0 =< BX1 andalso X1 >= BX0 andalso
                                Y0 =< BY1 andalso Y1 >= BY0 andalso
                                proper_cross(S1, S2)
                        end, Boxed)
              end, SA).

%% Probe points just inside each edge; detects containment and coincident outlines.
probe_inside(Segs, Poly) ->
    Idx = index(Poly),
    lists:any(fun({{X1, Y1}, {X2, Y2}}) ->
                      DX = X2 - X1, DY = Y2 - Y1,
                      L = math:sqrt(DX * DX + DY * DY),
                      case L > ?EPS of
                          false -> false;
                          true ->
                              P = {(X1 + X2) / 2.0 - DY / L * ?PROBE,
                                   (Y1 + Y2) / 2.0 + DX / L * ?PROBE},
                              in_index(P, Idx)
                      end
              end, Segs).

%% Y-band index of the segments, so a point only checks its band.
-define(BINS, 32).
index(#{bbox := {_, Y0, _, Y1}} = Poly) ->
    H = (Y1 - Y0) / ?BINS,
    Bin = fun(Y) when H > 0.0 -> max(0, min(?BINS - 1, floor((Y - Y0) / H)));
             (_) -> 0
          end,
    Map = lists:foldl(
            fun({{_, YA}, {_, YB}} = S, M) ->
                    lists:foldl(fun(K, M1) -> M1#{K => [S | maps:get(K, M1, [])]} end,
                                M, lists:seq(Bin(min(YA, YB)), Bin(max(YA, YB))))
            end, #{}, segments(Poly)),
    {Map, Bin}.

in_index({PX, PY}, {Map, Bin}) ->
    Hits = lists:foldl(
             fun({{X1, Y1}, {X2, Y2}}, N) ->
                     case (Y1 > PY) =/= (Y2 > PY) of
                         true ->
                             XI = X1 + (PY - Y1) * (X2 - X1) / (Y2 - Y1),
                             case XI > PX of true -> N + 1; false -> N end;
                         false -> N
                     end
             end, 0, maps:get(Bin(PY), Map, [])),
    Hits rem 2 =:= 1.

%%%
%%% Segments
%%%

cross2({X1, Y1}, {X2, Y2}) -> X1 * Y2 - Y1 * X2.
sub({X1, Y1}, {X2, Y2}) -> {X1 - X2, Y1 - Y2}.

%% Strict crossing; touching does not count.
proper_cross({A, B}, {C, D}) ->
    D1 = cross2(sub(B, A), sub(C, A)),
    D2 = cross2(sub(B, A), sub(D, A)),
    D3 = cross2(sub(D, C), sub(A, C)),
    D4 = cross2(sub(D, C), sub(B, C)),
    sgn(D1) * sgn(D2) < 0 andalso sgn(D3) * sgn(D4) < 0.

sgn(V) when V > ?EPS -> 1;
sgn(V) when V < -?EPS -> -1;
sgn(_) -> 0.

seg_dist({A, B} = S1, {C, D} = S2) ->
    case proper_cross(S1, S2) of
        true -> 0.0;
        false ->
            lists:min([pt_seg(A, S2), pt_seg(B, S2),
                       pt_seg(C, S1), pt_seg(D, S1)])
    end.

pt_seg({PX, PY}, {{AX, AY}, {BX, BY}}) ->
    DX = BX - AX, DY = BY - AY,
    L2 = DX * DX + DY * DY,
    T = case L2 > ?EPS of
            true -> max(0.0, min(1.0, ((PX - AX) * DX + (PY - AY) * DY) / L2));
            false -> 0.0
        end,
    QX = AX + T * DX, QY = AY + T * DY,
    math:sqrt((PX - QX) * (PX - QX) + (PY - QY) * (PY - QY)).
