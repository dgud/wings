%%
%%  prm_uv_unfold.erl --
%%
%%     Unfolds the walls of a primitive into UV islands.
%%
%%  Copyright (c) 2026 Micheus Vieira
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(prm_uv_unfold).
-export([create_uv_dlg/0,layout/3,lengths/4,cap_radius/2,cap_face/3,cross_scale/1,cross/4]).

-include_lib("wings/src/wings.hrl").

%%%
%%% The walls of a primitive are a grid of M columns around a closed loop (K)
%%% and L levels (N), the levels being a closed loop too when Closed is true.
%%% Pos(K, N) gives the position of a grid point and Cell(V) gives the {K, N}
%%% of a vertex index. The walls are unfolded into strips keeping the real
%%% lengths, the flat parts of the walls and the caps are projected on the XZ
%%% plane and placed above them, all with the same scale.
%%%
%%% The other exported functions are shared by the primitives that have their
%%% own layout: lengths/4 measures a grid for a proportional strip,
%%% cap_radius/2 and cap_face/3 draw a cap as a regular polygon with the area
%%% of the real one, and cross_scale/1 and cross/4 unfold a box like an opened
%%% box (a cross with the front in the middle). create_uv_dlg/0 is the dialog
%%% item that lets the user choose whether the UV coordinates are created.
%%%

create_uv_dlg() ->
    {?__(1,"Create UV coordinates"),wings_pref:get_value(primitive_uv, false),[{key,create_uv}]}.

layout({Pos, M, L, Closed, Cell, Walls}, Caps, Vt) ->
    Parts = parts(Pos, M, L, Closed, Cell, Walls),
    Strips = [strip(Part, M) || {strip, Part} <- Parts],
    Flats = [project([F], Vt) || F <- Caps] ++ [project(Fs, Vt) || {flat, Fs} <- Parts],
    StripH = lists:sum([H || {_,_,_,H} <- Strips]),
    StripW = lists:max([0.0 | [W || {_,_,W,_} <- Strips]]),
    FlatW = lists:sum([W || {_,_,W,_} <- Flats]),
    FlatH = lists:max([0.0 | [H || {_,_,_,H} <- Flats]]),
    Big = lists:max([StripW, StripH, FlatW, FlatH]),
    Gap = 0.02 * Big,
    NStrips = length(Strips),
    NFlats = length(Flats),
    TotW = max(StripW, FlatW + Gap*NFlats),
    TotH = StripH + Gap*NStrips + FlatH,
    Scale = 1.0 / max(TotW, TotH),
    {StripUvs, Y0} =
        lists:mapfoldl(fun({Fs, Uvs, _, H}, Y) ->
                           {place(Fs, Uvs, 0.0, Y, Scale), Y+H+Gap}
                       end, 0.0, Strips),
    {FlatUvs, _} =
        lists:mapfoldl(fun({Fs, Uvs, W, _}, X) ->
                           {place(Fs, Uvs, X, Y0, Scale), X+W+Gap}
                       end, 0.0, Flats),
    Map = maps:from_list(lists:append(StripUvs ++ FlatUvs)),
    Face = fun(F) -> {default,F,maps:get(F, Map),none,none} end,
    {[Face(F) || F <- Caps], [Face(F) || F <- Walls]}.

place(Fs, Uvs, X, Y, Scale) ->
    [{F, [{(X+U)*Scale, (Y+V)*Scale} || {U,V} <- Uv]} || {F,Uv} <- lists:zip(Fs, Uvs)].

%% The wall is split at its flat segments (a segment is the strip between two
%% levels): the flat ones are projected and each run of the others is a strip.
parts(Pos, M, L, Closed, Cell, Walls) ->
    Flat = [flat(Pos, M, N, (N+1) rem L) || N <- lists:seq(0, L-1)],
    Mixed = lists:member(true, Flat) andalso lists:member(false, Flat),
    case Closed andalso Mixed of
        true -> split(Pos, L, Cell, Walls, list_to_tuple(Flat));
        false -> [{strip, {Pos, L, Cell, Walls}}]
    end.

flat(Pos, M, N0, N1) ->
    Dys = [abs(element(2, Pos(I, N0)) - element(2, Pos(I, N1))) || I <- lists:seq(0, M-1)],
    lists:max(Dys) < ?EPSILON.

split(Pos, L, Cell, Walls, Types) ->
    Type = fun(N) -> element(N+1, Types) end,
    [S0|_] = [N || N <- lists:seq(0, L-1), Type(N) =/= Type((N-1+L) rem L)],
    Order = [(S0+J) rem L || J <- lists:seq(0, L-1)],
    Tagged = [{index(F, Cell, L, 2), F} || F <- Walls],
    [part(Run, Pos, L, Cell, Tagged) || Run <- runs([{Type(N), N} || N <- Order])].

part({Flat, Segs}, Pos, L, Cell, Tagged) ->
    Fs = [F || {S,F} <- Tagged, lists:member(S, Segs)],
    case Flat of
        true ->
            {flat, Fs};
        false ->
            A = hd(Segs),
            Pos1 = fun(I, N) -> Pos(I, (N+A) rem L) end,
            Cell1 =
                fun(V) ->
                    {K, N} = Cell(V),
                    {K, (N-A+L) rem L}
                end,
            {strip, {Pos1, length(Segs)+1, Cell1, Fs}}
    end.

%% Groups the consecutive elements with the same key.
runs([]) -> [];
runs([{Key,X}|Rest]) ->
    {Same, Others} = lists:splitwith(fun({K,_}) -> K =:= Key end, Rest),
    [{Key, [X | [Y || {_,Y} <- Same]]} | runs(Others)].

%% The column (Which = 1) or the segment (Which = 2) of a face, the one at the
%% seam being the last.
index(F, Cell, Max, Which) ->
    Is = lists:usort([element(Which, Cell(V)) || V <- F]),
    case Is of
        [0,Last] when Last =:= Max-1, Max > 2 -> Last;
        [First|_] -> First
    end.

%% A strip of two levels is unfolded face by face, the others have a band of
%% vertical segments with the faces of the rest hanging from it.
strip({Pos, 2, Cell, Fs}, M) ->
    chain(Pos, M, Cell, Fs);
strip({Pos, L, Cell, Fs}, M) ->
    flaps(Pos, M, L, Cell, Fs, wall_band(Pos, M, L)).

chain(Pos, M, Cell, Fs) ->
    H0 = e3d_vec:dist(Pos(0, 0), Pos(0, 1)),
    Cols = list_to_tuple(cols(0, M, Pos, {{0.0,H0}, {0.0,0.0}})),
    Pt = fun(K, N) -> element(N+1, element(K+1, Cols)) end,
    island(Fs, [wall(F, Cell, M, Pt) || F <- Fs]).

cols(M, M, _, Col) -> [Col];
cols(K, M, Pos, {A2,D2}=Col) ->
    Quad = {Pos(K,1), Pos(K,0), Pos(K+1,0), Pos(K+1,1)},
    {B2, C2} = attach(Quad, {D2,A2}, -1.0),
    [Col | cols(K+1, M, Pos, {B2,C2})].

%% The band is the run of segments between rings with the same length (the
%% vertical wall), or the first segment when there is none.
wall_band(Pos, M, L) ->
    Tots = list_to_tuple([element(M+1, ring(Pos, M, N)) || N <- lists:seq(0, L-1)]),
    Same =
        fun(N) ->
            abs(element(N+1, Tots) - element(N+2, Tots)) < ?EPSILON
        end,
    Runs = runs([{Same(N), N} || N <- lists:seq(0, L-2)]),
    Bands = [Segs || {true, Segs} <- Runs],
    case Bands of
        [] -> {0, 0};
        [Segs|_] -> {hd(Segs), lists:last(Segs)}
    end.

%% The faces of the bevel are unfolded from the band, column by column, so they
%% are not connected to each other. The columns whose bevel gets wider than the
%% wall would overlap their neighbors, so each of them is cut as an island of
%% its own and the islands are placed side by side with a gap between them.
flaps(Pos, M, L, Cell, Fs, {A, B}) ->
    Tagged = [{index(F, Cell, L, 2), F} || F <- Fs],
    BandFs = [F || {S,F} <- Tagged, S >= A, S =< B],
    FlapFs = [F || {S,F} <- Tagged, (S < A) orelse (S > B)],
    Pt = band_points(Pos, M, A, B),
    Cols = list_to_tuple([col(Pos, K, Pt, {A, B, L}) || K <- lists:seq(0, M-1)]),
    Items = [{index(F, Cell, M, 1), F, wall(F, Cell, M, Pt)} || F <- BandFs] ++
        [{index(F, Cell, M, 1), F, flap_face(F, Cell, M, Cols)} || F <- FlapFs],
    Groups = runs([{group(Pos, K, {A, B, L}), K} || K <- lists:seq(0, M-1)]),
    ChkIsland =
        fun(Ks) ->
            IslFs = [F || {C,F,_} <- Items, lists:member(C, Ks)],
            IslUvs = [Uv || {C,_,Uv} <- Items, lists:member(C, Ks)],
            island(IslFs, IslUvs)
        end,
    Islands = [ChkIsland(Ks) || {_, Ks} <- Groups],
    H = lists:max([Hi || {_,_,_,Hi} <- Islands]),
    Gap = 0.1 * H,
    {Placed, W} =
        lists:mapfoldl(fun({Fs1, Uvs, Wi, _}, X) ->
                           {{Fs1, [[{U+X,V} || {U,V} <- Uv] || Uv <- Uvs]}, X+Wi+Gap}
                       end, 0.0, Islands),
    {lists:append([Fs1 || {Fs1,_} <- Placed]), lists:append([Uvs || {_,Uvs} <- Placed]), W-Gap, H}.

band_points(Pos, M, A, B) ->
    Rings = list_to_tuple([ring(Pos, M, N) || N <- lists:seq(A, B+1)]),
    Dvs = [avg_dist(Pos, M, N, N+1) || N <- lists:seq(A, B)],
    {VStarts, H} =
        lists:mapfoldl(fun(D, Acc) ->
                           {Acc, Acc+D}
                       end, 0.0, Dvs),
    CumV = list_to_tuple(VStarts ++ [H]),
    fun(K, N) ->
        {element(K+1, element(N-A+1, Rings)), element(N-A+1, CumV)}
    end.

%% A column whose bevel is wider at some level than at the wall is a group of
%% its own, the other ones are grouped in runs.
group(Pos, K, {A, B, L}) ->
    Len = fun(N) -> e3d_vec:dist(Pos(K, N), Pos(K+1, N)) end,
    Wall = Len(A),
    Levels = lists:seq(0, A-1) ++ lists:seq(B+2, L-1),
    Widen =
        lists:any(fun(N) ->
                      Len(N) > Wall + ?EPSILON
                  end, Levels),
    case Widen of
        true -> {widen, K};
        false -> wall
    end.

%% The points of the two sides of the column K for the levels above and below
%% the band, keyed by {Side, Level}.
col(Pos, K, Pt, {A, B, L}) ->
    Top = flap(Pos, {Pt(K,A), Pt(K+1,A)}, {K, -1.0}, A, lists:seq(A-1, 0, -1)),
    Bot = flap(Pos, {Pt(K,B+1), Pt(K+1,B+1)}, {K, 1.0}, B+1, lists:seq(B+2, L-1)),
    maps:from_list(Top ++ Bot).

flap(Pos, {P2,Q2}, {K,Hand}, Base, Levels) ->
    Start = [{{0,Base}, P2}, {{1,Base}, Q2}],
    {Pts, _} =
        lists:foldl(fun(N, {Acc, {Pp,Qp,Np}}) ->
                        Quad = {Pos(K,Np), Pos(K+1,Np), Pos(K+1,N), Pos(K,N)},
                        {R2, S2} = attach(Quad, {Pp,Qp}, Hand),
                        {[{{1,N}, R2}, {{0,N}, S2} | Acc], {S2,R2,N}}
                    end, {Start, {P2,Q2,Base}}, Levels),
    Pts.

flap_face(F, Cell, M, Cols) ->
    Col = index(F, Cell, M, 1),
    Map = element(Col+1, Cols),
    [maps:get({side(K, Col), N}, Map) || V <- F, {K,N} <- [Cell(V)]].

side(Col, Col) -> 0;
side(_, _) -> 1.

%% Unfolds the quad {P,Q,R,S} with P and Q already placed in the plane at
%% {P2,Q2}, as two triangles. Hand is 1.0 when the quad turns counterclockwise
%% and -1.0 when it turns clockwise.
attach({P,Q,R,S}, {P2,Q2}, Hand) ->
    R2 = tri(P2, Q2, e3d_vec:dist(P, R), e3d_vec:dist(Q, R), Hand),
    S2 = tri(P2, R2, e3d_vec:dist(P, S), e3d_vec:dist(R, S), Hand),
    {R2, S2}.

tri({Px,Py}, {Qx,Qy}, RP, RQ, Side) ->
    Dx = Qx - Px,
    Dy = Qy - Py,
    L = math:sqrt(Dx*Dx + Dy*Dy),
    case L < ?EPSILON of
        true ->
            {Px, Py};
        false ->
            Ex = Dx / L,
            Ey = Dy / L,
            X = (RP*RP - RQ*RQ + L*L) / (2*L),
            Y = math:sqrt(max(RP*RP - X*X, 0.0)),
            {Px + Ex*X - Ey*Y*Side, Py + Ey*X + Ex*Y*Side}
    end.

project(Fs, Vt) ->
    island(Fs, [[{X,-Z} || V <- F, {X,_,Z} <- [element(V+1, Vt)]] || F <- Fs]).

%% Moves the island to the origin and mirrors it when it is turned over.
island(Fs, Uvs0) ->
    All = lists:append(Uvs0),
    UMin = lists:min([U || {U,_} <- All]),
    VMin = lists:min([V || {_,V} <- All]),
    UMax = lists:max([U || {U,_} <- All]),
    VMax = lists:max([V || {_,V} <- All]),
    W = UMax - UMin,
    Uvs = [[{U-UMin, V-VMin} || {U,V} <- Uv] || Uv <- Uvs0],
    Areas = [area(Uv) || Uv <- Uvs],
    AreaSum = lists:sum(Areas),
    case AreaSum < 0 of
        true -> {Fs, [[{W-U,V} || {U,V} <- Uv] || Uv <- Uvs], W, VMax-VMin};
        false -> {Fs, Uvs, W, VMax-VMin}
    end.

%% lengths(Pos, M, L, Closed) -> {Cum, RingLen}
%%  Cum is a tuple with the cumulative average distance between the levels
%%  (L+1 entries when the levels are a closed loop, L when they are not) and
%%  RingLen is the average length of the rings.
lengths(Pos, M, L, Closed) ->
    NSegs = case Closed of
                true -> L;
                false -> L-1
            end,
    Dvs = [avg_dist(Pos, M, N, (N+1) rem L) || N <- lists:seq(0, NSegs-1)],
    {Starts, Total} =
        lists:mapfoldl(fun(D, Acc) ->
                           {Acc, Acc+D}
                       end, 0.0, Dvs),
    Rings = [element(M+1, ring(Pos, M, N)) || N <- lists:seq(0, L-1)],
    RingSum = lists:sum(Rings),
    {list_to_tuple(Starts ++ [Total]), RingSum / L}.

cap_radius(Ps, Factor) ->
    N = length(Ps),
    Pairs = lists:zip(Ps, tl(Ps) ++ [hd(Ps)]),
    Sum =
        lists:foldl(fun({A,B}, Acc) ->
                        e3d_vec:add(Acc, e3d_vec:cross(A, B))
                    end, {0.0,0.0,0.0}, Pairs),
    Area = e3d_vec:len(Sum) / 2,
    RegArea = (N/2) * math:sin(2*math:pi()/N),
    math:sqrt(Area*Factor/RegArea).

cap_face(Idx, {Cx,Cy}, R) ->
    N = length(Idx),
    Delta = math:pi()*2/N,
    UVs = [{Cx+R*math:cos(I*Delta), Cy+R*math:sin(I*Delta)} || I <- lists:seq(0, N-1)],
    {default,Idx,UVs,none,none}.

cross_scale({Sx,Sy,Sz}) ->
    1.0 / max(max(4*Sz + 4*Sx, 2*Sy + 4*Sz), ?EPSILON).

cross(Side, Pt, Sizes, Scale) ->
    {U, V} = cross_uv(Side, Pt, Sizes),
    {U*Scale, V*Scale}.

cross_uv(front, {X,Y,_}, {Sx,Sy,Sz}) -> {2*Sz+(X+1)*Sx, 2*Sz+(Y+1)*Sy};
cross_uv(left, {_,Y,Z}, {_,Sy,Sz}) -> {(Z+1)*Sz, 2*Sz+(Y+1)*Sy};
cross_uv(right, {_,Y,Z}, {Sx,Sy,Sz}) -> {2*Sz+2*Sx+(1-Z)*Sz, 2*Sz+(Y+1)*Sy};
cross_uv(back, {X,Y,_}, {Sx,Sy,Sz}) -> {4*Sz+2*Sx+(1-X)*Sx, 2*Sz+(Y+1)*Sy};
cross_uv(top, {X,_,Z}, {Sx,Sy,Sz}) -> {2*Sz+(X+1)*Sx, 2*Sz+2*Sy+(1-Z)*Sz};
cross_uv(bottom, {X,_,Z}, {Sx,_,Sz}) -> {2*Sz+(X+1)*Sx, (Z+1)*Sz}.

ring(Pos, M, N) ->
    Ds = [e3d_vec:dist(Pos(I, N), Pos(I+1, N)) || I <- lists:seq(0, M-1)],
    {Starts, Total} =
        lists:mapfoldl(fun(D, Acc) ->
                           {Acc, Acc+D}
                       end, 0.0, Ds),
    list_to_tuple(Starts ++ [Total]).

avg_dist(Pos, M, N0, N1) ->
    Ds = [e3d_vec:dist(Pos(I, N0), Pos(I, N1)) || I <- lists:seq(0, M-1)],
    DSum = lists:sum(Ds),
    DSum / M.

wall(F, Cell, M, Pt) ->
    Cells = [Cell(V) || V <- F],
    Ks = [K || {K,_} <- Cells],
    Seam = lists:member(0, Ks) andalso lists:member(M-1, Ks),
    [Pt(wrap(K, M, Seam), N) || {K,N} <- Cells].

wrap(0, M, true) -> M;
wrap(K, _, _) -> K.

area(Uv) ->
    Pairs = lists:zip(Uv, tl(Uv) ++ [hd(Uv)]),
    Terms = [U1*V2 - U2*V1 || {{U1,V1},{U2,V2}} <- Pairs],
    TermSum = lists:sum(Terms),
    TermSum / 2.
