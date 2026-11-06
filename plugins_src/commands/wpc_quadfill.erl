%%
%%  wpc_quadfill.erl --
%%
%%     Fill an n-gon face (typically the face you get after
%%     "Remove hole") with a grid of quad faces.
%%
%%  Copyright (c) 2026 Micheus Vieira
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(wpc_quadfill).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").

-define(EPSILON0, 1.0e-9).
-define(EPSILON1, 1.0e-12).
-define(DEFAULT_CURVATURE, 100).
-define(TANGENT_GAIN, 0.4).
-define(DEFAULT_RELAX, 0).
-define(RELAX_OMEGA, 1.5).
-define(RELAX_ITER, 3).
-define(HALF_PI, 1.5707963267948966).
-define(AUTO_CANDIDATES, 64).
-define(DEFAULT_ANGLE, 60).
-define(CUT_DEPTH, 12).
-define(CUT_PENALTY, 0.15).
-define(DEEP_TOP, 6).
-define(STAR_BIAS, 0.6).
-define(NECK_BONUS, 0.5).
-define(REFLEX_DEG, 225.0).
-define(CONVEX_DEG, 155.0).
-define(TRI_SCALE, 0.4).
-define(TRI_WEIGHT, 0.3).
-define(BLEND_MIN, 0.5).
-define(BLEND_FULL, 0.87).
-define(REFLEX_MIN, 0.26).
-define(ANGLE_WEIGHT, 0.6).


init() -> true.

%%%
%%% Insert menu heading
%%%

menu({face},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) ->
    Menu.

parse([{_,{tesselate,_}}=Cmd|Rest], NewMenu, false) ->
    parse(Rest, [add_menu(),Cmd|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found);
parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [add_menu()|NewMenu].

add_menu() ->
    [{?__(1,"Quad Fill"),quad_fill,
      ?__(2,"Fill the selected n-gon faces with a grid of quads"),[option]}].

%%%
%%% Commands
%%%

command({face,{quad_fill,Ask}}, St) when is_atom(Ask) ->
    ask(Ask, St);
command({face,{quad_fill,Opts}}, St) ->
    quad_fill(Opts, St);
command(_, _) ->
    next.


ask(Ask, St) ->
    Sizes = face_sizes(St),
    Valid = [N || N <- Sizes, N > 4],
    case Valid of
        [] ->
            wings_u:error_msg(?__(1,"Select n-gon faces with five or more edges."));
        _ ->
            ask_dialog(Valid, Ask, St)
    end.

face_sizes(St) ->
    Collect =
        fun(Fs, We, Acc) ->
            Sizes = [length(wings_face:vertices_ccw(F, We)) ||
                        F <- gb_sets:to_list(Fs)],
            Sizes ++ Acc
        end,
    wings_sel:fold(Collect, [], St).

ask_dialog(Valid, Ask, St) ->
    Max = lists:max(Valid),
    Half = max(3, Max div 2),
    Qs = [{label_column,
           [{?__(2,"Rotation"),
             [{slider,{text,0,[{key,rot},{range,{0,Max-1}},{info,info(rot)}]}}]},
            {?__(3,"Split"),
             [{slider,{text,0,[{key,split},{range,{2-Half,Half-2}},{info,info(split)}]}}]},
            {?__(4,"Curvature %"),
             [{slider,{text,?DEFAULT_CURVATURE,
                       [{key,curv},{range,{0,200}},{info,info(curv)}]}}]},
            {?__(6,"Relax %"),
             [{slider,{text,?DEFAULT_RELAX,
                       [{key,relax},{range,{0,100}},{info,info(relax)}]}}]}
           ]}],
    wings_dialog:dialog_preview({face,quad_fill}, Ask, ?__(5,"Quad Fill"), Qs, St).

info(rot) ->
    ?__(1,"Rotate the grid along the face ring (number of vertices), starting from the automatic choice");
info(split) ->
    ?__(2,"Move the corners along the ring (sets the direction of the new edge loops), starting from the automatic choice");
info(curv) ->
    ?__(3,"0 = plain patch between the borders, 100 = follow the surrounding surface, above 100 = exaggerate the curvature");
info(relax) ->
    ?__(4,"0 = keep the initial grid, 100 = evenly spaced vertices (rounder grid)").

%%%
%%% Process commands
%%%

%%     The ring of the selected face (N vertices; an odd N also gets one
%%     triangle, see fill_odd/5) is divided
%%     into four sides of A, B, A and B edges (A+B = N/2). The patch is
%%     an A x B quad grid:
%%
%%         G(B,0) ---- top (A) ---- G(B,A)
%%           |                        |
%%         left (B)               right (B)
%%           |                        |
%%         G(0,0) --- bottom (A) -- G(0,A)
%%
%%     "Rotation" moves the first corner along the ring and "Split"
%%     chooses A, which together set the direction of the new edge
%%     loops. "Curvature" blends from a plain (bilinear Coons) patch to
%%     a bicubic Hermite patch whose cross-boundary tangents follow the
%%     surrounding faces. "Relax" then evens out the interior vertices
%%     with tangential smoothing (the border stays fixed and every
%%     vertex keeps its height over the patch), which gives the rounder
%%     grid.
%%

quad_fill(Opts0, St) ->
    Opts = maps:from_list(Opts0),
    Fill =
        fun(Fs, We) ->
            fill_faces(Fs, Opts, We)
        end,
    wings_sel:map_update_sel(Fill, St).

fill_faces(Fs, Opts, We0) ->
    Faces = gb_sets:to_list(Fs),
    Fill =
        fun(Face, {We1,Acc}) ->
            {We2,Patch} = fill_face(Face, Opts, We1),
            {We2,Patch ++ Acc}
        end,
    {We,New} = lists:foldl(Fill, {We0,[]}, Faces),
    {We,gb_sets:from_list(New)}.

%%  Fill Face with quads in two steps. First cut_corners/3 splits off
%%  the corners that are too sharp or too reflex with new edges (this
%%  only depends on the outline), then fill_piece/3 replaces each piece
%%  by a grid of quads.
fill_face(Face, Opts, We0) ->
    {We1,Pieces} = cut_corners(Face, Opts, We0),
    fill_pieces(Pieces, Opts, We1, []).

fill_pieces([Piece|Pieces], Opts, We0, Acc) ->
    Quad = length(wings_face:vertices_ccw(Piece, We0)) =:= 4,
    {We,Patch} = fill_piece(Quad, Piece, Opts, We0),
    fill_pieces(Pieces, Opts, We, Patch ++ Acc);
fill_pieces([], _, We, Acc) ->
    {We,Acc}.

%% fill_piece(IsQuad, Face, Opts, We0) -> {We,PatchFaces}
%%  A piece that already is a quad is kept as it is.
fill_piece(true, Face, _, We) ->
    {We,[Face]};
fill_piece(false, Face, Opts, We) ->
    fill_piece(Face, Opts, We).

%%  Replace Face by an A x B grid of quads. Every new element is made
%%  with wings_vertex:force_connect/4 and wings_edge:fast_cut/3, so the
%%  edge tables stay consistent and the face material, UV coordinates
%%  and vertex colors are interpolated from the border. Only the
%%  positions of the new interior vertices are written directly (vp).
fill_piece(Face, Opts, We) ->
    Ring = wings_face:vertices_ccw(Face, We),
    fill_ring(length(Ring), Face, Ring, Opts, We).

fill_ring(4, Face, _, _, We) ->
    {We,[Face]};
fill_ring(3, Face, _, _, We) ->
    {We,[Face]};
fill_ring(N, Face, Ring, Opts, We) when N rem 2 =/= 0, N > 3 ->
    fill_odd(N, Face, Ring, Opts, We);
fill_ring(N, _, _, _, _) when N < 3 ->  % this should no happen, but...
    wings_u:error_msg(?__(1,"Quad Fill needs faces with at least three edges."));
fill_ring(6, Face, Ring, Opts, We) ->
    fill_hexagon(star_layout(Face, Ring, Opts, decision_we(Opts, We)), Face, Ring, Opts, We);
fill_ring(N, Face, Ring, Opts, We) ->
    fill_grid(N, Face, Ring, Opts, We).

fill_hexagon({star,Off}, Face, Ring, Opts, We) ->
    fill_star(Off, Face, Ring, Opts, We);
fill_hexagon(grid, Face, Ring, Opts, We) ->
    fill_grid(6, Face, Ring, Opts, We).

fill_grid(N, Face, Ring0, Opts, We0) ->
    Half = N div 2,
    {Rot0,Split0} = base_corners(Ring0, decision_we(Opts, We0)),
    Split = clamp(Split0 + maps:get(split, Opts, 0), 1, Half-1),
    A = Split,
    B = Half - Split,
    Rot = ((Rot0 + maps:get(rot, Opts, 0)) rem N + N) rem N,
    {Head,Tail} = lists:split(Rot, Ring0),
    fill_patch(Face, Tail ++ Head, A, B, Opts, We0).

%%  The mesh as the layout decisions (grid corners, star or grid) see it:
%%  Opts may hold flat => #{Vertex => Pos} for vertices that are placed
%%  with the Curvature lift, so that Curvature never changes the topology.
decision_we(Opts, We) ->
    Flat = maps:get(flat, Opts, #{}),
    maps:fold(fun(V, Pos, W) ->
                  W#we{vp=array:set(V, Pos, W#we.vp)}
              end, We, Flat).

%%  Fill Face with an AxB grid. Ring starts at the grid corner G(0,0)
%%  and goes ccw, A edges along the first side.
fill_patch(Face, Ring, A, B, Opts, We0) ->
    Curv = maps:get(curv, Opts, ?DEFAULT_CURVATURE) / 100,
    Relax = maps:get(relax, Opts, ?DEFAULT_RELAX) / 100,
    S = patch_setup(Face, Ring, A, B, We0),
    Plan = patch_positions(S, Curv, Relax),
    {We1,G,Patch} = build_grid(Face, A, B, list_to_tuple(Ring), We0),
    {set_positions(Plan, G, We1),Patch}.

%%%
%%% Odd number of edges: quads and one central triangle
%%%

%% fill_odd(N, Face, Ring, Opts, We0) -> {We,PatchFaces}
%%  A quad mesh needs an even border, so one triangle is unavoidable.
%%  It is put in the middle: three ring vertices Ra, Rb, Rc split the
%%  ring in arcs of an odd number of edges; the triangle Ta Tb Tc is
%%  joined to them by the edges Ra-Ta, Rb-Tb, Rc-Tc, and each of the three
%%  regions (an arc, two spokes and a triangle side, an even polygon)
%%  is filled as any other piece. Made from the chords Ra-Rb, Rb-Rc, Rc-Ra,
%%  extruding the triangle they enclose and dissolving the chords.
%%  Rotation moves the three corners along the ring, which turns the
%%  triangle; the regions themselves are filled with Rotation 0.
fill_odd(N, Face, Ring, Opts, We0) ->
    Ps = [wings_vertex:pos(V, We0) || V <- Ring],
    Curv = maps:get(curv, Opts, ?DEFAULT_CURVATURE) / 100,
    Hub = tri_hub(Face, Ring, Ps, Curv, We0),
    {I0,S1,S2} = cached({triangle,Ps},
                        fun() ->
                            best_triangle(N, list_to_tuple(Ps), flat_hub(Hub))
                        end),
    I = ((I0 + maps:get(rot, Opts, 0)) rem N + N) rem N,
    Va = lists:nth(I+1, Ring),
    Vb = lists:nth((I+S1) rem N + 1, Ring),
    Vc = lists:nth((I+S1+S2) rem N + 1, Ring),
    {We1,Tri} = triangle_chords(Face, [{Va,Vb,Vc,S1},{Vb,Vc,Va,S2},{Vc,Va,Vb,N-S1-S2}], We0),
    Corners = [Va,Vb,Vc],
    We2 = wings_extrude_face:faces([Tri], We1),
    TriVs = wings_face:vertices_ccw(Tri, We2),
    We3 = lists:foldl(fun(T, W) ->
                          place_corner(T, Corners, Hub, W)
                      end, We2, TriVs),
    Flat = maps:from_list([{T,corner_pos(T, Corners, flat_hub(Hub), We2)} || T <- TriVs]),
    We4 = dissolve_chords(Corners, N, S1, S2, We3),
    Regions = lists:usort(lists:append([corner_faces(T, Tri, We4) || T <- TriVs])),
    {We5,Patch} = fill_pieces(Regions, Opts#{rot=>0,flat=>Flat}, We4, []),
    We = lift_triangle(TriVs, Ring, Hub, We5),
    {We,[Tri|Patch]}.

%%  The corners are chosen without the lift, so that Curvature and Relax
%%  never change the topology.
flat_hub({Cen,_}) ->
    {Cen,{0.0,0.0,0.0}}.

%%  Odd arc lengths within three edges of N/3 (a lopsided triangle is
%%  never wanted) as long as three of them add up to N; all odd lengths
%%  otherwise.
triangle_arcs(N) ->
    Odd = lists:seq(1, N-2, 2),
    Near = [S || S <- Odd, abs(S - (N / 3)) =< 3.0],
    Fits = [ok || S1 <- Near, S2 <- Near, lists:member(N - S1 - S2, Near)],
    case Fits of
        [ok|_] -> Near;
        [] -> Odd
    end.

%%  The topology only depends on the outline, and the dialog asks for
%%  the same one again at every slider change, so the last answer is kept.
cached(Key, Fun) ->
    cached(Key, Fun, persistent_term:get(wpc_quadfill_cache, none)).

cached(Key, _, {Key,Value}) ->
    Value;
cached(Key, Fun, _) ->
    Value = Fun(),
    persistent_term:put(wpc_quadfill_cache, {Key,Value}),
    Value.

%%  Centre of the ring and how far the surface lifts there. The lift is
%%  the one the grids get (see surface/3): for opposite ring vertices
%%  Pi and Pj, a cubic Hermite curve that continues the faces around the
%%  hole rises over the chord by -L (Oi + Oj) / 8, where O is the
%%  direction leading away from the hole and L the chord length. The
%%  mean of these, along the ring normal, is scaled by Curvature.
tri_hub(Face, Ring, Ps, Curv, We) ->
    N = length(Ring),
    Pp = list_to_tuple(Ps),
    Out = ring_outward(Face, Ring, We),
    Dir = fun(I) -> unit(e3d_vec:add(element((I - 1 + N) rem N + 1, Out),
                                     element(I rem N + 1, Out))) end,
    Up = fun(I) ->
             J = (I + N div 2) rem N,
             L = e3d_vec:dist(element(I+1, Pp), element(J+1, Pp)),
             e3d_vec:mul(e3d_vec:add(Dir(I), Dir(J)), (-L / 8.0))
         end,
    Rises = [Up(I) || I <- lists:seq(0, N-1)],
    Nrm = unit(newell(Ps)),
    Rise = e3d_vec:average(Rises),
    Bulge = e3d_vec:mul(Nrm, e3d_vec:dot(Rise, Nrm) * Curv * ?TANGENT_GAIN),
    {e3d_vec:average(Ps),Bulge}.

%%  A ring vertex pulled towards the centre by ?TRI_SCALE; the lift
%%  fades with the square of the distance from the centre.
tri_point(Pos, {Cen,Bulge}) ->
    Pull = e3d_vec:add(Cen, e3d_vec:mul(e3d_vec:sub(Pos, Cen), ?TRI_SCALE)),
    e3d_vec:add(Pull, e3d_vec:mul(Bulge, 1.0 - ?TRI_SCALE * ?TRI_SCALE)).

%% lift_triangle(TriVs, Ring, Hub, We) -> We
%%  The grids around the triangle get their own lift from the faces around
%%  the hole, so the triangle, placed before they exist, can end up
%%  lower than its neighbours and make a dent. Each corner is moved along
%%  the direction of the lift until it is as high as the highest new
%%  vertex that shares an edge with any corner (never lowered).
lift_triangle(TriVs, Ring, {Cen,Bulge}, We0) ->
    Len = e3d_vec:len(Bulge),
    lift_triangle(Len > ?EPSILON0, TriVs, Ring, Cen, unit(Bulge), We0).

lift_triangle(false, _, _, _, _, We) ->
    We;
lift_triangle(true, TriVs, Ring, Cen, Dir, We0) ->
    Height = fun(V, We) -> e3d_vec:dot(e3d_vec:sub(wings_vertex:pos(V, We), Cen), Dir) end,
    Target =
        fun(T) ->
            Near = wings_vertex:fold(fun(_, _, #edge{vs=V1,ve=V2}, A) ->
                                         [case V1 of T -> V2; _ -> V1 end|A]
                                     end, [], T, We0),
            [Height(V, We0) || V <- Near, not lists:member(V, TriVs),
                               not lists:member(V, Ring)]
        end,
    All = lists:append([Target(T) || T <- TriVs]),
    lists:foldl(fun(T, We) when All =/= [] ->
                        Up = lists:max(All) - Height(T, We0),
                        Pos = wings_vertex:pos(T, We),
                        New = e3d_vec:add(Pos, e3d_vec:mul(Dir, max(0.0, Up))),
                        We#we{vp=array:set(T, New, We#we.vp)};
                   (_, We) -> We
                end, We0, TriVs).

%% triangle_chords(Big, [{Va,Vb,Vc,Edges}], We) -> {We,TriangleFace}
%%  Connect Va-Vb (when the arc has more than one edge) in the face that
%%  holds the opposite vertex Vc.
triangle_chords(Big, [{Va,Vb,Vc,Edges}|Arcs], We0) when Edges >= 3 ->
    {We1,New} = wings_vertex:force_connect(Va, Vb, Big, We0),
    triangle_chords(pick_face([Big,New], [Vc], We1), Arcs, We1);
triangle_chords(Big, [_|Arcs], We) ->
    triangle_chords(Big, Arcs, We);
triangle_chords(Tri, [], We) ->
    {We,Tri}.

place_corner(T, Corners, Hub, We) ->
    We#we{vp=array:set(T, corner_pos(T, Corners, Hub, We), We#we.vp)}.

%%  Where the triangle corner T goes: its ring vertex (the one of Corners
%%  it shares an edge with) pulled towards the centre.
corner_pos(T, Corners, Hub, We) ->
    Near = wings_vertex:fold(fun(_, _, #edge{vs=V1,ve=V2}, A) ->
                                     [case V1 of T -> V2; _ -> V1 end|A]
                             end, [], T, We),
    [R] = [V || V <- Near, lists:member(V, Corners)],
    tri_point(wings_vertex:pos(R, We), Hub).

%%  Remove the chord of each arc that has one: the side quad beside the
%%  triangle then joins the arc's polygon.
dissolve_chords([Va,Vb,Vc], N, S1, S2, We0) ->
    Arcs = [{Va,Vb,S1},{Vb,Vc,S2},{Vc,Va,N-S1-S2}],
    lists:foldl(fun({A,B,Edges}, W) when Edges >= 3 ->
                        [{Edge,_,_}|_] = wings_vertex:edge_through(A, B, W),
                        wings_edge:dissolve_edge(Edge, W);
                   (_, W) -> W
                end, We0, Arcs).

corner_faces(T, Tri, We) ->
    wings_vertex:fold(fun(_, F, _, A) when F =/= Tri -> [F|A];
                         (_, _, _, A) -> A
                      end, [], T, We).

%%  Corners at ring indexes I, I+S1 and I+S1+S2 (arcs S1, S2 and the rest,
%%  all odd) with the best worst region plus a price for a lopsided
%%  triangle. The triangle corners are the ring vertices pulled towards Hub.
best_triangle(N, Pp, Hub) ->
    Tp = fun(K) -> tri_point(element(K rem N + 1, Pp), Hub) end,
    Arc = fun(I, S) -> [element((I + K) rem N + 1, Pp) || K <- lists:seq(0, S)] end,
    Ss = triangle_arcs(N),
    Costs = maps:from_list([{{I,S},piece_cost(Arc(I, S) ++ [Tp(I + S),Tp(I)])} ||
                               I <- lists:seq(0, N-1), S <- Ss]),
    Region = fun(I, S) -> maps:get({I rem N,S}, Costs) end,
    Cost = fun(I, S1, S2) ->
                S3 = N - S1 - S2,
                Regions = max(Region(I, S1), max(Region(I + S1, S2), Region(I + S1 + S2, S3))),
                Ls = [e3d_vec:dist(Tp(A), Tp(B)) ||
                        {A,B} <- [{I,I+S1},{I+S1,I+S1+S2},{I+S1+S2,I}]],
                LsMin = lists:min(Ls),
                LsMax = lists:max(Ls),
                Regions + ?TRI_WEIGHT * (1.0 - (LsMin / LsMax))
           end,
    Cands = [{Cost(I, S1, S2),I,S1,S2} ||
                I <- lists:seq(0, N-1), S1 <- Ss, S2 <- Ss, lists:member(N - S1 - S2, Ss)],
    {_,I,S1,S2} = lists:min(Cands),
    {I,S1,S2}.

%%%
%%% Three-quad template for six-sided pieces
%%%

%% star_layout(Face, Ring, Opts, We) -> grid | {star,Offset}
%%  A hexagon can be filled by a 1x2 grid (two quads, one edge across)
%%  or by three quads around a new vertex joined to every second ring
%%  vertex. The star is used when its worst quad is not much worse than
%%  the grid's. Only with Split at 0 (Split is what
%%  asks for a grid); an odd Rotation uses the other star.
star_layout(Face, Ring, Opts, We) ->
    Auto = maps:get(split, Opts, 0) =:= 0,
    star_layout_1(Auto, Face, Ring, Opts, We).

star_layout_1(false, _, _, _, _) ->
    grid;
star_layout_1(true, Face, Ring, Opts, We) ->
    Pp = ring_tuple(Ring, We),
    {Grid,_,_} = best_layout(Pp),
    Costs = [{star_cost(Off, Face, Ring, 0.0, We),Off} || Off <- [0,1]],
    {Cost,Off} = lists:min(Costs),
    Turn = abs(maps:get(rot, Opts, 0)) rem 2,
    star_pick(Cost =< Grid + ?STAR_BIAS, (Off + Turn) rem 2).

star_pick(true, Off) -> {star,Off};
star_pick(false, _) -> grid.

star_cost(Off, Face, Ring, Curv, We) ->
    R = rotate(Ring, Off),
    Ps = [wings_vertex:pos(V, We) || V <- R],
    O = star_center(Ps, R, Face, Curv, We),
    star_quads_cost(Ps, O).

%% Worst cost of the three quads around O
star_quads_cost(Ps, O) ->
    P = list_to_tuple(Ps),
    Quads = [[element(K+1, P),element((K+1) rem 6 + 1, P),element((K+2) rem 6 + 1, P),O] ||
                K <- [0,2,4]],
    lists:max([layout_cost(list_to_tuple(Q), 0, 1) || Q <- Quads]).

rotate(List, N) ->
    {Head,Tail} = lists:split(N, List),
    Tail ++ Head.

%%  Start from star_start/5 and search for the position in the plane of
%%  the ring that gives the best worst quad, with a shrinking 3x3 grid.
star_center(Ps, Ring, Face, Curv, We) ->
    Start = star_start(Ps, Ring, Face, Curv, We),
    {U,V} = plane_basis(newell(Ps)),
    PsMin = lists:min(Ps),
    PsMax = lists:max(Ps),
    Size = e3d_vec:dist(PsMin, PsMax) / 2,
    Search =
        fun(Step, Best) ->
            Moves = [e3d_vec:add(e3d_vec:mul(U, Du * Step * Size),
                                 e3d_vec:mul(V, Dv * Step * Size)) ||
                        Du <- [-1,0,1], Dv <- [-1,0,1]],
            Cands = [e3d_vec:add(Best, Move) || Move <- Moves],
            element(2, lists:min([{star_quads_cost(Ps, C),C} || C <- Cands]))
        end,
    lists:foldl(Search, Start, [0.3,0.15,0.08,0.04]).

%%  The centroid of the ring moved towards the average of its projections
%%  on the tangent planes at the ring vertices (planes made from the
%%  faces outside the hole), so the new vertex follows the surface.
star_start(Ps, Ring, Face, Curv, We) ->
    C = e3d_vec:average(Ps),
    Pr = fun({V,P}) ->
             N = vertex_normal(V, Face, We),
             e3d_vec:sub(C, e3d_vec:mul(N, e3d_vec:dot(e3d_vec:sub(C, P), N)))
         end,
    Rvpos = [wings_vertex:pos(V, We) || V <- Ring],
    Proj = [Pr(VP) || VP <- lists:zip(Ring, Rvpos)],
    Surf = e3d_vec:average(Proj),
    e3d_vec:add(C, e3d_vec:mul(e3d_vec:sub(Surf, C), Curv)).

%%  Ring vertices V0..V5 (starting at Off); the new vertex O gets edges
%%  to V0, V2 and V4. Built from the face operations: chord V0-V3, a
%%  vertex O cut into it, O joined to V2 and V4, and the leftover edge
%%  O-V3 dissolved so the two triangles beside it become one quad.
fill_star(Off, Face, Ring, Opts, We0) ->
    [V0,V1,V2,V3,V4,V5] = rotate(Ring, Off),
    Ps = [wings_vertex:pos(V, We0) || V <- [V0,V1,V2,V3,V4,V5]],
    Curv = maps:get(curv, Opts, ?DEFAULT_CURVATURE) / 100,
    PosO = star_center(Ps, [V0,V1,V2,V3,V4,V5], Face, Curv, We0),
    {We1,New} = wings_vertex:force_connect(V0, V3, Face, We0),
    [{Chord,_,_}|_] = wings_vertex:edge_through(V0, V3, We1),
    {We2,O} = wings_edge:fast_cut(Chord, PosO, We1),
    Fa = pick_face([Face,New], [V1], We2),
    Fb = pick_face([Face,New], [V4], We2),
    {We3,_} = wings_vertex:force_connect(O, V2, Fa, We2),
    {We4,_} = wings_vertex:force_connect(O, V4, Fb, We3),
    [{Spare,_,_}|_] = wings_vertex:edge_through(O, V3, We4),
    We = wings_edge:dissolve_edge(Spare, We4),
    Patch = wings_vertex:fold(fun(_, F, _, A) -> [F|A] end, [], O, We),
    {We,Patch}.

%%  The automatic corners; the Rotation and Split options are added to them.
base_corners(Ring, We) ->
    {_,Rot,Split} = best_layout(ring_tuple(Ring, We)),
    {Rot,Split}.

clamp(X, Min, _) when X < Min -> Min;
clamp(X, _, Max) when X > Max -> Max;
clamp(X, _, _) -> X.

%%%
%%% Topology
%%%

%%  Ring vertex at the boundary grid position (R,C). P is the tuple
%%  of ring vertices, starting at the corner G(0,0) and going ccw.
bnd(0, C, _, _, P) -> element(C+1, P);
bnd(R, A, A, _, P) -> element(A+R+1, P);
bnd(B, C, A, B, P) -> element(2*A+B-C+1, P);
bnd(R, 0, A, B, P) -> element(2*A+2*B-R+1, P).

build_grid(Face, A, B, P, We0) ->
    G0 = boundary_grid(A, B, P),
    {Chords,Patch1,We1} = make_chords(1, A, B, P, Face, [Face], [], We0),
    {G,We2} = cut_chords(Chords, A, B, P, G0, We1),
    {Patch,We} = make_rows(0, A, B, P, G, Patch1, We2),
    {We,G,Patch}.

boundary_grid(A, B, P) ->
    Pos = [{R,C} || R <- lists:seq(0, B), C <- lists:seq(0, A),
                    R =:= 0 orelse R =:= B orelse C =:= 0 orelse C =:= A],
    maps:from_list([{RC,bnd(R, C, A, B, P)} || {R,C}=RC <- Pos]).

%% make_chords(...) -> {[{Column,ChordEdge}],Patch,We}
%%  Split the face with one chord G(0,C)-G(B,C) for each interior
%%  column. A new edge always gets the same id as the new face.
make_chords(C, A, _, _, _, Patch, Chords, We) when C >= A ->
    {lists:reverse(Chords),Patch,We};
make_chords(C, A, B, P, Cur, Patch, Chords, We0) ->
    Va = bnd(0, C, A, B, P),
    Vb = bnd(B, C, A, B, P),
    {We,New} = wings_vertex:force_connect(Va, Vb, Cur, We0),
    Right = bnd(0, A, A, B, P),
    Next = pick_face([Cur,New], [Right], We),
    make_chords(C+1, A, B, P, Next, [New|Patch], [{C,New}|Chords], We).

cut_chords([{C,Edge}|Chords], A, B, P, G0, We0) ->
    Pa = wings_vertex:pos(bnd(0, C, A, B, P), We0),
    Pb = wings_vertex:pos(bnd(B, C, A, B, P), We0),
    {G,We} = cut_chord(1, B, C, Edge, Pa, Pb, G0, We0),
    cut_chords(Chords, A, B, P, G, We);
cut_chords([], _, _, _, G, We) ->
    {G,We}.

%% fast_cut/3 cuts Edge (Vs->Ve) in two; the new vertex and the new
%% edge (NewV -> Ve) share the same id, so the cut continues on it.
cut_chord(R, B, _, _, _, _, G, We) when R >= B ->
    {G,We};
cut_chord(R, B, C, Edge, Pa, Pb, G0, We0) ->
    Pos = lerp(Pa, Pb, R/B),
    {We,V} = wings_edge:fast_cut(Edge, Pos, We0),
    G = G0#{{R,C} => V},
    cut_chord(R+1, B, C, V, Pa, Pb, G, We).

%%  Split each column strip with the row edges G(R,C)-G(R,C+1).
make_rows(C, A, _, _, _, Patch, We) when C >= A ->
    {Patch,We};
make_rows(C, A, B, P, G, Patch0, We0) ->
    Bottom = [bnd(0, C, A, B, P),bnd(0, C+1, A, B, P)],
    Top = [bnd(B, C, A, B, P),bnd(B, C+1, A, B, P)],
    Strip = pick_face(Patch0, Bottom ++ Top, We0),
    {Patch,We} = split_strip(1, C, B, G, Strip, Top, Patch0, We0),
    make_rows(C+1, A, B, P, G, Patch, We).

split_strip(R, _, B, _, _, _, Patch, We) when R >= B ->
    {Patch,We};
split_strip(R, C, B, G, Strip, Top, Patch, We0) ->
    Va = maps:get({R,C}, G),
    Vb = maps:get({R,C+1}, G),
    {We,New} = wings_vertex:force_connect(Va, Vb, Strip, We0),
    Next = pick_face([Strip,New], Top, We),
    split_strip(R+1, C, B, G, Next, Top, [New|Patch], We).

%%  First face that has all vertices Vs. Crashes if there is none.
pick_face([Face|Faces], Vs, We) ->
    Ring = wings_face:vertices_ccw(Face, We),
    Has = fun(V) -> lists:member(V, Ring) end,
    All = lists:all(Has, Vs),
    case All of
        true -> Face;
        false -> pick_face(Faces, Vs, We)
    end;
pick_face([], _, _) ->
    error(quad_fill_face_not_found).

set_positions(Plan, G, #we{vp=Vp0}=We) ->
    Set = fun({RC,Pos}, Vp) -> array:set(maps:get(RC, G), Pos, Vp) end,
    We#we{vp=lists:foldl(Set, Vp0, maps:to_list(Plan))}.

%%%
%%% Geometry
%%%

%% patch_sides(Pp, A, B) -> S
%%  The four sides of the patch (positions of the ring vertices, which
%%  start at the corner G(0,0)), the arc-length fractions of the side
%%  vertices and the normal. The cross-boundary tangents are zero here,
%%  patch_setup/5 fills them in. It needs no mesh, so it also serves to
%%  judge pieces that do not exist yet.
patch_sides(Pp, A, B) ->
    {BotI,RightI,TopI,LeftI} = side_indices(A, B),
    Bot = side_pos(BotI, Pp),
    Right = side_pos(RightI, Pp),
    Top = side_pos(TopI, Pp),
    Left = side_pos(LeftI, Pp),
    #{bot=>Bot,top=>Top,left=>Left,right=>Right,
      tb=>zeros(A+1),tt=>zeros(A+1),tl=>zeros(B+1),tr=>zeros(B+1),
      a=>A,b=>B,
      uf=>fractions(Bot, Top),vf=>fractions(Left, Right),
      fnrm=>newell(tuple_to_list(Pp))}.

side_pos(Idx, Pp) ->
    list_to_tuple([element(I+1, Pp) || I <- Idx]).

%% side_indices(A, B) -> {Bottom,Right,Top,Left}
%%  Ring indices along each side, all in the direction of the grid.
side_indices(A, B) ->
    N = 2*(A+B),
    {lists:seq(0, A),lists:seq(A, A+B),lists:seq(2*A+B, A+B, -1),
     [0|lists:seq(N-1, N-B, -1)]}.

zeros(Count) ->
    list_to_tuple(lists:duplicate(Count, {0.0,0.0,0.0})).

ring_tuple(Ring, We) ->
    list_to_tuple([wings_vertex:pos(V, We) || V <- Ring]).

%%  patch_sides/3 plus the cross-boundary tangents, taken from the
%%  faces around the hole.
patch_setup(Face, Ring, A, B, We) ->
    N = 2*(A+B),
    {BotI,RightI,TopI,LeftI} = side_indices(A, B),
    S = patch_sides(ring_tuple(Ring, We), A, B),
    #{bot:=Bot,top:=Top,left:=Left,right:=Right} = S,
    Out = ring_outward(Face, Ring, We),
    Dv = mean_dist(Bot, Top),
    Du = mean_dist(Left, Right),
    S#{tb=>scale_tuple(side_out(BotI, Out, N), -Dv),
       tt=>scale_tuple(side_out(TopI, Out, N), Dv),
       tl=>scale_tuple(side_out(LeftI, Out, N), -Du),
       tr=>scale_tuple(side_out(RightI, Out, N), Du)}.

mean_dist(Ta, Tb) ->
    La = tuple_to_list(Ta),
    Lb = tuple_to_list(Tb),
    Ds = [e3d_vec:dist(Pa, Pb) || {Pa,Pb} <- lists:zip(La, Lb)],
    DsSum = lists:sum(Ds),
    DsSum / length(Ds).

scale_tuple(T, S) ->
    list_to_tuple([e3d_vec:mul(V, S) || V <- tuple_to_list(T)]).

%% patch_positions(S, Curv, Relax) -> #{{R,C} => Pos}
%%  Positions of the interior grid vertices (1 =< R < B, 1 =< C < A).
%%  Pos = (1-Curv)*Coons + Curv*Hermite where
%%    Coons   = bilinearly blended Coons patch of the four borders,
%%    Hermite = bicubic Coons patch that also interpolates cross
%%              tangents taken from the faces around the hole.
%%  The result is then smoothed by relax/5.
patch_positions(#{a:=A,b:=B}, _, _) when A < 2; B < 2 ->
    #{};
patch_positions(#{a:=A,b:=B}=S, Curv, Relax) ->
    Keys = [{R,C} || R <- lists:seq(1, B-1), C <- lists:seq(1, A-1)],
    Surf = [{RC,surface(RC, Curv, S)} || RC <- Keys],
    Pos = maps:from_list([{RC,P} || {RC,{P,_}} <- Surf]),
    Nrm = maps:from_list([{RC,Nr} || {RC,{_,Nr}} <- Surf]),
    relax(Keys, Pos, Nrm, Relax, S).

%% ring_outward(Face, Ring, We) -> tuple of unit vectors
%%  One vector per ring edge i (between ring vertex i and i+1): the
%%  direction, in the surface around the hole, that leads away from
%%  the hole and is perpendicular to the edge.
ring_outward(Face, Ring, We) ->
    Cen = wings_vertex:center(Ring, We),
    Next = tl(Ring) ++ [hd(Ring)],
    Out = [edge_outward(Face, Va, Vb, Cen, We) || {Va,Vb} <- lists:zip(Ring, Next)],
    list_to_tuple(Out).

edge_outward(Face, Va, Vb, Cen, We) ->
    Pa = wings_vertex:pos(Va, We),
    Pb = wings_vertex:pos(Vb, We),
    Mid = e3d_vec:average(Pa, Pb),
    Ev = unit(e3d_vec:sub(Pb, Pa)),
    Dir0 = outward_target(Face, Va, Vb, Mid, Cen, We),
    Dir = perpendicular(Dir0, Ev),
    Fallback = perpendicular(e3d_vec:sub(Mid, Cen), Ev),
    Len = e3d_vec:len(Dir),
    pick_dir(Len > ?EPSILON0, Dir, Fallback).

pick_dir(true, Dir, _) -> unit(Dir);
pick_dir(false, _, Fallback) -> unit(Fallback).

outward_target(Face, Va, Vb, Mid, Cen, We) ->
    Edge = wings_vertex:edge_through(Va, Vb, Face, We),
    #edge{lf=Lf,rf=Rf} = array:get(Edge, We#we.es),
    Other = other_face(Face, Lf, Rf),
    target_dir(Other >= 0, Other, Mid, Cen, We).

other_face(Face, Face, Rf) -> Rf;
other_face(_, Lf, _) -> Lf.

target_dir(true, Other, Mid, _, We) ->
    e3d_vec:sub(wings_face:center(Other, We), Mid);
target_dir(false, _, Mid, Cen, _) ->
    e3d_vec:sub(Mid, Cen).

perpendicular(V, Ev) ->
    e3d_vec:sub(V, e3d_vec:mul(Ev, e3d_vec:dot(V, Ev))).

unit(V) ->
    Len = e3d_vec:len(V),
    unit(Len > ?EPSILON1, V, Len).

unit(true, V, Len) -> e3d_vec:mul(V, 1.0/Len);
unit(false, _, _) -> {0.0,0.0,0.0}.

%% side_out(Idx, Out, N) -> tuple of unit vectors, one per side vertex
%%  Average of the outward vectors of the (one or two) ring edges that
%%  touch the vertex along this side. Corners therefore get a separate
%%  vector for each of their two sides.
side_out(Idx, Out, N) ->
    Pairs = pairs(Idx),
    Vecs = [element(ring_edge(X, Y, N)+1, Out) || {X,Y} <- Pairs],
    list_to_tuple(blend(none, Vecs)).

pairs([X,Y|T]) -> [{X,Y}|pairs([Y|T])];
pairs(_) -> [].

ring_edge(X, Y, N) ->
    Next = (X+1) rem N,
    ring_edge_1(Next =:= Y, X, Y).

ring_edge_1(true, X, _) -> X;
ring_edge_1(false, _, Y) -> Y.

blend(Prev, [V|Vs]) -> [mix(Prev, V)|blend(V, Vs)];
blend(Prev, []) -> [mix(Prev, none)].

mix(none, V) -> V;
mix(V, none) -> V;
mix(Va, Vb) -> unit(e3d_vec:add(Va, Vb)).

%%  Bilinear Coons position plus a bulge along the local patch normal:
%%
%%    Pos = Lin + Curv * Gain * ((Corr . N) N)
%%
%%  Corr is the Hermite correction (the difference between a cubic
%%  Hermite loft and a linear loft) in both grid directions, using the
%%  cross-boundary tangents. It is weighted by 4t(1-t) along the side
%%  so that it vanishes at the corners and the borders stay exact.
%%  Keeping only the normal component leaves flat holes untouched.
surface({R,C}, Curv, S) ->
    Lin = lin(R, C, S),
    Du = e3d_vec:sub(lin(R, C+1, S), lin(R, C-1, S)),
    Dv = e3d_vec:sub(lin(R+1, C, S), lin(R-1, C, S)),
    N = unit(e3d_vec:cross(Du, Dv)),
    Corr = correction(R, C, S),
    Bulge = e3d_vec:mul(N, e3d_vec:dot(Corr, N)),
    Pos = e3d_vec:add(Lin, e3d_vec:mul(Bulge, Curv * ?TANGENT_GAIN)),
    {Pos,N}.

lin(R, C, #{a:=A,bot:=Bot,top:=Top,left:=Left,right:=Right,uf:=Uf,vf:=Vf}) ->
    U = element(C+1, Uf),
    V = element(R+1, Vf),
    P00 = element(1, Bot),
    P10 = element(A+1, Bot),
    P01 = element(1, Top),
    P11 = element(A+1, Top),
    combine([{1.0-V,element(C+1, Bot)},{V,element(C+1, Top)},
             {1.0-U,element(R+1, Left)},{U,element(R+1, Right)},
             {-(1.0-U)*(1.0-V),P00},{-U*(1.0-V),P10},
             {-(1.0-U)*V,P01},{-U*V,P11}]).

correction(R, C, #{bot:=Bot,top:=Top,left:=Left,right:=Right,
                   tb:=Tb,tt:=Tt,tl:=Tl,tr:=Tr,uf:=Uf,vf:=Vf}) ->
    U = element(C+1, Uf),
    V = element(R+1, Vf),
    {_,_,Hu2,Hu3} = hermite(U),
    {_,_,Hv2,Hv3} = hermite(V),
    ChordV = e3d_vec:sub(element(C+1, Top), element(C+1, Bot)),
    ChordU = e3d_vec:sub(element(R+1, Right), element(R+1, Left)),
    CorrV = combine([{Hv2,e3d_vec:sub(element(C+1, Tb), ChordV)},
                     {Hv3,e3d_vec:sub(element(C+1, Tt), ChordV)}]),
    CorrU = combine([{Hu2,e3d_vec:sub(element(R+1, Tl), ChordU)},
                     {Hu3,e3d_vec:sub(element(R+1, Tr), ChordU)}]),
    combine([{side_weight(U),CorrV},{side_weight(V),CorrU}]).

side_weight(T) -> 4.0*T*(1.0-T).

%% Cubic Hermite basis: {h00,h01,h10,h11}. h00 and h01 weight the end
%% positions; h10 and h11 weight the end derivatives.
hermite(T) ->
    T2 = T*T,
    T3 = T2*T,
    {2.0*T3-3.0*T2+1.0,-2.0*T3+3.0*T2,T3-2.0*T2+T,T3-T2}.

combine(Terms) ->
    Add = fun({W,Vec}, Acc) ->
              e3d_vec:add(Acc, e3d_vec:mul(Vec, W))
          end,
    lists:foldl(Add, {0.0,0.0,0.0}, Terms).

lerp(Pa, Pb, T) ->
    Dist0 = e3d_vec:sub(Pb, Pa),
    Dist = e3d_vec:mul(Dist0, T),
    e3d_vec:add(Pa, Dist).

%%%
%%% Relax
%%%

%% relax(Keys, Pos, Nrm, Relax, S) -> #{{R,C} => Pos}
%%  Tangential Gauss-Seidel smoothing of the interior vertices. Each one
%%  moves toward the average of its four grid neighbours, but only
%%  perpendicular to its patch normal, so its height over the patch is
%%  kept and the border stays where it is. Relax (0.0 to 1.0) blends
%%  from the initial grid to the fully smoothed one.
relax(_, Pos, _, Relax, _) when Relax =< 0.0 ->
    Pos;
relax(Keys, Pos, Nrm, Relax, #{a:=A,b:=B}=S) ->
    Rel = relax_iter(?RELAX_ITER * max(A, B), Keys, Nrm, S, Pos),
    RCPos = [{RC,lerp(maps:get(RC, Pos), maps:get(RC, Rel), Relax)} || RC <- Keys],
    maps:from_list(RCPos).

relax_iter(0, _, _, _, Cur) ->
    Cur;
relax_iter(K, Keys, Nrm, S, Cur0) ->
    Cur = relax_sweep(Keys, Nrm, S, Cur0),
    relax_iter(K-1, Keys, Nrm, S, Cur).

relax_sweep([{R,C}=RC|Keys], Nrm, S, Cur0) ->
    Avg = e3d_vec:average([at(R-1, C, Cur0, S),at(R+1, C, Cur0, S),
                           at(R, C-1, Cur0, S),at(R, C+1, Cur0, S)]),
    P = maps:get(RC, Cur0),
    Dir = perpendicular(e3d_vec:sub(Avg, P), maps:get(RC, Nrm)),
    Cur = Cur0#{RC := e3d_vec:add(P, e3d_vec:mul(Dir, ?RELAX_OMEGA))},
    relax_sweep(Keys, Nrm, S, Cur);
relax_sweep([], _, _, Cur) ->
    Cur.

%%  Position of grid point (R,C): the interior ones come from Cur, the
%%  border ones from the four sides.
at(R, C, Cur, S) ->
    Found = maps:find({R,C}, Cur),
    at_1(Found, R, C, S).

at_1({ok,Pos}, _, _, _) ->
    Pos;
at_1(error, R, C, S) ->
    side_at(R, C, S).

side_at(0, C, #{bot:=Bot}) -> element(C+1, Bot);
side_at(R, C, #{b:=R,top:=Top}) -> element(C+1, Top);
side_at(R, 0, #{left:=Left}) -> element(R+1, Left);
side_at(R, _, #{right:=Right}) -> element(R+1, Right).

%%%
%%% Corner selection
%%%

%% best_layout(Pp) -> {Cost,Rot,Split}
%%  Choose the first corner (Rot, an index in the ring) and the number of
%%  edges of the first side (Split) from the shape of the outline Pp (a
%%  tuple of positions). Corners go where the outline turns by about 90
%%  degrees, never on a reflex vertex, and the sides get lengths and edge
%%  counts that fit the proportions of the outline. Cost is the one of
%%  the chosen layout (see layout_cost/3).
best_layout(Pp) ->
    N = tuple_size(Pp),
    Half = N div 2,
    Nrm = newell(tuple_to_list(Pp)),
    Turn = list_to_tuple([turning(I, N, Pp, Nrm) || I <- lists:seq(0, N-1)]),
    Cum = cumulative(Pp, N),
    Min = min_side(Half),
    Costs = [{corner_cost(K, A, Half-A, N, Turn, Cum),K,A} ||
                K <- lists:seq(0, N-1), A <- lists:seq(Min, Half-Min)],
    Best = lists:sublist(lists:sort(Costs), ?AUTO_CANDIDATES),
    Layouts = [{layout_cost(Pp, K, A),Cost,K,A} || {Cost,K,A} <- Best],
    {Total,_,Rot,Split} = lists:min(Layouts),
    {Total,Rot,Split}.

min_side(Half) when Half >= 4 -> 2;
min_side(_) -> 1.

%% turning(I, N, Pts, Nrm) -> Radians
%%  Signed turn of the outline at ring vertex I: positive for a convex
%%  vertex, negative for a reflex one.
turning(I, N, Pts, Nrm) ->
    Prev = element((I-1+N) rem N + 1, Pts),
    Cur = element(I+1, Pts),
    Next = element((I+1) rem N + 1, Pts),
    Ein = e3d_vec:sub(Cur, Prev),
    Eout = e3d_vec:sub(Next, Cur),
    Y = e3d_vec:dot(Nrm, e3d_vec:cross(Ein, Eout)),
    X = e3d_vec:dot(Ein, Eout),
    math:atan2(Y, X).

%% cumulative(Pts, N) -> tuple of N+1 lengths
%%  Length of the outline from ring vertex 0 to each ring vertex.
cumulative(Pts, N) ->
    Lens = [e3d_vec:dist(element(I+1, Pts), element((I+1) rem N + 1, Pts)) ||
               I <- lists:seq(0, N-1)],
    list_to_tuple(cumulative_1(Lens, 0.0, [0.0])).

cumulative_1([L|Ls], Acc, Out) ->
    Sum = Acc + L,
    cumulative_1(Ls, Sum, [Sum|Out]);
cumulative_1([], _, Out) ->
    lists:reverse(Out).

%% Length of the outline going forward from I to J.
arc(I, J, N, Cum) ->
    Total = element(N+1, Cum),
    Diff = element(J rem N + 1, Cum) - element(I rem N + 1, Cum),
    arc_1(Diff < 0.0, Diff, Total).

arc_1(true, Diff, Total) -> Diff + Total;
arc_1(false, Diff, _) -> Diff.

%%  Lower is better: corners close to 90 degrees, opposite sides of
%%  similar length, and edge counts A:B that match the side lengths.
corner_cost(K, A, B, N, Turn, Cum) ->
    Bot = arc(K, K+A, N, Cum),
    Right = arc(K+A, K+A+B, N, Cum),
    Top = arc(K+A+B, K+2*A+B, N, Cum),
    Left = arc(K+2*A+B, K+N, N, Cum),
    Corners = [K,K+A,K+A+B,K+2*A+B],
    Angles = [abs(?HALF_PI - element(I rem N + 1, Turn)) || I <- Corners],
    AngSum = lists:sum(Angles),
    Corner = AngSum / ?HALF_PI,
    Sides = side_diff(Bot, Top) + side_diff(Right, Left),
    Hor = max((Bot+Top) / 2, ?EPSILON1),
    Ver = max((Right+Left) / 2, ?EPSILON1),
    Aspect = abs(math:log((A/B) * (Ver/Hor))),
    Corner + 2.0*Sides + Aspect.

side_diff(La, Lb) ->
    abs(La-Lb) / max(La+Lb, ?EPSILON1).

%%  Build the plain (Coons) patch for corner K and first side A, without
%%  touching the mesh, and measure its cells: a folded or flat cell
%%  corner costs a lot, the rest cost more the further they are from a
%%  right angle.
layout_cost(Pp, K, A) ->
    N = tuple_size(Pp),
    {Head,Tail} = lists:split(K, tuple_to_list(Pp)),
    S = patch_sides(list_to_tuple(Tail ++ Head), A, N div 2 - A),
    Plan = patch_positions(S, 0.0, 0.0),
    Js = grid_jacobians(Plan, S),
    Folded = length([J || J <- Js, J =< 0.0]),
    JsSum = lists:sum(Js),
    JsMin = lists:min(Js),
    100.0*Folded + (1.0 - JsMin) + (1.0 - JsSum/length(Js)).

grid_jacobians(Plan, #{a:=A,b:=B,fnrm:=Nrm}=S) ->
    lists:append([cell_jacobians(R, C, Nrm, Plan, S) ||
                     R <- lists:seq(0, B-1), C <- lists:seq(0, A-1)]).

cell_jacobians(R, C, Nrm, Plan, S) ->
    Ps = [at(R, C, Plan, S),at(R, C+1, Plan, S),
          at(R+1, C+1, Plan, S),at(R+1, C, Plan, S)],
    [corner_jacobian(I, Ps, Nrm) || I <- lists:seq(0, 3)].

%% corner_jacobian(I, Ps, Nrm) -> sin of the corner angle, signed
%%  1.0 for a right angle, 0.0 for a flat corner, negative when folded.
corner_jacobian(I, Ps, Nrm) ->
    Cur = lists:nth(I+1, Ps),
    Next = lists:nth((I+1) rem 4 + 1, Ps),
    Prev = lists:nth((I+3) rem 4 + 1, Ps),
    Ea = e3d_vec:sub(Next, Cur),
    Eb = e3d_vec:sub(Prev, Cur),
    Len = max(e3d_vec:len(Ea) * e3d_vec:len(Eb), ?EPSILON1),
    e3d_vec:dot(Nrm, e3d_vec:cross(Ea, Eb)) / Len.

%% fractions(SideA, SideB) -> tuple of arc-length fractions (0.0 to 1.0)
%%  Where each vertex of two opposite sides lies along its side,
%%  averaged, so that the interior grid lines follow the spacing of the
%%  border vertices instead of assuming that it is even.
fractions(SideA, SideB) ->
    Fa = side_fractions(tuple_to_list(SideA)),
    Fb = side_fractions(tuple_to_list(SideB)),
    list_to_tuple([(Xa+Xb) / 2 || {Xa,Xb} <- lists:zip(Fa, Fb)]).

side_fractions(Pts) ->
    PaL = lists:droplast(Pts),
    PbL = tl(Pts),
    Lens = [e3d_vec:dist(Pa, Pb) || {Pa,Pb} <- lists:zip(PaL, PbL)],
    Total = lists:sum(Lens),
    side_fractions_1(Total > ?EPSILON1, Lens, Total).

side_fractions_1(true, Lens, Total) ->
    fractions_acc(Lens, Total, 0.0, [0.0]);
side_fractions_1(false, Lens, _) ->
    Count = length(Lens),
    [(I/Count) || I <- lists:seq(0, Count)].

fractions_acc([L|Ls], Total, Acc, Out) ->
    Sum = Acc + L,
    fractions_acc(Ls, Total, Sum, [(Sum/Total)|Out]);
fractions_acc([], _, _, Out) ->
    lists:reverse(Out).

%%%
%%% Corner cuts
%%%

%% cut_corners(Face, Opts, We) -> {We,Pieces}
%%  Split off the corners of the outline that are too sharp or too
%%  reflex with new edges between ring vertices, so the border itself
%%  is never changed. Each cut edge is divided into M parts so that
%%  both pieces have an even number of edges and can be filled with
%%  quads; the new vertices follow the curvature given by the surface
%%  normals at the two ends. Done whenever the corner angle (degrees,
%%  Opts key angle, default ?DEFAULT_ANGLE, not in the dialog) is above 0.
cut_corners(Face, Opts, We) ->
    Angle = maps:get(angle, Opts, ?DEFAULT_ANGLE),
    cut_corners_1(Angle > 0, Face, Angle, Opts, We).

cut_corners_1(true, Face, Angle, Opts, We) ->
    cut_face(Face, Angle * math:pi() / 180.0, Opts, ?CUT_DEPTH, We);
cut_corners_1(false, Face, _, _, We) ->
    {We,[Face]}.

%%  Cut one diagonal, then look at each piece the same way.
cut_face(Face, _, _, 0, We) ->
    {We,[Face]};
cut_face(Face, Angle, Opts, Depth, We0) ->
    Ring = wings_face:vertices_ccw(Face, We0),
    case plan_ubay(Ring, Face, We0) of
        none ->
            cut_face_1(Face, Ring, Angle, Opts, Depth, We0);
        {I,J,Points,Bay} ->
            close_bay(Face, Ring, I, J, Points, Bay, Angle, Opts, Depth, We0)
    end.

cut_face_1(Face, Ring, Angle, Opts, Depth, We0) ->
    case plan_any(Ring, Angle, Face, We0) of
        none ->
            {We0,[Face]};
        {I,J,Points} ->
            {We1,Pieces} = apply_cut(Face, Ring, I, J, Points, We0),
            Cut = fun(Piece, {We2,Acc}) ->
                      {We3,Fs} = cut_face(Piece, Angle, Opts, Depth-1, We2),
                      {We3,Fs ++ Acc}
                  end,
            lists:foldl(Cut, {We1,[]}, Pieces)
    end.

%%%
%%% Bays (re-entrances shaped like a U)
%%%

%% Ring vertices are classified by the angle of the hole at them:
%% reflex (r, near 270 degrees), straight (s, around 180) and
%% convex (c, near 90).
vertex_class(Angle) ->
    Deg = Angle * 180.0 / math:pi(),
    vertex_class_1(Deg).

vertex_class_1(Deg) when Deg > ?REFLEX_DEG -> r;
vertex_class_1(Deg) when Deg < ?CONVEX_DEG -> c;
vertex_class_1(_) -> s.

%% plan_ubay(Ring, Face, We) -> none | {I,J,Points,Bay}
%%  A bay is built on a base: two consecutive convex corners C1 and C2
%%  with only straight vertices between them (b edges). The walls go
%%  on from the corners through straight vertices, backwards from C1
%%  (d edges, ending at E) and forwards from C2 (u edges, ending at X).
%%  The shorter wall (r edges) has to end at a reflex vertex, the other
%%  one can be longer. The vertex P, r edges before C1, is joined to the
%%  vertex Q, r edges after C2, and the closed bay is an r x b block of
%%  quads: the new edge is divided in b parts.
%%  The smallest bay is closed first. Closing it changes the classes
%%  around it, so the ring is analysed again afterwards.
plan_ubay(Ring, _, _) when length(Ring) < 6 ->
    none;
plan_ubay(Ring, Face, We) ->
    N = length(Ring),
    Cls = list_to_tuple([vertex_class(A) || A <- hole_angles(Ring, Face, We)]),
    Corners = [I || I <- lists:seq(0, N-1), element(I+1, Cls) =:= c],
    Pp = ring_tuple(Ring, We),
    P2 = project_ring(Pp, plane_basis(newell(tuple_to_list(Pp)))),
    Bays = [Bay || {C1,C2} <- corner_pairs(Corners), Bay <- ubay(C1, C2, N, Cls, Pp, P2)],
    ubay_pick(lists:sort(Bays), Ring, Pp, Face, We).

corner_pairs([_]) ->
    [];
corner_pairs([]) ->
    [];
corner_pairs([First|_]=Corners) ->
    lists:zip(Corners, tl(Corners) ++ [First]).

ubay_pick([], _, _, _, _) ->
    none;
ubay_pick([{_,_,P,Q,C1,_,R,B}|_], Ring, _, Face, We) ->
    K = B - 1,
    Points = bay_points(P, Q, K, Ring, Face, We),
    {P,Q,Points,{lists:nth(C1+1, Ring),R,B}}.

%% ubay(C1, C2, N, Cls, Pp, P2) -> [{Size,L,P,Q,C1,C2,R,B}]
ubay(C1, C2, N, Cls, Pp, P2) ->
    B = (C2 - C1 + N) rem N,
    Straight =
        lists:all(fun(K) ->
                      element((C1+K) rem N + 1, Cls) =:= s
                  end, lists:seq(1, B-1)),
    ubay_1(Straight, B, C1, C2, N, Cls, Pp, P2).

ubay_1(true, B, C1, C2, N, Cls, Pp, P2) ->
    D = wall_length(C1, -1, N, Cls),
    U = wall_length(C2, 1, N, Cls),
    R = min(D, U),
    E = (C1 - D + N) rem N,
    X = (C2 + U) rem N,
    EndOk = wall_end_ok(D, U, element(E+1, Cls), element(X+1, Cls)),
    P = (C1 - R + N) rem N,
    Q = (C2 + R) rem N,
    Fits = N - 2 * R >= 4 andalso B + D + U < N,
    Ok = EndOk andalso Fits andalso ubay_ok(P, Q, C1, C2, N, Pp, P2),
    ubay_2(Ok, {R*B,B+2*R,P,Q,C1,C2,R,B});
ubay_1(false, _, _, _, _, _, _, _) ->
    [].

%% Edges from Corner to the first vertex that is not straight,
%% walking in direction Step.
wall_length(Corner, Step, N, Cls) ->
    wall_length(Corner, Step, N, Cls, 1).

wall_length(Corner, Step, N, Cls, K) when K < N ->
    Class = element((Corner + Step*K + 10*N) rem N + 1, Cls),
    wall_length_1(Class, Corner, Step, N, Cls, K);
wall_length(_, _, _, _, K) ->
    K.

wall_length_1(s, Corner, Step, N, Cls, K) ->
    wall_length(Corner, Step, N, Cls, K+1);
wall_length_1(_, _, _, _, _, K) ->
    K.

%%  The end of the shorter wall must be a reflex vertex.
wall_end_ok(D, U, ClassE, _) when D < U -> ClassE =:= r;
wall_end_ok(D, U, _, ClassX) when U < D -> ClassX =:= r;
wall_end_ok(_, _, ClassE, ClassX) -> ClassE =:= r orelse ClassX =:= r.

ubay_2(true, Bay) -> [Bay];
ubay_2(false, _) -> [].

%%  The new edge must run inside the hole, meet the walls at about a
%%  right angle and be about as long as the base.
ubay_ok(P, Q, C1, C2, N, Pp, P2) ->
    Pa = element(P+1, Pp),
    Pb = element(Q+1, Pp),
    Ca = element(C1+1, Pp),
    Cb = element(C2+1, Pp),
    Inside = diagonal_ok(min(P, Q), max(P, Q), P2, N),
    AngleP = vec_angle(Pa, element((P+1) rem N + 1, Pp), Pb),
    AngleQ = vec_angle(Pb, element((Q-1+N) rem N + 1, Pp), Pa),
    Ratio = e3d_vec:dist(Pa, Pb) / max(?EPSILON0, e3d_vec:dist(Ca, Cb)),
    Inside andalso right_angle(AngleP) andalso right_angle(AngleQ)
           andalso (Ratio > 0.5) andalso (Ratio < 2.0).

vec_angle(Corner, A, B) ->
    Va = unit(e3d_vec:sub(A, Corner)),
    Vb = unit(e3d_vec:sub(B, Corner)),
    math:acos(max(-1.0, min(1.0, e3d_vec:dot(Va, Vb)))).

right_angle(Angle) ->
    Deg = Angle * 180.0 / math:pi(),
    (Deg > 50.0) andalso (Deg < 130.0).

%%  K points on the new edge P-Q. When the ring edges that stay in the
%%  main polygon at both ends point along the chord, a Hermite curve
%%  continues them; otherwise a straight chord bent by the surface
%%  normals.
bay_points(P, Q, K, Ring, Face, We) ->
    N = length(Ring),
    Pos = fun(Idx) -> wings_vertex:pos(lists:nth((Idx + N) rem N + 1, Ring), We) end,
    Pa = Pos(P),
    Pb = Pos(Q),
    Chord = e3d_vec:sub(Pb, Pa),
    Len = e3d_vec:len(Chord),
    Cu = unit(Chord),
    Ua = unit(e3d_vec:sub(Pa, Pos(P-1))),
    Ub = unit(e3d_vec:sub(Pos(Q+1), Pb)),
    Da = e3d_vec:dot(Ua, Cu),
    Db = e3d_vec:dot(Ub, Cu),
    bay_points_1(Da > ?BLEND_MIN andalso Db > ?BLEND_MIN,
                 {Pa,Pb,e3d_vec:mul(blend_dir(Ua, Cu, Da), Len),
                  e3d_vec:mul(blend_dir(Ub, Cu, Db), Len)},
                 P, Q, K, Ring, Face, We).

bay_points_1(true, {Pa,Pb,Ta,Tb}, _, _, K, _, _, _) ->
    [hermite(Pa, Pb, Ta, Tb, T / (K + 1)) || T <- lists:seq(1, K)];
bay_points_1(false, _, P, Q, K, Ring, Face, We) ->
    cut_points(P, Q, K, Ring, Face, We).

%% blend_dir(U, Chord, Dot) -> unit vector
%%  Follows the ring edge U when it lies within ?BLEND_FULL of the
%%  chord and fades to the chord as the angle grows to ?BLEND_MIN.
blend_dir(U, Chord, Dot) ->
    W = max(0.0, min(1.0, (Dot - ?BLEND_MIN) / (?BLEND_FULL - ?BLEND_MIN))),
    unit(e3d_vec:add(e3d_vec:mul(U, W), e3d_vec:mul(Chord, 1.0 - W))).

%%  Connect P and Q, fill the bay with its own r x b grid (not affected
%%  by Rotation and Split) and go on with what is left of the face.
close_bay(Face, Ring, I, J, Points, {C1,R,B}, Angle, Opts, Depth, We0) ->
    {We1,Faces} = apply_cut(Face, Ring, I, J, Points, We0),
    BayFace = pick_face(Faces, [C1], We1),
    [Main] = Faces -- [BayFace],
    BayRing = wings_face:vertices_ccw(BayFace, We1),
    Start = index_of(C1, BayRing, 0),
    {We2,Bay} = fill_bay(length(BayRing), BayFace, rotate(BayRing, Start), B, R, Opts, We1),
    {We3,Rest} = cut_face(Main, Angle, Opts, Depth-1, We2),
    {We3,Bay ++ Rest}.

fill_bay(4, Face, _, _, _, _, We) ->
    {We,[Face]};
fill_bay(_, Face, Ring, A, B, Opts, We) ->
    fill_patch(Face, Ring, A, B, Opts, We).

%%  Re-entrances first (they are exact), then the cost based cut.
plan_any(Ring, Angle, Face, We) ->
    plan_any_1(plan_reentrance(Ring, Angle, Face, We), Ring, Angle, Face, We).

plan_any_1({I,J,K}, Ring, _, _, We) ->
    {I,J,hermite_points(I, J, K, Ring, We)};
plan_any_1(none, Ring, Angle, Face, We) ->
    plan_any_2(plan_cut(Ring, Angle, Face, We), Ring, Face, We).

plan_any_2({I,J,M}, Ring, Face, We) ->
    {I,J,cut_points(I, J, M, Ring, Face, We)};
plan_any_2(none, _, _, _) ->
    none.

%%%
%%% Re-entrances
%%%

%%  The angle of the hole at each ring vertex: a full turn minus the
%%  corners of the faces around the vertex, as the surface sees it.
hole_angles(Ring, Face, We) ->
    [hole_angle(V, Face, We) || V <- Ring].

hole_angle(V, Face, We) ->
    Fs = wings_vertex:fold(fun(_, F, _, A) -> [F|A] end, [], V, We),
    Corners = [corner_angle(F, V, We) || F <- Fs, F =/= Face, F >= 0],
    2 * math:pi() - lists:sum(Corners).

corner_angle(F, V, We) ->
    Vs = wings_face:vertices_ccw(F, We),
    N = length(Vs),
    I = index_of(V, Vs, 0),
    Prev = lists:nth((I + N - 1) rem N + 1, Vs),
    Next = lists:nth((I + 1) rem N + 1, Vs),
    P = wings_vertex:pos(V, We),
    A = unit(e3d_vec:sub(wings_vertex:pos(Prev, We), P)),
    B = unit(e3d_vec:sub(wings_vertex:pos(Next, We), P)),
    math:acos(max(-1.0, min(1.0, e3d_vec:dot(A, B)))).

index_of(V, [V|_], I) -> I;
index_of(V, [_|Vs], I) -> index_of(V, Vs, I+1).

%% plan_reentrance(Ring, Angle, Face, We) -> none | {I,J,K}
%%  A re-entrance runs between two consecutive reflex vertices (hole
%%  angle above 180 degrees plus Angle) with only non-reflex vertices
%%  between them. The path I..J has L edges; the edge that closes it
%%  gets K = L-3 new vertices, so the closed piece has two opposite
%%  sides of equal length and the main polygon keeps an even size.
%%  The shortest (innermost) one is taken first.
plan_reentrance(Ring, _, _, _) when length(Ring) rem 2 =/= 0; length(Ring) < 6 ->
    none;
plan_reentrance(Ring, Angle, Face, We) ->
    N = length(Ring),
    Limit = math:pi() + Angle,
    Reflex = [I || {I,A} <- lists:zip(lists:seq(0, N-1), hole_angles(Ring, Face, We)),
                   A > Limit],
    Pp = ring_tuple(Ring, We),
    P2 = project_ring(Pp, plane_basis(newell(tuple_to_list(Pp)))),
    Pairs = reentrance_pairs(Reflex, N),
    Good = [{L,I,J} || {I,J} <- Pairs, L <- [(J - I + N) rem N],
                       L >= 3, N - L >= 3, diagonal_ok(min(I, J), max(I, J), P2, N)],
    reentrance_pick(lists:sort(Good)).

reentrance_pairs([_], _) ->
    [];
reentrance_pairs([], _) ->
    [];
reentrance_pairs([First|_]=Reflex, _) ->
    lists:zip(Reflex, tl(Reflex) ++ [First]).

reentrance_pick([{L,I,J}|_]) ->
    {I,J,L-3};
reentrance_pick([]) ->
    none.

%%  K points from ring vertex I to J on a Hermite curve whose end
%%  tangents continue the ring edges that stay in the main polygon:
%%  the one coming into I and the one leaving J.
hermite_points(I, J, K, Ring, We) ->
    N = length(Ring),
    Pos = fun(Idx) -> wings_vertex:pos(lists:nth((Idx + N) rem N + 1, Ring), We) end,
    Pa = Pos(I),
    Pb = Pos(J),
    Chord = e3d_vec:sub(Pb, Pa),
    Len = e3d_vec:len(Chord),
    Ta = tangent(e3d_vec:sub(Pa, Pos(I-1)), Chord, Len),
    Tb = tangent(e3d_vec:sub(Pos(J+1), Pb), Chord, Len),
    [hermite(Pa, Pb, Ta, Tb, T / (K + 1)) || T <- lists:seq(1, K)].

%%  A tangent that points forward along the chord; the chord itself
%%  when the ring edge does not.
tangent(Dir, Chord, Len) ->
    U = unit(Dir),
    Fwd = e3d_vec:dot(U, unit(Chord)) > 0.2,
    e3d_vec:mul(choose_dir(Fwd, U, unit(Chord)), Len).

choose_dir(true, U, _) -> U;
choose_dir(false, _, C) -> C.

hermite(Pa, Pb, Ta, Tb, T) ->
    T2 = T * T,
    T3 = T2 * T,
    H00 = 2*T3 - 3*T2 + 1,
    H10 = T3 - 2*T2 + T,
    H01 = -2*T3 + 3*T2,
    H11 = T3 - T2,
    lists:foldl(fun e3d_vec:add/2, {0.0,0.0,0.0},
                [e3d_vec:mul(Pa, H00),e3d_vec:mul(Ta, H10),
                 e3d_vec:mul(Pb, H01),e3d_vec:mul(Tb, H11)]).

%% plan_cut(Ring, Angle, Face, We) -> none | {I,J,M}
%%  The cheapest diagonal from a critical vertex. Critical vertices are
%%  always cut when a diagonal exists; the corner angle sets which ones
%%  are critical.
plan_cut(Ring, _, _, _) when length(Ring) rem 2 =/= 0; length(Ring) < 6 ->
    none;
plan_cut(Ring, Angle, Face, We) ->
    Pp = ring_tuple(Ring, We),
    Crit = critical_vertices(Pp, Angle),
    plan_cut_1(Crit, Ring, Pp, Angle, Face, We).

plan_cut_1([], _, _, _, _, _) ->
    none;
plan_cut_1(Crit, Ring, Pp, Angle, Face, We) ->
    N = tuple_size(Pp),
    Reflex = reflex_vertices(Pp),
    Cands = cut_candidates(Crit, Pp, N),
    Scored = [{cut_cost(I, J, M, Pp, Ring, Face, We) - neck_bonus(I, J, Reflex),I,J,M} ||
                 {I,J} <- Cands, M <- cut_counts(I, J)],
    Top = lists:sublist(lists:sort(Scored), ?DEEP_TOP),
    Deep = [{deep_cost(I, J, M, Pp, Ring, Angle, Face, We) - neck_bonus(I, J, Reflex),I,J,M} ||
               {_,I,J,M} <- Top],
    pick_cut(lists:sort(Deep)).

%%  Vertices that turn the wrong way by more than REFLEX_MIN radians.
reflex_vertices(Pp) ->
    N = tuple_size(Pp),
    Nrm = newell(tuple_to_list(Pp)),
    [I || I <- lists:seq(0, N-1), turning(I, N, Pp, Nrm) < -?REFLEX_MIN].

%%  A cut between two reflex vertices closes a neck and cuts off a tab,
%%  which is the cut one makes by hand.
neck_bonus(I, J, Reflex) ->
    neck_bonus_1(lists:member(I, Reflex) andalso lists:member(J, Reflex)).

neck_bonus_1(true) -> ?NECK_BONUS;
neck_bonus_1(false) -> 0.0.

pick_cut([{_,I,J,M}|_]) ->
    {I,J,M};
pick_cut([]) ->
    none.

%%  Numbers of new vertices on the cut that make both pieces even.
cut_counts(I, J) ->
    Base = (J - I + 1) rem 2,
    [Base,Base+2].

%%  The worse of the costs of the two pieces made by the cut I-J with M
%%  new vertices, plus a small price for the cut. A piece that is
%%  already a quad costs nothing.
cut_cost(I, J, M, Pp, Ring, Face, We) ->
    N = tuple_size(Pp),
    Pts = tuple_to_list(Pp),
    Cut = cut_points(I, J, M, Ring, Face, We),
    P1 = sub_ring(Pts, I, J, N) ++ lists:reverse(Cut),
    P2 = sub_ring(Pts, J, I, N) ++ Cut,
    max(piece_cost(P1), piece_cost(P2)) + cut_extra(P1, P2, M).

%%  Price of a cut: a fixed part plus how far the four angles it makes
%%  at its ends are from a right angle or a straight line, which is
%%  what a grid wants at a corner or along a side. Extending a border
%%  edge from a reflex vertex makes 90 + 180 degrees and costs nothing.
cut_extra(P1, P2, M) ->
    Angles = [end_angle_cost(P1, 0), end_angle_cost(P1, length(P1) - M - 1),
              end_angle_cost(P2, 0), end_angle_cost(P2, length(P2) - M - 1)],
    ?CUT_PENALTY + ?ANGLE_WEIGHT * lists:sum(Angles) / 4.0.

end_angle_cost(Pts, I) ->
    Pp = list_to_tuple(Pts),
    Nrm = newell(Pts),
    Angle = math:pi() - turning(I, tuple_size(Pp), Pp, Nrm),
    Dev = min(abs(Angle - ?HALF_PI), abs(Angle - math:pi())),
    min(1.0, (Dev/?HALF_PI)).

piece_cost(Pts) when length(Pts) =:= 4 ->
    layout_cost(list_to_tuple(Pts), 0, 1);
piece_cost(Pts) ->
    {Cost,_,_} = best_layout(list_to_tuple(Pts)),
    Cost.

%%  Like cut_cost/7, but each piece may be cut once more (with a
%%  straight edge), so a corner that stays in one piece does not hide
%%  how good the first cut is.
deep_cost(I, J, M, Pp, Ring, Angle, Face, We) ->
    N = tuple_size(Pp),
    Pts = tuple_to_list(Pp),
    Cut = cut_points(I, J, M, Ring, Face, We),
    P1 = sub_ring(Pts, I, J, N) ++ lists:reverse(Cut),
    P2 = sub_ring(Pts, J, I, N) ++ Cut,
    max(piece_cost_cut(P1, Angle), piece_cost_cut(P2, Angle)) + cut_extra(P1, P2, M).

%%  The cheaper of filling the piece as it is and cutting it once.
piece_cost_cut(Pts, _) when length(Pts) < 6 ->
    piece_cost(Pts);
piece_cost_cut(Pts, Angle) ->
    Pp = list_to_tuple(Pts),
    N = tuple_size(Pp),
    Whole = piece_cost(Pts),
    Crit = critical_vertices(Pp, Angle),
    Cands = cut_candidates(Crit, Pp, N),
    Cuts = [straight_cost(I, J, M, Pts, N) || {I,J} <- Cands, M <- [(J - I + 1) rem 2]],
    lists:min([Whole|Cuts]).

straight_cost(I, J, M, Pts, N) ->
    Pa = lists:nth(I+1, Pts),
    Pb = lists:nth(J+1, Pts),
    Cut = [e3d_vec:add(Pa, e3d_vec:mul(e3d_vec:sub(Pb, Pa), K / (M + 1))) ||
              K <- lists:seq(1, M)],
    P1 = sub_ring(Pts, I, J, N) ++ lists:reverse(Cut),
    P2 = sub_ring(Pts, J, I, N) ++ Cut,
    max(piece_cost(P1), piece_cost(P2)) + cut_extra(P1, P2, M).

%% The elements from index I forward to J.
sub_ring(List, I, J, N) ->
    {Head,Tail} = lists:split(I, List),
    lists:sublist(Tail ++ Head, (J - I + N) rem N + 1).

%%  M points from ring vertex I to ring vertex J. They lie on the chord
%%  pushed along the average surface normal by the sagitta of an arc
%%  that turns as much as the two end normals, bulging outwards when
%%  the surface is convex.
cut_points(I, J, M, Ring, Face, We) ->
    Va = lists:nth(I+1, Ring),
    Vb = lists:nth(J+1, Ring),
    Pa = wings_vertex:pos(Va, We),
    Pb = wings_vertex:pos(Vb, We),
    Na = vertex_normal(Va, Face, We),
    Nb = vertex_normal(Vb, Face, We),
    Sag = sagitta(Pa, Pb, Na, Nb),
    Dir = unit(e3d_vec:add(Na, Nb)),
    Pt = fun(K) ->
             T = K / (M + 1),
             Lin = e3d_vec:add(e3d_vec:mul(Pa, 1.0 - T), e3d_vec:mul(Pb, T)),
             e3d_vec:add(Lin, e3d_vec:mul(Dir, Sag * 4.0 * T * (1.0 - T)))
         end,
    [Pt(K) || K <- lists:seq(1, M)].

sagitta(Pa, Pb, Na, Nb) ->
    Cos = max(-1.0, min(1.0, e3d_vec:dot(Na, Nb))),
    Turn = math:acos(Cos),
    Convex = e3d_vec:dot(e3d_vec:sub(Nb, Na), e3d_vec:sub(Pb, Pa)) > 0.0,
    Len = e3d_vec:dist(Pa, Pb),
    sign(Convex) * Len * Turn / 8.0.

sign(true) -> 1.0;
sign(false) -> -1.0.

%%  Average normal of the faces around V, not counting the face that
%%  is being filled.
vertex_normal(V, Face, We) ->
    Fs = wings_vertex:fold(fun(_, F, _, A) -> [F|A] end, [], V, We),
    Ns = [wings_face:normal(F, We) || F <- Fs, F =/= Face, F >= 0],
    unit(lists:foldl(fun e3d_vec:add/2, {0.0,0.0,0.0}, Ns)).

%%  Connect the two ring vertices and divide the new edge at the points
%%  of Cut (ordered from vertex I to vertex J).
apply_cut(Face, Ring, I, J, Cut, We0) ->
    Va = lists:nth(I+1, Ring),
    Vb = lists:nth(J+1, Ring),
    {We1,New} = wings_vertex:force_connect(Va, Vb, Face, We0),
    [{Edge,_,_}|_] = wings_vertex:edge_through(Va, Vb, We1),
    #edge{vs=Vs} = array:get(Edge, We1#we.es),
    %% Edge keeps the part next to its start vertex, so cut from the
    %% far end.
    Order = far_first(Vs =:= Va, Cut),
    We =
        lists:foldl(fun(Pos, W) ->
                        {W1,_} = wings_edge:fast_cut(Edge, Pos, W),
                        W1
                    end, We1, Order),
    {We,[Face,New]}.

far_first(true, Cut) -> lists:reverse(Cut);
far_first(false, Cut) -> Cut.

%%  Vertices whose interior angle is smaller than Angle, or larger than
%%  180 degrees plus Angle (radians).
critical_vertices(Pp, Angle) ->
    N = tuple_size(Pp),
    Nrm = newell(tuple_to_list(Pp)),
    [I || I <- lists:seq(0, N-1), critical(turning(I, N, Pp, Nrm), Angle)].

critical(Turn, Angle) ->
    Turn > math:pi() - Angle orelse Turn < -Angle.

%%  Diagonals that leave two pieces of at least 3 ring edges and run
%%  inside the outline: from each critical vertex to every other vertex
%%  that is not a neighbor.
cut_candidates(Crit, Pp, N) ->
    Far = [norm_cut(I, I+D, N) || I <- Crit, D <- lists:seq(3, N-3)],
    All = lists:usort(Far),
    Basis = plane_basis(newell(tuple_to_list(Pp))),
    P2 = project_ring(Pp, Basis),
    [{I,J} || {I,J} <- All, diagonal_ok(I, J, P2, N)].

norm_cut(I, J, N) ->
    A = (I + N) rem N,
    B = (J + N) rem N,
    {min(A, B),max(A, B)}.

%% Unit normal of the polygon (counter-clockwise outward)
newell(Pts) ->
    unit(newell_1(Pts, hd(Pts), {0.0,0.0,0.0})).

newell_1([Pa,Pb|Pts], First, Sum) ->
    newell_1([Pb|Pts], First, newell_add(Pa, Pb, Sum));
newell_1([Last], First, Sum) ->
    newell_add(Last, First, Sum).

newell_add({Xa,Ya,Za}, {Xb,Yb,Zb}, {Nx,Ny,Nz}) ->
    {Nx + (Ya-Yb)*(Za+Zb),Ny + (Za-Zb)*(Xa+Xb),Nz + (Xa-Xb)*(Ya+Yb)}.

%% Two unit vectors spanning the plane.
plane_basis(Nrm) ->
    {Nx,_,_} = Nrm,
    Ref = ref_axis(abs(Nx) < 0.9),
    U = unit(e3d_vec:cross(Ref, Nrm)),
    {U,e3d_vec:cross(Nrm, U)}.

ref_axis(true) -> {1.0,0.0,0.0};
ref_axis(false) -> {0.0,1.0,0.0}.

project_ring(Pp, {U,V}) ->
    list_to_tuple([{e3d_vec:dot(P, U),e3d_vec:dot(P, V)} || P <- tuple_to_list(Pp)]).

%%  The diagonal I-J (projected ring P2) crosses no ring edge and its
%%  middle is inside the outline.
diagonal_ok(I, J, P2, N) ->
    Pi = element(I+1, P2),
    Pj = element(J+1, P2),
    {Xi,Yi} = Pi,
    {Xj,Yj} = Pj,
    Mid = {(Xi+Xj) / 2,(Yi+Yj) / 2},
    edges_clear(0, N, [I,J], Pi, Pj, P2) andalso inside(0, N, Mid, P2, false).

edges_clear(K, N, _, _, _, _) when K >= N ->
    true;
edges_clear(K, N, Skip, Pi, Pj, P2) ->
    Next = (K+1) rem N,
    Touch = lists:member(K, Skip) orelse lists:member(Next, Skip),
    Clear = Touch orelse not crosses(Pi, Pj, element(K+1, P2), element(Next+1, P2)),
    Clear andalso edges_clear(K+1, N, Skip, Pi, Pj, P2).

crosses(P1, P2, P3, P4) ->
    D1 = orient(P3, P4, P1),
    D2 = orient(P3, P4, P2),
    D3 = orient(P1, P2, P3),
    D4 = orient(P1, P2, P4),
    D1*D2 < 0.0 andalso D3*D4 < 0.0.

orient({Xa,Ya}, {Xb,Yb}, {Xc,Yc}) ->
    (Xb-Xa)*(Yc-Ya) - (Yb-Ya)*(Xc-Xa).

inside(K, N, _, _, In) when K >= N ->
    In;
inside(K, N, {X,Y}=Pt, P2, In) ->
    {Xa,Ya} = element(K+1, P2),
    {Xb,Yb} = element((K+1) rem N + 1, P2),
    Cross = (Ya > Y) =/= (Yb > Y) andalso X < (Xb-Xa)*(Y-Ya)/(Yb-Ya) + Xa,
    inside(K+1, N, Pt, P2, In =/= Cross).
