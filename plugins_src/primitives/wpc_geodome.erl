%%
%%  wpc_geodome.erl --
%%
%%     Geodesic Dome Plugin
%%
%%  Copyright (c) 2003-2011 Anthony D'Agostino
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_geodome).
-export([init/0, menu/2, command/2]).
-include_lib("wings/src/wings.hrl").

init() -> true.

menu({shape}, []) ->
    geodome_menu();
menu({shape}, Menu) ->
    Menu ++ [separator|geodome_menu()];
menu(_, Menu) -> Menu.

geodome_menu() ->
    [{?__(1,"GeoDome"), geodome, [option]}].

command({shape,{geodome, Arg}}, St) -> make_geodome(Arg, St);
command(_, _) -> next.

%%% The rest are local functions.

make_geodome(Arg, St) when is_atom(Arg) ->
    wings_dialog:dialog_preview({shape,geodome}, Arg, ?__(1,"Geodesic Dome Options"), dialog(), St);
make_geodome(Arg, _) ->
    %set_pref(Arg), % save preferences
    ArgDict = dict:from_list(Arg),
    BaseFlag = dict:fetch(baseflag, ArgDict),
    DomeFlag = dict:fetch(domeflag, ArgDict),
    Resolution = dict:fetch(resolution, ArgDict),
    SpherizeFlag = dict:fetch(spherizeflag, ArgDict),
    AlgorithmFlag = dict:fetch(algorithmflag, ArgDict),
    Rot_X = dict:fetch(rot_x, ArgDict),
    Rot_Y = dict:fetch(rot_y, ArgDict),
    Rot_Z = dict:fetch(rot_z, ArgDict),
    Mov_X = dict:fetch(mov_x, ArgDict),
    Mov_Y = dict:fetch(mov_y, ArgDict),
    Mov_Z = dict:fetch(mov_z, ArgDict),
    Ground = dict:fetch(ground, ArgDict),
    CreateUV = dict:fetch(create_uv, ArgDict),

    {Verts, Faces0} = geodome_main(Resolution, AlgorithmFlag, BaseFlag, SpherizeFlag, DomeFlag),
    Vs = wings_shapes:transform_obj({Rot_X,Rot_Y,Rot_Z},{Mov_X,Mov_Y,Mov_Z},Ground, Verts),
    [H|_] = atom_to_list(AlgorithmFlag),
    Name = ?__(2,":GeoDome"),
    ObjName = lists:concat([[H-32], Resolution, Name]),
    Base = {base_shape(BaseFlag, DomeFlag), base_shape(BaseFlag, false)},
    Faces = geodome_uv_faces(CreateUV, SpherizeFlag, Faces0, Verts, Base),
    {new_shape, ObjName, Faces, Vs}.

%% UV layout: a flat geodome is unfolded like the polyhedron, its coplanar
%% faces together; a spherized one is cut along the edges of the regular
%% polyhedron, its faces in pairs across their most horizontal edge when the
%% pair fits in a hemisphere, each piece projected around its center. The
%% faces of the inside of a dome make pieces of their own.
geodome_uv_faces(false, _, Fs, _, _) -> Fs;
geodome_uv_faces(true, false, Fs, Vs, _) ->
    wings_shapes_uv:pack(Fs, wings_shapes_uv:unfold(Fs, list_to_tuple(Vs)));
geodome_uv_faces(true, true, Fs, Vs, {{BaseVs, BaseFs}, {RegVs, RegFs}}) ->
    Vt = list_to_tuple(Vs),
    FsT = list_to_tuple(Fs),
    K = length(Fs) div length(BaseFs),
    Piece = fun(I) -> [element(I*K + J + 1, FsT) || J <- lists:seq(0, K-1)] end,
    Regions = base_regions(BaseVs, BaseFs, RegVs, RegFs),
    Groups = base_groups(RegVs, RegFs),
    Keys = lists:usort([{G, Out} || {{R, Out}, _} <- maps:to_list(Regions), G <- Groups, lists:member(R, G)]),
    Faces =
        fun({G, Out}) ->
            lists:append([Piece(I) || R <- G, I <- maps:get({R, Out}, Regions, [])])
        end,
    Islands = [wings_shapes_uv:azimuthal(Faces(Key), Vt) || Key <- Keys],
    wings_shapes_uv:pack(Fs, Islands).

%% The faces of the subdivision come in the order of the base faces, so each
%% base face is an index. The regions are the base faces by the face of the
%% regular polyhedron they look at and by the side they look to:
%% #{{RegularFace, Outwards} => [BaseFace]}.
base_regions(BaseVs, BaseFs, RegVs, RegFs) ->
    Centers = list_to_tuple([e3d_vec:norm(e3d_vec:average(Ps)) || Ps <- base_points(RegVs, RegFs)]),
    Nearest =
        fun(C) ->
            {_, R} = lists:max([{e3d_vec:dot(C, element(R+1, Centers)), R} || R <- lists:seq(0, tuple_size(Centers)-1)]),
            R
        end,
    Add =
        fun({I, Ps}, Map) ->
            C = e3d_vec:norm(e3d_vec:average(Ps)),
            Key = {Nearest(C), e3d_vec:dot(e3d_vec:normal(Ps), C) > 0},
            Map#{Key => maps:get(Key, Map, []) ++ [I]}
        end,
    lists:foldl(Add, #{}, lists:zip(lists:seq(0, length(BaseFs)-1), base_points(BaseVs, BaseFs))).

base_points(Vs, Fs) ->
    Vt = list_to_tuple(Vs),
    [[element(V+1, Vt) || V <- F] || F <- Fs].

%% The faces of the regular polyhedron in pairs: [[I,J]] or [[I]].
base_groups(Vs, Fs) ->
    Vt = list_to_tuple(Vs),
    Infos = list_to_tuple([{F, e3d_vec:norm(e3d_vec:average(Ps))} || {F, Ps} <- lists:zip(Fs, base_points(Vs, Fs))]),
    Pair =
        fun(I, {Used, Acc}) ->
            case gb_sets:is_member(I, Used) of
                true -> {Used, Acc};
                false -> base_pair(I, Infos, Vt, Used, Acc)
            end
        end,
    {_, Groups} = lists:foldl(Pair, {gb_sets:empty(), []}, lists:seq(0, length(Fs)-1)),
    lists:reverse(Groups).

base_pair(I, Infos, Vt, Used, Acc) ->
    {F, Center} = element(I+1, Infos),
    Edges = lists:zip(F, tl(F) ++ [hd(F)]),
    Level = fun({A, B}) -> abs(element(2, element(A+1, Vt)) - element(2, element(B+1, Vt))) end,
    Candidates = [{Level(E), J} || E <- Edges, J <- base_neighbors(E, Infos),
                                  J =/= I, not gb_sets:is_member(J, Used),
                                  base_fits(Center, element(J+1, Infos), Vt)],
    case lists:sort(Candidates) of
        [{_, J}|_] -> {gb_sets:add(J, gb_sets:add(I, Used)), [[I, J] | Acc]};
        [] -> {gb_sets:add(I, Used), [[I] | Acc]}
    end.

base_neighbors({A, B}, Infos) ->
    [J || J <- lists:seq(0, tuple_size(Infos)-1),
          {F, _} <- [element(J+1, Infos)],
          lists:member(A, F), lists:member(B, F)].

%% Every vertex of the pair is within a quarter of a turn from the mean of
%% the centers of the two faces.
base_fits(Center, {F, Center2}, Vt) ->
    Mean = e3d_vec:norm(e3d_vec:add(Center, Center2)),
    Ds = [e3d_vec:dot(e3d_vec:norm(element(V+1, Vt)), Mean) || V <- F],
    lists:min(Ds) > -?EPSILON.

dialog() ->
    BaseFlag = get_pref(baseflag, icosahedron),
    DomeFlag = get_pref(domeflag, false),
    Resolution = get_pref(resolution, 3),
    SpherizeFlag = get_pref(spherizeflag, true),
    AlgorithmFlag = get_pref(algorithmflag, frequency),
    [{label_column, [
	{?__(1,"Resolution"), {slider, {text, Resolution,
	       [{key, resolution}, {range, {1, 30}}]}}}]},
     {hradio, [{?__(2,"Frequency (Edge-Cut Subdivision)"), frequency},
	       {?__(3,"Depth (Recursive Subdivision)"), depth}],
	       AlgorithmFlag,
	       [{key,algorithmflag}, {title, ?__(4,"Algorithm/Method of Subdivision")}]},
     {hradio, [{?__(5,"Icosahedron"), icosahedron},
	       {?__(6,"Octahedron"), octahedron},
	       {?__(7,"Tetrahedron"), tetrahedron}],
	       BaseFlag,
	       [{key,baseflag}, {title, ?__(8,"Base Type")}]},
     {hradio, [{?__(9,"Yes"), true},
	       {?__(10,"No"), false}],
	       SpherizeFlag,
	       [{key,spherizeflag}, {title, ?__(11,"Spherize")}]},
     {?__(12,"Generate Half-Dome"), DomeFlag, [{key, domeflag}]},
     wings_shapes_uv:create_uv_dlg(),
     wings_shapes:transform_obj_dlg()].

geodome_main(Resolution, AlgorithmFlag, BaseFlag, SpherizeFlag, DomeFlag) ->
    {Verts, Faces} = base_shape(BaseFlag, DomeFlag),
    RawTriangles1 = e3d_util:indexed_to_raw(Verts, Faces),
    case AlgorithmFlag of
	frequency ->
	    Frequency = Resolution,
	    RawTriangles2 = subdivide_rawtriangles_by_frequency(RawTriangles1, Frequency);
	depth ->
	    Depth = Resolution,
	    RawTriangles2 = subdivide_rawtriangles_by_depth(RawTriangles1, Depth)
    end,
    {Vs, Fs} = e3d_util:raw_to_indexed(RawTriangles2),
    case SpherizeFlag of
	true -> Vs2 = lists:map(fun e3d_vec:norm/1, Vs);
	false -> Vs2 = Vs
    end,
    {Vs2, Fs}.

base_shape(octahedron, DomeFlag) -> octahedron(DomeFlag);
base_shape(icosahedron, DomeFlag) -> icosahedron(DomeFlag);
base_shape(tetrahedron, DomeFlag) -> tetrahedron(DomeFlag).

icosahedron(false) ->
    H = math:sqrt(1/5),
    L = math:sqrt(4/5),
    S = math:pi()*2/5,
    A = L*math:cos(S),
    B = L*math:sin(S),
    C = L*math:cos(S*2),
    D = L*math:sin(S*2),
    E = 0.0,
    F = 1.0,
    Verts = [
    {E,H,L},{E,F,E},{B,H,A},{-E,-H,-L},{D,H,C},{-D,H,C},{B,-H,-A},
    {D,-H,-C},{-D,-H,-C},{-B,H,A},{E,-F,E},{-B,-H,-A}],
    Faces = [
    [1,0,2],[9,0,1],[1,5,9],[1,4,5],[1,2,4],[0,8,7],[9,11,8],
    [5,3,11],[4,6,3],[2,7,6],[3,5,4],[6,4,2],[2,0,7],[8,0,9],
    [11,9,5],[10,3,6],[10,6,7],[10,7,8],[10,8,11],[10,11,3]],
    {Verts, Faces};
icosahedron(true) ->
    Verts = [
    {0.000000,0.850651,0.525731},{0.000000,0.447214,0.894427},
    {-0.500000,0.525731,0.688191},{-0.500000,0.850651,0.162460},
    {-0.309017,0.850651,-0.425325},{0.000000,1.000000,0.000000},
    {0.309017,0.850651,-0.425325},{0.500000,0.850651,0.162460},
    {0.309017,-0.000000,-0.951057},{-0.309017,-0.000000,-0.951057},
    {0.525731,0.447214,-0.723607},{-0.000000,0.525731,-0.850651},
    {1.000000,-0.000000,0.000000},{0.809017,-0.000000,-0.587785},
    {0.809017,0.525731,-0.262866},{0.309017,-0.000000,0.951057},
    {0.809017,-0.000000,0.587785},{0.500000,0.525731,0.688191},
    {0.850651,0.447214,0.276393},{-0.809017,-0.000000,0.587785},
    {-0.309017,-0.000000,0.951057},{-0.809017,-0.000000,-0.587785},
    {-1.000000,-0.000000,0.000000},{-0.525731,0.447214,-0.723607},
    {-0.809017,0.525731,-0.262866},{-0.850651,0.447214,0.276393},
    {-0.825131,0.433797,0.268101},{-0.784746,0.509959,-0.254980},
    {-0.509959,0.433797,-0.701899},{0.825131,0.433797,0.268101},
    {0.485000,0.509959,0.667545},{0.784746,0.509959,-0.254980},
    {-0.000000,0.509959,-0.825131},{0.509959,0.433797,-0.701899},
    {0.485000,0.825131,0.157586},{0.299746,0.825131,-0.412566},
    {0.000000,0.970000,0.000000},{-0.299746,0.825131,-0.412566},
    {-0.485000,0.825131,0.157586},{-0.485000,0.509959,0.667545},
    {0.000000,0.433797,0.867594},{0.000000,0.825131,0.509959}],
    Faces = [
    [0,5,7],[18,17,7],[0,7,17],[1,0,17],[5,0,3],[2,0,1],
    [3,0,2],[2,25,3],[4,5,3],[25,24,3],[4,3,24],[24,23,4],[6,5,4],
    [23,11,4],[6,4,11],[11,10,6],[7,5,6],[10,14,6],[7,6,14],[14,18,7],
    [15,20,1],[19,22,25],[21,9,23],[8,13,10],[12,16,18],[10,11,8],
    [9,8,11],[11,23,9],[18,14,12],[13,12,14],[14,10,13],[1,17,15],
    [16,15,17],[17,18,16],[25,2,19],[20,19,2],[2,1,20],[23,24,21],
    [22,21,24],[24,25,22],[41,34,36],[29,34,30],[41,30,34],[40,30,41],
    [36,38,41],[39,40,41],[38,39,41],[39,38,26],[37,38,36],[26,38,27],
    [37,27,38],[27,37,28],[35,37,36],[28,37,32],[35,32,37],[32,35,33],
    [34,35,36],[33,35,31],[34,31,35],[31,34,29],[15,40,20],[19,26,22],
    [21,28,9],[8,33,13],[12,29,16],[33,8,32],[9,32,8],[32,9,28],
    [29,12,31],[13,31,12],[31,13,33],[40,15,30],[16,30,15],[30,16,29],
    [26,19,39],[20,39,19],[39,20,40],[28,21,27],[22,27,21],[27,22,26]],
    {Verts, Faces}.

octahedron(false) ->
    Verts = [
    {1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0},
    {-1.0,0.0,0.0},{0.0,-1.0,0.0},{0.0,0.0,-1.0}],
    Faces = [[1,2,0],[1,0,5],[1,5,3],[1,3,2],[4,0,2],[4,5,0],[4,3,5],[4,2,3]],
    {Verts, Faces};
octahedron(true) ->
    Verts = [
    {1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0},
    {-1.0,0.0,0.0},{0.0,0.9,0.0},{0.0,0.0,-1.0}], %% note y=0.9
    Faces = [[1,2,0],[1,0,5],[1,5,3],[1,3,2],[4,0,2],[4,5,0],[4,3,5],[4,2,3]],
    {Verts, Faces}.

tetrahedron(false) ->
    Verts = [
    {0.000000,0.612372,0.000000},{0.000000,-0.204124,0.577350},
    {-0.500000,-0.204124,-0.288675},{0.500000,-0.204124,-0.288675}],
    Faces = [[2,1,0],[3,2,0],[1,3,0],[2,3,1]],
    {Verts, Faces};
tetrahedron(true) ->
    Verts = [
    {0.000000,0.816496,0.000000},{0.000000,0.000000,0.577350},
    {-0.500000,0.000000,-0.288675},{0.500000,0.000000,-0.288675},
    {0.000000,0.816491,0.000000}],
    Faces = [[2,1,0],[3,2,0],[1,3,0],[1,2,4],[3,1,4],[2,3,4]],
    {Verts, Faces}.

% =====================================
% === Edge-Cut Subivision Functions ===
% =====================================
subdivide_rawtriangles_by_frequency(RawTriangles, Frequency) ->
    Process_Triangle = fun(Triangle) ->
	[V1,V2,V3] = Triangle,
	subdivide_triangle_by_frequency(V1, V2, V3, Frequency) end,
    A = lists:map(Process_Triangle, RawTriangles),
    RawTriangles2 = lists:append(A),
    RawTriangles2.

subdivide_triangle_by_frequency(V1, V2, V3, 1) ->
    [[V1,V2,V3]];
subdivide_triangle_by_frequency(V1, V2, V3, Frequency) ->
    NumCuts = Frequency,
    Verts = make_verts(V1, V2, V3, NumCuts),
    Faces = make_faces(NumCuts),
    RawTriangles = e3d_util:indexed_to_raw(Verts, Faces),
    RawTriangles.

% ==================
% === Make Faces ===
% ==================
make_faces(NumCuts) ->
    NumVerts = lists:sum(lists:seq(1, NumCuts+1)),
    DnRange = lists:seq(0, NumVerts-1*NumCuts-2),
    UpRange = lists:seq(0, NumVerts-2*NumCuts-2),
    DnFaces = make_faces_dn(DnRange),
    UpFaces = make_faces_up(UpRange),
    A = lists:append(DnFaces),
    B = lists:append(UpFaces),
    A ++ B.

make_faces_dn(Seq) ->
    make_faces_dn(Seq, 1).

make_faces_up(Seq) ->
    make_faces_up(Seq, 1).

make_faces_dn([], _) -> [];
make_faces_dn(Seq, N) ->
    {Head,Tail} = lists:split(N, Seq),
    L = length(Head),
    Face = [[I, I+L+1, I+L] || I <- Head],
    [Face | make_faces_dn(Tail, N+1)].

make_faces_up([], _) -> [];
make_faces_up(Seq, N) ->
    {Head,Tail} = lists:split(N, Seq),
    L = length(Head),
    Face = [[I+L, I+L+1, I+2*L+2] || I <- Head],
    [Face | make_faces_up(Tail, N+1)].

% ==================
% === Make Verts ===
% ==================
make_verts(V1, V2, V3, NumCuts) ->
    U = edge_cut_exact(V1, V3, NumCuts),
    V = edge_cut_exact(V1, V2, NumCuts),
    [A,B | TU] = U,
    [_,D | TV] = V,
    Verts1 = [A,B,D],
    Verts2 = make_span_verts(TU, TV, 2),
    Verts = lists:flatten([Verts1 ++ Verts2]),
    NumVerts = lists:sum(lists:seq(1, NumCuts+1)),
    true = length(Verts) == NumVerts, % verify sum
    Verts.

make_span_verts([], [], _) -> [];
make_span_verts(U, V, N) ->
    [H1|T1] = U,
    [H2|T2] = V,
    [edge_cut_exact(H1, H2, N) | make_span_verts(T1, T2, N+1)].

edge_cut(V1, V2, NumCuts) ->
    E = e3d_vec:sub(V2, V1),
    N = NumCuts-1,
    edge_cut(V1, V2, NumCuts, E, [], N).

edge_cut(V1, V2, _, _, Seq, 0) ->
    [V1] ++ lists:reverse(Seq) ++ [V2];
edge_cut(V1, V2, NumCuts, E, Seq, N) ->
    A = e3d_vec:mul(E, N/NumCuts),
    B = e3d_vec:add(V1, A),
    Seq2 = lists:append(Seq, [B]),
    edge_cut(V1, V2, NumCuts, E, Seq2, N-1).

edge_cut_exact(V1, V2, NumCuts) ->
    A = edge_cut(V1, V2, NumCuts),
    B = lists:reverse(edge_cut(V2, V1, NumCuts)),
    edge_cut_lists_average(A, B).

edge_cut_lists_average([], []) -> [];
edge_cut_lists_average(A, B) ->
    [HA|TA] = A,
    [HB|TB] = B,
    {X1,Y1,Z1} = HA,
    {X2,Y2,Z2} = HB,
    AveragedVertex = {(X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2},
    [AveragedVertex | edge_cut_lists_average(TA, TB)].

% =======================================
% === Recursive Subdivision Functions ===
% =======================================
subdivide_rawtriangles_by_depth(RawTriangles, Depth) ->
    Process_Triangle = fun(Triangle) ->
	[V1,V2,V3] = Triangle,
	subdivide_triangle_by_depth(V1, V2, V3, Depth) end,
    A = lists:map(Process_Triangle, RawTriangles),
    RawTriangles2 = append_ntimes(A, Depth),
    RawTriangles2.

subdivide_triangle_by_depth(V1, V2, V3, 1) ->
    [V1,V2,V3]; % [{a,b,c},{d,e,f},{g,h,i}];
subdivide_triangle_by_depth(V1, V2, V3, Depth) ->
    VA = e3d_vec:average(V1, V2),
    VB = e3d_vec:average(V2, V3),
    VC = e3d_vec:average(V3, V1),
    A  = subdivide_triangle_by_depth(V1, VA, VC, Depth-1),
    B  = subdivide_triangle_by_depth(V2, VB, VA, Depth-1),
    C  = subdivide_triangle_by_depth(V3, VC, VB, Depth-1),
    D  = subdivide_triangle_by_depth(VA, VB, VC, Depth-1),
    [A,B,C,D].

append_ntimes(DeepList, 1) -> DeepList;
append_ntimes(DeepList, N) ->
    List = lists:append(DeepList),
    append_ntimes(List, N-1).

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).
