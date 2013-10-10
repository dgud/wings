%%
%% HLines -- Hidden Line Renderer for Wings 0.98.17a and higher.
%%
%% Based on "HLines" hidden line elimination program from "Programming
%% Principles in Computer Graphics" and "Computer Graphics for Java
%% Programmers" by Leen Ammeraal, http://home.planet.nl/~ammeraal/ .
%% The source code listed in the books can be found here:
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/older/ppcgraph.zip
%% ftp://ftp.expa.fnt.hvu.nl/pub/ammeraal/English/grjava.zip
%%
%% Copyright (c) 2003-2011 Dmitry Efremov <defremov@aha.ru>
%%
%% BUGS:
%%  Near clipping will not work correctly if occurs in the viewport
%%  Duplicate and zero length line segments are generated in some cases
%%
%% $Id$
%%

-module(wpc_hlines).
-author('Dmitry Efremov <defremov@aha.ru>').

-export([init/0, menu/2, command/2]).
-import(lists, [
    foldl/3,
    foreach/2,
    keymember/3,
    keysearch/3,
    keyreplace/4
]).
-include("wings.hrl").
-include("e3d.hrl").

-define(EPS, 1.0e-6).
-define(BIG, 1.0e30).
-define(MAXDEPTH, 10).

-define(DEF_WIDTH, 288).
-define(DEF_HEIGHT, 216).
-define(DEF_EDGE_MODE, hard_edges).
-define(DEF_EDGE_WIDTH_OUTLINE, 1.0).
-define(DEF_EDGE_WIDTH_HARD, 1.0).
-define(DEF_EDGE_WIDTH_CREASE, 1.0).
-define(DEF_EDGE_WIDTH_MATERIAL, 1.0).
-define(DEF_EDGE_WIDTH_REGULAR, 1.0).
-define(DEF_EDGE_ONE_WIDTH_FOR_ALL, true).
-define(DEF_CREASE_ANGLE, 0).
-define(DEF_LINE_CAP, 0).
-define(DEF_SUBDIVISIONS, 0).
-define(DEF_OPTIMIZE, false).
-define(DEF_COLL_ANGLE, 0.5).
-define(DEF_COLL_DIST, 0.25).

init() ->
    true.

menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{"Cartoon edges (.eps)...", eps, [option]}].

command({file, {export, {eps, Arg}}}, St) ->
    export(Arg, export, St);
command({file, {export_selected, {eps, Arg}}}, St) ->
    export(Arg, export_selected, St);
command(_, _) -> next.

export(Arg, Op, _) when is_atom(Arg) ->
    wpa:dialog(Arg, ?__(1,"Cartoon edges Render Options"), dialog(),
        fun(Res) -> {file, {Op, {eps, Res}}} end);
export(Arg, Op, St) when is_list(Arg) ->
    set_pref(Arg),
    Camera_info = wpa:camera_info([aim, distance_to_aim,
        azimuth, elevation, tracking,
        fov, hither, yon]),
    Props = [{title, ?__(2,"Export")},
        {ext, ".eps"},
        {ext_desc, ?__(3,"Encapsulated Postscript (EPS) File")},
        {camera_info, Camera_info},
        {subdivisions, get_pref(subdivisions, ?DEF_SUBDIVISIONS)},
        {win_size, wings_wm:win_size()},
        {ortho_view, wings_wm:get_prop(orthogonal_view)}],

    case Op of
        export ->
            ?SLOW(wpa:export(Props, fun_export(Props), St))
            ;
        export_selected ->
            ?SLOW(wpa:export_selected(Props, fun_export(Props), St))
    end.

dialog() ->
    IntlPT=?__(1,"pt"),
    BB_width = get_pref(bb_width, ?DEF_WIDTH),
    BB_height = get_pref(bb_height, ?DEF_HEIGHT),
    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
    Edge_width_outline = get_pref(edge_width_outline,
        ?DEF_EDGE_WIDTH_OUTLINE),
    Edge_width_hard = get_pref(edge_width_hard, ?DEF_EDGE_WIDTH_HARD),
    Edge_width_crease = get_pref(edge_width_crease, ?DEF_EDGE_WIDTH_CREASE),
    Edge_width_material = get_pref(edge_width_material,
        ?DEF_EDGE_WIDTH_MATERIAL),
    Edge_width_regular = get_pref(edge_width_regular,
        ?DEF_EDGE_WIDTH_REGULAR),
    Edge_one_width_for_all = get_pref(edge_one_width_for_all,
        ?DEF_EDGE_ONE_WIDTH_FOR_ALL),
    Crease_angle = get_pref(crease_angle, ?DEF_CREASE_ANGLE),
    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),
    SubDiv = get_pref(subdivisions, ?DEF_SUBDIVISIONS),
    Optimize = get_pref(optimize, ?DEF_OPTIMIZE),
    Coll_angle = get_pref(coll_angle, ?DEF_COLL_ANGLE),
    Coll_dist = get_pref(coll_dist, ?DEF_COLL_DIST),

    [
        {hframe, [
            {label, ?__(2,"Width")},
            {text, BB_width, [{key, bb_width}]},
            {label, ?__(3,"Height")},
            {text, BB_height, [{key, bb_height}]},
            {label, IntlPT}
        ], [{title, ?__(4,"Bounding box")}]},

        {hframe,[{label,?__(5,"Sub-division Steps")},
            {text,SubDiv,[{key,subdivisions},{range,0,4}]}],
        [{title,?__(6,"Pre-rendering")}]},

        {hradio, [
            {?__(7,"All"), all_edges},
            {?__(8,"Hard"), hard_edges},
            {?__(9,"None"), no_edges}
        ], Edge_mode, [{key, edge_mode}, {title, ?__(10,"Show edges")}]},

        {hframe, [
            {slider,
                {text, Crease_angle, [
                    {key, crease_angle}, {range, {0, 180}}
                ]}}
        ], [{title, ?__(11,"Crease angle")}]},

        {hframe, [
            {vframe, [
                {label, ?__(12,"Outline")},
                {label, ?__(13,"Hard")},
                {label, ?__(14,"Crease")},
                {label, ?__(15,"Material")},
                {label, ?__(16,"Regular")}
            ]},
            {vframe, [
                {text, Edge_width_outline,
                    [{key, edge_width_outline}, {range, {0.0, ?BIG}}]},
                {text, Edge_width_hard,
                    [{key, edge_width_hard}, {range, {0.0, ?BIG}}]},
                {text, Edge_width_crease,
                    [{key, edge_width_crease}, {range, {0.0, ?BIG}}]},
                {text, Edge_width_material,
                    [{key, edge_width_material}, {range, {0.0, ?BIG}}]},
                {text, Edge_width_regular,
                    [{key, edge_width_regular}, {range, {0.0, ?BIG}}]}
            ]},
            {vframe, [
                {label, IntlPT},
                {label, IntlPT},
                {label, IntlPT},
                {label, IntlPT},
                {hframe, [
                    {label, IntlPT},
                    {?__(17,"All"), Edge_one_width_for_all,
                        [{key, edge_one_width_for_all}]}
                ]}
            ]}
        ], [{title, ?__(18,"Edge width")}]},

        {hradio, [
            {?__(19,"Butt"), 0},
            {?__(20,"Round"), 1},
            {?__(21,"Square"), 2}
        ], Line_cap, [{key, line_cap}, {title, ?__(22,"Line caps")}]},

        {vframe, [
            {?__(23,"Merge"), Optimize, [{key, optimize}]},
            {hframe, [
                {vframe, [
                    {label, ?__(24,"Angle")},
                    {label, ?__(25,"Distance")}
                ]},
                {vframe, [
                    {text, Coll_angle, [
                        {key, coll_angle}, {range, {0.0, 90.0}}
                    ]},
                    {text, Coll_dist, [
                        {key, coll_dist}, {range, {0.0, ?BIG}}
                    ]}
                ]},
                {vframe, [
                    {label, [176]},
                    {label, IntlPT}
                ]}
            ]}
        ], [{title, ?__(26,"Collinear lines (experimental)")}]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

fun_export(Props) ->
    fun (File_name, Scene) -> do_export(Props, File_name, Scene) end.

do_export(Props, File_name, #e3d_file{objs=Objs, mat=Mats}) ->
    Start_time = now(),

    [Aim, Distance, Azimuth, Elevation, {TrackX, TrackY}, Fov, Hither, Yonder] =
        proplists:get_value(camera_info, Props),

    Taim = e3d_mat:translate(Aim),
    Ry = e3d_mat:rotate(Azimuth, {0.0, 1.0, 0.0}),
    Rx = e3d_mat:rotate(Elevation, {1.0, 0.0, 0.0}),
    Tdist = e3d_mat:translate({TrackX, TrackY, -Distance}),
    Veye = e3d_mat:mul(Tdist, e3d_mat:mul(e3d_mat:mul(Rx, Ry), Taim)),

    Wbb = get_max(get_pref(bb_width, ?DEF_WIDTH), 1),
    Hbb = get_max(get_pref(bb_height, ?DEF_HEIGHT), 1),
    ARbb = Wbb / Hbb,

    {Wwin, Hwin} = proplists:get_value(win_size, Props),
    ARwin = Wwin / Hwin,

    {ARw, ARh} = if
        ARbb > ARwin -> {ARbb, 1.0};
        true -> {ARwin, ARwin / ARbb}
    end,

    Flen = 0.5 / math:tan(Fov * math:pi() / 180.0 / 2.0),

    VCs = foldl(fun(#e3d_object{obj=Obj}, VCs_acc) ->
        VCs_acc ++ Obj#e3d_mesh.vs
    end, [], Objs),

    EyeMesh = e3d_mesh:transform(#e3d_mesh{vs = VCs}, Veye),

    VCt = list_to_tuple(EyeMesh#e3d_mesh.vs),

    Is_ortho = proplists:get_value(ortho_view, Props),

    {Proj, Edge_norm, Front_face, Side_face, Wvp, Hvp} =
        if
            Is_ortho ->
                {fun ortho/2, fun ortho_en/1, fun ortho_ff/1, fun ortho_sf/1,
                Distance * ARw / Flen, Distance * ARh / Flen}
                ;
            true ->
                {fun persp/2, fun persp_en/1, fun persp_ff/1, fun persp_sf/1,
                ARw, ARh}
        end,

    View_port = {{-Wvp / 2.0, -Hvp / 2.0, -Yonder},
        {Wvp / 2.0, Hvp / 2.0, -Hither}},

    Frustum = if
        Is_ortho -> View_port;
        true -> {Wvp / 2.0, Hvp / 2.0, -Yonder, -Hither, -Flen}
    end,

    Mats_dict = materials(Mats),
    Crease_angle = get_pref(crease_angle, ?DEF_CREASE_ANGLE),
    Thresh_cos = math:cos((180.0 - Crease_angle) * math:pi() / 180.0),
    Edge_mode = get_pref(edge_mode, ?DEF_EDGE_MODE),
    Edge_type_fun =
        case get_pref(edge_one_width_for_all, ?DEF_EDGE_ONE_WIDTH_FOR_ALL) of
        false ->
            fun edge_type_group/5
            ;
        true ->
            fun edge_type_copy/5
    end,

wings_pb:start(""),
io:format("~n", []),

    Objs_total = length(Objs),

    {Edges_dict, Tria_qtree, _VI_incr, _Obj_count}
        = foldl(fun(#e3d_object{name = Name, obj = Mesh},
            {Edges_dict_acc0, Tria_qtree_acc0, VI_incr, Obj_count0}) ->
            Obj_count = Obj_count0 + 1,
Percent = Obj_count0 / Objs_total,
wings_pb:update(Percent * 0.28 + 0.01,
    ?__(1,"reading objects")++" " ++ integer_to_list(round(Percent * 100.0)) ++ "%"),
wings_pb:pause(),
io:format(?__(2,"Reading object ~B of")++" ~B \"~s\"...",
    [Obj_count, Objs_total, Name]),
            #e3d_mesh{vs = MVCs, fs = MFs, he = MHEs} = Mesh,
            Is_open = is_open(Mesh) orelse has_transp_faces(Mesh, Mats_dict),
            #e3d_mesh{fs = TMFs} = e3d_mesh:triangulate(Mesh),
            {Edge_dict, Tria_qtree_acc} = add_tri_mesh(incr(TMFs, VI_incr),
                VCt, Is_open, Mats_dict,
                Frustum, View_port, -Flen,
                Edge_norm, Front_face, Side_face, Proj,
                Tria_qtree_acc0),
            {HardEdge_set, Edge_set} = edge_sets(MHEs, MFs, Edge_mode, VI_incr),
            Edges_dict_acc = group_edges(Thresh_cos, HardEdge_set, Edge_set,
                Edge_type_fun, Edge_dict, Edges_dict_acc0),
io:format(" "++?__(3,"done")++"~n", []),
            {Edges_dict_acc, Tria_qtree_acc, VI_incr + length(MVCs),
                Obj_count}
        end, {dict_new(), qtree_new(bbox_2d(View_port)), 0, 0}, Objs),

wings_pb:update(0.3, ?__(4,"reading objects")++" 100%"),
wings_pb:pause(),
io:format(?__(5,"Exporting")),

    {Line_groups, Edges_total} =
        foldl(fun({Line_width, Edges}, {Dict_acc, Edge_count}) ->
            {dict_append_list(Line_width, Edges, Dict_acc),
                Edge_count + length(Edges)}
        end,
        {dict_new(), 0},
        [{Line_width, Edges} ||
            {Edge_type1, Line_width} <- [
                {outline, get_pref(edge_width_outline,
                    ?DEF_EDGE_WIDTH_OUTLINE)},
                {hard, get_pref(edge_width_hard,
                    ?DEF_EDGE_WIDTH_HARD)},
                {crease, get_pref(edge_width_crease,
                    ?DEF_EDGE_WIDTH_CREASE)},
                {material, get_pref(edge_width_material,
                    ?DEF_EDGE_WIDTH_MATERIAL)},
                {regular, get_pref(edge_width_regular,
                    ?DEF_EDGE_WIDTH_REGULAR)}
            ],
            {Edge_type2, Edges} <- Edges_dict,
            Edge_type1 == Edge_type2]),

    Prog_step = get_max(Edges_total div 20, 250),

    BB = {{0.0, 0.0}, {Wbb, Hbb}},
    Line_cap = get_pref(line_cap, ?DEF_LINE_CAP),

    {ok, F} = file:open(File_name, [write]),
    write_eps_header(F, BB, Line_cap),
    foldl(fun({Line_width, Edges}, {Group_count, Edge_count0}) ->
        {Ls0, Edge_count} = foldl(fun({EVI1, EVI2} = EVIt,
                {Ls_acc0, Edge_count_acc0}) ->
            EVCt = {coord(EVI1, VCt), coord(EVI2, VCt)},
            {Ls_acc, Edge_count_acc} =
                case project(Proj, -Flen, View_port, EVCt) of
                nil ->
                    {Ls_acc0, Edge_count_acc0}
                    ;
                LVCt ->
                    EP = ep(EVCt, Edge_norm),
                    Ts = get_objs(LVCt, Tria_qtree),
                    {Ls_acc0 ++ line_segment(EVIt, EVCt, EP, Ts, VCt),
                        Edge_count_acc0 + 1}
            end,
            if
                Edge_count_acc rem Prog_step == 0 ->
                    Percent = Edge_count_acc / Edges_total,
                    wings_pb:update(Percent * 0.69 + 0.3,
                        integer_to_list(round(Percent * 100.0)) ++ "%"),
                    wings_pb:pause(),
                    io:put_chars(".")
                    ;
                true ->
                    ok
            end,
            {Ls_acc, Edge_count_acc}
        end, {[], Edge_count0}, Edges),
        Offset = divide(bbox_size(bbox_2d(View_port)), 2.0),
        Ls1 = project(Proj, -Flen, View_port, Offset, Wbb / Wvp, Ls0),
        Ls = case get_pref(optimize, ?DEF_OPTIMIZE) of
            true ->
                Athr_deg = get_pref(coll_angle, ?DEF_COLL_ANGLE),
                Athr = math:sin(Athr_deg * math:pi() / 180.0),
                Dthr = get_pref(coll_dist, ?DEF_COLL_DIST),
                lstree_to_list(lists:foldl(fun(L, Ls_acc) ->
                     lstree_insert(L, Ls_acc, Athr, Dthr)
                end, nil, Ls1))
                ;
            false ->
                Ls1
        end,
        write_eps_line_group(F, Ls, Line_width, Group_count),
        {Group_count + 1, Edge_count}
    end, {0, 0}, Line_groups),

    ok = file:close(F),
wings_pb:update(1.0, ?__(6,"done")),
wings_pb:done(),
io:format(" "++?__(7,"done in ~.1f sec")++"~n", [timer:now_diff(now(), Start_time) / 1.0e6]).

edge_sets(Hard_edges, Faces, Edge_mode, Incr) when Edge_mode == all_edges ->
    {gb_sets:from_list(incr(Hard_edges, Incr)),
        gb_sets:from_list(edges(incr(Faces, Incr)))};
edge_sets(Hard_edges, _Faces, Edge_mode, Incr) when Edge_mode == hard_edges ->
    {gb_sets:from_list(incr(Hard_edges, Incr)), gb_sets:empty()};
edge_sets(_Hard_edges, _Faces, _Edge_mode, _Incr) ->
    {gb_sets:empty(), gb_sets:empty()}.


persp({X, Y, Z}, Zf) ->
    Rz = zero_div(Zf, Z),
    {X * Rz, Y * Rz};
persp({{X1, Y1, Z1}, {X2, Y2, Z2}}, Zf) ->
    Rz_1 = zero_div(Zf, Z1),
    Rz_2 = zero_div(Zf, Z2),
    {{X1 * Rz_1, Y1 * Rz_1}, {X2 * Rz_2, Y2 * Rz_2}}.

ortho({X, Y, _Z}, _) -> {X, Y}.

incr({A, B, C}, Incr) -> {A + Incr, B + Incr, C + Incr};
incr({A, B}, Incr) -> {A + Incr, B + Incr};
incr(#e3d_face{vs = VIs, mat = Mat}, Incr) -> 
    #e3d_face{vs = incr(VIs, Incr), mat = Mat};
incr([], _Incr)  -> [];
incr([A | T], Incr) -> [incr(A, Incr) | incr(T, Incr)];
incr(A, Incr) -> A + Incr.

normalize({P, Q}) when P > Q -> {Q, P};
normalize({P, Q})            -> {P, Q}.

%
%  [9, 2, 5, 3] -> [{2, 9}, {2, 5}, {3, 5}, {3, 9}]
%
npair(L) when is_list(L) -> npair(hd(L), L).

npair(H, [E]) -> [normalize({E, H})];
npair(H, [E | T]) -> [normalize({E, hd(T)}) | npair(H, T)].

coord(I, VCt) when is_integer(I) ->
    element(I + 1, VCt).

cull({{X1, Y1, Z1}, {X2, Y2, Z2}}, Frustum) ->
    outcode({X1, Y1, Z1}, Frustum)
        band outcode({X2, Y2, Z2}, Frustum) == 0;
cull({{X1, Y1, Z1}, {X2, Y2, Z2}, {X3, Y3, Z3}}, Frustum) ->
    outcode({X1, Y1, Z1}, Frustum)
        band outcode({X2, Y2, Z2}, Frustum)
        band outcode({X3, Y3, Z3}, Frustum) == 0.

flip({A, B, C}) -> {A, C, B};
flip({N, D}) -> {neg(N), -D}.

is_open(#e3d_mesh{fs = Fs}) ->
    EFs_dict = foldl(fun(#e3d_face{vs = FVIs}, ME_acc) ->
        foldl(fun(EVIt, FE_acc) ->
            case dict:find(EVIt, FE_acc) of
                {ok, Count} ->
                    dict:store(EVIt, Count + 1, FE_acc)
                    ;
                error ->
                    dict:store(EVIt, 1, FE_acc)
            end
        end, ME_acc, npair(FVIs))
    end, dict:new(), Fs),
    [] /= [C || {_, C} <- dict:to_list(EFs_dict), C < 2].

has_transp_faces(#e3d_mesh{fs = Fs}, Mats_dict) ->
    has_transp_faces(Fs, Mats_dict);
has_transp_faces([], _) -> false;
has_transp_faces([#e3d_face{mat = Mat} | T], Mats_dict) ->
    case hd(dict_fetch(hd(Mat), Mats_dict)) of
        true ->
            true
            ;
        false ->
            has_transp_faces(T, Mats_dict)
    end.

ep({EVC1, _EVC2} = EVCt, Edge_norm) ->
    EN = Edge_norm(EVCt),
    {EN, dot(EN, EVC1)}.

ept({TVC1, TVC2, TVC3}, Edge_norm) ->
    {ep({TVC1, TVC2}, Edge_norm),
    ep({TVC2, TVC3}, Edge_norm),
    ep({TVC3, TVC1}, Edge_norm)}.

triangle(TVIt, TVCt, TP, Edge_norm) ->
    {TVIt, TP, ept(TVCt, Edge_norm)}.

triangle(TVIt, TVCt, TP, Edge_norm, Is_FF) when Is_FF ->
    triangle(TVIt, TVCt, TP, Edge_norm);
triangle(TVIt, TVCt, TP, Edge_norm, _Is_FF) ->
    {flip(TVIt), flip(TP), ept(flip(TVCt), Edge_norm)}.

add_triangle(_Tria, nil, Qtree) -> Qtree;
add_triangle(Tria, BB, Qtree) -> qtree_insert({Tria, BB}, Qtree).

add_edges(FVIs, VCt, Frustum, FN, Is_FF, Is_SF, Mat, Edge_dict_acc0) ->
    foldl(fun({EVI1, EVI2} = EVIt, E_dict_acc0) ->
        EVCt = {coord(EVI1, VCt), coord(EVI2, VCt)},
        case cull(EVCt, Frustum) of
            true ->
                Face_type = face_type(Is_FF, Is_SF),
                dict:append(EVIt, {Face_type, FN, Mat}, E_dict_acc0)
                ;
            false ->
                E_dict_acc0
        end
    end, Edge_dict_acc0, npair(FVIs)).

face_type(_Is_FF, Is_SF) when Is_SF -> side;
face_type(Is_FF, _Is_SF) when Is_FF -> front;
face_type(_Is_FF, _Is_SF) -> back.

edges(#e3d_face{vs = FVIs}) -> npair(FVIs);
edges([]) -> [];
edges([F | T]) -> edges(F) ++ edges(T).

add_tri_mesh(TMFs, VCt, Is_open, Mats_dict,
    Frustum, View_port, Zf, Edge_norm, Front_face, Side_face, Proj,
    Tria_qtree_acc0) ->
        foldl(fun(#e3d_face{vs = TVIs, mat = Mat},
            {E_dict_acc0, T_qtree_acc0}) ->
        {TVI1, TVI2, TVI3} = TVIt = list_to_tuple(TVIs),
        {TVC1, TVC2, TVC3} = TVCt
            = {coord(TVI1, VCt), coord(TVI2, VCt), coord(TVI3, VCt)},
        case cull(TVCt, Frustum) of
            true ->
                TN = e3d_vec:normal(TVC1, TVC2, TVC3),
                TD = dot(TN, TVC1),
                TP = {TN, TD},
                Is_FF = Front_face(TP),
                case Is_open of
                    true ->
                        Is_transp = hd(dict_fetch(hd(Mat), Mats_dict)),
                        Is_SF = Side_face(TP),
                        if
                            Is_transp; Is_SF ->
                                T_qtree_acc = T_qtree_acc0
                                ;
                            true ->
                                T_qtree_acc = add_triangle(
                                    triangle(TVIt, TVCt, TP, Edge_norm, Is_FF),
                                    bbox(Proj, Zf, View_port, TVCt),
                                    T_qtree_acc0)
                        end,
                        {add_edges(TVIs, VCt, Frustum, TN, Is_FF, Is_SF, Mat,
                            E_dict_acc0), T_qtree_acc}
                        ;
                    false ->
                        case Is_FF of
                            true ->
                                {add_edges(TVIs, VCt, Frustum, TN,
                                    Is_FF, false, Mat,
                                    E_dict_acc0),
                                add_triangle(
                                    triangle(TVIt, TVCt, TP, Edge_norm),
                                    bbox(Proj, Zf, View_port, TVCt),
                                    T_qtree_acc0)}
                                ;
                            false ->
                                {E_dict_acc0, T_qtree_acc0}
                        end
                end
                ;
            false ->
                {E_dict_acc0, T_qtree_acc0}
        end
    end, {dict:new(), Tria_qtree_acc0}, TMFs).

materials(Mats) when is_list(Mats) ->
    materials(Mats, []).

materials([], Mats_dict) -> Mats_dict;
materials([{Name, Props} | T], Mats_dict) ->
    materials(T, dict_store(Name, is_transparent(Props), Mats_dict)).

%
% From src/wings_material.erl
%
is_transparent(MatProps) ->
    OpenGL = proplists:get_value(opengl, MatProps),
    foldl(fun(_, true) -> true;
        ({emission,_}, _) -> false;
        ({_,{_,_,_,1.0}}, _) -> false;
        ({_,{_,_,_,_}}, _) -> true;
        (_, _) -> false
    end, false, OpenGL).

is_visible(AFs) when length(AFs) < 2 -> true;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
    when FT1 == side, FT2 == side -> false;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
    when FT1 == side, FT2 == back -> false;
is_visible([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
    when FT1 == back, FT2 == side -> false;
is_visible(_AFs) -> true.

is_outline(AFs) when length(AFs) < 2 -> true;
is_outline([{FT1, _TN1, _Mat1}, {FT2, _TN2, _Mat2}])
    when (FT1 == front) xor (FT2 == front) -> true;
is_outline(_AFs) -> false.

is_crease(_EVIt, AFs, _ThreshCosA) when length(AFs) < 2 -> true;
is_crease(_EVIt, [{_FT1, TN1, _Mat1}, {_FT2, TN2, _Mat2}],
    ThreshCosA) -> is_le(dot(TN1, TN2), ThreshCosA);
is_crease(_, _, _) -> false.

is_material(AFs) when length(AFs) < 2 -> true;
is_material([{_FT1, _TN1, Mat1}, {_FT2, _TN2, Mat2}])
    when Mat1 /= Mat2 -> true;
is_material(_) -> false.

edge_type_group(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set) ->
    case is_visible(AFs) of
        true ->
            case is_outline(AFs) of
                true ->
                    outline
                    ;
                false ->
            case gb_sets:is_member(EVIt, HardEdge_set) of
                true ->
                    hard
                    ;
                false ->
            case is_crease(EVIt, AFs, ThreshCosA) of
                true ->
                    crease
                    ;
                false ->
            case is_material(AFs) of
                true ->
                    material
                    ;
                false ->
            case gb_sets:is_member(EVIt, Edge_set) of
                true ->
                    regular
                    ;
                false ->
                    none
            end
            end
            end
            end
            end
            ;
        false ->
            none
    end.

edge_type_copy(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set) ->
    case is_visible(AFs)
        andalso (is_outline(AFs)
            orelse is_material(AFs)
            orelse is_crease(EVIt, AFs, ThreshCosA)
            orelse gb_sets:is_member(EVIt, HardEdge_set)
            orelse gb_sets:is_member(EVIt, Edge_set))
    of
        true ->
            regular
            ;
        false ->
            none
    end.

group_edges(ThreshCosA, HardEdge_set, Edge_set, Edge_type_fun,
    Edge_dict, Edges_dict_acc0) ->
    dict:fold(fun(EVIt, AFs, Es_dict_acc0) ->
        case Edge_type_fun(EVIt, AFs, ThreshCosA, HardEdge_set, Edge_set) of
            none ->
                Es_dict_acc0
                ;
            Edge_type ->
                dict_append(Edge_type, EVIt, Es_dict_acc0)
        end
    end, Edges_dict_acc0, Edge_dict).


eps(A) when is_float(A) -> ?EPS + ?EPS * abs(A).
% is_eq(A, B) when is_float(A), is_float(B) -> abs(A - B) =< eps(B).
% is_ne(A, B) when is_float(A), is_float(B) -> abs(A - B) >  eps(B).
is_lt(A, B) when is_float(A), is_float(B) -> A <  B - eps(B).
is_le(A, B) when is_float(A), is_float(B) -> A =< B + eps(B).
is_gt(A, B) when is_float(A), is_float(B) -> A >  B + eps(B).
is_ge(A, B) when is_float(A), is_float(B) -> A >= B - eps(B).
% is_eq0(A) when is_float(A) -> abs(A) =< ?EPS.
% is_ne0(A) when is_float(A) -> abs(A) >  ?EPS.
% is_lt0(A) when is_float(A) -> A <  -?EPS.
% is_le0(A) when is_float(A) -> A =<  ?EPS.
% is_gt0(A) when is_float(A) -> A >   ?EPS.
% is_ge0(A) when is_float(A) -> A >= -?EPS.
% eps(A, Eps) when is_float(A), is_float(Eps) -> Eps + Eps * abs(A).
% is_eq(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     abs(A - B) =< eps(B, Eps).
% is_ne(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     abs(A - B) >  eps(B, Eps).
% is_lt(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A <  B - eps(B, Eps).
% is_le(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A =< B + eps(B, Eps).
% is_gt(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A >  B + eps(B, Eps).
% is_ge(A, B, Eps) when is_float(A), is_float(B), is_float(Eps) ->
%     A >= B - eps(B, Eps).
is_eq0(A, Eps) when is_float(A), is_float(Eps) -> abs(A) =< Eps.
% is_ne0(A, Eps) when is_float(A), is_float(Eps) -> abs(A) >  Eps.
is_lt0(A, Eps) when is_float(A), is_float(Eps) -> A <  -Eps.
% is_le0(A, Eps) when is_float(A), is_float(Eps) -> A =<  Eps.
% is_gt0(A, Eps) when is_float(A), is_float(Eps) -> A >   Eps.
% is_ge0(A, Eps) when is_float(A), is_float(Eps) -> A >= -Eps.

sign(A) when A > 0 -> 1;
sign(A) when A == 0 -> 0;
sign(A) when A < 0 -> -1.

get_min(A, B) when A =< B -> A;
get_min(A, B) when A > B -> B.

get_max(A, B) when A >= B -> A;
get_max(A, B) when A < B -> B.

add({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 + X2, Y1 + Y2, Z1 + Z2};
add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

sub({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 - X2, Y1 - Y2, Z1 - Z2};
sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

neg({X, Y, Z}) -> {-X, -Y, -Z};
neg({X, Y}) -> {-X, -Y}.

mul({X, Y, Z}, S) -> {X * S, Y * S, Z * S};
mul({X, Y}, S) -> {X * S, Y * S}.

divide({X, Y}, S) -> {X / S, Y / S}.

zero_div(A, B) when is_float(A), is_float(B) ->
    case catch A / B of
        R when is_float(R) -> R;
        _ -> sign(A) * ?BIG
    end.

cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {Y1 * Z2 - Y2 * Z1, X2 * Z1 - X1 * Z2, X1 * Y2 - X2 * Y1};
cross({X1, Y1}, {X2, Y2}) -> X1 * Y2 - X2 * Y1.

dot({X1, Y1, Z1}, {X2, Y2, Z2}) -> X1 * X2 + Y1 * Y2 + Z1 * Z2;
dot({X1, Y1}, {X2, Y2}) -> X1 * X2 + Y1 * Y2.

%normal(V1, V2, V3) -> cross(sub(V2, V1), sub(V3, V1)).

persp_en({V1, V2}) -> cross(V1, V2).

ortho_en({{X1, Y1, _}, {X2, Y2, _}}) -> {Y2 - Y1, X1 - X2, 0.0}.

persp_ff({_, D}) -> is_lt0(D, 0.001).

ortho_ff({{_, _, Z}, _}) -> is_gt(Z, 0.001).

persp_sf({_, D}) -> is_eq0(D, 0.001).

ortho_sf({{_, _, Z}, _}) -> is_eq0(Z, 0.001).

%
% Cohen-Sutherland
%
outcode({X, Y, Z}, {{Xmin, Ymin, Zmin}, {Xmax, Ymax, Zmax}}) ->
    C0 = if X < Xmin -> 1; true -> 0 end,
    C1 = if X > Xmax -> 2; true -> 0 end,
    C2 = if Y < Ymin -> 4; true -> 0 end,
    C3 = if Y > Ymax -> 8; true -> 0 end,
    C4 = if Z < Zmin -> 16; true -> 0 end,
    C5 = if Z > Zmax -> 32; true -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5;
outcode({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    C0 = if X < Xmin -> 1; true -> 0 end,
    C1 = if X > Xmax -> 2; true -> 0 end,
    C2 = if Y < Ymin -> 4; true -> 0 end,
    C3 = if Y > Ymax -> 8; true -> 0 end,
    C0 bor C1 bor C2 bor C3;
outcode({X, Y, Z}, {HSx, HSy, Zmin, Zmax, Zf}) ->
    R =  Z / Zf,
    Rx = HSx * R,
    Ry = HSy * R,
    {C0, C1, C2, C3} = if
        Z > 0.0 ->
            {if X < -Rx -> 0; true -> 1 end,
            if X > Rx -> 0; true -> 2 end,
            if Y < -Ry -> 0; true -> 4 end,
            if Y > Ry -> 0; true -> 8 end}
            ;
        true ->
            {if X < -Rx -> 1; true -> 0 end,
            if X > Rx -> 2; true -> 0 end,
            if Y < -Ry -> 4; true -> 0 end,
            if Y > Ry -> 8; true -> 0 end}
    end,
    C4 = if Z < Zmin -> 16; true -> 0 end,
    C5 = if Z > Zmax -> 32; true -> 0 end,
    C0 bor C1 bor C2 bor C3 bor C4 bor C5.

to_boundary({X, Y, Z}, {DX, DY, DZ}, Zlim) when is_float(Zlim) ->
    {X + DX * (Zlim - Z) / DZ, Y + DY * (Zlim - Z) / DZ, Zlim};
to_boundary({X, Y}, {DX, DY}, {{Xmin, _}, _}) when X < Xmin ->
    {Xmin, Y + DY * (Xmin - X) / DX};
to_boundary({X, Y}, {DX, DY}, {_, {Xmax, _}}) when X > Xmax ->
    {Xmax, Y + DY * (Xmax - X) / DX};
to_boundary({X, Y}, {DX, DY}, {{_, Ymin}, _}) when Y < Ymin ->
    {X + DX * (Ymin - Y) / DY, Ymin};
to_boundary({X, Y}, {DX, DY}, {_, {_, Ymax}}) when Y > Ymax ->
    {X + DX * (Ymax - Y) / DY, Ymax}.

clip({LC1, LC2}, Box) ->
    C1 = outcode(LC1, Box),
    C2 = outcode(LC2, Box),
    if
        C1 band C2 /= 0 ->
            nil
            ;
        C1 bor C2 /= 0 ->
            D = sub(LC2, LC1),
            case C1 /= 0 of
                true  ->
                    clip({to_boundary(LC1, D, Box), LC2}, Box)
                    ;
                false ->
                    clip({LC1, to_boundary(LC2, D, Box)}, Box)
            end
            ;
        true ->
            {LC1, LC2}
    end.

clip_z({LC1, LC2}, View_port) when size(View_port) == 2 ->
    {{_, _, Z1}, {_, _, Z2}} = {LC1, LC2},
    {{_, _, Zvp_min}, {_, _, Zvp_max}} = View_port,
    if
        Z1 >= Zvp_min, Z1 =< Zvp_max, Z2 >= Zvp_min, Z2 =< Zvp_max ->
            {LC1, LC2}
            ;
        ((Z1 < Zvp_min) and (Z2 < Zvp_min))
            or ((Z1 > Zvp_max) and (Z2 > Zvp_max)) ->
            nil
            ;
        true ->
            {if
                Z1 < Zvp_min ->
                    to_boundary(LC1, sub(LC2, LC1), Zvp_min)
                    ;
                Z1 > Zvp_max ->
                    to_boundary(LC1, sub(LC2, LC1), Zvp_max)
                    ;
                true ->
                    LC1
            end,
            if
                Z2 < Zvp_min ->
                    to_boundary(LC2, sub(LC2, LC1), Zvp_min)
                    ;
                Z2 > Zvp_max ->
                    to_boundary(LC2, sub(LC2, LC1), Zvp_max)
                    ;
                true ->
                    LC2
            end}
    end.



nearer({{_, _, LZ1}, {_, _, LZ2}},
    {{_, _, TZ1}, {_, _, TZ2}, {_, _, TZ3}}) ->
    is_gt(get_min(LZ1, LZ2), get_max(TZ1, get_max(TZ2, TZ3))).

%edge_of_tria(LIt, TIt) when LIt == nil -> false;
edge_of_tria(LIt, TIt) ->
    {TI1, TI2, TI3} = TIt,
    L = normalize(LIt),
           (L == normalize({TI1, TI2}))
    orelse (L == normalize({TI2, TI3}))
    orelse (L == normalize({TI3, TI1})).

outside_tria({D12, D23, D31}, {{_, ED12}, {_, ED23}, {_, ED31}}) ->
    is_gt(D12, ED12) orelse is_gt(D23, ED23) orelse is_gt(D31, ED31).

outside_edge(L1D, L2D, {_, ED}) ->
    (is_ge(L1D, ED) andalso is_gt(L2D, ED))
        orelse (is_gt(L1D, ED) andalso is_ge(L2D, ED)).

outside_tria({L1D12, L1D23, L1D31}, {L2D12, L2D23, L2D31},
    {EP12, EP23, EP31}) ->
    outside_edge(L1D12, L2D12, EP12)
        orelse outside_edge(L1D23, L2D23, EP23)
        orelse outside_edge(L1D31, L2D31, EP31).

orient(C, {N, D}) ->
    Dcn = dot(C, N),
    case is_lt(Dcn, D) of
        true -> -1;
        false ->
            case is_gt(Dcn, D) of
                true -> 1;
                false -> 0
            end
    end.

outside_seg({TVC1, TVC2, TVC3}, LP) ->
    P = orient(TVC1, LP) +  orient(TVC2, LP) + orient(TVC3, LP),
   (P > 1) orelse (P < -1).

line_segment(_, nil, _, _, _) -> [];
line_segment(_, LCt, _, [], _) -> [LCt];
line_segment(LIt, LCt, LP, [Triangle | T], VCt) ->
    {LC1, LC2} = LCt,
    {TIt, TP, EPt} = Triangle,
    {TI1, TI2, TI3} = TIt,
    {TN, TD} = TP,
    {EP12, EP23, EP31} = EPt,
    TCt = {coord(TI1, VCt), coord(TI2, VCt), coord(TI3, VCt)},
    {TVC1, TVC2, TVC3} = TCt,
    case
        nearer(LCt, TCt) orelse edge_of_tria(LIt, TIt)
    of
        true  ->
            line_segment(LIt, LCt, LP, T, VCt)
            ;
        false ->
            L1D = dot(LC1, TN),
            L2D = dot(LC2, TN),
            {{EN12, _}, {EN23, _}, {EN31, _}} = EPt,
            D1t = {dot(LC1, EN12), dot(LC1, EN23), dot(LC1, EN31)},
            D2t = {dot(LC2, EN12), dot(LC2, EN23), dot(LC2, EN31)},
            case
                (is_ge(L1D, TD) andalso is_ge(L2D, TD))
                    orelse outside_tria(D1t, D2t, EPt)
                    orelse outside_seg(TCt, LP)
            of
                true  ->
                    line_segment(LIt, LCt, LP, T, VCt)
                    ;
                false ->
                    L1_behind = is_le(L1D, TD),
                    L2_behind = is_le(L2D, TD),
                    case L1_behind orelse L2_behind of
                        true ->
                            Edges = [{TVC1, TVC2, EP12},
                                {TVC2, TVC3, EP23}, {TVC3, TVC1, EP31}],
                            L1_out = outside_tria(D1t, EPt),
                            L2_out = outside_tria(D2t, EPt),
                            case
                                L1_behind andalso L2_behind
                                    andalso not(L1_out orelse L2_out)
                            of
                                true ->
                                    []
                                    ;
                                false ->
                                    {Smin, Smax} = find_section(L1_out /= L2_out,
                                        LCt, LP, Edges),
                                    case
                                        divide_segment(LCt, TP, L1_behind,
                                            L2_behind, L1_out, L2_out, Smin, Smax)
                                    of
                                        {LA, LA} ->
                                            line_segment(LIt, LA, LP, T, VCt)
                                            ;
                                        {LA, LB} ->
                                            line_segment(LIt, LA, LP, T, VCt) ++
                                                line_segment(LIt, LB, LP, T, VCt)
                                    end
                            end
                            ;
                        false ->
                            line_segment(LIt, LCt, LP, T, VCt)
                    end
            end
    end.

divide_segment(LCt, TP, L1_behind, L2_behind, L1_out, L2_out,
    Smin, Smax) when L1_out xor L2_out ->
    {LC1, LC2} = LCt,
    {TN, TD} = TP,
    Lmin = param(LCt, Smin),
    Lmax = param(LCt, Smax),
    Lmin_behind = is_le(dot(Lmin, TN), TD),
    Lmax_behind = is_le(dot(Lmax, TN), TD),
    case Lmin_behind orelse Lmax_behind of
        true ->
            {case L1_out andalso (Smin > 0.0 + 0.001) of
                true ->
                    {LC1, Lmin}
                    ;
                false ->
                    case L1_behind of
                        true ->
                            nil
                            ;
                        false ->
                            {LC1, param(LCt, section(LCt, TP))}
                    end
            end,
            case L2_out andalso (Smax < 1.0 - 0.001) of
                true ->
                    {Lmax, LC2}
                    ;
                false ->
                    case L2_behind of
                        true ->
                            nil
                            ;
                        false ->
                            {param(LCt, section(LCt, TP)), LC2}
                    end
            end}
            ;
        false ->
            {case L1_out andalso (Smin > 0.0 + 0.001) of
                true ->
                    case L2_behind of
                        true ->
                            {LC1, param(LCt, section(LCt, TP))}
                            ;
                        false ->
                            {LC1, LC2}
                    end
                    ;
                false ->
                    nil
            end,
            case L2_out andalso (Smax < 1.0 - 0.001) of
                true ->
                    case L1_behind of
                        true ->
                            {param(LCt, section(LCt, TP)), LC2}
                            ;
                        false ->
                            {LC1, LC2}
                    end
                    ;
                false ->
                    nil
            end}
    end;
divide_segment(LCt, TP, _, _, L1_out, L2_out,
    Smin, Smax) when L1_out and L2_out ->
    {LC1, LC2} = LCt,
    {TN, TD} = TP,
    Lmin = param(LCt, Smin),
    Lmax = param(LCt, Smax),
    Lmin_behind = is_le(dot(Lmin, TN), TD),
    Lmax_behind = is_le(dot(Lmax, TN), TD),
    case Lmin_behind orelse Lmax_behind of
        true ->
            {case Lmin_behind of
                true ->
                    case L1_out andalso (Smin > 0.0 + 0.001) of
                        true ->
                            {LC1, Lmin}
                            ;
                        false ->
                            nil
                    end
                    ;
                false ->
                    {LC1, param(LCt, section(LCt, TP))}
            end,
            case Lmax_behind of
                true ->
                    case L2_out andalso (Smax < 1.0 - 0.001) of
                        true ->
                            {Lmax, LC2}
                            ;
                        false ->
                            nil
                    end
                    ;
                false ->
                    {param(LCt, section(LCt, TP)), LC2}
            end}
            ;
        false ->
            {LCt, LCt}
    end;
divide_segment(LCt, TP, L1_behind, L2_behind, _, _, _, _) ->
    {LC1, LC2} = LCt,
    {case L1_behind of
        true ->
            nil
            ;
        false ->
            {LC1, param(LCt, section(LCt, TP))}
    end,
    case L2_behind of
        true ->
            nil
            ;
        false ->
            {param(LCt, section(LCt, TP)), LC2}
    end}.

find_section(Find_one, LCt, LP, Edges) ->
    find_section(1.0, 0.0, Find_one, LCt, LP, Edges).

find_section(Smin, Smax, _, _, _, []) -> {Smin, Smax};
find_section(Smin, Smax, Find_one, LCt, LP, [{EC1, EC2, {EN, ED}} | T]) ->
    {LC1, LC2} = LCt,
    {LN, LD} = LP,
    R =  zero_div((LD - dot(LN, EC1)), dot(LN, sub(EC2, EC1))),
    if
        (R > 0.0 - 0.0001) and (R < 1.0 + 0.0001) ->
            S = zero_div((ED - dot(EN, LC1)), dot(EN, sub(LC2, LC1))),
            if
                (S > 0.0 - 0.0001) and (S < 1.0 + 0.0001) ->
                    case Find_one of
                        true ->
                            if
                                (S > 0.0 + 0.001) and (S < 1.0 - 0.001) ->
                                    {S, S}
                                    ;
                                true ->
                                    find_section(get_min(S, Smin), get_max(S, Smax),
                                        Find_one, LCt, LP, T)
                            end
                            ;
                        false ->
                            find_section(get_min(S, Smin), get_max(S, Smax),
                                Find_one, LCt, LP, T)
                    end
                    ;
                true ->
                    find_section(Smin, Smax, Find_one, LCt, LP, T)
            end
            ;
        true ->
            find_section(Smin, Smax, Find_one, LCt, LP, T)
    end.

section({LC1, LC2}, {TN, TD}) ->
    case catch (TD - dot(TN, LC1)) / dot(TN, sub(LC2, LC1)) of
        R when is_float(R)->
            R
            ;
        _ ->
            ?BIG
    end.

param({LC1, LC2}, U) -> add(LC1, mul(sub(LC2, LC1), U)).




%%
%% See http://www.tulrich.com/geekstuff/partitioning.html
%%

qtree_new(Box) when size(Box) == 2 -> qtree_new(cons(Box));
qtree_new(Cons) -> {[], Cons, nil, nil, nil, nil}.

qtree_insert({_Obj, BB} = QE, Q) -> qtree_insert(QE, mid(BB), Q, 0).

qtree_insert(false, _QE, _QE_mid, _Cons, Q, _Depth) -> Q;
qtree_insert(true, QE, QE_mid, Cons, nil, Depth) ->
    qtree_insert(QE, QE_mid, qtree_new(Cons), Depth + 1);
qtree_insert(true, QE, QE_mid, _Cons, Q, Depth) ->
    qtree_insert(QE, QE_mid, Q, Depth + 1).

qtree_insert(QE, QE_mid, {QEs, Cons, Q1, Q2, Q3, Q4}, Depth) ->
    C1 = cons(if Q1 == nil -> quad1(Cons); true -> Q1 end),
    C2 = cons(if Q2 == nil -> quad2(Cons); true -> Q2 end),
    C3 = cons(if Q3 == nil -> quad3(Cons); true -> Q3 end),
    C4 = cons(if Q4 == nil -> quad4(Cons); true -> Q4 end),
    F1 = fits(QE, QE_mid, C1),
    F2 = fits(QE, QE_mid, C2),
    F3 = fits(QE, QE_mid, C3),
    F4 = fits(QE, QE_mid, C4),
    if
        Depth > ?MAXDEPTH - 1; not (F1 or F2 or F3 or F4) ->
            {[QE | QEs], Cons, Q1, Q2, Q3, Q4}
            ;
        true ->
            N1 = qtree_insert(F1, QE, QE_mid, C1, Q1, Depth),
            N2 = qtree_insert(F2, QE, QE_mid, C2, Q2, Depth),
            N3 = qtree_insert(F3, QE, QE_mid, C3, Q3, Depth),
            N4 = qtree_insert(F4, QE, QE_mid, C4, Q4, Depth),
            {QEs, Cons, N1, N2, N3, N4}
    end.

cons({{Xmin, Ymin}, {Xmax, Ymax}}) ->
    Dx = (Xmax - Xmin) / 2.0,
    Dy = (Ymax - Ymin) / 2.0,
    {{{Xmin, Ymin}, {Xmax, Ymax}},
        {{Xmin - Dx, Ymin - Dy}, {Xmax + Dx, Ymax + Dy}},
        {Xmin + Dx, Ymin + Dy}};
cons({_QEs, Cons, _Q1, _Q2, _Q3, _Q4}) -> Cons.

quad1({{_, {Xmax, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymid}, {Xmax, Ymax}}.
quad2({{{Xmin, _}, {_, Ymax}}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymid}, {Xmid, Ymax}}.
quad3({{{Xmin, Ymin}, _}, _, {Xmid, Ymid}}) ->
    {{Xmin, Ymin}, {Xmid, Ymid}}.
quad4({{{_, Ymin}, {Xmax, _}}, _, {Xmid, Ymid}}) ->
    {{Xmid, Ymin}, {Xmax, Ymid}}.

inside({X, Y}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    (X >= Xmin) andalso (Y >= Ymin) andalso (X =< Xmax) andalso (Y =< Ymax).

fits({_, {Obj_min, Obj_max}}, QE_mid, {Cons_box, Cons_lim, _}) ->
    inside(Obj_min, Cons_lim) andalso inside(Obj_max, Cons_lim)
        andalso inside(QE_mid, Cons_box).

get_quad_objs(_LS, []) -> [];
get_quad_objs(LS, [{Obj, BB} | T]) ->
    case overlays(LS, BB) of
        true ->
            [Obj | get_quad_objs(LS, T)]
            ;
        false ->
            get_quad_objs(LS, T)
    end.

get_objs(_,  nil) -> [];
get_objs(LS, {QEs, {_BB, Lim, _Mid}, Q1, Q2, Q3, Q4}) ->
    case overlays(LS, Lim) of
        true ->
            get_quad_objs(LS, QEs)
                ++ get_objs(LS, Q1)
                ++ get_objs(LS, Q2)
                ++ get_objs(LS, Q3)
                ++ get_objs(LS, Q4)
            ;
        false ->
            []
    end.

intersects({{X1, Y1}, {X2, Y2}}, {{Xmin, Ymin}, {Xmax, Ymax}}) ->
    Dx = X2 - X1,
    Tx1 = zero_div(Xmin - X1, Dx),
    Tx2 = zero_div(Xmax - X1, Dx),
    Tx_near = get_max(-?BIG, get_min(Tx1, Tx2)),
    Tx_far = get_min(?BIG, get_max(Tx1, Tx2)),
    Dy = Y2 - Y1,
    Ty1 = zero_div(Ymin - Y1, Dy),
    Ty2 = zero_div(Ymax - Y1, Dy),
    Ty_near = get_max(Tx_near, get_min(Ty1, Ty2)),
    Ty_far = get_min(Tx_far, get_max(Ty1, Ty2)),
    (Ty_near =< Ty_far) andalso (Ty_near >= 0.0) andalso (Ty_far =< 1.0).

overlays({LC1, LC2}, Box) ->
    inside(LC1, Box) orelse inside(LC2, Box)
        orelse intersects({LC1, LC2}, Box).

mid({{Xmin, Ymin}, {Xmax, Ymax}}) -> {(Xmin + Xmax) / 2.0, (Ymin + Ymax) / 2.0}.

bbox_size({{Xmin, Ymin}, {Xmax, Ymax}}) -> {Xmax - Xmin, Ymax - Ymin}.

bbox({{X1, Y1}, {X2, Y2}}) ->
    Xmin = get_min(X1, X2),
    Ymin = get_min(Y1, Y2),
    Xmax = get_max(X1, X2),
    Ymax = get_max(Y1, Y2),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox({{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}},
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}}) ->
    Xmin = get_min(Xmin_1, Xmin_2),
    Ymin = get_min(Ymin_1, Ymin_2),
    Xmax = get_max(Xmax_1, Xmax_2),
    Ymax = get_max(Ymax_1, Ymax_2),
    {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox_2d({{Xmin, Ymin, _}, {Xmax, Ymax, _}}) -> {{Xmin, Ymin}, {Xmax, Ymax}}.

bbox(Proj, Zf, View_port, {TVC1, TVC2, TVC3}) ->
    TS1 = noclip_proj(Proj, Zf, View_port, {TVC1, TVC2}),
    TS2 = noclip_proj(Proj, Zf, View_port, {TVC2, TVC3}),
    TS3 = noclip_proj(Proj, Zf, View_port, {TVC3, TVC1}),
    B1 = case TS1 of nil -> nil ; Ct_1 -> bbox(Ct_1) end,
    B2 = case TS2 of
        nil ->
            B1
            ;
        Ct_2 ->
            case B1 of nil -> bbox(Ct_2) ; _ -> bbox(B1, bbox(Ct_2)) end
    end,
    B3 = case TS3 of
        nil ->
            B2
            ;
        Ct_3 ->
            case B2 of nil -> bbox(Ct_3) ; _ -> bbox(B2, bbox(Ct_3)) end
    end,
    intersection(B3, bbox_2d(View_port)).

intersection({{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}},
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}}) ->
    Xmin = get_max(Xmin_1, Xmin_2),
    Ymin = get_max(Ymin_1, Ymin_2),
    Xmax = get_min(Xmax_1, Xmax_2),
    Ymax = get_min(Ymax_1, Ymax_2),
    if
        (Xmin < Xmax) and (Ymin < Ymax) ->
            {{Xmin, Ymin}, {Xmax, Ymax}}
            ;
        true ->
            nil
    end.


noclip_proj(Proj, Zf, View_port, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case clip_z(LVCt, View_port) of
        {LVC1, LVC2} ->
           {Proj(LVC1, Zf), Proj(LVC2, Zf)}
            ;
        nil ->
            nil
    end.

project(Proj, Zf, View_port, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case clip_z(LVCt, View_port) of
        {LVC1, LVC2} ->
            clip({Proj(LVC1, Zf), Proj(LVC2, Zf)}, bbox_2d(View_port))
            ;
        nil ->
            nil
    end.

project(Proj, Zf, View_port, Offset, Scale, LVCt)
    when size(View_port) == 2, size(LVCt) == 2 ->
    case project(Proj, Zf, View_port, LVCt) of
        {C1, C2} ->
            {mul(add(C1, Offset), Scale), mul(add(C2, Offset), Scale)}
            ;
        nil ->
            nil
    end;
project(_, _, _, _, _, []) -> [];
project(Proj, Zf, View_port, Offset, Scale, [LVCt | T])
    when size(View_port) == 2 ->
    case project(Proj, Zf, View_port, Offset, Scale, LVCt) of
        {LVC1, LVC2} ->
            [{LVC1, LVC2} | project(Proj, Zf, View_port, Offset, Scale, T)]
            ;
        nil ->
            project(Proj, Zf, View_port, Offset, Scale, T)
    end.

merge(LS1, _BB1, [], _Dthr) -> [LS1];
merge(LS1, {{Xmin_1, Ymin_1}, {Xmax_1, Ymax_1}} = BB1, [LS2 | T], Dthr) ->
    {{Xmin_2, Ymin_2}, {Xmax_2, Ymax_2}} = BB2 = bbox(LS2),
    {{Xmin, Ymin}, {Xmax, Ymax}} = BB = bbox(BB1, BB2),
    case
        (get_max(Xmin_1, Xmin_2) =< get_min(Xmax_1, Xmax_2) + Dthr)
            andalso (get_max(Ymin_1, Ymin_2) =< get_min(Ymax_1, Ymax_2) + Dthr)
    of
        true ->
            {LS11, _LS12} = LS1,
            LS = if
                LS11 == {Xmin_1, Ymin_1}; LS11 == {Xmax_1, Ymax_1} ->
                    {{Xmin, Ymin}, {Xmax, Ymax}}
                    ;
                true ->
                    {{Xmin, Ymax}, {Xmax, Ymin}}
            end,
            merge(LS, BB, T, Dthr)
            ;
        false ->
            [LS2 | merge(LS1, BB1, T, Dthr)]
    end.

merge(LS, [], _Dthr) -> [LS];
merge(LS, LSs, Dthr) -> merge(LS, bbox(LS), LSs, Dthr).

lstree_insert(LS, LS_tree, Athr, Dthr) ->
    lstree_insert(unit_2d(LS, Dthr), LS, LS_tree, Athr, Dthr).

lstree_insert(nil, _LS0, LS_tree, _Athr, _Dthr) -> LS_tree;
lstree_insert(U0, LS0, nil, _Athr, Dthr)  ->
    {lstree_insert1(U0, mid(LS0), LS0, nil, Dthr), nil, nil};
lstree_insert(U0, LS0, {{U, _, _, _, _} = LS_tree, Small, Big}, Athr, Dthr) ->
    Sin = cross(U0, U),
    Cos = dot(U0, U),
    if
        ((Sin < -Athr) and (Cos > Athr))
            or ((Sin > Athr) and (Cos < -Athr)) ->
            {LS_tree, lstree_insert(U0, LS0, Small, Athr, Dthr), Big}
            ;
        ((Sin > Athr) and (Cos >= -Athr))
            or ((Sin < -Athr) and (Cos =< Athr))->
            {LS_tree, Small, lstree_insert(U0, LS0, Big, Athr, Dthr)}
            ;
        true ->
            {lstree_insert1(U0, mid(LS0), LS0, LS_tree, Dthr), Small, Big}
    end.

lstree_insert1(U0, Mid_LS0, LS0, nil, _Dthr)  ->
    {U0, Mid_LS0, [LS0], nil, nil};
lstree_insert1(U0, Mid_LS0, LS0, {U, Mid_LS, LSs, Small, Big}, Dthr) ->
    V = sub(Mid_LS0, Mid_LS),
    D1 = cross(U, V),
    D2 = cross(U0, neg(V)),
    D0 = get_min(abs(D1), abs(D2)),
    D = if D1 >= 0.0 -> D0 ; true -> -D0 end,
    if
        D < -Dthr ->
            {U, Mid_LS, LSs,
                lstree_insert1(U0, Mid_LS0, LS0, Small, Dthr), Big}
            ;
        D > Dthr ->
            {U, Mid_LS, LSs,
                Small, lstree_insert1(U0, Mid_LS0, LS0, Big, Dthr)}
            ;
        true ->
        {U, Mid_LS, merge(LS0, LSs, Dthr), Small, Big}
    end.

lstree_to_list(nil, List) -> List;
lstree_to_list({LS_tree, Small, Big}, List) ->
    LSs = lstree_to_list(LS_tree),
    lstree_to_list(Small, LSs ++ lstree_to_list(Big, List));
lstree_to_list({_U, _Mid_LS, LSs, Small, Big}, List) ->
    lstree_to_list(Small, LSs ++ lstree_to_list(Big, List)).

lstree_to_list(LS_tree) -> lstree_to_list(LS_tree, []).

unit_2d({{X1, Y1}, {X2, Y2}}, Dthr) ->
    UX0 = X2 - X1,
    UY0 = Y2 - Y1,
    if
        (abs(UX0) < Dthr) and (abs(UY0) < Dthr) ->
            nil
            ;
        true ->
            D = math:sqrt(UX0 * UX0 + UY0 * UY0),
            {UX0 / D, UY0 / D}
    end.

write_eps_header(F, {{Xbb_min, Ybb_min}, {Xbb_max, Ybb_max}}, Line_cap) ->
    io:put_chars(F, "%!PS-Adobe-2.0 EPSF-2.0\n"),
    io:fwrite(F, "%%BoundingBox: ~w ~w ~w ~w~n",
        [round(Xbb_min), round(Ybb_min), round(Xbb_max), round(Ybb_max)]),
    io:put_chars(F, "/s {newpath moveto lineto stroke} bind def\n"),
    case Line_cap of
        0 -> ok;
        _ -> io:fwrite(F, "~w setlinecap~n", [Line_cap])
    end.

write_eps_line_group(_F, [], _Line_width, _Group_count) -> ok;
write_eps_line_group(F, Ls, Line_width, Group_count)
    when Group_count > 0; Line_width /= 1.0 ->
    io:fwrite(F, "~.1f setlinewidth~n", [get_max(Line_width, 0.0)]),
    write_eps_line_group(F, Ls);
write_eps_line_group(F, Ls, _Line_width, _Group_count) ->
    write_eps_line_group(F, Ls).

write_eps_line_group(F, Ls) ->
    foreach(fun({{X1, Y1}, {X2, Y2}}) ->
        io:fwrite(F, "~.1f ~.1f ~.1f ~.1f s~n", [X2, Y2, X1, Y1])
    end, Ls).


dict_new() -> [].

dict_append_list(Key0, Values0, KVs) ->
    case keysearch(Key0, 1, KVs) of
        {value, {_Key, Values}} ->
            keyreplace(Key0, 1, KVs, {Key0, Values ++ Values0})
            ;
        false ->
            [{Key0, Values0} | KVs]
    end.

dict_append(Key0, Value, KVs) ->
    case keysearch(Key0, 1, KVs) of
        {value, {_Key, Values}} ->
            keyreplace(Key0, 1, KVs, {Key0, [Value | Values]})
            ;
        false ->
            [{Key0, [Value]} | KVs]
    end.

dict_store(Key0, Value, KVs) ->
    case keymember(Key0, 1, KVs) of
        true ->
            keyreplace(Key0, 1, KVs, {Key0, [Value]})
            ;
        false ->
            [{Key0, [Value]} | KVs]
    end.

dict_fetch(Key, KVs) ->
    {value, {_Key, Values}} = keysearch(Key, 1, KVs),
    Values.
