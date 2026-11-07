%%
%%  wings_extrude_edge.erl --
%%
%%     This module contains the Extrude (edge), Bevel (face/edge) and
%%     Bump commands. (All based on edge extrusion.)
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_extrude_edge).
-export([bump/1,bevel/1,bevel_faces/1,extrude/2,crease/1,extrude_edges/3,
         uv_temp/4]).

-include("wings.hrl").
-import(lists, [foldl/3,reverse/1]).

-define(DEFAULT_EXTRUDE_DIST, 0.2).
-define(BEVEL_EXTRUDE_DIST_KLUDGE, 0.0001).

-type edge_num() :: wings_edge:edge_num().

%%%
%%% The Bump command.
%%%

bump(St0) ->
    Dist = calc_bump_dist(St0),
    MF = fun(Fs, We) -> bump(Fs, Dist, We) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp={finish,_,Tv}}) -> Tv; (_, #we{temp=Tv}) -> Tv end,
    wings_move:plus_minus(normal, DF, St).

calc_bump_dist(St) ->
    Map = fun calc_bump_dist_1/2,
    Combine = fun min/2,
    wings_sel:dfold(Map, Combine, infinity, St) / 2.

calc_bump_dist_1(Faces, We) ->
    Edges = gb_sets:to_list(wings_edge:from_faces(Faces, We)),
    min_dist_from_edges(Edges, We).

bump(Faces, Dist, We0) ->
    Edges = gb_sets:from_list(wings_face:outer_edges(Faces, We0)),
    {We,_,_,_} = extrude_edges(Edges, Faces, Dist, We0),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    PlusMinus = {Faces,NewVs,gb_sets:empty()},
    FaceVs = gb_sets:from_list(wings_face:to_vertices(Faces, We)),
    We#we{temp=uv_temp(PlusMinus, FaceVs, We0, We)}.

%% uv_temp(PlusMinus, MovedVs, We0, We) -> {finish,Fun,Tv} | PlusMinus
%%  The temp of the commands that use wings_move:plus_minus/3 to move the
%%  vertices MovedVs and slide the new ones. Tv is for plus_minus/3, Fun
%%  is called when the drag ends (see wings_drag:normalize_fun/3).
uv_temp({_,NewVs,_}=PlusMinus, MovedVs, We0, We) ->
    case wings_va:any_uvs(We) of
        true ->
            Merge =
                fun(W) ->
                    case wings_pref:get_value(update_uv) of
                        true -> merge_overlapped(NewVs, W);
                        false -> W
                    end
                end,
            {finish,Merge,crease_uv(PlusMinus, MovedVs, We0, We)};
        false ->
            PlusMinus
    end.

%% With the third drag value at 0% some of the new vertices are on top of
%% each other and the faces between them have no area. They are merged when
%% the drag ends (see wings_drag:normalize_fun/3), not while the drag goes
%% on, as the funs depend on the vertices of the mesh.
merge_overlapped(NewVs, We) ->
    case overlapped(NewVs, We) of
        none ->
            We;
        {edge,Edge,Va,Vb} ->
            merge_overlapped_1(NewVs, collapse_zero_edge(Edge, Va, Vb, We), We);
        {face,Face,Va,Vb} ->
            %% Not connected: an edge is made first.
            {We1,Edge} = wings_vertex:force_connect(Va, Vb, Face, We),
            merge_overlapped_1(NewVs, collapse_zero_edge(Edge, Va, Vb, We1), We)
    end.

%% The vertices must go away or there is nothing more to merge.
merge_overlapped_1(NewVs, #we{vp=Vtab}=We, #we{vp=Vtab0}=We0) ->
    case wings_util:array_entries(Vtab) < wings_util:array_entries(Vtab0) of
        true -> merge_overlapped(NewVs, We);
        false -> We0
    end.

%% overlapped(NewVs, We) -> {edge,Edge,Va,Vb} | {face,Face,Va,Vb} | none
overlapped(NewVs, #we{vp=Vtab}=We) ->
    Vs = [V || V <- NewVs, array:get(V, Vtab) =/= undefined],
    Near =
        fun(Va, Vb) ->
            Dist = e3d_vec:dist(wings_vertex:pos(Va, We),
                                wings_vertex:pos(Vb, We)),
            Dist < 1.0e-9
        end,
    Pairs = [{Va,Vb} || Va <- Vs, Vb <- Vs, Va < Vb, Near(Va, Vb)],
    EdgeOf =
        fun({Va,Vb}) ->
            {overlapped_edge(Va, Vb, We),Va,Vb}
        end,
    FacesOf =
        fun({Va,Vb}) ->
            [{face,F,Va,Vb} || F <- overlapped_faces(Va, Vb, We)]
        end,
    Edges = [{edge,Edge,Va,Vb} ||
                {Edge,Va,Vb} <- lists:map(EdgeOf, Pairs), Edge =/= none],
    Faces = lists:append(lists:map(FacesOf, Pairs)),
    Found = Edges ++ Faces,
    case Found of
        [] -> none;
        [First|_] -> First
    end.

overlapped_edge(Va, Vb, We) ->
    wings_vertex:fold(fun(Edge, _, Rec, A) ->
                        case wings_vertex:other(Va, Rec) of
                            Vb -> Edge;
                            _ -> A
                        end
                    end, none, Va, We).

overlapped_faces(Va, Vb, We) ->
    FacesOf =
        fun(V) ->
            wings_vertex:fold(fun(_, F, _, A) ->
                                [F|A]
                              end, [], V, We)
        end,
    ordsets:intersection(ordsets:from_list(FacesOf(Va)),
                         ordsets:from_list(FacesOf(Vb))).

%% wings_collapse:collapse_edge/3 does not keep the UV coordinates of the
%% corners at Vb or at the other vertices of the faces next to the edge:
%% the corners keep the ones saved here (those of Va are given to Vb).
collapse_zero_edge(Edge, Va, Vb, #we{es=Etab}=We0) ->
    #edge{lf=Lf,rf=Rf} = array:get(Edge, Etab),
    Others = ordsets:from_list(wings_face:to_vertices([Lf,Rf], We0)) -- [Va,Vb],
    Saved = [{{F,V},Attr} || V <- [Va,Vb|Others],
                             {_,_,F,Attr} <- wings_va:corners(V, We0)],
    We = wings_collapse:collapse_edge(Edge, Vb, We0),
    foldl(fun(V, W0) ->
            foldl(fun({E,Side,F,_}, W) ->
                    Key =
                        case lists:keymember({F,V}, 1, Saved) of
                            true -> {F,V};
                            false -> {F,Va}
                        end,
                    case lists:keyfind(Key, 1, Saved) of
                        {_,Attr} -> wings_va:set_edge_attrs(E, Side, Attr, W);
                        false -> W
                    end
                end, W0, wings_va:corners(V, W0))
          end, We, [Vb|Others]).

%%
%% The Bevel command (for edges).
%%

bevel(St0) ->
    St = wings_sel:map_update_sel(fun bevel_edges/2, face, St0),
    bevel_drag(St).

bevel_edges(Edges, #we{mirror=MirrorFace}=We0) ->
    Dist = ?BEVEL_EXTRUDE_DIST_KLUDGE,
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, Dist, We0#we{mirror=none}),
    We2 = wings_edge:dissolve_edges(Edges, We1),
    Tv0 = bevel_tv(OrigVs, We2, Forbidden),
    Tv = scale_tv(Tv0, Dist),
    We3 = wings_collapse:collapse_vertices(OrigVs, We2),
    Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, We3#we.vp),
    We4 = We3#we{vp=Vtab,mirror=MirrorFace},
    Limit = bevel_limit(Tv, We4),
    UvData = bevel_uv_data(OrigVs, Forbidden, Tv, We0, We2, We4),
    We = We4#we{temp={Limit,Tv,UvData}},
    Sel =
        case gb_sets:is_empty(Forbidden) of
            true -> wings_we:new_items_as_gbset(face, We0, We);
            false -> gb_sets:empty()
        end,
    {We,Sel}.

%%
%% The Bevel command (for faces).
%%

bevel_faces(St0) ->
    St = wings_sel:map(fun bevel_faces/2, St0),
    bevel_drag(St).

bevel_faces(Faces, #we{mirror=MirrorFace}=We0) ->
    Dist = ?BEVEL_EXTRUDE_DIST_KLUDGE,
    Edges = wings_edge:from_faces(Faces, We0),
    {We1,OrigVs,_,Forbidden} = extrude_edges(Edges, Dist, We0#we{mirror=none}),
    case {wings_util:array_entries(We0#we.es),wings_util:array_entries(We1#we.es)} of
        {Same,Same} ->
            wings_u:error_msg(?__(1,"Object is too small to bevel."));
        {_,_} ->
            We2 = wings_edge:dissolve_edges(Edges, We1),
            Tv0 = bevel_tv(OrigVs, We2, Forbidden),
            Tv = scale_tv(Tv0, Dist),
            #we{vp=Vtab0} = We3 = wings_collapse:collapse_vertices(OrigVs, We2),
            Vtab = bevel_reset_pos(OrigVs, We2, Forbidden, Vtab0),
            We = We3#we{vp=Vtab,mirror=MirrorFace},
            Limit = bevel_limit(Tv, We),
            UvData = bevel_uv_data(OrigVs, Forbidden, Tv, We0, We2, We),
            We#we{temp={Limit,Tv,UvData}}
    end.

%%
%% Common bevel utilities.
%%

bevel_drag(St) ->
    MF = fun(_, #we{temp={Limit,_,_}}) -> Limit end,
    RF = fun min/2,
    Limit = wings_sel:dfold(MF, RF, infinite, St),
    Follow = wings_va:any_uvs(St),
    DF = fun(_, #we{temp={_,Tv,UvData}}=We) ->
            uv_tv(Follow, bevel_tv_fun(Tv, UvData, We))
         end,
    wings_drag:fold(DF, [{distance,{0.0,Limit}}], uv_flags(Follow), St).

bevel_tv_fun(Tv, UvData, We) ->
    Tr = wings_drag:translate_fun(Tv, We),
    case UvData of
        none -> Tr;
        _ -> {we,[bevel_uv_fun(UvData, We)],Tr}
    end.

%% The key [1] switches the update of the UV coordinates. The transformation
%% fun must accept the change of the mode.
uv_flags(true) -> [{mode,{fun uv_mode/2,uv}}];
uv_flags(false) -> [].

uv_mode(help, _) ->
    [?__(1,"[Shift] and/or [Ctrl] and/or [Alt] Constrain"),
     "  [1] ",uv_help()];
uv_mode({key,$1}, Mode) ->
    wings_pref:set_value(update_uv, not wings_pref:get_value(update_uv)),
    Mode;
uv_mode(_, _) ->
    none.

uv_help() ->
    case wings_pref:get_value(update_uv) of
        true -> ?__(1,"Turn Off UV");
        false -> ?__(2,"Turn On UV")
    end.

uv_tv(true, {Vs,Tr}) ->
    {Vs,uv_tr(Tr)};
uv_tv(true, {we,WeFuns,Tv}) ->
    {we,WeFuns,uv_tv(true, Tv)};
uv_tv(false, Tv) ->
    Tv.

uv_tr(Tr) ->
    fun(Move, Acc) when is_list(Move) -> Tr(Move, Acc);
       (new_mode_data, {uv,_}) -> uv_tr(Tr);
       (Key, Val) -> uv_tr(Tr(Key, Val))
    end.

%%%
%%% Bevel: the UV coordinates follow the new vertices while dragging.
%%% A new vertex either slides along an original edge ('rate') or moves
%%% inside an extruded face ('wedge'). Corners of new faces without UV
%%% continue the mapping of a neighbour face.
%%%

%% bevel_uv_data(OrigVs, Forbidden, Tv, We0, We2, We) -> none | {[NvData],Fill}
%%  NvData = {NV,Vec,Kind,[Class]}
%%  Kind = {rate,Dist} | wedge
%%  Class = {[{Edge,Side,Color}],UV,Rate}
%%  Rate = {DU,DV} | Wedge
%%  We0 is the original object, We2 the object before the original
%%  vertices are collapsed and We the object when the drag starts.
bevel_uv_data(OrigVs, Forbidden, Tv, We0, We2, #we{vc=Vct}=We) ->
    case wings_va:any_uvs(We) of
        false ->
            none;
        true ->
            VecList = [{NV,Vec} || {Vec,NVs} <- Tv, NV <- NVs],
            Vecs = gb_trees:from_orddict(lists:ukeysort(1, VecList)),
            Kinds = bevel_uv_kinds(OrigVs, Forbidden, We0, We2),
            HasVec =
                fun({_,NV,_}) ->
                    array:get(NV, Vct) =/= undefined andalso gb_trees:is_defined(NV, Vecs)
                end,
            VertexData =
                fun({_,NV,Kind}) ->
                    bevel_uv_vertex(NV, gb_trees:get(NV, Vecs), Kind, We0, We)
                end,
            Nvs = lists:map(VertexData, lists:filter(HasVec, Kinds)),
            {Nvs,bevel_uv_fill_data(Vecs, We0, We)}
    end.

bevel_uv_vertex(NV, Vec, {rate,Dist,Rates}, #we{fs=Fs0}, We) ->
    Classes = bevel_uv_classes(NV, Rates, [F || {F,_} <- Rates], Fs0, We),
    {NV,Vec,{rate,Dist},Classes};
bevel_uv_vertex(NV, Vec, {wedge,Wedge,Face}, #we{fs=Fs0}, We) ->
    Classes = bevel_uv_classes(NV, [], [Face], Fs0, We),
    {NV,Vec,wedge,[{Cs,UV,Wedge} || {Cs,UV,_} <- Classes]}.

%% bevel_uv_kinds(Vs, Forbidden, We0, We2) -> [{V,NV,Kind}]
%%  Kind = {rate,Dist,[{Face,{DU,DV}}]} | {wedge,Wedge,Face}
%%  {DU,DV} is the change of UV per unit of length when moving away
%%  from V along the edge (V,NV).
bevel_uv_kinds(Vs, Forbidden, #we{next_id=Next}=We0, We2) ->
    foldl(fun(V, A) ->
            wings_vertex:fold(
            fun(Edge, _, #edge{lf=Lf,rf=Rf}=Rec, Acc) ->
                case gb_sets:is_member(Edge, Forbidden) of
                    true ->
                        Acc;
                    false ->
                        NV = wings_vertex:other(V, Rec),
                        Kind = bevel_uv_kind(V, NV, Lf, Rf, Next, We0, We2),
                        [{V,NV,Kind}|Acc]
                end
            end, A, V, We2)
          end, [], Vs).

%% A vertex cut from an original edge is in two original faces, a vertex of
%% an extruded face in one.
bevel_uv_kind(V, NV, _, _, Next, We0, We2) ->
    Faces = [F || {_,_,F,_} <- wings_va:corners(NV, We2), F < Next],
    case Faces of
        [Face] ->
            {wedge,wings_va:uv_wedge(V, Face, We0),Face};
        _ ->
            Dist = e3d_vec:dist(wings_vertex:pos(V, We0),
                                wings_vertex:pos(NV, We2)),
            {rate,Dist,bevel_uv_edge_rates(V, NV, We0, We2)}
    end.

%% bevel_uv_edge_rates(V, NV, We0, We2) -> [{Face,{DU,DV}}]
%%  Uses We0 because the faces of We2 near V may have lost their UVs.
bevel_uv_edge_rates(V, NV, We0, We2) ->
    Pos = wings_vertex:pos(V, We0),
    Dir = e3d_vec:norm(e3d_vec:sub(wings_vertex:pos(NV, We2), Pos)),
    Es = wings_vertex:fold(
           fun(_, _, #edge{lf=Lf,rf=Rf}=Rec, Acc) ->
                Z = wings_vertex:other(V, Rec),
                ZVec = e3d_vec:sub(wings_vertex:pos(Z, We0), Pos),
                Len = e3d_vec:len(ZVec),
                case Len > 0.0 andalso e3d_vec:dot(Dir, ZVec)/Len > 0.999999 of
                    true -> [{Z,Len,[Lf,Rf]}|Acc];
                    false -> Acc
                end
           end, [], V, We0),
    case Es of
        [{Z,Len,Faces}|_] ->
            Rates = [bevel_uv_rate(V, Z, F, Len, We0) || F <- lists:usort(Faces)],
            [R || R <- Rates, R =/= none];
        [] ->
            []
    end.

bevel_uv_rate(V, NV, Face, Dist, We) ->
    case {wings_va:attr(uv, wings_va:vtx_attrs(V, Face, We)),
          wings_va:attr(uv, wings_va:vtx_attrs(NV, Face, We))} of
        {{U0,V0},{U1,V1}} when Dist > 0.0 ->
            {Face,{(U1-U0)/Dist,(V1-V0)/Dist}};
        _ ->
            none
    end.

%% bevel_uv_classes(NV, Rates, RefFaces, Fs0, We) -> [Class]
bevel_uv_classes(NV, Rates, RefFaces, Fs0, We) ->
    Corners = wings_va:corners(NV, We),
    WithUv = [{Edge,Side,Face,Col,UV} || {Edge,Side,Face,[Col|{_,_}=UV]} <- Corners],
    NewCs = bevel_uv_new_corners(NV, Fs0, We),
    Groups = bevel_uv_group(WithUv, []),
    Ref = bevel_uv_ref(Groups, lists:sort(RefFaces)),
    NewOf = fun(Group) when Group =:= Ref -> NewCs;
               (_) -> []
            end,
    [bevel_uv_class({Group,NewOf(Group)}, Rates) || Group <- Groups].

bevel_uv_new_corners(NV, Fs0, We) ->
    NoUV = [C || {_,_,Face,Attr}=C <- wings_va:corners(NV, We),
                 not is_uv_attr(Attr),
                 not gb_trees:is_defined(Face, Fs0)],
    [{Edge,Side,bevel_uv_color(Attr)} || {Edge,Side,_,Attr} <- NoUV].

is_uv_attr([_|{_,_}]) -> true;
is_uv_attr(_) -> false.

bevel_uv_ref([Class|_]=Classes, Faces) ->
    Found = [Cl || F <- Faces, Cl <- Classes, lists:keymember(F, 3, Cl)],
    case Found of
        [Cl|_] -> Cl;
        [] -> Class
    end;
bevel_uv_ref([], _) -> [].

bevel_uv_class({[{_,_,_,_,UV}|_]=Corners,NewCs}, Rates) ->
    Faces = [F || {_,_,F,_,_} <- Corners, lists:keymember(F, 1, Rates)],
    Rate =
        case {Faces,Rates} of
            {[],[]} -> {0.0,0.0};
            {[],_} -> element(2, lists:min(Rates));
            {_,_} -> element(2, lists:keyfind(lists:min(Faces), 1, Rates))
        end,
    {[{Edge,Side,Col} || {Edge,Side,_,Col,_} <- Corners]++NewCs,UV,Rate}.

%% Group the corners that have the same UV.
bevel_uv_group([{_,_,_,_,{U,V}}=Corner|Cs], Classes) ->
    {Same,Other} =
        lists:partition(fun([{_,_,_,_,{U0,V0}}|_]) ->
                            abs(U-U0) < 1.0e-6 andalso abs(V-V0) < 1.0e-6
                        end, Classes),
    bevel_uv_group(Cs, [[Corner|lists:append(Same)]|Other]);
bevel_uv_group([], Classes) -> Classes.

bevel_uv_fun({Nvs,Fill}, #we{lv=Lv0,rv=Rv0}) ->
    fun(We, [Dx|_]) ->
        case wings_pref:get_value(update_uv) of
            false ->
                We#we{lv=Lv0,rv=Rv0};
            true ->
                W1 = foldl(fun({_,Vec,Kind,Classes}, W0) ->
                                Delta = bevel_uv_delta(Kind, Vec, Dx),
                                foldl(fun({Corners,{U,V},Rate}, W2) ->
                                        {DU,DV} = Delta(Rate),
                                        UV = {U+DU,V+DV},
                                        foldl(fun({Edge,Side,Col}, W3) ->
                                                  wings_va:set_edge_attrs(Edge, Side, [Col|UV], W3)
                                              end, W2, Corners)
                                        end, W0, Classes)
                            end, We, Nvs),
                bevel_uv_fill(Fill, Dx, W1)
        end
    end.

bevel_uv_delta({rate,Dist}, Vec, Dx) ->
    L = Dx*e3d_vec:len(Vec) - Dist,
    fun({DU,DV}) -> {DU*L,DV*L} end;
bevel_uv_delta(wedge, Vec, Dx) ->
    DP = e3d_vec:mul(Vec, Dx),
    fun(Wedge) -> wings_va:uv_wedge_delta(Wedge, DP) end.

%%%
%%% Unfolding a new face into a neighbor face.
%%%

%% bevel_uv_fill_data(Vecs, We0, We) -> {Faces,Moves}
%%  Faces = [{Face,[{V,Edge,Side,Color,Across}]}]
%%  Across = [{V,Edge,Side}] (the corners of the face on the other side of
%%           the edge that goes from V to the next vertex of Face)
%%  Moves = gb_trees: V -> {Pos,Vec}
%%  Only the new faces (those not in the original object) that have corners
%%  without UV coordinates are included.
bevel_uv_fill_data(Vecs, #we{fs=Fs0}, #we{fs=Ftab}=We) ->
    NewFaces = [F || F <- gb_trees:keys(Ftab), not gb_trees:is_defined(F, Fs0)],
    FaceData = [bevel_uv_fill_face(F, We) || F <- NewFaces],
    Fs = [Data || Data <- FaceData, Data =/= none],
    Vs = lists:usort(lists:append(lists:map(fun bevel_uv_fill_vertices/1, Fs))),
    VecOf =
        fun(V) ->
            case gb_trees:lookup(V, Vecs) of
                {value,Vec} -> Vec;
                none -> {0.0,0.0,0.0}
            end
        end,
    Moves = [{V,{wings_vertex:pos(V, We),VecOf(V)}} || V <- Vs],
    {Fs,gb_trees:from_orddict(Moves),wings_va:seam_strips(NewFaces, We)}.

bevel_uv_fill_vertices({_,Cs}) ->
    Own = [V || {V,_,_,_,_} <- Cs],
    Across = [V || {_,_,_,_,Acr} <- Cs, {V,_,_} <- Acr],
    Own ++ Across.

bevel_uv_fill_face(Face, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    Cs = [bevel_uv_fill_corner(V, Next, Face, We) ||
             {V,Next} <- lists:zip(Vs, tl(Vs)++[hd(Vs)])],
    NoUV = [C || {_,_,_,Attr,_}=C <- Cs, not is_uv_attr(Attr)],
    case NoUV of
        [] -> none;
        _ -> {Face,[{V,E,Side,bevel_uv_color(Attr),Across} ||
                       {V,E,Side,Attr,Across} <- Cs]}
    end.

bevel_uv_fill_corner(V, Next, Face, We) ->
    {Edge,Side,_,Attr} = lists:keyfind(Face, 3, wings_va:corners(V, We)),
    CornersOf =
        fun(Other) ->
            VsCcw = wings_face:vertices_ccw(Other, We),
            Cs = [{V2,bevel_uv_corner(V2, Other, We)} || V2 <- VsCcw],
            [{V2,E2,S2} || {V2,{E2,S2,_,_}} <- Cs, E2 =/= none]
        end,
    Across =
        case wings_vertex:edge_through(V, Next, Face, We) of
            none ->
                [];
            EdgeNext ->
                #edge{lf=Lf,rf=Rf} = array:get(EdgeNext, We#we.es),
                Other =
                    case Lf of
                        Face -> Rf;
                        _ -> Lf
                    end,
                CornersOf(Other)
        end,
    {V,Edge,Side,Attr,Across}.

bevel_uv_corner(V, Face, We) ->
    case lists:keyfind(Face, 3, wings_va:corners(V, We)) of
        false -> {none,none,none,none};
        {E,S,F,A} -> {E,S,F,A}
    end.

bevel_uv_color([Col|_]) -> Col;
bevel_uv_color(_) -> none.

%% Positions come from the start positions: those in We are not moved yet.
bevel_uv_fill({Faces,Moves,Seams}, Dx, We) ->
    Pos = fun(V) ->
            {P,Vec} = gb_trees:get(V, Moves),
            e3d_vec:add(P, e3d_vec:mul(Vec, Dx))
          end,
    We1 = foldl(fun({_Face,Cs}, W) ->
                    bevel_uv_fill_face(Cs, Pos, W)
                end, We, Faces),
    wings_va:seam_strip_uv(Seams, We1).

bevel_uv_fill_face(Cs, Pos, We) ->
    Uvs = [bevel_uv_get(E, Side, We) || {_,E,Side,_,_} <- Cs],
    CUs = lists:zip(Cs, Uvs),
    Pairs = lists:zip(CUs, tl(CUs)++[hd(CUs)]),
    Anchors = [{C,CNext,Uv,UvNext} ||
                  {{C,Uv},{CNext,UvNext}} <- Pairs,
                  Uv =/= none, UvNext =/= none],
    case Anchors of
        [] ->
            We;
        [{{Va,_,_,_,Across},{Vb,_,_,_,_},UvA,UvB}|_] ->
            Pa = Pos(Va),
            Pb = Pos(Vb),
            foldl(fun({V,E,Side,Col,_}, W) ->
                    case bevel_uv_get(E, Side, W) of
                        none ->
                            UV = bevel_uv_unfold(Pos(V), Pa, Pb, UvA, UvB, Across, Pos, W),
                            wings_va:set_edge_attrs(E, Side, [Col|UV], W);
                        _ ->
                            W
                    end
                  end, We, Cs)
    end.

bevel_uv_get(Edge, Side, We) ->
    case wings_va:edge_attrs(Edge, Side, We) of
        [_|{_,_}=UV] -> UV;
        _ -> none
    end.

%% Rotates the new face about its edge (Pa,Pb) into the plane of the face
%% Across and maps P with that face. Without a usable mapping P gets the
%% UV of Pa.
bevel_uv_unfold(P, Pa, Pb, UvA, UvB, Across, Pos, We) ->
    E = e3d_vec:sub(Pb, Pa),
    Len = e3d_vec:len(E),
    UVs = [{V,bevel_uv_get(Edge, Side, We)} || {V,Edge,Side} <- Across],
    Pts = [{Pos(V),UV} || {V,UV} <- UVs, UV =/= none],
    case Len > 1.0e-9 andalso bevel_uv_map(Pts) of
        false ->
            UvA;
        none ->
            UvA;
        Map ->
            U = e3d_vec:mul(E, 1/Len),
            %% The direction (in the plane of Across) from the edge into Across.
            In = bevel_uv_inward(Pts, Pa, U),
            case In of
                none ->
                    UvA;
                _ ->
                    D = e3d_vec:sub(P, Pa),
                    S = e3d_vec:dot(D, U),
                    Perp = e3d_vec:sub(D, e3d_vec:mul(U, S)),
                    T = e3d_vec:len(Perp),
                    Q = e3d_vec:sub(e3d_vec:add(Pa, e3d_vec:mul(U, S)),
                                    e3d_vec:mul(In, T)),
                    Map(Q, {UvA,UvB})
            end
    end.

bevel_uv_inward(Pts, Pa, U) ->
    PerpOf =
        fun(P) ->
            D = e3d_vec:sub(P, Pa),
            Perp = e3d_vec:sub(D, e3d_vec:mul(U, e3d_vec:dot(D, U))),
            {e3d_vec:len(Perp),Perp}
        end,
    Perps = [PerpOf(P) || {P,_} <- Pts] ++ [{0.0,none}],
    case lists:max(Perps) of
        {L,Perp} when L > 1.0e-9 -> e3d_vec:mul(Perp, 1/L);
        _ -> none
    end.

%% bevel_uv_map(Pts) -> Map | none
%%  Pts = [{Pos,UV}]. Map(Q, _) is the UV of the point Q in the plane of
%%  three points that are not on a line (a least squares fit is not needed
%%  because the mapping of a face is linear).
bevel_uv_map([{P0,{U0,V0}}|Pts]) ->
    case bevel_uv_pick(Pts, P0) of
        {{P1,{U1,V1}},{P2,{U2,V2}}} ->
            E1 = e3d_vec:sub(P1, P0),
            E2 = e3d_vec:sub(P2, P0),
            G11 = e3d_vec:dot(E1, E1), G22 = e3d_vec:dot(E2, E2),
            G12 = e3d_vec:dot(E1, E2),
            Det = G11*G22 - G12*G12,
            Ka = e3d_vec:mul(e3d_vec:sub(e3d_vec:mul(E1, G22),
                                         e3d_vec:mul(E2, G12)), 1/Det),
            Kb = e3d_vec:mul(e3d_vec:sub(e3d_vec:mul(E2, G11),
                                         e3d_vec:mul(E1, G12)), 1/Det),
            fun(Q, _) ->
                DQ = e3d_vec:sub(Q, P0),
                A = e3d_vec:dot(DQ, Ka),
                B = e3d_vec:dot(DQ, Kb),
                {U0+A*(U1-U0)+B*(U2-U0),V0+A*(V1-V0)+B*(V2-V0)}
            end;
        none ->
            none
    end;
bevel_uv_map([]) -> none.

bevel_uv_pick(Pts, P0) ->
    Cands = [{P,UV} || {P,UV} <- Pts, e3d_vec:dist(P, P0) > 1.0e-9],
    Pairs = [{A,B} || A <- Cands, B <- Cands, A < B,
                      bevel_uv_noncollinear(P0, element(1, A), element(1, B))],
    case Pairs of
        [Pair|_] -> Pair;
        [] -> none
    end.

bevel_uv_noncollinear(P0, P1, P2) ->
    E1 = e3d_vec:sub(P1, P0),
    E2 = e3d_vec:sub(P2, P0),
    e3d_vec:len(e3d_vec:cross(E1, E2)) >
        1.0e-6*e3d_vec:len(E1)*e3d_vec:len(E2).

bevel_tv(Vs, We, Forbidden) ->
    foldl(fun(V, A) -> bevel_tv_1(V, We, Forbidden, A) end, [], Vs).

bevel_tv_1(V, We, Forbidden, Acc) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Tv0) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Tv0;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      Pos = wings_vertex:pos(OtherV, We),
		      Vec = e3d_vec:sub(Pos, Center),
		      [{Vec,[OtherV]}|Tv0]
	      end
      end, Acc, V, We).

bevel_reset_pos(Vs, We, Forbidden, Vtab) ->
    foldl(fun(V, A) ->
              bevel_reset_pos_1(V, We, Forbidden, A)
          end, Vtab, Vs).

bevel_reset_pos_1(V, We, Forbidden, Vtab) ->
    Center = wings_vertex:pos(V, We),
    wings_vertex:fold(
      fun(Edge, _, Rec, Vt) ->
	      case gb_sets:is_member(Edge, Forbidden) of
		  true -> Vt;
		  false ->
		      OtherV = wings_vertex:other(V, Rec),
		      array:set(OtherV, Center, Vt)
	      end
      end, Vtab, V, We).

bevel_limit(Tv, We) ->
    L0 = foldl(fun({Vec,[V]}, A) ->
		       bevel_limit_1(V, Vec, We, A)
	       end, [], Tv),
    L = wings_util:rel2fam(L0),
    try
	bevel_min_limit(L, We, infinite)
    catch
	error:badarith ->
	    extrude_problem()
    end.

bevel_limit_1(V, Vec, #we{vp=Vtab}=We, Acc) ->
    Pos = array:get(V, Vtab),
    Data = {Pos,Vec},
    wings_vertex:fold(fun(_, _, Rec, A) ->
			      OtherV = wings_vertex:other(V, Rec),
			      [{edge_name(V, OtherV),Data}|A]
		      end, Acc, V, We).

edge_name(Va, Vb) when Va < Vb -> {Va,Vb};
edge_name(Va, Vb) -> {Vb,Va}.

bevel_min_limit([{_Edge,[{O1,D1},{O2,D2}]}|Tail], We, Min0) ->
    %% Find intersection between lines.
    O2MinusO1 = e3d_vec:sub(O2, O1),
    D1CrossD2 = e3d_vec:cross(D1, D2),
    LenD1CrossD2 = e3d_vec:len(D1CrossD2),
    case LenD1CrossD2*LenD1CrossD2 of
	Z when Z < (0.01*?DEFAULT_EXTRUDE_DIST*?DEFAULT_EXTRUDE_DIST) ->
	    %% There is no intersection between the lines.
	    case e3d_vec:len(O2MinusO1) of
		Odist when Odist < ?EPSILON ->
		    %% As the vertices are already near each other,
		    %% we will assume that they will be moving apart.
		    bevel_min_limit(Tail, We, Min0);
		Odist ->
		    D1Len = e3d_vec:len(D1),
		    case e3d_vec:dist(e3d_vec:mul(D1, Odist/D1Len), O2MinusO1) of
			Dist when Dist < ?EPSILON ->
			    %% The vertices will be moved directly towards each
			    %% others.
			    S = Odist / (2*D1Len),
			    bevel_min_limit(Tail, We, min(Min0, S));
			_ ->
			    %% The vertices will not meet each other.
			    bevel_min_limit(Tail, We, Min0)
		    end
	    end;
	SqrLen ->
	    %% There is an intersection. Calculate its parameters.
	    S = e3d_vec:dot(e3d_vec:cross(O2MinusO1, D2), D1CrossD2)/SqrLen,
	    T = e3d_vec:dot(e3d_vec:cross(O2MinusO1, D1), D1CrossD2)/SqrLen,
	    if
		S > 0, T > 0 ->
		    Min = min(S, min(T, Min0)),
		    bevel_min_limit(Tail, We, Min);
		true ->
		    %% No intersection in the forward direction.
		    bevel_min_limit(Tail, We, Min0)
	    end
    end;
bevel_min_limit([{{Va,Vb},[{_,D1}]}|Tail], #we{vp=Vtab}=We, Min0) ->
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    case e3d_vec:len(D1) of
	DLen when DLen < ?EPSILON ->
	    bevel_min_limit(Tail, We, 0.0);
	DLen ->
	    case e3d_vec:len(e3d_vec:sub(VaPos, VbPos)) / DLen of
		Min when Min < Min0 ->
		    bevel_min_limit(Tail, We, Min);
		_ ->
		    bevel_min_limit(Tail, We, Min0)
	    end
    end;
bevel_min_limit([], _, Min) -> Min.

-spec extrude_problem() -> no_return().
extrude_problem() ->
    M = ?__(1,"Can't extrude/bevel; two or more vertices are "
	    "probably too near to each other.\n"
	    "Try the Cleanup command."),
    wings_u:error_msg(M).

%%
%% Crease command
%%

crease(St0) ->
    Dist = calc_extrude_dist(St0),
    MF = fun(Edges, We0) -> crease_edges(Edges, Dist, We0) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp={finish,_,Tv}}) -> Tv; (_, #we{temp=Tv}) -> Tv end,
    wings_move:plus_minus(normal, DF, St).

crease_edges(Edges, Dist, We0) ->
        We1 = extrude_1(Edges, Dist, We0),
        #we{temp={_,NewVs0,F}} = We1,
        ValidCaps = find_cap_vs(Edges, We0, []),
        #we{vp=Vtab}=We = foldl(fun(V, We2) ->
            EndCap = wings_vertex:fold(fun
              (Edge, _, #edge{lf=Lf,rf=Rf}=E, EndCapAcc) ->
                case wings_face:vertices(Lf, We2) of
                    3 ->
                      case wings_face:vertices(Rf, We2) of
                          3 ->
                              Other = wings_vertex:other(V, E),
                              case lists:member(Other, ValidCaps) of
                                  true -> [{d,Edge}|EndCapAcc];
                                  false -> EndCapAcc
                              end;
                          _ -> EndCapAcc
                      end;
                    N1 when N1 > 4 ->
                      case wings_face:vertices(Rf, We2) of
                          N2 when N2 > 4 ->
                            Other = wings_vertex:other(V, E),
                            [{c,Edge,Other}|EndCapAcc];
                          _ -> EndCapAcc
                      end;
                    _ -> EndCapAcc
                end
            end, [], V, We2), % vertex fold
            case lists:sort(EndCap) of
                [{d,E1},{c,E2,V1}] ->
                    We3 = wings_edge:dissolve_edge(E1, We2),
                    Pos = wings_vertex:pos(V1, We3),
                    Saved = crease_uv_save(V1, We3),
                    #we{vp=Vtab0}=We4 = wings_collapse:collapse_edge(E2, V1, We3),
                    Vtab = array:set(V1, Pos, Vtab0),
                    crease_uv_restore(V1, Saved, We4#we{vp=Vtab});
                [{d,E1}|_] ->
                    #we{vp=Vtab0}=We3 = wings_edge:dissolve_edge(E1, We2),
                    RVs = wings_vertex:fold(fun(_,_,Erec,RVs0) ->
                        [wings_vertex:other(V, Erec)|RVs0]
                    end, [], V, We3),
                    Center = wings_vertex:center(RVs, We3),
                    Vtab = array:set(V, Center, Vtab0),
                    (crease_uv_move(V, Center, We3))#we{vp=Vtab};
                _ -> We2
            end
        end, We1, NewVs0), % list foldl
        AllVs = orddict:fetch_keys(array:sparse_to_orddict(Vtab)),
        NewVs =  ordsets:intersection(lists:sort(NewVs0), AllVs),
        EdgeVs = gb_sets:from_list(wings_edge:to_vertices(Edges, We0)),
        We#we{temp=uv_temp({Edges,NewVs,F}, EdgeVs, We0, We)}.

%%%
%%% Crease: the new vertices slide by Vec*(Percent-1) (third drag value),
%%% so their corners follow the mapping of an original face (wedge).
%%%

%% The end caps collapse or move vertices without looking at the UVs;
%% the corners of those vertices are saved/restored/moved here.
%% crease_uv_save(V, We) -> [{Face,Attr}] | none
crease_uv_save(V, We) ->
    case wings_va:follow_uvs(We) of
        true -> [{F,Attr} || {_,_,F,Attr} <- wings_va:corners(V, We)];
        false -> none
    end.

%% crease_uv_restore(V, Saved, We) -> We
%%  A corner in a face that had no corner of V gets the attributes of the
%%  corner of V in the face with the lowest number.
crease_uv_restore(_, none, We) ->
    We;
crease_uv_restore(V, Saved, We) ->
    [{_,Default}|_] = lists:sort(Saved),
    foldl(fun({Edge,Side,Face,_}, W) ->
            Attr =
                case lists:keyfind(Face, 1, Saved) of
                    {_,A} -> A;
                    false -> Default
                end,
            wings_va:set_edge_attrs(Edge, Side, Attr, W)
          end, We, wings_va:corners(V, We)).

%% crease_uv_move(V, Pos, We) -> We
%%  Moves the UV coordinates of the corners of V as if V was moved to Pos.
crease_uv_move(V, Pos, We) ->
    case wings_va:follow_uvs(We) of
        true ->
            DP = e3d_vec:sub(Pos, wings_vertex:pos(V, We)),
            wings_va:uv_move(wings_va:uv_move_data([V], We), fun(_) -> DP end, We);
        false ->
            We
    end.

%% crease_uv(PlusMinus, MovedVs, We0, We) -> PlusMinus
%%  PlusMinus is the data for wings_move:plus_minus/3 (also used by Bump);
%%  MovedVs are the vertices that it moves along the normal. A UV fun is
%%  added if needed.
crease_uv({_,NewVs,Forbidden}=PlusMinus, MovedVs, We0, We) ->
    case wings_va:any_uvs(We) of
        false ->
            PlusMinus;
        true ->
            case crease_uv_data(MovedVs, NewVs, Forbidden, We0, We) of
                [] -> PlusMinus;
                Nvs -> {we,[crease_uv_fun(Nvs, MovedVs, NewVs)],PlusMinus}
            end
    end.

%% crease_uv_data(EdgeVs, NewVs, Forbidden, We0, We) -> [{NV,Vec,wedge,[Class]}]
crease_uv_data(EdgeVs, NewVs, Forbidden, #we{next_id=Next,fs=Fs0}, We) ->
    %% The same vectors as wings_move:move_vectors/5.
    Vecs = [{NV,crease_uv_vec(NV, EdgeVs, Forbidden, We)} || NV <- NewVs],
    VertexData =
        fun({NV,Vec}) ->
            crease_uv_vertex(NV, Vec, Next, Fs0, We)
        end,
    Data = lists:map(VertexData, [Item || {_,Vec}=Item <- Vecs, Vec =/= none]),
    [Nv || Nv <- Data, Nv =/= none].

crease_uv_vec(NV, EdgeVs, Forbidden, We) ->
    Pos = wings_vertex:pos(NV, We),
    wings_vertex:fold(
        fun(Edge, _, Rec, Acc) ->
            Other = wings_vertex:other(NV, Rec),
            case gb_sets:is_member(Other, EdgeVs) andalso
                not gb_sets:is_member(Edge, Forbidden) of
                false ->
                    Acc;
                true ->
                    Vec = e3d_vec:sub(Pos, wings_vertex:pos(Other, We)),
                    case Acc of
                        none -> Vec;
                        _ -> e3d_vec:add(Acc, Vec)
                    end
            end
        end, none, NV, We).

crease_uv_vertex(NV, Vec, Next, Fs0, We) ->
    Corners = wings_va:corners(NV, We),
    Faces = [F || {_,_,F,Attr} <- Corners, F < Next, is_uv_attr(Attr)],
    case lists:sort(Faces) of
        [Face|_] ->
            Classes = bevel_uv_classes(NV, [], [Face], Fs0, We),
            Nv = [{Cs,UV,crease_uv_wedge(NV, Cs, Corners, We)} ||
                     {Cs,UV,_} <- Classes],
            WithWedge = [W || {_,_,W} <- Nv, W =/= none],
            case WithWedge of
                [] -> none;
                _ -> {NV,Vec,wedge,Nv}
            end;
        [] ->
            none
    end.

%% crease_uv_wedge(NV, Corners, AllCorners, We) -> Wedge | none
%%  Each class of corners follows the texture of its own faces, because the
%%  faces around the vertex may be in different planes (or islands). The
%%  best conditioned wedge is used, since some corners are degenerate (for
%%  example a triangle with the three vertices on a line).
crease_uv_wedge(NV, Cs, Corners, We) ->
    ClassCorners = [{E,S} || {E,S,_} <- Cs],
    Faces = [F || {E,S,F,_} <- Corners, lists:member({E,S}, ClassCorners)],
    WedgeOf =
        fun(F) ->
            wings_va:uv_wedge(NV, F, We)
        end,
    Wedges = [W || W <- lists:map(WedgeOf, lists:usort(Faces)), W =/= none],
    case Wedges of
        [] -> none;
        _ -> element(2, lists:max([{crease_wedge_det(W),W} || W <- Wedges]))
    end.

crease_wedge_det({E1,E2,_,_,_,_}) ->
    e3d_vec:len(e3d_vec:cross(E1, E2)).

crease_uv_fun(Nvs, MovedVs, NewVs) ->
    fun(We, [_,_,Percent|_]=Move, TrFun) ->
        We1 =
            foldl(fun({_,Vec,Kind,Classes}, W0) ->
                    Delta = bevel_uv_delta(Kind, Vec, Percent-1.0),
                    foldl(fun({Corners,{U,V},Wedge}, W1) ->
                            {DU,DV} = Delta(Wedge),
                            UV = {U+DU,V+DV},
                            foldl(fun({Edge,Side,Col}, W2) ->
                                    wings_va:set_edge_attrs(Edge, Side, [Col|UV], W2)
                                    end, W1, Corners)
                            end, W0, Classes)
                    end, We, Nvs),
        crease_uv_walls(TrFun, Move, MovedVs, NewVs, We1)
    end.

crease_uv_walls(none, _, _, _, We) ->
    We;
crease_uv_walls(TrFun, Move, MovedVs, NewVs, We) ->
    Vp = foldl(fun({V,Pos}, A) ->
                    array:set(V, Pos, A)
                end, We#we.vp, TrFun(Move, [])),
    We1 = wings_va:wall_uv_strips(MovedVs, NewVs, We#we{vp=Vp}),
    We1#we{vp=We#we.vp}.

find_cap_vs(Edges0, #we{es=Etab}=We, Acc) ->
    case gb_sets:is_empty(Edges0) of
      true ->
          CapVs = find_cap_vs(lists:sort(Acc), []),
          valid_caps(CapVs, We);
      false ->
          {Edge,Edges} = gb_sets:take_smallest(Edges0),
          #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
          find_cap_vs(Edges, We, [Va,Vb|Acc])
    end.

find_cap_vs([V,V|Vs0], CapVs) ->
    Vs = rem_v(V, Vs0),
    find_cap_vs(Vs, CapVs);
find_cap_vs([V|Vs], CapVs) ->
    find_cap_vs(Vs, [V|CapVs]);
find_cap_vs([], CapVs) ->
    CapVs.

rem_v(V, [V|Vs]) ->
    rem_v(V, Vs);
rem_v(_, Vs) ->
    Vs.

valid_caps(CapVs, We) ->
    foldl(fun(V, Acc) ->
        Count = wings_vertex:fold(fun(_, _, _, Cnt) ->
            Cnt+1
        end, 0, V, We),
        case Count rem 2 of
            0 -> [V|Acc];
            _ -> Acc
        end
    end, [], CapVs).

%%
%% The Extrude command (for edges).
%%

extrude(Type, St0) ->
    Dist = calc_extrude_dist(St0),
    MF = fun(Edges, We) -> extrude_edges_uv(Edges, Dist, We) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp={finish,_,Tv}}) -> Tv; (_, #we{temp=Tv}) -> Tv end,
    wings_move:plus_minus(Type, DF, St).

extrude_edges_uv(Edges, Dist, We0) ->
    #we{temp=PlusMinus}=We = extrude_1(Edges, Dist, We0),
    EdgeVs = gb_sets:from_list(wings_edge:to_vertices(Edges, We0)),
    We#we{temp=uv_temp(PlusMinus, EdgeVs, We0, We)}.

calc_extrude_dist(St) ->
    Map = fun(Edges0, We) ->
		  Edges = gb_sets:to_list(Edges0),
		  min_dist_from_edges(Edges, We)
	  end,
    Combine = fun min/2,
    wings_sel:dfold(Map, Combine, 3.0*?DEFAULT_EXTRUDE_DIST, St) / 3.0.

extrude_1(Edges, ExtrudeDist, We0) ->
    {We1,_,New,Forbidden} = extrude_edges(Edges, ExtrudeDist, We0),
    Ns = orig_normals(Edges, We1),
    We = straighten(Ns, New, We1),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    We#we{temp={Edges,NewVs,Forbidden}}.

orig_normals(Es0, #we{es=Etab,vp=Vtab}) ->
    VsVec0 = gb_sets:fold(
	       fun(E, A) ->
		       #edge{vs=Va,ve=Vb} = array:get(E, Etab),
		       Vec = e3d_vec:norm_sub(array:get(Va, Vtab),
					      array:get(Vb, Vtab)),
		       [{Va,{Vec,Vb}},{Vb,{Vec,Va}}|A]
	       end, [], Es0),
    VsVec1 = sofs:relation(VsVec0, [{vertex,info}]),
    VsVec2 = sofs:relation_to_family(VsVec1),
    VsVec = sofs:to_external(VsVec2),
    orig_normals_1(VsVec, gb_trees:from_orddict(VsVec), []).

orig_normals_1([{V,[{VecA,_},{VecB,_}]}|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
orig_normals_1([{V,[{VecA,OtherV}]}|T], VsVec, Acc) ->
    OtherRec = gb_trees:get(OtherV, VsVec),
    case [Vec || {Vec,Vertex} <- OtherRec, Vertex =/= V] of
	[VecB] ->
	    orig_normals_1(T, VsVec, [{V,e3d_vec:cross(VecA, VecB)}|Acc]);
	_ ->
	    orig_normals_1(T, VsVec, Acc)
    end;
orig_normals_1([_|T], VsVec, Acc) ->
    orig_normals_1(T, VsVec, Acc);
orig_normals_1([], _, Acc) -> reverse(Acc).

straighten([{V,N0}|Ns], New, #we{vp=Vtab0}=We0) ->
    Pos = wings_vertex:pos(V, We0),
    Vtab = wings_vertex:fold(
	     fun(_, _, R, Vt0) ->
		     OtherV = wings_vertex:other(V, R),
		     case gb_sets:is_member(OtherV, New) of
			 false -> Vt0;
			 true ->
			     OPos0 = array:get(OtherV, Vt0),
			     Vec = e3d_vec:norm_sub(Pos, OPos0),
			     case e3d_vec:dot(N0, Vec) of
				 Dot when abs(Dot) < 0.87 ->
				     Vt0;
				 Dot when Dot < 0 ->
				     N = e3d_vec:neg(N0),
				     straighten_1(Vec, N, Pos, OtherV, OPos0, Vt0);
				 _ ->
				     straighten_1(Vec, N0, Pos, OtherV, OPos0, Vt0)
			     end
		     end
	     end, Vtab0, V, We0),
    We = We0#we{vp=Vtab},
    straighten(Ns, New, We);
straighten([], _, We) -> We.

straighten_1(Vec, N, {Cx,Cy,Cz}, OtherV, OPos0, Vt) ->
    try e3d_mat:rotate_s_to_t(Vec, N) of
        Rot ->
	    M0 = e3d_mat:translate(Cx, Cy, Cz),
	    M1 = e3d_mat:mul(M0, Rot),
	    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
	    OPos = e3d_mat:mul_point(M, OPos0),
	    array:set(OtherV, OPos, Vt)
    catch _:_ -> Vt
    end.

%%
%% Common help function for actually extruding edges.
%%
extrude_edges(Edges, ExtrudeDist, We) ->
    NoForbiddenFaces = gb_sets:empty(),
    extrude_edges(Edges, NoForbiddenFaces, ExtrudeDist, We).

extrude_edges(Edges, ForbiddenFaces, ExtrudeDist,
	      #we{next_id=Wid,es=Etab,vc=Vct0}=We0) ->
    G = digraph:new(),

    %% We update the 'vc' table here to handle the case that a
    %% a vertex's incident edge points to a completely unrelated
    %% face (i.e. a vertex is shared by two faces that have no
    %% common edge).
    Vct = gb_sets:fold(
	    fun(Edge, A) ->
		    #edge{vs=Va,ve=Vb} = Rec = array:get(Edge, Etab),
		    digraph_edge(G, ForbiddenFaces, Rec),
		    array:set(Vb, Edge, array:set(Va, Edge, A))
	    end, Vct0, Edges),
    Vs0 = digraph:vertices(G),
    Vs = sofs:to_external(sofs:domain(sofs:relation(Vs0))),
    {We1,Forbidden} =
	foldl(fun(V, A) ->
		      new_vertex(V, G, Edges, ForbiddenFaces, ExtrudeDist, A)
	      end, {We0#we{vc=Vct},[]}, Vs),
    NewVs = wings_we:new_items_as_gbset(vertex, We0, We1),
    We = connect(G, ExtrudeDist, Wid, We1),
    digraph:delete(G),
    {We,Vs,NewVs,gb_sets:from_list(Forbidden)}.

new_vertex(V, G, Edges, ForbiddenFaces, ExtrudeDist, {We0,F0}=Acc) ->
    case wings_vertex:fold(fun(E, F, R, A) -> [{E,F,R}|A] end, [], V, We0) of
	[_,_]=Es ->
	    case filter_edges(Es, Edges, ForbiddenFaces) of
		[] -> Acc;
		[{Edge,_,#edge{lf=Lf,rf=Rf}}] ->
		    New = {new,V},
		    digraph_insert(G, New, V, Lf),
		    digraph_insert(G, V, New, Lf),
		    digraph_insert(G, V, New, Rf),
		    digraph_insert(G, New, V, Rf),
		    {We0,[Edge|F0]}
	    end;
	Es0 ->
	    Es = filter_edges(Es0, Edges, ForbiddenFaces),
	    Center = wings_vertex:pos(V, We0),
	    We = foldl(fun({Edge,_,_}, W0) ->
			       new_vertex_1(V, G, Edge, Center, ExtrudeDist, W0)
		       end, We0, Es),
	    {We,F0}
    end.

new_vertex_1(V, G, Edge, Center, ExtrudeDist, #we{es=Etab}=We0) ->
    OtherPos = wings_vertex:other_pos(V, array:get(Edge, Etab), We0),
    Dir = e3d_vec:norm_sub(OtherPos, Center),
    Pos = e3d_vec:add_prod(Center, Dir, ExtrudeDist),
    {We,NewV=NewE} = wings_edge:fast_cut(Edge, Pos, We0),
    Rec = get_edge_rec(V, NewV, Edge, NewE, We),
    digraph_edge(G, Rec),
    We.

get_edge_rec(Va, Vb, EdgeA, EdgeB, #we{es=Etab}) ->
    case array:get(EdgeA, Etab) of
	#edge{vs=Va,ve=Vb}=Rec -> Rec;
	#edge{vs=Vb,ve=Va}=Rec -> Rec;
	_Other -> array:get(EdgeB, Etab)
    end.

filter_edges(Es, EdgeSet, FaceSet) ->
    foldl(fun({Edge,Face,_}=E, A) ->
		  case gb_sets:is_member(Edge, EdgeSet) orelse
		      gb_sets:is_member(Face, FaceSet) of
		      true -> A;
		      false -> [E|A]
		  end
	  end, [], Es).

digraph_edge(G, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    digraph_insert(G, Va, Vb, Lf),
    digraph_insert(G, Vb, Va, Rf).

digraph_edge(G, ForbiddenFaces, #edge{lf=Lf,rf=Rf,vs=Va,ve=Vb}) ->
    case gb_sets:is_member(Lf, ForbiddenFaces) of
	false -> digraph_insert(G, Va, Vb, Lf);
	true -> ok
    end,
    case gb_sets:is_member(Rf, ForbiddenFaces) of
	false -> digraph_insert(G, Vb, Va, Rf);
	true -> ok
    end.

digraph_insert(G, Va0, Vb0, Face) ->
    Va = {Va0,Face},
    Vb = {Vb0,Face},
    digraph:add_vertex(G, Va),
    digraph:add_vertex(G, Vb),
    digraph:add_edge(G, Va, Vb).

connect(G, ExtrudeDist, Wid, We) ->
    Cs = digraph_utils:components(G),
    connect(G, Cs, ExtrudeDist, Wid, We, []).

connect(G, [C|Cs], ExtrudeDist, Wid, #we{mirror=Mirror}=We0, Closed) ->
    case [VF || {V,_}=VF <- C, V >= Wid] of
	[] ->
	    case C of
		[{_,Mirror}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, Closed);
		[{_,Face}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, [Face|Closed])
	    end;
	[_] ->
	    extrude_problem();
	[Va0,Vb0] ->
	    case digraph_get_path(G, Va0, Vb0) of
		[{_,Mirror}|_] ->
		    connect(G, Cs, ExtrudeDist, Wid, We0, Closed);
		[{Va,Face}|Path0] ->
		    Path = [V || {V,_} <- Path0],
		    N = wings_face:normal(Face, We0),
		    We = connect_inner(Va, Path, N, Face, ExtrudeDist, We0),
		    connect(G, Cs, ExtrudeDist, Wid, We, Closed)
	    end
    end;
connect(_, [], ExtrudeDist, _, We0, Closed) ->
    We = wings_extrude_face:faces(Closed, We0),
    move_vertices(Closed, ExtrudeDist, We).

digraph_get_path(G, Va, Vb) ->
    case digraph:get_path(G, Va, Vb) of
	false -> digraph:get_path(G, Vb, Va);
	Path -> Path
    end.

connect_inner({new,Va}, [Va,Vb,{new,Vb}], N, Face, ExtrudeDist, We0) ->
    [{EdgeThrough,_,_}] = wings_vertex:edge_through(Va, Vb, We0),
    {We1,TempE} = wings_edge:fast_cut(EdgeThrough, default, We0),
    {We2,Edge} = wings_vertex:force_connect(Vb, Va, Face, We1),
    #we{vp=Vtab} = We2,
    APos = array:get(Va, Vtab),
    BPos = array:get(Vb, Vtab),
    Vec = e3d_vec:sub(APos, BPos),
    Pos1 = e3d_vec:add_prod(BPos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We3,NewE} = wings_edge:fast_cut(Edge, Pos1, We2),
    Pos2 = e3d_vec:add_prod(APos, e3d_vec:cross(Vec, N), ExtrudeDist),
    We4 = wings_edge:dissolve_edge(TempE, We3),
    {We,_} = wings_edge:fast_cut(NewE, Pos2, We4),
    wings_we_util:validate(We),
    We;
connect_inner({new,V}, [V|[B,C,_|_]=Next], N, Face, ExtrudeDist, We0) ->
    {We1,Current} = connect_one_inner(V, V, B, C, N, Face, ExtrudeDist, We0),
    #we{vp=Vtab} = We2 = connect_inner(Current, Next, N, Face, ExtrudeDist, We1),
    Edge = wings_vertex:fold(
	     fun(E, _, R, A) ->
		     case wings_vertex:other(V, R) of
			 Current -> E;
			 _ -> A
		     end
	     end, none, V, We2),
    VPos = array:get(V, Vtab),
    BPos = array:get(B, Vtab),
    Vec = e3d_vec:sub(VPos, BPos),
    Pos = e3d_vec:add_prod(VPos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We2),
    We;
connect_inner({new,_}, [A|[B,C]], _, Face, _, We0) ->
    {We1,Edge} = wings_vertex:force_connect(C, A, Face, We0),
    #we{vp=Vtab} = We1,
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    CPos = array:get(C, Vtab),
    Pos = e3d_vec:add(APos, e3d_vec:sub(CPos, BPos)),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
connect_inner(C, [B|[A,{new,_}]], N, Face, ExtrudeDist, We0) ->
    {We1,Edge} = wings_vertex:force_connect(A, C, Face, We0),
    #we{vp=Vtab} = We1,
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    Vec = e3d_vec:sub(BPos, APos),
    Pos = e3d_vec:add_prod(APos, e3d_vec:cross(Vec, N), ExtrudeDist),
    {We,_} = wings_edge:fast_cut(Edge, Pos, We1),
    We;
connect_inner(Current0, [A|[B,C,_|_]=Next], N, Face, ExtrudeDist, We0) ->
    {We,Current} = connect_one_inner(Current0, A, B, C, N, Face, ExtrudeDist, We0),
    connect_inner(Current, Next, N, Face, ExtrudeDist, We);
connect_inner(Current, [_|[_,_]=Next], N, Face, ExtrudeDist, We) ->
    connect_inner(Current, Next, N, Face, ExtrudeDist, We);
connect_inner(Current, [_,Last], _, Face, _, We0) ->
    {We,_} = wings_vertex:force_connect(Last, Current, Face, We0),
    We.

connect_one_inner(Current, A, B, C, N, Face, ExtrudeDist, We0) ->
    {We1,Edge} = wings_vertex:force_connect(B, Current, Face, We0),
    #we{vp=Vtab} = We1,
    Pos = new_vertex_pos(A, B, C, N, ExtrudeDist, Vtab),
    wings_edge:fast_cut(Edge, Pos, We1).

move_vertices([Face|Fs], ExtrudeDist, #we{vp=Vtab0}=We0) ->
    N = wings_face:normal(Face, We0),
    Vs = wings_face:vertices_ccw(Face, We0),
    Vtab = move_vertices(Vs, Vs, N, ExtrudeDist, Vtab0, Vtab0),
    We = We0#we{vp=Vtab},
    move_vertices(Fs, ExtrudeDist, We);
move_vertices([], _, We) -> We.

move_vertices([Va|[Vb,Vc|_]=Vs], First, N, ExtrudeDist, OldVtab, Vtab0) ->
    Pos = new_vertex_pos(Va, Vb, Vc, N, ExtrudeDist, OldVtab),
    Vtab = array:set(Vb, wings_util:share(Pos), Vtab0),
    move_vertices(Vs, First, N, ExtrudeDist, OldVtab, Vtab);
move_vertices([Va,Vb], [Vc,Vd|_], N, ExtrudeDist, OldVtab, Vtab) ->
    move_vertices([Va,Vb,Vc,Vd], [], N, ExtrudeDist, OldVtab, Vtab);
move_vertices([_,_], [], _, _, _, Vtab) -> Vtab.

new_vertex_pos(A, B, C, N, ExtrudeDist, Vtab) ->
    APos = array:get(A, Vtab),
    BPos = array:get(B, Vtab),
    CPos = array:get(C, Vtab),
    VecA0 = e3d_vec:norm_sub(APos, BPos),
    VecB0 = e3d_vec:norm_sub(BPos, CPos),
    VecA = e3d_vec:norm(e3d_vec:cross(VecA0, N)),
    VecB = e3d_vec:norm(e3d_vec:cross(VecB0, N)),
    Vec = average(VecA, VecB),
    e3d_vec:add_prod(BPos, Vec, ExtrudeDist).

average(Na, Nb) ->
    N = e3d_vec:norm(e3d_vec:add(Na, Nb)),
    case e3d_vec:dot(N, Na) of
	Dot when abs(Dot) < 1.0E-6 ->
	    e3d_vec:add(Na, Nb);
	Dot ->
	    e3d_vec:divide(N, Dot)
    end.

scale_tv(Tv, ExtrudeDist) ->
    S = 1.0 / ExtrudeDist,
    scale_tv_1(Tv, S, []).

scale_tv_1([{Vec,Vs}|T], S, Acc) ->
    scale_tv_1(T, S, [{e3d_vec:mul(Vec, S),Vs}|Acc]);
scale_tv_1([], _, Acc) -> Acc.

-spec min_dist_from_edges(Edges, #we{}) -> float() when
      Edges :: nonempty_list(edge_num()).

min_dist_from_edges(Edges0, We) ->
    Vs = wings_vertex:from_edges(Edges0, We),
    Edges = wings_edge:from_vs(Vs, We),
    min_dist_from_edges_1(Edges, We, infinite).

min_dist_from_edges_1([E|Es], #we{es=Etab,vp=Vtab}=We, D0) ->
    #edge{vs=Va,ve=Vb} = array:get(E, Etab),
    D = e3d_vec:dist(array:get(Va, Vtab), array:get(Vb, Vtab)),
    min_dist_from_edges_1(Es, We, min(D0, D));
min_dist_from_edges_1([], _, D) -> D.
