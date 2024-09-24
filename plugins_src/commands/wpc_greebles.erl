%%
%%  wpc_greebles.erl --
%%
%%     Plug-in for face greebles -> inset and move
%%
%%  Copyright (c) 2020 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(wpc_greebles).

-include_lib("wings/src/wings.hrl").

-export([init/0,menu/2,command/2]).

%%%
%%% plugin interface
%%%

init() -> true.

menu({face},Menu) ->
    add_menu(Menu);
menu(_,Menu) ->
    Menu.

add_menu(Menu) ->
    {M1, M2} = lists:splitwith(fun({_, {lift,_}}) -> false; (_) -> true end, Menu),
    {M3, M4} = lists:splitwith(fun(separator) -> false; (_) -> true end, M2),
    Entry = {?__(1,"Greebles"),greeble, ?__(2,"Create random extrusions"), [option]},
    M1 ++ M3 ++ [Entry|M4].

info(s_subdiv) ->
    ?__(1, "After splitting a face move edge in the same direction");
info(recurse) ->
    ?__(2, "Number of times the operation should be repeated");
info(limit) ->
    ?__(3, "Random number of face to keep operating on");
info(limit_mod) ->
    ?__(4, "After each repetition multiply this value with the above");
info(d_inset) ->
    ?__(5, "Inset before extrude on all repetitions");
info(d_inset_first) ->
    ?__(6, "Inset before extrude on first operation");
info(inset) ->
    ?__(7, "Percentage of face area decrease after each operation");
info(inset_var_f) ->
    ?__(8, "Randomly modify the size of the area");
info(inset_var_v) ->
    ?__(9, "Randomly modify the vertex moves");
info(height) ->
    ?__(10, "Length to be extruded");
info(height_m) ->
    ?__(11, "Multiply this value with the height each repetition");
info(bump_var_f) ->
    ?__(12, "Randomly modify height per face");
info(bump_var_v) ->
    ?__(13, "Randomly modify height per vertex").

command({face, {greeble, Ask}}, St) when is_atom(Ask) ->
    Recurse = 3,
    Limit = 75, LimitMod = 100,
    Dinset = false, DinsetFirst = true,
    Ssubdiv = true,
    Inset = 10, InsetVarianceF = true, InsetVarianceV = false,
    Height = 0.1, HeightMod = 1.0,
    HeightVarianceF = true, HeightVarianceV = false,
    Seed = rand:export_seed_s(rand:seed(exsp)),
    Qs = [{value, Seed, [{key,seed}]},
          {?__(21, "Split face straight"), Ssubdiv,
           [{key, s_subdiv}, {info, info(s_subdiv)}]},
          {hframe,
	   [{label, ?__(25,"Repeat")},
            {slider, {text, Recurse, [{key,recurse},{range,{1,20}},{info, info(recurse)}]}}
           ]},
          {label_column,
	   [{?__(30,"Random Selected %"),
             [{slider, {text, Limit, [{key,limit},{range,{1,100}}, {info, info(limit)}]}}]},
            {?__(31,"Modify Selected %"),
             [{slider, {text, LimitMod,
                        [{key,limit_mod}, {range,{1,100}},{info, info(limit_mod)}]}}]}
           ], [{title, ?__(32,"Limits")}]},
          {vframe,
	   [
            {hframe,
             [{?__(20, "Inset all"), Dinset, [{key, d_inset}, {info, info(d_inset)}]},
              {?__(22, "Inset first only"), DinsetFirst,
               [{key, d_inset_first}, {info, info(d_inset_first)}]}
             ]},
            {slider, {text, Inset, [{key,inset}, {range,{1,100}}, {info, info(inset)}]}},
            {?__(41,"Random scale per face"), InsetVarianceF,
             [{key,inset_var_f}, {info, info(inset_var_f)}]},
            {?__(42,"Random scale per vertex"), InsetVarianceV,
             [{key,inset_var_v}, {info, info(inset_var_v)}]}
           ],
           [{title, ?__(43,"Inset Modifiers")}]},
          {vframe,
	   [{label_column,
             [{?__(50, "Standard Height"), [{text, Height, [{key,height}, {info, info(height)}]}]},
              {?__(51, "Height Modifier"),
               [{text, HeightMod,[{key,height_m}, {range, {0.01,infinity}}, {info, info(height_m)}]}]}
             ]},
            {?__(52,"Random height per face"), HeightVarianceF,
             [{key,bump_var_f}, {info, info(bump_var_f)}]},
            {?__(53,"Random height per vertex"), HeightVarianceV,
             [{key,bump_var_v}, {info, info(bump_var_v)}]}],
           [{title, ?__(55,"Height Modifiers")}]}
         ],
    wings_dialog:dialog_preview({face,greeble}, Ask,
                                ?__(1,"Greeble Options"),
                                Qs, St);
command({face, {greeble, Opts0}}, St) ->
    %% ?dbg("Greeble: ~p~n", [Opts0]),
    Opts1 = maps:from_list(Opts0),
    #{limit:=Limit, seed:=Seed, inset:=Inset} = Opts1,
    State = Opts1#{first => true, limit := Limit/100, inset := Inset/100},
    rand:seed(Seed),
    Greeble = fun(Fs,We) -> greeble(Fs, State#{orig_sel=>Fs, orig_we=>We}, We) end,
    wings_sel:map_update_sel(Greeble, St);
command(_C, _) ->
    next.

greeble(Fs0, #{recurse:=Recurse} = State, We0) when Recurse > 0 ->
    {Subdiv,Fs1} = subdiv(Fs0,State,We0),
    Fs = sel_random(Fs1, State),
    Extrude = wings_extrude_face:faces(Fs, Subdiv),
    Inset = case double_extrude(State) of
                false ->
                    inset(Fs, true, true, State, Extrude);
                true ->
                    Inset0 = inset(Fs, true, false, State, Extrude),
                    Extrude1 = wings_extrude_face:faces(Fs, Inset0),
                    inset(Fs, false, true, State, Extrude1)
            end,
    greeble(Fs, update_state(State), Inset);
greeble(_, #{orig_sel:=Fs0, orig_we:=Orig}, We) ->
    Fs = gb_sets:union(Fs0, wings_we:new_items_as_gbset(face,Orig,We)),
    {We, Fs}.


double_extrude(#{d_inset:=true}) -> true;
double_extrude(#{first:=true, d_inset_first:=true}) -> true;
double_extrude(_) -> false.

update_state(State) ->
    State#{first=>false,
           recurse:=maps:get(recurse,State)-1,
           limit:=maps:get(limit, State)*maps:get(limit_mod, State)/100.0,
           height:=maps:get(height, State)*maps:get(height_m, State)
          }.

%% Inset code

inset(Fs, InsetP, BumpP, State, #we{vp=Vp0}=We) ->
    #{inset_var_f := IVF, inset_var_v := IVV, inset := DefInset,
      bump_var_f := BVF, bump_var_v := BVV, height := DefHeight} = State,
    {_,{_, FacesData}} = wpc_inset:collect_inset_face_data(Fs, We),
    Vp = lists:foldl(fun({FNorm,DistF,VData},Vp1) ->
                             InsetDist0 = inset_dist(DefInset, IVF, InsetP),
                             BumpDist0  = bump_dist(DefHeight, BVF, BumpP),
                             Move = fun({Vs,Data},Vp) ->
                                            InsetDist = inset_dist(InsetDist0, IVV, InsetP),
                                            Vpos0 = inset_faces(DistF,Data,InsetDist),
                                            BumpDist = bump_dist(BumpDist0, BVV, BumpP),
                                            Vpos = bump(Vpos0,FNorm,BumpDist),
                                            array:set(Vs, Vpos, Vp)
                                    end,
                             lists:foldl(Move,Vp1,VData)
                     end, Vp0, FacesData),
    We#we{vp=Vp}.

inset_faces(_,{Vpos,_},+0.0) -> Vpos;
inset_faces(SFDist,{Vpos,Dir},Percent) ->
    e3d_vec:add(Vpos, e3d_vec:mul(Dir, SFDist * Percent)).

bump(Vpos,_,+0.0) -> Vpos;
bump(Vpos,FNorm,Bump)->
    e3d_vec:add(Vpos, e3d_vec:mul(FNorm,Bump)).

sel_random(Fs, #{limit:=Percent}) ->
    Filter = fun(_Item) -> rand:uniform() < Percent end,
    gb_sets:filter(Filter, Fs).

inset_dist(_, _, false) ->
    0.0;
inset_dist(InsetPct, true, _) ->
    clamp(0.01, InsetPct+InsetPct*rand:normal()/5.0, 0.99);
inset_dist(InsetPct, _, _) ->
    _ = rand:uniform(), %% Keep rand in sync
    InsetPct.

bump_dist(_, _, false) ->
    0.0;
bump_dist(Height, true, _) ->
    clamp(Height-Height*0.1, Height+Height*rand:normal()/7.5, Height+Height*0.1);
bump_dist(Height, _, _) ->
    _ = rand:uniform(), %% Keep rand in sync
    Height.

clamp(Min, Value, Max) ->
    max(Min, min(Max, Value)).

%%  Subdiv

subdiv(Fs, #{first:=true}, We) ->
    {We, Fs};
subdiv(Fs0, State, We0) ->
    Fs1 = sel_random(Fs0, State),
    Subdiv = fun(Face, We1) -> simple_subdiv(Face, State, We1) end,
    {NewFs, We} = lists:mapfoldl(Subdiv, We0, gb_sets:to_list(Fs1)),
    {We, gb_sets:union(Fs1, gb_sets:from_list(NewFs))}.

simple_subdiv(Face, State, #we{vp=VPos}=We0) ->
    GetEdges = fun(_, Edge, #edge{vs=VS,ve=VE}, Acc) ->
                       Pt1 = array:get(VS,VPos),
                       Pt2 = array:get(VE,VPos),
                       [{e3d_vec:dist_sqr(Pt1,Pt2), Edge}|Acc]
               end,
    Edges = wings_face:fold(GetEdges, [], Face, We0),
    {E1, E2} = select_pair(Edges),
    Move = 0.5 + clamp(-2.5, rand:normal(), 2.5) / 10.0,
    {We1, V1, V2} = cut(E1, E2, Move, State, We0),
    {We, NewFace} = wings_vertex:force_connect(V1,V2,Face,We1),
    {NewFace, We}.

cut(E1, E2, Move, #{s_subdiv:=KeepStraight}, #we{es=Etab, vp=Vtab}=We0) ->
    #edge{vs=Vs1,ve=Ve1} = array:get(E1, Etab),
    #edge{vs=Vs2,ve=Ve2} = array:get(E2, Etab),
    VsPos1 = array:get(Vs1,Vtab),
    VePos1 = array:get(Ve1,Vtab),
    VsPos2 = array:get(Vs2,Vtab),
    VePos2 = array:get(Ve2,Vtab),
    Vec1 = e3d_vec:sub(VePos1, VsPos1),
    Vec2 = e3d_vec:sub(VePos2, VsPos2),
    Pos1 = e3d_vec:add(VsPos1, e3d_vec:mul(Vec1, Move)),
    Pos2 = case KeepStraight of
               true ->
                   _ = rand:normal(), %% Keep rand in sync
                   case e3d_vec:dot(Vec1,Vec2) > 1 of
                       true -> e3d_vec:add(VsPos2, e3d_vec:mul(Vec2, Move));
                       false -> e3d_vec:add(VsPos2, e3d_vec:mul(Vec2, 1.0 - Move))
                   end;
               false ->
                   Move2 = 0.5 + clamp(-2.5, rand:normal(), 2.5) / 10.0,
                   e3d_vec:add(VsPos2, e3d_vec:mul(Vec2, Move2))
           end,
    {We1, V1} = wings_edge:fast_cut(E1, Pos1, We0),
    {We,  V2} = wings_edge:fast_cut(E2, Pos2, We1),
    {We, V1, V2}.

select_pair(Edges) ->
    Pairs = make_pairs(Edges),
    case lists:reverse(lists:sort(Pairs)) of
        [{_,E1,E2}] -> {E1,E2};
        [{L1,E11,E12},{L2,E21,E22}|_] ->
            case (L1 - L2)/L1 < 0.05 of
                true ->
                    case rand:uniform() =< 0.5 of
                        true ->  {E11,E12};
                        false -> {E21,E22}
                    end;
                false ->
                    _ = rand:uniform(), %% Keep rand in sync
                    {E11,E12}
            end
    end.

make_pairs(Edges) ->
    N = length(Edges),
    {L1, L2} = lists:split(N div 2, Edges),
    make_pairs(L1,L2,[]).

make_pairs([{L1,E1}|Ls1],[{L2,E2}|Ls2],Acc) ->
    make_pairs(Ls1, Ls2, [{L1+L2,E1,E2}|Acc]);
make_pairs(_,_,Acc) ->
    Acc.

