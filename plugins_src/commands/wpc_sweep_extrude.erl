%%
%%  wpc_sweep_extrude.erl --
%%
%%    Plugin for making angled extrusions/regions/extractions that can be
%%    scaled and twisted interactively.
%%
%%  Copyright (c) 2008-2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_sweep_extrude).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    wings_pref:delete_value(sweep_mode),
    wings_pref:delete_value(sweep_center),
    true.
menu({face},Menu) ->
    lists:reverse(parse(Menu, [], false));

menu(_,Menu) ->
    Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [sweep_menu_headings()|NewMenu];
parse([S,A={_,{flatten,_}},S|Rest], NewMenu, false) when S==separator ->
    parse(Rest, [S,A,S,sweep_menu_headings()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

sweep_menu_headings() ->
    [{menu_title(sweep_extrude),
    {sweep,
      [sweep_menu(sweep_extrude),
       sweep_menu(sweep_region),
       sweep_menu(sweep_extract)]
    }}].

%%%% Menus
sweep_menu(Type) ->
    MenuTitle = menu_title(Type),
    F = fun(help, _Ns) ->
		Str1 = menu_string_1(Type),
		Str2 = ?__(4,"Pick axis and extrude relative to percentage of selection's radius"),
		Str3 = ?__(3,"Pick axis"),
		{Str1,Str2,Str3};
	   (1, _Ns) -> xyz(Type);
	   (2, _Ns) -> {face,{Type,{relative,{'ASK',[plane]}}}};
	   (3, _Ns) -> {face,{Type,{absolute,{'ASK',[plane]}}}}
        end,
    {MenuTitle,{sweep_extrude,F}}.

menu_title(sweep_extrude) -> ?__(1,"Sweep");
menu_title(sweep_region) ->  ?__(2,"Sweep Region");
menu_title(sweep_extract) -> ?__(3,"Sweep Extract").

menu_string_1(sweep_extrude) ->
    ?__(1,"Extrude along normal, using standard side to side axis");
menu_string_1(sweep_region) ->
    ?__(2,"Extrude region along its normal, using standard side to side axis");
menu_string_1(sweep_extract) ->
    ?__(3,"Extract and extrude region along its normal, using standard side to side axis").

xyz(Type) ->
    [axis_menu(Type,normal),
     axis_menu(Type,free),
     axis_menu(Type,x),
     axis_menu(Type,y),
     axis_menu(Type,z),
     separator,
     axis_menu(Type,last_axis),
     axis_menu(Type,default_axis)].

axis_menu(Type,Axis) ->
    AxisStr = wings_util:cap(wings_s:dir(Axis)),
    Help = axis_menu_string(Axis),
    F = fun (help, _Ns) ->
		Str3 = ?__(2,"Extrude relative to percentage of selection's radius"),
		{Help,[],Str3};
	    (1, _Ns) -> {face,{Type,{absolute,Axis}}};
	    (3, _Ns) -> {face,{Type,{relative,Axis}}};
	    (_,_) -> ignore
        end,
    {AxisStr,{Axis,F},Help}.

axis_menu_string(free) ->
    ?__(1,"Sweep freely relative to the screen");
axis_menu_string(normal) ->
    ?__(2,"Extrude along normal with no side to side motion.");
axis_menu_string(Axis) ->
    AxisStr = wings_s:dir(Axis),
    Str = ?__(3,"If the ~s axis is perpendicular to the extrusion normal, all movement will be constrained to its radial plane. Otherwise, it acts as an off axis component."),
    wings_util:format(Str,[AxisStr]).

%%%% Commands
command({face,{sweep_extrude,{Type,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_extrude(Type, Axis, St0)
    end);
command({face,{sweep_extrude,{Type, Axis}}},St) ->
    sweep_extrude(Type, Axis, St);

command({face,{sweep_region,{Type,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_region(Type, Axis, St0)
    end);
command({face,{sweep_region,{Type, Axis}}},St) ->
    sweep_region(Type, Axis, St);

command({face,{sweep_extract,{Type,{'ASK',Ask}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_extract(Type, Axis, St0)
    end);
command({face,{sweep_extract,{Type,Axis}}},St) ->
    sweep_extract(Type, Axis, St);

command(_,_) -> next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Choosing an axis perpendicular to the extrusion normal, will constrain all movement to its radial plane. Otherwise, it acts as an off axis component."),
    selection_ask(Rest,[{axis,Desc}|Ask]).

%%%% Start by Extruding Selection
sweep_extrude(Type, Axis, St0) ->
    St = wings_face_cmd:extrude_faces(St0),
    sweep_setup(Type, Axis, St).

sweep_region(Type, Axis, St0) ->
    St = wings_face_cmd:extrude_region(St0),
    sweep_setup(Type, Axis, St).

sweep_extract(Type, Axis, St0) ->
    St = wings_face_cmd:extract_region(St0),
    sweep_region(Type, Axis, St).

%%%% Setup
sweep_setup(Type,Axis,St) ->
    Prefs = wings_pref:get_value(sweep,{unlocked,unwarped,region,free_base}),
    {Lock,Warp,Center,Base} = Prefs,
    State = {Lock,Axis,Warp,Center,Base},

    SelC = wings_sel:center(St),

    Tvs = wings_sel:fold(fun(Fs, #we{id=Id}=We, Acc) ->

            Regions = wings_sel:face_regions(Fs,We),
            {AllVs,VsData} = collect_data(Type, Regions, We, Axis, SelC, [], State, [], []),
            [{Id, {AllVs, sweep_fun(Type, VsData, State)}} | Acc]
         end, [], St),
    Units = units(Type),
    Flags = [{mode,{modes(),State}}|flag(Axis)],
    wings_drag:setup(Tvs, Units, Flags, St).

units(absolute) -> [distance,skip,angle,percent,angle];
units(relative) -> [percent,skip,angle,percent,angle].


%% LoopNorm is the extrude direction
collect_data(Type, [Fs0|Rs], #we{mirror=M}=We, Axis0, SelC0, AllVs0, State, LVAcc0, ExData) ->
    Fs = gb_sets:delete_any(M, wings_face:extend_border(Fs0, We)),
    {OuterEs, RegVs} =  reg_data_0(Fs, We, [], []),
    {LoopNorm, LoopVs0} = loop_data_0(OuterEs, Fs, We),

    LoopVs = case Type of
      sweep_extrude -> LoopVs0;
      _otherwise when M =/= none ->
          MirEs = wings_face:to_edges([M],We),
          LoopEs = OuterEs -- MirEs,
          LoopVerts = wings_edge:to_vertices(LoopEs,We),
          LoopVerts;
      _otherwise -> LoopVs0
    end,
    Axis = axis_conversion(Axis0,LoopNorm),

    ExVs = ordsets:subtract(RegVs, LoopVs),
    AllVs = ordsets:union(RegVs ,AllVs0),

    LoopVs1 = [ V || V <- LoopVs , not ordsets:is_element(V,LVAcc0) ],

    SeedVpos = add_vpos_data(seed,LoopVs1,We,[]),
    AllVpos = add_vpos_data(extrude,ExVs,We,SeedVpos),

    LVAcc = ordsets:union(LoopVs,LVAcc0),
    LoopCenter = wings_vertex:center(LoopVs, We),
    {MaxR, LoopC, SelC} = lowest_point_rel_to_norm(LoopVs, LoopNorm, LoopCenter, SelC0, We),
    NW = non_warping_norm(Axis, LoopNorm),
    CN = specify_warp_and_center(Axis, NW, LoopC, SelC, State),
    Data = {{SelC, LoopC, LoopNorm, MaxR, NW, Axis}, CN},
    collect_data(Type, Rs, We, Axis0, SelC0, AllVs, State, LVAcc, [{Data,AllVpos}|ExData]);

collect_data(_Type, [], _We, _Axis0, _SelC0, AllVs, _State, _LVs, VsData) ->
    {AllVs, VsData}.


reg_data_0(Faces0, #we{es=Etab,fs=Ftab}=We, EAcc0, Vs0) ->
    case gb_sets:is_empty(Faces0) of
      false ->
        {Face, Faces} = gb_sets:take_smallest(Faces0),
        Edge = gb_trees:get(Face, Ftab),
        {EAcc, Vs} = reg_data_1(Edge, Face, Etab, EAcc0, Vs0),
        reg_data_0(Faces, We, EAcc, Vs);
      true ->
        OuterEdges = outer_edges_1(lists:sort(EAcc0),[]),
        {OuterEdges, ordsets:from_list(Vs0)}
    end.
reg_data_1(Edge,Face,Etab,EAcc,Vs) ->
    case array:get(Edge,Etab) of
      #edge{ve=Vb,lf=Face,ltpr=NextEdge} ->
        reg_data_2(NextEdge,Face,Edge,Etab,[Edge|EAcc],[Vb|Vs]);
      #edge{vs=Va,rf=Face,rtpr=NextEdge} ->
        reg_data_2(NextEdge,Face,Edge,Etab,[Edge|EAcc],[Va|Vs])
    end.

reg_data_2(LastEdge,_,LastEdge,_,EAcc,Vs) ->
    {EAcc,Vs};
reg_data_2(Edge,Face,LastEdge,Etab,EAcc,Vs) ->
    case array:get(Edge,Etab) of
      #edge{ve=V,lf=Face,ltpr=NextEdge} ->
        reg_data_2(NextEdge,Face,LastEdge,Etab,[Edge|EAcc],[V|Vs]);
      #edge{vs=V,rf=Face,rtpr=NextEdge} ->
        reg_data_2(NextEdge,Face,LastEdge,Etab,[Edge|EAcc],[V|Vs])
    end.

loop_data_0([], _, _) ->
    sweep_error(); % "Wholly selected objects cannot be Swept"
loop_data_0(OuterEs, Fs, #we{es=Etab, vp=Vtab, mirror=M}) ->
    EdgeSet = gb_sets:from_list(OuterEs),
    loop_data_1(EdgeSet, Fs, Etab, Vtab, M, [], []).

loop_data_1(Es0, Fs, Etab, Vtab, M, LNorms, Vs0) ->
    case gb_sets:is_empty(Es0) of
      false ->
        {Edge, Es1} = gb_sets:take_smallest(Es0),
        {Es, LoopNorm, Links, Vs} = loop_data_2(Edge, Edge, Es1, Fs, Etab, Vtab, M, Vs0),
        loop_data_1(Es, Fs, Etab, Vtab, M, [{Links,e3d_vec:neg(LoopNorm)}|LNorms], Vs);
      true ->
        AvgLoopNorm = e3d_vec:neg(average_loop_norm(LNorms)),
        {AvgLoopNorm, ordsets:from_list(Vs0)}
    end.

loop_data_2(Edge, Edge, Es, Fs, Etab, Vtab, M, Vs) ->
    E = array:get(Edge, Etab),
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,ltsu=NextLeft,rtsu=NextRight} = E,
    case gb_sets:is_member(Rf,Fs) of
      false ->
        VpA = array:get(Va,Vtab),
        EData = array:get(NextLeft,Etab),
        loop_data_3(NextLeft, EData, Edge, Es, Fs, Lf, Va, VpA, Etab, Vtab, M, [], Vs, 0);
      true ->
        VpB = array:get(Vb,Vtab),
        EData = array:get(NextRight,Etab),
        loop_data_3(NextRight, EData, Edge, Es, Fs, Rf, Vb, VpB, Etab, Vtab, M, [], Vs, 0)
    end.


loop_data_3(LastE,#edge{ve=Vb,lf=PrevF},
        LastE, Es, _Fs, PrevF, Vb, VpB, _Etab, _Vtab, M, VPs, Vs0, Links) ->
    case M == PrevF of
      false ->
        LoopNorm = e3d_vec:normal([VpB|VPs]),
        Vs = [Vb|Vs0],
        {Es, LoopNorm, Links+1, Vs};
      true ->
        LoopNorm = e3d_vec:normal(VPs),
        {Es, LoopNorm, Links, Vs0}
    end;

loop_data_3(LastE,#edge{vs=Va,rf=PrevF},
        LastE, Es, _Fs, PrevF, Va, VpA, _Etab, _Vtab, M, VPs, Vs0, Links) ->
    case M == PrevF of
      false ->
        LoopNorm = e3d_vec:normal([VpA|VPs]),
        Vs = [Va|Vs0],
        {Es, LoopNorm, Links+1, Vs};
      true ->
        LoopNorm = e3d_vec:normal(VPs),
        {Es, LoopNorm, Links, Vs0}
    end;

loop_data_3(CurE,#edge{vs=Va,ve=Vb,lf=PrevF,rf=Face,rtsu=NextEdge,ltsu=IfCurIsMember},
        LastE, Es0, Fs, PrevF, Vb, VpB, Etab, Vtab, M, VPs0, Vs0, Links) ->
    case gb_sets:is_member(CurE,Es0) of
      true ->
        EData = array:get(IfCurIsMember,Etab),
        Es = gb_sets:delete(CurE,Es0),
        VpA = array:get(Va,Vtab),
        case M == PrevF of
          false ->
            VPs = [VpB|VPs0],
            Vs = [Vb|Vs0],
            loop_data_3(IfCurIsMember,EData,LastE,Es,Fs,PrevF,Va,VpA,Etab,Vtab,M,VPs,Vs,Links+1);
          true ->
            loop_data_3(IfCurIsMember,EData,LastE,Es,Fs,Face,Va,VpA,Etab,Vtab,M,VPs0,Vs0,Links)
        end;
      false ->
        EData = array:get(NextEdge,Etab),
        loop_data_3(NextEdge, EData, LastE, Es0, Fs, Face, Vb, VpB, Etab, Vtab, M, VPs0, Vs0, Links)
    end;

loop_data_3(CurE,#edge{vs=Va,ve=Vb,lf=Face,rf=PrevF,ltsu=NextEdge,rtsu=IfCurIsMember},
        LastE, Es0, Fs, PrevF, Va, VpA, Etab, Vtab, M, VPs0, Vs0, Links) ->
    case gb_sets:is_member(CurE,Es0) of
      true ->
        EData = array:get(IfCurIsMember,Etab),
        Es = gb_sets:delete(CurE,Es0),
        VpB = array:get(Vb,Vtab),
        case M == PrevF of
          false ->
            VPs = [VpA|VPs0],
            Vs = [Va|Vs0],
            loop_data_3(IfCurIsMember,EData,LastE,Es,Fs,PrevF,Vb,VpB,Etab,Vtab,M,VPs,Vs,Links+1);
          true ->
            loop_data_3(IfCurIsMember,EData,LastE,Es,Fs,Face,Vb,VpB,Etab,Vtab,M,VPs0,Vs0,Links)
        end;
      false ->
        EData = array:get(NextEdge,Etab),
        loop_data_3(NextEdge, EData, LastE, Es0, Fs, Face, Va, VpA, Etab, Vtab, M, VPs0, Vs0, Links)
    end.


%%%% Setup Utilities
add_vpos_data(Type, Vs, #we{vp=Vtab}, Acc) ->
    lists:foldl(fun(V, A) ->
          [{V, Type, array:get(V, Vtab)} | A]
      end, Acc, Vs).

non_warping_norm(Axis0,Norm) ->
    Axis1 = e3d_vec:cross(Norm,Axis0),
    Axis = e3d_vec:cross(Axis1,Norm),
    e3d_vec:norm(Axis).

outer_edges_1([E,E|T],Out) ->
    outer_edges_1(T,Out);
outer_edges_1([E|T],Out) ->
    outer_edges_1(T,[E|Out]);
outer_edges_1([],Out) -> Out.

average_loop_norm([{_,LNorms}]) ->
    e3d_vec:norm(LNorms);
average_loop_norm([{LinksA,LNormA},{LinksB,LNormB}]) ->
    case LinksA < LinksB of
      true -> e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormA),LNormB));
      false -> e3d_vec:norm(e3d_vec:add(e3d_vec:neg(LNormB),LNormA))
    end;
average_loop_norm(LNorms) ->
    LoopNorms = [Norm||{_,Norm}<-LNorms],
    e3d_vec:norm(e3d_vec:neg(e3d_vec:add(LoopNorms))).

sqr_length({X,Y,Z}) ->
    X*X+Y*Y+Z*Z.

lowest_point_rel_to_norm(LoopVs, LoopNorm, LoopC, SelC, #we{vp=Vtab}) ->
    {L,LD,SD} = lists:foldl(fun
      (V, none) ->
          VPos = array:get(V,Vtab),
          LVec = e3d_vec:sub(VPos, LoopC),
          SVec = e3d_vec:sub(VPos, SelC),
          Len = sqr_length(LVec),
          LDot = e3d_vec:dot(LoopNorm, LVec),
          SDot = e3d_vec:dot(LoopNorm, SVec),
          {Len,LDot,SDot};
      (V, {Len0,LDot0,SDot0}) ->
          VPos = array:get(V,Vtab),
          LVec = e3d_vec:sub(VPos, LoopC),
          SVec = e3d_vec:sub(VPos, SelC),
          Len1 = sqr_length(LVec),
          LDot1 = e3d_vec:dot(LoopNorm, LVec),
          SDot1 = e3d_vec:dot(LoopNorm, SVec),
          LDot = case LDot1 > LDot0 of
            true -> LDot1;
            false -> LDot0
          end,
          SDot = case SDot1 > SDot0 of
            true -> SDot1;
            false -> SDot0
          end,
          Len = case Len1 > Len0 of
            true -> Len1;
            false -> Len0
          end,
          {Len,LDot,SDot}
    end,none,LoopVs),
    LoopCenter = e3d_vec:add_prod(LoopC,LoopNorm,LD),
    SelCenter = e3d_vec:add_prod(SelC,LoopNorm,SD),

    MaxRadius = math:sqrt(L),
    {MaxRadius, LoopCenter, SelCenter}.

%%%% Change data depending on the current Warp mode
specify_warp_and_center(Axis,_NW,_RegCntr,SelCntr,{_lock,_mode,warped,common,_base}) ->
    {Axis,SelCntr};
specify_warp_and_center(_Axis,NW,_RegCntr,SelCntr,{_lock,_mode,unwarped,common,_base}) ->
    {NW,SelCntr};
specify_warp_and_center(Axis,_NW,RegCntr,_SelCntr,{_lock,_mode,warped,region,_base}) ->
    {Axis,RegCntr};
specify_warp_and_center(_Axis,NW,RegCntr,_SelCntr,{_lock,_mode,unwarped,region,_base}) ->
    {NW,RegCntr}.

%%%% Flags
flag(free) -> [screen_relative, keep_drag]; %% <- keep_drag keeps the drag data
flag(_xyz) -> [].                           %%    from reseting on view_changed

%%%% Modes changed by number keys
modes() ->
    fun(help, State) -> sweep_help(State);
      ({key,$1},{_lock,_axis,_warp,region,_base})   -> {_lock,_axis,_warp,common,_base};
      ({key,$1},{_lock,_axis,_warp,common,_base})   -> {_lock,_axis,_warp,region,_base};

      ({key,$2},{_lock,_axis,unwarped,_cntr,_base}) -> {_lock,_axis,warped,_cntr,_base};
      ({key,$2},{_lock,_axis,warped,_cntr,_base})   -> {_lock,_axis,unwarped,_cntr,_base};

      ({key,$3},{unlocked,_axis,_warp,_cntr,_base}) -> {locked,_axis,_warp,_cntr,_base};
      ({key,$3},{locked,_axis,_warp,_cntr,_base})   -> {unlocked,_axis,_warp,_cntr,_base};

      ({key,$4},{_lock,_axis,_warp,_cntr,free_base})   -> {_lock,_axis,_warp,_cntr,freeze_base};
      ({key,$4},{_lock,_axis,_warp,_cntr,freeze_base}) -> {_lock,_axis,_warp,_cntr,free_base};

      (done,{Lock,_axis,Warp,Cntr,Base}) -> wings_pref:set_value(sweep,{Lock,Warp,Cntr,Base});
      (_,_) -> none
    end.

%%%% Mode help
sweep_help({Lock,Axis,Warp,Cntr,Base}) ->
    [cntr_help(Cntr),
     warp_help(Axis,Warp),
     lock_help(Lock,Axis),
     base_help(Axis,Base)].

cntr_help(region)         -> ?__(1,"[1] Selection Center");
cntr_help(common)         -> ?__(2,"[1] Region Center").

warp_help(normal,_)       -> [];
warp_help(_,warped)       -> ?__(1,"  [2] Maintain Shape");
warp_help(_,unwarped)     -> ?__(2,"  [2] Allow Warping").

lock_help(unlocked,free)  -> ?__(1,"  [3] Lock Axis");
lock_help(locked,free)    -> ?__(2,"  [3] Screen Relative");
lock_help(_,_)            -> [].

base_help(normal,_) -> [];
base_help(_,free_base)      -> ?__(1,"  [4] Freeze Base");
base_help(_,freeze_base)    -> ?__(2,"  [4] Thaw Base").

%%%% Sweep Mode/View Changes
sweep_fun(Type, VsData, State) ->
    {Lock,_mode,_warp,_center,Base} = State,
    fun(view_changed,_) ->  %% when view changes
        case Lock of
          unlocked ->
            NewAxis = e3d_vec:norm(view_vector()),
            NewData = lists:foldl(fun({{{SelC, LoopC, LoopNorm, MaxR, _NW, _Axis}, _},VPs}, Acc) ->
                NewNW = non_warping_norm(NewAxis,LoopNorm),
                CN = specify_warp_and_center(NewAxis,NewNW,LoopC,SelC, State),
                [{{{SelC, LoopC, LoopNorm, MaxR, NewNW, NewAxis}, CN},VPs}|Acc]
            end,[],VsData),
            sweep_fun(Type,NewData,State);
          locked ->
            sweep_fun(Type,VsData,State)
        end;

       (new_mode_data,{{_,_,W,C,_}=NewState,_}) ->
           NewData = case element(3,State)==W andalso element(4,State)==C of
             false ->
               lists:foldl(fun({{{SelC, LoopC, LoopNorm, MaxR, NW, Axis}, _},VPs}, Acc) ->
                  CN = specify_warp_and_center(Axis,NW,LoopC,SelC, NewState),
                  [{{{SelC, LoopC, LoopNorm, MaxR, NW, Axis}, CN},VPs}|Acc]
                end,[],VsData);
             true ->
               VsData
           end,
           sweep_fun(Type,NewData,NewState);

       ([Dist,_,Angle,Scale,Rotate|_], A) ->  %% when drag changes
         sweep(Type, Base, VsData, {-Angle,Dist,Rotate,Scale}, A)
    end.

sweep(Type, Base, VsData, DragData, A) ->
    lists:foldl(fun({Data, VPs},Acc0) ->
            lists:foldl(fun
                ({V, extrude, Vpos}, Acc) ->
                    [{V, extruded_face(Type, Vpos, Data, DragData)}|Acc];
                ({V, seed, Vpos}, Acc) ->
                    [{V, seed_face(Base, Vpos, Data, DragData)}|Acc]
            end,Acc0, VPs)
    end,A,VsData).

%%%% Main functions
extruded_face(_,Vpos,_,{0.0,0.0,0.0,0.0}) ->
    Vpos;

extruded_face(Type, Vpos,VData,{Angle,Dist,0.0,0.0}) ->
    out_and_side_to_side(Type, Vpos,VData,Angle,Dist);

extruded_face(Type, Vpos, {_,{_N,C}}=VData, {Angle,Dist,0.0,Scale}) ->
    ScPos = scale_extruded_section(Vpos,C,Scale),
    out_and_side_to_side(Type, ScPos, VData,Angle,Dist);

extruded_face(Type, Vpos, {{_,_,LNorm,_,_,_},{_,C}}=VData, {Angle,Dist,Rotate,0.0}) ->
    RPos = rotate(Vpos,LNorm,C,Rotate),
    out_and_side_to_side(Type, RPos,VData,Angle,Dist);

extruded_face(Type, Vpos,{{_,_,LNorm,_,_,_},{_,C}}=VData, {Angle,Dist,Rotate,Scale}) ->
    RPos = rotate(Vpos,LNorm,C,Rotate),
    ScPos = scale_extruded_section(RPos,C,Scale),
    out_and_side_to_side(Type, ScPos, VData, Angle, Dist).

out_and_side_to_side(absolute, Vpos,{{_,_,LoopNorm,_,_,Axis},{Norm,Center}},Angle,Dist) ->
    ExPos = e3d_vec:add(Vpos, e3d_vec:mul(LoopNorm, Dist)),
    Dot = e3d_vec:dot(Axis,LoopNorm),
    case Dot < (1 - 1.0E-12) of
      true ->
        rotate(ExPos,Norm,Center,Angle*2);
      false ->
        ExPos
    end;

out_and_side_to_side(relative, Vpos,{{_,_,LoopNorm,MaxR,_,Axis},{Norm,Center}},Angle,Percent) ->
    ExPos = e3d_vec:add(Vpos, e3d_vec:mul(LoopNorm, Percent*MaxR)),
    Dot = e3d_vec:dot(Axis,LoopNorm),
    case Dot < (1 - 1.0E-12) of
      true ->
        rotate(ExPos,Norm,Center,Angle*2);
      false ->
        ExPos
    end.

scale_extruded_section(Vpos,Center,Scale) ->
    ScaleVec0 =  e3d_vec:sub(Vpos,Center),
    ScaleVec = e3d_vec:norm(ScaleVec0),
    DistCntr = e3d_vec:len(ScaleVec0),
    e3d_vec:add(Vpos, e3d_vec:mul(ScaleVec, Scale*DistCntr)).

seed_face(_,Vpos,_,{0.0,_Dist,_Rotate,_Scale}) ->
    Vpos;
seed_face(freeze_base,Vpos,_,_) ->
    Vpos;
seed_face(_,Vpos,{{_,_,LoopNorm,_,_,Axis},{Norm,Center}},{Angle,_Dist,_Rotate,_Scale}) ->
     Dot = e3d_vec:dot(Axis,LoopNorm),
    case Dot < (1 - 1.0E-12) of
      true ->
        Vp = rotate(Vpos,Norm,Center,Angle),
        D = intersect_vec_plane(Center,Vpos,Norm),
        Pn0 = e3d_vec:sub(D,Vpos),
        Ln0 = e3d_vec:norm_sub(Vp,D),
        Dp = e3d_vec:dot(Ln0,Pn0),
        case Dp of
          0.0 -> Vpos;
          _ -> Int = e3d_vec:dot(e3d_vec:sub(Vpos,Vp),Pn0)/Dp,
               e3d_vec:add(Vp, e3d_vec:mul(Ln0, Int))
        end;
      false ->
        Vpos
    end.

%%%%  Helper functions
axis_conversion(Axis,Norm) ->
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      free -> view_vector();
      normal -> Norm;
      last_axis ->
          {_, Dir} = wings_pref:get_value(last_axis),
          Dir;
      default_axis ->
          {_, Dir} = wings_pref:get_value(default_axis),
          Dir;
      {_,_,_} -> Axis
    end.

intersect_vec_plane(PosA,PosB,PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm),
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

rotate(Vpos,Norm,{Cx,Cy,Cz},Angle) ->
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Norm)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).

sweep_error() ->
    wings_u:error(?__(2,"Sweep Region won't work for wholly selected objects")).

view_vector() ->
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul_point(M, {0.0,0.0,-1.0}).
