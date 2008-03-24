%%
%%  wpc_sweep_extrude.erl --
%%
%%    Plugin for making angled extrusions/regions/extractions that can be
%%    scaled and twisted interactively.
%%
%%  Copyright (c) 2008 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_sweep_extrude).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.

menu({face},[A|B]) ->
    [A,separator,
     sweep_menu(sweep_regular),
     sweep_menu(sweep_region),
     sweep_menu(sweep_extract),B];
menu(_,Menu) ->
    Menu.

%%%% Menus
sweep_menu(Type) ->
    MenuTitle = menu_title(Type),
    case wings_pref:get_value(advanced_menus) of
      false ->
        {MenuTitle,{sweep_extrude,xyz(Type)}};
      true ->
        F = fun(help, _Ns) ->
          Str1 = menu_string_1(Type),
          Str3 = ?__(2,"Pick axis"),
          {Str1,[],Str3};
          (1, _Ns) -> xyz(Type);
          (2, _Ns) -> ignore;
          (3, _Ns) -> Ask = [plane],
                      {face,{Type,{'ASK',Ask}}}
        end,
        {MenuTitle,{sweep_extrude,F}}
    end.
menu_title(sweep_regular) -> ?__(1,"Sweep");
menu_title(sweep_region) ->  ?__(2,"Sweep Region");
menu_title(sweep_extract) -> ?__(3,"Sweep Extract").

menu_string_1(sweep_regular) ->
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
    case wings_pref:get_value(advanced_menus) of
      false ->
        F = fun(1, _Ns) ->
          {face,{Type,Axis}}
        end,
        {AxisStr,F,Help};
      true ->
        F = {face,{Type,Axis}},
        {AxisStr,{Axis,F},Help}
    end.

axis_menu_string(free) ->
    ?__(1,"Sweep freely relative to the screen");
axis_menu_string(normal) ->
    ?__(2,"Extrude along normal with no side to side motion.");
axis_menu_string(Axis) ->
    AxisStr = wings_s:dir(Axis),
    Str = ?__(3,"If the ~s axis is perpendicular to the extrusion normal, all movement will be constrained to its radial plane. Otherwise, it acts as an off axis component."),
    wings_util:format(Str,[AxisStr]).

command({face,{sweep_regular,{'ASK',Ask}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_extrude(Axis,St0)
    end);
command({face,{sweep_regular,Axis}},St) ->
    sweep_extrude(Axis,St);

command({face,{sweep_region,{'ASK',Ask}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_region(Axis,St0)
    end);
command({face,{sweep_region,Axis}},St) ->
    sweep_region(Axis,St);

command({face,{sweep_extract,{'ASK',Ask}}},St) ->
    wings:ask(selection_ask(Ask), St, fun (Axis,St0) ->
        sweep_extract(Axis,St0)
    end);
command({face,{sweep_extract,Axis}},St) ->
    sweep_extract(Axis,St);

command(_,_) -> next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.
selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([plane|Rest],Ask) ->
    Desc = ?__(1,"Choosing an axis perpendicular to the extrusion normal, will constrain all movement to its radial plane. Otherwise, it acts as an off axis component."),
    selection_ask(Rest,[{axis,Desc}|Ask]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrude %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sweep_extrude(Axis,St) ->                                                     %%
    sweep_setup(Axis,extrude_faces(St)).                                      %%
                                                                              %%
extrude_faces(St) ->                                                          %%
    wings_sel:map(fun(Faces, We) ->                                           %%
        wings_extrude_face:faces(Faces, We)                                   %%
    end, St).                                                                 %%
                                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extrrude Region (from wings_face_cmd.erl) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sweep_region(Axis, St0) ->                                                    %%
    St = wings_sel:map(fun extrude_region_0/2, St0),                          %%
    sweep_setup(Axis, St).                                                    %%
                                                                              %%
extrude_region_0(Faces0, We0) ->                                              %%
    %% We KNOW that a gb_set with fewer elements sorts before                 %%
    %% a gb_set with more elements.                                           %%
    Rs = lists:sort(wings_sel:face_regions(Faces0, We0)),                     %%
    We = extrude_region_1(Rs, We0, []),                                       %%
    extrude_region_vmirror(We0, We).                                          %%
                                                                              %%
extrude_region_1([Faces0|Rs0]=Rs, We0, Acc) ->                                %%
    case gb_sets:size(Faces0) of                                              %%
    1 ->                                                                      %%
        [Face] = gb_sets:to_list(Faces0),                                     %%
        extrude_region_1(Rs0, We0, [Face|Acc]);                               %%
    _Other ->                                                                 %%
        We = wings_extrude_face:faces(Acc, We0),                              %%
        wings_extrude_face:regions(Rs, We)                                    %%
    end;                                                                      %%
extrude_region_1([], We, Faces) ->                                            %%
    wings_extrude_face:faces(Faces, We).                                      %%
                                                                              %%
extrude_region_vmirror(_, #we{mirror=none}=We) -> We;                         %%
extrude_region_vmirror(OldWe, #we{mirror=Face0}=We0) ->                       %%
  %% Merge the mirror face and any newly created faces to one new mirror face %%
  %% and flatten it.                                                          %%
    FaceSet = gb_sets:singleton(Face0),                                       %%
    Bordering = wings_face:extend_border(FaceSet, We0),                       %%
    NewFaces = wings_we:new_items_as_gbset(face, OldWe, We0),                 %%
    Dissolve0 = gb_sets:intersection(Bordering, NewFaces),                    %%
    case gb_sets:is_empty(Dissolve0) of                                       %%
    true -> We0;                                                              %%
    false ->                                                                  %%
        Dissolve = gb_sets:insert(Face0, Dissolve0),                          %%
        We1 = wings_dissolve:faces(Dissolve, We0),                            %%
        [Face] = NewFace = wings_we:new_items_as_ordset(face, We0, We1),      %%
        We = wings_facemat:assign('_hole_', NewFace, We1),                    %%
        wings_we:mirror_flatten(OldWe, We#we{mirror=Face})                    %%
    end.                                                                      %%
                                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Extract (from wings_face_cmd.erl) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sweep_extract(Axis, St0) ->                                                   %%
    St1 = wings_sel:fold(                                                     %%
        fun(Faces, We0, #st{sel=Sel0,onext=Oid}=S0) ->                        %%
            We = wings_dissolve:complement(Faces, We0),                       %%
            S = wings_shape:insert(We, extract, S0),                          %%
            Sel = [{Oid,Faces}|Sel0],                                         %%
            S#st{sel=Sel}                                                     %%
        end, St0#st{sel=[]}, St0),                                            %%
    Sel = St1#st.sel,                                                         %%
    St = wings_sel:set(Sel, St1),                                             %%
    sweep_region(Axis, St).                                                   %%
                                                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Setup
sweep_setup(Axis,ExSt) ->
    Warp = wings_pref:get_value(sweep_mode,unwarped),
    Cntr = wings_pref:get_value(sweep_center,region),
    State = {unlocked,Axis,Warp,Cntr},
    ExVs0 = wings_sel:fold(fun(Fs,We,Acc) ->
            [wings_face:to_vertices(Fs,We)|Acc]
            end,[],ExSt),
    ExVs = lists:merge(ExVs0),
    St = wings_sel_conv:more(ExSt),
    SelCntr = wings_sel:center(St),
    Tvs = wings_sel:fold(fun(Fs, We, Acc) ->
            AllVs = wings_face:to_vertices(Fs,We),
            Data = face_region(Fs,We,Axis,SelCntr,AllVs,State),
            sweep_data_setup(AllVs,ExVs,We,Data,State,Acc)
            end, [], St),
    Units = [angle,distance,percent,angle],
    Flags = [{mode,{modes(),State}}|flag(Axis)],
    wings_drag:setup(Tvs, Units, Flags, ExSt).

%%%% More Setup: Get face region normals
face_region(Fs,We,Axis,SelCntr,AllVs,State) ->
    face_region(wings_sel:face_regions(Fs,We),We,Axis,SelCntr,AllVs,State,[]).

face_region([Fs0|Regions], We, Axis0, SelCntr0, AllVs, State, NormAcc0) ->
    Fs = gb_sets:to_list(Fs0),
    NormsForRegion = lists:foldl(fun(Face,Acc) ->
            Norm = wings_face:normal(Face,We),
            [Norm|Acc]
            end,[],Fs),
    AverageNorm = e3d_vec:add(NormsForRegion),
    RegNorm = e3d_vec:norm(AverageNorm),
    RegVs = wings_face:to_vertices(Fs,We),
    RegCntr0 = wings_vertex:center(RegVs,We),
    RegCntr = lowest_point_relative_to_norm(RegCntr0,RegNorm,RegVs,We),
    Axis = e3d_vec:norm(axis_conversion(Axis0,RegNorm)),
    Norm = get_norm_data(Axis,RegNorm),
    SelCntr = lowest_point_relative_to_norm(SelCntr0,RegNorm,AllVs,We),
    {Warp,Center} = specify_warp_and_center(Axis,Norm,RegCntr,SelCntr,State),
    NormAcc = [{RegVs,Axis,RegNorm,Norm,RegCntr,SelCntr,{Warp,Center}}|NormAcc0],
    face_region(Regions, We, Axis0, SelCntr0, AllVs, State, NormAcc);
face_region([], _, _, _, _, _, Acc) -> Acc.

%%%% Finish Setup
sweep_data_setup(AllVs,ExVs,#we{id=Id}=We,Data,State,Acc) ->
    VsPos = wings_util:add_vpos(AllVs,We),
    [{Id,{AllVs,sweep_fun(VsPos,ExVs,Data,State)}}|Acc].

%%%% Setup Utilities
lowest_point_relative_to_norm(C,Norm,Vs,We) ->
    {Nx,Ny,Nz} = e3d_vec:neg(Norm),
    case {Nx,Ny,Nz} of
      {0.0,0.0,0.0} -> sweep_error();
      _other ->
        {Cx,Cy,Cz} = C,
        DistList = lists:foldl(fun(V,Acc) ->
            {Vx,Vy,Vz} = wings_vertex:pos(V,We),
            [(Nx*(Cx-Vx)+Ny*(Cy-Vy)+Nz*(Cz-Vz))|Acc]
            end,[],Vs),
        Lowest = lists:min(DistList),
        e3d_vec:add(C, e3d_vec:mul(Norm, Lowest))
    end.

get_norm_data(Axis0,Norm) ->
    Axis1 = e3d_vec:cross(Norm,Axis0),
    Axis = e3d_vec:cross(Axis1,Norm),
    e3d_vec:norm(Axis).

%%%% Change data depending on the current Warp mode
specify_warp_and_center(Axis,_Norm,_RegCntr,SelCntr,{_lock,_mode,warped,common}) ->
    {Axis,SelCntr};
specify_warp_and_center(_Axis,Norm,_RegCntr,SelCntr,{_lock,_mode,unwarped,common}) ->
    {Norm,SelCntr};
specify_warp_and_center(Axis,_Norm,RegCntr,_SelCntr,{_lock,_mode,warped,region}) ->
    {Axis,RegCntr};
specify_warp_and_center(_Axis,Norm,RegCntr,_SelCntr,{_lock,_mode,unwarped,region}) ->
    {Norm,RegCntr}.

%%%% Flags
flag(free) -> [screen_relative,keep_drag];  %% <- keep_drag keeps the drag data
flag(_xyz) -> [].                           %%    from reseting on view_changed

%%%% Modes changed by number keys
modes() ->
    fun(help, State) -> sweep_help(State);
      ({key,$1},{_lock,_axis,_warp,region})   -> {_lock,_axis,_warp,common};
      ({key,$1},{_lock,_axis,_warp,common})   -> {_lock,_axis,_warp,region};

      ({key,$2},{_lock,_axis,unwarped,_cntr}) -> {_lock,_axis,warped,_cntr};
      ({key,$2},{_lock,_axis,warped,_cntr})   -> {_lock,_axis,unwarped,_cntr};

      ({key,$3},{unlocked,_axis,_warp,_cntr}) -> {locked,_axis,_warp,_cntr};
      ({key,$3},{locked,_axis,_warp,_cntr})   -> {unlocked,_axis,_warp,_cntr};

      (done,{_lock,_axis,Warp,Cntr}) -> wings_pref:set_value(sweep_mode,Warp),
                                        wings_pref:set_value(sweep_center,Cntr);
      (_,_) -> none
    end.

%%%% Mode help
sweep_help({Lock,Axis,Warp,Cntr}) ->
    [cntr_help(Cntr),
     warp_help(Axis,Warp),
     lock_help(Lock,Axis)].

cntr_help(region) -> ?__(1,"[1] Selection Center");
cntr_help(common) -> ?__(2,"[1] Region Center").

warp_help(normal,_) -> [];
warp_help(_,warped) -> ?__(1,"  [2] Maintain Shape");
warp_help(_,unwarped) -> ?__(2,"  [2] Allow Warping").

lock_help(unlocked,free) -> ?__(1,"  [3] Lock Axis");
lock_help(locked,free) -> ?__(2,"  [3] Screen Relative");
lock_help(_,_) -> [].

%%%% Sweep Mode/View Changes
sweep_fun(VsPos,ExVs,Data,State) ->
    fun(view_changed,_) ->  %% when view changes
        {Lock,_mode,_warp,_center} = State,
        case Lock of
          unlocked ->
            NewAxis = e3d_vec:norm(view_vector()),
            NewData = lists:foldl(fun({RegVs,_Axis,RegNorm,_Norm,RegCntr,SelCntr,{_Warp,_Center}},Acc) ->
                NewNorm = get_norm_data(NewAxis,RegNorm),
                {Warp,Center} = specify_warp_and_center(NewAxis,NewNorm,RegCntr,SelCntr,State),
                [{RegVs,NewAxis,RegNorm,NewNorm,RegCntr,SelCntr,{Warp,Center}}|Acc]
                end,[],Data),
            sweep_fun(VsPos,ExVs,NewData,State);
          locked ->
            sweep_fun(VsPos,ExVs,Data,State)
        end;

       (new_mode_data,{NewState,_}) ->  %% when mode changes
        {_lock0,_mode0,Warp0,Center0} = State,
        {_lock1,_mode1,Warp1,Center1} = NewState,
        case {Warp0,Center0} =:= {Warp1,Center1} of
          true ->
            sweep_fun(VsPos,ExVs,Data,NewState);
          false ->
            NewData = lists:foldl(fun({RegVs,Axis,RegNorm,Norm,RegCntr,SelCntr,_},Acc) ->
                {Warp,Center} = specify_warp_and_center(Axis,Norm,RegCntr,SelCntr,NewState),
                [{RegVs,Axis,RegNorm,Norm,RegCntr,SelCntr,{Warp,Center}}|Acc]
                end,[],Data),
            sweep_fun(VsPos,ExVs,NewData,NewState)
        end;

       ([Angle,Dist,Scale,Rotate|_], A) ->  %% when drag changes
         sweep_verts2(VsPos,ExVs,Data,{Angle,Dist,Rotate,Scale},A)
    end.

sweep_verts2(VsPos,ExVs,Data,DragData,A) ->
    lists:foldl(fun({V,Vpos}, VsAcc) ->
        [{V,sweep(V,Vpos,ExVs,Data,DragData)}|VsAcc]
    end, A, VsPos).

%%%% Main functions
sweep(_V,Vpos,_ExVs,_Data,{0.0,0.0,0.0,0.0}) ->
    Vpos;

sweep(V,Vpos,ExVs,Data,DistData) ->
    lists:foldl(fun({RegVs,_,RegNorm,Norm,_,_,{Warp,Center}},Acc) ->
            case {lists:member(V,RegVs),lists:member(V,ExVs)} of
              {true,true} ->
                extruded_face(Vpos,RegNorm,Norm,Warp,Center,DistData);
              {true,false} ->
                seed_face(Vpos,RegNorm,Norm,Warp,Center,DistData);
              {_,_} -> Acc
            end
    end,[],Data).

extruded_face(Vpos,RegNorm,Norm,Warp,Center,{Angle,Dist,0.0,0.0}) ->
    out_and_side_to_side(Vpos,RegNorm,Norm,Warp,Center,Angle,Dist);

extruded_face(Vpos,RegNorm,Norm,Warp,Center,{Angle,Dist,0.0,Scale}) ->
    ScPos = scale_extruded_section(Vpos,Center,Scale),
    out_and_side_to_side(ScPos,RegNorm,Norm,Warp,Center,Angle,Dist);

extruded_face(Vpos,RegNorm,Norm,Warp,Center,{Angle,Dist,Rotate,0.0}) ->
    RotatePos = rotate(Vpos,RegNorm,Center,Rotate),
    out_and_side_to_side(RotatePos,RegNorm,Norm,Warp,Center,Angle,Dist);

extruded_face(Vpos,RegNorm,Norm,Warp,Center,{Angle,Dist,Rotate,Scale}) ->
    RotatePos = rotate(Vpos,RegNorm,Center,Rotate),
    ScPos = scale_extruded_section(RotatePos,Center,Scale),
    out_and_side_to_side(ScPos,RegNorm,Norm,Warp,Center,Angle,Dist).

out_and_side_to_side(Vpos,RegNorm,Norm,Warp,Center,Angle,Dist) ->
    ExPos = e3d_vec:add(Vpos,e3d_vec:mul(RegNorm, Dist)),
    Deg = e3d_vec:degrees(Norm,RegNorm),
    case ((Deg == 0.0) or (Deg == 180.0)) of
      true ->
        ExPos;
      false ->
        rotate(ExPos,Warp,Center,Angle*2)
    end.

scale_extruded_section(Vpos,Center,Scale) ->
    ScaleVec0 =  e3d_vec:sub(Vpos,Center),
    ScaleVec = e3d_vec:norm(ScaleVec0),
    DistCntr = e3d_vec:dist(Vpos,Center),
    e3d_vec:add(Vpos, e3d_vec:mul(ScaleVec, Scale*DistCntr)).

rotate(Vpos,Norm,{Cx,Cy,Cz},Angle) ->
    A0 = e3d_mat:translate(Cx,Cy,Cz),
    A1 = e3d_mat:mul(A0, e3d_mat:rotate(Angle, Norm)),
    A2 = e3d_mat:mul(A1, e3d_mat:translate(-Cx,-Cy,-Cz)),
    e3d_mat:mul_point(A2,Vpos).

seed_face(Vpos,RegNorm,Norm,Warp,Center,{Angle,_Dist,_Rotate,_Scale}) ->
    Deg = e3d_vec:degrees(Norm,RegNorm),
    case ((Deg == 0.0) or (Deg == 180.0)) of
      true ->
        Vpos;
      false ->
        OrigVs = rotate(Vpos,Warp,Center,Angle),
        D = intersect_vec_plane(Center,Vpos,Warp),
        Pn0 = e3d_vec:sub(D,Vpos),
        Ln0 = e3d_vec:sub(OrigVs,D),
        Pn1 = e3d_vec:norm(Pn0),
        Ln1 = e3d_vec:norm(Ln0),
        Dp1 = e3d_vec:dot(Ln1,Pn1),
        case Dp1 of
          0.0 -> Vpos;
          _ -> Int1 = e3d_vec:dot(e3d_vec:sub(Vpos,OrigVs),Pn1)/Dp1,
               e3d_vec:add(OrigVs, e3d_vec:mul(Ln1, Int1))
        end
    end.

%%%%  Helper functions
axis_conversion(Axis,Norm) ->
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      free -> view_vector();
      normal -> Norm;
      last_axis -> {_, Dir} = wings_pref:get_value(last_axis),
                   Dir;
      default_axis -> {_, Dir} = wings_pref:get_value(default_axis),
                      Dir;
      {_,_,_} -> Axis
    end.

intersect_vec_plane(PosA,PosB,PlaneNorm) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

sweep_error() ->
    wings_u:error(?__(1,"The average normal for a region cannot be null")).

view_vector() ->
    #view{azimuth=Az,elevation=El} = wings_view:current(),
    M0 = e3d_mat:rotate(-Az, {0.0,1.0,0.0}),
    M = e3d_mat:mul(M0, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    e3d_mat:mul_point(M, {0.0,0.0,1.0}).
