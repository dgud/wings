%%
%%  wpc_sel_absolute.erl --
%%
%%     Select by absolute coordinates, bounding box, axis
%%
%%  Copyright (c) 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_sel_absolute).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-export([init/0,menu/2,command/2]).

init() -> true.

menu({select,by},Menu) ->
    sel_menu(Menu);
menu({select,by,bb},Menu) ->
    Menu ++ sel_menu_bb();
menu({select,by,absolute},Menu) ->
    Menu ++ sel_menu_absolute();
menu(_,Menu) ->
    Menu.

command({select,{by,{bb,sel_bbox_inside}}}, St) ->
    sel_bbox(St, inside);
command({select,{by,{bb,sel_bbox_outside}}}, St) ->
    sel_bbox(St, outside);
command({select,{by,{absolute,sel_absolute_box}}}, St) ->
    sel_absolute_box(St);
command({select,{by,{absolute,sel_absolute_cyl}}}, St) ->
    sel_absolute_cyl(St);
command({select,{by,{absolute,sel_absolute_sphere}}}, St) ->
    sel_absolute_sphere(St);
command({select,{by,radial_range}}, St) ->
    radial_range(St);
command(_, _) ->
    next.

sel_menu(Menu) ->
    sel_submenu_absolute(sel_submenu_bb(Menu)) ++ [
        {?__(1,"Axis..."),radial_range,
         ?__(2,"Select by normals pointing towards axis or radial axis")}
    ].

sel_submenu_bb([]) ->
    [{?__(1,"Bounding Box"),{bb,[]}}];
sel_submenu_bb([{_Str,{bb,_List}}|_]=Menu) ->
    Menu;
sel_submenu_bb([Item|Menu]) ->
    [Item|sel_submenu_bb(Menu)].

sel_submenu_absolute([]) ->
    [{?__(1,"Absolute Coordinates"),{absolute,[]}}];
sel_submenu_absolute([{_Str,{absolute,_List}}|_]=Menu) ->
    Menu;
sel_submenu_absolute([Item|Menu]) ->
    [Item|sel_submenu_absolute(Menu)].

sel_menu_bb() ->
    [
        {?__(1,"Inside"),sel_bbox_inside,
         ?__(2,"Select by inside bounding box")},
        {?__(3,"Outside"),sel_bbox_outside,
         ?__(4,"Select by outside bounding box")}
    ].

sel_menu_absolute() ->
    [
        {?__(1,"Box..."),sel_absolute_box,
         ?__(2,"Select by absolute coordinates (Box)")},
        {?__(3,"Cylinder..."),sel_absolute_cyl,
         ?__(4,"Select by absolute coordinates (Cylinder)")},
        {?__(5,"Sphere..."),sel_absolute_sphere,
         ?__(6,"Select by absolute coordinates (Sphere)")}
    ].


%%
%% Select By Bounding Box
%%

sel_bbox(#st{bb=none}=St, _BBSel) ->
    St;
sel_bbox(#st{selmode=Mode,bb=[Point1,Point2]}=St, BBSel) ->
    {save_state, sel_bbox_1({Point1,Point2},Mode,BBSel,St)}.

sel_bbox_1(BV, vertex, BBSel, St) ->
    sel_bbox_vertex(BV, BBSel, St);
sel_bbox_1(BV, Mode, BBSel, St0)
  when Mode =:= face; Mode =:= edge; Mode =:= body ->
    St1=wings_sel_conv:mode(vertex, St0),
    St2=sel_bbox_vertex(BV, BBSel, St1),
    wings_sel_conv:mode(Mode, St2);
sel_bbox_1(BV, _, BBSel, St) ->
    St1 = wings_sel:set(face, [], St),
    sel_bbox_1(BV, face, BBSel, St1).

sel_vertex_fun(inside, F, Volume) ->
    fun (Vs0, #we{vp=Vtab}=_We) ->
        gb_sets:fold(fun (V, Acc) ->
            case F(array:get(V, Vtab), Volume) of
                true ->
                    Acc;
                false ->
                    gb_sets:delete(V, Acc)
            end
        end, Vs0, Vs0)
    end;
sel_vertex_fun(outside, F, Volume) ->
    fun (Vs0, #we{vp=Vtab}=_We) ->
        gb_sets:fold(fun (V, Acc) ->
            case F(array:get(V, Vtab), Volume) of
                true ->
                    gb_sets:delete(V, Acc);
                false ->
                    Acc
            end
        end, Vs0, Vs0)
    end.

sel_bbox_vertex_test(Pos, BB) ->
    e3d_bv:inside(Pos, BB).

sel_bbox_vertex(BV, BBSel, #st{sel=[]}=St) ->
    wings_sel:new_sel(sel_vertex_fun(BBSel, fun sel_bbox_vertex_test/2, BV), vertex, St);
sel_bbox_vertex(BV, BBSel, St) ->
    wings_sel:update_sel(sel_vertex_fun(BBSel, fun sel_bbox_vertex_test/2, BV), St).


%%
%% Select By Absolute Coordinates (Box)
%%

sel_absolute_box(St) ->
    Width = wpa:pref_get(?MODULE, width, 2.0),
    Height = wpa:pref_get(?MODULE, height, 2.0),
    Depth = wpa:pref_get(?MODULE, depth, 2.0),
    Frame = [{vframe,[
        frame_select(),
        {hframe, [
            frame_xyz(),
            {label_column,[
                {?__(2,"Width:"),{text,Width,[{key,width}]}},
                {?__(3,"Height:"),{text,Height,[{key,height}]}},
                {?__(4,"Depth:"),{text,Depth,[{key,depth}]}}
            ],[]}
        ]},
        frame_yref(),
        frame_scale()
        ]}],
    wings_dialog:dialog(?__(1,"Select By Absolute Coordinates (Box)"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,sel_absolute_box_1(Args, St)};
            (cancel) ->
                St;
            (Args) ->
                set_pref(Args),
                {commit,St,sel_absolute_box_1(Args, St)}
        end).

sel_absolute_box_1(Args, #st{selmode=Mode}=St) ->
    Select=proplists:get_value(select, Args, inside),
    Scale=abs(proplists:get_value(scale, Args, 1.0)),
    X=proplists:get_value(x, Args, 0.0)*Scale,
    Y_0=proplists:get_value(y, Args, 0.0)*Scale,
    Z=proplists:get_value(z, Args, 0.0)*Scale,
    Width=abs(proplists:get_value(width, Args, 2.0)*Scale),
    Height=abs(proplists:get_value(height, Args, 2.0)*Scale),
    Depth=abs(proplists:get_value(depth, Args, 2.0)*Scale),
    Y=case proplists:get_value(yref, Args, center) of
        center -> Y_0;
        ground -> Y_0+(Height*0.5)
    end,
    X1=X-(Width*0.5),
    X2=X+(Width*0.5),
    Y1=Y-(Height*0.5),
    Y2=Y+(Height*0.5),
    Z1=Z-(Depth*0.5),
    Z2=Z+(Depth*0.5),
    BB=e3d_bv:box({X1,Y1,Z1},{X2,Y2,Z2}),
    sel_absolute_box_2(BB, Mode, Select, St).
sel_absolute_box_2(BV, vertex, BBSel, St) ->
    sel_bbox_vertex(BV, BBSel, St);
sel_absolute_box_2(BV, Mode, BBSel, St0)
  when Mode =:= face; Mode =:= edge; Mode =:= body ->
    St1=wings_sel_conv:mode(vertex, St0),
    St2=sel_bbox_vertex(BV, BBSel, St1),
    wings_sel_conv:mode(Mode, St2);
sel_absolute_box_2(BV, _, BBSel, St) ->
    St1 = wings_sel:set(face, [], St),
    sel_absolute_box_2(BV, face, BBSel, St1).

%%
%% Select By Absolute Coordinates (Cylinder)
%%

sel_absolute_cyl(St) ->
    Radius = wpa:pref_get(?MODULE, radius, 1.0),
    Height = wpa:pref_get(?MODULE, height, 2.0),
    Axis = wpa:pref_get(?MODULE, cyl_axis, y),
    Frame = [{vframe,[
        frame_select(),
        {hframe,[
            frame_xyz(),
            {label_column,[
                {?__(2,"Radius:"),{text,Radius,[{key,radius}]}},
                {?__(3,"Height:"),{text,Height,[{key,height}]}},
                {?__(4,"Axis:"), {menu,[{string(Atom), Atom} || Atom <- [x, y, z]],Axis,[{key,cyl_axis}]}}
            ],[]}
        ]},
        frame_yref(),
        frame_scale()
        ]}],
    wings_dialog:dialog(?__(1,"Select By Absolute Coordinates (Cylinder)"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,sel_absolute_cyl_1(Args, St)};
            (cancel) ->
                St;
            (Args) ->
                set_pref(Args),
                {commit,St,sel_absolute_cyl_1(Args, St)}
        end).

sel_absolute_cyl_1(Args, #st{selmode=Mode}=St) ->
    Select=proplists:get_value(select, Args, inside),
    Scale=abs(proplists:get_value(scale, Args, 1.0)),
    X=proplists:get_value(x, Args, 0.0)*Scale,
    Y_0=proplists:get_value(y, Args, 0.0)*Scale,
    Z=proplists:get_value(z, Args, 0.0)*Scale,
    Radius=abs(proplists:get_value(radius, Args, 1.0)*Scale),
    Height=abs(proplists:get_value(height, Args, 1.0)*Scale),
    Axis0 = proplists:get_value(cyl_axis, Args, y),
    Axis=case Axis0 of
        x -> {1.0,0.0,0.0};
        y -> {0.0,1.0,0.0};
        z -> {0.0,0.0,1.0}
    end,
    Y=case proplists:get_value(yref, Args, center) of
        center -> Y_0;
        ground when Axis0 =:= y -> Y_0+(Height*0.5);
        ground -> Y_0+Radius
    end,
    {BX1,BX2} = case Axis0 of
        x ->
            {X-(Height*0.5), X+(Height*0.5)};
        _ ->
            {X-Radius,X+Radius}
    end,
    {BY1,BY2} = case Axis0 of
        y ->
            {Y-(Height*0.5), Y+(Height*0.5)};
        _ ->
            {Y-Radius,Y+Radius}
    end,
    {BZ1,BZ2} = case Axis0 of
        z ->
            {Z-(Height*0.5), Z+(Height*0.5)};
        _ ->
            {Z-Radius,Z+Radius}
    end,
    Cylinder=e3d_bv:cylinder({X,Y,Z}, Radius, Height, Axis),
    BB=e3d_bv:box({BX1,BY1,BZ1},{BX2,BY2,BZ2}),
    sel_absolute_cyl_2({Cylinder,BB}, Mode, Select, St).
sel_absolute_cyl_2(Cylinder, vertex, BBSel, St) ->
    sel_cyl_vertex(Cylinder, BBSel, St);
sel_absolute_cyl_2(Cylinder, Mode, BBSel, St0)
  when Mode =:= face; Mode =:= edge; Mode =:= body ->
    St1=wings_sel_conv:mode(vertex, St0),
    St2=sel_cyl_vertex(Cylinder, BBSel, St1),
    wings_sel_conv:mode(Mode, St2);
sel_absolute_cyl_2(Cylinder, _, BBSel, St) ->
    St1 = wings_sel:set(face, [], St),
    sel_absolute_cyl_2(Cylinder, face, BBSel, St1).

sel_cyl_vertex_test({X,Y,Z}=Pos, {Cylinder,{{BX1,BY1,BZ1},{BX2,BY2,BZ2}}}) ->
    if
        X >= BX1, X =< BX2, Y >= BY1, Y =< BY2, Z >= BZ1, Z =< BZ2 ->
            e3d_bv:inside(Pos, Cylinder);
        true ->
            false
    end.

sel_cyl_vertex(Cylinder, BBSel, #st{sel=[]}=St) ->
    wings_sel:new_sel(sel_vertex_fun(BBSel, fun sel_cyl_vertex_test/2, Cylinder), vertex, St);
sel_cyl_vertex(Cylinder, BBSel, St) ->
    wings_sel:update_sel(sel_vertex_fun(BBSel, fun sel_cyl_vertex_test/2, Cylinder), St).



%%
%% Select By Absolute Coordinates (Sphere)
%%

sel_absolute_sphere(St) ->
    Radius = wpa:pref_get(?MODULE, radius, 1.0),
    Frame = [{vframe,[
        frame_select(),
        {hframe,[
            frame_xyz(),
            {hframe,[{label,?__(2,"Radius:")},{text,Radius,[{key,radius}]}]}
        ]},
        frame_yref(),
        frame_scale()
        ]}],
    wings_dialog:dialog(?__(1,"Select By Absolute Coordinates (Sphere)"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,sel_absolute_sphere_1(Args, St)};
            (cancel) ->
                St;
            (Args) ->
                set_pref(Args),
                {commit,St,sel_absolute_sphere_1(Args, St)}
        end).

sel_absolute_sphere_1(Args, #st{selmode=Mode}=St) ->
    Select=proplists:get_value(select, Args, inside),
    Scale=abs(proplists:get_value(scale, Args, 1.0)),
    X=proplists:get_value(x, Args, 0.0)*Scale,
    Y_0=proplists:get_value(y, Args, 0.0)*Scale,
    Z=proplists:get_value(z, Args, 0.0)*Scale,
    Radius=abs(proplists:get_value(radius, Args, 1.0)*Scale),
    Y = case proplists:get_value(yref, Args, center) of
        center -> Y_0;
        ground -> Y_0+Radius
    end,
    BX1=X-Radius,
    BX2=X+Radius,
    BY1=Y-Radius,
    BY2=Y+Radius,
    BZ1=Z-Radius,
    BZ2=Z+Radius,
    BB = e3d_bv:box({BX1,BY1,BZ1},{BX2,BY2,BZ2}),
    Sphere={{X,Y,Z},e3d_vec:dist_sqr({X,Y,Z},{X+Radius,Y,Z})},
    sel_absolute_sphere_2({Sphere,BB}, Mode, Select, St).
sel_absolute_sphere_2(Sphere, vertex, BBSel, St) ->
    sel_sphere_vertex(Sphere, BBSel, St);
sel_absolute_sphere_2(Sphere, Mode, BBSel, St0)
  when Mode =:= face; Mode =:= edge; Mode =:= body ->
    St1=wings_sel_conv:mode(vertex, St0),
    St2=sel_sphere_vertex(Sphere, BBSel, St1),
    wings_sel_conv:mode(Mode, St2);
sel_absolute_sphere_2(Sphere, _, BBSel, St) ->
    St1 = wings_sel:set(face, [], St),
    sel_absolute_sphere_2(Sphere, face, BBSel, St1).

sel_sphere_vertex_test({X,Y,Z}=Pos, {Sphere,{{BX1,BY1,BZ1},{BX2,BY2,BZ2}}}) ->
    if
        X >= BX1, X =< BX2, Y >= BY1, Y =< BY2, Z >= BZ1, Z =< BZ2 ->
            e3d_bv:inside(Pos, Sphere);
        true ->
            false
    end.

sel_sphere_vertex(Sphere, BBSel, #st{sel=[]}=St) ->
    wings_sel:new_sel(sel_vertex_fun(BBSel, fun sel_sphere_vertex_test/2, Sphere), vertex, St);
sel_sphere_vertex(Sphere, BBSel, St) ->
    wings_sel:update_sel(sel_vertex_fun(BBSel, fun sel_sphere_vertex_test/2, Sphere), St).



%%
%% Select By Axis for Normals
%%

radial_range(St) ->
    Axis = wpa:pref_get(?MODULE, axis, x),
    AngleT = wpa:pref_get(?MODULE, anglet, 15.0),
    Frame = [{vframe,[
        {hframe,[{label,?__(2,"Normal:")},{menu,[{string(Atom), Atom} || Atom <- [
            x, y, z, radial_x, radial_y, radial_z, last_axis, default_axis
          ]],Axis,[{key,axis}]}]},
        {hframe,[{label,?__(3,"Angle Tolerance:")},{text,AngleT,[{key,anglet}]}]}
        ]}],
    wings_dialog:dialog(?__(1,"Select By Axis"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,radial_range_1(Args, St)};
            (cancel) ->
                St;
            (Args) ->
                set_pref(Args),
                {commit,St,radial_range_1(Args, St)}
        end).

radial_range_1(Args, #st{selmode=Mode}=St) ->
    AngleT=abs(proplists:get_value(anglet, Args, 0.1)),
    Axis=case proplists:get_value(axis, Args, y) of
        x -> {1.0, 0.0, 0.0};
        y -> {0.0, 1.0, 0.0};
        z -> {0.0, 0.0, 1.0};
        radial_x -> {radial, {0.0, 1.0, 1.0}};
        radial_y -> {radial, {1.0, 0.0, 1.0}};
        radial_z -> {radial, {1.0, 1.0, 0.0}};
        last_axis -> wings_util:make_vector(last_axis);
        default_axis -> wings_util:make_vector(default_axis)
    end,
    radial_range_2({AngleT,Axis}, Mode, St).
radial_range_2(AxisRange, vertex, St) ->
    radial_range_vertex(AxisRange, St);
radial_range_2(AxisRange, edge, St) ->
    radial_range_edge(AxisRange, St);
radial_range_2(AxisRange, face, St) ->
    radial_range_face(AxisRange, St);
radial_range_2(AxisRange, body, St) ->
    radial_range_2(AxisRange, face, wings_sel_conv:mode(face, St)).

radial_range_vtx_fun({AngleT,Axis}) ->
    fun (Vs0, We) ->
        gb_sets:fold(fun (V, Acc) ->
            case in_angle(Axis, wings_vertex:normal(V, We), AngleT) of
                true ->
                    Acc;
                false ->
                    gb_sets:delete(V, Acc)
            end
        end, Vs0, Vs0)
    end.

radial_range_vertex(AxisRange, #st{sel=[]}=St) ->
    wings_sel:new_sel(radial_range_vtx_fun(AxisRange), vertex, St);
radial_range_vertex(AxisRange, St) ->
    wings_sel:update_sel(radial_range_vtx_fun(AxisRange), St).


radial_range_edge_fun({AngleT,Axis}) ->
    fun (Es0, #we{es=Etab}=We) ->
        gb_sets:fold(fun (E, Acc) ->
            #edge{vs=V1,ve=V2} = array:get(E,Etab),
            Normal = e3d_vec:norm(e3d_vec:add(
                wings_vertex:normal(V1, We),
                wings_vertex:normal(V2, We))),
            case in_angle(Axis, Normal, AngleT) of
                true ->
                    Acc;
                false ->
                    gb_sets:delete(E, Acc)
            end
        end, Es0, Es0)
    end.

radial_range_edge(AxisRange, #st{sel=[]}=St) ->
    wings_sel:new_sel(radial_range_edge_fun(AxisRange), edge, St);
radial_range_edge(AxisRange, St) ->
    wings_sel:update_sel(radial_range_edge_fun(AxisRange), St).


radial_range_face_fun({AngleT,Axis}) ->
    fun (Fs0, We) ->
        gb_sets:fold(fun (F, Acc) ->
            case in_angle(Axis, wings_face:normal(F, We), AngleT) of
                true ->
                    Acc;
                false ->
                    gb_sets:delete(F, Acc)
            end
        end, Fs0, Fs0)
    end.

radial_range_face(AxisRange, #st{sel=[]}=St) ->
    wings_sel:new_sel(radial_range_face_fun(AxisRange), face, St);
radial_range_face(AxisRange, St) ->
    wings_sel:update_sel(radial_range_face_fun(AxisRange), St).

in_angle({radial, {MX,MY,MZ}}, {NX,NY,NZ}=Normal, AngleT) ->
    if
        AngleT < 90.0 ->
            X1 = MX*NX,
            Y1 = MY*NY,
            Z1 = MZ*NZ,
            if
                abs(X1) < ?EPSILON, abs(Y1) < ?EPSILON, abs(Z1) < ?EPSILON ->
                    false;
                true ->
                    e3d_vec:degrees(e3d_vec:norm({X1, Y1, Z1}), Normal) =< AngleT
            end;
        true ->
            true
    end;
in_angle({_X,_Y,_Z}=Axis, Normal, AngleT) ->
    (e3d_vec:degrees(Axis, Normal) =< AngleT) orelse
        (e3d_vec:degrees(Axis, e3d_vec:neg(Normal)) =< AngleT).


%%

frame_xyz() ->
    X = wpa:pref_get(?MODULE, x, 0.0),
    Y = wpa:pref_get(?MODULE, y, 0.0),
    Z = wpa:pref_get(?MODULE, z, 0.0),
    {label_column,[
        {string_label(x),{text,X,[{key,x}]}},
        {string_label(y),{text,Y,[{key,y}]}},
        {string_label(z),{text,Z,[{key,z}]}}
      ],[]}.

frame_select() ->
    Select = wpa:pref_get(?MODULE, select, inside),
    {hframe,[{label,?__(1,"Select:")},{hradio,[
        {?__(2,"Inside"), inside},
        {?__(3,"Outside"), outside}
      ],Select,[{key,select}]}]}.

frame_yref() ->
    YRef = wpa:pref_get(?MODULE, yref, center),
    Tooltip = ?__(3, "Where the Y coordinate is located."),
    {hframe,[{label,?__(1,"Y-Reference:")},{hradio,[
        {?__(2,"Ground"), ground},
        {string(center), center}
      ],YRef,[{key,yref},{info,Tooltip}]}]}.

frame_scale() ->
    Scale = wpa:pref_get(?MODULE, scale, 1.0),
    Tooltip = ?__(1,"Rescale the entered coordinates."),
    {hframe,[{label,?__(2,"Scale:")},{text,Scale,[{key,scale},{info,Tooltip}]}]}.

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

string_label(Atom) ->
    wings_util:stringify(Atom)++":".

string(Atom) ->
    wings_util:stringify(Atom).


