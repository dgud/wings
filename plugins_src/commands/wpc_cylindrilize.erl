%%
%%  wpc_cylindrilize.erl --
%%
%%    Plugin for inflating vertex selections cylindrically
%%
%%  Copyright (c) 2008-2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_cylindrilize).
-export([init/0,menu/2,command/2]).
-include("wings.hrl").

init() ->
    true.
menu({vertex,deform},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [cylindrilize_menu()|NewMenu];
parse([A = {_,{taper,_}}|Rest], NewMenu, false) ->
    parse(Rest, [A,cylindrilize_menu()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

%%%% Menus
cylindrilize_menu() ->
    MenuTitle = ?__(1,"Inflate Cylindrical"),
    F = fun(help, _Ns) ->
		Str1 = ?__(2,"Inflate vertices cylindrically around XYZ"),
		Str2 = ?__(3,"Pick central axis"),
		Str3 = ?__(4,"Pick central axis, center point and radius"),
		{Str1,Str2,Str3};
	   (1, _Ns) -> xyz();
	   (2, _Ns) -> Ask = [central_axis],
		       {vertex,{deform,{cylindrilize,{central_axis,{'ASK',Ask}}}}};
	   (3, _Ns) -> Ask = [central_axis,center,radius],
		       {vertex,{deform,{cylindrilize,{pick_all,{'ASK',Ask}}}}}
        end,
    {MenuTitle,{cylindrilize,F}}.

xyz() ->
    [axis_menu(x),
     axis_menu(y),
     axis_menu(z)].

axis_menu(Axis) ->
    AxisStr = wings_s:dir(Axis),
    Str = ?__(1,"Inflate vertices cylindrically around central axis ~s"),
    Help = wings_util:format(Str,[AxisStr]),
    F = fun(help, _Ns) ->
		HelpRmb = ?__(2,"Pick center point and radius"),
		{Help,[],HelpRmb};
	   (1, _Ns) -> {vertex,{deform,{cylindrilize,Axis}}};
	   (2, _Ns) -> ignore;
	   (3, _Ns) -> Ask = [center,radius],
		       {vertex,{deform,{cylindrilize,{Axis,{'ASK',Ask}}}}}
        end,
    {AxisStr,{Axis,F},[]}.

%%%% Commands
command({vertex,{deform,{cylindrilize,{central_axis,{'ASK',Ask}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun get_center_and_radius/2);
command({vertex,{deform,{cylindrilize,{central_axis,Axis}}}},St) ->
    get_center_and_radius(Axis,St);

command({vertex,{deform,{cylindrilize,{pick_all,{'ASK',Ask}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun cylinder_setup/2);
command({vertex,{deform,{cylindrilize,{pick_all,Data}}}},St) ->
    cylinder_setup(Data,St);

command({vertex,{deform,{cylindrilize,{Axis,{'ASK',Ask}}}}},St) ->
    wings:ask(selection_ask(Ask), St, fun ({Center,Radius}, St0) ->
    cylinder_setup({Axis,Center,Radius},St0)
    end);
command({vertex,{deform,{cylindrilize,{Axis,{Center,Radius}}}}},St) ->
    cylinder_setup({Axis,Center,Radius},St);

command({vertex,{deform,{cylindrilize,Axis}}},St) ->
    get_center_and_radius(Axis,St);

command(_,_) -> next.

%%%% Asks
selection_ask(Asks) ->
    Ask = selection_ask(Asks,[]),
    {Ask,[],[],[vertex, edge, face]}.

selection_ask([],Ask) -> lists:reverse(Ask);

selection_ask([AskType|Rest],Ask) ->
    {Pick,Desc} = case AskType of
      central_axis ->
        {axis,?__(1,"Pick central axis")};
      center ->
        {point,?__(2,"Pick center point")};
      radius ->
        {point,?__(3,"Pick radius")}
    end,
    selection_ask(Rest,[{Pick,Desc}|Ask]).

%%%% Selection data setup
get_center_and_radius(Axis0,St) ->
    Axis = axis_conversion(Axis0),
    Center = wings_sel:center(St),
    DistList = wings_sel:fold(fun(Vs,We,Acc) ->
            VList = gb_sets:to_list(Vs),
            get_radius(Axis,Center,VList,We,Acc)
            end,[],St),
    AllDists = lists:merge(DistList),
    Radius = lists:max(AllDists),
    cylinder_callback({Axis,Center,Radius},St).

get_radius(Axis,Center,VList,We,Acc) ->
    Dist = lists:foldl(fun(Vert,A) ->
            #we{vp=Vtab} = We,
            Pos = array:get(Vert,Vtab),
            CntrOnPlane = intersect_vec_plane(Center,Pos,Axis),
            [abs(e3d_vec:dist(CntrOnPlane,Pos))|A]
            end,[],VList),
    [Dist|Acc].

cylinder_setup({Axis0,Center,Radius0},St) ->
    Axis = axis_conversion(Axis0),
    CntrOnPlane = intersect_vec_plane(Center,Radius0,Axis),
    Radius = abs(e3d_vec:dist(CntrOnPlane,Radius0)),
    cylinder_callback({Axis,Center,Radius},St).

%%%% Callback
cylinder_callback(Data,St) ->
    Tvs = wings_sel:fold(fun(Vs,We,Acc) ->
          cylindrilize_verts(Data,Vs,We,Acc)
          end,[],St),
    wings_drag:setup(Tvs,[percent],St).

cylindrilize_verts(Data,Vs0,#we{id=Id}=We,Acc) ->
    Vs = gb_sets:to_list(Vs0),
    VsPos = wings_util:add_vpos(Vs,We),
    [{Id,{Vs,cylindrilize_fun(Data,VsPos)}}|Acc].

cylindrilize_fun(Data,VsPos) ->
    fun([Dist|_], A) ->
      cylindrilize_verts2(Data,VsPos,Dist,A)
    end.

cylindrilize_verts2(Data,VsPos,Dist,A) ->
    lists:foldl(fun({V,Vpos}, VsAcc) ->
          [{V,cylindrilize(Data,Vpos,Dist)}|VsAcc]
    end, A, VsPos).

%%%% Main function
cylindrilize({Axis,Center,Radius}, Vpos, Dist) ->
    CntrOnPlane = intersect_vec_plane(Center,Vpos,Axis),
    DistFromCenter = abs(e3d_vec:dist(CntrOnPlane,Vpos)),
    Factor = DistFromCenter - Radius,
    Vector = e3d_vec:sub(CntrOnPlane,Vpos),
    Norm = e3d_vec:norm(Vector),
    e3d_vec:add(Vpos, e3d_vec:mul(Norm, Dist * Factor)).

%%%% Helper functions
intersect_vec_plane(PosA,PosB,Vector) ->
    %% Return point where Vector through PosA intersects with plane at PosB
    PlaneNorm = e3d_vec:norm(Vector),
    DotProduct = e3d_vec:dot(PlaneNorm,PlaneNorm),
    Intersection = e3d_vec:dot(e3d_vec:sub(PosB,PosA),PlaneNorm)/DotProduct,
    e3d_vec:add(PosA, e3d_vec:mul(PlaneNorm, Intersection)).

axis_conversion(Axis) ->
    case Axis of
      x -> {1.0,0.0,0.0};
      y -> {0.0,1.0,0.0};
      z -> {0.0,0.0,1.0};
      _ -> Axis
    end.
