%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_body).
-export([menu/3,command/2]).
-export([auto_smooth/1,rename_selected/2,rename_filtered/3]).

-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foldl/3,mapfoldl/3,reverse/1,sort/1,seq/2]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Dup = flip_str(),
    FlipStrL = ?__(34,"Flip the object along ~s axis"),
    FlipStrM = ?__(35,"Pick point to flip object along the ~s axis"),
    FlipStrR = ?__(36,"Flip object along the global ~s axis") ++ Dup,

    Menu = [{?__(2,"Move"),{move,Dir}},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?__(3,"Flip..."),{flip,
			       [{wings_s:dir(x),flip_fun(x),
				 {io_lib:format(FlipStrL, [wings_s:dir(x)]),
				  io_lib:format(FlipStrM, [wings_s:dir(x)]),
				  io_lib:format(FlipStrR, [wings_s:dir(x)])},[]},
				{wings_s:dir(y),flip_fun(y),
				 {io_lib:format(FlipStrL, [wings_s:dir(y)]),
				  io_lib:format(FlipStrM, [wings_s:dir(y)]),
				  io_lib:format(FlipStrR, [wings_s:dir(y)])},[]},
				{wings_s:dir(z),flip_fun(z),
				 {io_lib:format(FlipStrL, [wings_s:dir(z)]),
				  io_lib:format(FlipStrM, [wings_s:dir(z)]),
				  io_lib:format(FlipStrR, [wings_s:dir(z)])},[]},
				{?__(37,"Pick"),flip_fun(pick),
				 {?__(38,"Pick axis to flip object along"),
				  ?__(39,"Pick axis and point to flip object along"),
				  ?__(40,"Pick global axis to flip object along") ++ Dup},[]}]}},
	    separator,
	    {?__(10,"Invert"),invert,
	     ?__(11,"Flip all normals, turning the object inside out")},
	    separator,
	    {?__(12,"Tighten"),tighten,
	     ?__(13,"Move vertices towards average midpoint")},
	    {?__(14,"Subdivide"),subdiv_fun(),
	     {?__(15,"Subdivide all faces to give the object a smoother apperance"),[],
	     ?__(53,"Subdivide all the object's faces")},[]},
	    {?__(16,"Combine"),combine,
	     ?__(17,"Combine multiple objects into a single object")},
	    {?__(18,"Separate"),separate,
	     ?__(19,"Separate a combined objects into its components")},
	    separator,
	    {?__(200, "Boolean *TEST*"),
             {bool, [{?__(201,"Union"),     add, ?__(202, "Add parts from intersecting objects")},
                     {?__(203,"Intersect"), isect, ?__(204, "Make objects from the intersecting parts")},
                     {?__(205,"Difference"), sub, ?__(206, "Subtract objects from secondary selection")}]}},
	    {?__(20,"Weld"),weld,
	     ?__(21,"Merge pair of faces that are nearly coincident"),
	     [option]},
	    separator,
	    {?__(22,"Cleanup"),cleanup,
	     ?__(23,"Remove various defects"),
	     [option]},
	    {?__(24,"Auto-Smooth"),auto_smooth,
	     ?__(25,"Set edges hard or soft depending on the angle between faces"),
	     [option]},
	    separator,
	    {?__(26,"Duplicate"),{duplicate,Dir}},
	    {?__(27,"Delete"),delete,?__(28,"Delete the selected objects")},
	    {?__(29,"Rename"),rename_fun(),
	     {?__(30,"Rename selected objects"),[],
	      ?__(51,"Add a common prefix to each selected object")},[]},
	    separator,
	    %%?__(31,"Show All"),show_all,
	    {?__(52,"Unhide Faces"),show_all,
	     ?__(32,"Show all faces for this object")},
	    {?__(33,"Vertex Attributes"),
	     {vertex_attributes,
	      [{?__(41,"Colors to Materials"),colors_to_materials,
		?__(42,"Convert vertex colors to materials")},
	       {?__(43,"Materials to Colors"),materials_to_colors,
		?__(44,"Convert materials to vertex colors")},
	       {?__(45,"Remove Colors"),remove_colors,
		?__(46,"Remove all vertex colors")},
	       {?__(47,"Remove UV Coordinates"),remove_uv_coordinates,
		?__(48,"Remove all UV coordinates")},
	       {?__(49,"Remove All Attributes"),remove_all_attributes,
		?__(50,"Remove all vertex colors and UV coordinates")}]}}|
	    mode_dependent(St)],
    wings_menu:popup_menu(X, Y, body, Menu).

rename_fun() ->
    fun
        (1,_) -> {body,rename};
        (3,_) -> {body,{rename,prefix}};
        (_,_) -> ignore
	     end.

flip_fun(pick) ->
    fun
	(1, _Ns) -> {body,{flip,{dup(local),{'ASK',[flip_axis]}}}};
	(2, _Ns) -> {body,{flip,{dup(point),{'ASK',[flip_axis,flip_point]}}}};
	(3, _Ns) -> {body,{flip,{dup(global),{'ASK',[flip_axis]}}}}
	       end;
flip_fun(Axis) ->
    fun
        (1, _Ns) -> {body,{flip,{dup(local),Axis}}};
        (2, _Ns) -> {body,{flip,{dup(point),{Axis,{'ASK',[flip_point]}}}}};
        (3, _Ns) -> {body,{flip,{dup(global),Axis}}}
    end.

subdiv_fun() ->
    fun
	(1, _Ns) -> {body,smooth};
	(3, _Ns) -> {body,subdiv};
	(_, _) -> ignore
    end.

dup(Type) ->
    %% Return {dup,Type} if Alt is pressed during Flip command, otherwise Type.
    case wings_io:is_modkey_pressed(?ALT_BITS) of
        true -> {dup,Type};
        false -> Type
    end.

mode_dependent(St) ->
    MF = fun(_, We) when ?IS_AREA_LIGHT(We) -> arealight;
            (_, _) -> object
         end,
    RF = fun(T, []) -> T;
            (T, T) -> T;
            (_, _) -> mixed
         end,
    Kind = wings_sel:dfold(MF, RF, [], St),
    Tail = vertex_color_item(Kind),
    arealight_conv(Kind, Tail).

vertex_color_item(object) ->
    [{?__(1,"Vertex Color"),vertex_color,
      ?__(3,"Apply vertex colors to selected objects")}];
vertex_color_item(_) -> [].

arealight_conv(arealight, T) ->
    [{?__(1,"Area Light to Object"),from_arealight,
      ?__(2,"Convert selected area lights to objects")}|T];
arealight_conv(object, T) ->
    [{?__(3,"Object To Area Light"),to_arealight,
      ?__(4,"Convert selected objects to area lights")}|T];
arealight_conv(mixed, T) -> T.

command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command(invert, St) ->
    {save_state,invert_normals(St)};
command(duplicate, St) ->
    {save_state,duplicate(none, St)};
command({duplicate,Dir}, St) ->
    duplicate(Dir, St);
command({duplicate_object,Ids}, St) ->
    {save_state,duplicate_object(Ids, St)};
command(delete, St) ->
    {save_state,delete(St)};
command({delete_object,Ids}, St) ->
    {save_state,delete_object(Ids, St)};
command(tighten, St) ->
    tighten(St);
command(smooth, St) ->
    ?SLOW({save_state,smooth(St)});
command(subdiv, St) ->
    ?SLOW({save_state,subdiv(St)});
command(combine, St) ->
    {save_state,combine(St)};
command(separate, St) ->
    {save_state,separate(St)};
command(auto_smooth, St) ->
    auto_smooth(St);
command({auto_smooth,Ask}, St) ->
    auto_smooth(Ask, St);
command({flip,{Type,{Axis,{'ASK',Ask}}}},St) ->
    wings:ask(flip_ask(Ask), St, fun(Result,St0) ->
					 {save_state,flip(Type,{Axis,Result}, St0)}
				 end);
command({flip,{Type,{'ASK',Ask}}}, St) ->
    wings:ask(flip_ask(Ask), St, fun(Result,St0) ->
					 {save_state,flip(Type, Result, St0)}
				 end);
command({flip,{Type, Plane}}, St) ->
    {save_state,flip(Type, Plane, St)};
command(cleanup, St) ->
    cleanup(false, St);
command({cleanup,Ask}, St) ->
    cleanup(Ask, St);
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command(rename, St) ->
    rename(St);
command({rename,prefix}, St) ->
    rename_prefix(St);
command({rename,Ids}, St) ->
    rename(Ids, St);
command(to_arealight, St) ->
    to_arealight(St);
command(from_arealight, St) ->
    from_arealight(St);
command({vertex_attributes,materials_to_colors}, St) ->
    {save_state,materials_to_colors(St)};
command({vertex_attributes,colors_to_materials}, St) ->
    {save_state,colors_to_materials(St)};
command({vertex_attributes,remove_colors}, St) ->
    {save_state,va_remove(color, St)};
command({vertex_attributes,remove_uv_coordinates}, St) ->
    {save_state,va_remove(uv, St)};
command({vertex_attributes,remove_all_attributes}, St) ->
    {save_state,va_remove(all, St)};
command({bool,add}, St0) ->
    St1 = ?SLOW(wings_sel:valid_sel(wings_bool:add(St0))),
    {save_state, wings_obj:recreate_folder_system(St1)};
command({bool,isect}, St0) ->
    St1 = ?SLOW(wings_sel:valid_sel(wings_bool:isect(St0))),
    {save_state, wings_obj:recreate_folder_system(St1)};
command({bool,sub}, St0) ->
    wings_bool:sub(St0);
command({weld,Ask}, St) ->
    ?SLOW(weld(Ask, St));
command(vertex_color, St) ->
    wings_color:choose(fun(Color) ->
			       set_color(Color, St)
		       end);
command(show_all, St) ->
    {save_state,
     wings_sel:map(fun(_, We) ->
			   wings_we:show_faces(We)
		   end, St)}.

%%%
%%% The Cleanup command.
%%%

cleanup(Ask, _) when is_atom(Ask) ->
    Qs = [{?__(1,"Short Edges"),true,[{key,short_edges}]},
	  {hframe,
	   [{label,?__(2,"Length Tolerance")},
	    {text,1.0E-3,[{range,{1.0E-5,10.0}}]}]},
	  {?__(3,"Isolated Vertices"),true,
	   [{key,isolated_vs}]}],
    wings_dialog:dialog(Ask,
			?__(4,"Cleanup"),
			[{vframe,Qs}],
			fun(Res) -> {body,{cleanup,Res}} end);
cleanup(Opts, St0) ->
    St = wings_sel:map(fun(_, We0) ->
			       We1 = cleanup_repeated_vtxs(We0),
			       We2 = cleanup_waists(We1),
			       We = cleanup_2edged_faces(We2),
			       cleanup_1(Opts, We)
		       end, St0),
    {save_state,St}.

cleanup_1([{short_edges,Flag},Tolerance|Opts], We0) ->
    We = case Flag of
	     true -> clean_short_edges(Tolerance, We0);
	     false -> We0
	 end,
    cleanup_1(Opts, We);
cleanup_1([{isolated_vs,true}|Opts], We) ->
    cleanup_1(Opts, clean_isolated_vertices(We));
cleanup_1([_|Opts], We) ->
    cleanup_1(Opts, We);
cleanup_1([], We) -> We.

clean_isolated_vertices(We) ->
    case wings_vertex:isolated(We) of
	[] -> We;
	[_]=Isolated ->
	    io:put_chars(?__(1,"Removed 1 isolated vertex\n")),
	    wings_vertex:dissolve_isolated(Isolated, We);
	Isolated ->
	    io:format(?__(2,"Removed ~p isolated vertices\n"),
		      [length(Isolated)]),
	    wings_vertex:dissolve_isolated(Isolated, We)
    end.

clean_short_edges(Tolerance, #we{es=Etab,vp=Vtab}=We) ->
    Short = array:sparse_foldl(
	      fun(Edge, #edge{vs=Va,ve=Vb}, A) ->
		      VaPos = array:get(Va, Vtab),
		      VbPos = array:get(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < Tolerance -> [Edge|A];
			  _Dist -> A
		      end
	      end, [], Etab),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case array:get(Edge, Et) of
		      undefined -> W;
		      _ -> wings_collapse:collapse_edge(Edge, W)
		  end
	  end, We, Short).

%%
%% A vertex may not be used more than once in a single face.
%%
cleanup_repeated_vtxs(#we{fs=Ftab}=We) ->
    cleanup_rep_1(gb_trees:keys(Ftab), We).

cleanup_rep_1([F|Fs], We0) ->
    case repeated_vertex(F, We0) of
	none ->
	    cleanup_rep_1(Fs, We0);
	V ->
	    io:format(?__(1,"Repeated vertex ~p in face ~p\n"), [V,F]),
	    We = cleanup_rep_2(F, V, We0),
	    NewFaces = wings_we:new_items_as_ordset(face, We0, We),
	    cleanup_rep_1(NewFaces++Fs, We)
    end;
cleanup_rep_1([], We) -> We.

cleanup_rep_2(Face, V0, #we{es=Etab,fs=Ftab0}=We0) ->
    Es = wings_vertex:fold(
	   fun(Edge, _, Rec, A) ->
		   case Rec of
		       #edge{ve=V0,lf=Face} -> [Edge|A];
		       #edge{vs=V0,rf=Face} -> [Edge|A];
		       _ -> A
		   end
	   end, [], V0, We0),
    Ves = wings_face:fold(fun(V, E, _, A) ->
				  [{V,E}|A]
			  end, [], Face, hd(Es), We0),
    Ftab = gb_trees:delete(Face, Ftab0),
    Mat = wings_facemat:face(Face, We0),
    We1 = wings_facemat:delete_face(Face, We0),
    cleanup_rep_3(Ves, V0, Face, Mat, Ftab, Etab, We1).

cleanup_rep_3([], _, _, _, Ftab, Etab, We) ->
    We#we{fs=Ftab,es=Etab};
cleanup_rep_3(Ves0, V, Face, Mat, Ftab0, Etab0, We0) ->
    [{_,First}|_] = Ves0,
    {NewFace,We1} = wings_we:new_id(We0),
    We = wings_facemat:assign(Mat, [NewFace], We1),
    Ftab = gb_trees:insert(NewFace, First, Ftab0),
    {Ves,Etab1} = cleanup_rep_4(Ves0, V, Face, NewFace, Etab0),
    [{_,Last}|NextVes] = Ves,
    Etab2 = cleanup_patch_edge(V, NewFace, First, Last, Etab1),
    Etab = cleanup_patch_edge(V, NewFace, Last, First, Etab2),
    cleanup_rep_3(NextVes, V, Face, Mat, Ftab, Etab, We).

cleanup_rep_4([{Vtx,Edge}|T]=Ves, V0, Face, NewFace, Etab0) ->
    Rec = case array:get(Edge, Etab0) of
	      #edge{lf=Face}=Rec0 -> Rec0#edge{lf=NewFace};
	      #edge{rf=Face}=Rec0 -> Rec0#edge{rf=NewFace}
	  end,
    Etab = array:set(Edge, Rec, Etab0),
    if
	Vtx =:= V0 -> {Ves,Etab};
	true -> cleanup_rep_4(T, V0, Face, NewFace, Etab)
    end.

repeated_vertex(Face, We) ->
    Vs = wings_face:vertices_ccw(Face, We),
    repeated_vertex_1(sort(Vs)).

repeated_vertex_1([V,V|_]) -> V;
repeated_vertex_1([_|T]) -> repeated_vertex_1(T);
repeated_vertex_1([]) -> none.

cleanup_patch_edge(V, Face, From, To, Etab) ->
    R = case array:get(From, Etab) of
	    #edge{lf=Face,ve=V}=R0 -> R0#edge{ltpr=To};
	    #edge{lf=Face,vs=V}=R0 -> R0#edge{ltsu=To};
	    #edge{rf=Face,vs=V}=R0 -> R0#edge{rtpr=To};
	    #edge{rf=Face,ve=V}=R0 -> R0#edge{rtsu=To}
	end,
    array:set(From, R, Etab).

%%
%% A waist is a vertex shared by edges all of which cannot be
%% reached from the incident edge of the vertex.
%%
cleanup_waists(#we{es=Etab,vp=Vtab}=We) ->
    VsEs0 = array:sparse_foldl(fun(E, #edge{vs=Va,ve=Vb}, A) ->
				       [{Va,E},{Vb,E}|A]
			       end, [], Etab),
    VsEs = wings_util:rel2fam(VsEs0),
    cleanup_waists_1(wings_util:array_keys(Vtab), VsEs, We).

cleanup_waists_1([V|Vs], [{V,AllEs}|VsEs], #we{es=Etab0,vp=Vtab0,vc=Vct0}=We0) ->
    Es0 = wings_vertex:fold(fun(E, _, _, A) -> [E|A] end, [], V, We0),
    case ordsets:subtract(AllEs, ordsets:from_list(Es0)) of
	[] ->					%Good.
	    cleanup_waists_1(Vs, VsEs, We0);
	[AnEdge|_]=Es ->
	    %% Some edges cannot be reached from the incident edge.
	    %% Repair by duplicating the original vertex.
	    {NewV,We1} = wings_we:new_id(We0),
	    Etab = patch_vtx_refs(Es, V, NewV, Etab0),
	    Vtab = array:set(NewV, array:get(V, Vtab0), Vtab0),
	    Vct = array:set(NewV, AnEdge, Vct0),
	    We = We1#we{es=Etab,vp=Vtab,vc=Vct},
	    io:format(?__(1,"Removed waist vertex: ~p\n"), [V]),

	    %% Re-process the newly added vertex. (Some of the
	    %% edges may not be reachable from the incident edge of
	    %% the new vertex.)
	    cleanup_waists_1([NewV|Vs], [{NewV,Es}|VsEs], We)
    end;
cleanup_waists_1([], [], We) -> We.

patch_vtx_refs([E|Es], OldV, NewV, Etab0) ->
    Etab = case array:get(E, Etab0) of
	       #edge{vs=OldV}=Rec ->
		   array:set(E, Rec#edge{vs=NewV}, Etab0);
	       #edge{ve=OldV}=Rec ->
		   array:set(E, Rec#edge{ve=NewV}, Etab0)
	   end,
    patch_vtx_refs(Es, OldV, NewV, Etab);
patch_vtx_refs([], _, _, Etab) -> Etab.

%%
%% Remove faces having only two edges.
%%
cleanup_2edged_faces(#we{fs=Ftab}=We) ->
    delete_2edged_faces_1(gb_trees:keys(Ftab), We).

delete_2edged_faces_1([Face|Faces], We0) ->
    case delete_if_bad(Face, We0) of
	bad_edge ->
	    wings_u:error_msg(?__(1,"Face") ++ integer_to_list(Face) ++
				  ?__(2,"has only one edge"));
	We -> delete_2edged_faces_1(Faces, We)
    end;
delete_2edged_faces_1([], We) -> We.

delete_if_bad(Face, #we{fs=Ftab,es=Etab}=We) ->
    case gb_trees:lookup(Face, Ftab) of
	{value,Edge} ->
	    case array:get(Edge, Etab) of
		#edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		    bad_edge;
		#edge{ltpr=Same,ltsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		#edge{rtpr=Same,rtsu=Same} ->
		    wings_edge:dissolve_edge(Edge, We);
		_ -> We
	    end;
	none -> We
    end.

%%%
%%% The Invert command.
%%%

invert_normals(St) ->
    wings_sel:map(fun(_, We) -> wings_we:invert_normals(We) end, St).

%%%
%%% The Duplicate command.
%%%

duplicate(Dir, St0) ->
    CF = fun(Items, We) ->
                 Empty = gb_sets:empty(),
                 New = [{We,Items,copy}],
                 {We,Empty,New}
         end,
    St = wings_sel:clone(CF, St0),
    case Dir of
	none -> St;
	_ -> wings_move:setup(Dir, St)
    end.

%%%
%%% Duplicate called from the Outliner or Object window.
%%%

duplicate_object(Objects0, St0) ->
    St = wings_sel:set(body, [{Id, gb_sets:singleton(0)} || Id <- Objects0], St0),
    CF = fun(Items, #we{id=_Id}=We) ->
                 {We,Items,[{We,gb_sets:empty(),copy}]}
         end,
    wings_sel:clone(CF, body, St).

%%%
%%% The Delete command.
%%%

delete(St) ->
    wings_sel:map_update_sel(
      fun(_, _) ->
              {#we{},gb_sets:empty()}
      end, St).

%%%
%%% Delete called from the Outliner or Object window.
%%%

delete_object(Objects, St) ->
    foldl(fun wings_obj:delete/2, St, Objects).

%%%
%%% The Flip command
%%%

flip_ask(Asks) ->
    Ask = flip_ask(Asks,[]),
    {Ask,[],[],[vertex,edge,face]}.

flip_ask([],Ask) -> lists:reverse(Ask);
flip_ask([flip_axis|Asks],Result) ->
    Pick = {axis,?__(1,"Select axis to flip object along") ++ flip_str()},
    flip_ask(Asks,[Pick|Result]);
flip_ask([flip_point|Asks],Result) ->
    Pick = {point,?__(2,"Select point along the chosen axis to flip object") ++ flip_str()},
    flip_ask(Asks,[Pick|Result]).

flip_str() -> "  " ++ ?__(1,"[Alt]+Click to Duplicate object").

flip({dup,Type}, Plane, St0) ->
    St = duplicate(none,St0),
    flip_cmd(Type,Plane,St);
flip(Type, Plane, St0) ->
    St = case dup(Type) of
	     {dup,_} -> duplicate(none,St0);
	     _Otherwise -> St0
	 end,
    flip_cmd(Type,Plane,St).

flip_cmd(Type, Plane, St) ->
    wings_sel:map(fun(_, We) -> flip_body(Type, Plane, We) end, St).

flip_body(_Type, {{_,_,_}=Plane,{_,_,_}=Point}, We) ->
    do_flip(Point, Plane, We);
flip_body(Type, Plane0, We) ->
    Plane = flip_normal(Plane0),
    Point0 = e3d_vec:average(wings_vertex:bounding_box(We)),
    Point = flip_center(Type, Plane0, Point0),
    do_flip(Point, Plane, We).

%% do_flip(Point, PlaneNormal, We0) -> We
%%  Flip an object around the plane defined by the point Point
%%  and the plane's normal PlaneNormal.
%%
do_flip({Cx,Cy,Cz}, PlaneNormal, We0) ->
    ToZMat = e3d_mat:rotate_to_z(PlaneNormal),
    FromZMat = e3d_mat:transpose(ToZMat),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, FromZMat),
    M2 = e3d_mat:mul(M1, e3d_mat:scale(1.0, 1.0, -1.0)),
    M3 = e3d_mat:mul(M2, ToZMat),
    M = e3d_mat:mul(M3, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We = wings_we:transform_vs(M, We0),
    wings_we:invert_normals(We).

flip_normal(x) -> {1.0,0.0,0.0};
flip_normal(y) -> {0.0,1.0,0.0};
flip_normal(z) -> {0.0,0.0,1.0};
flip_normal({Axis,_}) -> flip_normal(Axis);
flip_normal({_,_,_}=Normal) -> Normal.

flip_center(local, _, Center) -> Center;
flip_center(global, x, {_,Cy,Cz}) -> {0.0,Cy,Cz};
flip_center(global, y, {Cx,_,Cz}) -> {Cx,0.0,Cz};
flip_center(global, z, {Cx,Cy,_}) -> {Cx,Cy,0.0};
flip_center(global, {_,_,_}=Plane, Point) ->
    %% Project the Point down to the plane where the global axis
    %% meets the origin.
    e3d_vec:sub(Point, e3d_vec:mul(Plane, e3d_vec:dot(Point, Plane)));
flip_center(point, {x,{X,_,_}}, {_,Cy,Cz}) -> {X,Cy,Cz};
flip_center(point, {y,{_,Y,_}}, {Cx,_,Cz}) -> {Cx,Y,Cz};
flip_center(point, {z,{_,_,Z}}, {Cx,Cy,_}) -> {Cx,Cy,Z}.

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    wings_drag:fold(fun tighten/2, [percent], St).

tighten(_, #we{vp=Vtab}=We) ->
    Vs = wings_util:array_keys(Vtab),
    wings_vertex_cmd:tighten_vs(Vs, We).

%%%
%%% The subdiv command.
%%%

subdiv(St) ->
    wings_sel:map(fun(_, We) ->
	wings_subdiv:subdiv(We)
		  end, St).

%%%
%%% The Smooth command.
%%%

smooth(St) ->
    wings_sel:map(fun(_, We) ->
			  wings_subdiv:smooth(We)
		  end, St).

%%%
%%% The Combine command.
%%%

combine(#st{sel=[]}=St) -> St;
combine(#st{sel=[_]}=St) -> St;
combine(#st{}=St) ->
    CF = fun(Items, We) ->
                 {We,Items}
         end,
    wings_sel:combine(CF, St).

%%%
%%% The Separate command.
%%%

separate(St) ->
    CF = fun(Items, We0) ->
                 Empty = gb_sets:empty(),
                 [We|Wes] = wings_we:separate(We0),
                 New = [{W,Empty,sep} || W <- Wes],
                 {We,Items,New}
         end,
    wings_sel:clone(CF, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    do_auto_smooth(60, St).

auto_smooth(Ask, _) when is_atom(Ask) ->
    wings_dialog:ask(Ask,?__(1,"Auto Smooth Parameters"),
		     [{?__(2,"Crease Angle"),60,[{range,{0,180}}]}],
		     fun(Res) -> {body,{auto_smooth,Res}} end);
auto_smooth([Angle], St) ->
    {save_state,do_auto_smooth(Angle, St)}.

do_auto_smooth(Angle, St) ->
    Cos = cos_degrees(Angle),
    wings_sel:map(fun(_, We) -> auto_smooth_1(Cos, We) end, St).

auto_smooth_1(Cos, #we{es=Etab,he=Htab0}=We) ->
    Htab = array:sparse_foldl(fun(E, R, A) ->
				      auto_smooth(E, R, Cos, A, We)
			      end, Htab0, Etab),
    We#we{he=Htab}.

auto_smooth(Edge, #edge{lf=Lf,rf=Rf}, Cos, H0, We) ->
    Ln = wings_face:normal(Lf, We),
    Lr = wings_face:normal(Rf, We),
    case e3d_vec:is_zero(Ln) orelse e3d_vec:is_zero(Lr) of
	true -> H0;				%Ignore this edge.
	false ->
	    case e3d_vec:dot(Ln, Lr) of
		P when P < Cos ->
		    wings_edge:hardness(Edge, hard, H0);
		_ ->				%angle =< 60
		    wings_edge:hardness(Edge, soft, H0)
	    end
    end.

cos_degrees(Angle) ->
    math:cos(Angle*math:pi()/180.0).


%%%
%%% Rename selected objects.
%%%

%% used by wings_shape - Rename option - Selected
rename_selected(Mask, St) ->
    Objs = wings_sel:fold_obj(fun(Obj, A) -> [Obj|A] end, [], St),
    F = fun(_, I) ->
                Name = get_masked_name(Mask, I),
                {Name,I+1}
        end,
    {Names,_} = mapfoldl(F, 1, Objs),
    rename_1(Names, Objs, St).

%% used by wings_shape - Rename option - Filtered
rename_filtered(Filter, Mask, St) ->
    F = fun(#{name:=Name0}=Obj, {I,NameAcc,ObjAcc}=Acc) ->
                case wings_util:is_name_masked(Name0, Filter) of
                    true ->
                        Name = get_masked_name(Mask, I),
                        {I+1,[Name|NameAcc],[Obj|ObjAcc]};
                    false ->
                        Acc
                end
        end,
    {_,Names,Objs} = wings_obj:fold(F, {1,[],[]}, St),
    rename_1(Names, Objs, St).

get_masked_name(Mask,SeqNum) ->
    Idx=string:chr(Mask,$%),
    Mask0 = if Idx=:=0 -> Mask++"%";
	       true -> Mask
	    end,
    Len=string:len(Mask0),
    case string:chr(Mask0,$%) of
	0 -> integer_to_list(SeqNum)++Mask;
	Len -> string:sub_string(Mask,1,Len-1)++integer_to_list(SeqNum);
	Idx0 ->
	    Prefix=string:sub_string(Mask,1,Idx0-1),
	    Suffix0=string:sub_string(Mask,Idx0+1),
	    Idx1=string:chr(Suffix0,$%),
	    {SeqNum0,Suffix}= if Idx1=/=0 ->
				      StartNum=string:sub_string(Suffix0,1,Idx1-1),
				      case string:to_integer(StartNum) of
					  {error,_} ->  {SeqNum,Suffix0};
					  {Value,_} ->  {Value+SeqNum-1,string:sub_string(Suffix0,Idx1+1)}
				      end;
				 true -> {SeqNum,Suffix0}
			      end,
	    Prefix++integer_to_list(SeqNum0)++Suffix
    end.

rename_prefix(St) ->
    Q = [{hframe,[{text,"Enter Prefix", []}]}],
    DF = fun([Prefix]) ->
                 MF = fun(#{name:=Name0}=Obj) ->
                              Name = Prefix ++ Name0,
                              Obj#{name:=Name}
                      end,
                 wings_sel:map_obj(MF, St)
         end,
    wings_dialog:dialog(?__(1,"Prefix Selected Objects"), Q, DF).

rename(St) ->
    F = fun(Obj, A) -> [Obj|A] end,
    Objs = wings_sel:fold_obj(F, [], St),
    rename_1(Objs, St).

rename(Ids, St) ->
    Objs = [wings_obj:get(Id, St) || Id <- Ids],
    rename_1(Objs, St).

rename_1(Objs, St) ->
    Qs = rename_qs(Objs),
    wings_dialog:dialog(?__(1,"Rename"), Qs,
			fun([[]]) ->
                                ignore;
			   (NewNames) ->
				rename_1(NewNames, Objs, St)
			end).

rename_1(Names, Objs, St) ->
    rename_2(Names, Objs, St).

rename_2([""|Ns], [#{}|Objs], St) ->
    rename_2(Ns, Objs, St);
rename_2([N|Ns], [Obj|Objs], St0) ->
    St = wings_obj:put(Obj#{name:=N}, St0),
    rename_2(Ns, Objs, St);
rename_2([], [], St) -> St.

rename_qs(Objs) ->
    OldNames = [{label,Name} || #{name:=Name} <- Objs],
    TextFields = [{text,Name,[]} || #{name:=Name} <- Objs],
    [{hframe,
      [{vframe,OldNames},
       {vframe,TextFields}]}].

%%%
%%% Conversion to and from area lights.
%%%
to_arealight(St) ->
    wings_sel:map(fun to_arealight_1/2, St).

to_arealight_1(_, We) ->
    false = ?IS_ANY_LIGHT(We),                  %Assertion.
    #we{light=Light} = wings_light:import([{opengl,[{type,area}]}]),
    We#we{light=Light}.

from_arealight(St) ->
    wings_sel:map(fun(_, We) -> We#we{light=none} end, St).

%%%
%%% Convert materials to vertex colors.
%%%

materials_to_colors(St) ->
    wings_sel:map(fun(_, We) ->
			  wings_we:uv_to_color(We, St)
		  end, St).

%%%
%%% Convert vertex colors to materials.
%%%

colors_to_materials(St0) ->
    {St,#st{mat=Mat}} =
	wings_sel:mapfold(fun(_, We, S) ->
				  colors_to_materials_1(We, S)
			  end, St0, St0),
    St#st{mat=Mat}.

colors_to_materials_1(#we{fs=Ftab}=We0, St) ->
    colors_to_materials_2(gb_trees:keys(Ftab), We0, [], St).

colors_to_materials_2([F|Fs], We, Acc, St0) ->
    Colors = [C || C <- wings_va:face_attr(color, F, We)],
    case wings_color:average(Colors) of
	none ->
	    colors_to_materials_2(Fs, We, Acc, St0);
	Color ->
	    {Name,St} = color_material(Color, St0),
	    colors_to_materials_2(Fs, We, [{F,Name}|Acc], St)
    end;
colors_to_materials_2([], We, FaceMat, St) ->
    {wings_facemat:assign(FaceMat, We),St}.

color_material({R,G,B}=Color, #st{mat=Mat0}=St0) ->
    Name0 = ?__(1,"color_")++ fmt_int(R) ++ "_" ++ fmt_int(G) ++
	"_" ++ fmt_int(B),
    Name = list_to_atom(Name0),
    case gb_trees:is_defined(Name, Mat0) of
	true -> {Name,St0};
	false ->
	    Mat = [{opengl,[{diffuse,Color}]}],
	    case wings_material:add_materials([{Name,Mat}], St0) of
		{St,[]} -> {Name,St};
		{St,[{Name,New}]} -> {New,St}
	    end
    end.

fmt_int(I) ->
    L = integer_to_list(trunc(256*I)),
    fmt_int(length(L), L).

fmt_int(3, L) -> L;
fmt_int(N, L) -> fmt_int(N+1, [$0|L]).

%%%
%%% Removing vertex attributes.
%%%

va_remove(What, St) ->
    wings_sel:map(fun(_, We) ->
			  wings_va:remove(What, We)
		  end, St).

%%%
%%% The Weld command.
%%%

weld(Ask, _) when is_atom(Ask) ->
    Qs = [{hframe,
	   [{label,?__(1,"Distance Tolerance")},
	    {text,1.0E-3,[{range,{1.0E-5,10.0}}]}]}],
    wings_dialog:dialog(Ask, ?__(2,"Weld"), Qs,
			fun(Res) -> {body,{weld,Res}} end);
weld([Tolerance], St) ->
    CF = fun(_, We) ->
                 weld_objects(Tolerance, gb_sets:empty(), We)
         end,
    wings_sel:combine(CF, vertex, St).

%% Cycle over #we{} until it doesn't change. This is done because
%% matching faces that are connected by a single vertex can't be
%% processed (as far as I've tried). So the hope is that another face
%% pair in the shape will be connect the next time through the St,
%% which will make the unprocessed face pair share at least one
%% edge. All the same, not all shapes will be processed. If there only
%% single vert matching face pairs, then nothing can be done.

weld_objects(Tolerance, Vs0, We0) ->
    ErrorMsg = ?__(1,"Found no faces to weld."),
    case weld_1(Tolerance, We0) of
        error ->
            case gb_sets:is_empty(Vs0) of
                true ->
                    wings_u:error_msg(ErrorMsg);
                false ->
                    {We0,Vs0}
            end;
        {We,Vs1} ->
            Vs = gb_sets:union(Vs0, Vs1),
            AllVs = wings_sel:get_all_items(vertex, We),
            SelectedVs = gb_sets:intersection(AllVs, Vs),
            weld_objects(Tolerance, SelectedVs, We)
    end.

weld_1(Tol, #we{fs=Fs0}=We0) ->
    Fs = qualified_fs(gb_trees:keys(Fs0), Tol, We0, []),
    R = sofs:relation(Fs, [{key,face}]),
    F = sofs:relation_to_family(R),
    Parts0 = sofs:range(F),
    Parts1 = sofs:specification({external,fun([_]) -> false;
                                             (_) -> true end}, Parts0),
    Parts = sofs:to_external(Parts1),
    case weld_parts(Parts, Tol, We0) of
        We0 ->
            error;
        We ->
            {We,weld_selection(lists:append(Parts), We0, We)}
    end.

qualified_fs([F|Fs], Tol, We, Acc) ->
    Vs = wings_face:fold(
	   fun(V, _, _, Acc0) ->
		   [V|Acc0]
	   end, [], F, We),
    {X,Y,Z} = wings_vertex:center(Vs, We),
    Center = {granularize(X, Tol),granularize(Y, Tol),granularize(Z, Tol)},
    qualified_fs(Fs, Tol, We, [{{length(Vs),Center},F}|Acc]);
qualified_fs([], _, _, Acc) -> Acc.

granularize(F, Tol) -> Tol*round(F/Tol).

weld_parts([P|Ps], Tol, We0) ->
    We = weld_part(P, Tol, We0),
    weld_parts(Ps, Tol, We);
weld_parts([], _, We) -> We.

weld_part([F|Fs], Tol, We) ->
    weld_part_1(F, Fs, Tol, We, []);
weld_part([], _, We) -> We.

weld_part_1(Fa, [Fb|Fs], Tol, We0, Acc) ->
    try try_weld(Fa, Fb, Tol, We0) of
        error ->
            weld_part_1(Fa, Fs, Tol, We0, [Fb|Acc]);
        #we{}=We0 ->
            weld_part_1(Fa, Fs, Tol, We0, [Fb|Acc]);
        #we{}=We ->
            weld_part(Fs++Acc, Tol, We)
    catch
        error:_ ->
            weld_error()
    end;
weld_part_1(_, [], Tol, We, Acc) ->
    weld_part(Acc, Tol, We).

try_weld(Fa, Fb, Tol, We) ->
    Na = wings_face:normal(Fa, We),
    Nb = wings_face:normal(Fb, We),
    case e3d_vec:dot(Na, Nb) of
	Dot when Dot < -0.99 ->
	    case wings_face:are_neighbors(Fa, Fb, We) of
		true ->
		    case shared_edges(Fa, Fb, We) of
			[] ->
                            error;
			CommonEs ->
			    weld_neighbors(Fa, Fb, CommonEs, Tol, We)
		    end;
		false ->
		    try_weld_1(Fa, Fb, Tol, We)
	    end;
	_Dot ->
            error
    end.

shared_edges(Fa, Fb, We) ->
    FaEs = wings_face:to_edges([Fa], We),
    FbEs = wings_face:to_edges([Fb], We),
    AllEs = lists:sort(FaEs ++ FbEs),
    get_shared_edges(AllEs).

get_shared_edges([E,E|Es]) ->
    [E|get_shared_edges(Es)];
get_shared_edges([_|Es]) ->
    get_shared_edges(Es);
get_shared_edges([]) ->
    [].

weld_neighbors(Fa, Fb, CommonEs, Tol, We0) ->
    Vs0 = wings_edge:to_vertices(CommonEs,We0),
    #we{fs=Ftab}=We1 = dissolve_edges(CommonEs, We0),
    ARemains = gb_trees:is_defined(Fa,Ftab),
    BRemains = gb_trees:is_defined(Fb,Ftab),
    case {ARemains,BRemains} of
        {true,false} ->
            case check_weld_neighbors(Fa, Vs0, Tol, We1) of
                error ->
                    wings_dissolve:faces([Fa,Fb], We0);
                Other ->
                    Other
            end;
        {false,true} ->
            case check_weld_neighbors(Fb, Vs0, Tol, We1) of
                error ->
                    wings_dissolve:faces([Fa,Fb], We0);
                Other ->
                    Other
            end;
        {_,_} ->
            We = wings_dissolve:faces([Fa,Fb],We0),
            case wings_we:new_items_as_ordset(face, We0, We) of
                [NewFace] ->
                    case check_weld_neighbors(NewFace, Vs0, Tol, We) of
                        error -> We;
                        Other -> Other
                    end;
                _ ->
                    We
            end
    end.

check_weld_neighbors(Face, Vs0, Tol, We) ->
    Vs1 = wings_face:to_vertices([Face], We),
    if
	Vs0 =:= Vs1 ->
            error;
	true ->
	    Vs = Vs1 -- Vs0,
	    if
		Vs =:= [] ->
                    error;
		true ->
		    NVs = Vs0 -- Vs,
		    case get_vs_pairs(Vs, Tol, Face, We) of
			[] ->
                            error;
			CPList ->
			    connect_and_collapse(Face, CPList, NVs, [], We)
		    end
	    end
    end.

get_vs_pairs(Vs, Tol, Face, We) ->
    VPos = wings_util:add_vpos(Vs, We),
    get_vs_pairs_1(VPos, Tol, Face, [], We).

get_vs_pairs_1([VPos|VposList0], Tol, Face, Acc, We) ->
    case closest_pair(VposList0, VPos, {none,Tol}, Face, We) of
        {_,Vb}=Pair ->
            VposList = VposList0 -- [Vb],
            get_vs_pairs_1(VposList, Tol, Face, [Pair|Acc], We);
        none ->
            get_vs_pairs_1(VposList0, Tol, Face, Acc, We)
    end;
get_vs_pairs_1([], _, _, Acc, _) ->
    Acc.


closest_pair([{Vb,PosB}|VposList], {Va,PosA}, {V0,D0}, Face, We) ->
    D1 = e3d_vec:dist(PosA,PosB),
    D = if
	    D1 =< D0 ->
		case wings_vertex:edge_through(Va, Vb, Face, We) of
		    none -> {{Vb,PosB},D1};
		    _other -> {V0,D0}
		end;
	    true -> {V0,D0}
	end,
    closest_pair(VposList, {Va,PosA}, D, Face, We);
closest_pair([], {Va,_}, {{Vb,_},_}, _Face, #we{}) -> {Va,Vb};
closest_pair([], _, _, _, _) -> none.

dissolve_edges([E|CommonEs],We0) ->
    case catch wings_edge:dissolve_edge(E,We0) of
	#we{}=We ->
	    dissolve_edges(CommonEs,We);
	_ -> We0
    end;
dissolve_edges([],We) ->
    We.

connect_and_collapse(Face, [{Va,Vb}|CPList], NVs, [], We0) ->
    {We1, NewFace} = wings_vertex:force_connect(Va, Vb, Face, We0),
    [E] = wings_we:new_items_as_ordset(edge, We0, We1),
    #we{fs=Ftab}=We2 = wings_collapse:collapse_edge(E, We1),
    case gb_trees:is_defined(Face,Ftab) of
	true ->
	    case gb_trees:is_defined(NewFace,Ftab) of
		true ->
		    NFVs = wings_face:to_vertices([NewFace], We2) -- NVs,
		    NFCPList = get_pairs_for_this_face(CPList,NFVs),
		    FCPList = CPList -- NFCPList,
		    connect_and_collapse(NewFace, NFCPList, NVs, [{Face,FCPList}], We2);
		false ->
		    connect_and_collapse(Face, CPList, NVs, [], We2)
	    end;
	false ->
	    case gb_trees:is_defined(NewFace,Ftab) of
		false -> We2;
		true ->
		    connect_and_collapse(NewFace, CPList, NVs, [], We2)
	    end
    end;
connect_and_collapse(Face, [{Va,Vb}|CPList], NVs, StoredFs, We0) ->
    {We1, NewFace} = wings_vertex:force_connect(Va, Vb, Face, We0),
    [E] = wings_we:new_items_as_ordset(edge, We0, We1),
    #we{fs=Ftab}=We2 = wings_collapse:collapse_edge(E, We1),
    case gb_trees:is_defined(Face,Ftab) of
	true ->
	    case gb_trees:is_defined(NewFace,Ftab) of
		true ->
		    NFVs = wings_face:to_vertices([NewFace], We2) -- NVs,
		    NFCPList = get_pairs_for_this_face(CPList,NFVs),
		    FCPList = CPList -- NFCPList,
		    F = {Face,FCPList},
		    connect_and_collapse(NewFace, NFCPList, NVs, [F|StoredFs], We2);
		false ->
		    connect_and_collapse(Face, CPList, NVs, StoredFs, We2)
	    end;
	false ->
	    case gb_trees:is_defined(NewFace,Ftab) of
		false ->
		    [{OldFace,OldCPList}|Rest] = StoredFs,
		    connect_and_collapse(OldFace, OldCPList, NVs, Rest, We2);
		true ->
		    connect_and_collapse(NewFace, CPList, NVs, StoredFs, We2)
	    end
    end.

-spec weld_error() -> no_return().
weld_error() ->
    wings_u:error_msg(?__(1,"Weld could not be resolved")).

%% Assume if Va is in the face then Vb is as well
get_pairs_for_this_face([{Va,Vb}|CPList],FVs) ->
    case lists:member(Va,FVs) of
	true -> [{Va,Vb}|get_pairs_for_this_face(CPList,FVs)];
	false -> get_pairs_for_this_face(CPList,FVs)
    end;
get_pairs_for_this_face([],_FVs) ->
    [].

try_weld_1(Fa, Fb, Tol, We0) ->
    N = wings_face:vertices(Fa, We0),
    IterA = wings_face:iterator(Fa, We0),
    {Va,_,_,_} = wings_face:next_cw(IterA),
    PosA = wings_vertex:pos(Va, We0),
    IterB0 = weld_synced_iterator(N, Fb, PosA, We0),
    case weld_same_positions(N, IterA, IterB0, Tol, We0) of
	false ->
	    We0;
	true ->
	    {Vb,_,_,_} = wings_face:next_ccw(IterB0),
	    We = wings_face_cmd:force_bridge(Fa, Va, Fb, Vb, We0),
	    Es = wings_we:new_items_as_ordset(edge, We0, We),
	    foldl(fun(E, W) ->
			  wings_collapse:collapse_edge(E, W)
		  end, We, Es)
    end.

weld_synced_iterator(N, Face, Pos, We) ->
    Iter = wings_face:iterator(Face, We),
    weld_synced_iterator_1(N, Iter, Pos, We, []).

weld_synced_iterator_1(0, _, _, _, Acc) ->
    [{_,Iter}|_] = sort(Acc),
    Iter;
weld_synced_iterator_1(N, Iter0, Pos, We, Acc) ->
    {V,_,_,Iter} = wings_face:next_ccw(Iter0),
    D = e3d_vec:dist(Pos, wings_vertex:pos(V, We)),
    weld_synced_iterator_1(N-1, Iter, Pos, We, [{D,Iter0}|Acc]).

weld_same_positions(0, _, _, _, _) -> true;
weld_same_positions(N, IterA0, IterB0, Tol, We) ->
    {Va,_,_,IterA} = wings_face:next_cw(IterA0),
    {Vb,_,_,IterB} = wings_face:next_ccw(IterB0),
    PosA = wings_vertex:pos(Va, We),
    PosB = wings_vertex:pos(Vb, We),
    case e3d_vec:dist(PosA, PosB) of
	D when abs(D) < Tol -> weld_same_positions(N-1, IterA, IterB, Tol, We);
	_D -> false
    end.

weld_selection(Fs, OldWe, We) ->
    weld_selection(Fs, OldWe, We, []).

weld_selection([F|Fs], OldWe, #we{fs=Ftab}=We, Acc) ->
    case gb_trees:is_defined(F, Ftab) of
	true ->
            weld_selection(Fs, OldWe, We, Acc);
	false ->
	    Vs = wings_face:vertices_ccw(F, OldWe),
	    weld_selection(Fs, OldWe, We, Vs++Acc)
    end;
weld_selection([], _, _, Acc) ->
    gb_sets:from_list(Acc).

%%%
%%% Set vertex color for selected objects.
%%%

set_color(Color, St) ->
    wings_sel:map(fun(_, We) ->
			  wings_va:set_body_color(Color, We)
		  end, St).
