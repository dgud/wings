%%
%%  wings_body.erl --
%%
%%     This module contains most of the command for entire Wings objects.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_body.erl,v 1.78 2005/01/23 07:55:33 bjorng Exp $
%%

-module(wings_body).
-export([menu/3,command/2]).
-export([auto_smooth/1]).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,seq/2]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    FlipStr = ?__(5,"Flip the object around") ++ " ",

    Menu = [{basic,{?__(1,"Object operations"),ignore}},
	    {basic,separator},
	    {?__(2,"Move"),{move,Dir}},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?__(3,"Flip"),
	     {flip,[{wings_s:dir(x),x,FlipStr ++ wings_s:dir_axis(x)},
		    {wings_s:dir(y),y,FlipStr ++ wings_s:dir_axis(y)},
		    {wings_s:dir(z),z,FlipStr ++ wings_s:dir_axis(z)}]}},
	    separator,
	    {?__(10,"Invert"),invert,
	     ?__(11,"Flip all normals, turning the object inside out")},
	    separator,
	    {?__(12,"Tighten"),tighten,
	     ?__(13,"Move vertices towards average midpoint")},
	    {?__(14,"Smooth"),smooth,
	     ?__(15,"Subdivide all faces to give the object a smoother apperance")},
	    {?__(16,"Combine"),combine,
	     ?__(17,"Combine multiple objects into a single object")},
	    {?__(18,"Separate"),separate,
	     ?__(19,"Separate a combined objects into its components")},
	    separator,
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
	    {?__(29,"Rename..."),rename,
	     ?__(30,"Rename selected objects")},
	    separator,
	    {?__(31,"Show All"),show_all,
	     ?__(32,"Show all faces for this object")}|mode_dependent(St)],
    wings_menu:popup_menu(X, Y, body, Menu).

mode_dependent(St) ->
    SelObj = wings_sel:fold(fun(_, We, A) -> [We|A] end, [], St),
    Area = foldl(fun(#we{has_shape=Shape}=We, A) ->
			 Type = if 
				    ?IS_ANY_LIGHT(We), Shape -> arealight;
				    true -> object
				end,
			 case A of
			     none -> Type;
			     Type -> Type;
			     _ -> mixed
			 end
		 end, none, SelObj),
    ObjMode = foldl(fun(#we{mode=M}, none) -> M;
		       (#we{mode=M}, M) -> M;
		       (_, _) -> mixed
		    end, none, SelObj),
    Tail0 = vertex_color_item(Area),
    Tail1 = mat_col_conv(ObjMode, Area, Tail0),
    Tail2 = arealight_conv(Area, Tail1),
    case mode_conv(ObjMode, Area, Tail2) of
	[] -> [];
	Tail -> [separator|Tail]
    end.

vertex_color_item(object) ->
    [{?__(1,"Vertex Color"),vertex_color,
      ?__(2,"Apply vertex colors to selected objects "
	   "(first changing mode to vertex color mode if needed)")}];
vertex_color_item(_) -> [].
    
mat_col_conv(vertex, object, T) ->
    [{?__(1,"Colors to Materials"),colors_to_materials,
      ?__(2,"Convert vertex colors to materials")}|T];
mat_col_conv(material, object, T) ->
    [{?__(3,"Materials to Colors"),materials_to_colors,
      ?__(4,"Convert materials to vertex colors")}|T];
mat_col_conv(_, _, T) -> T.

arealight_conv(arealight, T) ->
    [{?__(1,"Area Light to Object"),from_arealight,
      ?__(2,"Convert selected area lights to objects")}|T];
arealight_conv(object, T) ->
    [{?__(3,"Object To Area Light"),to_arealight,
      ?__(4,"Convert selected objects to area lights")}|T];
arealight_conv(mixed, T) -> T.

mode_conv(material, object, T) ->
    [{?__(1,"Vertex Color Mode"),vertex_color_mode,
      ?__(2,"Change object mode to vertex color mode "
	   "(materials will be kept, but not shown)")}|T];
mode_conv(vertex, object, T) ->
    [{?__(3,"Material Mode"),material_mode,
      ?__(4,"Change object mode to material mode "
	   "(vertex colors will be kept, but not shown)")}|T];
mode_conv(_, _, T) -> T.

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
    save_state,duplicate(Dir, St);
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
command(combine, St) ->
    {save_state,combine(St)};
command(separate, St) ->
    {save_state,separate(St)};
command(auto_smooth, St) ->
    auto_smooth(St);
command({auto_smooth,Ask}, St) ->
    auto_smooth(Ask, St);
command({flip,Plane}, St) ->
    {save_state,flip(Plane, St)};
command(cleanup, St) ->
    cleanup(false, St);
command({cleanup,Ask}, St) ->
    cleanup(Ask, St);
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command(rename, St) ->
    rename(St);
command({rename,Ids}, St) ->
    rename(Ids, St);
command(to_arealight, St) ->
    to_arealight(St);
command({to_arealight,Ids}, St) ->
    to_arealight(Ids, St);
command(from_arealight, St) ->
    from_arealight(St);
command({from_arealight,Ids}, St) ->
    from_arealight(Ids, St);
command(materials_to_colors, St) ->
    {save_state,materials_to_colors(St)};
command(colors_to_materials, St) ->
    {save_state,colors_to_materials(St)};
command(material_mode, St) ->
    {save_state,set_mode(material, St)};
command(vertex_color_mode, St) ->
    {save_state,set_mode(vertex, St)};
command({weld,Ask}, St) ->
    weld(Ask, St);
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
    wings_ask:dialog(Ask,
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
    Short = foldl(
	      fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
		      VaPos = gb_trees:get(Va, Vtab),
		      VbPos = gb_trees:get(Vb, Vtab),
		      case abs(e3d_vec:dist(VaPos, VbPos)) of
			  Dist when Dist < Tolerance -> [Edge|A];
			  _Dist -> A
		      end
	      end, [], gb_trees:to_list(Etab)),
    foldl(fun(Edge, #we{es=Et}=W) ->
		  case gb_trees:is_defined(Edge, Et) of
		      true -> wings_collapse:collapse_edge(Edge, W);
		      false -> W
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
    Rec = case gb_trees:get(Edge, Etab0) of
	      #edge{lf=Face}=Rec0 -> Rec0#edge{lf=NewFace};
	      #edge{rf=Face}=Rec0 -> Rec0#edge{rf=NewFace}
	  end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
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
    R = case gb_trees:get(From, Etab) of
	    #edge{lf=Face,ve=V}=R0 -> R0#edge{ltpr=To};
	    #edge{lf=Face,vs=V}=R0 -> R0#edge{ltsu=To};
	    #edge{rf=Face,vs=V}=R0 -> R0#edge{rtpr=To};
	    #edge{rf=Face,ve=V}=R0 -> R0#edge{rtsu=To}
	end,
    gb_trees:update(From, R, Etab).

%%
%% A waist is a vertex shared by edges all of which cannot be
%% reached from the incident edge of the vertex.
%%
cleanup_waists(#we{es=Etab,vp=Vtab}=We) ->
    VsEs0 = foldl(fun({E,#edge{vs=Va,ve=Vb}}, A) ->
			  [{Va,E},{Vb,E}|A]
		  end, [], gb_trees:to_list(Etab)),
    VsEs = wings_util:rel2fam(VsEs0),
    cleanup_waists_1(gb_trees:keys(Vtab), VsEs, We).

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
	    Vtab = gb_trees:insert(NewV, gb_trees:get(V, Vtab0), Vtab0),
	    Vct = gb_trees:insert(NewV, AnEdge, Vct0),
	    We = We1#we{es=Etab,vp=Vtab,vc=Vct},
	    io:format(?__(1,"Removed waist vertex: ~p\n"), [V]),

	    %% Re-process the newly added vertex. (Some of the
	    %% edges may not be reachable from the incident edge of
	    %% the new vertex.)
	    cleanup_waists_1([NewV|Vs], [{NewV,Es}|VsEs], We)
    end;
cleanup_waists_1([], [], We) -> We.

patch_vtx_refs([E|Es], OldV, NewV, Etab0) ->
    Etab = case gb_trees:get(E, Etab0) of
	       #edge{vs=OldV}=Rec ->
		   gb_trees:update(E, Rec#edge{vs=NewV}, Etab0);
	       #edge{ve=OldV}=Rec ->
		   gb_trees:update(E, Rec#edge{ve=NewV}, Etab0)
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
	    wings_u:error(?__(1,"Face") ++ integer_to_list(Face) ++
			     ?__(2,"has only one edge"));
	We -> delete_2edged_faces_1(Faces, We)
    end;
delete_2edged_faces_1([], We) -> We.

delete_if_bad(Face, #we{fs=Ftab,es=Etab}=We) ->
    case gb_trees:lookup(Face, Ftab) of
	{value,Edge} ->
	    case gb_trees:get(Edge, Etab) of
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

duplicate(Dir, #st{onext=Oid0}=St0) ->
    St1 = wings_sel:fold(fun(_, We, St) ->
				 wings_shape:insert(We, copy, St)
			 end, St0, St0),
    %% Select the duplicate items, not the original items.
    Zero = gb_sets:singleton(0),
    Sel = [{Id,Zero} || Id <- seq(Oid0, St1#st.onext-1)],
    St = wings_sel:set(Sel, St1),
    case Dir of
	none -> St;
	_ -> wings_move:setup(Dir, St)
    end.

%%%
%%% Duplicate called from the Outliner or Object window.
%%%

duplicate_object(Objects, #st{shapes=Shs}=St) ->
    foldl(fun(Id, S) ->
		  We = gb_trees:get(Id, Shs),
		  wings_shape:insert(We, copy, S)
	  end, St, Objects).

%%%
%%% The Delete command.
%%%

delete(#st{shapes=Shapes0}=St) ->
    Shapes = wings_sel:fold(fun(_, #we{id=Id}, Shs) ->
				    gb_trees:delete(Id, Shs)
			    end, Shapes0, St),
    St#st{shapes=Shapes,sel=[]}.

%%%
%%% Delete called from the Outliner or Object window.
%%%

delete_object(Objects, #st{shapes=Shs0}=St) ->
    Shs = foldl(fun(Id, Shs) ->
			gb_trees:delete(Id, Shs)
		end, Shs0, Objects),
    wings_sel:valid_sel(St#st{shapes=Shs}).

%%%
%%% The Flip command
%%%

flip(Plane0, St) ->
    Plane = flip_scale(Plane0),
    wings_sel:map(fun(_, We) -> flip_body(Plane, We) end, St).

flip_body(Plane, We0) ->
    {Cx,Cy,Cz} = e3d_vec:average(wings_vertex:bounding_box(We0)),
    M0 = e3d_mat:translate(Cx, Cy, Cz),
    M1 = e3d_mat:mul(M0, Plane),
    M = e3d_mat:mul(M1, e3d_mat:translate(-Cx, -Cy, -Cz)),
    We = wings_we:transform_vs(M, We0),
    wings_we:invert_normals(We).

flip_scale(x) -> e3d_mat:scale(-1.0, 1.0, 1.0);
flip_scale(y) -> e3d_mat:scale(1.0, -1.0, 1.0);
flip_scale(z) -> e3d_mat:scale(1.0, 1.0, -1.0).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(_, #we{vp=Vtab}=We, A) ->
    Vs = gb_trees:keys(Vtab),
    wings_vertex_cmd:tighten(Vs, We, A).
    
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
combine(#st{shapes=Shs0,sel=[{Id,_}=S|_]=Sel0}=St) ->
    Shs1 = sofs:from_external(gb_trees:to_list(Shs0), [{id,object}]),
    Sel1 = sofs:from_external(Sel0, [{id,dummy}]),
    Sel2 = sofs:domain(Sel1),
    {Wes0,Shs2} = sofs:partition(1, Shs1, Sel2),
    Wes = sofs:to_external(sofs:range(Wes0)),
    Mode = unify_modes(Wes),
    We0 = wings_we:merge(Wes),
    We = We0#we{id=Id,mode=Mode},
    Shs = gb_trees:from_orddict(sort([{Id,We}|sofs:to_external(Shs2)])),
    St#st{shapes=Shs,sel=[S]}.

unify_modes([#we{mode=Mode}|Wes]) ->
    unify_modes(Wes, Mode).

unify_modes([#we{mode=Mode}|Wes], Mode) ->
    unify_modes(Wes, Mode);
unify_modes([_|_], _) ->
    wings_u:error(?__(1,
			 "Objects with vertex colors cannot be combined " 
			 "with objects with materials."));
unify_modes([], Mode) -> Mode.
		    
%%%
%%% The Separate command.
%%%

separate(St) ->
    wings_sel:fold(
      fun(_, #we{id=Id}=We0, St0) ->
	      case wings_we:separate(We0) of
		  [_] -> St0;
		  [We|Wes] ->
		      St1 = foldl(fun(W, A) ->
					  wings_shape:insert(W, sep, A)
				  end, St0, Wes),
		      wings_shape:replace(Id, We, St1)
	      end
      end, St, St).

%%%
%%% The Auto-Smooth command.
%%%

auto_smooth(St) ->
    do_auto_smooth(60, St).

auto_smooth(Ask, _) when is_atom(Ask) ->
    wings_ask:ask(Ask,?__(1,"Auto Smooth Parameters"),
		  [{?__(2,"Crease Angle"),60,[{range,{0,180}}]}],
		  fun(Res) -> {body,{auto_smooth,Res}} end);
auto_smooth([Angle], St) ->
    {save_state,do_auto_smooth(Angle, St)}.

do_auto_smooth(Angle, St) ->
    Cos = cos_degrees(Angle),
    wings_sel:map(fun(_, We) -> auto_smooth_1(Cos, We) end, St).

auto_smooth_1(Cos, #we{es=Etab,he=Htab0}=We) ->
    Htab = foldl(fun({E,R}, A) ->
			 auto_smooth(E, R, Cos, A, We)
		 end, Htab0, gb_trees:to_list(Etab)),
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

rename(St) ->
    Wes = wings_sel:fold(fun(_, We, A) -> [We|A] end, [], St),
    rename_1(Wes, St).

rename(Objects, #st{shapes=Shs}=St) ->
    Wes = foldl(fun(Id, A) -> [gb_trees:get(Id, Shs)|A] end, [], Objects),
    rename_1(Wes, St).

rename_1(Wes, St) ->
    Qs = rename_qs(Wes),
	wings_ask:dialog(?__(1,"Rename"), Qs,
			 fun(NewNames) ->
				 rename_1(NewNames, Wes, St)
			 end).

rename_1(Names, Wes, #st{shapes=Shs}=St) ->
    rename_2(Names, Wes, Shs, St).

rename_2([N|Ns], [#we{id=Id}=We|Wes], Shs0, St) ->
    Shs = gb_trees:update(Id, We#we{name=N}, Shs0),
    rename_2(Ns, Wes, Shs, St);
rename_2([], [], Shs, St) -> St#st{shapes=Shs}.

rename_qs(Wes) ->
    OldNames = [{label,Name} || #we{name=Name} <- Wes],
    TextFields = [{text,Name,[]} || #we{name=Name} <- Wes],
    [{hframe,
      [{vframe,OldNames},
       {vframe,TextFields}]}].

%%%
%%% Convert selected objects to area lights
%%%
to_arealight(#st{shapes=Shs}=St) ->
    Wes = wings_sel:fold(fun(_, We, A) -> [We|A] end, [], St),
    to_arealight_1(Wes, Shs, St).

to_arealight(Objects, #st{shapes=Shs}=St) ->
    Wes = foldl(fun(Id, A) -> [gb_trees:get(Id, Shs)|A] end, [], Objects),
    to_arealight_1(Wes, Shs, St).

to_arealight_1([], Shs, St) -> 
    St#st{shapes=Shs};
to_arealight_1([We0|Wes], Shs, St) when ?IS_ANY_LIGHT(We0) ->
    to_arealight_1(Wes, Shs, St);
to_arealight_1([#we{id=Id}=We0|Wes], Shs, St) ->
    #we{light=Light,has_shape=HasShape} = 
	wings_light:import([{opengl,[{type,area}]}]),
    We = We0#we{light=Light,has_shape=HasShape},
    to_arealight_1(Wes, gb_trees:update(Id, We, Shs), St).



%%%
%%% Convert selected area lights to objects
%%%
from_arealight(#st{shapes=Shs}=St) ->
    Wes = wings_sel:fold(
	    fun (_, #we{has_shape=HasShape}=We, A) 
		when ?IS_ANY_LIGHT(We), HasShape -> 
		    [We|A];
		(_, _, A) ->
		    A
	    end, [], St),
    from_arealight_1(Wes, Shs, St).

from_arealight(Objects, #st{shapes=Shs}=St) ->
    Wes = foldl(fun(Id, A) -> [gb_trees:get(Id, Shs)|A] end, [], Objects),
    from_arealight_1(Wes, Shs, St).

from_arealight_1([], Shs, St) -> 
    St#st{shapes=Shs};
from_arealight_1([#we{id=Id,has_shape=HasShape}=We|Wes], Shs, St)
    when ?IS_ANY_LIGHT(We), HasShape ->
    from_arealight_1(Wes, gb_trees:update(Id, We#we{light=none}, Shs), St);
from_arealight_1([We|Wes], Shs, St) 
  when ?IS_ANY_LIGHT(We) ->
    to_arealight_1(Wes, Shs, St).



%%%
%%% Set Mode.
%%%

set_mode(Mode, St) ->
    wings_sel:map(fun(_, We) -> We#we{mode=Mode} end, St).

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

colors_to_materials_1(#we{mode=vertex,fs=Ftab}=We0, St) ->
    colors_to_materials_2(gb_trees:keys(Ftab), We0#we{mode=material}, [], St);
colors_to_materials_1(We, St) -> {We,St}.

colors_to_materials_2([F|Fs], We, Acc, St0) ->
    Colors = [C || [_|C] <- wings_face:vinfo_ccw(F, We)],
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
%%% The Weld command.
%%%

weld(Ask, _) when is_atom(Ask) ->
    Qs = [{hframe,
	   [{label,?__(1,"Distance Tolerance")},
	    {text,1.0E-3,[{range,{1.0E-5,10.0}}]}]}],
    wings_ask:dialog(Ask, ?__(2,"Weld"), Qs,
		     fun(Res) -> {body,{weld,Res}} end);
weld([Tolerance], St0) ->
    St1 = combine(St0),
    {St2,Sel} = wings_sel:mapfold(fun(_, We, Acc) ->
					  weld_1(Tolerance, We, Acc)
				  end, [], St1),
    St = wings_sel:set(vertex, Sel, St2),
    {save_state,wings_sel:valid_sel(St)}.

weld_1(Tol, #we{id=Id,fs=Fs0}=We0, Acc) ->
    Fs = weld_1_list(gb_trees:keys(Fs0), Tol, We0, []),
    R = sofs:relation(Fs, [{key,face}]),
    F = sofs:relation_to_family(R),
    Part0 = sofs:range(F),
    Part1 = sofs:specification({external,fun([_]) -> false;
					    (_) -> true end}, Part0),
    Part = sofs:to_external(Part1),
    case weld_2(Part, Tol, We0) of
	We0 ->
	    wings_u:error(?__(1,"Found no faces to weld."));
	We ->
	    {We,[{Id,weld_selection(lists:append(Part), We0, We)}|Acc]}
    end.

weld_1_list([F|Fs], Tol, We, Acc) ->
    Vs = wings_face:fold(
	   fun(V, _, _, Acc0) ->
		   [V|Acc0]
	   end, [], F, We),
    {X,Y,Z} = wings_vertex:center(Vs, We),
    Center = {granularize(X, Tol),granularize(Y, Tol),granularize(Z, Tol)},
    weld_1_list(Fs, Tol, We, [{{length(Vs),Center},F}|Acc]);
weld_1_list([], _, _, Acc) -> Acc.

granularize(F, Tol) -> Tol*round(F/Tol).

weld_2([P|Ps], Tol, We0) ->
    We = weld_part(P, Tol, We0),
    weld_2(Ps, Tol, We);
weld_2([], _, We) -> We.

weld_part([F|Fs], Tol, We) ->
    weld_part_1(F, Fs, Tol, We, []);
weld_part([], _, We) -> We.

weld_part_1(Fa, [Fb|Fs], Tol, We0, Acc) ->
    case try_weld(Fa, Fb, Tol, We0) of
	no -> weld_part_1(Fa, Fs, Tol, We0, [Fb|Acc]);	
	We -> weld_part(Fs++Acc, Tol, We)
    end;
weld_part_1(_, [], Tol, We, Acc) ->
    weld_part(Acc, Tol, We).

try_weld(Fa, Fb, Tol, We) ->
    case wings_face:are_neighbors(Fa, Fb, We) of
	true -> no;
	false ->
	    Na = wings_face:normal(Fa, We),
	    Nb = wings_face:normal(Fb, We),
	    case e3d_vec:dot(Na, Nb) of
		Dot when Dot < -0.99 ->
		    try_weld_1(Fa, Fb, Tol, We);
		_Dot -> no
	    end
    end.

try_weld_1(Fa, Fb, Tol, We0) ->
    N = wings_face:vertices(Fa, We0),
    IterA = wings_face:iterator(Fa, We0),
    {Va,_,_,_} = wings_face:next_cw(IterA),
    PosA = wings_vertex:pos(Va, We0),
    IterB0 = weld_synced_iterator(N, Fb, PosA, We0),
    case weld_same_positions(N, IterA, IterB0, Tol, We0) of
	false -> no;
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
	true -> weld_selection(Fs, OldWe, We, Acc);
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
			  set_color_1(Color, We)
		  end, St).

set_color_1(Color, #we{es=Etab0}=We) ->
    Etab1 = foldl(fun({E,Rec}, A) ->
			  [{E,Rec#edge{a=Color,b=Color}}|A]
		  end, [], gb_trees:to_list(Etab0)),
    Etab = gb_trees:from_orddict(reverse(Etab1)),
    We#we{es=Etab,mode=vertex}.
