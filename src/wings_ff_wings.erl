%%
%%  wings_ff_wings.erl --
%%
%%     This module contain the functions for reading and writing .wings files.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_ff_wings.erl,v 1.66 2006/01/26 20:37:33 dgud Exp $
%%

-module(wings_ff_wings).
-export([import/2,export/2]).

-include("wings.hrl").
-include("e3d_image.hrl").
-import(lists, [sort/1,reverse/1,foldl/3]).

-define(WINGS_HEADER, "#!WINGS-1.0\r\n\032\04").

%% Load a wings file.

import(Name, St) ->
    wings_pb:start(?__(1,"opening wings file")),
    wings_pb:update(0.07, ?__(2,"reading file")),
    wings_pb:done(import_1(Name, St)).

import_1(Name, St0) ->
    case file:read_file(Name) of
	{ok,<<?WINGS_HEADER,Sz:32,Data/binary>>} when size(Data) =:= Sz ->
	    wings_pb:update(0.08, ?__(1,"converting binary")),
	    case catch binary_to_term(Data) of
		{wings,0,_Shapes} ->
                    {error, ?__(2,"Pre-0.80 Wings format no longer supported.")};
		{wings,1,_,_,_} ->
		    %% Pre-0.92. No longer supported.
                    {error,?__(3,"Pre-0.92 Wings format no longer supported.")};
		{wings,2,{Shapes,Materials,Props}} ->
                    import_vsn2(Shapes, Materials, Props, St0);
		{wings,_,_} ->
		    {error,?__(4,"unknown wings format")};
		Other ->
		    io:format("~P\n", [Other,20]),
                    {error,?__(5,"corrupt Wings file")}
	    end;
	{ok,_Bin} ->
	    {error,?__(6,"not a Wings file (or old Wings format)")};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_vsn2(Shapes, Materials0, Props, St0) ->
    wings_pb:update(0.10, ?__(1,"images and materials")),
    Images = import_images(Props),
    Materials1 = translate_materials(Materials0),
    Materials  = translate_map_images(Materials1, Images),
    {St1,NameMap0} = wings_material:add_materials(Materials, St0),
    NameMap1 = gb_trees:from_orddict(sort(NameMap0)),
    NameMap = optimize_name_map(Materials, NameMap1, []),
    St = import_props(Props, St1),
    wings_pb:update(1.0,?__(2,"objects")),
    import_objects(Shapes, NameMap, St).

optimize_name_map([{Name,_}|Ms], NameMap, Acc) ->
    case gb_trees:lookup(Name, NameMap) of
	none ->
	    optimize_name_map(Ms, NameMap, [{Name,Name}|Acc]);
	{value,NewName} ->
	    optimize_name_map(Ms, NameMap, [{Name,NewName}|Acc])
    end;
optimize_name_map([], _, Acc) -> gb_trees:from_orddict(sort(Acc)).

import_objects(Shapes, NameMap, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Oid} = import_objects(Shapes, Mode, NameMap, Oid0, []),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ Objs),
    St#st{shapes=Shs,onext=Oid}.

import_objects([Sh0|Shs], Mode, NameMap, Oid, ShAcc) ->
    {object,Name,{winged,Es,Fs,Vs,He},Props} = Sh0,
    ObjMode = import_object_mode(Props),
    Etab = import_edges(Es, #edge{}, 0, []),
    FaceMat = import_face_mat(Fs, NameMap, 0, []),
    Vtab = import_vs(Vs, 0, []),
    Htab = gb_sets:from_list(He),
    Perm = import_perm(Props),
    Mirror = proplists:get_value(mirror_face, Props, none),
    Pst0 = proplists:get_value(plugin_states, Props, []),
    Pst = try gb_trees:from_orddict(Pst0)
	  catch error:_ -> gb_trees:empty()
	  end,
    We = #we{es=Etab,vp=Vtab,he=Htab,perm=Perm,pst=Pst,
	     id=Oid,name=Name,mode=ObjMode,mirror=Mirror,mat=FaceMat},
    HiddenFaces = proplists:get_value(num_hidden_faces, Props, 0),
    import_objects(Shs, Mode, NameMap, Oid+1, [{HiddenFaces,We}|ShAcc]);
import_objects([], _Mode, _NameMap, Oid, Objs0) ->
    %%io:format("flat_size: ~p\n", [erts_debug:flat_size(Objs0)]),
    Objs = share_list(Objs0),
    %%io:format("size: ~p\n", [erts_debug:size(Objs)]),
    {Objs,Oid}.
    
import_edges([E|Es], Template, Edge, Acc) ->
    Rec = import_edge(E, Template),
    import_edges(Es, Template, Edge+1, [{Edge,Rec}|Acc]);
import_edges([], _Template, _Edge, Acc) -> reverse(Acc).

import_edge([{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}|T], Rec0) ->
    Rec = Rec0#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
		    ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu},
    import_edge(T, Rec);
import_edge([{uv_lt,<<U/float,V/float>>}|T], Rec) ->
    import_edge(T, Rec#edge{a={U,V}});
import_edge([{uv_rt,<<U/float,V/float>>}|T], Rec) ->
    import_edge(T, Rec#edge{b={U,V}});
import_edge([{color_lt,<<R:32/float,G:32/float,B:32/float>>}|T], Rec) ->
    import_edge(T, Rec#edge{a={R,G,B}});
import_edge([{color_rt,<<R:32/float,G:32/float,B:32/float>>}|T], Rec) ->
    import_edge(T, Rec#edge{b={R,G,B}});
import_edge([{color,Bin}|T], Rec) ->
    %% Old-style vertex colors (pre 0.98.15).
    <<R1/float,G1/float,B1/float,R2/float,G2/float,B2/float>> = Bin,
    import_edge(T, Rec#edge{a={R1,G1,B1},b={R2,G2,B2}});
import_edge([{uv,Bin}|T], Rec) ->
    %% Old-style UV coordinates (pre 0.98.15).
    <<U1/float,V1/float,U2/float,V2/float>> = Bin,
    import_edge(T, Rec#edge{a={U1,V1},b={U2,V2}});
import_edge([_|T], Rec) ->
    import_edge(T, Rec);
import_edge([], Rec) -> Rec.

import_face_mat([F|Fs], NameMap, Face, Acc) ->
    Mat = import_face_mat_1(F, NameMap, default),
    import_face_mat(Fs, NameMap, Face+1, [{Face,Mat}|Acc]);
import_face_mat([], _, _, Acc) -> reverse(Acc).

import_face_mat_1([{material,Name}|T], NameMap, _) ->
    %% Silently ignore materials not found in the name map.
    Mat = case gb_trees:lookup(Name, NameMap) of
	      none ->  default;
	      {value,Other} -> Other
	  end,
    import_face_mat_1(T, NameMap, Mat);
import_face_mat_1([_|T], NameMap, Mat) ->
    import_face_mat_1(T, NameMap, Mat);
import_face_mat_1([], _, Mat) -> Mat.

import_vs([Vtx|Vs], V, Acc) -> 
    Rec = import_vertex(Vtx, []),
    import_vs(Vs, V+1, [{V,Rec}|Acc]);
import_vs([], _V, Acc) -> reverse(Acc).

import_vertex([<<X/float,Y/float,Z/float>>|T], _) ->
    import_vertex(T, {X,Y,Z});
import_vertex([_|T], Rec) ->
    import_vertex(T, Rec);
import_vertex([], Rec) -> Rec.

import_perm(Props) ->
    case proplists:get_value(state, Props) of
	undefined -> 0;
	locked -> 1;
	hidden -> 2;
	hidden_locked -> 3;
	{hidden,Mode,Set} -> {Mode,gb_sets:from_list(Set)};
	_Unknown -> 0
    end.

import_object_mode(Ps) ->
    case proplists:get_value(mode, Ps, material) of
	undefined ->
	    io:format(?__(1,"Changed undefined mode to material\n")),
	    material;
	uv -> material;
	Other -> Other
    end.

import_props([{selection,{Mode,Sel0}}|Ps], St) ->
    Sel = import_sel(Sel0, St),
    import_props(Ps, St#st{selmode=Mode,sel=Sel});
import_props([{saved_selection,{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group(?__(1,"<Stored Selection>"), Mode, Sel, St0),
    import_props(Ps, St);
import_props([{{selection_group,Name},{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group(Name, Mode, Sel, St0),
    import_props(Ps, St);
import_props([{lights,Lights}|Ps], St0) ->
    St = wings_light:import(Lights, St0),
    import_props(Ps, St);
import_props([{views,Views}|Ps], St0) ->
    St = wings_view:import_views(Views, St0),
    import_props(Ps, St);
import_props([{current_view,CurrentView}|Ps], #st{views={_,Views}}=St) ->
    import_props(Ps, St#st{views={CurrentView,Views}});
import_props([{palette,Palette}|Ps], St) ->
    import_props(Ps, St#st{pal=Palette});
import_props([{scene_prefs,ScenePrefs}|Ps], St) ->
    lists:foreach(fun({Key,Val}) ->
			  wings_pref:set_scene_value(Key, Val)
		  end,
		  ScenePrefs),
    import_props(Ps, St);
import_props([{plugin_states,Pst0}|Ps], St0 =#st{pst=Previous}) ->
    St = try 
	     case gb_trees:keys(Previous) of
		 [] -> 
		     Pst = gb_trees:from_orddict(lists:sort(Pst0)),
		     St0#st{pst=Pst};
		 _ when Pst0 =:= [] -> 
		     St0;
		 PrevKeys ->
		     M=fun({Mod,Data},Acc) ->
			       case lists:member(Mod,PrevKeys) of
				   true -> 
				       try
					   Pst = Mod:merge_st(Data,St0),
					   [{Mod,Pst}|lists:keydelete(Mod,1,Acc)]
				       catch _:_ -> Acc
				       end;
				   false ->
				       [{Mod,Data}|Acc]
			       end
		       end,
		     Pst1 = lists:foldl(M,gb_trees:to_list(Previous),Pst0),
		     Pst  = gb_trees:from_orddict(lists:sort(Pst1)),
		     St0#st{pst=Pst}
	     end
	 catch error:Reason -> 
		 io:format("Failed importing plugins state Not a gb_tree ~p ~n",
			   [Reason]),
		 St0
	 end,
    import_props(Ps,St);
import_props([_|Ps], St) ->
    import_props(Ps, St);
import_props([], St) -> St.

import_sel(Sel, #st{onext=IdBase}) ->
    [{IdBase+Id,gb_sets:from_list(Elems)} || {Id,Elems} <- Sel].

new_sel_group(Name, Mode, Sel, #st{ssels=Ssels0}=St) ->
    Key = {Mode,Name},
    case gb_trees:is_defined(Key, Ssels0) of
	true -> St;
	false ->
	    Ssels = gb_trees:insert(Key, Sel, Ssels0),
	    St#st{ssels=Ssels}
    end.

import_images(Props) ->
    Empty = gb_trees:empty(),
    case proplists:get_value(images, Props) of
	undefined -> Empty;
	Images -> import_images_1(Images, Empty)
    end.
	    
import_images_1([{Id0,Im}|T], Map) ->
    try 
	#e3d_image{name=Name} = E3D = import_image(Im),
	Id = wings_image:new(Name, E3D),
	import_images_1(T, gb_trees:insert(Id0, Id, Map))
    catch
	throw:{bad_image,Image} -> 
	    E3d = #e3d_image{name=Image,width=1,height=1,image= <<0,0,0>>},
	    ID = wings_image:new(Image, E3d),
	    import_images_1(T, gb_trees:insert(Id0, ID, Map))
    end;
import_images_1([], Map) -> Map.

import_image(Im) ->
    Name = proplists:get_value(name, Im, ?__(1,"unnamed image")),
    case proplists:get_value(filename, Im) of
	undefined ->
	    W = proplists:get_value(width, Im, 0),
	    H = proplists:get_value(height, Im, 0),
	    PP = proplists:get_value(samples_per_pixel, Im, 0),
	    Pixels = proplists:get_value(pixels, Im),
	    if
		W*H*PP =:= size(Pixels) -> 
		    ok;
		true -> 
		    Str = io_lib:format(?__(2,"Bad image: ~p\n"), [Name]),
		    wings_u:message(lists:flatten(Str)),
		    throw({bad_image,Name})
	    end,
	    MaskSize = proplists:get_value(mask_size, Im),
	    Type = case PP of
		       1 when MaskSize =:= 1 -> a8;
		       1 -> g8;
		       2 -> g8a8;
		       3 -> r8g8b8;
		       4 -> r8g8b8a8
		   end,
	    #e3d_image{name=Name,width=W,height=H,type=Type,order=lower_left,
		       alignment=1,bytes_pp=PP,image=Pixels};
	Filename ->
	    Ps = [{filename,Filename}],
	    case wings_image:image_read(Ps) of
		#e3d_image{}=E3D ->
		    E3D#e3d_image{name=Name,filename=Filename};
		{error,_} ->
		    Str = io_lib:format(?__(2,"Bad image: ~p\n"), [Name]),
		    wings_u:message(lists:flatten(Str)),
		    throw({bad_image,Name})
	    end
    end.

translate_map_images(Mats, ImMap) ->
    [translate_map_images_1(M, ImMap) || M <- Mats].

translate_map_images_1({Name,Props0}=Mat, ImMap) ->
    case proplists:get_value(maps, Props0, []) of
	[] -> Mat;
	Maps ->
	    Props = lists:keydelete(maps, 1, Props0),
	    {Name,[{maps,translate_map_images_2(Maps, Name, ImMap)}|Props]}
    end.

translate_map_images_2([{Type,Im0}|T], Mat, ImMap) when is_integer(Im0) ->
    case gb_trees:lookup(Im0, ImMap) of
	none ->
	    %% Something wrong here.
	    io:format( ?__(1,"Material ~p, ~p texture: reference to non-existing image ~p\n"),
		       [Mat,Type,Im0]),
	    translate_map_images_2(T, Mat, ImMap);
	{value,Im} ->
	    if Type == normal -> wings_image:is_normalmap(Im);
	       true -> ok
	    end,
	    [{Type,Im}|translate_map_images_2(T, Mat, ImMap)]
    end;
translate_map_images_2([H|T], Mat, ImMap) ->
    [H|translate_map_images_2(T, Mat, ImMap)];
translate_map_images_2([], _, _) -> [].

%%%
%%% Sharing of floating point numbers on import.
%%%

share_list(Wes) ->
    Tabs0 = [{Vtab,Etab} || {_,#we{vp=Vtab,es=Etab}} <- Wes],
    Floats = share_floats(Tabs0, tuple_to_list(wings_color:white())),
    Tabs = share_list_1(Tabs0, Floats, gb_trees:empty(), []),
    share_list_2(Tabs, Wes, []).

share_list_1([{Vtab0,Etab0}|Ts], Floats, Tuples0, Acc) ->
    Vtab = share_vs(Vtab0, Floats, []),
    {Etab,Tuples} = share_es(Etab0, Floats, [], Tuples0),
    share_list_1(Ts, Floats, Tuples, [{Vtab,Etab}|Acc]);
share_list_1([], _, _, Ts) -> reverse(Ts).

share_list_2([{Vtab0,Etab0}|Ts],
	     [{NumHidden,#we{id=Id,mat=FaceMat}=We0}|Wes], Acc) ->
    Vtab = gb_trees:from_orddict(Vtab0),
    Etab = gb_trees:from_orddict(Etab0),
    We1 = wings_we:rebuild(We0#we{vp=Vtab,es=Etab,mat=default}),
    We2 = wings_facemat:assign(FaceMat, We1),
    We = if
	     NumHidden =:= 0 -> We2;
	     true ->
		 Hidden = lists:seq(0, NumHidden-1),
		 wings_we:hide_faces(Hidden, We2)
	 end,
    share_list_2(Ts, Wes, [{Id,We}|Acc]);
share_list_2([], [], Wes) -> sort(Wes).

share_floats([{Vtab,Etab}|T], Shared0) ->
    Shared1 = share_floats_1(Vtab, Shared0),
    Shared = share_floats_2(Etab, Shared1),
    share_floats(T, Shared);
share_floats([], Shared0) ->
    Shared1 = ordsets:from_list(Shared0),
    Shared = share_floats_4(Shared1, []),
    gb_trees:from_orddict(Shared).

share_floats_1([{_,{A,B,C}}|T], Shared) ->
    share_floats_1(T, [A,B,C|Shared]);
share_floats_1([], Shared) -> Shared.

share_floats_2([{_,#edge{a=A,b=B}}|T], Shared0) ->
    Shared1 = share_floats_3(A, Shared0),
    Shared = share_floats_3(B, Shared1),
    share_floats_2(T, Shared);
share_floats_2([], Shared) -> Shared.

share_floats_3({A,B}, [A,B|_]=Shared) -> Shared;
share_floats_3({A,B,C}, [A,B,C|_]=Shared) -> Shared;
share_floats_3({A,B}, Shared) -> [A,B|Shared];
share_floats_3({A,B,C}, Shared) -> [A,B,C|Shared];
share_floats_3(none, Shared) -> Shared.

share_floats_4([F|Fs], Acc) ->
    share_floats_4(Fs, [{F,F}|Acc]);
share_floats_4([], Acc) -> reverse(Acc).

share_vs([{V,{X0,Y0,Z0}}|Vs], Floats, Acc) ->
    X = gb_trees:get(X0, Floats),
    Y = gb_trees:get(Y0, Floats),
    Z = gb_trees:get(Z0, Floats),
    share_vs(Vs, Floats, [{V,{X,Y,Z}}|Acc]);
share_vs([], _, Acc) -> reverse(Acc).

share_es([{E,#edge{a=Same0,b=Same0}=Rec}|Vs], Floats, Acc, Shared0) ->
    {Same,Shared} = share_tuple(Same0, Floats, Shared0),
    share_es(Vs, Floats, [{E,Rec#edge{a=Same,b=Same}}|Acc], Shared);
share_es([{E,#edge{a=A0,b=B0}=Rec}|Vs], Floats, Acc, Shared0) ->
    {A,Shared1} = share_tuple(A0, Floats, Shared0),
    {B,Shared} = share_tuple(B0, Floats, Shared1),
    share_es(Vs, Floats, [{E,Rec#edge{a=A,b=B}}|Acc], Shared);
share_es([], _, Acc, Shared) -> {reverse(Acc),Shared}.

share_tuple({A0,B0}=Tuple0, Floats, Shared) ->
    case gb_trees:lookup(Tuple0, Shared) of
	none ->
	    A = gb_trees:get(A0, Floats),
	    B = gb_trees:get(B0, Floats),
	    Tuple = {A,B},
	    {Tuple,gb_trees:insert(Tuple, Tuple, Shared)};
	{value,Tuple} -> {Tuple,Shared}
    end;
share_tuple({A0,B0,C0}=Tuple0, Floats, Shared) ->
    case gb_trees:lookup(Tuple0, Shared) of
	none ->
	    A = gb_trees:get(A0, Floats),
	    B = gb_trees:get(B0, Floats),
	    C = gb_trees:get(C0, Floats),
	    Tuple = {A,B,C},
	    {Tuple,gb_trees:insert(Tuple, Tuple, Shared)};
	{value,Tuple} -> {Tuple,Shared}
    end;
share_tuple(none, _, Shared) -> {none,Shared}.

%%%
%%% Import of old materials format (up to and including wings-0.94.02).
%%%

translate_materials(Mats) ->
    [translate_material(M) || M <- Mats].
    
translate_material({Name,Props}=Mat) ->
    case proplists:is_defined(opengl, Props) of
	true -> Mat;
	false ->
	    Opac = proplists:get_value(opacity, Props),
	    {Name,translate_material(Props, Opac, [], [])}
    end.

translate_material([Mat|Mats], Opac, OpenGL, Maps) ->
    case Mat of
	{diffuse_map,Map} ->
	    translate_material(Mats, Opac, OpenGL, [{diffuse,Map}|Maps]);
	{diffuse,_}=Diff ->
	    translate_material(Mats, Opac, [trans(Diff, Opac)|OpenGL], Maps);
	{ambient,_}=Amb ->
	    translate_material(Mats, Opac, [trans(Amb, Opac)|OpenGL], Maps);
	{specular,_}=Spec ->
	    translate_material(Mats, Opac, [trans(Spec, Opac)|OpenGL], Maps);
	{shininess,Sh} ->
	    translate_material(Mats, Opac, [{shininess,1.0-Sh}|OpenGL], Maps);
	_ ->
	    translate_material(Mats, OpenGL, Opac, Maps)
    end;
translate_material([], _, OpenGL, Maps) ->
    [{opengl,OpenGL},{maps,Maps}].

trans({Key,{R,G,B}}, Opac) -> {Key,{R,G,B,Opac}}.
    
%%%
%%% Save a Wings file (in version 2).
%%%

export(Name, St0) ->
    wings_pb:start( ?__(1,"saving")),
    wings_pb:update(0.01, ?__(2,"lights")),
    Lights = wings_light:export(St0),
    Materials = wings_material:used_materials(St0),
    #st{shapes=Shs0,views={CurrentView,_}} = St = 
	remove_lights(St0),
    Sel0 = collect_sel(St),
    wings_pb:update(0.65, ?__(3,"renumbering")),
    {Shs1,Sel} = renumber(gb_trees:to_list(Shs0), Sel0, 0, [], []),
    Shs = foldl(fun shape/2, [], Shs1),
    wings_pb:update(0.98, ?__(4,"objects")),
    Props0 = export_props(Sel),
    Props1 = case Lights of
		 [] -> Props0;
		 [_|_] -> [{lights,Lights}|Props0]
	     end,
    Props2 = case export_images() of
		[] -> Props1;
		Images -> [{images,Images}|Props1]
	     end,
    Props3 = case wings_view:export_views(St) of
		 [] -> Props2;
		 Views -> [{current_view,CurrentView},{views,Views}|Props2]
	     end,
    Props4 = case wings_palette:palette(St) of
		 [] -> Props3;
		 Palette -> [{palette, Palette}|Props3]
	     end,
    Props5 = export_pst(St#st.pst,Props4),
    Props  = [{scene_prefs,wings_pref:get_scene_value()}|Props5],
    Wings = {wings,2,{Shs,Materials,Props}},
    wings_pb:update(0.99, ?__(5,"compressing")),
    Bin = term_to_binary(Wings, [compressed]),
    wings_pb:update(1.0, ?__(6,"writing file")),
    wings_pb:done(write_file(Name, Bin)).

remove_lights(#st{sel=Sel0,shapes=Shs0}=St) ->
    Shs1 = foldl(fun(We, A) when ?IS_ANY_LIGHT(We) -> A;
		    (#we{id=Id}=We, A) -> [{Id,We}|A]
		 end, [], gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(reverse(Shs1)),
    Sel = [S || {Id,_}=S <- Sel0, gb_trees:is_defined(Id, Shs)],
    St#st{sel=Sel,shapes=Shs}.

collect_sel(#st{selmode=Mode,sel=Sel0,ssels=Ssels}=St) ->
    Sel1 = [{Id,{Mode,gb_sets:to_list(Elems),selection}} ||
	       {Id,Elems} <- Sel0],
    Sel2 = collect_sel_groups(gb_trees:to_list(Ssels), St, Sel1),
    Sel3 = sofs:relation(Sel2, [{id,data}]),
    Sel = sofs:relation_to_family(Sel3),
    sofs:to_external(Sel).

collect_sel_groups([{{Mode,Name},Sel}|Gs], St, Acc0) ->
    Acc = [{Id,{Mode,gb_sets:to_list(Elems),{selection_group,Name}}} ||
	      {Id,Elems} <- wings_sel:valid_sel(Sel, Mode, St)] ++ Acc0,
    collect_sel_groups(Gs, St, Acc);
collect_sel_groups([], _, Acc) -> Acc.

renumber([{Id,We0}|Shs], [{Id,Root0}|Sel], NewId, WeAcc, RootAcc) ->
    Hidden = wings_we:num_hidden(We0),
    {We,Root} = wings_we:renumber(We0, 0, Root0),
    renumber(Shs, Sel, NewId+1, [{Hidden,We}|WeAcc],
	     [{NewId,Root}|RootAcc]);
renumber([{_,We0}|Shs], Sel, NewId, WeAcc, RootAcc) ->
    Hidden = wings_we:num_hidden(We0),
    We = wings_we:renumber(We0, 0),
    renumber(Shs, Sel, NewId+1, [{Hidden,We}|WeAcc], RootAcc);
renumber([], [], _NewId, WeAcc, RootAcc) ->
    {WeAcc,RootAcc}.

export_props(Sel0) ->
    Sel1 = sofs:family(Sel0, [{id,[{mode,list,key}]}]),
    Sel2 = sofs:family_to_relation(Sel1),
    Sel3 = sofs:projection(
	     {external,fun({Id,{Mode,Elems,Key}}) ->
			       {{Key,Mode},{Id,Elems}}
		       end}, Sel2),
    Sel = sofs:relation_to_family(Sel3),
    export_props_1(sofs:to_external(Sel), []).

export_props_1([{{What,Mode},Sel}|T], Acc) ->
    export_props_1(T, [{What,{Mode,Sel}}|Acc]);
export_props_1([], Acc) -> Acc.

export_pst(undefined, Props0) -> Props0;
export_pst(Pst0,Props0) ->
    try 
	Pst1 = gb_trees:to_list(Pst0),
	Pst = lists:filter(fun({Mod,_}) when is_atom(Mod) -> true;
			      (_) -> false end, Pst1),
	[{plugin_states,Pst}|Props0]
    catch error:Reason -> 
	    io:format("Failed exporting plugins state NOT a gb_tree ~p ~n",
		      [Reason]),
	    Props0
    end.

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape({Hidden,#we{mode=ObjMode,name=Name,vp=Vs0,es=Es0,he=Htab,pst=Pst}=We}, Acc) ->
    Vs1 = foldl(fun export_vertex/2, [], gb_trees:values(Vs0)),
    Vs = reverse(Vs1),
    UvFaces = gb_sets:from_ordset(wings_we:uv_mapped_faces(We)),
    Es1 = foldl(fun(E, A) ->
			export_edge(E, UvFaces, A)
		end, [], gb_trees:values(Es0)),
    Es = reverse(Es1),
    Fs1 = foldl(fun export_face/2, [], wings_facemat:all(We)),
    Fs = reverse(Fs1),
    He = gb_sets:to_list(Htab),
    Props0 = [{mode,ObjMode}|export_perm(We)],
    Props1 = hidden_faces(Hidden, Props0),
    Props2 = mirror(We, Props1),
    Props  = export_pst(Pst,Props2),
    [{object,Name,{winged,Es,Fs,Vs,He},Props}|Acc].

mirror(#we{mirror=none}, Props) -> Props;
mirror(#we{mirror=Face}, Props) -> [{mirror_face,Face}|Props].

hidden_faces(0, Props) -> Props;
hidden_faces(N, Props) -> [{num_hidden_faces,N}|Props].

export_perm(#we{perm=[]}) ->
    [{state,hidden_locked}];	     %Only for backward compatibility.
export_perm(#we{perm=0}) -> [];
export_perm(#we{perm=1}) -> [{state,locked}];
export_perm(#we{perm=2}) -> [{state,hidden}];
export_perm(#we{perm=3}) -> [{state,hidden_locked}];
export_perm(#we{perm={Mode,Elems}}) ->
    [{state,{hidden,Mode,gb_sets:to_list(Elems)}}].

export_edge(Rec, UvFaces, Acc) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
	  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Rec,
    Data0 = [{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}],
    Data = edge_data(Rec, UvFaces, Data0),
    [Data|Acc].
    
edge_data(#edge{lf=Lf,rf=Rf,a=A,b=B}, UvFaces, Acc0) ->
    Acc = edge_data_1(left, Lf, A, UvFaces, Acc0),
    edge_data_1(right, Rf, B, UvFaces, Acc).

edge_data_1(Side, Face, {U,V}, UvFaces, Acc) ->
    case gb_sets:is_member(Face, UvFaces) of
	false -> Acc;
	true when Side == left  -> [{uv_lt,<<U/float,V/float>>}|Acc];
	true when Side == right -> [{uv_rt,<<U/float,V/float>>}|Acc]
    end;
edge_data_1(_Side, _Face, {1.0,1.0,1.0}, _UvFaces, Acc) -> Acc;
edge_data_1(left, _Face, {R,G,B}, _UvFaces, Acc) ->
    [{color_lt,<<R:32/float,G:32/float,B:32/float>>}|Acc];
edge_data_1(right, _Face, {R,G,B}, _UvFaces, Acc) ->
    [{color_rt,<<R:32/float,G:32/float,B:32/float>>}|Acc];
edge_data_1(_, _, _, _, Acc) -> Acc.

export_face({_,default}, Acc) -> [[]|Acc];
export_face({_,Mat}, Acc) -> [[{material,Mat}]|Acc].

export_vertex({X,Y,Z}, Acc) ->
    [[<<X/float,Y/float,Z/float>>]|Acc].

export_images() ->
    export_images_1(wings_image:images()).

export_images_1([{Id,Im}|T]) ->
    [{Id,export_image(Im)}|export_images_1(T)];
export_images_1([]) -> [].

export_image(#e3d_image{filename=none,type=Type0,order=Order}=Im0) ->
    Im = case {export_img_type(Type0),Order} of
	     {Type0=Type,lower_left} -> Im0;
	     {Type,_} -> e3d_image:convert(Im0, Type, 1, lower_left)
	 end,
    #e3d_image{width=W,height=H,bytes_pp=PP,image=Pixels,name=Name} = Im,
    MaskSize = mask_size(Type),
    [{name,Name},{width,W},{height,H},{samples_per_pixel,PP},
     {mask_size,MaskSize},{pixels,Pixels}];
export_image(#e3d_image{name=Name,filename=Filename}=Im) ->
    case filelib:is_file(Filename) of
	false ->
	    export_image(Im#e3d_image{filename=none});
	true ->
	    [{name,Name},{filename,Filename}]
    end.

export_img_type(b8g8r8) -> r8g8b8;
export_img_type(b8g8r8a8) -> r8g8b8a8;
export_img_type(Type) -> Type.

mask_size(r8g8b8a8) -> 1;
mask_size(a8) -> 1;
mask_size(_) -> 0.
