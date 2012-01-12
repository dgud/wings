%%
%%  wings_material.erl --
%%
%%     This module manages the face materials (i.e. colors and textures).
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_material).
-export([material_menu/1,command/2,new/1,color/4,default/0,
	 add_materials/2,add_materials/3,
	 update_materials/2,
	 update_image/4,used_images/1,
	 used_materials/1,has_texture/2,
	 apply_material/3,is_transparent/2,
	 needed_attributes/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d_image.hrl").

-import(lists, [sort/1,foldl/3,reverse/1,
		keyreplace/4,keydelete/3,keyfind/3,flatten/1]).

material_menu(St) ->
    [{?__(4,"Material"),{material,material_fun(St)}}].

material_fun(St) ->
    fun(help, _Ns) ->
	    {?__(1,"Assign existing material to selection"),[],
	     ?__(2,"Create and assign new material")};
       (1, _Ns) ->
	    mat_list(St);
       (3, _) ->
	    {material,new};
       (_, _) -> ignore
    end.

mat_list(#st{mat=Mtab}) ->
    mat_list_1(gb_trees:to_list(Mtab), []).

mat_list_1([{Name,Ps}|Ms], Acc) ->
    OpenGL = prop_get(opengl, Ps, []),
    Diff = prop_get(diffuse, OpenGL),
    Menu = {atom_to_list(Name),{'VALUE',{assign,Name}},[],[{color,Diff}]},
    mat_list_1(Ms, [Menu|Acc]);
mat_list_1([], Acc) -> reverse(Acc).

new(_) ->
    new_1(new).

new_1(Act) ->
    wings_ask:ask(?__(1,"New Material"),
		  [{?__(2,"Material Name"),
		    ?__(3,"New Material")}],
		  fun([Name]) ->
			  Action = {action,{material,{Act,Name}}},
			  wings_wm:send_after_redraw(geom, Action),
			  ignore
		  end).

command(new, _) ->
    new_1(assign_new);
command({assign_new,Name}, St) ->
    new_material(Name, true, St);
command({new,Name}, St) ->
    new_material(Name, false, St);
command({edit,Mat}, St) ->
    edit(list_to_atom(Mat), false, St);
command({assign,Mat}, St) when is_atom(Mat) ->
    set_material(Mat, St);
command({assign,Mat}, St) ->
    set_material(list_to_atom(Mat), St);
command({select,[Mat,SelAct]}, St) ->
    {save_state,select_material(list_to_atom(Mat),SelAct, St)};
command({duplicate,MatList}, St) ->
    duplicate_material(MatList, St);
command({delete,MatList}, St) ->
    delete_material(MatList, St);
command({rename,MatList0}, St) ->
    case MatList0 -- ["default"] of
	[] -> St;
	MatList -> rename(MatList, St)
    end;
command({update,Name,Mat}, #st{mat=Mtab0}=St) ->
    Mtab = gb_trees:update(Name, Mat, Mtab0),
    St#st{mat=Mtab}.

new_material(Name0, Assign, #st{mat=Mtab}=St) ->
    Name1 = list_to_atom(Name0),
    case gb_trees:is_defined(Name1, Mtab) of
	true ->
	    Names = [atom_to_list(N) || N <- gb_trees:keys(Mtab)],
	    Name = list_to_atom(wings_util:unique_name(Name0, Names)),
	    new_material_1(Name, Assign, St);
	false ->
	    new_material_1(Name1, Assign, St)
    end.

new_material_1(Name, Assign, St0) ->
    Mat = make_default({1.0,1.0,1.0}, 1.0),
    St = add(Name, Mat, St0),
    edit(Name, Assign, St).

duplicate_material([M0|Ms], #st{mat=Mat}=St0) ->
    M1 = list_to_atom(M0),
    MatPs = gb_trees:get(M1, Mat),
    M = new_name(M0, Mat),
    St = add(M, MatPs, St0),
    duplicate_material(Ms, St);
duplicate_material([], St) -> St.

delete_material(["default"|Ms], St) ->
    delete_material(Ms, St);
delete_material([M0|Ms], #st{mat=Mat0}=St0) ->
    M = list_to_atom(M0),
    Mat = gb_trees:delete(M, Mat0),
    St = reassign_material(M, default, St0),
    delete_material(Ms, St#st{mat=Mat});
delete_material([], St) ->
    {save_state,St}.

rename(Mats, St) ->
    Qs = rename_qs(Mats),
    wings_ask:dialog(?__(1,"Rename"), Qs,
		     fun(NewNames) ->
			     rename_1(NewNames, St, [])
		     end).

rename_1([{Old,New}|Ms], #st{mat=Mat0}=St, Acc) ->
    MatPs = gb_trees:get(Old, Mat0),
    Mat = gb_trees:delete(Old, Mat0),
    rename_1(Ms, St#st{mat=Mat}, [{Old,list_to_atom(New),MatPs}|Acc]);
rename_1([], St, Acc) -> rename_2(Acc, St).

rename_2([{Old,New0,MatPs}|Ms], St0) ->
    case add(New0, MatPs, St0) of
	{St1,New} -> ok;
	#st{}=St1 -> New = New0
    end,
    St = reassign_material(Old, New, St1),
    rename_2(Ms, St);
rename_2([], St) -> St.

rename_qs(Ms) ->
    OldNames = [{label,M} || M <- Ms],
    TextFields = [{text,M,[{key,list_to_atom(M)}]} || M <- Ms],
    [{hframe,
      [{vframe,OldNames},
       {vframe,TextFields}]}].

reassign_material(Old, New, St0) ->
    %% It would be tempting to call select_material/2 here instead of
    %% make_fake_selection/2, but we must make sure that even invisible
    %% and locked objects gets selected.
    case make_fake_selection(Old, St0) of
	#st{sel=[]} -> St0;
	St1 ->
	    #st{shapes=Shs,mat=Mat} = set_material(New, St1),
	    St0#st{shapes=Shs,mat=Mat}
    end.

make_fake_selection(OldMat, #st{shapes=Shapes}=St) ->
    Sel0 = gb_trees:values(Shapes),
    Sel = make_fake_selection_1(Sel0, OldMat),
    St#st{selmode=face,sel=Sel}.

make_fake_selection_1([#we{id=Id,fs=Ftab,mat=OldMat}|Shs], OldMat) ->
    [{Id,gb_trees:keys(Ftab)}|make_fake_selection_1(Shs, OldMat)];
make_fake_selection_1([#we{mat=Atom}|Shs], OldMat) when is_atom(Atom) ->
    make_fake_selection_1(Shs, OldMat);
make_fake_selection_1([#we{id=Id,mat=MatTab}|Shs], OldMat) ->
    case [Face || {Face,Mat} <- MatTab, Mat =:= OldMat] of
	[] -> make_fake_selection_1(Shs, OldMat);
	Sel -> [{Id,gb_sets:from_ordset(Sel)}|make_fake_selection_1(Shs, OldMat)]
    end;
make_fake_selection_1([], _) -> [].

select_material(Mat,SelAct,#st{selmode=SelMode,sel=Sel0,shapes=Shs}=St) ->
    Sel = foldl(fun(#we{id=Id,perm=Perm}=We, Acc) when ?IS_SELECTABLE(Perm) andalso not ?IS_ANY_LIGHT(We) ->
            Sel1=lists:keyfind(Id, 1, Sel0), % check if Id is already in previous selection
            Sel2=selected_we(SelMode, We, Mat), % get #we selection using Mat
            select_material_1(SelAct,Sel1,Sel2,Acc);
        (_, Acc) -> Acc  % process no selectable or light #we
		end, [], gb_trees:values(Shs)),
    wings_sel:set(Sel, St).

%% sel_rem - remove #we's using Mat from any previous selection
select_material_1(sel_rem,Sel1,Sel2,Acc) ->
    case Sel1 of
    false -> Acc; % #we ins't in previous selection - nothing to do
    _ ->
        case subtract_sel(Sel1,Sel2) of
        false -> Acc;
        {_,GbNew}=Sel3 -> 
        case gb_sets:is_empty(GbNew) of
            true -> Acc;
            _ -> [Sel3|Acc]
            end
        end
    end;
%% select - select #we's using Mat - the original behavior
%% sel_add - add #we's using Mat to any previous selection
select_material_1(SelAct,Sel1,Sel2,Acc) ->
    case Sel2 of
    false ->
        case SelAct of
        select -> Acc;
        sel_add -> 
            case concat_sel(Sel1,false) of
            [] -> Acc;
            Sel3 -> [Sel3|Acc]  % preserve previous selection of #we
            end
        end;
    Sel2 ->
        case SelAct of
        select -> [Sel2|Acc]; % we are building a new selection 
        sel_add -> [concat_sel(Sel1,Sel2)|Acc] % if so, we will update the previous selection
        end
    end.

%% select the elements (face/edge/vertice) using the Mat
selected_we(SelMode,#we{id=Id,fs=Ftab}=We, Mat) ->
    MatFaces = wings_facemat:mat_faces(gb_trees:to_list(Ftab), We),
    case keyfind(Mat, 1, MatFaces) of
	false ->
	    false;
	{Mat,FaceInfoList} ->
	    Fs = [F || {F,_} <- FaceInfoList, F >= 0],
		SelItems = case SelMode of
			vertex -> wings_face:to_vertices(Fs, We);
			edge -> wings_face:to_edges(Fs, We);
			_ -> Fs
			end,
		{Id,gb_sets:from_ordset(SelItems)}
	end.

concat_sel(false,false) -> [];
concat_sel(false,NewSel) -> NewSel;
concat_sel(OldSel,false) -> OldSel;
concat_sel({Id,GbSetOld},{Id,GbSetNew}) ->
    {Id,gb_sets:union(GbSetOld,GbSetNew)}.

subtract_sel(false,false) -> false;
subtract_sel(false,_) -> false;
subtract_sel(OldSel,false) -> OldSel;
subtract_sel({Id,GbSetOld},{Id,GbSetNew}) ->
    {Id,gb_sets:subtract(GbSetOld,GbSetNew)}.
    
set_material(Mat, #st{selmode=face}=St) ->
    wings_sel:map(fun(Faces, We) ->
			  wings_facemat:assign(Mat, Faces, We)
		  end, St);
set_material(Mat, #st{selmode=body}=St) ->
    wings_sel:map(fun(_, #we{fs=Ftab}=We) ->
			  wings_facemat:assign(Mat, gb_trees:keys(Ftab), We)
		  end, St);
set_material(_, St) -> St.

default() ->
    Dm = wings_pref:get_value(material_default),
    M = [{default,make_default(Dm, 1.0, [{vertex_colors,set}])}],
    gb_trees:from_orddict(sort(M)).

make_default(Color, Opacity) ->
    make_default(Color, Opacity, []).

make_default({R,G,B}, Opacity, More) ->
    Color = {R,G,B,Opacity},
    Dark = {0.0,0.0,0.0,1.0},
    Mat = [{opengl,[{diffuse,Color},{ambient,Color},{specular,Dark},
		    {emission,Dark},{shininess,0.0}|More]},
	   {maps,[]}],
    sort([{K,sort(L)} || {K,L} <- Mat]).

update_image(MatName, MapType, Image, #st{mat=Mtab}) ->
    Mat = gb_trees:get(MatName, Mtab),
    Maps = prop_get(maps, Mat, []),
    {MapType,ImageId} = keyfind(MapType, 1, Maps),
    wings_image:update(ImageId, Image).

add_materials(Ms, St) ->
    Dir = wings_pref:get_value(current_directory),
    add_materials_1(Ms, Dir, St, []).

add_materials(Ms, Dir, St) ->
    add_materials_1(Ms, Dir, St, []).

add_materials_1([{Name,Mat0}|Ms], Dir, St0, NewNames) ->
    Mat1 = add_defaults(Mat0),
    Maps = load_maps(prop_get(maps, Mat1, []), Dir),
    Mat = keyreplace(maps, 1, Mat1, {maps,Maps}),
    case add(Name, Mat, St0) of
	#st{}=St ->
	    add_materials_1(Ms, Dir, St, NewNames);
	{#st{}=St,NewName} ->
	    add_materials_1(Ms, Dir, St, [{Name,NewName}|NewNames])
    end;
add_materials_1([], _, St, NewNames) -> {St,NewNames}.

add_defaults([]) ->
    add_defaults([{opengl,[]},{maps,[]}]);
add_defaults(Props0) ->
    OpenGL0 = prop_get(opengl, Props0, []),
    OpenGL = add_defaults_1(OpenGL0),
    Props = [{opengl,OpenGL}|lists:keydelete(opengl, 1, Props0)],
    case prop_get(maps, Props) of
	undefined -> [{maps,[]}|Props];
	_ -> Props
    end.

add_defaults_1(P) ->
    Def = {1.0,1.0,1.0,1.0},
    VertexColor = valid_vertex_color(prop_get(vertex_colors, P, ignore)),
    [{diffuse,norm(prop_get(diffuse, P, Def))},
     {ambient,norm(prop_get(ambient, P, Def))},
     {specular,norm(prop_get(specular, P, Def))},
     {emission,norm(prop_get(emission, P, {0.0,0.0,0.0,0.0}))},
     {shininess,prop_get(shininess, P, 1.0)},
     {vertex_colors,VertexColor}].

%% For future compatibility, ignore anything that we don't recognize.
valid_vertex_color(multiply=A) -> A;
valid_vertex_color(set=A) -> A;
valid_vertex_color(_) -> ignore.

update_materials([{Name,Mat0}|Ms], St) ->
    Mat1 = add_defaults(Mat0),
    Dir  = wings_pref:get_value(current_directory),
    Maps = load_maps(prop_get(maps, Mat1, []), Dir),
    Mat = keyreplace(maps, 1, Mat1, {maps,Maps}),
    update_materials(Ms, update(Name, Mat, St));
update_materials([], St) -> St.

norm({_,_,_,_}=Color) -> Color;
norm({R,G,B}) -> {R,G,B,1.0}.
    
load_maps([{Key,Filename}|T], Dir) when is_list(Filename) ->
    case load_map(Filename, Dir) of
	none -> load_maps(T, Dir);
	Map -> [{Key,Map}|load_maps(T, Dir)]
    end;
load_maps([{Key,{W,H,Bits}}|T], Dir) ->
    E3dImage = #e3d_image{type=r8g8b8,order=lower_left,
			  width=W,height=H,image=Bits},
    Id = wings_image:new(atom_to_list(Key), E3dImage),
    [{Key,Id}|load_maps(T, Dir)];
load_maps([{Key,#e3d_image{name=Name0}=E3dImage}|T], Dir) ->
    Name = case Name0 of 
	       [] -> atom_to_list(Key);
	       _ when is_list(Name0) -> Name0;
	       _ -> atom_to_list(Key)
	   end,
    Id = wings_image:new(Name, E3dImage),
    [{Key,Id}|load_maps(T, Dir)];
load_maps([{_,none}|T], Dir) ->
    load_maps(T, Dir);
load_maps([{_,Id}=Map|T], Dir) when is_integer(Id) ->
    [Map|load_maps(T, Dir)];
load_maps([], _) -> [].
    
load_map(MapName, Dir) ->
    try load_map_1(MapName, Dir) of
	none -> none;
	Im when is_integer(Im) -> Im
    catch
	error:R ->
	    io:format("~p\n", [R]),
	    io:format("~P\n", [erlang:get_stacktrace(),20]),
	    none
    end.

load_map_1(File0, Dir) ->
    File = filename:absname(File0, Dir),
    Ps = [{filename,File},{order,lower_left},{alignment,1}],
    case wings_image:image_read(Ps) of
	#e3d_image{}=Im ->
	    Name = filename:rootname(filename:basename(File)),
	    wings_image:new(Name, Im);
	{error,Error} ->
	    io:format(?__(1,"Failed to load") ++ " \"~s\": ~s\n",
		      [File,file:format_error(Error)]),
	    none
    end.
    
add(Name, Mat0, #st{mat=MatTab}=St) ->
    Mat = sort([{K,sort(L)} || {K,L} <- Mat0]),
    case gb_trees:lookup(Name, MatTab) of
	none ->
	    St#st{mat=gb_trees:insert(Name, Mat, MatTab)};
	{value,Mat} -> St;
	{value,_} ->
	    NewName = new_name(atom_to_list(Name), MatTab),
	    {add(NewName, Mat, St),NewName}
    end.

update(Name, Mat0, #st{mat=MatTab}=St) ->
    Mat = sort([{K,sort(L)} || {K,L} <- Mat0]),
    St#st{mat=gb_trees:update(Name, Mat, MatTab)}.

new_name(Name0, Tab) ->
    Names = [atom_to_list(N) || N <- gb_trees:keys(Tab)],
    Name = wings_util:unique_name(Name0, Names),
    list_to_atom(Name).

has_texture(Name, Mtab) ->
    Mat = gb_trees:get(Name, Mtab),
    has_texture(Mat).

has_texture(Mat) ->
    Maps = prop_get(maps, Mat, []),
    none =/= prop_get(diffuse, Maps, none).

apply_material(Name, Mtab, ActiveVertexColors) when is_atom(Name) ->
    Mat = gb_trees:get(Name, Mtab),
    OpenGL = prop_get(opengl, Mat),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR, prop_get(specular, OpenGL)),
    Shine = prop_get(shininess, OpenGL)*128,
    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, prop_get(emission, OpenGL)),
    Maps0 = prop_get(maps, Mat, []),
    VertexColors = case ActiveVertexColors of
		       false -> ignore;
		       true -> prop_get(vertex_colors, OpenGL, ignore)
		   end,
    Def = fun() -> ok end,
    {Maps,DeApply} =
	case VertexColors of
	    ignore ->
		%% Ignore vertex colors. If the hemispherical lighting
		%% shader is enabled, it is not enough to only disable
		%% COLOR_MATERIAL, but we must also disable the color
		%% array.
		gl:disable(?GL_COLOR_MATERIAL),
		case ActiveVertexColors of
		    true ->
			{Maps0,fun() ->
				       gl:enableClientState(?GL_COLOR_ARRAY)
			       end};
		    false ->
			{Maps0,Def}
		   end;
	    set ->
		%% Vertex colors overrides diffuse and ambient color
		%% and suppresses any texture.
		gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
		gl:enable(?GL_COLOR_MATERIAL),
		{[],Def};
	    multiply ->
		%% Vertex colors are multiplied with the texture.
		gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
		gl:enable(?GL_COLOR_MATERIAL),
		{Maps0,Def}
	end,
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, prop_get(diffuse, OpenGL)),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, prop_get(ambient, OpenGL)),
    apply_texture(prop_get(diffuse, Maps, none)),    
    apply_normal_map(get_normal_map(Maps0)),  %% Combine with vertex colors
    DeApply.

apply_texture(none) -> no_texture();
apply_texture(Image) ->
    case wings_pref:get_value(show_textures) of
	false -> no_texture();
	true ->
	    case wings_image:txid(Image) of
		none ->
		    %% Image was deleted.
		    no_texture();
		TxId ->
		    apply_texture_1(Image, TxId)
	    end
    end.

get_normal_map(Maps) ->
    case prop_get(normal, Maps, none) of
	none -> prop_get(bump, Maps, none);
	Map -> Map
    end.

apply_normal_map(none) -> ok;
apply_normal_map(TexId) ->
    Bump = wings_image:bumpid(TexId),
    gl:activeTexture(?GL_TEXTURE0 + ?NORMAL_MAP_UNIT),
    gl:bindTexture(?GL_TEXTURE_2D, Bump),
    gl:activeTexture(?GL_TEXTURE0).

apply_texture_1(Image, TxId) ->
    gl:enable(?GL_TEXTURE_2D),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    Ft=case wings_pref:get_value(filter_texture, false) of
	   true -> ?GL_LINEAR;
	   false -> ?GL_NEAREST
       end,
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, Ft),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, Ft),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_REPEAT),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_REPEAT),
    case wings_gl:is_ext({1,2}) of
	true ->
	    %% Calculate specular color correctly on textured models.
	    gl:lightModeli(?GL_LIGHT_MODEL_COLOR_CONTROL,
			   ?GL_SEPARATE_SPECULAR_COLOR);
	false -> ok
    end,
    case wings_image:info(Image) of
	#e3d_image{bytes_pp=4} ->
%	    gl:enable(?GL_BLEND),
	    gl:enable(?GL_ALPHA_TEST),
	    gl:alphaFunc(?GL_GREATER, 0.3);
	#e3d_image{type=a8} ->
%	    gl:enable(?GL_BLEND),
	    gl:enable(?GL_ALPHA_TEST),
	    gl:alphaFunc(?GL_GREATER, 0.3);
	_ ->
	    gl:disable(?GL_ALPHA_TEST)
    end,
    true.

no_texture() ->
    case wings_gl:is_ext({1,2}) of
	true ->
	    gl:lightModeli(?GL_LIGHT_MODEL_COLOR_CONTROL, ?GL_SINGLE_COLOR);
	false -> 
	    ok
    end,
    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_ALPHA_TEST),
    false.

%% Return the materials used by the objects in the scene.

used_materials(#st{shapes=Shs,mat=Mat0}) ->
    Used0 = foldl(fun(We, A) ->
			  [wings_facemat:used_materials(We)|A]
		  end, [], gb_trees:values(Shs)),
    Used1 = ordsets:union(Used0),
    Used2 = sofs:from_external(Used1, [name]),
    Mat = sofs:relation(gb_trees:to_list(Mat0), [{name,data}]),
    Used = sofs:restriction(Mat, Used2),
    sofs:to_external(Used).

%% Return all image ids used by materials.

used_images(#st{mat=Mat}) ->
    used_images_1(gb_trees:values(Mat), []).

used_images_1([M|Ms], Acc0) ->
    Maps = prop_get(maps, M, []),
    Acc = [Id || {_,Id} <- Maps, is_integer(Id)] ++ Acc0,
    used_images_1(Ms, Acc);
used_images_1([], Acc) -> gb_sets:from_list(Acc).

is_transparent(Name, Mtab) ->
    Mat = gb_trees:get(Name, Mtab),
    is_mat_transparent(Mat).

is_mat_transparent(Mat) ->
    OpenGL = prop_get(opengl, Mat),
    Trans = foldl(fun(_, true) -> true;
		   ({emission,_}, _) -> false;
		     ({_,{_,_,_,1.0}}, _) -> false;
		     ({_,{_,_,_,_}}, _) -> true;
		     (_, _) -> false
		  end, false, OpenGL),
    Trans.

%% needed_attributes(We, St) -> [Attr]
%%     Attr = color|uv|tangent
%%  Return a ordered list of the type of attributes that are needed
%%  according to the materials.
%%  tanget requires uv since it needs the uv's to calculate tanget space
needed_attributes(We, #st{mat=Mat}) ->
    Used = wings_facemat:used_materials(We),
    needed_attributes_1(Used, Mat, false, false, false).

needed_attributes_1(_, _, true, _, true) -> [color,uv,tangent];
needed_attributes_1([M|Ms], MatTab, Col0, UV0, TV0) ->
    Mat = gb_trees:get(M, MatTab),
    TV = TV0 orelse needs_tangents(Mat),
    UV = UV0 orelse needs_uvs(Mat),
    Col = Col0 orelse needs_vertex_colors(Mat),
    needed_attributes_1(Ms, MatTab, Col, UV, TV);
needed_attributes_1([], _, Col, UV, TV) ->
    L = if TV -> [uv, tangent];
	   UV -> [uv];
	   true -> []
	end,
    case Col of
	true -> [color|L];
	false -> L
    end.

needs_vertex_colors(Mat) ->
    OpenGL = prop_get(opengl, Mat),
    prop_get(vertex_colors, OpenGL, ignore) =/= ignore.

needs_uvs(Mat) ->
    OpenGL = prop_get(opengl, Mat),
    case prop_get(vertex_colors, OpenGL, ignore) of
	set ->
	    %% Vertex colors overrides the texture (if any).
	    false;
	_ ->
	    %% We need UV coordinates if there is a diffuse texture.
	    has_texture(Mat)
    end.

needs_tangents(Mat) ->
    Maps = prop_get(maps, Mat, []),
    none =/= get_normal_map(Maps).

-define(PREVIEW_SIZE, 100).

edit(Name, Assign, #st{mat=Mtab}=St) ->
    Mat = gb_trees:get(Name, Mtab),
    {dialog,Qs,Fun} = edit_dialog(Name, Assign, St, Mat),
    wings_ask:dialog(?__(1,"Material Properties: ")++atom_to_list(Name),
		     Qs, Fun).


edit_dialog(Name, Assign, St=#st{mat=Mtab0}, Mat0) ->
    OpenGL0 = prop_get(opengl, Mat0),
    VertexColors0 = prop_get(vertex_colors, OpenGL0, ignore),
    {Diff0,Opacity0} = ask_prop_get(diffuse, OpenGL0),
    {Amb0,_} = ask_prop_get(ambient, OpenGL0),
    {Spec0,_} = ask_prop_get(specular, OpenGL0),
    Shine0 = prop_get(shininess, OpenGL0),
    {Emiss0,_} = ask_prop_get(emission, OpenGL0),
    Maps0 = show_maps(Mat0),
    Preview = fun(A,S,D,F,G) ->
		      mat_preview(A,S,D,F,G,prop_get(maps,Mat0))
	      end,
    Hook = {hook,fun(is_disabled, {_Var,_I,Sto}) ->
			 gb_trees:get(vertex_colors, Sto) =/= ignore;
		    (_, _) -> void
		 end},
    AnyTexture = has_texture(Mat0),
    VtxColMenu = vertex_color_menu(AnyTexture, VertexColors0),
    Qs1 = [{vframe,
	    [
	     {hframe, 
	      [{custom,?PREVIEW_SIZE,?PREVIEW_SIZE+5,Preview},
	       {vframe,
		[{label,?__(1,"Diffuse")},
		 {label,?__(2,"Ambient")},
		 {label,?__(3,"Specular")},
		 {label,?__(4,"Emission")},
		 {label,"Vertex Colors"}
		]
	       },
	       {vframe,
		[{slider,{color,Diff0,
			  [{key,diffuse},Hook]}},
		 {slider,{color,Amb0,[{key,ambient}]}},
		 {slider,{color,Spec0,[{key,specular}]}},
		 {slider,{color,Emiss0,[{key,emission}]}},
		 VtxColMenu]}]},
	     {hframe, [{vframe, [{label,?__(5,"Shininess")},
				 {label,?__(6,"Opacity")}]},
		       {vframe, [{slider,{text,Shine0,
					  [{range,{0.0,1.0}},
					   {key,shininess}]}},
				 {slider,{text,Opacity0,
					  [{range,{0.0,1.0}},
					   {key,opacity}]}}]}]
	     }|Maps0]
	   }],
    Qs2 = wings_plugin:dialog({material_editor_setup,Name,Mat0}, Qs1),
    Qs = {hframe,[{vframe,Qs2},
		  {vframe,[{button,?__(7,"OK"),done,[ok,{key,material_editor_ok}]},
			   {button,wings_s:cancel(),cancel,[cancel]}]}]},
    Ask = fun([{diffuse,Diff},
	       {ambient,Amb},
	       {specular,Spec},
	       {emission,Emiss},
	       {vertex_colors,VertexColors},
	       {shininess,Shine},{opacity,Opacity}|More0]) ->
		  OpenGL = [ask_prop_put(diffuse, Diff, Opacity),
			    ask_prop_put(ambient, Amb, Opacity),
			    ask_prop_put(specular, Spec, Opacity),
			    ask_prop_put(emission, Emiss, Opacity),
			    {shininess,Shine},
			    {vertex_colors,VertexColors}],
		  Mat1 = keyreplace(opengl, 1, Mat0, {opengl,OpenGL}),
		  {Mat2,More} = update_maps(Mat1, More0),
		  case plugin_results(Name, Mat2, More) of
		      {again,Mat} -> edit_dialog(Name, Assign, St, Mat);
		      {ok,Mat}  ->
			  Mtab = gb_trees:update(Name, Mat, Mtab0),
			  maybe_assign(Assign, Name, St#st{mat=Mtab})
		  end
	  end,
    {dialog,Qs,Ask}.

vertex_color_menu(MultiplyPossible, Def0) ->
    Def = case MultiplyPossible of
	      true -> Def0;
	      false when Def0 =:= multiply -> set;
	      false -> Def0
	  end,
    {menu,[{"Ignore",ignore,[{info,"Ignore vertex colors"}]},
	   {"Set",set,[{info,"Show vertex colors"}]}|
	   case MultiplyPossible of
	       true ->
		   [{"Multiply",multiply,
		     [{info,"Multiply texture colors with vertex colors"}]}];
	       false -> []
	   end],Def,
     [{info,"Choose how to use vertex colors"},
      {key,vertex_colors}]}.

maybe_assign(false, _, St) -> St;
maybe_assign(true, Name, St) -> set_material(Name, St).

plugin_results(Name, Mat0, Res0) ->
    case wings_plugin:dialog_result({material_editor_result,Name,Mat0}, Res0) of
	{Mat,[{material_editor_ok,true}]} ->
	    {ok,Mat};
	{Mat,[{material_editor_ok,false}]} ->
	    {again,Mat};
	{_,Res} ->
	    io:format(?__(1,"Material editor plugin(s) left garbage:~n    ~P~n"), 
		      [Res,20]),
		      wings_u:error_msg(?__(2,"Plugin(s) left garbage"))
    end.

update_maps(Mat0, More0) ->
    Maps0 = sort(prop_get(maps, Mat0)),
    {Maps,More} = update_maps_1(More0, Maps0, []),
    Mat = [{maps,Maps}|keydelete(maps, 1, Mat0)],
    {Mat,More}.

update_maps_1([false|More], [M|Maps], Acc) ->
    update_maps_1(More, Maps, [M|Acc]);
update_maps_1([true|More], [_|Maps], Acc) ->
    update_maps_1(More, Maps, Acc);
update_maps_1(More, [], Acc) -> {Acc,More}.

show_maps(Mat) ->
    case prop_get(maps, Mat) of
	[] -> [];
	Maps ->
	    MapDisp = [show_map(M) || M <- sort(Maps)],
	    [{vframe,MapDisp,[{title,?__(1,"Textures")}]}]
    end.

show_map({Type,Image}) ->
    Texture = 
	case wings_image:info(Image) of
	    none ->
		[{label,flatten(io_lib:format(?__(1,"~p: <image deleted>"), [Type]))}];
	    #e3d_image{name=Name,width=W,height=H,bytes_pp=PP} ->
		Label = flatten(io_lib:format(?__(2,"~p: ~p [~px~px~p]"),
					      [Type,Name,W,H,PP*8])),
		[{label, Label},{button,?__(3,"Delete"),done}]
	end,
    {hframe, Texture}.

ask_prop_get(Key, Props) ->
    {R,G,B,Alpha} = prop_get(Key, Props),
    {{R,G,B},Alpha}.

ask_prop_put(specular=Key, {R,G,B}, _) ->
    {Key,{R,G,B,1.0}};
ask_prop_put(Key, {R,G,B}, Opacity) ->
    {Key,{R,G,B,Opacity}}.
    
mat_preview(X, Y, _W, _H, Common, Maps) ->
    wings_io:border(X, Y, ?PREVIEW_SIZE, ?PREVIEW_SIZE, ?PANE_COLOR),
    MM = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    PM = gl:getDoublev(?GL_PROJECTION_MATRIX),
    ViewPort = wings_wm:viewport(),
    {Ox,Oy,_} = wings_gl:project(X, Y+?PREVIEW_SIZE, 0, MM, PM, ViewPort),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:viewport(trunc(Ox), trunc(Oy), ?PREVIEW_SIZE, ?PREVIEW_SIZE),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:perspective(60.0, 1, 0.01, 256.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:translatef(0.0, 0.0, -2.0),
    wings_light:camera_lights(mat_preview),
    gl:shadeModel(?GL_SMOOTH),
    Alpha = gb_trees:get(opacity, Common),
    Amb = preview_mat(ambient, Common, Alpha),
    Diff = preview_mat(diffuse, Common, Alpha),
    Spec = preview_mat(specular, Common, Alpha),
    Shine = gb_trees:get(shininess, Common),
    gl:materialf(?GL_FRONT, ?GL_SHININESS, Shine*128.0),
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT, Amb),
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
    gl:materialfv(?GL_FRONT, ?GL_SPECULAR, Spec),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_CULL_FACE),
    gl:rotatef(-90,1,0,0),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    case apply_texture(prop_get(diffuse, Maps, none)) of
	true -> 
	    glu:quadricTexture(Obj, ?GLU_TRUE);
	false -> 
	    ignore
    end,
    glu:sphere(Obj, 0.9, 50, 50),
    glu:deleteQuadric(Obj),
    no_texture(),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_FLAT),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:popAttrib().

preview_mat(Key, Colors, Alpha) ->
    {R,G,B} = gb_trees:get(Key, Colors),
    {R,G,B,Alpha}.
    
%%% Return color in texture for the given UV coordinates.

color(Face, UV, We, #st{mat=Mtab}) ->
    Name = wings_facemat:face(Face, We),
    Props = gb_trees:get(Name, Mtab),
    Maps = prop_get(maps, Props),
    case prop_get(diffuse, Maps, none) of
	none ->
	    OpenGL = prop_get(opengl, Props),
	    {R,G,B,_} = prop_get(diffuse, OpenGL),
	    wings_color:share({R,G,B});
	DiffMap ->
	    color_1(UV, wings_image:info(DiffMap))
    end;
color(_Face, {_,_,_}=RGB, _We, _St) -> RGB.

color_1(_, none) -> wings_color:white();
color_1(none, _) -> wings_color:white();
color_1({U0,V0}, #e3d_image{width=W,height=H,image=Bits}) ->
    U = (((round(U0*W) rem W) + W) rem W),
    V = ((round(V0*H) rem H) + H) rem H,
    Pos = V*W*3 + U*3,
    <<_:Pos/binary,R:8,G:8,B:8,_/binary>> = Bits,
    wings_util:share(R/255, G/255, B/255).

prop_get(Key, Props) ->
    proplists:get_value(Key, Props).

prop_get(Key, Props, Def) ->
    proplists:get_value(Key, Props, Def).
