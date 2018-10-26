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
	 apply_material/4,is_transparent/2,
	 needed_attributes/2,
         specular_to_metal/1, specular_from_metal/1
        ]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-import(lists, [sort/1,foldl/3,reverse/1,
		keyreplace/4,keydelete/3,keyfind/3,flatten/1]).

-define(DEF_METALLIC, 0.1).
-define(DEF_ROUGHNESS, 0.8).

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
    wings_dialog:ask(?__(1,"New Material"),
		     [{?__(2,"Material Name"),?__(3,"New Material")}],
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
command({rename, Old, New}, St) ->
    rename_1([{list_to_atom(Old),New}], St, []);
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
    wings_dialog:dialog(?__(1,"Rename"), Qs,
			fun([{_,[]}]) -> ignore;
			   (NewNames) ->
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

reassign_material(Old, New, St) ->
    SF = set_material_fun(New, face),
    F = fun(We) ->
                Faces = faces_with_mat(Old, We),
                case gb_sets:is_empty(Faces) of
                    false -> SF(Faces, We);
                    true -> We
                end
        end,
    wings_obj:we_map(F, St).

faces_with_mat(OldMat, #we{fs=Ftab,mat=OldMat}) ->
    gb_sets:from_ordset(gb_trees:keys(Ftab));
faces_with_mat(_, #we{mat=Atom}) when is_atom(Atom) ->
    gb_sets:empty();
faces_with_mat(OldMat, #we{mat=MatTab}) ->
    Fs = [Face || {Face,Mat} <- MatTab, Mat =:= OldMat],
    gb_sets:from_ordset(Fs).

select_material(Mat, SelAct, #st{selmode=Mode}=St) ->
    F = select_fun(SelAct, Mat, Mode),
    case SelAct of
        select ->
            wings_sel:new_sel(F, Mode, St);
        sel_add ->
            wings_sel:update_sel_all(F, St);
        sel_rem ->
            wings_sel:update_sel(F, St)
    end.

select_fun(select, Mat, Mode) ->
    fun(_, We) ->
            selected_items(Mode, Mat, We)
    end;
select_fun(sel_add, Mat, Mode) ->
    fun(Items0, We) ->
            Items = selected_items(Mode, Mat, We),
            gb_sets:union(Items0, Items)
    end;
select_fun(sel_rem, Mat, Mode) ->
    fun(Items0, We) ->
            Items = selected_items(Mode, Mat, We),
            gb_sets:difference(Items0, Items)
    end.

%% Select the elements (face/edge/vertice) using Mat
selected_items(Mode, Mat, #we{fs=Ftab}=We) ->
    MatFaces = wings_facemat:mat_faces(gb_trees:to_list(Ftab), We),
    case keyfind(Mat, 1, MatFaces) of
	false ->
	    gb_sets:empty();
	{Mat,FaceInfoList} ->
	    Fs = [F || {F,_} <- FaceInfoList, F >= 0],
            SelItems = case Mode of
                           vertex -> wings_face:to_vertices(Fs, We);
                           edge -> wings_face:to_edges(Fs, We);
                           _ -> Fs
                       end,
            gb_sets:from_ordset(SelItems)
    end.

set_material(Mat, #st{selmode=Mode}=St) ->
    F = set_material_fun(Mat, Mode),
    wings_sel:map(F, St).

set_material_fun(Mat, face) ->
    fun(Faces, We) ->
            wings_facemat:assign(Mat, Faces, We)
    end;
set_material_fun(Mat, body) ->
    fun(_, #we{fs=Ftab}=We) ->
            wings_facemat:assign(Mat, gb_trees:keys(Ftab), We)
    end;
set_material_fun(_, _) ->
    fun(_, We) -> We end.

default() ->
    Dm = wings_pref:get_value(material_default),
    M = [{default,make_default(Dm, 1.0, [{vertex_colors,set}])}],
    gb_trees:from_orddict(sort(M)).

make_default(Color, Opacity) ->
    make_default(Color, Opacity, []).

make_default({R,G,B}, Opacity, More) ->
    Color = {R,G,B,Opacity},
    Dark = {0.0,0.0,0.0,1.0},
    Mat = [{opengl,[{diffuse,Color}, {metallic, ?DEF_METALLIC},
                    {roughness, ?DEF_ROUGHNESS}, {emission,Dark}|More]},
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
    Diff = norm(prop_get(diffuse, P, Def)),
    Emission = norm(prop_get(emission, P, {0.0,0.0,0.0,1.0})),
    [{diffuse, Diff},
     {emission,Emission},
     {metallic, def_metallic(P, Diff)},
     {roughness, def_roughness(P)},
     {vertex_colors,VertexColor}].

def_metallic(P, Diff) ->
    case prop_get(metallic, P) of
        undefined ->
            case prop_get(specular, P) of
                undefined -> ?DEF_METALLIC;
                Spec -> specular_to_metal(norm(Spec), Diff)
            end;
        Def when is_float(Def) -> Def
    end.

def_roughness(P) ->
    case prop_get(roughness, P) of
        undefined ->
            case prop_get(shininess, P) of
                undefined -> ?DEF_ROUGHNESS;
                Shin -> 1.0 - min(1.0, Shin)
            end;
        Def when is_float(Def) -> Def
    end.

%% For future compatibility, ignore anything that we don't recognize.
valid_vertex_color(multiply) -> set;
valid_vertex_color(set) -> set;
valid_vertex_color(_) -> ignore.

norm({_,_,_,_}=Color) -> Color;
norm({R,G,B}) -> {R,G,B,1.0}.

update_materials([{Name,Mat0}|Ms], St) ->
    Mat1 = add_defaults(Mat0),
    Dir  = wings_pref:get_value(current_directory),
    Maps = load_maps(prop_get(maps, Mat1, []), Dir),
    Mat = keyreplace(maps, 1, Mat1, {maps,Maps}),
    update_materials(Ms, update(Name, Mat, St));
update_materials([], St) -> St.

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
    try load_map_0(MapName, Dir) of
	none -> none;
	Im when is_integer(Im) -> Im
    catch
	error:R ->
	    io:format("~p\n", [R]),
	    io:format("~P\n", [erlang:get_stacktrace(),20]),
	    none
    end.

load_map_0(File, Dir) ->
    case wings_image:find_image(Dir, File) of
        false -> load_map_1(File, Dir);
        {true, Id}  -> Id
    end.

load_map_1(File0, Dir) ->
    File = filename:absname(File0, Dir),
    Ps = [{filename,File},{order,lower_left},{alignment,1}],
    case wings_image:image_read(Ps) of
	#e3d_image{}=Im ->
	    Name = filename:rootname(filename:basename(File)),
	    wings_image:new(Name, Im);
	{error,Error} ->
            case file:format_error(Error) of
                "unknown" ++ _ ->
                    io:format(?__(1,"Failed to load") ++ " \"~ts\": ~p\n",
                              [File,Error]);
                ErrStr ->
                    io:format(?__(1,"Failed to load") ++ " \"~ts\": ~s\n",
                              [File,ErrStr])
            end,
	    none
    end.

add(default, _, #st{}=St) -> St;
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
    Maps =/= [].

apply_material(Name, Mtab, false, RS0) ->
    case wings_shaders:set_state(material, Name, RS0) of
        {false, RS0} ->
            fun() -> RS0 end;
        {true, RS1} ->
            apply_material_1(Name, Mtab, false, RS1)
    end;
apply_material(Name, Mtab, true, RS) ->
    %% io:format("apply ~p~n",[Name]),
    apply_material_1(Name, Mtab, true, wings_shaders:clear_state(material,RS)).

apply_material_1(Name, Mtab, ActiveVertexColors, RS0) when is_atom(Name) ->
    case maps:get({material, Name}, RS0, undefined) of
        undefined ->
            Props = material_prop(Name, Mtab),
            apply_material_2(Props, ActiveVertexColors, RS0#{{material, Name}=>Props});
        Props ->
            apply_material_2(Props, ActiveVertexColors, RS0)
    end.

apply_material_2(Props, true, RS0) ->
    case lists:keysearch(vertex_colors, 1, Props) of
        {value, {_, ignore}} ->
            gl:disableClientState(?GL_COLOR_ARRAY),
            RS = lists:foldl(fun apply_material_3/2, RS0, Props),
            fun() -> gl:enableClientState(?GL_COLOR_ARRAY), RS end;
        _ ->
            RS = lists:foldl(fun apply_material_3/2, RS0, Props),
            fun() -> RS end
    end;
apply_material_2(Props, _, RS0) ->
    RS = lists:foldl(fun apply_material_3/2, RS0, Props),
    fun() -> RS end.

apply_material_3({{tex, Type}=TexType, TexId}, Rs0) ->
    case wings_shaders:set_state(TexType, TexId, Rs0) of
        {false, Rs0} ->
            Rs0;
        {true, Rs1} when TexId =:= none ->
            wings_shaders:set_uloc(texture_var(Type), enable(false), Rs1);
        {true, Rs1} ->
            gl:activeTexture(?GL_TEXTURE0 + tex_unit(Type)),
            gl:bindTexture(?GL_TEXTURE_2D, TexId),
            gl:activeTexture(?GL_TEXTURE0),
            wings_shaders:set_uloc(texture_var(Type), enable(true), Rs1)
    end;
apply_material_3({Type, Value}, Rs0)
  when Type =:= diffuse; Type =:= emission; Type =:= metallic; Type =:= roughness ->
    wings_shaders:set_uloc(Type, Value, Rs0);
apply_material_3({_Type,_}, Rs0) ->
    %% io:format("~p:~p: unsupported type ~p~n",[?MODULE,?LINE,_Type]),
    Rs0.

specular_to_metal(Props) ->
    S = prop_get(specular,Props),
    D = prop_get(diffuse, Props),
    specular_to_metal(S, D).

specular_to_metal({SR,SG,SB,_},{DR,DG,DB,_}) ->
    S0 = {SR,SG,SB},
    D0 = {DR,DG,DB},
    Len = e3d_vec:len(S0),
    S1 = e3d_vec:divide(S0, max(1.0, Len)),
    ACos = min(1.0, e3d_vec:dot(e3d_vec:norm(D0), S1)),
    Linear = 1.0 - math:acos(ACos) * 2 / math:pi(),
    %% io:format("~p ~n", [Linear]),
    Linear.

specular_from_metal(GL) ->
    specular_from_metal(prop_get(metallic, GL, ?DEF_METALLIC), prop_get(diffuse, GL)).

specular_from_metal(Met, {R,G,B,_A}) ->
    norm(wings_color:mix(Met, {R,G,B}, {0.1,0.1,0.1})).

add_old_props(Mat) ->
    GL = prop_get(opengl, Mat),
    Added = case prop_get(specular, GL) of
                undefined -> %% Assume old props is missing
                    Spec = specular_from_metal(prop_get(metallic, GL, ?DEF_METALLIC),
                                               prop_get(diffuse, GL)),
                    Rough = prop_get(roughness, GL, ?DEF_ROUGHNESS),
                    [{ambient, {0.0,0.0,0.0,0.0}}, {specular, Spec},
                     {shininess, 1.0 - Rough} | GL];
                _ -> GL
            end,
    [{opengl, Added}|lists:keydelete(opengl, 1, Mat)].

material_prop(Name, Mtab) ->
    Mat = gb_trees:get(Name, Mtab),
    OpenGL = prop_get(opengl, Mat),
    Maps = prop_get(maps, Mat, []),
    case wings_pref:get_value(show_textures) of
        true ->
            [{{tex,diffuse}, get_texture_map(diffuse, Maps)},
             {{tex,normal}, get_normal_map(Maps)},
             {{tex,pbr_orm},  get_pbr_map(Maps)},
             {{tex,emission}, get_texture_map(emission, Maps)}
             |OpenGL];
        false ->
            OpenGL
    end.

get_texture_map(Type, Maps) ->
    image_id(Type, prop_get(Type, Maps, none)).

get_pbr_map(Maps) ->
    PBRId = [prop_get(occlusion, Maps, none),
             prop_get(roughness, Maps, none),
             prop_get(metallic, Maps, none)],
    image_id(combined, PBRId).

get_normal_map(Maps) ->
    case prop_get(normal, Maps, none) of
        none -> image_id(normal, prop_get(bump, Maps, none));
        Map -> image_id(normal, Map)
    end.

image_id(_, none) -> none;
image_id(_, {[none,none,none],_}) -> none;
image_id(normal, Map) -> wings_image:bumpid(Map);
image_id(combined, Map) -> wings_image:combid(Map);
image_id(_, Map) -> wings_image:txid(Map).

enable(true)  -> 1;
enable(false) -> 0.

texture_var(diffuse) -> 'UseDiffuseMap';
texture_var(normal) ->  'UseNormalMap';
texture_var(pbr_orm) -> 'UsePBRMap'; %% red = occlusion green = roughness blue = metallic
texture_var(emission) -> 'UseEmissionMap'.

tex_unit(diffuse) -> ?DIFFUSE_MAP_UNIT;
tex_unit(normal) -> ?NORMAL_MAP_UNIT;
tex_unit(pbr_orm) -> ?PBR_MAP_UNIT; %% red = occlusion green = roughness blue = metallic
tex_unit(emission) -> ?EMISSION_MAP_UNIT.

%% Return the materials used by the objects in the scene.

used_materials(#st{mat=Mat0}=St) ->
    MF = fun(_, We) ->
                 wings_facemat:used_materials(We)
         end,
    RF = fun(M, A) -> ordsets:union(M, A) end,
    Used0 = wings_obj:dfold(MF, RF, ordsets:new(), St),
    Used1 = sofs:from_external(Used0, [name]),
    Mat = sofs:relation(gb_trees:to_list(Mat0), [{name,data}]),
    Used = sofs:restriction(Mat, Used1),
    [{Name,add_old_props(M)} || {Name,M} <- sofs:to_external(Used)].

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
    OpenGL = proplists:get_value(opengl, Mat, []),
    Trans = lists:any(fun({diffuse,{_,_,_,Alpha}}) when Alpha < 1.0 -> true;
                         (_) -> false
                      end, OpenGL),
    %% Trans orelse proplists:is_defined(diffuse, prop_get(maps, Mat)).
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
    has_texture(Mat).

needs_tangents(Mat) ->
    Maps = prop_get(maps, Mat, []),
    none =/= get_normal_map(Maps).

-define(PREVIEW_SIZE, 150).

edit(Name, Assign, #st{mat=Mtab}=St) ->
    Mat = gb_trees:get(Name, Mtab),
    DrawSphere = setup_sphere(),
    {dialog,Qs,Fun} = edit_dialog(Name, Assign, St, Mat, DrawSphere),
    Res = wings_dialog:dialog(?__(1,"Material Properties: ")++atom_to_list(Name),
                              Qs, Fun),
    wings_vbo:delete(DrawSphere),
    Res.

edit_dialog(Name, Assign, St=#st{mat=Mtab0}, Mat0, DrawSphere) ->
    OpenGL0 = prop_get(opengl, Mat0),
    VertexColors0 = prop_get(vertex_colors, OpenGL0, ignore),
    {Diff0,Opacity0} = ask_prop_get(diffuse, OpenGL0),
    Met0 = prop_get(metallic, OpenGL0),
    Roug0 = prop_get(roughness, OpenGL0),
    {Emiss0,_} = ask_prop_get(emission, OpenGL0),

    Maps = prop_get(maps,Mat0),
    MapList = [{{tex,diffuse},   get_texture_map(diffuse, Maps)},
               {{tex,normal},    get_normal_map(Maps)},  %% Have no tangents
               {{tex,pbr_orm},   get_pbr_map(Maps)},
               {{tex,emission},  get_texture_map(emission, Maps)}],

    Preview = fun(GLCanvas, Fields) ->
                      wings_light:init_opengl(),
                      mat_preview(GLCanvas,Fields,DrawSphere,MapList)
	      end,
    Refresh = fun(_Key, _Value, Fields) ->
		      GLCanvas = wings_dialog:get_widget(preview, Fields),
		      wxWindow:refresh(GLCanvas)
	      end,
    RHook = {hook, Refresh},
    VtxColMenu = vertex_color_menu(VertexColors0),
    OptDef = [RHook, {proportion,1}],
    TexOpt = [{range,{0.0,1.0}}, {digits, 6}|OptDef],

    Qs1 = {hframe,
           [{custom_gl,?PREVIEW_SIZE,?PREVIEW_SIZE,Preview,
             [{key, preview}, {proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]},
            {label_column,
             [{?__(1,"Base Color"),{slider,{color,Diff0, [{key,diffuse}|OptDef]}}},
              {?__(7,"Metallic"),  {slider,{text, Met0,  [{key,metallic}|TexOpt]}}},
              {?__(8,"Roughness"), {slider,{text, Roug0, [{key,roughness}|TexOpt]}}},
              {?__(4,"Emission"),  {slider,{color,Emiss0,[{key,emission}|OptDef]}}},
              separator,
              {?__(6,"Opacity"), {slider,{text,Opacity0, [{key,opacity}|TexOpt]}}},
              {"Vertex Colors", VtxColMenu}
             ], [{proportion,2}]}], [{proportion, 1}]},
    Qs2 = wings_plugin:dialog({material_editor_setup,Name,Mat0}, [{"Wings 3D", Qs1}]),
    Qs = {vframe_dialog,
	  [{oframe, Qs2, 1, [{style, buttons}]}],
	  [{buttons, [ok, cancel]}, {key, result}]},
    Ask = fun([{diffuse,Diff},
               {metallic, Met},
               {roughness, Roug},
	       {emission,Emiss},
               {opacity,Opacity},
	       {vertex_colors,VertexColors}|More]) ->
		  OpenGL = [ask_prop_put(diffuse, Diff, Opacity),
                            {metallic, Met},
                            {roughness, Roug},
			    ask_prop_put(emission, Emiss, Opacity),
			    {vertex_colors,VertexColors}],
		  Mat1 = keyreplace(opengl, 1, Mat0, {opengl,OpenGL}),
		  {ok,Mat} =  plugin_results(Name, Mat1, More),
		  Mtab = gb_trees:update(Name, Mat, Mtab0),
		  maybe_assign(Assign, Name, St#st{mat=Mtab})
	  end,
    {dialog,Qs,Ask}.

vertex_color_menu(multiply) ->
    vertex_color_menu(set);
vertex_color_menu(Def) ->
    {menu,[{"Ignore",ignore,[{info,"Ignore vertex colors"}]},
	   {"Set",set,[{info,"Show vertex colors"}]}],Def,
     [{info,"Choose how to use vertex colors"},
      {key,vertex_colors}]}.

maybe_assign(false, _, St) -> St;
maybe_assign(true, Name, St) -> set_material(Name, St).

plugin_results(Name, Mat0, Res0) ->
    case wings_plugin:dialog_result({material_editor_result,Name,Mat0}, Res0) of
	{Mat,[{result,ok}]} -> {ok,Mat};
	{_,Res} ->
	    io:format(?__(1,"Material editor plugin(s) left garbage:~n    ~P~n"),
		      [Res,20]),
            wings_u:error_msg(?__(2,"Plugin(s) left garbage"))
    end.

ask_prop_get(Key, Props) ->
    {R,G,B,Alpha} = prop_get(Key, Props),
    {{R,G,B},Alpha}.

ask_prop_put(Key, {R,G,B}, Opacity) ->
    {Key,{R,G,B,Opacity}}.

setup_sphere() ->
    {Len, Tris, Normals, UVs, Tgs} =
        wings_shapes:tri_sphere(#{subd=>4, ccw=>false, normals=>true, tgs=>true,
                                  uvs=>true, scale=>0.45}),
    Data = zip(Tris, Normals, UVs, Tgs),
    Layout = [vertex, normal, uv, tangent],
    D = fun(#{preview := PreviewMat} = RS0) ->
                RS1 = wings_shaders:use_prog(1, RS0),
                RS2 = lists:foldl(fun apply_material_3/2, RS1, PreviewMat),
                gl:drawArrays(?GL_TRIANGLES, 0, Len*3),
                wings_shaders:use_prog(0, RS2)
        end,
    wings_vbo:new(D, Data, Layout).

mat_preview(Canvas, Common, Vbo, Maps) ->
    {W,H} = wxWindow:getSize(Canvas),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:viewport(0, 0, W, H),
    {BR,BG,BB, _} = wxWindow:getBackgroundColour(wxWindow:getParent(Canvas)),
    %% wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND),
    BGC = fun(Col) -> (Col-15) / 255 end,
    gl:clearColor(BGC(BR),BGC(BG),BGC(BB),1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    Fov = 45.0, Aspect = W/H,
    MatP = e3d_transform:perspective(Fov, Aspect, 0.01, 256.0),
    gl:multMatrixd(e3d_transform:matrix(MatP)),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    Dist = (0.5/min(1.0,Aspect)) / math:tan(Fov/2*math:pi()/180),
    Eye = {0.0,0.0,Dist}, Up = {0.0,1.0,0.0},
    MatMV = e3d_transform:lookat(Eye, {0.0,0.0,0.0}, Up),
    gl:multMatrixd(e3d_transform:matrix(MatMV)),
    gl:shadeModel(?GL_SMOOTH),
    Alpha = wings_dialog:get_value(opacity, Common),
    Diff  = preview_mat(diffuse, Common, Alpha),
    Emis  = preview_mat(emission, Common, Alpha),
    Metal = {metallic, wings_dialog:get_value(metallic, Common)},
    Rough = {roughness, wings_dialog:get_value(roughness, Common)},
    Material = [Diff, Emis, Metal, Rough | Maps],
    gl:enable(?GL_BLEND),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:color4ub(255, 255, 255, 255),
    RS = #{ws_eyepoint=>Eye, view_from_world=> MatMV, preview=>Material},
    wings_dl:call(Vbo, RS),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_FLAT),
    gl:popAttrib(),
    wings_develop:gl_error_check("Rendering mat viewer").

zip(Vs, Ns, UVs, Tgs) ->
    zip_0(Vs, Ns, UVs, Tgs, []).

zip_0([V|Vs], [N|Ns], [UV|UVs], [T|Ts], Acc) ->
    zip_0(Vs, Ns, UVs,Ts, [V,N,UV,T|Acc]);
zip_0([], [], [], [],Acc) -> Acc.

preview_mat(Key, Colors, Alpha) ->
    {R,G,B} = wings_dialog:get_value(Key, Colors),
    {Key, {R,G,B,Alpha}}.

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
