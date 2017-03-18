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
     {specular,norm(prop_get(specular, P, {0.0,0.0,0.0,1.0}))},
     {emission,norm(prop_get(emission, P, {0.0,0.0,0.0,1.0}))},
     {shininess,prop_get(shininess, P, 0.9)},
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
	    io:format(?__(1,"Failed to load") ++ " \"~ts\": ~s\n",
		      [File,file:format_error(Error)]),
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
    none =/= prop_get(diffuse, Maps, none).

apply_material(Name, Mtab, ActiveVertexColors, RS) ->
    case maps:get(material, RS, undefined) of
        Name -> fun() -> RS end;
        _Active ->
            apply_material_1(Name, Mtab, ActiveVertexColors, RS)
    end.

apply_material_1(Name, Mtab, ActiveVertexColors, #{shader:=Shader}=RS)
  when is_atom(Name) ->
    Mat = gb_trees:get(Name, Mtab),
    OpenGL = prop_get(opengl, Mat),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR, prop_get(specular, OpenGL)),
    Shine = prop_get(shininess, OpenGL)*128,
    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, prop_get(emission, OpenGL)),
    VertexColors = case ActiveVertexColors of
		       false -> ignore;
		       true -> prop_get(vertex_colors, OpenGL, ignore)
		   end,
    DeApply = case VertexColors of
                  ignore when ActiveVertexColors ->
                      gl:disableClientState(?GL_COLOR_ARRAY),
                      fun() -> gl:enableClientState(?GL_COLOR_ARRAY), RS end;
                  _ ->
                      fun() -> RS#{material=>Name} end
              end,
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, prop_get(diffuse, OpenGL)),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, prop_get(ambient, OpenGL)),
    Maps = prop_get(maps, Mat, []),
    apply_texture(prop_get(diffuse, Maps, false), Shader),
    apply_normal_map(get_normal_map(Maps), Shader),  %% Combine with vertex colors
    DeApply.

enable(true)  -> 1;
enable(false) -> 0.

texture_var(diffuse) -> "UseDiffuseMap";
texture_var(normal) ->  "UseNormalMap".

shader_texture(What, Enable, #{}=Shader) ->
    wings_gl:set_uloc(Shader, texture_var(What), enable(Enable)).

apply_texture(false, Shader) -> no_texture(Shader);
apply_texture(Image, Shader) ->
    case wings_pref:get_value(show_textures) of
	false -> no_texture(Shader);
	true ->
	    case wings_image:txid(Image) of
		none ->
		    %% Image was deleted.
		    no_texture(Shader);
		TxId ->
		    apply_texture_1(Image, TxId, Shader)
	    end
    end.

get_normal_map(Maps) ->
    case prop_get(normal, Maps, none) of
	none -> prop_get(bump, Maps, none);
	Map -> Map
    end.

apply_normal_map(none, Shader) ->
    shader_texture(normal, false, Shader),
    false;
apply_normal_map(TexId, Shader) ->
    shader_texture(normal, true, Shader),
    Bump = wings_image:bumpid(TexId),
    gl:activeTexture(?GL_TEXTURE0 + ?NORMAL_MAP_UNIT),
    gl:bindTexture(?GL_TEXTURE_2D, Bump),
    gl:activeTexture(?GL_TEXTURE0),
    true.

apply_texture_1(Image, TxId, Shader) ->
    shader_texture(diffuse, true, Shader),
    gl:bindTexture(?GL_TEXTURE_2D, TxId),
    case wings_image:info(Image) of
	#e3d_image{bytes_pp=4} ->
	    gl:enable(?GL_ALPHA_TEST),
	    gl:alphaFunc(?GL_GREATER, 0.3);
	#e3d_image{type=a8} ->
	    gl:enable(?GL_ALPHA_TEST),
	    gl:alphaFunc(?GL_GREATER, 0.3);
	_ ->
	    gl:disable(?GL_ALPHA_TEST)
    end,
    true.

no_texture(Shader) ->
    shader_texture(diffuse, false, Shader),
    gl:disable(?GL_ALPHA_TEST),
    false.

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
    wings_dialog:dialog(?__(1,"Material Properties: ")++atom_to_list(Name),
			Qs, Fun).


edit_dialog(Name, Assign, St=#st{mat=Mtab0}, Mat0) ->
    OpenGL0 = prop_get(opengl, Mat0),
    VertexColors0 = prop_get(vertex_colors, OpenGL0, ignore),
    {Diff0,Opacity0} = ask_prop_get(diffuse, OpenGL0),
    {Amb0,_} = ask_prop_get(ambient, OpenGL0),
    {Spec0,_} = ask_prop_get(specular, OpenGL0),
    Shine0 = prop_get(shininess, OpenGL0),
    {Emiss0,_} = ask_prop_get(emission, OpenGL0),
    Preview = fun(GLCanvas, Fields) ->
		      mat_preview(GLCanvas,Fields,prop_get(maps,Mat0))
	      end,
    Refresh = fun(_Key, _Value, Fields) ->
		      GLCanvas = wings_dialog:get_widget(preview, Fields),
		      wxWindow:refresh(GLCanvas)
	      end,
    RHook = {hook, Refresh},
    AnyTexture = has_texture(Mat0),
    VtxColMenu = vertex_color_menu(AnyTexture, VertexColors0),
    Qs1 = {vframe,
	   [
	    {hframe,
	     [{custom_gl,?PREVIEW_SIZE,?PREVIEW_SIZE+5,Preview, [{key, preview}]},
	      {label_column,
	       [{?__(1,"Diffuse"), {slider,{color,Diff0, [{key,diffuse},RHook]}}},
		{?__(2,"Ambient"), {slider,{color,Amb0,[{key,ambient}, RHook]}}},
		{?__(3,"Specular"),{slider,{color,Spec0,[{key,specular}, RHook]}}},
		{?__(4,"Emission"),{slider,{color,Emiss0,[{key,emission}, RHook]}}}
	       ]}]},
	    {label_column,
	     [{"Vertex Colors", VtxColMenu},
	      {?__(5,"Shininess"),
	       {slider,{text,Shine0, [{range,{0.0,1.0}}, {key,shininess}, RHook]}}},
	      {?__(6,"Opacity"),
	       {slider,{text,Opacity0, [{range,{0.0,1.0}}, {key,opacity}, RHook]}}}
	     ]}
           ]
	  },
    Qs2 = wings_plugin:dialog({material_editor_setup,Name,Mat0}, [{"Wings 3D", Qs1}]),
    Qs = {vframe_dialog,
	  [{oframe, Qs2, 1, [{style, buttons}]}],
	  [{buttons, [ok, cancel]}, {key, result}]},
    Ask = fun([{diffuse,Diff},
	       {ambient,Amb},
	       {specular,Spec},
	       {emission,Emiss},
	       {vertex_colors,VertexColors},
	       {shininess,Shine},{opacity,Opacity}|More]) ->
		  OpenGL = [ask_prop_put(diffuse, Diff, Opacity),
			    ask_prop_put(ambient, Amb, Opacity),
			    ask_prop_put(specular, Spec, Opacity),
			    ask_prop_put(emission, Emiss, Opacity),
			    {shininess,Shine},
			    {vertex_colors,VertexColors}],
		  Mat1 = keyreplace(opengl, 1, Mat0, {opengl,OpenGL}),
		  {ok,Mat} =  plugin_results(Name, Mat1, More),
		  Mtab = gb_trees:update(Name, Mat, Mtab0),
		  maybe_assign(Assign, Name, St#st{mat=Mtab})
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
	{Mat,[{result,ok}]} -> {ok,Mat};
	{_,Res} ->
	    io:format(?__(1,"Material editor plugin(s) left garbage:~n    ~P~n"),
		      [Res,20]),
            wings_u:error_msg(?__(2,"Plugin(s) left garbage"))
    end.

ask_prop_get(Key, Props) ->
    {R,G,B,Alpha} = prop_get(Key, Props),
    {{R,G,B},Alpha}.

ask_prop_put(specular=Key, {R,G,B}, _) ->
    {Key,{R,G,B,1.0}};
ask_prop_put(Key, {R,G,B}, Opacity) ->
    {Key,{R,G,B,Opacity}}.

mat_preview(Canvas, Common, Maps) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:viewport(0, 0, ?PREVIEW_SIZE, ?PREVIEW_SIZE),
    {BR,BG,BB, _} = wxWindow:getBackgroundColour(wxWindow:getParent(Canvas)),
    %% wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND),
    BGC = fun(Col) -> (Col-15) / 255 end,
    gl:clearColor(BGC(BR),BGC(BG),BGC(BB),1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:perspective(60.0, 1.0, 0.01, 256.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:translatef(0.0, 0.0, -2.0),
    gl:shadeModel(?GL_SMOOTH),
    Alpha = wings_dialog:get_value(opacity, Common),
    Amb   = preview_mat(ambient, Common, Alpha),
    Diff  = preview_mat(diffuse, Common, Alpha),
    Spec  = preview_mat(specular, Common, Alpha),
    Shine = wings_dialog:get_value(shininess, Common),
    gl:materialf(?GL_FRONT, ?GL_SHININESS, Shine*128.0),
    gl:materialfv(?GL_FRONT, ?GL_AMBIENT, Amb),
    gl:materialfv(?GL_FRONT, ?GL_DIFFUSE, Diff),
    gl:materialfv(?GL_FRONT, ?GL_SPECULAR, Spec),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE),
    gl:rotatef(-90.0,1.0,0.0,0.0),
    gl:color4ub(255, 255, 255, 255),
    RS0 = wings_shaders:use_prog(1, #{}),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    %% UseNormalMap = apply_normal_map(get_normal_map(Maps)), No bi-tangent..
    RS1 = case apply_texture(prop_get(diffuse, Maps, false), RS0) of
              true -> glu:quadricTexture(Obj, ?GLU_TRUE), RS0;
              false -> RS0
          end,
    glu:sphere(Obj, 0.9, 50, 50),
    glu:deleteQuadric(Obj),
    no_texture(RS1),
    wings_shader:use_prog(0, RS1),
    gl:disable(?GL_BLEND),
    gl:shadeModel(?GL_FLAT),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW),
    gl:popMatrix(),
    gl:popAttrib().

preview_mat(Key, Colors, Alpha) ->
    {R,G,B} = wings_dialog:get_value(Key, Colors),
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
