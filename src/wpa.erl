%%
%%  wpa.erl --
%%
%%     Wings Plugin API.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%% Note: To keep the call graph clean, wpa MUST NOT be called
%%       from the wings core modules.

-module(wpa).
-export([ask/3,ask/4,dialog/3,dialog/4,dialog/5,error_msg/1,error_msg/2,
	 yes_no/2,yes_no/3,yes_no_cancel/3,
	 bind_unicode/2,bind_virtual/3,
	 import/2,import/3,import_filename/2,
	 export/3,export_selected/3,
	 export_filename/2,export_filename/3,
	 save_images/3,
	 dialog_template/2,dialog_template/3,
	 import_matrix/1,export_matrix/1,
	 send_command/1,
	 pref_get/2,pref_get/3,pref_set/2,pref_set/3,
	 pref_set_default/3,pref_delete/2,
	 scene_pref_get/2,scene_pref_get/3,
	 scene_pref_set/2,scene_pref_set_default/2,scene_pref_delete/2,
	 sel_get/1,sel_set/2,sel_set/3,sel_map/2,sel_fold/3,sel_convert/3,
	 sel_edge_regions/2,sel_face_regions/2,sel_strict_face_regions/2,
	 drop/2,
	 pick/3,
	 vertices/1,vertex_pos/2,vertex_flatten/3,vertex_center/2,
	 faces/1,face_vertices/2,face_outer_vertices_ccw/2,face_outer_edges/2,
	 face_dissolve/2,face_dissolve_complement/2,
	 edge_loop_vertices/2,
	 obj_name/1,obj_id/1,
	 camera_info/1,lights/1,import_lights/2,
	 image_formats/0,image_read/1,image_write/1,
	 vm_freeze/1,
	 triangulate/1,triangulate/2,quadrangulate/1,quadrangulate/2,
	 popup_console/0,version/0
	]).

%% Commands from other processes
-export([format_error/1]).

-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-import(lists, [reverse/1,foldl/3,foreach/2]).

format_error({crash,Term}) ->
    lists:flatten(io_lib:format(?__(1,"Internal error: ~P\n"), [Term,20])).

%%%
%%% ask/3,4 is simpler to use, but only supports a single list of fields.
%%% dialog/3,4 is more powerful but is slightly more involved.
%%%

ask(Title, Qs, Fun) ->
    wings_dialog:ask(Title, Qs, Fun).

ask(Bool, Title, Qs, Fun) ->
    wings_dialog:ask(Bool, Title, Qs, Fun).

dialog(Title, Qs, Fun) ->
    wings_dialog:dialog(Title, Qs, Fun).

dialog(Title, Qs, Fun, HelpFun) when is_list(Title) ->
    wings_dialog:dialog(Title, Qs, Fun, HelpFun);

dialog(Bool, Title, Qs, Fun) ->
    wings_dialog:dialog(Bool, Title, Qs, Fun).

dialog(Bool, Title, Qs, Fun, HelpFun) ->
    wings_dialog:dialog(Bool, Title, Qs, Fun, HelpFun).

%% Show String in a dialog box.
-spec error_msg(any()) -> no_return().
error_msg(String) ->
    wings_u:error_msg(String).

-spec error_msg(any(), any()) -> no_return().
error_msg(Format, Args) ->
    wings_u:error_msg(Format, Args).

yes_no(Question, Yes) ->
    wings_u:yes_no(Question, Yes).

yes_no(Question, Yes, No) ->
    wings_u:yes_no(Question, Yes, No).

yes_no_cancel(Question, Yes, No) ->
    wings_u:yes_no_cancel(Question, Yes, No).

bind_unicode(Key, Command) ->
    wings_hotkey:bind_unicode(Key, Command, plugin).

bind_virtual(Key, Mods, Command) ->
    wings_hotkey:bind_virtual(Key, Mods, Command, plugin).

%%%
%%% Import/export support.
%%%

%% returns: St
import(#e3d_file{}=E3dFile, St) ->
    wings_import:import(E3dFile, St);
import(Files, St0) when is_list(Files) ->
    Imps = importers(),
    Import = fun(File, St) ->
                     case maps:get(filename:extension(File), Imps, undefined) of
                         undefined ->
                             wings_u:message(?__(2, "Unknown file format: ")++File),
                             St;
                         Fun when is_function(Fun) ->
                             do_import(Fun, File, St)
                     end
             end,
    lists:foldl(Import, St0, Files).

%% Dummy to return props (file extensions) and fun()
import(Props, Importer, fetch_props) ->
    {Props, Importer};
%% Does not return.
import(Props, Importer, St0) ->
    Cont = fun(Name) ->
		   case ?SLOW(do_import(Importer, Name, St0)) of
		       #st{}=St -> St;
		       {error,Reason} ->
			   error_msg(?__(1,"Import failed: ") ++ Reason)
		   end
	   end,
    import_filename(Props, Cont).

do_import(Importer, Name, St0) ->
    wings_pb:start(?__(1,"reading file")),
    wings_pb:update(1.0),
    case wings_pb:done(Importer(Name)) of
	{ok,#e3d_file{}=E3DFile} ->
	    wings_import:import(E3DFile, St0);
	{error,Reason} ->
	    wings_u:error_msg(Reason)
    end.

%% import_filename([Prop], Continuation).
%%   The Continuation fun will be called like this: Continuation(Filename).
import_filename(Ps, Cont) ->
    wings_file:import_filename(Ps, Cont).

importers() ->
    Ms = ?GET(wings_plugins),
    Imps = [{M, M:menu({file,import}, [])} || M <- Ms],
    Add = fun({Props, Fun}, Acc) ->
                  case proplists:get_value(ext, Props) of
                      [_|_] = Ext ->
                          [{Ext, Fun}|Acc];
                      undefined ->
                          case proplists:get_value(extensions, Props) of
                              undefined -> Acc;
                              Exts0 ->
                                  [{Ext, Fun} || {Ext, _} <- Exts0] ++ Acc
                          end
                  end
          end,
    Props = fun({_, []}, Acc) -> Acc;
               ({M, [{_, Shortname}]}, Acc) ->
                    Add(M:command({file, {import, Shortname}}, fetch_props),Acc);
               ({M, [{_, Shortname, [option]}]}, Acc) ->
                    Cmd = M:command({file, {import, {Shortname, return}}},fetch_props),
                    Add(M:command(Cmd, fetch_props),Acc)
            end,
    maps:from_list(lists:foldl(Props, [], Imps)).

%% export([Property], ExporterFun, St)
%%  
%%  Recognized values for Property:
%%       {subdivisions,Levels}
%%       {tesselation,none|triangulation|quadrangulation}
%%       {include_colors,Bool}
%%       {include_uvs,Bool}
%%       {include_hard_edges,Bool}
%%       {include_normals,Bool}

export(none, Exporter, St) ->
    wings_export:export(Exporter, none, [], St);
export(Ps, Exporter, St) ->
    Cont = fun(Name) -> 
		   wings_export:export(Exporter, Name, Ps, St) 
	   end,
    export_filename(Ps, St, Cont).

export_selected(Props, Exporter, #st{selmode=Mode}=St0)
  when Mode =:= body; Mode =:= face ->
    F = fun(Items, We) -> export_sel_set_holes(Mode, Items, We) end,
    St1 = wings_sel:map(F, St0),
    Unselected = wings_sel:unselected_ids(St1),
    St = foldl(fun wings_obj:delete/2, St1, Unselected),
    export(Props, Exporter, St);
export_selected(_, _, _) -> error_msg(?__(1,"Select objects or faces.")).

export_sel_set_holes(body, _, We) -> We;
export_sel_set_holes(face, Faces0, #we{fs=Ftab}=We) ->
    Faces1 = gb_sets:to_list(Faces0),
    AllFaces = gb_trees:keys(Ftab),
    Holes = ordsets:subtract(AllFaces, Faces1),

    %% Create holes from the complement of the selection to prevent
    %% those faces from being exported. Normally we dissolve
    %% hole faces to save memory, but that is not necessary here 
    %% since this We is only temporary.
    wings_we:create_holes(Holes, We).

%% export_filename([Prop], Continuation).
%%   The Continuation fun will be called like this: Continuation(Filename).
export_filename(Prop, Cont) ->
    wings_file:export_filename(Prop, Cont).

%% export_filename([Prop], St, Continuation).
%%   The St will only be used to setup the default filename.
%%   The Continuation fun will be called like this: Continuation(Filename).
export_filename(Prop, St, Cont) ->
    wings_file:export_filename(Prop, St, Cont).

%% save_images(E3DFile0, Directory, DefaultFiletype) -> E3DFile
%%  Save all images in all materials, inserting the filename
%%  into each saved image. 
%%    E3DFile = #e3d_file{}
%%    DefaultFiletype = Extension (list), e.g. ".bmp".
save_images(E3DFile, Directory, Filetype) ->
    wings_export:save_images(E3DFile, Directory, Filetype).

%% dialog_template(Module, Type) -> Template
%%  Return a template for a standard dialog.
%%  Module = caller's module (used as preference key)
%%  Type = import|export
dialog_template(Mod, import) ->
    {vframe,
     [{?__(1,"Swap Y and Z Axes"),pref_get(Mod, swap_y_z, false),
       [{key,swap_y_z}]},
      {label_column,
       [{import_scale_s(),
	 {text,pref_get(Mod, import_scale, 1.0),
	  [{key,import_scale}]}},
	{"("++export_scale_s()++")",
	 {text,pref_get(Mod, export_scale, 1.0),
	  [{key,export_scale}]}}]}
     ]};
dialog_template(Mod, export) ->
    FileTypes = [{lists:flatten([Val," (*",Key,")"]),Key} ||
		    {Key,Val} <- image_formats()],
    DefFileType = pref_get(Mod, default_filetype, ".png"),
    {vframe,
     [{?__(1,"Swap Y and Z Axes"),pref_get(Mod, swap_y_z, false),
       [{key,swap_y_z}]},
      {label_column,
       [{"("++import_scale_s()++")",
	 {text,pref_get(Mod, import_scale, 1.0),
	  [{key,import_scale}]}},
	{export_scale_s(),
	 {text,pref_get(Mod, export_scale, 1.0),
	  [{key,export_scale}]}},
	{?__(6,"Sub-division Steps"),
	 {text,pref_get(Mod, subdivisions, 0),
	  [{key,subdivisions},{range,0,4}]}}]},
      panel,
      {?__(norms, "Export normals/smoothing groups"),  
       pref_get(Mod, include_normals, true), [{key,include_normals}]},
      {?__(uv,    "Export UV coordinates"),
       pref_get(Mod, include_uvs, true), [{key,include_uvs}]},
      {?__(color, "Export vertex colors"),
       pref_get(Mod, include_colors, true), [{key,include_colors}]},
      panel,
      {vframe,
       [{menu,FileTypes,DefFileType,[{key,default_filetype}]}],
       [{title,?__(7,"Default texture file type")}]} ]};
dialog_template(Mod, tesselation) ->
    {vframe,[{vradio,[{?__(tess0,"None"),none},
		      {?__(tess3,"Triangulation"),triangulate},
		      {?__(tess4,"Quadrangulation"),quadrangulate}],
	      pref_get(Mod,tesselation,none), [{key,tesselation}]}],
     [{title,?__(tess,"Tesselation")}]};

dialog_template(Mod, units) ->
    {vframe,[{vradio,[{?__(units0,"Centimeter"),centimeter},
    		      {?__(units1,"Decimeter"),decimeter},
		      {?__(units2,"Meter"),meter}],
	      pref_get(Mod,units,centimeter), [{key,units}]}],
     [{title,?__(units,"Units")}]}.
           
%% dialog_template(Module, Type, ExcludeKeys) -> Template
%%  Return a template for a standard dialog. The flags argument makes
%%  it possible to exclude certain keys.
%%  Module = caller's module (used as preference key)
%%  Type = import|export
%%  Flags = List of keys to exclude from the dialog template

dialog_template(Mod, Type, Flags) ->
    Dlg = dialog_template(Mod, Type),
    prune_keys(Dlg, ordsets:from_list(Flags)).

prune_keys({_,_,[{key,Key}|_]}=El, Exclude) ->
    case ordsets:is_element(Key, Exclude) of
	false -> El;
	true -> deleted
    end;
prune_keys({vframe,Items}, Exclude) ->
    {vframe,prune_list(Items, Exclude)};
prune_keys(Other, _) -> Other.

prune_list([I0|Is], Exclude) ->
    case prune_keys(I0, Exclude) of
	deleted -> prune_list(Is, Exclude);
	I -> [I|prune_list(Is, Exclude)]
    end;
prune_list([], _) -> [].

import_scale_s() -> ?__( 1, "Import scale").
export_scale_s() -> ?__( 1, "Export scale").

import_matrix(Attr) ->
    Scale = e3d_mat:scale(proplists:get_value(import_scale, Attr, 1.0)),
    case proplists:get_bool(swap_y_z, Attr) of
	false -> Scale;
	true ->
	    Rot = e3d_mat:rotate(-90, {1.0,0.0,0.0}),
	    e3d_mat:mul(Scale, Rot)
    end.

export_matrix(Attr) ->
    Scale = e3d_mat:scale(proplists:get_value(export_scale, Attr, 1.0)),
    case proplists:get_bool(swap_y_z, Attr) of
	false -> Scale;
	true ->
	    Rot = e3d_mat:rotate(90, {1.0,0.0,0.0}),
	    e3d_mat:mul(Scale, Rot)
    end.

%% Send a command to the plugin parent window (geometry window) 
%% from any erlang process. The command is sent to the non-deletable
%% original geometry window because the others does not surely exist.
%%
send_command(Command) ->
    wings_wm:psend(geom, {action,Command}).

%%%
%%% Preferences.
%%%
%%% As Mod, pass in ?MODULE.
%%%

pref_get(Mod, Key) ->
    wings_pref:get_value({Mod,Key}).

pref_get(Mod, Key, Default) ->
    wings_pref:get_value({Mod,Key}, Default).

pref_set(Mod, KeyVals) when is_list(KeyVals) ->
    foreach(fun({Key,Val}) ->
		    wings_pref:set_value({Mod,Key}, Val)
	    end, KeyVals).

pref_set(Mod, Key, Value) ->
    wings_pref:set_value({Mod,Key}, Value).

pref_set_default(Mod, Key, Value) ->
    wings_pref:set_default({Mod,Key}, Value).

pref_delete(Mod, Key) ->
    wings_pref:delete_value({Mod,Key}).

%%%
%%% Scene preferences.
%%%
%%% As Mod, pass in ?MODULE.
%%%
%%% The set/delete functions sets/deletes a list of values and
%%% then updates Wings global need_save state by sending a message
%%% to the geometry window, if there was a value change.
%%%

scene_pref_get(Mod, Key) ->
    wings_pref:get_scene_value({Mod,Key}).

scene_pref_get(Mod, Key, Default) ->
    wings_pref:get_scene_value({Mod,Key}, Default).

scene_pref_set(Mod, KeyVals) when is_list(KeyVals) ->
    Undefined = make_ref(),
    case
	foldl(fun({Key,Val}, NeedSave) ->
		      case wings_pref:get_scene_value({Mod,Key}, Undefined) of
			  Val -> NeedSave;
			  _ ->
			      wings_pref:set_scene_value({Mod,Key}, Val),
			      true
		      end
	      end, false, KeyVals) of
	true -> 
	    wings_wm:send(geom, need_save),
	    ok;
	false -> ok
    end.

scene_pref_set_default(Mod, KeyVals) when is_list(KeyVals) ->
    Undefined = make_ref(),
    case
	foldl(fun({Key,Val}, NeedSave) ->
		      case wings_pref:get_scene_value({Mod,Key}, Undefined) of
			  Val -> NeedSave;
			  Undefined ->
			      wings_pref:set_scene_value({Mod,Key}, Val),
			      true;
			  _ -> NeedSave
		      end
	      end, false, KeyVals) of
	true -> 
	    wings_wm:send(geom, need_save),
	    ok;
	false -> ok
    end.

scene_pref_delete(Mod, Keys) when is_list(Keys) ->
    Undefined = make_ref(),
    case
	foldl(fun(Key, NeedSave) ->
		      case wings_pref:get_scene_value({Mod,Key}, Undefined) of
			  Undefined -> NeedSave;
			  _ ->
			      wings_pref:delete_scene_value({Mod,Key}),
			      true
		      end
	      end, false, Keys) of
	true -> 
	    wings_wm:send(geom, need_save),
	    ok;
	false -> ok
    end.

%%%    
%%% Selection utilities.
%%%

sel_set(Sel, St) ->
    wings_sel:set(Sel, St).

sel_set(Mode, Sel, St) ->
    wings_sel:set(Mode, Sel, St).

sel_get(#st{sel=Sel}) ->
    Sel.

sel_map(F, St) ->
    wings_sel:map(F, St).

sel_fold(F, Acc, St) ->
    wings_sel:fold(F, Acc, St).

sel_convert(F, Mode, St) ->
    Sel = wings_sel:fold(
	    fun(Items0, #we{id=Id}=We, A) ->
		    case F(Items0, We) of
			[] -> A;
			[_|_]=Items ->
			    [{Id,gb_sets:from_list(Items)}|A];
			Items ->
			    case gb_sets:is_empty(Items) of
				true -> A;
				false -> [{Id,Items}|A]
			    end
		    end
	    end, [], St),
    wings_sel:set(Mode, Sel).

sel_edge_regions(Edges, We) ->
    wings_sel:edge_regions(Edges, We).

%% Faces must share at least one edge to belong to the same region
%% (sharing a vertex is not sufficient).
sel_face_regions(Faces, We) ->
    wings_sel:face_regions(Faces, We).

%% Faces that share at least one vertex (or edge) to belong to
%% the same region.
sel_strict_face_regions(Faces, We) ->
    wings_sel:strict_face_regions(Faces, We).

%%%
%%% Picking.
%%%

%% pick(X, Y, St0) -> {add|delete,{Id,Item,original|mirror},St}.
%%  Pick the item (body, face, edge, or vertex, depending on the selection
%%  mode in St0) and either add it to the selection or delete it from
%%  the selection.
%%
%%  The first element in the returned tuple will indicate whether
%%  something was added or deleted from the selection, and the second
%%  element will indicate which item in which object was picked.
%%
pick(X, Y, St) ->
    wings_pick:do_pick(X, Y, St).

%%%
%%% Drop support
%%%

drop(WindowName, DropData) ->
    wings_wm:send(WindowName, {drop,DropData}).

%%%
%%% Vertex functions.
%%%

vertices(#we{vp=Vtab}) ->
    wings_util:array_keys(Vtab).

vertex_pos(V, #we{vp=Vtab}) ->
    array:get(V, Vtab).

vertex_flatten(Vs, PlaneNormal, We) ->
    wings_vertex:flatten(Vs, PlaneNormal, We).

vertex_center(Vs, We) ->
    wings_vertex:center(Vs, We).

%%% Edges.

edge_loop_vertices(Edges, We) ->
    wings_edge_loop:edge_loop_vertices(Edges, We).

%%% Faces

faces(#we{fs=Ftab}) -> gb_trees:keys(Ftab).

face_vertices(Face, We) ->
    wings_face:vertices_ccw(Face, We).

face_outer_vertices_ccw(Faces, We) ->
    wings_vertex:outer_vertices_ccw(Faces, We).

face_outer_edges(Faces, We) ->
    wings_dissolve:outer_edge_partition(Faces, We).

face_dissolve(Faces, We) ->
    wings_dissolve:faces(Faces, We).

face_dissolve_complement(Faces, We) ->
    wings_dissolve:complement(Faces, We).

%%% Objects.

obj_name(#we{name=Name}) -> Name.
obj_id(#we{id=Id}) -> Id.

%%%
%%% Camera info.
%%%

camera_info(As) ->
    wings_view:camera_info(As, wings_view:current()).

%%%
%%% Get all lights.
%%%

lights(St) ->
    case wings_light:export(St) of
	[] -> wings_light:export_camera_lights();
	L -> L
    end.

import_lights(Lights, St) ->
    wings_light:import(Lights, St).

%%%
%%% Images.
%%%

image_formats() ->
    wings_image:image_formats().

image_read(Ps) ->
    wings_image:image_read(Ps).

image_write(Ps) ->
    wings_image:image_write(Ps).

%%%
%%% Virtual mirror.
%%%

vm_freeze(We) -> wings_we:freeze_mirror(We).

%%%
%%% Tesselation/subdivision.
%%%

triangulate(We) ->
    wings_tesselation:triangulate(We).

triangulate(Faces, We) ->
    wings_tesselation:triangulate(Faces, We).

quadrangulate(We) ->
    wings_tesselation:quadrangulate(We).

quadrangulate(Faces, We) ->
    wings_tesselation:quadrangulate(Faces, We).

%%%
%%% Console
%%%

popup_console() ->
    wings_console:popup_window().

%% Return version string.
version() ->
    ?WINGS_VERSION.

