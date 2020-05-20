%%
%%  wings_import.erl --
%%
%%     This module handles import of foreign objects.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_import).
-export([import/2,import_mesh/2]).

-include_lib("wings/e3d/e3d.hrl").
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,sort/1]).

%%-define(DUMP, 1).

import(File0, St) ->
    wings_pb:start("importing"),
    #e3d_file{objs=Objs,dir=Dir0} = distribute_materials(File0),
    Dir = get_file_directory(Dir0),
    N = length(Objs),
    Faces = foldl(fun(#e3d_object{obj=#e3d_mesh{fs=Ftab}}, S) ->
			  S+length(Ftab)+1
		  end, 0, Objs),
    wings_pb:done(translate_objects(Objs, 1, N, 0, Faces, Dir, St)).

get_file_directory(undefined) ->
    wings_pref:get_value(current_directory);
get_file_directory(Dir) when is_list(Dir) -> Dir.

translate_objects([#e3d_object{name=Name,mat=Mat,obj=#e3d_mesh{fs=Fs}}=Obj|Os],
		  I, N, Fsum0, Faces, Dir, St0) ->
    Fsum = Fsum0 + length(Fs) + 1,
    wings_pb:update(Fsum/Faces,
		    integer_to_list(I) ++ " of " ++ integer_to_list(N)),
    We0 = import_object(Obj),
    {St1,NameMap} = wings_material:add_materials(Mat, Dir, St0),
    We1 = rename_materials(NameMap, We0),
    We = import_attributes(We1, Obj),
    St = store_object(Name, We, St1),
    translate_objects(Os, I+1, N, Fsum, Faces, Dir, St);
translate_objects([], _, _, _, _, _, St) -> St.

import_attributes(We, #e3d_object{attr=Attr}) ->
    Visible = proplists:get_value(visible, Attr, true),
    Locked = proplists:get_value(false, Attr, false),
    P0 = case Visible of
	     true -> 0;
	     false -> ?PERM_HIDDEN_BIT
	 end,
    P = case Locked of
	    true -> P0 bor ?PERM_LOCKED_BIT;
	    false -> P0
	end,
    We#we{perm=P}.

store_object(undefined, We, #st{onext=Oid}=St) ->
    Name = "unnamed_object" ++ integer_to_list(Oid),
    wings_obj:new(Name, We, St);
store_object(Name, We, St) ->
    wings_obj:new(Name, We, St).

import_object(#e3d_object{name=_Name,obj=Mesh0}) ->
    %%io:format("\n~s:\n", [_Name]),
    Mesh1 = e3d_mesh:merge_vertices(Mesh0),
    Mesh2 = e3d_mesh:clean_faces(Mesh1),
    Mesh3 = e3d_mesh:transform(Mesh2),
    Mesh  = e3d_mesh:hard_edges_from_normals(Mesh3),
    import_mesh(material, Mesh).

-define(P(N), {N,fun N/2}).
-define(P_NOFAIL(N), {nofail,N,fun N/2}).

import_mesh(ObjType, Mesh) ->
    Pss0 = [{seq,[?P_NOFAIL(imp_build),
		  ?P_NOFAIL(imp_orient_normals),
		  ?P_NOFAIL(imp_build)]},
	    ?P(imp_partition)],
    Pss = polygonify_passes(Pss0, Mesh),
    run(Pss, ObjType, Mesh).

imp_polygons(_, Mesh) ->
    e3d_mesh:make_polygons(Mesh).
     
imp_quads(_, Mesh) ->
    e3d_mesh:make_quads(Mesh).

imp_build(ObjType, Mesh) ->
    wings_we:build(ObjType, Mesh).

imp_partition(ObjType, Mesh0) ->
    wings_pb:start(""),
    wings_pb:update(0.20, "retrying"),
    Meshes = e3d_mesh:partition(Mesh0),
    case length(Meshes) of
	1 ->
	    wings_pb:update(0.50, "retrying");
	N ->
	    wings_pb:update(0.50, "retrying ("++
			    integer_to_list(N)++" sub-objects)")
    end,
    Pss0 = [?P(imp_build),
	    ?P_NOFAIL(imp_orient_normals),
	    ?P_NOFAIL(imp_build),
	    ?P(imp_rip_apart)],
    Pss = polygonify_passes(Pss0, Mesh0),
    Wes = [run(Pss, ObjType, Mesh) || Mesh <- Meshes],
    wings_pb:done(wings_we:merge(Wes)).

imp_orient_normals(_, Mesh) ->
    e3d_mesh:orient_normals(Mesh).

imp_rip_apart(ObjType, Mesh) ->
    dump(Mesh),
    Wes = rip_apart(ObjType, Mesh),
    wings_we:merge(Wes).

polygonify_passes(Pss, #e3d_mesh{type=triangle}) ->
    [{seq,[?P_NOFAIL(imp_polygons),?P_NOFAIL(imp_build),
	   ?P_NOFAIL(imp_orient_normals),?P_NOFAIL(imp_build)]},
     {seq,[?P_NOFAIL(imp_quads),?P_NOFAIL(imp_build),
	   ?P_NOFAIL(imp_orient_normals),?P_NOFAIL(imp_build)]}|Pss];
polygonify_passes(Pss, _) -> Pss.

run([{seq,Seq}|Ps], ObjType, Mesh) ->
    try
	run(Seq, ObjType, Mesh)
    catch
	_:_ ->
	    run(Ps, ObjType, Mesh)
    end;
run([{nofail,_Name,Final}|Fs], ObjType, Mesh0) ->
    %%io:format("~p\n", [_Name]),
    case Final(ObjType, Mesh0) of
	#we{}=We -> We;
	#e3d_mesh{}=Mesh -> run(Fs, ObjType, Mesh);
	error -> run(Fs, ObjType, Mesh0)
    end;
run([{_Name,Final}], ObjType, Mesh0) ->
    %%io:format("~p\n", [_Name]),
    case Final(ObjType, Mesh0) of
	#we{}=We -> We
    end;
run([{_Name,F}|Fs], ObjType, Mesh0) ->
    %%io:format("~p\n", [_Name]),
    try F(ObjType, Mesh0) of
	#we{}=We -> We;
	#e3d_mesh{}=Mesh -> run(Fs, ObjType, Mesh);
        error -> run(Fs, ObjType, Mesh0)
    catch
	_:_R ->
	    %%_Stack = erlang:get_stacktrace(),
	    %%io:format("~p failed: ~P\n", [_Name,_R,10]),
	    %%io:format(" ~P\n", [_Stack,20]),
	    run(Fs, ObjType, Mesh0)
    end.

rip_apart(Mode, Mesh) ->
    rip_apart_1(Mesh, Mode, []).

rip_apart_1(#e3d_mesh{fs=Fs}=Mesh0, Mode, Acc0) ->
    case length(Fs) of
	N when N > 512 ->
	    Mid = N div 2,
	    First = lists:sublist(Fs, Mid),
	    Second = lists:nthtail(Mid, Fs),
 	    Mesh1 = e3d_mesh:renumber(Mesh0#e3d_mesh{fs=First}),
 	    Mesh2 = e3d_mesh:renumber(Mesh0#e3d_mesh{fs=Second}),
	    Acc = rip_apart_1(Mesh1, Mode, Acc0),
	    rip_apart_1(Mesh2, Mode, Acc);
	_ ->
	    rip_apart_2(Fs, Mode, Mesh0, Acc0)
    end.

rip_apart_2([#e3d_face{}=Face|T], Mode, Template, Acc) ->
    Fs = [Face],
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Fs,he=[]}),
    #we{} = We = wings_we:build(Mode, Mesh),
    rip_apart_2(T, Mode, Template, [We|Acc]);
rip_apart_2([], _, _, Wes) -> Wes.

%% rename_materials(NameMap, We0) -> We
rename_materials([], We) -> We;
rename_materials([_|_]=NameMap0, We) ->
    NameMap = gb_trees:from_orddict(sort(NameMap0)),
    rename_materials(NameMap, We);
rename_materials(NameMap, We) ->
    MatTab0 = wings_facemat:all(We),
    MatTab = foldl(fun({Face,Mat0}=Pair, A) ->
			   case gb_trees:lookup(Mat0, NameMap) of
			       none -> [Pair|A];
			       {value,Mat} -> [{Face,Mat}|A]
			   end
		   end, [], MatTab0),
    wings_facemat:assign(MatTab, We).

%% If there are materials in the #e3d_file{} record, distribute
%% them down to each object.
distribute_materials(#e3d_file{objs=Objs0,mat=Mat0}=File) ->
    Mat = sofs:relation(Mat0, [{name,data}]),
    Objs = distribute_materials_1(Objs0, Mat),
    File#e3d_file{objs=Objs}.

distribute_materials_1([#e3d_object{mat=[]}=Obj0|T], Mat) ->
    %% No material in the #e3d_object{} - use material from %e3d_file{}.
    Obj = distribute_materials_2(Obj0, Mat),
    [Obj|distribute_materials_1(T, Mat)];
distribute_materials_1([#e3d_object{mat=ObjMat0}=Obj0|T], Mat) ->
    %% Use the material from the #e3d_object{} itself.
    ObjMat = sofs:relation(ObjMat0, [{name,data}]),
    Obj = distribute_materials_2(Obj0, ObjMat),
    [Obj|distribute_materials_1(T, Mat)];
distribute_materials_1([], _) -> [].

%% distribute_materials_2(Obj, Mat)
%%  Remove any material not used; added default definitions for any
%%  material referenced but not defined.
distribute_materials_2(#e3d_object{obj=Mesh}=Obj, Mat) ->
    Used0 = e3d_mesh:used_materials(Mesh),
    Used = sofs:from_external(Used0, [name]),
    ObjMat0 = sofs:restriction(Mat, Used),
    ObjMat1 = sofs:extension(ObjMat0, Used, sofs:from_term([], data)),
    ObjMat = sofs:to_external(ObjMat1),
    Obj#e3d_object{mat=ObjMat}.

-ifndef(DUMP).
dump(_) -> ok.
-else.
dump(Mesh) ->
    TempName = "bad_object.obj",
    File = #e3d_file{objs=[#e3d_object{name="bad",obj=Mesh}]},
    e3d_obj:export(TempName, File).
-endif.
