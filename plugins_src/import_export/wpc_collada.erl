%%
%%  wpc_collada.erl --
%%
%%     Collada export.
%%
%%  Copyright (c) 2009-2011 David Parfitt
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_collada).
-export([init/0,menu/2,command/2]).
-import(lists, [map/2,foldl/3,keyfind/3, mapfoldl/3,flatten/1]).

-define(DEF_IMAGE_TYPE, ".bmp").

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Collada export state record
-record(c_exp,
	{
	  geoms = [],                   % Collada geometry nodes
	  objnames = [],                % list of object names
	  matl_defs = gb_trees:empty(), % defined materials
	  visualscenenodes = []         % list of visualscenenodes
	 }
       ).

-record(c_matl,
	{
	  type :: 'simple' | 'tx',      % simple material OR mapped image
	  mname = [],                   % material name
	  txname= [],                   % texture name
	  txfilename = [],	        % texture filename
	  surface = [],                 % Collada surface node
	  sampler2d = [],               % Collada sampler node
	  effect,                       % Collada effect node
	  diffnode                      % diffuse node (a color or an image)
	 }
       ).

init() ->
    %% Collada specifies an "up_axis" parameter, so your
    %% model should usually be the "right way up"
    wpa:pref_set_default(?MODULE, swap_y_z, false),
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

menu_entry(Menu) ->
    Menu ++ [{"Collada (.dae)...", dae,[option]}].

command({file,{export,{dae,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{dae,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"Collada Export Options"), dialog(export),
	       fun(Res) ->
		       {file,{Op,{dae,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Uvs = proplists:get_bool(include_uvs, Attr),
    Units = proplists:get_value(units, Attr),    
    %% If smoothing groups are not wanted, we'll turn off
    %% export of hard edges. That will create only one smoothing group.
    HardEdges = proplists:get_bool(include_normals, Attr),
    Ps = [{include_uvs,Uvs},{units,Units},{include_hard_edges,HardEdges},
	  {subdivisions,SubDivs},{include_hard_edges,HardEdges}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Filetype = proplists:get_value(default_filetype, Attr, ?DEF_IMAGE_TYPE),
    Contents1 = wpa:save_images(Contents0,
				filename:dirname(Filename), Filetype),
    %%io:format("Number of objects = ~p~n",[length(Contents1#e3d_file.objs)]),
    %%io:format("Filename = ~p~n",[Filename]),
    Contents2 = export_transform(Contents1, Attr),
    %% Export is a record of data collected while iterating over geometry
    %% and materials.
    ExportState0 = #c_exp{},
    #e3d_file{objs=Objs,mat=Mat} = Contents2,
    ExportState1 = foldl(fun (O, S) ->
				 make_geometry(O, S, Mat)
				 end,
			 ExportState0,Objs),
    LibraryGeometryNode = {library_geometries,ExportState1#c_exp.geoms},
    Asset = make_asset(Attr),
    LibraryVisualSceneNode = make_library_visual_scene(ExportState1),
    LibraryEffectsNode = make_library_effects(ExportState1),
    LibraryImagesNode = make_library_images(ExportState1),
    %% TODO
    %%Lights = proplists:get_value(lights, Attr, []),
    LibraryMaterialsNode = make_library_materials(ExportState1),
    SceneNode = make_scene(),
    ColladaNodes = ["\n",Asset,"\n",LibraryEffectsNode,"\n",
		    LibraryImagesNode,"\n",LibraryMaterialsNode,"\n",
		    LibraryGeometryNode,"\n",LibraryVisualSceneNode,"\n",SceneNode,"\n"],
    ColladaAtts = [#xmlAttribute{name='version',value='1.4.0'},
		   #xmlAttribute{name='xmlns',
		   value='http://www.collada.org/2005/11/COLLADASchema'}],
    Collada = #xmlElement{name='COLLADA',content=ColladaNodes,
			  attributes=ColladaAtts},
    FileContents = xmerl:export_simple(["\n",Collada],xmerl_xml),
    ok = file:write_file(Filename, [FileContents]).

make_library_materials(#c_exp{matl_defs=MatlDefs}) ->
    Matls = map(fun (Matl) ->
			MatlName = atom_to_list(Matl),
			InstanceEffect = {instance_effect,
					  [{url,"#" ++ MatlName ++ "-fx"}],[]},
			{material,[{id,MatlName},
				   {name,MatlName}],[InstanceEffect]}
		end, gb_trees:keys(MatlDefs)),
    {library_materials,[],Matls}.

make_library_effects(#c_exp{matl_defs=MatlDefs}) ->
    Materials = gb_trees:values(MatlDefs),

    Effects = map(fun (#c_matl{effect=Effect}) ->
			  Effect
		  end, Materials),
    {library_effects,[],Effects}.

make_library_images(#c_exp{matl_defs=MatlDefs}) ->
    Materials = gb_trees:values(MatlDefs),
    Images=map(fun (#c_matl{txfilename=Filename}) ->
			     InitFrom = {init_from,[],[Filename]},
			     Id = filename_to_id(Filename) ++ "-img",
			     {image,[{id,Id},{name,Id}],[InitFrom]}
		     end,Materials),
    {library_images,[],Images}.

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

dialog(Type) ->
    [wpa:dialog_template(?MODULE, units), panel, 
     wpa:dialog_template(?MODULE, Type, [include_colors])].

props() ->
    [{ext,".dae"},{ext_desc,?__(1,"Collada file")}].

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

num_to_text(Num) ->
    flatten(io_lib:format("~.10B",[Num])).

make_geometry(#e3d_object{name=Name,obj=Obj}, ExpState0, Mat) ->
    %% TODO: make split by material an option?
    %%ObjMesh = e3d_mesh:vertex_normals(Obj),
    %%Meshes = e3d_mesh:split_by_material(ObjMesh),
    make_geometry1([Obj],Name,0,ExpState0,Mat).

%% divide the faces of a mesh up by the material of each face.
%% maintains correct indexing per object. this is not the same as
%% e3d_mesh:split_by_material.
%% it doesn't create any new objects.
segment_by_material(#e3d_mesh{fs=Fs}) ->
    FacesByMaterial0 = gb_trees:empty(),
    {Segs,_} = foldl(fun (#e3d_face{mat=Mats}=Face, {Tree0,Index}) ->
		  Tree1 = case gb_trees:lookup(Mats, Tree0) of
			     {value,_} ->
				 FaceList0 = gb_trees:get(Mats, Tree0),
				 FaceList = [{Index,Face}|FaceList0],
				 gb_trees:update(Mats, FaceList, Tree0);
			     none ->
				  gb_trees:insert(Mats, [{Index,Face}], Tree0)
			 end,
		  {Tree1,Index+1}
	  end, {FacesByMaterial0,0}, Fs),
    Segs.

make_scene_node(ObjName, ObjMats) ->
    IMs = map(fun (Mat)->
		      BVI = {bind_vertex_input,[{input_semantic,"TEXCOORD"},
						{input_set,"1"},
						{semantic,"CHANNEL1"}],[]},
		      Target = "#" ++ atom_to_list(Mat),
		      {instance_material,[{symbol,Mat},{target,Target}],[BVI]}
	      end, ObjMats),
    TC = {technique_common,[],IMs},
    BM = {bind_material,[],[TC]},
    IG = {instance_geometry,[{url,"#" ++ ObjName}],[BM]},
    Translate = {translate,[{sid,"translate"}],["0.00000 0.00000 0.00000"]},
    RotateZ = {rotate,[{sid,"rotateZ"}],["0 0 1 0.00000"]},
    RotateY = {rotate,[{sid,"rotateY"}],["0 1 0 0.00000"]},
    RotateX = {rotate,[{sid,"rotateX"}],["1 0 0 0.00000"]},
    Scale   = {scale,[{sid,"scale"}],["1.0000 1.0000 1.0000"]},
    {node,[{layer,"L1"},{id,ObjName},{name,ObjName}],
     [Translate,"/n",RotateZ,RotateY,RotateX,Scale,IG]}.

make_geometry1([], _, _, Acc, _) ->
    Acc;
make_geometry1([Mesh | Meshes], Prefix, Counter,
	       #c_exp{objnames=ObjNames,
		      visualscenenodes=VisualSceneNodes}=ExpState0,
	       MatDefs) ->
    Name = Prefix ++ "-" ++ num_to_text(Counter),
    FacesByMaterial = segment_by_material(Mesh),
    {ExpState1,MatsForObj} = foldl(
			  fun (TMat, {TSt,Acc}) ->
				  FirstMatName = hd(TMat),
				  {use_material(FirstMatName, MatDefs, TSt),
				   gb_sets:add_element(FirstMatName,Acc)}
			  end,{ExpState0, gb_sets:new()},
			       gb_trees:keys(FacesByMaterial)),
    VisualSceneNode = make_scene_node(Name, gb_sets:to_list(MatsForObj)),
    ExpState2 = ExpState1#c_exp{objnames=[Name|ObjNames],
				visualscenenodes=[VisualSceneNode|VisualSceneNodes]},
    PosNode = make_mesh_source_pos(Name,Mesh),
    NormalsNode = make_mesh_source_normals(Name,Mesh),
    VertsNode = make_mesh_verts(Name),
    %% EmptyIndex is used for materials that don't have UV coords
    {UVsNode,EmptyIndex} = make_mesh_uvs(Name,Mesh),
    {PolysNode,_} = mapfoldl(fun (MatFaces, TxEmptyIndex) ->
			    {make_polylist(Name, MatFaces, TxEmptyIndex),
			     TxEmptyIndex}
		    end, EmptyIndex, gb_trees:values(FacesByMaterial)),
    MeshNode = {mesh,[PosNode, NormalsNode, UVsNode, VertsNode] ++ PolysNode},
    GeomNode = {geometry,[{name,Name},{id,Name}],[MeshNode]},
    ExpState3 = ExpState2#c_exp{geoms=[GeomNode|ExpState2#c_exp.geoms]},
    make_geometry1(Meshes, Prefix, Counter+1, ExpState3, MatDefs).

use_material(Name, MatDefs, #c_exp{matl_defs=ExpMatlDefs}=ExportState) ->
   case gb_trees:is_defined(Name, ExpMatlDefs) of
           false -> define_material(Name, lookup(Name, MatDefs), ExportState);
           true -> ExportState
       end.

define_material(_, undefined, ExportState) -> 
    ExportState;
define_material(Name, ThisMat, #c_exp{matl_defs=ExpMatlDefs}=ExportState) ->
    OpenGLMat = lookup(opengl, ThisMat),
    {Ar,Ag,Ab,O} = lookup(ambient, OpenGLMat),
    {Dr,Dg,Db,_} = lookup(diffuse, OpenGLMat),
    {Er,Eg,Eb,_} = lookup(emission, OpenGLMat),
    {Sr,Sg,Sb,_} = lookup(specular, OpenGLMat),
    Emission = {emission,[],[make_color(Er, Eg, Eb, 1.0)]},
    Ambient = {ambient,[],[make_color(Ar, Ag, Ab, O)]},
    Diffuse = {diffuse,[],[make_color(Dr, Dg, Db, 1.0)]},
    Specular = {specular,[],[make_color(Sr, Sg, Sb, 1.0)]},
    ColladaMatl0 = #c_matl{mname=Name,type='simple',diffnode=Diffuse},
    S = io_lib:format("~f",[lookup(shininess, OpenGLMat)]),
    Shininess = {shininess,[],[{float,[],["" ++ S]}]},
    ColladaMatl1 = case keyfind(maps, 1, ThisMat) of
	{maps,Maps} ->
	    case keyfind(diffuse, 1, Maps) of
		{diffuse,#e3d_image{filename=DiffFilename,name=DiffName}} ->
		    FileId = filename_to_id(DiffFilename),
		    DiffSampler2D = make_mat_sampler2D(FileId),
		    DiffSurface = make_mat_surface(FileId),
		    DiffTx = {texture,[{texcoord,"CHANNEL1"},
				       {texture,FileId ++ "-sampler"}],[]},
		    ColladaMatl0#c_matl{txname=DiffName,
					txfilename=DiffFilename,
					surface=DiffSurface,
					sampler2d=DiffSampler2D,type='tx',
					diffnode={diffuse,[],[DiffTx]}};
		_ -> ColladaMatl0
	    end;
	_ -> ColladaMatl0
    end,
    Children = [Emission,Ambient,Specular,Shininess,
		ColladaMatl1#c_matl.diffnode],
    Phong = {phong,[],Children},
    Technique = {technique,[{sid,"wings3d"}],[Phong]},
    ProfileChildren = [ColladaMatl1#c_matl.surface,
		       ColladaMatl1#c_matl.sampler2d,Technique],
    FXName = make_fx_name(Name),
    ProfileCommon = {profile_COMMON,[],ProfileChildren},
    Effect = {effect,[{id,[FXName]},{name,[FXName]}],[ProfileCommon]},
    ColladaMatl2 = ColladaMatl1#c_matl{effect=Effect},
    ExpMatlDefs1 = gb_trees:insert(Name, ColladaMatl2, ExpMatlDefs),
    ExportState#c_exp{matl_defs=ExpMatlDefs1}.

make_fx_name(Name) when is_atom(Name)->
    atom_to_list(Name) ++ "-fx";
make_fx_name(Name) ->
    Name ++ "-fx".

make_mat_surface(Id) ->
    InitFrom = {init_from,[],[Id ++ "-img"]},
    Format = {format,[],["A8R8G8B8"]},
    Surface = {surface,[{type,"2D"}],[InitFrom, Format]},
    {newparam,[{sid,Id ++ "-surface"}],[Surface]}.

make_mat_sampler2D(Id) ->
    Source = {source,[],[Id ++ "-surface"]},
    MinFilter = {minfilter,[],["LINEAR_MIPMAP_LINEAR"]},
    MagFilter = {magfilter,[],["LINEAR"]},
    Samp2d = {sampler2D,[],[Source,MinFilter,MagFilter]},
    {newparam,[{sid,Id ++ "-sampler"}],[Samp2d]}.

filename_to_id(AbsFileName) ->
    string:join(string:tokens(filename:basename(AbsFileName), "."), "_").

make_color(R, G, B, A) ->
    Color = io_lib:format("~f ~f ~f ~f~n",[R, G, B, A]),
    {color,[],[Color]}.

%% from wpc_wrl.erl
lookup(K, L) ->
    proplists:get_value(K, L).

make_mesh_source_pos(Name, Mesh) ->
    Verts0 = Mesh#e3d_mesh.vs,
    Verts = triple_to_array(Verts0),
    FloatArray = make_floatarray(Name ++ "-Pos-array",
				 fun floatlist_to_string/1, Verts),
    TechCommon = {technique_common,
		  [make_source_accessor(Name ++ "-Pos-array",
					length(Verts))]},
    {source,[{id,Name ++ "-Pos"}],[FloatArray,TechCommon]}.

make_mesh_source_normals(Name, Mesh) ->
    Normals = triple_to_array(get_normals(Mesh)),
    FloatArray = make_floatarray(Name ++ "-Normal-array",
				 fun floatlist_to_string/1, Normals),
    TechCommon = {technique_common,
		  [make_source_accessor(Name ++ "-Normal-array",
					length(Normals))]},
    {source,[{id,Name ++ "-Normal"}],[FloatArray,TechCommon]}.

make_mesh_uvs(Name, #e3d_mesh{tx=Tx}) ->
    %% the last UV coord is used by the default material
    UVs = double_to_array(Tx) ++ [0.0000,0.0000],
    FloatArray = make_floatarray(Name ++ "-UV-array",
				 fun floatlist_to_string/1, UVs),
    TechCommon = {technique_common,
		  [make_uvsource_accessor(Name ++ "-UV-array",
					  length(UVs))]},
    {{source,[{id,Name ++ "-UV"}],
      [FloatArray,TechCommon]},(length(UVs) div 2)-1}.

make_mesh_verts(Name) ->
    Input = {input,[{semantic,"POSITION"},{source,"#" ++ Name ++ "-Pos"}],[]},
    {vertices,[{id,Name ++ "-Vtx"}],[Input]}.

%% inefficient?
triple_to_array([]) -> [];
triple_to_array([{X,Y,Z}|Tail]) -> [X,Y,Z|triple_to_array(Tail)].

double_to_array([]) -> [];
double_to_array([{U,V}|Tail]) -> [U,V|double_to_array(Tail)].

%% all lists must be same length
%% make a list of Faces, Normals, and UVs
make_fnuv_in_order([], _, []) ->
    [];
make_fnuv_in_order([Face|Faces], NormalIndex, [UV|UVs]) ->
    [Face,NormalIndex,UV|make_fnuv_in_order(Faces, NormalIndex, UVs)].

%% make a list of Faces, Normals, and UVs,
%% use the normal index as the UV coord
make_fn_in_order([], _) ->
    [];
make_fn_in_order([Face | Faces], NormalIndex) ->
    [Face,NormalIndex,NormalIndex|make_fn_in_order(Faces, NormalIndex)].

get_normals(Mesh) ->
    gb_trees:values(face_normals(Mesh#e3d_mesh.fs,
				 list_to_tuple(Mesh#e3d_mesh.vs))).

%% from e3d_mesh
face_normals(Ftab, Vtab) ->
    {Ns,_} = mapfoldl(fun(#e3d_face{vs=Vs0}, Face) ->
				    Vs = [element(V+1, Vtab) || V <- Vs0],
				    {{Face,e3d_vec:normal(Vs)},Face+1}
			    end, 0, Ftab),
    gb_trees:from_orddict(Ns).

make_vcount_node(CountList) ->
    VCountData = flatten(string:join(map(
				       fun (Count) ->
					       io_lib:format("~w",[Count]) end,
				       CountList), " ")),
    {vcount,[VCountData]}.

%% without UVs
make_p_data(Indices, _, [], EmptyIndex) ->
    List = make_fn_in_order(Indices, EmptyIndex),
    PData = flatten(string:join(map(
				  fun (Index) ->
					  io_lib:format("~w",[Index]) end,
				  List), " ")),
    PData;
%% with UVs
make_p_data(Indices, NormalIndex, Tx,_) ->
    %% VERTEX NORMAL TEXCOORD ... repeated
    List = make_fnuv_in_order(Indices, NormalIndex, Tx),
    PData = flatten(string:join(map(
				      fun (Index) ->
					      io_lib:format("~w",[Index]) end,
				      List), " ")),
    PData.

%% MatFaces is the set of faces that have the same material
make_polylist(Name, MatFaces, EmptyIndex)->
    {Ps,_} = mapfoldl(fun ({NormalIndex,
			    #e3d_face{tx=FaceTx,vs=Indices}},
			   TxEmptyIndex) ->
			      {make_p_data(Indices, NormalIndex,
					   FaceTx, TxEmptyIndex),TxEmptyIndex}
		      end,EmptyIndex,MatFaces),

    {_,#e3d_face{mat=[Material|_]}} = hd(MatFaces),
    PNodeData = string:join(Ps, " "),
    PNode = {p,[PNodeData]},
    CountPs = map(fun ({_,Face}) -> length(Face#e3d_face.vs) end, MatFaces),
    VInput = {input,[{offset,"0"},{semantic,"VERTEX"},
		     {source,"#" ++ Name ++ "-Vtx"}],[]},
    NInput = {input,[{offset,"1"},{semantic,"NORMAL"},
		     {source,"#" ++ Name ++ "-Normal"}],[]},
    UVInput = {input,[{offset,"2"},{semantic,"TEXCOORD"},
		      {source,"#" ++ Name ++ "-UV"}],[]},
    VCount = make_vcount_node(CountPs),
    {polylist,[{count,num_to_text(length(MatFaces))},
	       {material,Material}],[VInput, NInput, UVInput, VCount, PNode]}.


make_floatarray(Name, F, Data) ->
    Count = length(Data),
    FloatData = F(Data),
    {'float_array',[{id,Name},{count,num_to_text(Count)}],[FloatData]}.

make_uvsource_accessor(Source, TotalVertCount) ->
    FaceCount = TotalVertCount div 2,
    S = {param,[{name,"S"},{type,"float"}],[]},
    T = {param,[{name,"T"},{type,"float"}],[]},
    {accessor,[{count,num_to_text(FaceCount)},
	       {source,"#" ++ Source},{stride,"2"}],[S,T]}.

make_source_accessor(Source, TotalVertCount) ->
    FaceCount = TotalVertCount div 3,
    X = {param,[{name,"X"},{type,"float"}],[]},
    Y = {param,[{name,"Y"},{type,"float"}],[]},
    Z = {param,[{name,"Z"},{type,"float"}],[]},
    {accessor,
     [{count,num_to_text(FaceCount)},{source,"#" ++ Source},{stride,"3"}],
     [X,Y,Z]}.
  
make_asset(Attr) when is_list(Attr) ->
    Units = proplists:get_value(units, Attr, centimeter),     

    Author = {author,["Wings3D Collada Exporter"]},
    AuthoringTool = {authoring_tool,
		     ["Wings3D " ++ wpa:version() ++ " Collada Exporter"]},
    Comments = {comments,[]},
    Copyright= {copyright,[]},
    Sourcedata = {source_data,[]},
    Contributor = {contributor,[],
		   ["\n",Author,"\n",AuthoringTool,"\n",Comments,"\n",Copyright,"\n",Sourcedata,"\n"]},

    UnitsXml =  case Units of
		    centimeter -> {unit,[{meter,'0.01'},{name,'centimeter'}],[]};
		    decimeter -> {unit,[{meter,'0.1'},{name,'decimeter'}],[]};    
		    meter -> {unit,[{meter,'1.0'},{name,'meter'}],[]};
		    _ ->
			" "
		end,	

    UpAxis = {up_axis,["Y_UP"]},
    CurrentDateTime = now_as_xml_dateTime(),
    {asset,[],
     ["\n",Contributor,"\n",{created,[CurrentDateTime]},"\n",
      {modified,[CurrentDateTime]},"\n",UnitsXml,"\n",UpAxis,"\n"]}.

make_scene() ->
    {scene,[{instance_visual_scene,[{url,"#Scene"}],[]}]}.

make_library_visual_scene(#c_exp{visualscenenodes=VisualSceneNodes}) ->
    VisualScene = {visual_scene,[{id,"Scene"},{name,"Scene"}],VisualSceneNodes},
    {library_visual_scenes,[VisualScene]}.

floatlist_to_string(List) ->
    flatten(string:join(map(fun (Index) ->
				    io_lib:format("~f",[Index]) end,
			    List), " ")).

now_as_xml_dateTime() ->
    Now = erlang:now(),
    {{Year,Month,Day},{Hour,Minutes,Seconds}} =
	calendar:now_to_universal_time(Now),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
	      [Year, Month, Day, Hour, Minutes, Seconds]).

%% keep this for debugging
%% show_xml(Node) ->
%%    io:format("~p~n",[flatten(xmerl:export_simple([Node], xmerl_xml))]).
