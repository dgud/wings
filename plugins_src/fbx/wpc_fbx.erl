%%
%%  wpc_fbx.erl --
%%
%%     FBX file import/export.
%%
%%  Copyright (c) 2003-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_fbx.erl,v 1.10 2006/01/20 14:47:02 giniu Exp $
%%

-module(wpc_fbx).

-export([init/0,menu/2,command/2]).

-include("e3d.hrl").
-include("e3d_image.hrl").
-include("fbx_ops.hrl").
-include("wings_intl.hrl").

-import(lists, [reverse/1,reverse/2,foldl/3,sort/1,keydelete/3,foreach/2]).

-define(DEF_IMPORT_SCALE, 0.1).
-define(DEF_EXPORT_SCALE, 10.0).

init() ->
    Dir = filename:dirname(code:which(?MODULE)),
    case os:type() of
	{win32,_} -> init_1(Dir, "wings_fbx_win32", ".dll");
	{unix,darwin} -> init_1(Dir, "wings_fbx_mac", ".so");
	{unix,linux} -> init_1(Dir, "wings_fbx_linux", ".so");
	_ -> false
    end.

init_1(Dir, Name, Ext) ->
    case filelib:is_file(filename:join(Dir, Name++Ext)) of
	false -> false;
	true ->
	    case erl_ddll:load_driver(Dir, Name) of
		ok -> init_2(Name);
		{error,Reason} ->
		    io:format("Failed to load ~s in ~s\n~s\n",
			      [Name,Dir,erl_ddll:format_error(Reason)]),
		    false
	    end
    end.

init_2(Name) ->
    case open_port({spawn,Name},[]) of
	Port when is_port(Port) ->
	    register(wings_fbx_port, Port),
	    true;
	_ ->
	    false
    end.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{fbx,Ask}}}, St) ->
    do_import(Ask, St);
command({file,{export,{fbx,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{fbx,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Alias FBX (.fbx)...",fbx,[option]}].

props() ->
    [{ext,".fbx"},{ext_desc,?__(1,"Alias FBX file")}].

%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"FBX Export Options"), dialog(export),
	       fun(Res) ->
		       {file,{Op,{fbx,Res}}}
	       end);
do_export(Attr, _Op, Exporter, St) ->
    set_pref(Attr),
    Ps = props(),
    Exporter(Ps, export_fun(Attr, St)).

export_fun(Attr, St) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr, St)
    end.

export_1(Filename, Contents0, Attr, St) ->
    #e3d_file{objs=Objs,mat=Mat0} = export_transform(Contents0, Attr),
    Mat = export_images(Mat0, filename:dirname(Filename)),
    Lights = wings_light:export(St),
    cast(?ExpInitialize),
    N = length(Objs) + length(Lights),
    cast(?ExpNumObjects, <<N:32/native>>),
    export_2(reverse(Objs), Mat),
    ExpScale = export_scale(Attr),
    export_lights(Lights, ExpScale),
    cast(?ExpLinkObjects),
    Password = proplists:get_value(password, Attr, []),
    Format = file_format(Attr),
    cast(?ExpSetFileFormat, [Format]),
    cast(?ExpSaveScene, [Filename,0,Password,0]),
    cast(?ExpDestroy),
    ok.

export_2([#e3d_object{name=Name,obj=Obj}|T], Mat) ->
    export_object(Name, Obj, Mat),
    export_2(T, Mat);
export_2([], _) -> ok.

file_format(Attr) ->
    case {proplists:get_bool(ascii_format, Attr),
	  proplists:get_value(fbx_format, Attr, fbx_6)} of
	{false,fbx_5} -> ?ExpFileFormatFbx5;
	{false,fbx_6} -> ?ExpFileFormatFbx6;
	{true,fbx_5} -> ?ExpFileFormatFbx5Ascii;
	{true,fbx_6} -> ?ExpFileFormatFbx6Ascii
    end.
    
export_object(Name, Mesh0, Mat) ->
    Mesh = add_normals(Mesh0),
    cast(?ExpCreateMesh),
    create_node(Name),
    export_mesh(Mesh, Mat),
    cast(?ExpAssignMesh),
    export_shading(Mesh).

export_shading(#e3d_mesh{tx=[]}) ->
    cast(?ExpSetShadingMode, <<?EnumLightShading:32/native>>);
export_shading(_) ->
    cast(?ExpSetShadingMode, <<?EnumLightTextureShading:32/native>>).

export_mesh(#e3d_mesh{fs=Fs,vs=Vs0,ns=Ns,tx=Tx,vc=VtxCol}=Mesh, Mat0) ->
    Vs = fake_matrix(Vs0),
    Mat = used_materials(Mesh, Mat0),
    Textures = texture_materials(Mat),
    if
	Textures =:= [] -> ok;
	true -> cast(?ExpInitTextures)
    end,
    MatMap = make_mat_map(Mat, Textures),
    NumVs = length(Vs),
    cast(?ExpInitControlPoints, <<NumVs:32/native>>),
    export_table(Vs),
    cast(?ExpInitNormals),
    export_table(Ns),
    export_uvs(Tx),
    export_faces(Fs, MatMap),
    export_mat(Mat),
    export_tx(Textures),
    export_vc(Fs, VtxCol).

export_table([{X,Y,Z}|T]) ->
    cast(?ExpPoint, X, Y, Z),
    export_table(T);
export_table([]) -> ok.

export_uvs([]) -> ok;
export_uvs(UVs) ->
    N = length(UVs),
    cast(?ExpInitUVs, <<N:32/native>>),
    export_uvs_1(UVs).

export_uvs_1([{U,V}|T]) ->
    cast(?ExpUV, U, V),
    export_uvs_1(T);
export_uvs_1([]) -> ok.

export_faces([#e3d_face{vs=Vs,tx=Tx,mat=[Mat|_]}|T], MatMap) ->
    {MatIndex,TxIndex} = gb_trees:get(Mat, MatMap),
    cast(?ExpBeginPolygon, <<MatIndex:32/native,TxIndex:32/native>>),
    export_face(Vs, Tx),
    cast(?ExpEndPolygon),
    export_faces(T, MatMap);
export_faces([], _) -> ok.

export_face([V|Vs], []) ->
    cast(?ExpAddPolygon, <<V:32/native,-1:32/native>>),
    export_face(Vs, []);
export_face([V|Vs], [T|Ts]) ->
    cast(?ExpAddPolygon, <<V:32/native,T:32/native>>),
    export_face(Vs, Ts);
export_face([], []) -> ok.

export_vc(_Fs, []) -> ok;
export_vc(Fs, Vc) ->
    cast(?ExpInitVertexColorTable),
    export_color_table(Vc),
    export_color_index(Fs),
    ok.

export_color_table([RGB|Vc]) ->
    cast(?ExpVertexColor, RGB),
    export_color_table(Vc);
export_color_table([]) -> ok.

export_color_index([#e3d_face{vc=Vc}|Fs]) ->
    foreach(fun(I) ->
		    cast(?ExpVertexColorIndex, <<I:32/native-unsigned-integer>>)
	    end, Vc),
    export_color_index(Fs);
export_color_index([]) -> ok.
    
add_normals(Mesh0) ->
    Mesh = e3d_mesh:vertex_normals(Mesh0),
    case vs_ns_same(Mesh) of
        true -> Mesh;
        false -> add_normals_1(Mesh)
    end.

add_normals_1(#e3d_mesh{fs=Fs0,vs=Vtab0,ns=Ntab0}=Mesh) ->
    Vtab1 = list_to_tuple(Vtab0),
    Ntab1 = list_to_tuple(Ntab0),
    {Fs,Vtab,Ntab} = add_normals_2(Fs0, {Vtab1,Ntab1}, [], [], [], 0),
    Mesh#e3d_mesh{fs=Fs,vs=Vtab,ns=Ntab}.

add_normals_2([#e3d_face{vs=Vs0,ns=Ns}=F0|Faces], {Vt,Nt}=Tabs,
              Facc, Vacc0, Nacc0, V0) ->
    {Vs,Vacc,Nacc,V} = add_normals_3(Vs0, Ns, Vt, Nt, [], Vacc0, Nacc0, V0),
    F = F0#e3d_face{vs=Vs},
    add_normals_2(Faces, Tabs, [F|Facc], Vacc, Nacc, V);
add_normals_2([], _, Facc, Vacc, Nacc, _) ->
    {reverse(Facc),reverse(Vacc),reverse(Nacc)}.

add_normals_3([Fv|Fs], [Nv|Ns], Vt, Nt, Facc, Vacc, Nacc, V) ->
    Vtx = element(Fv+1, Vt),
    N = element(Nv+1, Nt),
    add_normals_3(Fs, Ns, Vt, Nt, [V|Facc], [Vtx|Vacc], [N|Nacc], V+1);
add_normals_3([], [], _, _, Facc, Vacc, Nacc, V) ->
    {reverse(Facc),Vacc,Nacc,V}.

vs_ns_same(#e3d_mesh{fs=Fs}) ->            
    vs_ns_same_1(Fs).

vs_ns_same_1([#e3d_face{vs=Fs,ns=Fs}|T]) -> vs_ns_same_1(T);
vs_ns_same_1([_|_]) -> false;
vs_ns_same_1([]) -> true.

export_mat([{Name,Ps}|Ms]) ->
    cast(?ExpMaterial, [atom_to_list(Name),0]),
    OpenGL = proplists:get_value(opengl, Ps, []),
    export_opengl(OpenGL),
    export_mat(Ms);
export_mat([]) -> ok.

export_opengl([{ambient,{R,G,B,_}}|Ps]) ->
    cast(?ExpAmbient, R, G, B),
    export_opengl(Ps);
export_opengl([{diffuse,{R,G,B,_}}|Ps]) ->
    cast(?ExpDiffuse, R, G, B),
    export_opengl(Ps);
export_opengl([{specular,{R,G,B,_}}|Ps]) ->
    cast(?ExpSpecular, R, G, B),
    export_opengl(Ps);
export_opengl([{emission,{R,G,B,_}}|Ps]) ->
    cast(?ExpEmissive, R, G, B),
    export_opengl(Ps);
export_opengl([{opacity,Opac}|Ps]) ->
    cast(?ExpOpacity, <<Opac:64/native-float>>),
    export_opengl(Ps);
export_opengl([{shininess,Opac}|Ps]) ->
    cast(?ExpShininess, <<Opac:64/native-float>>),
    export_opengl(Ps);
export_opengl([P|Ps]) ->
    io:format("~p\n", [P]),
    export_opengl(Ps);
export_opengl([]) -> ok.

export_tx([{Name0,#e3d_image{name=none}=Im}|Txs]) ->
    Name = atom_to_list(Name0),
    export_tx([{Name,Im#e3d_image{name=Name}}|Txs]);
export_tx([{_,#e3d_image{name=Name,filename=Filename}}|Txs]) ->
    cast(?ExpCreateTexture, [Name,0]),
    cast(?ExpTextureFilename, [Filename,0]),
    export_tx(Txs);
export_tx([]) -> ok.

used_materials(Mesh, Mat0) ->
    Used0 = e3d_mesh:used_materials(Mesh),
    Used = sofs:from_external(Used0, [atom]),
    Mat1 = sofs:relation(Mat0),
    Mat = sofs:restriction(Mat1, Used),
    sofs:to_external(Mat).

make_mat_map(Mat, Txs) ->
    make_mat_map(Mat, Txs, 0, 0, []).

make_mat_map([{Name,_}|Ms], [{Name,_}|Txs], MI, TxI, Acc) ->
    make_mat_map(Ms, Txs, MI+1, TxI+1, [{Name,{MI,TxI}}|Acc]);
make_mat_map([{Name,_}|Ms], Txs, MI, TxI, Acc) ->
    make_mat_map(Ms, Txs, MI+1, TxI, [{Name,{MI,TxI}}|Acc]);
make_mat_map([], [], _, _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

texture_materials([{Name,Ps}|Ms]) ->
    Maps = proplists:get_value(maps, Ps, []),
    case proplists:get_value(diffuse, Maps, undefined) of
        undefined ->
            texture_materials(Ms);
        Image ->
            [{Name,Image}|texture_materials(Ms)]
    end;
texture_materials([]) -> [].

export_images([{Name,Ps0}|Ms], Root) ->
    Ps = export_images_1(Ps0, Root),
    [{Name,Ps}|export_images(Ms, Root)];
export_images([], _) -> [].

export_images_1([{maps,Maps0}|Ps], Root) ->
    Maps = export_images_2(Maps0, Root),
    [{maps,Maps}|Ps];
export_images_1([H|T], Root) ->
    [H|export_images_1(T, Root)];
export_images_1([], _) -> [].

export_images_2([{diffuse,#e3d_image{filename=none,name=Name}=Im}|Maps],
                Root) ->
    Filename = filename:join(Root, Name ++ ".tga"),
    ok = e3d_image:save(Im, Filename),
    [{diffuse,Im#e3d_image{filename=Filename}}|Maps];
export_images_2([{diffuse,_}|_]=Maps, _) ->
    Maps;
export_images_2([H|T], Root) ->
    [H|export_images_2(T, Root)];
export_images_2([], _) -> [].

fake_matrix(Vs0) ->
    {X,Y,Z} = Center = e3d_vec:average(e3d_vec:bounding_box(Vs0)),
    Vs = move_to_origin(Vs0, -X, -Y, -Z, []),
    set_default_t(Center),
    Vs.

move_to_origin([{X,Y,Z}|Vs], Mx, My, Mz, Acc) ->
    move_to_origin(Vs, Mx, My, Mz, [{X+Mx,Y+My,Z+Mz}|Acc]);
move_to_origin([], _, _, _, Acc) -> reverse(Acc).

%%%
%%% Exporting of lights.
%%%

export_lights([{Name,Ps}|T], ExpScale) ->
    export_light(Name, Ps, ExpScale),
    export_lights(T, ExpScale);
export_lights([], _) -> ok.

export_light(Name, Ps, ExpScale) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    case proplists:get_value(type, OpenGL) of
        ambient ->
            Amb = proplists:get_value(ambient, OpenGL, {0.0,0.0,0.0}),
            cast(?ExpAmbientColor, Amb);
        Type ->
            Pos0 = proplists:get_value(position, OpenGL),
            Pos = e3d_vec:mul(Pos0, ExpScale),
            Aim0 = proplists:get_value(aim_point, OpenGL, {0.0,0.0,0.0}),
            Aim = e3d_vec:mul(Aim0, ExpScale),
            cast(?ExpCreateLight),
            create_node(Name),
            cast(?ExpAssignLight),
            cast(?ExpLightType, <<(light_type(Type)):32/native>>),
            set_default_t(Pos),
            Diff = proplists:get_value(diffuse, OpenGL, {1.0,1.0,1.0}),
            cast(?ExpLightColor, Diff),
            export_light_1(Type, Pos, Aim, OpenGL)
    end.

export_light_1(infinite, Pos, Aim, _OpenGL) ->
    export_dir(Pos, Aim);
export_light_1(spot, Pos, Aim, OpenGL) ->
    export_dir(Pos, Aim),
    ConeAngle = 2*proplists:get_value(cone_angle, OpenGL, 45.0),
    cast(?ExpLightConeAngle, <<ConeAngle:64/native-float>>);
export_light_1(_, _, _, _) -> ok.

light_type(infinite) -> ?LightDirectional;
light_type(point) -> ?LightPoint;
light_type(spot) -> ?LightSpot.

export_dir({X,Y,Z}=Pos, Aim) ->
    {Xr,Yr,Zr} = axis_alignment_in_euler_angle(Pos, {X,Y-1.0,Z}, Aim),
    cast(?ExpSetDefaultR, Xr, Yr, Zr).

axis_alignment_in_euler_angle(PAB, PA, PB) ->
    call(?ExpAxisAlignmentInEulerAngle,
        [make_vec(PAB),make_vec(PA),make_vec(PB)]).

make_vec({X,Y,Z}) ->
    <<X:64/native-float,Y:64/native-float,Z:64/native-float>>.

%%%
%%% Common useful operations.
%%%

set_default_t(Pos) ->
    cast(?ExpSetDefaultT, Pos).

create_node(Name) ->
    cast(?ExpCreateNode, [Name,0]).

%%%
%%% Import
%%%

do_import(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"FBX Import Options"), dialog(import),
	       fun(Res) ->
		       {file,{import,{fbx,Res}}}
	       end);
do_import({Filename,Attr}, St) ->
    do_import_1(Filename, Attr, St);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import_filename(props(),
                        fun(Name) ->
                                do_import_1(Name, Attr, St)
                        end).

do_import_1(aborted, _, St) -> St;
do_import_1({error,Message}, _, _) -> wpa:error(Message);
do_import_1(Filename, Attr, St) ->
    cast(?ExpInitialize),
    Res = (catch do_import_2(Filename, Attr, St)),
    cast(?ExpDestroy),
    case Res of
        {'EXIT',Reason} -> exit(Reason);
        {error,Error} -> wpa:error(Error);
        {ok,Result} -> Result;
        keep -> keep
    end.

do_import_2(Filename, Attr, St) ->
    Password = proplists:get_value(password, Attr, []),
    case call(?ImpLoadScene, [Filename,0,Password,0]) of
        ok ->
            io:format(?__(1,"FBX version:")++" ~s\n", [call(?ImpVersion)]),
            do_import_3(Attr, St);
        password_protected ->
            ask_for_password(Filename, Attr);
        {error,_}=Error ->
            throw(Error)
    end.

do_import_3(Attr, St0) ->
    %%print_tree(),
    ImpScale = import_scale(Attr),
    Amb = call(?ImpGlobalAmbient),
    Light = {"_fbx_ambient_",[{opengl,[{type,ambient},{ambient,Amb}]}]},
    GlobalTransform = e3d_mat:identity(),
    Lights = import_tree(fun(light, Gl, A) ->
                                 [import_light(Gl, ImpScale)|A];
                            (_, _, A) -> A
                         end, GlobalTransform, []),
    St = wpa:import_lights([Light|Lights], St0),
    fold_tree(fun(T, _) when T == patch;
                             T == nurb ->
                      cast(?ImpConvertToMesh);
                 (_, _) -> []
              end, []),
    %%Local = local_transformation(),
    Objs = reverse(import_tree(fun import_mesh/3, GlobalTransform, [])),
    File = import_transform(#e3d_file{objs=Objs}, Attr),
    {ok,wpa:import(File, St)}.

ask_for_password(Filename, Attr) ->
    wpa:dialog(?__(1,"Enter Password"),
               [{text,[],[password]}],
	       fun([Pass]) ->
		       {file,{import,{fbx,{Filename,[{password,Pass}|Attr]}}}}
	       end).

import_tree(Importer, GlobalTransform, Acc) ->
    N = call(?ImpNumChildren),
    import_tree_1(0, N, Importer, GlobalTransform, Acc).

import_tree_1(N, N, _, _, Acc) -> Acc;
import_tree_1(I, N, Importer, Global0, Acc0) ->
    enter_node(I),
    Local = local_transformation(),
    Global = e3d_mat:mul(Global0, Local),
    Acc1 = Importer(node_type(), Global, Acc0),
    Acc = import_tree(Importer, Global, Acc1),
    pop_node(),
    import_tree_1(I+1, N, Importer, Global0, Acc).

%% print_trans(Mtx) ->
%%     Tx = element(10, Mtx),
%%     Ty = element(11, Mtx),
%%     Tz = element(12, Mtx),
%%     io:format("~p", [{Tx,Ty,Tz}]).

print_tree() ->
    Print = fun(Indent) ->
                    Local = local_transformation(),
                    Tx = element(10, Local),
                    Ty = element(11, Local),
                    Tz = element(12, Local),
                    Trans = {Tx,Ty,Tz},
                    io:format("~s~p: ~s (~p)\n",
                              [Indent,node_type(),node_name(),Trans])
            end,
    print_tree(Print).


print_tree(Printer) ->
    print_tree(Printer, []).

print_tree(Printer, Indent) ->
    Printer(Indent),
    N = call(?ImpNumChildren),
    print_tree_1(0, N, Printer, [$.|Indent]).

print_tree_1(N, N, _, _) -> ok;
print_tree_1(I, N, Printer, Indent) ->
    enter_node(I),
    print_tree(Printer, Indent),
    pop_node(),
    print_tree_1(I+1, N, Printer, Indent).

%%%
%%% Import a single mesh/object.
%%%

import_mesh(mesh, GlobalTransform, Acc) ->
    cast(?ImpMesh),
    Name = node_name(),
    %%Links = call(?ImpNumLinks),
    %%Shapes = call(?ImpNumShapes),
    N = call(?ImpNumVertices),
    cast(?ImpVertices),
    Vs = import_points(N, []),
    %% Ignore normals for now.
%%     cast(?ImpNormals),
%%     Ns = import_points(N, []),
    Ns = [],
    NumFaces = call(?ImpNumPolygons),
    Fs = import_faces(NumFaces-1, []),
    Mesh0 = #e3d_mesh{type=polygon,vs=Vs,ns=Ns,fs=Fs},
    Mesh1 = import_uvs(Mesh0),
    {Mesh2,Mat0} = import_materials(Mesh1),
    Mesh3 = import_tx(Mesh2),
    Mesh4 = import_vc(Mesh3),
    Mesh5 = e3d_mesh:transform(Mesh4, GlobalTransform),
    Mesh6 = e3d_mesh:merge_vertices(Mesh5),
    {Mesh,Mat} = mesh_combine_mat_tx(Mesh6, Mat0),
    Attr = [{visible,call(?ImpIsVisible)}],
    [#e3d_object{name=Name,obj=Mesh,mat=Mat,attr=Attr}|Acc];
import_mesh(_, _, Acc) -> Acc.

%%%
%%% Import a light.
%%%

import_light(Global, ImpScale) ->
    cast(?ImpLight),
    Type = light_type(),
    {R,G,B} = call(?ImpLightColor),
    Color = {R,G,B,1.0},
    Pos = e3d_vec:mul(e3d_mat:mul_point(Global, {0.0,0.0,0.0}), ImpScale),
    Dir = e3d_mat:mul_vector(Global, {0.0,1.0,0.0}),
    Aim = e3d_vec:sub(Pos, Dir),
    ConeAngle = call(?ImpConeAngle) / 2.0,
    OpenGL = [{type,Type},{position,Pos},{aim_point,Aim},
              {diffuse,Color},{ambient,Color},
              {cone_angle,ConeAngle}],
    {node_name(),[{opengl,OpenGL}]}.

%%%
%%% Insert textures into materials, creating new materials if
%%% necessary.
%%%

mesh_combine_mat_tx(#e3d_mesh{fs=[#e3d_face{mat=[]}|_]}=Mesh, Mat) ->
    %% No textures - nothing to do.
    {Mesh,Mat};
mesh_combine_mat_tx(#e3d_mesh{fs=[#e3d_face{mat=[_]}|_]}=Mesh, Mat) ->
    %% No textures - nothing to do.
    {Mesh,Mat};
mesh_combine_mat_tx(#e3d_mesh{fs=Fs0}=Mesh, Mat0) ->
    Comb = mesh_combine_mat_tx_1(Fs0, []),
    Mat1 = gb_trees:from_orddict(sort(Mat0)),
    {Mat,MatMap} = mesh_combine_mat_tx_2(Comb, Mat1, []),
    Fs = [F#e3d_face{mat=[gb_trees:get(K, MatMap)]} ||
             #e3d_face{mat=K}=F <- Fs0],
    {Mesh#e3d_mesh{fs=Fs},Mat}.

mesh_combine_mat_tx_1([#e3d_face{mat=MatTx}|Fs], [MatTx|_]=Acc) ->
    mesh_combine_mat_tx_1(Fs, Acc);
mesh_combine_mat_tx_1([#e3d_face{mat=[_,_]=MatTx}|Fs], Acc) ->
    mesh_combine_mat_tx_1(Fs, [MatTx|Acc]);
mesh_combine_mat_tx_1([], Acc) -> ordsets:from_list(Acc);
mesh_combine_mat_tx_1([H|_]=E, A) ->
    io:format("~p\n", [H]),
    erlang:error(badarg, [E,A]).

mesh_combine_mat_tx_2([[Name0,Tx]=Key|T], Mat0, MapAcc0) ->
    case gb_trees:get(Name0, Mat0) of
        [{maps,_}|_] ->
            %% Name in use. Generate a unique material name.
            name_in_use;
        Ps ->
            Mat = gb_trees:update(Name0, Tx++Ps, Mat0),
            MapAcc = [{Key,Name0}|MapAcc0],
            mesh_combine_mat_tx_2(T, Mat, MapAcc)
    end;
mesh_combine_mat_tx_2([], Mat, MapAcc) ->
    {gb_trees:to_list(Mat),gb_trees:from_orddict(sort(MapAcc))}.

%%%
%%% Import materials.
%%%

import_materials(Mesh) ->
    case call(?ImpInitMaterials) of
	false ->
	    {Mesh,[]};
	true ->
	    case layer_elem_reference_mode() of
		index ->
		    %% Index mode does not make sense.
		    {Mesh,[]};
		RM ->
		    MM = layer_elem_mapping_mode(),
		    N = call(?ImpNumMaterials),
		    Mat = import_materials_1(0, N, []),
		    {import_mat_mapping(MM, Mesh, Mat),Mat}
	    end
    end.

import_materials_1(N, N, Acc) -> reverse(Acc);
import_materials_1(I, N, Acc) ->
    cast(?ImpMaterial, <<I:32/native>>),
    Name = list_to_atom(call(?ImpMaterialName)),
    Amb = call(?ImpAmbient),
    Diff = call(?ImpDiffuse),
    Spec = call(?ImpSpecular),
    Emission = call(?ImpEmissive),
    Shininess = call(?ImpShininess),
    Opacity = call(?ImpOpacity),
    OpenGL = [{ambient,Amb},{diffuse,Diff},{specular,Spec},
              {emission,Emission},{shininess,Shininess},
              {opacity,Opacity}],
    Ps = [{opengl,OpenGL}],
    import_materials_1(I+1, N, [{Name,Ps}|Acc]).

import_mat_mapping(MapMode, Mesh, Mat) ->
    MatNames = list_to_tuple([Name || {Name,_} <- Mat]),
    case MapMode of
        by_polygon -> import_mat_by_polygon(Mesh, MatNames);
        all_same -> import_mat_all_same(Mesh, MatNames);
	Other ->
	    io:format(?__(1,"Cannot handle material mapping mode:")++" ~p\n",
		      [Other]),
	    Mesh
    end.
    
import_mat_by_polygon(#e3d_mesh{fs=Fs0}=Mesh, MatNames) ->
    cast(?ImpMaterialIndices),
    Fs = import_mat_by_polygon_1(Fs0, MatNames, []),
    Mesh#e3d_mesh{fs=Fs}.

import_mat_by_polygon_1([F|Fs], MatNames, Acc) ->
    Index = call(?ImpMaterialIndex),
    Mat = element(Index+1, MatNames),
    import_mat_by_polygon_1(Fs, MatNames, [F#e3d_face{mat=[Mat]}|Acc]);
import_mat_by_polygon_1([], _, Acc) -> reverse(Acc).

import_mat_all_same(#e3d_mesh{fs=Fs0}=Mesh, MatNames) ->
    case MatNames of
        {} -> Mesh;
        _ ->
            %% The SDK documentation says that the last material
            %% will be used. We'll do the same.
            Mat = [element(size(MatNames), MatNames)],
            Fs = [F#e3d_face{mat=Mat} || F <- Fs0],
            Mesh#e3d_mesh{fs=Fs}
    end.

%%%
%%% Import UV coordinates.
%%%

import_uvs(Mesh) ->
    case call(?ImpInitUVs) of
	false -> Mesh;
	true ->
	    case layer_elem_reference_mode() of
		index ->
		    %% Index mode does not make sense.
		    {Mesh,[]};
		RM ->
		    MM = layer_elem_mapping_mode(),
		    import_uvs_1(MM, Mesh)
	    end
    end.

import_uvs_1(MM, #e3d_mesh{vs=Vtab}=Mesh0) ->
    Tx = import_uv_tab(),
    Mesh = Mesh0#e3d_mesh{tx=Tx},
    case MM of
        by_polygon_vertex ->
            import_vertex_uvs(Mesh);
        by_control_point ->
            import_ctrl_point_uvs(Mesh);
        all_same when length(Tx) =:= length(Vtab) ->
            import_ctrl_point_uvs(Mesh);
        Other ->
            io:format(?__(1,"Can't handle mapping type:")++" ~p\n", [Other]),
            Mesh
    end.

import_vertex_uvs(#e3d_mesh{fs=Fs0}=Mesh) ->
    Fs = import_vertex_uvs(Fs0, 0, []),
    Mesh#e3d_mesh{fs=Fs}.

import_vertex_uvs([#e3d_face{}=F|Fs], Face, Acc) ->
    NumVs = call(?ImpPolygonSize, <<Face:32/native>>),
    UVs = import_face_uvs(Face, NumVs-1, []),
    import_vertex_uvs(Fs, Face+1, [F#e3d_face{tx=UVs}|Acc]);
import_vertex_uvs([], _, Acc) -> reverse(Acc).

import_face_uvs(_, J, Acc) when J < 0 -> Acc;
import_face_uvs(Face, J, Acc) ->
    V = call(?ImpPolygonUV, <<Face:32/native,J:32/native>>),
    import_face_uvs(Face, J-1, [V|Acc]).

import_ctrl_point_uvs(#e3d_mesh{fs=Fs0}=Mesh) ->
    Fs1 = foldl(fun(#e3d_face{vs=Vs}=F, A) ->
                        [F#e3d_face{tx=Vs}|A]
                end, [], Fs0),
    Fs = reverse(Fs1),
    Mesh#e3d_mesh{fs=Fs}.

import_uv_tab() ->
    N = call(?ImpNumUVs),
    cast(?ImpUVs),
    import_uvs(N, []).
    
import_uvs(0, Acc) -> reverse(Acc);
import_uvs(N, Acc) -> import_uvs(N-1, [call(?ImpUV)|Acc]).

%%%
%%% Import textures.
%%%

import_tx(Mesh) ->
    case call(?ImpInitTextures) of
	false -> Mesh;
	true ->
	    case layer_elem_reference_mode() of
		index ->
		    %% Index mode does not make sense.
		    {Mesh,[]};
		RM ->
		    MM = layer_elem_mapping_mode(),
		    N = call(?ImpNumTextures),
		    import_tx_1(MM, N, Mesh)
	    end
    end.

%%     case call(?ImpNumTextures) of
%%         0 ->
%%             Mesh;
%%         N ->
%%             case tx_mapping_type() of
%%                 uv -> import_tx_1(N, Mesh);
%%                 _ -> Mesh
%%             end
%%     end.

import_tx_1(all_same, N, Mesh) ->
    import_tx_all_same(N, Mesh);
import_tx_1(by_polygon, N, Mesh) ->
    import_tx_by_polygon(N, Mesh);
import_tx_1(_Other, _, Mesh) ->
    io:format("~p\n", [{?LINE,_Other}]),
    Mesh.

import_tx_all_same(N, #e3d_mesh{fs=Fs0}=Mesh) ->
    Tx = import_one_tx(N-1),
    Fs = [F#e3d_face{mat=[Mat,Tx]} || #e3d_face{mat=[Mat]}=F <- Fs0],
    Mesh#e3d_mesh{fs=Fs}.

import_tx_by_polygon(N, #e3d_mesh{fs=Fs0}=Mesh) ->
    Tx = list_to_tuple(import_all_tx(N-1, [])),
    cast(?ImpTextureIndices),
    Fs = import_tx_by_polygon_1(Fs0, Tx, []),
    Mesh#e3d_mesh{fs=Fs}.

import_tx_by_polygon_1([#e3d_face{mat=[Mat]}=F|Fs], TxTab, Acc) ->
    Index = call(?ImpTextureIndex),
    Tx = element(Index+1, TxTab),
    import_tx_by_polygon_1(Fs, TxTab, [F#e3d_face{mat=[Mat,Tx]}|Acc]);
import_tx_by_polygon_1([], _, Acc) -> reverse(Acc).

import_all_tx(I, Acc) when I >= 0 ->
    import_all_tx(I-1, [import_one_tx(I)|Acc]);
import_all_tx(_, Acc) -> Acc.
    
import_one_tx(I) ->
    cast(?ImpTexture, <<I:32/native>>),
    Filename = call(?ImpTextureFileName),
    [{maps,[{diffuse,Filename}]}].

%%%
%%% Face import.
%%%

import_points(0, Acc) -> reverse(Acc);
import_points(N, Acc) ->
    import_points(N-1, [call(?ImpPoint)|Acc]).

import_faces(Face, Acc) when Face < 0 -> Acc;
import_faces(Face, Acc) ->
    NumVs = call(?ImpPolygonSize, <<Face:32/native>>),
    Vs = import_face(Face, NumVs-1, []),
    import_faces(Face-1, [Vs|Acc]).

import_face(_, J, Acc) when J < 0 ->
    #e3d_face{vs=Acc,ns=Acc};
import_face(Face, J, Acc) ->
    V = call(?ImpPolygonVertex, <<Face:32/native,J:32/native>>),
    import_face(Face, J-1, [V|Acc]).

local_transformation() ->
    T = call(?ImpDefaultT),
    {Rx,Ry,Rz} = call(?ImpDefaultR),
    S = call(?ImpDefaultS),
    Rxq = e3d_q:from_angle_axis(Rx, {1.0,0.0,0.0}),
    Ryq = e3d_q:from_angle_axis(Ry, {0.0,1.0,0.0}),
    Rzq = e3d_q:from_angle_axis(Rz, {0.0,0.0,1.0}),
    Q = e3d_q:mul([Rzq,Ryq,Rxq]),
    RotMat = e3d_q:to_rotation_matrix(Q),
    Matrix0 = e3d_mat:translate(T),
    Matrix = e3d_mat:mul(Matrix0, RotMat),
    e3d_mat:mul(Matrix, e3d_mat:scale(S)).

%% Import vertex colors.
import_vc(Mesh) ->
    case call(?ImpInitVertexColors) of
	false -> Mesh;
	true ->
	    MM = layer_elem_mapping_mode(),
	    RM = layer_elem_reference_mode(),
	    import_vc_1(Mesh, MM, RM)
    end.

import_vc_1(Mesh, MM, RM) ->
    io:format(?__(1,"NYI: vertex color import:")++"\n  " ++
	      ?__(2,"mapping mode")++" = ~p, "++?__(3,"ref mode")++" = ~p\n",
	      [MM,RM]),
    Mesh.

%%%
%%% Import utilities.
%%%

fold_tree(F, Acc) ->
    N = call(?ImpNumChildren),
    fold_tree_1(0, N, F, Acc).

fold_tree_1(N, N, _, Acc) -> Acc;
fold_tree_1(I, N, F, Acc0) ->
    enter_node(I),
    Acc1 = F(node_type(), Acc0),
    Acc = fold_tree(F, Acc1),
    pop_node(),
    fold_tree_1(I+1, N, F, Acc).
    
%% num_children() ->
%%     call(?ImpNumChildren).

enter_node(N) ->
    cast(?ImpEnterChildNode, <<N:32/native>>).

pop_node() ->
    cast(?ImpPopNode).

node_type() ->
    case call(?ImpNodeType) of
        ?NodeEmpty -> empty;
        ?NodeNull -> null;
        ?NodeMarker -> marker;
        ?NodeSkeleton -> skeleton;
        ?NodeMesh -> mesh;
        ?NodeNurb -> nurb;
        ?NodePatch -> patch;
        ?NodeCamera -> camera;
        ?NodeCameraSwitcher -> camera_switcher;
        ?NodeLight -> light;
	?NodeOpticalReference -> optical_reference;
	?NodeOpticalMarker -> optical_marker;
	?NodeUnknown -> uknown
    end.

node_name() ->
    call(?ImpNodeName).

tx_mapping_type() ->
    case call(?ImpTxMappingType) of
        ?TxMapNull -> null;
        ?TxMapPlanar -> planar;
        ?TxMapSpherical -> spherical;
        ?TxMapCylindrical -> cylindrical;
        ?TxMapBox -> box;
        ?TxMapFace -> face;
        ?TxMapUV -> uv;
        ?TxMapEnvironment -> environment
    end.

light_type() ->
    case call(?ImpLightType) of
        ?LightPoint -> point;
        ?LightDirectional -> infinite;
        ?LightSpot -> spot
    end.

layer_elem_mapping_mode() ->
    call(?ImpMappingMode).

layer_elem_reference_mode() ->
    call(?ImpReferenceMode).
             
%%%
%%% Utilities.
%%%

cast(Cmd) ->
    cast(Cmd, []).

cast(Cmd, U, V) when is_float(U), is_float(V) ->
    cast(Cmd, <<U:64/native-float,V:64/native-float>>).

cast(Cmd, X, Y, Z) when is_float(X), is_float(Y), is_float(Z) ->
    cast(Cmd, <<X:64/native-float,Y:64/native-float,Z:64/native-float>>).

cast(Cmd, {X,Y,Z}) ->
    cast(Cmd, X, Y, Z);
cast(Cmd, {X,Y,Z,_}) ->
    cast(Cmd, X, Y, Z);
cast(Cmd, false) ->
    cast(Cmd, [0]);
cast(Cmd, true) ->
    cast(Cmd, [1]);
cast(Cmd, Data) ->
    %%io:format("~p\n", [Cmd]),
    erlang:port_control(wings_fbx_port, Cmd, Data).

call(Cmd) ->
    call(Cmd, []).

call(Cmd, Data) ->
    %%io:format("~p\n", [Cmd]),
    case cast(Cmd, Data) of
        [] -> ok;
        <<?RespError:8,Message/binary>> ->
            {error,binary_to_list(Message)};
        <<?RespBoolean:8,B:8>> -> B =/= 0;
        <<?RespFloat:8,T/binary>> -> get_float(T);
        <<?RespInteger:8,T/binary>> -> get_integer(T);
        <<?RespString:8,T/binary>> -> binary_to_list(T);
        <<?RespMappingMode:8,M:32/native>> -> get_mapping_mode(M);
        <<?RespReferenceMode:8,M:32/native>> -> get_reference_mode(M);
        <<?RespPasswordProtected:8>> -> password_protected
    end.

get_mapping_mode(?MappingByControlPoint) -> by_control_point;
get_mapping_mode(?MappingByPolygonVertex) -> by_polygon_vertex;
get_mapping_mode(?MappingByPolygon) -> by_polygon;
get_mapping_mode(?MappingAllSame) -> all_same.

get_reference_mode(?RefModeDirect) -> direct;
get_reference_mode(?RefModeIndex) -> index;
get_reference_mode(?RefIndexToDirect) -> index_to_direct.

get_float(<<F:64/native-float>>) -> F;
get_float(<<F:64/native-float,T/binary>>) ->
    get_float_1(T, [F]).
    
get_float_1(<<F:64/native-float>>, Acc) ->
    list_to_tuple(reverse(Acc, [F]));
get_float_1(<<F:64/native-float,T/binary>>, Acc) ->
    get_float_1(T, [F|Acc]).

get_integer(B) ->
    N = size(B),
    <<I:N/native-signed-unit:8>> = B,
    I.
    
dialog(import) ->
    [{label_column,
      [{?__(1,"Import scale"),{text,import_scale([]),[{key,import_scale}]}},
       {"("++?__(2,"Export scale")++")",{text,export_scale([]),[{key,export_scale}]}}]}
    ];
dialog(export) ->
    [{label_column,
      [{"("++?__(3,"Import scale")++")",{text,import_scale([]),[{key,import_scale}]}},
       {?__(4,"Export scale"),{text,export_scale([]),[{key,export_scale}]}},
       {?__(5,"Password"),{text,[],[{key,password},password]}}
      ]},
     {vframe,
      [{hradio,[{"FBX 5",fbx_5},
		{"FBX 6",fbx_6}],
	get_pref(fbx_format, fbx_6), [{key,fbx_format}]},
       {"ASCII FBX",get_pref(ascii_format, false),[{key,ascii_format}]}
      ],[{title,?__(6,"Format")}]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(export_scale(Attr)),
    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(import_scale(Attr)),
    e3d_file:transform(Contents, Mat).

import_scale(Attr) ->
    proplists:get_value(import_scale, Attr, 0.1).

export_scale(Attr) ->
    proplists:get_value(export_scale, Attr, 10.0).
