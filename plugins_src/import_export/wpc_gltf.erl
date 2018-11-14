%%
%%  wpc_collada.erl --
%%
%%     Collada export.
%%
%%  Copyright (c) 2017 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_gltf).
-export([init/0,menu/2,command/2]).

-define(DEF_IMAGE_TYPE, ".png").

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wx/include/gl.hrl").

-define(F32L, 32/float-little).

init() ->
    %% Collada specifies an "up_axis" parameter, so your
    %% model should usually be the "right way up"
    wpa:pref_set_default(?MODULE, swap_y_z, false),
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu, true);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu, true);
menu({file,import}, Menu) ->
    menu_entry(Menu, false);
menu(_, Menu) -> Menu.

menu_entry(Menu, true) ->
    Menu ++ [{"glTF (.gltf|.glb)...", glb,[option]}];
menu_entry(Menu, false) ->
    Menu ++ [{"glTF (.gltf|.glb)", glb}].

command({file, {import, glb}}, St) ->
    Props = [{extensions, [{".gltf", "gl Transmission Format"},
                           {".glb",  "gltf binary"}]}],
    wpa:import(Props, fun do_import/1, St);
command({file,{export,{glb,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{glb,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"gl Transmission Format Export Options"),
               dialog(export),
	       fun(Res) ->
		       {file,{Op,{glb,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Uvs = proplists:get_bool(include_uvs, Attr),
    %% Units = proplists:get_value(units, Attr),
    Ps = [{include_uvs,Uvs},%% {units,Units},
          {tesselation, triangulate},
          {include_hard_edges, true},
	  {subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)),
    keep.

dialog(Type) ->
    [{hframe,
      [
       {label, ?__(200,"File type") },
       {menu, [
               {"gl Transmission Format (*.gltf)", gltf},
               {"glTF binary (*.glb)", glb}
              ], glb, [{key, file_type}]}
      ]},
     %% wpa:dialog_template(?MODULE, units), panel,
     wpa:dialog_template(?MODULE, Type, [include_colors, include_normals])].

props() ->
    [{extensions,
      [{".glb",  "gltf binary"},
       {".gltf", "gl Transmission Format"}]}].

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Dir = filename:dirname(Filename),
    FileType = proplists:get_value(file_type, Attr, glb),
    ImageDir = case FileType of
                   glb -> filename:basedir(user_cache, "wings3d");
                   gltf -> Dir
               end,
    ok = filelib:ensure_dir(filename:join(ImageDir, "dummy")),
    Imagetype = proplists:get_value(default_filetype, Attr, ?DEF_IMAGE_TYPE),
    Contents1 = wpa:save_images(Contents0, ImageDir, Imagetype),
    #e3d_file{objs=Objs, mat=Mat, creator=Creator} = Contents1,
    GLTF0 = #{asset => #{generator => unicode:characters_to_binary(Creator),
                         version=> <<"2.0">>},
              accessors   => [],
              bufferViews => [],
              buffers => [],
              images => [],
              materials => [],
              meshes => [],
              nodes => [],
              scene => 0,
              scenes => [],
              textures => []
             },
    {Ns,GLTF1} = lists:foldl(fun exp_object/2, {[], GLTF0}, Objs),
    GLTF2 = exp_make_materials(Mat, FileType, GLTF1),
    GLTF3 = GLTF2#{
              accessors := lists:reverse(maps:get(accessors, GLTF2)),
              meshes    := lists:reverse(maps:get(meshes, GLTF2)),
              nodes     := lists:reverse(maps:get(nodes, GLTF2)),
              scenes    := [#{nodes=>lists:reverse(Ns)}],
              materials := lists:reverse(maps:get(materials, GLTF2)),
              images    := lists:reverse(maps:get(images, GLTF2)),
              textures  := lists:reverse(maps:get(textures, GLTF2))
             },
    GLTF4 = case maps:get(images, GLTF3) of
                [] -> maps:remove(textures, maps:remove(images,GLTF3));
                _  -> GLTF3
            end,

    {VtxData, GLTF5} = exp_setup_buffers(GLTF4),

    case FileType of
        glb -> %% Pack
            EncGLTF = jsone:encode(GLTF5, [{float_format, [compact]}]),
            GLTFSz  = byte_size(EncGLTF),
            GLTFAlignSz = (4-GLTFSz rem 4) rem 4,
            GLTFAlign = list_to_binary(lists:duplicate(GLTFAlignSz, $\s)),
            VtxDataSz = byte_size(VtxData),
            VtxAlignSz = (4-VtxDataSz rem 4) rem 4,
            VASzB = VtxAlignSz*8,
            TotSz = 12 + 8 + GLTFSz + GLTFAlignSz + 8 + VtxDataSz + VtxAlignSz,
            Bin = <<"glTF", 2:32/little, TotSz:32/little,
                    (GLTFSz+GLTFAlignSz):32/little, "JSON", EncGLTF/binary, GLTFAlign/binary,
                    (VtxDataSz+VtxAlignSz):32/little, "BIN", 0:8, VtxData/binary, 0:VASzB
                  >>,
            %% io:format("TotSz: ~p ~p ~p ~p ~p ~p~n", [28, GLTFSz, GLTFAlignSz, VtxDataSz, VtxAlignSz, TotSz]),
            %% io:format("      ~p~n",[byte_size(Bin)]),
            ok = file:write_file(filename:join(Dir, Filename), Bin);
        gltf ->
            FileBin = filename:rootname(filename:basename(Filename)) ++ ".bin",
            [Buff] = maps:get(buffers, GLTF5),
            GLTF = GLTF5#{buffers:=[Buff#{uri=>unicode:characters_to_binary(FileBin)}]},
            EncGLTF = jsone:encode(GLTF, [{indent, 2}, {space, 1},
                                          {float_format, [compact]}]),
            ok = file:write_file(Filename, unicode:characters_to_binary(EncGLTF)),
            ok = file:write_file(filename:join(Dir, FileBin), VtxData)
    end.

exp_object(#e3d_object{name=Name, obj=WMesh}, {Ns,GLTF0}) ->
    NameBin = unicode:characters_to_binary(Name),
    {Mesh,GLTF1} = exp_mesh(WMesh, NameBin, GLTF0),
    {MeshId, GLTF2} = exp_add(Mesh, meshes, GLTF1),
    Node = #{name => NameBin, mesh => MeshId},
    {Id, GLTF} = exp_add(Node, nodes, GLTF2),
    {[Id|Ns], GLTF}.

exp_mesh(WMesh0, Name, GLTF0) ->
    WMesh1 = e3d_mesh:vertex_normals(WMesh0),
    #e3d_mesh{vs=Vs,ns=Ns,tx=Tx,vc=Vc} = WMesh1,
    WMesh = WMesh1#e3d_mesh{vs=array:from_list(Vs),
                            ns=array:from_list(Ns),
                            tx=array:from_list(Tx, {0.0,0.0}),
                            vc=array:from_list(Vc)},
    FacesByMaterial = segment_by_material(WMesh),
    {Prims,GLTF} = exp_mesh_1(FacesByMaterial, WMesh, GLTF0, [], #{}),
    {#{name=>Name, primitives=>Prims},GLTF}.

exp_mesh_1([{[Mat|_], Fs}|MatFs], WMesh, GLTF0, Ps, S0) ->
    {MId, GLTF} = material_id(Mat, GLTF0),
    {Inds, S} = case {array:size(WMesh#e3d_mesh.tx),
		      array:size(WMesh#e3d_mesh.vc)} of
		    {0,0} -> exp_faces(Fs, WMesh, fun exp_face_n/3, [], S0);
		    {_,0} -> exp_faces(Fs, WMesh, fun exp_face_tx/3, [], S0);
		    {0,_} -> exp_faces(Fs, WMesh, fun exp_face_vc/3, [], S0);
		    {_,_} -> exp_faces(Fs, WMesh, fun exp_face_tx_vc/3, [], S0)
                end,
    P = #{material=> MId, indices=>length(Fs)*3},
    exp_mesh_1(MatFs, WMesh, GLTF, [{P, Inds}|Ps], S);
exp_mesh_1([], #e3d_mesh{tx=Tx, vc=Vc, vs=Vs}, GLTF0, Ps0, S) ->
    {Ps, GLTF1} = exp_add_index(lists:reverse(Ps0), GLTF0),
    MinMax = e3d_bv:box(array:to_list(Vs)),
    {Attr, GLTF} = exp_add_mesh_data(MinMax, array:size(Tx) =:= 0, array:size(Vc) =:= 0, S, GLTF1),
    {[P#{attributes=>Attr} || P <- Ps], GLTF}.

exp_add_index(Ps0, GLTF0) ->
    AppendBin = fun({P, Inds}, Bin) -> {{P, byte_size(Bin)}, <<Bin/binary, Inds/binary>>} end,
    {Ps1, Bin} = lists:mapfoldl(AppendBin, <<>>, Ps0),
    {BVId, GLTF1} = exp_add(Bin, bufferViews, GLTF0),
    lists:mapfoldr(fun({#{indices:=Len}=P, Offset}, GLTF_t) ->
                           Acces = exp_make_acc(BVId, Len, ?GL_UNSIGNED_INT,
                                                <<"SCALAR">>, Offset),
                           {AId, GLTF} = exp_add(Acces, accessors, GLTF_t),
                           {P#{indices:=AId}, GLTF}
                   end, GLTF1, Ps1).

exp_add_mesh_data({Min, Max}, NoTx, NoVc, S, GLTF0) ->
    VsData = lists:sort(maps:values(S)),
    N = maps:size(S),
    Bin = << <<Bin/binary>> || {_, Bin} <- VsData>>,
    Stride = case {NoTx,NoVc} of
		 {true,true} -> 6*4;
		 {false,true} -> 6*4+2*4;
		 {true,false} -> 6*4+4*4;
		 {false,false} -> 6*4+2*4+4*4
             end,

    {BVId, GLTF1} = exp_add(#{buffer=>Bin, byteStride=> Stride}, bufferViews, GLTF0),
    VsAcc = exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC3">>, 0),
    {VsA, GLTF2} = exp_add(VsAcc#{min=>tuple_to_list(Min), max=>tuple_to_list(Max)},
                           accessors, GLTF1),
    {NsA, GLTF3} = exp_add(exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC3">>, 3*4),
                           accessors, GLTF2),
    case {NoTx,NoVc} of
	{true,true} ->
            {#{'POSITION'=>VsA, 'NORMAL'=> NsA}, GLTF3};
	{false,true} ->
            {TxA, GLTF4} = exp_add(exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC2">>, 6*4),
                                   accessors, GLTF3),
            {#{'POSITION'=>VsA, 'NORMAL'=> NsA, 'TEXCOORD_0' => TxA}, GLTF4};
	{true,false} ->
	    {VcA, GLTF4} = exp_add(exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC4">>, 6*4),
				   accessors, GLTF3),
	    {#{'POSITION'=>VsA, 'NORMAL'=> NsA, 'COLOR_0' => VcA}, GLTF4};
	{false,false} ->
	    {TxA, GLTF4} = exp_add(exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC2">>, 6*4),
				   accessors, GLTF3),
	    {VcA, GLTF5} = exp_add(exp_make_acc(BVId, N, ?GL_FLOAT, <<"VEC4">>, 6*4+2*4),
				   accessors, GLTF4),
	    {#{'POSITION'=>VsA, 'NORMAL'=> NsA, 'TEXCOORD_0' => TxA, 'COLOR_0' => VcA}, GLTF5}
    end.

exp_faces([Face|Fs], WMesh, Fun, Inds, S0) ->
    {Is, S} = Fun(Face, WMesh, S0),
    exp_faces(Fs, WMesh, Fun, Is ++ Inds, S);
exp_faces([], _, _Fun, Inds, S) ->
    Bin = << <<I:32/little>> || I <- lists:reverse(Inds)>>,
    {Bin, S}.

exp_face_n(#e3d_face{vs=[V1,V2,V3], ns=[N1,N2,N3]}, #e3d_mesh{vs=Vs,ns=Ns}, S0) ->
    {F1,S1} = exp_data(V1,N1,Vs,Ns,S0),
    {F2,S2} = exp_data(V2,N2,Vs,Ns,S1),
    {F3,S3} = exp_data(V3,N3,Vs,Ns,S2),
    {[F3,F2,F1], S3}.

exp_face_tx(#e3d_face{vs=[V1,V2,V3], ns=[N1,N2,N3], tx=FTx},
            #e3d_mesh{vs=Vs,ns=Ns,tx=Tx}, S0) ->
    [T1,T2,T3] = fix_tx(FTx),
    {F1,S1} = exp_data(V1,N1,T1,Vs,Ns,Tx,none,S0),
    {F2,S2} = exp_data(V2,N2,T2,Vs,Ns,Tx,none,S1),
    {F3,S3} = exp_data(V3,N3,T3,Vs,Ns,Tx,none,S2),
    {[F3,F2,F1], S3}.

exp_face_vc(#e3d_face{vs=[V1,V2,V3], ns=[N1,N2,N3], vc=FVc},
	    #e3d_mesh{vs=Vs,ns=Ns,vc=Vc}, S0) ->
    [VC1,VC2,VC3] = fix_tx(FVc),
    {F1,S1} = exp_data(V1,N1,VC1,Vs,Ns,none,Vc,S0),
    {F2,S2} = exp_data(V2,N2,VC2,Vs,Ns,none,Vc,S1),
    {F3,S3} = exp_data(V3,N3,VC3,Vs,Ns,none,Vc,S2),
    {[F3,F2,F1], S3}.

exp_face_tx_vc(#e3d_face{vs=[V1,V2,V3], ns=[N1,N2,N3], tx=FTx, vc=FVc},
	    #e3d_mesh{vs=Vs,ns=Ns,tx=Tx,vc=Vc}, S0) ->
    [T1,T2,T3] = fix_tx(FTx),
    [VC1,VC2,VC3] = fix_tx(FVc),
    {F1,S1} = exp_data(V1,N1,{T1,VC1},Vs,Ns,Tx,Vc,S0),
    {F2,S2} = exp_data(V2,N2,{T2,VC2},Vs,Ns,Tx,Vc,S1),
    {F3,S3} = exp_data(V3,N3,{T3,VC3},Vs,Ns,Tx,Vc,S2),
    {[F3,F2,F1], S3}.


exp_data(V,N,Vs,Ns,S0) ->
    Key = {V,N},
    case maps:get(Key, S0, undefined) of
        {Id, _} -> {Id, S0};
        undefined ->
            Id = maps:size(S0),
            {X,Y,Z} = array:get(V, Vs),
            {NX,NY,NZ} = array:get(N, Ns),
            Bin = << X:?F32L, Y:?F32L, Z:?F32L,
                    NX:?F32L,NY:?F32L,NZ:?F32L>>,
            {Id, S0#{Key=>{Id, Bin}}}
    end.

exp_data(V,N,Uv,Vs,Ns,Tx,none,S0) ->
    Key = {V,N,Uv},
    case maps:get(Key, S0, undefined) of
        {Id, _} -> {Id, S0};
        undefined ->
            Id = maps:size(S0),
            {X,Y,Z} = array:get(V, Vs),
            {NX,NY,NZ} = array:get(N, Ns),
            {XU,XV} = array:get(Uv, Tx),
            Bin = << X:?F32L, Y:?F32L, Z:?F32L,
                     NX:?F32L,NY:?F32L,NZ:?F32L,
                     XU:?F32L,(1.0-XV):?F32L>>,
            {Id, S0#{Key=>{Id, Bin}}}
    end;
exp_data(V,N,Cl,Vs,Ns,none,Vc,S0) ->
    Key = {V,N,Cl},
    case maps:get(Key, S0, undefined) of
	{Id, _} -> {Id, S0};
	undefined ->
	    Id = maps:size(S0),
	    {X,Y,Z} = array:get(V, Vs),
	    {NX,NY,NZ} = array:get(N, Ns),
	    {R,G,B,A} =
	    	case array:get(Cl, Vc) of
		    {R0,G0,B0} -> {R0,G0,B0,1.0};
		    RGBA -> RGBA
		end,
	    Bin = << X:?F32L, Y:?F32L, Z:?F32L,
		     NX:?F32L,NY:?F32L,NZ:?F32L,
		     R:?F32L,G:?F32L,B:?F32L,A:?F32L>>,
	    {Id, S0#{Key=>{Id, Bin}}}
    end;
exp_data(V,N,{Uv,Cl},Vs,Ns,Tx,Vc,S0) ->
    Key = {V,N,Uv},
    case maps:get(Key, S0, undefined) of
	{Id, _} -> {Id, S0};
	undefined ->
	    Id = maps:size(S0),
	    {X,Y,Z} = array:get(V, Vs),
	    {NX,NY,NZ} = array:get(N, Ns),
	    {XU,XV} = array:get(Uv, Tx),
	    {R,G,B,A} =
	    case array:get(Cl, Vc) of
		{R0,G0,B0} -> {R0,G0,B0,1.0};
		RGBA -> RGBA
	    end,
	    Bin = << X:?F32L, Y:?F32L, Z:?F32L,
		     NX:?F32L,NY:?F32L,NZ:?F32L,
		     XU:?F32L,(1.0-XV):?F32L,
		     R:?F32L,G:?F32L,B:?F32L,A:?F32L>>,
	    {Id, S0#{Key=>{Id, Bin}}}
    end.


exp_setup_buffers(#{bufferViews:=BVs0} = GLTF0) ->
    {Bin, BVs} = exp_setup_buffer(lists:reverse(BVs0), <<>>, []),
    {0, GLTF} = exp_add(#{byteLength=>byte_size(Bin)}, buffers, GLTF0),
    {Bin, GLTF#{bufferViews:=BVs}}.

exp_setup_buffer([#{buffer:=B, byteStride:=Stride}|Bs], Bin, BVs)  ->
    BV = #{buffer => 0, byteLength=> byte_size(B),
           byteOffset => byte_size(Bin), byteStride => Stride},
    exp_setup_buffer(Bs, <<Bin/binary, B/binary>>, [BV|BVs]);
exp_setup_buffer([B|Bs], Bin, BVs) when is_binary(B) ->
    BV = #{buffer => 0, byteLength=> byte_size(B), byteOffset => byte_size(Bin)},
    exp_setup_buffer(Bs, <<Bin/binary, B/binary>>, [BV|BVs]);
exp_setup_buffer([], Bin, BVs) ->
    {Bin, lists:reverse(BVs)}.

exp_make_materials(WMat, Type, #{materials:=UsedMats} = GLTF) ->
    exp_make_materials(UsedMats, WMat, Type, GLTF#{materials:=[]}).

exp_make_materials([Used|UMs], WMats, Type, GLTF0) ->
    WMat = proplists:get_value(Used,WMats),
    GL   = proplists:get_value(opengl, WMat),
    {DR,DG,DB,DA} = proplists:get_value(diffuse, GL),
    Metallic = proplists:get_value(metallic, GL, 0.1),
    Roughness = proplists:get_value(roughness, GL, 0.8),
    {ER,EG,EB,_} = proplists:get_value(emission, GL),
    Maps = proplists:get_value(maps, WMat, []),
    %% [io:format("Map: ~p ~p~n", [T, I#e3d_image{image= <<>>}]) || {T,I} <- Maps],

    Base0 = #{baseColorFactor=> [DR,DG,DB,DA],
              metallicFactor => Metallic,
              roughnessFactor=> Roughness},
    DiffMap = proplists:get_value(diffuse, Maps),

    {Base1,GLTF1} = exp_add_image(DiffMap, Base0, baseColorTexture, Type, GLTF0),
    {Base, GLTF2} = exp_add_image(merge_met_roughness(Maps),
                                  Base1, metallicRoughnessTexture, Type, GLTF1),

    Mat0 = #{pbrMetallicRoughness => Base,
             emissiveFactor => [ER,EG,EB],
             name => atom_to_binary(Used, utf8),
             doubleSided => true
           },
    {Mat1, GLTF3} = exp_add_image(proplists:get_value(normal, Maps),
                                 Mat0, normalTexture, Type, GLTF2),
    {Mat2, GLTF4} = exp_add_image(proplists:get_value(emission, Maps),
                                  Mat1, emissiveTexture, Type, GLTF3),
    {Mat3, GLTF5} = exp_add_image(proplists:get_value(occlusion, Maps),
                                  Mat2, occlusionTexture, Type, GLTF4),

    Mat = case DiffMap of
              #e3d_image{bytes_pp = 4} ->
                  Mat3#{alphaMode=><<"BLEND">>, doubleSided:=false};
              _ when DA < 1.0 ->
                  Mat3#{alphaMode=><<"BLEND">>, doubleSided:=false};
              _ -> Mat3
          end,

    {_, GLTF} = exp_add(Mat, materials, GLTF5),
    exp_make_materials(UMs, WMats, Type, GLTF);
exp_make_materials([], _, _, GLTF) ->
    GLTF.

merge_met_roughness(Maps) ->
    {FN, E3d} =
        case {proplists:get_value(metallic, Maps),
              proplists:get_value(roughness, Maps)} of
            {undefined, undefined}=Undef -> Undef;
            {#e3d_image{filename=File}=Map, undefined} ->
                {File, e3d_image:expand_channel(b, e3d_image:convert(Map, g8, 1))};
            {undefined, #e3d_image{filename=File}=Map} ->
                {File, e3d_image:expand_channel(g, e3d_image:convert(Map, g8, 1))};
            {#e3d_image{width=W, height=H}=Met, #e3d_image{width=W, height=H}=Rough} ->
                RGBA0 = e3d_image:expand_channel(b, e3d_image:convert(Met, g8, 1)),
                RGBA1 = e3d_image:replace_channel(g, e3d_image:convert(Rough, g8, 1), RGBA0),
                {Met#e3d_image.filename, RGBA1};
            %% Different sizes, should rescale on off them here
            {#e3d_image{filename=File}=Map, _} ->
                {File, e3d_image:expand_channel(b, e3d_image:convert(Map, g8, 1))}
        end,
    case E3d of
        undefined -> undefined;
        _ ->
            Dir = filename:dirname(FN),
            Ext = filename:extension(FN),
            FileName = filename:basename(FN, Ext),
            SaveFile = filename:join(Dir, FileName ++ "_met_rough" ++ Ext),
            RGB = e3d_image:convert(E3d, r8g8b8),
            wings_image:image_write([{image, RGB}, {filename, SaveFile}]),
            RGB#e3d_image{filename=SaveFile}
    end.

exp_add_image(undefined, Data, _, _, GLTF) ->
    {Data, GLTF};
exp_add_image(#e3d_image{filename=File}, Data, Key, gltf, GLTF0) ->
    BinName = unicode:characters_to_binary(filename:basename(File)),
    {ImId, GLTF1} = exp_add(#{uri => BinName}, images, GLTF0),
    {TxId, GLTF2} = exp_add(#{source => ImId}, textures, GLTF1),
    {Data#{Key=>#{index=>TxId}}, GLTF2};
exp_add_image(#e3d_image{filename=File}, Data, Key, glb, GLTF0) ->
    {ok, ImageBin} = file:read_file(File),
    MType = case filename:extension(File) of
                ".png" -> <<"image/png">>;
                ".jpg" -> <<"image/jpeg">>
            end,
    {BvId, GLTF1} = exp_add(ImageBin, bufferViews, GLTF0),
    {ImId, GLTF2} = exp_add(#{bufferView => BvId, mimeType=>MType}, images, GLTF1),
    {TxId, GLTF3} = exp_add(#{source => ImId}, textures, GLTF2),
    {Data#{Key=>#{index=>TxId}}, GLTF3}.


segment_by_material(#e3d_mesh{fs=Fs}) ->
    FacesByMaterial0 = gb_trees:empty(),
    Sep = fun(#e3d_face{mat=Mats}=Face, Tree0) ->
                  Tree1 = case gb_trees:lookup(Mats, Tree0) of
                              {value,FaceList0} ->
                                  FaceList = [Face|FaceList0],
                                  gb_trees:update(Mats, FaceList, Tree0);
                              none ->
                                  gb_trees:insert(Mats, [Face], Tree0)
                          end,
                  Tree1
	  end,
    Segs = lists:foldl(Sep, FacesByMaterial0, Fs),
    gb_trees:to_list(Segs).

material_id(Mat, #{materials:= Mats} = GLTF) ->
    case material_id(Mat, Mats, 0) of
        {new, Id} ->
            {Id, GLTF#{materials:=Mats ++ [Mat]}};
        Id ->
            {Id, GLTF}
    end.

material_id(Mat, [Mat|_], Id) ->
    Id;
material_id(Mat, [_|Mats], Id) ->
    material_id(Mat, Mats, Id+1);
material_id(_, [], Id) ->
    {new, Id}.

fix_tx([_,_,_]=Tx) -> Tx;
fix_tx(_) -> [999999,999999,999999].

exp_make_acc(BvId, N, CT, Type,Offset) when is_integer(BvId) ->
    #{bufferView=>BvId,
      byteOffset=>Offset,
      componentType=>CT,
      count=> N,
      type=> Type}.

exp_add(New, What, GLTF) ->
    Orig = maps:get(What, GLTF),
    {length(Orig), GLTF#{What:=[New|Orig]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(is, {file="",
             dir ="",
             objs=[],
             matrix=[e3d_mat:identity()],
             buffers  %% Binary data
            }).

do_import(File) ->
    {ok, Bin} = file:read_file(File),
    FileSz = byte_size(Bin),
    case Bin of
        <<"glTF", _Ver:32/little, FileSz:32/little,Chunks/binary>> ->
            do_import(fetch_chunks(Chunks), File);
        <<"glTF", _/binary>> ->
            {error, "Data format error"};
        _ ->
            do_import([Bin], File)
    end.

do_import([JSON|Rest], File) ->
    Dir = filename:dirname(File),
    GLTF = jsone:decode(JSON, [{object_format, map}, {keys, atom}]),
    case maps:get(extensionsRequired, GLTF, []) of
        [] ->
            SceneId = maps:get(scene, GLTF, 0),
            #{nodes:=Nodes} = get_index(SceneId, scenes, GLTF),
            #{buffers:=BufferIds} = GLTF,
            GetBuffer = fun(Buff) -> imp_get_data(Buff, Dir, Rest) end,
            GetImage = fun(Buff, Type) -> imp_get_image(Buff, Type, File, Rest, GLTF) end,
            Buffers = [GetBuffer(BId) || BId <- BufferIds],
            {Mats, GLTF_1} = make_mats(GLTF#{read_image=>GetImage}, File),
            Objs = imp_objects(Nodes, GLTF_1, #is{buffers=Buffers, dir=Dir, file=filename:basename(File)}),
            {ok, #e3d_file{dir=Dir, objs=Objs, mat=Mats}};
        [_|_] ->
            {error, "Unsupported extension required"}
    end.

fetch_chunks(<<Len:32/little, "JSON", Rest/binary>>) ->
    PadSz = (4-Len rem 4) rem 4,
    <<Chunk:Len/binary, _:PadSz/binary, Chunks/binary>> = Rest,
    [Chunk | fetch_chunks(Chunks)];
fetch_chunks(<<Len:32/little, "BIN", 0:8, Rest/binary>>) ->
    PadSz = (4-Len rem 4) rem 4,
    <<Chunk:Len/binary, _:PadSz/binary, Chunks/binary>> = Rest,
    [Chunk | fetch_chunks(Chunks)];
fetch_chunks(<<Len:32/little, Type:32/little, Rest/binary>>) ->
    PadSz = (4-Len rem 4) rem 4,
    <<_Chunk:Len/binary, _:PadSz/binary, Chunks/binary>> = Rest,
    io:format("Skipped: ~p ~wbytes~n", [Type, Len]),
    fetch_chunks(Chunks);
fetch_chunks(_) ->
    [].

imp_objects([NodeId|Ns], GLTF, #is{matrix=MS, objs=Os}=IS) ->
    Node = get_index(NodeId, nodes, GLTF),
    Matrix = get_matrix(Node, hd(MS)),
    case Node of
        #{mesh:=MeshId} ->
            Mesh = get_index(MeshId, meshes, GLTF),
            Name0 = maps:get(name, Node, filename:rootname(IS#is.file)),
            Name = unicode:characters_to_list(maps:get(name, Mesh, Name0)),
            E3dMeshes = imp_mesh(Mesh, GLTF, IS),
            Objs = [#e3d_object{name=Name, obj=E3dMesh#e3d_mesh{matrix=Matrix}} ||
                       E3dMesh <- E3dMeshes],
            imp_objects(Ns, GLTF, IS#is{objs=Objs++Os});
        #{children:=Chs} ->
            Objs = imp_objects(Chs, GLTF, IS#is{matrix=[Matrix|MS]}),
            imp_objects(Ns, GLTF, IS#is{objs=Objs});
        #{} -> %% Cameras and animation
            imp_objects(Ns, GLTF, IS)
    end;
imp_objects([], _, #is{objs=Objs}) ->
    lists:reverse(Objs).

imp_mesh(#{primitives:=Ps}, GLTF, #is{buffers=Bs}) ->
    E3dMeshes = lists:foldl(fun(P,M) -> imp_faces(P, GLTF, Bs, M) end, [], Ps),
    [E3dMesh || {E3dMesh, _} <- E3dMeshes].

imp_faces(#{attributes:=As, indices:=_}=P, GLTF, Bs, MS0) ->
    Is = imp_mesh_data(indices, maps:get(indices, P, []), GLTF, Bs),
    Mat = case maps:get(material, P, undefined) of
              undefined -> default;
              MI ->
                  #{name:=Name} = get_index(MI, materials, GLTF),
                  binary_to_atom(Name, utf8)
          end,
    Mode = maps:get(mode, P, ?GL_TRIANGLES),
    VSA = maps:get('POSITION', As, none),
    NA  = maps:get('NORMAL', As, none),
    TXA = maps:get('TEXCOORD_0', As, none),
    VCA = maps:get('COLOR_0', As, none),
    Fs = fetch_is(Mode, Is, NA=/=none, TXA=/=none, VCA=/=none, [Mat]),
    case same_buffers({VSA,NA,TXA,VCA}, MS0) of
        {true, [{#e3d_mesh{fs=Fs0}=M,Buffs}|MS]} ->
            [{M#e3d_mesh{fs=Fs++Fs0},Buffs}|MS];
        {false, Buffs} ->
            Vs = imp_mesh_data(pos, VSA, GLTF, Bs),
            Ns = imp_mesh_data(normal, NA, GLTF, Bs),
            Tx = imp_mesh_data(tx, TXA, GLTF, Bs),
            Vc = imp_mesh_data(vc, VCA, GLTF, Bs),
            Fs = fetch_is(Mode, Is, NA=/=none, TXA=/=none, VCA=/=none, [Mat]),
            [{#e3d_mesh{type=triangle, vs=Vs, ns=Ns, tx=Tx, vc=Vc, fs=Fs},Buffs}|MS0]
    end.

same_buffers(Buffs, [{_,Buffs}|_]=MS) ->
    {true, MS};
same_buffers(Buffs, _) ->
    {false, Buffs}.

imp_mesh_data(_, none, _GLTF, _Buffers) ->
    [];
imp_mesh_data(What, AId, GLTF, Buffers) ->
    %% Fetch accessor
    #{bufferView:=BVId, componentType:=CType, type:=Type, count:=N} = A =
        get_index(AId, accessors, GLTF),
    Offset0 = maps:get(byteOffset, A, 0),

    %% Fetch buffer block
    #{buffer:=BId, byteLength:=_BuffL} = BV = get_index(BVId, bufferViews, GLTF),
    Offset1 = maps:get(byteOffset, BV, 0),
    Buffer = lists:nth(BId+1, Buffers),
    Offset = Offset0+Offset1,

    CSz = size(CType, Type),
    Stride = maps:get(byteStride, BV, CSz),
    BlockSz = CSz+(N-1)*Stride,

    %% io:format("What: ~p ~p BL:~p BSz:~p~n", [What, BVId, _BuffL, BlockSz]),
    %% io:format("OS ~p+~p=~p  ~p+~p < ~p~n",
    %%           [Offset0,Offset1,Offset, Offset,BlockSz,byte_size(Buffer)]),
    %% <<_:Offset1/binary,BuffBlock:BuffL/binary,_/binary>> = Buffer,
    %% <<_:Offset0/binary,Buff:BlockSz/binary, _/binary>> = BuffBlock,
    <<_:Offset/binary, Buff:BlockSz/binary, _/binary>> = Buffer,
    ModType = case {What, Type} of
                  {tx, <<"VEC2">>}        -> <<"VEC2_FLIP_Y">>;
                  {tx, <<"VEC2_FLIP_Y">>} -> <<"VEC2">>;
                  _ -> Type
              end,
    Data = imp_buff(Buff, CType, ModType, Stride-CSz),
    N = length(Data),
    Data.

%%%%%%%%%

imp_buff(Bin, ?GL_FLOAT, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1f(Bin, Skip, []));
imp_buff(Bin, ?GL_FLOAT, <<"VEC2">>, Skip) ->
    lists:reverse(imp_buff_2f(Bin, Skip, []));
imp_buff(Bin, ?GL_FLOAT, <<"VEC2_FLIP_Y">>, Skip) ->
    lists:reverse(imp_buff_2f_i(Bin, Skip, []));
imp_buff(Bin, ?GL_FLOAT, <<"VEC3">>, Skip) ->
    lists:reverse(imp_buff_3f(Bin, Skip, []));
imp_buff(Bin, ?GL_FLOAT, <<"VEC4">>, Skip) ->
    lists:reverse(imp_buff_4f(Bin, Skip, []));
imp_buff(Bin, ?GL_BYTE, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1b(Bin, Skip, []));
imp_buff(Bin, ?GL_UNSIGNED_BYTE, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1ub(Bin, Skip, []));
imp_buff(Bin, ?GL_SHORT, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1s(Bin, Skip, []));
imp_buff(Bin, ?GL_UNSIGNED_SHORT, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1us(Bin, Skip, []));
imp_buff(Bin, ?GL_UNSIGNED_INT, <<"SCALAR">>, Skip) ->
    lists:reverse(imp_buff_1ui(Bin, Skip, [])).

imp_buff_1f(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/float-little,_:Skip/binary, Rest/binary>> ->
            imp_buff_1f(Rest, Skip, [A|Acc]);
        <<A:32/float-little, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

imp_buff_2f(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/float-little,B:32/float-little,_:Skip/binary, Rest/binary>> ->
            imp_buff_2f(Rest, Skip, [{A,B}|Acc]);
        <<A:32/float-little,B:32/float-little, _/binary>> when Skip > 0 ->
            [{A,B}|Acc];
        <<>> -> Acc
    end.

imp_buff_2f_i(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/float-little,B:32/float-little,_:Skip/binary, Rest/binary>> ->
            imp_buff_2f_i(Rest, Skip, [{A,1.0-B}|Acc]);
        <<A:32/float-little,B:32/float-little, _/binary>> when Skip > 0 ->
            [{A,1.0-B}|Acc];
        <<>> -> Acc
    end.

imp_buff_3f(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/float-little,B:32/float-little,C:32/float-little,
          _:Skip/binary, Rest/binary>> ->
            imp_buff_3f(Rest, Skip, [{A,B,C}|Acc]);
        <<A:32/float-little,B:32/float-little,C:32/float-little,
          _/binary>> when Skip > 0 ->
            [{A,B,C}|Acc];
        <<>> -> Acc
    end.

imp_buff_4f(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/float-little,B:32/float-little,C:32/float-little,D:32/float-little,
          _:Skip/binary, Rest/binary>> ->
            imp_buff_4f(Rest, Skip, [{A,B,C,D}|Acc]);
        <<A:32/float-little,B:32/float-little,C:32/float-little,D:32/float-little,
          _/binary>> when Skip > 0 ->
            [{A,B,C,D}|Acc];
        <<>> -> Acc
    end.

imp_buff_1b(Bin, Skip, Acc) ->
    case Bin of
        <<A:8,_:Skip/binary, Rest/binary>> ->
            imp_buff_1b(Rest, Skip, [A|Acc]);
        <<A:8, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

imp_buff_1ub(Bin, Skip, Acc) ->
    case Bin of
        <<A:8/little-unsigned,_:Skip/binary, Rest/binary>> ->
            imp_buff_1ub(Rest, Skip, [A|Acc]);
        <<A:8/little-unsigned, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

imp_buff_1s(Bin, Skip, Acc) ->
    case Bin of
        <<A:16/little,_:Skip/binary, Rest/binary>> ->
            imp_buff_1s(Rest, Skip, [A|Acc]);
        <<A:16/little, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

imp_buff_1us(Bin, Skip, Acc) ->
    case Bin of
        <<A:16/little-unsigned,_:Skip/binary, Rest/binary>> ->
            imp_buff_1us(Rest, Skip, [A|Acc]);
        <<A:16/little-unsigned, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

imp_buff_1ui(Bin, Skip, Acc) ->
    case Bin of
        <<A:32/little-unsigned,_:Skip/binary, Rest/binary>> ->
            imp_buff_1ui(Rest, Skip, [A|Acc]);
        <<A:32/little-unsigned, _/binary>> when Skip > 0 ->
            [A|Acc];
        <<>> -> Acc
    end.

size(?GL_FLOAT, <<"SCALAR">>) -> 4;
size(?GL_FLOAT, <<"VEC2">>) -> 8;
size(?GL_FLOAT, <<"VEC2_FLIP_Y">>) -> 8;
size(?GL_FLOAT, <<"VEC3">>) -> 12;
size(?GL_FLOAT, <<"VEC4">>) -> 16;
size(?GL_BYTE, <<"SCALAR">>) -> 1;
size(?GL_UNSIGNED_BYTE, <<"SCALAR">>) -> 1;
size(?GL_SHORT, <<"SCALAR">>) -> 2;
size(?GL_UNSIGNED_SHORT, <<"SCALAR">>) -> 2;
size(?GL_UNSIGNED_INT, <<"SCALAR">>) -> 4.

fetch_is(Mode, Is, UseN, UseTx, UseVc, Mat) ->
    Inds = case Mode of
               ?GL_TRIANGLES -> fetch_tris(Is);
               ?GL_TRIANGLE_STRIP -> exit({nyi, strip});
               ?GL_TRIANGLE_FAN -> exit({nyi, fan})
           end,
    [#e3d_face{vs=Ind, ns=attr(UseN, Ind), tx=attr(UseTx, Ind), vc=attr(UseVc,Ind), mat=Mat}
     || Ind <- Inds].

fetch_tris([A,B,C|Is]) ->
    [[A,B,C]|fetch_tris(Is)];
fetch_tris([]) ->
    [].

attr(true, Ind) -> Ind;
attr(false, _Ind) -> [].

%% type(?GL_FLOAT) -> float;
%% type(?GL_BYTE) -> byte;
%% type(?GL_UNSIGNED_BYTE) -> ubyte;
%% type(?GL_SHORT) -> short;
%% type(?GL_UNSIGNED_SHORT) -> ushort;
%% type(?GL_UNSIGNED_INT) -> uint.

%%%%%%%%%

get_matrix(#{matrix:=List}, M0) ->
    Mat = list_to_tuple([float(I) || I <- List]),
    16 = tuple_size(Mat),
    e3d_mat:mul(M0, Mat);
get_matrix(Node, M0) ->
    T = make_matrix(translation, Node),
    R = make_matrix(rotation, Node),
    S = make_matrix(scale, Node),
    %% io:format("M=:~p~nT:~p~nR:~p~nS:~p~n",[M0,T,R,S]),
    e3d_mat:mul(M0, e3d_mat:mul(S, e3d_mat:mul(R, T))).

make_matrix(Key, Node) ->
    case maps:get(Key, Node, undefined) of
        undefined -> e3d_mat:identity();
        Vals0 ->
            [X,Y,Z|T] = [float(Val) || Val <- Vals0],
            case Key of
                translation -> e3d_mat:translate(X,Y,Z);
                scale       -> e3d_mat:scale(X,Y,Z);
                rotation when T =:= [0.0] -> e3d_mat:identity();
                rotation    -> e3d_q:to_rotation_matrix({{X,Y,Z},hd(T)})
            end
    end.

%%%%%%%%

imp_get_data(#{uri:=<<"data:application/octet-stream;base64,", Base64/binary>>}, _, _) ->
    base64:decode(Base64);
imp_get_data(#{uri:=Uri}, Dir, _) when byte_size(Uri) < 512 ->
    {ok, Buff} = file:read_file(filename:join(Dir, Uri)),
    Buff;
imp_get_data(#{byteLength:=BSz}, _, [Buffer]) ->
    <<Bin:BSz/binary, _Pad/binary>> = Buffer,
    Bin.

imp_get_image(#{uri:=<<"data:image/", Rest/binary>>}, ImageType, File0, _, _) ->
    {Ext, Bin} = case Rest of
                     <<"jpeg;base64,", Base64/binary>> -> {"jpeg", base64:decode(Base64)};
                     <<"png;base64,", Base64/binary>> -> {"png", base64:decode(Base64)}
                 end,
    DirFile = imp_save_filename(File0, ImageType, Ext),
    imp_save_image(DirFile, Bin);
imp_get_image(#{bufferView:=BVId, mimeType:=MType}=Image, ImageType, File0, [Buffer], GLTF) ->
    Ext = case MType of
              <<"image/png">> -> "png";
              <<"image/jpeg">> -> "jpeg"
          end,
    {Dir, File1} = imp_save_filename(File0, ImageType, Ext),
    File = maps:get(name, Image, File1),
    #{byteLength:=BSz} = BV = get_index(BVId, bufferViews, GLTF),
    Offset = maps:get(byteOffset, BV, 0),
    <<_:Offset/binary,Bin:BSz/binary,_/binary>> = Buffer,
    imp_save_image({Dir, unicode:characters_to_list(File)}, Bin).

imp_save_image({Dir, File}, Bin) ->
    %% Write temporary file, so we can load with wxImage:new(...)
    case file:write_file(filename:join(Dir, File), Bin) of
        ok -> File;
        {error, _} ->
            TmpDir = filename:basedir(user_cache, "wings3D"),
            TempFile = filename:join(TmpDir, File),
            _ = file:write_file(TempFile, Bin),
            TempFile
    end.

imp_save_filename(File0, ImageType, Ext) ->
    Dir = filename:dirname(File0),
    OrigName = filename:rootname(filename:basename(File0)),
    File = lists:flatten(io_lib:format("~ts_~ts.~s",[OrigName, ImageType, Ext])),
    {Dir, File}.

make_mats(#{materials:=Ms0}=GLTF, File) ->
    Dir = filename:dirname(File),
    DefName = filename:rootname(filename:basename(File)),
    Ms = mat_add_names(Ms0, 0, DefName),
    GLMs = [make_mat(Mat, GLTF, Dir) || Mat <- Ms],
    {GLMs, GLTF#{materials:=Ms}};
make_mats(GLTF, _) ->
    {[], GLTF}.

make_mat(#{name:=Name}=Mat, GLTF, Dir) ->
    case maps:get(pbrMetallicRoughness, Mat, none) of
        none ->
            Diffuse = [1.0,1.0,1.0,1.0],
            DiffuseTx = {diffuse, none},
            MetalTx = {metallic, none},
            RoughTx = {roughness, none},
            MetalF = 1.0,
            RoughF = 0.9;
        Pbr ->
            Diffuse = maps:get(baseColorFactor, Pbr, [1.0,1.0,1.0,1.0]),
            DiffuseTx = {diffuse, get_texture(baseColorTexture, Pbr, GLTF)},
            {Mtx,Rtx} = split_tx(get_texture(metallicRoughnessTexture, Pbr, GLTF), Dir),
            MetalTx = {metallic, Mtx},
            RoughTx = {roughness, Rtx},
            MetalF = maps:get(metallicFactor, Pbr, 1.0),
            RoughF = maps:get(roughnessFactor, Pbr, 0.9)
    end,

    NormalTx = {normal, get_texture(normalTexture, Mat, GLTF)},
    Emission = maps:get(emmissiveFactor, Mat, [0.0,0.0,0.0]),
    EmissionTx = {emission, get_texture(emissiveTexture, Mat, GLTF)},
    OccTx = {occlusion, get_texture(occlusionTexture, Mat, GLTF)},
    Txs = [NormalTx, DiffuseTx, MetalTx, RoughTx, EmissionTx, OccTx],
    Maps = case [Map || {_, F} = Map <- Txs, F =/= none] of
               [] -> [];
               Maps0 -> [{maps, Maps0}]
           end,
    DiffuseC = list_to_tuple(Diffuse),
    DefList = [{diffuse, DiffuseC},
               {roughness, RoughF},
               {metallic, MetalF},
               {emission, list_to_tuple(Emission)}],
    {binary_to_atom(Name, utf8), [{opengl, DefList}|Maps]}.

get_texture(Name, Mat, #{read_image:=Get} = GLTF) ->
    case maps:get(Name, Mat, none) of
        none -> none;
        #{index:=I} ->
            #{source:=SrcId} = get_index(I, textures, GLTF),
            case get_index(SrcId, images, GLTF) of
                #{bufferView:=_} = Ref ->
                    Get(Ref, Name);
                #{uri:=<<"data:image/", _/binary>>} = Ref ->
                    Get(Ref, Name);
                #{uri:=File} when byte_size(File) < 512 ->
                    unicode:characters_to_list(File)
            end
    end.

%% Need to split combined image file to two files
split_tx(none, _) -> {none, none};
split_tx(OrigFileName, Dir0) ->
    Dir = case filename:dirname(OrigFileName) of
              "." -> Dir0;
              Dir1 -> Dir1
          end,
    Ext = filename:extension(OrigFileName),
    File = filename:basename(OrigFileName, Ext),
    E3d = wings_image:image_read([{filename, OrigFileName}, {opt_dir, Dir0}]),
    try
        Metallic = e3d_image:channel(b,E3d),
        MetFile = filename:join(Dir, File++"_met" ++ Ext),
        wings_image:image_write([{filename, MetFile}, {image, Metallic}]),
        Roughness = e3d_image:channel(g,E3d),
        RoughFile = filename:join(Dir, File++"_rough" ++ Ext),
        wings_image:image_write([{filename, RoughFile}, {image, Roughness}]),
        {MetFile, RoughFile}
    catch _:Reason ->
            ?dbg("Internal error: ~p ~p~n", [Reason, erlang:get_stacktrace()]),
            {none, none}
    end.

mat_add_names([#{name:=_}=M|Ms], N, DefName) ->
    [M|mat_add_names(Ms, N, DefName)];
mat_add_names([M|Ms], N, DefName) ->
    Name = unicode:characters_to_binary(io_lib:format("~ts_~w", [DefName,N])),
    [M#{name=>Name}|mat_add_names(Ms, N+1, DefName)];
mat_add_names([], _, _) ->
    [].

%%%%%%%%%

get_index(I, Where, Map) ->
    #{Where:=List} = Map,
    lists:nth(1+I, List).
