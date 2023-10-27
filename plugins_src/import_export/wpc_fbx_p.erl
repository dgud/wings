%%
%%  wpc_fbx_p.erl --
%%
%%     FBX file import/export implemented entirely in erlang.
%%
%%  Copyright (c) 2023 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_fbx_p).

-export([init/0,menu/2,command/2]).

-import(lists, [map/2,foldl/3,keydelete/3,keyreplace/4,sort/1]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    wpa:pref_set_default(?MODULE, swap_y_z, false),
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{fbx_model,Ask}}}, St) ->
    fbx_p_import:do_import(Ask, St);
command({file,{export,{fbx_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{fbx_model,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{?__(1,"FBX (.fbx)..."),fbx_model,[option]}].

props() ->
    [{ext,".fbx"},{ext_desc,?__(1,"FBX Model File")}].


%%%
%%% Export.
%%%

%% FBX export options
-record(fopt, {
    compress = true
}).


do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    Dialog = [
        ] ++ common_mesh_options(export) ++ [
            panel,
            {?__(3, "Compressed"),true,[{key,compressed}]}
    ],
    wpa:dialog(Ask, ?__(1,"FBX Export Options"), Dialog,
           fun(Res) ->
               {file,{Op,{fbx_model,Res}}}
           end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Compressed = proplists:get_value(compressed, Attr, true),
    IncludeUVs = proplists:get_value(include_uvs, Attr, true),
    
    Ps = [{include_uvs,IncludeUVs},
          {include_hard_edges, true},
      {subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr, v74, Compressed)).

export_transform(E3dFile, KeyVals) ->
    Mat = wpa:export_matrix(KeyVals),
    e3d_file:transform(E3dFile, Mat).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_fun(Attr, _Version, Compressed) ->
    fun(Filename, Contents_0) ->
        set_pref(Attr),
        
        Dir = filename:dirname(Filename),
        Filetype = proplists:get_value(default_filetype, Attr, ".png"),
        Contents_1 = wpa:save_images(Contents_0, Dir, Filetype),
        
        #e3d_file{objs=ObjsW,mat=EMat}=export_transform(Contents_1, Attr),
        
        FbxOpt = #fopt{compress=Compressed},
        
        FilenameExt = filename:extension(Filename),
        _FilenamePrefix = filename:rootname(Filename, FilenameExt),
        Objs0 = [
                    object_for_fbx({}, Obj)
                || Obj <- ObjsW ],
        Mats0 = [ material_for_fbx(Mt, Dir) || Mt <- EMat],
        {Objs, IdAMap_0} = add_ident_numbers(Objs0),
        {Mats, IdAMap} = add_ident_numbers(Mats0, IdAMap_0),
        write_fbx_file(Filename, FbxOpt, Objs, Mats, IdAMap),
        ok
    end.



common_mesh_options(Type) ->
    [wpa:dialog_template(?MODULE, Type, [include_normals,include_colors])].


%%% The source for the exporter is roughly organized as:
%%%
%%% * Plugin interface code near the beginning of the source.
%%% * Prepare models and materials from e3d to an intermediate 
%%%   nested list and tuple structure.
%%% * Some functions that emit certain binary iolists for file structures.
%%%   (e.g. write_fbx, bytes_of_*)
%%% * Functions that turn intermediate nested tuple and list structures 
%%%   into a nested list binary closer to FBX's file structure.
%%% * Emit different FBX form sections
%%% * Emit the properties for each element for the main section.
%%% * Break out elements out of other elements for FBX's structure,
%%%   and create connection references between the elements.
%%% * Assemble the different sections into an iolist
%%% * Write the actual FBX iolist to file
%%% * add_ident_number which assigns numeric IDs to entities.
%%%


%%
%% Write FBX File
%%

%% Prepare models from e3d to an intermediate 
%% nested list and tuple structure.
object_for_fbx(_Options, #e3d_object{name=ObjName,obj=#e3d_mesh{}=Msh}=_) ->
    #e3d_mesh{ns=Norms,vs=Vertices,tx=Texs,fs=Fs,he=HEs} = e3d_mesh:vertex_normals(Msh),
    MatList_0 = object_for_fbx_matlist(Fs),
    MatList_1 = lists:usort([{B,A} || {A,B} <- MatList_0]),
    MatList = [
        {{mat, MtName}, mat}
    || {_MNum, MtName} <- MatList_1 ],
    
    %% Calculate the unique edges and if they are soft edges.
    VtxList = [Vs || #e3d_face{vs=Vs} <- Fs],
    Edges = uedge_list(VtxList, HEs),
    {model, ObjName, MatList ++ [
        {geom, [
            {vertices, Vertices},
            {vtxindex, VtxList},
            {ns, lists:append([from_idx_list(Ns, Norms) || #e3d_face{ns=Ns} <- Fs])},
            {uv, Texs},
            {uvidx, [Tx || #e3d_face{tx=Tx} <- Fs]},
            {mats, [orddict:fetch(Mt, MatList_0) || #e3d_face{mat=[Mt|_]} <- Fs]},
            {edges, Edges}
        ]}
    ]}.
object_for_fbx_matlist(Fs) ->
    lists:foldl(
        fun (#e3d_face{mat=[Mt|_]}, OL) ->
                case orddict:find(Mt, OL) of
                    error ->
                        orddict:store(Mt, orddict:size(OL), OL);
                    _ ->
                        OL
                end
        end, orddict:new(), Fs).


%% Prepare materials from e3d to an intermediate 
%% nested list and tuple structure.
material_for_fbx({MatName, Attr}, Dir)
  when is_atom(MatName) ->
    OA = proplists:get_value(opengl, Attr, none),
    OM = proplists:get_value(maps, Attr, none),
    TexL = material_for_fbx_maps(OM, Dir),
    
    {AmR,AmG,AmB,_} = proplists:get_value(ambient, OA, {0.8,0.8,0.8,1.0}),
    {SpR,SpG,SpB,SpA} = proplists:get_value(specular, OA, {0.8,0.8,0.8,1.0}),
    Shininess = proplists:get_value(shininess, OA, 0.5),
    {DfR,DfG,DfB,DfA} = proplists:get_value(diffuse, OA, {0.8,0.8,0.8,1.0}),
    {EmR,EmG,EmB,EmA} = proplists:get_value(emission, OA, {0.0,0.0,0.0,1.0}),
    Metallic = proplists:get_value(metallic, OA,0.1),
    _ = proplists:get_value(roughness, OA,0.8),
    
    Props = [
        {<<"Emissive">>,{vec3d,EmR,EmG,EmB}},
        {<<"EmissiveColor">>,{color,EmR,EmG,EmB}},
        {<<"EmissiveFactor">>,{number,EmA}},
        {<<"Ambient">>,{vec3d,AmR,AmG,AmB}},
        {<<"AmbientColor">>,{color,AmR,AmG,AmB}},
        {<<"Diffuse">>,{vec3d,DfR,DfG,DfB}},
        {<<"DiffuseColor">>,{color,DfR,DfG,DfB}},
        {<<"DiffuseFactor">>,{number,1.0}},
        {<<"TransparentColor">>,{color,1.0 - DfA,1.0 - DfA,1.0 - DfA}},
        {<<"TransparencyFactor">>,{number,1.0}},
        {<<"Opacity">>,{number,DfA}},
        {<<"Specular">>,{vec3d,SpR,SpG,SpB}},
        {<<"SpecularColor">>,{color,SpR,SpG,SpB}},
        {<<"SpecularFactor">>,{number,SpA}},
        {<<"Shininess">>,{number,Shininess}},
        {<<"ReflectionColor">>,{color,1.0,1.0,1.0}},
        {<<"ReflectionFactor">>,{number,Metallic}}
    ],
    
    {mat, MatName, TexL ++ [{prop, Props}]}.

material_for_fbx_maps(OM, Dir) ->
    material_for_fbx_maps(OM, Dir, []).
material_for_fbx_maps([{MapType,#e3d_image{name=MapName,filename=FileName_0}=_}|R], Dir, OL) ->
    WhichStr = case MapType of
        diffuse -> <<"DiffuseColor">>;
        normal -> <<"NormalMap">>;
        bump -> <<"Bump">>;
        metallic -> <<"ReflectionFactor">>;
        roughness -> <<"ShininessExponent">>;
        emission -> <<"EmissiveColor">>;
        _ -> <<"Unspecified">>
    end,
    
    FileName = relpath(FileName_0, Dir),
    
    FileNameOnly = filename:basename(FileName),
    
    FileNameB = str_to_utf8b(FileName),
    FileNameOnlyB = str_to_utf8b(FileNameOnly),
    
    Cont = [
        {{video, MapName},
            [ {filename_only, FileNameOnlyB},
              {relative_path, FileNameB},
              {prop, [{<<"Path">>, {kstring, FileNameB}}]}
            ]},
        {filename_only, FileNameOnlyB},
        {relative_path, FileNameB},
        {prop,[{<<"UseMaterial">>,{bool,1}}]}],
    B={{tex,{for, WhichStr, MapName}},Cont},
    material_for_fbx_maps(R, Dir, [B|OL]);
material_for_fbx_maps([], _Dir, OL) ->
    lists:reverse(OL).

relpath(A, B) ->
    relpath_1(filename:split(A), filename:split(B)).
relpath_1([A|B1],[A|B2]) ->
    relpath_1(B1,B2);
relpath_1(B1,[]) ->
    filename:join(B1);
relpath_1(B1,[_|R1]) ->
    relpath_1([".."|B1],R1).

from_idx_list(Ns, L_0) ->
    Arr = array:from_list(L_0),
    [array:get(Idx, Arr) || Idx <- Ns].

-define(FBX_VERSION, 7400).

%% FBX is in little byte order.
-define(SINT, little-signed-integer).
-define(UINT, little-unsigned-integer).

%% note:
%% If while making modifications, a runtime error happens in these functions,
%% (e.g. bytes_sint64), it is likely a value was placed in the wrong place 
%% earlier in the process. e.g. a string got placed into an attr_int tuple.
%%

bytes_sint64(B) ->
    <<B:64/?SINT>>.
bytes_sint32(B) ->
    <<B:32/?SINT>>.
bytes_sint8(B) ->
    <<B:8/?SINT>>.
bytes_uint32(B) ->
    <<B:32/?UINT>>.
bytes_uint8(B) ->
    <<B:8/?UINT>>.
bytes_float64(B) ->
    <<B:64/little-float>>.

%% Write whole iolist
%%
write_list(Blist, Fo) ->
    file:write(Fo, Blist).

bt_count(Blist) when is_list(Blist) ->
    lists:sum([bt_count(B) || B <- Blist]);
bt_count(Blist) when is_binary(Blist) ->
    byte_size(Blist).


%% Write the header of the FBX file
%% 
bytes_of_header() ->
    %% Kaydara FBX Binary, two spaces and a nul
    BytesMagic = <<
        16#4B,16#61,16#79,16#64,16#61,16#72,16#61,16#20,16#46,16#42,
        16#58,16#20,16#42,16#69,16#6E,16#61,16#72,16#79,16#20,16#20,16#00>>,
    
    %% DOS Ctrl+Z byte used after file signature strings.
    BytesDOSStop = <<16#1A,16#00>>,
    
    %% 4 Byte unsigned int specifying the version.
    BytesVersionNumber = bytes_uint32(?FBX_VERSION),
    [ BytesMagic, BytesDOSStop, BytesVersionNumber ].

bytes_of_node_name(Name) when is_list(Name) ->
    [
        bytes_uint8(string:length(Name)),
        iolist_to_binary(Name)
    ];
bytes_of_node_name(Name) when is_binary(Name) ->
    [
        bytes_uint8(byte_size(Name)),
        Name
    ].


%% Use deflate to compress some bytes
%%
deflate(Bin) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, best_compression, deflated, 15, 8, default),
    Bin_1 = zlib:deflate(Z, Bin),
    Bin_2 = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    iolist_to_binary([Bin_1|Bin_2]).


bytes_of_node_attribute(StrData) when is_binary(StrData) ->
    [<<"S">>, bytes_uint32(byte_size(StrData)), StrData];
bytes_of_node_attribute({attr_str,StrData}) when is_binary(StrData) ->
    [<<"S">>, bytes_uint32(byte_size(StrData)), StrData];
bytes_of_node_attribute({attr_str,StrData}) when is_list(StrData) ->
    StrDataB = str_to_utf8b(StrData),
    [<<"S">>, bytes_uint32(byte_size(StrDataB)), StrDataB];
bytes_of_node_attribute({attr_data,DatData}) when is_list(DatData) ->
    [<<"R">>, bytes_uint32(length(DatData)), iolist_to_binary(DatData)];
bytes_of_node_attribute({attr_data,DatData}) when is_binary(DatData) ->
    [<<"R">>, bytes_uint32(byte_size(DatData)), DatData];
bytes_of_node_attribute({attr_dflo,AttrVal}) ->
    [<<"D">>, bytes_float64(AttrVal)];
bytes_of_node_attribute({attr_int,AttrVal})
  when AttrVal <  1 bsl 32 andalso
       AttrVal > -1 bsl 32 ->
    [<<"I">>, bytes_sint32(AttrVal)];
bytes_of_node_attribute({attr_int,AttrVal})
  when is_integer(AttrVal) ->
    [<<"L">>, bytes_sint64(AttrVal)];
bytes_of_node_attribute({attr_int64,AttrVal}) ->
    [<<"L">>, bytes_sint64(AttrVal)];
bytes_of_node_attribute({attr_char,AttrVal}) ->
    [<<"C">>, bytes_sint8(AttrVal)];

%% Non-compressed list of numbers
bytes_of_node_attribute({attr_dflo_list, L})
  when is_list(L) ->
    Len = length(L),
    Enc = 0,
    Bin = iolist_to_binary([bytes_float64(A) || A <- L]),
    Cln = byte_size(Bin),
    [<<"d">>, <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT>>, Bin];
bytes_of_node_attribute({attr_int_list, L})
  when is_list(L) ->
    Len = length(L),
    Enc = 0,
    Bin = iolist_to_binary([bytes_sint32(A) || A <- L]),
    Cln = byte_size(Bin),
    [<<"i">>, <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT>>, Bin];

%% Compressed list of numbers
bytes_of_node_attribute({{deflate, attr_dflo_list}, L})
  when is_list(L) ->
    Len = length(L),
    Enc = 1,
    Bin = deflate(iolist_to_binary([bytes_float64(A) || A <- L])),
    Cln = byte_size(Bin),
    [<<"d">>, <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT>>, Bin];
bytes_of_node_attribute({{deflate, attr_int_list}, L})
  when is_list(L) ->
    Len = length(L),
    Enc = 1,
    Bin = deflate(iolist_to_binary([bytes_sint32(A) || A <- L])),
    Cln = byte_size(Bin),
    [<<"i">>, <<Len:32/?UINT, Enc:32/?UINT, Cln:32/?UINT>>, Bin];

%% list style node attribute
%% [nodeType, [properties ...] | nested-nodes]
bytes_of_node_attribute([AttrTp,StrData])
  when is_atom(AttrTp) ->
    bytes_of_node_attribute({AttrTp,StrData}).



bytes_of_node_attributes(AttrLst) ->
    lists:append([ bytes_of_node_attribute(C) || C <- AttrLst]).

bytes_of_node_children(Lst, StartOffset_0) ->
    case Lst of
        [] ->
            []; %% No bytes if no children
        _ ->
            {FL,_} =
                lists:foldl(
                    fun (C, {NodeBinList, StartOffset}) ->
                        Bytes = bytes_of_node(C, StartOffset),
                        {[Bytes | NodeBinList], StartOffset + bt_count(Bytes)}
                    end, {[], StartOffset_0}, Lst ++ case Lst of [false] -> []; _ -> [false] end
                ),
            lists:reverse(FL)
    end.

bytes_of_node(Node, StartOffset)
  when is_tuple(Node);
       Node =:= false ->
    case Node of
        false ->
            EndOffset_0 = 0;
        _ ->
            EndOffset_0 = StartOffset + 4 + 4 + 4 + 1
    end,
    case Node of
        
        {NodeName,NodeAttr,NodeList} when is_list(NodeList) ->
            NumProperties = length(NodeAttr),
            PropListBin = bytes_of_node_attributes(NodeAttr),
            PropertyListLen = bt_count(PropListBin),
            NodeNameBin = bytes_of_node_name(NodeName),
            EndOffset_1 = EndOffset_0 + (bt_count(NodeNameBin) - 1) + bt_count(PropListBin),
            ReturnedNodeListBin = bytes_of_node_children(NodeList, EndOffset_1),
            NodeListBin = ReturnedNodeListBin,
            EndOffset_2 = EndOffset_1 + bt_count(NodeListBin);
        false ->
            EndOffset_2 = 0,
            NumProperties = 0,
            PropertyListLen = 0,
            PropListBin = [],
            NodeListBin = [],
            NodeNameBin = [0],
            EndOffset_2 = EndOffset_0
    end,
    [
        bytes_uint32(EndOffset_2),
        bytes_uint32(NumProperties),
        bytes_uint32(PropertyListLen),
        case Node of
            false ->
                [ <<0>> ];
            _ ->
                [
                NodeNameBin,
                PropListBin,
                NodeListBin
                ]
        end
    ];
bytes_of_node([NodeName,NodeAttr|NodeList], StartOffset) ->
    bytes_of_node({NodeName,NodeAttr,NodeList}, StartOffset).

bytes_of_uuid(UUID) ->
    case length(UUID) =:= 16 of
        true ->
            iolist_to_binary(UUID);
        false ->
            iolist_to_binary(string:copies([0], 16)) %% 16 bytes
    end.

bytes_of_padding_120_zeros() ->
    %% 120 bytes of padded zeros
    iolist_to_binary(string:copies([0], 120)).



bytes_of_00_00() ->
    <<16#00,16#00>>.

bytes_of_version() ->
    BytesVersionNumber = bytes_uint32(?FBX_VERSION),
    BytesVersionNumber.

bytes_of_alignment_padding(Offset) ->
    %% Pad with zeros for 16 byte alignment
    MLen = Offset rem 16,
    case MLen > 0 of
        true ->
            iolist_to_binary(string:copies([0], 16 - MLen));
        false ->
            []
    end.


%% Write the header, the main body and the footer of the FBX
%%
write_fbx({MainBody,UUIDIsh_1,UUIDIsh_2}) ->
    StartOffset_0 = 0,
    
    BytesHeader = bytes_of_header(),
    StartOffset_1 = bt_count(BytesHeader) + StartOffset_0,
    BytesMainBody = bytes_of_node_children(MainBody, StartOffset_1),
    
    BytesUUID1 = bytes_of_uuid(UUIDIsh_1),
    StartOffset_2 = bt_count(BytesMainBody) + bt_count(BytesUUID1) + StartOffset_1,
    
    %% At least in some files, there tends to be a UUID-like block, followed
    %% by what seems to be some padding zeros to align at a 16-byte boundary,
    %% it can be noticed the first UUID-like block isn't aligned, but everything
    %% after is aligned. After the alignment padding, bytes of the file version
    %% appears (like in the header), and some more zeros and then another
    %% UUID-like block, but some files across different versions are sometimes 
    %% different from this.
    %%
    BytesPad1 = bytes_of_alignment_padding(StartOffset_2),
    BytesPad2 = bytes_of_00_00(),
    BytesPad3 = bytes_of_00_00(),
    BytesPad4 = bytes_of_version(),
    BytesPad5 = bytes_of_padding_120_zeros(),
    BytesUUID2 = bytes_of_uuid(UUIDIsh_2),
    
    [
        BytesHeader,
        BytesMainBody,
        BytesUUID1,
        BytesPad1,
        BytesPad2,
        BytesPad3,
        BytesPad4,
        BytesPad5,
        BytesUUID2
    ].

%%%
%%%
%%%

%%% Functions that turn intermediate nested tuple and list structures 
%%% into a nested list binary closer to FBX's file structure.


-define(FBX_OBJECTS_MODEL_VERSION, 232).
-define(FBX_SUMMARY_VERSION, 101).
-define(FBX_SUMMARY_TAKES_VERSION, 101).
-define(FBX_DEFINITIONS_VERSION, 100).
-define(FBX_OBJECTS_MODEL_LAYERELEMENT_VERSION, 101).
-define(FBX_OBJECTS_MODEL_LAYER_VERSION, 100).
-define(FBX_OBJECTS_MATERIAL_VERSION, 101).
-define(FBX_AMBIENTRENDERSETTINGS_VERSION, 101).
-define(FBX_GEOMETRY_VERSION, 124).

name_type_pair(Name, Type)
  when is_list(Name),
       is_binary(Type) ->
    name_type_pair(str_to_utf8b(Name), Type);
name_type_pair(Name, Type)
  when is_binary(Name),
       is_binary(Type) ->
    <<Name/binary, 0, 1, Type/binary>>.



%% Property tuples to FBX data types
%%

-define(PROPTUPLE(Name, Cont), 
    {<<"P">>,[iolist_to_binary(Name) | Cont],[]}
    ).

prop({Name,{color,R,G,B}}) ->
    ?PROPTUPLE(Name, [
        <<"Color">>,<<>>,<<"A">>,
            {attr_dflo,R},
            {attr_dflo,G},
            {attr_dflo,B}
    ]);
prop({Name,{color_rgb,R,G,B}}) ->
    ?PROPTUPLE(Name, [
        <<"ColorRGB">>, <<"Color">>, <<>>,
            {attr_dflo,R},
            {attr_dflo,G},
            {attr_dflo,B}
    ]);
prop({Name,{number,Num}}) when is_integer(Num) ->
    ?PROPTUPLE(Name, [
        <<"Number">>,<<>>,<<"A">>,
            {attr_int,Num}
    ]);
prop({Name,{number,Num}}) when is_float(Num) ->
    ?PROPTUPLE(Name, [
        <<"Number">>,<<>>,<<"A">>,
            {attr_dflo,Num}
    ]);
prop({Name,{vec3d,X,Y,Z}}) ->
    ?PROPTUPLE(Name, [
        <<"Vector3D">>,<<"Vector">>,<<>>,
            {attr_dflo,X},
            {attr_dflo,Y},
            {attr_dflo,Z}
    ]);
prop({Name,{double,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"double">>,<<"Number">>,<<>>,
            {attr_dflo,Num}
    ]);
prop({Name,{int,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"int">>,<<"Integer">>,<<>>,
            {attr_int,Num}
    ]);
prop({Name,{bool,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"bool">>,<<>>,<<>>,
            {attr_int,Num}
    ]);
prop({Name,{enum,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"enum">>,<<>>,<<>>,
            {attr_int,Num}
    ]);
prop({Name,{lcltranslation,X,Y,Z}}) ->
    ?PROPTUPLE(Name, [
        <<"Lcl Translation">>,<<>>,<<"A">>,
            {attr_dflo,X},
            {attr_dflo,Y},
            {attr_dflo,Z}
    ]);
prop({Name,{lclscaling,X,Y,Z}}) ->
    ?PROPTUPLE(Name, [
        <<"Lcl Scaling">>,<<>>,<<"A">>,
            {attr_dflo,X},
            {attr_dflo,Y},
            {attr_dflo,Z}
    ]);
prop({Name,{lclrotation,X,Y,Z}}) ->
    ?PROPTUPLE(Name, [
        <<"Lcl Rotation">>,<<>>,<<"A">>,
            {attr_dflo,X},
            {attr_dflo,Y},
            {attr_dflo,Z}
    ]);
prop({Name,{ktime,Time}}) ->
    ?PROPTUPLE(Name, [
        <<"KTime">>,<<>>,<<>>,
            {attr_int64, Time}
    ]);
prop({Name,{kstring,Str}}) when is_binary(Str) ->
    ?PROPTUPLE(Name, [
        <<"KString">>,<<>>,<<>>,Str
    ]);
prop({Name,{kstring_xrefurl,Str}}) when is_binary(Str) ->
    ?PROPTUPLE(Name, [
        <<"KString">>,<<"XRefUrl">>,<<>>,Str
    ]);
prop({Name,{datetime,Str}}) when is_binary(Str) ->
    ?PROPTUPLE(Name, [
        <<"DateTime">>,<<>>,<<>>,Str
    ]);
prop({Name,{visibility,Val}}) ->
    ?PROPTUPLE(Name, [
        <<"Visibility">>, <<>>, <<"A">>,
            {attr_dflo, Val}
    ]);
prop({Name,{object}}) ->
    ?PROPTUPLE(Name, [
        <<"object">>, <<>>, <<>>
    ]);
prop({Name,{reference}}) ->
    ?PROPTUPLE(Name, [
        <<"Reference">>,<<>>,<<"A">>
    ]);
prop({Name,{compound}}) ->
    ?PROPTUPLE(Name, [
        <<"Compound">>,<<>>,<<>>
    ]);
prop({Name,{bool_a,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"Bool">>,<<>>,<<"A">>,
            {attr_int,Num}
    ]);
prop({Name,{integer_a,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"Integer">>,<<>>,<<"A">>,
            {attr_int,Num}
    ]);
prop({Name,{float_a,Num}}) ->
    ?PROPTUPLE(Name, [
        <<"Float">>,<<>>,<<"A">>,
            {attr_dflo,Num}
    ]);
prop({Name,{color_alpha,R,G,B,Alph}}) ->
    ?PROPTUPLE(Name, [
        <<"ColorAndAlpha">>,<<>>,<<"A">>,
            {attr_dflo,R},
            {attr_dflo,G},
            {attr_dflo,B},
            {attr_dflo,Alph}
    ]);
prop({Name,{visibility_inheritance,B}}) ->
    ?PROPTUPLE(Name, [
        <<"Visibility Inheritance">>,<<>>,<<>>,
            {attr_int,B}
    ]);

prop({Name,{vectex,X,Y,Z}}) ->  % Shows up in texture properties.
    ?PROPTUPLE(Name, [
        <<"Vector">>, <<>>, <<"A">>,
            {attr_dflo, X},
            {attr_dflo, Y},
            {attr_dflo, Z}
    ]).


%% Turn list of tuple properties into list of FBX nested parameters.
%%
proplist_to_properties70(PropLst) ->
    [<<"Properties70">>, [] |
        lists:map(fun prop/1, PropLst)].


%% Default properties for materials that will show up in the
%% template part of definitions.
%%
default_properties70_material() ->
    [
        {<<"ShadingModel">>, {kstring, <<"Phong">>}},
        {<<"MultiLayer">>, {bool, 0}},
        {<<"EmissiveColor">>, {color, 0.0, 0.0, 0.0}},
        {<<"EmissiveFactor">>,{number,1.0}},
        {<<"AmbientColor">>, {color, 0.0, 0.0, 0.0}},
        {<<"AmbientFactor">>, {number, 1.0}},
        {<<"DiffuseColor">>, {color, 0.0, 0.0, 0.0}},
        {<<"DiffuseFactor">>,{number,1.0}},
        {<<"NormalMap">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"Bump">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"BumpFactor">>, {double, 1.0}},
        {<<"TransparentColor">>,{color,1.0,1.0,1.0}},
        {<<"TransparencyFactor">>, {number, 0.0}},
        {<<"Opacity">>, {number, 1.0}},
        {<<"DisplacementColor">>, {color_rgb, 0.0, 0.0, 0.0}},
        {<<"DisplacementFactor">>, {double, 1.0}},
        {<<"VectorDisplacementColor">>, {color_rgb, 0.0, 0.0, 0.0}},
        {<<"VectorDisplacementFactor">>, {double, 1.0}},
        {<<"SpecularColor">>, {color, 0.0, 0.0, 0.0}},
        {<<"SpecularFactor">>,{number,1.0}},
        {<<"Opacity">>, {number, 1.0}},
        {<<"ReflectionColor">>,{color,0.0,0.0,0.0}},
        {<<"ReflectionFactor">>, {number, 1.0}},
        {<<"Shininess">>, {number, 0.0}},
        {<<"ShininessExponent">>,{number,20.0}}
    ].


%% Default properties for geometries that will show up
%% in the template part of the definitions.
%%
default_properties70_geometry() ->
    [
        {<<"Color">>, {color_rgb, 0.9, 0.9, 0.9}},
        {<<"BBoxMin">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"BBoxMax">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"Primary Visibility">>, {bool, 1}},
        {<<"Casts Shadows">>, {bool, 1}},
        {<<"Receive Shadows">>, {bool, 1}}
    ].
    

%% Default properties for models that will show up
%% in the template part of the definitions.
%%
default_properties70_object() ->
    [
        {<<"QuaternionInterpolate">>, {enum, 0}},
        {<<"RotationOffset">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"RotationPivot">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"ScalingOffset">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"ScalingPivot">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"TranslationActive">>, {bool, 0}},
        {<<"TranslationMin">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"TranslationMax">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"TranslationMinX">>, {bool, 0}},
        {<<"TranslationMinY">>, {bool, 0}},
        {<<"TranslationMinZ">>, {bool, 0}},
        {<<"TranslationMaxX">>, {bool, 0}},
        {<<"TranslationMaxY">>, {bool, 0}},
        {<<"TranslationMaxZ">>, {bool, 0}},
        {<<"RotationOrder">>, {enum, 0}},
        {<<"RotationSpaceForLimitOnly">>, {bool, 0}},
        {<<"RotationStiffnessX">>, {double, 0.0}},
        {<<"RotationStiffnessY">>, {double, 0.0}},
        {<<"RotationStiffnessZ">>, {double, 0.0}},
        {<<"AxisLen">>, {double, 10.0}},
        {<<"PreRotation">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"PostRotation">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"RotationActive">>, {bool, 0}},
        {<<"RotationMin">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"RotationMax">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"RotationMinX">>, {bool, 0}},
        {<<"RotationMinY">>, {bool, 0}},
        {<<"RotationMinZ">>, {bool, 0}},
        {<<"RotationMaxX">>, {bool, 0}},
        {<<"RotationMaxY">>, {bool, 0}},
        {<<"RotationMaxZ">>, {bool, 0}},
        {<<"InheritType">>, {enum, 0}},
        {<<"ScalingActive">>, {bool, 0}},
        {<<"ScalingMin">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"ScalingMax">>, {vec3d, 1.0, 1.0, 1.0}},
        {<<"ScalingMinX">>, {bool, 0}},
        {<<"ScalingMinY">>, {bool, 0}},
        {<<"ScalingMinZ">>, {bool, 0}},
        {<<"ScalingMaxX">>, {bool, 0}},
        {<<"ScalingMaxY">>, {bool, 0}},
        {<<"ScalingMaxZ">>, {bool, 0}},
        {<<"GeometricTranslation">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"GeometricRotation">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"GeometricScaling">>, {vec3d, 1.0, 1.0, 1.0}},
        {<<"MinDampRangeX">>, {double, 0.0}},
        {<<"MinDampRangeY">>, {double, 0.0}},
        {<<"MinDampRangeZ">>, {double, 0.0}},
        {<<"MaxDampRangeX">>, {double, 0.0}},
        {<<"MaxDampRangeY">>, {double, 0.0}},
        {<<"MaxDampRangeZ">>, {double, 0.0}},
        {<<"MinDampStrengthX">>, {double, 0.0}},
        {<<"MinDampStrengthY">>, {double, 0.0}},
        {<<"MinDampStrengthZ">>, {double, 0.0}},
        {<<"MaxDampStrengthX">>, {double, 0.0}},
        {<<"MaxDampStrengthY">>, {double, 0.0}},
        {<<"MaxDampStrengthZ">>, {double, 0.0}},
        {<<"PreferedAngleX">>, {double, 0.0}},
        {<<"PreferedAngleY">>, {double, 0.0}},
        {<<"PreferedAngleZ">>, {double, 0.0}},
        {<<"LookAtProperty">>, {object}},
        {<<"UpVectorProperty">>, {object}},
        {<<"Show">>, {bool, 1}},
        {<<"NegativePercentShapeSupport">>, {bool, 1}},
        {<<"DefaultAttributeIndex">>, {int, -1}},
        {<<"Freeze">>, {bool, 0}},
        {<<"LODBox">>, {bool, 0}},
        {<<"Lcl Translation">>, {lcltranslation, 0.0, 0.0, 0.0}},
        {<<"Lcl Rotation">>, {lclrotation, 0.0, 0.0, 0.0}},
        {<<"Lcl Scaling">>, {lclscaling, 1.0, 1.0, 1.0}},
        {<<"Visibility">>, {visibility, 1.0}},
        {<<"Visibility Inheritance">>, {visibility_inheritance, 1}}
    ].


map_attr_dflo(#fopt{compress=true},L) ->
    [{{deflate, attr_dflo_list}, L}];
map_attr_dflo(_, L) ->
    [{attr_dflo_list, L}].

map_attr_int(#fopt{compress=true},L) ->
    [{{deflate, attr_int_list}, L}];
map_attr_int(_,L) ->
    [{attr_int_list, L}].
    
layer(L_Normals, L_Binormals, L_Tangents, L_Smoothing, L_UV, L_Materials) ->
    D1 =
        [<<"Layer">>, [{attr_int, 0}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_LAYER_VERSION}]]
        ],
    L_No = if length(L_Normals) > 0 ->
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementNormal">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    L_Nb = if length(L_Binormals) > 0 ->
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementBinormal">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    L_Nt = if length(L_Tangents) > 0 ->
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementTangents">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    L_Sm = if length(L_Smoothing) > 0 ->
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementSmoothing">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    L_M = if length(L_Materials) > 0 ->
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementMaterial">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    L_U = if length(L_UV) > 0 -> 
            [[<<"LayerElement">>, [],
                [<<"Type">>, [{attr_str, <<"LayerElementUV">>}]],
                [<<"TypedIndex">>, [{attr_int, 0}]]
            ]];
            true -> []
    end,
    D1 ++ L_No ++ L_Nb ++ L_Nt ++ L_Sm ++ L_M ++ L_U.


%% Turn list of tuples to Vertices list
%%
tuplelist_to_numbers(List) ->
    tuplelist_to_numbers(List, []).
tuplelist_to_numbers([{X,Y,Z}|L], OL) ->
    tuplelist_to_numbers(L, [Z,Y,X|OL]);
tuplelist_to_numbers([], OL) ->
    lists:reverse(OL).


%% Turn list of tuples to UV list
%%
tuplelist2_to_numbers(List) ->
    tuplelist2_to_numbers(List, []).
tuplelist2_to_numbers([{X,Y}|L], OL) ->
    tuplelist2_to_numbers(L, [Y,X|OL]);
tuplelist2_to_numbers([], OL) ->
    lists:reverse(OL).


%% Turn list of lists to PolygonVertexIndex list
%%
lists_to_polyvtxi(List) ->
    lists_to_polyvtxi(List, []).
lists_to_polyvtxi([List|L], OL) ->
    [A|B] = lists:reverse(List),
    List_1 = [((-A)-1)|B],
    lists_to_polyvtxi(L, [List_1|OL]);
lists_to_polyvtxi([], OL) ->
    lists:reverse(lists:flatten(OL)).


object_mesh_geom(FbxOpt, OID, Name, Props, Vertices, PolygonVertexIndex, Normals, Binormals, Tangents, UV, UVIndex, Materials, Edges) ->

    L_UV = case UV of [] -> []; _ ->
        [[<<"LayerElementUV">>, [{attr_int, 0}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_LAYERELEMENT_VERSION}]],
            [<<"Name">>, [{attr_str, <<>>}]],
            [<<"MappingInformationType">>, [{attr_str, <<"ByPolygonVertex">>}]],
            [<<"ReferenceInformationType">>, [{attr_str, <<"IndexToDirect">>}]],
            [<<"UV">>, map_attr_dflo(FbxOpt, tuplelist2_to_numbers(UV))],
            [<<"UVIndex">>, map_attr_int(FbxOpt, lists:append(UVIndex))]
        ]]
    end,
    L_Normals = case Normals of [] -> []; _ ->
        [[<<"LayerElementNormal">>, [{attr_int, 0}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_LAYERELEMENT_VERSION}]],
            [<<"Name">>, [{attr_str, <<>>}]],
            [<<"MappingInformationType">>, [{attr_str, <<"ByPolygonVertex">>}]],
            [<<"ReferenceInformationType">>, [{attr_str, <<"Direct">>}]],
            [<<"Normals">>, map_attr_dflo(FbxOpt, tuplelist_to_numbers(Normals))] %,
            % [<<"NormalsW">>, map_attr_int(FbxOpt, [1 || _ <- Normals])]
        ]]
    end,
    L_Binormals = case Binormals of [] -> [] %% TODO: Later
    end,
    L_Tangents = case Tangents of [] -> [] %% TODO: Later
    end,
    Smoothing = [Sm || {_,Sm} <- Edges],
    L_Smoothing = 
        [[<<"LayerElementSmoothing">>, [{attr_int, 0}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_LAYERELEMENT_VERSION}]],
            [<<"Name">>, [{attr_str, <<>>}]],
            [<<"MappingInformationType">>, [{attr_str, <<"ByEdge">>}]],
            [<<"ReferenceInformationType">>, [{attr_str, <<"Direct">>}]],
            [<<"Smoothing">>, map_attr_int(FbxOpt, Smoothing)]
        ]],

    L_Materials = case Materials of [] -> []; _ ->
        Materials_U = lists:usort(Materials),
        OneMat = case Materials_U of [_] -> true; _ -> false end,
        [[<<"LayerElementMaterial">>, [{attr_int, 0}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_LAYERELEMENT_VERSION}]],
            [<<"Name">>, [{attr_str, <<>>}]],
            [<<"MappingInformationType">>, [{attr_str, if OneMat -> <<"AllSame">>; true -> <<"ByPolygon">> end}]],
            [<<"ReferenceInformationType">>, [{attr_str, <<"IndexToDirect">>}]],
            [<<"Materials">>, map_attr_int(FbxOpt, if OneMat -> Materials_U; true -> Materials end)]
        ]]
    end,
    
    UEdges = [UIdx || {UIdx,_} <- Edges],

    [<<"Geometry">>, [{attr_int64, OID}, {attr_str, Name}, {attr_str, <<"Mesh">>}],
        proplist_to_properties70(Props),
        
        [<<"Vertices">>, map_attr_dflo(FbxOpt, tuplelist_to_numbers(Vertices))],
        [<<"PolygonVertexIndex">>, map_attr_int(FbxOpt, lists_to_polyvtxi(PolygonVertexIndex))],
        [<<"Edges">>, map_attr_int(FbxOpt, UEdges)],
        [<<"GeometryVersion">>, [{attr_int, ?FBX_GEOMETRY_VERSION}]]
        ] ++ L_Normals ++ L_Binormals ++ L_Tangents ++ L_Smoothing ++ L_UV ++ L_Materials ++ [
        layer(L_Normals, L_Binormals, L_Tangents, L_Smoothing, L_UV, L_Materials)
    ].


%% Calculate list of unique edges
%%    
uedge_list(PolygonVIdx, HEs_0) ->
    HEs = gb_sets:from_list(HEs_0),
    uedge_list(PolygonVIdx, HEs, []).
uedge_list([[C|[B2|_]=L]|R], HEs, OL) ->
    OL_1 = uedge_list_1(C, L, [{C,B2}|OL]),
    uedge_list(R, HEs, OL_1);
uedge_list([], HEs, OL) ->
    uedge_list_2(lists:reverse(OL), HEs).
uedge_list_1(F, [C|[B2|_]=L], OL) ->
    uedge_list_1(F, L, [{C,B2}|OL]);
uedge_list_1(F, [C], OL) ->
    [{C,F}|OL].
uedge_list_2(Edges, HEs) ->
    uedge_list_2(Edges, HEs, 0, #{}, []).
uedge_list_2([{A1,A2}|R], HEs, Idx, Seen, OL) ->
    {OL_1, Seen_1} =
        case maps:is_key({A1,A2},Seen) orelse
             maps:is_key({A2,A1},Seen)
        of
            true ->
                {OL, Seen};
            false ->
                Sm = case gb_sets:is_member({A1,A2}, HEs) orelse
                          gb_sets:is_member({A2,A1}, HEs) of
                    true -> 0;
                    false -> 1
                end,
                {[{Idx,Sm}|OL],Seen#{ {A1,A2} => 1 }}
        end,
    uedge_list_2(R, HEs, Idx+1, Seen_1, OL_1);
uedge_list_2([], _, _Idx, _Seen, OL) ->
    lists:reverse(OL).


object_mesh_model(OID, Name, Props) ->

    [<<"Model">>, [{attr_int64, OID}, {attr_str, Name}, {attr_str, <<"Mesh">>}],
        [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MODEL_VERSION}]],
        proplist_to_properties70(Props),
        [<<"Culling">>, [{attr_str, <<"CullingOff">>}]]
    ].


phong_material(MID, Name, Props) ->
    [<<"Material">>, [{attr_int64, MID}, {attr_str, Name}, {attr_str, ""}],
            [<<"Version">>, [{attr_int, ?FBX_OBJECTS_MATERIAL_VERSION}]],
            [<<"ShadingModel">>, [{attr_str, "phong"}]],
            [<<"MultiLayer">>, [{attr_int, 0}]],
            proplist_to_properties70(Props)
    ].

default_properties70_video() ->
    [
        {<<"Width">>, {int, 0}},
        {<<"Height">>, {int, 0}},
        {<<"Path">>, {kstring, <<"">>}},
        {<<"RelPath">>, {kstring, <<"">>}},
        {<<"AccessMode">>, {enum, 0}},
        {<<"FrameRate">>, {double, 0.0}},
        {<<"FreeRunning">>, {bool, 0}},
        {<<"ImageSequence">>, {bool, 0}},
        {<<"ImageSequenceOffset">>, {int, 0}},
        {<<"InterlaceMode">>, {enum, 0}},
        {<<"LastFrame">>, {int, 0}},
        {<<"Loop">>, {bool, 0}},
        {<<"Offset">>, {ktime, 0}},
        {<<"PlaySpeed">>, {double, 1.0}},
        {<<"StartFrame">>, {int, 0}},
        {<<"StopFrame">>, {int, 0}}
    ].
object_video(VID, Name, Props, FilenameOnly, RelativePath) ->
    [<<"Video">>, [{attr_int64, VID}, {attr_str, Name}, {attr_str, <<"Clip">>}],
        [<<"Type">>, [{attr_str, <<"Clip">>}]],
        proplist_to_properties70(
            [
                {<<"Path">>, {kstring, FilenameOnly}}
            ] ++ Props),
        [<<"UseMipMap">>, [{attr_int, 0}]],
        [<<"Filename">>, [{attr_str, FilenameOnly}]],
        [<<"RelativeFilename">>, [{attr_str, RelativePath}]]
    ].

default_properties70_texture() ->
    [
        {<<"PremultiplyAlpha">>, {bool, 1}},
        {<<"UVSet">>, {kstring, <<"default">>}},
        {<<"Translation">>, {vectex, 0.0, 0.0, 0.0}},
        {<<"Rotation">>, {vectex, 0.0, 0.0, 0.0}},
        {<<"Scaling">>, {vectex, 1.0, 1.0, 1.0}},
        {<<"Texture alpha">>, {number, 1.0}},
        {<<"TextureTypeUse">>, {enum, 0}},
        {<<"CurrentTextureBlendMode">>, {enum, 1}},
        {<<"UseMaterial">>, {bool, 1}},
        {<<"UseMipMap">>, {bool, 0}},
        {<<"CurrentMappingType">>, {enum, 0}},
        {<<"UVSwap">>, {bool, 0}},
        {<<"WrapModeU">>, {enum, 0}},
        {<<"WrapModeV">>, {enum, 0}},
        {<<"TextureRotationPivot">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"TextureScalingPivot">>, {vec3d, 0.0, 0.0, 0.0}},
        {<<"VideoProperty">>, {object}}
    ].
object_texture(TID, Name, Props, FilenameOnly, RelativePath)
  when is_integer(TID) ->
    [<<"Texture">>, [{attr_int64, TID}, {attr_str, Name}, {attr_str, "TextureVideoClip"}],
        [<<"Type">>, [{attr_str, <<"TextureVideoClip">>}]],
        [<<"Version">>, [{attr_int, 202}]],
        [<<"TextureName">>, [{attr_str, FilenameOnly}]],
        proplist_to_properties70(Props),
        [<<"Media">>, [{attr_str, FilenameOnly}]],
        [<<"FileName">>, [{attr_str, FilenameOnly}]],
        [<<"RelativeFilename">>, [{attr_str, RelativePath}]],
        [<<"ModelUVTranslation">>, [{attr_dflo, 0.0}, {attr_dflo, 0.0}]],
        [<<"ModelUVScaling">>, [{attr_dflo, 1.0}, {attr_dflo, 1.0}]],
        [<<"Texture_Alpha_Source">>, [{attr_str, <<"None">>}]],
        [<<"Cropping">>, [{attr_int, 0}, {attr_int, 0}, {attr_int, 0}, {attr_int, 0}]]
    ].



fbx_form_definitions(Geoms, Models, Mats, Texs, Vids)
  when is_list(Geoms),
       is_list(Models),
       is_list(Mats),
       is_list(Texs),
       is_list(Vids) ->
    Geoms_Len = length(Geoms),
    Models_Len = length(Models),
    Mats_Len = length(Mats),
    Texs_Len = length(Texs),
    Vids_Len = length(Vids),
    Total = Geoms_Len + Models_Len + Mats_Len + Texs_Len + Vids_Len + 1,
    D0 =
        [<<"Definitions">>, [],
            [<<"Version">>, [{attr_int, ?FBX_DEFINITIONS_VERSION}]],
            [<<"Count">>, [{attr_int, Total}]],
            [<<"ObjectType">>, [{attr_str, <<"GlobalSettings">>}],
                [<<"Count">>, [{attr_int, 1}]]
            ]
        ],

    D_G = if Geoms_Len > 0 ->
                [[<<"ObjectType">>, [{attr_str, <<"Geometry">>}],
                    [<<"Count">>, [{attr_int, Geoms_Len}]],
                    [<<"PropertyTemplate">>, [{attr_str, <<"FbxMesh">>}],
                        proplist_to_properties70(default_properties70_geometry())
                    ]
                ]];
            true -> []
    end,
    D_Mo = if Models_Len > 0 ->
                [[<<"ObjectType">>, [{attr_str, <<"Model">>}],
                    [<<"Count">>, [{attr_int, Models_Len}]],
                    [<<"PropertyTemplate">>, [{attr_str, <<"FbxNode">>}], 
                        proplist_to_properties70(default_properties70_object())
                    ]
                ]];
            true -> []
    end,
    D_Ma = if Mats_Len > 0 ->
                [[<<"ObjectType">>, [{attr_str, <<"Material">>}],
                    [<<"Count">>, [{attr_int, Mats_Len}]],
                    [<<"PropertyTemplate">>, [{attr_str, <<"FbxSurfacePhong">>}],
                        proplist_to_properties70(default_properties70_material())
                    ]
                ]];
            true -> []
    end,
    D_Tx = if Texs_Len > 0 ->
                [[<<"ObjectType">>, [{attr_str, <<"Texture">>}],
                    [<<"Count">>, [{attr_int, Texs_Len}]],
                    [<<"PropertyTemplate">>, [{attr_str, <<"FbxFileTexture">>}],
                        proplist_to_properties70(default_properties70_texture())
                    ]
                ]];
            true -> []
    end,
    D_Vd = if Vids_Len > 0 ->
                [[<<"ObjectType">>, [{attr_str, <<"Video">>}],
                    [<<"Count">>, [{attr_int, Vids_Len}]],
                    [<<"PropertyTemplate">>, [{attr_str, <<"FbxVideo">>}],
                        proplist_to_properties70(default_properties70_video())
                    ]
                ]];
            true -> []
    end,
    D0 ++ D_G ++ D_Mo ++ D_Ma ++ D_Tx ++ D_Vd.

fbx_form_header_ext_creation_time_stamp(CreationTime) ->
    Year = str_to_num(string:sub_string(CreationTime, 1+0, 4)),
    Month = str_to_num(string:sub_string(CreationTime, 1+5, 7)),
    Day = str_to_num(string:sub_string(CreationTime, 1+8, 10)),
    Hour = str_to_num(string:sub_string(CreationTime, 1+11, 13)),
    Minute = str_to_num(string:sub_string(CreationTime, 1+14, 16)),
    Second = str_to_num(string:sub_string(CreationTime, 1+17, 19)),
    Millisecond = str_to_num(string:sub_string(CreationTime, 1+20, 23)),
    [<<"CreationTimeStamp">>, [],
        [<<"Version">>, [{attr_int, 1000}]],
        [<<"Year">>, [{attr_int, Year}]],
        [<<"Month">>, [{attr_int, Month}]],
        [<<"Day">>, [{attr_int, Day}]],
        [<<"Hour">>, [{attr_int, Hour}]],
        [<<"Minute">>, [{attr_int, Minute}]],
        [<<"Second">>, [{attr_int, Second}]],
        [<<"Millisecond">>, [{attr_int, Millisecond}]] 
    ].

fbx_form_header_ext(CreationTool, CreationTime, TimeSaved) ->
    WingsName = <<"Wings3D">>,
    WingsVersion = <<"1.0">>,
    [<<"FBXHeaderExtension">>, [],
        [<<"FBXHeaderVersion">>, [{attr_int, 1003}]],
        [<<"FBXVersion">>, [{attr_int, ?FBX_VERSION}]],
        [<<"EncryptionType">>, [{attr_int, 0}]],
        fbx_form_header_ext_creation_time_stamp(CreationTime),
        [<<"Creator">>, [{attr_str, CreationTool}]],
        [<<"SceneInfo">>, [{attr_str, name_type_pair(<<"GlobalInfo">>,<<"SceneInfo">>)},{attr_str,<<"UserData">>}],
            [<<"Type">>, [{attr_str, <<"UserData">>}]],
            [<<"Version">>, [{attr_int, 100}]],
            [<<"MetaData">>, [],
                [<<"Version">>, [{attr_int,100}]],
                [<<"Title">>, [{attr_str, <<"">>}]],
                [<<"Subject">>, [{attr_str, <<"">>}]],
                [<<"Author">>, [{attr_str, <<"">>}]],
                [<<"Keywords">>, [{attr_str, <<"">>}]],
                [<<"Revision">>, [{attr_str, <<"">>}]],
                [<<"Comment">>, [{attr_str, <<"">>}]]
            ],
            proplist_to_properties70([
                {<<"DocumentUrl">>, {kstring, <<>>}},
                {<<"SrcDocumentUrl">>, {kstring, <<>>}},
                {<<"Original">>, {compound}},
                {<<"Original|ApplicationVendor">>, {kstring, WingsName}},
                {<<"Original|ApplicationName">>, {kstring, WingsName}},
                {<<"Original|ApplicationVersion">>, {kstring, WingsVersion}},
                {<<"Original|DateTime_GMT">>, {datetime, TimeSaved}},
                {<<"Original|FileName">>, {kstring, <<>>}},
                {<<"LastSaved">>, {compound}},
                {<<"LastSaved|ApplicationVendor">>, {kstring, WingsName}},
                {<<"LastSaved|ApplicationName">>, {kstring, WingsName}},
                {<<"LastSaved|ApplicationVersion">>, {kstring, WingsVersion}},
                {<<"LastSaved|DateTime_GMT">>, {datetime, TimeSaved}}
            ])
        ]
    ].

fbx_form_global_settings() ->
    [<<"GlobalSettings">>, [],
        [<<"Version">>, [{attr_int, 1000}]],
        proplist_to_properties70([
            {<<"UpAxis">>, {int, 1}},
            {<<"UpAxisSign">>, {int, 1}},
            {<<"FrontAxis">>, {int, 2}},
            {<<"FrontAxisSign">>, {int, 1}},
            {<<"CoordAxis">>, {int, 0}},
            {<<"CoordAxisSign">>, {int, 1}},
            {<<"OriginalUpAxis">>, {int, -1}},
            {<<"OriginalUpAxisSign">>, {int, 1}},
            {<<"UnitScaleFactor">>, {double, 1.0}},
            {<<"OriginalUnitScaleFactor">>, {double, 1.0}},
            {<<"AmbientColor">>, {color_rgb, 0.0, 0.0, 0.0}},
            {<<"DefaultCamera">>, {kstring, <<"Producer Perspective">>}},
            {<<"TimeMode">>, {enum, 0}},
            {<<"TimeSpanStart">>, {ktime, 0}},
            {<<"TimeSpanStop">>, {ktime, 1000}},
            {<<"CustomFrameRate">>, {double, 24.0}}
        ])
    ].



obj_vs(Obj) ->
    proplists:get_value(vertices, Obj, false).
obj_faces(Obj) ->
    proplists:get_value(vtxindex, Obj, false).
obj_ns(Obj) ->
    proplists:get_value(ns, Obj, false).
obj_uv(Obj) ->
    proplists:get_value(uv, Obj, false).
obj_uvidx(Obj) ->
    proplists:get_value(uvidx, Obj, false).
obj_mats(Obj) ->
    proplists:get_value(mats, Obj, false).
obj_edges(Obj) ->
    proplists:get_value(edges, Obj, false).

    
object_mesh_model_props(A) ->
    RotationPivot = proplists:get_value(rotation_pivot, A, false),
    ScalingPivot = proplists:get_value(scaling_pivot, A, false),
    D1 = object_mesh_model_props_rpiv(RotationPivot),
    D2 = object_mesh_model_props_spiv(ScalingPivot),
    D1 ++ D2 ++ [
        %% Have our model rescaled from 0.0 ... 1.0 to 0.0 ... 100.0
        {<<"Lcl Scaling">>, {lclscaling,100.0,100.0,100.0}}
    ].

    
object_mesh_model_props_rpiv([X,Y,Z]) ->
    [ {<<"RotationPivot">>, {vec3d,float(X),float(Y),float(Z)}} ];
object_mesh_model_props_rpiv(false) ->
    [].


object_mesh_model_props_spiv([X,Y,Z]) ->
    [ {<<"ScalingPivot">>, {vec3d, float(X), float(Y), float(Z)}} ];
object_mesh_model_props_spiv(false) ->
    [].


obj_filename_only(Obj) ->
    %% Only the filename without a path ("todo.bmp")
    proplists:get_value(filename_only, Obj, false).
    
obj_relative_path(Obj) ->
    %% Relative path on the file system ("..\\ASCII\\todo.bmp")
    proplists:get_value(relative_path, Obj, false).
    
    
object_texture_props(_A) ->
    [].
object_video_props(A) ->
    FilenameOnly = obj_filename_only(A),
    [
        {<<"Path">>, {kstring, FilenameOnly}}
    ].
    
phong_material_props(A) ->
    Props = proplists:get_value(prop, A, []),
    Props.

fbx_form_objects(FbxOpt, Geoms, Models, Mats, Texs, Vids) ->
    D_G = 
        [
            object_mesh_geom(FbxOpt, OID, NameTypePair,
                object_mesh_model_props(A),
                obj_vs(A),
                obj_faces(A),
                obj_ns(A),
                [], [],
                obj_uv(A),
                obj_uvidx(A),
                obj_mats(A),
                obj_edges(A))
        || {_OType, {OID, NameTypePair, _}, A} <- Geoms ],
    D_Mo =
        [
            case OType of
                model ->
                    object_mesh_model(OID, NameTypePair,
                        object_mesh_model_props(A))
            end
        || {OType, {OID, NameTypePair, _}, A} <- Models ],
    D_Ma = [
            phong_material(MID, NameTypePair,
                phong_material_props(A))
        || {mat, {MID, NameTypePair, _}, A} <- Mats],
    D_Tx = [
            object_texture(TID, NameTypePair,
                object_texture_props(A),
                obj_filename_only(A),
                obj_relative_path(A))
        || {tex, {for, _, {TID, NameTypePair, _}}, A} <- Texs],
    D_Vd = [
            object_video(VID, NameTypePair,
                object_video_props(A),
                obj_filename_only(A),
                obj_relative_path(A))
        || {video, {VID, NameTypePair, _}, A} <- Vids],
    
    {<<"Objects">>, [], D_G ++ D_Mo ++ D_Ma ++ D_Tx ++ D_Vd}.


%% Build list of connections between videos and textures.
%%
connections_to_textures(Texs) ->
    D4_0 = lists:map( fun connections_to_textures_1/1, Texs),
    D4 = lists:append(D4_0),
    D4.
connections_to_textures_1({_,{for,_,{TID, _,_}},R})
  when is_integer(TID) ->
    connections_to_textures_1(TID,R,[]).
connections_to_textures_1(TID,[{{video,{VID,_,_}},_}|R],OL)
  when is_integer(TID),is_integer(VID) ->
    %% Videos that connect to textures
    OL_1 = [{oo, VID, TID} |OL],
    connections_to_textures_1(TID,R,OL_1);
connections_to_textures_1(TID,[_|R],OL) ->
    connections_to_textures_1(TID,R,OL);
connections_to_textures_1(_TID,[],OL) ->
    lists:reverse(OL).


%% Build list of connections between textures and materials.
%%
connections_to_materials(Texs, Mats) ->
    D4 = connections_to_textures(Texs),
    D3_0 = lists:map( fun connections_to_materials_1/1, Mats),
    D3 = lists:append(D3_0),
    D3 ++ D4.
connections_to_materials_1({_,{OID, OName, _},R})
  when is_integer(OID) ->
    connections_to_materials_1(OID, OName, R, []).
connections_to_materials_1(OID, OName, [{{tex, {for, Which, {TID,_,_}}}, _}|R], OL)
  when is_integer(TID) ->
    %% Textures that connect to materials
    OL_1 = [{op, TID, OID, Which}|OL],
    connections_to_materials_1(OID, OName, R, OL_1);
connections_to_materials_1(_OID, _OName, [{{tex, _}, _}=_A|_R], _OL) ->
    error(unexpected);
connections_to_materials_1(OID, OName, [_|R], OL) ->
    connections_to_materials_1(OID, OName, R, OL);
connections_to_materials_1(_OID, _OName, [], OL) ->
    lists:reverse(OL).


%% Build list of connections from geometries and materials
%% to models.
%%
connections_to_models(Models) ->
    D2_0 = lists:map( fun connections_to_models_1/1, Models),
    D2 = lists:append(D2_0),
    D2.
connections_to_models_1({_,{OID,OName,_},R})
  when is_integer(OID) ->
    connections_to_models_1(OID,OName,R,[]).
connections_to_models_1(OID,OName,[{{mat, {MID,_,_}}, _}|R],OL)
  when is_integer(MID) ->
    %% Materials that connect to Models
    OL_1 = [{oo, MID, OID} | OL],
    connections_to_models_1(OID,OName,R,OL_1);
connections_to_models_1(OID,OName,[{{geom, {MID,_,_}},_}|R],OL)
  when is_integer(MID) ->
    %% Geometries that connect to Models
    OL_1 = [{oo, MID, OID} | OL],
    connections_to_models_1(OID,OName,R,OL_1);
connections_to_models_1(OID,OName,[_|R],OL) ->
    connections_to_models_1(OID,OName,R,OL);
connections_to_models_1(_OID,_OName,[],OL) ->
    lists:reverse(OL).


%% Build list of connections between models and the root node.
%%
connections_to_root_node(Models) ->
    %% Models that connect to Scene
    D1 = [ {oo, OID, 0}
        || {_,{OID, _OName, _},_} <- Models],
    D1.


fbx_form_connections(CL) ->

    D1 = lists:map(
            fun
                ({oo,OID1,OID2})
                  when is_integer(OID1), is_integer(OID2) ->
                    [<<"C">>, [<<"OO">>,
                        {attr_int64, OID1},
                        {attr_int64, OID2}]
                    ];
                ({op,OID1,OID2,Kind})
                  when is_integer(OID1), is_integer(OID2), is_binary(Kind) ->
                    [<<"C">>, [<<"OP">>,
                        {attr_int64, OID1},
                        {attr_int64, OID2},
                        {attr_str, Kind}]
                    ]
            end, CL),
    
    {<<"Connections">>, [], D1}.


%%% Break out elements out of other elements for FBX's structure.
%%%

%% Break out videos from textures.
%%
textures_objects(Texs,IdAMap) ->
    textures_objects(Texs,IdAMap,[],[]).
textures_objects([T|Texs],IdAMap,OL,O2) ->
    {Vids, T_1,IdAMap_1} = textures_objects_1(T,IdAMap),
    textures_objects(Texs,IdAMap_1,[Vids|OL],[T_1|O2]);
textures_objects([],IdAMap,OL,O2) ->
    Vids = lists:append(lists:reverse(OL)),
    Texs = lists:reverse(O2),
    {Vids,Texs,IdAMap}.
textures_objects_1({Kind,{for,Which,{TID, _,_}=Ident},R},IdAMap)
  when is_integer(TID) ->
    {R_1, IdAMap_1} = add_ident_numbers(R, IdAMap),
    {Vids, R_2} = textures_objects_1(TID,R_1,[],[]),
    {Vids, {Kind,{for,Which,Ident},R_2}, IdAMap_1}.
textures_objects_1(TID,[{{video,Ident},VI}|R],OL,O2) ->
    %% Videos that connect to Textures
    OL_1 = [{video,Ident,VI} |OL],
    O2_1 = [{{video,Ident},vid}|O2],
    textures_objects_1(TID,R,OL_1,O2_1);
textures_objects_1(TID,[E|R],OL,O2) ->
    O2_1 = [E|O2],
    textures_objects_1(TID,R,OL,O2_1);
textures_objects_1(_TID,[],OL,O2) ->
    {lists:reverse(OL), lists:reverse(O2)}.



%% Break out videos, textures from materials.
%%
materials_objects(Mats,IdAMap) ->
    materials_objects(Mats,IdAMap,[],[],[]).
materials_objects([M|Mats],IdAMap,OV,OL,O2) ->
    {Texs_0, M_1, IdAMap_1} = materials_objects_1(M, IdAMap),
    {Vids, Texs, IdAMap_2} = textures_objects(Texs_0, IdAMap_1),
    materials_objects(Mats,IdAMap_2,[Vids|OV],[Texs|OL],[M_1|O2]);
materials_objects([],IdAMap,OV,OL,O2) ->
    Vids = lists:append(lists:reverse(OV)),
    Texs = lists:append(lists:reverse(OL)),
    Mats = lists:reverse(O2),
    { Vids, Texs, Mats, IdAMap}.
materials_objects_1({Kind,{OID, OName, _}=Ident,R},IdAMap)
  when is_integer(OID) ->
    {R_1, IdAMap_1} = add_ident_numbers(R, IdAMap),
    {Texs, R_2} = materials_objects_1(OID, OName, R_1, [],[]),
    {Texs, {Kind,Ident,R_2},IdAMap_1}.
materials_objects_1(OID, OName, [{{tex, Ident}, Tx}|R], OL, O2) ->
    %% Textures that connect to Models
    OL_1 = [{tex, Ident, Tx} |OL],
    O2_1 = [{{tex, Ident}, tex}|O2],
    materials_objects_1(OID, OName, R, OL_1,O2_1);
materials_objects_1(OID, OName, [E|R], OL,O2) ->
    O2_1 = [E|O2],
    materials_objects_1(OID, OName, R, OL,O2_1);
materials_objects_1(_OID, _OName, [], OL,O2) ->
    {lists:reverse(OL), lists:reverse(O2)}.


name_only(OName) ->
    name_only(OName, []).
name_only(<<C,OName/binary>>, OL)
  when C =/= 0, C =/= 1 ->
    name_only(OName, [C|OL]);
name_only(_, OL) ->
    lists:reverse(OL).


%% Break out nested geometries in models
%%
mesh_objects(Models, IdAMap) ->
    mesh_objects(Models, IdAMap, [], []).
mesh_objects([M|Models], IdAMap, OL, O2) ->
    {Geoms, M_1} = mesh_objects_1(M, IdAMap),
    mesh_objects(Models, IdAMap, [Geoms|OL], [M_1|O2]);
mesh_objects([], _IdAMap, OL, O2) ->
    Geoms = lists:append(lists:reverse(OL)),
    Models = lists:reverse(O2),
    {Geoms, Models}.
mesh_objects_1({Kind,{OID,OName,_}=Ident,R}, IdAMap)
  when is_integer(OID) ->
    {Geoms, R_1} = mesh_objects_1(OID, IdAMap,OName,R,[],[]),
    {Geoms, {Kind,Ident,R_1}}.
mesh_objects_1(OID,{ActualIdAMap,_}=IdAMap,OName,[{{mat, AtomIdent}, _}|R],OL,O2)
  when is_atom(AtomIdent) ->
    ActualIdent = maps:get(AtomIdent, ActualIdAMap),
    OL_1 = OL,
    O2_1 = [{{mat, ActualIdent}, mat}|O2],
    mesh_objects_1(OID,IdAMap,OName,R,OL_1,O2_1);
mesh_objects_1(OID,IdAMap,OName,[{geom, G}|R],OL,O2) ->
    GID = OID+150000, % This should be okay
    GName = name_only(OName) ++ "__geo",
    Ident = {GID, name_type_pair(GName, <<"Geometry">>), <<"Geometry">>},
    OL_1 = [{geom, Ident, G} | OL],
    O2_1 = [{{geom, Ident}, geom}|O2],
    mesh_objects_1(OID,IdAMap,OName,R,OL_1,O2_1);
mesh_objects_1(OID,{_, _}=IdAMap,OName,[E|R],OL,O2) ->
    O2_1 = [E|O2],
    mesh_objects_1(OID,IdAMap,OName,R,OL,O2_1);
mesh_objects_1(_OID,_IdAMap,_OName,[],OL,O2) ->
    {lists:reverse(OL), lists:reverse(O2)}.

date_time_to_strings({{Year,Mon,Day},{Hr,Min,Sec}}) ->
    TimeSaved_0 = io_lib:format(
        "~2..0w/~2..0w/~4..0w ~2..0w:~2..0w:~2..0w.~3..0w", [
            Mon,Day,Year,Hr,Min,Sec,0
        ]),
    TimeSaved = iolist_to_binary(TimeSaved_0),
    CreationTime_0 = io_lib:format(
        "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w", [
        Year,Mon,Day,Hr,Min,Sec,0
        ]),
    CreationTime = lists:flatten(CreationTime_0),
    {CreationTime, TimeSaved}.

%% Assemble the different sections into an iolist
fbx_form(FbxOpt, Objs0, Mats0, IdAMap) ->

    {CreationTime, TimeSaved} = date_time_to_strings(erlang:localtime()),
    CreationTool = "Wings3D FBX Plugin",
    
    %% These values seem random, but not sure.
    FileID    = [
        16#12,16#34,16#56,16#78,16#9A,16#BC,16#DE,16#FF,
        16#11,16#22,16#33,16#44,16#55,16#66,16#77,16#88],
    UUIDIsh_1 = FileID, % NOTE: This value is usually different from FileID
    
    %% This value always shows up the same in every FBX file
    UUIDIsh_2 = [
        16#F8,16#5A,16#8C,16#6A,16#DE,16#F5,16#D9,16#7E,
        16#EC,16#E9,16#0C,16#E3,16#75,16#8F,16#29,16#0B],
    
    %% Separate the objects into geometries and models
    %%
    {Geoms, Models} = mesh_objects(Objs0, IdAMap),
    
    %% Separate the materials into materials, videos and textures
    %%
    {Vids, Texs, Mats, _} = materials_objects(Mats0, IdAMap),
    
    CL3 = connections_to_materials(Texs, Mats),
    CL2 = connections_to_models(Models),
    CL1 = connections_to_root_node(Models),
    Connections = CL1 ++ CL2 ++ CL3,

    {
        [
            fbx_form_header_ext(CreationTool, CreationTime, TimeSaved),
            [<<"FileId">>, [{attr_data, FileID}]],
            [<<"CreationTime">>, [{attr_str, CreationTime}]],
            [<<"Creator">>, [{attr_str, CreationTool}]],
            fbx_form_global_settings(),
            fbx_form_definitions(Geoms, Models, Mats, Texs, Vids),
            fbx_form_objects(FbxOpt, Geoms, Models, Mats, Texs, Vids),
            fbx_form_connections(Connections)
        ],
        UUIDIsh_1, % UUID-ish 1
        UUIDIsh_2  % UUID-ish 2
    }.

str_to_num(S) ->
    list_to_integer(lists:flatten(S)).


%% Write the completed iolist to file.
%%
write_fbx_file(Filename, FbxOpt, Objs, Mats, IdAMap) ->
    {ok, Fo} = file:open(Filename,[write,binary]),
    write_list(write_fbx(fbx_form(FbxOpt, Objs, Mats, IdAMap)), Fo),
    file:close(Fo).


fbx_type_from_atom(A) ->
    case A of
        model -> <<"Model">>;
        mat -> <<"Material">>;
        tex -> <<"Texture">>;
        video -> <<"Video">>
    end.


%% Assign numeric identifiers to all the elements (models, materials,
%% textures).
%%
add_ident_numbers(Objs0) ->
    add_ident_numbers(Objs0, #{}, 1000).
add_ident_numbers(Objs0, {IdAMap, NextNumber}) ->
    add_ident_numbers(Objs0, IdAMap, NextNumber).

add_ident_numbers(Objs0, IdAMap, NextNumber) ->
    add_ident_numbers(Objs0, IdAMap, NextNumber, []).
add_ident_numbers([{A,B,C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_atom(B) ->
    B_1 = atom_to_list(B),
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{A,ActualIdent,C}|OL]);
add_ident_numbers([{A,B,C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_list(B) ->
    B_1 = B,
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{A,ActualIdent,C}|OL]);

add_ident_numbers([{{A,B},C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_atom(B) ->
    B_1 = atom_to_list(B),
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{{A,ActualIdent},C}|OL]);
add_ident_numbers([{{A,B},C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_list(B) ->
    B_1 = B,
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{{A,ActualIdent},C}|OL]);

add_ident_numbers([{{A,{for,Which,B}},C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_atom(B),is_binary(Which) ->
    B_1 = atom_to_list(B),
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{{A,{for,Which,ActualIdent}},C}|OL]);
add_ident_numbers([{{A,{for,Which,B}},C}|L], IdAMap, NextNumber, OL)
  when is_atom(A),is_list(B),is_binary(Which) ->
    B_1 = B,
    NameTypePair = name_type_pair(B_1, fbx_type_from_atom(A)),
    ActualIdent = {NextNumber,NameTypePair,A},
    add_ident_numbers(
        L,
        IdAMap#{B => ActualIdent},
        NextNumber+1,
        [{{A,{for,Which,ActualIdent}},C}|OL]);

add_ident_numbers([Other|L], IdAMap, NextNumber, OL) ->
    add_ident_numbers(L,IdAMap,NextNumber,[Other|OL]);
add_ident_numbers([], IdAMap, NextNumber, OL) ->
    {lists:reverse(OL), {IdAMap, NextNumber}}.


str_to_utf8b(A)
  when is_list(A) ->
    unicode:characters_to_nfc_binary(A);
str_to_utf8b(A)
  when is_binary(A) ->
    A.



%%%
