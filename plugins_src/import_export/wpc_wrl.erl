%%
%%  wpc_wrl.erl --
%%
%%     VRML export plugin.
%%
%%  Copyright (c) 2004-2011 Sean Hinde, Danni Coy and Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_wrl).

%% Thanks KayosIII (Danni Aaron Coy) who wrote the export of UV coordinates 
%%

-export([init/0, menu/2, command/2]).

-import(lists, [foreach/2, foldl/3]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    x3d_import:init_import(),
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{wrl,Ask}}}, St) ->
    x3d_import:do_import(Ask, St);
command({file,{export,{wrl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{wrl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"X3D/VRML (.x3d|.x3dj|.wrl)...", wrl, [option]}].


props(wrl) ->
    [{ext, ".wrl"},{ext_desc, ?__(1,"VRML 2.0 File")}];
props(x3d) ->
    [{ext, ".x3d"},{ext_desc, ?__(2,"X3D File")}];
props(x3dj) ->
    [{ext, ".x3dj"},{ext_desc, ?__(3,"X3D JSON File")}].

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"VRML Export Options"), dialog(export),
               fun(Res) ->
                       {file,{Op,{wrl,Res}}}
               end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    FileType = proplists:get_value(exp_file_type, Attr, wrl),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Tesselation = proplists:get_value(tesselation, Attr, none),
    ExportUV = proplists:get_value(include_uvs, Attr, true),
    ExportVC = proplists:get_value(include_colors, Attr, true),
    Ps = [{tesselation,Tesselation},{subdivisions,SubDivs},
          {include_uvs,ExportUV},{include_colors,ExportVC}|props(FileType)],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
            export(Filename, Contents, Attr)
    end.
set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

dialog(export) ->
    wpa:pref_set_default(?MODULE, default_filetype, ".png"),
    [{hframe,[{label,?__(2,"Output format") ++ ":"},
              {menu, [ {?__(1,"VRML 2.0"), wrl},
                       {?__(3,"X3D"), x3d},
                       {?__(4,"X3D JSON"), x3dj} ],
                     wrl, [{key, exp_file_type}]}]},
     wpa:dialog_template(?MODULE, tesselation),
     panel,
     wpa:dialog_template(?MODULE, export)].

%% The intent is to create each object from
%% a sequence of Shapes. Each "sub Shape" will consist of all the
%% faces and vertices for one material..
export(File_name, Export0, Attr) ->
    WhichExt = case filename:extension(File_name) of
        ".x3d"  -> x3d;
        ".x3dj" -> x3dj;
        _       -> wrl
    end,
    %%io:format("~p~n~p~n",[Objs, Mat]),
    Filetype = proplists:get_value(default_filetype, Attr, ".png"),
    ExportN  = proplists:get_value(include_normals, Attr, true),
    Dir = filename:dirname(File_name),
    Export1 = wpa:save_images(Export0, Dir, Filetype),
    Export = export_transform(Export1, Attr),
    export(WhichExt, File_name, Export, ExportN, Dir).
export(WhichExt, File_name, Export, ExportN, Dir) ->
    #e3d_file{objs=Objs,mat=Mat,creator=Creator} = Export,
    {ok,F} = file:open(File_name, [write]),
    case WhichExt of
        wrl ->
            io:format(F, "#VRML V2.0 utf8\n", []),
            io:format(F, "#Exported from ~s\n",[Creator]);
        x3d ->
            BaseName = filename:basename(File_name),
            ExportedFrom = io_lib:format("Exported from ~s", [Creator]),
            BaseName_1 = unicode:characters_to_nfc_binary(BaseName),
            io:format(F, "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n", []),
            io:format(F, "<!DOCTYPE X3D PUBLIC \"ISO//Web3D//DTD X3D 3.0//EN\"\n", []),
            io:format(F, "  \"http://www.web3d.org/specifications/x3d-3.0.dtd\">\n", []),
            io:format(F, "<X3D version=\"3.0\" profile=\"Interchange\">\n", []),
            io:format(F, "  <head>\n", []),
            io:format(F, "    <meta name=\"filename\" content=\"~s\"/>\n", [BaseName_1]),
            io:format(F, "    <meta name=\"generator\" content=\"~s\"/>\n", [ExportedFrom]),
            io:format(F, "  </head>\n", []),
            io:format(F, "  <Scene>\n", []);
        x3dj ->
            BaseName = filename:basename(File_name),
            ExportedFrom = io_lib:format("Exported from ~s", [Creator]),
            BaseName_1 = unicode:characters_to_nfc_binary(BaseName),
            io:format(F, "{ \"X3D\": {\n", []),
            io:format(F, "  \"encoding\":\"UTF-8\",\n", []),
            io:format(F, "  \"@profile\":\"Interchange\",\n", []),
            io:format(F, "  \"@version\":\"3.2\",\n", []),
            io:format(F, "  \"@xsd:noNamespaceSchemaLocation\":\"https://www.web3d.org/specifications/x3d-3.2.xsd\",\n", []),
            io:format(F, "  \"JSON schema\":\"https://www.web3d.org/specifications/x3d-4.0-JSONSchema.autogenerated.json\",\n", []),
            io:format(F, "  \"head\": {\n", []),
            io:format(F, "    \"meta\": [\n", []),
            io:format(F, "      { \"@name\":\"title\",\n", []),
            io:format(F, "        \"@content\":\"~s\" },\n", [BaseName_1]),
            io:format(F, "      { \"@name\":\"generator\",\n", []),
            io:format(F, "        \"@content\":\"~s\" } ] },\n", [ExportedFrom]),
            io:format(F, "  \"Scene\": {\n", []),
            io:format(F, "    \"-children\":[\n", [])
    end,
    [#e3d_object{name=LastObjName}|_] = lists:reverse(Objs),
    
    try foldl(fun(#e3d_object{name = Name, obj=Obj}, Used_mats0) ->
              Name_1 = unicode:characters_to_nfc_binary(clean_id(Name)),
              case WhichExt of
                wrl ->
                  io:format(F, "DEF ~s Transform {\n",[Name_1]),
                  io:format(F, "  children [\n",[]);
                x3d ->
                  io:format(F, "    <Transform DEF=\"~s\">\n", [Name_1]);
                x3dj ->
                  io:format(F, "{ \"Transform\": {\n",[]),
                  io:format(F, "  \"@DEF\": \"~s\",\n",[Name_1]),
                  io:format(F, "  \"-children\": [\n",[])
              end,
              ObjMesh = e3d_mesh:vertex_normals(Obj),
              Meshes = e3d_mesh:split_by_material(ObjMesh),
              Used_mats = all_shapes(WhichExt, fun(Mesh,UsedMats) ->
                          export_object(
                                fun(Indent, Data) ->
                                    to_io(WhichExt, F, Indent, Data)
                                end, Mesh, Mat,
                                ExportN,
                                UsedMats, Dir)
                      end, F, Used_mats0, Meshes),
              case WhichExt of
                wrl ->
                  io:put_chars(F, "\n  ]\n"),
                  io:put_chars(F, "}\n\n");
                x3d ->
                  io:put_chars(F, "\n    </Transform>\n");
                x3dj ->
                  io:put_chars(F, "\n  ]\n"),
                  io:format(F, "} }~s\n\n", [cma(LastObjName =/= Name)])
              end,
              Used_mats
          end, [], Objs)
    catch _:Err:ST ->
            io:format(?__(1,"VRML Error: ~P in")++" ~p~n", [Err,30,ST])
    end,
    case WhichExt of
        wrl -> ok;
        x3d ->
            io:put_chars(F, "  </Scene>\n"),
            io:put_chars(F, "</X3D>\n");
        x3dj ->
            io:put_chars(F, "    ] }\n"),
            io:put_chars(F, "} }\n")
    end,
    ok = file:close(F).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

export_object(Output, #e3d_mesh{fs=Fs0,ns=NTab,vs=VTab,tx=UVTab,vc=ColTab}, 
              Mat_defs, ExportN, Used_mats0, Dir) ->
    Output(2, {start_tag, "Shape"}),
    Output(3, {after_attrs}),
    Fs = reorder(Fs0),
    [#e3d_face{mat=[Material|_]}|_] = Fs,
    Used_mats = material(Output, Material, Mat_defs, Used_mats0, Dir),
    Output(3, {start_tag, "geometry", "IndexedFaceSet"}),

    if ExportN == true ->
            Output(4, {tag_attr_bool, "normalPerVertex", true, true});
       true -> ignore
    end,
    
    if
        (ColTab == []) -> ignore;
        true -> %% Use vertex colors
            Output(4, {tag_attr_bool, "colorPerVertex", true, true})
    end,
    WriteCoordIndex = fun (IdxIndent) ->
        %% Write vertex indices
        Output(IdxIndent, {tag_attr_array, "coordIndex", {fun(#e3d_face{vs=Vs}) -> print_face(Output, Vs) end,Fs}, true})
    end,
    WriteNormalIndex = fun (IdxIndent) ->
        if ExportN ->
            %% Write per-vertex normal index
            Output(IdxIndent, {tag_attr_array, "normalIndex", {fun(#e3d_face{ns=Ns}) -> print_face(Output, Ns) end,Fs}, true});
           true  -> ignore
        end
    end,
    WriteColorIndex = fun (IdxIndent) ->
        Output(IdxIndent, {tag_attr_array, "colorIndex", {fun(#e3d_face{vc=Vc}) -> print_face(Output, Vc) end,Fs}, true})
    end,
    WriteTexCoordIndex = fun (IdxIndent) ->
        Output(IdxIndent, {tag_attr_array, "texCoordIndex", {fun(#e3d_face{tx=UV}) -> print_face(Output, UV) end,Fs}, true})
    end,
    Output(4, {only_x3d, [WriteCoordIndex, WriteNormalIndex] ++ if 
        ColTab /= []-> [WriteColorIndex];
        UVTab /= [] -> [WriteTexCoordIndex];
        true -> []
    end}),
    Output(4, {after_attrs}),
    
    HasAfterNormal = ColTab /= [] orelse UVTab /= [],
    HasAfterCoord = ExportN orelse HasAfterNormal,
    
    %% Helpers
    W3 = fun({X,Y,Z}) ->  Output(6, {array_item_vec3, {X,Y,Z}}) end,
    W2 = fun({U,V}) ->    Output(6, {array_item_vec2, {U,V}}) end,
    %% Write vertex coordinates
    Output(4, {start_tag, "coord", "Coordinate"}),
    Output(5, {tag_attr_array, "point", {W3,VTab}, false}),
    Output(5, {after_attrs}),
    Output(4, {close_tag, "Coordinate", HasAfterCoord}),
    Output(4, {only_wrl, [WriteCoordIndex]}),

    if ExportN ->
            %% Write Normal vectors
            Output(4, {start_tag, "normal", "Normal"}),
            Output(5, {tag_attr_array, "vector", {W3,NTab}, false}),
            Output(5, {after_attrs}),
            Output(4, {close_tag, "Normal", HasAfterNormal}),
            Output(4, {only_wrl, [WriteNormalIndex]});
       true  -> ignore
    end,

    if 
        ColTab /= []->                % Use vertex colors
            Output(4, {start_tag, "color", "Color"}),
            Output(5, {tag_attr_array, "color", {W3,ColTab}, false}),
            Output(5, {after_attrs}),
            Output(4, {close_tag, "Color", false}),
            Output(4, {only_wrl, [WriteColorIndex]});
        UVTab /= [] ->                % Use UV-coords
            Output(4, {start_tag, "texCoord", "TextureCoordinate"}),
            Output(5, {tag_attr_array, "point", {W2,UVTab}, false}),
            Output(5, {after_attrs}),
            Output(4, {close_tag, "TextureCoordinate", false}),
            Output(4, {only_wrl, [WriteTexCoordIndex]});
        true ->
            ignore
    end,
    %% Close Shape and IndexedFaceSet
    Output(3, {close_tag, "IndexedFaceSet", false}),
    Output(2, {close_tag, "Shape", false}),
    Used_mats.

material(Output, Name, Mat_defs, Used, Dir) ->
    case lists:member(Name, Used) of
        true ->
            use_material(Output, Name),
            Used;
        false ->
            def_material(Output, Name, lookup(Name, Mat_defs), Dir),
            [Name|Used]
    end.

% Note: vrml represents ambient colour as a proportion of 
% diffuse colour, not in its own right.
def_material(Output, Name, Mat0, Dir) ->
    DiffuseMapVal = case lists:keysearch(maps, 1, Mat0) of
        {value, {maps,Maps}} ->
            lists:keysearch(diffuse, 1, Maps);
        _ -> error
    end,
    HaveMap = case DiffuseMapVal of {value, _} -> true; _ -> false end,
    
    Mat = lookup(opengl, Mat0),
    Output(3, {start_tag, "appearance", "Appearance"}),
    Output(4, {after_attrs}),
    
    Output(4, {start_tag_defined, "material", "Material", clean_id(Name)}),
    {Ar, Ag, Ab, O0} = lookup(ambient, Mat),
    {Dr, Dg, Db, _} = lookup(diffuse, Mat),
    Output(5, {tag_attr_vec3, "diffuseColor", {Dr, Dg, Db}, true}),
    {Er, Eg, Eb, _} = lookup(emission, Mat),
    Output(5, {tag_attr_vec3, "emissiveColor", {Er, Eg, Eb}, true}),
    {Sr, Sg, Sb, _} = lookup(specular, Mat),
    Output(5, {tag_attr_vec3, "specularColor", {Sr, Sg, Sb}, true}),
    case (Ar+Ag+Ab)/3 of
        Amb0 when abs(Amb0) < ?EPSILON ->
            Amb = 1.0, %% wrml needs a non zero value for ambient intensity
            O = 0.0;
        Amb0 ->
            Amb = Amb0,
            O = 1.0-O0
    end,
    Output(5, {tag_attr_float, "ambientIntensity", Amb, true}),
    Output(5, {tag_attr_float, "transparency", O, true}),
    S = lookup(shininess, Mat),
    Output(5, {tag_attr_float, "shininess", S, false}),
    Output(5, {after_attrs}),
    Output(4, {close_tag, "Material", HaveMap}),
    case DiffuseMapVal of
        {value, {diffuse,#e3d_image{filename=FileName}}} ->
            File = case string:prefix(FileName, Dir) of
                       nomatch -> FileName;
                       [_Delim|File0] -> File0
                   end,
            Output(4, {start_tag, "texture", "ImageTexture"}),
            Output(5, {tag_attr_string, "url", File, false}),
            Output(4, {close_tag_no_content, "ImageTexture", false});
        _ ->
            ignore
    end,
    Output(3, {close_tag, "Appearance", true}).
    
use_material(Output, Mat) ->
    Output(3, {start_tag, "appearance", "Appearance"}),
    Output(3, {after_attrs}),
    Output(4, {attr_use, "material", "Material", clean_id(Mat)}),
    Output(3, {close_tag, "Appearance", true}).

print_face(Output, Vs) ->
    Output(0, {raw, "          "}),
    foreach(fun(V) -> Output(0, {raw, io_lib:format("~p, ", [V])}) end, Vs),
    Output(0, {raw,"-1"}).

%%  The reorder is cludge to sort faces (vertex lists) after order,
%%  seems that Strata needs it to get good triangulation.
reorder(Fs0) ->
    Fs = [reorder_face(Face) || Face <- Fs0],
    lists:keysort(#e3d_face.vs, Fs).
reorder_face(F=#e3d_face{vs=[V|Vs],vc=Vc,tx=Tx,ns=Ns}) ->
    Min = min(Vs,1,0,V),
    F#e3d_face{vs=reorder_list([V|Vs],Min,[]),
               vc=reorder_list(Vc,Min,[]),
               tx=reorder_list(Tx,Min,[]),
               ns=reorder_list(Ns,Min,[])}.

min([V|R],N,_,Min) when V < Min ->
    min(R,N+1,N,V);
min([_|R],N,Idx,Min) ->
    min(R,N+1,Idx,Min);
min([],_,Idx,_) -> Idx.

reorder_list([], _, []) -> [];
reorder_list(L, 0, Acc) -> L ++ lists:reverse(Acc);
reorder_list([V|R],N,Acc) ->
    reorder_list(R,N-1,[V|Acc]).
    

% Useful helpers
all(F, IO, [H,H1|T]) ->
    F(H),    
    io:put_chars(IO, ",\n"),
    all(F, IO, [H1|T]);
all(F, _, [H]) ->
    F(H),
    ok.


all_shapes(wrl, F, IO, AccIn, [H,H1|T]) ->
    Acc = F(H, AccIn),
    io:put_chars(IO, "    ,\n"),    
    all_shapes(wrl, F,IO,Acc,[H1|T]);
all_shapes(x3d, F, IO, AccIn, [H,H1|T]) ->
    Acc = F(H, AccIn),
    all_shapes(x3d, F,IO,Acc,[H1|T]);
all_shapes(x3dj, F, IO, AccIn, [H,H1|T]) ->
    Acc = F(H, AccIn),
    io:put_chars(IO, "    ,\n"),    
    all_shapes(x3dj, F,IO,Acc,[H1|T]);
all_shapes(_, F, _, AccIn, [H]) ->
    F(H, AccIn).

lookup(K, L) ->
    {value, {K, V}} =  lists:keysearch(K, 1, L),
    V.

% Fix to SF bug report 539951 - invalid ids
% If the first char is not allowed
% then prefix whole id with W. For rest of not allowed chars
% turn them into a safe 2 char representation.
clean_id(Id) when is_atom(Id) ->
    clean_id(atom_to_list(Id));
clean_id([First|T]) ->
    case is_not_allowed_first_char(First) of
        true ->
            clean_id_rest([$W,First|T]);
        false ->
            [First|clean_id_rest(T)]
    end.

clean_id_rest([]) ->
    [];
clean_id_rest([H|T]) ->
    case is_not_allowed_char(H) of
        true ->
            fix_char(H)++clean_id_rest(T);
        false ->
            [H|clean_id_rest(T)]
    end.

is_not_allowed_first_char(C) ->
    (is_not_allowed_char(C) or ((C >= 16#30) and (C =< 16#39))).

is_not_allowed_char(C) ->
    (C =< 16#20) or lists:member(C, [16#22,16#23,16#27,16#2b,16#2c,
                                     16#2d,16#2e,16#5b,16#5c,16#5d,
                                     16#7b,16#7d,16#7f]).
fix_char($ ) ->
    "_";
fix_char(C) ->
    fix1(<<C>>).

fix1(<<C1:4,C2:4>>) ->
    [C1+65,C2+65].

-define(INDENTSPC(Ind),  string:copies("  ", Ind)).

to_io(wrl, F, Indent, Data) ->
    case Data of
        {only_wrl, List} ->
            [Fn(Indent) || Fn <- List];
        {only_x3d, _} ->
            ok;
        {start_tag, OntoAttr, Tag} ->
            io:format(F, "~s~s ~s {\n",[ ?INDENTSPC(Indent), OntoAttr, Tag]);
        {start_tag, Tag} ->
            io:format(F, "~s~s {\n",[ ?INDENTSPC(Indent), Tag]);
        {array_item_vec3, {V1,V2,V3}} ->
            io:format(F, "~s~p ~p ~p",[ ?INDENTSPC(Indent), V1,V2,V3]);
        {array_item_vec2, {V1,V2}} ->
            io:format(F, "~s~p ~p",[ ?INDENTSPC(Indent), V1,V2]);
        {raw, Str} ->
            io:format(F, "~s",[Str]);
        {attr_use, OntoAttr, _Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s~s USE ~s\n",[ ?INDENTSPC(Indent), OntoAttr, Id_1]);
        {start_tag_defined, OntoAttr, Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s~s DEF ~s ~s {\n",[ ?INDENTSPC(Indent), OntoAttr, Id_1, Tag]);
        {close_tag, _Tag, _} ->
            io:format(F, "~s}\n", [ ?INDENTSPC(Indent) ]);
        {close_tag_no_content, _Tag, _} ->
            io:format(F, "~s}\n", [ ?INDENTSPC(Indent) ]);
        {after_attrs} ->
            ok;
        {tag_attr_bool, AttrName, AttrValue, _} ->
            AttrVal_1 = case AttrValue of
                true -> "TRUE"
            end,
            io:format(F, "~s~s ~s\n", [ ?INDENTSPC(Indent), AttrName, AttrVal_1]);
        {tag_attr_vec3, AttrName, {V1,V2,V3}, _} ->
            io:format(F, "~s~s ~p ~p ~p\n", [ ?INDENTSPC(Indent), AttrName, V1,V2,V3]);
        {tag_attr_float, AttrName, AttrValue, _} ->
            io:format(F, "~s~s ~p\n", [ ?INDENTSPC(Indent), AttrName, AttrValue]);
        {tag_attr_string, AttrName, AttrValue, _} ->
            AttrValue_1 = binary_to_list(unicode:characters_to_nfc_binary(AttrValue)),
            io:format(F, "~s~s ~p\n", [ ?INDENTSPC(Indent), AttrName, AttrValue_1]);
        {tag_attr_array, AttrName, {WF, List}, _} ->
            io:format(F, "~s~s [\n", [ ?INDENTSPC(Indent), AttrName]),
            all(WF,F,List),
            io:put_chars(F, " ]\n")
    end;
to_io(x3d, F, Indent, Data) ->
    case Data of
        {only_x3d, List} ->
            [Fn(Indent) || Fn <- List];
        {only_wrl, _} ->
            ok;
        {start_tag, _OntoAttr, Tag} ->
            io:format(F, "~s<~s \n",[ ?INDENTSPC(Indent), Tag]);
        {start_tag, Tag} ->
            io:format(F, "~s<~s \n",[ ?INDENTSPC(Indent), Tag]);
        {array_item_vec3, {V1,V2,V3}} ->
            io:format(F, "~s~p ~p ~p",[ ?INDENTSPC(Indent), V1,V2,V3]);
        {array_item_vec2, {V1,V2}} ->
            io:format(F, "~s~p ~p",[ ?INDENTSPC(Indent), V1,V2]);
        {raw, Str} ->
            io:format(F, "~s",[Str]);
        {attr_use, _OntoAttr, Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s<~s USE=\"~s\"/>\n",[ ?INDENTSPC(Indent), Tag, Id_1]);
        {start_tag_defined, _OntoAttr, Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s<~s DEF=\"~s\" \n",[ ?INDENTSPC(Indent), Tag, Id_1]);
        {close_tag, Tag, _} ->
            io:format(F, "~s</~s>\n", [ ?INDENTSPC(Indent), Tag ]);
        {close_tag_no_content, _Tag, _} ->
            io:format(F, "~s/>\n", [ ?INDENTSPC(Indent) ]);
        {after_attrs} ->
            io:format(F, "~s>\n", [ ?INDENTSPC(Indent) ]);
        {tag_attr_bool, AttrName, AttrValue, _} ->
            AttrVal_1 = case AttrValue of
                true -> "TRUE"
            end,
            io:format(F, "~s~s=\"~s\"\n", [ ?INDENTSPC(Indent), AttrName, AttrVal_1]);
        {tag_attr_vec3, AttrName, {V1,V2,V3}, _} ->
            io:format(F, "~s~s=\"~p ~p ~p\"\n", [ ?INDENTSPC(Indent), AttrName, V1,V2,V3]);
        {tag_attr_float, AttrName, AttrValue, _} ->
            io:format(F, "~s~s=\"~p\"\n", [ ?INDENTSPC(Indent), AttrName, AttrValue]);
        {tag_attr_string, AttrName, AttrValue, _} ->
            Str_1 = unicode:characters_to_nfc_binary(AttrValue),
            io:format(F, "~s~s=\"~s\"\n", [ ?INDENTSPC(Indent), AttrName, Str_1]);
        {tag_attr_array, AttrName, {WF, List}, _} ->
            io:format(F, "~s~s=\"\n", [ ?INDENTSPC(Indent), AttrName]),
            all(WF,F,List),
            io:put_chars(F, "\"\n")
    end;
to_io(x3dj, F, Indent, Data) ->
    case Data of
        {only_x3d, List} ->
            [Fn(Indent) || Fn <- List];
        {only_wrl, _} ->
            ok;
        {start_tag, OntoAttr, Tag} ->
            io:format(F, "~s\"-~s\": {\"~s\": {\n",[ ?INDENTSPC(Indent), OntoAttr, Tag]);
        {start_tag, Tag} ->
            io:format(F, "~s{\"~s\": {\n",[ ?INDENTSPC(Indent), Tag]);
        {array_item_vec3, {V1,V2,V3}} ->
            io:format(F, "~s~p,~p,~p",[ ?INDENTSPC(Indent), V1,V2,V3]);
        {array_item_vec2, {V1,V2}} ->
            io:format(F, "~s~p,~p",[ ?INDENTSPC(Indent), V1,V2]);
        {raw, Str} ->
            io:format(F, "~s",[Str]);
        {attr_use, OntoAttr, Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s\"-~s\":{\"~s\": {\"@USE\":\"~s\"}}\n",[ ?INDENTSPC(Indent), OntoAttr, Tag, Id_1]);
        {start_tag_defined, OntoAttr, Tag, Id} ->
            Id_1 = unicode:characters_to_nfc_binary(Id),
            io:format(F, "~s\"-~s\":{\"~s\": {\"@DEF\":\"~s\", \n",[ ?INDENTSPC(Indent), OntoAttr, Tag, Id_1]);
        {close_tag, _Tag, AddComma} ->
            io:format(F, "~s} }~s\n", [ ?INDENTSPC(Indent),cma(AddComma) ]);
        {close_tag_no_content, _Tag, AddComma} ->
            io:format(F, "~s} }~s\n", [ ?INDENTSPC(Indent),cma(AddComma) ]);
        {after_attrs} ->
            ok;
        {tag_attr_bool, AttrName, AttrValue, AddComma} ->
            AttrVal_1 = case AttrValue of
                true -> "1"
            end,
            io:format(F, "~s\"@~s\": ~s~s\n", [
                ?INDENTSPC(Indent), AttrName, AttrVal_1, cma(AddComma)]);
        {tag_attr_vec3, AttrName, {V1,V2,V3}, AddComma} ->
            io:format(F, "~s\"@~s\": [~p,~p,~p]~s\n", [
                ?INDENTSPC(Indent), AttrName, V1,V2,V3, cma(AddComma)]);
        {tag_attr_float, AttrName, AttrValue, AddComma} ->
            io:format(F, "~s\"@~s\": ~p~s\n", [
                ?INDENTSPC(Indent), AttrName, AttrValue, cma(AddComma)]);
        {tag_attr_string, AttrName, AttrValue, AddComma} ->
            AttrValue_1 = binary_to_list(unicode:characters_to_nfc_binary(AttrValue)),
            io:format(F, "~s\"@~s\": ~p~s\n", [
                ?INDENTSPC(Indent), AttrName, AttrValue_1, cma(AddComma)]);
        {tag_attr_array, AttrName, {WF, List}, AddComma} ->
            io:format(F, "~s\"@~s\":[\n", [ ?INDENTSPC(Indent), AttrName]),
            all(WF,F,List),
            io:format(F, " ]~s\n", [cma(AddComma)])
    end.


cma(true) ->
    ",";
cma(false) ->
    "".


