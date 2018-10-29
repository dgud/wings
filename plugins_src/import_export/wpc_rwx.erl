%%
%%  wpc_rwx.erl --
%%
%%     Criterion Renderware import/export.
%%
%%  Copyright (c) 2003-2011 Geoffrey Bantle and Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_rwx).

-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{export,{rwx,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{rwx,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Renderware (.rwx)...",rwx,[option]}].

props() ->
    [{ext,".rwx"},{ext_desc,?__(1,"Criterion Renderware File")}].


%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"Renderware export options"),
	       dialog(export),
	       fun(Res) ->
		       {file,{Op,{rwx,Res}}}
	       end);

do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Ps = [{subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)).


export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Filename, Contents0, Attr) ->
    Contents1 = export_transform(Contents0, Attr),
    Contents = wpa:save_images(Contents1, filename:dirname(Filename), ".jpg"),
    case rwx_export(Filename, Contents, Attr) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

dialog(export) ->
    [{?__(1,"Faceted lightsampling"),get_pref(faceted_light_sampling,false),[{key,faceted_light_sampling}]},
     %%{"Write Textures to Jpeg files?",get_pref(write_jpeg,false),[{key,write_jpeg}]},
     {?__(2,"Optimize file size?"),get_pref(opt_fs,false),[{key,opt_fs}]},	
     {label_column,
      [{?__(3,"Ambient"),{text,get_pref(ambient_value,0.58),[{key,ambient_value}]}},
       {?__(4,"Diffuse"),{text,get_pref(diffuse_value,0.15),[{key,diffuse_value}]}},
       {?__(5,"Specular"),{text,get_pref(specular_value,0.15),[{key,specular_value}]}}]},

     {label_column,
      [{?__(6,"Export scale"),{text,get_pref(export_scale, 0.1),[{key,export_scale}]}},
       {?__(7,"Sub-division Steps"),{text,get_pref(subdivisions, 0),
			      [{key,subdivisions},{range,0,4}]}}

      ]} ].
%%     {label_column,
%%      [{"Jpeg quality",{text,get_pref(jpeg_quality,65),[{key,jpeg_quality},{range,0,100}]}}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = e3d_mat:scale(proplists:get_value(export_scale, Attr, 1.0)),
    e3d_file:transform(Contents, Mat).

rwx_export(File,#e3d_file{objs=Objs,mat=Mat,creator=Creator}, Flags) ->

    %%put(rwx_path,filename:dirname(File)),
    %%put(write_jpeg, proplists:get_value(write_jpeg,Flags)),	
    %%put (jpeg_quality, proplists:get_value(jpeg_quality,Flags)),
    
    %%This is bad science right here... all of these values should really be coupled
    %%to individual material settings in wings material editor.
    %%This is on the 'todo' list for
    %%the 1.5 release (patching wings material editor...).

    Av = proplists:get_value(ambient_value,Flags),
    Dv = proplists:get_value(diffuse_value,Flags),
    Sv = proplists:get_value(specular_value,Flags),

    %%contains all the info we need for rwx surface information. the gb_tree returned by build_mat_tree/3
    %%contains texturemap, diffuse and opacity information associated with particular material names.
    RSurface = {{Av,Dv,Sv},build_mat_tree(Mat)},


    case file:open(File, [write]) of
	{error,_}=Error -> Error;
	{ok,F} ->
	    label(F,Creator),
	    io:format(F, "#See http://www.wings3d.com for details \r\n",[]),
	    io:format(F, "#Criterion Renderware export v1.0.\r\n",[]),
	    io:format(F, "modelbegin \r\n",[]), 
	    io:format(F, "clumpbegin \r\n",[]),
	    lists:foreach(fun(Object)->rwx_export_object(F,Object,Flags,RSurface) end,Objs),
	    io:format(F, "clumpend \r\n",[]),
	    io:format(F, "modelend \r\n",[]),
	    ok = file:close(F)

	    %% notice above that all calls to 'clumpbegin/clumpend' have to be nested within 
	    %% EXACTLY one 'toplevel' call to clumpbegin/clumpend
	    %% (that's why we write those tags
	    %%at the beginning and end of file as well as before and after each object.)
	    %%I don't know why this is, and the RWX file format specification says nothing on the
	    %%subject at all.
    end.

rwx_export_object(F,#e3d_object{name=Name,obj=Mesh},Flags,RSurface) ->
    io:format(F,"#Object name ~s\r\n",[Name]),
    io:format(F,"clumpbegin \r\n",[]),

    %%RWX uses the "Lightsampling" flag to decide whether or not to use shared vertex
    %%normals for shading objects, this is another thing that needs to go into 
    %%individual material settings in a later release...

    case proplists:get_value(faceted_light_sampling,Flags) of
	true->
	    io:format(F,"LightSampling Facet \r\n",[]);		
	false->
	    io:format(F,"LightSampling Vertex \r\n",[])
    end,

    %%Optimized file writing doesn't really shave off a whole lot size from the final
    %%file yet.
    Optimized_FS = proplists:get_value(opt_fs,Flags),

    %%write the actual mesh to a 'clump'. A clump in a RWX file is analagous to an 
    %%object in wings.
    write_clump(F,Mesh,RSurface,Optimized_FS),
    io:format(F,"clumpend \r\n",[]).	

write_clump(F,#e3d_mesh{vs=Vs,fs=Fs,tx=Tx1,vc=Vc},RSurface,OFS_Flag)->	
    %%UV's in Renderware are flipped with respect to the V axis.
    case length(Tx1) of 
	0 ->
	    Tx = Tx1;
	_ ->
	    Tx = flip_uvs(Tx1)
    end,


    %%Notes on what is to follow:
    %%Renderware files differ in how they store 3d meshes to wings files in one major
    %% respect: Wings stores uv's on a per face basis (when needed) and Renderware only
    %%allows uv's to be defined on a per vertex basis. (the same goes for vertex colors) 
    %%what this means is that before exporting a mesh to Renderware it must be split along
    %%the vertices where there exists more than one vertex ID/UV/Vcol pair or triple. 
    %%of Course this means faces need their vertex id's refreshed to point to the proper
    %%vertices.....

    %%one nasty side effect of this is that split vertices cause a bit of damage to lighting
    %%since they can no longer share vertex normals with other vertices that occupy the same
    %%space as them. Since RWX dosn't allow us to define vertex normals explicitly or 
    %%make some other concession for this problem (such as a special tag) there
    %%is no getting around this. 


    %%Step through each face in the model and return a pair of boolean values for each
    %%one indicating whether or not it has texture coordinates or Vertex colors.
    Unsorted_Faces = lists:foldl(fun(#e3d_face{vc=Vc0,tx=Tx0}=CurrFace,FsAccum)-> 
					 FsAccum ++ [{face_mode(Tx0,Vc0),CurrFace}] end,[],Fs),

    %%any face that has the tuple {false,false} associated with it is of course 
    %%a material only face and therefore has no 'tagged data' associated with its
    %%vertices.
    Mat_Faces = lists:filter(fun({{false,false},_})->true;
				({_,_})->false end,Unsorted_Faces),

    %%tagged faces are any faces with vertex color/texture coordinates associated with them..
    Tagged_Faces = Unsorted_Faces -- Mat_Faces,

    %%build a custom lookup list that gives us access to every unique vertex/texture/color id pair/triple. 
    Taglist = lists:usort(collect_tags(Tagged_Faces)),

    %%build a new set of vertex indices for the tagged verts
    TIndices = build_new_indices(Taglist,1),

    Matlist = lists:usort(lists:foldl(fun(#e3d_face{vs=Vs0},IDaccum)-> IDaccum ++ Vs0 end, [], Fs)),
    MIndices = build_new_indices(Matlist,(length(TIndices))+1),

    %%build gb trees for fast lookup of vertex and tag values according to ID. This is nessecary since the
    %%the id's for the exported mesh have to be scrambled, so random access is nessecary.
    Vert_Tree = build_vdat_tree(Vs),
    Tx_Tree = build_vdat_tree(Tx),
    Vc_Tree = build_vdat_tree(Vc),

    write_Verts(F,OFS_Flag,TIndices,{Vert_Tree,Tx_Tree,Vc_Tree}),	
    write_Verts(F,OFS_Flag,MIndices,Vert_Tree),

    %%create a gb_tree to store the tag/value pairs with their new index values so we reference the correct
    %%vertex when writing the mesh faces in the file.
    TID_Tree = build_ID_Tree(TIndices), 
    MID_Tree = build_ID_Tree(MIndices),	

    TFaces0 = build_new_faces(Tagged_Faces,TID_Tree),
    MFaces0 = build_new_faces(Mat_Faces,MID_Tree),
    RawFaces = lists:keysort(1,(TFaces0 ++ MFaces0)),

    %%get a list of all material names to use later on to make faces with same material be declared 
    %%chronologically in file and avoid redundant material definitions...
    MFace_keys = proplists:get_keys(RawFaces),

    %%hack, ugly, nasty icky hack.

    {RMat,SurfTree} = RSurface,  

    write_surface(F,RMat),
    write_rfaces(F,MFace_keys,RawFaces,SurfTree).


flip_uvs([{U,V}|T])->
    [{U,1.0 - V}|flip_uvs(T)];
flip_uvs([])->
    [].
build_vdat_tree(VDat)->
    {Vdat_Tree,_} = lists:foldl(fun(Vert,{Tree,Index})->
					{gb_trees:insert(Index,Vert,Tree),Index + 1} end,{gb_trees:empty(),1},VDat),
    Vdat_Tree.

face_mode(Tx,Vc)->
    case length(Tx) of
	0->
	    face_mode({false,Vc});
	_->
	    face_mode({true,Vc})
    end.

face_mode({Bool1,Vc})->
    case length(Vc) of
	0->
	    {Bool1,false};
	_->
	    {Bool1,true}
    end.	



collect_tags([{Bool,Face}|T])->
    Unsorted_keys = collect_tag_keys(Bool,Face),
    lists:flatten([Unsorted_keys|collect_tags(T)]);
collect_tags([])->
    [].

collect_tag_keys({true,false},#e3d_face{vs=Vs,tx=Tx})->
    TxPad = pad_tags(Tx,false),
    collect_tag_keys(Vs,TxPad);

collect_tag_keys({true,true},#e3d_face{vs=Vs,tx=Tx,vc=Vc})->

    collect_tag_keys(Vs,Tx,Vc);

collect_tag_keys({false,true},#e3d_face{vs=Vs,vc=Vc})->
    VcPad = pad_tags(false,Vc),
    collect_tag_keys(Vs,VcPad);
collect_tag_keys({false,false},#e3d_face{vs=Vs})->
    Vs;


collect_tag_keys([H|T],[H2|T2])->
    [{H,H2}|collect_tag_keys(T,T2)];
collect_tag_keys(_,[])->
    [].

collect_tag_keys([H|T],[H2|T2],[H3|T3])->
    [{H,H2,H3}|collect_tag_keys(T,T2,T3)];
collect_tag_keys(_,_,[])->
    [].


%%when a list of tags only has a vertex color or only has a uv index we need to 'pad' 
%%each value in the list with a 'false' atom so that we know what type of data we are 
%%exporting later on.
pad_tags(false,Taglist)->
    lists:map(fun(CurrID)-> {false,CurrID} end, Taglist);
pad_tags(Taglist,false)->
    lists:map(fun(CurrID)-> {CurrID,false} end, Taglist).

build_ID_Tree(Keylist)->
    Empty = gb_trees:empty(),
    lists:foldl(fun(CurrEntry,Tree)->
			{Key,Index} = CurrEntry,
			gb_trees:insert(Key,Index,Tree)
		end, Empty, Keylist).

build_new_indices([H|T],Index)->
    [{H,Index}|build_new_indices(T,Index+1)];
build_new_indices([],_Offset)->	
    [].

build_new_faces(FaceList,ID_Tree)->
    lists:foldl(fun({Bool,#e3d_face{mat=Mat} = CurrFace},FaceAccum)-> 
			%%io:format("Bool is ~w\r\n",[Bool]), 
			[Mname] = Mat,
			[{Mname,build_new_face(collect_tag_keys(Bool,CurrFace),ID_Tree)}] ++ FaceAccum end,[],FaceList).

build_new_face(KeyData,ID_Tree)->
    lists:foldl(fun(CurrID,IDs)-> IDs ++ [gb_trees:get(CurrID,ID_Tree)] end,[],KeyData).

write_Verts(F,OFS_Flag,[{{VID,{UVKey,false}},_Index}|T],{VT,TT,VCT})->
    {_, Vdata} = gb_trees:lookup(VID+1,VT),

    {_,UVdata} = gb_trees:lookup(UVKey+1,TT),
    write_Vert(F,OFS_Flag,Vdata,uv,UVdata),
    write_Verts(F,OFS_Flag,T,{VT,TT,VCT});

write_Verts(F,OFS_Flag,[{{VID,{false,VColKey}},_Index}|T],{VT,TT,VCT})->
						%get our actual vertex position data
    {_, Vdata} = gb_trees:lookup(VID+1,VT),

						%get the Vertex color data.... 
    {_,VColdata} = gb_trees:lookup(VColKey+1,VCT),


    write_Vert(F,OFS_Flag,Vdata,prelight,VColdata),
    write_Verts(F,OFS_Flag,T,{VT,TT,VCT});

write_Verts(F,OFS_Flag,[{VID,_Index}|T],VT)->
    {_, Vdata} = gb_trees:lookup(VID+1,VT),
    write_Vert(F,OFS_Flag,Vdata),
    write_Verts(F,OFS_Flag,T,VT);

write_Verts(_F,_OFS_Flag,[],_Trees)->
    [].


write_Vert(F,OFS_Flag,Vdata,Tag,Tag_data)->
    io:format(F,"Vertex ",[]), write_tuple(F,OFS_Flag,Vdata), write_tag(F,Tag), write_tuple(F,OFS_Flag,Tag_data), 
    io:format(F," \r\n", []).
write_Vert(F,OFS_Flag,Vdata)->
    io:format(F,"Vertex ",[]), write_tuple(F,OFS_Flag,Vdata), io:format(F,"\r\n",[]).


write_tag(F,uv)->
    io:format(F,"~s ",["UV"]);
write_tag(F,prelight)->
    io:format(F,"~s ",["#!PreLight"]).

build_mat_tree(Mat)->
    lists:foldl(fun({Name,CurrMat},MatTree)->
			GLMat = proplists:get_value(opengl,CurrMat),
			Maps = proplists:get_value(maps,CurrMat),
			{R,G,B,Opacity} = proplists:get_value(diffuse,GLMat),
			gb_trees:insert(Name,{R,G,B,Opacity,Maps},MatTree) end, gb_trees:empty(), Mat).

write_rfaces(F,[H|T],RawFaces,SurfTree)->
    CurrMaterial = gb_trees:get(H,SurfTree),
    FaceGroup = proplists:get_all_values(H,RawFaces),
    write_rMat(none,F,CurrMaterial),
    write_rfaces(F,FaceGroup),
    write_rfaces(F,T,RawFaces,SurfTree);
write_rfaces(_F,[],_RawFaces,_SurfTree)->
    [].

write_rfaces(F,RawFaces)->
    lists:foreach(fun(Currface)-> write_rface(F,Currface) end,RawFaces).

write_rface(F,Face)->
    Face_Type = poly_type(Face),
    case Face_Type of
	"Polygon"->
	    io:fwrite(F,"~s",[Face_Type]), io:fwrite(F," ~w ",[length(Face)]);
	_->
	    io:fwrite(F,"~s",[Face_Type])
    end,
    lists:foreach(fun(CurrVert)->io:fwrite(F," ~w",[CurrVert]) end,Face),
    io:fwrite(F,"\r\n",[]).

poly_type(Face)->
    case length(Face) of
	3->"Triangle";
	4->"Quad";
	_->"Polygon"
    end.

write_tuple(F,false,Item)->
    lists:foreach(fun(CurrElem)->io:fwrite(F,"~f ",[CurrElem]) end,tuple_to_list(Item));
write_tuple(F,true,Item)->
    lists:foreach(fun(CurrElem)->io:fwrite(F,"~.4f ",[CurrElem]) end,tuple_to_list(Item)).
write_rMat(Mode,F,{R,G,B,Opacity,[]})->
    write_rMat(Mode,F,{R,G,B,Opacity,[{diffuse,none}]});
write_rMat(_Mode,F,{R,G,B,Opacity,[{diffuse,Map}|_]})->
    io:format(F,"Color ~w ~w ~w\r\n", [R,G,B]),
    io:format(F,"Opacity ~w\r\n", [Opacity]),
    case Map of 
	none ->
	    io:put_chars(F, "Texture Null\r\n");
	#e3d_image{filename=Name0} ->
	    Name = filename:rootname(filename:basename(Name0)),
	    io:format(F,"Texture ~s\r\n", [Name])
    end.

write_surface(F,{Av,Dv,Sv})->
    io:fwrite(F,"Surface ~f ~f ~f\r\n",[Av,Dv,Sv]).

label(F, Creator) ->
    io:format(F, "#This file was created with ~s\r\n", [Creator]).
