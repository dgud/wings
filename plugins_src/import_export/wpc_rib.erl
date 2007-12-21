%%
%%  wpc_rib.erl --
%%
%%     Renderman exporter.
%%
%%  Copyright (c) 2002 Bjorn Gustavsson, Danni Coy (KayosIII).
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$

-module(wpc_rib).
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,
		flat_length/1,append/1,append/2]).

init() ->
    %% Disabled.
    false.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu({file,render}, Menu0) ->
    Menu1 = case os:find_executable("rendrib") of
		false -> Menu0;
		_Path -> Menu0 ++ [{"BMRT 2.6",rendrib,[option]}]
	    end,
    Menu2 = case os:find_executable("air") of
		false -> Menu1;
		_Path2 -> Menu1 ++ [{"Air",air,[option]}]
	   end,
    Menu3 = case os:find_executable("entropy") of
		false -> Menu2;
		_Path3 -> Menu2 ++  [{"Entropy",entropy,[option]}]
	    end,
    Menu3;
menu(_, Menu) -> Menu.

command({file,{export,{rib,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{rib,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command({file,{render,{rendrib,Ask}}}, St) ->
    do_render(Ask, rendrib, St);
command({file,{render,{air, Ask}}}, St) ->
    do_render(Ask, air, St);
command({file, {render, {entropy, Ask}}}, St) ->
    do_render(Ask, entropy, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"RenderMan (.rib)...",rib,[option]}].

props() ->
    [{ext,".rib"},{ext_desc,"RenderMan File"}].

render_props() ->
    [{ext,".tif"},{ext_desc,"Tiff Bitmap"}].

dialog_qs(export)->
    MeshVar = {mesh_type,get_pref(mesh_type, poly)},
    [{vframe,
      [{key_alt,MeshVar,"Polygon Mesh (older renderer)",poly},
       {key_alt,MeshVar,"Subdivision Mesh (smoother)",subdiv}],
      [{title,"Mesh Type"}]},
     {"Triangulate Faces (needed for BMRT)",get_pref(triangulate, true),
      [{key,triangulate}]},
     {"Expand Faces (BMRT)",get_pref(expand_faces, true),
      [{key,expand_faces}]},
     {"Export UV Coordinates",get_pref(export_uv, true),[{key,export_uv}]}|
     common_dialog()].
dialog_qs(render, Engine)->
    DefVar = {render_type,get_pref(render_type, preview)},
    [{hframe,
      [{vframe,
	[{key_alt,DefVar,"Preview Window",preview},
	 {key_alt,DefVar,"File",file}],
	[{title,"Output"}]},
       {vframe,
	[{label_column,
	  [{"Width",{text,get_pref(width, 320),[{key,width}]}},
	   {"Height",{text,get_pref(height, 240),[{key,height}]}}]}],
	[{title,"Resolution"}]}]}|render_dialog(Engine)].

render_dialog(rendrib) ->
    MeshVar = {mesh_type,get_pref(mesh_type, poly)},
    [{vframe,
      [{key_alt,MeshVar,"Polygon Mesh (older renderer)",poly},
       {"Export UV Coordinates",get_pref(export_uv, false),[{key,export_uv}]},
       {key_alt,MeshVar,"Subdivision Mesh (smoother)",subdiv}],
      [{title,"Mesh Type"}]}| common_dialog()];
render_dialog(air) ->
    MeshVar = {mesh_type,get_pref(mesh_type, subdiv)},
    [{vframe,
      [{key_alt,MeshVar,"Polygon Mesh (older renderer)",poly},
       {key_alt,MeshVar,"Subdivision Mesh (smoother)",subdiv}],
      [{title,"Mesh Type"}]},
     {"Export UV Coordinates",get_pref(export_uv, true),[{key,export_uv}]} | common_dialog()];
render_dialog(entropy) ->
    MeshVar = {mesh_type,get_pref(mesh_type, subdiv)},
    [{vframe,
      [{key_alt,MeshVar,"Polygon Mesh (older renderer)",poly},
       {key_alt,MeshVar,"Subdivision Mesh (smoother)",subdiv}],
      [{title,"Mesh Type"}]},
     {"Export UV Coordinates",get_pref(export_uv, true),[{key,export_uv}]} | common_dialog()].

common_dialog() ->
    [{"Export Normals",get_pref(export_normals, true),[{key,export_normals}]}].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

do_render(Ask, Engine, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "RIB Rendering Options", dialog_qs(render, Engine),
	       fun(Res) ->
		       {file,{render,{Engine,Res}}}
	       end);
do_render(Attr0, Engine, St) ->
    set_pref(Attr0),
    Attr1 = case proplists:get_value(render_type, Attr0) of
		file ->
		    RendFile = wpa:export_filename(render_props(), St),
		    [{render_file,RendFile}|Attr0];
		preview -> Attr0
	    end,
    Ls = wpa:lights(St),
    Attr2 = [{lights,Ls},{tmp_render,Engine}|Attr1],
    Attr = add_attr(Engine, Attr2),
    wpa:export(none, render_fun(Attr), St).

add_attr(rendrib, Attr) ->
    case proplists:get_value(mesh_type, Attr) of
	poly ->
	    TriFs = true, ExpandFs = true;
	_ ->
	    TriFs = false, ExpandFs = false
    end,
    [{triangulate,TriFs},{expand_faces,ExpandFs}|Attr];
add_attr(air, Attr) ->
    [{triangulate,false},{expand_faces,false}|Attr];
add_attr(entropy, Attr) ->
    [{triangulate,false},{expand_faces,false}|Attr].

render_fun(Attr) ->
    fun(Filename, Contents) ->
	    case render(Filename, Contents, Attr) of
		ok -> ok;
		{error,Error} -> {error,Error}
	    end
    end.

render(none, Contents, Attr) ->
    TmpName = "wpc_rib_temp" ++ random_string() ++ ".rib",
    TxList = export_1(TmpName, Contents, Attr),
    Width = proplists:get_value(width, Attr),
    Height = proplists:get_value(height, Attr),
    Renderer0 = proplists:get_value(tmp_render, Attr),
    Options1 = case Renderer0 of
		   rendrib ->
		       " -silent -res " ++ integer_to_list(Width) ++ " " ++
			   integer_to_list(Height) ++ " -d 16 ";
		   air ->
		       " ";
		   entropy ->
		       " -silent -res " ++ integer_to_list(Width) ++ " " ++
			   integer_to_list(Height) ++ " -d 16 "
	       end,
    Options2 = case Renderer0 of
		   rendrib ->
		       " -silent -res " ++ integer_to_list(Width) ++ " " ++
			   integer_to_list(Height) ++ " ";
		   air ->
		       " ";
		   entropy ->
		       " -silent -res " ++ integer_to_list(Width) ++ " " ++
			   integer_to_list(Height) ++ " "
	       end,
    Renderer = atom_to_list(Renderer0),
    F = fun() ->
		case proplists:get_value(render_file, Attr) of
		    undefined ->
			os:cmd(Renderer ++ Options1 ++ TmpName);
		    RendFile ->
			os:cmd(Renderer ++ Options2 ++ TmpName),
			case os:find_executable("iv") of
			    false -> true;
			    _Path ->  os:cmd("iv " ++ RendFile)
			end
		end,
		ok = file:delete(TmpName),
		foreach(fun(TmpImg) ->
				ok = file:delete(TmpImg)
			end, TxList)
	end,
    spawn(F),
    ok.

random_string() ->
    {A,B,C} = now(),
    foldl(fun(I, Acc) ->
		  integer_to_list(I) ++ [$_|Acc]
	  end, [$_|os:getpid()], [A,B,C]).
    
			  
    

%%%
%%% Export functions.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "RIB Export Options", dialog_qs(export),
	       fun(Res) ->
		       {file,{Op,{rib,Res}}}
	       end);
do_export(Attr0, _Op, Exporter, St) when is_list(Attr0) ->
    set_pref(Attr0),
    Ls = wpa:lights(St),
    Attr = [{lights,Ls},{tmp_render,none}|Attr0],
    Exporter(props(), export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export_1(Filename, Contents, Attr)
    end.

export_1(Name, #e3d_file{objs=Objs,mat=Mat,creator=Creator}, Attr) ->
    {ok,F} = file:open(Name, [write]),
    Base = filename:basename(filename:rootname(Name, ".rib")),
    io:format(F, "# Exported from ~s\n", [Creator]),
    case proplists:get_value(render_file, Attr) of
    	undefined -> ok;
	RenderFile0 ->
	    RenderFile = filename:basename(RenderFile0),
	    io:format(F, "Display ~p \"file\" \"rgba\"\n", [RenderFile])
    end,
    export_camera(F),
    io:put_chars(F, "WorldBegin\n"),
    io:put_chars(F, "Identity\n"),
    export_lights(F, Attr),
    TmpImgs = export_materials_one(Mat, Base, Attr),
    foreach(fun(Obj) -> export_object(F, Obj, Mat, Base, Attr) end, Objs),
    io:put_chars(F, "WorldEnd\n"),
    ok = file:close(F),
    case proplists:get_value(tmp_render, Attr) of
  	none -> true;
  	_ -> TmpImgs
    end,
    ok.

export_object(F, #e3d_object{name=Name,obj=Mesh0}, Mat, Base, Attr) ->
    Mesh1 = case proplists:get_bool(triangulate, Attr) of
		true -> e3d_mesh:triangulate(Mesh0);
		false -> Mesh0
	    end,
    Mesh = e3d_mesh:vertex_normals(Mesh1),

    io:format(F, "# Object: ~s\n", [Name]),
    io:put_chars(F, "AttributeBegin\n"),
    #e3d_mesh{fs=Fs0} = Mesh,
    Fs1 = [{M,FaceRec} || #e3d_face{mat=M}=FaceRec <- Fs0],
    Fs = sofs:to_external(sofs:relation_to_family(sofs:relation(Fs1))),
    export_all(F, Fs, Mesh, Base, Mat, Attr),
    io:put_chars(F, "AttributeEnd\n").

export_all(F, [{[MatName],Faces}|T], OrigMesh, Base, Mat, Attr) ->
    write_shader(F, MatName, Mat, Base, Attr),
    Mesh = OrigMesh#e3d_mesh{fs=Faces},
    MeshType = proplists:get_value(mesh_type, Attr),
    export_mesh(F, MeshType, Mesh, Attr),
    export_all(F, T, OrigMesh, Base, Mat, Attr);
export_all(_F, [], _, _, _, _) -> ok.

export_mesh(F, _, Mesh, Attr) ->
    #e3d_mesh{fs=Fs,vs=Vs0,ns=Ns0,tx=Tx0,he=He} = e3d_mesh:renumber(Mesh),
    {FsV,FsN,FsUV} = separate_faces(Fs),
    Vs = list_to_tuple(Vs0),
    Ns = list_to_tuple(Ns0),
    Tx = list_to_tuple(Tx0),
    export_mesh(F, Fs, FsV, FsN, FsUV, Vs, Ns, Tx, He, Attr).
    
export_mesh(F, Fs, FsV, FsN, FsUV, Vs, Ns, Tx, He0, Attr) ->
    MeshType = proplists:get_value(mesh_type, Attr),
    ExpandFaces = proplists:get_bool(expand_faces, Attr),
    case MeshType of
    	subdiv ->
	    io:put_chars(F, "SubdivisionMesh \"catmull-clark\"\n");
    	poly ->
	    io:put_chars(F, "PointsPolygons\n")
    end,

    io:put_chars(F, "[ "),
    foreach(fun(#e3d_face{vs=FaceVs}) ->
		    io:format(F, " ~p", [length(FaceVs)])
	    end, Fs),
    io:put_chars(F, "]\n"),

    io:put_chars(F, "[ "),
    FsVI = case ExpandFaces of
	       true ->
		   NumVs0 = [length(FaceVs) || #e3d_face{vs=FaceVs} <- Fs],
		   NumVs = lists:sum(NumVs0),
		   seq(0, NumVs-1);
	       false -> FsV
	   end,
    foreach(fun(V) ->
		    io:format(F, "~p ", [V])
	    end, FsVI),
    io:put_chars(F, "]\n"),

    %% Attributes (i.e. creases)
    case MeshType of
	subdiv ->
	    He = create_loops(He0),
	    io:put_chars(F, "[\"interpolateboundary\""),
	    foreach(fun(_) -> io:put_chars(F, " \"crease\"") end, He),
	    io:put_chars(F, "] [0 0"),
	    foreach(fun(H) -> io:format(F, " ~p 1", [length(H)]) end, He),
	    io:put_chars(F, "]\n["),
	    foreach(fun(H) ->
			    foreach(fun(V) ->
					    io:format(F, " ~p", [V])
				    end, H)
		    end, He),
	    io:put_chars(F, "]\n["),
	    foreach(fun(_) -> io:put_chars(F, " 2") end, He),
	    io:put_chars(F, "]\n");
	poly -> ok
    end,

    %% Vertex coords
    io:put_chars(F, "\"P\"\n[\n"),
    case ExpandFaces  of
	true ->
	    foreach(fun(V) ->
			    {X,Y,Z} = element(V+1, Vs),
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, FsV);
	false ->
	    foreach(fun({X,Y,Z}) ->
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, tuple_to_list(Vs))
    end,
    io:put_chars(F, "]\n"),

    %% Normals
    case proplists:get_bool(export_normals, Attr) of
	true ->
	    case ExpandFaces of
		true ->
		    io:put_chars(F, "\"N\" \n[\n");
		false ->
		    io:put_chars(F, "\"facevarying float[3] N\" \n[\n")
	    end,
	    foreach(fun(N) ->
			    {X,Y,Z} = element(N+1, Ns),
			    io:format(F, "~p ~p ~p\n", [X,Y,Z])
		    end, FsN),
	    io:put_chars(F, "]\n");
	false -> ok
    end,

    %% UV coordinates
    case proplists:get_bool(export_uv, Attr) andalso FsUV =/= [] of
	true ->
	    case ExpandFaces of
		true ->
		    io:put_chars(F, "\"st\" \n[\n");
		false ->
		    io:put_chars(F, "\"facevarying float[2] st\" \n[\n")
	    end,
	    foreach(fun(UV) ->
			    {S0,T0} = element(UV+1, Tx),
			    %%wings measures textures from bottom left;
			    %% Renderman from top left - must invert T0
			    io:format(F, "~p ~p\n", [S0,1-T0])
		    end, FsUV),
	    io:put_chars(F, "]\n");
	false -> ok
    end.

export_camera(F) ->
    [{OX,OY,OZ},Dist,Az,El,{TrackX,TrackY},Fov] =
	wpa:camera_info([aim,distance_to_aim,azimuth,elevation,tracking,fov]),
    io:format(F, "Projection \"perspective\" \"fov\" ~p\n", [Fov]),
    io:format(F, "Scale ~p ~p ~p\n", [1,1,-1]),
    io:format(F, "Translate ~p ~p ~p\n", [TrackX,TrackY,-Dist]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [El,1,0,0]),
    io:format(F, "Rotate ~p ~p ~p ~p\n", [Az,0,1,0]),
    io:format(F, "Translate ~p ~p ~p\n", [OX,OY,OZ]).

export_materials_one(Mats, Base, Attr) ->
    export_materials_one(Mats, Base, Attr, []).

export_materials_one([{Name,Mat}|T], Base, Attr, Acc) ->
    case proplists:get_value(diffuse_map, Mat, none) of
	none ->
	    export_materials_one(T,Base, Attr, Acc);
	{W,H,DiffMap} ->
	    case proplists:get_value(tmp_render, Attr) of
		none ->
		    MapFile = Base ++ "_" ++ atom_to_list(Name) ++
			"_diffmap.tif",
		    Image = #e3d_image{image=DiffMap,width=W,height=H},
		    ok = e3d_image:save(Image, MapFile),
		    export_materials_one(T,Base, Attr, Acc);
		_ ->
		    MapFile = "wpc_tif_temp_" ++ atom_to_list(Name) ++
			os:getpid() ++ ".tif",
		    Image = #e3d_image{image=DiffMap,width=W,height=H},
		    ok = e3d_image:save(Image, MapFile),
		    export_materials_one(T,Base, Attr, [MapFile|Acc])
	    end
    end;
export_materials_one([], _Base, _Attr, Acc) -> Acc.

write_shader(F, Name, [{Name,Mat}|_], Base, Attr) ->
    export_material(F, Name, Mat, Base, Attr);
write_shader(F, Name, [_|T], Base, Attr) ->
    write_shader(F, Name, T, Base, Attr).

export_material(F, Name, Mat, Base, Attr) ->
    OpenGL = proplists:get_value(opengl, Mat),
    Maps = proplists:get_value(maps, Mat),
    {Dr,Dg,Db,Opacity} = proplists:get_value(diffuse, OpenGL),
    io:format(F, "Color ~p ~p ~p\n", [Dr,Dg,Db]),
    io:format(F, "Opacity ~p ~p ~p\n", [Opacity,Opacity,Opacity]),
    {Ar,Ag,Ab,_} = proplists:get_value(ambient, OpenGL),
    {Sr,Sg,Sb,_} = proplists:get_value(specular, OpenGL),
    Shine = proplists:get_value(shininess, OpenGL),
    Ka = (Ar+Ag+Ab)/3,
    Kd = (Dr+Dg+Db)/3,
    %%%Ks = (Sr+Sg+Sb)/3,
    case proplists:get_value(diffuse, Maps, none) of
	none ->
	    io:format(F, "Surface \"plastic\"\n"
		      " \"float Ka\" [~p]\n"
		      " \"float Kd\" [~p]\n"
		      " \"float Ks\" [~p]\n"
		      " \"float roughness\" [~p]\n"
		      " \"color specularcolor\" [~p ~p ~p]\n",
		      [Ka,Kd,Shine,0.1,Sr,Sg,Sb]);
	{_,_,_DiffMap} ->
	    MapFile  = case proplists:get_value(tmp_render, Attr) of
			   none -> Base ++ "_" ++ atom_to_list(Name) ++ "_diffmap.tif";
			   _ -> "wpc_tif_temp_" ++ atom_to_list(Name) ++ os:getpid() ++ ".tif"
		       end,
	    io:format(F, "Surface \"paintedplastic\"\n"
		      " \"float Ka\" [~p]\n"
		      " \"float Kd\" [~p]\n"
		      " \"float Ks\" [~p]\n"
		      " \"float roughness\" [~p]\n"
		      " \"color specularcolor\" [~p ~p ~p]\n"
		      " \"string texturename\" [~p]\n",
		      [Ka,Kd,Shine,0.1,Sr,Sg,Sb,MapFile])
    end.

export_lights(F, Attr) ->
    declare(F, "from", "point"),
    declare(F, "to", "point"),
    declare(F, "lightcolor", "color"),
    Ls = proplists:get_value(lights, Attr),
    foldl(fun(L, I) -> export_light(F, I, L), I+1 end, 0, Ls).
    
export_light(F, I, {_,Ps}) ->
    OpenGL = proplists:get_value(opengl, Ps, []),
    Type = proplists:get_value(type, OpenGL, point),
    export_light(F, Type, I, OpenGL).

export_light(F, point, I, OpenGL) ->
    io:format(F, "LightSource ~p ~p", ["pointlight",I]),
    export_light_common(F, OpenGL),
    io:nl(F);
export_light(F, infinite, I, OpenGL) ->
    To = proplists:get_value(aim_point, OpenGL, {0,0,1}),
    io:format(F, "LightSource ~p ~p", ["distantlight",I]),
    export_light_common(F, OpenGL),
    show_point(F, "to", To),
    io:nl(F);
export_light(F, spot, I, OpenGL) ->
    To = proplists:get_value(aim_point, OpenGL, {0,0,1}),
    Angle0 = proplists:get_value(cone_angle, OpenGL, 30),
    Angle = Angle0*math:pi()/180,
    io:format(F, "LightSource ~p ~p", ["spotlight",I]),
    export_light_common(F, OpenGL),
    show_point(F, "to", To),
    io:format(F, " ~p ~p ", ["coneangle",Angle]),
    io:nl(F);
export_light(F, ambient, I, OpenGL) ->
    io:format(F, "LightSource ~p ~p", ["ambientlight",I]),
    {R,G,B,_} = proplists:get_value(ambient, OpenGL, {0.0,0.0,0.0,1.0}),
    io:format(F, " ~p ~p ", ["intensity",1.0]),
    show_point(F, "lightcolor", {R,G,B}),
    io:nl(F);
export_light(_, Type, _, _) ->
    io:format("Ignoring unknown light type: ~p\n", [Type]).

export_light_common(F, OpenGL) ->
    From = proplists:get_value(position, OpenGL, {0,0,0}),
    {R,G,B,_} = proplists:get_value(diffuse, OpenGL, {1,1,1,1}),
    io:format(F, " ~p ~p ", ["intensity",1.0]),
    show_point(F, "lightcolor", {R,G,B}),
    show_point(F, "from", From).
    
show_point(F, Label, {X,Y,Z}) ->
    io:format(F, "~p [~p ~p ~p] ", [Label,X,Y,Z]).

declare(F, N, V) ->
    io:format(F, "Declare ~p ~p\n", [N,V]).

%%%
%%% Utilities.
%%%

separate_faces(L) -> separate_faces(L, [], [], []).

separate_faces([#e3d_face{vs=Vs,tx=Tx,ns=Ns}|T], VAcc, NAcc, UVAcc) ->
    separate_faces(T, [Vs|VAcc], [Ns|NAcc], [Tx|UVAcc]);
separate_faces([], VAcc, NAcc, UVAcc) ->
    {append(reverse(VAcc)),append(reverse(NAcc)),append(reverse(UVAcc))}.

create_loops([]) -> [];
create_loops(L) -> create_loops(L, []).

create_loops(Vl, Acc) ->
    if
	length(Vl) >0 ->
	    Vh = hd(Vl),
	    Vt = tl(Vl),
	    {V0,V1} = Vh,
	    {Acc0,Nl} = create_loops0(V1,V0,Vt, [V0,V1]),
	    Acc1 = lists:append(Acc, [Acc0]),
	    create_loops(Nl,Acc1);
	length(Vl) =< 0 ->
	    Acc
    end.

create_loops0( V,V0,Vl,Acc) ->
    {Nxt,Nl} = get_next(V, Vl, []),
    case Nxt of
	V0 ->
	    Acc0 = lists:append(Acc,[V0]),
	    {Acc0,Nl};
	Nxt when Nxt >= 0 ->
	    Acc0 = lists:append(Acc,[Nxt]),
	    create_loops0(Nxt,V0,Nl,Acc0);
	Nxt when Nxt < 0 ->
	    {Acc,Nl}
    end.

%% Find first edge with shared vertex,
%% return other vertex in that edge, remove edge from list.

get_next(X, [{X,Z}|T], Acc) ->
    Nl = lists:append(Acc,T),
    {Z,Nl};
get_next(X,[{Z,X}|T],Acc)->
    Nl = lists:append(Acc,T),
    {Z,Nl};
get_next(X, [H|T],Acc) ->
    Nl = lists:append(Acc,[H]),
    get_next(X, T, Nl);
get_next(_, [], Acc) -> {-1,Acc}.
