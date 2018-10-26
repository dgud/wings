%%
%%  wpc_x.erl --
%%
%%     DirectX export plugin
%%
%%  Copyright (c) 2004-2011 Sean Hinde, Danni Coy, Dan Gudmundsson, and Ed Kolis
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_x).

%% Thanks KayosIII (Danni Aaron Coy) who wrote the export of UV coordinates 
%%
-export([init/0, menu/2, command/2]).
-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

-import(lists, [foreach/2, foldl/3, map/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({file, export}, Menu) ->
    menu_entry(Menu);
menu({file, export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{export,{x,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{x,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{?__(1,"DirectX (.x)..."), x, [option]}].

props() ->
    [{ext, ".x"},{ext_desc, ?__(1,"DirectX File")}].

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"DirectX Export Options"), dialog(export),
	       fun(Res) ->
		       {file,{Op,{x,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Tesselation = proplists:get_value(tesselation, Attr, none),
    ExportUV = proplists:get_value(include_uvs, Attr, true),
    ExportVC = proplists:get_value(include_colors, Attr, true),
    Ps = [{tesselation,Tesselation},{subdivisions,SubDivs},
	  {include_uvs,ExportUV},{include_colors,ExportVC}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
	    export(Filename, Contents, Attr)
    end.
set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

dialog(export) ->
    wpa:pref_set_default(?MODULE, default_filetype, ".png"),
    [wpa:dialog_template(?MODULE, tesselation),
     panel,
     wpa:dialog_template(?MODULE, export)].

export(File_name, Export0, Attr) ->
    %%io:format("~p~n~p~n",[Objs, Mat]),
    Dir = filename:dirname(File_name),
    Filetype = proplists:get_value(default_filetype, Attr, ".png"),
    ExportN  = proplists:get_value(include_normals, Attr, true),
    Export1 = wpa:save_images(Export0, Dir, Filetype),
    Export = export_transform(Export1, Attr),
    #e3d_file{objs=Objs,mat=Mat,creator=Creator} = Export,
    {ok,F} = file:open(File_name, [write]),
    io:format(F, "xof 0303txt 0064\r\n", []), % a standard directx header
    io:format(F, "#Exported from ~s\r\n",[Creator]),
    try
    	% export materials (need to come before objects)
        foreach(fun({Name, M}) -> def_material(F, Name, M, Dir) end, Mat),

    	% export objects
    	foldl(fun(#e3d_object{name = Name, obj=Obj}, _) ->
		      io:format(F, "Frame ~s {\r\n",[clean_id(Name)]),
		      ObjMesh = e3d_mesh:vertex_normals(Obj),
		      export_object(F, ObjMesh, Mat, ExportN),
		      io:put_chars(F, "}"),
		      ok
	      end, [], Objs)
    catch _:Err ->
	    io:format(?__(1,"DirectX Error: ~P in ~p~n"), [Err,30, erlang:get_stacktrace()])
    end,
    ok = file:close(F).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

export_object(F, #e3d_mesh{fs=Fs0,ns=NTab,vs=VTab,tx=UVTab}, 
	      Mat_defs, ExportN) ->
    io:format(F, "\tMesh {\r\n",[]),
    Fs = reorder(Fs0),
    [#e3d_face{}|_] = Fs,
    %% Duplicate stuff, once for each face each vertex/normal/UV is on
    Vdup = duplicate(Fs, VTab, vs),
    Ndup = duplicate(Fs, NTab, ns),
    UVdup0 = duplicate(Fs, UVTab, uvs),
    %% V coordinates need to be inverted for DirectX
    UVdup = map(fun({U, V}) -> {U, 1.0 - V} end, UVdup0),
    %Used_mats = material(F, Material, Mat_defs, Used_mats0),
    io:format(F, "\t\t~p;\r\n", [length(Vdup)]), % vertex count
    
    %% Helpers
    W3 = fun({X,Y,Z}, StartNum) ->  io:format(F, "\t\t~p, ~p, ~p;", [X, Y, Z]), StartNum + 1 end,
    W2 = fun({U,V}, StartNum) ->    io:format(F, "\t\t~p, ~p;", [U, V]), StartNum + 1 end,
    %% Write vertex coordinates
    all(W3,F,Vdup,"\r\n",";", 0),
    io:format(F, "\r\n",[]),
    io:format(F, "\t\t~p;\r\n", [length(Fs)]), % face count
    %% Write vertex indices
    all(fun(#e3d_face{vs=Vs}, LVI) -> print_face(F, Vs, LVI) end,F,Fs,"\r\n",";", 0),
    io:put_chars(F, "\r\n"),

    if ExportN ->
	    %% Write Normal vectors
	    io:format(F, "\t\tMeshNormals {\r\n\t\t\t~p;", [length(Ndup)]), %vertex count
	    all(W3,F,Ndup,"\r\n",";", 0),
	    %% Write per-vertex normal index
	    io:format(F, "\t\t~p;\r\n", [length(Fs)]), % face count
	    all(fun(#e3d_face{ns=Ns}, LNI) -> print_face(F, Ns, LNI) end,F,Fs,"\r\n",";", 0),
	    io:put_chars(F, "\t\t}\r\n");
       true  -> ignore
    end,

    if 
	%% Vertex colors not supported - use UV's
	UVTab /= [] ->				% Use UV-coords
	    io:format(F, "\t\tMeshTextureCoords {\r\n",[]),
	    io:format(F, "\t\t~p;\r\n", [length(UVdup)]), % UV coord count
	    all(W2,F,UVdup,"\r\n",";", 0),
	    io:format(F, "\t\t}\r\n",[]);
	true ->
	    ignore
    end,
    
    % Write materials
    io:format(F, "\t\tMeshMaterialList {\r\n", []),
    io:format(F, "\t\t\t~p;\r\n", [length(Mat_defs)]),
    io:format(F, "\t\t\t~p;\r\n", [length(Fs)]),
    foreach(fun(#e3d_face{mat=M}) -> io:format(F, "\t\t\t~p;\r\n", [index_of(M, Mat_defs, 0)]) end, Fs), 
    io:format(F, "\t\t\t;\r\n", []),
    foreach(fun({Name, _}) -> io:format(F, "\t\t\t{~s}\r\n", [clean_id(Name)]) end, Mat_defs),
    io:format(F, "\t\t}\r\n", []),

    %% Close Mesh
    io:put_chars(F, "\t}").

% Note: directx does not seem to support "ambient" color
def_material(F, Name, Mat0, Dir) ->
    Mat = lookup(opengl, Mat0),
    io:format(F, "\t\tMaterial ~s {\r\n",[clean_id(Name)]),
    {Dr, Dg, Db, Da} = lookup(diffuse, Mat),
    io:format(F, "\t\t\t~p; ~p; ~p; ~p;;\r\n",[Dr, Dg, Db, Da]),
    S = lookup(shininess, Mat),
    io:format(F, "\t\t~p;\r\n",[S]),
    {Sr, Sg, Sb, _} = lookup(specular, Mat),
    io:format(F, "\t\t\t~p; ~p; ~p;;\r\n",[Sr, Sg, Sb]),
    {Er, Eg, Eb, _} = lookup(emission, Mat),
    io:format(F, "\t\t\t~p; ~p; ~p;;\r\n",[Er, Eg, Eb]),
    
    case lists:keysearch(maps, 1, Mat0) of 
	{value, {maps,Maps}} -> 
	    case lists:keysearch(diffuse, 1, Maps) of
		{value, {diffuse,#e3d_image{filename=File0}}} ->
                    File = case string:prefix(File0, Dir) of
                               nomatch -> File0;
                               [_|Filename] -> Filename
                           end,
		    io:format(F, "\t\t\tTextureFilename { \"~s\"; }\r\n",
			      [File]);
		_ ->
		    ignore
            end;
	_ -> ignore
    end,
    io:put_chars(F, "\t\t}\r\n").
    
%% this is really simple since verts are duplicated
%% according to their appearance on the faces
print_face(F, Vs, StartNum) ->
    io:format(F, "\t\t~p;", [length(Vs)]),
    PrintVertex = fun(N, StartNum2) -> io:format(F, "~p", [N]), StartNum2 + 1 end,
    Indices = create_list(StartNum, length(Vs)),
    all(PrintVertex, F, Indices, " ", "", StartNum),
    io:put_chars(F, ";"),
    StartNum + length(Vs).

create_list(_Min, Len) when Len == 0 ->
    [];
    
create_list(Min, Len) ->
    [Min | create_list(Min + 1, Len - 1)].
    
duplicate(Fs, VTab, vs) ->
    Result = map(fun(#e3d_face{vs=FVs}) -> duplicate_face(FVs, VTab) end, Fs),
    lists:flatten(Result);

duplicate(Fs, NTab, ns) ->
    Result = map(fun(#e3d_face{ns=FNs}) -> duplicate_face(FNs, NTab) end, Fs),
    lists:flatten(Result);

duplicate(Fs, UVTab, uvs) ->
    Result = map(fun(#e3d_face{tx=FUVs}) -> duplicate_face(FUVs, UVTab) end, Fs),
    lists:flatten(Result).
    
duplicate_face(Pts, Tab) ->
    lists:flatten(duplicate_face(Pts, Tab, [])).

duplicate_face([Pt|T], Tab, ResultSoFar) ->
    NewResult = lists:nth(Pt + 1, Tab), 
    duplicate_face(T, Tab, [ResultSoFar, NewResult]);

duplicate_face([], _Tab, Result) ->
    Result.
    
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
all(F, IO, [H,H1|T], Separator, Ender, StartNum) when is_integer(StartNum) ->
    Result = F(H, StartNum),    
    io:format(IO, ",~s", [Separator]),
    all(F, IO, [H1|T], Separator, Ender, Result),
    Result;
all(F, IO, [H,H1|T], Separator, Ender, StartNum) ->
    Result = F(H, StartNum),    
    io:format(IO, ",~s", [Separator]),
    all(F, IO, [H1|T], Separator, Ender, Result),
    ok;
all(F, IO, [H], Separator, Ender, StartNum) when is_integer(StartNum) ->
    Result = F(H, StartNum),
    io:format(IO, "~s~s", [Ender, Separator]),
    Result;
all(F, IO, [H], Separator, Ender, StartNum) ->
    F(H, StartNum),
    io:format(IO, "~s~s", [Ender, Separator]),
    ok.

lookup(K, L) ->
    {value, {K, V}} =  lists:keysearch(K, 1, L),
    V.
    
index_of([Item], [{Key, _Value}|_T], StartCount) when Item == Key ->
    StartCount;

index_of(Item, [{_Key, _Value}|T], StartCount) ->
    index_of(Item, T, StartCount + 1);

index_of(_Item, [], _StartCount) ->
    notfound.

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
