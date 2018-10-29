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
-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

-import(lists, [foreach/2, foldl/3]).

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

command({file,{export,{wrl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{wrl,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) -> next.

menu_entry(Menu) ->
    Menu ++ [{"VRML 2.0 (.wrl)...", wrl, [option]}].

props() ->
    [{ext, ".wrl"},{ext_desc, ?__(1,"VRML 2.0 File")}].

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"VRML Export Options"), dialog(export),
	       fun(Res) ->
		       {file,{Op,{wrl,Res}}}
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

%% The intent is to create each object from
%% a sequence of Shapes. Each "sub Shape" will consist of all the
%% faces and vertices for one material..
export(File_name, Export0, Attr) ->
    %%io:format("~p~n~p~n",[Objs, Mat]),
    Filetype = proplists:get_value(default_filetype, Attr, ".png"),
    ExportN  = proplists:get_value(include_normals, Attr, true),
    Dir = filename:dirname(File_name),
    Export1 = wpa:save_images(Export0, Dir, Filetype),
    Export = export_transform(Export1, Attr),
    #e3d_file{objs=Objs,mat=Mat,creator=Creator} = Export,
    {ok,F} = file:open(File_name, [write]),
    io:format(F, "#VRML V2.0 utf8\n", []),
    io:format(F, "#Exported from ~s\n",[Creator]),
    try foldl(fun(#e3d_object{name = Name, obj=Obj}, Used_mats0) ->
		      io:format(F, "DEF ~s Transform {\n",[clean_id(Name)]),
		      io:format(F, "  children [\n",[]),
		      ObjMesh = e3d_mesh:vertex_normals(Obj),
		      Meshes = e3d_mesh:split_by_material(ObjMesh),
		      Used_mats = all(fun(Mesh,UsedMats) ->
					      export_object(F, Mesh, Mat,
							    ExportN,
							    UsedMats, Dir)
				      end, F, Used_mats0, Meshes),
		      io:put_chars(F, "\n  ]\n"),
		      io:put_chars(F, "}\n\n"),
		      Used_mats
	      end, [], Objs)
    catch _:Err ->
	    io:format(?__(1,"VRML Error: ~P in")++" ~p~n", [Err,30, erlang:get_stacktrace()])
    end,
    ok = file:close(F).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

export_object(F, #e3d_mesh{fs=Fs0,ns=NTab,vs=VTab,tx=UVTab,vc=ColTab}, 
	      Mat_defs, ExportN, Used_mats0, Dir) ->
    io:format(F, "    Shape {\n",[]),
    Fs = reorder(Fs0),
    [#e3d_face{mat=[Material|_]}|_] = Fs,
    Used_mats = material(F, Material, Mat_defs, Used_mats0, Dir),
    io:format(F, "      geometry IndexedFaceSet {\n",[]),

    if ExportN == true ->
	    io:format(F, "        normalPerVertex TRUE\n",[]);
       true -> ignore
    end,
    
    if
	(ColTab == []) -> ignore;
	true -> %% Use vertex colors
	    io:format(F, "        colorPerVertex TRUE\n",[])
    end,
    
    %% Helpers
    W3 = fun({X,Y,Z}) ->  io:format(F, "          ~p ~p ~p", [X,Y,Z]) end,
    W2 = fun({U,V}) ->    io:format(F, "          ~p ~p", [U,V]) end,
    %% Write vertex coordinates
    io:format(F, "        coord Coordinate { point [\n",[]),
    all(W3,F,VTab),
    io:format(F, " ] }\n",[]),
    %% Write vertex indecies
    io:put_chars(F, "        coordIndex [\n"),
    all(fun(#e3d_face{vs=Vs}) -> print_face(F, Vs) end,F,Fs),
    io:put_chars(F, " ]\n"),

    if ExportN ->
	    %% Write Normal vectors
	    io:format(F, "        normal Normal { vector [\n",[]),
	    all(W3,F,NTab),
	    io:format(F, " ] }\n",[]),
	    %% Write per-vertex normal index
	    io:put_chars(F, "        normalIndex [\n"),
	    all(fun(#e3d_face{ns=Ns}) -> print_face(F, Ns) end,F,Fs),
	    io:put_chars(F, " ]\n");
       true  -> ignore
    end,

    if 
	ColTab /= []->				% Use vertex colors
	    io:format(F, "        color Color { color [\n",[]),
	    all(W3,F,ColTab),
	    io:format(F, " ] }\n",[]),   
	    io:put_chars(F, "        colorIndex [\n"),
	    all(fun(#e3d_face{vc=Vc}) -> print_face(F, Vc) end,F,Fs),
	    io:put_chars(F, " ]\n");
	UVTab /= [] ->				% Use UV-coords
	    io:format(F, "        texCoord TextureCoordinate { point [\n",[]),
	    all(W2,F,UVTab),
	    io:format(F, " ] }\n",[]),   
	    io:put_chars(F, "        texCoordIndex [\n"),
	    all(fun(#e3d_face{tx=UV}) -> print_face(F, UV) end,F,Fs),
	    io:put_chars(F, " ]\n");
	true ->
	    ignore
    end,
    %% Close Shape and IndexedFaceSet
    io:put_chars(F, "      }\n    }"),
    Used_mats.

material(F, Name, Mat_defs, Used, Dir) ->
    case lists:member(Name, Used) of
        true ->
            use_material(F, Name),
            Used;
        false ->
            def_material(F, Name, lookup(Name, Mat_defs), Dir),
            [Name|Used]
    end.

% Note: vrml represents ambient colour as a proportion of 
% diffuse colour, not in its own right.
def_material(F, Name, Mat0, Dir) ->
    Mat = lookup(opengl, Mat0),
    io:format(F, "      appearance Appearance {\n",[]),
    io:format(F, "        material DEF ~s Material {\n",[clean_id(Name)]),
    {Ar, Ag, Ab, O} = lookup(ambient, Mat),
    {Dr, Dg, Db, _} = lookup(diffuse, Mat),
    io:format(F, "          diffuseColor ~p ~p ~p\n",[Dr, Dg, Db]),
    {Er, Eg, Eb, _} = lookup(emission, Mat),
    io:format(F, "          emissiveColor ~p ~p ~p\n", [Er, Eg, Eb]),
    {Sr, Sg, Sb, _} = lookup(specular, Mat),
    io:format(F, "          specularColor ~p ~p ~p\n",[Sr, Sg, Sb]),
    Amb = (Ar+Ag+Ab)/3,
    io:format(F, "          ambientIntensity ~p\n",[Amb]),
    io:format(F, "          transparency ~p\n",[1.0-O]),
    S = lookup(shininess, Mat),
    io:format(F, "          shininess ~p\n",[S]),
    io:put_chars(F, "        }\n"),
    case lists:keysearch(maps, 1, Mat0) of
        {value, {maps,Maps}} ->
            case lists:keysearch(diffuse, 1, Maps) of
                {value, {diffuse,#e3d_image{filename=FileName}}} ->
                    File = case string:prefix(FileName, Dir) of
                               nomatch -> FileName;
                               [_Delim|File0] -> File0
                           end,
                    io:format(F, "        texture ImageTexture { url ~p }\n",
                              [File]);
		_ ->
		    ignore
	    end;
	_ -> ignore
    end,
    io:format(F, "      }\n", []).
    
use_material(F, Mat) ->
    io:format(F, "      appearance Appearance {\n",[]),
    io:format(F, "        material USE ~s\n",[clean_id(Mat)]),
    io:format(F, "      }\n", []).

print_face(F, Vs) ->
    io:put_chars(F, "          "),
    foreach(fun(V) -> io:format(F, "~p, ", [V]) end, Vs),
    io:put_chars(F, "-1").

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

all(F, IO, AccIn, [H,H1|T]) ->
    Acc = F(H, AccIn),
    io:put_chars(IO, ",\n"),    
    all(F,IO,Acc,[H1|T]);
all(F, _, AccIn, [H]) ->
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
