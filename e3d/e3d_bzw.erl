%%
%%  e3d_bzw.erl --
%%
%%     Functions for reading and writing BZFlag World Files (.bzw).
%%
%%  Copyright (c) 2006-2011 Dave Rodgers  (aka: trepan)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_bzw.erl,v 1.44 2005/03/04 09:03:57 dgud Exp $
%%

-module(e3d_bzw).
-export([import/1,export/2,export/3]).

-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).

-record(ost,
	{v=[],					%Vertices.
	 vt=[],					%Texture vertices.
	 vn=[],					%Vertex normals.
	 f=[],					%Faces.
	 g=[],					%Groups.
	 mat=[],				%Current material.
	 matdef=[],				%Material definitions.
	 dir,					%Directory of .bzw file.
	 seen=gb_sets:empty()}).		%Unknown type seen.

import(Name) ->
    case read_open(Name) of
	{ok,Fd} ->
	    Dir = filename:dirname(Name),
	    try import_1(Fd, Dir) of
		#e3d_file{}=E3dFile ->
		    {ok,E3dFile#e3d_file{dir=Dir}}
	    catch
		throw:Error -> Error
	    after
		close(Fd)
	    end;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Fd, Dir) ->
    Ost0 = read(fun parse/2, Fd, #ost{dir=Dir}),
    Ost = remember_eof(Ost0),
    #ost{v=Vtab0,vt=TxTab0,f=Ftab0,g=Gs0,vn=VnTab0,matdef=Mat} = Ost,
    Vtab = reverse(Vtab0),
    TxTab = reverse(TxTab0),
    VnTab = reverse(VnTab0),
    Ftab = make_ftab(Ftab0, []),
    Gs1 = reverse(Gs0),
    Gs = separate(Gs1, []),
    Template = #e3d_mesh{type=polygon,vs=Vtab,tx=TxTab,ns=VnTab},
    Objs = make_objects(Gs, Ftab, Template),
    #e3d_file{objs=Objs,mat=Mat}.

separate([{eof,N}], []) -> [{undefined,N}];
separate([{eof,_}], [{_,_,E}|_]=Acc) ->
    separate_1(Acc, E, []);
separate([{group,[Name|_],N}|T], Acc) ->
    separate(T, [{Name,N,get_face_num(T)}|Acc]);
separate([{name,Name,Start}|T0], Acc) ->
    {T,End} = skip_upto_name(T0),
    separate(T, [{Name,Start,End}|Acc]).

separate_1([{Name,S,E}|T], E, Acc) ->
    separate_1(T, S, [{Name,E-S}|Acc]);
separate_1([], _, Acc) -> Acc.
    
get_face_num([{eof,N}|_]) -> N;
get_face_num([{group,_,N}|_]) -> N.

skip_upto_name([{eof,N}]=T) -> {T,N};
skip_upto_name([{name,_,N}|_]=T) -> {T,N};
skip_upto_name([_|T]) -> skip_upto_name(T).

make_objects([{Name,N}|T], Fs0, Template) ->
    {Ftab,Fs} = split(Fs0, N, []),
    Mesh = e3d_mesh:renumber(Template#e3d_mesh{fs=Ftab}),
    Obj = #e3d_object{name=Name,obj=Mesh},
    [Obj|make_objects(T, Fs, Template)];
make_objects([], [], _) -> [].

split(Fs, 0, Acc) -> {reverse(Acc),Fs};
split([F|Fs], N, Acc) -> split(Fs, N-1, [F|Acc]).
    
make_ftab([{Mat,Vs0}|Fs], Acc) ->
    Vs = [V || {V,_,_} <- Vs0],
    Tx = case [Vt || {_,Vt,_} <- Vs0] of
	     [none|_] -> [];
	     Tx0 -> Tx0
	 end,
    Ns = case [Vn || {_,_,Vn} <- Vs0] of
	     [none|_] -> [];
	     Ns0 -> Ns0
	 end,
    make_ftab(Fs, [#e3d_face{mat=Mat,vs=Vs,tx=Tx,ns=Ns}|Acc]);
make_ftab([], Acc) -> Acc.

read(Parse, Fd0, Acc) ->
    {Line,Fd} = get_line(Fd0),
    read_1(Parse, Line, Fd, Acc).

read_1(_, eof, _, Acc) -> Acc;
read_1(Parse, [], Fd, Acc) ->
    %% Blank line - ignore and read the next line.
    read(Parse, Fd, Acc);
read_1(Parse, "#" ++ _Comment, Fd, Acc) ->
    %% Comment - ignore and read the next line.
    read(Parse, Fd, Acc);
read_1(Parse, [Ctrl|Line], Fd, Acc) when Ctrl =< $\s ->
    %% Ignore any leading whitespace (especially TAB and spaces,
    %% but also control characters such as ^@ which ZBrush 2
    %% emits at the end of a file).
    read_1(Parse, Line, Fd, Acc);
read_1(Parse, "mtllib" ++ Name0, Fd, Acc0) ->
    Name = skip_blanks(Name0),
    case Parse(["mtllib",Name], Acc0) of
	eof -> Acc0;
	Acc -> read(Parse, Fd, Acc)
    end;
read_1(Parse, Line, Fd, Acc0) ->
    Tokens = collect(Line, [], []),
    case Parse(Tokens, Acc0) of
	eof -> Acc0;
	Acc -> read(Parse, Fd, Acc)
    end.

collect([$\s|T], [], Tokens) ->
    collect(T, [], Tokens);
collect([$\s|T], Curr, Tokens) ->
    collect(T, [], [reverse(Curr)|Tokens]);
collect([H|T], Curr, Tokens) ->
    collect(T, [H|Curr], Tokens);
collect([], [], Tokens) ->
    reverse(Tokens);
collect([], Curr, Tokens) ->
    collect([], [], [reverse(Curr)|Tokens]).

parse(["  vertex",X0,Y0,Z0|_], #ost{v=Vtab}=Ost) ->
    X = str2float(X0),
    Y = str2float(Y0),
    Z = str2float(Z0),
    Ost#ost{v=[{X,Y,Z}|Vtab]};
parse(["  texcoord",U0,V0|_], #ost{vt=Vt}=Ost) ->
    U = str2float(U0),
    V = str2float(V0),
    Ost#ost{vt=[{U,V}|Vt]};
parse(["  normal",X0,Y0,Z0|_], #ost{vn=Vn}=Ost) ->
    X = str2float(X0),
    Y = str2float(Y0),
    Z = str2float(Z0),
    Ost#ost{vn=[{X,Y,Z}|Vn]};
parse(["face"|Vlist0], #ost{f=Ftab,mat=Mat}=Ost) ->
    Vlist = collect_vs(Vlist0, Ost),
    Ost#ost{f=[{Mat,Vlist}|Ftab]};
parse(["name"], Ost) ->Ost;
parse(["name"|Names], Ost) ->
    remember_group(Names, Ost);
parse(["o"], Ost) -> Ost;
parse(["o",Name|_], Ost) ->
    remember_name(Name, Ost);
parse(["usemtl"|[Mat|_]], Ost) ->
    Ost#ost{mat=[list_to_atom(Mat)]};
parse(["mtllib",FileName], #ost{dir=Dir}=Ost) ->
    Mat = read_matlib(FileName, Dir),
    Ost#ost{matdef=Mat};
parse(["End","Of","File"], _Ost) ->
    %% In files written by ZBrush 1.x.
    eof;
parse([Tag|_]=Other, #ost{seen=Seen}=Ost) ->
    case gb_sets:is_member(Tag, Seen) of
	true -> Ost;
	false ->
	    io:format("Ignoring: ~p\n", [Other]),
	    Ost#ost{seen=gb_sets:insert(Tag, Seen)}
    end.

remember_eof(#ost{f=Ftab,g=Gs}=Ost) ->
    Ost#ost{g=[{eof,length(Ftab)}|Gs]}.

remember_group(Names, #ost{g=[{group,Names,_}|_]}=Ost) -> Ost;
remember_group(Names, #ost{f=Ftab,g=Gs}=Ost) ->
    Ost#ost{g=[{group,Names,length(Ftab)}|Gs]}.

remember_name(Name, #ost{f=Ftab,g=Gs}=Ost) ->
    Ost#ost{g=[{name,Name,length(Ftab)}|Gs]}.
    
collect_vs([V|Vs], Ost) ->
    [collect_vtxref(V, Ost)|collect_vs(Vs, Ost)];
collect_vs([], _Ost) -> [].

collect_vtxref(S, Ost) ->
    case collect_vtxref_1(S, []) of
	[V] -> collect_vtxref_2(V, none, none, Ost);
	[V,Vt] -> collect_vtxref_2(V, Vt, none, Ost);
	[V,Vt,Vn|_] -> collect_vtxref_2(V, Vt, Vn, Ost)
    end.

collect_vtxref_1([], Acc) -> reverse(Acc);
collect_vtxref_1(S0, Acc) ->
    {Ref,S} = collect_one_vtxref(S0),
    collect_vtxref_1(S, [Ref|Acc]).

collect_vtxref_2(V0, Vt0, Vn0, #ost{v=Vtab,vt=VtTab,vn=VnTab}) ->
    V = resolve_vtxref(V0, Vtab),
    Vt = resolve_vtxref(Vt0, VtTab),
    Vn = resolve_vtxref(Vn0, VnTab),
    {V,Vt,Vn}.

resolve_vtxref(none, _) -> none;
resolve_vtxref(V, _) when V > 0 -> V-1;
resolve_vtxref(V0, Tab) when V0 < 0 ->
    case length(Tab)+V0 of
	V when V >= 0 -> V
    end.

collect_one_vtxref(S) ->
    collect_one_vtxref(S, []).

collect_one_vtxref([$/|S], Acc) ->
    collect_one_vtxref_done(S, Acc);
collect_one_vtxref([H|T], Acc) ->
    collect_one_vtxref(T, [H|Acc]);
collect_one_vtxref([], Acc) ->
    collect_one_vtxref_done([], Acc).

collect_one_vtxref_done(S, []) -> {none,S};
collect_one_vtxref_done(S, V0) -> {list_to_integer(reverse(V0)),S}.

read_matlib(Name, Dir) ->
    case try_matlib(filename:join(Dir, Name)) of
	error ->
	    case try_matlib(filename:join(Dir, Name)) of
		error -> [];
		Other -> Other
	    end;
	Res -> Res
    end.

try_matlib(Name) ->
    case read_open(Name) of
	{ok,Fd} ->
	    Res = read(fun mtl_parse/2, Fd, []),
	    close(Fd),
	    [{Mat,[{maps,Maps},{opengl,fixup_mat(OpenGL)}]} ||
		{Mat,OpenGL,Maps} <- Res];
	{error,_Reason} -> error
    end.

%% Combine diffuse color with opacity.
fixup_mat(OpenGL0) ->
    Opacity = proplists:get_value(opacity, OpenGL0, 1.0),
    OpenGL1 = lists:keydelete(opacity, 1, OpenGL0),
    {R,G,B} = proplists:get_value(diffuse, OpenGL1, {1.0,1.0,1.0}),
    OpenGL = lists:keydelete(diffuse, 1, OpenGL0),
    [{diffuse,{R,G,B,Opacity}}|OpenGL].

mtl_parse(["newmtl"|Name0], Ms) ->
    Name = list_to_atom(space_concat(Name0)),
    [{Name,[],[]}|Ms];
mtl_parse(["d",Opacity], Mtl) ->
    mtl_add({opacity,str2float(Opacity)}, Mtl);
mtl_parse(["Ka"|RGB], Mtl) ->
    mtl_add({ambient,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["Kd"|RGB], Mtl) ->
    mtl_add({diffuse,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["Ks"|RGB], Mtl) ->
    mtl_add({specular,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["Ke"|RGB], Mtl) ->
    mtl_add({emission,mtl_text_to_tuple(RGB)}, Mtl);
mtl_parse(["texture"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({diffuse,Filename}, Mtl);
mtl_parse(["map_Ka"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({ambient,Filename}, Mtl);
mtl_parse(["map_Bump"|Filename0], Mtl) ->
    Filename = space_concat(Filename0),
    map_add({bump,Filename}, Mtl);
mtl_parse([_|_], [{_,_,_}|_]=Mtl) -> Mtl.

mtl_add(P, [{Name,OpenGL,Maps}|Ms]) ->
    [{Name,[P|OpenGL],Maps}|Ms].

map_add(P, [{Name,OpenGL,Maps}|Ms]) ->
    [{Name,OpenGL,[P|Maps]}|Ms].

mtl_text_to_tuple(L) ->
    list_to_tuple([str2float(F) || F <- L]).

str2float("."++_=S) -> str2float_1("0"++S);
str2float([$-|"."++_=S]) -> str2float_1("-0"++S);
str2float(S) -> str2float_1(S).

str2float_1(S) ->
    try
	list_to_float(S)
    catch
	error:badarg ->
	    str2float_2(S, [])
    end.

str2float_2([H|T], Acc) when H == $e; H == $E ->
    foreach(fun($-) -> ok;
	       ($+) -> ok;
	       (D) when $0 =< D, D =< $9 -> ok
	    end, Acc),
    NumStr = reverse(Acc, ".0e") ++ T,
    list_to_float(NumStr);
str2float_2([H|T], Acc) ->
    str2float_2(T, [H|Acc]);
str2float_2([], Acc) ->
    float(list_to_integer(reverse(Acc))).

space_concat([Str|[_|_]=T]) ->
    Str ++ [$\s|space_concat(T)];
space_concat([S]) -> S;
space_concat([]) -> [].

skip_blanks([$\s|T]) -> skip_blanks(T);
skip_blanks([$\t|T]) -> skip_blanks(T);
skip_blanks(S) -> S.

read_open(Name) ->
    case file:open(Name, [read,raw,read_ahead]) of
	{ok,Fd} -> {ok,{Fd,[]}};
	{error,_}=Error -> Error
    end.

close({Fd,_}) ->
    file:close(Fd).
	    
get_line({Fd,Buf}) ->
    get_line(Buf, Fd, []).

get_line([], Fd, Line) ->
    case file:read(Fd, 128) of
	eof ->
	    case Line of
		[] -> {eof,{Fd,[]}};
		_ -> {reverse(Line),{Fd,[]}}
	    end;
	{ok,Cs} -> get_line(Cs, Fd, Line)
    end;
get_line([$\r|Cs], Fd, Line) ->
    {reverse(Line),{Fd,Cs}};
get_line([$\n|Cs], Fd, Line) ->
    {reverse(Line),{Fd,Cs}};
get_line([C|Cs], Fd, Line) ->
    get_line(Cs, Fd, [C|Line]).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Export.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


export(File, Contents) ->
    export(File, Contents, []).


export(File, #e3d_file{objs=Objs,mat=Mat,creator=Creator}, Flags) ->
    case file:open(File, [write]) of
	{error,_}=Error -> Error;
	{ok,F} ->
            materials(F, File, Mat, Creator),
%	    foldl(fun(#e3d_object{name=Name}=Obj, {Vbase,UVbase,Nbase}) ->
%			  io:put_chars(F, "mesh\r\n"),
%			  io:format(F, "  name ~s\r\n", [Name]),
%			  export_object(F, Obj, Flags) end, {1,1,1}, Objs),
	    foreach(fun(#e3d_object{name=Name}=Obj) ->
			  io:put_chars(F, "mesh\r\n"),
			  io:format(F, "  name ~s\r\n", [Name]),
			  export_object(F, Obj, Flags) end, Objs),
	    ok = file:close(F)
    end.


export_object(F, #e3d_object{name=Name,obj=Mesh0}, Flags) ->
    Mesh = case proplists:get_bool(include_normals, Flags) of
	       false -> Mesh0;
	       true -> e3d_mesh:vertex_normals(Mesh0)
	   end,
    #e3d_mesh{fs=Fs0,vs=Vs,tx=Tx,ns=Ns} = Mesh,
    mesh_info(F, Mesh),
	bzw_inside_1(F,Mesh),
    foreach(fun({X,Y,Z}) ->
        io:format(F, "  vertex ~s ~s ~s\r\n", [fmtf(X), fmtf(Y), fmtf(Z)])
    end, Vs),
    foreach(fun({X,Y,Z}) ->
        io:format(F, "  normal ~s ~s ~s\r\n", [fmtf(X), fmtf(Y), fmtf(Z)])
    end, Ns),
    foreach(fun({U,V}) ->
        io:format(F, "  texcoord ~s ~s\r\n", [fmtf(U), fmtf(V)])
    end, Tx),
    %object_group(F, Name, Flags),
    object_group(Flags),
    Fs1 = [{Mat,FaceRec} || #e3d_face{mat=Mat}=FaceRec <- Fs0],
    Fs = sofs:to_external(sofs:relation_to_family(sofs:relation(Fs1))),
    foreach(fun(Face) ->
        face_mat(F, Name, Face, Flags)
    end, Fs),
    io:put_chars(F, "end\r\n").

bzw_inside_1(F,#e3d_mesh{vs=Vs}=Mesh) ->
    case wpa:pref_get(wpc_bzw, bzw_inside) of
      true ->
        #e3d_mesh{fs=Fs} = e3d_mesh:triangulate(Mesh),
        case volume(Fs,Vs,[]) < 1.0E-6 of
          true ->
            P = "  #No legal inside point, since volume of object is too small\r\n",
            io:put_chars(F, P);
          false ->
            {Cx,Cy,Cz} = e3d_vec:average(Vs),
            io:format(F, "  inside ~s ~s ~s\r\n", [fmtf(Cx), fmtf(Cy), fmtf(Cz)])
        end;
      false ->
        ok
    end.

volume([#e3d_face{vs=[V1,V2,V3]}|Ftab],Vs,Acc) ->
    Va = lists:nth(V1+1,Vs),
    Vb = lists:nth(V2+1,Vs),
    Vc = lists:nth(V3+1,Vs),
    Bc = e3d_vec:cross(Vb, Vc),
    volume(Ftab,Vs,[e3d_vec:dot(Va, Bc)/6.0|Acc]);

volume([],_,Acc) ->
    Volume = lists:sum(Acc),
    Volume.

%fmtf(F) when abs(F) < 1.0e-40 ->
%    "0";
%fmtf(F) when abs(F) < 0.1; abs(F) >= 10000 ->
%    lists:flatten(io_lib:format("~.4f", [F]));
fmtf(F) ->
    lists:flatten(io_lib:format("~.4f", [F])).

%object_group(F, Name, Flags) ->
object_group(Flags) ->
    case proplists:get_bool(group_per_material, Flags) of
	true -> ok;
	false -> ok
%	false -> io:format(F, "g ~s\r\n", [Name])
    end.

face_mat(F, Name, {Ms,Fs}, Flags) ->
    mat_group(F, Name, Ms, Flags),
    io:put_chars(F, "  matref"),
    foldl(fun(M, Prefix) ->
		  io:format(F, "~c~s", [Prefix,atom_to_list(M)])
	  end, $\s, Ms),
    eol(F),
    foreach(fun(Vs) -> face(F, Vs) end, Fs).

mat_group(F, Name, Ms, Flags) ->
    case proplists:get_bool(group_per_material, Flags) of
	true ->
	    io:format(F, "  #matref ~s # group?", [Name]),
	    foreach(fun(M) ->
			    io:format(F, "_~s", [atom_to_list(M)])
		    end, Ms),
	    eol(F);
	false -> ok
    end.

face(F, #e3d_face{vs=Vs,tx=Tx,ns=Ns}) ->
    io:put_chars(F, "  face"), eol(F),
    print_list(F, "vertices",  Vs),
    print_list(F, "normals",   Ns),
    print_list(F, "texcoords", Tx),
%    io:put_chars(F, "    vertices"), face_v(F, Vs), eol(F),
%    io:put_chars(F, "    normals"), face_n(F, Ns), eol(F),
%    io:put_chars(F, "    texcoords"), face_t(F, Tx), eol(F),
    io:put_chars(F, "  endface"), eol(F).

print_list(_, _, []) -> ok;
print_list(F, ListName, Indices) ->
  io:format(F, "    ~s", [ListName]),
  print_indices(F, Indices), eol(F).

print_indices(F, [Index|T]) ->
  io:put_chars(F, [$\s,integer_to_list(Index)]),
  print_indices(F, T);
print_indices(_, []) -> ok.
  

%face_v(F, [V|Vs]) ->
%    io:put_chars(F, [$\s,integer_to_list(V)]),
%    face_v(F, Vs);
%face_v(_, []) -> ok.
%
%face_t(F, [T|Ts]) ->
%    io:put_chars(F, [$\s,integer_to_list(T)]),
%    face_t(F, Ts);
%face_t(_, []) -> ok.
%
%face_n(F, [N|Ns]) ->
%    io:put_chars(F, [$\s,integer_to_list(N)]),
%    face_n(F, Ns);
%face_n(_, []) -> ok.


materials(F, Name0, Mats, Creator) ->
    Root = filename:rootname(Name0, ".bzw"),
    Name = Root ++ ".mtl",
    label(F, Creator),
    foreach(fun(M) -> material(F, Root, M) end, Mats),
    {ok,filename:basename(Name)}.

material(F, Root, {Name,Mat}) ->
    OpenGL = proplists:get_value(opengl, Mat),
    {_,_,_,Opacity} = proplists:get_value(diffuse, OpenGL),
    Shininess = proplists:get_value(shininess, OpenGL),
    io:put_chars(F, "material\r\n"),
    io:format(F, "  name ~s\r\n", [atom_to_list(Name)]),
%    mat_color(F, "  ambient", ambient, OpenGL, true),
    mat_color(F, "  diffuse", diffuse, OpenGL, false),
    io:format(F, " ~.4f\r\n", [Opacity]),
    mat_color(F, "  emission", emission, OpenGL, true),
    mat_color(F, "  specular", specular, OpenGL, true),
    io:format(F, "  shininess ~.4f\r\n", [Shininess*128]),
    Maps = proplists:get_value(maps, Mat),
    export_maps(F, Maps, Root),
    io:put_chars(F, "end\r\n"),
    eol(F).

mat_color(F, Label, Key, Mat, EndLine) ->
    {R,G,B,_} = proplists:get_value(Key, Mat),
    io:format(F, "~s ~.4f ~.4f ~.4f", [Label,R,G,B]),
    case EndLine of
        true -> eol(F);
        false -> ok
    end.

export_maps(F, [{diffuse,Map}|T], Base) ->
    export_map(F, "texture", Map, Base),
    export_maps(F, T, Base);
export_maps(F, [{ambient,Map}|T], Base) ->
    export_map(F, "Ka", Map, Base),
    export_maps(F, T, Base);
export_maps(F, [{emission,Map}|T], Base) ->
    export_map(F, "Ke", Map, Base),
    export_maps(F, T, Base);
export_maps(F, [{bump,Map}|T], Base) ->
    export_map(F, "Bump", Map, Base),
    export_maps(F, T, Base);
export_maps(F, [_|T], Base) ->
    export_maps(F, T, Base);
export_maps(_, [], _) -> ok.

export_map(_, _, none, _) -> ok;
export_map(F, Label0, #e3d_image{filename=none,name=ImageName}=Image, Root) ->
    Label = "  " ++ Label0,
    MapFile = filename:join(filename:dirname(Root), ImageName ++ ".tga"),
    io:format(F, "~s ~s\r\n", [Label,filename:basename(MapFile)]),
    ok = e3d_image:save(Image, MapFile);
export_map(F, Label0, #e3d_image{filename=Filename}, _Root) ->
    Label = "  " ++ Label0,
    io:format(F, "~s ~s\r\n", [Label,filename:basename(Filename)]).

label(F, Creator) ->
    io:format(F, "# Exported from ~s\r\n\r\n", [Creator]).

mesh_info(F, #e3d_mesh{vs=Vs,fs=Fs}) ->
    io:format(F, "  #~w vertices, ~w faces\r\n", [length(Vs),length(Fs)]).

eol(F) ->
    io:put_chars(F, "\r\n").
