%%
%%  wpc_ply.erl --
%%
%%     Ply format (by Stanford university) import/export.
%%
%%  Copyright (c) 2003-2011 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_ply).

-export([init/0,menu/2,command/2]).

-export([import/1]). %% ,export/2,export/3]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keysearch/3,foreach/2,
		map/2,foldl/3]).

init() ->
    true.

menu({file,import}, Menu) ->
    menu_entry(Menu);
% menu({file,export}, Menu) ->
%     menu_entry(Menu);
% menu({file,export_selected}, Menu) ->
%     menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{import,{ply,Ask}}}, St) ->
    do_import(Ask, St);
%command({file,{export,{ply,Ask}}}, St) ->
%    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
%    do_export(Ask, export, Exporter, St);
%command({file,{export_selected,{ply,Ask}}}, St) ->
%    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
%    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"Stanford ply (.ply)...",ply,[option]}].

props() ->
    [{ext,".ply"},{ext_desc,"Stanford ply File"}].

%%%
%%% Import.
%%%

do_import(Ask, _) when is_atom(Ask) ->
    wpa:dialog(Ask, "Stanford PLY Import Options",
	       dialog(import),
	       fun(Res) ->
		       {file,{import,{ply,Res}}}
	       end);
do_import(Attr, St) ->
    set_pref(Attr),
    wpa:import(props(), import_fun(Attr), St).

import_fun(Attr) ->
    fun(Filename) ->
	    case import(Filename) of
		{ok,E3dFile0} ->
		    E3dFile = import_transform(E3dFile0, Attr),
		    {ok,E3dFile};
		{error,Error} ->
		    {error,Error}
	    end
    end.

%%%
%%% Export.
%%%

%do_export(Ask, Op, _Exporter, St) when is_atom(Ask) ->
%    wpa:dialog(Ask, dialog(export), St,
%	       fun(Res) ->
%		       {file,{Op,{ply,Res}}}
%	       end);
%do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
%    set_pref(Attr),
%    SubDivs = proplists:get_value(subdivisions, Attr, 0),
%    Ps = [{subdivisions,SubDivs}|props()],
%    Exporter(Ps, export_fun(Attr)).

%export_fun(Attr) ->
%    fun(Filename, Contents) ->
%	    export_1(Filename, Contents, Attr)
%    end.

%export_1(Filename, Contents0, Attr) ->
%    Contents = export_transform(Contents0, Attr),
%    case export(Filename, Contents, Attr) of
%	ok -> ok;
%	{error,_}=Error -> Error
%    end.

dialog(import) ->
    [wpa:dialog_template(?MODULE, import)].

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%export_transform(Contents, Attr) ->
%    Mat = wpa:export_matrix(Attr),
%    e3d_file:transform(Contents, Mat).

import_transform(Contents, Attr) ->
    Mat = wpa:import_matrix(Attr),
    e3d_file:transform(Contents, Mat).

import(Name) ->
    case read_open(Name) of
	{ok,Fd} ->
	    Dir = filename:dirname(Name),
	    Res = import_1(Fd, Dir),
	    close(Fd),
	    Res;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(Fd, Dir) ->
    case catch import_2(Fd, Dir) of
	{'EXIT',Reason} -> exit(Reason);
	{error,_}=Error -> Error;
	{format_not_supported,Reason} -> {error, Reason};
	#e3d_file{}=E3dFile -> {ok,E3dFile}
    end.

import_2(Fd0, _Dir) ->
    {Spec, Fd1} = read_header(Fd0),
%%    io:format("Spec ~p~n", [Spec]),
    Data = read(Spec, Fd1, []),
    Vs0 = convert_vs(Data),
    MyAcc = fun
        ({X,Y,Z,Color}, {Vs1,Vc1}) ->
            {[{X,Y,Z}|Vs1],[convert_rgb(Color)|Vc1]};
        ({X,Y,Z},{Vs1,Vc1}) ->{[{X,Y,Z}|Vs1],Vc1}
    end, 
    {Vs,VC} = lists:foldr(MyAcc,{[],[]},Vs0),
    {Fs0,OnlyTris} = convert_fs(Data),
    Fs = if VC==[] -> Fs0; true -> process_vc(Fs0) end,
    Type = if OnlyTris -> triangle; true -> polygon end,
    Mesh = #e3d_mesh{type = Type, vs = Vs, fs = Fs, vc=VC},
    #e3d_file{objs = [#e3d_object{obj=Mesh}]}.

convert_rgb({R,_,_}=Color) when is_integer(R) -> wings_color:rgb4fv(Color);
convert_rgb({R,_,_,_}=Color) when is_integer(R) -> wings_color:rgb4fv(Color);
convert_rgb(Color) -> Color.

convert_vs([{vertex, _No, Vars, _Ts, Data}|_]) -> 
    convert_vs(Vars, Vars, Data, []);
convert_vs([_|T]) -> 
    convert_vs(T).

    
%% element properties probably don't need to be any order ... likely x,y,z would be first
%% but not wanting to make any assumptions about what fields come next.
convert_vs([x,y,z|_]=Keys,Vars,[[X,Y,Z|_]=Values|T],Acc)  ->
    IsRGB =  lists:member(red,Keys) andalso lists:member(green,Keys) andalso lists:member(blue,Keys),
    if
        IsRGB =:= true ->
            HasAlpha = lists:member(alpha,Keys),
            Zipped    = lists:zip(Keys, Values),
            {red,R}   = lists:keyfind(red,1,Zipped),
            {green,G} = lists:keyfind(green,1,Zipped),
            {blue,B}  = lists:keyfind(blue,1,Zipped),
            Color =
		if HasAlpha =:= true ->
		    {alpha,A}  = lists:keyfind(alpha,1,Zipped),
		    {R,G,B,A};
		true -> {R,G,B}
		end,
            convert_vs(Vars,Vars,T,[{float(X),float(Y),float(Z),Color}|Acc]);
        true -> 
            convert_vs(Vars,Vars,T,[{float(X),float(Y),float(Z)}|Acc])
    end;
convert_vs([_|V1], Vs, [[_|T0]|T1], Acc) ->
    convert_vs(V1,Vs, [T0|T1],Acc);
convert_vs(_,_,[],Acc) -> 
    reverse(Acc).

convert_fs([{face, _No, Vars, _Ts, Data}|_]) -> 
    convert_fs(Vars, Vars, Data, true, []);
convert_fs([_|T]) ->
    convert_fs(T).
convert_fs([vertex_index|V1],Vars,T,OT,Acc) ->
    convert_fs([vertex_indices|V1],Vars,T,OT,Acc);
convert_fs([vertex_indices|_],Vars,[[Fs|_]|T],OT,Acc) ->
    TrisOnly = OT andalso (length(Fs) == 3),
    convert_fs(Vars,Vars,T, TrisOnly,  [#e3d_face{vs = Fs}|Acc]);
convert_fs([_What|V1], Vs, [[_|T0]|T1], OT, Acc) ->
%%    io:format("Skipped ~p ~n", [_What]),
    convert_fs(V1,Vs, [T0|T1],OT,Acc);
convert_fs(_,_,[],OT,Acc) -> 
    {reverse(Acc), OT}.

process_vc(Fs) ->
    lists:foldr(fun(#e3d_face{vs=Vs}=F, Acc) ->
		    [F#e3d_face{vc=Vs}|Acc]
		end, [], Fs).

read([{What, No, Vars, Types}|Rest], Fd0, A) ->
    {Elements, Fd1} =
	read_elements(Types, Types, get_line(Fd0), No, [], []),
    read(Rest, Fd1, [{What, No, Vars, Types, Elements} | A]);
read([], _, A) ->
    reverse(A).

read_elements([float|TR], T, {[V|VR],Fd}, No, Row, Tot) ->
    read_elements(TR,T, {VR,Fd},No,[str2float(V)|Row],Tot);
read_elements([{LT, Type}|TR], T, {[V|VR0],Fd}, No, Row, Tot)
  when LT /= float->
    {List, VR1}= read_list(list_to_integer(V), Type, VR0, []),
    read_elements(TR,T, {VR1,Fd},No, [List|Row],Tot);
read_elements([int|TR], T, {[V|VR],Fd}, No, Row, Tot) ->
    read_elements(TR,T, {VR,Fd},No,[list_to_integer(V)|Row],Tot);
read_elements([], T, {[],Fd}, No, Row, Tot) ->
    if No > 1 ->
	    read_elements(T,T,get_line(Fd),No-1,[],[reverse(Row)|Tot]);
       true ->
	    {reverse([reverse(Row)|Tot]), Fd}
    end.

read_list(No, _, Rs, Acc) when No < 1 ->
    {reverse(Acc), Rs};
read_list(No, float, [H|T], Acc)->
    read_list(No-1, float, T, [str2float(H)|Acc]);
read_list(No, int, [H|T], Acc)->
    read_list(No-1, int, T, [list_to_integer(H)|Acc]).

read_header(Fd0) ->
    {["ply"], Fd1} = get_line(Fd0),
    case get_line(Fd1) of
	{["format","ascii","1.0"], Fd2} ->
	    {Head, Fd3} = read_header(get_line(Fd2), []),
	    Spec = parseHead(Head, []),
	    {Spec, Fd3};
	{["format",Format,Ver], _} -> format_not_supported(Format,Ver);
	Res -> Res
    end.

format_not_supported(Format,Ver) ->
    ErrorMsg = io_lib:format("Error reading PLY file:\n\n" ++
			     "~p ~p format is not supported.",[Format,Ver]),
    throw({format_not_supported,ErrorMsg}).

parseHead([{Type, Num}|Rest], Acc) ->
    {Vars, Types, R2} = parseProps(Rest, [], []),
    parseHead(R2, [{Type, Num, Vars, Types}|Acc]);
parseHead([], Acc) ->
    reverse(Acc).
    
parseProps([{Var, Type}|Rest], V0, T0) when is_atom(Type), is_atom(Var) ->
    parseProps(Rest, [Var|V0], [Type|T0]);
parseProps([{ListType, Var, Type}|Rest], V0, T0) 
  when is_atom(Type),is_atom(Var) ->
    parseProps(Rest, [Var|V0], [{ListType,Type}|T0]);
parseProps(Rest,V0,T0) ->
    {reverse(V0),reverse(T0),Rest}.

read_header({["comment"|_] ,Fd}, Acc) ->
    read_header(get_line(Fd), Acc);
read_header({["element","vertex",C], Fd}, Acc) ->
    read_header(get_line(Fd), [{vertex, list_to_integer(C)}|Acc]);
read_header({["element","face",C], Fd}, Acc) ->
    read_header(get_line(Fd), [{face, list_to_integer(C)}|Acc]);
read_header({["element","edge",C], Fd}, Acc) ->
    read_header(get_line(Fd), [{edge, list_to_integer(C)}|Acc]);
read_header({["element","material",C], Fd}, Acc) ->
    read_header(get_line(Fd), [{material, list_to_integer(C)}|Acc]);
read_header({["property", Type, Var], Fd}, Acc) ->
    read_header(get_line(Fd), [{list_to_atom(Var), type(Type)}|Acc]);
read_header({["property","material_index"=Var, Type], Fd}, Acc) ->
    read_header(get_line(Fd), [{list_to_atom(Var), type(Type)}|Acc]);
read_header({["property", "list", ListLenType, Type, Var], Fd}, Acc) ->
    read_header(get_line(Fd), [{list_to_atom(ListLenType),
				list_to_atom(Var),
				type(Type)}|Acc]);
read_header({["end_header"|_] ,Fd}, Acc) ->
    {reverse(Acc), Fd}.

type("float") -> float;
type("float32") -> float;
type("double") -> float;
type(_) -> int.

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
		_ -> {string:tokens(reverse(Line)," \t\n"),{Fd,[]}}
	    end;
	{ok,Cs} -> get_line(Cs, Fd, Line)
    end;
get_line([$\r|Cs], Fd, Line) ->	%%  in this case, it's expected the pair \r\n
    get_line(Cs, Fd, Line);
get_line([$\n|Cs], Fd, Line) ->
    {string:tokens(reverse(Line, []), " \t\n"),{Fd,Cs}};
get_line([C|Cs], Fd, Line) ->
    get_line(Cs, Fd, [C|Line]).
    
str2float(S) ->
    case catch list_to_float(S) of
	{'EXIT',_} -> str2float_1(S, []);
	F -> F
    end.

str2float_1([H|T], Acc) when H == $e; H == $E ->
    foreach(fun($-) -> ok;
	       ($+) -> ok;
	       (D) when $0 =< D, D =< $9 -> ok
	    end, Acc),
    NumStr = reverse(Acc, ".0e") ++ T,
    list_to_float(NumStr);
str2float_1([H|T], Acc) ->
    str2float_1(T, [H|Acc]);
str2float_1([], Acc) ->
    float(list_to_integer(reverse(Acc))).
