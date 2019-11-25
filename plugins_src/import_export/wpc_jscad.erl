%%
%%  wpc_jscad.erl --
%%
%%     OpenJSCAD export.
%%
%%  Copyright (c) 2019 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_jscad).

-export([init/0,menu/2,command/2]).

-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/intl_tools/wings_intl.hrl").

init() ->
    true.

menu({file,export}, Menu) ->
    menu_entry(Menu);
menu({file,export_selected}, Menu) ->
    menu_entry(Menu);
menu(_, Menu) -> Menu.

command({file,{export,{jscad,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export(Ps, Fun, St) end,
    do_export(Ask, export, Exporter, St);
command({file,{export_selected,{jscad,Ask}}}, St) ->
    Exporter = fun(Ps, Fun) -> wpa:export_selected(Ps, Fun, St) end,
    do_export(Ask, export_selected, Exporter, St);
command(_, _) ->
    next.

menu_entry(Menu) ->
    Menu ++ [{"OpenJSCAD (.jscad)...",jscad,[option]}].

props() ->
    [{ext,".jscad"},{ext_desc,?__(1,"OpenJSCAD File")}].

%%%
%%% Export.
%%%

do_export(Ask, Op, _Exporter, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"OpenJSCAD Export Options"), dialog(export),
	       fun(Res) ->
                       {file,{Op,{jscad,Res}}}
	       end);
do_export(Attr, _Op, Exporter, _St) when is_list(Attr) ->
    set_pref(Attr),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Ps = [{subdivisions,SubDivs}|props()],
    Exporter(Ps, export_fun(Attr)).

export_fun(Attr) ->
    fun(Filename, Contents) ->
            export(Filename, Contents, Attr)
    end.

export(Filename, Contents0, Attr) ->
    Contents = export_transform(Contents0, Attr),
    #e3d_file{objs=Objs,mat=Mat,creator=Creator} = Contents,
    {ok,F} = file:open(Filename, [write]),
    FName = filename:basename(Filename),
    NObjs = length(Objs),
    %% Write file head
    io:format(F, "// File         : ~ts\n",[FName]),
    io:format(F, "// Objects      : ~lp\n",[NObjs]),
    io:format(F, "// Exported from: ~ts\n",[Creator]),
    %% Write file body
    io:put_chars(F, "function main() {\n"),
    io:put_chars(F, "  return [\n"),
    try
	lists:foldl(fun(#e3d_object{name = Name, obj=Obj}, AccObj) ->
                            Meshes = e3d_mesh:split_by_material(Obj),
                            NMeshes = length(Meshes),
                            all(fun(Mesh,AccMeshes) ->
                                        export_object(F, Name, Mesh, Mat),
                                        AccMeshes-1
                                end, F, NMeshes, Meshes),
                            if AccObj > 1 ->
                                    io:put_chars(F, ",\n");
                               true -> ignore
                            end,
                            AccObj-1
                    end, NObjs, Objs),
	io:put_chars(F, "\n    ];\n}\n")
    catch _:Err:Stacktrace ->
            io:format(?__(1,"OpenJSCAD Error: ~P in")++" ~p~n", [Err,30,Stacktrace])
    end,
    ok = file:close(F).

export_object(F, ObjName, #e3d_mesh{type=Type,fs=Fs,vs=VTab}, Mat_defs) ->
    [#e3d_face{mat=[Material|_]}|_] = Fs,
    DifColor = material(Material, Mat_defs),
    io:format(F, "\t// ~ts.~ts\n",[ObjName,Material]),
    io:format(F, "\tcolor(~w, \n",[DifColor]),
    io:put_chars(F, "\t    polyhedron(\n"),

    %% Write vertex coordinates
    io:put_chars(F, "\t\t{ points: [\n"),
    all(fun({X,Y,Z}) -> io:format(F, "\t\t\t[~.9f,~.9f,~.9f]", [X,Y,Z]) end,F,VTab),
    io:put_chars(F, "\n\t\t\t],\n"),
    %% Write vertex indexes for faces
    SType =
	case Type of
	    triangle -> "triangles";
	    _ -> "polygons"
	end,
    io:format(F, "\t\t  ~s: [\n",[SType]),
    all(fun(#e3d_face{vs=Vs}) -> io:format(F, "\t\t\t~w", [lists:reverse(Vs)]) end,F,Fs),
    io:put_chars(F, "\n\t\t\t]\n"),
    %% Close polyhedron's data (points and triangles/polygons)
    io:put_chars(F, "\t\t}\n"),
    %% Close color and polyhedron
    io:put_chars(F, "\t    )\n"),
    io:put_chars(F, "\t)").

material(Name, Mat_defs) ->
    MatInfo = lookup(Name, Mat_defs),
    Mat = lookup(opengl, MatInfo),
    {Dr, Dg, Db, _} = lookup(diffuse, Mat),
    [Dr, Dg, Db].

dialog(export) ->
    [{label_column,
      [{?__(1,"Swap Y and Z Axes"),
	{"",get_pref(swap_y_z, true),
	 [{key,swap_y_z}]}},
       {export_scale_s(),
	{text,get_pref(export_scale, 1.0),
	 [{key,export_scale},{range,{1.0,infinity}}]}},
       {?__(3,"Sub-division Steps"),
	{text,get_pref(subdivisions, 0),
	 [{key,subdivisions},{range,{0,4}}]}}
      ]}
    ].

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

export_transform(Contents, Attr) ->
    Mat = wpa:export_matrix(Attr),
    e3d_file:transform(Contents, Mat).

export_scale_s() -> ?__( 1, "Export scale").

%%% useful helpers
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
