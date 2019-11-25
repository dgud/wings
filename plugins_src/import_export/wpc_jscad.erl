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
    FName = filename:basename(Filename)++filename:extension(Filename),
    NObjs = length(Objs),
    %% Write file head
    io:format(F, "// File         : ~ts\n",[FName]),
    io:format(F, "// Objects      : ~lp\n",[NObjs]),
    io:format(F, "// Exported from: ~ts\n",[Creator]),
    %% Write file body
    io:format(F, "function main() {\n",[]),
    io:format(F, "  return [\n",[]),
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
    	io:format(F, "\n  ];\n}\n",[])
    catch _:Err:Stacktrace ->
	io:format(?__(1,"OpenJSCAD Error: ~P in")++" ~p~n", [Err,30,Stacktrace])
    end,
    ok = file:close(F).

export_object(F, ObjName, #e3d_mesh{type=Type,fs=Fs,vs=VTab}, Mat_defs) ->
    [#e3d_face{mat=[Material|_]}|_] = Fs,
    DifColor = material(Material, Mat_defs),
    io:format(F, "    // ~ts.~ts\n",[ObjName,Material]),
    io:format(F, "    color(~p, \n",[DifColor]),
    io:format(F, "      polyhedron(\n",[]),

    %% Helpers
    W3 = fun({X,Y,Z}) ->  io:format(F, "                    [~p,~p,~p]", [X,Y,Z]) end,
    %% Write vertex coordinates
    io:format(F, "        { points: [\n",[]),
    all(W3,F,VTab),
    io:format(F, "\n                  ],\n",[]),
    %% Write vertex indexes for faces
    SType =
	case Type of
	    triangle -> "triangles";
	    _ -> "polygons"
	end,
    io:format(F, "          ~s: [\n",[SType]),
    Ident = "          "++string:right("",length(SType))++"     ",
    all(fun(#e3d_face{vs=Vs}) -> print_face(F,Ident,Vs) end,F,Fs),
    io:format(F, "\n          ~s  ]\n",[string:right("",length(SType))]),
    %% Close polyhedron's data (points and triangles/polygons)
    io:put_chars(F, "        }\n"),
    %% Close color and polyhedron
    io:put_chars(F, "      )\n"),
    io:put_chars(F, "    )").

material(Name, Mat_defs) ->
    MatInfo = lookup(Name, Mat_defs),
    Mat = lookup(opengl, MatInfo),
    {Dr, Dg, Db, _} = lookup(diffuse, Mat),
    [Dr, Dg, Db].

print_face(F, Ident, Vs0) ->
    Vs = Vs0,
    io:put_chars(F, Ident++"["),
    %% we need to invert the face order to get it in CW sequence as OpenJSCAD needs
    lists:foldr(fun(V,1) ->
		    io:format(F, "~lp", [V]);
	       (V,Acc) ->
		    io:format(F, "~lp,", [V]),
		   Acc-1
	       end, length(Vs), Vs),
    io:put_chars(F, "]").

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
