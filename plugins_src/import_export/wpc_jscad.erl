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
    FacesGroup = proplists:get_value(faces_group, Attr, false),
    CreateRet = proplists:get_value(create_returns, Attr, object),
    BuildMain = proplists:get_value(build_main, Attr, true),
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    Tesselation = proplists:get_value(tesselation, Attr, none),
    Ps = [{tesselation,Tesselation},{faces_group,FacesGroup},{create_returns,CreateRet},
          {build_main,BuildMain},{subdivisions,SubDivs}|props()],
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
    try
        FunNames =
            lists:foldl(fun(#e3d_object{name=Name, obj=Obj}, Acc) ->
                            case proplists:get_bool(group_per_material, Attr) of
                                true ->
                                    Meshes = e3d_mesh:split_by_material(Obj),
                                    export_object(F, Name, Meshes, Mat, Attr, Acc);
                                false ->
                                    export_object(F, Name, Obj, Mat, Attr, Acc)
                            end
                        end, [], Objs),
        case proplists:get_value(build_main, Attr) of
            true -> export_main(F, FunNames, Attr);
            false -> ignore
        end
    catch _:Err:Stacktrace ->
        io:format(?__(1,"OpenJSCAD Error: ~P in")++" ~p~n", [Err,30,Stacktrace])
    end,
    ok = file:close(F).

export_main(F, FunNames, Flags) ->
    io:put_chars(F, "function main() {\n"),
    io:put_chars(F, "\treturn [\n"),
    all(fun(FunName) ->
        case proplists:get_value(create_returns, Flags) of
            object ->
                io:format(F, "\t\t~s", [FunName]);
            properties ->
                io:format(F, "\t\t~s.csg", [FunName])
        end
        end,F,FunNames),
    io:put_chars(F, "\n\t];\n}\n").

export_object(F, Name, Meshes, MatDefs, Flags, Acc0) when is_list(Meshes) ->
    lists:foldl(fun(#e3d_mesh{}=Mesh, Acc) ->
                    export_object(F, Name, Mesh, MatDefs, Flags, Acc)
                end, Acc0, Meshes);
export_object(F, Name, #e3d_mesh{vs=Vtab,vc=ColDefs0,fs=Fs}=Mesh, MatDefs, Flags, Acc) ->
    GroupPerMat = proplists:get_bool(group_per_material, Flags),
    ColDefs = array:from_list(ColDefs0),
    ObjName0 =
        case GroupPerMat of
            true ->
                [#e3d_face{mat=[Material|_]}|_] = Fs,
                io_lib:format("~ts_~tp()",[Name,Material]);
            false ->
                io_lib:format("~ts()",[Name])
        end,
    ObjName = string:replace(ObjName0," ","_"),
    %% each mesh will be created by a dedicated function using its name
    io:format(F, "function ~s {\n",[ObjName]),
    export_obj_vertices(F, Vtab),
    case proplists:get_bool(faces_group, Flags) of
        true ->
            Mats = export_groups_faces(F, Mesh),
            export_obj_colors(F, Mats, ColDefs, MatDefs, true),
            StrGroup = ",groups:Groups",
            io:put_chars(F, "\tCsgPolys = Polygons.map((m,idx) => CSG.Polygon.createFromPoints(m.map(n => Points[n])).setColor(Colors[Groups[idx]])),\n");
        false ->
            export_obj_faces(F, Fs),
            export_obj_colors(F, Fs, ColDefs, MatDefs, false),
            StrGroup = "",
            io:put_chars(F, "\tCsgPolys = Polygons.map((m,idx) => CSG.Polygon.createFromPoints(m.map(n => Points[n])).setColor(Colors[idx])),\n")
    end,
    io:put_chars(F, "\tCsg = CSG.fromPolygons(CsgPolys);\n"),
    case proplists:get_value(create_returns, Flags) of
        object ->
            io:put_chars(F, "\treturn Csg;\n");
        properties ->
            io:put_chars(F, "\treturn {points:Points,polygons:Polygons"++StrGroup++",csgpolys:CsgPolys,csg:Csg};\n")
    end,
    io:put_chars(F, "}\n\n"),
    Acc++[ObjName].

export_obj_vertices(F, Vtab) ->
    %% Writes vertex coordinates
    io:put_chars(F, "\tlet Points = [\n"),
    all(fun({X,Y,Z}) -> io:format(F, "\t\t\t[~.9f,~.9f,~.9f]", [X,Y,Z]) end,F,Vtab),
    io:put_chars(F, "\n\t\t];\n").

export_obj_faces(F, Fs) ->
    %% Writes vertex index of each face
    io:put_chars(F, "\tlet Polygons = [\n"),
    all(fun(#e3d_face{vs=Vs}) -> io:format(F, "\t\t\t~w", [Vs]) end,F,Fs),
    io:put_chars(F, "\n\t\t];\n").

export_groups_faces(F, #e3d_mesh{fs=Fs0}) ->
    {_,MatUsed,MatFaces} =
        lists:foldl(fun(#e3d_face{vs=Vs,mat=[Mat|_]}, {Grp0,AccMat0,AccFs}) ->
                        case gb_trees:lookup(Mat,AccMat0) of
                            none ->
                                Grp = Grp0+1,
                                AccMat = gb_trees:enter(Mat,Grp,AccMat0);
                            {value,Grp1} ->
                                Grp = Grp1,
                                AccMat = AccMat0
                        end,
                        {Grp, AccMat, [{Grp,Vs}|AccFs]}
                    end, {-1,gb_trees:empty(),[]}, Fs0),
    {Grs,Fs} = split_group_face(MatFaces),
    %% Writes vertex index of each face
    io:put_chars(F, "\tlet Polygons = [\n"),
    all(fun(Vs) -> io:format(F, "\t\t\t~w", [Vs]) end,F,Fs),
    io:put_chars(F, "\n\t\t];\n"),
    %% Writes group index of each face
    io:put_chars(F, "\tlet Groups = [\n"),
    all(fun(G) -> io:format(F, "\t\t\t~w", [G]) end,F,Grs),
    io:put_chars(F, "\n\t\t];\n"),
    ordsets:from_list([{Grp,Mat} || {Mat,Grp} <- gb_trees:to_list(MatUsed)]).

split_group_face(GrpFaces) ->
    split_group_face(GrpFaces, {[],[]}).
split_group_face([], Acc) -> Acc;
split_group_face([{G,F}|GrpFaces], {GAcc,FAcc}) ->
    split_group_face(GrpFaces,{[G|GAcc],[F|FAcc]}).

export_obj_colors(F, [#e3d_face{}|_]=Fs, ColDefs, MatDefs, FacesGroup) ->
    io:put_chars(F, "\tlet Colors = [\n"),
    all(fun(#e3d_face{vc=Cols,mat=[Mat|_]}) ->
            [R,G,B,A] = choose_color(Cols, Mat, ColDefs, MatDefs, FacesGroup),
            io:format(F, "\t\t\t[~.6f,~.6f,~.6f,~.6f]", [R,G,B,A])
        end,F,Fs),
    io:put_chars(F, "\n\t\t];\n");
export_obj_colors(F, Mats, ColDefs, MatDefs, FacesGroup) ->
    io:put_chars(F, "\tlet Colors = [\n"),
    all(fun({_,Mat}) ->
        [R,G,B,A] = choose_color([], Mat, ColDefs, MatDefs, FacesGroup),
        io:format(F, "\t\t\t[~.6f,~.6f,~.6f,~.6f]", [R,G,B,A])
        end,F,Mats),
    io:put_chars(F, "\n\t\t];\n").

choose_color(Cols, Mat, ColDefs, MatDefs, FacesGroup) ->
    case {Mat,FacesGroup} of
        {default,false} ->
            %% if set, it uses the face color in case not grouping by material
            case Cols of
                [] -> material(default, MatDefs);
                _ ->
                    C0 = [array:get(Ic,ColDefs) || Ic <- Cols],
                    %% jscad doesn't supports vertex color, so we compute the face's average color
                    {R0,G0,B0} = e3d_vec:average(C0),
                    [R0,G0,B0,1.0]
            end;
        _ -> material(Mat, MatDefs)
    end.

material(Name, Mat_defs) ->
    MatInfo = lookup(Name, Mat_defs),
    Mat = lookup(opengl, MatInfo),
    {Dr, Dg, Db, Da} = lookup(diffuse, Mat),
    [Dr, Dg, Db, Da].

dialog(export) ->
    [wpa:dialog_template(?MODULE, tesselation),
     {label_column,
      [{?__(1,"One group per material"),
        {"",get_pref(group_per_material, true),
        [{key,group_per_material}]}},
       {?__(6,"Swap Y and Z Axes"),
        {"",get_pref(swap_y_z, true),
         [{key,swap_y_z}]}},
       {export_scale_s(),
	{text,get_pref(export_scale, 1.0),
	 [{key,export_scale},{range,{1.0,infinity}}]}},
       {?__(7,"Sub-division Steps"),
	{text,get_pref(subdivisions, 0),
	 [{key,subdivisions},{range,{0,4}}]}},
       {?__(2,"Export faces group"),
        {"",get_pref(faces_group, false),
         [{key,faces_group},{info,?__(3,"Exports face's group IDs by material")}]}},
       {?__(4,"Build the main() statement"),
        {"",get_pref(build_main, true),
         [{key,build_main},{info,?__(5,"Add a main() function to build the scene")}]}}
      ]},
     {vframe,[{vradio,[{?__(9,"CSG Object"),object},
                       {?__(10,"Properties"),properties}],

               get_pref(create_returns, object),
       [{key,create_returns},
        {info,?__(11,"Returns the object or properties: {points:?,polygons:?,groups:?,"++
                     "cgspolygons:?,csg:?}")}]}],
      [{title,?__(8,"Creation function returns")}]}

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

lookup(K, L) ->
    {value, {K, V}} =  lists:keysearch(K, 1, L),
    V.
