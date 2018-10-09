%%
%%  wings_info.erl --
%%
%%     Format information about the selected items.
%%
%%  Copyright (c) 2016 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_info).
-export([info/1]).

-include("wings.hrl").
-import(lists, [foldl/3,sort/1]).

-spec info(#st{}) -> iolist().

info(#st{sel=[]}) ->
    [];
info(St) ->
    case wings_wm:get_prop(show_info_text) of
        false ->
            [];
        true ->
            [basic(St),measure(St)]
    end.

basic(#st{selmode=body,sel=[_]}=St) ->
    deep_object_info(St);
basic(#st{selmode=body}=St) ->
    summary_object_info(St);
basic(#st{selmode=Mode,sel=[_]}=St) ->
    MF = fun(Items, _) ->
                 case gb_sets:size(Items) of
                     Sz when Sz < 5 ->
                         {items,gb_sets:to_list(Items)};
                     Sz ->
                         {size,Sz}
                 end
         end,
    RF = fun(T, ignore) -> T end,
    case wings_sel:dfold(MF, RF, ignore, St) of
        {items,[_]=Vs} ->
            io_lib:format(str(Mode, one), Vs);
        {items,Vs} ->
            io_lib:format(str(Mode, few), [item_list(Vs)]);
        {size,N} ->
            io_lib:format(str(Mode, many), [N])
    end;
basic(#st{selmode=Mode,sel=Sel}=St) ->
    On = length(Sel),
    MF = fun(Items, _) -> gb_sets:size(Items) end,
    RF = fun erlang:'+'/2,
    N = wings_sel:dfold(MF, RF, 0, St),
    io_lib:format(str(Mode, multiple_objects), [N,On]).

measure(#st{sel=[_,_,_|_]}) ->
    %% Nothing to do when more than two objects selected.
    [];
measure(#st{selmode=vertex}=St) ->
    MF = fun(Vs0, We) ->
                 case gb_sets:size(Vs0) =< 2 of
                     true ->
                         Vs = gb_sets:to_list(Vs0),
                         [wings_vertex:pos(V, We) || V <- Vs];
                     false ->
                         [none,none,none]
                 end
         end,
    RF = fun erlang:'++'/2,
    Positions = wings_sel:dfold(MF, RF, [], St),
    measure_vs(Positions);
measure(#st{selmode=edge}=St) ->
    MF = fun(Es, #we{id=Id,es=Etab}=We) ->
                 EF = fun(Edge) ->
                              #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
                              {{Id,Edge},
                               wings_vertex:pos(Va, We),
                               wings_vertex:pos(Vb, We)}
                      end,
                 case gb_sets:size(Es) =< 2 of
                     true ->
                         [EF(E) || E <- gb_sets:to_list(Es)];
                     false ->
                         [none,none,none]
                 end
         end,
    RF = fun erlang:'++'/2,
    EdgeInfo = wings_sel:dfold(MF, RF, [], St),
    [measure_es(EdgeInfo)|enhanced_info(edge, EdgeInfo)];
measure(#st{selmode=face}=St) ->
    MF = fun(Fs, #we{id=Id}=We) ->
                 FF = fun(Face) ->
                              Center = wings_face:center(Face, We),
                              N = wings_face:normal(Face, We),
                              Mat = wings_facemat:face(Face, We),
                              Area = area_info(Face, We),
                              {{Id,Face},Center,N,Mat,Area}
                      end,
                 case gb_sets:size(Fs) =< 2 of
                     true ->
                         [FF(Face) || Face <- gb_sets:to_list(Fs)];
                     false ->
                         [none,none,none]
                 end
         end,
    RF = fun erlang:'++'/2,
    FaceInfo = wings_sel:dfold(MF, RF, [], St),
    [measure_fs(FaceInfo)|enhanced_info(face, FaceInfo)];
measure(_) -> [].

measure_vs([Pos]) ->
    io_lib:format(?__(1,". Position ~s"), [wings_util:nice_vector(Pos)]);
measure_vs([PosA,PosB]) ->
    PosDiff = e3d_vec:sub(PosB, PosA),
    Dist = e3d_vec:len(PosDiff),
    io_lib:format(?__(2,". Distance ~s  ~s"),
                  [wings_util:nice_float(Dist),
                   wings_util:nice_vector(PosDiff)]);
measure_vs(_) -> [].

measure_es([{_,PosA,PosB}]) ->
    PosDiff = e3d_vec:sub(PosB, PosA),
    Length = e3d_vec:len(PosDiff),
    Mid = e3d_vec:average(PosA, PosB),
    io_lib:format(". " ++ ?__(1,"Midpoint ~s>\nLength ~s") ++
                      "  ~s",
                  [wings_util:nice_vector(Mid),
                   wings_util:nice_float(Length),
                   wings_util:nice_vector(PosDiff)]);
measure_es([{_E0,Pos0A,Pos0B},{_E1,Pos1A,Pos1B}]) ->
    V0 = e3d_vec:sub(Pos0B, Pos0A),
    V1 = e3d_vec:sub(Pos1B, Pos1A),
    RawAngle = e3d_vec:degrees(V0, V1),
    Angle = case {Pos0A,Pos0B} of
                {Pos1B,_} -> 180.0 - RawAngle;
                {_,Pos1A} -> 180.0 - RawAngle;
                {_,_} -> RawAngle
            end,
    io_lib:format(?__(2,". Angle ~s") ++ "~c",
                         [wings_util:nice_float(Angle),?DEGREE]);
measure_es(_) -> [].

measure_fs([{_,Center,_,Mat,Area}]) ->
    io_lib:format(?__(1,". Midpoint ~s\n"
                      "Material ~ts.") ++ Area,
                  [wings_util:nice_vector(Center),
                   Mat]);
measure_fs([{_,_,N0,_,_},{_,_,N1,_,_}]) ->
    Angle = e3d_vec:degrees(N0, N1),
    io_lib:format(?__(2,". Angle ~s") ++ "~c",
                  [wings_util:nice_float(Angle),?DEGREE]);
measure_fs(_) -> [].

enhanced_info(Mode, Info) ->
    case wings_pref:get_value(info_enhanced_text) of
        true ->
            enhanced_info_1(Mode, Info);
        false ->
            []
    end.

enhanced_info_1(edge, [{{Id0,E0},Pos0A,Pos0B},
                       {{Id1,E1},Pos1A,Pos1B}]) ->
    Length0 = e3d_vec:dist(Pos0A, Pos0B),
    Length1 = e3d_vec:dist(Pos1A, Pos1B),
    Mid0 = e3d_vec:average(Pos0A, Pos0B),
    Mid1 = e3d_vec:average(Pos1A, Pos1B),
    MidDiff = e3d_vec:sub(Mid1, Mid0),
    Dist = e3d_vec:len(MidDiff),
    Diff = abs(Length0 - Length1),
    io_lib:format(?__(42,"\nDistance ~s")++"  ~s\n"++
                      ?__(43,"Object~s")++" "++?__(41,"Edge~s ~s")++"  "++
                      ?__(43,"Object~s")++" "++?__(41,"Edge~s ~s")++"  "++
                      ?__(45,"Difference ~s"),
                  [wings_util:nice_float(Dist),
                   wings_util:nice_abs_vector(MidDiff),
                   wings_util:stringify(Id0),
                   wings_util:stringify(E0),
                   wings_util:nice_float(Length0),
                   wings_util:stringify(Id1),
                   wings_util:stringify(E1),
                   wings_util:nice_float(Length1),
                   wings_util:nice_float(Diff)]);
enhanced_info_1(face, [{{Id0,F0},Center0,_,_,Area0},
                       {{Id1,F1},Center1,_,_,Area1}]) ->
    CenterDiff = e3d_vec:sub(Center1, Center0),
    Dist = e3d_vec:len(CenterDiff),
    io_lib:format(?__(42,"\nDistance ~s") ++ "  ~s\n" ++
                      ?__(43,"Object~s")++" "++?__(48,"Face~s")++Area0++"  "++
                      ?__(43,"Object~s")++" "++?__(48,"Face~s")++Area1,
                  [wings_util:nice_float(Dist),
                   wings_util:nice_abs_vector(CenterDiff),
                   wings_util:stringify(Id0),
                   wings_util:stringify(F0),
                   wings_util:stringify(Id1),
                   wings_util:stringify(F1)]);
enhanced_info_1(_, _) -> [].

item_list(Items) ->
    item_list(Items, "").

item_list([Item|Items], Sep) ->
    [Sep,integer_to_list(Item)|item_list(Items, ", ")];
item_list([], _Sep) -> [].

deep_object_info(St) ->
    MF = fun(_, We) -> deep_object_info_1(We) end,
    RF = fun(S, ignore) -> S end,
    wings_sel:dfold(MF, RF, ignore, St).

deep_object_info_1(We) when ?IS_LIGHT(We) ->
    wings_light:info(We);
deep_object_info_1(#we{id=Id,name=Name,fs=Ftab,es=Etab,vp=Vtab}=We) ->
    Faces = gb_trees:size(Ftab),
    case array:size(Etab) < 50000 of
        true ->
            Edges = wings_util:array_entries(Etab),
            Vertices = wings_util:array_entries(Vtab),
            Format = ?__(new_object_info,
                         "Object ~p \"~ts\" has ~p polygons, "
                         "~p edges, ~p vertices~s~s.");
        false ->
            Edges = array:size(Etab),
            Vertices = array:size(Vtab),
            Format = ?__(new_object_info2,
                         "Object ~p \"~ts\" has ~p polygons, "
                         "~~~p edges, ~~~p vertices~s~s.")
    end,
    io_lib:format(Format, [Id,Name,Faces,Edges,Vertices,
                           vtx_attributes(We),hole_info(We)]).

vtx_attributes(We) ->
    case wings_va:any_attributes(We) of
	false -> "";
	true -> ", " ++ ?__(1,"vertex attributes")
    end.

hole_info(#we{holes=[]}) ->
    "";
hole_info(#we{holes=Holes}) ->
    case length(Holes) of
	1 -> [", 1 ",?__(1,"hole")];
	N -> [", ",integer_to_list(N)," ",?__(2,"holes")]
    end.

summary_object_info(St) ->
    MF = fun(_, We) ->
                 #we{fs=Ftab,es=Etab,vp=Vtab} = We,
                 Faces = gb_trees:size(Ftab),
                 Appr = array:size(Etab) >= 50000,
                 case Appr of
                     false ->
                         Edges = wings_util:array_entries(Etab),
                         Vertices = wings_util:array_entries(Vtab);
                     true ->
                         Edges = array:size(Etab),
                         Vertices = array:size(Vtab)
                 end,
                 {1,Faces,Edges,Vertices}
         end,
    RF = fun({A,B,C,D}, {A0,B0,C0,D0}) ->
                 {A+A0,B+B0,C+C0,D+D0}
         end,
    Acc0 = {0,0,0,0},
    {N,Faces,Edges,Vertices} = wings_sel:dfold(MF, RF, Acc0, St),
    io_lib:format(?__(2,
             "~p objects, ~p faces, ~p edges, ~p vertices"),
                  [N,Faces,Edges,Vertices]).

area_info(Face, We) ->
    case wings_face:vertices(Face, We) =< 50 of
        true ->
            A = wings_face:area(Face, We),
            io_lib:format(?__(40," Area ~s"), [wings_util:nice_float(A)]);
        false ->
            []
    end.

str(vertex, one) ->
    ?__(1,"Vertex ~p selected");
str(vertex, few) ->
    ?__(2,"Vertices ~s selected");
str(vertex, many) ->
    ?__(3,"~p vertices selected");
str(vertex, multiple_objects) ->
    ?__(4,"~p vertices selected in ~p objects");
str(edge, one) ->
    ?__(5,"Edge ~p selected");
str(edge, few) ->
    ?__(6,"Edges ~s selected");
str(edge, many) ->
    ?__(7,"~p edges selected");
str(edge, multiple_objects) ->
    ?__(8,"~p edges selected in ~p objects");
str(face, one) ->
    ?__(9,"Face ~p selected");
str(face, few) ->
    ?__(10,"Faces ~s selected");
str(face, many) ->
    ?__(11,"~p faces selected");
str(face, multiple_objects) ->
    ?__(12,"~p faces selected in ~p objects").
