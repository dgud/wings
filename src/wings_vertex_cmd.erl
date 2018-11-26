%%
%%  wings_vertex_cmd.erl --
%%
%%     This module contains most of the commands for vertices.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_vertex_cmd).
-export([menu/3,command/2,tighten_vs/2,tighten_vs/3,
	 connect/2,bevel_vertex/2,flatten/2]).

-export([set_color/2]).

-include("wings.hrl").
-import(lists, [member/2,foldl/3,mapfoldl/3,sort/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{?STR(menu,2,"Move"),{move,Dir},[],[magnet]},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?STR(menu,3,"Extrude"),{extrude,Dir}},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    {?STR(menu,4,"Connect"),connect_menu(),
	     {?STR(menu,5,"Create a new edge by connecting selected vertices"),
	      ?STR(menu,18,"Create new edges through faces between two selected vertices"),
	      ?STR(menu,17,"Connect vertices and return the new edge selected")},[]},
	    {?STR(menu,6,"Tighten"),tighten,
	     ?STR(menu,7,"Move selected vertices towards average midpoint"),[magnet]},
	    {?STR(menu,8,"Bevel"),bevel,?STR(menu,9,"Create faces of selected vertices")},
	    {?STR(menu,10,"Dissolve"),dissolve,
	     ?STR(menu,11,"Delete selected vertices (clearing selection)")},
	    {?STR(menu,12,"Collapse"),collapse,
	     ?STR(menu,13,"Delete selected vertices (creating a face selection)")},
	    separator,
	    {?STR(menu,14,"Deform"),wings_deform:sub_menu(St)},
	    separator,
	    {?STR(menu,15,"Vertex Color"),vertex_color,
	     ?STR(menu,16,"Apply vertex colors to selected vertices")}],
    wings_menu:popup_menu(X, Y, vertex, Menu).

connect_menu() ->
    fun
      (1, _Ns) -> {vertex,connect};
      (2, _Ns) -> {vertex,connect_cuts};
      (3, _Ns) -> {vertex,connecting_edge}
    end.

%% Vertex menu.
command({flatten,Plane}, St) ->
    flatten(Plane, St);
command(connect, St) ->
    {save_state,connect(St)};
command(connect_cuts, St) ->
    {save_state,connect_cuts(St)};
command(connecting_edge, St) ->
    {save_state,connecting_edge(St)};
command(tighten, St) ->
    tighten(St);
command({tighten,Magnet}, St) ->
    tighten(Magnet, St);
command(bevel, St) ->
    ?SLOW(bevel(St));
command({extrude,Type}, St) ->
    ?SLOW(extrude(Type, St));
command({deform,Deform}, St0) ->
    ?SLOW(wings_deform:command(Deform, St0));
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command(dissolve, St) ->
    {save_state,dissolve(St)};
command(collapse, St) ->
    {save_state,wings_collapse:collapse(St)};
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command(vertex_color, St) ->
    wings_color:choose(fun(Color) ->
			       set_color(Color, St)
		       end);
command({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun command/2).
    
%%%
%%% The Flatten command.
%%%

flatten({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun flatten/2);
flatten({Plane,Center}, St) ->
    flatten(Plane, Center, St);
flatten(Plane, St) ->
    flatten(Plane, average, St).

flatten(Plane0, average, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Vs, We) ->
	       wings_vertex:flatten(Vs, Plane, We)
       end, St)};
flatten(Plane0, Center, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Vs, We) ->
	       wings_vertex:flatten(Vs, Plane, Center, We)
       end, St)}.
    
%%%
%%% The Extrude command.
%%%

extrude(Type, St0) ->
    MF = fun(Vs, We0) -> extrude_vertices(Vs, We0) end,
    St = wings_sel:map(MF, St0),
    DF = fun(_, #we{temp=PlusMinus}) -> PlusMinus end,
    wings_move:plus_minus(Type, DF, St).

extrude_vertices(Vs, We0) ->
    We = gb_sets:fold(
	   fun(V, A) ->
		   ex_new_vertices(V, We0, A)
	   end, We0, Vs),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    We#we{temp={Vs,NewVs,gb_sets:empty()}}.

ex_new_vertices(V, OrigWe, #we{vp=Vtab}=We0) ->
    Center = wings_vertex:pos(V, We0),
    {We,VsFaces} =
	wings_vertex:fold(
	  fun(Edge, Face, Rec, {W0,Vs}) ->
		  OtherV = wings_vertex:other(V, Rec),
		  R = edge_ratio(OtherV, OrigWe),
		  Pos0 = array:get(OtherV, Vtab),
		  Dir = e3d_vec:sub(Pos0, Center),
		  Pos = e3d_vec:add(Center, e3d_vec:mul(Dir, R)),
		  {W,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
		  {W,[NewV,Face|Vs]}
	  end, {We0,[]}, V, We0),
    ex_connect(VsFaces, VsFaces, We).

edge_ratio(V, #we{vp=Vtab}) ->
    case array:get(V, Vtab) of
	undefined -> 1/3;
	_ -> 0.25
    end.

ex_connect([_,Face|T], More, #we{mirror=Face}=We) ->
    ex_connect(T, More, We);
ex_connect([Va,Face|[Vb|_]=T], More, We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    ex_connect(T, More, We);
ex_connect([_,Face], _, #we{mirror=Face}=We) ->
    We;
ex_connect([Va,Face], [Vb|_], We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    We;
ex_connect([], _, We) ->
    We.

%%%
%%% The Bevel command.
%%%

bevel(St0) ->
    St = wings_sel:map_update_sel(
           fun(VsSet, We) ->
                   bevel_1(VsSet, We)
           end, face, St0),
    Limit = bevel_limit(St),
    DF = fun(_, #we{temp={_,Tv}}) -> Tv end,
    wings_drag:fold(DF, [{distance,{0.0,Limit}}], St).

bevel_limit(St) ->
    MF = fun(_, #we{temp={Limit,_}}) -> Limit end,
    RF = fun min/2,
    wings_sel:dfold(MF, RF, infinity, St).

bevel_vertex(V, We0) ->
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 -> We0;
	NumEdges ->
	    {We,_,_,_} = bevel_vertex_1(V, Es, NumEdges, [], We0, [], []),
	    We
    end.

bevel_1(VsSet, We0) ->
    Vs = gb_sets:to_list(VsSet),
    {We1,VecVs0,WeTrans,Fs0} = bevel_vertices(Vs, VsSet, We0, We0, [], [], []),
    {VecVs,Limit} = bevel_normalize(VecVs0),
    case VecVs of
	[] ->
	    {We0, gb_sets:empty()};
	_ ->
	    Tv = {we,WeTrans,wings_drag:translate_fun(VecVs, We1)},
	    {We1#we{temp={Limit,Tv}}, gb_sets:from_list(Fs0)}
    end.

bevel_vertices([V|Vs], VsSet, WeOrig, We0, Acc0, Facc, WeTrans0) ->
    Adj = adjacent(V, VsSet, WeOrig),
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 ->					%Winged vertex - ignore.
	    bevel_vertices(Vs, VsSet, WeOrig, We0, Acc0, Facc, WeTrans0);
	NumEdges ->
	    {We,Acc,WeTrans,Face} =
		bevel_vertex_1(V, Es, NumEdges, Adj, We0, Acc0, WeTrans0),
	    bevel_vertices(Vs, VsSet, WeOrig, We, Acc, [Face|Facc], WeTrans)
    end;
bevel_vertices([], _, _, We, Acc, Facc, WeTrans) -> {We,Acc,WeTrans,Facc}.

bevel_vertex_1(V, Es, NumEdges, Adj, We0, Vec0, WeTrans0) ->
    {InnerFace,We1} = wings_we:new_id(We0),
    {Ids,We2} = wings_we:new_wrap_range(NumEdges, 2, We1),
    #we{es=Etab0,vc=Vct0,vp=Vtab0,fs=Ftab0} = We1,
    {Vct,Vtab} = bevel_vertices_1(V, Ids, NumEdges, Vct0, Vtab0),
    {_,Etab,Vec} = foldl(
		     fun(E, {Ids0,Etab1,Vs0}) ->
			     {Etab,Vec} = bevel(V, E, InnerFace, Ids0,
						Adj, Vtab0, Etab1),
			     {wings_we:bump_id(Ids0),Etab,[Vec|Vs0]}
		     end, {Ids,Etab0,Vec0}, Es),
    Mat = bevel_material(Es, We2),
    NewEdge = wings_we:id(1, Ids),
    Ftab = gb_trees:insert(InnerFace, NewEdge, Ftab0),
    We3 = wings_facemat:assign(Mat, [InnerFace], We2),
    We4 = We3#we{es=Etab,fs=Ftab,vc=Vct,vp=Vtab},

    %% Handling updating of vertex attributes.
    {We,WeTrans} =
	case wings_va:any_attributes(We4) of
	    false ->
		%% No vertex attributes.
		{We4,WeTrans0};
	    true ->
		%% Define a fun to update the vertex attributes dynamically.
		VaTrans = bevel_va_fun(V, Es, InnerFace, We0),
		WeTrans1 = [VaTrans|WeTrans0],

		%% Calculate initial values for vertex attributes.
		%% This is necessary to get correct results in case two
		%% neighboring vertices are being beveled.
		We5 = VaTrans(We4, [0.0]),
		{We5,WeTrans1}
	end,
    {We,Vec,WeTrans,InnerFace}.

bevel_material(Es, We) ->
    bevel_material(Es, We, []).

bevel_material([{_,Face,_}|Es], We, Acc) ->
    Mat = wings_facemat:face(Face, We),
    bevel_material(Es, We, [{Mat,Face}|Acc]);
bevel_material([], _, A0) ->
    A1 = sofs:relation(A0, [{mat,face}]),
    A2 = sofs:relation_to_family(A1),
    A = sofs:to_external(A2),
    [{_,Mat}|_] = sort([{-length(Fs),M} || {M,Fs} <- A]),
    Mat.

bevel(V, {Edge,Face,Rec0}, InnerFace, Ids, Adj, Vtab, Etab0) ->
    Vprev = wings_we:id(0, Ids),
    Eprev = wings_we:id(1, Ids),
    Va = wings_we:id(2, Ids),
    Ecurr = wings_we:id(3, Ids),
    Vb = wings_we:id(4, Ids),
    Enext = wings_we:id(5, Ids),
    {Rec,Curr} =
	case Rec0 of
	    #edge{vs=V,ve=Vother,rf=Face} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Eprev},
		 Rec0#edge{vs=Vb,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Eprev,ltsu=Enext}};
	    #edge{vs=V,ve=Vother} ->
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Enext},
		 Rec0#edge{vs=Vprev,ve=Va,lf=InnerFace,
			   rtsu=Edge,ltpr=Enext,ltsu=Eprev}};
	    #edge{ve=V,vs=Vother,lf=Face} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Eprev},
		 Rec0#edge{vs=Va,ve=Vb,rf=InnerFace,
			   ltsu=Edge,rtpr=Eprev,rtsu=Enext}};
	    #edge{ve=V,vs=Vother} ->
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Enext},
		 Rec0#edge{vs=Va,ve=Vprev,rf=InnerFace,
			   ltsu=Edge,rtpr=Enext,rtsu=Eprev}}
	end,
    Etab = array:set(Edge, Rec, Etab0),
    Vpos = array:get(V, Vtab),
    Vec = bevel_vec(Adj, Vother, Vpos, Vtab),
    {array:set(Ecurr, Curr, Etab),{Vec,Va}}.

bevel_vec(Adj, Vother, Vpos, Vtab) ->
    Opos = array:get(Vother, Vtab),
    case member(Vother, Adj) of
	true ->
	    e3d_vec:sub(e3d_vec:average(Opos, Vpos), Vpos);
	false ->
	    e3d_vec:sub(Opos, Vpos)
    end.

bevel_vertices_1(V, Ids, N, Vct0, Vtab0) ->
    Pos = array:get(V, Vtab0),
    Vct = array:reset(V, Vct0),
    Vtab = array:reset(V, Vtab0),
    bevel_new_vertices(Ids, N, Pos, Vct, Vtab).

bevel_new_vertices(Ids, N, Pos, Vct0, Vtab0) when N > 0 ->
    V = Id = wings_we:id(0, Ids),
    Edge = Id + 1,
    Vct = array:set(V, Edge, Vct0),
    Vtab = array:set(V, Pos, Vtab0),
    bevel_new_vertices(wings_we:bump_id(Ids), N-1, Pos, Vct, Vtab);
bevel_new_vertices(_, _, _, Vct, Vtab) -> {Vct,Vtab}.

bevel_normalize(VecVs) ->
    mapfoldl(fun({Vec,V}, M0) ->
		     Min = min(e3d_vec:len(Vec), M0),
		     {{e3d_vec:norm(Vec),[V]},Min}
	     end, infinity, VecVs).

adjacent(V, Vs, We) ->
    wings_vertex:fold(
      fun(_, _, Rec, A) ->
	      OtherV = wings_vertex:other(V, Rec),
	      case gb_sets:is_member(OtherV, Vs) of
		  true -> [OtherV|A];
		  false -> A
	      end
      end, [], V, We).

%%%
%%% Update vertex attributes for Bevel.
%%%

bevel_va_fun(V, Es0, InnerFace, OrigWe) ->
    Es = bevel_va_preprocess_edges(Es0, V, OrigWe),
    fun(We, [Move]) ->
	    bevel_va(Es, Move, V, InnerFace, OrigWe, We)
    end.

%% Preprocess the edge information to optimize performance
%% of the drag operation.
bevel_va_preprocess_edges([{Edge,_,Rec}|T], V, OrigWe) ->
    %% If performance becomes a real issue, we could precalculate
    %% a lot more (at the expense of using more memory). Here we
    %% basically just precalculates Scale (because it seems
    %% ridiculously expensive to calculate it for every mouse
    %% motion event).
    case Rec of
	#edge{vs=V,ve=OtherV} ->
	    Scale = bevel_scale_factor(V, OtherV, OrigWe),
	    [{vs,Edge,Scale}|bevel_va_preprocess_edges(T, V, OrigWe)];
	#edge{ve=V,vs=OtherV} ->
	    Scale = bevel_scale_factor(V, OtherV, OrigWe),
	    [{ve,Edge,Scale}|bevel_va_preprocess_edges(T, V, OrigWe)]
    end;
bevel_va_preprocess_edges([], _, _) -> [].

%% During the drag, update the vertex attributes.
bevel_va([{vs,Edge,Scale}|Es], Move, V, InnerFace, OrigWe, #we{es=Etab}=We0) ->
    W = Move * Scale,
    LeftAttr = wings_va:edge_attrs(Edge, left, W, OrigWe),
    RightAttr = wings_va:edge_attrs(Edge, right, W, OrigWe),
    InnerAttr = wings_va:average_attrs(LeftAttr, RightAttr),
    #edge{rtpr=Rtpr,rf=Rf} = array:get(Edge, Etab),
    We1 = wings_va:set_edge_attrs(Rtpr, Rf, RightAttr, We0),
    We2 = bevel_va_inner(Rtpr, InnerFace, InnerAttr, We1),
    We = wings_va:set_edge_attrs(Edge, left, LeftAttr, We2),
    bevel_va(Es, Move, V, InnerFace, OrigWe, We);
bevel_va([{ve,Edge,Scale}|Es], Move, V, InnerFace, OrigWe, #we{es=Etab}=We0) ->
    W = 1.0 - Move*Scale,
    LeftAttr = wings_va:edge_attrs(Edge, left, W, OrigWe),
    RightAttr = wings_va:edge_attrs(Edge, right, W, OrigWe),
    InnerAttr = wings_va:average_attrs(LeftAttr, RightAttr),
    #edge{ltpr=Ltpr,lf=Lf} = array:get(Edge, Etab),
    We1 = wings_va:set_edge_attrs(Ltpr, Lf, LeftAttr, We0),
    We2 = bevel_va_inner(Ltpr, InnerFace, InnerAttr, We1),
    We = wings_va:set_edge_attrs(Edge, right, RightAttr, We2),
    bevel_va(Es, Move, V, InnerFace, OrigWe, We);
bevel_va([], _, _, _, _, We) -> We.

bevel_va_inner(E, Inner, Attr, #we{es=Etab}=We) ->
    case array:get(E, Etab) of
	#edge{lf=Inner,ltpr=Ltpr} ->
	    wings_va:set_edge_attrs(Ltpr, Inner, Attr, We);
	#edge{rf=Inner,rtpr=Rtpr} ->
	    wings_va:set_edge_attrs(Rtpr, Inner, Attr, We)
    end.

%% bevel_scale_factor(VertexA, VertexB) -> Scale
%%  Calculate a scale factor to multiply the scale factor
%%  to multiply Move by to reparameterize it to the
%%  interval 0..1.
bevel_scale_factor(Va, Vb, #we{vp=Vtab}) ->
    PosA = array:get(Va, Vtab),
    PosB = array:get(Vb, Vtab),
    case e3d_vec:dist(PosB, PosA) of
	Dist when Dist < 1.0E-10 -> 1.0;
	Dist -> 1.0 / Dist
    end.

%%%
%%% The Connect command.
%%%

connect(#st{}=St) ->
    wings_sel:map(fun connect/2, St).

connect(Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
	     ({Face,Vs}, Acc) -> wings_vertex:connect(Face, Vs, Acc)
	  end, We, FaceVs).

connecting_edge(St0) ->
    wings_sel:map_update_sel(fun connecting_edge/2, edge, St0).

connecting_edge(Vs0, #we{mirror=MirrorFace}=We0) ->
    FaceVs = wings_vertex:per_face(Vs0, We0),
    We1 = lists:foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
			 ({Face,Vs}, Acc) -> wings_vertex:connect(Face, Vs, Acc)
		      end, We0, FaceVs),
    Sel = wings_we:new_items_as_gbset(edge, We0, We1),
    {We1,Sel}.

connect_cuts(St) ->
    wings_sel:map_update_sel(fun connect_cuts_fun/2, edge, St).

connect_cuts_fun(Vs0, We) ->
    case gb_sets:size(Vs0) of
        2 -> ok;
        3 -> ok;
        _ -> connect_cuts_error()
    end,
    Vs = gb_sets:to_list(Vs0),
    VsPairs = [{A,B} || A <- Vs, B <- Vs, A < B],
    do_connect_cuts(VsPairs, gb_sets:empty(), We).

do_connect_cuts([{VS0,VE0}|Pairs], Es0, We0) ->
    {We,Es1} = wings_vertex:connect_cut(VS0, VE0, We0),
    Es = gb_sets:union(Es0, Es1),
    do_connect_cuts(Pairs, Es, We);
do_connect_cuts([], Es, We) ->
    {We,Es}.

-spec connect_cuts_error() -> no_return().
connect_cuts_error() ->
    Msg = ?__(1, "Defined only for two or three selected vertices, per object."),
    wings_u:error_msg(Msg).


%%%
%%% The Tighten command.
%%%

tighten(St) ->
    wings_drag:fold(fun tighten_vs/2, [percent], St).

tighten_vs(Vs, We) when is_list(Vs) ->
    Translate = [{tighten_vec(V, We),[V]} || V <- Vs],
    wings_drag:translate_fun(Translate, We);
tighten_vs(Vs, We) ->
    tighten_vs(gb_sets:to_list(Vs), We).

tighten_vec(V, #we{vp=Vtab,mirror=MirrorFace}=We) ->
    Cs = wings_vertex:fold(
	   fun(_, Face, _, A) when Face =/= MirrorFace ->
		   FaceVs = wings_face:to_vertices([Face], We),
		   C = wings_vertex:center(FaceVs, We),
		   [C|A];
	      (_, _, _, A) -> A
	   end, [], V, We),
    Center = e3d_vec:average(Cs),
    e3d_vec:sub(Center, array:get(V, Vtab)).

%%%
%%% The magnetic version of Tighten.
%%%

tighten(Magnet, St) ->
    Flags = wings_magnet:flags(Magnet, []),
    wings_drag:fold(fun(Vs, We) ->
                            tighten_vs(Vs, We, Magnet)
                    end, [percent,falloff], Flags, St).

tighten_vs(Vs, We, Magnet) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    magnet_move(Tv, Magnet, We);
tighten_vs(Vs, We, Magnet) ->
    tighten_vs(gb_sets:to_list(Vs), We, Magnet).

magnet_move(Tv, Magnet0, We) ->
    Vs = lists:append([Vs || {_,Vs} <- Tv]),
    {VsInf,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs, We),
    Vec = magnet_tighten_vec(Affected, We, []),
    {Affected,wings_move:magnet_move_fun(Vec, VsInf, Magnet)}.

magnet_tighten_vec([V|Vs], We, Acc) ->
    Vec = tighten_vec(V, We),
    magnet_tighten_vec(Vs, We, [{V,Vec}|Acc]);
magnet_tighten_vec([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).
    
%%%
%%% The Dissolve command. Like Collapse, but stays in vertex mode
%%% (without any selection).
%%%

dissolve(St0) ->
    St = wings_collapse:collapse(St0),
    St#st{selmode=vertex,sel=[]}.

%%%
%%% Set vertex color.
%%%

set_color(Color, St) ->
    wings_sel:map(fun(Vs, We) ->
			  wings_va:set_vertex_color(Vs, Color, We)
		  end, St).
