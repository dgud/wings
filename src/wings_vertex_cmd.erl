%%
%%  wings_vertex_cmd.erl --
%%
%%     This module contains most of the commands for vertices.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_vertex_cmd.erl,v 1.65 2006/03/16 12:50:59 giniu Exp $
%%

-module(wings_vertex_cmd).
-export([menu/3,command/2,tighten/3,tighten/4,
	 connect/2,bevel_vertex/2, flatten/2]).

-export([set_color/2]).

-include("wings.hrl").
-import(lists, [member/2,keymember/3,foldl/3,mapfoldl/3,
		reverse/1,last/1,sort/1]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{basic,{?STR(menu,1,"Vertex operations"),ignore}},
	    {basic,separator},
	    {?STR(menu,2,"Move"),{move,Dir},[],[magnet]},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    separator,
	    {?STR(menu,3,"Extrude"),{extrude,Dir}},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    {?STR(menu,4,"Connect"),connect,
	     ?STR(menu,5,"Create a new edge by connecting selected vertices")},
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

%% Vertex menu.
command({flatten,Plane}, St) ->
    flatten(Plane, St);
command(connect, St) ->
    {save_state,connect(St)};
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
    {St,Tvs} = wings_sel:mapfold(
		 fun(Vs, We0, Acc) ->
			 extrude_vertices(Vs, We0, Acc)
		 end, [], St0),
    wings_move:plus_minus(Type, Tvs, St).

extrude_vertices(Vs, We0, Acc) ->
    We = foldl(fun(V, A) ->
		       ex_new_vertices(V, We0, A)
	       end, We0, gb_sets:to_list(Vs)),
    NewVs = wings_we:new_items_as_ordset(vertex, We0, We),
    {We,[{Vs,NewVs,gb_sets:empty(),We}|Acc]}.

ex_new_vertices(V, OrigWe, #we{vp=Vtab}=We0) ->
    Center = wings_vertex:pos(V, We0),
    {We,VsFaces} =
	wings_vertex:fold(
	  fun(Edge, Face, Rec, {W0,Vs}) ->
		  OtherV = wings_vertex:other(V, Rec),
		  R = edge_ratio(OtherV, OrigWe),
		  Pos0 = gb_trees:get(OtherV, Vtab),
		  Dir = e3d_vec:sub(Pos0, Center),
		  Pos = e3d_vec:add(Center, e3d_vec:mul(Dir, R)),
		  {W,NewV} = wings_edge:fast_cut(Edge, Pos, W0),
		  {W,[NewV,Face|Vs]}
	  end, {We0,[]}, V, We0),
    ex_connect(VsFaces, VsFaces, We).

edge_ratio(V, #we{vp=Vtab}) ->
    case gb_trees:is_defined(V, Vtab) of
	false -> 1/3;
	true -> 0.25
    end.

ex_connect([Va,Face|[Vb|_]=T], More, We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    ex_connect(T, More, We);
ex_connect([Va,Face], [Vb|_], We0) ->
    {We,_} = wings_vertex:force_connect(Va, Vb, Face, We0),
    We.

%%%
%%% The Bevel command.
%%%

bevel(St0) ->
    {St,{Tvs0,FaceSel}} =
	wings_sel:mapfold(
	  fun(VsSet, We, A) ->
		  bevel_1(VsSet, We, A)
	  end, {[],[]}, St0),
    {Min,Tvs} = bevel_normalize(Tvs0),
    wings_drag:setup(Tvs, [{distance,{0.0,Min}}],
		     wings_sel:set(face, FaceSel, St)).

bevel_vertex(V, We0) ->
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 -> We0;
	NumEdges ->
	    {We,_,_} = bevel_vertex_1(V, Es, NumEdges, [], We0, []),
	    We
    end.

bevel_1(VsSet, #we{id=Id}=We0, {Tvs,Fa}) ->
    Vs = gb_sets:to_list(VsSet),
    {We,Tv,Fs0} = bevel_vertices(Vs, VsSet, We0, We0, [], []),
    FaceSel = case Fs0 of
		  [] -> Fa;
		  _ -> [{Id,gb_sets:from_list(Fs0)}|Fa]
	      end,
    {We,{[{Id,Tv}|Tvs],FaceSel}}.

bevel_vertices([V|Vs], VsSet, WeOrig, We0, Acc0, Facc) ->
    Adj = adjacent(V, VsSet, WeOrig),
    Es = wings_vertex:fold(
	   fun(Edge, Face, Rec, Acc) ->
		   [{Edge,Face,Rec}|Acc]
	   end, [], V, We0),
    case length(Es) of
	2 ->					%Winged vertex - ignore.
	    bevel_vertices(Vs, VsSet, WeOrig, We0, Acc0, Facc);
	NumEdges ->
	    {We,Acc,Face} = bevel_vertex_1(V, Es, NumEdges, Adj, We0, Acc0),
	    bevel_vertices(Vs, VsSet, WeOrig, We, Acc, [Face|Facc])
    end;
bevel_vertices([], _, _, We, Acc, Facc) -> {We,Acc,Facc}.

bevel_vertex_1(V, Es, NumEdges, Adj, We0, Vec0) ->
    {InnerFace,We1} = wings_we:new_id(We0),
    {Ids,We2} = wings_we:new_wrap_range(NumEdges, 2, We1),
    #we{es=Etab0,vc=Vct0,vp=Vtab0,fs=Ftab0} = We1,
    {Vct,Vtab} = bevel_vertices_1(V, Ids, NumEdges, Vct0, Vtab0),
    {_,Etab,Vec} = foldl(
		     fun(E, {Ids0,Etab1,Vs0}) ->
			     {Etab,Vec} = bevel(V, E, InnerFace, Ids0,
						Adj, Vtab0, Etab0, Etab1),
			     {wings_we:bump_id(Ids0),Etab,[Vec|Vs0]}
		     end, {Ids,Etab0,Vec0}, Es),
    Mat = bevel_material(Es, We2),
    NewEdge = wings_we:id(1, Ids),
    Ftab = gb_trees:insert(InnerFace, NewEdge, Ftab0),
    We = wings_facemat:assign(Mat, [InnerFace], We2),
    {We#we{es=Etab,fs=Ftab,vc=Vct,vp=Vtab},Vec,InnerFace}.

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

bevel(V, {Edge,Face,Rec0}, InnerFace, Ids, Adj, Vtab, OrigEtab, Etab0) ->
    Vprev = wings_we:id(0, Ids),
    Eprev = wings_we:id(1, Ids),
    Va = wings_we:id(2, Ids),
    Ecurr = wings_we:id(3, Ids),
    Vb = wings_we:id(4, Ids),
    Enext = wings_we:id(5, Ids),
    {Rec,Curr} =
	case Rec0 of
	    #edge{vs=V,ve=Vother,rf=Face,rtpr=ColEdge} ->
		Col = bevel_color(ColEdge, Face, OrigEtab),
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Eprev},
		 Rec0#edge{vs=Vb,ve=Va,a=Col,lf=InnerFace,
			   rtsu=Edge,ltpr=Eprev,ltsu=Enext}};
	    #edge{vs=V,ve=Vother,rf=Of,rtpr=ColEdge} ->
		Col = bevel_color(ColEdge, Of, OrigEtab),
		{Rec0#edge{vs=Va,rtpr=Ecurr,ltsu=Enext},
		 Rec0#edge{vs=Vprev,ve=Va,a=Col,lf=InnerFace,
			   rtsu=Edge,ltpr=Enext,ltsu=Eprev}};
	    #edge{ve=V,vs=Vother,lf=Face,ltpr=ColEdge} ->
		Col = bevel_color(ColEdge, Face, OrigEtab),
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Eprev},
		 Rec0#edge{vs=Va,ve=Vb,b=Col,rf=InnerFace,
			   ltsu=Edge,rtpr=Eprev,rtsu=Enext}};
	    #edge{ve=V,vs=Vother,lf=Of,ltpr=ColEdge} ->
		Col = bevel_color(ColEdge, Of, OrigEtab),
		{Rec0#edge{ve=Va,ltpr=Ecurr,rtsu=Enext},
		 Rec0#edge{vs=Va,ve=Vprev,b=Col,rf=InnerFace,
			   ltsu=Edge,rtpr=Enext,rtsu=Eprev}}
	end,
    Etab = gb_trees:update(Edge, Rec, Etab0),
    Vpos = gb_trees:get(V, Vtab),
    Vec = bevel_vec(Adj, Vother, Vpos, Vtab),
    {gb_trees:insert(Ecurr, Curr, Etab),{Vec,Va}}.

bevel_vec(Adj, Vother, Vpos, Vtab) ->
    Opos = gb_trees:get(Vother, Vtab),
    case member(Vother, Adj) of
	true ->
	    e3d_vec:sub(e3d_vec:average([Opos,Vpos]), Vpos);
	false ->
	    e3d_vec:sub(Opos, Vpos)
    end.

bevel_vertices_1(V, Ids, N, Vct0, Vtab0) ->
    Pos = gb_trees:get(V, Vtab0),
    Vct = gb_trees:delete(V, Vct0),
    Vtab = gb_trees:delete(V, Vtab0),
    bevel_new_vertices(Ids, N, Pos, Vct, Vtab).

bevel_new_vertices(Ids, N, Pos, Vct0, Vtab0) when N > 0 ->
    V = Id = wings_we:id(0, Ids),
    Edge = Id + 1,
    Vct = gb_trees:insert(V, Edge, Vct0),
    Vtab = gb_trees:insert(V, Pos, Vtab0),
    bevel_new_vertices(wings_we:bump_id(Ids), N-1, Pos, Vct, Vtab);
bevel_new_vertices(_, _, _, Vct, Vtab) -> {Vct,Vtab}.

bevel_normalize(Tvs) ->
    bevel_normalize(Tvs, 1.0E207, []).

bevel_normalize([{Id,VecVs0}|Tvs], Min0, Acc) ->
    {VecVs,Min} = bevel_normalize_1(VecVs0, Min0),
    bevel_normalize(Tvs, Min, [{Id,VecVs}|Acc]);
bevel_normalize([], Min, Tvs) -> {Min,Tvs}.

bevel_normalize_1(VecVs, Min0) ->
    mapfoldl(fun({Vec,V}, M0) ->
		     Min = case e3d_vec:len(Vec) of
			       Len when Len < M0 -> Len;
			       _Len -> M0
			   end,
		     {{e3d_vec:norm(Vec),[V]},Min}
	     end, Min0, VecVs).

bevel_color(ColEdge, Face, Etab0) ->
    case gb_trees:get(ColEdge, Etab0) of
	#edge{lf=Face,a=Col} -> Col;
	#edge{rf=Face,b=Col} -> Col
    end.

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
%%% The Connect command.
%%%

connect(St) ->
    wings_sel:map(fun connect/2, St).

connect(Vs0, #we{mirror=MirrorFace}=We) ->
    FaceVs = wings_vertex:per_face(Vs0, We),
    foldl(fun({Face,_}, Acc) when Face =:= MirrorFace -> Acc;
	     ({Face,Vs}, Acc) -> wings_vertex:connect(Face, Vs, Acc)
	  end, We, FaceVs).

%%%
%%% The Tighten command.
%%%

tighten(St) ->
    Tvs = wings_sel:fold(fun tighten/3, [], St),
    wings_drag:setup(Tvs, [percent], St).

tighten(Vs, #we{id=Id}=We, Acc) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    [{Id,Tv}|Acc];
tighten(Vs, We, Acc) -> 
    tighten(gb_sets:to_list(Vs), We, Acc).

tighten_vec(V, #we{vp=Vtab,mirror=MirrorFace}=We) ->
    Cs = wings_vertex:fold(
	   fun(_, Face, _, A) when Face =/= MirrorFace ->
		   FaceVs = wings_face:to_vertices([Face], We),
		   C = wings_vertex:center(FaceVs, We),
		   [C|A];
	      (_, _, _, A) -> A
	   end, [], V, We),
    Center = e3d_vec:average(Cs),
    e3d_vec:sub(Center, gb_trees:get(V, Vtab)).

%%%
%%% The magnetic version of Tighten.
%%%

tighten(Magnet, St) ->
    Tvs = wings_sel:fold(fun(Vs, We, Acc) ->
				 tighten(Vs, We, Magnet, Acc)
			 end, [], St),
    Flags = wings_magnet:flags(Magnet, []),
    wings_drag:setup(Tvs, [percent,falloff], Flags, St).

tighten(Vs, We, Magnet, Acc) when is_list(Vs) ->
    Tv = foldl(
	   fun(V, A) ->
		   Vec = tighten_vec(V, We),
		   [{Vec,[V]}|A]
	   end, [], Vs),
    magnet_move(Tv, Magnet, We, Acc);
tighten(Vs, We, Magnet, Acc) -> 
    tighten(gb_sets:to_list(Vs), We, Magnet, Acc).

magnet_move(Tv, Magnet0, #we{id=Id}=We, Acc) ->
    Vs = lists:append([Vs || {_,Vs} <- Tv]),
    {VsInf,Magnet,Affected} = wings_magnet:setup(Magnet0, Vs, We),
    Vec = magnet_tighten_vec(Affected, We, []),
    [{Id,{Affected,wings_move:magnet_move_fun(Vec, VsInf, Magnet)}}|Acc].

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
			  set_color_1(gb_sets:to_list(Vs), Color,
				      We#we{mode=vertex})
		  end, St).

set_color_1([V|Vs], Color, #we{es=Etab0}=We) ->
    Etab = wings_vertex:fold(
	     fun(Edge, _Face, Rec0, Es) ->
		     Rec = case Rec0 of
			       #edge{vs=V} -> Rec0#edge{a=Color};
			       #edge{ve=V} -> Rec0#edge{b=Color}
			   end,
		     gb_trees:update(Edge, Rec, Es)
	     end, Etab0, V, We),
    set_color_1(Vs, Color, We#we{es=Etab});
set_color_1([], _, We) -> We.
