%%%-------------------------------------------------------------------
%%% File    : auv_placement.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Algotrihms for placing charts on texture.
%%%
%%% Created :  7 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id$

-module(auv_placement).

-include_lib("wings/src/wings.hrl").
-include("auv.hrl").

-export([place_areas/1,rotate_area/2,group_edge_loops/2]).

-import(lists, [max/1,sort/1,map/2,reverse/1]).

%% Returns a gb_tree with areas...
place_areas(Areas0) ->
    Rotate = fun(#we{id=Id,name=Ch0}=We0, BBs) ->
		     {{Dx,Dy}=Size,Vs} = center(We0),
		     Ch = Ch0#ch{size=Size},
		     We = We0#we{vp=array:from_orddict(Vs),name=Ch},
		     {We,[{Dx,Dy,Id}|BBs]}
	     end,
    {Areas1,Sizes0} = lists:mapfoldl(Rotate, [], Areas0),
%    ?DBG("~p~n",[Sizes0]),    
    {Positions0, Max} = fill(Sizes0, [0,0]),
%    ?DBG("~p~n",[Positions0]),
    Scale  = 1 / max(Max),
    move_and_scale_charts(Areas1, lists:sort(Positions0), Scale, []).

center(#we{vp=VTab}) ->
    VL = array:sparse_to_orddict(VTab),
    {{_,Xmin},{_,Xmax},{_,Ymin},{_,Ymax}} = auv_util:maxmin(VL),
    Dx = Xmax - Xmin,
    Dy = Ymax - Ymin,
    CX = Xmin + Dx / 2,
    CY = Ymin + Dy / 2,
    Vs = auv_util:moveAndScale(VL, -CX, -CY, 1, []),
    {{Dx,Dy}, Vs}.

fill(Areas, [0,0]) ->  %% First time
    Map = fun({W,H,Id}) when W > H ->
		     {{W,width}, H, Id};
	     ({W,H,Id}) ->
		  {{H,height}, W, Id}
	  end,
    SL0 = sort(map(Map, Areas)),
    [First|SL] = reverse(SL0),
    {Res,PX,PY} = insert(First, 0,0),
    fill(SL, PX,PY, [Res]);
fill(Areas, [PX,PY]) ->
    Map = fun({W,H,Id}) when W > H ->
		     {{W,width}, H, Id};
		({W,H,Id}) ->
		     {{H,height}, W, Id}
	     end,
    SL0 = sort(map(Map, Areas)),
    SL = reverse(SL0),
    fill(SL, PX,PY, []).

fill([], MaxX, MaxY, Res) ->
    {Res, [MaxX,MaxY]};
fill([Biggest|SL], MX0, MY0, Res0) ->
    {W0,H0} = case Biggest of {{W,width},H,_} -> {W,H}; {{H,height},W,_} -> {W,H} end,
    %% Calc possible places
    A1 = max([(W0 + MX0), max([H0,MY0])]),  %% Build Element to the right
    A2 = max([max([W0, MX0]), (MY0 + H0)]), %% Build Element on the top 
    if 
	A1 < A2 ->
	    {New, MX1, MY1} = insert(Biggest, MX0, 0),
	    if MY0 >= MY1 ->
		    {SL2, Res1} = fill_area(SL, W0,MY0-H0, MX0,MY1, [], [New|Res0]), 
		    fill(SL2, MX1, MY0, Res1);
	       MY1 > MY0 ->
		    {SL2, Res1} = fill_area(SL, MX0,MY1-MY0, 0,MY0, [], [New|Res0]), 
		    fill(SL2, MX1, MY1, Res1)
	    end;
	true ->	    
	    {New, MX1, MY1} = insert(Biggest, 0, MY0),
	    if MX0 >= MX1 ->
		    {SL2, Res1} = fill_area(SL, MX0-W0,H0, MX1,MY0, [], [New|Res0]), 
		    fill(SL2, MX0, MY1, Res1);
	       MX1 > MX0 ->
		    {SL2, Res1} = fill_area(SL, MX1-MX0,MY0, MX0,0, [], [New|Res0]), 
		    fill(SL2, MX1, MY1, Res1)
	       end
    end.

insert({{X,width}, Y, I},XP,YP) ->
    New = {I, {XP+X/2, YP+Y/2}},
    {New, XP+X, YP+Y};
insert({{Y,height}, X, I},XP,YP) ->
    New = {I, {XP+X/2, YP+Y/2}},
    {New, XP+X, YP+Y}.

fill_area([Sel = {{X0,width},Y0,_}|SL], MX, MY, XP,YP, UU, Res0) 
  when X0 =< MX, Y0 =< MY ->
    {New,_,_} = insert(Sel,XP,YP),
    {SL2, Res1}= fill_area(SL, MX-X0, MY, XP+X0, YP, [], Res0),
    fill_area(SL2, X0, MY-Y0, XP, YP+Y0, UU, [New|Res1]);
fill_area([Sel={{Y0,height},X0,_}|SL], MX, MY, XP,YP,UU, Res0) 
  when X0 =< MX, Y0 =< MY ->
    {New,_,_} = insert(Sel,XP,YP),
    {SL2, Res1} = fill_area(SL, MX,MY-Y0, XP, YP+Y0,[], Res0),
    fill_area(SL2, MX-X0, Y0, XP+X0, YP, UU, [New|Res1]);
fill_area([Nouse|SL], MaxX, MaxY, XP,YP, Unused, Res) ->
    fill_area(SL,MaxX,MaxY, XP,YP, [Nouse|Unused], Res);
fill_area([], _,_, _,_, Unused,Res) ->
    {reverse(Unused), Res}.

%%%%%%%%%%%%%%%%
move_and_scale_charts([We0|RA], [{C,{Cx,Cy}}|RP], S, Acc) ->
    Transform0 = e3d_mat:scale(S, S, 0.0),
    Transform = e3d_mat:mul(e3d_mat:translate(S*Cx, S*Cy, 0.0), Transform0),
    We = wings_we:transform_vs(Transform, We0),
    move_and_scale_charts(RA, RP, S, [{C,We}|Acc]);
move_and_scale_charts([], [], _, Acc) -> Acc.

rotate_area(Vs, #we{vp=Orig}=We) ->
    VTab = array:from_orddict(lists:sort(Vs)),
    Fs = wings_we:visible(We),
    [{_,Eds3}|_] = group_edge_loops(Fs,We),
    Eds4 = make_convex(reverse(Eds3), [], VTab),
    [#be{vs=LV1,ve=LV2,dist=_Dist}|_] = lists:reverse(lists:keysort(5, Eds4)),
    LV1P = array:get(LV1, VTab),
    LV2P = array:get(LV2, VTab),
    O1 = array:get(LV1, Orig),
    O2 = array:get(LV2, Orig),
    Normal = {NX,NY,NZ} = 
	try auv_mapping:chart_normal(Fs,We) catch throw:_ -> {0.0,0.0,1.0} end,
    ANX = abs(NX), ANY = abs(NY), ANZ = abs(NZ),
    Csys = if ANX > ANY, ANX > ANZ ->  csys(NX, Normal, {0.0,0.0,-1.0});
	      ANZ > ANY -> csys(NZ, Normal, {1.0,0.0,0.0});
	      true -> csys(NY, Normal, {1.0,0.0,0.0})
	   end,
    O11 = mul_point(O1, Csys), 
    O12 = mul_point(O2, Csys),
    RealAngle = math:atan2(element(2,O12)-element(2,O11),
			   element(1,O12)-element(1,O11)),
    Angle = math:atan2(element(2,LV2P)-element(2,LV1P),
 		       element(1,LV2P)-element(1,LV1P)),
    Rotate = Angle - RealAngle,    
    ?DBG("Angle ~p~n  P1 ~p~n  P2 ~p~n",  
 	 [Angle*180/math:pi(), 
 	  {auv_segment:map_vertex(LV1, (We#we.name)#ch.vmap), LV1P}, 
 	  {auv_segment:map_vertex(LV2, (We#we.name)#ch.vmap), LV2P}]),
    ?DBG("Real ~p ~p~n  ~p~n  RAngle ~p  => ~p~n",
 	 [O1,O2,{O11,O12},RealAngle*180/math:pi(),Rotate*180/math:pi()]),
    Rot = e3d_mat:rotate(-(Rotate*180/math:pi()), {0.0,0.0,1.0}),
    Res = [{Id,e3d_mat:mul_point(Rot, Vtx)} || {Id,Vtx} <- Vs],
    %%	    ?DBG("Rot angle ~p ~p~n", [Angle*180/math:pi(), Res]),
    Res.

mul_point(P, {X,Y,Z}) ->
    {e3d_vec:dot(P,X), e3d_vec:dot(P,Y),e3d_vec:dot(P,Z)}.

csys(Dir0,Z,X0) ->
    X1 = if Dir0 > 0.0 -> X0;
	    true ->e3d_vec:neg(X0)
	 end,
    Y = e3d_vec:cross(Z,X1),
    X = e3d_vec:cross(Y,Z),
    {e3d_vec:norm(X),e3d_vec:norm(Y),Z}.
		
-define(PI, 3.141592).
-define(ALMOSTPI, (?PI-(0.5/180*?PI))). %% cluster together straight lines

make_convex([This, Next|Rest], Acc, Vs) ->
    case calc_dir(This,Next,Vs) >= ?ALMOSTPI of
	true ->
	    New = #be{vs=This#be.vs, ve=Next#be.ve, 
		      edge=[This#be.edge,Next#be.edge],
		      dist=dist(This#be.vs,Next#be.ve,Vs)}, 
	    if Acc == [] ->
		    make_convex([New|Rest], Acc, Vs);
	       true ->
		    make_convex([hd(Acc),New|Rest], tl(Acc), Vs)
	    end;
	false ->
	    make_convex([Next|Rest], [This|Acc], Vs)
    end;
make_convex([This],Acc, Vs) ->
    [Next|Acc2] = lists:reverse(Acc),
    case calc_dir(This,Next,Vs) >= ?ALMOSTPI of
	true ->
	    New = #be{vs=This#be.vs, ve=Next#be.ve,
		      edge=[This#be.edge,Next#be.edge],
		      dist=dist(This#be.vs,Next#be.ve,Vs)},
	    Acc3 = reverse(Acc2),
	    make_convex([hd(Acc3),New], tl(Acc3), Vs);
	false ->
	    [This|Acc]
    end.
	          
%% Group edgeloops and return a list sorted by total dist.
%% [{TotDist, [{V1,V2,Edge,Dist},...]}, ...]
group_edge_loops(Fs, We = #we{name=#ch{emap=Emap}}) ->
    case auv_util:outer_edges(Fs, We, false) of
	[] -> [];
	Eds1 ->
	    Info = fun({Edge,Face},Tree) ->
			   Cut = gb_trees:is_defined(Edge,Emap),
			   case array:get(Edge, We#we.es) of
			       #edge{vs=V1,ve=V2,lf=Face} ->
				  Dist = dist(V1,V2,We#we.vp),
				  Be = #be{vs=V1,ve=V2,edge=Edge,face=Face,
					   cut=Cut,dist=Dist},
				   gb_trees:insert(V1,Be,Tree);
			      #edge{vs=V2,ve=V1,rf=Face} ->
				  Dist = dist(V1,V2,We#we.vp),
				  Be = #be{vs=V1,ve=V2,edge=Edge,face=Face,
					   cut=Cut,dist=Dist},
				   gb_trees:insert(V1,Be,Tree)
			  end
		   end,
	    Eds  = lists:foldl(Info, gb_trees:empty(), Eds1),
	    Loops = sort_edges(Eds),
%%	    io:format("Cuts ~p ~n",[Loops]),
	    Add = fun(#be{dist=Dist}, Acc) ->  Acc + Dist end,
	    SumLoops = [{lists:foldl(Add, 0, Loop), Loop} 
			|| Loop <- Loops],
	    lists:reverse(lists:sort(SumLoops))
    end.

calc_dir(#be{vs=V11,ve=V12},#be{vs=V12,ve=V22}, Vs) ->    
    C  = array:get(V12, Vs),
    V1 = array:get(V11, Vs),
    V2 = array:get(V22, Vs),
    {X1,Y1,_} = e3d_vec:sub(V1, C),
    {X2,Y2,_} = e3d_vec:sub(V2, C),
    Angle = case (math:atan2(Y1,X1) - math:atan2(Y2,X2)) of 
		A when A >= 0.0 ->
		    A;
		A -> 
		    2 * math:pi() + A
	    end,
%    ?DBG("Angle Vertex ~p Edges ~w : ~p-~p = ~p ~n",
%	[V12,{_E1,_E2},math:atan2(Y1,X1), math:atan2(Y2,X2),Angle]),
    Angle.
 
dist(V1, V2, Vs) ->
    e3d_vec:dist(array:get(V1, Vs), array:get(V2, Vs)).

%% Returns a list of loops 
sort_edges(Eds) ->
    {_V1, BE=#be{ve=V2}, EdsT0} = gb_trees:take_smallest(Eds),
    sort_edges(V2, EdsT0, [[BE]]).

sort_edges(V21, EdsT0, All = [Current|Acc]) ->
    case gb_trees:lookup(V21, EdsT0) of
	{value, BE = #be{ve=V22}} -> 
	    sort_edges(V22, gb_trees:delete(V21,EdsT0), 
		       [[BE|Current]|Acc]);
	none ->	    
	    case catch gb_trees:take_smallest(EdsT0) of
		{_, BE = #be{ve=V2}, EdsT1} ->
		    sort_edges(V2, EdsT1, [[BE]|All]);
		{'EXIT', _} -> %% Stop
		    All
	    end
    end.
