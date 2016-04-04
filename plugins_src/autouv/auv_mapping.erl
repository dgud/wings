%%
%%  auv_mapping.erl --
%%
%%     The UV parametrisation algorithms.
%%
%%  Copyright (c) 2002-2011 Dan Gudmundsson, Raimo Niskanen, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%%%%%% Least Square Conformal Maps %%%%%%%%%%%%
%% Algorithms based on the paper, 
%% (now probably totally ruined by me or Raimo)
%% 'Least Square Conformal Maps for Automatic Texture Generation Atlas'
%% by Bruno Levy, Sylvain Petitjean, Nicolas Ray, Jerome Mailot
%% Presented on Siggraph 2002
%%
%%%% The Conjugate Gradient Method (trad)
%% Algorithms based on the paper:
%%       An Introduction to 
%%  the Conjugate Gradient Method
%%    Without the Agonizing Pain
%% by
%%  Jonathan Richard Shewchuk, March 7, 1994
%%
%% The Column Norm Preconditioning was stumbled upon, just briefly
%% mentioned, in the paper:
%%      Incomplete Factorization Preconditioning
%%        for Linear Least Squares Problems
%% by
%%      Xiaoge Wang, 1994

%% All credits about the LSQCM implementation goes to Raimo, who
%% implemented the lot. 

-module(auv_mapping).

-export([stretch_opt/2, fs_area/2, area2d2/3,area3d/3, calc_area/3]).
-export([map_chart/3, projectFromChartNormal/2, chart_normal/2]).

%% Internal exports. 
-export([model_l2/5]).
-export([lsq/2, lsq/3,  % Debug entry points
	 find_pinned/2,
	 find_pinned_from_edges/2,
	 split_edges_1/2]).

-include_lib("wings/src/wings.hrl").
-include("auv.hrl").
-include_lib("wings/e3d/e3d.hrl").

-import(lists, [foldl/3,reverse/1]).

map_chart(Type, We, Options) ->
    Faces = wings_we:visible(We),
    case catch auv_placement:group_edge_loops(Faces, We) of
	[] ->
	    {error,?__(1,"A closed surface cannot be mapped. "
	     "(Either divide it into into two or more charts, "
	     "or cut it along some edges.)")};
	[{_,[_,_]}] ->
	    {error,?__(2,"A cut in a closed surface must consist of at least two edges.")};
	_ when Type == lsqcm, is_list(Options), length(Options) < 2 ->
	    {error,?__(3,"At least 2 vertices (per chart) must be selected")};
	[Best|_] ->
	    map_chart_1(Type, Faces, Best, Options, We);	
	Err ->
	    io:format(?__(4,"Error:")++" ~p~n", [Err]),
	    {error, ?__(5,"Internal Error")}
    end.

map_chart_1(Type, Chart, Loop, Options, We) ->
    try map_chart_2(Type, Chart, Loop, Options, We)
    catch error:{badarith,_} ->
	    {error,?__(1,"Numeric problem, probably a bad face with an empty area.")};
	throw:What ->
	    {error,lists:flatten(What)};
	_:Reason ->
	    Msg = io_lib:format(?__(2,"Internal error:")++" ~P", [Reason,10]),
	    io:format("~p:~p "++?__(3,"Error")++" ~p~n  ~p ~n",
		      [?MODULE,?LINE,Reason,erlang:get_stacktrace()]),
	    {error,lists:flatten(Msg)}
    end.

map_chart_2(project, C, _, _, We) ->    projectFromChartNormal(C, We);
map_chart_2(camera, C, _, Dir, We) ->   projectFromCamera(C, Dir, We);
map_chart_2(lsqcm, C, Loop, Pinned, We) -> lsqcm(C, Pinned, Loop, We);
map_chart_2(Op,C, Loop, Pinned, We) ->  volproject(Op,C, Pinned, Loop,We).

volproject(Type,Chart,_Pinned,{_,BEdges},We) ->
    {Center,Axes,LoopInfo} = find_axes(Chart,BEdges,We),
    %%io:format("Res: ~p ~n",[{Center,Axes}]),
    Rot = rot_mat(Axes),
    CalcUV = case Type of 
		 cyl -> fun cyl/1; 
		 sphere -> fun sphere/1
	     end,    
    Vs0 = wings_face:to_vertices(Chart, We),
    Transform = fun(V) ->
			Pos = wings_vertex:pos(V, We),
			Vec = e3d_vec:sub(Pos,Center),
			e3d_mat:mul_vector(Rot,Vec)
		end,
    Vs1 = lists:sort([{V,Transform(V)} || V <- Vs0]),
    Tagged = leftOrRight(LoopInfo, Chart, We#we{vp=array:from_orddict(Vs1)}),
    %%io:format("Tagged ~w~n",[gb_sets:to_list(Tagged)]),
    [{V,fix_positions(V,Pos,CalcUV(Pos),Tagged)} || {V,Pos} <- Vs1].

sphere({X,Y,Z}) ->    
    S = catchy(catch math:atan2(X,Z)/math:pi()),	
    T = math:acos(clamp(-Y))/math:pi()-0.5,
    {S,T,0.0}.

cyl({X,Y,Z}) ->
    S = catchy(catch math:atan2(X,Z)/math:pi()),
    T = Y,
    {S,T,0.0}.

catchy({'EXIT', _}) -> math:pi()/4;
catchy(X) -> X.    

clamp(X) when X > 1.0 -> 1.0;
clamp(X) when X < -1.0 -> -1.0;
clamp(X) -> X.

fix_positions(_V,{_,_,Z},Proj,_) when Z > 0.0 -> Proj;
fix_positions(V,_,Proj = {X,Y,Z},Tags) ->
    case gb_sets:is_member(V,Tags) of
	true when X > 0.0 -> 
	    {X-2.0,Y,Z};
	false when X < 0.0 ->
	    {X+2.0,Y,Z};
	_ ->
	    Proj
    end.

leftOrRight({LL,LR}, Free0, We) ->
    Del = fun(#be{face=F},{Fs,Ch}) -> {[F|Fs],gb_sets:delete_any(F,Ch)} end,
    {F1,Free1} = foldl(Del,{[],gb_sets:from_list(Free0)},LL),
    {F2,Free}  = foldl(Del,{[],Free1},LR),
    [Fs1,Fs2] = expand_faces([F1,F2],Free,[],[F1,F2],[],We),
    Set1 = wings_vertex:from_faces(Fs1,We),
    Set2 = wings_vertex:from_faces(Fs2,We),
    case wings_vertex:center(Set1,We) > wings_vertex:center(Set2,We) of
	true  -> gb_sets:from_ordset(Set2);
	false -> gb_sets:from_ordset(Set1)
    end.

expand_faces([Fs0|Rest],Free0,New,[Set|Acc1],Tot,We) ->
    {NewFs,Free} = foldl(fun(Face, A) ->
				 do_face_more(Face, We, A)
			 end, {[],Free0}, Fs0),
    expand_faces(Rest,Free,[NewFs|New],Acc1,[NewFs++Set|Tot],We);
expand_faces([],Free,New,[],Tot,We) ->
    case gb_sets:is_empty(Free) of
	true -> Tot;
	false -> expand_faces(reverse(New),Free,[],reverse(Tot),[],We)
    end.

do_face_more(Face, We, Acc) ->
    wings_face:fold(fun(_,_,#edge{lf=LF,rf=RF},P={A1,Free}) ->
			    AFace = if LF == Face -> RF; true -> LF end,
			    case gb_sets:is_member(AFace,Free) of
				true -> 
				    {[AFace|A1],
				     gb_sets:delete(AFace,Free)};
				false ->
				    P
			    end
		    end, Acc, Face,We).

rot_mat({{Ux,Uy,Uz},{Vx,Vy,Vz},{Wx,Wy,Wz}}) ->
    {Ux,Vx,Wx,
     Uy,Vy,Wy,
     Uz,Vz,Wz,
     0.0,0.0,0.0}.

find_axes(Fs,BEdges,We) ->
    ChartNormal = chart_normal(Fs,We),
    case forms_closed_object(BEdges,ChartNormal,We) of
	undefined ->
	    throw(
	      ?__(1,"I currently can't sphere/cylinder map this type of chart/cuts,\n"
	      "I can't figure out which axes you want as X,Y, and Z,\n"
	      "please use unfolding or one of the projection mappings."));

%%	    find_axes_from_eigenv(Fs,ChartNormal,BEdges,We);
	Nice -> 
	    Nice
    end.

forms_closed_object(BEdges0,ChartNormal,We=#we{name=#ch{emap=Emap}}) ->
    BEdges = [{auv_segment:map_edge(Edge,Emap),BE} || BE = #be{edge=Edge} <- BEdges0],
    case is_an_8(BEdges, false) of
	false -> undefined;
	Edge -> 
	    {North,South,Link,LinkR} = split_edges(Edge,BEdges,We),
	    NorthSouth = e3d_vec:sub(North,South),
	    Center = e3d_vec:average(North,South),
	    %%io:format("Temp: ~p ~n",[{North,South,Center}]),
	    LC = center(Link,We),
	    LCdir0 = e3d_vec:sub(LC,Center),
	    LCdir = case e3d_vec:len(LCdir0) > 0.0005 of
			true -> LCdir0;
			false -> e3d_vec:neg(ChartNormal)
		    end,
	    {Center,calc_axis(NorthSouth,LCdir),{Link,LinkR}}
    end.

center(Bes,We) ->
    Eds = lists:map(fun(#be{edge=E}) -> E end, Bes),
    Vs = wings_vertex:from_edges(Eds,We),
    wings_vertex:center(Vs,We).

calc_axis(Y0,Z0) ->
    Y = e3d_vec:norm(Y0),
    X = e3d_vec:norm(e3d_vec:cross(e3d_vec:norm(Z0),Y)),
    Z = e3d_vec:norm(e3d_vec:cross(X,Y)),
    {X,Y,Z}.

is_an_8([],E) ->
    E;
is_an_8([{E,_},{E,_}|R],_) -> %% Skip these
    is_an_8(R, E);
is_an_8([{E,_}|R],HaveRemoved) -> %% Hmm we must take them in order
    case lists:keysearch(E,1,R) of %% O(N2) I know..
	false -> is_an_8(R,HaveRemoved);
	_ when HaveRemoved =/= false ->
	    E;
	_ ->
	    case reverse(R) of
		[{E,_}|R2] ->
		    is_an_8(reverse(R2), E);
		_ -> E
	    end
    end.

%%  Split edges splits into three parts two loops 
%%  and a link between them.
%%    bc  
%%    _ defg 
%%  a/ \____h
%%   \_/--\_|     => 2 loops: mnoabc fghijk
%%   onmdekji     =>    link: def
%% 
%% d(L) -> %% DBG
%%     lists:map(fun({E,_BE}) -> E end,L).

getEs(L) ->
    lists:map(fun({_E,BE}) -> BE end,L).

split_edges(Edge,Bes,We) ->
    {L1,L2,Link} = split_edges_1(Edge,Bes),
    %% Reorder so that the pinned vertices are longest from each other
    North = case L1 of
		[] -> wings_vertex:pos((hd(Link))#be.vs,We);
		_  -> center(L1,We)
	    end,
    South = case L2 of
		[] -> wings_vertex:pos((lists:last(Link))#be.ve,We);
		_  -> center(L2,We)
	    end,
    LinkR = (((getEs(Bes) -- L1) -- L2) -- Link),
    {North,South,Link,LinkR}.

split_edges_1(Edge,Bes) ->
%%     io:format("Split: ~w ~w~n",[Edge,d(Bes)]),
    {Before,BE1,After} = find_split(Edge,Bes,[]),
    {LeftLoop0,BE2,RightLoop0} = find_split(Edge,After,[BE1]),
    LeftLoop  = LeftLoop0 ++ [BE2],
    %% NOTE: Order is important below
    RightLoop = reverse(RightLoop0 ++ Before),
    {Loop1,Link1} = find_link(LeftLoop,  reverse(LeftLoop), []),
    {Loop2,Link2} = find_link(RightLoop, reverse(RightLoop), []),
%%     io:format("L1:~w~nL2:~w~nLink1:~w~nLink2:~w~n~n",
%% 	      [Loop1,(Loop2),(Link1),(Link2)]),
    Link = reorder_link(Link2++reverse(Link1)),
%%     io:format("Link:~w~n",[d(Link)]),
    {getEs(Loop1),getEs(Loop2),getEs(Link)}.

find_split(Edge,[G={Edge,_}|Bes],Acc) -> {reverse(Acc),G,Bes};
find_split(Edge,[This|Bes],Acc) ->
    find_split(Edge,Bes,[This|Acc]).

find_link([{E,_}|_],[{E,_}|_],Link = [{E,_}|_]) ->
    {[],Link};
find_link([G={E,_}|C1],[{E,_}|C2],Link) ->
    find_link(C1,C2,[G|Link]);
find_link(C1,_,Link) ->
    find_loop(C1,Link,[]).

find_loop([{E,_}|_],[{E,_}|_]=Link, Loop) ->
    {Loop,Link};
find_loop([G|C1],Link,Loop) ->
    find_loop(C1,Link,[G|Loop]);
find_loop([],[],Loop) -> {Loop,[]};
find_loop([],Link,[]) -> {[],Link}.

reorder_link([]) -> [];
reorder_link(A=[_]) -> A;
reorder_link(Ok  = [{_,#be{ve=V}},{_,#be{vs=V}}|_]) -> Ok;
reorder_link(Rev = [{_,#be{vs=V}},{_,#be{ve=V}}|_]) ->
    %% reverse(Rev);  %% Correctness asserted below
    reorder_link(reverse(Rev));
reorder_link(Other) ->
    io:format("Other: ~w~n",[Other]),
    exit(internal_error).

%%%% Uncomplete fixme.. BUGBUG
%%% I can't get this to work satisfactory..aarg.
%% find_axes_from_eigenv(Fs,CNormal,BEdges,We) ->
%%     Vs0 = wings_face:to_vertices(Fs, We),
%%     BVs = [Ver || #be{vs=Ver} <- BEdges],
%%     Center = wings_vertex:center(BVs,We),
%%     Vpos = lists:usort([wings_vertex:pos(V, We) || V <- Vs0]),
%%     {{Ev1,Ev2,Ev3},{Bv1,Bv2,Bv3}} = e3d_bv:eigen_vecs(Vpos),
%%     Vecs = [{Bv1,Ev1},{Bv2,Ev2},{Bv3,Ev3}],
%% %%    io:format("EIG ~p~n",[Vecs]),
%%     [{_Z0,C},{V1,A},{V2,B}] = find_closest(Vecs,CNormal,0.0,[]),
%%     case A/C > B/C of
%% 	true ->  Y0=V1,_X0=V2;
%% 	false -> _X0=V1,Y0=V2
%%     end,
%%     {X,Z,Y} = calc_axis(CNormal,Y0),
%% %%    io:format("Res ~p~n ~p~n ~p~n",[X,Y,Z]),
%%     {Center,{X,Y,Z},undefined}.

%% find_closest([H = {V,_}|R],N,Best,All=[B|Which]) ->
%%     Dot = e3d_vec:dot(V,N),
%%     case abs(Dot) > abs(Best) of
%% 	true -> find_closest(R,N,Dot,[H|All]);
%% 	false -> find_closest(R,N,Best,[B,H|Which])
%%     end;
%% find_closest([H = {V,_}|R],N,_,[]) ->
%%     Dot = e3d_vec:dot(V,N),
%%     find_closest(R,N,Dot,[H]);
%% find_closest([],_,Best,All) ->
%%     case Best > 0.0 of
%% 	true ->All;
%% 	false -> [{e3d_vec:neg(V),Ev}|| {V,Ev} <- All]
%%     end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
projectFromChartNormal(Chart, We) ->
    Normal = chart_normal(Chart,We),
    Vs0 = wings_face:to_vertices(Chart, We),
    rotate_to_z(Vs0, Normal, We).

projectFromCamera(Chart,{matrices,{MM,PM,VP}},We) ->
    Vs = wings_face:to_vertices(Chart, We),
    Proj = fun(V) ->
		   {X,Y,Z} = wings_vertex:pos(V, We),
		   {S,T, _} = wings_gl:project(X,Y,Z,MM,PM,VP),
		   {V,{S,T,0.0}}
	   end,
    lists:map(Proj, Vs).

chart_normal([],_We) -> throw(?__(1,"Can not calculate normal for chart."));
chart_normal(Fs,We = #we{es=Etab}) ->
    CalcNormal = fun(Face,Area) -> face_normal(Face,Area,We) end,
    N0 = foldl(CalcNormal, e3d_vec:zero(), Fs),
    case e3d_vec:norm(N0) of
	{0.0,0.0,0.0} -> %% Bad normal Fallback1
	    %%	    BE = auv_util:outer_edges(Fs,We,false),
	    [{_,BE}|_] = auv_placement:group_edge_loops(Fs,We),
	    EdgeNormals = 
		fun(#be{edge=Edge}, Sum0) ->
			#edge{lf=LF,rf=RF} = array:get(Edge, Etab),
			Sum1 = CalcNormal(LF,Sum0),
			CalcNormal(RF,Sum1)
		end,
	    N1 = foldl(EdgeNormals, e3d_vec:zero(), BE),
	    case e3d_vec:norm(N1) of
		{0.0,0.0,0.0} -> %% Bad normal Fallback2
		    NewFs = decrease_chart(Fs,BE),
		    chart_normal(NewFs, We);
		N -> e3d_vec:neg(N)
	    end;
	N -> N
    end.

face_normal(Face,Sum,We) ->
    Normal = wings_face:normal(Face, We),
    Vs0 = wpa:face_vertices(Face, We),
    Area = calc_area(Vs0,Normal, We),
    e3d_vec:add(Sum, e3d_vec:mul(Normal, Area)).

decrease_chart(Fs0,BE) ->
    Fs1 = gb_sets:from_list(Fs0),
    Del = fun(#be{face=Face},FSin) ->
		  gb_sets:del_element(Face,FSin)
	  end,
    Fs = foldl(Del, Fs1, BE),
    gb_sets:to_list(Fs).

rotate_to_z(Vs, Normal, We) ->
    Rot = e3d_mat:rotate_s_to_t(Normal,{0.0,0.0,1.0}),
    [{V,e3d_mat:mul_point(Rot, wings_vertex:pos(V, We))} || V <- Vs].

%% Alg. found in comp.graphics.algorithms faq
%% To be correct it needs the polygons to flat but we
%% don't need to be 100% correct.

fs_area(Fs,We) ->
    fs_area(Fs,We,0.0).
fs_area([Face|Rest],We,Area) ->
    Vs0 = wpa:face_vertices(Face, We),
    NewArea = try 
		  Normal = wings_face:normal(Face, We),
		  calc_area(Vs0, Normal, We)
	      catch _:_ ->
		      0.0
	      end,
    fs_area(Rest,We,NewArea+Area);
fs_area([],_,Area) ->
    Area.

calc_area(Vs0, Normal, We) ->
    [V|Vs] = [wings_vertex:pos(V, We) || V <- Vs0],
    Sum = sum_crossp([V|Vs] ++ [V], e3d_vec:zero()),
    0.5 * abs(e3d_vec:dot(Normal, Sum)).

sum_crossp([V1,V2|Vs], Acc) ->
    Cross = e3d_vec:cross(V1,V2),
    sum_crossp([V2|Vs], e3d_vec:add(Acc, Cross));
sum_crossp([_Last], Acc) ->
    Acc.

lsqcm(Fs, none, Loop, We) ->
    %%	{V1, V2} = find_pinned_from_edges(Loop,We),
    %%	[V1,V2];  % the new stuff picks different pinned 
    {V3, V4} = find_pinned(Loop,We),
    lsqcm(Fs,[V3,V4],Loop,We);
lsqcm(Fs, Pinned, _Loop, We) ->
    ?DBG("Project and tri ~n", []),
    LSQState = lsq_setup(Fs,We,Pinned),
    {ok,Vs2} = lsq(LSQState, Pinned),
    %%?DBG("LSQ res ~p~n", [Vs2]),
    Patch = fun({Idt, {Ut,Vt}}) -> {Idt,{Ut,Vt,0.0}} end,
    Vs3 = lists:sort(lists:map(Patch, Vs2)),
    TempVs = array:from_orddict(Vs3),
    Area = fs_area(Fs, We, 0.0),
    MappedArea = fs_area(Fs, We#we{vp=TempVs}, 0.0),
    Scale = Area/MappedArea,
    scaleVs(Vs3,math:sqrt(Scale),[]).

scaleVs([{Id, {X,Y,_}}|Rest],Scale,Acc) 
  when is_float(X), is_float(Y), is_float(Scale) ->
    scaleVs(Rest, Scale, [{Id, {X*Scale,Y*Scale,0.0}}|Acc]);
scaleVs([],_,Acc) ->
    lists:reverse(Acc).

-record(link,{no=0,pos={0.0,0.0,0.0}}).

find_pinned_from_edges({Circumference, BorderEdges}, #we{vp=Vtab}) ->    
    [First,Second,Third|RestOfEdges] = 
	case BorderEdges of %% Check order
	    [#be{ve=TO},#be{vs=TO}|_] -> BorderEdges;
	    _ -> reverse(BorderEdges)
	end,
    POS = fun(V) -> array:get(V,Vtab) end,
    Left = #link{no=1,pos=POS(Second#be.vs)},
    Right = foldl(fun(#be{vs=V}, #link{no=No,pos=Apos}) ->
			  #link{no=No+1,pos=e3d_vec:add(Apos,POS(V))}
		  end, #link{}, RestOfEdges),
    Vert1 = {First#be.vs,POS(First#be.vs)},
    VertN = {Third#be.vs,POS(Third#be.vs)},
    Best = {Vert1,VertN,calc_vp(Vert1,VertN,Left,Right)},
    HCC = Circumference/2, %% - Circumference/100,
    Dist = First#be.dist + Second#be.dist,
%    io:format("Pinned ~p ~n", [Best]),
%    io:format(" L ~p R ~p~n",[Left,Right]),
    find_pinned_from_edges2(First,[Second],Third,RestOfEdges,First,Dist,HCC,Left,Right,POS,Best).

find_pinned_from_edges2(_S,_LVs,Start,_,Start,_Dist,_HCC,_Left0,_Right0,_POS,{{V1,_},{V2,_},_Best}) ->
%%    io:format("Pinned ~p ~p ~p~n", [V1,V2,_Best]),
    {{V1,{0.0,0.0}},{V2,{1.0,1.0}}};
find_pinned_from_edges2(S,LVs,E,[Next|RVs],Start,Dist,HCC,Left0,Right0,POS,Best0)
  when Dist < HCC ->
%    io:format("LT ~p(~p) +~p ",[Dist,HCC,E]),
    NewDist = Dist + E#be.dist,
    {Left,Right,Best} = recalc_positions(S,E,Left0,Next,Right0,POS,Best0,incr),
    find_pinned_from_edges2(S,LVs++[E],Next,RVs,Start,NewDist,HCC,Left,Right,POS,Best);
find_pinned_from_edges2(S,[Next|LVs],E,RVs,Start,Dist,HCC,Left0,Right0,POS,Best0) -> 
%    io:format("RT ~p(~p) -~p ",[Dist,HCC,S]),
    NewDist = Dist - S#be.dist,
    {Left,Right,Best} = recalc_positions(E,S,Right0,Next,Left0,POS,Best0,decr),
    find_pinned_from_edges2(Next,LVs,E,RVs++[S],Start,NewDist,HCC,Left,Right,POS,Best).

recalc_positions(Start,Prev,#link{no=Ino,pos=Ipos},Next,#link{no=Dno,pos=Dpos},POS,Best0,Op) ->
    {_PV1,_PV2,Least} = Best0,
    Left  = #link{no=Ino+1,pos=e3d_vec:add(Ipos,POS(Prev#be.vs))},
    Right = #link{no=Dno-1,pos=e3d_vec:sub(Dpos,POS(Next#be.vs))},
    Vert1 = {Start#be.vs,POS(Start#be.vs)},
    Vert2 = {Next#be.vs,POS(Next#be.vs)},
    New = calc_vp(Vert1,Vert2,Left,Right),
    case New < Least of
	true when Op == incr -> {Left,Right,{Vert1,Vert2,New}};
	true ->  {Right,Left,{Vert2,Vert1,New}};
	false when Op == incr -> {Left,Right,Best0};
	false -> {Right,Left,Best0}
    end.

calc_vp(_VI1={_Id1,V1},_VI2={_Id2,V2},Left,Right) ->
    try 
	AxisVec = e3d_vec:sub(V2,V1),
	AxisLen = e3d_vec:len(AxisVec),
	Axis = e3d_vec:norm(AxisVec),
	LeftPos = divide(Left#link.pos, Left#link.no),
	RightPos = divide(Right#link.pos, Right#link.no),
	Vec1 = e3d_vec:sub(V2,LeftPos),
	Vec2 = e3d_vec:sub(V2,RightPos),
	A1   = abs(math:acos(e3d_vec:dot(e3d_vec:norm(Vec1),Axis))),
	A2   = abs(math:acos(e3d_vec:dot(e3d_vec:norm(Vec2),Axis))),
	abs((A1 + 10*e3d_vec:len(Vec1)/AxisLen) - (A2 + 10*e3d_vec:len(Vec2)/AxisLen))
    catch _:_What ->
	    9999999999.99
    end.

divide(What,0) -> What;
divide(Vec,S) -> e3d_vec:divide(Vec,S).
     
find_pinned({Circumference, BorderEdges}, We) ->
    Vs = [array:get(V1, We#we.vp) || #be{vs=V1} <- BorderEdges],
    Center = e3d_vec:average(Vs),
    AllC = lists:map(fun(#be{vs=Id}) ->
			     Pos = array:get(Id, We#we.vp),
			     Dist = e3d_vec:dist(Pos, Center),
			     {Dist, Id, Pos}
		     end, BorderEdges),
    [{_,V0,_V1Pos}|_] = lists:reverse(lists:sort(AllC)),
    BE1 = reorder_edge_loop(V0, BorderEdges, []),
    HalfCC = Circumference/2, %% - Circumference/100,
    {V1, V2} = find_pinned(BE1, BE1, 0.0, HalfCC, HalfCC, undefined), 
    {{V1,{0.0,0.0}},{V2,{1.0,1.0}}}.
    
find_pinned(Curr=[#be{vs=C1,dist=Clen}|CR],Start=[#be{ve=S2,dist=Slen}|SR],Len,HCC,Best,BVs) ->    
    Dlen = HCC-(Clen+Len),
    ADlen = abs(Dlen),
%    ?DBG("Testing ~p ~p ~p ~p ~p~n", [{S2,C1},Dlen,{Len+Clen,HCC}, Best, BVs]),    
    if 
	Dlen >= 0.0 ->
	    if ADlen < Best ->
		    find_pinned(CR,Start,Clen+Len,HCC,ADlen,{S2,C1});
	       true ->
		    find_pinned(CR,Start,Clen+Len,HCC,Best,BVs)
	    end;
	Dlen < 0.0 ->
	    if ADlen < Best ->
		    find_pinned(Curr,SR,Len-Slen,HCC, ADlen,{S2,C1});
	       true ->
		    find_pinned(Curr,SR,Len-Slen,HCC,Best,BVs)
	    end
    end;
find_pinned([], _, _, _, _Best, Bvs) ->
%    ?DBG("Found ~p ~p~n", [_Best, Bvs]),
    Bvs.

reorder_edge_loop(V1, [Rec=#be{vs=V1}|Ordered], Acc) ->
    Ordered ++ lists:reverse([Rec|Acc]);
reorder_edge_loop(V1, [H|Tail], Acc) ->
    reorder_edge_loop(V1, Tail, [H|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Least Square Conformal Maps %%%%%%%%%%%%

-record(lsq,{a,x0,ap,temp1,temp2,dr}).

lsq_setup(Fs,We,Pinned) ->
    {M,N,D,DR,L1,L2} = lsq_init(Fs,We,Pinned),
    {Lquv0,{Np,Usum,Vsum}} =  
	lists:mapfoldl(
	  fun({P,{U,V} = UV}, {I,X,Y}) ->
		  {ok,Q} = dict:find(P, D),
		  {{Q,UV}, {I+1,X+U,Y+V}}
	  end,{0,0.0,0.0},Pinned),
    Lquv = lists:sort(Lquv0), % Must be sorted for pick() and insert() to work.
    ?DBG("lsq_int - Lquv = ~p~n",[Lquv]),
    %% Build the basic submatrixes 
    %% M1 = Re(M), M2 = Im(M), M2n = -M2
    {M1,M2,M2n} = build_basic(M,L1,L2),
    %% Compile the basic submatrixes into the ones related to 
    %% free points (Mf*) i.e unknown, 
    %% and pinned points (Mp*).
    {Mfp1c,Mfp2c,Mfp2nc,LuLv} = build_cols(M1,M2,M2n,Lquv),
    ?DBG("lsq_int - LuLv = ~p~n", [LuLv]),
    %% Compose the matrix and vector to solve
    %% for a Least SQares solution.
    {Af,Ap} = build_matrixes(N,Mfp1c,Mfp2c,Mfp2nc),
    ?DBG("Solving matrices~n", []),
    X0Fix = auv_matrix:vector(lists:duplicate(M-Np, Usum/Np)++
			      lists:duplicate(M-Np, Vsum/Np)),

    #lsq{a=Af,x0=X0Fix,ap=Ap,temp1=LuLv,temp2=Lquv,dr=DR}.

lsq_init(Fs0,We0,Pinned0) -> 
    %% Do a real triangulation, might be optimized later.
    We = wings_tesselation:triangulate(Fs0, We0),
    Fs = Fs0 ++ wings_we:new_items_as_ordset(face,We0,We),
    Pinned = gb_trees:from_orddict(lists:sort(Pinned0)),
    lsq_init_fs(Fs,Pinned,We,{0,dict:new(),dict:new()},0,[],[]).

lsq_init_fs([F|Fs],P,We = #we{vp=Vtab},Ds0,N,Re0,Im0) ->
    Vs = [[A0|_],[B0|_],[C0|_]] = wings_va:face_attr([vertex|uv], F, We),
    {[A,B,C],Ds} = update_dicts(Vs,Ds0),
%%    {X1=Z0x,Y1=Z0y,X2=Z1x,Y2=Z1y,X3=Z2x,Y3=Z2y} = 
    {X1,Y1,X2,Y2,X3,Y3} = 
	project_tri(array:get(A0,Vtab),array:get(B0,Vtab),
		    array:get(C0,Vtab)), 
    %% Raimos old solution. 
    SqrtDT0 = try math:sqrt(abs((X2-X1)*(Y3-Y1)-(Y2-Y1)*(X3-X1)))
	     catch _:_ -> 0.000001
	     end,
    SqrtDT = if SqrtDT0 =:= 0.0 -> 1.0;  % this can happen e.g. in a bevel/extrude without offset
        true -> SqrtDT0
    end,
    W1re = X3-X2, W1im = Y3-Y2, 
    W2re = X1-X3, W2im = Y1-Y3, 
    W3re = X2-X1, W3im = Y2-Y1,

    Re=[[{A,W1re/SqrtDT},{B,W2re/SqrtDT},{C,W3re/SqrtDT}]|Re0],
    Im=[[{A,W1im/SqrtDT},{B,W2im/SqrtDT},{C,W3im/SqrtDT}]|Im0],

%% Levy's c-code
    %% Vector2 z01 = z1 - z0 ;
    %% Vector2 z02 = z2 - z0 ;
    %% double a = z01.x ;
    %% double b = z01.y ;
    %% double c = z02.x ;
    %% double d = z02.y ;
    %% assert(b == 0.0) ;

    %% // Note  : 2*id + 0 --> u
    %% //         2*id + 1 --> v
    %% int u0_id = 2*id0     ;
    %% int v0_id = 2*id0 + 1 ;
    %% int u1_id = 2*id1     ;
    %% int v1_id = 2*id1 + 1 ;
    %% int u2_id = 2*id2     ;
    %% int v2_id = 2*id2 + 1 ;

    %% // Note : b = 0

    %% // Real part
    %% nlBegin(NL_ROW) ;
    %% nlCoefficient(u0_id, -a+c)  ;
    %% nlCoefficient(v0_id,  b-d)  ;
    %% nlCoefficient(u1_id,   -c)  ;
    %% nlCoefficient(v1_id,    d)  ;
    %% nlCoefficient(u2_id,    a) ;
    %% nlEnd(NL_ROW) ;

    %% // Imaginary part
    %% nlBegin(NL_ROW) ;
    %% nlCoefficient(u0_id, -b+d) ;
    %% nlCoefficient(v0_id, -a+c) ;
    %% nlCoefficient(u1_id,   -d) ;
    %% nlCoefficient(v1_id,   -c) ;
    %% nlCoefficient(v2_id,    a) ;
    %% nlEnd(NL_ROW) ;
    %% }

    lsq_init_fs(Fs,P,We,Ds,N+1,Re,Im);
lsq_init_fs([],_,_We,{M,D,DR},N,Re0,Im0) ->
    {M,N,D,DR,vecs(M,Re0,[]),vecs(M,Im0,[])}.

vecs(M,[R|Rs],Acc) ->
    vecs(M,Rs,[auv_matrix:vector(M,R)|Acc]);
vecs(_,[],Acc) -> Acc.

update_dicts(Ids,{N,D,DR}) ->
    update_dicts(Ids,N,D,DR,[]).
update_dicts([[P|_]|Rest],N,D,DR,Acc) ->
    case dict:find(P,D) of
	error ->
	    N1 = N+1,
	    update_dicts(Rest,N1,dict:store(P,N1,D),dict:store(N1,P,DR),[N1|Acc]);
	{ok,Id} ->
	    update_dicts(Rest,N,D,DR,[Id|Acc])
    end;
update_dicts([],N,D,DR,Acc) ->
    {lists:reverse(Acc),{N,D,DR}}.

project_tri(P0,P1,P2) ->
    L = e3d_vec:sub(P1,P0),
    X = e3d_vec:norm(L),
    T = e3d_vec:sub(P2,P0),
    Z = e3d_vec:norm(e3d_vec:cross(X,T)),
    Y = e3d_vec:cross(Z,X),
    {0.0,0.0,
     e3d_vec:len(L),0.0,
     e3d_vec:dot(T,X), e3d_vec:dot(T,Y)}.
    
lsq(L, Lpuv) when is_list(Lpuv) ->
    lsq(L, Lpuv, env);
lsq(Name, Method) when is_atom(Method) ->
    {ok, [{L, Lpuv}]} = file:consult(Name),
    lsq(L, Lpuv, Method).

lsq(L, Lpuv, Method0) when is_record(L,lsq), is_list(Lpuv), is_atom(Method0) ->
    Method = case Method0 of 
		 env ->
		     case os:getenv("WINGS_AUTOUV_SOLVER") of
			 "ge" -> ge;
			 "cg" -> cg;
			 "cg_jacobian" -> cg_jacobian;
			 "cg_colnorm" -> cg_colnorm;
			 _ -> cg_colnorm
		     end;
		 M -> M
	     end,
    try lsq_int(L, Lpuv, Method)
    catch
	error:badarg ->
	    ST = erlang:get_stacktrace(),
	    error(badarg, {[L,Lpuv,Method],ST})
    end;
lsq(L, Lpuv, Method) ->
    error(badarg, [L, Lpuv, Method]).

lsq_int(#lsq{a=Af,x0=X0,ap=Ap,temp1=LuLv,temp2=Lquv,dr=Rdict},_Pinned,Method) ->
    %% Clean this mess up    
    {Np,K_LuLv} = keyseq_neg(LuLv),
    U = auv_matrix:vector(Np, K_LuLv),
    ?DBG("build_matrixes - U = ~p~n", [U]),
    B = auv_matrix:mult(Ap, U),
    
    X = case Method of
	    ge -> minimize_ge(Af,B);
	    _ ->
		{_,X1} = minimize_cg(Af,X0,B),
		X1
	end,
    %%    ?DBG("X=~p~n", [X]),
    %% Extract the vector of previously unknown points,
    %% and insert the pinned points. Re-translate the
    %% original point identities.
    lsq_result(X, Lquv, Rdict).

build_basic(M,L1,L2) ->
    M1 = auv_matrix:rows(M, L1),
    M2 = auv_matrix:rows(M, L2),
    M2n = auv_matrix:rows(M, [auv_matrix:mult(-1, X) || X <- L2]),
    {M1,M2,M2n}.

build_cols(M1,M2,M2n,Lquv) ->
    %% Build column lists of the M matrixes
    M1c = auv_matrix:cols(M1),
    M2c = auv_matrix:cols(M2),
    M2nc = auv_matrix:cols(M2n),
    %% Split the column lists into free (Mf) and pinned (Mp)
    {Lq,Lu,Lv} = split_quv(Lquv), % Lquv is sorted
    {pick(M1c, Lq),pick(M2c, Lq),pick(M2nc, Lq), Lu++Lv}.

split_quv(Lquv) ->
    split_quv(Lquv, [], [], []).

split_quv([], Lq, Lu, Lv) ->
    {lists:reverse(Lq),lists:reverse(Lu),lists:reverse(Lv)};
split_quv([{Q,{U,V}} | Lquv], Lq, Lu, Lv) ->
    split_quv(Lquv, [Q | Lq], [U | Lu], [V | Lv]).

build_matrixes(N,{Mf1c,Mp1c},{Mf2c,Mp2c},{Mf2nc,Mp2nc}) ->
    %% Build the matrixes Af and Ap, and vector B
    %% A = [ M1 -M2 ],  B = Ap U, U is vector of pinned points
    %%     [ M2  M1 ]
    Afu = auv_matrix:cols(N, Mf1c++Mf2nc),
    Afl = auv_matrix:cols(N, Mf2c++Mf1c),
    Af  = auv_matrix:cat_rows(Afu, Afl),
    Apu = auv_matrix:cols(N, Mp1c++Mp2nc),
    Apl = auv_matrix:cols(N, Mp2c++Mp1c),
    Ap  = auv_matrix:cat_rows(Apu, Apl),
    {Af, Ap}.

keyseq_neg(L) ->
    keyseq(1, L, []).

keyseq(N, [], R) ->
    {N-1,lists:reverse(R)};
keyseq(N, [X | L], R) ->
    keyseq(N+1, L, [{N,-X} | R]).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%              t   _       t _
%% by solving A   A x  =  A   b
%%
%% using Gaussian Elimination and back substitution.
%%
minimize_ge(A, B) ->
    AA = mk_solve_matrix(A, B),
    AAA = auv_matrix:reduce(AA),
%%    ?DBG("Reduced: ~p~n", [AAA]),
    X = auv_matrix:backsubst(AAA),
    ?DBG("Solved~n",[]),
    X.    

mk_solve_matrix(Af,B) ->
    AfT = auv_matrix:trans(Af),
    AfTAf = auv_matrix:mult_trans(AfT, AfT),
    AfTB = auv_matrix:mult(-1, auv_matrix:mult(AfT, B)),
    auv_matrix:cat_cols(AfTAf, AfTB).

%%               _   _    2
%% Minimize || A x - b ||  
%%
%%             -1  t    _      -1  t  _
%% by solving M   A   A x  =  M   A   b
%%                                                         __
%% using the Preconditioned Coujugate Gradient method with x0 as 
%% iteration start vector.
%%
minimize_cg(A, X0, B) ->
    ?DBG("minimize_cg - dim A=~p X0=~p B=~p~n",
	 [auv_matrix:dim(A), auv_matrix:dim(X0), auv_matrix:dim(B)]),
    {N,M} = auv_matrix:dim(A),
    {M,1} = auv_matrix:dim(X0),
    {N,1} = auv_matrix:dim(B),
    I = M,
    Epsilon = 1.0e-3,
    At = auv_matrix:trans(A),
    AtB = auv_matrix:mult(At, B),

    %% A very cheap preconditioning. The column norm
    %% takes no time to calculate compared to 
    %% AtA above. The iteration time impact is also 
    %% very low since it is a matrix multiplication
    %% with a diagonal (i.e. very sparse) matrix.
    %% The preconditioning effect (on the required
    %% number of iterations) is modest, but 
    %% cost effective.
    Diag = auv_matrix:row_norm(At),
    M_inv = try [1/V || V <- Diag] of
		Diag_inv ->
		    M_i = auv_matrix:diag(Diag_inv),
		    fun (R_new) ->
			    auv_matrix:mult(M_i, R_new)
		    end
	    catch
		error:badarith ->
		    fun (R_new) ->
			    auv_matrix:mult(1, R_new)
		    end
	    end,
    R = auv_matrix:sub(AtB, auv_matrix:mult(At, auv_matrix:mult(A, X0))),
    D = M_inv(R),
    Delta = auv_matrix:mult(auv_matrix:trans(R), D),
    Delta_max = Epsilon*Epsilon*Delta,
    minimize_cg(M_inv, At, A, AtB, Delta_max, 
		Delta, I, D, R, X0).

minimize_cg(_, _At, _A, _, _, 
	    _, 0, _D, _, X) ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {stopped, X};
minimize_cg(_, _At, _A, _, Delta_max, 
	    Delta, _, _D, _, X) when Delta < Delta_max ->
    ?DBG("minimize_cg() sizes were ~p ~p ~p~n", 
	 [auv_matrix:dim(_At), auv_matrix:dim(_A), auv_matrix:dim(_D)]),
    {ok, X};
minimize_cg(M_inv, At, A, AtB, Delta_max, 
	    Delta, I, D, R, X) ->
%%    ?DBG("minimize_cg() step ~p Delta=~p~n", [I, Delta]),
    P = auv_matrix:mult(A, D),
    Alpha = Delta / auv_matrix:mult(auv_matrix:trans(P), P),
    X_new = auv_matrix:add(X, auv_matrix:mult(Alpha, D)),
    if (I + 5) rem 10 == 0 ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    minimize_cg_2(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, R, X_new, Alpha, P)
    end.

minimize_cg_2(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, R, X_new, Alpha, P) ->
    R_new = auv_matrix:sub(R, auv_matrix:mult(Alpha, auv_matrix:mult(At, P))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    if Delta_new < Delta_max ->
	    minimize_cg_3(M_inv, At, A, AtB, Delta_max,
			  Delta, I, D, X_new);
       true ->
	    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
	    minimize_cg(M_inv, At, A, AtB, Delta_max,
			Delta_new, I-1, D_new, R_new, X_new)
    end.

minimize_cg_3(M_inv, At, A, AtB, Delta_max,
	      Delta, I, D, X_new) ->
    ?DBG("minimize_cg() recalculating residual ~p~n", [Delta]),
    R_new = auv_matrix:sub
	      (AtB, auv_matrix:mult(At, auv_matrix:mult(A, X_new))),
    S = M_inv(R_new),
    Delta_new = auv_matrix:mult(auv_matrix:trans(R_new), S),
    D_new = auv_matrix:add(S, auv_matrix:mult(Delta_new/Delta, D)),
    minimize_cg(M_inv, At, A, AtB, Delta_max,
		Delta_new, I-1, D_new, R_new, X_new).

%% Extract the result from vector X and combine it with the 
%% pinned points. Re-translate the point identities.
%%
lsq_result(X, Lquv, Rdict) ->
    {MM,1} = auv_matrix:dim(X),
    {Ulist, Vlist} = split(auv_matrix:vector(X), MM div 2),
    {[],UVlistR} = 
	foldl(
	  fun (U, {[], R}) ->
		  {[], [{U,0.0} | R]};
	      (U, {[V | L], R}) ->
		  {L, [{U,V} | R]};
	      (Other, State) ->
		  throw({error, {?FILE, ?LINE, [Other, State, X]}})
	  end, {Vlist, []}, Ulist),
    UVlist = insert(lists:reverse(UVlistR), Lquv),
    {_, TxMapR} =
	foldl(
	  fun (UV, {Q,R}) ->
		  {Q+1,[{dict:fetch(Q, Rdict),UV} | R]}
	  end, {1,[]}, UVlist),
    TxMap = lists:reverse(TxMapR),
%%    ?DBG("lsq_result - TxMap = ~p~n", [TxMap]),
    {ok, TxMap}.

%% Picks terms with specified indexes from a list.
%%
%% L: list of terms
%% P: list of indexes in ascending order
%%
%% Return: {L_remaining, L_picked}
%%
pick(L, P) when is_list(L), is_list(P) ->
    case pick(1, L, P, [], []) of
	{_, _} = Ok ->
	    Ok;
	Fault ->
	    error(Fault, [L, P])
    end;
pick(L, P) ->
    error(badarg, [L, P]).

pick(_, L, [], R, Q) ->
    {lists:reverse(R, L), lists:reverse(Q)};
pick(_, [], _, _, _) ->
    badarg;
pick(_, _, [I, J | _], _, _) when I >= J ->
    badarg;
pick(I, [V | L], [I | P], R, Q) ->
    pick(I+1, L, P, R, [V | Q]);
pick(I, [V | L], P, R, Q) ->
    pick(I+1, L, P, [V | R], Q);
pick(_, _, _, _, _) ->
    badarg.



%% Insert terms with specified indexes in a list
%%
%% L: List of terms
%% S: List of {Pos,Term} tuples with Term to be 
%%    inserted at position Pos in L
%%
insert(L, S) when is_list(L), is_list(S) ->
    case insert(1, L, S, []) of
	R when is_list(R) ->
	    R;
	Fault ->
	    error(Fault, [L, S])
    end;
insert(L, S) ->
    error(badarg, [L, S]).

insert(_, L, [], R) ->
    lists:reverse(R, L);
insert(_, _, [{I,_}, {J,_} | _], _) when I >= J ->
    badarg;
insert(I, L, [{I,E} | S], R) ->
    insert(I+1, L, S, [E | R]);
insert(_, [], _, _) ->
    badarg;
insert(I, [E | L], S, R) ->
    insert(I+1, L, S, [E | R]).



%% Split a list into two after N terms
%%
split(L, N) ->
    split(L, N, []).

split([], _, R) ->
    {lists:reverse(R), []};
split(L, 0, R) ->
    {lists:reverse(R), L};
split([E | L], N, R) ->
    split(L, N-1, [E | R]).

area2d2({S1,T1},{S2,T2},{S3,T3})
  when is_float(S1),is_float(S2),is_float(S3),
       is_float(T1),is_float(T2),is_float(T3) ->
    ((S2-S1)*(T3-T1)-(S3-S1)*(T2-T1)).

area3d(V1, V2, V3) ->
    e3d_vec:area(V1, V2, V3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Texture metric stretch 
%% From 'Texture Mapping Progressive Meshes' by
%% Pedro V. Sander, John Snyder Steven J. Gortler, Hugues Hoppe

-record(s,{f2v,   % Face 2 vertex id
	   v2f,   % Vertex 2 faces
	   f2a,   % Face(s) 3d area
	   f2ov,  % Face original vertex 3d position
	   bv     % Border vertices
	  }).

-define(MIN_STRETCH, 1.01).
-define(MAX_ITER, 100).
-define(MAX_LEVELS, 6).
-define(VERTEX_STEP, 0.001).

stretch_opt(We0, OVs) ->
    Fs = wings_we:visible(We0),
    wings_pb:start(?__(1,"optimizing")),
    wings_pb:update(0.01, ?__(2,"initializing")),

    %% {FaceToStretchMean, FaceToStretchWorst,FaceToVerts,VertToFaces,VertToUvs}
    {F2S2,_F2S8,Uvs,State,Scale} = stretch_setup(Fs,We0,OVs),

    V2S = stretch_per_vertex(gb_trees:to_list(State#s.v2f),F2S2,State,gb_trees:empty()),
    S2V = lists:reverse(lists:keysort(2,gb_trees:to_list(V2S))),
    {SUvs0,_F2S2} = wings_pb:done(stretch_iter(S2V,1,V2S,F2S2,Uvs,State)),
    %% Verify
    _Mean2  = model_l2(gb_trees:keys(_F2S2), _F2S2, State#s.f2a,0.0, 0.0),
    io:format(?__(3,"After Stretch sum (mean) ~p")++" ~n",  [_Mean2]),

    SUvs1 = gb_trees:to_list(SUvs0),
    
    Suvs = [{Id,{S0/Scale,T0/Scale,0.0}} || {Id,{S0,T0}} <- SUvs1],
    We0#we{vp=array:from_orddict(Suvs)}.

stretch_setup(Fs, We0, OVs) ->
    Be = wings_face:outer_edges(Fs, We0),
    Bv0 = foldl(fun(Edge, Acc) ->
			#edge{vs=Vs,ve=Ve} = array:get(Edge, We0#we.es),
			[Vs,Ve|Acc]
		end, [], Be),
    Bv = gb_sets:from_list(Bv0),
    Tris0 = triangulate(Fs,We0),
    {S,F2A,F2OV} = calc_scale(Tris0, OVs, 0.0, 0.0, [], []),
    Tris = [{Face,[{Id1,{S1*S,T1*S}},{Id2,{S2*S,T2*S}},{Id3,{S3*S,T3*S}}]} ||
	       {Face,[{Id1,{S1,T1}},{Id2,{S2,T2}},{Id3,{S3,T3}}]} <- Tris0],
    {F2S2,F2S8,Uvs,State0} = init_stretch(Tris,F2OV, [], [], [], [], []),
    Worst = model_l8(gb_trees:keys(F2S8), F2S8, 0.0), 
    Mean  = model_l2(gb_trees:keys(F2S2), F2S2, F2A,0.0, 0.0),
    io:format(?__(1,"Stretch sum (worst) ~p")++" ~n", [Worst]),
    io:format(?__(2,"Stretch sum (mean) ~p")++" ~n",  [Mean]),
    {F2S2,F2S8,Uvs,State0#s{f2a=F2A,f2ov=F2OV,bv=Bv},S}.

stretch_iter(S2V0=[{_,First}|_],I,V2S0,F2S20,Uvs0,State) 
  when First > ?MIN_STRETCH, I < ?MAX_ITER ->
    if
	I rem 4 =:= 0 ->
	    wings_pb:update(I/?MAX_ITER, ?__(1,"iteration")++" "++integer_to_list(I));
	true ->
	    ok
    end,
    {V2S,F2S2,Uvs} = stretch_iter2(S2V0,V2S0,F2S20,Uvs0,State),
    S2V = lists:reverse(lists:keysort(2, gb_trees:to_list(V2S))),
    stretch_iter(S2V,I+1,V2S,F2S2,Uvs,State);
stretch_iter(_,_,_,F2S2,Uvs,_) ->
    {Uvs,F2S2}.

stretch_iter2([{V,OldVal}|R],V2S0,F2S20,Uvs0,State)
  when OldVal > ?MIN_STRETCH ->
    Line = random_line(),
    #s{f2v=F2Vs,v2f=V2Fs} = State,
    Fs   = gb_trees:get(V,V2Fs),
    Val  = gb_trees:get(V,V2S0),
    %%	    ?DBG("~p ~.4f:",[V,Val]), 
    {PVal,Uvs,F2S2} = opt_v(Val,0,?VERTEX_STEP,V,Line,Fs,F2S20,Uvs0,State),
    case PVal == Val of 
	true -> 
	    stretch_iter2(R,V2S0,F2S20,Uvs0,State);
	false ->
	    Vs0  = lists:usort(lists:append([gb_trees:get(F,F2Vs)|| F<-Fs])),
	    Upd0 = foldl(fun(Vtx, New) ->
				 [{Vtx,gb_trees:get(Vtx, V2Fs)}|New]
			 end, [], Vs0),
	    V2S = stretch_per_vertex(Upd0,F2S2,State,V2S0),
	    stretch_iter2(R,V2S,F2S2,Uvs,State)
    end;
stretch_iter2(_,V2S,F2S2,Uvs,_) ->
    {V2S, F2S2, Uvs}.

random_line() ->
    X   = rand:uniform()-0.5,
    Y   = rand:uniform()-0.5,
    Len = math:sqrt(X*X+Y*Y),
    {X/Len,Y/Len}.

opt_v(PVal,I,Step,V,L,Fs,F2S0,Uvs0,_State=#s{f2v=F2Vs,f2ov=F2OV,f2a=F2A}) ->    
    UV = gb_trees:get(V, Uvs0),
    {Data,F2S1} = 
	foldl(fun(Face, {Acc,Fs0}) ->
		      Vs = [V1,V2,V3] = gb_trees:get(Face, F2Vs),
		      {[{Vs,
			 {gb_trees:get(V1,Uvs0),
			  gb_trees:get(V2,Uvs0),
			  gb_trees:get(V3,Uvs0)},
			 gb_trees:get(Face, F2OV),
			 Face,
			 gb_trees:get(Face, F2A)}|Acc],
		       [{Face,gb_trees:get(Face,F2S0)}|Fs0]}
	      end, {[],[]}, Fs),
    {Stretch,St,F2S2} = opt_v2(PVal,I,Step,V,UV,L,Data,F2S1),
    case Stretch < PVal of
	true ->
	    F2S = update_fs(F2S2,F2S0),
	    {Stretch,gb_trees:update(V,St,Uvs0),F2S};
	false ->
	    {PVal,Uvs0,F2S0}
    end.

update_fs([{Face,S}|Ss],F2S) ->
    update_fs(Ss,gb_trees:update(Face,S,F2S));
update_fs([],F2S) -> F2S.

opt_v2(PVal,I,Step,V,UV={S0,T0},L={X,Y},Data,FS0) 
  when I < ?MAX_LEVELS ->
    St = {S0+X*Step,T0+Y*Step},
    {Stretch,FS} = calc_stretch(V,Data,St,0.0,0.0,[]),
    if 
	Stretch < PVal ->
%	    io:format(">"),
	    opt_v2(Stretch,I,Step,V,St,L,Data,FS);
	(I rem 2) == 0 ->
%	    io:format("S"),
	    opt_v2(PVal,I+1,-Step*0.9,V,UV,L,Data,FS0);
	true ->
%	    io:format("<"),
	    NewStep = Step/10,
	    opt_v2(PVal,I+1,NewStep,V,UV,L,Data,FS0)
    end;
opt_v2(PVal,_I,_Step,_V,St,_L,_,FS) ->
%    io:format("~n"),
    {PVal,St,FS}.
       
calc_stretch(V,[{[V,_,_],{_,UV2,UV3},{Q1,Q2,Q3},Face,FA}|R],UV1,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV1,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(V,[{[_,V,_],{UV1,_,UV3},{Q1,Q2,Q3},Face,FA}|R],UV2,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV2,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(V,[{[_,_,V],{UV1,UV2,_},{Q1,Q2,Q3},Face,FA}|R],UV3,Mean,Area,FS) ->
    S = l2(UV1,UV2,UV3,Q1,Q2,Q3),
    calc_stretch(V,R,UV3,S*S*FA+Mean,FA+Area,[{Face,S}|FS]);
calc_stretch(_,[],_,Mean,Area,FS) ->
    {math:sqrt(Mean/Area),reverse(FS)}.

stretch_per_vertex([{V,Fs}|R],F2S,State=#s{bv=Bv,f2a=F2A},Tree) ->
    case gb_sets:is_member(V,Bv) of
	false ->
	    Res = model_l2(Fs,F2S,F2A,0.0,0.0),
	    stretch_per_vertex(R,F2S,State,gb_trees:enter(V,Res,Tree));
	true ->
	    stretch_per_vertex(R,F2S,State,Tree)
    end;
stretch_per_vertex([], _, _,Acc) ->
    Acc.

init_stretch([{Face,FUvs=[{Id1,P1},{Id2,P2},{Id3,P3}]}|R],
	     Ovs,F2S2,F2S8,F2Vs,V2Fs,UVs) ->
    {Q1,Q2,Q3} = gb_trees:get(Face,Ovs),
    S2 = l2(P1,P2,P3,Q1,Q2,Q3),
    S8 = l8(P1,P2,P3,Q1,Q2,Q3),
    init_stretch(R,Ovs, [{Face,S2}|F2S2],[{Face,S8}|F2S8],
		 [{Face, [Id1,Id2,Id3]}|F2Vs],
		 [{Id1,Face},{Id2,Face},{Id3,Face}|V2Fs],
		 FUvs ++ UVs);
init_stretch([],_,F2S2,F2S8,F2Vs,V2Fs0,Uvs) ->
    V2Fs1 = sofs:relation(V2Fs0),
    V2Fs2 = sofs:relation_to_family(V2Fs1),
    V2Fs = sofs:to_external(V2Fs2),
    {gb_trees:from_orddict(lists:sort(F2S2)),
     gb_trees:from_orddict(lists:sort(F2S8)),
     gb_trees:from_orddict(lists:usort(Uvs)),
     #s{f2v = gb_trees:from_orddict(lists:sort(F2Vs)),
	v2f = gb_trees:from_orddict(V2Fs)}}.

calc_scale([{Face,[{Id1,P1},{Id2,P2},{Id3,P3}]}|R], Ovs, A2D, A3D,F2A,F2OVs) ->
    A2 = abs(area2d2(P1,P2,P3)/2),
    Q1 = array:get(Id1,Ovs),
    Q2 = array:get(Id2,Ovs),
    Q3 = array:get(Id3,Ovs),    
    A3 = area3d(Q1,Q2,Q3),
    calc_scale(R,Ovs,A2+A2D,A3+A3D,[{Face,A3}|F2A],[{Face,{Q1,Q2,Q3}}|F2OVs]);
calc_scale([],_Ovs,A2D,A3D,F2A,F2OVs) ->
    {math:sqrt(A3D/A2D), 
     gb_trees:from_orddict(lists:sort(F2A)), 
     gb_trees:from_orddict(lists:sort(F2OVs))}.

model_l8([Face|R], F2S8, Worst) ->
    FVal = gb_trees:get(Face,F2S8),
    New  = if FVal > Worst -> 
%		   ?DBG("Face ~p has worst ~p~n", [Face,FVal]),
		   FVal;
	      true -> 
		   Worst
	   end,
    model_l8(R,F2S8,New);
model_l8([], _, Worst) -> Worst.

model_l2([Face|R], F2S2, F2A, Mean, Area)  ->
    TriM = gb_trees:get(Face,F2S2),
    case gb_trees:get(Face,F2A) of
	A when is_float(TriM), is_float(A) ->
	    model_l2(R,F2S2,F2A,TriM*TriM*A+Mean,Area+A)
    end;
model_l2([],_,_,Mean,Area) ->
    math:sqrt(Mean/Area).

l2({S1,T1}, {S2,T2}, {S3,T3},
   {Q1x,Q1y,Q1z}, {Q2x,Q2y,Q2z}, {Q3x,Q3y,Q3z})
  when is_float(S1), is_float(S2), is_float(S3),
       is_float(T1), is_float(T2), is_float(T3),
       is_float(Q1x), is_float(Q1y), is_float(Q1z),
       is_float(Q2x), is_float(Q2y), is_float(Q2z),
       is_float(Q3x), is_float(Q3y), is_float(Q3z) ->
    T23 = T2-T3,    T31 = T3-T1,    T12 = T1-T2,
    S32 = S3-S2,    S13 = S1-S3,    S21 = S2-S1,
    case S21*T31-S13*T12 of
	DoubleArea when DoubleArea > 0.00000001 ->
	    SX = Q1x*T23+Q2x*T31+Q3x*T12,
	    SY = Q1y*T23+Q2y*T31+Q3y*T12,
	    SZ = Q1z*T23+Q2z*T31+Q3z*T12,
	    A = SX*SX+SY*SY+SZ*SZ,

	    TX = Q1x*S32+Q2x*S13+Q3x*S21,
	    TY = Q1y*S32+Q2y*S13+Q3y*S21,
	    TZ = Q1z*S32+Q2z*S13+Q3z*S21,
	    C = TX*TX+TY*TY+TZ*TZ,

	    math:sqrt((A+C)/(2.0*DoubleArea*DoubleArea));
	_ -> 
	    9999999999.9
    end.

l8(P1,P2,P3,Q1,Q2,Q3) ->  %% Worst stretch value
    A2 = area2d2(P1,P2,P3),
    if A2 > 0.00000001 ->
	    SS = ss(P1,P2,P3,Q1,Q2,Q3,A2),
	    ST = st(P1,P2,P3,Q1,Q2,Q3,A2),
	    A = e3d_vec:dot(SS,SS),
	    B = e3d_vec:dot(SS,ST),
	    C = e3d_vec:dot(ST,ST),
	    math:sqrt(0.5*((A+C)+math:sqrt((A-C)*(A-C)+4*B*B)));
       true ->
	    9999999999.9
    end.
    
ss({_,T1},{_,T2},{_,T3},{Q1x,Q1y,Q1z},{Q2x,Q2y,Q2z},{Q3x,Q3y,Q3z},A) 
  when is_float(T1),is_float(T2),is_float(T3),
       is_float(Q1x),is_float(Q1y),is_float(Q1z),
       is_float(Q2x),is_float(Q2y),is_float(Q2z),
       is_float(Q3x),is_float(Q3y),is_float(Q3z) ->
    T23 = T2-T3,    T31 = T3-T1,    T12 = T1-T2,
    {(Q1x*T23+Q2x*T31+Q3x*T12)/A,
     (Q1y*T23+Q2y*T31+Q3y*T12)/A,
     (Q1z*T23+Q2z*T31+Q3z*T12)/A}.
    
st({S1,_},{S2,_},{S3,_},{Q1x,Q1y,Q1z},{Q2x,Q2y,Q2z},{Q3x,Q3y,Q3z},A) 
  when is_float(S1),is_float(S2),is_float(S3),
       is_float(Q1x),is_float(Q1y),is_float(Q1z),
       is_float(Q2x),is_float(Q2y),is_float(Q2z),
       is_float(Q3x),is_float(Q3y),is_float(Q3z) ->
    S32 = S3-S2,    S13 = S1-S3,    S21 = S2-S1,
    {(Q1x*S32+Q2x*S13+Q3x*S21)/A,
     (Q1y*S32+Q2y*S13+Q3y*S21)/A,
     (Q1z*S32+Q2z*S13+Q3z*S21)/A}.

triangulate(Fs,We) ->
    TriWe = wings_tesselation:triangulate(Fs, We),
    TriFs = Fs ++ wings_we:new_items_as_ordset(face, We, TriWe),
    get_face_vspos(TriFs,TriWe, []).

get_face_vspos([Face|Fs], We, Tris) ->
    Vs0 = wpa:face_vertices(Face, We),
    Vs1 = [{V,wings_vertex:pos(V,We)} || V <- Vs0],
    if length(Vs0) == 3 ->
	    Vs2 = [{Vid, {Vx, Vy}} || {Vid,{Vx,Vy,_}} <- Vs1],
	    get_face_vspos(Fs,We,[{Face, Vs2}|Tris]);
       true ->
	    io:format(?__(1,"Error: Face isn't triangulated ~p with ~p vertices")++"~n",
		      [Face, Vs1]),
	    error({triangulation_bug, [Face, Vs1]})    
    end;
get_face_vspos([], _, Tris) ->
    Tris.
