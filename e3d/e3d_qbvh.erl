%%
%%  e3d_qbvh.erl --
%%
%%     Implements a quad-bvh. Quad Bounding volume hierarchies
%%     http://www.uni-ulm.de/fileadmin/website_uni_ulm/iui.inst.100/institut/Papers/QBVH.pdf
%%
%%     This code is not intended to be fast in erlang, 
%%     it is intended to be fast when sent down OpenCL.
%%
%%     The code is inspired from LuxRays variant.
%%
%%  Copyright (c) 2010-2011, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(e3d_qbvh).

-export([init/1, init/2,
	 n_nodes/1, n_quads/1, convert_to_image2d/3
	]).

-export([ray/2, ray/4, ray_trace/2]). 

%%-define(DEBUG,1).
-ifdef(DEBUG).
-export([print_tree/3, print_stack/1, print_split/4]).
-endif.

-include("e3d.hrl").

-import(lists, [reverse/1]).

-define(EPSILON, 0.0001).
-define(MAX_PRIMS_PER_LEAF, 4).
-define(FULL_SWEEP, 8*?MAX_PRIMS_PER_LEAF).
-define(SKIP_FACTOR, 3).
-define(NB_BINS, 8).

-define(F32, 32/float-native).
-define(I32, 32/signed-native).
-define(QNODE_SZ, ((24+4)*4)).
-define(QTRI_SZ,  ((12*3+4)*4)).

-record(qnode, 
	{children, 				% 4 Children 
						% Child = Leaf -> <<IsLeaf:1, NoOfLeafs:4, StartLeaf:27>> 
						%         Node -> child index
	 bboxes  				% 4 Bounding boxes
	}).

-record(ray, 
	{o,d, 					% Origin, Direction vector
	 n, f}).				% Near far (or MinT MaxT)

-record(hit, {t, b1, b2, f = 16#ffffffff}).
-record(qtri, {v,e1,e2,f}).

%%--------------------------------------------------------------------
%% @doc Builds the qbvh tree 
%%   Indata is a list of {NumberOfFaces, GetVs(FaceIndex)} tuples.
%%   GetVs should return {VertexPos1,VertexPos2,VertexPos2}
%%   Face indecies are 0 numbered.
%% @end
%%--------------------------------------------------------------------

-spec init([{NoFs :: integer(), GetVs :: function()}]) -> 
		  {BB::e3d_bbox(), QTree :: binary(), QTris :: binary(), Map :: term()}.
init(Data) ->    
    init(Data, []).

init(FaceData, Opts)  ->
    Eps = proplists:get_value(epsilon, Opts, ?EPSILON),
    {PrimBBs, PrimCs, WorldBB, CentriodBB} = init_bb(FaceData, Eps),
    NPrims = array:size(PrimBBs),
    
    %% Init index map and add 3 primitives at the end for the last quad
    PIndx0 = array:from_list(lists:seq(0, NPrims-1)),
    PIndx1 = array:set(NPrims, 0, PIndx0),
    PIndx2 = array:set(NPrims+1, 0, PIndx1),
    PIndx  = array:set(NPrims+2, 0, PIndx2),
    Nodes0 = {array:new([{default, create_empty_node()}]), 0},
    {PrimIndx, {Nodes,_NodeLen}} = 
	build_tree(0, NPrims, Nodes0, PIndx, 
		   PrimBBs, PrimCs, WorldBB, CentriodBB, -1, 0, 0),
    
%%    io:format("~nNodes ~p: ~p~n",[NPrims, array:to_list(PrimIndx)]),
%%    io:format("Tree ~p: ~p~n",[_NodeLen, array:to_list(Nodes)]),
%%    print_tree(Nodes, PrimIndx, WorldBB),
    
    {Tree, {_N, Tris}} = swap_prims(Nodes, PrimIndx, FaceData),
    {WorldBB, array:foldl(fun binary_node/3, <<>>, Tree), Tris, []}.


n_nodes(Qnodes) ->
    0 = byte_size(Qnodes) rem (6*4+4)*4,
    byte_size(Qnodes) div (6*4+4)*4.

n_quads(Quads) ->
    0 = byte_size(Quads) rem (9*4+4)*4,
    byte_size(Quads) div (9*4+4)*4.

%% Convert the nodes to fit a image
convert_to_image2d({MaxW,MaxH}, QNs, QQs) ->
    {NDim={NodeW,NodeH}, LDim={LeafW,LeafH}} = calc_image_sizes(QNs,QQs),
    MaxH >= NodeH andalso MaxH >= LeafH orelse exit(no_image_support),
    MaxW >= NodeW andalso MaxW >= LeafW orelse exit(no_image_support),

    IQns = convert_to_image(NodeW, QNs, <<>>),
    IQqs = <<QQs/binary, 0:(LeafW*LeafH*10*4*8)>>,
    {NDim, IQns, LDim, IQqs}.

convert_to_image(W, <<BBs:(24*4)/binary, Qns0/binary>>, Acc0) ->
    Copy = <<Acc0/binary, BBs/binary>>,
    {Qns, Acc} = convert_children(0, W, Qns0, Copy),
    convert_to_image(W, Qns, Acc);
convert_to_image(_, <<>>, Acc) -> Acc.

convert_children(4, _W, Qns, Acc) ->  {Qns, Acc};
convert_children(J, W, <<16#ffffffff:?I32, Qns/binary>>, Acc) ->  %% EMPTY
    convert_children(J+1, W, Qns, <<Acc/binary, 16#ffffffff:?I32>>);
convert_children(J, W, <<C0:?I32, Qns/binary>>, Acc) when C0 < 0 -> %% IS_LEAF
    Count = first_quad(C0) * 10,
    X = (Count rem W) div 10, %% "div 10" in order to not waste bits
    Y = Count div W,
    C = 16#80000000 bor (((no_quads(C0)-1) band 16#F) bsl 27) bor
	((X bsl 16) bor Y)  band 16#7ffffff,
    convert_children(J+1, W, Qns, <<Acc/binary, C:?I32>>);
convert_children(J, W, <<C0:?I32, Qns/binary>>, Acc) ->
    Count = C0*7,
    X = (Count rem W) div 7, %% "/ 7" in order to not waste bits
    Y = Count div W,
    C = (X bsl 16) bor Y,
    convert_children(J+1, W, Qns, <<Acc/binary, C:?I32>>).

%%--------------------------------------------------------------------
%% @doc Creates a ray
%% @end
%%--------------------------------------------------------------------

ray(Orig, Vector) ->
    ray(Orig, Vector, ?EPSILON*10, ?E3D_INFINITY).
ray(Orig, Vector, MinT, MaxT) ->
    #ray{o=Orig, d=Vector, n=MinT, f=MaxT}.

ray_trace(Ray = #ray{d=Dir}, {_BB, Tree, Tris, _Map}) ->   
%%    io:format("Dir ~p ~n", [Ray]),
    Swap = inv_sign(Dir),
    ray_trace(Ray, [0], #hit{}, Swap, Tree, Tris).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_bb(Data, Eps) when is_list(Data) ->
    init_bb(lists:reverse(Data), Eps, [], [], e3d_bv:box(), e3d_bv:box()).

init_bb([{NrFs, Fun}|Rest], Eps, BBs, Cs, WBB0, CBB0) ->
    {PrimBBs, PrimCs, WorldBB, CentriodBB} = 
	init_bb_1(NrFs-1, Fun, Eps, BBs, Cs, WBB0, CBB0),
    init_bb(Rest, Eps, PrimBBs, PrimCs, WorldBB, CentriodBB);
init_bb([], _Eps, PrimBBs, PrimCs, WorldBB, CentriodBB) ->
    {array:from_list(PrimBBs), array:from_list(PrimCs),
     WorldBB, CentriodBB}.

init_bb_1(FaceIndex, GetVs, Eps, BBs, Cs, WBB0, CBB0) 
  when FaceIndex >= 0 ->
    {V1,V2,V3} = GetVs(FaceIndex),
    BB = e3d_bv:box([V1,V2,V3], Eps),
    Center = e3d_bv:center(BB),
    WBB = e3d_bv:union(WBB0, BB),
    CBB = e3d_bv:union(CBB0, Center),
    init_bb_1(FaceIndex-1, GetVs, Eps, [BB|BBs], [Center|Cs], WBB, CBB);
init_bb_1(_, _, _Eps, BBs, Cs, WBB0, CBB0) ->
    {BBs, Cs, WBB0, CBB0}.

build_tree(Start, End, Nodes, PrimIndx, _PrimBBs, _PrimCs, NodeBox, 
	   _CentriodBB, Parent, Child, _Depth) 
  when (End - Start) =< ?MAX_PRIMS_PER_LEAF ->
    {PrimIndx, create_tmp_leaf(Parent, Child, Start, End, NodeBox, Nodes)};
build_tree(Start, End, Nodes0, PrimIndx0, PrimBBs, PrimCs, 
	   NodeBox, CentriodBB = {Cmin,Cmax}, Parent, Child, Depth) ->
    Step = if (End - Start) < ?FULL_SWEEP  -> 1; true -> ?SKIP_FACTOR end,
    %% Find axis to do the split
    case e3d_bv:max_extent(CentriodBB) of
	undefined ->
	    io:format("BB ~p~n",[CentriodBB]),
	    %% If the bbox is a point, create a leaf, hoping there are
	    %% not more than 64 primitives that share the same center.
	    case (End - Start) > 64 of
		true  -> exit({geometry_error,
			       too_many_faces_with_same_center});
		false ->
		    {PrimIndx0, create_tmp_leaf(Parent, Child, Start, End,
						NodeBox, Nodes0)}
	    end;
	Axis ->
	    K0 = CminAxis = element(Axis, Cmin),
	    CdistAxis = (element(Axis, Cmax) - K0),
	    K1 = ?NB_BINS / CdistAxis,
	    case Depth rem 2 of
		0 ->
		    {Curr, Nodes1} = 
			create_node(Parent, Child, NodeBox, Nodes0),
		    LeftChild = 0,
		    RightChild = 2;
		1 ->
		    Nodes1 = Nodes0,
		    Curr = Parent,
		    LeftChild = Child,
		    RightChild = Child+1
	    end,
	    MinBin = binning(Start,End,Step,Axis,K0,K1,
			     PrimIndx0,PrimCs,PrimBBs,
			     erlang:make_tuple(?NB_BINS, 0),
			     erlang:make_tuple(?NB_BINS, e3d_bv:box())),
	    %% The split plane coordinate is the coordinate of the end of
	    %% the chosen bin along the split axis	    
	    SplitPos = CminAxis + (MinBin + 1) * CdistAxis/?NB_BINS,
	    Empty = e3d_bv:box(),
	    {Store, PrimIndx1, LBB, LCBB, RBB, RCBB} = 
		split(Start, End, Start, SplitPos, Axis, 
		      PrimIndx0, PrimBBs, PrimCs, 
		      Empty, Empty, Empty, Empty),
	    %% io:format("~p ~f: ~n",[Axis, SplitPos]),
	    %% print_split(Start,Store,End,PrimIndx1),
	    {PrimIndx, Nodes} = 
		build_tree(Start, Store, Nodes1, PrimIndx1, PrimBBs, PrimCs, 
			   LBB, LCBB, Curr, LeftChild, Depth+1),
	    build_tree(Store, End, Nodes, PrimIndx, PrimBBs, PrimCs, 
		       RBB, RCBB, Curr, RightChild, Depth+1)
    end.

create_node(Parent, _Child, _NodeBox, {Nodes, NNodes}) when Parent < 0 ->
    {NNodes, {Nodes, NNodes+1}};
create_node(Parent, Child, NodeBox, {Nodes, NNodes}) ->
    #qnode{bboxes=BBs,children=Ch} = array:get(Parent, Nodes),
    Updated = #qnode{bboxes   = setelement(Child+1, BBs, NodeBox),
		     children = setelement(Child+1, Ch, NNodes)},
    {NNodes, {array:set(Parent, Updated, Nodes), NNodes+1}}.

create_tmp_leaf(Parent, Child, Start, End, NodeBox, {Nodes, NNodes}) 
  when Parent < 0 ->
    %% The leaf is directly encoded in the intermediate node.
    create_tmp_leaf(0, Child, Start, End, NodeBox, {Nodes, NNodes});
create_tmp_leaf(Parent, Child, Start, End, NodeBox, {Nodes, NNodes}) ->
    #qnode{bboxes=BBs,children=Ch} = array:get(Parent, Nodes),
    Quads = ((End - Start)+3) div 4,
    Updated = #qnode{bboxes   = setelement(Child+1, BBs, NodeBox),
		     children = setelement(Child+1, Ch, {true, Quads, Start})},
    {array:set(Parent, Updated, Nodes), NNodes}.

create_empty_node() ->
    Empty = e3d_bv:box(),
    EmptyLeaf = -1, 
    #qnode{bboxes   = {Empty,Empty,Empty,Empty},
	   children = {EmptyLeaf, EmptyLeaf, EmptyLeaf, EmptyLeaf}}.

swap_prims(Nodes, PrimIndx, Data) ->
    swap_prims(0, array:get(0, Nodes), 1, Nodes, {0, <<>>}, PrimIndx, Data).

swap_prims(Index, N0=#qnode{children=Children}, I, Nodes0, 
	   Prims0, PrimIndx, Data) 
  when I < 5 ->
    case element(I, Children) of
	-1 -> 
	    swap_prims(Index, N0, I+1, Nodes0, Prims0, PrimIndx, Data);
	Leaf = {true, _Quads, _Start} ->
	    {NewLeaf, Prims} = swap_leaf(Leaf, Prims0, PrimIndx, Data),
	    Node = setelement(I, Children, NewLeaf),
	    swap_prims(Index, N0#qnode{children=Node}, I+1, Nodes0, 
		       Prims, PrimIndx, Data);
	NodeIndx ->
	    {Nodes,Prims} = 
		swap_prims(NodeIndx, array:get(NodeIndx, Nodes0), 1, 
			   Nodes0, Prims0, PrimIndx, Data),
	    swap_prims(Index, N0, I+1, Nodes, Prims, PrimIndx, Data)
    end;
swap_prims(Index, Node, 5, Nodes, Prims, _PrimIndx, _Data) ->
    {array:set(Index, Node, Nodes), Prims}.

swap_leaf({true, Quads, Start}, {NQuads, Prims0}, PrimIndx, Data) ->
    Leaf = 16#80000000 bor (((Quads-1) band 16#F) bsl 27) 
	bor (NQuads band 16#7ffffff),
    Prims = swap_leafs(Quads, NQuads, Start, Prims0, PrimIndx, Data),
    {Leaf, {NQuads+Quads, Prims}}.

swap_leafs(NbQuads, Num, Offset, Prims, PrimIndx, Data) 
  when NbQuads > 0 ->
    QT = quad_tri(Offset, PrimIndx, Data),
    swap_leafs(NbQuads-1, Num+1, Offset+4, 
	       <<Prims/binary, QT/binary>>, PrimIndx, Data);
swap_leafs(_NbQuads, _Num, _Offset, Prims, _PrimIndx, _Data) ->
    Prims.
    
quad_tri(Offset, PrimIndx, Data) ->
    Prim1 = array:get(Offset+0, PrimIndx),
    {{V00x,V00y,V00z},{V01x,V01y,V01z},{V02x,V02y,V02z}} = 
	get_vs(Prim1, Data),
    Prim2 = array:get(Offset+1, PrimIndx),
    {{V10x,V10y,V10z},{V11x,V11y,V11z},{V12x,V12y,V12z}} = 
	get_vs(Prim2, Data),
    Prim3 = array:get(Offset+2, PrimIndx),
    {{V20x,V20y,V20z},{V21x,V21y,V21z},{V22x,V22y,V22z}} = 
	get_vs(Prim3, Data),
    Prim4 = array:get(Offset+3, PrimIndx),
    {{V30x,V30y,V30z},{V31x,V31y,V31z},{V32x,V32y,V32z}} = 
	get_vs(Prim4, Data),
%%    io:format("~w ",[[Prim1,Prim2,Prim3,Prim4]]),
    <<V00x:?F32,V10x:?F32,V20x:?F32,V30x:?F32,
      V00y:?F32,V10y:?F32,V20y:?F32,V30y:?F32,
      V00z:?F32,V10z:?F32,V20z:?F32,V30z:?F32,

      (V01x-V00x):?F32,(V11x-V10x):?F32,(V21x-V20x):?F32,(V31x-V30x):?F32, 
      (V01y-V00y):?F32,(V11y-V10y):?F32,(V21y-V20y):?F32,(V31y-V30y):?F32,
      (V01z-V00z):?F32,(V11z-V10z):?F32,(V21z-V20z):?F32,(V31z-V30z):?F32,

      (V02x-V00x):?F32,(V12x-V10x):?F32,(V22x-V20x):?F32,(V32x-V30x):?F32, 
      (V02y-V00y):?F32,(V12y-V10y):?F32,(V22y-V20y):?F32,(V32y-V30y):?F32, 
      (V02z-V00z):?F32,(V12z-V10z):?F32,(V22z-V20z):?F32,(V32z-V30z):?F32,

      Prim1:?I32, Prim2:?I32, Prim3:?I32, Prim4:?I32
    >>.

get_vs(Prim0, Data) ->
    {Prim, GetVs} = locate_data(Prim0, Data),
    GetVs(Prim).

locate_data(Prim, [{Id, Data}|_]) 
  when Prim < Id ->
    {Prim, Data};
locate_data(Prim, [{Id,_}|Rest]) ->
    locate_data(Prim-Id, Rest).

binary_node(_, #qnode{children={C0,C1,C2,C3}, 
		      bboxes={{B0min,B0max},
			      {B1min,B1max},
			      {B2min,B2max},
			      {B3min,B3max}
			     }}, Bin) ->
    {V00x,V00y,V00z} = B0min, {V01x,V01y,V01z} = B0max, 
    {V10x,V10y,V10z} = B1min, {V11x,V11y,V11z} = B1max,
    {V20x,V20y,V20z} = B2min, {V21x,V21y,V21z} = B2max,
    {V30x,V30y,V30z} = B3min, {V31x,V31y,V31z} = B3max,
    <<Bin/binary, 
      V00x:?F32,V10x:?F32,V20x:?F32,V30x:?F32,
      V00y:?F32,V10y:?F32,V20y:?F32,V30y:?F32,
      V00z:?F32,V10z:?F32,V20z:?F32,V30z:?F32,
      V01x:?F32,V11x:?F32,V21x:?F32,V31x:?F32,
      V01y:?F32,V11y:?F32,V21y:?F32,V31y:?F32,
      V01z:?F32,V11z:?F32,V21z:?F32,V31z:?F32,
      C0:?I32,C1:?I32, C2:?I32, C3:?I32>>.

%% Binning is relative to the centroids bbox and to the
%% primitives' centroid.
binning(Start, End, Step, Axis,K0,K1,PrimIndx,PrimCs, PrimBB, Bins, BinBBs) 
  when Start < End ->
    Index = array:get(Start, PrimIndx),
    BinPos = ceil(K1 * (element(Axis, array:get(Index, PrimCs)) - K0)),
    Id = erlang:min(?NB_BINS, BinPos),
    BinBB = e3d_bv:union(element(Id,BinBBs), array:get(Index,PrimBB)),
    binning(Start + Step, End, Step, Axis,K0,K1,PrimIndx,PrimCs,PrimBB, 
	    setelement(Id, Bins, 1+element(Id, Bins)),
	    setelement(Id, BinBBs, BinBB));
binning(_Start, _End, _Step, _Axis,_K0,_K1,
	_PrimIndx,_PrimCs, _PrimBB, BinsT, BinBBsT) ->
    %% Evaluate Split
    Bins = tuple_to_list(BinsT),
    BinBBs = tuple_to_list(BinBBsT),    
    LeftEstimates = count_binning(Bins, BinBBs, 0, e3d_bv:box(), []),
    RightEstimates = count_binning(reverse(Bins), reverse(BinBBs), 
				   0, e3d_bv:box(), []),
    find_minbox(0, reverse(LeftEstimates), tl(RightEstimates),
		?E3D_INFINITY, undefined).
    
count_binning([Bin|Bins], [BinBB|BinBBs], CurrNb0, CurrBB0, Est) ->
    CurrNb = Bin + CurrNb0,
    CurrBB = e3d_bv:union(CurrBB0, BinBB),
    Area = e3d_bv:surface_area(CurrBB),
    count_binning(Bins, BinBBs, CurrNb, CurrBB, [Area*CurrNb|Est]);
count_binning([], [], _CurrNb, _CurrBB, Volumes) ->
    Volumes.

find_minbox(I, [Left|Ls], [Right|Rs], MinCost, Bin) ->
    case Left + Right of
	Cost when Cost < MinCost ->
	    find_minbox(I+1, Ls, Rs, Cost, I);
	_ ->
	    find_minbox(I+1, Ls, Rs, MinCost, Bin)
    end;
find_minbox(_, _, [], _MinCost, Bin) ->
    Bin.

ceil(X) when is_float(X) ->
    round(0.5+X).

%% Split 
split(I, End, Store, SplitPos, Axis, PrimIndx0, PrimBBs, PrimCs, 
      LBB0, LCBB0, RBB0, RCBB0) 
  when I < End ->
    Index = array:get(I, PrimIndx0),
    case element(Axis, array:get(Index, PrimCs)) =< SplitPos of
	true ->
	    PrimIndx1 = array:set(I, array:get(Store, PrimIndx0), PrimIndx0),
	    PrimIndx  = array:set(Store, Index, PrimIndx1),
	    
	    LBB  = e3d_bv:union(LBB0, array:get(Index, PrimBBs)),
	    LCBB = e3d_bv:union(LCBB0, array:get(Index, PrimCs)),
	    split(I+1, End, Store+1, SplitPos, Axis, PrimIndx, PrimBBs, PrimCs, 
		  LBB, LCBB, RBB0, RCBB0);
	false ->
	    RBB  = e3d_bv:union(RBB0, array:get(Index, PrimBBs)),
	    RCBB = e3d_bv:union(RCBB0, array:get(Index, PrimCs)),
	    split(I+1, End, Store, SplitPos, Axis, PrimIndx0, PrimBBs, PrimCs, 
		  LBB0, LCBB0, RBB, RCBB)
    end;
split(_I, _End, Store, _SplitPos, _Axis, PrimIndx, _PrimBBs, _PrimCs, 
      LBB, LCBB, RBB, RCBB) ->
    {Store, PrimIndx, LBB, LCBB, RBB, RCBB}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DEBUGGING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_leaf(Index)  -> Index < 0.
is_empty(Index) -> Index =:= 16#ffffffff.
no_quads(Index) -> ((Index bsr 27) band 16#F) + 1.
first_quad(Index) -> Index band 16#7ffffff.

inv_sign({X,Y,Z}) -> 
    {inv(X),inv(Y),inv(Z),
     X < 0.0, Y < 0.0, Z < 0.0}.
inv(N) -> 
    try 1.0/N
    catch _:_ ->
	    ?E3D_INFINITY
    end.

ray_trace(Ray, [NodeData|Stack], Hit, Swap, Tree, Tris)
  when length(Stack) < 24 ->  %% Hardcoded limit in opencl code
    case is_leaf(NodeData) of
	false ->
	    Node = get_node(NodeData, Tree),
	    Visit = qbbox_intersect(Ray, Node, Swap),
	    S = add_stack(Visit, Node#qnode.children, Stack),
	    ray_trace(Ray, S, Hit, Swap, Tree, Tris);
	true ->
	    QuadPrims = no_quads(NodeData),
	    Offset    = first_quad(NodeData),
	    {R,H} = qtri_intersect(Offset, Offset+QuadPrims, Ray,Hit, Tris),
	    ray_trace(R,Stack,H,Swap,Tree,Tris)
    end;
ray_trace(_, [], Hit, _, _, _) -> Hit.

qbbox_intersect(#ray{o={X,Y,Z}, n=N,f=F},
		#qnode{bboxes={X0,Y0,Z0,X1,Y1,Z1}},
		{InvX,InvY,InvZ,SwapX,SwapY,SwapZ}) ->
    %% Swap signs
    {Xmin,Xmax} = if SwapX -> {X1,X0}; true -> {X0,X1} end,
    {Ymin,Ymax} = if SwapY -> {Y1,Y0}; true -> {Y0,Y1} end,
    {Zmin,Zmax} = if SwapZ -> {Z1,Z0}; true -> {Z0,Z1} end,

    Min0 = [N,N,N,N],
    Max0 = [F,F,F,F],
    Max = fun(In,C,MC,Inv) -> max(In, (MC-C)*Inv) end,
    Min = fun(In,C,MC,Inv) -> min(In, (MC-C)*Inv) end,
    %% X coord
    MinX = apply2c2(Min0,X,Xmin,InvX,Max), % max(Min0, (Xmin-X)*InvX)
    MaxX = apply2c2(Max0,X,Xmax,InvX,Min),
    %%io:format("BBox X ~p~n", [apply2(MaxX, MinX, fun(MaxV,MinV) -> MaxV >= MinV end)]),
    %% Y coord
    MinY = apply2c2(MinX,Y,Ymin,InvY,Max),
    MaxY = apply2c2(MaxX,Y,Ymax,InvY,Min),
    %%io:format("BBox Y ~p~n", [apply2(MaxY, MinY, fun(MaxV,MinV) -> MaxV >= MinV end)]),
    %% Z coord
    MinZ = apply2c2(MinY,Z,Zmin,InvZ,Max),
    MaxZ = apply2c2(MaxY,Z,Zmax,InvZ,Min),    
    %%    io:format("BBox intersect ~p~n", [apply2(MaxZ, MinZ, fun(MaxV,MinV) -> MaxV >= MinV end)]),
    apply2(MaxZ, MinZ, fun(MaxV,MinV) -> MaxV >= MinV end).

add_stack([true|Visit], [Ch|Chs], Stack) ->
    case is_empty(Ch) of
	true  -> add_stack(Visit,Chs,Stack);
	false -> 
%%	    print_stack([Ch|Stack]),
	    add_stack(Visit,Chs,[Ch|Stack])
    end;
add_stack([_|Visit], [_|Chs], Stack) ->
    add_stack(Visit,Chs,Stack);
add_stack([],[],Stack) -> Stack.

qtri_intersect(Offset, Size, Ray0, Hit0, Tris) 
  when Offset < Size ->
    Tri = get_qtriangles(Offset, Tris),
    {Ray,Hit} = qtri_intersect(Ray0, Tri, Hit0),
    qtri_intersect(Offset+1, Size, Ray, Hit, Tris);
qtri_intersect(_, _, Ray, Hit, _) -> {Ray, Hit}.

qtri_intersect(#ray{d={DirX,DirY,DirZ}, 
		    o={Ox,Oy,Oz},
		    n=Near,f=Far} = Ray0,
	       #qtri{v  = {Tx,Ty,Tz},
		     e1 = {E1X,E1Y,E1Z},
		     e2 = {E2X,E2Y,E2Z},
		     f  = Fs},
	       Hit0) ->
    %% Calc B1
    Bary = fun(E1,D1,E2,D2) -> (D1*E1) - (D2*E2) end,
    MulAdd = fun(SX,EX,SY,EY,SZ,EZ) ->
		     SX*EX+SY*EY+SZ*EZ
	     end,
    MulAddDiv = fun(SX,EX,SY,EY,SZ,EZ,Div) ->
			try (SX*EX+SY*EY+SZ*EZ)/Div
			catch _:_ -> -1.0 end
		end,
    
    S1X = apply2c2(E2Z,DirY,E2Y,DirZ,Bary),
    S1Y = apply2c2(E2X,DirZ,E2Z,DirX,Bary),
    S1Z = apply2c2(E2Y,DirX,E2X,DirY,Bary),

    Div = apply6(S1X,E1X,S1Y,E1Y,S1Z,E1Z,MulAdd),

    Dx = [Ox-X || X <- Tx],
    Dy = [Oy-Y || Y <- Ty],
    Dz = [Oz-Z || Z <- Tz],
    
    B1 = apply7(Dx,S1X,Dy,S1Y,Dz,S1Z,Div,MulAddDiv),
    %% Calc B2
    
    S2X = apply4(E1Z,Dy,E1Y,Dz,Bary),
    S2Y = apply4(E1X,Dz,E1Z,Dx,Bary),
    S2Z = apply4(E1Y,Dx,E1X,Dy,Bary),
    B2  = apply4c3(S2X,DirX,S2Y,DirY,S2Z,DirZ,Div,MulAddDiv),
    
    %% Calc. B0
    B0  = apply2(B1, B2, fun(Bx,By) -> 1.0 - Bx - By end),
    
    %% Calc T
    T = apply7(E2X,S2X,E2Y,S2Y,E2Z,S2Z,Div, MulAddDiv),
    
    case check_qtris(Div,B0,B1,B2,T,Fs,Near,Far,undefined) of
	undefined -> 
	    {Ray0,Hit0};
	Hit ->
%	    io:format(" Hit ~p~n", [Hit]),
	    {Ray0#ray{f=Hit#hit.t},Hit}
    end.

check_qtris([D|Ds],[B0|B0s],[B1|B1s],[B2|B2s],[T|Ts],[Id|Ids],N,F,Prev) ->
    if D =/= 0.0, B0 >= 0.0, B1 >= 0.0, B2 >= 0.0,
       T > N, T < F ->
	    Hit = #hit{t=T,b1=B1,b2=B2,f=Id},
	    check_qtris(Ds,B0s,B1s,B2s,Ts,Ids,N,T,Hit);
       true ->
	    check_qtris(Ds,B0s,B1s,B2s,Ts,Ids,N,F,Prev)
    end;
check_qtris([],[],[],[],[],[],_,_,Hit) -> Hit.

%% Helpers

apply2([A,B,C,D],[X,Y,Z,W], Fun) ->
    [Fun(A,X), Fun(B,Y), Fun(C,Z), Fun(D,W)].

apply2c2([A,B,C,D],R, [X,Y,Z,W], I, Fun) ->
    [Fun(A,R,X,I), Fun(B,R,Y,I), Fun(C,R,Z,I), Fun(D,R,W,I)].

apply4([A1,B1,C1,D1],[A2,B2,C2,D2], [A3,B3,C3,D3], [A4,B4,C4,D4], Fun) ->
    [Fun(A1,A2,A3,A4), Fun(B1,B2,B3,B4), 
     Fun(C1,C2,C3,C4), Fun(D1,D2,D3,D4)].

apply6([A1,B1,C1,D1], [A2,B2,C2,D2], 
       [A3,B3,C3,D3], [A4,B4,C4,D4], 
       [A5,B5,C5,D5], [A6,B6,C6,D6], 
       Fun) ->
    [Fun(A1,A2,A3,A4,A5,A6), Fun(B1,B2,B3,B4,B5,B6), 
     Fun(C1,C2,C3,C4,C5,C6), Fun(D1,D2,D3,D4,D5,D6)].

apply7([A1,B1,C1,D1], [A2,B2,C2,D2], 
       [A3,B3,C3,D3], [A4,B4,C4,D4], 
       [A5,B5,C5,D5], [A6,B6,C6,D6], [A7,B7,C7,D7], 
       Fun) ->
    [Fun(A1,A2,A3,A4,A5,A6,A7), Fun(B1,B2,B3,B4,B5,B6,B7), 
     Fun(C1,C2,C3,C4,C5,C6,C7), Fun(D1,D2,D3,D4,D5,D6,D7)].

apply4c3([A1,B1,C1,D1], X, 
	 [A3,B3,C3,D3], Y, 
	 [A5,B5,C5,D5], Z, 
	 [A7,B7,C7,D7], 
	 Fun) ->
    [Fun(A1,X,A3,Y,A5,Z,A7), Fun(B1,X,B3,Y,B5,Z,B7), 
     Fun(C1,X,C3,Y,C5,Z,C7), Fun(D1,X,D3,Y,D5,Z,D7)].


get_node(Index, Tree) ->
    Skip = (Index*?QNODE_SZ),
    <<_:Skip/binary, 
      V00x:?F32,V10x:?F32,V20x:?F32,V30x:?F32,
      V00y:?F32,V10y:?F32,V20y:?F32,V30y:?F32,
      V00z:?F32,V10z:?F32,V20z:?F32,V30z:?F32,
      
      V01x:?F32,V11x:?F32,V21x:?F32,V31x:?F32,
      V01y:?F32,V11y:?F32,V21y:?F32,V31y:?F32,
      V01z:?F32,V11z:?F32,V21z:?F32,V31z:?F32,
      
      C0:?I32,C1:?I32, C2:?I32, C3:?I32, 
      _/binary>> = Tree,
    #qnode{children=[C0,C1,C2,C3], 
     	   bboxes={[V00x,V10x,V20x,V30x],
		   [V00y,V10y,V20y,V30y],
		   [V00z,V10z,V20z,V30z],
		   [V01x,V11x,V21x,V31x],
		   [V01y,V11y,V21y,V31y],
		   [V01z,V11z,V21z,V31z]}}.

get_qtriangles(Index, Binary) ->
    Skip = (Index*?QTRI_SZ),
    <<_:Skip/binary, 
      V00x:?F32,V10x:?F32,V20x:?F32,V30x:?F32,
      V00y:?F32,V10y:?F32,V20y:?F32,V30y:?F32,
      V00z:?F32,V10z:?F32,V20z:?F32,V30z:?F32,
      
      E00x:?F32,E10x:?F32,E20x:?F32,E30x:?F32,
      E00y:?F32,E10y:?F32,E20y:?F32,E30y:?F32,
      E00z:?F32,E10z:?F32,E20z:?F32,E30z:?F32,
      
      E01x:?F32,E11x:?F32,E21x:?F32,E31x:?F32,
      E01y:?F32,E11y:?F32,E21y:?F32,E31y:?F32,
      E01z:?F32,E11z:?F32,E21z:?F32,E31z:?F32,
      
      Prim1:?I32, Prim2:?I32, Prim3:?I32, Prim4:?I32,
      _/binary>> = Binary,
    #qtri{v  = {[V00x,V10x,V20x,V30x],
		[V00y,V10y,V20y,V30y],
		[V00z,V10z,V20z,V30z]},
	  e1 = {[E00x,E10x,E20x,E30x],
		[E00y,E10y,E20y,E30y],
		[E00z,E10z,E20z,E30z]},
	  e2 = {[E01x,E11x,E21x,E31x],
		[E01y,E11y,E21y,E31y],
		[E01z,E11z,E21z,E31z]},
	  f  = [Prim1,Prim2, Prim3, Prim4]}.

calc_image_sizes(Qnodes, Qtris) ->
    NodePixelsReq = e3d_qbvh:n_nodes(Qnodes) * 7,
    NodeW = min(roundup(trunc(math:sqrt(NodePixelsReq)), 7), 16#7fff),
    NH1 = if (NodePixelsReq rem NodeW) == 0 -> 0; true -> 1 end,
    NodeH = NodePixelsReq div NodeW + NH1,

    LeafPixelsReq = e3d_qbvh:n_quads(Qtris) * 10,
    LeafW = min(roundup(trunc(math:sqrt(LeafPixelsReq)), 10), 32760),
    LH1 = if (LeafPixelsReq rem LeafW) == 0 -> 0; true -> 1 end,
    LeafH = LeafPixelsReq div LeafW + LH1,
    {{NodeW, NodeH}, {LeafW,  LeafH}}.

roundup(Size, Div) ->
    R = Size rem Div,
    if R == 0 -> Size;
       true -> Size+Div-R
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(DEBUG).
print_tree(Nodes, PrimIndx, WorldBB) ->
    print_tree(0, Nodes, PrimIndx, WorldBB, 1).

print_tree(Index, Nodes, PrimIndx, Box, Level) ->
    #qnode{children=Children, bboxes=BBs} = array:get(Index, Nodes),
    [print_child(C, Nodes, PrimIndx, Box, Level) 
     || C <- lists:zip(tuple_to_list(Children), tuple_to_list(BBs))].
    
print_child({-1,_BB}, _, _, _, Level) ->
    io:format("~*.1c empty~n", [Level,$:]);
print_child({{true, _Quads, Start},BB}, _, PrimIndx, PBB, Level) ->
    check_bb(PBB,BB),
    io:format("~*.1c ~p ~p ~p ~p ~s~n", 
	      [Level, $:|[array:get(Start+I,PrimIndx) || I <- lists:seq(0,3)]] ++ [f(BB)]);
print_child({Node,BB}, Nodes, PrimIndx, PBB, Level) ->
    check_bb(PBB, BB),
    io:format("~*.1c ~p ~s~n", [Level,$:, Node, f(BB)]),
    print_tree(Node, Nodes, PrimIndx, BB, Level+2). 

check_bb(PBB,BB) -> check_bb(c, PBB,BB). 
check_bb(What, PBB, BB) ->   %% Assert tree while printing it
    case e3d_bv:union(PBB, BB) of
	PBB -> ok;
	_Other -> 
	    io:format("Bad bounding box:~p ~n >>> ~s~n <<< ~s~n", 
		      [What, f(PBB), f(BB)])
    end.

f({B1,B2}) -> 
    "<" ++ f(B1) ++ "," ++ f(B2) ++ ">";
f({X,Y,Z}) ->
    io_lib:format("{~.2g, ~.2g, ~.2g}", [X,Y,Z]);
f(X) when is_float(X) ->
    io_lib:format("~.3g", [X]);
f(X) when is_integer(X) ->
    io_lib:format("~p", [X]);
f(X) when is_list(X) ->
    "[" ++ [f(E) ++ "," || E <- X] ++ "]".


print_split(Start,Store,End, PrimIndx) ->
    io:format("Left: [",[]),
    [io:format("~p ", [array:get(Id,PrimIndx)]) 
     || Id <- lists:seq(Start,Store-1)],
    io:format("]~n   Right: [",[]),
    [io:format("~p ", [array:get(Id,PrimIndx)]) 
     || Id <- lists:seq(Store,End-1)],
    io:format("]~n",[]).

print_stack([Top|Rest]) ->
    case is_leaf(Top) of
	true ->
	    io:format(" leaf ~p(~p), ", [first_quad(Top), no_quads(Top)]);
	false ->
	    io:format(" node ~p, ",[Top])
    end,
    print_stack(Rest);
print_stack([]) ->
    io:format("~n",[]).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

