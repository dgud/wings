%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2016, Dan Gudmundsson
%%% @doc Bounding Volume Hierarchy
%%%      Simple tree for ray-testing or intersections
%%%      Output can be term() tree or a binary() for OpenCL or for parallel raytracing
%%% @end
%%% Created : 29 Apr 2016 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(e3d_bvh).
-export([init/1, init/2,
	 ray/2, ray/4, ray_trace/2, is_inside/2,
	 intersect/2,
	 hit_triangle/5, tri_intersect/6]).

-include("e3d.hrl").

-type hit() ::
	#{t  => float(),
	  b1 => float(),
	  b2 => float(),
	  mesh => integer(),
	  face => integer(),
          tri => {e3d_vec:point(),e3d_vec:point(),e3d_vec:point()}
         }.

-type mf() :: {Mesh::term(),Face::integer()}.

-type tri_intersect() ::  %% The (new) edge that intersects the two triangle
	#{p1    => e3d_vec:point(),  %% Edge point 1
	  p2    => e3d_vec:point(),  %% Edge point 2
	  mf1   => mf(),  %% Mesh and Face of P1
	  mf2   => mf(),  %% Mesh and Face of P2
	  other => mf()}. %% Intersecting Mesh and Face

-type e3d_bvh() :: #{vs => #{integer()=>array:array()} | binary(),
		     ns => tree() | binary()}.

-type e3d_compiled() :: {bvh, compiled, atom()}.

-type leaf() :: #{bb    => e3d_bbox(),
		  c2    => e3d_vec:point(),  %% CenterPos*2
		  vs    => {integer(), integer(), integer()}, %% Vertex indices
		  mesh  => integer(),
		  index => integer()}.

-type tree_node() :: #{bb    => e3d_bbox(),
		       left  => tree_node() | leaf(),
		       right => tree_node() | leaf()}.

-type tree() :: leaf() | tree_node().

-define(F32, 32/float-native).
-define(I32, 32/signed-native).
-define(U32, 32/unsigned-native).

-define(EPSILON, 0.00000001).
-define(IsLeaf(NodeData), ((NodeData) band 16#80000000) > 1).
-define(GetSkipIndex(NodeData), ((NodeData) band 16#7fffffff)).
-define(ENTRY_SZ, 8*4).

-spec init([{NoFs :: integer(), GetVs :: function()}] | e3d_compiled()) -> e3d_bvh().
init({bvh, compiled, Mod}) ->
    #{vs=>Mod:verts(), ns=>Mod:tree()};
init(Data) ->
    init(Data, []).

-spec init([{NoFs :: integer(), GetVs :: function()}], []) -> e3d_bvh() | e3d_compiled().
init(FaceData, Opts)  ->
    TT  = proplists:get_value(treetype, Opts, 4),
    Eps = proplists:get_value(epsilon, Opts, ?EPSILON),
    Nodes= lists:foldl(fun({N, GetVs}, Acc) ->
			       make_nodes(N-1, GetVs, Eps, Acc)
		       end, [], FaceData),
    Root = build_hierarchy(Nodes, TT),
    case proplists:get_value(binary, Opts, false) of
	false  ->
	    Vs0 = [{GetVs(meshId),GetVs(verts)}
		   || {_, GetVs} <- FaceData],
	    Vs = maps:from_list(Vs0),
	    case proplists:get_value(compile, Opts, false) of
		false ->
		    #{vs=>Vs, ns=>Root};
		true ->
		    compile(Root, Vs)
	    end;
	true ->
	    DeepVs0 = [GetVs(verts) || {_, GetVs} <- FaceData],
	    {VsBin,VsOffsets} = build_bin(DeepVs0, <<>>, [0]),
	    {_,A} = build_array(Root, 0, array:new(), VsOffsets),
	    #{vs=>VsBin, ns=>list_to_binary(array:to_list(A))}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a ray
%% @end
%%--------------------------------------------------------------------
-spec ray(e3d_vec:point(), e3d_vec:vector()) -> e3d_ray().
ray(Orig, Vector) ->
    ray(Orig, Vector, ?EPSILON*10, ?E3D_INFINITY).

-spec ray(e3d_vec:point(), e3d_vec:vector(), float(), float()) -> e3d_ray().
ray(Orig, Vector, MinT, MaxT) ->
    #ray{o=Orig, d=Vector, n=MinT, f=MaxT}.

%%--------------------------------------------------------------------
%% @doc Cast a ray on BVH
%% @end
%%--------------------------------------------------------------------
-spec ray_trace(e3d_ray(), e3d_bvh()) -> false | hit().
ray_trace(Ray = #ray{d=Dir}, #{ns:=#{}=Root, vs:=Vs}) ->
    Swap  = e3d_bv:inv_sign(Dir),
    {_, Hit} = erl_ray_trace(Ray, false, Swap, Root, Vs),
    Hit;
ray_trace(Ray = #ray{d=Dir}, #{vs:=VsBin, ns:=Bin}) when is_binary(VsBin) ->
    Swap  = e3d_bv:inv_sign(Dir),
    {_, Hit} = bin_ray_trace(0, Ray, false, Swap, Bin, VsBin, 1),
    Hit.

%%--------------------------------------------------------------------
%% @doc Is Point on the inside of the bvh
%% @end
%%--------------------------------------------------------------------
-spec is_inside(e3d_vec:point(), e3d_bvh()) -> boolean().
is_inside(P, Bvh) ->
    %% Test with random Vector
    Vec0 = {rand:uniform()*2.0-1.0, rand:uniform()*2.0-1.0, rand:uniform()*2.0-1.0},
    Vec = e3d_vec:norm(Vec0),
    Ray = #ray{o=P, d=Vec, n=?EPSILON*2, f=?E3D_INFINITY, bfc=false},
    case ray_trace(Ray, Bvh) of
        false ->
            %% io:format("Miss ~s ~s~n",[e3d_vec:format(P), e3d_vec:format(Vec)]),
            false;
        #{tri:={A,B,C}} = _Dbg ->
            N = e3d_vec:normal(A,B,C),
            Dot = e3d_vec:dot(N,Vec),
            if abs(Dot) < 0.1 ->
                    %% io:format("Retry (~.f)~n",[Dot]),
                    is_inside(P, Bvh);  %% Not sure pick other vector
               true ->
                    %% io:format("~.3f ~s: ~s ~s~n => ~p~n",
                    %%           [Dot, e3d_vec:format(P), e3d_vec:format(Vec), e3d_vec:format(N), _Dbg]),
                    Dot > 0.0
            end
    end.

%%--------------------------------------------------------------------
%% @doc Intersect two BVH's and return new edges on intersecting
%% triangles if any.
%% @end
%% --------------------------------------------------------------------
-spec intersect(e3d_bvh(), e3d_bvh()) -> [tri_intersect()|{coplanar, mf(), mf()}].
intersect(#{ns:=N1, vs:=Vs1}, #{ns:=N2, vs:=Vs2}) ->
    intersect_1(N1, N2, Vs1, Vs2, []).

%% --------------------------------------------------------
%% Internals
%% --------------------------------------------------------

make_nodes(N, GetVs, Eps, Ns) when N >= 0 ->
    Tri  = GetVs({verts, N}),
    Mesh = GetVs(meshId),
    {V1,V2,V3} = get_tri(Tri, GetVs(verts)),
    {Min,Max} = BB = e3d_bv:box([V1,V2,V3], Eps),
    C2 = e3d_vec:average(Min,Max),
    Node = #{bb=>BB, c2=>C2, vs=>Tri, mesh=>Mesh, index=>N},
    make_nodes(N-1, GetVs, Eps, [Node|Ns]);
make_nodes(_, _, _, Acc) ->
    Acc.

build_hierarchy([Node], _TT) -> Node;
build_hierarchy(Nodes, TT) ->
    Split = find_best_split(Nodes),
    case partition(Split, Nodes) of
	{L1,L2} ->
	    #{bb:=LBB} = Left  = build_hierarchy(L1, TT),
	    #{bb:=RBB} = Right = build_hierarchy(L2, TT),
	    #{bb=>e3d_bv:union(LBB,RBB), left=>Left, right=>Right}
    end.

find_best_split(Nodes) ->
    C0 = lists:foldl(fun(#{c2:=C}, Acc) -> e3d_vec:add(C,Acc) end, e3d_vec:zero(), Nodes),
    %io:format("~p~n ~p~n",[C0, Nodes]),
    {Mx,My,Mz} = Mean2 = e3d_vec:divide(C0, float(length(Nodes))),
    {Vx,Vy,Vz} = lists:foldl(fun(#{c2:=C}, Acc) ->
				     {X,Y,Z} = e3d_vec:sub(C, Mean2),
				     V = {X*X,Y*Y,Z*Z},
				     e3d_vec:add(Acc, V)
			     end, e3d_vec:zero(), Nodes),
    %io:format("SPLIT: ~p < ~p~n", [{Vx,Vy,Vz}, Mean2]),
    if Vx =:= Vy, Vx =:= Vz -> %% Perfectly centered use mean
	    if abs(Mx) < abs(Mz), abs(My) < abs(Mz) -> fun(#{c2:={_,_,V}}) -> V < Mz end;
	       abs(Mx) < abs(My) -> fun(#{c2:={_,V,_}}) -> V < My end;
	       true -> fun(#{c2:={V,_,_}}) -> V < Mx end
	    end;
       Vx < Vz, Vy =< Vz ->
	    fun(#{c2:={_,_,V}}) -> V < Mz end;
       Vx < Vy ->
	    fun(#{c2:={_,V,_}}) -> V < My end;
       true ->
	    fun(#{c2:={V,_,_}}) -> V < Mx end
    end.

partition(_, [N1,N2]) ->
    {[N1],[N2]};
partition(Split, Nodes) ->
    lists:partition(Split, Nodes).

build_array(#{bb:=BB, left:=L, right:=R}, Offset0, Array0, VsOffsets) ->
    {Offset1, Array1} = build_array(L, Offset0+1,  Array0, VsOffsets),
    {Offset2, Array2} = build_array(R, Offset1,  Array1, VsOffsets),
    {{V0x,V0y,V0z},{V1x,V1y,V1z}} = BB,
    Bin = <<V0x:?F32, V0y:?F32, V0z:?F32,V1x:?F32, V1y:?F32, V1z:?F32, Offset2:?U32, 0:?U32>>,
    {Offset2, array:set(Offset0, Bin, Array2)};
build_array(#{index:=I, mesh:=MeshId, vs:={V1,V2,V3}}, Offset0, Array0, VsOffsets) ->
    Leaf = (Offset0+1) bor 16#80000000,
    Pad = 0,
    VsOff = lists:nth(MeshId+1, VsOffsets),
    Bin = <<(V1+VsOff):?U32, (V2+VsOff):?U32, (V3+VsOff):?U32,
	    MeshId:?U32, I:?U32, Pad:?U32,  Leaf:?U32, Pad:?U32>>,
    {Offset0+1, array:set(Offset0, Bin, Array0)}.

build_bin([Top|DeepVs0], Acc0, [Prev|_]=Indx) ->
    Acc = array:foldl(fun(_, {X,Y,Z}, Acc) -> <<Acc/binary, X:?F32, Y:?F32, Z:?F32>> end,
		      Acc0, Top),
    Sz = (byte_size(Acc) - byte_size(Acc0)) div 12,
    build_bin(DeepVs0, Acc, [Prev+Sz|Indx]);
build_bin([], Acc, Indx) ->
    {Acc, lists:reverse(Indx)}.

erl_ray_trace(Ray0, Hit0, Swap, #{bb:=BB, left:=L, right:=R}, Vs) ->
    case e3d_bv:hit(Ray0, Swap, BB) of
	true ->
	    {Ray, Hit} = erl_ray_trace(Ray0, Hit0, Swap, L, Vs),
	    erl_ray_trace(Ray, Hit, Swap, R, Vs);
	false ->
	    {Ray0, Hit0}
    end;
erl_ray_trace(R, H, _Swap, #{mesh:=Mesh, index:=I, vs:=Face}, Vs) ->
    hit_triangle(R, H, get_tri(Mesh, Face, Vs), Mesh, I).

bin_ray_trace(Current, Ray0, Hit0, Swap, Bin, Vs, Level) ->
    Offset = (Current*?ENTRY_SZ),
    case Bin of
	<<_:Offset/binary, Data:(6*4)/binary, NodeData:?U32, _/binary>> ->
	    case ?IsLeaf(NodeData) of
		true  ->
		    {Ray, Hit} = bin_tri_hit(Ray0, Hit0, Data, Vs),
		    bin_ray_trace(Current+1, Ray, Hit, Swap, Bin, Vs, Level);
		false ->
		    case bin_bb_intersect(Ray0, Swap, Data) of
			true  -> bin_ray_trace(Current+1, Ray0, Hit0, Swap, Bin, Vs, Level+1);
			false -> bin_ray_trace(NodeData, Ray0, Hit0, Swap, Bin, Vs, Level+1)
		    end
	    end;
	<<_:Offset/binary>> -> {Ray0, Hit0}
    end.

bin_tri_hit(Ray, Hit, <<V1:?U32, V2:?U32, V3:?U32, MeshId:?U32, I:?U32, _:?U32>>, Vs) ->
    <<_:V1/binary-unit:96, X1:?F32, Y1:?F32, Z1:?F32, _/binary>> = Vs,
    <<_:V2/binary-unit:96, X2:?F32, Y2:?F32, Z2:?F32, _/binary>> = Vs,
    <<_:V3/binary-unit:96, X3:?F32, Y3:?F32, Z3:?F32, _/binary>> = Vs,
    V1p = {X1,Y1,Z1},
    V2p = {X2,Y2,Z2},
    V3p = {X3,Y3,Z3},
    hit_triangle(Ray, Hit, {V1p, V2p, V3p}, MeshId, I).

bin_bb_intersect(Ray, Swap, <<MIx:?F32, MIy:?F32, MIz:?F32, MAx:?F32, MAy:?F32, MAz:?F32>>) ->
    e3d_bv:hit(Ray, Swap, {{MIx,MIy,MIz},{MAx,MAy,MAz}}).

%% Woop JCGT 2(1)
%% http://jcgt.org/published/0002/01/05/paper.pdf
hit_triangle(#ray{o=Orig, d=Dir, bfc=BFCull}=Ray, Hit0, % {_,_,Kz} = Order, {_,_,Sz} = Shear,
	     {TA,TB,TC}=Tri, Mesh, Face) ->
    {_,_,Kz} = Order = order(Dir),
    {_,_,Sz} = Shear = shear(Dir,Order),

    %% Calc vs relative ray origin
    A = e3d_vec:sub(TA, Orig),
    B = e3d_vec:sub(TB, Orig),
    C = e3d_vec:sub(TC, Orig),
    %% Shear and scale vs
    {Ax, Ay} = shear_scale(A, Shear, Order),
    {Bx, By} = shear_scale(B, Shear, Order),
    {Cx, Cy} = shear_scale(C, Shear, Order),
    %% Calc scaled barycentric coords
    case bary_centric(Ax,Ay,Bx,By,Cx,Cy, BFCull) of
	false ->
            {Ray,Hit0};
	{U,V,W,Det} ->
	    %% Calc scaled z-coords and use them for hit dist
	    Az = Sz*element(Kz, A),
	    Bz = Sz*element(Kz, B),
	    Cz = Sz*element(Kz, C),
	    T = U*Az + V*Bz + W*Cz,
            case BFCull of
                true when T < 0.0 ->
                    {Ray, Hit0};
                true ->
                    case Hit0 of
                        #{t:=HitT} when T > (HitT*Det) ->
                            {Ray, Hit0};
                        _ ->
                            RcpDet = 1.0 / Det,
                            Hit = #{t=>Far=T*RcpDet,
                                    b1=>V*RcpDet,
                                    b2=>W*RcpDet,
                                    mesh=>Mesh,
                                    face=>Face,
                                    tri=>Tri
                                   },
                            {Ray#ray{f=Far}, Hit}
                    end;
                false ->
                    RcpDet = 1.0 / Det,
                    Far = T*RcpDet,
                    case Hit0 of
                        _ when Far < 0.0 ->
                            %% io:format("    Miss ~.3f < 0.0 ~n",[Far]),
                            {Ray, Hit0};
                        #{t:=HitT} when (Far > HitT) ->
                            %% io:format("    Miss ~.3f > ~.3f~n",[Far, HitT]),
                            {Ray, Hit0};
                        _ ->
                            Hit = #{t=>Far,
                                    b1=>V*RcpDet,
                                    b2=>W*RcpDet,
                                    mesh=>Mesh,
                                    face=>Face,
                                    tri=>Tri
                                   },
                            {Ray#ray{f=Far}, Hit}
                    end
            end
    end.

bary_centric(Ax,Ay,Bx,By,Cx,Cy, BFCull) ->
    U = Cx*By-Cy*Bx,
    V = Ax*Cy-Ay*Cx,
    W = Bx*Ay-By*Ax,
    if
        %% Backface culling
        BFCull andalso (U < 0.0 orelse V < 0.0 orelse W < 0.0) ->
            false;
        %% non backface culling
        not BFCull andalso
        ((U < 0.0 orelse V < 0.0 orelse W < 0.0)
         andalso (U > 0.0 orelse V > 0.0 orelse W > 0.0)) ->
            false;
        true ->
            case U+V+W of
		0.0 -> false;
		Det -> {U,V,W,Det}
	    end
    end.

order({X0,Y0,Z0}) ->
    %% Calc dimension where ray dir is maximal
    %% and swap the other to preserve winding
    X = abs(X0), Y = abs(Y0), Z = abs(Z0),
    if X >= Y ->
	    case X >= Z of
		true  when X0 >= 0 -> {2, 3, 1};
		true               -> {3, 2, 1};
		false when Z0 >= 0 -> {1, 2, 3};
		false              -> {2, 1, 3}
	    end;
       true ->
	    case Y >= Z of
		true  when Y0 >= 0 -> {3, 1, 2};
		true               -> {1, 3, 2};
		false when Z0 >= 0 -> {1, 2, 3};
		false              -> {2, 1, 3}
	    end
    end.

shear(Dir, {OX,OY,OZ}) ->
    DZ = element(OZ,Dir),
    {element(OX,Dir)/DZ, element(OY,Dir)/DZ, 1.0/DZ}.

shear_scale(Vec, {Sx,Sy,_Sz}, {OX,OY,OZ}) ->
    Az = element(OZ, Vec),
    {element(OX,Vec)-Sx*Az, element(OY,Vec)-Sy*Az}.

%%-------------------------------------------------------

intersect_1(#{bb:=BB1}=N1, #{bb:=BB2}=N2, Vs1, Vs2, Acc) ->
    case e3d_bv:intersect(BB1, BB2) of
	true  -> intersect_2(N1, N2, Vs1, Vs2, Acc);
        false -> Acc
    end.

intersect_2(#{left:=L1,right:=R1},  #{left:=L2,right:=R2}, Vs1, Vs2, Acc0) ->
    Acc1 = intersect_1(L1, L2, Vs1, Vs2, Acc0),
    Acc2 = intersect_1(L1, R2, Vs1, Vs2, Acc1),
    Acc3 = intersect_1(L2, R1, Vs2, Vs1, Acc2),
    intersect_1(R2, R1, Vs2, Vs1, Acc3);
intersect_2(#{left:=L1,right:=R1}, Leaf, Vs1, Vs2, Acc) ->
    intersect_1(L1, Leaf, Vs1, Vs2, intersect_1(R1, Leaf, Vs1, Vs2, Acc));
intersect_2(Leaf, #{left:=L1,right:=R1}, Vs1, Vs2, Acc) ->
    intersect_1(L1, Leaf, Vs2, Vs1, intersect_1(R1, Leaf, Vs2, Vs1, Acc));
intersect_2(#{mesh:=Mesh1, index:=I1, vs:=F1},
	    #{mesh:=Mesh2, index:=I2, vs:=F2},
	    Vs1, Vs2, Acc) ->
    T1 = get_tri(Mesh1, F1, Vs1),
    T2 = get_tri(Mesh2, F2, Vs2),
    %% io:format("~p ~p:~p:~p~n~p ~p:~p:~p",
    %% 	      [Mesh1, I1  div 2, I1, T1, Mesh2, I2 div 2, I2, T2]),
    F1Id = {Mesh1,I1},
    F2Id = {Mesh2,I2},
    try tri_intersect(T1, T2, F1, F2, F1Id, F2Id) of
	Intersect  ->
	    [Intersect|Acc]
    catch
        throw:false ->
            Acc;
        throw:{line,F1Id} ->  %% Bad triangulation?
            intersect_line(T1, T2, F1, F1Id, F2Id, Acc);
        throw:{line,F2Id} ->
            intersect_line(T2, T1, F2, F2Id, F1Id, Acc);
        throw:{coplanar, _DD0, _DD1} ->
            %% io:format("TestCross: ~w ~w (~w,~w)~n",[Mesh1, Mesh2, _DD0, _DD1]),
            %% io:format("1:{~s ~s ~s}~n2:{~s ~s ~s}~n",
            %%           [e3d_vec:format(V) || V <- tuple_to_list(T1)++tuple_to_list(T2)]),
            %% io:format("~p: ~p ~p~n", [?LINE, F1,F2]),
            case coplanar(T1,T2,F1Id,F2Id) of
                false -> Acc;
                CoP -> [CoP|Acc]
            end
    end.

%% Möller (realtimerendering, page 590 in 2nd edition)
tri_intersect({V0,V1,V2}, {U0,U1,U2}, {IdV0,IdV1,IdV2}, {IdU0,IdU1,IdU2}, F1, F2) ->
    E1 = e3d_vec:sub(V1, V0),
    E2 = e3d_vec:sub(V2, V0),
    N1 = e3d_vec:cross(E1,E2),
    D1 = -e3d_vec:dot(N1,V0),
    %% Plane equation 1: N1x+D1=0

    %% Put U0,U1,U2 in Plane eq 1 to compute signed distances to the plane
    Du0 = eps(e3d_vec:dot(N1,U0)+D1),
    Du1 = eps(e3d_vec:dot(N1,U1)+D1),
    Du2 = eps(e3d_vec:dot(N1,U2)+D1),

    Du0Du1 = Du0*Du1,
    Du0Du2 = Du0*Du2,

    if Du0Du1 > 0.0 andalso Du0Du2 > 0.0 ->
            %% No intersection occur? Check if triangle above or under plane 1
            throw(false);
       N1 =:= {0.0,0.0,0.0} -> %% Thin triangle (line)
            %% io:format("Line: ~p ~p~n",[F1, {IdV0,IdV1,IdV2}]),
            throw({line, F1});
       true ->
            ok
    end,

    E3 = e3d_vec:sub(U1, U0),
    E4 = e3d_vec:sub(U2, U0),
    N2 = e3d_vec:cross(E3,E4),
    D2 = -e3d_vec:dot(N2,U0),
    %% Plane equation 2: N2x+D2=0
    Dv0 = eps(e3d_vec:dot(N2,V0)+D2),
    Dv1 = eps(e3d_vec:dot(N2,V1)+D2),
    Dv2 = eps(e3d_vec:dot(N2,V2)+D2),

    Dv0Dv1 = Dv0*Dv1,
    Dv0Dv2 = Dv0*Dv2,

    if Dv0Dv1 > 0.0 andalso Dv0Dv2 > 0.0 ->
            %% No intersection occur? Check if triangle above or under plane 2 ??
            throw(false);
       N2 =:= {0.0,0.0,0.0} -> %% Thin triangle (line)
            %% io:format("Line: ~p: ~p~n",[F2,{IdU0,IdU1,IdU2}]),
            throw({line, F2});
       true ->
            ok
    end,
    %% Compute direction of intersection line
    D = e3d_vec:cross(N1,N2),
    {Index,_,_} = largest_dir(D),
    %% Compute interval for triangle 1
    {ISect1,A1,A2} = tri_intvals({V0,IdV0},{V1,IdV1},{V2,IdV2},
                                 Index, Dv0, Dv1, Dv2, Dv0Dv1, Dv0Dv2),
    {ISect2,B1,B2} = tri_intvals({U0, IdU0}, {U1,IdU1}, {U2,IdU2},
                                 Index, Du0, Du1, Du2, Du0Du1, Du0Du2),
    pick_points(sort2(ISect1), sort2(ISect2), A1, A2, B1, B2, F1, F2).

tri_intvals(V0, V1, V2, Index, D0, D1, D2, DOD1, _DOD2)
  when DOD1 > 0.0 ->
    %% here we know that D0D2<=0.0
    %%  that is D0, D1 are on the same side, D2 on the other or on the plane
    isect2(V2,V0,V1,Index,D2,D0,D1);
tri_intvals(V0, V1, V2, Index, D0, D1, D2, _DOD1, DOD2)
  when DOD2 > 0.0 ->
    %% here we know that d0d1<=0.0
    isect2(V1,V0,V2,Index,D1,D0,D2);
tri_intvals(V0, V1, V2, Index, D0, D1, D2, _DOD1, _DOD2)
  when (D1*D2>0.0) orelse  D0 =/= 0.0 ->
    isect2(V0,V1,V2,Index,D0,D1,D2);
tri_intvals(V0, V1, V2, Index, D0, D1, D2, _DOD1, _DOD2)
  when D1 =/= 0.0 ->
    isect2(V1,V0,V2,Index,D1,D0,D2);
tri_intvals(V0, V1, V2, Index, D0, D1, D2, _DOD1, _DOD2)
  when D2 =/= 0.0 ->
    isect2(V2,V0,V1,Index,D2,D0,D1);
tri_intvals(_V1, _V2, _V3, _Index, _D0, _D1, _D2, DOD1, DOD2) ->
    %% triangles are coplanar
    throw({coplanar, DOD1, DOD2}).

isect2({V0,IdV0}, {V1,IdV1}, {V2,IdV2}, Index, D0, D1, D2) ->
    Tmp0 = D0/(D0-D1),
    VV0 = element(Index, V0),
    VV1 = element(Index, V1),
    VV2 = element(Index, V2),
    Isect0 = VV0+(VV1-VV0)*Tmp0,
    Diff00 = e3d_vec:sub(V1,V0),
    Diff01 = e3d_vec:mul(Diff00, Tmp0),
    P0 = e3d_vec:add(V0, Diff01),
    Tmp1 = D0/(D0-D2),
    Isect1 = VV0+(VV2-VV0)*Tmp1,
    Diff10 = e3d_vec:sub(V2,V0),
    Diff11 = e3d_vec:mul(Diff10, Tmp1),
    P1 = e3d_vec:add(V0, Diff11),
    {{Isect0, Isect1}, {P0,IdV0,IdV1}, {P1,IdV0,IdV2}}.

pick_points({IS10,IS11,_}, {IS20,IS21,_}, _A1, _A2, _B1, _B2, _F1, _F2)
  when (IS11 < IS20) orelse (IS21 < IS10) ->
    throw(false);
pick_points({IS10,IS11,Min1}, {IS20,IS21,Min2}, A1, A2, B1, B2, F1, F2)
  when IS20 < IS10 ->
    {MF1,P1} = {F1, pick_point(Min1,A1,A2)},
    {MF2,P2} = if IS21 < IS11 -> {F2, pick_point(Min2,B2,B1)};
		  true -> {F1, pick_point(Min1,A2,A1)}
	       end,
    #{mf1=>MF1, mf2=>MF2, p1=>P1, p2=>P2, other=>F2};
pick_points({_IS10,IS11,Min1}, {_IS20,IS21,Min2}, A1, A2, B1, B2, F1, F2) ->
    {MF1,P1} = {F2,pick_point(Min2, B1, B2)},
    {MF2,P2} = if IS21 > IS11 -> {F1,pick_point(Min1, A2, A1)};
		  true -> {F2,pick_point(Min2, B2, B1)}
	       end,
    #{mf1=>MF1, mf2=>MF2, p1=>P1, p2=>P2, other=>F1}.

largest_dir({X,Y,Z}) ->
    AX=abs(X), AY=abs(Y), AZ=abs(Z),
    if AY < AZ, AX < AZ -> {3,1,2};
       AX < AY -> {2,1,3};
       true -> {1,2,3}
    end.

pick_point(true, P1, _P2) -> P1;
pick_point(false,_P1, P2) -> P2.

sort2({A,B}) when A > B ->
    {B,A,false};
sort2({A,B}) ->
    {A,B,true}.

eps(V) when abs(V) < ?EPSILON -> 0.0;
eps(V) -> V.

%% We don't (currently) handle coplanar but return the faces that are
%% coplanar application might be able to handle that
%% Triangle/triangle intersection test routine, by Tomas Moller, 1997
coplanar({V0,V1,V2}, {U0,U1,U2},FI1,FI2) ->
    {_,I0,I1} = largest_dir(e3d_vec:normal(V0,V1,V2)),
    %% test all edges of triangle 1 against the edges of triangle 2
    CoP = edge_against_tri(V0,V1,U0,U1,U2,I0,I1) orelse
        edge_against_tri(V1,V2,U0,U1,U2,I0,I1) orelse
        edge_against_tri(V2,V0,U0,U1,U2,I0,I1) orelse
        %% finally, test if tri1 is totally contained in tri2 or vice versa
        point_in_tri(V0,U0,U1,U2,I0,I1) orelse
        point_in_tri(U0,V0,V1,V2,I0,I1),
    case CoP of
        false -> false;
        true -> {coplanar, FI1, FI2}
    end.

edge_against_tri(V0,V1,U0,U1,U2,I0,I1) ->
    Ax = element(I0,V1)-element(I0,V0),
    Ay = element(I1,V1)-element(I1,V0),
    edge_edge(V0,U0,U1,Ax,Ay,I0,I1) orelse
        edge_edge(V0,U1,U2,Ax,Ay,I0,I1) orelse
        edge_edge(V0,U2,U0,Ax,Ay,I0,I1).

edge_edge(V0,U0,U1,Ax,Ay,I0,I1) ->
    Bx=element(I0,U0)-element(I0,U1),
    By=element(I1,U0)-element(I1,U1),
    Cx=element(I0,V0)-element(I0,U0),
    Cy=element(I1,V0)-element(I1,U0),
    F=Ay*Bx-Ax*By,
    D=By*Cx-Bx*Cy,
    if F > 0, D >= 0, D =< F ->
            E=Ax*Cy-Ay*Cx,
            E >= 0 andalso E =< F;
       F < 0, D =< 0, D >= F ->
            E=Ax*Cy-Ay*Cx,
            E =< 0 andalso E >= F;
       true -> false
    end.

%% check if V0 is inside tri(U0,U1,U2)
point_in_tri(V0,U0,U1,U2,I0,I1) ->
    A0=element(I1,U1)-element(I1,U0),
    B0=-(element(I0,U1)-element(I0,U0)),
    C0=-A0*element(I0,U0)-B0*element(I1,U0),
    D0=A0*element(I0,V0)+B0*element(I1,V0)+C0,

    A1=element(I1,U2)-element(I1,U1),
    B1=-(element(I0,U2)-element(I0,U1)),
    C1=-A1*element(I0,U1)-B1*element(I1,U1),
    D1=A1*element(I0,V0)+B1*element(I1,V0)+C1,

    A2=element(I1,U0)-element(I1,U2),
    B2=-(element(I0,U0)-element(I0,U2)),
    C2=-A2*element(I0,U2)-B2*element(I1,U2),
    D2=A2*element(I0,V0)+B2*element(I1,V0)+C2,
    (D0*D1>0.0) andalso (D0*D2>0.0).


%% Special case for slim triangles, handle intersection test them as rays
intersect_line({A,B,C}=Line,Tri,{VA,VB,VC},F1,{Mesh,Face}=F2, Acc) ->
    {Ray,Edge,NLen} = make_ray(Line),
    %% io:format("Line: ~p ~p ~w~n",[Edge, Line, NLen]),
    case hit_triangle(Ray, false, Tri, Mesh, Face) of
        {_, false} -> Acc;
        {#ray{o=O, d=V, f=Far}, _} ->
            P = e3d_vec:add_prod(O, V, Far),
            {P1,P2} = case Edge of
                          {A,B} ->
                              PO = case Far < NLen of
                                       true -> {P, VA, VC};
                                       false -> {P, VB, VC}
                                   end,
                              {{P,VA,VB}, PO};
                          {C,A} ->
                              PO = case Far < NLen of
                                       true -> {P, VB, VC};
                                       false -> {P, VA, VB}
                                   end,
                              {{P,VC,VA}, PO};
                          {B,C} ->
                              PO = case Far < NLen of
                                       true -> {P, VA, VB};
                                       false -> {P, VA, VC}
                                   end,
                              {{P,VB,VC}, PO}
                      end,
            Int = #{mf1=>F1, mf2=>F1, p1=>P1, p2=>P2, other=>F2},
            [Int|Acc]
    end.

make_ray({A,B,C}) ->
    E1 = e3d_vec:sub(B,A),  E1L = e3d_vec:len(E1),
    E2 = e3d_vec:sub(A,C),  E2L = e3d_vec:len(E2),
    E3 = e3d_vec:sub(C,B),  E3L = e3d_vec:len(E3),
    if E1L > E2L ->
            if E1L > E3L -> {ray(A, e3d_vec:divide(E1,E1L), 0.0, E1L), {A,B}, E2L};
               true      -> {ray(B, e3d_vec:divide(E3,E3L), 0.0, E3L), {B,C}, E1L}
            end;
       E2L < E3L         -> {ray(B, e3d_vec:divide(E3,E3L), 0.0, E3L), {B,C}, E1L};
       true              -> {ray(C, e3d_vec:divide(E2,E2L), 0.0, E2L), {C,A}, E3L}
    end.

get_tri(MeshId, {V1,V2,V3}, AllVs) ->
    Vs = maps:get(MeshId,AllVs),
    %%{element(V1+1, Vs),element(V2+1, Vs),element(V3+1, Vs)}.
    {array:get(V1, Vs),array:get(V2, Vs),array:get(V3, Vs)}.

get_tri({V1,V2,V3}, Vs) ->
    {array:get(V1, Vs),array:get(V2, Vs),array:get(V3, Vs)}.

%%%% Compile to constants

compile(Tree, Verts) ->
    Mod = "tmp_" ++ ?MODULE_STRING ++ integer_to_list(erlang:unique_integer([positive,monotonic])),
    H1 = io_lib:format("-module(~s).~n-compile(export_all).~n~n", [Mod]),
    F1 = io_lib:format("tree() ->~n ~w.~n~n", [Tree]),
    F2 = io_lib:format("verts() ->~n ~w.~n", [Verts]),
    {Dir,File} = try
		     BD = filename:basedir(user_cache, "e3d"),
		     F = filename:join(BD, Mod),
		     ok = filelib:ensure_dir(F),
		     {BD, F}
		 catch _:_ ->
			 {"/tmp", filename:join("/tmp", Mod)}
		 end,
    ok = file:write_file(File  ++ ".erl", iolist_to_binary([H1,F1,F2])),
    {ok, Module} = c:c(File, [{outdir, Dir}]),
    ok = file:delete(File ++ ".erl"),
    ok = file:delete(File ++ ".beam"),
    {bvh, compiled, Module}.
