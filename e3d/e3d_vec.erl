%%
%%  e3d_vec.erl --
%%
%%     Arithmetic on vectors and points (represented as three-tuples).
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%


-module(e3d_vec).

-export([zero/0,is_zero/1,add/1,add/2,add_prod/3,sub/1,sub/2,lerp/3,
	 norm_sub/2,mul/2,divide/2,neg/1,dot/2,cross/2,
	 len/1,dist/2,dist_sqr/2,
	 norm/1,norm/3,normal/3,normal/1,average/1,average/2,average/4,
	 bounding_box/1,area/3,area_flat_polygon/2,interior_angles/2,tri_in_hull/1,
   minimal_isosceles/1,
   degrees/2,plane/3,plane/1,plane/2,plane_side/2,plane_dist/2]).

-include("e3d.hrl").

-compile(inline).
-compile({inline_size,24}).

-spec zero() -> e3d_vector().
    
zero() ->
    {0.0,0.0,0.0}.

-spec is_zero(e3d_vector()) -> boolean().
     
is_zero({0.0,0.0,0.0}) -> true;
is_zero(_) -> false.

-spec add(e3d_vector(), e3d_vector()) -> e3d_vector().

add({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12) ->
    {V10+V20,V11+V21,V12+V22}.

add_prod({V10,V11,V12}, {V20,V21,V22}, S) when is_float(S) ->
    {S*V20+V10,S*V21+V11,S*V22+V12}.

-spec add([e3d_vector()]) -> e3d_vector().

add([{V10,V11,V12}|T]) ->
    add(T, V10, V11, V12).

-spec sub(e3d_vector(), e3d_vector()) -> e3d_vector().

sub({V10,V11,V12}, {V20,V21,V22}) ->
    {V10-V20,V11-V21,V12-V22}.

-spec norm_sub(e3d_vector(), e3d_vector()) -> e3d_vector().

norm_sub({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12) ->
    Nx = V10-V20,
    Ny = V11-V21,
    Nz = V12-V22,
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz).

-spec sub([e3d_vector()]) -> e3d_vector().

sub([{V10,V11,V12}|T]) ->
    sub(V10, V11, V12, T).

-spec mul(e3d_vector(), S::float()) -> e3d_vector().

mul({V10,V11,V12}, S) when is_float(S) ->
    {V10*S,V11*S,V12*S}.

-spec divide(e3d_vector(), S::float()) -> e3d_vector().

divide({V10,V11,V12}, S) ->
    InvS = 1/S,
    {V10*InvS,V11*InvS,V12*InvS}.

-spec neg(e3d_vector()) -> e3d_vector().

neg({X,Y,Z}) -> {-X,-Y,-Z}.

-spec dot(e3d_vector(), e3d_vector()) -> float().

dot({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12) ->
    V10*V20 + V11*V21 + V12*V22.

-spec cross(e3d_vector(), e3d_vector()) -> e3d_vector().

cross({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22) ->
    {V11*V22-V12*V21,V12*V20-V10*V22,V10*V21-V11*V20}.

-spec len(e3d_vector()) -> float().

len({X,Y,Z}) when is_float(X), is_float(Y), is_float(Z) ->
    math:sqrt(X*X+Y*Y+Z*Z).

-spec lerp(e3d_vector(), e3d_vector(), float()) -> e3d_vector().

lerp({V10,V11,V12}, {V20,V21,V22}, T)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22) ->
    {V10+(V20-V10)*T, V11+(V21-V11)*T, V12+(V22-V12)*T}.

-spec dist(e3d_vector(), e3d_vector()) -> float().

dist({V10,V11,V12}, {V20,V21,V22}) when is_float(V10), is_float(V11), is_float(V12),
					is_float(V20), is_float(V21), is_float(V22) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    math:sqrt(X*X+Y*Y+Z*Z).

-spec dist_sqr(e3d_vector(), e3d_vector()) -> float().

dist_sqr({V10,V11,V12}, {V20,V21,V22})
  when is_float(V10), is_float(V11), is_float(V12) ->
    X = V10-V20,
    Y = V11-V21,
    Z = V12-V22,
    X*X+Y*Y+Z*Z.

-spec norm(e3d_vector()) -> e3d_vector().

norm({V1,V2,V3}) ->
    norm(V1, V2, V3).

-spec norm(X::float(), Y::float(), Z::float()) -> e3d_vector().

norm(V1, V2, V3) when is_float(V1), is_float(V2), is_float(V3) ->
    norm(V1*V1+V2*V2+V3*V3, V1, V2, V3).

-spec normal(e3d_vector(), e3d_vector(), e3d_vector()) -> e3d_vector().

normal({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
    D10 = V10-V20,
    D11 = V11-V21,
    D12 = V12-V22,
    D20 = V20-V30,
    D21 = V21-V31,
    D22 = V22-V32,
    N0 = D11*D22-D12*D21,
    N1 = D12*D20-D10*D22,
    N2 = D10*D21-D11*D20,
    D = math:sqrt(N0*N0+N1*N1+N2*N2),
    try {N0/D,N1/D,N2/D}
    catch
	error:badarith -> {0.0,0.0,0.0}
    end.

-spec area(e3d_vector(), e3d_vector(), e3d_vector()) -> float().

area({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32})
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
    D10 = V10-V20,
    D11 = V11-V21,
    D12 = V12-V22,
    D20 = V20-V30,
    D21 = V21-V31,
    D22 = V22-V32,
    N0 = D11*D22-D12*D21,
    N1 = D12*D20-D10*D22,
    N2 = D10*D21-D11*D20,
    math:sqrt(N0*N0+N1*N1+N2*N2)*0.5.


%%  http://geomalgorithms.com/a01-_area.html
%%  Signed area : polygon should not be bent ... you are on honor system. 
%%    case area is negative means counter clockwise order was given.
%%    case area is positive means clockwise order was given.
%%----------------------------------------------------------------------
%% Polygon ... an order list of polygon points. Don't feed non-flat polys.
%% N : outside normal to polygon/face]
%% Test:  e3d_vec:area_flat_polygon([{0.0,0.0,0.0},{1.0,0.0,0.0},{1.0,1.0,0.0},{0.0,1.0,0.0}],{0.0,0.0,1.0}).
%% TestMore: 
%% e3d_vec:area_flat_polygon([{0.0,3.0,0.0},{3.0,2.0,0.0},{2.0,-3.0,0.0},{-2.0,-3.0,0.0},{-3.0,2.0,1.0}], {0.0,0.0,1.0}).
area_flat_polygon([{V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32}|_]=Polygon0,{_,_,_}=N)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
  Len = length(Polygon0),

  PolygonBuffed = Polygon0 ++ [ lists:nth(1,Polygon0) ], %% Lame
  MyCross = fun(Vi, Acc) -> 
      Pt1 = lists:nth(Vi, PolygonBuffed),
      Pt2 = lists:nth(Vi+1, PolygonBuffed),
      Val = e3d_vec:cross(Pt1,Pt2),
      [Val|Acc]
  end,
  List = lists:foldr(MyCross, [], lists:seq(1,Len)),
  Sum = e3d_vec:add(List),
  Dot = e3d_vec:dot(N,Sum),
  Dot/2.0.


%% Polygon ... listed in clockwise or counter clockwise order 
%% {_,_,_} = N ... normal to the face pointing towards viewer.
%% N ... the normal can be used to make sure clock vs counterclock
%% does not matter. See SIGNED companion function, area_flat_polygon
%% Note:  Don't use this on a crazy bent ... not-flat poly.
interior_angles([{V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32}|_]=Polygon, {_,_,_}=N)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32) ->
    Test = area_flat_polygon(Polygon, N) < 0.0,
    Pts =
    case Test of 
      true -> lists:reverse(Polygon);
      false -> Polygon
    end, 
    Len = length(Pts),
    MyAcc = fun(Index, Acc) -> 
        Idx0 = Index rem Len,
        Idx1 = (Index+1) rem Len,
        Idx2 = (Index+2) rem Len,
        Pt0 = lists:nth(Idx0+1,Pts),
        Pt1 = lists:nth(Idx1+1,Pts),
        Pt2 = lists:nth(Idx2+1,Pts),
        D1 = e3d_vec:sub(Pt0,Pt1),
        D2 = e3d_vec:sub(Pt2,Pt1),
        Nxx = e3d_vec:cross(D1,D2),
        AngNs = abs(e3d_vec:degrees(Nxx,N)),
        %% should effectively capture left or right turn as we walk.
        LeftTurn = AngNs > 45.0,  
        Ang = abs(e3d_vec:degrees(D1,D2)),
        if (LeftTurn) -> [{Pt1,Ang}|Acc];
             true -> [{Pt1,360-Ang}|Acc]
        end
    end,
    lists:foldl(MyAcc,[], lists:seq(0,length(Pts))).


%% maximal area inscribed triangle.
-record(mit, % MAX INSCRIBED TRIANGLE
       {     % generally a state record used so the implementation can
             % follow the source/example very closely.
         a=0, % first vertex in current triangle
         b=1, % second vertex in current triangle
         c=2, % third vertex in current triangle            
         ba=0, % best first index
         bb=1, % best second index
         bc=2, % best third index
         iters=0
       }).
%% Source for idea of max fit triangle ...
%% http://stackoverflow.com/questions/1621364/how-to-find-largest-triangle-in-convex-hull-aside-from-brute-force-search
%% --------------------------------------------------------------------
%% The title of the work of ispiration (paper) is ...
%% On a general method for maximizing and minimizing among certain 
%% geometric problems 
%%---------------------------------------------------------------------
tri_in_hull([{_,_,_}|_]=Pts) -> 
    N = length(Pts),
    %% Assume points have been sorted already, as 0...(n-1)
    Area0 = fun({A,B,C}) when is_integer(A), is_integer(B), is_integer(C) -> 
        abs(
        e3d_vec:area(lists:nth(A+1,Pts),lists:nth(B+1,Pts),lists:nth(C+1,Pts))
        )
    end,
    Area = 
    fun
        ({#mit{a=A,b=B,c=C},{D,E,F}}) ->
            Temp={(A+D) rem N, (B+E) rem N, (C+F) rem N},
            Area0(Temp);
        (#mit{a=A,b=B,c=C}) -> Area0({A,B,C});
        ({A,B,C})           -> Area0({A,B,C})
    end,
    Incr = fun %% write this way so matches reference code.
        (#mit{iters=I}=Mit, '(A+1)%n') ->
            Mit#mit{iters=I+1,a=(Mit#mit.a+1) rem N};
        (#mit{iters=I}=Mit, '(B+1)%n') ->
            Mit#mit{iters=I+1,b=(Mit#mit.b+1) rem N};
        (#mit{iters=I}=Mit, '(C+1)%n') ->
            Mit#mit{iters=I+1,c=(Mit#mit.c+1) rem N}
    end,
    LoopA = 
    fun (#mit{}=AccA,FunA) ->
        LoopB = 
        fun(#mit{}=AccB,FunB) ->
            LoopC = fun (#mit{}=AccC, MeFun) ->
                %%io:format("A = ~p, B = ~p, C = ~p\n", [ AccC#mit.a, AccC#mit.b, AccC#mit.c ]),
                Ar1 = Area(AccC),
                Ar2 = Area(Incr(AccC,'(C+1)%n')),
                %%io:format("Ar1 = ~.4f, Ar2 = ~.4f\n", [ Ar1, Ar2 ]),
                Test = (Ar1 =< Ar2),
                if (Test) -> 
                    MeFun(Incr(AccC,'(C+1)%n'),MeFun);
                    true -> AccC end
            end,
            AccB2 = LoopC(AccB,LoopC),
            case Area(AccB2) =< Area(Incr(AccB2,'(B+1)%n')) of 
                 true -> 
                    FunB(Incr(AccB2,'(B+1)%n'),FunB);
                 false -> AccB2
            end
        end,
        #mit{a=A,b=B,c=C} = AccA2 = LoopB(AccA,LoopB),
        AccA3 = 
        case  Area({A,B,C}) > Area({AccA2#mit.ba, AccA2#mit.bb, AccA2#mit.bc}) of
            true ->  AccA2#mit{ba=AccA2#mit.a,bb=AccA2#mit.b,bc=AccA2#mit.c};
            false -> AccA2
        end,
        AccA4 =
        AccA3#mit{
            a=  (A+1) rem N, 
            b = if ((A+1) rem N)==B -> (B+1) rem N; true -> B end,
            c = if ((B+1) rem N)==C -> (C+1) rem N; true -> C end
        },
        if (AccA4#mit.a == 0) -> AccA4#mit{}; true -> FunA(AccA4,FunA) end
    end, 
    #mit{} = Fin = LoopA(#mit{},LoopA),
    BA = Fin#mit.ba,
    BB = Fin#mit.bb,
    BC = Fin#mit.bc,
    [lists:nth(BA+1,Pts),lists:nth(BB+1,Pts),lists:nth(BC+1,Pts)].



%% Code this so it tries all combinations !
%% Find minimal isosceles enclosing triangle with 
%% two flush edges (not given) ... using all possible
%% combinations of two flush edges. 
%% Inspired by reference : Implementation of linear minimum area enclosing
%%   triangle algorithm  (Ovidiu Parvu, David Gilbert)
minimal_isosceles([{_,_,_}|_]=NGon) ->
    N0 = e3d_vec:normal(NGon),
    A0 = abs(e3d_vec:area_flat_polygon(NGon,N0)),
    random:seed(now()),
    Len  = length(NGon), 
    MyAcc = fun(I,Acc) -> 
        V1 = I rem Len,
        V2 = (I+1) rem Len,
        Pt1 = lists:nth(V1+1,NGon), 
        Pt2 = lists:nth(V2+1,NGon),
        [{V1,Pt1,V2,Pt2}|Acc]
    end,
    Es = lists:foldr(MyAcc,[],lists:seq(0,Len-1)),
    Combos = combine2(Es),

    MyLocal = fun({{_V1,P1,_V2,P2},{_V3,P3,_V4,P4}}, Acc) -> 
        %% toes out or toes in !
        D1 = e3d_vec:dist(P1,P4),
        D2 = e3d_vec:dist(P2,P3),
        case catch  % catch nil generated by parallel lines
            (case D1 < D2 of 
                true ->  minimal_isosceles(P1, P2, P3, P4, NGon);
                false -> minimal_isosceles(P3, P4, P1, P2, NGon)
            end) of 
            {I,II,III}=Triangle -> 
                Area = abs(e3d_vec:area(I,II,III)),
                %% io:format("Current Area = ~.6f, Area0 = ~.6f\n", [ Area, A0 ]),
                if (Area > A0) -> [{Area,Triangle}|Acc];
                    true -> Acc  % SHOULD NEVER HAPPEN. 
                end;
            _ERROR -> 
                    Acc
        end
    end,
    List0 = lists:foldl(MyLocal, [], Combos),
    [{_,Triangle}|_] = lists:sort(List0),
    Triangle.


%% Given Two flush edges ... find local minmal isosceles triangle 
%% which encloses NGon convext polygon
%% Parameters:
%% Left hand flush edge : {X1,Y1,Z1},{X2,Y2,Z2}
%% Right hand flush edge : {X3,Y3,Z3},{X4,Y4,Z4}
%% NGon : convex polygon
minimal_isosceles({X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}, {X4,Y4,Z4}, [{_,_,_}|_]=NGon) -> 
    I = line_line_intersect({X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}),
    V12 = e3d_vec:norm(e3d_vec:sub({X2,Y2,Z2},{X1,Y1,Z1})),
    V67 = e3d_vec:norm(e3d_vec:sub({X3,Y3,Z3},{X4,Y4,Z4})),
    Vi =  e3d_vec:norm(e3d_vec:average([V12,V67])),
    MyAcc = fun({X,Y,Z}=Pt,Acc) ->
        R0 = abs(e3d_vec:dist(I,{X,Y,Z})),
        Vx = e3d_vec:sub({X,Y,Z},I),
        Alpha0 = abs(e3d_vec:degrees(Vi,Vx)),
        Cos0 = R0*abs(math:cos(Alpha0 * math:pi() / 180.0)),
        [{Cos0,Pt}|Acc]
    end,
    [{Cos1,{_Fx,_Fy,_Fz}}|_] = lists:reverse(lists:sort(lists:foldl(MyAcc,[],NGon))),
    Theta = abs(e3d_vec:degrees(V12,V67)),
    Alpha = Theta/2.0,
    R = abs(Cos1 / math:cos(Alpha * math:pi() / 180.0)), %% Length of side of Isosceles
    II  = e3d_vec:add(I, e3d_vec:mul(V12,R)),
    III = e3d_vec:add(I, e3d_vec:mul(V67,R)),
    {I,II,III}.

%% Combonatorics take two at a time. Needed for minimal_isosceles
combine2(List) ->
    L2 = [{X,Y}||X<-List,Y<-List],
    MyAcc = fun({X,Y}, Acc) ->
        case X == Y of 
        true -> Acc;
        false ->
            case gb_sets:is_member({Y,X}, Acc) orelse gb_sets:is_member({X,Y}, Acc)  of 
                true ->   Acc;
                false ->  gb_sets:add({X,Y},Acc)
            end
                
        end
    end,
    Set = lists:foldl(MyAcc, gb_sets:empty(), L2),
    gb_sets:to_list(Set).

-define(EPSILON,0.0000001). 
%% don't apply this until change of reference frames has flattened the Z values.
%% http://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
%% TEST:
%% line_line_intersect({0.0,0.0,0.0},{1.0,1.0,0.0},{3.0,1.0,0.0},{4.0,0.0,0.0})
line_line_intersect({X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}) when 
    (Z1 < ?EPSILON) andalso (Z1 > -?EPSILON) andalso 
    (Z2 < ?EPSILON) andalso (Z2 > -?EPSILON) andalso 
    (Z3 < ?EPSILON) andalso (Z3 > -?EPSILON) andalso 
    (Z4 < ?EPSILON) andalso (Z4 > -?EPSILON) ->
    X =  ((X1*Y1-Y1*X2)*(X3-X4) - (X1-X2)*(X3*Y4-Y3*X4)) /
               ((X1-X2)*(Y3-Y4) - (Y1-Y2)*(X3-X4)),
    Y =  ((X1*Y2-Y1*X2)*(Y3-Y4) - (Y1-Y2)*(X3*Y4-Y3*X4)) /
               ((X1-X2)*(Y3-Y4) - (Y1-Y2)*(X3-X4)),
    {X,Y,0.0};


%% Assume given actual intersecting and also non parallel vectors.
%% And then forcefully ensure that is the case by ZEROING Z ordinate !
%% Test : 
%% line_line_intersect({1.0,0.0,0.0},{1.0,0.5,0.5},{0.5,0.5,1.0},{0.0,0.0,1.0}).
line_line_intersect({X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}) -> 
    E1 = e3d_vec:normal([{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}]),
    D1 = e3d_vec:sub({X1,Y1,Z1},{X2,Y2,Z2}),
    D2 = e3d_vec:sub({X3,Y3,Z3},{X4,Y4,Z4}),
    Ang0 = round(abs(e3d_vec:degrees(D1,D2))),
    Ang = Ang0 rem 180,
    if (Ang < 2) ->
            nil;
        true ->
            Pts = [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
            Mr = e3d_mat:rotate_s_to_t(E1,{0.0,0.0,1.0}), %% E1 should be stubby/nubby dir
            [Zpt|_] = [e3d_mat:mul_point(Mr,Pt) || Pt <- Pts],
            Tr = e3d_mat:translate(e3d_vec:mul(Zpt,-1.0)),
            List = [Mr,Tr],
            M3 = e3d_mat:mul(List),
            Inverted = e3d_mat:mul([e3d_mat:invert(M0)||M0 <- lists:reverse(List)]),
            NewPts =  [e3d_mat:mul_point(M3,Pt) || Pt <- Pts],   
            %% ZERO that Z Man !
            [P5,P6,P7,P8] = [{X,Y,0.0}||{X,Y,_Z}<-NewPts],
            CPA0 = line_line_intersect(P5,P6,P7,P8), % Closest Point of Approach
            e3d_mat:mul_point(Inverted, CPA0)
    end.
    


      
    
%% Point normal form !    
plane({CX,CY,CZ}, {A,B,C}) 
    when is_float(CX), is_float(CY), is_float(CZ),
         is_float(A),  is_float(A),  is_float(A) ->
     D = -A*CX-B*CY-C*CZ,                  
     {{A,B,C},D}.
    
%% Calculate plane coefficients for a plane on which the triangle lies   
plane({X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}) ->
        {A,B,C} = e3d_vec:normal({X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}),
        {CX,CY,CZ} = e3d_vec:average([{X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}]),
        D = -A*CX-B*CY-C*CZ,                  
        {{A,B,C},D}.
%% For applicaion at the face level.
plane([{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}|T]) ->
        {A,B,C} = e3d_vec:normal([{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}|T]),
        {CX,CY,CZ} = e3d_vec:average([{X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}]),
        D = -A*CX-B*CY-C*CZ,                  
        {{A,B,C},D}.

%% Helper function used to sort points according to which side of a plane they are on.	
plane_side({X,Y,Z}, {{A,B,C},D}) ->
    Temp=A*X+B*Y+C*Z+D,
    if  Temp < 0.0000 -> -1;  true -> 1 end.

%% Using Coeff to calculate signed distance from point to plane.
plane_dist({X,Y,Z},{{A,B,C},D}) ->
	(A*X+B*Y+C*Z+D)/math:sqrt(A*A+B*B+C*C).
        

%% normal([{X,Y,Z}]) ->
%%  Calculate the averaged normal for the polygon using Newell's method.

-spec normal([e3d_vector()]) -> e3d_vector().

normal([{Ax,Ay,Az},{Bx,By,Bz},{Cx,Cy,Cz}])
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    Sx = (Ay-By)*(Az+Bz) + (By-Cy)*(Bz+Cz) + (Cy-Ay)*(Cz+Az),
    Sy = (Az-Bz)*(Ax+Bx) + (Bz-Cz)*(Bx+Cx) + (Cz-Az)*(Cx+Ax),
    Sz = (Ax-Bx)*(Ay+By) + (Bx-Cx)*(By+Cy) + (Cx-Ax)*(Cy+Ay),
    SqrLen = Sx*Sx + Sy*Sy + Sz*Sz,
    norm(SqrLen, Sx, Sy, Sz);
normal([{Ax,Ay,Az},{Bx,By,Bz},{Cx,Cy,Cz},{Dx,Dy,Dz}])
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    %% The same result as the Newell normal (after normalization)
    %% can be calculated by taking the cross product of the vectors
    %% formed by the diagonals of the quad. (From Christer Ericson:
    %% "Real-Time Collision Detection", Chapter 12.)
    V10 = Dx-Bx, V11 = Dy-By, V12 = Dz-Bz,
    V20 = Ax-Cx, V21 = Ay-Cy, V22 = Az-Cz,
    Nx = V11*V22-V12*V21,
    Ny = V12*V20-V10*V22,
    Nz = V10*V21-V11*V20,
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz);
normal([{Ax,Ay,Az},{Bx,By,Bz}|[{Cx,Cy,Cz}|_]=T]=First)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Bx), is_float(By), is_float(Bz) ->
    Sx = (Ay-By)*(Az+Bz) + (By-Cy)*(Bz+Cz),
    Sy = (Az-Bz)*(Ax+Bx) + (Bz-Cz)*(Bx+Cx),
    Sz = (Ax-Bx)*(Ay+By) + (Bx-Cx)*(By+Cy),
    normal_1(T, First, Sx, Sy, Sz).

normal_1([{Ax,Ay,Az}], [{Bx,By,Bz}|_], Sx, Sy, Sz)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Sx), is_float(Sy), is_float(Sz) ->
    Nx = Sx + (Ay-By)*(Az+Bz),
    Ny = Sy + (Az-Bz)*(Ax+Bx),
    Nz = Sz + (Ax-Bx)*(Ay+By),
    SqrLen = Nx*Nx + Ny*Ny + Nz*Nz,
    norm(SqrLen, Nx, Ny, Nz);
normal_1([{Ax,Ay,Az}|[{Bx,By,Bz}|_]=T], First, Sx0, Sy0, Sz0)
  when is_float(Ax), is_float(Ay), is_float(Az),
       is_float(Sx0), is_float(Sy0), is_float(Sz0) ->
    Sx = Sx0 + (Ay-By)*(Az+Bz),
    Sy = Sy0 + (Az-Bz)*(Ax+Bx),
    Sz = Sz0 + (Ax-Bx)*(Ay+By),
    normal_1(T, First, Sx, Sy, Sz).

%% average([{X,Y,Z}]) -> {Ax,Ay,Az}
%%  Average the given list of points.

-spec average([e3d_vector()]) -> e3d_vector().

average([{V10,V11,V12},B]) ->
    {V20,V21,V22} = B,
    V0 = if
	     V10 =:= V20 -> V10;
	     is_float(V10) -> 0.5*(V10+V20)
	 end,
    V1 = if
	     V11 =:= V21 -> V11;
	     is_float(V11) -> 0.5*(V11+V21)
	 end,
    if
	V12 =:= V22 -> {V0,V1,V12};
	is_float(V12) -> {V0,V1,0.5*(V12+V22)}
    end;
average([{V10,V11,V12}|T]=All) ->
    average(T, V10, V11, V12, length(All)).

-spec average(e3d_vector(), e3d_vector()) -> e3d_vector().

average({V10,V11,V12}, {V20,V21,V22}) ->
    V0 = if
	     V10 =:= V20 -> V10;
	     is_float(V10) -> 0.5*(V10+V20)
	 end,
    V1 = if
	     V11 =:= V21 -> V11;
	     is_float(V11) -> 0.5*(V11+V21)
	 end,
    if
	V12 =:= V22 -> {V0,V1,V12};
	is_float(V12) -> {V0,V1,0.5*(V12+V22)}
    end.

-spec average(e3d_vector(), e3d_vector(), e3d_vector(), e3d_vector()) -> e3d_vector().

average({V10,V11,V12}, {V20,V21,V22}, {V30,V31,V32}, {V40,V41,V42})
    when is_float(V10), is_float(V11), is_float(V12) ->
    L = 0.25,
    {L*(V10+V20+V30+V40),L*(V11+V21+V31+V41),L*(V12+V22+V32+V42)}.


%% Should be removed and calls should be changed to e3d_bv instead.
-spec bounding_box([e3d_vector()]) -> [e3d_vector()].
bounding_box(List) when is_list(List) ->
    tuple_to_list(e3d_bv:box(List)).

-spec degrees(e3d_vector(), e3d_vector()) -> float().
    
degrees(V0, V1) ->
    Dot = e3d_vec:dot(V0,V1),
    LenMul = e3d_vec:len(V0) * e3d_vec:len(V1),
    %%% protect against divide-by-zero
    RawCos = if (abs(LenMul) > 1.0E-30) -> Dot / LenMul;
               true -> 1.0
             end,
    %%% protect against invalid cosine values
    Cos = if
            (RawCos > +1.0) -> +1.0;
            (RawCos < -1.0) -> -1.0;
            true -> RawCos
          end,
    math:acos(Cos) * (180.0 / math:pi()).

%%%
%%% Internal functions.
%%% 

add([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32);
add([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10+V20, A1+V11+V21, A2+V12+V22);
add([{V10,V11,V12}|T], A0, A1, A2)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(A0), is_float(A1), is_float(A2) ->
    add(T, A0+V10, A1+V11, A2+V12);
add([], A0, A1, A2) -> {A0,A1,A2}.

sub(A0, A1, A2, [{V10,V11,V12}|T]) ->
    sub(A0-V10, A1-V11, A2-V12, T);
sub(A0, A1, A2, []) -> {A0,A1,A2}.

norm(SqrLen, _, _, _) when SqrLen < 1.0E-16 ->
    {0.0,0.0,0.0};
norm(SqrLen, V1, V2, V3) ->
    D = math:sqrt(SqrLen),
    try {V1/D,V2/D,V3/D}
    catch
	error:badarith -> {0.0,0.0,0.0}
    end.

average([{V10,V11,V12},{V20,V21,V22},{V30,V31,V32}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(V30), is_float(V31), is_float(V32),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10+V20+V30, A1+V11+V21+V31, A2+V12+V22+V32, L);
average([{V10,V11,V12},{V20,V21,V22}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(V20), is_float(V21), is_float(V22),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10+V20, A1+V11+V21, A2+V12+V22, L);
average([{V10,V11,V12}|T], A0, A1, A2, L)
  when is_float(V10), is_float(V11), is_float(V12),
       is_float(A0), is_float(A1), is_float(A2) ->
    average(T, A0+V10, A1+V11, A2+V12, L);
average([], A0, A1, A2, L0) ->
    L = 1.0/float(L0),
    {A0*L,A1*L,A2*L}.
