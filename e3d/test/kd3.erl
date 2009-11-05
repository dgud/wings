%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% @copyright (C) 2009, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2009 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(test).

-compile(export_all).

-define(TC(Cmd), tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-define(SAMPLE, 10).

go() ->
    start().

start() ->    
    Coord = fun(A,B) ->
		    As = math:sin(A),
		    {As*math:cos(B),As*math:sin(B),math:cos(A)}
	    end,
    Vec = [math:pi()*I/?SAMPLE || I <- lists:seq(0, ?SAMPLE-1)],
    L0 = lists:usort([Coord(A,B) || A <- Vec, B <- Vec]),
    {L,_} = lists:mapfoldl(fun(A,Acc) -> {{A,Acc},Acc+1} end,0, L0),
    io:format("Testdata ~p ~n", [length(L)]),
%%     L  = [{e3d_vec:norm(P), Id} || {P,Id} <- L0],
    %% Warmup
    %% kdtree:from_list(L), e3d_kd3:from_list(L),
    K = ?TC(kdtree:from_list(L)),
    E = ?TC(e3d_kd3:from_list(L)),
    ?TC(gb_trees:from_orddict(lists:sort(L))),
    io:format("~n"),
    
    w(kdtree:nearest({0,0,0},K), e3d_kd3:nearest({0,0,0}, E)),
    w(kdtree:nearest({1,1,1},K), e3d_kd3:nearest({1,1,1}, E)),
    w(kdtree:nearest({0,1,1},K), e3d_kd3:nearest({0,1,1}, E)),
    w(kdtree:nearest({0,1,0},K), e3d_kd3:nearest({0,1,0}, E)),
    w(kdtree:nearest({1,1,0},K), e3d_kd3:nearest({1,1,0}, E)),
    w(kdtree:nearest({1,0,0},K), e3d_kd3:nearest({1,0,0}, E)),
    w(kdtree:nearest({0,0,1},K), e3d_kd3:nearest({0,0,1}, E)),
    w(kdtree:nearest({0.2,0.77,0.60},K), e3d_kd3:nearest({0.2,0.77,0.60}, E)),

    ?TC(check_nearest(kdtree,  K, L)),
    ?TC(check_nearest(e3d_kd3, E, L)),
    io:format("~n"),
       
    %%     io:format("~p~n~p~n",[
    %% 			  [Id || {_,Id} <- kdtree:to_list(K)],
    %% 			  [Id || {_,Id} <- e3d_kd3:to_list(E)]]),			  
    
    Dist = [{e3d_vec:dist_sqr({0.5,1.0,0.0}, P),O} || O = {P,_} <- L],
    SL   = [O || {_, O} <- lists:sort(Dist)],

    %% [ io:format("~p ~p ~n", [D,A]) || {D,{_,A}} <- lists:sort(Dist) ],
    %check_take_nearest(kdtree,  K, SL),
    %?TC(check_take_nearest(kdtree,  K, SL)), 
    ?TC(check_take_nearest(e3d_kd3, E, SL)), 
    io:format("~n"),
    ?TC(check_take_nearest2(kdtree,  K, SL)), 
    ?TC(check_take_nearest2(e3d_kd3, E, SL)), 
    io:format("~n"),
    ?TC(check_delete(kdtree,  K, L)),
    ?TC(check_delete(e3d_kd3, E, L)),
    io:format("~n"),
    ?TC(check_delete2(kdtree,  K, L)),
    ?TC(check_delete2(e3d_kd3, E, L)),

    ok.

check_nearest(Mod, T, L) ->
    lists:foreach(fun({P, Id}) ->
			  {P, Id} = Mod:nearest(P, T)
		  end, L).

check_take_nearest(Mod, T, [Obj|Res]) ->
    case Mod:take_nearest({0.0,0.8,0.0}, T) of
	{Obj,Rest} ->
	    %% io:format("~p ",[element(2, Obj)]),
	    check_take_nearest(Mod,Rest,Res);
	{Other,_} ->
	    io:format("~p = ~p ~n",[Obj,Other])	    
    end;
check_take_nearest(Mod, T, []) ->
    undefined = Mod:take_nearest({0.0,0.0,0.0}, T).    

check_take_nearest2(Mod, T, [{P,Id}|Res]) ->
    case Mod:take_nearest(P, T) of
	{{P,Id},Rest} ->
	    check_take_nearest2(Mod,Rest,Res);
	{Other,_} ->
	    io:format("~p = ~p ~n",[{P,Id},Other])	    
    end;
check_take_nearest2(Mod, T, []) ->
    undefined = Mod:take_nearest({0.0,0.0,0.0}, T).    

w(K, E) ->
    io:format("~p ~p => ~p ~n", [element(2,K), element(2,E), element(1, E)]).


check_delete(Mod, Tree, [{P,_}|R]) ->
    Mod:delete(P,Tree),
    check_delete(Mod, Tree, R);
check_delete(_, _, []) ->
    ok.

check_delete2(Mod, Tree, [{P,_}|R]) ->
    Tree2 = Mod:delete(P,Tree),
    check_delete(Mod, Tree2, R);
check_delete2(Mod, Tree, []) ->
    true = Mod:is_empty(Tree).

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
	{_,{'EXIT',Reason}} -> exit(Reason);
	{T,R} ->
	    io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
	    R
    end.
