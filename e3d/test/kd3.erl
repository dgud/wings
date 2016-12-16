%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% @copyright (C) 2009-2011, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2009 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(kd3).

-compile(export_all).

-define(TC(Info,Cmd), tc(fun() -> R = Cmd, erlang:garbage_collect(),R end, Info, ?LINE)).

-define(SAMPLE, 1000).

-define(POINT, {0.6,0.72,0.17}).

go() ->
    start().

start() ->
    Coord = fun(A,B) ->
		    As = math:sin(A),
		    {As*math:cos(B),As*math:sin(B),math:cos(A)}
	    end,
    Vec = [math:pi()*I/?SAMPLE || I <- lists:seq(0, ?SAMPLE-1)],
    L0 = lists:sort([Coord(A,B) || A <- Vec, B <- Vec]),
    {L,_} = lists:mapfoldl(fun(A,Acc) -> {{Acc,A},Acc+1} end,0, L0),
    io:format("Testdata ~p ~n", [length(L)]),
%%     L  = [{e3d_vec:norm(P), Id} || {P,Id} <- L0],
    erlang:garbage_collect(),
    _E = ?TC(e3d_fl, e3d_kd3:from_list(L)),
    K = _E, E = _E,
    %%K = _K, E = _K,
    ?TC(gb_fl, gb_trees:from_orddict(lists:sort(L))),
    io:format("~n"),

    erlang:garbage_collect(),
    ?TC(e3d_cn, check_nearest(e3d_kd3, E, L)),
    io:format("~n"),
    ?TC(e3d_cd, check_delete(e3d_kd3, E, L)),
    io:format("~n"),
    ?TC(e3d_cd2, check_delete2(e3d_kd3, E, L)),

    io:format("~n"),
    SList = ?TC(create_ref, lists:sort([{e3d_vec:dist_sqr(?POINT, P),O} || O = {_,P} <- L])),
    SL   = [O || {_, O} <- SList],

    %% [ io:format("~p ~p ~n", [D,A]) || {D,{_,A}} <- lists:sort(Dist) ],
    erlang:garbage_collect(),
    %?TC(e3d_tn, check_take_nearest(e3d_kd3, E, SL, length(SL))), 
    ?TC(e3d_fo, check_fold_all(e3d_kd3, E, length(SL))),
    ?TC(e3d_fo, check_fold_dist(e3d_kd3, E, element(1, lists:split(50, SL)))),
    io:format("~n"),
    ?TC(e3d_tn2, check_take_nearest2(e3d_kd3, E, SL)), 
    ok.

check_nearest(Mod, T, L) ->
    lists:foreach(fun({_Id, P}) ->
			  {_MaybeOtherId, P} = Mod:nearest(P, T)
		  end, L).

check_fold_all(Mod, T, Len) ->
    DoAll = fun({_,_}, N) -> N-1 end,
    0 = Mod:fold(DoAll, Len, ?POINT, 2.0, T).

check_fold_dist(Mod, T, List) ->
    [{_,Last}|_] = lists:reverse(List),
    Dist = e3d_vec:dist(Last, ?POINT),
    Check = fun(Object, L) ->
		    case lists:member(Object, L) of
			true -> lists:delete(Object,L);
			false -> erlang:display(check_dist)
		    end
	    end,
    [] = Mod:fold(Check, List, ?POINT, Dist, T).


check_take_nearest(Mod, T, [Obj={_Obj,P}|Res], Len) ->
    case (Mod:take_nearest(?POINT, T)) of
	{{_,P},Rest} ->
	    check_take_nearest(Mod,Rest,Res, Len-1);
	{Other,_} ->
	    io:format("~p = ~p ~n",[Obj,Other])
    end;
check_take_nearest(Mod, T, [], 0) ->
    undefined = Mod:take_nearest({0.0,0.0,0.0}, T).

check_take_nearest2(Mod, T, [{_Id,P}|Res]) ->
    case Mod:take_nearest(P, T) of
	{{_,_P},Rest} ->
	    check_take_nearest2(Mod,Rest,Res);
	{Other,_} ->
	    io:format("~p = ~p ~n",[{_Id,P},Other])
    end;
check_take_nearest2(Mod, T, []) ->
    undefined = Mod:take_nearest({0.0,0.0,0.0}, T).

w(K, E) ->
    io:format("~p ~p => ~p ~n", [element(2,K), element(2,E), element(1, E)]).


check_delete(Mod, Tree, [{_,P}|R]) ->
    Mod:delete(P,Tree),
    check_delete(Mod, Tree, R);
check_delete(_, _, []) ->
    ok.

check_delete2(Mod, Tree, [{_,P}|R]) ->
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
