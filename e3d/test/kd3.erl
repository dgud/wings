%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% @copyright (C) 2009, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2009 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(kd3).

-compile(export_all).

-define(TC(Info,Cmd), tc(fun() -> R = Cmd, erlang:garbage_collect(),R end, Info, ?LINE)).

-define(SAMPLE, 100).

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
    {L,_} = lists:mapfoldl(fun(A,Acc) -> {{A,Acc},Acc+1} end,0, L0),
    io:format("Testdata ~p ~n", [length(L)]),
%%     L  = [{e3d_vec:norm(P), Id} || {P,Id} <- L0],
    %% Warmup
    _ = orig_kd3:from_list(L),
    erlang:garbage_collect(),
    K = ?TC(ori_fl, orig_kd3:from_list(L)),
    E = ?TC(e3d_fl, e3d_kd3:from_list(L)),
    ?TC(gb_fl, gb_trees:from_orddict(lists:sort(L))),
    io:format("~n"),
    
    w(orig_kd3:nearest({0,0,0},K), e3d_kd3:nearest({0,0,0}, E)),
    w(orig_kd3:nearest({1,1,1},K), e3d_kd3:nearest({1,1,1}, E)),
    w(orig_kd3:nearest({0,1,1},K), e3d_kd3:nearest({0,1,1}, E)),
    w(orig_kd3:nearest({0,1,0},K), e3d_kd3:nearest({0,1,0}, E)),
    w(orig_kd3:nearest({1,1,0},K), e3d_kd3:nearest({1,1,0}, E)),
    w(orig_kd3:nearest({1,0,0},K), e3d_kd3:nearest({1,0,0}, E)),
    w(orig_kd3:nearest({0,0,1},K), e3d_kd3:nearest({0,0,1}, E)),
    w(orig_kd3:nearest({0.2,0.77,0.60},K), e3d_kd3:nearest({0.2,0.77,0.60}, E)),

    erlang:garbage_collect(),
    ?TC(ori_cn, check_nearest(orig_kd3, E, L)),
    ?TC(e3d_cn, check_nearest(e3d_kd3, E, L)),
    io:format("~n"),    
    ?TC(ori_cd, check_delete(orig_kd3, E, L)),
    ?TC(e3d_cd, check_delete(e3d_kd3, E, L)),
    io:format("~n"),
    ?TC(ori_cd2, check_delete2(orig_kd3, E, L)),
    ?TC(e3d_cd2, check_delete2(e3d_kd3, E, L)),

    io:format("~n"),
       
    %%     io:format("~p~n~p~n",[
    %% 			  [Id || {_,Id} <- orig_kd3:to_list(K)],
    %% 			  [Id || {_,Id} <- e3d_kd3:to_list(E)]]),			  
    
    SList = ?TC(create_ref, lists:sort([{e3d_vec:dist_sqr(?POINT, P),O} || O = {P,_} <- L])),
    SL   = [O || {_, O} <- SList],

    %% [ io:format("~p ~p ~n", [D,A]) || {D,{_,A}} <- lists:sort(Dist) ],
    erlang:garbage_collect(),
    ?TC(ori_tn, check_take_nearest(orig_kd3, E, SL, length(SL))), 
    ?TC(e3d_tn, check_take_nearest(e3d_kd3, E, SL, length(SL))), 
    io:format("~n"),
    ?TC(ori_tn2, check_take_nearest2(orig_kd3, E, SL)), 
    ?TC(e3d_tn2, check_take_nearest2(e3d_kd3, E, SL)), 
    ok.

check_nearest(Mod, T, L) ->
    lists:foreach(fun({P, _Id}) ->
			  {P, _MaybeOtherId} = Mod:nearest(P, T)
		  end, L).

%% check_take_nearest(Mod, T, [Obj={P,1011}|Res], Len) ->
%%     io:format("Size ~p~n", [?TC(e3d_kd3:size(T))]),
%%     Node = ?TC(Mod:nearest(?POINT, T)),
%%     ?TC(Mod:delete_object(Node, T));  %% DEBUG
check_take_nearest(Mod, T, [Obj={P,_Obj}|Res], Len) ->
    case (Mod:take_nearest(?POINT, T)) of
	{{P,_},Rest} ->
	    check_take_nearest(Mod,Rest,Res, Len-1);
	{Other,_} ->
	    io:format("~p = ~p ~n",[Obj,Other])
    end;
check_take_nearest(Mod, T, [], 0) ->
    undefined = Mod:take_nearest({0.0,0.0,0.0}, T).

check_take_nearest2(Mod, T, [{P,_Id}|Res]) ->
    case Mod:take_nearest(P, T) of
	{{P,_},Rest} ->
	    check_take_nearest2(Mod,Rest,Res);
	{Other,_} ->
	    io:format("~p = ~p ~n",[{P,_Id},Other])	    
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
