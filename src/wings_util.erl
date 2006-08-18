%%
%%  wings_util.erl --
%%
%%     Various utility functions that not obviously fit somewhere else.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_util.erl,v 1.111 2005/11/09 21:00:28 dgud Exp $
%%
%% Note: To keep the call graph clean, wings_util MUST NOT call
%%       other wings_* modules (except wings_pref).

-module(wings_util).
-export([wings/0,share/1,share/3,make_vector/1,
	 rel2fam/1,
	 format/2,
	 key_format/2,
	 cap/1,upper/1,stringify/1,quote/1,
	 expand_utf8/1,
	 add_vpos/2,update_vpos/2,
	 gb_trees_smallest_key/1,gb_trees_largest_key/1,
	 gb_trees_map/2,gb_trees_to_gb_set/1,
	 nice_float/1,
	 unique_name/2,
	 tc/3,
	 min/2,max/2,limit/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [foreach/2,map/2,foldl/3,reverse/1,member/2,last/1]).

-ifdef(DEBUG).
wings() -> "Wings 3D [debug]".
-else.
wings() -> "Wings 3D".
-endif.
    
share(X, X, X) -> {X,X,X};
share(X, X, Z) -> {X,X,Z};
share(X, Y, Y) -> {X,Y,Y};
share(X, Y, X) -> {X,Y,X};
share(X, Y, Z) -> {X,Y,Z}.

share({X,X,X}) -> {X,X,X};
share({X,X,Z}) -> {X,X,Z};
share({X,Y,Y}) -> {X,Y,Y};
share({X,Y,X}) -> {X,Y,X};
%%
share({X,X,X,X}) -> {X,X,X,X};
%%
share({X,X,X,A}) -> {X,X,X,A};
share({X,X,Z,X}) -> {X,X,Z,X};
share({X,Y,X,X}) -> {X,Y,X,X};
share({X,Y,Y,Y}) -> {X,Y,Y,Y};
%%
share({X,X,Y,Y}) -> {X,X,Y,Y};
share({X,Y,X,Y}) -> {X,Y,X,Y};
share({X,Y,Y,X}) -> {X,Y,Y,X};
%%
share({X,X,Z,A}) -> {X,X,Z,A};
share({X,Y,X,A}) -> {X,Y,X,A};
share({X,Y,Z,X}) -> {X,Y,Z,X};
share({X,Y,Y,A}) -> {X,Y,Y,A};
share({X,Y,Z,Y}) -> {X,Y,Z,Y};
share({X,Y,Z,Z}) -> {X,Y,Z,Z};
%%
share(Other) -> Other.

make_vector({_,_,_}=Vec) -> Vec;
make_vector(x) -> {1.0,0.0,0.0};
make_vector(y) -> {0.0,1.0,0.0};
make_vector(z) -> {0.0,0.0,1.0};
make_vector(free) -> free;
make_vector(normal) -> normal;
make_vector(intrude) -> normal;
make_vector(Axis) when Axis == last_axis; Axis == default_axis ->
    {_,Vec} = wings_pref:get_value(Axis),
    Vec.

key_format(Key, Msg) ->
    [Key,160,Msg].

%% Like io_lib:format/2, but with very restricted format characters.
%% BUT allows arguments to ~s to be lists containing Unicode characters.
%%
%% Format directives allowed: ~s ~p

format(Format, Args) ->
    format_1(Format, Args, []).

rel2fam(Rel) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(Rel))).

quote(Str) when is_list(Str) ->
    [$",Str,$"].

stringify({Atom,Other}) when is_atom(Atom) ->
    cap(atom_to_list(Atom)) ++
	case stringify(Other) of
	    [] -> [];
	    Str -> "|" ++ Str
	end;
stringify(Atom) when is_atom(Atom) ->
    cap(atom_to_list(Atom));
stringify(Int) when integer(Int) ->
    integer_to_list(Int);
stringify(_Other) -> [].

cap(Str) when is_atom(Str) -> cap(atom_to_list(Str));
cap(Str) -> cap(Str, true).

cap([Lower|T], true) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|cap(T, false)];
cap([$_|T], _Any) ->
    [$\s|cap(T, true)];
cap([H|T], _Any) ->
    [H|cap(T, false)];
cap([], _Flag) -> [].
    
upper(Str) when is_atom(Str) -> upper(atom_to_list(Str));
upper([Lower|T]) when $a =< Lower, Lower =< $z ->
    [Lower-$a+$A|upper(T)];
upper([H|T]) ->
    [H|upper(T)];
upper([]) -> [].

%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
					 C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when 16#80 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
					    C2 band 16#C0 =:= 16#80,
					    C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when 16#800 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80,
					       C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when 16#10000 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {reverse(Acc),Bad}.
    

add_vpos(Vs, #we{vp=Vtab}) -> add_vpos(Vs, Vtab);
add_vpos(Vs, Vtab) ->
    foldl(fun(V, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A]
	  end, [], Vs).

update_vpos(Vs, #we{vp=Vtab}) -> update_vpos(Vs, Vtab);
update_vpos(Vs, Vtab) ->
    foldl(fun({V,_}, A) ->
		  [{V,gb_trees:get(V, Vtab)}|A];
	     ({V,_,Dist,Inf}, A) ->
		  [{V,gb_trees:get(V, Vtab),Dist,Inf}|A]
	  end, [], reverse(Vs)).

gb_trees_smallest_key({_, Tree}) ->
    smallest_key1(Tree).

smallest_key1({Key, _Value, nil, _Larger}) ->
    Key;
smallest_key1({_Key, _Value, Smaller, _Larger}) ->
    smallest_key1(Smaller).

gb_trees_largest_key({_, Tree}) ->
    largest_key1(Tree).

largest_key1({Key, _Value, _Smaller, nil}) ->
    Key;
largest_key1({_Key, _Value, _Smaller, Larger}) ->
    largest_key1(Larger).

gb_trees_map(F, {Size,Tree}) ->
    {Size,gb_trees_map_1(F, Tree)}.

gb_trees_map_1(_, nil) -> nil;
gb_trees_map_1(F, {K,V,Smaller,Larger}) ->
    {K,F(K, V),
     gb_trees_map_1(F, Smaller),
     gb_trees_map_1(F, Larger)}.

gb_trees_to_gb_set({Size,Tree}) ->
    {Size,gb_trees_to_gb_set_1(Tree)}.

gb_trees_to_gb_set_1(nil) ->
    nil;
gb_trees_to_gb_set_1({K,_,Smaller,Larger}) ->
    {K,gb_trees_to_gb_set_1(Smaller),gb_trees_to_gb_set_1(Larger)}.

nice_float(F) when is_float(F) ->
    simplify_float(lists:flatten(io_lib:format("~f", [F]))).

simplify_float(F) ->
    reverse(simplify_float_1(reverse(F))).

simplify_float_1("0."++_=F) -> F;
simplify_float_1("0"++F) -> simplify_float_1(F);
simplify_float_1(F) -> F.

%%
%% Create a unique name by appending digits.
%%

unique_name(Name, Names) ->
    case member(Name, Names) of
	false -> Name;
	true -> unique_name_1(reverse(Name), Names)
    end.

unique_name_1([C|Cs], Names) when $0 =< C, C =< $9, Cs /= [] ->
    unique_name_1(Cs, Names);
unique_name_1(Name, Names0) ->
    Base0 = [First|_] = reverse(Name),
    Names = [N || N <- Names0, hd(N) =:= First],
    Base = case member($\s, Base0) andalso last(Base0) =/= $\s of
	       true -> Base0 ++ " ";
	       false -> Base0
	   end,
    unique_name_2(Base, 2, gb_sets:from_list(Names)).

unique_name_2(Base, I, Names) ->
    Name = Base ++ integer_to_list(I),
    case gb_sets:is_member(Name, Names) of
	true -> unique_name_2(Base, I+1, Names);
	false -> Name
    end.

tc(Fun,Mod,Line) ->
    case timer:tc(erlang, apply, [Fun,[]]) of
	{_,{'EXIT',Reason}} -> exit(Reason);
	{T,R} ->
	    io:format("~p:~p: Time: ~p\n", [Mod, Line, T]),
	    R
    end.

max(A, B) when A > B -> A;
max(_A, B) -> B.

min(A, B) when A < B -> A;
min(_A, B) -> B.

limit(Val, {'-infinity',infinity}) -> Val;
limit(Val, {Min,infinity}) when Val < Min -> Min;
limit(Val, {'-infinity',Max}) when Val > Max -> Max;
limit(Val, {Min,Max}) when Min < Max, Val < Min -> Min;
limit(Val, {Min,Max}) when Min < Max, Val > Max -> Max;
limit(Val, {Min,Max}) when Min < Max -> Val.

%%%
%%% Local functions.
%%%

format_1("~s"++F, [S0|Args], Acc) ->
    S = if
	    is_atom(S0) -> atom_to_list(S0);
	    is_list(S0) -> S0;
	    true -> 
		io:format("Bad string formatter for ~p in ~p~n", 
			  [S0, lists:flatten(reverse(Acc) ++ F)]),
		erlang:error(badarg)
	end,
    format_1(F, Args, [S|Acc]);
format_1("~p"++F, [S0|Args], Acc) ->
    S = format_p(S0),
    format_1(F, Args, [S|Acc]);
format_1("~~"++F, Args, Acc) ->
    format_1(F, Args, [$~|Acc]);
format_1([C|F], Args, Acc) when C =/= $~ ->
    format_1(F, Args, [C|Acc]);
format_1([], [], Acc) -> reverse(Acc).

format_p(Str) when is_list(Str)  ->
    [$",Str,$"];
format_p(Str) ->
    io_lib:format("~p", [Str]).
