%%
%%  e3d_util.erl --
%%
%%     Utility functions.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_util.erl,v 1.2 2006/08/02 20:25:02 antoneos Exp $
%%

-module(e3d_util).
-export([make_uniq/2, indexed_to_raw/2, raw_to_indexed/1]).

-import(lists, [reverse/1,sort/1,sublist/2,sublist/3,all/2]).

make_uniq([Name|_]=Ns0, Max) when is_atom(Name) ->
    Ns1 = [atom_to_list(N) || N <- Ns0],
    Ns = make_uniq(Ns1, Max),
    [{list_to_atom(Orig),list_to_atom(New)} || {Orig,New} <- Ns];
make_uniq(Ns, Max) -> make_uniq_0(Ns, Max).

make_uniq_0(L0, Max) ->
    L1 = [{sublist(Name, Max),Name} || Name <- L0],
    L = make_uniq_1(L1, Max),
    [{From,To} || {To,From} <- L].

make_uniq_1(L, Max) ->
    R = sofs:relation(L),
    F0 = sofs:relation_to_family(R),
    F = sofs:to_external(F0),
    make_uniq_2(F, Max, [], done).

make_uniq_2([{Short,[Orig]}|T], Max, Acc, Status) ->
    make_uniq_2(T, Max, [{Short,Orig}|Acc], Status);
make_uniq_2([{Short,Os0}|T], Max, Acc0, _) ->
    Os = [{Short,Orig} || Orig <- Os0],
    Acc = remove_chars(Os, Max, []) ++ Acc0,
    make_uniq_2(T, Max, Acc, not_done);
make_uniq_2([], _, Acc, done) -> Acc;
make_uniq_2([], Max, Acc, not_done) ->
    L0 = [{sublist(Key, Max),Key} || {Key,_} <- Acc],
    L1 = make_uniq_1(L0, Max),
    L2 = sofs:relation(L1),
    L = sofs:relative_product(L2, sofs:relation(Acc)),
    sofs:to_external(L).

remove_chars([{Orig,Orig}=Pair|T], Max, Acc) when length(Orig) < Max ->
    remove_chars(T, Max, [Pair|Acc]);
remove_chars([{Key,Orig}|T], Max, Acc) when length(Key) >= Max ->
    New = {sublist(Key, Max-1)++sublist(Orig, Max+1, 999999),Orig},
    remove_chars(T, Max, [New|Acc]);
remove_chars([{Key,Orig}|T], Max, Acc) ->
    C = trunc($a+rand:uniform(26)),
    remove_chars(T, Max, [{[C|Key],Orig}|Acc]);
remove_chars([], Max, Acc) ->
    L0 = [{sublist(Key, Max),Key} || {Key,_} <- Acc],
    L1 = make_uniq_1(L0, Max),
    L2 = sofs:relation(L1),
    L = sofs:relative_product(L2, sofs:relation(Acc)),
    sofs:to_external(L).

%
% Functions for meshes stored in indexed format (verts/faces lists)
%
indexed_to_raw(Verts, Faces) -> % replace indices with values
    Make_Raw_Face = fun(Face) ->
			[lists:nth(Index+1, Verts) || Index <- Face]
		    end,
    lists:map(Make_Raw_Face, Faces).

raw_to_indexed(RawTriangles) -> % replace values with indices
    VertsWithDups = lists:append(RawTriangles),
    Verts = remove_dups(VertsWithDups),
    IndexedVerts = dict:from_list(add_indices(Verts)),
    Get_Index = fun(Vertex) -> dict:fetch(Vertex, IndexedVerts) end,
    Process_Face = fun(Face) -> lists:map(Get_Index, Face) end,
    Faces = lists:map(Process_Face, RawTriangles),  % re-indexes the Faces list
    {Verts, Faces}.

remove_dups(List) -> % remove duplicates (uses Zero tolerance)
    lists:usort(List).

add_indices(List) ->
    ListLen = length(List),
    Indices = lists:seq(0, ListLen-1),
    IndexedList = lists:zip(List, Indices),
    IndexedList.

