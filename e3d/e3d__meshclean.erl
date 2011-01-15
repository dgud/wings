%%
%%  e3d__meshclean.erl --
%%
%%     Internal module for cleaning E3D meshes.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(e3d__meshclean).
-export([orient_normals/1,clean_faces/1]).

-include("e3d.hrl").

-import(lists, [reverse/1,foldl/3]).

%% orient_normals(Mesh0) -> Mesh
%%  Orient the face normals consistently.
orient_normals(#e3d_mesh{fs=Fs0}=Mesh) ->
    Faces = number_faces(Fs0),
    Edges = build_edges(Faces),

    Ws0 = sofs:relation(Edges, [{face,{edge,side}}]),
    Ws1 = sofs:relation_to_family(Ws0),
    Ws = gb_trees:from_orddict(sofs:to_external(Ws1)),

    ProjFun2 = {external,fun({F,{E,S}}) -> {E,{F,S}} end},
    E2F0 = sofs:projection(ProjFun2, Ws0),
    E2F = ets:new(e2f, [bag, named_table]),
    ets:insert(E2F, sofs:to_external(E2F0)),
    
    case catch orient_0(Ws,[], 100) of
	{'EXIT', Reason} -> 
	    ets:delete(e2f),
	    %%io:format("Reorient-normals failed with: ~p~n", [Reason]),
	    exit(Reason);
	Res0 -> 
	    ets:delete(e2f),    
	    Ftab0 = sofs:from_term(Faces, [{face,data}]),
	    Res = sofs:from_term(gb_trees:to_list(Res0), [{face,ok}]),
	    Ftab2 = sofs:relative_product1(Ftab0, Res),
	    Ftab = foldl(fun({F,true}, A) -> [F|A];
			    ({#e3d_face{vs=Vs}=F,false}, A) ->
				 [F#e3d_face{vs=reverse(Vs)}|A]
			 end, [], sofs:to_external(Ftab2)),
	    Mesh#e3d_mesh{fs=Ftab}
    end.

qadd(Face, Es, Bool, Q) ->
    lists:foldl(fun({E,S}, Q0) -> 
			queue:in({Face,{E, not Bool xor S}},Q0) 
		end,Q,Es).
qget(Q) ->
    queue:out(Q).

orient_0(_Ws0,BF,Max) when length(BF) > Max  -> 
    exit({to_many_retries, ?MODULE});
orient_0(Ws0,BF,Max) ->
    case catch orient_1(Ws0,gb_trees:empty()) of
	{bad, Bad} ->
%% 	    io:format("Found bad face ~p when reorienting deleting it and retrying ~n",
%% 		      [Bad]),
	    Es0 = gb_trees:get(Bad, Ws0),
	    Ws = gb_trees:delete(Bad, Ws0),
	    [ets:delete_object(e2f, {E, {Bad,S}}) || {E,S} <- Es0], 
	    orient_0(Ws,[Bad|BF],Max);
	Else ->
	    Else
    end.

orient_1(Ws0, Res0) ->
    case gb_trees:is_empty(Ws0) of
	true -> Res0;
	false ->	    
	    {Face,Es,Ws1} = gb_trees:take_smallest(Ws0),
	    Res1 = gb_trees:insert(Face, true, Res0),
	    Q = queue:new(),
	    {Ws,Res} = orient_2(qadd(Face,Es,true,Q), Ws1, Res1, undefined),
	    orient_1(Ws, Res)
    end.

orient_2(Es0, Ws0, Res0, Bad0) ->
    case qget(Es0) of
	{empty, _} -> {Ws0,Res0};
	{{value,{Face,{E,Side}}},Es1} ->
	    case ets:lookup(e2f, E) of
		[_] ->
		    orient_2(Es1, Ws0, Res0,Bad0);
		[{E,{Face,_}},{E,Other}] ->
		    {Es,Ws,Res,Bad} = 
			orient_3(Other, E, {Face,Side}, Es1, Ws0, Res0, Bad0),
		    orient_2(Es, Ws, Res, Bad);
		[{E,Other},{E,{Face,_}}] ->
		    {Es,Ws,Res,Bad} = 
			orient_3(Other, E, {Face,Side}, Es1, Ws0, Res0, Bad0),
		    orient_2(Es, Ws, Res, Bad)
	    end
    end.

orient_3({Face,Side1}, _E, {Father,Side2}, Es0, Ws0, Res0, Bad0) ->
    Correct = not (Side1 == Side2),
    case gb_trees:lookup(Face, Ws0) of
	{value,FaceEs0} ->
	    Bad = if Correct ->            Bad0;
		     Bad0 == undefined ->  Father;
		     true ->               Bad0
		  end,
	    Ws = gb_trees:delete(Face, Ws0),
	    Res = gb_trees:insert(Face, Correct, Res0),
	    Es = qadd(Face,FaceEs0,Correct,Es0),
	    {Es,Ws,Res,Bad};
	none ->
	    Orient = gb_trees:get(Face, Res0),
	    if 
		Orient == Correct ->   % Consistent
		    ok;
		Bad0 == undefined ->   % NOT consistent	and no Bad Face
% 		    io:format("Not cons ~p ~p => ~p(~p) Father has ~p Bad ~p~n", 
% 			      [_E,Father,Face,Correct,gb_trees:lookup(Father, Res0),Bad0]),
		    throw({bad,Father});
		true ->
% 		    io:format("Not cons ~p ~p => ~p(~p) Father has ~p Bad ~p~n", 
% 			      [_E,Father,Face,Correct,gb_trees:lookup(Father, Res0),Bad0]),
		    throw({bad,Bad0})
	    end,
	    {Es0,Ws0,Res0,Bad0}
    end.

build_edges(Fs) ->
    build_edges(Fs, []).

build_edges([{Face,#e3d_face{vs=Vs}}|Fs], Acc0) ->
    Edges = pairs(Vs),
    Acc = foldl(fun(Edge, A) ->
			Name = edge_name(Edge),
			Side = Name =:= Edge,
			[{Face,{edge_name(Edge),Side}}|A]
		end, Acc0, Edges),
    build_edges(Fs, Acc);
build_edges([], Acc) -> Acc.

pairs(Vs) ->
    pairs(Vs, Vs, []).
pairs([V2|[V1|_]=Vs], First, Acc) ->
    pairs(Vs, First, [{V1,V2}|Acc]);
pairs([V2], [V1|_], Acc) ->
    [{V1,V2}|Acc].

edge_name({Vs,Ve}=Name) when Vs < Ve -> Name;
edge_name({Vs,Ve}) -> {Ve,Vs}.

number_faces(Fs) ->
    number_faces(Fs, 0, []).
number_faces([Rec|Fs], Face, Acc) ->
    number_faces(Fs, Face+1, [{Face,Rec}|Acc]);
number_faces([], _, Acc) -> reverse(Acc).

%% clean_faces(Mesh0) -> Mesh
%%  Remove duplicate vertices, faces with fewer than three edges,
%%  and faces with vertices that are not defined in the vertex table.
%%  Also remove vertex colors, texture coordinates, and/or normals
%%  for the face if they are invalid.
clean_faces(#e3d_mesh{fs=Fs0,vs=Vs,vc=Vc,tx=Tx,ns=Ns}=Mesh0) ->
    Sizes = {length(Vs),[{length(Vc),#e3d_face.vc},
			 {length(Tx),#e3d_face.tx},
			 {length(Ns),#e3d_face.ns}]},
    Fs1 = clean_bad_refs(Fs0, Sizes, []),
    Fs = clean_faces_1(Fs1, []),
    Mesh = Mesh0#e3d_mesh{fs=Fs},
    e3d_mesh:renumber(Mesh).

clean_faces_1([#e3d_face{}=Face0|Fs], Acc) ->
    #e3d_face{vs=Vs} = Face = clean_face_dups(Face0),
    case Vs of
	[_,_,_|_] ->
	    case length(ordsets:from_list(Vs)) =:= length(Vs) of
		true ->
		    clean_faces_1(Fs, [Face|Acc]);
		false ->
		    clean_faces_1(Fs, Acc)
	    end;
	_ -> clean_faces_1(Fs, Acc)
    end;
clean_faces_1([], Acc) -> reverse(Acc).

clean_face_dups(#e3d_face{vs=Vs0,vc=Vc0,tx=Tx0,ns=Ns0}=Face) ->
    L0 = zip_face(Vs0, Vc0, Tx0, Ns0),
    L = clean_dup_vs(L0, L0),
    {Vs,Vc,Tx,Ns} = unzip_face(L),
    Face#e3d_face{vs=Vs,vc=Vc,tx=Tx,ns=Ns}.
	    
clean_dup_vs([{V,_}|[{V,_}|_]=Vs], First) ->
    clean_dup_vs(Vs, First);
clean_dup_vs([{V,_}], [{V,_}|_]) -> [];
clean_dup_vs([V|Vs], First) ->
    [V|clean_dup_vs(Vs, First)];
clean_dup_vs([], _) -> [].

zip_face([V|Vs], A0, B0, C0) ->
    [A1|A] = head_tail(A0),
    [B1|B] = head_tail(B0),
    [C1|C] = head_tail(C0),
    [{V,{A1,B1,C1}}|zip_face(Vs, A, B, C)];
zip_face([], [], [], []) -> [].

head_tail([]) -> [none|[]];
head_tail([_|_]=L) -> L.

unzip_face(L) ->
    unzip_face_1(L, [], [], [], []).

unzip_face_1([{V,{A,B,C}}|T], Vacc0, Aacc0, Bacc0, Cacc0) ->
    Vacc = [V|Vacc0],
    Aacc = cons(A, Aacc0),
    Bacc = cons(B, Bacc0),
    Cacc = cons(C, Cacc0),
    unzip_face_1(T, Vacc, Aacc, Bacc, Cacc);
unzip_face_1([], Vacc, Aacc, Bacc, Cacc) ->
    {reverse(Vacc),reverse(Aacc),reverse(Bacc),reverse(Cacc)}.

cons(none, Tail) -> Tail;
cons(Head, Tail) -> [Head|Tail].

clean_bad_refs([#e3d_face{vs=Vs}=Face0|Fs],
	       {Vsz,SzList}=Sizes, Acc) ->
    case [V || V <- Vs, V >= Vsz] of
	[_|_] ->
	    %% There is a missing vertex. We will delete the
	    %% entire face.
	    clean_bad_refs(Fs, Sizes, Acc);
	[] ->
	    %% Vertices are OK. Now check all other attributes.
	    Face = clean_bad_refs_1(SzList, Face0),
	    clean_bad_refs(Fs, Sizes, [Face|Acc])
    end;
clean_bad_refs([], _, Acc) -> reverse(Acc).

clean_bad_refs_1([{Sz,Pos}|T], Face0) ->
    case [E || E <- element(Pos, Face0), E >= Sz] of
	[] ->
	    clean_bad_refs_1(T, Face0);
	[_|_] ->
	    %% A bad vertex color, UV coordinate, or normal.
	    %% Reset to empty list.
	    %%io:format("~p: ~p\n", [Pos,Face0]),
	    Face = setelement(Pos, Face0, []),
	    clean_bad_refs_1(T, Face)
    end;
clean_bad_refs_1([], Face) -> Face.
