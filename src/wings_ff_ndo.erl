%%
%%  wings_ff_ndo.erl --
%%
%%     Import and export of Nendo .ndo files.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_ff_ndo).
-export([import/2,export/2]).
-include("wings.hrl").
-import(lists, [reverse/1,foldl/3,foreach/2]).

-define(NDO_HEADER10, "nendo 1.0").
-define(NDO_HEADER11, "nendo 1.1").

import(Name, St) ->
    case file:read_file(Name) of
	{ok,<<?NDO_HEADER10,_Data/binary>>} ->
            {error,?__(1,"Nendo 1.0 files not supported")};
	{ok,<<?NDO_HEADER11,Data/binary>>} ->
            import_1(Data, St);
	{ok,_Bin} ->
	    {error,?__(2,"not a Nendo file")};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

import_1(<<_:8,NumObjs:16,_:8,Objs/binary>>, St) ->
    %% io:format(?__(1,"~w object(s)\n"), [NumObjs]),
    read_objects(NumObjs, Objs, St).

read_objects(0, _, St) -> St;
read_objects(_N, <<>>, St) ->
    %% io:format(?__(1,"  ~w empty object(s)\n"), [N]),
    St;
read_objects(N, Bin, St0) ->
    case read_object(Bin) of
	bad ->
	    <<First:16/binary,_/binary>> = Bin,
            io:format(?__(2,"  garbage: ~w\n"), [First]),
	    St0;
	{Name,We,Rest} ->
	    St = wings_obj:new(Name, We, St0),
	    read_objects(N-1, Rest, St)
    end.

read_object(Bin) ->
    %%show_first(Bin),
    read_object_0(Bin).

read_object_0(<<0:8,_/binary>>=T) -> read_object_1(T);
read_object_0(<<_:8,T/binary>>) -> read_object_1(T).

read_object_1(<<0:16,T/binary>>) ->
    read_object(T);
read_object_1(<<1:16,C:8,_/binary>>=T0) when C < $\s; C > $~ ->
    <<_:16,T/binary>> = T0,
    read_object_1(T);
read_object_1(<<L:16,T0/binary>>) ->
    case get_name(L, T0) of
	bad -> bad;
	{Name,T1} ->
	    <<Vis:8,Sensivity:8,_:8,_:8,_:72/binary,T2/binary>> = T1,
	    Perm = case {Vis,Sensivity} of
		       {1,1} -> 0;		%Visible, unlocked
		       {1,0} -> 1;		%Visible, locked
		       {0,1} -> 2;		%Hidden, unlocked
		       {0,0} -> 3		%Hidden, locked
		   end,
	    {Etab,Htab,ColTab,T3} = read_edges(T2),
	    T4 = skip_faces(T3),
	    {Vtab,T5} = read_vertices(T4),
	    T = skip_rest(T5),
	    We0 = #we{es=Etab,vp=Vtab,he=Htab,perm=Perm},
	    We1 = wings_we:rebuild(We0),
	    We2 = wings_va:set_edge_colors(ColTab, We1),
	    We = clean_bad_edges(We2),
	    {Name,We,T}
    end.

get_name(L, Bin) when byte_size(Bin) < L -> bad;
get_name(L, Bin) ->
    <<Name0:L/binary,T/binary>> = Bin,
    Name = binary_to_list(Name0),
    foldl(fun(C, Val) when $\s =< C, C < 127 -> Val;
	     (_C, _) -> bad
	  end, {Name,T}, Name).

skip_rest(<<Sz0:16,T0/binary>>) ->
    Sz = 2*Sz0,
    <<_Skip:Sz/binary,T/binary>> = T0,
    %%io:format("  skipping ~w: ~w\n", [Szip,Sk]),
    skip_rest_1(T).

skip_rest_1(<<Sz0:16,T0/binary>>) ->
    Sz = 2*Sz0,
    <<_Skip:Sz/binary,T/binary>> = T0,
%%    io:format("  skipping ~w: ~w\n", [Sz,Skip]),
%%    show_first(T),
    skip_rest_2(T).

skip_rest_2(<<0:8,T/binary>>) -> T;
skip_rest_2(<<2:8,Sz1:16,Sz2:16,T/binary>>) ->
    Sz = Sz1 * Sz2,
    skip_texture(Sz, T);
skip_rest_2(<<4:8,Sz1:16,Sz2:16,T/binary>>) ->
    Sz = Sz1 * Sz2,
    skip_texture(Sz, T).

skip_texture(0, T) -> T;
skip_texture(N, <<Pixels:8,_RGB:24,T/binary>>) when N > 0 ->
    skip_texture(N-Pixels, T).

read_edges(<<NumEdges:16,T/binary>>) ->
    read_edges(0, NumEdges, T, [], [], []).
    
read_edges(N, N, T, Eacc, Hacc, ColAcc) ->
    Etab = array:from_orddict(reverse(Eacc)),
    Htab = gb_sets:from_ordset(reverse(Hacc)),
    {Etab,Htab,ColAcc,T};
read_edges(Edge, N, <<EdgeRec0:25/binary,T/binary>>, Eacc, Hacc0, ColAcc) ->
    <<Vb:16,Va:16,Lf:16,Rf:16,Ltsu:16,Rtsu:16,Rtpr:16,Ltpr:16,
     Hardness:8,BColor0:4/binary,AColor0:4/binary>> = EdgeRec0,
    AColor = convert_color(AColor0),
    BColor = convert_color(BColor0),
    EdgeRec = {Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
			  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu}},
    Hacc = if
	       Hardness =:= 0 -> Hacc0;
	       true -> [Edge|Hacc0]
	   end,
    ColInfo = {Edge,AColor,BColor},
    read_edges(Edge+1, N, T, [EdgeRec|Eacc], Hacc, [ColInfo|ColAcc]).

skip_faces(<<NumFaces:16,T0/binary>>) ->
    Skip = 2*NumFaces,
    <<_:Skip/binary,T/binary>> = T0,
    T.

read_vertices(<<NumVertices:16,T/binary>>) ->
    read_vertices(0, NumVertices, T, []).
    
read_vertices(N, N, T, Acc) ->
    {array:from_orddict(reverse(Acc)),T};
read_vertices(V, N,
	      <<_:16,X:32/float,Y:32/float,Z:32/float,T/binary>>, Acc) ->
    Pos = wings_util:share(X/10.0, Y/10.0, Z/10.0),
    read_vertices(V+1, N, T, [{V,Pos}|Acc]).

clean_bad_edges(#we{es=Etab}=We) ->
    clean_bad_edges(wings_util:array_keys(Etab), We).

clean_bad_edges([Edge|T], #we{es=Etab}=We0) ->
    We = case array:get(Edge, Etab) of
	     undefined-> We0;
	     #edge{ltpr=Same,ltsu=Same,rtpr=Same,rtsu=Same} ->
		 io:format(?__(1,"Bad edge: ~w\n"), [Edge]),
		 We0;
	     #edge{ltpr=Same,ltsu=Same} ->
		 wings_edge:dissolve_edge(Edge, We0);
	     #edge{rtpr=Same,rtsu=Same} ->
		 wings_edge:dissolve_edge(Edge, We0);
	     _ -> We0
	 end,
    clean_bad_edges(T, We);
clean_bad_edges([], We) -> We.

%%
%% Export.
%%

export(Name, #st{shapes=Shapes0}=St) ->
    Shapes1 = gb_trees:values(Shapes0),
    foreach(fun check_size/1, Shapes1),
    Shapes2 = foldl(fun(Sh, A) ->
			    shape(Sh, St, A)
		end, [], Shapes1),
    Shapes = reverse(Shapes2),
    write_file(Name, Shapes).

check_size(#we{name=Name,es=Etab}) ->
    case wings_util:array_entries(Etab) of
	Sz when Sz > 65535 ->
	    wings_u:error_msg(?__(1,"Object \"")
			  ++Name
			  ++?__(2,"\" cannot be exported ")
			  ++?__(3,"to Nendo format (too many edges)."));
	_ -> ok
    end.
	    
shape(We, _, Acc) when ?IS_LIGHT(We) -> Acc;
shape(#we{name=Name,perm=Perm}=We0, St, Acc) ->
    NameChunk = [<<(length(Name)):16>>|Name],
    Vis = if
	      ?IS_VISIBLE(Perm) -> 1;
	      true -> 0
	  end,
    Sense = if
		Perm =:= 1; Perm =:= 3 -> 0;
		true -> 1
	    end,
    Shaded = 1,
    EnableColors = 1,
    Header = <<Vis:8,Sense:8,Shaded:8,EnableColors:8,0:72/unit:8>>,
    We1 = wings_we:uv_to_color(We0, St),
    We = wings_we:renumber(We1, 0),
    #we{vc=Vct,vp=Vtab,es=Etab,fs=Ftab} = We,
    EdgeChunk = write_edges(array:sparse_to_orddict(Etab), We, []),
    FaceChunk = write_faces(gb_trees:values(Ftab), []),
    VertexChunk = write_vertices(array:sparse_to_list(Vct),
				 array:sparse_to_list(Vtab), []),
    FillChunk = [0,0,0,0,0,1],
    [[NameChunk,Header,EdgeChunk,FaceChunk,VertexChunk,FillChunk]|Acc].

write_edges([{Edge,Erec0}|Es], #we{he=Htab}=We, Acc) ->
    Hardness = case gb_sets:is_member(Edge, Htab) of
		   false -> 0;
		   true -> 1
	       end,
    ACol = wings_va:attr(color, wings_va:edge_attrs(Edge, left, We)),
    BCol = wings_va:attr(color, wings_va:edge_attrs(Edge, right, We)),
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
	  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Erec0,
    Erec = [<<Vb:16,Va:16,Lf:16,Rf:16,Ltsu:16,Rtsu:16,Rtpr:16,Ltpr:16,
	     Hardness:8>>,convert_color(BCol)|convert_color(ACol)],
    write_edges(Es, We, [Erec|Acc]);
write_edges([], _We, Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_faces([Edge|Fs], Acc) ->
    write_faces(Fs, [<<Edge:16>>|Acc]);
write_faces([], Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_vertices([Edge|Es], [{X,Y,Z}|Ps], Acc) ->
    Vtx = <<Edge:16,(X*10):32/float,(Y*10):32/float,(Z*10):32/float>>,
    write_vertices(Es, Ps, [Vtx|Acc]);
write_vertices([], [], Acc) ->
    list_to_binary([<<(length(Acc)):16>>|reverse(Acc)]).

write_file(Name, Objects) ->
    NumObjs = length(Objects),
    Data = [<<?NDO_HEADER11,0:8,NumObjs:16,1:8>>|Objects],
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

%%%
%%% Common utilities.
%%%

convert_color(<<R:8,G:8,B:8,_A:8>>) ->
    wings_color:store({R/255,G/255,B/255});
convert_color({R,G,B}) ->
    [trunc(R*255),trunc(G*255),trunc(B*255),255];
convert_color(none) ->
    [255,255,255,255].
