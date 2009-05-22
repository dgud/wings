%%
%%  wings_draw_setup.erl --
%%
%%     Setup and Create data binaries for drawing
%%
%%  Copyright (c) 2009 Dan Gudmundsson & Björn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_draw_setup).

-export([work/2, smooth/2]).
-include("wings.hrl").

%%
-record(dd, {i = 0,       % vertex index
	     vs= <<>>,    % vertex pos
	     n = <<>>,    % normals
	     x = none,    % uv or vertex color binary
	     fm= [],      % Face map: id -> {StartIndex,VertexCount},
	     type=2}).    % type default uv i.e 2

%% API

%% Setup face_vs and face_fn and additional uv coords or vertex colors
work(D=#dlo{face_vs=none, src_we=#we{fs=Ftab}}, St) ->
    Prepared = wings_draw_util:prepare(gb_trees:to_list(Ftab), D, St),
    setup_flat_faces(Prepared, D);
work(D=#dlo{face_fn=none}, _St) ->
    setup_flat_normals(D);
work(D, _) -> D.

%% Setup face_vs and face_sn and additional uv coords or vertex colors
smooth(D=#dlo{face_vs=none, src_we=#we{fs=Ftab}}, St) ->
    exit(nyi); % Should we do it like this.
%%     Prepared = wings_draw_util:prepare(gb_trees:to_list(Ftab), D, St),
%%     setup_smooth_faces(Prepared, D);
smooth(D=#dlo{face_sn=none}, _St) ->
    setup_smooth_normals(D);
smooth(D, _) -> D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_flat_faces({material,MatFaces,#st{mat=Mtab}}, D) ->
    {MatInfo,DD} = mat_flat_faces(MatFaces,D,Mtab,[],#dd{}),
    D#dlo{face_vs = DD#dd.vs,     % Vertex positions
	  face_fn = DD#dd.n,      % Flat Normals
	  face_uv = DD#dd.x,     % UV coords or vertex colors
	  face_map= array:from_orddict(lists:sort(DD#dd.fm)),   % Face Map
	  mat_map = MatInfo % Mat Map
	 }.

mat_flat_faces([{Mat,Fs}|T],D,Mtab,MatMap,Acc0=#dd{x=UV,i=Start}) ->
    IncludeUVs = wings_pref:get_value(show_textures) andalso
	wings_material:has_texture(Mat,Mtab),
    if
	IncludeUVs, UV =/= none ->
	    Acc = flat_faces_uv(Fs,D,Acc0);
	IncludeUVs ->
	    %% Material requires UV's but we don't have any for the
	    %% previous faces.
	    UVSize = Start * 4*2,
	    PrevUV = <<0:UVSize>>,
	    Acc = flat_faces_uv(Fs,D,Acc0#dd{x=PrevUV});
	UV =:= none ->
	    Acc = flat_faces(Fs,D,Acc0);
	true -> %% We have uv for other materials
	    Acc1 = #dd{i=Stop,x=UV0} = flat_faces(Fs,D,Acc0),
	    UVSize = (Stop - Start)*4*2,
	    UVBin = <<UV/binary, 0:UVSize>>,
	    Acc  = Acc1#dd{x=UVBin}
    end,
    Size  = Acc#dd.i - Start,
    mat_flat_faces(T,D,Mtab,[{Mat,Start,Size}|MatMap],Acc);

mat_flat_faces([], _D,_Mtab,MatMap,Acc) ->
    {MatMap,Acc}.

flat_faces([{Face,_Edge}|Fs], D, Acc0) ->
    Acc = flat_face(Face, D, Acc0),
    flat_faces(Fs, D, Acc);
flat_faces([], _, Acc) -> Acc.

flat_face(Face, #dlo{ns=Ns}, DD=#dd{i=Start,vs=Vs,n=FN,fm=FaceMap}) ->
    case gb_trees:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    DD#dd{i = Start+3,
		  vs= add3(Vs, Pos),
		  n = dup3(3,FN, Normal),
		  fm=[{Face, {Start,3}}|FaceMap]};
	[Normal|Pos] ->
	    DD#dd{i =Start+6,
		  vs=add4(Vs, Pos),
		  n =dup3(6,FN,Normal),
		  fm=[{Face, {Start,6}}|FaceMap]};
	{N,Fs,VsPos} ->
	    NoVs  = length(Fs) * 3,
	    VsBin = add_list(Fs, list_to_tuple(VsPos), Vs),
	    FNBin = dup3(NoVs, FN, N),
	    DD#dd{i=NoVs+Start,vs=VsBin,
		  n=FNBin,fm=[{Face, {Start,NoVs}}|FaceMap]}
    end.

flat_faces_uv([{Face,Edge}|Fs], D, Acc0) ->
    Acc = flat_face_uv(Face, Edge, D, Acc0),
    flat_faces_uv(Fs, D, Acc);
flat_faces_uv([], _, Acc) -> Acc.

flat_face_uv(Face, Edge, #dlo{src_we=We,ns=Ns},
	     DD=#dd{i=Start,vs=Vs,n=FN,x=UV,fm=FaceMap,type=Type}
	    ) ->
    UVs = wings_face:vertex_info(Face, Edge, We),
    case gb_trees:get(Face, Ns) of
	[Normal|Pos = [_,_,_]] ->
	    DD#dd{i = Start+3,
		  vs= add3(Vs, Pos),
		  n = dup3(3, FN, Normal),
		  x = add3_x(UV, UVs, Type),
		  fm=[{Face, {Start,3}}|FaceMap]};
	[Normal|Pos] ->
	    DD#dd{i = Start+6,
		  vs= add4(Vs, Pos),
		  n = dup3(6, FN, Normal),
		  x = add4_x(UV, UVs, Type),
		  fm=[{Face, {Start,6}}|FaceMap]};
	{N,Fs,VsPos0} ->
	    NoVs  = length(Fs) * 3,
	    VsPos = list_to_tuple(VsPos0),
	    VsBin = add_list(Fs, VsPos, Vs),
	    FNBin = dup3(NoVs, FN, N),
	    DD#dd{i = NoVs+Start,
		  vs= VsBin,
		  n = FNBin,
		  x = add_list_x(Fs, list_to_tuple(UVs), UV, VsPos, Type),
		  fm= [{Face, {Start,NoVs}}|FaceMap]}
    end.

%% setup only normals
setup_flat_normals(D=#dlo{face_map=Fmap0, ns=Ns}) ->
    Fs = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    FN = setup_flat_normals_1(Fs, Ns, <<>>),
    D#dlo{face_fn = FN}.

setup_flat_normals_1([{Face, {_, Count}}|Fs], Ns, FN) ->
    [Normal|_] = gb_trees:get(Face,Ns),
    setup_flat_normals_1(Fs, Ns, dup3(Count,FN,Normal));
setup_flat_normals_1([],_,FN) ->
    FN.

setup_smooth_normals(D=#dlo{src_we=#we{}=We,ns=Ns0,face_map=Fmap0}) ->
    Ns1 = lists:foldl(fun({F,[N|_]}, A) -> [{F,N}|A];
			 ({F,{N,_,_}}, A) -> [{F,N}|A]
		      end, [], gb_trees:to_list(Ns0)),
    Ns = lists:reverse(Ns1),
    %%NOTE: This must be stable i.e. same order as in the flat case. (is it?)
    Flist = wings_we:normals(Ns, We),
    Ftab  = array:from_orddict(Flist),
    Fs    = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    SN = setup_smooth_normals(Fs, Ftab, Ns0, <<>>),
    D#dlo{face_sn=SN}.

setup_smooth_normals([{Face, {_,3}}|Fs], Ftab, Flat, SN0) ->
    [[_|N1],[_|N2],[_|N3]] = array:get(Face,Ftab),
    SN = add3(SN0, [N1,N2,N3]),
    setup_smooth_normals(Fs,Ftab,Flat,SN);
setup_smooth_normals([{Face, {_,6}}|Fs], Ftab, Flat, SN0) ->
    [[_|N1],[_|N2],[_|N3],[_|N4]] = array:get(Face,Ftab),
    SN = add4(SN0, [N1,N2,N3,N4]),
    setup_smooth_normals(Fs,Ftab,Flat,SN);
setup_smooth_normals([{Face, {_,Count}}|Fs], Ftab, Flat, SN0) ->
    VsInfo = list_to_tuple(array:get(Face,Ftab)),
    {FNormal,TriFs,Pos} = gb_trees:get(Face,Flat),
    SN = case size(VsInfo) =:= length(Pos) of
	     true  ->
		 setup_smooth_normals_1(TriFs,VsInfo,SN0);
	     false ->
		 %% Tesselated face set flat normals
		 %%(instead of random as previously)
		 dup3(Count,SN0,FNormal)
	 end,
    setup_smooth_normals(Fs,Ftab,Flat,SN);
setup_smooth_normals([],_,_,SN) ->
    SN.

setup_smooth_normals_1([{A,B,C}|Fs], VsInfo, SN0) ->
    [_|N1] = element(A,VsInfo),
    [_|N2] = element(B,VsInfo),
    [_|N3] = element(C,VsInfo),
    SN = add3(SN0, [N1,N2,N3]),
    setup_smooth_normals_1(Fs,VsInfo,SN);
setup_smooth_normals_1([], _, SN) ->
    SN.


%%
%% Create binaries
%%

add3(Bin, [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32 >>.

add4(Bin, [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32>>.

add_list([{A,B,C}|Fs], Vtab, Vs0) ->
    PA = element(A, Vtab),
    PB = element(B, Vtab),
    PC = element(C, Vtab),
    Vs = add3(Vs0, [PA,PB,PC]),
    add_list(Fs, Vtab, Vs);
add_list([], _, Vs) -> Vs.

dup3(3, Bin, {NX,NY,NZ}) ->
    <<Bin/binary,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32 >>;
dup3(6, Bin, {NX,NY,NZ}) ->
    <<Bin/binary,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     NX:?F32,NY:?F32,NZ:?F32 >>;
dup3(I, Bin0, N={NX,NY,NZ}) ->
    Bin = <<Bin0/binary,
	   NX:?F32,NY:?F32,NZ:?F32,
	   NX:?F32,NY:?F32,NZ:?F32,
	   NX:?F32,NY:?F32,NZ:?F32 >>,
    dup3(I-3, Bin, N).

dup2(I, Bin0, N={NX,NY}) when I > 0 ->
    Bin = <<Bin0/binary,
	   NX:?F32,NY:?F32,
	   NX:?F32,NY:?F32,
	   NX:?F32,NY:?F32 >>,
    dup3(I-3, Bin, N).

add3_x(Bin, [{U1,V1},{U2,V2},{U3,V3}], 2) ->
    <<Bin/binary,U1:?F32,V1:?F32,U2:?F32,V2:?F32,U3:?F32,V3:?F32>>;
add3_x(Bin, [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}], 3) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32>>;
add3_x(Bin, Other, 3) ->
    add_x3(Bin, Other);
add3_x(Bin, Other, 2) ->
    add_x2(Bin, Other).

add4_x(Bin, [{U1,V1},{U2,V2},{U3,V3},{U4,V4}], 2) ->
    <<Bin/binary,
     U1:?F32,V1:?F32,U2:?F32,V2:?F32,
     U3:?F32,V3:?F32,U3:?F32,V3:?F32,
     U4:?F32,V4:?F32,U1:?F32,V1:?F32>>;
add4_x(Bin, [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}], 3) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32>>;
add4_x(Bin, [A,B,C,D], 3) ->
    add_x3(Bin, [A,B,C,C,D,A]);
add4_x(Bin, [A,B,C,D], 2) ->
    add_x2(Bin, [A,B,C,C,D,A]).

add_x2(Bin, [{U1,V1}|T]) ->
    add_x2(<<Bin/binary,U1:?F32,V1:?F32>>,T);
add_x2(Bin, [_|T]) ->
    add_x2(<<Bin/binary,0:?F32,0:?F32>>,T);
add_x2(Bin, []) -> Bin.

add_x3(Bin, [{R,G,B}|T]) ->
    add_x2(<<Bin/binary,R:?F32,G:?F32,B:?F32>>,T);
add_x3(Bin, [_|T]) ->
    add_x2(<<Bin/binary,1.0:?F32,1.0:?F32,1.0:?F32>>,T);
add_x3(Bin, []) -> Bin.

add_list_x(Fs, UVs, Bin, VsPos, Type) ->
    case size(UVs) =:= size(VsPos) of
	true ->
	    add_list_x_1(Fs, UVs, Bin, Type);
	false when Type =:= 2 ->
	    dup2(length(Fs)*3,{0.0,0.0},Bin);
	false when Type =:= 2 ->
	    dup3(length(Fs)*3,{1.0,1.0,1.0},Bin)
    end.

add_list_x_1([{A,B,C}|Fs], Xtab, Bin0, Type) ->
    PA = element(A, Xtab),
    PB = element(B, Xtab),
    PC = element(C, Xtab),
    Bin = add3_x(Bin0, [PA,PB,PC], Type),
    add_list_x_1(Fs, Xtab, Bin, Type);
add_list_x_1([],_,Bin,_) -> Bin.
