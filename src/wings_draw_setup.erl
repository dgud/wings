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
-export([work/2,smooth/2]).
-export([vertexPointer/1,normalPointer/1,texCoordPointer/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,any/2,sort/1]).

%%%
%%% Help functions to set up buffer pointers.
%%%

vertexPointer({Stride,BinVs}) ->
    gl:vertexPointer(3, ?GL_FLOAT, Stride, BinVs).

normalPointer({Stride,Ns}) ->
    gl:normalPointer(?GL_FLOAT, Stride, Ns).

texCoordPointer({Stride,UV}) ->
    gl:texCoordPointer(2, ?GL_FLOAT, Stride, UV);
texCoordPointer(none) -> ok.

%% Setup face_vs and face_fn and additional uv coords or vertex colors
work(#dlo{face_vs=none,src_we=#we{fs=Ftab}}=D, St) ->
    Prepared = wings_draw_util:prepare(gb_trees:to_list(Ftab), D, St),
    setup_flat_faces(Prepared, D);
work(#dlo{face_fn=none}=D, _St) ->
    %% Can this really happen?
    setup_flat_normals(D);
work(D, _) -> D.

%% Setup face_vs and face_sn and additional uv coords or vertex colors
smooth(#dlo{face_vs=none}=D, St) ->
    setup_smooth_normals(work(D, St));
smooth(D=#dlo{face_sn=none}, _St) ->
    setup_smooth_normals(D);
smooth(D, _) -> D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_flat_faces({material,MatFaces,#st{mat=Mtab}}, D) ->
    mat_flat_faces(MatFaces, D, Mtab).

mat_flat_faces(MatFs, D, Mtab) ->
    IncludeUVs = wings_pref:get_value(show_textures) andalso
	any(fun({Mat,_}) ->
		    wings_material:has_texture(Mat, Mtab)
	    end, MatFs),
    case IncludeUVs of
	false ->
	    plain_flat_faces(MatFs, D, 0, <<>>, [], []);
	true ->
	    uv_flat_faces(MatFs, D, 0, <<>>, [], [])
    end.

plain_flat_faces([{Mat,Fs}|T], #dlo{ns=Ns}=D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, Ns, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    <<_:3/unit:32,Ns/bytes>> = Vs,
    D#dlo{face_vs={24,Vs},face_fn={24,Ns},face_uv=none,
	  face_map=FaceMap,mat_map=MatInfo}.

flat_faces_1([{Face,_}|Fs], Ns, Start, Vs, FaceMap) ->
    case gb_trees:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    flat_faces_1(Fs, Ns, Start+3,
			 add_tri(Vs, Normal, Pos),
			 [{Face,{Start,3}}|FaceMap]);
	[Normal|Pos] ->
	    flat_faces_1(Fs, Ns, Start+6,
			 add_quad(Vs, Normal, Pos),
			 [{Face,{Start,6}}|FaceMap]);
	{Normal,Faces,VsPos} ->
	    NoVs  = length(Faces) * 3,
	    VsBin = add_poly(Vs, Normal, Faces, list_to_tuple(VsPos)),
	    flat_faces_1(Fs, Ns, NoVs+Start,
			 VsBin, [{Face,{Start,NoVs}}|FaceMap])
    end;
flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

uv_flat_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = uv_flat_faces_1(Fs, D, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,Start0,Start-Start0}|MatInfo0],
    uv_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
uv_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    <<_:3/unit:32,Ns/bytes>> = Vs,
    <<_:3/unit:32,UV/bytes>> = Ns,
    D#dlo{face_vs={32,Vs},face_fn={32,Ns},face_uv={32,UV},
	  face_map=FaceMap,mat_map=MatInfo}.

uv_flat_faces_1([{Face,Edge}|Fs], #dlo{ns=Ns,src_we=We}=D, Start, Vs, FaceMap) ->
    UVs = wings_face:vertex_info(Face, Edge, We),
    case gb_trees:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    uv_flat_faces_1(Fs, D, Start+3,
			    add_tri(Vs, Normal, Pos, UVs),
			    [{Face,{Start,3}}|FaceMap]);
	[Normal|Pos] ->
	    uv_flat_faces_1(Fs, D, Start+6,
			    add_quad(Vs, Normal, Pos, UVs),
			    [{Face,{Start,6}}|FaceMap]);
	{Normal,Faces,VsPos} ->
	    NoVs  = length(Faces) * 3,
	    VsBin = add_poly(Vs, Normal, Faces,
			     list_to_tuple(VsPos), list_to_tuple(UVs)),
	    uv_flat_faces_1(Fs, D, NoVs+Start,
			    VsBin, [{Face,{Start,NoVs}}|FaceMap])
    end;
uv_flat_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

%% setup only normals
setup_flat_normals(D=#dlo{face_map=Fmap0,ns=Ns}) ->
    Fs = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    FN = setup_flat_normals_1(Fs, Ns, <<>>),
    D#dlo{face_fn={0,FN}}.

setup_flat_normals_1([{Face, {_, Count}}|Fs], Ns, FN) ->
    [Normal|_] = gb_trees:get(Face,Ns),
    setup_flat_normals_1(Fs, Ns, dup3(Count,FN,Normal));
setup_flat_normals_1([],_,FN) ->
    FN.

setup_smooth_normals(D=#dlo{src_we=#we{}=We,ns=Ns0,face_map=Fmap0}) ->
    Ns1 = lists:foldl(fun({F,[N|_]}, A) -> [{F,N}|A];
			 ({F,{N,_,_}}, A) -> [{F,N}|A]
		      end, [], gb_trees:to_list(Ns0)),
    Ns = reverse(Ns1),
    %%NOTE: This must be stable i.e. same order as in the flat case. (is it?)
    Flist = wings_we:normals(Ns, We),
    Ftab  = array:from_orddict(Flist),
    Fs    = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    SN = setup_smooth_normals(Fs, Ftab, Ns0, <<>>),
    D#dlo{face_sn={0,SN}}.

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

add_tri(Bin, {NX,NY,NZ},
	[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32>>.

add_tri(Bin, {NX,NY,NZ},
	[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}],
	[{U1,V1},{U2,V2},{U3,V3}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U3:?F32,V3:?F32>>.

add_quad(Bin, {NX,NY,NZ},
	 [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32>>.

add_quad(Bin, {NX,NY,NZ},
	 [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
	 [{U1,V1},{U2,V2},{U3,V3},{U4,V4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U3:?F32,V3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U3:?F32,V3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U4:?F32,V4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     U1:?F32,V1:?F32>>;
add_quad(Bin, N, Pos, _) ->
    Z = {0.0,0.0},
    add_quad(Bin, N, Pos, [Z,Z,Z,Z]).

add_poly(Vs0, Normal, [{A,B,C}|Fs], Vtab) ->
    PA = element(A, Vtab),
    PB = element(B, Vtab),
    PC = element(C, Vtab),
    Vs = add_tri(Vs0, Normal, [PA,PB,PC]),
    add_poly(Vs, Normal, Fs, Vtab);
add_poly(Vs, _, _, _) -> Vs.

add_poly(Vs0, Normal, [{A,B,C}|Fs], Vtab, UVtab) ->
    PA = element(A, Vtab),
    PB = element(B, Vtab),
    PC = element(C, Vtab),
    UVa = element(A, UVtab),
    UVb = element(B, UVtab),
    UVc = element(C, UVtab),
    Vs = add_tri(Vs0, Normal, [PA,PB,PC], [UVa,UVb,UVc]),
    add_poly(Vs, Normal, Fs, Vtab, UVtab);
add_poly(Vs, _, _, _, _) -> Vs.

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
