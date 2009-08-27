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

-export([we/3]).  %% For plugins

-export([work/2,smooth/2,prepare/3,flat_faces/2]).
-export([enableVertexPointer/1,enableNormalPointer/1,
	 enableColorPointer/1,enableTexCoordPointer/1,
	 disableVertexPointer/1,disableNormalPointer/1,
	 disableColorPointer/1,disableTexCoordPointer/1]).
-export([face_vertex_count/1]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,any/2,sort/1]).

%%%
%%% we(We, [Option], St) -> #vab{} See wings.hrl
%%%    Generates rendering buffers from a we,
%%%    reuses data if available.
%%% Options are:
%%%    {smooth, true|false}  default false
%%%    {subdiv, Level}       default  0 not implemented yet.
%%%
we(We, Options, St) ->
    wings_dl:fold(fun(Dl, undefined) -> we_1(Dl, We, Options, St);
		     (_Dl, Res)      -> Res
		  end, undefined).

we_1(Dlo=#dlo{src_we=Orig=#we{id=Id}}, Curr=#we{id=Id}, Opt, St) ->
    case Orig =:= Curr of
	true  -> we_2(Dlo, Opt, St);
	false -> we_2(#dlo{src_we=Curr}, Opt, St)
    end;
we_1(_, _, _, _) ->
    undefined.

we_2(Dlo, Opt, St) ->
    Smooth = proplists:get_value(smooth, Opt, false),
    #dlo{vab=Vab} =
	case Smooth of
	    true ->  smooth(Dlo, St);
	    false -> work(Dlo, St)
	end,
    Vab.

%%%
%%% Help functions to activate and disable buffer pointers.
%%%

enableVertexPointer({Stride,BinVs}) ->
    gl:vertexPointer(3, ?GL_FLOAT, Stride, BinVs),
    gl:enableClientState(?GL_VERTEX_ARRAY),
    true.

enableNormalPointer({Stride,Ns}) ->
    gl:normalPointer(?GL_FLOAT, Stride, Ns),
    gl:enableClientState(?GL_NORMAL_ARRAY),
    true.

enableColorPointer({Stride,Color}) ->
    gl:colorPointer(3, ?GL_FLOAT, Stride, Color),
    gl:enableClientState(?GL_COLOR_ARRAY),
    true;
enableColorPointer(none) -> false.

enableTexCoordPointer({Stride,UV}) ->
    gl:texCoordPointer(2, ?GL_FLOAT, Stride, UV),
    gl:enableClientState(?GL_TEXTURE_COORD_ARRAY),
    true;
enableTexCoordPointer(none) -> false.

disableVertexPointer({_Stride,_BinVs}) ->
    gl:disableClientState(?GL_VERTEX_ARRAY).

disableNormalPointer({_Stride,_Ns}) ->
    gl:disableClientState(?GL_NORMAL_ARRAY).

disableColorPointer({_Stride,_Color}) ->
    gl:disableClientState(?GL_COLOR_ARRAY);
disableColorPointer(none) -> ok.

disableTexCoordPointer({_Stride,_UV}) ->
    gl:disableClientState(?GL_TEXTURE_COORD_ARRAY);
disableTexCoordPointer(none) -> ok.

face_vertex_count(#dlo{vab=#vab{mat_map=[{_Mat,_Type,Start,Count}|_]}}) ->
    Start+Count;
face_vertex_count(#vab{mat_map=[{_Mat,_Type,Start,Count}|_]}) ->
    Start+Count.

%% Setup face_vs and face_fn and additional uv coords or vertex colors
work(#dlo{vab=none,src_we=#we{fs=Ftab}}=D, St) ->
    Prepared = prepare(gb_trees:to_list(Ftab), D, St),
    flat_faces(Prepared, D);
work(#dlo{vab=#vab{face_vs=none},src_we=#we{fs=Ftab}}=D, St) ->
    Prepared = prepare(gb_trees:to_list(Ftab), D, St),
    flat_faces(Prepared, D);
work(#dlo{vab=#vab{face_fn=none}}=D, _St) ->
    %% Can this really happen?
    setup_flat_normals(D);
work(D, _) -> D.

%% Setup face_vs and face_sn and additional uv coords or vertex colors
smooth(#dlo{vab=none}=D, St) ->
    setup_smooth_normals(work(D, St));
smooth(#dlo{vab=#vab{face_vs=none}}=D, St) ->
    setup_smooth_normals(work(D, St));
smooth(D=#dlo{vab=#vab{face_sn=none}}, _St) ->
    setup_smooth_normals(D);
smooth(D, _) -> D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flat_faces({plain,MatFaces}, D) ->
    plain_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({uv,MatFaces}, D) ->
    uv_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({color,MatFaces}, D) ->
    col_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({color_uv,MatFaces}, D) ->
    col_uv_faces(MatFaces, D, 0, <<>>, [], []).

plain_flat_faces([{Mat,Fs}|T], #dlo{ns=Ns}=D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, Ns, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    case Vs of
	<<>> ->
	    Ns = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs
    end,
    S = 24,
    D#dlo{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_uv=none,
		   face_map=FaceMap,mat_map=MatInfo}}.

flat_faces_1([{Face,_}|Fs], Ns, Start, Vs, FaceMap) ->
    case array:get(Face, Ns) of
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
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    uv_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
uv_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    case Vs of
	<<>> ->
	    Ns = UV = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,UV/bytes>> = Ns
    end,
    S = 32,
    D#dlo{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_uv={S,UV},
		   face_map=FaceMap,mat_map=MatInfo}}.

uv_flat_faces_1([{Face,Edge}|Fs], #dlo{ns=Ns,src_we=We}=D, Start, Vs, FaceMap) ->
    UVs = wings_va:face_attr(uv, Face, Edge, We),
    case array:get(Face, Ns) of
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

col_flat_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_flat_faces_1(Fs, D, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    col_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
col_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    case Vs of
	<<>> ->
	    Ns = Col = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,Col/bytes>> = Ns
    end,
    S = 36,
    D#dlo{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},face_vc={S,Col},
		   face_uv=none,face_map=FaceMap,mat_map=MatInfo}}.

col_flat_faces_1([{Face,Edge}|T], #dlo{ns=Ns,src_we=We}=D, Start, Vs0, Fmap0) ->
    Cols = wings_va:face_attr(color, Face, Edge, We),
    case array:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    Vs = add_col_tri(Vs0, Normal, Pos, Cols),
	    Fmap = [{Face,{Start,3}}|Fmap0],
	    col_flat_faces_1(T, D, Start+3, Vs, Fmap);
	[Normal|Pos] ->
	    Vs = add_col_quad(Vs0, Normal, Pos, Cols),
	    Fmap = [{Face,{Start,6}}|Fmap0],
	    col_flat_faces_1(T, D, Start+6, Vs, Fmap);
	{Normal,Faces,VsPos} ->
	    NumVs  = length(Faces) * 3,
	    Vs = add_col_poly(Vs0, Normal, Faces,
			      list_to_tuple(VsPos), list_to_tuple(Cols)),
	    Fmap = [{Face,{Start,NumVs}}|Fmap0],
	    col_flat_faces_1(T, D, Start+NumVs, Vs, Fmap)
    end;
col_flat_faces_1([], _, Start, Vs, Fmap) ->
    {Start,Vs,Fmap}.

col_uv_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_uv_faces_1(Fs, D, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    col_uv_faces(T, D, Start, Vs, FaceMap, MatInfo);
col_uv_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    case Vs of
	<<>> ->
	    Ns = Col = UV = Vs;
	_ ->
	    <<_:3/unit:32,Ns/bytes>> = Vs,
	    <<_:3/unit:32,Col/bytes>> = Ns,
	    <<_:3/unit:32,UV/bytes>> = Col
    end,
    S = 44,
    D#dlo{vab=#vab{face_vs={S,Vs},face_fn={S,Ns},
		   face_vc={S,Col},face_uv={S,UV},
		   face_map=FaceMap,mat_map=MatInfo}}.

col_uv_faces_1([{Face,Edge}|Fs], #dlo{ns=Ns,src_we=We}=D, Start, Vs, FaceMap) ->
    UVs = wings_va:face_attr([color|uv], Face, Edge, We),
    case array:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    col_uv_faces_1(Fs, D, Start+3,
			   add_col_uv_tri(Vs, Normal, Pos, UVs),
			   [{Face,{Start,3}}|FaceMap]);
	[Normal|Pos] ->
	    col_uv_faces_1(Fs, D, Start+6,
			   add_col_uv_quad(Vs, Normal, Pos, UVs),
			   [{Face,{Start,6}}|FaceMap]);
	{Normal,Faces,VsPos} ->
	    NoVs  = length(Faces) * 3,
	    VsBin = add_col_uv_poly(Vs, Normal, Faces,
			     list_to_tuple(VsPos), list_to_tuple(UVs)),
	    col_uv_faces_1(Fs, D, NoVs+Start,
			   VsBin, [{Face,{Start,NoVs}}|FaceMap])
    end;
col_uv_faces_1([], _, Start, Vs, FaceMap) ->
    {Start,Vs,FaceMap}.

%% setup only normals
setup_flat_normals(D=#dlo{vab=#vab{face_map=Fmap0}=Vab,ns=Ns}) ->
    Fs = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    FN = setup_flat_normals_1(Fs, Ns, <<>>),
    D#dlo{vab=Vab#vab{face_fn={0,FN}}}.

setup_flat_normals_1([{Face, {_, Count}}|Fs], Ns, FN) ->
    [Normal|_] = array:get(Face,Ns),
    setup_flat_normals_1(Fs, Ns, dup3(Count,FN,Normal));
setup_flat_normals_1([],_,FN) ->
    FN.

setup_smooth_normals(D=#dlo{src_we=#we{}=We,ns=Ns0,mirror=MM,
			    vab=#vab{face_map=Fmap0}=Vab}) ->
    Ns1 = array:sparse_foldl(fun(F,[N|_], A) -> [{F,N}|A];
				(F,{N,_,_}, A) -> [{F,N}|A]
			     end, [], Ns0),
    Ns = reverse(Ns1),
    Flist = wings_we:normals(Ns, We, MM),
    Ftab  = array:from_orddict(Flist),
    Fs    = lists:keysort(2, array:sparse_to_orddict(Fmap0)),
    SN = setup_smooth_normals(Fs, Ftab, Ns0, <<>>),
    D#dlo{vab=Vab#vab{face_sn={0,SN}}}.

setup_smooth_normals([{Face,{_,3}}|Fs], Ftab, Flat, SN0) ->
    %% One triangle.
    case array:get(Face, Ftab) of
	[N1,N2,N3] ->
	    %% Common case: the face is a triangle.
	    SN = add3(SN0, N1, N2, N3),
	    setup_smooth_normals(Fs, Ftab, Flat, SN);
	_ ->
	    %% Degenerate case: The original face had more than
	    %% 3 vertices, so there should have been at least
	    %% 2 triangles, but some triangles were degenerated
	    %% and therefore discarded. Use the face normal
	    %% for all vertices.
	    {Fn,_,_} = array:get(Face, Flat),
	    SN = dup3(3, SN0, Fn),
	    setup_smooth_normals(Fs, Ftab, Flat, SN)
    end;
setup_smooth_normals([{Face,{_,6}}|Fs], Ftab, Flat, SN0) ->
    %% Two triangles.
    case array:get(Face, Ftab) of
	[N1,N2,N3,N4] ->
	    %% Common case: triangulated quad.
	    SN = add4(SN0, N1, N2, N3, N4),
	    setup_smooth_normals(Fs, Ftab, Flat, SN);
	_ ->
	    %% Degenerate case: The original face had more than
	    %% 4 vertices, so there should have been at least
	    %% 3 triangles, but some triangles were degenerated
	    %% and therefore discarded. Use the face normal
	    %% for all vertices.
	    {Fn,_,_} = array:get(Face, Flat),
	    SN = dup3(6, SN0, Fn),
	    setup_smooth_normals(Fs, Ftab, Flat, SN)
    end;
setup_smooth_normals([{Face,{_,Count}}|Fs], Ftab, Flat, SN0) ->
    VsInfo = list_to_tuple(array:get(Face, Ftab)),
    {FNormal,TriFs,Pos} = array:get(Face, Flat),
    SN = case size(VsInfo) =:= length(Pos) of
	     true  ->
		 setup_smooth_normals_1(TriFs,VsInfo,SN0);
	     false ->
		 %% Tesselated face set flat normals
		 %%(instead of random as previously)
		 dup3(Count,SN0,FNormal)
	 end,
    setup_smooth_normals(Fs,Ftab,Flat,SN);
setup_smooth_normals([],_,_,SN) -> SN.

setup_smooth_normals_1([{A,B,C}|Fs], VsInfo, SN0) ->
    N1 = element(A, VsInfo),
    N2 = element(B, VsInfo),
    N3 = element(C, VsInfo),
    SN = add3(SN0, N1, N2, N3),
    setup_smooth_normals_1(Fs,VsInfo,SN);
setup_smooth_normals_1([], _, SN) -> SN.

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
     U3:?F32,V3:?F32>>;
add_tri(Bin,N, Pos, _UV) ->
    Z = {0.0,0.0},
    add_tri(Bin, N, Pos, [Z,Z,Z]).

add_col_uv_tri(Bin, {NX,NY,NZ},
	       [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}],
	       [[{R1,G1,B1}|{U1,V1}],
		[{R2,G2,B2}|{U2,V2}],
		[{R3,G3,B3}|{U3,V3}]]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     U3:?F32,V3:?F32>>;
add_col_uv_tri(Bin, N, Pos, Attrs0) ->
    Attrs = fix_color_uv(Attrs0),
    add_col_uv_tri(Bin, N, Pos, Attrs).

add_col_tri(Bin, {NX,NY,NZ},
	    [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3}],
	    [{R1,G1,B1},{R2,G2,B2},{R3,G3,B3}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32>>;
add_col_tri(Bin,N, Pos, Cols0) ->
    Cols = [def_color(C) || C <- Cols0],
    add_col_tri(Bin, N, Pos, Cols).

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

add_col_quad(Bin, {NX,NY,NZ},
	     [{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
	     [{R1,G1,B1},{R2,G2,B2},{R3,G3,B3},{R4,G4,B4}]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R4:?F32,G4:?F32,B4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32>>;
add_col_quad(Bin, N, Pos, Cols0) ->
    Cols = [def_color(C) || C <- Cols0],
    add_col_quad(Bin, N, Pos, Cols).

add_col_uv_quad(Bin, {NX,NY,NZ},
		[{X1,Y1,Z1},{X2,Y2,Z2},{X3,Y3,Z3},{X4,Y4,Z4}],
		[[{R1,G1,B1}|{U1,V1}],
		 [{R2,G2,B2}|{U2,V2}],
		 [{R3,G3,B3}|{U3,V3}],
		 [{R4,G4,B4}|{U4,V4}]]) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     U1:?F32,V1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R2:?F32,G2:?F32,B2:?F32,
     U2:?F32,V2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     U3:?F32,V3:?F32,
     X3:?F32,Y3:?F32,Z3:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R3:?F32,G3:?F32,B3:?F32,
     U3:?F32,V3:?F32,
     X4:?F32,Y4:?F32,Z4:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R4:?F32,G4:?F32,B4:?F32,
     U4:?F32,V4:?F32,
     X1:?F32,Y1:?F32,Z1:?F32,
     NX:?F32,NY:?F32,NZ:?F32,
     R1:?F32,G1:?F32,B1:?F32,
     U1:?F32,V1:?F32>>;
add_col_uv_quad(Bin, N, Pos, Attrs0) ->
    Attrs = fix_color_uv(Attrs0),
    add_col_uv_quad(Bin, N, Pos, Attrs).

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
    %% A tesselated face may have more Vs than UVs
    UVa = uv_element(A, Vtab, UVtab),
    UVb = uv_element(B, Vtab, UVtab),
    UVc = uv_element(C, Vtab, UVtab),
    Vs = add_tri(Vs0, Normal, [PA,PB,PC], [UVa,UVb,UVc]),
    add_poly(Vs, Normal, Fs, Vtab, UVtab);
add_poly(Vs, _, _, _, _) -> Vs.

add_col_poly(Vs0, Normal, [{A,B,C}|Fs], Vtab, ColTab) ->
    PA = element(A, Vtab),
    PB = element(B, Vtab),
    PC = element(C, Vtab),
    %% A tesselated face may have more vertices than colors
    ColA = col_element(A, Vtab, ColTab),
    ColB = col_element(B, Vtab, ColTab),
    ColC = col_element(C, Vtab, ColTab),
    Vs = add_col_tri(Vs0, Normal, [PA,PB,PC], [ColA,ColB,ColC]),
    add_col_poly(Vs, Normal, Fs, Vtab, ColTab);
add_col_poly(Vs, _, _, _, _) -> Vs.

add_col_uv_poly(Vs0, Normal, [{A,B,C}|Fs], Vtab, AttrTab) ->
    PA = element(A, Vtab),
    PB = element(B, Vtab),
    PC = element(C, Vtab),
    %% A tesselated face may have more vertices than vertex attributes
    AttrA = attr_element(A, AttrTab),
    AttrB = attr_element(B, AttrTab),
    AttrC = attr_element(C, AttrTab),
    Vs = add_col_uv_tri(Vs0, Normal, [PA,PB,PC], [AttrA,AttrB,AttrC]),
    add_col_uv_poly(Vs, Normal, Fs, Vtab, AttrTab);
add_col_uv_poly(Vs, _, _, _, _) -> Vs.

uv_element(A, _Vtab, Tab) when A =< tuple_size(Tab) ->
    element(A, Tab);
uv_element(A, Vtab, Tab) ->
    find_element(tuple_size(Tab), element(A, Vtab), Vtab, Tab, {0.0,0.0}).

col_element(A, _, Tab) when A =< tuple_size(Tab) ->
    element(A, Tab);
col_element(A, Vtab, Tab) ->
    find_element(tuple_size(Tab), element(A, Vtab), Vtab, Tab, {1.0,1.0,1.0}).

find_element(0, _, _, _, Def) ->
    Def;
find_element(I, P, Vtab, UVTab, Def) ->
    case P =:= element(I, Vtab) of
	true -> element(I, UVTab);
	false -> find_element(I-1, P, Vtab, UVTab, Def)
    end.

attr_element(A, Tab) when A =< tuple_size(Tab) ->
    element(A, Tab);
attr_element(_, _) ->
    [none|none].

fix_color_uv(Attrs) ->
    case good_uvs(Attrs) of
	false ->
	    %% Bad UVs, possibly bad vertex colors too. Fix both.
	    Zuv = {0.0,0.0},
	    [[def_color(C)|Zuv] || [C|_] <- Attrs];
	true ->
	    %% Good UVs, bad vertex colors.
	    [[def_color(C)|UV] || [C|UV] <- Attrs]
    end.

good_uvs([[_|{_,_}]|T]) -> good_uvs(T);
good_uvs([_|_]) -> false;
good_uvs([]) -> true.

def_color({_,_,_}=C) -> C;
def_color(_) -> {1.0,1.0,1.0}.

add3(Bin, {X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}) ->
    <<Bin/binary,
     X1:?F32,Y1:?F32,Z1:?F32,
     X2:?F32,Y2:?F32,Z2:?F32,
     X3:?F32,Y3:?F32,Z3:?F32>>.

add4(Bin, {X1,Y1,Z1}, {X2,Y2,Z2}, {X3,Y3,Z3}, {X4,Y4,Z4}) ->
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

%%%
%%% Collect information about faces.
%%%

prepare(Ftab, #dlo{src_we=We}, St) ->
    prepare(Ftab, We, St);
prepare(Ftab0, #we{}=We, St) ->
    Ftab = wings_we:visible(Ftab0, We),
    prepare_1(Ftab, We, St).

prepare_1(Ftab, We, St) ->
    MatFaces = wings_facemat:mat_faces(Ftab, We),
    case wings_va:any_attributes(We) of
	false ->
	    %% Since there are no vertex attributes,
	    %% we don't need to look at the materials
	    %% to figure out what to do.
	    {plain,MatFaces};
	true ->
	    %% There are UV coordinates and/or vertex colors,
	    %% so we will have to look at the materials to
	    %% figure out what we'll need.
	    Attrs = wings_material:needed_attributes(We, St),
	    {prepare_2(Attrs),MatFaces}
    end.

prepare_2([]) ->
    plain;
prepare_2([color]) ->
    case wings_pref:get_value(show_colors) of
	false -> plain;
	true -> color
    end;
prepare_2([uv]) ->
    case wings_pref:get_value(show_textures) of
	true -> uv;
	false -> plain
    end;
prepare_2([color,uv]) ->
    case wings_pref:get_value(show_colors) of
	false ->
	    prepare_2([uv]);
	true ->
	    case wings_pref:get_value(show_textures) of
		false -> color;
		true -> color_uv
	    end
    end.
