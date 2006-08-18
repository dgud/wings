%%
%%  wings__du.erl --
%%
%%     Low-level drawing utilities.
%%
%%  Copyright (c) 2003-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings__du.erl,v 1.18 2004/04/20 17:49:17 bjorng Exp $
%%

-module(wings__du).
-export([plain_face/1,plain_face/2,uv_face/2,uv_face/3,vcol_face/2,vcol_face/3,
	 smooth_plain_face/2,smooth_plain_face/3,
	 smooth_uv_face/2,smooth_uv_face/3,
	 smooth_vcol_face/2,smooth_vcol_face/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,seq/2]).

%% plain_face([Position]) -> ok
%%  Draw a tri or quad face with neither UV coordinates nor vertex colors.
plain_face([A,B,C]) ->
    wpc_ogla:tri(A, B, C);
plain_face([A,B,C,D]) ->
    wpc_ogla:quad_tri(A, B, C, D).

%% plain_face([{VertexA,VertexB,VertexC}], [Position]) -> ok
%%  Draw a face with neither UV coordinates nor vertex colors.
plain_face(Fs, VsPos) ->
    plain_face_1(Fs, list_to_tuple(VsPos)).

plain_face_1([{A,B,C}|Fs], Vtab) ->
    wpc_ogla:tri(element(A, Vtab), element(B, Vtab), element(C, Vtab)),
    plain_face_1(Fs, Vtab);
plain_face_1([], _) -> ok.

%% uv_face([Position], [UV]) -> ok
%%  Draw a tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face([A,B,C], [UVa,UVb,UVc]) ->
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    uv_face_vtx(C, UVc);
uv_face([A,B,C,D], [UVa,UVb,UVc,UVd]) ->
    uv_face_vtx(A, UVa),
    uv_face_vtx(B, UVb),
    uv_face_vtx(C, UVc),
    gl:vertex3fv(C),
    uv_face_vtx(D, UVd),
    uv_face_vtx(A, UVa).

%% uv_face([{VertexA,VertexB,VertexC}], [Position], [UV]) -> ok
%%  Draw a face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
uv_face(Fs, VsPos0, UVs0) ->
    case {list_to_tuple(VsPos0),list_to_tuple(UVs0)} of
	{VsPos,UVs} when size(VsPos) =:= size(UVs) ->
	    uv_face_1(Fs, none, VsPos, UVs);
	{VsPos,_} ->
	    plain_face_1(Fs, VsPos)
    end.

uv_face_1([{A,B,C}|Fs], Prev, Vtab, UVtab) ->
    if
	A =:= Prev ->
	    gl:vertex3fv(element(A, Vtab));
	true ->
	    uv_face_vtx(element(A, Vtab), element(A, UVtab))
    end,
    uv_face_vtx(element(B, Vtab), element(B, UVtab)),
    uv_face_vtx(element(C, Vtab), element(C, UVtab)),
    uv_face_1(Fs, C, Vtab, UVtab);
uv_face_1([], _, _, _) -> ok.

uv_face_vtx(Pos, {U,V}) ->
    gl:texCoord2f(U, V),
    gl:vertex3fv(Pos);
uv_face_vtx(Pos, _) ->
    gl:texCoord2i(0, 0),
    gl:vertex3fv(Pos).

%% vcol_face([Position], [Color]) -> ok
%%  Draw a tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face([A,B,C], [Ca,Cb,Cc]) ->
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb, Ca),
    vcol_face_vtx(C, Cc, Cb);
vcol_face([A,B,C,D], [Ca,Cb,Cc,Cd]) ->
    vcol_face_vtx(A, Ca),
    vcol_face_vtx(B, Cb, Ca),
    vcol_face_vtx(C, Cc, Cb),
    gl:vertex3fv(C),
    vcol_face_vtx(D, Cd, Cc),
    vcol_face_vtx(A, Ca, Cd).

%% vcol_face([{VertexA,VertexB,VertexC}], [Position], [Color]) -> ok
%%  Draw a face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
vcol_face(Fs, VsPos0, Cols0) ->
    case {list_to_tuple(VsPos0),list_to_tuple(Cols0)} of
	{VsPos,Cols} when size(VsPos) =:= size(Cols) ->
	    vcol_face_1(Fs, none, VsPos, Cols);
	{VsPos,_} ->
	    plain_face_1(Fs, VsPos)
    end.

vcol_face_1([{A,B,C}|Fs], Prev, Vtab, Ctab) ->
    Acol = element(A, Ctab),
    if
	A =:= Prev ->
	    gl:vertex3fv(element(A, Vtab));
	true ->
	    vcol_face_vtx(element(A, Vtab), Acol)
    end,
    vcol_face_vtx(element(B, Vtab), Bcol=element(B, Ctab), Acol),
    vcol_face_vtx(element(C, Vtab), element(C, Ctab), Bcol),
    vcol_face_1(Fs, C, Vtab, Ctab);
vcol_face_1([], _, _, _) -> ok.

vcol_face_vtx(Pos, Prev, Prev) ->
    gl:vertex3fv(Pos);
vcol_face_vtx(Pos, Col, _) ->
    vcol_face_vtx(Pos, Col).

vcol_face_vtx(Pos, {R,G,B}) ->
    gl:color3f(R, G, B),
    gl:vertex3fv(Pos);
vcol_face_vtx(Pos, _) ->
    gl:color3f(1, 1, 1),
    gl:vertex3fv(Pos).

%%%
%%% Drawing of faces with smooth normals.
%%%

%% smooth_plain_face([{Position,_,VertexNormal}]) -> ok
%%  Draw a smooth tri or quad with neither UV coordinates nor vertex colors.
smooth_plain_face([A,B,C], [Na,Nb,Nc]) ->
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(C, Nc);
smooth_plain_face([A,B,C,D], [Na,Nb,Nc,Nd]) ->
    smooth_plain_face_vtx(A, Na),
    smooth_plain_face_vtx(B, Nb),
    smooth_plain_face_vtx(C, Nc),
    gl:vertex3fv(C),
    smooth_plain_face_vtx(D, Nd),
    smooth_plain_face_vtx(A, Na).

%% smooth_plain_face([{VertexA,VertexB,VertexC}],
%%                   [Position], [[_|VertexNormal]]) -> ok
%%  Draw a smooth face with neither UV coordinates nor vertex colors.
smooth_plain_face(Fs, VsPos0, Ns0) ->
    case {list_to_tuple(VsPos0),list_to_tuple(Ns0)} of
	{VsPos,Ns} when size(VsPos) =:= size(Ns) ->
	    smooth_plain_face_1(Fs, none, VsPos, Ns);
	{VsPos,_} ->
	    plain_face_1(Fs, VsPos)
    end.

smooth_plain_face_1([{A,B,C}|Fs], Prev, Vtab, Ntab) ->
    if
	A =:= Prev ->
	    gl:vertex3fv(element(A, Vtab));
	true ->
	    smooth_plain_face_vtx(element(A, Vtab), element(A, Ntab))
    end,
    smooth_plain_face_vtx(element(B, Vtab), element(B, Ntab)),
    smooth_plain_face_vtx(element(C, Vtab), element(C, Ntab)),
    smooth_plain_face_1(Fs, C, Vtab, Ntab);
smooth_plain_face_1([], _, _, _) -> ok.

smooth_plain_face_vtx(P, [_|N]) ->
    gl:normal3fv(N),
    gl:vertex3fv(P).

%% smooth_uv_face([{Position,UV,VertexNormal}]) -> ok
%%  Draw a smoth tri or quad with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face([A,B,C], [Ai,Bi,Ci]) ->
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    smooth_uv_face_vtx(C, Ci);
smooth_uv_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    smooth_uv_face_vtx(A, Ai),
    smooth_uv_face_vtx(B, Bi),
    smooth_uv_face_vtx(C, Ci),
    gl:vertex3fv(C),
    smooth_uv_face_vtx(D, Di),
    smooth_uv_face_vtx(A, Ai).

%% smooth_uv_face([{VertexA,VertexB,VertexC}],
%%                [Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smoth face with UV coordinates. For vertices without
%%  UV coordinates, (0, 0) will be used.
smooth_uv_face(Fs, VsPos0, UVs0) ->
    case {list_to_tuple(VsPos0),list_to_tuple(UVs0)} of
	{VsPos,UVs} when size(VsPos) =:= size(UVs) ->
	    smooth_uv_face_1(Fs, none, VsPos, UVs);
	{VsPos,_} ->
	    plain_face_1(Fs, VsPos)
    end.

smooth_uv_face_1([{A,B,C}|Fs], Prev, Vtab, UVtab) ->
    if
	A =:= Prev ->
	    gl:vertex3fv(element(A, Vtab));
	true ->
	    smooth_uv_face_vtx(element(A, Vtab), element(A, UVtab))
    end,
    smooth_uv_face_vtx(element(B, Vtab), element(B, UVtab)),
    smooth_uv_face_vtx(element(C, Vtab), element(C, UVtab)),
    smooth_uv_face_1(Fs, C, Vtab, UVtab);
smooth_uv_face_1([], _, _, _) -> ok.

smooth_uv_face_vtx(P, [{U,V}|N]) ->
    gl:texCoord2f(U, V),
    gl:normal3fv(N),
    gl:vertex3fv(P);
smooth_uv_face_vtx(P, [_|N]) ->
    gl:texCoord2i(0, 0),
    gl:normal3fv(N),
    gl:vertex3fv(P).

%% smooth_vcol_face([Position], [[UV|VertexNormal]]) -> ok
%%  Draw a smooth tri or quad with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face([A,B,C], [Ai,Bi,Ci]) ->
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi, Ai),
    smooth_vcol_face_vtx(C, Ci, Bi);
smooth_vcol_face([A,B,C,D], [Ai,Bi,Ci,Di]) ->
    smooth_vcol_face_vtx(A, Ai),
    smooth_vcol_face_vtx(B, Bi, Ai),
    smooth_vcol_face_vtx(C, Ci, Bi),
    gl:vertex3fv(C),
    smooth_vcol_face_vtx(D, Di, Ci),
    smooth_vcol_face_vtx(A, Ai, Di).

%% smooth_vcol_face([{VertexA,VertexB,VertexC}],
%%                  [Position], [[Color|VertexNormal]]) -> ok
%%  Draw a smooth face with vertex colors. For vertices without
%%  vertex colors, (1.0, 1.0, 1.0) will be used.
smooth_vcol_face(Fs, VsPos0, Cols0) ->
    case {list_to_tuple(VsPos0),list_to_tuple(Cols0)} of
	{VsPos,Cols} when size(VsPos) =:= size(Cols) ->
	    smooth_vcol_face_1(Fs, none, VsPos, Cols);
	{VsPos,_} ->
	    plain_face_1(Fs, VsPos)
    end.

smooth_vcol_face_1([{A,B,C}|Fs], Prev, Vtab, Ctab) ->
    Ai = element(A, Ctab),
    if
	A =:= Prev ->
	    gl:vertex3fv(element(A, Vtab));
	true ->
	    smooth_vcol_face_vtx(element(A, Vtab), Ai)
    end,
    smooth_vcol_face_vtx(element(B, Vtab), Bi=element(B, Ctab), Ai),
    smooth_vcol_face_vtx(element(C, Vtab), element(C, Ctab), Bi),
    smooth_vcol_face_1(Fs, C, Vtab, Ctab);
smooth_vcol_face_1([], _, _, _) -> ok.

smooth_vcol_face_vtx(P, [Col|N], [Col|_]) ->
    gl:normal3fv(N),
    gl:vertex3fv(P);
smooth_vcol_face_vtx(P, Attr, _) ->
    smooth_vcol_face_vtx(P, Attr).

smooth_vcol_face_vtx(P, [{R,G,B}|N]) ->
    gl:color3f(R, G, B),
    gl:normal3fv(N),
    gl:vertex3fv(P);
smooth_vcol_face_vtx(P, [_|N]) ->
    gl:color3f(1, 1, 1),
    gl:normal3fv(N),
    gl:vertex3fv(P).
