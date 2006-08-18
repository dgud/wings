%%
%%  wings_draw_util.erl --
%%
%%     Utilities for drawing objects.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_draw_util.erl,v 1.141 2005/01/09 21:08:43 bjorng Exp $
%%

-module(wings_draw_util).
-export([init/0,prepare/3,
	 plain_face/2,uv_face/3,vcol_face/2,vcol_face/3,
	 smooth_plain_faces/2,smooth_uv_faces/2,smooth_vcol_faces/2,
	 unlit_face/2,unlit_face/3,
	 force_flat_color/2,force_flat_color/3,good_triangulation/5]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,foreach/2,foldl/3,keydelete/3]).

init() ->
    P = <<16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55,
	 16#AA,16#AA,16#AA,16#AA,16#55,16#55,16#55,16#55>>,
    gl:polygonStipple(P).

%%%
%%% Set material and draw faces.
%%%

prepare(Ftab, #dlo{src_we=We}, St) ->
    prepare(Ftab, We, St);
prepare(Ftab0, We, St) ->
    Ftab = wings_we:visible(Ftab0, We),
    prepare_1(Ftab, We, St).

prepare_1(Ftab, #we{mode=vertex}=We, St) ->
    case {wings_pref:get_value(show_colors),Ftab} of
	{false,[{_,Edge}|_]} when is_integer(Edge) ->
	    Fs0 = sofs:from_external(Ftab, [{face,edge}]),
	    Fs1 = sofs:domain(Fs0),
	    Fs = sofs:to_external(Fs1),
	    {color,{[{wings_color:white(),Fs}],[]},St};
	{false,_} ->
	    {color,{[{wings_color:white(),Ftab}],[]},St};
	{true,_} ->
	    {color,vtx_color_split(Ftab, We),St}
    end;
prepare_1(Ftab, #we{mode=material}=We, St) ->
    {material,prepare_mat(Ftab, We),St}.

prepare_mat(Ftab, We) ->
    case wings_pref:get_value(show_materials) of
	false -> [{default,Ftab}];
	true -> wings_facemat:mat_faces(Ftab, We)
    end.

vtx_color_split([{_,Edge}|_]=Ftab0, We) when is_integer(Edge) ->
    vtx_color_split_1(Ftab0, We, [], []);
vtx_color_split(Ftab, _) ->
    vtx_smooth_color_split(Ftab).

vtx_color_split_1([{Face,Edge}|Fs], We, SameAcc, DiffAcc) ->
    Cols = wings_face:vertex_info(Face, Edge, We),
    case vtx_color_split_2(Cols) of
	different -> vtx_color_split_1(Fs, We, SameAcc, [[Face|Cols]|DiffAcc]);
	Col -> vtx_color_split_1(Fs, We, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_color_split_1([], _, SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_color_split_2(Cols0) ->
    case no_colors(Cols0) of
	true ->
	    wings_color:white();
	false ->
	    case Cols0 of
		[C,C|Cols] -> vtx_color_split_3(Cols, C);
		_ -> different
	    end
    end.

vtx_color_split_3([C|Cols], C) -> vtx_color_split_3(Cols, C);
vtx_color_split_3([_|_], _) -> different;
vtx_color_split_3([], C) -> C.

no_colors([{_,_,_}|_]) -> false;
no_colors([_|Cols]) -> no_colors(Cols);
no_colors([]) -> true.

vtx_smooth_color_split(Ftab) ->
    vtx_smooth_color_split_1(Ftab, [], []).

vtx_smooth_color_split_1([{_,Vs}=Face|Fs], SameAcc, DiffAcc) ->
    case vtx_smooth_face_color(Vs) of
	different ->
	    vtx_smooth_color_split_1(Fs, SameAcc, [Face|DiffAcc]);
	Col ->
	    vtx_smooth_color_split_1(Fs, [{Col,Face}|SameAcc], DiffAcc)
    end;
vtx_smooth_color_split_1([], SameAcc, DiffAcc) ->
    {wings_util:rel2fam(SameAcc),DiffAcc}.

vtx_smooth_face_color(Vs) ->
    case smooth_no_colors(Vs) of
	true ->
	    wings_color:white();
	false ->
	    case Vs of
		[[Col|_],[Col|_]|T] ->
		    vtx_smooth_face_color_1(T, Col);
		_ ->
		    different
	    end
    end.

vtx_smooth_face_color_1([[Col|_]|T], Col) ->
    vtx_smooth_face_color_1(T, Col);
vtx_smooth_face_color_1([_|_], _) -> different;
vtx_smooth_face_color_1([], Col) -> Col.

smooth_no_colors([[{_,_,_}|_]|_]) -> false;
smooth_no_colors([_|Cols]) -> smooth_no_colors(Cols);
smooth_no_colors([]) -> true.

%%
%% Triangulate and draw a face.
%%

plain_face(Face, #dlo{ns=Ns}) ->
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:plain_face(VsPos);
	{N,Fs,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:plain_face(Fs, VsPos)
    end;
plain_face(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    Ps = wings_face:vertex_positions(Face, Edge, We),
    case wings_draw:face_ns_data(Ps) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:plain_face(VsPos);
	{N,Fs,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:plain_face(Fs, VsPos)
    end.

%%
%% Tesselate and draw face. Include UV coordinates.
%%

uv_face(Face, Edge, #dlo{src_we=We,ns=Ns}) ->
    UVs = wings_face:vertex_info(Face, Edge, We),
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:uv_face(VsPos, UVs);
	{N,Fs,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:uv_face(Fs, VsPos, UVs)
    end.

%%
%% Tesselate and draw face. Include vertex colors.
%%

vcol_face(Face, #dlo{src_we=We,ns=Ns}) ->
    Cols = wings_face:vertex_info(Face, We),
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(VsPos, Cols);
	{N,Fs,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(Fs, VsPos, Cols)
    end.

vcol_face(Face, #dlo{ns=Ns}, Cols) ->
    case gb_trees:get(Face, Ns) of
	[N|VsPos] ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(VsPos, Cols);
	{N,Fs,VsPos} ->
	    gl:normal3fv(N),
	    wings__du:vcol_face(Fs, VsPos, Cols)
    end.

%% good_triangulation(Normal, Point1, Point2, Point3, Point4) -> true|false
%%  Return true if triangulation by connecting Point1 to Point3 is OK.
%%  The normal Normal should be averaged normal for the quad.
good_triangulation({Nx,Ny,Nz}, {Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz})
  when is_float(Ax), is_float(Ay), is_float(Az) ->
    CAx = Cx-Ax, CAy = Cy-Ay, CAz = Cz-Az,
    ABx = Ax-Bx, ABy = Ay-By, ABz = Az-Bz,
    DAx = Dx-Ax, DAy = Dy-Ay, DAz = Dz-Az,
    D1 = Nx*(CAy*ABz-CAz*ABy) + Ny*(CAz*ABx-CAx*ABz) + Nz*(CAx*ABy-CAy*ABx),
    D2 = Nx*(DAz*CAy-DAy*CAz) + Ny*(DAx*CAz-DAz*CAx) + Nz*(DAy*CAx-DAx*CAy),
    good_triangulation_1(D1, D2).

good_triangulation_1(D1, D2) when D1 > 0, D2 > 0 -> true;
good_triangulation_1(_, _) -> false.

%% force_flat_color(OriginalDlist, Color) -> NewDlist.
%%  Wrap a previous display list (that includes gl:color*() calls)
%%  into a new display lists that forces the flat color Color
%%  on all elements.
force_flat_color(Dl, RGB) ->
    force_flat_color(Dl, RGB, fun() -> ok end).

force_flat_color(OriginalDlist, {R,G,B}, DrawExtra) ->
    Dl = gl:genLists(1),
    gl:newList(Dl, ?GL_COMPILE),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_POLYGON_BIT bor ?GL_LINE_BIT bor
		  ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    DrawExtra(),
    gl:enable(?GL_LIGHTING),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHT0),
    gl:disable(?GL_LIGHT1),
    gl:disable(?GL_LIGHT2),
    gl:disable(?GL_LIGHT3),
    gl:disable(?GL_LIGHT4),
    gl:disable(?GL_LIGHT5),
    gl:disable(?GL_LIGHT6),
    gl:disable(?GL_LIGHT7),
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0,0,0,0}),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, {R,G,B,1}),
    wings_dl:call(OriginalDlist),
    gl:popAttrib(),
    gl:endList(),
    {call,Dl,OriginalDlist}.

smooth_plain_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] ->
	    wings__du:smooth_plain_face(Pos, VsInfo);
	{_,TriFs,Pos} ->
	    wings__du:smooth_plain_face(TriFs, Pos, VsInfo)
    end,
    smooth_plain_faces(Fs, Ns);
smooth_plain_faces([], _) -> ok.

smooth_uv_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] ->
	    wings__du:smooth_uv_face(Pos, VsInfo);
	{_,TriFs,Pos} ->
	    wings__du:smooth_uv_face(TriFs, Pos, VsInfo)
    end,
    smooth_uv_faces(Fs, Ns);
smooth_uv_faces([], _) -> ok.

smooth_vcol_faces([{Face,VsInfo}|Fs], Ns) ->
    case gb_trees:get(Face, Ns) of
	[_|Pos] ->
	    wings__du:smooth_vcol_face(Pos, VsInfo);
	{_,TriFs,Pos} ->
	    wings__du:smooth_vcol_face(TriFs, Pos, VsInfo)
    end,
    smooth_vcol_faces(Fs, Ns);
smooth_vcol_faces([], _) -> ok.

%% Draw a face without any lighting.
unlit_face(Face, #dlo{ns=Ns}) ->
    case gb_trees:get(Face, Ns) of
	[_|VsPos] -> wings__du:plain_face(VsPos);
	{_,Fs,VsPos} -> wings__du:plain_face(Fs, VsPos)
    end;
unlit_face(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    unlit_face(Face, Edge, We).

unlit_face(Face, Edge, We) ->
    Ps = wings_face:vertex_positions(Face, Edge, We),
    case wings_draw:face_ns_data(Ps) of
	[_|VsPos] -> wings__du:plain_face(VsPos);
	{_,Fs,VsPos} -> wings__du:plain_face(Fs, VsPos)
    end.
