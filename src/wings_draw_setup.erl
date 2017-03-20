%%
%%  wings_draw_setup.erl --
%%
%%     Setup and Create data binaries for drawing
%%
%%  Copyright (c) 2010-2011 Dan Gudmundsson & Björn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_draw_setup).

-export([we/3]).  %% For plugins

-export([work/2,smooth/2,prepare/3,prepare/4,flat_faces/2]).
-export([enable_pointers/3,disable_pointers/2]).
-export([face_vertex_count/1,has_active_color/1]).

%% Used by wings_proxy.
-export([create_vab/4,create_tangent_vab/5,
	 add_ts/5,add_tangents/3]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,sort/1,foldl/3]).

%%%
%%% we(We, [Option], St) -> #vab{} See wings.hrl
%%%    Generates rendering buffers from a we,
%%%    reuses data if available.
%%% Options are:
%%%    {smooth, true|false}                            default false
%%%    {subdiv, Level :: integer()}                    default 0 
%%%    {attribs, undefined|plain|uv|color|color_uv}    default undefined 
%%%              undefined -> you get what is available and enabled in wings_prefs
%%%              plain ->  only vertex positions and normal
we(We, Options, St) ->
    wings_dl:fold(fun(Dl, undefined) -> we_1(Dl, We, Options, St);
		     (_Dl, Res)      -> Res
		  end, undefined).

we_1(Dlo=#dlo{src_we=Orig=#we{id=Id}}, Curr=#we{id=Id}, Opt, St) ->
    case Orig =:= Curr of
	true  -> we_2(Dlo, Opt, St);
	false -> we_2(wings_draw:changed_we(Dlo,#dlo{src_we=Curr}), Opt, St)
    end;
we_1(_, _, _, _) ->
    undefined.

we_2(Dlo0, Opt, St) ->
    Smooth = proplists:get_value(smooth, Opt, false),
    Attrib = proplists:get_value(attribs, Opt, undefined),
    Dlo1 = setup_vmirror(proplists:get_value(vmirror, Opt, undefined), Dlo0),
    Dlo2 = setup_subdiv(proplists:get_value(subdiv, Opt, 0), Dlo1),
    Dlo = check_attrib(Attrib, Dlo2),
    #dlo{vab=Vab} =
	case Smooth of
	    true  -> smooth(Dlo, St, Attrib);
	    false -> work(Dlo, St, Attrib)
	end,
    Vab.

setup_vmirror(undefined, Dlo) -> Dlo;
setup_vmirror(_, Dlo=#dlo{src_we=#we{mirror=none}}) -> Dlo;
setup_vmirror(_, #dlo{src_we=We}) ->
    Mirrored = wings_we:freeze_mirror(We),
    wings_draw:changed_we(#dlo{}, #dlo{src_we=Mirrored}).

setup_subdiv(0, Dlo) -> Dlo;
setup_subdiv(N, #dlo{src_we=We}) ->
    SubDived = sub_divide(N, We),
    wings_draw:changed_we(#dlo{}, #dlo{src_we=SubDived}).
sub_divide(0, We) -> We;
sub_divide(N, We) ->
    sub_divide(N-1, wings_subdiv:smooth(We)).

check_attrib(undefined, Dlo) -> Dlo;     
check_attrib(_, Dlo = #dlo{vab=none}) -> Dlo;
check_attrib(uv, Dlo = #dlo{vab = #vab{face_uv={_,_}, face_vc=none}}) -> Dlo;
check_attrib(color, Dlo = #dlo{vab = #vab{face_vc={_,_}, face_uv=none}}) -> Dlo;
check_attrib(color_uv, Dlo = #dlo{vab = #vab{face_vc={_,_}, face_uv={_,_}}}) -> Dlo;
check_attrib(_, D) -> 
    D#dlo{vab=none}. %% Force rebuild

%%%
%%% Help functions to activate and disable buffer pointers.
%%%

has_active_color(#vab{face_vc=Color}) ->
    Color =/= none.

%% enable_pointers(#vab{}, [ExtraPointer], RS) ->
%%    ExtraPointer = face_normals | vertex_normals | colors | uvs | tangents
%%  Enable the vertex buffer pointer, and optionally other pointers.

enable_pointers(#vab{id=Vbo,face_vs={Stride,BinVs}}=Vab, Extra, RS0) ->
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:vertexPointer(3, ?GL_FLOAT, Stride, BinVs),
    CS = foldl(fun(What,Acc) ->
                       case enable_pointer(What, Vab) of
                           ok -> Acc;
                           State -> [State|Acc]
                       end
               end, [?GL_VERTEX_ARRAY], Extra),
    OldCs = maps:get({vbo,Vbo}, RS0, []),
    %io:format("Enable: ~p => ~p~n", [CS, CS -- OldCs]),
    [enable_state(State)  || State <- CS],
    %io:format("Disable: ~p => ~p~n", [OldCs, OldCs -- CS]),
    [disable_state(State) || State <- OldCs -- CS],
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    RS0#{{vbo,Vbo}=>CS}.

%% disable_pointers(RS)
%%  Disable the active pointers

disable_pointers(#vab{id=Vbo}, RS0) ->
    OldCs = maps:get({vbo, Vbo}, RS0, []),
    [disable_state(What) || What <- OldCs],
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    RS0#{{vbo, Vbo}=>[]}.

enable_pointer(face_normals, #vab{face_fn={Stride,Ns}}) ->
    gl:normalPointer(?GL_FLOAT, Stride, Ns),
    ?GL_NORMAL_ARRAY;
enable_pointer(vertex_normals, #vab{id=MainVbo,face_sn={vbo,Vbo}}) ->
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:normalPointer(?GL_FLOAT, 0, 0),
    enable_state(?GL_NORMAL_ARRAY),
    gl:bindBuffer(?GL_ARRAY_BUFFER, MainVbo),
    ok;
enable_pointer(vertex_normals, #vab{face_sn={Stride,Ns}}) ->
    %% Only used by wings_cc.
    gl:normalPointer(?GL_FLOAT, Stride, Ns),
    ?GL_NORMAL_ARRAY;
enable_pointer(colors, #vab{face_vc=FaceCol}) ->
    case FaceCol of
	none ->
	    ok;
	{Stride,Color} ->
	    gl:colorPointer(3, ?GL_FLOAT, Stride, Color),
	    ?GL_COLOR_ARRAY
    end;
enable_pointer(uvs, #vab{face_uv=FaceUV}) ->
    case FaceUV of
	none ->
	    ok;
	{Stride,UV} ->
	    gl:texCoordPointer(2, ?GL_FLOAT, Stride, UV),
	    ?GL_TEXTURE_COORD_ARRAY
    end;
enable_pointer(tangents, #vab{face_ts=FaceTs}) ->
    case FaceTs of
	none ->
	    ok;
	{Stride,Ts} ->
	    gl:vertexAttribPointer(?TANGENT_ATTR, 4, ?GL_FLOAT,
				   ?GL_FALSE, Stride, Ts),
	    {attrib, ?TANGENT_ATTR}
    end.

enable_state({attrib, Attr}) ->
    gl:enableVertexAttribArray(Attr);
enable_state(Attr) ->
    gl:enableClientState(Attr).

disable_state({attrib, Attr}) ->
    gl:disableVertexAttribArray(Attr);
disable_state(Attr) ->
    gl:disableClientState(Attr).

face_vertex_count(#dlo{vab=#vab{mat_map=[{_Mat,_Type,Start,Count}|_]}}) ->
    Start+Count;
face_vertex_count(#vab{mat_map=[{_Mat,_Type,Start,Count}|_]}) ->
    Start+Count.

%% Setup face_vs and face_fn and additional uv coords or vertex colors
work(Dlo, St) ->
    work(Dlo, St, undefined).

work(#dlo{ns={_}}=D0, St, Attr) ->
    D = wings_draw:update_normals(D0),
    work(D, St, Attr);
work(#dlo{vab=none,src_we=#we{fs=Ftab}}=D, St, Attr) ->
    Prepared = prepare(gb_trees:to_list(Ftab), D, St, Attr),
    flat_faces(Prepared, D);
work(#dlo{vab=#vab{face_vs=none},src_we=#we{fs=Ftab}}=D, St, Attr) ->
    Prepared = prepare(gb_trees:to_list(Ftab), D, St, Attr),
    flat_faces(Prepared, D);
work(#dlo{vab=#vab{face_fn=none}=Vab}=D, St, Attr) ->
    %% Can this really happen? If it can, it happens infrequently,
    %% so we don't have to handle it efficiently.
    work(D#dlo{vab=Vab#vab{face_vs=none}}, St, Attr);
work(D, _, _) -> D.

%% Setup face_vs and face_sn and additional uv coords or vertex colors
smooth(Dlo, St) ->
    smooth(Dlo, St, undefined).

smooth(#dlo{ns={_}}=D0, St, Attr) ->
    D = wings_draw:update_normals(D0),
    smooth(D, St, Attr);
smooth(#dlo{vab=none}=D, St, Attr) ->
    setup_smooth_normals(work(D, St, Attr));
smooth(#dlo{vab=#vab{face_vs=none}}=D, St, Attr) ->
    setup_smooth_normals(work(D, St, Attr));
smooth(D=#dlo{vab=#vab{face_sn=none}}, _St, _) ->
    setup_smooth_normals(D);
smooth(D, _, _) -> D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flat_faces({plain,MatFaces}, D) ->
    plain_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({uv,MatFaces}, D) ->
    uv_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({uv_tangent,MatFaces}, D) ->
    Z = e3d_vec:zero(),
    Array = array:new([{default, {Z,Z}}]),
    tangent_flat_faces(MatFaces, D, 0, <<>>, [], [], {Array, []}); 
flat_faces({color,MatFaces}, D) ->
    col_flat_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({color_uv,MatFaces}, D) ->
    col_uv_faces(MatFaces, D, 0, <<>>, [], []);
flat_faces({color_uv_tangent,MatFaces}, D) ->
    Z = e3d_vec:zero(),
    Array = array:new([{default, {Z,Z}}]),
    col_tangent_faces(MatFaces, D, 0, <<>>, [], [], {Array, []}).

plain_flat_faces([{Mat,Fs}|T], #dlo{ns=Ns}=D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = flat_faces_1(Fs, Ns, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    plain_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
plain_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    Vab = create_vab([vertices,face_normals], Vs, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

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
    Vab = create_vab([vertices,face_normals,uvs], Vs, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

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

%% Also needs uv's
tangent_flat_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0, Ts0) ->
    {Start,Vs,FaceMap,Ts} = tangent_flat_faces_1(Fs, D, Start0, Vs0, Fmap0, Ts0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    tangent_flat_faces(T, D, Start, Vs, FaceMap, MatInfo, Ts);
tangent_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo, {VsTs0, RevF2V}) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    VsTs = array:map(fun(_V, {T,BT}) ->
			     {e3d_vec:norm(T),e3d_vec:norm(BT)}
		     end, VsTs0),
    Data = add_tangents(lists:reverse(RevF2V), VsTs, Vs),
    What = [vertices,face_normals,uvs],
    Vab = create_tangent_vab(What, Vs, Data, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

tangent_flat_faces_1([{Face,Edge}|Fs], #dlo{ns=Ns,src_we=We}=D, Start, Vs, FaceMap, Ts0) ->
    UVs = wings_va:face_attr(uv, Face, Edge, We),
    case array:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    tangent_flat_faces_1(Fs, D, Start+3,
				 add_tri(Vs, Normal, Pos, UVs),
				 [{Face,{Start,3}}|FaceMap],
				 add_ts(Pos, UVs, Normal,
					wings_face:vertices_ccw(Face, We), Ts0)
				);
	[Normal|Pos] ->
	    tangent_flat_faces_1(Fs, D, Start+6,
				 add_quad(Vs, Normal, Pos, UVs),
				 [{Face,{Start,6}}|FaceMap],
				 add_ts(Pos, UVs, Normal,
					wings_face:vertices_ccw(Face, We), Ts0));
	Info = {Normal,Faces,VsPos} ->
	    NoVs  = length(Faces) * 3,
	    VsBin = add_poly(Vs, Normal, Faces,
			     list_to_tuple(VsPos), list_to_tuple(UVs)),
	    tangent_flat_faces_1(Fs, D, NoVs+Start,
				 VsBin, [{Face,{Start,NoVs}}|FaceMap],
				 add_ts(Info, UVs, Normal,
					wings_face:vertices_ccw(Face, We), Ts0))
    end;
tangent_flat_faces_1([], _, Start, Vs, FaceMap, Ts) ->
    {Start,Vs,FaceMap,Ts}.


col_flat_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0) ->
    {Start,Vs,FaceMap} = col_flat_faces_1(Fs, D, Start0, Vs0, Fmap0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    col_flat_faces(T, D, Start, Vs, FaceMap, MatInfo);
col_flat_faces([], D, _Start, Vs, FaceMap0, MatInfo) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    Vab = create_vab([vertices,face_normals,colors], Vs, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

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
    Vab = create_vab([vertices,face_normals,colors,uvs],
		     Vs, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

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

%% Also needs uv's
col_tangent_faces([{Mat,Fs}|T], D, Start0, Vs0, Fmap0, MatInfo0, Ts0) ->
    {Start,Vs,FaceMap,Ts} = col_tangent_faces_1(Fs, D, Start0, Vs0, Fmap0, Ts0),
    MatInfo = [{Mat,?GL_TRIANGLES,Start0,Start-Start0}|MatInfo0],
    col_tangent_faces(T, D, Start, Vs, FaceMap, MatInfo, Ts);
col_tangent_faces([], D, _Start, Vs, FaceMap0, MatInfo, {VsTs0, RevF2V}) ->
    FaceMap = array:from_orddict(sort(FaceMap0)),
    VsTs = array:map(fun(_V, {T,BT}) ->
			     {e3d_vec:norm(T),e3d_vec:norm(BT)}
		     end, VsTs0),
    Data = add_tangents(lists:reverse(RevF2V), VsTs, Vs),
    What = [vertices,face_normals,colors,uvs],
    Vab = create_tangent_vab(What, Vs, Data, FaceMap, MatInfo),
    D#dlo{vab=Vab}.

col_tangent_faces_1([{Face,Edge}|Fs], #dlo{ns=Ns,src_we=We}=D, Start, Vs, FaceMap, Ts0) ->
    UVs = wings_va:face_attr([color|uv], Face, Edge, We),
    case array:get(Face, Ns) of
	[Normal|Pos =[_,_,_]] ->
	    col_tangent_faces_1(Fs, D, Start+3,
				add_col_uv_tri(Vs, Normal, Pos, UVs),
				[{Face,{Start,3}}|FaceMap],
				add_ts(Pos, [UV|| [_|UV] <- UVs], Normal,
				       wings_face:vertices_ccw(Face, We), Ts0));
	[Normal|Pos] ->
	    col_tangent_faces_1(Fs, D, Start+6,
				add_col_uv_quad(Vs, Normal, Pos, UVs),
				[{Face,{Start,6}}|FaceMap],
				add_ts(Pos, [UV|| [_|UV] <- UVs], Normal,
				       wings_face:vertices_ccw(Face, We), Ts0));
	Info = {Normal,Faces,VsPos} ->
	    NoVs  = length(Faces) * 3,
	    VsBin = add_col_uv_poly(Vs, Normal, Faces,
				    list_to_tuple(VsPos), list_to_tuple(UVs)),
	    col_tangent_faces_1(Fs, D, NoVs+Start,
				VsBin, [{Face,{Start,NoVs}}|FaceMap],
				add_ts(Info, [UV|| [_|UV] <- UVs], Normal,
				       wings_face:vertices_ccw(Face, We), Ts0))
    end;
col_tangent_faces_1([], _, Start, Vs, FaceMap,Ts) ->
    {Start,Vs,FaceMap,Ts}.


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
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(SN), SN, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    D#dlo{vab=Vab#vab{face_sn={vbo,Vbo}}}.

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

add_ts([P1,P2,P3], [{U1,V1},{U2,V2},{U3,V3}], N, Vs, {Ts,F2V}) ->
    {X1,Y1,Z1} = e3d_vec:sub(P2, P1),
    {X2,Y2,Z2} = e3d_vec:sub(P3, P1),
    S1 = U2-U1,
    S2 = U3-U1,
    T1 = V2-V1,
    T2 = V3-V1,
    try 
	F = 1.0 / (S1*T2 - S2*T1),
	Tangent = {F*(T2*X1-T1*X2), F*(T2*Y1-T1*Y2), F*(T2*Z1-T1*Z2)},
	BiTangent = {F*(S1*X2-S2*X1), F*(S1*Y2-S2*Y1), F*(S1*Z2-S2*Z1)},
	H = case e3d_vec:dot(e3d_vec:cross(N, Tangent), BiTangent) < 0.0 of
		true  -> 1;
		false -> -1
	    end,
	{add_tangent(Vs, Tangent, BiTangent, Ts), [{N,H,Vs}|F2V]}
    catch _:badarith ->
	    {Ts, [{N,0,Vs}|F2V]}
    end;
add_ts([P1,P2,P3,P4], [UV1,UV2,UV3,UV4], N, [V1,V2,V3,V4], Ts) ->  % Quads
    add_ts([P3,P4,P1],[UV3,UV4,UV1], N, [V3,V4,V1],
	   add_ts([P1,P2,P3],[UV1,UV2,UV3], N, [V1,V2,V3], Ts));
add_ts({_N,Fs,VsPos}, UVs, N, Vs, Ts) -> %% Polys
    add_ts2(Fs, list_to_tuple(VsPos), list_to_tuple(UVs), N, list_to_tuple(Vs), Ts);
add_ts([_,_,_], _, N, Vs, {Ts,F2V}) -> %% Bad UVs ignore
    {Ts, [{N,1,Vs}|F2V]}.

add_ts2([{V1,V2,V3}|Fs], VsPos, UVs, N, Vs, Ts0) ->
    Ts = add_ts([element(V1,VsPos),element(V2,VsPos), element(V3,VsPos)],
		[uv_element(V1, VsPos, UVs),uv_element(V2, VsPos, UVs),uv_element(V3, VsPos, UVs)],
		N,
		[id_element(V1, VsPos, Vs), id_element(V2, VsPos, Vs), id_element(V3, VsPos, Vs)],
		Ts0),
    add_ts2(Fs, VsPos, UVs, N, Vs, Ts);
add_ts2([], _, _, _, _, Ts) -> Ts.

id_element(A, _Vtab, Ids) when A =< tuple_size(Ids) -> element(A, Ids);
id_element(A, Vtab, Ids) ->
    find_element(tuple_size(Ids), element(A, Vtab), Vtab, Ids, element(1, Ids)).

add_tangent([V|Vs], Tangent, BiTangent, Ts) ->
    {T0, B0} = array:get(V,Ts),
    add_tangent(Vs, Tangent, BiTangent, 
		array:set(V, {e3d_vec:add(T0, Tangent), e3d_vec:add(B0, BiTangent)}, Ts));
add_tangent([], _, _, Ts) -> Ts.

add_tangents([{N, H, Face}|Fs], Ts, Bin0) ->
    Bin = add_tangents1(Face, Ts, H, N, undefined, Bin0),
    add_tangents(Fs, Ts, Bin);
add_tangents([], _, Bin) -> Bin.

add_tangents1([V|Vs], Ts, H0, N, Prev, Bin0) ->
    case array:get(V, Ts) of
	{{0.0, 0.0, 0.0}, BiT} ->
	    {Tan = {X,Y,Z}, H} = get_tangent(Prev, BiT, H0, N),
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H:?F32>>,
	    add_tangents1(Vs, Ts, H, N, Tan, Bin);
	{Tan = {X,Y,Z}, _} when H0 /= 0 ->
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H0:?F32>>,
	    add_tangents1(Vs, Ts, H0, N, Tan, Bin);
	{Tan = {X,Y,Z}, BiT} ->
	    H = case e3d_vec:dot(e3d_vec:cross(N, Tan), BiT) < 0.0 of
		    true  -> 1;
		    false -> -1
		end,
	    Bin = <<Bin0/binary, X:?F32,Y:?F32,Z:?F32, H:?F32>>,
	    add_tangents1(Vs, Ts, H, N, Tan, Bin)
    end;
add_tangents1([], _, _, _, _, Bin) -> Bin.

get_tangent(undefined, {0.0,0.0,0.0}, H0, N) ->
    H = if H0 =:= 0 -> -1; true -> H0 end,
    {cross_axis(N), H};
get_tangent(undefined, BiT, 0, N) ->
    T = e3d_vec:cross(BiT, N),
    H = case e3d_vec:dot(e3d_vec:cross(N, T), BiT) < 0.0 of
	    true  -> 1;
	    false -> -1
	end,
    {T, H};
get_tangent(undefined, BiT, H, N) ->
    {e3d_vec:cross(BiT, N), H};
get_tangent(Prev, _, H, _) ->
    {Prev, H}.

cross_axis(N = {NX,NY,NZ}) ->
    try 
	V2 = case abs(NX) > abs(NY) of
		 true ->
		     ILen = 1.0 / math:sqrt(NX*NX+NZ*NZ),
		     {-NZ*ILen, 0.0, NX * ILen};
		 false ->
		     ILen = 1.0 / math:sqrt(NY*NY+NZ*NZ),
		     {0.0, NZ*ILen, -NY * ILen}
	     end,
	e3d_vec:cross(N, V2)
    catch _:badarith ->
	    {1.0, 0.0,0.0}
    end.

%%%
%%% Collect information about faces.
%%%
prepare(Ftab, Dlo, St) ->
    prepare(Ftab, Dlo, St, undefined).

prepare(Ftab, #dlo{src_we=We}, St, Attr) ->
    prepare(Ftab, We, St, Attr);
prepare(Ftab0, #we{}=We, St, Attr) ->
    Ftab = wings_we:visible(Ftab0, We),
    prepare_1(Ftab, We, St, Attr).

prepare_1(Ftab, We, St, Attr) ->
    MatFaces = mat_faces(Ftab, We),
    case wings_va:any_attributes(We) of
	false when Attr =:= undefined ->
	    %% Since there are no vertex attributes,
	    %% we don't need to look at the materials
	    %% to figure out what to do.
	    {plain,MatFaces};
	false  ->
	    {Attr, MatFaces};
	true ->
	    %% There are UV coordinates and/or vertex colors,
	    %% so we will have to look at the materials to
	    %% figure out what we'll need.
	    Attrs = wings_material:needed_attributes(We, St),
	    {prepare_2(Attr, Attrs),MatFaces}
    end.

prepare_2(Attr, _)
  when Attr == plain; Attr == color;
       Attr == uv; Attr == color_uv ->
    Attr;
prepare_2(_, []) ->
    plain;
prepare_2(_, Attrs) ->
    prepare_3(Attrs, plain).

prepare_3([color|Rest], Prev) ->
    case wings_pref:get_value(show_colors) of
	true -> prepare_3(Rest, color);
	false -> prepare_3(Rest, Prev)
    end;
prepare_3([uv, tangent], Prev) ->
    case wings_pref:get_value(show_normal_maps) of
	true -> prepare_4(uv_tangent, Prev);
	false -> prepare_3([uv], Prev)
    end;
prepare_3([uv], Prev) ->
    case wings_pref:get_value(show_textures) of
	true -> prepare_4(uv, Prev);
	false -> Prev
    end;
prepare_3([], Attr) -> Attr.

prepare_4(Attr, plain) -> Attr;
prepare_4(uv, color) -> color_uv;
prepare_4(uv_tangent, color) -> color_uv_tangent.

mat_faces(Ftab, We) ->
    case wings_pref:get_value(show_materials) of
	false ->
	    [{default,Ftab}];
	true ->
	    wings_facemat:mat_faces(Ftab, We)
    end.

%% create_tangent_vab(What, VsData, AllData, FaceMap, MatInfo)
%%  Create a #vab{} record with tangent data.

create_tangent_vab(What, VsData, AllData, FaceMap, MatInfo) ->
    Vab = create_vab(What, AllData, FaceMap, MatInfo),
    VsSize = byte_size(VsData),
    Vab#vab{data=VsData,face_ts={16,VsSize}}.

%%%
%%% Create a #vab{} record.
%%%

-type vab_item_tag() ::
	'vertices' |
	'face_normals' |
	'colors' |
	'uvs'.

-spec create_vab([vab_item_tag()], binary(), any(), any()) -> #vab{}.

create_vab(What, <<>>, FaceMap, MatInfo) ->
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, 0, <<>>, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    Empty = <<>>,
    Vab = #vab{id=Vbo,data=Empty,face_map=FaceMap,mat_map=MatInfo},
    foldl(fun(E, Vab0) ->
		  set_vab_item(E, {0,0}, Vab0)
	  end, Vab, What);
create_vab(What, Data, FaceMap, MatInfo) ->
    Stride = lists:foldl(fun(Item, Sum) ->
				 Sum + width(Item)
			 end, 0, What),
    [Vbo] = gl:genBuffers(1),
    gl:bindBuffer(?GL_ARRAY_BUFFER, Vbo),
    gl:bufferData(?GL_ARRAY_BUFFER, byte_size(Data), Data, ?GL_STATIC_DRAW),
    gl:bindBuffer(?GL_ARRAY_BUFFER, 0),
    Vab = #vab{id=Vbo,data=Data,face_map=FaceMap,mat_map=MatInfo},
    create_vab_1(What, 0, Stride, Data, Vab).

create_vab_1([H|T], Pos, Stride, Data0, Vab0) ->
    Item = {Stride,Pos},
    Vab = set_vab_item(H, Item, Vab0),
    create_vab_1(T, Pos+width(H), Stride, Data0, Vab);
create_vab_1([], _, _, _, Vab) -> Vab.

set_vab_item(vertices, Item, Vab) ->
    Vab#vab{face_vs=Item};
set_vab_item(face_normals, Item, Vab) ->
    Vab#vab{face_fn=Item};
set_vab_item(colors, Item, Vab) ->
    Vab#vab{face_vc=Item};
set_vab_item(uvs, Item, Vab) ->
    Vab#vab{face_uv=Item}.

width(vertices) -> 3*4;
width(face_normals) -> 3*4;
width(vertex_normals) -> 3*4;
width(uvs) -> 2*4;
width(colors) -> 3*4.
