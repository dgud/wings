%%
%%  wpc_ambocc.erl --
%%
%%     Plug-in for Generating Ambient Occlusion via OpenGL
%%
%%     Uses hardware-accelerated OpenGL calls, instead of ray-tracing, to
%%     calculate and set a per-vertex ambient-occlusion factor. A HemiCube is
%%     used to sample the environment's visibility. The results are stored in
%%     the vertex-colors and can also be baked to a texture through autouv.
%%
%%  CopyRight (c) 2008-2011 Anthony D'Agostino, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_ambocc).
%% Plugin entry points
-export([init/0,menu/2,command/2]).

%% Reused by ambocc_gl2.erl
-export([make_disp_list/1, get_up_right/1, render_hemicube/5,
	 get_ao_factor/1, ambient/0]).


-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").

init() ->
    true.

menu({tools}, Menu) ->
    Menu ++ [separator,{?__(1,"Ambient Occlusion"),ambient_occlusion,
			?__(2,"Add Ambient-Occlusion vertex colors via OpenGL")}];
menu(_, Menu) -> Menu.

command({tools,ambient_occlusion}, St0) ->
    St = case wings_gl:have_fbo() of
	     true ->
		 ambocc_gl2:ambient_occlusion(St0);
	     _ ->
		 ambient_occlusion(St0)
	 end,
    create_ambient_light(St);
command(_Cmd, _) -> next.

ambient_occlusion(St) ->
    StartTime = os:timestamp(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_gl(),
    {Vabs,DrawFun} = make_disp_list(St),
    #st{shapes=Shapes} = St,
    ProcessObject = fun(_, We) -> process_obj(We, DrawFun) end,
    Shapes2 = ?SLOW(gb_trees:map(ProcessObject, Shapes)),
    St2 = St#st{shapes=Shapes2},
    _ = [wings_draw_setup:delete_vab(Vab) || Vab <- Vabs],
    gl:popAttrib(),
    EndTime = os:timestamp(),
    Seconds = timer:now_diff(EndTime,StartTime)/1.0e6,
    VidCard = gl:getString(?GL_RENDERER),
    io:fwrite(?__(1,"OpenGL AmbOcc time: ~.1fs (~s)\n"), [Seconds,VidCard]),
    St2.

process_obj(We, _) when ?IS_NOT_VISIBLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_NOT_SELECTABLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_ANY_LIGHT(We) ->
    case We#we.name =/= ambient() of
	true -> We#we{perm=[]};
	false -> We
    end;
process_obj(We0, DrawFun) ->
    #we{es=Etab,vp=Vtab,name=Name} = We0,
    io:fwrite(?__(1,"Processing: ~s\n"), [Name]),
    GetColor =
	fun(Key,_Val) ->
		Eye = wings_vertex:pos(Key, We0) ,
		LookAt = wings_vertex:normal(Key, We0),
		get_ao_color(Eye, LookAt, DrawFun)
	end,
    VertexColors = array:sparse_map(GetColor, Vtab),
    SetColor =
	fun(Edge, #edge{vs=Va,ve=Vb}, W) ->
		Color1 = array:get(Va, VertexColors),
		Color2 = array:get(Vb, VertexColors),
		wings_va:set_edge_color(Edge, Color1, Color2, W)
	end,
    array:sparse_foldl(SetColor, We0, Etab).

make_disp_list(St) ->
    #st{shapes=Shapes} = St,
    Wes = gb_trees:values(Shapes),
    Vabs = [wings_draw_setup:we(We, [], St) ||
	       We <- Wes, is_plain_geometry(We)],
    Draw = fun() ->
		   _ = [draw_vab(Vab) || Vab <- Vabs],
		   ok
	   end,
    {Vabs,Draw}.

is_plain_geometry(#we{perm=P}=We) ->
    not (?IS_NOT_VISIBLE(P) orelse
	 ?IS_NOT_SELECTABLE(P) orelse
	 ?IS_ANY_LIGHT(We)).

draw_vab(Vab) ->
    wings_draw_setup:enable_pointers(Vab, []),
    Count = wings_draw_setup:face_vertex_count(Vab),
    gl:drawArrays(?GL_TRIANGLES, 0, Count),
    wings_draw_setup:disable_pointers(Vab, []).

get_ao_factor(Buffer) ->
    NumWhitePixels = num_white(Buffer, 0),
    Samples = 0.75*byte_size(Buffer),
    Misses = -0.25*byte_size(Buffer) + NumWhitePixels,
    Factor = Misses/Samples,
    Factor.

num_white(<<255:8,Rest/binary>>, Sum) ->
    num_white(Rest, Sum+1);
num_white(<<_:8,Rest/binary>>, Sum) ->
    num_white(Rest, Sum);
num_white(<<>>, Sum) ->
    Sum.

read_frame() ->
    Hemirez = 64, % Must be even and/or power-of-two
    W = H = Hemirez,
    Buffer = wings_io:get_buffer(W*H, ?GL_UNSIGNED_BYTE),
    gl:readPixels(0,0, W,H, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    get_ao_factor(ImageBin).

setup_gl() ->
    gl:clearColor(1.0,1.0,1.0,0.0),  % Sky Color
    gl:color4f(0.0,0.0,0.0,1.0),     % Obj Color
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_CULL_FACE).

get_ao_color(Eye, Lookat, DrawFun) ->
    gl:clear(?GL_COLOR_BUFFER_BIT),
    render_hemicube(0, 0, Eye, Lookat, DrawFun),
    Factor = read_frame(),
    {Factor,Factor,Factor}.

get_up_right(Lookat) ->
    Up1   = up(Lookat),
    Right = e3d_vec:cross(Up1, Lookat),
    Up    = e3d_vec:cross(Lookat, Right),
    {Up,Right}.

up({X,Y,Z})
  when abs(Y) > abs(X), abs(Y) > abs(Z) ->
    {1.0,0.0,0.0};
up(_) ->
    {0.0,1.0,0.0}.

render_hemicube(SX, SY, Eye, Lookat, DF) ->
    {Up,Right} = get_up_right(Lookat),
    Hemirez = 64, % Must be even and/or power-of-two
    P1 = trunc(Hemirez * 0.00),
    P2 = trunc(Hemirez * 0.25),
    P3 = trunc(Hemirez * 0.50),
    P4 = trunc(Hemirez * 0.75),
    W = P3, H = P2,
    EpL = e3d_vec:add(Eye, Lookat),
    EpR = e3d_vec:add(Eye, Right),
    EmR = e3d_vec:sub(Eye, Right),
    EmU = e3d_vec:sub(Eye, Up),
    EpU = e3d_vec:add(Eye, Up),
    LookatN = e3d_vec:neg(Lookat),
    render_view(Eye, EpL, Up,      [-1,1,-1,1], [SX+P2,SY+P2,W,W], DF), % Center
    render_view(Eye, EpR, Up,      [ 0,1,-1,1], [SX+P1,SY+P2,H,W], DF), % Right
    render_view(Eye, EmR, Up,      [-1,0,-1,1], [SX+P4,SY+P2,H,W], DF), % Left
    render_view(Eye, EmU, Lookat,  [-1,1, 0,1], [SX+P2,SY+P1,W,H], DF), % Down
    render_view(Eye, EpU, LookatN, [-1,1,-1,0], [SX+P2,SY+P4,W,H], DF). % Up

render_view(Eye, Lookat, Up, Frustum, Viewport, DrawFun) ->
    Near = 0.01,
    Far = 100.0,
    {Ex,Ey,Ez} = Eye,
    {Dx,Dy,Dz} = Lookat,
    {Ux,Uy,Uz} = Up,
    [L,R,B,T] = [I*Near || I <- Frustum],
    [X,Y,W,H] = Viewport,
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:frustum(L,R, B,T, Near,Far),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    glu:lookAt(Ex,Ey,Ez, Dx,Dy,Dz, Ux,Uy,Uz),
    gl:viewport(X,Y, W,H),
    DrawFun().

create_ambient_light(St) ->
    wings_pref:set_value(scene_lights, true),
    SceneLights = wings_light:export(St),
    case proplists:is_defined(ambient(), SceneLights) of
	true ->
	    St;
	false ->
	    White = {1.0,1.0,1.0,1.0},
	    Lights = [{ambient(),[{opengl,[{type,ambient},{ambient,White}]}]}],
	    wings_light:import(Lights,St)
    end.

ambient() ->
    ?__(1,"Ambient").
