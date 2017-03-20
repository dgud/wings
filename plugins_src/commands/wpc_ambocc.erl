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

-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

init() ->
    true.

menu({tools}, Menu) ->
    Menu ++ [separator,{?__(1,"Ambient Occlusion"),ambient_occlusion,
			?__(2,"Add Ambient-Occlusion vertex colors via OpenGL")}];
menu(_, Menu) -> Menu.

command({tools,ambient_occlusion}, St0) ->
    St = ambient_occlusion(St0),
    create_ambient_light(St);
command(_Cmd, _) -> next.

-record(ao, {df, fbo, tex, cleanup_fbo, buf}).
-define(TEX_SZ, 1024).
-define(SAMPLE_SZ, 64).
-define(NUM_SAMPLES, (?TEX_SZ div ?SAMPLE_SZ)).

ambient_occlusion(St) ->
    StartTime = os:timestamp(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_gl(),
    AO_0 = setup_shaders(),
    DrawFun = make_disp_list(St),
    AO = AO_0#ao{df=DrawFun},
    #st{shapes=Shapes} = St,
    ProcessObject = fun(_,We) -> process_obj(We, AO) end,
    Shapes2 = ?SLOW(gb_trees:map(ProcessObject, Shapes)),
    St2 = St#st{shapes=Shapes2},
    cleanup(AO),
    gl:popAttrib(),
    EndTime = os:timestamp(),
    Seconds = timer:now_diff(EndTime,StartTime)/1.0e6,
    VidCard = gl:getString(?GL_RENDERER),
    io:fwrite(?__(1,"OpenGL AmbOcc GL2 time: ~.1fs (~s)\n"), [Seconds,VidCard]),
    St2.

setup_gl() ->
    gl:clearColor(1.0,1.0,1.0,0.0),  % Sky Color
    gl:color4f(0.0,0.0,0.0,1.0),     % Obj Color
    gl:shadeModel(?GL_FLAT),
    %% gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_CULL_FACE).

setup_shaders() ->
    Buffers = wings_gl:setup_fbo({?TEX_SZ,?TEX_SZ},
				 [{color,
				   [{wrap_s,   ?GL_CLAMP_TO_EDGE},
				    {wrap_t,   ?GL_CLAMP_TO_EDGE}]},
				  {depth,[]}]),
    [{fbo,Fbo},{color,Tex}|_] = Buffers,
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
    #ao{fbo=Fbo, tex=Tex, cleanup_fbo=Buffers,
	buf = wings_io:get_buffer(?TEX_SZ*?TEX_SZ, ?GL_UNSIGNED_BYTE)}.

cleanup(#ao{cleanup_fbo=Fbo}) ->
    wings_gl:delete_fbo(Fbo).

process_obj(We, _) when ?IS_NOT_VISIBLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_NOT_SELECTABLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_ANY_LIGHT(We) ->
    case We#we.name =/= ambient() of
	true -> We#we{perm=?PERM_HIDDEN_BIT};
	false -> We
    end;
process_obj(We0, AO) ->
    #we{es=Etab,vp=Vtab,name=Name} = We0,
    io:fwrite(?__(1,"Processing: ~s\n"), [Name]),
    gl:clear(?GL_COLOR_BUFFER_BIT  bor ?GL_DEPTH_BUFFER_BIT),
    VertexColors = calc_ao(array:sparse_to_orddict(Vtab), We0, AO, []),
    SetColor = fun(Edge, #edge{vs=Va,ve=Vb}, W) ->
		       Color1 = array:get(Va, VertexColors),
		       Color2 = array:get(Vb, VertexColors),
		       wings_va:set_edge_color(Edge, Color1, Color2, W)
	       end,
    array:sparse_foldl(SetColor, We0, Etab).

calc_ao([], _We, _AO, Vc) ->
    array:from_orddict(lists:reverse(Vc));
calc_ao(VList, We, AO, Vc0) ->
    {Batch, Rest} =
	try lists:split(?NUM_SAMPLES*?NUM_SAMPLES, VList)
	catch _:_ ->
		{VList, []}
	end,
    (render_hemisphere(0, 0, Batch, We, AO)),
    Bin = (read_frame(AO)),
    Vc = (get_ao_factors(Batch, Bin, Vc0)),
    calc_ao(Rest, We, AO, Vc).

render_hemisphere(X, Y, [{Vertex,Eye}|Rest], We, #ao{df=DrawFun}=AO)
  when X < ?NUM_SAMPLES ->
    LookAt = wings_vertex:normal(Vertex,We),
    render_hemicube(X*?SAMPLE_SZ, Y*?SAMPLE_SZ,
		    Eye, LookAt, DrawFun),
    render_hemisphere(X+1,Y, Rest, We, AO);
render_hemisphere(_,_,[], _, _) -> ok;
render_hemisphere(_,Y,Vs,We,AO) ->
    render_hemisphere(0,Y+1,Vs,We,AO).

read_frame(#ao{tex=Tex, fbo=Fbo, buf=Buffer}) ->
    wings_gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, 0),
    gl:bindTexture(?GL_TEXTURE_2D, Tex),
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    wings_gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, Fbo),
    gl:clear(?GL_COLOR_BUFFER_BIT  bor ?GL_DEPTH_BUFFER_BIT),
    %% test_img("Test", ImageBin),
    ImageBin.

%% Could be optimized...
get_ao_factors(Batch, Image, Vc0) ->
    Images = split_image(0, 0, Image, []),
    get_ao_factor(Batch, Images, Vc0).

split_image(Col, Row, Image0, All) when Col < ?NUM_SAMPLES ->
    Start = (?SAMPLE_SZ*Col) + (Row*?TEX_SZ*?SAMPLE_SZ),
    <<_:Start/binary, Image/binary>> = Image0,
    Sample = split_image2(0, Image, <<>>),
    split_image(Col+1, Row, Image0, [Sample|All]);
split_image(_, Row, Image, All) when Row < (?NUM_SAMPLES-1) ->
    split_image(0, Row+1, Image, All);
split_image(_, _, _, All) ->
    lists:reverse(All).

split_image2(Row, Image, Sample0)
  when Row < ?SAMPLE_SZ ->
    Skip = ?TEX_SZ-?SAMPLE_SZ,
    case Image of
	<<Sample:?SAMPLE_SZ/binary, _:Skip/binary,Rest/binary>> ->
	    split_image2(Row+1, Rest, <<Sample0/binary, Sample/binary>>);
	<<Sample:?SAMPLE_SZ/binary, _/binary>> ->
	    <<Sample0/binary, Sample/binary>>
    end;
split_image2(_, _, Sample) ->
    Sample.

get_ao_factor([{Vx, _}|Vs], [Buffer|Is], Vc0) ->
    AO = get_ao_factor(Buffer),
    get_ao_factor(Vs, Is, [{Vx,{AO,AO,AO}}|Vc0]);
get_ao_factor([], _Unused, Vc) ->
    Vc.

%%
%%   Code for taking a look at the fisheye renders, keep it.
%%
%% test_img(Name, Bin) ->
%%     {_,S,M} = now(),
%%     Str = "_" ++ integer_to_list(S) ++ "_" ++ integer_to_list(M),
%%     RGB = << <<G:8,G:8,G:8>> || <<G:8>> <= Bin >>,
%%     Envelope = #e3d_image{image=RGB, width=?TEX_SZ, height=?TEX_SZ},
%%     wings_image:new_temp(Name ++ Str, Envelope).
%%

make_disp_list(St) ->
    #st{shapes=Shapes} = St,
    Wes = gb_trees:values(Shapes),
    Vabs = [wings_draw_setup:we(We, [], St) ||
	       We <- Wes, is_plain_geometry(We)],
    Draw = fun() ->
		   _ = [draw_vab(Vab) || Vab <- Vabs],
		   ok
	   end,
    Draw.

is_plain_geometry(#we{perm=P}=We) ->
    not (?IS_NOT_VISIBLE(P) orelse
	 ?IS_NOT_SELECTABLE(P) orelse
	 ?IS_ANY_LIGHT(We)).

draw_vab(Vab) ->
    RS = wings_draw_setup:enable_pointers(Vab, [], #{}),
    Count = wings_draw_setup:face_vertex_count(Vab),
    gl:drawArrays(?GL_TRIANGLES, 0, Count),
    wings_draw_setup:disable_pointers(Vab, RS).

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
    case wings_pref:get_value(scene_lights, true) of
        true -> ok;
        false -> wings_view:command(scene_lights, St)
    end,
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
