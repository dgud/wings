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
%%     This module improves performance with newer cards requires shading
%%     and fbo.
%%
%%  Copyright (c) 2009 Anthony D'Agostino, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(ambocc_gl2).

-export([ambient_occlusion/1]).

-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include("e3d_image.hrl").

-record(ao, {dl, sp, fbo, tex, cleanup_fbo, buf}).
-define(TEX_SZ, 1024).
-define(SAMPLE_SZ, 64).
-define(NUM_SAMPLES, (?TEX_SZ div ?SAMPLE_SZ)).
-define(RADIE, (?SAMPLE_SZ div 2)).
-define(AREA,  (?SAMPLE_SZ*?SAMPLE_SZ)).
-define(FISH_EYE_AREA, (3.141592653589793 * ?RADIE * ?RADIE)).

ambient_occlusion(St) ->
    StartTime = now(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    setup_gl(),
    AO_0 = setup_shaders(),
    DispList = wpc_ambocc:make_disp_list(St),
    AO = AO_0#ao{dl=DispList},
    #st{shapes=Shapes} = St,
    ProcessObject = fun(_,We) -> process_obj(We,AO) end,
    Shapes2 = ?SLOW(gb_trees:map(ProcessObject, Shapes)),
    St2 = St#st{shapes=Shapes2},
    cleanup(AO),
    gl:popAttrib(),
    EndTime = now(),
    Seconds = timer:now_diff(EndTime,StartTime)/1.0e6,
    VidCard = gl:getString(?GL_RENDERER),
    io:fwrite("OpenGL AmbOcc GL2 time: ~.1fs (~s)\n", [Seconds,VidCard]),
    St3 = create_ambient_light(St2),
    St3.

setup_gl() ->
    gl:clearColor(1,1,1,0),  % Sky Color
    gl:color4f(0,0,0,1),     % Obj Color
    gl:shadeModel(?GL_FLAT),
    %% gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_CULL_FACE).

setup_shaders() ->
    VS   = wings_gl:compile(vertex, fisheye_vs()),
    FS   = wings_gl:compile(fragment, fisheye_fs()),
    Prog = wings_gl:link_prog([VS,FS]),
    gl:useProgram(Prog),
    Buffers = wings_gl:setup_fbo({?TEX_SZ,?TEX_SZ},
				 [{color,[%{internal, ?GL_LUMINANCE8},
					  %{format,   ?GL_LUMINANCE},
					  {wrap_s,   ?GL_CLAMP_TO_EDGE},
					  {wrap_t,   ?GL_CLAMP_TO_EDGE}
					  %%,{min, ?GL_LINEAR_MIPMAP_LINEAR}
					  %%,{gen_mipmap, ?GL_TRUE}
					 ]},
				  {depth,[]}]),
    [{fbo,Fbo},{color,Tex}|_] = Buffers,
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0_EXT),
    #ao{sp=Prog, fbo=Fbo, tex=Tex, cleanup_fbo=Buffers,
	buf = wings_io:get_buffer(?TEX_SZ*?TEX_SZ, ?GL_UNSIGNED_BYTE)}.

cleanup(#ao{sp=Shader, dl=DispList, cleanup_fbo=Fbo}) ->
    gl:deleteLists(DispList,1),
    wings_gl:delete_fbo(Fbo),
    gl:useProgram(0),
    gl:deleteProgram(Shader).

process_obj(We, _) when ?IS_NOT_VISIBLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_NOT_SELECTABLE(We#we.perm) ->
    We;
process_obj(We, _) when ?IS_ANY_LIGHT(We) ->
    case We#we.name =/= "Ambient" of
	true -> We#we{perm=[]};
	false -> We
    end;
process_obj(We0, AO) ->
    #we{es=Etab,vp=Vtab,name=Name} = We0,
    io:fwrite("Processing: ~s\n", [Name]),
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

render_hemisphere(X,Y,[{Vertex,Eye}|Rest], We, AO = #ao{dl=DispList})
  when X < ?NUM_SAMPLES ->
    LookAt = wings_vertex:normal(Vertex,We),
    Mat = model_view(Eye, LookAt, get_up_right(LookAt)),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadMatrixd(Mat),
    gl:viewport(X*?SAMPLE_SZ,Y*?SAMPLE_SZ,?SAMPLE_SZ,?SAMPLE_SZ),
    %% Comment out the following line to see performance of the other calls
    gl:callList(DispList),
    render_hemisphere(X+1,Y, Rest, We, AO);
render_hemisphere(_,_,[], _, _) -> ok;
render_hemisphere(_,Y,Vs,We,AO) ->
    render_hemisphere(0,Y+1,Vs,We,AO).

read_frame(#ao{tex=Tex, fbo=Fbo, buf=Buffer}) ->
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
    gl:bindTexture(?GL_TEXTURE_2D, Tex),
    %%
    %% Check if hardware mipmapping works on
    %% RGB textures, they don't work with LUMINANCE on ATI
    %%
    %gl:generateMipmapEXT(?GL_TEXTURE_2D),
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, Fbo),
    gl:clear(?GL_COLOR_BUFFER_BIT  bor ?GL_DEPTH_BUFFER_BIT),
    %% test_img("Test", ImageBin),
    ImageBin.

%% Temporary fix
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

get_ao_factor(Buffer) ->
    Occluded = num_black(Buffer, 0),
    Result = 1.0 - (Occluded / (?FISH_EYE_AREA)),
    erlang:min(1.0, erlang:max(0.0, Result)).

num_black(<<255:8,Rest/binary>>, Sum) ->
    num_black(Rest, Sum);
num_black(<<0:8, Rest/binary>>, Sum) ->
    num_black(Rest, Sum+1);
num_black(<<>>, Sum) ->
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

model_view(Eye, {Dx,Dy,Dz}, {{UpX,UpY,UpZ}, {Rx,Ry,Rz}}) ->
    Z = 0.0,
    Rot = {Rx, UpX, -Dx,
	   Ry, UpY, -Dy,
	   Rz, UpZ, -Dz,
	   Z,   Z,   Z},
    Trans = e3d_mat:translate(e3d_vec:neg(Eye)),
    e3d_mat:expand(e3d_mat:mul(Rot,Trans)).

create_ambient_light(St) ->
    wings_pref:set_value(scene_lights, true),
    SceneLights = wings_light:export(St),
    case proplists:is_defined("Ambient", SceneLights) of
	true ->
	    St;
	false ->
	    White = {1.0,1.0,1.0,1.0},
	    Lights = [{"Ambient",[{opengl,[{type,ambient},{ambient,White}]}]}],
	    wings_light:import(Lights,St)
    end.

fisheye_vs() ->
<<"
varying vec4 color;

void main(void)
{
   float near = 0.001;
   float far  = 100.0;
   vec4 pos   = gl_ModelViewMatrix * gl_Vertex;
   pos.w      = 1.0;

   if(length(pos.xyz) > 0.00001) {
       color = vec4(0.0,0.0,0.0,1.0);
   } else {
       // Discard faces connected to the camera vertex
       // To avoid concave vertices, backfaces will take
       // of shadowing anyway.
      color = vec4(1.0,1.0,1.0,1.0);
   }


   float dist = length(pos.xyz);
   pos.xy = pos.xy / dist;

   pos.z = -((far+near)+2.0*pos.z)/(far - near);

   gl_Position = pos;
   // gl_FrontColor = color;
}
">>.

fisheye_fs() ->
<<"
varying vec4 color;

void main(void)
{
   // Discard faces connected to the camera vertex
   if(color.x > 0.001)
     discard;
   gl_FragColor = vec4(0.0,0.0,0.0,1.0);
}
">>.

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
