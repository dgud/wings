%%
%%  ambocc_gl2 --
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

-record(ao, {dl, fbo, tex, cleanup_fbo, buf}).
-define(TEX_SZ, 1024).
-define(SAMPLE_SZ, 64).
-define(NUM_SAMPLES, (?TEX_SZ div ?SAMPLE_SZ)).

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
    St2.

setup_gl() ->
    gl:clearColor(1,1,1,0),  % Sky Color
    gl:color4f(0,0,0,1),     % Obj Color
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

cleanup(#ao{dl=DispList, cleanup_fbo=Fbo}) ->
    gl:deleteLists(DispList,1),
    wings_gl:delete_fbo(Fbo).

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
    wpc_ambocc:render_hemicube(X*?SAMPLE_SZ,Y*?SAMPLE_SZ,
			       Eye,LookAt,DispList),
    render_hemisphere(X+1,Y, Rest, We, AO);
render_hemisphere(_,_,[], _, _) -> ok;
render_hemisphere(_,Y,Vs,We,AO) ->
    render_hemisphere(0,Y+1,Vs,We,AO).

read_frame(#ao{tex=Tex, fbo=Fbo, buf=Buffer}) ->
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, 0),
    gl:bindTexture(?GL_TEXTURE_2D, Tex),
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_LUMINANCE, ?GL_UNSIGNED_BYTE, Buffer),
    ImageBin = wings_io:get_bin(Buffer),
    gl:bindFramebufferEXT(?GL_FRAMEBUFFER_EXT, Fbo),
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
    AO = wpc_ambocc:get_ao_factor(Buffer),
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
