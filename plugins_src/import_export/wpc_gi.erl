%%
%%  wpc_gi.erl --
%%
%%     OpenGL renderer.
%%
%%  Copyright (c) 2002-2004 Bjorn Gustavsson
%%    2003-2004 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_gi.erl,v 1.1 2004/04/01 12:06:03 dgud Exp $

-module(wpc_gi).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,
		flat_length/1,append/1,append/2]).

-record(r,
	{acc_size=6,
	 attr,
	 mat,
	 data,
	 max = 20,
	 progress = true,
	 no_l = 0,
	 amb = none,
	 lights = [],
	 fish = undefined %% Fisheye texture
	}).

-record(f, {vs=[],  % Vertices
	    ns=[],  % Normals
	    c,      % Center (shooting) Position 
	    dir,    % Normal i.e. shooting direction
	    area,   % Physical area size
	    txsz,   % texture size 
	    txid,   % texture Id
	    mat=undefined}).

-record(d, {fdl,    % fast display list
	    fs = [],
	    we}).

-record(shaders, {stereo_vp,rad_vp,rad_fp}).

%get_opaque({[Smooth,_TrL],_Tr}) -> Smooth.
%get_transp({[_,TrL],_}) -> TrL.
%is_transp({_,Tr}) -> Tr.

%% Light record
-record(light, {type,
		pos,
		aim,
		attr,
		dl = [], % Display lists
		sv = []  % List of display lists of shadow volumes
	       }).

%%% PROGRAM PARAMETERS 

init() ->
    true.

menu({file,render}, Menu0) ->
    [{"GI test",gi,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{gi,Ask}}}, St) ->
    do_render(Ask, St);
command(_, _) -> next.

dialog_qs(render) ->
    DefOutput = get_pref(output_type, preview),
    DefKey   = {output_type,DefOutput},
    SubDiv   = get_pref(subdivisions, 0),
    Back     = get_pref(background_color, {0.4,0.4,0.4}),
    Alpha    = get_pref(render_alpha, false),
    Shadow   = get_pref(render_shadow, false),
    BumpMap  = get_pref(render_bumps, false),
    Filename = get_pref(output_file, "output.tga"),

    BumpP = wings_util:is_gl_ext({1,3}, bump_exts()) or programmable(),
    StencilP = hd(gl:getIntegerv(?GL_STENCIL_BITS)) >= 8,
    Ps = [{dialog_type,save_dialog},{ext,".tga"},{ext_desc,"Targa File"}],

    [{hframe,
      [{label,"Sub-division Steps"},{text,SubDiv,[{key,subdivisions},
						  {range,1,4}]}]},
     {hframe,
      [{label,"Background Color"},{color,Back,[{key,background_color}]}]},
     {"Render Alpha Channel",Alpha,[{key,render_alpha}]},
     {"Render Shadows",Shadow,
      [{key,render_shadow}, {hook, fun(is_disabled, _) -> not StencilP;
				      (_,_) -> void end}]},
     {"Render Self-Shadows (Bump maps)", BumpMap,
      [{key,render_bumps}, {hook, fun(is_disabled, _) -> not BumpP; (_,_) ->
					  void end}]},
     {vframe,
      [{key_alt,DefKey,"Image Window",preview},
       {hframe,
	[{key_alt,DefKey,"File: ",file},
	 {button,{text,Filename,[{key,output_file},{props,Ps},
				 {hook,fun button_hook/2}]}}]}],
      [{title,"Render Output"}]}
    ].

button_hook(is_disabled, {_Var,_I,Store}) ->
    gb_trees:get(output_type, Store) =/= file;
button_hook(_, _) -> void.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

do_render(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Render Options",
	       dialog_qs(render),
	       fun(Res) ->
		       {file,{render,{gi,Res}}}
	       end);
do_render(Attr, St) ->
    io:format("~p Err ~p~n", [?LINE,wings_util:gl_error_string(gl:getError())]),
    set_pref(Attr),
    {_,_,W,H} = wings_wm:viewport(),
    if W >= 512, H >= 512 ->
	    wings_pb:start("Rendering"),
	    io:format("~p Err ~p~n", [?LINE,wings_util:gl_error_string(gl:getError())]),
	    Data = create_dls(St, Attr),
	    io:format("~p Err ~p~n", [?LINE,wings_util:gl_error_string(gl:getError())]),
	    [FishId] = gl:genTextures(1),
	    gl:bindTexture(?GL_TEXTURE_2D, FishId),
	    gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),

	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
	    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
	    gl:bindTexture(?GL_TEXTURE_2D, 0),

	    Rr = #r{attr=Attr,data=Data,fish=FishId},
	    wings_pb:pause(),
	    radiosity(Rr),
	    wings_pb:done(),
	    render_exit(Rr);
       true ->
	    io:format("Enlarge your viewport\n")
    end.

del_list(X) when integer(X) ->
    gl:deleteLists(X,1);
del_list({[Smooth,TrL],_Tr}) ->
    del_list(Smooth),
    del_list(TrL);
del_list(_) -> ok.

render_exit(#r{lights=Lights, data=D}) ->
    io:format("Err ~p~n", [wings_util:gl_error_string(gl:getError())]),
    %% BUGBUG delete all textures also
    foreach(fun(#light{sv=L1, dl=L2}) ->
		    foreach(fun(DL) -> del_list(DL) end,L1++L2)
	    end, Lights),
    foreach(fun(#d{fdl=DL}) -> del_list(DL) end, D),
    case erase(gl_shaders) of
	#shaders{stereo_vp=Stereo, rad_vp=Vp, rad_fp=Fp} ->
	    gl:deleteProgramsARB(3, [Stereo,Vp,Fp]);
	_ ->
	    ignore
    end,
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_wm:dirty().

create_dls(St0, Attr) ->
    St = invisible_holes(St0),
    SubDiv = proplists:get_value(subdivisions, Attr),
    %%    RenderAlpha = proplists:get_bool(render_alpha, Attr),
    load_shaders(),
    Mtab = St#st.mat,
    Objects = foldl(fun(#we{perm=P},Wes) when ?IS_NOT_VISIBLE(P) ->
			    Wes;
		       (We = #we{light=none},Wes) ->
			    [We|Wes];
		       (_,Wes) ->
			    Wes
		    end, [], gb_trees:values(St#st.shapes)),
    Ds = foldl(fun(We, Wes) ->
		       prepare_mesh(We, SubDiv,Wes,length(Objects),St,Mtab)
	       end, [], Objects),
    wings_pb:update(0.98, "Rendering"),
    ?CHECK_ERROR(),
    Ds.

prepare_mesh(We0=#we{light=none},SubDiv,Fs,Shapes,_St,Mtab) ->
    Done  = length(Fs),
    Step  = 0.90/Shapes,
    Start = Done * Step,

    We = case SubDiv of
	     0 ->
		 %% If no sub-divisions requested, it is safe to
		 %% first triangulate and then freeze any mirror.
		 wings_pb:update(0.5*Step+Start, "Quadrangulating"),
		 We1 = wpa:quadrangulate(We0),
		 wpa:vm_freeze(We1);
	     _ ->
		 %% Otherwise, we must do it in this order
		 %% (slower if there is a virtual mirror).
		 wings_pb:update(0.55*Step+Start, "Subdividing and Quadrangulating"),
		 We1 = wpa:vm_freeze(We0),
		 We2 = sub_divide(SubDiv, We1),
		 wpa:quadrangulate(We2)
	 end,
    wings_pb:update(Step+Start, "Generating mesh data"),
    Data = build_data(We,Mtab),
    wings_pb:pause(),
    FLst = draw_ids_dl(Data),
    wings_pb:update(Step+Start, "Generating mesh data"),
    [#d{fdl=FLst,fs=Data}|Fs].

build_data(We,Mtab) ->
    FN0 = [{Face,wings_face:normal(Face, We)} ||
	      Face <- gb_trees:keys(We#we.fs)],
    FVN = wings_we:normals(FN0, We),
    %% sorted by mat ??
    MatFs = wings_material:mat_faces(FVN, We),
    C = fun({MatName,Fs}, Acc) ->
		Mat = gb_trees:get(MatName, Mtab),
		create_face(Fs, MatName, Mat, We, Acc)
	end,
    foldl(C, [], MatFs).

create_face([{Face, Data}|Rest], MatName, Mat, We, Acc) ->
    Vs  = wings_face:vertex_positions(Face, We),
    Ns  = [VN || [_|VN] <- Data],
    Sz  = {16,16},
    C   = e3d_vec:average(Vs),
    Normal  = e3d_vec:norm(e3d_vec:average(Ns)),
    [TexId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TexId),
    gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR_MIPMAP_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, ?GL_TRUE),
    %% Start with black texture (no lights)
    Emit = {_, {R0,G0,B0}} = get_emission(Mat,3),
    R = round(256*R0),G = round(256*G0),B = round(256*B0),
    gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGB,16,16,0,?GL_RGB,?GL_UNSIGNED_BYTE,
		  <<R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,

		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,

		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,

		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,
		   R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B,  R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B, R,G,B
		   >>),
    
    F   = #f{vs=Vs,ns=Ns,mat=Mat,txsz=Sz,dir=Normal,txid=TexId,c=C},
    New = {get_id(Face,We#we.id), Emit, F},
    create_face(Rest, MatName, Mat, We, [New|Acc]);
create_face([], _, _, _, Acc) ->
    Acc.

get_emission(Mat, Energy) ->
    OpenGL = proplists:get_value(opengl, Mat),
    {R,G,B,_} = proplists:get_value(emission, OpenGL),
    {Energy*(R+G+B)/3, {R,G,B}}.

-define(STOP, 0.5).
find_emitter(#r{data=Data}) ->
    Worst = {stop, {?STOP, stop}, stop},
    foldl(fun(#d{fs=Fs},Acc) -> foldl(fun find_emitter/2, Acc, Fs) end, 
	  Worst, Data).

find_emitter(FaceD = {_,Emit,_}, {_,Best,_}) when Emit > Best ->
    FaceD;
find_emitter(_, Best) ->
    Best.

calc_energy({OldE,OC={R2,B2,G2}}, NewCol={R1,B1,G1}, {_,{Energy,_},_}) -> 
    Diff = (R1-R2+B1-B2+G1-G2)/3,
    if Diff < 0 -> 
	    io:format("?? ~p ~p ~n", [OC, NewCol]),
       	    {OldE, NewCol};
       true ->
	    {OldE+Energy*Diff, NewCol}
    end.

draw_ids_dl(Fs) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_QUADS), 
    draw_ids2(Fs, ?GL_QUADS),
    gl:'end'(),
    gl:endList(),
    List.

draw_ids2([], _) -> ok;
draw_ids2([{Id, _, #f{vs=[V1,V2,V3]}}|R], Type = ?GL_TRIANGLES) ->
    gl:color3ubv(Id),
    gl:vertex3fv(V1),
    gl:vertex3fv(V2),
    gl:vertex3fv(V3),
    draw_ids2(R, Type);
draw_ids2([{Id, _, #f{vs=[V1,V2,V3,V4]}}|R], Type = ?GL_QUADS) ->
    gl:color3ubv(Id),
    gl:vertex3fv(V1),
    gl:vertex3fv(V2),
    gl:vertex3fv(V3),
    gl:vertex3fv(V4),
    draw_ids2(R, Type);
draw_ids2(R, Type0) ->
    Type = swap(Type0),
    gl:'end'(),
    gl:'begin'(Type),
    draw_ids2(R, Type).

swap(?GL_TRIANGLES) -> ?GL_QUADS;
swap(?GL_QUADS) -> ?GL_TRIANGLES.

vertex([V1,V2,V3,V4],[N1,N2,N3,N4]) ->
    gl:'begin'(?GL_QUADS),
    gl:normal3fv(N1),
    gl:texCoord2fv({0.0,0.0}),
    gl:vertex3fv(V1),
    gl:normal3fv(N2),
    gl:texCoord2fv({1.0,0.0}),
    gl:vertex3fv(V2),
    gl:normal3fv(N3),
    gl:texCoord2fv({1.0,1.0}),
    gl:vertex3fv(V3),
    gl:normal3fv(N4),
    gl:texCoord2fv({0.0,1.0}),
    gl:vertex3fv(V4),
    gl:'end'();
vertex([V1,V2,V3],[N1,N2,N3]) ->
    gl:'begin'(?GL_TRIANGLES),
    gl:normal3fv(N1),
    gl:texCoord2fv({0.0,0.0}),
    gl:vertex3fv(V1),
    gl:normal3fv(N2),
    gl:texCoord2fv({1.0,0.0}),
    gl:vertex3fv(V2),
    gl:normal3fv(N3),
    gl:texCoord2fv({1.0,1.0}),
    gl:vertex3fv(V3),
    gl:'end'().

get_id(FId,WId) when FId < 16#FFFF ->
    R = 255- (WId band 16#00FF),
    G = 255- (FId band 16#FF00),
    B = 255- (FId band 16#00FF),
    {R,G,B}.

%% Make the hole material a true hole (entirely invisible).
invisible_holes(#st{mat=Mat}=St) ->
    Hole0 = gb_trees:get('_hole_', Mat),
    OpenGl0 = proplists:get_value(opengl, Hole0),
    OpenGl = map(fun({Key,{R,G,B,_}}) -> {Key,{R,G,B,0.0}};
		    (Other) -> Other end, OpenGl0),
    Hole = [{opengl,OpenGl}|lists:keydelete(opengl, 1, Hole0)],
    St#st{mat=gb_trees:update('_hole_', Hole, Mat)}.

sub_divide(0, We) -> We;
sub_divide(N, We) -> sub_divide(N-1, wings_subdiv:smooth(We)).

%%%
%%% Rendering.
%%%

radiosity(Rr0) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:clearColor(0.0, 0.0, 0.0, 1),
    gl:frontFace(?GL_CCW),
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),

    Rr = calc_radiosity(Rr0, 0),
    render(Rr),

    capture("by GL_RAD", 3, ?GL_RGB, Rr#r.fish),    
    gl:popAttrib().

calc_radiosity(Rr0 = #r{max=Max},N) when N < Max ->
    case find_emitter(Rr0) of
	{stop, _, _} ->
	    io:format("stop ~n",[]),
	    Rr0;
	Emitter ->
	    io:format("Emitter ~p~n",[{element(1,Emitter),element(2,Emitter)}]),
	    %% 1st pass 
	    %% Capture FishEye view to get visible pixels
	    Matrix = draw_fisheye(Emitter,Rr0),

	    %% 2nd pass
	    %% Draw again 
	    gl:enable(?GL_TEXTURE_2D),
	    gl:bindTexture(?GL_TEXTURE_2D, Rr0#r.fish),

	    TempMem = sdl_util:alloc(3*4, ?GL_UNSIGNED_BYTE),
	    enable_shaders(radiosity, [Matrix,Emitter]),
	    %%    foreach(fun(Data) -> draw_fast(Data) end, Rr#r.data),
	    gl:disable(?GL_VERTEX_PROGRAM_ARB),
	    gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
	    New = ?TC(map(fun(Data) -> calc_rad(Data,Emitter,TempMem,Rr0#r.fish) end, Rr0#r.data)),
	    disable_shaders(),
	    Rr = Rr0#r{data=New},
	    if 
		Rr0#r.progress -> render(Rr), gl:swapBuffers();
		true -> ignore
	    end,
	   % capture("RAD " ++ integer_to_list(N), 3, ?GL_RGB, Rr#r.fish),
	    %% Repeat pass1 and pass2 for every emitting face	    
	    calc_radiosity(Rr, N+1)
    end;
calc_radiosity(RR, _) -> RR.

%% Actual drawing
draw_fisheye({_,_,#f{c=From,dir=Dir}}, #r{data=Dt, fish=Fish}) ->
    %% No shadows variant
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    {X,Y,_,_} = wings_wm:viewport(),
    W = H = 512,
    gl:viewport(X, Y, W, H),
    wings_view:load_matrices(false),
    Mat = setup_model_view(From,Dir),
%    io:format("Modelview~n"),
%    print_matrix(gl:getFloatv(?GL_MODELVIEW_MATRIX)),
%    io:format("Projection~n"),
%    print_matrix(gl:getFloatv(?GL_PROJECTION_MATRIX)),
    #view{hither=Near,yon=Far} = wings_view:current(),
    enable_shaders(stereographic_proj, [Near,Far]),
    gl:disable(?GL_TEXTURE_2D),
    foreach(fun(Data) -> draw_fast(Data) end, Dt),
    gl:bindTexture(?GL_TEXTURE_2D, Fish),
    gl:readBuffer(?GL_BACK),
    gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_RGB,X,Y,W,H,0),
    disable_shaders(),
    Mat.
    
draw_fast(#d{fdl=DL}) ->
    ?CHECK_ERROR(),
    gl:polygonMode(?GL_FRONT, ?GL_FILL),
%    gl:polygonMode(?GL_FRONT, ?GL_LINE),
%    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING),
    gl:enable(?GL_CULL_FACE),
    gl:enable(?GL_DEPTH_TEST),
    gl:disable(?GL_BLEND),
    gl:callList(DL),
    ?CHECK_ERROR().

calc_rad(D=#d{fs=Fs0},Emitter,Mem,Fish) ->
    Fs = calc_rad_face(Fs0,Emitter,Mem,Fish,[]),
    D#d{fs=Fs}.
calc_rad_face([{Id,{_,Old},F}|Fs],E={Id,_,_},Mem,Fish,Acc) ->
    calc_rad_face(Fs, E, Mem, Fish, [{Id, {0,Old}, F}|Acc]);
calc_rad_face([{Id,Emit0,F=#f{vs=Vs,ns=Ns,txsz=Tsz,txid=Txid}}|Fs],E,Mem,Fish,Acc) ->    
    {X,Y,_,_} = wings_wm:viewport(),
    {W,H} = Tsz, 
    gl:viewport(X,Y,W,H),
%    gl:viewport(X,Y,128,128),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),

    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_CULL_FACE),
    
    %% Background
    gl:disable(?GL_BLEND),
    gl:bindTexture(?GL_TEXTURE_2D, Txid),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2fv({0.0,0.0}), gl:vertex2f(0,0),
    gl:texCoord2fv({1.0,0.0}), gl:vertex2f(W,0),
    gl:texCoord2fv({1.0,1.0}), gl:vertex2f(W,H),
    gl:texCoord2fv({0.0,1.0}), gl:vertex2f(0,H),
    gl:'end'(),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_ONE, ?GL_ONE),
    
    gl:enable(?GL_VERTEX_PROGRAM_ARB),
    gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
    gl:bindTexture(?GL_TEXTURE_2D, Fish),
    gl:color3ubv(Id),
    vertex(Vs,Ns),
    gl:disable(?GL_VERTEX_PROGRAM_ARB),
    gl:disable(?GL_FRAGMENT_PROGRAM_ARB),

    Max = if W > H -> W; true -> H end,
    Level = round(math:log(Max)/math:log(2)),

    gl:bindTexture(?GL_TEXTURE_2D, Txid),
    gl:readBuffer(?GL_BACK),
    gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_RGB,X,Y,W,H,0),
    gl:finish(),
    gl:getTexImage(?GL_TEXTURE_2D,Level,?GL_RGB,?GL_FLOAT,Mem),
    <<R:32/native-float,G:32/native-float,B:32/native-float>> = sdl_util:getBin(Mem), 
    Emit = calc_energy(Emit0, {R,G,B}, E),
%%    io:format("RGB ~p ~p ~n",[{R,G,B},Emit]),
%    timer:sleep(25),gl:swapBuffers(),timer:sleep(25),
    calc_rad_face(Fs,E,Mem,Fish, [{Id,Emit,F}|Acc]);
calc_rad_face([],_,_,_, Acc) ->
    Acc.

%% Render scene
render(Rr) ->
    {X,Y,W,H} = wings_wm:viewport(),
    gl:viewport(X, Y, W, H),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_view:load_matrices(false),   
    gl:enable(?GL_DEPTH_TEST),
    gl:enable(?GL_CULL_FACE), 
    gl:disable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),

    RenderFace = fun({_,_,#f{vs=[V1,V2,V3,V4],txid=TxId}}) ->
			 gl:bindTexture(?GL_TEXTURE_2D,TxId),
			 gl:'begin'(?GL_QUADS),
			 gl:texCoord2fv({0.0,0.0}),
			 gl:vertex3fv(V1),
			 gl:texCoord2fv({1.0,0.0}),
			 gl:vertex3fv(V2),
			 gl:texCoord2fv({1.0,1.0}),
			 gl:vertex3fv(V3),
			 gl:texCoord2fv({0.0,1.0}),
			 gl:vertex3fv(V4),
			 gl:'end'();
		    ({_,_,#f{vs=[V1,V2,V3],txid=TxId}}) ->
			 gl:bindTexture(?GL_TEXTURE_2D,TxId),
			 gl:'begin'(?GL_TRIANGLES),
			 gl:texCoord2fv({0.0,0.0}),
			 gl:vertex3fv(V1),
			 gl:texCoord2fv({1.0,0.0}),
			 gl:vertex3fv(V2),
			 gl:texCoord2fv({1.0,1.0}),
			 gl:vertex3fv(V3),
			 gl:'end'()
		 end,
    foreach(fun(#d{fs=Fs}) -> foreach(RenderFace, Fs) end, Rr#r.data).

%%%
%%% Capture generated image.
%%%
capture(Name, N, Type, Fish) ->
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    %% Grab stereo texture 
    Wt=Ht=512,
    NumBytes2 = N*Wt*Ht,
    Mem2 = sdl_util:alloc(NumBytes2, ?GL_UNSIGNED_BYTE),
    gl:bindTexture(?GL_TEXTURE_2D, Fish),
    gl:getTexImage(?GL_TEXTURE_2D,0,Type,?GL_UNSIGNED_BYTE,Mem2),
    Pixels2 = sdl_util:getBin(Mem2),    
    Temp = #e3d_image{bytes_pp=N,order=lower_left,width=Wt,height=Ht,
		      image=Pixels2},
    wings_image:new_temp("<<Stereo" ++ Name ++">>", Temp),

    %% Grab Render
    gl:readBuffer(?GL_BACK),
    {X,Y,W,H} = wings_wm:viewport(),
    NumBytes = N*W*H,
    Mem = sdl_util:alloc(NumBytes, ?GL_UNSIGNED_BYTE),
    gl:readPixels(X, Y, W, H, Type, ?GL_UNSIGNED_BYTE, Mem),
    Pixels = sdl_util:getBin(Mem),
    Image = #e3d_image{bytes_pp=N,order=lower_left,width=W,height=H,image=Pixels},
    Id = wings_image:new_temp("<<Rendered"++ Name ++">>", Image),
    wings_image:window(Id).

%% Utilities

setup_model_view({_Px,_Py,_Pz}=Point, {Dx,Dy,Dz}=Dir) ->
    Up = {_Upx,_Upy,_Upz} = up(Dir),
    S = {Sx,Sy,Sz} = e3d_vec:cross(Dir,Up),
    {Ux,Uy,Uz} = e3d_vec:cross(S,Dir),
    Z = 0.0,
    Rot = { Sx, Ux, -Dx,
	    Sy, Uy, -Dy,
	    Sz, Uz, -Dz,
	    Z,   Z,   Z},
    Trans = e3d_mat:translate(e3d_vec:neg(Point)),
    Mat   = e3d_mat:expand(e3d_mat:mul(Rot,Trans)),
    %%  DEBUG
    %%     gl:loadIdentity(),
    %%     glu:lookAt(Px,Py,Pz, Px+Dx,Py+Dy,Pz+Dz, Upx,Upy,Upz),
    %%     io:format("Corr~n~w~n", [gl:getFloatv(?GL_MODELVIEW_MATRIX)]),    
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadMatrixd(Mat),
    %% io:format("My~n~w~n", [gl:getFloatv(?GL_MODELVIEW_MATRIX)]),
    Mat.
    
up({X,Y,Z}) when abs(Y) > abs(X), abs(Y) > abs(Z) ->
    {1.0,0.0,0.0};
up(_) ->
    {0.0,1.0,0.0}.

bump_exts() ->
    ['GL_ARB_texture_cube_map',
     'GL_ARB_multitexture',
     'GL_ARB_texture_env_combine',
     'GL_ARB_texture_env_dot3',
     'GL_EXT_texture3D'].

programmable() ->
    wings_util:is_gl_ext(['GL_ARB_vertex_program',
			  'GL_ARB_fragment_program']).

enable_shaders(stereographic_proj, [Near,Far]) ->
    #shaders{stereo_vp = Stereo} =  get(gl_shaders),
    gl:bindProgramARB(?GL_VERTEX_PROGRAM_ARB,Stereo),
    gl:enable(?GL_VERTEX_PROGRAM_ARB),
    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,0, Near,Far,0.0,0.0);

enable_shaders(radiosity, [Mat, {_, {Energy, {R,G,B}}, #f{c={Px,Py,Pz},dir={Dx,Dy,Dz}}}]) ->
    Area = 1.0,
    #shaders{rad_vp = VP, rad_fp=FP} =  get(gl_shaders),
    gl:enable(?GL_VERTEX_PROGRAM_ARB),
    gl:bindProgramARB(?GL_VERTEX_PROGRAM_ARB,VP),
    gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
    gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,FP),
    gl:programEnvParameter4fARB(?GL_FRAGMENT_PROGRAM_ARB,0,Px,Py,Pz,0),
    gl:programEnvParameter4fARB(?GL_FRAGMENT_PROGRAM_ARB,1,Dx,Dy,Dz,0),
    gl:programEnvParameter4fARB(?GL_FRAGMENT_PROGRAM_ARB,2,Area,Energy,0,0),
    gl:programEnvParameter4fARB(?GL_FRAGMENT_PROGRAM_ARB,3,R*Energy,G*Energy,B*Energy,1),
    gl:matrixMode(?GL_MATRIX1_ARB),
    gl:loadMatrixd(Mat),
    gl:matrixMode(?GL_MODELVIEW_MATRIX).

disable_shaders() ->
    gl:bindProgramARB(?GL_VERTEX_PROGRAM_ARB,0),
    gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,0),
    gl:disable(?GL_VERTEX_PROGRAM_ARB),
    gl:disable(?GL_FRAGMENT_PROGRAM_ARB).

load_shaders() ->
    Stereo = load_program(?GL_VERTEX_PROGRAM_ARB, stereographic_proj()),
    RadVP  = load_program(?GL_VERTEX_PROGRAM_ARB,radiosity_vp()),
    RadFP  = load_program(?GL_FRAGMENT_PROGRAM_ARB,radiosity_fp()),
    Shaders = #shaders{stereo_vp = Stereo,
		       rad_vp = RadVP,
		       rad_fp = RadFP},
    put(gl_shaders, Shaders),
    Shaders.

load_program(Target, Prog) ->
    [PId] = gl:genProgramsARB(1),
    gl:bindProgramARB(Target,PId),

    gl:programStringARB(Target,?GL_PROGRAM_FORMAT_ASCII_ARB,size(Prog),Prog),
    case gl:getString(?GL_PROGRAM_ERROR_STRING_ARB) of
	[] -> PId;
	Err ->
	    print_program(binary_to_list(Prog)),
	    io:format("Error when loading program: ~s\n", [Err]),
	    exit(prog_error)
    end.

print_matrix([A,B,C,D|R]) ->
    io:format("~.3f ~.3f ~.3f ~.3f~n", [A,B,C,D]),
    print_matrix(R);
print_matrix([]) -> ok.


print_program(Cs) ->
    io:format("~3w: ", [1]),
    print_program_1(Cs, 1).

print_program_1([$\n|Cs], Line0) ->
    Line = Line0 + 1,
    io:format("\n~3w: ", [Line]),
    print_program_1(Cs, Line);
print_program_1([C|Cs], Line) ->
    io:put_chars([C]),
    print_program_1(Cs, Line);
print_program_1([], _) -> ok.

stereographic_proj() ->
    <<"!!ARBvp1.0

     PARAM fisheye[4]  = { state.matrix.modelview };
     PARAM nearfar = program.env[0];
     ATTRIB xyz = vertex.position;

     TEMP pos, temp;

     DP4 pos.x, fisheye[0], xyz;
     DP4 pos.y, fisheye[1], xyz;
     DP4 pos.z, fisheye[2], xyz;
     MOV pos.w, 1.0;

     # Normalize
     DP3 temp.x, pos, pos;
     RSQ temp.x, temp.x;
     MUL pos.xy, pos, temp.x;

     # Namnare
     ADD temp.y, nearfar.y, -nearfar.x;
     RCP temp.y, temp.y;
     # Taljare
     ADD temp.z, -nearfar.y, -nearfar.x;
     ADD temp.x, -pos.z, -pos.z;
     ADD temp.x, temp.x, temp.z;
     # Divide
     MUL pos.z, temp.x, temp.y;
     MOV result.position, pos;
     MOV result.color, vertex.color;

     END
     ">>.

radiosity_vp() ->
    <<"!!ARBvp1.0
PARAM mvp[4]      = { state.matrix.mvp };
PARAM fisheye[4]  = { state.matrix.program[1] };
PARAM scale = { 2.0, 2.0, -1.0, -1.00 };


ATTRIB xyz = vertex.position;
ATTRIB faceId = vertex.color;
TEMP pos, temp;

DP4 pos.x, fisheye[0], xyz;
DP4 pos.y, fisheye[1], xyz;
DP4 pos.z, fisheye[2], xyz;

#DP4 result.position.x, mvp[0], xyz;
#DP4 result.position.y, mvp[1], xyz;
#DP4 result.position.z, mvp[2], xyz;
#DP4 result.position.w, mvp[3], xyz;

MAD temp.xy, vertex.texcoord, scale, scale.z;
MOV result.position.xy, temp;
MOV result.position.z, 0.0;
MOV result.position.w, 1.0;

# Normalize to get fisheye
DP3 temp.x, pos, pos;
RSQ temp.x, temp.x;
MUL pos.xy, pos, temp.x;
# Move to texture space
MAD pos.xy, pos, 0.5, 0.5;

MOV result.texcoord[0].xy,  pos;
MOV result.texcoord[1].xyz, xyz;
MOV result.texcoord[2].xyz, vertex.normal;
MOV result.color,           faceId;

END
">>.

radiosity_fp() ->
    <<"!!ARBfp1.0      
PARAM color = { 1.0, 1.00, 1.0, 1.00 };
# 1/256 = 0.0039 within acceptable range
PARAM error = { 0.0039, 0.0039, 0.0039, 0.0039 };
PARAM shootPos      = program.env[0];
PARAM shootNormal   = program.env[1];
PARAM shootArea     = program.env[2];
PARAM shootEnergy   = program.env[3];

ATTRIB fishEyePos  = fragment.texcoord[0];
ATTRIB texPos3d    = fragment.texcoord[1];
ATTRIB texNormal   = fragment.texcoord[2];
ATTRIB faceId      = fragment.color;

TEMP visible, fishId, r, dist2, cos, top, bot, fij;

# Check visisbility if not visible kill fragment
TEX fishId, fishEyePos, texture[0], 2D;
MOV visible.w, 1.0;
SUB visible.xyz, fishId, faceId;
ABS visible.xyz, visible;
SUB visible.xyz, error, visible;
KIL visible;

# Calculate Radiosity
SUB r, shootPos, texPos3d;  
DP3 dist2.x, r, r;
RSQ dist2.z, dist2.x;
MUL r, r, dist2.z;
DP3 cos.x, texNormal, r;
DP3 cos.y, shootNormal, r;
MUL top.x,   cos.x, -cos.y;
MAX top.x,   top.x, 0.0;
MAD bot.x, 3.141592, dist2.x, shootArea.x;
MAX bot.x,   bot.x, 1.0;
RCP bot,   bot.x;
MUL fij.x,   top.x, bot.x;
MUL fij,   fij.x, color; 
MUL fij,   fij, shootEnergy;
MOV result.color.rgb, fij;
#MOV result.color.rgb, faceId;
END
">>.


