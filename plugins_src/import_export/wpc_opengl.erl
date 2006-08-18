%%
%%  wpc_opengl.erl --
%%
%%     OpenGL renderer.
%%
%%  Copyright (c) 2002-2004 Bjorn Gustavsson
%%		  2003-2004 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_opengl.erl,v 1.74 2006/01/25 20:30:01 dgud Exp $

-module(wpc_opengl).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-import(lists, [foldl/3,map/2,foreach/2,reverse/1,seq/2,
		flat_length/1,append/1,append/2]).

-export([calcTS/4]). %% debug
-export([expand_arealights/1]). % debug

%% UNTIL ESDL catches up..
-ifndef(GL_DEPTH_CLAMP_NV).
-define(GL_DEPTH_CLAMP_NV,	16#864F).
-endif.

-define(STENCIL_INIT, 128).

-record(r,
	{acc_size=6,
	 attr,
	 mat,
	 data,
	 no_l	= 0,	 
	 amb	= none,
	 lights = [],
	 mask,
	 bump,	% true | false
	 shadow % true | false
	}).

-record(d, {l,	     % smooth display list
	    f,	     % fast display list
	    matfs,   % [{mat,[{Face, [VInfo|Normal]}]}]
	    hasBump, % true | false
	    we}).

get_opaque({[Smooth,_TrL],_Tr}) -> Smooth.
get_transp({[_,TrL],_}) -> TrL.
is_transp({_,Tr}) -> Tr.

%% Light record
-record(light, {type, 
		pos, 
		aim, 
		attr, 
		dl = [], % Display lists
		sv = []	 % List of display lists of shadow volumes
	       }).

init() ->
    true.

menu({file,render}, Menu0) ->
    [{"OpenGL",opengl,[option]}] ++ Menu0;
menu(_, Menu) -> Menu.

command({file,{render,{opengl,Ask}}}, St) ->
    do_render(Ask, St);
command(_, _) -> next.

dialog_qs(render) ->
    DefOutput = get_pref(output_type, preview),
    DefKey = {output_type,DefOutput},
    SubDiv = get_pref(subdivisions, 0),
    Back = get_pref(background_color, {0.4,0.4,0.4}),
    Alpha = get_pref(render_alpha, false),
    Shadow = get_pref(render_shadow, false),
    BumpMap  = get_pref(render_bumps, false),
    Filename = get_pref(output_file, "output.tga"),
    
    BumpP = wings_gl:is_ext({1,3}, bump_exts()) or programmable(),
    StencilP = hd(gl:getIntegerv(?GL_STENCIL_BITS)) >= 8,
    Ps = [{dialog_type,save_dialog},{ext,".tga"},{ext_desc,?__(1,"Targa File")}],

    [aa_frame(),
     {hframe,
      [{label,?__(2,"Sub-division Steps")},{text,SubDiv,[{key,subdivisions},
						  {range,1,4}]}]},
     {hframe,
      [{label,?__(3,"Background Color")},{color,Back,[{key,background_color}]}]},
     {?__(4,"Render Alpha Channel"),Alpha,[{key,render_alpha}]},
     {?__(5,"Render Shadows"),Shadow,
      [{key,render_shadow}, {hook, fun(is_disabled, _) -> not StencilP; (_,_) -> void end}]},
     {?__(6,"Render Self-Shadows (Bump maps)"), BumpMap, 
      [{key,render_bumps}, {hook, fun(is_disabled, _) -> not BumpP; (_,_) -> void end}]},
     {vframe,
      [{key_alt,DefKey,?__(7,"Image Window"),preview},
       {hframe,
	[{key_alt,DefKey,?__(8,"File")++": ",file},
	 {button,{text,Filename,[{key,output_file},{props,Ps},
				 {hook,fun button_hook/2}]}}]}],
      [{title,?__(9,"Render Output")}]}
    ].

button_hook(is_disabled, {_Var,_I,Store}) ->
    gb_trees:get(output_type, Store) =/= file;
button_hook(_, _) -> void.

aa_frame() ->
    HaveAccum = have_accum(),
    Def0 = get_pref(aa, if HaveAccum -> regular; true -> draft end),
    Def = case HaveAccum of
	      false -> draft;
	      true -> Def0
	  end,
    {hframe,
     [{menu,
       [{?__(1,"Draft Quality (no AA)"),draft}|
	case have_accum() of
	    false -> [];
	    true ->
		[{?__(2,"Regular Quality (normal AA)"),regular},
		 {?__(3,"Super Quality"),super},
		 {?__(4,"Premium Quality"),premium}]
	end],Def,[{key,aa}]}]}.

have_accum() ->
    [R] = gl:getIntegerv(?GL_ACCUM_RED_BITS),
    [G] = gl:getIntegerv(?GL_ACCUM_GREEN_BITS),
    [B] = gl:getIntegerv(?GL_ACCUM_BLUE_BITS),
    R >= 16 andalso G >= 16 andalso B >= 16.

get_pref(Key, Def) ->
    wpa:pref_get(?MODULE, Key, Def).

set_pref(KeyVals) ->
    wpa:pref_set(?MODULE, KeyVals).

%%%
%%% Rendering.
%%%

do_render(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, ?__(1,"Render Options"),
	       dialog_qs(render),
	       fun(Res) ->
		       {file,{render,{opengl,Res}}}
	       end);
do_render(Attr, St) ->
    set_pref(Attr),
    
    Aa = proplists:get_value(aa, Attr),
    AccSize = translate_aa(Aa),
    RenderAlpha	 = proplists:get_bool(render_alpha, Attr),
    RenderShadow = proplists:get_value(render_shadow, Attr, false),
    RenderBumps	 = proplists:get_value(render_bumps, Attr, false),

    wings_pb:start(?__(2,"Rendering")),
    {Data,{NOL, Amb,Lights}} = create_dls(St, Attr, RenderShadow, RenderBumps),
    
    Rr = #r{acc_size=AccSize,attr=Attr,
	    data=Data,mat=St#st.mat,
	    no_l=NOL,amb=Amb,lights=Lights,
	    mask = RenderAlpha, shadow=RenderShadow},
    wings_pb:pause(),
    render_redraw(Rr),
    wings_pb:done(),
    render_exit(Rr).

translate_aa(draft) -> 1;
translate_aa(regular) -> 4;
translate_aa(super) -> 8;
translate_aa(premium) -> 16.

del_list(X) when integer(X) ->
    gl:deleteLists(X,1);
del_list({[Smooth,TrL],_Tr}) ->
    del_list(Smooth),
    del_list(TrL);
del_list(_) -> ok.

render_exit(#r{lights=Lights, data=D}) ->
    gl:getError(),
    foreach(fun(#light{sv=L1, dl=L2}) ->
		    foreach(fun(DL) -> del_list(DL) end,L1++L2)
	    end, Lights),
    foreach(fun(#d{l=DL}) -> del_list(DL) end, D),
    case erase(gl_shaders) of
	{Vp, Fp} -> 
	    gl:deleteProgramsARB(2, [Vp,Fp]);
	_ ->
	    ignore
    end,
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    wings_wm:dirty().

prepare_mesh(We0=#we{light=none},SubDiv,RenderAlpha,RenderBumps,Wes,Shapes,St) ->    
    Done  = length(Wes),
    Step  = 0.90/Shapes, 
    Start = Done * Step,
    
    We = case SubDiv of
	     0 ->
		 %% If no sub-divisions requested, it is safe to
		 %% first triangulate and then freeze any mirror.
		 wings_pb:update(0.8*Step+Start, ?__(1,"Triangulating")),
		 We1 = wpa:triangulate(We0),
		 wpa:vm_freeze(We1);
	     _ ->
		 %% Otherwise, we must do it in this order
		 %% (slower if there is a virtual mirror).
		 wings_pb:update(0.9*Step+Start, ?__(2,"Subdividing and triangulating")),
		 We1 = wpa:vm_freeze(We0),
		 We2 = sub_divide(SubDiv, We1),
		 wpa:triangulate(We2)
	 end,
    wings_pb:pause(),
    Fast = dlist_mask(RenderAlpha, We),
    wings_pb:update(Step+Start, ?__(3,"Generating mesh data")),
    FN0	     = [{Face,wings_face:normal(Face, We)} || Face <- gb_trees:keys(We#we.fs)],
    FVN	     = wings_we:normals(FN0, We),  %% gb_tree of {Face, [VInfo|Normal]}
    MatFs0   = wings_facemat:mat_faces(FVN, We), %% sorted by mat..
    MatFs    = patch_tangent_space(MatFs0, We, []),
    PAble = programmable(),
    wings_pb:pause(),
    Rtype = if
		RenderBumps and PAble -> 
		    load_shaders(),
		    pable;
		true -> 
		    skip
	    end,
    DL = draw_faces(MatFs, We, St#st.mat, Rtype),
    [#d{l=DL,f=Fast,hasBump=RenderBumps,matfs=MatFs,we=We}|Wes].

create_dls(St0, Attr, Shadows, Bumps) ->
    St = invisible_holes(St0),
    SubDiv = proplists:get_value(subdivisions, Attr),
    RenderAlpha = proplists:get_bool(render_alpha, Attr),
    Objects = foldl(fun(#we{perm=P},Wes) when ?IS_NOT_VISIBLE(P) ->
			    Wes;
		       (We = #we{light=none},Wes) ->
			    [We|Wes];
		       (_,Wes) ->
			    Wes
		    end,[], gb_trees:values(St#st.shapes)),
    Ds = foldl(fun(We, Wes) ->
		       prepare_mesh(We, SubDiv, RenderAlpha, Bumps, 
				    Wes, length(Objects), St)
	       end, [], Objects),
    Ls = if
	     Shadows or Bumps -> %% Needs per-light data..
		 Ls0  = expand_arealights(wpa:lights(St)),
		 Mats = St#st.mat,
		 wings_pb:update(0.95, ?__(1,"Generating shadows and bump data per light ")),
		 wings_pb:pause(),
		 create_light_data(Ls0, Ds, 0, Mats, Bumps, Shadows, none, []);
	     true ->
		 {0,none,[]}
	 end,
    wings_pb:update(0.98, ?__(2,"Rendering")),
    ?CHECK_ERROR(),
    {Ds, Ls}.

expand_arealights([]) -> [];
expand_arealights([{Name,Ps0}=Light|Ls]) ->
    case proplists:get_value(visible, Ps0) of
	false -> [Light|expand_arealights(Ls)];
	true ->
	    OpenGL = proplists:get_value(opengl, Ps0),
	    Ps = proplists:delete(opengl, Ps0),
	    case proplists:get_value(type, OpenGL) of
		area -> 
		    expand_arealight(Name, Ps, OpenGL)++expand_arealights(Ls);
		_ -> 
		    [Light|expand_arealights(Ls)]
	    end
    end.

expand_arealight(Name, Ps, OpenGL0) ->
    Diffuse = proplists:get_value(diffuse, OpenGL0),
    Ambient = proplists:get_value(ambient, OpenGL0),
    Specular = proplists:get_value(specular, OpenGL0),
    #e3d_mesh{vs=Vs,fs=Fs} = Mesh = 
%%% 	e3d_mesh:triangulate(proplists:get_value(mesh, OpenGL0)),
	proplists:get_value(mesh, OpenGL0),
    OpenGL = proplists_delete_list(
	       [diffuse,ambient,specular,mesh,cone_angle,spot_exponent], 
	       OpenGL0),
    As0 = e3d_mesh:face_areas(Mesh),
    Area = foldl(fun (A, Acc) -> A+Acc end, 0.0, As0),
    case catch [A/Area || A <- As0] of
	{'EXIT',{badarith,_}} -> [];
	As -> expand_arealight_1(Name, Ps, OpenGL, 1,
				 Diffuse, Ambient, Specular,
				 list_to_tuple(Vs), Fs, As)
    end.

expand_arealight_1(_Name, _Ps, _OpenGL, _I, _Diffuse, _Ambient, _Specular,
		   _VsT, [], []) -> [];
expand_arealight_1(Name, Ps, OpenGL, I, Diffuse, Ambient, Specular,
		   VsT, [#e3d_face{vs=Vs}|Fs], [A|As]) ->
    Positions = [element(V+1, VsT) || V <- Vs],
    P0 = e3d_vec:average(Positions),
%%%     [P1,P2,P3] = Ps,
%%%     P12 = e3d_vec:average(P1, P2),
%%%     P23 = e3d_vec:average(P2, P3),
%%%     P31 = e3d_vec:average(P3, P1),
%%%     P01 = e3d_vec:average(P0, P1),
%%%     P02 = e3d_vec:average(P0, P2),
%%%     P03 = e3d_vec:average(P0, P3),
    case e3d_vec:normal(Positions) of
	{0.0,0.0,0.0} -> 
	    expand_arealight_1(Name, Ps, OpenGL, I+1, 
			       Diffuse, Ambient, Specular,
			       VsT, Fs, As);
	Direction ->
%%% 	    PPs = [{P1,A/36},{P2,A/36},{P3,A/36},
%%% 		   {P12,A/12},{P23,A/12},{P31,A/12},
%%% 		   {P01,A/6},{P02,A/6},{P03,A/6},{P0,A/6}],
	    PPs = [{P0,A}],
	    expand_arealight_2(Name++"_"++integer_to_list(I), Ps, OpenGL, 1,
			       Diffuse, Ambient, Specular, Direction, PPs)
		++expand_arealight_1(Name, Ps, OpenGL, I+1,
				     Diffuse, Ambient, Specular,
				     VsT, Fs, As)
    end.

expand_arealight_2(_Name, _Ps, _OpenGL, _J, _Diffuse, _Ambient, _Specular,
		   _Direction, []) -> [];
expand_arealight_2(Name, Ps, OpenGL, J, Diffuse, Ambient, Specular,
		   Direction, [{Position,Power}|PPs]) ->
    [{Name++"_"++integer_to_list(J),
      [{opengl,
	[{type,spot},
	 {diffuse,scale_color(Diffuse, Power)},
	 {ambient,scale_color(Ambient, Power)},
	 {specular,scale_color(Specular, Power)},
	 {position,Position},
	 {aim_point,e3d_vec:add(Position, Direction)},
	 {cone_angle,90.0},
	 {spot_exponent,1.0}|OpenGL]}
       |Ps]}|expand_arealight_2(Name, Ps, OpenGL, J+1,
				Diffuse, Ambient, Specular, Direction, PPs)].

proplists_delete_list([], Ps) -> Ps;
proplists_delete_list([Key|Keys], Ps) -> 
    proplists_delete_list(Keys, proplists:delete(Key, Ps)).
			     
scale_color({R,G,B,A}, Scale) -> {R*Scale,G*Scale,B*Scale,A}.



create_light_data([], _Ds, C, _Mat, _Bumps, _Shadow, Amb, Acc) ->
    {C,Amb,Acc};
create_light_data([{_Name,L0}|Ls], Ds, C, Mat, Bumps, Shadow, Amb, Acc) ->
    L = proplists:get_value(opengl, L0),
    Vis = proplists:get_value(visible, L0),
    case proplists:get_value(type, L) of
	_ when Vis == false -> %% Disabled
	    create_light_data(Ls, Ds, C, Mat, Bumps, Shadow, Amb, Acc);
	ambient when Amb == none -> 
	    AmbC = proplists:get_value(ambient, L),
	    create_light_data(Ls, Ds, C+1, Mat, Bumps, Shadow, AmbC, Acc);
	ambient	 -> %% Several ambient ?? 
	    create_light_data(Ls, Ds, C, Mat, Bumps, Shadow, Amb, Acc);
	_ ->
	    Li = create_light(L,#light{}, []),
	    {DLs,SVs} = 
		lists:foldl(
		  fun(D = #d{l=DL},{DLs,SLs}) ->
			  case is_transp(DL) orelse (not Shadow) of
			      true -> 
				  %% Transperant objects don't cast shadows
				  {[create_bumped(D,Bumps,Mat,Li)|DLs], SLs};
			      false -> 
				  List = gl:genLists(1),
				  gl:newList(List, ?GL_COMPILE),    
				  create_shadow_volume(Li, D),
				  gl:endList(),
				  {[create_bumped(D,Bumps,Mat,Li)|DLs],[List|SLs]}
			  end
		  end, {[],[]}, Ds),
	    create_light_data(Ls, Ds, C+1, Mat, Bumps, Shadow, Amb, 
			      [Li#light{dl = DLs,sv = SVs}|Acc])
    end.

create_light([{type,Type}|R], L, Acc) -> create_light(R,L#light{type=Type},Acc);
create_light([{position, Pos}|R], L, Acc)  -> create_light(R,L#light{pos=Pos},Acc);
create_light([{aim_point, Aim}|R], L, Acc)  -> create_light(R,L#light{aim=Aim},Acc);
create_light([Attr|R], L, Acc)	      -> create_light(R,L,[Attr|Acc]);
create_light([], L, Acc) ->  L#light{attr=Acc}.

dlist_mask(false, _) -> none;
dlist_mask(true, #we{fs=Ftab}=We) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_TRIANGLES),
    dlist_mask_2(gb_trees:to_list(Ftab), We),
    gl:'end'(),
    gl:endList(),
    List.

dlist_mask_2([{Face,Edge}|Fs], We) ->
    wings_draw_util:unlit_face(Face, Edge, We),
    dlist_mask_2(Fs, We);
dlist_mask_2([], _We) -> ok.

patch_tangent_space(Acc, #we{mode=vertex}, []) ->
    Acc;
patch_tangent_space([{Mat,Fs} | R], We, Acc) ->
    Patched = foldl(fun({Face, [[UV1|N1],[UV2|N2],[UV3|N3]|_BUG]},Res) ->
			    [V1,V2,V3|_] = wings_face:vertex_positions(Face, We),
			    TBN1 = calcTS(V3,V1,V2,UV3,UV1,UV2,N1),
			    TBN2 = calcTS(V1,V2,V3,UV1,UV2,UV3,N2),
			    TBN3 = calcTS(V2,V3,V1,UV2,UV3,UV1,N3),
			    [{Face, [[UV1|TBN1],[UV2|TBN2],[UV3|TBN3]]}|Res];
		       (_,Res) -> %% Horribly triangulated faces skipped
			    Res
		    end, [], Fs),
    patch_tangent_space(R, We, [{Mat,Patched}|Acc]);
patch_tangent_space([], _, Acc) ->
    Acc.


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

render_redraw(#r{attr=Attr}=Rr) ->
    render_image(Rr, false),
    ObjectImage = capture(3, ?GL_RGB),
    Image = case proplists:get_bool(render_alpha, Attr) of
		true ->
		    render_image(Rr, true),
		    MaskImage = capture(1, ?GL_RED),
		    combine_images(ObjectImage, MaskImage);
		false -> ObjectImage
	    end,
    case proplists:get_value(output_type, Attr) of
	preview ->
	    Id = wings_image:new_temp(?__(1,"<<Rendered>>"), Image),
	    wings_image:window(Id);
	file ->
	    RendFile = proplists:get_value(output_file, Attr),
	    ok = e3d_image:save(Image, RendFile)
    end.

render_image(#r{acc_size=AccSize,attr=Attr}=Rr, RendMask) ->
    gl:clear(?GL_ACCUM_BUFFER_BIT),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    case RendMask of
	false ->
	    {R,G,B} = proplists:get_value(background_color, Attr),
	    gl:clearColor(R, G, B, 1);
	true ->
	    gl:clearColor(0, 0, 0, 1),
	    gl:color3f(1, 1, 1)
    end,
    J = jitter(AccSize),
    jitter_draw(J, ?GL_LOAD, Rr, RendMask),
    gl:accum(?GL_RETURN, 1.0),
    gl:popAttrib().

jitter_draw([{Jx,Jy}|J], Op, #r{acc_size=AccSize}=Rr, RendMask) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:frontFace(?GL_CCW),
    {_,_,W,H} = wings_wm:viewport(),
    [Fov,Hither,Yon] = wpa:camera_info([fov,hither,yon]),
    accPerspective(Fov, W/H, Hither, Yon, Jx, Jy, 0, 0, 1),
    draw_all(Rr, RendMask),
    gl:accum(Op, 1/AccSize),
    jitter_draw(J, ?GL_ACCUM, Rr, RendMask);
jitter_draw([], _, _,_) -> ok.

draw_all(#r{data=Wes,lights=Ligths,amb=Amb,mat=Mat,shadow=Shadows,no_l=PerLigth},false) 
  when PerLigth > 0 ->
    wings_view:modelview(false),
    if 
	Shadows == true -> 
	    case wings_gl:is_ext('GL_NV_depth_clamp') of
		true -> gl:enable(?GL_DEPTH_CLAMP_NV);
		false -> setup_projection_matrix()
	    end;
	true ->
	    ignore
    end,     
    ?CHECK_ERROR(),
    %% Carmack's reversed shadow algorithm
    gl:clearStencil(?STENCIL_INIT),
    disable_lights(),
    gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE),    
    gl:clear(?GL_COLOR_BUFFER_BIT bor 
	     ?GL_DEPTH_BUFFER_BIT bor 
	     ?GL_STENCIL_BUFFER_BIT),

    gl:disable(?GL_ALPHA_TEST),

    gl:frontFace(?GL_CCW),
    gl:shadeModel(?GL_SMOOTH),

    gl:enable(?GL_DEPTH_TEST),
    gl:depthMask(?GL_TRUE),
    gl:depthFunc(?GL_LESS),

    ?CHECK_ERROR(),
    %% Set the depth-buffer and ambient colors

    setup_amb(Amb),
    gl:enable(?GL_LIGHTING),
    gl:disable(?GL_CULL_FACE),
    bind_ambient_shaders(),
    enable_shaders(),
    foreach(fun(#d{l=DL}) ->	gl:callList(get_opaque(DL)) end, Wes),
    disable_lights(),
    gl:disable(?GL_LIGHTING),
    %% Additive blend in the other lights
    gl:blendFunc(?GL_ONE, ?GL_ONE),
    gl:enable(?GL_BLEND), 

    %% No more depth writes...
    gl:depthMask(?GL_FALSE),
    case Shadows of
	true ->
	    gl:enable(?GL_STENCIL_TEST);
	false ->
	    ignore
    end,
    bind_light_shaders(),
    foldl(fun(L,Clear) -> draw_with_shadows(Clear, L, Mat) end, 
	  false, Ligths),
    gl:disable(?GL_STENCIL_TEST),

    gl:depthMask(?GL_TRUE),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:disable(?GL_BLEND),
    ok;

draw_all(RR, RMask) ->
    %% No shadows variant
    wings_view:modelview(true),
    foreach(fun(Data) -> render_redraw(Data, RMask, false) end, 
	    RR#r.data),
    foreach(fun(Data) -> render_redraw(Data, RMask, true) end, 
	    RR#r.data).

draw_with_shadows(true, L, _Mats) ->
    %% New light, clear and update stencil
    gl:clear(?GL_STENCIL_BUFFER_BIT),
    draw_with_shadows(false, L, _Mats);
draw_with_shadows(false, L=#light{sv=Shadow,dl=DLs}, _Mats) ->
    gl:colorMask(?GL_FALSE,?GL_FALSE,?GL_FALSE,?GL_FALSE),

    gl:stencilFunc(?GL_ALWAYS, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:depthFunc(?GL_LESS),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_FRONT),
    {Inc,Dec} = case wings_gl:is_ext({1,4}, ['GL_EXT__stencil_wrap']) of
		    true -> {?GL_INCR_WRAP, ?GL_DECR_WRAP};
		    false -> {?GL_INCR, ?GL_DECR}
		end,
    gl:stencilOp(?GL_KEEP, Inc, ?GL_KEEP),    
    foreach(fun(DL) -> gl:callList(DL) end, Shadow),
    gl:cullFace(?GL_BACK),
    gl:stencilOp(?GL_KEEP, Dec, ?GL_KEEP),
    foreach(fun(DL) -> gl:callList(DL) end, Shadow),

    gl:stencilFunc(?GL_EQUAL, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:stencilOp(?GL_KEEP,?GL_KEEP,?GL_KEEP),
    gl:colorMask(?GL_TRUE,?GL_TRUE,?GL_TRUE,?GL_TRUE),
    gl:depthFunc(?GL_EQUAL),
    setup_light(L),
    gl:enable(?GL_LIGHTING),
    case programmable() of
	true ->
	    enable_shaders();
	false ->
	    skip
    end,
    foreach(fun(DL) -> 
		    case is_transp(DL) of
			true -> 
			    gl:disable(?GL_CULL_FACE),	  
			    gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE);
			false ->
			    gl:enable(?GL_CULL_FACE),
			    gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE)
		    end,
		    gl:callList(get_opaque(DL)) 
	    end, DLs),
    gl:enable(?GL_CULL_FACE),
    gl:depthFunc(?GL_LESS),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE),
    foreach(fun(DL) -> case is_transp(DL) of 
			   true -> gl:callList(get_transp(DL)); 
			   false -> ok 
		       end
	    end, DLs),
    gl:blendFunc(?GL_ONE, ?GL_ONE),
    gl:disable(?GL_LIGHTING),
    disable_shaders(),
    disable_lights(),
    %%    debug_shad(Shadow),
    true.


%%%%%%%%%%%%%%%%%%%%
-ifdef(DEBUG).
debug_shad(Lists) ->
    %% DEBUG draw shadows...
    gl:stencilFunc(?GL_ALWAYS, ?STENCIL_INIT, 16#FFFFFFFF),
    gl:stencilOp(?GL_KEEP,?GL_KEEP,?GL_KEEP),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_SRC_ALPHA),
    gl:color4f(0.0,0.0,1.0,0.5),
    gl:shadeModel(?GL_FLAT),
    %%	  gl:disable(?GL_BLEND),
    gl:depthFunc(?GL_GEQUAL),

    %% Toggle DEPTH TEST to see shadows
    %%    gl:disable(?GL_DEPTH_TEST),  
    %%	  gl:disable(?GL_STENCIL_TEST),
    gl:enable(?GL_CULL_FACE),
    gl:cullFace(?GL_FRONT),
    foreach(fun(DL) -> gl:callList(DL) end, Lists),
    gl:color4f(1.0,0.0,0.0,0.5), 
    gl:cullFace(?GL_BACK),
    foreach(fun(DL) -> gl:callList(DL) end, Lists),
    gl:disable(?GL_CULL_FACE),
    gl:polygonMode(?GL_FRONT_AND_BACK,?GL_FILL),
    true.
-endif.

create_shadow_volume(#light{type=infinite,aim=Aim,pos=LPos}, 
		     #d{we=We,matfs=Matfs}) ->
    LightDir = e3d_vec:norm(e3d_vec:sub(Aim, LPos)),
    {FF,_BF,Loops} = partition_model(We, Matfs, LightDir, dir),
    %% Draw shadow walls.
    foreach(fun(Vs) -> build_shadow_edge_ext_infinite(Vs,LightDir,We) end, Loops),    
    %% Draw Top Cap
    #we{fs=FTab} = We,
    gl:'begin'(?GL_TRIANGLES),
    foreach(fun(Face) -> 
		    Edge = gb_trees:get(Face, FTab),
		    wings_draw_util:unlit_face(Face, Edge, We)
	    end, FF),
    gl:'end'();
create_shadow_volume(#light{pos=LPos},#d{we=We,matfs=Matfs}) ->

    {FF,BF,Loops} = partition_model(We, Matfs, LPos, pos),
    %% Draw walls
    foreach(fun(Vs) -> build_shadow_edge_ext(Vs,LPos,We) end, Loops),
    %% Draw Top Cap
    #we{fs=FTab} = We,
    gl:'begin'(?GL_TRIANGLES),
    foreach(fun(Face) -> 
		    Edge = gb_trees:get(Face, FTab),
		    wings_draw_util:unlit_face(Face, Edge, We)
	    end, FF),
    %% Draw bottom cap
    foreach(fun(Face) -> 
		    Vs = wings_face:vertices_ccw(Face, We),
		    draw_bottom_face(Vs,LPos,We#we.vp) 
	    end, BF),
    gl:'end'().

render_redraw(D, RMask, RenderTrans) ->
    case RMask of
	false -> render_redraw_1(D, RenderTrans);
	true -> render_mask(D)
    end.

render_redraw_1(#d{l=DL}, RenderTrans) ->
    case not is_transp(DL) andalso RenderTrans of
	true ->
	    ok;
	false ->
	    ?CHECK_ERROR(),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    gl:shadeModel(?GL_SMOOTH),
	    gl:enable(?GL_LIGHTING),
	    gl:enable(?GL_POLYGON_OFFSET_FILL),
	    gl:enable(?GL_CULL_FACE),

	    case is_transp(DL) of
		false -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_FALSE);
		true -> gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE, ?GL_TRUE)
	    end,

	    case RenderTrans of
		true ->
		    %% Transparent materials should not update the depth buffer.
		    gl:enable(?GL_BLEND),
		    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
		    gl:depthMask(?GL_FALSE);
		false ->
		    gl:disable(?GL_BLEND),
		    gl:depthMask(?GL_TRUE)
	    end,

	    %% Backsides of opaque objects should be drawn
	    %% if the object has any transparency.
	    case is_transp(DL) andalso not RenderTrans of
		true -> gl:disable(?GL_CULL_FACE);
		false -> gl:enable(?GL_CULL_FACE)
	    end,

	    case RenderTrans of
		false -> wings_dl:call(get_opaque(DL));
		true ->	 wings_dl:call(get_transp(DL))
	    end,
	    gl:depthMask(?GL_TRUE),
	    ?CHECK_ERROR()
    end.

render_mask(#d{f=Dlist}) ->
    ?CHECK_ERROR(),
    gl:enable(?GL_POLYGON_OFFSET_FILL),
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:shadeModel(?GL_FLAT),
    gl:disable(?GL_LIGHTING),
    gl:enable(?GL_CULL_FACE),
    gl:callList(Dlist),
    ?CHECK_ERROR().

accFrustum(Left, Right, Bottom, Top, ZNear, ZFar,
	   Pixdx, Pixdy, Eyedx, Eyedy, Focus) ->
    {_,_,W,H} = wings_wm:viewport(),

    Xwsize = Right - Left,
    Ywsize = Top - Bottom,
    Dx = -(Pixdx*Xwsize/W + Eyedx*ZNear/Focus),
    Dy = -(Pixdy*Ywsize/H + Eyedy*ZNear/Focus),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:frustum(Left+Dx, Right+Dx, Bottom+Dy, Top+Dy, ZNear, ZFar),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:translatef(-Eyedx, -Eyedy, 0).

accPerspective(Fovy, Aspect, ZNear, ZFar,
	       Pixdx, Pixdy, Eyedx, Eyedy, Focus) ->
    Fov = ((Fovy*math:pi())/180)/2,
    Top = ZNear / (math:cos(Fov) / math:sin(Fov)),
    Bottom = -Top,
    Right = Top * Aspect,
    Left = -Right,
    accFrustum(Left, Right, Bottom, Top, ZNear, ZFar,
	       Pixdx, Pixdy, Eyedx, Eyedy, Focus).

jitter(1) ->
    [{0.0,0.0}];
jitter(2) ->
    [{0.25,0.75},{0.75,0.25}];
jitter(3) ->
    [{0.5033922635,0.8317967229},
     {0.7806016275,0.2504380877},
     {0.22261828938,0.4131553612}];
jitter(4) ->
    [{0.375,0.25},{0.125,0.75},{0.875,0.25},{0.625,0.75}];
jitter(5) ->
    [{0.375,0.25},{0.125,0.75},{0.875,0.25},{0.625,0.75}];
jitter(6) ->
    [{0.4646464646,0.4646464646},{0.1313131313,0.7979797979},
     {0.5353535353,0.8686868686},{0.8686868686,0.5353535353},
     {0.7979797979,0.1313131313},{0.2020202020,0.2020202020}];
jitter(8) ->
    [{0.5625,0.4375},{0.0625,0.9375},{0.3125,0.6875},{0.6875,0.8125},
     {0.8125,0.1875},{0.9375,0.5625},{0.4375,0.0625},{0.1875,0.3125}];
jitter(9) ->
    [{0.5,0.5},{0.1666666666,0.9444444444},{0.5,0.1666666666},
     {0.5,0.8333333333},{0.1666666666,0.2777777777},
     {0.8333333333,0.3888888888},{0.1666666666,0.6111111111},
     {0.8333333333,0.7222222222},{0.8333333333,0.0555555555}];
jitter(12) ->
    [{0.4166666666,0.625},{0.9166666666,0.875},{0.25,0.375},
     {0.4166666666,0.125},{0.75,0.125},{0.0833333333,0.125},{0.75,0.625},
     {0.25,0.875},{0.5833333333,0.375},{0.9166666666,0.375},
     {0.0833333333,0.625},{0.583333333,0.875}];
jitter(16) ->
    [{0.375,0.4375},{0.625,0.0625},{0.875,0.1875},{0.125,0.0625},
     {0.375,0.6875},{0.875,0.4375},{0.625,0.5625},{0.375,0.9375},
     {0.625,0.3125},{0.125,0.5625},{0.125,0.8125},{0.375,0.1875},
     {0.875,0.9375},{0.875,0.6875},{0.125,0.3125},{0.625,0.8125}].

%%%
%%% Capture generated image.
%%%

capture(N, Type) ->
    gl:pixelStorei(?GL_PACK_ALIGNMENT, 1),
    gl:readBuffer(?GL_BACK),
    {X,Y,W,H} = wings_wm:viewport(),
    NumBytes = N*W*H,
    Mem = sdl_util:alloc(NumBytes, ?GL_UNSIGNED_BYTE),
    gl:readPixels(X, Y, W, H, Type, ?GL_UNSIGNED_BYTE, Mem),
    Pixels = sdl_util:getBin(Mem),
    #e3d_image{bytes_pp=N,order=lower_left,width=W,height=H,image=Pixels}.

combine_images(#e3d_image{image=Pixels0}=Image, #e3d_image{image=Mask}) ->
    Pixels = combine_img_1(Pixels0, Mask, []),
    Image#e3d_image{type=b8g8r8a8,bytes_pp=4,image=Pixels}.

combine_img_1(<<RGB:768/binary,P/binary>>, 
	      <<A:256/binary,M/binary>>, 
	      Acc) ->
    Chunk = combine_img_2(binary_to_list(RGB), binary_to_list(A), []),
    combine_img_1(P, M, [Chunk|Acc]);
combine_img_1(<<R:8,G:8,B:8,P/binary>>, <<A:8,M/binary>>, Acc) ->
    combine_img_1(P, M, [<<B:8,G:8,R:8,A:8>>|Acc]);
combine_img_1(<<>>, <<>>, Acc) ->
    list_to_binary(reverse(Acc)).

combine_img_2([R,G,B|P], [A|M], Acc) ->
    combine_img_2(P, M, [[B,G,R,A]|Acc]);
combine_img_2([], [], Acc) -> list_to_binary(reverse(Acc)).

%% Utilities
setup_projection_matrix() ->
    #view{hither=Near} = wings_view:current(),
    [ M0, M4, M8, M12,
      M1, M5, M9, M13,
      M2, M6, _M10,_M14,
      M3, M7, _M11,M15] =
	gl:getDoublev(?GL_PROJECTION_MATRIX),
    %% Patch to get infinitive far plane
    Infinite = [ M0, M4, M8,  M12,
		 M1, M5, M9,  M13,
		 M2, M6, -1.0,-1.0,
		 M3, M7, -2.0*Near ,M15],
    gl:matrixMode(?GL_PROJECTION),
    gl:loadMatrixd(Infinite),
    gl:matrixMode(?GL_MODELVIEW).

%% All vertices must be backfacing for the face to be backfaced.
frontfacing([],_) ->  false;
frontfacing([[_|{_,_,N1}]|RN],[L1|RL]) when tuple(N1) ->
    case e3d_vec:dot(N1, L1) >= 0 of
	true -> % Backfacing
	    frontfacing(RN,RL);
	false ->
	    true
    end;
frontfacing([[_|N1]|RN],[L1|RL]) ->
    case e3d_vec:dot(N1, L1) >= 0 of
	true -> % Backfacing
	    frontfacing(RN,RL);
	false ->
	    true
    end.

%% Returns {FacesFacingLigth,FacesAwayFromLigth,CW_VsLoops}
partition_model(We, Matfs, LPos, DirOrPos) ->
    Partition=
	fun({Face, Ns}, {FF,BF,EL}) ->
		IsFF = case DirOrPos of
			   dir ->
			       LL = lists:duplicate(length(Ns),LPos),
			       frontfacing(Ns,LL);
			   _ ->
			       VPs = wings_face:vertex_positions(Face,We),
			       LDs = [e3d_vec:norm(e3d_vec:sub(VP,LPos))
				      || VP <- VPs],
			       frontfacing(Ns,LDs)
		       end,
		case IsFF of
		    false -> %% BackFacing
			{FF,[Face|BF],EL};
		    true -> %% FrontFacing
			Es = wings_face:fold(fun(_V,Edge,_E,Es) ->
						     [{Edge,Face}|Es]
					     end, EL, Face, We),
			{[Face|FF],BF,Es}
		end
	end,
    Cluster = fun({_Mat,List}, A0) ->
		      foldl(Partition, A0, List)
	      end,
    {FF,BF,EL} = foldl(Cluster, {[],[],[]}, Matfs),
    Eds	  = outer_edges_1(lists:sort(EL), []),	 
    Loops = case wings_edge_loop:edge_loop_vertices(Eds,We) of
		none -> [];
		Ls -> 
		    %% Fix cw order
		    [fix_cw_order(Loop, FF, We) || Loop <- Ls]
	    end,
    {FF,BF,Loops}.
outer_edges_1([{E,_},{E,_}|T], Out) ->
    outer_edges_1(T, Out);
outer_edges_1([{E,_F}|T], Out) -> %% Only the edges I'm interested of
    outer_edges_1(T, [E|Out]);
outer_edges_1([], Out) -> reverse(Out).

fix_cw_order(Vs = [V1,V2|_], FF, We = #we{es=Etab}) ->
    Eds = wings_edge:from_vs([V1,V2],We),
    E = get_edge(lists:sort(Eds)),
    case gb_trees:get(E,Etab) of
	#edge{vs=V1,ve=V2,lf=Face} -> 
	    case lists:member(Face, FF) of
		false ->
		    Vs;
		true ->
		    lists:reverse(Vs)
	    end;
	#edge{vs=V2,ve=V1, lf=Face} ->
	    case lists:member(Face, FF) of
		false ->
		    lists:reverse(Vs);
		true ->
		    Vs
	    end
    end.

get_edge([A,A|_]) -> A;
get_edge([_|R]) -> get_edge(R).

%%%%%%%%% Shadow Building functions %%%% 
%% Edge loop is input, see wings_edge_loop:edge_loop_vertices/2
build_shadow_edge_ext_infinite([V0|_] = Vs, {X,Y,Z}, #we{vp=Vtab}) ->
    gl:'begin'(?GL_TRIANGLE_FAN),
    gl:vertex4f(X,Y,Z,0.0),
    foreach(fun(V) -> gl:vertex3fv(gb_trees:get(V, Vtab)) end, Vs),
    gl:vertex3fv(gb_trees:get(V0, Vtab)),
    gl:'end'().

build_shadow_edge_ext(Vs0, L, #we{vp=VP}) ->
    Vs = [V0|_] = lists:reverse(Vs0),
    gl:'begin'(?GL_QUAD_STRIP),
    build_shadow_edge_ext(Vs, V0, L, VP).
build_shadow_edge_ext([V|Vs], V0, L, Vtab) ->
    Vp = gb_trees:get(V, Vtab),
    gl:vertex3fv(Vp),
    {X,Y,Z} = e3d_vec:sub(Vp,L),
    gl:vertex4f(X,Y,Z,0.0),
    build_shadow_edge_ext(Vs,V0,L,Vtab);
build_shadow_edge_ext([], V, L, Vtab) ->
    Vp = gb_trees:get(V, Vtab),
    gl:vertex3fv(Vp),
    {X,Y,Z} = e3d_vec:sub(Vp,L),
    gl:vertex4f(X,Y,Z,0.0),
    gl:'end'().

draw_bottom_face([V1,V2,V3|_BUG],LPos,Vtab) ->
    draw_bottom_face(gb_trees:get(V1,Vtab),LPos),
    draw_bottom_face(gb_trees:get(V2,Vtab),LPos),
    draw_bottom_face(gb_trees:get(V3,Vtab),LPos).

draw_bottom_face(V,L) ->
    {X,Y,Z} = e3d_vec:sub(V,L),
    gl:vertex4f(X,Y,Z,0.0).

%%% Lights 

disable_lights() ->
    disable_lights(?GL_LIGHT0).
disable_lights(Lnum) when Lnum > ?GL_LIGHT7 -> 
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, {0.0,0.0,0.0,1.0}),
    ok;
disable_lights(Lnum) ->
    gl:disable(Lnum),
    disable_lights(Lnum+1).

setup_amb(none) ->    ok;
setup_amb(Amb) ->
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, Amb).

setup_light(#light{type=infinite,aim=Aim,pos=Pos,attr=L}) ->
    {X,Y,Z} = e3d_vec:norm(e3d_vec:sub(Pos,Aim)),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {X,Y,Z,0}),
    gl:lightf(?GL_LIGHT0, ?GL_LINEAR_ATTENUATION, 0.0),
    gl:lightf(?GL_LIGHT0, ?GL_QUADRATIC_ATTENUATION, 0.0),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 180.0),
    case programmable() of 
	true ->
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,1,X,Y,Z,0.0),
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,2,-X,-Y,-Z,1.0);
	false ->   skip
    end,
    setup_light_attr(L);
setup_light(#light{type=point,pos={X,Y,Z},attr=L}) ->
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, 180.0),
    case programmable() of 
	true ->
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,1,X,Y,Z,1.0),
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,2,-1.0,-1.0,-1.0,1.0);
	false ->   skip
    end,
    setup_light_attr(L);
setup_light(#light{type=spot,aim=Aim,pos=Pos={X,Y,Z},attr=L}) ->
    Dir = {Sx,Sy,Sz} = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightfv(?GL_LIGHT0, ?GL_SPOT_DIRECTION, Dir),
    case programmable() of 
	true ->
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,1,X,Y,Z,1.0),
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,2,Sx,Sy,Sz,1.0);
	false -> skip
    end,
    setup_light_attr(L);
setup_light(#light{type=area,aim=Aim,pos=Pos={X,Y,Z},attr=L}) ->
    Dir = {Sx,Sy,Sz} = e3d_vec:norm(e3d_vec:sub(Aim, Pos)),
    gl:lightfv(?GL_LIGHT0, ?GL_POSITION, {X,Y,Z,1}),
    gl:lightfv(?GL_LIGHT0, ?GL_SPOT_DIRECTION, Dir),
    case programmable() of 
	true ->
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,1,X,Y,Z,1.0),
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,2,Sx,Sy,Sz,1.0);
	false -> skip
    end,
    setup_light_attr(L).

setup_light_attr([]) ->
    gl:enable(?GL_LIGHT0);
setup_light_attr([{diffuse,Col}|As]) ->
    gl:lightfv(?GL_LIGHT0, ?GL_DIFFUSE, Col),
    setup_light_attr(As);
setup_light_attr([{specular,Col}|As]) ->
    gl:lightfv(?GL_LIGHT0, ?GL_SPECULAR, Col),
    setup_light_attr(As);
setup_light_attr([{ambient,_Col}|As]) -> %% No ambient from these right
    gl:lightfv(?GL_LIGHT0, ?GL_AMBIENT, {0.0,0.0,0.0,1.0}),
    setup_light_attr(As);
setup_light_attr([{cone_angle,Angle}|As]) ->
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_CUTOFF, Angle),
    setup_light_attr(As);
setup_light_attr([{spot_exponent,Exp}|As]) ->
    gl:lightf(?GL_LIGHT0, ?GL_SPOT_EXPONENT, Exp),
    setup_light_attr(As);
setup_light_attr([{linear_attenuation,Att}|As]) ->
    gl:lightf(?GL_LIGHT0, ?GL_LINEAR_ATTENUATION, Att),
    setup_light_attr(As);
setup_light_attr([{quadratic_attenuation,Att}|As]) ->
    gl:lightf(?GL_LIGHT0, ?GL_QUADRATIC_ATTENUATION, Att),
    setup_light_attr(As);
setup_light_attr([_|As]) ->
    setup_light_attr(As).

%% Bump helpers

bump_exts() ->
    ['GL_ARB_texture_cube_map',
     'GL_ARB_multitexture',
     'GL_ARB_texture_env_combine',
     'GL_ARB_texture_env_dot3',
     'GL_EXT_texture3D'].

programmable() ->
    wings_gl:is_ext(['GL_ARB_vertex_program', 
		     'GL_ARB_fragment_program']).
%    false.

%% Bump drawings 
create_bumped(#d{l=DL}, false, _,_) -> DL;  % No Bumps use default DL
create_bumped(#d{l=DL,we=We}, _,_,_)	    % Vertex mode can't have bumps.
  when We#we.mode == vertex -> DL;
create_bumped(#d{matfs=MatFs,l=DL,we=We},true,Mtab,#light{pos=LPos, type=Type}) -> 
    case programmable() of
	true -> DL;
	false -> 
	    LType = case Type of 
			infinite -> dir;
			_ -> pos
		    end,
	    draw_faces(MatFs, We, Mtab, {LType,LPos})
    end.

mult_inverse_model_transform(Pos) ->
    #view{origin=Origin,distance=Dist,azimuth=Az,
	  elevation=El,pan_x=PanX,pan_y=PanY} = wings_view:current(),
    M0 = e3d_mat:translate(e3d_vec:neg(Origin)),
    M1 = e3d_mat:mul(M0, e3d_mat:rotate(-Az, {0.0,1.0,0.0})),
    M2 = e3d_mat:mul(M1, e3d_mat:rotate(-El, {1.0,0.0,0.0})),
    M =  e3d_mat:mul(M2, e3d_mat:translate(-PanX, -PanY, Dist)),
    {M,e3d_mat:mul_point(M, Pos)}.

draw_faces(MFlist, We, Mtab, L) ->    
    ListOp = gl:genLists(1),
    gl:newList(ListOp, ?GL_COMPILE),
    Trans = draw_smooth_opaque(MFlist, Mtab, We, L, []),
    gl:endList(),
    if
	Trans =:= [] ->
	    {[ListOp,none],false};
	true ->
	    ListTr = gl:genLists(1),
	    gl:newList(ListTr, ?GL_COMPILE),
	    draw_smooth_tr(MFlist, Mtab, We, L),
	    gl:endList(),
	    {[ListOp,ListTr],true}
    end.

draw_smooth_opaque([{M,Faces}=MatFaces|T], Mtab, We, L, Acc) ->
    case wings_material:is_transparent(M, Mtab) of
	true ->
	    draw_smooth_opaque(T, Mtab, We, L, [MatFaces|Acc]);
	false ->
	    do_draw_smooth(M, Faces, We, L, Mtab),
	    draw_smooth_opaque(T, Mtab, We, L, Acc)
    end;
draw_smooth_opaque([], _, _, _, Acc) -> Acc.

draw_smooth_tr([{M,Faces}|T], Mtab, We, L) ->
    do_draw_smooth(M, Faces, We, L, Mtab),
    draw_smooth_tr(T, Mtab, We, L);
draw_smooth_tr([], _, _, _) -> ok.

do_draw_smooth(_, Faces, #we{mode = vertex}=We,_,Mtab) ->
    disable_shaders(),
    wings_material:apply_material(default, Mtab),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:colorMaterial(?GL_FRONT_AND_BACK, ?GL_AMBIENT_AND_DIFFUSE),
    gl:'begin'(?GL_TRIANGLES),
    draw_vtx_color(Faces, We),
    gl:'end'(),
    enable_shaders(),
    gl:disable(?GL_COLOR_MATERIAL);

do_draw_smooth(MatN, Faces, We, L, Mtab) ->
    gl:pushAttrib(?GL_TEXTURE_BIT),
    Mat = gb_trees:get(MatN, Mtab),
    case L of
	skip ->
	    IsTxMat = wings_material:apply_material(MatN, Mtab),
	    gl:'begin'(?GL_TRIANGLES),
	    do_draw_faces(Faces, IsTxMat, We),
	    gl:'end'(),
	    gl:popAttrib();
	Pable ->
	    apply_bumped_mat(Mat, Pable),
	    gl:'begin'(?GL_TRIANGLES),
	    do_draw_bumped(Faces, true, We, Pable),
	    gl:'end'(),
	    disable_bumps(),
	    gl:popAttrib()
    end.

draw_vtx_color([], _) -> ok;
draw_vtx_color([{Face,[[Col1|N1],[Col2|N2],[Col3|N3]|_BUG]}|Faces], We) ->
    [V1,V2,V3|_] = wings_face:vertex_positions(Face, We),
    gl:normal3fv(N1),
    glcolor3fv(Col1),
    gl:vertex3fv(V1),

    gl:normal3fv(N2),
    glcolor3fv(Col2),
    gl:vertex3fv(V2),

    gl:normal3fv(N3),
    glcolor3fv(Col3),
    gl:vertex3fv(V3),
    draw_vtx_color(Faces,We).

do_draw_faces([],_,_) -> ok;
%%% Unprogrammable cards..
do_draw_faces([{Face, [[UV1|TBN1],[UV2|TBN2],[UV3|TBN3]|_BUG]}|Fs],Tex, We) ->
    [V1,V2,V3|_] = wings_face:vertex_positions(Face, We),
    gl:normal3fv(element(3, TBN1)),
    texCoord(UV1, Tex),
    gl:vertex3fv(V1),

    gl:normal3fv(element(3, TBN2)),
    texCoord(UV2,Tex),
    gl:vertex3fv(V2),

    gl:normal3fv(element(3, TBN3)),
    texCoord(UV3,Tex),
    gl:vertex3fv(V3),
    do_draw_faces(Fs, Tex, We).

do_draw_bumped([], _,_,_) -> ok;
do_draw_bumped([{Face, [[UV1|TBN1],[UV2|TBN2],[UV3|TBN3]|_BUG]}|Fs], 
	       Txt, We, pable) ->
    [V1,V2,V3|_] = wings_face:vertex_positions(Face, We),
    gl:normal3fv(element(3, TBN1)),
    gl:multiTexCoord3fv(?GL_TEXTURE0, element(1,TBN1)),
    texCoord(UV1, true, ?GL_TEXTURE1),
    %%	  io:format("V=~p.~p~n",[V1,[UV1|N1]]),
    gl:vertex3fv(V1),

    gl:normal3fv(element(3, TBN2)),
    gl:multiTexCoord3fv(?GL_TEXTURE0, element(1,TBN2)),
    texCoord(UV2, true, ?GL_TEXTURE1),
    %%	  io:format("V=~p.~p~n",[V2,[UV2|N2]]),
    gl:vertex3fv(V2),

    gl:normal3fv(element(3, TBN3)),
    gl:multiTexCoord3fv(?GL_TEXTURE0, element(1,TBN3)),
    texCoord(UV3, true, ?GL_TEXTURE1),
    %%	  io:format("V=~p.~p~n",[V3,[UV3|N3]]),
    gl:vertex3fv(V3),
    do_draw_bumped(Fs, Txt, We, pable);
do_draw_bumped([{Face, [[UV1|TBN1],[UV2|TBN2],[UV3|TBN3]|_BUG]}|Fs], 
	       Txt, We, L) ->
    [V1,V2,V3|_] = wings_face:vertex_positions(Face, We),
    gl:normal3fv(element(3, TBN1)),
    texCoord(UV1, true, ?GL_TEXTURE1),
    %%    io:format("V=~p.~p~n",[V1,[UV1|N1]]),
    bumpCoord(TBN1, V1, L),
    texCoord(UV1, Txt,	?GL_TEXTURE3),
    gl:vertex3fv(V1),

    gl:normal3fv(element(3, TBN2)),
    texCoord(UV2, true, ?GL_TEXTURE1),
    %%    io:format("V=~p.~p~n",[V2,[UV2|N2]]),
    bumpCoord(TBN2, V2, L),
    texCoord(UV2, Txt,	?GL_TEXTURE3),
    gl:vertex3fv(V2),

    gl:normal3fv(element(3, TBN3)),
    texCoord(UV3, true, ?GL_TEXTURE1),
    %%    io:format("V=~p.~p~n",[V3,[UV3|N3]]),
    bumpCoord(TBN3, V3, L),
    texCoord(UV3, Txt,	?GL_TEXTURE3),
    gl:vertex3fv(V3),
    do_draw_bumped(Fs, Txt, We, L).

glcolor3fv(Col) when size(Col) == 3 ->
    gl:color3fv(Col);
glcolor3fv(_) ->
    gl:color3f(1.0,1.0,1.0).

texCoord(UV = {_,_}, true) ->
    gl:texCoord2fv(UV);
texCoord(_, true) ->
    gl:texCoord2fv({0.0,0.0});
texCoord(_, false) -> ok.
texCoord(UV = {_,_}, true, Unit) ->
    gl:multiTexCoord2fv(Unit, UV);
texCoord(_, true, Unit) ->
    gl:multiTexCoord2fv(Unit, {0.0,0.0});
texCoord(_, false, _) ->
    ok.

default_uv(UV = {_,_}) -> UV;	 
default_uv(_) -> {0.0,0.0}.

calcTS(V1,V2,V3,UV1,UV2,UV3,N) ->
    {S1,T1} = default_uv(UV1),
    {S2,T2} = default_uv(UV2),
    {S3,T3} = default_uv(UV3),
    if S1 == S2;
       S3 == S2;
       T1 == T2;
       T3 == T2 ->
	    calcTS(V1,V2,V3,N);
       true ->
	    Side1 = e3d_vec:sub(V1,V2),
	    Side2 = e3d_vec:sub(V3,V2),
	    DT1 = T1-T2,
	    DT2 = T3-T2,
	    Stan1 = e3d_vec:norm(e3d_vec:sub(e3d_vec:mul(Side1,DT2),
					     e3d_vec:mul(Side2,DT1))),	    
	    DS1 = S1-S2,
	    DS2 = S3-S2,
	    Ttan1 = e3d_vec:norm(e3d_vec:sub(e3d_vec:mul(Side1,DS2),
					     e3d_vec:mul(Side2,DS1))),
	    %% OK we the 2 tangents but cross them again with normal
	    %% so they are orthogonal
	    Stan  = e3d_vec:norm(e3d_vec:cross(Ttan1,N)),
	    Ttan  = e3d_vec:norm(e3d_vec:cross(Stan1,N)),
	    check_coordsys(Stan,Ttan,N)
    end.

calcTS(V10,V20,V30,N) ->
    %% We don't have good uv's to base our calcs on 
    %% so fix something that looks ok.
    %% We pick one vertex of the face
    %% we must always pick the same vertex when checking a face
    %% to get the coordinate system in a consistent dir
    [O,V1,V2] = lists:sort([V10,V20,V30]),
    S1 = e3d_vec:sub(V1,O),
    S2 = e3d_vec:sub(V2,O),
    Ls1 = e3d_vec:dot(S1,S1),
    Ls2 = e3d_vec:dot(S2,S2),
    Dir = if Ls1 > Ls2 -> S1;
	     true -> S2
	  end,
    V = Dir,
    Bi = e3d_vec:norm(e3d_vec:cross(V, N)),
    T  = e3d_vec:norm(e3d_vec:cross(Bi, N)),
    check_coordsys(T, Bi, N).

check_coordsys(Stan,Ttan,N) ->
    Check = e3d_vec:dot(e3d_vec:cross(Stan,Ttan),N),
    if 
	Check < 0.0 ->
	    {e3d_vec:mul(Stan,-1.0),e3d_vec:mul(Ttan,-1.0), N};
	true ->
	    {Stan,Ttan,N}
    end.
		   
bumpCoord({Vx,Vy,Vn}, Vpos, {Type,InvLight0}) ->
    LDir = case Type of
	       dir ->
		   InvLight0;
	       pos -> 
		   e3d_vec:sub(InvLight0,Vpos)
	   end,
    InvLight = e3d_vec:norm(LDir),
    %% Calc orthonormal space per vertex.
    %% Norm RGB could (should) be done with a normal-cube map
    X = e3d_vec:dot(Vx,InvLight),
    Y = e3d_vec:dot(Vy,InvLight),
    Z = e3d_vec:dot(Vn,InvLight),
    gl:multiTexCoord3f(?GL_TEXTURE0, X,Y,Z).

apply_bumped_mat(Mat, Programmable) ->
    OpenGL = proplists:get_value(opengl, Mat),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_DIFFUSE, 
		  proplists:get_value(diffuse, OpenGL)), 
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_AMBIENT, 
		  proplists:get_value(ambient, OpenGL)),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_SPECULAR, 
		  proplists:get_value(specular, OpenGL)),
    Shine = proplists:get_value(shininess, OpenGL)*128,
    gl:materialf(?GL_FRONT_AND_BACK, ?GL_SHININESS, Shine),
    gl:materialfv(?GL_FRONT_AND_BACK, ?GL_EMISSION, 
		  proplists:get_value(emission, OpenGL)),    
    [{diffuse, DefDiff},{bump,DefBump},{gloss,DefGloss}] = 
	wings_image:default(all),
    Maps = proplists:get_value(maps, Mat),
    Diffuse = case proplists:get_value(diffuse, Maps, none) of
		  none -> DefDiff;
		  Diff0 -> wings_image:txid(Diff0)
	      end,
    Bump    = case proplists:get_value(normal, Maps, none) of
		  none -> 
		      case proplists:get_value(bump, Maps, none) of
			  none -> DefBump;
			  Bump0 -> wings_image:bumpid(Bump0)
		      end;
		  Bump0 -> wings_image:bumpid(Bump0)
	      end,
    Gloss   = case proplists:get_value(gloss, Maps, none) of
		  none -> DefGloss;
		  Gloss0 -> wings_image:txid(Gloss0)
	      end,
    activate_textures(Diffuse,Bump,Gloss,Programmable).

activate_textures(Diffuse,Bump,Gloss,pable) ->
    gl:activeTexture(?GL_TEXTURE1),
    gl:enable(?GL_TEXTURE_2D),	   
    gl:bindTexture(?GL_TEXTURE_2D,Bump),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:activeTexture(?GL_TEXTURE2),
    gl:enable(?GL_TEXTURE_2D),	   
    gl:bindTexture(?GL_TEXTURE_2D,Gloss),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:activeTexture(?GL_TEXTURE0),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D,Diffuse),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE);

activate_textures(Diffuse,Bump,_,_) ->  % Non programable hardware
    %% Start by creating bump data
    CubeMap = wings_image:normal_cubemapid(),
    gl:activeTexture(?GL_TEXTURE0),
    gl:enable(?GL_TEXTURE_CUBE_MAP),
    gl:bindTexture(?GL_TEXTURE_CUBE_MAP,CubeMap),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB, ?GL_REPLACE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB, ?GL_TEXTURE),

    gl:activeTexture(?GL_TEXTURE1),
    gl:enable(?GL_TEXTURE_2D),	   
    gl:bindTexture(?GL_TEXTURE_2D,Bump),

    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB,  ?GL_DOT3_RGB),

    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB,  ?GL_PREVIOUS),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE1_RGB,  ?GL_TEXTURE),

    %% Modulate with material (diffuse)
    gl:activeTexture(?GL_TEXTURE2),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Bump), %% Needs some texture here

    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB,      ?GL_MODULATE),	
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB,      ?GL_PRIMARY_COLOR),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE1_RGB,      ?GL_PREVIOUS),

    %% Modulate with diffuse texture
    gl:activeTexture(?GL_TEXTURE3),
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Diffuse), 
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_COMBINE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_COMBINE_RGB,  ?GL_MODULATE),    
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE0_RGB,  ?GL_TEXTURE),
    gl:texEnvi(?GL_TEXTURE_ENV, ?GL_SOURCE1_RGB,  ?GL_PREVIOUS).    

%% norm_rgb(V1, V2, V3) when is_float(V1), is_float(V2), is_float(V3) ->
%%     D = math:sqrt(V1*V1+V2*V2+V3*V3),
%%     case catch {0.5+V1/D*0.5,0.5+V2/D*0.5,0.5+V3/D*0.5} of
%% {'EXIT',_} -> {0.0,0.0,0.0};
%% R -> R
%%     end.

disable_bumps() ->
    gl:activeTexture(?GL_TEXTURE0),
    gl:disable(?GL_TEXTURE_CUBE_MAP),
    gl:activeTexture(?GL_TEXTURE1),
    gl:disable(?GL_TEXTURE_2D),
    gl:activeTexture(?GL_TEXTURE2),
    gl:disable(?GL_TEXTURE_2D),
    gl:activeTexture(?GL_TEXTURE3),
    gl:disable(?GL_TEXTURE_2D).

bind_light_shaders() ->    
    case get(gl_shaders) of
	{Vp,Fp,_Amb} ->	    
	    gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,Fp),
	    gl:bindProgramARB(?GL_VERTEX_PROGRAM_ARB,Vp);
	_ -> skip
    end.
bind_ambient_shaders() ->    
    case get(gl_shaders) of
	{_Vp,_Fp,Amb} ->	    
	    gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,Amb);
	_ -> 
	    skip
    end.

enable_shaders() ->
    case get(gl_shaders) of
	{_Vp,_Fp,_Amb} ->
	    {_InvMV,{Cx,Cy,Cz}} = mult_inverse_model_transform({0.0,0.0,0.0}),
	    gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
	    gl:enable(?GL_VERTEX_PROGRAM_ARB),
	    gl:programEnvParameter4fARB(?GL_VERTEX_PROGRAM_ARB,0,Cx,Cy,Cz,0);
	_ ->
	    ok
    end.

disable_shaders() ->
    case get(gl_shaders) of
	{_Vp,_Fp,_Amb} ->
	    gl:disable(?GL_VERTEX_PROGRAM_ARB),
	    gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
	    ok;
	_ ->
	    ok
    end.

load_shaders() ->
    VP = load_program(?GL_VERTEX_PROGRAM_ARB,vertex_prog()),
    FP = load_program(?GL_FRAGMENT_PROGRAM_ARB,fragment_prog()),
    Amb = load_program(?GL_FRAGMENT_PROGRAM_ARB,ambient_prog()),
    Shaders = {VP,FP,Amb},
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
	    io:format(?__(1,"Error when loading program")++": ~s\n", [Err]),
	    exit(prog_error)
    end.   

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
    

vertex_prog() ->
    <<"!!ARBvp1.0

PARAM mvp[4]  = { state.matrix.mvp };
PARAM camera  = program.env[0];
PARAM light   = program.env[1];
PARAM spotdir = program.env[2];

ATTRIB xyz = vertex.position;
ATTRIB normal = vertex.normal;
ATTRIB tangent = vertex.texcoord[0];
ATTRIB st = vertex.texcoord[1];

TEMP pos, dir, binormal, ldist2, temp, cam;

XPD binormal, tangent, normal;

DP4 result.position.x, mvp[0], xyz;
DP4 result.position.y, mvp[1], xyz;
DP4 result.position.z, mvp[2], xyz;
DP4 result.position.w, mvp[3], xyz;

# Check for infinite light
SGE pos.w, light.w, 0.5;
MUL pos, xyz, pos.w;
SUB dir, light, pos;
# Calc vector 
DP3 ldist2.x, dir, dir;
RSQ dir.w, ldist2.x;
MUL dir.xyz, dir, dir.w;

DP3 result.texcoord[1].x, tangent, dir;
DP3 result.texcoord[1].y, binormal, dir;
DP3 result.texcoord[1].z, normal, dir;
# Put dist*dist in w
MOV result.texcoord[1].w, ldist2.x;

# Calc spotdir dot3 ligthdir to get spot falloff
DP3 ldist2.y, dir, -spotdir;

SUB cam, camera, xyz;
DP3 cam.w, cam, cam;
RSQ cam.w, cam.w;
MUL cam.xyz, cam, cam.w;
ADD dir, cam, dir;
DP3 dir.w, dir, dir;
RSQ dir.w, dir.w;
MUL dir.xyz, dir, dir.w;

DP3 result.texcoord[2].x, tangent, dir;
DP3 result.texcoord[2].y, binormal, dir;
DP3 result.texcoord[2].z, normal, dir;
# Store spot falloff
MOV result.texcoord[2].w, ldist2.y;

MOV result.texcoord[0], st;

END
">>.

fragment_prog() ->
    <<"!!ARBfp1.0

PARAM param = { 2.0, -1.0, 0.0, 0.0 };

PARAM matdiff  = state.material.diffuse;
PARAM matspec  = state.material.specular;
PARAM matemm   = state.material.emission;
PARAM matshin  = state.material.shininess;

PARAM ldiff = state.light[0].diffuse;
PARAM lspec = state.light[0].specular;

TEMP base, dot3, gloss, color, diffuse, specular, lvec, att, temp;

# Attenuation
MOV temp.w, fragment.texcoord[1].w;
RSQ temp.x, temp.w;
DST temp, temp.w, temp.x;  # temp = {1,d,d^2,1/d}
DP3 att.w, temp, state.light[0].attenuation;
RCP att.w, att.w;

# Spot attenution
MOV temp.y, fragment.texcoord[2].w;
ADD temp.x, temp.y, -state.light[0].spot.direction.w;
MOV temp.w, state.light[0].attenuation.w;
LIT temp, temp;
# if spot used i.e. tex.w > 0.999 
SGE temp.x, fragment.texcoord[2].w, 0.999;
SUB temp.y, 1.0, temp.z;
MAD temp.z, temp.x, temp.y, temp.z;
# mul with temp.z
MUL att, att.w, temp.z;

TEX base,  fragment.texcoord[0], texture[0], 2D;
TEX dot3,  fragment.texcoord[0], texture[1], 2D;
TEX gloss, fragment.texcoord[0], texture[2], 2D;

# ReCalc RGB to XYZ 
MAD dot3, dot3, param.x, param.y;
# Normalize
DP3 dot3.w, dot3, dot3;
RSQ dot3.w, dot3.w;
MUL dot3.xyz, dot3, dot3.w;

# Multiply Bump with ligth vectors 
MOV lvec, fragment.texcoord[1];
DP3 lvec.w, lvec, lvec;
RSQ lvec.w, lvec.w;
MUL lvec.xyz, lvec, lvec.w;

DP3_SAT diffuse, lvec, dot3;

MOV lvec, fragment.texcoord[2];
DP3 lvec.w, lvec, lvec;
RSQ lvec.w, lvec.w;
MUL lvec.xyz, lvec, lvec.w;

DP3_SAT specular, lvec, dot3;

POW specular, specular.x, matshin.x;

MUL specular, specular, gloss;
MUL specular, specular, matspec;
MUL specular, specular, lspec;
MUL color, base, matdiff;
MUL color, color, ldiff;

MAD color, color, diffuse, specular;
MAD color.xyz, color, att.x, matemm;
MOV result.color, color;

END
">>.


ambient_prog() ->
    <<"!!ARBfp1.0

PARAM matamb  = state.material.ambient;
PARAM lamb = state.lightmodel.ambient;
TEMP color,tex;
MUL color, matamb, lamb;
TEX tex, fragment.texcoord[0], texture[0], 2D;
MUL result.color, color, tex;

END
">>.
