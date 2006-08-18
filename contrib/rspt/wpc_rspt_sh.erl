%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_sh.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

-module(wpc_rspt_sh).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

%% must be the same as in wpc_rspt_re
%-define(CULL_FRONT,?GL_BACK).
%-define(CULL_BACK,?GL_FRONT).
-define(CULL_FRONT,?GL_FRONT).
-define(CULL_BACK,?GL_BACK).

-export([init/0,menu/2,command/2]).
-export([shadow/4]).
init()->
	true.

menu(X,Menu)->
	Menu.

command(_,_)->
	next.


%%%%%%
%%%%%% Version 09 of light, info from wings_light.erl
%%%%%%
-record(light,
	{type,
	diffuse={1.0,1.0,1.0,1.0},
	ambient={0.0,0.0,0.0,1.0},
	specular={1.0,1.0,1.0,1.0},
	aim,                      % aim spot
	lin_att,
	quad_att,
	spot_angle,
	spot_exp,
	prop=[]                    % extra properties whoo-hoo what does wings handle here ???
	}).

%%
%%Top of shadow control 
%%Switch on type of light
%% incorrectly named, does not use the stencil shadow ..
%%
shadow(#we{light=#light{type=ambient}},St,Scene,ExtObj)->
	%% disable stencil test, do nothing 
	ok;
shadow(#we{light=#light{type=spot}}=WE,St,Scene,ExtObj)->
	
	shadow_spotlight(WE,St,Scene,ExtObj),
	%% build the shadow ...
	%% render into stencil where ??
	%% set stencil functions ...
	ok;
shadow(#we{light=#light{type=point}}=WE,St,Scene,ExtObj)->
	gl:clear(?GL_COLOR_BUFFER_BIT),
	%% bail out
	bail_out;
shadow(#we{light=#light{type=Kind}}=WE,St,Scene,ExtObj)->
	io:format("Ignoring unknown light kind ~p ~n",[Kind]).


%%
%%Build shadow and set stencil using spotlight
%%
shadow_spotlight(WE,St,Scene,ExtObj)->
	%%io:format("Rendering shadow of spotlight ... ~p ~n",[WE]),
	%% recompile materials to white, should realy be alpha tex only .. ? hmm
	ExtObj(mat_none),
	shadow_view_depth_textured(WE,St,Scene,ExtObj),
	ok.

%%
%%Render shadow view frame
%%Store in new texture (gl:genTexture(...))
%%Reconstruct Z-buffer data (render scene with clear etc)
%%Reload accum data (could replace above with recall z-buffer)
%%Apply shadow buffer how ?? (check ) 
%%Render scene as (normal)
%%


%%
%%Needs to be  modified to accomedate more maps
%%(needs tex projection transform)
%%
%%Inverse to texture coordinates
%%Inverse here means going towards wolrd coordinates ...
%%
%%
invert_texgen(#we{vp=VP,light=Light}=WE,St,ExtObj,ViewNumber)->
	
	%%io:format("INvert texgen ~n",[]),

	%%
	%%Can be done differently
	%%

	%% fix one part of transform
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),
	setup_camera_view(ExtObj),
	
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	%% was 0.5,0.5
	gl:scalef(0.5,0.5,0.5),
	gl:translatef(1,1,1),
	
	%% transform to camera view (shadow light)
	%%io:format("INvert texgen aim ~n",[]),
	#light{aim={A1,A2,A3},spot_angle=Angle} = Light,

	%% should be modified to accomedate more maps
	%% added part of the projection matrix
	%%[W_Hither,W_Yon] = wpa:camera_info([hither,yon]),
	% should W_Hither and W_Yon be used here ?
	%%glu:perspective(2*Angle,1.0,W_Hither,W_Yon),
	%%io:format("invert_texgen viewNumber=~p~n",[ViewNumber]),
	projection(Angle,ViewNumber),

	%%io:format("INvert texgen origin ~n",[]),
	{X1,X2,X3} = Origin = get_average(VP),
	%% need to fix the up vector (skip lookAt ??)
	%%{U1,U2,U3} = vector_cross_product({X1,X2,X3},{A1,A2,A3}),

	%%io:format("INvert texgen beyond ~n",[]),
	%% does not do up vector properly
	glu:lookAt(X1,X2,X3,A1,A2,A3,0,1,0),	
	gl:matrixMode(?GL_MODELVIEW),	

	gl:enable(?GL_TEXTURE_2D),
	%% in effect after application ??
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_LINEAR),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP_TO_BORDER),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP_TO_BORDER),
	gl:enable(?GL_TEXTURE_GEN_S),
	gl:enable(?GL_TEXTURE_GEN_T),
	gl:enable(?GL_TEXTURE_GEN_R),

	gl:enable(?GL_TEXTURE_GEN_Q),
	F = 1,
	gl:texGenfv(?GL_S,?GL_EYE_PLANE,{1*F,0,0,0}),
	gl:texGenfv(?GL_T,?GL_EYE_PLANE,{0,1*F,0,0}),
	%% translate -0.5 here ? was 0,0,1,0
	gl:texGenfv(?GL_R,?GL_EYE_PLANE,{0,0,1,0}),
	%% does q matter for transform ?? 
	gl:texGenfv(?GL_Q,?GL_EYE_PLANE,{0,0,0,1}),	
	ok.

%%
%%Render shadow texture from eyeview (what to do here??)
%%
shadow_view_depth_textured(Light,St,Scene,ExtObj)->


	% side effect none
	%% needs to be modified to accomedate multiple maps
	DepthMap = shadow_build(Light,St,Scene),


	%% same for all
	setup_camera_view(ExtObj),
	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

	%% do this ?? yes
	gl:disable(?GL_LIGHTING),


	%% required ?? (must check how to send this to viewer ..)
	gl:color3f(1,1,1),
	%gl:blendEquation(?GL_MAX),
	gl:blendFunc(?GL_ONE,?GL_ONE),
	gl:enable(?GL_BLEND),

	
	enableOffset(),

	lists:foldl(fun(DMap,Acc)->
		gl:clear(?GL_DEPTH_BUFFER_BIT),
		%% required because of the texture unit state
		gl:bindTexture(?GL_TEXTURE_2D,DMap),
		invert_texgen(Light,St,ExtObj,Acc),
		render_part_map(DMap,Scene),
		Acc+1 end,
		0,DepthMap),
	
	
	disableOffset(),
	gl:clear(?GL_DEPTH_BUFFER_BIT),
	%% may be incorrect for rendering
	gl:color4f(0,0,0,0), %% mayb some other method ??
	gl:callList(Scene),
	gl:color4f(1,1,1,1),



	%%gl:blendEquation(?GL_FUNC_ADD),
	%%gl:disable(?GL_BLEND),

	%% remove texture effects 
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_NONE),
	gl:bindTexture(?GL_TEXTURE_2D,0),
	texgen_off(),	%% stop inversion

	shadow_map_destroy(DepthMap),	

	gl:blendEquation(?GL_FUNC_ADD),
	%% working version 
	gl:blendFunc(?GL_DST_COLOR,?GL_ZERO),
	gl:enable(?GL_BLEND),

	%% re-enable lights
	gl:enable(?GL_LIGHTING),
		

	


	%%gl:color3f(1,1,1), %% destroys anything ?? 
	ok.
	

%%
%% Buile one shadow
%%
shadow_build(#we{light=Light,vp=VP}=WE,St,Scene,[TObj|Tail],MapN)->
	
	%% this part should become  a function

	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),	
	%% render to camera
	shadow_camera(WE,St,MapN),
	gl:disable(?GL_LIGHTING),
	gl:color3f(1,1,1),
	% render lights dont do this ...
	% render backsides for better z-buffer data
	gl:cullFace(?CULL_BACK),
	%%gl:cullFace(?GL_BACK),
	
	% should now be setup,  but we should also setup all the other stuff
	gl:callList(Scene),

	shadow_apply_tex(TObj),

	%%io:format("Shadow number=~p -> texture=~p~n",[MapN,TObj]),

	shadow_build(WE,St,Scene,Tail,MapN+1);
shadow_build(_,_,_,[],_)->
	ok.

%%
%%This is shadow build but this version is only for spotlights
%% returns: list of shadow map objects
%%
shadow_build(#we{light=Light,vp=VP}=WE,St,Scene)->

	%% create texture object, 
	%%[TObj] = gl:genTextures(total()),
	TObj = gl:genTextures(total()*total()),

	% this should copy the depth buffer to current texture 2d object ...

	%% build all textures
	shadow_build(WE,St,Scene,TObj,0),

	%%

	io:format("Returned error code ~p ~n",[gl:getError()]),
	% be nice
	gl:cullFace(?CULL_FRONT),
	%%gl:cullFace(?GL_FRONT),
	gl:enable(?GL_LIGHTING),
	TObj.


shadow_map_destroy(TObj)->
	%% check how this looks
	%% odd last elem arg
	gl:deleteTextures(total()*total(),TObj),
	ok.


%%
%% Should be changed to take argument to adapt view to part of image
%%
shadow_camera(#we{light=Light,vp=VP}=WE,St,MapN)->
	%
	%setup camera view 
	%
	%%io:format("Fetching Light .. ~p ~n",[Light]),
	#light{aim={A1,A2,A3},spot_angle=Angle} = Light,
	%%io:format("Done fetching light~n",[]),
	{X1,X2,X3} = Origin = get_average(VP),
	% fix projection matrix
	Size = shadow_viewport(),
	gl:viewport(0,0,Size,Size), % the viewport

	%% projection here should be changed to allow mult mapped textures
	gl:matrixMode(?GL_PROJECTION),
	gl:loadIdentity(),
	%%[W_Hither,W_Yon] = wpa:camera_info([hither,yon]),
	% should W_Hither and W_Yon be used here ?
	% NO, produces poor numerical results!!!
	% nothing better for the moment
	%%glu:perspective(2*Angle,1.0,W_Hither,W_Yon),
	projection(Angle,MapN),


	%% matrix mode
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),
	%%io:format("View point {~p,~p,~p} -> {~p,~p,~p}~n",[X1,X2,X3,A1,A2,A3]),
	%% needs changing since light does not work from above
	glu:lookAt(X1,X2,X3,A1,A2,A3,0,1,0),
	ok.


%%
%%Apply depth shadow texture 
%%
shadow_apply_tex(TObj)->

	%%io:format("Binding texture map ~p~n",[TObj]),

	%% build binary
	Size = get_tilesize(),
	Val  = 0,
	L    = Size*Size*32,
	Bin  = <<Val:L>>,
	

	%% need to transfer data to tex before copy ?? probably ...
	gl:bindTexture(?GL_TEXTURE_2D,TObj),

	%% assume values can be used (must in OpenGL 1.4 ??)
	%% what is target here ?? probably ?GL_TEXTURE_2D

	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP_TO_BORDER),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP_TO_BORDER),
	gl:getError(),

	%% debug version to texture surface ...
	%%gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGBA,8,8,0,?GL_RGBA,?GL_UNSIGNED_BYTE,debug_tex()),

	gl:texImage2D(?GL_TEXTURE_2D,0,?GL_DEPTH_COMPONENT,Size,Size,0,?GL_DEPTH_COMPONENT,?GL_UNSIGNED_BYTE,Bin),

	%%io:format("memory build shadow map: ~p~n",[glu:errorString(gl:getError())]),
	%% should be moved away from here
	%% the modern OpenGL 1.4 and the old versions of this
	%%gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_COMPARE_R_TO_TEXTURE),
	%%gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_FUNC,?GL_LEQUAL),

	%% copy buffer to texture object
	gl:getError(),
	gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_DEPTH_COMPONENT,0,0,Size,Size,0),
	%%io:format("build shadow map: ~p~n",[glu:errorString(gl:getError())]),

	%% go to normal settings
	gl:bindTexture(?GL_TEXTURE_2D,0),

	ok.	


%%if we start using tex planes in inversion to depth maps ...
%%NVIDIA claims that method be faster than changin the texture matrix ...
%%Ignoring NVIDIA claims, they seem to have confused things ... 
%%
texgen_off()->

	%% reset the state of texture matrix 
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	gl:matrixMode(?GL_MODELVIEW),

	gl:disable(?GL_TEXTURE_GEN_S),
	gl:disable(?GL_TEXTURE_GEN_T),
	gl:disable(?GL_TEXTURE_GEN_R),
	gl:disable(?GL_TEXTURE_GEN_Q),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_TEXTURE_1D),
	ok.


%
%Return value divisable by two and not larger than view screen
% ot texture limits 
%
shadow_viewport()->
	wpc_rspt_re:get_tilesize().

get_tilesize()->
	wpc_rspt_re:get_tilesize().
	

%%
%%Get origin
%%
get_average(X)->
	List = gb_trees:values(X),
	{A1,A2,A3} = lists:foldl(fun({X1,X2,X3},{Y1,Y2,Y3})->
			{X1+Y1,X2+Y2,X3+Y3}
		    end,{0,0,0},List),
	L = length(List),
	{A1/L,A2/L,A3/L}.



%% use ext object
setup_camera_view(ExtObj)->
	ExtObj(setup_camera_view).


%%
%% Render map into view
%% (realy bad name)
%%
render_part_map(DepthMap,Scene)->

	%%io:format("Applying map  ~p ~n",[DepthMap]),

	gl:bindTexture(?GL_TEXTURE_2D,DepthMap),	
	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_DEPTH_TEXTURE_MODE,?GL_INTENSITY),	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_COMPARE_R_TO_TEXTURE),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_FUNC,?GL_LEQUAL),

	
	gl:color4f(1,1,1,1), 
	gl:callList(Scene),	
	%%gl:disable(?GL_ALPHA_TEST),

	ok.


%%
%%Clamp values to range
%%
clamp(V,Floor,Ceiling)->
	case { V<Floor , V > Ceiling }  of
		{true,_} ->
			Floor;
		{_,true} ->
			Ceiling;
		{_,_}    ->
			V
	end.

%% total number of shadow map parts
total()->
	 C = clamp(wpc_rspt_ps:get_pref(shadow_detail,1),1,1000),
	%%io:format("Total number of shadow maps is ~p~n",[C]),
	C.

projection(Angle,Number)->
	Total = total(),
	%% perhaps replace w_hither
	[W_Hither,W_Yon] = wpa:camera_info([hither,yon]),
	%% find the part of the image
	Y = Number div Total,
	X = Number - Y * Total,
	Fac = math:tan((2.0*math:pi())*Angle/360.0),
	%%io:format("Angle = ~p Fac=~p~n",[Angle,Fac]),
	
	A0 = -Fac*W_Hither,
	A1 =  Fac*W_Hither,
	B0 = -Fac*W_Hither,
	B1 =  Fac*W_Hither,

	X0 = (X/Total)*A1 + ((Total-X)/Total)*A0,
	Y0 = (Y/Total)*B1 + ((Total-Y)/Total)*B0,
	X1 = ((X+1)/Total)*A1 + ((Total-X-1)/Total)*A0,
	Y1 = ((Y+1)/Total)*B1 + ((Total-Y-1)/Total)*B0,
	


	%% convert coords using tangent to angle	


	% should W_Hither and W_Yon be used here ?
	% NO, produces poor numerical results!!!
	% nothing better for the moment
	%%glu:perspective(2*Angle,1.0,W_Hither,W_Yon),
	gl:frustum(X0,X1,Y0,Y1,W_Hither,W_Yon),
	%%io:format("Shadow frustum (~p) (L=~p,R=~p,B=~p,T=~p,N=~p,F=~p)~n",[Number,X0,X1,Y0,Y1,W_Hither,W_Yon]),
	ok.



enableOffset()->
	case wpc_rspt_ps:get_pref(shadow_polygon_offset,no_off) of

		no_off ->
			ok;
		offset ->
			io:format("Enabling poly offset!!~n",[]),
			gl:enable(?GL_POLYGON_OFFSET_FILL),
			%% was negative
			%% needs a lot of attention ??
			gl:polygonOffset(0,-2000000.0),
			ok
	end.

disableOffset()->
	gl:disable(?GL_POLYGON_OFFSET_POINT),
	gl:disable(?GL_POLYGON_OFFSET_LINE),
	gl:disable(?GL_POLYGON_OFFSET_FILL),
	ok.
