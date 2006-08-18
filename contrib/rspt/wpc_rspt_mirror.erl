%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_mirror.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%
%%% Plugin for mirror and refraction (test currently)
%%%

%%
%% errors: slow and produces reflections on back side that are visible !!!??
%% should perhaps be replaced augumented with cube mapped refraction/reflection
%%

-module(wpc_rspt_mirror).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2,ambPipe/1]).


init()->
	%%io:format("Loading ~p ... ~n",[?MODULE]),
	true.

%% start with all red for ambient and diff
menu(rspt_material_menu_ambPipe,Menu)->
	[{"Mirror & Refraction surface",?MODULE}] ++ Menu;

%%%%menu(rspt_material_menu_diffPipe,Menu)->
%%%%	[{"Mirror test stuff",?MODULE}] ++ Menu;

menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


ambPipe({F,X,WE,ST,Scene,ExtObj})->
	%%io:format("|| wpc_rspt_mirror called as ambPrg surface ~p ||~n",[X]),
	AObject = transform({F,X,WE,ST,Scene,ExtObj}),
	%%light_render_all(ST,Scene,ExtObj),
	light_render_all(ST,Scene,AObject),
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% The Transform
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

transform({F,X,WE,ST,Scene,ExtOBJ})->
	%% get surface normal ...
	%% get point on surface ..
	{XV,YV,ZV} = wpc_rspt_sc:surface_vertex(F,WE),
	%% build x4 param surface equation
	{NX,NY,NZ,NW} = wpc_rspt_sc:surface_4param(F,WE),
	%% build rotation equations (translation and rotation)
	fun(Arg)->
		case Arg of
			clip_surface ->
				%%io:format("Clip plane ~p ~p ~p ~p ~n",[NX,NY,NZ,NW]),
				%% realy needs polygon offset (or similar) to fix mirror errors
				{NX,NY,NZ,NW*1.1};
			surface_mirror ->
				%% object space -> mirror space -> object space
				%%% set all transforms required
				%%io:format("Translation vertex is located at ~p ~p ~p ~n",[XV,YV,ZV]),
				gl:translatef(XV,YV,ZV),
				%%% find the rotation axis
				DEFA = {0,1.0,0},
				{RX,RY,RZ} = wpc_rspt_ve:normalize(wpc_rspt_ve:cross_product(DEFA,{-NX,-NY,-NZ})),
				DEG = (360.0*math:acos(-NY))/(2.0*math:pi()),
				%%io:format("-NY is ~p ~n ",[-NY]),
				case -NY > 0.998 of
					true ->
						%%io:format("No rotation~n",[]),
						%% no rotate
						gl:scalef(1,-1,1);
					false->
						gl:rotatef(DEG,RX,RY,RZ),
						%%io:format("Rotating as ~p  with (~p,~p,~p) ~n",[DEG,RX,RY,RZ]),
						%% mirror in mirror space
						%%gl:rotatef(180,1,0,0),
						%%gl:rotatef(180,0,1,0),
						gl:scalef(1,-1,1),
						%% invert now and go back to object space
						gl:rotatef(-DEG,RX,RY,RZ),
					
						ok
				end,
				%%io:format("Mirror finished~n",[]),
				gl:translatef(-XV,-YV,-ZV),
				ok;
			  SuperArg ->
				ExtOBJ(SuperArg)
		end
	end.
	%%%% for the moment ...
	%%%%ExtOBJ. 




%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% The rendering of all objects
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

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
%%
%%mirrors and other stuff should have special treatment
%%
%%ExtObj is an object for passing additional data
%%
light_render_all(#st{shapes=Sh}=St,Scene,ExtObj)->

	gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	gl:pushClientAttrib(?GL_CLIENT_ALL_ATTRIB_BITS),
	gl:pushMatrix(),
	gl:matrixMode(?GL_PROJECTION),
	gl:pushMatrix(),
	gl:matrixMode(?GL_MODELVIEW),

	gl:enable(?GL_CLIP_PLANE0),

	gl:clipPlane(?GL_CLIP_PLANE0,ExtObj(clip_surface)),
	gl:clearColor(0,0,0,1),
	gl:clear(?GL_DEPTH_BUFFER_BIT),
	gl:disable(?GL_CULL_FACE),
	ExtObj(mat_none),
	setup_camera_view(ExtObj),
	gl:colorMask(?GL_FALSE,?GL_FALSE,?GL_FALSE,?GL_FALSE),	
	gl:callList(Scene),
	gl:colorMask(?GL_TRUE,?GL_TRUE,?GL_TRUE,?GL_TRUE),	

	%% setup blend functions to render proper mirror images

	gl:enable(?GL_BLEND),
	gl:blendEquation(?GL_FUNC_ADD),
	gl:blendFunc(?GL_ONE,?GL_ONE),

	light_ambient(St,Scene,ExtObj),

	


	NLights = lists:foldl(fun({N,X},NumberLights)->
				case ?IS_LIGHT(X) of
					true ->
						light_render(X,St,Scene,NumberLights+1,ExtObj),
						NumberLights+1;
					false ->
						NumberLights
				end
			end,1,gb_trees:to_list(Sh)),
	gl:disable(?GL_BLEND),	
	gl:popMatrix(),
	gl:matrixMode(?GL_PROJECTION),
	gl:popMatrix(),
	gl:matrixMode(?GL_MODELVIEW),
	gl:popClientAttrib(),
	gl:popAttrib(),
	
	%% needed to rebuild scene and depth buffer data
	gl:disable(?GL_STENCIL_TEST),
	gl:clear(?GL_DEPTH_BUFFER_BIT),
	gl:disable(?GL_CLIP_PLANE0),
	gl:enable(?GL_CULL_FACE),
	gl:colorMask(?GL_FALSE,?GL_FALSE,?GL_FALSE,?GL_FALSE),	
	setup_camera_view2(ExtObj), %% push matrix should realy be enough ..
	%% write z-buffer
	ExtObj(mat_none),
	gl:callList(Scene),

	gl:colorMask(?GL_TRUE,?GL_TRUE,?GL_TRUE,?GL_TRUE),

	

	ok.


setup_camera_view2(ExtObj)->
	%% for safety (moved form material)
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),
	ExtObj(setup_camera_view).

setup_camera_view(ExtObj)->
	%% for safety (moved form material)
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),
	ExtObj(setup_camera_view),
	%% temp but okay for now
	%% not alright since flipping poly side
	%%gl:rotatef(180,1,0,0),
	%%gl:rotatef(180,0,1,0),
	%%gl:scalef(1,-1,1),
	ExtObj(surface_mirror),
	ok.

	



%%
%%render ambient light ... no shadows
%%
light_ambient(#st{shapes=Sh,mat=Mat}=St,Scene,ExtObj)->
	% display material defs
	%%io:format("mat is ~n ~p ~n",[gb_trees:to_list(Mat)]),
	% render everything from camera
	%%setup_camera_view(ExtObj),
	light_alloff(),
	gl:enable(?GL_LIGHTING),
	Ambient = lists:foldl(fun({N,X},Acc)->
			case X of
				#we{light=none} ->
					%not ambient or light
					Acc;
				#we{light=#light{type=ambient,ambient=Amb}} ->
					Amb;
				#we{light=#light{}} ->
					Acc
			end 
		end,{0.1,0.1,0.1},gb_trees:to_list(Sh)),
	%%io:format("Ambient light ~p ~n",[Ambient]),
	gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT,Ambient),

	%% there should be a foldl over all ambient fragment programs here .. later, as defined
	%% see diffuse, of how to solve this

	%%gl:callList(Scene),
	executeProg(Scene,ExtObj,[ambFragProg0,ambFragProg1,ambFragProg2,ambFragProg3,ambFragProg4]),

	gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT,{0,0,0,0}),
	ok.



%
%
%
%Wrong for infinite lights
% should introduce functions for spot and pointlights 
%
%
light_render(#we{light=#light{type=ambient}},_,_,_,_)->
	ok;
light_render(#we{light=Light,vp=VP}=WE,#st{}=St,Scene,Number,ExtObj)->

	
	


	%%setup_camera_view(ExtObj),

	%% setup blend equation
	gl:enable(?GL_BLEND),
	gl:blendEquation(?GL_FUNC_ADD),
	gl:blendFunc(?GL_ONE,?GL_ONE),

	%%
	%%Should be this with stencil set to clip to polygon to mirror into ..
	%%
	%%The state beyond this point, is
	%%     Z-buffer      : scene correct 
	%%     Color         : has none shadowed points in scene as values from 0..1 for each color
	%%                     0 in shadow, 1 in light values in between are sem shadowed by something 
	%%     accum         : storing current rendered data if selected for temp buffer
	%%     aux0          : possibly target buffer for writing
	%%     writeBuffer   : any, do not change front only is illegal however use front_and_back
	%%     ambient light : {0,0,0,0}
	%%     texture matrix: Identity
	%%     lights        : all off
	%%
	


	% has to change sooner or later ? depends on the quality and power
	% of shadow maps and misc ...
	gl:enable(?GL_LIGHT0),
	gl:enable(?GL_LIGHTING),
	%%io:format("Fetching Light .. ~p ~n",[Light]),
	#light{aim={A1,A2,A3},type=Type,diffuse=Diffuse,ambient=Ambient,specular=Specular,
		lin_att=Lin_att,quad_att=Quad_att,spot_angle=Spot_angle,spot_exp=Spot_exp,prop=Prop} = Light,
	case Type of
		ambient->
			ok; % do nothing
		point  ->
			%%io:format("Point light~n",[]),
			{X1,X2,X3} = Origin = get_average(VP),
			gl:lightfv(?GL_LIGHT0,?GL_AMBIENT,Ambient),
			gl:lightfv(?GL_LIGHT0,?GL_DIFFUSE,Diffuse),
			gl:lightfv(?GL_LIGHT0,?GL_SPECULAR,Specular),
			gl:lightfv(?GL_LIGHT0,?GL_POSITION,{X1,X2,X3,1}),
			gl:lightfv(?GL_LIGHT0,?GL_SPOT_DIRECTION,{A1-X1,A2-X2,A3-X3}),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_EXPONENT,0),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_CUTOFF,180.0),
			% wings does not seem to have this ... ignore for now
			%gl:lightfv(?GL_LIGHT0,?GL_CONSTANT_ATTENUATION,???),
			gl:lightf(?GL_LIGHT0,?GL_LINEAR_ATTENUATION,Lin_att),
			gl:lightf(?GL_LIGHT0,?GL_QUADRATIC_ATTENUATION,Quad_att),
			% should introduce a display state and another function handling compilation

			%%gl:callList(Scene);
			ok;
		Kind      ->
			%%io:format("Done fetching light of type ~p rendering as spotlight ~n",[Kind]),
			{X1,X2,X3} = Origin = get_average(VP),
			gl:lightfv(?GL_LIGHT0,?GL_AMBIENT,Ambient),
			gl:lightfv(?GL_LIGHT0,?GL_DIFFUSE,Diffuse),
			gl:lightfv(?GL_LIGHT0,?GL_SPECULAR,Specular),
			gl:lightfv(?GL_LIGHT0,?GL_POSITION,{X1,X2,X3,1}),
			gl:lightfv(?GL_LIGHT0,?GL_SPOT_DIRECTION,{A1-X1,A2-X2,A3-X3}),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_EXPONENT,Spot_exp*128),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_CUTOFF,Spot_angle),
			% wings does not seem to have this ... ignore for now
			%gl:lightfv(?GL_LIGHT0,?GL_CONSTANT_ATTENUATION,???),
			gl:lightf(?GL_LIGHT0,?GL_LINEAR_ATTENUATION,Lin_att),
			gl:lightf(?GL_LIGHT0,?GL_QUADRATIC_ATTENUATION,Quad_att),
			% should introduce a display state and another function handling compilation
			ok
			%%gl:callList(Scene)
	end,

	case Type of
		ambient ->
			ok;
		_       ->
			executeProg(Scene,ExtObj,[diffFragProg0,diffFragProg1,diffFragProg2,diffFragProg3,diffFragProg4])
	end,
	

	% be nice
	%%gl:disable(?GL_BLEND),
	gl:disable(?GL_LIGHT0),
	ok.


%%%
%%% execute all programs for this pass
%%%
executeProg(_,_,[])->
	ok;
executeProg(Scene,ExtObj,[EL|L])->
	%% included mat_full in operator
	ExtObj({mat_full,EL}),
	gl:callList(Scene),
	executeProg(Scene,ExtObj,L).



%
%Eliminate all light
%
light_alloff()->
	gl:disable(?GL_LIGHT0),
	gl:disable(?GL_LIGHT1),
	gl:disable(?GL_LIGHT2),
	gl:disable(?GL_LIGHT3),
	gl:disable(?GL_LIGHT4),
	gl:disable(?GL_LIGHT5),
	gl:disable(?GL_LIGHT6),
	gl:disable(?GL_LIGHT7),
	gl:disable(?GL_TEXTURE_2D),
	ok.


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
