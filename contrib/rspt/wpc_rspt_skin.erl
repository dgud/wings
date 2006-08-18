%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_skin.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%
%%% Model to produce phong shading
%%%
%%%


-module(wpc_rspt_skin).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2,attrib/2]).
-export([help/0,material_opts/1]).

init()->
	%%io:format("Loading ~p ... ~n",[?MODULE]),
	true.

%% start with all red for ambient and diff
menu(rspt_material_menu_ambFragProg0,Menu)->
	[{"Skin",wpc_rspt_skin}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Skin",wpc_rspt_skin}] ++ Menu;
menu(rspt_help,Menu)->
	[{"Using the skin shader",{wpc_rspt_skin,help}}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


help()->
	wpc_rspt_wm:help_window(wpc_rspt_skinshader_help_window,"Skin shader",
				["Skin shader",
				 "The shader simulates the effect of surfaces with transparent top layers. "
				 "These surfaces do not have the same sharp shadows as other surfaces have. "
				 "This is because the transparent layer reflects incoming light in all directions "
				 "and hence all sahdows appear more smooth. Human skin is in fact transparent and is nicely "
				 "simulated by this shader.",
				 "Options",
				 "average",
				 "Value to bias how much of all incoming light is scattered.",
				 "blend",
				 "Determines how much the transparent layer reflects incoming light from arbitary directions."],[]).

%%%
%%%
%%%

%%%
%%%
%%%
sane(Text)->
	sane(gl:getIntegerv(?GL_PROGRAM_ERROR_POSITION_ARB),Text).

sane([-1|_],_)->
	ok;
sane([X|_],Text)->
	saneNumber(X,Text,1).

saneNumber(-1,_,1)->
	ok;
saneNumber(0,_,L)->
	io:format("On line ~p ~n",[L]),
	ok;
saneNumber(X,[10|T],L)->
	saneNumber(X-1,T,L+1);
saneNumber(X,[_|T],L)->
	saneNumber(X-1,T,L);
saneNumber(_,_,_)->
	io:format("Unknown instruction state ?~n",[]).


vertexMach()->
	gl:enable(?GL_VERTEX_PROGRAM_ARB),	
	%% should probably replace transforms with OPTION invariance 
	Text = "!!ARBvp1.0
		
		PARAM biasM          = {0.5,0.5,0.5,0};
		PARAM biasO          = {0.5,0.5,0.5,1};
		PARAM mvp[4]        = { state.matrix.mvp };
		PARAM mv[4]         = { state.matrix.modelview };
		ATTRIB iPos         = vertex.position;
        	ATTRIB iNormal      = vertex.normal;	
		OUTPUT oPos         = result.position;
        	OUTPUT oColor       = result.color;
		TEMP tPos,xfNormal,scalar;

		#
		# as specified to be correct matrix multiplication, row major
		#
		DP4   tPos.x, iPos,mvp[0];
		DP4   tPos.y, iPos,mvp[1];
		DP4   tPos.z, iPos,mvp[2];
		DP4   tPos.w, iPos,mvp[3];
		MOV   oPos, tPos;

		# as specified correct transformation (DP3 transform)
		DP3 xfNormal.x, iNormal, mv[0];
		DP3 xfNormal.y, iNormal, mv[1];
		DP3 xfNormal.z, iNormal, mv[2];
	
		# normalize normal
		DP3 scalar, xfNormal,xfNormal;
		RSQ scalar,scalar.x;
		MUL xfNormal,scalar,xfNormal;

		#
		# set the out color to be the normals transformed to eye coordinates
		# position will have to be divided to yield previous position !!!??
		# if we can do this ...
		MAD oColor,xfNormal,biasM,biasO;
				
		# I should perpahs force alpha to one ...

		END",
	Bin  = list_to_binary(Text),
	gl:programStringARB(?GL_VERTEX_PROGRAM_ARB,?GL_PROGRAM_FORMAT_ASCII_ARB,length(Text),Bin),
	%%io:format("Vertex Program String state ~p~n",[glu:errorString(gl:getError())]),
	%%io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	%%sane(Text),
	ok.


getInvX()->
	[_,_,Tile,_] = gl:getIntegerv(?GL_VIEWPORT),
	"{" ++ float_to_list(2.0/Tile) ++ ",0,0,-1};".
	%%"{0.0039,0,0,-1};".

getInvY()->
	[_,_,_,Tile] = gl:getIntegerv(?GL_VIEWPORT),
	"{0," ++ float_to_list(2.0/Tile) ++ ",0,-1};".
	%%"{0,0.0039,0,-1};".

getInvZ()->
	% fix this one ? hmm
	%%io:format("The viewport distance is -- ~p ---~n",[gl:getFloatv(?GL_DEPTH_RANGE)]),
	[Near,Far] = gl:getFloatv(?GL_DEPTH_RANGE),
	"{0,0," ++ float_to_list(2.0/(Far-Near)) ++ ",-1};".

attrib(V,ProgID)->
	%%io:format("Using red only in program!! ~p as replacement for phong ~n",[ProgID]),
	gl:getError(),
	vertexMach(),
	
	gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
	%%[X] = gl:genProgramsARB(1),





	Text = "!!ARBfp1.0
      		

		# debug transforms need replacing ..		
		PARAM invX       = " ++ getInvX() ++ " #{0.0039,0,0,-1};
		PARAM invY       = " ++ getInvY() ++ " #{0,0.0039,0,-1};
		PARAM invZ       = " ++ getInvZ() ++ " #{0,0,2,-1};

		PARAM skyL       = {0,1,0,0};
		PARAM eyeDir     = {0,0,1,0};
	    	PARAM col        = {0,1,0,1};
		PARAM bias       = {0.5,0.5,0.5,0};
		PARAM biasO      = {0.5,0.5,0.5,1};
		PARAM white      = {1,1,1,1};
		PARAM num2       = {2,2,2,0};
		PARAM prinv[4]   = { state.matrix.projection.inverse };
		#PARAM dPos       = fragment.position;
		PARAM lPos       = state.light[0].position;

		# bias normals
		PARAM norm1      = {0,-1,0,0};
		PARAM norm2      = {0.5,0.5,0,0};
		PARAM norm3      = {-0.25,0.5,-0.25};
		PARAM norm4      = {-0.25,0.5,0.25};   " ++
		blend(V,ProgID)	%%PARAM avg        = {0.7,0.7,0.7,0.2};
			%%# factor to blend norms with 
			%%PARAM blendF     = {0.4,0.4,0.4,0.4};
			%%PARAM blendIF    = {0.6,0.6,0.6,0.6};
		++ "
	        OUTPUT oColor  = result.color;
    		TEMP cPos,ePos,wPos,dPos,VP,s,VPu,halfDir,halfDirU,dots,temp,xfNormal;
		TEMP out0,out1,out2,out3,out4,out5,nNormal;		  


		#MOV oColor, fragment.color;
		  
 		# unpack the normal data
		SUB xfNormal, fragment.color, bias;
		#MUL xfNormal, xfNormal,       num2;
		# normalize to correct numerical errors
		DP3 s,xfNormal,xfNormal;
		RSQ s,s.x;
		MUL xfNormal,s,xfNormal;

		# device -> eye coordinate normal transform ..
		# should test if z coords are logaritmic values .. they may be that .. 
		MOV wPos, fragment.position;
		# has some effect on rendering worth investigating further ...
		#EX2 wPos.z, -wPos.z; # test attempt to check if z is logarithmic
		# could try using a z-offset for wPos to stop bad lightning ...

		# go from viewport transform -> device coords
		DPH dPos.x,wPos, invX;
		DPH dPos.y,wPos, invY;
		# use state.depth.range instead
		#DPH dPos.z,wPos, invZ;
		MUL dPos.z, dPos.z,state.depth.range.z;
		ADD dPos.z, dPos.z,state.depth.range.x; 
		MOV dPos.w,wPos.w;

		# do divison and transform to clip coords
		#RCP s,fragment.position.w;
		#MUL cPos,dPos,s.x;
		MOV cPos,dPos;
		#MUL cPos,dPos,fragment.position.w;

		# force last to be one
		SWZ cPos.w, cPos, x,y,z,1;

		# clip -> eye coords
		DP4 ePos.x,cPos, prinv[0];
		DP4 ePos.y,cPos, prinv[1];
		DP4 ePos.z,cPos, prinv[2];
		DP4 ePos.w,cPos, prinv[3];

		# in theory we now only need to apply the default light model

		# get VP 
		SUB VP,lPos,ePos; 
		#MOV VP, skyL;        # correct since light always up
				     # could be bound incorrectness from previous stage
		#MOV VP,lPos; # move lPos to test ...		

		# normalize VP
		DP3 s,VP,VP;
		RSQ s,s.x;
		MUL VPu,s,VP;

		# calc half dir normalized
		#
		ADD halfDir, VPu, eyeDir;
		DP3 s, halfDir,halfDir;
		RSQ s,s.x;
		MUL halfDirU,halfDir,s;

		############################## aspect 0  ############################ 

		# calculate diffuse and specular dot
		DP3 dots.x, xfNormal,VPu;
		DP3 dots.y, xfNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD out0.xyz, dots.z, state.lightprod[0].specular, temp;
		MOV out0.w, state.lightprod[0].diffuse.w;		

		######################################################################

		############################## aspect 1  ############################ 

		###### calculate nNormal ####

		MUL temp,blendF,norm1;
		MAD nNormal,xfNormal,blendIF,temp;

		# calculate diffuse and specular dot
		DP3 dots.x, nNormal,VPu;
		DP3 dots.y, nNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD out1.xyz, dots.z, state.lightprod[0].specular, temp;
		MOV out1.w, state.lightprod[0].diffuse.w;		

		######################################################################

		############################## aspect 2  ############################ 

		###### calculate nNormal ####

		MUL temp,blendF,norm2;
		MAD nNormal,xfNormal,blendIF,temp;

		# calculate diffuse and specular dot
		DP3 dots.x, nNormal,VPu;
		DP3 dots.y, nNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD out2.xyz, dots.z, state.lightprod[0].specular, temp;
		MOV out2.w, state.lightprod[0].diffuse.w;		

		######################################################################

		############################## aspect 3  ############################ 

		###### calculate nNormal ####

		MUL temp,blendF,norm3;
		MAD nNormal,xfNormal,blendIF,temp;

		# calculate diffuse and specular dot
		DP3 dots.x, nNormal,VPu;
		DP3 dots.y, nNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD out3.xyz, dots.z, state.lightprod[0].specular, temp;
		MOV out3.w, state.lightprod[0].diffuse.w;		

		######################################################################

		############################## aspect 4  ############################ 

		###### calculate nNormal ####

		MUL temp,blendF,norm4;
		MAD nNormal,xfNormal,blendIF,temp;

		# calculate diffuse and specular dot
		DP3 dots.x, nNormal,VPu;
		DP3 dots.y, nNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD out4.xyz, dots.z, state.lightprod[0].specular, temp;
		MOV out4.w, state.lightprod[0].diffuse.w;		

		######################################################################

		### average results into fragment ###
		
		MUL temp,out0,avg;
		MAD temp,out1,avg,temp;
		MAD temp,out2,avg,temp;
		MAD temp,out3,avg,temp;
		MAD oColor.xyz,out4,avg,temp;
		MOV oColor.w, state.lightprod[0].diffuse.w;		

		# debug stuff
		
		#MAD oColor,VPu,bias,biasO;
		#MOV oColor.xyz,ePos;
		#MOV oColor, fragment.color;
		#MOV oColor.xyz,wPos.z;

   		END",
	Bin = list_to_binary(Text),

	%%gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,X), %% later
	gl:programStringARB(?GL_FRAGMENT_PROGRAM_ARB,?GL_PROGRAM_FORMAT_ASCII_ARB,length(Text),Bin),


	%%io:format("Fragment Program String state ~p~n",[glu:errorString(gl:getError())]),
	%%io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	%io:format("Position is ~p ~n",[gl:getIntegerv(?GL_PROGRAM_ERROR_POSITION_ARB)]),
	%io:format("Are we using program ? ~p (number ~p) ~n",[gl:isProgramARB(?GL_FRAGMENT_PROGRAM_ARB),-1]),
	%%sane(Text),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_LIGHTING),
	%%gl:color3f(1,0,0),
	ok.


blend(V,ProgID)->
	Src = case ProgID of
		diffFragProg0 ->
			[diffFragProg0Opts,wpc_rspt];
		ambFragProg0 ->
			[ambFragProg0Opts,wpc_rspt]
	      end,
	Avg   = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,average|Src],0.7),0.01,1),
	Blend = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,blend|Src],0.4),0.0,1.0),
	ABlend = 1.0 - Blend,
	TAvg    = float_to_list(Avg),
	TBlend  = float_to_list(Blend),
	TABlend = float_to_list(ABlend),
	
	Txt = "
		PARAM avg        = {"++TAvg++","++TAvg++","++TAvg++",0.2};
		# factor to blend norms with 
		PARAM blendF     = {"++TBlend++","++TBlend++","++TBlend++","++TBlend++"};
		PARAM blendIF    = {"++TABlend++","++TABlend++","++TABlend++","++TABlend++"};
	      ",

	io:format("The txt is ~p ~n",[Txt]),

	Txt.

material_opts([S,diffFragProg0|Tail])->
	[Mat|_] = lists:reverse(Tail),
	wpc_rspt_ma:path_set([],[object,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.7,[object,average,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.4,[object,blend,diffFragProg0Opts|Tail]),
	ok;
material_opts([S,ambFragProg0|Tail])->
	[Mat|_] = lists:reverse(Tail),
	wpc_rspt_ma:path_set([],[object,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.7,[object,average,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.4,[object,blend,ambFragProg0Opts|Tail]),
	ok.
