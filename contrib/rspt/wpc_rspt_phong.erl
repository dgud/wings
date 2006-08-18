%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_phong.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%
%%% Model to produce phong shading
%%%
%%%


-module(wpc_rspt_phong).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2,attrib/2,help/0]).


init()->
	%%io:format("Loading ~p ... ~n",[?MODULE]),
	true.

%% start with all red for ambient and diff
menu(rspt_material_menu_ambFragProg0,Menu)->
	[{"Phong",wpc_rspt_phong}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Phong",wpc_rspt_phong}] ++ Menu;
menu(rspt_help,Menu)->
	[{"Per pixel phong lightning",{wpc_rspt_phong,help}}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


help()->
	wpc_rspt_wm:help_window(wpc_rspt_phong_help_window,"Phong shader",
		["Per pixel phong shader"],[]).
		


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
		# as specified to be correct matrix multiplication (duh) row major it seems
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



%%
%% Should check and apply texture if appropriate ...
%%
attrib(V,ProgID)->
	fragMach(V,ProgID).

fragMach(V,ProgID)->
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
	        OUTPUT oColor  = result.color;
    		TEMP cPos,ePos,wPos,dPos,VP,s,VPu,halfDir,halfDirU,dots,temp,xfNormal;
		  

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

		# calculate diffuse and specular dot
		DP3 dots.x, xfNormal,VPu;
		DP3 dots.y, xfNormal,halfDirU;
		MOV dots.w, state.material.shininess.x;
		LIT dots,dots;

		# acc color contribs
		MAD temp, dots.y, state.lightprod[0].diffuse, state.lightprod[0].ambient;
		MAD oColor.xyz, dots.z, state.lightprod[0].specular, temp;
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
