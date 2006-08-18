%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_celshader.erl,v 1.1 2004/08/25 05:33:05 bjorng Exp $
%%

%%%
%%% Model to produce cel shading
%%%
%%%


-module(wpc_rspt_celshader).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2,attrib/2,help/0]).
-export([material_opts/1]).

init()->
	%%io:format("Loading ~p ... ~n",[?MODULE]),
	true.

%% start with all red for ambient and diff
menu(rspt_material_menu_ambFragProg0,Menu)->
	[{"Cel shader",wpc_rspt_celshader}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Cel shader",wpc_rspt_celshader}] ++ Menu;
menu(rspt_help,Menu)->
	[{"Using the cel shader",{wpc_rspt_celshader,help}}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


help()->
	wpc_rspt_wm:help_window(wpc_rspt_celshader_help_window,"Cel shader", 
		["Per pixel cel shader",
		 "This shader simulates a limited number of shades in rendering that is common in most painted art or drawn art."
		 "The 3D object is rendered as if there are a limited number of light and shade directions resulting in sharp "
		 "shadows on the 3D object. The number of shadows can be adjusted for each direction in the final image."
		 "It is possible to have separate shader for ambient light and diffuse light",
		 "Options",
		 "x_dirs",
		 "Determines the number of possible light directions of incoming light in horizontal directions. "
		 "The value for this option determines the width of each direction, 0 is smooth and 1 means no direction",
		 "y_dirs",
		 "Determines the number of possible light directions of incoming light in vertical directions. "
		 "The value for this option determines the width of each direction, 0 is smooth and 1 means no direction",
		 "z_dirs",
		 "Determines the number of possible light directions of incoming light in depth directions. "
		 "The value for this option determines the width of each direction, 0 is smooth and 1 means no direction",
		 "offset",
		 "Due to numerical errors it is sometimes nescessary to introduce a small offset in the computations."
		 "This value tweaks how rounding errors should be handled."
		],[]).
		

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
	io:format("Vertex Program String state ~p~n",[glu:errorString(gl:getError())]),
	io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	sane(Text),
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
		# cel shader bias funs                          " ++
		celbias(V,ProgID)   ++             %%PARAM num4       = {4,4,4,0};
	                                    %%PARAM div3       = {0.333333,0.333333,0.333333,0};
	"	PARAM prinv[4]   = { state.matrix.projection.inverse };
		#PARAM dPos       = fragment.position;
		PARAM lPos       = state.light[0].position;
	        OUTPUT oColor  = result.color;
    		TEMP cPos,ePos,wPos,dPos,VP,s,VPu,halfDir,halfDirU,dots,temp,xfNormal,celt,celf,celr;
		  

		#MOV oColor, fragment.color;
		  
 		# unpack and frac the normal data as celshading
		MUL celt, fragment.color, num4;
		FLR celf,celt;
		MUL_SAT celr,celf,div3;
		
		#
		# treat the computed value as real normal
		#
		SUB xfNormal, celr, bias;
		#MUL xfNormal, xfNormal,       num2;
		# normalize to correct numerical errors
		DP3 s,xfNormal,xfNormal;
		RSQ s,s.x;
		MUL xfNormal,s,xfNormal;

		# device -> eye coordinate normal transform ..
		MOV wPos, fragment.position;

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

		############# cel shader norm #############

		# pack and celshade VPu instead
		MAD VPu,bias,VPu,bias;

		# unpack and frac the normal data as celshading
		MUL celt, VPu, num4;
		FLR celf,celt;
		MUL_SAT celr,celf,div3;
	
		# unpack to VPu		
		SUB VPu, celr, bias;
		# normalize to correct numerical errors
		DP3 s,VPu,VPu;
		RSQ s,s.x;
		MUL VPu,s,VPu;
		
		############## end of cel shader #############

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


	io:format("Fragment Program String state ~p~n",[glu:errorString(gl:getError())]),
	io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	%io:format("Position is ~p ~n",[gl:getIntegerv(?GL_PROGRAM_ERROR_POSITION_ARB)]),
	%io:format("Are we using program ? ~p (number ~p) ~n",[gl:isProgramARB(?GL_FRAGMENT_PROGRAM_ARB),-1]),
	sane(Text),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_LIGHTING),
	%%gl:color3f(1,0,0),
	ok.


celbias(V,ProgID)->
	%%io:format("V is ~p ~n",[V]),
	Src = case ProgID of
		diffFragProg0 ->
			[diffFragProg0Opts,wpc_rspt];
		ambFragProg0 ->
			[ambFragProg0Opts,wpc_rspt]
	      end,

	%%io:format("Src has been set target ~p ~n",[[read,x_dirs|Src]]),
	%%io:format("Catched get is ~p ~n",[catch(wpc_rspt_ma:path_get_maybe(V,[read,x_dirs|Src],0.333))]),
	X = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,x_dirs|Src],0.333),0.01,1),			
	Y = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,y_dirs|Src],0.333),0.01,1),			
	Z = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,z_dirs|Src],0.333),0.01,1),			
	Offset = wpc_rspt_wm:clamp(wpc_rspt_ma:path_get_maybe(V,[read,offset|Src],0.0),0,1),

	%%io:format("Finished XYZ calc~n",[]),

	%%Offset = 0,

	Txt2 = "
		PARAM num4      = {" ++ float_to_list(Offset+1/X) ++","++float_to_list(Offset+1/Y)++","++float_to_list(Offset+1/Z)++",0};
		PARAM div3      = {" ++ float_to_list(X) ++ ","++float_to_list(Y)++","++float_to_list(Z)++",0};
	",

	%%io:format("Finished with celbias !! txt2=~p ~n",[Txt2]),

	Txt = "
	       PARAM num4       = {4,4,4,0};
	       PARAM div3       = {0.333333,0.333333,0.333333,0};
	",
	Txt2.


%%
%% Define material structure of elem
%%
material_opts([S,diffFragProg0|Tail])->
	%%io:format("Mat opts celshader diff! (~p)~n",[[object,x_dirs,diffFragProg0Opts|Tail]]),
	[Mat|_] = lists:reverse(Tail),
	wpc_rspt_ma:path_set([],[object,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,x_dirs,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,y_dirs,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,z_dirs,diffFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.0,[object,offset,diffFragProg0Opts|Tail]),
	ok;
material_opts([S,ambFragProg0|Tail])->
	%%io:format("Mat opts celshader amb (~p)~n",[[object,x_dirs,diffFragProg0Opts|Tail]]),
	[Mat|_] = lists:reverse(Tail),
	wpc_rspt_ma:path_set([],[object,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,x_dirs,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,y_dirs,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.05,[object,z_dirs,ambFragProg0Opts|Tail]),
	wpc_rspt_ma:path_set(0.0,[object,offset,ambFragProg0Opts|Tail]),
	ok.
