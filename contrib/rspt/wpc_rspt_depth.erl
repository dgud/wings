%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_depth.erl,v 1.1 2004/08/25 05:33:05 bjorng Exp $
%%

%%%
%%% Model to produce phong shading
%%%
%%%


-module(wpc_rspt_depth).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2,attrib/2]).


init()->
	%%io:format("Loading ~p ... ~n",[?MODULE]),
	true.

%% start with all red for ambient and diff
menu(rspt_material_menu_ambFragProg0,Menu)->
	[{"Depth color",wpc_rspt_depth}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Depth color",wpc_rspt_depth}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


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
	io:format("Using red only in program!! ~p as replacement for phong ~n",[ProgID]),
	gl:getError(),
	vertexMach(),
	
	gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
	%%[X] = gl:genProgramsARB(1),





	Text = "!!ARBfp1.0
      		
		PARAM white = {1,1,1,1};
	        OUTPUT oColor  = result.color;
		MOV oColor.xyz, fragment.position.z;
		MOV oColor.w, white.w;
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
