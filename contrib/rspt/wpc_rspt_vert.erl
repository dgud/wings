%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_vert.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%
%%% Fragment program test module
%%%
%%%


-module(wpc_rspt_vert).
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
	[{"Vertex program gold",?MODULE}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Vertex program gold",?MODULE}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


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
	saneNumber(X-1,T,L).

attrib(V,ProgID)->
	io:format("Using gold only in program!! ~p as replacement for phong ~n",[ProgID]),
	gl:getError(),
	gl:enable(?GL_VERTEX_PROGRAM_ARB),
	%%[X] = gl:genProgramsARB(1),
	%% from vertex opengl description
	Text = "!!ARBvp1.0

	PARAM white           = {1,1,1,1};
	PARAM  eyeDir         = {0,0, 1,0};
	PARAM  skyL           = {0,5,0,1};
        ATTRIB iPos         = vertex.position;
        ATTRIB iNormal      = vertex.normal;
	PARAM  mv[4]        = { state.matrix.modelview };
	#PARAM  mvinv[4]     = { state.matrix.modelview.inverse };
        PARAM  mvinv[4]     = { state.matrix.modelview.invtrans };
        PARAM  mvp[4]       = { state.matrix.mvp };
        #PARAM  lightDir     = state.light[0].position;
        #PARAM  halfDir      = state.light[0].half;
	PARAM  lPos         = state.light[0].position;
        PARAM  specExp      = state.material.shininess;
        PARAM  ambientCol   = state.lightprod[0].ambient;
        PARAM  diffuseCol   = state.lightprod[0].diffuse;
        PARAM  specularCol  = state.lightprod[0].specular;
        TEMP   xfNormal, temp, dots,lightPosT,halfDirT,scalar,lightDir,halfDir,tPos,ePos;
        OUTPUT oPos         = result.position;
        OUTPUT oColor       = result.color;

        # Transform the vertex to clip coordinates.
        #DP4   tPos.x, mvp[0], iPos;
        #DP4   tPos.y, mvp[1], iPos;
        #DP4   tPos.z, mvp[2], iPos;
        #DP4   tPos.w, mvp[3], iPos;

	#
	# as specified to be correct matrix multiplication (duh) row major it seems
	#
	DP4   tPos.x, iPos,mvp[0];
	DP4   tPos.y, iPos,mvp[1];
	DP4   tPos.z, iPos,mvp[2];
	DP4   tPos.w, iPos,mvp[3];
	MOV   oPos, tPos;

	#
	# eye position of vertex
	#
	DP4   ePos.x, iPos,mv[0];
	DP4   ePos.y, iPos,mv[1];
	DP4   ePos.z, iPos,mv[2];
	DP4   ePos.w, iPos,mv[3];

        # Transform the normal to eye coordinates.
        #DP3   xfNormal.x, mvinv[0], iNormal;
        #DP3   xfNormal.y, mvinv[1], iNormal;
        #DP3   xfNormal.z, mvinv[2], iNormal;
	
	#
	# as specified in opengl (semi light if multipied with inverse only ...)
	#
	#DP3    xfNormal.x, iNormal, mvinv[0];
	#DP3    xfNormal.y, iNormal, mvinv[1];
	#DP3    xfNormal.z, iNormal, mvinv[2];

	# alt method
	# eye view transform GL specifies transposed mult with inverse ..
	#DP3     xfNormal.x, state.matrix.modelview.row[0], iNormal;
	#DP3     xfNormal.y, state.matrix.modelview.row[1], iNormal;
	#DP3     xfNormal.z, state.matrix.modelview.row[2], iNormal;
	
	# as specified correct transformation (DP3 transform)
	DP3 xfNormal.x, iNormal, mv[0];
	DP3 xfNormal.y, iNormal, mv[1];
	DP3 xfNormal.z, iNormal, mv[2];
	

	# light and half dir recalculated to eye coordinates, duh
	#DP4  lightPosT.x, lPos, mv[0];
	#DP4  lightPosT.y, lPos, mv[1];
	#DP4  lightPosT.z, lPos, mv[2];
	#DP4  lightPosT.w, lPos, mv[3];

	# lPos is already in eye view coordinates
	SUB  lightPosT,lPos,ePos;

	# normalize PosT
	DP3 scalar,lightPosT,lightPosT;
	RSQ scalar,scalar.x;
	MUL lightDir,scalar,lightPosT;
	
	# calc normalized half vector
	ADD halfDirT, lightDir,eyeDir;
	DP3 scalar, halfDirT,halfDirT;
	RSQ scalar,scalar.x;
	MUL halfDir,scalar,halfDirT;

	# normalize normal
	DP3 scalar, xfNormal,xfNormal;
	RSQ scalar,scalar.x;
	MUL xfNormal,scalar,xfNormal;

        # Compute diffuse and specular dot products and use LIT to compute
        # lighting coefficients.
        DP3   dots.x, xfNormal, lightDir;
        DP3   dots.y, xfNormal, halfDir;
        MOV   dots.w, specExp.x;
        LIT   dots, dots;

        # Accumulate color contributions.
        MAD   temp, dots.y, diffuseCol, ambientCol;
        MAD   oColor.xyz, dots.z, specularCol, temp;
        MOV   oColor.w, diffuseCol.w;
	
	# debug stuff
	#SUB lightPosT,lPos,ePos;
	# supposedly working normalization
	#DP3 scalar, lightPosT,lightPosT;
	#RSQ scalar,scalar.x;
	#MUL lightDir,lightPosT,scalar;
	#DP3 oColor.xyz, lightDir,xfNormal;
        END",


	Bin = list_to_binary(Text),

	%%gl:bindProgramARB(?GL_VERTEX_PROGRAM_ARB,X), %% later
	gl:programStringARB(?GL_VERTEX_PROGRAM_ARB,?GL_PROGRAM_FORMAT_ASCII_ARB,length(Text),Bin),


	io:format("Program String state ~p~n",[glu:errorString(gl:getError())]),
	io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	io:format("Position is ~p ~n",[gl:getIntegerv(?GL_PROGRAM_ERROR_POSITION_ARB)]),
	%%io:format("Are we using program ? ~p (number ~p) ~n",[gl:isProgramARB(?GL_VERTEX_PROGRAM_ARB),-1]),
	sane(Text),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_LIGHTING),
	%%gl:color3f(1,0,0),
	ok.
