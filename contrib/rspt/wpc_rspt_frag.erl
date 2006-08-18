%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_frag.erl,v 1.1 2004/08/25 05:33:05 bjorng Exp $
%%

%%%
%%% Fragment program test module
%%%
%%%


-module(wpc_rspt_frag).
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
	[{"Fragment program red",?MODULE}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Fragment program red",?MODULE}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.

command(_,_)-> next.


%%%
%%%
%%%


attrib(V,ProgID)->
	io:format("Using red only in program!! ~p as replacement for phong ~n",[ProgID]),
	gl:getError(),
	gl:enable(?GL_FRAGMENT_PROGRAM_ARB),
	%%[X] = gl:genProgramsARB(1),
	TextOld = "!!ARBfp1.0
      		# Simple program to show how to code up the default texture environment

	    	  PARAM col = {1,0,0,1};
	
	          OUTPUT outColor = result.color;
    
      		  MOV outColor, col;              #perform the modulation
    
   		  END",

	Text = "!!ARBfp1.0
PARAM col = {1,0,0,1};
PARAM zix = {0.002,0.002,0.002,1};
PARAM neg = {0,0,-1,1};
TEMP x,y,z,tx,ty,tz,tx2,ty2,tz2,tf,tf2;
OUTPUT outc = result.color;
DP3 x, state.matrix.mvp.inverse.row[0], fragment.position;
DP3 y, state.matrix.mvp.inverse.row[1], fragment.position;
DP3 z, state.matrix.mvp.inverse.row[2], fragment.position;
SIN ty,y .y;
SIN tx,x .x;
SIN tz,z .z;
SWZ tx2, tx, x,0,0,0;
SWZ ty2, ty, 0,y,0,1;
SWZ tz2, tz, 0,0,z,0;
ADD tf, tx2,ty2;
ADD tf2, tz2,tf;
# should bias obj for numerical errors ...
#KIL neg; no effect for anything why ?? check
#COS obj,
#MUL obj, zix, fragment.position;
MOV outc, tf2;
END
",


	Bin = list_to_binary(Text),

	%%gl:bindProgramARB(?GL_FRAGMENT_PROGRAM_ARB,X), %% later
	gl:programStringARB(?GL_FRAGMENT_PROGRAM_ARB,?GL_PROGRAM_FORMAT_ASCII_ARB,length(Text),Bin),


	io:format("Program String state ~p~n",[glu:errorString(gl:getError())]),
	io:format("Program errors  ~p ~n",[gl:getString(?GL_PROGRAM_ERROR_STRING_ARB)]),
	io:format("Position is ~p ~n",[gl:getIntegerv(?GL_PROGRAM_ERROR_POSITION_ARB)]),
	io:format("Are we using program ? ~p (number ~p) ~n",[gl:isProgramARB(?GL_FRAGMENT_PROGRAM_ARB),-1]),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_LIGHTING),
	%%gl:color3f(1,0,0),
	ok.
