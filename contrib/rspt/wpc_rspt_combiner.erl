%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_combiner.erl,v 1.1 2004/08/25 05:33:05 bjorng Exp $
%%

%%%
%%% Module to combine results
%%%
%%%

-module(wpc_rspt_combiner).
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
	[{"Combiner",wpc_rspt_combiner}] ++ Menu;
menu(rspt_material_menu_diffFragProg0,Menu)->
	[{"Combiner",wpc_rspt_combiner}] ++ Menu;
menu(rspt_help,Menu)->
	[{"Combining textures",{wpc_rspt_combiner,help}}] ++ Menu;
menu(X, Menu)->
	%%io:format("Ignoring menu ~p ~n",[X]),
	Menu.


command(_,_)-> next.


help()->
	wpc_rspt_wm:help_window(wpc_rspt_skinshader_help_window,"Skin shader",
				["Combiner",
				 "Experimental texture combiner"],[]).

material_opts([S,diffFragProg0|Tail])->
	material_init(diffFragProg0,diffFragProg0Opts,[diffFragProg0Opts|Tail]),
	ok;
material_opts([S,ambFragProg0|Tail])->
	material_init(ambFragProg0,ambFrag0Opts,[ambFragProg0Opts|Tail]),
	ok.


material_init(NSel,NOpts,Target)->
	io:format("Setting the path ~p ~n",[Target]),
	wpc_rspt_ma:path_set([],[object|Target]),
	wpc_rspt_ma:path_set(add,[object,combiner|Target]),
	wpc_rspt_ma:path_set(nothing,[object,combinerOpts|Target]),
	wpc_rspt_ma:path_set(standard,[object,NSel,componentA|Target]),
	wpc_rspt_ma:path_set(nothing,[object,NOpts,componentA|Target]),
	wpc_rspt_ma:path_set(standard,[object,NSel,componentB|Target]),
	wpc_rspt_ma:path_set(nothing,[object,NOpts,componentB|Target]),
	ok.





attrib(V,ProgID)->
	ok.
