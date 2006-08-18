%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_ps.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%%
%%%% plugin module handler, will handle plugin loading and execution ..
%%%%
%%%%

-module(wpc_rspt_ps).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2]).

-export([get_mat/2,get_mat/1,handle_event/1,getPrefixed/1,help_window/3,get_pref/2,set_pref/1,is_enabled/0]).
-export([material_make_default/2,unique_name/2]).

%% link into app
init()->
	%% since the console is visible have a legend here, and in application
	io:format("                                                             ~n",[]),
	io:format("                                                             ~n",[]),
	io:format("                                                             ~n",[]),
	io:format("        XXXXXXXXXXX                                          ~n",[]),
	io:format("        XXX      XXX                                         ~n",[]),
	io:format("        XXX      XXX                                         ~n",[]),
	io:format("        XXX     XXX             RSPT version 0.1             ~n",[]),
	io:format("        XXXXXXXXX                                            ~n",[]),
	io:format("     XXXXXXXXXXXXX              Wings plugin system          ~n",[]),
	io:format("   XXX  XXX     XXX                                          ~n",[]),
	io:format("  XXX   XXX      XXX                                         ~n",[]),
	io:format("  XXX   XXX       XXX           Status: in development       ~n",[]),
	io:format("   XXX  XXX        XXX                                       ~n",[]),
	io:format("     XXXXXX                                                  ~n",[]),
	io:format("          XXX                                                ~n",[]),
	io:format("           XXX                                               ~n",[]),
	io:format("          XXX                                                ~n",[]),
	io:format("         XXX                                                 ~n",[]),
	io:format("   XXXXXXX                                                   ~n",[]),
	io:format("                                                             ~n",[]),
	io:format("                                                             ~n",[]),
	io:format("                                                             ~n",[]),
	true.

menu({edit,plugin_preferences},Menu)->
	Menu ++ [{"RSPT",rspt_ps}];
menu({help},Menu)->
	Menu ++ [separator,{"About RSPT",rspt_about},{"RSPT help",rspt_help,"Documentation for the rspt plugin system"}];	
menu(X, Menu)->
	io:format("Called with ~p ~n",[X]),
	Menu.

command({edit,{plugin_preferences,rspt_ps}},St)->
	pref_edit(St);
command({help,rspt_about},St)->
	about();
command({help,rspt_help},St)->
	help();
command(X,_)->
	%%io:format("Comman ~p ~n",[X]), 
	next.


%%%%%%
%%%%%% other funs
%%%%%%

%%
%% get plugins that have material defines
%%
%% L: amb | diff
%% X: pass 0-4
get_mat(L,X)->
	%%io:format("wpc_rspt_ps get_mat called ~n",[]),
	%%case catch wings_plugin:menu(list_to_atom("rspt_material_menu_" ++ L ++ "FragProg" ++ [$0 + X]),[]) of
	%%	M ->	
	%%		io:format("The menu processed is ~p ~n",[M]),
	%%		M
	%%end.
	wings_plugin:menu(list_to_atom("rspt_material_menu_" ++ L ++ "FragProg" ++ [$0 + X]),[]).

%%
%%
%% get plugins that have cpu pipline defines
%%
%%
get_mat(L)->
	wings_plugin:menu(list_to_atom("rspt_material_menu_" ++ L),[]).


%%
%% get all plugins with prefix rspt_* something where * is L and string
%%
%%
getPrefixed(L)->
	%%io:format("wpc_rspt_ps called ~p sending ~n",[list_to_atom("rspt_" ++ L)]),
	wings_plugin:menu(list_to_atom("rspt_" ++ L),[]).

%%
%% About fun
%%
%%
about()->
	Xs         = 200,
	Ys         = 200,
	{W,H}      = wings_wm:top_size(),
	X          = trunc((W-Xs)/2),
	Y          = trunc((H-Ys)/2),
	Op         =  {push,fun handle_event/1},
	wings_wm:new(rspt_about_window,{X,Y,highest},{Xs,Ys}, Op),
	wings_wm:grab_focus(help),
	wings_wm:dirty(),
	keep.


%% defined elsewhere
%%-record(mousebutton,{a1,a2,a3,a4,a5}).

handle_event(redraw)->
	wings_io:ortho_setup(),
	{Xs,Ys} = wings_wm:win_size(),
	wings_io:raised_rect(0,0,Xs,Ys),
	gl:color3f(0,0,0),
	gl:recti(3,3,Xs-3,Ys-3),
	gl:color3f(0,0,0),
	gl:recti(4,4,Xs-4,Ys-4),


	gl:pushAttrib(?GL_TEXTURE_BIT),

	gl:enable(?GL_TEXTURE_2D),
	gl:bindTexture(?GL_TEXTURE_2D,0),
	gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGB,32,16,0,?GL_RGB,?GL_UNSIGNED_BYTE,icon()),	
	gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_MODULATE),
	gl:disable(?GL_LIGHTING),
	%% will look terribly i think
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_NEAREST),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_NEAREST),	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP),	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP),

	gl:shadeModel(?GL_SMOOTH),

	gl:'begin'(?GL_POLYGON),
	gl:color3f(0,0,0),
	gl:texCoord2f(0,1),
	gl:vertex2i(12,120),
	gl:texCoord2f(0,0),
	gl:vertex2i(12,12),
	gl:color3f(1,1,1),
	gl:texCoord2f(0.75,0),
	gl:vertex2i(Xs-12,12),
	gl:texCoord2f(0.75,1),
	gl:color3f(0,0,0),
	gl:vertex2i(Xs-12,120),
	gl:'end'(),
	gl:color3f(1,1,1),

	gl:shadeModel(?GL_FLAT),
	gl:disable(?GL_TEXTURE_2D),

	gl:popAttrib(),
	gl:color3f(1,1,1),
	%%wings_io:text_at(80,85,"RSPT"),
	wings_io:text_at(10,140,"Rolf Stenholm Power Tools"),
	wings_io:text_at(10,165,"        Version 0.1      "),
	gl:enable(?GL_COLOR_LOGIC_OP),
	gl:logicOp(?GL_XOR),
	gl:color3f(1,1,1),
	gl:recti(4,4,trunc(Xs/2),Ys-4),
	%%gl:recti(4,4,Xs-4,Ys-4),
	gl:disable(?GL_COLOR_LOGIC_OP),
	keep;
handle_event(#mousebutton{})->
	delete;
handle_event(X)->
	io:format("Event ~p ~n",[X]),
	keep.


%% icon as binary
icon()->
	%%io:format("Fetching data ~n",[]),
	Data = data_conv(icon_data()),
	%%io:format("Building binary .. from ~p ~n",[Data]),
	list_to_binary(Data).

data_conv([$ |T])->
	[0,0,0 |data_conv(T)];
data_conv([_|T])->
	[255,255 ,255 | data_conv(T)];
data_conv([])->
	[].

icon_data()->
	"        XXXXXXXXXXX             "++
	"        XXX      XXX            "++
	"        XXX      XXX            "++
	"        XXX     XXX             "++
	"        XXXXXXXXX               "++
	"     XXXXXXXXXXXXX              "++
	"   XXX  XXX     XXX             "++
	"  XXX   XXX      XXX            "++
	"  XXX   XXX       XXX           "++
	"   XXX  XXX        XXX          "++
	"     XXXXXX                     "++
	"          XXX                   "++
	"           XXX                  "++
	"          XXX                   "++
	"         XXX                    "++
	"   XXXXXXX                      ".





%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Help function, should link with plugin documents ..
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%


%%
%% This function should query all plugins for rspt_help function and add a link {module,Answer} if no error ..
%% doing it later ...
%%
help()->
	wpc_rspt_wm:help_window(rspt_help_window,"RSPT Help",
		["This is the help documentation for Rolf Stenholm Power Tools. "
%% skip this madness
		"Click on links to view specific topic. ",""
%	,
%	"........XXXXXXXXXXX.............",
%	"........XXX......XXX............",
%	"........XXX......XXX............",
%	"........XXX.....XXX.............",
%	"........XXXXXXXXX...............",
%	".....XXXXXXXXXXXXX..............",
%	"...XXX..XXX.....XXX.............",
%	"..XXX...XXX......XXX............",
%	"..XXX...XXX.......XXX...........",
%	"...XXX..XXX........XXX..........",
%	".....XXXXXX.....................",
%	"..........XXX...................",
%	"...........XXX..................",
%	"..........XXX...................",
%	".........XXX....................",
%	"...XXXXXXX......................"





		],
		getPrefixed("help")).


%%% defect why??
help_window(Title,Text,Links)->
	wpc_rspt_wm:help_window(rspt_help_window,Title,Text,Links).


%% the misc stuff (same as in re but should only be here)

get_pref(Key,Def)->
	wpa:pref_get(?MODULE,Key,Def).
	
set_pref(KeyVals) ->
	wpa:pref_set(?MODULE,KeyVals).


%%% is this plugin enabled ??
is_enabled()->
	get_pref(enabled,true).



%%%
%%% pref edit stuff
%%%

pref_edit(St)->
	Enabled = get_pref(enabled,true),
	wpa:dialog("RSPT preferences",
			[{hframe,[{"Enabled",Enabled,[{key,enabled}]}]}],
			fun(Attr)-> set_pref(Attr),St end).


material_make_default(X,Y)->
	%%wings_material:make_default(X,Y).
	%% as in wings_material , but here since make_default invisible
	{R,G,B} = X,
	Color = {R,G,B,Y},
	White = {1.0,1.0,1.0,1.0},
	Mat = [{opengl,[{diffuse,Color},{ambient,Color},{specular,White},
		{emission,{0.0,0.0,0.0,0.0}},{shininess,1.0}]},
		{maps,[]}],
	%% note rspt uses unsorted values ..
	lists:sort([{K,lists:sort(L)} || {K,L} <- Mat]).
		

unique_name(X,Y)->
	wings_util:unique_name(X,Y).

