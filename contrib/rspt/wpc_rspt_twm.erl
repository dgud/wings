%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_twm.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%


%
%
% Texture windows managaer, all texture here
%
% crashes on auto save
%
% TAINTED_MODULE


-module(wpc_rspt_twm).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2,export/1]).

-define(WINDOW_NAME,"RSPT texture editor").
-define(MENU_BAR_HEIGHT,20).
-define(BUTTON_HEIGHT,28).
-define(CL_GLLISTS,10).
-define(CL_VERTEX,1).
-define(CL_LINE,2).
-define(CL_FACE,3).
-define(CL_LOCKED,4).


%% very alpha ... 
-record(tw,{
		%% points
		glPoint,
		%% faces
		glFace,
		%% lines
		glLine,
		glLocked,
		glTexFaces,
		
		%%
		%% already partly obsolete		

		showMat=none, %% the material to show to viewer

		%% the scene GL calllist
		glScene,

		%% the x,y,z position (Z should be distance division based)
		x=-0.5,
		y=-0.5,
		z=1.0, %% was 1.5

		%% visible objects in compiled scene
		visobj,
		%% the edit mode (vertex | line | face | object)	
		%% current only support vertex
		mode=vertex,
		%% (textured | normal)
		vismode=normal,
		width=100,
		height=100,


		selection=[],
		
		%% popup menu handler
		popup_handler=none
		}).

%% record to handle drag requests, self is tw record
-record(drag,{sx=0,sy=0,x=0,y=0,self}).

%% handle arbitary action
-record(action,{self,handler=none,ostate}).

%% selection is
%% list of ( tuple of object number ? , gb tree of vertexes ) 
%%
%% each tex coordinate can be identified uniqely by face and edge
%%
%% 




%% toolbar state
-record(but,{mode=vertex}).

init()->
	true.


menu({tools},Menu)->
	%%io:format("Menu is ~p ~n",[Menu]),
	Menu ++ [separator,{"RSPT texturing tool",rspt_twm_open}];
%%menu({view},Menu)->
%%	io:format("{view}-> ~p ~n",[Menu]),
%%	Menu;
%% ignore
menu(N, Menu)->
	io:format("(wpc_rspt_twm) Menu ~p ~n",[N]),
	Menu.

%% not used
command({rspt_twm_debug,rspt_debug_report_selection},#st{sel=X}=St)->
	io:format("Selection is ~p ~n",[X]),
	St;
command({tools,rspt_twm_open},St)->
	%%io:format("Open window!! ~n",[]),
	tex_window(St),
	wings_wm:dirty();
	%%next;
%command({rspt_popupmenu,rspt_sphere_map},St)->
%	io:format("Resend ~p ~n",[wings_wm:this()]),
%	wings_wm:send(wings_wm:this(),rspt_sphere_map),
%	next;
command({rspt_popupmenu,Action},St)->
	io:format("Resend action from rspt_popupmenu ~p to ~p ~n",[Action,wings_wm:this()]),
	wings_wm:send(wings_wm:this(),{popup_menu,Action}),
	next;
command(X,_)->
	io:format("(wpc_rspt_twm) Command ~p ~n",[X]), 
	next.


tex_window(St)->
	wings_wm:delete(?WINDOW_NAME),
	{W,H0} = wings_wm:top_size(),
	%% skip button for now
	H = H0 - 3*?MENU_BAR_HEIGHT, %% -?BUTTON_HEIGHT-200,
	LFirst = gl:genLists(?CL_GLLISTS + wpc_rspt_ma:quantity(St)), %% how many lists
	Data0 = #tw{width=W,height=H, 
			glPoint=compile(St,?CL_VERTEX,LFirst,vertex),
			glLine=compile(St,?CL_LINE,LFirst,line),
			glFace=compile(St,?CL_FACE,LFirst,face),
			glLocked=compile(St,?CL_LOCKED,LFirst,locked),
			glScene=LFirst
		
		},

	%% set handler for the popupmenu
	Data1 = build_popup_handler(Data0),	
	Data = Data1,

	Op = {seq,push,{replace,fun(X)-> handle_twm(Data,X) end}},
	wings_wm:toplevel(list_to_atom(?WINDOW_NAME),?WINDOW_NAME,{0,2*?MENU_BAR_HEIGHT,highest},{W,H},[closable,
			%% skip, may not work properly in wings ??
			%%{toolbar,fun create_toolbar/3},
			menubar],Op),
	menubar().


build_popup_handler(#tw{}=Self)->

	Handler = lists:foldl(fun({Action,F},H)->
				gb_trees:insert(Action,F,H)
		    end,gb_trees:empty(),wpc_rspt_twc:popup_menu_register()),
	Self#tw{popup_handler=Handler}.

tex_recompile(#tw{glScene=LFirst}=Data)->
	St = wpc_rspt_wm:get_current_state(),
	NTW = 	Data#tw{ 
			glPoint=compile(St,?CL_VERTEX,LFirst,vertex),
			glLine=compile(St,?CL_LINE,LFirst,line),
			glFace=compile(St,?CL_FACE,LFirst,face),
			glLocked=compile(St,?CL_LOCKED,LFirst,locked),
			glScene=LFirst
	
		},
	{replace,fun(X)-> handle_twm(NTW,X) end}.	


compile(St,T,N,W)->
	gl:newList(N+T,?GL_COMPILE),
	wpc_rspt_twc:all(St,N+?CL_GLLISTS-1,W),
	gl:endList(),
	N+T.


%% handle move commands and so on
%handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_move_uv})->
%	io:format("Switching to rspt_move command!!~n",[]),
%	wings_io:grab(),
%	wings_io:warp(W div 2,H div 2),
%	{replace,fun(X)-> handle_action(#action{self=Self},X) end};
handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_move_uv})->
	
	actionHandler(Self,fun(XD,YD)-> fun({CU,CV},IU,IV)->
					%DU = IU/W,
					%DV = IV/H,
					XM = 3.0*XD/W,
					YM = -3.0*YD/H,
					{XM+IU,YM+IV}
				      end
			   end);

handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_scale_uv})->
	
	actionHandler(Self,fun(XD,_)-> fun({CU,CV},IU,IV)->
					%DU = IU/W,
					%DV = IV/H,
					Factor = 1.0 + 2*XD/W,
					{CU+Factor*(IU-CU),CV+Factor*(IV-CV)}
				      end
			   end);
handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_scale_uv_u})->
	
	actionHandler(Self,fun(XD,_)-> fun({CU,CV},IU,IV)->
					Factor = 1.0 + 2*XD/W,
					{CU+Factor*(IU-CU),IV}
				      end
			   end);
handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_scale_uv_v})->
	
	actionHandler(Self,fun(XD,_)-> fun({CU,CV},IU,IV)->
					Factor = 1.0 + 2*XD/W,
					{IU,CV+Factor*(IV-CV)}
				      end
			   end);
handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_join_uv})->
	
	actionHandler(Self,fun(XD,_)-> fun({CU,CV},IU,IV)->
					{CU,CV}
				      end
			   end);
handle_twm(#tw{width=W,height=H}=Self,{popup_menu,rspt_rotate_uv})->
	
	actionHandler(Self,fun(XD,_)-> 
					%%io:format("-------------------Rotating ~p ------------~n",[XD]),
				     fun({CU,CV},IU,IV)->
					L = math:sqrt((IU-CU)*(IU-CU)+(IV-CV)*(IV-CV)),
					Rad = math:atan2(IV-CV,IU-CU),
					Factor = (2*math:pi()*XD)/W,
					%%io:format("UV (~p,~p) Inverted (~p,~p)~n",[IU,IV,CU+L*math:cos(Rad),CV+L*math:sin(Rad)]),
					%%io:format("Factor ~p degree ~p  rad is ~p ~n",[Factor,360*Factor/(2*math:pi()),Rad]),
					{CU+L*math:cos(Factor+Rad),CV+L*math:sin(Factor+Rad)}
				      end
			   end);
%% handle the popupmenu
handle_twm(#tw{}=Self,{popup_menu,rspt_ycylinder_map})->
	cylinder_map(Self,ycylinder_map);
handle_twm(#tw{}=Self,{popup_menu,rspt_xcylinder_map})->
	cylinder_map(Self,xcylinder_map);
handle_twm(#tw{}=Self,{popup_menu,rspt_zcylinder_map})->
	cylinder_map(Self,zcylinder_map);
handle_twm(#tw{selection=Sel}=Self,{popup_menu,rspt_sphere_map})->
	%%io:format("Sphere map now!!~n",[]),
	OSt = wpc_rspt_wm:get_current_state(),

	wpc_rspt_twc:sphere_map(Sel),

	St0 = wpc_rspt_wm:get_current_state(),
	St = wpc_rspt_wm:undo_save(OSt,St0),

	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,St}),
	tex_recompile(Self); %% renew all graphics
handle_twm(#tw{selection=Sel,popup_handler=HLib}=Self,{popup_menu,Event})->
	Handler = gb_trees:get(Event,HLib),
		
	OSt = wpc_rspt_wm:get_current_state(),

	case catch(Handler(Sel)) of
		{'EXIT',What} ->
				ok;
		nothing ->
				ok;
		Unknown ->
				St0 = wpc_rspt_wm:get_current_state(),
				St = wpc_rspt_wm:undo_save(OSt,St0),
				wings_wm:dirty(),
				wings_wm:send(geom,{new_state,St}),
				tex_recompile(Self)
	end;


handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_400}})->
	zoom_set(Self,400);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_200}})->
	zoom_set(Self,200);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_100}})->
	zoom_set(Self,100);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_75}})->
	zoom_set(Self,75);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_50}})->
	zoom_set(Self,50);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_25}})->
	zoom_set(Self,25);
handle_twm(Self,{action,{rspt_tex_view,rspt_tex_view_10}})->
	zoom_set(Self,10);
handle_twm(Self,#keyboard{keysym=#keysym{unicode=$ }})->
	deselect_all(Self);
handle_twm(#tw{mode=vertex}=Self,#mousebutton{button=3,x=X,y=Y,state=?SDL_RELEASED})->
	popup_vertex_menu(Self,X,Y);
handle_twm(#tw{mode=vertex}=Self,#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED})->
	select_point(Self,X,Y);
handle_twm(#tw{}=Self,#mousebutton{button=4,state=?SDL_RELEASED})->
	zoom_step(Self,1.1);
handle_twm(#tw{}=Self,#mousebutton{button=5,state=?SDL_RELEASED})->
	zoom_step(Self,0.9);
handle_twm(#tw{}=Self,#mousemotion{state=2,xrel=XD,yrel=YD})->
	move_step(Self,XD,YD);
handle_twm(Self,close)->
	delete;
handle_twm(Self,redraw)->
	draw_twm(Self),
	keep;

handle_twm(Self,{action,{rspt_edit,rspt_undo}})->
	io:format("Doing undo!!!~n",[]),
	Ost = wings_undo:undo(wpc_rspt_wm:get_current_state()),
	update_state(Self,Ost),
	keep;
handle_twm(Self,{action,{rspt_edit,rspt_redo}})->
	io:format("Doing redo!!!~n",[]),
	Ost = wings_undo:redo(wpc_rspt_wm:get_current_state()),
	update_state(Self,Ost),
	keep;

handle_twm(#tw{}=Self,{action,{rspt_texture,Name}})->
	NState = Self#tw{showMat=Name},
	%%handle_twm(NState,redraw),
	wings_wm:dirty(),
	{replace, fun(X)-> handle_twm(NState,X) end};	

handle_twm(#tw{}=Self,{action,{rspt_tex_select,Name}})->
	NSelf = tex_select_fun(Self,Name),
	wings_wm:dirty(),
	{replace,fun(X)-> handle_twm(NSelf,X) end};

handle_twm(Self,{action,TAction})->
	St0 = wpc_rspt_wm:get_current_state(),
	io:format("TWM Sending ~p~n",[TAction]),
	case command(TAction,St0) of
		next->
			ok;
		NSt ->
			wpc_rspt_wm:set_current_state(NSt)
	end,
	keep;

handle_twm(Self,{current_state,_})->
	%% do not print current state!!
	keep;
handle_twm(Self,X)->
	io:format("TWM reporting ~p ~n",[X]),
	keep.


%% we entered drag mode in view (should drop into handle_twm on released
handle_drag(#drag{sx=SX,sy=SY,x=X,y=Y,self=S},redraw)->
	draw_twm(S),
	%% draw a line around area

	%% xor line is not visible here ... skipping
	%%gl:enable(?GL_COLOR_LOGIC_OP),
	gl:logicOp(?GL_XOR),
	gl:'begin'(?GL_LINE_LOOP),
	gl:color3f(1,1,1),
	gl:vertex3f(SX,SY,0),
	gl:vertex3f(SX,Y,0),
	gl:vertex3f(X,Y,0),
	gl:vertex3f(X,SY,0),
	gl:'end'(),
	gl:disable(?GL_COLOR_LOGIC_OP),
	keep;
handle_drag(#drag{self=Self}=Drag,#mousemotion{x=X,y=Y,xrel=XD,yrel=YD})->
	{TX,TY} = translate_point(Self,X,Y),
	ND = Drag#drag{x=TX,y=TY},
	wings_wm:dirty(),
	{replace,fun(X)->  handle_drag(ND,X) end};
handle_drag(#drag{sx=SX,sy=SY,x=X,y=Y,self=#tw{mode=vertex,selection=Sel}=Self},#mousebutton{button=1,x=WX,y=WY,state=?SDL_RELEASED})->
	%%{X,Y} = translate_point(Self,WX,WY),

	SelSet = gb_sets:from_list(Sel),
	NSelf = case {SX < X,SY > Y} of
			{true,true} ->
				%% include
				Ents = wpc_rspt_twc:select_verts(wpc_rspt_wm:get_current_state(),{SX,X,Y,SY}),
				%%io:format("Ents is ~p ~n",[Ents]),
				ESet = gb_sets:from_list(Ents),
				Self#tw{selection=gb_sets:to_list(gb_sets:union(ESet,SelSet))};
			{false,false} ->
				%% exclude
				Ents = wpc_rspt_twc:select_verts(wpc_rspt_wm:get_current_state(),{X,SX,SY,Y}),	
				ESet = gb_sets:from_list(Ents),
				Self#tw{selection=gb_sets:to_list(gb_sets:difference(SelSet,ESet))};
			{_,_} ->
				Self
		end,	

	wings_wm:dirty(),
	{replace,fun(X)-> handle_twm(NSelf,X) end};
handle_drag(Drag,_)->
	keep.	



handle_action(#action{self=S},redraw)->
	draw_twm(S),
	keep;
handle_action(#action{handler=none,self=Self},#mousemotion{xrel=XREL,yrel=YREL,x=X,y=Y})->

	#tw{selection=Sel,width=W,height=H,z=Z}=Self,	
	XD = XREL,%% - (W div 2),
	YD = YREL,%% - (H div 2),
	case {XD,YD,abs(XD)>20,abs(YD)>20} of
		
		{_,_,true,_} ->
			keep;
		{_,_,_,true} ->
			keep;
		{0,0,_,_} ->
			keep;
		_ ->
			%%io:format("Warping ~p ~p ~n",[XD,YD]),
			wpc_rspt_twc:iterator_funmapUV(Sel,fun(_,U,V)->
					{U + 5*Z*XD/W,V + 5*Z*-YD/H}
				      end),	
			wings_io:warp(W div 2,H div 2),
			wings_wm:dirty(),
		keep
	end;
handle_action(#action{handler=Handler,self=Self},#mousemotion{xrel=XREL,yrel=YREL,x=X,y=Y})->

	#tw{selection=Sel,width=W,height=H,z=Z}=Self,	
	XD = XREL,%% - (W div 2),
	YD = YREL,%% - (H div 2),
	case {XD,YD,X==(W div 2),Y==(W div 2)} of
		
		{_,_,true,_} ->
			keep;
		{_,_,_,true} ->
			keep;
		{0,0,_,_} ->
			keep;
		_ ->
			%%io:format("Warping ~p ~p ~n",[XD,YD]),
			wpc_rspt_twc:iterator_funmapUV(Sel,Handler(XD,YD)),	
			wings_io:warp(W div 2,H div 2),
			wings_wm:dirty(),
		keep
	end;					
handle_action(#action{self=S,ostate=OSt},#mousebutton{button=1,state=?SDL_RELEASED})->
	#tw{width=Width,height=Height} = S,
	wings_io:ungrab(Width div 2,Height div 2),
	
	St0 = wpc_rspt_wm:get_current_state(),
	St = wpc_rspt_wm:undo_save(OSt,St0),
	
	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,St}),

	tex_recompile(S),
	{replace,fun(X)-> handle_twm(S,X) end};	
handle_action(#action{self=S,ostate=OSt},#mousebutton{button=3,state=?SDL_RELEASED})->

	io:format("Returning to old state!!!~n",[]),
	#tw{width=Width,height=Height} = S,
	wings_io:ungrab(Width div 2,Height div 2),

	wpc_rspt_wm:set_current_state(OSt),	
	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,OSt}),

	tex_recompile(S),
	{replace,fun(X)-> handle_twm(S,X) end};	

handle_action(S,{current_state,_})->
	keep;
handle_action(S,W)->
	io:format("(wpc_rspt_twm) Action reporting ~p ~n",[W]),
	keep.

draw_twm(#tw{glPoint=P,glFace=F,glLine=L,glLocked=LC,width=W,height=H,vismode=VM,x=X,y=Y,z=Z,showMat=ShowMat,selection=Sel}=Self)->
	wings_io:ortho_setup(),
	RGB = wings_pref:get_value(menu_color),
	wings_io:border(0,0,W-1,H-1,RGB),
	%% should call the scene call list
	ortho_setup(Self),

	texture_setup(ShowMat,wpc_rspt_wm:get_current_state(),X,Y,Z),


	gl:'begin'(?GL_POLYGON),
	gl:color3f(0.5,0.5,0.5),
	gl:vertex3f(-0.5,-0.5,0),
	gl:vertex3f(0.5,-0.5,0),
	gl:vertex3f(0.5,0.5,0),
	gl:vertex3f(-0.5,0.5,0),
	gl:'end'(),

	texture_off(ShowMat),


	gl:scalef(1.0/Z,1.0/Z,1.0),
	gl:translatef(X,Y,0),


	gl:color3f(0,0,0),
	gl:callList(LC),

	gl:color3f(1,1,1),
	%% should modify the size of pixels
	gl:pointSize(6.0),
	gl:callList(P),
	gl:pointSize(1.0),
	%% only if rendering textures and then we should use 
	%%gl:callList(F),
	gl:callList(L),	

	%% calls using selection
	gl:pointSize(6.0),
	gl:color4f(1,0,0,1),
	wpc_rspt_twc:render_selection(wpc_rspt_wm:get_current_state(),Sel),
	gl:pointSize(1.0),
	

	ok.
		


%%
%% Texture setup
%%
texture_setup(none,St,X,Y,Z)->
	ok;
texture_setup(Name,#st{mat=Mat}=St,X,Y,Z)->


	io:format("Rendering texture ~p ~n",[Name]),

	wpc_rspt_ma:texture(proplists:get_value(maps,gb_trees:get(Name,Mat))),	

	gl:enable(?GL_TEXTURE_2D),
	gl:matrixMode(?GL_TEXTURE),
	gl:pushMatrix(),
	gl:loadIdentity(),

	gl:translatef(-X,-Y,0),
	gl:scalef(Z,Z,1),	



	gl:matrixMode(?GL_MODELVIEW),	
	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_LINEAR),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),	
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_REPEAT),
	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_REPEAT),
	gl:enable(?GL_TEXTURE_GEN_S),
	gl:enable(?GL_TEXTURE_GEN_T),
	
	gl:texGenfv(?GL_S,?GL_OBJECT_LINEAR,{1,0,0,0}),
	gl:texGenfv(?GL_T,?GL_OBJECT_LINEAR,{0,1,0,0}),
	

	ok.

texture_off(none)->
	ok;
texture_off(Name)->
	gl:matrixMode(?GL_TEXTURE),
	gl:popMatrix(),
	gl:matrixMode(?GL_MODELVIEW),

	gl:disable(?GL_TEXTURE_GEN_S),
	gl:disable(?GL_TEXTURE_GEN_T),
	gl:bindTexture(?GL_TEXTURE_2D,0),
	gl:disable(?GL_TEXTURE_2D),
	gl:color4f(1,1,1,1), %% miscolor ?
	ok.

%%
%% Assumes that we have called wings_io:ortho_setup 
%%
ortho_setup(#tw{width=W,height=H,x=X,y=Y,z=Z})->
	
	

	gl:matrixMode(?GL_PROJECTION),

	%% reduce ortho setup to normalized values
	gl:translatef(W/2,H/2,0),
	gl:scalef(W,-H,1),

	%% th stuff below should be the size control in window

	%%gl:translatef(0,H,0),
	%% reverse direction of textures (proper gl direction)
	%%gl:scalef(W,-H,1),
	%%gl:scalef((W),H-80,1),
	%%gl:disable(?GL_CULL_FACE),
	ok.



%%
%% Menu
%%
menubar()->
	Menus = [{"Texture",rspt_texture,fun(St)-> texture_menu(St) end},
		 {"Edit",rspt_edit,fun(St)-> editMenu(St) end},
		{"View",rspt_tex_view,fun(St)-> viewMenu(St) end},
		{"Select",rspt_tex_select,fun(St)->selectMenu(St) end},
		{"Debug",rspt_twm_debug,fun(St)-> debugMenu(St) end}


		],
	wings_wm:menubar(list_to_atom(?WINDOW_NAME),Menus).


editMenu(St)->	
	[{"Undo",rspt_undo,"Undo last step"},
	 {"Redo",rspt_redo,"Redo last step"}].

%texture_menu(X)->
%	io:format("Bogus call to texture menu!! recieved ~p ~n",[X),
%	[{"Bogus menu!!~n",[]}];
texture_menu(Ignore)->
	#st{mat=Mat}= wpc_rspt_wm:get_current_state(),
	io:format("Texture menu!!~n",[]),
	%% build menu using material data keys
	Names = gb_trees:keys(Mat),
	Res = [{"none",none,"Deselect material"}] ++ [{atom_to_list(El),El,"Selects material to display"} || El <- Names],
	io:format("Texture menu ~p ~n",[Res]),
	Res.


debugMenu(St)->
	[{"Report selection",rspt_debug_report_selection,"Print current selection to window"}].

selectMenu(St)->
	[{"Deselect all",rspt_deselect_all,"Deselect all uv points"},
	 {"Invert selection",rspt_invert_selection,"Invert selected uv points"},
	 {"Select all",rspt_select_all,"Select all uv points"},
	 {"Select X positive",rspt_select_xpos,"Select all uv point with x>0"}, 
         {"Select X negative",rspt_select_xneg,"Select all uv point with x<=0"}, 
	 {"Select Y positive",rspt_select_ypos,"Select all uv point with y>0"}, 
         {"Select Y negative",rspt_select_yneg,"Select all uv point with y<=0"}, 
	 {"Select Z positive",rspt_select_zpos,"Select all uv point with z>0"}, 
         {"Select Z negative",rspt_select_zneg,"Select all uv point with z<=0"},
	 {"Select undefined",rspt_select_undef,"Select uv points with no value"},
	 {"Select clockwise",rspt_select_clockwise,"Select uv faces wich are clockwise defined"},
	 {"Select faces",rspt_select_faces,"Select uv points from faces selected in wings window"},
	 {"Select facing up",rspt_select_faces_up,"Select faces facing the up direction"},
	 {"Select facing down",rspt_select_faces_down,"Select faces facing the down direction"},
	 {"Select facing west",rspt_select_faces_west,"Select faces facing the west direction"},
	 {"Select facing east",rspt_select_faces_east,"Select faces facing the east direction"},
	 {"Select facing north",rspt_select_faces_north,"Select faces facing the north direction"},
	 {"Select facing south",rspt_select_faces_south,"Select faces facing the south direction"},
	 {"Select uv neighbours",rspt_select_neighbours,"Select all uv points on selected faces"}
	].
	

viewMenu(St)->
	[{"400% view",rspt_tex_view_400,"Zoom %400"},
	 {"200% view",rspt_tex_view_200,"Zoom %200"},
	 {"100% view",rspt_tex_view_100,"Zoom %100"},
	 {"75% view",rspt_tex_view_75,"Zoom %75"},
	 {"50% view",rspt_tex_view_50,"Zoom %50"},
	 {"25% view",rspt_tex_view_25,"Zoom %25"},
	 {"10% view",rspt_tex_view_10,"Zoom %10"}].	

placeholderMenu(St)->
	[{"Nothing",nothing,"Empty menu"}].


%% needs changing
selectionMenu(St)->
	ok.

create_toolbar(Name,Pos,W)->
	BHeight = ?BUTTON_HEIGHT + 6,
	io:format("Toolbar Name is ~p W=~p Pos=~p ~n",[Name,W,Pos]),
	wings_wm:new(Name,Pos,{W,BHeight},init_button()).

init_button()->
	{seq,push,get_button_event(#but{})}.

get_button_event(St)->
	{replace,fun(Ev)-> button_event(St,Ev) end}.

button_event(St,redraw)->
	wings_io:ortho_setup(),
	{W,H} = wings_wm:win_size(),
	wings_io:border(0,0,W-1,H-1,{0,1.0,1.0}),
	keep;
button_event(St,Ev)->
	io:format("Button event ~p ~n",[Ev]),
	keep.

zoom_step(#tw{z=Z}=Self,N)->
	wings_wm:dirty(),
	NSelf = Self#tw{z=Z*N},
	{replace,fun(X)-> handle_twm(NSelf,X) end}.	


zoom_set(#tw{z=Z}=Self,N)->
	wings_wm:dirty(),
	NSelf = Self#tw{z=100.0/N,x=-0.5,y=-0.5},
	{replace,fun(X)-> handle_twm(NSelf,X) end}.	



translate_point(#tw{x=TX,y=TY,z=Z,width=W,height=H}=Self,X,Y)->
	XP = Z * ((X/W) -0.5) - TX,
	YP = Z * ((1.0-Y/H) -0.5) - TY,
	io:format("Pressed point {~p,~p}~n",[XP,YP]),
	{XP,YP}.

move_step(#tw{x=X,y=Y,z=Z,width=W,height=H}=Self,XD,YD)->
	%%{OW,OH} = wings_wm:win_size(),
	NXD = 2*Z *(XD/W),
	NYD = 2*Z *(YD/H),
	%%io:format("Width=~p Win_Width=~p Xrel=~p Zoom=~p final=~p ~n",[W,OW,XD,Z,NXD]),
	NSelf = Self#tw{x=X+NXD,y=Y-NYD},
	wings_wm:dirty(),
	{replace,fun(X)-> handle_twm(NSelf,X) end}.

select_point(#tw{z=Z,selection=CSel}=Self,X,Y)->
	Val = 0.01,
	{XT,YT} = translate_point(Self,X,Y),
	Point = wpc_rspt_twc:select_vert(wpc_rspt_wm:get_current_state(),{XT-Val*Z,XT+Val*Z,YT-Val*Z,YT+Val*Z}),
	io:format("Selected uvc point ~p ~n",[Point]),
	case Point of
		none ->
			%% got a drag request
			{replace,fun(X)-> handle_drag(#drag{sx=XT,sy=YT,x=XT,y=YT,self=Self},X) end};
			%%keep;
		NSel ->
			%%NS = Self#tw{selection=[NSel|CSel]},
			NS = Self#tw{selection=xor_list(NSel,CSel)},
			wings_wm:dirty(),
			{replace,fun(X)-> handle_twm(NS,X) end}
	end.

deselect_all(Self)->
	NS = Self#tw{selection=[]},
	wings_wm:dirty(),
	{replace,fun(X)-> handle_twm(NS,X) end}.


xor_list(EL,[])->
	[EL];
xor_list(EL,[EL|T])->
	T;
xor_list(EL,[A|T])->
	[A|xor_list(EL,T)].



popup_vertex_menu(#tw{}=Self,X,Y)->
	wings_menu:popup_menu(X,Y,rspt_popupmenu,	
		wpc_rspt_twc:popup_menu_entries(

		[
		 {"Move uv points",rspt_move_uv,"Move uv coordinates in view"},
		 {"Scale uv points",rspt_scale_uv,"Scale uv points in view"},
		 {"Scale uv points in u",rspt_scale_uv_u,"Scale uv points in view"},
		  {"Scale uv points in v",rspt_scale_uv_v,"Scale uv points in view"},
		 {"Rotate uv points",rspt_rotate_uv,"Scale uv points in view"},
		 {"Sphere map",rspt_sphere_map,"Sphere map selected UV coordinates"},
		 {"Cylinder Y map",rspt_ycylinder_map,"Cylinder map selected UV coordinates"},
		 {"Cylinder Z map",rspt_zcylinder_map,"Cylinder map selected UV coordinates"},
		 {"Cylinder X map",rspt_xcylinder_map,"Cylinder map selected UV coordinates"}
		 
		]) ),
	wings_wm:dirty(),
	keep.


cylinder_map(#tw{selection=Sel}=Self,A)->
	io:format("undo save enabled!!~n",[]),
	%%io:format("Cylinder ~p map now!!~n",[A]),
	OSt = wpc_rspt_wm:get_current_state(),

	wpc_rspt_twc:A(Sel),

	St0 = wpc_rspt_wm:get_current_state(),
	St = wpc_rspt_wm:undo_save(OSt,St0),
	
	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,St}),
	tex_recompile(Self). %% renew all graphics


tex_select_fun(#tw{selection=Sel}=Self,rspt_deselect_all)->
	Self#tw{selection=[]};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_all)->
	Ents = wpc_rspt_twc:select_verts(wpc_rspt_wm:get_current_state(),{-10000000.0,100000000.0,-100000000.0,10000000000.0}),
	Self#tw{selection=Ents};
tex_select_fun(#tw{selection=Sel}=Self,rspt_invert_selection)->
	Self#tw{selection=wpc_rspt_twc:select_invert(wpc_rspt_wm:get_current_state(),Sel)};


tex_select_fun(#tw{selection=Sel}=Self,rspt_select_xpos)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> X > 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_ypos)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> Y > 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_zpos)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> Z > 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_xneg)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> X =< 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_yneg)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> Y =< 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_zneg)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({{X,Y,Z},_})-> Z =< 0.0001 end)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_undef)->
	Self#tw{selection=wpc_rspt_twc:select_by_fun(wpc_rspt_wm:get_current_state(),Sel,
		fun({_,{U,V}})-> 
			(U == 0) and (V==0) 
		  end
		)};

tex_select_fun(#tw{selection=Sel}=Self,rspt_select_clockwise)->
	Self#tw{selection=wpc_rspt_twc:select_clockwise(wpc_rspt_wm:get_current_state(),Sel)};

tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces)->
	Self#tw{selection=wpc_rspt_twc:select_selection(wpc_rspt_wm:get_current_state(),Sel)};

tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_up)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				-Y >= 0.5
			end
			)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_down)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				Y >=  0.5
			end
			)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_west)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				X >= 0.5
			end
			)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_east)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				-X >= 0.5
			end
			)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_north)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				-Z >= 0.5
			end
			)};
tex_select_fun(#tw{selection=Sel}=Self,rspt_select_faces_south)->
	Self#tw{selection=wpc_rspt_twc:select_normal(wpc_rspt_wm:get_current_state(),Sel,
			fun({X,Y,Z})->
				Z >= 0.5
			end
			)};
			

tex_select_fun(#tw{selection=Sel}=Self,rspt_select_neighbours)->
	Self#tw{selection=wpc_rspt_twc:select_neighbours(wpc_rspt_wm:get_current_state(),Sel)};

tex_select_fun(Self,Name)->
	io:format("Ignoring selection ~p  ~n ",[Name]),
	Self.


actionHandler(#tw{width=W,height=H}=Self,FF)->
	%%io:format("Switching to rspt_move command!!~n",[]),
	wings_io:grab(),
	wings_io:warp(W div 2,H div 2),
	State = wpc_rspt_wm:get_current_state(),
	{replace,fun(X)-> handle_action(#action{self=Self,handler=FF,ostate=State},X) end}.


update_state(Self,St)->
	io:format("Equal Undo state with present (~p)!!~n",[St==wpc_rspt_wm:get_current_state()]),
	wpc_rspt_wm:set_current_state(St),
	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,St}),
	tex_recompile(Self),
	St.


%%
%%Export state to geom handler
%%(used by twc
export(FF)->
	%%io:format("Doing export !!~n",[]),
	OSt = wpc_rspt_wm:get_current_state(),
	%%io:format("Going for FF ~p~n",[FF]), 
	Ans = (catch FF()),
	
	%%io:format("Executing FF result (~p)~n",[Ans]),
	St0 = wpc_rspt_wm:get_current_state(),
	St = wpc_rspt_wm:undo_save(OSt,St0),
	wpc_rspt_wm:set_current_state(St),
	
	wings_wm:dirty(),
	wings_wm:send(geom,{new_state,St}).
	
