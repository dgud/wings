%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_wm.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%%%%
%%%%% RSPT window manager, anything to wings specific that may change goes here
%%%%%
%%%%%
%%%%% TAINTED_MODULE

-module(wpc_rspt_wm).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

%% replacing the raw insert
-define(INDENT_FAC,10).
-define(INDENT_OFF,8).

-export([init/0,menu/2,command/2]).

%%
%% An alternative windows specification is realy needed here ...
%%
-export([help_window/4,handle_help_window/2,window/5,window/7,window/8]).
-export([get_current_state/0,set_current_state/1]).
-export([legend_material/1]).
-export([chain_register/2]).
-export([undo_save/2]).
-export([tx_grab/3,tx_getPos/1,tx_setPos/2,tx_getName/1,tx_getText/1,tx_setText/2]).
-export([clamp/3]).

init()->
	true.


%% ignore
menu(_, Menu)->
	Menu.

%% not used
command(_,_)->
	%%io:format("Comman ~p ~n",[X]), 
	next.

%%
%% Will double for general window configuration
%%
%%
%% has a field of links on bottom of window
%% the scrollbar should perhaps be skipped if window and contents fit
%%
%% TREE = "{" ATOM "," "[" TREE | "" | OTHER "]" "}"
%% OTHER anything else that does not appear to be a TREE
%% ATOM erlang atom 
%%
%% tree field should be a function (monitor)
%%
-record(hw,{lines,rows,crr,txh,tw,th,wh,links,tree,vist,style=wings,legend,legendY=0,
	  
%%
%%The text cursor and text focus
%%Elems should be able to grab key focus
%%Elems should have an additonal field for tracking sub field of target
%%
%%tx_focus path to send key events to, none if no focus
%%tx_name  text field name 
%%tx_pos   text field position, zero if before all text
%%tx_text  text that is illegal for field or none
%%
%%tx_grab(HW,Path,Name) grab all key events and send to path 'path' with name 'name'
%%tx_getPos(HW)        get postion
%%tx_setPos(HW,X)       set position
%%tx_getText(HW)       get text
	tx_focus=none,tx_name,tx_pos=0,tx_text=none,
%%
%% The tree event server
%% one server for EVERY event 
%%
%% Server setup is this
%%
%% tobj   %% the tree object (skipping all other stuff around)
%%
%%
%% click       gb_tree of events and funs dealing with click events
%%                fun({push,HW,Obj,X,Y}) where X is record hw (expose hw record ?? no dubious .. but okay for now)%%
%%                returns new hw value (may change st through rspt_wm as sideffect)
%%                A is the action to take.
%%                E will be an event parsed to be class relative ...
%%
%% ren         gb_tree of fun({render,D,Indent,Y})  that render something and returns height of rendered
%% height           gb_tree of height values in lines, that is height in pixels ?LINE_HEIGHT*gb_trees:get(.. ) 
%%                   fun(height)
%%
%%
%% Service call chain of action will be as this (Action Chain)
%%  [Service, PathN,..Path0]
%% Will call
%%    [Service, PathN,..,Path0]
%%         [PathN,..,Path0]
%%              [Service,PathN-1,..Path0]
%%                      [PathN-1,..,Path0]
%%                         ...
%%                       [Service,Path0]
%%                           [Path0]
%%                          [Service]
%%
%%  The service state should have be replaced before rendering, and all other 
%%   program paths should be skipped ... 
%%  Combiners will have to deal with the call chain mechanically ...
%%
%%
	tobj=gb_trees:empty()

	}).

%%
%% Links reference to other modules, should be two parameter tuples {text,atom}
%%
help_window(Name,Title,Text,Links)->
	%%io:format("Links are ~p ~n",[Links]),
	wings_wm:delete(Name),
	%% this does not handle links .. ignoring for now
	{Rows,Lines} = wings_text:break_lines(Text,60),
	{W,H} = wings_wm:top_size(),
	MaxH = trunc(0.75*H),
	Xs = 64 *?CHAR_WIDTH,
	Ys = case (Rows+length(Links))*?LINE_HEIGHT of
		Ys0 when Ys0 > MaxH -> MaxH;
		Ys0 -> Ys0
	      end,
	%% maybe use center function instead
	X  = trunc((W-Xs)/2),
	Y  = trunc((H-Ys)/2),
	Data = #hw{lines=Lines,rows=Rows,crr=0,tw=Xs,txh=Rows*?LINE_HEIGHT,th=(length(Links)+Rows+1)*?LINE_HEIGHT,wh=Ys,links=Links},
	Op = {seq,push,{replace,fun(X)-> handle_help_window(Data,X) end}},
	Size = {Xs + ?CHAR_WIDTH,Ys + ?LINE_HEIGHT},
	wings_wm:toplevel(Name,Title,{X,Y,highest},Size,[closable,vscroller],Op),
	wings_wm:dirty().



%%
%%
handle_help_window(Data,close)->
	delete;
handle_help_window(#hw{lines=Lines,links=Links,crr=Crr}=HW,redraw)->
	%%io:format("Lines ++ Links is ~p ~n",[Lines ++ Links]),	
	%% do something to the scroller
	update_scroller(HW),
	wings_io:ortho_setup(),
	{W,H} = wings_wm:win_size(),
	wings_io:border(0,0,W-1,H-1,{1.0,1.0,1.0}),
	gl:color3f(0,0,0),
	gl:translated(4,4+?LINE_HEIGHT,0),
	%%io:format("grabbing first line ~p ~n",[Crr]),
	OLines = lists:nthtail(Crr,Lines++Links),
	%%io:format("Done grabbing line!~n",[]),
	lists:foldl(fun(L,Y) ->
			case L of
				%% link
				{LT,_} ->
					gl:color3f(0,0,1),
					wings_io:text_at(5,Y,LT),
					gl:color3f(0,0,0);
				_ ->
					wings_io:text_at(5,Y,L)
			end,
			Y + ?LINE_HEIGHT
		end, 0,OLines),
	keep;
handle_help_window(#hw{th=Th,wh=Wh}=HW,{set_knob_pos,Pos})->
	%%io:format("Got set_knob_pos ~p ~n",[Pos]),
	Lines = Th div ?LINE_HEIGHT,
	VisLines = Wh div ?LINE_HEIGHT,
	HWN = HW#hw{crr=trunc(Lines*Pos)},
	update_scroller(HWN),
	{replace, fun(X)-> handle_help_window(HWN,X) end};
handle_help_window(#hw{wh=Wh,crr=Crr}=HW,scroll_page_up)->
	scroll_step(-Wh div ?LINE_HEIGHT,HW);
handle_help_window(#hw{wh=Wh,crr=Crr}=HW,scroll_page_down)->
	scroll_step(Wh div ?LINE_HEIGHT,HW);
handle_help_window(#hw{}=HW,#mousebutton{button=4,state=?SDL_RELEASED})->
	scroll_step(-10,HW);
handle_help_window(#hw{}=HW,#mousebutton{button=5,state=?SDL_RELEASED})->
	scroll_step(10,HW);
handle_help_window(#hw{crr=Crr,lines=Lines,links=Links}=HW,#mousebutton{button=1,state=?SDL_RELEASED,y=Y,x=X})->

	case grab_line_by_y(HW,X,Y) of
		{_,A} ->
					io:format("Calling new module by through command? name ~p ~n",[A]),
					io:format("~p~n----- ~n",[catch(A())]);
		_ ->
					none
	end,
	keep;
handle_help_window(#hw{}=HW,#mousemotion{x=X,y=Y})->
	case grab_line_by_y(HW,X,Y) of
		{_,_} ->
			%% set mouse to hand
			wings_io:set_cursor(pointing_hand);
		_ ->
			%% plain mouse
			wings_io:set_cursor(arrow)
	end,
	keep;
handle_help_window(Data,X)->
	%%io:format("Got ~p ~n",[X]),
	keep.

update_scroller(#hw{crr=Crr,th=Th}=HW)->
	{_,H} = wings_wm:win_size(),
	Name  = wings_wm:this(),
	wings_wm:set_knob(Name,Crr*?LINE_HEIGHT/Th,H/Th). 

scroll_step(Step,#hw{th=Th,wh=Wh,crr=Crr}=HW)->
	Lines = Th div ?LINE_HEIGHT,
	VisLines = Wh div ?LINE_HEIGHT,
	%% incorrect
	NCrr = case Crr+Step of
		Neg when Neg < 0 -> 
				0;
		Crr1 when Crr1 >= (Lines-VisLines) -> 
				Lines-VisLines;
		X ->
			X
	       end,
	NHW = HW#hw{crr=NCrr},
	update_scroller(NHW),
	{replace, fun(X)-> handle_help_window(NHW,X) end}.

		

%%
%% only Y is currently used
%%
grab_line_by_y(#hw{crr=Crr,lines=Lines,links=Links}=HW,X,Y)->
	%% need position
	Yline = (Y div ?LINE_HEIGHT)+Crr,
	TLines = length(Lines ++ Links),
	%%io:format("Mouse button one pressed at line ~p  total lines are ~p ~n",[Yline,TLines]),
	case Yline of
		Yl0 when (Yl0 >=TLines) ->
			tree;
		Yl1 when (Yl1) >= length(Lines) ->
			%%io:format("Clicked on line ~n",[]),
			% call new module and do nothing else
			case lists:nth(Yl1+1,Lines++Links) of
				{A,B} ->
					{A,B};
				_ ->
					no
			end;
		_->
			no
	end.






%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% Material editor  window
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%% General window, used by material editor window
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%


window(Name,Title,Text,Links,Tree)->
	window(Name,Title,Text,Links,Tree, fun()-> ok end,0).

window(Name,Title,Text,Links,Tree,Legend,LegendY)->
	window(Name,Title,Text,Links,Tree,Legend,LegendY,fun(X)-> X end).
	
%%
%% Links reference to other modules, should be two parameter tuples {text,atom}
%%
window(Name,Title,Text,Links,Tree,Legend,LegendY,Reg)->
	%%io:format("Links are ~p ~n",[Links]),
	%%io:format("Tree is ~p ~n",[Tree]),
	wings_wm:delete(Name),
	%% this does not handle links .. ignoring for now
	{Rows,Lines} = wings_text:break_lines(Text,60),
	{W,H} = wings_wm:top_size(),
	MaxH = trunc(0.75*H),
	Xs = 64 *?CHAR_WIDTH,
	Ys = MaxH,
	%% maybe use center function instead
	X  = trunc((W-Xs)/2),
	Y  = trunc((H-Ys)/2),
	[{RootName,_}] = Tree(), %% might be bad
	Data0 = #hw{lines=Lines,rows=Rows,crr=0,tw=Xs,txh=Rows*?LINE_HEIGHT,th=(1000)*?LINE_HEIGHT,wh=Ys,links=Links,tree=Tree,
			vist=[{RootName,[]}],legend=Legend,legendY=LegendY	
		},
	%%
	%%bad but nothing simple that is better
	%% 
	Data1 = window_tree_defaults(Data0),
	Data  = Reg(Data1),
	Op = {seq,push,{replace,fun(X)-> handle_window(Data,X) end}},
	Size = {Xs + ?CHAR_WIDTH,Ys + ?LINE_HEIGHT},
	wings_wm:toplevel(Name,Title,{X,Y,highest},Size,[closable,vscroller,resizable],Op),
	wings_wm:dirty().

%%
%% Set common registers
%%
window_tree_defaults(Data)->
	F = fun(X)->
		case X of
			height ->
				%%1;
				0;
			{render,D,IX,Y} ->
				%%AbsName = "Opaque",
				%%wings_io:text_at(IX,Y+?LINE_HEIGHT,AbsName),
				%%Y + ?LINE_HEIGHT;
				Y;
			{push,HW,Obj,PX,PY} ->
				HW;
			_ ->
				io:format("Default handler ignoring ~p~n",[X])
		end
	    end,
	D0 =chain_register({[],F},Data),

	F2 = fun(X)->
		case X of
			height ->
				1;
			{render,D,IX,Y,TxName,TxPos,TxText}->
				TxData = {TxName,TxPos,TxText},
				generic_bar_render(TxData,all,IX,Y,D,{D,D,D},"");
			{render,D,IX,Y}->
				%%Middle = trunc(0.5*?LINE_HEIGHT),
				%%Text = io_lib:fwrite("~7.4. f",[D]), %%float_to_list(D),
				%%DText = lists:sublist(Text,6),
				%%wings_io:sunken_rect(IX,Y+Middle,80,?LINE_HEIGHT+Middle,{1,1,1}),
				%%wings_io:text_at(IX+4,Y+Middle+?LINE_HEIGHT,Text),
				%%wings_io:sunken_rect(IX+120,Y+?LINE_HEIGHT,100,4,{0,0,0}),
				%%wings_io:raised_rect(IX+120+trunc(D*100),Y+4+Middle,8,16,{0.8,0.8,0.8}),
				%%Y + 2*?LINE_HEIGHT;
				generic_bar_render(IX,Y,D,{D,D,D},"");
			{push,HW,[Serv|Obj],PX,PY} ->
				%%case PX of
				%%	PX0 when PX0 >= 110, PX0 <240 ->
				%%		PXC = clamp((PX-120.0)/100.0,0.0,1.0),
				%%		wpc_rspt_ma:path_set(PXC,[object|Obj]);
				%%	_ ->
				%%		ok
				%%end,
				%%HW;
				generic_bar_event(HW,all,PX,PY,[Serv|Obj],fun(I)->I end);
			{keyevent,HW,[S|Obj],Name,Key} ->
				D = wpc_rspt_ma:path_get([object|Obj]),
				generic_bar_keyevent(HW,D,Key,[S|Obj],fun(I)-> I end);
			_->
				io:format("Float handler ignoring ~p~n",[X])
		end
	     end,

	D2 = chain_register({[float],F2},D0),		
	F3 = fun(X)->
		case X of
			height ->
				
				1;
			{render,D,IX,Y}->	
				Text = integer_to_list(D),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				Y + ?LINE_HEIGHT;
			{push,HW,Obj,PX,PY} ->
				HW;			
			_->
				io:format("Int handler ignoring ~p~n",[X])
		end
	     end,

	D3 = chain_register({[int],F3},D2),
	F4 = fun(X)->
		case X of
			height ->
				9;

			{render,{A1,A2,A3,A4},IX,Y,TxName,TxPos,TxText}->
				%%io:format("Rendering with sub component as text target!!~n",[]),
				Y1 = color_render({A1,A2,A3,A4},IX,Y,TxName,TxPos,TxText);

			{render,{A1,A2,A3,A4},IX,Y}->	
				{B1,B2,B3} = wings_color:rgb_to_hsv({A1,A2,A3}),
				wings_io:border(IX,Y+6,80,2*?LINE_HEIGHT-8,{A1,A2,A3,A4},{0.0,0.0,0.0,0.0}),
				Y1 = generic_bar_render(IX,Y +2*?LINE_HEIGHT,A1,{A1,0,0},"Red"),
				Y2 = generic_bar_render(IX,Y1,A2,{0,A2,0},"Green"),
				Y3 = generic_bar_render(IX,Y2,A3,{0,0,A3},"Blue"),
				Y4 = generic_bar_render(IX,Y3,A4,{A4,A4,A4},"Alpha"),
				Y5 = generic_bar_render(IX,Y4,B1/360.0,wings_color:hsv_to_rgb({B1,1,1}),"Hue"),
				Y6 = generic_bar_render(IX,Y5,B2,wings_color:hsv_to_rgb({0,B2,0}),"Saturation"),
				Y7 = generic_bar_render(IX,Y6,B3,wings_color:hsv_to_rgb({0,0,B3}),"Value"),
				Y7;

			{push,HW,[S|Obj],PX,PY} ->
				{A1,A2,A3,A4} = wpc_rspt_ma:path_get([object|Obj]),
				{B1,B2,B3}    = wings_color:rgb_to_hsv({A1,A2,A3}),
				NHW = case ((PY-2*?LINE_HEIGHT) div (?LINE_HEIGHT))+1 of
					1 ->
						generic_bar_event(HW,red,PX,PY,[S|Obj],fun(I)->{I,A2,A3,A4} end);
					2 ->
						generic_bar_event(HW,green,PX,PY,[S|Obj],fun(I)->{A1,I,A3,A4} end);
					3 ->
						generic_bar_event(HW,blue,PX,PY,[S|Obj],fun(I)->{A1,A2,I,A4} end);
					4->
						generic_bar_event(HW,alpha,PX,PY,[S|Obj],fun(I)->{A1,A2,A3,I} end);
					5->
						generic_bar_event(HW,hue,PX,PY,[S|Obj],fun(I)-> {C1,C2,C3} = wings_color:hsv_to_rgb(I*360,B2,B3),
											 {C1,C2,C3,A4} end);
					6->
						generic_bar_event(HW,saturation,PX,PY,[S|Obj],fun(I)-> {C1,C2,C3} = wings_color:hsv_to_rgb(B1,I,B3),
											 {C1,C2,C3,A4} end);
					7->
						generic_bar_event(HW,value,PX,PY,[S|Obj],fun(I)-> {C1,C2,C3} = wings_color:hsv_to_rgb(B1,B2,I),
											 {C1,C2,C3,A4} end);
					Val->
						%%io:format("Clicked on color line ~p~n",[Val]),
						tx_grab(HW,none,none)
				end,
				NHW;
			{keyevent,HW,[S|Obj],Name,Key} ->
				%%io:format("Keyevent target=~p name=~p value=~p ~n",[[S|Obj],Name,Key]),
				{A1,A2,A3,A4} = wpc_rspt_ma:path_get([object|Obj]),
				{B1,B2,B3}    = wings_color:rgb_to_hsv({A1,A2,A3}),
				NHW = case tx_getName(HW) of
					red ->
						generic_bar_keyevent(HW,A1,Key,[S|Obj],fun(I)-> {I,A2,A3,A4} end);
					green ->
						generic_bar_keyevent(HW,A2,Key,[S|Obj],fun(I)-> {A1,I,A3,A4} end);
					blue  ->
						generic_bar_keyevent(HW,A3,Key,[S|Obj],fun(I)-> {A1,A2,I,A4} end);
					alpha ->
						generic_bar_keyevent(HW,A4,Key,[S|Obj],fun(I)-> {A1,A2,A3,I} end);
					hue   ->
						generic_bar_keyevent(HW,B1/360.0,Key,[S|Obj],fun(I)->{C1,C2,C3}=wings_color:hsv_to_rgb(I*360,B2,B3),
												{C1,C2,C3,A4} end);
					saturation   ->
						generic_bar_keyevent(HW,B2,Key,[S|Obj],fun(I)->{C1,C2,C3}=wings_color:hsv_to_rgb(B1,I,B3),
												{C1,C2,C3,A4} end);
					value   ->
						generic_bar_keyevent(HW,B3,Key,[S|Obj],fun(I)->{C1,C2,C3}=wings_color:hsv_to_rgb(B1,B2,I),
												{C1,C2,C3,A4} end)		
				end;
			_->
				io:format("Color handler ignoring ~p~n",[X])
			
		end
	     end,

	D4 = chain_register({[col],F4},D3),
	F5 = fun(X)->
		case X of
			height ->
				1;
			{render,Zatom,IX,Y}->	
				Text = atom_to_list(Zatom),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				Y + ?LINE_HEIGHT;		
			{push,HW,[S|Obj],PX,PY} ->
		
				HW;	
			_->
				io:format("Atom handler ignoring ~p~n",[X])
		end
	     end,

	D5 = chain_register({[atom],F5},D4),

	%% handler of the texture function
	F6 = fun(X)->
		case X of
			height ->
				1;
			{render,Z,IX,Y}->
				[S|Path] = Z,
				io:format("Rendering RSPT WM  material ~p !! path=~p ~n",[Z,Path]),
				PathD = wpc_rspt_ma:path_get_maybe([object|Path],[]),
				io:format("After get maybe !! ~p ~n",[PathD]),
				case PathD of
					[] ->
						gl:color3f(0.0,0.0,1.0),
						Text = "Add texture",
						wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
						gl:color3f(0.0,0.0,0.0);
					Other ->	
						gl:color3f(0.0,0.0,1.0),
						wings_io:text_at(IX,Y+?LINE_HEIGHT,"Strip texture"),
						gl:color3f(0.0,0.0,0.0)
				end,
				Y + ?LINE_HEIGHT;
			%{render,Zatom,IX,Y}->
			%	io:format("Rendering texture handler data is ~p !!~n",[Zatom]),
			%	Text = "Under construction",
			%	wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				
				%%io:format("Stopped rendering texture handler~n",[]),
			%	Y + ?LINE_HEIGHT;
			{push,HW,[S|Obj],Px,Py} ->
					%%[Mat|_] = lists:reverse(Obj),
					io:format("Path for set object ~p ~n",[[S|Obj]]),
					PathD = wpc_rspt_ma:path_get_maybe([object|Obj],[]),
					case PathD of
						[] ->
							%% add image
							%% print available images
							IMG = wings_image:images(),
							IDS = [ID|| {ID,_} <- IMG],
							io:format("Images are ~p ~n",[IDS]),	
							case IDS of
								[ID|_] ->
									io:format("Setting the image as ~p to ~p ~n",[ID,Obj]),
									wpc_rspt_ma:path_set(ID,[object,diffuse |Obj]);
								_ ->
									ok
							end,
							

							%%io:format("Get returning ~p ~n",
							ok;
						Other ->
							%% strip image
							wpc_rspt_ma:path_set([],[object|Obj])
					end,
					HW;
			_ ->
				io:format("Texture handler ignoring ~n",[])
		end
	      end,

	D6 = chain_register({[maps],F6},D5),


	F7 = fun(X)->
		case X of
			height ->
				16;
			{render,Zatom,IX,Y}->
				%%io:format("Rendering texture handler data is ~p !!~n",[Zatom]),
				%%Text = "Image here",
				%%wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				%%io:format("Stopped rendering texture handler~n",[]),
				render_texture_image(Zatom,IX,Y+?LINE_HEIGHT div 2,15*?LINE_HEIGHT,15*?LINE_HEIGHT),
				Y + 16*?LINE_HEIGHT;
			{push,HW,[S|Obj],Px,Py} ->
					io:format("Clicked object ~p ID is ~p ~n",[[S|Obj],wpc_rspt_ma:path_get([object|Obj])]),
					switch_image([S|Obj],wpc_rspt_ma:path_get([object|Obj])),
					HW;
			_ ->
				io:format("Texture handler ignoring ~n",[])
		end
	      end,
	D7 = chain_register({[int,maps],F7},D6).

	%%D0.


switch_image([S|Obj],ID)->
	io:format("Setting ~p ~n",[Obj]),
	IMG = wings_image:images(),
	[First|Tail] = IDS = [XID|| {XID,_} <- IMG],
	NID = switch_image_1(ID,First,IDS),
	wpc_rspt_ma:path_set(NID,[object|Obj]),
	ok.

%% find next image
switch_image_1(ID,_,[ID,NID|_])->
	NID;
switch_image_1(ID,First,[El|Tail])->	
	switch_image_1(ID,First,Tail);
switch_image_1(ID,First,[])->
	First.

render_texture_image(Tex,TX,TY,W,H)->
		gl:pushAttrib(?GL_TEXTURE_BIT),

		
		gl:bindTexture(?GL_TEXTURE_2D,0),
		case wings_image:txid(Tex) of
				none ->
					io:format("No texture for object with texture~n",[]), 
					ok;
				ID   ->
					gl:bindTexture(?GL_TEXTURE_2D,ID),
					gl:enable(?GL_TEXTURE_2D),

					%%gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGB,32,16,0,?GL_RGB,?GL_UNSIGNED_BYTE,icon()),	
					gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
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
					gl:vertex2f(TX,TY),
					gl:texCoord2f(0,0),
					gl:vertex2f(TX,TY+H),
					gl:texCoord2f(1.0,0.0),
					gl:vertex2i(TX+W,TY+H),
					gl:texCoord2f(1.0,1.0),
					gl:vertex2i(TX+W,TY),
					gl:'end'(),
					gl:color3f(0,0,0)

		end,
		gl:bindTexture(?GL_TEXTURE_2D,0),
		gl:shadeModel(?GL_FLAT),
		gl:disable(?GL_TEXTURE_2D),
		ok.


color_render({A1,A2,A3,A4},IX,Y,TxName,TxPos,TxText)->
		TxData = {TxName,TxPos,TxText},
		%%io:format("Rendering text as ~p~n",[TxData]),
		{B1,B2,B3} = wings_color:rgb_to_hsv({A1,A2,A3}),
		wings_io:border(IX,Y+6,80,2*?LINE_HEIGHT-8,{A1,A2,A3,A4},{0.0,0.0,0.0,0.0}),
		Y1 = generic_bar_render(TxData,red,IX,Y +2*?LINE_HEIGHT,A1,{A1,0,0},"Red"),
		Y2 = generic_bar_render(TxData,green,IX,Y1,A2,{0,A2,0},"Green"),
		Y3 = generic_bar_render(TxData,blue,IX,Y2,A3,{0,0,A3},"Blue"),
		Y4 = generic_bar_render(TxData,alpha,IX,Y3,A4,{A4,A4,A4},"Alpha"),
		Y5 = generic_bar_render(TxData,hue,IX,Y4,B1/360.0,wings_color:hsv_to_rgb({B1,1,1}),"Hue"),
		Y6 = generic_bar_render(TxData,saturation,IX,Y5,B2,wings_color:hsv_to_rgb({0,B2,0}),"Saturation"),
		Y7 = generic_bar_render(TxData,value,IX,Y6,B3,wings_color:hsv_to_rgb({0,0,B3}),"Value"),
		Y7.

generic_bar_keyevent(HW,Val,Key,[S|Obj],F)->
		Text = io_lib:fwrite("~7.4. f",[Val]),
		{NHW,NUVal} = gen_text_event(HW,Text,bar_index,Key),
		%%io:format("Reading back float ~p text is ~p ~n",[NText,tx_getText(NHW)]),
		%%{ok,[NRead],_} = io_lib:fread("~f",NText),
		NVal = case NUVal of
				none ->
					Val;
				NUVal1 when is_float(NUVal1) ->
					clamp(NUVal1,0.0,1.0)
			end,
		wpc_rspt_ma:path_set(F(NVal),[object|Obj]),
		NHW.

generic_bar_render({Target,TxPos,TxText},Target,IX,Y,Val,Col,Desc)->
	%%io:format("Rendering target with text ~p ~n",[TxText]),
	Y1 = generic_bar_render(IX,Y,Val,Col,Desc),
	%% now finally render the bar itself
	Txt = case TxText of
		none ->
			lists:flatten(io_lib:fwrite("~7.4. f",[Val]));
		_ ->
			TxText
	      end,
	wings_io:sunken_rect(IX,Y+2,80,?LINE_HEIGHT-4,{1,1,1}),
	wings_io:text_at(IX+4,Y+?LINE_HEIGHT-2,Txt),
	Width = wings_text:width(lists:sublist(Txt,TxPos)),
	gl:enable(?GL_COLOR_LOGIC_OP),
	gl:logicOp(?GL_XOR),
	gl:color3f(1,1,1),
	gl:recti(IX+Width+?CHAR_WIDTH div 2,Y+4,IX+Width+?CHAR_WIDTH+?CHAR_WIDTH div 2,Y+?LINE_HEIGHT-2),
	gl:disable(?GL_COLOR_LOGIC_OP),
	gl:color3f(0,0,0),
	Y1;
generic_bar_render(_,_,IX,Y,Val,Col,Desc)->
	generic_bar_render(IX,Y,Val,Col,Desc).

generic_bar_render(IX,Y,Val,Col,Desc)->
		Text = io_lib:fwrite("~7.4. f",[Val]),
		wings_io:sunken_rect(IX,Y+2,80,?LINE_HEIGHT-4,{1,1,1}),
		wings_io:text_at(IX+4,Y+?LINE_HEIGHT-2,Text),
		wings_io:sunken_rect(IX+120,Y+?LINE_HEIGHT/2,100,4,Col),
		wings_io:raised_rect(IX+120+trunc(Val*100),Y+2,8,?LINE_HEIGHT-4,{0.8,0.8,0.8}),
		wings_io:text_at(IX+240,Y+?LINE_HEIGHT,Desc),
		Y + ?LINE_HEIGHT.

generic_bar_render(IX,Y,Val,Col)->
		generic_bar_render(IX,Y,Val,Col,"").


generic_bar_event(PX,PY,[S|Obj])->
		%% sub fun of function below
		generic_bar_event(PX,PY,[S|Obj],fun(I)->I end).

%%
%% To set more complex objects
%%
generic_bar_event(HW,Name,PX,PY,[S|Obj],F)->
	case PX of
		PX0 when PX0 =< 80 ->
			%% key event grab key focus
			tx_grab(HW,[S|Obj],Name);
		_ ->
			generic_bar_event(PX,PY,[S|Obj],F),
			HW
	end.

generic_bar_event(PXI,PY,[S|Obj],F)->
		PX = PXI -8,
		case PX of
			PX0 when PX0 >= 110, PX0 <240 ->
				PXC = clamp((PX-120.0)/100.0,0.0,1.0),
				wpc_rspt_ma:path_set(F(PXC),[object|Obj]);
			_ ->
				ok
		end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Event handler of handle_window
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%
handle_window(Data,close)->
	delete;
handle_window(#hw{lines=Lines,links=Links,crr=Crr,legend=LG,legendY=OffsetY}=HW,redraw)->
	%%io:format("Lines ++ Links is ~p ~n",[Lines ++ Links]),	
	%% do something to the scroller
	update_scroller(HW),
	wings_io:ortho_setup(),
	{W,H} = wings_wm:win_size(),
	RGB = wings_pref:get_value(menu_color),
	wings_io:border(0,0,W-1,H-1,RGB),


	gl:color3f(0,0,0),
	%% was 4 y translated hmm
	gl:pushMatrix(),
	gl:translated(4,0+?LINE_HEIGHT,0),

	%% render legend here


	%% render text
	LY = lists:foldl(fun(L,Y) ->
			case L of
				%% link
				{LT,_} ->
					gl:color3f(0,0,1),
					wings_io:text_at(5,Y,LT),
					gl:color3f(0,0,0);
				_ ->
					wings_io:text_at(5,Y,L)
			end,
			Y + ?LINE_HEIGHT
		end, -Crr+OffsetY,Lines++Links),
	Res = render_tree(HW,LY), %% render the tree

	gl:popMatrix(),
	wings_io:border(0,0,W-1,128,RGB),
	%% run whatever renderer we are using
	(catch LG()),	


	Res;
handle_window(#hw{th=Th,wh=Wh}=HW,{set_knob_pos,Pos})->
	%%io:format("Got set_knob_pos ~p ~n",[Pos]),
	Lines = Th div ?LINE_HEIGHT,
	VisLines = Wh div ?LINE_HEIGHT,
	HWN = HW#hw{crr=trunc(Lines*Pos)},
	update_scroller(HWN),
	{replace, fun(X)-> handle_window(HWN,X) end};
handle_window(#hw{wh=Wh,crr=Crr,legendY=Ly}=HW,scroll_page_up)->
	scroll_step_win(-Wh+Ly,HW);
handle_window(#hw{wh=Wh,crr=Crr,legendY=Ly}=HW,scroll_page_down)->
	scroll_step_win(Wh-Ly,HW);
handle_window(#hw{}=HW,#mousebutton{button=4,state=?SDL_RELEASED})->
	scroll_step_win(-4*?LINE_HEIGHT,HW);
handle_window(#hw{}=HW,#mousebutton{button=5,state=?SDL_RELEASED})->
	scroll_step_win(4*?LINE_HEIGHT,HW);
handle_window(#hw{crr=Crr,lines=Lines,links=Links}=HW,#mousebutton{button=1,state=?SDL_RELEASED,y=Y,x=X})->

	NHW = case grab_line_by_y(HW,X,Y) of
		{_,A} ->
					Res = (catch A()),
					%%io:format("~p~n----- ~n",[Res]),
					HW;
		tree ->
					%%io:format("Tree click handle in tree~n",[]),
					HW0 = tree_push_event(HW,X,Y),
					wings_wm:dirty(),
					HW0;
		_ ->
					HW
	      end,
	{replace,fun(X)-> handle_window(NHW,X) end};
handle_window(#hw{}=HW,#mousemotion{x=X,y=Y})->
	case grab_line_by_y(HW,X,Y) of
		{_,_} ->
			%% set mouse to hand
			wings_io:set_cursor(pointing_hand);
		_ ->
			%% plain mouse
			wings_io:set_cursor(arrow)
	end,
	keep;
handle_window(Data,#keyboard{}=Key)->
	NHW = handle_keyboard(Data,Key),
	wings_wm:dirty(), %% is this realy needed, dubious ..
	%%io:format("Text is now after key event =~p ~n",[tx_getText(NHW)]),
	{replace,fun(X)-> handle_window(NHW,X) end};
handle_window(Data,X)->
	%%io:format("Got ~p ~n",[X]),
	keep.


render_tree(#hw{tree=Tree,vist=Vis}=HW,Y)->
	%%
	%% empty vis if structures do not match .. 
	%%
	%%case (catch(render_tree(Tree(),Vis,Y,0))) of
	%%	Ans when is_integer(Ans) ->
	%%		keep; %% do nothing
	%%	_ -> %% error kill window (material has changed but not from this window ...)
	%%		delete
	%%end.
	render_tree(Tree(),Vis,Y,0,HW,[]),
	keep.
		

%%
%%Render tree
%%
render_tree([{Name,More}|Tail],[{Name,Open}|TailV],Y,Indent,HW,Path)->
	AbsName = " " ++ atom_to_list(Name),
	wings_io:text_at(?INDENT_OFF+?INDENT_FAC*Indent,Y,AbsName),
	dir_open(?INDENT_FAC*Indent,Y,?INDENT_OFF,?LINE_HEIGHT),

	%% render the open directory
	NY = render_tree(More,Open,Y + ?LINE_HEIGHT,Indent+1,HW,[Name|Path]),
	render_tree(Tail,TailV,NY,Indent,HW,Path);
render_tree([{Name,More}|Tail],Vis,Y,Indent,HW,Path)->
	AbsName = " " ++ atom_to_list(Name),
	wings_io:text_at(?INDENT_OFF+?INDENT_FAC*Indent,Y,AbsName),
	dir_closed(?INDENT_FAC*Indent,Y,?INDENT_OFF,?LINE_HEIGHT),

	render_tree(Tail,Vis,Y + ?LINE_HEIGHT,Indent,HW,Path);

%%
%% This part should be modified to overide with chain_call methods
%%
%%
%%render_tree([Any|Tail],Vis,Y,Indent)->
%%	AbsName = "Opaque",
%%	wings_io:text_at(5+10*Indent,Y,AbsName),
%%	render_tree(Tail,Vis,Y + ?LINE_HEIGHT,Indent);

render_tree([],_,Y,_,HW,[])->
	Y;

%% do [] as any other ..
render_tree([],_,Y,Indent,#hw{tobj=TOBJ,tx_focus=Focus,tx_name=TxName,tx_pos=TxPos,tx_text=TxText}=HW,Path)->
	%%AbsName = "Opaque",
	%%wings_io:text_at(?INDENT_OFF+?INDENT_FAC*Indent,Y,AbsName),
	%%Y + ?LINE_HEIGHT.
	
	%%io:format("Render tree empty node Path is ~p !!!~n",[Path]),

	%% fetch the service type
	Serv = [empty|Path], %%get_tree_service([empty|Path]),
	F = chain_get(TOBJ,Serv),

	%%io:format("Got match ~p ~n",[F]),

	%%io:format("Calling ~p as fun~n",[F]),
	case Serv of
	
		%% component with text focus
		Focus ->
			?LINE_HEIGHT +  F({render,[empty|Path],?INDENT_OFF + ?INDENT_FAC*Indent,Y-?LINE_HEIGHT,TxName,TxPos,TxText});
		_ ->
			?LINE_HEIGHT + F({render,[empty|Path],?INDENT_OFF + ?INDENT_FAC*Indent,Y-?LINE_HEIGHT})
	end;

render_tree(Any,_,Y,Indent,#hw{tobj=TOBJ,tx_focus=Focus,tx_name=TxName,tx_pos=TxPos,tx_text=TxText}=HW,Path)->
	%%AbsName = "Opaque",
	%%wings_io:text_at(?INDENT_OFF+?INDENT_FAC*Indent,Y,AbsName),
	%%Y + ?LINE_HEIGHT.
	
	%% fetch the service type
	Serv = get_tree_service([Any|Path]),
	F = chain_get(TOBJ,Serv),
	%%io:format("Calling ~p as fun~n",[F]),
	case Serv of
	
		%% component with text focus
		Focus ->
			?LINE_HEIGHT +  F({render,Any,?INDENT_OFF + ?INDENT_FAC*Indent,Y-?LINE_HEIGHT,TxName,TxPos,TxText});
		_ ->
			?LINE_HEIGHT + F({render,Any,?INDENT_OFF + ?INDENT_FAC*Indent,Y-?LINE_HEIGHT})
	end.
	


scroll_step_win(Step,#hw{th=Th,wh=Wh,crr=Crr}=HW)->
	Lines = Th div ?LINE_HEIGHT,
	VisLines = Wh div ?LINE_HEIGHT,

	NCrr = case Crr+Step of
		Neg when Neg < 0 -> 
				0;
		Crr1 when Crr1 >= (Lines-VisLines) -> 
				Lines-VisLines;
		X ->
			X
	       end,
	NHW = HW#hw{crr=NCrr},
	update_scroller(NHW),
	{replace, fun(X)-> handle_window(NHW,X) end}.


tree_offset(#hw{lines=Lines,links=Links,legendY=Ly}=HW)->
	trunc(Ly div ?LINE_HEIGHT) + length(Lines) + length(Links).


tree_push_event(#hw{vist=Vis,tree=Tree,crr=Crr,tobj=TOBJ}=HW,X,Y)->
	%% find relative tree position and find tree line
	Yline = (trunc((Y+Crr) div ?LINE_HEIGHT) - tree_offset(HW)),
	%%io:format("Tree line is ~p ~n",[Yline]),
	%%io:format("Tree and Vis {~p,~p}~n",[Tree(),Vis]),
	Ret = tree_handle(Tree(),Vis,0,X,Yline,[],0,HW),
	%%io:format("Pushed for event ~p ~n",[Ret]),
	%% open or close only
	case Ret of
		R0 when is_integer(R0) ->
			HW;
		{_,_,[dir|TPath]} ->
		    %% open or close directory 
		    NVis = tree_chain(Tree(),Vis,lists:reverse(TPath)),
 		    %%io:format("NHW Vis ~p ~n",[NVis]),
		    NHW = HW#hw{vist=NVis},
		    NHW;
		%% use call chain to find event handler
		{Xcind,Yclick,Obj} ->
			%%io:format("Should answer to request ~p from tree ~n",[Obj]),
			%%io:format("Coordinate (~p, ~p) in local element space Y=~p~n",[X-Xcind*?INDENT_FAC-?INDENT_OFF,
			%%							       Y+Crr-?LINE_HEIGHT*(Yclick+tree_offset(HW)),Y]),
			%%
			%%Call translated ...
			%%
			(chain_get(TOBJ,Obj))({push,HW,Obj,X-Xcind*?INDENT_FAC-?INDENT_OFF,Y +Crr - ?LINE_HEIGHT*(Yclick+tree_offset(HW))})
			%%HW

	end.

tree_handle([{Name,_}|_],[{Name,_}|_],TLine,X,TLine,Path,Indent,HW)->
	{Indent,TLine,tree_service([Name|Path],Indent,X,HW)};
tree_handle([{Name,Add}|TTree],[{Name,Open}|TailV],TLine,X,Y,Path,Indent,HW)->
	%%io:format("Tree==Vis descend TLine=~p Y=~p ~n",[TLine,Y]),
	%% check if this node has been clicked
	NY = tree_handle(Add,Open,TLine+1,X,Y,[Name|Path],Indent+1,HW),
	case NY of
		NY0 when is_integer(NY0) ->
			%% continue
			tree_handle(TTree,TailV,NY,X,Y,Path,Indent,HW);
		_ ->
			%% list, stop we found item
			NY
	end;
tree_handle([{Name,_}|TTree],Vis,Tline,X,Tline,Path,Indent,HW)->
	{Indent,Tline,tree_service([Name|Path],Indent,X,HW)};
tree_handle([{Name,_}|TTree],Vis,Tline,X,Y,Path,Indent,HW)->
	tree_handle(TTree,Vis,Tline+1,X,Y,Path,Indent,HW);
tree_handle([Any|TTree],Vis,Tline,X,Tline,Path,Indent,HW)->
	%% check what service this is
	{Indent,Tline,tree_service([Any|Path],Indent,X,HW)};
tree_handle([Any|TTree],Vis,Tline,X,Y,Path,Indent,HW)->
	tree_handle(TTree,Vis,Tline+1,X,Y,Path,Indent,HW);

%% corrected version
tree_handle([],_,Tline,X,Y,Path,Indent,HW)->
	Val = tree_service_opaque([empty|Path],Tline,Indent,X,Y,HW),
	%%io:format("Empty tree handle height ~p other ~p ~n",[Val,Tline]),
	Val;
	%%Tline;

% correct line, but opaque or otherwise
tree_handle(Any,_,Tline,X,Y,Path,Indent,HW)->
	tree_service_opaque([Any|Path],Tline,Indent,X,Y,HW).


	
%%
%% Adds service name to list for easy recognition
%% i.e. dir if directory state, object,float,integer,text, or other
%%
tree_service(Path,Indent,X,HW)->
	%% assume dir if X is low
	case X of
		X0 when X0 =< (4+Indent*?INDENT_FAC+?INDENT_OFF),X0 >= (4+Indent*?INDENT_FAC) ->
			[dir|Path];
		_ ->
		 	[dirname|Path]
	end.

%%
%% Same as above but for object types instead
%%
%%
tree_service_opaque(Path,Tline,Indent,X,Y,#hw{tobj=TOBJ}=HW)->
	Serv = get_tree_service(Path),
	case (chain_get(TOBJ,Serv))(height) of
		0 ->
			%%io:format("Empty node to click~n!!",[]),
			Tline;

		Y0 when (Tline) =< Y, Y < (Y0 +Tline) ->
			%% we clicked this serv
			{Indent,Tline,Serv};
		Y1 ->
			Y1 + Tline
	end.
			
		

%%
%% get the type of service from compounded Path name
%% used in rendering and event handling
%%
get_tree_service(Path)->

	case Path of
		[X1|T1] when is_integer(X1) ->
			[int|T1];
		[X2|T2] when is_float(X2)   ->
			[float|T2];
		%% Warning this test may be incorrect at times
		[X3|T3] when is_list(X3)    ->
			[text|T3];
		%% quaternion, probably color
		[{A1,A2,A3,A4}|T4] when is_float(A1),is_float(A2),is_float(A3),is_float(A4) ->
			[col|T4];

		%% empty
		[empty|P] ->
			[empty|P];
	
		%% atom 
		[X4|T4] when is_atom(X4) ->
			[atom|T4];

		_ ->
			[object|Path]
	end.

	
%%
%%
%% Dir chain handlers
%%
%%


%%
%% select handlers
%%
tree_chain(_,[],[Name])->
	[{Name,[]}];
%% close object
tree_chain(_,[{Name,_}|Tail],[Name])->
	Tail;
%% entry in both vis and tree but not selected
tree_chain([{Name,_}|TTail],[{Name,D}|Tail],[Other])->
	[{Name,D} | tree_chain(TTail,Tail,[Other])];
tree_chain([{Name,_}|TTail],Tail,[Name])->
	[{Name,[]}|Tail];
%% tree entry not present in vis but selected
tree_chain([{Name,_}|TTail],VisT,[Name])->
	[{Name,[]}|VisT];
tree_chain([_|TTail],Tail,[Name])->
	tree_chain(TTail,Tail,Name);
tree_chain([{Name,_}|TTail],[{Name,_}|Tail],[Other])->
	tree_chain(TTail,Tail,[Other]);


%%%%
%%%% path handlers (handle directories)
%%%%
%% sub entry
tree_chain([{Name,More}|_],[{Name,D}|Tail],[Name|Path])->
	[{Name,tree_chain(More,D,Path)}|Tail];
tree_chain([{Name,More}|TTail],[{Name,D}|Tail],[Other|Path])->
	[{Name,D}|tree_chain(TTail,Tail,[Other|Path])];
%% tree entry not present in vis
tree_chain([X|TTail],[{Name,D}|Tail],[Name|Path])->
	tree_chain(TTail,[{Name,D}|Tail],[Name|Path]);
%% no likeness
tree_chain([X|TTail],[{AName,AD}|Tail],[Name|Path])->
	tree_chain(TTail,[{AName,AD}|Tail],[Name|Path]);




%%%
%%% Patchers and debug opts
%%%
tree_chain(X,Y,Z) when is_atom(Z) ->
	%% nescesary because of some error somewhere, not fixing
	tree_chain(X,Y,[Z]);
tree_chain(X,Y,Z)->
	io:format("Tree ~p ~n Vis ~p ~n Path ~p ~n",[X,Y,Z]),
	throw("invalid descent").




%% go somewhere else ??
-define(PREVIEW_SIZE,128).

legend_material(Name)->
	(catch(material_preview(0,0,?PREVIEW_SIZE,?PREVIEW_SIZE,Name))).

material_preview(X,Y,W,H,Name)->
	%%io:format("Material preview!!~n",[]),

	wings_io:border(X,Y,?PREVIEW_SIZE,?PREVIEW_SIZE,{0.6,0.6,0.6}),
	%%io:format("Border setup cleared!~n",[]),
	MM = gl:getDoublev(?GL_MODELVIEW_MATRIX),
	PM = gl:getDoublev(?GL_PROJECTION_MATRIX),
	ViewPort = wings_wm:viewport(),
	{true,Ox,Oy,_} = glu:project(X,Y + ?PREVIEW_SIZE,0,MM,PM,ViewPort),
	gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	gl:viewport(trunc(Ox),trunc(Oy),?PREVIEW_SIZE,?PREVIEW_SIZE),
	gl:matrixMode(?GL_PROJECTION),
	gl:pushMatrix(),
	gl:loadIdentity(),
	glu:perspective(60.0,1,0.01,256.0),
	gl:matrixMode(?GL_MODELVIEW),
	gl:pushMatrix(),
	gl:loadIdentity(),
	gl:translatef(0,0,-2.0),
	wings_light:modeling_lights(camera,mat_preview),
	gl:shadeModel(?GL_SMOOTH),
	%% go for the material defs here
	%% grab current state st
	St = wings_wm:get_current_state(),
	ProgId = diffFragProg0,

	%%io:format("Sending ~p with ProgID ~p ",[Name,ProgId]),

	%% set attribs
	wpc_rspt_ma:attrib(list_to_atom(Name),St,ProgId),

	gl:enable(?GL_LIGHTING),
	gl:enable(?GL_BLEND),
	gl:enable(?GL_CULL_FACE),
	
	gl:color3f(1,1,1),

	%% end of mat defs
	Obj = glu:newQuadric(),
	glu:quadricDrawStyle(Obj,?GLU_FILL),
	glu:quadricNormals(Obj,?GLU_SMOOTH),
	glu:sphere(Obj,0.9,50,50),
	glu:deleteQuadric(Obj),
	gl:disable(?GL_LIGHTING),
	gl:disable(?GL_BLEND),
	gl:disable(?GL_VERTEX_PROGRAM_ARB),
	gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
	gl:shadeModel(?GL_FLAT),
	gl:matrixMode(?GL_PROJECTION),
	gl:popMatrix(),
	gl:matrixMode(?GL_MODELVIEW),
	gl:popMatrix(),
	gl:popAttrib(),
	%%io:format("Material preview end!!~n"),
	ok.


%%
%% Function to stop direct inclusion of wm module
%%
get_current_state()->
	wings_wm:get_current_state().

set_current_state(#st{}=St)->
	wings_wm:current_state(St).


%%%
%%% render dir, X,Y are start points
%%% Y is upper left
%%%
dir_closed(X,Y,W,H)->
	X0 = X+0.5,
	Y0 = Y+0.5,
	gl:color3f(0,0,0),
	gl:'begin'(?GL_TRIANGLES),
	gl:vertex2f(X0,Y0),
	gl:vertex2f(X0,Y0-H*0.5),
	gl:vertex2f(X0+W,Y0 - H/4.0),
	gl:color3f(1,1,1),
	gl:vertex2f(X0,Y0-2),
	gl:vertex2f(X0,Y0-H*0.5-2),
	gl:vertex2f(X0+W,Y0 - H/4.0-2),
	gl:color3f(0,0,0),
	gl:'end'().

%%%
%%% render dir, X,Y are start points
%%%
dir_open(X,Y,W,H)->
	X0 = X+0.5,
	Y0 = Y+0.5,
	gl:color3f(0,0,0),
	gl:'begin'(?GL_TRIANGLES),
	gl:vertex2f(X0,Y0-H*0.5),
	gl:vertex2f(X0+W,Y0-H*0.5),
	gl:vertex2f(X0+W/2.0,Y0),
	gl:color3f(1,1,1),
	gl:vertex2f(X0,Y0-H*0.5-2),
	gl:vertex2f(X0+W,Y0-H*0.5-2),
	gl:vertex2f(X0+W/2.0,Y0-2),
	gl:color3f(0,0,0),
	gl:'end'().




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Chain handling part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Chain register
%%
%%
chain_register({ChainC,Obj},#hw{tobj=TOBJ}=HW)->
	HW#hw{tobj=chain_set(TOBJ,ChainC,Obj)}.



%%
%% Chain data set
%%
chain_set(Data,Chain,FV)->
	gb_trees:enter(Chain,FV,Data).

%%
%% Chain data get (no set)
%%
chain_get(Data,Chain)->
	FX = fun(Trg)->
			%%io:format("Chain call with target ~p ~n",[Trg]),
			case gb_trees:lookup(Trg,Data) of
				{value,V} ->
						V;
				none ->
					next
			end
	    end,
	%% we now have FX to search the tree with
	%% run call_chain on above data
	call_chain(FX,Chain).

%%
%% Implementation of a call chain call
%% Input: Fun(X)-> next | Other  *   Chain as described
%% 
call_chain(F,[Service|Chain])->
	%% last name is material name, do not use in call_chain
	[Name|RC] = lists:reverse(Chain),
	%%io:format("Reversed ~p ~n",[RC]),
	%% somewhat silly to use reverse twice to skip an entry
	call_chain_1(F,Service,lists:reverse(RC)).

call_chain_1(F,S,[El|RC])->
	case F([S,El|RC]) of
		next ->
			case F([El|RC]) of
				next ->
					call_chain_1(F,S,RC);
				O0 ->
					O0
			end;
		 O1 ->
			O1
	end;
call_chain_1(F,S,[])->
	case F([S]) of
		next ->
			case F([]) of
				next ->
					fail;
				O0 ->
					O0
			end;
		 O1 ->
			O1
	end.



undo_save(OldState,St0)->
	wings_undo:save(OldState,St0).


%%
%% Clamp value to proper values
%%
clamp(Val,F,T)->
	case Val of
		Val1 when Val1 < F ->
			F;
		Val2 when Val2 > T ->
			T;
		_ ->
			Val
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% handle_keyboard(Data,Key),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%tx_grab(HW,Path,Name) grab all key events and send to path 'path' with name 'name'
tx_grab(#hw{}=HW,Path,Name)->
	HW#hw{tx_focus=Path,tx_name=Name,tx_pos=0,tx_text=none}.
	
%%tx_getPos(HW)        get postion
tx_getPos(#hw{tx_pos=Pos})->
	Pos.
%%tx_setPos(HW,X)       set position
tx_setPos(#hw{}=HW,X) when is_integer(X)->
	HW#hw{tx_pos=X}.

tx_getName(#hw{tx_name=N}=HW)->
	N.

tx_getText(#hw{tx_text=Text}=HW)->
	Text.

tx_setText(HW,Text)->
	HW#hw{tx_text=Text}.

%% do nothing
handle_keyboard(#hw{tobj=TOBJ,tx_focus=none}=HW,Key)->
	HW;
handle_keyboard(#hw{tobj=TOBJ,tx_focus=Path,tx_name=Name}=HW,Key)->
	(chain_get(TOBJ,Path))({keyevent,HW,Path,Name,Key}).


%%% generic handler for text managment 

gen_text_event(#hw{tx_text=none}=HW,Text,bar_index,Key)->
	F = fun({NHW,NText})->
		io:format("New text string is ~p ~n",[NText]),
		case io_lib:fread("~f",lists:flatten(NText)) of
			{ok,[Val],_} ->
				{NHW#hw{tx_text=lists:sublist(NText,7)},Val};
			_ ->
				%%io:format("Keeping old text ~p ~n",[Text]),
				{NHW#hw{tx_text=lists:sublist(NText,7)},none}

		end
	    end,
	gen_text_event(HW,lists:flatten(Text),F,Key);
gen_text_event(#hw{tx_text=AText}=HW,Text,bar_index,Key)->
	F = fun({NHW,NText})->
		io:format("New text string is ~p ~n",[NText]),
		case io_lib:fread("~f",NText) of
			{ok,[Val],_} ->
				{NHW#hw{tx_text=lists:sublist(NText,7)},Val};
			_ ->
				%%io:format("Keeping old text ~p ~n",[Text]),
				{NHW#hw{tx_text=lists:sublist(NText,7)},none}

		end
	    end,
	gen_text_event(HW,lists:flatten(AText),F,Key);

gen_text_event(#hw{tx_pos=P}=HW,Text,F,	#keyboard{state=?SDL_PRESSED,keysym=#keysym{scancode=Scan,sym=Sym,mod=Mod,unicode=Unicode}})->
	case Sym of
		?SDLK_LEFT ->
			PC = clamp(P-1,0,length(Text)+1),
			{HW#hw{tx_pos=PC}, none};
		?SDLK_RIGHT ->
			PC = clamp(P+1,0,length(Text)+1),
			{HW#hw{tx_pos=PC}, none};
		_ ->
			io:format("Pressed ordinary key ~p ~n",[Unicode]),
			PC = clamp(P+1,0,length(Text)+2),
			F({HW#hw{tx_pos=PC},gen_text_event_1(P,Unicode,Text)})
	end;
gen_text_event(HW,Text,_,_)->
	{HW,Text}.

gen_text_event_1(0,Uni,Text)->
	[Uni|Text];
gen_text_event_1(N,Uni,[C|T])->
	[C|gen_text_event_1(N-1,Uni,T)];
gen_text_event_1(_,Uni,[])->
	Uni.
