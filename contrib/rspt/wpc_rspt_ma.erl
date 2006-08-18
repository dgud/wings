%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_ma.erl,v 1.1 2004/08/25 05:33:05 bjorng Exp $
%%

%
%Material module
%
%
%
%
-module(wpc_rspt_ma).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").


-export([init/0,menu/2,command/2]).
-export([dialog/2]).
-export([texture_compile/1,face/4,uvc/1,quantity/1,compile/3,resolve/3,gval/3,attrib/3]).
%% the get and set material value funs (should skip bad values ??, no since these need policies )
-export([path_get/2,path_get/1,path_set/3,path_set/2,path_get_maybe/3,path_get_maybe/2,texture/1]).


%% link into app
init()->
	%%io:format("Loading wpc_rspt_ma ... ~n",[]),
	true.


menu(X, Menu)->
	%%io:format("Material menu ~p ~n",[X]),
	Menu.

command({material,{new,Name}},St)->

	case wpc_rspt_ps:is_enabled() of
		true ->
			%% bypass regular new
			io:format("RSPT creating new material~n",[]),
			%% do more here
			{Name1,St1} = new_material(Name,false,St),
			wpc_rspt_wm:set_current_state(St1),
			St2 = material_window(Name1,St1),

			%% correct ?? no
			St2;
		false ->
			next
	end;
command({material,{edit,Name}},St)->

	%% check if enabled
	case wpc_rspt_ps:is_enabled() of
		true ->
			%% send what here ??
			io:format("RSPT enabled bypassing!~n",[]),
			St1 = material_window(Name,St),
			St1;
		false ->
			io:format("RSPT disabled!~n",[]),
			next
	end;

command(X,_)-> 
	io:format("Mat Command ~p ~n",[X]),
	next.

%% material dialog for wings
%% should not show up if disabled
dialog({material_editor_setup,Name,Mat},Dialog)->
	Dialog; %% ++ build_dialog(Mat);

%%
%% as formatted below
%%
dialog({material_editor_result,Name,Mat},Dialog)->
	%%io:format("Dialog {material_editor_result,~p,~p} dialog data ~p ~n",[Name,Mat,Dialog]),
	io:format("Dialog {material_editor_result,~p,~p},~p ~n",[Name,Mat,Dialog]),
	%%invalid;
	%% the dialog data is the a proplist of features
	%%{[{wpc_rspt,Dialog}]++Mat,[]};
	{insert(Dialog,Mat),[]};

%%
%% another dialog instruction
dialog(X,Dialog)->
	io:format("Dialog entry ~p  ~n~n",[X]),
	Dialog.


%%%%%%%%%
%%%%%%%%% dialog construction of plugin modules
%%%%%%%%%

build_dialog(Mat)->
	[{vframe,[
		{label,"Ambient material pipeline"},
		build_pipe("ambPipe",Mat),
		{label,"Ambient material stages"},
		  {hframe,build_dia("amb",0,5,Mat)},
		{label,"Diffuse & Specular material pipeline"},
		build_pipe("diffPipe",Mat),
		  {label,"Diffuse & Specular material stages"},
	          {hframe,build_dia("diff",0,5,Mat)}]

	,[{title,"RSPT material options"}]}].

build_pipe(Arg,Mat)->
	{menu,[{"Default",standard},
		{"Foobar",foobar}]
		++ wpc_rspt_ps:get_mat(Arg),
		gval(list_to_atom(Arg),Mat,standard),
		[{key,list_to_atom(Arg)}]}.
		

build_dia(L,X,X,Mat)->
	[];
build_dia(L,X,Y,Mat)->

	[{vframe,[{menu,[{"None",standard},
			{"Pure red",red_only},
			%% fetch all material plugins ..
			{"Blend",blend}] ++ wpc_rspt_ps:get_mat(L,X), 
			gval(list_to_atom(L ++ "FragProg" ++ [$0+X]),Mat,standard),
			[{key,list_to_atom(L ++ "FragProg" ++ [$0+X])}]
		  }
		]}| build_dia(L,X+1,Y,Mat)].


 

%%%%%%%%%%%
%%%%%%%%%%% Create the material by default opengl ...
%%%%%%%%%%% 

%%%
%%%Should not directly be compiled into the scene data, additional call list should be used to 
%%%be able to adapt scene behaviour ...

%% likely to be obsolete, but keep in case this becomes useful
% should return gb_trees with textures, may need further adjustments
texture_compile(#st{mat=Mat}=St)->
	%% could use this to build mipmaps of every texture and pass on those values
	%% (saved for later)
	% write out all material data
	%%io:format("=======================~n~p~n=======================~n ~n",[Mat]),
	ok.
%
%color or uv
%
uvc({X,Y})->
	%%io:format("Coord ~p ~p ~n",[X,Y]),
	%%gl:texCoord2f(X,Y),
	%% unit 0 
	gl:multiTexCoord2f(?GL_TEXTURE0,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE1,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE2,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE3,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE4,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE5,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE6,X,Y),
	gl:multiTexCoord2f(?GL_TEXTURE7,X,Y),
	ok;
uvc({X,Y,Z})->
	%% ignore the color data
	%%gl:color3f(X,Y,Z),
	ok;
uvc(X)->
	% never happens
	%%io:format("=====================~n Other material ~p~n===================~n",[X]),
	ok.

%
%Find the material 
%
face(Face,#we{mat=Mat}=WE,St,Scene) when is_atom(Mat) ->
	%attrib(Mat,St);
	resolve(Mat,St,Scene);
face(Face,#we{mat=Mat}=WE,St,Scene) ->
	Name = proplists:get_value(Face,Mat),
	%attrib(Name,St).
	resolve(Name,St,Scene).



%
%Setup attributes for material (ignore vertex light)
%
attrib(Name,#st{mat=Mat}=St,ProgID)->
	case gb_trees:lookup(Name,Mat) of
		{value,V} ->
			% proplist of stuff (safe mode???)
			OpenGL = proplists:get_value(opengl,V),
			Diffuse  = proplists:get_value(diffuse,OpenGL),
			Specular = proplists:get_value(specular,OpenGL),
			Ambient  = proplists:get_value(ambient,OpenGL),
			Emission = proplists:get_value(emission,OpenGL),
			Shininess = proplists:get_value(shininess,OpenGL),
			% wings seems to multiply this with 128 why ? check later ...
			% appears to be to cap values to 0-1
			gl:materialf(?GL_FRONT_AND_BACK,?GL_SHININESS,Shininess*128),
			gl:materialfv(?GL_FRONT_AND_BACK,?GL_AMBIENT,Ambient),
			gl:materialfv(?GL_FRONT_AND_BACK,?GL_DIFFUSE,Diffuse),
			gl:materialfv(?GL_FRONT_AND_BACK,?GL_SPECULAR,Specular),
			gl:materialfv(?GL_FRONT_AND_BACK,?GL_EMISSION,Emission),
			gl:disable(?GL_TEXTURE_2D),
			gl:disable(?GL_ALPHA_TEST),
			gl:enable(?GL_LIGHTING),
			gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
			gl:disable(?GL_VERTEX_PROGRAM_ARB),
			gl:disable(?GL_COLOR_LOGIC_OP),
			%%
			%%Handle maps part and potential texture id
			%%
			TexMat = proplists:get_value(maps,V),
			texture(TexMat),
			maProg(V,ProgID),
			ok;
		none ->
			io:format("Warning material ~p is ignored ~n",[Name]),
			%% do what ?? (ignore for the moment)
			ok
	end,
	ok.

%
%Use textures as appropriate if we find some
% does it crash some later needed use of stencil ?? hmm 
%
texture(Mat)->
	case proplists:get_value(diffuse,Mat,none) of
		none ->
			gl:bindTexture(?GL_TEXTURE_2D,0),
			% no texture
			ok;
		Tex  ->
			case wings_image:txid(Tex) of
				none ->
					io:format("No texture for object with texture~n",[]), 
					ok;
				ID   ->
					gl:bindTexture(?GL_TEXTURE_2D,ID),
					%%io:format("Using texture ~p ~n",[ID]),
					% render texture
					gl:enable(?GL_TEXTURE_2D),
					%% was GL_MODULATE only for debug ?? not true
					gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_MODULATE),
					%%
					%% Wings does not have mipmaps this is bad ... or maybe unimportant in close ups ??
					%% rebuild textures in wings ??? or copy them ??
					%% mipmap by hand ??
					%%
					gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_LINEAR),
					gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),	
					gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_REPEAT),	
					gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_REPEAT),
					case wings_image:info(Tex) of
						#e3d_image{bytes_pp=4}->
							io:format("pp4 image!! ~n",[]),
							gl:enable(?GL_ALPHA_TEST),
							gl:alphaFunc(?GL_GREATER,0.5);
						#e3d_image{type=a8} ->
							io:format("a8 image!! ~n",[]),
							gl:enable(?GL_ALPHA_TEST),
							gl:alphaFunc(?GL_GREATER,0.5);
						_ ->
							gl:disable(?GL_ALPHA_TEST)
					end,
					ok
			end
	end.	




%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% Compile all materials as requested
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%

%%
%%Will require more material defs
%%likely to change to different material requests for ambiant & specular pass versus ambient
%%
%% Numbering of material to scene data
%%
%%  Scene | Scene +1 | Scene +2 | ... | Scene + N
%%  the scene                          
%%          mat 1                      
%%                     mat 2            
%%                                ...
%%                                      mat N (last material)
%%
compile(Scene,#st{mat=Mat}=St,Request)->
	case Request of

		%% bind material to scene objects
		full ->
			%%io:format("<<<<< material on >>>>>>>~n",[]),
			lists:foldl(fun({Cmat,_},Acc) ->
					gl:getError(),
					gl:newList(Acc,?GL_COMPILE),
					attrib(Cmat,St,0),
					gl:endList(),
					%%io:format("material GL state ~p compile number ~p ~n",[glu:errorString(gl:getError()),Acc]),
					Acc+1
				    end,Scene+1,gb_trees:to_list(Mat)),
			%% compiled on the gpu
			ok;
		%% run program with ID either ambient or 
		{full,ProgID} ->
			%%io:format("<<<<< material on >>>>>>>~n",[]),
			lists:foldl(fun({Cmat,_},Acc) ->
					gl:getError(),
					gl:newList(Acc,?GL_COMPILE),
					attrib(Cmat,St,ProgID),
					gl:endList(),
					%%io:format("material GL state ~p compile number ~p ~n",[glu:errorString(gl:getError()),Acc]),
					Acc+1
				    end,Scene+1,gb_trees:to_list(Mat)),
			%% compiled on the gpu
			ok;
		%% bind empty material definitons to scene
		none ->
			%% zero all material settings
			lists:foldl(fun({Cmat,_},Acc) ->
					gl:getError(),
					gl:newList(Acc,?GL_COMPILE),
					gl:endList(),
					%%io:format("material zero mat GL state ~p compile number ~p~n",[glu:errorString(gl:getError()),Acc]),
					Acc+1
				    end,Scene+1,gb_trees:to_list(Mat)),
			%% compiled on the gpu
			ok
	end.


%%
%%Number of material compile targets needed
%%tightly connected to re modules scene entity 
%%
quantity(#st{mat=Mat}=St)->
		%%io:format("quantity of material is ~p ~n",[gb_trees:size(Mat)]),
		gb_trees:size(Mat).


%%
%%get the material number for the current face and call it
%%
resolve(Matv,#st{mat=Mat}=St,Scene)->
	%% resolve material number
	Keys = gb_trees:keys(Mat),
	%%io:format("Resolved number are ~p  scene is ~p ~n",[resolve_number(Keys,Matv,Scene+1),Scene]),
	gl:callList(resolve_number(Keys,Matv,Scene+1)),
	ok.


%%
%% get number
%%
resolve_number([Matv|_],Matv,Num)->
	Num;
resolve_number([_|T],Matv,Num)->
	resolve_number(T,Matv,Num+1).



%%%%
%%%%should fetch data values ...
%%%%

gval(Key,Mat,Def)->
	case proplists:lookup(wpc_rspt,Mat) of

		none ->
			Def;
		{_,T} ->
			case proplists:lookup(Key,T) of

				none ->
					Def;
				{_,V}    ->
					V
			end
	end.

insert(Key,Val,Mat)->
	%%io:format("<<< INSERT >>> {~p,~p} ~p ~n",[Key,Val,Mat]),
	case proplists:lookup(wpc_rspt,Mat) of
	
		none ->
			%%io:format("<< insert into empty >> ~n",[]),
			[{wpc_rspt,[{Key,Val}]}]++Mat;

		{_,List} ->
			%%io:format("<< insert into proplist >>~n",[]),
			[{wpc_rspt, [{Key,Val}] ++ proplists:delete(Key,List)}]
			++ proplists:delete(wpc_rspt,Mat)
	end.

insert([{Key,Val}|T],Mat)->
	insert(T,insert(Key,Val,Mat));
insert([],Mat)->
	%% ignore other ??
	Mat;
insert(_,Mat)->
	Mat.




%%%%%%%%%%%
%%%%%%%%%%%
%%%%%%%%%%% Ma program, anything that can be defined by OpenGL
%%%%%%%%%%%
%%%%%%%%%%%

maProg(V,ProgID)->
	case proplists:lookup(wpc_rspt,V) of

		none ->
			%%io:format("Material has no rspt ext ~n ~p ~n============~n",[V]),
			progZero(ProgID);
		
		{_,Ents} ->
			
			case proplists:lookup(ProgID,Ents) of

				none ->
					%% skip 
					progZero(ProgID);
				{_,standard} ->
					%% skip this entry
					progZero(ProgID);		

				{_,red_only} ->	
					%%io:format("Using red only in program!! ~p ~n",[ProgID]),
					gl:disable(?GL_TEXTURE_2D),
					gl:disable(?GL_LIGHTING),
					gl:color3f(1,0,0);
				{_,blend} ->
					gl:logicOp(?GL_INVERT),
					gl:enable(?GL_COLOR_LOGIC_OP),
					ok;
				{_,Mod} ->
					%%io:format("Calling module ~p ~n",[Mod]),
					%%io:format("Result: ~p ~n",[catch Mod:attrib(V,ProgID)]),
					catch Mod:attrib(V,ProgID),
					ok
				%% plugin system here now ..
			end
	end.




%%
%%Program is zero or none, skip data for anything not Prog0
%%
progZero(ProgID)->					
	%% no prog, change scene only if appropriate
	case ProgID of

		ambFragProg0 ->
			ok;
		diffFragProg0 ->
			ok;

	
			_ ->
				%%io:format("Using all white in ~p ~n",[ProgID]),
				%% perhaps disable buffer writing here ?? 
				%% not done since interfering with other stuff
				%%gl:disable(?GL_TEXTURE_2D),
				gl:color3f(1,1,1),
				gl:blendEquation(?GL_FUNC_ADD),
				gl:blendFunc(?GL_ZERO,?GL_ONE),
				gl:enable(?GL_BLEND),
				ok
			
	end,
	ok.


%%
%% Create material window ...
%%
material_window(Name,#st{mat=Mat}=St)->

	AName = list_to_atom(Name),
	
	%% fix any material without rspt funs
	fix_rspt_mat(AName),

	%%io:format("Materials ~p ~n",[wings_wm:get_current_state()]),
	%% bombs if renaming materials ??!! check
	Req = fun()-> 
			#st{mat=OMat} = wpc_rspt_wm:get_current_state(),
			[{AName,gb_trees:get(AName,OMat)}]
		end,
	wpc_rspt_wm:window(list_to_atom(Name++"_material_"++"window"),"Material properties " ++ Name,
				[Name,""],[],Req,fun()-> wpc_rspt_wm:legend_material(Name) end,160,fun(Data)-> material_chain_defs(Data) end),

		%%%%%%% [{AName,gb_trees:get(AName,Mat)}]),
	wpc_rspt_wm:get_current_state().

%%
%% Include material attribute settings
%%
material_chain_defs(Data0)->
	Data = material_def_chains(Data0),
	Elem = wpc_rspt_ps:getPrefixed("material_event_chain"),
	lists:foldl(fun(A,Acc)->
				case catch(A(Acc)) of
					{'EXIT',W} ->
						%% should not be removed
						io:format("Warning plugin error reported as ~p keeping material_chain~n",[W]),
						Acc;
					Acc1       ->
						Acc1
				end
			end,Data,Elem).


material_def_chains(Data0)->
	F1 = fun(X)->
		%%io:format("Mat diffFragProg0 handler called!!~n",[]),
		case X of
			height ->
				1;
			{render,Zatom,IX,Y}->	
				Text = atom_to_list(Zatom),
				gl:color3f(0,0,1),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				gl:color3f(0,0,0),
				Y + ?LINE_HEIGHT;		
			{push,HW,[S|Obj],PX,PY} ->
				%% should rotate Obj
				%%io:format("Target ~p called~n",[Obj]),
				%% value is 
				OVal = path_get([object|Obj]),
				NVal = grab_next_menu_option([standard] ++ filter_a(wpc_rspt_ps:get_mat("diff",0)),OVal),
				path_set(NVal,[object|Obj]), %% finished
				catch(NVal:material_opts([S|Obj])),
				HW;	
			_->
				io:format("Diff handler ignoring ~p~n",[X])
		end
	     end,
	D1 = wpc_rspt_wm:chain_register({[atom,diffFragProg0,wpc_rspt],F1},Data0),
	F2 = fun(X)->
		%%io:format("Mat diffFragProg0 handler called!!~n",[]),
		case X of
			height ->
				1;
			{render,Zatom,IX,Y}->	
				Text = atom_to_list(Zatom),
				gl:color3f(0,0,1),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				gl:color3f(0,0,0),
				Y + ?LINE_HEIGHT;		
			{push,HW,[S|Obj],PX,PY} ->
				OVal = path_get([object|Obj]),
				NVal = grab_next_menu_option([standard] ++ filter_a(wpc_rspt_ps:get_mat("amb",0)),OVal),
				path_set(NVal,[object|Obj]), %% finished
				catch(NVal:material_opts([S|Obj])),
				HW;	
			_->
				io:format("Amb handler ignoring ~p~n",[X])
		end
	     end,
	D2 = wpc_rspt_wm:chain_register({[atom,ambFragProg0,wpc_rspt],F2},D1),
	F3 = fun(X)->
		case X of
			height ->
				1;
			{render,Zatom,IX,Y}->	
				Text = atom_to_list(Zatom),
				gl:color3f(0,0,1),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				gl:color3f(0,0,0),
				Y + ?LINE_HEIGHT;		
			{push,HW,[S|Obj],PX,PY} ->
				OVal = path_get([object|Obj]),
				NVal = grab_next_menu_option([standard] ++ filter_a(wpc_rspt_ps:get_mat("ambPipe")),OVal),
				path_set(NVal,[object|Obj]), %% finished
				catch(NVal:material_opts([S|Obj])),
				HW;	
			_->
				io:format("DiffPrg handler ignoring ~p~n",[X])
		end
	     end,
	D3 = wpc_rspt_wm:chain_register({[atom,ambPipePrg,wpc_rspt],F3},D2),
	F4 = fun(X)->
		case X of
			height ->
				1;
			{render,Zatom,IX,Y}->	
				Text = atom_to_list(Zatom),
				gl:color3f(0,0,1),
				wings_io:text_at(IX,Y+?LINE_HEIGHT,Text),
				gl:color3f(0,0,0),
				Y + ?LINE_HEIGHT;		
			{push,HW,[S|Obj],PX,PY} ->
				OVal = path_get([object|Obj]),
				NVal = grab_next_menu_option([standard] ++ filter_a(wpc_rspt_ps:get_mat("diffPipe")),OVal),
				path_set(NVal,[object|Obj]), %% finished
				catch(NVal:material_opts([S|Obj])),
				HW;	
			_->
				io:format("AmbPrg handler ignoring ~p~n",[X])
		end
	     end,
	D4 = wpc_rspt_wm:chain_register({[atom,diffPipePrg,wpc_rspt],F4},D3).



filter_a([{_,V}|T])->
	[V|filter_a(T)];
filter_a([])->
	[].

fix_rspt_mat(AName)->
	%%io:format("Fixing material ~p~n",[AName]),

	%%io:format("Fixing material ~p~n",[AName]),
	path_set_maybe(standard,[object,diffFragProg0,wpc_rspt,AName]),
	path_set_maybe(nothing,[object,diffFragProg0Opts,wpc_rspt,AName]),
	path_set_maybe(standard,[object,ambFragProg0,wpc_rspt,AName]),
	path_set_maybe(nothing,[object,ambFragProg0Opts,wpc_rspt,AName]),
	path_set_maybe(standard,[object,ambPipePrg,wpc_rspt,AName]),
	path_set_maybe(nothing,[object,ambPipePrgOpts,wpc_rspt,AName]),
	path_set_maybe(standard,[object,diffPipePrg,wpc_rspt,AName]),
	path_set_maybe(nothing,[object,diffPipePrgOpts,wpc_rspt,AName]),
	ok.

grab_next_menu_option([E|_]=List,Val)->
	case grab_next_menu_option_1(List,Val) of
		fail ->
			E;
		NVal ->
			NVal
	end.

grab_next_menu_option_1([E,T|_],E)->
	T;
grab_next_menu_option_1([],E)->
	fail;
grab_next_menu_option_1([_|T],V)->
	grab_next_menu_option_1(T,V).

%%%%%%%%%%%
%%%%%%%%%%%
%%%%%%%%%%% Path based set and get of material state, will make other versions obsolete in the future .. 
%%%%%%%%%%% Should there be a safe version ?? maybe not
%%%%%%%%%%%
%%%%%%%%%%%
%%%%%%%%%%% Typical path is  [Service,PathN,PathN-1,PathN-2,..,Path3,Path2,Path1,Path0]
%%%%%%%%%%% where PathN - Path0  is an atom
%%%%%%%%%%% Service is also an atom
%%%%%%%%%%%
%%%%%%%%%%% Current services 
%%%%%%%%%%%
%%%%%%%%%%%  object     set or get data raw ..
%%%%%%%%%%%  dir        directory set or get ?? (not material set, and not present here)
%%%%%%%%%%%

%-export([path_get/3,path_get/2,path_set/4,path_set/3]).

%% expansion to below
path_get(Path)->
	path_get(wpc_rspt_wm:get_current_state(),Path).


%% to be compatile with attrib calls to material (read only), Path is here incomplete
path_get(D,[read | Tail])->
	path_get_rec(lists:reverse(Tail),D);

%% first name is service and not entity
path_get(St,[object | Tail])->
	path_get_1(St,Tail).

path_get_1(#st{mat=Mat}=St,Path)->
	[Name|Tail] = lists:reverse(Path),
	TMat = gb_trees:get(Name,Mat),
	path_get_rec(Tail,TMat).

path_get_rec([Val|Tail],Tmat)->
	path_get_rec(Tail,proplists:get_value(Val,Tmat));
path_get_rec([],Data)->
	Data.



%%
%% do get current set current using wpc_rspt_wm -> wings_wm
%%
path_set(Val,Path)->
	St0 = wpc_rspt_wm:get_current_state(),
	%%io:format("Setting path ~p ~p ~n",[Val,Path]),
	St1 = path_set(St0,Val,Path),
	St2 = wpc_rspt_wm:undo_save(St0,St1),
	%% send this ??
	wpc_rspt_wm:set_current_state(St2).
	

path_set(St,Val,[object|Tail])->
	path_set_1(St,Val,Tail).

path_set_1(#st{mat=Mat}=St,Val,Path)->
	[Name | Tail] = lists:reverse(Path),
	case gb_trees:lookup(Name,Mat) of
		{value,Elem} ->
			St#st{mat=gb_trees:update(Name,path_set_rec(Tail,Elem,Val),Mat)};

		none ->
			St#st{mat=gb_trees:insert(Name,path_set_rec(Tail,[],Val),Mat)}
	end.


path_set_rec([Val|Tail],[{Val,Elem}|Tmat],Nobj)->
	[{Val,path_set_rec(Tail,Elem,Nobj)}|Tmat];
path_set_rec([Val|Tail],[Elem|Tmat],Nobj)->
	[Elem|path_set_rec([Val|Tail],Tmat,Nobj)];
path_set_rec([Val|Tail],[],Nobj)->
	[{Val,path_set_rec(Tail,[],Nobj)}];
path_set_rec([],_,Nobj)->
	Nobj.	

%%
%% changes order of elems bad
%%

%path_set_rec([Val|Tail],Tmat,Nobj)->
%	%%io:format(".",[]),
%	case proplists:get_value(Val,Tmat) of
%
%		undefined ->
%				%% does not exist, add path
%				Tmat ++ [{Val,path_set_rec(Tail,[],Nobj)}];
%		Elem      ->
%				proplists:delete(Val,Tmat) ++ [{Val,path_set_rec(Tail,Elem,Nobj)}]
%	end;
%%
%%
%% should we catch bogus sets ? 
%% No since there is no way of knowing what the set request wants, ie might be intentional ...
%%
%path_set_rec([],_,Nobj)->
%	Nobj.


		
%%
%% Set maybe
%%
path_set_maybe(Val,Path)->
	%%io:format("Set path maybe~n",[]),
	case catch(path_get(Path)) of
		{'EXIT',_} ->
			%io:format("Setting material ~p ~p ~n",[Val,Path]),
			path_set(Val,Path);
		undefined  ->
			%io:format("Setting material ~p ~p ~n",[Val,Path]),
			path_set(Val,Path);
		_ ->
			%% somethings already there skip!!
			ok
	end.



%%
%% get maybe
%%
path_get_maybe(Val,Path,Def)->
	case catch(path_get(Val,Path)) of
		{'EXIT',Reason} ->
			%%io:format("Not finding cause: ~p ~n",[Reason]),
			Def;
		undefined ->
			Def;
		V ->
			V
	end.	


path_get_maybe(Path,Def)->
	 path_get_maybe(wpc_rspt_wm:get_current_state(),Path,Def).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% New material override, use proper edit window
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% possible better to clone default if present and assuming that as default
new_material(NameI,Ass,#st{mat=Mat}=St)->
	Name1 = list_to_atom(NameI),
	case gb_trees:is_defined(Name1,Mat) of
		true ->
			Names = [atom_to_list(N) || N <- gb_trees:keys(Mat)],
			Name  = wpc_rspt_ps:unique_name(NameI,Names),
			new_material_1(Name,Ass,St);
		false ->
			new_material_1(NameI,Ass,St)
	end.

new_material_1(Name,Ass,#st{mat=Mat}=St)->
	TMa = wpc_rspt_ps:material_make_default({1.0,1.0,1.0},1.0),
	%%io:format("Setting Name ~p  as ~p ~n Mat ~p ~n",[Name,TMa,gb_trees:insert(list_to_atom(Name),TMa,Mat)]),
	St1 = St#st{mat=gb_trees:insert(list_to_atom(Name),TMa,Mat)},
	{Name,St1}.


