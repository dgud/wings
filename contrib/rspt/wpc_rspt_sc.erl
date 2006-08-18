%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_sc.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%
%%
%%This is shape compile for wpc_rspt_re.erl !!
%%
%%
%%
%%

-module(wpc_rspt_sc).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").
-define(max_iters,500).
-export([init/0,menu/2,command/2]).
-export([all/2,all/4,grab_adjacent_edges/2,surface_normal/2,surface_4param/2,surface_vertex/2]).

-define(debugP(X,W),ok).
-define(debugR(X,W),ok).
%-define(debugR(X,W),io:format(X,W)).
-define(debugM(X,W),ok).
%-define(debugM(X,W),io:format(X,W)).
-define(debug2(X,W),ok).
%-define(debugN(X,W),io:format(X,W)).
-define(debugN(X,W),ok).

%%-define(RENDER_DIR,counter_clock).
-define(RENDER_DIR,clockwise).

%% link into app
init()->
	%%io:format("Loading wpc_rspt_sc ... ~n",[]),
	true.

menu(_, Menu)->
	Menu.

command(_,_)-> next.


%%
%% render all
%%
all(St,Scene)->
	%pure_render_all(St,Scene).
	pure_render_all_by_mat(St,Scene).


%%
%% render all at programed pass Pass
%%
all(St,Scene,Pass,ExtObj)->
	pure_render_all_by_mat(St,Scene,Pass,ExtObj).

%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% pure render stuff
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%

%%
%%Do it by hand nuff said
%%Needed either way because of mirror and misc
%%list calls should be outside this if faster renders are required ...
pure_render_all(#st{shapes=Sh}=St,Scene)->
	% do not need Acc here anyway realy
	lists:foldl(fun({N,X},_)->
			?debugN("Rendering ~p ~n",[N]),
			case ?IS_NOT_LIGHT(X) of
				true ->
					pure_render_obj(X,St,Scene);
				false->
					ok
			end
		    end,ok ,gb_trees:to_list(Sh)),
	ok.

pure_render_obj(#we{fs=FS,mode=M,mat=Mat}=WE,#st{}=ST,Scene)->
	% possibly messes up the rendering, may ditch this in the future ...
	%case M of
	%	vertex ->
	%		gl:disable(?GL_LIGHTING);
	%	material ->
	%		gl:enable(?GL_LIGHTING);
	%	Other    ->
	%		gl:enable(?GL_LIGHTING)
	%end,
	%%io:format("========================================~nWE is ~n ~p ~n===================================~n",[WE]),
	%%io:format("Object material is ~p~n",[Mat]),
	lists:foldl(fun({F,X},Acc)->
			pure_render_face(F,X,WE,ST,Scene)
		    end,ok,gb_trees:to_list(FS)),
	ok.

%%
%%How does the face look hmm shoud be incident vertex
%% cycle through all vertexes after that
%% ignore vertex material, probably a waste of time
%%
pure_render_face(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene)->
		%% fix the material
		wpc_rspt_ma:face(F,WE,ST,Scene),

		%io:format("Rendering object ~p ~n",[Name]),
		Edge = gb_trees:get(X,ES),
		%% ' needed, from where is the begin token ??
		?debugP("Poly->",[]),
		gl:'begin'(?GL_POLYGON),
		%%gl:color3f(1,1,1),
		%% walk edge with face F
		pure_render_face_1(Edge,F,WE,ST,start,clockwise),
		gl:'end'(),
		?debugP("~n",[]),
		ok.


%%
%%Cycle through all edges stopping at edge Fin
%%
pure_render_face_1(Edge,F,WE,ST,Edge,Order)->
	ok;
pure_render_face_1(#edge{lf=F,ltsu=LTSU,ltpr=LTPR}=Edge,F,#we{es=ES}=WE,ST,start,Order)->
	?debugR("(startl)",[]),
	edge_render_poly(Edge,F,WE,Order),
	case Order of
		clockwise ->
			pure_render_face_1(gb_trees:get(LTSU,ES),F,WE,ST,Edge,Order);
		counter_clock ->
			pure_render_face_1(gb_trees:get(LTPR,ES),F,WE,ST,Edge,Order)
	end;
pure_render_face_1(#edge{rf=F,rtsu=RTSU,rtpr=RTPR}=Edge,F,#we{es=ES}=WE,ST,start,Order)->
	?debugR("(startr next ~p ~p)",[gb_trees:get(RTSU,ES),gb_trees:get(RTPR,ES)]),
	edge_render_poly(Edge,F,WE,Order),
	case Order of
		clockwise ->
			pure_render_face_1(gb_trees:get(RTSU,ES),F,WE,ST,Edge,Order);
		counter_clock ->
			pure_render_face_1(gb_trees:get(RTPR,ES),F,WE,ST,Edge,Order)
	end;
pure_render_face_1(#edge{lf=F,ltsu=LTSU,ltpr=LTPR}=Edge,F,#we{es=ES}=WE,ST,Fin,Order)->
	?debugR("l",[]),
	edge_render_poly(Edge,F,WE,Order),
	case Order of
		clockwise ->
			pure_render_face_1(gb_trees:get(LTSU,ES),F,WE,ST,Fin,Order);
		counter_clock ->
			pure_render_face_1(gb_trees:get(LTPR,ES),F,WE,ST,Fin,Order)
	end;
pure_render_face_1(#edge{rf=F,rtsu=RTSU,rtpr=RTPR}=Edge,F,#we{es=ES}=WE,ST,Fin,Order)->
	?debugR("r",[]),
	edge_render_poly(Edge,F,WE,Order),
	case Order of
		clockwise ->
			pure_render_face_1(gb_trees:get(RTSU,ES),F,WE,ST,Fin,Order);
		counter_clock ->
			pure_render_face_1(gb_trees:get(RTPR,ES),F,WE,ST,Fin,Order)
	end;
pure_render_face_1(E,F,WE,ST,S,Or)->
	% something is realy wrong
	?debugP("~nrender_face_1 ~n~p~n ~p~n ~p~n ~p~n ~p~n ~p ~n",[E,F,too_long,too_long,S,Or]),
	throw("pure_render_face_1 fault!!!").


%%
%%render edge part, may want to improve on this
%%  does not do hard edges properly ..... ?
%%
%% Subtle error with texCoordinates here, both a and b are wrong, what gives ??
%%  probably an error from caller above !!!
%% Something to do with first or last vertex being wrong, very likely a vertex does not receive
%% a texcoord because of faults in the start issue ?? odd,anyway
%%
%%
%%
%%
edge_render_poly(#edge{vs=VS,ve=VE,b=B,a=A,lf=Face},Face,#we{vp=VP}=WE,clockwise)->
	% clockwise as calc from order of wings 
	wpc_rspt_ma:uvc(A),
	%gl:color3fv(B),
	gl:normal3fv(wpc_rspt_ve:normal(VS,Face,WE)),
	gl:vertex3fv(gb_trees:get(VS,VP)),
	?debugP(" ~pe ~p",[VS,gb_trees:get(VE,VP)]),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,rf=Face},Face,#we{vp=VP}=WE,clockwise)->
	% clockwise as calc from order of wings 
	wpc_rspt_ma:uvc(B),
	%gl:color3fv(A),
	gl:normal3fv(wpc_rspt_ve:normal(VE,Face,WE)),
	gl:vertex3fv(gb_trees:get(VE,VP)),
	?debugP(" ~ps ~p",[VE,gb_trees:get(VS,VP)]),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,lf=FACE},Face,#we{vp=VP}=WE,counter_clock)->
	% counter clockwise as calc from order of wings 
	wpc_rspt_ma:uvc(B),
	%gl:color3fv(B),
	gl:normal3fv(wpc_rspt_ve:normal(VE,Face,WE)),
	gl:vertex3fv(gb_trees:get(VE,VP)),
	?debugP("cc~p",[gb_trees:get(VE,VP)]),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,rf=FACE},Face,#we{vp=VP}=WE,counter_clock)->
	% counter clockwise as calc from order of wings 
	wpc_rspt_ma:uvc(A),
	%gl:color3fv(A),
	gl:normal3fv(wpc_rspt_ve:normal(VS,Face,WE)),
	gl:vertex3fv(gb_trees:get(VS,VP)),	
	?debugP("cc~p",[gb_trees:get(VS,VP)]),
	ok.

%%%%
%%%%Need a surface normal for one surface for various misc reasons like mirrors ...
%%%%
surface_normal(EID,Face,#we{es=ES}=WE)->
	Vec = case gb_trees:get(EID,ES) of

			#edge{rf=Face,rtsu=RTSU} ->
					surface_normal_i(EID,RTSU,WE,right);
			#edge{lf=Face,ltsu=LTSU} ->
					surface_normal_i(EID,LTSU,WE,left)
		end,
	wpc_rspt_ve:normalize(Vec).



%
%Will use both edges and compute each normal and normalize
%
%
surface_normal(EID,#we{es=ES}=WE)->
	#edge{rtsu=RTSU,ltsu=LTSU}=CE = gb_trees:get(EID,ES),
	%?debugM("surface normal with edge ~p ~n",[CE]),
	Avec  = surface_normal_i(EID,RTSU,WE,right),
	Bvec  = surface_normal_i(EID,LTSU,WE,left),
	Anorm = wpc_rspt_ve:normalize(Avec),
	Bnorm = wpc_rspt_ve:normalize(Bvec),
	wpc_rspt_ve:add(Anorm,Bnorm).

%
%Produce vector normal i
%
surface_normal_i(FIRST,SUCC,#we{es=ES}=WE,left)->
	#edge{vs=VS ,ve=VE }  = E1 = gb_trees:get(FIRST,ES),
	#edge{vs=VS2,ve=VE2}  = E2 = gb_trees:get(SUCC,ES),
	?debugM("normal left E1,E2 ~n~p~n~p~n~n",[E1,E2]),
	V1 = wpc_rspt_ve:create(VE,VS,WE),
	% need to build proper vector
	V2 = case E2 of
		% went left
		#edge{vs=VS} ->
			wpc_rspt_ve:create(VS2,VE2,WE);
		#edge{ve=VS} ->
			wpc_rspt_ve:create(VE2,VS2,WE)
		end,
	wpc_rspt_ve:cross_product(V1,V2);
surface_normal_i(FIRST,SUCC,#we{es=ES}=WE,right)->
	#edge{vs=VS ,ve=VE }  = E1 = gb_trees:get(FIRST,ES),
	#edge{vs=VS2,ve=VE2}  = E2 = gb_trees:get(SUCC,ES),
	?debugM("normal right E1,E2 ~n~p~n~p~n~n",[E1,E2]),
	V1 = wpc_rspt_ve:create(VS,VE,WE),
	% need to build proper vector
	V2 = case E2 of
		% went right
		#edge{vs=VE} ->
			wpc_rspt_ve:create(VS2,VE2,WE);
		#edge{ve=VE} ->
			wpc_rspt_ve:create(VE2,VS2,WE)
		end,
	wpc_rspt_ve:cross_product(V1,V2).



%
%Error with some strange shapes, may loop indefinetly (incorrect loops?)
% I will use a max loop counter to stop this, there are always troubled meshes ...
%
%Catch messy meshes
grab_adjacent_edges(_,_,_,_,0)->
	?debug2("$",[]),
	throw({reduce_problem,recursion_astray}),
	[];
%Stop executing
grab_adjacent_edges(IncEdg,TarVer,IncEdg,EdgTre,Iter)->
	?debug2("!",[]),
	[];
grab_adjacent_edges(IncEdg,TarVer,CurEdg,EdgTre,Iter)->
	?debug2(".",[]),
	%?debug("Recurse on grab_adj_edge ~p ~p ~p ~p ~n",[IncEdg,TarVer,CurEdg,EdgTre]),
	case gb_trees:get(CurEdg,EdgTre) of
		
		#edge{vs=TarVer,rtpr=Targ}=E ->
				[{CurEdg,E} | grab_adjacent_edges(IncEdg,TarVer,Targ,EdgTre,Iter-1)];
		#edge{ve=TarVer,ltpr=Targ}=E ->
				[{CurEdg,E} | grab_adjacent_edges(IncEdg,TarVer,Targ,EdgTre,Iter-1)]
	end.

grab_adjacent_edges_i(TarVer,#we{es=EdgTre,vc=VC}=WE)->
	?debug2(",",[]),
	Edge = gb_trees:get(TarVer,VC),
	%?debug("Start on grab_adj_edge ~p ~p ~p ~p ~n",[Edge,TarVer,VC,EdgTre]),
	case gb_trees:get(Edge,EdgTre) of
		
		#edge{vs=TarVer,rtpr=Targ}=E ->
				[{Edge,E} | grab_adjacent_edges(Edge,TarVer,Targ,EdgTre,?max_iters)];
		#edge{ve=TarVer,ltpr=Targ}=E ->
				[{Edge,E} | grab_adjacent_edges(Edge,TarVer,Targ,EdgTre,?max_iters)]
	end.

%
%Needed because of the double side bug and threat for corruption 
% (handle double faces properly instead ?,hmm... )
%
grab_adjacent_edges(TarVer,#we{es=ES0}=WE)->
	case catch grab_adjacent_edges_i(TarVer,WE) of
		{reduce_problem,recursion_astray} ->
			% run slow
			%%?debug2("Bad_recursion!!!~n",[]),
			%%exit(bad_recursion),
			[{ID,El} || ({ID,El}) <- (gb_trees:to_list(ES0)), El#edge{vs=TarVer} or El#edge{ve=TarVer}];
		Other ->
			Other
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Render by material, below, working on a useful cpu based material
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% for mirror support.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% To render by cpu
%%
pure_render_all_by_mat(#st{mat=Mat}=St,Scene,Pass,ExtObj)->
	lists:foldl(fun({_,MD}=M,Acc)->
			case wpc_rspt_ma:gval(Pass,MD,standard) of
				standard ->
					ok;
				Other ->
					io:format("Rendering Pass ~p with ~p ~n",[Pass,Other]),
					pure_render_scene_by_mat(Acc,M,St,Scene,{Other,Pass},ExtObj),
					ok
			end,
			Acc+1
			end,Scene+1,gb_trees:to_list(Mat)),
	ok.

pure_render_all_by_mat(#st{mat=Mat}=St,Scene)->
	%%io:format("Mat trees to list ~p ~n",[gb_trees:to_list(Mat)]),
	lists:foldl(fun(X,Acc)->
			pure_render_scene_by_mat(Acc,X,St,Scene,none,none),
			Acc+1
			end,Scene+1,gb_trees:to_list(Mat)),
	ok.


%%
%% get number for material, slow ..
%%
resolve_number(MName,#st{mat=Mat}=St,Scene) when is_atom(MName) ->
	Keys = gb_trees:keys(Mat),
	resolve_number(Keys,MName,Scene+1);
resolve_number([Matv|_],Matv,Num)->
	Num;
resolve_number([_|T],Matv,Num)->
	resolve_number(T,Matv,Num+1).


%%
%% FF is a function that should be exected for every face ..
%%
pure_render_scene_by_mat({MName,_}=Mat,#st{shapes=Sh}=St,Scene,FF,ExtObj)->
	pure_render_scene_by_mat(resolve_number(MName,St,Scene),Mat,#st{shapes=Sh}=St,Scene,FF,ExtObj).

pure_render_scene_by_mat(Num,{MName,_}=Mat,#st{shapes=Sh}=St,Scene,FF,ExtObj)->
	%% bind material params to surfaces
	io:format("Numerical material value ~p scene number is ~p ~n",[Num,Scene]),
	gl:callList(Num),
	lists:foldl(fun({Name,X},_)->
			case ?IS_NOT_LIGHT(X) of
				true ->
					pure_render_obj_by_mat(Num,Mat,X,St,Scene,FF,ExtObj);
				false ->
					ok
			end
		    end,ok,gb_trees:to_list(Sh)),
	ok.

pure_render_obj_by_mat(Num,{MName,_}=Mat,#we{mat=OMat}=WE,St,Scene,FF,ExtObj) when is_atom(OMat), is_atom(FF) ->
	case OMat of
		MName ->		
			%% switch to normal rendering (should have fun to affect rendering process somewhere, will change ...!!!)
			pure_render_obj_by_mat(WE,St,Scene),  %% takes time to process normals
			ok;
		_ ->
			ok
	end,
	ok;
pure_render_obj_by_mat(Num,{MatName,Mat},#we{mat=OMat,fs=FS}=WE,St,Scene,FF,ExtObj) when is_atom(FF) ->
	[pure_render_face_by_mat(FaceN,gb_trees:get(FaceN,FS),WE,St,Scene) || {FaceN,FaceMat} <- OMat, FaceMat =:= MatName ],
	ok;
%%
%%
pure_render_obj_by_mat(Num,{MName,_}=Mat,#we{mat=OMat}=WE,St,Scene,FF,ExtObj) when is_atom(OMat) ->
	case OMat of
		MName ->		
			pure_render_obj_by_mat(WE,St,Scene,FF,ExtObj),  %% run with fun
			ok;
		_ ->
			ok
	end,
	ok;
pure_render_obj_by_mat(Num,{MatName,Mat},#we{mat=OMat,fs=FS}=WE,St,Scene,FF,ExtObj) ->
	[pure_render_face_by_mat(FaceN,gb_trees:get(FaceN,FS),WE,St,Scene,FF,ExtObj) || {FaceN,FaceMat} <- OMat, FaceMat =:= MatName ],
	ok.





pure_render_obj_by_mat(#we{fs=FS,mode=M,mat=Mat}=WE,#st{}=ST,Scene)->
	lists:foldl(fun({F,X},Acc)->
			pure_render_face_by_mat(F,X,WE,ST,Scene)
		    end,ok,gb_trees:to_list(FS)),
	ok.

pure_render_obj_by_mat(#we{fs=FS,mode=M,mat=Mat}=WE,#st{}=ST,Scene,FF,ExtObj)->
	lists:foldl(fun({F,X},Acc)->
			pure_render_face_by_mat(F,X,WE,ST,Scene,FF,ExtObj)
		    end,ok,gb_trees:to_list(FS)),
	ok.
			

pure_render_face_by_mat(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene)->

	Edge = gb_trees:get(X,ES),
	gl:'begin'(?GL_POLYGON),
	%%pure_render_face_1(Edge,F,WE,ST,start,clockwise),
	pure_render_face_1(Edge,F,WE,ST,start,?RENDER_DIR),
	gl:'end'(),
	ok.

%%
%%Cpu based material send results to FF from here and let callback do a take over ..
%% should clear and set the stencil buffer (clearing expensiv ?? could reduce clears ..)
%% activate it to secure the rest of the scene ..
%% May interfer with unsharp mask but that should perhaps be moved elsewhere ...
%%
pure_render_face_by_mat(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene,{Mod,FF},ExtObj)->
	%%io:format("Calling ~p : ~p ~n",[Mod,FF]), 
	
	gl:disable(?GL_BLEND),
	%% setup stencil buffer for clipping ...
	gl:enable(?GL_STENCIL_TEST),
	%%gl:clearStencil(0),
	gl:clear(?GL_STENCIL_BUFFER_BIT),
	gl:stencilFunc(?GL_ALWAYS,1,1),
	gl:stencilOp(?GL_ZERO,?GL_ZERO,?GL_REPLACE),		

	gl:colorMask(?GL_FALSE,?GL_FALSE,?GL_FALSE,?GL_FALSE),	
	
	ExtObj(mat_none),
	gl:callList(Scene+1),
	gl:disable(?GL_LIGHTING),
	gl:color3f(1,1,1),
	Edge = gb_trees:get(X,ES),
	gl:'begin'(?GL_POLYGON),
	pure_render_face_1(Edge,F,WE,ST,start,clockwise),
	gl:'end'(),

	%%gl:disable(?GL_STENCIL_TEST),
	gl:enable(?GL_LIGHTING),

	%% now render whatever plugin wants
	gl:stencilFunc(?GL_EQUAL,1,1),
	gl:stencilOp(?GL_KEEP,?GL_KEEP,?GL_KEEP),
	%%gl:color3f(0,0,1),
	gl:colorMask(?GL_TRUE,?GL_TRUE,?GL_TRUE,?GL_TRUE),
	catch (Mod:FF({F,X,WE,ST,Scene,ExtObj})),
	%%gl:clearStencil(1),
	gl:disable(?GL_STENCIL_TEST),
	ok.


%%%
%%% get the 4parameter quation of a surface ...
%%%
surface_4param(Face,#we{es=ES,fs=FS,vp=VP}=WE)->
	EID = gb_trees:get(Face,FS),
	#edge{vs=VS} =Edge = gb_trees:get(EID,ES),
	{XV,YV,ZV} = surface_vertex(Face,WE),
	{NX,NY,NZ} = surface_normal(EID,Face,WE),
	%% calculate the last entry
	NW = - (NX*XV + NY*YV + NZ*ZV),
	{NX,NY,NZ,NW}.

surface_vertex(Face,#we{es=ES,fs=FS,vp=VP}=WE)->
	EID = gb_trees:get(Face,FS),
	#edge{vs=VS} =Edge = gb_trees:get(EID,ES),
	{XV,YV,ZV} = gb_trees:get(VS,VP).
