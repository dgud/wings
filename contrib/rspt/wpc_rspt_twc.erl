%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_twc.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%
%
% A module to compile as scene scene data
% (should be based on oher compile methods)
%
%
%

-module(wpc_rspt_twc).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-define(max_iters,500).

-export([init/0,menu/2,command/2]).
-export([all/3,ff_all/6,select_vert/2,select_verts/2,render_selection/2]).
-export([getData_selection/2,getData_selection/1,setData_selection/2,setData_selection/2]).
-export([sphere_map/1,ycylinder_map/1,xcylinder_map/1,zcylinder_map/1,popup_menu_register/0,iterator_funmap/2]).
-export([iterator_funmapUV/2]).
-export([popup_menu_entries/1]).
%select exports
-export([select_all/1,select_invert/2,select_by_fun/3,select_clockwise/2,select_selection/2,select_normal/3,select_neighbours/2]).



%% link into app
init()->
	true.

menu(_, Menu)->
	Menu.

command(_,_)-> next.

%%
%% render all
%%
%%in: St:St Scene:callist  What:(vertex | face | line | locked)
%%
all(St,Scene,What)->
	all(St,Scene,What,none).


all(St,Scene,What,FF)->
	pure_render_all_by_mat(St,Scene,What,FF).


%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% pure render stuff
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%



%%
%%How does the face look hmm shoud be incident vertex
%% cycle through all vertexes after that
%% ignore vertex material, probably a waste of time
%%
pure_render_face(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene,FF)->
		%% fix the material
		wpc_rspt_ma:face(F,WE,ST,Scene),

		%io:format("Rendering object ~p ~n",[Name]),
		Edge = gb_trees:get(X,ES),
		%% ' needed, from where is the begin token ??
		gl:'begin'(?GL_POLYGON),
		%%gl:color3f(1,1,1),
		%% walk edge with face F
		pure_render_face_1(X,Edge,F,WE,ST,start,clockwise,FF),
		gl:'end'(),
		ok.


%%
%%Cycle through all edges stopping at edge Fin
%%
pure_render_face_1(EID,Edge,F,WE,ST,Edge,Order,FF)->
	ok;
pure_render_face_1(EID,#edge{lf=F,ltsu=LTSU,ltpr=LTPR}=Edge,F,#we{es=ES}=WE,ST,start,Order,FF)->
	case Order of
		clockwise ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,LTSU),
			pure_render_face_1(LTSU,gb_trees:get(LTSU,ES),F,WE,ST,Edge,Order,FF);
		counter_clock ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,LTPR),
			pure_render_face_1(LTPR,gb_trees:get(LTPR,ES),F,WE,ST,Edge,Order,FF)
	end;
pure_render_face_1(EID,#edge{rf=F,rtsu=RTSU,rtpr=RTPR}=Edge,F,#we{es=ES}=WE,ST,start,Order,FF)->
	case Order of
		clockwise ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,RTSU),
			pure_render_face_1(RTSU,gb_trees:get(RTSU,ES),F,WE,ST,Edge,Order,FF);
		counter_clock ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,RTPR),
			pure_render_face_1(RTPR,gb_trees:get(RTPR,ES),F,WE,ST,Edge,Order,FF)
	end;
pure_render_face_1(EID,#edge{lf=F,ltsu=LTSU,ltpr=LTPR}=Edge,F,#we{es=ES}=WE,ST,Fin,Order,FF)->
	case Order of
		clockwise ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,LTSU),
			pure_render_face_1(LTSU,gb_trees:get(LTSU,ES),F,WE,ST,Fin,Order,FF);
		counter_clock ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,LTPR),
			pure_render_face_1(LTPR,gb_trees:get(LTPR,ES),F,WE,ST,Fin,Order,FF)
	end;
pure_render_face_1(EID,#edge{rf=F,rtsu=RTSU,rtpr=RTPR}=Edge,F,#we{es=ES}=WE,ST,Fin,Order,FF)->
	case Order of
		clockwise ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,RTSU),
			pure_render_face_1(RTSU,gb_trees:get(RTSU,ES),F,WE,ST,Fin,Order,FF);
		counter_clock ->
			edge_render_poly_d(EID,Edge,F,WE,Order,FF,RTPR),
			pure_render_face_1(RTPR,gb_trees:get(RTPR,ES),F,WE,ST,Fin,Order,FF)
	end;
pure_render_face_1(EID,E,F,WE,ST,S,Or,FF)->
	% something is realy wrong
	throw("pure_render_face_1 fault!!!").


%%%
%%% render one vertex coord
%%%
pure_render_face_2(#edge{rf=F,rtsu=RTSU,rtpr=RTPR}=Edge,F,#we{es=ES}=WE,Order,FF)->
	edge_render_poly(Edge,F,WE,Order,FF);
pure_render_face_2(#edge{lf=F,ltsu=LTSU,ltpr=LTPR}=Edge,F,#we{es=ES}=WE,Order,FF)->
	edge_render_poly(Edge,F,WE,Order,FF);
pure_render_face_2(EID,F,#we{es=ES}=WE,Order,FF)->
	pure_render_face_2(gb_trees:get(EID,ES),F,WE,Order,FF).


%%%
%%% decision time, check for include, exclusion, double generation of vertexes 
%%%
edge_render_poly_d(EID,Edge,F,WE,Order,none,Next)->
	edge_render_poly(Edge,F,WE,Order,none);
edge_render_poly_d(EID,Edge,F,WE,Order,FF,NextEID)->
	case {FF(kind),FF({member,EID,F}),FF({member,NextEID,F})} of
		{xline,true,true} ->
				%% render line
				edge_render_poly(Edge,F,WE,Order,FF),
				pure_render_face_2(NextEID,F,WE,Order,FF);
		{iline,true,_} ->
				%% render line
				edge_render_poly(Edge,F,WE,Order,FF),
				pure_render_face_2(NextEID,F,WE,Order,FF);
		{iline,_,true} ->
				%% render line
				edge_render_poly(Edge,F,WE,Order,FF),
				pure_render_face_2(NextEID,F,WE,Order,FF);
		{vertex,true,_} ->
				%% normal vertex render (may include holes)
				edge_render_poly(Edge,F,WE,Order,FF)
				
	end.


%%
%%render edge part, may want to improve on this
%%  does not do hard edges properly ..... ?
%%
%%
%% Renders texcoordinates as destination posito instead of pos
%%
%%
%% (rendering back face if clokwise)
%%
%%
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,lf=Face},Face,#we{vp=VP}=WE,clockwise,FF)->
	% clockwise as calc from order of wings 
	%%io:format("Left face~n",[]),
	uvc(A),
	%gl:color3fv(B),
	%%gl:normal3fv(wpc_rspt_ve:normal(VE,Face,WE)),
	%%gl:vertex3fv(gb_trees:get(VE,VP)),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,rf=Face},Face,#we{vp=VP}=WE,clockwise,FF)->
	% clockwise as calc from order of wings 
	uvc(B),
	%%io:format("Right face~n",[]),
	%gl:color3fv(A),
	%%gl:normal3fv(wpc_rspt_ve:normal(VS,Face,WE)),
	%%gl:vertex3fv(gb_trees:get(VS,VP)),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,lf=FACE},Face,#we{vp=VP}=WE,counter_clock,FF)->
	% counter clockwise as calc from order of wings 
	uvc(B),
	%gl:color3fv(B),
	%%gl:normal3fv(wpc_rspt_ve:normal(VS,Face,WE)),
	%%gl:vertex3fv(gb_trees:get(VS,VP)),
	ok;
edge_render_poly(#edge{vs=VS,ve=VE,a=A,b=B,rf=FACE},Face,#we{vp=VP}=WE,counter_clock,FF)->
	% counter clockwise as calc from order of wings 
	uvc(A),
	%gl:color3fv(A),
	%%gl:normal3fv(wpc_rspt_ve:normal(VE,Face,WE)),
	%%gl:vertex3fv(gb_trees:get(VE,VP)),	
	ok.

%% should render vertex position
uvc({X,Y})->
	%%io:format("Rendering vertex with ~p ~p ~n",[X,Y]),
	gl:vertex3f(X,Y,0);
uvc(_)->
	%% assumed position
	gl:vertex3f(0,0,0).

%%%%
%%%%Need a surface normal for one surface for various misc reasons like mirrors ...
%%%%
%surface_normal(EID,Face,#we{es=ES}=WE)->
%	Vec = case gb_trees:get(EID,ES) of
%
%			#edge{rf=Face,rtsu=RTSU} ->
%					surface_normal_i(EID,RTSU,WE,right);
%			#edge{lf=Face,ltsu=LTSU} ->
%					surface_normal_i(EID,LTSU,WE,left)
%		end,
%	wpc_rspt_ve:normalize(Vec).



%
%Will use both edges and compute each normal and normalize
%
%
surface_normal(EID,#we{es=ES}=WE)->
	#edge{rtsu=RTSU,ltsu=LTSU}=CE = gb_trees:get(EID,ES),
	Avec  = surface_normal_i(EID,RTSU,WE,right),
	Bvec  = surface_normal_i(EID,LTSU,WE,left),
	Anorm = wpc_rspt_ve:normalize(Avec),
	Bnorm = wpc_rspt_ve:normalize(Bvec),
	wpc_rspt_ve:add(Anorm,Bnorm).


%%
%% Get surface normal of (St,WEID,FID)
%%
surface_normal(#st{shapes=WE}=St,WEID,FID)->
	#we{fs=Faces,es=Edges} = WEO = gb_trees:get(WEID,WE),
	EdgeID = gb_trees:get(FID,Faces),
	Edge   = gb_trees:get(EdgeID,Edges),
	Normal = wpc_rspt_ve:normalize(surface_normal_1(WEO,FID,EdgeID,Edge)),
	Normal.


%%
%%
%% Should this be ported into the rendering part ?? probably
%%

surface_normal_1(WE,FID,EdgeID,Edge)->
	surface_normal_2(0,WE,FID,EdgeID,Edge).
		
% can't decide direction suggest up
surface_normal_2(100,_,_,_,_)->

	%% disable concealing errors ..
	{0.0,0.0,0.0};
surface_normal_2(Calls,#we{es=Edges}=WE,FID,EdgeID,#edge{vs=VS,ve=VE,rf=FID,rtsu=SUCC})->
	Res = surface_normal_i(EdgeID,SUCC,WE,right),
	case wpc_rspt_ve:dot_product(Res,Res) < 0.01 of
		true ->
			%%io:format("Warning deranged cross product ~p ...!!~n",[SUCC]),
			surface_normal_2(Calls+1,WE,FID,SUCC,gb_trees:get(SUCC,Edges));
		false ->
			Res
	end;
surface_normal_2(Calls,#we{es=Edges}=WE,FID,EdgeID,#edge{vs=VS,ve=VE,lf=FID,ltsu=SUCC,ltpr=Next})->
	Res = surface_normal_i(EdgeID,SUCC,WE,left),
	case wpc_rspt_ve:dot_product(Res,Res) < 0.01 of
		true ->
			%%io:format("Warning deranged cross product ~p...!!~n",[Next]),
			surface_normal_2(Calls+1,WE,FID,Next,gb_trees:get(Next,Edges));
		false ->
			Res
	end.


%
%Produce vector normal i
%
surface_normal_i(FIRST,SUCC,#we{es=ES}=WE,left)->
	#edge{vs=VS ,ve=VE }  = E1 = gb_trees:get(FIRST,ES),
	#edge{vs=VS2,ve=VE2}  = E2 = gb_trees:get(SUCC,ES),
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
	throw({reduce_problem,recursion_astray}),
	[];
%Stop executing
grab_adjacent_edges(IncEdg,TarVer,IncEdg,EdgTre,Iter)->
	[];
grab_adjacent_edges(IncEdg,TarVer,CurEdg,EdgTre,Iter)->
	case gb_trees:get(CurEdg,EdgTre) of
		
		#edge{vs=TarVer,rtpr=Targ}=E ->
				[{CurEdg,E} | grab_adjacent_edges(IncEdg,TarVer,Targ,EdgTre,Iter-1)];
		#edge{ve=TarVer,ltpr=Targ}=E ->
				[{CurEdg,E} | grab_adjacent_edges(IncEdg,TarVer,Targ,EdgTre,Iter-1)]
	end.

grab_adjacent_edges_i(TarVer,#we{es=EdgTre,vc=VC}=WE)->
	Edge = gb_trees:get(TarVer,VC),
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% texture rendered
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pure_render_all_by_mat(#st{mat=Mat}=St,Scene,What,FF)->
	%%io:format("Mat trees to list ~p ~n",[gb_trees:to_list(Mat)]),
	lists:foldl(fun(X,Acc)->
			pure_render_scene_by_mat(Acc,X,St,Scene,What,FF),
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
%%
pure_render_scene_by_mat({MName,_}=Mat,#st{shapes=Sh}=St,Scene,What,FF)->
	pure_render_scene_by_mat(resolve_number(MName,St,Scene),Mat,#st{shapes=Sh}=St,Scene,What,FF).

pure_render_scene_by_mat(Num,{MName,_}=Mat,#st{shapes=Sh}=St,Scene,What,FF)->
	%% bind material params to surfaces
	%%io:format("Numerical material value ~p scene number is ~p ~n",[Num,Scene]),
	
	%% ignore for now ..

	%%gl:callList(Num),
	lists:foldl(fun({Name,#we{perm=P}=X},_)->
			%% should change slightly
			case {?IS_NOT_LIGHT(X),?IS_VISIBLE(P),?IS_SELECTABLE(P),What} of
				{true,true,true,locked}->
					%% do nothing ..
					ok;
				{true,true,true,_} ->
				
					io:format("Compiling object ~p ~n",[Name]),
					pure_render_obj_by_mat(Num,Mat,X,St,Scene,What,ff_setName(Name,FF));
				{true,true,false,locked}->
					%% assume line render
					pure_render_obj_by_mat(Num,Mat,X,St,Scene,line,ff_setName(Name,FF));
				A ->
					io:format("Ignoring ~p in compile ~p  with res ~p ~n",[Name,What,A]),
					ok
			end
		    end,ok,gb_trees:to_list(Sh)),
	ok.

%%
%% Create function from FF bound to specific object ID
%%
ff_setName(WID,none)->
	none;
ff_setName(WID,FF)->
	fun(X)->
		case X of
			
			{member,EID,F} ->
				FF({member,WID,EID,F});
			
			X0       ->
				FF(X0)
		end
	end.


pure_render_obj_by_mat(Num,{MName,_}=Mat,#we{mat=OMat}=WE,St,Scene,What,FF) when is_atom(OMat) ->
	case OMat of
		MName ->		
			%% switch to normal rendering (should have fun to affect rendering process somewhere, will change ...!!!)
			pure_render_obj_by_mat(WE,St,Scene,What,FF),  %% takes time to process normals
			ok;
		_ ->
			ok
	end,
	ok;
pure_render_obj_by_mat(Num,{MatName,Mat},#we{mat=OMat,fs=FS}=WE,St,Scene,What,FF)  ->
	[pure_render_face_by_mat(FaceN,gb_trees:get(FaceN,FS),WE,St,Scene,What,FF) || {FaceN,FaceMat} <- OMat, FaceMat =:= MatName ],
	ok.




pure_render_obj_by_mat(#we{fs=FS,mode=M,mat=Mat}=WE,#st{}=ST,Scene,What,FF)->
	lists:foldl(fun({F,X},Acc)->
			pure_render_face_by_mat(F,X,WE,ST,Scene,What,FF)
		    end,ok,gb_trees:to_list(FS)),
	ok.


			
pure_render_face_by_mat(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene,vertex,FF)->

	Edge = gb_trees:get(X,ES),
	gl:'begin'(?GL_POINTS),
	pure_render_face_1(X,Edge,F,WE,ST,start,clockwise,FF),
	gl:'end'(),
	ok;

pure_render_face_by_mat(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene,line,FF)->

	Edge = gb_trees:get(X,ES),
	gl:'begin'(?GL_LINE_LOOP),
	pure_render_face_1(X,Edge,F,WE,ST,start,clockwise,FF),
	gl:'end'(),
	ok;

pure_render_face_by_mat(F,X,#we{es=ES,vp=VP,name=Name}=WE,ST,Scene,face,FF)->

	Edge = gb_trees:get(X,ES),
	gl:'begin'(?GL_POLYGON),
	pure_render_face_1(X,Edge,F,WE,ST,start,clockwise,FF),
	gl:'end'(),
	ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% select one or more vertexes from view
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%
%input: St, {X0,X1,Y0,Y1}, 
%output: one vertex
%
select_vert(#st{shapes=Sh}=St,{X0,X1,Y0,Y1})->
	case select_vert_1(gb_trees:to_list(Sh),{X0,X1,Y0,Y1},single) of
		[] ->
			none;
		[X] ->
			X
	end.

%
%input: St, {X0,X1,Y0,Y1},
%output: list of vertexes
%
select_verts(#st{shapes=Sh}=St,{X0,X1,Y0,Y1})->
	select_vert_1(gb_trees:to_list(Sh),{X0,X1,Y0,Y1},all).



select_vert_1([],_,_)->
	[];
select_vert_1([El|Tail],Sq,Quest)->
	case {get_vert_shape(El,Sq,Quest),Quest} of

		{[],_} ->
			select_vert_1(Tail,Sq,Quest);

		{[R0],single}->
				[R0];
		{R1,_} ->
			R1 ++ select_vert_1(Tail,Sq,Quest)
	end.

get_vert_shape({Name,#we{es=ES,id=ID,perm=P}=WE},Sq,Q)->
	case {?IS_NOT_LIGHT(WE),?IS_VISIBLE(P),?IS_SELECTABLE(P)} of
		{true,true,true} ->
			%%io:format("Considering ~p ~n",[Name]),
			get_vert_shape_1(gb_trees:to_list(ES),WE,Sq,Q);
		_ ->
			[]
	end.

get_vert_shape_1([],_,_,_)->
	[];
get_vert_shape_1([{Name,#edge{a=A,b=B,lf=LF,rf=RF}=E} |Tail],#we{id=ID}=WE,Sq,Q)->
	KeyL = {ID,Name,LF},
	KeyR = {ID,Name,RF},
	case Q of
		single ->
			case consider_uvc(KeyL,A,Sq) of
				[] ->
					case consider_uvc(KeyR,B,Sq) of
						[] ->
							get_vert_shape_1(Tail,WE,Sq,Q);
						R1 ->
							R1
					end;
				R0 ->
					R0
			end;
		all ->
			consider_uvc(KeyL,A,Sq) ++ consider_uvc(KeyR,B,Sq) ++ get_vert_shape_1(Tail,WE,Sq,Q)
	end.

consider_uvc(Key,{X,Y},{X0,X1,Y0,Y1})->
	%%io:format("Consider {~p,~p}, {~p,~p,~p,~p}~n",[X,Y,X0,X1,Y0,Y1]),
	case { X >= X0, X =< X1, Y >= Y0, Y =< Y1} of
		{true,true,true,true} ->
			[Key];
		_ ->
			[]
	end;
%% if not textured
consider_uvc(Key,_,Sq)->
	consider_uvc(Key,{0,0},Sq).




%%%%%%%%%%%%%%%%%% select functions and inverts

select_all(St)->
	select_verts(St,{-10000000.0,100000000.0,-100000000.0,10000000000.0}).

select_invert(St,Sel)->
	A = select_all(St),
	ASet = gb_sets:from_list(A),
	SSet = gb_sets:from_list(Sel),
	gb_sets:to_list(gb_sets:difference(ASet,SSet)).


select_by_fun(St,Sel,FF)->
	A = select_all(St),
	SF = select_filter(FF,getData_selection(St,A),A),
	NSF = remove_bad_faces(St,SF),

	SFS = gb_sets:from_list(NSF),
	SSel = gb_sets:from_list(Sel),
	gb_sets:to_list(gb_sets:union(SFS,SSel)).


select_filter(FF,[],[])->
	[];
select_filter(FF,[El|Tail],[N|ITail])->
	case FF(El) of
		true  ->
			[N|select_filter(FF,Tail,ITail)];
		false ->
			select_filter(FF,Tail,ITail)
	end.

remove_bad_faces(St,Sel)->
	Uns = select_invert(St,Sel),
	FaceSet = select_faces(Uns),
	NSel = select_filter_face(Sel,FaceSet).

select_faces(List)->
	select_faces_1(List,gb_sets:empty()).

select_faces_1([],Set)->
	Set;
select_faces_1([{WEID,EID,FID}|Tail],Set)->
	select_faces_1(Tail,gb_sets:add({WEID,FID},Set)).

select_filter_face([],Set)->
	[];
select_filter_face([{WEID,EID,FID}|Tail],Set)->
	case gb_sets:is_member({WEID,FID},Set) of
		false ->
			[{WEID,EID,FID} | select_filter_face(Tail,Set)];
		true ->
			select_filter_face(Tail,Set)
	end.

%%
%%Take union of selection
%%
select_union(Sel,NSF)->
	SFS = gb_sets:from_list(NSF),
	SSel = gb_sets:from_list(Sel),
	gb_sets:to_list(gb_sets:union(SFS,SSel)).

%%%% select using ordering of uv

select_clockwise(St,Sel)->
	All = select_all(St),
	NSel = select_clockwise_1(St,All),
	select_union(Sel,NSel).

select_clockwise_1(St,[])->
	[];
select_clockwise_1(St,[{WEID,EID,FID} | Tail])->

	case is_clockwise(St,WEID,EID,FID) of
		true ->
			[{WEID,EID,FID} | select_clockwise_1(St,Tail)];
		false->
			select_clockwise_1(St,Tail)
	end.

is_clockwise(#st{shapes=WE}=St,WEID,EID,FID)->
	#we{es=Edges,vp=VP}=OBJ = gb_trees:get(WEID,WE),
	E0 = gb_trees:get(EID,Edges),
	{_,{U0,V0}} = getdata_selected_edge(E0,FID,VP),

	E1 = getnext_edge(E0,FID,Edges),

	{_,{U1,V1}} = getdata_selected_edge(E1,FID,VP),

	E2 = getnext_edge(E1,FID,Edges),
	{_,{U2,V2}} = getdata_selected_edge(E2,FID,VP),
	X1 = U0 - U1,
	X2 = V0 - V1,
	Y1 = U2 - U1,
	Y2 = V2 - V1,
	%% is this clock wise hmmm
	(X1 * Y2 - X2 * Y1) =< 0. 

getnext_edge(#edge{ltsu=NE,lf=LF},LF,Edges)->
	gb_trees:get(NE,Edges);
getnext_edge(#edge{rtsu=NE,rf=RF},RF,Edges)->
	gb_trees:get(NE,Edges).


select_selection(#st{sel=FSel,selmode=face}=St,Sel)->
	%% fetch all faces in FSel
	Faces = create_face_set(gb_sets:empty(),FSel),
	A     = select_all(St),
	NSel  = lists:filter(fun({WEID,EID,FID})->
				gb_sets:is_member({WEID,FID},Faces)
			     end,
			     A),
	select_union(Sel,NSel);
select_selection(St,Sel)->
	Sel.	
	

create_face_set(Crr,[])->
	Crr;
create_face_set(Crr,[{WEID,SET}|Tail])->
	NList = [{WEID,E} || E <- gb_sets:to_list(SET)],
	NCrr  = gb_sets:union(gb_sets:from_list(NList),Crr),
	create_face_set(NCrr,Tail).

select_normal(St,Sel,FF)->
	A = select_all(St),
	NSel = lists:filter(fun({WEID,EID,FID})->
				Normal = surface_normal(St,WEID,FID),
				FF(Normal)
			    end,
			    A),
	select_union(Sel,NSel).


select_neighbours(St,Sel)->
	A = select_all(St),
	Faces = create_face_selection(gb_sets:empty(),Sel),
	NSel  = lists:filter(fun({WEID,EID,FID})->
				gb_sets:is_member({WEID,FID},Faces)
			     end,
			     A),
	NSel. %% super set of Sel

create_face_selection(Set,[])->
	Set;
create_face_selection(Set,[{WEID,EID,FID}|Tail])->
	create_face_selection(gb_sets:add({WEID,FID},Set),Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% render selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


render_selection(St,L)->
	gl:'begin'(?GL_POINTS),
	render_selection_1(St,L),
	gl:'end'(),
	ok.

render_selection_1(#st{},[])->
	ok;
render_selection_1(#st{shapes=WE}=St,[{WEID,EID,FID}|Tail])->
	%%io:format("Fetching object in ~p ~n",[WE]),
	#we{es=Edges}=OBJ = gb_trees:get(WEID,WE),
	%%io:format("Fetching edge in object ~n",[]),
	E = gb_trees:get(EID,Edges),
	render_selected_edge(E,FID),
	render_selection_1(St,Tail).

render_selected_edge(#edge{a={X,Y},lf=LF},LF)->
	gl:vertex3f(X,Y,0);
render_selected_edge(#edge{b={X,Y},rf=RF},RF)->
	gl:vertex3f(X,Y,0);
render_selected_edge(_,_)->
	gl:vertex3f(0,0,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% get data from selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getData_selection(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	getData_selection(St,Sel).

getData_selection(St,[])->
	[];
getData_selection(#st{shapes=WE}=St,[{WEID,EID,FID}|Tail] )->
	#we{es=Edges,vp=VP}=OBJ = gb_trees:get(WEID,WE),
	E = gb_trees:get(EID,Edges),
	{Pos,Tex} = getdata_selected_edge(E,FID,VP), 	
	[{Pos,Tex}|getData_selection(St,Tail)].

getdata_selected_edge(#edge{ve=VE,vs=VS,a={U,V},lf=LF}=E,LF,VP)->
	Pos = gb_trees:get(VS,VP),
	{Pos,{U,V}};
getdata_selected_edge(#edge{vs=VS,ve=VE,b={U,V},rf=RF}=E,RF,VP)->
	Pos = gb_trees:get(VE,VP),
	{Pos,{U,V}};
getdata_selected_edge(#edge{ve=VE,vs=VS,lf=LF}=E,LF,VP)->
	Pos = gb_trees:get(VS,VP),
	{Pos,{0,0}};
getdata_selected_edge(#edge{vs=VS,ve=VE,rf=RF}=E,RF,VP)->
	Pos = gb_trees:get(VE,VP),
	{Pos,{0,0}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% set data of selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setData_selection(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	wpc_rspt_wm:set_current_state(setData_selection(St,Sel)),
	ok.

setData_selection(St,[])->
	St;
setData_selection(#st{shapes=WE}=St,[{{WEID,EID,FID},Data} | SelData])->
	OBJ = #we{es=Edges,vp=VP} = gb_trees:get(WEID,WE),
	E = gb_trees:get(EID,Edges),
	NE = setData_selected_edge(E,FID,Data),	
	NEdges = gb_trees:update(EID,NE,Edges),
	NOBJ = OBJ#we{es=NEdges,mode=uv},
	NWE    = gb_trees:update(WEID,NOBJ,WE),
	setData_selection(St#st{shapes=NWE},SelData).
	
setData_selected_edge(#edge{ve=VE,a=UU,lf=LF}=E,LF,{U,V})->
	E#edge{a={U,V}};
setData_selected_edge(#edge{vs=VS,b=UU,rf=RF}=E,RF,{U,V})->
	E#edge{b={U,V}}.



%%%%%%
%%%%%% Create selection group function modifier
%%%%%%
%%%%%%
ff_modifier(Kind,Set,Inverse)->
	fun(X)->
		case {X,Inverse} of
			{{member,WID,EID,F} , false} ->
					gb_sets:is_member({WID,EID,F},Set);
			{{member,WID,EID,F} , true} ->
					gb_sets:is_member({WID,EID,F},Set) == false;
			{kind,_} ->
					Kind
		end
	end.

ff_all(St,Scene,What,Kind,Set,Inverse)->
	all(St,Scene,What,ff_modifier(Kind,Set,Inverse)). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% uv map functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% (spherical)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sphere_map(Sel)->
	Data = getData_selection(Sel),
	%% not needed by this function
	%%{SX,SY,SZ,LX,LY,LZ} = Bounds = calc_bounds(Data),
	SData = sphere_map_calc(Sel,Data),
	%%io:format("Set data is ~p ~n",[SData]),
	setData_selection(SData).

sphere_map_calc([],[])->
	[];
sphere_map_calc([El|Tail],[{{X,Y,Z},_}|ITail])->
	[{El,{math:atan2(Z,X)/(2.0*math:pi())+0.5,math:atan2(Y,X)/(2.0*math:pi())+0.5}}|sphere_map_calc(Tail,ITail)].	


ycylinder_map(Sel)->
	Data = getData_selection(Sel),
	{SX,SY,SZ,LX,LY,LZ} = calc_bounds(Data),
	%%io:format("SY=~p and LY=~p ~n",[SY,LY]),
	SData = ycylinder_map_calc(Sel,Data,SY,LY),
	setData_selection(SData).

ycylinder_map_calc([],[],SY,LY)->
	[];
ycylinder_map_calc([El|Tail],[{{X,Y,Z},_}|ITail],SY,LY)->
	[{El,{math:atan2(Z,X)/(2.0*math:pi())+0.5,(Y-SY)/(LY-SY)}} | ycylinder_map_calc(Tail,ITail,SY,LY)].

xcylinder_map(Sel)->
	Data = getData_selection(Sel),
	{SX,SY,SZ,LX,LY,LZ} = calc_bounds(Data),
	%%io:format("SY=~p and LY=~p ~n",[SY,LY]),
	SData = xcylinder_map_calc(Sel,Data,SX,LX),
	setData_selection(SData).

xcylinder_map_calc([],[],SX,LX)->
	[];
xcylinder_map_calc([El|Tail],[{{X,Y,Z},_}|ITail],SX,LX)->
	[{El,{math:atan2(Z,Y)/(2.0*math:pi())+0.5,(X-SX)/(LX-SX)}} | xcylinder_map_calc(Tail,ITail,SX,LX)].

zcylinder_map(Sel)->
	Data = getData_selection(Sel),
	{SX,SY,SZ,LX,LY,LZ} = calc_bounds(Data),
	%%io:format("SY=~p and LY=~p ~n",[SY,LY]),
	SData = zcylinder_map_calc(Sel,Data,SZ,LZ),
	setData_selection(SData).

zcylinder_map_calc([],[],SZ,LZ)->
	[];
zcylinder_map_calc([El|Tail],[{{X,Y,Z},_}|ITail],SZ,LZ)->
	[{El,{math:atan2(X,Y)/(2.0*math:pi())+0.5,(Z-SZ)/(LZ-SZ)}} | zcylinder_map_calc(Tail,ITail,SZ,LZ)].



iterator_funmap(Sel,F)->
	Data = getData_selection(Sel),
	Limits = calc_bounds(Data),
	SData = iterator_map_calc(Sel,Data,Limits,F),
	setData_selection(SData).

iterator_map_calc([],[],_,_)->
	[];
iterator_map_calc([El|Tail],[{{X,Y,Z},_}|ITail],Limits,F)->
	[{El,F(Limits,X,Y,Z)} | iterator_map_calc(Tail,ITail,Limits,F)].


planar_xz(Sel)->
	iterator_funmap(Sel,fun({SX,SY,SZ,LX,LY,LZ},X,Y,Z)->
					{(X-SX)/(LX-SX),(Z-SZ)/(LZ-SZ)}
				end).

planar_yz(Sel)->
	iterator_funmap(Sel,fun({SX,SY,SZ,LX,LY,LZ},X,Y,Z)->
					{(Y-SY)/(LY-SY),(Z-SZ)/(LZ-SZ)}
				end).

planar_xy(Sel)->
	iterator_funmap(Sel,fun({SX,SY,SZ,LX,LY,LZ},X,Y,Z)->
					{(X-SX)/(LX-SX),(Y-SY)/(LY-SY)}
				end).

flip_u(Sel)->
	iterator_funmapUV(Sel,fun({CU,CV},U,V)->
					{CU-(U-CU),V}
				end).

flip_v(Sel)->
	iterator_funmapUV(Sel,fun({CU,CV},U,V)->
					{U,CV-(V-CV)}
				end).

join(Sel)->
	iterator_funmapUV(Sel,fun({CU,CV},U,V)->
					{CU,CV}
				end).


iterator_funmapUV(Sel,F)->
	Data = getData_selection(Sel),
	Center = calc_centerUV(Data),
	SData = iterator_map_calcUV(Sel,Data,Center,F),
	setData_selection(SData).

iterator_map_calcUV([],[],_,_)->
	[];
iterator_map_calcUV([El|Tail],[{_,{U,V}}|ITail],Center,F)->
	[{El,F(Center,U,V)} | iterator_map_calcUV(Tail,ITail,Center,F)].

calc_centerUV(Data)->
	{SU,SV,LU,LV} = calc_boundsUV(Data),
	{(SU+LU)/2,(SV+LV)/2}.

%%% calculate bounds
calc_boundsUV(Data)->
	calc_boundsUV_1(10000000.0,1000000000.0,-10000000000.0,-1000000000.0,Data).


%%% calculate bounds
calc_bounds(Data)->
	calc_bounds_1(10000000.0,1000000000.0,100000000000.0,-1000000000000.0,-10000000000.0,-1000000000.0,Data).


min(X,Y)->
	case X < Y of
		true ->
			X;
		false ->
			Y
	end.

max(X,Y)->
	case X > Y of
		true ->
			X;
		false ->
			Y
	end.

calc_bounds_1(SX,SY,SZ,LX,LY,LZ,[])->
	{SX,SY,SZ,LX,LY,LZ};
calc_bounds_1(SX,SY,SZ,LX,LY,LZ,[{{X,Y,Z},_}| ITail])->
	calc_bounds_1(min(SX,X),min(SY,Y),min(SZ,Z),max(LX,X),max(LY,Y),max(LZ,Z),ITail).


calc_boundsUV_1(SU,SV,LU,LV,[])->
	{SU,SV,LU,LV};
calc_boundsUV_1(SU,SV,LU,LV,[{_,{U,V}}| ITail])->
	calc_boundsUV_1(min(SU,U),min(SV,V),max(LU,U),max(LV,V),ITail).


%%%%%%%% cube map of texture

cube_map(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	map_up(Sel),
	map_down(Sel),
	map_west(Sel),
	map_east(Sel),
	map_south(Sel),
	map_north(Sel),
	ok.


%%
%%The clockwise detect is wrong here ... botches parallel points
%%
-define(HALF_DET,0.45).

map_up(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						 -Y >=? HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_xz(NSel),
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{ -0.25*(V-CV)+CV-0.125 , 0.33 - 0.33*(U-CU)+CU}
				end).

map_down(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						 Y >= ?HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_xz(NSel),
	%iterator_funmapUV(NSel,fun({CU,CV},U,V)->
	%				{ -0.25*(U-CU)+CU-0.125 , -0.33 - 0.33*(V-CV)+CV}
	%			end).
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{ -0.25*(V-CV)+CV-0.125, -0.33 + 0.33*(U-CU)+CU}
				end).


	
map_south(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						 Z >= ?HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_xy(NSel),
	%iterator_funmapUV(NSel,fun({CU,CV},U,V)->
	%				{ 0.25*(U-CU)+CU -0.125+0.25, 0.33*(V-CV)+CV}
	%			end).
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{  -0.25*(U-CU)+CU -0.125+0.25, 0.33*(V-CV)+CV}
				end).
		


map_west(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						-X >= ?HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_yz(NSel),
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{ -0.25*(V-CV)+CV-0.125,0.33*(U-CU)+CU}	
				end).

map_east(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						X >= ?HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_yz(NSel),
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{ 0.25*(V-CV)+CV-0.125+0.5 , 0.33*(U-CU)+CU}
				end).

map_north(Sel)->
	St = wpc_rspt_wm:get_current_state(),
	NSel = select_normal_from_sel(St,Sel,fun({X,Y,Z})->
						-Z >= ?HALF_DET
					     end),
	%%io:format("Map up is ~p ~n",[NSel]),
	planar_xy(NSel),
	iterator_funmapUV(NSel,fun({CU,CV},U,V)->
					{ 0.25*(U-CU)+CU-0.125-0.25 , 0.33*(V-CV)+CV}
				end).


select_normal_from_sel(St,Sel,FF)->
	NSel = lists:filter(fun({WEID,EID,FID})->
				Normal = surface_normal(St,WEID,FID),
				FF(Normal)
			    end,
			    Sel),
	NSel.

	
%%
%% Create all register funs
%%
popup_menu_register()->
	[{rspt_twc_planar_xz,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> planar_xz(X) end) end},
         {rspt_twc_planar_yz,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> planar_yz(X) end) end},
	 {rspt_twc_planar_xy,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> planar_xy(X) end) end},
	 {rspt_twc_flip_u,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> flip_u(X) end) end},
	 {rspt_twc_flip_v,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> flip_v(X) end) end},
	 {rspt_twc_join,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> join(X) end) end},
	 {rspt_twc_cube_map,fun(Sel)-> X=Sel, wpc_rspt_twm:export(fun()-> cube_map(X) end) end}
	].

popup_menu_entries(Menu)->
	Menu ++ 
	[{"Planar map XZ",rspt_twc_planar_xz,"Plane map selected UV points"},
	{"Planar map YZ",rspt_twc_planar_yz,"Plane map selected UV points"},
	{"Planar map XY",rspt_twc_planar_xy,"Plane map selected UV points"},
	{"Flip u",rspt_twc_flip_u,"Flip UV points in U"},
	{"Flip v",rspt_twc_flip_v,"Flip UV points in V"},
	{"Join point",rspt_twc_join,"Join uv points"},
	{"Cube map",rspt_twc_cube_map,"Cube map uv points"}
	].
