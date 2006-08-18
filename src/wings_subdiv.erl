%%
%%  wings_subdiv.erl --
%%
%%     This module implements the Smooth command for objects and faces.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_subdiv.erl,v 1.90 2005/01/15 09:49:07 bjorng Exp $
%%

-module(wings_subdiv).
-export([smooth/1,smooth/5,inc_smooth/2]).

-include("wings.hrl").

-import(lists, [map/2,foldl/3,reverse/1,reverse/2,sort/1,merge/1,foreach/2]).

%%% The Catmull-Clark subdivision algorithm is used, with
%%% Tony DeRose's extensions for creases.

smooth(We) ->
    {Faces,Htab} = smooth_faces_htab(We),
    smooth(Faces, Htab, We).
    
smooth(Fs, Htab, #we{vp=Vtab,es=Etab}=We) ->
    Vs = gb_trees:keys(Vtab),
    Es = gb_trees:keys(Etab),
    smooth(Fs, Vs, Es, Htab, We).

smooth(Fs, Vs, Es, Htab, #we{vp=Vp,next_id=Id}=We0) ->
    wings_pb:start(?__(1,"smoothing")),
    wings_pb:update(0.05, ?__(2,"calculating face centers")),
    FacePos0 = face_centers(Fs, We0),

    %% First do all topological changes to the edge table.
    wings_pb:update(0.20, ?__(3,"cutting edges")),
    We1 = cut_edges(Es, Htab, We0#we{vc=undefined}),
    wings_pb:update(0.25, ?__(4,"updating materials")),
    We2 = smooth_materials(Fs, FacePos0, We1),
    wings_pb:update(0.47, ?__(5,"creating new faces")),
    {We3,Hide} = smooth_faces(FacePos0, Id, We2),
    wings_pb:update(0.60, ?__(6,"moving vertices")),

    %% Now calculate all vertex positions.
    FacePos = gb_trees:from_orddict(FacePos0),
    {UpdatedVs,Mid} = update_edge_vs(Es, We0, FacePos, Htab, Vp, Id),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(Vs, FacePos, Htab, We0, UpdatedVs ++ NewVs),

    %% Done, except that we'll need to re-hide any hidden faces
    %% and rebuild tables.
    wings_pb:update(1.0, ?__(7,"finishing")),
    We4 = We3#we{vp=Vtab},
    We = if
	     Hide =:= [] ->
		 wings_we:rebuild(We4);
	     true ->
		 wings_we:hide_faces(Hide, We4) %Will force a rebuild.
	 end,
    wings_pb:done(We).

inc_smooth(#we{vp=Vp,next_id=Next}=We0, OldWe) ->
    {Faces,Htab} = smooth_faces_htab(We0),
    FacePos0 = face_centers(Faces, We0),
    FacePos = gb_trees:from_orddict(FacePos0),
    {UpdatedVs,Mid} = update_edge_vs(We0, FacePos, Htab, Vp, Next),
    NewVs = smooth_new_vs(FacePos0, Mid),
    Vtab = smooth_move_orig(gb_trees:keys(Vp), FacePos, Htab, We0,
			    UpdatedVs ++ NewVs),
    OldWe#we{vp=Vtab}.

smooth_faces_htab(#we{mirror=none,fs=Ftab,he=Htab}) ->
    Faces = gb_trees:keys(Ftab),
    {Faces,Htab};
smooth_faces_htab(#we{mirror=Face,fs=Ftab,he=Htab}=We) ->
    Faces = gb_trees:keys(gb_trees:delete(Face, Ftab)),
    He0 = wings_face:to_edges([Face], We),
    He = gb_sets:union(gb_sets:from_list(He0), Htab),
    {Faces,He}.

%%%
%%% Calculation of face centers.
%%%

face_centers(Faces, We) ->
    face_centers(Faces, We, []).

face_centers([Face|Fs], We, Acc) ->
    {Vs,Cols} = wings_face:fold(
		  fun(V, _, #edge{ve=V,a=C}, {Vs0,Col0}) ->
			  {[V|Vs0],[C|Col0]};
		     (V, _, #edge{vs=V,b=C}, {Vs0,Col0}) ->
			  {[V|Vs0],[C|Col0]}
		  end, {[],[]}, Face, We),
    case Vs of
	[_,_] ->
	    wings_u:error(?__(1,"Face ") ++ integer_to_list(Face) ++
			  ?__(2," has only two edges."));
	_ ->
	    Center0 = wings_vertex:center(Vs, We),
	    Center = wings_util:share(Center0),
	    Col = wings_color:average(Cols),
	    face_centers(Fs, We, [{Face,{Center,Col,length(Vs)}}|Acc])
    end;
face_centers([], _We, Acc) -> reverse(Acc).

%%%
%%% Updating of the topology (edge and hard edge tables).
%%%

cut_edges(Es, Hard, #we{he=Htab0,next_id=Id0}=We) ->
    Etab0 = prepare_etab(Es, We),
    {Id,Etab,Htab} = cut_edges_1(Es, Hard, Id0, Etab0, Htab0),
    We#we{es=Etab,he=Htab,next_id=Id}.

prepare_etab(Es, #we{es=Etab0,next_id=Id}) ->
    Etab = prepare_etab_1(Id+length(Es)-1, Id, []),
    gb_trees:from_orddict(gb_trees:to_list(Etab0) ++ Etab).

prepare_etab_1(Id, Lim, Acc) when Id >= Lim ->
    prepare_etab_1(Id-1, Lim, [{Id,dummy}|Acc]);
prepare_etab_1(_, _, Acc) -> Acc.

cut_edges_1([Edge|Es], Hard, NewEdge, Etab0, Htab0) ->
    Rec = gb_trees:get(Edge, Etab0),
    Etab = fast_cut(Edge, Rec, NewEdge, Etab0),
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    Htab = case gb_sets:is_member(Edge, Htab0) of
		       true -> gb_sets:insert(NewEdge, Htab0);
		       false -> Htab0
		   end,
	    cut_edges_1(Es, Hard, NewEdge+1, Etab, Htab);
	false ->
	    cut_edges_1(Es, Hard, NewEdge+1, Etab, Htab0)
    end;
cut_edges_1([], _Hard, Id, Etab, Htab) ->
    {Id,Etab,Htab}.

fast_cut(Edge, Template, NewV=NewEdge, Etab0) ->
    #edge{a=ACol,b=BCol,lf=Lf,rf=Rf,
	  ltpr=EdgeA,rtsu=EdgeB,rtpr=NextBCol} = Template,
    NewColA = mix_color(EdgeA, Etab0, Lf, ACol),
    NewColB = mix_color(NextBCol, Etab0, Rf, BCol),
    NewEdgeRec = Template#edge{vs=NewV,a=NewColA,ltsu=Edge,rtpr=Edge},
    Etab1 = gb_trees:update(NewEdge, NewEdgeRec, Etab0),
    EdgeRec = Template#edge{ve=NewV,b=NewColB,rtsu=NewEdge,ltpr=NewEdge},
    Etab2 = gb_trees:update(Edge, EdgeRec, Etab1),
    Etab = wings_edge:patch_edge(EdgeA, NewEdge, Edge, Etab2),
    wings_edge:patch_edge(EdgeB, NewEdge, Edge, Etab).

mix_color(_, _, _, none) -> none;
mix_color(E, Etab, Face, OtherColor) ->
    wings_color:average(OtherColor,
			case gb_trees:get(E, Etab) of
			    #edge{lf=Face,a=Col} -> Col;
			    #edge{rf=Face,b=Col} -> Col
			end).

smooth_faces(FacePos, Id, We0) ->
    We = smooth_faces_1(FacePos, Id, [], We0),
    case wings_we:any_hidden(We0) of
	false -> {We,[]};
	true -> {We,smooth_faces_hide(FacePos, We0)}
    end.

smooth_faces_1([{Face,{_,Color,NumIds}}|Fs], Id, EsAcc0, #we{es=Etab0}=We0) ->
    {Ids,We} = wings_we:new_wrap_range(NumIds, 1, We0),
    NewV = wings_we:id(0, Ids),
    Fun = smooth_edge_fun(Face, NewV, Color, Id),
    {Etab,EsAcc,_} = face_fold(Fun, {Etab0,EsAcc0,Ids}, Face, We),
    smooth_faces_1(Fs, Id, EsAcc, We#we{es=Etab});
smooth_faces_1([], _, Es, #we{es=Etab0}=We) ->
    Etab1 = gb_trees:to_list(Etab0) ++ reverse(Es),
    Etab = gb_trees:from_orddict(Etab1),
    We#we{es=Etab,fs=undefined}.

smooth_edge_fun(Face, NewV, Color, Id) ->
    fun(Edge, Rec0, Next, {Etab0,Es0,Ids0}) ->
	    LeftEdge = RFace = wings_we:id(0, Ids0),
	    NewEdge = LFace = wings_we:id(1, Ids0),
	    RightEdge = wings_we:id(2, Ids0),
	    case Rec0 of
		#edge{ve=Vtx,b=OldCol,rf=Face} when Vtx >= Id ->
		    Ids = Ids0,
		    Rec = Rec0#edge{rf=RFace,rtsu=NewEdge},
		    NewErec = #edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge,
				    ltpr=RightEdge,ltsu=Next},
		    Es = store(NewEdge, NewErec, Es0);
		#edge{vs=Vtx,a=OldCol,lf=Face} when Vtx >= Id ->
		    Ids = Ids0,
		    Rec = Rec0#edge{lf=RFace,ltsu=NewEdge},
		    NewErec = #edge{vs=Vtx,a=OldCol,ve=NewV,b=Color,
				    rf=RFace,lf=LFace,
				    rtpr=Edge,rtsu=LeftEdge,
				    ltpr=RightEdge,ltsu=Next},
		    Es = store(NewEdge, NewErec, Es0);
		#edge{vs=Vtx,rf=Face} when Vtx >= Id ->
		    Rec = Rec0#edge{rf=LFace,rtpr=NewEdge},
		    Es = Es0,
		    Ids = wings_we:bump_id(Ids0);
		#edge{ve=Vtx,lf=Face} when Vtx >= Id ->
		    Rec = Rec0#edge{lf=LFace,ltpr=NewEdge},
		    Es = Es0,
		    Ids = wings_we:bump_id(Ids0)
	    end,
	    Etab = gb_trees:update(Edge, Rec, Etab0),
	    {Etab,Es,Ids}
    end.

smooth_faces_hide(Fs, #we{next_id=Id}) ->
    smooth_faces_hide_1(Fs, Id, []).

smooth_faces_hide_1([{Face,{_,_,NumIds}}|Fs], Id, Acc) when Face >= 0 ->
    smooth_faces_hide_1(Fs, Id+NumIds, Acc);
smooth_faces_hide_1([{_,{_,_,NumIds}}|Fs], Id, Acc0) ->
    Acc = smooth_faces_hide_2(NumIds, Id, Acc0),
    smooth_faces_hide_1(Fs, Id+NumIds, Acc);
smooth_faces_hide_1([], _, Acc) -> Acc.

smooth_faces_hide_2(0, _, Acc) -> Acc;
smooth_faces_hide_2(N, Id, Acc) -> smooth_faces_hide_2(N-1, Id+1, [Id|Acc]).

%% Store in reverse order.
store(Key, New, [{K,_Old}|_]=Dict) when Key > K ->
    [{Key,New}|Dict];
store(Key, New, [{K,_Old}=E|Dict]) when Key < K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{_K,_Old}|Dict]) ->		%Key == K
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].

face_fold(F, Acc, Face, #we{es=Etab,fs=Ftab}) ->
    Edge = gb_trees:get(Face, Ftab),
    face_fold(Edge, Etab, F, Acc, Face, Edge, not_done).

face_fold(LastEdge, _, _, Acc, _, LastEdge, done) -> Acc;
face_fold(Edge, Etab, F, Acc0, Face, LastEdge, _) ->
    case gb_trees:get(Edge, Etab) of
	#edge{lf=Face,ltsu=NextEdge}=E ->
	    Acc = F(Edge, E, NextEdge, Acc0),
	    face_fold(NextEdge, Etab, F, Acc, Face, LastEdge, done);
	#edge{rf=Face,rtsu=NextEdge}=E ->
	    Acc = F(Edge, E, NextEdge, Acc0),
	    face_fold(NextEdge, Etab, F, Acc, Face, LastEdge, done)
    end.

%%
%% XXX This is ugly. Here the materials are directly manpulated.
%%
smooth_materials(_, _, #we{mat=Mat}=We) when is_atom(Mat) -> We;
smooth_materials(Fs, FacePos, #we{fs=Ftab,mat=Mat0}=We) ->
    case length(Fs) =:= gb_trees:size(Ftab) of
	true ->				  %We are smoothing all faces.
	    smooth_materials_1(Mat0, FacePos, We, []);
	false ->		 %Must pick up the faces not smoothed.
	    Mat1 = sofs:from_external(Mat0, [{face,mat}]),
	    Changed = sofs:from_external(Fs, [face]),
	    {Mat2,Keep0} = sofs:partition(1, Mat1, Changed),
	    Mat = sofs:to_external(Mat2),
	    Keep = sofs:to_external(Keep0),
	    smooth_materials_1(Mat, FacePos, We, Keep)
    end.

smooth_materials_1(Fmat, Fpos, #we{next_id=Id}=We, Keep) ->
    Mat = smooth_materials_2(Fmat, Fpos, Id, Keep),
    We#we{mat=sort(Mat)}.

smooth_materials_2([{F,Mat}|Fs], [{F,{_,_,N}}|Fpos], Face, Acc0) ->
    NextFace = Face+N,
    Acc = smooth_materials_3(Mat, NextFace, Face, Acc0),
    smooth_materials_2(Fs, Fpos, NextFace, Acc);
smooth_materials_2([], [], _, Acc) -> Acc.

smooth_materials_3(_, Face, Face, Acc) -> Acc;
smooth_materials_3(Mat, NextFace, Face, Acc) ->
    smooth_materials_3(Mat, NextFace, Face+1, [{Face,Mat}|Acc]).

%%%
%%% Moving of vertices.
%%%

smooth_move_orig(Vs, FacePos, Htab, #we{vp=Vtab}=We, VtabTail) ->
    MoveFun = smooth_move_orig_fun(Vtab, FacePos, Htab),
    RevVtab = case gb_trees:size(Vtab) of
		  N when N =:= length(Vs) ->
		      smooth_move_orig_all(gb_trees:to_list(Vtab), MoveFun, We, []);
		  _ ->
		      smooth_move_orig_some(Vs, gb_trees:to_list(Vtab), MoveFun, We, [])
	      end,
    gb_trees:from_orddict(reverse(RevVtab, VtabTail)).

smooth_move_orig_all([{V,Pos0}|Vs], MoveFun, We, Acc) ->
    Pos = smooth_move_orig_1(V, Pos0, MoveFun, We),
    smooth_move_orig_all(Vs, MoveFun, We, [{V,Pos}|Acc]);
smooth_move_orig_all([], _FacePos, _MoveFun, Acc) -> Acc.

smooth_move_orig_some([V|Vs], [{V,Pos0}|Vs2], MoveFun, We, Acc) ->
    Pos = smooth_move_orig_1(V, Pos0, MoveFun, We),
    smooth_move_orig_some(Vs, Vs2, MoveFun, We, [{V,Pos}|Acc]);
smooth_move_orig_some(Vs, [Pair|Vs2], MoveFun, We, Acc) ->
    smooth_move_orig_some(Vs, Vs2, MoveFun, We, [Pair|Acc]);
smooth_move_orig_some([], [], _, _, Acc) -> Acc;
smooth_move_orig_some([], Vs2, _, _, Acc) -> reverse(Vs2, Acc).

smooth_move_orig_1(V, S, MoveFun, We) ->
    {_,Ps0,Hard} = wings_vertex:fold(MoveFun, {V,[],[]}, V, We),
    case length(Hard) of
	NumHard when NumHard < 2 ->
	    Ps = e3d_vec:add(Ps0),
	    {A,B} = case length(Ps0) of
			2*3 -> {1/9,1/3};
			2*4 -> {1/16,2/4};
			2*5 -> {1/25,3/5};
			N0 -> 
			    N = N0 bsr 1,
			    {1.0/(N*N),(N-2.0)/N}
		    end,
	    Pos = e3d_vec:add_prod(e3d_vec:mul(Ps, A), S, B),
	    wings_util:share(Pos);
	NumHard when NumHard =:= 2 ->
	    Pos0 = e3d_vec:add([e3d_vec:mul(S, 6.0)|Hard]),
	    Pos = e3d_vec:mul(Pos0, 1/8),
	    wings_util:share(Pos);
	_ThreeOrMore -> S
    end.

smooth_move_orig_fun(Vtab, FacePos, Htab) ->
    case gb_sets:is_empty(Htab) of
	true ->
	    %% No hard edges imply that all faces can be found
	    %% in the FacePos table. Therefore gb_trees:get/2 is safe.
	    fun(_Edge, Face, Erec, {V,Ps,_}) ->
		    OPos = wings_vertex:other_pos(V, Erec, Vtab),
		    {FPos,_,_} = gb_trees:get(Face, FacePos),
		    {V,[OPos,FPos|Ps],[]}
	    end;
	false ->
	    fun(Edge, Face, Erec, {V,Ps0,Hard0}) ->
		    OPos = wings_vertex:other_pos(V, Erec, Vtab),
		    FPos = case gb_trees:lookup(Face, FacePos) of
			       none -> none;
			       {value,{Fp,_,_}} -> Fp
			   end,
		    Es = case gb_sets:is_member(Edge, Htab) of
			     true -> [OPos|Hard0];
			     false -> Hard0
			 end,
		    Ps = [FPos,OPos|Ps0],
		    {V,Ps,Es}
	    end
    end.

%% Update the position for the vertex that was created in the middle
%% of each original edge.
update_edge_vs(#we{es=Etab}, FacePos, Hard, Vtab, V) ->
    update_edge_vs_all(gb_trees:to_list(Etab), FacePos, Hard, Vtab, V, []).

update_edge_vs(Es, #we{es=Etab}, FacePos, Hard, Vtab, V) ->
    case gb_trees:size(Etab) of
	N when N =:= length(Es) ->
	    update_edge_vs_all(gb_trees:to_list(Etab), FacePos, Hard, Vtab, V, []);
	_ ->
	    update_edge_vs_some(Es, Etab, FacePos, Hard, Vtab, V, [])
    end.

update_edge_vs_all([{Edge,Rec}|Es], FacePos, Hard, Vtab, V, Acc) ->
    Pos = update_edge_vs_1(Edge, Hard, Rec, FacePos, Vtab),
    update_edge_vs_all(Es, FacePos, Hard, Vtab, V+1, [{V,Pos}|Acc]);
update_edge_vs_all([], _, _, _, V, Acc) ->
    {reverse(Acc),V}.

update_edge_vs_some([E|Es], Etab, FacePos, Hard, Vtab, V, Acc) ->
    Rec = gb_trees:get(E, Etab),
    Pos = update_edge_vs_1(E, Hard, Rec, FacePos, Vtab),
    update_edge_vs_some(Es, Etab, FacePos, Hard, Vtab, V+1, [{V,Pos}|Acc]);
update_edge_vs_some([], _, _, _, _, V, Acc) ->
    {reverse(Acc),V}.

update_edge_vs_1(Edge, Hard, Rec, FacePos, Vtab) ->
    case gb_sets:is_member(Edge, Hard) of
	true ->
	    #edge{vs=Va,ve=Vb} = Rec,
	    e3d_vec:average(gb_trees:get(Va, Vtab), gb_trees:get(Vb, Vtab));
	false ->
	    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = Rec,
	    {LfPos,_,_} = gb_trees:get(Lf, FacePos),
	    {RfPos,_,_} = gb_trees:get(Rf, FacePos),
	    Pos0 = e3d_vec:average(gb_trees:get(Va, Vtab),
				   gb_trees:get(Vb, Vtab),
				   LfPos, RfPos),
	    wings_util:share(Pos0)
    end.

smooth_new_vs(FacePos, V) ->
    smooth_new_vs(FacePos, V, []).
    
smooth_new_vs([{_,{Center,_,NumIds}}|Fs], V, Acc) ->
    smooth_new_vs(Fs, V+NumIds, [{V,Center}|Acc]);
smooth_new_vs([], _, Acc) -> reverse(Acc).
