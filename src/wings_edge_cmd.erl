%%
%%  wings_edge.erl --
%%
%%     This module contains most edge command and edge utility functions.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_edge_cmd).

%% Commands.
-export([menu/3,command/2]).
-export([hardness/2,set_color/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,mapfoldl/3,reverse/1,sort/1]).
-import(e3d_vec, [add/2,sub/2,neg/1,norm/1,len/1,
		  average/2, dot/2,cross/2]).

menu(X, Y, St) ->
    Dir = wings_menu_util:directions(St),
    Menu = [{?__(2,"Move"),{move,Dir},[],[magnet]},
	    wings_menu_util:rotate(St),
	    wings_menu_util:scale(St),
	    {?__(3,"Slide"), slide,
	     ?__(4,"Slide edges along neighbor edges")},
	    separator,
	    {?__(5,"Extrude"),{extrude,Dir}},
	    {?__(22,"Crease"),crease,
	     ?__(23,"Extrusion commonly used for adding wrinkles to organic models")},
	    separator,
	    wings_menu_util:flatten(),
	    separator,
	    cut_line(St),
	    {?__(6,"Connect"),connect(),
	     	{?__(7,"Create a new edge by connecting midpoints of selected edges"), [],
		 ?__(25,"Create a new edge and slides it along neighbor edges")},[]},
	    {?__(8,"Bevel"),bevel,
	     ?__(9,"Round off selected edges")},
	    separator,
	    {?__(10,"Dissolve"), dslv(),
		{?__(11,"Eliminate selected edges"), [],
		 ?__(50,"Eliminate selected edges and remove remaining isolated verts")},[]},
	    {?__(12,"Collapse"),collapse_fun(),
	     	{?__(13,"Delete edges, replacing them with vertices"), [],
		 ?__(21,"Delete edges, replacing them with vertices and remove any newly created isolated vertices")},[]},
	    separator,
	    {?__(14,"Hardness"),
	     {hardness,[{?__(15,"Soft"),soft},
			        {?__(16,"Hard"),hard},
			        {?__(24,"Invert"),invert}
			        ]}},
	    separator,
	    {?__(17,"Loop Cut"),loop_cut,
	     ?__(18,"Cut into two objects along edge loop")},
	    separator,
	    {?__(19,"Vertex Color"),vertex_color,
	     ?__(20,"Apply vertex colors to selected edges")}],
    wings_menu:popup_menu(X, Y, edge, Menu).

connect() ->
    fun
	(1, _Ns) -> {edge,connect};
	(3, _Ns) -> {edge,connect_slide};
	(_, _) -> ignore
    end.
dslv() ->
    fun
	(1, _Ns) -> {edge,dissolve};
	(3, _Ns) -> {edge,clean_dissolve};
	(_, _) -> ignore
    end.
collapse_fun() ->
    fun
	(1, _Ns) -> {edge,collapse};
	(3, _Ns) -> {edge,clean_collapse};
	(_, _) -> ignore
    end.
cut_line(#st{sel=[{_,Es}]}) ->
    case gb_sets:size(Es) of
	1 -> cut_fun();
	_ -> plain_cut_menu()
    end;
cut_line(_) -> plain_cut_menu().

plain_cut_menu() ->
    {cut_command(),{cut,cut_entries()},
     ?__(2,"Cut into edges of equal length")}.

cut_fun() ->
    F = fun(help, _Ns) ->
		{?__(1,"Cut into edges of equal length"),[],
		 ?__(2,"Cut at arbitrary position")};
	   (1, _Ns) -> cut_entries();
	   (2, _) -> ignore;
	   (3, _) -> {edge,cut_pick}
	end,
    {cut_command(),{cut,F}}.

cut_command() ->
    ?__(1,"Cut").

cut_entries() ->
    [cut_entry(2),
     cut_entry(3),
     cut_entry(4),
     cut_entry(5),
     separator,
     cut_entry(10),
     separator,
     cut_ask_entry()].

cut_entry(N) ->
    Str = integer_to_list(N),
    {Str,N,?__(1,"Cut into ") ++ Str ++ ?__(2," edges of equal length")}.

cut_ask_entry() ->
    {?__(2,"Enter Number..."), ask, ?__(1, "Cut into <N> segments")}.

%% Edge commands.
command(bevel, St) ->
    ?SLOW(wings_extrude_edge:bevel(St));
command({extrude,Type}, St) ->
    ?SLOW(wings_extrude_edge:extrude(Type, St));
command({flatten,Plane}, St) ->
    flatten(Plane, St);
command(slide, St) ->
    slide(St);
command(cut_pick, St) ->
    cut_pick(St);
command({cut,ask}, St) ->
    wings_dialog:ask(cut_command(),
		     [{?__(1,"Segments"), 2, []}],
		     fun([Ret]) -> cut(Ret, St) end);
command({cut,Num}, St) ->
    {save_state,cut(Num, St)};
command(connect, St) ->
    {save_state,connect(St)};
command(connect_slide, St) ->
    connect_slide(St);
command(clean_dissolve, St) ->
    {save_state,clean_dissolve(St)};
command(dissolve, St) ->
    {save_state,dissolve(St)};
command(collapse, St) ->
    {save_state, wings_collapse:uniform_collapse(St)};
command(clean_collapse, St0) ->
    St = wings_collapse:clean_uniform_collapse(St0),
    {save_state,St};
command({hardness,Type}, St) ->
    {save_state,hardness(Type, St)};
command(loop_cut, St) ->
    ?SLOW({save_state,loop_cut(St)});
command(auto_smooth, St) ->
    wings_body:auto_smooth(St);
command({move,Type}, St) ->
    wings_move:setup(Type, St);
command({rotate,Type}, St) ->
    wings_rotate:setup(Type, St);
command({scale,Type}, St) ->
    wings_scale:setup(Type, St);
command(crease, St) ->
    ?SLOW(wings_extrude_edge:crease(St));
command(vertex_color, St) ->
    wings_color:choose(fun(Color) ->
			       set_color(Color, St)
		       end).
%%%
%%% The Connect command.
%%%

connect(St) ->
    wings_sel:map_update_sel(fun connect/2, St).

connect(Es0, We0) ->
    Es1 = gb_sets:to_list(Es0),
    Es = remove_nonconnectable(Es1, Es0, We0, []),
    {Vs,We1} = cut_edges(Es, We0),
    We2 = wings_vertex_cmd:connect(Vs, We1),
    Sel = wings_we:new_items_as_gbset(edge, We1, We2),
    We = wings_edge:dissolve_isolated_vs(Vs, We2),
    {We,Sel}.

connect_slide(St0) ->
    St = wings_sel:map_update_sel(fun connect/2, St0),
    slide(St).

cut_edges(Es, We) ->
    mapfoldl(fun(Edge, W0) ->
		     {W,V} = wings_edge:cut(Edge, 2, W0),
		     {V,W}
	     end, We, Es).

%% Remove from the selection all edges that will obviously not get connected,
%% to avoid having those edges first cut and later joined again.

remove_nonconnectable([E|Es], Sel, We, Acc) ->
    Fs = wings_face:from_edges([E], We),
    NearEs = gb_sets:delete(E, gb_sets:from_ordset(wings_face:to_edges(Fs, We))),
    case gb_sets:is_disjoint(Sel, NearEs) of
	false ->
	    remove_nonconnectable(Es, Sel, We, [E|Acc]);
	true ->
	    %% None of edges in the two faces on either side of this
	    %% edge is selected. Therefore, don't even bother cutting
	    %% this edge since there is no chance that the new vertex
	    %% will get connected.
	    remove_nonconnectable(Es, Sel, We, Acc)
    end;
remove_nonconnectable([], _, _, Acc) -> Acc.

%%%
%%% The Vertex Color command.
%%%

set_color(Color, St) ->
    wings_sel:map(fun(Es, We) ->
			  wings_va:set_edge_color(Es, Color, We)
		  end, St).

%%%
%%% The Cut command.
%%%

cut(N, #st{selmode=edge}=St) when N > 1 ->
    wings_sel:map_update_sel(
      fun(Edges, We0) ->
	      We = cut_edges(Edges, N, We0),
	      S = wings_we:new_items_as_gbset(vertex, We0, We),
	      {We,S}
      end, vertex, St);
cut(_, St) -> St.

cut_edges(Edges, N, We0) ->
    gb_sets:fold(fun(Edge, W0) ->
			 {We,_} = wings_edge:cut(Edge, N, W0),
			 We
		 end, We0, Edges).

%%%
%%% Cut at an arbitrary position.
%%%

cut_pick(#st{sel=[_]}=St0) ->
    MF = fun(Es, We) ->
                 case gb_sets:to_list(Es) of
                     [E] -> cut_pick_make_tvs(E, We);
                     _ -> cut_pick_error()
                 end
         end,
    St = wings_sel:map_update_sel(MF, vertex, St0),
    Units = [{percent,{0.0,1.0}}],
    Flags = [{initial,[0]}],
    DF = fun(#we{temp=General}) -> General end,
    wings_drag:general(DF, Units, Flags, St);
cut_pick(#st{}) ->
    cut_pick_error().

-spec cut_pick_error() -> no_return().
cut_pick_error() ->
    wings_u:error_msg(?__(1,"Only one edge can be cut at an arbitrary position.")).

cut_pick_make_tvs(Edge, #we{es=Etab,vp=Vtab,next_id=NewV}=We) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    Start = array:get(Va, Vtab),
    End = array:get(Vb, Vtab),
    Dir = e3d_vec:sub(End, Start),
    Char = {7,7,3.0,3.0,7.0,0.0,
	    <<2#01111100,
	     2#10000010,
	     2#10000010,
	     2#10000010,
	     2#10000010,
	     2#10000010,
	     2#01111100>>},
    Fun = fun(I, D) -> cut_pick_marker(I, D, Edge, We, Start, Dir, Char) end,
    Sel = gb_sets:singleton(NewV),
    {We#we{temp=Fun},Sel}.

cut_pick_marker([I], D, Edge, We0, Start, Dir, Char) ->
    {X,Y,Z} = Pos = e3d_vec:add_prod(Start, Dir, I),
    {MM,PM,ViewPort} = wings_u:get_matrices(0, original),
    {Sx,Sy,_} = wings_gl:project(X, Y, Z, MM, PM, ViewPort),
    Draw = fun(Ds) ->
		   gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
		   gl:color3f(1.0, 0.0, 0.0),
		   gl:shadeModel(?GL_FLAT),
		   gl:disable(?GL_DEPTH_TEST),
		   gl:matrixMode(?GL_PROJECTION),
		   gl:pushMatrix(),
		   gl:loadIdentity(),
		   {W,H} = wings_wm:win_size(),
		   glu:ortho2D(0.0, W, 0.0, H),
		   gl:matrixMode(?GL_MODELVIEW),
		   gl:pushMatrix(),
		   gl:loadIdentity(),
		   gl:rasterPos2f(Sx, Sy),
		   wings_io:draw_bitmap(Char),
		   gl:popMatrix(),
		   gl:matrixMode(?GL_PROJECTION),
		   gl:popMatrix(),
		   gl:popAttrib(),
                   Ds
	   end,
    {We,_} = wings_edge:fast_cut(Edge, Pos, We0),
    D#dlo{hilite={edge, {call_in_this_win,wings_wm:this(),Draw}},src_we=We};
cut_pick_marker({finish,[I]}, D0, Edge, We, Start, Dir, Char) ->
    D = cut_pick_marker([I], D0, Edge, We, Start, Dir, Char),
    D#dlo{vs=none,hilite=none}.

%%%
%%% Clean Dissolve
%%%

clean_dissolve(St0) ->
    St = wings_sel:map(fun(Es, #we{es=Etab}=We) ->
            E1 = gb_sets:size(Es),
            E2 = wings_util:array_entries(Etab),
            case E1 =:= E2 of
              false ->
                    IsolatedVs1 = wings_vertex:isolated(We),
                    We1 = wings_edge:dissolve_edges(Es, We),
                    IsolatedVs2 = wings_vertex:isolated(We1),
                    C = IsolatedVs2 -- IsolatedVs1,
                    wings_edge:dissolve_isolated_vs(C, We1);
              true ->
                   #we{}
            end
    end, St0),
    wings_sel:clear(St).

%%%
%%% The Dissolve command.
%%%

dissolve(St0) ->
    St = wings_sel:map(fun(Es, #we{es=Etab}=We) ->
            E1 = gb_sets:size(Es),
            E2 = wings_util:array_entries(Etab),
            case E1 =:= E2 of
              false ->
                   wings_edge:dissolve_edges(Es, We);
              true ->
                   #we{}
            end
    end, St0),
    wings_sel:clear(St).

%%%
%%% The Hardness command.
%%%

hardness(invert, St) ->
    wings_sel:map(fun(Edges, #we{he=Htab0}=We) ->
			  WereSoft = gb_sets:subtract(Edges,Htab0),
			  Outside = gb_sets:subtract(Htab0,Edges),
			  NowHard = gb_sets:union(WereSoft,Outside),
			  We#we{he=NowHard}
		  end, St);
hardness(soft, St) ->
    wings_sel:map(fun(Edges, #we{he=Htab0}=We) ->
			  Htab = gb_sets:difference(Htab0, Edges),
			  We#we{he=Htab}
		  end, St);
hardness(hard, St) ->
    wings_sel:map(fun(Edges, #we{he=Htab0}=We) ->
			  Htab = gb_sets:union(Htab0, Edges),
			  We#we{he=Htab}
		  end, St).

%%%
%%% The Slide command.
%%%

slide(St0) ->
    Mode = wings_pref:get_value(slide_mode, relative),
    Stop = wings_pref:get_value(slide_stop, false),
    State = {Mode,none,Stop},
    SUp = SDown = SN = SBi = {0.0,0.0,0.0},

    %% FIXME: The use of the process dicationary (wings_slide) will
    %% not work when each #we{} are stored in its own process.
    %%
    %% FIXME: Someone who understands the Up, Dw, N, and Bi parameters
    %% should rewrite this code in a way that can be parallelized
    %% (avoid the use of wings_sel:mapfold/3, since it forces
    %% sequential evaluation in each process).

    {St,{_,_,_,_,MinUp,MinDw}} =
	wings_sel:mapfold(
	  fun(EsSet, We, {Up0,Dw0,N0,Bi0,MinUp,MinDw}) ->
		  LofEs0 = wings_edge_loop:partition_edges(EsSet, We),
		  LofEs = reverse(sort([{length(Es),Es} || Es <- LofEs0])),
		  {{Slides,MUp,MDw},Up,Dw,N,Bi} =
		      slide_setup_edges(LofEs,Up0,Dw0,N0,Bi0,We,
					{gb_trees:empty(),MinUp,MinDw}),
		  {We#we{temp=make_slide_tv(Slides, State)},
                   {Up,Dw,N,Bi,MUp,MDw}}
	  end, {SUp, SDown, SN, SBi, unknown,unknown}, St0),
    Units = slide_units(State,MinUp,MinDw),
    Flags = [{mode,{slide_mode(MinUp,MinDw),State}},{initial,[0]}],
    DF = fun(_, #we{temp=Tv}) -> Tv end,
    wings_drag:fold(DF, Units, Flags, St).

slide_mode(MinUp,MinDw) ->
    fun(help, State)		  ->	slide_help(State);
       ({key,$1}, {relative,F,S}) ->	{absolute,F,S};
       ({key,$1}, {absolute,F,S}) ->	{relative,F,S};
       ({key,$2}, {Mode,none,S})  ->
	    case get(wings_slide) of
		undefined ->
		    {Mode,none,S};
		Dx when Dx >= 0 ->
		    {Mode,positive,S};
		_ ->
		    {Mode,negative,S}
	    end;
       ({key,$2}, {Mode,_,S})	  ->	{Mode,none,S};
       ({key,$3}, {Mode,F,false}) ->	{Mode,F,true};
       ({key,$3}, {Mode,F,true})  ->	{Mode,F,false};

       (units, NewState) ->
	    slide_units(NewState,MinUp,MinDw);

       (done, {NewMode,_,NewStop})->
	    wings_pref:set_value(slide_mode, NewMode),
	    wings_pref:set_value(slide_stop, NewStop),
	    erase(wings_slide);

       (_, _) -> none
    end.

slide_units({absolute,_,false},_,_) -> [distance];
slide_units({absolute,_Freeze,true},MinUp,MinDw) ->
    [{distance, {-MinUp, MinDw}}];
slide_units({relative,_,false},_,_) -> [percent];
slide_units({relative,_,true},_,_) -> [{percent,{-1.0,1.0}}].

slide_help({Mode,Freeze,Stop}) ->
    ["[1] ",slide_help_mode(Mode),
     "  [2] ",slide_help_freeze(Freeze),
     "  [3] ",slide_help_stop(Stop)].

slide_help_mode(relative) -> ?__(1,"Absolute");
slide_help_mode(absolute) -> ?__(2,"Relative").

slide_help_freeze(none)   -> ?STR(slide_help_mode,3,"Freeze direction");
slide_help_freeze(_)	  -> ?STR(slide_help_mode,4,"Thaw direction").

slide_help_stop(false)	  -> ?STR(slide_help_mode,5,"Stop at other edges");
slide_help_stop(true)	  -> ?STR(slide_help_mode,6,"Continue past other edges").

make_slide_tv(Slides, State) ->
    Vs = gb_trees:keys(Slides),
    {Vs,make_slide_fun(Vs, Slides, State)}.

%% The calculating fun
slide_fun(Dx0,{Mode,Freeze,_Stop}, Slides) ->
    {Dx,I} =
	case Freeze of	 %% 3 = UP, 2 = Down
	    none when Dx0 >= 0 -> {Dx0, 3};
	    none ->		  {-Dx0,2};
	    positive -> 	  {Dx0, 3};
	    negative -> 	  {-Dx0,2}
	end,
    case Mode of
	relative ->
	    fun(V,A) ->
		    Slide = gb_trees:get(V, Slides),
		    {Dir, Len, Count} = element(I, Slide),
		    ScaleDir = e3d_vec:mul(e3d_vec:norm(Dir), Dx*(Len/Count)),
		    [{V,e3d_vec:add(element(1,Slide), ScaleDir)}|A]
	    end;
	absolute ->
	    fun(V,A) ->
		    Slide = gb_trees:get(V, Slides),
		    {Dir, _Len, _C} = element(I, Slide),
		    ScaleDir = e3d_vec:mul(e3d_vec:norm(Dir), Dx),
		    [{V,e3d_vec:add(element(1,Slide), ScaleDir)}|A]
	    end
    end.

make_slide_fun(Vs, Slides, State) ->
    fun([Dx|_],Acc) ->
	    put(wings_slide, Dx),
	    Fun = slide_fun(Dx,State,Slides),
	    foldl(Fun, Acc, Vs);
       (new_mode_data, {NewState,_}) ->
	    make_slide_fun(Vs,Slides,NewState);
       (_,_) ->
	    make_slide_fun(Vs,Slides,State)
    end.

slide_setup_edges([{_Sz,Es0}|LofEs],GUp0,GDw0,GN0,GBi0,We,Acc0) ->
    Parts = slide_part_loop(Es0,We),
    {GUp,GDw,GN,GBi,Acc} = slide_add_edges(Parts,GUp0,GDw0,GN0,GBi0,Acc0),
    slide_setup_edges(LofEs,GUp,GDw,GN,GBi,We,Acc);
slide_setup_edges([], Up,Dw,N,Bi,_,Slides) ->
    {Slides,Up,Dw,N,Bi}.

slide_add_edges([{LUp,LDw,LN,Es}|Parts],GUp,GDw,GN,GBi,Acc0) ->
    %%	  io:format("UDN ~p ~p ~p ~p~n", [len(LUp), len(LDw), len(LN), length(Es)]),
    Count  = length(Es)/2,
    Vec1   = norm(LUp), LenUp = len(LUp),
    Vec2   = norm(LDw), LenDw = len(LDw),
    Vec3   = norm(LN),	LenN  = len(LN),
    Bi	   = norm(cross(Vec1,Vec3)),
    Rotation = dot(Bi, norm(GBi)),
%    io:format("BIs ~p ~p ~p ~p ~n", [Bi, norm(GBi), Rotation, Count]),
    case (LenUp/Count > 0.707) and (LenN/Count > 0.707) and
	(abs(Rotation) > 0.707) of
	true ->  %% Edge rings
	    Acc  = slide_dirs(Es,Rotation,Acc0),
	    slide_add_edges(Parts,GUp,GDw,GN,GBi,Acc);
	false ->
	    case (LenUp >= 1) or (LenDw >= 1)
		or (LenN =< 1) or (Count < 4) of
		true -> %% Make sure up is up..
		    DotUp1 = dot(Vec1,norm(GUp)),
		    %%DotDw1 = dot(Vec1,norm(GDw)),
		    %%DotUp2 = dot(Vec2,norm(GUp)),
		    DotDw2 = dot(Vec2,norm(GDw)),
		    if (DotUp1 >= 0) and (DotDw2 >= 0) ->
			    Acc  = slide_dirs(Es,1.0,Acc0),
			    slide_add_edges(Parts,add(Vec1, GUp),add(Vec2,GDw),
					    GN,add(GBi,Bi), Acc);
		       (DotUp1 =< 0) and (DotDw2 =< 0) ->
			    Acc  = slide_dirs(Es,-1.0,Acc0),
			    slide_add_edges(Parts,add(neg(Vec1),GUp),
					    add(neg(Vec2),GDw),
					    GN,add(GBi,neg(Bi)),Acc);
		       true ->
			    Acc  = slide_dirs(Es,-1.0,Acc0),
			    slide_add_edges(Parts,add(Vec2,GUp),
					    add(Vec1,GDw),GN,add(GBi,Bi),Acc)
		    end;
		false ->
		    %% Probably a loop, make sure it goes in/out and not out/in.
		    %% BUGBUG this isn't good enough..
		    Norm0 = Vec3,
		    Dot = dot(Norm0,norm(GUp)),
		    Norm = if Dot < 0.0 -> neg(Norm0); true -> Norm0 end,
		    Acc = slide_dirs(Es,Dot,Acc0),
		    slide_add_edges(Parts,GUp,add(Norm, GDw),GN,GBi,Acc)
	    end
    end;
slide_add_edges([],GUp,GDw,GN,GBi,Acc) ->
    {GUp,GDw,GN,GBi,Acc}.

slide_dirs([{V1,V1dir},{V2,V2dir}|Es],Up,{Acc0,MinU0,MinD0}) ->
    {_,{_,V1M1},{_,V1M2}} = V1dir,
    {_,{_,V2M1},{_,V2M2}} = V2dir,
    case Up < 0 of
	true ->
	    MinU = lists:min([MinU0,V1M2,V2M2]),
	    MinD = lists:min([MinD0,V1M1,V2M1]),
	    Acc1 = add_slide_vertex(V1,swap(V1dir), Acc0),
	    Acc  = add_slide_vertex(V2,swap(V2dir), Acc1),
	    slide_dirs(Es,Up,{Acc,MinU,MinD});
	false ->
	    MinU = lists:min([MinU0,V1M1,V2M1]),
	    MinD = lists:min([MinD0,V1M2,V2M2]),
	    Acc1 = add_slide_vertex(V1,V1dir,Acc0),
	    Acc  = add_slide_vertex(V2,V2dir,Acc1),
	    slide_dirs(Es,Up,{Acc,MinU,MinD})
    end;
slide_dirs([],_,Acc) -> Acc.

slide_part_loop(Es,We) ->
    Def   = {0.0,0.0,0.0},
    Eis   = slide_gather_info(Es,We,[]),
    Parts = wings_edge_loop:edge_links(Es,We),
    [slide_dir(P,Eis,Def,Def,Def,[]) || P <- Parts].

slide_gather_info([Edge|Es],We=#we{es=Etab,vp=Vtab},Acc) ->
    #edge{vs=V1,ve=V2,ltpr=LP,ltsu=LS,lf=LF,rtpr=RP,rtsu=RS,rf=RF} =
	array:get(Edge, Etab),
    A1 = other(V1, array:get(RP, Etab)),
    B1 = other(V1, array:get(LS, Etab)),
    A2 = other(V2, array:get(RS, Etab)),
    B2 = other(V2, array:get(LP, Etab)),
    N1 = wings_face:normal(LF,We), N2 = wings_face:normal(RF,We),
    N = norm(average(N2,N1)),
    V1pos = array:get(V1, Vtab), V2pos = array:get(V2, Vtab),
    A1pos = array:get(A1, Vtab), A2pos = array:get(A2, Vtab),
    B1pos = array:get(B1, Vtab), B2pos = array:get(B2, Vtab),
    E1v1  = sub(A1pos,V1pos), E2v1 = sub(B1pos,V1pos),
    E1v2  = sub(A2pos,V2pos), E2v2 = sub(B2pos,V2pos),
    NE1v1 = norm(E1v1), NE2v1 = norm(E2v1),
    NE1v2 = norm(E1v2), NE2v2 = norm(E2v2),

    E1 = norm(average(NE1v1,NE1v2)), E2 = norm(average(NE2v1,NE2v2)),
    New = {V1, {V1pos,{NE1v1,len(E1v1)},{NE2v1,len(E2v1)}},
	   V2, {V2pos,{NE1v2,len(E1v2)},{NE2v2,len(E2v2)}},
	   E1, E2, N},
    slide_gather_info(Es, We, [{Edge,New}|Acc]);
slide_gather_info([], _, Acc) ->
    gb_trees:from_orddict(sort(Acc)).

slide_dir([Edge|R],Es,Up0,Dw0,N0,Acc) ->
    {V1id,V1E,V2id,V2E,E1,E2,MyN} = find_edge(Edge,Es),
    Up = add(Up0, E1),
    Dw = add(Dw0, E2),
    N  = add(N0, MyN),
    slide_dir(R,Es,Up,Dw,N,[{V1id,V1E},{V2id,V2E}|Acc]);
slide_dir([],_,Up,Dw,N,Acc) ->
    {Up,Dw,N,Acc}.

find_edge({Edge,V1,V2},Es) ->
    case gb_trees:get(Edge, Es) of
	{V1,_,V2,_,_,_,_} = R -> R;
	{V2,V2E,V1,V1E,E2,E1,N} ->
	    {V1,swap(V1E),V2,swap(V2E),E1,E2,N}
    end.

other(Vertex, #edge{vs=Vertex,ve=Other}) -> Other;
other(Vertex, #edge{vs=Other,ve=Vertex}) -> Other.

swap({Vpos,Ndir,Pdir}) ->
    {Vpos,Pdir,Ndir}.

add_slide_vertex(V,{Vpos,{Ndir,NL},{Pdir,PL}},Acc) ->
    case gb_trees:lookup(V,Acc) of
	none ->
	    gb_trees:insert(V, {Vpos,{Ndir,NL,1},{Pdir,PL,1}},Acc);
	{value, {_, {Ndir0,NL0,NC0},{Pdir0,PL0,PC0}}} ->
	    New = {Vpos, {add(Ndir,Ndir0),NL+NL0,NC0+1},
		   {add(Pdir,Pdir0),PL+PL0,PC0+1}},
	    gb_trees:update(V, New, Acc)
    end.

%%%
%%% The Loop Cut command.
%%%

loop_cut(St) ->
    wings_sel:clone(fun loop_cut/2, body, St).

loop_cut(Edges, #we{id=Id,fs=Ftab}=We0) ->
    AdjFaces = wings_face:from_edges(Edges, We0),
    case loop_cut_partition(AdjFaces, Edges, We0, []) of
	[_] ->
	    wings_u:error_msg(?__(1,"Edge loop doesn't divide object #~p "
                                  "into two (or more) parts."),
                              [Id]);
	Parts0 ->
	    %% We arbitrarily decide that the largest part of the object
	    %% will be left unselected and will keep the name of the object.

	    Parts1 = [{gb_sets:size(P),P} || P <- Parts0],
	    Parts2 = reverse(sort(Parts1)),
	    [_|Parts] = [gb_sets:to_list(P) || {_,P} <- Parts2],

	    %% Also, this first part will also contain any sub-object
	    %% that was not reachable from any of the edges. Therefore,
	    %% we calculate the first part as the complement of the union
	    %% of all other parts.

	    FirstComplement = ordsets:union(Parts),
	    First = ordsets:subtract(gb_trees:keys(Ftab), FirstComplement),

	    We = wings_dissolve:complement(First, We0),
            New = loop_cut_make_copies(Parts, We0),
            {We,gb_sets:empty(),New}
    end.

loop_cut_make_copies([P|Parts], We0) ->
    Sel = gb_sets:singleton(0),
    We = wings_dissolve:complement(P, We0),
    [{We,Sel,cut}|loop_cut_make_copies(Parts, We0)];
loop_cut_make_copies([], _) -> [].

loop_cut_partition(Faces0, Edges, We, Acc) ->
    case gb_sets:is_empty(Faces0) of
	true -> Acc;
	false ->
	    {AFace,Faces1} = gb_sets:take_smallest(Faces0),
	    Reachable = collect_faces(AFace, Edges, We),
	    Faces = gb_sets:difference(Faces1, Reachable),
	    loop_cut_partition(Faces, Edges, We, [Reachable|Acc])
    end.

collect_faces(Face, Edges, We) ->
    collect_faces(gb_sets:singleton(Face), We, Edges, gb_sets:empty()).

collect_faces(Work0, We, Edges, Acc0) ->
    case gb_sets:is_empty(Work0) of
	true -> Acc0;
	false ->
	    {Face,Work1} = gb_sets:take_smallest(Work0),
	    Acc = gb_sets:insert(Face, Acc0),
	    Work = collect_maybe_add(Work1, Face, Edges, We, Acc),
	    collect_faces(Work, We, Edges, Acc)
    end.

collect_maybe_add(Work, Face, Edges, We, Res) ->
    wings_face:fold(
      fun(_, Edge, Rec, A) ->
	      case gb_sets:is_member(Edge, Edges) of
		  true -> A;
		  false ->
		      Of = wings_face:other(Face, Rec),
		      case gb_sets:is_member(Of, Res) of
			  true -> A;
			  false -> gb_sets:add(Of, A)
		      end
	      end
      end, Work, Face, We).

%%%
%%% The Flatten command.
%%%

flatten({'ASK',Ask}, St) ->
    wings:ask(Ask, St, fun flatten/2);
flatten({Plane,Center}, St) ->
    flatten(Plane, Center, St);
flatten(edge_loop, St) ->
    {save_state,
     wings_sel:map(
       fun(Es, We0) ->
	       EGroups = wings_edge_loop:partition_edges(Es, We0),
	       foldl(fun(Edges, #we{vp=Vtab}=We) ->
	           case wings_edge_loop:edge_loop_vertices(Edges,We) of
	               [Vs] ->
	                   Positions = [array:get(V, Vtab) || V <- Vs],
	                   Plane = e3d_vec:normal(Positions),
	                   wings_vertex:flatten(Vs, Plane, We);
	               _ -> We
	           end
	       end, We0, EGroups)
       end, St)};
flatten(Plane, St) ->
    flatten(Plane, average, St).

flatten(Plane0, average, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Es, We0) ->
	       EGroups = wings_edge_loop:partition_edges(Es, We0),
	       foldl(fun(Edges, We) ->
	           Vs = wings_edge:to_vertices(Edges, We),
	           wings_vertex:flatten(Vs, Plane, We)
	       end, We0, EGroups)
       end, St)};
flatten(Plane0, Center, St) ->
    Plane = wings_util:make_vector(Plane0),
    {save_state,
     wings_sel:map(
       fun(Es, We) ->
	       Vs = wings_edge:to_vertices(Es, We),
	       wings_vertex:flatten(Vs, Plane, Center, We)
       end, St)}.
