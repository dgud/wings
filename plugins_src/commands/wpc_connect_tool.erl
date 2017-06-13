%%
%%  wpc_connect_tool.erl --
%%
%%     Connect/Cut mode plugin.
%%
%%  Copyright (c) 2004-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_connect_tool).

-export([init/0,menu/2,command/2]).
-export([line_intersect2d/4]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).

-include_lib("wings/src/wings.hrl").

%% State info
-record(cs, {v=[],  %% Connected vertices
	     last,  %% Last vertex selected (for ending loop)
	     we,    %% Current we
	     st,    %% Working St
	     cpos,  %% Cursor Position
	     mode=normal, %% or slide
	     loop_cut = false, %% Cut all around
	     backup,%% State to go back when something is wrong
	     ost}). %% Original St

%% Vertex info
-record(vi, {id,    %% Vertex Id
	     mm,    %% MatrixMode
	     pos}). %% Vertex Pos

-define(EPS, 1.0E-6).

init() -> true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(1,"Connect"), connect,
	       ?__(2,"Mode for quickly connecting vertices and edges")}
	     ];
menu(_, Menu) -> Menu.

command({tools,connect}, St0) ->
    wings:mode_restriction([vertex,edge]), %% ,face
    St = wings_undo:init(St0#st{selmode=edge,sel=[],sh=true}),
    wings_draw:refresh_dlists(St),
    C = #cs{ost=St0, st=St},
    help(C),
    wings_tweak:toggle_draw(false),
    {seq,push,update_connect_handler(C)};
command(_, _) -> next.
                      
%% Event handler for connect mode

update_connect_handler(#cs{st=St}=C) ->
    wings_wm:current_state(St),
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_connect_event(Ev, C) end}.

handle_connect_event(redraw, C) ->
    help(C),
    redraw(C),
    keep;
handle_connect_event(Ev, #cs{st=St}=C) ->
    Cam = wings_camera:event(Ev, St),
    case Cam of
	next -> handle_connect_event0(Ev, C);
	Other -> Other
    end.

handle_connect_event0(#keyboard{sym=?SDLK_ESCAPE}, C) ->
    exit_connect(C);
handle_connect_event0(#mousemotion{}=Ev, #cs{st=St, v=VL}=C) ->
    Update = VL /= [],
    Redraw = fun() -> redraw(C) end,
    Options = [{always_dirty,Update}, 
	       {filter, fun(Hit) -> filter_hl(Hit,C) end}],
    case wings_pick:hilite_event(Ev, St, Redraw, Options) of
	next -> handle_connect_event1(Ev, C);
	Other -> Other
    end;
handle_connect_event0(Ev=#keyboard{unicode=Char}, S=#cs{loop_cut=LC}) ->
    case Char of
	$1 -> update_connect_handler(S#cs{loop_cut=(not LC)});
	_ -> handle_connect_event1(Ev, S)
    end;
handle_connect_event0(Ev, S) -> handle_connect_event1(Ev, S).

handle_connect_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
		      #cs{st=St0}=C) ->
    case wpa:pick(X, Y, St0) of
	{add,{_,_,MM},#st{selmode=edge}=St} ->
	    cut_edge(X, Y, MM, St, C);
	_Other ->
	    keep
    end;
handle_connect_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED},
		      #cs{st=St0}=C0) ->
    case wpa:pick(X,Y,St0) of
	{add,{_,_,MM},St} ->
	    C = do_connect(MM, St, C0),
	    update_hook(C),
	    wings_draw:refresh_dlists(C#cs.st),
	    update_connect_handler(C);
	_Other ->
	    keep
    end;
handle_connect_event1(#mousebutton{button=3,state=?SDL_RELEASED}, C) ->
    exit_connect(C);
handle_connect_event1(init_opengl, #cs{st=St}) ->
    wings:init_opengl(St),
    wings_draw:refresh_dlists(St),
    keep;
handle_connect_event1(quit=Ev, C) ->
    wings_wm:later(Ev),
    exit_connect(C);
handle_connect_event1(got_focus, C = #cs{st=#st{selmode=Mode0,sh=Sh0}=St}) ->
    {Mode,Sh} = case Mode0 of
      edge -> {Mode0,Sh0};
      vertex -> {Mode0,Sh0};
      _ -> {edge,true}
    end,
    update_connect_handler(C#cs{st=St#st{selmode=Mode,sel=[],sh=Sh}});
handle_connect_event1({current_state,St}, #cs{st=St}) ->
    %% FIXME: Don't compare state records. It won't work when
    %% we record have been moved to their own process.
    keep; %% Ignore my own changes.
handle_connect_event1({current_state,St}=Ev, C) ->
    case topological_change(St) of
	false ->
	    wings_draw:refresh_dlists(St),
	    update_connect_handler(C#cs{st=St});
	true ->
	    wings_tweak:toggle_draw(true),
	    wings_wm:later(Ev),
	    wings:unregister_postdraw_hook(geom,?MODULE),
	    pop
    end;
handle_connect_event1({slide_setup,Drag},_C) ->
    wings_drag:do_drag(Drag,none);
handle_connect_event1({new_state,St0},
		      #cs{mode=slide,we=ObjId,v=[V|VR],backup=Old}=C0) ->
    wings_tweak:toggle_draw(false),
    F = fun(#we{vp=Vtab}) ->
                array:get(V#vi.id, Vtab)
        end,
    Pos = wings_obj:with_we(F, ObjId, St0),
    St1 = St0#st{sel=[],temp_sel=none,sh=true},
    C1 = C0#cs{mode=normal,v=[V#vi{pos=Pos}|VR],st=St1},
    case connect_edge(C1) of
	C1 when VR /= [] -> 
	    update_hook(Old),
	    wings_draw:refresh_dlists(Old#cs.st),
	    update_connect_handler(Old);
	C ->
	    St = wings_undo:save(Old#cs.st,C#cs.st),
	    update_hook(C),
	    wings_draw:refresh_dlists(St),
	    update_connect_handler(C#cs{st=St})
    end;
handle_connect_event1({new_state,St}=Ev, C) ->
    case topological_change(St) of
	false ->
	    wings_draw:refresh_dlists(St),
	    update_connect_handler(C#cs{st=St});
	true ->
	    wings_tweak:toggle_draw(true),
	    wings_wm:later(Ev),
	    wings:unregister_postdraw_hook(geom,?MODULE),
	    pop
    end;
handle_connect_event1({action,Action}, #cs{st=St0}=C) ->
    case Action of
	{select,Cmd} -> select_cmd(Cmd, C);
	{view,auto_rotate} -> keep;
	{view,smoothed_preview} -> keep;
	{view,aim} ->
	    St = fake_selection(St0),
	    wings_view:command(aim, St),
	    update_connect_handler(C);
	{view,highlight_aim} ->
	    St = fake_selection(St0),
	    wings_view:command(aim, St),
	    update_connect_handler(C);
	{view,Cmd} ->
	    case wings_view:command(Cmd, St0) of
		keep ->
		    keep;
		#st{}=St ->
		    refresh_dlists(Cmd, St),
		    update_connect_handler(C#cs{st=St})
	    end;
	{edit,undo_toggle} ->
	    St = wings_undo:undo_toggle(St0),
	    undo_refresh(St,C);
	{edit,undo} ->
	    St = wings_undo:undo(St0),
	    undo_refresh(St,C);
	{edit,redo} ->
	    St = wings_undo:redo(St0),
	    undo_refresh(St,C);
	Other ->
	    wings_wm:later({action,Other}),
	    exit_connect(C)
    end;
handle_connect_event1({update_state,St}, C) ->
    wings_draw:refresh_dlists(St),
    update_connect_handler(C#cs{st=St});
handle_connect_event1(Ev, #cs{st=St}) ->
    case wings_hotkey:event(Ev, St) of
	next -> keep;
	Other -> wings_wm:later({action,Other})
    end.

undo_refresh(St0,C0) ->
    St = St0#st{sel=[],temp_sel=none,sh=true},
    C = C0#cs{v=[],we=undefined,last=undefined,mode=normal,st=St},
    update_hook(C),
    wings_draw:refresh_dlists(St),
    update_connect_handler(C).

exit_connect(#cs{ost=St,st=#st{shapes=Shs,views=Views}}) ->
    wings_tweak:toggle_draw(true),
    wings:unregister_postdraw_hook(geom, ?MODULE),
    wings_wm:later({new_state,St#st{shapes=Shs,views=Views}}),
    pop.

refresh_dlists(wireframe_selected, _) -> ok;
refresh_dlists(shade_selected, _) -> ok;
refresh_dlists(toggle_wireframe, _) -> ok;
refresh_dlists(orthogonal_view, _) -> ok;
refresh_dlists(aim, _) -> ok;
refresh_dlists(frame, _) -> ok;
refresh_dlists(toggle_lights, _) -> ok;
refresh_dlists({along,_}, _) -> ok;
refresh_dlists({toggle_lights,_}, _) -> ok;
refresh_dlists(_, St) -> wings_draw:refresh_dlists(St).

select_cmd(deselect, #cs{st=St0}=C) ->
    St = wings_sel:reset(St0),
    update_connect_handler(C#cs{st=St});
select_cmd(vertex=M, C) -> mode_change(M, C);
select_cmd(edge=M, C) -> mode_change(M, C);
% select_cmd(face=M, C) -> mode_change(M, C);
% select_cmd(body=M, C) -> mode_change(M, C);
% select_cmd({adjacent,M}, C) -> mode_change(M, C);
select_cmd(_, _) -> keep.

mode_change(Mode, #cs{st=St0}=C) ->
    St = St0#st{selmode=Mode,sh=false},
    update_connect_handler(C#cs{st=St}).

topological_change(#st{shapes=Shs}) ->
    R = wings_dl:fold(fun(#dlo{src_we=We}, [We|Wes]) -> Wes;
			 (#dlo{drag=none}, [_|Wes]) -> Wes;
			 (_, _) -> changed
		      end, gb_trees:values(Shs)),
    R =:= changed.

redraw(#cs{st=St}) ->
    wings:redraw("", St),
    keep.

filter_hl(_, _) -> true.  %% Debug connection

do_connect(MM, #st{selmode=vertex}=St0, #cs{v=VL,loop_cut=LC,we=Prev}=C0) ->
    MF = fun(Vs, #we{id=ObjId}=We0, []) ->
                 [Id1] = gb_sets:to_list(Vs),
                 Pos = wings_vertex:pos(Id1, We0),
                 Fs = vertex_fs(Id1, We0),
                 VI = #vi{id=Id1,mm=MM,pos=Pos},
                 case VL of
                     [] ->
                         {We0,{no_connection,[VI],ObjId,Id1}};
                     [#vi{id=Id1}|_] ->
                         {We0,{no_connection,[],undefined,undefined}};
                     [#vi{id=Id2}|_] when Prev =:= ObjId ->
                         Ok = vertex_fs(Id2, We0),
                         We = connect_link(get_line(Id1, Id2, MM, We0),
                                           Id1, Fs, Id2, Ok, LC, MM, We0),
                         #we{} = We,
                         {We,{connected,[VI],ObjId,Id1}};
                     _ ->
                         {We0,none}
                 end
         end,
    try wings_sel:mapfold(MF, [], St0) of
        {_,none} ->
            C0;
        {_,{no_connection,ViList,WeId,Last}} ->
            C0#cs{v=ViList,we=WeId,last=Last,
                  st=St0#st{sel=[],temp_sel=none,sh=true}};
        {St1,{connected,ViList,WeId,Last}} ->
            St2 = St1#st{sel=[],temp_sel=none,sh=true},
            St = wings_undo:save(St0, St2),
            C0#cs{v=ViList,we=WeId,last=Last,st=St}
    catch
        _:_ ->
            C0
    end;
do_connect(_, _, C) ->
    C.

connect_edge(C0=#cs{v=[_]}) -> C0;
connect_edge(C0=#cs{v=[#vi{id=Id1,mm=MM}=VI,#vi{id=Id2}],
                    loop_cut=LC,we=ObjId,st=St0}) ->
    UF = fun(We0) ->
                 Fs = vertex_fs(Id1, We0),
                 Ok = vertex_fs(Id2, We0),
                 #we{} = connect_link(get_line(Id1, Id2, MM, We0),
                                      Id1, Fs, Id2, Ok, LC, MM, We0)
         end,
    try
        St = wings_obj:update(UF, [ObjId], St0),
	C0#cs{v=[VI],last=Id1,st=St}
    catch
        _:_ ->
	    C0
    end.

cut_edge(X, Y, MM, #st{selmode=edge}=St0, #cs{v=VL,we=Prev}=C0) ->
    MF = fun(_, #we{id=Id}=We, []) when Prev =/= undefined,
                                        Prev =/= Id ->
                 {We,none};             %Wrong We
            (Edges, #we{id=Id,es=Etab}=We0, []) ->
                 [Edge] = gb_sets:to_list(Edges),
                 #edge{vs=V1,ve=V2} = array:get(Edge, Etab),
                 {Pos,_Fs} = calc_edgepos(X, Y, Edge, MM, We0, VL),
                 {We,V} = wings_edge:fast_cut(Edge, Pos, We0),
                 {We,{Id,V,Pos,V1,V2}}
         end,
    case wings_sel:mapfold(MF, [], St0) of
        {_,none} ->
            keep;
        {St1,{ObjId,V,Pos,V1,V2}} ->
            St = St1#st{sel=[],temp_sel=none,sh=true},
            VI = #vi{id=V,mm=MM,pos=Pos},
            C = C0#cs{v=[VI|VL],we=ObjId,last=V,st=St},
            wings_draw:refresh_dlists(C#cs.st),
            {drag,Drag} = slide(C, V1, V2),
            wings_wm:later({slide_setup,Drag}),
            update_connect_handler(C#cs{mode=slide,backup=C0})
    end.

vertex_fs(Id, We) ->
    Fs = wings_vertex:fold(fun(_,Face,_,Acc) -> [Face|Acc] end, [], Id, We),
    ordsets:from_list(Fs).

connect_link(CutLine,IdStart,FacesStart,IdEnd,FacesEnd,LC,MM,We0) ->
    Prev = gb_sets:from_list([IdStart,IdEnd]),
    We1 = connect_link1(CutLine,IdStart,FacesStart,IdEnd,FacesEnd,Prev,
			{normal,MM},We0),
    case LC of  %% loop cut
	true ->
%%	    io:format("**********Second**********~n",[]),
	    FsStart = vertex_fs(IdStart,We1),
	    FsEnd   = vertex_fs(IdEnd,We1),
	    connect_link1(CutLine,IdStart,FsStart,IdEnd,FsEnd,Prev,
			  {inverted,MM},We1);
	false ->  We1
    end.
connect_link1(CutLine,IdStart,FacesStart,IdEnd,FacesEnd,Prev0,NMM,We0) ->
%%    io:format("~p cut ~p <=> ~p ~n", [?LINE, IdStart, IdEnd]),
    case connect_done(FacesStart,FacesEnd,Prev0,NMM,We0) of
	{true,LastFace} -> %% Done
	    wings_vertex:connect(LastFace,[IdStart,IdEnd],We0);
	{false,_} ->	    
	    Find = check_possible(CutLine,Prev0,NMM,We0),
	    Cuts = wings_face:fold_faces(Find,[],FacesStart,We0),
	    Selected = select_way(lists:usort(Cuts),We0,NMM),
	    {We1,Id1} = case Selected of
			    {vertex,Id,_Face} ->
				{We0, Id};
			    {edge,Edge,_Face,_,Pos} ->
				wings_edge:fast_cut(Edge, Pos, We0)
			end,
	    Ok = vertex_fs(Id1,We1),
%% 	    io:format("~p ~p of ~p fs ~w~n", [?LINE, Id1, Cuts, Selected]),
%% 	    io:format("~p ~w ~w ~w~n", [?LINE, Ok,FacesStart,ordsets:intersection(Ok,FacesStart)]),
	    [First] = ordsets:intersection(Ok,FacesStart),
	    We = wings_vertex:connect(First,[Id1,IdStart],We1),
	    Prev = gb_sets:insert(Id1, Prev0),
	    connect_link1(CutLine,Id1,Ok,IdEnd,FacesEnd,Prev,NMM,We)
    end.

check_possible(CutLine,Prev,{_,MM},We) ->
    fun(Face, _V, Edge, #edge{vs=Vs,ve=Ve}, Acc) -> 
	    case gb_sets:is_member(Vs,Prev) orelse 
		gb_sets:is_member(Ve,Prev) of
		true -> %% Already used.
		    Acc;
		false ->
		    Edge2D = get_line(Vs,Ve,MM,We),
		    InterRes = line_intersect2d(CutLine,Edge2D),
		    case InterRes of
			{false,{1,Pos2D}} -> 
			    [{edge,Edge,Face,false,
			      pos2Dto3D(Pos2D,Edge2D,Vs,Ve,We)}|Acc];
			{false,_} -> Acc;
			{true, Pos2D} ->
			    [{edge,Edge,Face,true,
			      pos2Dto3D(Pos2D,Edge2D,Vs,Ve,We)}|Acc];
			{{point, 3},_Pos2d} -> 
			    [{vertex,Vs,Face}|Acc];
			{{point, 4},_Pos2d} -> 
			    [{vertex,Ve,Face}|Acc];
			_Else -> 
			    Acc
		    end
	    end	    
    end.

connect_done(End,Start,Prev,MM,We) ->
    First = gb_sets:size(Prev) == 2,
    case ordsets:intersection(Start,End) of
	[LastFace] when First -> %% Done
	    {check_normal(LastFace,MM,We), LastFace};
	[LastFace] -> 
	    {true,LastFace};
	List ->  %% Arrgh imageplane like construction workarounds
	    GoodNormals = [Face || Face <- List, check_normal(Face,MM,We)],
%%	    io:format("~p ~w from ~w~n", [?LINE,GoodNormals,List]),
	    case GoodNormals of
		[Face] -> {true,Face}; 
		_ ->      {false,undefined}
	    end
    end.

select_way([],_,_) -> exit(vertices_are_not_possible_to_connect);
select_way([Cut],_,_) -> Cut;
select_way(Cuts,We,NMM = {Mode,_}) ->     
    Priortize = 
	fun(Cut = {edge,_,Face,Intersect,_}) ->
		FaceScreen = check_normal(Face,NMM,We),
		if 
		    FaceScreen and Intersect ->  {1,Cut};
		    FaceScreen -> {3,Cut};
		    Intersect, Mode == normal ->    {4,Cut};
		    Intersect, Mode == inverted ->  {7,Cut};
		    true ->  {6,Cut}
		end;
	   (Cut = {vertex,_,Face}) ->
		FaceScreen = check_normal(Face,NMM,We),
		if 
		    FaceScreen -> {2,Cut};
		    Mode == inverted -> {8,Cut};
		    true -> {5,Cut}
		end
	end,
    Sorted = lists:sort(lists:map(Priortize, Cuts)),
%%    io:format("~p ~p ~p~n",[?LINE, Mode, Sorted]),
    [{_P,Cut}|_R] = Sorted,
    Cut.

check_normal(Face,{Way,MM},We = #we{id=Id}) ->
    {MVM,_PM,_} = wings_u:get_matrices(Id, MM),
    Normal0 = wings_face:normal(Face,We),
    {_,_,Z} = e3d_mat:mul_vector(MVM, Normal0),
    if 
	Way == normal, Z > 0.1 -> true;
	Way == inverted, Z < -0.1 -> true;
	true -> false
    end.

pos2Dto3D({IX,IY}, {V1Sp,V2Sp}, V1,V2, #we{vp=Vs}) ->
    Pos1 = array:get(V1, Vs),
    Pos2 = array:get(V2, Vs),
    TotDist = e3d_vec:dist(V1Sp,V2Sp),
    Dist = e3d_vec:dist(V1Sp,{float(IX),float(IY),0.0}) / TotDist,
    Vec = e3d_vec:mul(e3d_vec:sub(Pos2,Pos1),Dist),
    e3d_vec:add(Pos1, Vec).

get_line(V1,V2,MM,#we{id=Id,vp=Vs}) ->
    P1 = array:get(V1, Vs),
    P2 = array:get(V2, Vs),
    Matrices = wings_u:get_matrices(Id, MM),
    V1Sp = setelement(3,obj_to_screen(Matrices, P1),0.0),
    V2Sp = setelement(3,obj_to_screen(Matrices, P2),0.0),
    {V1Sp,V2Sp}.

calc_edgepos(X,Y0,Edge,MM,#we{id=Id,es=Es,vp=Vs},VL) ->
    {_,H} = wings_wm:win_size(),
    Y = H-Y0,
    #edge{vs=V1,ve=V2,lf=F1,rf=F2} = array:get(Edge, Es),
    Pos1 = array:get(V1, Vs),
    Pos2 = array:get(V2, Vs),
    Matrices = wings_u:get_matrices(Id, MM),
    V1Sp = setelement(3,obj_to_screen(Matrices, Pos1),0.0),
    V2Sp = setelement(3,obj_to_screen(Matrices, Pos2),0.0),
    Dist = case VL of
        [] ->
          V1Dist  = e3d_vec:dist(V1Sp,{float(X),float(Y),0.0}),
          V2Dist  = e3d_vec:dist(V2Sp,{float(X),float(Y),0.0}),
          %%TotDist = e3d_vec:dist(V1Sp,V2Sp),
          TotDist = V1Dist+V2Dist,
          V1Dist/TotDist;
        _ ->
          Cursor = {float(X),float(Y),0.0},
          e3d_vec:dist(V1Sp, Cursor) / e3d_vec:dist(V1Sp,V2Sp)
    end,
    Vec = e3d_vec:sub(Pos2,Pos1),
    Pos = e3d_vec:add(Pos1,e3d_vec:mul(Vec, abs(Dist))),
    {Pos, ordsets:from_list([F1,F2])}.

line_intersect2d({V1,V2},{V3,V4}) ->
    line_intersect2d(V1,V2,V3,V4).

line_intersect2d({X1,Y1,_},{X2,Y2,_},{X3,Y3,_},{X4,Y4,_}) ->
    line_intersect2d({X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4});
line_intersect2d({X1,Y1},{X2,Y2},{X3,Y3},{X4,Y4}) ->
    Div = ((Y4-Y3)*(X2-X1)-(X4-X3)*(Y2-Y1)),
    if Div == 0.0 -> {false,{both,paralell}};
       true ->
	    Ua = ((X4-X3)*(Y1-Y3)-(Y4-Y3)*(X1-X3)) / Div,
	    Ub = ((X2-X1)*(Y1-Y3)-(Y2-Y1)*(X1-X3)) / Div,
	    X = X1 + Ua*(X2-X1),
	    Y = Y1 + Ua*(Y2-Y1),
	    
	    if 
		(Ua < -?EPS); (Ua > 1.0+?EPS) ->
		    if (Ub < -?EPS); (Ub > 1.0+?EPS) -> 
			    {false,{both,{X,Y}}};
		       true -> 
			    {false,{1,{X,Y}}}
		    end;
		(Ub < -?EPS); (Ub > 1.0+?EPS)  ->
		    {false, {2, {X,Y}}};
		(Ua > -?EPS), (Ua < 1.0+?EPS) ->
		    if (Ub > ?EPS), (Ub < 1.0-?EPS) -> {true, {X,Y}};
		       Ub > -?EPS, Ub < ?EPS -> {{point,3},{X,Y}};
		       true -> {{point,4},{X,Y}}
		    end;
		true ->
		    if 
			Ua > -?EPS, Ua < ?EPS -> {{point,1},{X,Y}};
			true -> {{point,2},{X,Y}}
		    end
	    end
    end.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    wings_gl:project(X, Y, Z, MVM, PM, VP).

help(Cs = #cs{v=[]}) ->
    Msg1 = wings_msg:button_format(?__(1,"Select vertex or cut edge [press button to slide]")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(2,"Exit Connect")),
    Msg = wings_msg:join([Msg1,Msg2,Msg3]),    
    wings_wm:message(Msg, lc_help(Cs));
help(Cs) ->
    Msg1 = wings_msg:button_format(?__(3,"Connects edges/vertices [reselect last vertex to end]")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(4,"Exit Connect")),
    Msg = wings_msg:join([Msg1,Msg2,Msg3]),
    wings_wm:message(Msg, lc_help(Cs)).

lc_help(#cs{loop_cut=true}) -> "[1] " ++ ?__(1,"Loop Connect Off");
lc_help(_) ->                  "[1] " ++ ?__(2,"Loop Connect On").

fake_selection(St) ->
    wings_dl:fold(fun(#dlo{src_sel=none}, S) ->
			  %% No selection, try highlighting.
			  fake_sel_1(S);
		     (#dlo{src_we=#we{id=Id},src_sel={Mode,Els}}, S) ->
			  S#st{selmode=Mode,sel=[{Id,Els}]}
		  end, St).

fake_sel_1(St0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wpa:pick(X, Y, St0) of
	{add,_,St} -> St;
	_ -> St0
    end.

update_hook(#cs{v=[]}) ->
    wings:unregister_postdraw_hook(geom,?MODULE);
update_hook(C) ->
    Hook = fun(_St) -> draw_connect(C) end,
    wings:register_postdraw_hook(geom,?MODULE,Hook).

draw_connect(#cs{v=[#vi{pos=Pos0,mm=MM},#vi{pos=Pos1}],we=Id}) ->
    Matrices = wings_u:get_matrices(Id, MM),
    Pos01 = setelement(3, obj_to_screen(Matrices, Pos0), 0.0),
    Matrices = wings_u:get_matrices(Id, MM),
    Pos11 = setelement(3, obj_to_screen(Matrices, Pos1), 0.0),
    gldraw_connect(Pos01,Pos11);
draw_connect(#cs{v=[#vi{pos=Pos0,mm=MM}],we=Id}) ->
    {_W,H} = wings_wm:win_size(),
    {_,X,Y0} = wings_wm:local_mouse_state(),
    Y = H-Y0,
    Matrices = wings_u:get_matrices(Id, MM),
    Pos = setelement(3, obj_to_screen(Matrices, Pos0), 0.0),
    gldraw_connect(Pos, {X,Y,0.0}).

gldraw_connect(Pos0, Pos1) ->
    {W,H} = wings_wm:win_size(),
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_LIGHTING),
    gl:disable(?GL_DEPTH_TEST),
    gl:disable(?GL_ALPHA_TEST),
    gl:color3f(0.0, 0.0, 0.0),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0.0, W, 0.0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    wings_vbo:draw(fun(_) ->
			   gl:drawArrays(?GL_LINES, 0, 2)
		   end, [Pos0,Pos1]),
    gl:popAttrib().

slide(#cs{st=#st{}=St0,we=ObjId,v=[#vi{id=Id1,mm=MM}|_]}=C, S, E) ->
    F = fun(#we{vp=Vtab}) ->
                Start0 = array:get(S, Vtab),
                End0   = array:get(E, Vtab),
                Curr   = array:get(Id1, Vtab),
                {Curr,Start0,End0}
        end,
    {Curr,Start0,End0} = wings_obj:with_we(F, ObjId, St0),
    Matrices = wings_u:get_matrices(ObjId, MM),
    P0 = {P0x,P0y,_} = obj_to_screen(Matrices, Start0),
    P1 = {P1x,P1y,_} = obj_to_screen(Matrices, End0),
    %% Decide what's up and down.
    {Dx,Dy,_} = e3d_vec:sub(P1, P0),
    {Start,End} = if
                      abs(Dx) > abs(Dy), P0x < P1x ->
                          {Start0,End0};
                      abs(Dx) > abs(Dy) ->
                          {End0,Start0};
                      P0y < P1y ->
                          {Start0,End0};
                      true ->
                          {End0,Start0}
                  end,
    {Tv,Init} = slide_make_tvs(Id1, Curr, Start, End, C),
    FS = fun(_, #we{id=Id}) when Id =:= ObjId ->
                 gb_sets:singleton(Id1);
            (_, _) ->
                 gb_sets:empty()
         end,
    St = wings_sel:new_sel(FS, vertex, St0),
    Units = [{percent,{0.0+2*?EPS,1.0-2*?EPS}}],
    Flags = [{initial,[Init]}],
    DF = fun(_, _) -> Tv end,
    wings_drag:fold(DF, Units, Flags, St).

slide_make_tvs(V, Curr, Start, End, C) ->
    Dir = e3d_vec:sub(End, Start),
    TotDist = e3d_vec:len(Dir),
    Dist = e3d_vec:dist(Start,Curr),
    CursorPos  = Dist/TotDist,
    Fun = fun(I, Acc) -> sliding(I, Acc, V, Start, Dir, C) end,
    {{[V],Fun},CursorPos}.

sliding([Dx|_],Acc,V,Start,Dir,C= #cs{v=[Vi|Vr]}) ->
    Pos = e3d_vec:add_prod(Start, Dir, Dx),
    if Vr == [] -> ignore; %% No line when sliding single vertex
       true -> update_hook(C#cs{v=[Vi#vi{pos=Pos}|Vr]})
    end,
    [{V,Pos}|Acc].
