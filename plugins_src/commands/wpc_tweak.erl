%%
%%  wpc_tweak.erl --
%%
%%     Tweak mode plugin.
%%
%%  Copyright (c) 2001-2002 Howard Trickey,
%%                2002-2005 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_tweak.erl,v 1.67 2005/10/04 20:31:15 giniu Exp $
%%

-module(wpc_tweak).

-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").

-import(lists, [member/2,foldl/3]).

-record(tweak,
	{tmode,					% wait or drag
	 magnet=false,				% true/false
	 mag_type=dome,				% magnet type: Type
	 mag_r=1.0,				% magnet influence radius
	 ox,oy,					% original X,Y
	 cx,cy,					% current X,Y
	 orig_st,				% keeps undo, selection
	 st}).					% wings st record (working)

-record(drag,
	{vs,
	 pos0,				  	%Original position.
	 pos,					%Current position.
	 mag,
	 mm					%original|mirror
	 }).

-record(mag,
	{orig,					%Orig pos of vertex
						% being moved.
	 vs,					%[{V,Pos,Distance,Influence}]
						%  (not changed while dragging)
	 vtab=[]				%[{V,Pos}] (latest)
	}).

init() -> true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
	      {?__(1,"Tweak"), tweak,
	       ?__(2,"Mode for quickly moving single elements")}
	     ];
menu(_, Menu) -> Menu.

command({tools,tweak}, St0) ->
    wings:mode_restriction([vertex,edge,face,body]),
    Active = wings_wm:this(),
    wings_wm:callback(fun() -> wings_u:menu_restriction(Active, [view]) end),
    case wpa:pref_get(?MODULE, sel_mode) of
	{Mode,Sh0,Mag,MagType} ->
	    MagR = 1.0;
	{Mode,Sh0,Mag,MagType,MagR} ->
	    ok;
	_ ->
	    Mode = vertex,
	    Sh0 = true,
	    Mag = false,
	    MagType = dome,
	    MagR = 1.0
    end,
    Sh = Sh0 andalso wings_pref:get_value(smart_highlighting),
    St = wings_undo:init(St0#st{selmode=Mode,sel=[],sh=Sh}),
    wings_draw:refresh_dlists(St),
    T = #tweak{magnet=Mag,mag_type=MagType,mag_r=MagR,
	       tmode=wait,orig_st=St0,st=St},
    help(T),
    {seq,push,update_tweak_handler(T)};
command(_, _) -> next.

%% Event handler for tweak mode

update_tweak_handler(#tweak{st=St}=T) ->
    wings_draw:update_sel_dlist(),
    wings_wm:current_state(St),
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_tweak_event(Ev, T) end}.

handle_tweak_event(redraw, #tweak{st=St}=T) ->
    help(T),
    redraw(St),
    draw_magnet(T),
    keep;
handle_tweak_event(Ev, #tweak{st=St}=T) ->
    case wings_camera:event(Ev, St) of
	next -> handle_tweak_event0(Ev, T);
	Other -> Other
    end.

handle_tweak_event0(#keyboard{sym=?SDLK_ESCAPE}, T) ->
    exit_tweak(T);
handle_tweak_event0(#keyboard{unicode=C}=Ev, T0) ->
    case magnet_hotkey(C, T0) of
	none -> handle_tweak_event1(Ev, T0);
	T -> update_tweak_handler(T)
    end;
handle_tweak_event0(#mousemotion{}=Ev, #tweak{tmode=wait,st=St}=T) ->
    Redraw = fun() -> redraw(St) end,
    case wings_pick:hilite_event(Ev, St, Redraw) of
	next -> handle_tweak_event1(Ev, T);
	Other -> Other
    end;
handle_tweak_event0(Ev, T) -> handle_tweak_event1(Ev, T).

handle_tweak_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
		    #tweak{tmode=wait,st=St0}=T0) ->
    case wpa:pick(X, Y, St0) of
	{add,MM,St} ->
	    begin_drag(MM, St, T0),
	    do_tweak(0.0, 0.0, false),
	    T = T0#tweak{tmode=drag,ox=X,oy=Y,cx=X,cy=Y},
	    update_tweak_handler(T);
	_ ->
	    update_tweak_handler(T0)
    end;
handle_tweak_event1(#mousemotion{x=X,y=Y,state=?SDL_PRESSED,mod=Mod},
		    #tweak{tmode=drag,cx=CX,cy=CY}=T0) ->
    DX = float(X-CX),
    DY = float(Y-CY),
    do_tweak(DX, DY, (Mod band wings_msg:free_lmb_modifier()) =/= 0),
    T = T0#tweak{cx=X,cy=Y},
    update_tweak_handler(T);
handle_tweak_event1(#mousebutton{button=1,state=?SDL_RELEASED},
		    #tweak{tmode=drag}=T) ->
    end_drag(T);
handle_tweak_event1(#mousemotion{state=?SDL_RELEASED},
		    #tweak{tmode=drag}=T) ->
    end_drag(T);
handle_tweak_event1(#mousebutton{button=3,state=?SDL_RELEASED}, T) ->
    exit_tweak(T);
handle_tweak_event1(init_opengl, #tweak{st=St}) ->
    wings:init_opengl(St),
    wings_draw:refresh_dlists(St),
    keep;
handle_tweak_event1(quit=Ev, T) ->
    wings_wm:later(Ev),
    exit_tweak(T);
handle_tweak_event1({current_state,St}=Ev, T) ->
    case topological_change(St) of
	false ->
	    update_tweak_handler(T#tweak{st=St});
	true ->
	    wings_wm:later(Ev),
	    remember_mode(T),
	    pop
    end;
handle_tweak_event1({new_state,St}=Ev, T) ->
    case topological_change(St) of
	false ->
	    wings_draw:refresh_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	true ->
	    wings_wm:later(Ev),
	    remember_mode(T),
	    pop
    end;
handle_tweak_event1({action,Action}, #tweak{st=St0}=T) ->
    case Action of
	{select,Cmd} -> select_cmd(Cmd, T);
	{view,auto_rotate} -> keep;
	{view,smoothed_preview} -> keep;
	{view,aim} ->
	    St = fake_selection(St0),
	    wings_view:command(aim, St),
	    update_tweak_handler(T);
	{view,Cmd} ->
	    case wings_view:command(Cmd, St0) of
		keep ->
		    keep;
		#st{}=St ->
		    refresh_dlists(Cmd, St),
		    update_tweak_handler(T#tweak{st=St})
	    end;
	{edit,undo_toggle} ->
	    St = wings_undo:undo_toggle(St0),
	    wings_draw:refresh_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	{edit,undo} ->
	    St = wings_undo:undo(St0),
	    wings_draw:refresh_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	{edit,redo} ->
	    St = wings_undo:redo(St0),
	    wings_draw:refresh_dlists(St),
	    update_tweak_handler(T#tweak{st=St});
	Other ->
	    wings_wm:later({action,Other}),
	    exit_tweak(T)
    end;
handle_tweak_event1(Ev, #tweak{st=St}) ->
    case wings_hotkey:event(Ev, St) of
	next -> keep;
	Other -> wings_wm:later({action,Other})
    end.

exit_tweak(#tweak{orig_st=St,st=#st{shapes=Shs,views=Views}}=T) ->
    remember_mode(T),
    wings_wm:later({new_state,St#st{shapes=Shs,views=Views}}),
    pop.

remember_mode(#tweak{magnet=Mag,mag_type=MagType,mag_r=MagR,
		     st=#st{selmode=Mode,sh=Sh}}) ->
    wpa:pref_set(?MODULE, sel_mode, {Mode,Sh,Mag,MagType,MagR}).

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

select_cmd(deselect, #tweak{st=St0}=T) ->
    St = wings_sel:reset(St0),
    update_tweak_handler(T#tweak{st=St});
select_cmd(less, T) ->
    update_tweak_handler(magnet_radius(-1, T));
select_cmd(more, T) ->
    update_tweak_handler(magnet_radius(1, T));
select_cmd(vertex=M, T) -> mode_change(M, T);
select_cmd(edge=M, T) -> mode_change(M, T);
select_cmd(face=M, T) -> mode_change(M, T);
select_cmd(body=M, T) -> mode_change(M, T);
select_cmd({adjacent,M}, T) -> mode_change(M, T);
select_cmd(_, _) -> keep.

mode_change(Mode, #tweak{st=St0}=T) ->
    St = St0#st{selmode=Mode,sh=false},
    update_tweak_handler(T#tweak{st=St}).

topological_change(#st{shapes=Shs}) ->
    R = wings_dl:fold(fun(#dlo{src_we=We}, [We|Wes]) -> Wes;
			 (#dlo{drag=none}, [_|Wes]) -> Wes;
			 (_, _) -> changed
		      end, gb_trees:values(Shs)),
    R =:= changed.

redraw(St) ->
    wings:redraw("", St),
    keep.

begin_drag(MM, St, T) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
			 begin_drag_fun(D, MM, St, T)
		 end, []).

begin_drag_fun(#dlo{src_we=We}=D, _, _, _) when ?IS_LIGHT(We) -> D;
begin_drag_fun(#dlo{src_sel={body,_},src_we=#we{vp=Vtab}=We}=D, _MM, _St, _T) ->
    Vs = gb_trees:keys(Vtab),
    Center = wings_vertex:center(Vs, We),
    Id = e3d_mat:identity(),
    D#dlo{drag={matrix,Center,Id,e3d_mat:expand(Id)}};
begin_drag_fun(#dlo{src_sel={Mode,Els},src_we=We}=D0, MM, St, T) ->
    Vs0 = sel_to_vs(Mode, gb_sets:to_list(Els), We),
    Center = wings_vertex:center(Vs0, We),
    {Vs,Magnet} = begin_magnet(T, Vs0, Center, We),
    D = wings_draw:split(D0, Vs, St),
    D#dlo{drag=#drag{vs=Vs0,pos0=Center,pos=Center,mag=Magnet,mm=MM}};
begin_drag_fun(D, _, _, _) -> D.

end_drag(#tweak{st=St0}=T) ->
    St1 = wings_dl:map(fun end_drag/2, St0),
    St = wings_undo:save(St0, St1),
    wings_draw:refresh_dlists(St),
    help(T),
    update_tweak_handler(T#tweak{tmode=wait,st=St}).

end_drag(#dlo{src_we=#we{id=Id},drag=#drag{}}=D0, #st{shapes=Shs0}=St0) ->
    #dlo{src_we=We} = D = wings_draw:join(D0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    {D#dlo{vs=none,sel=none,drag=none},St};
end_drag(#dlo{src_we=#we{id=Id},drag={matrix,_,Matrix,_},
	      proxy_data=Pd}, #st{shapes=Shs0}=St0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    D = #dlo{src_we=We,proxy_data=Pd},
    {wings_draw:changed_we(D, D),St};
end_drag(D, St) -> {D,St}.
    
sel_to_vs(vertex, Vs, _) -> Vs;
sel_to_vs(edge, Es, We) -> wings_vertex:from_edges(Es, We);
sel_to_vs(face, [Face], We) -> wings_face:vertices_ccw(Face, We).

do_tweak(DX, DY, Normal) ->
    wings_dl:map(fun(D, _) ->
			 do_tweak(D, DX, DY, Normal)
		 end, []).
    				
do_tweak(#dlo{drag={matrix,Pos0,Matrix0,_},src_we=#we{id=Id}}=D0,
	 DX,DY,_AlongNormal) ->
    Matrices = wings_u:get_matrices(Id, original),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    Pos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    Move = e3d_vec:sub(Pos, Pos0),
    Matrix = e3d_mat:mul(e3d_mat:translate(Move), Matrix0),
    D0#dlo{drag={matrix,Pos,Matrix,e3d_mat:expand(Matrix)}};
do_tweak(#dlo{drag=#drag{vs=Vs,pos=Pos0,mag=Mag0,mm=MM}=Drag,
	      src_we=#we{id=Id}}=D0, DX, DY, AlongNormal) ->
    Matrices = wings_u:get_matrices(Id, MM),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    Pos = tweak_pos(AlongNormal, Vs, Pos0, TweakPos, D0),
    {Vtab,Mag} = magnet_tweak(Mag0, Pos),
    D = D0#dlo{sel=none,drag=Drag#drag{pos=Pos,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(D, _, _, _) -> D.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    glu:project(X, Y, Z, MVM, PM, VP).

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    glu:unProject(Xs, Ys, Zs, MVM, PM, VP).

tweak_pos(false, _, _, Pos, _) -> Pos;
tweak_pos(true, Vs, Pos0, TweakPos, #dlo{src_we=We}=D) ->
    N = case Vs of
	    [V] ->				%Vertex mode
		vertex_normal(V, D);		%Unormalized
	    [V1,V2] ->				%Edge mode
		N1 = vertex_normal(V1, D),
		N2 = vertex_normal(V2, D),
		e3d_vec:add(N1, N2);		%Unormalized
	    _ ->				%Face mode
		VsPos = [wings_vertex:pos(V, We) || V <- Vs],
		e3d_vec:normal(VsPos)
	end,
    %% Return the point along the normal closest to TweakPos.
    T = e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / e3d_vec:dot(N, N),
    e3d_vec:add_prod(Pos0, N, T).

%% vertex_normal(Vertex, DLO) -> UnormalizedNormal
%%  Calculate the vertex normal. Will also work for vertices surrounded
%%  by one or more hidden faces. 
vertex_normal(V, D) ->
    OrigWe = wings_draw:original_we(D),
    FaceNs = [face_normal(F, D) || F <- wings_face:from_vs([V], OrigWe)],
    e3d_vec:add(FaceNs).

%% face_normal(Face, DLO) -> Normal
%%  Calculate the face normal. Will also work for faces that
%%  are hidden (including the virtual mirror face).
face_normal(Face, #dlo{src_we=#we{vp=Vtab}}=D) ->
    #we{vp=OrigVtab} = OrigWe = wings_draw:original_we(D),
    Vs = wings_face:vertices_ccw(Face, OrigWe),
    VsPos = [vertex_pos(V, Vtab, OrigVtab) || V <- Vs],
    e3d_vec:normal(VsPos).

vertex_pos(V, Vtab, OrigVtab) ->
    case gb_trees:lookup(V, Vtab) of
	none -> gb_trees:get(V, OrigVtab);
	{value,Pos} -> Pos
    end.

help(#tweak{magnet=false}) ->
    Msg1 = wings_msg:button_format(?__(1,"Drag freely")),
    Msg2 = wings_camera:help(),
    Msg3 = wings_msg:button_format([], [], ?__(2,"Exit tweak mode")),
    FreeMod = wings_msg:free_lmb_modifier(),
    ModName = wings_msg:mod_name(FreeMod),
    Msg4 = [ModName,$+,wings_msg:button_format(?__(3,"Along normal"))],
    Msg = wings_msg:join([Msg1,Msg4,Msg2,Msg3]),
    wings_wm:message(Msg, "[1] "++?__(4,"Magnet On"));
help(#tweak{magnet=true,mag_type=Type}) ->
    FreeMod = wings_msg:free_lmb_modifier(),
    ModName = wings_msg:mod_name(FreeMod),
    Norm = [ModName,$+,wings_msg:button_format(?__(5,"Along normal"))],
    Drag = wings_msg:join([?__(6,"Drag"), Norm]),
    Msg = wings_msg:button_format(Drag, [], ?__(7,"Exit")),
    Types = help_1(Type, [{2,dome}, {3,straight}, {4,spike}]),
    MagMsg = wings_msg:join(["[1] "++?__(8,"Magnet Off"),
			     "[+]/[-] "++?__(9,"Tweak R"),
			     Types]),
    wings_wm:message(Msg, MagMsg).

intl_type(dome)     -> ?__(1,"Dome");
intl_type(straight) -> ?__(2,"Straight");
intl_type(spike)    -> ?__(3,"Spike");
intl_type(Type)     -> wings_util:cap(atom_to_list(Type)).

help_1(Type, [{Digit,Type}|T]) ->
    wings_msg:join("[" ++ [$0+Digit] ++ "] " ++ 
                    [{bold,intl_type(Type)}],
		   help_1(Type, T));
help_1(Type, [{Digit,ThisType}|T]) ->
    wings_msg:join("[" ++ [$0+Digit] ++ "] " ++ 
                    intl_type(ThisType),
		   help_1(Type, T));
help_1(_, []) -> [].

fake_selection(St) ->
    wings_dl:fold(fun(#dlo{src_sel=none}, S) ->
			  %% No selection, try highlighting.
			  fake_sel_1(S);
		     (#dlo{src_we=#we{id=Id},src_sel={Mode,Els}}, S) ->
			  S#st{selmode=Mode,sel=[{Id,Els}]}
		  end, St).

fake_sel_1(St0) ->
    case wings_pref:get_value(use_temp_sel) of
	false -> St0;
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} -> St;
		_ -> St0
	    end
    end.

%%%
%%% Magnetic tweak. Standard tweak is a special case of magnetic tweak
%%% (vertices to be moved have the influence set to 1.0).
%%%

magnet_hotkey(C, #tweak{magnet=Mag,mag_type=Type0}=T) ->
    case hotkey(C) of
	none -> none;
	toggle when Mag == true ->
	    setup_magnet(T#tweak{magnet=false});
	toggle when Mag == false ->
	    setup_magnet(T#tweak{magnet=true});
	Type0 -> T;
	Type -> setup_magnet(T#tweak{magnet=true,mag_type=Type})
    end.

hotkey($1) -> toggle;
hotkey($2) -> dome;
hotkey($3) -> straight;
hotkey($4) -> spike;
hotkey(_) -> none.

setup_magnet(#tweak{tmode=drag}=T) ->
    wings_dl:map(fun(D, _) ->
			 setup_magnet_fun(D, T)
		 end, []),
    do_tweak(0.0, 0.0, false),
    wings_wm:dirty(),
    T;
setup_magnet(T) -> T.

setup_magnet_fun(#dlo{drag=#drag{vs=Vs0,pos0=Center}=Drag}=Dl0,
		 #tweak{st=St}=T) ->
    We = wings_draw:original_we(Dl0),
    {Vs,Mag} = begin_magnet(T, Vs0, Center, We),
    Dl = wings_draw:split(Dl0, Vs, St),
    Dl#dlo{drag=Drag#drag{mag=Mag}};
setup_magnet_fun(Dl, _) -> Dl.

begin_magnet(#tweak{magnet=false}=T, Vs, Center, We) ->
    Mirror = mirror_info(We),
    Near = near(Center, Vs, [], Mirror, T, We),
    Mag = #mag{orig=Center,vs=Near},
    {[Va || {Va,_,_,_,_} <- Near],Mag};
begin_magnet(#tweak{magnet=true}=T, Vs, Center, #we{vp=Vtab0}=We) ->
    Mirror = mirror_info(We),
    Vtab1 = sofs:from_external(gb_trees:to_list(Vtab0), [{vertex,info}]),
    Vtab2 = sofs:drestriction(Vtab1, sofs:set(Vs, [vertex])),
    Vtab = sofs:to_external(Vtab2),
    Near = near(Center, Vs, Vtab, Mirror, T, We),
    Mag = #mag{orig=Center,vs=Near},
    {[Va || {Va,_,_,_,_} <- Near],Mag}.

near(Center, Vs, MagVs, Mirror, #tweak{mag_r=R,mag_type=Type}, We) ->
    RSqr = R*R,
    M = foldl(fun({V,Pos}, A) ->
		      case e3d_vec:dist_sqr(Pos, Center) of
			  DSqr when DSqr =< RSqr ->
			      D = math:sqrt(DSqr),
			      Inf = mf(Type, D, R),
			      Matrix = mirror_matrix(V, Mirror),
			      [{V,Pos,Matrix,D,Inf}|A];
			  _ -> A
		      end;
		 (_, A) -> A
	      end, [], MagVs),
    foldl(fun(V, A) ->
		  Matrix = mirror_matrix(V, Mirror),
		  Pos = wpa:vertex_pos(V, We),
		  [{V,Pos,Matrix,0.0,1.0}|A]
	  end, M, Vs).
    
mf(dome, D, R) when is_float(R) ->
    math:sin((R-D)/R*math:pi()/2);
mf(straight, D, R) when is_float(R) ->
    (R-D)/R;
mf(spike, D0, R) when is_float(R) ->
    D = (R-D0)/R,
    D*D.

magnet_tweak(#mag{orig=Orig,vs=Vs}=Mag, Pos) ->
    Vec = e3d_vec:sub(Pos, Orig),
    Vtab = foldl(fun({V,P0,Plane,_,1.0}, A) ->
			 P1 = e3d_vec:add(P0, Vec),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A];
		    ({V,P0,Plane,_,Inf}, A) ->
			 P1 = e3d_vec:add_prod(P0, Vec, Inf),
			 P = mirror_constrain(Plane, P1),
			 [{V,P}|A]
		 end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

magnet_radius(Sign, #tweak{mag_r=Falloff0}=T0) ->
    case Falloff0+Sign*?GROUND_GRID_SIZE/10 of
	Falloff when Falloff > 0 ->
	    setup_magnet(T0#tweak{mag_r=Falloff});
	_Falloff -> T0
    end.

draw_magnet(#tweak{magnet=false}) -> ok;
draw_magnet(#tweak{st=#st{selmode=body}}) -> ok;
draw_magnet(#tweak{mag_r=R}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_view:load_matrices(false),
    gl:color4f(0, 0, 1, 0.1),
    wings_dl:fold(fun(D, _) ->
			  draw_magnet_1(D, R)
		  end, []),
    gl:popAttrib().

draw_magnet_1(#dlo{mirror=Mtx,drag=#drag{mm=Side,pos={X,Y,Z}}}, R) ->
    case Side of
	mirror -> gl:multMatrixf(Mtx);
	original -> ok
    end,
    gl:translatef(X, Y, Z),
    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, R, 50, 50),
    glu:deleteQuadric(Obj);
draw_magnet_1(_, _) -> [].

mirror_info(#we{mirror=none}) -> {[],none};
mirror_info(#we{mirror=Face}=We) ->
    PlaneNormal = wings_face:normal(Face, We),
    FaceVs = wpa:face_vertices(Face, We),
    Origin = wings_vertex:center(FaceVs, We),
    M0 = e3d_mat:translate(Origin),
    M = e3d_mat:mul(M0, e3d_mat:project_to_plane(PlaneNormal)),
    Flatten = e3d_mat:mul(M, e3d_mat:translate(e3d_vec:neg(Origin))),
    {FaceVs,Flatten}.

mirror_matrix(V, {MirrorVs,Flatten}) ->
    case member(V, MirrorVs) of
	false -> none;
	true -> Flatten
    end.

mirror_constrain(none, Pos) -> Pos;
mirror_constrain(Matrix, Pos) -> e3d_mat:mul_point(Matrix, Pos).
