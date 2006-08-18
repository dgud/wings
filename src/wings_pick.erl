%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_pick.erl,v 1.149 2005/04/12 20:25:19 bjorng Exp $
%%

-module(wings_pick).
-export([event/2,event/3,hilite_event/3, hilite_event/4]).
-export([do_pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2,last/1,reverse/1,reverse/2,
		sort/1,foldl/3,map/2,min/1,
		keysearch/3,member/2,delete/2,seq/2]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

%% For marquee picking.
-record(marquee,
	{ox,oy,					%Original X,Y.
	 cx,cy,					%Current X,Y.
	 st
	}).

%% For highlighting.
-record(hl,
	{st,					%Saved state.
	 redraw,				%Redraw function.
	 always_dirty = false,                  %Always redraw on mousemotion?
	 filter,                                %Filter fun, allow hl? true/false
	 prev=none				%Previous hit ({Id,Item}).
	}).

event(Ev, St) ->
    event(Ev, St, St).

event(#mousemotion{}=Mm, #st{selmode=Mode}=St, Redraw) ->
    case hilite_enabled(Mode) of
	false -> next;
	true -> {seq,push,handle_hilite_event(Mm, #hl{st=St,redraw=Redraw})}
    end;
event(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, St, _) ->
    pick(X, Y, Mod, St);
event(_, _, _) -> next.

hilite_event(Mm, St, Redraw) ->
    hilite_event(Mm, St, Redraw, []).
hilite_event(#mousemotion{}=Mm, #st{selmode=Mode}=St, Redraw, Options) ->
    case hilite_enabled(Mode) of
	false -> next;
	true ->
	    HL = #hl{st=St,redraw=Redraw,
		     always_dirty=proplists:get_value(always_dirty,Options,false),
		     filter=proplists:get_value(filter,Options,true)},
	    {seq,push,handle_hilite_event(Mm, HL)}
    end;
hilite_event(_, _, _,_) -> next.

hilite_enabled(vertex) -> wings_pref:get_value(vertex_hilite);
hilite_enabled(edge) -> wings_pref:get_value(edge_hilite);
hilite_enabled(face) -> wings_pref:get_value(face_hilite);
hilite_enabled(body) -> wings_pref:get_value(body_hilite).

pick(X, Y, Mod, St) when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
    Pick = #marquee{ox=X,oy=Y,st=St},
    clear_hilite_marquee_mode(Pick);
pick(X, Y, _, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    Pick = #marquee{ox=X,oy=Y,st=St0},
	    clear_hilite_marquee_mode(Pick);
	{PickOp,_,St} ->
	    wings_wm:dirty(),
	    wings_draw:refresh_dlists(St),
	    Pick = #pick{st=St,op=PickOp},
	    {seq,push,get_pick_event(Pick)}
    end.

%%
%% Highlighting on mouse move.
%%

get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(redraw, #hl{redraw=#st{sel=[]}=St,prev={_,Where,{_,Elem}}}) ->
    Info = case Where of
	       original ->
		   io_lib:format("#~p", [Elem]);
	       mirror ->
		   wings_util:format("#~p ~s", 
				     [Elem,
				      ?__(2,"(in mirror)")])
	   end,
    wings:redraw(Info, St),
    keep;
handle_hilite_event(redraw, #hl{redraw=#st{}=St}) ->
    wings:redraw(St),
    keep;
handle_hilite_event(redraw, #hl{redraw=Redraw}) ->
    Redraw(),
    keep;
handle_hilite_event(#mousemotion{x=X,y=Y}, HL) ->
    #hl{prev=PrevHit,always_dirty=Dirty,st=St,filter=Accept}=HL,
    case raw_pick(X, Y, St) of
	PrevHit when Dirty ->
	    wings_wm:dirty(),
	    get_hilite_event(HL);
	PrevHit ->
	    get_hilite_event(HL);
	none ->
	    wings_wm:dirty(),
	    insert_hilite_dl(none, St),
	    wings_draw:refresh_dlists(St),
	    get_hilite_event(HL#hl{prev=none});
	Hit ->
	    case accept_hl(Accept, Hit) of
		true ->
		    wings_wm:dirty(),
		    insert_hilite_dl(Hit, St),
		    wings_draw:refresh_dlists(St),
		    get_hilite_event(HL#hl{prev=Hit});
		false ->
		    wings_wm:dirty(),
		    insert_hilite_dl(none, St),
		    wings_draw:refresh_dlists(St),
		    get_hilite_event(HL#hl{prev=none})
	    end
    end;
handle_hilite_event(init_opengl, #hl{st=St}) ->
    wings:init_opengl(St);
handle_hilite_event(_, _) ->
    insert_hilite_dl(none, none),
    next.

accept_hl(Fun, Hit) when is_function(Fun) ->
    Fun(Hit);
accept_hl(_,_) -> true.

insert_hilite_dl(Hit, St) ->
    wings_dl:map(fun(D, _) ->
			 insert_hilite_dl_1(D, Hit, St)
			end, []).

insert_hilite_dl_1(#dlo{src_we=We}=D, _, _) when ?IS_LIGHT(We) -> D;
insert_hilite_dl_1(#dlo{src_we=#we{id=Id}}=D, {Mode,_,{Id,Item}=Hit}, St) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    hilite_color(Hit, St),
    case wings_wm:lookup_prop(select_backface) of
	{value,true} ->
	    gl:disable(?GL_CULL_FACE),
	    hilit_draw_sel(Mode, Item, D),
	    gl:enable(?GL_CULL_FACE);
	_ ->	    
	    hilit_draw_sel(Mode, Item, D)
    end,
    gl:endList(),
    D#dlo{hilite=List};
insert_hilite_dl_1(#dlo{hilite=none}=D, _, _) -> D;
insert_hilite_dl_1(D, _, _) -> D#dlo{hilite=none}.

hilite_color({Id,Item}, #st{sel=Sel}) ->
    Key = case keysearch(Id, 1, Sel) of
	      false -> unselected_hlite;
	      {value,{Id,Items}} ->
		  case gb_sets:is_member(Item, Items) of
		      false -> unselected_hlite;
		      true -> selected_hlite
		  end
	  end,
    gl:color3fv(wings_pref:get_value(Key)).

hilit_draw_sel(vertex, V, #dlo{src_we=#we{vp=Vtab}}) ->
    gl:pointSize(wings_pref:get_value(selected_vertex_size)),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(gb_trees:get(V, Vtab)),
    gl:'end'();
hilit_draw_sel(edge, Edge, #dlo{src_we=#we{es=Etab,vp=Vtab}}) ->
    #edge{vs=Va,ve=Vb} = gb_trees:get(Edge, Etab),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:'begin'(?GL_LINES),
    wpc_ogla:two(gb_trees:get(Va, Vtab),
		 gb_trees:get(Vb, Vtab)),
    gl:'end'();
hilit_draw_sel(face, Face, D) ->
    case wings_pref:get_value(selection_style) of
	stippled -> gl:enable(?GL_POLYGON_STIPPLE);
	solid -> ok
    end,
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:'begin'(?GL_TRIANGLES),
    wings_draw_util:unlit_face(Face, D),
    gl:'end'(),
    gl:disable(?GL_POLYGON_STIPPLE);
hilit_draw_sel(body, _, #dlo{src_we=We}=D) ->
    case wings_pref:get_value(selection_style) of
	stippled -> gl:enable(?GL_POLYGON_STIPPLE);
	solid -> ok
    end,
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    gl:'begin'(?GL_TRIANGLES),
    foreach(fun(Face) ->
		    wings_draw_util:unlit_face(Face, D)
	    end, wings_we:visible(We)),
    gl:'end'(),
    gl:disable(?GL_POLYGON_STIPPLE).

%%
%% Marquee picking.
%%
clear_hilite_marquee_mode(#marquee{st=St}=Pick) ->
    Ctrl = wings_s:key(ctrl),
    Shift = wings_s:key(shift),
    CtrlMsg = ?__(ctrl_action,"Deselect"),
    ShiftMsg = ?__(shift_action,
		   "(De)select only elements wholly inside marquee"),
    Mctrl = wings_util:key_format(Ctrl, CtrlMsg),
    Mshift = wings_util:key_format(Shift, ShiftMsg),
    Message = wings_msg:join(Mctrl, Mshift),
    wings_wm:message(Message),
    wings_wm:dirty_mode(front),
    {seq,push,
     fun(redraw) ->
	     wings:redraw(St),
	     wings_wm:later(now_enter_marquee_mode),
	     keep;
	(now_enter_marquee_mode) ->
	     wings_wm:grab_focus(wings_wm:this()),
	     wings_io:ortho_setup(),
	     gl:flush(),
	     gl:drawBuffer(?GL_FRONT),
	     get_marquee_event(Pick);
	(Ev) ->
	     wings_io:putback_event(Ev),
	     keep
     end}.

get_marquee_event(Pick) ->
    {replace,fun(Ev) -> marquee_event(Ev, Pick) end}.

marquee_event(redraw, #marquee{cx=Cx,cy=Cy,st=St}=M) ->
    gl:drawBuffer(?GL_BACK),
    wings:redraw(St),
    gl:drawBuffer(?GL_FRONT),
    wings_io:ortho_setup(),
    draw_marquee(Cx, Cy, M),
    keep;
marquee_event(init_opengl, #marquee{st=St}) ->
    wings:init_opengl(St);
marquee_event(#mousemotion{x=X,y=Y}, #marquee{cx=Cx,cy=Cy}=M) ->
    draw_marquee(Cx, Cy, M),
    draw_marquee(X, Y, M),
    get_marquee_event(M#marquee{cx=X,cy=Y});
marquee_event(#mousebutton{x=X0,y=Y0,mod=Mod,button=1,state=?SDL_RELEASED}, M) ->
    {Inside,Op} =
	if
	    Mod band ?SHIFT_BITS =/= 0, Mod band ?CTRL_BITS =/= 0 ->
		{true,delete};
	    Mod band ?CTRL_BITS =/= 0 ->
		{false,delete};
	    Mod band ?SHIFT_BITS =/= 0 ->
		{true,add};
	    true ->
		{false,add}
	end,
    #marquee{ox=Ox,oy=Oy,st=St0} = M,
    gl:drawBuffer(?GL_BACK),
    X = (Ox+X0)/2.0,
    Y = (Oy+Y0)/2.0,
    W = abs(Ox-X)*2.0,
    H = abs(Oy-Y)*2.0,
    case marquee_pick(Inside, X, Y, W, H, St0) of
	{none,_} -> ok;
	{Hits,_} ->
	    St = marquee_update_sel(Op, Hits, St0),
	    wings_wm:later({new_state,St})
    end,
    wings_wm:release_focus(),
    wings_wm:later(revert_state),
    wings_wm:dirty_mode(back),
    pop;
marquee_event(_, _) -> keep.

marquee_pick(false, X, Y, W, H, St0) ->
    case pick_all(false, X, Y, W, H, St0) of
	{none,_}=None -> None;
	{Hits,St} -> {[{abs(Id),Face} || {Id,Face} <- Hits],St}
    end;
marquee_pick(true, X, Y0, W, H, St0) ->
    case pick_all(true, X, Y0, W, H, St0) of
	{none,_}=R -> R;
	{Hits0,St} ->
	    Hits1 = wings_util:rel2fam(Hits0),
	    HitsOrig = [Hit || {Id,_}=Hit <- Hits1, Id > 0],
	    HitsMirror = [Hit || {Id,_}=Hit <- Hits1, Id < 0],
	    {MM,PM,ViewPort} = wings_u:get_matrices(0, original),
	    {_,_,_,Wh} = ViewPort,
	    Y = Wh - Y0,
	    RectData = {MM,PM,ViewPort,X-W/2,Y-H/2,X+W/2,Y+H/2},
	    Hits2 = marquee_convert(HitsOrig, RectData, St, []),
	    Hits3 = marquee_convert(HitsMirror, RectData, St, []),
	    Hits = sofs:to_external(sofs:union(Hits2, Hits3)),
	    {Hits,St}
    end.

marquee_convert([{Id,Faces}|Hits], RectData0,
	       #st{selmode=Mode,shapes=Shs}=St, Acc) ->
    We = gb_trees:get(abs(Id), Shs),
    RectData = if
		   Id < 0 ->
		       {MM,PM,_} = wings_u:get_matrices(-Id, mirror),
		       RectData1 = setelement(2, RectData0, PM),
		       setelement(1, RectData1, MM);
		   true -> RectData0
	       end,
    case marquee_convert_1(Faces, Mode, RectData, We) of
	[] ->
	    marquee_convert(Hits, RectData, St, Acc);
	Items ->
	    marquee_convert(Hits, RectData, St, [{abs(Id),Items}|Acc])
    end;
marquee_convert([], _, _, Hits) ->
    sofs:family_to_relation(sofs:family(Hits)).

marquee_convert_1(Faces0, face, Rect, #we{vp=Vtab}=We) ->
    Vfs0 = wings_face:fold_faces(
	     fun(Face, V, _, _, A) ->
		     [{V,Face}|A]
	     end, [], Faces0, We),
    Vfs = wings_util:rel2fam(Vfs0),
    Kill0 = [Fs || {V,Fs} <- Vfs,
		   not is_inside_rect(gb_trees:get(V, Vtab), Rect)],
    Kill1 = sofs:set(Kill0, [[face]]),
    Kill = sofs:union(Kill1),
    Faces1 = sofs:from_external(Faces0, [face]),
    Faces = sofs:difference(Faces1, Kill),
    sofs:to_external(Faces);
marquee_convert_1(Faces, vertex, Rect, #we{vp=Vtab}=We) ->
    Vs = wings_face:to_vertices(Faces, We),
    [V || V <- Vs, is_inside_rect(gb_trees:get(V, Vtab), Rect)];
marquee_convert_1(Faces, edge, Rect, #we{vp=Vtab}=We) ->
    Es0 = wings_face:fold_faces(fun(_, _, E, Rec, A) ->
					[{E,Rec}|A]
				end, [], Faces, We),
    Es = ordsets:from_list(Es0),
    [E || {E,#edge{vs=Va,ve=Vb}} <- Es,
	  is_all_inside_rect([gb_trees:get(Va, Vtab),gb_trees:get(Vb, Vtab)], Rect)];
marquee_convert_1(_Faces, body, Rect, #we{vp=Vtab}) ->
    case is_all_inside_rect(gb_trees:values(Vtab), Rect) of
	true -> [0];
	false -> []
    end.

is_all_inside_rect([P|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([], _Rect) -> true.

is_inside_rect({Px,Py,Pz}, {MM,PM,ViewPort,X1,Y1,X2,Y2}) ->
    {Sx,Sy,_} = glu:project(Px, Py, Pz, MM, PM, ViewPort),
    X1 < Sx andalso Sx < X2 andalso
	Y1 < Sy andalso Sy < Y2.

draw_marquee(undefined, undefined, _) -> ok;
draw_marquee(X, Y, #marquee{ox=Ox,oy=Oy}) ->
    gl:color3f(1, 1, 1),
    gl:enable(?GL_COLOR_LOGIC_OP),
    gl:logicOp(?GL_XOR),
    gl:'begin'(?GL_LINE_LOOP),
    gl:vertex2i(X, Oy),
    gl:vertex2i(X, Y),
    gl:vertex2i(Ox, Y),
    gl:vertex2i(Ox, Oy),
    gl:'end'(),
    gl:flush(),
    gl:disable(?GL_COLOR_LOGIC_OP).

marquee_update_sel(Op, Hits0, #st{selmode=body}=St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits2 = sofs:domain(Hits1),
    Zero = sofs:from_term([0], [data]),
    Hits = sofs:constant_function(Hits2, Zero),
    marquee_update_sel_1(Op, Hits, St);
marquee_update_sel(Op, Hits0, St) ->
    Hits1 = sofs:relation(Hits0, [{id,data}]),
    Hits = sofs:relation_to_family(Hits1),
    marquee_update_sel_1(Op, Hits, St).

marquee_update_sel_1(add, Hits, #st{sel=Sel0}=St) ->
    Sel1 = [{Id,gb_sets:to_list(Items)} || {Id,Items} <- Sel0],
    Sel2 = sofs:from_external(Sel1, [{id,[data]}]),
    Sel3 = sofs:family_union(Sel2, Hits),
    Sel4 = sofs:to_external(Sel3),
    Sel = [{Id,gb_sets:from_list(Items)} || {Id,Items} <- Sel4],
    St#st{sel=Sel};
marquee_update_sel_1(delete, Hits, #st{sel=Sel0}=St) ->
    Sel1 = [{Id,gb_sets:to_list(Items)} || {Id,Items} <- Sel0],
    Sel2 = sofs:from_external(Sel1, [{id,[data]}]),
    Sel3 = sofs:family_difference(Sel2, Hits),
    Sel4 = sofs:to_external(Sel3),
    Sel = [{Id,gb_sets:from_list(Items)} || {Id,Items} <- Sel4, Items =/= []],
    St#st{sel=Sel}.

%%
%% Drag picking.
%%

get_pick_event(Pick) ->
    {replace,fun(Ev) -> pick_event(Ev, Pick) end}.

pick_event(redraw, #pick{st=St}) ->
    wings:redraw(St),
    keep;
pick_event(#mousemotion{x=X,y=Y}, #pick{op=Op,st=St0}=Pick) ->
    case do_pick(X, Y, St0) of
	none -> keep;
	{Op,_,St} ->
	    wings_wm:dirty(),
	    wings_draw:refresh_dlists(St),
	    get_pick_event(Pick#pick{st=St});
	{_,_,_} -> keep
    end;
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=St}) ->
    wings_wm:later({new_state,St}),
    pop;
pick_event(_, _) -> keep.

do_pick(X, Y, St) ->
    case raw_pick(X, Y, St) of
	none -> none;
	Hit -> update_selection(Hit, St)
    end.

raw_pick(X0, Y0, #st{selmode=Mode}=St) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {W,H} = wings_wm:win_size(),
    X = float(X0),
    Y = H-float(Y0),
    S = 5,
    glu:pickMatrix(X, Y, S, S, {0,0,W,H}),
    wings_view:projection(),
    wings_view:modelview(),
    case wings_wm:lookup_prop(select_backface) of
	{value,true} ->
	    draw();
	_ ->
	    gl:enable(?GL_CULL_FACE),
	    draw(),
	    gl:disable(?GL_CULL_FACE)
    end,
    Hits = get_hits(HitBuf),
    case best_face_hit(Hits, Mode) of
	none -> none;
	{Id,Face} -> convert_hit(Id, Face, X, Y, St)
    end.

update_selection({Mode,MM,{Id,Item}}, #st{sel=Sel0}=St) ->
    {Type,Sel} = update_selection(Id, Item, Sel0, []),
    {Type,MM,St#st{selmode=Mode,sel=Sel,sh=false}}.

update_selection(Id, Item, [{I,_}=H|T], Acc) when Id > I ->
    update_selection(Id, Item, T, [H|Acc]);
update_selection(Id, Item, [{I,_}|_]=T, Acc) when Id < I ->
    {add,reverse(Acc, [{Id,gb_sets:singleton(Item)}|T])};
update_selection(Id, Item, [{_,Items0}|T0], Acc) -> %Id == I
    case gb_sets:is_member(Item, Items0) of
	true ->
	    Items = gb_sets:delete(Item, Items0),
	    T = case gb_sets:is_empty(Items) of
		    true -> T0;
		    false -> [{Id,Items}|T0]
		end,
	    {delete,reverse(Acc, T)};
	false ->
	    Items = gb_sets:insert(Item, Items0),
	    {add,reverse(Acc, [{Id,Items}|T0])}
    end;
update_selection(Id, Item, [], Acc) ->
    {add,reverse(Acc, [{Id,gb_sets:singleton(Item)}])}.

%%%
%%% Pick up raw hits.
%%%

get_hits(HitBuf) ->
    gl:flush(),
    case gl:renderMode(?GL_RENDER) of
	0 -> [];
	NumHits ->
 	    HitData = sdl_util:read(HitBuf, 5*NumHits),
 	    get_hits_1(NumHits, HitData, [])
    end.

get_hits_1(0, _, Acc) -> Acc;
get_hits_1(N, [2,_,_,A,B|T], Acc) ->
    get_hits_1(N-1, T, [{A,B}|Acc]).

%%%
%%% Filter face hits to obtain just one hit.
%%%

best_face_hit([], _) -> none;
best_face_hit([Hit], _) -> Hit;
best_face_hit([{Id,_}|T]=Hits, body) ->
    %% If all hits are in the same body we can return any hit.
    best_face_hit_body(T, Id, Hits);
best_face_hit(Hits, _) -> best_face_hit_1(Hits).

best_face_hit_body([{Id,_}|T], Id, Hits) ->
    best_face_hit_body(T, Id, Hits);
best_face_hit_body([_|_], _, Hits) ->
    %% Different bodies.  Must find the nearest face the in the usual way.
    best_face_hit_1(Hits);
best_face_hit_body([], _, [Hit|_]) -> Hit.

best_face_hit_1(Hits0) ->
    Hits = sort([{abs(Id),Id,Face} || {Id,Face} <- Hits0]),
    {_,_,W,H} =  wings_wm:viewport(),
    Model = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    Proj = gl:getDoublev(?GL_PROJECTION_MATRIX),
    ViewPort = {0,0,W,H},
    Orig = glu:unProject(0, 0, 0, Model, Proj, ViewPort),
    Dir0 = glu:unProject(0, 0, 0.5, Model, Proj, ViewPort),
    Dir = e3d_vec:norm_sub(Dir0, Orig),
    ViewRay = {Orig,Dir},
    {_,Best,_} = wings_dl:fold(fun(D, A) ->
				       best_face_hit_1(D, ViewRay, A) end,
			       {Hits,none,infinite}),
    Best.

best_face_hit_1(#dlo{src_we=#we{id=Id}=We,ns=Ns}, EyePoint,
		{[{Id,_,_}|_],_,_}=A) ->
    best_face_hit_2(We, Ns, EyePoint, A);
best_face_hit_1(_, _, A) -> A.

best_face_hit_2(#we{id=Id,vp=Vtab}=We, Ns, Ray, {[{Id,Id,Face}|Hits],Hit0,T0})
  when ?IS_LIGHT(We) ->
    {Orig,Dir} = Ray,
    P = gb_trees:get(1, Vtab),
    T = e3d_vec:dot(Dir, P) - e3d_vec:dot(Dir, Orig),
    A = if
	    T < T0 ->
		{Hits,{Id,Face},T};
	    true ->
		{Hits,Hit0,T0}
	end,
    best_face_hit_2(We, Ns, Ray, A);
best_face_hit_2(#we{id=AbsId}=We, Ns, Ray, {[{AbsId,Id,Face}|Hits],Hit0,T0}) ->
    case gb_trees:get(Face, Ns) of
	[N|[P0|_]] -> ok;
	{N,_,[P0|_]} -> ok
    end,
    P = if 
	    Id < 0 ->
		e3d_mat:mul_point(wings_dl:mirror_matrix(AbsId), P0);
	    true ->
		P0
	end,
    {Orig,Dir} = Ray,
    A = case e3d_vec:dot(N, Dir) of
	    Den when abs(Den) < 1.0e-20 ->
		{Hits,Hit0,T0};
	    Den ->
		T = (e3d_vec:dot(N, P) - e3d_vec:dot(N, Orig)) / Den,
		if
		    T < T0 ->
			{Hits,{Id,Face},T};
		    true ->
			{Hits,Hit0,T0}
		end
	end,
    best_face_hit_2(We, Ns, Ray, A);
best_face_hit_2(_, _, _, A) -> A.

%%
%% Given a face selection hit, return the correct vertex/edge/face/body.
%%

convert_hit(Id, Face, X, Y, #st{selmode=Mode0,shapes=Shs,sel=Sel,sh=Sh}) ->
    Mode = if
	       Sh, Mode0 =/= body, Sel == [] ->
		   {auto,Mode0};
	       true -> Mode0
	   end,
    We = gb_trees:get(abs(Id), Shs),
    if
	Id < 0 -> convert_hit_1(Mode, X, Y, -Id, Face, mirror, We);
	true -> convert_hit_1(Mode, X, Y, Id, Face, original, We)
    end.

convert_hit_1(body, _X, _Y, Id, _Face, MM, _We) ->
    {body,MM,{Id,0}};
convert_hit_1(face, _X, _Y, Id, Face, MM, _We) ->
    {face,MM,{Id,Face}};
convert_hit_1({auto,_}, X, Y, Id, Face, MM, We) ->
    Trans = wings_u:get_matrices(Id, MM),
    Vs = sort(find_vertex(Face, We, X, Y, Trans)),
    [{Vdist0,{Xva,Yva},V},{_,{Xvb,Yvb},_}|_] = Vs,
    Vdist = math:sqrt(Vdist0),
    Es = find_edge(Face, We, X, Y, Trans),
    {Edist0,_,Edge} = min(Es),
    Edist = math:sqrt(Edist0),
    Xd = Xva-Xvb,
    Yd = Yva-Yvb,
    Lim0 = math:sqrt(Xd*Xd+Yd*Yd) / 4,
    Lim1 = min([math:sqrt(L) || {_,L,_} <- Es]) / 4,
    Lim = min([20.0,Lim0,Lim1]),
    Hilite = if
		 Vdist < Lim -> {vertex,MM,{Id,V}};
		 Edist < Lim -> {edge,MM,{Id,Edge}};
		 true -> {face,MM,{Id,Face}}
	     end,
    check_restriction(Hilite, Id, V, Edge, Face);
convert_hit_1(Mode, X, Y, Id, Face, MM, We) ->
    Trans = wings_u:get_matrices(Id, MM),
    case Mode of
	vertex ->
	    {_,_,V} = min(find_vertex(Face, We, X, Y, Trans)),
	    {vertex,MM,{Id,V}};
	edge ->
	    {_,_,E} = min(find_edge(Face, We, X, Y, Trans)),
	    {edge,MM,{Id,E}}
    end.

find_vertex(Face, We, X, Y, Trans) ->
    Vs0 = wings_face:vertices_ccw(Face, We),
    map(fun(V) ->
		{Xs,Ys} = Pos = project_vertex(V, We, Trans),
		Dx = X-Xs,
		Dy = Y-Ys,
		{Dx*Dx+Dy*Dy,Pos,V}
	end, Vs0).

find_edge(Face, We, Cx, Cy, Trans) ->
    wings_face:fold(
      fun(_, Edge, #edge{vs=Va,ve=Vb}, A) ->
	      {Ax,Ay} = project_vertex(Va, We, Trans),
	      {Bx,By} = project_vertex(Vb, We, Trans),
	      if
		  is_float(Ax), is_float(Ay),
		  is_float(Bx), is_float(By) ->
		      Xdist = Bx-Ax,
		      Ydist = By-Ay,
		      L = Xdist*Xdist+Ydist*Ydist,
		      {Px,Py} =
			  try ((Cx-Ax)*Xdist+(Cy-Ay)*Ydist)/L of
			      R when R =< 0 -> {Ax,Ay};
			      R when R >= 1 -> {Bx,By};
			      R -> {Ax+R*Xdist,Ay+R*Ydist}
			  catch
			      error:badarith -> {Ax,Ay}
			  end,
		      Xdiff = Px-Cx,
		      Ydiff = Py-Cy,
		      DistSqr = Xdiff*Xdiff + Ydiff*Ydiff,
		      [{DistSqr,L,Edge}|A]
	      end
      end, [], Face, We).

project_vertex(V, We, {ModelMatrix,ProjMatrix,ViewPort}) ->
    {Px,Py,Pz} = wings_vertex:pos(V, We),
    {Xs,Ys,_} = glu:project(Px, Py, Pz, ModelMatrix,
			    ProjMatrix, ViewPort),
    {Xs,Ys}.

check_restriction({Mode,MM,_}=Hilite, Id, V, Edge, Face) ->
    case wings:get_mode_restriction() of
	all -> Hilite;
	Modes ->
	    case member(Mode, Modes) of
		true -> Hilite;
		false -> restrict_hilite(Mode, Modes, Id, V, Edge, Face, MM)
	    end
    end.

restrict_hilite(vertex, Modes, Id, _V, Edge, Face, MM) ->
    case member(edge, Modes) of
	true -> {edge,MM,{Id,Edge}};
	false ->
	    true = member(face, Modes),
	    {face,MM,{Id,Face}}
    end;
restrict_hilite(edge, Modes, Id, V, _Edge, Face, MM) ->
    case member(vertex, Modes) of
	true -> {vertex,MM,{Id,V}};
	false ->
	    true = member(face, Modes),
	    {face,MM,{Id,Face}}
    end;
restrict_hilite(face, Modes, Id, V, Edge, _Face, MM) ->
    case member(edge, Modes) of
	true -> {edge,MM,{Id,Edge}};
	false ->
	    true = member(vertex, Modes),
	    {vertex,MM,{Id,V}}
    end.
	    
%%
%% Pick all in the given rectangle (with center at X,Y).
%%

pick_all(_DrawFaces, _X, _Y, W, H, St) when W < 1.0; H < 1.0 ->
    {none,St};
pick_all(DrawFaces, X, Y0, W, H, St) ->
    HitBuf = get(wings_hitbuf),
    gl:selectBuffer(?HIT_BUF_SIZE, HitBuf),
    gl:renderMode(?GL_SELECT),
    gl:initNames(),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    {Ww,Wh} = wings_wm:win_size(),
    Y = Wh-Y0,
    glu:pickMatrix(X, Y, W, H, [0,0,Ww,Wh]),
    wings_view:projection(),
    wings_view:modelview(),
    case DrawFaces of
	true ->
	    gl:enable(?GL_CULL_FACE),
	    draw(),
	    gl:disable(?GL_CULL_FACE);
	false -> marquee_draw(St)
    end,
    {get_hits(HitBuf),St}.

marquee_draw(#st{selmode=edge}) ->
    Draw = fun(#we{es=Etab,vp=Vtab}=We) ->
		   Vis = gb_sets:from_ordset(wings_we:visible(We)),
		   marquee_draw_edges(gb_trees:to_list(Etab), Vtab, Vis)
	   end,
    marquee_draw_1(Draw);
marquee_draw(#st{selmode=vertex}) ->
    Draw = fun(#we{vp=Vtab}=We) ->
		   case wings_we:any_hidden(We) of
		       false ->
			   marquee_draw_all_vs(gb_trees:to_list(Vtab));
		       true ->
			   marquee_draw_some_vs(wings_we:visible_vs(We),
						Vtab)
		   end
	   end,
    marquee_draw_1(Draw);
marquee_draw(_) -> draw().

marquee_draw_all_vs([{V,Pos}|VsPos]) ->
    gl:loadName(V),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(Pos),
    gl:'end'(),
    marquee_draw_all_vs(VsPos);
marquee_draw_all_vs([]) -> ok.

marquee_draw_some_vs([V|Vs], Vtab) ->
    gl:loadName(V),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(gb_trees:get(V, Vtab)),
    gl:'end'(),
    marquee_draw_some_vs(Vs, Vtab);
marquee_draw_some_vs([], _) -> ok.

marquee_draw_edges([{Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}|Es], Vtab, Vis) ->
    case gb_sets:is_member(Lf, Vis) orelse gb_sets:is_member(Rf, Vis) of
	false -> ok;
	true ->
	    gl:loadName(Edge),
	    gl:'begin'(?GL_LINES),
	    wpc_ogla:two(gb_trees:get(Va, Vtab),
			 gb_trees:get(Vb, Vtab)),
	    gl:'end'()
    end,
    marquee_draw_edges(Es, Vtab, Vis);
marquee_draw_edges([], _, _) -> ok.

marquee_draw_1(Draw) ->
    wings_dl:fold(fun(D, _) -> marquee_draw_fun(D, Draw) end, []).

marquee_draw_fun(#dlo{src_we=#we{perm=Perm}}, _) when not ?IS_SELECTABLE(Perm) -> ok;
marquee_draw_fun(#dlo{mirror=Mirror,src_we=#we{id=Id}=We}, Draw) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    gl:pushName(0),
    Draw(We),
    gl:popName(),
    gl:endList(),
    gl:pushName(Id),
    case Mirror of
	none ->
	    wings_dl:call(List);
	Matrix ->
	    wings_dl:call(List),
	    gl:pushMatrix(),
	    gl:multMatrixf(Matrix),
	    wings_dl:call(List),
	    gl:popMatrix()
    end,
    gl:popName(),
    gl:deleteLists(List, 1).

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

draw() ->
    wings_dl:map(fun draw_fun/2, []).

draw_fun(#dlo{work=Work,src_we=#we{id=Id,perm=Perm}=We}=D, _)
  when ?IS_LIGHT(We), ?IS_SELECTABLE(Perm) ->
    gl:pushName(Id),
    gl:pushName(1),
    wings_dl:call(Work),
    gl:popName(),
    gl:popName(),
    D;
draw_fun(#dlo{pick=none}=D, _) ->
    List = gl:genLists(1),
    gl:newList(List, ?GL_COMPILE),
    draw_1(D),
    gl:endList(),
    draw_dlist(D#dlo{pick=List});
draw_fun(D, _) -> draw_dlist(D).

draw_dlist(#dlo{mirror=none,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    wings_dl:call(Pick),
    gl:popName(),
    D;
draw_dlist(#dlo{mirror=Matrix,pick=Pick,src_we=#we{id=Id}}=D) ->
    gl:pushName(Id),
    wings_dl:call(Pick),
    gl:loadName(-Id),
    gl:frontFace(?GL_CW),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    wings_dl:call(Pick),
    gl:popMatrix(),
    gl:popName(),
    gl:frontFace(?GL_CCW),
    D.

draw_1(#dlo{ns=Ns0,src_we=#we{perm=Perm}=We})
  when ?IS_SELECTABLE(Perm) ->
    Ns = wings_we:visible(gb_trees:to_list(Ns0), We),
    gl:pushName(0),
    foreach(fun({Face,Info}) ->
		    gl:loadName(Face),
		    face(Info)
	    end, Ns),
    gl:popName();
draw_1(_) -> ok.

face([_|[A,B,C]]) ->
    gl:'begin'(?GL_TRIANGLES),
    wpc_ogla:tri(A, B, C),
    gl:'end'();
face([_|[A,B,C,D]]) ->
    gl:'begin'(?GL_QUADS),
    wpc_ogla:quad(A, B, C, D),
    gl:'end'();
face({_,Fs,VsPos}) ->
    gl:'begin'(?GL_TRIANGLES),
    wings__du:plain_face(Fs, VsPos),
    gl:'end'().
