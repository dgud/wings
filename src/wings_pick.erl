%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_pick).
-export([event/2,event/3,hilite_event/3, hilite_event/4]).
-export([do_pick/3]).
-export([marquee_pick/3,paint_pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2,reverse/2,sort/1,usort/1,map/2,min/1,
		keysearch/3,member/2,keysort/2]).

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
event(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_PRESSED}, St, _)
        when Mod band (?SHIFT_BITS bor ?CTRL_BITS) =/= 0 ->
    marquee_pick(X, Y, St);
event(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, St, _) ->
    paint_pick(X, Y, St);
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

marquee_pick(X, Y, St)  ->
    Pick = #marquee{ox=X,oy=Y,st=St},
    clear_hilite_marquee_mode(Pick).
paint_pick(X, Y, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    Pick = #marquee{ox=X,oy=Y,st=St0},
	    clear_hilite_marquee_mode(Pick);
	{PickOp,_,St} ->
	    wings_wm:grab_focus(),
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

handle_hilite_event(redraw,#hl{redraw=#st{sel=[]}=St,prev={SelMode,Where,{Obj,Elem}}}=Hl) ->
    Mode = case SelMode of
      vertex -> ?__(4,"Vertex");
      edge -> ?__(5,"Edge");
      face -> ?__(6,"Face");
      body -> none
    end,
    Info = case Where of
      original ->
        case SelMode of
          body ->
            wings_util:format("~s #~p", [?__(3,"Object"),Obj]);
          _Other ->
            enhanced_hl_info(wings_util:format("~s #~p, ~s #~p",
               [?__(3,"Object"),Obj,Mode,Elem]),Hl)
        end;
      mirror ->
        case SelMode of
          body ->
            wings_util:format("~s #~p ~s", [?__(3,"Object"),
              Obj,?__(2,"(in mirror)")]);
          _Other ->
            enhanced_hl_info(wings_util:format("~s #~p, ~s #~p ~s",
            [?__(3,"Object"),Obj,Mode,Elem,?__(2,"(in mirror)")]),Hl)
        end
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
    gl:vertex3fv(array:get(V, Vtab)),
    gl:'end'();
hilit_draw_sel(edge, Edge, #dlo{src_we=#we{es=Etab,vp=Vtab}}) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    gl:lineWidth(wings_pref:get_value(selected_edge_width)),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(array:get(Va, Vtab)),
    gl:vertex3fv(array:get(Vb, Vtab)),
    gl:'end'();
hilit_draw_sel(face, Face, #dlo{vab=#vab{face_map=Map, face_vs=Vs}}) ->
    case wings_pref:get_value(selection_style) of
	stippled -> gl:enable(?GL_POLYGON_STIPPLE);
	solid -> ok
    end,
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_setup:enableVertexPointer(Vs),
    {Start,NoElements} = array:get(Face,Map),
    gl:drawArrays(?GL_TRIANGLES, Start, NoElements),
    wings_draw_setup:disableVertexPointer(Vs),
    gl:disable(?GL_POLYGON_STIPPLE);
hilit_draw_sel(body, _, #dlo{vab=#vab{face_vs=Vs}}=D) ->
    case wings_pref:get_value(selection_style) of
	stippled -> gl:enable(?GL_POLYGON_STIPPLE);
	solid -> ok
    end,
    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
    wings_draw_setup:enableVertexPointer(Vs),
    Count = wings_draw_setup:face_vertex_count(D),
    gl:drawArrays(?GL_TRIANGLES, 0, Count),
    wings_draw_setup:disableVertexPointer(Vs),
    gl:disable(?GL_POLYGON_STIPPLE).

enhanced_hl_info(Base,#hl{redraw=#st{sel=[],shapes=Shs},prev={SelMode,_,{Obj,Elem}}})->
    case wings_pref:get_value(info_text_on_hilite) of
      true ->
        We = gb_trees:get(Obj, Shs),
        case SelMode of
          vertex ->
            {X,Y,Z} = wings_vertex:pos(Elem, We),
            [Base|io_lib:format(?__(1,". Position <~s  ~s  ~s>"),
                               [wings_util:nice_float(X),
                                wings_util:nice_float(Y),
                                wings_util:nice_float(Z)])];
          edge -> 
            #edge{vs=Va,ve=Vb} = array:get(Elem, We#we.es),
            {Xa,Ya,Za} = wings_vertex:pos(Va, We),
            {Xb,Yb,Zb} = wings_vertex:pos(Vb, We),
            Length = e3d_vec:dist({Xa,Ya,Za}, {Xb,Yb,Zb}),
            {X,Y,Z} = e3d_vec:average({Xa,Ya,Za}, {Xb,Yb,Zb}),
            [Base|io_lib:format(?__(3,". Midpoint <~s  ~s  ~s>\nLength ~s") ++
                                "  <~s  ~s  ~s>", %++ "\nVa = ~p\nVb = ~p",
                                [wings_util:nice_float(X),
                                 wings_util:nice_float(Y),
                                 wings_util:nice_float(Z),
                                 wings_util:nice_float(Length),
                                 wings_util:nice_float(abs(Xb - Xa)),
                                 wings_util:nice_float(abs(Yb - Ya)),
                                 wings_util:nice_float(abs(Zb - Za))])];
								% Va,Vb])];
          face ->
            {X,Y,Z} = wings_face:center(Elem, We),
            Area = area_info(Elem, We),
            Mat = wings_facemat:face(Elem, We),
            [Base|io_lib:format(?__(4,". Midpoint <~s  ~s  ~s> \nMaterial ~s.")
                                ++ Area,
                                [wings_util:nice_float(X),
                                 wings_util:nice_float(Y),
                                 wings_util:nice_float(Z),
                                 Mat])]
         end;
      false ->
        Base
    end.

area_info(Face, We) ->
    case wings_face:vertices(Face,We) =< 50 of
      true -> A = wings_face:area(Face, We),
              wings_util:format(?__(40," Area ~s"), [wings_util:nice_float(A)]);
      false -> []
    end.

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
marquee_event(#mousebutton{x=X0,y=Y0,mod=Mod,button=B,state=?SDL_RELEASED}, M)
        when B==1;B==2 ->
    %% Button 2 is only used in Tweak Mode for Maya cam. If there are any
    %% issues I will revert the code to only accept release states from B 1.
    %% - Richard
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
		   not is_inside_rect(array:get(V, Vtab), Rect)],
    Kill1 = sofs:set(Kill0, [[face]]),
    Kill = sofs:union(Kill1),
    Faces1 = sofs:from_external(Faces0, [face]),
    Faces = sofs:difference(Faces1, Kill),
    sofs:to_external(Faces);
marquee_convert_1(Faces, vertex, Rect, #we{vp=Vtab}=We) ->
    Vs = wings_face:to_vertices(Faces, We),
    [V || V <- Vs, is_inside_rect(array:get(V, Vtab), Rect)];
marquee_convert_1(Faces, edge, Rect, #we{vp=Vtab}=We) ->
    Es0 = wings_face:fold_faces(fun(_, _, E, Rec, A) ->
					[{E,Rec}|A]
				end, [], Faces, We),
    Es = ordsets:from_list(Es0),
    [E || {E,#edge{vs=Va,ve=Vb}} <- Es,
	  is_all_inside_rect([array:get(Va, Vtab),array:get(Vb, Vtab)], Rect)];
marquee_convert_1(_Faces, body, Rect, #we{vp=Vtab}) ->
    case is_all_inside_rect(array:sparse_to_list(Vtab), Rect) of
	true -> [0];
	false -> []
    end.

is_all_inside_rect([P|Ps], Rect) ->
    is_inside_rect(P, Rect) andalso is_all_inside_rect(Ps, Rect);
is_all_inside_rect([], _Rect) -> true.

is_inside_rect({Px,Py,Pz}, {MM,PM,ViewPort,X1,Y1,X2,Y2}) ->
    {Sx,Sy,_} = wings_gl:project(Px, Py, Pz, MM, PM, ViewPort),
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
marquee_update_sel(Op, Hits0, #st{selmode=Mode}=St) ->
    Hits1 = wings_util:rel2fam(Hits0),
    Hits2 = [{Id,begin
		     Items1 = gb_sets:from_ordset(Items0),
		     Items = expand_light_items(Mode, Id, Items1, St),
		     gb_sets:to_list(Items)
		 end} || {Id,Items0} <- Hits1],
    Hits = sofs:from_external(Hits2, [{id,[data]}]),
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
pick_event(#mousebutton{button=B,state=?SDL_RELEASED}, #pick{st=St})
        when B==1;B==2 ->
    %% Button 2 is only used in Tweak Mode for Maya cam. If there are any
    %% issues I will revert the code to only accept release states from B 1.
    %% - Richard
    wings_wm:release_focus(),
    wings_wm:later({new_state,St}),
    pop;
pick_event(_, _) -> keep.

%% do_pick(X, Y, St0) -> {add|delete,{Id,Item,original|mirror},St}.
%%  Pick the item (body, face, edge, or vertex, depending on the selection
%%  mode in St0) and either add it to the selection or delete it from
%%  the selection.
%%
%%  The first element in the returned tuple will indicate whether
%%  something was added or deleted from the selection, and the second
%%  element will indicate which item in which object was picked.
%%
%%  NOTE: Lights will only be selected in their entirety.
%%  So in face mode, for instance, a light will have either all of
%%  its faces selected or none.
%%
-type vm_mirror_side() :: 'original'|'mirror'.
-spec do_pick(non_neg_integer(), non_neg_integer(), #st{}) ->
    'none' | {'add'|'delete',{non_neg_integer(),non_neg_integer(),
			      vm_mirror_side()},#st{}}.
do_pick(X, Y, St) ->
    case raw_pick(X, Y, St) of
	none ->
	    none;
	Hit ->
	    update_selection(Hit, St)
    end.

raw_pick(X0, Y0, St) ->
    {W,H} = wings_wm:win_size(),
    X = float(X0),
    Y = H-float(Y0),
    S = 5,
    set_pick_matrix(X, Y, S, S, W, H),
    case wings_wm:lookup_prop(select_backface) of
	{value,true} ->
	    %% Only in AutoUV windows.
	    wpc_pick:cull(false);
	_ ->
	    wpc_pick:cull(true)
    end,
    case dlo_pick(St, true) of
	[] -> none;
	[{Id,Face}] -> convert_hit(Id, Face, X, Y, St)
    end.

set_pick_matrix(X, Y, Xs, Ys, W, H) ->
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    wpc_pick:pick_matrix(X, Y, Xs, Ys, {0,0,W,H}),
    wings_view:projection(),
    wings_view:modelview(),
    set_pick_matrix().

set_pick_matrix() ->
    ModelMatrix = gl:getDoublev(?GL_MODELVIEW_MATRIX),
    ProjMatrix = gl:getDoublev(?GL_PROJECTION_MATRIX),
    wpc_pick:matrix(ModelMatrix, ProjMatrix).

%% update_selection({Mode,MM,{Id,Item}}, St0) ->
%%                 {add|delete,{Id,Item,MM},St0}.
%%    Mode = body|face|edge|vertex
%%    MM = original|mirror
%%  If the item (body/face/edge/vertex) was not selected, add it
%%  to the selection, otherwise delete it from the selection.
%%  The first element in the returned tuple will indicate whether
%%  something was added or deleted from the selection.
%%
%%  NOTE: Lights must only be selected in their entirety.
%%  So in face mode, for instance, a light will have either all of
%%  its faces selected or none.
%%
update_selection({Mode,MM,{Id,Item}}, #st{sel=Sel0}=St) ->
    Items = expand_light_items(Mode, Id, gb_sets:singleton(Item), St),
    {Type,Sel} = update_selection(Id, Items, Sel0, []),
    {Type,{Id,Item,MM},St#st{selmode=Mode,sel=Sel,sh=false}}.

update_selection(Id, Items, [{I,_}=H|T], Acc) when Id > I ->
    update_selection(Id, Items, T, [H|Acc]);
update_selection(Id, Items, [{I,_}|_]=T, Acc) when Id < I ->
    {add,reverse(Acc, [{Id,Items}|T])};
update_selection(Id, Items1, [{_,Items0}|T0], Acc) -> %Id == I
    case gb_sets:is_disjoint(Items1, Items0) of
	true ->
	    %% Add to selection.
	    Items = gb_sets:union(Items0, Items1),
	    {add,reverse(Acc, [{Id,Items}|T0])};
	false ->
	    %% Delete from selection.
	    Items = gb_sets:difference(Items0, Items1),
	    T = case gb_sets:is_empty(Items) of
		    true -> T0;
		    false -> [{Id,Items}|T0]
		end,
	    {delete,reverse(Acc, T)}
    end;
update_selection(Id, Items, [], Acc) ->
    {add,reverse(Acc, [{Id,Items}])}.

expand_light_items(Mode, Id, Items, #st{shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    case ?IS_LIGHT(We) of
	false -> Items;
	true -> wings_sel:get_all_items(Mode, We)
    end.

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
    {Xs,Ys,_}  = wings_gl:project(Px, Py, Pz, ModelMatrix,
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
    {Ww,Wh} = wings_wm:win_size(),
    Y = Wh-Y0,
    set_pick_matrix(X, Y, W, H, Ww, Wh),
    case DrawFaces of
	true ->
	    wpc_pick:cull(true),
	    {dlo_pick(St, false),St};
	false ->
	    wpc_pick:cull(false),
	    {marquee_pick(St),St}
    end.

marquee_pick(#st{selmode=edge}) ->
    PickFun = fun(#we{es=Etab,vp=Vtab,id=Id}=We, Acc) ->
		      Vis = gb_sets:from_ordset(wings_we:visible(We)),
		      EsPos = visible_edges(array:sparse_to_orddict(Etab),
					    Vtab, Vis, []),
		      case wpc_pick:edges(EsPos) of
			  [] ->
			      Acc;
			  Picked -> [{Id,E} || E <- Picked] ++ Acc
		      end
	      end,
    setup_pick_context(PickFun);
marquee_pick(#st{selmode=vertex}) ->
    PickFun = fun(#we{vp=Vtab,id=Id}=We, Acc) ->
		      VsPos = case wings_we:any_hidden(We) of
				  false ->
				      array:sparse_to_orddict(Vtab);
				  true ->
				      Vs = wings_we:visible_vs(We),
				      [{V,array:get(V, Vtab)} || V <- Vs]
			      end,
		      case wpc_pick:vertices(VsPos) of
			  [] ->
			      Acc;
			  Picked ->
			      [{Id,V} || V <- Picked] ++ Acc
		      end
	      end,
    setup_pick_context(PickFun);
marquee_pick(St) -> dlo_pick(St, false).

visible_edges([{Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}|Es], Vtab, Vis, Acc) ->
    case gb_sets:is_member(Lf, Vis) orelse gb_sets:is_member(Rf, Vis) of
	false ->
	    visible_edges(Es, Vtab, Vis, Acc);
	true ->
	    E = {Edge,{array:get(Va, Vtab),array:get(Vb, Vtab)}},
	    visible_edges(Es, Vtab, Vis,  [E|Acc])
    end;
visible_edges([], _, _, Acc) -> Acc.

setup_pick_context(PickFun) ->
    Res = wings_dl:fold(fun(D, Acc) ->
				setup_pick_context_fun(D, PickFun, Acc)
			end, []),
    sort(Res).

setup_pick_context_fun(#dlo{src_we=#we{perm=Perm}}, _PickFun, Acc)
  when ?IS_NOT_SELECTABLE(Perm) ->
    Acc;
setup_pick_context_fun(#dlo{mirror=none,src_we=We}, PickFun, Acc) ->
    PickFun(We, Acc);
setup_pick_context_fun(#dlo{mirror=Matrix,src_we=We}, PickFun, Acc0) ->
    Acc1 = PickFun(We, Acc0),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    set_pick_matrix(),
    wpc_pick:front_face(cw),
    Acc = PickFun(We, Acc1),
    wpc_pick:front_face(ccw),
    gl:popMatrix(),
    set_pick_matrix(),
    Acc.

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

dlo_pick(St, OneHit) ->
    Hits0 = wings_dl:map(fun(D, Acc) ->
				 do_dlo_pick(D, St, OneHit, Acc)
			 end, []),
    case Hits0 of
	{_,Hits} -> Hits;
	_ -> Hits0
    end.

do_dlo_pick(#dlo{src_we=#we{perm=Perm}}=D, _St, _OneHit, Acc)
  when ?IS_NOT_SELECTABLE(Perm) ->
    {D,Acc};
do_dlo_pick(D=#dlo{vab=none}, St, OneHit, Acc) ->
    do_dlo_pick(wings_draw_setup:work(D, St), St, OneHit, Acc);
do_dlo_pick(D=#dlo{vab=#vab{face_vs=none}}, St, OneHit, Acc) ->
    do_dlo_pick(wings_draw_setup:work(D, St), St, OneHit, Acc);
do_dlo_pick(#dlo{mirror=none,src_we=#we{id=Id}}=D, _, OneHit, Acc) ->
    do_dlo_pick_0(Id, D, OneHit, Acc);
do_dlo_pick(#dlo{mirror=Matrix,src_we=#we{id=Id}}=D0, _, OneHit, Acc0) ->
    {D1,Acc1} = do_dlo_pick_0(Id, D0, OneHit, Acc0),
    gl:pushMatrix(),
    gl:multMatrixf(Matrix),
    set_pick_matrix(),
    wpc_pick:front_face(cw),
    {D,Acc} = do_dlo_pick_0(-Id, D1, OneHit, Acc1),
    wpc_pick:front_face(ccw),
    gl:popMatrix(),
    set_pick_matrix(),
    {D,Acc}.

do_dlo_pick_0(Id, #dlo{vab=#vab{face_vs=Vs,face_map=Map0}}=D0, OneHit, Acc0) ->
    case wpc_pick:faces(Vs, OneHit) of
	[] ->
	    {D0,Acc0};
	{Hit0,Depth} ->
	    case Acc0 of
		{PrevDepth,_} when PrevDepth < Depth ->
		    {D0,Acc0};
		_ ->
		    {D,Map} = dlo_tri_map(D0, Map0),
		    Acc = do_dlo_pick_1([Hit0], Map, Id, []),
		    {D,{Depth,Acc}}
	    end;
	RawHits ->
	    {D,Map} = dlo_tri_map(D0, Map0),
	    {D,usort(do_dlo_pick_1(RawHits, Map, Id, Acc0))}
    end.

do_dlo_pick_1([H|Hits], [{Face,{Start,Num}}|_]=T, Id, Acc)
  when Start =< H, H < Start+Num ->
    do_dlo_pick_1(Hits, T, Id, [{Id,Face}|Acc]);
do_dlo_pick_1([_|_]=Hits, [_|T], Id, Acc) ->
    do_dlo_pick_1(Hits, T, Id, Acc);
do_dlo_pick_1([], [_|_], _, Acc) -> Acc;
do_dlo_pick_1([], [], _, Acc) -> Acc.

dlo_tri_map(#dlo{tri_map=none}=D, Map0) ->
    Map1 = keysort(2, array:sparse_to_orddict(Map0)),
    {D#dlo{tri_map=Map1},Map1};
dlo_tri_map(#dlo{tri_map=TriMap}=D, _) -> {D,TriMap}.
