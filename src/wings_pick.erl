%%
%%  wings_pick.erl --
%%
%%     This module handles picking using OpenGL.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_pick).
-export([event/2,event/3,hilite_event/3, hilite_event/4]).
-export([do_pick/3,raw_pick/3]).
-export([paint_pick/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-include("e3d.hrl").

-import(lists, [foreach/2,reverse/2,sort/1,usort/1,map/2,min/1,
		keyfind/3,member/2,keysort/2]).

%% For ordinary picking.
-record(pick,
	{st,					%Saved state.
	 op					%Operation: add/delete
	}).

%% For marquee picking.
-record(marquee,
	{o, 					% start pos
	 overlay,				% overlay win
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
    setup_marquee(X, Y, St);
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

hilite_enabled(body) -> wings_pref:get_value(body_hilite);
hilite_enabled(_) -> true.

paint_pick(X, Y, St0) ->
    case do_pick(X, Y, St0) of
	none ->
	    setup_marquee(X, Y, St0);
	{PickOp,_,St} ->
	    wings_wm:grab_focus(),
	    wings_wm:dirty(),
	    wings_draw:refresh_dlists(St),
	    Pick = #pick{st=St,op=PickOp},
	    {seq,push,get_pick_event(Pick)}
    end.

setup_marquee(X,Y, St) ->
    OL = wings_frame:get_overlay(),
    Pick = #marquee{o=wings_wm:local2screen({X,Y}),st=St, overlay=OL},
    clear_hilite_marquee_mode(Pick).

%%
%% Highlighting on mouse move.
%%
get_hilite_event(HL) ->
    fun(Ev) -> handle_hilite_event(Ev, HL) end.

handle_hilite_event(redraw,#hl{redraw=#st{sel=[]}=St,prev=Prev}=Hl) when is_tuple(Prev) ->
    case Prev of
      {_,_} ->
        {{SelMode,Where,{Obj,Elem}},_} = Prev;
      _ -> {SelMode,Where,{Obj,Elem}} = Prev
    end,
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
handle_hilite_event(#mousemotion{x=X,y=Y}, #hl{prev={_,_}=PH}=HL0) ->
    #hl{always_dirty=Dirty,st=St,filter=Accept}=HL0,
    case {raw_pick(X, Y, St),tweak_hilite(X,Y,St)} of
	PH when Dirty ->
	    wings_wm:dirty(),
	    get_hilite_event(HL0);
	PH ->
	    wings_draw:refresh_dlists(St),
	    get_hilite_event(HL0);
	{none,none} ->
	    wings_wm:dirty(),
	    insert_hilite_dl(none, St),
	    wings_draw:refresh_dlists(St),
	    get_hilite_event(HL0#hl{prev=none});
	{SelHit,PointHit}=Hit ->
	    case accept_hl(Accept, PointHit) of
		true ->
		    wings_wm:dirty(),
		    case tweak_vector() of
		      true ->
		        insert_tweak_vector(SelHit, PointHit, St),
		        HL = HL0#hl{prev=Hit};
		      _ ->
		        insert_hilite_dl(SelHit, St),
		        HL = HL0#hl{prev=SelHit}
		    end,
		    wings_draw:refresh_dlists(St),
		    get_hilite_event(HL);
		false ->
		    wings_wm:dirty(),
		    insert_hilite_dl(none, St),
		    wings_draw:refresh_dlists(St),
		    get_hilite_event(HL0#hl{prev=none})
		end
    end;
handle_hilite_event(#mousemotion{x=X,y=Y}, #hl{prev=PrevHit}=HL0) ->
    #hl{always_dirty=Dirty,st=St,filter=Accept}=HL0,
    case raw_pick(X, Y, St) of
	PrevHit when Dirty ->
	    wings_wm:dirty(),
	    get_hilite_event(HL0);
	PrevHit ->
	    case tweak_vector() of
	      true ->
	        case tweak_hilite(X,Y,St) of
	          none ->
	            HL = HL0;
	          Hit ->
	            wings_wm:dirty(),
	            insert_tweak_vector(PrevHit, Hit, St),
	            HL = HL0#hl{prev={PrevHit,Hit}}
	        end;
	      _ ->
	        HL = HL0
	    end,
	    wings_draw:refresh_dlists(St),
	    get_hilite_event(HL);
	none ->
	    wings_wm:dirty(),
	    insert_hilite_dl(none, St),
	    wings_draw:refresh_dlists(St),
	    get_hilite_event(HL0#hl{prev=none});
	Hit ->
	    case accept_hl(Accept, Hit) of
	    true ->
	        wings_wm:dirty(),
	        case tweak_vector() of
	          true ->
	            Hit0 = tweak_hilite(X, Y, St),
	            insert_tweak_vector(Hit, Hit0, St);
	          _ ->
	            insert_hilite_dl(Hit, St)
	        end,
	        wings_draw:refresh_dlists(St),
	        get_hilite_event(HL0#hl{prev=Hit});
	    false ->
	        wings_wm:dirty(),
	        insert_hilite_dl(none, St),
	        wings_draw:refresh_dlists(St),
	        get_hilite_event(HL0#hl{prev=none})
	    end
    end;
handle_hilite_event(init_opengl, #hl{st=St}) ->
    wings:init_opengl(St);
handle_hilite_event(_, _) ->
    insert_hilite_dl(none, none),
    next.

tweak_hilite(X, Y, St) ->
    Stp = St#st{selmode=face,sel=[],sh=true},
    raw_pick(X, Y, Stp).

accept_hl(Fun, Hit) when is_function(Fun) ->
    Fun(Hit);
accept_hl(_,_) -> true.

insert_hilite_dl(Hit, St) ->
    Extra = fun(_) -> [] end,
    insert_hilite_dl(Hit, Extra, St).

insert_hilite_dl(Hit, Extra, St) ->
    wings_dl:map(fun(D, _) ->
			 insert_hilite_dl_1(D, Hit, Extra, St)
		 end, []).

insert_hilite_dl_1(#dlo{src_we=We}=D, _, _, _) when ?IS_LIGHT(We) -> D;
insert_hilite_dl_1(#dlo{open=Open,src_we=#we{id=Id}=We}=D,
		   {Mode,_,{Id,Item}=Hit}, Extra, St) ->
    DrawExtra = Extra(We),
    HiliteColor = hilite_color(Hit, St),
    DrawHilite = hilite_draw_sel_fun(Mode, Item, D),
    Draw0 = [fun(RS) -> gl:color3fv(HiliteColor), RS end,
	     DrawHilite,DrawExtra],
    ShowBack = wings_pref:get_value(show_backfaces),
    SelBack = wings_wm:lookup_prop(select_backface) =:= {value,true},
    Draw = if
	       SelBack; ShowBack andalso Open ->
		   [fun(RS) -> gl:disable(?GL_CULL_FACE), RS end,
		    Draw0,
		    fun(RS) -> gl:enable(?GL_CULL_FACE),RS end];
	       true ->
		   Draw0
	   end,
    D#dlo{hilite={Mode, Draw}};
insert_hilite_dl_1(#dlo{hilite=none}=D, _, _, _) -> D;
insert_hilite_dl_1(D, _, _, _) -> D#dlo{hilite=none}.

tweak_vector() ->
    Draw = wings_wm:lookup_prop(tweak_draw) =:= {value,true},
    case wings_pref:get_value(tweak_active) of
      true when Draw ->
        case wings_wm:is_geom() of
          true ->
            case wings_pref:get_value(tweak_point) of
              from_element -> true;
              _ ->
                case wings_pref:get_value(tweak_axis) of
                  element_normal -> true;
                  element_normal_edge -> true;
                  _ -> false
                end
            end;
          false -> false
        end;
      _other -> false
    end.

insert_tweak_vector(Hit, {Mode,_,{_,Item}}, St) ->
    UseEdgeNormal = wings_pref:get_value(tweak_axis) =:=
	element_normal_edge,
    F = fun(We) ->
		{Normal,EdgeNormal,Center} =
		    wings_tweak:point_center(Mode, Item, We),
		N = case UseEdgeNormal of
			true -> EdgeNormal;
			false -> Normal
		    end,
		draw_tweak_vector_fun(Center, N)
	end,
    insert_hilite_dl(Hit, F, St).

hilite_color({Id,Item}, #st{sel=Sel}) ->
    Key = case keyfind(Id, 1, Sel) of
	      false -> unselected_hlite;
	      {Id,Items} ->
		  case gb_sets:is_member(Item, Items) of
		      false -> unselected_hlite;
		      true -> selected_hlite
		  end
	  end,
    wings_pref:get_value(Key).

draw_tweak_vector_fun(Center, Normal) ->
    Length = wings_pref:get_value(tweak_vector_size),
    Width = wings_pref:get_value(tweak_vector_width),
    Color = wings_pref:get_value(tweak_vector_color),
    Point = e3d_vec:add_prod(Center, Normal, Length),
    Data = [Center,Point],
    D = fun(RS) ->
		gl:lineWidth(Width),
		gl:pointSize(Width+2),
		gl:color3fv(Color),
		gl:drawArrays(?GL_LINES, 0, 2),
		gl:drawArrays(?GL_POINTS, 1, 1),
                RS
	end,
    wings_vbo:new(D, Data).

hilite_draw_sel_fun(vertex, V, #dlo{src_we=#we{vp=Vtab}}) ->
    PointSize = wings_pref:get_value(selected_vertex_size),
    Data = [array:get(V, Vtab)],
    D = fun(RS) ->
		gl:pointSize(PointSize),
		gl:drawArrays(?GL_POINTS, 0, 1),
                RS
	end,
    wings_vbo:new(D, Data);
hilite_draw_sel_fun(edge, Edge, #dlo{src_we=#we{es=Etab,vp=Vtab}}) ->
    #edge{vs=Va,ve=Vb} = array:get(Edge, Etab),
    LineWidth = wings_pref:get_value(selected_edge_width),
    Data = [array:get(Va, Vtab),array:get(Vb, Vtab)],
    D = fun(RS) ->
		gl:lineWidth(LineWidth),
		gl:drawArrays(?GL_LINES, 0, 2),
                RS
	end,
    wings_vbo:new(D, Data);
hilite_draw_sel_fun(face, Face, #dlo{vab=#vab{face_map=Map}=Vab}) ->
    {Start,NoElements} = array:get(Face, Map),
    fun(RS0) ->
	    gl:enable(?GL_POLYGON_STIPPLE),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    RS = wings_draw_setup:enable_pointers(Vab, [], RS0),
	    gl:drawArrays(?GL_TRIANGLES, Start, NoElements),
	    gl:disable(?GL_POLYGON_STIPPLE),
            wings_draw_setup:disable_pointers(Vab, RS)
    end;
hilite_draw_sel_fun(body, _, #dlo{vab=#vab{}=Vab}=D) ->
    fun(RS0) ->
	    gl:enable(?GL_POLYGON_STIPPLE),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_FILL),
	    RS = wings_draw_setup:enable_pointers(Vab, [], RS0),
	    Count = wings_draw_setup:face_vertex_count(D),
	    gl:drawArrays(?GL_TRIANGLES, 0, Count),
	    gl:disable(?GL_POLYGON_STIPPLE),
            wings_draw_setup:disable_pointers(Vab, RS)
    end.

enhanced_hl_info(Base,#hl{redraw=#st{sel=[],shapes=Shs},prev=Prev}) when is_tuple(Prev) ->
    case Prev of
      {_,_} ->
        {{SelMode,_,{Obj,Elem}},_} = Prev;
      _ -> {SelMode,_,{Obj,Elem}} = Prev
    end,
    %% Mouseover info text for temp hilited selection
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
    marquee_message(),
    wings_wm:dirty(),
    {seq,push,
     fun(redraw) ->
	     wings:redraw(St),
	     wings_wm:later(now_enter_marquee_mode),
	     keep;
	(now_enter_marquee_mode) ->
	     wings_io:grab(),
	     wings_wm:grab_focus(wings_wm:this()),
	     get_marquee_event(Pick);
	(Ev) ->
	     wings_io:putback_event(Ev),
	     keep
     end}.

marquee_message() ->
    Ctrl = wings_s:key(ctrl),
    Shift = wings_s:key(shift),
    CtrlMsg = ?__(ctrl_action,"Deselect"),
    ShiftMsg = ?__(shift_action,
		   "(De)select only elements wholly inside marquee"),
    Mctrl = wings_util:key_format(Ctrl, CtrlMsg),
    Mshift = wings_util:key_format(Shift, ShiftMsg),
    Message = wings_msg:join(Mctrl, Mshift),
    wings_wm:message(Message).

get_marquee_event(Pick) ->
    {replace,fun(Ev) -> marquee_event(Ev, Pick) end}.

marquee_event(redraw, #marquee{st=St}) ->
    wings:redraw(St),
    keep;
marquee_event(#mousemotion{x=X0,y=Y0}, #marquee{o={OX,OY}, overlay=OL}) ->
    {X,Y} = wings_wm:local2screen({X0,Y0}),
    MinX = min(X,OX),
    MinY = min(Y,OY),
    W = max(X,OX) - MinX,
    H = max(Y,OY) - MinY,
    wings_frame:overlay_draw(OL, {MinX,MinY,W,H}, 170),
    keep;
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
    #marquee{o=Orig,st=St0,overlay=OL} = M,
    {Ox,Oy} = wings_wm:screen2local(Orig),
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
    wings_frame:overlay_hide(OL),
    wings_io:ungrab(X0,Y0),
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
pick_event(#mousebutton{button=1,state=?SDL_RELEASED}, #pick{st=St}) ->
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
    S = 5.0,
    Ms = set_pick_matrix(X, Y, S, S, W, H),
    case wings_wm:lookup_prop(select_backface) of
	{value,true} ->
	    %% Only in AutoUV windows.
	    wpc_pick:cull(false);
	_ ->
	    wpc_pick:cull(true)
    end,
    case dlo_pick(St, true, Ms) of
	[] -> none;
	[{Id,Face}] -> convert_hit(Id, Face, X, Y, St)
    end.

set_pick_matrix(X, Y, Xs, Ys, W, H) ->
    Pick = wpc_pick:pick_matrix(X, Y, Xs, Ys, {0,0,W,H}),
    ProjMatrix = wings_view:projection(Pick),
    {_, ModelMatrix} = wings_view:modelview(),
    set_pick_matrix_1(e3d_transform:matrix(ProjMatrix),
		      e3d_transform:matrix(ModelMatrix)).

set_pick_matrix({PM, MM}) ->
    set_pick_matrix_1(PM, MM).

set_pick_matrix({PM, MM}, PostModel) ->
    set_pick_matrix_1(PM, e3d_mat:mul(MM, PostModel)).

set_pick_matrix_1(ProjMatrix, ModelMatrix) ->
    wpc_pick:matrix(ModelMatrix, ProjMatrix),
    {ProjMatrix, ModelMatrix}.

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
    Ms = set_pick_matrix(X, Y, W, H, Ww, Wh),
    case DrawFaces of
	true ->
	    wpc_pick:cull(true),
	    {dlo_pick(St, false, Ms),St};
	false ->
	    wpc_pick:cull(false),
	    {marquee_pick(St, Ms),St}
    end.

marquee_pick(#st{selmode=edge}, Ms) ->
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
    setup_pick_context(PickFun, Ms);
marquee_pick(#st{selmode=vertex}, Ms) ->
    PickFun = fun(#we{vp=Vtab,id=Id}=We, Acc) ->
		      VsPos = case wings_we:is_open(We) of
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
    setup_pick_context(PickFun, Ms);
marquee_pick(St, Ms) -> dlo_pick(St, false, Ms).

visible_edges([{Edge,#edge{vs=Va,ve=Vb,lf=Lf,rf=Rf}}|Es], Vtab, Vis, Acc) ->
    case gb_sets:is_member(Lf, Vis) orelse gb_sets:is_member(Rf, Vis) of
	false ->
	    visible_edges(Es, Vtab, Vis, Acc);
	true ->
	    E = {Edge,{array:get(Va, Vtab),array:get(Vb, Vtab)}},
	    visible_edges(Es, Vtab, Vis,  [E|Acc])
    end;
visible_edges([], _, _, Acc) -> Acc.

setup_pick_context(PickFun, Ms) ->
    Res = wings_dl:fold(fun(D, Acc) ->
				setup_pick_context_fun(D, PickFun, Ms, Acc)
			end, []),
    sort(Res).

setup_pick_context_fun(#dlo{src_we=#we{perm=Perm}}, _PickFun, _, Acc)
  when ?IS_NOT_SELECTABLE(Perm) ->
    Acc;
setup_pick_context_fun(#dlo{mirror=none,src_we=We}, PickFun, _, Acc) ->
    PickFun(We, Acc);
setup_pick_context_fun(#dlo{mirror=Mirror,src_we=We}, PickFun, Ms, Acc0) ->
    Acc1 = PickFun(We, Acc0),
    set_pick_matrix(Ms, Mirror),
    wpc_pick:front_face(cw),
    Acc = PickFun(We, Acc1),
    wpc_pick:front_face(ccw),
    set_pick_matrix(Ms),
    Acc.

%%
%% Draw for the purpose of picking the items that the user clicked on.
%%

dlo_pick(St, OneHit, Matrices) ->
    Hits0 = wings_dl:map(fun(D, Acc) ->
				 do_dlo_pick(D, St, OneHit, Matrices, Acc)
			 end, []),
    case Hits0 of
	{_,Hits} ->
	    %% Only one hit.
	    Hits;
	_ ->
	    %% All hits.
	    usort(Hits0)
    end.

do_dlo_pick(#dlo{src_we=#we{perm=Perm}}=D, _St, _OneHit, _Ms, Acc)
  when ?IS_NOT_SELECTABLE(Perm) ->
    {D,Acc};
do_dlo_pick(D=#dlo{vab=none}, St, OneHit, Ms, Acc) ->
    do_dlo_pick(wings_draw_setup:work(D, St), St, OneHit, Ms, Acc);
do_dlo_pick(D=#dlo{vab=#vab{face_vs=none}}, St, OneHit, Ms, Acc) ->
    do_dlo_pick(wings_draw_setup:work(D, St), St, OneHit, Ms, Acc);
do_dlo_pick(#dlo{mirror=none,src_we=#we{id=Id}=We}=D, _, OneHit, _Ms, Acc)
  when ?IS_AREA_LIGHT(We) ->
    wpc_pick:cull(false),
    Res = do_dlo_pick_0(Id, D, OneHit, Acc),
    wpc_pick:cull(true),
    Res;
do_dlo_pick(#dlo{mirror=none,open=Open,src_we=#we{id=Id}}=D, _, OneHit, _Ms, Acc) ->
    case wings_pref:get_value(show_backfaces) of
        true when Open -> wpc_pick:front_face(cw);
        _ -> wpc_pick:front_face(ccw)
    end,
    do_dlo_pick_0(Id, D, OneHit, Acc);
do_dlo_pick(#dlo{mirror=Matrix,open=Open,src_we=#we{id=Id}}=D0, _, OneHit, Ms, Acc0) ->
    case wings_pref:get_value(show_backfaces) of
        true when Open ->
            wpc_pick:front_face(cw),
            {D1,Acc1} = do_dlo_pick_0(Id, D0, OneHit, Acc0);
        _ ->
            {D1,Acc1} = do_dlo_pick_0(Id, D0, OneHit, Acc0),
            wpc_pick:front_face(cw)
    end,
    set_pick_matrix(Ms, Matrix),
    {D,Acc} = do_dlo_pick_0(-Id, D1, OneHit, Acc1),
    wpc_pick:front_face(ccw),
    set_pick_matrix(Ms),
    {D,Acc}.

do_dlo_pick_0(Id, #dlo{vab=#vab{data=VsBin,face_vs={Stride,_},face_map=Map0}}=D0,
	      OneHit, Acc0) ->
    Vs = {Stride,VsBin},
    case wpc_pick:faces(Vs, OneHit) of
	[] ->
	    %% No hit.
	    {D0,Acc0};
	{Hit0,Depth} ->
	    %% OneHit is true - only the nearest hit is returned.
	    case Acc0 of
		{PrevDepth,_} when PrevDepth < Depth ->
		    {D0,Acc0};
		_ ->
		    {D,Map} = dlo_tri_map(D0, Map0),
		    Acc = do_dlo_pick_1([Hit0], Map, Id, []),
		    {D,{Depth,Acc}}
	    end;
	RawHits ->
	    %% OneHit is false - all hits are returned.
	    {D,Map} = dlo_tri_map(D0, Map0),
	    {D,do_dlo_pick_1(RawHits, Map, Id, Acc0)}
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
