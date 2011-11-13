%%
%%  wings_vec.erl --
%%
%%     This module implements "vectors" and the secondary selection mode.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_vec).

-export([init/0,do_ask/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1,member/2,last/1]).

-record(ss, {f,					%Fun.
	     selmodes,				%Legal selection modes.
	     is_axis=false,			%True if axis.
	     info="",				%Info message.
	     vec=none,				%Current vector.
	     alt_vec=none,			%Alternative vector (no mirror).
	     sw_msg=none,			%Message for switching.
	     cb,				%Callback
	     mag=false,				%Magnet possible or not.
	     new_st				%St to be used during
						%command execution.
	    }).

%% Keep the display for the current vector.
-record(vec,
	{vec,					%Display list for vector.
	 src_vec=undefined			%Source for vector.
	}).

init() ->
    DefAxis = {{0.0,0.0,0.0},{1.0,0.0,0.0}},
    wings_pref:set_default(last_axis, DefAxis),
    wings_pref:set_default(default_axis, DefAxis),
    wings_pref:set_default(magnet_type, dome),
    wings_pref:set_default(magnet_distance_route, shortest),
    wings_pref:set_default(magnet_radius, 1.0).

%% Call wings:ask/3; DO NOT call this function directly.
do_ask({Do,Done}, St, Cb) ->
    Modes = [vertex,edge,face],
    do_ask_1(Modes, Do, Done, [], St, Cb);
do_ask({Do,Done,Flags}, St, Cb) ->
    Modes = [vertex,edge,face],
    do_ask_1(Modes, Do, Done, Flags, St, Cb);
do_ask({Do,Done,Flags,Modes}, St, Cb) ->
    do_ask_1(Modes, Do, Done, Flags, St, Cb).

do_ask_1(_, [], Res, _, St, Cb) ->
    wings_wm:later(build_result(Res, Cb, St)),
    keep;
do_ask_1(Modes, Do0, Done, Flags, NewSt, Cb) ->
    wings_menu:kill_menus(),
    Do = add_help_text(Do0),
    Mag = member(magnet, Flags),
    Ss = #ss{cb=Cb,mag=Mag,selmodes=Modes,new_st=NewSt,
	     f=fun(_, _) -> keep end},
    wings_wm:later({ask_init,Do,Done}),
    wings_tweak:toggle_draw(false),
    erase_vector(),
    St = wings_wm:get_current_state(),
    {seq,push,get_event(Ss, St)}.

add_help_text([{_,_}=Pair|T]) ->
    [Pair|add_help_text(T)];
add_help_text([Type|T]) ->
    Val = {Type,
	   case Type of
	       axis_point ->?__(1,"Pick axis (and point)");
	       axis -> ?__(2,"Pick axis");
	       point -> ?__(3,"Pick point");
	       magnet -> ?__(4,"Pick outer boundary point for magnet influence");
	       _ -> []
	   end},
    [Val|add_help_text(T)];
add_help_text([]) -> [].

magnet_possible_now(_, #ss{mag=false}) -> no;
magnet_possible_now([], _) -> inactive;
magnet_possible_now(Pl, _) ->
    case last(Pl) of
	{magnet,_} -> active;
	_ -> inactive
    end.

common_message(Msg, More, MagnetPossible, Right) ->
    Rmb = case More of
	      [] -> ?__(1,"Execute");
	      [_|_] ->?__(2,"Continue")
	  end,
    Message = wings_msg:join(wings_msg:button_format(Msg, [], Rmb),
			     common_magnet_message(MagnetPossible)),
    wings_wm:message(Message, Right).

common_magnet_message(no) -> [];
common_magnet_message(inactive) ->
    [$\s,wings_msg:rmb_format(?__(1,"Magnet"))];
common_magnet_message(active) ->
    ["  "|wings_magnet:info_string()].

magnet_message(Msg, Right) ->
    Message = wings_msg:join(wings_msg:button_format(Msg, [], ?__(1,"Execute ")),
			     wings_msg:rmb_format(?__(2,"Magnet options"))),
    wings_wm:message(Message, Right).

mode_restriction(Modes, #st{selmode=Mode}=St) ->
    wings:mode_restriction(Modes),
    case member(Mode, Modes) of
	true -> St;
	false -> St#st{selmode=last(Modes)}
    end.

pick_finish() ->
    wings_wm:dirty(),
    wings_dl:map(fun clear_orig_sel/2, []).

clear_orig_sel(D, _) -> D#dlo{sel=none,orig_sel=none,orig_mode=none}.

clear_sel() ->
    wings_wm:dirty(),
    wings_dl:map(fun clear_sel/2, []).

clear_sel(D, _) -> D#dlo{sel=none}.

%%%
%%% Event handler for secondary selection mode.
%%%

get_event(Ss, St) ->
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    {replace,fun(Ev) -> handle_event(Ev, Ss, St) end}.

handle_event({ask_init,Do,Done}, #ss{selmodes=Modes}=Ss,
	     #st{selmode=Mode}=St0) ->
    wings_dl:map(fun(#dlo{orig_sel=none,sel=Dlist}=D, _) ->
			 D#dlo{orig_sel=Dlist,orig_mode=Mode}
		 end, []),
    wings_u:menu_restriction(wings_wm:this(), [view,select]),
    St = wings_sel:reset(mode_restriction(Modes, St0)),
    pick_next(Do, Done, Ss, St);
handle_event(Event, Ss, St) ->
    case wings_camera:event(Event, St) of
	next -> handle_event_1(Event, Ss, St);
	Other -> Other
    end.

handle_event_1(Event, Ss, St) ->
    case wings_pick:event(Event, St, fun() -> redraw(Ss, St) end) of
	next -> handle_event_2(Event, Ss, St);
	Other -> Other
    end.

handle_event_2(#mousebutton{x=X,y=Y}=Ev0, Ss, St0) ->
    case wings_menu:is_popup_event(Ev0) of
	{yes,Xglobal,Yglobal,Mod} ->
	    case temp_selection(X, Y, St0) of
		none ->
		    exit_menu(Xglobal, Yglobal, Mod, Ss, St0);
		#st{}=St ->
		    Ev = wings_wm:local2global(Ev0),
		    wings_io:putback_event(Ev),
		    wings_wm:later({new_state,St})
	    end;
	no -> handle_event_3(Ev0, Ss, St0)
    end;
handle_event_2(Ev, Ss, St) -> handle_event_3(Ev, Ss, St).

handle_event_3(#keyboard{}=Ev, Ss, St0) ->
    case handle_key(Ev, Ss, St0) of
	next ->
	    case wings_hotkey:event(Ev, St0) of
		next -> handle_event_4(Ev, Ss, St0);
		{Menu,_}=Act when Menu == view; Menu == select->
		    wings_io:putback_event({action,Act}),
		    keep;
		_Other -> keep
	    end;
	Other -> Other
    end;
handle_event_3(Ev, Ss, St) -> handle_event_4(Ev, Ss, St).

handle_event_4({new_state,St}, #ss{f=Check}=Ss, _St0) ->
    case Check(check, St) of
	{Vec,Msg} -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[{Vec,Msg}] -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[{Vec,{Msg,SwMsg}},{AltVec,_}] ->
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=AltVec,sw_msg=SwMsg}, St)
    end;
handle_event_4({update_state,St}, #ss{f=Check}=Ss, _St0) ->
    case Check(check, St) of
	{Vec,Msg} -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[{Vec,Msg}] -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[{Vec,{Msg,SwMsg}},{AltVec,_}] ->
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=AltVec,sw_msg=SwMsg}, St)
    end;
handle_event_4(redraw, Ss, St) ->
    redraw(Ss, St),
    keep;
handle_event_4({action,{select,Cmd}}, Ss, St0) ->
    case wings_sel_cmd:command(Cmd, St0) of
	#st{}=St0 -> keep;
	#st{}=St -> filter_sel_command(Ss, St);
	{save_state,St} -> filter_sel_command(Ss, St);
	keep -> keep
    end;

handle_event_4({action,{view,auto_rotate}}, _, _) ->
    keep;

%%%% Vector op Highlight Aim
handle_event_4({action,{view,highlight_aim}}, Ss, St0) ->
    {{_,Cmd},St1} = wings:highlight_aim_setup(St0),
    St2 = wings_view:command(Cmd,St1),
    get_event(Ss, St2);
%%%% Temporary Select Aim for View commands with no selection
handle_event_4({action,{view,Cmd}}, Ss, #st{sel=[]}=St0)
  when Cmd == align_to_selection; Cmd == aim; Cmd == frame ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
      {add,_,St} ->
        St1 = wings_view:command(Cmd,St),
        St2 = wings_sel:clear(St1),
        get_event(Ss, St2);
      _Other ->
        St = wings_view:command(Cmd, St0),
        get_event(Ss, St)
    end;
handle_event_4({action,{view,Cmd}}, Ss, St0) ->
    St = wings_view:command(Cmd, St0),
    get_event(Ss, St);
handle_event_4({action,{secondary_selection,abort}}, _, _) ->
    erase_vector(),
    wings_tweak:toggle_draw(true),
    wings_wm:later(revert_state),
    pick_finish(),
    pop;
handle_event_4(quit, _Ss, _St) ->
    erase_vector(),
    wings_tweak:toggle_draw(true),
    wings_io:putback_event(quit),
    pop;
handle_event_4(init_opengl, _, St) ->
    erase_vector(),
    wings:init_opengl(St);
handle_event_4(_Event, Ss, St) ->
    get_event(Ss, St).

temp_selection(X, Y, St0) ->
    case St0 of
	#st{sel=[{_,_}|_]} -> 
	    none;
	_ ->
	    case wings_pick:do_pick(X, Y, St0) of
		{add,_,St} -> St;
		_ -> none
	    end
    end.

pick_next(Do, Done, #ss{is_axis=true,vec={{_,_,_},{_,_,_}}=Vec}=Ss, St) ->
    wings_pref:set_value(last_axis, Vec),
    pick_next_1(Do, Done, Ss, St);
pick_next(Do, Done, Ss, St) -> pick_next_1(Do, Done, Ss, St).

pick_next_1([], Res, #ss{cb=Cb,new_st=NewSt}, _) ->
    wings_tweak:toggle_draw(true),
    pick_finish(),
    wings:clear_mode_restriction(),
    wings_wm:later(build_result(Res, Cb, NewSt)),
    pop;
pick_next_1([{Fun0,Desc}|More], Done, Ss, St) when is_function(Fun0) ->
    Fun = fun(message, Right) -> common_message(Desc, More, no, Right);
	     (A, B) ->
		     case Fun0(A, B) of
		       {result,Res} ->
		           %% the 'result' tag allows plugins to add custom secondary
		           %% selections to the Done accumulator.
		           %% See example in wpc_circularise.
		           {More,[Res|Done]};
		       Other -> Other
		     end
	  end,
    get_event(Ss#ss{f=Fun,is_axis=false,vec=none,info=""}, wings_sel:reset(St));
pick_next_1([{Type,Desc}|More], Done, Ss, St0) ->
    MagnetPossible = magnet_possible_now(More, Ss),
    Check = case Type of
		magnet -> magnet;
		point -> fun check_point/1;
		axis -> fun check_vector/1;
		axis_point -> fun check_vector/1
	    end,
    Fun = case Type of
	      magnet ->
		  fun(check, S) ->
			  check_magnet_point(S);
		     (exit, {Mod,Vec,_}) ->
			  exit_magnet(Vec, Mod, Done);
		     (message, Right) ->
			  magnet_message(Desc, Right)
		  end;
	      _ ->
		  fun(check, S) ->
			  Check(S);
		     (exit, {Mod,Vec,S}) ->
			  common_exit(Type, Vec, Mod, More, Done,
				      MagnetPossible, S);
		     (message, Right) ->
			  common_message(Desc, More, MagnetPossible, Right)
		  end
	  end,
    IsAxis = (Type =/= point andalso Type =/= magnet),
    St = case Type of
	     magnet -> St0#st{selmode=vertex};
	     _ -> St0
	 end,
    clear_sel(),
    get_event(Ss#ss{f=Fun,is_axis=IsAxis,vec=none,info=""}, wings_sel:reset(St)).

redraw(#ss{info=Info,f=Message,vec=Vec}=Ss, St) ->
    Message(message, right_message(Ss)),
    wings:redraw(Info, St),
    gl:pushAttrib(?GL_CURRENT_BIT bor ?GL_ENABLE_BIT bor
		  ?GL_TEXTURE_BIT bor ?GL_POLYGON_BIT bor
		  ?GL_LINE_BIT bor ?GL_COLOR_BUFFER_BIT bor
		  ?GL_LIGHTING_BIT),
    wings_view:load_matrices(false),
    draw_vec(Vec),
    gl:popAttrib(),
    wings_wm:current_state(St).

right_message(#ss{alt_vec=none}) -> [];
right_message(#ss{sw_msg=Msg}) ->
    "[1] " ++ Msg.

filter_sel_command(#ss{selmodes=Modes}=Ss, #st{selmode=Mode}=St) ->
    case member(Mode, Modes) of
	true -> handle_event({new_state,St}, Ss, St);
	false -> keep
    end.

handle_key(#keyboard{sym=$1}, #ss{vec=Vec,alt_vec=Vec}, St) ->
    wings_io:putback_event({new_state,St}),
    keep;
handle_key(#keyboard{sym=$1}, #ss{f=Check}=Ss, St) ->
    case Check(check, St) of
	{Vec,Msg} -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[{Vec,Msg}] -> 
	    get_event(Ss#ss{info=Msg,vec=Vec,alt_vec=none,sw_msg=none}, St);
	[_,{AltVec,{Msg,SwMsg}}] ->
	    get_event(Ss#ss{info=Msg,vec=AltVec,alt_vec=AltVec,sw_msg=SwMsg}, St)
    end;
handle_key(#keyboard{sym=27}, _, _) ->		%Escape
    wings_wm:later({action,{secondary_selection,abort}});
handle_key(_, _, _) -> next.

exit_menu(X, Y, Mod, #ss{f=Exit,vec=Vec}=Ss, St) ->
    case Exit(exit, {Mod,Vec,St}) of
	error ->
	    Menu = [{wings_s:cancel(),abort,
		     ?__(2,"Cancel current command")}],
	    wings_menu:popup_menu(X, Y, secondary_selection, Menu);
	keep ->
	    keep;
	{Do,Done} ->
	    pick_next(Do, Done, Ss, St)
    end.

common_exit(_, none, _, _, _, _, _) ->
    error;
common_exit(Type, Vec, Mod, More, Acc, inactive, _St) ->
    RmbMod = wings_msg:free_rmb_modifier(),
    if
	Mod band RmbMod =:= 0 ->
	    common_exit_1(Type, Vec, More, Acc);
	More =:= [] ->
	    common_exit_1(Type, Vec, add_magnet(More), Acc);
	true ->
	    case last(More) of
		{magnet,_} -> common_exit_1(Type, Vec, More, Acc);
		_  -> common_exit_1(Type, Vec, add_magnet(More), Acc)
	    end
    end;
common_exit(Type, Vec, _, More, Acc, _, _) ->
    common_exit_1(Type, Vec, More, Acc).

common_exit_1(axis, {_,Vec}, PickList, Acc) ->
    {PickList,add_to_acc(Vec, Acc)};
common_exit_1(axis_point, {Point,Vec}, PickList, Acc0) ->
    Acc = add_to_acc(Point, add_to_acc(Vec, Acc0)),
    {PickList,Acc};
common_exit_1(point, Point, PickList, Acc) ->
    {PickList,add_to_acc(Point, Acc)}.

add_magnet(More) ->
    More ++ [{magnet,?__(1,"Pick outer boundary point for magnet influence")}].

add_to_acc(Vec, [radial]) -> [{radial,Vec}];
add_to_acc(Vec, Acc) -> [Vec|Acc].

build_result([Res], Cb, St) ->
    build_result_1(Res, Cb, St);
build_result(Res, Cb, St) ->
    build_result_1(list_to_tuple(reverse(Res)), Cb, St).

build_result_1(Res, Cb, St0) ->
    St = St0#st{ask_args=Res},
    {vec_command,fun() -> Cb(Res, St) end,St}.

%%%
%%% Vector functions.
%%%

check_vector(#st{sel=[]}) -> {none,""};

check_vector(#st{selmode=vertex,sel=[{_Ob1,Sel1},{_Ob2,Sel2},{_Ob3,Sel3}]}=St) ->
    VertNum = gb_sets:size(Sel1) + gb_sets:size(Sel2) + gb_sets:size(Sel3),
    case VertNum of
      3 ->
        PosList = wings_sel:fold(fun(Verts,We,Acc) ->
                        Vs = gb_sets:to_list(Verts),
                        get_pos(Vs,We,Acc)
                        end,[],St),
        Positions = lists:merge(PosList),
        get_vec(vertex,plane,Positions);
      _ ->
        Str = guard_string(),
        {none,Str}
    end;

check_vector(#st{selmode=vertex,sel=[{Id0,Sel0},{Id1,Sel1}],shapes=Shs}=St) ->
    SelSize = gb_sets:size(Sel0) + gb_sets:size(Sel1),
    case SelSize of
      2 ->
        We0 = gb_trees:get(Id0,Shs),
        We1 = gb_trees:get(Id1,Shs),
        [EL0] = gb_sets:to_list(Sel0),
        [EL1] = gb_sets:to_list(Sel1),
        get_vec(vertex, [EL0, EL1], [We0, We1]);
      3 ->
        PosList = wings_sel:fold(fun(Verts,We,Acc) ->
                        Vs = gb_sets:to_list(Verts),
                        get_pos(Vs,We,Acc)
                        end,[],St),
        Positions = lists:merge(PosList),
        get_vec(vertex,plane,Positions);

      _ ->
        Str = guard_string(),
        {none,Str}
    end;
check_vector(#st{selmode=edge,sel=[{Id0,Sel0},{Id1,Sel1}],shapes=Shs}) ->
        We0 = gb_trees:get(Id0,Shs),
        We1 = gb_trees:get(Id1,Shs),
        EL0 = gb_sets:to_list(Sel0),
        EL1 = gb_sets:to_list(Sel1),
        case length(EL0) + length(EL1) of
          2 -> get_vec(edge, EL0 ++ EL1, [We0, We1]);
          _ -> get_vec(edge, [{EL0}, {EL1}], [We0, We1])
        end;

check_vector(#st{selmode=Mode,sel=[{Id0,Sel0},{Id1,Sel1}],shapes=Shs}) ->
    SelSize = gb_sets:size(Sel0) + gb_sets:size(Sel1),
    case SelSize of
      2 ->
        We0 = gb_trees:get(Id0,Shs),
        We1 = gb_trees:get(Id1,Shs),
        [EL0] = gb_sets:to_list(Sel0),
        [EL1] = gb_sets:to_list(Sel1),
        get_vec(Mode, [EL0, EL1], [We0, We1]);
      _ ->
        Str = guard_string(),
        {none,Str}
    end;

check_vector(#st{selmode=Mode,sel=[{Id,Elems0}],shapes=Shs}) ->
    We = gb_trees:get(Id, Shs),
    Elems = gb_sets:to_list(Elems0),
    get_vec(Mode, Elems, We);

check_vector(_) ->
    Str = guard_string(),
    {none,Str}.

guard_string() ->
    ?__(1,"Select parts of one object only") ++
    ?__(2,", or select one element in each of two objects") ++
	?__(4,",\nor a single edge loop on each object ") ++
    ?__(3,", or any three vertices.").

get_pos(Vs,We,Acc) ->
    Positions = lists:foldl(fun(Vert,A) ->
                #we{vp=Vtab} = We,
                Pos = array:get(Vert, Vtab),
                [Pos|A]
                end,[],Vs),
    [Positions|Acc].

%%% Three verts two or three objects
get_vec(vertex,plane,Positions) ->
    case Positions of
        [Vp1,Vp2,Vp3] ->
            Vec0 = e3d_vec:sub(Vp1,Vp2),
            Vec1 = e3d_vec:sub(Vp1,Vp3),
            Vec = e3d_vec:cross(Vec0,Vec1),
            Center = e3d_vec:average(Positions),
            [{{Center,Vec},?__(13,"3-point perp. normal saved as axis.")}];
        _ -> {none,?__(27,"Vertices cannot share coordinates when defining a 3 point plane normal")}
    end;

%%% Two Objects
get_vec(vertex, [Va, Vb], [We0, We1]) ->
    VaPos = wings_vertex:pos(Va, We0),
    VbPos = wings_vertex:pos(Vb, We1),
    Vec = e3d_vec:norm_sub(VaPos, VbPos),
    Center = e3d_vec:average(VaPos, VbPos),
    Normal = e3d_vec:norm(e3d_vec:add(wings_vertex:normal(Va, We0),
              wings_vertex:normal(Vb, We1))),
    [{{Center,Vec},
      {?__(9,"Direction between vertices saved as axis."),
       ?__(10,"Use average of vertex normals as axis")}},
     {{Center,Normal},
      {?__(11,"Average of vertex normals saved as axis."),
       ?__(12,"Use direction between vertices as axis")}}];

%% Use two edge loops on separate objects.
get_vec(edge, [{Edges1},{Edges2}], [We1,We2]) ->
    Loop1 = wings_edge_loop:edge_loop_vertices(Edges1, We1),
    Loop2 = wings_edge_loop:edge_loop_vertices(Edges2, We2),
    case {Loop1, Loop2} of
    {[Vs1],[Vs2]} ->
        LoopCenter1 = wings_vertex:center(Vs1, We1),
        LoopCenter2 = wings_vertex:center(Vs2, We2),
        Center = e3d_vec:average(LoopCenter1, LoopCenter2),
        Vec = e3d_vec:norm_sub(LoopCenter1, LoopCenter2),
        [{{Center,Vec},?__(28,"Axis between edge loop centers saved as axis.")}];		
    _Other ->
        [{none,guard_string()}]
    end;

get_vec(edge, [Edge1,Edge2], [#we{es=Etab0,vp=Vtab0},#we{es=Etab1,vp=Vtab1}]) ->
    #edge{vs=Va1,ve=Vb1} = array:get(Edge1, Etab0),
    #edge{vs=Va2,ve=Vb2} = array:get(Edge2, Etab1),
    Va1Pos = array:get(Va1, Vtab0),
    Vb1Pos = array:get(Vb1, Vtab0),
    Va2Pos = array:get(Va2, Vtab1),
    Vb2Pos = array:get(Vb2, Vtab1),
    Center1 = e3d_vec:average(Va1Pos, Vb1Pos),
    Center2 = e3d_vec:average(Va2Pos, Vb2Pos),
    Center = e3d_vec:average(Center1, Center2),
    Vec = e3d_vec:norm_sub(Center1, Center2),
    Vec1 = e3d_vec:norm_sub(Va1Pos,Vb1Pos),
    Vec2 = e3d_vec:norm_sub(Va2Pos,Vb2Pos),
    Cross = e3d_vec:cross(Vec1,Vec2),
    [{{Center,Vec},
      {?__(5,"Direction between edges saved as axis."),
       ?__(24,"Save cross vector")}},
     {{Center,Cross},
      {?__(25,"Cross product of edges saved as axis"),
       ?__(26,"Save direction between edges")}}];


get_vec(face, [Face1, Face2], [We0, We1]) ->
    Center1 = wings_face:center(Face1, We0),
    Center2 = wings_face:center(Face2, We1),
    Center = e3d_vec:average(Center1, Center2),
    Vec = e3d_vec:norm_sub(Center1, Center2),
    Face1n = wings_face:normal(Face1, We0),
    Face2n = wings_face:normal(Face2, We1),
    Normal = e3d_vec:norm(e3d_vec:add(Face1n, Face2n)),
    [{{Center,Vec},
      {?__(17,"Direction between face centers saved as axis."),
       ?__(18,"Use average of face normals")}},
     {{Center,Normal},
      {?__(19,"Average of face normals saved as axis."),
       ?__(20,"Use direction between face centers")}}];

%% Use single edge as axis
get_vec(edge, [Edge], #we{es=Etab,vp=Vtab,mirror=Mir}=We) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf} = array:get(Edge, Etab),
    VaPos = array:get(Va, Vtab),
    VbPos = array:get(Vb, Vtab),
    Vec = e3d_vec:norm_sub(VbPos, VaPos),
    Center = wings_vertex:center([Va,Vb], We),
    Normal = case Mir of
      Lf ->
        Mn = wings_face:normal(Mir, We),
        Rn = wings_face:normal(Rf, We),
        Cross = e3d_vec:cross(Mn, Rn),
        e3d_vec:norm(e3d_vec:cross(Cross, Mn));
      Rf ->
        Mn = wings_face:normal(Mir, We),
        Ln = wings_face:normal(Lf, We),
        Cross = e3d_vec:cross(Mn, Ln),
        e3d_vec:norm(e3d_vec:cross(Cross, Mn));
      _ ->
        Ln = wings_face:normal(Lf, We),
        Rn = wings_face:normal(Rf, We),
        e3d_vec:norm(e3d_vec:add(Ln, Rn))
    end,
    [{{Center,Vec},
      {?__(1,"Edge saved as axis"),?__(2,"Save edge normal")}},
     {{Center,Normal},
      {?__(3,"Edge normal saved as axis"),?__(4,"Save edge direction")}}];
%% Use direction between two edges
get_vec(edge, [Edge1,Edge2], #we{es=Etab,vp=Vtab}) ->
    #edge{vs=Va1,ve=Vb1} = array:get(Edge1, Etab),
    #edge{vs=Va2,ve=Vb2} = array:get(Edge2, Etab),
    Va1Pos = array:get(Va1, Vtab),
    Vb1Pos = array:get(Vb1, Vtab),
    Va2Pos = array:get(Va2, Vtab),
    Vb2Pos = array:get(Vb2, Vtab),
    Center1 = e3d_vec:average(Va1Pos, Vb1Pos),
    Center2 = e3d_vec:average(Va2Pos, Vb2Pos),
    Center = e3d_vec:average(Center1, Center2),
    Vec = e3d_vec:norm_sub(Center1, Center2),
    Vec1 = e3d_vec:norm_sub(Va1Pos, Vb1Pos),
    Vec2 = e3d_vec:norm_sub(Va2Pos, Vb2Pos),
    Cross = e3d_vec:cross(Vec1, Vec2),
    [{{Center,Vec},
      {?__(5,"Direction between edges saved as axis."),?__(24,"Save cross vector")}},
     {{Center,Cross},
      {?__(25,"Cross product of edges saved as axis"),?__(26,"Save direction between edges")}}];
%% Use edge-loop normal.
get_vec(edge, Edges, #we{vp=Vtab}=We) ->
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_ccw(Vs, Vtab),
	    [{{Center,Vec},?__(6,"Edge loop normal saved as axis.")}];
	[Vs1,Vs2] ->
	    LoopCenter1 = wings_vertex:center(Vs1, We),
	    LoopCenter2 = wings_vertex:center(Vs2, We),
		Center = e3d_vec:average(LoopCenter1, LoopCenter2),
	    Vec = e3d_vec:norm_sub(LoopCenter1, LoopCenter2),
	    [{{Center,Vec},?__(28,"Axis between edge loop centers saved as axis.")}];		
	_Other ->
	    [{none,?__(29,"Multi-edge selection must form either a single or two closed edge loops.")}]
    end;

%% Vertex normal
get_vec(vertex, [V], We) ->
    Vec = vertex_no_mirror_norm(V,We),
    Center = wings_vertex:center([V], We),
    [{{Center,Vec},?__(8,"Vertex normal saved.")}];
%% Direction between 2 vertices as axis
get_vec(vertex, [Va,Vb]=Vs, We) ->
    VaPos = wings_vertex:pos(Va, We),
    VbPos = wings_vertex:pos(Vb, We),
    Vec = e3d_vec:norm_sub(VaPos, VbPos),
    Center = wings_vertex:center(Vs, We),
    Normal = e3d_vec:norm(e3d_vec:add(vertex_no_mirror_norm(Va,We),
        vertex_no_mirror_norm(Vb,We))),
    [{{Center,Vec},
      {?__(9,"Direction between vertices saved as axis."),
       ?__(10,"Use average of vertex normals as axis")}},
     {{Center,Normal},
      {?__(11,"Average of vertex normals saved as axis."),
       ?__(12,"Use direction between vertices as axis")}}];

%% 3-point (defines face) perpendicular
get_vec(vertex, [_,_,_]=Vs, #we{vp=Vtab}=We) ->
    Vec = wings_face:face_normal_ccw(Vs, Vtab),
    Center = wings_vertex:center(Vs, We),
    [{{Center,Vec},?__(13,"3-point perp. normal saved as axis.")}];
%% Take the edge loop normal.
get_vec(vertex, Vs0, #we{vp=Vtab}=We) ->
    Edges = find_edges(Vs0, We),
    case wings_edge_loop:edge_loop_vertices(Edges, We) of
	[Vs] -> 
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_cw(Vs, Vtab),
	    [{{Center,Vec},?__(14,"Vertex loop normal saved as axis.")}];
	_Other ->
	    [{none,?__(15,"Multi-vertex selection must form a single closed edge loop.")}]
    end;

%% Face normal
get_vec(face, [Face], We) ->
    Vec = wings_face:normal(Face, We),
    Vs = wings_face:to_vertices([Face], We),
    Center = wings_vertex:center(Vs, We),
    [{{Center,Vec},?__(16,"Face normal saved as axis.")}];
%% Direction between two faces as axis
get_vec(face, [Face1,Face2], We) ->
    Center1 = wings_face:center(Face1, We),
    Center2 = wings_face:center(Face2, We),
    Center = e3d_vec:average(Center1, Center2),
    Vec = e3d_vec:norm_sub(Center1, Center2),
    Face1n = wings_face:normal(Face1, We),
    Face2n = wings_face:normal(Face2, We),
    Normal = e3d_vec:norm(e3d_vec:add(Face1n, Face2n)),
    [{{Center,Vec},
      {?__(17,"Direction between face centers saved as axis."),
       ?__(18,"Use average of face normals")}},
     {{Center,Normal},
      {?__(19,"Average of face normals saved as axis."),
       ?__(20,"Use direction between face centers")}}];
get_vec(face, Faces, #we{vp=Vtab}=We) ->
    case wings_vertex:outer_vertices_ccw(Faces, We) of
	error ->
	    [{none,?__(22,"Multi-face selection must have a single edge loop.")}];
	Vs when is_list(Vs) ->
	    Center = wings_vertex:center(Vs, We),
	    Vec = wings_face:face_normal_cw(Vs, Vtab),
	    [{{Center,Vec},?__(21,"Edge loop normal for region saved as axis.")}]
    end;

get_vec(_, _, _) -> {none,?__(23,"Select vertices, edges, or faces.")}.

%%%
%%% Point functions.
%%%

check_point(#st{sel=[]}) -> {none,""};
check_point(St) ->
    case kill_mirror(St) of
        St ->
            Center = e3d_vec:average(wings_sel:bounding_box(St)),
            [{Center,?__(1,"Midpoint of selection saved.")}];
        NoMirror ->
            Center = e3d_vec:average(wings_sel:bounding_box(St)),
            NoMirrorCenter = e3d_vec:average(wings_sel:bounding_box(NoMirror)),
            [{NoMirrorCenter,
              {?__(1,"Midpoint of selection saved."),
               ?__(3,"Include virtual mirror in reference point calculation")}},
             {Center,
              {?__(1,"Midpoint of selection saved."),
               ?__(2,"Disregard virtual mirror in reference point calculation")}}]
    end.

check_magnet_point(#st{sel=[]}) -> {none,""};
check_magnet_point(St0) ->
    St = kill_mirror(St0),
    Center = e3d_vec:average(wings_sel:bounding_box(St)),
    [{Center,?__(1,"Midpoint of selection saved.")}].

kill_mirror(#st{shapes=Shs0}=St) ->
    Shs = kill_mirror_1(gb_trees:values(Shs0), []),
    St#st{shapes=Shs}.

kill_mirror_1([#we{id=Id}=We0|Wes], Acc) ->
    We = wings_we:break_mirror(We0),
    kill_mirror_1(Wes, [{Id,We}|Acc]);
kill_mirror_1([], Acc) ->
    gb_trees:from_orddict(reverse(Acc)).

%%%
%%% Magnet functions.
%%%

exit_magnet(Vec, Mod, Acc) ->
    case wings_msg:free_rmb_modifier() of
	ModRmb when Mod band ModRmb =/= 0 ->
	    Fun = fun(Mag) ->
			  {[],[Mag|Acc]}
		  end,
	    case Vec  of
		none ->
		    wings_magnet:dialog(Fun);
		Point ->
		    wings_magnet:dialog(Point, Fun)
	    end;
	_ ->
	    case Vec of
		none ->
		    error;
		Point ->
		    Mag = {magnet,wings_pref:get_value(magnet_type),
			   wings_pref:get_value(magnet_distance_route),Point},
		    {[],[Mag|Acc]}
	    end
    end.

%%%
%%% Utilities.
%%%

find_edges(Vs, We) ->
    VsSet = gb_sets:from_list(Vs),
    Es = find_edges(Vs, VsSet, We, []),
    ordsets:from_list(Es).

find_edges([V|Vs], VsSet, We, Acc0) ->
    Acc = wings_vertex:fold(
	    fun(E, _, Rec, A) ->
		    OtherV = wings_vertex:other(V, Rec),
		    case gb_sets:is_member(OtherV, VsSet) of
			false -> A;
			true -> [E|A]
		    end
	    end, Acc0, V, We),
    find_edges(Vs, VsSet, We, Acc);
find_edges([], _VsSet, _We, Acc) -> Acc.

%%%
%%% Showing of active vector.
%%%

erase_vector() ->
    case erase(wings_current_vector) of
	undefined -> ok;
	#vec{vec=VecDl} -> catch gl:deleteLists(VecDl, 1)
    end.

draw_vec(none) ->
    erase_vector();
draw_vec(Vec) ->
    case get(wings_current_vector) of
	#vec{src_vec=Vec,vec=VecDl} -> ok;
	_ ->
	    erase_vector(),
	    VecDl = make_vec_dlist(Vec),
	    put(wings_current_vector, #vec{vec=VecDl,src_vec=Vec}),
	    VecDl
    end,
    wings_dl:call(VecDl).

make_vec_dlist(Vec) ->
    Dlist = gl:genLists(1),
    gl:newList(Dlist, ?GL_COMPILE),
    make_vec_dlist_1(Vec),
    gl:endList(),
    Dlist.

make_vec_dlist_1({Center,Vec0}) ->
    Vec = e3d_vec:mul(Vec0, wings_pref:get_value(active_vector_size)),
    End = e3d_vec:add(Center,Vec),
    HeadVec = e3d_vec:mul(Vec, -0.2),
    HeadPt = e3d_vec:add(End, HeadVec),
    case HeadVec of
	{Same,Same,Same} ->
	    PosHead0 = e3d_vec:cross(HeadVec, {0.25,-0.25,0.25}),
	    PosHead1 = e3d_vec:cross(HeadVec, {-0.25,0.25,-0.25});
	_Other ->
	    PosHead0 = e3d_vec:cross(HeadVec, {0.25,0.25,0.25}),
	    PosHead1 = e3d_vec:cross(HeadVec, {-0.25,-0.25,-0.25})
    end,
    Width = wings_pref:get_value(active_vector_width),
    gl:color3fv(wings_pref:get_value(active_vector_color)),
    gl:pointSize(Width*3.5),
    gl:lineWidth(Width),
    gl:'begin'(?GL_LINES),
    gl:vertex3fv(Center),
    gl:vertex3fv(End),
    gl:vertex3fv(End),
    gl:vertex3fv(e3d_vec:sub(HeadPt, PosHead0)),
    gl:vertex3fv(End),
    gl:vertex3fv(e3d_vec:sub(HeadPt, PosHead1)),
    gl:'end'();
make_vec_dlist_1(Center) ->
    Width = wings_pref:get_value(active_vector_width),
    gl:color3fv(wings_pref:get_value(active_vector_color)),
    gl:pointSize(Width*3.5),
    gl:'begin'(?GL_POINTS),
    gl:vertex3fv(Center),
    gl:'end'().

vertex_no_mirror_norm(V, #we{mirror=Mir}=We) ->
    {Mn,Ns} = wings_vertex:fold(fun(_, Face, _, {M,A}) when Face =/= Mir ->
          {M,[wings_face:normal(Face, We)|A]};
          (_,_,_,{_,A}) ->
            {wings_face:normal(Mir, We),A}
      end, {none,[]}, V, We),
    N = e3d_vec:norm(e3d_vec:add(Ns)),
    case Mn of
      none ->
        N;
      Mn ->
        Cross = e3d_vec:cross(Mn,N),
        e3d_vec:cross(Cross,Mn)
    end.
