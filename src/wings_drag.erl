%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_drag).
-export([do_drag/2,fold/3,fold/4,
         matrix/3,matrix/4,general/4,drag_only/4,
         compose/1,translate_fun/2]).

-export_type([drag/0,vec_transform_fun/0,vertices/0,vertex_transform/0]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-import(lists, [append/1,foldl/3,sort/1,reverse/1,reverse/2,
                member/2,unzip/1]).

%% Main drag record. Kept in state.
-record(drag,
	{x,y,			% Original 2D position,
	 xs=0,ys=0,		% Summary of mouse movements
	 zs=0,			% Z move in screen relative
	 p4=0,p5=0,		% An optional forth drag parameter
	 psum=[0,0,0,0,0],	% Whereas xs,ys,zs,p4 and p5 are the displayed
	 			%  distances psum is the unconstrained mouse
	 			%  summary pre parameter
	 xt=0,yt=0,		% Last warp length
	 lmb_timer=0,		% Lmb is pressed timer
	 mmb_timer=0,		% Mmb is pressed timer
	 rmb_timer=0,		% Rmb is pressed timer (forth parameter)
	 offset,		% Offset for each dimension.
	 unit :: [unit()],      % Unit that drag is done in.
	 unit_sc,		% Scales for each dimension.
	 flags=[] :: [flag()],  % Flags.
	 falloff,		% Magnet falloff.
	 mode_fun :: mode_fun(),% Special mode.
	 mode_data :: mode_data(),% State for mode.
	 info="",		% Information line.
	 st :: #st{},           % Saved st record.
	 last_move,		% Last move.
         drag                   % Drag fun.
	}).


%% Drag per object.

-record(do,
	{tr_fun :: vec_transform_fun(),  %List of transformation funs.
	 we_funs :: [we_transform_fun()] %List of funs that operate on the We.
	}).

-type vertex_num() :: wings_vertex:vertex_num().

-type vertices() :: [vertex_num()].

-type e3d_matrix() :: e3d_mat:matrix().

-type mat_transform_fun() :: fun((e3d_matrix(), [float()]) -> e3d_matrix()).
-type we_transform_fun() :: fun((#we{}, [float()]) -> #we{}).
-type vec_transform_fun() ::
        fun((_, _) -> [{vertex_num(),e3d_vec:vector()}]
                          | vec_transform_fun()).
-type general_fun() :: fun((_, #dlo{}) -> #dlo{}).

-type vertex_transform() :: {vertices(),vec_transform_fun()}.

-type basic_tv() :: {vertices(),vec_transform_fun()}.

-type fold_tv() :: basic_tv() | {'we',[we_transform_fun()],basic_tv()}.

-type inf_or_float() :: 'infinity' | float().
-type limit2() :: {inf_or_float(),inf_or_float()}.

-type unit() :: 'angle'   | {'angle',limit2()}
              | 'distance'| {'distance',limit2()}
              | 'dx'      | {'dx',limit2()}
              | 'dy'      | {'dy',limit2()}
              | 'dz'
              | 'falloff'
              | {'number',limit2()}
              | 'percent' | {'percent',limit2()}
              | 'rx'      | {'rx',limit2()}
              | 'skip'
              | plugin_unit_kludge().

%% FIXME: Should wrap in a tuple, e.e. {custom,CustomType}.
-type plugin_unit_kludge() :: 'absolute_diameter'
                            | 'diametric_factor'.

-type mode_fun() :: fun((any(), mode_data()) -> mode_data()).
-type mode_data() :: any().

-type flag() :: {'initial',list()}
              | 'keep_drag'
              | {'mode',{mode_fun(),mode_data()}}
              | {'rescale_normals',boolean()}
              | 'screen_relative'.

-type drag() :: #drag{} | {'general',general_fun()} | #do{}.


-spec fold(Fun, [unit()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((wings_sel:item_set(), #we{}) -> fold_tv()).

fold(F, Units, St) when is_function(F, 2) ->
    fold(F, Units, [], St).

-spec fold(Fun, [unit()], [flag()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((wings_sel:item_set(), #we{}) -> fold_tv()).

fold(F, Units, Flags, St) when is_function(F, 2) ->
    #st{sel=Sel,shapes=Shapes} = St,
    Tvs = fold_1(Sel, F, Shapes),
    Drag = init(Units, Flags, St),
    wings_draw:refresh_dlists(St),
    break_apart(Tvs, St),
    {drag,Drag}.

-spec matrix(Fun, [unit()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((#we{}) -> mat_transform_fun()).

matrix(F, Units, #st{selmode=body}=St) ->
    matrix(F, Units, [], St).

-spec matrix(Fun, [unit()], [flag()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((#we{}) -> mat_transform_fun()).

matrix(F, Units, Flags, #st{selmode=body}=St) when is_function(F, 1) ->
    #st{sel=Sel,shapes=Shapes} = St,
    Tvs = get_funs(Sel, F, Shapes),
    Drag = init(Units, Flags, St),
    wings_draw:refresh_dlists(St),
    insert_matrix(Tvs),
    {drag,Drag}.

-spec general(Fun, [unit()], [flag()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((#we{}) -> general_fun()).

general(F, Units, Flags, St) when is_function(F, 1) ->
    #st{sel=Sel,shapes=Shapes} = St,
    Tvs = get_funs(Sel, F, Shapes),
    Drag = init(Units, Flags, St),
    wings_draw:refresh_dlists(St),
    break_apart_general(Tvs),
    {drag,Drag}.

-spec drag_only(Fun, [unit()], [flag()], #st{}) -> {'drag',#drag{}} when
      Fun :: fun((_) -> 'ok').

drag_only(F, Units, Flags, St) when is_function(F, 1) ->
    {drag,init(Units, Flags, St, F)}.

-spec compose([{vertices(),vec_transform_fun()}]) ->
                     {vertices(),vec_transform_fun()}.

compose([{_,_}=Transform]) ->
    Transform;
compose(Transforms) ->
    {Vs0,TransformFuns} = unzip(Transforms),
    Vs = append(Vs0),
    F = compose_fun(TransformFuns),
    {Vs,F}.

-spec translate_fun([{e3d_vec:vector(),vertices()}], #we{}) ->
                           {vertices(),vec_transform_fun()}.

translate_fun([_|_]=VecVs0, #we{vp=Vtab}) ->
    SS = sofs:from_term(VecVs0, [{vec,[vertex]}]),
    FF = sofs:relation_to_family(SS),
    FU = sofs:family_union(FF),
    VecVs1 = sofs:to_external(FU),
    Affected = foldl(fun({_,Vs}, A) -> Vs++A end, [], VecVs1),
    VecVs = insert_vtx_data(VecVs1, Vtab, []),
    {Affected,translate_fun(VecVs)}.

%%%
%%% Local functions.
%%%

init(Units, Flags, St) ->
    init(Units, Flags, St, fun default_drag_fun/1).

init(Units, Flags, St, DragFun) ->
    cursor_boundary(60),
    wings_io:grab(),
    wings_wm:grab_focus(),
    Offset0 = proplists:get_value(initial, Flags, []),
    Offset = pad_offsets(Offset0),
    UnitSc = unit_scales(Units),
    Falloff = falloff(Units),
    {ModeFun,ModeData} = setup_mode(Flags, Falloff),
    #drag{unit=Units,unit_sc=UnitSc,flags=Flags,offset=Offset,
		 falloff=Falloff,
		 mode_fun=ModeFun,mode_data=ModeData,
		 st=St,drag=DragFun}.

fold_1([{Id,Items}|T], F, Shapes0) ->
    We0 = gb_trees:get(Id, Shapes0),
    ?ASSERT(We0#we.id =:= Id),
    Tv = F(Items, We0),
    Shapes = case We0 of
                 #we{temp=[]} ->
                     Shapes0;
                 #we{} ->
                     We = We0#we{temp=[]},
                     gb_trees:update(Id, We, Shapes0)
             end,
    case Tv of
        {we,WeFuns,OtherTv} when is_list(WeFuns) ->
            [{Id,WeFuns,OtherTv}|fold_1(T, F, Shapes)];
        _ ->
            [{Id,[],Tv}|fold_1(T, F, Shapes)]
    end;
fold_1([], _, _) -> [].

get_funs([{Id,_}|T], F, Shapes0) ->
    We0 = gb_trees:get(Id, Shapes0),
    ?ASSERT(We0#we.id =:= Id),
    General = F(We0),
    Shapes = case We0 of
                 #we{temp=[]} ->
                     Shapes0;
                 #we{} ->
                     We = We0#we{temp=[]},
                     gb_trees:update(Id, We, Shapes0)
             end,
    [{Id,General}|get_funs(T, F, Shapes)];
get_funs([], _, _) -> [].


%%%
%%% Handle composing of transformation funs.
%%%

compose_fun(TransformFuns0) ->
    fun(Arg, Acc) when is_list(Arg) ->
            execute_composed(TransformFuns0, Arg, Acc);
       (Key, We) ->
            TransformFuns = execute_updated(TransformFuns0, Key, We),
            compose_fun(TransformFuns)
    end.

execute_composed([TF|TFs], Arg, Acc0) ->
    Acc = TF(Arg, Acc0),
    execute_composed(TFs, Arg, Acc);
execute_composed([], _Arg, Acc) -> Acc.

execute_updated([TF0|TFs], Key, We) ->
    TF = TF0(Key, We),
    [TF|execute_updated(TFs, Key, We)];
execute_updated([], _Key, _We) -> [].

%% make sure cursor isn't too close to the edge of the window since this can
%% cause drag response problems.
cursor_boundary(P) ->
    {W,H} = wings_wm:win_size(),
    {_,Mx,My} = wings_wm:local_mouse_state(),
    X = if Mx-P > 0 ->
		if Mx+P < W -> Mx;
		   true -> W-P
		end;
           true -> P
        end,
    Y = if My-P > 0 ->
		if My+P < H -> My;
		   true -> H-P
		end;
           true -> P
        end,
    wings_io:warp(X, Y).

setup_mode(Flags, Falloff) ->
    case proplists:get_value(mode, Flags, none) of
	none ->
	    {standard_mode_fun(Falloff),none};
	{F,_}=Mode when is_function(F, 2) ->
	    Mode
    end.

standard_mode_fun(Falloff) ->
    Help0 = ?__(1,"[Shift] and/or [Ctrl] and/or [Alt] Constrain"),
    Help = case Falloff of
	       none -> Help0;
	       _ ->
		   M = [?__(2,"[+] or [-] Adjust Radius"),
			"  "|Help0],
		   lists:flatten(M)
	   end,
    fun(help, _) -> Help;
       (_, _) -> none
    end.

unit_scales(Units) ->
    case wings_pref:get_value(drag_custom) of
      false ->
        default_drag(Units);
      true -> 
        custom_drag(Units)
    end.

custom_drag(Units) ->
    BasicSc = 1/500,
    DistSc = custom_absolute_drag(),
    PercentSc = custom_relative_drag(),
    AngleSc = custom_rotations_drag(),
    unit_scales_1(Units, BasicSc, DistSc, PercentSc, AngleSc).

custom_absolute_drag() ->
    Speed = wings_pref:get_value(drag_speed_abs),
    #view{fov=Fov, distance=D} = wings_view:current(),
    (D/((11-Speed)*((11-Speed)*300)))*Fov/60.

custom_relative_drag() ->
    Speed = wings_pref:get_value(drag_speed_relative),
    #view{distance=D} = wings_view:current(),
    (D/((11-Speed)*((11-Speed)*900))).

custom_rotations_drag() ->
    Speed = wings_pref:get_value(drag_speed_rotate),
    1/((10.1-Speed)*8).

default_drag(Units) ->
    #view{distance=D} = wings_view:current(),
    BasicSc = 1/500,
    PercentSc = 1/500,
    DistSc = D/(9*500),
    AngleSc = 1/50,
    unit_scales_1(Units, BasicSc, DistSc, PercentSc, AngleSc).

unit_scales_1([U|Us], BasicSc, DistSc, PercentSc, AngleSc) ->
    Sc = case clean_unit(U) of
	     distance -> DistSc;
	     dx -> DistSc;
	     dy -> DistSc;
	     dz -> DistSc;
	     angle -> AngleSc;
	     percent -> PercentSc;
	     _ -> BasicSc
	 end,
    [Sc|unit_scales_1(Us, BasicSc, DistSc, PercentSc, AngleSc)];
unit_scales_1([], _, _, _, _) -> [].

falloff([falloff|_]) -> 1.0;
falloff([_|T]) -> falloff(T);
falloff([]) -> none.

pad_offsets(Ds) ->
    case length(Ds) of
	L when L >= 5 -> Ds;
	L -> Ds ++ lists:duplicate(5-L, 0.0)
    end.

%% When the zoom changes during a camera event the units have to be rescaled.
adjust_unit_scaling([D|Ds],[U|Us],[U0|Us0]) ->
    [D*U0/U|adjust_unit_scaling(Ds,Us,Us0)];
adjust_unit_scaling(Ds,[],[]) -> Ds;
adjust_unit_scaling([],[],[]) -> [].

%%
%% Here we break apart the objects into two parts - static part
%% (not moved during drag) and dynamic (part of objects actually
%% moved).
%%
break_apart(Tvs, St) ->
    wings_dl:map(fun(D, Data) ->
			 break_apart_1(D, Data, St)
		 end, Tvs).

break_apart_1(#dlo{src_we=#we{id=Id}=We}=D0, [{Id,WFs,Tv}|Tvs], St) ->
    {Vs,TF} = mirror_constrain(Tv, We),
    D1 = if
	     ?IS_LIGHT(We) -> D0#dlo{split=none};
	     true -> D0
	 end,
    D = wings_draw:split(D1, Vs, St),
    Do = #do{tr_fun=TF,we_funs=WFs},
    {D#dlo{drag=Do},Tvs};
break_apart_1(D, Tvs, _) -> {D,Tvs}.

translate_fun(VecVs) ->
    fun(new_falloff, _Falloff) ->
	    translate_fun(VecVs);
       ([Dx|_], Acc) ->
	    foldl(fun({Vec,VsPos}, A) ->
			  translate(Vec, Dx, VsPos, A)
		  end, Acc, VecVs)
    end.

insert_vtx_data([{Vec,Vs0}|VecVs], Vtab, Acc) ->
    Vs = insert_vtx_data_1(Vs0, Vtab, []),
    insert_vtx_data(VecVs, Vtab, [{Vec,Vs}|Acc]);
insert_vtx_data([], _Vtab, Acc) -> Acc.

insert_vtx_data_1([V|Vs], Vtab, Acc) ->
    insert_vtx_data_1(Vs, Vtab, [{V,array:get(V, Vtab)}|Acc]);
insert_vtx_data_1([], _Vtab, Acc) -> Acc.

mirror_constrain(Tvs, #we{mirror=none}) ->
    Tvs;
mirror_constrain({Vs,Tr0}=Tv, #we{mirror=Face}=We) when is_function(Tr0, 2) ->
    M = wings_we:mirror_projection(We),
    VsSet0 = wings_face:vertices_cw(Face, We),
    VsSet = ordsets:from_list(VsSet0),
    case ordsets:intersection(ordsets:from_list(Vs), VsSet) of
	[] ->
            Tv;
	[_|_]=Mvs ->
	    Tr = constrain_fun(Tr0, M, Mvs),
	    {Vs,Tr}
    end.

constrain_fun(Tr0, M, Vs) ->
    fun(Cmd, Arg) ->
	    case Tr0(Cmd, Arg) of
		Tr when is_function(Tr, 2) ->
		    constrain_fun(Tr, M, Vs);
		List ->
		    constrain_vs(List, Vs, M, [])
	    end
    end.

constrain_vs([{V,Pos0}=H|T], Vs, M, Acc) ->
    case member(V, Vs) of
	false ->
	    constrain_vs(T, Vs, M, [H|Acc]);
	true ->
	    Pos = e3d_mat:mul_point(M, Pos0),
	    constrain_vs(T, Vs, M, [{V,Pos}|Acc])
    end;
constrain_vs([], _, _, Acc) -> Acc.

insert_matrix(Tvs) ->
    Id = e3d_mat:identity(),
    wings_dl:map(fun(D, Data) ->
			 insert_matrix_fun(D, Data, Id)
		 end, sort(Tvs)).

insert_matrix_fun(#dlo{work=Work,edges=Edges,sel=Sel,src_sel=SrcSel,smooth=Smooth,
		       src_we=#we{id=Id}=We,mirror=M,
		       proxy=Proxy,proxy_data=Pd,open=Open},
		  [{Id,Tr}|Tvs], Matrix) ->
    {#dlo{work=Work,edges=Edges,sel=Sel,drag={matrix,Tr,Matrix,Matrix},
	  src_we=We,src_sel=SrcSel,mirror=M,smooth=Smooth,
	  proxy=Proxy,proxy_data=Pd,open=Open},Tvs};
insert_matrix_fun(D, Tvs, _) -> {D,Tvs}.

break_apart_general(Tvs) ->
    wings_dl:map(fun break_apart_general/2, Tvs).

break_apart_general(#dlo{src_we=#we{id=Id}}=D, [{Id,Fun}|Tvs]) ->
    {D#dlo{drag={general,Fun}},Tvs};
break_apart_general(D, Tvs) -> {D,Tvs}.

%%%
%%% Handling of drag events.
%%%

-spec do_drag(#drag{}, [_] | 'none') -> term().

do_drag(#drag{flags=Flags}=Drag, none) ->
    wings_menu:kill_menus(), %% due to toolbar menu facility
    case proplists:get_bool(rescale_normals, Flags) of
	false -> ok;
	true -> gl:enable(gl_rescale_normal())
    end,
    case wings_pref:get_value(hide_sel_while_dragging) of
	true -> clear_sel_dlists();
	false -> ok
    end,
    {_,X,Y} = wings_wm:local_mouse_state(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    wings_tweak:toggle_draw(false),
    {seq,push,handle_drag_event_2(Ev, Drag#drag{x=X,y=Y})};
do_drag(#drag{unit=Units}=Drag0, Move) when length(Units) =:= length(Move) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    Drag1 = motion(Ev, Drag0#drag{x=X,y=Y}),
    ungrab(Drag1),
    Drag2 = possible_falloff_update(Move, Drag1),
    Drag = ?SLOW(motion_update(Move, Drag2)),
    St = normalize(Move, Drag),
    DragEnded = {new_state,St#st{drag_args=Move}},
    wings_wm:later(DragEnded),
    keep;
do_drag(Drag0, _) ->
    wings_menu:kill_menus(),
    {_,X,Y} = wings_wm:local_mouse_state(),
    ungrab(Drag0#drag{x=X,y=Y}),
    wings_wm:later(revert_state),
    keep.

gl_rescale_normal() ->
    case wings_gl:is_ext('GL_EXT_rescale_normal') of
	true -> ?GL_RESCALE_NORMAL;
	false -> ?GL_NORMALIZE
    end.

help_message(#drag{unit=Unit,mode_fun=ModeFun,mode_data=ModeData}) ->
    Accept = wings_msg:button_format(wings_s:accept()),
    Message = wings_msg:join(drag_help(Unit)),
    Cancel = wings_msg:button_format([], [], wings_s:cancel()),
    NumEntry = ?__(1,"Numeric Entry"),
    Tab = wings_util:key_format("[Tab]", NumEntry),
    Switch = switch(),
    Constraint = ?__(2,"Switch Constraint Set")++Switch,
    ShiftTab = wings_util:key_format("[Shift]+[Tab]", Constraint),
    Msg = wings_msg:join([Accept, Message, Cancel, Tab, ShiftTab]),
    MsgRight = ModeFun(help, ModeData),
    wings_wm:message(Msg, MsgRight).

switch() ->
    case wings_pref:get_value(con_alternate) of
      true -> ?__(1," (using Alternate Constraints)");
      false -> []
    end.

drag_help(Units) ->
    {_,_,Message} = foldl(fun
        (falloff, {N,S,Acc}) -> {N+1,S,Acc};
        (skip, {N,_,Acc}) -> {N+1,true,Acc};
        (P, {3,S,MsgAcc}) ->
          case P of
            dz ->
              Msg = zmove_help(?__(1,"Move along Screen Normal")),
              {4,S,[Msg|MsgAcc]};
            angle ->
              Msg = zmove_help(?__(4,"Rotate")),
              {4,S,[Msg|MsgAcc]};
            percent ->
              Msg = zmove_help(?__(2,"Scale")),
              {4,S,[Msg|MsgAcc]};
            _ when S ->
              Msg = zmove_help(wings_util:stringify(P)),
              {4,S,[Msg|MsgAcc]};
            _ ->
              Msg = zmove_help(?__(3,"Adjust 2nd value")),
              {4,S,[Msg|MsgAcc]}
          end;
        (P, {4,S,MsgAcc}) ->
          case P of
            angle ->
              Msg = p4_help(?__(4,"Rotate")),
              {5,S,[Msg|MsgAcc]};
            percent ->
              Msg = p4_help(?__(2,"Scale")),
              {5,S,[Msg|MsgAcc]};
            _ when S ->
              Msg = p4_help(wings_util:stringify(P)),
              {5,S,[Msg|MsgAcc]};
            _ ->
              Msg = p4_help(?__(5,"Adjust 3rd value")),
              {5,S,[Msg|MsgAcc]}
          end;
        (P, {5,S,MsgAcc}) ->
          case P of
            angle ->
              Msg = p5_help(?__(4,"Rotate")),
              {6,S,[Msg|MsgAcc]};
            percent ->
              Msg = p5_help(?__(2,"Scale")),
              {6,S,[Msg|MsgAcc]};
            _ when S ->
              Msg = p5_help(wings_util:stringify(P)),
              {6,S,[Msg|MsgAcc]};
            _ ->
              Msg = p5_help(?__(6,"Adjust 4th value")),
              {6,S,[Msg|MsgAcc]}
          end;
        (_, {N,S,Acc}) -> {N+1,S,Acc}
    end,{1,false,[]}, Units),
    reverse(Message).

zmove_help(Msg) ->
    wings_s:lmb() ++ plus_drag_string() ++ Msg.

p4_help(Msg) ->
    DragMsg = plus_drag_string() ++ Msg,
    case wings_pref:get_value(camera_mode) of
      tds -> wings_s:key(ctrl) ++ "+" ++ wings_s:rmb() ++ DragMsg;
      blender -> wings_s:key(ctrl) ++ "+" ++ wings_s:rmb() ++ DragMsg;
      sketchup -> wings_s:key(ctrl) ++ "+" ++ wings_s:rmb() ++ DragMsg;
      _ -> wings_s:mmb() ++ DragMsg
    end.

p5_help(Msg) ->
    wings_s:rmb() ++ plus_drag_string() ++ Msg.

plus_drag_string() -> ?__(1,"+Drag: ").

get_drag_event(Drag) ->
    case wings_pref:get_value(hide_sel_while_dragging) of
	true -> ok;
	false ->
	    clear_sel_dlists(),
	    wings_draw:update_sel_dlist()
    end,
    wings_wm:dirty(),
    get_drag_event_1(Drag).

get_drag_event_1(Drag) ->
    {replace,fun(Ev) -> handle_drag_event_0(Ev, Drag) end}.

%% When the Rmb is pressed we store the time in order to determine if the
%% intention is to drag or cancel the command. Later, when the Rmb is released
%% we take the time again. If the time interval is short then the event is
%% handled as an intention to cancel the command.
handle_drag_event_0(#mousebutton{button=3,state=?SDL_PRESSED}=Ev,
		  #drag{rmb_timer=0}=Drag) ->
    StartTimer = os:timestamp(),
    handle_drag_event_0(Ev, Drag#drag{rmb_timer=StartTimer});
handle_drag_event_0(#mousebutton{button=1,state=?SDL_PRESSED}=Ev,
		  #drag{lmb_timer=0}=Drag) ->
    StartTimer = os:timestamp(),
    handle_drag_event_0(Ev, Drag#drag{lmb_timer=StartTimer});
handle_drag_event_0(#keyboard{sym=9, mod=Mod},Drag)->
    case Mod band ?SHIFT_BITS =/= 0 of
	true ->
	    case wings_pref:get_value(con_alternate) of
		true ->  wings_pref:set_value(con_alternate,false);
		false -> wings_pref:set_value(con_alternate,true)
	    end, get_drag_event(Drag);
	false ->
	    numeric_input(Drag)
    end;
handle_drag_event_0(#mousebutton{button=2,state=?SDL_RELEASED},
		  #drag{lmb_timer=0,mmb_timer=C,rmb_timer=0}=Drag) when C > 2 ->
    get_drag_event_1(Drag#drag{mmb_timer=0});
handle_drag_event_0(#mousebutton{button=3,state=?SDL_RELEASED,mod=Mod}=Ev,
		  #drag{mmb_timer=C}=Drag) when C > 2 ->
    if
	Mod band ?CTRL_BITS =/= 0 ->
	    get_drag_event_1(Drag#drag{lmb_timer=0,mmb_timer=0,rmb_timer=0});
	true ->
	    handle_drag_event_1(Ev,Drag#drag{lmb_timer=0,rmb_timer=0})
    end;

handle_drag_event_0(Cancel, #drag{})
  when Cancel =:= grab_lost; Cancel =:= cancel ->
    case Cancel of
        grab_lost -> wings_wm:release_focus();
        cancel -> ignore
    end,
    wings_dl:map(fun invalidate_fun/2, []),
    wings_tweak:toggle_draw(true),
    wings_wm:later(revert_state),
    pop;

handle_drag_event_0(#mousebutton{button=3,state=?SDL_RELEASED},
		  #drag{rmb_timer=StartTime}=Drag) when StartTime =/= 0 ->
    %% When Rmb is released we subtract the StartTime (when the Rmb was pressed)
    %% from the Stop time (relased) and if the result is less than 500000 ms, then
    %% we cancel the drag. If not we continue the drag using the 
    Stop = os:timestamp(),
    Time = timer:now_diff(Stop, StartTime),
    case Time < 500000 of
        false ->
            get_drag_event_1(Drag#drag{lmb_timer=0,mmb_timer=0,rmb_timer=0});
        true ->
            wings_dl:map(fun invalidate_fun/2, []),
            ungrab(Drag),
            wings_tweak:toggle_draw(true),
            wings_wm:later(revert_state),
            pop
    end;

handle_drag_event_0(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_RELEASED},
		  #drag{lmb_timer=StartTime}=Drag0) when StartTime =/= 0 ->
    %% When Lmb timer is less than 500000 ms, we Accept and finish the drag, if
    %% not, then we continue using the lmb parameter.
    Stop = os:timestamp(),
    Time = timer:now_diff(Stop, StartTime),
    case Time < 500000 of
        false ->
	    get_drag_event_1(Drag0#drag{lmb_timer=0,mmb_timer=0,rmb_timer=0});
        true ->
	    Ev = #mousemotion{x=X,y=Y,state=0,mod=Mod},
	    Drag = ?SLOW(motion(Ev, Drag0)),
	    quit_drag(Drag)
    end;
handle_drag_event_0(#mousebutton{button=B,x=X,y=Y,mod=Mod,state=?SDL_RELEASED}, Drag0)
  when B =:= 1; B =:= 3 ->
    %% This function guards against reported crashes of the rmb being held, and
    %% clicking the lmb. I can't reproduce this crash, but I don't doubt that it
    %% happens. The probable cause is likely to do with misinterpreted or conflated
    %% mouse button hits. ~Richard Jones
    Ev = #mousemotion{x=X,y=Y,state=0,mod=Mod},
    Drag = ?SLOW(motion(Ev, Drag0)),
    quit_drag(Drag);

handle_drag_event_0(Event, Drag = #drag{st=St}) ->
    case wings_camera:event(Event, St, fun() -> redraw(Drag) end) of
	next -> handle_drag_event_1(Event, Drag);
	keep ->
	    %% Clear any potential marker for an edge about to be
	    %% cut (Cut RMB).
	    wings_dl:map(fun(#dlo{hilite=none}=D, _) -> D;
			    (D, _) -> D#dlo{hilite=none}
			 end, []),
	    %% Recalc unit_scales since zoom can have changed.   UNITS ARE MIXED IN SOME CASES
	    #drag{xs=Xs0,ys=Ys0,zs=Zs0,p4=P4th0,p5=P5th0,psum=Psum0,unit=Unit,
	          unit_sc=Us0} = Drag,
	    Us = unit_scales(Unit),
	    Psum = adjust_unit_scaling(Psum0,Us,Us0),
	    [Xs,Ys,Zs,P4th,P5th] = adjust_unit_scaling([Xs0,Ys0,Zs0,P4th0,P5th0],Us,Us0),
	    get_drag_event(Drag#drag{xs=Xs,ys=Ys,zs=Zs,p4=P4th,p5=P5th,psum=Psum,unit_sc=Us});
	Other ->
	    %% Clear any potential marker for an edge about to be
	    %% cut (Cut RMB).
	    wings_dl:map(fun(#dlo{hilite=none}=D, _) -> D;
			    (D, _) -> D#dlo{hilite=none}
			 end, []),
	    Other
    end.

handle_drag_event_1(#keyboard{unicode=C}=Ev,
		    #drag{mode_fun=ModeFun,mode_data=ModeData0}=Drag0) ->
    case ModeFun({key,C}, ModeData0) of
	none -> handle_drag_event_2(Ev, Drag0);
	ModeData ->
	    wings_wm:dirty(),
	    wings_wm:message_right(ModeFun(help, ModeData)),
	    Val = {ModeData,Drag0#drag.falloff},
	    Drag1 = parameter_update(new_mode_data, Val,
	                 Drag0#drag{mode_data={changed,ModeData}}),
	    Drag = case ModeFun(units, ModeData) of
		       none -> Drag1;
		       Units ->
		           Us = unit_scales(Units),
		           #drag{xs=Xs0,ys=Ys0,zs=Zs0,p4=P4th0,p5=P5th0,psum=Psum0,
		                 unit_sc=Us0} = Drag1,
		           Psum = adjust_unit_scaling(Psum0,Us,Us0),
		           [Xs,Ys,Zs,P4th,P5th] = adjust_unit_scaling([Xs0,Ys0,Zs0,P4th0,P5th0],Us,Us0),
		           Drag1#drag{xs=Xs,ys=Ys,zs=Zs,p4=P4th,p5=P5th,psum=Psum,unit=Units,
		                      unit_sc=Us}
		   end,
	    get_drag_event(Drag)
    end;
handle_drag_event_1(Ev, Drag) ->
    handle_drag_event_2(Ev, Drag).

handle_drag_event_2(redraw, Drag) ->
    help_message(Drag),
    redraw(Drag),
    get_drag_event_1(Drag);
handle_drag_event_2(#mousemotion{}=Ev, Drag0) ->
    Drag = motion(Ev, Drag0),
    get_drag_event(Drag);
handle_drag_event_2({numeric_preview,Move}, Drag0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    Drag1 = possible_falloff_update(Move, Drag0#drag{x=X,y=Y}),
    Drag = ?SLOW(motion_update(Move, Drag1)),
    get_drag_event(Drag);
handle_drag_event_2({drag_arguments,Move}, Drag0) ->
    Drag1 = possible_falloff_update(Move, Drag0),
    Drag = ?SLOW(motion_update(Move, Drag1)),
    St = normalize(Move, Drag),
    DragEnded = {new_state,St#st{drag_args=Move}},
    wings_tweak:toggle_draw(true),
    wings_wm:later(DragEnded),
    pop;

handle_drag_event_2(view_changed, Drag) ->
    get_drag_event(view_changed(Drag));
handle_drag_event_2({action,{drag_arguments,_}=DragArgs}, _) ->
    wings_wm:later(DragArgs);
handle_drag_event_2({action,numeric_input}, Drag) ->
    numeric_input(Drag);
handle_drag_event_2({action,{numeric_preview,Move}}, _) ->
    wings_wm:later({numeric_preview,Move});
handle_drag_event_2({camera,Ev,NextEv}, #drag{st=St}) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_camera:event(Ev#mousebutton{x=X,y=Y}, St) of
      next -> NextEv;
      Other -> Other
    end;
handle_drag_event_2(Event, #drag{st=St}=Drag0) ->
    case wings_hotkey:event(Event,St) of
	next ->
	    get_drag_event_1(Drag0);
	{view,quick_preview} ->
	    get_drag_event_1(Drag0);
	{view,smooth_proxy} ->
	    get_drag_event_1(Drag0);
	{view,Cmd} ->
	    wings_view:command(Cmd, St),
	    Drag = view_changed(Drag0),
	    get_drag_event(Drag);
	{select,less} ->
	    Drag = magnet_radius(-1, Drag0),
	    get_drag_event(Drag);
	{select,more} ->
	    Drag = magnet_radius(1, Drag0),
	    get_drag_event(Drag);
	_Other ->
	    wings_wm:later(Event),
	    quit_drag(Drag0)
    end.

quit_drag(#drag{last_move=Move} = Drag) ->
    ungrab(Drag),
    St = normalize(Move, Drag),
    DragEnded = {new_state,St#st{drag_args=Move}},
    wings_tweak:toggle_draw(true),
    wings_wm:later(DragEnded),
    pop.

ungrab(#drag{x=Ox,y=Oy}) ->
    wings_wm:release_focus(),
    wings_io:ungrab(Ox, Oy).

invalidate_fun(#dlo{drag=none}=D, _) -> D;
invalidate_fun(#dlo{src_we=We,proxy_data=PD}=D, _) ->
    wings_draw:abort_split(D#dlo{src_we=We#we{es=array:new()},
        proxy_data=wings_proxy:invalidate(PD, vab)}).

numeric_input(Drag0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    ungrab(Drag0),
    Ev = #mousemotion{x=X,y=Y,state=0,mod=0},
    {Move0,Drag} = case mouse_translate(Ev, Drag0) of
		       {{_,M},D} -> {M,D};
		       Other -> Other
		   end,
    wings_dialog:dialog(?__(1,"Numeric Input"),
			{drag_preview, make_query(Move0, Drag)},
			fun
			    ({dialog_preview,Res}) ->
				{numeric_preview,make_move(Res, Drag)};
			    (Res) ->
				{drag_arguments,make_move(Res, Drag)}
			end).

make_query(Move, #drag{unit=Units}) ->
    make_query_1(Units, Move).

make_query_1([U0|Units], [V|Vals]) ->
    case clean_unit(U0) of
	percent ->
	    [{hframe,[{text,V*100.0,percent_qrange(U0)},{label,"%"}]}|
	     make_query_1(Units, Vals)];
	angle ->
	    [{hframe,[{label,?__(2,"A")},
		      {text,V,qrange(U0)},{label,[?DEGREE]}]}|
	     make_query_1(Units, Vals)];
	skip ->
	     make_query_1(Units, Vals);
	falloff ->
	    [{hframe,[{label,qstr(falloff)},
		      {text,V,[{range,{1.0E-6,infinity}},
			       {width, undefined}]}]}|
	     make_query_1(Units, Vals)];
	U ->
	    [{hframe,[{label,qstr(U)},{text,V,qrange(U0)}]}|
	     make_query_1(Units, Vals)]
    end;
make_query_1([], []) -> [].

qstr(distance) -> ?__(1,"D");
qstr(dx) -> ?__(2,"Dx");
qstr(dy) -> ?__(3,"Dy");
qstr(dz) ->  ?__(4,"Dz");
qstr(falloff) ->  ?__(5,"R");
qstr(angle) ->	?__(6,"A");
qstr(Atom) -> wings_util:stringify(Atom).

qrange({_,{_,_}=Range}) -> [{range,Range}, {width, undefined}];
qrange(_) -> [{width, undefined}].

percent_qrange({_,{Min,Max}}) ->
    [{range,{safe_mul_100(Min),safe_mul_100(Max)}}, {width, undefined}];
percent_qrange(_) -> [{width, undefined}].

safe_mul_100(A) ->
    case catch 100*A of
	{'EXIT',_} -> A;
	P -> P
    end.

make_move(Move, #drag{unit=Units}) ->
    make_move_1(Units, Move).

make_move_1([skip|Units],Vals) ->
    [0|make_move_1(Units, Vals)];
make_move_1([{percent,_}=Unit|Units], [V|Vals]) ->
    [clamp(Unit, V/100)|make_move_1(Units, Vals)];
make_move_1([percent|Units], [V|Vals]) ->
    [V/100|make_move_1(Units, Vals)];
make_move_1([{U,{_Min,_Max}}=Unit|Units], [V|Vals]) ->
    make_move_1([U|Units], [clamp(Unit, V)|Vals]);
make_move_1([_U|Units], [V|Vals]) ->
    [float(V)|make_move_1(Units, Vals)];
make_move_1([], []) -> [].

magnet_radius(_Sign, #drag{falloff=none}=Drag) -> Drag;
magnet_radius(Sign, #drag{falloff=Falloff0}=Drag0) ->
    case Falloff0+Sign*?GROUND_GRID_SIZE/10 of
	Falloff when Falloff > 0 ->
	    Drag = Drag0#drag{falloff=Falloff},
	    parameter_update(new_falloff, Falloff, Drag);
	_Falloff -> Drag0
    end.

view_changed(#drag{flags=Flags}=Drag0) ->
    help_message(Drag0),
    case member(screen_relative, Flags) of
	false -> Drag0;
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    wings_dl:map(fun view_changed_fun/2, []),
	    case member(keep_drag,Flags) of
		true ->
		    Drag0#drag{x=X,y=Y};
		false ->
		    Drag0#drag{x=X,y=Y,xs=0,ys=0,zs=0,p4=0,p5=0,psum=[0,0,0,0,0]}
	    end
    end.

view_changed_fun(#dlo{drag={matrix,Tr,_,_},transparent=#we{}=We}=D, _) ->
    Id = e3d_mat:identity(),
    {D#dlo{src_we=We,drag={matrix,Tr,Id,Id}},[]};
view_changed_fun(#dlo{drag={matrix,Tr,_,Mtx}}=D, _) ->
    {D#dlo{drag={matrix,Tr,Mtx,Mtx}},[]};
view_changed_fun(#dlo{drag=#do{tr_fun=F0}=Do,src_we=We}=D, _) ->
    F = F0(view_changed, We),
    {D#dlo{drag=Do#do{tr_fun=F}},[]};
view_changed_fun(D, _) -> {D,[]}.

motion(Event, Drag0) ->
    {Move,Drag} = mouse_translate(Event, Drag0),
    motion_update(Move, Drag).

mouse_translate(Event0, Drag0) ->
    Mode = wings_pref:get_value(camera_mode),
    {Event,Mod,Drag1} = mouse_pre_translate(Mode, Event0,Drag0),
	mouse_range(Event, Drag1, Mod).

mouse_pre_translate(Mode, #mousemotion{state=Mask,mod=Mod}=Ev,Drag)
        when Mode=:=blender; Mode=:=sketchup; Mode=:=tds ->
    if
    Mask band ?SDL_BUTTON_RMASK =/= 0,
    Mod band ?CTRL_BITS =/= 0 ->
        {Ev#mousemotion{state=?SDL_BUTTON_MMASK},
         Mod band (bnot ?CTRL_BITS),Drag};
    true -> {Ev,Mod,Drag}
    end;
mouse_pre_translate(_, #mousemotion{mod=Mod}=Ev,Drag) ->
    {Ev,Mod,Drag}.

mouse_range(#mousemotion{x=X, y=Y, state=Mask},
		#drag{x=OX, y=OY,
			xs=Xs0, ys=Ys0, zs=Zs0, p4=P4th0,p5=P5th0,
			psum=Psum0,
			mode_data=MD,
			xt=Xt0, yt=Yt0, mmb_timer=Count0,
			unit_sc=UnitScales, unit=Unit, offset=Offset,
			last_move=LastMove}=Drag0, Mod) ->
    %%io:format("Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xs0,Ys0}]),
    [Xp,Yp,Zp,P4thp,P5thp] =
    case Mod =/= 0 of
		true -> Psum0;
		false -> [Xs0,Ys0,Zs0,P4th0,P5th0]
    end,

    case wings_pref:lowpass(X-OX, Y-OY) of
		{0,0} ->
			Drag = Drag0#drag{xt=0,yt=0},
			{{no_change,LastMove},Drag};

		{XD0,YD0} ->
			CS = constraints_scale(Unit,Mod,UnitScales),
			XD = CS*(XD0 + Xt0),
			YD = CS*(YD0 + Yt0),

			ModeData =
			case MD of
				{_,MD0} -> MD0;
				MD0 -> MD0
			end,
			ParaNum = length(Unit),
			case Mask of
				?SDL_BUTTON_LEFT when ParaNum >= 3 ->
					Xs = {no_con, Xs0},
					Ys = {no_con, -Ys0},
					Zs = {con, - (Zp - XD)},	%Horizontal motion
					P4th = {no_con, -P4th0},
					P5th = {no_con, -P5th0},
					Count = Count0;
				?SDL_BUTTON_MMASK when ParaNum >= 4 ->
					Xs = {no_con, Xs0},
					Ys = {no_con, -Ys0},
					Zs = {no_con, -Zs0},
					P4th = {con, - (P4thp + XD)},
					P5th = {no_con, -P5th0},
					Count = Count0 + 1;
				?SDL_BUTTON_RMASK when ParaNum >= 5 ->
					Xs = {no_con, Xs0},
					Ys = {no_con, -Ys0},
					Zs = {no_con, -Zs0},
					P4th = {no_con, -P4th0},
					P5th = {con, - (P5thp + XD)},
					Count = Count0;
				_ ->
					Xs = {con, Xp + XD},
					Ys = {con, - (Yp + YD)},
					Zs = {no_con, -Zs0},
					P4th = {no_con, -P4th0},
					P5th = {no_con, -P5th0},
					Count = Count0
			end,
			wings_io:warp(OX, OY),

			% Ds means DragSummary
			Ds0 = mouse_scale([Xs,Ys,Zs,P4th,P5th], UnitScales),
			Ds1 = add_offset_to_drag_sum(Ds0, Unit, Offset),
			Ds = round_to_constraint(Unit, Ds1, Mod, []),

			Psum = [S || {_,S} <- [Xs,Ys,Zs,P4th,P5th]],
			[Xs2,Ys2,Zs2,P4th2,P5th2] = constrain_2(Unit, Psum, UnitScales, Offset),

			[Xs1,Ys1,Zs1,P4th1,P5th1] = scale_mouse_back(Ds, UnitScales, Offset),
			Move = constrain_1(Unit, Ds, Drag0),
			Drag = Drag0#drag{xs=Xs1,ys=-Ys1,zs=-Zs1,p4=-P4th1,p5=-P5th1,
							  psum=[Xs2,-Ys2,-Zs2,-P4th2,-P5th2],
							  xt=XD0,yt=YD0,mmb_timer=Count,mode_data=ModeData},
			{Move,Drag}
    end.

mouse_scale([{Tag,D}|Ds], [S|Ss]) ->
    [{Tag,D*S}|mouse_scale(Ds, Ss)];
mouse_scale(Ds, _) -> Ds.

scale_mouse_back([D|Ds], [S|Ss], [O|Ofs]) ->
    [(D-O)/S|scale_mouse_back(Ds,Ss,Ofs)];
scale_mouse_back(Ds, _, _) -> Ds.

constraints_scale([U0|_],Mod,[UnitScales|_]) ->
    case wings_pref:get_value(con_alternate) of
    true -> case constraint_factor_alt(clean_unit(U0),Mod) of
        none -> 1.0;
        {_,What} ->
          What*0.01/UnitScales
      end;
    false -> case constraint_factor(clean_unit(U0),Mod) of
        none -> 1.0;
        {_,What} ->
          What*0.01/UnitScales
      end
    end.

round_to_constraint([U0|Us], [{con,D0}|Ds], Mod, Acc) ->
    U = clean_unit(U0),
    D = case wings_pref:get_value(con_alternate) of
      true -> case constraint_factor_alt(U, Mod) of
        none -> D0;
        {F1,F2} ->
          round(D0*F1)*F2
        end;
      false -> case constraint_factor(U, Mod) of
        none -> D0;
        {F1,F2} ->
          round(D0*F1)*F2
            end
    end,
    round_to_constraint(Us, Ds, Mod, [D|Acc]);

round_to_constraint([_|Us], [{no_con, D}|Ds], Mod, Acc) ->
    round_to_constraint(Us, Ds, Mod, [D|Acc]);
round_to_constraint([], [{_,D}|Ds], Mod, Acc) ->
    round_to_constraint([], Ds, Mod, [D|Acc]);

round_to_constraint([_|_], [], _, Acc) -> reverse(Acc);
round_to_constraint([], [], _, Acc) -> reverse(Acc).

constrain_1([falloff], _, #drag{falloff=Falloff}) ->
    [Falloff];
constrain_1([_|Us], [D|Ds], Drag) ->
    [D|constrain_1(Us, Ds, Drag)];
constrain_1([], _, _) -> [].

constrain_2([U|Us], [D|Ds], [S|Ss], [O|Of]) ->
    [clamp_2(U, D*S+O, S, O, D)|constrain_2(Us, Ds, Ss, Of)];
constrain_2([], [D|Ds], [], Of) ->
    [D|constrain_2([], Ds, [], Of)];
constrain_2([],[],[],_) -> [].

add_offset_to_drag_sum([{Tag,D}|Ds], [U|Us], [O|Ofs]) ->
    [{Tag,clamp(U, D+O)}|add_offset_to_drag_sum(Ds, Us, Ofs)];
add_offset_to_drag_sum([{Tag,D}|Ds], _, _) ->
    [{Tag,D}|add_offset_to_drag_sum(Ds, [], [])];
add_offset_to_drag_sum([],_,_) -> [].

add_offset([D|Ds], [U|Us], [O|Ofs]) ->
    [clamp(U, D+O)|add_offset(Ds, Us, Ofs)];
add_offset([D|Ds], _, _) ->
    [D|add_offset(Ds, [], [])];
add_offset([],_,_) -> [].

clamp({_,{Min,_Max}}, D) when D < Min -> Min;
clamp({_,{_Min,Max}}, D) when D > Max -> Max;
clamp(_, D) -> D.

clamp_2({_,{Min,_Max}}, DSc, S, O, _) when DSc < Min -> (Min-O)/S;
clamp_2({_,{_Min,Max}}, DSc, S, O, _) when DSc > Max -> (Max-O)/S;
clamp_2(_, _, _, _, D) -> D.

constraint_factor(angle, Mod) ->
    RCS = wings_pref:get_value(con_rot_shift),
    RCC = wings_pref:get_value(con_rot_ctrl),
    RCCS = wings_pref:get_value(con_rot_ctrl_shift),
    RCA = wings_pref:get_value(con_rot_alt),
    RCCA= wings_pref:get_value(con_rot_ctrl_alt),
    RCSA = wings_pref:get_value(con_rot_shift_alt),
    RCCSA = wings_pref:get_value(con_rot_ctrl_shift_alt),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0 -> {1/RCCSA,RCCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/RCSA,RCSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/RCCA,RCCA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0 -> {1/RCCS,RCCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/RCC,RCC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/RCS,RCS};
      Mod band ?ALT_BITS =/= 0 -> {1/RCA,RCA};
      true -> none
    end;
constraint_factor(percent, Mod) ->
    SCS = wings_pref:get_value(con_scale_shift),
    SCC = wings_pref:get_value(con_scale_ctrl),
    SCCS = wings_pref:get_value(con_scale_ctrl_shift),
    SCA = wings_pref:get_value(con_scale_alt),
    SCCA = wings_pref:get_value(con_scale_ctrl_alt),
    SCSA = wings_pref:get_value(con_scale_shift_alt),
    SCCSA = wings_pref:get_value(con_scale_ctrl_shift_alt),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0-> {1/SCCSA,SCCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/SCSA,SCSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/SCCA,SCCA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?SHIFT_BITS =/= 0 -> {1/SCCS,SCCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/SCC,SCC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/SCS,SCS};
      Mod band ?ALT_BITS =/= 0 -> {1/SCA,SCA};
      true -> none
    end;
constraint_factor(_, Mod) ->
    DCS = wings_pref:get_value(con_dist_shift),
    DCC = wings_pref:get_value(con_dist_ctrl),
    DCCS = wings_pref:get_value(con_dist_ctrl_shift),
    DCA = wings_pref:get_value(con_dist_alt),
    DCCA = wings_pref:get_value(con_dist_ctrl_alt),
    DCSA = wings_pref:get_value(con_dist_shift_alt),
    DCCSA = wings_pref:get_value(con_dist_ctrl_shift_alt),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0-> {1/DCCSA,DCCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/DCSA,DCSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/DCCA,DCCA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?SHIFT_BITS =/= 0 -> {1/DCCS,DCCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/DCC,DCC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/DCS,DCS};
      Mod band ?ALT_BITS =/= 0 -> {1/DCA,DCA};
      true -> none
    end.

constraint_factor_alt(angle, Mod) ->
    RCRS = filter_angle(wings_pref:get_value(con_rot_shift)),
    RCRC = filter_angle(wings_pref:get_value(con_rot_ctrl)),
    RCRCS = filter_angle(wings_pref:get_value(con_rot_ctrl_shift)),
    RCRA = filter_angle(wings_pref:get_value(con_rot_alt)),
    RCRCA = filter_angle(wings_pref:get_value(con_rot_ctrl_alt)),
    RCRSA = filter_angle(wings_pref:get_value(con_rot_shift_alt)),
    RCRCSA = filter_angle(wings_pref:get_value(con_rot_ctrl_shift_alt)),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0 -> {1/RCRCSA,RCRCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/RCRSA,RCRSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/RCRCA,RCRCA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0 -> {1/RCRCS,RCRCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/RCRC,RCRC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/RCRS,RCRS};
      Mod band ?ALT_BITS =/= 0 -> {1/RCRA,RCRA};
      true -> none
    end;
constraint_factor_alt(percent, Mod) ->
    SCS = 1.0/wings_pref:get_value(con_scale_shift),
    SCC = 1.0/wings_pref:get_value(con_scale_ctrl),
    SCCS = 1.0/wings_pref:get_value(con_scale_ctrl_shift),
    SCA = 1.0/wings_pref:get_value(con_scale_alt),
    SCCA = 1.0/wings_pref:get_value(con_scale_ctrl_alt),
    SCSA = 1.0/wings_pref:get_value(con_scale_shift_alt),
    SCCSA = 1.0/wings_pref:get_value(con_scale_ctrl_shift_alt),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0-> {1/SCCSA,SCCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/SCSA,SCSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/SCCA,SCCA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?SHIFT_BITS =/= 0 -> {1/SCCS,SCCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/SCC,SCC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/SCS,SCS};
      Mod band ?ALT_BITS =/= 0 -> {1/SCA,SCA};
      true -> none
    end;
constraint_factor_alt(_, Mod) ->
    DCS = wings_pref:get_value(con_dist_a_shift),
    DCC = wings_pref:get_value(con_dist_a_ctrl),
    DCCS = wings_pref:get_value(con_dist_a_ctrl_shift),
    DCA = wings_pref:get_value(con_dist_a_alt),
    DCCA = wings_pref:get_value(con_dist_a_ctrl_alt),
    DCSA = wings_pref:get_value(con_dist_a_shift_alt),
    DCCSA = wings_pref:get_value(con_dist_a_ctrl_shift_alt),
    if
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0,
      Mod band ?CTRL_BITS =/= 0-> {1/DCCSA,DCCSA};
      Mod band ?SHIFT_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/DCSA,DCSA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?ALT_BITS =/= 0 -> {1/DCCA,DCCA};
      Mod band ?CTRL_BITS =/= 0,
      Mod band ?SHIFT_BITS =/= 0 -> {1/DCCS,DCCS};
      Mod band ?CTRL_BITS =/= 0 -> {1/DCC,DCC};
      Mod band ?SHIFT_BITS =/= 0 -> {1/DCS,DCS};
      Mod band ?ALT_BITS =/= 0 -> {1/DCA,DCA};
      true -> none
    end.

filter_angle(Degrees) ->
    case Degrees =/= 180.0 of
      true -> 180.0 - Degrees;
      false -> 180.0
    end.

%%%
%%% Update selection for new mouse position.
%%%
motion_update({_,undefined}, #drag{unit=Units,offset=Offset}=Drag) ->
    Move0 = lists:duplicate(length(Units),0.0),
    Move1 = constrain_1(Units, Move0, Drag),
    Move = add_offset(Move1, Units, Offset),
    motion_update(Move,Drag);

motion_update({no_change,LastMove}, #drag{unit=Units,mode_data={_,MD}}=Drag) ->
    Move = constrain_1(Units, LastMove, Drag),
    motion_update(Move,Drag#drag{mode_data=MD});
motion_update({no_change,_}, #drag{falloff=none}=Drag) ->
    Drag;
motion_update({no_change,LastMove}, #drag{unit=Units}=Drag) ->
    Move = constrain_1(Units, LastMove, Drag),
    motion_update(Move,Drag);
motion_update(Move, #drag{unit=Units,drag=DragFun}=Drag) ->
    DragFun(Move),
    Msg0 = progress_units(Units, Move),
    Msg = reverse(trim(reverse(lists:flatten(Msg0)))),
    Drag#drag{info=Msg,last_move=Move}.

default_drag_fun(Move) ->
    wings_dl:map(fun(D, _) ->
			 motion_update_fun(D, Move)
		 end, []).

motion_update_fun(#dlo{src_we=We,drag={matrix,Tr,Mtx0,_}}=D, Move) when ?IS_LIGHT(We) ->
    Mtx = Tr(Mtx0, Move),
    wings_light:update_matrix(D, Mtx);
motion_update_fun(#dlo{drag={matrix,Trans,Matrix0,_}}=D, Move) ->
    Matrix = Trans(Matrix0, Move),
    D#dlo{drag={matrix,Trans,Matrix0,Matrix}};
motion_update_fun(#dlo{drag={general,Fun}}=D, Move) ->
    Fun(Move, D);
motion_update_fun(#dlo{drag=#do{tr_fun=Tr,we_funs=WeFuns},
                       src_we=We0}=D0, Move) ->
    We = foldl(fun(WeFun, W) -> WeFun(W, Move) end, We0, WeFuns),
    D = D0#dlo{src_we=We},
    Vtab = Tr(Move, []),
    wings_draw:update_dynamic(D, Vtab);
motion_update_fun(D, _) -> D.

possible_falloff_update(_, #drag{falloff=none}=Drag) -> Drag;
possible_falloff_update(Move, Drag) ->
    NewFalloff = lists:last(Move),
    parameter_update(new_falloff, NewFalloff, Drag#drag{falloff=NewFalloff}).

parameter_update(Key, Val, Drag) ->
    wings_dl:map(fun(D, _) ->
			 parameter_update_fun(D, Key, Val)
		 end, []),
    {_,X,Y} = wings_wm:local_mouse_state(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    motion(Ev, Drag).

parameter_update_fun(#dlo{drag=#do{tr_fun=Tr0}=Do}=D, Key, Val) ->
    Tr = Tr0(Key, Val),
    D#dlo{drag=Do#do{tr_fun=Tr}};
parameter_update_fun(D, _, _) -> D.

translate({Xt0,Yt0,Zt0}, Dx, VsPos, Acc) ->
    Xt = Xt0*Dx, Yt = Yt0*Dx, Zt = Zt0*Dx,
    foldl(fun({V,{X,Y,Z}}, A) ->
		  Pos = wings_util:share(X+Xt, Y+Yt, Z+Zt),
		  [{V,Pos}|A]
	  end, Acc, VsPos).

progress_units([Unit|Units], [N|Ns]) ->
    [unit(clean_unit(Unit), N)|progress_units(Units, Ns)];
progress_units([], []) -> [].

clean_unit({Unit,_}) when is_atom(Unit) -> Unit;
clean_unit(Unit) when is_atom(Unit) -> Unit.

unit(angle, A) ->
    trim(io_lib:format("~10.2f~c  ", [A,?DEGREE]));
unit(number, N) ->
    ["N: "|trim(io_lib:format("~10.4f  ", [N]))];
unit(distance, D) ->
    ["D: "|trim(io_lib:format("~10.4f  ", [D]))];
unit(dx, D) ->
    ["DX: "|trim(io_lib:format("~10.4f  ", [D]))];
unit(dy, D) ->
    ["DY: "|trim(io_lib:format("~10.4f  ", [D]))];
unit(dz, D) ->
    ["DZ: "|trim(io_lib:format("~10.4f  ", [D]))];
unit(percent, P) ->
    trim(io_lib:format("~.2f%  ", [P*100.0]));
unit(falloff, R) ->
    ["R: "|trim(io_lib:format("~10.2f", [R]))];
unit(skip,_) ->
    %% the atom 'skip' can be used as a place holder. See wpc_arc.erl
    [];
unit(Unit, D) ->
    [wings_util:stringify(Unit) ++ ": "|trim(io_lib:format("~10.4f  ", [D]))].

trim([$\s|T]) -> trim(T);
trim([[_|_]=H|T]) ->
    case trim(H) of
	[] -> trim(T);
	S -> [S|T]
    end;
trim(S) -> S.

normalize(Move, #drag{mode_fun=ModeFun,mode_data=ModeData,
		      st=#st{shapes=Shs0}=St}) ->
    ModeFun(done, ModeData),
    gl:disable(gl_rescale_normal()),
    Shs = wings_dl:map(fun(D, Sh) ->
			       normalize_fun(D, Move, Sh)
		       end, Shs0),
    St#st{shapes=Shs}.

normalize_fun(#dlo{drag=none}=D, _Move, Shs) -> {D,Shs};
normalize_fun(#dlo{drag={matrix,_,_,_},transparent=#we{id=Id}=We,
		   proxy_data=PD}=D0, _Move, Shs0) when ?IS_LIGHT(We) ->
    Shs = gb_trees:update(Id, We, Shs0),
    D = D0#dlo{work=none,smooth=none,drag=none,src_we=We,transparent=false,
	   proxy_data=wings_proxy:invalidate(PD, dl)},
    {wings_draw:changed_we(D, D),Shs};
normalize_fun(#dlo{drag={matrix,_,_,Matrix},src_we=#we{id=Id}=We0,
		   proxy_data=PD}=D0,
	      _Move, Shs0) ->
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    D = D0#dlo{work=none,smooth=none,edges=none,sel=none,drag=none,src_we=We,
	       mirror=none,proxy_data=wings_proxy:invalidate(PD, dl)},
    {wings_draw:changed_we(D, D),Shs};
normalize_fun(#dlo{drag={general,Fun},src_we=#we{id=Id}=We}=D0, Move, Shs) ->
    D1 = Fun({finish,Move}, D0),
    D = D1#dlo{drag=none,sel=none},
    {wings_draw:changed_we(D, D),gb_trees:update(Id, We, Shs)};
normalize_fun(#dlo{src_we=#we{id=Id}}=D0, _Move, Shs) ->
    #dlo{src_we=We} = D = wings_draw:join(D0),
    {D,gb_trees:update(Id, We, Shs)}.

%%%
%%% Redrawing while dragging.
%%%

redraw(#drag{info=Info,st=St}) ->
    wings:redraw(Info, St).

clear_sel_dlists() ->
    wings_dl:map(fun clear_sel_dlists/2, []).

clear_sel_dlists(#dlo{drag=none}=D, _) -> D;
clear_sel_dlists(#dlo{drag={matrix,_,_,_}}=D, _) -> D;
clear_sel_dlists(D, _) -> D#dlo{sel=none}.
