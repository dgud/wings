%%
%%  wings_drag.erl --
%%
%%     This module handles interactive commands.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_drag).
-export([setup/3,setup/4,do_drag/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1,reverse/1,reverse/2,member/2]).

%% Main drag record. Kept in state.
-record(drag,
	{x,					%Original 2D position
	 y,
	 xs=0,					%Summary of mouse movements
	 ys=0,
	 zs=0,                  %Z move in screen relative
	 fp=0,                  %An optional forth drag parameter
	 psum=[0,0,0,0],        % Whereas xs,ys,zs and fp are the displayed distances
	                        % psum is the unconstrained mouse summary pre parameter 
	 xt=0,					%Last warp length
	 yt=0,
	 mmb_count=0,
	 fp_count=0,            % Rmb depression timer (forth parameter)
	 offset,				%Offset for each dimension.
	 unit,					%Unit that drag is done in.
	 unit_sc,				%Scales for each dimension.
	 flags=[],				%Flags.
	 falloff,				%Magnet falloff.
	 mode_fun,				%Special mode.
	 mode_data,				%State for mode.
	 info="",                               %Information line.
	 st,					%Saved st record.
	 last_move				%Last move.
	}).

%% Drag per object.
-record(do,
	{funs,					%List of transformation funs.
	 we_funs				%List of funs that operate on
						%  the We.
	}).

setup(Tvs, Unit, St) ->
    setup(Tvs, Unit, [], St).

setup(Tvs, Units, Flags, St) ->
    wings_io:grab(),
    wings_wm:grab_focus(),
    Offset0 = proplists:get_value(initial, Flags, []),
    Offset = pad_offsets(Offset0),
    UnitSc = unit_scales(Units),
    Falloff = falloff(Units),
    {ModeFun,ModeData} = setup_mode(Flags, Falloff),
    Drag = #drag{unit=Units,unit_sc=UnitSc,flags=Flags,offset=Offset,
		 falloff=Falloff,
		 mode_fun=ModeFun,mode_data=ModeData,
		 st=St},
    case Tvs of
	{matrix,TvMatrix} ->
	    wings_draw:refresh_dlists(St),
	    insert_matrix(TvMatrix);
	{general,General} ->
	    wings_draw:invalidate_dlists(St),
	    break_apart_general(General);
	_ = _BR ->
	    wings_draw:invalidate_dlists(St),
	    break_apart(Tvs, St)
    end,
    {drag,Drag}.

setup_mode(Flags, Falloff) ->
    case proplists:get_value(mode, Flags, none) of
	none ->
	    {standard_mode_fun(Falloff),none};
	{_,_}=Mode ->
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
    case wings_pref:get_value(drag_cam_dist_abs) of
      true ->
        #view{fov=Fov, distance=D} = wings_view:current(),
        (D/((11-Speed)*((11-Speed)*300)))*Fov/60;
      false ->
        1/((11-Speed)*80)
    end.

custom_relative_drag() ->
    Speed = wings_pref:get_value(drag_speed_relative),
    case wings_pref:get_value(drag_cam_dist_relative) of
      true ->
        #view{distance=D} = wings_view:current(),
        (D/((11-Speed)*((11-Speed)*900)));
      false ->
        1/((11-Speed)*100)
    end.

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
	L when L >= 4 -> Ds;
	L -> Ds ++ lists:duplicate(4-L, 0.0)
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
		 end, wings_util:rel2fam(Tvs)).

break_apart_1(#dlo{src_we=#we{id=Id}=We}=D0, [{Id,TvList0}|Tvs], St) ->
    TvList = mirror_constrain(TvList0, We),
    {Vs,FunList,WeFuns} = combine_tvs(TvList, We),
    D1 = if
	     ?IS_LIGHT(We) -> D0#dlo{split=none};
	     true -> D0
	 end,
    D = wings_draw:split(D1, Vs, St),
    Do = #do{funs=FunList,we_funs=WeFuns},
    {D#dlo{drag=Do},Tvs};
break_apart_1(D, Tvs, _) -> {D,Tvs}.

combine_tvs(TvList, #we{vp=Vtab}) ->
    {FunList,WeFuns,VecVs0} = split_tv(TvList, [], [], []),
    SS = sofs:from_term(VecVs0, [{vec,[vertex]}]),
    FF = sofs:relation_to_family(SS),
    FU = sofs:family_union(FF),
    VecVs1 = sofs:to_external(FU),
    Affected = foldl(fun({_,Vs}, A) -> Vs++A end, [], VecVs1),
    case insert_vtx_data(VecVs1, Vtab, []) of
	[] ->
	    combine_tv_1(FunList, WeFuns, Affected, []);
	VecVs ->
	    combine_tv_1(FunList, WeFuns, Affected, [translate_fun(VecVs)])
    end.

translate_fun(VecVs) ->
    fun(new_falloff, _Falloff) ->
	    translate_fun(VecVs);
       ([Dx|_], Acc) ->
	    foldl(fun({Vec,VsPos}, A) ->
			  translate(Vec, Dx, VsPos, A)
		  end, Acc, VecVs)
    end.

combine_tv_1([{Aff,Fun}|T], WeFuns, Aff0, FunList) ->
    combine_tv_1(T, WeFuns, Aff++Aff0, [Fun|FunList]);
combine_tv_1([], WeFuns, Aff, FunList) ->
    {Aff,FunList,WeFuns}.

split_tv([{we,F}|T], WeFacc, Facc, Vacc) when is_function(F, 2) ->
    split_tv(T, [F|WeFacc], Facc, Vacc);
split_tv([{_,F}=Fun|T], WeFacc, Facc, Vacc) when is_function(F, 2) ->
    split_tv(T, WeFacc, [Fun|Facc], Vacc);
split_tv([L|T], WeFacc, Facc, Vacc) when is_list(L) ->
    split_tv(T, WeFacc, Facc, L++Vacc);
split_tv([], WeFuns, Funs, VecVs) ->
    {Funs,WeFuns,VecVs}.

insert_vtx_data([{Vec,Vs0}|VecVs], Vtab, Acc) ->
    Vs = insert_vtx_data_1(Vs0, Vtab, []),
    insert_vtx_data(VecVs, Vtab, [{Vec,Vs}|Acc]);
insert_vtx_data([], _Vtab, Acc) -> Acc.

insert_vtx_data_1([V|Vs], Vtab, Acc) ->
    insert_vtx_data_1(Vs, Vtab, [{V,array:get(V, Vtab)}|Acc]);
insert_vtx_data_1([], _Vtab, Acc) -> Acc.

mirror_constrain(Tvs, #we{mirror=none}) ->
    Tvs;
mirror_constrain(Tvs, #we{mirror=Face}=We) ->
    M = wings_we:mirror_projection(We),
    Vs = wings_face:vertices_cw(Face, We),
    VsSet = ordsets:from_list(Vs),
    mirror_constrain_1(Tvs, VsSet, M, []).

mirror_constrain_1([{we,Tr}=Fun|Tvs], VsSet, M, Acc) when is_function(Tr) ->
    mirror_constrain_1(Tvs, VsSet, M, [Fun|Acc]);
mirror_constrain_1([{Vs,Tr0}=Fun|Tvs], VsSet, M, Acc) when is_function(Tr0) ->
    case ordsets:intersection(ordsets:from_list(Vs), VsSet) of
	[] ->
	    mirror_constrain_1(Tvs, VsSet, M, [Fun|Acc]);
	[_|_]=Mvs ->
	    Tr = constrain_fun(Tr0, M, Mvs),
	    mirror_constrain_1(Tvs, VsSet, M, [{Vs,Tr}|Acc])
    end;
mirror_constrain_1([VecVs0|Tvs], VsSet, M, Acc) ->
    VecVs1 = sofs:from_term(VecVs0, [{vec,[vertex]}]),
    VecVs2 = sofs:family_to_relation(VecVs1),
    VecVs3 = sofs:to_external(VecVs2),
    VecVs = mirror_constrain_2(VecVs3, VsSet, M, []),
    mirror_constrain_1(Tvs, VsSet, M, [VecVs|Acc]);
mirror_constrain_1([], _, _, Acc) -> Acc.

mirror_constrain_2([{Vec0,V}|T], VsSet, M, Acc) ->
    case member(V, VsSet) of
	false ->
	    mirror_constrain_2(T, VsSet, M, [{Vec0,[V]}|Acc]);
	true ->
	    Vec = e3d_mat:mul_vector(M, Vec0),
	    mirror_constrain_2(T, VsSet, M, [{Vec,[V]}|Acc])
    end;
mirror_constrain_2([], _, _, Acc) -> Acc.

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
    {GX,GY} = wings_wm:local2global(X, Y),
    {seq,push,handle_drag_event_1(Ev, Drag#drag{x=GX,y=GY})};
do_drag(#drag{unit=Units}=Drag0, Move) when length(Units) =:= length(Move) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    Ev = #mousemotion{x=X,y=Y,state=0},
    {GX,GY} = wings_wm:local2global(X, Y),
    Drag1 = motion(Ev, Drag0#drag{x=GX,y=GY}),
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
    {GX,GY} = wings_wm:local2global(X, Y),
    ungrab(Drag0#drag{x=GX,y=GY}),
    wings_wm:later(revert_state),
    keep.

gl_rescale_normal() ->
    case wings_gl:is_ext('GL_EXT_rescale_normal') of
	true -> ?GL_RESCALE_NORMAL;
	false -> ?GL_NORMALIZE
    end.

help_message(#drag{unit=Unit,mode_fun=ModeFun,mode_data=ModeData}) ->
    Accept = wings_msg:button_format(wings_s:accept()),
    ZMsg = zmove_help(Unit),
    FpMsg = fpmove_help(Unit),
    Cancel = wings_msg:button_format([], [],wings_s:cancel()),
    NumEntry = ?__(1,"Numeric Entry"),
    Tab = wings_util:key_format("[Tab]", NumEntry),
    Switch = switch(),
    Constraint = ?__(2,"Switch Constraint Set")++Switch,
    ShiftTab = wings_util:key_format("[Shift]+[Tab]", Constraint),
    Msg = wings_msg:join([Accept,ZMsg,FpMsg,Cancel,Tab,ShiftTab]),
    MsgRight = ModeFun(help, ModeData),
    wings_wm:message(Msg, MsgRight).

switch() ->
    case wings_pref:get_value(con_alternate) of
      true -> ?__(1," (using Alternate Constraints)");
      false -> []
    end.
	
zmove_help([_]) -> [];
zmove_help([_,_]) -> [];
zmove_help([_,_,falloff]) -> [];
zmove_help([_,_,dz|_]) ->
    zmove_help_1(?__(1,"Drag to move along Z"));
zmove_help([_,_,percent|_]) ->
    zmove_help_1(?__(3,"Drag to adjust Scale"));
zmove_help([_,_,skip|_]) -> [];
zmove_help([_,_,_|_]) ->
    zmove_help_1(?__(2,"Drag to adjust third parameter")).

fpmove_help([_]) -> [];
fpmove_help([_,_]) -> [];
fpmove_help([_,_,_]) -> [];
fpmove_help([_,_,_,falloff]) -> [];
fpmove_help([_,_,_,_,falloff]) ->
    fpmove_help_1(?__(1,"Drag to adjust forth parameter"));
fpmove_help([_,_,_,angle|_]) ->
    fpmove_help_1(?__(2,"Drag to adjust Rotation"));
fpmove_help([_,_,_,percent|_]) ->
    fpmove_help_1(?__(3,"Drag to adjust Scale"));
fpmove_help([_,_,skip,Type|_]) ->
    Str = wings_util:stringify(Type),
    Help = ?__(4,"Drag to adjust") ++ " " ++ Str,
    fpmove_help_1(Help);
fpmove_help([_,_,_,_|_]) ->
    fpmove_help_1(?__(1,"Drag to adjust forth parameter")).

zmove_help_1(Msg) ->
    case wings_pref:get_value(camera_mode) of
	tds -> wings_msg:mod_format(?CTRL_BITS, 3, Msg);
	blender -> wings_msg:mod_format(?CTRL_BITS, 3, Msg);
	sketchup -> wings_msg:mod_format(?CTRL_BITS, 3, Msg);
	_ -> wings_msg:mod_format(0, 2, Msg)
    end.
fpmove_help_1(Msg) ->
	wings_msg:mod_format(0, 3, Msg).

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
    {replace,fun(Ev) -> handle_drag_event(Ev, Drag) end}.

%%%% When the Rmb is pressed we store the time in order to determine if the
%%%% intention is to drag or cancel the command. Later, when the Rmb is released
%%%% we take the time again. If the time interval is short then the event is
%%%% handled as an intention to cancel the command.
handle_drag_event(#mousebutton{button=3,state=?SDL_PRESSED}=Ev,
		  #drag{fp_count=C}=Drag) when C == 0 ->
	StartTimer = now(),
    handle_drag_event(Ev, Drag#drag{fp_count=StartTimer});

handle_drag_event(#keyboard{sym=9, mod=Mod},Drag)->
    case Mod band ?SHIFT_BITS =/= 0 of
      true -> 
        case wings_pref:get_value(con_alternate) of
          true ->  wings_pref:set_value(con_alternate,false);
          false -> wings_pref:set_value(con_alternate,true)
        end, get_drag_event(Drag);
      false -> numeric_input(Drag)
    end;
handle_drag_event(#mousebutton{button=2,state=?SDL_RELEASED},
		  #drag{mmb_count=C,fp_count=0}=Drag) when C > 2 ->
    get_drag_event_1(Drag#drag{mmb_count=0,fp_count=0});
handle_drag_event(#mousebutton{button=3,state=?SDL_RELEASED,mod=Mod}=Ev,
		  #drag{mmb_count=C}=Drag) when C > 2 ->
    if
	Mod band ?CTRL_BITS =/= 0 ->
	    get_drag_event_1(Drag#drag{mmb_count=0,fp_count=0});
	true ->
	    handle_drag_event_0(Ev,Drag#drag{fp_count=0})
    end;

%%%% When Rmb is released we subtract the StartTime (when the Rmb was pressed)
%%%% from the Stop time (relased) and if the result is less than 16000 ms, then
%%%% we cancel the drag.
handle_drag_event(#mousebutton{button=3,state=?SDL_RELEASED},
          #drag{fp_count=StartTime}=Drag) when StartTime=/=0->
    Stop = now(),
    Time = timer:now_diff(Stop, StartTime),
    % io:format("Time ~p\n",[Time]),
    case Time < 250000 of
        false ->
            get_drag_event_1(Drag#drag{fp_count=0});
        true ->
            wings_dl:map(fun invalidate_fun/2, []),
            ungrab(Drag),
            wings_wm:later(revert_state),
            pop
	end;
handle_drag_event(#mousebutton{button=3,x=X,y=Y,mod=Mod,state=?SDL_RELEASED}, Drag0) ->
%%%% This function guards against reported crashes of the rmb being held, and
%%%% clicking the lmb. I can't reproduce this crash, but I don;t doubt that it
%%%% happens. The probable cause is likely to do with misinterpreted or conflated
%%%% mouse button hits. ~Richard Jones
    Ev = #mousemotion{x=X,y=Y,state=0,mod=Mod},
    Drag = ?SLOW(motion(Ev, Drag0)),
    quit_drag(Drag);

handle_drag_event(Event, Drag = #drag{st=St}) ->
    case wings_camera:event(Event, St, fun() -> redraw(Drag) end) of
	next -> handle_drag_event_0(Event, Drag);
	keep ->
	    %% Clear any potential marker for an edge about to be
	    %% cut (Cut RMB).
	    wings_dl:map(fun(#dlo{hilite=none}=D, _) -> D;
			    (D, _) -> D#dlo{hilite=none}
			 end, []),
	    %% Recalc unit_scales since zoom can have changed.   UNITS ARE MIXED IN SOME CASES
	    #drag{xs=Xs0,ys=Ys0,zs=Zs0,fp=Fp0,psum=Psum0,unit=Unit,
	          unit_sc=Us0} = Drag,
	    Us = unit_scales(Unit),
		Psum = adjust_unit_scaling(Psum0,Us,Us0),
		[Xs,Ys,Zs,Fp] = adjust_unit_scaling([Xs0,Ys0,Zs0,Fp0],Us,Us0),
	    get_drag_event(Drag#drag{xs=Xs,ys=Ys,zs=Zs,fp=Fp,psum=Psum,unit_sc=Us});
	Other ->
	    %% Clear any potential marker for an edge about to be
	    %% cut (Cut RMB).
	    wings_dl:map(fun(#dlo{hilite=none}=D, _) -> D;
			    (D, _) -> D#dlo{hilite=none}
			 end, []),
	    Other
    end.

handle_drag_event_0(#keyboard{unicode=C}=Ev,
		    #drag{mode_fun=ModeFun,mode_data=ModeData0}=Drag0) ->
    case ModeFun({key,C}, ModeData0) of
	none -> handle_drag_event_1(Ev, Drag0);
	ModeData ->
	    wings_wm:dirty(),
	    wings_wm:message_right(ModeFun(help, ModeData)),
	    Val = {ModeData,Drag0#drag.falloff},
	    Drag1 = case wings_pref:get_value(drag_resets) of %% Probably Remove This %%         
	        false -> parameter_update(new_mode_data, Val,
	                 Drag0#drag{mode_data={changed,ModeData}});
	        true -> parameter_update(new_mode_data, Val,
	                Drag0#drag{mode_data=ModeData,xs=0,ys=0,zs=0,fp=0,
	                           psum=[0,0,0,0]})
	    end,
	    Drag = case ModeFun(units, ModeData) of
		       none -> Drag1;
		       Units ->
		           Us = unit_scales(Units),
		           #drag{xs=Xs0,ys=Ys0,zs=Zs0,fp=Fp0,psum=Psum0,
		                 unit_sc=Us0} = Drag1,
		           Psum = adjust_unit_scaling(Psum0,Us,Us0),
		           [Xs,Ys,Zs,Fp] = adjust_unit_scaling([Xs0,Ys0,Zs0,Fp0],Us,Us0),
		           Drag1#drag{xs=Xs,ys=Ys,zs=Zs,fp=Fp,psum=Psum,unit=Units,
		                      unit_sc=Us}
		   end,
	    get_drag_event(Drag)
    end;
handle_drag_event_0(Ev, Drag) -> handle_drag_event_1(Ev, Drag).

handle_drag_event_1(redraw, Drag) ->
    help_message(Drag),
    redraw(Drag),
    get_drag_event_1(Drag);
handle_drag_event_1(#mousemotion{}=Ev, Drag0) ->
    Drag = motion(Ev, Drag0),
    get_drag_event(Drag);
handle_drag_event_1(#mousebutton{button=1,x=X,y=Y,mod=Mod,state=?SDL_RELEASED}, Drag0) ->
    Ev = #mousemotion{x=X,y=Y,state=0,mod=Mod},
    Drag = ?SLOW(motion(Ev, Drag0)),
    quit_drag(Drag);
handle_drag_event_1({drag_arguments,Move}, Drag0) ->
    ungrab(Drag0),
    Drag1 = possible_falloff_update(Move, Drag0),
    Drag = ?SLOW(motion_update(Move, Drag1)),
    St = normalize(Move, Drag),
    DragEnded = {new_state,St#st{drag_args=Move}},
    wings_wm:later(DragEnded),
    pop;

handle_drag_event_1(view_changed, Drag) ->
    get_drag_event(view_changed(Drag));
handle_drag_event_1({action,{drag_arguments,_}=DragArgs}, _) ->
    wings_wm:later(DragArgs);
handle_drag_event_1(Event, #drag{st=St}=Drag0) ->
    case wings_hotkey:event(Event,St) of
	next ->
	    get_drag_event(Drag0);
	{view,quick_preview} ->
	    get_drag_event(Drag0);
	{view,smooth_proxy} ->
	    get_drag_event(Drag0);
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
    wings_wm:later(DragEnded),
    pop.

ungrab(#drag{x=Ox,y=Oy}) ->
    wings_wm:release_focus(),
    wings_io:ungrab(Ox, Oy).

invalidate_fun(#dlo{drag=none}=D, _) -> D;
invalidate_fun(#dlo{src_we=We}=D, _) ->
    wings_draw:abort_split(D#dlo{src_we=We#we{es=array:new()}}).

numeric_input(Drag0) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    Ev = #mousemotion{x=X,y=Y,state=0,mod=0},
    {Move0,Drag} = case mouse_translate(Ev, Drag0) of
	    {{_,M},D} -> {M,D};
		Other -> Other
	end,
    wings_ask:dialog(?__(1,"Numeric Input"),
		     make_query(Move0, Drag),
		     fun(Res) ->
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
	U ->
	    [{hframe,[{label,qstr(U)},{text,V,qrange(U0)}]}|
	     make_query_1(Units, Vals)]
    end;
make_query_1([], []) -> [].

qstr(distance) -> ?__(1,"Dx");
qstr(dx) -> ?__(2,"Dx");
qstr(dy) -> ?__(3,"Dy");
qstr(dz) ->  ?__(4,"Dz");
qstr(falloff) ->  ?__(5,"R");
qstr(angle) ->	?__(6,"A");
qstr(Atom) -> wings_util:stringify(Atom).

qrange({_,{_,_}=Range}) -> [{range,Range}];
qrange(_) -> [].

percent_qrange({_,{Min,Max}}) ->
    [{range,{safe_mul_100(Min),safe_mul_100(Max)}}];
percent_qrange(_) -> [].

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
	    case member(keep_drag,Flags) of
	      true ->
	        wings_dl:map(fun view_changed_fun/2, []),
	        {_,X,Y} = wings_io:get_mouse_state(),
	        Drag0#drag{x=X,y=Y};
	      false ->
	    wings_dl:map(fun view_changed_fun/2, []),
	    {_,X,Y} = wings_io:get_mouse_state(),
	        Drag0#drag{x=X,y=Y,xs=0,ys=0,zs=0,fp=0,psum=[0,0,0,0]}
	    end
    end.

view_changed_fun(#dlo{drag={matrix,Tr,_,_},transparent=#we{}=We}=D, _) ->
    Id = e3d_mat:identity(),
    {D#dlo{src_we=We,drag={matrix,Tr,Id,Id}},[]};
view_changed_fun(#dlo{drag={matrix,Tr,_,Mtx}}=D, _) ->
    {D#dlo{drag={matrix,Tr,Mtx,Mtx}},[]};
view_changed_fun(#dlo{drag=#do{funs=Tv0}=Do,src_we=We}=D, _) ->
    Tv = update_tvs(Tv0, We, []),
    {D#dlo{drag=Do#do{funs=Tv}},[]};
view_changed_fun(D, _) -> {D,[]}.

update_tvs([F0|Fs], NewWe, Acc) ->
    F = F0(view_changed, NewWe),
    update_tvs(Fs, NewWe, [F|Acc]);
update_tvs([], _, Acc) -> reverse(Acc).

motion(Event, Drag0) ->
    {Move,Drag} = mouse_translate(Event, Drag0),
    motion_update(Move, Drag).

mouse_translate(Event0, Drag0) ->
    Mode = wings_pref:get_value(camera_mode),
    {Event,Mod,Drag1} = mouse_pre_translate(Mode, Event0,Drag0),
	mouse_range(Event, Drag1, Mod).

mouse_pre_translate(Mode, #mousemotion{state=Mask,mod=Mod}=Ev,Drag)
        when Mode==blender; Mode==sketchup; Mode==tds ->
    if
    Mask band ?SDL_BUTTON_RMASK =/= 0,
    Mod band ?CTRL_BITS =/= 0 ->
        {Ev#mousemotion{state=?SDL_BUTTON_MMASK},
         Mod band (bnot ?CTRL_BITS),Drag};
    true -> {Ev,Mod,Drag}
    end;

mouse_pre_translate(_, #mousemotion{mod=Mod}=Ev,Drag) ->
    {Ev,Mod,Drag}.

mouse_range(#mousemotion{x=X0, y=Y0, state=Mask},
            #drag{x=OX, y=OY,
                   xs=Xs0, ys=Ys0, zs=Zs0, fp=Fp0,
                   psum=Psum0,
                   mode_data=MD,
                   xt=Xt0, yt=Yt0, mmb_count=Count0,
                   unit_sc=UnitScales, unit=Unit, offset=Offset,
                   last_move=LastMove}=Drag0,
			Mod) ->
    %%io:format("Mouse Range ~p ~p~n", [{X0,Y0}, {OX,OY,Xs0,Ys0}]),
    [Xp,Yp,Zp,Fpp] = case Mod =/= 0 of
        true -> Psum0;
        false -> [Xs0,Ys0,Zs0,Fp0]
    end,
    {X,Y} = wings_wm:local2global(X0, Y0),

    case wings_pref:lowpass(X-OX, Y-OY) of
    {0,0} ->
        Drag = Drag0#drag{xt=0,yt=0},
        {{no_change,LastMove},Drag};

    {XD0,YD0} ->

        CS = constraints_scale(Unit,Mod,UnitScales),
        XD = CS*(XD0 + Xt0),
        YD = CS*(YD0 + Yt0),

        ModeData = case MD of
          {_,MD0} -> MD0;
          MD0 -> MD0
        end,

        case {Mask band ?SDL_BUTTON_MMASK =/= 0,Mask band ?SDL_BUTTON_RMASK =/= 0} of
          {true,false} ->
            Xs = {no_con, Xs0},
            Ys = {no_con, -Ys0},
            Zs = case wings_pref:get_value(camera_mode) of
                   maya -> {con, - (Zp - XD)};	%Horizontal motion
                   _cam -> {con, - (Zp + YD)}	%Vertical motion
                 end,
            Fp = {no_con, -Fp0},
            Count = Count0 + 1;
          {false,true} ->
            Xs = {no_con, Xs0},
            Ys = {no_con, -Ys0},
            Zs = {no_con, -Zs0},
            Fp = {con, - (Fpp + XD)},
            Count = Count0;
          {_,_} ->
            Xs = {con, Xp + XD},
            Ys = {con, - (Yp + YD)},
            Zs = {no_con, -Zs0},
            Fp = {no_con, -Fp0},
            Count = Count0
        end,
        wings_io:warp(OX, OY),

        % Ds means DragSummary
        Ds0 = mouse_scale([Xs,Ys,Zs,Fp], UnitScales),
        Ds1 = add_offset_to_drag_sum(Ds0, Unit, Offset),
        Ds = round_to_constraint(Unit, Ds1, Mod, []),

        Psum = [S || {_,S} <- [Xs,Ys,Zs,Fp]],
        [Xs2,Ys2,Zs2,Fp2] = constrain_2(Unit, Psum, UnitScales, Offset),

        [Xs1,Ys1,Zs1,Fp1] = scale_mouse_back(Ds, UnitScales, Offset),
        Move = constrain_1(Unit, Ds, Drag0),
        Drag = Drag0#drag{xs=Xs1,ys=-Ys1,zs=-Zs1,fp=-Fp1,
                          psum=[Xs2,-Ys2,-Zs2,-Fp2],
                          xt=XD0,yt=YD0,mmb_count=Count,mode_data=ModeData},
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

motion_update(Move, #drag{unit=Units}=Drag) ->
    wings_dl:map(fun(D, _) ->
			 motion_update_fun(D, Move)
		 end, []),
    Msg0 = progress_units(Units, Move),
    Msg = reverse(trim(reverse(lists:flatten(Msg0)))),
    Drag#drag{info=Msg,last_move=Move}.

motion_update_fun(#dlo{src_we=We,drag={matrix,Tr,Mtx0,_}}=D, Move) when ?IS_LIGHT(We) ->
    Mtx = Tr(Mtx0, Move),
    wings_light:update_matrix(D, Mtx);
motion_update_fun(#dlo{drag={matrix,Trans,Matrix0,_}}=D, Move) ->
    Matrix = Trans(Matrix0, Move),
    D#dlo{drag={matrix,Trans,Matrix0,Matrix}};
motion_update_fun(#dlo{drag={general,Fun}}=D, Move) ->
    Fun(Move, D);
motion_update_fun(#dlo{drag=#do{funs=Tv,we_funs=WeFuns},src_we=We0}=D0, Move) ->
    We = foldl(fun(WeFun, W) -> WeFun(W, Move) end, We0, WeFuns),
    D = D0#dlo{src_we=We},
    Vtab = foldl(fun(F, A) -> F(Move, A) end, [], Tv),
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

parameter_update_fun(#dlo{drag=#do{funs=Tv0}=Do}=D, Key, Val) ->
    Tv = foldl(fun(F, A) -> [F(Key, Val)|A] end, [], Tv0),
    D#dlo{drag=Do#do{funs=Tv}};
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
clear_sel_dlists(#dlo{drag={matrix,_,_}}=D, _) -> D;
clear_sel_dlists(D, _) -> D#dlo{sel=none}.
