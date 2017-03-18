%%
%%  wpc_sculpt.erl --
%%
%%     A plugin to add a few basic sculpting tools.
%%
%%  Copyright (c) 2010-2012 Richard Jones
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_sculpt).
-export([init/0,menu/2,command/2]).
-export([update_dlist/3,draw/5,get_data/3]).

-export([sculpt_menu/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include_lib("wings/src/wings.hrl").

-import(lists, [foldl/3,sort/1,reverse/1,member/2]).

-record(sculpt,
    {mode,             % pull, pinch, smooth
     active=false,     % sculpt active true|false
     id=none,          % id of initial object
     mag,              % magnet true|false
     rad,              % magnet radius
     mag_type,         % magnet type
     str,              % strength
     mir,              % mirror info
     locked,           % masked vertices
     st,               % state
     wst,              % working state
     ost}).            % original state

init() ->
    wings_pref:set_default(sculpt_strength, 0.005),
    wings_pref:set_default(sculpt_mode, pull),
    wings_pref:set_default(sculpt_magnet, {false,1.0}),
    wings_pref:set_default(sculpt_magnet_type, dome),
    wings_pref:set_default(sculpt_initial, false),
    wings_pref:set_default(sculpt_constraint_axis, none),
    wings_pref:set_default(sculpt_magnet_influence, true),
    %% Delete old prefs
    wings_pref:delete_value(sculpt_magnet_color),
    true.

menu({tools}, Menu) ->
    reverse(parse(Menu, [], false));
menu(_, Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [sculpt_heading()|NewMenu];
parse([{_,tweak_menu,_}=T|Rest], NewMenu, false) ->
    parse(Rest, [sculpt_heading(),T|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

sculpt_heading() ->
    {?__(1,"Sculpt"),sculpt,?__(2,"Enter sculpt mode")}.

command({tools,sculpt}, St) ->
    sculpt_mode_setup(St);
command({sculpt,_}, St) -> St;
command(_,_) -> next.

sculpt_mode_setup(#st{shapes=Shs}=St0) ->
    wings_tweak:toggle_draw(false),
    St = wings_undo:init(St0#st{selmode=face,sel=[],sh=false}),
    Mir = mirror_info(Shs, []),
    Mode = wings_pref:get_value(sculpt_mode),
    Str = wings_pref:get_value(sculpt_strength),
    {Mag,Rad} = wings_pref:get_value(sculpt_magnet),
    MagType = wings_pref:get_value(sculpt_magnet_type),
    Lv = shape_attr(gb_trees:to_list(Shs)),
    wings_pref:set_default(sculpt_current_id, none),
    Sc = #sculpt{mode=Mode,mir=Mir,str=Str,mag=Mag,rad=Rad,mag_type=MagType,
          locked=Lv,st=St,wst=St,ost=St0},
    wings:mode_restriction([face]),
    wings_wm:dirty(),
    {seq,push,update_sculpt_handler(Sc)}.

shape_attr(S) ->
    L = foldl(fun
        ({_,We}, Acc) when ?IS_LIGHT(We) -> Acc;
        ({Id,#we{perm=P,pst=Pst}}, Lv0) when ?IS_SELECTABLE(P) ->
            locked_vs(Pst, Id, Lv0);
        (_, Acc) -> Acc
    end, [], S),
    reverse(L).

locked_vs(Pst, Id, Lv) ->
    case wings_pref:get_value(magnet_mask_on) of
      true ->
        case gb_trees:is_defined(wpc_magnet_mask, Pst) of
          true ->
            [{Id,gb_sets:to_list(wpc_magnet_mask:get_locked_vs(Pst))}|Lv];
          _otherwise ->
            Lv
        end;
      false -> Lv
    end.

update_sculpt_handler(Sc) ->
    {replace,fun(Ev) ->
        handle_sculpt_event_0(Ev, Sc)
    end}.

handle_sculpt_event_0(redraw, #sculpt{st=St}=Sc) ->
    wings:redraw("",St),
    help(Sc),
    update_sculpt_handler(Sc);
handle_sculpt_event_0(Ev, #sculpt{active=true}=Sc) ->
    handle_sculpt_event_1(Ev, Sc);
handle_sculpt_event_0({update_state,St}, Sc) ->
    wings_draw:refresh_dlists(St),
    wings_wm:current_state(St),
    update_sculpt_handler(Sc#sculpt{st=St,wst=St});
handle_sculpt_event_0({current_state,St}, #sculpt{st=St}) ->
    keep;
handle_sculpt_event_0({current_state,#st{shapes=Shs}=St1},
  #sculpt{st=St0}=Sc) ->
    Mir = mirror_info(Shs, []),
    Lv = shape_attr(gb_trees:to_list(Shs)),
    St = wings_undo:save(St0, St1),
    update_sculpt_handler(Sc#sculpt{locked=Lv,mir=Mir,st=St,wst=St});
handle_sculpt_event_0({new_state,#st{shapes=Shs}=St1},
  #sculpt{st=St0}=Sc) ->
    Mir = mirror_info(Shs, []),
    Lv = shape_attr(gb_trees:to_list(Shs)),
    St = wings_undo:save(St0, St1#st{sel=[],selmode=face,sh=false}),
    wings_draw:refresh_dlists(St),
    wings_wm:current_state(St),
    update_sculpt_handler(Sc#sculpt{locked=Lv,mir=Mir,st=St,wst=St});
handle_sculpt_event_0(Ev, #sculpt{st=St}=Sc) ->
    case wings_camera:event(Ev, St) of
      next -> handle_sculpt_event_1(Ev, Sc);
      Other -> Other
    end.

handle_sculpt_event_1({adv_menu_abort,_}, Sc) ->
    update_sculpt_handler(Sc#sculpt{active=true});
handle_sculpt_event_1({note,menu_aborted}, Sc) ->
    update_sculpt_handler(Sc#sculpt{active=true});
handle_sculpt_event_1(#mousemotion{x=X,y=Y}, #sculpt{active=true}=Sc) ->
    do_sculpt(X, Y, Sc);
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},
		      #sculpt{st=St,wst=St,active=true}=Sc) ->
    update_sculpt_handler(Sc#sculpt{id=none,active=false});
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},
		      #sculpt{active=true}=Sc) ->
    #sculpt{st=#st{shapes=Shs},wst=St0} =Sc0=clear_influence(Sc),
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_draw:refresh_dlists(St),
    wings_wm:current_state(St),
    wings_wm:dirty(), % it was necessary when I tested in a Intel video card
    update_sculpt_handler(Sc0#sculpt{id=none,st=St,wst=St,active=false});
handle_sculpt_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
		      #sculpt{st=St}=Sc) ->
    do_sculpt(X, Y, Sc#sculpt{wst=St,active=true});
handle_sculpt_event_1(#mousebutton{button=3,mod=Mod,x=X,y=Y,state=?SDL_RELEASED}, Sc)
  when Mod band ?ALT_BITS =/= 0 ->
    sculpt_menu(X, Y, Sc);
handle_sculpt_event_1(#keyboard{sym=Sym,mod=Mod,state=?SDL_PRESSED}=Ev, #sculpt{st=St}=Sc) ->
    case is_altkey_magnet_event(Sym,Mod) of
	true ->
	    {_,X,Y} = wings_wm:local_mouse_state(),
	    case wings_pick:do_pick(X,Y,St) of
		{_, _, _} ->
		    adjust_magnet(X, Y, Sc);
		none ->
		    keep
	    end;
	_ ->
	    handle_key(Sym, Ev, Sc)
    end;
handle_sculpt_event_1({action,Action}, Sc) ->
    command_handling(Action, Sc);
handle_sculpt_event_1(got_focus, #sculpt{st=St}=Sc) ->
    Str = wings_pref:get_value(sculpt_strength),
    wings_wm:dirty(),
    update_sculpt_handler(Sc#sculpt{id=none,st=St#st{selmode=face,sel=[],sh=false},active=false,str=Str});
handle_sculpt_event_1(lost_focus, #sculpt{st=#st{shapes=Shs},wst=St0,active=true}=Sc) ->
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_wm:current_state(St),
    wings_wm:send(geom,{update_state,St}),
    update_sculpt_handler(Sc#sculpt{id=none,st=St,wst=St,active=false});
handle_sculpt_event_1(quit=Ev, Sc) ->
    wings_wm:later(Ev),
    exit_sculpt(Sc);
handle_sculpt_event_1(_,_) ->
    keep.

%%%
%%% Adjust Magnet
%%%

adjust_magnet(X, Y, Sc) ->
    wings_io:grab(),
    wings_io:change_event_handler(?SDL_KEYUP, true),
    update_magnet_handler(X, Y, Sc).

update_magnet_handler(X, Y, Sc) ->
    wings_wm:dirty(),
    {replace,fun(Ev) ->
        handle_magnet_event(Ev, X, Y, Sc) end}.

handle_magnet_event(redraw, X, Y, #sculpt{st=St}=Sc) ->
    wings_draw:refresh_dlists(St),
    wings:redraw("", St),
    help(Sc),
    draw_magnet(X, Y, Sc),
    update_magnet_handler(X, Y, Sc);
handle_magnet_event(#mousemotion{x=X}, X0, Y0, Sc0) ->
    DX = X-X0, %since last move X
    wings_io:warp(X0,Y0),
    Sc = adjust_magnet_radius(DX, Sc0),
    update_magnet_handler(X0, Y0, Sc);
handle_magnet_event(#mousebutton{button=4,state=?SDL_RELEASED}, X, Y, Sc0) ->
    update_magnet_handler(X, Y, adjust_strength(1, Sc0));
handle_magnet_event(#mousebutton{button=5,state=?SDL_RELEASED}, X, Y, Sc0) ->
    update_magnet_handler(X, Y, adjust_strength(-1, Sc0));
handle_magnet_event(#mousebutton{button=Button}, X, Y, Sc)
  when Button =:= 4; Button =:= 5 ->
    update_magnet_handler(X, Y, Sc);
handle_magnet_event(#mousebutton{}=Ev, X, Y, Sc) ->
    wings_wm:later(Ev),
    end_magnet_event(X, Y, Sc);
handle_magnet_event(#keyboard{sym=Sym,state=?SDL_RELEASED}, X, Y, Sc)
  when Sym =:= ?SDLK_LALT; Sym =:= ?SDLK_RALT ->
    end_magnet_event(X, Y, Sc);
handle_magnet_event(#keyboard{}, X, Y, Sc) ->
    update_magnet_handler(X, Y, Sc);
handle_magnet_event(_, X, Y, Sc) ->
    end_magnet_event(X, Y, Sc).

end_magnet_event(X, Y, #sculpt{str=Str}=Sc) ->
    wings_io:change_event_handler(?SDL_KEYUP, false),
    wings_pref:set_value(sculpt_strength, Str),
    wings_io:ungrab(X, Y),
    wings_wm:dirty(),
    update_sculpt_handler(Sc).

%%%
%%% Adjust Radius
%%%

adjust_magnet_radius(0, Sc) ->
    Sc;
adjust_magnet_radius(MouseMovement, #sculpt{rad=Rad0}=Sc)
  when MouseMovement < 0 ->
    case Rad0 - 0.05*Rad0 of
    Rad when Rad >= 0.01 ->
        Sc#sculpt{rad=Rad};
    _otherwise ->
        Sc#sculpt{rad=0.01}
    end;
adjust_magnet_radius(_, #sculpt{rad=Rad0}=Sc) ->
    Rad = Rad0 + 0.05*Rad0,
    Sc#sculpt{rad=Rad}.

%%%
%%% Adjust Strength
%%%

adjust_strength(MouseMovement, #sculpt{str=Str0}=Sc) when MouseMovement < 0 ->
    case Str0 - strength_increment() of
        Str when Str > 0.001 ->
            Sc#sculpt{str=Str};
        _otherwise -> Sc#sculpt{str=0.001}
    end;
adjust_strength(_, #sculpt{str=Str0}=Sc) ->
    case Str0 + strength_increment() of
        Str when Str < 0.1 ->
            Sc#sculpt{str=Str};
        _otherwise -> Sc#sculpt{str=0.1}
    end.

strength_increment() ->
    case wings_io:is_modkey_pressed(?SHIFT_BITS) of
        true -> 0.01;
        false -> 0.001
    end.

%%%
%%% Draw Magnet
%%%

draw_magnet(X, Y, #sculpt{rad=Rad,str=Str,st=#st{shapes=Shs}=St}) ->
    {Xm,Ym,Zm} = case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
		     {_,Side,{Id,Face}} ->
			 #we{mirror=Mir}=We = gb_trees:get(Id, Shs),
			 Point = wings_face:center(Face, We),
			 case Side of
			     mirror ->
				 Mnorm = wings_face:normal(Mir, We),
				 PointOnPlane = wings_face:center(Mir, We),
				 Dist = dist_along_vector(Point, PointOnPlane, Mnorm),
				 e3d_vec:add_prod(Point, Mnorm, Dist * -2);
			     original -> Point
			 end;
		     none ->
			 {0.0,0.0,0.0}
		 end,
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_view:load_matrices(false),
    P = Str*10,
    wings_io:set_color({1.0*P,0.0,1.0/P,0.1}),
    gl:translatef(Xm, Ym, Zm),
    Obj = glu:newQuadric(),
    glu:sphere(Obj, Rad, 40, 40),
    glu:deleteQuadric(Obj),
    gl:popAttrib().

dist_along_vector({Xa,Ya,Za},{Xb,Yb,Zb},{Vx,Vy,Vz}) ->
%% Return Distance between PosA and PosB along Normalized Vector
    Vx*(Xa-Xb)+Vy*(Ya-Yb)+Vz*(Za-Zb).

%%%
%%% Sculpt
%%%

do_sculpt(X, Y, Sc) ->
    case sculpt(X, Y, Sc) of
      keep -> keep;
      {St,Id0} ->
        wings_draw:refresh_dlists(St),
        wings_wm:dirty(),
        Id = case wings_pref:get_value(sculpt_initial) of
            true -> Id0;
            false -> none
        end,
        update_sculpt_handler(Sc#sculpt{id=Id,st=St})
    end.

sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str0,mag=false,mode=smooth,locked=Locked,
  st=#st{shapes=Shs0}=St}) ->
%% Smooth mode
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        Lvs = lookup_locked_vs(Id, Locked),
        Str = Str0*10,
        #we{vp=Vtab0}=We = gb_trees:get(Id, Shs0),
        Vtab = wings_face:fold_faces(fun
            (_, V, _, _, Vtab1) ->
                case ordsets:is_element(V, Lvs) of
                    true -> Vtab1;
                    false ->
                        Pos = array:get(V, Vtab0),
                        smooth(V, Pos, 1, Str, Mir, We, Vtab1)
                end
            end, Vtab0, [Face], We),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str0,rad=Rad,mag_type=MagType,mode=smooth,
  locked=Locked,st=#st{shapes=Shs1}=St}) ->
%% Smooth mode with magnet
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        Shs0=check_is_same_id(Id,Shs1),
        We = gb_trees:get(Id, Shs1),
        {Positions,_} = vpos(Face, We),
        Cnt = e3d_vec:average(Positions),
        {VsDyn,Vtab} = smooth_magnetic(Locked,Str0,Rad,Cnt,MagType,Mir,We),
        NewPst = set_edge_influence(VsDyn,We#we{vp=Vtab}),
        Shs = gb_trees:update(Id, We#we{vp=Vtab,pst=NewPst}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str,mag=false,mode=pinch,locked=Locked,
  st=#st{shapes=Shs0}=St}) ->
%% Pinch mode
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        Lvs = lookup_locked_vs(Id, Locked),
        #we{vp=Vtab0}=We = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We),
        Cnt = e3d_vec:average(Positions),
        Vtab = wings_face:fold_faces(fun
            (_, V, _, _, Vtab1) ->
                case ordsets:is_element(V, Lvs) of
                    true -> Vtab1;
                    false ->
                        Pos = array:get(V, Vtab0),
                        Vtab2 = pinch(V, Pos, Cnt, 1, Str, Mir, We, Vtab1),
                        case wings_io:is_modkey_pressed(?SHIFT_BITS) of
                            false ->
                              Vtab2;
                            true ->
                              NewPos = array:get(V, Vtab2),
                              smooth(V, NewPos, 1, Str*10, Mir, We, Vtab2)
                        end
                end
            end, Vtab0, [Face], We),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str,rad=Rad,mag_type=MagType,mode=pinch,
  locked=Locked,st=#st{shapes=Shs1}=St}) ->
%% Pinch mode with magnet
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        Smooth=wings_io:is_modkey_pressed(?SHIFT_BITS),
        Shs0=check_is_same_id(Id,Shs1),
        #we{vp=Vtab0}=We = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We),
        Cnt = {Cx,Cy,Cz} = e3d_vec:average(Positions),
        Lvs = lookup_locked_vs(Id, Locked),
        {VsDyn,Vtab} = case Smooth of
          true ->
            smooth_magnetic(Locked,Str,Rad,Cnt,MagType,Mir,We);
          _ ->
            array:sparse_foldl(fun
            (V, Pos={Px,Py,Pz}, {VsDyn0,Vtab1}) ->
                case ordsets:is_element(V, Lvs) of
                    true -> {VsDyn0,Vtab1};
                    false ->
                        case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
                            true ->
                                Dist = e3d_vec:dist(Pos, Cnt),
                                case Dist =< Rad of
                                    true ->
                                        Inf = magnetic_influence(MagType, Dist, Rad),
                                        VsDyn1=VsDyn0++[{V,Inf}],
                                        {VsDyn1,pinch(V, Pos, Cnt, Inf, Str, Mir, We, Vtab1)};
                                    false -> {VsDyn0,Vtab1}
                                end;
                            false -> {VsDyn0,Vtab1}
                        end
                end
            end, {[],Vtab0}, Vtab0)
        end,
        NewPst = set_edge_influence(VsDyn,We),
        Shs = gb_trees:update(Id, We#we{vp=Vtab,pst=NewPst}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;

sculpt(X, Y, #sculpt{id=ID,locked=Locked,mir=Mir,str=Str,mag=false,
  st=#st{shapes=Shs0}=St}) ->
%% Push and Pull mode
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        #we{vp=Vtab0}=We = gb_trees:get(Id, Shs0),
        {Positions,Vpos} = vpos(Face, We),
        Lvs = lookup_locked_vs(Id, Locked),
        Normal0 = e3d_vec:normal(Positions),
        Normal = case wings_io:is_modkey_pressed(?CTRL_BITS) of
          false -> Normal0;
          true -> e3d_vec:neg(Normal0)
        end,
        Vtab = foldl(fun
            ({V,Pos}, Vtab1) ->
                 case ordsets:is_element(V, Lvs) of
                     true -> Vtab1;
                     false ->
                         NewPos0 = e3d_vec:add_prod(Pos, Normal, Str),
                         NewPos1 = constraint_check(Pos, NewPos0),
                         NewPos = handle_mirror(Id, V, NewPos1, Mir),
                         case wings_io:is_modkey_pressed(?SHIFT_BITS) of
                           false ->
                             array:set(V, NewPos, Vtab1);
                           true ->
                             smooth(V, NewPos, 1, Str*10, Mir, We, Vtab1)
                         end
                 end
            end, Vtab0, Vpos),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,locked=Locked,mir=Mir,str=Str,rad=Rad,
  mag_type=MagType,st=#st{shapes=Shs1}=St}) ->
%% Push and Pull mode with magnet
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        Smooth=wings_io:is_modkey_pressed(?SHIFT_BITS),
        Shs0=check_is_same_id(Id,Shs1),
        #we{vp=Vtab0}=We = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We),
        Cnt = {Cx,Cy,Cz} = e3d_vec:average(Positions),
        Lvs = lookup_locked_vs(Id, Locked),
        {_,VsDyn,Vtab} = case Smooth of
          true ->
            {VsDyn0,Vtab1}=smooth_magnetic(Locked,Str,Rad,Cnt,MagType,Mir,We),
            {none,VsDyn0,Vtab1};
          _ ->
            array:sparse_foldl(fun
            (V, Pos={Px,Py,Pz}, {FNs0,VsDyn0,Vtab1}) ->
                 case ordsets:is_element(V, Lvs) of
                     true -> {FNs0,VsDyn0,Vtab1};
                     false ->
                         case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
                             true ->
                                 Dist = e3d_vec:dist(Pos, Cnt),
                                 case Dist =< Rad of
                                     true ->
                                       Inf = magnetic_influence(MagType, Dist, Rad),
                                       VsDyn1=VsDyn0++[{V,Inf}],
                                       {FNs,Normal0} = vertex_normal(V, We, FNs0),
                                       Normal = case wings_io:is_modkey_pressed(?CTRL_BITS) of
                                         false -> Normal0;
                                         true -> e3d_vec:neg(Normal0)
                                       end,
                                       NewPos0 = e3d_vec:add_prod(Pos, Normal, Str*Inf),
                                       NewPos1 = constraint_check(Pos, NewPos0),
                                       NewPos = handle_mirror(Id, V, NewPos1, Mir),
                                       {FNs,VsDyn1,array:set(V, NewPos, Vtab1)};
                                     false -> {FNs0,VsDyn0,Vtab1}
                                 end;
                             false -> {FNs0,VsDyn0,Vtab1}
                         end
                 end
            end, {gb_trees:empty(),[],Vtab0}, Vtab0)
        end,
        NewPst = set_edge_influence(VsDyn,We#we{vp=Vtab}),
        Shs = gb_trees:update(Id, We#we{vp=Vtab,pst=NewPst}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end.

smooth_magnetic(Locked,Str0,Rad,{Cx,Cy,Cz}=Cnt,MagType,Mir,We) ->
    Str = Str0*10,
    #we{id=Id,vp=Vtab0}=We,
    Lvs = lookup_locked_vs(Id, Locked),
    array:sparse_foldl(fun
        (V, Pos={Px,Py,Pz}, {VsDyn0,Vtab1}) ->
            case ordsets:is_element(V, Lvs) of
                true ->  {VsDyn0,Vtab1};
                false ->
                    case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
                        true ->
                            Dist = e3d_vec:dist(Pos, Cnt),
                            case Dist =< Rad of
                                true ->
                                    Inf = magnetic_influence(MagType, Dist, Rad),
                                    VsDyn1=VsDyn0++[{V,Inf}],
                                    {VsDyn1,smooth(V, Pos, Inf, Str, Mir, We, Vtab1)};
                                false -> {VsDyn0,Vtab1}
                            end;
                        false ->  {VsDyn0,Vtab1}
                    end
                end
           end, {[],Vtab0}, Vtab0).

in_dist_boundaries([], [], _) -> true;
in_dist_boundaries([H|T], [P|R], Rad) ->
    Min = H-Rad,
    case P < Min of
      true -> false;
      false ->
        Max = H+Rad,
        case P > Max of
          true -> false;
          false -> in_dist_boundaries(T, R, Rad)
        end
    end.

vertex_normal(V, We, FNormals0) ->
    {Fn,Ns} = wings_vertex:fold(fun(_, Face, _, {FNormals,A}) ->
        case gb_trees:lookup(Face, FNormals) of
            {_,Normal} ->
                {FNormals,[Normal|A]};
            _ ->
                Normal = wings_face:normal(Face, We),
                {gb_trees:insert(Face,Normal,FNormals),[Normal|A]}
        end
    end, {FNormals0,[]}, V, We),
    {Fn,e3d_vec:norm(e3d_vec:add(Ns))}.

vpos(Face, #we{fs=Ftab}=We) ->
    Edge = gb_trees:get(Face, Ftab),
    vpos(Face, Edge, We).

vpos(Face, Edge, #we{es=Etab,vp=Vtab}) ->
    vpos_1(Edge, Etab, Vtab, Face, Edge, [], []).

vpos_1(LastEdge, _, _, _, LastEdge, Acc, Vpos) when Acc =/= [] ->
    {Acc,Vpos};
vpos_1(Edge, Etab, Vtab, Face, LastEdge, Acc, Vpos) ->
    case array:get(Edge, Etab) of
    #edge{vs=V,lf=Face,ltsu=NextEdge} ->
        Pos = array:get(V, Vtab),
        vpos_1(NextEdge, Etab, Vtab, Face, LastEdge, [Pos|Acc], [{V,Pos}|Vpos]);
    #edge{ve=V,rf=Face,rtsu=NextEdge} ->
        Pos = array:get(V, Vtab),
        vpos_1(NextEdge, Etab, Vtab, Face, LastEdge, [Pos|Acc], [{V,Pos}|Vpos])
    end.

lookup_locked_vs(_, []) -> [];
lookup_locked_vs(Id, Locked) ->
    case orddict:find(Id, Locked) of
        {_,Lvs0} -> Lvs0;
        error -> []
    end.

magnetic_influence(bell, Dist, Rad) -> math:sin((Rad-Dist)/Rad*math:pi());
magnetic_influence(dome, Dist, Rad) -> math:sin((Rad-Dist)/Rad*math:pi()/2);
magnetic_influence(straight, Dist, Rad) -> (Rad-Dist)/Rad;
magnetic_influence(spike, Dist, Rad) ->
    D = (Rad-Dist)/Rad,
    D*D;
magnetic_influence(absolute, _, _) -> 1.0.

smooth(V, Pos, Inf, Str, Mir, #we{id=Id}=We, Vtab) ->
    Positions = wings_vertex:fold(fun(_, _, E, Acc) ->
                 OtherPos = wings_vertex:other_pos(V, E, We),
                 [OtherPos|Acc]
        end, [], V, We),
    Avg = e3d_vec:average(Positions),
    Vec = e3d_vec:sub(Avg, Pos),
    NewPos0 = e3d_vec:add_prod(Pos, Vec, Str*Inf),
    NewPos1 = constraint_check(Pos, NewPos0),
    NewPos = handle_mirror(Id, V, NewPos1, Mir),
    array:set(V, NewPos, Vtab).

pinch(V, Pos, Cnt, Inf, Str, Mir, #we{id=Id}, Vtab) ->
    Vec = case wings_io:is_modkey_pressed(?CTRL_BITS) of
          false -> e3d_vec:sub(Cnt, Pos);
          true -> e3d_vec:sub(Pos, Cnt)
    end,
    NewPos0 = e3d_vec:add_prod(Pos, Vec, Str*Inf),
    NewPos1 = constraint_check(Pos, NewPos0),
    NewPos = handle_mirror(Id, V, NewPos1, Mir),
    array:set(V, NewPos, Vtab).

constraint_check({X1,Y1,Z1}=OrigPos, {X2,Y2,Z2}=NewPos) ->
    Const = wings_pref:get_value(sculpt_constraint_axis),
    case Const of
        none -> NewPos;
        x -> {X2,Y1,Z1};
        y -> {X1,Y2,Z1};
        z -> {X1,Y1,Z2};
        default_axis ->
            {_,DefaultAxis} = wings_pref:get_value(default_axis),
            intersect_vec_plane(OrigPos, NewPos, DefaultAxis, DefaultAxis);
        x_radial -> {X1,Y2,Z2};
        y_radial -> {X2,Y1,Z2};
        z_radial -> {X2,Y2,Z1};
        default_axis_radial ->
            {_,DefaultAxis} = wings_pref:get_value(default_axis),
            intersect_vec_plane(NewPos, OrigPos, DefaultAxis, DefaultAxis)
    end.

intersect_vec_plane(PosA, PosB, Plane, Vec) ->
%% Return point where Vec through PosA intersects with Plane at PosB
    case e3d_vec:dot(Vec,Plane) of
      0.0 ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane),
        e3d_vec:add(PosB, e3d_vec:mul(Plane, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(PosB, PosA), Plane) / Dot,
        e3d_vec:add(PosA, e3d_vec:mul(Vec, Intersection))
    end.

%%%
%%% Mirror Handling
%%%

mirror_info(Shs0, Acc0) ->
    case gb_trees:is_empty(Shs0) of
      true -> sort(Acc0);
      false ->
        {Id,#we{mirror=M}=We,Shs} = gb_trees:take_smallest(Shs0),
        Acc = case M of
          none -> Acc0;
          _ ->
            FaceVs = wings_face:vertices_ccw(M, We),
            Flatten = wings_we:mirror_projection(We),
            [{Id,{FaceVs,Flatten}}|Acc0]
        end,
        mirror_info(Shs, Acc)
    end.

handle_mirror(Id, V, Pos, Mir) ->
    case orddict:find(Id, Mir) of
        error -> Pos;
        {ok,{MirVs,Matrix}} ->
            case member(V, MirVs) of
                true -> e3d_mat:mul_point(Matrix, Pos);
                false -> Pos
            end
    end.

%%%
%%% Handle Key Hits
%%%

handle_key(Sym, Ev, Sc) ->
    wings_wm:dirty(),
    case Sym of
      ?SDLK_ESCAPE -> exit_sculpt(Sc);
      _ ->
        case wings_hotkey:event(Ev, sculpt) of
          next -> keep;
          Action -> command_handling(Action, Sc)
        end
    end.

is_altkey_magnet_event(Sym,Mod) ->
    case Sym of
      ?SDLK_LALT -> Mod band (?SHIFT_BITS bor ?CTRL_BITS) =:= 0;
      ?SDLK_RALT -> Mod band (?SHIFT_BITS bor ?CTRL_BITS) =:= 0;
      _ -> false
    end.

command_handling(Action, #sculpt{st=St0,mag=Mag}=Sc) ->
    case Action of
      {view,highlight_aim} ->
          {{_,Cmd},St1} = wings:highlight_aim_setup(St0),
          St = wings_view:command(Cmd, St1),
          update_sculpt_handler(Sc#sculpt{st=St});
      {view,auto_rotate} -> keep;
      {view,Cmd} ->
          case wings_view:command(Cmd, St0) of
              keep ->
		  keep;
              #st{}=St ->
                  wings_draw:refresh_dlists(St),
		  wings_wm:dirty(),
                  update_sculpt_handler(Sc#sculpt{st=St})
          end;
      {edit,undo_toggle} ->
          St = wings_u:caption(wings_undo:undo_toggle(St0)),
          wings_wm:current_state(St),
          wings_draw:refresh_dlists(St),
          update_sculpt_handler(Sc#sculpt{st=St});
      {edit,undo} ->
          St = wings_u:caption(wings_undo:undo(St0)),
          wings_wm:current_state(St),
          wings_draw:refresh_dlists(St),
          update_sculpt_handler(Sc#sculpt{st=St});
      {edit,redo} ->
          St = wings_u:caption(wings_undo:redo(St0)),
          wings_wm:current_state(St),
          wings_draw:refresh_dlists(St),
          update_sculpt_handler(Sc#sculpt{st=St});
      {sculpt,Mode} when Mode =:= pull; Mode =:= pinch; Mode =:= smooth ->
          wings_wm:dirty(),
          update_sculpt_handler(Sc#sculpt{mode=Mode});
      {sculpt,MagT} when MagT =:= dome; MagT =:= absolute; MagT =:= straight;
        MagT =:= spike; MagT =:= bell ->
          wings_wm:dirty(),
          update_sculpt_handler(Sc#sculpt{mag=true,mag_type=MagT});
      {sculpt,magnet} ->
          wings_wm:dirty(),
          update_sculpt_handler(Sc#sculpt{mag = not Mag});
      {sculpt,mask_toggle} ->
          MM = wings_pref:get_value(magnet_mask_on),
          wings_pref:set_value(magnet_mask_on, not MM),
          wings_wm:dirty(),
          handle_sculpt_event_0({new_state,St0}, Sc);
      {sculpt,prefs} ->
          prefs(Sc);
      {edit, {preferences, prefs}} ->
	  prefs(Sc);
      {sculpt,exit_sculpt} ->
          exit_sculpt(Sc);
      {sculpt,{axis_constraint,Axis}} ->
          Const0 = wings_pref:get_value(sculpt_constraint_axis),
          case Axis of
              Const0 ->
                  wings_pref:set_value(sculpt_constraint_axis, none),
                  wings_wm:dirty(),
                  update_sculpt_handler(Sc);
              clear_constraints ->
                  wings_pref:set_value(sculpt_constraint_axis, none),
                  wings_wm:dirty(),
                  update_sculpt_handler(Sc);
              Axis ->
                  wings_pref:set_value(sculpt_constraint_axis, Axis),
                  wings_wm:dirty(),
                  update_sculpt_handler(Sc)
          end;
	{hotkey, Cmd} ->
	    wings_hotkey:command({Cmd,Sc}, St0);
	{window, _} ->
	    defer;
	{file, _} ->
	    defer;
      _ -> keep
    end.

%%%
%%% Exit Sculpt
%%%

exit_sculpt(#sculpt{mag=Mag,mag_type=MagType,str=Str,rad=Rad,mode=Mode,ost=St0}=Sc) ->
    wings_pref:set_value(sculpt_mode, Mode),
    wings_pref:set_value(sculpt_magnet, {Mag,Rad}),
    wings_pref:set_value(sculpt_strength, Str),
    wings_pref:set_value(sculpt_magnet_type, MagType),
    wings_tweak:toggle_draw(true),
    #sculpt{st=#st{shapes=Shs}}=remove_influence(Sc),
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings:clear_mode_restriction(),
    wings_wm:later({new_state,St}),
    pop.

%%%
%%% Info Line
%%%

help(#sculpt{mag=Mag,rad=Rad,mag_type=MagType,str=Str,mode=Mode}) ->
    AddSmooth = "  "++ ?__(13,"Hold [Shift]: Smooth")++") ",
    ModeMsg =
        case Mode of
            pinch ->
              Pinch = mode(pinch),
              Pinch++" ("++?__(12,"Hold [Ctrl]: Inflate")++AddSmooth;
            smooth -> mode(smooth);
            _ ->
              Pull = mode(pull),
              Pull++" ("++?__(11,"Hold [Ctrl]: Push")++AddSmooth
        end,
    Sculpt = ?__(1,"L: Sculpt"),
    Menu = ?__(2,"[Alt]+R: Sculpt Menu"),
    MagnetType = io_lib:format(?__(3,"Magnet: ~s"),[magtype(MagType)]),
    Radius = ?__(4,"Alt+Drag: Adjust Radius"),
    Strength = ?__(5,"Alt+Scroll(+[Shift]): ")  ++ ?__(6,"Adjust Strength"),
    Exit = "[Esc]: " ++ exit_string(),
    StatusBar = io_lib:format(?__(7,"Strength: ~p")++"  "++
                              ?__(8,"Radius: ~s"),
                              [trunc(Str/0.001),
                               if Mag ->
                                   wings_util:nice_float(Rad) ++ "  " ++
                                   MagnetType;
                                  true -> ?__(9,"None (Magnet is off)")
                               end]),
    {_,H} = wings_wm:win_size(),
    Constraint = constraint_info(),
    LLine = wings_msg:join([?__(10,"Sculpt Mode")++": "++ModeMsg,StatusBar,Constraint]),
    wings_io:info(0, H-?LINE_HEIGHT-3, LLine),
    wings_wm:message(wings_msg:join([Sculpt,Menu,Exit]),
                     wings_msg:join([Radius,Strength])).

magtype(bell) -> ?__(1,"Bell");
magtype(dome) -> ?__(2,"Dome");
magtype(straight) -> ?__(3,"Straight");
magtype(spike) -> ?__(4,"Spike");
magtype(absolute) -> ?__(5,"Absolute").

mode(push) -> ?__(1,"Push");
mode(pull) -> ?__(2,"Pull");
mode(pinch) -> ?__(3,"Pinch");
mode(smooth) -> ?__(4,"Smooth");
mode(inflate) -> ?__(5,"Inflate").

exit_string() -> ?__(1,"Exit").

%%%
%%% Sculpt Dialog
%%%

prefs(#sculpt{str=Str}) ->
    Strength = trunc(Str/0.001),
    Confine = wings_pref:get_value(sculpt_initial),
    ShowInfluence = wings_pref:get_value(sculpt_magnet_influence),
    Menu = [{vframe,
	     [{hframe,[{slider,{text,Strength,[{key,sculpt_strength},{range,{1,100}}]}}],
	       [{title,?__(1,"Strength")}]}]}],
    C = [separator,{?__(3,"Confine sculpt to initial object"),
		    Confine,[{key,sculpt_initial}]},
         {?__(4,"Show Magnet influence"),
          ShowInfluence,[{key,sculpt_magnet_influence}]}],
    PrefQs = [{Lbl, make_query(Ps)} || {Lbl, Ps} <- Menu] ++ C,
    wings_dialog:dialog(?__(10,"Sculpt Preferences"), PrefQs,
			fun(Result) -> set_values(Result) end).

make_query([_|_]=List) ->
    [make_query(El) || El <- List];
make_query({color,[_|_]=Str,Key}) ->
    Def = wings_pref:get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query(Other) -> Other.

set_values([{sculpt_strength=Key, Value}|Result]) ->
    wings_pref:set_value(Key, Value*0.001),
    set_values(Result);
set_values([{Key, Value}|Result]) ->
    wings_pref:set_value(Key, Value),
    set_values(Result);
set_values([]) -> ok.

%%%
%%% Sculpt Menu
%%%

sculpt_menu(X0, Y0, Sc) ->
    Pos = wings_wm:local2screen({X0, Y0}),
    Menu = sculpt_menu(Sc),
    wings_menu:popup_menu(wings_wm:this_win(), Pos, sculpt, Menu).

sculpt_menu(#sculpt{mag=Mag,mag_type=MagType,mode=Mode}) ->
    [{mode(pull)++"/"++mode(push),pull,?__(4,"Activate the Pull/Push sculpt tool"),crossmark(Mode, pull)},
     {mode(pinch)++"/"++mode(inflate),pinch,?__(5,"Activate the Pinch/Inflate sculpt tool"),crossmark(Mode, pinch)},
     {mode(smooth),smooth,?__(6,"Activate the smoothing brush"),crossmark(Mode, smooth)},
     separator,
     {magtype(dome),dome,?__(7,"Activate the dome magnet"),crossmark(MagType, dome)},
     {magtype(absolute),absolute,?__(8,"Activate the absolute magnet"),crossmark(MagType, absolute)},
     {magtype(straight),straight,?__(9,"Activate the straight magnet"),crossmark(MagType, straight)},
     {magtype(spike),spike,?__(10,"Activate the spike magnet"),crossmark(MagType, spike)},
     {magtype(bell),bell,?__(11,"Activate the bell magnet"),crossmark(MagType, bell)},
     separator,
     {?__(1,"Magnet On/Off"),magnet,?__(12,"Toggle the magnet on or off"),crossmark(Mag, true)},
     {?__(2,"Magnet Mask On/Off"),mask_toggle,
      ?__(13,"Toggle the Magnet Mask. Masking options are found in the Tools menu."),
         wings_menu_util:crossmark(magnet_mask_on)},
     separator,
     {?__(16,"Axis Constraints"),{axis_constraint, constraints_menu()}},
     separator,
     {?__(3,"Preferences"),prefs,?__(14,"Sculpt mode preferences")},
     {exit_string(),exit_sculpt,?__(15,"Exit sculpt mode")}].

constraints_menu() ->
    Constraint = wings_pref:get_value(sculpt_constraint_axis),
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Rad = ?__(5,"Radial of ~s"),
    Def = ?__(4,"Default"),
    Help1 = ?__(1,"Constrain movement to the ~s axis."),
    Help2 = ?__(7,"Constrain movement to the radial of the ~s axis."),
    [{X,x,wings_util:format(Help1, [X]),crossmark(x, Constraint)},
     {Y,y,wings_util:format(Help1, [Y]),crossmark(y, Constraint)},
     {Z,z,wings_util:format(Help1, [Z]),crossmark(z, Constraint)},
     {?__(3,"Default Axis"),default_axis,wings_util:format(Help1, [Def]),
       crossmark(default_axis, Constraint)},
     separator,
     {wings_util:format(Rad, [X]),x_radial,wings_util:format(Help2, [X]),crossmark(x_radial, Constraint)},
     {wings_util:format(Rad, [Y]),y_radial,wings_util:format(Help2, [Y]),crossmark(y_radial, Constraint)},
     {wings_util:format(Rad, [Z]),z_radial,wings_util:format(Help2, [Z]),crossmark(z_radial, Constraint)},
     {?__(6,"Radial of Default Axis"),default_axis_radial,wings_util:format(Help2, [Def]),
       crossmark(default_axis_radial, Constraint)},
     separator,
     {?__(8,"Clear Constraints"),clear_constraints}].

constraint_info() ->
    Constraint = wings_pref:get_value(sculpt_constraint_axis),
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Def = ?__(1,"Default"),
    Rad = ?__(2,"Radial of ~s"),
    Str = case Constraint of
        none -> ?__(3,"None");
        x -> X;
        y -> Y;
        z -> Z;
        default_axis -> Def;
        x_radial -> wings_util:format(Rad, [X]);
        y_radial -> wings_util:format(Rad, [Y]);
        z_radial -> wings_util:format(Rad, [Z]);
        default_axis_radial -> wings_util:format(Rad, [Def])
    end,
    wings_util:format(?__(4,"Constraint: ~s"), [Str]).

crossmark(Axis, Axis) -> [crossmark];
crossmark(_, _) -> [].

%%%
%%% Support for highlighting the magnet influence
%%%

%% This function will clean the vertices influence information when the list is empty or
%% it will add the vertices influence information to Pst field of the we#
set_edge_influence([],#we{pst=Pst}) ->
    remove_pst(Pst);
set_edge_influence(VsDyn,#we{pst=Pst,es=Etab,vp=Vtab}=We) ->
    case wings_pref:get_value(sculpt_magnet_influence) of
    true ->
        Vs = [V || {V,_} <- VsDyn],
        Edges = wings_edge:from_vs(Vs,We),
        EdDyn=to_edges_raw(Edges,VsDyn,Etab,Vtab),
        add_pst(EdDyn,Pst);
    _ ->
        Pst
    end.

%% It adds the plugin functionality
add_pst(InfData,Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
    none ->
        Data = gb_trees:empty(),
        NewData = gb_trees:insert(edge_info,InfData,Data),
        gb_trees:insert(?MODULE,NewData,Pst);
    {_,Data} ->
        NewData = gb_trees:enter(edge_info,InfData,Data),
        gb_trees:update(?MODULE,NewData,Pst)
    end.

%% It removes the plugin functionality
remove_pst(Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
    none -> Pst;
    {_,Data} ->
        NewData = gb_trees:delete_any(edge_info,Data),
        gb_trees:update(?MODULE,NewData,Pst)
    end.

%% It removes the plugin functionality
remove_influence(#sculpt{id=none,st=#st{shapes=Shs0}=St}=Sc) ->
    wings_pref:delete_value(sculpt_current_id),
    Shs1 = lists:map(fun
            (#we{id=Id,pst=none}=We) -> {Id,We};
            (#we{id=Id,pst=Pst}=We) ->
                NewPst=gb_trees:delete_any(?MODULE, Pst),
              {Id,We#we{pst=NewPst}}
          end,gb_trees:values(Shs0)),
    Sc#sculpt{st=St#st{shapes=gb_trees:from_orddict(Shs1)}}.

%% It clean any information about the vertices influence from the shapes
clear_influence(#sculpt{id=none,st=#st{shapes=Shs0}=St}=Sc) ->
    Sc#sculpt{st=St#st{shapes=clear_influence_vs(Shs0)}};
clear_influence(#sculpt{id=Id,st=#st{shapes=Shs0}=St}=Sc) ->
    #we{pst=Pst}=We= gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{pst=remove_pst(Pst)}, Shs0),
    Sc#sculpt{st=St#st{shapes=Shs}}.

clear_influence_vs(Shs) ->
    Shs1 = lists:map(fun
            (#we{id=Id,pst=none}=We) -> {Id,We};
            (#we{id=Id,pst=Pst}=We) ->
                NewPst=remove_pst(Pst),
              {Id,We#we{pst=NewPst}}
          end,gb_trees:values(Shs)),
    gb_trees:from_orddict(Shs1).

%% It's used to reset the highlighting when a new we# got the focus
check_is_same_id(IdNew,Shs0) ->
    OldId=wings_pref:get_value(sculpt_current_id),
    wings_pref:set_value(sculpt_current_id,IdNew),
    case OldId of
    none -> Shs0;
    IdNew -> Shs0;
    ID ->
        #we{pst=Pst0}=We0 = gb_trees:get(ID, Shs0),
        gb_trees:update(ID,We0#we{pst=remove_pst(Pst0)},Shs0)
    end.

%%%
%%% Functions of general purpose
%%%
to_edges_raw([],_ , _, _) -> [];
to_edges_raw(_, [] , _, _) -> [];
to_edges_raw(Edges, VsDyn, Etab, Vtab) ->
    to_edges_raw_1(Edges, VsDyn, Etab, Vtab, []).

to_edges_raw_1([], _, _, _, Acc) -> Acc;
to_edges_raw_1([Edge|Edges], VsDyn, Etab, Vtab, Acc) ->
    #edge{vs=Va0,ve=Vb0} = array:get(Edge, Etab),
    Cola=get_vs_influence(Va0, VsDyn),
    Colb=get_vs_influence(Vb0, VsDyn),
    VsPair=[{Va0,Cola, Vb0,Colb}],
    to_edges_raw_1(Edges, VsDyn, Etab, Vtab, VsPair++Acc).

get_vs_influence(V, VsDyn) ->
    case lists:keysearch(V, 1, VsDyn) of
    false -> 0.0;
    {_, {_,Value}} -> Value
    end.

%%%
%%% Functions to produce the visual effect (inspired on wpc_magnet_mask.erl file)
%%%

%% It generate the OpenGl list of colored vertices
update_dlist({edge_info,EdgeInfo},#dlo{plugins=Pdl,src_we=#we{vp=Vtab}}=D, _) ->
    Key = ?MODULE,
    case EdgeInfo of
	[] ->
	    D#dlo{plugins=[{Key,none}|Pdl]};
	_ ->
	    Str = wings_pref:get_value(sculpt_strength),
	    ColFac = Str*10,		     % the range of 0..1
	    ColFrom = col_to_vec(wings_pref:get_value(edge_color)),
	    ColTo = {1.0*ColFac,0.0,1.0}, % from blue to magenta
	    ColRange = e3d_vec:sub(ColTo, ColFrom),
	    Lines = prepare_edge_pump(EdgeInfo, Vtab, ColFrom, ColRange, <<>>),
	    Draw = draw_fun(Lines),
	    D#dlo{plugins=[{Key,Draw}|Pdl]}
    end.

draw_fun(Data) ->
    N = byte_size(Data) div (4*3*4),
    F = fun(RS) ->
		gl:depthFunc(?GL_LEQUAL),
		gl:drawArrays(?GL_LINES, 0, N),
		gl:depthFunc(?GL_LESS),
                RS
	end,
    wings_vbo:new(F, Data, [vertex,color]).

prepare_edge_pump([{Id1,Inf1,Id2,Inf2}|SegInf], Vtab, Col, Range, Acc0) ->
    case {array:get(Id1, Vtab),array:get(Id2, Vtab)} of
        {undefined,_} ->
	    prepare_edge_pump(SegInf, Vtab, Col, Range, Acc0);
        {_,undefined} ->
	    prepare_edge_pump(SegInf, Vtab, Col, Range, Acc0);
        {V1,V2} ->
	    Col1 = color_gradient(Col, Range, Inf1),
	    Col2 = color_gradient(Col, Range, Inf2),
	    {R1,G1,B1} = Col1,
	    {R2,G2,B2} = Col2,
	    {X1,Y1,Z1} = V1,
	    {X2,Y2,Z2} = V2,
	    Acc = <<Acc0/binary,
		    X1:?F32,Y1:?F32,Z1:?F32,
		    R1:?F32,G1:?F32,B1:?F32,
		    X2:?F32,Y2:?F32,Z2:?F32,
		    R2:?F32,G2:?F32,B2:?F32>>,
	    prepare_edge_pump(SegInf, Vtab, Col, Range, Acc)
    end;
prepare_edge_pump([], _, _, _, Acc) -> Acc.

%% It'll provide the vertice data for 'update_dlist' function
get_data(update_dlist, Data, Acc) ->  % for draw lists
    case gb_trees:lookup(edge_info, Data) of
    none ->
        {ok, Acc};
    {_,EdgeInfo} ->
        {ok, [{plugin, {?MODULE, {edge_info, EdgeInfo}}}|Acc]}
    end.

%% It'll use the list prepared by 'update_dlist' function and then
%% draw it (only for plain draw).
draw(plain, DrawEdges, _D, SelMode, RS) ->
    gl:lineWidth(edge_width(SelMode)),
    wings_dl:call(DrawEdges, RS);
draw(_,_,_,_, RS) -> RS.

edge_width(edge) -> wings_pref:get_value(edge_width);
edge_width(_) -> 1.

col_to_vec({R,G,B}) when is_integer(R) -> {R/255.0,G/255.0,B/255.0};
col_to_vec({_,_,_}=Col) -> Col;
col_to_vec({R,G,B,_}) when is_integer(R) -> col_to_vec({R,G,B});
col_to_vec({R,G,B,_}) -> col_to_vec({R,G,B}).

color_gradient(Cb, Cr, Perc) ->
    e3d_vec:add_prod(Cb, Cr, Perc).
