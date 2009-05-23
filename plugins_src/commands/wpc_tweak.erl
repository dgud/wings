%%
%%  wpc_tweak.erl --
%%
%%     Tweak mode plugin.
%%
%%  Copyright (c) 2001-2002 Howard Trickey,
%%                2002-2009 Bjorn Gustavsson.
%%
%%  Various changes and improvements by Andrew Shpagin.
%%  Multiple selections and access to regular Wings commands by Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_tweak.erl 590 2009-04-22 03:07:31Z optigon $
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
     dc={0,0},              % double click selection
     st}).					% wings st record (working)

-record(drag,
    {vs,
     pos0,				%Original position.
     pos,				%Current position.
     mag,
     mm					%original|mirror
    }).

-record(mag,
    {orig,				%Orig pos of vertex being moved.
     vs,				%[{V,Pos,Distance,Influence}]
                        %(not changed while dragging)
     vtab=[]		    %[{V,Pos}] (latest)
    }).

init() ->
    wings_pref:set_default(tweak_xyz,[false,false,false]),
    wings_pref:set_default(tweak_single_click,true),
    wings_pref:set_default(tweak_double_click,true),
    case catch wings_pref:get_value(tweak_double_click_speed) of
       undefined ->
           Val = 200000;
       Value ->
           wings_pref:delete_value(tweak_double_click_speed),
           Val = Value*100000
    end,
    wings_pref:set_default(tweak_double_click_speed54,Val),
    wings_pref:set_default(tweak_ctrl,slide),
    wings_pref:set_default(tweak_mmb_select,false),
    case wings_pref:get_value(start_in_tweak) of
      true ->
        self() ! {external, launch_tweak},
        ok;
      false ->
        ok
    end,
    true.

menu({tools}, Menu0) ->
    Menu0 ++ [separator,
          {?__(1,"Tweak"), tweak,
           ?__(4,"Mode for adjusting geometry quickly.") ++" "++
           [{bold,?__(3,"Evoking command toggles Tweak mode On/Off.")}],
           [option]}];
menu(_, Menu) -> Menu.

command({tools,{tweak,Ask}}, St) ->
    Cam = wings_pref:get_value(camera_mode),
    tweak(Ask,Cam,St);
command(_, _) -> next.

tweak(Ask,maya,_St) when is_atom(Ask) ->
    {Radio1,Radio2} = tweak_pref(),
    MmbHook = fun (is_disabled, {_Var,_I,Store}) ->
              gb_trees:get(tweak_mmb_select, Store);
              (_,_) -> void
            end,
    ClickHook = fun (is_disabled, {_Var,_I,Store}) ->
              not ((gb_trees:get(tweak_double_click, Store)) or
                  (gb_trees:get(tweak_single_click, Store)));
              (_, _) -> void
          end,
    DblClkSpd = wings_pref:get_value(tweak_double_click_speed54)/100000,
    TweakPrefs = [{vframe,
        [{?__(8,"Mmb Selects/Deselects (Maya camera mode only)"),tweak_mmb_select},
         {vframe,[{?__(1,"Lmb single click Selects/Deselects"),tweak_single_click},
         {?__(2,"Lmb double click initiates Paint Select/Deselect"),tweak_double_click},
       {hframe,[{slider,{text,DblClkSpd,[{key,tweak_double_click_speed54},{range,{1.0,3.0}},
        {hook,ClickHook}]}}],
       [{title,?__(3,"Click Speed")}]},
       {vframe,[{vradio,[{Radio1,select},
                         {Radio2,slide}],tweak_ctrl}],
       [{title,?__(7,"Button Options")},{hook,ClickHook}]}],
       [{title,?__(9,"Options Panel")},{hook,MmbHook}]}]}],

    PrefQs = [{Lbl,make_query(Ps)} || {Lbl,Ps} <- TweakPrefs],
    wings_ask:dialog(Ask, ?__(4,"Tweak Mode Preferences"),PrefQs,
    fun(Result) -> set_values(Result), {tools,{tweak,Result}} end);

tweak(Ask,_Cam,_St) when is_atom(Ask) ->
    {Radio1,Radio2} = tweak_pref(),
    ClickHook = fun (is_disabled, {_Var,_I,Store}) ->
              not ((gb_trees:get(tweak_double_click, Store)) or
                  (gb_trees:get(tweak_single_click, Store)));
              (_, _) -> void
          end,
    DblClkSpd = wings_pref:get_value(tweak_double_click_speed54)/100000,
    TweakPrefs = [{vframe,
        [{?__(1,"Lmb single click Selects/Deselects"),tweak_single_click},
         {?__(2,"Lmb double click initiates Paint Select/Deselect"),tweak_double_click},
       {hframe,[{slider,{text,DblClkSpd,[{key,tweak_double_click_speed54},{range,{1.0,3.0}},
        {hook,ClickHook}]}}],
       [{title,?__(3,"Click Speed")}]},
       {vframe,[{vradio,[{Radio1,select},
                         {Radio2,slide}],tweak_ctrl}],
       [{title,?__(7,"Button Options")},{hook,ClickHook}]} ]}],

    PrefQs = [{Lbl,make_query(Ps)} || {Lbl,Ps} <- TweakPrefs],
    wings_ask:dialog(Ask, ?__(4,"Tweak Mode Preferences"),PrefQs,
    fun(Result) -> set_values(Result), {tools,{tweak,Result}} end);

tweak(_,_,St) ->
    case wpa:pref_get(?MODULE, sel_mode) of
    {_Mode,_Sh0,Mag,MagType} ->
        MagR = 1.0;
    {_Mode,_Sh0,Mag,MagType,MagR} ->
        ok;
    _ ->
        Mag = false,
        MagType = dome,
        MagR = 1.0
    end,
    T = #tweak{magnet=Mag,mag_type=MagType,mag_r=MagR,
           tmode=wait,st=St},
    help(T),
    {seq,push,update_tweak_handler(T)}.

make_query([_|_]=List)	->
    [make_query(El) || El <- List];
make_query({[_|_]=Str,Key}) ->
    case wings_pref:get_value(Key) of
    Def when Def == true; Def == false ->
        {Str,Def,[{key,Key}]};
    Def ->
        {Str,{text,Def,[{key,Key}]}}
    end;
make_query({menu,List,Key}) ->
    Def = wings_pref:get_value(Key),
    {menu,List,Def,[{key,Key}]};
make_query({vradio,List,Key}) ->
    Def = wings_pref:get_value(Key),
    {vradio,List,Def,[{key,Key}]};
make_query(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([make_query(El) || El <- tuple_to_list(Tuple)]);
make_query(Other) -> Other.

set_values([{tweak_double_click_speed54=Key,Value}|Result]) ->
    wings_pref:set_value(Key, Value*100000),
    set_values(Result);
set_values([{Key,Value}|Result]) ->
    wings_pref:set_value(Key, Value),
    set_values(Result);
set_values([]) -> ok.

tweak_pref() ->
    case wings_pref:get_value(camera_mode) of
      mb -> {?__(1,"[Alt]+L Selects"),?__(2,"[Alt]+L Slides(+[Ctrl] to Clean)")};
      _  -> {?__(3,"[Ctrl]+L Selects"), ?__(4,"[Ctrl]+L Slides(+[Alt] to Clean)")}
    end.

shift() -> ?KMOD_SHIFT.
ctrl() -> ?KMOD_CTRL.
alt() -> ?KMOD_ALT.
f1() -> ?SDLK_F1.
f2() -> ?SDLK_F2.
f3() -> ?SDLK_F3.

mod_key_combo() ->
    Mod = sdl_keyboard:getModState(),
    Shift = (Mod band shift()) =/= 0,
    Ctrl = (Mod band ctrl()) =/= 0,
    Alt = (Mod band alt()) =/= 0,
    {Shift,Ctrl,Alt}.
fkey_combo() ->
    Keys = sdl_keyboard:getKeyState(),
    F1 = element(f1()+1,Keys) =/= 0,
    F2 = element(f2()+1,Keys) =/= 0,
    F3 = element(f3()+1,Keys) =/= 0,
    [F1,F2,F3].

%% Event handler for tweak mode
update_tweak_handler(#tweak{tmode=drag,st=#st{}=St}=T) ->
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    {replace,fun(Ev) ->
        handle_tweak_event(Ev, T#tweak{st=St}) end};

update_tweak_handler(#tweak{st=#st{}=St}=T) ->
    wings:mode_restriction(none),
    wings_wm:current_state(St),
    wings_draw:refresh_dlists(St),
    wings_wm:dirty(),
    {replace,fun(Ev) ->
        handle_tweak_event(Ev, T#tweak{st=St}) end}.

handle_tweak_event(redraw, #tweak{st=St}=T) ->
    help(T),
    redraw(St),
    draw_magnet(T),
    keep;

handle_tweak_event({vec_command,Command,_}, T) when is_function(Command) ->
    %% Use to execute command with vector arguments (see wings_vec.erl).
    process_cmd_response(Command(),T);

handle_tweak_event(revert_state, #tweak{st=St0}=T) ->
    St = clear_temp_sel(St0),
    update_tweak_handler(T#tweak{st=St});

handle_tweak_event({note,menu_aborted}, #tweak{st=St0}=T) ->
    St = clear_temp_sel(St0),
    update_tweak_handler(T#tweak{st=St});
handle_tweak_event({drop,Pos,DropData}, #tweak{st=St}) ->
    wings:handle_drop(DropData, Pos, St);
handle_tweak_event(language_changed, _) ->
    This = wings_wm:this(),
    wings_wm:toplevel_title(This, geom_title(This)),
    wings_wm:menubar(This, get(wings_menu_template)),
    keep;
handle_tweak_event(Ev, #tweak{st=St}=T) ->
    case wings_camera:event(Ev, St) of
      next -> handle_tweak_event0(Ev, T);
      Other ->
          Other
     end.

handle_tweak_event0(#keyboard{sym=?SDLK_ESCAPE}, T) ->
    exit_tweak(T);

handle_tweak_event0(#keyboard{unicode=C}=Ev, #tweak{st=St0}=T) ->
    case tweak_hotkey(C, T) of
      none ->
            St = fake_sel(St0),
            case wings_hotkey:event(Ev,St) of
              next ->
                update_tweak_handler(T);
              Action ->
                handle_tweak_event2({action,Action},T#tweak{st=St})
            end;
      T1 -> update_tweak_handler(T1)
    end;

handle_tweak_event0(#mousemotion{}=Ev, #tweak{tmode=wait,st=St}=T) ->
    case wings_pick:event(Ev, St) of
      next -> handle_tweak_event1(Ev, T);
      Other -> Other
    end;

handle_tweak_event0(#mousemotion{x=X,y=Y,state=State,mod=Mod},
            #tweak{tmode=drag,cx=CX,cy=CY,ox=OX,oy=OY}=T0)
            when State =/= ?SDL_BUTTON_RMASK ->
    DX = float(X-CX),
    DY = float(Y-CY),
    DxOrg = float(X-OX),
    DyOrg = float(Y-OY),
    FKeys = fkey_combo(),
    TKeys = wings_pref:get_value(tweak_xyz),
    C = tweak_constraints(FKeys,TKeys,[]),

    Mod1 = (Mod band alt()) =/= 0,
    Mod2 = (Mod band shift()) =/= 0,
    Mod3 = (Mod band ctrl()) =/= 0,

    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select)==true,
    TwkCtrl = wings_pref:get_value(tweak_ctrl) == select,
    L = wings_pref:get_value(tweak_single_click),
    LL = wings_pref:get_value(tweak_double_click),
    Lm = L == false andalso LL == false,
    Mode=if
         Mod1, Mod3, Lm -> slide;
         Mod1, Mod3, TwkCtrl -> slide;
         Mod1, Mod3, MayaMod -> slide;
         Mod1, Mod2 -> relax;
         Mod2, Mod3 -> tangent;
         Mod3 andalso TwkCtrl==false andalso Cam =/= mb, not Lm, not MayaMod -> slide;
         Mod1 andalso TwkCtrl==false andalso Cam == mb andalso Lm==false -> slide;
         Mod1 -> normal;
         Mod3, MayaMod -> normal;
         State == ?SDL_BUTTON_MMASK andalso Cam == maya -> normal;
         C == [true,true,false] -> xymove;
         C == [false,true,true] -> yzmove;
         C == [true,false,true] -> zxmove;
         C == [true,false,false]  -> xmove;
         C == [false,true,false]  -> ymove;
         C == [false,false,true]  -> zmove;
         true -> screen
     end,
    do_tweak(DX, DY,DxOrg,DyOrg,Mode),
    T = T0#tweak{cx=X,cy=Y},
    update_tweak_handler(T);

handle_tweak_event0(Ev, T) ->
    handle_tweak_event1(Ev, T).

handle_tweak_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
            #tweak{tmode=wait,dc={0,0},st=St0}=T0) ->
    ModKeys = mod_key_combo(),
    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    TwkCtrl = wings_pref:get_value(tweak_ctrl),
    L = wings_pref:get_value(tweak_single_click),
    LL = wings_pref:get_value(tweak_double_click),
    Lm = L == false andalso LL == false,
    case ModKeys of
      {false,true,false} when Lm andalso not MayaMod->
        wings_pick:paint_pick(X, Y, St0);
      {false,true,false} when TwkCtrl == select andalso not MayaMod ->
        wings_pick:paint_pick(X, Y, St0);
      {false,false,true} when Cam == mb andalso Lm ->
        wings_pick:paint_pick(X, Y, St0);
      {false,false,true} when Cam == mb andalso TwkCtrl == select ->
        wings_pick:paint_pick(X, Y, St0);
      {true,false,false} ->
        wings_pick:marquee_pick(X, Y, St0);
      _Other when
        MayaMod andalso ModKeys == {false,true,false};
        Cam =/= mb;
        Cam == mb andalso ModKeys == {false,false,false};
        Cam == mb andalso ModKeys == {false,false,true} andalso Lm==false andalso TwkCtrl == slide ->
          pick_event(X,Y,Cam,1,T0);
      _Other ->
        update_tweak_handler(T0)
    end;

handle_tweak_event1(#mousebutton{button=2,x=X,y=Y,state=?SDL_PRESSED},
            #tweak{tmode=wait,st=St0}=T0) ->
    ModKeys = mod_key_combo(),
    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    case ModKeys of
      {false,false,false} when MayaMod ->
        wings_pick:paint_pick(X, Y, St0);
      {false,false,false} when Cam==maya ->
        pick_event(X,Y,maya,2,T0);
      {false,false,false} when Cam==mb ->
        update_tweak_handler(T0);
      {false,true,false} when Cam==mb ->
        update_tweak_handler(T0);
      _Other when Cam==mb ->
        pick_event(X,Y,mb,2,T0);
      _Other ->
        update_tweak_handler(T0)
    end;

handle_tweak_event1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
            #tweak{tmode=wait,dc={true,Time1},st=#st{selmode=Selmode}=St0}=T0) ->
    Time2 = now(),
    SecondPress = timer:now_diff(Time2,Time1),
    ModKeys = mod_key_combo(),
    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    ClickSpeed = wings_pref:get_value(tweak_double_click_speed54),
    DC = wings_pref:get_value(tweak_double_click),
    TwkCtrl = wings_pref:get_value(tweak_ctrl),
    L = wings_pref:get_value(tweak_single_click),
    LL = wings_pref:get_value(tweak_double_click),
    Lm = L == false andalso LL == false,
    case ModKeys of
      {false,true,false} when Lm andalso not MayaMod->
        wings_pick:paint_pick(X, Y, St0);
      {false,true,false} when TwkCtrl == select andalso not MayaMod ->
        wings_pick:paint_pick(X, Y, St0);
      {false,false,true} when  Cam == mb andalso Lm ->
        wings_pick:paint_pick(X, Y, St0);
      {false,false,true} when Cam == mb andalso TwkCtrl == select ->
        wings_pick:paint_pick(X, Y, St0);
      {true,false,false} ->
        wings_pick:marquee_pick(X, Y, St0);
      {false,false,false} when SecondPress < ClickSpeed andalso DC == true ->
        St = wings_undo:undo(St0),
        wings_pick:paint_pick(X,Y,St#st{selmode=Selmode});
      _Other when
        MayaMod andalso ModKeys == {false,true,false};
        Cam =/= mb;
        Cam == mb andalso ModKeys == {false,false,false};
        Cam == mb andalso ModKeys == {false,true,false} andalso Lm==false andalso TwkCtrl == slide ->
          pick_event(X,Y,Cam,1,T0);
      _Other ->
        update_tweak_handler(T0)
    end;

handle_tweak_event1(#mousebutton{button=1,state=?SDL_RELEASED},
            #tweak{tmode=drag,dc={Time1,0}}=T) ->
    Time2 = now(),
    Click = timer:now_diff(Time2,Time1),
    ClickSelect = wings_pref:get_value(tweak_single_click),
    ClickSpeed = wings_pref:get_value(tweak_double_click_speed54),
    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    case Cam of
      maya when MayaMod ->
          end_drag(T#tweak{dc={0,0}});
      _Cam when Click < ClickSpeed ->
          end_pick(ClickSelect, T#tweak{dc={true,Time2}});
      _Cam -> end_drag(T#tweak{dc={0,0}})
    end;

handle_tweak_event1(#mousebutton{button=2,state=?SDL_RELEASED},
            #tweak{tmode=drag}=T) ->
    Cam = wings_pref:get_value(camera_mode),
    case Cam of
      maya -> end_drag(T#tweak{dc={0,0}});
      mb -> end_drag(T#tweak{dc={0,0}})
    end;

handle_tweak_event1(#mousemotion{state=?SDL_RELEASED},
            #tweak{tmode=drag}=T) ->
    end_drag(T);

%%%% Right Click Menus
handle_tweak_event1(#mousebutton{button=3,state=?SDL_PRESSED}, #tweak{}) ->
    keep;

handle_tweak_event1(#mousebutton{button=3,state=?SDL_RELEASED,x=X,y=Y},
        #tweak{st=#st{sel=Sel}=St0}) ->
    {GlobalX, GlobalY} = wings_wm:local2global(X,Y),
    case Sel =:= [] andalso wings_pref:get_value(use_temp_sel) of
    false ->
        popup_menu(GlobalX, GlobalY, St0);
    true ->
        case wings_pick:do_pick(X, Y, St0) of
        {add,_,St1} ->
            St = set_temp_sel(St0,St1),
            wings_wm:current_state(St),
            wings_draw:refresh_dlists(St),
            popup_menu(GlobalX, GlobalY, St);
        _ ->
            popup_menu(GlobalX, GlobalY, St0)
        end
    end;

handle_tweak_event1(Ev,T) ->
    handle_tweak_event2(Ev,T).

handle_tweak_event2(init_opengl, #tweak{st=St}) ->
    wings:init_opengl(St),
    keep;
handle_tweak_event2(quit=Ev, T) ->
    wings_wm:later(Ev),
    exit_tweak(T);

handle_tweak_event2({current_state,St}, T) ->
    update_tweak_handler(T#tweak{st=St});

handle_tweak_event2({new_state,St1}, #tweak{st=St0}=T) ->
    St2 = clear_temp_sel(St1),
    St3 = wings_undo:save(St0, St2),
    St = case St3 of
         #st{saved=false} -> St3;
         _Other -> wings_u:caption(St3#st{saved=false})
    end,
    update_tweak_handler(T#tweak{st=St});

handle_tweak_event2({action,Action}, #tweak{tmode=wait,st=#st{}=St0}=T) ->
    Hs = wings_pref:get_value(hilite_select),
    case Action of
    {view,aim} ->
        St = wings_view:command(aim, St0),
        update_tweak_handler(T#tweak{st=St});
    {view,highlight_aim} ->
        HL0 = wings_pref:get_value(highlight_aim_at_unselected),
        HL1 = wings_pref:get_value(highlight_aim_at_selected),
        {_,X,Y} = wings_wm:local_mouse_state(),
        {{_,Cmd0},St1} = case wings_pick:do_pick(X, Y, St0) of
              {add,_,St2} when HL0 =:= true ->
                  {{view,highlight_aim},{add,St0,St2}};
              {delete,_,St2} when HL1 =:= true ->
                  {{view,highlight_aim},{delete,St0,St2}};
              _Other ->
                  {{view,aim}, St0}
        end,
        St = wings_view:command(Cmd0,St1),
        update_tweak_handler(T#tweak{st=St});

    {edit,undo_toggle} ->
        St = wings_u:caption(wings_undo:undo_toggle(clear_temp_sel(St0))),
        update_tweak_handler(T#tweak{st=St});
    {edit,undo} ->
        St = wings_u:caption(wings_undo:undo(clear_temp_sel(St0))),
        update_tweak_handler(T#tweak{st=St});
    {edit,redo} ->
        St = wings_u:caption(wings_undo:redo(clear_temp_sel(St0))),
        update_tweak_handler(T#tweak{st=St});
    {select,{edge_loop,edge_loop}}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,{edge_loop,edge_ring}}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,{oriented_faces,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,{similar_material,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,{similar_area,_}}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,similar}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select,all}=Cmd when Hs -> hotkey_select_setup(Cmd,T);
    {select, C}=Cmd when C==vertex;C==edge;C==face;C==body ->
        St = clear_temp_sel(St0),
        do_cmd(Cmd,T#tweak{st=St});
    {select, {adjacent,_}}=Cmd ->
        St = clear_temp_sel(St0),
        do_cmd(Cmd,T#tweak{st=St});
    {file,_}=Cmd ->
        St = clear_temp_sel(St0),
        do_cmd(Cmd,T#tweak{st=St});
    keep -> keep;
    Cmd ->
        do_cmd(Cmd, T)
   end;

handle_tweak_event2({action,Action}, #tweak{tmode=drag}=T) ->
    case Action of
      {select, more} -> do_cmd(Action, T);
      {select, less} -> do_cmd(Action, T);
      Action -> keep
    end;

handle_tweak_event2(_, T) ->
    update_tweak_handler(T).

pick_event(X, Y, Cam, B, #tweak{st=#st{}=St0}=T0) ->
    case wings_pick:do_pick(X, Y, St0) of
      {add,MM,St1} ->
        begin_drag(MM, St1, T0),
        do_tweak(0.0, 0.0, 0.0, 0.0, screen),
        Time = now(),
        T = T0#tweak{tmode=drag,ox=X,oy=Y,cx=X,cy=Y,dc={Time,0}},
        update_tweak_handler(T);
      {delete,MM,_} ->
        begin_drag(MM, St0, T0),
        do_tweak(0.0, 0.0, 0.0, 0.0, screen),
        Time = now(),
        T = T0#tweak{tmode=drag,ox=X,oy=Y,cx=X,cy=Y,dc={Time,0}},
        update_tweak_handler(T);
      none when B == 1 ->
        wings_pick:marquee_pick(X, Y, St0);
      none when Cam == maya; Cam == mb ->
        update_tweak_handler(T0)
    end.

popup_menu(X, Y, #st{sel=[]}=St) ->
    wings_shapes:menu(X, Y, St);
popup_menu(X, Y, #st{selmode=Mode}=St) ->
    case wings_light:is_any_light_selected(St) of
    true -> wings_light:menu(X, Y, St);
    false ->
        case Mode of
        vertex -> wings_vertex_cmd:menu(X, Y, St);
        edge -> wings_edge_cmd:menu(X, Y, St);
        face -> wings_face_cmd:menu(X, Y, St);
        body -> wings_body:menu(X, Y, St)
        end
    end.

exit_tweak(#tweak{st=#st{}=St}=T) ->
    remember_mode(T),
    wings_wm:later({new_state,St}),
    wings_wm:later({current_state,St}),
    pop.

remember_mode(#tweak{magnet=Mag,mag_type=MagType,mag_r=MagR,
             st=#st{selmode=Mode,sh=Sh}}) ->
    wpa:pref_set(?MODULE, sel_mode, {Mode,Sh,Mag,MagType,MagR}).

do_cmd({tools, {tweak,false}}, #tweak{st=St}=T) ->
    exit_tweak(T#tweak{st=clear_temp_sel(St)});

do_cmd({tools, {tweak,true}}, #tweak{st=St}=T) ->
    wings_plugin:command({tools, {tweak,true}}, St),
    exit_tweak(T#tweak{st=clear_temp_sel(St)});

do_cmd({select, less}, #tweak{tmode=drag}=T) ->
    update_tweak_handler(magnet_radius(-1,T));

do_cmd({select, more}, #tweak{tmode=drag}=T) ->
    update_tweak_handler(magnet_radius(1, T));

do_cmd(Cmd, #tweak{st=#st{}=St0}=T) ->
    St1 = remember_command(Cmd, St0),
    case wings_plugin:command(Cmd,St1) of
      next -> do_wings_cmd(Cmd,T);
      Result -> process_cmd_response(Result,T)
    end.

do_wings_cmd({view,Cmd}, #tweak{st=#st{}=St0}=T) ->
    case wings_view:command(Cmd,St0) of
        #st{}=St ->
            St1 = clear_temp_sel(St),
            update_tweak_handler(T#tweak{st=St1});
        Other ->
          Other
    end;

do_wings_cmd(Cmd, #tweak{st=#st{}=St0}=T) ->
    St1 = remember_command(Cmd, St0),
    Result = cmd_type(Cmd, St1),
    process_cmd_response(Result,T).

process_cmd_response(Result,T) ->
    case Result of
      {save_state,St} ->
          handle_tweak_event2({new_state,St}, T);
      #st{}=St ->
          update_tweak_handler(T#tweak{st=St});
      {drag,Drag} ->
          wings_drag:do_drag(Drag, none);
      keep ->
          keep;
      {saved,St} ->
          update_tweak_handler(T#tweak{st=St});
      {new,St} ->
          update_tweak_handler(T#tweak{st=wings_u:caption(wings_undo:init(St))});
      quit ->
          exit_tweak(T),
          wings:save_windows(),
          exit(normal);
      {replace,Ev} ->
          handle_tweak_event2(Ev, T);
      Other ->
        Other
    end.

cmd_type({select, Cmd}, St) -> wings_sel_cmd:command(Cmd, St#st{temp_sel=none});
cmd_type(Cmd, St) -> wings:command(Cmd, St).

hotkey_select_setup(Cmd,#tweak{st=St0}=T) ->
    {_,X,Y} = wings_wm:local_mouse_state(),
    case wings_pick:do_pick(X, Y, St0) of
      {add,_,St} -> do_cmd(Cmd,T#tweak{st=St});
      _Other     -> do_cmd(Cmd, T)
    end.

remember_command({C,_}=Cmd, St) when C =:= vertex; C =:= edge;
                     C =:= face; C =:= body ->
    St#st{repeatable=Cmd,ask_args=none,drag_args=none};
remember_command(_Cmd, St) -> St.

redraw(St) ->
    wings:redraw(St),
    keep.

begin_drag(MM, St, T) ->
    wings_draw:refresh_dlists(St),
    wings_dl:map(fun(D, _) ->
             begin_drag_fun(D, MM, St, T)
         end, []).

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
    St = wings_dl:map(fun end_drag/2, St0),
    help(T),
    handle_tweak_event2({new_state,St},T#tweak{tmode=wait}).

end_drag(#dlo{src_we=#we{id=Id},drag=#drag{}}=D0, #st{shapes=Shs0}=St0) ->
    #dlo{src_we=We} = D = wings_draw:join(D0),
    TwkCtrl = wings_pref:get_value(tweak_ctrl),
    L = wings_pref:get_value(tweak_single_click),
    LL = wings_pref:get_value(tweak_double_click),
    Lm = L == false andalso LL == false,
    Cam = wings_pref:get_value(camera_mode),
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    St = case {mod_key_combo(),TwkCtrl} of
        {{true,true,true},_} when MayaMod ->
            {Nc,We1} = collapse_short_edges(0.0001,We),
            Shs = gb_trees:update(Id,We1, Shs0),
            if
              Nc -> St0#st{shapes=Shs};
              true -> St0#st{shapes=Shs,sel=[]}
            end;
        {{true,true,true},select} ->
            {Nc,We1} = collapse_short_edges(0.0001,We),
            Shs = gb_trees:update(Id,We1, Shs0),
            if
              Nc -> St0#st{shapes=Shs};
              true -> St0#st{shapes=Shs,sel=[]}
            end;
        {{false,true,true},slide} when not Lm andalso not MayaMod ->
            {Nc,We1} = collapse_short_edges(0.0001,We),
            Shs = gb_trees:update(Id,We1, Shs0),
            if
              Nc -> St0#st{shapes=Shs};
              true -> St0#st{shapes=Shs,sel=[]}
            end;
        _Otherwise ->
            Shs = gb_trees:update(Id, We, Shs0),
            St0#st{shapes=Shs}
    end,
    {D#dlo{vs=none,sel=none,drag=none},St};
end_drag(#dlo{src_we=#we{id=Id},drag={matrix,_,Matrix,_}}=D,
        #st{shapes=Shs0}=St0) ->
    We0 = gb_trees:get(Id, Shs0),
    We = wings_we:transform_vs(Matrix, We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St = St0#st{shapes=Shs},
    D1 = D#dlo{src_we=We},
    D2 =wings_draw:changed_we(D1, D),
    {D2#dlo{vs=none,sel=none,drag=none},St};
end_drag(D, St) -> {D, St}.

end_pick(true, #tweak{st=#st{selmode=Selmode}=St0}=T0) ->
    St1 = wings_dl:map(fun end_pick_1/2, St0),
    {_,X,Y} = wings_wm:local_mouse_state(),
    St = case wings_pick:do_pick(X, Y, St1#st{selmode=Selmode}) of
        {_,_,St2} -> St2;
        none -> St1
    end,
    T = T0#tweak{st=St0,tmode=wait},
    help(T),
    handle_tweak_event2({new_state,St},T);

end_pick(false, #tweak{st=St0}=T) ->
    St = wings_dl:map(fun end_pick_1/2, St0),
    help(T),
    handle_tweak_event2({new_state,St},T#tweak{tmode=wait}).

end_pick_1(#dlo{mirror=M,ns=Ns,proxy_data=Pd,src_we=We},St0) ->
    {#dlo{ns=Ns,mirror=M,proxy_data=Pd,src_we=We},St0}.

sel_to_vs(edge, _, We) when ?IS_LIGHT(We) ->
    Items = gb_sets:to_list(wings_sel:get_all_items(edge, We)),
    wings_edge:to_vertices(Items,We);
sel_to_vs(Mode, _, We) when ?IS_LIGHT(We) ->
    gb_sets:to_list(wings_sel:get_all_items(Mode, We));
sel_to_vs(vertex, Vs, _) -> Vs;
sel_to_vs(edge, Es, We) -> wings_vertex:from_edges(Es, We);
sel_to_vs(face, [Face], We) -> wings_face:vertices_ccw(Face, We);
sel_to_vs(face, Fs, We) -> wings_face:to_vertices(Fs, We).

do_tweak(DX, DY, DxOrg,DyOrg,Mode) ->
    wings_dl:map(fun
        (#dlo{src_we=We}=D, _) when ?IS_LIGHT(We) ->
             do_tweak(D, DX, DY, DxOrg, DyOrg, screen);
        (D, _) ->
             do_tweak(D, DX, DY, DxOrg, DyOrg, Mode)
         end, []).

%%
%%  Additional functions for lookup around vertex
%%

collect_neib_faces(V,#we{mirror=MirrorFace}=We) ->
    %% The We is not a complete one, but from the display lists. Therefore,
    %% the face table is not complete. In particular, it does not contain
    %% hidden faces (negative face numbers), so we must ignore any negative
    %% face number.
    wings_vertex:fold(fun(_, Face, _, A) when Face =/= MirrorFace,
                          Face >= 0 ->
                  [Face|A];
             (_,_,_,A) ->
                  A
              end, [],V,We).

collect_neib_verts(V,#we{es=Es}=We) ->
    Facelist=collect_neib_faces(V,We),
    foldl(fun(Face,D) ->
          Edges = wings_face:to_edges([Face], We),
          NearVerts=foldl(fun(E,B) ->
                      #edge{vs=Vs,ve=Ve}=gb_trees:get(E,Es),
                      if
                          V==Vs -> [Ve|B];
                          V==Ve -> [Vs|B];
                          true -> B
                      end
                  end,[],Edges),
          NearVerts ++ D
      end, [],Facelist).

check_if_face_contains_vs(Face,We,Vs)->
    Verts=wings_face:to_vertices([Face],We),
    foldl(fun({Vert,_,_,_,_},P)->
          case member(Vert,Verts) of
              true -> P;
              _ -> none
          end
      end,all,Vs).

check_if_Vs_have_V(V,Vs)->
    foldl(fun({VinVs,_,_,_,_},Res)-> if VinVs==V -> true; true->Res end end,false,Vs).

check_if_Vs_have_V12(V1,V2,Vs)->
    case check_if_Vs_have_V(V1,Vs) of
    true -> check_if_Vs_have_V(V2,Vs);
    _ -> false
    end.

get_nverts(Vs)->
    foldl(fun(_,S)->S+1 end,0,Vs).

collect_neib_verts_vs(V,#we{es=Es}=We,Vs) ->
    Facelist0=collect_neib_faces(V,We),
    Facelist=case get_nverts(Vs) of
         2-> foldl(fun(Face,FL)->
                   Res=check_if_face_contains_vs(Face,We,Vs),
                   case	Res of
                       all -> [Face|FL];
                       _ ->FL
                   end
               end,[],Facelist0);
         _->Facelist0
         end,
    foldl(fun(Face,D) ->
          Edges = wings_face:to_edges([Face], We),
          NearVerts=foldl(fun(E,B) ->
                      Edg=gb_trees:get(E,Es),
                      #edge{vs=VS,ve=VE}=Edg,
                      Have= case get_nverts(Vs) of
                            2 -> check_if_Vs_have_V12(VE,VS,Vs);
                            _ ->false
                        end,
                      if
                          Have==true -> B;
                          V==VS -> [VE|B];
                          V==VE -> [VS|B];
                          true -> B
                      end
                  end,[],Edges),
          NearVerts ++ D
      end, [],Facelist).

collect_neib_verts_coor(V,We)->
    VertList=collect_neib_verts(V,We),
    foldl(fun(E,B) -> [wings_vertex:pos(E,We)|B] end,[],VertList).

get_orig_pos(V,We,Vs)->
    Pos=foldl(
      fun({Vert,Coor,_,_,_},P) ->
          if V==Vert -> Coor; true-> P end
      end,none,Vs),
    case Pos of
    none -> wings_vertex:pos(V,We);
    _ -> Pos
    end.

collect_neib_verts_coor_vs(V,We,Vs)->
    VertList=collect_neib_verts_vs(V,We,Vs),
    foldl(fun(E,B) -> [get_orig_pos(E,We,Vs)|B] end,[],VertList).

sub_pos_from_list(List,Pos) ->
    foldl(fun(E,B) -> [e3d_vec:sub(E,Pos)|B] end,[],List).

relax_vec(V, We) ->
    case collect_neib_verts_coor(V, We) of
    [] ->
        %% Because of hidden faces there may be no neighbouring vertices,
        %% so we default to the position of the vertex itself.
        wings_vertex:pos(V, We);
    Cs ->
        e3d_vec:average(Cs)
    end.

slide_one_vec(Vpos, TweakPos, PosList) ->
    Dpos=e3d_vec:sub(TweakPos,Vpos),
    {Dp,_} = foldl(fun(Vec, {VP,W}) ->
              Vn = e3d_vec:norm(Vec),
              Dotp0 = e3d_vec:dot(Vn,Dpos),
              {Dotp,Sign} = if
                  Dotp0 < 0 -> {-Dotp0/1.5, -1.0};
                  true -> {Dotp0, 1.0}
              end,
              if
                  Dotp > W ->
                      Len = e3d_vec:len(Vec),
                      Dotp2 = if
                          Dotp > Len -> Len;
                          true -> Dotp
                      end,
                      {e3d_vec:mul(Vn, Dotp2 * Sign),Dotp};
                  true -> {VP,W}
              end
     end,{{0,0,0},0},PosList),
    e3d_vec:add(Vpos,Dp).

slide_vec_w(V, Vpos, VposS, TweakPosS, We, W,Vs) ->
    Dv = e3d_vec:sub(VposS,Vpos),
    TweakPos = e3d_vec:sub(TweakPosS, Dv),
    Cs = sub_pos_from_list(collect_neib_verts_coor_vs(V, We, Vs), Vpos),
    TweakPos2=e3d_vec:add(Vpos, e3d_vec:mul(e3d_vec:sub(TweakPos, Vpos), W)),
    slide_one_vec(Vpos, TweakPos2, Cs).

relax_vec(V, #we{}=We,Pos0,Pos,Weight) ->
    Vec = relax_vec(V,We),
    Len = e3d_vec:dist(Pos0,Pos),
    Len1 = if
        Len > 1 -> 1.0;
        true -> Len
    end,
    D = e3d_vec:sub(Vec,Pos0),
    e3d_vec:add_prod(Pos0, D, Len1 * Weight).

relax_vec_fn(V, #we{}=We,Pos0,Weight) ->
    Vec = relax_vec(V,We),
    D = e3d_vec:sub(Vec,Pos0),
    e3d_vec:add_prod(Pos0, D, Weight).

%%
%% scanning over the mesh to collapse short edges
%%

collapse_short_edges(Tolerance, #we{es=Etab,vp=Vtab}=We) ->
    Short = foldl(
          fun({Edge,#edge{vs=Va,ve=Vb}}, A) ->
              case gb_trees:is_defined(Va,Vtab) of
              true->
                  case gb_trees:is_defined(Vb,Vtab) of
                  true->
                      VaPos = wings_vertex:pos(Va, We),
                      VbPos = wings_vertex:pos(Vb, We),
                      case abs(e3d_vec:dist(VaPos, VbPos)) of
                      Dist when Dist < Tolerance -> [Edge|A];
                      _Dist -> A
                      end;
                  false-> A
                  end;
              false -> A
              end
          end, [], gb_trees:to_list(Etab)),
    NothingCollapsed = Short == [],
    We1 = wings_collapse:collapse_edges(Short,We),
    {NothingCollapsed, We1}.

%%
%% end of additional geo-functions block
%%

do_tweak(#dlo{drag={matrix,Pos0,Matrix0,_},src_we=#we{id=Id}}=D0,
     DX,DY,_,_,Mode) ->
    Matrices = wings_u:get_matrices(Id, original),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    Pos = case Mode of
        xmove -> {Tx,Py,Pz};
        ymove -> {Px,Ty,Pz};
        zmove -> {Px,Py,Tz};
        xymove -> {Tx,Ty,Pz};
        yzmove -> {Px,Ty,Tz};
        zxmove -> {Tx,Py,Tz};
        _Other -> TweakPos
    end,
    Move = e3d_vec:sub(Pos, Pos0),
    Matrix = e3d_mat:mul(e3d_mat:translate(Move), Matrix0),
    D0#dlo{drag={matrix,Pos,Matrix,e3d_mat:expand(Matrix)}};

do_tweak(#dlo{drag=#drag{vs=Vs,pos=Pos0,pos0=Orig,mag=Mag0,mm=MM}=Drag,
          src_we=#we{id=Id}=We}=D0, DX, DY, DxOrg, DyOrg,Mode) ->
    Matrices = wings_u:get_matrices(Id, MM),
    {Xs,Ys,Zs} = obj_to_screen(Matrices, Pos0),
    TweakPos = screen_to_obj(Matrices, {Xs+DX,Ys-DY,Zs}),
    [V2|_]=Vs,
    {Tx,Ty,Tz} = TweakPos,
    {Px,Py,Pz} = Pos0,
    {Vtab,Mag} =
    case Mode of
        xmove -> Pos = {Tx,Py,Pz},
             magnet_tweak(Mag0, Pos);
        ymove -> Pos = {Px,Ty,Pz},
             magnet_tweak(Mag0, Pos);
        zmove -> Pos = {Px,Py,Tz},
             magnet_tweak(Mag0, Pos);
        xymove -> Pos = {Tx,Ty,Pz},
              magnet_tweak(Mag0, Pos);
        yzmove -> Pos = {Px,Ty,Tz},
              magnet_tweak(Mag0, Pos);
        zxmove -> Pos = {Tx,Py,Tz},
              magnet_tweak(Mag0, Pos);
        relax -> Pos=relax_vec(V2,We,Pos0,TweakPos,1.0),
              Len=(abs(DxOrg)+abs(DyOrg))/200.0,
              Len1=case Len>1 of
                   true -> 1.0;
                   false -> Len
               end,
              magnet_tweak_fn(Mag0, Pos,We,Len1);
        slide -> Pos = TweakPos,
              magnet_tweak_slide_fn(Mag0, We,Orig,TweakPos);
        normal -> Pos = tweak_pos(true,false,Vs, Pos0, TweakPos, D0),
              magnet_tweak(Mag0, Pos);
        tangent -> Pos = tweak_pos(false,true,Vs, Pos0, TweakPos, D0),
              magnet_tweak(Mag0, Pos);
        _ 	-> Pos = TweakPos,
              magnet_tweak(Mag0, Pos)
    end,
    D = D0#dlo{sel=none,drag=Drag#drag{pos=Pos,mag=Mag}},
    wings_draw:update_dynamic(D, Vtab);
do_tweak(D, _, _, _, _, _) -> D.

obj_to_screen({MVM,PM,VP}, {X,Y,Z}) ->
    glu:project(X, Y, Z, MVM, PM, VP).

screen_to_obj({MVM,PM,VP}, {Xs,Ys,Zs}) ->
    glu:unProject(Xs, Ys, Zs, MVM, PM, VP).

tweak_pos(false, true, _, Pos0, TweakPos, #dlo{src_we=#we{}=We,src_sel={face,Sel0}}) ->
    Faces = gb_sets:to_list(Sel0),
    FaceRegions = wings_sel:strict_face_regions(Faces,We),
    Normals = face_region_normals(FaceRegions,We,[]),
    case Normals of
    [[]] -> TweakPos;
    _Otherwise ->
        N = e3d_vec:average(Normals),
    %% constraining by the plane
    Dot = e3d_vec:dot(N, N),
      if
      Dot == 0.0 -> Pos0;
      true ->
        T = -e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
        e3d_vec:add_prod(TweakPos, N, T)
      end
    end;

tweak_pos(false, true,Vs, Pos0, TweakPos, D) ->
    Normals = [vertex_normal(V, D) || V <- Vs],
    N = e3d_vec:average(Normals),
    %% constraining by the plane
    Dot = e3d_vec:dot(N, N),
      if
      Dot == 0.0 -> Pos0;
      true ->
        T = -e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
        e3d_vec:add_prod(TweakPos, N, T)
      end;

%%%% Along Average Normal
tweak_pos(true, false, _, Pos0, TweakPos, #dlo{src_we=#we{}=We,src_sel={face,Sel0}}) ->
    Faces = gb_sets:to_list(Sel0),
    FaceRegions = wings_sel:strict_face_regions(Faces,We),
    Normals = face_region_normals(FaceRegions,We,[]),
    case Normals of
    [[]] -> TweakPos;
    _Otherwise ->
        N = e3d_vec:average(Normals),
        %% Return the point along the normal closest to TweakPos.
        Dot = e3d_vec:dot(N, N),
        if
        Dot == 0.0 -> Pos0;
        true ->
          T = e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
          e3d_vec:add_prod(Pos0, N, T)
        end
    end;

tweak_pos(true, false, Vs, Pos0, TweakPos, D) ->
    Normals = [vertex_normal(V, D) || V <- Vs],
    N = e3d_vec:average(Normals),
    %% Return the point along the normal closest to TweakPos.
    Dot = e3d_vec:dot(N, N),
      if
      Dot == 0.0 -> Pos0;
      true ->
        T = e3d_vec:dot(N, e3d_vec:sub(TweakPos, Pos0)) / Dot,
        e3d_vec:add_prod(Pos0, N, T)
      end.

face_region_normals([Faces|Regions],We,Normals) ->
    Edges = wings_face:outer_edges(Faces, We),
    LoopNorm = loop_norm(Edges, We),
    face_region_normals(Regions,We,[LoopNorm|Normals]);
face_region_normals([],_,Normals) ->
    Normals.

loop_norm([], _) ->
    [];
loop_norm(Edges,We) ->
%%%% Return average normal of multiple loops in a single face region
    Loops = wings_edge_loop:edge_loop_vertices(Edges, We),
    loop_norm_1(Loops, We, []).

loop_norm_1([Vs|Loops], We, Normals) ->
    Norm = wings_face:face_normal_ccw(Vs, We),
    loop_norm_1(Loops, We, [Norm|Normals]);
loop_norm_1([], _, [First|Normals]) ->
    e3d_vec:norm(e3d_vec:average([e3d_vec:neg(First)]++Normals)).

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
    Constraints = [fkey_help(),[{bold,?__(3,"XYZ Constraints")}]++
        ?__(7,"(+[Alt] to Toggle)")],
    Tail = [Constraints,exit_help()],
    All = common_help(Tail),
    Msg = wings_msg:join(All),
    wings_wm:message(Msg, "[1] "++?__(4,"Magnet On"));
help(#tweak{magnet=true,mag_type=Type}) ->
    All = common_help([]),
    Msg = wings_msg:join(All),
    Types = help_1(Type, [{2,dome},{3,straight},{4,spike}]),
    MagMsg = wings_msg:join(["[1] "++?__(5,"Magnet Off"),
                 "[+]/[-] "++?__(6,"Tweak R"),
                 Types]),
    wings_wm:message(Msg, MagMsg).

%% Messages common for magnet and no magnet.
common_help(Tail0) ->
    AltMod = alt(),
    CtrlMod = ctrl(),
    ShiftMod = shift(),
    Cam = wings_pref:get_value(camera_mode),
    Button = case Cam of
      mb -> 2;
      _  -> 1
    end,
    SC = wings_pref:get_value(tweak_single_click),
    DC = wings_pref:get_value(tweak_double_click),
    NoMod = wings_pref:get_value(tweak_ctrl) == slide,
    MayaMod = Cam == maya andalso wings_pref:get_value(tweak_mmb_select),
    Tail = [slide_help(Cam, MayaMod, Button, {SC,DC,NoMod}, AltMod, CtrlMod) | Tail0],
    [wings_msg:button_format(?__(2,"Drag")),
    case {SC,DC,NoMod} of
      _Maya when MayaMod -> wings_msg:mod_format(0, 2, ?__(6,"Select"));
      {true,true,_} -> ?__(7,"L/LL:") ++" "++ ?__(6,"Select");
      {true,false,false} ->
        case Cam of
          mb -> ?__(8,"L/")++wings_msg:mod_format(AltMod,1,?__(6,"Select"));
          _  -> ?__(8,"L/")++wings_msg:mod_format(CtrlMod,1,?__(6,"Select"))
        end;
      {true,false,true} ->
        wings_msg:button_format(?__(6,"Select"));
      {false,true,false} ->
        case Cam of
          mb -> ?__(9,"LL/")++wings_msg:mod_format(AltMod,1,?__(6,"Select"));
          _  -> ?__(9,"LL/")++wings_msg:mod_format(CtrlMod,1,?__(6,"Select"))
        end;
      {false,true,true} ->
        ?__(10,"L")++wings_msg:button_format(?__(6,"Select"));
      {false,false,_} ->
        case Cam of
          mb -> wings_msg:mod_format(AltMod,1,?__(6,"Select"));
          _  -> wings_msg:mod_format(CtrlMod,1,?__(6,"Select"))
        end
    end,
    case Cam of
      maya when MayaMod -> wings_msg:mod_format(CtrlMod,1,?__(3,"Along Normal"));
      maya   -> wings_msg:mod_format(0, 2, ?__(3,"Along Normal"));
      _other -> wings_msg:mod_format(AltMod, Button, ?__(3,"Along Normal"))
    end,
    wings_msg:mod_format(CtrlMod bor ShiftMod, Button, ?__(4,"In Tangent Plane")),
    wings_msg:mod_format(AltMod bor ShiftMod, Button, ?__(5,"Relax"))|Tail].

exit_help() ->
    ?__(2,"[Esc]:") ++ " " ++ ?__(1,"Exit").

slide_help(Cam, MayaMod, Button, Prefs, AltMod, CtrlMod) ->
    {Mod,Clean} = case Cam of
        mb -> {AltMod, ?__(3,"(+[Ctrl] to Clean)")};
        _  -> {CtrlMod, ?__(4,"(+[Alt] to Clean)")}
    end,
    case Prefs of
      _Maya when MayaMod ->
        wings_msg:mod_format(AltMod bor CtrlMod, Button,{bold,?__(1,"Slide")})++
        ?__(2,"(+[Shift] to Clean)");
      {true,_,true} ->
        wings_msg:mod_format(Mod, 1,{bold,?__(1,"Slide")})++Clean;
      {_,true,true} ->
        wings_msg:mod_format(Mod, 1,{bold,?__(1,"Slide")})++Clean;
      _Otherwise ->
        wings_msg:mod_format(AltMod bor CtrlMod, Button,{bold,?__(1,"Slide")})++
        ?__(2,"(+[Shift] to Clean)")
    end.

intl_type(dome)     -> ?__(1,"Dome");
intl_type(straight) -> ?__(2,"Straight");
intl_type(spike)    -> ?__(3,"Spike").

help_1(Type, [{Digit,Type}|T]) ->
    wings_msg:join("[" ++ [$0+Digit] ++ "] " ++
           [{bold,intl_type(Type)}],
           help_1(Type, T));
help_1(Type, [{Digit,ThisType}|T]) ->
    wings_msg:join("[" ++ [$0+Digit] ++ "] " ++
           intl_type(ThisType),
           help_1(Type, T));
help_1(_, []) -> [].

fkey_help() ->
    [Fx,Fy,Fz] = wings_pref:get_value(tweak_xyz),
    F1 = case Fx of
        true -> [{bold,"F1"}];
        false -> "F1"
    end,
    F2 = case Fy of
        true -> [{bold,"F2"}];
        false -> "F2"
    end,
    F3 = case Fz of
        true -> [{bold,"F3"}];
        false -> "F3"
    end,
    "["++F1++","++F2++","++F3++"]: ".

fake_sel(#st{sel=[]}=St0) ->
    case wings_pref:get_value(use_temp_sel) of
    false -> St0;
    true ->
        {_,X,Y} = wings_wm:local_mouse_state(),
        case wings_pick:do_pick(X, Y, St0) of
        {add,_,St} -> set_temp_sel(St0,St);
        _ -> St0
        end
    end;
fake_sel(St) -> St.

set_temp_sel(#st{sh=Sh,selmode=Mode}, St) ->
    St#st{temp_sel={Mode,Sh}}.

clear_temp_sel(#st{temp_sel=none}=St) -> St;
clear_temp_sel(#st{temp_sel={Mode,Sh}}=St) ->
    St#st{temp_sel=none,selmode=Mode,sh=Sh,sel=[]}.

%%%
%%% Magnetic tweak. Standard tweak is a special case of magnetic tweak
%%% (vertices to be moved have the influence set to 1.0).
%%%

tweak_hotkey(C, #tweak{magnet=Mag}=T) ->
    case magnet_hotkey(C) of
    none -> constraint_hotkey();
    toggle when Mag == true ->
        setup_magnet(T#tweak{magnet=false});
    toggle when Mag == false ->
        setup_magnet(T#tweak{magnet=true});
    _ when Mag == false -> constraint_hotkey();
    Type -> setup_magnet(T#tweak{mag_type=Type})
    end.

constraint_hotkey() ->
%% Alt + F1/2/3 toggles xyx constraints on/off
    Alt = mod_key_combo() == {false,false,true},
    Fkeys = fkey_combo(),
    Constraints = wings_pref:get_value(tweak_xyz),
    case Alt of
      false -> none;
      true when Fkeys =/= [false,false,false] ->
        C = set_constraint_toggles(Fkeys,Constraints,[]),
        wings_pref:set_value(tweak_xyz,C),
        none;
      _other -> none
    end.
set_constraint_toggles([true|Fkeys],[Pref|Constraints],C) ->
    NewC = case Pref of
      true -> false;
      false -> true
    end,
    set_constraint_toggles(Fkeys,Constraints,[NewC|C]);
set_constraint_toggles([false|Fkeys],[Pref|Constraints],C) ->
    set_constraint_toggles(Fkeys,Constraints,[Pref|C]);
set_constraint_toggles([],[],C) ->
    lists:reverse(C).

tweak_constraints([true|Fkeys],[false|Tkeys],Constraints) ->
    tweak_constraints(Fkeys,Tkeys,[true|Constraints]);
tweak_constraints([true|Fkeys],[true|Tkeys],Constraints) ->
    tweak_constraints(Fkeys,Tkeys,[false|Constraints]);
tweak_constraints([_|Fkeys],[Key|Tkeys],Constraints) ->
    tweak_constraints(Fkeys,Tkeys,[Key|Constraints]);
tweak_constraints([],[],Constraints) ->
    lists:reverse(Constraints).

magnet_hotkey($1) -> toggle;
magnet_hotkey($2) -> dome;
magnet_hotkey($3) -> straight;
magnet_hotkey($4) -> spike;
magnet_hotkey(_) -> none.

setup_magnet(#tweak{tmode=drag}=T) ->
    wings_dl:map(fun(D, _) ->
             setup_magnet_fun(D, T)
         end, []),
    do_tweak(0.0, 0.0, 0.0, 0.0,screen),
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
    M0 = foldl(fun({V,Pos}, A) ->
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
    M = minus_locked_vs(M0,We),
    foldl(fun(V, A) ->
          Matrix = mirror_matrix(V, Mirror),
          Pos = wpa:vertex_pos(V, We),
          [{V,Pos,Matrix,0.0,1.0}|A]
      end, M, Vs).

minus_locked_vs(MagVs, #we{pst=Pst}) ->
    Mask = wings_pref:get_value(magnet_mask_on),
    case gb_trees:is_defined(wpc_magnet_mask,Pst) of
      true when Mask ->
        LockedVs = gb_sets:to_list(wpc_magnet_mask:get_locked_vs(Pst)),
        remove_masked(LockedVs, MagVs);
      _otherwise ->
        MagVs
    end.

remove_masked([V|LockedVs],MagVs) ->
    remove_masked(LockedVs,lists:keydelete(V,1,MagVs));
remove_masked([],MagVs) -> MagVs.

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

magnet_tweak_fn(#mag{vs=Vs}=Mag, _,We,Weight) ->
    Vtab = foldl(fun({V,P0,Plane,_,1.0}, A) ->
             P1=relax_vec_fn(V,We,P0,Weight),
             P = mirror_constrain(Plane, P1),
             [{V,P}|A];
            ({V,P0,Plane,_,Inf}, A) ->
             P1=relax_vec_fn(V,We,P0,Weight*Inf),
             P = mirror_constrain(Plane, P1),
             [{V,P}|A]
         end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

magnet_tweak_slide_fn(#mag{vs=Vs}=Mag, We,Orig,TweakPos) ->
    Vtab = foldl(fun({V,P0,Plane,_,Inf}, A) ->
             P1=slide_vec_w(V,P0,Orig,TweakPos,We,Inf,Vs),
             P = mirror_constrain(Plane, P1),
             [{V,P}|A]
         end, [], Vs),
    {Vtab,Mag#mag{vtab=Vtab}}.

magnet_radius(Sign, #tweak{mag_r=Falloff0,st=St}=T0) ->
    case Falloff0+Sign*?GROUND_GRID_SIZE/10 of
    Falloff when Falloff > 0 ->
        setup_magnet(T0#tweak{mag_r=Falloff,st=St});
    _Falloff -> T0#tweak{st=St}
    end.

get_inv_magnet_value2(MagType,Value,Pos) ->
    Step=0.1,
    V1=mf(MagType,Pos,1.0),
    V2=mf(MagType,Pos+Step,1.0),
    case	Value<V1 andalso Value>=V2 of
    true -> Pos+(V1-Value)/(V1-V2)*Step;
    false -> get_inv_magnet_value2(MagType,Value,Pos+Step)
    end.
get_inv_magnet_value(MagType,Value) ->
    get_inv_magnet_value2(MagType,Value,0.0).

draw_magnet(#tweak{magnet=false}) -> ok;
draw_magnet(#tweak{st=#st{selmode=body}}) -> ok;
draw_magnet(#tweak{mag_r=R,mag_type=Mt}) ->
    gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
    gl:disable(?GL_DEPTH_TEST),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_view:load_matrices(false),
    gl:color4f(0, 0, 1, 0.06),
    R2=[get_inv_magnet_value(Mt,X/10.0)||X<-lists:seq(1,9)],
    wings_dl:fold(fun(D, _) -> draw_magnet_1(D, R,R2) end, []),
    gl:popAttrib().

draw_magnet_1(#dlo{mirror=Mtx,drag=#drag{mm=Side,pos={X,Y,Z}}}, R,R2) ->
    case Side of
    mirror -> gl:multMatrixf(Mtx);
    original -> ok
    end,
    gl:translatef(X, Y, Z),

    Obj = glu:newQuadric(),
    glu:quadricDrawStyle(Obj, ?GLU_FILL),
    glu:quadricNormals(Obj, ?GLU_SMOOTH),
    glu:sphere(Obj, R, 20, 20),
    glu:deleteQuadric(Obj),
    gl:color4f(0, 0, 1, 0.03),
    lists:foreach(
      fun(R3) ->
          Obj2 = glu:newQuadric(),
          glu:quadricDrawStyle(Obj2, ?GLU_FILL),
          glu:quadricNormals(Obj2, ?GLU_SMOOTH),
          glu:sphere(Obj, R3*R, 20, 20),
          glu:deleteQuadric(Obj2)
      end,R2);

draw_magnet_1(_, _,_) -> [].

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

geom_title(geom) ->
    ?__(1,"Geometry");
geom_title({geom,N}) ->
    ?__(2,"Geometry #") ++ integer_to_list(N).
