%%
%%  wpc_sculpt.erl --
%%
%%     A plugin to add a few basic sculpting tools.
%%
%%  Copyright (c) 2010-2011 Richard Jones
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_sculpt).
-export([init/0,menu/2,command/2]).
-export([update_dlist/3,draw/4,get_data/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-define(ALPHA_THUMB_SIZE, 64).
-define(ALPHA_THUMB_COLOUR, {1.0,1.0,0.0}).
-define(DEGREE2RAD, 0.01745329251994323).
-define(RAD2DEGREE, 57.2957795130823209).

-include_lib("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").

-import(lists, [foldl/3,sort/1,reverse/1,member/2]).

-record(sculpt,
    {mode,			% pull, pinch, smooth
     active=false,	% sculpt active true|false
     id=none,		% id of initial object
     mag,			% magnet true|false
     rad,			% magnet radius
     mag_type,		% magnet type
     str,			% strength
     wgt,			% weight
     mir,			% mirror info
     locked,		% masked vertices
     st,			% state
     alp_info :: tuple(),  % alpha start coord and seg_inf
     wst,			% working state
     ost}).			% original state

-type vtx_location() :: 'none' | e3d_vector().
-record(vl,
    {v=none :: vtx_location(),    % vertex location 
     vn=none :: vtx_location()}). % vertex normal

-type vl() :: 'none' | #vl{}.
-record(seg_inf,        % segment information
    {va=none :: vl(),   % begin vertex
     vb=none :: vl(),   % end vertex
     vc=none :: vl()}). % current vertex

init() ->
    wings_pref:set_default(sculpt_strength, 0.005),
    wings_pref:set_default(sculpt_mode, pull),
    wings_pref:set_default(sculpt_magnet, {false,1.0}),
    wings_pref:set_default(sculpt_magnet_type, dome),
    wings_pref:set_default(sculpt_initial, false),
    BrushFile=wings_util:lib_dir(wings)++"/textures/brush.png",
    wings_pref:set_default(sculpt_alpha_brush,BrushFile),
    wings_pref:set_default(alpha_brush_weight, 0.5),
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
    Active = wings_wm:this(),
    wings_tweak:toggle_draw(false),
    St = wings_undo:init(St0#st{selmode=face,sel=[],sh=false}),
    Mir = mirror_info(Shs, []),
    prepare_alpha_data(wings_pref:get_value(sculpt_mode)),
    Mode = wings_pref:get_value(sculpt_mode),
    Str = wings_pref:get_value(sculpt_strength),
    Wgt = wings_pref:get_value(alpha_brush_weight),
    {Mag,Rad} = wings_pref:get_value(sculpt_magnet),
    MagType = wings_pref:get_value(sculpt_magnet_type),
    Lv = shape_attr(gb_trees:to_list(Shs)),
    wings_pref:set_default(sculpt_current_id, none),
    Sc = #sculpt{mode=Mode,mir=Mir,str=Str,wgt=Wgt,mag=Mag,rad=Rad,mag_type=MagType,
          locked=Lv,st=St,wst=St,ost=St0,
          alp_info={0,0,sculpt,#seg_inf{va=none,vb=none,vc=none},none}},
    wings:mode_restriction([face]),
    wings_wm:callback(fun() -> wings_u:menu_restriction(Active, [view]) end),
    {seq,push,update_sculpt_handler(Sc)}.

prepare_alpha_data(Mode) ->
   % it prevents Wings to crash if something wrong happen in alpha_brush in a previous operation 
    case ets:info(alpha_brush) of
      undefined -> ets:new(alpha_brush,[set,named_table]);
      _ -> unload_alpha_data()
    end,
    case Mode of
      alpha_brush ->
      	load_alpha_data();
      _ -> ok 
    end.

unload_alpha_data() ->
    case ets:lookup(alpha_brush,uv_map) of
      [{uv_map,TxId}] ->
        wings_image:unload_texture(TxId),
        ets:delete(alpha_brush,uv_map);    
      _ -> ok
    end,
    ets:delete_all_objects(alpha_brush).
    
load_alpha_data() ->
	case wings_pref:get_value(sculpt_alpha_brush) of
	  Name when Name=:="" ->
		wings_pref:set_value(sculpt_mode,pull), % set to default sculpt mode 
		wings_u:message(?__(1,"You need to choose a brush.\nGo to Sculpt Preferences."));
	  FileName -> 
		load_alpha_image(FileName)
	end.

load_alpha_image(Name) ->
    Props = [{filename,Name}],
    case wpa:image_read(Props) of
        #e3d_image{}=Image0 -> 
            case wings_image:load_texture(Image0) of
            {error,_} -> 
                wings_u:message(?__(1,"There were an error loading the texture.\nSee the console window for details."));
            TxId ->
                case e3d_image:convert(Image0,g8) of
                    {error, Reason} ->
                    	wings_image:unload_texture(TxId),
                        wings_u:message(?__(2,"Error converting image to grayscale.\n")++
                        io_lib:format(?__(3,"Reason:")++" ~p",[Reason]));
                    Image -> 
                        ets:insert(alpha_brush,{uv_map,TxId}),    
                        alpha2weight(Image)
                end
            end;
        {error,_} -> 
            wings_u:message(?__(4,"Alpha brush file not found.\nGo to Sculpt Preferences."))
    end.

alpha2weight(#e3d_image{width=W0,height=H0,image=Pixels0}=Image) ->
    ets:insert(alpha_brush,{image,Image}),    
    {W,H,MaxX,MaxY,ZeroOfs,Pixels}=prepare_img2cart({W0,H0,Pixels0}),
    PixLst=binary_to_list(Pixels),
    AlpInf=lists:foldl(fun(Item,Acc) ->
        Weight=Item/255.0,
        [Weight | Acc]
    end, [], PixLst),
    ets:insert(alpha_brush,{weight,{W,H,MaxX,MaxY,ZeroOfs,list_to_tuple(AlpInf)}}).

% force the image to have odd number of rows and cols
% to be used as zero coordenate on the cartesian axis (just for make easy calc/find values)
% if necessary, zero row or/and col is/are added
prepare_img2cart({W0,H0,Img0}) ->
    H0h=H0 div 2,
    W0h=W0 div 2,
    {H,Img1}= if ((H0 rem 2)==0) -> {H0+1,add_row_zero(Img0,W0)};
    true -> {H0,Img0}
    end,
    {W,ImgLst}= if ((W0 rem 2)==0) -> {W0+1,add_col_zero(Img1,W0)};
    true -> {W0,Img1}
    end,
    ZeroOfs=W*(H div 2)+(W div 2)+1,
    {W,H,W0h,H0h,ZeroOfs,ImgLst}.

add_row_zero(Img,W) ->
    Nl=list_to_binary(lists:duplicate(W, 0)),
    <<Img/binary,Nl/binary>>.

add_col_zero(Img,W) ->
    add_col_zero_0(W,Img,<<>>).

add_col_zero_0(_,<<>>,Acc) -> Acc;
add_col_zero_0(W,Bin,Acc) ->
    <<R:W/binary,T/binary>> =Bin,
    Zero=list_to_binary([0]),
    add_col_zero_0(W,T,<<Acc/binary,R/binary,Zero/binary>>).

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

handle_sculpt_event_0(redraw, #sculpt{st=St,mode=Mode}=Sc) ->
    wings:redraw("",St),
    help(Sc),
    draw_brush_icon(Mode),
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
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},  %% Micheus
  #sculpt{active=true,mode=alpha_brush,alp_info={_,_,SctMode,_,_}}=Sc) ->
    Sc1=case SctMode of
	  stamp -> stamp(Sc);
	  _ -> Sc
    end,
    #sculpt{st=St}=Sc0=end_sculpt(Sc1),
    wings_wm:current_state(St),
    wings_wm:dirty(), % it was necessary when I tested it with an Intel video card
    update_sculpt_handler(Sc0);
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},
  #sculpt{st=St,wst=St,active=true}=Sc) ->
    update_sculpt_handler(Sc#sculpt{id=none,active=false});
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},
  #sculpt{st=#st{shapes=Shs},mode=alpha_brush,wst=St0,active=true}=Sc) ->
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_wm:current_state(St),
    update_sculpt_handler(Sc#sculpt{id=none,st=St,wst=St,active=false});
handle_sculpt_event_1(#mousebutton{state=?SDL_RELEASED},
  #sculpt{active=true}=Sc) ->
    #sculpt{st=#st{shapes=Shs},wst=St0} =Sc0=clear_sculpt_info(Sc),
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_draw:refresh_dlists(St),
    wings_wm:current_state(St),
    wings_wm:dirty(), % it was necessary when I tested in a Intel video card
    update_sculpt_handler(Sc0#sculpt{id=none,st=St,wst=St,active=false});
handle_sculpt_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED}, %% Micheus
  #sculpt{st=#st{shapes=Shs}=St,mode=alpha_brush,id=ID}=Sc) ->
    SctMode = case wings_io:is_modkey_pressed(?KMOD_ALT) of
      true -> stamp;
      _ -> case wings_io:is_key_pressed(?SDLK_f) of
        true -> sculpt_plain;
      	_ -> sculpt
      	end
    end,
    {V0,PlainData}=case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        #we{vp=Vtab0}= We = gb_trees:get(Id, Shs),
        {Positions,_} = vpos(Face, We),
        Cnt=e3d_vec:average(Positions),
        Fn=wings_face:normal(Face,We),
        {X0,PlainData0}=case SctMode of 
          stamp -> {X-1,none};  %% force initial orientation for stamp mode
          sculpt_plain -> {X,{Vtab0,gb_trees:empty()}};
          _ -> {X,none}
        end,
        {#vl{v=scr2d_to_pnt3d(X0,Y,Cnt,Fn),vn=Fn},PlainData0};
      _ -> {none,none}
    end,
    do_sculpt(X,Y,Sc#sculpt{wst=St,st=St,active=true,alp_info={X,Y,SctMode,#seg_inf{va=V0},PlainData}});
handle_sculpt_event_1(#mousebutton{button=1,x=X,y=Y,state=?SDL_PRESSED},
  #sculpt{st=St}=Sc) ->
    do_sculpt(X, Y, Sc#sculpt{wst=St,active=true,alp_info={X,Y,sculpt,none,none}});
handle_sculpt_event_1(#mousebutton{button=3,mod=Mod,x=X,y=Y,state=?SDL_PRESSED}, Sc)
  when Mod band ?CTRL_BITS =:= 0 ->
    {GX,GY} = wings_wm:local2global(X, Y),
    adjust_str_wdt(GX, GY, Sc);
handle_sculpt_event_1(#mousebutton{button=3,mod=Mod,x=X,y=Y,state=?SDL_RELEASED}, Sc)
  when Mod band ?CTRL_BITS =/= 0 ->
    sculpt_menu(X, Y, Sc);
handle_sculpt_event_1(#keyboard{sym=Sym,mod=Mod,state=?SDL_PRESSED}=Ev, 
  #sculpt{alp_info={_,_,SctMode,_,_}}=Sc) when SctMode =/= stamp ->
	case is_altkey_magnet_event(Sym,Mod) of
	  true ->
	  	wings_io:change_event_handler(?SDL_KEYUP, ?SDL_ENABLE),
	  	{_,X,Y} = wings_wm:local_mouse_state(),
		{GX,GY} = wings_wm:local2global(X, Y),
		adjust_magnet(GX, GY, Sc);
      _ -> 
      	handle_key(Sym, Ev, Sc)
    end;
handle_sculpt_event_1({action,Action}, Sc) ->
    command_handling(Action, Sc);
handle_sculpt_event_1(got_focus, #sculpt{st=St}=Sc) ->
    Str = wings_pref:get_value(sculpt_strength),
    Wgt = wings_pref:get_value(alpha_brush_weight),
    wings_wm:dirty(),
    update_sculpt_handler(Sc#sculpt{id=none,st=St#st{selmode=face,sel=[],sh=false},active=false,str=Str,wgt=Wgt});
handle_sculpt_event_1(lost_focus, #sculpt{active=true}=Sc) ->
    #sculpt{st=St}=end_sculpt(Sc),
    wings_wm:current_state(St),
    wings_wm:send(geom,{update_state,St}),
    update_sculpt_handler(Sc#sculpt{id=none,st=St,wst=St,active=false});
handle_sculpt_event_1(_,_) ->
    keep.

%%%
%%% Adjust Magnet
%%%

adjust_magnet(X, Y, Sc) ->
    wings_io:grab(),
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
handle_magnet_event(#mousemotion{x=X, y=Y}, X0, Y0, Sc0) ->
    {GX,_} = wings_wm:local2global(X, Y),
    DX = GX-X0, %since last move X
    wings_io:warp(X0,Y0),
    Sc = adjust_magnet_radius(DX, Sc0),
    update_magnet_handler(X0, Y0, Sc);
handle_magnet_event(_, X, Y, #sculpt{str=Str}=Sc) ->
    end_magnet_event(X, Y, Str),
    update_sculpt_handler(Sc).

end_magnet_event(X, Y, Str) ->
	wings_io:change_event_handler(?SDL_KEYUP, ?SDL_IGNORE),
	wings_pref:set_value(sculpt_strength, Str),
    wings_io:ungrab(X, Y),
    wings_wm:dirty().

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
%%% Adjust Strength and Weight
%%%

adjust_str_wdt(X, Y, Sc) ->
    wings_io:grab(),
    update_strength_handler(X, Y, {X,Y}, Sc).

update_strength_handler(X, Y, Oc, Sc) ->
    wings_wm:dirty(),
    {replace,fun(Ev) ->
   	  	handle_strength_event(Ev, X, Y, Oc, Sc)
   	 end}.

handle_strength_event(redraw, X, Y, {XS,YS}=Oc, #sculpt{st=St}=Sc) ->
    wings_draw:refresh_dlists(St),
    wings:redraw("", St),
    help(Sc),
    draw_magnet(XS, YS, Sc),
    update_strength_handler(X, Y, Oc, Sc);
handle_strength_event(#mousebutton{button=1,state=?SDL_RELEASED}=Ev, X, Y, Oc, #sculpt{str=Str,wgt=Wgt}=Sc) ->
%% passing back the event just in case both buttons (3 and 1) are pressed and
%% the button 1 be released before 3 
	end_strength_event(X, Y, Oc, Str, Wgt),
	handle_sculpt_event_0(Ev, Sc);
handle_strength_event(#mousebutton{button=3,state=?SDL_RELEASED}, X, Y, Oc, #sculpt{str=Str,wgt=Wgt}=Sc) ->
	end_strength_event(X, Y, Oc, Str, Wgt),
    update_sculpt_handler(Sc);
handle_strength_event(#mousebutton{button=4,state=?SDL_RELEASED}, X, Y, Oc, Sc0) ->
    Sc = adjust_strength(1, Sc0),
    update_strength_handler(X, Y, Oc, Sc);
handle_strength_event(#mousebutton{button=5,state=?SDL_RELEASED}, X, Y, Oc, Sc0) ->
    Sc = adjust_strength(-1, Sc0),
    update_strength_handler(X, Y, Oc, Sc);
handle_strength_event(#mousemotion{x=X, y=Y, mod=Mod}, X0, _, Oc, #sculpt{mode=alpha_brush}=Sc0) 
  when Mod band ?ALT_BITS =:= 0; Mod band ?CTRL_BITS =:= 0 ->
    {GX,_} = wings_wm:local2global(X, Y),
    DX = GX-X0, %since last move X
    Sc = adjust_weight_value(DX, Sc0),
    update_strength_handler(X, Y, Oc, Sc);
handle_strength_event(lost_focus, X, Y, Oc, #sculpt{str=Str,wgt=Wgt}=Sc) ->
	end_strength_event(X, Y, Oc, Str, Wgt),
    update_sculpt_handler(Sc);
handle_strength_event(_, X, Y, Oc, Sc) ->
    update_strength_handler(X, Y, Oc, Sc).

end_strength_event(_, _, {X0, Y0}, Str, Wgt) ->
	wings_pref:set_value(sculpt_strength, Str),
    wings_pref:set_value(alpha_brush_weight, Wgt),
    wings_io:ungrab(X0, Y0),
    wings_wm:dirty().

adjust_strength(0, Sc) ->
    Sc;
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
%%% Adjust Weight
%%%
adjust_weight_value(0, Sc) ->
    Sc;
adjust_weight_value(MouseMovement, #sculpt{wgt=Wgt0}=Sc) when MouseMovement < 0 ->
    case Wgt0 - weight_increment() of
        Wgt when Wgt > 0.1 ->
            Sc#sculpt{wgt=Wgt};
        _otherwise -> Sc#sculpt{wgt=0.1}
    end;
adjust_weight_value(_, #sculpt{wgt=Wgt0}=Sc) ->
    Wgt = Wgt0 + weight_increment(),
    Sc#sculpt{wgt=Wgt}.

weight_increment() ->
    0.1.

%%%
%%% Draw Magnet
%%%

draw_magnet(X, Y, #sculpt{rad=Rad,str=Str,st=#st{shapes=Shs}=St}) ->
    {LX,LY} = wings_wm:global2local(X, Y),
    {Xm,Ym,Zm} = case wings_pick:raw_pick(LX, LY, St#st{selmode=face,sel=[],sh=false}) of
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
      UpdInfo -> update_sculpt(UpdInfo,Sc)  %% Micheus
    end.
update_sculpt(#sculpt{}=Sc,_) ->
    update_sculpt_handler(Sc);
update_sculpt({St,Id0},Sc) ->
    Id=update_sculpt_1({St,Id0},wings_pref:get_value(sculpt_initial)),
    update_sculpt_handler(Sc#sculpt{id=Id,st=St});
update_sculpt({St,Id0,{_,_,SctMode,_,_}=AlphInfo},#sculpt{st=St0}=Sc) ->
    Id=update_sculpt_1({St,Id0},true),
	St1 = if SctMode=:=stamp -> St0;
	  true -> St
	end,
	update_sculpt_handler(Sc#sculpt{id=Id,st=St1,alp_info=AlphInfo}).
update_sculpt_1({St,Id0},Confine) ->
        wings_draw:refresh_dlists(St),
        wings_wm:dirty(),
        case Confine of
            true -> Id0;
            false -> none
        end.

%% In alpha_brush mode, mouse movement is necessary to start sculpting
sculpt(X, Y, #sculpt{mode=alpha_brush,alp_info={X,Y,sculpt,_,_}}=Sc) -> Sc;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str,wgt=Wgt,rad=Rad,mag_type=MagType,mode=alpha_brush=Mode,
    locked=Locked,st=#st{shapes=Shs0}=St,alp_info={Xo,Yo,SctMode,
    SegInfo0,PlainData}}) when SctMode =/= stamp ->
%% Sculpt mode of sculpt brush - Micheus
	Smooth=wings_io:is_modkey_pressed(?SHIFT_BITS),
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        #we{pst=Pst,fs=Ftab,es=Etab}=We0 = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We0),
        Cnt=e3d_vec:average(Positions),
        Fn=wings_face:normal(Face,We0),
        Vc=scr2d_to_pnt3d(X,Y,Cnt,Fn), % current vertex (cursor pos)
		{FN,Vtab,VsDyn,PlainData0}= case Smooth of
			false ->
			  calc_sculpt(SctMode,Locked,Str,Wgt,Rad,Vc,Fn,SegInfo0,Mir,We0,PlainData);
			_ -> 
			  {gb_trees:empty(),smooth_magnetic(Locked,Str,Rad,Cnt,MagType,Mir,We0),[],PlainData}
		end,
		VsPairs= face2edges_col(gb_trees:keys(FN),VsDyn,Ftab,Etab,Vtab),
		#seg_inf{va=#vl{v=Va},vb=#vl{v=Vb}}=SegInfo=update_seg_inf(SegInfo0,Vc,Fn),
		Vo=e3d_vec:norm_sub(Vb,Va), % vertex orientation

		We=We0#we{pst=set_sculpt_info(Mode,[Vc,Vo,1.0,VsPairs,VsDyn],Pst),vp=Vtab},
		Shs = gb_trees:update(Id, We, Shs0),
		{St#st{shapes=Shs},Id,{Xo,Yo,SctMode,SegInfo,PlainData0}};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str,wgt=Wgt,rad=Rad,mode=alpha_brush=Mode,locked=Locked,
  st=#st{shapes=Shs0}=St,alp_info={Xo,Yo,stamp=SctMode,
  #seg_inf{va=Va0}=SegInfo0,PlainData}}) ->
%% stamp mode of sculpt brush
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
        #we{pst=Pst,fs=Ftab,es=Etab}=We0 = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We0),
        Cnt=e3d_vec:average(Positions),
        Fn=wings_face:normal(Face,We0),
        Vc=scr2d_to_pnt3d(X,Y,Cnt,Fn), % current vertex (cursor pos)
        SegInfo1=update_seg_inf(SegInfo0#seg_inf{vb=#vl{v=Va}=Va0},Vc,Fn),
        
        {FN,Vtab,VsDyn,_}=calc_sculpt(SctMode,Locked,Str*10,Wgt,Rad,Va,Fn,SegInfo1,Mir,We0,none),
        VsPairs= face2edges_col(gb_trees:keys(FN),VsDyn,Ftab,Etab,Vtab), %% less use of memory
        #seg_inf{vb=#vl{v=Vb}}=SegInfo=update_seg_inf(SegInfo0,Vc,Fn),
        #vl{v=Va}=Va0,
        Vo=e3d_vec:norm_sub(Vb,Va), % vertex orientation

        We=We0#we{pst=set_sculpt_info(Mode,[Va,Vo,Rad,VsPairs,VsDyn],Pst),vp=Vtab},
        Shs = gb_trees:update(Id, We, Shs0),
        {St#st{shapes=Shs},Id,{Xo,Yo,SctMode,SegInfo#seg_inf{va=Va0},PlainData}};
      _ ->
        keep
    end;
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
        We = gb_trees:get(Id, Shs1),
        {Positions,_} = vpos(Face, We),
        Cnt = e3d_vec:average(Positions),
        Vtab = smooth_magnetic(Locked,Str0,Rad,Cnt,MagType,Mir,We),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs1),
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
                        pinch(V, Pos, Cnt, 1, Str, Mir, We, Vtab1)
                end
            end, Vtab0, [Face], We),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,mir=Mir,str=Str,rad=Rad,mag_type=MagType,mode=pinch=Mode,
  locked=Locked,st=#st{shapes=Shs1}=St}) ->
%% Pinch mode with magnet
	Smooth=wings_io:is_modkey_pressed(?SHIFT_BITS),
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
		Shs0=check_is_same_id(Mode,Id,Shs1),
        #we{vp=Vtab0,pst=Pst}=We = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We),
        Cnt = {Cx,Cy,Cz} = e3d_vec:average(Positions),
        Lvs = lookup_locked_vs(Id, Locked),
		{Influenced,Vtab} = case Smooth of
		  true -> 
		  	{[],smooth_magnetic(Locked,Str,Rad,Cnt,MagType,Mir,We)};
          _ ->
			array:sparse_foldl(fun
				(V, Pos={Px,Py,Pz}, {Influenced0,Vtab1}) ->
					case ordsets:is_element(V, Lvs) of
						true -> {Influenced0,Vtab1};
						false ->
							case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
								true ->
									Dist = e3d_vec:dist(Pos, Cnt),
									case Dist =< Rad of
										true ->
											Inf = magnetic_influence(MagType, Dist, Rad),
											Influenced1=[Influenced0|[{V,Inf}]],
											{Influenced1,pinch(V, Pos, Cnt, Inf, Str, Mir, We, Vtab1)};
										false -> {Influenced0,Vtab1}
									end;
								false -> {Influenced0,Vtab1}
							end
						end
				   end, {[],Vtab0}, Vtab0)
		end,
		NewPst = set_sculpt_info(Mode,Influenced,Pst),
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
                         NewPos = handle_mirror(Id, V, NewPos0, Mir),
                         array:set(V, NewPos, Vtab1)
                 end
            end, Vtab0, Vpos),
        Shs = gb_trees:update(Id, We#we{vp=Vtab}, Shs0),
        {St#st{shapes=Shs},Id};
      _ ->
        keep
    end;
sculpt(X, Y, #sculpt{id=ID,locked=Locked,mir=Mir,str=Str,rad=Rad,mode=Mode,
  mag_type=MagType,st=#st{shapes=Shs1}=St}) ->
%% Push and Pull mode with magnet
    case wings_pick:raw_pick(X, Y, St#st{selmode=face,sel=[],sh=false}) of
      {_,_,{Id,Face}} when ID =:= none; Id =:= ID ->
		Shs0=check_is_same_id(Mode,Id,Shs1),
        #we{vp=Vtab0,pst=Pst}=We = gb_trees:get(Id, Shs0),
        {Positions,_} = vpos(Face, We),
        Cnt = {Cx,Cy,Cz} = e3d_vec:average(Positions),
        Lvs = lookup_locked_vs(Id, Locked),
        Smooth=wings_io:is_modkey_pressed(?SHIFT_BITS),
        {_,Influenced,Vtab} = case Smooth of
		  true -> 
		  	{none,[],smooth_magnetic(Locked,Str,Rad,Cnt,MagType,Mir,We)};
          _ ->
			array:sparse_foldl(fun
				(V, Pos={Px,Py,Pz}, {FNs0,Influenced0,Vtab1}) ->
					 case ordsets:is_element(V, Lvs) of
						 true -> {FNs0,Influenced0,Vtab1};
						 false ->
							 case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
								 true ->
									 Dist = e3d_vec:dist(Pos, Cnt),
									 case Dist =< Rad of
										 true ->
										   Inf = magnetic_influence(MagType, Dist, Rad),
										   Influenced1=[Influenced0|[{V,Inf}]],
										   {FNs,Normal0} = vertex_normal(V, We, FNs0),
										   Normal = case wings_io:is_modkey_pressed(?CTRL_BITS) of
											 false -> Normal0;
											 true -> e3d_vec:neg(Normal0)
										   end,
										   NewPos0 = e3d_vec:add_prod(Pos, Normal, Str*Inf),
										   NewPos = handle_mirror(Id, V, NewPos0, Mir),
										   {FNs,Influenced1,array:set(V, NewPos, Vtab1)};
										 false -> {FNs0,Influenced0,Vtab1}
									 end;
								 false -> {FNs0,Influenced0,Vtab1}
							 end
						 end
					end, {gb_trees:empty(),[],Vtab0}, Vtab0)
		end,
		NewPst = set_sculpt_info(Mode,Influenced,Pst),
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
        (V, Pos={Px,Py,Pz}, Vtab1) ->
            case ordsets:is_element(V, Lvs) of
                true -> Vtab1;
                false ->
                    case in_dist_boundaries([Cx,Cy,Cz], [Px,Py,Pz], Rad) of
                        true ->
                            Dist = e3d_vec:dist(Pos, Cnt),
                            case Dist =< Rad of
                                true ->
                                    Inf = magnetic_influence(MagType, Dist, Rad),
                                    smooth(V, Pos, Inf, Str, Mir, We, Vtab1);
                                false -> Vtab1
                            end;
                        false -> Vtab1
                    end
                end
           end, Vtab0, Vtab0).

stamp(#sculpt{id=Id,mir=Mir,str=Str,wgt=Wgt,rad=Rad,mode=alpha_brush,locked=Locked,
  st=#st{shapes=Shs0}=St,alp_info={_,_,_,#seg_inf{va=#vl{v=Va,vn=Fn}}=SegInfo0,_}}=Sc) ->
    We0=gb_trees:get(Id,Shs0),
    {_,Vtab,_,_}=calc_sculpt(stamp,Locked,Str*10,Wgt,Rad,Va,Fn,SegInfo0,Mir,We0,none),
    We=We0#we{vp=Vtab},
    Shs=gb_trees:update(Id,We,Shs0),
    Sc#sculpt{st=St#st{shapes=Shs}}.

calc_sculpt(SctMode,Locked,Str,Wgt,Rad,Vc,Fn,SegInfo0,Mir,#we{id=Id,vp=Vtab0}=We0,none) ->
    Mul = if SctMode=:=stamp -> Wgt;
      true -> Str*Wgt
    end,
    Lvs = lookup_locked_vs(Id, Locked),
    Ori=orient_vec(SegInfo0#seg_inf{vc=#vl{v=Vc,vn=Fn}}),
    [{weight,{_,_,MaxX,MaxY,_,_}}]= AlpWgt= ets:lookup(alpha_brush,weight),
    {RFN,RVtab,RVsDyn} = array:sparse_foldl(fun
        (V, Pos, {FNs0,Vtab1,VsDyn}=Acc) ->
           case ordsets:is_element(V, Lvs) of
             true -> Acc;
             false ->
               case in_radius_boundaries(Vc,Pos,Rad) of
                 {true,Pos0} ->
                    case in_degree_boundaries(Fn,Pos0) of
                      {true,Deg} ->
                        case in_image_boundaries(Fn,Ori,Pos0,Deg,Rad,MaxX,MaxY) of
                          {true, ImgX, ImgY} ->
                            {FNs,Normal0} = vertex_normal(V, We0, FNs0),
                            Inf = alpha_influence(alpha_brush, AlpWgt, ImgX, ImgY),
                            Normal = case wings_io:is_modkey_pressed(?CTRL_BITS) of
                              false -> Normal0;
                              true -> e3d_vec:neg(Normal0)
                            end,
                            NewPos0 = e3d_vec:add_prod(Pos, Normal, Mul*Inf),
                            NewPos = handle_mirror(Id, V, NewPos0, Mir),
                            {FNs,array:set(V, NewPos, Vtab1),VsDyn++[{V,Inf}]}; 
                          {false, _, _} -> 
                            Acc
                        end;
                      {false,_} -> Acc
                    end;
                 {false,_} -> Acc
               end
           end
        end, {gb_trees:empty(),Vtab0,[]}, Vtab0),
    {RFN,RVtab,RVsDyn,none};
calc_sculpt(SctMode,Locked,Str,Wgt,Rad,Vc,Fn,SegInfo0,Mir,#we{id=Id,vp=Vtab0}=We0,{OrigVtab,VsInf}) ->
    Mul = if SctMode=:=stamp -> Wgt;
      true -> Str*Wgt
    end,
    Lvs = lookup_locked_vs(Id, Locked),
    Ori=orient_vec(SegInfo0#seg_inf{vc=#vl{v=Vc,vn=Fn}}),
    [{weight,{_,_,MaxX,MaxY,_,_}}]= AlpWgt= ets:lookup(alpha_brush,weight),
    {RFN,RVtab,RVsDyn,RVsInf} = array:sparse_foldl(fun
        (V, Pos, {FNs0,Vtab1,VsDyn,VsInf0}=Acc) ->
           case ordsets:is_element(V, Lvs) of
             true -> Acc;
             false ->
               case in_radius_boundaries(Vc,Pos,Rad) of
                 {true,Pos0} ->
                    case in_degree_boundaries(Fn,Pos0) of
                      {true,Deg} ->
                        case in_image_boundaries(Fn,Ori,Pos0,Deg,Rad,MaxX,MaxY) of
                          {true, ImgX, ImgY} ->
                            {FNs,Normal0} = vertex_normal(V, We0#we{vp=OrigVtab}, FNs0),
                            Inf = alpha_influence(alpha_brush, AlpWgt, ImgX, ImgY),
                            case Inf of
                            0.0 -> 
                            	{FNs,Vtab1,VsDyn++[{V,Inf}],VsInf0};
                            _ ->
								Inf0 = case wings_io:is_modkey_pressed(?CTRL_BITS) of
								  false -> +1.0*Inf;
								  true -> -1.0*Inf
								end,
								{Inf2,Pos1}= case gb_trees:lookup(V, VsInf0) of
								  none -> 
									{Inf0,Pos};
								  {value, Inf1} ->
									{choose_influence(Inf1,Inf0),array:get(V, OrigVtab)}
								end,
								NewPos0 = e3d_vec:add_prod(Pos1, Normal0, Mul*Inf2),
								NewPos = handle_mirror(Id, V, NewPos0, Mir),
								VsInf2=gb_trees:enter(V, Inf2, VsInf0),
								{FNs,array:set(V, NewPos, Vtab1),VsDyn++[{V,Inf}],VsInf2}
							end;
                          {false, _, _} -> 
                            Acc
                        end;
                      {false,_} -> Acc
                    end;
                 {false,_} -> Acc
               end
           end
        end, {gb_trees:empty(),Vtab0,[],VsInf}, Vtab0),
        {RFN,RVtab,RVsDyn,{OrigVtab,RVsInf}}.

choose_influence(OldInf,NewInf) when OldInf > 0.0, NewInf >= 0.0 ->
  if OldInf >= NewInf -> OldInf;
  	true -> NewInf
  end;
choose_influence(OldInf,NewInf) when OldInf < 0.0, NewInf =< 0.0 ->
  if OldInf < NewInf -> OldInf;
  	true -> NewInf
  end;
choose_influence(_,NewInf) -> NewInf.

in_radius_boundaries(Vref, V, Rad) -> 
    V0=e3d_vec:sub(V, Vref),  % V translated to Vref
    {e3d_vec:len(V0)=<Rad, V0}.

in_degree_boundaries(VrN, V) ->
    Deg=e3d_vec:degrees(VrN,V),
    {abs(Deg-90.0) =< 45.0, Deg}.

in_image_boundaries(VrN,Vo,V,Deg0,Rad,MaxX,MaxY) ->
    Elev=Deg0-90.0,
    Vlen0=e3d_vec:len(V),
    Op=math:sin(Elev/?RAD2DEGREE)*Vlen0,
    Vp=e3d_vec:add(V,e3d_vec:mul(VrN,Op)),

    Azmt=e3d_vec:degrees(Vo,Vp),
    Deg1=case e3d_vec:cross(Vo,Vp) of
     {_,Ysgn,_} when Ysgn < 0.0 -> -Azmt+180.0;
     _ -> Azmt+180.0
    end,
    Vlen1=e3d_vec:len(Vp),
    Y=-math:sin(Deg1*?DEGREE2RAD)*Vlen1,
    X=-math:cos(Deg1*?DEGREE2RAD)*Vlen1,

    MaxXf=MaxX*1.0,
    MaxYf=MaxY*1.0,
    Rad0=e3d_vec:len({MaxXf,MaxYf,0.0}),
    S=Rad0/Rad,

    X0=X*S,
    Y0=Y*S,
    case {(abs(X0) =< MaxXf),(abs(Y0) =< MaxYf)} of
      {true,true} -> {true, X0, Y0}; 
      {_,_} -> {false, X0, Y0}
    end.

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
magnetic_influence(absolute, _, _) -> 1.

alpha_influence(alpha_brush, [{weight,{W,_,_,_,ZeroOfs,AlpInf}}]=_AlpWgt,X,Y) ->
	X0=trunc(X),
	Y0=trunc(Y),
    Idx=ZeroOfs+(W*-Y0)+X0,
    element(Idx,AlpInf).

smooth(V, Pos, Inf, Str, Mir, #we{id=Id}=We, Vtab) ->
    Positions = wings_vertex:fold(fun(_, _, E, Acc) ->
                 OtherPos = wings_vertex:other_pos(V, E, We),
                 [OtherPos|Acc]
        end, [], V, We),
    Avg = e3d_vec:average(Positions),
    Vec = e3d_vec:sub(Avg, Pos),
    NewPos0 = e3d_vec:add_prod(Pos, Vec, Str*Inf),
    NewPos = handle_mirror(Id, V, NewPos0, Mir),
    array:set(V, NewPos, Vtab).

pinch(V, Pos, Cnt, Inf, Str, Mir, #we{id=Id}, Vtab) ->
    Vec = case wings_io:is_modkey_pressed(?CTRL_BITS) of
          false -> e3d_vec:sub(Cnt, Pos);
          true -> e3d_vec:sub(Pos, Cnt)
    end,
    NewPos0 = e3d_vec:add_prod(Pos, Vec, Str*Inf),
    NewPos = handle_mirror(Id, V, NewPos0, Mir),
    array:set(V, NewPos, Vtab).

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
      {sculpt,Mode} when Mode =:= pull; Mode =:= pinch; Mode =:= smooth; Mode =:= alpha_brush ->
          prepare_alpha_data(Mode),
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
      {sculpt,exit_sculpt} ->
          exit_sculpt(Sc);
      _ -> keep
    end.

%%%
%%% Exit Sculpt
%%%

exit_sculpt(#sculpt{mag=Mag,mag_type=MagType,str=Str,wgt=Wgt,rad=Rad,mode=Mode,ost=St0}=Sc) ->
    wings_pref:set_value(sculpt_mode, Mode),
    wings_pref:set_value(sculpt_magnet, {Mag,Rad}),
    wings_pref:set_value(sculpt_strength, Str),
    wings_pref:set_value(sculpt_magnet_type, MagType),
    wings_pref:set_value(alpha_brush_weight, Wgt),
    wings_tweak:toggle_draw(true),
    #sculpt{st=#st{shapes=Shs}}=remove_plugin(Sc),
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_wm:later({new_state,St}),
    pop.

%%%
%%% Info Line
%%%

help(#sculpt{mag=Mag,rad=Rad,mag_type=MagType,str=Str,wgt=Wgt,mode=Mode,alp_info={_,_,SctMode,_,_}}) ->
    ModeMsg =
        case Mode of
            alpha_brush ->
              Alpha = mode(alpha_brush),
              Alpha++" "++?__(13,"(Hold [Ctrl]: Invert  [Shift]: Smooth)");
            pinch ->
              Pinch = mode(pinch),
              Pinch++" "++?__(12,"(Hold [Ctrl]: Inflate)");
            smooth -> mode(smooth);
            _ ->
              Pull = mode(pull),
              Pull++" "++?__(11,"(Hold [Ctrl]: Push)")
        end,
    Sculpt = ?__(1,"L: Sculpt"),
    Sculpt2 = if Mode=:=alpha_brush -> ?__(14,"[F]+L: Flat Sculpt  [Alt]+LL: Stamp Mode");
      true -> ""
    end,
    Menu = ?__(2,"[Ctrl]+R: Sculpt Menu"),
    MagnetType = io_lib:format(?__(3,"Magnet: ~s"),[magtype(MagType)]),
    Radius = ?__(4,"Alt+Drag: Adjust Radius"),
    Strength = if SctMode =/= stamp ->
   		?__(5,"R+Scroll(+[Shift]): ")  ++ ?__(6,"Adjust Strength");
   	  true -> ""
   	end,
    Exit = "[Esc]: " ++ exit_string(),
    
	{Text,Values} = case Mode of
		alpha_brush ->
			Weight = ?__(16,"R+Drag: ")  ++ ?__(17,"Adjust Weight"),
			{?__(15,"Max. Weight: ~s")++"  "++?__(7,"Strength: ~p") ++
			 if SctMode=:=stamp -> "~s";
			 true -> "  "++?__(8,"Radius: ~s")
			 end,
			 [wings_util:nice_float(Wgt),trunc(Str/0.001),
			  if SctMode=:=stamp -> "";
			    true -> wings_util:nice_float(Rad)
			  end]
			};
		_ ->
			Weight = "",
			{?__(7,"Strength: ~p")++"  "++?__(8,"Radius: ~s"),
			[trunc(Str/0.001),
			 if Mag -> wings_util:nice_float(Rad) ++ "  " ++ MagnetType;
			   true -> ?__(9,"None (Magnet is off)")
			 end]}
	end,
    StatusBar = io_lib:format(Text,Values),
    {_,H} = wings_wm:win_size(),
    LLine = wings_msg:join([?__(10,"Sculpt Mode")++": "++ModeMsg, StatusBar]),
    wings_io:info(0, H-?LINE_HEIGHT-3, LLine),
    wings_wm:message(wings_msg:join([Sculpt,Sculpt2,Menu,Exit]),
                     wings_msg:join([Weight,Radius,Strength])).

magtype(bell) -> ?__(1,"Bell");
magtype(dome) -> ?__(2,"Dome");
magtype(straight) -> ?__(3,"Straight");
magtype(spike) -> ?__(4,"Spike");
magtype(absolute) -> ?__(5,"Absolute").

mode(push) -> ?__(1,"Push");
mode(pull) -> ?__(2,"Pull");
mode(pinch) -> ?__(3,"Pinch");
mode(smooth) -> ?__(4,"Smooth");
mode(inflate) -> ?__(5,"Inflate");
mode(alpha_brush) -> ?__(6,"Alpha Brush"). % Micheus

exit_string() -> ?__(1,"Exit").

%%%
%%% Sculpt Dialog
%%%

prefs(#sculpt{str=Str}) ->
    Strength = trunc(Str/0.001),
    Confine = wings_pref:get_value(sculpt_initial),
    AlphaFile = wings_pref:get_value(sculpt_alpha_brush),
	BrowseProps = [ {dialog_type, open_dialog},
	        {filename, AlphaFile},
	        {extensions, [{".png","PNG"}, {".tga", "TGA"}, {".jpg","JPEG"}, {".bmp", "Bitmap"}]}],
    Menu = [{vframe,
      [{hframe,[{slider,{text,Strength,[{key,sculpt_strength},{range,{1,100}}]}}],
         [{title,?__(1,"Strength")}]},
       {vframe,[{button,{text,AlphaFile,[{key,sculpt_alpha_brush},{props,BrowseProps}]}},
                {hframe,[
                    {label,?__(2,"Max. Weight")},
                    {text,wings_pref:get_value(alpha_brush_weight),[{key,alpha_brush_weight},{range,{0.001,infinity}}]}
                ]}],
         [{title,?__(3,"Alpha settings")}]}
         ]}],
    C = [separator,{?__(4,"Confine sculpt to initial object"),
          Confine,[{key,sculpt_initial}]}],
    PrefQs = [{Lbl, make_query(Ps)} || {Lbl, Ps} <- Menu] ++ C,
    wings_ask:dialog(?__(5,"Sculpt Preferences"), PrefQs,
    fun(Result) -> set_values(Result) end).

make_query([_|_]=List) ->
    [make_query(El) || El <- List];
make_query({color,[_|_]=Str,Key}) ->
    Def = wings_pref:get_value(Key),
    {Str,{color,Def,[{key,Key}]}};
make_query(Other) -> Other.

set_values([{sculpt_alpha_brush=Key, Value}|Result]) ->
    case wings_pref:get_value(Key) of
    Value -> ok;
    _-> 
        wings_pref:set_value(Key, Value),
        load_alpha_image(Value)
    end,
    set_values(Result);
set_values([{alpha_brush_weight=Key, Value}|Result]) ->
    wings_pref:set_value(Key, Value),
    set_values(Result);
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
    {X,Y} =  wings_wm:local2global(X0, Y0),
    Menu = sculpt_menu(Sc),
    wings_menu:popup_menu(X, Y, sculpt, Menu).

sculpt_menu(#sculpt{mag=Mag,mag_type=MagType,mode=Mode}) ->
    [{mode(push)++"/"++mode(pull),pull,crossmark(Mode, pull)},
     {mode(pinch)++"/"++mode(inflate),pinch,crossmark(Mode, pinch)},
     {mode(smooth),smooth,crossmark(Mode, smooth)},
     {mode(alpha_brush),alpha_brush,crossmark(Mode, alpha_brush)},
     separator,
     {magtype(dome),dome,crossmark(MagType, dome)},
     {magtype(absolute),absolute,crossmark(MagType, absolute)},
     {magtype(straight),straight,crossmark(MagType, straight)},
     {magtype(spike),spike,crossmark(MagType, spike)},
     {magtype(bell),bell,crossmark(MagType, bell)},
     separator,
     {?__(1,"Magnet On/Off"),magnet,crossmark(Mag, true)},
     {?__(2,"Magnet Mask On/Off"),mask_toggle,
         wings_menu_util:crossmark(magnet_mask_on)},
     separator,
     {?__(3,"Preferences"),prefs},
     separator,
     {exit_string(),exit_sculpt}].

%%
%% Implementation of a visual feedback for sculpting - highlighting the vertice influence
%% Added by Micheus - 2011/2012
%%
%% it updates the orientation vector for alpha brush mode
update_seg_inf(#seg_inf{va=_,vb=none,vc=_}=AlphaInfo,Vc,Vcn) ->
    AlphaInfo#seg_inf{vb=#vl{v=Vc,vn=Vcn},vc=none};
update_seg_inf(#seg_inf{va=_,vb=Vb,vc=_}=AlphaInfo,Vc,Vcn) ->
    AlphaInfo#seg_inf{va=Vb,vb=#vl{v=Vc,vn=Vcn},vc=none}.

%% Adding plugin state information to #we's pst. If ScptInfo list is 
%% empty, then remove pst from the #we.
set_sculpt_info(alpha_brush,[],Pst) -> remove_pst(brush_info,Pst);
set_sculpt_info(_,[],Pst) -> remove_pst(hl_info,Pst);
set_sculpt_info(alpha_brush,ScptInfo,Pst) ->
	add_pst(brush_info,ScptInfo,Pst);
set_sculpt_info(_,ScptInfo0,Pst) ->
	ScptInfo=lists:flatten(ScptInfo0),
	add_pst(hl_info,ScptInfo,Pst).

add_pst(InfoType,ScptInfo,Pst) ->
    case gb_trees:lookup(?MODULE, Pst) of
      none ->
          Data = gb_trees:empty(),
          NewData = gb_trees:insert(InfoType,ScptInfo,Data),
          gb_trees:insert(?MODULE,NewData,Pst);
      {_,Data} ->
          NewData = gb_trees:enter(InfoType,ScptInfo,Data),
          gb_trees:update(?MODULE,NewData,Pst)
    end.

remove_pst(InfoType, Pst) ->
	case gb_trees:lookup(?MODULE, Pst) of
	  none -> Pst;
	  {_,Data} ->
	  	  NewData = gb_trees:delete_any(InfoType,Data),
		  gb_trees:update(?MODULE,NewData,Pst)
	end.

%% updating St record before leave sculpt
end_sculpt(Sc) ->
    #sculpt{st=#st{shapes=Shs},wst=St0}=Sc0=clear_sculpt_info(Sc),
    St = wings_undo:save(St0, St0#st{shapes=Shs}),
    wings_draw:refresh_dlists(St),
    Sc0#sculpt{id=none,st=St,wst=St,active=false}.

%% Removing plugin state information from #we's pst
clear_sculpt_info(#sculpt{mode=Mode,id=none,st=#st{shapes=Shs0}=St}=Sc) ->
	Sc#sculpt{st=St#st{shapes=clear_sculpt_info(Mode,Shs0)},alp_info={0,0,sculpt,#seg_inf{va=none,vb=none,vc=none},none}};
clear_sculpt_info(#sculpt{mode=Mode,id=Id,st=#st{shapes=Shs0}=St}=Sc) ->
	#we{pst=Pst}=We= gb_trees:get(Id, Shs0),
	Shs = gb_trees:update(Id, We#we{pst=set_sculpt_info(Mode,[],Pst)}, Shs0),
	Sc#sculpt{st=St#st{shapes=Shs},alp_info={0,0,sculpt,#seg_inf{va=none,vb=none,vc=none},none}};
clear_sculpt_info(Sc) -> 
	Sc#sculpt{alp_info={0,0,sculpt,#seg_inf{va=none,vb=none,vc=none},none}}.

clear_sculpt_info(Mode,Shs) ->
    Shs1 = lists:map(fun
            (#we{id=Id,pst=none}=We) -> {Id,We};
            (#we{id=Id,pst=Pst}=We) ->
            	NewPst=set_sculpt_info(Mode,[],Pst),
            	{Id,We#we{pst=NewPst}}
          end,gb_trees:values(Shs)),
    gb_trees:from_orddict(Shs1).

% It removes the plugin functionality
remove_plugin(#sculpt{id=none,st=#st{shapes=Shs0}=St}=Sc) ->
	wings_pref:delete_value(sculpt_current_id),
    Shs1 = lists:map(fun
            (#we{id=Id,pst=none}=We) -> {Id,We};
            (#we{id=Id,pst=Pst}=We) ->
            	NewPst=gb_trees:delete_any(?MODULE, Pst),
              {Id,We#we{pst=NewPst}}
          end,gb_trees:values(Shs0)),
	unload_alpha_data(),
    ets:delete(alpha_brush),    
    Sc#sculpt{st=St#st{shapes=gb_trees:from_orddict(Shs1)}}.

% It's used to reset the highlighting when a new we# got the focus
check_is_same_id(Mode,IdNew,Shs) ->
	OldId=wings_pref:get_value(sculpt_current_id),
	wings_pref:set_value(sculpt_current_id,IdNew),
	case OldId of
	none -> Shs;
	IdNew -> Shs;
	ID ->
		#we{pst=Pst}=We0 = gb_trees:get(ID, Shs),
		gb_trees:update(ID,We0#we{pst=set_sculpt_info(Mode,[],Pst)},Shs)
	end.

%%
%% Preparing dlist for painting the alpha brush data
%% Functions to produce the visual effect (inspired on wpc_magnet_mask.erl file)
%% It generate the OpenGl list of colored vertices
update_dlist({hl_info,HlInfo},#dlo{plugins=Pdl,src_we=#we{vp=Vtab}}=D, _) ->
    Key = ?MODULE,
    Str=wings_pref:get_value(sculpt_strength),
    ColFac=Str*10,  % range of 0..1 
    Col={1.0*ColFac,0.0*ColFac,1.0},
    Pos = positions(HlInfo,Vtab,[]),
    case Pos of
      [] ->
        D#dlo{plugins=[{Key,none}|Pdl]};
      _ ->
        List = gl:genLists(1),
        gl:newList(List,?GL_COMPILE),
        gl:'begin'(?GL_POINTS),
        pump_vertices(Col,Pos),
        gl:'end'(),
        gl:endList(),
        D#dlo{plugins=[{Key,{vs,List}}|Pdl]}
    end;
%% It generate the OpenGl list of colored edges
update_dlist({brush_info,ScptInfo},#dlo{plugins=Pdl,src_we=#we{vp=Vtab}}=D,_) ->
    Key = ?MODULE,
    [Vp,Vo,Size,EdgeLst,VsList]=ScptInfo,
    D1=D,
    case ScptInfo of
      [] ->
        D1#dlo{plugins=[{Key,none}|Pdl]};
      _ ->
      	if EdgeLst =/= [] ->
      		Pos = positions(VsList,Vtab,[]),
			Str=wings_pref:get_value(sculpt_strength),
			ColFac=Str*10, % range of 0..1
			Col={1.0*ColFac,0.0*ColFac,1.0},

			List = gl:genLists(1),
			gl:newList(List,?GL_COMPILE),
			gl:'begin'(?GL_LINES),
			pump_vertices(?ALPHA_THUMB_COLOUR,build_vector(Vp,Vo,Size)),
			pump_vertices(EdgeLst),
			gl:'end'(),
			gl:endList(),
			
			List2 = gl:genLists(1),
			gl:newList(List2,?GL_COMPILE),
			gl:'begin'(?GL_POINTS),
			pump_vertices(Col,Pos),
			gl:'end'(),
			gl:endList(),
			D1#dlo{plugins=[{Key,{vs,List,List2}}|Pdl]};
		true -> D1#dlo{plugins=[{Key,none}|Pdl]}
		end
    end.

%% pumping Lines
pump_vertices([]) -> ok;
pump_vertices([{V1,{R1,G1,B1},V2,{R2,G2,B2}}|SegInf]) ->
    gl:color3f(R1,G1,B1),
    gl:vertex3fv(V1),
    gl:color3f(R2,G2,B2),
    gl:vertex3fv(V2),
    pump_vertices(SegInf).
    
%% pumping vertices
pump_vertices({R,G,B}=Col,[{V,Inf}|Vs]) ->
    gl:color3f(R*Inf,G*Inf,B*Inf),
    gl:vertex3fv(V),
    pump_vertices(Col,Vs);
pump_vertices({R,G,B}=Col,[V|Vs]) ->
    gl:color3f(R,G,B),
    gl:vertex3fv(V),
    pump_vertices(Col,Vs);
pump_vertices(_,[]) -> ok.

positions([{V,Inf}|Influenced],Vtab,Acc) ->
    Pos = {array:get(V,Vtab),Inf},
    positions(Influenced,Vtab,[Pos|Acc]);
positions([],_,Acc) -> Acc.

build_vector(Vp,Vn,Size) ->
    Vnf=e3d_vec:add(Vp,e3d_vec:mul(Vn,Size)),
    [Vp,Vnf].

% It'll will provide de vertices data for 'update_dlist' function
get_data(update_dlist, Data, Acc) ->
	case gb_trees:lookup(hl_info, Data) of
	none ->
		case gb_trees:lookup(brush_info, Data) of
			none ->
			  {ok, Acc};
			{_,ScptInfo} ->
			  {ok, [{plugin, {?MODULE, {brush_info, ScptInfo}}}|Acc]}
		end;
	{_,HlInfo} ->
	    {ok, [{plugin, {?MODULE, {hl_info, HlInfo}}}|Acc]}
	end.

% It'll use the list prepared by 'update_dlist' function and then draw it (for plain or smooth flag)
draw(_, {vs,List}, _D, Selmode) ->
    PtSize = wings_pref:get_value(masked_vertex_size),
    Size = PtSize*0.6,
    gl:pointSize(vert_display(Size,Selmode)),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_dl:call(List),
    gl:disable(?GL_BLEND);
draw(_, {vs,List,List2}, _D, Selmode) ->
    PtSize = wings_pref:get_value(masked_vertex_size),
    Size = PtSize*0.08,
    Size2 = PtSize*0.45,
    gl:lineWidth(vert_display(Size,Selmode)),
    gl:pointSize(vert_display(Size2,Selmode)),
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    wings_dl:call(List),
    wings_dl:call(List2),
    gl:disable(?GL_BLEND);
draw(_,_,_,_) -> ok.

vert_display(Size,vertex) ->
    VSize = wings_pref:get_value(selected_vertex_size),
    case VSize >= Size of
      true -> VSize + 0.1;
      false -> Size
    end;
vert_display(Size,_Selmode) -> Size.

crossmark(Key, Key) ->
    [crossmark];
crossmark(_, _) ->
    [].

%%
%% Drawing the Alpha Brush thumbnail
draw_brush_icon(alpha_brush) ->
	case {ets:lookup(alpha_brush,uv_map),ets:lookup(alpha_brush,image)} of
		{[{uv_map,TxId}],[{image,Image}]} ->
			#e3d_image{width=W0,height=H0}=Image,
			H=?ALPHA_THUMB_SIZE,
			Scl=H/H0,
			W=trunc(W0*Scl),
			draw_thumbnail(2,2,W,H,TxId);
		{_,_} -> ok
	end;
draw_brush_icon(_) -> ok.

draw_thumbnail(X, Y, W, H, TxId) ->
    gl:enable(?GL_TEXTURE_2D),
	wings_image:draw_image(X, Y+H, W, -H, TxId),
    gl:disable(?GL_TEXTURE_2D),

    {Wm,Hm} = wings_wm:win_size(),
    {R,G,B}=?ALPHA_THUMB_COLOUR,
    gl:color3f(R,G,B),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:ortho(0,Wm,Hm,0,0.0,100.0),
    gl:lineWidth(2),
	gl:'begin'(?GL_LINES),
    gl:vertex2f(X+(W/2),Y+(H/2)),
    gl:vertex2f(X+W+1,Y+(H/2)),
	gl:'end'(),
	gl:popMatrix().


%% Support functions for AlphaBrush resource
%%
%% It computes the 3d position for a point by projecting 2d screen   
%% coordenate in the plane defined by the vertice (V) and its normal (Vn)
%% using the current space transformation. (used by Sculpt Brush code)
scr2d_to_pnt3d(X0,Y0,V,Vn) ->
    {W,H} = wings_wm:win_size(),
    Wc=trunc((W+1)/2),
    Hc=trunc((H+1)/2),
    X=adjust_offset((X0-Wc)/Wc),
    Y=adjust_offset(((H-Y0+1)-Hc)/Hc),
    
    PMi = get_proj_matrix_inv(),
    
    {Xa,Ya,Za,Sa}=e3d_mat:mul(PMi,{X,Y,-1.0,1.0}),
    PosA=e3d_vec:mul({Xa,Ya,Za},1.0/Sa),
    {Xb,Yb,Zb,Sb}=e3d_mat:mul(PMi,{X,Y,0.0,1.0}),
    PosB=e3d_vec:mul({Xb,Yb,Zb},1.0/Sb),
    
    Dir=e3d_vec:norm(e3d_vec:sub(PosB,PosA)),
    
    case e3d_vec:dot(Dir,Vn) of
    0.0 ->
        Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn),
        e3d_vec:add(PosB, e3d_vec:mul(Vn, Intersection));
      Dot ->
        Intersection = e3d_vec:dot(e3d_vec:sub(V,PosB), Vn) / Dot,
        e3d_vec:add(PosB, e3d_vec:mul(Dir, Intersection))
    end.

adjust_offset(N) when N < 0.0 -> N+1.0; 
adjust_offset(N) -> N. 

%% computes the inverse projection matrix for the current space transformation.
get_proj_matrix_inv() ->
    ModelMatrix = list_to_tuple(gl:getDoublev(?GL_MODELVIEW_MATRIX)),
    ProjMatrix = list_to_tuple(gl:getDoublev(?GL_PROJECTION_MATRIX)),
    e3d_mat:invert(e3d_mat:mul(ProjMatrix, ModelMatrix)).

%% build the orientation vector
orient_vec(#seg_inf{va=Vla,vb=none,vc=Vlc}) ->
    orient_vec_1(Vla,Vlc);
orient_vec(#seg_inf{va=_,vb=Vlb,vc=Vlc}) ->
    orient_vec_1(Vlb,Vlc).
orient_vec_1(#vl{v=Va},#vl{v=Vb}) ->
    e3d_vec:norm_sub(Vb,Va).

face2edges_col(Faces, VsDyn, Ftab, Etab, Vtab) ->
    to_edges_raw(Faces, VsDyn, Ftab, Etab, Vtab, []).

to_edges_raw([Face|Faces], VsDyn, Ftab, Etab, Vtab, Acc0) ->
    Edge = gb_trees:get(Face, Ftab),
    Acc = to_edges_raw_1(Edge, VsDyn, Etab, Vtab, Acc0, Face, Edge, not_done),
    to_edges_raw(Faces, VsDyn, Ftab, Etab, Vtab, Acc);
to_edges_raw([],_ , _, _, _, Acc) -> Acc.

to_edges_raw_1(LastEdge, _, _, _, Acc, _, LastEdge, done) -> Acc;
to_edges_raw_1(Edge, VsDyn, Etab, Vtab, Acc, Face, LastEdge, _) ->
    case array:get(Edge, Etab) of
	#edge{vs=Va0,ve=Vb0,lf=Face,ltsu=NextEdge} ->
		Cola=get_vs_color(Va0, VsDyn),
		Colb=get_vs_color(Vb0, VsDyn),
        VsPair=[{array:get(Va0, Vtab),Cola, array:get(Vb0, Vtab),Colb}],
	    to_edges_raw_1(NextEdge, VsDyn, Etab, Vtab, VsPair++Acc, Face, LastEdge, done);
	#edge{vs=Va0,ve=Vb0,rf=Face,rtsu=NextEdge} ->
		Cola=get_vs_color(Va0, VsDyn),
		Colb=get_vs_color(Vb0, VsDyn),
        VsPair=[{array:get(Va0, Vtab),Cola, array:get(Vb0, Vtab),Colb}],
	    to_edges_raw_1(NextEdge, VsDyn, Etab, Vtab, VsPair++Acc, Face, LastEdge, done)
    end.

get_vs_color(V, VsDyn) ->
    case lists:keysearch(V, 1, VsDyn) of
        false -> {0.0,0.0,0.0};
        {_, {_,Value}} ->
        	{1.0*Value,1.0*Value,1.0*Value}
    end.


