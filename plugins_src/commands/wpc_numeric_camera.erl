%%
%%  wpc_numeric_camera.erl --
%%
%%    Plugin to change the camera postion numerically, including a preview
%%    mode.
%%
%%  Copyright (c) 2009 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_numeric_camera).
-export([init/0,menu/2,command/2]).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-record(nc,
    {cur_view,
     other,     %% Other view
     st}).

init() ->
    wings_pref:set_default(cam_pos_specify_dist,false),
    wings_pref:set_default(cam_pos_preview,true),
    true.

menu({view},Menu) ->
    lists:reverse(parse(Menu, [], false));
menu(_,Menu) -> Menu.

parse([], NewMenu, true) ->
    NewMenu;
parse([], NewMenu, false) ->
    [menu_entry(), separator|NewMenu];
parse([A = {_,{along,_}}|Rest], NewMenu, false) ->
    parse(Rest, [A,menu_entry()|NewMenu], true);
parse([Elem|Rest], NewMenu, Found) ->
    parse(Rest, [Elem|NewMenu], Found).

menu_entry() ->
    {?__(1,"Numeric Camera Position"),camera_position,
     ?__(2,"Set the position of the camera numerically.")}.

command({view,camera_position},_) ->
    set_camera_position();
command({view,{camera_position,Res}},St) ->
    start_nc_mode(Res,St);
command(_,_) -> next.

%%%% Set Position of Camera Numerically
set_camera_position() ->
    View = wings_view:current(),
    #view{origin=Origin,distance=Dist,pan_x=PanX,pan_y=PanY} = View,
    {OriX,OriY,OriZ} = e3d_vec:neg(Origin),
    {CamX,CamY,CamZ} = wings_view:eye_point(),
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    CamDist = wings_pref:get_value(cam_pos_specify_dist),
    Preview = wings_pref:get_value(cam_pos_preview),
    DistHook =
    fun (is_disabled, {_Var,_I,Sto}) ->
           not gb_trees:get(cam_pos_specify_dist, Sto);
        (_, _) -> void
    end,
    Qs =
      [{hframe,[{label,[{bold,?__(2,"Camera Position:")}]},
                {label,io_lib:format("{~s ~s ~s}",
                 [wings_util:nice_float(CamX),
                  wings_util:nice_float(CamY),
                  wings_util:nice_float(CamZ)])}]},
       {hframe,[{label,X},{text,CamX},
                {label,Y},{text,CamY},
                {label,Z},{text,CamZ}]},
      separator,
       {hframe,[{label,[{bold,?__(3,"Focal Point:")}]},
                {label,io_lib:format("{~s ~s ~s}",
                 [wings_util:nice_float(OriX),
                  wings_util:nice_float(OriY),
                  wings_util:nice_float(OriZ)])}]},
       {hframe,[{label,X},{text,OriX},
                {label,Y},{text,OriY},
                {label,Z},{text,OriZ}]},
      separator,
       {hframe,[{label,[{bold,?__(4,"Zoom:")}]},
                {label,io_lib:format("~s",[wings_util:nice_float(Dist)])}]},
       {?__(5,"Specify the Distance between the Camera and Focal Point"),CamDist,
        [{info,?__(6,"Adjust the Distance between the camera and the Focal Point")++
          ?__(7," along the vector defined by the Position and the Focal Point.")},
         {key,cam_pos_specify_dist}]},
       {hframe,[{label,?__(8,"Distance")},{text,Dist,[{hook,DistHook}]}]},
      separator,
       {label,[{bold,?__(9,"Rotate Camera around Global Y through Focal Point:")}]},
       {hframe,[{hradio,[{?__(10,"Right"),right},
                         {?__(11,"Left"),left}],right,[{key,cam_x}]}]},
       {hframe,[{label,?__(12,"Degrees")},{text,0.0,[{range,{0.0,180.0}}]}]},
      separator,
       {label,[{bold,?__(13,"Rotate Camera around Screen Relative X through Focal Point:")}]},
       {hframe,[{hradio,[{?__(14,"Up"),up},
                         {?__(15,"Down"),down}],up,[{key,cam_y}]}]},
       {hframe,[{label,?__(12,"Degrees")},{text,0.0,[{range,{0.0,180.0}}]}]},
      separator,
       {label,[{bold,?__(16,"Screen Relative Pan Modifiers:")}]},
       {hframe,[{label,X},{text,PanX},
                {label,Y},{text,PanY}]},
      separator,
      {?__(17,"Show new camera position in Preview Mode after pressing OK"),Preview,
       [{key,cam_pos_preview}]},
      separator
      ],
    wings_ask:dialog(?__(1,"Position Camera Numerically"),Qs,
    fun(Res) ->
        {view,{camera_position,Res}}
    end).

start_nc_mode(Res,St) ->
    {Nc,Preview} = set_camera_position(Res,St),
    check_preview_1(Preview,Nc).

check_preview(true,Nc) ->
    event_handler(Nc);
check_preview(false,#nc{st=St}) ->
    exit_nc_mode(St).
check_preview_1(true,Nc) ->
    help(),
    {seq,push,event_handler(Nc)};
check_preview_1(false,#nc{st=St}) ->
    St.

event_handler(#nc{st=St}=Nc) ->
    wings:mode_restriction([]),
    Active = wings_wm:this(),
    wings_wm:callback(fun() -> wings_u:menu_restriction(Active, []) end),
    wings_wm:current_state(St),
    wings_draw:update_sel_dlist(),
    wings_wm:dirty(),
    {replace,fun(Ev) ->
        handle_event(Ev, Nc) end}.

handle_event(redraw, #nc{st=St}) ->
    help(),
    wings:redraw(?__(1,"Numeric Camera Preview"),St),
    keep;

handle_event(#mousebutton{button=1,state=?SDL_PRESSED}, _) ->
    set_camera_position();

handle_event(#mousebutton{button=3,state=?SDL_RELEASED}, #nc{st=St}) ->
    exit_nc_mode(St);

handle_event(#keyboard{sym=?SDLK_ESCAPE}, #nc{st=St}) ->
    exit_nc_mode(St);

handle_event(#keyboard{unicode=$1}, #nc{cur_view=CurView,other=OtherView}=Nc) ->
    Window = wings_wm:this(),
    wings_wm:set_prop(Window,current_view,OtherView),
    event_handler(Nc#nc{cur_view=OtherView,other=CurView});

handle_event({action,{view,{camera_position,Res}}},#nc{st=St}) ->
    {Nc,Preview} = set_camera_position(Res,St),
    check_preview(Preview,Nc);

handle_event(_,Nc) ->
    event_handler(Nc).

exit_nc_mode(St) ->
    wings_wm:later({new_state,St}),
    pop.

help() ->
    L = ?__(1,"Re-open numeric camera dialog"),
    R = ?__(2,"Accept current camera position and exit"),
    M =wings_msg:button_format(L,[],R),
    V = "[1] " ++ ?__(3,"Flip between the old and new camera positions"),
    wings_wm:message(M,V).

set_camera_position([CamX,CamY,CamZ, OriX,OriY,OriZ,
        {_,UseDist},ZDist, {_,RCamX},DegX, {_,RCamY},DegY, PanX,PanY, {_,Preview}], St) ->
    Window = wings_wm:this(),
    OldView = wings_view:current(),
    Origin = e3d_vec:neg({OriX,OriY,OriZ}),
    #view{elevation=El0} = OldView,
    {Az1,El1} = case round(abs(El0)/180) rem 2 of
      1 -> Normal = e3d_vec:norm(e3d_vec:sub(Origin,{CamX,CamY,CamZ})),
           {Az2,El2} = wings_view:align_view_to_normal(Normal),
           {Az2,El2+180};
      0 -> Normal = e3d_vec:norm(e3d_vec:sub({CamX,CamY,CamZ},Origin)),
           wings_view:align_view_to_normal(Normal)
    end,
    Az = rotate_camera(Az1,RCamX,DegX),
    El = rotate_camera(El1,RCamY,DegY),
    Dist = case UseDist of
        false -> e3d_vec:dist({CamX,CamY,CamZ}, Origin);
        true -> ZDist
    end,
    NewView = OldView#view{origin=Origin,distance=Dist,pan_x=PanX,pan_y=PanY,
        azimuth=Az,elevation=El},
    wings_pref:set_value(cam_pos_specify_dist,UseDist),
    wings_pref:set_value(cam_pos_preview,Preview),
    wings_wm:set_prop(Window,current_view,NewView),
    {#nc{cur_view=NewView,other=OldView,st=St},Preview}.

rotate_camera(Deg,_,0.0) -> Deg;
rotate_camera(Az,right,Deg) -> Az - Deg;
rotate_camera(Az,left,Deg) -> Az + Deg;
rotate_camera(El,up,Deg) -> El + Deg;
rotate_camera(El,down,Deg) -> El - Deg.
