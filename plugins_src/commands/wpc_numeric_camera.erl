%%
%%  wpc_numeric_camera.erl --
%%
%%    Plugin to set the camera postion numerically.
%%
%%  Copyright (c) 2009-2011 Richard Jones.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_numeric_camera).
-export([init/0,menu/2,command/2]).
-define(NEED_ESDL, 1).
-include_lib("wings/src/wings.hrl").

init() ->
    wings_pref:delete_value(cam_pos_preview),
    wings_pref:delete_value(cam_pos_specify_dist),
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

command({view,camera_position}, St) ->
   set_camera_position(true, St);
command({view,{camera_position,Res}},St) ->
    set_camera_position(Res,St);
command(_,_) -> next.

%%%% Set Position of Camera Numerically
set_camera_position(true, St) ->
    OrigView=#view{distance=Dist} = wings_view:current(),
    D = trunc(Dist)*1.0,
    X = wings_s:dir(x),
    Y = wings_s:dir(y),
    Z = wings_s:dir(z),
    Qs =
      [{vframe,
       [{vframe,
       [{label,?__(2,"Camera Position:")},
        {hframe,[{label,X},{text,D}]},
        {hframe,[{label,Y},{text,D}]},
        {hframe,[{label,Z},{text,D}]}]},
       separator,
       {vframe,
       [{label,?__(3,"Focal Point:")},
        {hframe,[{label,X},{text,0.0}]},
        {hframe,[{label,Y},{text,0.0}]},
        {hframe,[{label,Z},{text,0.0}]}]}]}],
    preview_fun(Qs, OrigView, St);

set_camera_position(Res,St) ->
    camera_position(Res,St).

preview_fun(Qs, OrigView, St) ->
    Window = wings_wm:this(),
    Title = ?__(1,"Position Camera Numerically"),
    Dialog = {preview,Qs},
    wings_dialog:dialog(Title, Dialog,
			fun
			    ({dialog_preview,Res}) ->
			       {view,{camera_position,{OrigView,Res}}};
			    (cancel) ->
			       wings_wm:set_prop(Window,current_view,OrigView),
			       St;
			    (Res) ->
			       {view,{camera_position,{OrigView,Res}}}
		       end).

camera_position({OldView,[CamX,CamY,CamZ, FX,FY,FZ]}, St) ->
    AzF = case CamX of
        0.0 -> 1.0;
        _ -> -CamX/abs(CamX)
    end,
    Y2 = case {CamX,CamZ} of
        {0.0,0.0} -> {0.0,0.0,1.0};
        _ -> e3d_vec:sub({CamX,0.0,CamZ}, {FX,0.0,FZ})
    end,
    Az =  AzF * e3d_vec:degrees(Y2, {0.0,0.0,1.0}),
    ElF = case CamY of
        0.0 -> 1.0;
        _ -> CamY/abs(CamY)
    end,
    Dist = e3d_vec:sub({CamX,CamY,CamZ}, {FX,FY,FZ}),
    El = ElF * e3d_vec:degrees(Dist,Y2),
    D = e3d_vec:len(Dist),
    NewView = OldView#view{origin={FX,FY,FZ},distance=D,azimuth=Az,elevation=El,
        pan_x=0.0,pan_y=0.0},
    Window = wings_wm:this(),
    wings_wm:set_prop(Window, current_view, NewView),
    St.

