%%
%%  wpc_tools_win_resize --
%%
%%     Plug-in to resize a selected window
%%
%%  Copyright (c) 2025 Micheus Vieira
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wpc_tools_win_resize).

-include_lib("wings/src/wings.hrl").

-export([init/0,menu/2,command/2]).

%%%
%%% plugin interface
%%%


init() -> true.

menu({tools},Menu) ->
    parse(Menu,[],false);
menu(_,Menu) -> Menu.

parse([],MenuNew,true) ->
    MenuNew;
parse([],MenuNew,false) ->
    MenuNew ++ [separator|draw_menu()];
parse([{_,screenshot,_,_}=ScreenShot|Rest],MenuNew,_) ->
    MenuNew2 = MenuNew ++ [ScreenShot],
    case Rest of
        [Now|Rest2] ->
            MenuNew3 = MenuNew2 ++ draw_menu() ++ [Now],
            parse(Rest2,MenuNew3,true);
        [] ->
            MenuNew2 ++ draw_menu()
    end;
parse([First|Rest],MenuNew,State) ->
    MenuNew2 = MenuNew ++ [First],
    parse(Rest,MenuNew2,State).

draw_menu() ->
    [{?__(1,"Resize Window"),resize_window,
      ?__(2,"Resizes a detached window or the main window")}].

command({tools,resize_window}, St) ->
    resize_window(St);
command({tools,{resize_window,Res}}, St) ->
    resize_window(Res,St);
command(_,_) -> next.

%%
%% Resize the main app or a float window.
%%

get_win_borders(Win) ->
    XBorder =
        wxSystemSettings:getMetric(?wxSYS_BORDER_X ,[{win,Win}]) +
        wxSystemSettings:getMetric(?wxSYS_EDGE_X ,[{win,Win}]) +
        wxSystemSettings:getMetric(?wxSYS_FRAMESIZE_X ,[{win,Win}]),
    YBorder =
        wxSystemSettings:getMetric(?wxSYS_BORDER_Y ,[{win,Win}]) +
        wxSystemSettings:getMetric(?wxSYS_EDGE_Y ,[{win,Win}]) +
        wxSystemSettings:getMetric(?wxSYS_FRAMESIZE_Y ,[{win,Win}]),
    {XBorder,YBorder}.

get_full_win_size(Win) ->
    {XBorder,YBorder} = get_win_borders(Win),
    {W,H} = wxWindow:getSize(Win),
    {W-XBorder*2,H-YBorder}.

set_full_win_size(Win,{W,H}) ->
    {XBorder,YBorder} = get_win_borders(Win),
    wxWindow:setSize(Win,{W+XBorder*2,H+YBorder}).

resize_window(_St) ->
    {MaxWidth,MaxHeight} = wx_misc:displaySize(),
    MainWin = wings_frame:get_top_frame(),
    {_, Free} = wx_object:call(wings_frame, get_windows),

    Wins0 = wxWindow:getChildren(MainWin),
    WxWins = [{wxWindow:getLabel(Win),Win} || Win <- Wins0, wxWindow:getLabel(Win) =/= []],
    PickWxInfo = fun(Name) ->
            WinTitle = wings_wm:get_prop(Name,title),
            case lists:keyfind(WinTitle,1,WxWins) of
                {_,WxWin} -> {get_full_win_size(WxWin),WxWin};
                false -> {wings_wm:win_size(Name),wx:null()}
            end
        end,
    WinToSel = [{"Wings3D App",main}] ++
               [{wings_wm:get_prop(Name,title),Name} || {Name,_,_,_} <- Free],
    WinSize = [{main,{get_full_win_size(MainWin),MainWin}}] ++
              [{Name,PickWxInfo(Name)} || {Name,_,_,_} <- Free],

    WinSizeHook = fun(Key,Val,Store) ->
        case Key of
            win_to_sel ->
                {_, {{W,H},WxWin}} = lists:keyfind(Val, 1, WinSize),
                wings_dialog:set_value(wxwin,WxWin,Store),
                wings_dialog:set_value(width,W,Store),
                wings_dialog:set_value(height,H,Store),
                wings_dialog:set_value(win_size_opt,size_to_id({W,H}),Store),

                Opt = wings_dialog:get_value(win_size_opt,Store),
                wings_dialog:enable(pnl_resize, Opt==custom, Store),
                wings_dialog:update(pnl_resize, Store);
            win_size_opt ->
                if Val =/= custom ->
                    {W,H} = id_to_size(Val);
                true ->
                    Win = wings_dialog:get_value(win_to_sel,Store),
                    {_,{{W,H},_}} = lists:keyfind(Win, 1, WinSize)
                end,
                wings_dialog:set_value(width,W,Store),
                wings_dialog:set_value(height,H,Store),
                wings_dialog:enable(pnl_resize, Val==custom, Store),
                wings_dialog:update(pnl_resize, Store);
            _ -> ok
        end
                  end,
    Qs =
    [{vframe, [
        {hframe, [
            {value,"",[{key,wxwin}]},
            {label_column, [
                {?__(2,"Window"),{menu,WinToSel,main,[{key,win_to_sel},{hook,WinSizeHook}]}}
            ]}
        ], [{key,pnl_win_to_sel},{enabled,length(WinToSel)>1}]},
        {label_column, [
            {?__(3,"Dimension"),
             {menu,
              [{"SD [4:3]" ,sd},
               {"HD [16:9]" ,hd},
               {"Full HD [16:9]",full_hd},
               {"Quad HD (2K) [16:9]",quad_hd},
               {"512x512 [1:1]",k},
               {"1024x1024 [1:1]",k1},
               {?__(4,"Custom"),custom}],
              custom, [{key,win_size_opt},{hook,WinSizeHook}]}}
        ]},
        {hframe, [
            {label_column, [
                {?__(6, "Width"),{text, 0, [{range,{10, MaxWidth}}, {key,width}]}}
            ]},
            {label_column, [
                {?__(7, "Height"),{text, 0, [{range,{10, MaxHeight}}, {key,height}]}}
            ]}
        ],[{title, ?__(5, "Custom size")},{key,pnl_resize},{margin,false}]}
    ]}],
    wings_dialog:dialog(?__(1,"Set Window's Size"), Qs,
                        fun(Res)-> {tools,{resize_window,Res}} end).

resize_window([{wxwin,WxWin},{win_to_sel,_},{win_size_opt,_},{width,W},{height,H}], _St) ->
    set_full_win_size(WxWin,{W,H}),
    ok.

%% Utils

id_to_size(sd) -> {640,480};
id_to_size(hd) -> {1280,720};
id_to_size(full_hd) -> {1920,1080};
id_to_size(quad_hd) -> {2560,1440};
id_to_size(k) -> {512,512};
id_to_size(k1) -> {1024,1024}.

size_to_id({640,480}) -> sd;
size_to_id({1280,720}) -> hd;
size_to_id({1920,1080}) -> full_hd;
size_to_id({2560,1440}) -> quad_hd;
size_to_id({512,512}) -> k;
size_to_id({1024,1024}) -> k1;
size_to_id(_) -> custom.
