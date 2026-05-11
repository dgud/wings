%%
%%  wpc_color_panel.erl --
%%
%%     A tool that shows a color hue bar
%%
%%  Copyright (c) 2023-2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_color_panel).
-export([init/0,menu/2,command/2]).
-export([win_data/1,win_name/0,window/1,window/5]).
-export([init/1,
     handle_call/3, handle_cast/2,
     handle_sync_event/3, handle_event/2, 
     handle_info/2, code_change/3, terminate/2
    ]).

-include_lib("wings/src/wings.hrl").

%-behaviour(wx_object).

-record(state, {
    win,
    mdown = false :: boolean(),
    p,
    histcol = [] :: [{integer(),integer(),integer()}], %% 0 ... 255 color tuples
    nh = false   :: boolean(), %% Don't add the next selected color to history
    col = {0.0,0.0,0.0} :: {float(),float(),float()}, %% 0 ... 1.0 color tuple
    cbit = none         :: binary() | none,       %% Color bitmap bytes
    %% cbits is the size of color bitmap after subtracting COLSQUARE
    cbits = {1,1}       :: {integer(),integer()},
    mode = gray         :: atom(),
    modep = 0.5,
    hold = none         :: {integer(),integer(),integer()} | none,
    hsel = none         :: {integer(),integer(),integer()} | none  %% More persistent than hold
}).

-define(COLSQUARE, 20).

init() ->
    true.

menu({window},Menu) ->
    tools_menu_entry(Menu);
menu(_,Menu) -> Menu.

command({window,?MODULE},St0) ->
    St = St0,
    window(St),
    keep;
    
command(_,_) ->
    next.

%%%
%%%

-define(WIN_NAME, {plugin, hsb_color_panel}).

win_data(?WIN_NAME) ->
    {?WIN_NAME, {right,[]}}.

win_name() ->
    ?WIN_NAME.

window(St) ->
    case wings_wm:is_window(?WIN_NAME) of
	true ->
	    wings_wm:raise(?WIN_NAME),
	    keep;
	false ->
		Pos  = {50, 50},
		Size = {368, 100},
		Ps0 = [],
	    window(?WIN_NAME, Pos, Size, Ps0, St)
    end.

window(?WIN_NAME, Pos, Size, Ps0, St) ->
	{Frame,Ps} = wings_frame:make_win(?__(1,"Color Panel"), [{size, Size}, {pos, Pos}|Ps0]),
	Window = wings_sup:window(undefined, ?MODULE, [Frame, Size, Ps, [] ]),
	F = fun(_) -> no end,
	Fs = [{drag_filter, F}, {display_data, geom_display_lists}|Ps],
	wings_wm:toplevel(?WIN_NAME, Window, Fs, {push,change_state(Window, St)}),
    keep.

%%%
%%%

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, _, _}, _Window, _St0) -> keep;
forward_event({current_state, St}, Window, _St0) ->
    {replace, change_state(Window, St)};
forward_event({apply, ReturnSt, Fun}, Window, St0) ->
    %% Apply ops from window in wings process
    case ReturnSt of
    true ->
        St = Fun(St0),
        {replace, change_state(Window, St)};
    false ->
        Fun(St0)
    end;
forward_event(Ev, Window, _) ->
    wx_object:cast(Window, Ev),
    keep.


%%
%%  Color Tool 
%%

init([Frame, {_W,_}, _Ps, _Cols0]) ->
    try
        Win = wxScrolledWindow:new(Frame, [{style, wings_frame:get_border()}]),
            #{bg:=BG} = wings_frame:get_colors(),
            wxPanel:setBackgroundColour(Win, BG),
        
        BorderSz = wxBoxSizer:new(?wxHORIZONTAL),
        
        wxWindow:setSizer(Win, BorderSz),
        wxScrolledWindow:setScrollRate(Win, 0, 105),
        
        ColorArea = wxPanel:new(Win, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
        wxWindow:setBackgroundStyle(ColorArea, ?wxBG_STYLE_PAINT),
        
        wxPanel:connect(ColorArea, paint, [callback]),
        wxPanel:connect(ColorArea, size, [{skip,false}]),
        [wxPanel:connect(ColorArea, EvN, [{skip, false}]) || EvN <- [left_up,left_down,motion,leave_window]],
        wxBoxSizer:add(BorderSz, ColorArea, [{flag, ?wxEXPAND bor ?wxALL}, {border, 4}, {proportion, 1}]),
        [wxWindow:connect(Win, EvN, [{skip, false}]) ||
        EvN <- [left_down, left_up, left_dclick, middle_up, right_up]],
        wxWindow:connect(ColorArea, right_up, [{skip, false}]),
        
        {Win, #state{win=Win,p=ColorArea}}
    catch _:Reason:ST ->
        io:format("ERROR: ~p ~p ~p~n",[?MODULE, Reason, ST]),
            error(Reason)
    end.

new_bit(ColorArea, Mode, Param) ->
    new_bit(ColorArea, none, none, Mode, Param).
new_bit(ColorArea, Cbit0, CbitSize0, Mode, Param) ->
    Size = wxWindow:getSize(ColorArea),
    if Size =/= CbitSize0 ->
            {W,H} = Size,
            Bar_Height = H - ?COLSQUARE,
            HSB_Dat = generate_hsb(W, Bar_Height, Mode, Param),
            {HSB_Dat, Size};
        true ->
            {Cbit0, CbitSize0}
    end.

handle_sync_event(#wx{event=#wxPaint{}}=_, _, #state{p=ColorArea,histcol=HistCol,cbit=Cbit,cbits=CbitS,hold=HoldColor}=_) ->
    draw_colors(ColorArea, HistCol, Cbit, CbitS, HoldColor).

handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, State) ->
    wings_frame ! Ev,
    {noreply, State};
handle_event(#wx{obj={wx_ref,_,wxPanel,[]},event=#wxSize{}}=_Ev,#state{p=ColorArea,cbit=Cbit,cbits=Cbits0,mode=Mode,modep=Param}=State) ->
    wxWindow:refresh(ColorArea),
    try
        {HSB, Size} = new_bit(ColorArea, Cbit, Cbits0, Mode, Param),
        {noreply, State#state{cbit=HSB, cbits=Size}}
    catch _:_:_ST ->
        {noreply, State}
    end;
handle_event(#wx{obj=ColorArea,event=#wxMouse{type=left_down,x=X,y=Y}}=_Ev,#state{p=ColorArea,col=Col0,histcol=HC,mode=Mode,modep=Param}=State) ->
    wxWindow:refresh(ColorArea),
    Size = wxWindow:getSize(ColorArea),
    {W,H} = Size,
    H1 = H - ?COLSQUARE,
    X2 = X * 360.0 / W,
    Y2 = (H1 - Y) / H1,
    if ((X2 >= 0.0) andalso (X2 =< 360.0) andalso
         (Y2 >= 0.0) andalso (Y2 =< 1.0)) ->
            %% Color area
            Col = hsb_from_2d(X2, Y2, Mode, Param),
            NoHist = false;
        (Y2 < 0.0) ->
            %% Color history squares
            Idx = round(X) div ?COLSQUARE,
            if Idx < length(HC) ->
                    Col = col_to_1(lists:nth(Idx+1,HC)),
                    NoHist = true;
                true ->
                    Col = Col0,
                    NoHist = true
            end;
        true ->
            Col = Col0,
            NoHist = true
    end,
    set_color_to_scene(Col),
    {noreply, State#state{mdown=true,col=Col,nh=NoHist,hold=col_to_255(Col),hsel=none}};
handle_event(#wx{obj=ColorArea,event=#wxMouse{type=left_up,x=_X,y=_Y}}=_Ev,#state{p=ColorArea,col=Col,histcol=HistCol0,nh=NoHist}=State) ->
    wxWindow:refresh(ColorArea),
    if not NoHist ->
            HistCol1 = add_to_hist(Col, HistCol0);
        true ->
            HistCol1 = HistCol0
    end,
    {noreply, State#state{mdown=false,histcol=HistCol1}};
handle_event(#wx{event=#wxMouse{type=right_up}}=Ev,#state{win=Win,hold=HoldCol}=State) ->
    case wings_menu:is_popup_event(Ev) of
    {yes, GlobalPos} ->
        wings_wm:psend(?WIN_NAME, {apply, false, fun(_) ->
            wings_menu:popup_menu(Win, GlobalPos, ?MODULE, [
                {?__(1,"Colors to Gray"),{'VALUE',col_gray},?__(2,"Colors to Gray")},
                {?__(3,"Colors to White"),{'VALUE',col_white},?__(4,"Colors to White")},
                {?__(5,"Colors to Black"),{'VALUE',col_black},?__(5,"Colors to Black")},
                {?__(7,"Colors to Specific Gray"),{'VALUE',col_gray_spec},?__(7,"Colors to Specific Gray")},
                {?__(15,"Colors at Specific Saturation"),{'VALUE',col_spec_sat},?__(15,"Colors at Specific Saturation")},
                {?__(17,"Colors at Specific Value"),{'VALUE',col_spec_val},?__(17,"Colors at Specific Value")},
                {?__(9,"Grayscale"),{'VALUE',col_gs},?__(9,"Grayscale")},
                separator,
                {?__(11,"Send history to palette"),{'VALUE',col_hist_pal},?__(12,"Send color history to palette")},
                separator,
                {?__(13,"Trim history"),{'VALUE',col_hist_trim},?__(14,"Trim color history")}
            ] ++
            if HoldCol =:= none -> [];
                true ->
                    [{?__(18,"Delete color"),{'VALUE',col_hist_del},?__(19,"Delete color from history")}]
            end)
        end});
    _ ->
        ok
    end,
    {noreply, State#state{hsel=HoldCol}};
handle_event(#wx{obj=ColorArea,event=#wxMouse{type=motion,x=X,y=Y}}=_,#state{mdown=true,p=ColorArea,mode=Mode,modep=Param}=State) ->
    Size = wxWindow:getSize(ColorArea),
    {W,H} = Size,
    H1 = H - ?COLSQUARE,
    
    X2 = X * 360.0 / W,
    Y2 = (H1 - Y) / H1,
    if ((X2 >= 0.0) andalso (X2 =< 360.0) andalso
          (Y2 >= 0.0) andalso (Y2 =< 1.0)) ->
            %% Color area
            Col = hsb_from_2d(X2, Y2, Mode, Param),
            wxWindow:refresh(ColorArea),
            preview_color_to_scene(Col),
            {noreply, State#state{col=Col,hold=col_to_255(Col)}};
        true ->
            {noreply, State}
    end;
handle_event(#wx{obj=ColorArea,event=#wxMouse{type=leave_window}}=_,#state{mdown=false,p=ColorArea,hold={_,_,_}}=State) ->
    %% Keeps the preview color square on until the mouse moves again 
    %% after mouse button is released.
    wxWindow:refresh(ColorArea),
    {noreply, State#state{hold=none}};
handle_event(_Ev, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

    
handle_cast({action,{?MODULE,col_gray}}, State) ->
    set_mode_state(State, gray);
handle_cast({action,{?MODULE,col_black}}, State) ->
    set_mode_state(State, black);
handle_cast({action,{?MODULE,col_white}}, State) ->
    set_mode_state(State, white);
handle_cast({action,{?MODULE,col_spec_sat}}, State) ->
    wings_wm:psend(?WIN_NAME, {apply, false, fun(_) ->
        Dialog =
          [{hframe,[{label,?__(6,"Specific saturation (0.0 to 1.0):")},
                    {slider,{text,0.5,[{range,{0.0,1.0}}]}}]}],
        wpa:dialog(true, ?__(5,"Specific saturation"),
            Dialog,
            fun([Amt|_]) -> {?MODULE,col_spec_sat,max(0.0,min(1.0,Amt))} end)
    end}),
    {noreply, State};
handle_cast({action,{?MODULE,col_spec_sat,Amt}}, #state{p=ColorArea}=State)
  when Amt >= 0.0, Amt =< 1.0 ->
    wxWindow:refresh(ColorArea),
    set_mode_state(State, sat_spec, Amt);
handle_cast({action,{?MODULE,col_spec_val}}, State) ->
    wings_wm:psend(?WIN_NAME, {apply, false, fun(_) ->
        Dialog =
          [{hframe,[{label,?__(8,"Specific value (0.0 to 1.0):")},
                    {slider,{text,0.5,[{range,{0.0,1.0}}]}}]}],
        wpa:dialog(true, ?__(7,"Specific value"),
            Dialog,
            fun([Amt|_]) -> {?MODULE,col_spec_val,max(0.0,min(1.0,Amt))} end)
    end}),
    {noreply, State};
handle_cast({action,{?MODULE,col_spec_val,Amt}}, #state{p=ColorArea}=State)
  when Amt >= 0.0, Amt =< 1.0 ->
    wxWindow:refresh(ColorArea),
    set_mode_state(State, val_spec, Amt);
handle_cast({action,{?MODULE,col_gray_spec}}, State) ->
    wings_wm:psend(?WIN_NAME, {apply, false, fun(_) ->
        Dialog =
          [{hframe,[{label,?__(4,"Specific gray (0.0 to 1.0):")},
                    {slider,{text,0.5,[{range,{0.0,1.0}}]}}]}],
        wpa:dialog(true, ?__(3,"Specific gray"),
            Dialog,
            fun([Amt|_]) -> {?MODULE,col_gray_spec,max(0.0,min(1.0,Amt))} end)
    end}),
    {noreply, State};
handle_cast({action,{?MODULE,col_gray_spec,Amt}}, #state{p=ColorArea}=State)
  when Amt >= 0.0, Amt =< 1.0 ->
    wxWindow:refresh(ColorArea),
    set_mode_state(State, gray_spec, Amt);

handle_cast({action,{?MODULE,col_gs}}, State) ->
    set_mode_state(State, gs);
handle_cast({action,{?MODULE,col_hist_pal}}, #state{histcol=HC0,p=ColorArea}=State)
  when length(HC0) > 0 ->
    wings_wm:psend(?WIN_NAME, {apply, true, fun(#st{pal=Pal0}=St) ->
        St1 = St#st{pal=remove_none(Pal0) ++
                        [col_to_1(C) || C <- lists:reverse(HC0)]},
        wings_wm:send(geom, {new_state,St1}),
        St1
    end}),
    wxWindow:refresh(ColorArea),
    {noreply, State#state{histcol=[]}};
handle_cast({action,{?MODULE,col_hist_trim}}, State) ->
    wings_wm:psend(?WIN_NAME, {apply, false, fun(_) ->
        Dialog =
          [{hframe,[{label,?__(2,"Trim to:")},{text,10}]}],
        wpa:dialog(true, ?__(1,"Trim history"),
            Dialog,
            fun([Amt|_]) -> {?MODULE,col_hist_trim,Amt} end)
    end}),
    {noreply, State};
handle_cast({action,{?MODULE,col_hist_trim,Amt}}, #state{histcol=HC0,p=ColorArea}=State) ->
    wxWindow:refresh(ColorArea),
    {noreply, State#state{histcol=lists:sublist(HC0,Amt)}};
    
handle_cast({action,{?MODULE,col_hist_del}}, #state{histcol=HC0,p=ColorArea,hsel=Col}=State) ->
    wxWindow:refresh(ColorArea),
    {noreply, State#state{histcol=remove_col(HC0,Col)}};


handle_cast(_Req, State) -> io:format("_Req=~p~n", [_Req]),
    {noreply, State}.
    

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_From, _To, State) ->
    State.

terminate(_Reason, _) ->
    wings ! {wm, {delete, ?WIN_NAME}},
    normal.


set_color_to_scene(Color) ->
    Set =
        fun(#st{}=St0) ->
            St = color_from_st(Color,St0),
            wings_wm:send(geom, {new_state, St}),
            St
        end,
    wings_wm:psend(?WIN_NAME, {apply, true, Set}).

color_from_st(Color,#st{sel=Sel,selmode=SelMode}=St0) ->
    case SelMode of
        vertex ->
            wings_vertex_cmd:set_color(Color, St0);
        edge ->
            wings_edge_cmd:set_color(Color, St0);
        face ->
            wings_face_cmd:set_color(Color, St0);
        body ->
            St1 = wings_sel_conv:mode(face, St0),
            St2 = wings_face_cmd:set_color(Color, St1),
            St2#st{sel=Sel,selmode=SelMode}
    end.

%% Show a preview of the currently changing color
preview_color_to_scene(Color) ->
    wings_wm:psend(?WIN_NAME, {apply, true, fun(#st{}=St) ->
        St1=color_from_st(Color,St),
        wings_wm:send(geom, {update_state, St1}),
        St1
    end}).


set_mode_state(State, Mode) ->
    set_mode_state(State#state{hold=none}, Mode, 0.5).
set_mode_state(#state{p=ColorArea}=State, Mode, Param) ->
    {Cbit,Size} = new_bit(ColorArea, Mode, Param),
    wxWindow:refresh(ColorArea),
    {noreply, State#state{mode=Mode,modep=Param,cbit=Cbit,cbits=Size}}.

tools_menu_entry([]) ->
    tools_menu_entry_1();
tools_menu_entry([{_,palette,_}=A|Menu]) ->
    [A|tools_menu_entry_1()] ++ Menu;
tools_menu_entry([A|Menu]) ->
    [A|tools_menu_entry(Menu)].
tools_menu_entry_1() ->
    [{"Color Panel",?MODULE,"Color panel"}].


draw_colors(ColorArea, HistoryCol, HSB_Dat, {HSBW,HSBH}, HoldColor) ->
    Size = wxWindow:getSize(ColorArea),
    SWidth = ?COLSQUARE,
    SHeight = SWidth,
    HSB_I = wxImage:new(HSBW, HSBH - SHeight, HSB_Dat),
    HSB = wxBitmap:new(HSB_I),
	
	DC = get_dc(ColorArea),
	draw_colors_1(DC, SWidth, SHeight, Size, HSB, ColorArea, HistoryCol, HoldColor),
	wxPaintDC:destroy(DC),
	
    wxBitmap:destroy(HSB),
    wxImage:destroy(HSB_I).

get_dc(ColorArea) ->
	case os:type() of
		{win32, _} ->
			wx:typeCast(wxBufferedPaintDC:new(ColorArea), wxPaintDC);
		_ ->
			wxPaintDC:new(ColorArea)
	end.


draw_colors_1(DC, SWidth, SHeight, {W,H}=Size, HSB, _ColorArea, HistoryCol, HoldColor) ->
    Bar_Height = H - SHeight,
    
    %% Clear background
    BCol = {30,30,30},
    B = wxBrush:new(BCol),
    P = wxPen:new(BCol),
    wxDC:setPen(DC, P),
    wxDC:setBrush(DC, B),
    wxDC:drawRectangle(DC, {0,0}, Size),
    wxPen:destroy(P),
    wxBrush:destroy(B),
    
    wxDC:drawBitmap(DC, HSB, {0, 0}),
    
    lists:foldl(fun(Color, {CurX, CurY}) ->
        draw_one_square(DC, CurX, CurY+Bar_Height, SWidth,SHeight, Color),
        case CurX+SWidth > W of
            true ->
                {0, CurY+SHeight};
            false ->
                {CurX+SWidth, CurY}
        end
    end, {0,0}, HistoryCol),
    
    if HoldColor =/= none ->
        draw_one_square(DC, W-48,H-48,48,48, HoldColor);
        true -> ok
    end,
    ok.

draw_one_square(DC, X,Y,W,H, Color) ->
    B = wxBrush:new(Color),
    wxDC:setBrush(DC, B),
    wxDC:drawRectangle(DC, {X, Y}, {W, H}),
    wxBrush:destroy(B).


hsb(H, S, V) when S < 0, H < 0 ->
    {V, V, V};

hsb(H_01, S, V) ->
    H_0 = case (H_01 >= 360.0) of
        true ->
            0.0;
        false ->
            H_01
    end,
    wings_color:hsb_to_rgb(H_0, S, V).


col_to_255({R, G, B}) ->
    {round(R * 255), round(G * 255), round(B * 255)}.

col_to_1({R, G, B}) ->
    {R / 255.0, G / 255.0, B / 255.0}.


add_to_hist(Col, HistCol0) ->
    add_to_hist_1(col_to_255(Col), HistCol0).
add_to_hist_1(Col, [Col|_]=HistCol0) ->
    HistCol0;
add_to_hist_1(Col, HistCol0) ->
    [Col|HistCol0].


-spec generate_hsb(integer(),integer(),atom(),any()) -> binary().

generate_hsb(W, H, Mode, Param) ->
    generate_hsb([], W, H, Mode, Param, 0, 0).
generate_hsb(Img, _W, H, _Mode, _Param, IY, _IX) when IY >= H ->
    list_to_binary(lists:reverse(Img));
generate_hsb(Img, W, H, Mode, Param, IY, IX) when IX >= W ->
    generate_hsb(Img, W, H, Mode, Param, IY+1, 0);
generate_hsb(Img, W, H, Mode, Param, IY, IX) when IY < H, IX < W ->
    Y2 = (H - IY) / float(H),
    {R_0,G_0,B_0} = hsb_from_2d(round(IX * 360.0 / W), Y2, Mode, Param),
    R = floor(R_0 * 255),
    G = floor(G_0 * 255),
    B = floor(B_0 * 255),
    generate_hsb([<<R,G,B>>|Img], W, H, Mode, Param, IY, IX+1).

-spec hsb_from_2d(number(), float(), atom(), any()) -> {float(),float(),float()}.

hsb_from_2d(Hue, Y2, gray, _) ->
    hsb(Hue, Y2, 0.5 + Y2 * 0.5);
hsb_from_2d(Hue, Y2, gray_spec, Amt) ->
    hsb(Hue, Y2, Amt + Y2 * (1.0 - Amt));
hsb_from_2d(Hue, Y2, white, _) ->
    %% brightness at 100, change saturation
    hsb(Hue, Y2, 1.0);
hsb_from_2d(Hue, Y2, black, _) ->
    %% change brightness, saturation at 100
    hsb(Hue, 1.0, Y2);
hsb_from_2d(Hue, Y2, sat_spec, Amt) ->
    hsb(Hue, Amt, Y2);
hsb_from_2d(Hue, Y2, val_spec, Amt) ->
    hsb(Hue, Y2, Amt);
hsb_from_2d(Hue, Y2, gs, _) ->
    %% brightness at 100 to 0, saturation 0 for grayscale
    hsb(Hue, 0.0, Y2).

remove_none(A) ->
    A1 = lists:dropwhile(
            fun (C) -> C =:= none end,
            lists:reverse(A)),
    lists:reverse(A1).

remove_col(HC0,Col) ->
    [C || C <- HC0, C =/= Col].


