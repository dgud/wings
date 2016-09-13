%%
%%  ww_color_slider.erl --
%%
%%     A color slider
%%
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(ww_color_slider).
-behaviour(wx_object).
%% Callbacks
-export([init/1, terminate/2, code_change/3,
	 handle_sync_event/3, handle_event/2, handle_cast/2, handle_info/2,
	 handle_call/3]).

%% API
-export([new/3, new/4, getColor/1, setColor/2, connect/2, connect/3]).

-ifdef(DEBUG).
-export([test/0]). % Test
-endif.

new(Parent, Id, Col) ->
    new(Parent, Id, Col, []).
new(Parent, Id, Col, Opts) ->
    wx_object:start(?MODULE, [Parent, Id, Col, Opts], []).

getColor(Ctrl) ->
    wx_object:call(Ctrl, get_color).

setColor(Ctrl, RGB) ->
    wx_object:cast(Ctrl, {set_color, RGB}).

connect(Ctrl, Msg) ->
    connect(Ctrl, Msg, []).

connect(Ctrl, col_changed, Opts) ->
    wx_object:call(Ctrl, {connect, Opts});
connect(Ctrl, What, Opts) ->
    wxPanel:connect(Ctrl, What, Opts).


%% Callbacks

-include_lib("wx/include/wx.hrl").

-record(state, {self, this, curr, mode,
		c1, c2,
		bmp, bgb,
		focus=false,
		capture=false,
		fpen,
		handlers=[] %% Listeners or callbacks
	       }).
-define(PANEL_MIN_SIZE, {150, 20}).
-define(SLIDER_MIN_HEIGHT, 10).
-define(SLIDER_OFFSET, {8, 5}).

-define(wxGC, wxGraphicsContext).

init([Parent, Id, Col, Opts0]) ->
    {Mode, Opts1} = default(color, Opts0, rgb),
    {Style0, Opts} = default(style, Opts1, 0),
    Style =  Style0 bor ?wxFULL_REPAINT_ON_RESIZE
	bor ?wxCLIP_CHILDREN bor ?wxTAB_TRAVERSAL,
    Panel = wxPanel:new(Parent, [{winid, Id}, {style, Style}|Opts]),
    wxWindow:setMinSize(Panel, ?PANEL_MIN_SIZE),
    Bmp = slider_bitmap(),
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, erase_background), %% WIN32 only?
    BGC = wxPanel:getBackgroundColour(Parent),
    case os:type() of
	{win32,_} ->
	    wxPanel:setBackgroundColour(Panel, BGC);
	_ -> ignore
    end,

    Brush = wxBrush:new(BGC),
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, left_up),
    wxPanel:connect(Panel, motion),
    wxPanel:connect(Panel, set_focus),
    wxPanel:connect(Panel, kill_focus),
    wxPanel:connect(Panel, char_hook, [callback]),
    wxPanel:connect(Panel, key_down,  [callback]),

    {Curr,SCol,ECol}  = get_col_range(Mode, Col),

    FPen = wxPen:new(wxSystemSettings:getColour(?wxSYS_COLOUR_HIGHLIGHT)),

    {Panel, #state{self=self(), this=Panel,
		   curr=Curr, mode=Mode,
		   c1=SCol, c2=ECol,
		   bmp=Bmp, bgb=Brush,
		   fpen=FPen}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Redraw the control
handle_sync_event(#wx{obj=Panel, event=#wxPaint{}}, _,
		  #state{this=Panel, curr=Curr, mode=Mode,
			 c1=C1, c2=C2,
			 bmp=Bmp, bgb=BGB,
			 focus=Focus, fpen=FPen
			}) ->
    DC = case os:type() of
	     {win32, _} -> %% Flicker on windows
		 BDC = wx:typeCast(wxBufferedPaintDC:new(Panel), wxPaintDC),
		 wxDC:setBackground(BDC, BGB),
		 wxDC:clear(BDC),
		 BDC;
	     _ ->
		 wxPaintDC:new(Panel)
	 end,
    {_,_, W0,H0} = wxPanel:getRect(Panel),
    %% Draw focus rectangle
    if Focus ->
	    wxDC:setPen(DC, FPen),
	    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
	    wxDC:drawRoundedRectangle(DC, {1,1,W0-2,H0-2}, 3);
       true -> ignore
    end,
    %% Draw background
    {XOFF,YOFF} = ?SLIDER_OFFSET,
    {_, HMIN} = ?PANEL_MIN_SIZE,
    Y0 = (H0 - HMIN) div 2,
    X = get_curr(Mode, Curr),
    case Mode of
	hue ->
	    Width = W0-2*XOFF,
	    PartW = round(Width*60/360),
	    Draw = fun(Hue, {X0, C00, C11, S, V}) ->
			   DW = if Hue =:= 300 -> XOFF+Width - X0;
				   true -> PartW
				end,
			   wxDC:gradientFillLinear(DC, {X0, Y0+YOFF, DW, ?SLIDER_MIN_HEIGHT},
						   C00, C11, [{nDirection, ?wxRIGHT}]),
			   {X0+PartW, C11, rgb256(hsv_to_rgb(Hue+120,S,V)), S,V}
		   end,
	    {_H, S, V} = Curr,
   	    lists:foldl(Draw, {XOFF,rgb256(C1),rgb256(C2),S,V}, lists:seq(0, 300, 60));
	_ ->
	    wxDC:gradientFillLinear(DC, {XOFF, Y0+YOFF, W0-2*XOFF, ?SLIDER_MIN_HEIGHT},
				    rgb256(C1), rgb256(C2), [{nDirection, ?wxRIGHT}])
    end,
    %% Draw selector
    Pos = XOFF + (W0-2*XOFF)*X,
    wxDC:drawBitmap(DC, Bmp, {trunc(Pos-7),Y0}),
    wxPaintDC:destroy(DC),
    ok;

%%% Key events must be handled sync'ed so we can call skip for TAB traversal
handle_sync_event(#wx{event=#wxKey{keyCode=Key}}, Event, 
		  #state{self=Self, curr=Curr, mode=Mode}) ->
    Prev = get_curr(Mode, Curr),
    Move = case Key of
	       ?WXK_LEFT     -> -0.01;
	       ?WXK_RIGHT    ->  0.01;
	       ?WXK_PAGEUP   -> -0.10;
	       ?WXK_PAGEDOWN ->  0.10;
	       ?WXK_HOME     -> -Prev;
	       ?WXK_END      ->  1.0-Prev;
	       ?WXK_NUMPAD_LEFT     -> -0.01;
	       ?WXK_NUMPAD_RIGHT    ->  0.01;
	       ?WXK_NUMPAD_PAGEUP   -> -0.10;
	       ?WXK_NUMPAD_PAGEDOWN ->  0.10;
	       ?WXK_NUMPAD_HOME     -> -Prev;
	       ?WXK_NUMPAD_END      ->  1.0-Prev;
	       _ -> false
	   end,
    %% io:format("Key ~p ~p~n",[Key, Move]),
    case Move of
	false -> wxEvent:skip(Event);
	_ -> Self ! {move, Move}
    end,
    ok.

%% Other events
handle_event(#wx{event=#wxMouse{type=motion, x=X}},
	     #state{this=This, mode=Mode, curr=Curr, capture=true} = State0) ->
    State = State0#state{curr=slider_pos(This, X, Mode, Curr)},
    [apply_callback(H, get_curr_color(State)) || H <- State#state.handlers],
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=left_down, x=X}},
	     #state{this=This, mode=Mode, curr=Curr, capture=false} = State0) ->
    %% wxPanel:setFocus(This),  %% crashes on win64 when in autouv..
    wxPanel:captureMouse(This),
    State = State0#state{curr=slider_pos(This, X, Mode, Curr), capture=true},
    [apply_callback(H, get_curr_color(State)) || H <- State#state.handlers],
    {noreply, State#state{focus=true}};
handle_event(#wx{event=#wxMouse{type=left_up}},
	     #state{this=This, capture=Captured} = State) ->
    Captured andalso wxPanel:releaseMouse(This),
    wxWindow:refresh(This),
    {noreply, State#state{capture=false}};
handle_event(#wx{event=#wxFocus{type=What}}, #state{this=This} = State) ->
    wxWindow:refresh(This),
    {noreply, State#state{focus=What=:=set_focus}};

handle_event(_Ev, State) ->
    %% io:format("Skip Ev ~p~n",[_Ev]),
    {noreply, State}.

handle_call({connect, Opts}, From, #state{handlers=Curr} = State) ->
    case proplists:get_value(callback, Opts) of
	undefined -> 
	    {reply, ok, State#state{handlers=[From|Curr]}};
	CB when is_function(CB) ->
	    {reply, ok, State#state{handlers=[CB|Curr]}};
	Bad ->
	    {reply, {error, {badarg, Bad}}, State}
    end;

handle_call(get_color, _From, State) ->
    {reply, get_curr_color(State), State}.

handle_cast({set_color, Col}, State = #state{this=This, mode=Mode}) ->
    {Curr, SCol, ECol} = get_col_range(Mode, Col),
    wxWindow:refresh(This),
    {noreply, State#state{curr=Curr, c1=SCol, c2=ECol}}.

terminate(_Reason, #state{this=_This, bmp=Bmp, bgb=BGB, fpen=Fpen}) ->
    wxBrush:destroy(BGB),
    wxBitmap:destroy(Bmp),
    wxPen:destroy(Fpen),
    %% wxPanel:destroy(This), %% Is destroyed by the owner
    ok.

handle_info({move,Move}, State0 = #state{this=This, mode=Mode, curr=Prev}) ->
    wxWindow:refresh(This),
    V = get_curr(Mode, Prev),
    State = State0#state{curr=set_curr(Mode, max(0.0, min(1.0, V+Move)), Prev)},
    [apply_callback(H, get_curr_color(State)) || H <- State#state.handlers],
    {noreply, State};
handle_info(_, State) -> State.

code_change(_, _, State) -> State.

default(Key, Opts, Def) ->
    {proplists:get_value(Key, Opts, Def),
     proplists:delete(Key,Opts)}.

slider_pos(This, X, Mode, Curr) ->
    wxWindow:refresh(This),
    {W, _} = wxPanel:getSize(This),
    {X0,_Y0} = ?SLIDER_OFFSET,
    Value = max(0.0, min(1.0, (X-X0)/(W-X0*2))),
    set_curr(Mode, Value, Curr).

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R, G, B).

rgb_to_hsv(R,G,B) ->
    {H,S,V} = wings_color:rgb_to_hsv(R,G,B),
    {round(H),S,V}.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H, S, V).

hsv_to_rgb(H, S, V) ->
    wings_color:hsv_to_rgb(H, S, V).

get_curr(rgb, {_H,_S,V}) -> V;
get_curr(red, {R, _G, _B}) -> R;
get_curr(green, {_R, G, _B}) -> G;
get_curr(blue, {_R, _G, B}) -> B;
get_curr(hue, {H, _S, _V}) -> H / 360;
get_curr(sat, {_H, S, _V}) -> S;
get_curr(val, {_H, _S, V}) -> V.

set_curr(rgb, V, {H,S,_V}) -> {H,S,V};
set_curr(red, R, {_R, G, B}) -> {R,G,B};
set_curr(green, G, {R, _G, B}) -> {R,G,B};
set_curr(blue, B, {R, G, _B}) -> {R,G,B};
set_curr(hue, H, {_H, S, V}) -> {H*360.0, S,V};
set_curr(sat, S, {H, _S, V}) -> {H,S,V};
set_curr(val, V, {H, S, _V}) -> {H,S,V}.

get_curr_color(#state{mode=Mode, curr=Curr}) ->
    get_curr_color(Mode, Curr).

get_curr_color(C, RGB) when C =:= red; C =:= blue; C =:= green -> RGB;
get_curr_color(_Other, HSV) -> hsv_to_rgb(HSV).

get_col_range(rgb, {R,G,B}) ->
    HSV  = {Hue,S,_V} = rgb_to_hsv(R, G, B),
    SCol = hsv_to_rgb(Hue, S, 0.0),
    ECol = hsv_to_rgb(Hue, S, 1.0),
    {HSV,SCol,ECol};
get_col_range(red, RGB={_R,G,B}) ->
    {RGB,{0,G,B},{1,G,B}};
get_col_range(green, RGB={R,_G,B}) ->
    {RGB,{R,0,B},{R,1,B}};
get_col_range(blue, RGB={R,G,_B}) ->
    {RGB,{R,G,0},{R,G,1}};
get_col_range(sat, RGB) ->
    HSV={H,_S,V} = rgb_to_hsv(RGB),
    S0 = hsv_to_rgb(H,0.0,V),
    S1 = hsv_to_rgb(H,1.0,V),
    {HSV,S0,S1};
get_col_range(val, RGB) ->
    HSV={H,S,_V} = rgb_to_hsv(RGB),
    V0 = hsv_to_rgb(H,S,0.0),
    V1 = hsv_to_rgb(H,S,1.0),
    {HSV,V0,V1};
get_col_range(hue, RGB) ->
    HSV={_H,S,V} = rgb_to_hsv(RGB),
    V0 = hsv_to_rgb(0.0,S,V),
    V1 = hsv_to_rgb(60.0,S,V),
    {HSV,V0,V1}.

rgb256({R,G,B}) -> {round(R*255),round(G*255),round(B*255)};
rgb256({R,G,B,_A}) -> {round(R*255),round(G*255),round(B*255)}.

apply_callback(Pid, Col) when is_pid(Pid) ->
    Pid ! {col_changed, Col};
apply_callback(CB, Col) when is_function(CB) ->
    CB({col_changed, Col}).

%% Image / icon data

slider_bitmap() ->
    I = wxImage:new(15, 20, rgb()), %alpha(), [{static_data, false}]), doesn't work...
    wxImage:setAlpha(I, alpha()),
    Bmp = wxBitmap:new(I),
    wxImage:destroy(I),
    Bmp.

rgb() ->
    <<0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,0,0,0,0,0,0,0,0,0,240,240,240,193,193,193,141,141,141,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,112,141,141,141,193,193,193,240,240,240,240,240,240,0,0,0,0,0,0,240,240,240,141,141,141,221,221,221,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,252,221,221,221,134,134,134,232,232,232,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,252,252,252,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,242,252,252,252,112,112,112,211,211,211,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,241,251,251,251,112,112,112,198,198,198,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,240,240,240,240,240,240,0,0,0,240,240,240,240,240,240,240,240,240,251,251,251,112,112,112,195,195,195,240,240,240,0,0,0,240,240,240,240,240,240,112,112,112,251,251,251,239,239,239,239,239,239,239,239,239,0,0,0,239,239,239,239,239,239,251,251,251,112,112,112,195,195,195,250,250,250,0,0,0,0,0,0,240,240,240,112,112,112,251,251,251,238,238,238,238,238,238,0,0,0,0,0,0,238,238,238,238,238,238,251,251,251,112,112,112,188,190,190,250,250,250,0,0,0,0,0,0,240,240,240,112,112,112,250,250,250,236,236,236,236,236,236,0,0,0,0,0,0,236,236,236,236,236,236,250,250,250,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,231,234,234,112,112,112,250,250,250,235,235,235,235,235,235,0,0,0,235,235,235,235,235,235,235,235,235,250,250,250,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,252,252,252,112,112,112,246,246,246,219,219,219,219,219,219,0,0,0,0,0,0,219,219,219,219,219,219,246,246,246,112,112,112,188,190,190,255,255,255,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,217,217,217,217,217,217,0,0,0,0,0,0,217,217,217,217,217,217,245,245,245,112,112,112,195,195,195,255,255,255,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,215,215,215,215,215,215,0,0,0,0,0,0,215,215,215,215,215,215,245,245,245,112,112,112,195,195,195,240,240,240,0,0,0,0,0,0,240,240,240,112,112,112,245,245,245,218,218,218,214,214,214,214,214,214,214,214,214,214,214,214,218,218,218,245,245,245,112,112,112,195,195,195,240,240,240,0,0,0,0,0,0,240,240,240,165,165,165,182,182,182,244,244,244,217,217,217,212,212,212,212,212,212,217,217,217,244,244,244,182,182,182,114,114,114,200,200,200,240,240,240,0,0,0,0,0,0,240,240,240,225,225,225,155,155,155,180,180,180,244,244,244,215,215,215,215,215,215,244,244,244,180,180,180,104,104,104,149,149,149,216,216,216,240,240,240,0,0,0,0,0,0,240,240,240,240,240,240,222,222,222,151,151,151,180,180,180,243,243,243,243,243,243,180,180,180,105,105,105,145,145,145,205,205,205,234,234,234,240,240,240,0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,222,222,222,151,151,151,178,178,178,180,180,180,106,106,106,145,145,145,205,205,205,234,234,234,240,240,240,240,240,240,0,0,0,0,0,0,0,0,0,240,240,240,240,240,240,240,240,240,224,224,224,158,158,158,138,138,138,160,160,160,205,205,205,234,234,234,240,240,240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,240,240,240,240,240,234,234,234,223,223,223,222,222,222,234,234,234,240,240,240,0,0,0,0,0,0,0,0,0,0,0,0>>.


alpha() ->
    <<0,0,171,188,186,176,198,224,226,226,227,242,132,0,0,0,168,252,255,255,255,255,255,255,255,255,255,219,33,0,0,184,255,255,255,255,255,255,255,255,255,255,253,126,0,0,184,255,255,255,249,182,200,251,255,255,255,255,119,0,0,184,255,255,255,130,5,12,118,255,255,255,255,142,0,0,184,254,255,255,5,0,1,16,255,255,255,255,140,0,4,184,244,255,255,5,1,0,47,255,255,255,254,127,0,0,184,244,255,255,13,0,0,51,255,255,255,255,119,0,0,184,254,255,255,11,0,0,32,255,255,255,254,119,0,0,184,254,255,255,44,0,1,40,255,255,255,255,119,0,0,184,244,255,255,44,0,0,16,255,255,255,255,119,0,0,184,243,255,255,51,0,0,27,255,255,255,255,119,0,0,184,244,255,255,93,0,0,58,255,255,255,255,119,0,0,119,254,255,255,162,1,1,159,255,255,255,255,119,0,0,130,255,255,255,247,167,165,246,255,255,255,255,124,0,0,119,184,255,255,255,255,255,255,255,255,255,255,119,0,0,0,119,184,255,255,255,255,255,255,255,254,184,0,0,0,2,0,119,184,255,255,255,255,255,253,217,0,22,0,0,0,5,0,119,184,255,255,253,226,155,17,0,0,0,0,0,0,0,2,119,184,255,184,94,3,0,0,0,0>>.

-ifdef(DEBUG).
test() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> run_test() end),
    receive {'EXIT', Pid, Msg} -> Msg end.

-spec run_test() -> no_return().

run_test() ->
    Frame = wxFrame:new(wx:new(), -1, "FOO"),
    Panel = wxPanel:new(Frame),
    Sz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sz, wxButton:new(Panel, 42, [{label, "A button"}])),
    wxSizer:add(Sz, wxStaticText:new(Panel, 43, "Some static text")),
    wxSizer:add(Sz, wxSlider:new(Panel, 46, 27, 1, 100), [{flag, ?wxEXPAND}]),
    RGB = fun(What) -> rgb(wxSystemSettings:getColour(What)) end,
    wxSizer:add(Sz, new(Panel, 45, RGB(?wxSYS_COLOUR_ACTIVECAPTION), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, RGB(?wxSYS_COLOUR_HIGHLIGHT), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, RGB(?wxSYS_COLOUR_MENUHILIGHT), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, RGB(?wxSYS_COLOUR_ACTIVEBORDER), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, RGB(?wxSYS_COLOUR_BTNHILIGHT), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, RGB(?wxSYS_COLOUR_BACKGROUND), []), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, wxButton:new(Panel, 44, [{label, "B button"}])),
    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,red}]), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,green}]), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,blue}]), [{flag, ?wxEXPAND}]),

    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,hue}]), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,sat}]), [{flag, ?wxEXPAND}]),
    wxSizer:add(Sz, new(Panel, -1, {0.5,0.73,0.5}, [{color,val}]), [{flag, ?wxEXPAND}]),

    wxPanel:setSizerAndFit(Panel, Sz),
    wxSizer:setSizeHints(Sz, Frame),
    wxFrame:show(Frame),
    exit(ok).

rgb({R,G,B,_}) -> {R/255, G/255, B/255}.
-endif.
